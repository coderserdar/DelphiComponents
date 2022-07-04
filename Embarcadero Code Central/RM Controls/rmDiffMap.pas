{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmDiffMap
Purpose  : This is a map component primarily used in showing where the
           differences are with in the mapping of the rmDiff Components.
Date     : 04-24-2000
Author   : Ryan J. Mills
Version  : 1.80
Notes    : This component originally came to me from Bernie Caudrey.
           I've only modified the original source to work with my rmDiff
           components.  I would like to go back and rewrite the drawing and
           mapping algorithms because I think they are unnecessarily complex.
           It also doesn't use any resources where it should.
================================================================================}

unit rmDiffMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
   TAByte = array of BYTE;
   pAByte = ^TAByte;

  TMapClickEvent = procedure(Sender:TObject; IndicatorPos : integer) of object;

  TrmDiffMap = class(TCustomControl)
  private
    { Private declarations }
    FColorDeleted     : TColor;
    FColorInserted    : TColor;
    FColorModified    : TColor;
    FShowIndicator    : Boolean;
    FIndicatorPos     : integer;
    FRows             : integer;
    FIndicator        : TBitmap;
    FData             : TAByte;
    FOnMapClick : TMapClickEvent;
    procedure DrawIndicator;
    procedure SetIndicatorPos(Value : integer);
    function  MapHeight : integer;
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   SetData(Value : TAByte; Size : integer);
    procedure   MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    property    Data : TAByte read FData;
  published
    { Published declarations }
    property Color;
    property Caption;
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property ColorDeleted     : TColor  read FColorDeleted     write FColorDeleted;
    property ColorInserted    : TColor  read FColorInserted    write FColorInserted;
    property ColorModified    : TColor  read FColorModified    write FColorModified;
    property ShowIndicator    : Boolean read FShowIndicator    write FShowIndicator;
    property IndicatorPos     : integer read FIndicatorPos     write SetIndicatorPos;
    property IndicatorRange   : integer read FRows;
    property OnMapClick : TMapClickEvent read FOnMapClick write FOnMapClick;
  end;

implementation

const
TOP_MARGIN      = 5;
      BOTTOM_MARGIN   = 5;
      INDICATOR_WIDTH = 4;

      // These constants are reproduced here from IntfDifferenceEngine.pas
      RECORD_SAME     = 0;
      RECORD_DELETED  = 1;
      RECORD_INSERTED = 2;
      RECORD_MODIFIED = 3;

constructor TrmDiffMap.Create(AOwner : TComponent);
begin
   inherited;
   FShowIndicator      := True;
   FColorDeleted       := clRed;
   FColorInserted      := clLime;
   FColorModified      := clYellow;
   Caption             := '';
   FData               := nil;
   FRows               := 0;
   IndicatorPos        := 0;

   FIndicator          := TBitmap.Create;
   FIndicator.Width    := 4;
   FIndicator.Height   := 6;

   FIndicator.Canvas.Pixels[0,0] := clBlack;
   FIndicator.Canvas.Pixels[0,1] := clBlack;
   FIndicator.Canvas.Pixels[0,2] := clBlack;
   FIndicator.Canvas.Pixels[0,3] := clBlack;
   FIndicator.Canvas.Pixels[0,4] := clBlack;
   FIndicator.Canvas.Pixels[1,1] := clBlack;
   FIndicator.Canvas.Pixels[1,2] := clBlack;
   FIndicator.Canvas.Pixels[1,3] := clBlack;
   FIndicator.Canvas.Pixels[2,2] := clBlack;

   FIndicator.Canvas.Pixels[1,0] := clFuchsia;
   FIndicator.Canvas.Pixels[2,0] := clFuchsia;
   FIndicator.Canvas.Pixels[2,1] := clFuchsia;
   FIndicator.Canvas.Pixels[3,2] := clFuchsia;
   FIndicator.Canvas.Pixels[1,4] := clFuchsia;
   FIndicator.Canvas.Pixels[2,4] := clFuchsia;

   FIndicator.TransparentMode  := tmFixed;
   FIndicator.TransparentColor := clFuchsia;
   FIndicator.Transparent      := True;
   fIndicator.SaveToFile('c:\indicator.bmp');
end;


destructor TrmDiffMap.Destroy;
begin
   if FData <> nil then begin
      FData := nil;
   end;
   FIndicator.Free;
   inherited;
end;


procedure TrmDiffMap.SetData(Value : TAByte; Size : integer);
begin
   if FData <> nil then begin
      FData := nil;
   end;

   FData := Value;

   if FData <> nil then begin
      FRows := Size;
   end else begin
      FRows := 0;
   end;
end;



procedure TrmDiffMap.SetIndicatorPos(Value : integer);
begin
   FIndicatorPos := Value;
   Refresh;
end;

procedure TrmDiffMap.Paint;
var
   i               : Integer;
   j               : Integer;
   NrOfDataRows    : Integer;
   Ht              : Integer;
   Ct              : Integer;
   CurrIndex       : Integer;
   PixelPos        : Integer;
   PixelHt         : Double;   // amount of pixel height for each row - could be a rather small number
   PixelFrac       : Double;   // Faction part of pixel - left over from previous
   PixelPrevHt     : Double;   // logical height of previous mapped pixel (eg. .92)
   NrOfPixelRows   : Double;   // Number of rows that the current pixel is to represent.
   ExtraPixel      : Double;   // Left over pixel from when calculating number of rows for the next pixel.
                               // eg. 1/.3 = 3 rows, .1 remaining, next 1.1/.3 = 3 rows, .2 remain, next 1.2/.3 = 4 rows.

   DrawIt          : Boolean; // Drawing flag for each column
   RowModified     : Boolean;
   RowDeleted      : Boolean;
   RowInserted     : Boolean;

   ExitLoop        : Boolean;  // loop control

   // Draws the line between two points on the horizonatal line of i.
   procedure DrawLine(X1, X2 : integer);
   var
      k : integer;
   begin
      // What colour?  Black or Background?
      if DrawIt then begin
         if RowModified then begin
            Canvas.Pen.Color := ColorModified;
         end else begin
            if RowInserted then begin
               Canvas.Pen.Color := ColorInserted;
            end else begin
               if RowDeleted then begin
                  Canvas.Pen.Color := ColorDeleted;
               end;
            end;
         end;
      end else begin
         Canvas.Pen.Color := Color;
      end;
      // Draw the pixels for the map here
      for k := 0 to Round(NrOfPixelRows) - 1 do begin
         Canvas.MoveTo(X1, PixelPos + k);
         Canvas.LineTo(X2, PixelPos + k);
      end;
   end;

begin
   inherited;
   if csDesigning in ComponentState then begin
      exit;
   end;

   Ht         := MapHeight;
   Ct         := FRows;
   if Ct < 1 then begin
      Ct := 1;
   end;
   PixelHt       := Ht / Ct;
   CurrIndex     := 1;
   NrOfPixelRows := 0.0;
   PixelPrevHt   := 0.0;
   PixelPos      := 5;
   i             := 1;
   ExtraPixel    := 0.0;

   J := CurrIndex;
   while J < Ct do begin
      NrOfDataRows := 0;
      PixelPrevHt  := PixelPrevHt - NrOfPixelRows; // remainder from prevous pixel row (+ or -)
      PixelFrac    := frac(PixelPrevHt);             // We want just the fractional part!

      // Calculate how high the pixel line is to be
      if PixelHt < 1.0 then begin
         NrOfPixelRows := 1.0;                      // Each Pixel line represents one or more rows of data
      end else begin
         NrOfPixelRows := Int(PixelHt + ExtraPixel); // We have several pixel lines for each row of data.
         ExtraPixel    := frac(PixelHt + ExtraPixel);// save frac part for next time
      end;

      // Calculate the nr of data rows to be represented by the Pixel Line about to be drawn.
      ExitLoop := False;
      repeat
         // the '.../2.0' checks if half a Pixel Ht will fit, else leave remainder for next row.
         if (PixelFrac + PixelHt <= NrOfPixelRows) or
            (PixelFrac + PixelHt / 2.0 <= NrOfPixelRows) then begin
            PixelFrac := PixelFrac + PixelHt;
            inc(NrOfDataRows);
         end else begin
            ExitLoop := True;
         end;
      until (PixelFrac >= NrOfPixelRows) or (ExitLoop);

      // go through each data row, check if a file has been modified.
      // if any file has been modified then we add to the Mapping.
      if NrOfDataRows > 0 then begin
         DrawIt := False;
      end;

      RowModified := False;
      RowInserted := False;
      RowDeleted  := False;

      for j := j to j + NrOfDataRows - 1 do begin
         if j < ct then begin
            case Data[j] of
               RECORD_MODIFIED :
               begin
                  DrawIt      := True;
                  RowModified := True;
               end;
               RECORD_INSERTED :
               begin
                  DrawIt      := True;
                  RowInserted := True;
               end;
               RECORD_DELETED :
               begin
                  DrawIt     := True;
                  RowDeleted := True;
               end;
            end;
         end else begin
            i := i;
         end;
      end;

      // Mapping is drawn here
      if ShowIndicator then begin
         DrawLine(INDICATOR_WIDTH, Width - INDICATOR_WIDTH);
      end else begin
         DrawLine(0, Width);
      end;
      inc(PixelPos, Trunc(NrOfPixelRows));  // the pixel pos on the map.
      PixelPrevHt := int(PixelPrevHt) + PixelFrac;
   end;

   if ShowIndicator then begin
      DrawIndicator;
   end;

end;


procedure TrmDiffMap.DrawIndicator;
var
   Y    : integer;
begin
   Canvas.Pen.Color := clBlack;
   if FRows <> 0 then begin
      Y := TOP_MARGIN + Trunc((IndicatorPos / FRows) * MapHeight);
      Canvas.Draw(0, Y - (FIndicator.Height div 2), FIndicator);
   end;
end;


function TrmDiffMap.MapHeight : integer;
begin
   Result := Height - TOP_MARGIN - BOTTOM_MARGIN;
end;


procedure TrmDiffMap.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
   NewRow    : Integer;
   NewTopRow : Integer;
   PixelHt   : Double;
begin
   inherited;
   if Assigned(OnMapClick) then begin
      PixelHt := MapHeight / IndicatorRange; // This is how much of a pixel (or how many pixels), each row represents.
      // (the pixel clicked) + Half the Pixel Height + 1.0 / the pixel height
      // eg. Of 1000 rows, in the pixel area height of 500, then pixelHt = .5 (500/1000)
      //     Therefore, if pixel 100 is clicked we get - (100 + .25 + 1) / .5 = 51
      // Y-5, we subtract 5 as we start 5 pixels from the top of lblQuickPickArea.
      NewRow := Round(((Y - TOP_MARGIN) * 1.0 + PixelHt / 2.0 + 1.0) / PixelHt);

      if NewRow > IndicatorRange - 1 then begin
         NewRow := IndicatorRange - 1
      end else begin
         if NewRow < 1 then begin
            NewRow := 1;
         end;
      end;
      OnMapClick(self, NewRow);
   end;
end;


end.






