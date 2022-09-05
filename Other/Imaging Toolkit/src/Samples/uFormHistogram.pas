// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17601: uFormHistogram.pas 
//
//    Rev 1.5    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.4    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.3    29-09-2003 18:43:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.2    25-07-2003 00:03:24  mcm
// Corrected histogram view for 4 bit images.

//
//   Rev 1.1    05-02-03 16:36:14  mcm
// Corrected a bug in interpretating no. of colors.

//
//   Rev 1.0    27-05-2002 16:22:32  mcm

unit uFormHistogram;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
     {$ENDIF}
     mcmImage,
     mcmImageTypeDef,
     mcmImageColor,
     mcmShape,
     umcmIntE;

type
  TFormHistogram = class(TForm)
    HistogramView : TmcmImageCtrl;
    lChannel      : TLabel;
    lInputMin     : TLabel;
    lInputMax     : TLabel;
    lOutputMin    : TLabel;
    lOutputMax    : TLabel;
    lGamma        : TLabel;
    mcmShape      : TmcmShape;
    btnOK         : TButton;
    btnCancel     : TButton;
    cbChannel     : TComboBox;
    rsGamma       : TmcmRealSpin;
    btnReset      : TButton;
    btnAuto       : TButton;
    seInputMin    : TmcmIntSpin;
    seInputMax    : TmcmIntSpin;
    seOutputMax   : TmcmIntSpin;
    seOutputMin   : TmcmIntSpin;
    procedure FormCreate         (Sender : TObject);
    procedure FormDestroy        (Sender : TObject);
    procedure btnOKClick         (Sender : TObject);
    procedure btnCancelClick     (Sender : TObject);
    procedure cbChannelChange    (Sender : TObject);
    procedure btnAutoClick       (Sender : TObject);
    procedure btnResetClick      (Sender : TObject);
    procedure seInOutMaxMinChange(Sender : TObject);
    procedure mcmShapeMouseDown  (Sender : TObject;
                                  Button : TMouseButton;
                                  Shift  : TShiftState;
                                  X, Y   : Integer);
    procedure mcmShapeMouseMove  (Sender : TObject;
                                  Shift  : TShiftState;
                                  X, Y   : Integer);
    procedure mcmShapeMouseUp    (Sender : TObject;
                                  Button : TMouseButton;
                                  Shift  : TShiftState;
                                  X, Y   : Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FImage          : TmcmImage;
    FImageColor     : TmcmImageColor;

    IntValues       : TRect;
    RedValues       : TRect;
    GreenValues     : TRect;
    BlueValues      : TRect;
    IntSlope        : double;
    IntOffset       : double;
    RedSlope        : double;
    RedOffset       : double;
    GreenSlope      : double;
    GreenOffset     : double;
    BlueSlope       : double;
    BlueOffset      : double;

    procedure ShowHistLine;
    procedure CalculateHistogram;
    procedure SetImage(Value : TmcmImage);
    procedure ShowHistogram;
  public
    { Public declarations }
    property Image : TmcmImage
      read   FImage
      write  SetImage;
  end;

var FormHistogram : TFormHistogram;

implementation

{$R *.DFM}

procedure TFormHistogram.FormCreate(Sender : TObject);
begin
  cbChannel.ItemIndex := 0;
  FImage := Nil; 
  FImageColor := TmcmImageColor.Create(Self);
  HistogramView.Image.ImageFormat := IF_RGB24;
  HistogramView.Image.Height := 128;
  HistogramView.Image.Width  := 256;

  IntSlope    := 1.0;
  IntOffset   := 0.0;
  RedSlope    := 1.0;
  RedOffset   := 0.0;
  GreenSlope  := 1.0;
  GreenOffset := 0.0;
  BlueSlope   := 1.0;
  BlueOffset  := 0.0;
  
  IntValues   := Rect(0, 255, 255, 0);
  RedValues   := Rect(0, 255, 255, 0);
  GreenValues := Rect(0, 255, 255, 0);
  BlueValues  := Rect(0, 255, 255, 0);
end; // TFormHistogram.FormCreate.


procedure TFormHistogram.FormDestroy(Sender : TObject);
begin
  if Assigned(FImageColor)
  then FImageColor.Free;
  if Assigned(FImage)
  then FImage.Free;
end; // TFormHistogram.FormDestroy.


procedure TFormHistogram.FormShow(Sender : TObject);
begin
  ShowHistLine;
end; // TFormHistogram.FormShow.


procedure TFormHistogram.btnOKClick(Sender : TObject);
begin
;
end; // TFormHistogram.btnOKClick.


procedure TFormHistogram.btnCancelClick(Sender : TObject);
begin
  FImageColor.ResultImage.Assign(FImage);
end; // TFormHistogram.btnCancelClick.


procedure TFormHistogram.SetImage(Value : TmcmImage);
begin
  FImage := TmcmImage.Create;
  FImage.Assign(Value);
  FImageColor.ResultImage := Value;
  FImageColor.SourceImage[0] := FImage;
  FImageColor.GetHistogram;

  if (FImage.ImageFormat = IF_BW) or
     (FImage.ImageFormat = IF_GREY4) or
     (FImage.ImageFormat = IF_GREY8)
  then begin
       cbChannel.ItemIndex := 0;
       cbChannel.Enabled := False;
  end;
  ShowHistogram;
end; // TFormHistogram.SetImage.


procedure TFormHistogram.ShowHistogram;
var i         : word;
    pHist     : PVectorC;
    NoColors  : cardinal;
    MaxValue  : cardinal;
    NextValue : cardinal;
    Scale     : double;
begin
  HistogramView.Image.FillAll(255);
  HistogramView.Image.Canvas.Pen.Style := PSSOLID;
  case cbChannel.ItemIndex of
  0 : begin
        pHist := FImageColor.IntHistogram;
        HistogramView.Image.Canvas.Pen.Color := RGB(0, 0, 0);
      end;
  1 : begin
        pHist := FImageColor.RedHistogram;
        HistogramView.Image.Canvas.Pen.Color := RGB(255, 0, 0);
      end;
  2 : begin
        pHist := FImageColor.GreenHistogram;
        HistogramView.Image.Canvas.Pen.Color := RGB(0, 255, 0);
      end;
  3 : begin
        pHist := FImageColor.BlueHistogram;
        HistogramView.Image.Canvas.Pen.Color := RGB(0, 0, 255);
      end;
  else pHist := Nil;
  end;

  if Assigned(pHist)
  then begin
       NoColors := 256;
       MaxValue := 0;
       NextValue := 0;
       for i := 0 to (NoColors - 1)
       do begin
          if (MaxValue < pHist^[i])
          then MaxValue := pHist^[i];
          if (NextValue < pHist^[i]) and (MaxValue > pHist^[i])
          then NextValue := pHist^[i];
       end;
       if (NextValue > 0) and False
       then Scale := HistogramView.Image.Height / NextValue
       else Scale := HistogramView.Image.Height / MaxValue;

       for i := 0 to (NoColors - 1)
       do begin
          HistogramView.Image.Canvas.MoveTo(i, HistogramView.Image.Height - 1);
          HistogramView.Image.Canvas.LineTo(i, HistogramView.Image.Height - 1 - Round(Scale * pHist^[i]));
       end;
       HistogramView.DrawImage;
  end;
end; // TFormHistogram.ShowHistogram.


procedure TFormHistogram.cbChannelChange(Sender : TObject);
begin
  seOutputMax.OnChange := Nil;
  seOutputMin.OnChange := Nil;
  seInputMax.OnChange := Nil;
  seInputMin.OnChange := Nil;
  case cbChannel.ItemIndex of
  0 : begin
        seOutputMax.Value := IntValues.Top;
        seOutputMin.Value := IntValues.Bottom;
        seInputMax.Value  := IntValues.Right;
        seInputMin.Value  := IntValues.Left;
      end;
  1 : begin
        seOutputMax.Value := RedValues.Top;
        seOutputMin.Value := RedValues.Bottom;
        seInputMax.Value  := RedValues.Right;
        seInputMin.Value  := RedValues.Left;
      end;
  2 : begin
        seOutputMax.Value := GreenValues.Top;
        seOutputMin.Value := GreenValues.Bottom;
        seInputMax.Value  := GreenValues.Right;
        seInputMin.Value  := GreenValues.Left;
      end;
  3 : begin
        seOutputMax.Value := BlueValues.Top;
        seOutputMin.Value := BlueValues.Bottom;
        seInputMax.Value  := BlueValues.Right;
        seInputMin.Value  := BlueValues.Left;
      end;
  end;
  ShowHistLine;
  ShowHistogram;
  seOutputMax.OnChange := seInOutMaxMinChange;
  seOutputMin.OnChange := seInOutMaxMinChange;
  seInputMax.OnChange := seInOutMaxMinChange;
  seInputMin.OnChange := seInOutMaxMinChange;
end; // TFormHistogram.cbChannelChange.


procedure TFormHistogram.CalculateHistogram;
begin
  ShowHistLine;
  case cbChannel.ItemIndex of
  0 : begin // Intensity
        IntSlope  := (seOutputMax.Value - seOutputMin.Value) /
                     (seInputMax.Value  - seInputMin.Value);
        IntOffset := seOutputMin.Value - IntSlope * seInputMin.Value;

        RedSlope    := IntSlope;
        RedOffset   := IntOffset;
        GreenSlope  := IntSlope;
        GreenOffset := IntOffset;
        BlueSlope   := IntSlope;
        BlueOffset  := IntOffset;
      end;
  1 : begin // Red
        RedSlope  := (seOutputMax.Value - seOutputMin.Value) /
                     (seInputMax.Value  - seInputMin.Value);
        RedOffset := seOutputMin.Value - RedSlope * seInputMin.Value;
      end;
  2 : begin // Green
        GreenSlope  := (seOutputMax.Value - seOutputMin.Value) /
                       (seInputMax.Value  - seInputMin.Value);
        GreenOffset := seOutputMin.Value - GreenSlope * seInputMin.Value;
      end;
  3 : begin // Blue
        BlueSlope  := (seOutputMax.Value - seOutputMin.Value) /
                      (seInputMax.Value  - seInputMin.Value);
        BlueOffset := seOutputMin.Value - BlueSlope * seInputMin.Value;
      end;
  end;
  FImageColor.SetHistogram(RedSlope, RedOffset,
                           GreenSlope, GreenOffset,
                           BlueSlope,  BlueOffset);
  FImageColor.ResultImage.Modified := True;
end; // TFormHistogram.CalculateHistogram.


procedure TFormHistogram.ShowHistLine;
var i : word;
    Points    : array[0..25] of TPoint;
begin
  i := 0;
  Points[i].x := 0;
  Points[i].y := 128 - seOutputMin.Value div 2;
  inc(i);
  Points[i].x := seInputMin.Value;
  Points[i].y := 128 - seOutputMin.Value div 2;
  inc(i);
  Points[i].x := seInputMax.Value;
  Points[i].y := 128 - seOutputMax.Value div 2;
  inc(i);
  Points[i].x := 255;
  Points[i].y := 128 - seOutputMax.Value div 2;
  inc(i);
  mcmShape.SetPoints(Slice(Points, i));
end; // TFormHistogram.ShowHistLine.


procedure TFormHistogram.btnAutoClick(Sender : TObject);
var i       : word;
    LoValue : word;
    HiValue : word;
    pHist   : PVectorC;
begin
  case cbChannel.ItemIndex of
  0 : pHist := FImageColor.IntHistogram;
  1 : pHist := FImageColor.RedHistogram;
  2 : pHist := FImageColor.GreenHistogram;
  3 : pHist := FImageColor.BlueHistogram;
  else pHist := Nil;
  end;

  i := 0;
  while (i < 255) and (pHist^[i] = 0)
  do inc(i);
  LoValue := i;
  i := 255;
  while (i > 0) and (pHist^[i] = 0)
  do dec(i);
  HiValue := i;

  seOutputMin.Value := 0;
  seOutputMax.Value := 255;
  seInputMin.Value  := LoValue;
  seInputMax.Value  := HiValue;
end; // TFormHistogram.btnAutoClick.


procedure TFormHistogram.btnResetClick(Sender : TObject);
begin
  IntSlope    := 1.0;
  IntOffset   := 0.0;
  RedSlope    := 1.0;
  RedOffset   := 0.0;
  GreenSlope  := 1.0;
  GreenOffset := 0.0;
  BlueSlope   := 1.0;
  BlueOffset  := 0.0;

  seInputMin.Value  := 0;
  seInputMax.Value  := 255;
  seOutputMin.Value := 0;
  seOutputMax.Value := 255;
  rsGamma.Value     := 1.0;

  IntValues   := Rect(0, 255, 255, 0);
  RedValues   := Rect(0, 255, 255, 0);
  GreenValues := Rect(0, 255, 255, 0);
  BlueValues  := Rect(0, 255, 255, 0);
  ShowHistLine;
end; // TFormHistogram.btnResetClick.


procedure TFormHistogram.seInOutMaxMinChange(Sender : TObject);
begin
  if (Sender is TmcmIntSpin)
  then if (TmcmIntSpin(Sender).Text <> '')
       then begin
            if (TmcmIntSpin(Sender).Value > 255)
            then TmcmIntSpin(Sender).Value := 255;
            if (Sender = seInputMax) and (seInputMin <> Nil)
            then if (seInputMax.Value <= seInputMin.Value)
                 then seInputMax.Value := seInputMin.Value + 1;
            if (Sender = seInputMin) and (seInputMax <> Nil)
            then if (seInputMax.Value <= seInputMin.Value)
                 then seInputMin.Value := seInputMax.Value - 1;
            if (Sender = seOutputMax) and (seOutputMin <> Nil)
            then if (seOutputMax.Value <= seOutputMin.Value)
                 then seOutputMax.Value := seOutputMin.Value + 1;
            if (Sender = seOutputMin) and (seOutputMax <> Nil)
            then if (seOutputMax.Value <= seOutputMin.Value)
                 then seOutputMin.Value := seOutputMax.Value - 1;

            case cbChannel.ItemIndex of
            0 : begin
                  IntValues.Top      := seOutputMax.Value;
                  IntValues.Bottom   := seOutputMin.Value;
                  IntValues.Right    := seInputMax.Value;
                  IntValues.Left     := seInputMin.Value;
                end;
            1 : begin
                  RedValues.Top      := seOutputMax.Value;
                  RedValues.Bottom   := seOutputMin.Value;
                  RedValues.Right    := seInputMax.Value;
                  RedValues.Left     := seInputMin.Value;
                end;
            2 : begin
                  GreenValues.Top    := seOutputMax.Value;
                  GreenValues.Bottom := seOutputMin.Value;
                  GreenValues.Right  := seInputMax.Value;
                  GreenValues.Left   := seInputMin.Value;
                end;
            3 : begin
                  BlueValues.Top     := seOutputMax.Value;
                  BlueValues.Bottom  := seOutputMin.Value;
                  BlueValues.Right   := seInputMax.Value;
                  BlueValues.Left    := seInputMin.Value;
                end;
            end;

            if (Visible)
            then CalculateHistogram;
       end;
end; // TFormHistogram.seOutputMaxChange.


procedure TFormHistogram.mcmShapeMouseDown(Sender : TObject;
                                           Button : TMouseButton;
                                           Shift  : TShiftState;
                                           X, Y   : Integer);
begin
;
end; // TFormHistogram.mcmShapeMouseDown.


procedure TFormHistogram.mcmShapeMouseMove(Sender : TObject;
                                           Shift  : TShiftState;
                                           X, Y   : Integer);
begin
;
end; // TFormHistogram.mcmShapeMouseMove.


procedure TFormHistogram.mcmShapeMouseUp(Sender : TObject;
                                         Button : TMouseButton;
                                         Shift  : TShiftState;
                                         X, Y   : Integer);
begin
;
end; // TFormHistogram.mcmShapeMouseUp.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}


end.
