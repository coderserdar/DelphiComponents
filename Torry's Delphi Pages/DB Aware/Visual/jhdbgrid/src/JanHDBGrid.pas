unit JanHDBGrid;

interface

uses
   Windows, Messages, SysUtils, Classes, Controls, Grids, DBGrids, Graphics, DB;

type
   TJanHDBGrid=class(TDBGrid)
   private
      FLines: integer;
      FSelectColor: TColor;
      FSelectFontColor: TColor;
      FActiveColor: TColor;
      FActiveFontColor: TColor;
      function GetGridFocus: boolean;
   protected
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
   published
      property MoveByRows: integer read FLines write FLines;
      property ActiveRowColor: TColor read FActiveColor write FActiveColor;
      property ActiveRowFontColor: TColor read FActiveFontColor write FActiveFontColor;
      property SelectRowColor: TColor read FSelectColor write FSelectColor;
      property SelectRowFontColor: TColor read FSelectFontColor write FSelectFontColor;
      property OnMouseWheelDown;
      property OnMouseWheelUp;
   end;

procedure Register;

implementation

constructor TJanHDBGrid.Create(AOwner: TComponent);
begin
   FLines:=1;
   FSelectColor:=clNavy;
   FSelectFontColor:=clWhite;
   FActiveColor:=clBlue;
   FActiveFontColor:=clYellow;
   inherited;
end;

destructor TJanHDBGrid.Destroy;
begin
   inherited;
end;

function TJanHDBGrid.GetGridFocus: boolean;
begin
   Result:=true;
   if CanFocus and(not(csDesigning in ComponentState)) then begin
      if not Focused then SetFocus;
      Result:=Focused;
   end;
end;

function TJanHDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
   Result:=false;
   if Assigned(OnMouseWheelDown) then OnMouseWheelDown(Self, Shift, MousePos, Result);
   if not Result then
   begin
      if not GetGridFocus then Exit;
      if DataLink.Active then begin
         Result:=DataLink.DataSet.MoveBy(FLines)<>0;
      end;
   end;
end;

function TJanHDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
   Result:=false;
   if Assigned(OnMouseWheelUp) then OnMouseWheelUp(Self, Shift, MousePos, Result);
   if not Result then begin
      if not GetGridFocus then Exit;
      if DataLink.Active then Result:=DataLink.DataSet.MoveBy(-FLines)<>0;
   end;
end;

procedure TJanHDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var txt: string;
begin
   if csLoading in ComponentState then begin
      Canvas.Brush.Color:=Color;
      Canvas.FillRect(Rect);
      Exit;
   end;

   with Canvas do begin
      if (gdSelected in State)and(Focused) then begin
         Brush.Color:=FSelectColor;
         Canvas.Font.Color:=FSelectFontColor;
      end else
         if SelectedRows.CurrentRowSelected then begin
            Brush.Color:=FActiveColor;
            Canvas.Font.Color:=FActiveFontColor;
         end
         else begin
            Brush.Color:=Color;
            Canvas.Font.Color:=clBlack;
         end;

      FillRect(Rect);
      try
         txt:=Column.Field.AsString;
      except
         txt:='';
      end;
      TextRect(Rect, Rect.Left, Rect.Top, txt);
   end;
end;

procedure Register;
begin
   RegisterComponents('Data Controls', [TJanHDBGrid]);
end;

end.

