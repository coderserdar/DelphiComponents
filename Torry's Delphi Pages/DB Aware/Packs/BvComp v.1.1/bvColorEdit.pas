unit bvColorEdit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,
{$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  QDialogs,
  Types,
{$endif}

  SysUtils, Classes;

type
  TbvColorEdit = class(TSpeedButton)
  private
    { Private declarations }
    FColorValue:TColor;
    FOnChangeColor:TNotifyEvent;

    procedure SetColorValue(Value:TColor);
    function GetColorValue:Tcolor;

  protected
    { Protected declarations }
    procedure paint; override;

  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    procedure Click; override;
  published
    { Published declarations }
    property ColorValue:TColor read GetColorValue write SetColorValue;
    property OnChangeColor:TNotifyEvent read FOnChangeColor write FOnChangeColor;
  end;


implementation


constructor TbvColorEdit.Create(AOwner:TComponent);
begin
   inherited;
   height:=23;
   width :=50;
   FColorValue:=clBlack;
   Cursor:=crHandPoint;
   FOnChangeColor:=nil;

end;

procedure TbvColorEdit.paint;
var thREct:TRect;
begin
   inherited;
   thRect:=Self.GetClientRect;
   inc(thRect.Left,3);
   inc(thRect.top,3);

   if Flat then begin
     dec(thREct.Right,3);
     dec(thRect.bottom,3);
   end
   else begin
     dec(thREct.Right,4);
     dec(thRect.bottom,4);
   end;

   if (thRect.left<thRect.right) and (thRect.Top<thRect.Bottom)
   then begin
      if Flat and (FState<>bsDown) or not Flat and (FState=bsDown)
      then begin
         Frame3D( Canvas, thRect, clBtnHighlight, clBtnShadow, 1);
         //dec(thREct.Right,1);
         //dec(thRect.bottom,1);
      end
      else begin
         Frame3D( Canvas, thRect, clBtnShadow,clBtnHighlight, 1);
         //dec(thREct.Right,2);
         //dec(thRect.bottom,2);
      end;
      //Canvas.pen.Color:=clBlack;
      //Canvas.Rectangle(thREct);
      inc(thRect.Left,1);
      inc(thRect.top,1);
      dec(thREct.Right,2);
      dec(thRect.bottom,2);
      if (thRect.left<thRect.right) and (thRect.Top<thRect.Bottom)
      then begin
         canvas.brush.Color:=FColorValue;
         Canvas.FillRect(thREct);
      end;
   end;
end;

procedure TbvColorEdit.SetColorValue(Value:TColor);
begin
    FColorValue:=Value;
    if Visible and not (csLoading in Componentstate)
       //and (Owner Is TCustomForm) and (Owner as TCustomForm).Visible
    then paint;

    if assigned(FOnChangeColor) then FOnChangeColor(Self);
end;

function TbvColorEdit.GetColorvalue:TColor;
begin
  result:=FColorValue;
end;

procedure TbvColorEdit.Click;
begin
   inherited;
   with TColorDialog.create(Self) do
   try
      Color:=self.FColorValue;
      if Execute then self.ColorValue:=Color;
   finally
      free
   end;

end;

end.
