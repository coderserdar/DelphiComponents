unit mbXPBevel;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics;

type
  TBevelStyle = (bsBox, bsLeftLine, bsTopLine, bsRightLine, bsBottomLine);
  TmbXPBevel = class(TCustomControl)
  private
   FStyle: TBevelStyle;
   FNColor, FDColor: TColor;
   FTransparent: boolean;

   procedure SetTransparent(t: boolean);
   procedure SetNColor(c: TColor);
   procedure SetDColor(c: TColor);
   procedure SetStyle(s: TBevelStyle);
  protected
   procedure Paint; override;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
   constructor Create(AOwner: TComponent); override;
  published
   property Align;
   property Anchors;
   property Constraints;
   property Visible;
   property Enabled;
   property Style: TBevelStyle read FStyle write SetStyle default bsBox;
   property EnabledColor: TColor read FNColor write SetNColor default clActiveBorder;
   property DisabledColor: TColor read FDColor write SetDColor default clInactiveBorder;
   property Transparent: boolean read FTransparent write SetTransparent default false;
   property Color;
   property ParentColor;
   property ParentShowHint;
  end;

implementation

constructor TmbXPBevel.Create(AOwner: TComponent);
begin
 inherited;
 DoubleBuffered := true;
 ControlStyle := COntrolStyle + [csOpaque];
 FStyle := bsBox;
 FNColor := clActiveBorder;
 FDColor := clInactiveBorder;
 Ftransparent := false;
end;

procedure TmbXPBevel.Paint; 
begin
 with Canvas do
  begin
   if Enabled then
    Pen.Color := FNColor
   else
    Pen.Color := FDColor;
   Brush.Color := Color;
   FillRect(ClientRect);
   case FStyle of
    bsBox: Rectangle(ClientRect);
    bsLeftLine:
     begin
      MoveTo(0, 0);
      LineTo(0, Height);
     end;
    bsTopLine:
     begin
      MoveTo(0, 0);
      LineTo(Width, 0);
     end;
    bsRightLine:
     begin
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height - 1);
     end;
    bsBottomLine:
     begin
      MoveTo(0, Height - 1);
      LineTo(Width - 1, Height - 1);
     end;
   end;
  end;
end;

procedure TmbXPBevel.SetNColor(c: TColor);
begin
 FNColor := c;
 invalidate;
end;

procedure TmbXPBevel.SetDColor(c: TColor);
begin
 FDColor := c;
 invalidate;
end;

procedure TmbXPBevel.SetStyle(s: TBevelStyle);
begin
 FStyle := s;
 invalidate;
end;

procedure TmbXPBevel.SetTransparent(t: boolean);
begin
 FTransparent := t;
 invalidate;
end;

procedure TmbXPBevel.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 invalidate;
end;

procedure TmbXPBevel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 if not FTransparent then
  Message.Result := 1
 else
  inherited;
end;

end.
