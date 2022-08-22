unit TB97MaskEdit;

{ TMaskEdit97 v0.1 }

interface

uses
  Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  Mask;

type
  { TMaskEdit97 }

  TMaskEdit97 = class(TCustomMaskEdit)
  private
    MouseInControl: Boolean;
    procedure RedrawBorder (const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Align;
    property CharCase;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    {$IFDEF TB97D3}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents ('Toolbar97', [TMaskEdit97]);
end;

{ TMaskEdit97 }

constructor TMaskEdit97.Create (AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  Ctl3D := False;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed]; {fixes a VCL bug with Win 3.x}
  Height := 19;
end;

procedure TMaskEdit97.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  MouseInControl := True;
  RedrawBorder (0);
end;

procedure TMaskEdit97.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
  RedrawBorder (0);
end;

procedure TMaskEdit97.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics (DC, Metrics);
  SelectObject (DC, SaveFont);
  ReleaseDC (0, DC);

  Height := Metrics.tmHeight + 6;
end;

procedure TMaskEdit97.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TMaskEdit97.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
end;

procedure TMaskEdit97.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TMaskEdit97.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder (0);
end;

procedure TMaskEdit97.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    RedrawBorder (0);
end;

procedure TMaskEdit97.WMNCCalcSize (var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect (Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TMaskEdit97.WMNCPaint (var Message: TMessage);
begin
  inherited;
  RedrawBorder (HRGN(Message.WParam));
end;

procedure TMaskEdit97.RedrawBorder (const Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  NewClipRgn: HRGN;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  DC := GetWindowDC(Handle);
  try
    { Use update region }
    if (Clip <> 0) and (Clip <> 1) then begin
      GetWindowRect (Handle, R);
      if SelectClipRgn(DC, Clip) = ERROR then begin
        NewClipRgn := CreateRectRgnIndirect(R);
        SelectClipRgn (DC, NewClipRgn);
        DeleteObject (NewClipRgn);
      end;
      OffsetClipRgn (DC, -R.Left, -R.Top);
    end;

    { This works around WM_NCPAINT problem described at top of source code }
    {no!  R := Rect(0, 0, Width, Height);}
    GetWindowRect (Handle, R);  OffsetRect (R, -R.Left, -R.Top);
    BtnFaceBrush := GetSysColorBrush(COLOR_BTNFACE);
    WindowBrush := GetSysColorBrush(COLOR_WINDOW);
    if ((csDesigning in ComponentState) and Enabled) or
       (not(csDesigning in ComponentState) and
        (Focused or (MouseInControl and not(Screen.ActiveControl is TMaskEdit97)))) then begin
      DrawEdge (DC, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      with R do begin
        FillRect (DC, Rect(Left, Top, Left+1, Bottom-1), BtnFaceBrush);
        FillRect (DC, Rect(Left, Top, Right-1, Top+1), BtnFaceBrush);
      end;
      DrawEdge (DC, R, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, WindowBrush);
    end
    else begin
      FrameRect (DC, R, BtnFaceBrush);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, BtnFaceBrush);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, WindowBrush);
    end;
  finally
    ReleaseDC (Handle, DC);
  end;
end;

end.
