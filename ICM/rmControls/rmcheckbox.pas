{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCheckBox
Purpose  : Simple Replacement for the MSCheckbox to allow for centering the Box.
Date     : 05-08-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmCheckbox;

interface

{$I CompilerDefines.INC}

uses Windows, Messages, Classes, Controls, Forms, Graphics;

type
  TCBXAlignment = (cbxLeft, cbxRight, cbxCentered);

  TrmCustomCheckBox = class(TCustomControl)
  private
    FChecked: Boolean;
    FMouseDown : boolean;
    fKeyDown : boolean;
    fMouseInControl : boolean;
    fCBXAlignment: TCBXAlignment;
    fFlat: Boolean;
    fShowFocusRect: Boolean;
    fWantTabs: boolean;
    fWantArrows: boolean;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFocusChanged(var MSG:TMessage); message CM_FOCUSCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure SetChecked(Value: Boolean);
    procedure SetCBXAlignment(const Value: TCBXAlignment);
    function CaptionRect:TRect;
    function CBXRect:TRect;
    procedure SetFlat(const Value: Boolean);
    procedure SetShowFocusRect(const Value: Boolean);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure PaintCheck(Rect:TRect); virtual;
    procedure Paint; override;

    procedure Click; override;

    property Checked: Boolean read FChecked write SetChecked default False;
    property CBXAlignment : TCBXAlignment read fCBXAlignment write SetCBXAlignment default cbxLeft;
    property Flat: Boolean read fFlat write SetFlat default false;
    property ShowFocusRect : boolean read fShowFocusRect write SetShowFocusRect default true;

    property IsMouseDown:Boolean read fMouseDown;
    property IsMouseInControl:boolean read fMouseInControl;
    property IsKeyDown:boolean read fKeyDown;
    property WantTabs:boolean read fWantTabs write fWantTabs default false;
    property WantArrows:boolean read fWantArrows write fWantArrows default false;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TrmCheckBox = class(TrmCustomCheckBox)
  published
    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property CBXAlignment;
    property Checked;
    property Caption;
    property Enabled;
    property Font;
    property Flat;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowFocusRect;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses imglist, Actnlist, rmLibrary;

{ TrmCustomCheckBox }

constructor TrmCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 90, 17);
  ControlStyle := [csClickEvents, csCaptureMouse, csDoubleClicks, csSetCaption];
  ParentFont := True;
  Color := clBtnFace;
  fCBXAlignment := cbxLeft;
  fMouseInControl := false;
  FChecked := false;
  fFlat := false;
  fShowFocusRect := true;
  FKeyDown := false;
  FMouseDown := false;
  fWantTabs := false;
  fWantArrows := false;
  TabStop := true;
end;

procedure TrmCustomCheckBox.Paint;
var
   wRect : TRect;
   wFlags : integer;
begin
   Canvas.brush.color := color;
   Canvas.Font := font;
   Canvas.FillRect(clientrect);

   wFlags := dt_VCenter or DT_SingleLine;

   case fCBXAlignment of
      cbxLeft: wFlags := wFlags or dt_left;
      cbxRight: wFlags := wFlags or dt_right;
      cbxCentered: wFlags := wFlags or DT_CENTER;
   end;

   PaintCheck(CBXRect);

   if fCBXAlignment <> cbxCentered then
   begin
      wRect := CaptionRect;
      DrawText(Canvas.Handle, PChar(Caption), length(caption), wRect, wFlags);
      if Focused and fShowFocusRect then
      begin
         inflaterect(wRect, 2, 2);
         Canvas.DrawFocusRect(wRect);
      end;
   end
   else
   begin
      if Focused and fShowFocusRect then
      begin
         wRect := CBXRect;
         InflateRect(wRect, 2, 2);
         Canvas.DrawFocusRect(wRect);
      end;
   end;
end;

procedure TrmCustomCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   wRect : TRect;
begin
  Inherited;
  if canfocus then
     SetFocus;

  UnionRect(wRect, CaptionRect, CBXRect);
  if (Button = mbLeft) and Enabled and ptinrect(wRect, point(x,y)) then
  begin

     FMouseDown := true;
     invalidate;
  end;
end;

procedure TrmCustomCheckBox.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TrmCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;
    
procedure TrmCustomCheckBox.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TrmCustomCheckBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TrmCustomCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
       Invalidate;
    end;
end;

procedure TrmCustomCheckBox.PaintCheck(Rect:TRect);
var
   wFlags : integer;
begin
   wFlags := DFCS_BUTTONCHECK;

   if FFlat then
      wFlags := wFlags or DFCS_FLAT;

   if FChecked then
      wFlags := wFlags or DFCS_CHECKED;

   if (not Enabled) then
      wFlags := wFlags or DFCS_INACTIVE;

   if (fMouseInControl and FMouseDown) or (fKeyDown) then
      wFlags := wFlags or DFCS_PUSHED;

   DrawFrameControl(canvas.handle, Rect, DFC_BUTTON, wFlags);
end;

procedure TrmCustomCheckBox.SetCBXAlignment(const Value: TCBXAlignment);
begin
  if fCBXAlignment <> Value then
  begin
    fCBXAlignment := Value;
    invalidate;
  end;
end;

procedure TrmCustomCheckBox.cmFocusChanged(var MSG: TMessage);
begin
   inherited;
   fKeyDown := false;
   FMouseDown := False;
   invalidate;  
end;

procedure TrmCustomCheckBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   wMD: boolean;
   wRect : TRect;
begin
  inherited;

  UnionRect(wRect, CaptionRect, CBXRect);
  wMD := fMouseDown;
  fMouseDown := false;
  if wMD and enabled and (ptinrect(wRect, point(x,y))) then
  begin
     FChecked := not fChecked;
     Invalidate;
     Click;
  end;
end;

procedure TrmCustomCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   fLastCheck : boolean;
begin
  inherited;
  fLastCheck := fMouseInControl;
  fMouseInControl := PTInRect(ClientRect, point(x,y));
  if fLastCheck <> fMouseInControl then
     Invalidate;
end;

procedure TrmCustomCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (key = vk_space) and (Shift = []) then
  begin
     fKeyDown := true;
     invalidate;
  end;
end;

procedure TrmCustomCheckBox.KeyUp(var Key: Word; Shift: TShiftState);
var
   wKD: boolean;
begin
  inherited;
  wKD := fKeyDown;
  fKeyDown := false;
  if wKD and (key = vk_space) then
  begin
     FChecked := not fChecked;
     Invalidate;
     click;
  end;
end;

procedure TrmCustomCheckBox.WMLButtonUp(var Message: TWMLButtonUp);
var
   wClicked : boolean;
begin
   wClicked := csClicked in ControlState;
   if wClicked then
      ControlState := ControlState - [csClicked];

   Inherited;

   if wClicked then
      ControlState := ControlState + [csClicked];
end;

procedure TrmCustomCheckBox.Click;
begin
  inherited;
  Invalidate;
end;

function TrmCustomCheckBox.CaptionRect: TRect;
begin
   case fCBXAlignment of
      cbxLeft:
         begin
            result := rect(0, 0, Canvas.textwidth(Caption), Canvas.Textheight(caption));
            offsetRect(result, RectWidth(CBXRect)+7, (height div 2) - (RectHeight(result) div 2));
         end;
      cbxRight:
         begin
            result := rect(width-Canvas.textwidth(Caption), 0, width, Canvas.Textheight(caption));
            offsetRect(result, -(RectWidth(CBXRect)+7), (height div 2) - (RectHeight(result) div 2));
         end;
      cbxCentered: result := CBXRect;
   end;
end;

function TrmCustomCheckBox.CBXRect: TRect;
begin
   result := Rect(0, 0, 13, 13);
   case fCBXAlignment of
      cbxLeft: offsetRect(result, 3, (height div 2) - (RectHeight(result) div 2));
      cbxRight: offsetRect(result, width - (RectWidth(result)+3), (height div 2) - (RectHeight(result) div 2));
      cbxCentered: offsetRect(result, (width div 2) - (rectWidth(result) div 2), (height div 2) - (RectHeight(result) div 2));
   end;
end;

procedure TrmCustomCheckBox.CMMouseLeave(var Message: TMessage);
begin
   inherited;
   invalidate;
end;

procedure TrmCustomCheckBox.SetFlat(const Value: Boolean);
begin
  fFlat := Value;
  invalidate;
end;

procedure TrmCustomCheckBox.SetShowFocusRect(const Value: Boolean);
begin
  fShowFocusRect := Value;
  invalidate;
end;

procedure TrmCustomCheckBox.WMEraseBkgnd(var message: TMessage);
begin
   message.result := 0;   
end;

procedure TrmCustomCheckBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
   inherited;

   if fWantTabs then
      Message.Result := Message.Result or DLGC_WANTTAB;

   if fWantArrows then
      Message.Result := Message.Result or DLGC_WANTARROWS;

end;

end.
