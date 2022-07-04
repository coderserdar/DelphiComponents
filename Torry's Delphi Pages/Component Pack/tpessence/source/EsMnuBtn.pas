
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsMnuBtn;
  {-menu popup button}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, Dialogs, ExtCtrls, Graphics, Messages, Menus,
  EsBase;

type
  TEsMenuPosition = (mpBelow, mpRight);

  TEsCustomMenuButton = class(TEsBase)
  private
    function GetGlyph: TBitmap;
  protected {private}
    {.Z+}
    {property variables}
    FCaption      : TCaption;
    FMenuPosition : TEsMenuPosition;
    FShowGlyph    : Boolean;

    {internal variables}
    mbDown        : Boolean;
    mbDragging    : Boolean;
    mbGlyph       : TBitmap;
    mbImage       : TImageList;
    mbState       : TButtonState;

    {property methods}
    procedure SetCaption(Value : TCaption);
    procedure SetMenuPosition(Value : TEsMenuPosition);
    procedure SetShowGlyph(Value : Boolean);
    procedure SetGlyph(Glyph: TBitmap);                              {!!.09}

    {internal methods}
    procedure cbDrawFocusRect;

    {vcl message methods}
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg : TMessage);
      message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage);
      message CM_SYSCOLORCHANGE;

    procedure CMDialogKey(var Message: TCMDialogKey);                {!!.09}
      message CM_DIALOGKEY;                                          {!!.09}
    procedure CMDialogChar(var Message: TCMDialogChar);              {!!.09}
      message CM_DIALOGCHAR;                                         {!!.09}


    {windows message methods}
    procedure WMGetDlgCode(var Msg : TWMGetDlgCode);
      message WM_GETDLGCODE;
    procedure WMKillFocus(var Message : TWMKillFocus);
      message WM_KILLFOCUS;
    procedure WMRButtonUp(var Msg : TWMRButtonUp);
      message WM_RBUTTONUP;
    procedure WMSetFocus(var Message : TWMSetFocus);
      message WM_SETFOCUS;
    procedure WMSize(var Message : TWMSize);
      message WM_SIZE;
    {.Z-}

  protected
    {.Z+}
    procedure KeyDown(var Key : Word; Shift : TShiftState);
      override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure Paint;
      override;
    {.Z-}

    property Caption : TCaption
      read FCaption
      write SetCaption;

    property MenuPosition : TEsMenuPosition
      read FMenuPosition
      write SetMenuPosition
      default mpBelow;

    property ShowGlyph : Boolean
      read FShowGlyph
      write SetShowGlyph
      default True;

    property Glyph : TBitmap                                         {!!.09}
      read GetGlyph write SetGlyph;                                  {!!.09}

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    procedure Click;
      override;
    destructor Destroy;
      override;
    {.Z-}
  end;

  TEsMenuButton = class(TEsCustomMenuButton)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property Caption;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EsLabelInfo;
    property Font;
    property Glyph;                                                  {!!.09}
    property MenuPosition;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowGlyph;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Version;
    property Visible;

    {events}
    property OnClick;
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
  end;


implementation
uses
  Forms;



procedure TEsCustomMenuButton.cbDrawFocusRect;
var
  R : TRect;
begin
  {draw focus rect around caption}
  Canvas.Pen.Color := clBtnFace;                                       {!!.01}
  R := Bounds(3, 3, ClientWidth-6, ClientHeight-6);
  if mbState = bsDown then
    OffsetRect(R, 1, 1);
  Canvas.DrawFocusRect(R);
end;

procedure TEsCustomMenuButton.Click;
var
  P : TPoint;
begin
  if PopupMenu <> nil then begin
    case FMenuPosition of
      mpBelow : P := Point(0, Height);
      mpRight : P := Point(Width, 0);
    end;
    P := ClientToScreen(P);
    PopupMenu.PopupComponent := Self;
    PopupMenu.Popup(P.X, P.Y);
  end;

  inherited Click;
end;

procedure TEsCustomMenuButton.CMEnabledChanged(var Message : TMessage);
begin
  inherited;

  Invalidate;
end;

procedure TEsCustomMenuButton.CMFontChanged(var Msg : TMessage);
begin
  inherited;

  Invalidate;
end;

procedure TEsCustomMenuButton.CMSysColorChange(var Message : TMessage);
begin
  inherited;

  Invalidate;
end;

constructor TEsCustomMenuButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csDoubleClicks];
  TabStop := True;

  Width := 75;
  Height := 24;
  ParentFont := True;

  FMenuPosition := mpBelow;
  FShowGlyph := True;

  {load the down arrow bitmap}
  mbGlyph := TBitmap.Create;
  mbGlyph.Handle := LoadBitmap(HInstance, 'ESDOWNARROW');

end;

destructor TEsCustomMenuButton.Destroy;
begin
  mbGlyph.Free;
  mbGlyph:= nil;

  mbImage.Free;
  mbImage := nil;

  inherited Destroy;
end;

procedure TEsCustomMenuButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case FMenuPosition of
    mpBelow :
      if (Key in [VK_UP, VK_DOWN]) and not (ssAlt in Shift) then begin
        mbState := bsDown;
        try
          Refresh;
          Click;
        finally
          mbState := bsUp;
          Refresh;
        end;
      end;
    mpRight :
      if (Key = VK_RIGHT) and not (ssAlt in Shift) then begin
        mbState := bsDown;
        try
          Refresh;
          Click;
        finally
          mbState := bsUp;
          Refresh;
        end;
      end;
  end;
end;

procedure TEsCustomMenuButton.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and Enabled then begin
    SetFocus;
    if not mbDown then begin
      mbState := bsDown;
      Repaint;
    end;
    mbDragging := True;
    Refresh;
  end;
end;

procedure TEsCustomMenuButton.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  NewState : TButtonState;
begin
  inherited MouseMove(Shift, X, Y);

  if mbDragging then begin
    NewState := mbState;
    if not mbDown then
      NewState := bsUp;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if mbDown then
        NewState := bsUp
      else
        NewState := bsDown;

    if NewState <> mbState then begin
      mbState := NewState;
      Repaint;
    end;
  end;
end;

procedure TEsCustomMenuButton.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if mbDragging then begin
    mbDragging := False;
    mbState := bsUp;
    Repaint;
    if mbDown and (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      Click;
  end;
end;

procedure TEsCustomMenuButton.Paint;
var
  X : Integer;
  Y : Integer;
  B : TBitmap;
  R : TRect;
  G : TImageList;
begin
  Canvas.Font := Font;

  {draw the button face}
  R := DrawButtonFace(Canvas, ClientRect, 1, bsNew, False, mbState = bsDown, False);
  with Canvas do begin
    {draw focus rect around caption if necessary}
    if Focused then
      cbDrawFocusRect;

    {draw caption}
    Inc(R.Left);
    if Length(FCaption) > 0 then begin
      if FShowGlyph and not mbGlyph.Empty then
        X := mbGlyph.Width + 2
      else
        X := 0;
      R.Left := (ClientWidth - TextWidth(FCaption) - X) div 2;

      if mbState = bsDown then begin
        Inc(R.Left);
        Inc(R.Top);
      end;

      DrawText(Canvas.Handle, @FCaption[1], Length(FCaption), R,
        DT_SINGLELINE or DT_VCENTER);
    end;

    {draw glyph}
    if FShowGlyph and Assigned(mbGlyph) and not mbGlyph.Empty then begin
      B := TBitmap.Create;
      try                                                              {!!.04}
        B.Assign(mbGlyph); {copy}
        {$IFDEF Win32}
        G := TImageList.CreateSize(B.Width, B.Height);
        {$ELSE}
        G := TImageList.Create(B.Width, B.Height);
        {$ENDIF}
        try
          G.AddMasked(B, B.Canvas.Pixels[0,0]);
          if Length(FCaption) = 0 then begin
            X := (Width - G.Width) shr 1;
            Y := (Height - G.Height) shr 1;
          end else begin
            X := R.Left+TextWidth(FCaption)+2;
            Y := (Height - G.Height) shr 1;
          end;

          if mbState = bsDown then
            Inc(Y);

          G.Draw(Canvas, X, Y, 0)
        finally
          G.Free;
        end;
      finally                                                          {!!.04}
        B.Free;                                                        {!!.04}
      end;                                                             {!!.04}
    end;
  end;
end;

procedure TEsCustomMenuButton.SetCaption(Value : TCaption);
begin
  if Value <> FCaption then begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TEsCustomMenuButton.SetMenuPosition(Value : TEsMenuPosition);
begin
  if Value <> FMenuPosition then begin
    FMenuPosition := Value;
    case FMenuPosition of
      mpBelow : mbGlyph.Handle := LoadBitmap(HInstance, 'ESDOWNARROW');
      mpRight : mbGlyph.Handle := LoadBitmap(HInstance, 'ESRIGHTARROW');
    end;
    Invalidate;
  end;
end;

procedure TEsCustomMenuButton.SetShowGlyph(Value : Boolean);
begin
  if Value <> FShowGlyph then begin
    FShowGlyph := Value;
    Invalidate;
  end;
end;

procedure TEsCustomMenuButton.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TEsCustomMenuButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  Invalidate;
end;

procedure TEsCustomMenuButton.WMRButtonUp(var Msg : TWMRButtonUp);
begin
  {ignore so that local menu is not displayed}
end;

procedure TEsCustomMenuButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  Invalidate;
end;

procedure TEsCustomMenuButton.WMSize(var Message:TWMSize);
begin
  inherited;

  Invalidate;
end;

{!!.09 -- Added }
procedure TEsCustomMenuButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if  (CharCode = VK_RETURN)  or
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TEsCustomMenuButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;
{!!.09 -- End Added }

{!!.09 -- Added }
procedure TEsCustomMenuButton.SetGlyph(Glyph: TBitmap);
begin
  if Assigned(mbGlyph) then begin
    mbGlyph.Free;
  end;
  mbGlyph := TBitmap.Create;
  mbGlyph.Assign(Glyph);
  Invalidate;
end;

function TEsCustomMenuButton.GetGlyph: TBitmap;
begin
  if not Assigned(mbGlyph) then
    mbGlyph := TBitmap.Create;
  Result := mbGlyph;
end;
{!!.09 -- End Added }

end.
