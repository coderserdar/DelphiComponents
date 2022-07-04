
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
  {$C FIXED PRELOAD}
{$ENDIF}

unit EsMarque;
  {-scrolling marquee component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, ExtCtrls, Forms, Graphics, Messages, MMSystem, StdCtrls,
  SysUtils,                                                            {!!.01}
  EsData, EsLabel;

const
  SM_TIMER = WM_USER + 500;

type
  TEsBehaviors = (bhCycle, bhBounce, bhOnce);
  TEsScrollDirection = (sdRightToLeft, sdLeftToRight, sdTopToBottom, sdBottomToTop);
  TEsScrollSpeed = 0..10;

  TEsCustomScrollingMarquee = class(TEsCustomLabel)
  protected {private}
    {.Z+}
    {property variables}
    FActive          : Boolean;
    FBehavior        : TEsBehaviors;
    FScrollSpeed     : TEsScrollSpeed;
    FScrollDirection : TEsScrollDirection;

    {event variables}
    FOnCycle         : TNotifyEvent;

    {internal variables}
    smBmp            : TBitmap;
    smDC             : hDC;       {temp device context}
    smDelay          : UINT;
    smDirty          : Boolean;
    smHandle         : hWnd;
    smOldBmp         : hBitmap;
    smPos            : TPoint;    {current position of text}
    smResolution     : UINT;
    smTextHeight     : Integer;
    smTextWidth      : Integer;
    smTimerId        : MMRESULT;

    {internal methods}
    procedure smChanged;
    procedure smKillTimer;
    procedure smSetTimer;
    procedure smWndProc(var Message : TMessage);

    {property methods}
    function GetAlignment : TAlignment;
    procedure SetActive(Value : Boolean);
    procedure SetAlignment(Value : TAlignment);
    procedure SetBehavior(Value : TEsBehaviors);
    procedure SetScrollDirection(Value : TEsScrollDirection);
    procedure SetScrollSpeed(Value : TEsScrollSpeed);

    {vcl message methods}
    procedure CMColorChanged(var Message : TCMExit);
      message CM_COLORCHANGED;
    procedure CMFontChanged(var Message : TCMExit);
      message CM_FONTCHANGED;
    procedure CMTextChanged(var Message : TCMExit);
      message CM_TEXTCHANGED;
    {.Z-}

  protected
    {.Z+}
    procedure Paint;
      override;

    procedure DoOnCycle;
      dynamic;
    {.Z-}

    {protected properties}
    property Active : Boolean
      read FActive
      write SetActive
      default False;

    property Alignment : TAlignment
      read GetAlignment
      write SetAlignment;

    property Behavior : TEsBehaviors
      read FBehavior
      write SetBehavior
      default bhCycle;

    property ScrollDirection : TEsScrollDirection
      read FScrollDirection
      write SetScrollDirection
      default sdRightToLeft;

    property ScrollSpeed : TEsScrollSpeed
      read FScrollSpeed
      write SetScrollSpeed;

    {protected Events}
    property OnCycle : TNotifyEvent
      read FOnCycle
      write FOnCycle;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure Invalidate;                                              {!!.05}
      override;
    {.Z-}
  end;

  TEsScrollingMarquee = class(TEsCustomScrollingMarquee)
  published
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    {properties}
    property Active;
    property Align;
    property Alignment;
    property Appearance;
    property Behavior;
    property Caption;
    property Color;
    property ColorScheme;
    property Cursor;
    property CustomSettings;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ScrollDirection;
    property ScrollSpeed;
    property ShowAccelChar;
    property ShowHint;
    property WordWrap;
    property Version;
    property Visible;

    {events}
    property OnClick;
    property OnCycle;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation


const
  MaxDelay    = 20;
  MinDelay    = MaxDelay div High(TesScrollSpeed);
  SafetyMargin = 15;

{timer callback routine}
procedure smTimerProc(uTimerId, uMessage : UINT; dwUser, dw1, dw2 : DWORD);
  {$IFDEF Win32} stdcall; {$ELSE} far; {$ENDIF}
begin
  PostMessage(dwUser, SM_TIMER, 0, uTimerId);
end;


{*** TEsCustomScrollingMarquee ***}

procedure TEsCustomScrollingMarquee.CMColorChanged(var Message : TCMExit);
begin
  inherited;

  smChanged;
  Invalidate;
end;

procedure TEsCustomScrollingMarquee.CMFontChanged(var Message : TCMExit);
begin
  inherited;

  smChanged;
  Invalidate;
end;

procedure TEsCustomScrollingMarquee.CMTextChanged(var Message : TCMExit);
begin
  inherited;

  smChanged;
  Invalidate;
end;

constructor TEsCustomScrollingMarquee.Create(AOwner : TComponent);
var
  TimerCaps : TTimeCaps;
begin
  inherited Create(AOwner);

  Transparent := False;
  FActive := False;
  FBehavior := bhCycle;
  FScrollSpeed := 5;
  FScrollDirection := sdRightToLeft;

  {create our window handle}
{$IFDEF VER140}
  smHandle := Classes.AllocateHWnd(smWndProc);                       {!!.09}
{$ELSE}
  smHandle := AllocateHWnd(smWndProc);
{$ENDIF}


  {create our drawing bitmap}
  smBmp := TBitmap.Create;

  {set flag to indicate that our bitmap is dirty}
  smDirty := True;

  {query timer resolution and adjust our requested resolution if necessary}
  timeGetDevCaps(@TimerCaps, SizeOf(TimerCaps));
  smResolution := MinDelay;
  if TimerCaps.wPeriodMin > smResolution then
    smResolution := TimerCaps.wPeriodMin;
  if TimerCaps.wPeriodMax < smResolution then
    smResolution := TimerCaps.wPeriodMax;

  smSetTimer;

end;

destructor TEsCustomScrollingMarquee.Destroy;
begin
  smKillTimer;

  {destroy our window handle}
  if smHandle <> 0 then begin
{$IFDEF VER140}
    Classes.DeallocateHWnd(smHandle);                                {!!.09}
{$ELSE}
    DeallocateHWnd(smHandle);
{$ENDIF}
    smHandle := 0;
  end;

  if smDC <> 0 then begin
    smBmp.Handle := SelectObject(smDC, smOldBmp);
    DeleteDC(smDC);
  end;

  smBmp.Free;
  smBmp := nil;

  inherited Destroy;
end;

procedure TEsCustomScrollingMarquee.DoOnCycle;
begin
  if Assigned(FOnCycle) then
    FOnCycle(Self);
  if FBehavior = bhOnce then
    SetActive(False);
end;

function TEsCustomScrollingMarquee.GetAlignment : TAlignment;
begin
  Result := inherited Alignment;
end;

{!!.04} {added}
procedure TEsCustomScrollingMarquee.Invalidate;
begin
  inherited Invalidate;

  smDirty := True;
end;

procedure TEsCustomScrollingMarquee.Paint;
const
  Alignments : array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  Wrap : array[Boolean] of Word = (0, DT_WORDBREAK);                   {!!.04}
  Prefix : array[Boolean] of Word = (DT_NOPREFIX, 0);                  {!!.04}
var
  W     : Integer;
  H     : Integer;
  Flags : Word;
begin
  {determine the size of the bitmap}
  if FScrollDirection in [sdTopToBottom, sdBottomToTop] then begin
    W := ClientWidth;
    H := ClientHeight + smTextHeight;                                  {!!.01}
  end else begin
    W := ClientWidth + smTextWidth;
    H := ClientHeight + smTextHeight;
  end;

  if smDirty or smBmp.Empty or (smDC = 0) then begin
    if smDirty and (smDC <> 0) then
      SelectObject(smDC, smOldBmp);

    {create a temporary device context}
    if smDC = 0 then
      smDC := CreateCompatibleDC(Canvas.Handle);

    {create a bitmap to draw on}
    smBmp.Handle := CreateCompatibleBitmap(Canvas.Handle, W, H);

    {select the bitmap into the temporary DC}
    smOldBmp := SelectObject(smDC, smBmp.Handle);

    Flags := Prefix[ShowAccelChar] or DT_EXPANDTABS;                   {!!.04}
    if FScrollDirection in [sdTopToBottom, sdBottomToTop] then         {!!.04}
      Flags := Flags or Wrap[WordWrap] or Alignments[Alignment];       {!!.04}

    {draw the text to our DC}
    PaintTo(smDC, Rect(0, 0, W, H), Flags);
    smDirty := False;
  end;

  {clear area trailing movement}
  Canvas.Brush.Color := Color;

  {copy DC to our canvas}
  BitBlt(Canvas.Handle, smPos.X, smPos.Y, W, H, smDC, 0, 0, SRCCOPY);

  case FScrollDirection of
    sdLeftToRight : FillRect(Canvas.Handle, Rect(0, 0, smPos.X, ClientHeight), Canvas.Brush.Handle);
    sdTopToBottom : FillRect(Canvas.Handle, Rect(0, 0, ClientWidth, smPos.Y), Canvas.Brush.Handle);
  end;
end;

procedure TEsCustomScrollingMarquee.SetActive(Value : Boolean);
begin
  if Value <> FActive then begin
    FActive := Value;

    {reset position}
    smPos.X := 0;
    smPos.Y := 0;

    {determine starting position}
    if FActive then begin
      smChanged;
      {set initial position}
      case FScrollDirection of
        sdRightToLeft :
          if FBehavior = bhBounce then
            smPos.X := ClientWidth - smTextWidth
          else
            smPos.X := ClientWidth;
        sdLeftToRight :
          if FBehavior = bhBounce then
            smPos.X := 0
          else
            smPos.X := -smTextWidth;
        sdTopToBottom :
          if FBehavior = bhBounce then
            smPos.Y := 0
          else
            smPos.Y := -smTextHeight;
        sdBottomToTop :
          if FBehavior = bhBounce then
            smPos.Y := ClientHeight - smTextHeight
          else
            smPos.Y := ClientHeight;
      end;
    end;

    {clear}
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);

    {force repaint}
    Invalidate;
  end;

  smSetTimer;                                                          {!!.06}
end;

procedure TEsCustomScrollingMarquee.SetAlignment(Value : TAlignment);
begin
  inherited Alignment := Value;
  smDirty := True;
end;

procedure TEsCustomScrollingMarquee.SetBehavior(Value : TEsBehaviors);
var
  WasActive : Boolean;
begin
  if Value <> FBehavior then begin
    WasActive := Active;
    SetActive(False);
    FBehavior := Value;
    SetActive(WasActive);
  end;
end;

procedure TEsCustomScrollingMarquee.SetScrollDirection(Value : TEsScrollDirection);
var
  WasActive : Boolean;
begin
  if Value <> FScrollDirection then begin
    WasActive := Active;
    SetActive(False);
    FScrollDirection := Value;
    SetActive(WasActive);
  end;
end;

procedure TEsCustomScrollingMarquee.SetScrollSpeed(Value : TEsScrollSpeed);
begin
  if (Value <> FScrollSpeed) and (Value >= 0) and (Value <= High(TEsScrollSpeed)) then begin
    FScrollSpeed := Value;
    smKillTimer;
    if Value > 0 then
      smSetTimer;
    Invalidate;
  end;
end;

{!!.01} {revised}
procedure TEsCustomScrollingMarquee.smChanged;
var
  R     : TRect;
  Flags : Word;
  Buf   : PChar;                                                       {!!.03}
begin
  smDirty := True;
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;

  {must have a parent window before attempting this}
  if Parent <> nil then begin
    smTextWidth := Canvas.TextWidth(Caption);
    {determine height of text}
    R := ClientRect;

    Buf := StrAlloc(Length(Text)+1);                                   {!!.03}
    try                                                                {!!.03}
      StrPLCopy(Buf, Text, Length(Text)+1);  {get label's caption}     {!!.05}
      Flags := DT_CALCRECT;
      if WordWrap then
        Flags := Flags or DT_WORDBREAK;
      smTextHeight := DrawText(Canvas.Handle, Buf, StrLen(Buf), R, Flags);
    finally                                                            {!!.03}
      StrDispose(Buf);                                                 {!!.03}
    end;                                                               {!!.03}
  end;
end;

procedure TEsCustomScrollingMarquee.smKillTimer;
begin
  if smTimerId <> 0 then
    timeKillEvent(smTimerId);
  smTimerId := 0;
end;

procedure TEsCustomScrollingMarquee.smSetTimer;
var
  R : UINT;
begin
  smKillTimer;                                                         {!!.06}

  smDelay := Trunc(MaxDelay * (High(TEsScrollSpeed) - FScrollSpeed) / High(TEsScrollSpeed));
  if smDelay < MinDelay then
    smDelay := MinDelay;

  R := smDelay;
  if R < smResolution then
    R := smResolution;

  if FActive then begin                                                {!!.06}
    {$IFDEF Win32}
    smTimerId := timeSetEvent(smDelay, R, @smTimerProc, smHandle, TIME_PERIODIC);
    {$ELSE}
    smTimerId := timeSetEvent(smDelay, R, smTimerProc, smHandle, TIME_PERIODIC);
    {$ENDIF}
  end;
end;

procedure TEsCustomScrollingMarquee.smWndProc(var Message : TMessage);
begin
  if Message.Msg = SM_TIMER then begin
    if FActive then begin
      case FScrollDirection of
        sdRightToLeft :
          begin
            Dec(smPos.X);
            if (FBehavior = bhBounce) and (smTextWidth < ClientWidth) then begin
              if smPos.X < 0 then begin
                FScrollDirection := sdLeftToRight;
                DoOnCycle;
              end;
            end else if smPos.X < -smTextWidth-SafetyMargin then begin
              smPos.X := ClientWidth;
              DoOnCycle;
            end;
          end;
        sdLeftToRight :
          begin
            Inc(smPos.X);
            if (FBehavior = bhBounce) and (smTextWidth < ClientWidth) then begin
              if smPos.X > ClientWidth-smTextWidth then begin
                FScrollDirection := sdRightToLeft;
                DoOnCycle;
              end;
            end else if smPos.X > ClientWidth+SafetyMargin then begin
              smPos.X := -smTextWidth;
              DoOnCycle;
            end;
          end;
        sdTopToBottom :
          begin
            Inc(smPos.Y);
            if (FBehavior = bhBounce) and (smTextHeight < ClientHeight) then begin
              if smPos.Y > ClientHeight-smTextHeight then begin
                FScrollDirection := sdBottomToTop;
                DoOnCycle;
              end;
            end else if smPos.Y > ClientHeight+SafetyMargin then begin
              smPos.Y := -smTextHeight;
              DoOnCycle;
            end;
          end;
        sdBottomToTop :
          begin
            Dec(smPos.Y);
            if (FBehavior = bhBounce) and (smTextHeight < ClientHeight) then begin
              if smPos.Y < 0 then begin
                FScrollDirection := sdTopToBottom;
                DoOnCycle;
              end;
            end else if smPos.Y < -smTextHeight-SafetyMargin then begin
              smPos.Y := ClientHeight;
              DoOnCycle;
            end;
          end;
      end;
      Invalidate;
    end;
  end;

  with Message do                                                      {!!.03}
    Result := DefWindowProc(smHandle, Msg, wParam, lParam);            {!!.03}
end;


end.
