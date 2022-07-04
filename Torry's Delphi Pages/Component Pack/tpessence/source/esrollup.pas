
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

unit EsRollUp;

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Forms, Messages, MMSystem,
  EsConst, EsData;

const
  ruDefAnimate      = True;
  ruDefAnimateSpeed = 8;

type
  TEsAnimateSpeed = 0..10;

  TEsCustomRollUp = class(TComponent)
  protected {private}
    {.Z+}
    {property variables}
    FAnimate      : Boolean;
    FAnimateSpeed : TEsAnimateSpeed;
    FHookForm     : Boolean;
    FMinHeight    : Integer;

    {event variables}
    FOnRollDown: TNotifyEvent;
    FOnRollUp  : TNotifyEvent;

    {internal variables}
    ruNewWndProc  : TFarProc;
    ruOldHeight   : Integer;
    ruPrevWndProc : TFarProc;

    {property methods}
    function GetRolledUp : Boolean;
    function GetVersion : string;
    procedure SetAnimateSpeed(Value : TEsAnimateSpeed);
    procedure SetHookForm(Value : Boolean);
    procedure SetMinHeight(Value : Integer);
    procedure SetRolledUp(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure ruWndProc(var Msg : TMessage);
    {.Z-}

  protected
    {.Z+}
    procedure DoOnRollDown;
      dynamic;
    procedure DoOnRollUp;
      dynamic;
    {.Z-}

    {properties}
    property Animate : Boolean
      read FAnimate
      write FAnimate
      default ruDefAnimate;

    property AnimateSpeed : TEsAnimateSpeed
      read FAnimateSpeed
      write FAnimateSpeed
      default ruDefAnimateSpeed;

    property HookForm : Boolean
      read FHookForm
      write SetHookForm
      default False;

    property MinHeight : Integer
      read FMinHeight
      write SetMinHeight;

    property RolledUp : Boolean
      read GetRolledUp
      write SetRolledUp
      stored False;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    {events}
    property OnRollDown : TNotifyEvent
      read FOnRollDown
      write FOnRollDown;

    property OnRollUp : TNotifyEvent
      read FOnRollUp
      write FOnRollUp;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    {.Z-}
  end;

  TEsRollUp = class(TEsCustomRollUp)
  published
    {properties}
    property Animate;
    property AnimateSpeed;
    property HookForm;
    property MinHeight;
    property RolledUp;
    property Version;

    {events}
    property OnRollDown;
    property OnRollUp;
  end;


implementation


constructor TEsCustomRollUp.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {create instance of our window procedure}
{$IFDEF VER140}
  ruNewWndProc := Classes.MakeObjectInstance(ruWndProc);             {!!.09}
{$ELSE}
  ruNewWndProc := MakeObjectInstance(ruWndProc);
{$ENDIF}

  FAnimate      := ruDefAnimate;
  FAnimateSpeed := ruDefAnimateSpeed;
  FHookForm     := False;
  FMinHeight    := 0;

end;

destructor TEsCustomRollUp.Destroy;
begin
  {restore old wnd proc}
  SetHookForm(False);

{$IFDEF VER140}
  Classes.FreeObjectInstance(ruNewWndProc);                          {!!.09}
{$ELSE}
  FreeObjectInstance(ruNewWndProc);
{$ENDIF}

  inherited Destroy;
end;

procedure TEsCustomRollUp.DoOnRollDown;
begin
  if Assigned(FOnRollDown) then
    FOnRollDown(Self);
end;

procedure TEsCustomRollUp.DoOnRollUp;
begin
  if Assigned(FOnRollUp) then
    FOnRollUp(Self);
end;

function TEsCustomRollUp.GetRolledUp : Boolean;
begin
  Result := (Owner is TForm) and (FMinHeight >= TForm(Owner).ClientHeight);
end;

function TEsCustomRollUp.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsCustomRollUp.ruWndProc(var Msg : TMessage);
begin
  with Msg do begin
    if (Msg = WM_SYSCOMMAND) and (wParam = SC_MINIMIZE) then begin
      if not IsIconic(TForm(Owner).Handle) and not RolledUp then begin
        {click on minimize button}
        try
          SetRolledUp(True);
        except
          Application.HandleException(Self);
        end;
      end else
        if Assigned(ruPrevWndProc) then
          Result := CallWindowProc(ruPrevWndProc, TForm(Owner).Handle, Msg, wParam, lParam);
    end else if (Msg = WM_SYSCOMMAND) and (wParam = SC_MAXIMIZE) then begin
      if not IsZoomed(TForm(Owner).Handle) and RolledUp then begin
        {click on maximize button}
        try
          SetRolledUp(False);
        except
          Application.HandleException(Self);
        end;
      end else
        if Assigned(ruPrevWndProc) then
          Result := CallWindowProc(ruPrevWndProc, TForm(Owner).Handle, Msg, wParam, lParam);
    end else
      if Assigned(ruPrevWndProc) then
        Result := CallWindowProc(ruPrevWndProc, TForm(Owner).Handle, Msg, wParam, lParam);
  end;
end;

procedure TEsCustomRollUp.SetAnimateSpeed(Value : TEsAnimateSpeed);
begin
  if (Value <> FAnimateSpeed) then
    FAnimateSpeed := Value;
end;

procedure TEsCustomRollUp.SetHookForm(Value : Boolean);
begin
  if (Owner is TForm) and (Value <> FHookForm) then begin
    FHookForm := Value;
    if not (csDesigning in ComponentState) then begin
      if Value then begin
        ruPrevWndProc:= Pointer(
          SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(ruNewWndProc)))
      end else if Assigned(ruPrevWndProc) then begin
        SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(ruPrevWndProc));
        ruPrevWndProc := nil;
      end;
    end;
  end;
end;

procedure TEsCustomRollUp.SetMinHeight(Value : Integer);
var
  WasRolledUp : Boolean;
begin
  if (Value >= 0) and (Value <> FMinHeight) then begin
    WasRolledUp := RolledUp;
    FMinHeight := Value;
    if WasRolledUp then
      RolledUp := True;
  end;
end;

procedure TEsCustomRollUp.SetRolledUp(Value : Boolean);
const
  TMult = 5;
var
  I           : Integer;
  Form        : TForm;
  SC          : Boolean;
  Step        : Integer;
  SpeedFactor : Integer;
  T           : DWord;                                                 {!!.05}
begin
  if (Owner is TForm) then begin
    Form := TForm(Owner);
    SpeedFactor := High(FAnimateSpeed) - FAnimateSpeed + 1;
    if Value then begin
      ruOldHeight := Form.ClientHeight;
      SC := Form.AutoScroll;
      Form.AutoScroll := False;
      try
        if FAnimate then begin
          Step := ruOldHeight div (SpeedFactor * 3);
          if Step < 2 then Step := 2;
          I := ruOldHeight+Step;
          while I > FMinHeight do begin
            Dec(I, Step);
            if I < FMinHeight then
              I := FMinHeight;
            Form.ClientHeight := I;
            Application.ProcessMessages;
            if FAnimateSpeed <> High(FAnimateSpeed) then begin
              T := timeGetTime;
              while Abs(timeGetTime - T) < (SpeedFactor * TMult) do {wait};
            end;
          end;
        end else
          Form.ClientHeight := FMinHeight;
      finally
        Form.AutoScroll := SC;
      end;

      FMinHeight := Form.ClientHeight;
      DoOnRollUp;
    end else begin
      if (ruOldHeight < FMinHeight) then
        ruOldHeight := FMinHeight;

      SC := Form.AutoScroll;
      Form.AutoScroll := False;
      try
        if FAnimate then begin
          Step := ruOldHeight div (SpeedFactor * 3);
          if Step < 2 then Step := 2;
          I := FMinHeight-Step;
          while I < ruOldHeight do begin
            Inc(I, Step);
            if I > ruOldHeight then
              I := ruOldHeight;
            Form.ClientHeight := I;
            Application.ProcessMessages;
            if FAnimateSpeed <> High(FAnimateSpeed) then begin
              T := timeGetTime;
              while Abs(timeGetTime - T) < (SpeedFactor * TMult) do {wait};
            end;
          end;
        end else
          Form.ClientHeight := ruOldHeight;
      finally
        Form.AutoScroll := SC;
      end;

      DoOnRollDown;
    end;
  end;
end;

procedure TEsCustomRollup.SetVersion(const Value : string);
begin
end;

end.
