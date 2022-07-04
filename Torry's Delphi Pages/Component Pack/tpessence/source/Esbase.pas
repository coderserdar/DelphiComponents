
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

{$IFDEF WIN32}                                                         {!!.02}
  {$J+} {Writable constants}                                           {!!.02}
{$ENDIF}                                                               {!!.02}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsBase;
  {-essentials' base class}

interface

{$IFDEF Win32}
  {$R ESBASE.R32}
{$ELSE}
  {$R ESBASE.R16}
{$ENDIF Win32}

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, ExtCtrls, Forms, Graphics, Menus, Messages,
  StdCtrls, SysUtils,
  EsConst, EsData, EsLabel, EsUtil;

type
  TEsLabelPosition = (dlpTopLeft, dlpBottomLeft);

  EEssentialsError = class(Exception);

  TEsAttachEvent = procedure(Sender : TObject; Value : Boolean)
    of object;

type
  {.Z+}
  TEsAttachedLabel = class(TEsCustomLabel)
  private
    FEsControl    : TWinControl;

    {internal methods}
    procedure eslSavePosition;

  protected
    procedure Loaded;
      override;

  public
    constructor Create(AOwner : TComponent);
      override;
    constructor CreateEx(AOwner : TComponent; AControl : TWinControl);
      virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
      override;

  published
    {properties from TCustomLabel}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property Alignment;
    property Caption;
    property Color;
    property FocusControl;
    property Font;
    property Height;
    property Left;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowAccelChar;
    property ShowHint;
    property Tag;
    property Top;
    property Transparent;
    property Width;
    property WordWrap;

    {properties from TEsCustomLabel}
    property Appearance
      default apNone;

    property ColorScheme
      default csText;

    property CustomSettings;

    property EsControl : TWinControl
      read FEsControl
      write FEsControl;
  end;
  {.Z-}

type
  TEsLabelInfo = class(TPersistent)
  private
    {.Z+}
    {property variables}
    FOffsetX  : Integer;
    FOffsetY  : Integer;

    {event variables}
    FOnChange : TNotifyEvent;
    FOnAttach : TEsAttachEvent;

    {internal methods}
    procedure DoOnAttach;
    procedure DoOnChange;

    {property methods}
    procedure SetOffsetX(Value : Integer);
    procedure SetOffsetY(Value : Integer);
    procedure SetVisible(Value : Boolean);
    {.Z-}
  public
    {.Z+}
    ALabel   : TEsAttachedLabel;
    FVisible : Boolean;

    property OnAttach : TEsAttachEvent
      read FOnAttach
      write FOnAttach;

    property OnChange : TNotifyEvent
      read FOnChange
      write FOnChange;

    procedure SetOffsets(X, Y : Integer);
    {.Z-}

  published
    property OffsetX: Integer
      read FOffsetX
      write SetOffsetX
      nodefault;

    property OffsetY: Integer
      read FOffsetY
      write SetOffsetY
      nodefault;

    property Visible : Boolean
      read FVisible
      write SetVisible
      nodefault;
  end;

type
  {$IFDEF NeedMouseWheel}                                              {!!.05}
  TMouseWheelEvent = procedure(Sender : TObject; Shift : TShiftState; Delta, XPos, YPos : Word)
    of object;
  {$ENDIF}                                                             {!!.05}

  TEsBase = class(TCustomControl)
  protected {private}
    {.Z+}
    {property variables}
    FEsLabel : TEsLabelInfo;

    {event variables}
    {$IFDEF NeedMouseWheel}                                            {!!.05}
    FOnMouseWheel : TMouseWheelEvent;
    {$ENDIF}                                                           {!!.05}

    {property methods}
    function GetAttachedLabel : TEsAttachedLabel;
    function GetVersion : string;
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure LabelChange(Sender : TObject);
    procedure LabelAttach(Sender : TObject; Value : Boolean);
    procedure PositionLabel;

    {private message methods}
    procedure ESAssignLabel(var Msg : TMessage);
      message ES_ASSIGNLABEL;
    procedure ESPositionLabel(var Msg : TMessage);
      message ES_POSITIONLABEL;
    procedure ESRecordLabelPosition(var Msg : TMessage);
      message ES_RECORDLABELPOSITION;

    {windows message methods}
    {$IFDEF NeedMouseWheel}                                            {!!.05}
    procedure WMMouseWheel(var Msg : TMessage);
      message WM_MOUSEWHEEL;
    {$ENDIF}                                                           {!!.05}
    {.Z-}

  protected
    {descendants can set the value of this variable after calling inherited }
    {create to set the default location and point-of-reference (POR) for the}
    {attached label. if dlpTopLeft, the default location and POR will be at }
    {the top left of the control. if dlpBottomLeft, the default location and}
    {POR will be at the bottom left}
    {.Z+}
    DefaultLabelPosition : TEsLabelPosition;

    procedure Notification(AComponent : TComponent; Operation: TOperation);
      override;

    {$IFDEF NeedMouseWheel}                                            {!!.05}
    procedure DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
      dynamic;
    {$ENDIF}                                                           {!!.05}
    {.Z-}
    property EsLabelInfo : TEsLabelInfo
      read FEsLabel
      write FEsLabel;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);        {!!.05}
      override;
    {.Z-}
    property AttachedLabel : TEsAttachedLabel
      read GetAttachedLabel;

  published
    {$IFDEF NeedMouseWheel}                                            {!!.05}
    property OnMouseWheel : TMouseWheelEvent
      read FOnMouseWheel
      write FOnMouseWheel;
    {$ELSE}                                                            {!!.05}
    {$IFNDEF Windows}                                                  {!!.05}
    property OnMouseWheel;                                             {!!.05}
    {$ENDIF}                                                           {!!.05}
    {$ENDIF}                                                           {!!.05}
  end;


implementation


{*** TEsLabelInfo ***}

procedure TEsLabelInfo.DoOnAttach;
begin
  if Assigned(FOnAttach) then
    FOnAttach(Self, FVisible);
end;

procedure TEsLabelInfo.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEsLabelInfo.SetOffsets(X, Y : Integer);
begin
  if (X <> FOffsetX) or (Y <> FOffsetY) then begin
    FOffsetX := X;
    FOffsetY := Y;
    DoOnChange;
  end;
end;

procedure TEsLabelInfo.SetOffsetX(Value : Integer);
begin
  if Value <> FOffsetX then begin
    FOffsetX := Value;
    DoOnChange;
  end;
end;

procedure TEsLabelInfo.SetOffsetY(Value : Integer);
begin
  if Value <> FOffsetY then begin
    FOffsetY := Value;
    DoOnChange;
  end;
end;

procedure TEsLabelInfo.SetVisible(Value : Boolean);
begin
  if Value <> FVisible then begin
    FVisible := Value;
    DoOnAttach;
  end;
end;


{*** TEsAttachedLabel ***}

constructor TEsAttachedLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {set new defualts}
  AutoSize    := True;
  ColorScheme := csText;
  Appearance  := apNone;
  ParentFont  := True;
  Transparent := False;

end;

constructor TEsAttachedLabel.CreateEx(AOwner : TComponent; AControl : TWinControl);
begin
  FEsControl := AControl;

  Create(AOwner);
end;

procedure TEsAttachedLabel.eslSavePosition;
var
  PF : TForm;
  I  : Integer;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  {see if our associated control is on the form - save position}
  PF := TForm(GetParentForm(Self));
  if Assigned(PF) then begin
    for I := 0 to Pred(PF.ComponentCount) do begin
      if PF.Components[I] = FEsControl then begin
        SendMessage(FEsControl.Handle, ES_ASSIGNLABEL, 0, LongInt(Self));
        PostMessage(FEsControl.Handle, ES_RECORDLABELPOSITION, 0, 0);
        Break;
      end;
    end;
  end;
end;

procedure TEsAttachedLabel.Loaded;
begin
  inherited Loaded;

  eslSavePosition;
end;

procedure TEsAttachedLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  eslSavePosition;
end;


{*** TEsBase ***}

constructor TEsBase.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Height := 25;
  Width  := 75;

  {set default position and reference point}
  DefaultLabelPosition := dlpTopLeft;

  FEsLabel := TEsLabelInfo.Create;
  FEsLabel.OnChange := LabelChange;
  FEsLabel.OnAttach := LabelAttach;
end;

destructor TEsBase.Destroy;
begin
  {detatch and destroy label, if any}
  FEsLabel.Visible := False;

  {destroy label info}
  FEsLabel.Free;
  FEsLabel := nil;

  inherited Destroy;
end;

{$IFDEF NeedMouseWheel}                                                {!!.05}
procedure TEsBase.DoOnMouseWheel(Shift : TShiftState; Delta, XPos, YPos : SmallInt);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, Delta, XPos, YPos);
end;
{$ENDIF}                                                               {!!.05}

procedure TEsBase.ESAssignLabel(var Msg : TMessage);
begin
  FEsLabel.ALabel := TEsAttachedLabel(Msg.lParam);
end;

procedure TEsBase.ESPositionLabel(var Msg : TMessage);
const
  DX : Integer = 0;
  DY : Integer = 0;
begin
  if FEsLabel.Visible and Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) and
     not (csLoading in ComponentState) then begin
    if DefaultLabelPosition = dlpTopLeft then begin
      DX := FEsLabel.ALabel.Left - Left;
      DY := FEsLabel.ALabel.Top + FEsLabel.ALabel.Height - Top;
    end else begin
      DX := FEsLabel.ALabel.Left - Left;
      DY := FEsLabel.ALabel.Top - Top - Height;
    end;
    if (DX <> FEsLabel.OffsetX) or (DY <> FEsLabel.OffsetY) then
      PositionLabel;
  end;
end;

procedure TEsBase.ESRecordLabelPosition(var Msg : TMessage);
begin
  if Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) then begin
    {if the label was cut and then pasted, this will complete the re-attachment}
    FEsLabel.FVisible := True;

    if DefaultLabelPosition = dlpTopLeft then
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
        FEsLabel.ALabel.Top + FEsLabel.ALabel.Height - Top)
    else
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
        FEsLabel.ALabel.Top - Top - Height);
  end;
end;

function TEsBase.GetAttachedLabel : TEsAttachedLabel;
begin
  if not FEsLabel.Visible then
    raise EEssentialsError.Create(StrRes[SCEsLabelNotAttached]);

  Result := FEsLabel.ALabel;
end;

function TEsBase.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsBase.LabelAttach(Sender : TObject; Value : Boolean);
var
  PF : TForm;
begin
  if csLoading in ComponentState then
    Exit;

  PF := TForm(GetParentForm(Self));
  if Value then begin
    if Assigned(PF) then begin
      FEsLabel.ALabel.Free;
      FEsLabel.ALabel := TEsAttachedLabel.CreateEx(PF, Self);
      FEsLabel.ALabel.Parent := Parent;
      FEsLabel.ALabel.Caption := Name+'Label';
      FEsLabel.SetOffsets(0, 0);
      PositionLabel;
      FEsLabel.ALabel.BringToFront;
      {turn off auto size}
      FEsLabel.ALabel.AutoSize := False;
    end;
  end else begin
    if Assigned(PF) then begin
      FEsLabel.ALabel.Free;
      FEsLabel.ALabel := nil;
    end;
  end;
end;

procedure TEsBase.LabelChange(Sender : TObject);
begin
  if not (csLoading in ComponentState) then
    PositionLabel;
end;

procedure TEsBase.Notification(AComponent : TComponent; Operation: TOperation);
var
  PF : TForm;
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if Assigned(FEsLabel) and (AComponent = FEsLabel.ALabel) then begin
      PF := TForm(GetParentForm(Self));
      if Assigned(PF) and not (csDestroying in PF.ComponentState) then begin
        FEsLabel.FVisible := False;
        FEsLabel.ALabel := nil;
      end
    end;
end;

procedure TEsBase.PositionLabel;
begin
  if FEsLabel.Visible and Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) and
     not (csLoading in ComponentState) then begin

    if DefaultLabelPosition = dlpTopLeft then begin
      FEsLabel.ALabel.SetBounds(Left + FEsLabel.OffsetX,
                         FEsLabel.OffsetY - FEsLabel.ALabel.Height + Top,
                         FEsLabel.ALabel.Width, FEsLabel.ALabel.Height);
    end else begin
      FEsLabel.ALabel.SetBounds(Left + FEsLabel.OffsetX,
                         FEsLabel.OffsetY + Top + Height,
                         FEsLabel.ALabel.Width, FEsLabel.ALabel.Height);
    end;
  end;
end;

procedure TEsBase.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if HandleAllocated then
    PostMessage(Handle, ES_POSITIONLABEL, 0, 0);
end;

procedure TEsBase.SetVersion(const Value : string);
begin
end;

{$IFDEF NeedMouseWheel}                                        {!!.05}
procedure TEsBase.WMMouseWheel(var Msg : TMessage);
begin
  inherited;

  with Msg do
    DoOnMouseWheel(KeysToShiftState(LOWORD(wParam)) {fwKeys},
                   HIWORD(wParam) {zDelta},
                   LOWORD(lParam) {xPos},   HIWORD(lParam) {yPos});
end;
{$ENDIF}                                                       {!!.05}

initialization
  {register the attached label class}
  if Classes.GetClass(TEsAttachedLabel.ClassName) = nil then
    Classes.RegisterClass(TEsAttachedLabel);

end.