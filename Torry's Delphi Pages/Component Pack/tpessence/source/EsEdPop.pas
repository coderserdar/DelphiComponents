
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

unit EsEdPop;
  {-base popup edit field class}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, ExtCtrls, Forms, Graphics, Menus, Messages,
  StdCtrls, SysUtils,
  EsBase, EsConst, EsData, EsLabel, EsUtil;

type
  {.Z+}
  TEsEdButton = class(TBitBtn)
  public                                                               {!!.05}
     procedure Click;
       override;
  end;
  {.Z-}

  TEsEdPopup = class(TCustomEdit)
  protected {private}
    {.Z+}
    {property variables}
    FButton      : TEsEdButton;
    FEsLabel     : TEsLabelInfo;
    FPopupActive : Boolean;
    FShowButton  : Boolean;

    {property methods}
    function GetAttachedLabel : TEsAttachedLabel;
    function GetVersion : string;
    procedure SetShowButton(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    function GetButtonWidth : Integer;
    procedure LabelChange(Sender : TObject);
    procedure LabelAttach(Sender : TObject; Value : Boolean);
    procedure PositionLabel;

    procedure ESAssignLabel(var Msg : TMessage);
      message ES_ASSIGNLABEL;
    procedure ESPositionLabel(var Msg : TMessage);
      message ES_POSITIONLABEL;
    procedure ESRecordLabelPosition(var Msg : TMessage);
      message ES_RECORDLABELPOSITION;
    {.Z-}

  protected
    {.Z+}
    {descendants can set the value of this variable after calling inherited }
    {create to set the default location and point-of-reference (POR) for the}
    {attached label. if dlpTopLeft, the default location and POR will be at }
    {the top left of the control. if dlpBottomLeft, the default location and}
    {POR will be at the bottom left}
    DefaultLabelPosition : TEsLabelPosition;

    procedure CreateParams(var Params : TCreateParams);
      override;
    procedure CreateWnd;
      override;
    function GetButtonEnabled : Boolean;                               {!!.03}
      dynamic;
    procedure Notification(AComponent : TComponent; Operation: TOperation);
      override;
    procedure PopupClose(Sender : TObject);
      dynamic;
    {.Z-}

    property EsLabelInfo : TEsLabelInfo
      read FEsLabel
      write FEsLabel;

    property ShowButton : Boolean
      read FShowButton
      write SetShowButton
      default True;

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

    {moved from protected to public}
    procedure PopupOpen;                                               {!!.02}
      dynamic;

    {.Z+}
    property PopupActive : Boolean
      read FPopupActive;
    {.Z-}
  end;


implementation


{*** TEsEditBtn ***}

procedure TEsEdButton.Click;
begin
  TEsEdPopup(Parent).PopupOpen;
end;


{*** TEsEdPopup ***}

constructor TEsEdPopup.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FShowButton := True;
  FButton := TEsEdButton.Create(Self);
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.Caption := '';
  FButton.TabStop := False;
  FButton.Style := bsNew;

  {set default position and reference point}
  DefaultLabelPosition := dlpTopLeft;

  FEsLabel := TEsLabelInfo.Create;
  FEsLabel.OnChange := LabelChange;
  FEsLabel.OnAttach := LabelAttach;
end;

procedure TEsEdPopup.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TEsEdPopup.CreateWnd;
begin
  inherited CreateWnd;

  {force button placement}
  SetBounds(Left, Top, Width, Height);

  FButton.Enabled := GetButtonEnabled;                                 {!!.03}

end;

destructor TEsEdPopup.Destroy;
begin
  {detatch and destroy label, if any}
  FEsLabel.Visible := False;

  {destroy label info}
  FEsLabel.Free;
  FEsLabel := nil;

  FButton.Free;
  FButton := nil;

  inherited Destroy;
end;

procedure TEsEdPopup.ESAssignLabel(var Msg : TMessage);
begin
  FEsLabel.ALabel := TEsAttachedLabel(Msg.lParam);
end;

procedure TEsEdPopup.ESPositionLabel(var Msg : TMessage);
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

procedure TEsEdPopup.ESRecordLabelPosition(var Msg : TMessage);
begin
  if Assigned(FEsLabel.ALabel) and (FEsLabel.ALabel.Parent <> nil) then begin
    {if the label was cut and then pasted, this will complete the reattachment}
    FEsLabel.FVisible := True;

    if DefaultLabelPosition = dlpTopLeft then
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
                          FEsLabel.ALabel.Top + FEsLabel.ALabel.Height - Top)
    else
      FEsLabel.SetOffsets(FEsLabel.ALabel.Left - Left,
                          FEsLabel.ALabel.Top - Top - Height);
  end;
end;

function TEsEdPopup.GetAttachedLabel : TEsAttachedLabel;
begin
  if not FEsLabel.Visible then
    raise EEssentialsError.Create(StrRes[SCEsLabelNotAttached]);

  Result := FEsLabel.ALabel;
end;

{!!.03}
function TEsEdPopup.GetButtonEnabled : Boolean;
begin
  Result := not ReadOnly;
end;

function TEsEdPopup.GetButtonWidth : Integer;
begin
  if Assigned(FButton) and FShowButton then
    Result := FButton.Width
  else
    Result := 0;
end;

function TEsEdPopup.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsEdPopup.LabelAttach(Sender : TObject; Value : Boolean);
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
      TLabel(FEsLabel.ALabel).AutoSize := False;
    end;
  end else begin
    if Assigned(PF) then begin
      FEsLabel.ALabel.Free;
      FEsLabel.ALabel := nil;
    end;
  end;
end;

procedure TEsEdPopup.LabelChange(Sender : TObject);
begin
  if not (csLoading in ComponentState) then
    PositionLabel;
end;

procedure TEsEdPopup.Notification(AComponent : TComponent; Operation: TOperation);
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

procedure TEsEdPopup.PopupClose;
begin
  FPopupActive := False;
end;

procedure TEsEdPopup.PopupOpen;
begin
  FPopupActive := True;
end;

procedure TEsEdPopup.PositionLabel;
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

procedure TEsEdPopup.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
var
  H : Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not HandleAllocated then
    Exit;

  if HandleAllocated then
    PostMessage(Handle, ES_POSITIONLABEL, 0, 0);

  if not FShowButton then begin
    FButton.Height := 0;
    FButton.Width := 0;
    Exit;
  end;

  H := ClientHeight;
  {$IFDEF Win32}
  if BorderStyle = bsNone then begin
    FButton.Height := H;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(Fbutton.Glyph) then
      if FButton.Width < FButton.Glyph.Width+4 then
        FButton.Width := FButton.Glyph.Width+4;
    FButton.Left := Width - FButton.Width;
    FButton.Top := 0;
  end else if Ctl3D then begin
    FButton.Height := H;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(FButton.Glyph) then
      if FButton.Width < FButton.Glyph.Width+4 then
        FButton.Width := FButton.Glyph.Width+4;
    FButton.Left := Width - FButton.Width - 4;
    FButton.Top := 0;
  end else begin
    FButton.Height := H - 2;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(Fbutton.Glyph) then
      if FButton.Width < FButton.Glyph.Width+4 then
        FButton.Width := FButton.Glyph.Width+4;
    FButton.Left := Width - FButton.Width - 1;
    FButton.Top := 1;
  end;
  {$ELSE} {block revised}                                              {!!.02}
  FButton.Height := H;
  FButton.Width := (FButton.Height div 4) * 3;
  if (FButton.Glyph <> nil) then
    if FButton.Width < FButton.Glyph.Width+6 then
      FButton.Width := FButton.Glyph.Width+6;
  FButton.Left := Width - FButton.Width;
  FButton.Top := 0;
  {$ENDIF}
end;

procedure TEsEdPopup.SetShowButton(Value : Boolean);
begin
  if Value <> FShowButton then begin
    FShowButton := Value;
    {force resize and redisplay of button}
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TEsEdPopup.SetVersion(const Value : string);
begin
end;

end.
