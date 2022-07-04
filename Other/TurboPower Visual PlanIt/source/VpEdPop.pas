{*********************************************************}
{*                   VPEDPOP.PAS 1.03                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpEdPop;
  {-base popup edit field class}

interface

uses
  Windows, Buttons, Classes, Controls, ExtCtrls, Forms, Graphics, Menus,
  Messages, StdCtrls, SysUtils, VpBase, VpConst;

type
  TVpEdButton = class(TBitBtn)
  public
     procedure Click;
       override;
  end;

  TVpEdPopup = class(TCustomEdit)
  protected {private}
    {property variables}
    FButton      : TVpEdButton;
    FPopupActive : Boolean;
    FShowButton  : Boolean;

    function GetVersion : string;
    procedure SetShowButton(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    function GetButtonWidth : Integer;

  protected
    procedure CreateParams(var Params : TCreateParams);  override;
    procedure CreateWnd; override;
    function GetButtonEnabled : Boolean; dynamic;
    procedure PopupClose(Sender : TObject); dynamic;
    property ShowButton : Boolean
      read FShowButton write SetShowButton default True;
    property Version : string read GetVersion write SetVersion stored False;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
      override;
    procedure PopupOpen; dynamic;
    property PopupActive : Boolean read FPopupActive;
  end;

implementation

{*** TVpEditBtn ***}

procedure TVpEdButton.Click;
begin
  TVpEdPopup(Parent).PopupOpen;
end;


{*** TVpEdPopup ***}

constructor TVpEdPopup.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption];

  FShowButton := True;
  FButton := TVpEdButton.Create(Self);
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.Caption := '';
  FButton.TabStop := False;
  FButton.Style := bsNew;
end;

procedure TVpEdPopup.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

procedure TVpEdPopup.CreateWnd;
begin
  inherited CreateWnd;

  {force button placement}
  SetBounds(Left, Top, Width, Height);

  FButton.Enabled := GetButtonEnabled;
end;

destructor TVpEdPopup.Destroy;
begin
  FButton.Free;
  FButton := nil;

  inherited Destroy;
end;

function TVpEdPopup.GetButtonEnabled : Boolean;
begin
  Result := not ReadOnly;
end;

function TVpEdPopup.GetButtonWidth : Integer;
begin
  if Assigned(FButton) and FShowButton then
    Result := FButton.Width
  else
    Result := 0;
end;

function TVpEdPopup.GetVersion : string;
begin
  Result := VpVersionStr;
end;

procedure TVpEdPopup.PopupClose;
begin
  FPopupActive := False;
end;

procedure TVpEdPopup.PopupOpen;
begin
  FPopupActive := True;
end;

procedure TVpEdPopup.SetBounds(ALeft, ATop, AWidth, AHeight : Integer);
var
  H : Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not HandleAllocated then
    Exit;

  if not FShowButton then begin
    FButton.Height := 0;
    FButton.Width := 0;
    Exit;
  end;

  H := ClientHeight;
  if BorderStyle = bsNone then begin
    FButton.Height := H;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(Fbutton.Glyph) then
      if FButton.Width < FButton.Glyph.Width + 6 then
        FButton.Width := FButton.Glyph.Width + 6;
    FButton.Left := Width - FButton.Width;
    FButton.Top := 0;
  end else if Ctl3D then begin
    FButton.Height := H;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(FButton.Glyph) then
      if FButton.Width < FButton.Glyph.Width + 6 then
        FButton.Width := FButton.Glyph.Width + 6;
    FButton.Left := Width - FButton.Width - 4;
    FButton.Top := 0;
  end else begin
    FButton.Height := H - 2;
    FButton.Width := (FButton.Height div 4) * 3;
    if Assigned(Fbutton.Glyph) then
      if FButton.Width < FButton.Glyph.Width + 6 then
        FButton.Width := FButton.Glyph.Width + 6;
    FButton.Left := Width - FButton.Width - 1;
    FButton.Top := 1;
  end;
end;

procedure TVpEdPopup.SetShowButton(Value : Boolean);
begin
  if Value <> FShowButton then begin
    FShowButton := Value;
    {force resize and redisplay of button}
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TVpEdPopup.SetVersion(const Value : string);
begin
  // Leave empty
end;
{=====}

end.
