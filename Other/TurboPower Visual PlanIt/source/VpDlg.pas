{*********************************************************}
{*                   VPDLG.PAS 1.03                      *}
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

unit VpDlg;
  {dialog components base classes}

interface

uses
  Classes, Forms, Graphics, VpBase, VpBaseDS, VpData, VpConst, VpSR;

type
  TVpDialogPosition = (mpCenter, mpCenterTop, mpCustom);
  TVpDialogOption = (doSizeable);
  TVpDialogOptions = set of TVpDialogOption;

  TVpDialogPlacement = class(TPersistent)
  protected {private}
    {property variables}
    FPosition : TVpDialogPosition;
    FHeight   : Integer;
    FLeft     : Integer;
    FTop      : Integer;
    FWidth    : Integer;
  published
    {properties}
    property Position : TVpDialogPosition read FPosition write FPosition;
    property Top : Integer read FTop write FTop;
    property Left : Integer read FLeft write FLeft;
    property Height : Integer read FHeight write FHeight;
    property Width : Integer read FWidth write FWidth;
  end;

  TVpBaseDialog = class(TVpComponent)
  protected {private}
    {property variables}
    FDataStore   : TVpCustomDataStore;
    FOptions     : TVpDialogOptions;
    FPlacement   : TVpDialogPlacement;
    FOnHelpClick : TNotifyEvent;
    FControlLink : TVpControlLink;

    function GetVersion: String;
    procedure SetVersion(const Value: string);
    procedure SetControlLink (const v : TVpControlLink);
    procedure SetDataStore(Value: TVpCustomDataStore);
    procedure DoFormPlacement(Form : TForm);
    procedure SetFormCaption(Form : TForm; const Title, SubTitle : string);
    property Options : TVpDialogOptions read FOptions write FOptions;
    property Placement : TVpDialogPlacement read FPlacement write FPlacement;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Execute : Boolean; virtual;
    property ControlLink : TVpControlLink
             read FControlLink write SetControlLink;
  published
    property DataStore: TVpCustomDataStore read FDataStore write SetDataStore;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

constructor TVpBaseDialog.Create(AOwner : TComponent);
var
  I: integer;
begin
  inherited Create(AOwner);
  FOptions            := [];
  FPlacement          := TVpDialogPlacement.Create;
  FPlacement.Position := mpCenter;
  FPlacement.Left     := 10;
  FPlacement.Height   := 250;
  FPlacement.Top      := 10;
  FPlacement.Width    := 400;

  { connect to the first DataStore found on the parent form. }
  I := 0;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    while (I < Owner.ComponentCount) and (FDataStore = nil) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then
        FDataStore := TVpCustomDataStore(Owner.Components[I])
      else if (Owner.Components[I] is TVpControlLink) then
        FControlLink := TVpControlLink (Owner.Components[I]);
      Inc(I);
    end;
end;

destructor TVpBaseDialog.Destroy;
begin
  FDataStore := nil;
  FControlLink := nil;

  FPlacement.Free;
  FPlacement := nil;

  inherited Destroy;
end;
{=====}

function TVpBaseDialog.Execute : Boolean;
begin
  { Do nothing. }
  Result := False;
end;
{=====}

function TVpBaseDialog.GetVersion: string;
begin
  Result := VpVersionStr;
end;
{=====}

procedure TVpBaseDialog.SetFormCaption(Form : TForm; const Title, SubTitle : string);
begin
  if Title = '' then
    Form.Caption := RSUntitled + ' - ' + SubTitle
  else
    Form.Caption := Title + ' - ' + SubTitle;
end;
{=====}

procedure TVpBaseDialog.SetVersion(const Value: string);
begin
// This method left intentionally blank.
end;
{=====}

procedure TVpBaseDialog.SetControlLink (const v : TVpControlLink);
begin
  if FControlLink <> v then
    FControlLink := v;
end;

procedure TVpBaseDialog.SetDataStore(Value: TVpCustomDataStore);
begin
  if FDataStore <> Value then begin
    FDataStore := Value;
  end;
end;
{=====}

procedure TVpBaseDialog.DoFormPlacement(Form : TForm);
begin
  {set proper style for displayed form}
  if doSizeable in FOptions then
    Form.BorderStyle := bsSizeable
  else
    Form.BorderStyle:= bsDialog;

  if (Screen.ActiveForm <> nil)
  and(Screen.ActiveForm.FormStyle = fsStayOnTop) then
    Form.FormStyle := fsStayOnTop;

  Form.Height := FPlacement.Height;
  Form.Width  := FPlacement.Width;

  {set position}
  case FPlacement.Position of
    mpCenter : begin
      Form.Position := poScreenCenter;
    end;
    mpCenterTop : begin
      Form.Top := (Screen.Height - Form.Height) div 3;
      Form.Left := (Screen.Width - Form.Width) div 2;
    end;
    mpCustom : begin
      Form.Top    := FPlacement.Top;
      Form.Left   := FPlacement.Left;
    end;
  end;
end;


end.
