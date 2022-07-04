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
 * The Original Code is TurboPower XMLPartner
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

{*********************************************************}
{* XMLPartner: ExErr.PAS 2.57                            *}
{*********************************************************}
{* XMLPartner: XML Editor Document Error Window          *}
{*********************************************************}
unit ExErr;

interface

uses
{$IFDEF WIN32}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls,
  QStdCtrls,
  QExtCtrls,
  QForms,
{$ENDIF}
  SysUtils,
  Classes,
  inifiles;

type
  TfrmErrors = class(TForm)
    pnlMsg: TPanel;
    pnlBottom: TPanel;
    pbOK: TButton;
    memErrors: TMemo;
    procedure pbOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FErrors : TStringList;
      { Calling form is responsible for freeing the object referenced
        by FErrors. }
    FINIFile : TINIFile;

    procedure SetIntroMsg(const aMsg : string);

  public
    { Public declarations }
    property Errors : TStringList read FErrors write FErrors;
    property IntroMsg : string write SetIntroMsg;
    property INIFile : TINIFile read FINIFile write FINIFile;
  end;

var
  frmErrors: TfrmErrors;

implementation

uses
  ExUtil;

{$R *.dfm}

const
  csSection = 'ErrorWindow';

procedure TfrmErrors.pbOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmErrors.FormResize(Sender: TObject);
begin
  pbOK.Left := (pnlBottom.ClientWidth div 2) - (pbOK.Width div 2);
end;

procedure TfrmErrors.FormShow(Sender: TObject);
begin
  if assigned(FINIFile) then
    RestoreFormState(Self, FINIFile, csSection);
  if assigned(FErrors) then
    memErrors.Lines.Assign(FErrors);
end;

procedure TfrmErrors.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(FINIFile) then
    SaveFormState(Self, FINIFile, csSection);
end;

procedure TfrmErrors.SetIntroMsg(const aMsg : string);
begin
  pnlMsg.Caption := Format('  The following errors were encountered while %s:',
                           [aMsg]);
end;

end.
