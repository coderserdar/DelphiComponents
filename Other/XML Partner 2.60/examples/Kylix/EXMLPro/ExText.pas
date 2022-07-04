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
{* XMLPartner: ExText.PAS 2.57                           *}
{*********************************************************}
{* XMLPartner: XML Editor Text Edit Window               *}
{*********************************************************}
unit ExText;

interface

uses
{$IFDEF WIN32}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QStdCtrls,
  QControls,
  QExtCtrls,
  QForms,
{$ENDIF}
  SysUtils,
  Classes,
  INIFiles;

type
  TTextForm = class(TForm)
    Label1: TLabel;
    TextEdit: TMemo;
    Panel1: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FINIFile : TINIFile;

  public
    { Public declarations }
    property INIFile : TINIFile read FINIFile write FINIFile;
  end;

var
  TextForm: TTextForm;

implementation

uses
  ExUtil;

{$R *.dfm}

const
  csSection = 'TextEditor';

procedure TTextForm.FormShow(Sender: TObject);
begin
  Width := 400;
  Height := 300;
{$IFDEF WIN32}
  Left := (GetSystemMetrics(SM_CXSCREEN) - Width) shr 1;
  Top := (GetSystemMetrics(SM_CYSCREEN) - Height) shr 1;
{$ENDIF}
  if assigned(FINIFile) then
    with FINIFile do
      if ReadBool(csSection, 'SaveTextWinPos', False) then begin
        RestoreFormState(Self, FINIFile, csSection);
        if Width < 150 then
          Width := 150;
        if Height < 150 then
          Height := 150;
      end;
  TextEdit.SetFocus;
end;

procedure TTextForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(FINIFile) then
    SaveFormState(Self, FINIFile, csSection);
end;

end.
