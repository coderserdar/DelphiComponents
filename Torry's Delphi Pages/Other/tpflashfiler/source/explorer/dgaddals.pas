{*********************************************************}
{* Dialog to define an alias                             *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit dgaddals;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  FileCtrl,
  Buttons,
  ExtCtrls,
  ffllbase,
  ffllunc,
  uconsts,
  uentity,
  ubase;

type
  TdlgAddAlias = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    cboDrives: TDriveComboBox;
    Label3: TLabel;
    Label4: TLabel;
    lstFolders: TDirectoryListBox;
    edtAlias: TEdit;
    edtPath: TEdit;
    cbCheckSpace: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure lstFoldersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDatabase : TffeDatabaseItem;
    FServer : TffeServerItem;
  public
  end;

function ShowAddAliasDlg(aServer: TffeServerItem;
                         var aDatabase : TffeDatabaseItem): TModalResult;

var
  dlgAddAlias: TdlgAddAlias;

implementation

{$R *.DFM}

function ShowAddAliasDlg(aServer : TffeServerItem;
                         var aDatabase: TffeDatabaseItem): TModalResult;
begin
  with TdlgAddAlias.Create(nil) do
  try
    edtAlias.MaxLength := ffcl_GeneralNameSize;      {!!.10}
    FServer := aServer;
    Result := ShowModal;
    aDatabase := FDatabase;
  finally
    Free;
  end;
end;

procedure TdlgAddAlias.FormCreate(Sender: TObject);
begin
  FDatabase := nil;
  HelpContext := hcAddDatabaseDlg;
end;

procedure TdlgAddAlias.btnOKClick(Sender: TObject);
var
  ExistingAliases: TStringList;
  UNCFilename: TffFullFilename;
begin
  if edtAlias.Text = '' then
    raise Exception.Create('You must enter an alias name.');

  if edtPath.Text = '' then
    raise Exception.Create('You must enter a path.');

  { Check if directory is on local machine }
  UNCFilename := FFExpandUNCFileName(edtPath.Text);
  if Copy(UNCFilename, 2, 1) = ':' then
    if MessageDlg('This path is local to this workstation.  ' +
                  'Are you sure you want to locate a database here?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Abort;

  { If directory is not valid, then ask "do you want to create?" }
  if not DirectoryExists(edtPath.Text) then
    if MessageDlg('Directory ' + edtPath.Text + ' does not exist, ' +
                  'do you want to create this directory?', mtConfirmation,
                  [mbYes, mbNo], 0) = mrYes then
      ForceDirectories(edtPath.Text)
    else
      Exit;

  { Go get all the aliases for this server.  Need a fresh list in case any
    were added by other users recently }
  ExistingAliases := TStringList.Create;
  try
    FServer.GetAliases(ExistingAliases);
    if ExistingAliases.IndexOf(edtAlias.Text) <> -1 then
      raise Exception.CreateFmt('The alias "%s" is already defined for this server.', [edtAlias.Text]);
  finally
    ExistingAliases.Free;
  end;

  { Physically add the alias to the server }
  FServer.AddAlias(edtAlias.Text, edtPath.Text, cbCheckSpace.Checked); {!!.11}

  { Now add an entry to our internal list o' databases }
  FDatabase := FServer.AddDatabase(edtAlias.Text);

  ModalResult := mrOK;
end;

procedure TdlgAddAlias.lstFoldersChange(Sender: TObject);
begin
  edtPath.Text := lstFolders.Directory;
end;

end.
