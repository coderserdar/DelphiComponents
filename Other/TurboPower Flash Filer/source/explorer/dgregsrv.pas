{*********************************************************}
{* Dialog to register/unregister servers                 *}
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

unit dgregsrv;

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
  Buttons,
  ExtCtrls,
  ffllbase,
  ubase,
  uconsts;

type
  TdlgRegisteredServers = class(TForm)
    btnRemove: TBitBtn;
    btnCancel: TBitBtn;
    lstServers: TListBox;
    btnAdd: TBitBtn;
    lblRegServers: TLabel;
    cboServerName: TComboBox;
    lblNewServer: TLabel;
    btnOK: TBitBtn;
    Bevel1: TBevel;
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure cboServerNameChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstServersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetCtrlStates;
  protected                
  public
    procedure FillComboBox;
  end;

function ShowRegisteredServersDlg: TModalResult;

var
  dlgRegisteredServers: TdlgRegisteredServers;

implementation

{$R *.DFM}

uses
  ffllprot,                                                            {!!.07}
  uconfig;

function ShowRegisteredServersDlg: TModalResult;
begin
  with TdlgRegisteredServers.Create(nil) do
  try
    lstServers.Clear;
    lstServers.Items.AddStrings(Config.RegisteredServers);
    FillComboBox;
    Result := ShowModal;
  finally
    Free;
  end;
end;

procedure TdlgRegisteredServers.FormCreate(Sender: TObject);
begin
  HelpContext := hcRegisteredServersDlg;
end;

procedure TdlgRegisteredServers.FillComboBox;
var
  S: Integer;
begin

  { Fill combo box dropdown with all the available server names that are not
    already registered }
  cboServerName.Items.Clear;
  with ServerList do
    for S := 0 to Count - 1 do
      if (lstServers.Items.IndexOf(Items[S].ServerName) = -1) and
         (Items[S].ServerName<>ffc_SingleUserServerName) then              {!!.07}
        cboServerName.Items.Add(Items[S].ServerName);
end;

procedure TdlgRegisteredServers.cboServerNameChange(Sender: TObject);
begin
  btnAdd.Enabled := FFShStrTrim(cboServerName.Text) <> '';
end;

procedure TdlgRegisteredServers.btnAddClick(Sender: TObject);
begin
  lstServers.Items.Add(cboServerName.Text);
  cboServerName.ItemIndex := -1;
  cboServerName.Text := '';
  FillComboBox;
  btnAdd.Enabled := False;
end;

procedure TdlgRegisteredServers.btnRemoveClick(Sender: TObject);
var
  I: Integer;
begin
  with lstServers do begin
    I := 0;
    while I < Items.Count do begin
      if Selected[I] then
        Items.Delete(I)
      else
        Inc(I);
    end;
  end;
  FillComboBox;
  SetCtrlStates;
end;

procedure TdlgRegisteredServers.btnOKClick(Sender: TObject);
begin
  with Config.RegisteredServers do begin
    Clear;
    AddStrings(lstServers.Items);
  end;
  Config.Save;
end;

procedure TdlgRegisteredServers.lstServersClick(Sender: TObject);
begin
  SetCtrlStates;
end;

procedure TdlgRegisteredServers.SetCtrlStates;
begin
  btnRemove.Enabled := (lstServers.SelCount > 0);
end;

procedure TdlgRegisteredServers.FormShow(Sender: TObject);
begin
  SetCtrlStates;
end;

end.

