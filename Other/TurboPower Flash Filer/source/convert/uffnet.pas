{*********************************************************}
{* FlashFiler: Form used to set for FF1 to FF2           *}
{* conversion program.                                   *}
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

unit uFFNet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmFFTransport = class(TForm)
    gbSingle: TGroupBox;
    gbIPXSPX: TGroupBox;
    gbTCPIP: TGroupBox;
    cbxSUEnabled: TCheckBox;
    cbxIPXEnabled: TCheckBox;
    cbxIPXListen: TCheckBox;
    cbxTCPEnabled: TCheckBox;
    cbxTCPListen: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblTCPNic: TLabel;
    cbTCPIntf: TComboBox;
    lblTCPPort: TLabel;
    lblUDPSr: TLabel;
    lblUDPCl: TLabel;
    edtTCPPort: TEdit;
    edtUDPServer: TEdit;
    edtUDPClient: TEdit;
    lblIPXSocket: TLabel;
    lblIPXClient: TLabel;
    lblSPX: TLabel;
    edtIPXSr: TEdit;
    edtIPXCl: TEdit;
    edtSPX: TEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TCPIntfcNum : longint;
  end;

var
  frmFFTransport : TfrmFFTransport;

implementation

uses
  FFLLWsck;

{$R *.DFM}

procedure TfrmFFTransport.FormShow(Sender : TObject);
begin
  FFWSGetLocalHosts(cbTCPIntf.Items);
  if TCPIntfcNum > Pred(cbTCPIntf.Items.Count) then begin
    MessageDlg('The bound interface is no longer available. ' + #13#10 +
               'Bindings will be reset to all adapters.',
               mtInformation, [mbOK], 0);
    cbTCPIntf.ItemIndex := 0;
  end else
    cbTCPIntf.ItemIndex := TCPIntfcNum;
end;

end.
