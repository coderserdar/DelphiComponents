{*********************************************************}
{* Main dialog unit                                      *}
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

Unit ufscomms;

Interface

{$I FsDEFINE.INC}

Uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  FsCLCfg,
  FsConst,
  FsLLBase,
  FsLLProt,
  FsCLBase,
  Mask,
  Windows,
  fsllwsck, {!!.11}
  Registry; {!!.06}

Type
  TfrmFFCommsMain = Class(TForm)
    cboProtocol: TComboBox;
    lblTransport: TLabel;
    lblServerName: TLabel;
    efServerName: TEdit;
    lblTitle: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    efServerAddress: TMaskEdit;
    lblServerAddress: TLabel;
    chkAsHostName: TCheckBox;
    Procedure FormCreate(Sender: TObject);
    Procedure btnCancelClick(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure cboProtocolClick(Sender: TObject);
    Procedure cboProtocolChange(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure chkAsHostNameClick(Sender: TObject);
  Private
    Procedure SetCtrlStates;
  Public
    Protocol: TfsCommsProtocolClass;
  End;

Var
  frmFFCommsMain: TfrmFFCommsMain;

Implementation

Uses
  FsUtil;

{$R *.DFM}

Function NormalizeIPAddress(Const Addr: String): String;
Var
  Idx: Integer;
  StartOctet: Boolean;
Begin
  StartOctet := True;
  For Idx := 1 To Length(Addr) Do
    If Addr[Idx] = '.' Then
      Begin
        If Length(Result) = 0 Then
          Result := Result + '0'
        Else If Result[Length(Result)] = '.' Then
          Result := Result + '0';
        Result := Result + Addr[Idx];
        StartOctet := True;
        Continue;
      End
    Else If Addr[Idx] = '0' Then
      Begin
        If StartOctet Then
          Continue
        Else
          Result := Result + Addr[Idx];
      End
    Else
      Begin
        StartOctet := False;
        Result := Result + Addr[Idx];
      End;
  If Result[Length(Result)] = '.' Then
    Result := Result + '0';
End;

Procedure TfrmFFCommsMain.FormCreate(Sender: TObject);
Var
  ProtocolName: TffShStr;
  ServerAddress: String;
  ServerName: String;
  Reg: TRegistry; {!!.06}
Begin

  { Load the protocol combo box dropdown list. }
  FFClientConfigGetProtocolNames(cboProtocol.Items);

  { Get the current protocol setting. }
  FFClientConfigReadProtocol(Protocol, ProtocolName);
  With cboProtocol Do
    ItemIndex := Items.IndexOf(ProtocolName);
  btnOK.Enabled := cboProtocol.ItemIndex <> -1;

  SetCtrlStates;

  { Get the current Server name & address. }
  FSSeparateAddress(FFClientConfigReadServerName,
    ServerName, ServerAddress);
  efServerName.Text := ServerName;

  Reg := TRegistry.Create;
  Try
    If Reg.OpenKey('Software\FSSQL\FSSQL\1.0\Comms', False) Then
      chkAsHostName.Checked := Reg.ReadBool('ServerAddressAsText');
  Finally
    Reg.Free;
  End;

  If chkAsHostName.Checked Then {begin !!.06}
    efServerAddress.EditMask := ''
  Else
    efServerAddress.EditMask := '999.999.999.999;1'; {end !!.06}

  efServerAddress.Text := ServerAddress;
End;

Procedure TfrmFFCommsMain.cboProtocolClick(Sender: TObject);
Begin
  btnOK.Enabled := cboProtocol.ItemIndex <> -1;
End;

Procedure TfrmFFCommsMain.btnOKClick(Sender: TObject);
Var {begin !!.01}
  Addr: String;
  Idx: Integer;
  Reg: TRegistry; {!!.06}
Begin
  Addr := efServerAddress.Text;

  {Strip spaces if tcp/ip}
  If (cboProtocol.Items[cboProtocol.ItemIndex] = fsc_TCPIP) Then
    For Idx := Length(Addr) Downto 1 Do
      If Addr[Idx] = ' ' Then
        Delete(Addr, Idx, 1); {!!.01}

  {Strip unnecessary 0's }
  If Not chkAsHostName.Checked Then
    Addr := NormalizeIPAddress(Addr);
  {end !!.01}
  FFClientConfigWriteProtocolName(cboProtocol.Items[cboProtocol.ItemIndex]);
  If (Addr = '...') Or (Addr = '  -  -  -  -  -  ') Then {!!.02}
    FFClientConfigWriteServerName('') {!!.02}
  Else {!!.02} If chkAsHostName.Checked Then {!!.11}
      FFClientConfigWriteServerName(efServerName.Text + '@' + Addr) {!!.02}
    Else If FsWSInstalled Then {!!.11}
      If WinsockRoutines.inet_addr(PChar(Addr)) <> INADDR_NONE Then {!!.11}
        FFClientConfigWriteServerName(efServerName.Text + '@' + Addr) {!!.02}
      Else
        Begin {!!.11}
          ModalResult := mrNone; {!!.11}
          Raise Exception.Create('Invalid IP address in Server Address'); {!!.11}
        End {!!.11}
    Else {!!.11}
      FFClientConfigWriteServerName(efServerName.Text + '@' + Addr); {!!.11}

  Reg := TRegistry.Create;
  Try
    If Reg.OpenKey('Software\FSSQL\FSSQL\1.0\Comms', True) Then
      Reg.WriteBool('ServerAddressAsText', chkAsHostName.Checked);
  Finally
    Reg.Free;
  End;

  Close;
  { to ensure that we can get the correct exit state
    when displaying form from FFE }
  ModalResult := mrOK; {!!.07}
End;

Procedure TfrmFFCommsMain.btnCancelClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmFFCommsMain.SetCtrlStates;
Var
  IsSingleUserOrNil: boolean;
Begin
  { Update UI based upon chosen protocol. }
  { Has user chosen SUP or has not chosen anything at all? }
  IsSingleUserOrNil :=
    (cboProtocol.ItemIndex = -1) Or
    (cboProtocol.Items[cboProtocol.ItemIndex] = fsc_SingleUser);

  efServerName.Enabled := (Not IsSingleUserOrNil);
  efServerAddress.Enabled := efServerName.Enabled;
  chkAsHostName.Enabled := cboProtocol.Items[cboProtocol.ItemIndex] = fsc_TCPIP;
  lblServerName.Enabled := efServerName.Enabled;
  lblServerAddress.Enabled := efServerName.Enabled;

  { Set server address edit mask. }
  If (cboProtocol.Items[cboProtocol.ItemIndex] = fsc_IPXSPX) Then {Start !!.01}
    efServerAddress.EditMask := 'AA-AA-AA-AA-AA-AA;1'
  Else If (cboProtocol.Items[cboProtocol.ItemIndex] = fsc_TCPIP) Then
    If chkAsHostName.Checked Then {begin !!.06}
      efServerAddress.EditMask := ''
    Else
      efServerAddress.EditMask := '999.999.999.999;1' {end !!.06}

      { We know that the transport is SingleUser, but we still want to
        display any old server address correctly.}
  Else If (efServerAddress.Text <> '') And
    Not (efServerAddress.Text[1] In ['0'..'9']) Then
    efServerAddress.EditMask := 'AA-AA-AA-AA-AA-AA;1'
  Else If chkAsHostName.Checked Then {begin !!.06}
    efServerAddress.EditMask := ''
  Else
    efServerAddress.EditMask := '999.999.999.999;1'; {end !!.06}
End;

Procedure TfrmFFCommsMain.cboProtocolChange(Sender: TObject);
Begin
  SetCtrlStates;
End;

Procedure TfrmFFCommsMain.Button1Click(Sender: TObject);
Begin
  efServerAddress.Enabled := Not efServerAddress.Enabled;
End;

Procedure TfrmFFCommsMain.chkAsHostNameClick(Sender: TObject); {begin !!.06}
Begin
  efServerAddress.Text := '';
  If chkAsHostName.Checked Then
    efServerAddress.EditMask := ''
  Else
    efServerAddress.EditMask := '999.999.999.999;1';
End; {end !!.06}

End.

