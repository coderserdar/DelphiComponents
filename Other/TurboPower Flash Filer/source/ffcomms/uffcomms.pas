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

unit uFFComms;

interface

{$I FFDEFINE.INC}

uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  FFCLCfg,
  FFConst,
  FFLLBase,
  FFLLProt,
  FFCLBase,
  Mask,
  Windows,
  ffllwsck,                                                            {!!.11}
  Registry;                                                            {!!.06}

type
  TfrmFFCommsMain = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cboProtocolClick(Sender: TObject);
    procedure cboProtocolChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkAsHostNameClick(Sender: TObject);
  private
    procedure SetCtrlStates;
  public
    Protocol: TffCommsProtocolClass;
  end;

var
  frmFFCommsMain: TfrmFFCommsMain;

implementation

uses
  FFUtil;

{$R *.DFM}

function NormalizeIPAddress(const Addr : string) : string;
var
  Idx : Integer;
  StartOctet : Boolean;
begin
  StartOctet := True;
  for Idx := 1 to Length(Addr) do
    if Addr[Idx] = '.' then begin
      if Length(Result) = 0 then
        Result := Result + '0'
      else if Result[Length(Result)] = '.' then
        Result := Result + '0';
      Result := Result + Addr[Idx];
      StartOctet := True;
      Continue;
    end else if Addr[Idx] = '0' then begin
      if StartOctet then
        Continue
      else
        Result := Result + Addr[Idx];
    end else begin
      StartOctet := False;
      Result := Result + Addr[Idx];
    end;
  if Result[Length(Result)] = '.' then
    Result := Result + '0';
end;

procedure TfrmFFCommsMain.FormCreate(Sender: TObject);
var
  ProtocolName: TffShStr;
  ServerAddress : string;
  ServerName : string;
  Reg : TRegistry;                                                     {!!.06}
begin

  { Load the protocol combo box dropdown list. }
  FFClientConfigGetProtocolNames(cboProtocol.Items);

  { Get the current protocol setting. }
  FFClientConfigReadProtocol(Protocol, ProtocolName);
  with cboProtocol do
    ItemIndex := Items.IndexOf(ProtocolName);
  btnOK.Enabled := cboProtocol.ItemIndex <> -1;

  SetCtrlStates;

  { Get the current Server name & address. }
  FFSeparateAddress(FFClientConfigReadServerName,
                    ServerName, ServerAddress);
  efServerName.Text := ServerName;

  Reg := TRegistry.Create;
  try
    if Reg.OpenKey('Software\TurboPower\FlashFiler\2.0\FFComms', False) then
      chkAsHostName.Checked := Reg.ReadBool('ServerAddressAsText');
  finally
    Reg.Free;
  end;

  if chkAsHostName.Checked then                                        {begin !!.06}
    efServerAddress.EditMask := ''
  else
    efServerAddress.EditMask := '999.999.999.999;1';                   {end !!.06}

  efServerAddress.Text := ServerAddress;
end;

procedure TfrmFFCommsMain.cboProtocolClick(Sender: TObject);
begin
  btnOK.Enabled := cboProtocol.ItemIndex <> -1;
end;

procedure TfrmFFCommsMain.btnOKClick(Sender: TObject);
var                                                                    {begin !!.01}
  Addr : string;
  Idx  : Integer;
  Reg  : TRegistry;                                                    {!!.06}
begin
  Addr := efServerAddress.Text;

  {Strip spaces if tcp/ip}
  if (cboProtocol.Items[cboProtocol.ItemIndex] = ffc_TCPIP) then
    for Idx := Length(Addr) downto 1 do
      if Addr[Idx] = ' ' then
        Delete(Addr, Idx, 1); {!!.01}

  {Strip unnecessary 0's }
  if not chkAsHostName.Checked then
    Addr := NormalizeIPAddress(Addr);
                                                                       {end !!.01}
  FFClientConfigWriteProtocolName(cboProtocol.Items[cboProtocol.ItemIndex]);
  if (Addr = '...') or (Addr = '  -  -  -  -  -  ') then               {!!.02}
    FFClientConfigWriteServerName('')                                  {!!.02}
  else                                                                 {!!.02}
    if chkAsHostName.Checked then                                      {!!.11}
      FFClientConfigWriteServerName(efServerName.Text + '@' + Addr)    {!!.02}
    else if FFWSInstalled then                                         {!!.11}
      if WinsockRoutines.inet_addr(PChar(Addr)) <> INADDR_NONE then    {!!.11}
        FFClientConfigWriteServerName(efServerName.Text + '@' + Addr)  {!!.02}
      else begin                                                       {!!.11}
        ModalResult := mrNone;                                         {!!.11}
        raise Exception.Create('Invalid IP address in Server Address');{!!.11}
      end                                                              {!!.11}
    else                                                               {!!.11}
      FFClientConfigWriteServerName(efServerName.Text + '@' + Addr);   {!!.11}

  Reg := TRegistry.Create;
  try
    if Reg.OpenKey('Software\TurboPower\FlashFiler\2.0\FFComms', True) then
      Reg.WriteBool('ServerAddressAsText', chkAsHostName.Checked);
  finally
    Reg.Free;
  end;

  Close;
  { to ensure that we can get the correct exit state
    when displaying form from FFE }
  ModalResult := mrOK;                                                 {!!.07}
end;

procedure TfrmFFCommsMain.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFFCommsMain.SetCtrlStates;
var
  IsSingleUserOrNil : boolean;
begin
  { Update UI based upon chosen protocol. }
  { Has user chosen SUP or has not chosen anything at all? }
  IsSingleUserOrNil :=
    (cboProtocol.ItemIndex = -1) or
    (cboProtocol.Items[cboProtocol.ItemIndex] = ffc_SingleUser);

  efServerName.Enabled := (not IsSingleUserOrNil);
  efServerAddress.Enabled := efServerName.Enabled;
  chkAsHostName.Enabled := cboProtocol.Items[cboProtocol.ItemIndex] = ffc_TCPIP;
  lblServerName.Enabled := efServerName.Enabled;
  lblServerAddress.Enabled := efServerName.Enabled;

  { Set server address edit mask. }
  if (cboProtocol.Items[cboProtocol.ItemIndex] = ffc_IPXSPX) then      {Start !!.01}
    efServerAddress.EditMask := 'AA-AA-AA-AA-AA-AA;1'
  else if (cboProtocol.Items[cboProtocol.ItemIndex] = ffc_TCPIP) then
    if chkAsHostName.Checked then                                      {begin !!.06}
      efServerAddress.EditMask := ''
    else
      efServerAddress.EditMask := '999.999.999.999;1'                  {end !!.06}

  { We know that the transport is SingleUser, but we still want to
    display any old server address correctly.}
  else if (efServerAddress.Text <> '') and
          not (efServerAddress.Text[1] in ['0'..'9']) then
    efServerAddress.EditMask := 'AA-AA-AA-AA-AA-AA;1'
  else
    if chkAsHostName.Checked then                                      {begin !!.06}
      efServerAddress.EditMask := ''
    else
      efServerAddress.EditMask := '999.999.999.999;1';                 {end !!.06}
end;

procedure TfrmFFCommsMain.cboProtocolChange(Sender: TObject);
begin
  SetCtrlStates;
end;

procedure TfrmFFCommsMain.Button1Click(Sender: TObject);
begin
  efServerAddress.Enabled := not efServerAddress.Enabled;
end;

procedure TfrmFFCommsMain.chkAsHostNameClick(Sender: TObject);                {begin !!.06}
begin
  efServerAddress.Text := '';
  if chkAsHostName.Checked then
    efServerAddress.EditMask := ''
  else
    efServerAddress.EditMask := '999.999.999.999;1';
end;                                                                   {end !!.06}

end.
