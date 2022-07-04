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

unit Uffsnet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FFSrCfg, FFSrEng, FFLLWsck;

type
  TFFNetConfigForm = class(TForm)
    grpTCPIP: TGroupBox;
    Label2: TLabel;
    lblTCPPort: TLabel;
    lblUDPSr: TLabel;
    lblUDPCl: TLabel;
    chkTCPEnabled: TCheckBox;
    chkTCPListen: TCheckBox;
    cmbTCPIntf: TComboBox;
    edtTCPPort: TEdit;
    edtUDPServer: TEdit;
    edtUDPClient: TEdit;
    grpIPXSPX: TGroupBox;
    lblIPXSocket: TLabel;
    lblIPXClient: TLabel;
    lblSPX: TLabel;
    chkIPXEnabled: TCheckBox;
    chkIPXListen: TCheckBox;
    edtIPXServer: TEdit;
    edtIPXClient: TEdit;
    edtSPXSocket: TEdit;
    grpSUP: TGroupBox;
    chkSUPEnabled: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }

    OurGenInfo : TffGeneralInfo;

    FEngine : TffServerEngine;

    { The following variables are used to hold the values retrieved via
      ValidateValues. }
    TCPPort : integer;
    UDPPortS: integer;
    UDPPortC: integer;
    IPXPortS: integer;
    IPXPortC: integer;
    SPXPort : integer;

    procedure InitCtrlStates;

    procedure RefreshBindings;

    procedure SetEngine(anEngine : TffServerEngine);

    function ValidateValues : boolean;
      { Used to validate user input once the user clicks the OK button.
        If all the network config values are valid then this function
        returns True and fills in several private variables (listed above)
        with the appropriate information. }

  public
    { Public declarations }

    property ServerEngine : TffServerEngine read FEngine write SetEngine;
      { The server engine being configured by this dialog. }
  end;

  procedure EnableGroupBox(aBox : TGroupBox; Value: Boolean);

implementation

uses
  FFLLBase,
  FFLLComp,
  FFLLEng,
  FFLLExcp,
  FFLLProt,
  FFSrBDE,
  UffEgMgr;

{$R *.DFM}

{====================================================================}
procedure TFFNetConfigForm.FormCreate(Sender: TObject);
begin
  FEngine := nil;
end;
{--------}
procedure TFFNetConfigForm.FormShow(Sender: TObject);
begin
  if not assigned(FEngine) then
    Close;
  InitCtrlStates;
  RefreshBindings;
end;
{--------}
procedure TFFNetConfigForm.InitCtrlStates;
var
  ServerUp : boolean;
begin

  ServerUp := (FEngine.State = ffesStarted);

  EnableGroupBox(grpTCPIP, FFEngineManager.TCPIPTransport.Supported);
  EnableGroupBox(grpIPXSPX, FFEngineManager.IPXSPXTransport.Supported);

  cmbTCPIntf.Enabled := (not ServerUp);
  edtTCPPort.Enabled := (not ServerUp);
  edtUDPServer.Enabled := (not ServerUp);
  edtUDPClient.Enabled := (not ServerUp);
  edtIPXServer.Enabled := (not ServerUp);
  edtIPXClient.Enabled := (not ServerUp);
  edtSPXSocket.Enabled := (not ServerUp);

end;
{--------}
procedure TffNetConfigForm.RefreshBindings;
var
  Idx : Integer;
begin
  FFWSGetLocalHosts(cmbTCPIntf.Items);
  Idx := OurGenInfo.giTCPInterface + 1;
  if Idx > Pred(cmbTCPIntf.Items.Count) then begin
    MessageDlg('The bound interface is no longer available. '+#13+#10 +
               'Bindings will be reset to all adapters.',
               mtInformation, [mbOK], 0);
    cmbTCPIntf.ItemIndex := 0;
  end else
    cmbTCPIntf.ItemIndex := Idx;

end;
function TffNetConfigForm.ValidateValues : boolean;
const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
var
  aControl : TWinControl;
  ec : integer;
begin
  aControl := nil;
  Result := True;

  Val(edtTCPPort.Text, TCPPort, ec);
  if (ec <> 0) or (TCPPort < 1024) or (TCPPort > 65535) then begin
    Result := False;
    if aControl = nil then
      aControl := edtTCPPort;
    ShowMessage(PortError);
  end;

  Val(edtUDPServer.Text, UDPPortS, ec);
  if (ec <> 0) or (UDPPortS < 1024) or (UDPPortS > 65535) then begin
    Result := False;
    if aControl = nil then
      aControl := edtUDPServer;
    ShowMessage(PortError);
  end;

  Val(edtUDPClient.Text, UDPPortC, ec);
  if (ec <> 0) or (UDPPortC < 1024) or (UDPPortC > 65535) or
     (UDPPortS = UDPPortC) then begin
    Result := False;
    if aControl = nil then
      aControl := edtUDPClient;
    ShowMessage(PortError);
  end;

  Val(edtIPXServer.Text, IPXPortS, ec);
  if (ec <> 0) or (IPXPortS < 1024) or (IPXPortS > 65535) then begin
    Result := False;
    if aControl = nil then
      aControl := edtIPXServer;
    ShowMessage(PortError);
  end;

  Val(edtIPXClient.Text, IPXPortC, ec);
  if (ec <> 0) or (IPXPortC < 1024) or (IPXPortC > 65535) or
     (IPXPortC = IPXPortS) then begin
    Result := False;
    if aControl = nil then
      aControl := edtIPXClient;
    ShowMessage(PortError);
  end;

  Val(edtSPXSocket.Text, SPXPort, ec);
  if (ec <> 0) or (SPXPort < 1024) or (SPXPort > 65535) or
     (SPXPort = IPXPortS) or (SPXPort = IPXPortC) then begin
    Result := False;
    if aControl = nil then
      aControl := edtTCPPort;
    ShowMessage(PortError);
  end;

  if assigned(aControl) then
    ActiveControl := aControl;
end;
{--------}
procedure TFFNetConfigForm.btnOKClick(Sender: TObject);
var
  errStr : array [0..127] of char;
  aResult : TffResult;
begin
   if ValidateValues then begin
    with OurGenInfo do begin
      giTCPPort := TCPPort;
      giUDPPortSr := UDPPortS;
      giUDPPortCl := UDPPortC;
      giIPXSocketSr := IPXPortS;
      giIPXSocketCl := IPXPortC;
      giSPXSocket := SPXPort;
      FFSetTCPPort(TCPPort);
      FFSetUDPPortServer(UDPPortS);
      FFSetUDPPortClient(UDPPortC);
      FFSetIPXSocketServer(IPXPortS);
      FFSetIPXSocketClient(IPXPortC);
      FFSetSPXSocket(SPXPort);
      giSingleUser := chkSUPEnabled.Checked;
      giIPXSPX := chkIPXEnabled.Checked;
      giIPXSPXLFB := chkIPXListen.Checked;
      giTCPIP := chkTCPEnabled.Checked;
      giTCPIPLFB := chkTCPListen.Checked;
      giTCPInterface := cmbTCPIntf.ItemIndex - 1;
    end;
    FEngine.Configuration.GeneralInfo^ := OurGenInfo;
    aResult := FEngine.WriteGeneralInfo(False);
    if aResult <> DBIERR_NONE then begin
      ffStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
      showMessage(format('Could not save configuration: %s [$%x/%d])',
                         [strPas(errStr), aResult, aResult]));
      self.modalResult := mrNone;
    end
    else
      ModalResult := mrOK;
  end else
    ModalResult := mrNone;
end;
{--------}
procedure TFFNetConfigForm.SetEngine(anEngine : TffServerEngine);
begin
  FEngine := anEngine;
  if assigned(FEngine) then begin
    OurGenInfo := FEngine.Configuration.GeneralInfo^;
    with OurGenInfo do begin
      chkSUPEnabled.Checked := giSingleUser;
      chkIPXEnabled.Checked := giIPXSPX;
      chkIPXListen.Checked := giIPXSPXLFB;
      chkTCPEnabled.Checked := giTCPIP;
      chkTCPListen.Checked := giTCPIPLFB;
      edtTCPPort.Text := IntToStr(giTCPPort);
      edtUDPServer.Text := IntToStr(giUDPPortSr);
      edtUDPClient.Text := IntToStr(giUDPPortCl);
      edtIPXServer.Text := IntToStr(giIPXSocketSr);
      edtIPXClient.Text := IntToStr(giIPXSocketCl);
      edtSPXSocket.Text := IntToStr(giSPXSocket);
    end;
  end;
end;
{====================================================================}

{===Utility routines=================================================}
procedure EnableGroupBox(aBox : TGroupBox; Value: Boolean);
var
  anIndex : integer;
begin

  aBox.Enabled := Value;
  if not Value then
    aBox.Font.Color := clGrayText;

  { Disable the child controls. }
  for anIndex := 0 to pred(aBox.ControlCount) do begin
    aBox.Controls[anIndex].Enabled := Value;
  end;

end;

{====================================================================}

end.
