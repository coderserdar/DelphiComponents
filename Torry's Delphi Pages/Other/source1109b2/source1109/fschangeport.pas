Unit fschangeport;

{$I FsDEFINE.INC}

Interface

Uses
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Stdctrls,
  Buttons;
Type
  TFSChangePortClient = Class(TForm)
    btnDiscard: TBitBtn;
    btnSave: TBitBtn;
    grpTCPIP: TGroupBox;
    lblTCPPort: TLabel;
    lblUDPSr: TLabel;
    lblUDPCl: TLabel;
    edtTCPPort: TEdit;
    edtUDPServer: TEdit;
    edtUDPClient: TEdit;
    grpIPXSPX: TGroupBox;
    lblIPXSocket: TLabel;
    lblIPXClient: TLabel;
    lblSPX: TLabel;
    edtIPXServer: TEdit;
    edtIPXClient: TEdit;
    edtSPXSocket: TEdit;
    Procedure btnSaveClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    { Private declarations }
    TCPPort: Integer;
    UDPPortS: Integer;
    UDPPortC: Integer;
    IPXPortS: Integer;
    IPXPortC: Integer;
    SPXPort: Integer;
    Function ValidateValues: boolean;
    { Used to validate user input once the user clicks the OK button.
      If all the network config values are valid then this function
      returns True and fills in several private variables (listed above)
      with the appropriate information. }

  Public
  End;

Var
  FSChangePortClient: TFSChangePortClient;

Implementation

Uses
  FsLLProt;

{$R *.DFM}

{=====================================================================}

Procedure TFSChangePortClient.btnSaveClick(Sender: TObject);
Const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
Begin
  If ValidateValues Then
    Begin
      FsSetTCPPort(TCPPort);
      FsSetUDPPortServer(UDPPortS);
      FsSetUDPPortClient(UDPPortC);
      FsSetIPXSocketServer(IPXPortS);
      FsSetIPXSocketClient(IPXPortC);
      FsSetSPXSocket(SPXPort);
      ModalResult := MrOk;
    End;
End;

Function TFSChangePortClient.ValidateValues: boolean;
Const
  PortError = 'Port number should be a unique number between 1024 and 65535 inclusive';
Var
  aControl: TWinControl;
  ec: Integer;
Begin
  aControl := Nil;
  Result := True;

  Val(edtTCPPort.Text, TCPPort, ec);
  If (ec <> 0) Or (TCPPort < 1024) Or (TCPPort > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtTCPPort;
      ShowMessage(PortError);
    End;

  Val(edtUDPServer.Text, UDPPortS, ec);
  If (ec <> 0) Or (UDPPortS < 1024) Or (UDPPortS > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtUDPServer;
      ShowMessage(PortError);
    End;

  Val(edtUDPClient.Text, UDPPortC, ec);
  If (ec <> 0) Or (UDPPortC < 1024) Or (UDPPortC > 65535) Or
    (UDPPortS = UDPPortC) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtUDPClient;
      ShowMessage(PortError);
    End;

  Val(edtIPXServer.Text, IPXPortS, ec);
  If (ec <> 0) Or (IPXPortS < 1024) Or (IPXPortS > 65535) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtIPXServer;
      ShowMessage(PortError);
    End;

  Val(edtIPXClient.Text, IPXPortC, ec);
  If (ec <> 0) Or (IPXPortC < 1024) Or (IPXPortC > 65535) Or
    (IPXPortC = IPXPortS) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtIPXClient;
      ShowMessage(PortError);
    End;

  Val(edtSPXSocket.Text, SPXPort, ec);
  If (ec <> 0) Or (SPXPort < 1024) Or (SPXPort > 65535) Or
    (SPXPort = IPXPortS) Or (SPXPort = IPXPortC) Then
    Begin
      Result := False;
      If aControl = Nil Then
        aControl := edtTCPPort;
      ShowMessage(PortError);
    End;

  If assigned(aControl) Then
    ActiveControl := aControl;
End;

Procedure TFSChangePortClient.FormShow(Sender: TObject);
Begin
  TCPPort := FsGetTCPPort;
  UDPPortS := FsGetUDPPortServer;
  UDPPortC := FsGetUDPPortClient;
  IPXPortS := FsGetIPXSocketServer;
  IPXPortC := FsGetIPXSocketClient;
  SPXPort := FsGetSPXSocket;

  edtTCPPort.Text := IntToStr(TCPPort);
  edtUDPServer.Text := IntToStr(UDPPortS);
  edtUDPClient.Text := IntToStr(UDPPortC);
  edtIPXServer.Text := IntToStr(IPXPortS);
  edtIPXClient.Text := IntToStr(IPXPortC);
  edtSPXSocket.Text := IntToStr(SPXPort);
End;

End.

