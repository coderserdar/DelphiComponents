unit ServerMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IPC;

type
  TfrmServer = class(TForm)
    btnSend: TButton;
    edtUserName: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtSessionName: TEdit;
    btnOpen: TButton;
    btnClose: TButton;
    lblStatus: TLabel;
    IPCServer: TIPCServer;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtServerUserName: TEdit;
    edtServerPassword: TEdit;
    edtSomeNumber: TEdit;
    edtSomeReal: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    procedure btnSendClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure IPCServerConnect(AHwnd: HWND);
    procedure IPCServerDisconnect(AHwnd: HWND);
    procedure IPCServerClientData(MsgPointer: Pointer; AHwnd: HWND);
    procedure IPCServerAfterClose(Sender: TObject);
  private
    FClientHwnd: HWND;
  public
    { Public declarations }
  end;

type
  //You can create any type of record you want, but if you use "STRING" type
  //Make sure you use subtypes of ShortString ie str: string[15]
  PIPCData = ^TIPCData;
  TIPCData = record
    UserName: string[15]; //Make sure you use a subtypes of ShortString
    Password: string[15]; //because the size of the record is need.
    SomeNumber: Integer;
    SomeChar: Char;
    SomeReal: Real;
  end;

var
  frmServer: TfrmServer;

implementation

{$R *.DFM}

procedure TfrmServer.btnSendClick(Sender: TObject);
var
  MsgData: TIPCData;
begin
  if IPCServer.Active then
  begin
    MsgData.UserName := edtUserName.Text;
    MsgData.Password := edtPassword.Text;
    MsgData.SomeNumber := 987654;
    MsgData.SomeReal := 231.321;
    MsgData.SomeChar := 'A';
    IPCServer.SendMsg(@MsgData, FClientHwnd, Sizeof(MsgData));
  end;
end;

procedure TfrmServer.btnOpenClick(Sender: TObject);
begin
  if not IPCServer.Active then
    IPCServer.SessionName := edtSessionName.Text;
  IPCServer.Open;
end;

procedure TfrmServer.btnCloseClick(Sender: TObject);
begin
  IPCServer.Close;
end;

procedure TfrmServer.IPCServerConnect(AHwnd: HWND);
begin
  lblStatus.Caption := 'Connected';
end;

procedure TfrmServer.IPCServerDisconnect(AHwnd: HWND);
begin
  lblStatus.Caption := 'Disconnected';
end;

procedure TfrmServer.IPCServerClientData(MsgPointer: Pointer; AHwnd: HWND);
var
  AMsg: TIPCData;
begin
  FClientHwnd := AHwnd;
  AMsg := TIPCData(MsgPointer^);
  edtServerUserName.Text := AMsg.UserName;
  edtServerPassword.Text := AMsg.Password;
  edtSomeNumber.Text := IntToStr(AMsg.SomeNumber);
  edtSomeReal.Text := FloatToStr(AMsg.SomeReal);
end;

procedure TfrmServer.IPCServerAfterClose(Sender: TObject);
begin
  lblStatus.Caption := 'Disconnected';
  FClientHwnd := 0;

end;

end.
