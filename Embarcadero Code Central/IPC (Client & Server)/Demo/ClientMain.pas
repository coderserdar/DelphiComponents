unit ClientMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IPC;

type
  TfrmClient = class(TForm)
    btnSend: TButton;
    IPCClient: TIPCClient;
    edtUserName: TEdit;
    edtPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtSessionName: TEdit;
    btnOpen: TButton;
    btnClose: TButton;
    lblStatus: TLabel;
    GroupBox1: TGroupBox;
    edtServerUserName: TEdit;
    edtServerPassword: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtSomeNumber: TEdit;
    Label5: TLabel;
    edtSomeReal: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    procedure btnSendClick(Sender: TObject);
    procedure IPCClientConnect(AHwnd: HWND);
    procedure IPCClientDisconnect(AHwnd: HWND);
    procedure IPCClientData(MsgPointer: Pointer);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
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
  frmClient: TfrmClient;

implementation

{$R *.DFM}

procedure TfrmClient.btnSendClick(Sender: TObject);
var
  MsgData: TIPCData;
begin
  if IPCClient.Active then
  begin
    MsgData.UserName := edtUserName.Text;
    MsgData.Password := edtPassword.Text;
    MsgData.SomeNumber := 1234;
    MsgData.SomeReal := 45.321;
    MsgData.SomeChar := 'X';
    IPCClient.SendMsg(@MsgData, Sizeof(MsgData));
  end;
end;

procedure TfrmClient.IPCClientConnect(AHwnd: HWND);
begin
  lblStatus.Caption := 'Connected';
end;

procedure TfrmClient.IPCClientDisconnect(AHwnd: HWND);
begin
  lblStatus.Caption := 'Disconnected';
end;

procedure TfrmClient.IPCClientData(MsgPointer: Pointer);
var
  AMsg: TIPCData;
begin
  AMsg := TIPCData(MsgPointer^);
  edtServerUserName.Text := AMsg.UserName;
  edtServerPassword.Text := AMsg.Password;
  edtSomeNumber.Text := IntToStr(AMsg.SomeNumber);
  edtSomeReal.Text := FloatToStr(AMsg.SomeReal);
end;

procedure TfrmClient.btnOpenClick(Sender: TObject);
begin
  if not IPCClient.Active then
    IPCClient.SessionName := edtSessionName.Text;
  IPCClient.Open;
end;

procedure TfrmClient.btnCloseClick(Sender: TObject);
begin
  IPCClient.Close;
end;

end.
