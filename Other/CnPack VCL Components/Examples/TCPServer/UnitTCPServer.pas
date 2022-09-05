unit UnitTCPServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WinSock, CnThreadingTCPServer;

type
  TFormTCPServer = class(TForm)
    lblIP: TLabel;
    edtIP: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    btnOpen: TButton;
    mmoResult: TMemo;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTCP: TCnThreadingTCPServer;
    procedure Log(const Msg: string);
  public
    procedure TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TCPError(Sender: TObject; SocketError: Integer);
  end;

var
  FormTCPServer: TFormTCPServer;

implementation

{$R *.DFM}

procedure TFormTCPServer.btnOpenClick(Sender: TObject);
begin
  if FTCP.Active then
  begin
    FTCP.Active := False;
    btnOpen.Caption := 'Open';
  end
  else
  begin
    FTCP.LocalIP := edtIP.Text;
    FTCP.LocalPort := StrToInt(edtPort.Text);

    FTCP.Active := True;
    if FTCP.Listening then
    begin
      btnOpen.Caption := 'Close';
      Log('Listening at Port: ' + IntToStr(FTCP.ActualLocalPort));
    end;
  end;
end;

procedure TFormTCPServer.FormCreate(Sender: TObject);
begin
  FTCP := TCnThreadingTCPServer.Create(Self);
  FTCP.OnAccept := TCPAccept;
  FTCP.OnError := TCPError;
end;

procedure TFormTCPServer.Log(const Msg: string);
begin
  mmoResult.Lines.Add(Msg);
end;

procedure TFormTCPServer.TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
var
  C: Integer;
  RecvBuf: array[0..1023] of Byte;
  SendBuf: array[0..1023] of Byte;
begin
  Log('Connected: ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
  // ��ʾ������ա���
  C := ClientSocket.Recv(RecvBuf, SizeOf(RecvBuf) - 1);
  if C = SOCKET_ERROR then
    Exit;

  Log('Get ' + IntToStr(C) + ' Bytes.');
  SendBuf[0] := Ord('A');
  SendBuf[1] := Ord('B');
  C := ClientSocket.Send(SendBuf, 2);
  if C = SOCKET_ERROR then
    Exit;

  Log('Send ' + IntToStr(C) + ' Bytes.');

  //�˳��¼���������Ͽ���
end;

procedure TFormTCPServer.TCPError(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

end.
