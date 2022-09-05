{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTCPForwarder;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ����� TCP �˿�ת��ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ Liu Xiao
* ��    ע��һ��ʹ�� ThreadingTCPServer �Ķ��̶߳˿�ת����������̳߳ػ���
* ����ƽ̨��PWin7 + Delphi 5
* ���ݲ��ԣ�PWin7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2020.02.25 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Contnrs, WinSock, CnConsts, CnNetConsts, CnClasses,
  CnThreadingTCPServer, CnTCPClient;

type
  TCnTCPForwarder = class(TCnThreadingTCPServer)
  {* TCP �˿�ת���������ÿ���ͻ��������������߳�}
  private
    FRemoteHost: string;
    FRemotePort: Word;
    FOnRemoteConnected: TNotifyEvent;
    procedure SetRemoteHost(const Value: string);
    procedure SetRemotePort(const Value: Word);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function DoGetClientThread: TCnTCPClientThread; override;
    {* ��������ʹ�� TCnTCPForwardThread}

    procedure DoRemoteConnected; virtual;
  published
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    {* ת����Զ������}
    property RemotePort: Word read FRemotePort write SetRemotePort;
    {* ת����Զ�̶˿�}

    property OnRemoteConnected: TNotifyEvent read FOnRemoteConnected write FOnRemoteConnected;
    {* ������Զ�̷�����ʱ����}
  end;

implementation

const
  FORWARDER_BUF_SIZE = 32 * 1024;

type
  TCnForwarderClientSocket = class(TCnClientSocket)
  {* ��װ�Ĵ���һ�ͻ�������ת���Ķ��󣬰���˫��ͨѶ����һ�� Socket}
  private
    FRemoteSocket: TSocket;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Shutdown; override;
    {* �ر�ǰ��������� Socket}

    // send/recv �շ����ݷ�װ
    function RemoteSend(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function RemoteRecv(var Buf; Len: Integer; Flags: Integer = 0): Integer;

    property RemoteSocket: TSocket read FRemoteSocket write FRemoteSocket;
    {* ����Զ�̷������� Socket}
  end;

  TCnTCPForwardThread = class(TCnTCPClientThread)
  {* �пͻ���������ʱ�Ĵ����̣߳��ӿͻ��˷����˫���д}
  protected
    function DoGetClientSocket: TCnClientSocket; override;
    procedure Execute; override;
  end;

{ TCnTCPForwarder }

function TCnTCPForwarder.DoGetClientThread: TCnTCPClientThread;
begin
  Result := TCnTCPForwardThread.Create(True);
end;

procedure TCnTCPForwarder.DoRemoteConnected;
begin
  if Assigned(FOnRemoteConnected) then
    FOnRemoteConnected(Self);
end;

procedure TCnTCPForwarder.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnTCPForwarderName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTCPForwarderComment;
end;

procedure TCnTCPForwarder.SetRemoteHost(const Value: string);
begin
  FRemoteHost := Value;
end;

procedure TCnTCPForwarder.SetRemotePort(const Value: Word);
begin
  FRemotePort := Value;
end;

{ TCnTCPForwardThread }

function TCnTCPForwardThread.DoGetClientSocket: TCnClientSocket;
begin
  Result := TCnForwarderClientSocket.Create;
end;

procedure TCnTCPForwardThread.Execute;
var
  Client: TCnForwarderClientSocket;
  Forwarder: TCnTCPForwarder;
  Buf: array[0..FORWARDER_BUF_SIZE - 1] of Byte;
  Ret: Integer;
  SockAddr: TSockAddr;
  ReadFds: TFDSet;
begin
  // �ͻ����������ϣ��¼����в����ɱ���
  DoAccept;
  Forwarder := TCnTCPForwarder(ClientSocket.Server);

  Client := TCnForwarderClientSocket(ClientSocket);
  Client.RemoteSocket := Forwarder.CheckSocketError(socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
  if Client.RemoteSocket = INVALID_SOCKET then
    Exit;

  SockAddr.sin_family := AF_INET;
  SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(TCnTCPClient.LookupHostAddr(Forwarder.RemoteHost))));
  SockAddr.sin_port := ntohs(Forwarder.RemotePort);

  if Forwarder.CheckSocketError(WinSock.connect(Client.RemoteSocket, SockAddr, SizeOf(SockAddr))) <> 0 then
  begin
    // ����Զ�̷�����ʧ�ܣ������˳�
    Forwarder.CheckSocketError(closesocket(Client.RemoteSocket));
    Client.RemoteSocket := INVALID_SOCKET;
    Exit;
  end;

  Forwarder.DoRemoteConnected;

  // ���ӳɹ��󣬱��߳̿�ʼѭ��ת�����������˳�
  while not Terminated do
  begin
    // SELECT ���� Socket �ϵ���Ϣ��׼������дȥ
    FD_ZERO(ReadFds);
    FD_SET(Client.Socket, ReadFds);
    FD_SET(Client.RemoteSocket, ReadFds);

    Ret := Forwarder.CheckSocketError(WinSock.select(0, @ReadFds, nil, nil, nil));
    if Ret <= 0 then
    begin
      Client.Shutdown;
      Exit;
    end;

    if FD_ISSET(Client.Socket, ReadFds) then // �ͻ�����������
    begin
      Ret := Client.Recv(Buf, SizeOf(Buf));
      if Ret <= 0 then
      begin
        Client.Shutdown;
        Exit;
      end;
      Ret := Client.RemoteSend(Buf, Ret); // ���������
      if Ret <= 0 then
      begin
        Client.Shutdown;
        Exit;
      end;
    end;

    if FD_ISSET(Client.RemoteSocket, ReadFds) then // �������������
    begin
      Ret := Client.RemoteRecv(Buf, SizeOf(Buf));
      if Ret <= 0 then
      begin
        Client.Shutdown;
        Exit;
      end;
      Ret := Client.Send(Buf, Ret); // �����ͻ���
      if Ret <= 0 then
      begin
        Client.Shutdown;
        Exit;
      end;
    end;
    Sleep(0);
  end;
end;

{ TCnForwarderClientSocket }

procedure TCnForwarderClientSocket.Shutdown;
begin
  inherited;
  if FRemoteSocket <> INVALID_SOCKET then
  begin
    (Server as TCnTCPForwarder).CheckSocketError(WinSock.shutdown(FRemoteSocket, 2)); // SD_BOTH
    (Server as TCnTCPForwarder).CheckSocketError(closesocket(FRemoteSocket));
    FRemoteSocket := INVALID_SOCKET;
  end;
end;

constructor TCnForwarderClientSocket.Create;
begin
  inherited;

end;

destructor TCnForwarderClientSocket.Destroy;
begin

  inherited;
end;

function TCnForwarderClientSocket.RemoteRecv(var Buf; Len,
  Flags: Integer): Integer;
begin
  Result := (Server as TCnTCPForwarder).CheckSocketError(
    WinSock.recv(FRemoteSocket, Buf, Len, Flags));
end;

function TCnForwarderClientSocket.RemoteSend(var Buf; Len,
  Flags: Integer): Integer;
begin
  Result := (Server as TCnTCPForwarder).CheckSocketError(
    WinSock.send(FRemoteSocket, Buf, Len, Flags));
end;

end.
