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

unit CnThreadingTCPServer;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�����ͨѶ��������߳����� TCP Server ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ Liu Xiao
* ��    ע��һ�����׵Ķ��߳�����ʽ TCP Server���¿ͻ�������ʱ�����̣߳�������
*           �� OnAccept �¼���ѭ�� Recv/Send ���������ɣ��˳��¼���Ͽ����ӣ�
*           ���̳߳ػ���
* ����ƽ̨��PWin7 + Delphi 5
* ���ݲ��ԣ�PWin7 + Delphi 2009 ~
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2020.02.21 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Contnrs, WinSock, CnConsts, CnNetConsts, CnClasses;

type
  ECnServerSocketError = class(Exception);

  TCnThreadingTCPServer = class;

  TCnClientSocket = class
  {* ��ÿһ�������ͻ���ͨ�ŵ� Socket ��װ}
  private
    FSocket: TSocket;
    FRemoteIP: string;
    FRemotePort: Word;
    FServer: TCnThreadingTCPServer;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FLocalIP: string;
    FLocalPort: Word;
    FTag: TObject;
  protected
    procedure DoShutdown; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Shutdown; virtual;
    {* ��װ�Ĺر� Socket �Ĳ���}

    // send/recv �շ����ݷ�װ
    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    // ע�� Recv ���� 0 ʱ˵����ǰ�����ѶϿ�����������Ҫ���ݷ���ֵ���Ͽ�����

    property Server: TCnThreadingTCPServer read FServer write FServer;
    {* ������ TCnThreadingTCPServer ʵ������}
    property Socket: TSocket read FSocket write FSocket;
    {* �Ϳͻ���ͨѶ��ʵ�� Socket}
    property LocalIP: string read FLocalIP write FLocalIP;
    {* �ͻ���������ʱ�ı��� IP}
    property LocalPort: Word read FLocalPort write FLocalPort;
    {* �ͻ���������ʱ�ı��ض˿�}
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    {* Զ�̿ͻ��˵� IP}
    property RemotePort: Word read FRemotePort write FRemotePort;
    {* Զ�̿ͻ��˵Ķ˿�}
    property Tag: TObject read FTag write FTag;
    {* Tag ����������Ķ���}

    property BytesSent: Cardinal read FBytesSent;
    {* ���ͻ��˵ķ����ֽ������� Send �Żᱻͳ��}
    property BytesReceived: Cardinal read FBytesReceived;
    {* ���ͻ��˵���ȡ�ֽ������� Recv �Żᱻͳ��}
  end;

  TCnServerSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;

  TCnSocketAcceptEvent = procedure (Sender: TObject; ClientSocket: TCnClientSocket) of object;

  TCnTCPAcceptThread = class(TThread)
  {* �����̣߳�һ�� TCPServer ֻ��һ�������� Accept}
  private
    FServer: TCnThreadingTCPServer;
    FServerSocket: TSocket;
  protected
    procedure Execute; override;
  public
    property ServerSocket: TSocket read FServerSocket write FServerSocket;
    {* �����߳����õ� Socket ����}
    property Server: TCnThreadingTCPServer read FServer write FServer;
    {* �����߳������� TCPServer ����}
  end;

  TCnTCPClientThread = class(TThread)
  {* Accept �ɹ������ÿ���¿ͻ������Ĵ����̣߳������ж����ÿ����Ӧһ�� ClientSocket ��װ}
  private
    FClientSocket: TCnClientSocket;
  protected
    procedure Execute; override;
    procedure DoAccept; virtual;

    function DoGetClientSocket: TCnClientSocket; virtual;
    {* ���������ʹ����չ���ݵ� ClientSocket}
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property ClientSocket: TCnClientSocket read FClientSocket;
    {* ��װ�Ĺ�����ͻ���ʹ�õ��������}
  end;

  TCnThreadingTCPServer = class(TCnComponent)
  {* �򵥵Ķ��߳� TCP Server}
  private
    FSocket: TSocket;            // ������ Socket
    FAcceptThread: TCnTCPAcceptThread;
    FListLock: TRTLCriticalSection;
    FClientThreads: TObjectList; // �洢 Accept ����ÿ���� Client ͨѶ���߳�
    FActive: Boolean;
    FListening: Boolean;
    FActualLocalPort: Word;
    FLocalPort: Word;
    FLocalIP: string;
    FOnError: TCnServerSocketErrorEvent;
    FOnAccept: TCnSocketAcceptEvent;
    FCountLock: TRTLCriticalSection;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FOnShutdownClient: TNotifyEvent;
    FMaxConnections: Cardinal;
    procedure SetActive(const Value: Boolean);
    procedure SetLocalIP(const Value: string);
    procedure SetLocalPort(const Value: Word);
    function GetClientCount: Integer;
    function GetClient(Index: Integer): TCnClientSocket;
    function GetActualLocalPort: Word;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function CheckSocketError(ResultCode: Integer): Integer;
    function DoGetClientThread: TCnTCPClientThread; virtual;
    {* ���������ʹ��������Ϊ�� ClientThread}

    procedure DoShutdownClient(Client: TCnClientSocket); virtual;

    procedure ClientThreadTerminate(Sender: TObject);
    procedure IncRecv(C: Integer);
    procedure IncSent(C: Integer);

    function Bind: Boolean;
    function Listen: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    {* ��ʼ��������ͬ�� Active := True}
    procedure Close; virtual;
    {* �ر����пͻ������Ӳ�ֹͣ��������ͬ�� Active := False}
    function KickAll: Integer; virtual;
    {* �ر����пͻ�������}

    property ClientCount: Integer read GetClientCount;
    {* ��Ŀͻ�������}
    property Clients[Index: Integer]: TCnClientSocket read GetClient;
    {* ��Ŀͻ��˷�װ����}

    property BytesSent: Cardinal read FBytesSent;
    {* ���͸����ͻ��˵����ֽ���}
    property BytesReceived: Cardinal read FBytesReceived;
    {* �Ӹ��ͻ�����ȡ�����ֽ���}
    property Listening: Boolean read FListening;
    {* �Ƿ����ڼ���}
    property ActualLocalPort: Word read GetActualLocalPort;
    {* LocalPort Ϊ 0 ʱ���ѡ��һ���˿ڼ��������ظö˿�ֵ}
  published
    property Active: Boolean read FActive write SetActive;
    {* �Ƿ�ʼ����}
    property LocalIP: string read FLocalIP write SetLocalIP;
    {* �����ı��� IP}
    property LocalPort: Word read FLocalPort write SetLocalPort;
    {* �����ı��ض˿�}
    property MaxConnections: Cardinal read FMaxConnections write FMaxConnections;
    {* �ܹ����������������������� Accept ʱֱ�ӹر��½�������}

    property OnError: TCnServerSocketErrorEvent read FOnError write FOnError;
    {* �����¼�}
    property OnAccept: TCnSocketAcceptEvent read FOnAccept write FOnAccept;
    {* �¿ͻ���������ʱ�������¼�����������ѭ�����ա�������˳��¼���Ͽ�����}
    property OnShutdownClient: TNotifyEvent read FOnShutdownClient write FOnShutdownClient;
    {* ���� ClientSocket.Shutdown �ر�ĳ�ͻ���ʱ��������ʹ���߶���رտͻ���������ص�������Դ}
  end;

implementation

var
  WSAData: TWSAData;

{ TCnThreadingTCPServer }

function TCnThreadingTCPServer.Bind: Boolean;
var
  SockAddr, ConnAddr: TSockAddr;
  Len, Ret: Integer;
begin
  Result := False;
  if FActive then
  begin
    SockAddr.sin_family := AF_INET;
    if FLocalIP <> '' then
      SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(FLocalIP)))
    else
      SockAddr.sin_addr.S_addr := INADDR_ANY;

    SockAddr.sin_port := ntohs(FLocalPort);
    Result := CheckSocketError(WinSock.bind(FSocket, SockAddr, SizeOf(SockAddr))) = 0;

    FActualLocalPort := FLocalPort;
    if FActualLocalPort = 0 then
    begin
      Len := SizeOf(ConnAddr);
      Ret := CheckSocketError(WinSock.getsockname(FSocket, ConnAddr, Len));
      if Ret = 0 then
        FActualLocalPort := ntohs(ConnAddr.sin_port);
    end;
  end;
end;

function TCnThreadingTCPServer.CheckSocketError(ResultCode: Integer): Integer;
begin
  Result := ResultCode;
  if ResultCode = SOCKET_ERROR then
  begin
    if Assigned(FOnError) then
      FOnError(Self, WSAGetLastError);
  end;
end;

procedure TCnThreadingTCPServer.ClientThreadTerminate(Sender: TObject);
begin
  // �ͻ����߳̽����������߳���ɾ����Ч�� Socket ���߳����á�Sender �� Thread ʵ��
  EnterCriticalSection(FListLock);
  try
    FClientThreads.Remove(Sender);
  finally
    LeaveCriticalSection(FListLock);
  end;
end;

procedure TCnThreadingTCPServer.Close;
begin
  if not FActive then
    Exit;

  if FActive then
  begin
    // ֹ֪ͨͣ Accept �̣߳���ֹ������ Client ����
    WinSock.shutdown(FSocket, 2); // SD_BOTH������δ����ʱ�ĳ���
    CheckSocketError(WinSock.closesocket(FSocket)); // intterupt accept call
    FSocket := INVALID_SOCKET;
    FAcceptThread.Terminate;
    try
      FAcceptThread.WaitFor;
    except
      ;  // WaitFor ʱ�����Ѿ� Terminated�����³������Ч�Ĵ�
    end;
    FAcceptThread := nil;

    // �ߵ����пͻ���
    KickAll;

    FActualLocalPort := 0;
    FListening := False;
    FActive := False;
  end;
end;

constructor TCnThreadingTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  InitializeCriticalSection(FListLock);
  InitializeCriticalSection(FCountLock);
  FClientThreads := TObjectList.Create(False);
end;

destructor TCnThreadingTCPServer.Destroy;
begin
  Close;
  FClientThreads.Free;
  DeleteCriticalSection(FCountLock);
  DeleteCriticalSection(FListLock);
  inherited;
end;

function TCnThreadingTCPServer.DoGetClientThread: TCnTCPClientThread;
begin
  Result := TCnTCPClientThread.Create(True);
end;

procedure TCnThreadingTCPServer.DoShutdownClient(Client: TCnClientSocket);
begin
  if Assigned(FOnShutdownClient) then
    FOnShutdownClient(Client);
end;

function TCnThreadingTCPServer.GetActualLocalPort: Word;
begin
  Result := FActualLocalPort;
end;

function TCnThreadingTCPServer.GetClient(Index: Integer): TCnClientSocket;
begin
  if (Index >= 0) and (Index < FClientThreads.Count) then
    Result := TCnTCPClientThread(FClientThreads[Index]).ClientSocket
  else
    Result := nil;
end;

function TCnThreadingTCPServer.GetClientCount: Integer;
begin
  Result := FClientThreads.Count;
end;

procedure TCnThreadingTCPServer.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnThreadingTCPServerName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnThreadingTCPServerComment;
end;

procedure TCnThreadingTCPServer.IncRecv(C: Integer);
begin
  EnterCriticalSection(FCountLock);
  Inc(FBytesReceived, C);
  LeaveCriticalSection(FCountLock);
end;

procedure TCnThreadingTCPServer.IncSent(C: Integer);
begin
  EnterCriticalSection(FCountLock);
  Inc(FBytesSent, C);
  LeaveCriticalSection(FCountLock);
end;

function TCnThreadingTCPServer.KickAll: Integer;
var
  CT: TCnTCPClientThread;
begin
  Result := 0;

  // �ر����пͻ�������
  while FClientThreads.Count > 0 do
  begin
    EnterCriticalSection(FListLock);
    try
      if FClientThreads.Count = 0 then
        Exit;

      CT := TCnTCPClientThread(FClientThreads[0]);
      CT.ClientSocket.ShutDown;
      CT.Terminate;

      try
        CT.WaitFor;
        // �ȴ��߳̽�����ע�Ⲣ���ȴ��������߳���� OnTerminate ����
      except
        ; // WaitFor ʱ���ܾ����Ч����Ϊ�Ѿ�������
      end;
    finally
      LeaveCriticalSection(FListLock);
    end;

    // �߳̽���ʱ�߳�ʵ���Ѿ��� FClientThreads ���޳���
    Inc(Result);
  end;
  FClientThreads.Clear;
end;

function TCnThreadingTCPServer.Listen: Boolean;
begin
  if FActive and not FListening then
    FListening := CheckSocketError(WinSock.listen(FSocket, SOMAXCONN)) = 0;

  Result := FListening;
end;

procedure TCnThreadingTCPServer.Open;
begin
  if FActive then
    Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  FActive := FSocket <> INVALID_SOCKET;
  if FActive then
  begin
    if Bind then
    begin
      if Listen then
      begin
        // ���� Accept �̣߳����µĿͻ�������
        if FAcceptThread = nil then
        begin
          FAcceptThread := TCnTCPAcceptThread.Create(True);
          FAcceptThread.FreeOnTerminate := True;
        end;

        FAcceptThread.Server := Self;
        FAcceptThread.ServerSocket := FSocket;
        FAcceptThread.Resume;
      end;
    end;
  end;
end;

procedure TCnThreadingTCPServer.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      if Value then
        Open
      else
        Close;
    end
    else
      FActive := Value;
  end;
end;

procedure TCnThreadingTCPServer.SetLocalIP(const Value: string);
begin
  FLocalIP := Value;
end;

procedure TCnThreadingTCPServer.SetLocalPort(const Value: Word);
begin
  FLocalPort := Value;
end;

{ TCnAcceptThread }

procedure TCnTCPAcceptThread.Execute;
var
  Sock: TSocket;
  SockAddr, ConnAddr: TSockAddr;
  Len, Ret: Integer;
  ClientThread: TCnTCPClientThread;
begin
  FServer.FBytesReceived := 0;
  FServer.FBytesSent := 0;

  while not Terminated do
  begin
    Len := SizeOf(SockAddr);
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    try
      Sock := WinSock.accept(FServerSocket, @SockAddr, @Len);
    except
      Sock := INVALID_SOCKET;
    end;

    // �µĿͻ���������
    if Sock <> INVALID_SOCKET then
    begin
      // ���������������ֱ�Ӷϵ�
      if (FServer.MaxConnections > 0) and (FServer.ClientCount >= FServer.MaxConnections) then
      begin
        closesocket(Sock);
        Sock := INVALID_SOCKET;
        Continue;
      end;

      // ���µĿͻ��̣߳�������һ���ͻ��̳߳أ��治����̣߳�
      ClientThread := FServer.DoGetClientThread;
      ClientThread.FreeOnTerminate := True;
      ClientThread.OnTerminate := FServer.ClientThreadTerminate;

      ClientThread.ClientSocket.Socket := Sock;
      ClientThread.ClientSocket.Server := FServer;

      Len := SizeOf(ConnAddr);
      Ret := FServer.CheckSocketError(WinSock.getsockname(Sock, ConnAddr, Len));
      if Ret = 0 then
      begin
        // �ø� Socket �ı�����Ϣ
        ClientThread.ClientSocket.LocalIP := inet_ntoa(ConnAddr.sin_addr);
        ClientThread.ClientSocket.LocalPort := ntohs(ConnAddr.sin_port);
      end
      else // ���û�õ��������ü����� Socket �ı�����Ϣ��ע�� IP �����ǿ�
      begin
        ClientThread.ClientSocket.LocalIP := FServer.LocalIP;
        ClientThread.ClientSocket.LocalPort := FServer.ActualLocalPort;
      end;

      // �ø� Socket �ĶԶ˿ͻ�����Ϣ
      ClientThread.ClientSocket.RemoteIP := inet_ntoa(SockAddr.sin_addr);
      ClientThread.ClientSocket.RemotePort := ntohs(SockAddr.sin_port);

      EnterCriticalSection(FServer.FListLock);
      try
        FServer.FClientThreads.Add(ClientThread);
      finally
        LeaveCriticalSection(FServer.FListLock);
      end;
      ClientThread.Resume;
    end;
  end;
end;

{ TCnTCPClientThread }

constructor TCnTCPClientThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FClientSocket := DoGetClientSocket;
end;

destructor TCnTCPClientThread.Destroy;
begin
  FClientSocket.Free;
  inherited;
end;

procedure TCnTCPClientThread.DoAccept;
begin
  if Assigned(FClientSocket.Server.OnAccept) then
    FClientSocket.Server.OnAccept(FClientSocket.Server, FClientSocket);
end;

function TCnTCPClientThread.DoGetClientSocket: TCnClientSocket;
begin
  Result := TCnClientSocket.Create;
end;

procedure TCnTCPClientThread.Execute;
begin
  // �ͻ����������ϣ��¼����в����ɱ���
  DoAccept;

  // �ͻ���������ˣ����ԶϿ������ˣ�����¼���ͷû�����Ͽ��Ļ�
  FClientSocket.Shutdown;
end;

{ TCnClientSocket }

constructor TCnClientSocket.Create;
begin

end;

destructor TCnClientSocket.Destroy;
begin
  inherited;

end;

procedure TCnClientSocket.DoShutdown;
begin
  FServer.DoShutdownClient(Self);
end;

function TCnClientSocket.Recv(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(WinSock.recv(FSocket, Buf, Len, Flags));
  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesReceived, Result);
    FServer.IncRecv(Result);
  end;
end;

function TCnClientSocket.Send(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(WinSock.send(FSocket, Buf, Len, Flags));
  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesSent, Result);
    FServer.IncSent(Result);
  end;
end;

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ECnServerSocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ECnServerSocketError.Create('WSACleanup');
end;

procedure TCnClientSocket.Shutdown;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    FServer.CheckSocketError(WinSock.shutdown(FSocket, 2)); // SD_BOTH
    FServer.CheckSocketError(WinSock.closesocket(FSocket));
    FSocket := INVALID_SOCKET;

    DoShutdown;
  end;
end;

initialization
  Startup;

finalization
  Cleanup;

end.
