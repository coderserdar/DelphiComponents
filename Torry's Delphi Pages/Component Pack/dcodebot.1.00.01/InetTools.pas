
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit InetTools;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, WinSock, WinTools, WinInet;

type
  EWinSocketError = class(Exception);

  TSocketAddress = record
    case Integer of
      1: (Address: Longint);
      2: (
        A: Byte;
        B: Byte;
        C: Byte;
        D: Byte);
  end;

  TUniformLocator = record
    Host: string;
    Resource: string;
  end;

function UniformLocator(const Url: string): TUniformLocator;

function HttpRequest(const Locator: TUniformLocator): string;
function HttpResponse: string;

{ The AddrToHost function takes a string value such as 'microsoft.com'
  and returns 207.46.250.119 }

function HostToAddr(const Host: string): Cardinal;

{ The AddrToHost function takes a numeric value such as 207.46.250.119
  and returns 'microsoft.com' }

function AddrToHost(Addr: Cardinal): string;

{ The AddrToStr function takes a numeric value such as 207.46.250.119
  and returns a string value of '207.46.250.119' }

function AddrToStr(Addr: Cardinal): string;

{ The AddrToStr function takes a string value such as '207.46.250.119'
  and returns a numeric value of 207.46.250.119 }

function StrToAddr(const S: string): Cardinal;

const
  WM_SOCKET = WM_USER + $100;
  SD_RECEIVE = 0;
  SD_SEND = 1;
  SD_BOTH = 2;

type
  TWMSocket = packed record
    Msg: Cardinal;
    Socket: TSocket;
    Event: Word;
    Error: Word;
    Result: Longint;
  end;

  TSocketErrorEvent = procedure(Sender: TObject; Operation: string;
    ErrorCode: Integer; var Handled: Boolean) of object;

  TTcpSocket = class
  private
    FAddress: Longint;
    FConnected: Boolean;
    FReceiveBuffer: Pointer;
    FReceiveLength: Cardinal;
    FHost: string;
    FPort: Integer;
    FHandle: TSocket;
    FData: Pointer;
    FShuttingDown: Boolean;
    FWindow: TUtilityWindow;
    FOnAccept: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnRead: TNotifyEvent;
    FOnWrite: TNotifyEvent;
    FOnError: TSocketErrorEvent;
    function Error(const Operation: string; ErrorCode: Integer = 0): Integer;
    function GetHandleCreated: Boolean;
    procedure SetReceiveLength(Value: Cardinal);
    procedure WMSocket(var Message: TWMSocket); message WM_SOCKET;
  protected
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure AsyncSelect(Mask: Integer);
  public
    constructor Create; overload;
    constructor Create(Socket: THandle; Address: Longint; Port: Integer); overload;
    destructor Destroy; override;
    procedure Close;
    procedure Connect;
    procedure Listen;
    function Accept: TTcpSocket;
    function Send(Buffer: Pointer; Len: Cardinal): Cardinal; overload;
    function Send(const Text: string): Cardinal; overload;
    function Receive(Buffer: Pointer; Len: Cardinal): Cardinal; overload;
    function Receive(var Text: string): Cardinal; overload;
    property Connected: Boolean read FConnected;
    property Address: Longint read FAddress write FAddress;
    property ReceiveLength: Cardinal read FReceiveLength write SetReceiveLength;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Handle: TSocket read FHandle;
    property HandleCreated: Boolean read GetHandleCreated;
    property Data: Pointer read FData write FData;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
  end;

{ TInternetBuffer class }

  TInternetBuffer = class
  private
    FBlockSize: Integer;
    FBytesAvailable: DWORD;
    FBytesRead: DWORD;
    FData: string;
    FBlock: Pointer;
    function GetData: Pointer;
    procedure SetBlockSize(const Value: Integer);
    function GetDataSize: Integer;
    function GetAsString: string;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property BlockSize: Integer read FBlockSize write SetBlockSize;
    property BytesAvailable: DWORD read FBytesAvailable;
    property BytesRead: DWORD read FBytesRead;
    property Data: Pointer read GetData;
    property DataSize: Integer read GetDataSize;
    property AsString: string read GetAsString;
  end;

{ TInternetSession class }

  TWriteBufferEvent = procedure (Sender: TObject; Data: Pointer; DataSize: Integer;
    var CanWrite: Boolean) of object;

  TInternetSession = class(TObject)
  private
    FAborted: Boolean;
    FSession: HINTERNET;
    FURL: string;
    FBuffer: TInternetBuffer;
    FOnDownload: TNotifyEvent;
    FOnWriteBuffer: TWriteBufferEvent;
    function GetConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure Connect;
    procedure Disconnect;
    procedure Download;
    property Buffer: TInternetBuffer read FBuffer;
    property Connected: Boolean read GetConnected;
    property URL: string read FURL write FURL;
    property OnDownload: TNotifyEvent read FOnDownload write FOnDownload;
    property OnWriteBuffer: TWriteBufferEvent read FOnWriteBuffer write FOnWriteBuffer;
  end;

function Download(const URL: string): string;

implementation

uses
  StrConst;

function CheckSocketResult(ResultCode: Integer; const Operation: string): Integer;
begin
  Result := ResultCode;
  if Result <> 0 then
  begin
    Result := WSAGetLastError;
    raise EWinSocketError.CreateFmt(SWinSocketError, [SysErrorMessage(Result),
      Result, Operation]);
  end;
end;

var
  SocketsStarted: Boolean;

procedure Startup;
var
  Version: Word;
  Data: TWSAData;
begin
  if SocketsStarted then Exit;
  WordRec(Version).Lo := 2;
  WordRec(Version).Hi := 0;
  CheckSocketResult(WSAStartup(Version, Data), 'WSAStartup');
  SocketsStarted := True;
end;

function UniformLocator(const Url: string): TUniformLocator;
var
  S: string;
  I: Integer;
begin
  S := StringReplace(Trim(Url), ' ', '%20', [rfReplaceAll]);
  I := Pos('://', S);
  if I > 0 then
    S := Copy(S, I + 3, Length(S));
  I := Pos('/', S);
  if I > 0 then
  begin
    Result.Host := Copy(S, 1, I - 1);
    if Length(S) > I then
      Result.Resource := Copy(S, I, Length(S))
    else
      Result.Resource := '/';
  end
  else
  begin
    Result.Host := S;
    Result.Resource := '/';
  end;
end;

function HttpRequest(const Locator: TUniformLocator): string;
const
  Request =
    'GET %s HTTP/1.1'#13#10 +
    'Host: %s'#13#10 +
    'Connection: CLOSE'#13#10 +
    'Accept: */*'#13#10 +
    'Accept-Language: en-us'#13#10 +
    'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)'#13#10 +
    #13#10;
begin
  Result := Format(Request, [Locator.Resource, Locator.Host]);
end;

function HttpResponse: string;
const
	Response =
		'HTTP/1.1 200 OK'#13#10 +
		'Connection: close'#13#10 +
		'Server: terraserver/1.0'#13#10 +
		'Content-Type: text/html'#13#10 +
		#13#10;
begin
  Result := Response;
end;

function HostToAddr(const Host: string): Cardinal;
var
  HostEnt: PHostEnt;
begin
  Startup;
  Result := 0;
  HostEnt := gethostbyname(PChar(Host));
  if HostEnt <> nil then
    Result := PLongword(HostEnt^.h_addr_list^)^
  else
    CheckSocketResult(1, 'gethostbyname');
end;

function AddrToHost(Addr: Cardinal): string;
var
  HostEnt: PHostEnt;
begin
  Startup;
  HostEnt := gethostbyaddr(@Addr, SizeOf(Addr), AF_INET);
  if HostEnt <> nil then
    Result := HostEnt.h_name
  else
    Result := inet_ntoa(TInAddr(Addr));
end;

function AddrToStr(Addr: Cardinal): string;
begin
  Startup;
  Result := inet_ntoa(TInAddr(Addr));
end;

function StrToAddr(const S: string): Cardinal;
begin
  Startup;
  Result := inet_addr(PChar(S));
end;

{ TTcpSocket }

constructor TTcpSocket.Create;
begin
  Startup;
  FWindow := TUtilityWindow.Create(Self);
  FHandle := INVALID_SOCKET;
  ReceiveLength := 50000;
end;

constructor TTcpSocket.Create(Socket: THandle; Address: Longint; Port: Integer);
begin
  Create;
  FHandle := Socket;
  if HandleCreated then
  begin
    FConnected := True;
    AsyncSelect(FD_READ or FD_WRITE or FD_CLOSE);
  end;
  FAddress := Address;
  FPort := Port;
end;

destructor TTcpSocket.Destroy;
begin
  Close;
  ReceiveLength := 0;
  FWindow.Free;
end;

procedure TTcpSocket.CreateHandle;
begin
  if FHandle = INVALID_SOCKET then
  begin
    FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
    AsyncSelect(0);
  end;
end;

procedure TTcpSocket.DestroyHandle;
begin
  if HandleCreated then
  begin
    AsyncSelect(0);
    closesocket(FHandle);
    FHandle := INVALID_SOCKET;
    if FConnected and Assigned(FOnDisconnect) then
    begin
      FConnected := False;
      FOnDisconnect(Self);
    end
    else
      FConnected := False;
  end;
end;

procedure TTcpSocket.AsyncSelect(Mask: Integer);
begin
  if HandleCreated then
    WSAAsyncSelect(FHandle, FWindow.Handle, WM_SOCKET, Mask);
end;

function TTcpSocket.Error(const Operation: string; ErrorCode: Integer = 0): Integer;
var
  Handled: Boolean;
begin
  Handled := False;
  if ErrorCode = 0 then
    Result := WSAGetLastError
  else
    Result := ErrorCode;
  if Result <>  WSAEWOULDBLOCK then
  try
    if Assigned(FOnError) then
      FOnError(Self, Operation, Result, Handled);
    if not Handled then
      raise EWinSocketError.CreateFmt(SWinSocketError, [SysErrorMessage(Result),
        Result, Operation]);
  finally
    Close;
  end;
end;

procedure TTcpSocket.Close;
begin
  DestroyHandle;
end;

procedure TTcpSocket.Connect;
var
  Addr: TSockAddrIn;
begin
  Close;
  try
    Addr.sin_family := AF_INET;
    if FAddress <> 0 then
      Addr.sin_addr.s_addr := FAddress
    else
      Addr.sin_addr.s_addr := HostToAddr(FHost);
    Addr.sin_port := htons(FPort);
	except
  	Error('lookup');
    Exit;
  end;
	CreateHandle;
  if HandleCreated then
  begin
    AsyncSelect(FD_CONNECT or FD_CLOSE);
    if WinSock.connect(FHandle, Addr, SizeOf(Addr)) = SOCKET_ERROR then
      Error('connect');
  end;
end;

procedure TTcpSocket.Listen;
var
  Addr: TSockAddrIn;
begin
  Close;
  FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if HandleCreated then
  begin
    FConnected := True;
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := INADDR_ANY;
    Addr.sin_port := htons(Port);
    if WinSock.bind(FHandle, Addr, SizeOf(Addr)) = SOCKET_ERROR then
      Error('bind')
    else if WinSock.listen(FHandle, SOMAXCONN) = SOCKET_ERROR then
      Error('listen')
    else
	    AsyncSelect(FD_ACCEPT);
  end;
end;

function TTcpSocket.Accept: TTcpSocket;
var
  Addr: TSockAddrIn;
  Len: Integer;
  S: TSocket;
begin
  Result := nil;
  if HandleCreated then
  begin
    Len := SizeOf(Addr);
    S := WinSock.accept(FHandle, @Addr, @Len);
    if S <> INVALID_SOCKET then
      Result := TTcpSocket.Create(S, Addr.sin_addr.s_addr, ntohs(Addr.sin_port));
  end;
end;

function TTcpSocket.Send(Buffer: Pointer; Len: Cardinal): Cardinal;
var
  ErrorCode: Integer;
  Bytes: Integer;
begin
  Result := 0;
  if HandleCreated then
  begin
    ErrorCode := 0;
    Bytes := WinSock.send(FHandle, Buffer^, Len, 0);
    if Bytes = SOCKET_ERROR then
     begin
      ErrorCode := Error('send');
      Bytes := 0;
    end;
    Result := Bytes;
    if (Bytes = 0) and (ErrorCode <> WSAEWOULDBLOCK) then
      Close;
  end;
end;

function TTcpSocket.Send(const Text: string): Cardinal;
begin
  Result := Send(Pointer(Text), Length(Text));
end;

function TTcpSocket.Receive(Buffer: Pointer; Len: Cardinal): Cardinal;
var
  ErrorCode: Integer;
  Bytes: Integer;
begin
  Result := 0;
  if HandleCreated then
  begin
    ErrorCode := 0;
    Bytes := WinSock.recv(FHandle, Buffer^, Len, 0);
    if Bytes = SOCKET_ERROR then
    begin
      ErrorCode := Error('recv');
      Bytes := 0;
    end;
    Result := Bytes;
    if FShuttingDown then
      FShuttingDown := Bytes > 0
    else if (Bytes = 0) and (ErrorCode <> WSAEWOULDBLOCK) then
      Close;
  end;
end;

function TTcpSocket.Receive(var Text: string): Cardinal;
begin
  Result := 0;
  if FReceiveLength > 0 then
    Result := Receive(FReceiveBuffer, FReceiveLength);
  if Result > 0 then
    SetString(Text, PChar(FReceiveBuffer), Result)
  else
    Text := '';
end;

procedure TTcpSocket.WMSocket(var Message: TWMSocket);

  procedure Invoke(Event: TNotifyEvent);
  begin
    if Assigned(Event) then Event(Self);
  end;

begin
  if Message.Error <> 0 then
  case Message.Event of
    FD_READ: Error('recv', Message.Error);
    FD_WRITE: Error('send', Message.Error);
    FD_ACCEPT: Error('accept', Message.Error);
    FD_CONNECT: Error('connect', Message.Error);
    FD_CLOSE: Error('close', Message.Error);
  else
    Error('unknown', Message.Error);
  end
  else
  case Message.Event of
    FD_READ: Invoke(FOnRead);
    FD_WRITE: Invoke(FOnWrite);
    FD_ACCEPT: Invoke(FOnAccept);
    FD_CONNECT:
      begin
        FConnected := True;
        AsyncSelect(FD_READ or FD_WRITE or FD_CLOSE);
        Invoke(FOnConnect);
      end;
    FD_CLOSE:
      begin
        shutdown(FHandle, SD_SEND);
        FShuttingDown := True;
        if Assigned(FOnRead) then
          while FShuttingDown do Invoke(FOnRead);
        Close;
      end;
  end;
end;

function TTcpSocket.GetHandleCreated: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TTcpSocket.SetReceiveLength(Value: Cardinal);
begin
  if Value <> FReceiveLength then
  begin
    if FReceiveLength > 0 then
      FreeMem(FReceiveBuffer);
    FReceiveLength := Value;
    if FReceiveLength > 0 then
      GetMem(FReceiveBuffer, FReceiveLength);
  end;
end;

{ TInternetBuffer }

destructor TInternetBuffer.Destroy;
begin
  Clear;
  BlockSize := 0;
  inherited Destroy;
end;

procedure TInternetBuffer.Clear;
begin
  FBytesAvailable := 0;
  FBytesRead := 0;
  FData := '';
end;

procedure TInternetBuffer.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  if FData = '' then
    Exit;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TInternetBuffer.SaveToStream(Stream: TStream);
begin
  if FData = '' then
    Exit;
  Stream.Write(PChar(FData)^, Length(FData));
end;

procedure TInternetBuffer.SetBlockSize(const Value: Integer);
begin
  if Value <> FBlockSize then
  begin
    FBlockSize := Value;
    if FBlock <> nil then
      FreeMem(FBlock);
    FBlock := nil;
    if FBlockSize > 0 then
      GetMem(FBlock, FBlockSize);
  end;
end;

function TInternetBuffer.GetData: Pointer;
begin
  Result := Pointer(FData);
end;

function TInternetBuffer.GetDataSize: Integer;
begin
  Result := Length(FData);
end;

function TInternetBuffer.GetAsString;
begin
  Result := FData;
end;

{ TInternetSession }

constructor TInternetSession.Create;
begin
  inherited Create;
  FBuffer := TInternetBuffer.Create;
  FBuffer.BlockSize := 1024;
end;

destructor TInternetSession.Destroy;
begin
  FBuffer.Free;
  Disconnect;
  inherited Destroy;
end;

procedure TInternetSession.Abort;
begin
  FAborted := True;
end;

procedure TInternetSession.Connect;
var
  S: string;
begin
  if FSession = nil then
  begin
    S := ClassName;
    FSession := InternetOpen(PChar(S), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    if FSession = nil then
      RaiseLastWin32Error;
  end;
end;

procedure TInternetSession.Disconnect;
begin
  if FSession <> nil then
  begin
    InternetCloseHandle(FSession);
    FSession := nil
  end;
end;

procedure TInternetSession.Download;

  function GetBytesAvailable(Connection: HINTERNET): DWORD;
  var
    Buffer, BufferSize, Reserved: DWORD;
  begin
     BufferSize := SizeOf(Buffer);
     Reserved := 0;
     if HttpQueryInfo(Connection, HTTP_QUERY_CONTENT_LENGTH or
        HTTP_QUERY_FLAG_NUMBER, @Buffer, BufferSize, Reserved) then
       Result := Buffer
     else
       Result := 0;
  end;

var
  Connection: HINTERNET;
  BytesRead: Cardinal;
  CanWrite: Boolean;
  P: PChar;
begin
  FAborted := False;
  FBuffer.Clear;
  if FURL = '' then Exit;
  Connection := InternetOpenUrl(FSession, PChar(FURL), nil, 0,
    INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS or INTERNET_FLAG_RELOAD, 0);
  if Connection <> nil then
  try
    FBuffer.FBytesAvailable := GetBytesAvailable(Connection);
    InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);
    while (not FAborted) and (BytesRead > 0) do
    begin
      Inc(FBuffer.FBytesRead, BytesRead);
      CanWrite := True;
      if Assigned(FOnWriteBuffer) then
        FOnWriteBuffer(Self, FBuffer.FBlock, BytesRead, CanWrite);
      if CanWrite then
      begin
        {$WARNINGS OFF}
        SetLength(FBuffer.FData, Length(FBuffer.FData) + BytesRead);
        P := PChar(Integer(FBuffer.FData) + Length(FBuffer.FData) - BytesRead);
        {$WARNINGS ON}
        Move(PChar(FBuffer.FBlock)^, PChar(P)^, BytesRead);
      end;
      InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);
    end;
  finally
    InternetCloseHandle(Connection);
  end;
end;

function TInternetSession.GetConnected: Boolean;
begin
  Result := FSession <> nil;
end;

function Download(const URL: string): string;
var
  Session: TInternetSession;
begin
  Session := TInternetSession.Create;
  try
    Session.URL := URL;
    Session.Connect;
    Session.Download;
    Result := Session.Buffer.AsString;
    Session.Connect;
  except
    Result := '';
  end;
  Session.Free;
end;

initialization
  SocketsStarted := False;
finalization
  if SocketsStarted then
    WSACleanup;
end.

