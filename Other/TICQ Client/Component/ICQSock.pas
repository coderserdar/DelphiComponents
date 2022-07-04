unit ICQSock;
{(C) Alex Demchenko}
{ Modified by NighTrader 02-04-2003 }

//{$DEFINE DEBUG} {Show debug errors}
//{$DEFINE PARSE} {Dump packets into file}

interface
uses
{$IFDEF DEBUGDC}
  dbugintf, Dialogs,
{$ENDIF}
  Windows, WinSock, Classes, ICQWorks, ICQLang;

const
  CNetPktLen = $FFFF; {Size of network packet}
  DefaultSockType = True; {True = non-blocking using blocking sockets architecture; False = blocking}

type
  PNetPacket = ^TNetPacket;
  TNetPacket = record
    Buf: array[0..CNetPktLen - 1] of Byte;
    BufLen: Word;
    Offset: Word;
    Next: PNetPacket;
  end;

  {Thread-safe implementation of software buffer}
  TNetBuffer = class(TObject)
  private
    FPkt: PNetPacket;
    Shared: Integer;
    CS: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    procedure Clear;
    procedure AddPacket(Buffer: Pointer; BufLen: LongWord);
    procedure DelPacket;
    function GetPacket(Buffer: Pointer): LongWord;
    function SkipData(Len: Word): Boolean;
    procedure AddStr(const Value: String);
    function GetStr: String;
    function GetLength: LongWord;
  end;

  {Thread-safe implementation of Berkeley sockets}
  TCustomSocket = class(TThread)
  private
    FSocket: TSocket;
    FIp: Integer;
    FDoConnect: Boolean;
    FWorking: Boolean;
    FConnected: Boolean;
    FAssync: Boolean;
    FBuffer: TNetBuffer;
    FDataSentEvent: Boolean;
    FErrLang: TICQLangType;
    procedure ProcessBuffer;
  protected
    FHost: String;
    FPort: Word;
    FLastError: Word;
    FLastErrMsg: String;
    Buffer: Pointer;
    BufLen: LongWord;
    function Resolve(const Host: String): Integer;
    procedure Execute; override;
    procedure FreeSocket;
    procedure OnError; virtual;
    procedure OnConnect; virtual;
    procedure OnConnectError; virtual;
    procedure OnDisconnect; virtual;
    procedure OnReceive; virtual;
    procedure OnDataSent; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartWork(Socket: TSocket);
    procedure Connect;
    procedure SendData(Buffer: Pointer; Len: LongWord);
    procedure SendStr(const Value: String);
    property Host: String read FHost write FHost;
    property Port: Word read FPort write FPort;
    property Working: Boolean read FWorking write FWorking;
    property Connected: Boolean read FConnected write FConnected;
    property Assync: Boolean read FAssync write FAssync;
    property ErrorLanguage: TICQLangType read FErrLang write FErrLang;
  end;

  TProxySocket = class(TCustomSocket)
  private
    FDestHost: String;
    FDestIp: Integer;
    FDestPort: Word;
    FProxyUser: String;
    FProxyPass: String;
    FProxyAuth: Boolean;
    FProxyReady: Boolean;
    FProxyResolve: Boolean;
  protected
    procedure Execute; override;
  public
    property Host: String read FDestHost write FDestHost; {Real Host}
    property Port: Word read FDestPort write FDestPort;   {Real Port}

    property ProxyHost: String read FHost write FHost;
    property ProxyPort: Word read FPort write FPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property ProxyAuth: Boolean read FProxyAuth write FProxyAuth default False;
    property ProxyResolve: Boolean read FProxyResolve write FProxyResolve default False;

    property ProxyReady: Boolean read FProxyReady write FProxyReady default False;
  end;

  TOnError = procedure(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String) of object;
  TOnRecveive = procedure(Sender: TObject; Buffer: Pointer; BufLen: LongWord) of object;

  {Extends TProxySocket with events}
  TEventSocket = class(TProxySocket)
  private
    FOnConnect: TNotifyEvent;
    FOnError: TOnError;
    FOnDisconnect: TNotifyEvent;
    FOnConnectError: TNotifyEvent;
    FOnReceive: TOnRecveive;
    FOnDataSent: TNotifyEvent;
  protected
    procedure OnConnect; override;
    procedure OnError; override;
    procedure OnConnectError; override;
    procedure OnDisconnect; override;
    procedure OnReceive; override;
    procedure OnDataSent; override;
  published
    property _OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property _OnError: TOnError read FOnError write FOnError;
    property _OnConnectError: TNotifyEvent read FOnConnectError write FOnConnectError;
    property _OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property _OnReceive: TOnRecveive read FOnReceive write FOnReceive;
    property _OnDataSent: TNotifyEvent read FOnDataSent write FOnDataSent;
  end;

  TSOCKSSocket = class(TEventSocket)
  private
    FSrcBuf: array[0..255] of Byte;
    FSrcLen: Word;
  end;

  TSOCKS4Socket = class(TSOCKSSocket)
  protected
    procedure OnConnect; override;
    procedure OnReceive; override;
  end;

  TSOCKS5Socket = class(TSOCKSSocket)
  private
    FSocksProgress: Word;
  protected
    procedure OnConnect; override;
    procedure OnReceive; override;
  end;

  THTTPSocket = class(TEventSocket)
  private
    FBuf: array[0..$FFFE] of Byte;
    FCurLen, FLen: LongWord;
  protected
    procedure OnConnect; override;
    procedure OnReceive; override;
  end;

  THTTPSSocket = class(TEventSocket)
  private
    FBuf: array[0..8191] of Byte;
    FCurLen: Word;
  protected
    procedure OnConnect; override;
    procedure OnReceive; override;
  end;

  TMySocket = class;
  TOnClientConnected = procedure(Sender: TObject; Socket: TMySocket) of object;
  TOnSrvSockConnected = procedure(Sender: TObject; Socket: TSocket) of object;

  TTCPServer = class(TThread)
  private
    FSocket: TSocket;
    FPort: Word;
    FClient: TSocket;
    FLastError: Word;
    FLastErrMsg: String;
    FErrLang: TICQLangType;
    FOnError: TOnError;
    FOnClientConnected: TOnSrvSockConnected;
  protected
    procedure Execute; override;
    procedure WaitForConnection;
    procedure FreeSocket;
  public
    constructor Create;
    destructor Destroy; override;

    function Start: Boolean;
    procedure OnError; virtual;
    procedure OnClientConnected; 
    property Port: Word read FPort write FPort;
    property ErrorLanguage: TICQLangType read FErrLang write FErrLang;
    property _OnError: TOnError read FOnError write FOnError;
    property _OnClientConnected: TOnSrvSockConnected read FOnClientConnected write FOnClientConnected;
  end;

  TOnAdvPktParse = procedure(Sender: TObject; Buffer: Pointer; BufLen: LongWord; Incoming: Boolean) of object;
  TOnRecv = procedure(Sender: TObject; Socket: TSocket; Buffer: Pointer; BufLen: LongWord) of object;

  TServerSocket = class(TTCPServer)
    private
      fOnCliConn:TOnSrvSockConnected;
      procedure DoConnEvent;
    public

      procedure OnClientConnected; virtual;
      property OnConnected: TOnSrvSockConnected read fOnCliConn write fOnCliConn;
    End;

  { TSrvSock like in MyScoket.pas }
  TSrvSocket = class(TObject)
    private
      fSrv:TTCPServer;
      fPort:word;
      FOnClientConnected: TOnClientConnected;
      FOnError:TOnError;
      fIsListening:Boolean;

      procedure OnSrvConnProc(Sender: TObject; Socket: TSocket);
      procedure OnSrvErrProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
      function GetPort:Word;
      procedure SetPort( aPort: Word);
    Public
      constructor Create;
      destructor Destroy; override;
      function StartServer(Port: Word): Boolean;
      function StopServer: Boolean;
      property Port: Word read GetPort write SetPort;
    published
      property OnError:TOnError read fOnError write fOnError;
      property OnClientConnected: TOnClientConnected read FOnClientConnected write FOnClientConnected;
  End;

  { TMySock like in MySocket.pas }
  TMySocket = class(TObject)
    private
      FHost:String;
      fPort:Word;
      // Threaded Socket
      FEventSocket:TEventSocket;
      FSocket:TSocket;
      //  Events
      FOnConnectError: TNotifyEvent;
      FOnDisconnect: TNotifyEvent;
      FOnPktParse: TOnAdvPktParse;
      FOnError: TOnError;
      FOnRecv: TOnRecv;
      FOnConnectProc: TNotifyEvent;
      FOnDataSent: TNotifyEvent;

      function GetClientSocket: TSocket;
      procedure SetClientSocket(Socket: TSocket);
      function IsConnected: Boolean;
      {$HINTS OFF}      // added by eraser 1.6.2003
      Procedure SetHost( aHost: String);
      Procedure SetPort( aPort: Word);
      Function  GetHost: String;
      Function  GetPort: Word;
      {$HINTS ON}       // added by eraser 1.6.2003
      Procedure SetProxyType( aProxyType: TProxyType);
      Procedure SetProxyHost( aProxyHost: String);
      Procedure SetProxyPort( aProxyPort: Word);
      Procedure SetProxyAuth( aProxyAuth: Boolean);
      Procedure SetProxyPass( aProxyPass: String);
      Procedure SetProxyUser( aProxyUser: String);
      Procedure SetProxyRslv( aProxyRslv: Boolean);
      Function  GetProxyType: TProxyType;
      Function  GetProxyHost: String;
      Function  GetProxyPort: Word;
      Function  GetProxyAuth: Boolean;
      Function  GetProxyPass: String;
      Function  GetProxyUser: String;
      Function  GetProxyRslv: Boolean;

      Procedure OnConnectErrorProc(Sender: TObject);
      Procedure OnDisconnectProc(Sender: TObject);
      Procedure OnConnect(Sender: TObject);
      Procedure OnErrorProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
      Procedure OnReceive(Sender: TObject; Buffer: Pointer; BufLen: LongWord);
      Procedure OnDataSentProc(Sender: TObject);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Connect; dynamic;
      procedure Disconnect;
      procedure SendData(var Buf; BufLen: LongWord);
      property Host: String read fHost write fHost;
      property Port: Word read fPort write fPort;
      property ProxyType: TProxyType read GetProxyType write SetProxyType;
      property ProxyHost: String read GetProxyHost write SetProxyHost;
      property ProxyPort: Word read GetProxyPort write SetProxyPort;
      property ProxyUser: String read GetProxyUser write SetProxyUser;
      property ProxyAuth: Boolean read GetProxyAuth write SetProxyAuth;
      property ProxyPass: String read GetProxyPass write SetProxyPass;
      property ProxyResolve: Boolean read GetProxyRslv write SetProxyRslv default False;
//      property WndHandle: THandle read GetWndHandle;
    published
      property OnConnectError: TNotifyEvent read FOnConnectError write FOnConnectError;
      property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
      property OnPktParseA: TOnAdvPktParse read FOnPktParse write FOnPktParse;
      property OnError: TOnError read FOnError write FOnError;
      property OnReceiveProc: TOnRecv read FOnRecv write FOnRecv;
      property OnConnectProc: TNotifyEvent read FOnConnectProc write FOnConnectProc;
      property OnDataSent: TNotifyEvent read FOnDataSent write FOnDataSent;
      property ClientSocket: TSocket read GetClientSocket write SetClientSocket;
      property Connected: Boolean read IsConnected;
  End;

var
  WSAData: TWSAData;
  WSAStarted: Boolean;

function WaitForRead(Sock: TSocket; Timeout: DWord): Boolean;
function GetHTTPStatus(List: TStringList): String;

function WSockAddrToIp(Value: LongWord): String;
function WSAErrorToStr(ErrorNo: Integer): String;
function GetLocalIP: Integer;
function FindBindPort: Word;



implementation

const
  b64alphabet: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function EncodeBase64(Value: String): String;
const
  pad: PChar = '====';

  function EncodeChunk(const Chunk: String): String;
  var
    W: LongWord;
    i, n: Byte;
  begin
    n := Length(Chunk); W := 0;
    for i := 0 to n - 1 do
      W := W + Ord(Chunk[i + 1]) shl ((2 - i) * 8);
    Result := b64alphabet[(W shr 18) and $3f] +
              b64alphabet[(W shr 12) and $3f] +
              b64alphabet[(W shr 06) and $3f] +
              b64alphabet[(W shr 00) and $3f];
    if n <> 3 then
      Result := Copy(Result, 0, n + 1) + Copy(pad, 0, 3 - n);   //add padding when out len isn't 24 bits
  end;

begin
  Result := '';
  while Length(Value) > 0 do
  begin
    Result := Result + EncodeChunk(Copy(Value, 0, 3));
    Delete(Value, 1, 3);
  end;
end;

function DecodeBase64(Value: String): String;
  function DecodeChunk(const Chunk: String): String;
  var
    W: LongWord;
    i: Byte;
  begin
    W := 0; Result := '';
    for i := 1 to 4 do
      if Pos(Chunk[i], b64alphabet) <> 0 then
        W := W + Word((Pos(Chunk[i], b64alphabet) - 1)) shl ((4 - i) * 6);
    for i := 1 to 3 do
      Result := Result + Chr(W shr ((3 - i) * 8) and $ff);
  end;
begin
  Result := '';
  if Length(Value) mod 4 <> 0 then Exit;
  while Length(Value) > 0 do
  begin
    Result := Result + DecodeChunk(Copy(Value, 0, 4));
    Delete(Value, 1, 4);
  end;
end;

procedure ShowMessage(const Msg: String);
begin
  MessageBox(0, PChar(Msg), 'Message', 0);
end;

function WaitForRead(Sock: TSocket; Timeout: DWord): Boolean;
var
  readfd: TFDSet;
  tv: TimeVal;
begin
  tv.tv_sec := 0; tv.tv_usec := Timeout;
  FD_ZERO(readfd); FD_SET(Sock, readfd);
  if select(0, @readfd, nil, nil, @tv) < 1 then
    Result := False
  else
    Result := True;
end;

function GetHTTPStatus(List: TStringList): String;
var
  i, c: Word;
  S: String;
begin
  Result := '';
  if List.Count < 1 then Exit;
  S := List.Strings[0]; c := 0;
  for i := 1 to Length(S) do
    if c = 1 then
      Result := Result + S[i]
    else
      if S[i] = ' ' then Inc(c);
end;

{ TMySocket }
constructor TMySocket.Create;
Begin
  Inherited Create;
  FEventSocket := TEventSocket.Create;
  FEventSocket._OnConnectError := OnConnectErrorProc;
  FEventSocket._OnConnect      := OnConnect;
  FEventSocket._OnDisconnect   := OnDisconnectProc;
  FEventSocket._OnError        := OnErrorProc;
  FEventSocket._OnReceive      := OnReceive;
  FEventSocket._OnDataSent     := OnDataSentProc;
  fSocket := INVALID_SOCKET;
End;

destructor TMySocket.Destroy;
Begin
  FEventSocket.Free;
End;

Procedure TMySocket.OnConnectErrorProc(Sender: TObject);
Begin
  If Assigned(fOnConnectError) then
    fOnConnectError(Self);
End;

Procedure TMySocket.OnDisconnectProc(Sender: TObject);
Begin
  If Assigned(fOnDisconnect) then
    fOnDisconnect(Self);
  fEventSocket.FreeSocket;
End;

Procedure TMySocket.OnConnect(Sender: TObject);
Begin
  If Assigned(FOnConnectProc) then
    fOnConnectProc(Self);
End;

Procedure TMySocket.OnErrorProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
Begin
  If Assigned(fOnError) then
    fOnError(Self, ErrorType, ErrorMsg);
End;

Procedure TMySocket.OnReceive(Sender: TObject; Buffer: Pointer; BufLen: LongWord);
Begin
  If Assigned(fOnRecv) then
    fOnRecv(Self, ClientSocket, Buffer, BufLen);
End;

Procedure TMySocket.OnDataSentProc(Sender: TObject);
Begin
  If Assigned(fOnDataSent) then
    fOnDataSent(Self);
End;

function TMySocket.GetClientSocket: TSocket;
Begin
  If FSocket = INVALID_SOCKET then
    Result := FEventSocket.FSocket
  Else
    Result := FSocket;
End;

procedure TMySocket.SetClientSocket(Socket: TSocket);
Begin
  fSocket := Socket;
  fEventSocket.ProxyReady := True;
  FEventSocket.StartWork(fSocket);

End;

function TMySocket.IsConnected: Boolean;
Begin
  Result := FEventSocket.Connected;
End;

procedure TMySocket.Connect;
Begin
  If FEventSocket.Connected Then exit;
  FEventSocket.Host := fHost;
  FEventSocket.Port := fPort;

  FEventSocket.ProxyReady := True;
  FEventSocket.ProxyHost  := fHost;
  FEventSocket.ProxyPort  := fPort;

  FEventSocket.Connect;
End;

procedure TMySocket.Disconnect;
Begin
  If Not FEventSocket.Connected then Exit;
  FEventSocket.FreeSocket;
End;

procedure TMySocket.SendData(var Buf; BufLen: LongWord);
Begin
  FEventSocket.SendData(@Buf, BufLen);
End;

Procedure TMySocket.SetHost( aHost: String);
Begin
  FEventSocket.Host := aHost;
End;

Procedure TMySocket.SetPort( aPort: Word);
Begin
  FEventSocket.Port := aPort
End;

Function  TMySocket.GetHost: String;
Begin
  Result := FEventSocket.Host;
End;

Function  TMySocket.GetPort: Word;
Begin
  Result := FEventSocket.Port;
End;

Procedure TMySocket.SetProxyType( aProxyType: TProxyType);
Begin
  //
End;

Procedure TMySocket.SetProxyHost( aProxyHost: String);
Begin
  FEventSocket.ProxyHost := aProxyHost;
End;

Procedure TMySocket.SetProxyPort( aProxyPort: Word);
Begin
  FEventSocket.ProxyPort := aProxyPort;
End;

Procedure TMySocket.SetProxyAuth( aProxyAuth: Boolean);
Begin
  FEventSocket.ProxyAuth := aProxyAuth;
End;

Procedure TMySocket.SetProxyPass( aProxyPass: String);
Begin
  FEventSocket.ProxyPass := aProxyPass;
End;

Procedure TMySocket.SetProxyUser( aProxyUser: String);
Begin
  FEventSocket.ProxyUser := aProxyUser;
End;

Procedure TMySocket.SetProxyRslv( aProxyRslv: Boolean);
Begin
  FEventSocket.ProxyResolve := aProxyRslv;
End;

Function  TMySocket.GetProxyType: TProxyType;
Begin
  Result := P_NONE;
End;

Function  TMySocket.GetProxyHost: String;
Begin
  Result := FEventSocket.ProxyHost;
End;

Function  TMySocket.GetProxyPort: Word;
Begin
  Result := FEventSocket.ProxyPort;
End;

Function  TMySocket.GetProxyAuth: Boolean;
Begin
  Result := FEventSocket.ProxyAuth;
End;

Function  TMySocket.GetProxyPass: String;
Begin
  Result := FEventSocket.ProxyPass;
End;

Function  TMySocket.GetProxyUser: String;
Begin
  Result := FEventSocket.ProxyUser;
End;

Function  TMySocket.GetProxyRslv: Boolean;
Begin
  Result := FEventSocket.ProxyResolve;
End;

{ TSrvSocket }
constructor TSrvSocket.Create;
Begin
  inherited Create;
  fSrv := TServerSocket.Create;
  //fSrv.OnConnected := OnCliConnProc;
  fSrv.FreeOnTerminate    := False;
  fSrv._OnClientConnected := OnSrvConnProc;
  fSrv._OnError           := OnSrvErrProc;
  fIsListening := False;
End;

destructor TSrvSocket.Destroy;
Begin
  fSrv.FreeOnTerminate := True;
  StopServer;
  inherited Destroy;
End;

procedure TSrvSocket.OnSrvErrProc(Sender: TObject; ErrorType: TErrorType; ErrorMsg: String);
Begin
  If assigned(fOnError) then
    fOnError(Sender, ErrorType, ErrorMsg);
End;

procedure TSrvSocket.OnSrvConnProc(Sender: TObject; Socket: TSocket);
Var
  aMS:TMySocket;
Begin
  If Assigned(fOnClientConnected) then Begin
    aMS := TMySocket.Create;
    aMS.ProxyType := P_None;
    aMS.ClientSocket := Socket;
    fOnClientConnected(Self, aMS);
  End;
End;

function TSrvSocket.GetPort:Word;
Begin
  Result := fSrv.Port;
End;

procedure TSrvSocket.SetPort( aPort: Word);
Begin
  fSrv.Port := aPort;
  fPort := aPort;
End;

function TSrvSocket.StartServer(Port: Word): Boolean;
Begin
  fSrv.Port := Port;
  fPort := Port;
  Result := fSrv.Start;
  fIsListening := Result;
End;

function TSrvSocket.StopServer: Boolean;
Begin
  fSrv.FreeSocket;
  fIsListening := False;
  Result := True;
End;

{ TServerSocket }

procedure TServerSocket.OnClientConnected;
Begin
  If assigned(fOnCliConn) then
    Synchronize(DoConnEvent);
End;

procedure TServerSocket.DoConnEvent;
Begin
  fOnCliConn(Self, fClient);
End;

constructor TNetBuffer.Create;
begin
  inherited Create;
  FPkt := nil;
  Shared := 0;
end;

destructor TNetBuffer.Destroy;
begin
  Clear;
  if Shared = 1 then DeleteCriticalSection(CS);
  inherited;
end;

{Swap pointers}
procedure XChg(var Critical, Normal); assembler;
asm
  mov  ecx, [edx]
  xchg [eax], ecx
  mov  [edx], ecx
end;

procedure TNetBuffer.Enter; {Synchronization - enter critical section}
var
  j: Integer;
begin
  j := 1; XChg(Shared, j); if j = 0 then InitializeCriticalSection(CS);
  EnterCriticalSection(CS);
end;

procedure TNetBuffer.Leave; {Synchronization - leave critical section}
begin
  LeaveCriticalSection(CS);
end;

procedure TNetBuffer.Clear;
var
  p: Pointer;
begin
  while FPkt <> nil do begin
    p := FPkt^.Next;
    FreeMem(FPkt);
    FPkt := p;
  end;
end;

procedure TNetBuffer.AddPacket(Buffer: Pointer; BufLen: LongWord);
var
  p: PNetPacket;
begin
  if BufLen > CNetPktLen then BufLen := CNetPktLen;
  if FPkt = nil then begin
    GetMem(FPkt, SizeOf(TNetPacket));
    p := FPkt;
  end else begin
    p := FPkt;
    while p <> nil do begin
      if p^.Next = nil then Break;
      p := p^.Next;
    end;
    GetMem(p^.Next, SizeOf(TNetPacket));
    p := p^.Next;
  end;
  p^.BufLen := BufLen;
  p^.Offset := 0;
  p^.Next := nil;
  Move(Buffer^, p^.Buf, BufLen);
end;

procedure TNetBuffer.DelPacket;
var
  p: PNetPacket;
begin
  if (FPkt = nil) then Exit;
  if FPkt^.Next <> nil then
  begin
    p := FPkt^.Next;
    FreeMem(FPkt);
    FPkt := p;
  end else
  begin
    FreeMem(FPkt);
    FPkt := nil;
  end;
end;

function TNetBuffer.GetPacket(Buffer: Pointer): LongWord;
begin
  if (FPkt = nil) or (FPkt^.Offset >= FPkt^.BufLen) then
    Result := 0
  else begin
    Move(Ptr(LongWord(@FPkt^.Buf) + FPkt^.Offset)^, Buffer^, FPkt^.BufLen - FPkt^.Offset);
    Result := FPkt^.BufLen - FPkt^.Offset;
  end;
end;

function TNetBuffer.SkipData(Len: Word): Boolean;
begin
  if FPkt = nil then
    Result := True
  else begin
    Inc(FPkt^.Offset, Len);
    Result := FPkt^.Offset >= FPkt^.BufLen;
  end;
end;

procedure TNetBuffer.AddStr(const Value: String);
begin
  AddPacket(@Value[1], Length(Value));
end;

function TNetBuffer.GetStr: String;
var
  p: array[0..CNetPktLen] of Char;
begin
  p[GetPacket(@p)] := #0;
  Result := PChar(@p);
end;

function TNetBuffer.GetLength: LongWord;
var
  p: PNetPacket;
begin
  Result := 0;
  p := FPkt;
  while p <> nil do begin
    Inc(Result, p^.BufLen);
    p := p^.Next;
  end;
end;


//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

{ TCustromSocket }

{** CONSTRUCTOR **}
constructor TCustomSocket.Create;
begin
  inherited Create(True);
  FConnected := False;
  FWorking := False;
  FAssync := DefaultSockType;
  FBuffer := TNetBuffer.Create;
end;


{** DESTRUCTOR **}
destructor TCustomSocket.Destroy;
begin
  FreeSocket;
  if FBuffer <> nil then FBuffer.Free;
  inherited Destroy;
end;

procedure TCustomSocket.ProcessBuffer;
var
  ret: Integer;
  Buf: array[0..CNetPktLen - 1] of Byte;
begin
  if FSocket <> INVALID_SOCKET then
    while True do
    begin
      FBuffer.Enter;
      ret := FBuffer.GetPacket(@Buf);
      FBuffer.Leave;
      if (ret < 1) then begin {All data has been sent}
        if (not FDataSentEvent) then
          Synchronize(OnDataSent);
        FDataSentEvent := True;
        Exit;
      end;
      if send(FSocket, Buf, ret, 0) = SOCKET_ERROR then begin
        FLastError := WSAGetLastError;
        FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SEND);
        Synchronize(OnError);
        Break;
      end else
      FBuffer.Enter;
      if FBuffer.SkipData(ret) then
        FBuffer.DelPacket;
      FBuffer.Leave;
    end;
end;

{Tries to resolve host and stores result in FIp, returns False on error}
function TCustomSocket.Resolve(const Host: String): Integer;
var
  he: PHostEnt;
begin
  Result := inet_addr(PChar(Host));
  if DWord(Result) = DWord(INADDR_NONE) then
  begin
    he := gethostbyname(PChar(Host));
    if he = nil then Exit;
    Result := PInteger(he^.h_addr_list^)^;
  end;
end;

{** Main Thread **}
procedure TCustomSocket.Execute;
var
  sin: sockaddr_in;
  buf: array[0..CNetPktLen - 1] of Char;
  rc: Integer;
begin
  if (FDoConnect) and (not Terminated) then begin
    {Resolving Host}
    FIp := Resolve(FHost);
    {$IFDEF DEBUGDC}
      SendDebugFmtEx('Resolved FHost=%s to FIp=%d', [FHost, FIP], mtInformation);
    {$ENDIF}
    if DWord(FIp) = DWord(INADDR_NONE) then begin
      if (not Terminated) then begin
        FLastError := WSAGetLastError;
        FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_RESOLVE);
        Synchronize(OnError);
      end;
      Exit;
    end else begin
      sin.sin_family := PF_INET;
      sin.sin_addr.S_addr := FIp;
      sin.sin_port := htons(FPort);
      {Connecting...}
      if WinSock.connect(FSocket, sin, SizeOf(sin)) = SOCKET_ERROR then begin
        FLastError := WSAGetLastError;
        FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_CONNECT);
        if (not Terminated) then Synchronize(OnConnectError);
        Exit;
      end;
    end;
  end;
  {Connected successfully!}
  FConnected := True;
  {$IFDEF DEBUGDC}
    SendDebugEx('Connected', mtInformation);
  {$ENDIF}
  Synchronize(OnConnect);
  {Receiving data if any avaible}
  while not Terminated do begin
  {
     If no data arrived in 100ms we send qued data to the connected socket.
     That slows sometimes connection, but doesn't require another thread
     for sending.
  }
    if Assync and (not WaitForRead(FSocket, 10)) then begin
      ProcessBuffer;
    end else begin
    {$IFDEF DEBUGDC}
      SendDebugEx('Using OnRecv.', mtInformation);
    {$ENDIF}

      rc := recv(FSocket, buf, SizeOf(buf), 0);
      if (rc = 0) and (not Terminated) then begin Synchronize(OnDisconnect); Break; end;
      if (rc = SOCKET_ERROR) then begin
        rc := WSAGetLastError;
        if ((rc = WSAECONNRESET) or (rc = WSAECONNABORTED)) and (not Terminated) then begin Synchronize(OnDisconnect); Break; end;
        if (not Terminated) then begin
          FLastError := rc;
          FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_RECV);
          Synchronize(OnError);
        end;
        Break;
      end;
      Buffer := @buf;
      BufLen := rc;
      if rc > 0 then
        if Assync then
          Synchronize(OnReceive)
        else
          OnReceive;
    end;
  end;
end;

{Start work w/o allocating the new socket}
procedure TCustomSocket.StartWork(Socket: TSocket);
begin
  if FWorking then Exit else FWorking := True;
  FDoConnect := False;
  FConnected := False;
  FSocket := Socket;
  Resume;
end;

{Start work with new socket}
procedure TCustomSocket.Connect;
begin
  if FWorking then Exit else FWorking := True;
  FDoConnect := True;
  FConnected := False;
  FSocket := WinSock.socket(PF_INET, SOCK_STREAM, 0);
  if FSocket = INVALID_SOCKET then begin
    FLastError := WSAGetLastError;
    FLastErrMsg :=  ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SOCKET);
    Synchronize(OnError);
    Exit;
  end;
  Resume;
end;

{Frees the WSA socket}
procedure TCustomSocket.FreeSocket;
begin
  if not Terminated then
    Terminate;
  if FSocket <> INVALID_SOCKET then begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  FWorking := False;
end;

{Sends data to connected socket}
procedure TCustomSocket.SendData(Buffer: Pointer; Len: LongWord);
begin
  {$IFDEF DEBUGDC}
    SendDebugEx('SendData', mtInformation);
  {$ENDIF}
  if Len = 0 then Exit;
  if (Assync) and (FBuffer <> nil) then begin
    FDataSentEvent := False;
    FBuffer.Enter;
    FBuffer.AddPacket(Buffer, Len);
    FBuffer.Leave;
  end else
    if send(FSocket, Buffer^, Len, 0) = SOCKET_ERROR then begin
      FLastError := WSAGetLastError;
      FLastErrMsg :=  ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SEND);
      Synchronize(OnError);
      Exit;
    end;
end;

procedure TCustomSocket.SendStr(const Value: String);
begin
  SendData(@Value[1], Length(Value));
  if ClassName = 'THTTPSocket' then
  begin
    {$IFDEF PARSE}
    LogText('http.txt', 'Sent (proxy): '#13#10 + Value);
    {$ENDIF}
  end;
end;

procedure TCustomSocket.OnError;
begin
  FreeSocket;
end;

procedure TCustomSocket.OnConnect;
begin
end;

procedure TCustomSocket.OnConnectError;
begin
  FreeSocket;
end;

procedure TCustomSocket.OnDisconnect;
begin
  FreeSocket;
end;

procedure TCustomSocket.OnReceive;
begin
end;

procedure TCustomSocket.OnDataSent;
begin
end;






{ TProxySocket }

procedure TProxySocket.Execute;
begin
  if FProxyResolve then
    FDestIp := Resolve(FDestHost); {Resolve destination host, if possible}
  inherited;
end;



{ TEventSocket }

procedure TEventSocket.OnConnect;
begin
  if ClassName <> 'THTTPSocket' then begin
    if ProxyReady and Assigned(_OnConnect) then
      FOnConnect(Self);
  end else
    if Assigned(_OnConnect) then
      FOnConnect(Self);
  inherited;
end;

procedure TEventSocket.OnError;
begin
  inherited;
  if Assigned(_OnError) then
    FOnError(Self, ERR_SOCKET, FLastErrMsg);
end;

procedure TEventSocket.OnConnectError;
begin
  inherited;
  if Assigned(_OnConnectError) then
    FOnConnectError(Self);
end;

procedure TEventSocket.OnDisconnect;
begin
  inherited;
  if Assigned(_OnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TEventSocket.OnReceive;
begin
  if ProxyReady then begin
    if Assigned(_OnReceive) then
      FOnReceive(Self, Buffer, BufLen);
  end else
    inherited;
end;

procedure TEventSocket.OnDataSent;
begin
  if Assigned(_OnDataSent) then
    FOnDataSent(Self);
end;





{ TSOCKS4Socket }

procedure TSOCKS4Socket.OnConnect;
var
  buf: array[0..255] of Byte;
begin
  inherited;
  if not ProxyReady then begin
    FSrcLen := 0;
    buf[0] := 4;                                                          //Socks4
    buf[1] := 1;                                                          //Code: 1 - Connect
    PWord(Ptr(LongWord(@Buf) + 2))^ := htons(Port);                       //Port
    if FProxyResolve then
      PDWord(Ptr(LongWord(@Buf) + 4))^ := inet_addr('0.0.0.1')            //SOCKS4a extension
    else
      PDWord(Ptr(LongWord(@Buf) + 4))^ := FDestIp;                        //Host
    if ProxyAuth then                                                     //Add some packet specified data when using proxy authentication
    begin
      if Length(ProxyUser) > 0 then                                       //Test if ProxyUserID string is not nil
        Move(ProxyUser[1], buf[8], Length(ProxyUser));                    //If it's not then add it to packet
      buf[8 + Length(ProxyUser)] := 0;                                    //Always present NULL termination byte
    end else
      buf[8] := 0;                                                        //Always present NULL termination byte
    SendData(@buf, 9 + Length(ProxyUser));                                //Send data
    if FProxyResolve then                                                 //SOCKS4a extension
      SendStr(FDestHost + #0);
  end;
end;

procedure TSOCKS4Socket.OnReceive;
var
  i: LongWord;
begin
  if not ProxyReady then begin
    for i := 0 to BufLen - 1 do
    begin
      FSrcBuf[FSrcLen] := PByte(LongWord(Buffer) + i)^;
      Inc(FSrcLen);
      if FSrcLen = 8 then
      begin
        Dec(BufLen, i);
        FSrcLen := 0;
        if PByte(Ptr(LongWord(Buffer) + 1))^ <> 90 then
        begin
          FLastError := 0;
          FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SOCKS4CONN);
          Synchronize(OnConnectError);
          Exit;
        end;
        ProxyReady := True;
        Synchronize(OnConnect);
        if i < BufLen - 1 then begin
          Buffer := Ptr(LongWord(Buffer) + i); //Continue handling of remaining data
          Synchronize(OnReceive);
        end;
      end;
    end;
  end else
    inherited;
end;




{ TSOCKS5Socket }

procedure TSOCKS5Socket.OnConnect;
var
  buf: array[0..2] of Byte;
begin
  inherited;
  if not ProxyReady then begin
    FSocksProgress := 0;                //Socks authorization progress
    buf[0] := 5;                        //Socks5
    buf[1] := 1;                        //Number of methods
    if ProxyAuth then                   //Choose auth method
      buf[2] := 2                       //Use authentication
    else
      buf[2] := 0;                      //Plain connect
    SendData(@buf, 3);                  //Send SOCKS5 initialization packet
  end;
end;

procedure TSOCKS5Socket.OnReceive;
  procedure DoSocks5Connect;
  var
    len: Word;
    buf: array[0..255] of Byte;
  begin
    if not ProxyResolve then         //Socks5 supports on-server-resolving
      len := 4
    else
      len := Length(Host) + 1;
    buf[0] := 5;                        //Socks5
    buf[1] := 1;                        //Command: connect
    buf[2] := 0;                        //Reserved
    if ProxyResolve then
    begin
      buf[3] := 3;
      buf[4] := len - 1;
      Move(PChar(Host)^, buf[5], len - 1);
    end else
    begin
      buf[3] := 1;
      PDWord(LongWord(@buf) + 4)^ := FDestIp;
    end;
    PWord(LongWord(@buf) + 4 + Len)^ := htons(Port);
    SendData(@buf, 6 + Len);
  end;
var
  i: LongWord;
  UserLen, PassLen: Word;
begin
  if not ProxyReady then begin
    for i := 0 to BufLen - 1 do
    begin
      FSrcBuf[FSrcLen] := PByte(LongWord(Buffer) + i)^;
      Inc(FSrcLen);
      case FSocksProgress of
        0:
        begin
          if FSrcLen = 2 then
          begin
            if FSrcBuf[1] = $ff then
            begin
              FLastError := 0;
              FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SOCKS5AUTH);
              Synchronize(OnConnectError);
              Exit;
            end;
            FSrcLen := 0;
            if FSrcBuf[1] = 2 then
            begin
              UserLen := Length(ProxyUser);
              PassLen := Length(ProxyPass);
              FSrcBuf[0] := 1;
              FSrcBuf[1] := UserLen;
              Move(PChar(ProxyUser)^, Ptr(LongWord(@FSrcBuf) + 2)^, UserLen);
              FSrcBuf[UserLen + 2] := PassLen;
              Move(PChar(ProxyPass)^, Ptr(LongWord(@FSrcBuf) + 3 + UserLen)^, UserLen);
              SendData(@FSrcBuf, 3 + UserLen + PassLen);
              Inc(FSocksProgress);
            end else
            begin
              Inc(FSocksProgress, 2);
              DoSocks5Connect;
            end;
          end;
        end;
        1:
        begin
          if FSrcLen = 2 then
          begin
            if FSrcBuf[1] <> 0 then
            begin
              FLastError := 0;
              FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SOCKS5NA);
              Synchronize(OnConnectError);
              Exit;
            end;
            FSrcLen := 0;
            Inc(FSocksProgress);
            DoSocks5Connect;
          end;
        end;
        2:
        begin
          if FSrcLen = 10 then
          begin
            if (FSrcBuf[0] <> 5) or (FSrcBuf[1] <> 0) then
            begin
              FLastError := 0;
              FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_SOCKS5CONN);
              Synchronize(OnConnectError);
              Exit;
            end;
            FSrcLen := 0;
            ProxyReady := True;
            Synchronize(OnConnect);
            if i < BufLen - 1 then begin
              Buffer := Ptr(LongWord(Buffer) + i);
              Synchronize(OnReceive); //Continue handling of remaining data
            end;
          end;
        end;
      end;
    end;
  end else
    inherited;
end;





{ THTTPSocket }

procedure THTTPSocket.OnConnect;
begin
  inherited;
  FLen := 0;
  FCurLen := 0;
end;

procedure THTTPSocket.OnReceive;
  function GetHTTPLength(List: TStringList): Integer;
  var
    i: Word;
  begin
    Result := 0;
    if List.Count < 1 then Exit;
    for i := 0 to List.Count - 1 do
      if Copy(List.Strings[i], 0, 16) = 'Content-Length: ' then
      begin
        Result := StrToInt(Copy(List.Strings[i], 16, $FF));
        Exit;
      end;
  end;

var
  i: LongWord;
  List: TStringList;
  S: String;
begin
  if not ProxyReady then begin
    if BufLen < 1 then Exit;
    for i := 0 to BufLen - 1 do
    begin
      FBuf[FCurLen] := PByte(LongWord(Buffer) + i)^;
      Inc(FCurLen);
      if FLen = 0 then
        if FCurLen > 3 then
          if Copy(PChar(@FBuf), FCurLen - 3, 4) = #13#10#13#10 then
          begin
            List := TStringList.Create;
            List.Text := PChar(@FBuf);
            S := GetHTTPStatus(List);
            FLen := GetHTTPLength(List);
            List.Free;
            if S <> '200 OK' then
            begin
              FLastError := 0;
              FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_HTTPSTAT) + S;
              Synchronize(OnError);
              Exit;
            end;
            if FLen + FCurLen > SizeOf(FBuf) then begin
              FLastError := 0;
              FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_HTTPBUF);
              Synchronize(OnError);
              Exit;
            end;
            FCurLen := 0;
            {$IFDEF PARSE}
            LogText('http.txt', 'Received (proxy): '#13#10 + PChar(@FBuf));
            {$ENDIF}
          end;
      if (FCurLen = FLen) then
      begin

        {We suppose that only one page can be received}
        ProxyReady := True;
        Buffer := @FBuf;
        BufLen := FCurLen;
        Synchronize(OnReceive);
        Exit;
      end;
    end;
  end else
    inherited;
end;





{ THTTPSSocket }

procedure THTTPSSocket.OnConnect;
begin
  inherited;
  if not ProxyReady then begin
    FCurLen := 0;
    if FProxyResolve then
      SendStr('CONNECT ' + FDestHost + ':' + IntToStr(FDestPort) + ' HTTP/1.0' + #13#10)
    else
      SendStr('CONNECT ' + WSockAddrToIp(FDestIp) + ':' + IntToStr(FDestPort) + ' HTTP/1.0' + #13#10);
    SendStr('User-Agent: Mozilla/4.08 [en] (WinNT; U ;Nav)' + #13#10);
    if FProxyAuth then
      SendStr('Proxy-Authorization: Basic ' + EncodeBase64(ProxyUser + ':' + ProxyPass) + #13#10);
    SendStr(#13#10);
  end;
end;

procedure THTTPSSocket.OnReceive;
var
  i: LongWord;
  List: TStringList;
  S: String;
begin
  if not ProxyReady then begin
    if BufLen < 1 then Exit;
    for i := 0 to BufLen - 1 do
    begin
      FBuf[FCurLen] := PByte(LongWord(Buffer) + i)^;
      Inc(FCurLen);
      if FCurLen > 3 then
        if Copy(PChar(@FBuf), FCurLen - 3, 4) = #13#10#13#10 then
        begin
          List := TStringList.Create;
          List.Text := PChar(@FBuf);
          S := GetHTTPStatus(List);
          List.Free;
          CharLowerBuff(@S[1], Length(S));
          if S <> '200 connection established' then
          begin
            FLastError := 0;
            FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_HTTPSTAT) + S;
            Synchronize(OnError);
            Exit;
          end;
          ProxyReady := True;
          {Handle remaining data}
          if i < BufLen - 1 then begin
            Buffer := Ptr(LongWord(Buffer) + i);
            BufLen := BufLen - i;
            Synchronize(OnReceive);
            Exit;
          end;
        end;
    end;
  end else
    inherited;
end;

{ TTCPServer }

constructor TTCPServer.Create;
begin
  inherited Create(True);
  FSocket := INVALID_SOCKET;
end;

destructor TTCPServer.Destroy;
begin
  FreeSocket;
  inherited;
end;

procedure TTCPServer.FreeSocket;
begin
  if not Terminated then Terminate;
  if FSocket <> INVALID_SOCKET then
    closesocket(FSocket);
end;

procedure TTCPServer.WaitForConnection;
Var
  FD,FDW,FDE:TFDSet;
Begin
  FD_ZERO(FD);
  FD_SET(fSocket, FD);
  FD_ZERO(FDW);
  FD_SET(fSocket, FDW);
  FD_ZERO(FDE);
  FD_SET(fSocket, FDE);
  select(fSocket + 1, @FD, @FDW, @FDE, nil); // Need to add a timeout maybe.
End;

procedure TTCPServer.Execute;
begin
  while not Terminated do begin
    //Should Wait For Connection here.
    WaitForConnection;
    FClient := accept(FSocket, nil, nil);
    if (FClient = INVALID_SOCKET) then begin
      if not Terminated then begin
        FLastError := WSAGetLastError;
        FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_ACCEPT);
        Synchronize(OnError);
      end;
      Exit;
    end else
      Synchronize(OnClientConnected);
  end;
end;

function TTCPServer.Start: Boolean;
var
  srv_addr: TSockAddrIn;
begin
  Result := False;
//  FreeSocket;
  FSocket := socket(PF_INET, SOCK_STREAM, 0);
  srv_addr.sin_family := AF_INET;
  srv_addr.sin_port := htons(FPort);
  srv_addr.sin_addr.S_addr := INADDR_ANY;
  if bind(FSocket, srv_addr, sizeof(srv_addr)) = SOCKET_ERROR then begin
    FLastError := WSAGetLastError;
    FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_BIND);
    Synchronize(OnError);
    Exit;
  end;
  if listen(FSocket, SOMAXCONN) = SOCKET_ERROR then begin
    FLastError := WSAGetLastError;
    FLastErrMsg := ICQLanguages[FErrLang].Translate(IMSG_ESOCK_LISTEN);
    Synchronize(OnError);
    Exit;
  end;
  Result := True;
  Self.Resume;
end;

procedure TTCPServer.OnError;
begin
  FreeSocket;
  if Assigned(_OnError) then
    FOnError(Self, ERR_SOCKET, FLastErrMsg);
end;

procedure TTCPServer.OnClientConnected;
begin
  if Assigned(_OnClientConnected) then
    FOnClientConnected(Self, FClient);
end;




///-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

function WSAErrorToStr(ErrorNo: Integer): String;
begin
  case ErrorNo of
    WSAEINTR:           Result := 'Interrupted system call';
    WSAEBADF:           Result := 'Bad file number';
    WSAEACCES:          Result := 'Permission denied';
    WSAEFAULT:          Result := 'Bad address';
    WSAEINVAL:          Result := 'Invalid argument';
    WSAEMFILE:          Result := 'Too many open files';
    WSAEWOULDBLOCK:     Result := 'Operation would block';
    WSAEINPROGRESS:     Result := 'Operation now in progress';
    WSAEALREADY:        Result := 'Operation already in progress';
    WSAENOTSOCK:        Result := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:    Result := 'Destination address required';
    WSAEMSGSIZE:        Result := 'Message too long';
    WSAEPROTOTYPE:      Result := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:     Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: Result := 'Socket type not supported';
    WSAEOPNOTSUPP:      Result := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:    Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT:    Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE:      Result := 'Address already in use';
    WSAEADDRNOTAVAIL:   Result := 'Can''t assign requested address';
    WSAENETDOWN:        Result := 'Network is down';
    WSAENETUNREACH:     Result := 'Network is unreachable';
    WSAENETRESET:       Result := 'Network dropped connection on reset';
    WSAECONNABORTED:    Result := 'Software caused connection abort';
    WSAECONNRESET:      Result := 'Connection reset by peer';
    WSAENOBUFS:         Result := 'No buffer space available';
    WSAEISCONN:         Result := 'Socket is already connected';
    WSAENOTCONN:        Result := 'Socket is not connected';
    WSAESHUTDOWN:       Result := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:    Result := 'Too many references: can''t splice';
    WSAETIMEDOUT:       Result := 'Connection timed out';
    WSAECONNREFUSED:    Result := 'Connection refused';
    WSAELOOP:           Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:    Result := 'File name too long';
    WSAEHOSTDOWN:       Result := 'Host is down';
    WSAEHOSTUNREACH:    Result := 'No route to host';
    WSAENOTEMPTY:       Result := 'Directory not empty';
    WSAEPROCLIM:        Result := 'Too many processes';
    WSAEUSERS:          Result := 'Too many users';
    WSAEDQUOT:          Result := 'Disc quota exceeded';
    WSAESTALE:          Result := 'Stale NFS file handle';
    WSAEREMOTE:         Result := 'Too many levels of remote in path';
    WSASYSNOTREADY:     Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED: Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:  Result := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:  Result := 'Host not found';
    WSATRY_AGAIN:       Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:     Result := 'Non-recoverable error';
    WSANO_DATA:         Result := 'No Data';
    else                Result := 'Not a WinSock error';
  end;
end;

function FindBindPort: Word;
var
  i: Word;
  srv_address: sockaddr_in;
  sock: TSocket;
begin
  Result := 0;
  sock := socket(AF_INET, SOCK_STREAM, 0);
  if sock = INVALID_SOCKET then
    Exit;
  srv_address.sin_family := AF_INET;
  srv_address.sin_addr.s_addr := INADDR_ANY;
  for i := gPortRange.First to gPortRange.Last do
  begin
    srv_address.sin_port := htons(i);
    if bind(sock, srv_address, SizeOf(srv_address)) <> SOCKET_ERROR then
    begin
      closesocket(sock);
      Result := i;
      Exit;
    end;
  end;
end;

function GetLocalIP: Integer;
type
  PaPInAddr = ^TaPInAddr;
  TaPInAddr = array[0..$FFFE] of PInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array[0..MAXGETHOSTSTRUCT - 1] of Char;
  I: Integer;
begin
  Result := -1;
  GetHostName(Buffer, SizeOf(Buffer));
  phe := GetHostByName(Buffer);
  if phe = nil then Exit;
  pptr := PaPInAddr(Phe^.h_addr_list);
  I := 0;
  while pptr^[I] <> nil do
  begin
    Result := pptr^[I]^.S_addr;
    Inc(I);
  end;
end;

function WSockAddrToIp(Value: LongWord): String;
var
  ia: in_addr;
begin
  ia.S_addr := Value;
  Result := inet_ntoa(ia);
end;

initialization
  {Use WinSock 1.1}
  if WSAStartup(MAKEWORD(1, 1), WSAData) <> 0 then begin
    {$IFDEF DEBUG}
    MessageBox(0, PChar('Could not start WSA'), 'Error!', MB_ICONERROR);
    {$ENDIF}
    WSAStarted := False;
  end else
    WSAStarted := True;

finalization
  if WSACleanUp <> 0 then begin
    {$IFDEF DEBUG}
    MessageBox(0, PChar('Could not cleanup WSA'), 'Error!', MB_ICONERROR);
    {$ENDIF}
  end;
  WSAStarted := False;
end.
