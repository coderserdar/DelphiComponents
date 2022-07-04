{$INCLUDE ..\cDefines.inc}
unit cSocketsTCPClient;

{                                                                              }
{                        TCP client socket class v3.05                         }
{                                                                              }
{       This unit is copyright © 2001-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{               Its original file name is cSocketsTCPClient.pas                }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   [ cSockets ]                                                               }
{   11/12/2001  0.01  Initial version of AClientSocket, TClientSocket.         }
{   13/12/2001  0.02  Added Socks5 support for TCP clients.                    }
{                     Added read/write throttling.                             }
{   04/02/2002  0.03  Added TProxyClientSocket.                                }
{   16/02/2002  0.04  Added GetReadRate, GetWriteRate.                         }
{   [ cSocketsTCPClient ]                                                      }
{   01/07/2002  3.05  Revised for Fundamentals 3.                              }
{                     Created cSocketsTCPClient unit from cSockets.            }
{                                                                              }

interface

uses
  { Delphi }
  WinSock,

  { Fundamentals }
  cSocketHostLookup,
  cSocketsTCP,
  cSocks;



{                                                                              }
{ TTCPClientSocket                                                             }
{   TClientSocket represents an initiated TCP socket connection.               }
{   Implements connection and proxy negotiation functionality.                 }
{                                                                              }
type
  { ATCPClientSocketProxy                                                      }
  TTCPClientSocket = class;
  ATCPClientSocketProxy = class
  protected
    FHost    : String;
    FPort    : String;
    FNext    : ATCPClientSocketProxy;
    FResolve : Boolean;
    FSocket  : TTCPClientSocket;

    procedure SetNext(const Next: ATCPClientSocketProxy);
    procedure GetNextHost (var Host, Port: String);

    procedure DoResolve(const LookupMethod: TSocketHostLookupMethod;
              const Host, Port: String);
    procedure SetResolved(const Address: TInAddr; const Port: Word); virtual; abstract;
    procedure SetResolveFailed(const ErrorCode: Integer; const Msg: String); virtual;
    procedure TriggerDataAvailable; virtual; abstract;
    procedure DoNegotiate; virtual; abstract;

    procedure SetNegotiated;
    procedure SetFailed(const ErrorCode: Integer; const Msg: String);

  public
    constructor Create(const Host, Port: String;
                const ResolveThroughProxy: Boolean = True;
                const Next: ATCPClientSocketProxy = nil);
    destructor Destroy; override;

    procedure Negotiate(const Socket: TTCPClientSocket);
    property  Host: String read FHost write FHost;
    property  Port: String read FPort write FPort;
    property  Next: ATCPClientSocketProxy read FNext write SetNext;
    property  ResolveThroughProxy: Boolean read FResolve write FResolve;
  end;

  { TTCPClientSocket                                                           }
  TTCPClientSocketEvent = procedure (Sender: TTCPClientSocket) of object;
  TTCPClientSocket = class(ATCPClientSocket)
  protected
    FHost             : String;
    FPort             : String;
    FProxy            : ATCPClientSocketProxy;

    FResolvedPort     : Word;
    FConnectAddr      : TSockAddrIn;
    FNegotiatingProxy : ATCPClientSocketProxy;

    FOnResolved       : TTCPClientSocketEvent;
    FOnNegotiating    : TTCPClientSocketEvent;
    FOnConnected      : TTCPClientSocketEvent;
    FOnConnectFailed  : TTCPClientSocketEvent;

    procedure SetHost(const Host: String);
    procedure SetPort(const Port: String);
    function  GetPortAsInteger: Integer;
    procedure SetPortAsInteger(const Port: Integer);
    procedure SetProxy(const Proxy: ATCPClientSocketProxy);

    procedure DoResolve(const LookupMethod: TSocketHostLookupMethod;
              const Host, Port: String); virtual;
    procedure ActLookupComplete(const ErrorCode: Integer;
              const Host: String; const Addr: TInAddr); override;
    procedure SetResolved(const Address: TInAddr; const Port: Word); virtual;
    procedure SetResolveFailed(const ErrorCode: Integer; const Msg: String); virtual;
    procedure TriggerResolved; virtual;
    procedure DoConnect;
    procedure TCPConnect;

    procedure RaiseError(const Msg: String; const ErrorCode: Integer); override;

    procedure WMSocket(const Events, lWordHi: Word); override;
    procedure TriggerDataAvailable; override;
    procedure TriggerConnected; virtual;
    procedure TriggerConnectFailed; virtual;
    procedure TriggerNegotiating; virtual;
    procedure SetConnected; virtual;
    procedure SetNegotiated(const Proxy: ATCPClientSocketProxy);
    procedure SetConnectFailed(const ErrorCode: Integer; const Msg: String); virtual;
    procedure StartNegotiating;
    procedure HandleConnectEvent(const Error: Word);

  public
    constructor Create(const Host, Port: String;
                const Proxy: ATCPClientSocketProxy = nil); reintroduce; overload;
    destructor Destroy; override;

    property  Host: String read FHost write SetHost;
    property  Port: String read FPort write SetPort;
    property  PortInt: Integer read GetPortAsInteger write SetPortAsInteger;
    property  Proxy: ATCPClientSocketProxy read FProxy write SetProxy;

    function  GetRemoteAddress: TInAddr; override;

    procedure Connect;
    function  Connecting: Boolean;
    function  Negotiating: Boolean;
    property  NegotiatingProxy: ATCPClientSocketProxy read FNegotiatingProxy;

    property  OnResolved: TTCPClientSocketEvent read FOnResolved write FOnResolved;
    property  OnNegotiating: TTCPClientSocketEvent read FOnNegotiating write FOnNegotiating;
    property  OnConnected: TTCPClientSocketEvent read FOnConnected write FOnConnected;
    property  OnConnectFailed: TTCPClientSocketEvent read FOnConnectFailed write FOnConnectFailed;
  end;



{                                                                              }
{ ATCPClientSocketProxyWithAuth                                                }
{                                                                              }
type
  ATCPClientSocketProxyWithAuth = class(ATCPClientSocketProxy)
  protected
    FUseAuthentication : Boolean;
    FUsername          : String;
    FPassword          : String;

  public
    constructor Create(const Host, Port: String; const ResolveThroughProxy: Boolean = True;
                const UseAuthentication: Boolean = False;
                const Username: String = ''; const Password: String = '';
                const Next: ATCPClientSocketProxy = nil);

    property  UseAuthentication: Boolean read FUseAuthentication write FUseAuthentication;
    property  Username: String read FUsername write FUsername;
    property  Password: String read FPassword write FPassword;
  end;



{                                                                              }
{ THTTPTunnelSocketProxy                                                       }
{                                                                              }
type
  THTTPTunnelSocketProxy = class(ATCPClientSocketProxyWithAuth)
  protected
    procedure SetResolved(const Address: TInAddr; const Port: Word); override;
    procedure TriggerDataAvailable; override;
    procedure DoNegotiate; override;
    procedure DoRequest(const Host, Port: String);
  end;



{                                                                              }
{ TSocks5ConnectionProxy                                                       }
{                                                                              }
type
  TSocks5ClientState = (ssSocksGreeting, ssSocksAuthenticating,
                        ssSocksRequested, ssSocksResponseData,
                        ssSocksNegotiated);
  TSocks5SocketProxy = class(ATCPClientSocketProxyWithAuth)
  protected
    FDestHost     : String;
    FDestPort     : String;
    FResolvedAddr : TInAddr;
    FResolvedPort : Word;

    FState        : TSocks5ClientState;
    FResponse     : Array[1..SOCKS5_MAX_MSG_SIZE] of Byte;
    FResponseLen  : Integer;

    procedure SetResolved(const Address: TInAddr; const Port: Word); override;
    procedure TriggerDataAvailable; override;
    procedure DoNegotiate; override;

    procedure DoRequest;
    procedure DoRequestConnect;

  public
    property  State: TSocks5ClientState read FState;
  end;



implementation

uses
  { Delphi }
  Windows,
  SysUtils,

  { Fundamentals }
  cUtils,
  cStrings,
  cDateTime,
  cLinkedLists,
  cWinSock,
  cSockets;



{                                                                              }
{ TTCPClientSocket                                                             }
{                                                                              }
constructor TTCPClientSocket.Create(const Host, Port: String;
    const Proxy: ATCPClientSocketProxy);
begin
  inherited Create;
  SetHost(Host);
  SetPort(Port);
  SetProxy(Proxy);
end;

destructor TTCPClientSocket.Destroy;
begin
  FreeAndNil(FProxy);
  inherited Destroy;
end;

procedure TTCPClientSocket.RaiseError(const Msg: String; const ErrorCode: Integer);
var S : String;
begin
  S := Msg;
  if (FHost <> '') and (FPort <> '') then
    S := '(Host ' + FHost + ':' + FPort + ') ' + S;
  inherited RaiseError(S, ErrorCode);
end;

function TTCPClientSocket.Negotiating: Boolean;
begin
  Result := FState = ssNegotiating;
end;

function TTCPClientSocket.Connecting: Boolean;
begin
  Result := FState in [ssResolving, ssResolved, ssConnecting, ssNegotiating];
end;

function TTCPClientSocket.GetPortAsInteger: Integer;
begin
  Result := StrToIntDef(FPort, 0);
end;

procedure TTCPClientSocket.SetPort(const Port: String);
begin
  if Port = FPort then
    exit;
  CheckStateClosed('SetPort');
  FPort := Port;
end;

procedure TTCPClientSocket.SetPortAsInteger(const Port: Integer);
begin
  SetPort(IntToStr(Port));
end;

procedure TTCPClientSocket.SetHost(const Host: String);
begin
  if Host = FHost then
    exit;
  CheckStateClosed('SetHost');
  FHost := Host;
end;

procedure TTCPClientSocket.SetProxy(const Proxy: ATCPClientSocketProxy);
begin
  if Proxy = FProxy then
    exit;
  CheckStateClosed('SetProxy');
  FreeAndNil(FProxy);
  FProxy := Proxy;
end;

function TTCPClientSocket.GetRemoteAddress: TInAddr;
begin
  if State = ssConnected then
    Result := FConnectAddr.sin_addr else
    Result := inherited GetRemoteAddress;
end;

procedure TTCPClientSocket.TriggerResolved;
begin
  if Assigned(FOnResolved) then
    FOnResolved(self);
end;

procedure TTCPClientSocket.TriggerConnectFailed;
begin
  if Assigned(FOnConnectFailed) then
    FOnConnectFailed(self);
end;

procedure TTCPClientSocket.TriggerConnected;
begin
  if Assigned(FOnConnected) then
    FOnConnected(self);
end;

procedure TTCPClientSocket.TriggerNegotiating;
begin
  if Assigned(FOnNegotiating) then
    FOnNegotiating(self);
end;

procedure TTCPClientSocket.ActLookupComplete(const ErrorCode: Integer;
    const Host: String; const Addr: TInAddr);
begin
  if ErrorCode = 0 then
    SetResolved(Addr, FResolvedPort) else
    SetResolveFailed(ErrorCode, '');
end;

procedure TTCPClientSocket.WMSocket(const Events, lWordHi: Word);
begin
  inherited WMSocket(Events, lWordHi);
  if Events and FD_CONNECT <> 0 then
    HandleConnectEvent(lWordHi);
end;

procedure TTCPClientSocket.HandleConnectEvent(const Error: Word);
begin
  if Error = 0 then
    begin
      if Assigned(FProxy) then
        StartNegotiating else
        SetConnected;
    end else
    SetConnectFailed(Error, '');
end;

procedure TTCPClientSocket.TriggerDataAvailable;
begin
  if (FState = ssNegotiating) and Assigned(FNegotiatingProxy) then
    FNegotiatingProxy.TriggerDataAvailable;
  inherited TriggerDataAvailable;
end;

procedure TTCPClientSocket.DoResolve(const LookupMethod: TSocketHostLookupMethod;
  const Host, Port: String);
begin
  ResolvePort(Port, FProtocol, FResolvedPort);
  DoLookup(Host, LookupMethod);
end;

procedure TTCPClientSocket.TCPConnect;
var Err : Integer;
begin
  if (FLocalHost <> '') or (FLocalPort <> '') then
    BindLocalAddress;
  Err := WinSock.Connect(FSocketHandle, FConnectAddr, Sizeof(TSockAddrIn));
  if Err = 0 then
    exit;
  Err := WSAGetLastError;
  if Err = WSAEWOULDBLOCK then
    exit;
  RaiseWinSockError('Connect failed', Err);
end;

procedure TTCPClientSocket.DoConnect;
begin
  SetState(ssConnecting);
  SetSocketHandle(AllocateSocketHandle(FProtocol));
  SetSocketAsynchronous;
  TCPConnect;
end;

procedure TTCPClientSocket.SetResolved(const Address: TInAddr; const Port: Word);
begin
  if FState = ssResolving then
    begin
      FillChar(FConnectAddr, Sizeof(TSockAddrIn), #0);
      FConnectAddr.sin_family := AF_INET;
      FConnectAddr.sin_addr := Address;
      FConnectAddr.sin_port := Port;
      SetState(ssResolved);
      TriggerResolved;
      DoConnect;
    end else
  if (FState = ssNegotiating) and Assigned(FNegotiatingProxy) then
    FNegotiatingProxy.SetResolved(Address, Port);
end;

procedure TTCPClientSocket.SetResolveFailed(const ErrorCode: Integer;
    const Msg: String);
begin
  if FState = ssResolving then
    SetConnectFailed(ErrorCode, Msg) else
  if (FState = ssNegotiating) and Assigned(FNegotiatingProxy) then
    FNegotiatingProxy.SetResolveFailed(ErrorCode, Msg);
end;

procedure TTCPClientSocket.SetConnectFailed(const ErrorCode: Integer;
    const Msg: String);
begin
  SetError(ErrorCode, Msg);
  SetState(ssClosed);
  TriggerConnectFailed;
end;

procedure TTCPClientSocket.SetConnected;
begin
  FReadTimerStart := GetTickCount;
  FReadTimerCount := 0;
  SetState(ssConnected);
  RefreshThrottleTimer;
  TriggerConnected;
end;

procedure TTCPClientSocket.StartNegotiating;
begin
  FNegotiatingProxy := FProxy;
  SetState(ssNegotiating);
  TriggerNegotiating;
  FProxy.Negotiate(self);
end;

procedure TTCPClientSocket.SetNegotiated(const Proxy: ATCPClientSocketProxy);
begin
  if (FState <> ssNegotiating) or (Proxy <> FNegotiatingProxy) then
    exit;
  FNegotiatingProxy := Proxy.Next;
  if Assigned(FNegotiatingProxy) then
    begin
      TriggerNegotiating;
      FNegotiatingProxy.Negotiate(self);
    end else
    SetConnected;
end;

procedure TTCPClientSocket.Connect;
begin
  CheckStateClosed('Connect');
  if FPort = '' then
    RaiseError('Port not specified', -1);
  if FHost = '' then
    RaiseError('Host not specified', -1);
  ClearError;
  SetState(ssResolving);
  if Assigned(FProxy) then
    DoResolve(lmAsync, FProxy.Host, FProxy.Port) else
    DoResolve(lmAsync, FHost, FPort);
end;



{                                                                              }
{ ATCPClientSocketProxy                                                        }
{                                                                              }
constructor ATCPClientSocketProxy.Create(const Host, Port: String;
    const ResolveThroughProxy: Boolean; const Next: ATCPClientSocketProxy);
begin
  inherited Create;
  FHost := Host;
  FPort := Port;
  FResolve := ResolveThroughProxy;
  FNext := Next;
end;

destructor ATCPClientSocketProxy.Destroy;
begin
  FreeAndNil(FNext);
  inherited Destroy;
end;

procedure ATCPClientSocketProxy.SetNext(const Next: ATCPClientSocketProxy);
begin
  if Next = FNext then
    exit;
  FreeAndNil(FNext);
  FNext := Next;
end;

procedure ATCPClientSocketProxy.GetNextHost(var Host, Port: String);
begin
  if Assigned(FNext) then
    begin
      Host := FNext.Host;
      Port := FNext.Port;
    end else
    begin
      Host := FSocket.Host;
      Port := FSocket.Port;
    end;
end;

procedure ATCPClientSocketProxy.DoResolve(const LookupMethod: TSocketHostLookupMethod;
  const Host, Port: String);
begin
  FSocket.DoResolve(LookupMethod, Host, Port);
end;

procedure ATCPClientSocketProxy.Negotiate(const Socket: TTCPClientSocket);
begin
  Assert(Assigned(Socket), 'Assigned(Socket)');
  FSocket := Socket;
  DoNegotiate;
end;

procedure ATCPClientSocketProxy.SetNegotiated;
begin
  FSocket.SetNegotiated(self);
end;

procedure ATCPClientSocketProxy.SetFailed(const ErrorCode: Integer; const Msg: String);
begin
  FSocket.SetConnectFailed(ErrorCode, Msg);
end;

procedure ATCPClientSocketProxy.SetResolveFailed(const ErrorCode: Integer; const Msg: String);
begin
  SetFailed(ErrorCode, Msg);
end;



{                                                                              }
{ ATCPClientSocketProxyWithAuth                                                }
{                                                                              }
constructor ATCPClientSocketProxyWithAuth.Create(const Host, Port: String;
    const ResolveThroughProxy: Boolean; const UseAuthentication: Boolean;
    const Username: String; const Password: String; const Next: ATCPClientSocketProxy);
begin
  inherited Create(Host, Port, ResolveThroughProxy);
  FUseAuthentication := UseAuthentication;
  FUsername := Username;
  FPassword := Password;
  FNext := Next;
end;



{                                                                              }
{ THTTPTunnelSocketProxy                                                       }
{                                                                              }
procedure THTTPTunnelSocketProxy.DoNegotiate;
var H, P : String;
begin
  GetNextHost(H, P);
  if FResolve then
    DoRequest(H, P) else
    DoResolve(lmAsync, H, P);
end;

procedure THTTPTunnelSocketProxy.SetResolved(const Address: TInAddr; const Port: Word);
begin
  DoRequest(IPAddressStr(Address), NetPortToPortStr(Port));
end;

procedure THTTPTunnelSocketProxy.DoRequest(const Host, Port: String);
var Request : String;
begin
  Request := 'CONNECT ' + Host + ':' + Port + ' HTTP/1.1' + CRLF +
             'Host: ' + Host + CRLF +
             'Date: ' + NowAsRFCDateTime + CRLF;
  if FUseAuthentication and ((FUsername <> '') or (FPassword <> '')) then
    Request := Request + 'Proxy-Authorization: Basic ' +
               MIMEBase64Encode(FUsername + ':' + FPassword) + CRLF;
  Request := Request + CRLF;
  FSocket.SendStr(Request);
end;

procedure THTTPTunnelSocketProxy.TriggerDataAvailable;
var S, C, M : String;
    I, L, E : Integer;
begin
  S := FSocket.PeekAvailable;
  L := Length(S);
  if L = 0 then
    exit;
  if ((L >= 1) and (UpCase(S[1]) <> 'H')) or
     ((L >= 2) and (UpCase(S[2]) <> 'T')) or
     ((L >= 3) and (UpCase(S[3]) <> 'T')) or
     ((L >= 4) and (UpCase(S[4]) <> 'P')) or
     ((L >= 5) and (UpCase(S[5]) <> '/')) then
    SetFailed(HTTPTunnelErrorInvalidResponse, '');
  I := PosStr(CRLF + CRLF, S);
  if I = 0 then
    exit;
  S := StrAfter(StrBefore(S, CRLF), ' ');
  StrSplitAtChar(S, ' ', C, M);
  E := StrToIntDef(C, -1);
  if E = -1 then
    SetFailed(HTTPTunnelErrorInvalidResponse, '');
  if E div 100 = 2 then // Success
    begin
      FSocket.Skip(I + 4);
      SetNegotiated;
    end else
    SetFailed(HTTPTunnelErrorTunnelFailed, 'HTTP Error ' + IntToStr(E) + ': ' + M);
end;



{                                                                              }
{ TSocks5SocketProxy                                                           }
{                                                                              }
procedure TSocks5SocketProxy.DoNegotiate;
begin
  GetNextHost(FDestHost, FDestPort);
  if FResolve then
    DoRequest else
    DoResolve(lmAsync, FDestHost, FDestPort);
end;

procedure TSocks5SocketProxy.SetResolved(const Address: TInAddr; const Port: Word);
begin
  FResolvedAddr := Address;
  FResolvedPort := Port;
  DoRequest;
end;

procedure TSocks5SocketProxy.DoRequest;
var Greeting : TSocks5Greeting;
begin
  FState := ssSocksGreeting;
  PopulateSocks5Greeting(Greeting, FUseAuthentication);
  FSocket.Send(Greeting, Sizeof(TSocks5Greeting));
end;

procedure TSocks5SocketProxy.DoRequestConnect;
var IP4Req   : TSocks5IP4Message;
    Resolved : Boolean;
begin
  if FResolve then
    begin
      ResolvePort(FDestPort, spTCP, FResolvedPort);
      Resolved := IsIPAddress(FDestHost, FResolvedAddr)
    end else
    Resolved := True;
  FState := ssSocksRequested;
  if Resolved then
    begin
      PopulateSocks5IP4Message(IP4Req, SOCKS5_REQ_CODE_CONNECT,
          FResolvedAddr, FResolvedPort);
      FSocket.Send(IP4Req, Sizeof(TSocks5IP4Message));
    end else
    FSocket.SendStr(Socks5DomainRequest(SOCKS5_REQ_CODE_CONNECT,
        FDestHost, FResolvedPort));
end;

procedure TSocks5SocketProxy.TriggerDataAvailable;
var GreetResponse    : TSocks5GreetingResponse;
    UserPassResponse : TSocks5UserPassResponse;
    P                : PChar;
begin
  Case FState of
    ssSocksGreeting :
      if FSocket.InBufferSize < Sizeof(GreetResponse) then
        exit else
        begin
          FSocket.Read(GreetResponse, Sizeof(GreetResponse));
          if (GreetResponse.Version <> 5) or
             not (GreetResponse.Method in [SOCKS5_METHOD_NOAUTH, SOCKS5_METHOD_USERPASS]) then
            begin
              SetFailed(SocksErrorNegotiationFailed, 'Socks negotiation failed');
              exit;
            end;
          if GreetResponse.Method = SOCKS5_METHOD_NOAUTH then
            DoRequestConnect else
            begin
              FState := ssSocksAuthenticating;
              FSocket.SendStr(Socks5UserPassMessage(Username, Password));
            end;
        end;
    ssSocksAuthenticating :
      if FSocket.InBufferSize < Sizeof(UserPassResponse) then
        exit else
        begin
          FSocket.Read(UserPassResponse, Sizeof(UserPassResponse));
          if UserPassResponse.Status <> 0 then
            begin
              SetFailed(SocksErrorAuthenticationFailed, 'Socks authentication failed');
              exit;
            end;
          DoRequestConnect;
        end;
    ssSocksRequested :
      if FSocket.InBufferSize < Sizeof(TSocks5ResponseHeader) then
        exit else
        begin
          FSocket.Read(FResponse, Sizeof(TSocks5ResponseHeader));
          FResponseLen := Socks5ResponseSize(PSocks5ResponseHeader(@FResponse));
          if FResponseLen > SOCKS5_MAX_MSG_SIZE then
            begin
              SetFailed(SocksErrorInvalidResponse, 'Invalid response from Socks server');
              exit;
            end;
          FState := ssSocksResponseData;
        end;
  end;
  Case FState of
    ssSocksResponseData :
      if FSocket.InBufferSize < (FResponseLen - Sizeof(TSocks5ResponseHeader)) then
        exit else
        begin
          P := @FResponse;
          Inc(P, Sizeof(TSocks5ResponseHeader));
          FSocket.Read(P^, FResponseLen - Sizeof(TSocks5ResponseHeader));
          if PSocks5ResponseHeader(@FResponse)^.Code = SOCKS5_RESP_CODE_Success then
            begin
              FState := ssSocksNegotiated;
              SetNegotiated;
            end else
            SetFailed(SocksErrorBase + PSocks5ResponseHeader(@FResponse)^.Code, '');
        end;
  end;
end;



end.

