{$INCLUDE ..\cDefines.inc}
unit cSockets;

interface

uses
  { Delphi }
  SysUtils,
  Windows,
  Messages,
  WinSock,
  Classes,

  { Fundamentals }
  cWindows,
  cLinkedLists,
  cWinSock,
  cSocketHostLookup;



{                                                                              }
{                             Socket class v3.15                               }
{                                                                              }
{       This unit is copyright © 2001-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cSockets.pas                    }
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
{   15/01/2001  0.01  Moved TSocketStream from cStreams into cSockets.         }
{   17/01/2001  0.02  Bug fixes.                                               }
{   25/01/2001  0.03  Refactored.                                              }
{   09/04/2001  0.04  Removed dependancies on Delphi's sockets.                }
{   05/12/2001  0.05  Added custom TWSocketX class.                            }
{   10/12/2001  0.06  Removed dependancies on ICS.                             }
{                     Added ASocket class.                                     }
{   11/12/2001  0.07  Added AClientSocket, TClientSocket.                      }
{                     Spawned cSocketHostLookup and cWinSock from cSockets.    }
{   13/12/2001  0.08  Added Socks5 support for TCP clients.                    }
{                     Added read/write throttling.                             }
{   15/12/2001  0.09  Added TSocketStream, TTCPServerSocket.                   }
{   20/12/2001  0.10  Revision.                                                }
{   04/02/2002  0.11  Added TProxyClientSocket.                                }
{   05/02/2002  0.12  Added TUDPSocket.                                        }
{   16/02/2002  0.13  Added GetReadRate, GetWriteRate.                         }
{   01/07/2002  3.14  Revised for Fundamentals 3.                              }
{                     Created cTCPStream, cSocketsTCP, cSocketsUDP units       }
{                     from cSockets.                                           }
{   25/03/2003  3.15  Cancel lookups when socket is terminated.                }
{                                                                              }
const
  UnitName    = 'cSockets';
  UnitVersion = '3.15';
  UnitDesc    = 'Sockets';



{                                                                              }
{ ASocket                                                                      }
{   Base class for WinSock socket implementations.                             }
{                                                                              }
{   Derived classes must implement WMSocket to respond to socket messages,     }
{   implement GetAsynchronousEvents to return applicable events and            }
{   implement ActLookupComplete if DoLookup is called.                         }
{                                                                              }
const
  WM_SOCKET = WM_USER + 552;

type
  TSocketState = (ssClosed, ssResolving, ssResolved, ssConnecting,
                  ssNegotiating, ssConnected, ssListening);
  ASocket = class;
  TSocketEvent = procedure (Sender: ASocket) of object;
  TSocketStateChangeEvent = procedure (Sender: ASocket;
      OldState, State: TSocketState) of object;
  ASocket = class(TTimerHandle)
  protected
    FProtocol      : TSocketProtocol;
    FState         : TSocketState;
    FSocketHandle  : TSocket;
    FOnStateChange : TSocketStateChangeEvent;
    FOnError       : TSocketEvent;
    FErrorCode     : Integer;
    FErrorMessage  : String;
    FLocalHost     : String;
    FLocalPort     : String;
    FLookups       : TSocketHostLookupArray;

    procedure Init; overload; virtual;
    procedure Init(const SocketHandle: TSocket); overload; virtual;
    procedure Init(const Protocol: TSocketProtocol); overload; virtual;
    procedure Clear; virtual;

    procedure WMSocket(const Events, lWordHi: Word); virtual; abstract;
    function  HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer; override;

    procedure SetError(const ErrorCode: Integer; const ErrorMessage: String);
    function  GetErrorMessage: String;
    procedure ClearError;
    procedure RaiseError(const Msg: String; const ErrorCode: Integer); virtual;
    procedure RaiseWinSockError(const Msg: String; const ErrorCode: Integer);
    procedure RaiseLastWinSockError(const Msg: String);
    procedure CheckStateClosed(const Operation: String);
    function  IsSocketConnected: Boolean; virtual;
    procedure CheckSocketConnected;

    procedure SetState(const State: TSocketState);
    procedure TriggerStateChange(const OldState, State: TSocketState); virtual;

    procedure SetProtocol(const Protocol: TSocketProtocol); virtual;
    function  SocketGetLocalAddr: TSockAddrIn;
    procedure SetLocalHost(const LocalHost: String); virtual;
    procedure SetLocalPort(const LocalPort: String); virtual;
    function  GetLocalPort: String; virtual;

    function  GetSocketHandle: TSocket;
    procedure SetSocketHandle(const SocketHandle: TSocket);
    procedure DestroySocketHandle;

    function  GetAsynchronousEvents: LongInt; virtual; abstract;
    procedure SetSocketAsynchronous;
    procedure SetSocketBlocking;

    procedure SocketBind(const Address: TSockAddrIn);
    procedure BindLocalAddress;

    procedure DoLookup(const Host: String; const LookupMethod: TSocketHostLookupMethod);
    procedure OnHostLookupComplete(Sender: TSocketHostLookup); virtual;
    procedure ActLookupComplete(const ErrorCode: Integer; const Host: String;
              const Addr: TInAddr); virtual;

  public
    constructor Create(AOwner: TComponent); reintroduce; overload; override;
    constructor Create; reintroduce; overload;
    constructor Create(const SocketHandle: TSocket); reintroduce; overload;
    constructor Create(const Protocol: TSocketProtocol); reintroduce; overload;
    destructor Destroy; override;

    procedure Terminate; override;

    property  Protocol: TSocketProtocol read FProtocol write SetProtocol;
    property  LocalHost: String read FLocalHost write SetLocalHost;
    property  LocalPort: String read GetLocalPort write SetLocalPort;

    function  LocalHostName: String;

    property  State: TSocketState read FState;
    property  OnStateChange: TSocketStateChangeEvent read FOnStateChange write FOnStateChange;

    property  SocketHandle: TSocket read GetSocketHandle write SetSocketHandle;
    function  ReleaseSocket: TSocket;

    property  ErrorCode: Integer read FErrorCode;
    property  ErrorMessage: String read GetErrorMessage;
    property  OnError: TSocketEvent read FOnError write FOnError;
  end;
  ASocketArray = Array of ASocket;



{                                                                              }
{ ASocketExSendBuffer                                                          }
{   Base class for socket classes that require a send buffer.                  }
{   Also provides functionality to throttle the rate at which data is sent.    }
{   WriteThrottleRate is in bytes per second.                                  }
{                                                                              }
{   Derived classes must implement SendBufferedData, add items to FOutBuffer   }
{   when appropriate, call ActSocketSent when data was actually sent and use   }
{   GetThrottledWriteSize to find out how much data may be sent.               }
{                                                                              }
type
  ASocketExSendBuffer = class;
  ASocketExSendBufferEvent = procedure (const Sender: ASocketExSendBuffer) of object;
  ASocketExSendBuffer = class(ASocket)
  protected
    FOutBuffer         : TDoublyLinkedList;
    FReadyToSend       : Boolean;
    FOnSendBufferEmpty : ASocketExSendBufferEvent;
    FUseSendBuffer     : Boolean;

    FWriteTimerStart   : Integer;
    FWriteTimerCount   : Integer;
    FWriteRate         : Integer;

    FThrottleWrite     : Boolean;
    FWriteThrottleRate : Integer;

    procedure Init; override;
    procedure WMSocket(const Events, lWordHi: Word); override;
    procedure HandleWriteEvent; virtual;

    function  ThrottleTimerActive: Boolean; virtual;
    procedure SetThrottleWrite(const ThrottleWrite: Boolean); virtual;
    procedure TriggerSendBufferEmpty; virtual;

    procedure RefreshThrottleTimer;
    procedure TriggerTimer; override;
    function  GetThrottledWriteSize(const Size: Integer): Integer; virtual;
    procedure ActSocketSent(const SendResult, Size: Integer; var Error, BytesSent: Integer);
    procedure SendBufferedData; virtual; abstract;

  public
    destructor Destroy; override;

    function  GetWriteRate: Integer;
    property  ThrottleWrite: Boolean read FThrottleWrite write SetThrottleWrite;
    property  WriteThrottleRate: Integer read FWriteThrottleRate write FWriteThrottleRate;

    property  UseSendBuffer: Boolean read FUseSendBuffer write FUseSendBuffer default True;
    property  OnSendBufferEmpty: ASocketExSendBufferEvent read FOnSendBufferEmpty write FOnSendBufferEmpty;
    function  IsSendBufferEmpty: Boolean;
    function  IsSendFlushed: Boolean;
    procedure ClearSendBuffer;
  end;

procedure AdvanceTimer(var StartTick, Count: Integer; const TickCount: Integer;
          var Elapsed, TransferRate: Integer);
function  ThrottledSize(var StartTick, Count: Integer;
          const Size, TickCount, ThrottleRate: Integer;
          var TransferRate: Integer): Integer;



{                                                                              }
{ TSocketList                                                                  }
{   Thread-safe container for ASocket classes.                                 }
{                                                                              }
type
  TSocketList = class
  protected
    FLock : TRTLCriticalSection;
    FList : ASocketArray;

    function  GetItem(const Idx: Integer): ASocket;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    procedure Add(const Socket: ASocket);
    procedure Remove(const Socket: ASocket);
    function  Count: Integer;
    property  Item[const Idx: Integer]: ASocket read GetItem;
  end;

{ List of all sockets in application }
var
  GlobalSocketList: TSocketList = nil;



{                                                                              }
{ Error Codes                                                                  }
{                                                                              }
const
  WinSockErrorBase                  = WSABASEERR;

  SocketErrorBase                   = WinSockErrorBase + 10000;
  SocketErrorSocketNotClosed        = SocketErrorBase + 1;
  SocketErrorSocketNotConnected     = SocketErrorBase + 2;
  SocketErrorCanNotSetHandle        = SocketErrorBase + 3;
  SocketErrorCanNotSetProtocol      = SocketErrorBase + 4;
  SocketErrorHostNotResolved        = SocketErrorBase + 5;

  SocketProxyErrorBase              = WinSockErrorBase + 20000;
  SocketProxyErrorProxyRequired     = SocketProxyErrorBase + 1;

  SocksErrorBase                    = WinSockErrorBase + 30000;
  SocksErrorGeneralServerFailure    = SocksErrorBase + 1;
  SocksErrorConnectionNotAllowed    = SocksErrorBase + 2;
  SocksErrorNetworkUnreachable      = SocksErrorBase + 3;
  SocksErrorHostUnreachable         = SocksErrorBase + 4;
  SocksErrorConnectionRefused       = SocksErrorBase + 5;
  SocksErrorTTLExpired              = SocksErrorBase + 6;
  SocksErrorCommandNotSupported     = SocksErrorBase + 7;
  SocksErrorAddressTypeNotSupported = SocksErrorBase + 8;
  SocksErrorNegotiationFailed       = SocksErrorBase + 101;
  SocksErrorAuthenticationFailed    = SocksErrorBase + 102;
  SocksErrorInvalidResponse         = SocksErrorBase + 103;

  HTTPTunnelErrorBase               = WinSockErrorBase + 40000;
  HTTPTunnelErrorInvalidResponse    = HTTPTunnelErrorBase + 1;
  HTTPTunnelErrorTunnelFailed       = HTTPTunnelErrorBase + 2;

  SocketGeneralErrorBase            = WinSockErrorBase + 50000;
  SocketGeneralSystemError          = SocketGeneralErrorBase + 1;
  SocketGeneralResourceError        = SocketGeneralErrorBase + 2;
  SocketGeneralApplicationError     = SocketGeneralErrorBase + 3;
  SocketGeneralClientError          = SocketGeneralErrorBase + 4;
  SocketGeneralServerError          = SocketGeneralErrorBase + 5;
  SocketGeneralProxyError           = SocketGeneralErrorBase + 6;
  SocketGeneralProtocolError        = SocketGeneralErrorBase + 7;
  SocketGeneralTimeOutError         = SocketGeneralErrorBase + 8;
  SocketGeneralConnectError         = SocketGeneralErrorBase + 9;
  SocketGeneralAbortError           = SocketGeneralErrorBase + 10;

  UserSocketErrorBase               = WinSockErrorBase + 60000;



implementation

uses
  { Fundamentals }
  cUtils;



{                                                                              }
{ Implementation constants                                                     }
{                                                                              }
const
  ThrottleInterval = 50;



{                                                                              }
{ Error Codes                                                                  }
{                                                                              }
function SocketErrorStr(const ErrorCode: Integer): String;
begin
  Case ErrorCode of
    SocketErrorSocketNotClosed        : Result := 'Operation only allowed on closed socket';
    SocketErrorSocketNotConnected     : Result := 'Socket not connected';
    SocketErrorCanNotSetHandle        : Result := 'Can not set socket handle';
    SocketErrorCanNotSetProtocol      : Result := 'Can not set protocol';
    SocketErrorHostNotResolved        : Result := 'Host not resolved';
    SocketProxyErrorProxyRequired     : Result := 'Proxy required';
    SocksErrorNegotiationFailed       : Result := 'Socks negotiation failed';
    SocksErrorAuthenticationFailed    : Result := 'Socks authentication failed';
    SocksErrorGeneralServerFailure    : Result := 'General socks server failure';
    SocksErrorConnectionNotAllowed    : Result := 'Connection not allowed by rule set';
    SocksErrorNetworkUnreachable      : Result := 'Network unreachable';
    SocksErrorHostUnreachable         : Result := 'Host unreachable';
    SocksErrorConnectionRefused       : Result := 'Connection refused';
    SocksErrorTTLExpired              : Result := 'Socks error: TTL expired';
    SocksErrorCommandNotSupported     : Result := 'Socks command not supported';
    SocksErrorAddressTypeNotSupported : Result := 'Address type not supported';
    HTTPTunnelErrorInvalidResponse    : Result := 'Invalid HTTP response';
    HTTPTunnelErrorTunnelFailed       : Result := 'HTTP tunnel failed';
    SocketGeneralSystemError          : Result := 'System error';
    SocketGeneralResourceError        : Result := 'Resource error';
    SocketGeneralApplicationError     : Result := 'Application error';
    SocketGeneralClientError          : Result := 'Client error';
    SocketGeneralServerError          : Result := 'Server error';
    SocketGeneralProxyError           : Result := 'Proxy error';
    SocketGeneralProtocolError        : Result := 'Prtotocol error';
    SocketGeneralTimeOutError         : Result := 'Timeout';
    SocketGeneralConnectError         : Result := 'Connect error';
    SocketGeneralAbortError           : Result := 'Aborted';
  else
    Result := '';
  end;
end;



{                                                                              }
{ ASocket                                                                      }
{                                                                              }
constructor ASocket.Create;
begin
  inherited Create(nil);
  Init;
end;

constructor ASocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

constructor ASocket.Create(const SocketHandle: TSocket);
begin
  inherited Create(nil);
  Init(SocketHandle);
end;

constructor ASocket.Create(const Protocol: TSocketProtocol);
begin
  inherited Create(nil);
  Init(Protocol);
end;

procedure ASocket.Init;
begin
  FSocketHandle := INVALID_SOCKET;
  FState := ssClosed;
  if Assigned(GlobalSocketList) then
    GlobalSocketList.Add(self);
end;

procedure ASocket.Init(const SocketHandle: TSocket);
begin
  Init;
  SetSocketHandle(SocketHandle);
end;

procedure ASocket.Init(const Protocol: TSocketProtocol);
begin
  Init;
  SetProtocol(Protocol);
end;

procedure ASocket.Clear;
begin
  FState := ssClosed;
  FreeAndNilObjectArray(ObjectArray(FLookups));
  DestroySocketHandle;
end;

destructor ASocket.Destroy;
begin
  Clear;
  if Assigned(GlobalSocketList) then
    GlobalSocketList.Remove(self);
  inherited Destroy;
end;

procedure ASocket.Terminate;
var I : Integer;
    L : TSocketHostLookup;
begin
  inherited Terminate;
  For I := 0 to Length(FLookups) - 1 do
    begin
      L := FLookups[I];
      if Assigned(L) then
        L.Cancel;
    end;
  Clear;
end;

procedure ASocket.SetError(const ErrorCode: Integer; const ErrorMessage: String);
begin
  FErrorCode := ErrorCode;
  FErrorMessage := ErrorMessage;
  if (FErrorCode <> 0) and Assigned(FOnError) then
    FOnError(self);
end;

function ASocket.GetErrorMessage: String;
var Group: String;
begin
  // no error
  if FErrorCode = 0 then
    Result := '' else
  // custom message
  if FErrorMessage <> '' then
    Result := FErrorMessage else
  // custom error
  if FErrorCode >= SocketErrorBase then
    begin
      // specific message
      Result := SocketErrorStr(FErrorCode);
      if Result = '' then
        begin
          // general message
          Group := '';
          if FErrorCode >= UserSocketErrorBase then
            Group := 'user' else
          if FErrorCode >= SocketGeneralErrorBase then
            Group := 'general' else
          if FErrorCode >= HTTPTunnelErrorBase then
            Group := 'http-tunnel' else
          if FErrorCode >= SocksErrorBase then
            Group := 'socks' else
          if FErrorCode >= SocketProxyErrorBase then
            Group := 'proxy';
          Result := 'Socket ' + Group + iif(Group <> '', ' ', '') + 'error #' +
              IntToStr(FErrorCode);
        end;
    end else
  // winsock error
  if FErrorCode >= WinSockErrorBase then
    Result := WinSockErrorAsString(FErrorCode) else
    // error code only
    Result := 'Socket error #' + IntToStr(FErrorCode);
end;

procedure ASocket.ClearError;
begin
  FErrorCode := 0;
  FErrorMessage := '';
end;

procedure ASocket.RaiseError(const Msg: String; const ErrorCode: Integer);
begin
  SetError(ErrorCode, Msg);
  RaiseSocketError(Msg);
end;

procedure ASocket.RaiseWinSockError(const Msg: String; const ErrorCode: Integer);
begin
  RaiseError(Msg + ': ' + WinSockErrorAsString(ErrorCode), ErrorCode);
end;

procedure ASocket.RaiseLastWinSockError(const Msg: String);
begin
  RaiseWinSockError(Msg, WinSock.WSAGetLastError);
end;

procedure ASocket.CheckStateClosed(const Operation: String);
begin
  if FState = ssClosed then
    exit;
  RaiseError('Operation only allowed on closed socket: ' + Operation, SocketErrorSocketNotClosed);
end;

function ASocket.IsSocketConnected: Boolean;
begin
  Result := FState in [ssNegotiating, ssConnected];
end;

procedure ASocket.CheckSocketConnected;
begin
  if IsSocketConnected then
    exit;
  RaiseError('Socket not connected', SocketErrorSocketNotConnected);
end;

procedure ASocket.TriggerStateChange(const OldState, State: TSocketState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(self, OldState, State);
end;

procedure ASocket.SetState(const State: TSocketState);
var OldState : TSocketState;
begin
  if State = FState then
    exit;
  OldState := FState;
  FState := State;
  TriggerStateChange(OldState, State);
end;

procedure ASocket.DestroySocketHandle;
var H : TSocket;
begin
  H := FSocketHandle;
  if H = INVALID_SOCKET then
    exit;
  FSocketHandle := INVALID_SOCKET;
  WinSock.CloseSocket(H);
end;

function ASocket.ReleaseSocket: TSocket;
begin
  Result := FSocketHandle;
  FSocketHandle := INVALID_SOCKET;
end;

function ASocket.GetSocketHandle: TSocket;
begin
  if FSocketHandle = INVALID_SOCKET then
    FSocketHandle := AllocateSocketHandle(FProtocol);
  Result := FSocketHandle;
end;

procedure ASocket.SetSocketHandle(const SocketHandle: TSocket);
begin
  if SocketHandle = FSocketHandle then
    exit;
  if FState in [ssNegotiating, ssConnected, ssListening] then
    RaiseError('Socket handle can not be set on open socket', SocketErrorCanNotSetHandle);
  DestroySocketHandle;
  FSocketHandle := SocketHandle;
end;

function ASocket.LocalHostName: String;
begin
  Result := cWinSock.LocalHostName;
end;

procedure ASocket.SetProtocol(const Protocol: TSocketProtocol);
begin
  if Protocol = FProtocol then
    exit;
  CheckStateClosed('SetProtocol');
  FProtocol := Protocol;
end;

procedure ASocket.SetLocalHost(const LocalHost: String);
begin
  if LocalHost = FLocalHost then
    exit;
  CheckStateClosed('SetLocalHost');
  FLocalHost := LocalHost;
end;

procedure ASocket.SetLocalPort(const LocalPort: String);
begin
  if LocalPort = FLocalPort then
    exit;
  CheckStateClosed('SetLocalPort');
  FLocalPort := LocalPort;
end;

function ASocket.SocketGetLocalAddr: TSockAddrIn;
var L : Integer;
begin
  L := Sizeof(TSockAddrIn);
  FillChar(Result, L, #0);
  if WinSock.GetSockName(FSocketHandle, Result, L) <> 0 then
    RaiseLastWinSockError('Error retrieving local binding information');
end;

function ASocket.GetLocalPort: String;
var L : TSockAddrIn;
    I : Integer;
begin
  Result := FLocalPort;
  if (Result <> '') or not IsSocketConnected then
    exit;
  // If connected retrieve actual local port bound to
  I := Sizeof(TSockAddrIn);
  FillChar(L, I, #0);
  if WinSock.GetSockName(FSocketHandle, L, I) <> 0 then
    RaiseLastWinSockError('Error retrieving local name');
  Result := NetPortToPortStr(L.sin_port);
end;

function ASocket.HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
begin
  if (Msg = WM_SOCKET) and (FSocketHandle = TSocket(wParam)) then
    begin
      WMSocket(LoWord(LongWord(lParam)), HiWord(LongWord(lParam)));
      Result := 0;
    end else
    Result := inherited HandleWM(Msg, wParam, lParam);
end;

procedure ASocket.SocketBind(const Address: TSockAddrIn);
begin
  if WinSock.Bind(GetSocketHandle, PSockAddrIn(@Address)^, Sizeof(TSockAddrIn)) <> 0 then
    RaiseLastWinSockError('Socket bind failed');
end;

procedure ASocket.BindLocalAddress;
var Local : TSockAddrIn;
begin
  FillChar(Local, Sizeof(Local), #0);
  Local.sin_family := AF_INET;
  if FLocalHost <> '' then
    if ResolveHost(FLocalHost, Local.sin_addr) <> 0 then
      Local.sin_addr.S_addr := INADDR_ANY;
  if FLocalPort <> '' then
    if ResolvePort(FLocalPort, FProtocol, Local.sin_port) <> 0 then
      RaiseLastWinSockError('Invalid port');
  SocketBind(Local);
end;

procedure ASocket.SetSocketAsynchronous;
var Events : LongInt;
    Mode   : LongInt;
  begin
    if FSocketHandle <> INVALID_SOCKET then
      begin
        Mode := 1;
        if WinSock.ioctlsocket(FSocketHandle, FIONBIO, Mode) <> 0 then
          RaiseLastWinSockError('Asynchronous mode not set');
        Events := GetAsynchronousEvents;
        if WinSock.WSAAsyncSelect(FSocketHandle, GetWindowHandle, WM_SOCKET, Events) <> 0 then
          RaiseLastWinSockError('Asynchronous mode not set');
      end;
  end;

procedure ASocket.SetSocketBlocking;
var Mode : Integer;
begin
  if FSocketHandle <> INVALID_SOCKET then
    begin
      if WinSock.WSAAsyncSelect(FSocketHandle, GetWindowHandle, WM_SOCKET, 0) <> 0 then
        RaiseLastWinSockError('Blocking mode not set');
      Mode := 0;
      if WinSock.ioctlsocket(FSocketHandle, FIONBIO, Mode) <> 0 then
        RaiseLastWinSockError('Blocking mode not set');
    end;
end;

procedure ASocket.DoLookup(const Host: String; const LookupMethod: TSocketHostLookupMethod);
var Lookup : TSocketHostLookup;
    Addr   : TInAddr;
begin
  Lookup := SocketHostLookup(Host, Addr, LookupMethod, OnHostLookupComplete);
  if Assigned(Lookup) then
    Append(ObjectArray(FLookups), Lookup) else
    ActLookupComplete(0, Host, Addr);
end;

procedure ASocket.OnHostLookupComplete(Sender: TSocketHostLookup);
var A : TInAddr;
    I : Integer;
begin
  I := PosNext(Sender, ObjectArray(FLookups));
  if I >= 0 then
    try
      Remove(ObjectArray(FLookups), I, 1, False);
      if Sender.Success and (Sender.AddressCount > 0) then
        A := Sender.Address[0] else
        Integer(A) := INADDR_ANY;
      ActLookupComplete(Sender.ErrorCode, Sender.Host, A);
    finally
      Sender.Free;
    end;
end;

procedure ASocket.ActLookupComplete(const ErrorCode: Integer; const Host: String;
    const Addr: TInAddr);
begin
end;



{                                                                              }
{ ASocketExSendBuffer                                                          }
{                                                                              }

// AdvanceTimer adjusts timer values to be ~ averaged over last couple of seconds
procedure AdvanceTimer(var StartTick, Count: Integer; const TickCount: Integer;
    var Elapsed, TransferRate: Integer);
begin
  Elapsed := TickCount - StartTick;
  if Elapsed <= 1000 then
    exit;
  TransferRate := (Count * 1000) div Elapsed;
  While Elapsed > 50 do
    begin
      Elapsed := Elapsed div 2;
      Count := Count div 2;
    end;
  StartTick := TickCount - Elapsed;
end;

// Calculates how much of Size can be processed, given throttle parameters
function ThrottledSize(var StartTick, Count: Integer;
    const Size, TickCount, ThrottleRate: Integer; var TransferRate: Integer): Integer;
var Elapsed : Integer;
begin
  AdvanceTimer(StartTick, Count, TickCount, Elapsed, TransferRate);
  Result := MinI(Size, MaxI(0, ((ThrottleRate * (Elapsed + 20)) div 1000) - Count));
end;

procedure ASocketExSendBuffer.Init;
begin
  inherited Init;
  FUseSendBuffer := True;
  FOutBuffer := TDoublyLinkedList.Create;
  FWriteTimerStart := GetTickCount;
  FWriteTimerCount := 0;
  FTimerInterval := ThrottleInterval;
end;

destructor ASocketExSendBuffer.Destroy;
begin
  SetTimerActive(False);
  FreeAndNil(FOutBuffer);
  inherited Destroy;
end;

function ASocketExSendBuffer.IsSendBufferEmpty: Boolean;
begin
  Result := FOutBuffer.IsEmpty;
end;

function ASocketExSendBuffer.IsSendFlushed: Boolean;
begin
  Result := FReadyToSend and FOutBuffer.IsEmpty;
end;

function ASocketExSendBuffer.GetWriteRate: Integer;
var Elapsed : Integer;
begin
  AdvanceTimer(FWriteTimerStart, FWriteTimerCount, GetTickCount, Elapsed, FWriteRate);
  Result := FWriteRate;
end;

function ASocketExSendBuffer.ThrottleTimerActive: Boolean;
begin
  Result := FThrottleWrite and (FState = ssConnected);
end;

procedure ASocketExSendBuffer.RefreshThrottleTimer;
begin
  SetTimerActive(ThrottleTimerActive);
end;

procedure ASocketExSendBuffer.SetThrottleWrite(const ThrottleWrite: Boolean);
begin
  if ThrottleWrite = FThrottleWrite then
    exit;
  FThrottleWrite := ThrottleWrite;
  RefreshThrottleTimer;
  if not ThrottleWrite and (FState = ssConnected) then
    PostMessage(FWindowHandle, WM_SOCKET, FSocketHandle, FD_WRITE); // Re-start unthrottled handling
end;

procedure ASocketExSendBuffer.TriggerTimer;
begin
  if FThrottleWrite then
    HandleWriteEvent;
end;

procedure ASocketExSendBuffer.WMSocket(const Events, lWordHi: Word);
begin
  if Events and FD_WRITE <> 0 then
    HandleWriteEvent;
end;

procedure ASocketExSendBuffer.HandleWriteEvent;
begin
  FReadyToSend := True;
  if not FOutBuffer.IsEmpty then
    begin
      SendBufferedData;
      if FOutBuffer.IsEmpty then
        TriggerSendBufferEmpty;
    end;
end;

procedure ASocketExSendBuffer.TriggerSendBufferEmpty;
begin
  if Assigned(FOnSendBufferEmpty) then
    FOnSendBufferEmpty(self);
end;

function ASocketExSendBuffer.GetThrottledWriteSize(const Size: Integer): Integer;
begin
  if FThrottleWrite and (FState = ssConnected) then
    Result := ThrottledSize(FWriteTimerStart, FWriteTimerCount,
            Size, GetTickCount, FWriteThrottleRate, FWriteRate) else
    Result := Size;
end;

procedure ASocketExSendBuffer.ActSocketSent(const SendResult, Size: Integer;
    var Error, BytesSent: Integer);
begin
  if SendResult = SOCKET_ERROR then
    begin
      BytesSent := 0;
      Error := WinSock.WSAGetLastError;
      if Error = WSAEWOULDBLOCK then
        FReadyToSend := False;
    end else
    begin
      BytesSent := SendResult;
      Error := 0;
      Inc(FWriteTimerCount, SendResult);
    end;
end;

procedure ASocketExSendBuffer.ClearSendBuffer;
begin
  FOutBuffer.DeleteList;
end;



{                                                                              }
{ TSocketList                                                                  }
{                                                                              }
constructor TSocketList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TSocketList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TSocketList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TSocketList.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TSocketList.Count: Integer;
begin
  Result := Length(FList);
end;

procedure TSocketList.Add(const Socket: ASocket);
begin
  Lock;
  try
    Append(ObjectArray(FList), Socket);
  finally
    Unlock;
  end;
end;

procedure TSocketList.Remove(const Socket: ASocket);
var I : Integer;
begin
  Lock;
  try
    I := PosNext(Socket, ObjectArray(FList));
    if I < 0 then
      exit;
    cUtils.Remove(ObjectArray(FList), I, 1, False);
  finally
    Unlock;
  end;
end;

function TSocketList.GetItem(const Idx: Integer): ASocket;
begin
  Result := FList[Idx];
end;



initialization
  GlobalSocketList := TSocketList.Create;
finalization
  FreeAndNil(GlobalSocketList);
end.

