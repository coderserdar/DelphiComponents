{$INCLUDE ..\cDefines.inc}
unit cSocketsUDP;

{                                                                              }
{                             UDP Sockets v3.03                                }
{                                                                              }
{       This unit is copyright © 2002-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cSocketsUDP.pas                   }
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
{   05/02/2002  0.01  Added AUDPSocket.                                        }
{   09/10/2002  3.02  Revised for Fundamentals 3.                              }
{   22/06/2003  3.03  Fixed a bug in HandleReadEvent.                          }
{                                                                              }

interface

uses
  { Delphi }
  WinSock,

  { Fundamentals }
  cLinkedLists,
  cWinSock,
  cSocketHostLookup,
  cSockets;



{                                                                              }
{ AUDPSocket                                                                   }
{   UDP Socket with Send, Receive and Broadcast operations.                    }
{                                                                              }
{   SendTo has overloaded versions for either resolved or unresolved hosts.    }
{   Multiple host name resolutions can be done simultaneously. The packets     }
{   are sent in the order that they are resolved.                              }
{                                                                              }
{   ReadPacket returns False if no packet is available.                        }
{   BroadcastOption must be True to use Broadcast.                             }
{                                                                              }
{   If UseReceiveBuffer = True then unread packets are buffered, up to         }
{   MaxReceiveBufferPacketCount packets (set to -1 for no limit).              }
{                                                                              }
const
  DefaultMaxUDPPacketSize = 4096;

type
  AUDPSocket = class;
  AUDPSocketEvent = procedure (Sender: AUDPSocket) of object;
  AUDPSendFailedEvent = procedure (Sender: AUDPSocket; ErrorCode: Integer;
      Addr: TInAddr; PortNum: Word; Host, Port: String;
      const Data; DataSize: Integer) of object;
  AUDPSocket = class(ASocketExSendBuffer)
  protected
    FMaxPacketSize     : Integer;
    FBroadcastOption   : Boolean;
    FOnDataAvailable   : AUDPSocketEvent;
    FOnSendFailed      : AUDPSendFailedEvent;
    FBound             : Boolean;
    FDesignBound       : Boolean;
    FUseReceiveBuffer  : Boolean;
    FMaxRecvBufPackets : Integer;
    FInBuffer          : TDoublyLinkedList;

    procedure Init; override;
    procedure SetProtocol(const Protocol: TSocketProtocol); override;
    procedure SetLocalHost(const LocalHost: String); override;
    procedure SetLocalPort(const LocalPort: String); override;

    procedure CheckBound;
    function  GetBound: Boolean;
    procedure SetBound(const Bound: Boolean); virtual;
    procedure Loaded; override;

    procedure WMSocket(const Events, lWordHi: Word); override;
    function  GetAsynchronousEvents: LongInt; override;

    procedure SocketSetBroadcastOption(const BroadcastOption: Boolean);
    procedure SetBroadcastOption(const BroadcastOption: Boolean);

    procedure ActLookupComplete(const ErrorCode: Integer; const Host: String;
              const Addr: TInAddr); override;
    procedure TriggerSendFailed(const ErrorCode: Integer;
              const Addr: TInAddr; const PortNum: Word;
              const Host, Port: String;
              const Data; const DataSize: Integer);

    procedure SendBufferedData; override;
    function  SocketSendTo(const Dest: TSockAddr; const Buf; const Size: Integer;
              var Error: Integer): Boolean;

    function  SocketReadPacket (var Buf; const BufSize: Integer; var Size: Integer;
              var Address: TSockAddr): Boolean;
    procedure BufferInPacket(const Buf: String; const Addr: TSockAddr);
    function  ReceiveBufferAvailable: Boolean;

    procedure HandleReadEvent; virtual;
    procedure TriggerDataAvailable; virtual;

  public
    destructor Destroy; override;

    property  MaxPacketSize: Integer read FMaxPacketSize write FMaxPacketSize default DefaultMaxUDPPacketSize;
    property  BroadcastOption: Boolean read FBroadcastOption write SetBroadcastOption default False;
    property  UseReceiveBuffer: Boolean read FUseReceiveBuffer write FUseReceiveBuffer default True;
    property  MaxReceiveBufferPacketCount: Integer read FMaxRecvBufPackets write FMaxRecvBufPackets default -1;

    function  SendBufferPacketCount: Integer;
    function  SendBufferSize: Integer;
    function  ReceiveBufferPacketCount: Integer;
    function  ReceiveBufferSize: Integer;

    property  Bound: Boolean read GetBound write SetBound default False;
    procedure Bind; virtual;

    procedure SendTo(const Addr: TInAddr; const Port: Word; const Data: String); overload;
    procedure SendTo(const Addr: TInAddr; const Port: Word; const Data; const DataSize: Integer); overload;
    procedure SendTo(const Host, Port: String; const Data: String); overload;
    procedure SendTo(const Host, Port: String; const Data; const DataSize: Integer); overload;

    procedure Broadcast(const Port: String; const Data: String); overload;
    procedure Broadcast(const Port: String; const Data; const DataSize: Integer); overload;
    procedure Broadcast(const Port: Word; const Data: String); overload;
    procedure Broadcast(const Port: Word; const Data; const DataSize: Integer); overload;

    function  ReadPacket(var Buf; const BufSize: Integer;
              var Size: Integer; var Address: TSockAddr): Boolean; overload;
    function  ReadPacket(var Buf: String; var Address: TSockAddr): Boolean; overload;

    property  OnSendFailed: AUDPSendFailedEvent read FOnSendFailed write FOnSendFailed;
    property  OnDataAvailable: AUDPSocketEvent read FOnDataAvailable write FOnDataAvailable;
  end;



{                                                                              }
{ AUDPClientSocket                                                             }
{   AUDPClientSocket is used when all packets are sent to the same host.       }
{                                                                              }
{   To use, set Host and Port before doing other operations.                   }
{   Once resolved, RemoteAddr contains the resolved address of the host.       }
{                                                                              }
type
  AUDPClientSocket = class(AUDPSocket)
  protected
    FHost : String;
    FPort : String;

    FRemoteAddress : TInAddr;
    FRemotePort    : Word;

    procedure RaiseError(const Msg: String; const ErrorCode: Integer); override;
    procedure CheckResolved;

    procedure SetHost(const Host: String);
    procedure SetPort(const Port: String);
    function  GetPortAsInteger: Integer;
    procedure SetPortAsInteger(const Port: Integer);

    procedure ActLookupComplete(const ErrorCode: Integer; const Host: String;
              const Addr: TInAddr); override;

  public
    property  Host: String read FHost write SetHost;
    property  Port: String read FPort write SetPort;
    property  PortInt: Integer read GetPortAsInteger write SetPortAsInteger;

    procedure Resolve(const LookupMethod: TSocketHostLookupMethod = lmThread);
    property  RemoteAddress: TInAddr read FRemoteAddress;
    property  RemotePort: Word read FRemotePort;

    procedure Send(const Data; const DataSize: Integer);
    procedure SendStr(const Data: String);
  end;



{                                                                              }
{ TfndUDPSocket                                                                }
{                                                                              }
type
  TfndUDPSocket = class(AUDPSocket)
  published
    property  LocalHost;
    property  LocalPort;
    property  UseSendBuffer;
    property  MaxPacketSize;
    property  BroadcastOption;
    property  UseReceiveBuffer;
    property  MaxReceiveBufferPacketCount;
    property  Bound;
    property  OnError;
    property  OnSendFailed;
    property  OnDataAvailable;
  end;



{                                                                              }
{ TfndUDPClientSocket                                                          }
{                                                                              }
type
  TfndUDPClientSocket = class(AUDPClientSocket)
  published
    property  LocalHost;
    property  LocalPort;
    property  UseSendBuffer;
    property  MaxPacketSize;
    property  BroadcastOption;
    property  UseReceiveBuffer;
    property  MaxReceiveBufferPacketCount;
    property  Bound;
    property  Host;
    property  Port;
    property  OnError;
    property  OnSendFailed;
    property  OnDataAvailable;
  end;



implementation

uses
  // Delphi
  SysUtils,
  Classes,

  // Fundamentals
  cUtils,
  cStrings;



{                                                                              }
{ AUDPSocket                                                                   }
{                                                                              }

{ TUDPSendBufferItem                                                           }
type
  TUDPSendBufferItem = class(TDoublyLinkedString)
  protected
    procedure SetResolving(const Host, Port: String);

  public
    Addr      : TSockAddr;
    Host      : String;
    Port      : String;
    Resolving : Boolean;

    constructor Create(const Addr: TSockAddr; const Data: String); overload;
    constructor Create(const Addr: TSockAddr; const Data; const DataSize: Integer); overload;
    constructor Create(const Host, Port: String; const Data: String); overload;
    constructor Create(const Host, Port: String; const Data; const DataSize: Integer); overload;
  end;

constructor TUDPSendBufferItem.Create(const Addr: TSockAddr; const Data: String);
begin
  inherited Create(Data);
  self.Addr := Addr;
end;

constructor TUDPSendBufferItem.Create(const Addr: TSockAddr; const Data; const DataSize: Integer);
begin
  inherited Create(DupBuf(Data, DataSize));
  self.Addr := Addr;
end;

procedure TUDPSendBufferItem.SetResolving(const Host, Port: String);
begin
  Resolving := True;
  self.Host := Host;
  self.Port := Port;
end;

constructor TUDPSendBufferItem.Create(const Host, Port: String; const Data: String);
begin
  inherited Create(Data);
  SetResolving(Host, Port);
end;

constructor TUDPSendBufferItem.Create(const Host, Port: String; const Data; const DataSize: Integer);
begin
  inherited Create(DupBuf(Data, DataSize));
  SetResolving(Host, Port);
end;

{ TUDPReceiveBufferItem                                                        }
type
  TUDPReceiveBufferItem = class(TDoublyLinkedString)
  public
    Addr : TSockAddr;

    constructor Create(const Addr: TSockAddr; const Data: String);
  end;

constructor TUDPReceiveBufferItem.Create(const Addr: TSockAddr; const Data: String);
begin
  inherited Create(Data);
  self.Addr := Addr;
end;

{ AUDPSocket                                                                   }
procedure AUDPSocket.Init;
begin
  inherited Init;
  FProtocol := spUDP;
  FUseReceiveBuffer := True;
  FMaxRecvBufPackets := -1;
  FMaxPacketSize := DefaultMaxUDPPacketSize;
  FInBuffer := TDoublyLinkedList.Create;
end;

destructor AUDPSocket.Destroy;
begin
  FreeAndNil(FInBuffer);
  inherited Destroy;
end;

procedure AUDPSocket.SetProtocol(const Protocol: TSocketProtocol);
begin
  if Protocol <> spUDP then
    RaiseError('AUDPSocket.SetProtocol: Only UDP allowed here', SocketErrorCanNotSetProtocol);
end;

procedure AUDPSocket.SocketSetBroadcastOption(const BroadcastOption: Boolean);
var Opt : LongBool;
begin
  Opt := BroadcastOption;
  if WinSock.setsockopt(GetSocketHandle, SOL_SOCKET, SO_BROADCAST, PChar(@Opt), Sizeof(Opt)) <> 0 then
    RaiseLastWinSockError('Socket broadcast option not set');
end;

procedure AUDPSocket.SetBroadcastOption(const BroadcastOption: Boolean);
begin
  if FBroadcastOption = BroadcastOption then
    exit;
  if FBound then
    SocketSetBroadcastOption(BroadcastOption);
  FBroadcastOption := BroadcastOption;
end;

function AUDPSocket.GetAsynchronousEvents: LongInt;
begin
  Result := FD_READ or FD_WRITE;
end;

procedure AUDPSocket.WMSocket(const Events, lWordHi: Word);
begin
  if Events and FD_READ <> 0 then
    HandleReadEvent;
  inherited WMSocket(Events, lWordHi);
end;

procedure AUDPSocket.SetLocalHost(const LocalHost: String);
begin
  if LocalHost = FLocalHost then
    exit;
  inherited SetLocalHost(LocalHost);
  FBound := False;
end;

procedure AUDPSocket.SetLocalPort(const LocalPort: String);
begin
  if LocalPort = FLocalPort then
    exit;
  inherited SetLocalPort(LocalPort);
  FBound := False;
end;

procedure AUDPSocket.Bind;
begin
  if FBound then
    DestroySocketHandle;
  BindLocalAddress;
  FBound := True;
  SetSocketAsynchronous;
  if FBroadcastOption then
    SocketSetBroadcastOption(True);
end;

procedure AUDPSocket.CheckBound;
begin
  if not FBound then
    Bind;
end;

function AUDPSocket.GetBound: Boolean;
begin
  if [csDesigning, csLoading] * ComponentState <> [] then
    Result := FDesignBound else
    Result := FBound;
end;

procedure AUDPSocket.SetBound(const Bound: Boolean);
begin
  if [csDesigning, csLoading] * ComponentState <> [] then
    begin
      FDesignBound := Bound;
      exit;
    end;
  if Bound = FBound then
    exit;
  if Bound then
    Bind;
end;

procedure AUDPSocket.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    if FDesignBound then
      begin
        Bind;
        if FBroadcastOption then
          SocketSetBroadcastOption(True);
      end;
end;

procedure AUDPSocket.BufferInPacket(const Buf: String; const Addr: TSockAddr);
begin
  FInBuffer.Append(TUDPReceiveBufferItem.Create(Addr, Buf));
end;

function AUDPSocket.ReceiveBufferAvailable: Boolean;
begin
  Result := FUseReceiveBuffer and
            ((FMaxRecvBufPackets < 0) or (ReceiveBufferPacketCount < FMaxRecvBufPackets));
end;

procedure AUDPSocket.HandleReadEvent;
var Buf  : String;
    Size : Integer;
    Addr : TSockAddr;
    R    : Boolean;
begin
  // Allow packets to be read directly
  TriggerDataAvailable;

  if not ReceiveBufferAvailable then
    exit;

  // Buffer unread packets
  Repeat
    SetLength(Buf, FMaxPacketSize);
    R := SocketReadPacket(Pointer(Buf)^, FMaxPacketSize, Size, Addr);
    if R then
      begin
        SetLength(Buf, Size);
        BufferInPacket(Buf, Addr);
      end
  Until not R;
end;

procedure AUDPSocket.TriggerDataAvailable;
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(self);
end;

function AUDPSocket.SocketSendTo(const Dest: TSockAddr;
    const Buf; const Size: Integer; var Error: Integer): Boolean;
var L, M, R : Integer;
    P : Pointer;
    D : TSockAddr;
begin
  L := GetThrottledWriteSize(Size);
  if L < Size then // Throttled
    begin
      Result := False;
      Error := 0;
      exit;
    end;
  P := @Buf;
  D := Dest;
  M := Sizeof(D);
  R := WinSock.sendto(FSocketHandle, P^, L, 0, D, M);
  ActSocketSent(R, Size, Error, L);
  Result := L = Size;
end;

procedure AUDPSocket.TriggerSendFailed(const ErrorCode: Integer;
    const Addr: TInAddr; const PortNum: Word; const Host, Port: String;
    const Data; const DataSize: Integer);
begin
  if Assigned(FOnSendFailed) then
    FOnSendFailed(self, ErrorCode, Addr, PortNum, Host, Port, Data, DataSize);
end;

procedure AUDPSocket.SendBufferedData;
var P, N : TUDPSendBufferItem;
    D : Pointer;
    L, Error : Integer;
begin
  P := TUDPSendBufferItem(FOutBuffer.First);
  While Assigned(P) do
    begin
      N := TUDPSendBufferItem(P.Next);
      if not P.Resolving then
        begin
          L := Length(P.Value);
          if L = 0 then
            D := nil else
            D := Pointer(P.Value);
          if SocketSendTo(P.Addr, D^, L, Error) then
            FOutBuffer.Delete(P) else
            if (Error = WSAEWOULDBLOCK) or (Error = 0) then
              exit else
              begin
                FOutBuffer.Remove(P);
                TriggerSendFailed(Error, P.Addr.sin_addr, ntohs(P.Addr.sin_port),
                    '', '', Pointer(P.Value), Length(P.Value));
                P.Free;
              end
        end;
      P := N;
    end;
end;

function AUDPSocket.SendBufferPacketCount: Integer;
begin
  Result := FOutBuffer.Count;
end;

function AUDPSocket.SendBufferSize: Integer;
var P : TUDPSendBufferItem;
begin
  Result := 0;
  P := TUDPSendBufferItem(FOutBuffer.First);
  While Assigned(P) do
    begin
      Inc(Result, Length(P.Value));
      P := TUDPSendBufferItem(P.Next);
    end;
end;

function AUDPSocket.ReceiveBufferPacketCount: Integer;
begin
  Result := FInBuffer.Count;
end;

function AUDPSocket.ReceiveBufferSize: Integer;
var P : TUDPReceiveBufferItem;
begin
  Result := 0;
  P := TUDPReceiveBufferItem(FInBuffer.First);
  While Assigned(P) do
    begin
      Inc(Result, Length(P.Value));
      P := TUDPReceiveBufferItem(P.Next);
    end;
end;

procedure AUDPSocket.SendTo(const Addr: TInAddr; const Port: Word; const Data: String);
var L, Error : Integer;
    A : TSockAddr;
    D : Pointer;
begin
  CheckBound;
  PopulateSockAddr(A, Addr, Port);
  if not FUseSendBuffer or (FReadyToSend and FOutBuffer.IsEmpty) then
    begin
      L := Length(Data);
      if L = 0 then
        D := nil else
        D := Pointer(Data);
      if SocketSendTo(A, D^, L, Error) then
        exit;
      if Error <> WSAEWOULDBLOCK then
        TriggerSendFailed(Error, Addr, Port, '', '', Pointer(Data), Length(Data));
      if not FUseSendBuffer then
        RaiseWinSockError('Send failed', Error);
    end;
  FOutBuffer.Append(TUDPSendBufferItem.Create(A, Data));
end;

procedure AUDPSocket.SendTo(const Addr: TInAddr; const Port: Word; const Data; const DataSize: Integer);
var Error : Integer;
    A : TSockAddr;
begin
  CheckBound;
  PopulateSockAddr(A, Addr, Port);
  if not FUseSendBuffer or (FReadyToSend and FOutBuffer.IsEmpty) then
    begin
      if SocketSendTo(A, Data, DataSize, Error) then
        exit;
      if Error <> WSAEWOULDBLOCK then
        TriggerSendFailed(Error, Addr, Port, '', '', Data, DataSize);
      if not FUseSendBuffer then
        RaiseWinSockError('Send failed', Error);
    end;
  FOutBuffer.Append(TUDPSendBufferItem.Create(A, Data, DataSize));
end;

procedure AUDPSocket.ActLookupComplete(const ErrorCode: Integer; const Host: String; const Addr: TInAddr);
var B, N : TUDPSendBufferItem;
    P : Word;
    A : TInAddr;
begin
  B := TUDPSendBufferItem(FOutBuffer.First);
  While Assigned(B) do
    begin
      N := TUDPSendBufferItem(B.Next);
      if B.Resolving and (B.Host = Host) then
        begin
          FOutBuffer.Remove(B);
          ResolvePort(B.Port, spUDP, P);
          P := ntohs(P);
          if ErrorCode <> 0 then
            begin
              Integer(A) := INADDR_ANY;
              TriggerSendFailed(ErrorCode, A, P, Host, B.Port,
                  Pointer(B.Value), Length(B.Value));
            end else
            SendTo(Addr, P, B.Value);
          B.Free;
        end;
      B := N;
    end;
end;

procedure AUDPSocket.SendTo(const Host, Port: String; const Data: String);
begin
  CheckBound;
  FOutBuffer.Append(TUDPSendBufferItem.Create(Host, Port, Data));
  DoLookup(Host, lmThread);
end;

procedure AUDPSocket.SendTo(const Host, Port: String; const Data; const DataSize: Integer);
begin
  CheckBound;
  FOutBuffer.Append(TUDPSendBufferItem.Create(Host, Port, Data, DataSize));
  DoLookup(Host, lmThread);
end;

procedure AUDPSocket.Broadcast(const Port: Word; const Data: String);
var A : TInAddr;
begin
  CheckBound;
  Integer(A) := INADDR_BROADCAST;
  SendTo(A, Port, Data);
end;

procedure AUDPSocket.Broadcast(const Port: Word; const Data; const DataSize: Integer);
var A : TInAddr;
begin
  CheckBound;
  Integer(A) := INADDR_BROADCAST;
  SendTo(A, Port, Data, DataSize);
end;

procedure AUDPSocket.Broadcast(const Port: String; const Data: String);
var A : TInAddr;
    P : Word;
    E : Integer;
begin
  CheckBound;
  Integer(A) := INADDR_BROADCAST;
  E := ResolvePort(Port, spUDP, P);
  if E <> 0 then
    RaiseWinSockError('Error resolving port', E);
  P := ntohs(P);
  SendTo(A, P, Data);
end;

procedure AUDPSocket.Broadcast(const Port: String; const Data; const DataSize: Integer);
var A : TInAddr;
    P : Word;
    E : Integer;
begin
  CheckBound;
  Integer(A) := INADDR_BROADCAST;
  E := ResolvePort(Port, spUDP, P);
  if E <> 0 then
    RaiseWinSockError('Error resolving port', E);
  P := ntohs(P);
  SendTo(A, P, Data, DataSize);
end;

function AUDPSocket.SocketReadPacket(var Buf; const BufSize: Integer;
    var Size: Integer; var Address: TSockAddr): Boolean;
var L, M : Integer;
begin
  ClearError;
  CheckBound;
  L := Sizeof(Address);
  Size := WinSock.RecvFrom(FSocketHandle, Buf, BufSize, 0, Address, L);
  if Size <> SOCKET_ERROR then
    Result := True
  else
    begin
      M := WinSock.WSAGetLastError;
      if M = WSAEMSGSIZE then
        begin
          SetError(M, 'Packet truncated: Buffer too small');
          Result := True;
          Size := BufSize;
        end
      else
        begin
          Result := False;
          if M <> WSAEWOULDBLOCK then
            RaiseWinSockError('Error reading packet', M);
        end;
    end;
end;

function AUDPSocket.ReadPacket(var Buf; const BufSize: Integer; var Size: Integer;
    var Address: TSockAddr): Boolean;
var S : String;
    R : TUDPReceiveBufferItem;
begin
  if not FInBuffer.IsEmpty then
    begin
      R := TUDPReceiveBufferItem(FInBuffer.RemoveFirst);
      S := R.Value;
      Size := MinI(BufSize, Length(S));
      if Size > 0 then
        Move(Pointer(S)^, Buf, Size);
      Address := R.Addr;
      R.Free;
      Result := True;
      exit;
    end;
  Result := SocketReadPacket(Buf, BufSize, Size, Address);
end;

function AUDPSocket.ReadPacket(var Buf: String; var Address: TSockAddr): Boolean;
var L : Integer;
    P : Pointer;
    R : TUDPReceiveBufferItem;
begin
  if not FInBuffer.IsEmpty then
    begin
      R := TUDPReceiveBufferItem(FInBuffer.RemoveFirst);
      Buf := R.Value;
      Address := R.Addr;
      R.Free;
      Result := True;
      exit;
    end;
  SetLength(Buf, FMaxPacketSize);
  P := Pointer(Buf);
  Result := SocketReadPacket(P^, FMaxPacketSize, L, Address);
  if Result then
    SetLength(Buf, L);
end;



{                                                                              }
{ AUDPClientSocket                                                             }
{                                                                              }
procedure AUDPClientSocket.RaiseError(const Msg: String; const ErrorCode: Integer);
var S : String;
begin
  S := Msg;
  if (FHost <> '') and (FPort <> '') then
    S := S + ': Host ' + FHost + ':' + FPort;
  inherited RaiseError(S, ErrorCode);
end;

procedure AUDPClientSocket.SetHost(const Host: String);
begin
  if Host = FHost then
    exit;
  CheckStateClosed('SetHost');
  FHost := Host;
end;

procedure AUDPClientSocket.SetPort(const Port: String);
begin
  if Port = FPort then
    exit;
  CheckStateClosed('SetPort');
  FPort := Port;
end;

function AUDPClientSocket.GetPortAsInteger: Integer;
begin
  Result := StrToIntDef(FPort, 0);
end;

procedure AUDPClientSocket.SetPortAsInteger(const Port: Integer);
begin
  SetPort(IntToStr(Port));
end;

procedure AUDPClientSocket.ActLookupComplete(const ErrorCode: Integer;
    const Host: String; const Addr: TInAddr);
begin
  if (Host = FHost) and (FState = ssResolving) then
    if ErrorCode <> 0 then
      begin
        SetError(ErrorCode, '');
        SetState(ssClosed);
      end else
      begin
        FRemoteAddress := Addr;
        SetState(ssResolved);
      end
  else
    inherited ActLookupComplete(ErrorCode, Host, Addr);
end;

procedure AUDPClientSocket.Resolve(const LookupMethod: TSocketHostLookupMethod);
var P : Word;
begin
  CheckStateClosed('Resolve');
  SetState(ssResolving);
  if ResolvePort(FPort, spUDP, P) <> 0 then
    RaiseLastWinSockError('Port not resolved');
  FRemotePort := ntohs(P);
  DoLookup(FHost, LookupMethod);
end;

procedure AUDPClientSocket.CheckResolved;
begin
  if FState <> ssResolved then
    begin
      Resolve(lmBlock);
      if FState <> ssResolved then
        inherited RaiseError('Host not resolved', SocketErrorHostNotResolved);
    end;
end;

procedure AUDPClientSocket.Send(const Data; const DataSize: Integer);
begin
  CheckResolved;
  SendTo(FRemoteAddress, FRemotePort, Data, DataSize);
end;

procedure AUDPClientSocket.SendStr(const Data: String);
begin
  CheckResolved;
  SendTo(FRemoteAddress, FRemotePort, Data);
end;



end.

