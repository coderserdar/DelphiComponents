{$INCLUDE ..\cDefines.inc}
unit cWinSock;

{                                                                              }
{                           WinSock functions 3.03                             }
{                                                                              }
{      This unit is copyright © 2001-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cWinSock.pas                    }
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
{ Description:                                                                 }
{   Support functions to access WinSock API.                                   }
{                                                                              }
{ Revision history:                                                            }
{   11/12/2001  0.01  Spawned from cSockets.                                   }
{   12/12/2001  0.02  Added LocalHost functions.                               }
{   01/07/2002  3.03  Refactored for Fundamentals 3.                           }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,
  Classes,
  WinSock,

  { Fundamentals }
  cUtils;



{                                                                              }
{ WinSockStartup                                                               }
{   The first call to WinSockStartup initializes the WinSock API. Subsequent   }
{   calls have no effect. The WinSock API is shut down when the application    }
{   shuts down.                                                                }
{                                                                              }
procedure WinSockStartup;



{                                                                              }
{ Exceptions                                                                   }
{                                                                              }
type
  EWinSock = class(Exception);

procedure RaiseSocketError(const Msg: String);
procedure RaiseWinSockError(const Msg: String; const WinSockError: Integer);
procedure RaiseLastWinSockError(const Msg: String);



{                                                                              }
{ WinSockErrorAsString                                                         }
{                                                                              }
function  WinSockErrorAsString(const Error: Integer): String;



{                                                                              }
{ TSocketProtocol                                                              }
{                                                                              }
type
  TSocketProtocol = (spTCP, spUDP);

function  SocketProtocolAsString(const Protocol: TSocketProtocol): String;



{                                                                              }
{ IP Addresses                                                                 }
{   IsIPAddress returns True if Address is a valid IP address. NetAddress      }
{   contains the address in network byte order.                                }
{   IsInternetIP returns True if Address appears to be an Internet IP.         }
{                                                                              }
function  IsIPAddress(const Address: String; var NetAddress : TInAddr): Boolean;
function  IPAddressStr(const Address: TInAddr): String;
function  IsInternetIPAddress(const Address: TInAddr): Boolean;
procedure ReverseIP(var Address : TInAddr);



{                                                                              }
{ ResolvePort                                                                  }
{   Returns the WinSock error (0 for success).                                 }
{   NetPort contains the Port value in network byte order.                     }
{                                                                              }
function  ResolvePort(const Port: String; const Protocol: TSocketProtocol;
          var NetPort : Word): Integer;
function  NetPortToPort(const NetPort: Word): Word;
function  NetPortToPortStr(const NetPort: Word): String;



{                                                                              }
{ ResolveHost                                                                  }
{   Resolves Host (IP or domain name). Blocks. Returns True if successful.     }
{                                                                              }
function  ResolveHost(const Host: String; var Address : TInAddr): Integer;



{                                                                              }
{ HostEnt functions                                                            }
{                                                                              }
type
  TInAddrArray = Array of TInAddr;

function  HostEntAddressesCount(const HostEnt: PHostEnt): Integer;
function  HostEntAddresses(const HostEnt: PHostEnt): TInAddrArray;
function  HostEntAddress(const HostEnt: PHostEnt; const Index: Integer = 0): TInAddr;
function  HostEntAddressStr(const HostEnt: PHostEnt; const Index: Integer = 0): String;
function  HostEntName(const HostEnt: PHostEnt): String;



{                                                                              }
{ LocalHost                                                                    }
{                                                                              }
function  LocalHostName: String;
function  LocalIPAddresses: TInAddrArray;
function  LocalIPAddressesStr: StringArray;
procedure AddLocalIPAddressesToStrings(const S: TStrings);
function  GuessInternetIP: TInAddr;
function  GuessInternetIPStr: String;



{                                                                              }
{ RemoteHost                                                                   }
{                                                                              }
function  GetRemoteHostName(const Address: TInAddr): String;



{                                                                              }
{ AllocateSocketHandle                                                         }
{   Returns a handle to a new WinSock socket.                                  }
{                                                                              }
function  AllocateSocketHandle(const Protocol: TSocketProtocol): TSocket;



{                                                                              }
{ WinSock structures                                                           }
{                                                                              }
procedure PopulateSockAddr(var SockAddr : TSockAddr; const Addr: TInAddr; const Port: Word);



{                                                                              }
{ WinSock constants                                                            }
{                                                                              }
const
  SD_RECEIVE     = 0;
  SD_SEND        = 1;
  SD_BOTH        = 2;

  // WinSock2 Socket Options extentions
  SO_GROUP_ID              = $2001; // ID of a socket group
  SO_GROUP_PRIORITY        = $2002; // the relative priority within a group
  SO_MAX_MSG_SIZE          = $2003; // maximum message size



implementation

uses
  { Fundamentals }
  cStrings;



{                                                                              }
{ WinSockStartup / WinSockCleanup                                              }
{                                                                              }
var
  WinSockStarted : Boolean = False;
  WinSockData    : WSAData;

procedure WinSockStartup;
var Err : Integer;
begin
  if WinSockStarted then
    exit;
  Err := WinSock.WSAStartup($101, WinSockData);
  if Err <> 0 then
    RaiseWinSockError('Winsock startup failed', Err);
  WinSockStarted := True;
end;

procedure WinSockCleanup;
begin
  if not WinSockStarted then
    exit;
  WinSockStarted := False;
  WinSock.WSACleanup;
end;



{                                                                              }
{ RaiseSocketError                                                             }
{                                                                              }
procedure RaiseSocketError(const Msg: String);
begin
  raise EWinSock.Create(Msg);
end;

procedure RaiseWinSockError(const Msg: String; const WinSockError: Integer);
begin
  RaiseSocketError(Msg + ': ' + WinSockErrorAsString(WinSockError));
end;

procedure RaiseLastWinSockError(const Msg: String);
begin
  RaiseWinSockError(Msg, WinSock.WSAGetLastError);
end;



{                                                                              }
{ WinSockErrorAsString                                                         }
{                                                                              }
function WinSockErrorAsString(const Error: Integer): String;
begin
  Case Error of
    0                  : Result := '';
    WSASYSNOTREADY     : Result := 'WinSock not ready';
    WSAVERNOTSUPPORTED : Result := 'WinSock version not supported';
    WSAEINPROGRESS     : Result := 'Blocking WinSock operation in progress';
    WSAEPROCLIM	       : Result := 'WinSock task limit reached';
    WSAEFAULT          : Result := 'Generic WinSock fault';
    WSANOTINITIALISED  : Result := 'WinSock not initialized';
    WSAENETDOWN	       : Result := 'The network subsystem has failed';
    WSAENETUNREACH     : Result := 'The network is unreachable';
    WSAENETRESET       : Result := 'Network reset';
    WSAEHOSTDOWN       : Result := 'Host is unavailable';
    WSAEHOSTUNREACH    : Result := 'Host is unreachable';
    WSAHOST_NOT_FOUND  : Result := 'Host not found';
    WSANO_DATA         : Result := 'Host address not found';
    WSAECONNRESET      : Result := 'Connection reset by peer';
    WSANO_RECOVERY     : Result := 'Nonrecoverable WinSock error occurred';
    WSAEMFILE	       : Result := 'WinSock file error';
    WSAENOBUFS	       : Result := 'No buffer space available for socket';
    WSAESOCKTNOSUPPORT : Result := 'The socket type is not supported';
    WSAENOTSOCK        : Result := 'Socket operation on non-socket or not connected';
    WSAENOTCONN        : Result := 'Socket is not connected';
    WSAESHUTDOWN       : Result := 'Socket is shutting down';
    WSAETIMEDOUT       : Result := 'Socket operation timed out';
    WSAECONNREFUSED    : Result := 'Connection refused';
    WSAEADDRINUSE      : Result := 'Address in use';
    WSAEADDRNOTAVAIL   : Result := 'Address not available';
    WSAEBADF           : Result := 'Socket error: Invalid format';
    WSAEINVAL          : Result := 'Socket error: Invalid operation';
    WSAEACCES          : Result := 'Socket permission denied';
    WSAEMSGSIZE        : Result := 'Socket error: Invalid message size';
    WSAENOPROTOOPT     : Result := 'Protocol not available';
    WSAEPROTONOSUPPORT : Result := 'Protocol not supported';
    WSAEPFNOSUPPORT    : Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT    : Result := 'Address family not supported by protocol family';
    WSAEOPNOTSUPP      : Result := 'Socket error: Operation not supported';
    WSAENAMETOOLONG    : Result := 'Socket error: Name too long';
    WSAEINTR           : Result := 'Socket error: System level interruption';
    WSAECONNABORTED    : Result := 'Connection aborted';
    WSAEDISCON         : Result := 'Socket has been disconnected';
  else
    Result := 'WinSock error #' + IntToStr(Error);
  end;
end;



{                                                                              }
{ SocketProtocolAsString                                                       }
{                                                                              }
const
  ProtocolStr: Array[TSocketProtocol] of String = ('tcp', 'udp');

function SocketProtocolAsString(const Protocol: TSocketProtocol): String;
begin
  Result := ProtocolStr[Protocol];
end;



{                                                                              }
{ IsIPAddress                                                                  }
{                                                                              }
function IsIPAddress(const Address: String; var NetAddress : TInAddr): Boolean;
var P, Q : PChar;
    I    : Integer;
begin
  // Not long enough
  if Length(Address) < 7 then
    begin
      NetAddress.S_addr := u_long(INADDR_NONE);
      Result := False;
      exit;
    end;

  // Quick validity checking
  P := PChar(Address);
  While P^ = ' ' do
    Inc(P);
  Q := P;
  I := 0;
  While Q^ <> #0 do
    if not (Q^ in ['0'..'9', '.', ' ']) then
      begin
        NetAddress.S_addr := u_long(INADDR_NONE);
        Result := False;
        exit;
      end else
      begin
        if Q^ = '.' then
          Inc(I);
        Inc(Q);
      end;
  if I <> 3 then // 3 dots required
    begin
      NetAddress.S_addr := u_long(INADDR_NONE);
      Result := False;
      exit;
    end;

  // Use WinSock to resolve IP
  WinSockStartup;
  NetAddress.S_addr := WinSock.Inet_Addr(P);
  if NetAddress.S_addr <> u_long(INADDR_NONE) then
    Result := True else
    if Address = '255.255.255.255' then // Check for broadcast IP (INADDR_NONE = INADDR_BROADCAST)
      begin
        NetAddress.S_addr := u_long(INADDR_BROADCAST);
        Result := True;
      end else
      Result := False;
end;



{                                                                              }
{ IPAddressStr                                                                 }
{                                                                              }
function IPAddressStr(const Address: TInAddr): String;
begin
  Result := PChar(inet_ntoa(Address));
end;



{                                                                              }
{ ResolvePort                                                                  }
{                                                                              }
function ResolvePort(const Port: String; const Protocol: TSocketProtocol;
    var NetPort : Word): Integer;
var PEnt : PServEnt;
    Prot : String;
begin
  if Port = '' then
    begin
      NetPort := 0;
      Result := 0;
      exit;
    end;
  if StrIsNumeric(Port) then
    begin
      NetPort := htons(StrToInt(Port));
      Result := 0;
      exit;
    end;
  WinSockStartup;
  Prot := SocketProtocolAsString(Protocol);
  PEnt := WinSock.GetServByName(PChar(Port), PChar(Prot));
  if not Assigned(PEnt) then
    begin
      NetPort := 0;
      Result := WinSock.WSAGetLastError;
    end else
    begin
      NetPort := PEnt^.s_port;
      Result := 0;
    end;
end;

function NetPortToPort(const NetPort: Word): Word;
begin
  Result := ntohs(NetPort);
end;

function NetPortToPortStr(const NetPort: Word): String;
begin
  Result := IntToStr(NetPortToPort(NetPort));
end;



{                                                                              }
{ ResolveHost                                                                  }
{                                                                              }
function ResolveHost(const Host: String; var Address : TInAddr): Integer;
var HostEnt : PHostEnt;
begin
  if IsIPAddress(Host, Address) then
    begin
      Result := 0;
      exit;
    end;
  HostEnt := WinSock.GetHostByName(PChar(Host));
  Result := WinSock.WSAGetLastError;
  if Assigned(HostEnt) then
    Address := HostEntAddress(HostEnt, 0) else
    Address.S_addr := u_long(INADDR_NONE);
end;



{                                                                              }
{ HostEntAddressCount                                                          }
{                                                                              }
function HostEntAddressesCount(const HostEnt: PHostEnt): Integer;
var P : ^PInAddr;
    Q : PInAddr;
begin
  Result := 0;
  if not Assigned(HostEnt) then
    exit;

  Assert(HostEnt^.h_addrtype = AF_INET, 'IP addresses required');
  Assert(HostEnt^.h_length = Sizeof(TInAddr), 'IP addresses required');

  P := Pointer(HostEnt^.h_addr_list);
  if not Assigned(P) then
    exit;
  Q := P^;
  While Assigned(Q) do
    begin
      Inc(P);
      Inc(Result);
      Q := P^
    end;
end;



{                                                                              }
{ HostEntAddresses                                                             }
{                                                                              }
function HostEntAddresses(const HostEnt: PHostEnt): TInAddrArray;
var P : ^PInAddr;
    I, L : Integer;
begin
  L := HostEntAddressesCount(HostEnt);
  SetLength(Result, L);
  if L = 0 then
    exit;
  P := Pointer(HostEnt^.h_addr_list);
  For I := 0 to L - 1 do
    begin
      Result[I] := P^^;
      Inc(P);
    end;
end;



{                                                                              }
{ HostEntAddress                                                               }
{                                                                              }
function HostEntAddress(const HostEnt: PHostEnt; const Index: Integer): TInAddr;
var P : ^PInAddr;
    Q : PInAddr;
    I : Integer;
begin
  LongInt(Result.S_addr) := LongInt(INADDR_NONE);
  if not Assigned(HostEnt) then
    exit;

  Assert(HostEnt^.h_addrtype = AF_INET, 'IP addresses required');
  Assert(HostEnt^.h_length = Sizeof(TInAddr), 'IP addresses required');

  P := Pointer(HostEnt^.h_addr_list);
  if not Assigned(P) then
    exit;
  Q := P^;
  I := 0;
  While Assigned(Q) and (I < Index) do
    begin
      Inc(P);
      Inc(I);
      Q := P^
    end;
  if Assigned(Q) then
    Result := Q^;
end;



{                                                                              }
{ HostEntAddressStr                                                            }
{                                                                              }
function HostEntAddressStr(const HostEnt: PHostEnt; const Index: Integer): String;
begin
  Result := IPAddressStr(HostEntAddress(HostEnt, Index));
end;



{                                                                              }
{ HostEntName                                                                  }
{                                                                              }
function HostEntName(const HostEnt: PHostEnt): String;
begin
  Result := HostEnt.h_name;
end;



{                                                                              }
{ LocalHostName                                                                }
{                                                                              }
function LocalHostName: String;
var Buf : Array[0..255] of Char;
begin
  WinSockStartup;
  if WinSock.gethostname(@Buf, Sizeof(Buf)) <> 0 then
    RaiseLastWinSockError('LocalHostName not available');
  Result := PChar(@Buf);
end;



{                                                                              }
{ LocalIPAddress                                                               }
{                                                                              }
function LocalIPAddresses: TInAddrArray;
begin
  WinSockStartup;
  Result := HostEntAddresses(WinSock.gethostbyname(PChar(LocalHostName)));
end;

function LocalIPAddressesStr: StringArray;
var V : TInAddrArray;
    I, L : Integer;
begin
  V := LocalIPAddresses;
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := IPAddressStr(V[I]);
end;

procedure AddLocalIPAddressesToStrings(const S: TStrings);
var V : TInAddrArray;
    I, L : Integer;
begin
  V := LocalIPAddresses;
  L := Length(V);
  For I := 0 to L - 1 do
    S.Add(IPAddressStr(V[I]));
end;



{                                                                              }
{ GuessInternetIP                                                              }
{                                                                              }
function IsInternetIPAddress(const Address: TInAddr): Boolean;
begin
  Result := not (Byte(Address.S_un_b.s_b1) in [0, 10, 125, 127, 169, 172, 192, 224, 255]);
end;

procedure ReverseIP(var Address: TInAddr);
begin
  Swap(Byte(Address.S_un_b.s_b1), Byte(Address.S_un_b.s_b4));
  Swap(Byte(Address.S_un_b.s_b2), Byte(Address.S_un_b.s_b3));
end;

function GuessInternetIP: TInAddr;
var A : TInAddrArray;
    I : Integer;
begin
  A := LocalIPAddresses;
  For I := 0 to Length(A) - 1 do
    if IsInternetIPAddress(A[I]) then
      begin
        Result := A[I];
        exit;
      end;
  LongInt(Result.S_addr) := LongInt(INADDR_NONE);
end;



{                                                                              }
{ GuessInternetIPStr                                                           }
{                                                                              }
function GuessInternetIPStr: String;
var A : TInAddr;
begin
  A := GuessInternetIP;
  if LongInt(A) = LongInt(INADDR_NONE) then
    Result := '' else
    Result := IPAddressStr(A);
end;



{                                                                              }
{ RemoteHost                                                                   }
{                                                                              }
function GetRemoteHostName(const Address: TInAddr): String;
var E : PHostEnt;
begin
  E := WinSock.gethostbyaddr(@Address, Sizeof(TInAddr), AF_INET);
  if not Assigned(E) then
    begin
      Result := '';
      exit;
    end;
  Result := StrPas(E^.h_name);
end;



{                                                                              }
{ AllocateSocketHandle                                                         }
{                                                                              }
function AllocateSocketHandle(const Protocol: TSocketProtocol): TSocket;
begin
  WinSockStartup;
  Case Protocol of
    spTCP : Result := WinSock.Socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    spUDP : Result := WinSock.Socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    else Result := INVALID_SOCKET;
  end;
  if Result = INVALID_SOCKET then
    RaiseLastWinSockError('Socket allocation failed');
end;



{                                                                              }
{ WinSock structures                                                           }
{                                                                              }
procedure PopulateSockAddr(var SockAddr: TSockAddr; const Addr: TInAddr; const Port: Word);
begin
  FillChar(SockAddr, Sizeof(TSockAddr), #0);
  With SockAddr do
    begin
      sin_family := AF_INET;
      sin_port := htons(Port);
      sin_addr := Addr;
    end;
end;



initialization
finalization
  WinSockCleanup;
end.

