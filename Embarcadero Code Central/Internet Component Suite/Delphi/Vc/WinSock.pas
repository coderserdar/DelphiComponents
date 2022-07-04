{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE, C.H.R. Citadelle
EMail:        francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette
              francois.piette@rtfm.be
Creation:     July 18, 1996
Object:       Windows 16bit API Interface Unit for Delphi 1.x and
              compatible with Borland Delphi 2.x Winsock
Support:      Please ask your question in the following newsgroup:
              news://forums.borland.com/borland.public.delphi.vcl.components.using
Legal issues: Copyright (C) 1996, 1997, 1998 by François PIETTE 
              <francois.piette@pophost.be>

              This software is provided 'as-is', without any express or
  	      implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source 
                 distribution.

Updates:
Sep 21, 1997 Added $IFDEF to warn Delphi 2 or 3 user that something is wrong
             if they use this file.
Dec 13, 1997 Changed winsocket form 'WINSOCK.DLL' to 'WINSOCK' because Win 3.x
             like thos have it without extension (don't ask me why !)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Winsock;

interface

{$IFNDEF VER80}
  'This file is for use with Delphi 1 only. Use the Borland provided file'
  'with any other Delphi Version. If you use this file with Delphi 2 or 3'
  'this is probably because your library path is wrong or you have not'
  'restored the directory structure when unzipping the file (you must use'
  'pkunzip option -d to restore the files).'
{$ENDIF}


uses WinTypes, WinProcs;

const
  winsocket = 'WINSOCK';
  { If your application can't find winsock.dll on startup, please try to  }
  { change the preceding line to "winsocket = 'winsock.dll';"             }
  { Also, try upper or lower case letters. Win 3.x is very capricious !   }

  { Misc constants }
  FD_SETSIZE               = 64;

  {
    Commands for ioctlsocket(),  taken from the BSD file fcntl.h.

    Ioctl's have the command encoded in the lower word,
    and the size of any in or out parameters in the upper
    word.  The high 2 bits of the upper word are used
    to encode the in/out status of the parameter; for now
    we restrict parameters to at most 128 bytes.
    0x20000000 distinguishes new & old ioctl's
  }
  IOCPARM_MASK             = $7f;         { parameters must be < 128 bytes }
  IOC_VOID                 = $20000000;   { no parameters }
  IOC_OUT                  = $40000000;   { copy out parameters }
  IOC_IN                   = $80000000;   { copy in parameters }
  IOC_INOUT                = (IOC_IN + IOC_OUT);

  FIONREAD                 = $4004667F;   { get # bytes to read }
  FIONBIO                  = $8004667E;   { set/clear non-blocking i/o }
  FIOASYNC                 = $8004667D;   { set/clear async i/o }

  { Socket I/O Controls }
  SIOCSHIWAT               = $80047300;   { set high watermark }
  SIOCGHIWAT               = $40047301;   { set low watermark }
  SIOCSLOWAT               = $80047302;   { set low watermark }
  SIOCGLOWAT               = $40047303;   { get low watermark }
  SIOCATMARK               = $40047307;   { at oob mark? }

  INADDR_ANY               = $00000000;
  INADDR_LOOPBACK          = $7f000001;
  INADDR_BROADCAST         = $ffffffff;
  INADDR_NONE              = $ffffffff;

  WSADESCRIPTION_LEN       = 256;
  WSASYS_STATUS_LEN        = 128;

  { Protocols }
  IPPROTO_IP         =  0;              { dummy for IP }
  IPPROTO_ICMP       =  1;              { control message protocol }
  IPPROTO_GGP        =  2;              { gateway^2 (deprecated) }
  IPPROTO_TCP        =  6;              { tcp }
  IPPROTO_PUP        =  12;             { pup }
  IPPROTO_UDP        =  17;             { user datagram protocol }
  IPPROTO_IDP        =  22;             { xns idp }
  IPPROTO_ND         =  77;             { UNOFFICIAL net disk proto }
  IPPROTO_RAW        = 255;             { raw IP packet }
  IPPROTO_MAX        = 256;

  { Port/socket numbers: network standard functions }
  IPPORT_ANY         =     0;
  IPPORT_ECHO        =     7;
  IPPORT_DISCARD     =     9;
  IPPORT_SYSTAT      =     11;
  IPPORT_DAYTIME     =     13;
  IPPORT_NETSTAT     =     15;
  IPPORT_FTP         =     21;
  IPPORT_TELNET      =     23;
  IPPORT_SMTP        =     25;
  IPPORT_TIMESERVER  =     37;
  IPPORT_NAMESERVER  =     42;
  IPPORT_WHOIS       =     43;
  IPPORT_MTP         =     57;

  { Port/socket numbers: host specific functions }
  IPPORT_TFTP        =     69;
  IPPORT_RJE         =     77;
  IPPORT_FINGER      =     79;
  IPPORT_TTYLINK     =     87;
  IPPORT_SUPDUP      =     95;

  { UNIX TCP sockets }
  IPPORT_EXECSERVER  =     512;
  IPPORT_LOGINSERVER =     513;
  IPPORT_CMDSERVER   =     514;
  IPPORT_EFSSERVER   =     520;

  { UNIX UDP sockets }
  IPPORT_BIFFUDP     =     512;
  IPPORT_WHOSERVER   =     513;
  IPPORT_ROUTESERVER =     520;

  { Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root) }
  IPPORT_RESERVED    =     1024;

  { Link numbers }
  IMPLINK_IP         =     155;
  IMPLINK_LOWEXPER   =     156;
  IMPLINK_HIGHEXPER  =     158;

  INVALID_SOCKET     =     $ffff;
  SOCKET_ERROR       =     (-1);

  { Types }
  SOCK_STREAM        =  1;              { stream socket }
  SOCK_DGRAM         =  2;              { datagram socket }
  SOCK_RAW           =  3;              { raw-protocol interface }
  SOCK_RDM           =  4;              { reliably-delivered message }
  SOCK_SEQPACKET     =  5;              { sequenced packet stream }

  { Option flags per-socket }
  SO_DEBUG           =  $0001;         { turn on debugging info recording }
  SO_ACCEPTCONN      =  $0002;         { socket has had listen() }
  SO_REUSEADDR       =  $0004;         { allow local address reuse }
  SO_KEEPALIVE       =  $0008;         { keep connections alive }
  SO_DONTROUTE       =  $0010;         { just use interface addresses }
  SO_BROADCAST       =  $0020;         { permit sending of broadcast msgs }
  SO_USELOOPBACK     =  $0040;         { bypass hardware when possible }
  SO_LINGER          =  $0080;         { linger on close if data present }
  SO_OOBINLINE       =  $0100;         { leave received OOB data in line }
  SO_DONTLINGER      = (not SO_LINGER);

 { Additional options }
  SO_SNDBUF          =  $1001;         { send buffer size }
  SO_RCVBUF          =  $1002;         { receive buffer size }
  SO_SNDLOWAT        =  $1003;         { send low-water mark }
  SO_RCVLOWAT        =  $1004;         { receive low-water mark }
  SO_SNDTIMEO        =  $1005;         { send timeout }
  SO_RCVTIMEO        =  $1006;         { receive timeout }
  SO_ERROR           =  $1007;         { get error status and clear }
  SO_TYPE            =  $1008;         { get socket type }


  { TCP options }
  TCP_NODELAY        =  $0001;

  { Address families }
  AF_UNSPEC          =  0;              { unspecified }
  AF_UNIX            =  1;              { local to host (pipes, portals) }
  AF_INET            =  2;              { internetwork: UDP, TCP, etc. }
  AF_IMPLINK         =  3;              { arpanet imp addresses }
  AF_PUP             =  4;              { pup protocols: e.g. BSP }
  AF_CHAOS           =  5;              { mit CHAOS protocols }
  AF_NS              =  6;              { XEROX NS protocols }
  AF_ISO             =  7;              { ISO protocols }
  AF_OSI             =  AF_ISO;         { OSI is ISO }
  AF_ECMA            =  8;              { european computer manufacturers }
  AF_DATAKIT         =  9;              { datakit protocols }
  AF_CCITT           =  10;             { CCITT protocols, X.25 etc }
  AF_SNA             =  11;             { IBM SNA }
  AF_DECnet          =  12;             { DECnet }
  AF_DLI             =  13;             { Direct data link interface }
  AF_LAT             =  14;             { LAT }
  AF_HYLINK          =  15;             { NSC Hyperchannel }
  AF_APPLETALK       =  16;             { AppleTalk }
  AF_NETBIOS         =  17;             { NetBios-style addresses }
  AF_MAX             =  18;

  { Protocol families, same as address families for now }
  PF_UNSPEC          =  AF_UNSPEC;
  PF_UNIX            =  AF_UNIX;
  PF_INET            =  AF_INET;
  PF_IMPLINK         =  AF_IMPLINK;
  PF_PUP             =  AF_PUP;
  PF_CHAOS           =  AF_CHAOS;
  PF_NS              =  AF_NS;
  PF_ISO             =  AF_ISO;
  PF_OSI             =  AF_OSI;
  PF_ECMA            =  AF_ECMA;
  PF_DATAKIT         =  AF_DATAKIT;
  PF_CCITT           =  AF_CCITT;
  PF_SNA             =  AF_SNA;
  PF_DECnet          =  AF_DECnet;
  PF_DLI             =  AF_DLI;
  PF_LAT             =  AF_LAT;
  PF_HYLINK          =  AF_HYLINK;
  PF_APPLETALK       =  AF_APPLETALK;
  PF_MAX             =  AF_MAX;

 { Level number for (get/set)sockopt() to apply to socket itself }
 SOL_SOCKET          =  -1;             { options for socket level }

 { Maximum queue length specifiable by listen }
 SOMAXCONN           =   5;

 MSG_OOB             =   1;             { process out-of-band data }
 MSG_PEEK            =   2;             { peek at incoming message }
 MSG_DONTROUTE       =   4;             { send without using routing tables }

 MSG_MAXIOVLEN       =  16;

 { Define constant based on rfc883, used by gethostbyxxxx() calls }
 MAXGETHOSTSTRUCT    =  1024;

 { Define flags to be used with the WSAAsyncSelect() call }
 FD_READ             =  1;
 FD_WRITE            =  2;
 FD_OOB              =  4;
 FD_ACCEPT           =  8;
 FD_CONNECT          =  16;
 FD_CLOSE            =  32;

 { All Windows Sockets error constants are biased by WSABASEERR fromthe normal }
 WSABASEERR          = 10000;

 { Windows Sockets definitions of regular Microsoft C error constants }
 WSAEINTR            = (WSABASEERR+4);
 WSAEBADF            = (WSABASEERR+9);
 WSAEACCES           = (WSABASEERR+13);
 WSAEFAULT           = (WSABASEERR+14);
 WSAEINVAL           = (WSABASEERR+22);
 WSAEMFILE           = (WSABASEERR+24);

 { Windows Sockets definitions of regular Berkeley error constants }
 WSAEWOULDBLOCK      = (WSABASEERR+35);
 WSAEINPROGRESS      = (WSABASEERR+36);
 WSAEALREADY         = (WSABASEERR+37);
 WSAENOTSOCK         = (WSABASEERR+38);
 WSAEDESTADDRREQ     = (WSABASEERR+39);
 WSAEMSGSIZE         = (WSABASEERR+40);
 WSAEPROTOTYPE       = (WSABASEERR+41);
 WSAENOPROTOOPT      = (WSABASEERR+42);
 WSAEPROTONOSUPPORT  = (WSABASEERR+43);
 WSAESOCKTNOSUPPORT  = (WSABASEERR+44);
 WSAEOPNOTSUPP       = (WSABASEERR+45);
 WSAEPFNOSUPPORT     = (WSABASEERR+46);
 WSAEAFNOSUPPORT     = (WSABASEERR+47);
 WSAEADDRINUSE       = (WSABASEERR+48);
 WSAEADDRNOTAVAIL    = (WSABASEERR+49);
 WSAENETDOWN         = (WSABASEERR+50);
 WSAENETUNREACH      = (WSABASEERR+51);
 WSAENETRESET        = (WSABASEERR+52);
 WSAECONNABORTED     = (WSABASEERR+53);
 WSAECONNRESET       = (WSABASEERR+54);
 WSAENOBUFS          = (WSABASEERR+55);
 WSAEISCONN          = (WSABASEERR+56);
 WSAENOTCONN         = (WSABASEERR+57);
 WSAESHUTDOWN        = (WSABASEERR+58);
 WSAETOOMANYREFS     = (WSABASEERR+59);
 WSAETIMEDOUT        = (WSABASEERR+60);
 WSAECONNREFUSED     = (WSABASEERR+61);
 WSAELOOP            = (WSABASEERR+62);
 WSAENAMETOOLONG     = (WSABASEERR+63);
 WSAEHOSTDOWN        = (WSABASEERR+64);
 WSAEHOSTUNREACH     = (WSABASEERR+65);
 WSAENOTEMPTY        = (WSABASEERR+66);
 WSAEPROCLIM         = (WSABASEERR+67);
 WSAEUSERS           = (WSABASEERR+68);
 WSAEDQUOT           = (WSABASEERR+69);
 WSAESTALE           = (WSABASEERR+70);
 WSAEREMOTE          = (WSABASEERR+71);

 { Extended Windows Sockets error constant definitions }
 WSASYSNOTREADY      = (WSABASEERR+91);
 WSAVERNOTSUPPORTED  = (WSABASEERR+92);
 WSANOTINITIALISED   = (WSABASEERR+93);

 { Authoritative Answer: Host not found }
 WSAHOST_NOT_FOUND   = (WSABASEERR+1001);
 HOST_NOT_FOUND      = WSAHOST_NOT_FOUND;

{ Non-Authoritative: Host not found, or SERVERFAIL }
 WSATRY_AGAIN        = (WSABASEERR+1002);
 TRY_AGAIN           = WSATRY_AGAIN;

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }
 WSANO_RECOVERY      = (WSABASEERR+1003);
 NO_RECOVERY         = WSANO_RECOVERY;

{ Valid name, no data record of requested type }
 WSANO_DATA          = (WSABASEERR+1004);
 NO_DATA             = WSANO_DATA;

{ no address, look for MX record }
 WSANO_ADDRESS       = WSANO_DATA;
 NO_ADDRESS          = WSANO_ADDRESS;

{ Windows Sockets errors redefined as regular Berkeley error constants }
 EWOULDBLOCK         = WSAEWOULDBLOCK;
 EINPROGRESS         = WSAEINPROGRESS;
 EALREADY            = WSAEALREADY;
 ENOTSOCK            = WSAENOTSOCK;
 EDESTADDRREQ        = WSAEDESTADDRREQ;
 EMSGSIZE            = WSAEMSGSIZE;
 EPROTOTYPE          = WSAEPROTOTYPE;
 ENOPROTOOPT         = WSAENOPROTOOPT;
 EPROTONOSUPPORT     = WSAEPROTONOSUPPORT;
 ESOCKTNOSUPPORT     = WSAESOCKTNOSUPPORT;
 EOPNOTSUPP          = WSAEOPNOTSUPP;
 EPFNOSUPPORT        = WSAEPFNOSUPPORT;
 EAFNOSUPPORT        = WSAEAFNOSUPPORT;
 EADDRINUSE          = WSAEADDRINUSE;
 EADDRNOTAVAIL       = WSAEADDRNOTAVAIL;
 ENETDOWN            = WSAENETDOWN;
 ENETUNREACH         = WSAENETUNREACH;
 ENETRESET           = WSAENETRESET;
 ECONNABORTED        = WSAECONNABORTED;
 ECONNRESET          = WSAECONNRESET;
 ENOBUFS             = WSAENOBUFS;
 EISCONN             = WSAEISCONN;
 ENOTCONN            = WSAENOTCONN;
 ESHUTDOWN           = WSAESHUTDOWN;
 ETOOMANYREFS        = WSAETOOMANYREFS;
 ETIMEDOUT           = WSAETIMEDOUT;
 ECONNREFUSED        = WSAECONNREFUSED;
 ELOOP               = WSAELOOP;
 ENAMETOOLONG        = WSAENAMETOOLONG;
 EHOSTDOWN           = WSAEHOSTDOWN;
 EHOSTUNREACH        = WSAEHOSTUNREACH;
 ENOTEMPTY           = WSAENOTEMPTY;
 EPROCLIM            = WSAEPROCLIM;
 EUSERS              = WSAEUSERS;
 EDQUOT              = WSAEDQUOT;
 ESTALE              = WSAESTALE;
 EREMOTE             = WSAEREMOTE;

type
  { Basic system type definitions, taken from the BSD file sys/types.h. }
  u_char   = byte;
  u_short  = word;
  u_int    = word;
  u_long   = longint;
  short    = word;

  { low level handle wich refer to sockets }
  TSocket = u_int;

  { Select uses arrays of SOCKETs. }
  TFDSet = packed record
    fd_count : u_short;
    fd_array : array [0..(FD_SETSIZE - 1)] of TSocket;
  end;
  PFDSet = ^TFDSet;

  {  Structure used in select() call, taken from the BSD file sys/time.h. }
  TTimeVal = packed record
    tv_sec  : longint;
    tv_usec : longint;
  end;
  PTimeVal = ^TTimeVal;

  { Structures returned by network data base library, taken from the
    BSD file netdb.h.  All addresses are supplied in host order, and
    returned in network order (suitable for use in system calls). }

  HostEnt = record
    h_name      : PChar;          { official name of host }
    h_aliases   : ^PChar;         { alias list }
    h_addrtype  : short;          { host address type }
    h_length    : short;          { length of address }
    h_addr_list : ^PChar;         { list of addresses }
  end;
  PHostEnt = ^HostEnt;

  NetEnt = record
    n_name     : PChar;           { official name of net }
    n_aliases  : ^PChar;          { alias list }
    n_addrtype : short;           { net address type }
    n_net      : u_long;          {  network # }
  end;
  PNetEnt = ^NetEnt;

  ServEnt = record
    s_name    : PChar;            { official service name }
    s_aliases : ^PChar;           { alias list }
    s_port    : integer;          { port # }
    s_proto   : PChar;            { protocol to use }
  end;
  PServEnt = ^ServEnt;

  Protoent = record
    p_name    : PChar;            { official protocol name }
    p_aliases : ^PChar;           { alias list }
    p_proto   : integer;          { protocol # }
  end;
  Pprotoent = ^protoent;

  {  Internet address (old style... should be updated) }
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;

  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  TInAddr = packed record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  PInAddr = ^TInAddr;

  { Socket address, internet style. }
  TSockAddrIn = packed record
    case integer of
    0: (sin_family : u_short;
        sin_port   : u_short;
        sin_addr   : TInAddr;
        sin_zero   : array[0..7] of char);
    1: (sa_family: u_short;
        sa_data: array [0..13] of char);
  end;
  PSockAddrIn = ^TSockAddrIn;
  TSockAddr   = TSockAddrIn;

  PWSADATA = ^TWSADATA;
  TWSADATA = packed record
    wVersion       : word;
    wHighVersion   : word;
    szDescription  : array [0..WSADESCRIPTION_LEN] of char;
    szSystemStatus : array [0..WSASYS_STATUS_LEN] of char;
    iMaxSockets    : u_short;
    iMaxUdpDg      : u_short;
    lpVendorInfo   : PChar;
  end;

  { Structure used by kernel to pass protocol information in raw sockets. }
  TSockProto = packed record
    sp_family   : u_short;
    sp_protocol : u_short;
  end;

  {  Structure used for manipulating linger option. }
  TLinger = packed record
    l_onoff  : u_short;
    l_linger : u_short;
  end;

{ Socket function prototypes }

function accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
function closesocket(s: TSocket): Integer;
function connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer;
function ioctlsocket(s: TSocket; cmd: Longint; var arg: u_long): Integer;
function getpeername(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function getsockopt(s: TSocket; level, optname: Integer; optval: PChar; var optlen: Integer): Integer;
function htonl(hostlong: u_long): u_long;
function htons(hostshort: u_short): u_short;
function inet_addr(cp: PChar): u_long;
function inet_ntoa(inaddr: TInAddr): PChar;
function listen(s: TSocket; backlog: Integer): Integer; 
function ntohl(netlong: u_long): u_long; 
function ntohs(netshort: u_short): u_short; 
function recv(s: TSocket; var Buf; len, flags: Integer): Integer;
function recvfrom(s: TSocket; var Buf; len, flags: Integer;
  var from: TSockAddr; var fromlen: Integer): Integer; 
function select(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): Longint;
function send(s: TSocket; var Buf; len, flags: Integer): Integer; 
function sendto(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr;
  tolen: Integer): Integer; 
function setsockopt(s: TSocket; level, optname: Integer; optval: PChar;
  optlen: Integer): Integer; 
function shutdown(s: TSocket; how: Integer): Integer; 
function socket(af, struct, protocol: Integer): TSocket;
function gethostbyaddr(addr: Pointer; len, struct: Integer): PHostEnt;
function gethostbyname(name: PChar): PHostEnt;
function gethostname(name: PChar; len: Integer): Integer;
function getservbyport(port: Integer; proto: PChar): PServEnt;
function getservbyname(name, proto: PChar): PServEnt;
function getprotobynumber(proto: Integer): PProtoEnt;
function getprotobyname(name: PChar): PProtoEnt;
function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Integer; 
function WSACleanup: Integer; 
procedure WSASetLastError(iError: Integer); 
function WSAGetLastError: Integer;
function WSAIsBlocking: BOOL; 
function WSAUnhookBlockingHook: Integer;
function WSASetBlockingHook(lpBlockFunc: TFarProc): TFarProc;
function WSACancelBlockingCall: Integer;
function WSAAsyncGetServByName(HWindow: HWND; wMsg: u_int; 
  name, proto, buf: PChar; buflen: Integer): THandle; 
function WSAAsyncGetServByPort( HWindow: HWND; wMsg, port: u_int;
  proto, buf: PChar; buflen: Integer): THandle; 
function WSAAsyncGetProtoByName(HWindow: HWND; wMsg: u_int;
  name, buf: PChar; buflen: Integer): THandle; 
function WSAAsyncGetProtoByNumber(HWindow: HWND; wMsg: u_int; number: Integer;
  buf: PChar; buflen: Integer): THandle;
function WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
  name, buf: PChar; buflen: Integer): THandle;
function WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PChar;
  len, struct: Integer; buf: PChar; buflen: Integer): THandle;
function WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
function WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
function WSARecvEx(s: TSocket; var buf; len: Integer; var flags: Integer): Integer;
function WSAMakeSyncReply(Buflen, Error: Word): Longint;
function WSAMakeSelectReply(Event, Error: Word): Longint;
function WSAGetAsyncBuflen(Param: Longint): Word;
function WSAGetAsyncError(Param: Longint): Word;
function WSAGetSelectEvent(Param: Longint): Word;
function WSAGetSelectError(Param: Longint): Word;


implementation

function WSAMakeSyncReply;
begin
  WSAMakeSyncReply:= MakeLong(Buflen, Error);
end;

function WSAMakeSelectReply;
begin
  WSAMakeSelectReply:= MakeLong(Event, Error);
end;

function WSAGetAsyncBuflen;
begin
  WSAGetAsyncBuflen:= LOWORD(Param);
end;

function WSAGetAsyncError;
begin
  WSAGetAsyncError:= HIWORD(Param);
end;

function WSAGetSelectEvent;
begin
  WSAGetSelectEvent:= LOWORD(Param);
end;

function WSAGetSelectError;
begin
  WSAGetSelectError:= HIWORD(Param);
end;

function accept;            external    winsocket name 'accept';
function bind;              external    winsocket name 'bind';
function closesocket;       external    winsocket name 'closesocket';
function connect;           external    winsocket name 'connect';
function getpeername;       external    winsocket name 'getpeername';
function getsockname;       external    winsocket name 'getsockname';
function getsockopt;        external    winsocket name 'getsockopt';
function htonl;             external    winsocket name 'htonl';
function htons;             external    winsocket name 'htons';
function inet_addr;         external    winsocket name 'inet_addr';
function inet_ntoa;         external    winsocket name 'inet_ntoa';
function ioctlsocket;       external    winsocket name 'ioctlsocket';
function listen;            external    winsocket name 'listen';
function ntohl;             external    winsocket name 'ntohl';
function ntohs;             external    winsocket name 'ntohs';
function recv;              external    winsocket name 'recv';
function recvfrom;          external    winsocket name 'recvfrom';
function select;            external    winsocket name 'select';
function send;              external    winsocket name 'send';
function sendto;            external    winsocket name 'sendto';
function setsockopt;        external    winsocket name 'setsockopt';
function shutdown;          external    winsocket name 'shutdown';
function socket;            external    winsocket name 'socket';

function gethostbyaddr;     external    winsocket name 'gethostbyaddr';
function gethostbyname;     external    winsocket name 'gethostbyname';
function getprotobyname;    external    winsocket name 'getprotobyname';
function getprotobynumber;  external    winsocket name 'getprotobynumber';
function getservbyname;     external    winsocket name 'getservbyname';
function getservbyport;     external    winsocket name 'getservbyport';
function gethostname;       external    winsocket name 'gethostname';

function WSAAsyncSelect;        external winsocket name 'WSAAsyncSelect';
function WSARecvEx;             external winsocket name 'WSARecvEx';
function WSAAsyncGetHostByAddr; external winsocket name 'WSAAsyncGetHostByAddr';
function WSAAsyncGetHostByName; external winsocket name 'WSAAsyncGetHostByName';
function WSAAsyncGetProtoByNumber; external winsocket name 'WSAAsyncGetProtoByNumber';
function WSAAsyncGetprotoByName; external winsocket name 'WSAAsyncGetprotoByName';
function WSAAsyncGetServByPort; external winsocket name 'WSAAsyncGetServByPort';
function WSAAsyncGetServByName; external winsocket name 'WSAAsyncGetServByName';
function WSACancelAsyncRequest; external winsocket name 'WSACancelAsyncRequest';
function WSASetBlockingHook;    external winsocket name 'WSASetBlockingHook';
function WSAUnhookBlockingHook; external winsocket name 'WSAUnhookBlockingHook';
function WSAGetLastError;       external winsocket name 'WSAGetLastError';
procedure WSASetLastError;      external winsocket name 'WSASetLastError';
function WSACancelBlockingCall; external winsocket name 'WSACancelBlockingCall';
function WSAIsBlocking;         external winsocket name 'WSAIsBlocking';
function WSAStartup;            external winsocket name 'WSAStartup';
function WSACleanup;            external winsocket name 'WSACleanup';

end.

