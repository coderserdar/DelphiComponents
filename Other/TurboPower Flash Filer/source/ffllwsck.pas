{*********************************************************}
{* FlashFiler: Low-level Winsock implementation          *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

{$IFDEF CBuilder3}
(*$HPPEMIT '' *)
(*$HPPEMIT '#undef h_addr' *)
(*$HPPEMIT '' *)
{$ENDIF}

{ Use the following DEFINE to force loading of Winsock 1 }
{.$DEFINE ForceWinSock1}

unit ffllwsck;

interface

uses
  Classes,
  Windows,
  Messages,
  SysUtils,
  ffconst,
  ffllwsct,
  ffllbase,
  ffsrmgr,
  ffllexcp;

{$R ffwscnst.res}

const
  ffwscEventComplete = WM_USER + $0FF1;

{===Standard Winsock constants===}
const
  Fd_SETSIZE = 64;

  IocPARM_MASK = $7F;
  Ioc_VOID     = $20000000;
  Ioc_OUT      = $40000000;
  Ioc_IN       = $80000000;
  Ioc_INOUT    = (Ioc_IN or Ioc_OUT);

  { Protocols }

  IpPROTO_IP   = 0;
  IpPROTO_ICMP = 1;
  IpPROTO_GGP  = 2;
  IpPROTO_TCP  = 6;
  IpPROTO_PUP  = 12;
  IpPROTO_UDP  = 17;
  IpPROTO_IDP  = 22;
  IpPROTO_ND   = 77;

  IpPROTO_RAW  = 255;
  IpPROTO_MAX  = 256;

  { Port/socket numbers: network standard functions}

  IpPORT_ECHO       = 7;
  IpPORT_DISCARD    = 9;
  IpPORT_SYSTAT     = 11;
  IpPORT_DAYTIME    = 13;
  IpPORT_NETSTAT    = 15;
  IpPORT_FTP        = 21;
  IpPORT_TELNET     = 23;
  IpPORT_SMTP       = 25;
  IpPORT_TIMESERVER = 37;
  IpPORT_NAMESERVER = 42;
  IpPORT_WHOIS      = 43;
  IpPORT_MTP        = 57;

  { Port/socket numbers: host specific functions }

  IpPORT_TFTP    = 69;
  IpPORT_RJE     = 77;
  IpPORT_FINGER  = 79;
  IpPORT_TTYLINK = 87;
  IpPORT_SUPDUP  = 95;

  { UNIX TCP sockets }

  IpPORT_EXECSERVER  = 512;
  IpPORT_LOGINSERVER = 513;
  IpPORT_CMDSERVER   = 514;
  IpPORT_EFSSERVER   = 520;

  { UNIX UDP sockets }

  IpPORT_BIFFUDP     = 512;
  IpPORT_WHOSERVER   = 513;
  IpPORT_ROUTESERVER = 520;

  { Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root). }

  IpPORT_RESERVED = 1024;

  { Link numbers }

  ImpLINK_IP        = 155;
  ImpLINK_LOWEXPER  = 156;
  ImpLINK_HIGHEXPER = 158;

  { Get # bytes to read }
  FIoNREAD = Ioc_OUT or ((longint(SizeOf(longint)) and IocPARM_MASK) shl 16) or
    (longint(Byte('f')) shl 8) or 127;

  { Set/Clear non-blocking i/o }
  FIoNBIO  = Ioc_IN or((longint(SizeOf(longint)) and IocPARM_MASK) shl 16) or
    (longint(Byte('f')) shl 8) or 126;

  { Set/Clear async i/o }
  FIoASYNC = Ioc_IN or ((longint(SizeOf(longint)) and IocPARM_MASK) shl 16) or
    (longint(Byte('f')) shl 8) or 125;

  InAddr_ANY       = $00000000;
  InAddr_LOOPBACK  = $7F000001;
  InAddr_BROADCAST = $FFFFFFFF;
  InAddr_NONE      = $FFFFFFFF;

  WsaDESCRIPTION_LEN = 256;
  WsaSYS_STATUS_LEN  = 128;
  WsaProtocolLen = 255;
  WsaMaxProtocolChain = 7;

  { Options for use with (get/set)sockopt at the IP level. }

  Ip_OPTIONS         = 1;
  Ip_MULTICAST_IF    = 2;           { set/get IP multicast interface   }
  Ip_MULTICAST_TTL   = 3;           { set/get IP multicast timetolive  }
  Ip_MULTICAST_LOOP  = 4;           { set/get IP multicast loopback    }
  Ip_ADD_MEMBERSHIP  = 5;           { add  an IP group membership      }
  Ip_DROP_MEMBERSHIP = 6;           { drop an IP group membership      }

  Ip_DEFAULT_MULTICAST_TTL  = 1;    { normally limit m'casts to 1 hop  }
  Ip_DEFAULT_MULTICAST_LOOP = 1;    { normally hear sends if a member  }
  Ip_MAX_MEMBERSHIPS        = 20;   { per socket; must fit in one mbuf }

  Ipx_ADDRESS = $4007;              { querying IPX info }

  Invalid_SOCKET = -1;
  Socket_ERROR   = -1;

  { Types }

  Sock_STREAM    = 1;               { stream socket }
  Sock_DGRAM     = 2;               { datagram socket }
  Sock_RAW       = 3;               { raw-protocol interface }
  Sock_RDM       = 4;               { reliably-delivered message }
  Sock_SEQPACKET = 5;               { sequenced packet stream }

  { Option flags per-socket. }

  So_DEBUG       = $0001;           { turn on debugging info recording }
  So_ACCEPTCONN  = $0002;           { socket has had listen() }
  So_REUSEADDR   = $0004;           { allow local address reuse }
  So_KEEPALIVE   = $0008;           { keep connections alive }
  So_DONTROUTE   = $0010;           { just use interface addresses }
  So_BROADCAST   = $0020;           { permit sending of broadcast msgs }
  So_USELOOPBACK = $0040;           { bypass hardware when possible }
  So_LINGER      = $0080;           { linger on close if data present }
  So_OOBINLINE   = $0100;           { leave received OOB data in line }

  So_DONTLINGER  = $FF7F;

  { Additional options. }

  So_SNDBUF   = $1001;              { send buffer size }
  So_RCVBUF   = $1002;              { receive buffer size }
  So_SNDLOWAT = $1003;              { send low-water mark }
  So_RCVLOWAT = $1004;              { receive low-water mark }
  So_SNDTIMEO = $1005;              { send timeout }
  So_RCVTIMEO = $1006;              { receive timeout }
  So_ERROR    = $1007;              { get error status and clear }
  So_TYPE     = $1008;              { get socket type }

  { Options for connect and disconnect data and options.  Used only by
    non-TCP/IP transports such as DECNet, OSI TP4, etc. }

  So_CONNDATA    = $7000;
  So_CONNOPT     = $7001;
  So_DISCDATA    = $7002;
  So_DISCOPT     = $7003;
  So_CONNDATALEN = $7004;
  So_CONNOPTLEN  = $7005;
  So_DISCDATALEN = $7006;
  So_DISCOPTLEN  = $7007;

  { Option for opening sockets for synchronous access. }

  So_OPENTYPE    = $7008;

  So_SYNCHRONOUS_ALERT    = $10;
  So_SYNCHRONOUS_NONALERT = $20;

  { Other NT-specific options. }

  So_MAXDG     = $7009;
  So_MAXPATHDG = $700A;

  { TCP options. }

  TCP_NoDELAY   = $0001;
  TCP_BsdURGENT = $7000;

  { Address families. }

  Af_UNSPEC    = 0;                 { unspecified }
  Af_UNIX      = 1;                 { local to host (pipes, portals) }
  Af_INET      = 2;                 { internetwork: UDP, TCP, etc. }
  Af_IMPLINK   = 3;                 { arpanet imp addresses }
  Af_PUP       = 4;                 { pup protocols: e.g. BSP }
  Af_CHAOS     = 5;                 { mit CHAOS protocols }
  Af_IPX       = 6;                 { IPX and SPX }
  Af_NS        = 6;                 { XEROX NS protocols }
  Af_ISO       = 7;                 { ISO protocols }
  Af_OSI       = Af_ISO;            { OSI is ISO }
  Af_ECMA      = 8;                 { european computer manufacturers }
  Af_DATAKIT   = 9;                 { datakit protocols }
  Af_CCITT     = 10;                { CCITT protocols, X.25 etc }
  Af_SNA       = 11;                { IBM SNA }
  Af_DECnet    = 12;                { DECnet }
  Af_DLI       = 13;                { Direct data link interface }
  Af_LAT       = 14;                { LAT }
  Af_HYLINK    = 15;                { NSC Hyperchannel }
  Af_APPLETALK = 16;                { AppleTalk }
  Af_NETBIOS   = 17;                { NetBios-style addresses }
  Af_VOICEVIEW = 18;                { VoiceView }
  Af_MAX       = 19;

  { Protocol families, same as address families for now. }

  Pf_UNSPEC    = Af_UNSPEC;
  Pf_UNIX      = Af_UNIX;
  Pf_INET      = Af_INET;
  Pf_IMPLINK   = Af_IMPLINK;
  Pf_PUP       = Af_PUP;
  Pf_CHAOS     = Af_CHAOS;
  Pf_NS        = Af_NS;
  Pf_IPX       = Af_IPX;
  Pf_ISO       = Af_ISO;
  Pf_OSI       = Af_OSI;
  Pf_ECMA      = Af_ECMA;
  Pf_DATAKIT   = Af_DATAKIT;
  Pf_CCITT     = Af_CCITT;
  Pf_SNA       = Af_SNA;
  Pf_DECnet    = Af_DECnet;
  Pf_DLI       = Af_DLI;
  Pf_LAT       = Af_LAT;
  Pf_HYLINK    = Af_HYLINK;
  Pf_APPLETALK = Af_APPLETALK;
  Pf_VOICEVIEW = Af_VOICEVIEW;

  Pf_MAX       = Af_MAX;

  { Level number for (get/set)sockopt() to apply to socket itself. }

  Sol_SOCKET = $FFFF;               {options for socket level }

  { Maximum queue length specifiable by listen. }

  SoMAXCONN     = 5;

  Msg_OOB       = $1;               {process out-of-band data }
  Msg_PEEK      = $2;               {peek at incoming message }
  Msg_DONTROUTE = $4;               {send without using routing tables }

  Msg_MAXIOVLEN = 16;

  Msg_PARTIAL   = $8000;            {partial send or recv for message xport }

  { Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MaxGETHOSTSTRUCT = 1024;

  { Define flags to be used with the WSAAsyncSelect() call. }

  Fd_READ    = $01;
  Fd_WRITE   = $02;
  Fd_OOB     = $04;
  Fd_ACCEPT  = $08;
  Fd_CONNECT = $10;
  Fd_CLOSE   = $20;

  { Protocols for IPX/SPX }

  NSPROTO_IPX   = 1000;
  NSPROTO_SPX   = 1256;
  NSPROTO_SPXII = 1257;

type
  EffWinsockException = class(EffCommsException)
    public
      constructor CreateTranslate(aErrorCode : integer;
                                  aDummy     : pointer);
  end;

{===FF Winsock types===}
type
  TffWinsockFamily = (         {the Winsock family types we support}
                      wfTCP,   {..TCP/IP}
                      wfIPX);  {..IPX/SPX}

  TffWinsockFamilies = set of TffWinsockFamily;

  { The following record type is used to track Winsock versions supported
    by this module. }
  TffWinsockVerRec = record
    VerNum : Word;
    ModuleName : array[0..12] of AnsiChar;
  end;

  TffwsWinsockVersion = (ffwvNone, ffwvWinSock1, ffwvWinSock2);
    { Identifies the winsock version we have loaded in FFWSInstalled. }


{===Standard Winsock types===}
type
  TffwsSocket = integer;           {a Winsock socket}

  PffwsFDSet = ^TffwsFDSet;
  TffwsFDSet = packed record       {an array of sockets}
    fd_count : integer;
    fd_array : array [0..pred(FD_SETSIZE)] of TffwsSocket;
  end;

  PffwsTimeVal = ^TffwsTimeVal;
  TffwsTimeVal = packed record     {a time value}
    tv_sec  : longint;
    tv_usec : longint;
  end;

  PffwsHostEnt = ^TffwsHostEnt;
  TffwsHostEnt = packed record     {host entity}
    h_name    : PAnsiChar;
    h_aliases : ^PAnsiChar;
    h_addrtype: smallint;
    h_length  : smallint;
    case byte of
      0: (h_addr_list: ^PAnsiChar);
      1: (h_Addr     : ^PAnsiChar)
  end;

  PffwsNetEnt = ^TffwsNetEnt;
  TffwsNetEnt = packed record      {network entity}
    n_name    : PAnsiChar;
    n_aliases : ^PAnsiChar;
    n_addrtype: smallint;
    n_net     : longint;
  end;

  PffwsServEnt = ^TffwsServEnt;
  TffwsServEnt = packed record     {server entity}
    s_name   : PAnsiChar;
    s_aliases: ^PAnsiChar;
    s_port   : smallint;
    s_proto  : PAnsiChar;
  end;

  PffwsProtoEnt = ^TffwsProtoEnt;
  TffwsProtoEnt = packed record   {protocol entity}
    p_name   : PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto  : smallint;
  end;

  PffwsInAddr = ^TffwsInAddr;
  TffwsInAddr = TffWord32;

  PffwsSockAddrIn = ^TffwsSockAddrIn;
  TffwsSockAddrIn = packed record
    sin_family: word;
    sin_port  : word;
    sin_addr  : TffwsInAddr;
    sin_zero  : array [0..7] of AnsiChar;
  end;

  PffwsIPXAddr = ^TffwsIPXAddr;
  TffwsIPXAddr = array [0..5] of byte;

  PffwsIPXNetNum = ^TffwsIPXNetNum;
  TffwsIPXNetNum = array [0..3] of byte;

  PffwsSockAddrIPX = ^TffwsSockAddrIPX;
  TffwsSockAddrIPX = packed record
    sipx_family  : word;
    sipx_netnum  : TffwsIPXNetNum;
    sipx_nodenum : TffwsIPXAddr;
    sipx_socket  : word;
  end;

  { Structure used by kernel to store most addresses. }
  PffwsSockAddr = ^TffwsSockAddr;
  TffwsSockAddr = record
    case integer of
      0 : (TCP : TffwsSockAddrIn);
      1 : (IPX : TffwsSockAddrIPX);
  end;

  PffWSAData = ^TffWSAData;
  TffWSAData = packed record
    wVersion      : word;
    wHighVersion  : word;
    szDescription : array [0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array [0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets   : word;
    iMaxUdpDg     : word;
    lpVendorInfo  : PAnsiChar;
  end;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PffwsSockProto = ^TffwsSockProto;
  TffwsSockProto = packed record
    sp_family   : word;
    sp_protocol : word;
  end;

  { Structure used for manipulating linger option. }
  PffwsLinger = ^TffwsLinger;
  TffwsLinger = packed record
    l_onoff  : word;
    l_linger : word;
  end;

  {structure for querying IPX address info (from NWLINK.H)}
  PffwsIPXAddrInfo = ^TffwsIPXAddrInfo;
  TffwsIPXAddrInfo = packed record
    adapternum : integer;              {input: 0-based adapter number}
    netnum     : TffwsIPXNetNum;       {output: IPX network number}
    nodenum    : TffwsIPXAddr;         {output: IPX node address}
    wan        : boolean;              {output: TRUE = adapter is on a wan link}
    status     : boolean;              {output: TRUE = wan link is up (or adapter is not wan)}
    maxpkt     : integer;              {output: max packet size, not including IPX header}
    linkspeed  : longint;              {output: link speed in 100 bytes/sec (i.e. 96 == 9600)}
  end;

  TffwsProtocolChain = packed record
    chainLen: Integer;  { The length of the chain:
                          0 -> layered protocol,
                          1 -> base protocol,
                          > 1 -> protocol chain }
    chainEntries: Array[0..WsaMaxProtocolChain - 1] of DWORD;
  end;

  { Structure for retrieving protocol information. }
  PffwsProtocolInfo = ^TffwsProtocolInfo;
  TffwsProtocolInfo = packed record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: TffwsProtocolChain;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: Array[0..WsaProtocolLen] of AnsiChar;
  end;

  { Socket function types }
  tffwsrAccept =
     function(S : TffwsSocket; var Addr : TffwsSockAddr; var Addrlen : integer) : TffwsSocket
             stdcall;
  tffwsrBind =
     function(S : TffwsSocket; var Addr : TffwsSockAddr; NameLen : integer) : integer
              stdcall;
  tffwsrCloseSocket =
     function(S : TffwsSocket) : integer
              stdcall;
  tffwsrConnect =
     function(S : TffwsSocket; var Name : TffwsSockAddr; NameLen : integer) : integer
             stdcall;
  tffwsrEnumProtocols =
     function( Protocols : PInteger; aBuffer : PffwsProtocolInfo;
               var BufferLength : DWORD ) : Integer; stdcall;
  tffwsrIOCtlSocket =
     function(S : TffwsSocket; Cmd : longint; var Arg : longint) : integer
             stdcall;
  tffwsrGetPeerName =
     function(S : TffwsSocket; var Name : TffwsSockAddr; var NameLen : integer): integer
             stdcall;
  tffwsrGetSockName =
     function(S : TffwsSocket; var Name : TffwsSockAddr; var NameLen : integer): integer
             stdcall;
  tffwsrGetSockOpt =
     function(S : TffwsSocket; Level, OptName : integer;
              var OptVal; var OptLen: integer): integer
             stdcall;
  tffwsrhtonl =
     function(HostLong : longint) : longint
             stdcall;
  tffwsrhtons =
     function(HostShort : word) : word
             stdcall;
  tffwsrINet_Addr =
     function(Cp : PAnsiChar) : dword                                  {!!.11}
             stdcall;
  tffwsrINet_NtoA =
     function(InAddr : TffwsInAddr) : PAnsiChar
             stdcall;
  tffwsrListen =
     function(S : TffwsSocket; Backlog : integer) : integer
             stdcall;
  tffwsrntohl =
     function(NetLong : longint) : longint
             stdcall;
  tffwsrntohs =
     function(NetShort : word) : word
             stdcall;
  tffwsrRecv =
     function(S : TffwsSocket; var Buf; Len, Flags : integer) : integer
             stdcall;
  tffwsrRecvFrom =
     function(S : TffwsSocket; var Buf; Len, Flags : integer;
              var From: TffwsSockAddr; var FromLen : integer) : integer
              stdcall;
  tffwsrSelect =
     function(Nfds : integer; Readfds, Writefds,
              Exceptfds : PffwsFDSet; Timeout : PffwsTimeVal) : longint
              stdcall;
  tffwsrSend =
     function(S : TffwsSocket; var Buf; Len, Flags : integer) : integer
             stdcall;
  tffwsrSendTo =
     function(S : TffwsSocket; var Buf; Len, Flags : integer;
              var AddrTo : TffwsSockAddr; ToLen : integer) : integer
              stdcall;
  tffwsrSetSockOpt =
     function(S : TffwsSocket; Level, OptName : integer;
              var OptVal; OptLen : integer) : integer
             stdcall;
  tffwsrShutdown =
     function(S : TffwsSocket; How : integer) : integer
             stdcall;
  tffwsrSocket =
     function(Af, Struct, Protocol : integer) : TffwsSocket
             stdcall;
  tffwsrGetHostByAddr =
     function(var Addr; Len, Struct : integer): PffwsHostEnt
             stdcall;
  tffwsrGetHostByName =
     function(Name : PAnsiChar) : PffwsHostEnt
             stdcall;
  tffwsrGetHostName =
     function(Name : PAnsiChar; Len : integer): integer
             stdcall;
  tffwsrGetServByPort =
     function(Port : integer; Proto : PAnsiChar) : PffwsServEnt
             stdcall;
  tffwsrGetServByName =
     function(Name, Proto : PAnsiChar) : PffwsServEnt
             stdcall;
  tffwsrGetProtoByNumber =
     function(Proto : integer) : PffwsProtoEnt
             stdcall;
  tffwsrGetProtoByName =
     function(Name : PAnsiChar) : PffwsProtoEnt
             stdcall;
  tffwsrWSAStartup =
     function(wVersionRequired : word; var WSData : TffWSAData) : integer
             stdcall;
  tffwsrWSACleanup =
     function : integer
             stdcall;
  tffwsrWSASetLastError =
     procedure(iError : integer)
             stdcall;
  tffwsrWSAGetLastError =
     function : integer
             stdcall;
  tffwsrWSAIsBlocking =
     function : BOOL
             stdcall;
  tffwsrWSAUnhookBlockingHook =
     function : integer
             stdcall;
  tffwsrWSASetBlockingHook =
     function(lpBlockFunc : TFarProc) : TFarProc
             stdcall;
  tffwsrWSACancelBlockingCall =
     function : integer
             stdcall;
  tffwsrWSAAsyncGetServByName =
     function(HWindow : HWnd; wMsg : integer;
              Name, Proto, Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSAAsyncGetServByPort =
     function(HWindow : HWnd; wMsg, Port : integer;
              Proto, Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSAAsyncGetProtoByName =
     function(HWindow : HWnd; wMsg : integer;
              Name, Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSAAsyncGetProtoByNumber =
     function(HWindow : HWnd; wMsg : integer; Number : integer;
              Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSAAsyncGetHostByName =
     function(HWindow : HWnd; wMsg : integer;
              Name, Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSAAsyncGetHostByAddr =
     function(HWindow : HWnd; wMsg : integer; Addr : PAnsiChar;
              Len, Struct : integer; Buf : PAnsiChar; BufLen : integer) : THandle
             stdcall;
  tffwsrWSACancelAsyncRequest =
     function(hAsyncTaskHandle : THandle) : integer
             stdcall;
  tffwsrWSAAsyncSelect =
     function(S : TffwsSocket; HWindow : HWnd; wMsg : integer; lEvent : longint) : integer
             stdcall;

type
  PffWinsockRoutines = ^TffWinsockRoutines;
  TffWinsockRoutines = record  {record of Winsock function pointers}
    accept                   : tffwsrAccept;
    bind                     : tffwsrBind;
    closesocket              : tffwsrCloseSocket;
    connect                  : tffwsrConnect;
    ioctlsocket              : tffwsrIOCtlSocket;
    getpeername              : tffwsrGetPeerName;
    getsockname              : tffwsrGetSockName;
    getsockopt               : tffwsrGetSockOpt;
    htonl                    : tffwsrhtonl;
    htons                    : tffwsrhtons;
    inet_addr                : tffwsrINet_Addr;
    inet_ntoa                : tffwsrINet_Ntoa;
    listen                   : tffwsrListen;
    ntohl                    : tffwsrntohl;
    ntohs                    : tffwsrntohs;
    recv                     : tffwsrRecv;
    recvfrom                 : tffwsrRecvFrom;
    select                   : tffwsrSelect;
    send                     : tffwsrSend;
    sendTo                   : tffwsrSendTo;
    setsockopt               : tffwsrSetSockOpt;
    shutdown                 : tffwsrShutdown;
    socket                   : tffwsrSocket;
    gethostbyaddr            : tffwsrGetHostByAddr;
    gethostbyname            : tffwsrGetHostByName;
    gethostname              : tffwsrGetHostName;
    getservbyport            : tffwsrGetServByPort;
    getservbyname            : tffwsrGetServByName;
    getprotobynumber         : tffwsrGetProtoByNumber;
    getprotobyname           : tffwsrGetProtoByName;
    WSAStartup               : tffwsrWSAStartup;
    WSACleanup               : tffwsrWSACleanup;
    WSAEnumProtocols         : tffwsrEnumProtocols;
    WSASetLastError          : tffwsrWSASetLastError;
    WSAGetLastError          : tffwsrWSAGetLastError;
    WSAIsBlocking            : tffwsrWSAIsBlocking;
    WSAUnhookBlockingHook    : tffwsrWSAUnhookBlockingHook;
    WSASetBlockingHook       : tffwsrWSASetBlockingHook;
    WSACancelBlockingCall    : tffwsrWSACancelBlockingCall;
    WSAAsyncGetServByName    : tffwsrWSAAsyncGetServByName;
    WSAAsyncGetServByPort    : tffwsrWSAAsyncGetServByPort;
    WSAAsyncGetProtoByName   : tffwsrWSAAsyncGetProtoByName;
    WSAAsyncGetProtoByNumber : tffwsrWSAAsyncGetProtoByNumber;
    WSAAsyncGetHostByName    : tffwsrWSAAsyncGetHostByName;
    WSAAsyncGetHostByAddr    : tffwsrWSAAsyncGetHostByAddr;
    WSACancelAsyncRequest    : tffwsrWSACancelAsyncRequest;
    WSAAsyncSelect           : tffwsrWSAAsyncSelect;
  end;

var
  WinsockRoutines : TffWinsockRoutines;
  ffwsFamiliesInstalled : TffWinsockFamilies;

function FFWSInstalled : boolean;
  {-Returns true if Winsock is installed}

function WSAMakeSyncReply(Buflen, Error : word) : longint;
  {-Construct the response to a WSAAsyncGetXByY routine}
function WSAMakeSelectReply(Event, Error : word) : longint;
  {-Construct the response to WSAAsyncSelect}
function WSAGetAsyncBuflen(lParam : longint) : integer;
  {-Extract the buffer length from lParam in response to a WSAGetXByY}
function WSAGetAsyncError(lParam : longint) : integer;
  {-Extract the error from lParam in response to a WSAGetXByY}
function WSAGetSelectEvent(lParam : longint) : integer;
  {-Extract the event from lParam in response to a WSAAsyncSelect}
function WSAGetSelectError(lParam : longint) : integer;
  {-Extract the error from lParam in response to a WSAAsyncSelect}

{===FlashFiler helper routines===}
procedure FFWSAsyncSelect(aSocket : TffwsSocket;
                          aWindow : HWnd;
                          aEvent  : longint);
function FFWSCreateSocket(aAF, aStruct, aProtocol : integer) : TffwsSocket;
function FFWSCvtAddrToStr(aAddr : TffwsInAddr) : TffNetName;
function FFWSCvtIPXAddrToStr(const aNetNum : TffwsIPXNetNum;
                             const aAddr   : TffwsIPXAddr) : TffNetName;
function FFWSCvtStrToAddr(aStr : TffNetName; var aAddr : TffwsInAddr) : boolean;
function FFWSCvtStrToIPXAddr(const aStr    : TffNetName;
                               var aNetNum : TffwsIPXNetNum;
                               var aAddr   : TffwsIPXAddr) : boolean;
procedure FFWSDestroySocket(aSocket : TffwsSocket);
function FFWSGetLocalHosts(aList : TStrings) : Boolean;
function FFWSGetLocalHostByNum(const NIC      : Integer;
                                 var aNetName : TffNetName;
                                 var aAddr   : TffwsInAddr) : Boolean;
function FFWSGetLocalIPXAddr(var aNetNum : TffwsIPXNetNum;
                             var aAddr   : TffwsIPXAddr) : boolean;
function FFWSGetRemoteHost(const aName : TffNetName;
                             var aNetName : TffNetName; var aAddr : TffwsInAddr) : boolean;
function FFWSGetRemoteNameFromAddr(aAddr : TffwsInAddr) : TffNetName;
procedure FFWSGetSocketOption(aSocket : TffwsSocket; aLevel, aOptName : integer;
                         var aOptValue; aOptValueLen : integer);
procedure FFWSSetSocketOption(aSocket : TffwsSocket; aLevel, aOptName : integer;
                         var aOptValue; aOptValueLen : integer);

const
  ffcNumWinsockVersions = 2;
    { Number of supported Winsock versions. }

var
  ffStrResWinsock : TffStringResource; {in FFWSCNST.RC}

  { This array defines the Winsock versions supported by this module. }
  ffWinsockVerArray : array[1..ffcNumWinsockVersions] of TffWinsockVerRec =
    ((VerNum : $0101; ModuleName : 'wsock32.dll'),         { WinSock 1 }
     (VerNum : $0202; ModuleName : 'ws2_32.dll'));         { WinSock 2 }


implementation

var
  UnitInitializationDone : boolean;
  ffwsLoadedWinsockVersion : TffwsWinsockVersion;
  WSLibHandle : THandle;
  LockFFWSInstalled : TRTLCriticalSection;

{===EffWinsockException==============================================}
constructor EffWinsockException.CreateTranslate(aErrorCode : integer;
                                                aDummy     : pointer);
var
  ErrorMsg : TffShStr;
begin
  ErrorMsg := ffStrResWinsock[aErrorCode];
  inherited CreateEx(ffStrResGeneral, fferrWinsock, [aErrorCode, aErrorCode, ErrorMsg]);
end;
{===Macro expansion==================================================}
function WSAMakeSyncReply(Buflen, Error : word) : longint;
register;
asm
  movzx eax, ax
  shl edx, 16
  or eax, edx
end;
{--------}
function WSAMakeSelectReply(Event, Error : word) : longint;
register;
asm
  movzx eax, ax
  shl edx, 16
  or eax, edx
end;
{--------}
function WSAGetAsyncBuflen(lParam : longint) : integer;
register;
asm
  and eax, $0000FFFF
end;
{--------}
function WSAGetAsyncError(lParam : longint) : integer;
register;
asm
  shr eax, 16
end;
{--------}
function WSAGetSelectEvent(lParam : longint) : integer;
register;
asm
  and eax, $0000FFFF
end;
{--------}
function WSAGetSelectError(lParam : longint) : integer;
register;
asm
  shr eax, 16
end;
{====================================================================}


{===Unit initialization/finalization=================================}
function FFWSInstalled : boolean;
const
  ffcMaxProtoInfoRecords = 15;
var
  aBuffer : PChar;
  pBuffer : PffwsProtocolInfo absolute aBuffer;
  aCode : HFile;
  aCount : integer;
  aFile : TOFStruct;
  anError : integer;
  anIndex : integer;
  anOffset : integer;
  aProtocolInfo : PffwsProtocolInfo;
  aSize : DWORD;
  aVersion : integer;
  WSData : TffWSAData;
begin
  EnterCriticalSection(LockFFWSInstalled);
  try
    Result := (ffwsLoadedWinsockVersion <> ffwvNone);

    { If this routine has already been called, exit. }
    if UnitInitializationDone then
      Exit;
    { No matter what happens next, we've initialized. }
    UnitInitializationDone := true;
    ffwsLoadedWinsockVersion := ffwvNone;
    aVersion := 0;

    { Load the Winsock DLL.  Note that we try to load the most recent
      Winsock version first. }
    for anIndex := ffcNumWinsockVersions downto 1 do begin

      {$IFDEF ForceWinSock1}
      if anIndex <> 1 then Continue;
      {$ENDIF}

      { Check to see if the file exists before trying to load it }
      aCode := OpenFile(ffWinsockVerArray[anIndex].ModuleName, aFile, OF_EXIST);
      if aCode = HFILE_ERROR then Continue;

      { If we get this far, we should have a good module -- load it }
      WSLibHandle := LoadLibrary(ffWinsockVerArray[anIndex].ModuleName);
      if WSLibHandle <> 0 then begin
        aVersion := anIndex;
        break;
      end;

    end;

    if (WSLibHandle = 0) then
      Exit;
    {load and validate all pointers}
    @WinsockRoutines.accept := GetProcAddress(WSLibHandle, 'accept');
    if not Assigned(WinsockRoutines.accept) then Exit;

    @WinsockRoutines.bind := GetProcAddress(WSLibHandle, 'bind');
    if not Assigned(WinsockRoutines.bind) then Exit;

    @WinsockRoutines.closesocket := GetProcAddress(WSLibHandle, 'closesocket');
    if not Assigned(WinsockRoutines.closesocket) then Exit;

    @WinsockRoutines.connect := GetProcAddress(WSLibHandle, 'connect');
    if not Assigned(WinsockRoutines.connect) then Exit;

    @WinsockRoutines.getpeername := GetProcAddress(WSLibHandle, 'getpeername');
    if not Assigned(WinsockRoutines.getpeername) then Exit;

    @WinsockRoutines.getsockname := GetProcAddress(WSLibHandle, 'getsockname');
    if not Assigned(WinsockRoutines.getsockname) then Exit;

    @WinsockRoutines.getsockopt := GetProcAddress(WSLibHandle, 'getsockopt');
    if not Assigned(WinsockRoutines.getsockopt) then Exit;

    @WinsockRoutines.htonl := GetProcAddress(WSLibHandle, 'htonl');
    if not Assigned(WinsockRoutines.htonl) then Exit;

    @WinsockRoutines.htons := GetProcAddress(WSLibHandle, 'htons');
    if not Assigned(WinsockRoutines.htons) then Exit;

    @WinsockRoutines.inet_addr := GetProcAddress(WSLibHandle, 'inet_addr');
    if not Assigned(WinsockRoutines.inet_addr) then Exit;

    @WinsockRoutines.inet_ntoa := GetProcAddress(WSLibHandle, 'inet_ntoa');
    if not Assigned(WinsockRoutines.inet_ntoa) then Exit;

    @WinsockRoutines.ioctlsocket := GetProcAddress(WSLibHandle, 'ioctlsocket');
    if not Assigned(WinsockRoutines.ioctlsocket) then Exit;

    @WinsockRoutines.listen := GetProcAddress(WSLibHandle, 'listen');
    if not Assigned(WinsockRoutines.listen) then Exit;

    @WinsockRoutines.ntohl := GetProcAddress(WSLibHandle, 'ntohl');
    if not Assigned(WinsockRoutines.ntohl) then Exit;

    @WinsockRoutines.ntohs := GetProcAddress(WSLibHandle, 'ntohs');
    if not Assigned(WinsockRoutines.ntohs) then Exit;

    @WinsockRoutines.recv := GetProcAddress(WSLibHandle, 'recv');
    if not Assigned(WinsockRoutines.recv) then Exit;

    @WinsockRoutines.recvfrom := GetProcAddress(WSLibHandle, 'recvfrom');
    if not Assigned(WinsockRoutines.recvfrom) then Exit;

    @WinsockRoutines.select := GetProcAddress(WSLibHandle, 'select');
    if not Assigned(WinsockRoutines.select) then Exit;

    @WinsockRoutines.send := GetProcAddress(WSLibHandle, 'send');
    if not Assigned(WinsockRoutines.send) then Exit;

    @WinsockRoutines.sendto := GetProcAddress(WSLibHandle, 'sendto');
    if not Assigned(WinsockRoutines.sendto) then Exit;

    @WinsockRoutines.setsockopt := GetProcAddress(WSLibHandle, 'setsockopt');
    if not Assigned(WinsockRoutines.setsockopt) then Exit;

    @WinsockRoutines.shutdown := GetProcAddress(WSLibHandle, 'shutdown');
    if not Assigned(WinsockRoutines.shutdown) then Exit;

    @WinsockRoutines.socket := GetProcAddress(WSLibHandle, 'socket');
    if not Assigned(WinsockRoutines.socket) then Exit;

    @WinsockRoutines.gethostbyaddr := GetProcAddress(WSLibHandle, 'gethostbyaddr');
    if not Assigned(WinsockRoutines.gethostbyaddr) then Exit;

    @WinsockRoutines.gethostbyname := GetProcAddress(WSLibHandle, 'gethostbyname');
    if not Assigned(WinsockRoutines.gethostbyname) then Exit;

    @WinsockRoutines.gethostname := GetProcAddress(WSLibHandle, 'gethostname');
    if not Assigned(WinsockRoutines.gethostname) then Exit;

    @WinsockRoutines.getservbyport := GetProcAddress(WSLibHandle, 'getservbyport');
    if not Assigned(WinsockRoutines.getservbyport) then Exit;

    @WinsockRoutines.getservbyname := GetProcAddress(WSLibHandle, 'getservbyname');
    if not Assigned(WinsockRoutines.getservbyname) then Exit;

    @WinsockRoutines.getprotobynumber := GetProcAddress(WSLibHandle, 'getprotobynumber');
    if not Assigned(WinsockRoutines.getprotobynumber) then Exit;

    @WinsockRoutines.getprotobyname := GetProcAddress(WSLibHandle, 'getprotobyname');
    if not Assigned(WinsockRoutines.getprotobyname) then Exit;

    @WinsockRoutines.WSAStartup := GetProcAddress(WSLibHandle, 'WSAStartup');
    if not Assigned(WinsockRoutines.WSAStartup) then Exit;

    @WinsockRoutines.WSACleanup := GetProcAddress(WSLibHandle, 'WSACleanup');
    if not Assigned(WinsockRoutines.WSACleanup) then Exit;

    if aVersion > 1 then begin
      @WinsockRoutines.WSAEnumProtocols := GetProcAddress(WSLibHandle, 'WSAEnumProtocolsA');
      if not Assigned(WinsockRoutines.WSAEnumProtocols) then Exit;
    end;

    @WinsockRoutines.WSASetLastError := GetProcAddress(WSLibHandle, 'WSASetLastError');
    if not Assigned(WinsockRoutines.WSASetLastError) then Exit;

    @WinsockRoutines.WSAGetLastError := GetProcAddress(WSLibHandle, 'WSAGetLastError');
    if not Assigned(WinsockRoutines.WSAGetLastError) then Exit;

    @WinsockRoutines.WSAIsBlocking := GetProcAddress(WSLibHandle, 'WSAIsBlocking');
    if not Assigned(WinsockRoutines.WSAIsBlocking) then Exit;

    @WinsockRoutines.WSAUnhookBlockingHook := GetProcAddress(WSLibHandle, 'WSAUnhookBlockingHook');
    if not Assigned(WinsockRoutines.WSAUnhookBlockingHook) then Exit;

    @WinsockRoutines.WSASetBlockingHook := GetProcAddress(WSLibHandle, 'WSASetBlockingHook');
    if not Assigned(WinsockRoutines.WSASetBlockingHook) then Exit;

    @WinsockRoutines.WSACancelBlockingCall := GetProcAddress(WSLibHandle, 'WSACancelBlockingCall');
    if not Assigned(WinsockRoutines.WSACancelBlockingCall) then Exit;

    @WinsockRoutines.WSAAsyncGetServByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetServByName');
    if not Assigned(WinsockRoutines.WSAAsyncGetServByName) then Exit;

    @WinsockRoutines.WSAAsyncGetServByPort := GetProcAddress(WSLibHandle, 'WSAAsyncGetServByPort');
    if not Assigned(WinsockRoutines.WSAAsyncGetServByPort) then Exit;

    @WinsockRoutines.WSAAsyncGetProtoByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetProtoByName');
    if not Assigned(WinsockRoutines.WSAAsyncGetProtoByName) then Exit;

    @WinsockRoutines.WSAAsyncGetProtoByNumber := GetProcAddress(WSLibHandle, 'WSAAsyncGetProtoByNumber');
    if not Assigned(WinsockRoutines.WSAAsyncGetProtoByNumber) then Exit;

    @WinsockRoutines.WSAAsyncGetHostByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetHostByName');
    if not Assigned(WinsockRoutines.WSAAsyncGetHostByName) then Exit;

    @WinsockRoutines.WSAAsyncGetHostByAddr := GetProcAddress(WSLibHandle, 'WSAAsyncGetHostByAddr');
    if not Assigned(WinsockRoutines.WSAAsyncGetHostByAddr) then Exit;

    @WinsockRoutines.WSACancelAsyncRequest := GetProcAddress(WSLibHandle, 'WSACancelAsyncRequest');
    if not Assigned(WinsockRoutines.WSACancelAsyncRequest) then Exit;

    @WinsockRoutines.WSAAsyncSelect := GetProcAddress(WSLibHandle, 'WSAAsyncSelect');
    if not Assigned(WinsockRoutines.WSAAsyncSelect) then Exit;

    { If we got here then we have succeeded. }
    if (WinsockRoutines.WSAStartup
          (ffWinsockVerArray[aVersion].VerNum, WSData) = 0) then begin
      ffwsLoadedWinsockVersion := TffwsWinsockVersion(aVersion);

      { Determine which winsock families are installed.  Allocate a buffer that
        will hold several protocol records. }
      if aVersion > 1 then begin
        ffwsFamiliesInstalled := [];
        { Allocate a buffer that we know is too small. }
        aSize := sizeOf(TffwsProtocolInfo);
        FFGetMem(aBuffer, 32);
        try
          Fillchar(aBuffer^, 32, 0);
          aSize := 0;
          aCount := WinsockRoutines.WSAEnumProtocols(nil, pBuffer, aSize);
          if aCount < 0 then begin
            anError := WinsockRoutines.WSAGetLastError;
            if anError = WSAENOBUFS then begin
              FFFreeMem(aBuffer, 32);
              FFGetMem(aBuffer, aSize);
              fillChar(aBuffer^, aSize, 0);
              aCount := WinsockRoutines.WSAEnumProtocols(nil, pBuffer, aSize);
            end;
          end;
          if aCount > 0 then begin
            anOffset := 0;
            for anIndex := 1 to aCount do begin
              { Grab the record. }
              aProtocolInfo := @(aBuffer[anOffset]);

              { Is it a family we care about? }
              case aProtocolInfo^.iAddressFamily of
                Af_INET : include(ffwsFamiliesInstalled, wfTCP);
                Af_IPX  : include(ffwsFamiliesInstalled, wfIPX);
              end;  { case }

              { Position to the next record. }
              inc(anOffset, sizeOf(TffwsProtocolInfo));
            end;
          end;
        finally
          if aSize > 0 then
            FFFreemem(aBuffer, aSize)
          else
            FFFreemem(aBuffer, 32);
        end;
      end
      else begin
        { Winsock 1: Assume all families supported. }
        ffwsFamiliesInstalled := [wfTCP, wfIPX];
      end;
    end;

  finally
    LeaveCriticalSection(LockFFWSInstalled);
  end;
  Result := (ffwsLoadedWinsockVersion <> ffwvNone);
end;
{--------}
procedure FinalizeUnit;
begin
  ffStrResWinsock.Free;
  DeleteCriticalSection(LockFFWSInstalled);
  if UnitInitializationDone then begin
    if (WSLibHandle <> 0) then begin
      if (ffwsLoadedWinsockVersion <> ffwvNone) then
        WinsockRoutines.WSACleanUp;
      FreeLibrary(WSLibHandle);
    end;
  end;
end;
{====================================================================}


{===FlashFiler helper routines=======================================}
procedure FFWSAsyncSelect(aSocket : TffwsSocket;
                          aWindow : HWnd;
                          aEvent  : longint);
var
  Error : integer;
begin
  if (WinsockRoutines.WSAAsyncSelect(aSocket, aWindow,
                                     ffwscEventComplete, aEvent) = SOCKET_ERROR) then begin
    Error := WinsockRoutines.WSAGetLastError;
    raise EffWinsockException.CreateTranslate(Error, nil);
  end;
end;
{--------}
function FFWSCreateSocket(aAF, aStruct, aProtocol : integer) : TffwsSocket;
var
  Error : integer;
begin
  Result := WinsockRoutines.socket(aAF, aStruct, aProtocol);
  if (Result = INVALID_SOCKET) then begin
    Error := WinsockRoutines.WSAGetLastError;
    raise EffWinsockException.CreateTranslate(Error, nil);
  end;
end;
{--------}
function FFWSCvtAddrToStr(aAddr : TffwsInAddr) : TffNetName;
begin
  Result := FFStrPas(WinsockRoutines.inet_ntoa(aAddr));
end;
{--------}
function FFWSCvtIPXAddrToStr(const aNetNum : TffwsIPXNetNum;
                             const aAddr   : TffwsIPXAddr) : TffNetName;
const
  HexChars : string[16] = '0123456789ABCDEF';
var
  i, j : integer;
begin
{Begin !!.03}
{$IFDEF IsDelphi}
  Result[0] := chr((2 * sizeof(TffwsIPXNetNum)) +
                   1 +
                   (2 * sizeof(TffwsIPXAddr)) +
                   5);
{$ELSE}
  SetLength(Result, (2 * sizeof(TffwsIPXNetNum)) + 1 +
                    (2 * sizeof(TffwsIPXAddr)) + 5);
{$ENDIF}
{End !!.03}
  j := 0;
  for i := 0 to pred(sizeof(TffwsIPXNetNum)) do begin
    Result[j+1] := HexChars[(aNetNum[i] shr 4) + 1];
    Result[j+2] := HexChars[(aNetNum[i] and $F) + 1];
    inc(j, 2);
  end;
  inc(j);
  Result[j] := ':';
  for i := 0 to pred(sizeof(TffwsIPXAddr)) do begin
    if (i <> 0) then
      Result[j] := '-';
    Result[j+1] := HexChars[(aAddr[i] shr 4) + 1];
    Result[j+2] := HexChars[(aAddr[i] and $F) + 1];
    inc(j, 3);
  end;
end;
{--------}
function FFWSCvtStrToAddr(aStr : TffNetName; var aAddr : TffwsInAddr) : boolean;
var
  StrZ : TffStringZ;
begin
  FFStrPCopy(StrZ, aStr);
  aAddr := TffWord32(WinsockRoutines.inet_addr(StrZ));
  Result := (aAddr <> INADDR_NONE);
end;
{--------}
function FFWSCvtStrToIPXAddr(const aStr    : TffNetName;
                               var aNetNum : TffwsIPXNetNum;
                               var aAddr   : TffwsIPXAddr) : boolean;
var
  i, j    : integer;
  Nibble  : integer;
  Ch      : char;
  DoUpper : boolean;
  DoNetNum: boolean;
begin
  Nibble := 0;
  Result := false;
  j := 0;
  DoNetNum := true;
  DoUpper := true;
  for i := 1 to length(aStr) do begin
    Ch := upcase(aStr[i]);
    if ('0' <= Ch) and (Ch <= '9') then
      Nibble := ord(Ch) - ord('0')
    else if ('A' <= Ch) and (Ch <= 'F') then
      Nibble := ord(Ch) - ord('A') + 10
    else if (Ch <> '-') and (Ch <> ':') then
      Exit;
    if (Ch = '-') or (Ch = ':') then begin
      if DoNetNum then
        j := 0;
      DoNetNum := false;
      DoUpper := true;
    end
    else
      if DoUpper then begin
        if DoNetNum then
          aNetNum[j] := Nibble shl 4
        else
          aAddr[j] := Nibble shl 4;
        DoUpper := false;
      end
      else begin
        if DoNetNum then
          aNetNum[j] := aNetNum[j] or Nibble
        else
          aAddr[j] := aAddr[j] or Nibble;
        inc(j);
        DoUpper := true;
      end;
  end;
  Result := true;
end;
{--------}
procedure FFWSDestroySocket(aSocket : TffwsSocket);
begin
  if (aSocket <> INVALID_SOCKET) then begin
    WinsockRoutines.shutdown(aSocket, 2);
    WinsockRoutines.closesocket(aSocket);
  end;
end;
{--------}
function FFWSGetLocalHosts(aList : TStrings) : Boolean;
type
  TaPInAddr = array [0..255] of PFFWord32;
  PaPInAddr = ^TaPInAddr;
var
  ZStr      : TffStringZ;
  HostEnt   : PffwsHostEnt;
  IPAddress : TffNetName;
  pptr      : PaPInAddr;
  Idx       : Integer;

begin
  aList.BeginUpdate;
  try
    aList.Clear;
    aList.Add('<ALL INTERFACES>');
    Result := False;
    if (WinsockRoutines.gethostname(ZStr, SizeOf(ZStr)) = 0) then begin
      HostEnt := WinsockRoutines.gethostbyname(ZStr);
      if Assigned(HostEnt) then begin
        pptr := PaPInAddr(HostEnt^.h_addr_list);
        Idx := 0;
        while Assigned(pptr^[Idx]) do begin
          {pptr is assigned if any winsock based protocol is installed}
          {When IPX/SPX is installed, and TCP/IP is an IP address still
           is returned. We must filter this out.}
          IPAddress := FFWSCvtAddrToStr(pptr^[Idx]^);
          if IPAddress <> '127.0.0.1' then
            aList.Add(Format('Adapter %D: %S', [Idx, IPAddress]));
          Inc(Idx);
        end;
        Result := true;
      end;
    end;
  finally
    aList.EndUpdate;
  end;
end;
{--------}
function FFWSGetLocalHostByNum(const NIC      : Integer;
                                 var aNetName : TffNetName;
                                 var aAddr   : TffwsInAddr) : Boolean;
type
  TaPInAddr = array [0..255] of PffWord32;
  PaPInAddr = ^TaPInAddr;
var
  ZStr      : TffStringZ;
  HostEnt   : PffwsHostEnt;
  pptr      : PaPInAddr;
begin
  Result := False;
  if (WinsockRoutines.gethostname(ZStr, SizeOf(ZStr)) = 0) then begin
    HostEnt := WinsockRoutines.gethostbyname(ZStr);
    if Assigned(HostEnt) then begin
      pptr := PaPInAddr(HostEnt^.h_addr_list);
      if NIC = -1 then begin
        aNetName := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
        aAddr := InAddr_ANY;
        Result := True;
      end else begin
        if Assigned(pptr^[NIC]) then begin
          aNetName := FFStrPasLimit(HostEnt^.h_name, Pred(SizeOf(TffNetName)));
          aAddr:= pptr^[NIC]^;
          Result := True;
        end;
      end;
    end;
  end;
end;
{--------}
function FFWSGetLocalIPXAddr(var aNetNum : TffwsIPXNetNum;
                             var aAddr   : TffwsIPXAddr) : boolean;
var
  Addr    : TffwsSockAddr;
  IPXInfo : TffwsIPXAddrInfo;
  S       : TffwsSocket;
begin
  // Create IPX socket.
  S := FFWSCreateSocket(AF_IPX, SOCK_DGRAM, NSPROTO_IPX);
  // Socket must be bound prior to calling IPX_ADDRESS
  FillChar(Addr, sizeof(Addr), 0);
  Addr.IPX.sipx_family := AF_IPX;
  WinsockRoutines.bind(S, Addr, sizeof(TffwsSockAddrIPX));
  // Specify which adapter to check.
  FillChar(IPXInfo, sizeof(IPXInfo), 0);
  IPXInfo.adapternum := 0;
  FFWSGetSocketOption(S, NSPROTO_IPX, IPX_ADDRESS, IPXInfo, sizeof(IPXInfo));
  aNetNum := IPXInfo.netnum;
  aAddr := IPXInfo.nodenum;
  Result := true;
  // Destroy IPX socket.
  FFWSDestroySocket(S);
end;
{--------}
function FFWSGetRemoteHost(const aName : TffNetName;
                             var aNetName : TffNetName; var aAddr : TffwsInAddr) : boolean;
var
  ZStr    : TffStringZ;
  HostEnt : PffwsHostEnt;
begin
  HostEnt := WinsockRoutines.gethostbyname(FFStrPCopy(ZStr, aName));
  if (HostEnt = nil) then
    Result := false
  else begin
    aAddr := PffwsInAddr((HostEnt^.h_addr)^)^;
    aNetName := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
    Result := true;
  end;
end;
{--------}
function FFWSGetRemoteNameFromAddr(aAddr : TffwsInAddr) : TffNetName;
var
  HostEnt : PffwsHostEnt;
begin
  HostEnt := WinsockRoutines.gethostbyaddr(aAddr, sizeof(aAddr), PF_INET);
  if (HostEnt = nil) then
    Result := ''
  else
    Result := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
end;
{--------}
procedure FFWSGetSocketOption(aSocket : TffwsSocket; aLevel, aOptName : integer;
                         var aOptValue; aOptValueLen : integer);
var
  Error : integer;
begin
  Error := WinsockRoutines.getsockopt(aSocket, aLevel, aOptName, aOptValue, aOptValueLen);
  if (Error = SOCKET_ERROR) then begin
    Error := WinsockRoutines.WSAGetLastError;
    raise EffWinsockException.CreateTranslate(Error, nil);
  end;
end;
{--------}
procedure FFWSSetSocketOption(aSocket : TffwsSocket; aLevel, aOptName : integer;
                         var aOptValue; aOptValueLen : integer);
var
  Error : integer;
begin
  Error := WinsockRoutines.setsockopt(aSocket, aLevel, aOptName, aOptValue, aOptValueLen);
  if (Error = SOCKET_ERROR) then begin
    Error := WinsockRoutines.WSAGetLastError;
    raise EffWinsockException.CreateTranslate(Error, nil);
  end;
end;
{====================================================================}


initialization
  UnitInitializationDone := false;
  ffwsLoadedWinsockVersion := ffwvNone;
  ffStrResWinsock := nil;
  ffStrResWinsock := TffStringResource.Create(hInstance, 'FF_WINSOCK_ERROR_STRINGS');
  InitializeCriticalSection(LockFFWSInstalled);

finalization
  FinalizeUnit;

end.
