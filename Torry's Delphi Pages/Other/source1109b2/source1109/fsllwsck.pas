{$I fsdefine.inc}

{$IFDEF CBuilder3}
(*$HPPEMIT '' *)
(*$HPPEMIT '#undef h_addr' *)
(*$HPPEMIT '' *)
{$ENDIF}

{ Use the following DEFINE to force loading of Winsock 1 }
{.$DEFINE ForceWinSock1}

Unit fsllwsck;

Interface

Uses
  Classes,
  Windows,
  Messages,
  SysUtils,
  fsconst,
  fsllwsct,
  fsllbase,
  fssrmgr,
  fsllexcp;

{$R fswscnst.res}

Const
  FSWScEventComplete = WM_USER + $0FF1;

  {===Standard Winsock constants===}
Const
  Fd_SETSIZE = 64;

  IocPARM_MASK = $7F;
  Ioc_VOID = $20000000;
  Ioc_OUT = $40000000;
  Ioc_IN = $80000000;
  Ioc_INOUT = (Ioc_IN Or Ioc_OUT);

  { Protocols }

  IpPROTO_IP = 0;
  IpPROTO_ICMP = 1;
  IpPROTO_GGP = 2;
  IpPROTO_TCP = 6;
  IpPROTO_PUP = 12;
  IpPROTO_UDP = 17;
  IpPROTO_IDP = 22;
  IpPROTO_ND = 77;

  IpPROTO_RAW = 255;
  IpPROTO_MAX = 256;

  { Port/socket numbers: network standard functions}

  IpPORT_ECHO = 7;
  IpPORT_DISCARD = 9;
  IpPORT_SYSTAT = 11;
  IpPORT_DAYTIME = 13;
  IpPORT_NETSTAT = 15;
  IpPORT_FTP = 21;
  IpPORT_TELNET = 23;
  IpPORT_SMTP = 25;
  IpPORT_TIMESERVER = 37;
  IpPORT_NAMESERVER = 42;
  IpPORT_WHOIS = 43;
  IpPORT_MTP = 57;

  { Port/socket numbers: host specific functions }

  IpPORT_TFTP = 69;
  IpPORT_RJE = 77;
  IpPORT_FINGER = 79;
  IpPORT_TTYLINK = 87;
  IpPORT_SUPDUP = 95;

  { UNIX TCP sockets }

  IpPORT_EXECSERVER = 512;
  IpPORT_LOGINSERVER = 513;
  IpPORT_CMDSERVER = 514;
  IpPORT_EFSSERVER = 520;

  { UNIX UDP sockets }

  IpPORT_BIFFUDP = 512;
  IpPORT_WHOSERVER = 513;
  IpPORT_ROUTESERVER = 520;

  { Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root). }

  IpPORT_RESERVED = 1024;

  { Link numbers }

  ImpLINK_IP = 155;
  ImpLINK_LOWEXPER = 156;
  ImpLINK_HIGHEXPER = 158;

  { Get # bytes to read }
  FIoNREAD = Ioc_OUT Or ((Longint(SizeOf(Longint)) And IocPARM_MASK) Shl 16) Or
    (Longint(Byte('f')) Shl 8) Or 127;

  { Set/Clear non-blocking i/o }
  FIoNBIO = Ioc_IN Or ((Longint(SizeOf(Longint)) And IocPARM_MASK) Shl 16) Or
    (Longint(Byte('f')) Shl 8) Or 126;

  { Set/Clear async i/o }
  FIoASYNC = Ioc_IN Or ((Longint(SizeOf(Longint)) And IocPARM_MASK) Shl 16) Or
    (Longint(Byte('f')) Shl 8) Or 125;

  InAddr_ANY = $00000000;
  InAddr_LOOPBACK = $7F000001;
  InAddr_BROADCAST = $FFFFFFFF;
  InAddr_NONE = $FFFFFFFF;

  WsaDESCRIPTION_LEN = 256;
  WsaSYS_STATUS_LEN = 128;
  WsaProtocolLen = 255;
  WsaMaxProtocolChain = 7;

  { Options for use with (get/set)sockopt at the IP level. }

  Ip_OPTIONS = 1;
  Ip_MULTICAST_IF = 2; { set/get IP multicast interface   }
  Ip_MULTICAST_TTL = 3; { set/get IP multicast timetolive  }
  Ip_MULTICAST_LOOP = 4; { set/get IP multicast loopback    }
  Ip_ADD_MEMBERSHIP = 5; { add  an IP group membership      }
  Ip_DROP_MEMBERSHIP = 6; { drop an IP group membership      }

  Ip_DEFAULT_MULTICAST_TTL = 1; { normally limit m'casts to 1 hop  }
  Ip_DEFAULT_MULTICAST_LOOP = 1; { normally hear sends if a member  }
  Ip_MAX_MEMBERSHIPS = 20; { per socket; must fit in one mbuf }

  Ipx_ADDRESS = $4007; { querying IPX info }

  Invalid_SOCKET = -1;
  Socket_ERROR = -1;

  { Types }

  Sock_STREAM = 1; { stream socket }
  Sock_DGRAM = 2; { datagram socket }
  Sock_RAW = 3; { raw-protocol interface }
  Sock_RDM = 4; { reliably-delivered message }
  Sock_SEQPACKET = 5; { sequenced packet stream }

  { Option flags per-socket. }

  So_DEBUG = $0001; { turn on debugging info recording }
  So_ACCEPTCONN = $0002; { socket has had listen() }
  So_REUSEADDR = $0004; { allow local address reuse }
  So_KEEPALIVE = $0008; { keep connections alive }
  So_DONTROUTE = $0010; { just use interface addresses }
  So_BROADCAST = $0020; { permit sending of broadcast msgs }
  So_USELOOPBACK = $0040; { bypass hardware when possible }
  So_LINGER = $0080; { linger on close if data present }
  So_OOBINLINE = $0100; { leave received OOB data in line }

  So_DONTLINGER = $FF7F;

  { Additional options. }

  So_SNDBUF = $1001; { send buffer size }
  So_RCVBUF = $1002; { receive buffer size }
  So_SNDLOWAT = $1003; { send low-water mark }
  So_RCVLOWAT = $1004; { receive low-water mark }
  So_SNDTIMEO = $1005; { send timeout }
  So_RCVTIMEO = $1006; { receive timeout }
  So_ERROR = $1007; { get error status and clear }
  So_TYPE = $1008; { get socket type }

  { Options for connect and disconnect data and options.  Used only by
    non-TCP/IP transports such as DECNet, OSI TP4, etc. }

  So_CONNDATA = $7000;
  So_CONNOPT = $7001;
  So_DISCDATA = $7002;
  So_DISCOPT = $7003;
  So_CONNDATALEN = $7004;
  So_CONNOPTLEN = $7005;
  So_DISCDATALEN = $7006;
  So_DISCOPTLEN = $7007;

  { Option for opening sockets for synchronous access. }

  So_OPENTYPE = $7008;

  So_SYNCHRONOUS_ALERT = $10;
  So_SYNCHRONOUS_NONALERT = $20;

  { Other NT-specific options. }

  So_MAXDG = $7009;
  So_MAXPATHDG = $700A;

  { TCP options. }

  TCP_NoDELAY = $0001;
  TCP_BsdURGENT = $7000;

  { Address families. }

  Af_UNSPEC = 0; { unspecified }
  Af_UNIX = 1; { local to host (pipes, portals) }
  Af_INET = 2; { internetwork: UDP, TCP, etc. }
  Af_IMPLINK = 3; { arpanet imp addresses }
  Af_PUP = 4; { pup protocols: e.g. BSP }
  Af_CHAOS = 5; { mit CHAOS protocols }
  Af_IPX = 6; { IPX and SPX }
  Af_NS = 6; { XEROX NS protocols }
  Af_ISO = 7; { ISO protocols }
  Af_OSI = Af_ISO; { OSI is ISO }
  Af_ECMA = 8; { european computer manufacturers }
  Af_DATAKIT = 9; { datakit protocols }
  Af_CCITT = 10; { CCITT protocols, X.25 etc }
  Af_SNA = 11; { IBM SNA }
  Af_DECnet = 12; { DECnet }
  Af_DLI = 13; { Direct data link interface }
  Af_LAT = 14; { LAT }
  Af_HYLINK = 15; { NSC Hyperchannel }
  Af_APPLETALK = 16; { AppleTalk }
  Af_NETBIOS = 17; { NetBios-style addresses }
  Af_VOICEVIEW = 18; { VoiceView }
  Af_MAX = 19;

  { Protocol families, same as address families for now. }

  Pf_UNSPEC = Af_UNSPEC;
  Pf_UNIX = Af_UNIX;
  Pf_INET = Af_INET;
  Pf_IMPLINK = Af_IMPLINK;
  Pf_PUP = Af_PUP;
  Pf_CHAOS = Af_CHAOS;
  Pf_NS = Af_NS;
  Pf_IPX = Af_IPX;
  Pf_ISO = Af_ISO;
  Pf_OSI = Af_OSI;
  Pf_ECMA = Af_ECMA;
  Pf_DATAKIT = Af_DATAKIT;
  Pf_CCITT = Af_CCITT;
  Pf_SNA = Af_SNA;
  Pf_DECnet = Af_DECnet;
  Pf_DLI = Af_DLI;
  Pf_LAT = Af_LAT;
  Pf_HYLINK = Af_HYLINK;
  Pf_APPLETALK = Af_APPLETALK;
  Pf_VOICEVIEW = Af_VOICEVIEW;

  Pf_MAX = Af_MAX;

  { Level number for (get/set)sockopt() to apply to socket itself. }

  Sol_SOCKET = $FFFF; {options for socket level }

  { Maximum queue length specifiable by listen. }

  SoMAXCONN = 5;

  Msg_OOB = $1; {process out-of-band data }
  Msg_PEEK = $2; {peek at incoming message }
  Msg_DONTROUTE = $4; {send without using routing tables }

  Msg_MAXIOVLEN = 16;

  Msg_PARTIAL = $8000; {partial send or recv for message xport }

  { Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MaxGETHOSTSTRUCT = 1024;

  { Define flags to be used with the WSAAsyncSelect() call. }

  Fd_READ = $01;
  Fd_WRITE = $02;
  Fd_OOB = $04;
  Fd_ACCEPT = $08;
  Fd_CONNECT = $10;
  Fd_CLOSE = $20;

  { Protocols for IPX/SPX }

  NSPROTO_IPX = 1000;
  NSPROTO_SPX = 1256;
  NSPROTO_SPXII = 1257;

Type
  EfsWinsockException = Class(EfsCommsException)
  Public
    Constructor CreateTranslate(aErrorCode: Integer;
      aDummy: pointer);
  End;

  {===FF Winsock types===}
Type
  TfsWinsockFamily = ({the Winsock family types we support}
    wfTCP, {..TCP/IP}
    wfIPX); {..IPX/SPX}

  TfsWinsockFamilies = Set Of TfsWinsockFamily;

  { The following record type is used to track Winsock versions supported
    by this module. }
  TfsWinsockVerRec = Record
    VerNum: Word;
    ModuleName: Array[0..12] Of AnsiChar;
  End;

  TfswsWinsockVersion = (ffwvNone, ffwvWinSock1, ffwvWinSock2);
  { Identifies the winsock version we have loaded in FSWSInstalled. }

{===Standard Winsock types===}
Type
  TfswsSocket = Integer; {a Winsock socket}

  PfswsFDSet = ^TfswsFDSet;
  TfswsFDSet = Packed Record {an array of sockets}
    fd_count: Integer;
    fd_array: Array[0..pred(FD_SETSIZE)] Of TfswsSocket;
  End;

  PfswsTimeVal = ^TfswsTimeVal;
  TfswsTimeVal = Packed Record {a time value}
    tv_sec: Longint;
    tv_usec: Longint;
  End;

  PfswsHostEnt = ^TfswsHostEnt;
  TfswsHostEnt = Packed Record {host entity}
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    Case Byte Of
      0: (h_addr_list: ^PAnsiChar);
      1: (h_Addr: ^PAnsiChar)
  End;

  PfswsNetEnt = ^TfswsNetEnt;
  TfswsNetEnt = Packed Record {network entity}
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: Smallint;
    n_net: Longint;
  End;

  PfswsServEnt = ^TfswsServEnt;
  TfswsServEnt = Packed Record {server entity}
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
    s_port: Smallint;
    s_proto: PAnsiChar;
  End;

  PfswsProtoEnt = ^TfswsProtoEnt;
  TfswsProtoEnt = Packed Record {protocol entity}
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: Smallint;
  End;

  PfswsInAddr = ^TfswsInAddr;
  TfswsInAddr = TffWord32;

  PfswsSockAddrIn = ^TfswsSockAddrIn;
  TfswsSockAddrIn = Packed Record
    sin_family: Word;
    sin_port: Word;
    sin_addr: TfswsInAddr;
    sin_zero: Array[0..7] Of AnsiChar;
  End;

  PfswsIPXAddr = ^TfswsIPXAddr;
  TfswsIPXAddr = Array[0..5] Of Byte;

  PFSWSIPXNetNum = ^TFSWSIPXNetNum;
  TFSWSIPXNetNum = Array[0..3] Of Byte;

  PFSWSSockAddrIPX = ^TFSWSSockAddrIPX;
  TFSWSSockAddrIPX = Packed Record
    sipx_family: Word;
    sipx_netnum: TFSWSIPXNetNum;
    sipx_nodenum: TfswsIPXAddr;
    sipx_socket: Word;
  End;

  { Structure used by kernel to store most addresses. }
  PFSWSSockAddr = ^TFSWSSockAddr;
  TFSWSSockAddr = Record
    Case Integer Of
      0: (TCP: TfswsSockAddrIn);
      1: (IPX: TFSWSSockAddrIPX);
  End;

  PFSWSAData = ^TFSWSAData;
  TFSWSAData = Packed Record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: Array[0..WSADESCRIPTION_LEN] Of AnsiChar;
    szSystemStatus: Array[0..WSASYS_STATUS_LEN] Of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  End;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PFSWSSockProto = ^TFSWSSockProto;
  TFSWSSockProto = Packed Record
    sp_family: Word;
    sp_protocol: Word;
  End;

  { Structure used for manipulating linger option. }
  PFSWSLinger = ^TFSWSLinger;
  TFSWSLinger = Packed Record
    l_onoff: Word;
    l_linger: Word;
  End;

  {structure for querying IPX address info (from NWLINK.H)}
  PfswsIPXAddrInfo = ^TfswsIPXAddrInfo;
  TfswsIPXAddrInfo = Packed Record
    adapternum: Integer; {input: 0-based adapter number}
    netnum: TFSWSIPXNetNum; {output: IPX network number}
    nodenum: TfswsIPXAddr; {output: IPX node address}
    wan: boolean; {output: TRUE = adapter is on a wan link}
    status: boolean; {output: TRUE = wan link is up (or adapter is not wan)}
    maxpkt: Integer; {output: max packet size, not including IPX header}
    linkspeed: Longint; {output: link speed in 100 bytes/sec (i.e. 96 == 9600)}
  End;

  TfswsProtocolChain = Packed Record
    chainLen: Integer; { The length of the chain:
    0 -> layered protocol,
    1 -> base protocol,
    > 1 -> protocol chain }
    chainEntries: Array[0..WsaMaxProtocolChain - 1] Of DWORD;
  End;

  { Structure for retrieving protocol information. }
  PfswsProtocolInfo = ^TfswsProtocolInfo;
  TfswsProtocolInfo = Packed Record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: TfswsProtocolChain;
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
    szProtocol: Array[0..WsaProtocolLen] Of AnsiChar;
  End;

  { Socket function types }
  tfswsrAccept =
    Function(S: TfswsSocket; Var Addr: TFSWSSockAddr; Var Addrlen: Integer): TfswsSocket
    Stdcall;
  tfswsrBind =
    Function(S: TfswsSocket; Var Addr: TFSWSSockAddr; NameLen: Integer): Integer
    Stdcall;
  tfswsrCloseSocket =
    Function(S: TfswsSocket): Integer
    Stdcall;
  tfswsrConnect =
    Function(S: TfswsSocket; Var Name: TFSWSSockAddr; NameLen: Integer): Integer
    Stdcall;
  tfswsrEnumProtocols =
    Function(Protocols: PInteger; aBuffer: PfswsProtocolInfo;
    Var BufferLength: DWORD): Integer; stdcall;
  tfswsrIOCtlSocket =
    Function(S: TfswsSocket; Cmd: Longint; Var Arg: Longint): Integer
    Stdcall;
  tfswsrGetPeerName =
    Function(S: TfswsSocket; Var Name: TFSWSSockAddr; Var NameLen: Integer): Integer
    Stdcall;
  tfswsrGetSockName =
    Function(S: TfswsSocket; Var Name: TFSWSSockAddr; Var NameLen: Integer): Integer
    Stdcall;
  tfswsrGetSockOpt =
    Function(S: TfswsSocket; Level, OptName: Integer;
    Var OptVal; Var OptLen: Integer): Integer
    Stdcall;
  tfswsrhtonl =
    Function(HostLong: Longint): Longint
    Stdcall;
  tfswsrhtons =
    Function(HostShort: Word): Word
    Stdcall;
  tfswsrINet_Addr =
    Function(Cp: PAnsiChar): dword {!!.11}
  Stdcall;
  tfswsrINet_NtoA =
    Function(InAddr: TfswsInAddr): PAnsiChar
    Stdcall;
  tfswsrListen =
    Function(S: TfswsSocket; Backlog: Integer): Integer
    Stdcall;
  tfswsrntohl =
    Function(NetLong: Longint): Longint
    Stdcall;
  tfswsrntohs =
    Function(NetShort: Word): Word
    Stdcall;
  tfswsrRecv =
    Function(S: TfswsSocket; Var Buf; Len, Flags: Integer): Integer
    Stdcall;
  tfswsrRecvFrom =
    Function(S: TfswsSocket; Var Buf; Len, Flags: Integer;
    Var From: TFSWSSockAddr; Var FromLen: Integer): Integer
    Stdcall;
  tfswsrSelect =
    Function(Nfds: Integer; Readfds, Writefds,
    Exceptfds: PfswsFDSet; Timeout: PfswsTimeVal): Longint
    Stdcall;
  tfswsrSend =
    Function(S: TfswsSocket; Var Buf; Len, Flags: Integer): Integer
    Stdcall;
  tfswsrSendTo =
    Function(S: TfswsSocket; Var Buf; Len, Flags: Integer;
    Var AddrTo: TFSWSSockAddr; ToLen: Integer): Integer
    Stdcall;
  tfswsrSetSockOpt =
    Function(S: TfswsSocket; Level, OptName: Integer;
    Var OptVal; OptLen: Integer): Integer
    Stdcall;
  tfswsrShutdown =
    Function(S: TfswsSocket; How: Integer): Integer
    Stdcall;
  tfswsrSocket =
    Function(Af, Struct, Protocol: Integer): TfswsSocket
    Stdcall;
  tfswsrGetHostByAddr =
    Function(Var Addr; Len, Struct: Integer): PfswsHostEnt
    Stdcall;
  tfswsrGetHostByName =
    Function(Name: PAnsiChar): PfswsHostEnt
    Stdcall;
  tfswsrGetHostName =
    Function(Name: PAnsiChar; Len: Integer): Integer
    Stdcall;
  tfswsrGetServByPort =
    Function(Port: Integer; Proto: PAnsiChar): PfswsServEnt
    Stdcall;
  tfswsrGetServByName =
    Function(Name, Proto: PAnsiChar): PfswsServEnt
    Stdcall;
  tfswsrGetProtoByNumber =
    Function(Proto: Integer): PfswsProtoEnt
    Stdcall;
  tfswsrGetProtoByName =
    Function(Name: PAnsiChar): PfswsProtoEnt
    Stdcall;
  tfswsrWSAStartup =
    Function(wVersionRequired: Word; Var WSData: TFSWSAData): Integer
    Stdcall;
  tfswsrWSACleanup =
    Function: Integer
    Stdcall;
  tfswsrWSASetLastError =
    Procedure(iError: Integer)
    Stdcall;
  tfswsrWSAGetLastError =
    Function: Integer
    Stdcall;
  tfswsrWSAIsBlocking =
    Function: BOOL
    Stdcall;
  tfswsrWSAUnhookBlockingHook =
    Function: Integer
    Stdcall;
  tfswsrWSASetBlockingHook =
    Function(lpBlockFunc: TFarProc): TFarProc
    Stdcall;
  tfswsrWSACancelBlockingCall =
    Function: Integer
    Stdcall;
  tfswsrWSAAsyncGetServByName =
    Function(HWindow: HWnd; wMsg: Integer;
    Name, Proto, Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSAAsyncGetServByPort =
    Function(HWindow: HWnd; wMsg, Port: Integer;
    Proto, Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSAAsyncGetProtoByName =
    Function(HWindow: HWnd; wMsg: Integer;
    Name, Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSAAsyncGetProtoByNumber =
    Function(HWindow: HWnd; wMsg: Integer; Number: Integer;
    Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSAAsyncGetHostByName =
    Function(HWindow: HWnd; wMsg: Integer;
    Name, Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSAAsyncGetHostByAddr =
    Function(HWindow: HWnd; wMsg: Integer; Addr: PAnsiChar;
    Len, Struct: Integer; Buf: PAnsiChar; BufLen: Integer): THandle
    Stdcall;
  tfswsrWSACancelAsyncRequest =
    Function(hAsyncTaskHandle: THandle): Integer
    Stdcall;
  tfswsrWSAAsyncSelect =
    Function(S: TfswsSocket; HWindow: HWnd; wMsg: Integer; lEvent: Longint): Integer
    Stdcall;

Type
  PfsWinsockRoutines = ^TfsWinsockRoutines;
  TfsWinsockRoutines = Record {record of Winsock function pointers}
    accept: tfswsrAccept;
    bind: tfswsrBind;
    closesocket: tfswsrCloseSocket;
    connect: tfswsrConnect;
    ioctlsocket: tfswsrIOCtlSocket;
    getpeername: tfswsrGetPeerName;
    getsockname: tfswsrGetSockName;
    getsockopt: tfswsrGetSockOpt;
    htonl: tfswsrhtonl;
    htons: tfswsrhtons;
    inet_addr: tfswsrINet_Addr;
    inet_ntoa: tfswsrINet_Ntoa;
    listen: tfswsrListen;
    ntohl: tfswsrntohl;
    ntohs: tfswsrntohs;
    recv: tfswsrRecv;
    recvfrom: tfswsrRecvFrom;
    select: tfswsrSelect;
    send: tfswsrSend;
    sendTo: tfswsrSendTo;
    setsockopt: tfswsrSetSockOpt;
    shutdown: tfswsrShutdown;
    socket: tfswsrSocket;
    gethostbyaddr: tfswsrGetHostByAddr;
    gethostbyname: tfswsrGetHostByName;
    gethostname: tfswsrGetHostName;
    getservbyport: tfswsrGetServByPort;
    getservbyname: tfswsrGetServByName;
    getprotobynumber: tfswsrGetProtoByNumber;
    getprotobyname: tfswsrGetProtoByName;
    WSAStartup: tfswsrWSAStartup;
    WSACleanup: tfswsrWSACleanup;
    WSAEnumProtocols: tfswsrEnumProtocols;
    WSASetLastError: tfswsrWSASetLastError;
    WSAGetLastError: tfswsrWSAGetLastError;
    WSAIsBlocking: tfswsrWSAIsBlocking;
    WSAUnhookBlockingHook: tfswsrWSAUnhookBlockingHook;
    WSASetBlockingHook: tfswsrWSASetBlockingHook;
    WSACancelBlockingCall: tfswsrWSACancelBlockingCall;
    WSAAsyncGetServByName: tfswsrWSAAsyncGetServByName;
    WSAAsyncGetServByPort: tfswsrWSAAsyncGetServByPort;
    WSAAsyncGetProtoByName: tfswsrWSAAsyncGetProtoByName;
    WSAAsyncGetProtoByNumber: tfswsrWSAAsyncGetProtoByNumber;
    WSAAsyncGetHostByName: tfswsrWSAAsyncGetHostByName;
    WSAAsyncGetHostByAddr: tfswsrWSAAsyncGetHostByAddr;
    WSACancelAsyncRequest: tfswsrWSACancelAsyncRequest;
    WSAAsyncSelect: tfswsrWSAAsyncSelect;
  End;

Var
  WinsockRoutines: TfsWinsockRoutines;
  fswsFamiliesInstalled: TfsWinsockFamilies;

Function FSWSInstalled: boolean;
{-Returns true if Winsock is installed}

Function WSAMakeSyncReply(Buflen, Error: Word): Longint;
{-Construct the response to a WSAAsyncGetXByY routine}
Function WSAMakeSelectReply(Event, Error: Word): Longint;
{-Construct the response to WSAAsyncSelect}
Function WSAGetAsyncBuflen(lParam: Longint): Integer;
{-Extract the buffer length from lParam in response to a WSAGetXByY}
Function WSAGetAsyncError(lParam: Longint): Integer;
{-Extract the error from lParam in response to a WSAGetXByY}
Function WSAGetSelectEvent(lParam: Longint): Integer;
{-Extract the event from lParam in response to a WSAAsyncSelect}
Function WSAGetSelectError(lParam: Longint): Integer;
{-Extract the error from lParam in response to a WSAAsyncSelect}

{===FlashFiler helper routines===}
Procedure FSWSAsyncSelect(aSocket: TfswsSocket;
  aWindow: HWnd;
  aEvent: Longint);
Function FSWSCreateSocket(aAF, aStruct, aProtocol: Integer): TfswsSocket;
Function FSWSCvtAddrToStr(aAddr: TfswsInAddr): TffNetName;
Function FSWSCvtIPXAddrToStr(Const aNetNum: TFSWSIPXNetNum;
  Const aAddr: TfswsIPXAddr): TffNetName;
Function FSWSCvtStrToAddr(aStr: TffNetName; Var aAddr: TfswsInAddr): boolean;
Function FSWSCvtStrToIPXAddr(Const aStr: TffNetName;
  Var aNetNum: TFSWSIPXNetNum;
  Var aAddr: TfswsIPXAddr): boolean;
Procedure FSWSDestroySocket(aSocket: TfswsSocket);
Function FSWSGetLocalHosts(aList: TStrings): Boolean;
Function FSWSGetLocalHostByNum(Const NIC: Integer;
  Var aNetName: TffNetName;
  Var aAddr: TfswsInAddr): Boolean;
Function FSWSGetLocalIPXAddr(Var aNetNum: TFSWSIPXNetNum;
  Var aAddr: TfswsIPXAddr): boolean;
Function FSWSGetRemoteHost(Const aName: TffNetName;
  Var aNetName: TffNetName; Var aAddr: TfswsInAddr): boolean;
Function FSWSGetRemoteNameFromAddr(aAddr: TfswsInAddr): TffNetName;
Procedure FSWSGetSocketOption(aSocket: TfswsSocket; aLevel, aOptName: Integer;
  Var aOptValue; aOptValueLen: Integer);
Procedure FSWSSetSocketOption(aSocket: TfswsSocket; aLevel, aOptName: Integer;
  Var aOptValue; aOptValueLen: Integer);

Const
  fscNumWinsockVersions = 2;
  { Number of supported Winsock versions. }

Var
  fsStrResWinsock: TfsStringResource; {in FSWSCNST.RC}

  { This array defines the Winsock versions supported by this module. }
  fsWinsockVerArray: Array[1..fscNumWinsockVersions] Of TfsWinsockVerRec =
  ((VerNum: $0101; ModuleName: 'wsock32.dll'), { WinSock 1 }
    (VerNum: $0202; ModuleName: 'ws2_32.dll')); { WinSock 2 }

Implementation

Var
  UnitInitializationDone: boolean;
  fswsLoadedWinsockVersion: TfswsWinsockVersion;
  WSLibHandle: THandle;
  LockFSWSInstalled: TRTLCriticalSection;

  {===EfsWinsockException==============================================}

Constructor EfsWinsockException.CreateTranslate(aErrorCode: Integer;
  aDummy: pointer);
Var
  ErrorMsg: TffShStr;
Begin
  ErrorMsg := fsStrResWinsock[aErrorCode];
  Inherited CreateEx(fsStrResGeneral, fserrWinsock, [aErrorCode, aErrorCode, ErrorMsg]);
End;
{===Macro expansion==================================================}

Function WSAMakeSyncReply(Buflen, Error: Word): Longint;
  Register;
Asm
  movzx eax, ax
  shl edx, 16
  or eax, edx
End;
{--------}

Function WSAMakeSelectReply(Event, Error: Word): Longint;
  Register;
Asm
  movzx eax, ax
  shl edx, 16
  or eax, edx
End;
{--------}

Function WSAGetAsyncBuflen(lParam: Longint): Integer;
  Register;
Asm
  and eax, $0000FFFF
End;
{--------}

Function WSAGetAsyncError(lParam: Longint): Integer;
  Register;
Asm
  shr eax, 16
End;
{--------}

Function WSAGetSelectEvent(lParam: Longint): Integer;
  Register;
Asm
  and eax, $0000FFFF
End;
{--------}

Function WSAGetSelectError(lParam: Longint): Integer;
  Register;
Asm
  shr eax, 16
End;
{====================================================================}

{===Unit initialization/finalization=================================}

Function FSWSInstalled: boolean;
Const
  ffcMaxProtoInfoRecords = 15;
Var
  aBuffer: PChar;
  pBuffer: PfswsProtocolInfo Absolute aBuffer;
  aCode: HFile;
  aCount: Integer;
  aFile: TOFStruct;
  anError: Integer;
  anIndex: Integer;
  anOffset: Integer;
  aProtocolInfo: PfswsProtocolInfo;
  aSize: DWORD;
  aVersion: Integer;
  WSData: TFSWSAData;
Begin
  EnterCriticalSection(LockFSWSInstalled);
  Try
    Result := (fswsLoadedWinsockVersion <> ffwvNone);

    { If this routine has already been called, exit. }
    If UnitInitializationDone Then
      Exit;
    { No matter what happens next, we've initialized. }
    UnitInitializationDone := True;
    fswsLoadedWinsockVersion := ffwvNone;
    aVersion := 0;

    { Load the Winsock DLL.  Note that we try to load the most recent
      Winsock version first. }
    For anIndex := fscNumWinsockVersions Downto 1 Do
      Begin

        {$IFDEF ForceWinSock1}
        If anIndex <> 1 Then Continue;
        {$ENDIF}

        { Check to see if the file exists before trying to load it }
        aCode := OpenFile(fsWinsockVerArray[anIndex].ModuleName, aFile, OF_EXIST);
        If aCode = HFILE_ERROR Then Continue;

        { If we get this far, we should have a good module -- load it }
        WSLibHandle := LoadLibrary(fsWinsockVerArray[anIndex].ModuleName);
        If WSLibHandle <> 0 Then
          Begin
            aVersion := anIndex;
            break;
          End;

      End;

    If (WSLibHandle = 0) Then
      Exit;
    {load and validate all pointers}
    @WinsockRoutines.accept := GetProcAddress(WSLibHandle, 'accept');
    If Not Assigned(WinsockRoutines.accept) Then Exit;

    @WinsockRoutines.bind := GetProcAddress(WSLibHandle, 'bind');
    If Not Assigned(WinsockRoutines.bind) Then Exit;

    @WinsockRoutines.closesocket := GetProcAddress(WSLibHandle, 'closesocket');
    If Not Assigned(WinsockRoutines.closesocket) Then Exit;

    @WinsockRoutines.connect := GetProcAddress(WSLibHandle, 'connect');
    If Not Assigned(WinsockRoutines.connect) Then Exit;

    @WinsockRoutines.getpeername := GetProcAddress(WSLibHandle, 'getpeername');
    If Not Assigned(WinsockRoutines.getpeername) Then Exit;

    @WinsockRoutines.getsockname := GetProcAddress(WSLibHandle, 'getsockname');
    If Not Assigned(WinsockRoutines.getsockname) Then Exit;

    @WinsockRoutines.getsockopt := GetProcAddress(WSLibHandle, 'getsockopt');
    If Not Assigned(WinsockRoutines.getsockopt) Then Exit;

    @WinsockRoutines.htonl := GetProcAddress(WSLibHandle, 'htonl');
    If Not Assigned(WinsockRoutines.htonl) Then Exit;

    @WinsockRoutines.htons := GetProcAddress(WSLibHandle, 'htons');
    If Not Assigned(WinsockRoutines.htons) Then Exit;

    @WinsockRoutines.inet_addr := GetProcAddress(WSLibHandle, 'inet_addr');
    If Not Assigned(WinsockRoutines.inet_addr) Then Exit;

    @WinsockRoutines.inet_ntoa := GetProcAddress(WSLibHandle, 'inet_ntoa');
    If Not Assigned(WinsockRoutines.inet_ntoa) Then Exit;

    @WinsockRoutines.ioctlsocket := GetProcAddress(WSLibHandle, 'ioctlsocket');
    If Not Assigned(WinsockRoutines.ioctlsocket) Then Exit;

    @WinsockRoutines.listen := GetProcAddress(WSLibHandle, 'listen');
    If Not Assigned(WinsockRoutines.listen) Then Exit;

    @WinsockRoutines.ntohl := GetProcAddress(WSLibHandle, 'ntohl');
    If Not Assigned(WinsockRoutines.ntohl) Then Exit;

    @WinsockRoutines.ntohs := GetProcAddress(WSLibHandle, 'ntohs');
    If Not Assigned(WinsockRoutines.ntohs) Then Exit;

    @WinsockRoutines.recv := GetProcAddress(WSLibHandle, 'recv');
    If Not Assigned(WinsockRoutines.recv) Then Exit;

    @WinsockRoutines.recvfrom := GetProcAddress(WSLibHandle, 'recvfrom');
    If Not Assigned(WinsockRoutines.recvfrom) Then Exit;

    @WinsockRoutines.select := GetProcAddress(WSLibHandle, 'select');
    If Not Assigned(WinsockRoutines.select) Then Exit;

    @WinsockRoutines.send := GetProcAddress(WSLibHandle, 'send');
    If Not Assigned(WinsockRoutines.send) Then Exit;

    @WinsockRoutines.sendto := GetProcAddress(WSLibHandle, 'sendto');
    If Not Assigned(WinsockRoutines.sendto) Then Exit;

    @WinsockRoutines.setsockopt := GetProcAddress(WSLibHandle, 'setsockopt');
    If Not Assigned(WinsockRoutines.setsockopt) Then Exit;

    @WinsockRoutines.shutdown := GetProcAddress(WSLibHandle, 'shutdown');
    If Not Assigned(WinsockRoutines.shutdown) Then Exit;

    @WinsockRoutines.socket := GetProcAddress(WSLibHandle, 'socket');
    If Not Assigned(WinsockRoutines.socket) Then Exit;

    @WinsockRoutines.gethostbyaddr := GetProcAddress(WSLibHandle, 'gethostbyaddr');
    If Not Assigned(WinsockRoutines.gethostbyaddr) Then Exit;

    @WinsockRoutines.gethostbyname := GetProcAddress(WSLibHandle, 'gethostbyname');
    If Not Assigned(WinsockRoutines.gethostbyname) Then Exit;

    @WinsockRoutines.gethostname := GetProcAddress(WSLibHandle, 'gethostname');
    If Not Assigned(WinsockRoutines.gethostname) Then Exit;

    @WinsockRoutines.getservbyport := GetProcAddress(WSLibHandle, 'getservbyport');
    If Not Assigned(WinsockRoutines.getservbyport) Then Exit;

    @WinsockRoutines.getservbyname := GetProcAddress(WSLibHandle, 'getservbyname');
    If Not Assigned(WinsockRoutines.getservbyname) Then Exit;

    @WinsockRoutines.getprotobynumber := GetProcAddress(WSLibHandle, 'getprotobynumber');
    If Not Assigned(WinsockRoutines.getprotobynumber) Then Exit;

    @WinsockRoutines.getprotobyname := GetProcAddress(WSLibHandle, 'getprotobyname');
    If Not Assigned(WinsockRoutines.getprotobyname) Then Exit;

    @WinsockRoutines.WSAStartup := GetProcAddress(WSLibHandle, 'WSAStartup');
    If Not Assigned(WinsockRoutines.WSAStartup) Then Exit;

    @WinsockRoutines.WSACleanup := GetProcAddress(WSLibHandle, 'WSACleanup');
    If Not Assigned(WinsockRoutines.WSACleanup) Then Exit;

    If aVersion > 1 Then
      Begin
        @WinsockRoutines.WSAEnumProtocols := GetProcAddress(WSLibHandle, 'WSAEnumProtocolsA');
        If Not Assigned(WinsockRoutines.WSAEnumProtocols) Then Exit;
      End;

    @WinsockRoutines.WSASetLastError := GetProcAddress(WSLibHandle, 'WSASetLastError');
    If Not Assigned(WinsockRoutines.WSASetLastError) Then Exit;

    @WinsockRoutines.WSAGetLastError := GetProcAddress(WSLibHandle, 'WSAGetLastError');
    If Not Assigned(WinsockRoutines.WSAGetLastError) Then Exit;

    @WinsockRoutines.WSAIsBlocking := GetProcAddress(WSLibHandle, 'WSAIsBlocking');
    If Not Assigned(WinsockRoutines.WSAIsBlocking) Then Exit;

    @WinsockRoutines.WSAUnhookBlockingHook := GetProcAddress(WSLibHandle, 'WSAUnhookBlockingHook');
    If Not Assigned(WinsockRoutines.WSAUnhookBlockingHook) Then Exit;

    @WinsockRoutines.WSASetBlockingHook := GetProcAddress(WSLibHandle, 'WSASetBlockingHook');
    If Not Assigned(WinsockRoutines.WSASetBlockingHook) Then Exit;

    @WinsockRoutines.WSACancelBlockingCall := GetProcAddress(WSLibHandle, 'WSACancelBlockingCall');
    If Not Assigned(WinsockRoutines.WSACancelBlockingCall) Then Exit;

    @WinsockRoutines.WSAAsyncGetServByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetServByName');
    If Not Assigned(WinsockRoutines.WSAAsyncGetServByName) Then Exit;

    @WinsockRoutines.WSAAsyncGetServByPort := GetProcAddress(WSLibHandle, 'WSAAsyncGetServByPort');
    If Not Assigned(WinsockRoutines.WSAAsyncGetServByPort) Then Exit;

    @WinsockRoutines.WSAAsyncGetProtoByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetProtoByName');
    If Not Assigned(WinsockRoutines.WSAAsyncGetProtoByName) Then Exit;

    @WinsockRoutines.WSAAsyncGetProtoByNumber := GetProcAddress(WSLibHandle, 'WSAAsyncGetProtoByNumber');
    If Not Assigned(WinsockRoutines.WSAAsyncGetProtoByNumber) Then Exit;

    @WinsockRoutines.WSAAsyncGetHostByName := GetProcAddress(WSLibHandle, 'WSAAsyncGetHostByName');
    If Not Assigned(WinsockRoutines.WSAAsyncGetHostByName) Then Exit;

    @WinsockRoutines.WSAAsyncGetHostByAddr := GetProcAddress(WSLibHandle, 'WSAAsyncGetHostByAddr');
    If Not Assigned(WinsockRoutines.WSAAsyncGetHostByAddr) Then Exit;

    @WinsockRoutines.WSACancelAsyncRequest := GetProcAddress(WSLibHandle, 'WSACancelAsyncRequest');
    If Not Assigned(WinsockRoutines.WSACancelAsyncRequest) Then Exit;

    @WinsockRoutines.WSAAsyncSelect := GetProcAddress(WSLibHandle, 'WSAAsyncSelect');
    If Not Assigned(WinsockRoutines.WSAAsyncSelect) Then Exit;

    { If we got here then we have succeeded. }
    If (WinsockRoutines.WSAStartup
      (fsWinsockVerArray[aVersion].VerNum, WSData) = 0) Then
      Begin
        fswsLoadedWinsockVersion := TfswsWinsockVersion(aVersion);

        { Determine which winsock families are installed.  Allocate a buffer that
          will hold several protocol records. }
        If aVersion > 1 Then
          Begin
            fswsFamiliesInstalled := [];
            { Allocate a buffer that we know is too small. }
            aSize := sizeOf(TfswsProtocolInfo);
            FFGetMem(aBuffer, 32);
            Try
              Fillchar(aBuffer^, 32, 0);
              aSize := 0;
              aCount := WinsockRoutines.WSAEnumProtocols(Nil, pBuffer, aSize);
              If aCount < 0 Then
                Begin
                  anError := WinsockRoutines.WSAGetLastError;
                  If anError = WSAENOBUFS Then
                    Begin
                      FFFreeMem(aBuffer, 32);
                      FFGetMem(aBuffer, aSize);
                      fillChar(aBuffer^, aSize, 0);
                      aCount := WinsockRoutines.WSAEnumProtocols(Nil, pBuffer, aSize);
                    End;
                End;
              If aCount > 0 Then
                Begin
                  anOffset := 0;
                  For anIndex := 1 To aCount Do
                    Begin
                      { Grab the record. }
                      aProtocolInfo := @(aBuffer[anOffset]);

                      { Is it a family we care about? }
                      Case aProtocolInfo^.iAddressFamily Of
                        Af_INET: include(fswsFamiliesInstalled, wfTCP);
                        Af_IPX: include(fswsFamiliesInstalled, wfIPX);
                      End; { case }

                      { Position to the next record. }
                      inc(anOffset, sizeOf(TfswsProtocolInfo));
                    End;
                End;
            Finally
              If aSize > 0 Then
                FFFreemem(aBuffer, aSize)
              Else
                FFFreemem(aBuffer, 32);
            End;
          End
        Else
          Begin
            { Winsock 1: Assume all families supported. }
            fswsFamiliesInstalled := [wfTCP, wfIPX];
          End;
      End;

  Finally
    LeaveCriticalSection(LockFSWSInstalled);
  End;
  Result := (fswsLoadedWinsockVersion <> ffwvNone);
End;
{--------}

Procedure FinalizeUnit;
Begin
  fsStrResWinsock.Free;
  DeleteCriticalSection(LockFSWSInstalled);
  If UnitInitializationDone Then
    Begin
      If (WSLibHandle <> 0) Then
        Begin
          If (fswsLoadedWinsockVersion <> ffwvNone) Then
            WinsockRoutines.WSACleanUp;
          FreeLibrary(WSLibHandle);
        End;
    End;
End;
{====================================================================}

{===FlashFiler helper routines=======================================}

Procedure FSWSAsyncSelect(aSocket: TfswsSocket;
  aWindow: HWnd;
  aEvent: Longint);
Var
  Error: Integer;
Begin
  If (WinsockRoutines.WSAAsyncSelect(aSocket, aWindow,
    FSWScEventComplete, aEvent) = SOCKET_ERROR) Then
    Begin
      Error := WinsockRoutines.WSAGetLastError;
      Raise EfsWinsockException.CreateTranslate(Error, Nil);
    End;
End;
{--------}

Function FSWSCreateSocket(aAF, aStruct, aProtocol: Integer): TfswsSocket;
Var
  Error: Integer;
Begin
  Result := WinsockRoutines.socket(aAF, aStruct, aProtocol);
  If (Result = INVALID_SOCKET) Then
    Begin
      Error := WinsockRoutines.WSAGetLastError;
      Raise EfsWinsockException.CreateTranslate(Error, Nil);
    End;
End;
{--------}

Function FSWSCvtAddrToStr(aAddr: TfswsInAddr): TffNetName;
Begin
  Result := FFStrPas(WinsockRoutines.inet_ntoa(aAddr));
End;
{--------}

Function FSWSCvtIPXAddrToStr(Const aNetNum: TFSWSIPXNetNum;
  Const aAddr: TfswsIPXAddr): TffNetName;
Const
  HexChars: String[16] = '0123456789ABCDEF';
Var
  i, j: Integer;
Begin
  {Begin !!.03}
  {$IFDEF IsDelphi}
  Result[0] := chr((2 * sizeof(TFSWSIPXNetNum)) +
    1 +
    (2 * sizeof(TfswsIPXAddr)) +
    5);
  {$ELSE}
  SetLength(Result, (2 * sizeof(TFSWSIPXNetNum)) + 1 +
    (2 * sizeof(TfswsIPXAddr)) + 5);
  {$ENDIF}
  {End !!.03}
  j := 0;
  For i := 0 To pred(sizeof(TFSWSIPXNetNum)) Do
    Begin
      Result[j + 1] := HexChars[(aNetNum[i] Shr 4) + 1];
      Result[j + 2] := HexChars[(aNetNum[i] And $F) + 1];
      inc(j, 2);
    End;
  inc(j);
  Result[j] := ':';
  For i := 0 To pred(sizeof(TfswsIPXAddr)) Do
    Begin
      If (i <> 0) Then
        Result[j] := '-';
      Result[j + 1] := HexChars[(aAddr[i] Shr 4) + 1];
      Result[j + 2] := HexChars[(aAddr[i] And $F) + 1];
      inc(j, 3);
    End;
End;
{--------}

Function FSWSCvtStrToAddr(aStr: TffNetName; Var aAddr: TfswsInAddr): boolean;
Var
  StrZ: TffStringZ;
Begin
  FFStrPCopy(StrZ, aStr);
  aAddr := TffWord32(WinsockRoutines.inet_addr(StrZ));
  Result := (aAddr <> INADDR_NONE);
End;
{--------}

Function FSWSCvtStrToIPXAddr(Const aStr: TffNetName;
  Var aNetNum: TFSWSIPXNetNum;
  Var aAddr: TfswsIPXAddr): boolean;
Var
  i, j: Integer;
  Nibble: Integer;
  Ch: char;
  DoUpper: boolean;
  DoNetNum: boolean;
Begin
  Nibble := 0;
  Result := False;
  j := 0;
  DoNetNum := True;
  DoUpper := True;
  For i := 1 To length(aStr) Do
    Begin
      Ch := upcase(aStr[i]);
      If ('0' <= Ch) And (Ch <= '9') Then
        Nibble := ord(Ch) - ord('0')
      Else If ('A' <= Ch) And (Ch <= 'F') Then
        Nibble := ord(Ch) - ord('A') + 10
      Else If (Ch <> '-') And (Ch <> ':') Then
        Exit;
      If (Ch = '-') Or (Ch = ':') Then
        Begin
          If DoNetNum Then
            j := 0;
          DoNetNum := False;
          DoUpper := True;
        End
      Else If DoUpper Then
        Begin
          If DoNetNum Then
            aNetNum[j] := Nibble Shl 4
          Else
            aAddr[j] := Nibble Shl 4;
          DoUpper := False;
        End
      Else
        Begin
          If DoNetNum Then
            aNetNum[j] := aNetNum[j] Or Nibble
          Else
            aAddr[j] := aAddr[j] Or Nibble;
          inc(j);
          DoUpper := True;
        End;
    End;
  Result := True;
End;
{--------}

Procedure FSWSDestroySocket(aSocket: TfswsSocket);
Begin
  If (aSocket <> INVALID_SOCKET) Then
    Begin
      WinsockRoutines.shutdown(aSocket, 2);
      WinsockRoutines.closesocket(aSocket);
    End;
End;
{--------}

Function FSWSGetLocalHosts(aList: TStrings): Boolean;
Type
  TaPInAddr = Array[0..255] Of PFFWord32;
  PaPInAddr = ^TaPInAddr;
Var
  ZStr: TffStringZ;
  HostEnt: PfswsHostEnt;
  IPAddress: TffNetName;
  pptr: PaPInAddr;
  Idx: Integer;

Begin
  aList.BeginUpdate;
  Try
    aList.Clear;
    aList.Add('<ALL INTERFACES>');
    Result := False;
    If (WinsockRoutines.gethostname(ZStr, SizeOf(ZStr)) = 0) Then
      Begin
        HostEnt := WinsockRoutines.gethostbyname(ZStr);
        If Assigned(HostEnt) Then
          Begin
            pptr := PaPInAddr(HostEnt^.h_addr_list);
            Idx := 0;
            While Assigned(pptr^[Idx]) Do
              Begin
                {pptr is assigned if any winsock based protocol is installed}
                {When IPX/SPX is installed, and TCP/IP is an IP address still
                 is returned. We must filter this out.}
                IPAddress := FSWSCvtAddrToStr(pptr^[Idx]^);
                If IPAddress <> '127.0.0.1' Then
                  aList.Add(Format('Adapter %D: %S', [Idx, IPAddress]));
                Inc(Idx);
              End;
            Result := True;
          End;
      End;
  Finally
    aList.EndUpdate;
  End;
End;
{--------}

Function FSWSGetLocalHostByNum(Const NIC: Integer;
  Var aNetName: TffNetName;
  Var aAddr: TfswsInAddr): Boolean;
Type
  TaPInAddr = Array[0..255] Of PffWord32;
  PaPInAddr = ^TaPInAddr;
Var
  ZStr: TffStringZ;
  HostEnt: PfswsHostEnt;
  pptr: PaPInAddr;
Begin
  Result := False;
  If (WinsockRoutines.gethostname(ZStr, SizeOf(ZStr)) = 0) Then
    Begin
      HostEnt := WinsockRoutines.gethostbyname(ZStr);
      If Assigned(HostEnt) Then
        Begin
          pptr := PaPInAddr(HostEnt^.h_addr_list);
          If NIC = -1 Then
            Begin
              aNetName := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
              aAddr := InAddr_ANY;
              Result := True;
            End
          Else
            Begin
              If Assigned(pptr^[NIC]) Then
                Begin
                  aNetName := FFStrPasLimit(HostEnt^.h_name, Pred(SizeOf(TffNetName)));
                  aAddr := pptr^[NIC]^;
                  Result := True;
                End;
            End;
        End;
    End;
End;
{--------}

Function FSWSGetLocalIPXAddr(Var aNetNum: TFSWSIPXNetNum;
  Var aAddr: TfswsIPXAddr): boolean;
Var
  Addr: TFSWSSockAddr;
  IPXInfo: TfswsIPXAddrInfo;
  S: TfswsSocket;
Begin
  // Create IPX socket.
  S := FSWSCreateSocket(AF_IPX, SOCK_DGRAM, NSPROTO_IPX);
  // Socket must be bound prior to calling IPX_ADDRESS
  FillChar(Addr, sizeof(Addr), 0);
  Addr.IPX.sipx_family := AF_IPX;
  WinsockRoutines.bind(S, Addr, sizeof(TFSWSSockAddrIPX));
  // Specify which adapter to check.
  FillChar(IPXInfo, sizeof(IPXInfo), 0);
  IPXInfo.adapternum := 0;
  FSWSGetSocketOption(S, NSPROTO_IPX, IPX_ADDRESS, IPXInfo, sizeof(IPXInfo));
  aNetNum := IPXInfo.netnum;
  aAddr := IPXInfo.nodenum;
  Result := True;
  // Destroy IPX socket.
  FSWSDestroySocket(S);
End;
{--------}

Function FSWSGetRemoteHost(Const aName: TffNetName;
  Var aNetName: TffNetName; Var aAddr: TfswsInAddr): boolean;
Var
  ZStr: TffStringZ;
  HostEnt: PfswsHostEnt;
Begin
  HostEnt := WinsockRoutines.gethostbyname(FFStrPCopy(ZStr, aName));
  If (HostEnt = Nil) Then
    Result := False
  Else
    Begin
      aAddr := PfswsInAddr((HostEnt^.h_addr)^)^;
      aNetName := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
      Result := True;
    End;
End;
{--------}

Function FSWSGetRemoteNameFromAddr(aAddr: TfswsInAddr): TffNetName;
Var
  HostEnt: PfswsHostEnt;
Begin
  HostEnt := WinsockRoutines.gethostbyaddr(aAddr, sizeof(aAddr), PF_INET);
  If (HostEnt = Nil) Then
    Result := ''
  Else
    Result := FFStrPasLimit(HostEnt^.h_name, pred(sizeof(TffNetName)));
End;
{--------}

Procedure FSWSGetSocketOption(aSocket: TfswsSocket; aLevel, aOptName: Integer;
  Var aOptValue; aOptValueLen: Integer);
Var
  Error: Integer;
Begin
  Error := WinsockRoutines.getsockopt(aSocket, aLevel, aOptName, aOptValue, aOptValueLen);
  If (Error = SOCKET_ERROR) Then
    Begin
      Error := WinsockRoutines.WSAGetLastError;
      Raise EfsWinsockException.CreateTranslate(Error, Nil);
    End;
End;
{--------}

Procedure FSWSSetSocketOption(aSocket: TfswsSocket; aLevel, aOptName: Integer;
  Var aOptValue; aOptValueLen: Integer);
Var
  Error: Integer;
Begin
  Error := WinsockRoutines.setsockopt(aSocket, aLevel, aOptName, aOptValue, aOptValueLen);
  If (Error = SOCKET_ERROR) Then
    Begin
      Error := WinsockRoutines.WSAGetLastError;
      Raise EfsWinsockException.CreateTranslate(Error, Nil);
    End;
End;
{====================================================================}

Initialization
  UnitInitializationDone := False;
  fswsLoadedWinsockVersion := ffwvNone;
  fsStrResWinsock := Nil;
  fsStrResWinsock := TfsStringResource.Create(hInstance, 'FS_WINSOCK_ERROR_STRINGS');
  InitializeCriticalSection(LockFSWSInstalled);

Finalization
  FinalizeUnit;

End.

