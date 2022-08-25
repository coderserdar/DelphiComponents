unit IPHLPAPI;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// Magenta Systems Internet Protocol Helper Component
// 26th November 2018 - Release 4.0 (C) Magenta Systems Ltd, 2018
// based on work by by Dirk Claessens

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

//------------------------------------------------------------------------------
//     Partial translation of  IPHLPAPI.DLL ( IP-Helper API )
// http://users.pandora.be/dirk.claessens2/
//     D. Claessens
//------------------------------------------------------------------------------
{
v1.3 - 18th September 2001
  Angus Robertson, Magenta Systems Ltd, England
     delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  All functions are dynamically loaded so program can be used on W95/NT4
  Added GetFriendlyIfIndex

v1.4 - 28th February 2002 - Angus
  Minor change to TIP_ADAPTER_INFO

v 1.5 - 26 July 2002 - Angus
  Added GetPerAdapterInfo and TIP_PER_ADAPTER_INFO

v 1.6 - 19 August 2002 - Angus
  Added AllocateAndGetTcpExTableFromStack and AllocateAndGetUdpExTableFromStack,
  which are APIs for XP only (not Vista), info from Netstatp at www.sysinternals.com
  Added MIB_TCP_STATE constants

v1.8 - 25th October 2005 - Angus

v1.9 - 8th August 2006 - Angus
   Corrected IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

v2.0 - 25th February 2007 - Angus
   Many more IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

v2.1 - 5th August 2008 - Angus
    Updated to be compatible with Delphi 2009
    Note there are only ANSI versions of the IP Helper APIs, no Wide/Unicode versions

v2.2 - 16th January 2009 - Angus
    Added GetAdaptersAddresses (XP and later) has IPv6 addresses (but IPv6 structures not done yet)
    Added GetExtendedTcpTable and GetExtendedUdpTable (XP SP2, W2K3 SP1, Vista and later),
      replacements for AllocateAndGetTcpExTableFromStack/etc

v2.3 - 3rd August 2009
    Changed ULONGLONG to LONGLONG for Delphi 7 compatability

v2.4 - 8th August 2010
    Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
    Removed packed for 64-bit compatibility in Delphi XE2 and later

v3.0 -  26th November 2018
    Only supporting XP SP3 and later, so removed AllocateAndGetTcpExTableFromStack/etc
   Added IPv6 support, numerous new structures and functions, Vista and later
   Added notification functions for interface changes
   Major clean up of Microsoft caps names with underscores to Delphi Txxxx type formats

Pending - IPv6 not yet supported for ARP or IP Routing table, sorry

}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface
uses
  Windows, winsock ;

const
  VERSION = '4.0';

//------------- headers from Microsoft IPTYPES.H--------------------------------

const
  ANY_SIZE = 1;
  TCPIP_OWNING_MODULE_SIZE = 16;
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  MAX_ADAPTER_NAME_LENGTH = 256; // arb.
  MAX_ADAPTER_ADDRESS_LENGTH = 8; // arb.
  DEFAULT_MINIMUM_ENTITIES = 32; // arb.
  MAX_HOSTNAME_LEN = 128; // arb.
  MAX_DOMAIN_NAME_LEN = 128; // arb.
  MAX_SCOPE_ID_LEN = 256; // arb.
  MAX_DHCPV6_DUID_LENGTH = 130; // RFC 3315

 // Node Types ( NETBIOS)
  BROADCAST_NODETYPE = 1;
  PEER_TO_PEER_NODETYPE = 2;
  MIXED_NODETYPE = 4;
  HYBRID_NODETYPE = 8;

  NETBIOSTypes  : array[0..8] of string =
    ( 'UNKNOWN', 'BROADCAST', 'PEER_TO_PEER', '', 'MIXED', '', '', '', 'HYBRID'
      );

// Adapter Types
{  IF_OTHER_ADAPTERTYPE = 1;    // 8 August 2006 corrected literals to
MIB_IF_TYPE_xx in ipifcons.h
  IF_ETHERNET_ADAPTERTYPE = 6;
  IF_TOKEN_RING_ADAPTERTYPE = 9;
  IF_FDDI_ADAPTERTYPE = 15;
  IF_PPP_ADAPTERTYPE = 23;
  IF_LOOPBACK_ADAPTERTYPE = 24;
  IF_SLIP_ADAPTERTYPE = 28;   }

//  Adapted from Ipifcons.h : // JP Turchi, 9 Feb 2007

  //MIN_IF_TYPE                     = 1;

  IF_TYPE_OTHER                   = 1;   // None of the below
  IF_TYPE_REGULAR_1822            = 2;
  IF_TYPE_HDH_1822                = 3;
  IF_TYPE_DDN_X25                 = 4;
  IF_TYPE_RFC877_X25              = 5;
  IF_TYPE_ETHERNET_CSMACD         = 6;
  IF_TYPE_IS088023_CSMACD         = 7;
  IF_TYPE_ISO88024_TOKENBUS       = 8;
  IF_TYPE_ISO88025_TOKENRING      = 9;
  IF_TYPE_ISO88026_MAN            = 10;
  IF_TYPE_STARLAN                 = 11;
  IF_TYPE_PROTEON_10MBIT          = 12;
  IF_TYPE_PROTEON_80MBIT          = 13;
  IF_TYPE_HYPERCHANNEL            = 14;
  IF_TYPE_FDDI                    = 15;
  IF_TYPE_LAP_B                   = 16;
  IF_TYPE_SDLC                    = 17;
  IF_TYPE_DS1                     = 18;  // DS1-MIB
  IF_TYPE_E1                      = 19;  // Obsolete; see DS1-MIB
  IF_TYPE_BASIC_ISDN              = 20;
  IF_TYPE_PRIMARY_ISDN            = 21;
  IF_TYPE_PROP_POINT2POINT_SERIAL = 22;  // proprietary serial
  IF_TYPE_PPP                     = 23;
  IF_TYPE_SOFTWARE_LOOPBACK       = 24;
  IF_TYPE_EON                     = 25;  // CLNP over IP
  IF_TYPE_ETHERNET_3MBIT          = 26;
  IF_TYPE_NSIP                    = 27;  // XNS over IP
  IF_TYPE_SLIP                    = 28;  // Generic Slip
  IF_TYPE_ULTRA                   = 29;  // ULTRA Technologies
  IF_TYPE_DS3                     = 30;  // DS3-MIB
  IF_TYPE_SIP                     = 31;  // SMDS, coffee
  IF_TYPE_FRAMERELAY              = 32;  // DTE only
  IF_TYPE_RS232                   = 33;
  IF_TYPE_PARA                    = 34;  // Parallel port
  IF_TYPE_ARCNET                  = 35;
  IF_TYPE_ARCNET_PLUS             = 36;
  IF_TYPE_ATM                     = 37;  // ATM cells
  IF_TYPE_MIO_X25                 = 38;
  IF_TYPE_SONET                   = 39;  // SONET or SDH
  IF_TYPE_X25_PLE                 = 40;
  IF_TYPE_ISO88022_LLC            = 41;
  IF_TYPE_LOCALTALK               = 42;
  IF_TYPE_SMDS_DXI                = 43;
  IF_TYPE_FRAMERELAY_SERVICE      = 44;  // FRNETSERV-MIB
  IF_TYPE_V35                     = 45;
  IF_TYPE_HSSI                    = 46;
  IF_TYPE_HIPPI                   = 47;
  IF_TYPE_MODEM                   = 48;  // Generic Modem
  IF_TYPE_AAL5                    = 49;  // AAL5 over ATM
  IF_TYPE_SONET_PATH              = 50;
  IF_TYPE_SONET_VT                = 51;
  IF_TYPE_SMDS_ICIP               = 52;  // SMDS InterCarrier Interface
  IF_TYPE_PROP_VIRTUAL            = 53;  // Proprietary virtual/internal
  IF_TYPE_PROP_MULTIPLEXOR        = 54;  // Proprietary multiplexing
  IF_TYPE_IEEE80212               = 55;  // 100BaseVG
  IF_TYPE_FIBRECHANNEL            = 56;
  IF_TYPE_HIPPIINTERFACE          = 57;
  IF_TYPE_FRAMERELAY_INTERCONNECT = 58;  // Obsolete, use 32 or 44
  IF_TYPE_AFLANE_8023             = 59;  // ATM Emulated LAN for 802.3
  IF_TYPE_AFLANE_8025             = 60;  // ATM Emulated LAN for 802.5
  IF_TYPE_CCTEMUL                 = 61;  // ATM Emulated circuit
  IF_TYPE_FASTETHER               = 62;  // Fast Ethernet (100BaseT)
  IF_TYPE_ISDN                    = 63;  // ISDN and X.25
  IF_TYPE_V11                     = 64;  // CCITT V.11/X.21
  IF_TYPE_V36                     = 65;  // CCITT V.36
  IF_TYPE_G703_64K                = 66;  // CCITT G703 at 64Kbps
  IF_TYPE_G703_2MB                = 67;  // Obsolete; see DS1-MIB
  IF_TYPE_QLLC                    = 68;  // SNA QLLC
  IF_TYPE_FASTETHER_FX            = 69;  // Fast Ethernet (100BaseFX)
  IF_TYPE_CHANNEL                 = 70;
  IF_TYPE_IEEE80211               = 71;  // Radio spread spectrum
  IF_TYPE_IBM370PARCHAN           = 72;  // IBM System 360/370 OEMI Channel
  IF_TYPE_ESCON                   = 73;  // IBM Enterprise Systems Connection
  IF_TYPE_DLSW                    = 74;  // Data Link Switching
  IF_TYPE_ISDN_S                  = 75;  // ISDN S/T interface
  IF_TYPE_ISDN_U                  = 76;  // ISDN U interface
  IF_TYPE_LAP_D                   = 77;  // Link Access Protocol D
  IF_TYPE_IPSWITCH                = 78;  // IP Switching Objects
  IF_TYPE_RSRB                    = 79;  // Remote Source Route Bridging
  IF_TYPE_ATM_LOGICAL             = 80;  // ATM Logical Port
  IF_TYPE_DS0                     = 81;  // Digital Signal Level 0
  IF_TYPE_DS0_BUNDLE              = 82;  // Group of ds0s on the same ds1
  IF_TYPE_BSC                     = 83;  // Bisynchronous Protocol
  IF_TYPE_ASYNC                   = 84;  // Asynchronous Protocol
  IF_TYPE_CNR                     = 85;  // Combat Net Radio
  IF_TYPE_ISO88025R_DTR           = 86;  // ISO 802.5r DTR
  IF_TYPE_EPLRS                   = 87;  // Ext Pos Loc Report Sys
  IF_TYPE_ARAP                    = 88;  // Appletalk Remote Access Protocol
  IF_TYPE_PROP_CNLS               = 89;  // Proprietary Connectionless Proto
  IF_TYPE_HOSTPAD                 = 90;  // CCITT-ITU X.29 PAD Protocol
  IF_TYPE_TERMPAD                 = 91;  // CCITT-ITU X.3 PAD Facility
  IF_TYPE_FRAMERELAY_MPI          = 92;  // Multiproto Interconnect over FR
  IF_TYPE_X213                    = 93;  // CCITT-ITU X213
  IF_TYPE_ADSL                    = 94;  // Asymmetric Digital Subscrbr Loop
  IF_TYPE_RADSL                   = 95;  // Rate-Adapt Digital Subscrbr Loop
  IF_TYPE_SDSL                    = 96;  // Symmetric Digital Subscriber Loop
  IF_TYPE_VDSL                    = 97;  // Very H-Speed Digital Subscrb Loop
  IF_TYPE_ISO88025_CRFPRINT       = 98;  // ISO 802.5 CRFP
  IF_TYPE_MYRINET                 = 99;  // Myricom Myrinet
  IF_TYPE_VOICE_EM                = 100; // Voice recEive and transMit
  IF_TYPE_VOICE_FXO               = 101; // Voice Foreign Exchange Office
  IF_TYPE_VOICE_FXS               = 102; // Voice Foreign Exchange Station
  IF_TYPE_VOICE_ENCAP             = 103; // Voice encapsulation
  IF_TYPE_VOICE_OVERIP            = 104; // Voice over IP encapsulation
  IF_TYPE_ATM_DXI                 = 105; // ATM DXI
  IF_TYPE_ATM_FUNI                = 106; // ATM FUNI
  IF_TYPE_ATM_IMA                 = 107; // ATM IMA
  IF_TYPE_PPPMULTILINKBUNDLE      = 108; // PPP Multilink Bundle
  IF_TYPE_IPOVER_CDLC             = 109; // IBM ipOverCdlc
  IF_TYPE_IPOVER_CLAW             = 110; // IBM Common Link Access to Workstn
  IF_TYPE_STACKTOSTACK            = 111; // IBM stackToStack
  IF_TYPE_VIRTUALIPADDRESS        = 112; // IBM VIPA
  IF_TYPE_MPC                     = 113; // IBM multi-proto channel support
  IF_TYPE_IPOVER_ATM              = 114; // IBM ipOverAtm
  IF_TYPE_ISO88025_FIBER          = 115; // ISO 802.5j Fiber Token Ring
  IF_TYPE_TDLC                    = 116; // IBM twinaxial data link control
  IF_TYPE_GIGABITETHERNET         = 117;
  IF_TYPE_HDLC                    = 118;
  IF_TYPE_LAP_F                   = 119;
  IF_TYPE_V37                     = 120;
  IF_TYPE_X25_MLP                 = 121; // Multi-Link Protocol
  IF_TYPE_X25_HUNTGROUP           = 122; // X.25 Hunt Group
  IF_TYPE_TRANSPHDLC              = 123;
  IF_TYPE_INTERLEAVE              = 124; // Interleave channel
  IF_TYPE_FAST                    = 125; // Fast channel
  IF_TYPE_IP                      = 126; // IP (for APPN HPR in IP networks)
  IF_TYPE_DOCSCABLE_MACLAYER      = 127; // CATV Mac Layer
  IF_TYPE_DOCSCABLE_DOWNSTREAM    = 128; // CATV Downstream interface
  IF_TYPE_DOCSCABLE_UPSTREAM      = 129; // CATV Upstream interface
  IF_TYPE_A12MPPSWITCH            = 130; // Avalon Parallel Processor
  IF_TYPE_TUNNEL                  = 131; // Encapsulation interface
  IF_TYPE_COFFEE                  = 132; // Coffee pot
  IF_TYPE_CES                     = 133; // Circuit Emulation Service
  IF_TYPE_ATM_SUBINTERFACE        = 134; // ATM Sub Interface
  IF_TYPE_L2_VLAN                 = 135; // Layer 2 Virtual LAN using 802.1Q
  IF_TYPE_L3_IPVLAN               = 136; // Layer 3 Virtual LAN using IP
  IF_TYPE_L3_IPXVLAN              = 137; // Layer 3 Virtual LAN using IPX
  IF_TYPE_DIGITALPOWERLINE        = 138; // IP over Power Lines
  IF_TYPE_MEDIAMAILOVERIP         = 139; // Multimedia Mail over IP
  IF_TYPE_DTM                     = 140; // Dynamic syncronous Transfer Mode
  IF_TYPE_DCN                     = 141; // Data Communications Network
  IF_TYPE_IPFORWARD               = 142; // IP Forwarding Interface
  IF_TYPE_MSDSL                   = 143; // Multi-rate Symmetric DSL
  IF_TYPE_IEEE1394                = 144; // IEEE1394 High Perf Serial Bus
  IF_TYPE_IF_GSN = 145;           // following added Oct 2014
  IF_TYPE_DVBRCC_MACLAYER = 146;
  IF_TYPE_DVBRCC_DOWNSTREAM = 147;
  IF_TYPE_DVBRCC_UPSTREAM = 148;
  IF_TYPE_ATM_VIRTUAL = 149;
  IF_TYPE_MPLS_TUNNEL = 150;
  IF_TYPE_SRP = 151;
  IF_TYPE_VOICEOVERATM = 152;
  IF_TYPE_VOICEOVERFRAMERELAY = 153;
  IF_TYPE_IDSL = 154;
  IF_TYPE_COMPOSITELINK = 155;
  IF_TYPE_SS7_SIGLINK = 156;
  IF_TYPE_PROP_WIRELESS_P2P = 157;
  IF_TYPE_FR_FORWARD = 158;
  IF_TYPE_RFC1483 = 159;
  IF_TYPE_USB = 160;
  IF_TYPE_IEEE8023AD_LAG = 161;
  IF_TYPE_BGP_POLICY_ACCOUNTING = 162;
  IF_TYPE_FRF16_MFR_BUNDLE = 163;
  IF_TYPE_H323_GATEKEEPER = 164;
  IF_TYPE_H323_PROXY = 165;
  IF_TYPE_MPLS = 166;
  IF_TYPE_MF_SIGLINK = 167;
  IF_TYPE_HDSL2 = 168;
  IF_TYPE_SHDSL = 169;
  IF_TYPE_DS1_FDL = 170;
  IF_TYPE_POS = 171;
  IF_TYPE_DVB_ASI_IN = 172;
  IF_TYPE_DVB_ASI_OUT = 173;
  IF_TYPE_PLC = 174;
  IF_TYPE_NFAS = 175;
  IF_TYPE_TR008 = 176;
  IF_TYPE_GR303_RDT = 177;
  IF_TYPE_GR303_IDT = 178;
  IF_TYPE_ISUP = 179;
  IF_TYPE_PROP_DOCS_WIRELESS_MACLAYER = 180;
  IF_TYPE_PROP_DOCS_WIRELESS_DOWNSTREAM = 181;
  IF_TYPE_PROP_DOCS_WIRELESS_UPSTREAM = 182;
  IF_TYPE_HIPERLAN2 = 183;
  IF_TYPE_PROP_BWA_P2MP = 184;
  IF_TYPE_SONET_OVERHEAD_CHANNEL = 185;
  IF_TYPE_DIGITAL_WRAPPER_OVERHEAD_CHANNEL = 186;
  IF_TYPE_AAL2 = 187;
  IF_TYPE_RADIO_MAC = 188;
  IF_TYPE_ATM_RADIO = 189;
  IF_TYPE_IMT = 190;
  IF_TYPE_MVL = 191;
  IF_TYPE_REACH_DSL = 192;
  IF_TYPE_FR_DLCI_ENDPT = 193;
  IF_TYPE_ATM_VCI_ENDPT = 194;
  IF_TYPE_OPTICAL_CHANNEL = 195;
  IF_TYPE_OPTICAL_TRANSPORT = 196;
  IF_TYPE_IEEE80216_WMAN = 237;
  IF_TYPE_WWANPP = 243; // WWAN devices based on GSM technology
  IF_TYPE_WWANPP2 = 244;    // WWAN devices based on CDMA technology

  MAX_IF_TYPE                     = 244;

//-------------from other MS header files---------------------------------------

  MAX_INTERFACE_NAME_LEN = 256; { mrapi.h }
  MAXLEN_PHYSADDR = 8; { iprtrmib.h }
  MAXLEN_IFDESCR = 256; { --"---     }

type
  TAddressFamily = Integer;
  TNetIfIndex = Integer;

// information for IPv6 stuff missing from winsock
  PScopeLevel = ^TScopeLevel;
  TScopeLevel = (
    ScopeLevelInterface    = 1,
    ScopeLevelLink         = 2,
    ScopeLevelSubnet       = 3,
    ScopeLevelAdmin        = 4,
    ScopeLevelSite         = 5,
    ScopeLevelOrganization = 8,
    ScopeLevelGlobal       = 14,
    ScopeLevelCount        = 16);

  PScopeID = ^TScopeID;
  TScopeID = record
      Value: ULONG; // Dummy actually a record with C bitfields
  end;

  PIn6Addr = ^in6_addr;
  in6_addr = record
    case integer of
      0: (S6_addr: array [0..15] of Byte);
      1: (u6_addr16: array [0..7] of Word);
  end;

  PInAddr6 = ^TInAddr6;
  TInAddr6 = record
    case integer of
      0: (S6_addr: array [0..15] of Byte);
      1: (u6_addr8: array [0..15] of Byte);
      2: (u6_addr16: array [0..7] of Word);
      3: (u6_addr32: array [0..3] of Integer);
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = record
    sin6_family:   u_short;     // AF_INET6
    sin6_port:     u_short;     // Transport level port number
    sin6_flowinfo: u_long;      // IPv6 flow information
    sin6_addr:     TInAddr6;    // IPv6 address
    case integer of
      0: (sin6_scope_id: u_long);      // Set of interfaces for a scope.
      1: (sin6_scope_struct: TScopeID);
  end;

  PIPv6MReq = ^TIPv6MReq;
  TIPv6MReq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: u_long;   // Interface index.
  end;

  sockaddr_in6_w2ksp1 = record
    sin6_family: u_short;
    sin6_port: u_short;
    sin6_flowinfo: u_long;
    sin6_addr: TInAddr6;
    sin6_scope_id: u_long;
  end;

  PSockAddrInet = ^TSockAddrInet;
  TSockAddrInet = record               // both IPv4 and IPv6 versions, check family
    case Word of
     1: (Ipv4: TSockAddrIn;);
     2: (Ipv6: TSockAddrIn6;);
     3: (si_family: TAddressFamily;);
  end;

// Structure to hold a pair of source, destination addresses. }
type
  _sockaddr_in6_pair = record
    SourceAddress: PSockAddrIn6;
    DestinationAddress: PSockAddrIn6;
  end;
  TSockAddrIn6Pair = _sockaddr_in6_pair;
  PSockAddrIn6Pair = ^_sockaddr_in6_pair;

type
  PSocketAddress = ^TSocketAddress;
  TSocketAddress = record
    lpSockaddr: PSockAddrInet;       // both IPv4 and IPv6 versions, check family or length
    iSockaddrLength: Integer;
  end;

const
{ Address families. - Oct 2014 }
  AF_UNSPEC       = 0;               { unspecified }
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  AF_NS           = 6;               { XEROX NS protocols }
  AF_IPX          = AF_NS;           { IPX and SPX }
  AF_ISO          = 7;               { ISO protocols }
  AF_OSI          = AF_ISO;          { OSI is ISO }
  AF_ECMA         = 8;               { european computer manufacturers }
  AF_DATAKIT      = 9;               { datakit protocols }
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  AF_SNA          = 11;              { IBM SNA }
  AF_DECnet       = 12;              { DECnet }
  AF_DLI          = 13;              { Direct data link interface }
  AF_LAT          = 14;              { LAT }
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  AF_APPLETALK    = 16;              { AppleTalk }
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  AF_VOICEVIEW    = 18;              { VoiceView }
  AF_FIREFOX      = 19;              { FireFox }
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  AF_BAN          = 21;              { Banyan }
  AF_INET6        = 23;              { Internetwork Version 6 }
  AF_CLUSTER      = 24;              { Microsoft Wolfpack }
  AF_12844        = 25;              { IEEE 1284.4 WG AF }
  AF_IRDA         = 26;              { IrDA }
  AF_NETDES       = 28;              { Network Designers OSI & gateway enabled protocols }
  AF_TCNPROCESS   = 29;
  AF_TCNMESSAGE   = 30;
  AF_ICLFXBM      = 31;
  AF_MAX          = 32;


//------------------------------------------------------------------------------

type
  TMacAddress = array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;

type
  PNetLuid = ^TNetLuid;
  TNetLuid = record
    case Word of
     1: (Value: Int64;);
     2: (Reserved: Int64;);
     3: (NetLuidIndex: Int64;);
     4: (IfType: Int64;);
  end;
 TIFLuid = TNetLuid;
 PIFLuid = PNetLuid;

type
  TIpPrefixOrigin = (
    IpPrefixOriginOther,
    IpPrefixOriginManual,
    IpPrefixOriginWellKnown,
    IpPrefixOriginDhcp,
    IpPrefixOriginRouterAdvertisement);

  TIpSuffixOrigin = (
    IpSuffixOriginOther,
    IpSuffixOriginManual,
    IpSuffixOriginWellKnown,
    IpSuffixOriginDhcp,
    IpSuffixOriginLinkLayerAddress,
    IpSuffixOriginRandom);

  TIpDadState= (
    IpDadStateInvalid,
    IpDadStateTentative,
    IpDadStateDuplicate,
    IpDadStateDeprecated,
    IpDadStatePreferred);

//------IP address structures---------------------------------------------------

  PIpAddressString = ^TIpAddressString;
  TIpAddressString = array[0..15] of AnsiChar; //  IP as string
  //
  PIpAddrString = ^TIpAddrString;
  TIpAddrString = record // for use in linked lists
    Next: PIpAddrString;
    IpAddress: TIpAddressString;
    IpMask: TIpAddressString;
    Context: DWORD;
  end;

//----------Fixed Info STRUCTURES---------------------------------------------

  PTFixedInfo = ^TFixedInfo;
  TFixedInfo = record
    HostName: array[1..MAX_HOSTNAME_LEN + 4] of AnsiChar;    // Angus
    DomainName: array[1..MAX_DOMAIN_NAME_LEN + 4] of AnsiChar;   // Angus
    CurrentDNSServer: PIpAddrString;
    DNSServerList: TIpAddrString;
    NodeType: UINT;
    ScopeID: array[1..MAX_SCOPE_ID_LEN + 4] of AnsiChar;   // Angus
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDNS: UINT;
  end;

//----------INTERFACE STRUCTURES-------------------------------------------------

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// The following are the the operational states for WAN and LAN interfaces. //
// The order of the states seems weird, but is done for a purpose. All      //
// states >= CONNECTED can transmit data right away. States >= DISCONNECTED //
// can tx data but some set up might be needed. States < DISCONNECTED can   //
// not transmit data.                                                       //
// A card is marked UNREACHABLE if DIM calls InterfaceUnreachable for       //
// reasons other than failure to connect.                                   //
//                                                                          //
// NON_OPERATIONAL -- Valid for LAN Interfaces. Means the card is not       //
//                      working or not plugged in or has no address.        //
// UNREACHABLE     -- Valid for WAN Interfaces. Means the remote site is    //
//                      not reachable at this time.                         //
// DISCONNECTED    -- Valid for WAN Interfaces. Means the remote site is    //
//                      not connected at this time.                         //
// CONNECTING      -- Valid for WAN Interfaces. Means a connection attempt  //
//                      has been initiated to the remote site.              //
// CONNECTED       -- Valid for WAN Interfaces. Means the remote site is    //
//                      connected.                                          //
// OPERATIONAL     -- Valid for LAN Interfaces. Means the card is plugged   //
//                      in and working.                                     //
//                                                                          //
// It is the users duty to convert these values to MIB-II values if they    //
// are to be used by a subagent                                             //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

const
// Angus added from ipifcons.h
  IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
  IF_OPER_STATUS_UNREACHABLE = 1 ;
  IF_OPER_STATUS_DISCONNECTED = 2 ;
  IF_OPER_STATUS_CONNECTING = 3 ;
  IF_OPER_STATUS_CONNECTED = 4 ;
  IF_OPER_STATUS_OPERATIONAL = 5 ;

  MIB_IF_TYPE_OTHER = 1 ;
  MIB_IF_TYPE_ETHERNET = 6 ;
  MIB_IF_TYPE_TOKENRING = 9 ;
  MIB_IF_TYPE_FDDI = 15 ;
  MIB_IF_TYPE_PPP = 23 ;
  MIB_IF_TYPE_LOOPBACK = 24 ;
  MIB_IF_TYPE_SLIP = 28 ;
  MIB_IF_TYPE_ATM = 37;
  MIB_IF_TYPE_IEEE80211 = 71;
  MIB_IF_TYPE_TUNNEL = 131;
  MIB_IF_TYPE_IEEE1394 = 144;
  MIB_IF_TYPE_IEEE80216_WMAN = 237;
  MIB_IF_TYPE_WWANPP = 243;
  MIB_IF_TYPE_WWANPP2 = 244;


  MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
  MIB_IF_OPER_STATUS_UNREACHABLE = 1 ;
  MIB_IF_OPER_STATUS_DISCONNECTED = 2 ;
  MIB_IF_OPER_STATUS_CONNECTING = 3 ;
  MIB_IF_OPER_STATUS_CONNECTED = 4 ;
  MIB_IF_OPER_STATUS_OPERATIONAL = 5 ;

  MIB_TCP_STATE_CLOSED = 1 ;
  MIB_TCP_STATE_LISTEN = 2 ;
  MIB_TCP_STATE_SYN_SENT = 3 ;
  MIB_TCP_STATE_SYN_RCVD = 4 ;
  MIB_TCP_STATE_ESTAB = 5 ;
  MIB_TCP_STATE_FIN_WAIT1 = 6 ;
  MIB_TCP_STATE_FIN_WAIT2 = 7 ;
  MIB_TCP_STATE_CLOSE_WAIT = 8 ;
  MIB_TCP_STATE_CLOSING = 9 ;
  MIB_TCP_STATE_LAST_ACK = 10 ;
  MIB_TCP_STATE_TIME_WAIT = 11 ;
  MIB_TCP_STATE_DELETE_TCB = 12 ;

// wtype for MIB_IPADDRROW - Nov 2014
const
  MIB_IPADDR_PRIMARY = $0001;        // Primary ipaddr
  MIB_IPADDR_DYNAMIC = $0004;        // Dynamic ipaddr
  MIB_IPADDR_DISCONNECTED = $0008;   // Address is on disconnected interface
  MIB_IPADDR_DELETED = $0040;        // Address being deleted
  MIB_IPADDR_TRANSIENT = $0080;      // Transient address
  MIB_IPADDR_DNS_ELIGIBLE = $0100;   // Address is published in DNS.

// Bit values of IP_ADAPTER_UNICAST_ADDRESS Flags field.

const
  IP_ADAPTER_ADDRESS_DNS_ELIGIBLE   = $01;
  IP_ADAPTER_ADDRESS_TRANSIENT      = $02;

// Bit values of IP_ADAPTER_ADDRESSES Flags field.

const
  IP_ADAPTER_DDNS_ENABLED               = $00000001;
  IP_ADAPTER_REGISTER_ADAPTER_SUFFIX    = $00000002;
  IP_ADAPTER_DHCP_ENABLED               = $00000004;
  IP_ADAPTER_RECEIVE_ONLY               = $00000008;
  IP_ADAPTER_NO_MULTICAST               = $00000010;
  IP_ADAPTER_IPV6_OTHER_STATEFUL_CONFIG = $00000020;
  IP_ADAPTER_NETBIOS_OVER_TCPIP_ENABLED = $00000040;
  IP_ADAPTER_IPV4_ENABLED               = $00000080;
  IP_ADAPTER_IPV6_ENABLED               = $00000100;
  IP_ADAPTER_IPV6_MANAGE_ADDRESS_CONFIG = $00000200;

// Flags used as argument to GetAdaptersAddresses().
// "SKIP" flags are added when the default is to include the information.
// "INCLUDE" flags are added when the default is to skip the information.

const
  GAA_FLAG_SKIP_UNICAST = $0001;
  GAA_FLAG_SKIP_ANYCAST = $0002;
  GAA_FLAG_SKIP_MULTICAST = $0004;
  GAA_FLAG_SKIP_DNS_SERVER = $0008;
  GAA_FLAG_INCLUDE_PREFIX = $0010;
  GAA_FLAG_SKIP_FRIENDLY_NAME = $0020;
  GAA_FLAG_INCLUDE_WINS_INFO = $0040;
  GAA_FLAG_INCLUDE_GATEWAYS = $0080;
  GAA_FLAG_INCLUDE_ALL_INTERFACES = $0100;
  GAA_FLAG_INCLUDE_ALL_COMPARTMENTS = $0200;
  GAA_FLAG_INCLUDE_TUNNEL_BINDINGORDER = $0400;

type
// OperStatus for GetAdaptersAddresses().
  TIfOperStatus = (
    IF_OPER_STATUS_NONE,
    IF_OPER_STATUS_UP {= 1},
    IF_OPER_STATUS_DOWN {= 2},
    IF_OPER_STATUS_TESTING {= 3},
    IF_OPER_STATUS_UNKNOWN {= 4},
    IF_OPER_STATUS_DORMANT {= 5},
    IF_OPER_STATUS_NOT_PRESENT {= 6},
    IF_OPER_STATUS_LOWER_LAYER_DOWN {= 7 } );

  TAdminStatus = (
    IF_ADMIN_STATUS_None,
    IF_ADMIN_STATUS_UP, { = 1 }
    IF_ADMIN_STATUS_DOWN, { = 2 }
    IF_ADMIN_STATUS_TESTING {= 3 } ) ;

{/// Define compartment ID type: }
type
    Puint32 = ^DWORD;
    TNetIfCompartmentId = Puint32;
    TNetIfNetworkGuid = TGUID;

const
    NET_IF_COMPARTMENT_ID_UNSPECIFIED = 0;
    NET_IF_COMPARTMENT_ID_PRIMARY = 1;

    NET_IF_LINK_SPEED_UNKNOWN: Int64 = -1;

// Define datalink interface access types.
type
  TNetIfAccessTtype = (
    NET_IF_ACCESS_UNKNOWN,
    NET_IF_ACCESS_LOOPBACK,
    NET_IF_ACCESS_BROADCAST,
    NET_IF_ACCESS_POINT_TO_POINT,
    NET_IF_ACCESS_POINT_TO_MULTI_POINT,
    NET_IF_ACCESS_MAXIMUM );

// Define datalink interface direction types.
  TNetIfDirectionType = (
    NET_IF_DIRECTION_SENDRECEIVE,
    NET_IF_DIRECTION_SENDONLY,
    NET_IF_DIRECTION_RECEIVEONLY,
    NET_IF_DIRECTION_MAXIMUM  );

  TNetIfConnectionType = (
    NET_IF_CONNECTION_UNKNOWN,
    NET_IF_CONNECTION_DEDICATED,
    NET_IF_CONNECTION_PASSIVE,
    NET_IF_CONNECTION_DEMAND,
    NET_IF_CONNECTION_MAXIMUM );

  TNetIfMediaConnectState = (
    MediaConnectStateUnknown,
    MediaConnectStateConnected,
    MediaConnectStateDisconnected  );

  TMibIfTableLevel = (
    MibIfTableNormal,
    MibIfTableRaw);

// Types of tunnels (sub-type of IF_TYPE when IF_TYPE is IF_TYPE_TUNNEL). }
  TTunnelType = (
    TUNNEL_TYPE_NONE {= 0},
    TUNNEL_TYPE_OTHER {= 1},
    TUNNEL_TYPE_DIRECT {= 2},
    unused3,
    unused4,
    unused5,
    unused6,
    unused7,
    unused8,
    unused9,
    unused10,
    TUNNEL_TYPE_6TO4 {= 11},
    unused12,
    TUNNEL_TYPE_ISATAP {= 13},
    TUNNEL_TYPE_TEREDO {= 14} );

// Medium the Ndis Driver is running on (OID_GEN_MEDIA_SUPPORTED/ OID_GEN_MEDIA_IN_USE, MediaType).
  TNdisMedium = (
    NdisMedium802_3,
    NdisMedium802_5,
    NdisMediumFddi,
    NdisMediumWan,
    NdisMediumLocalTalk,
    NdisMediumDix,
    NdisMediumArcnetRaw,
    NdisMediumArcnet878_2,
    NdisMediumAtm,
    NdisMediumWirelessWan,
    NdisMediumIrda,
    NdisMediumBpc,
    NdisMediumCoWan,
    NdisMedium1394,
    NdisMediumInfiniBand,
    NdisMediumTunnel,
    NdisMediumNative802_11,
    NdisMediumLoopback,
    NdisMediumWiMax,
    NdisMediumIP );

// Physical Medium Type definitions. Used with OID_GEN_PHYSICAL_MEDIUM.
  TNdisPhysicalMedium = (
    NdisPhysicalMediumUnspecified,
    NdisPhysicalMediumWirelessLan,
    NdisPhysicalMediumCableModem,
    NdisPhysicalMediumPhoneLine,
    NdisPhysicalMediumPowerLine,
    NdisPhysicalMediumDSL,      // includes ADSL and UADSL (G.Lite)
    NdisPhysicalMediumFibreChannel,
    NdisPhysicalMedium1394,
    NdisPhysicalMediumWirelessWan,
    NdisPhysicalMediumNative802_11,
    NdisPhysicalMediumBluetooth,
    NdisPhysicalMediumInfiniband,
    NdisPhysicalMediumWiMax,
    NdisPhysicalMediumUWB,
    NdisPhysicalMedium802_3,
    NdisPhysicalMedium802_5,
    NdisPhysicalMediumIrda,
    NdisPhysicalMediumWiredWAN,
    NdisPhysicalMediumWiredCoWan,
    NdisPhysicalMediumOther,
    NdisPhysicalMediumMax);       // Not a real physical type, defined as an upper-bound

// Oct 2014
  TRouterDiscoveryBehaviour = (RouterDiscoveryDisabled, RouterDiscoveryEnabled,
                               RouterDiscoveryDhcp, RouterDiscoveryUnchanged = -1);

  TBandwidthFlag = (NlbwDisabled, NlbwEnabled, NlbwUnchanged = -1);

  TPathBandwidthRod = record
    Bandwidth: LONGLONG;
    Instability: LONGLONG;
    BandwidthPeaked: Boolean;
  end;

  TNetworkCategort = (NetworkCategoryPublic, NetworkCategoryPrivate,
                      NetworkCategoryDomainAuthenticated,
                      NetworkCategoryUnchanged = -1, NetworkCategoryUnknown = -1);

  TInterfaceOffloadRod = (NlatUnspecified, NlatUnicast, NlatAnycast, NlatMulticast,
                          NlatBroadcast, NlatInvalid );

  TRouteOrigin = (NlroManual, NlroWellKnown, NlroDHCP, NlroRouterAdvertisement, Nlro6to4);

  TNeighborState = (NlnsUnreachable, NlnsIncomplete, NlnsProbe, NlnsDelay,
                    NlnsStale, NlnsReachable,  NlnsPermanent, NlnsMaximum );

  TLinkLocalAddressBehavior = (LinkLocalAlwaysOff, LinkLocalDelayed, LinkLocalAlwaysOn,
                               LinkLocalUnchanged = -1);


  TTcpConnectionOffloadState = (
    TcpConnectionOffloadStateInHost,
    TcpConnectionOffloadStateOffloading,
    TcpConnectionOffloadStateOffloaded,
    TcpConnectionOffloadStateUploading,
    TcpConnectionOffloadStateMax);

// ParameterChange.
  TMibNoticationType = (MibParameterNotification,
        MibAddInstance, MibDeleteInstance, MibInitialNotification);
//  PMIB_NOTIFICATION_TYPE = ^MIB_NOTIFICATION_TYPE;

// RouteProtocol - Too complex for type
const
  MibIpProtoOther   = 1;
  MibIpProtoLocal   = 2;
  MibIpProtoNetMgmt = 3;
  MibIpProtoIcmp    = 4;
  MibIpProtoEgp     = 5;
  MibIpProtoGgp     = 6;
  MibIpProtoHello   = 7;
  MibIpProtoRip     = 8;
  MibIpProtoIsIs    = 9;
  MibIpProtoEsIs    = 10;
  MibIpProtoCisco   = 11;
  MibIpProtoBbn     = 12;
  MibIpProtoOspf    = 13;
  MibIpProtoBgp     = 14;
  MibIpProtoNtAutostatic   = 10002;
  MibIpProtoNTStatic       = 10006;
  MibIpProtoNTStaticNonDod = 10007;



type
  TIpAddressPrefix = record
    Prefix: TSockAddrInet;
    PrefixLength: byte;
  end;
  PIpAddressPrefix = ^TIpAddressPrefix;

type
  PTMibIfRow = ^TMibIfRow;   // Windows 2000 and later, replaced by MibIfRow2
  TMibIfRow = record
    wszName: array[1..MAX_INTERFACE_NAME_LEN] of WCHAR;
    dwIndex: DWORD;
    dwType: DWORD;       // see MIB_IF_TYPE and IF_TYPE_xx
    dwMTU: DWORD;
    dwSpeed: DWORD;
    dwPhysAddrLen: DWORD;
    bPhysAddr: array[1..MAXLEN_PHYSADDR] of byte;
    AdminStatus: TAdminStatus;    // see MIB_IF_ADMIN_STATUS
    OperStatus: TIfOperStatus;     // see MIB_IF_OPER_STATUS
    dwLastChange: DWORD;
    dwInOctets: DWORD;
    dwInUcastPkts: DWORD;
    dwInNUCastPkts: DWORD;
    dwInDiscards: DWORD;
    dwInErrors: DWORD;
    dwInUnknownProtos: DWORD;
    dwOutOctets: DWORD;
    dwOutUCastPkts: DWORD;
    dwOutNUCastPkts: DWORD;
    dwOutDiscards: DWORD;
    dwOutErrors: DWORD;
    dwOutQLen: DWORD;
    dwDescrLen: DWORD;
    bDescr: array[1..MAXLEN_IFDESCR] of AnsiChar; //byte;
  end;

 //
  PTMibIfTable = ^TMIBIfTable;
  TMibIfTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIfRow;
  end;

// 27 Oct 2014 -
const
// maximum string size in -wchar- units
    IF_MAX_STRING_SIZE = 256;
    IF_MAX_PHYS_ADDRESS_LENGTH = 32;

type
  TInterfaceAndOperStatus = (
    HardwareInterface,
    FilterInterface,
    ConnectorPresent,
    NotAuthenticated,
    NotMediaConnected,
    Paused,
    LowPower,
    EndPointInterface);

  TInterfaceAndOperStatusFlags = set of TInterfaceAndOperStatus;

  PMibIFRow2 = ^TMibIFRow2;        // Vista and later
  TMibIFRow2 = record
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  // Read-Only fields.
    InterfaceGuid: TGUID;
    Alias: Array[0..IF_MAX_STRING_SIZE] of WCHAR;
    Description: Array[0..IF_MAX_STRING_SIZE] of WCHAR;
    PhysicalAddressLength: ULONG;
    PhysicalAddress: Array[0..IF_MAX_PHYS_ADDRESS_LENGTH-1] of byte;
    PermanentPhysicalAddress: Array[0..IF_MAX_PHYS_ADDRESS_LENGTH-1] of byte;
    Mtu: ULONG;
    IfType: DWORD;       // see MIB_IF_TYPE  and IF_TYPE_xx
    TunnelType: TTunnelType;
    MediaType: TNdisMedium;
    PhysicalMediumType: TNdisPhysicalMedium;
    AccessType: TNetIfAccessTtype;
    DirectionType: TNetIfDirectionType;
    InterfaceAndOperStatusFlags: TInterfaceAndOperStatusFlags;
    OperStatus: TIfOperStatus;
    AdminStatus: TAdminStatus;    // see MIB_IF_ADMIN_STATUS
    MediaConnectState: TNetIfMediaConnectState;
    NetworkGuid:  TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
  // Statistics.
    TransmitLinkSpeed: LONGLONG;
    ReceiveLinkSpeed: LONGLONG;
    InOctets: LONGLONG;
    InUcastPkts: LONGLONG;
    InNUcastPkts: LONGLONG;
    InDiscards: LONGLONG;
    InErrors: LONGLONG;
    InUnknownProtos: LONGLONG;
    InUcastOctets: LONGLONG;
    InMulticastOctets: LONGLONG;
    InBroadcastOctets: LONGLONG;
    OutOctets: LONGLONG;
    OutUcastPkts: LONGLONG;
    OutNUcastPkts: LONGLONG;
    OutDiscards: LONGLONG;
    OutErrors: LONGLONG;
    OutUcastOctets: LONGLONG;
    OutMulticastOctets: LONGLONG;
    OutBroadcastOctets: LONGLONG;
    OutQLen: LONGLONG;
  end;

  PTMibIfTable2 = ^TMIBIfTable2;
  TMibIfTable2 = record
    NumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIfRow2;
  end;


//------ADAPTER INFO STRUCTURES-------------------------------------------------

  PIpAdapterInfo = ^TIpAdapterInfo;
  TIpAdapterInfo = record
    Next: PIpAdapterInfo;
    ComboIndex: DWORD;
    AdapterName: array[1..MAX_ADAPTER_NAME_LENGTH + 4] of AnsiChar;       // Angus
    Description: array[1..MAX_ADAPTER_DESCRIPTION_LENGTH + 4] of AnsiChar;    // Angus
    AddressLength: UINT;
    Address: array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;      // Angus
    Index: DWORD;
    aType: UINT;
    DHCPEnabled: UINT;
    CurrentIPAddress: PIpAddrString;
    IPAddressList: TIpAddrString;
    GatewayList: TIpAddrString;
    DHCPServer: TIpAddrString;
    HaveWINS: BOOL;
    PrimaryWINSServer: TIpAddrString;
    SecondaryWINSServer: TIpAddrString;
    LeaseObtained: LongInt ; // UNIX time, seconds since 1970
    LeaseExpires: LongInt;   // UNIX time, seconds since 1970
    SpareStuff: array [1..200] of AnsiChar ;   // Angus - space for IP address lists
  end;

  PIpPerAdapterInfo = ^TIpPerAdapterInfo;  // Angus
  TIpPerAdapterInfo = record
    AutoconfigEnabled: UINT;
    AutoconfigActive: UINT;
    CurrentDnsServer: PIpAddrString;
    DnsServerList: TIpAddrString;
    SpareStuff: array [1..200] of AnsiChar ;   // space for IP address lists
  end;

// 12 Jan 2009 new stuff for GetAdaptersAddresses, requires winsock2

  PIpAdapterUnicastAddress = ^TIpAdapterUnicastAddress;
  TIpAdapterUnicastAddress = record
    Union: record
    case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterUnicastAddress;
    Address: TSocketAddress;
    PrefixOrigin: TIpSuffixOrigin;
    SuffixOrigin: TIpSuffixOrigin;
    DadState: TIpDadState;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;
  end;

  PIpAdapterAnycaseAddress = ^TIpAdapterAnycaseAddress;
  TIpAdapterAnycaseAddress = record
    Union: record
      case Integer of
        0: (Alignment: int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterAnycaseAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterMulticastAddress = ^TIpAdapterMulticastAddress;
  TIpAdapterMulticastAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Flags: DWORD);
    end;
    Next: PIpAdapterMulticastAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterDnsServerAddress = ^TIpAdapterDnsServerAddress;
  TIpAdapterDnsServerAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterDnsServerAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterPrefix = ^TIpAdapterPrefix;
  TIpAdapterPrefix = record
    Union: record
    case Integer of
      0: (Alignment: LONGLONG);
      1: (Length: ULONG; Flags: DWORD);
    end;
    Next: PIpAdapterPrefix;
    Address: TSocketAddress;
    PrefixLength: ULONG;
  end;

  PIpAdapterWinsServerAddress = ^TIpAdapterWinsServerAddress;
  TIpAdapterWinsServerAddress = record
   Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterWinsServerAddress;
    Address: TSocketAddress;
  end;

  PIpAdapterGatewayAddress = ^TIpAdapterGatewayAddress;
  TIpAdapterGatewayAddress = record
    Union: record
      case Integer of
        0: (Alignment: Int64);
        1: (Length: DWORD; Reserved: DWORD);
    end;
    Next: PIpAdapterGatewayAddress;
    Address: TSocketAddress;
  end;


// linked records (NEXT) filled by GetAdaptersAddresses(), XP and later, some elements XP SP1, some Vista
// length: XP SP3=144, Vista=
type
  PIpAdapterAddresses = ^TIpAdapterAddresses;
  TIpAdapterAddresses = record
    Union: record
      case Integer of
        0: (Alignment: int64);
        1: (Length: DWORD;
            IfIndex: DWORD);
    end;
    Next: PIpAdapterAddresses;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIpAdapterUnicastAddress;
    FirstAnycastAddress: PIpAdapterAnycaseAddress;
    FirstMulticastAddress: PIpAdapterMulticastAddress;
    FirstDnsServerAddress: PIpAdapterDnsServerAddress;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: DWORD;
    OperStatus: TIfOperStatus;        // last element for XP no SP
    Ipv6IfIndex: DWORD;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIpAdapterPrefix;    // last element for XP SP1
    TransmitLinkSpeed: Int64;           // following elements Vista and later
    ReceiveLinkSpeed: Int64;
    FirstWinsServerAddress: PIpAdapterWinsServerAddress;
    FirstGatewayAddress: PIpAdapterGatewayAddress;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: TIfLuid;
    Dhcpv4Server: TSocketAddress;
    CompartmentId: TNetIfCompartmentId;
    NetworkGuid: TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
    TunnelType: TTunnelType;
    // DHCP v6 Info.
    Dhcpv6Server: TSocketAddress;
    Dhcpv6ClientDuid: array [0..MAX_DHCPV6_DUID_LENGTH] of byte;
    Dhcpv6ClientDuidLength: ULONG;
    Dhcpv6Iaid: ULONG;
  end;

type
    PMibIPInterfaceRow = ^TMibIPInterfaceRow;
    TMibIPInterfaceRow = record
     // Key Structure;
        Family: TAddressFamily;
        InterfaceLuid: TNetLuid;
        InterfaceIndex: TNetIfIndex;
    // Read-Write fields.
    // Fields currently not exposed.
        MaxReassemblySize: ULONG;
        InterfaceIdentifier: LONGLONG;
        MinRouterAdvertisementInterval: ULONG;
        MaxRouterAdvertisementInterval: ULONG;
    // Fileds currently exposed.
        AdvertisingEnabled: BOOLEAN;
        ForwardingEnabled: BOOLEAN;
        WeakHostSend: BOOLEAN;
        WeakHostReceive: BOOLEAN;
        UseAutomaticMetric: BOOLEAN;
        UseNeighborUnreachabilityDetection: BOOLEAN;
        ManagedAddressConfigurationSupported: BOOLEAN;
        OtherStatefulConfigurationSupported: BOOLEAN;
        AdvertiseDefaultRoute: BOOLEAN;
        RouterDiscoveryBehavior: TRouterDiscoveryBehaviour;
        DadTransmits: ULONG;             // DupAddrDetectTransmits in RFC 2462.
        BaseReachableTime: ULONG;
        RetransmitTime: ULONG;
        PathMtuDiscoveryTimeout: ULONG;  // Path MTU discovery timeout (in ms).
        LinkLocalAddressBehavior: TLinkLocalAddressBehavior;
        LinkLocalAddressTimeout: ULONG;  // In ms.
        ZoneIndices : array[0..Ord(ScopeLevelCount)-1] of ULONG; // Zone part of a SCOPE_ID.
        SitePrefixLength: ULONG;
        Metric: ULONG;
        NlMtu: ULONG;
    // Read Only fields.
        Connected: BOOLEAN;
        SupportsWakeUpPatterns: BOOLEAN;
        SupportsNeighborDiscovery: BOOLEAN;
        SupportsRouterDiscovery: BOOLEAN;
        ReachableTime: ULONG;
        TransmitOffload: TInterfaceOffloadRod;
        ReceiveOffload: TInterfaceOffloadRod;
    // Disables using default route on the interface. This flag
    // can be used by VPN clients to restrict Split tunnelling.
        DisableDefaultRoutes: BOOLEAN;
    end;

type
    PMibIPInterfaceTable = ^TMibIPInterfaceTable;
    TMibIPInterfaceTable = record
        NumEntries: ULONG;
        Table : array[0..ANY_SIZE-1] of TMibIPInterfaceRow;
    end;

type
    PMibIpForwardRow2 = ^TMibIpForwardRow2;
    TMibIpForwardRow2 = record
        InterfaceLuid: TNetLuid;
        InterfaceIndex: TNetIfIndex;
        DestinationPrefix: TIpAddressPrefix;
        NextHop: TSockAddrInet;
      // Read-Write Fields.
        SitePrefixLength: UCHAR;
        ValidLifetime: ULONG;
        PreferredLifetime: ULONG;
        Metric: ULONG;
        Protocol: ULONG; // TRouteProtocol;
        Loopback: BOOLEAN;
        AutoconfigureAddress: BOOLEAN;
        Publish: BOOLEAN;
        Immortal: BOOLEAN;
      // Read-Only Fields.
        Age: ULONG;
        Origin: TRouteOrigin;
    end;

type
  PMibIpForwardTable2 = ^TMibIpForwardTable2;
  TMibIpForwardTable2 = record
    NumEntries: ULONG;
    Table: Array[0..ANY_SIZE-1] of TMibIpForwardRow2;
  end;

  PMibUnicastIpAddressRow = ^TMibUnicastIpAddressRow;
  TMibUnicastIpAddressRow = record
  // Key Structure.
    Address: TSockAddrInet;
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  // Read-Write Fileds.
    PrefixOrigin: TIpPrefixOrigin;
    SuffixOrigin: TIpSuffixOrigin;
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    OnLinkPrefixLength: byte;
    SkipAsSource: BOOLEAN;
  // Read-Only Fields.
    DadState: TIpDadState;
    ScopeId: TScopeId;
    CreationTimeStamp: TFileTime; {LARGE_INTEGER}
  end;

  PMibUnicastIpAddressTable = ^TMibUnicastIpAddressTable;
  TMibUnicastIpAddressTable = record
    NumEntries: ULONG;
    Table: Array[0..ANY_SIZE-1] of TMibUnicastIpAddressRow;
  end;

  PMibAnycastIpAddressRow = ^TMibAnycastIpAddressRow;
  TMibAnycastIpAddressRow = record
 // Key Structure.
    Address: TSockAddrInet;
    InterfaceLuid: TNetLuid;
    InterfaceIndex: TNetIfIndex;
  //Read-Only Fields. }
    ScopeId: TScopeId;
  end;

  PMibAnycastIpAddressTable = ^TMibAnycastIpAddressTable;
  TMibAnycastIpAddressTable = record
    NumEntries: ULONG;
    Table: array[0..ANY_SIZE - 1] of TMibAnycastIpAddressRow;
  end;

  PMibMulticastIpAddressRow = ^TMibMulticastIpAddressRow;
  TMibMulticastIpAddressRow = record
  // Key Structure.
    Address: TSockAddrInet;
    InterfaceIndex: TNetIfIndex;
    InterfaceLuid: TNetLuid;
  // Read-Only Fields.
    ScopeId: TScopeId;
  end;

  PMibMulticastIpAddressTable = ^TMibMulticastIpAddressTable;
  TMibMulticastIpAddressTable = record
    NumEntries: ULONG;
    Table: array[0..ANY_SIZE - 1] of TMibMulticastIpAddressRow;
  end;

//----------------TCP STRUCTURES------------------------------------------------

  PTMibTCPRow = ^TMibTCPRow;
  TMibTCPRow = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
  end;
  //
  PTMibTCPTable = ^TMibTCPTable;
  TMibTCPTable = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCPRow;
  end;
  //
  PTMibTCP6Row = ^TMibTCP6Row;
  TMibTCP6Row = record
    dwState: DWORD;
    LocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    RemoteAddr: IN6_ADDR;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
  end;
  //
  PTMibTCP6Table = ^TMibTCP6Table;
  TMibTCP6Table = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCP6Row;
  end;
  //
  PTMibTCPRow2 = ^TMibTCPRow2;
  TMibTCPRow2 = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: LongInt;
    dwOffloadState: TTcpConnectionOffloadState;
  end;
  //
  PTMibTCPTable2 = ^TMibTCPTable2;
  TMibTCPTable2 = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCPRow2;
  end;
  //
  PTMibTCP6Row2 = ^TMibTCP6Row2;
  TMibTCP6Row2 = record
    LocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    RemoteAddr: IN6_ADDR;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: LongInt;
    dwOffloadState: TTcpConnectionOffloadState;
  end;
  //
  PTMibTCP6Table2 = ^TMibTCP6Table2;
  TMibTCP6Table2 = record
    dwNumEntries: DWORD;
    Table: array[0..0] of TMibTCP6Row2;
  end;
  //
  PTMibTCPStats = ^TMibTCPStats;
  TMibTCPStats = record
    dwRTOAlgorithm: DWORD;
    dwRTOMin: DWORD;
    dwRTOMax: DWORD;
    dwMaxConn: DWORD;
    dwActiveOpens: DWORD;
    dwPassiveOpens: DWORD;
    dwAttemptFails: DWORD;
    dwEstabResets: DWORD;
    dwCurrEstab: DWORD;
    dwInSegs: DWORD;
    dwOutSegs: DWORD;
    dwRetransSegs: DWORD;
    dwInErrs: DWORD;
    dwOutRsts: DWORD;
    dwNumConns: DWORD;
  end;

//---------UDP STRUCTURES-------------------------------------------------------

  PTMibUDPRow = ^TMibUDPRow;
  TMibUDPRow = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
  end;
 //
  PTMibUDPTable = ^TMIBUDPTable;
  TMIBUDPTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibUDPRow;
  end;
 //
  PTMibUDP6Row = ^TMibUDP6Row;
  TMibUDP6Row = record
    dwLocalAddr: IN6_ADDR;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
  end;
 //
  PTMibUDP6Table = ^TMIBUDP6Table;
  TMIBUDP6Table = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibUDP6Row;
  end;
 //
  PTMibUdpStats = ^TMIBUdpStats;
  TMIBUdpStats = record
    dwInDatagrams: DWORD;
    dwNoPorts: DWORD;
    dwInErrors: DWORD;
    dwOutDatagrams: DWORD;
    dwNumAddrs: DWORD;
  end;

//-----------IP STRUCTURES------------------------------------------------------

 //
  PTMibIPNetRow = ^TMibIPNetRow;
  TMibIPNetRow = record
    dwIndex: DWord;
    dwPhysAddrLen: DWord;
    bPhysAddr: TMACAddress;
    dwAddr: DWord;
    dwType: DWord;
  end;
  //
  PTMibIPNetTable = ^TMibIPNetTable;
  TMibIPNetTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPNetRow;
  end;
  //
  PTMibIPStats = ^TMibIPStats;
  TMibIPStats = record
    dwForwarding: DWORD;
    dwDefaultTTL: DWORD;
    dwInReceives: DWORD;
    dwInHdrErrors: DWORD;
    dwInAddrErrors: DWORD;
    dwForwDatagrams: DWORD;
    dwInUnknownProtos: DWORD;
    dwInDiscards: DWORD;
    dwInDelivers: DWORD;
    dwOutRequests: DWORD;
    dwRoutingDiscards: DWORD;
    dwOutDiscards: DWORD;
    dwOutNoRoutes: DWORD;
    dwReasmTimeOut: DWORD;
    dwReasmReqds: DWORD;
    dwReasmOKs: DWORD;
    dwReasmFails: DWORD;
    dwFragOKs: DWORD;
    dwFragFails: DWORD;
    dwFragCreates: DWORD;
    dwNumIf: DWORD;
    dwNumAddr: DWORD;
    dwNumRoutes: DWORD;
  end;
  //
  PTMibIPAddrRow = ^TMibIPAddrRow;
  TMibIPAddrRow = record
    dwAddr: DWORD;
    dwIndex: DWORD;
    dwMask: DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    Unused1: WORD;
    wType: WORD;  // XP and later - MIB_IPADDR_xx literals - Nov 2014
  end;
  //
  PTMibIPAddrTable = ^TMibIPAddrTable;
  TMibIPAddrTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPAddrRow;
  end;

  //
  PTMibIPForwardRow = ^TMibIPForwardRow;
  TMibIPForwardRow = record
    dwForwardDest: DWORD;
    dwForwardMask: DWORD;
    dwForwardPolicy: DWORD;
    dwForwardNextHop: DWORD;
    dwForwardIFIndex: DWORD;
    dwForwardType: DWORD;
    dwForwardProto: DWORD;
    dwForwardAge: DWORD;
    dwForwardNextHopAS: DWORD;
    dwForwardMetric1: DWORD;
    dwForwardMetric2: DWORD;
    dwForwardMetric3: DWORD;
    dwForwardMetric4: DWORD;
    dwForwardMetric5: DWORD;
  end;
  //
  PTMibIPForwardTable = ^TMibIPForwardTable;
  TMibIPForwardTable = record
    dwNumEntries: DWORD;
    Table: array[0..ANY_SIZE - 1] of TMibIPForwardRow;
  end;

//--------ICMP-STRUCTURES------------------------------------------------------

  PTMibICMPStats = ^TMibICMPStats;
  TMibICMPStats = record
    dwMsgs: DWORD;
    dwErrors: DWORD;
    dwDestUnreachs: DWORD;
    dwTimeEcxcds: DWORD;
    dwParmProbs: DWORD;
    dwSrcQuenchs: DWORD;
    dwRedirects: DWORD;
    dwEchos: DWORD;
    dwEchoReps: DWORD;
    dwTimeStamps: DWORD;
    dwTimeStampReps: DWORD;
    dwAddrMasks: DWORD;
    dwAddrReps: DWORD;
  end;

  PTMibICMPInfo = ^TMibICMPInfo;
  TMibICMPInfo = record
    InStats: TMibICMPStats;
    OutStats: TMibICMPStats;
  end;

// 13 Jan 2009 - GetExtendedTcpTable and GetExtendedUdpTable structures, XP SP2, Vista and better

type
   TTcpTableClass = (
    TCP_TABLE_BASIC_LISTENER,
    TCP_TABLE_BASIC_CONNECTIONS,
    TCP_TABLE_BASIC_ALL,
    TCP_TABLE_OWNER_PID_LISTENER,
    TCP_TABLE_OWNER_PID_CONNECTIONS,
    TCP_TABLE_OWNER_PID_ALL,
    TCP_TABLE_OWNER_MODULE_LISTENER,
    TCP_TABLE_OWNER_MODULE_CONNECTIONS,
    TCP_TABLE_OWNER_MODULE_ALL) ;

  TUdpTableClass = (
    UDP_TABLE_BASIC,
    UDP_TABLE_OWNER_PID,
    UDP_TABLE_OWNER_MODULE );

  TTcpIpOwnerModuleInfoClass = (
    TcpIpOwnerModuleInfoClassBasic  );

  TTcpIpOwnerModuleBasicInfo = record
    pModuleName: PWCHAR;
    pModulePath: PWCHAR;
  end;
  PTcpIpOwnerModuleBasicInfo = ^TTcpIpOwnerModuleBasicInfo;

  TTcpIpOwnerModuleBasicInfoEx = record
    TcpIpOwnerModuleBasicInfo: TTcpIpOwnerModuleBasicInfo ;
    Buffer: Array[0..1024] of byte;  // space for module name and path
  end;

  TMibTcpRowOwnerPID = record
    dwState: LongInt;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibTcpRowOwnerPID = ^TMibTcpRowOwnerPID;

  TMibTcpTableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcpRowOwnerPID;
  end;
  PTMibTcpTableOwnerPID = ^TMibTcpTableOwnerPID;

  TMibTcp6RowOwnerPID = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    ucRemoteAddr: TInAddr6;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibTcp6RowOwnerPID = ^TMibTcp6RowOwnerPID;

  TMibTcp6TableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcp6RowOwnerPID;
  end;
  PTMibTcp6TableOwnerPID = ^TMibTcp6TableOwnerPID;

  TMibTcpRowOwnerModule = record
    dwState: DWORD;
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwRemoteAddr: DWORD;
    dwRemotePort: DWORD;
    dwOwningPid: DWORD;
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibTcpRowOwnerModule = ^TMibTcpRowOwnerModule;

  TMibTcpTableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcpRowOwnerModule;
  end;
  PTMibTcpTableOwnerModule = ^TMibTcpTableOwnerModule;

// Oct 2014
  TMibTcp6RowOwnerModule = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    ucRemoteAddr: TInAddr6;
    dwRemoteScopeId: DWORD;
    dwRemotePort: DWORD;
    dwState: DWORD;
    dwOwningPid: DWORD;
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibTcp6RowOwnerModule = ^TMibTcp6RowOwnerModule;

  TMibTcp6TableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibTcp6RowOwnerModule;
  end;
  PTMibTcp6TableOwnerModule = ^TMibTcp6TableOwnerModule;

  TMibUdpRowOwnerPID = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibUdpRowOwnerPID = ^TMibUdpRowOwnerPID;

  _MIB_UDPTABLE_OWNER_PID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdpRowOwnerPID;
  end;
  TMibUdpTableOwnerPID = _MIB_UDPTABLE_OWNER_PID;
  PTMibUdpTableOwnerPID = ^_MIB_UDPTABLE_OWNER_PID;

  TMibUdp6RowOwnerPID = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
  end;
  PTMibUdp6RowOwnerPID = ^TMibUdp6RowOwnerPID;

  TMibUdp6TableOwnerPID = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdp6RowOwnerPID;
  end;
  PTMibUdp6TableOwnerPID = ^TMibUdp6TableOwnerPID;

  TMibUdpRowOwnerModule = record
    dwLocalAddr: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
    unknown: DWORD;  // Angus - had to add this dummy element so the record is the correct length and timestamp works
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    SpecificPortBind: integer;
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibUdpRowOwnerModule = ^TMibUdpRowOwnerModule;

  TMibUdpTableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdpRowOwnerModule;
  end;
  PTMibUdpTableOwnerModule = ^TMibUdpTableOwnerModule;

  TMibUdp6RowOwnerModule = record
    ucLocalAddr: TInAddr6;
    dwLocalScopeId: DWORD;
    dwLocalPort: DWORD;
    dwOwningPid: DWORD;
    unknown: DWORD;  // Angus - had to add this dummy element so the record is the correct length and timestamp works
    liCreateTimestamp: TFileTime; {LARGE_INTEGER}
    SpecificPortBind: integer;
    OwningModuleInfo: Array[0..TCPIP_OWNING_MODULE_SIZE-1] of LONGLONG;
  end;
  PTMibUdp6RowOwnerModule = ^TMibUdp6RowOwnerModule;

  TMibUdp6TableOwnerModule = record
    dwNumEntries: DWORD;
    table: Array[0..ANY_SIZE-1] of TMibUdp6RowOwnerModule;
  end;
  PTMibUdp6TableOwnerModule = ^TMibUdp6TableOwnerModule;

//------------------imports from IPHLPAPI.DLL-----------------------------------

var

GetAdaptersInfo: function ( pAdapterInfo: PIpAdapterInfo; pOutBufLen: PULONG ): DWORD; stdcall;

GetPerAdapterInfo: function (IfIndex: ULONG; pPerAdapterInfo: PIpPerAdapterInfo; pOutBufLen: PULONG):DWORD; stdcall;

GetNetworkParams: function ( FixedInfo: PTFixedInfo; pOutPutLen: PULONG ): DWORD; stdcall;

GetTcpTable: function ( pTCPTable: PTMibTCPTable; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;

GetTcpStatistics: function ( pStats: PTMibTCPStats ): DWORD; stdcall;

GetUdpTable: function ( pUdpTable: PTMibUDPTable; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;

GetUdpStatistics: function ( pStats: PTMibUdpStats ): DWORD; stdcall;

GetIpStatistics: function ( pStats: PTMibIPStats ): DWORD; stdcall;

GetIpNetTable: function ( pIpNetTable: PTMibIPNetTable; pdwSize: PULONG;  bOrder: BOOL ): DWORD; stdcall;

GetIpAddrTable: function ( pIpAddrTable: PTMibIPAddrTable; pdwSize: PULONG; bOrder: BOOL ): DWORD; stdcall;

GetIpForwardTable: function ( pIPForwardTable: PTMibIPForwardTable; pdwSize: PULONG; bOrder: BOOL ): DWORD; stdCall;

GetIcmpStatistics: function ( pStats: PTMibICMPInfo ): DWORD; stdCall;

GetRTTAndHopCount: function ( DestIPAddress: DWORD; HopCount: PULONG;  MaxHops: ULONG; RTT: PULONG ): BOOL; stdCall;

GetIfTable: function ( pIfTable: PTMibIfTable; pdwSize: PULONG;   bOrder: boolean ): DWORD; stdCall;

GetIfEntry: function ( pIfRow: PTMibIfRow ): DWORD; stdCall;

// warning - documentation is vague about where the result is provided
GetFriendlyIfIndex: function (var IfIndex: DWORD): DWORD; stdcall;

// 12 Jan 2009 replacement for GetAdaptersInfo, XP and later

GetAdaptersAddresses: function ( Family: LongWord; Flags: LongWord; Reserved: Pointer;
         AdapterAddresses: PIpAdapterAddresses; SizePointer: PULONG): DWORD stdcall ;

// 12 Jan 2009 - replacement for AllocateAndGetTcpExTableFromStack - XP SP2, W2K3 SP1, Vista and later

GetExtendedTcpTable: function ( pTCPTable: Pointer; pDWSize: PDWORD;
  bOrder: BOOL; ulAf: LongWord; TableClass: TTcpTableClass; Reserved: LongWord): DWORD; stdcall;

GetOwnerModuleFromTcpEntry: function( pTcpEntry: PTMibTcpRowOwnerModule;
  InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): LongInt stdcall ;

GetExtendedUdpTable: function ( pUdpTable: Pointer; pdwSize: PDWORD;
  bOrder: BOOL; ulAf: LongWord; TableClass: TUdpTableClass; Reserved: LongWord): LongInt stdcall ;

GetOwnerModuleFromUdpEntry: function ( pUdpEntry: PTMibUdpRowOwnerModule;
  InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): LongInt stdcall ;

// Nov 2014 - notify IP address and route changes, some Vista and later
type
TIpForwardChangeCallback = procedure (CallerContext: Pointer;  Row: PMibIpForwardRow2;
                                                NotificationType: TMibNoticationType); stdcall;
PIpForwardChangeCallback = ^TIpForwardChangeCallback;

TIpInterfaceChangeCallback = procedure (CallerContext: Pointer; Row: PMibIPInterfaceRow;
                                                NotificationType: TMibNoticationType); stdcall;
PIpInterfaceChangeCallback = ^TIpInterfaceChangeCallback;

TUnicastIpAddressChangeCallback = procedure (CallerContext: Pointer; Row: PMibUnicastIpAddressRow;
                                                NotificationType: TMibNoticationType); stdcall;
PUnicastIpAddressChangeCallback = ^TUnicastIpAddressChangeCallback;

TStableUnicastIpAddressTableCallback = procedure (CallerContext: Pointer;
                                                AddressTable: PMibUnicastIpAddressTable); stdcall;
PStableUnicastIpAddressTableCallback = ^TStableUnicastIpAddressTableCallback;

// Nov 2014 - get IP addresses and routes, some Vista and later
var
NotifyAddrChange: function (var Handle: THandle; overlapped: POVERLAPPED): DWORD; stdcall;

NotifyRouteChange: function (var Handle: THandle; overlapped: POVERLAPPED): DWORD; stdcall;

NotifyRouteChange2: function (Family: TAddressFamily; Callback: PIpForwardChangeCallback;
        CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;

CancelIPChangeNotify: function (notifyOverlapped: POVERLAPPED): BOOL; stdcall;

NotifyIpInterfaceChange: function (Family: TAddressFamily; Callback: PIpInterfaceChangeCallback;
                CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;

NotifyUnicastIpAddressChange: function (Family: TAddressFamily; Callback: PUnicastIpAddressChangeCallback;
                CallerContext: Pointer; InitialNotification: BOOLEAN; var NotificationHandle: THandle): DWORD; stdcall;

NotifyStableUnicastIpAddressTable: function (Family: TAddressFamily; var Table: PMibUnicastIpAddressTable;
    CallerCallback: TStableUnicastIpAddressTableCallback; CallerContext: Pointer; NotificationHandle: PHandle): DWORD; stdcall;

SetUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;

CancelMibChangeNotify2: function (NotificationHandle: THandle): DWORD; stdcall;

GetIpInterfaceEntry: function (var Row: PMibIPInterfaceRow): DWORD; stdcall;

GetIpInterfaceTable: function (Family: TAddressFamily; var Table: PMibIPInterfaceTable): DWORD; stdcall;

InitializeIpInterfaceEntry: procedure (var Row: PMibIPInterfaceRow); stdcall;

SetIpInterfaceEntry: function (Row: PMibIPInterfaceRow): DWORD; stdcall;

CreateUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;

DeleteUnicastIpAddressEntry: function (const Row: PMibUnicastIpAddressRow): DWORD; stdcall;

GetUnicastIpAddressEntry: function (var Row: PMibUnicastIpAddressRow): DWORD; stdcall;

GetUnicastIpAddressTable: function (Family: TAddressFamily; var Table: PMibUnicastIpAddressTable): DWORD; stdcall;

InitializeUnicastIpAddressEntry: function (var Row: PMibUnicastIpAddressRow): DWORD; stdcall;

CreateAnycastIpAddressEntry: function (const Row: PMibAnycastIpAddressRow): DWORD; stdcall;

DeleteAnycastIpAddressEntry: function (const Row: PMibAnycastIpAddressRow): DWORD; stdcall;

GetAnycastIpAddressEntry: function (var Row: PMibAnycastIpAddressRow): DWORD; stdcall;

GetAnycastIpAddressTable: function (Family: TAddressFamily; var Table: PMibAnycastIpAddressTable): DWORD; stdcall;

GetMulticastIpAddressEntry: function (var Row: PMibMulticastIpAddressRow): DWORD; stdcall;

GetMulticastIpAddressTable: function (Family: TAddressFamily; var Table: PMibMulticastIpAddressTable): DWORD; stdcall;

FreeMibTable: procedure (Memory: Pointer); stdcall;

ConvertInterfaceNameToLuidW: function (const InterfaceName: PWideChar; InterfaceLuid: PNetLuid): DWORD; stdcall;

ConvertInterfaceLuidToNameW: function (const InterfaceLuid: PNetLuid; InterfaceName: PWideChar; Length: DWORD): DWORD; stdcall;

ConvertInterfaceIndexToLuid: function (const InterfaceIndex: TNetIfIndex; InterfaceLuid: PNetLuid): DWORD; stdcall;


// 27 Oct 2014 - IPv6 versions of earlier APIs
GetIfEntry2: function (var pIfRow: PMibIfRow2): DWORD; stdCall;

GetIfTable2: function (var pIfTable: PTMibIfTable2): DWORD; stdCall;

GetIfTable2Ex: function (Level: TMibIfTableLevel; var pIfTable: PTMibIfTable2): DWORD; stdCall;

GetIpForwardTable2: function (Family: TAddressFamily; var pIPForwardTable: PMibIPForwardTable2): DWORD; stdCall;

GetTcp6Table: function (var pTCPTable: PTMibTCP6Table; pDWSize: PDWORD; bOrder: BOOL): DWORD; stdcall;

GetTcpStatisticsEx: function (dwFamily: DWORD; var pStats: PTMibTCPStats): DWORD; stdcall;

GetUdp6Table: function (var pUdpTable: PTMibUDP6Table; pDWSize: PDWORD; bOrder: BOOL ): DWORD; stdcall;

GetUdpStatisticsEx: function (dwFamily: DWORD; var pStats: PTMibUdpStats): DWORD; stdcall;

GetOwnerModuleFromTcp6Entry: function( pTcpEntry: PTMibTcp6RowOwnerModule;
  InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): DWORD stdcall ;

GetOwnerModuleFromUdp6Entry: function ( pUdpEntry: PTMibUdp6RowOwnerModule;
  InfoClass: TTcpIpOwnerModuleInfoClass; pBuffer: Pointer; pdwSize: PDWORD): DWORD stdcall ;



// Nov 2014-- Vista and later functions from NTDLL
// only ANSI versions since IP address don't need unicode
RtlIpv4AddressToStringA: function (const Addr: PInAddr; S: PAnsiChar): PAnsiChar; stdcall;
RtlIpv4AddressToStringExA: function (const Address: PInAddr; Port: Word;
                  AddressString: PAnsiChar; var AddressStringLength: DWORD): Longint; stdcall;
RtlIpv4StringToAddressA: function (const S: PAnsiChar; Strict: Boolean;
                              var Terminator: PAnsiChar; Address: PInAddr): DWORD; stdcall;
RtlIpv4StringToAddressExA: function (const AddressString: PAnsiChar; Strict: Boolean;
                                              Address: PInAddr; var Port: Word): DWORD; stdcall;
RtlIpv6AddressToStringA: function (const Addr: PIn6Addr; S: PAnsiChar): PAnsiChar; stdcall;
RtlIpv6AddressToStringExA: function (const Address: PIn6Addr; ScopeId: DWORD; Port: Word;
                           AddressString:  PAnsiChar; var AddressStringLength: DWORD): DWORD stdcall ;
RtlIpv6StringToAddressA: function (const S: PAnsiChar; var Terminator: PAnsiChar;
                                                              Addr: PIn6Addr): DWORD; stdcall;
RtlIpv6StringToAddressExA: function (const AddressString: PAnsiChar; var Address: PIn6Addr;
                                                  var ScopeId: DWORD; var Port: Word): DWORD; stdcall;
RtlEthernetAddressToStringA: function (const Addr: Pointer; S: PAnsiChar): PAnsiChar; stdcall;
RtlEthernetStringToAddressA: function (const S: PAnsiChar; var Terminator: PAnsiChar;
                                                                  var Addr: Pointer): DWORD; stdcall;

// load them

const
    IpHlpDLL = 'IPHLPAPI.DLL';
    Ntdll = 'NTDLL.DLL';
var
    IpHlpModule: THandle;
    NtdllModule: THandle;

    function LoadIpHlp: Boolean;

implementation

function LoadIpHlp: Boolean;
begin
    Result := True;
    if IpHlpModule <> 0 then Exit;

// open DLL
    IpHlpModule := LoadLibrary (IpHlpDLL);
    if IpHlpModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    GetAdaptersInfo := GetProcAddress (IpHlpModule, 'GetAdaptersInfo') ;
    GetNetworkParams := GetProcAddress (IpHlpModule, 'GetNetworkParams') ;
    GetTcpTable := GetProcAddress (IpHlpModule, 'GetTcpTable') ;
    GetTcpStatistics := GetProcAddress (IpHlpModule, 'GetTcpStatistics') ;
    GetUdpTable := GetProcAddress (IpHlpModule, 'GetUdpTable') ;
    GetUdpStatistics := GetProcAddress (IpHlpModule, 'GetUdpStatistics') ;
    GetIpStatistics := GetProcAddress (IpHlpModule, 'GetIpStatistics') ;
    GetIpNetTable := GetProcAddress (IpHlpModule, 'GetIpNetTable') ;
    GetIpAddrTable := GetProcAddress (IpHlpModule, 'GetIpAddrTable') ;
    GetIpForwardTable := GetProcAddress (IpHlpModule, 'GetIpForwardTable') ;
    GetIcmpStatistics := GetProcAddress (IpHlpModule, 'GetIcmpStatistics') ;
    GetRTTAndHopCount := GetProcAddress (IpHlpModule, 'GetRTTAndHopCount') ;
    GetIfTable := GetProcAddress (IpHlpModule, 'GetIfTable') ;
    GetIfEntry := GetProcAddress (IpHlpModule, 'GetIfEntry') ;
    GetFriendlyIfIndex := GetProcAddress (IpHlpModule, 'GetFriendlyIfIndex') ;
    GetPerAdapterInfo := GetProcAddress (IpHlpModule, 'GetPerAdapterInfo') ;
    GetAdaptersAddresses := GetProcAddress (IpHlpModule, 'GetAdaptersAddresses') ;
    GetExtendedTcpTable := GetProcAddress (IpHlpModule, 'GetExtendedTcpTable') ;
    GetOwnerModuleFromTcpEntry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromTcpEntry') ;
    GetExtendedUdpTable := GetProcAddress (IpHlpModule, 'GetExtendedUdpTable') ;
    GetOwnerModuleFromUdpEntry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromUdpEntry') ;

    NotifyAddrChange := GetProcAddress (IpHlpModule, 'NotifyAddrChange') ;
    NotifyRouteChange := GetProcAddress (IpHlpModule, 'NotifyRouteChange') ;
    NotifyRouteChange2 := GetProcAddress (IpHlpModule, 'NotifyRouteChange2') ;
    CancelIPChangeNotify := GetProcAddress (IpHlpModule, 'CancelIPChangeNotify') ;
    NotifyIpInterfaceChange := GetProcAddress (IpHlpModule, 'NotifyIpInterfaceChange') ;
    NotifyUnicastIpAddressChange := GetProcAddress (IpHlpModule, 'NotifyUnicastIpAddressChange') ;
    CancelMibChangeNotify2 := GetProcAddress (IpHlpModule, 'CancelMibChangeNotify2') ;
    GetIfTable2 := GetProcAddress (IpHlpModule, 'GetIfTable2') ;
    GetIfTable2Ex := GetProcAddress (IpHlpModule, 'GetIfTable2Ex') ;
    GetIfEntry2 := GetProcAddress (IpHlpModule, 'GetIfEntry2') ;
    GetIpForwardTable2 := GetProcAddress (IpHlpModule, 'GetIpForwardTable2') ;
    GetTcp6Table := GetProcAddress (IpHlpModule, 'GetTcp6Table') ;
    GetTcpStatisticsEx := GetProcAddress (IpHlpModule, 'GetTcpStatisticsEx') ;
    GetUdp6Table := GetProcAddress (IpHlpModule, 'GetUdp6Table') ;
    GetUdpStatisticsEx := GetProcAddress (IpHlpModule, 'GetUdpStatisticsEx') ;
    GetOwnerModuleFromTcp6Entry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromTcp6Entry') ;
    GetOwnerModuleFromUdp6Entry := GetProcAddress (IpHlpModule, 'GetOwnerModuleFromUdp6Entry') ;
    CreateUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'CreateUnicastIpAddressEntry') ;
    DeleteUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'DeleteUnicastIpAddressEntry') ;
    GetUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetUnicastIpAddressEntry') ;
    GetUnicastIpAddressTable := GetProcAddress (IpHlpModule, 'GetUnicastIpAddressTable') ;
    InitializeUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'InitializeUnicastIpAddressEntry') ;
    NotifyUnicastIpAddressChange := GetProcAddress (IpHlpModule, 'NotifyUnicastIpAddressChange') ;
    NotifyStableUnicastIpAddressTable := GetProcAddress (IpHlpModule, 'NotifyStableUnicastIpAddressTable') ;
    SetUnicastIpAddressEntry := GetProcAddress (IpHlpModule, 'SetUnicastIpAddressEntry') ;
    CreateAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'CreateAnycastIpAddressEntry') ;
    DeleteAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'DeleteAnycastIpAddressEntry') ;
    GetAnycastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetAnycastIpAddressEntry') ;
    GetAnycastIpAddressTable := GetProcAddress (IpHlpModule, 'GetAnycastIpAddressTable') ;
    GetMulticastIpAddressEntry := GetProcAddress (IpHlpModule, 'GetMulticastIpAddressEntry') ;
    GetMulticastIpAddressTable := GetProcAddress (IpHlpModule, 'GetMulticastIpAddressTable') ;
    FreeMibTable := GetProcAddress (IpHlpModule, 'FreeMibTable') ;
    ConvertInterfaceNameToLuidW := GetProcAddress (IpHlpModule, 'ConvertInterfaceNameToLuidW') ;
    ConvertInterfaceLuidToNameW := GetProcAddress (IpHlpModule, 'ConvertInterfaceLuidToNameW') ;
    ConvertInterfaceIndexToLuid := GetProcAddress (IpHlpModule, 'ConvertInterfaceIndexToLuid') ;

    NtdllModule := LoadLibrary (Ntdll);
    if NtdllModule <> 0 then
    begin
    RtlIpv4AddressToStringA := GetProcAddress (NtdllModule,  'RtlIpv4AddressToStringA') ;
    RtlIpv4AddressToStringExA := GetProcAddress (NtdllModule, 'RtlIpv4AddressToStringExA') ;
    RtlIpv4StringToAddressA := GetProcAddress (NtdllModule, 'RtlIpv4StringToAddressA') ;
    RtlIpv4StringToAddressExA := GetProcAddress (NtdllModule, 'RtlIpv4StringToAddressExA') ;
    RtlIpv6AddressToStringA := GetProcAddress (NtdllModule, 'RtlIpv6AddressToStringA') ;
    RtlIpv6AddressToStringExA := GetProcAddress (NtdllModule, 'RtlIpv6AddressToStringExA') ;
    RtlIpv6StringToAddressA := GetProcAddress (NtdllModule, 'RtlIpv6StringToAddressA') ;
    RtlIpv6StringToAddressExA := GetProcAddress (NtdllModule, 'RtlIpv6StringToAddressExA') ;
    RtlEthernetAddressToStringA := GetProcAddress (NtdllModule, 'RtlEthernetAddressToStringA') ;
    RtlEthernetStringToAddressA := GetProcAddress (NtdllModule, 'RtlEthernetStringToAddressA') ;

    end;

end;

initialization
    IpHlpModule := 0 ;
    NtdllModule := 0 ;
finalization
    if IpHlpModule <> 0 then
    begin
        FreeLibrary (IpHlpModule) ;
        IpHlpModule := 0 ;
    end ;
    if NtdllModule <> 0 then
    begin
        FreeLibrary (NtdllModule) ;
        NtdllModule := 0 ;
    end ;

end.
