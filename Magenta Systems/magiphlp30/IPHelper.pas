unit IPHelper;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// must turn off range checking or various records declared array [0..0] die !!!!!
{$R-}
{$Q-}

// Magenta Systems Internet Protocol Helper Component
// 26th November 2018 - Release 3.0 (C) Magenta Systems Ltd, 2018
// based on work by by Dirk Claessens

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/


(*
  ==========================
  Delphi IPHelper functions
  ==========================
  Requires : NT4/SP4 or higher, WIN98/WIN98se
  Originally Developed on: D4.03
  Originally Tested on   :  WIN-NT4/SP6, WIN98se, WIN95/OSR1

  Warning - currently only supports Delphi 5 and later unless int64 is removed
  (Int64 is only used to force Format to show unsigned 32-bit numbers)

  ================================================================
                    This software is FREEWARE
                    -------------------------
  If this software works, it was surely written by Dirk Claessens
  http://users.pandora.be/dirk.claessens2/
  (If it doesn't, I don't know anything about it.)
  ================================================================

  Version: 1.2 2000-12-03
{ List of Fixes & Additions

v1.1
-----
Fix :  wrong errorcode reported in GetNetworkParams()
Fix :  RTTI MaxHops 20 > 128
Add :  ICMP -statistics
Add :  Well-Known port numbers
Add :  RecentIP list
Add :  Timer update

v1.2
----
Fix :  Recent IP's correct update
ADD :  ICMP-error codes translated

v1.3 - 18th September 2001
----
  Angus Robertson, Magenta Systems Ltd, England
     delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  Slowly converting procs into functions that can be used by other programs,
     ie Get_ becomes IpHlp
  Primary improvements are that current DNS server is now shown, also
     in/out bytes for each interface (aka adaptor)
  All functions are dynamically loaded so program can be used on W95/NT4
  Tested with Delphi 6 on Windows 2000 and XP

v1.4 - 28th February 2002 - Angus
----
  Fixed major memory leak in IpHlpIfTable (except instead of finally)
  Fixed major memory leak in Get_AdaptersInfo (incremented buffer pointer)
  Created IpHlpAdaptersInfo which returns TAdaptorRows

v 1.5 - 26 July 2002 - Angus
-----
  Using GetPerAdapterInfo to get DNS adapter info for each adapter

v 1.6 - 19 August 2002 - Angus
-----
  Added IpHlpTCPTable and IpHlpUDPTable which returns TConnRows
  On XP, use undocumented APIs for improved connections list adding process and EXE

v1.7 - 14th October 2003 - Angus
  Force range checking off to avoid errors in array [0..0], should really use pointers
  Validate dwForwardProto to check for bad values

v1.8 - 25th October 2005 - Angus
  Added extra elements to TConnInfo for end user application

v1.9 - 8th August 2006 - Angus
  Interfaces now show type description, adaptor correct type description


v2.0 - 25th February 2007 - Angus
   Many more IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

  Note: IpHlpNetworkParams returns dynamic DNS address (and other stuff)
  Note: IpHlpIfEntry returns bytes in/out for a network adaptor

v2.1 - 5th August 2008 - Angus
    Updated to be compatible with Delphi 2009

v2.2 - 16th January 2009 - Angus
    Added GetAdaptersAddresses (XP and later) has IPv6 addresses (but not yet getting them)
      Note: gateway IPs don't seem to be returned by GetAdaptersAddresses
    Added GetExtendedTcpTable and GetExtendedUdpTable (XP SP2, W2K3 SP1, Vista and later),
      replacements for AllocateAndGetTcpExTableFromStack/etc, added connection start time
    Using WideString for program paths and adaptor descriptions for Unicode compatibility
    Added two public variables:
     ShowExePath if true displays full program path for connection tables
     UseAdressesAPI if true uses GetAdaptersAddresses instead of GetAdaptersInfo

v2.3 - 3rd August 2009
    Changed ULONGLONG to LONGLONG for Delphi 7 compatability

v2.4 - 8th August 2010
    Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
    Tested with 32-bit and 64-bit in Delphi XE2

v3.0 - 26th November 2018
   Only supporting XP SP3 and later, so remove code for earlier OSs
   Added IPv6 support, numerous new structures and functions, Vista and later
   Still runs on XP SP3, but TCP and UDP connection lists not supported and
     some other functions return limited info, IP addresses in particular
   Added notification functions for interface changes, Vista and later
   UseAdressesAPI removed
   corrected MacAddr2Str so it does not skip first byte

Pending - IPv6 not yet supported for ARP or IP Routing table, sorry

*)

interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs, IpHlpApi, Psapi, Winsock,
  TypInfo ;

const
  NULL_IP       = '  0.  0.  0.  0';

//------conversion of well-known port numbers to service names----------------

type
  TWellKnownPort = record
    Prt: DWORD;
    Srv: string;
  end;


const
    // only most "popular" services...
  WellKnownPorts: array[1..28] of TWellKnownPort
  = ( ( Prt: 7; Srv: 'ECHO' ), {ping}
    ( Prt: 9; Srv: 'DISCRD' ), { Discard}
    ( Prt: 13; Srv: 'DAYTIM' ), {DayTime}
    ( Prt: 17; Srv: 'QOTD' ), {Quote Of The Day}
    ( Prt: 19; Srv: 'CHARGEN' ), {CharGen}
    ( Prt: 20; Srv: 'FTP ' ),
    ( Prt: 21; Srv: 'FTPC' ), { File Transfer Control Protocol}
    ( Prt: 23; Srv: 'TELNET' ), {TelNet}
    ( Prt: 25; Srv: 'SMTP' ), { Simple Mail Transfer Protocol}
    ( Prt: 37; Srv: 'TIME' ),
    ( Prt: 53; Srv: 'DNS ' ),
    ( Prt: 67; Srv: 'BOOTPS' ), { BOOTP Server }
    ( Prt: 68; Srv: 'BOOTPC' ), { BOOTP Client }
    ( Prt: 69; Srv: 'TFTP' ), { Trivial FTP  }
    ( Prt: 70; Srv: 'GOPHER' ), { Gopher       }
    ( Prt: 79; Srv: 'FING' ), { Finger       }
    ( Prt: 80; Srv: 'HTTP' ), { HTTP         }
    ( Prt: 88; Srv: 'KERB' ), { Kerberos     }
    ( Prt: 109; Srv: 'POP2' ), { Post Office Protocol Version 2 }
    ( Prt: 110; Srv: 'POP3' ), { Post Office Protocol Version 3 }
    ( Prt: 119; Srv: 'NNTP' ), { Network News Transfer Protocol }
    ( Prt: 123; Srv: 'NTP ' ), { Network Time protocol          }
    ( Prt: 135; Srv: 'LOCSVC'), { Location Service              }
    ( Prt: 137; Srv: 'NBNAME' ), { NETBIOS Name service          }
    ( Prt: 138; Srv: 'NBDGRAM' ), { NETBIOS Datagram Service     }
    ( Prt: 139; Srv: 'NBSESS' ), { NETBIOS Session Service        }
    ( Prt: 161; Srv: 'SNMP' ), { Simple Netw. Management Protocol }
    ( Prt: 443; Srv: 'HTTPS' ) { HTTPS         }
    );


//-----------conversion of ICMP error codes to strings--------------------------
             {taken from www.sockets.com/ms_icmp.c }

const
  ICMP_ERROR_BASE = 11000;
  IcmpErr : array[1..22] of string =
  (
   'IP_BUFFER_TOO_SMALL','IP_DEST_NET_UNREACHABLE', 'IP_DEST_HOST_UNREACHABLE',
   'IP_PROTOCOL_UNREACHABLE', 'IP_DEST_PORT_UNREACHABLE', 'IP_NO_RESOURCES',
   'IP_BAD_OPTION','IP_HARDWARE_ERROR', 'IP_PACKET_TOO_BIG', 'IP_REQUEST_TIMED_OUT',
   'IP_BAD_REQUEST','IP_BAD_ROUTE', 'IP_TTL_EXPIRED_TRANSIT',
   'IP_TTL_EXPIRED_REASSEM','IP_PARAMETER_PROBLEM', 'IP_SOURCE_QUENCH',
   'IP_OPTION_TOO_BIG', 'IP_BAD_DESTINATION','IP_ADDRESS_DELETED',
   'IP_SPEC_MTU_CHANGE', 'IP_MTU_CHANGE', 'IP_UNLOAD'
  );


//----------conversion of diverse enumerated values to strings------------------

  AdaptTypes    : array[1..MAX_IF_TYPE] of string = (    // 9 February 2007
    'Other', 'Reg_1822', 'HDH_1822', 'DDN_X25', 'RFC877X25', 'Ethernet', 'ISO88023',
    'ISO88024', 'Token Ring', 'ISO88026', 'StarLan', 'Proteon10', 'Proteon80', 'HyperChnl',
    'FDDI', 'LAP_B','SDLC', 'DS1', 'E1', 'Basic ISDN', 'Primary ISDN', 'Prop_P2P', 'PPP', 'Loopback',
    'EON','Eth_3MB', 'NSIP', 'SLIP','Ultra', 'DS3', 'SIP', 'FrameRly', 'RS232', 'Para', 'Arcnet',
    'Arcnet+', 'ATM', 'MIO_X25', 'Sonet', 'X25_PLE', 'ISO88022', 'LocalTalk', 'SMDS_DXI',
    'FrmRlySrv', 'V35', 'HSSI', 'HIPPI', 'Modem', 'AAL5', 'SonetPath', 'Sonet_VT', 'SMDS_ICIP',
    'Prop_Virt', 'Prop_Mux', 'IEEE80212','FibreChnl', 'HIPPIifce', 'FrmRlyIcn', 'ALanE8023',
    'ALanE8025', 'CCT_Emul', 'FastEther', 'ISDN', 'V11', 'V36', 'G703_64K', 'G703_2MB',
    'QLLC', 'FastEthFX', 'Channel', '802.11 Wireless', 'IBM370', 'Escon', 'DSLW', 'ISDN_S', 'ISDN_U',
    'LAP_D', 'IPSwitch', 'RSRB', 'ATM_Logic', 'DSO', 'DOSBundle', 'BSC', 'Async', 'CNR',
    'ISO88025',  'EPLRS', 'ARAP', 'Prop_CNLS', 'HostPad', 'TermPad', 'FrmRlyMPI', 'X213',
    'ADSL', 'RADSL', 'SDSL', 'VDSL', 'ISO88025', 'Myrinet', 'Voice_EM', 'Voice_FX0',
    'Voice_FXS', 'Voice_Cap','VOIP', 'ATM_DXI', 'ATM_FUNI', 'ATM_IMA', 'PPPMulti', 'IpOvCDLC',
    'IpOvCLAW', 'Stck2Stck',  'VirtIPAdr', 'MPC', 'IpOv_ATM', '88025Fibr', 'TDLC', 'GigaBit',
    'HDLC', 'LAP_F', 'V37', 'X25_MLP', 'X25_Hunt', 'TransHDLC', 'InterLeav', 'Fast', 'IP',
    'CATV_MACL', 'CATV_DwnS', 'CATV_UpSt', 'A12MPP_Sw', 'Tunnel', 'Coffee', 'CES', 'ATM_SubIF',
    'L2_VLAN', 'L3_IPVLAN', 'L3_IPXVLN', 'PowerLine',  'MedaiMail', 'DTM', 'DCN', 'IPForward',
    'MSDSL', '1394 Firewire', 'GSN',
     // following added Oct 2014
    'DVBRCC_MacLayer', 'DVBRCC_Downstream', 'DVBRCC_Upstream', 'ATM_Virtual', 'MPLS_Tunnel',
    'SRP', 'VoiceOverATM', 'VoiceOverFrameRelay', 'IDSL', 'CompositeLink', 'SS7_Siglink',
    'Prop_Wireless_P2P', 'FR_Forward', 'RFC1483', 'USB', 'IEEE8023AD_LAG', 'BGP_Policy_Accounting',
    'FRF16_MFR_Bundle', 'H323_Gatekeeper', 'H323_Proxy',  'MPLS', 'MF_Siglink', 'HDSL2',
    'SHDSL', 'DS1_FDL',  'POS', 'DVB_ASI_In', 'DVB_ASI_Out', 'PLC',  'NFAS', 'TR008',
    'GR303_RDT', 'GR303_IDT', 'ISUP', 'Prop_Docs_Wireless_MacLayer', 'Prop_Docs_Wireless_Downstream',
    'Prop_Docs_Wireless_Upstream', 'HiperLan2', 'Prop_BWA_P2MP','Sonet_Overhead_Channel',
    'Digital_Wrapper_Overhead_Channel', 'AAL2', 'Radio_Mac', 'ATM_Radio', 'IMT', 'MVL',
    'Reach_DSL', 'FR_DLCI_Endpt', 'ATM_VCI_Endpt', 'Optical_Channel', 'Optical_Transport',  // 196
    '','','',  // 197-199
    '','','','','','','','','','', // 200-209
    '','','','','','','','','','', // 210-219
    '','','','','','','','','','', // 220-229
    '','','','','','','', // 230-236
    '802.16 WiMax',     // 237
    '','','','','', // 238-242
    'WWAN GSM',   // 243 WWAN devices based on GSM technology
    'WWAN CDMA'   // 244 WWAN devices based on CDMA technology
    );

  ARPEntryType  : array[0..4] of string = ('', 'Other', 'Invalid',
    'Dynamic', 'Static'
    );
  TCPConnState  :
    array[0..12] of string =
    ('',  'closed', 'listening', 'syn_sent',
    'syn_rcvd', 'established', 'fin_wait1',
    'fin_wait2', 'close_wait', 'closing',
    'last_ack', 'time_wait', 'delete_tcb'
    );

  TCPToAlgo     : array[0..4] of string =
    ('',  'Const.Timeout', 'MIL-STD-1778',
    'Van Jacobson', 'Other' );

  IPForwTypes   : array[0..4] of string =
    ('',  'other', 'invalid', 'local', 'remote' );

  IPForwProtos  : array[0..18] of string =
    ('',  'OTHER', 'LOCAL', 'NETMGMT', 'ICMP', 'EGP',
    'GGP', 'HELO', 'RIP', 'IS_IS', 'ES_IS',
    'CISCO', 'BBN', 'OSPF', 'BGP', 'BOOTP',
    'AUTO_STAT', 'STATIC', 'NOT_DOD' );

  MibIpAddrPrimary = 'Primary';
  MibIpAddrDynamic = 'Dynamic';
  MibIpAddrDisconnected = 'Disconnected';
  MibIpAddrDeleted = 'Being Deleted';
  MibIpAddrTransient = 'Transient';
  MibIpAddrDnsEligible = 'Published in DNS';

 // TInterfaceAndOperStatusFlags literals
  MibIfoHardwareInterface = 'Hardware';
  MibIfoFilterInterface = 'Filter';
  MibIfoConnectorPresent = 'Connector Present';
  MibIfoNotAuthenticated = 'Not Authenticated';
  MibIfoNotMediaConnected = 'Not Media Connected';
  MibIfoPaused = 'Paused';
  MibIfoLowPower = 'Low Power';
  MibIfoEndPointInterface = 'End Point';

  IpPrefixOrigins : array[0..4] of string =
      ('Other','Manual','Well Known','Dhcp','Router Advert');

  IpSuffixOrigins : array[0..5] of string =
    ('Other','Manual','Well Known','Dhcp','Link Layer','Random');

  DadStates : array[0..4] of string =
    ('Invalid','Tentative','Duplicate','Deprecated','Preferred');

  IfOperStatuses : array[0..7] of string =
    ('None', 'Up', 'Down', 'Testing', 'Unknown', 'Dormant', 'Not Present', 'Lower Layer Down');

  AdminStatuses : array[0..3] of string =
    ('None', 'Up', 'Down', 'Testing') ;

  NdisMediums : array[0..19] of string =
    ('Ethernet 802.3','Token Ring 802.5','FDDI','WAN','LocalTalk','DIX','Arcnet','Arcnet 878.2','ATM','Wireless WAN',
    'IrDA','Broadcast PC','CoWan','Firewire 1394','InfiniBand','Tunnel','Native 802.11','Loopback','WiMax','IP' );

  NdisPhysicalMediums : array[0..20] of string =
    ('Unspecified','Wireless LAN','Cable Modem','Phone Line','Power Line','xDSL','Fibre Channel',
    '1394 bus','Wireless WAN','Native 802.11','Bluetooth','Infiniband','WiMax','UWB','Ethernet 802.3',
    'Toekn Ring 802.5','IrDA','Wired WAN','Wired CoWan','Other','');

  TunnelTypes : array[0..14] of string =
    ('None','Other', 'Direct', 'u3', 'u4', 'u5', 'u6', 'u7', 'u8', 'u9', 'u10',
    '6to4', 'u12', 'ISATAP', 'Teredo') ;

  NetIfAccessTtypes : array[0..5] of string =
    ('Unknown','Loopback','Broadcast','Point to Point','Point to Multi Point','');

  NetIfDirectionTypes : array[0..3] of string =
    ('Send and Receive','Send Only','Receive Only',''  );

  NetIfConnectionTypes : array[0..4] of string =
    ('Unknown','Dedicated','Passive','Demand','');

  NetIfMediaConnectStates : array[0..2] of string =
    ('Unknown','Connected','Disconnected'  );

type

// for IpHlpNetworkParams
  TNetworkParams = record
    HostName: string ;
    DomainName: string ;
    CurrentDnsServer: string ;
    DnsServerTot: integer ;
    DnsServerNames: array [0..9] of string ;
    NodeType: UINT;
    ScopeID: string ;
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDNS: UINT;
  end;

// for IpHlpIfTable and IpHlpIfTable2
  TIfRows = array of TMibIfRow ; // dynamic array of rows

  TIfRow2 = record      // Nov 2014
    Mib: TMibIfRow2;
    InterfaceName: WideString ;
    Description: WideString ;
    FriendlyName: WideString ;
  end;
  TIfRows2 = array of TIfRow2 ; // dynamic array of rows

// for IpHlpAdaptersInfo
  TAdaptorInfo = record
    AdapterName: WideString ;  // 14 Jan 2009, was string
    Description: WideString ;  // 14 Jan 2009, was string
    MacAddress: string ;
    Index: DWORD;
    aType: UINT;
    DHCPEnabled: UINT;
    CurrIPAddress: string ;
    CurrIPMask: string ;
    IPAddressTot: integer ;
    IPAddressList: array of string ;
    IPMaskList: array of string ;
    GatewayTot: integer ;
    GatewayList: array of string ;
    DHCPTot: integer ;
    DHCPServer: array of string ;
    HaveWINS: BOOL;
    PrimWINSTot: integer ;
    PrimWINSServer: array of string ;
    SecWINSTot: integer ;
    SecWINSServer: array of string ;
    LeaseObtained: LongInt ; // UNIX time, seconds since 1970
    LeaseExpires: LongInt;   // UNIX time, seconds since 1970
    AutoConfigEnabled: UINT ;  // next 4 from IP_Per_Adaptor_Info, W2K and later
    AutoConfigActive: UINT ;
    CurrentDNSServer: string ;
    DNSServerTot: integer ;
    DNSServerList: array of string ;
// following from GetAdaptersAddresses for Vista and later, a few for XP
    AnycastIPAddrList: array of string ;
    AnycastIPAddrTot: integer ;
    MulticastIPAddrList: array of string ;
    MulticastIPAddrTot: integer ;
    PrefixIPAddrList: array of string ;
    PrefixTot: Integer ; //  Nov 2014
    PrefixMaskList: array of string ;  //  Nov 2014
    FriendlyName: WideString ;
    Mtu: DWORD;
    IfType: DWORD;
    OperStatus: TIfOperStatus;
    Ipv6Index: DWORD;
    XmitLinkSpeed: Int64;
    RecvLinkSpeed: Int64;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: TIFLuid;
    CompartmentId: TNetIfCompartmentId;
    NetworkGuid: TNetIfNetworkGuid;
    ConnectionType: TNetIfConnectionType;
    TunnelType: TTunnelType;
    InterfaceName: WideString ;   // Nov 2014
    DnsSuffix: string;  // Nov 2014 was missing
    Flags: DWORD;  // Mov 2014 -  IP_ADAPTER_xxx flags
  end ;

  TAdaptorRows = array of TAdaptorInfo ;

// for IpHlpTCPStatistics and IpHlpUDPStatistics
  TConnInfo = record
    State: Integer ;
    LocalAddr: String ;
    LocalPort: Integer ;
    RemoteAddr: String ;
    RemotePort: Integer ;
    ProcessID: DWORD ;
    LocalHost: string ;   // 13 Oct 2004 - not used in this component, but for DNS lookups and display
    RemoteHost: string ;
    DispRow: integer ;
    ProcName: WideString ;    // 15 Jan 2009 - Unicode
    CreateDT: TDateTime ;     // 15 Jan 2009
    LocSockAddr: TSockAddrInet;   // Nov 2014
    RemSockAddr: TSockAddrInet;   // Nov 2014
  end;

  TConnRows = array of TConnInfo ;

// IP address record, IPv4 and IPv6, binary and string versions - Nov 2014
   TIpType = (IpTypeUnicast, IpTypeAnycast, IpTypeMulticast);

  TIpAddrInfo = record
    IpAddress: string ;
    IpMask: string ;
    IpType: TIpType ;
    TypeStr: string ;
    SockAddr: TSockAddrInet ;
    IFLuid: TNetLuid ;
    IFIndex: TNetIfIndex ;
    InterfaceName: WideString ;
    Description: WideString ;
    FriendlyName: WideString ;
    PrefixOrig: TIpPrefixOrigin ;
    SuffixOrig: TIpSuffixOrigin ;
    ValidSecs: Integer ;
    DupliState: TIpDadState ;
    IpScopeId: TScopeID ;
    CreationDT: TDateTime ;
  end;

  TIpAddrInfos = array of TIpAddrInfo ;

  TIpChangesEvent = Procedure (IpAddrInfo: TIpAddrInfo; CallerContext: Pointer;
                                                NotificationType: TMibNoticationType) of object ;

//---------------exported stuff-----------------------------------------------

function IpHlpAdaptersInfo(var AdpTot: integer;var AdpRows: TAdaptorRows): integer ;
procedure Get_AdaptersInfo( List: TStrings );
function IpHlpNetworkParams (var NetworkParams: TNetworkParams): integer ;
procedure Get_NetworkParams( List: TStrings );
procedure Get_ARPTable( List: TStrings );
function IpHlpTCPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET): integer ;
procedure Get_TCPTable( List: TStrings );
function IpHlpTCPStatistics (var TCPStats: TMibTCPStats): integer ;
procedure Get_TCPStatistics( List: TStrings );
function IpHlpUDPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET): integer ;
procedure Get_UDPTable( List: TStrings );
function IpHlpUdpStatistics (UdpStats: TMibUDPStats): integer ;
procedure Get_UDPStatistics( List: TStrings );
procedure Get_IPAddrTable( List: TStrings );
procedure Get_IPForwardTable( List: TStrings );
function IpHlpIPStatistics (var IPStats: TMibIPStats): integer ;
procedure Get_IPStatistics( List: TStrings );
function Get_RTTAndHopCount( IPAddr: DWORD; MaxHops: Longint;
  var RTT: longint; var HopCount: longint ): integer;
procedure Get_ICMPStats( ICMPIn, ICMPOut: TStrings );
function IpHlpIfTable(var IfTot: integer; var IfRows: TIfRows): integer ;
function IpHlpIfTable2(var IfTot: integer; var IfRows2: TIfRows2): integer ;
procedure Get_IfTable( List: TStrings );
procedure Get_IfTable2( List: TStrings );
function IpHlpIfEntry(Index: integer; var IfRow: TMibIfRow): integer ;
procedure Get_RecentDestIPs( List: TStrings );
function IpChangesStart (Family: TAddressFamily; CallerContext: Pointer): Integer ;
function IpChangesStop: Integer ;
function IpHlpAdaptersAddr(Family: TAddressFamily; var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
function GetIpAddrType (wtype: DWORD): string;
function GetIfoFlags (Flags: TInterfaceAndOperStatusFlags): string ;
function IpHlpIpAddrTable(var IpAddrInfos: TIpAddrInfos; Family: TAddressFamily = AF_INET;
           AllIps: Boolean = True; Names: Boolean = True; AdptIdx: TNetIfIndex = 0): integer ;

// conversion utils
function MacAddr2Str( MacAddr: array of byte; size: integer ): string;
function IpAddr2Str( IPAddr: DWORD ): string;
function Str2IpAddr( IPStr: string ): DWORD;
function Port2Str( nwoPort: DWORD ): string;
function Port2Wrd( nwoPort: DWORD ): DWORD;
function Port2Svc( Port: DWORD ): string;
function ICMPErr2Str( ICMPErrCode: DWORD) : string;
function Ip6Addr2Str (const Value: TInAddr6; Scope: DWORD = 0; Port: Word = 0): string;
function Ip6Addr2Str2 (const Value: TInAddr6; Scope: DWORD = 0; Port: Word = 0): string;
function SocketAddr2Str (MyAddress: TSockAddrInet): string ; overload ;  // Nov 2014
function SocketAddr2Str (MyAddress: TSocketAddress): string ; overload ; // Nov 2014

var
    ShowExePath: boolean = false ;
    NotificationHandle: THandle;  // for NotifyIpInterfaceChange
    fIpChangesEvent: TIpChangesEvent = nil ;  // Nov 2014 set to event function


implementation

var
  RecentIPs     : TStringList;

//--------------General utilities-----------------------------------------------


{ extracts next "token" from string, then eats string }
function NextToken( var s: string; Separator: char ): string;
var
  Sep_Pos       : byte;
begin
  Result := '';
  if length( s ) > 0 then begin
    Sep_Pos := pos( Separator, s );
    if Sep_Pos > 0 then begin
      Result := copy( s, 1, Pred( Sep_Pos ) );
      Delete( s, 1, Sep_Pos );
    end
    else begin
      Result := s;
      s := '';
    end;
  end;
end;

//------------------------------------------------------------------------------
{ concerts numerical MAC-address to ww-xx-yy-zz string }
function MacAddr2Str( MacAddr: array of byte; size: integer ): string;
var
    i: integer;
    blank: boolean;
begin
    if (Size = 0) or (Length (MacAddr) < size) then
    begin
        Result := 'Blank';
        exit;
    end
    else
        Result := '';
    blank := true ;
    for i := 0 to Size - 1 do   // Feb 2016 corrected to base 0
    begin
        if MacAddr[i] <> 0 then blank := false ;
        Result := Result + IntToHex( MacAddr[i], 2 );
        if i < size - 1 then Result := Result + '-';
    end;
    if blank then Result := 'Blank';
end;

//------------------------------------------------------------------------------
{ converts IPv4-address in network byte order DWORD to dotted decimal string}
// Nov 2014 - strip spaces from within IP address
function IpAddr2Str( IPAddr: DWORD ): string;
var
  i             : integer;
begin
  Result := '';
  for i := 1 to 4 do
  begin
 //   Result := Result + Format( '%3d.', [IPAddr and $FF] );
    Result := Result + IntToStr(IPAddr and $FF);
    if i <> 4 then Result := Result + '.';
    IPAddr := IPAddr shr 8;
  end;
//  Delete( Result, Length( Result ), 1 );
end;

//------------------------------------------------------------------------------
{ converts IPv6-address to hex string, without removing blank hex pairs}
{ port is in network port order - ie wrong way around }
function Ip6Addr2Str2 (const Value: TInAddr6; Scope: DWORD = 0; Port: Word = 0): string;      // Nov 2014
begin
    Result := Lowercase (Format('%x:%x:%x:%x:%x:%x:%x:%x',
     [MakeWord(Value.S6_addr[1], Value.S6_addr[0]), MakeWord(Value.S6_addr[3], Value.S6_addr[2]),
      MakeWord(Value.S6_addr[5], Value.S6_addr[4]), MakeWord(Value.S6_addr[7], Value.S6_addr[6]),
       MakeWord(Value.S6_addr[9], Value.S6_addr[8]), MakeWord(Value.S6_addr[11], Value.S6_addr[10]),
         MakeWord(Value.S6_addr[13], Value.S6_addr[12]), MakeWord(Value.S6_addr[15], Value.S6_addr[14])])) ;
    if Scope <> 0 then
        Result := Result + '%' + IntToStr (Scope);
    if Port <> 0 then
        Result := '[' + Result + ']:' + IntToStr (Port2Wrd (Port))
    else
        Result := '[' + Result + ']';
end;

//------------------------------------------------------------------------------
{ converts IPv6-address to hex string, using Windows API if available }
{ port is in network port order - ie wrong way around }
function Ip6Addr2Str (const Value: TInAddr6; Scope: DWORD = 0; Port: Word = 0): string;      // Nov 2014
var
    Buffer: array[0..45] of AnsiChar ;
    ret, len: DWORD;
begin
    result := '' ;
    if NOT LoadIpHlp then exit ;
    if NOT Assigned (RtlIpv6AddressToStringExA) then
        Result := Ip6Addr2Str2 (Value, Scope, Port)
    else
    begin
        len := 45;
        ret := RtlIpv6AddressToStringExA (@Value, Scope, Port, Buffer, len);
        if ret = 0 then
        begin
            result := String (Buffer);
            if Pos ('[', result) <> 1 then result := '[' + result + ']' ;
        end;
    end;
end ;

//------------------------------------------------------------------------------
{ converts a TSocketAddress structure with an IPv4 or IPv6 address and port to a string }
function SocketAddr2Str (MyAddress: TSockAddrInet): string ;  // Nov 2014
begin
    result := '' ;
    if MyAddress.si_family = AF_INET then  // IPv4
       result := IpAddr2Str (DWORD (MyAddress.Ipv4.sin_addr))
    else if MyAddress.si_family = AF_INET6 then  // IPv6
       result := Ip6Addr2Str (MyAddress.Ipv6.sin6_addr,
                                    MyAddress.Ipv6.sin6_scope_id) ;
  end;

function SocketAddr2Str (MyAddress: TSocketAddress): string ;  // Nov 2014
begin
    result := SocketAddr2Str (MyAddress.lpSockaddr^);
end;

//------------------------------------------------------------------------------
// swap any number of bytes, integer, double, extended, anything
// ByteSwaps (@value, sizeof (value)) ;

procedure ByteSwaps(DataPtr : Pointer;NoBytes : integer);
var
    i : integer;
    dp : PAnsiChar;
    tmp : AnsiChar;
begin
  // Perform a sanity check to make sure that the function was called properly
    if (NoBytes > 1) then
    begin
        Dec(NoBytes);
        dp := PAnsiChar(DataPtr);
    // we are now safe to perform the byte swapping
        for i := NoBytes downto (NoBytes div 2 + 1) do
        begin
            tmp := PAnsiChar(Integer(dp)+i)^;
            PAnsiChar(Integer(dp)+i)^ := PAnsiChar(Integer(dp)+NoBytes-i)^;
            PAnsiChar(Integer(dp)+NoBytes-i)^ := tmp;
        end;
    end;
end;

//------------------------------------------------------------------------------
// create IPv4 subnet mask from prefix length, 30=255.255.255.0, etc
function CreateMask (len: Integer): string ;
var
    I: Integer ;
    mask: DWORD;
begin
    result := '' ;
    if (len < 8) or (len >= 31) then exit ;
    mask := $FFFFFFFF; ;
    for I := 31 downto len do
        mask := mask div 2 ;
//  ByteSwaps (@mask, 4) ; // convert to network order
    Result := IPAddr2Str (mask) ;
end;

//------------------------------------------------------------------------------
function IpHlpConvIntLuidToStr (const InterfaceLuid: TNetLuid): WideString ;
var
    Buffer: array[0..MAX_ADAPTER_NAME_LENGTH] of WideChar ;
begin
    result := '' ;
    if NOT Assigned (ConvertInterfaceLuidToNameW) then Exit;
    if ConvertInterfaceLuidToNameW (@InterfaceLuid, Buffer, MAX_ADAPTER_NAME_LENGTH) <> 0 then exit ;
    Result := String (Buffer) ;
end ;

//------------------------------------------------------------------------------
function IpHlpConvIntIdxToStr (const InterfaceIndex: TNetIfIndex): WideString ;
var
    InterfaceLuid: TNetLuid;
begin
    result := '' ;
    if NOT Assigned (ConvertInterfaceIndexToLuid) then Exit;
    if ConvertInterfaceIndexToLuid (InterfaceIndex, @InterfaceLuid) <> 0 then exit ;
    Result := IpHlpConvIntLuidToStr (InterfaceLuid) ;
end ;

//------------------------------------------------------------------------------
{ converts dotted decimal IP-address to network byte order DWORD}
function Str2IpAddr( IPStr: string ): DWORD;
var
  i             : integer;
  Num           : DWORD;
begin
  Result := 0;
  for i := 1 to 4 do
  try
    Num := ( StrToInt( NextToken( IPStr, '.' ) ) ) shl 24;
    Result := ( Result shr 8 ) or Num;
  except
    Result := 0;
  end;

end;

//------------------------------------------------------------------------------
{ converts port number in network byte order to DWORD }
function Port2Wrd( nwoPort: DWORD ): DWORD;
begin
  Result := Swap( WORD( nwoPort ) );
end;

//------------------------------------------------------------------------------
{ converts port number in network byte order to string }
function Port2Str( nwoPort: DWORD ): string;
begin
  Result := IntToStr( Port2Wrd( nwoPort ) );
end;

//------------------------------------------------------------------------------
{ converts well-known port numbers to service ID }
function Port2Svc( Port: DWORD ): string;
var
  i             : integer;
begin
  Result := Format( '%4d', [Port] ); // in case port not found
  for i := Low( WellKnownPorts ) to High( WellKnownPorts ) do
    if Port = WellKnownPorts[i].Prt then
    begin
      Result := WellKnownPorts[i].Srv;
      BREAK;
    end;
end;

//------------------------------------------------------------------------------
function FileTimeToInt64 (const FileTime: TFileTime): Int64 ;
begin
    Move (FileTime, result, SizeOf (result)) ;
end;

//------------------------------------------------------------------------------
const
  FileTimeBase = -109205.0;   // days between years 1601 and 1900
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nsec per Day

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
    Result := FileTimeToInt64 (FileTime) / FileTimeStep ;
    Result := Result + FileTimeBase ;
end;

//------------------------------------------------------------------------------
function GetIpAddrType (wtype: DWORD): string;

    procedure buildres (lit: string);
    begin
      if result <> '' then result := result + ', ';
      result := result + lit;
    end ;

begin
    result := '';
    if wtype = 0 then exit;
    if wtype AND MIB_IPADDR_PRIMARY <> 0 then buildres (MibIpAddrPrimary);
    if wtype AND MIB_IPADDR_DYNAMIC <> 0 then buildres (MibIpAddrDynamic);
    if wtype AND MIB_IPADDR_DISCONNECTED <> 0 then buildres (MibIpAddrDisconnected);
    if wtype AND MIB_IPADDR_DELETED <> 0 then buildres (MibIpAddrDeleted);
    if wtype AND MIB_IPADDR_TRANSIENT <> 0 then buildres (MibIpAddrTransient);
    if wtype AND MIB_IPADDR_DNS_ELIGIBLE <> 0 then buildres (MibIpAddrDnsEligible);
end ;


function GetIfoFlags (Flags: TInterfaceAndOperStatusFlags): string ;

    procedure buildres (lit: string);
    begin
      if result <> '' then result := result + ', ';
      result := result + lit;
    end ;

begin
    result := '';
    if HardwareInterface in Flags then buildres (MibIfoHardwareInterface);
    if FilterInterface in Flags then buildres (MibIfoFilterInterface);
    if ConnectorPresent in Flags then buildres (MibIfoConnectorPresent);
    if NotAuthenticated in Flags then buildres (MibIfoNotAuthenticated);
    if NotMediaConnected in Flags then buildres (MibIfoNotMediaConnected);
    if Paused in Flags then buildres (MibIfoPaused);
    if LowPower in Flags then buildres (MibIfoLowPower);
    if EndPointInterface in Flags then buildres (MibIfoEndPointInterface);  
end ;

//-----------------------------------------------------------------------------
{ general,  fixed network parameters }

procedure Get_NetworkParams( List: TStrings );
var
    NetworkParams: TNetworkParams ;
    I, ErrorCode: integer ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    ErrorCode := IpHlpNetworkParams (NetworkParams) ;
    if ErrorCode <> 0 then
    begin
        List.Add (SysErrorMessage (ErrorCode));
        exit;
    end ;
    with NetworkParams do
    begin
        List.Add( 'HOSTNAME          : ' + HostName );
        List.Add( 'DOMAIN            : ' + DomainName );
        List.Add( 'DHCP SCOPE        : ' + ScopeID );
        List.Add( 'NETBIOS NODE TYPE : ' + NETBIOSTypes[NodeType] );
        List.Add( 'ROUTING ENABLED   : ' + IntToStr( EnableRouting ) );
        List.Add( 'PROXY   ENABLED   : ' + IntToStr( EnableProxy ) );
        List.Add( 'DNS     ENABLED   : ' + IntToStr( EnableDNS ) );
        if DnsServerTot <> 0 then
        begin
            for I := 0 to Pred (DnsServerTot) do
                List.Add( 'DNS SERVER ADDR   : ' + DnsServerNames [I] ) ;
        end ;
    end ;
end ;

function IpHlpNetworkParams (var NetworkParams: TNetworkParams): integer ;
var
  FixedInfo     : PTFixedInfo;         // Angus
  InfoSize      : Longint;
  PDnsServer    : PIpAddrString ;   // Angus
begin
    InfoSize := 0 ;   // Angus
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetNetworkParams( Nil, @InfoSize );  // Angus
    if result <> ERROR_BUFFER_OVERFLOW then exit ; // Angus
    GetMem (FixedInfo, InfoSize) ;                    // Angus
    try
    result := GetNetworkParams( FixedInfo, @InfoSize );   // Angus
    if result <> ERROR_SUCCESS then exit ;
    NetworkParams.DnsServerTot := 0 ;
    with FixedInfo^ do
    begin
        NetworkParams.HostName := Trim (String (HostName)) ;      // 8 Aug 2010
        NetworkParams.DomainName := Trim (String (DomainName)) ;  // 8 Aug 2010
        NetworkParams.ScopeId := Trim (String (ScopeID)) ;        // 8 Aug 2010
        NetworkParams.NodeType := NodeType ;
        NetworkParams.EnableRouting := EnableRouting ;
        NetworkParams.EnableProxy := EnableProxy ;
        NetworkParams.EnableDNS := EnableDNS ;
        NetworkParams.DnsServerNames [0] := String (DNSServerList.IPAddress) ;  // 8 Aug 2010
        if NetworkParams.DnsServerNames [0] <> '' then
                                        NetworkParams.DnsServerTot := 1 ;
        PDnsServer := DnsServerList.Next;
        while PDnsServer <> Nil do
        begin
            NetworkParams.DnsServerNames [NetworkParams.DnsServerTot] :=
                                       String (PDnsServer^.IPAddress) ;   // 8 Aug 2010
            inc (NetworkParams.DnsServerTot) ;
            if NetworkParams.DnsServerTot >=
                            Length (NetworkParams.DnsServerNames) then exit ;
            PDnsServer := PDnsServer.Next ;
        end;
    end ;
    finally
       FreeMem (FixedInfo) ;                     // Angus
    end ;
end;

//------------------------------------------------------------------------------

function ICMPErr2Str( ICMPErrCode: DWORD) : string;
begin
   Result := 'UnknownError : ' + IntToStr( ICMPErrCode );
   dec( ICMPErrCode, ICMP_ERROR_BASE );
   if ICMPErrCode in [Low(ICMpErr)..High(ICMPErr)] then
     Result := ICMPErr[ ICMPErrCode];
end;


//------------------------------------------------------------------------------
// interfaces on PC, similar to adaptors but no addresses
// include bytes in/out for each adaptor, W2K and later

function IpHlpIfTable(var IfTot: integer; var IfRows: TIfRows): integer ;
var
  I,
  TableSize   : integer;
  pBuf, pNext : PAnsiChar;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  SetLength (IfRows, 0) ;
  IfTot := 0 ; // Angus
  TableSize := 0;
   // first call: get memsize needed
  result := GetIfTable (Nil, @TableSize, false) ;  // Angus
  if result <> ERROR_INSUFFICIENT_BUFFER then exit ;
  GetMem( pBuf, TableSize );
  try
      FillChar (pBuf^, TableSize, #0);  // clear buffer, since W98 does not

   // get table pointer
      result := GetIfTable (PTMibIfTable (pBuf), @TableSize, false) ;
      if result <> NO_ERROR then exit ;
      IfTot := PTMibIfTable (pBuf)^.dwNumEntries ;
      if IfTot = 0 then exit ;
      SetLength (IfRows, IfTot) ;
      pNext := pBuf + SizeOf(IfTot) ;
      for i := 0 to Pred (IfTot) do
      begin
         IfRows [i] := PTMibIfRow (pNext )^ ;
         inc (pNext, SizeOf (TMibIfRow)) ;
      end;
  finally
      FreeMem (pBuf) ;
  end ;
end;

//------------------------------------------------------------------------------
// interfaces on PC, similar to adaptors but no addresses
// include bytes in/out for each adaptor, Vista and later
function IpHlpIfTable2(var IfTot: integer; var IfRows2: TIfRows2): integer ;
var
    I: integer;
    pIfTable2: PTMibIfTable2;
    IfRows: TIfRows ;
    sDescr: AnsiString ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    SetLength (IfRows2, 0) ;
    IfTot := 0 ;
    pIfTable2 := nil ;
    if (Win32MajorVersion < 6) OR (NOT Assigned (GetIfTable2Ex)) then
    begin
        result := IpHlpIfTable (IfTot, IfRows) ;
        if result <> NO_ERROR then exit ;
        SetLength (IfRows2, IfTot) ;
        for I := 0 to Pred (IfTot) do
        begin
            with IfRows2 [I] do   // update MIB_IF_ROW2 from MIB_IF_ROW, fewer fields and not exact matches
            begin
                Mib.InterfaceIndex := IfRows [I].dwIndex ;
                Mib.IfType := IfRows [I].dwType ;
                Mib.Mtu := IfRows [I].dwMTU ;
                Mib.TransmitLinkSpeed := IfRows [I].dwSpeed ;
                Move (IfRows [I].bPhysAddr, Mib.PhysicalAddress, IfRows [I].dwPhysAddrLen) ;
                Mib.PhysicalAddressLength := IfRows [I].dwPhysAddrLen ;
                Mib.AdminStatus := IfRows [I].AdminStatus ;
                Mib.OperStatus := TIfOperStatus (IfRows [I].OperStatus) ;
                Mib.InOctets := IfRows [I].dwInOctets ;
                Mib.InUcastPkts := IfRows [I].dwInUcastPkts ;
                Mib.InNUcastPkts := IfRows [I].dwInNUCastPkts ;
                Mib.InDiscards := IfRows [I].dwInDiscards ;
                Mib.InErrors := IfRows [I].dwInErrors ;
                Mib.InUnknownProtos := IfRows [I].dwInUnknownProtos ;
                Mib.OutOctets := IfRows [I].dwOutOctets ;
                Mib.OutUcastPkts := IfRows [I].dwOutUcastPkts ;
                Mib.OutNUcastPkts := IfRows [I].dwOutNUCastPkts ;
                Mib.OutDiscards := IfRows [I].dwOutDiscards ;
                Mib.OutErrors := IfRows [I].dwOutErrors ;
                Mib.OutQLen := IfRows [I].dwOutQLen ;
                sDescr := AnsiString (IfRows [I].bDescr) ;
                Move (IfRows [I].wszName, Mib.Alias, IF_MAX_STRING_SIZE) ;
                FriendlyName := Trim(Mib.Alias) ;
                Description := sDescr ;
            end;
        end;
    end
    else
    begin
   // get table pointer
        try
            result := GetIfTable2Ex (MibIfTableNormal, pIfTable2) ;
            if result <> NO_ERROR then exit ;
            IfTot := pIfTable2^.NumEntries ;
            if IfTot = 0 then exit ;
            SetLength (IfRows2, IfTot) ;
            for I := 0 to Pred (IfTot) do
            begin
                IfRows2 [I].Mib := pIfTable2^.Table [I] ;
                IfRows2 [I].FriendlyName := Trim(IfRows2 [I].Mib.Alias) ;
                IfRows2 [I].Description := Trim(IfRows2 [I].Mib.Description) ;
                IfRows2 [I].InterfaceName := IpHlpConvIntIdxToStr (IfRows2 [I].Mib.InterfaceIndex) ;
            end;
        finally
            FreeMibTable (pIfTable2) ;
        end;
    end ;
end;

procedure Get_IfTable( List: TStrings );
var
  IfRows        : TIfRows ;
  Error, I      : integer;
  NumEntries    : integer;
  sDescr, sIfName: string ;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  SetLength (IfRows, 0) ;
  Error := IpHlpIfTable (NumEntries, IfRows) ;
  if (Error <> NO_ERROR) then
      List.Add( SysErrorMessage( GetLastError ) )
  else if NumEntries = 0 then
      List.Add( 'no entries.' )
  else
  begin
      for I := 0 to Pred (NumEntries) do
      begin
          with IfRows [I] do
          begin
             if wszName [1] = #0 then
                 sIfName := ''
             else
                 sIfName := WideCharToString (@wszName) ;  // convert Unicode to string
             sIfName := Trim (sIfName) ;
             sDescr := Trim (String (bDescr)) ;   // 8 Aug 2010
             List.Add (Format (
               '%0.8x |%-8s |%-18 |%8d |%12d |%-20s |%-20s |%10d |%10d | %-s| %-s',
               [dwIndex, AdaptTypes[dwType], MacAddr2Str( bPhysAddr , dwPhysAddrLen) ,
               dwMTU, dwSpeed,
               AdminStatuses [Ord (AdminStatus)], IfOperStatuses [Ord (OperStatus)],
               Int64 (dwInOctets), Int64 (dwOutOctets),  // counters are 32-bit
               sIfName, sDescr] )  // Angus, added in/out
               );
          end;
      end ;
  end ;
  SetLength (IfRows, 0) ;  // free memory
end ;

procedure Get_IfTable2( List: TStrings );
var
    IfRows2: TIfRows2 ;
    Error, I, J, NumEntries: integer ;
    IpAddrInfos: TIpAddrInfos ;
    S: string ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    Error := IpHlpIfTable2 (NumEntries, IfRows2) ;
    if (Error <> 0) then
        List.Add( SysErrorMessage( GetLastError ) )
    else if NumEntries = 0 then
        List.Add( 'no entries.' )
    else
    begin
        List.Add (Format (
               '%-8s|%-14s|%-40s|%-30s|%-10s|%-17s|%-5s|%-12s|%-8s|%-11s|%-14s|%-14s|%-14s|%-16s|%-30s|%-12s|%-12s|%-12s|%-12s|%-8s',
               ['Index', 'Interface', 'Description', 'Friendly Name',
               'Type', 'MAC Address', 'MTU', 'Speed', 'Admin St', 'Oper Status',
               'Media Type',  'Phys Medium','Access Type', 'Direction', 'Interface/Oper Status',
               'Conn State', 'Conn Type', 'In Octets', 'Out Octets', 'Tunnel' ] ) );
        List.Add('');
        for I := 0 to Pred (NumEntries) do
        begin
            with IfRows2 [I] do
            begin
                List.Add (Format (
                   '%0.8x|%-14s|%-40s|%-30s|%-10s|%-17s|%5d|%12d|%-8s|%-11s|%-14s|%-14s|%-14s|%-16s|%-30s|%-12s|%-12s|%12d|%12d|%-8s',
                   [Mib.InterfaceIndex, InterfaceName, Copy (Description, 1, 40), Copy (FriendlyName, 1, 30),
                   AdaptTypes[Mib.IfType],
                   MacAddr2Str( Mib.PhysicalAddress, Mib.PhysicalAddressLength ),
                   Mib.MTU, Mib.TransmitLinkSpeed,
                   AdminStatuses [Ord (Mib.AdminStatus)], IfOperStatuses [Ord (Mib.OperStatus)],
                   NdisMediums [Ord (Mib.MediaType)],  NdisPhysicalMediums [Ord (Mib.PhysicalMediumType)],
                   NetIfAccessTtypes [Ord (Mib.AccessType)], NetIfDirectionTypes [Ord (Mib.DirectionType)],
                   GetIfoFlags (Mib.InterfaceAndOperStatusFlags), NetIfMediaConnectStates [Ord (Mib.MediaConnectState)],
                   NetIfConnectionTypes  [Ord (Mib.ConnectionType)],
                   Mib.InOctets, Mib.OutOctets, TunnelTypes [Ord (Mib.TunnelType)]] ) );

                if IpHlpIpAddrTable (IpAddrInfos, AF_UNSPEC, True, false, Mib.InterfaceIndex) = 0 then
                begin
                    if Length (IpAddrInfos) <> 0 then
                    begin
                        S := '' ;
                        for J := 0 to Pred (Length (IpAddrInfos)) do
                        begin
                            with IpAddrInfos [J] do
                            begin
                            S := S + IpAddress ;
                            if IPMask <> '' then
                                S := S + '=' + IPMask + ' | '
                            else
                                S := S + ' | ';
                            end;
                        end;
                        List.Add(IntToStr (Length (IpAddrInfos)) + ' IP Address(es): ' + S);
                        SetLength (IpAddrInfos, 0) ;  // free memory
                    end ;
                end;
                List.Add('');
            end;
        end ;
    end ;
    SetLength (IfRows2, 0) ;  // free memory
end ;

function IpHlpIfEntry(Index: integer; var IfRow: TMibIfRow): integer ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  FillChar (IfRow, SizeOf (TMibIfRow), #0);  // clear buffer, since W98 does not
  IfRow.dwIndex := Index ;
  result := GetIfEntry (@IfRow) ;
end ;

//-----------------------------------------------------------------------------
{ Info on installed adapters, IPv4 and/or IPv6 addresses }

function IpHlpAdaptersAddr(Family: TAddressFamily; var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
var
  BufLen   : DWORD;
  PBuf     : PAnsiChar ;
  I        : integer ;
  len      : integer ;
  Flags : LongWord ;
  AdapterAddresses : PIpAdapterAddresses;
  UnicastAddress   : PIpAdapterUnicastAddress;
  AnycastAddress   : PIpAdapterAnycaseAddress;
  MulticastAddress : PIpAdapterMulticastAddress;
  DnsServerAddress : PIpAdapterDnsServerAddress;
  PrefixAddress    : PIpAdapterPrefix;  // aka mask
  WinsServerAddress: PIpAdapterWinsServerAddress;
  GatewayAddress   : PIpAdapterGatewayAddress;
  AdapterAddressLen: integer;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  SetLength (AdpRows, 4) ;
  AdpTot := 0 ;
  BufLen := 0 ;
  if NOT Assigned (GetAdaptersAddresses) then Exit;

  Flags := GAA_FLAG_INCLUDE_PREFIX ;
  if (Win32MajorVersion >= 6) then Flags := Flags OR GAA_FLAG_INCLUDE_GATEWAYS or
                    GAA_FLAG_INCLUDE_WINS_INFO or GAA_FLAG_INCLUDE_ALL_INTERFACES ;  // gateway, etc are Vista and later
  result := GetAdaptersAddresses ( Family, Flags, Nil, Nil, @BufLen) ;
  if (result <> ERROR_BUFFER_OVERFLOW) then exit ;
  GetMem( pBuf, BufLen );
  try
      FillChar (pBuf^, BufLen, #0);  // clear buffer
      result := GetAdaptersAddresses ( Family, Flags, Nil, PIpAdapterAddresses(PBuf), @BufLen );
      if result = NO_ERROR then
      begin
         AdapterAddresses := PIpAdapterAddresses(PBuf) ;
         while ( AdapterAddresses <> nil ) do
         begin
            AdapterAddressLen := AdapterAddresses^.Union.Length ; // 144 for XP SP3, 376 for Win7
            AdpRows [AdpTot].IPAddressTot := 0 ;
            SetLength (AdpRows [AdpTot].IPAddressList, 2) ;
            SetLength (AdpRows [AdpTot].IPMaskList, 2) ;
            AdpRows [AdpTot].GatewayTot := 0 ;
            SetLength (AdpRows [AdpTot].GatewayList, 2) ;
            AdpRows [AdpTot].DHCPTot := 0 ;
            SetLength (AdpRows [AdpTot].DHCPServer, 2) ;
            AdpRows [AdpTot].PrimWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].PrimWINSServer, 2) ;
            AdpRows [AdpTot].SecWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].SecWINSServer, 2) ;
            AdpRows [AdpTot].DNSServerTot := 0 ;
            SetLength (AdpRows [AdpTot].DNSServerList, 2) ;
            AdpRows [AdpTot].DNSServerList [0] := '' ;
            AdpRows [AdpTot].AnycastIPAddrTot := 0 ;
            SetLength (AdpRows [AdpTot].AnycastIPAddrList, 2) ;
            AdpRows [AdpTot].MulticastIPAddrTot := 0 ;
            SetLength (AdpRows [AdpTot].MulticastIPAddrList, 2) ;
            AdpRows [AdpTot].PrefixTot := 0 ;
            SetLength (AdpRows [AdpTot].PrefixIPAddrList, 2) ;
            SetLength (AdpRows [AdpTot].PrefixMaskList, 2) ;
            AdpRows [AdpTot].CurrIPAddress := NULL_IP;
            AdpRows [AdpTot].CurrIPMask := NULL_IP;
            AdpRows [AdpTot].AdapterName := Trim (WideString (AdapterAddresses^.AdapterName)) ; // 8 Aug 2010
            AdpRows [AdpTot].Description := Trim (AdapterAddresses^.Description) ;
            AdpRows [AdpTot].FriendlyName := Trim (AdapterAddresses^.FriendlyName) ;
            AdpRows [AdpTot].MacAddress := MacAddr2Str( TMacAddress(
                 AdapterAddresses^.PhysicalAddress ), AdapterAddresses^.PhysicalAddressLength ) ;
            AdpRows [AdpTot].Index := AdapterAddresses^.Union.IfIndex ;   // IP4 interface ID
            AdpRows [AdpTot].InterfaceName := IpHlpConvIntIdxToStr (AdpRows [AdpTot].Index) ;  // Nov 2014
            AdpRows [AdpTot].aType := AdapterAddresses^.IfType ;
            AdpRows [AdpTot].DHCPEnabled := 0 ;
            if ((AdapterAddresses^.Flags AND IP_ADAPTER_DHCP_ENABLED) =
                                IP_ADAPTER_DHCP_ENABLED) then AdpRows [AdpTot].DHCPEnabled := 1 ;
            AdpRows [AdpTot].Mtu := AdapterAddresses^.Mtu ;
            AdpRows [AdpTot].IfType := AdapterAddresses^.IfType ;
            AdpRows [AdpTot].OperStatus := AdapterAddresses^.OperStatus ;
            AdpRows [AdpTot].DnsSuffix := Trim (AdapterAddresses^.DnsSuffix) ;  // Nov 2014

        // Unicast, IP for single interface, get list of IP addresses and masks for IPAddressList
            I := 0 ;
            UnicastAddress := AdapterAddresses^.FirstUnicastAddress ;
            while (UnicastAddress <> Nil) do
            begin
                len := UnicastAddress.Union.Length ;
                if len <> 48 then break ; // sanity check
                AdpRows [AdpTot].IPAddressList [I] := SocketAddr2Str (UnicastAddress.Address) ;
                UnicastAddress := UnicastAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].IPAddressList) <= I then
                begin
                     SetLength (AdpRows [AdpTot].IPAddressList, I * 2) ;
                     SetLength (AdpRows [AdpTot].IPMaskList, I * 2) ;  // Nov 2014 NOT USED, NO MASKS!!
                end ;
            end ;
            AdpRows [AdpTot].IPAddressTot := I ;

        // Address Prefix, aka IP masks  - XP SP1 and later only
        // only one mask appears if they are all the same
            I := 0 ;
            PrefixAddress := AdapterAddresses^.FirstPrefix ;
            while (PrefixAddress <> Nil) do
            begin
                len := PrefixAddress.Union.Length ;
                if len <> 24 then break ; // sanity check
                if PrefixAddress.Address.lpSockaddr.si_family = AF_INET then
                    AdpRows [AdpTot].PrefixMaskList [I] := CreateMask (PrefixAddress.PrefixLength)
                else
                    AdpRows [AdpTot].PrefixMaskList [I] := '/' + IntToStr(PrefixAddress.PrefixLength);
                AdpRows [AdpTot].PrefixIPAddrList [I] := SocketAddr2Str (PrefixAddress.Address) ; // ie 192.168.0.0 for mask 255.255.0.0, len=16
                PrefixAddress := PrefixAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].PrefixMaskList) <= I then
                begin
                    SetLength (AdpRows [AdpTot].PrefixMaskList, I * 2) ;
                    SetLength (AdpRows [AdpTot].PrefixIPAddrList, I * 2) ;
                end ;
            end ;
            AdpRows [AdpTot].PrefixTot := I ;

        // keep first IP as current, best we can do
            if AdpRows [AdpTot].IPAddressTot > 0 then
            begin
                AdpRows [AdpTot].CurrIPAddress := AdpRows [AdpTot].IPAddressList [0] ;
                AdpRows [AdpTot].CurrIPMask := AdpRows [AdpTot].IPMaskList [0] ;
            end ;

        // Anycast IP6, group of IP addresses
            I := 0 ;
            AnycastAddress := AdapterAddresses^.FirstAnycastAddress ;
            while (AnycastAddress <> Nil) do
            begin
                len := AnycastAddress.Union.Length ;
                if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].AnycastIPAddrList [I] := SocketAddr2Str (AnycastAddress.Address) ;
                inc (I) ;
                if Length (AdpRows [AdpTot].AnycastIPAddrList) <= I then
                     SetLength (AdpRows [AdpTot].AnycastIPAddrList, I * 2) ;
                AnycastAddress := AnycastAddress.Next ;
            end ;
            AdpRows [AdpTot].AnycastIPAddrTot := I ;

        // Multicast IP6, broadcast IP addresses
            I := 0 ;
            MulticastAddress := AdapterAddresses^.FirstMulticastAddress ;
            while (MulticastAddress <> Nil) do
            begin
                len := MulticastAddress.Union.Length ;
                if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].MulticastIPAddrList [I] := SocketAddr2Str (MulticastAddress.Address) ;
                inc (I) ;
                if Length (AdpRows [AdpTot].MulticastIPAddrList) <= I then
                         SetLength (AdpRows [AdpTot].MulticastIPAddrList, I * 2) ;
                MulticastAddress := MulticastAddress.Next ;
            end ;
            AdpRows [AdpTot].MulticastIPAddrTot := I ;

        // get list of DNS server addresses
            I := 0 ;
            DnsServerAddress := AdapterAddresses^.FirstDnsServerAddress ;
            while (DnsServerAddress <> Nil) do
            begin
                len := DnsServerAddress.Union.Length ;
                if len <> 24 then break ; // sanity check
                AdpRows [AdpTot].DNSServerList [I] := SocketAddr2Str (DnsServerAddress.Address) ;
                DnsServerAddress := DnsServerAddress.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].DNSServerList) <= I then
                     SetLength (AdpRows [AdpTot].DNSServerList, I * 2) ;
                AdpRows [AdpTot].CurrentDNSServer := AdpRows [AdpTot].DNSServerList [0] ;
            end ;
            AdpRows [AdpTot].DNSServerTot := I ;

        // stuff only available for Vista and later
         //   AdpRows [AdpTot].PrimWINSServer [0] := 'AddrLen=' + IntToStr (AdapterAddressLen) ; // !! TEMP
            if (Win32MajorVersion >= 6) and (AdapterAddressLen > 300) then
            begin
                AdpRows [AdpTot].Ipv6Index := AdapterAddresses^.Ipv6IfIndex ;
                AdpRows [AdpTot].XmitLinkSpeed := AdapterAddresses^.TransmitLinkSpeed ;
                AdpRows [AdpTot].RecvLinkSpeed := AdapterAddresses^.ReceiveLinkSpeed ;
                AdpRows [AdpTot].Ipv4Metric := AdapterAddresses^.Ipv4Metric ;
                AdpRows [AdpTot].Ipv6Metric := AdapterAddresses^.Ipv6Metric ;
                AdpRows [AdpTot].Luid := AdapterAddresses^.Luid ;
                AdpRows [AdpTot].CompartmentId := AdapterAddresses^.CompartmentId ;
                AdpRows [AdpTot].NetworkGuid := AdapterAddresses^.NetworkGuid ;
                AdpRows [AdpTot].ConnectionType := AdapterAddresses^.ConnectionType ;
                AdpRows [AdpTot].TunnelType := AdapterAddresses^.TunnelType ;

            // get list of IP addresses for GatewayList
                I := 0 ;
                GatewayAddress := AdapterAddresses^.FirstGatewayAddress ;
                while (GatewayAddress <> Nil) do
                begin
                    len := GatewayAddress.Union.Length ;
                    if len <> 24 then break ; // sanity check
                    AdpRows [AdpTot].GatewayList [I] := SocketAddr2Str (GatewayAddress.Address) ;
                    GatewayAddress := GatewayAddress.Next ;
                    inc (I) ;
                    if Length (AdpRows [AdpTot].GatewayList) <= I then
                             SetLength (AdpRows [AdpTot].GatewayList, I * 2) ;
                end ;
                AdpRows [AdpTot].GatewayTot := I ;

            // get list of IP addresses for Primary WIIS Server
                I := 0 ;
                WinsServerAddress := AdapterAddresses^.FirstWinsServerAddress ;
                while (WinsServerAddress <> Nil) do
                begin
                    len := WinsServerAddress.Union.Length ;
                    if len <> 24 then break ; // sanity check
                    AdpRows [AdpTot].PrimWINSServer [I] := SocketAddr2Str (WinsServerAddress.Address) ;
                    WinsServerAddress := WinsServerAddress.Next ;
                    inc (I) ;
                    if Length (AdpRows [AdpTot].PrimWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].PrimWINSServer, I * 2) ;
                end ;
                AdpRows [AdpTot].PrimWINSTot := I ;

            end;

        // get ready for next adaptor
            inc (AdpTot) ;
            if Length (AdpRows) <= AdpTot then SetLength (AdpRows, AdpTot * 2) ;  // more memory
            AdapterAddresses := AdapterAddresses^.Next;
         end ;
         SetLength (AdpRows, AdpTot) ;
      end;
  finally
      FreeMem( pBuf );
  end;
end;

// adaptors and IPv4 addresses only

function IpHlpAdaptersInfo(var AdpTot: integer; var AdpRows: TAdaptorRows): integer ;
var
  BufLen        : DWORD;
  AdapterInfo   : PIpAdapterInfo;
  PIpAddr       : PIpAddrString;
  PBuf          : PAnsiChar ;
  I          : integer ;
  PerAdapterInfo: TIpPerAdapterInfo ;
  ret      : integer ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  SetLength (AdpRows, 4) ;
  AdpTot := 0 ;
  BufLen := 0 ;
  result := GetAdaptersInfo( Nil, @BufLen );
//  if (result <> ERROR_INSUFFICIENT_BUFFER) and (result = NO_ERROR) then exit ;
  if (result <> ERROR_BUFFER_OVERFLOW) then exit ;  // 11 Jan 2009 should be the only result
  GetMem( pBuf, BufLen );
  try
      FillChar (pBuf^, BufLen, #0);  // clear buffer
      result := GetAdaptersInfo( PIpAdapterInfo (PBuf), @BufLen );
      if result = NO_ERROR then
      begin
         AdapterInfo := PIpAdapterInfo (PBuf) ;
         while ( AdapterInfo <> nil ) do
         begin
            AdpRows [AdpTot].IPAddressTot := 0 ;
            SetLength (AdpRows [AdpTot].IPAddressList, 2) ;
            SetLength (AdpRows [AdpTot].IPMaskList, 2) ;
            AdpRows [AdpTot].GatewayTot := 0 ;
            SetLength (AdpRows [AdpTot].GatewayList, 2) ;
            AdpRows [AdpTot].DHCPTot := 0 ;
            SetLength (AdpRows [AdpTot].DHCPServer, 2) ;
            AdpRows [AdpTot].PrimWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].PrimWINSServer, 2) ;
            AdpRows [AdpTot].SecWINSTot := 0 ;
            SetLength (AdpRows [AdpTot].SecWINSServer, 2) ;
            AdpRows [AdpTot].DNSServerTot := 0 ;
            SetLength (AdpRows [AdpTot].DNSServerList, 2) ;
            AdpRows [AdpTot].DNSServerList [0] := '' ;
            AdpRows [AdpTot].CurrIPAddress := NULL_IP;
            AdpRows [AdpTot].CurrIPMask := NULL_IP;
            AdpRows [AdpTot].AdapterName := Trim( string( AdapterInfo^.AdapterName ) );
            AdpRows [AdpTot].Description := Trim( string( AdapterInfo^.Description ) );
            AdpRows [AdpTot].MacAddress := MacAddr2Str( TMacAddress(
                                 AdapterInfo^.Address ), AdapterInfo^.AddressLength ) ;
            AdpRows [AdpTot].Index := AdapterInfo^.Index ;
            AdpRows [AdpTot].InterfaceName := IpHlpConvIntIdxToStr (AdpRows [AdpTot].Index) ;  // Nov 2014
            AdpRows [AdpTot].aType := AdapterInfo^.aType ;
            AdpRows [AdpTot].DHCPEnabled := AdapterInfo^.DHCPEnabled ;
            if AdapterInfo^.CurrentIPAddress <> Nil then
            begin
                AdpRows [AdpTot].CurrIPAddress := String (AdapterInfo^.CurrentIPAddress.IpAddress) ;   // 8 Aug 2010
                AdpRows [AdpTot].CurrIPMask := String (AdapterInfo^.CurrentIPAddress.IpMask) ;   // 8 Aug 2010
            end ;

        // get list of IP addresses and masks for IPAddressList
            I := 0 ;
            PIpAddr := @AdapterInfo^.IPAddressList ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].IPAddressList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                AdpRows [AdpTot].IPMaskList [I] := String (PIpAddr.IpMask) ;  // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].IPAddressList) <= I then
                begin
                     SetLength (AdpRows [AdpTot].IPAddressList, I * 2) ;
                     SetLength (AdpRows [AdpTot].IPMaskList, I * 2) ;
                end ;
            end ;
            AdpRows [AdpTot].IPAddressTot := I ;

        // get list of IP addresses for GatewayList
            I := 0 ;
            PIpAddr := @AdapterInfo^.GatewayList ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].GatewayList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].GatewayList) <= I then
                             SetLength (AdpRows [AdpTot].GatewayList, I * 2) ;
            end ;
            AdpRows [AdpTot].GatewayTot := I ;

        // get list of IP addresses for DHCP Server
            I := 0 ;
            PIpAddr := @AdapterInfo^.DHCPServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].DHCPServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].DHCPServer) <= I then
                             SetLength (AdpRows [AdpTot].DHCPServer, I * 2) ;
            end ;
            AdpRows [AdpTot].DHCPTot := I ;

        // get list of IP addresses for PrimaryWINSServer
            I := 0 ;
            PIpAddr := @AdapterInfo^.PrimaryWINSServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].PrimWINSServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].PrimWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].PrimWINSServer, I * 2) ;
            end ;
            AdpRows [AdpTot].PrimWINSTot := I ;

       // get list of IP addresses for SecondaryWINSServer
            I := 0 ;
            PIpAddr := @AdapterInfo^.SecondaryWINSServer ;
            while (PIpAddr <> Nil) do
            begin
                AdpRows [AdpTot].SecWINSServer [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                PIpAddr := PIpAddr.Next ;
                inc (I) ;
                if Length (AdpRows [AdpTot].SecWINSServer) <= I then
                             SetLength (AdpRows [AdpTot].SecWINSServer, I * 2) ;
            end ;
            AdpRows [AdpTot].SecWINSTot := I ;

            AdpRows [AdpTot].LeaseObtained := AdapterInfo^.LeaseObtained ;
            AdpRows [AdpTot].LeaseExpires := AdapterInfo^.LeaseExpires ;

       // get per adaptor info, W2K and later - 1.5 12 July 2002
            if Assigned (GetPerAdapterInfo) then
            begin
                BufLen := SizeOf (PerAdapterInfo) ;
                ret := GetPerAdapterInfo (AdpRows [AdpTot].Index, @PerAdapterInfo, @BufLen) ;
                if ret = 0 then
                begin
                    AdpRows [AdpTot].AutoConfigEnabled := PerAdapterInfo.AutoconfigEnabled ;
                    AdpRows [AdpTot].AutoConfigActive := PerAdapterInfo.AutoconfigActive ;
                    if PerAdapterInfo.CurrentDNSServer <> Nil then
                        AdpRows [AdpTot].CurrentDNSServer := String (PerAdapterInfo.CurrentDNSServer.IpAddress) ; // 8 Aug 2010

                // get list of DNS IP addresses
                    I := 0 ;
                    PIpAddr := @PerAdapterInfo.DNSServerList ;
                    while (PIpAddr <> Nil) do
                    begin
                        AdpRows [AdpTot].DNSServerList [I] := String (PIpAddr.IpAddress) ; // 8 Aug 2010
                        PIpAddr := PIpAddr.Next ;
                        inc (I) ;
                        if Length (AdpRows [AdpTot].DNSServerList) <= I then
                        begin
                             SetLength (AdpRows [AdpTot].DNSServerList, I * 2) ;
                        end ;
                    end ;
                    AdpRows [AdpTot].DNSServerTot := I ;
                end ;
            end ;

        // get ready for next adaptor
            inc (AdpTot) ;
            if Length (AdpRows) <= AdpTot then
                            SetLength (AdpRows, AdpTot * 2) ;  // more memory
            AdapterInfo := AdapterInfo^.Next;
         end ;
         SetLength (AdpRows, AdpTot) ;
      end ;
  finally
      FreeMem( pBuf );
  end;
end ;

procedure Get_AdaptersInfo( List: TStrings );
var
  AdpTot: integer;
  AdpRows: TAdaptorRows ;
  Error: DWORD ;
  I, J: integer ;
  S: string ;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    SetLength (AdpRows, 0) ;
    AdpTot := 0 ;
    Error := IpHlpAdaptersAddr(AF_UNSPEC, AdpTot, AdpRows) ; // IPv4 and IPv6
    if (Error <> 0) then
        List.Add( SysErrorMessage( GetLastError ) )
    else if AdpTot = 0 then
        List.Add( 'no entries.' )
    else
    begin
        List.Add( Format('%-8s|%-14s|%-40s|%-30s|%-10s|%-17s|%-4s|%-16s|%-11s|%-16s|%-16s|%-6s|%-12s|%-10s|%-10s|%s',
                    ['Index', 'Interface', 'Description', 'Friendly Name',
                    'Type', 'MAC Address', 'DHCP', 'DND Suffix', 'Xmit Speed', 'DHCP Server', 'WINS Server',
                    'Metric','Op Status', 'Conn Type', 'Tunnel', 'GUID' ])) ;
        List.Add('');
        for I := 0 to Pred (AdpTot) do
        begin
            with AdpRows [I] do
            begin
                List.Add( Format('%8.8x|%-14s|%-40s|%-30s|%-10s|%-17s|%4d|%-16s|%11d|%-16s|%-16s|%6d|%-12s|%-10s|%-10s|%s',
                    [Index, InterfaceName, Copy (Description, 1, 40), Copy (FriendlyName, 1, 30),
                    AdaptTypes [aType], MacAddress, DHCPEnabled, DnsSuffix, XmitLinkSpeed,
                    DHCPServer [0], PrimWINSServer [0], Ipv4Metric,
                    IfOperStatuses [Ord (OperStatus)], NetIfConnectionTypes [Ord (ConnectionType)],
                    TunnelTypes [Ord (TunnelType)], AdapterName
                     ])) ;

                if IPAddressTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (IPAddressTot) do
                    begin
                        S := S + IPAddressList [J] ;
                        if IPMaskList [J] <> '' then
                            S := S + '/' + IPMaskList [J] + ' | '
                        else
                            S := S + ' | ';
                    end;
                    List.Add(IntToStr (IPAddressTot) + ' IP Addresse(s): ' + S);
                end ;
                if PrefixTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (PrefixTot) do
                    begin
                        S := S + PrefixIPAddrList [J] ;
                        if PrefixMaskList [J] <> '' then
                            S := S + '=' + PrefixMaskList [J] + ' | '
                        else
                            S := S + ' | ';
                    end;
                    List.Add(IntToStr (PrefixTot) + ' IP Prefixes(s): ' + S);
                end ;
                if DNSServerTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (DNSServerTot) do
                        S := S + DNSServerList [J] + ' | ';
                    List.Add(IntToStr (DNSServerTot) + ' DNS Server(s): ' + S);
                end ;
                if GatewayTot <> 0 then
                begin
                    S := '' ;
                    for J := 0 to Pred (GatewayTot) do
                        S := S + GatewayList [J] + ' | ';
                    List.Add(IntToStr (GatewayTot) + ' Gateway(s): ' + S);
                end ;
                List.Add( '  ' );
            end ;
       end ;
  end ;
  SetLength (AdpRows, 0) ;
end ;

//-----------------------------------------------------------------------------
{ get round trip time and hopcount to indicated IP }
function Get_RTTAndHopCount( IPAddr: DWORD; MaxHops: Longint; var RTT: Longint;
  var HopCount: Longint ): integer;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  if not GetRTTAndHopCount( IPAddr, @HopCount, MaxHops, @RTT ) then
  begin
    Result := GetLastError;
    RTT := -1; // Destination unreachable, BAD_HOST_NAME,etc...
    HopCount := -1;
  end
  else
    Result := NO_ERROR;
end;

//-----------------------------------------------------------------------------
{ ARP-table lists relations between remote IP and remote MAC-address.
 NOTE: these are cached entries ;when there is no more network traffic to a
 node, entry is deleted after a few minutes.
}
procedure Get_ARPTable( List: TStrings );
var
  IPNetRow      : TMibIPNetRow;
  TableSize     : DWORD;
  NumEntries    : DWORD;
  ErrorCode     : DWORD;
  i             : integer;
  pBuf          : PAnsiChar;
begin
  if NOT LoadIpHlp then exit ;
  if not Assigned( List ) then EXIT;
  List.Clear;
  // first call: get table length
  TableSize := 0;
  ErrorCode := GetIPNetTable( Nil, @TableSize, false );   // Angus
  //
  if ErrorCode = ERROR_NO_DATA then
  begin
    List.Add( ' ARP-cache empty.' );
    EXIT;
  end;
  // get table
  GetMem( pBuf, TableSize );
  NumEntries := 0 ;
  try
  ErrorCode := GetIpNetTable( PTMIBIPNetTable( pBuf ), @TableSize, false );
  if ErrorCode = NO_ERROR then
  begin
    NumEntries := PTMIBIPNetTable( pBuf )^.dwNumEntries;
    if NumEntries > 0 then // paranoia striking, but you never know...
    begin
      inc( pBuf, SizeOf( DWORD ) ); // get past table size
      for i := 1 to NumEntries do
      begin
        IPNetRow := PTMIBIPNetRow( PBuf )^;
        with IPNetRow do
          List.Add( Format( '%8x | %-20s | %-16s| %-10s',
                           [dwIndex, MacAddr2Str( bPhysAddr, dwPhysAddrLen ),
                           IPAddr2Str( dwAddr ), ARPEntryType[dwType]
                           ]));
        inc( pBuf, SizeOf( IPNetRow ) );
      end;
    end
    else
      List.Add( ' ARP-cache empty.' );
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );

  // we _must_ restore pointer!
  finally
      dec( pBuf, SizeOf( DWORD ) + NumEntries * SizeOf( IPNetRow ) );
      FreeMem( pBuf );
  end ;
end;

//------------------------------------------------------------------------------

// get list of current TCP connections, XP gets process Id so we can find EXE

function IpHlpTCPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET): integer ;
var
  i, NumEntries, CurEntry, TableLen : integer;
  TableSize, ModSize : DWORD;
  ErrorCode2 : DWORD;
  pTCPTableEx2  : PTMibTCPTableOwnerModule;
  TcpIpOwnerModuleBasicInfoEx: TTcpIpOwnerModuleBasicInfoEx ;
  pTCP6TableEx2  : PTMibTCP6TableOwnerModule;
  LocalFileTime: TFileTime ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  CurEntry := 0 ;
  TableLen := 0 ;
  SetLength (ConnRows, 0) ;
  if not Assigned (GetExtendedTCPTable) then exit;
  pTCPTableEx2 := Nil ;
  pTCP6TableEx2 := Nil ;

  try
    // use latest API XP SP2, W2K3 SP1, Vista and later, first call : get size of table
    // IPv4 connections
      if Family in [AF_INET, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedTCPTable (Nil, @TableSize, false, AF_INET, TCP_TABLE_OWNER_MODULE_ALL, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;
          // get required size of memory, call again
          GetMem (pTCPTableEx2, TableSize);
          // get table
          result := GetExtendedTCPTable (pTCPTableEx2, @TableSize, true, AF_INET, TCP_TABLE_OWNER_MODULE_ALL, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pTCPTableEx2^.dwNumEntries;
          if NumEntries >= 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pTCPTableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      State := dwState ;
                      LocSockAddr.si_family := AF_INET;
                      LocSockAddr.Ipv4.sin_addr := in_addr (dwLocalAddr);
                      LocalAddr := IpAddr2Str (dwLocalAddr) ;
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      RemSockAddr.si_family := AF_INET;
                      RemSockAddr.Ipv4.sin_addr := in_addr (dwRemoteAddr);
                      RemoteAddr := IPAddr2Str (dwRemoteAddr) ;
                      RemotePort := Port2Wrd (dwRemotePort) ;
                      if dwRemoteAddr = 0 then RemotePort := 0;
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromTcpEntry ( @pTCPTableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath ;
                      end;
                  end;
                  inc (CurEntry) ;
              end ;
          end;
      end ;

   // IPv6 connections
      if Family in [AF_INET6, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedTCPTable (Nil, @TableSize, false, AF_INET6, TCP_TABLE_OWNER_MODULE_ALL, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;
          // get required size of memory, call again
          GetMem (pTCP6TableEx2, TableSize);
          // get table
          result := GetExtendedTCPTable (pTCP6TableEx2, @TableSize, true, AF_INET6, TCP_TABLE_OWNER_MODULE_ALL, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pTCP6TableEx2^.dwNumEntries;
          if NumEntries > 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pTCP6TableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      State := dwState ;
                      LocSockAddr.si_family := AF_INET6;
                      LocSockAddr.Ipv6.sin6_addr := ucLocalAddr;
                      LocSockAddr.Ipv6.sin6_scope_id := dwLocalScopeId ;
                      LocalAddr := Ip6Addr2Str (ucLocalAddr, dwLocalScopeId) ;
//                      LocalAddr := Ip6Addr2Str2 (ucLocalAddr, dwLocalScopeId, dwLocalPort) ;  // temp testing
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      RemSockAddr.si_family := AF_INET6;
                      RemSockAddr.Ipv6.sin6_addr := ucRemoteAddr;
                      RemSockAddr.Ipv6.sin6_scope_id := dwRemoteScopeId ;
                      RemoteAddr := Ip6Addr2Str (ucRemoteAddr, dwRemoteScopeId) ;
                      RemotePort := Port2Wrd (dwRemotePort) ;
                      if RemoteAddr  = '' then RemotePort := 0;
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromTcp6Entry ( @pTCP6TableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath ;
                      end;
                  end;
                  inc (CurEntry) ;
              end ;
          end;
      end
  finally
    if pTCPTableEx2 <> Nil then FreeMem (pTCPTableEx2) ;
    if pTCP6TableEx2 <> Nil then FreeMem (pTCP6TableEx2) ;
  end ;
end;

//------------------------------------------------------------------------------

// display list of current TCP connections

procedure Get_TCPTable( List: TStrings );
var
  ConnRows: TConnRows ;
  ErrorCode, NumEntries, I: integer ;
  DispName, DispTime: string ;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  RecentIPs.Clear;
  ErrorCode := IpHlpTCPTable (ConnRows, AF_UNSPEC) ;  // both IPv4 and IPv6
  if ErrorCode <> NO_ERROR then
  begin
     List.Add (SysErrorMessage (ErrorCode));
     exit ;
  end;
  NumEntries := Length (ConnRows) ;
  if NumEntries = 0 then
  begin
      List.Add ('No TCP/IP connections') ;
      exit ;
  end ;
  for I := 0 to Pred (NumEntries) do
  begin
      with ConnRows [I] do
      begin

        // build display for user
          if ShowExePath then       // 15 Jan 2009
              DispName := ProcName
          else
              DispName := ExtractFileName (ProcName) ;
          DispTime := '' ;
          if CreateDT > 0 then DispTime := DateTimeToStr (CreateDT) ;
          List.Add (Format( '%-30s : %-7s|%-30s : %-7s| %-15s| %8d|%-37s|%-20s',
                  [LocalAddr, Port2Svc (LocalPort),
                  RemoteAddr, Port2Svc (RemotePort),
                  TCPConnState[State], ProcessId, DispName, DispTime] ) );
          if (not (RemoteAddr = ''))
                 and ( RecentIps.IndexOf(RemoteAddr) = -1 ) then
                                            RecentIPs.Add (RemoteAddr) ;
      end ;
  end ;
end ;

//------------------------------------------------------------------------------

procedure Get_TCPStatistics( List: TStrings );
var
  TCPStats      : TMibTCPStats;
  ErrorCode     : DWORD;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  if NOT LoadIpHlp then exit ;
  ErrorCode := GetTCPStatistics( @TCPStats );
  if ErrorCode = NO_ERROR then
    with TCPStats do
    begin
      List.Add( 'Retransmission algorithm :' + TCPToAlgo[dwRTOAlgorithm] );
      List.Add( 'Minimum Time-Out         :' + IntToStr( dwRTOMin ) + ' ms' );
      List.Add( 'Maximum Time-Out         :' + IntToStr( dwRTOMax ) + ' ms' );
      List.Add( 'Maximum Pend.Connections :' + IntToStr( dwRTOAlgorithm ) );
      List.Add( 'Active Opens             :' + IntToStr( dwActiveOpens ) );
      List.Add( 'Passive Opens            :' + IntToStr( dwPassiveOpens ) );
      List.Add( 'Failed Open Attempts     :' + IntToStr( dwAttemptFails ) );
      List.Add( 'Established conn. Reset  :' + IntToStr( dwEstabResets ) );
      List.Add( 'Current Established Conn.:' + IntToStr( dwCurrEstab ) );
      List.Add( 'Segments Received        :' + IntToStr( dwInSegs ) );
      List.Add( 'Segments Sent            :' + IntToStr( dwOutSegs ) );
      List.Add( 'Segments Retransmitted   :' + IntToStr( dwReTransSegs ) );
      List.Add( 'Incoming Errors          :' + IntToStr( dwInErrs ) );
      List.Add( 'Outgoing Resets          :' + IntToStr( dwOutRsts ) );
      List.Add( 'Cumulative Connections   :' + IntToStr( dwNumConns ) );
    end
  else
    List.Add( SyserrorMessage( ErrorCode ) );
end;

function IpHlpTCPStatistics (var TCPStats: TMibTCPStats): integer ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetTCPStatistics( @TCPStats );
end;

//------------------------------------------------------------------------------

// get list of current UDP connections, XP gets process Id so we can find EXE

function IpHlpUDPTable(var ConnRows: TConnRows; Family: TAddressFamily = AF_INET): integer ;
var
  i, NumEntries, CurEntry, TableLen : integer;
  TableSize, ModSize    : DWORD;
  ErrorCode2 : DWORD;
  pUDPTableEx2: PTMibUDPTableOwnerModule;
  TcpIpOwnerModuleBasicInfoEx: TTcpIpOwnerModuleBasicInfoEx ;
  pUDP6TableEx2: PTMibUDP6TableOwnerModule;
  LocalFileTime: TFileTime ;
begin
  result := ERROR_NOT_SUPPORTED ;
  if NOT LoadIpHlp then exit ;
  CurEntry := 0 ;
  TableLen := 0 ;
  SetLength (ConnRows, 0) ;
  if not Assigned (GetExtendedUDPTable) then exit;
  pUDPTableEx2 := Nil ;
  pUDP6TableEx2 := nil ;

  try
      // use latest API XP SP2, W2K3 SP1, Vista and later, first call : get size of table
      if Family in [AF_INET, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedUDPTable (Nil, @TableSize, false, AF_INET, UDP_TABLE_OWNER_MODULE, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;

          // get required size of memory, call again
          GetMem (pUDPTableEx2, TableSize);
          // get table
          result := GetExtendedUdpTable (pUDPTableEx2, @TableSize, true, AF_INET, UDP_TABLE_OWNER_MODULE, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pUDPTableEx2^.dwNumEntries;
          if NumEntries <> 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pUDPTableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      State := -1 ;
                      LocSockAddr.si_family := AF_INET;
                      LocSockAddr.Ipv4.sin_addr := in_addr (dwLocalAddr);
                      LocalAddr := IpAddr2Str (dwLocalAddr) ;
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      RemoteAddr := '' ;
                      RemotePort := 0 ;
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromUdpEntry ( @pUDPTableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath ;
                      end;
                  end;
                  inc (CurEntry) ;
              end;
          end ;
      end;
      if Family in [AF_INET6, AF_UNSPEC] then
      begin
          TableSize := 0 ;
          result := GetExtendedUDPTable (Nil, @TableSize, false, AF_INET6, UDP_TABLE_OWNER_MODULE, 0);
          if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;

          // get required size of memory, call again
          GetMem (pUDP6TableEx2, TableSize);
          // get table
          result := GetExtendedUdpTable (pUDP6TableEx2, @TableSize, true, AF_INET6, UDP_TABLE_OWNER_MODULE, 0) ;
          if result <> NO_ERROR then exit ;
          NumEntries := pUDP6TableEx2^.dwNumEntries;
          if NumEntries <> 0 then
          begin
              TableLen := TableLen + NumEntries;
              SetLength (ConnRows, TableLen) ;
              for I := 0 to Pred (NumEntries) do
              begin
                  with ConnRows [CurEntry], pUDP6TableEx2^.Table [I] do
                  begin
                      ProcName := '' ;
                      State := -1 ;
                      LocSockAddr.si_family := AF_INET6;
                      LocSockAddr.Ipv6.sin6_addr := ucLocalAddr;
                      LocSockAddr.Ipv6.sin6_scope_id := dwLocalScopeId ;
                      LocalAddr := Ip6Addr2Str (ucLocalAddr, dwLocalScopeId) ;
                      LocalPort := Port2Wrd (dwLocalPort) ;
                      RemSockAddr.si_family := 0 ;
                      RemoteAddr := '' ;
                      RemotePort := 0 ;
                      FileTimeToLocalFileTime (liCreateTimestamp, LocalFileTime) ;
                      CreateDT := FileTimeToDateTime (LocalFileTime) ;
                      ProcessID := dwOwningPid ;
                      if ProcessID > 0 then
                      begin
                          ModSize := SizeOf (TcpIpOwnerModuleBasicInfoEx) ;
                          ErrorCode2 := GetOwnerModuleFromUdp6Entry ( @pUDP6TableEx2^.Table [I],
                                TcpIpOwnerModuleInfoClassBasic, @TcpIpOwnerModuleBasicInfoEx, @ModSize);
                          if ErrorCode2 = NO_ERROR then
                                  ProcName := TcpIpOwnerModuleBasicInfoEx.TcpIpOwnerModuleBasicInfo.pModulePath ;
                      end;
                  end;
                  inc (CurEntry) ;
              end;
          end ;
      end;

  finally
     if pUdpTableEx2 <> Nil then FreeMem (pUdpTableEx2) ;
     if pUdp6TableEx2 <> Nil then FreeMem (pUdp6TableEx2) ;
  end ;
end;

//------------------------------------------------------------------------------

// display list of current UDP connections

procedure Get_UDPTable( List: TStrings );
var
  ConnRows: TConnRows ;
  ErrorCode, NumEntries, I: integer ;
  DispName, DispTime: string ;
begin
  if not Assigned( List ) then EXIT;
  List.Clear;
  ErrorCode := IpHlpUDPTable (ConnRows, AF_UNSPEC) ; // IPv4 and IPv6
  if ErrorCode <> NO_ERROR then
  begin
     List.Add (SysErrorMessage (ErrorCode));
     exit ;
  end;
  NumEntries := Length (ConnRows) ;
  if NumEntries = 0 then
  begin
      List.Add ('No UDP Connections') ;
      exit ;
  end ;
  for I := 0 to Pred (NumEntries) do
  begin
      with ConnRows [I] do
      begin
        // build display for user
          if ShowExePath then       // 15 Jan 2009
              DispName := ProcName
          else
              DispName := ExtractFileName (ProcName) ;
          DispTime := '' ;
          if CreateDT > 0 then DispTime := DateTimeToStr (CreateDT) ;
          List.Add (Format( '%-30s : %-7s| %8d|%-64s|%-20s',
                  [LocalAddr, Port2Svc (LocalPort),
                  ProcessId, DispName, DispTime] ) );
      end ;
  end ;
end ;

function IpHlpConvUniRow (Row: TMibUnicastIpAddressRow): TIpAddrInfo ;
begin
    with Result, Row do
    begin
        IpAddress := SocketAddr2Str (Address) ;
        if Address.si_family = AF_INET then
            IpMask := CreateMask (OnLinkPrefixLength)
        else
            IpMask := '/' + IntToStr(OnLinkPrefixLength);
        IpType := IpTypeUnicast ;
        TypeStr := IpPrefixOrigins [Ord(PrefixOrigin)] ;
        if Ord(PrefixOrigin) <> Ord(SuffixOrigin) then
                TypeStr := TypeStr + ', ' + IpSuffixOrigins [Ord(SuffixOrigin)] ;
        SockAddr := Address ;
        IFLuid := InterfaceLuid ;
        IFIndex := InterfaceIndex ;
        PrefixOrig := PrefixOrigin ;
        SuffixOrig := SuffixOrigin ;
        ValidSecs := ValidLifetime ;
        DupliState := DadState ;
        IpScopeId := ScopeId ;
        CreationDT := FileTimeToDateTime (CreationTimeStamp) ;
    end;
end;

//------------------------------------------------------------------------------
{ returns IPv4 and IPv6 addresses for all or some adaptors }

function IpHlpIpAddrTable(var IpAddrInfos: TIpAddrInfos; Family: TAddressFamily = AF_INET;
           AllIps: Boolean = True; Names: Boolean = True; AdptIdx: TNetIfIndex = 0): integer ;
var
    I, J, NumEntries, CurEntry, TableLen : integer;
    TableSize: DWORD;
    pIPAddrTable: PTMibIPAddrTable;
    PUuicastTable: PMibUnicastIpAddressTable;
    PMulticastTable: PMibMulticastIpAddressTable;
    PAnycastTable: PMibAnycastIpAddressTable;
    IfRows2: TIfRows2 ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    CurEntry := 0 ;
    pIPAddrTable := nil ;
    PUuicastTable := Nil ;
    PMulticastTable := nil ;
    PAnycastTable := Nil ;
    SetLength (IpAddrInfos, 0) ;
    TableLen := 0 ;

    try
        if (Win32MajorVersion < 6) OR (NOT Assigned (GetUnicastIpAddressTable)) then  // only Vista and later
        begin

          // first call: get table length
            TableSize := 0 ;
            result := GetIpAddrTable(Nil, @TableSize, true );  // Angus
            if result <> ERROR_INSUFFICIENT_BUFFER then EXIT;
            GetMem (pIPAddrTable, TableSize );
          // get table
            result := GetIpAddrTable (pIPAddrTable, @TableSize, true );
            if result = NO_ERROR then
            begin
                NumEntries := pIPAddrTable^.dwNumEntries;
                TableLen := TableLen + NumEntries ;
                SetLength (IpAddrInfos, TableLen) ;
                if NumEntries > 0 then
                begin
                    for I := 0 to Pred (NumEntries) do
                    begin
                        if (AdptIdx <> 0) and (AdptIdx <> TNetIfIndex (pIPAddrTable^.Table [I].dwIndex)) then continue ;
                        with IpAddrInfos [CurEntry], pIPAddrTable^.Table [I] do
                        begin
                            IpAddress := IPAddr2Str (dwAddr) ;
                            IpMask := IPAddr2Str (dwMask) ;
                            IpType := IpTypeUnicast ;
                            TypeStr := GetIpAddrType (wtype) ;
                            SockAddr.si_family := AF_INET ;
                            SockAddr.Ipv4.sin_addr := in_addr (dwAddr) ;
                            IFLuid.Value := 0 ;
                            IFIndex := dwIndex ;
                            PrefixOrig := IpPrefixOriginManual ;
                            SuffixOrig := IpSuffixOriginManual ;
                            ValidSecs := 0 ;
                            DupliState := IpDadStateInvalid ;
                            IpScopeId.Value := 0 ;
                            CreationDT := 0 ;
                        end;
                        inc (CurEntry) ;
                    end;
                end;
            end;
            SetLength (IpAddrInfos, CurEntry) ;
            exit ;  // Dec 2015 nothing more on XP
        end
        else
        begin
          // get Unicast table
            result := GetUnicastIpAddressTable (Family, PUuicastTable);
            if result = NO_ERROR then
            begin
                NumEntries := PUuicastTable^.NumEntries;
                TableLen := TableLen + NumEntries ;
                SetLength (IpAddrInfos, TableLen) ;
                if NumEntries > 0 then
                begin
                    for I := 0 to Pred (NumEntries) do
                    begin
                        if (AdptIdx <> 0) and (AdptIdx <> PUuicastTable^.Table [I].InterfaceIndex) then continue ;
                        IpAddrInfos [CurEntry] := IpHlpConvUniRow (PUuicastTable^.Table [I]) ;
                        inc (CurEntry) ;
                    end;
                end;
            end;

          // see if getting multicast and broadcast addresses
            if AllIps then
            begin

          // get Multicast table
                result := GetMulticastIpAddressTable (Family, PMulticastTable);
                if result = NO_ERROR then
                begin
                    NumEntries := PMulticastTable^.NumEntries;
                    TableLen := TableLen + NumEntries ;
                    SetLength (IpAddrInfos, TableLen) ;
                    if NumEntries > 0 then
                    begin
                        for I := 0 to Pred (NumEntries) do
                        begin
                            if (AdptIdx <> 0) and (AdptIdx <> PMulticastTable^.Table [I].InterfaceIndex) then continue ;
                            with IpAddrInfos [CurEntry], PMulticastTable^.Table [I] do
                            begin
                                IpAddress := SocketAddr2Str (Address) ;
                                IpType := IpTypeMulticast ;
                                TypeStr := 'Multicast' ;
                                SockAddr := Address ;
                                IFLuid := InterfaceLuid ;
                                IFIndex := InterfaceIndex ;
                                IpScopeId := ScopeId ;
                            end;
                            inc (CurEntry) ;
                        end;
                    end;
                end;

              // get Anycast table
                result := GetAnycastIpAddressTable (Family, PAnycastTable);
                if result = NO_ERROR then
                begin
                    NumEntries := PAnycastTable^.NumEntries;
                    TableLen := TableLen + NumEntries ;
                    SetLength (IpAddrInfos, TableLen) ;
                    if NumEntries > 0 then
                    begin
                        for I := 0 to Pred (NumEntries) do
                        begin
                            if (AdptIdx <> 0) and (AdptIdx <> PAnycastTable^.Table [I].InterfaceIndex) then continue ;
                            with IpAddrInfos [CurEntry], PAnycastTable^.Table [I] do
                            begin
                                IpAddress := SocketAddr2Str (Address) ;
                                IpType := IpTypeAnycast ;
                                TypeStr := 'Anycast' ;
                                SockAddr := Address ;
                                IFLuid := InterfaceLuid ;
                                IFIndex := InterfaceIndex ;
                                IpScopeId := ScopeId ;
                            end;
                            inc (CurEntry) ;
                        end;
                    end;
                end;
            end;
        end;
    finally
        FreeMem (pIPAddrTable);
        if Assigned (PUuicastTable) then FreeMibTable (PUuicastTable);
        if Assigned (PMulticastTable) then FreeMibTable (PMulticastTable);
        if Assigned (PAnycastTable) then FreeMibTable (PAnycastTable);
    end;
    SetLength (IpAddrInfos, CurEntry) ;

// find adaptor names from interface table
    if NOT Names then Exit ;
    if (Win32MajorVersion < 6) then exit ; 
    if IpHlpIfTable2 (NumEntries, IfRows2) <> 0 then Exit ;
    if NumEntries = 0 then Exit ;
    for I := 0 to Pred (CurEntry) do
    begin
        for J := 0 to Pred (NumEntries) do
        begin
            if IpAddrInfos [I].IFIndex = IfRows2 [J].Mib.InterfaceIndex then
            begin
                with IpAddrInfos [I] do
                begin
                    InterfaceName := IfRows2 [J].InterfaceName ;
                    Description := IfRows2 [J].Description ;
                    FriendlyName := IfRows2 [J].FriendlyName ;
                    Break ;
                end;
            end;
        end;
    end;
    SetLength(IfRows2, 0);
end;

//------------------------------------------------------------------------------
{ returns addresses for all adaptors }
procedure Get_IPAddrTable( List: TStrings );
var
    IpAddrInfos: TIpAddrInfos ;
    ErrorCode, NumEntries, I: integer;
begin
    if not Assigned( List ) then EXIT;
    List.Clear;
    ErrorCode := IpHlpIpAddrTable (IpAddrInfos, AF_UNSPEC) ; // IPv4 and IPv6
    if ErrorCode <> NO_ERROR then
    begin
        List.Add (SysErrorMessage (ErrorCode));
        exit ;
    end;
    NumEntries := Length (IpAddrInfos) ;
    if NumEntries = 0 then
    begin
        List.Add ('No IP Addresses') ;
        exit ;
    end ;

    List.Add( Format( '%-30s|%-15s|%-22s|%-14s|%-40s|%-30s',
              ['IP Address', 'IP Mask', 'Type', 'Interface',
               'Description', 'Friendly Name'] ) );
     List.Add('');
    for I:= 0 to Pred (NumEntries) do
    begin
        with IpAddrInfos [I] do
          List.Add( Format( '%-30s|%-15s|%-22s|%-14s|%-40s|%-30s',
              [IpAddress, IpMask, TypeStr, InterfaceName,
                Copy (Description, 1, 40), Copy (FriendlyName, 1, 30)] ) );
    end ;
end;

//-----------------------------------------------------------------------------
{ gets entries in routing table; equivalent to "Route Print" }
procedure Get_IPForwardTable( List: TStrings );
var
  IPForwRow     : TMibIPForwardRow;
  TableSize     : DWORD;
  ErrorCode     : DWORD;
  i             : integer;
  pBuf          : PAnsiChar;
  NumEntries    : DWORD;
begin
  if NOT LoadIpHlp then exit ;
  if not Assigned( List ) then EXIT;
  List.Clear;
  TableSize := 0;

  // first call: get table length
  NumEntries := 0 ;
  ErrorCode := GetIpForwardTable(Nil, @TableSize, true);
  if Errorcode <> ERROR_INSUFFICIENT_BUFFER then
    EXIT;

  // get table
  GetMem( pBuf, TableSize );
  ErrorCode := GetIpForwardTable( PTMibIPForwardTable( pBuf ), @TableSize, true);
  if ErrorCode = NO_ERROR then
  begin
    NumEntries := PTMibIPForwardTable( pBuf )^.dwNumEntries;
    if NumEntries > 0 then
    begin
      inc( pBuf, SizeOf( DWORD ) );
      for i := 1 to NumEntries do
      begin
        IPForwRow := PTMibIPForwardRow( pBuf )^;
        with IPForwRow do
        begin
          if (dwForwardType > 4) then dwForwardType := 0 ;     // Angus, allow for bad value
          if (dwForwardProto > 18) then dwForwardProto := 0 ;  // Angus, allow for bad value
          List.Add( Format(
            '%-17s|%-17s|%-17s|%-9.8x|%-9s|   %6.5d|    %-8s|        %3.2d',
            [ IPAddr2Str( dwForwardDest ),
              IPAddr2Str( dwForwardMask ),
              IPAddr2Str( dwForwardNextHop ),
              dwForwardIFIndex,
              IPForwTypes[dwForwardType],
              dwForwardNextHopAS,
              IPForwProtos[dwForwardProto],
              dwForwardMetric1
              ] ) );
        end ;
        inc( pBuf, SizeOf( TMibIPForwardRow ) );
      end;
    end
    else
      List.Add( 'no entries.' );
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );
  dec( pBuf, SizeOf( DWORD ) + NumEntries * SizeOf( TMibIPForwardRow ) );
  FreeMem( pBuf );
end;

//------------------------------------------------------------------------------
procedure Get_IPStatistics( List: TStrings );
var
  IPStats       : TMibIPStats;
  ErrorCode     : integer;
begin
  if not Assigned( List ) then EXIT;
  if NOT LoadIpHlp then exit ;
  ErrorCode := GetIPStatistics( @IPStats );
  if ErrorCode = NO_ERROR then
  begin
    List.Clear;
    with IPStats do
    begin
      if dwForwarding = 1 then
        List.add( 'Forwarding Enabled      : ' + 'Yes' )
      else
        List.add( 'Forwarding Enabled      : ' + 'No' );
      List.add( 'Default TTL             : ' + inttostr( dwDefaultTTL ) );
      List.add( 'Datagrams Received      : ' + inttostr( dwInReceives ) );
      List.add( 'Header Errors     (In)  : ' + inttostr( dwInHdrErrors ) );
      List.add( 'Address Errors    (In)  : ' + inttostr( dwInAddrErrors ) );
      List.add( 'Datagrams Forwarded     : ' + inttostr( dwForwDatagrams ) );   // Angus
      List.add( 'Unknown Protocols (In)  : ' + inttostr( dwInUnknownProtos ) );
      List.add( 'Datagrams Discarded     : ' + inttostr( dwInDiscards ) );
      List.add( 'Datagrams Delivered     : ' + inttostr( dwInDelivers ) );
      List.add( 'Requests Out            : ' + inttostr( dwOutRequests ) );
      List.add( 'Routings Discarded      : ' + inttostr( dwRoutingDiscards ) );
      List.add( 'No Routes          (Out): ' + inttostr( dwOutNoRoutes ) );
      List.add( 'Reassemble TimeOuts     : ' + inttostr( dwReasmTimeOut ) );
      List.add( 'Reassemble Requests     : ' + inttostr( dwReasmReqds ) );
      List.add( 'Succesfull Reassemblies : ' + inttostr( dwReasmOKs ) );
      List.add( 'Failed Reassemblies     : ' + inttostr( dwReasmFails ) );
      List.add( 'Succesful Fragmentations: ' + inttostr( dwFragOKs ) );
      List.add( 'Failed Fragmentations   : ' + inttostr( dwFragFails ) );
      List.add( 'Datagrams Fragmented    : ' + inttostr( dwFRagCreates ) );
      List.add( 'Number of Interfaces    : ' + inttostr( dwNumIf ) );
      List.add( 'Number of IP-addresses  : ' + inttostr( dwNumAddr ) );
      List.add( 'Routes in RoutingTable  : ' + inttostr( dwNumRoutes ) );
    end;
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );
end;

function IpHlpIPStatistics (var IPStats: TMibIPStats): integer ;      // Angus
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetIPStatistics( @IPStats );
end ;

//------------------------------------------------------------------------------
procedure Get_UdpStatistics( List: TStrings );
var
  UdpStats      : TMibUDPStats;
  ErrorCode     : integer;
begin
  if NOT LoadIpHlp then exit ;
  if not Assigned( List ) then EXIT;
  ErrorCode := GetUDPStatistics( @UdpStats );
  if ErrorCode = NO_ERROR then
  begin
    List.Clear;
    with UDPStats do
    begin
      List.add( 'Datagrams (In)    : ' + inttostr( dwInDatagrams ) );
      List.add( 'Datagrams (Out)   : ' + inttostr( dwOutDatagrams ) );
      List.add( 'No Ports          : ' + inttostr( dwNoPorts ) );
      List.add( 'Errors    (In)    : ' + inttostr( dwInErrors ) );
      List.add( 'UDP Listen Ports  : ' + inttostr( dwNumAddrs ) );
    end;
  end
  else
    List.Add( SysErrorMessage( ErrorCode ) );
end;

function IpHlpUdpStatistics (UdpStats: TMibUDPStats): integer ;          // Angus
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    result := GetUDPStatistics (@UdpStats) ;
end ;

//------------------------------------------------------------------------------
procedure Get_ICMPStats( ICMPIn, ICMPOut: TStrings );
var
  ErrorCode     : DWORD;
  ICMPStats     : PTMibICMPInfo;
begin
  if NOT LoadIpHlp then exit ;
  if ( ICMPIn = nil ) or ( ICMPOut = nil ) then EXIT;
  ICMPIn.Clear;
  ICMPOut.Clear;
  New( ICMPStats );
  ErrorCode := GetICMPStatistics( ICMPStats );
  if ErrorCode = NO_ERROR then
  begin
    with ICMPStats.InStats do
    begin
      ICMPIn.Add( 'Messages received    : ' + IntToStr( dwMsgs ) );
      ICMPIn.Add( 'Errors               : ' + IntToStr( dwErrors ) );
      ICMPIn.Add( 'Dest. Unreachable    : ' + IntToStr( dwDestUnreachs ) );
      ICMPIn.Add( 'Time Exceeded        : ' + IntToStr( dwTimeEcxcds ) );
      ICMPIn.Add( 'Param. Problems      : ' + IntToStr( dwParmProbs ) );
      ICMPIn.Add( 'Source Quench        : ' + IntToStr( dwSrcQuenchs ) );
      ICMPIn.Add( 'Redirects            : ' + IntToStr( dwRedirects ) );
      ICMPIn.Add( 'Echo Requests        : ' + IntToStr( dwEchos ) );
      ICMPIn.Add( 'Echo Replies         : ' + IntToStr( dwEchoReps ) );
      ICMPIn.Add( 'Timestamp Requests   : ' + IntToStr( dwTimeStamps ) );
      ICMPIn.Add( 'Timestamp Replies    : ' + IntToStr( dwTimeStampReps ) );

      ICMPIn.Add( 'Addr. Masks Requests : ' + IntToStr( dwAddrMasks ) );
      ICMPIn.Add( 'Addr. Mask Replies   : ' + IntToStr( dwAddrReps ) );
    end;
     //
//    with ICMPStats^.OutStats do
    with ICMPStats.OutStats do
    begin
      ICMPOut.Add( 'Messages sent        : ' + IntToStr( dwMsgs ) );
      ICMPOut.Add( 'Errors               : ' + IntToStr( dwErrors ) );
      ICMPOut.Add( 'Dest. Unreachable    : ' + IntToStr( dwDestUnreachs ) );
      ICMPOut.Add( 'Time Exceeded        : ' + IntToStr( dwTimeEcxcds ) );
      ICMPOut.Add( 'Param. Problems      : ' + IntToStr( dwParmProbs ) );
      ICMPOut.Add( 'Source Quench        : ' + IntToStr( dwSrcQuenchs ) );
      ICMPOut.Add( 'Redirects            : ' + IntToStr( dwRedirects ) );
      ICMPOut.Add( 'Echo Requests        : ' + IntToStr( dwEchos ) );
      ICMPOut.Add( 'Echo Replies         : ' + IntToStr( dwEchoReps ) );
      ICMPOut.Add( 'Timestamp Requests   : ' + IntToStr( dwTimeStamps ) );
      ICMPOut.Add( 'Timestamp Replies    : ' + IntToStr( dwTimeStampReps ) );
      ICMPOut.Add( 'Addr. Masks Requests : ' + IntToStr( dwAddrMasks ) );
      ICMPOut.Add( 'Addr. Mask Replies   : ' + IntToStr( dwAddrReps ) );
    end;
  end
  else
    IcmpIn.Add( SysErrorMessage( ErrorCode ) );
  Dispose( ICMPStats );
end;

//------------------------------------------------------------------------------
procedure Get_RecentDestIPs( List: TStrings );
begin
  if Assigned( List ) then
    List.Assign( RecentIPs )
end;

//------------------------------------------------------------------------------

procedure UnicastIpAddressChangeCallback (CallerContext: Pointer; Row: PMibUnicastIpAddressRow;
                                                NotificationType: TMibNoticationType); stdcall;
var
    IpAddrInfo: TIpAddrInfo;
begin
    if NOT Assigned (fIpChangesEvent) then Exit;
    if NotificationType <> MibInitialNotification then
    begin
        if NOT Assigned (Row) then Exit;
        IpAddrInfo := IpHlpConvUniRow (Row^);
    end;
    fIpChangesEvent (IpAddrInfo, CallerContext, NotificationType);
end;

//------------------------------------------------------------------------------

function IpChangesStart (Family: TAddressFamily; CallerContext: Pointer): Integer ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NOT LoadIpHlp then exit ;
    if NOT Assigned (NotifyUnicastIpAddressChange) then exit;
    IpChangesStop;
    Result := NotifyUnicastIpAddressChange (Family, @UnicastIpAddressChangeCallback,
                                                  CallerContext, True, NotificationHandle);
end ;

//------------------------------------------------------------------------------

function IpChangesStop: Integer ;
begin
    result := ERROR_NOT_SUPPORTED ;
    if NotificationHandle = 0 then Exit;
    if NOT LoadIpHlp then exit ;
    if NOT Assigned (CancelMibChangeNotify2) then exit;
    Result := CancelMibChangeNotify2 (NotificationHandle);
    NotificationHandle := 0;
end ;

//------------------------------------------------------------------------------

initialization

  RecentIPs := TStringList.Create;

finalization
  IpChangesStop;
  RecentIPs.Free;

end.


