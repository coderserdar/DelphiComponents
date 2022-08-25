unit MagentaPackhdrs;

{ Magenta Systems Internet Packet Monitoring Components

Magenta Systems raw socket packet headers and helpers.
Updated by Angus Robertson, Magenta Systems Ltd, England, v1.3 9th August 2010
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Some of the TCP/IP headers are taken from 'Hands-On TCP/IP Programming' by Alfred
Mirzagotov in The Delphi Magazine January 2004.

8 Aug 2008 - 1.2 - updated to support ICS V6 and V7, and Delphi 2009

}

interface

uses
  Windows, Messages, Classes, SysUtils, Winsock,
  Magsubs1, MagClasses,
  OverbyteIcsWSocket ;

const
  sTrafficMask = '%-20s %-50s %-12s %5s %7s %5s %7s %8s %8s' ;
//               pc09.magenta         ermintrude.digitalspy.co.uk                        www-http     1.51K    [10] 6.94K    [10] 19:54:17 19:54:27
  sTrafficHdr = 'Local IP             Remote IP                                          Service      Sent [packet] Recv [packet] First    Last' ;
  sServiceMask = '%-12s %5s %7s %5s %7s %5s' ;
//               www-http     1.51K    [10] 6.94K    [10]    22
  sServiceHdr = 'Service      Sent [packet] Recv [packet] Hosts' ;
  MaxDnsLookupAttempts = 6 ;  // total for both addresses
  InitialTrafficSize = 100 ;

type

    TMacAddr = array [0..5] of byte ;  // a MAC address

// record used to return packet to application for both raw sockets and winpcap

    TPacketInfo = record
        PacketLen: integer ;   // total length of packet including network interface layer
        EtherProto: word ;     // ethernet protocol
        EtherSrc: TMacAddr ;   // ethernet MAC addresses
        EtherDest: TMacAddr ;
        AddrSrc: TInAddr ;     // IP addresses are 32-bit binary (we may not need ASCII)
        AddrDest: TInAddr ;
        PortSrc: integer ;     // transport layer ports
        PortDest: integer ;
        ProtoType: byte ;      // transport layer protocol
        TcpFlags: word ;       // TCP/IP packet type flags
        SendFlag: boolean ;    // true if packet being sent from local IP
        IcmpType: byte ;       // ICMP packet type
        DataLen: integer ;     // length of data (less headers)
        DataBuf: AnsiString ;  // packet data (may be blank even if datalen<>0)
        PacketDT: TDateTime ;  // when packet was captured
  end ;

  TPacketEvent = procedure (Sender: TObject; PacketInfo: TPacketInfo) of object;

// record used for maintaining traffic statistics

    TTrafficInfo = packed record  // first four elements are used for sorting, keep together and packed
        AddrLoc: TInAddr ;     // IP addresses are 32-bit binary
        AddrRem: TInAddr ;
        ServPort: word ;    // service port
        PackType: word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc  - 12 bytes to here
        HostLoc: string ;      // host domains for IP addresses, if available
        HostRem: string ;
        ServName: string ;     // looked up
        BytesSent: int64 ;     // traffic
        BytesRecv: int64 ;
        PacksSent: integer ;
        PacksRecv: integer ;
        LookupAttempts: integer ; // how many host name lookup attempts
        FirstDT: TDateTime ;   // when this traffic started
        LastDT: TDateTime ;    // last traffic update
  end ;
  PTrafficInfo = ^TTrafficInfo ;

  TServiceInfo = packed record  // first two elements are used for sorting, keep together and packed
        ServPort: word ;    // service port
        PackType: word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc  - 4 bytes to here
        ServName: string ;     // looked up
        TotalHosts: integer;   // how many different hosts for this service
        BytesSent: int64 ;     // traffic
        BytesRecv: int64 ;
        PacksSent: integer ;
        PacksRecv: integer ;
  end ;
  PServiceInfo = ^TServiceInfo ;

const
  TrafficIPCompLen = 12 ;
  ServiceCompLen = 4 ;

type
  THdrEthernet = packed record   // Ethernet frame header - Network Interface Layer
    dmac: TMacAddr;
    smac: TMacAddr;
    protocol: WORD;
  end;
  PHdrEthernet = ^THdrEthernet ;

const     //rfc1340 ethernet protocols
  PROTO_PUP     =	$0200;
  PROTO_XNS     =	$0600;
  PROTO_IP      =	$0800;
  PROTO_ARP     =	$0806;
  PROTO_REVARP  =	$0835;
  PROTO_SCA     =	$6007;
  PROTO_ATALK   =	$809B;
  PROTO_AARP    =	$80F3;
  PROTO_IPX     =	$8137;
  PROTO_NOVELL  =	$8138;
  PROTO_SNMP    =	$814C;
  PROTO_IPV6    =	$86DD;
  PROTO_XIMETA  =	$88AD;
  PROTO_LOOP    =	$900D;

  OFFSET_IP =	14;   // length of ethernet frame header

  TCP_FLAG_FIN =	$01;   // TCP flags
  TCP_FLAG_SYN =	$02;
  TCP_FLAG_RST =	$04;
  TCP_FLAG_PSH =	$08;
  TCP_FLAG_ACK =	$10;
  TCP_FLAG_URG =	$20;
  TCP_FLAG_ECH =	$40;
  TCP_FLAG_CWR =	$80;

type

  THdrIP = packed record   // IP header (RFC 791) - Internet Layer
    ihl_ver : BYTE;        // Combined field:
                           //   ihl:4 - IP header length divided by 4
                           //   version:4 - IP version
    tos     : BYTE;        // IP type-of-service field
    tot_len : WORD;        // total length
    id      : WORD;        // unique ID
    frag_off: WORD;        // Fragment Offset + fragmentation flags (3 bits)
    ttl     : BYTE;        // time to live
    protocol: BYTE;        // protocol type
    check   : WORD;        // IP header checksum
    saddr   : TInAddr;     // source IP
    daddr   : TInAddr;     // destination IP
   {The options start here...}
  end;
  PHdrIP = ^THdrIP;

  (* Most of IP header is self-explanatory, but here are some
     extra details for the curious (more in RFC 791):

    -ih.ihl is header length in bytes divided by 4
     Internet Header Length is the length of the internet
     header in 32 bit words, and thus points to the beginning
     of the data.  Note that the minimum value for a correct
     header is 5.

    -ih.tos - IP type-of-service field provides an indication of the
     quality of service desired. Several networks offer service precedence,
     which somehow treats high precedence traffic as more important than
     other traffic (generally by accepting only traffic above a certain
     precedence at time of high load).

    -ih.id  - An identifying value assigned by the sender to aid in
     assembling the fragments of a datagram.

    -ih.frag_off contains 3 bit fragmentation flags and fragment offset.
     These are used to keep track of the pieces when a datagram has to
     be split up. This can happen when datagrams are forwarded through
     a network for which they are too big. See RFC815 about reassembly.
       Bit 0: reserved, must be zero
       Bit 1: (DF) 0 = May Fragment,  1 = Don't Fragment.
       Bit 2: (MF) 0 = Last Fragment, 1 = More Fragments.
       Bits?: indicates where in the datagram this fragment belongs

    -ih.protocol tells IP at the other end to send the datagram
     to TCP. Although most IP traffic uses TCP, there are other
     protocols that can use IP, so you have to tell IP which
     protocol to send the datagram to.

    -ih.check[sum] allows IP at the other end to verify that the header
     wasn't damaged in transit. Note that TCP and IP have separate
     checksums. IP only needs to be able to verify that the header
     didn't get damaged in transit, or it could send a message to
     the wrong place.
   *)

  THdrTCP = packed record     // TCP header (RFC 793) - Transport Layer
    source : WORD;  // source port
    dest   : WORD;  // destination port
    seq    : DWORD; // sequence number
    ack_seq: DWORD; // next sequence number
    flags  : WORD;  // Combined field:
                    //   res1:4 - reserved, must be 0
                    //   doff:4 - TCP header length divided by 4
                    //   fin:1  - FIN
                    //   syn:1  - SYN
                    //   rst:1  - Reset
                    //   psh:1  - Push
                    //   ack:1  - ACK
                    //   urg:1  - Urgent
                    //   res2:2 - reserved, must be 0
    window : WORD;  // window size
    check  : WORD;  // checksum, computed later
    urg_ptr: WORD;  // used for async messaging?
  end;
  PHdrTCP = ^THdrTCP;
  (* Details of TCP header can be found in RFC 793

    -th.seq - the sequence number of the first data octet in this segment
     (except when SYN is present). If SYN is present the sequence number
     is the initial sequence number (ISN) and the first data octet is ISN+1.

    -th.doff - data offset - the number of 32 bit words in the TCP Header.
     This indicates where the data begins. The TCP header (even one
     including options) is an integral number of 32 bits long.

    -th.ack_seq is used when ACK flag is set. If ACK is set this field
     contains the value of the next sequence number the sender of the
     segment is expecting to receive. Once a connection is established
     this is always sent. This simply means that receiver got all the
     octets up to the specific sequence number.
     For example, sending a packet with an acknowledgement of 1500
     indicates that you have received all the data up to octet
     number 1500. If the sender doesn't get an acknowledgement
     within a reasonable amount of time, it sends the data again.

    -th.window is used to control how much data can be in transit
     at any one time. It is not practical to wait for each datagram
     to be acknowledged before sending the next one. That would slow
     things down too much. On the other hand, you can't just keep
     sending, or a fast computer might overrun the capacity of a slow
     one to absorb data. Thus each end indicates how much new data
     it is currently prepared to absorb by putting the number of
     octets in its "window" field. As the computer receives data,
     the amount of space left in its window decreases. When it goes
     to zero, the sender has to stop. As the receiver processes
     the data, it increases its window, indicating that it is ready
     to accept more data.
     [ See RFC813 for details and "silly-window-syndrome" ]
     Often the same datagram can be used to acknowledge receipt of
     a set of data and to give permission for additional new data
     (by an updated window).

    -th.urgent field allows one end to tell the other to skip ahead
     in its processing to a particular octet. This is often useful
     for handling asynchronous events, for example when you type
     a control character or other command that interrupts output.
   *)

  THdrUDP = packed record  // UDP header (RFC 768)    - Transport Layer
    src_port: WORD;        // source port
    dst_port: WORD;        // destination port
    length  : WORD;        // length, including this header
    checksum: WORD;        // UDP checksum
  end;
  PHdrUDP = ^THdrUDP;

type
  TTcpFlagType = (ftFIN, ftSYN, ftRST, ftPSH, ftACK, ftURG);


// class used for maintaining traffic statistics
type

  TTrafficClass = class(TComponent)
  protected
    { Protected declarations }
      FTrafficInfo: array of TTrafficInfo ;
      FServiceInfo: array of TServiceInfo ;
      FTrafficList: TFindList ;
      FServiceList: TFindList ;
      FTotTraffic: integer ;
      FTotService: integer ;
      FLookupLoc: integer ;
      FLookupRem: integer ;
      FLookupBusy: boolean ;
      FWSocket: TWSocket ;
      procedure DoneLookup (Sender: TObject; Error: Word);
      procedure NextLookup ;
  public
    { Public declarations }
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear ;
      procedure Add (PacketInfo: TPacketInfo) ;
      procedure LookupHosts ;
      procedure UpdateService ;
      function GetServNameEx (PackType, ServPort: word): string ;
      function GetUnSortTraf (item: integer): PTrafficInfo ;
      function GetSortedTraf (item: integer): PTrafficInfo ;
      function GetFmtTrafStr (item: integer): string ;
      function GetSortedServ (item: integer): PServiceInfo ;
      function GetFmtServStr (item: integer): string ;
      function GetTotals: TServiceInfo ; 
  published
      property TotTraffic: integer          read FTotTraffic ;
      property TotService: integer          read FTotService ;
  end;

var
  PortNameArray: array of string ;   // dynamic array for TCP and UDP port names, indexed by number
  TotalPortNames: integer = -1 ;
  ProtoNameArray: array of string ;  // dynamic array for IP protocol names, indexed by number
  TotalProtoNames: integer = -1 ;
  PortListFileName: string = 'ports.txt' ;
  ProtocolListFileName: string = 'protocols.txt' ;

// get name given a number
function GetEtherProtoName (protocol: word): string ;
function GetIPProtoName(protocol: integer): string ;
function GetServiceName(s_port, d_port: Integer): string;
function GetServName (port: integer): string ;
function GetServiceNameEx(s_port, d_port: Integer): string;
function GetICMPType (x: word): string ;
function GetFlags(flags: word): string ;
procedure LoadPortNameList ;

// these routines manipulate combined fields (set/get nibbles or bits)
procedure SetTHdoff(VAR th: THdrTCP; value: Byte);
function  GetTHdoff(th: THdrTCP): Word;
procedure SetTHflag(VAR th: THdrTCP; flag: TTcpFlagType; on: Boolean);
function  GetTHflag(th: THdrTCP; flag: TTcpFlagType): Boolean;
procedure SetIHver(VAR ih: THdrIP; value: Byte);
function  GetIHver(ih: THdrIP): Byte;
procedure SetIHlen(VAR ih: THdrIP; value: Byte);
function  GetIHlen(ih: THdrIP): Word;

function IPToStr (IPAddr: TInAddr): string ;
function StrToIP (strIP: string): TInAddr ;
function IsIPStr (strIP: string): boolean ;
function IsFmtIPStr (var strIP: string): boolean ;
function Str2IP (strIP: string; var IPAddr: TInAddr): boolean ;
function AscToInt (value: string): Integer;
function MacToStr (MacAddr: TMacAddr): string ;

implementation

type

  TEtherProto = record
    iType: integer ;
    iName: string ;
  end ;

  TIPProto = record
    iType: integer ;
    iName: string ;
  end ;

  TWellKnownSvc = record
    port: integer ;
    svc: string ;
  end ;

var
  // Ethernet Protocol types
  EtherProto: array[1..14] Of TEtherProto = (
    (iType: PROTO_PUP;      iName: 'PUP'),
    (iType: PROTO_XNS;      iName: 'XNS'),
    (iType: PROTO_IP;       iName: 'IP'),
    (iType: PROTO_ARP;      iName: 'ARP'),
    (iType: PROTO_REVARP;   iName: 'RARP'),
    (iType: PROTO_SCA;      iName: 'SCA'),
    (iType: PROTO_ATALK;    iName: 'ATLK'),
    (iType: PROTO_AARP;     iName: 'AARP'),
    (iType: PROTO_IPX;      iName: 'IPX'),
    (iType: PROTO_NOVELL;   iName: 'NOVL'),
    (iType: PROTO_SNMP;     iName: 'SNMP'),
    (iType: PROTO_IPV6;     iName: 'IPV6'),
    (iType: PROTO_XIMETA;   iName: 'XIMT'),
    (iType: PROTO_LOOP;     iName: 'LOOP')
  );

  // IP Protocol types
  IpProto: array[1..6] Of TIPProto = (
    (iType: IPPROTO_IP;   iName: 'IP'),   // dummy 
    (iType: IPPROTO_ICMP; iName: 'ICMP'),
    (iType: IPPROTO_IGMP; iName: 'IGMP'),
    (iType: IPPROTO_TCP;  iName: 'TCP'),
    (iType: IPPROTO_UDP;  iName: 'UDP'),
    (iType: $80;          iName: 'ISO-IP')
  );

  // Well known service ports
  WellKnownSvcs: array[1..46] of TWellKnownSvc = (
    ( port:   0; svc: 'LOOPBACK'),
    ( port:   1; svc: 'TCPMUX'),    { TCP Port Service Multiplexer  }
    ( port:   7; svc: 'ECHO' ),     { Echo                          }
    ( port:   9; svc: 'DISCARD' ),  { Discard                       }
    ( port:  13; svc: 'DAYTIME' ),  { DayTime                       }
    ( port:  17; svc: 'QOTD' ),     { Quote Of The Day              }
    ( port:  19; svc: 'CHARGEN' ),  { Character Generator           }
    ( port:  20; svc: 'FTP_DATA' ), { Ftp                           }
    ( port:  21; svc: 'FTP_CTL' ),  { File Transfer Control Protocol}
    ( port:  22; svc: 'SSH' ),      { SSH Remote Login Protocol     }
    ( port:  23; svc: 'TELNET' ),   { TelNet                        }
    ( port:  25; svc: 'SMTP' ),     { Simple Mail Transfer Protocol }
    ( port:  37; svc: 'TIME' ),
    ( port:  42; svc: 'NAME' ),     { Host Name Server              }
    ( port:  43; svc: 'WHOIS' ),    { WHO IS service                }
    ( port:  53; svc: 'DNS' ),      { Domain Name Service           }
    ( port:  66; svc: 'SQL*NET' ),  { Oracle SQL*NET                }
    ( port:  67; svc: 'BOOTPS' ),   { BOOTP Server                  }
    ( port:  68; svc: 'BOOTPC' ),   { BOOTP Client                  }
    ( port:  69; svc: 'TFTP' ),     { Trivial FTP                   }
    ( port:  70; svc: 'GOPHER' ),   { Gopher                        }
    ( port:  79; svc: 'FINGER' ),   { Finger                        }
    ( port:  80; svc: 'HTTP' ),     { HTTP                          }
    ( port:  88; svc: 'KERBEROS' ), { Kerberos                      }
    ( port:  92; svc: 'NPP' ),      { Network Printing Protocol     }
    ( port:  93; svc: 'DCP' ),      { Device Control Protocol       }
    ( port: 109; svc: 'POP2' ),     { Post Office Protocol Version 2}
    ( port: 110; svc: 'POP3' ),     { Post Office Protocol Version 3}
    ( port: 111; svc: 'SUNRPC' ),   { SUN Remote Procedure Call     }
    ( port: 119; svc: 'NNTP' ),     { Network News Transfer Protocol}
    ( port: 123; svc: 'NTP' ),      { Network Time protocol         }
    ( port: 135; svc: 'LOCSVC' ),   { Location Service              }
    ( port: 137; svc: 'NETBIOS-NAME' ),  { NETBIOS Name service          }
    ( port: 138; svc: 'NETBIOS-DATA' ),  { NETBIOS Datagram Service      }
    ( port: 139; svc: 'NETBIOS-SESS' ),  { NETBIOS Session Service       }
    ( port: 161; svc: 'SNMP' ),     { Simple Netw. Mgmt Protocol    }
    ( port: 162; svc: 'SNMPTRAP' ), { SNMP TRAP                     }
    ( port: 220; svc: 'IMAP3' ),    { Interactive Mail Access Protocol v3 }
    ( port: 443; svc: 'HTTPS' ),    { HTTPS                         }
    ( port: 445; svc: 'MS-DS-SMB'), { Microsoft Directory Services - SAMBA }
    ( port: 514; svc: 'SYSLOG' ),   { UDP Syslog                    }
    ( port: 520; svc: 'ROUTER' ),   { UDP Router                    }
    ( port:1433; svc: 'MSSQLSRV' ), { MS SQL Server                 }
    ( port:1434; svc: 'MSSQLMON' ), { MS SQL Monitor                }
    ( port:3306; svc: 'MYSQL' ),    { MySQL                         }
    ( port:5900; svc: 'VNC' )       { VNC - similar to PC Anywhere  }
  );

function GetEtherProtoName (protocol: word): string ;
var
    I: integer;
begin
    result := IntToHex (protocol, 4) ;
    for I := 1 To SizeOf (EtherProto) div SizeOf (TEtherProto) do
    begin
        if protocol = EtherProto [I].itype then result := EtherProto [I].iName ;
    end ;
end;

function GetIPProtoName (protocol: integer): string ;
var
    I: integer;
begin
    result := IntToStr (protocol) ;
    for I := 1 To SizeOf (IPPROTO) div SizeOf (TIPProto) do
    begin
        if protocol = IPPROTO [I].itype then result := IPPROTO [I].iName ;
    end ;
end;

function GetServiceName (s_port, d_port: integer): string ;
var
    I: integer;
begin
    result := '';
    for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
    begin
        if (s_port = WellKnownSvcs [I].port) OR (d_port = WellKnownSvcs [I].port) then
        begin
            result := WellKnownSvcs[I].svc;
            exit ;
        end;
    end ;
    if (result = '') and (s_port < 1024) then result := '<' + IntToStr (s_port) + '>' ;
    if (result = '') and (d_port < 1024) then result := '<' + IntToStr (d_port) + '>' ;
end ;

function  GetICMPType(x: word): string ;
begin
    result := 'UNKNOWN';
    case x of
     0: Result := 'ECHO_REPLY'; // Echo Reply
     3: Result := 'DEST-UNREA'; // Destination Unreachable
     4: Result := 'SRC_Q';  // Source Quench
     5: Result := 'REDIR';  // Redirect
     8: Result := 'ECHO';   // Echo
    11: Result := 'TTLX';   // Time Exceeded
    12: Result := 'BADPAR'; // Parameter Problem
    13: Result := 'TIME';   // Timestamp
    14: Result := 'TIME_REPLY'; // Timestamp Reply
    15: Result := 'INFO';   // Information Request
    16: Result := 'INFO_REPLY'; // Information Reply
   end ;
end ;

// load well know port list from file ports.txt, which is copied from RFC 1700 with
// superflous lines removed or prefixed with #
// Note: currently using UDP port where TCP is different, should really have two arrays

procedure LoadPortNameList ;
var
  PortInfo: TStringList ;
  line, port: string ;
  I, J, K, L, M: integer ;
begin
    TotalPortNames := 0 ;
    if FileExists (PortListFileName) then
    begin
        TotalPortNames := 10000 ;
        SetLength (PortNameArray, TotalPortNames) ;
	  	PortInfo := TStringList.Create ;
		try
            try
               	PortInfo.LoadFromFile (PortListFileName) ;
    			I := PortInfo.Count ;
            except
                I := 0 ;
            end ;
            if I <> 0 then
            begin
            	for J := 0 to Pred (I)  do
                begin
                // sample line - ignore / onwards
                // echo              7/tcp    Echo
                   	line := PortInfo [J] ;
                    if Length (line) < 5 then continue ;
                    if line [1] = '#' then continue ;
                    K := Pos (' ', line) ;
                    M := Pos ('/', line) ;
                    if (K < 2) or (M < K) then continue ;
                    port := Copy (line, K, M - K) ;
                    L := AscToInt (Trim (port)) ;
                    if (L = 0) then continue ;
                    if L >= TotalPortNames then continue ;  // ignore high ports
                  //if PortNameArray [L] = '' then
                    PortNameArray [L] := Copy (line, 1, Pred (K)) ;
                end ;
            end
            else
                TotalPortNames := 0 ;
        finally
			  PortInfo.Destroy ;
        end ;
	end ;
end ;

function GetServName (port: integer): string ;
var
    I: integer;
begin
    result := '' ;
    if TotalPortNames < 0 then LoadPortNameList ;  // try and load list
    if (port > 0) and (port < TotalPortNames) then result := PortNameArray [port] ;
    if result = '' then  // nothing in list, try hard coded ports
    begin
        for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
        begin
            if (port = WellKnownSvcs [I].port) then
            begin
                result := WellKnownSvcs[I].svc;
                exit ;
            end;
        end ;
    end ;
    if (result = '') then result := '<' + IntToStr (port) + '>' ;
end ;

function GetServiceNameEx (s_port, d_port: integer): string ;
var
    I: integer;
    s_name, d_name: string ;
begin
    result := '';
    s_name := '' ;
    d_name := '';
    if TotalPortNames < 0 then LoadPortNameList ;  // try and load list
    if (s_port > 0) and (s_port < TotalPortNames) then s_name := PortNameArray [s_port] ;
    if (d_port > 0) and (d_port < TotalPortNames) then d_name := PortNameArray [d_port] ;
    if d_name <> '' then
        result := d_name
    else
        result := s_name ;
    if result = '' then  // nothing in list, try hard coded ports
    begin
        for I := 1 to SizeOf (WellKnownSvcs) div SizeOf (TWellKnownSvc) do
        begin
            if (s_port = WellKnownSvcs [I].port) OR (d_port = WellKnownSvcs [I].port) then
            begin
                result := WellKnownSvcs[I].svc;
                exit ;
            end;
        end ;
    end ;
    if (result = '') and (s_port < 1024) then result := '<' + IntToStr (s_port) + '>' ;
    if (result = '') then result := '<' + IntToStr (d_port) + '>' ;
end ;

(* IP header record contains "ihl_ver" which is used
   to store two parameters: IP header length and IP version.
   IP version is stored in the high nibble of "ihl_ver"
   (it occupies 4 bits). IP header length is stored in the
   low nibble of "ihl_ver" (also uses 4 bits).
   IP header length is expressed in 32 bit words
   (4 8-bit bytes), therefore we divide or multiply
   the low nibble by 4 depending on the function.
*)

function GetIHlen(ih: THdrIP): Word;  // IP header length
begin
  // multiply the low nibble by 4
  // and return the length in bytes
  Result := (ih.ihl_ver AND $0F) SHL 2
end;

procedure SetIHlen(VAR ih: THdrIP; value: Byte);
begin
  // divide the value by 4 and store it in low nibble
  value := value SHR 2;
  ih.ihl_ver := value OR (ih.ihl_ver AND $F0)
end;

function GetIHver(ih: THdrIP): Byte;  // IP version
begin
  // get the high nibble
  Result := ih.ihl_ver SHR 4
end;

procedure SetIHver(VAR ih: THdrIP; value: Byte);
begin
  // set the high nibble
  ih.ihl_ver := (value SHL 4) OR (ih.ihl_ver AND $0F)
end;

(* TCP header record contains "flags" which is used
   to store several parameters:
     Least Significant Bit
       res1:4 - reserved, must be 0
       doff:4 - TCP header length divided by 4
       fin:1  - FIN
       syn:1  - SYN
       rst:1  - Reset
       psh:1  - Push
       ack:1  - ACK
       urg:1  - Urgent
       res2:2 - reserved, must be 0
     MSB
*)

CONST flagMask: Array[ftFIN..ftURG] of Integer = ($100, $200, $400, $800, $1000, $2000);

function GetTHflag(th: THdrTCP; flag: TTcpFlagType): Boolean;
begin
  Result := Boolean(th.flags AND flagMask[flag])
end;

procedure SetTHflag(VAR th: THdrTCP; flag: TTcpFlagType; on: Boolean);
begin
  if on then
    th.flags := th.flags OR flagMask[flag]
  else
    th.flags := th.flags AND NOT flagMask[flag]
end;

function GetTHdoff(th: THdrTCP): Word;
begin
  // doff (data offset) stored in 32 bit words,
  // multiply the value by 4 to get byte offset
  Result := (($00F0 AND th.flags) SHR 4) SHL 2;
end;

procedure SetTHdoff(VAR th: THdrTCP; value: Byte);
VAR x: Integer;
begin
  x := value SHR 2; // divide the value by 4
  th.flags := (x SHL 4) OR (th.flags AND $FF0F)
end;

function GetFlags(flags: word): string ;
begin
    result := '' ;
    if (flags AND TCP_FLAG_FIN) = TCP_FLAG_FIN then result := result + 'FIN ' ;
    if (flags AND TCP_FLAG_SYN) = TCP_FLAG_SYN then result := result + 'SYN ' ;
    if (flags AND TCP_FLAG_RST) = TCP_FLAG_RST then result := result + 'RST ' ;
    if (flags AND TCP_FLAG_PSH) = TCP_FLAG_PSH then result := result + 'PSH ' ;
    if (flags AND TCP_FLAG_ACK) = TCP_FLAG_ACK then result := result + 'ACK ' ;
    if (flags AND TCP_FLAG_URG) = TCP_FLAG_URG then result := result + 'URG ' ;
    if (flags AND TCP_FLAG_ECH) = TCP_FLAG_ECH then result := result + 'ECH ' ;
    if (flags AND TCP_FLAG_CWR) = TCP_FLAG_CWR then result := result + 'CWR ' ;
    result := trim (result) ;
end ;

// Convert a 32-bit IP address into a string representation

function IPToStr (IPAddr: TInAddr): string ;
begin
    with IPAddr.S_un_b do
        Result := Format('%d.%d.%d.%d', [Ord (s_b1), Ord (s_b2), Ord (s_b3), Ord (s_b4)]) ;
end;

function StrToIP (strIP: string): TInAddr ;
begin
    Str2IP (strIP, result) ;
end ;

function IsIPStr (strIP: string): boolean ;
var
    IPAddr: TInAddr ;
begin
   result := Str2IP (strIP, IPAddr) ;
end ;

function IsFmtIPStr (var strIP: string): boolean ;
var
    IPAddr: TInAddr ;
begin
    result := Str2IP (strIP, IPAddr) ;
    if result then strIP := IPToStr (IPAddr) ;  // formats less space, zeros, etc.
end ;

function AscToInt (value: string): Integer;   // simple version of StrToInt
var
    E: Integer;
begin
    Val (value, result, E) ;
end;

function Str2IP (strIP: string; var IPAddr: TInAddr): boolean ;
var
    I, len, value, startpos, dotpos: Integer;
    MyIPAddr: TInAddr ;
    nonzeroflag: boolean ;
begin
    result := false ;
    IPAddr.S_addr := 0 ;
    len := Length (strIP) ;
    if len < 7 then exit ;    // 0.0.0.0 bare IP address

// read each dotted number
    nonzeroflag := false ;
    startpos := 1 ;
    for I := 1 to 4 do
    begin
        if len <= 0 then exit ;
        if I < 4 then
            dotpos := Pos ('.', Copy (strIP, startpos, len))
        else
            dotpos := len + 1 ;
        if dotpos <= 0 then exit ;   // not enough dots
        if dotpos > 1 then
            value := AscToInt (Copy (strIP, startpos, Pred (dotpos)))
        else
            value := 0 ;  // allow for blank
        if value > 255 then exit ;   // number invalid for conversion
        if value > 0 then nonzeroflag := true ;
        case I of
            1: MyIPAddr.S_un_b.s_b1 := u_char (value) ;
            2: MyIPAddr.S_un_b.s_b2 := u_char (value) ;
            3: MyIPAddr.S_un_b.s_b3 := u_char (value) ;
            4: MyIPAddr.S_un_b.s_b4 := u_char (value) ;
        end ;
        startpos := startpos + dotpos ;
        len := len - dotpos ;
    end ;

// check valid IP address, only allowed all zeroes
    if (MyIPAddr.S_un_b.s_b1 = u_char (0)) and nonzeroflag then exit ;

// found a valid IP address
    IPAddr := MyIPAddr ;
    result := true ;
end ;

function MacToStr (MacAddr: TMacAddr): string ;
begin
    result := Format ('%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
                   [MacAddr [0], MacAddr [1], MacAddr [2],
                    MacAddr [3], MacAddr [4], MacAddr [5]]) ;
end ;

// called by TFindList for sort and find comparison of traffic records
// sort is by source IP, then dest IP, then ServPort, then PackType

function CompareIPTraffic (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := CompareGTMem (Item1, Item2, TrafficIPCompLen) ;  // warning record must be packed
end ;

function CompareServTraffic (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := CompareGTMem (Item1, Item2, ServiceCompLen) ;  // warning record must be packed
end ;

constructor TTrafficClass.Create(AOwner: TComponent);
begin
    SetLength (FTrafficInfo, InitialTrafficSize) ;
    FTrafficList := TFindList.Create ;
    FTrafficList.Sorted := true ;
    FTrafficList.Capacity := InitialTrafficSize ;
    FTotTraffic := 0 ;
    SetLength (FServiceInfo, 0) ;
    FServiceList := TFindList.Create ;
    FServiceList.Sorted := true ;
    FTotService := 0 ;
    FWSocket := TWSocket.Create (AOwner) ;
    FWSocket.OnDnsLookupDone := DoneLookup ;
    FLookupBusy := false ;
end;

destructor TTrafficClass.Destroy;
begin
    Clear ;
    SetLength (FTrafficInfo, 0) ;
    FreeAndNil (FTrafficList) ;
    SetLength (FServiceInfo, 0) ;
    FreeAndNil (FServiceList) ;
    FreeAndNil (FWSocket) ;
end;

procedure TTrafficClass.Clear ;
begin
    SetLength (FTrafficInfo, InitialTrafficSize) ;
    FTrafficList.Clear ;
    FTotTraffic := 0 ;
    SetLength (FServiceInfo, 0) ;
    FServiceList.Clear ;
    FTotService := 0 ;
end;

procedure TTrafficClass.Add (PacketInfo: TPacketInfo) ;
var
    NewTraffic: TTrafficInfo ;
    TrafficRec: PTrafficInfo ;
    recnr, I: integer ;
begin
    FillChar (NewTraffic, Sizeof(NewTraffic), 0) ;
    with NewTraffic, PacketInfo do
    begin
        if EtherProto <> PROTO_IP then exit ;
        if NOT (ProtoType in [IPPROTO_TCP, IPPROTO_UDP, IPPROTO_ICMP]) then exit ;
        PackType := ProtoType ;
        if SendFlag then
        begin
            AddrLoc := AddrSrc ;
            AddrRem := AddrDest ;
            ServPort := PortDest ;
            BytesSent := PacketLen ;
            PacksSent := 1 ;
        end
        else
        begin
            AddrLoc := AddrDest ;
            AddrRem := AddrSrc ;
            ServPort := PortSrc ;
            BytesRecv := PacketLen ;
            PacksRecv := 1 ;
        end ;
        if ProtoType = IPPROTO_ICMP then
        begin
            ServPort := IcmpType ;
            if ServPort = 0 then ServPort := 8 ;  // change echo-reply to echo (ie ping)
        end
        else 
        begin
            if (ServPort >= 1024) and (PortSrc < 1024) then
                ServPort := PortSrc
            else if (ServPort >= 1024) and (PortDest < 1024) then
                ServPort := PortDest
        end ;
        LastDT := PacketDT ;
    end ;

  // see if only got a record for this traffic, update it
    if FTrafficList.Find (@NewTraffic, CompareIPTraffic, recnr) then
    begin
        TrafficRec := FTrafficList [recnr] ;
        if NOT Assigned (TrafficRec) then exit ;  // sanity check
        if CompareMem (TrafficRec, @NewTraffic, TrafficIPCompLen) then // double check for correct record
        begin
            inc (TrafficRec^.BytesSent, NewTraffic.BytesSent) ;
            inc (TrafficRec^.PacksSent, NewTraffic.PacksSent) ;
            inc (TrafficRec^.BytesRecv, NewTraffic.BytesRecv) ;
            inc (TrafficRec^.PacksRecv, NewTraffic.PacksRecv) ;
            TrafficRec^.LastDT := NewTraffic.LastDT ;
            exit ;
        end ;
    end ;

  // otherwise add a new traffic record
    if Length (FTrafficInfo) <= FTotTraffic then
    begin
        SetLength (FTrafficInfo, FTotTraffic * 2) ;  // allocate more records in dynamic array
      // must rebuild pointer list since resized array may have moved in memory
        FTrafficList.Clear ;
        FTrafficList.Capacity := FTotTraffic * 2 ;
        for I := 0 to Pred (FTotTraffic) do FTrafficList.Add (@FTrafficInfo [I]) ;
        FTrafficList.Sort (CompareIPTraffic) ;
    end ;
    NewTraffic.FirstDT := NewTraffic.LastDT ;
    FTrafficInfo [FTotTraffic] := NewTraffic ;
    FTrafficList.AddSorted (@FTrafficInfo [FTotTraffic], CompareIPTraffic) ;
    inc (FTotTraffic) ;
    LookupHosts ; // start lookup of host names
end ;

function TTrafficClass.GetUnSortTraf (item: integer): PTrafficInfo ;
begin
    if item < FTotTraffic then
        result := @FTrafficInfo [item]
    else
        FillChar (result, Sizeof(result), 0) ;
end;

function TTrafficClass.GetSortedTraf (item: integer): PTrafficInfo ;
begin
    if item < FTotTraffic then
        result := FTrafficList [item]
    else
        FillChar (result, Sizeof(result), 0) ;
end;

function TTrafficClass.GetServNameEx (PackType, ServPort: word): string ;
begin
    if PackType = IPPROTO_TCP then
        result := Lowercase (GetServName (ServPort))
    else if PackType = IPPROTO_UDP then
        result := Lowercase (GetServName (ServPort))
    else if PackType = IPPROTO_ICMP then
        result := Lowercase (GetICMPType (ServPort))
    else
        result := GetEtherProtoName (PackType) ;
end ;

function TTrafficClass.GetFmtTrafStr (item: integer): string ;
var
    TrafficRec: PTrafficInfo ;
    disploc, disprem: string ;
begin
    result := '' ;
    if item >= FTotTraffic then exit ;
    TrafficRec := FTrafficList [item] ;
    if NOT Assigned (TrafficRec) then exit ;  // sanity check
    with TrafficRec^ do
    begin
        disploc := HostLoc ;
        disprem := HostRem ;
        if disploc = '' then disploc := IPToStr (AddrLoc) ;
        if disprem = '' then disprem :=  IPToStr (AddrRem) ;
        if ServName = '' then ServName := GetServNameEx (PackType, ServPort) ;
        result := Format (sTrafficMask, [disploc, disprem, ServName,
            IntToKbyte (BytesSent), '[' + IntToKbyte (PacksSent) + ']',
                IntToKbyte (BytesRecv), '[' + IntToKbyte (PacksRecv) + ']',
                                    TimeToStr (FirstDT), TimeToStr (LastDT)  ]) ;
    end ;
end;

procedure TTrafficClass.UpdateService ;
var
    I, recnr: integer ;
    NewService: TServiceInfo ;
    ServiceRec: PServiceInfo ;

   procedure RebuildList ;
   var
        J: integer ;
   begin
        FServiceList.Clear ;
        for J := 0 to Pred (FTotService) do FServiceList.Add (@FServiceInfo [J]) ;
        FServiceList.Sort (CompareServTraffic) ;
   end ;

begin
    FServiceList.Clear ;
    FTotService := 0 ;
    if FTotTraffic = 0 then
    begin
        SetLength (FServiceInfo, 0) ;
        exit ;
    end ;
    SetLength (FServiceInfo, InitialTrafficSize) ;
    FServiceList.Capacity := InitialTrafficSize ;

// add total record
    FillChar (NewService, Sizeof(NewService), 0) ;
    NewService.ServName := 'TOTALS' ;
    FServiceInfo [FTotService] := NewService ;
    FServiceList.Add (@FServiceInfo [FTotService]) ;
    FTotService := 1 ;
    for I := 0 to Pred (FTotTraffic) do
    begin
        FillChar (NewService, Sizeof(NewService), 0) ;
        NewService.ServPort := FTrafficInfo [I].ServPort ;
        NewService.PackType := FTrafficInfo [I].PackType ;
        NewService.ServName := FTrafficInfo [I].ServName ;
        NewService.BytesSent := FTrafficInfo [I].BytesSent ;
        NewService.BytesRecv := FTrafficInfo [I].BytesRecv ;
        NewService.PacksSent := FTrafficInfo [I].PacksSent ;
        NewService.PacksRecv := FTrafficInfo [I].PacksRecv ;
        NewService.TotalHosts := 1 ;

     // increment totals
        inc (FServiceInfo [0].BytesSent, NewService.BytesSent) ;
        inc (FServiceInfo [0].PacksSent, NewService.PacksSent) ;
        inc (FServiceInfo [0].BytesRecv, NewService.BytesRecv) ;
        inc (FServiceInfo [0].PacksRecv, NewService.PacksRecv) ;
        inc (FServiceInfo [0].TotalHosts) ;

    // see if updating existing record
        if FServiceList.Find (@NewService, CompareServTraffic, recnr) then
        begin
            ServiceRec := FServiceList [recnr] ;
            if NOT Assigned (ServiceRec) then continue ; // sanity check
            if CompareMem (ServiceRec, @NewService, ServiceCompLen) then // double check for correct record
            begin
                inc (ServiceRec^.BytesSent, NewService.BytesSent) ;
                inc (ServiceRec^.PacksSent, NewService.PacksSent) ;
                inc (ServiceRec^.BytesRecv, NewService.BytesRecv) ;
                inc (ServiceRec^.PacksRecv, NewService.PacksRecv) ;
                inc (ServiceRec^.TotalHosts) ;
                continue ;    // next record
            end ;
        end ;

      // otherwise add a new service record
        if Length (FServiceInfo) <= FTotService then
        begin
            SetLength (FServiceInfo, FTotService * 2) ;  // allocate more records in dynamic array
          // must rebuild pointer list since resized array may have moved in memory
            FServiceList.Clear ;
            FServiceList.Capacity := FTotService * 2 ;
            RebuildList ;
        end ;
        FServiceInfo [FTotService] := NewService ;
        FServiceList.AddSorted (@FServiceInfo [FTotService], CompareServTraffic) ;
        inc (FTotService) ;
    end ;
    SetLength (FServiceInfo, FTotService) ;
    RebuildList ;     // keep Delphi 2006 happy 
end ;

function TTrafficClass.GetSortedServ (item: integer): PServiceInfo ;
begin
    if item < FTotService then
        result := @FServiceInfo [item]
    else
        FillChar (result, Sizeof(result), 0) ;
end ;

function TTrafficClass.GetFmtServStr (item: integer): string ;
var
    ServiceRec: PServiceInfo ;
begin
    result := '' ;
    if item >= FTotService then exit ;
    if FServiceList [0] <> @FServiceInfo [0] then  // sanity check
    begin
        result := 'Dynamic Array Memory Error' ;
        exit ;
    end;
    ServiceRec := FServiceList [item] ;
    if NOT Assigned (ServiceRec) then exit ;  // sanity check
    with ServiceRec^ do
    begin
        if ServName = '' then ServName := GetServNameEx (PackType, ServPort) ;
        result := Format (sServiceMask, [ServName,  IntToKbyte (BytesSent), '[' +
            IntToKbyte (PacksSent) + ']', IntToKbyte (BytesRecv), '[' +
                        IntToKbyte (PacksRecv) + ']', IntToCStr (TotalHosts)]) ;
    end ;
end ;

// total all traffic records

function TTrafficClass.GetTotals: TServiceInfo ;
var
    I: integer ;
begin
    FillChar (result, Sizeof(result), 0) ;
    if FTotTraffic = 0 then exit ;
    for I := 0 to Pred (FTotTraffic) do
    begin
        inc (result.BytesSent, FTrafficInfo [I].BytesSent) ;
        inc (result.BytesRecv, FTrafficInfo [I].BytesRecv) ;
        inc (result.PacksSent, FTrafficInfo [I].PacksSent) ;
        inc (result.PacksRecv, FTrafficInfo [I].PacksRecv) ;
    end ;
end ;

// look for next DNS lookup that needs doing, keep count of failures to avoid too many

procedure TTrafficClass.NextLookup ;
begin
    if FTotTraffic = 0 then exit ;
    if (FLookupLoc >= 0) then
    begin
        while FLookupLoc < FTotTraffic do
        begin
            with FTrafficInfo [FLookupLoc] do
            begin
                if (HostLoc = '') and (LookupAttempts < MaxDnsLookupAttempts) then
                begin
                    if FLookupLoc > 0 then  // copy previous record if same address
                    begin
                        if (AddrLoc.S_addr = FTrafficInfo [Pred (FLookupLoc)].AddrLoc.S_addr) then
                            HostLoc := FTrafficInfo [Pred (FLookupLoc)].HostLoc ;
                    end ;
                    if (HostLoc = '') then
                    begin
                        inc (LookupAttempts) ;
                        FWSocket.ReverseDnsLookup (IPToStr (AddrLoc)) ;
                        exit ; // async lookup started
                    end ;
                end ;
            end ;
            inc (FLookupLoc) ;
        end ;
        FLookupLoc := - 1 ;
        FLookupRem := 0 ;
    end ;
    if (FLookupRem >= 0) then
    begin
        while FLookupRem < FTotTraffic do
        begin
            with FTrafficInfo [FLookupRem] do
            begin
                if (HostRem = '') and (LookupAttempts < MaxDnsLookupAttempts) then
                begin
                    inc (LookupAttempts) ;
                    FWSocket.ReverseDnsLookup (IPToStr (AddrRem)) ;
                    exit ; // async lookup started
                end ;
            end ;
            inc (FLookupRem) ;
        end ;
        FLookupRem := - 1 ;
    end ;
    FLookupBusy := false ;
end ;

procedure TTrafficClass.DoneLookup (Sender: TObject; Error: Word);
begin
    if FLookupLoc >= 0 then
    begin
        if Error = 0 then FTrafficInfo [FLookupLoc].HostLoc :=
                                                     Lowercase (FWSocket.DnsResult) ;
        inc (FLookupLoc) ;
    end
    else if FLookupRem >= 0 then
    begin
        if Error = 0 then FTrafficInfo [FLookupRem].HostRem :=
                                                    Lowercase (FWSocket.DnsResult) ;
        inc (FLookupRem) ;
    end ;
    NextLookup ;
end ;

procedure TTrafficClass.LookupHosts ;
begin
    if FLookupBusy then exit ;
    if FTotTraffic = 0 then exit ;
    FLookupLoc := 0 ;
    FLookupRem := -1;
    NextLookup ;
end ;

end.
