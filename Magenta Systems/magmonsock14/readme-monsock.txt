Magenta Systems Internet Packet Monitoring Components v1.4
==========================================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018

Email: delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, 2018.

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Note that release 6 only fully supports Unicode RAS APIs with Delphi
2009 and later. Compatible with Windows 2000, XP, 2003, Vista, 2008,
7, 8, 2012, 10, 2016 and 2019.

The components need François PIETTE internet component suite (ICS) V8
from from http://www.overbyte.eu/.


Introduction
------------

Magenta Systems Internet Packet Monitoring Components are a set of Delphi
components designed to capture and monitor internet packets using either
raw sockets or the WinPcap device driver.  Hardware permitting, ethernet
packets may be captured and interpreted, and statistics maintained about
the traffic.  Uses of packet monitoring include totalling internet traffic
by IP address and service, monitoring external or internal IP addresses
and services accessed, network diagnostics, and many other applications.
The component includes two demonstration applications, one that displays
raw packets, the other that totals internet traffic.  The components
include various filters to reduce the number of packets that need to be
processed, by allowing specific IP addresses to be ignored, LAN mask to
ignore local traffic, and ignore non-IP traffic such as ARP.

The components capture packets using two different techniques, with
differing benefits and features:

1 - Raw sockets which are available with Windows 2000 and later.  This
uses WSocket version 6 and later from the François Piette internet
component suite, from http://www.overbyte.be/. Major benefit is that no
other software needs to be installed, but raw sockets don't seem to work
fully with some network adaptors, and ignore non-IP protocols. Some
adaptors may capture received packets, but ignore anything sent.

2 - WinPcap device driver or Npcap NDIS driver needs to be installed.
WinPCap is no longer developed, although it still installs and works on
Windows 10 Win64. Npcap is a new project being actively developed,
https://nmap.org/npcap/, which uses newer NDIS 6 light weight filter
monitoring APIs for Windows 7 and later which is faster and less overhead
than WinPCap, and is fully supported on Windows 10.  Npcap should be
installed with the WinPCap compatible box ticked, this component does
not yet support any advanced features of Npcap.

Npcap may be installed so that administrator program rights are required by
applications for improved security.


Component Overview
------------------

There are two main low level components, TMonitorSocket in
MagentaMonsock.pas which supports raw window sockets, and TMonitorPcap in
MagentaMonpcap.pas that supports WinPcap.  Both have very similar
properties and return ethernet packets using identical events, formatted
identically, allowing the same application to use either or both low level
components.  There are subtle differences, raw sockets monitors a specific
IP address, whereas WinPcap monitors all traffic on an adaptor.  Both may
potentially monitor traffic other than the local PC, depending on LAN
structure.  Common functions and declarations are in MagentaPackhdrs.pas


Common Types
------------

TMacAddr = array [0..5] of byte ;  // a MAC address

// record used to return packet to application for
// both raw sockets and winpcap

TPacketInfo = record
    PacketLen: integer ;   // total length of packet
    EtherProto: word ;     // ethernet protocol
    EtherSrc: TMacAddr ;   // ethernet MAC addresses
    EtherDest: TMacAddr ;
    AddrSrc: TInAddr ;     // IP addresses are 32-bit binary
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

TTrafficInfo = packed record
    AddrLoc: TInAddr ;  // IP addresses are 32-bit binary
    AddrRem: TInAddr ;
    ServPort: word ;    // service port
    PackType: word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc
    HostLoc: string ;   // host domains for IP addresses, if available
    HostRem: string ;
    ServName: string ;  // looked up
    BytesSent: int64 ;  // traffic
    BytesRecv: int64 ;
    PacksSent: integer ;
    PacksRecv: integer ;
    LookupAttempts: integer ; // how many host name lookup attempts
    FirstDT: TDateTime ;   // when this traffic started
    LastDT: TDateTime ;    // last traffic update
end ;
PTrafficInfo = ^TTrafficInfo ;

TServiceInfo = packed record
    ServPort: word ;    // service port
    PackType: word ;    // protocol or packet type, TCP, UDP, ARP, ICMP, etc
    ServName: string ;  // looked up
    TotalHosts: integer;// how many different hosts for this service
    BytesSent: int64 ;  // traffic
    BytesRecv: int64 ;
    PacksSent: integer ;
    PacksRecv: integer ;
end ;
PServiceInfo = ^TServiceInfo ;

THdrEthernet = packed record   // Ethernet frame header - Network Interface Layer
    dmac: TMacAddr;
    smac: TMacAddr;
    protocol: WORD;
end;
PHdrEthernet = ^THdrEthernet ;



Class TMonitorSocket
--------------------

The component may be installed on palette, but is non-visual so it's
usually easier to create it in code.  This class is for monitoring
raw sockets on Windows 2000 and better.

TMonitorSocket is a descendent of TCustomWSocket (in
OverbyteIcsWsocket.pas).

The following properties should be set before monitoring is started:

Addr - IP address on which to listen for packets.
AddrMask - IP mask of address to ignore
IgnoreData - true/false, true if only doing statistics
IgnoreLAN - if AddrMask should be used
SetIgnoreIP - a list of IP addresses that should be ignored
onPacketEvent - the event in which packets will be returned

The LocalIPList public variable lists all IP addresses available for
monitoring.

The StartMonitor and StopMonitor methods start and stop raw packet
monitoring, with the onPacketEvent event being called, often several
times a second, as a TPacketInfo record.

There are also four cumulative traffic properties, TotRecvBytes,
TotSendBytes, TotRecvPackets and TotSendPackets which are reset each
time monitoring starts.


Class TMonitorPcap
-------------------

The component may be installed on palette, but is non-visual so it's
usually easier to create it in code.  This class uses WinPcap or Npcap
that must have been previously installed. The high level WinPcap
functions are in MagentaPcap.pas, MagentaPacket32.pas,
MagentaNdis_def.pas and MagentaBpf.pas.  The interface to WinPcap is
packet.dll, and all functions are loaded dynamically with LoadPacketDll
so the application will work even if the DLL is not available.  The
component uses a thread internally to poll the device driver for new
packets.

The following properties should be set before monitoring is started:

MonAdapter - index of adaptor to monitor, selected from AdapterDescList
Addr - local IP address (see below)
AddrMask - IP mask for IP address
Promiscuous - true/false, true to monitor sent packets, but may not work
IgnoreData - true/false, true if only doing statistics
IgnoreLAN - if AddrMask should be used to ignore local traffic
SetIgnoreIP - a list of IP addresses that should be ignored
onPacketEvent - the event in which packets will be returned

There are other exposed WinPcap methods:

GetAdapters - fills the AdapterNameList and AdapterDescList lists with
  the names of network adaptors
GetIPAddresses - returns three lists of IPs, masks and broadcast IPs for
  a specific network adaptor.

The StartMonitor and StopMonitor methods start and stop WinPcap packet
monitoring, with the onPacketEvent event being called, often several
times a second, as a TPacketInfo record.

There are also four cumulative traffic properties, TotRecvBytes,
TotSendBytes, TotRecvPackets and TotSendPackets which are reset each
time monitoring starts.


Class TTrafficClass
-------------------

This component is used to accumulate internet traffic statistics. It is
the basis of the Traffic Monitor demo application.  Use is very simple,
just call the Add method from onPacketEvent.  The component checks for
unique remote IP addresses and ports (ie services), and totals traffic
for them in TTrafficInfo.  The UpdateService method may be called to
update TServiceInfo records which consolidate traffic for any IP into
service.  The component automatically reverse looks-up IP address into
domain names, where possible.


Demonstration Application
-------------------------

Two Windows demonstration applications are supplied, with source and
compiled programs, SOCKMON.EXE displays raw packets, while SOCKSTAT.EXE
totals internet traffic.


Files Enclosed
--------------

=Demo Applications
monmain.dfm
monmain.pas
sockmon.dpr
sockmon.exe
sockmon.res
statmain.dfm
statmain.pas
sockstat.dpr
sockstat.exe
sockstat.res

=Component
MagentaBpf.pas
MagentaMonpcap.pas
MagentaMonsock.pas
MagentaNdis_def.pas
MagentaPacket32.pas
MagentaPackhdrs.pas
MagentaPcap.pas

=Support files
MagSubs1.pas
MagClasses.pas
ports.txt
protocols.txt


Release Notes
-------------

29th October 2005 - 1.1 - baseline

8th August 2008 - 1.2 - updated to support ICS V6 and V7, and Delphi 2009
                        when stopping capture ignore any buffered data
                          so it stops faster
                        tested with WinPCap 4.0.2 which is included

13th August 2010 - 1.3 - fixed various cast warnings with Delphi 2009
                         tested with WinPCap 4.1.2 which is included

26th November 2018 - 1.4 - comsmetics only for Npcap support, tested
                             with ICS V8.
                           added program Admin rights check and force
                            Wincap if missing since socket monitoring
                            won't work


Copyright Information
---------------------

Magenta Systems Internet Packet Monitoring Components are freeware, but
are still copyrighted by Magenta Systems Ltd who may change the status
or withdraw it at any time, without notice.

Magenta Systems Internet Packet Monitoring Components may be freely
distributed via web pages, FTP sites, BBS and conferencing systems or on
CD-ROM in unaltered zip format, but no charge may be made other than
reasonable media or bandwidth cost.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: https://www.magsys.co.uk/delphi/




