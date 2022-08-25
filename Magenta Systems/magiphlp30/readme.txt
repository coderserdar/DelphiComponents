agenta Systems Internet Protocol Helper Component v3.0 Beta
===========================================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Compatible with Windows Vista, 2008, 7, 8, 2012, 10, 2016 and 2019.

Note that some IP Helper APIs have minimum OS requirements.


Introduction
------------

Magenta Systems Internet Protocol Helper Component is a set of
functions implementing the the Internet Protocol Helper (IP Helper) APIs.
They enable the retrieval and modification of network configuration
settings for the local computer. The component loads all functions
dynamically so applications will still run on earlier Windows versions
that did not support certain IP Helper functions. The component returns
the various parameters returned by the APIs as Delphi records, for ease
of integration into applications.

A comprehensive TCP/IP/ARP Monitor application is included using all the
APIs to display TCP and UPD Connections (and the names of applications
using them), IP address and routing tables, network adaptor information
(such as MAC and IP addresses), ARP table, network statistics and dynamic
DNS server addresses.

IP Helper provides capabilities in the following areas:

- Retrieving Information About Network Configuration
- Managing Network Adapters
- Managing Interfaces
- Managing IP Addresses
- Using the Address Resolution Protocol
- Retrieving Information on the Internet Protocol and the Internet Control
  Message Protocol
- Managing Routing
- Receiving Notification of Network Events
- Retrieving Information About the Transmission Control Protocol and the
  User Datagram Protocol

The conversion and demo application are based on earlier work by Dirk
Claessens, but with many bug fixes and enhancements.


Converted IP Helper APIs
------------------------

Note this is only a partial conversion of IP Helper APIs, where most
Windows API are converted into a Delphi function that return one or more
records or string lists.  Many of these records include dynamic string
arrays, for instance returning multiple IP addresses.

AllocateAndGetTcpExTableFromStack as IpHlpTCPTable
AllocateAndGetUdpExTableFromStack as IpHlpUDPTable
GetAdaptersInfo as IpHlpAdaptersInfo
GetIcmpStatistics as Get_ICMPStats
GetIfEntry as IpHlpIfEntry
GetIfTable as IpHlpIfTable
GetIpAddrTable as Get_IPAddrTable
GetIpForwardTable as Get_IPForwardTable
GetIpNetTable as Get_ARPTable
GetIpStatistics as Get_IPStatistics
GetNetworkParams as IpHlpNetworkParams
GetPerAdapterInfo as IpHlpAdaptersInfo
GetRTTAndHopCount
GetTcpStatistics as IpHlpTCPStatistics
GetTcpTable as IpHlpTCPTable
GetUdpStatistics as Get_UdpStatistics
GetUdpTable as IpHlpUDPTable
GetExtendedTcpTable as IpHlpTCPTable
GetExtendedUdpTable as IpHlpUDPTable
GetAdaptersAddresses as IpHlpAdaptersInfo

Full information about these APIs is available in MSDN.


DNS Helper
---------

The DNS Helper unit returns the dynamic DNS address for all platforms,
primarily by running the winipcfg.exe or ipconfig.exe programs and
redirecting the report output to a text file which is parsed for the
address.

On Windows W2K and later, it's more efficient to use IpHlpNetworkParams
which returns the dynamic DNS IP address.


Files Enclosed
--------------

=Demo Application
utcpip.dfm
utcpip.pas
tcpipmon.dpr
tcpipmon.exe

=Headers and Components
iphelper.pas
iphlpapi.pas
dnshelper.pas


History
-------

1.9 - 8th August 2006
Corrected IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi
Interfaces now show type description, adaptor correct type description
Made main window resizable, needs minimum screen 1024x768

v2.0 - 25th February 2007 - Angus
Many more IF_xx_ADAPTER type literals, thanks to Jean-Pierre Turchi

v2.1 - 5th August 2008 - Angus
Made compatible with Delphi 2009, supports Unicode

v2.2 - 16th January 2009 - Angus
Added GetAdaptersAddresses (XP and later) has IPv6 addresses (but not yet
getting them) Note: gateway IPs don't seem to be returned by
GetAdaptersAddresses
Added GetExtendedTcpTable and GetExtendedUdpTable (XP SP2, W2K3 SP1, Vista
and later)
Replacements for AllocateAndGetTcpExTableFromStack/etc.
Added connection start time.
Using WideString for program paths and adaptor descriptions for Unicode
compatibility
Added two public variables:
ShowExePath if true displays full program path for connection tables
UseAdressesAPI if true uses GetAdaptersAddresses instead of GetAdaptersInfo

v2.3 - 3rd August 2009
Changed ULONGLONG to LONGLONG for Delphi 7 compatibility

v2.4 - 8th August 2010
Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
Fixes for 64-bit compatibility with Delphi XE2 and later

v3.0 - 26th November 2018
Added IPv6 support, numerous new structures and functions, Vista and later.
Only supporting XP SP3 and later, so remove code for earlier OSs.
Still runs on XP SP3, but TCP and UDP connection lists not supported and
some other functions return limited info, IP addresses in particular.
Note XP has not been tested for several years since it's out of support.
Added notification functions for interface changes.
Larger windows, some columns wider.

IPv6 not yet supported for ARP or IP Routing table, and need more
documentation on IPV6.


Copyright Information
---------------------

Magenta Systems Internet Protocol Helper Component is freeware, but is
still copyrighted by Magenta Systems Ltd who may change the status or
withdraw it at any time, without notice.

Magenta Systems Internet Protocol Helper Component may be freely
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



