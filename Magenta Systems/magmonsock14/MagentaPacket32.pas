{ Magenta Systems Internet Packet Monitoring Components

********************************************************************************
--------------------------------------------------------------------------------
                                  API for the
                 Packet Capture Driver by Politecnico di Torino

                      Converted By Lars Peter Christiansen
--------------------------------------------------------------------------------

 TERMS AND CONDITIONS OF USE.
 some parts of this software is Copyright(C) 2000 Lars Peter Christiansen.

 The author of this software assumes no liability for damages caused under
 any circumstances whatsoever, and is under no obligation. Use of the software
 indicates acceptance of all conditions contained in this document. If you do
 not agree to these terms, you must delete this software immediately.

 You may distribute the archive in which this software is distributed, but
 under no circumstances must this archive be changed. Distributing a modified
 archive is a violation of the software license.

 If you do redistribute this software, please let me know at the email address
 given below.

 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : lp@nzlab.dk
Website: http://www.nzlab.dk

Packet32.dll author:
old website: http://netgroup-serv.polito.it/windump
new website: http://www.winpcap.org/

Updated by Angus Robertson, Magenta Systems Ltd, England, v1.3 9th August 2010
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Some parts Copyright Magenta Systems Ltd

--------------------------------------------------------------------------------

   Original Filename : Packet32.pas

   Packet.dll written in C.

   PacketOpenAdapter
   PacketCloseAdapter
   PacketResetAdapter

   PacketGetAdapterNames
   PacketSetBuff
   PacketSetBpf
   PacketSetHwFilter
   PacketGetNetType
   PacketGetStats

   PacketAllocatePacket
   PacketFreePacket
   PacketSendPacket
   PacketRecievePacket
   PacketWaitPacket
   PacketRequest

   PacketGetDriverVersion
   PacketGetVersion
   PacketGetNetInfoEx
   PacketSetMinToCopy
   PacketSetSnapLen


********************************************************************************

  CHANGES :

   8th August 2010 - Angus Robertson, Magenta Systems Ltd,
                  Tested with WinPcap version 4.1.1 21st October 2009

  8th August 2008 - Angus Robertson, Magenta Systems Ltd,
                  Updated for Delphi 2009
                  Tested with WinPcap version 4.0.2 9th November 2007

  29 October 2005 - Angus Robertson, Magenta Systems Ltd
                 replaced static linkage with dynamic DLL loading
                 added new winpcap website
                 added PacketGetDriverVersion, PacketGetVersion, PacketGetNetInfoEx
                 added PacketSetMinToCopy, PacketSetSnapLen
                 tested with 3.1 final release 5th August 2005

  17 January  2002 : - Added "PacketSetReadTimeout()".
                     - Corrected TPacket. Thanks to Deheng Xu.

  20 November 2000 : TPacket Modified. No longer delivers faulty packets.
                     Thanks to Pol-Brieuc Lemétayer.
                     Email : pol-brieuc.lemetayer@eleve.emn.fr


}

unit MagentaPacket32;

interface

uses Windows, Winsock,
     MagentaBpf;         // Needs bpf structures

Const
  DLL  = 'packet.dll';            // Name of DLL file
  DEFAULT_DRIVERBUFFER = 1000000; // Dimension of the buffer in driver
  MAX_LINK_NAME_LENGTH = 64;      // Adapters symbolic names maximum length

type

  // Adapter with which the driver communicates
  Padapter = ^Tadapter;
  Tadapter = packed Record
    hFile        : LongWord;
    SymbolicLink : array [0..MAX_LINK_NAME_LENGTH-1] of AnsiChar;
  end;

  // Packet the driver uses as means of data transport.
  // both snooped data and certain device controlling
  Ppacket = ^Tpacket;
  Tpacket = packed record           // Changed Jan.1 2002. Thanks to Deheng Xu
    hevent             :Thandle;
    OverLapped         :TOVERLAPPED;
    Buffer             :Pointer;
   //Next               :Pointer;     // also commented out in "packet32.h"
    Length             :Longword;
    ulBytesReceived    :LongWord;
    bIoComplete        :Boolean;
  end;


  // [Gotten from LIBPCAP\ntddpack.h]
  // Data structure to control the device driver
  PPACKET_OID_DATA = ^TPACKET_OID_DATA;
  TPACKET_OID_DATA = packed record
    Oid   : LongWord;               // Device control code
    Length: LongWord;               // Length of data field
    Data  : Pointer;                // Start of data field
  end;

  // [Gotten from BPF.h? - more appropiate here!]
  Pnet_type = ^Tnet_type;
  Tnet_type = packed record
    LinkType,
    LinkSpeed : LongWord;
  end;

// from winsock2.h
// Portable socket structure (RFC 2553).

// Desired design of maximum size and alignment.
// These are implementation specific.

const
  _SS_MAXSIZE   = 128;               // Maximum size.
  {$EXTERNALSYM _SS_MAXSIZE}
  _SS_ALIGNSIZE = SizeOf(Int64);  // Desired alignment.
  {$EXTERNALSYM _SS_ALIGNSIZE}

// Definitions used for sockaddr_storage structure paddings design (holds both ip4 and ip6 addresses)

  _SS_PAD1SIZE = _SS_ALIGNSIZE - SizeOf(short);
  {$EXTERNALSYM _SS_PAD1SIZE}
  _SS_PAD2SIZE = _SS_MAXSIZE - (SizeOf(short) + _SS_PAD1SIZE + _SS_ALIGNSIZE);
  {$EXTERNALSYM _SS_PAD2SIZE}

type
  sockaddr_storage = record
    ss_family: short;               // Address family.
    __ss_pad1: array [0.._SS_PAD1SIZE - 1] of AnsiChar;  // 6 byte pad, this is to make
                                   // implementation specific pad up to
                                   // alignment field that follows explicit
                                   // in the data structure.
    __ss_align: Int64;            // Field to force desired structure.
    __ss_pad2: array [0.._SS_PAD2SIZE - 1] of AnsiChar;  // 112 byte pad to achieve desired size;
                                   // _SS_MAXSIZE value minus size of
                                   // ss_family, __ss_pad1, and
                                   // __ss_align fields is 112.
  end;
  {$EXTERNALSYM sockaddr_storage}
  TSockAddrStorage = sockaddr_storage;
  PSockAddrStorage = ^sockaddr_storage;

 // from packet32.h - used by PacketGetNetInfoEx for 3.1 and later
  Pnpf_if_addr = ^Tnpf_if_addr ;
  Tnpf_if_addr = packed record
    IPAddress: TSockAddrStorage ;  // includes IP4 and IP6 addresses
    SubnetMask: TSockAddrStorage ;
    Broadcast: TSockAddrStorage ;
  end ;

 // from packet32.h - used by PacketGetNetInfoEx for 3.0 and earlier
  Pnpf_if_addr30 = ^Tnpf_if_addr30 ;
  Tnpf_if_addr30 = packed record
    IPAddress: TSockAddrIn ;
    SubnetMask: TSockAddrIn ;
    Broadcast: TSockAddrIn ;
  end ;

var

//------------------------------------------------------------------------------
//ULONG PacketGetAdapterNames(PTSTR pStr, PULONG BufferSize)
//------------------------------------------------------------------------------
PacketGetAdapterNames: Function (pStr: PAnsiChar; BufferSize: PLongWord) : Boolean; cdecl ;
{
This is the first function that must be used to communicate with the driver.
It returns the names of the adapters installed in the system through the user
allocated buffer pStr. BufferSize is the length of the buffer.

Warning: the result of this function is obtained querying directly the registry,
therefore the format of the result in Windows NT is different from the one in
Windows 95/98. This is due to the fact that Windows 95 uses the ASCII
encoding method to store a string, while Windows NT uses UNICODE. After a
call to PacketGetAdapterNames in Windows 95,  pStr contains an ASCII string
with the names of the adapters separated by ASCII "\0". The string is
terminated by a double "\0". In Windows NT, pStr contains a UNICODE string
with the names of the adapters separated by a "\0" UNICODE character
(i.e. 2 ASCII "\0"), and the string ends with a double UNICODE "\0".

Angus - above warning only relates to WinPcap 3.0 and earlier
with WinPcap 3.1 and later only ASCII is returned

Returns:
If the function succeeds, the return value is nonzero. If the return value is zero, BufferSize contains the number of bytes that are needed to contain the adapter list.
Usually, this is the first function that should be used to communicate with the driver. It returns the names of the adapters installed on the system and supported by WinPcap. After the names of the adapters, pStr contains a string that describes each of them.
After a call to PacketGetAdapterNames pStr contains, in succession:

1 a variable number of ASCII (Unicode for 3.0 and earlier) strings, each with the
  names of an adapter, separated by a "\0"
2 a double "\0"
3 a number of ASCII strings (for all versions), each with the description of an
  adapter, separated by a "\0". The number of descriptions is the same of the one
  of names. The fisrt description corresponds to the first name, and so on.
4 a double "\0".

}

//------------------------------------------------------------------------------
// LPADAPTER PacketOpenAdapter(LPTSTR AdapterName)
//------------------------------------------------------------------------------
PacketOpenAdapter: Function (AdapterName:PAnsiChar) : PAdapter; cdecl ;
{
This function receives a string containing the name of the adapter to open and
returns the pointer to a properly initialized ADAPTER object. The names of the
adapters can be obtained calling the PacketGetAdapterNames function.

Note: as already said, the Windows 95 version of the capture driver works with
the ASCII format, the Windows NT version with UNICODE. Therefore, AdapterName
should be an ASCII string in Windows 95, and a UNICODE string in Windows NT.
This difference is not a problem if the string pointed by AdapterName was
obtained through the PacketGetAdapterNames function, because it returns the
names of the adapters in the proper format. Instead, some problems can arise
in Windows NT if the string is obtained from ANSI C functions like scanf,
because they use the ASCII format. This can be a relevant problem when porting
command-line applications from UNIX. To avoid it, we included in the Windows NT
version of PacketOpenAdapter a routine to convert strings from ASCII to UNICODE.
PacketOpenAdapter in Windows NT accepts both the ASCII and the UNICODE format.
If a ASCII string is received, it is converted to UNICODE before being passed
to the driver.
}

//------------------------------------------------------------------------------
// VOID PacketCloseAdapter(LPADAPTER lpAdapter)
//------------------------------------------------------------------------------
PacketCloseAdapter: Procedure (pAdapter:Padapter); cdecl ;
{
This function deallocates the ADAPTER structure lpAdapter, and closes the
adapter pointed by it.
}

//------------------------------------------------------------------------------
// LPPACKET PacketAllocatePacket(void)
//------------------------------------------------------------------------------
PacketAllocatePacket: Function: PPacket; cdecl ;
{
Allocates a PACKET structure and returns a pointer to it. The structure
returned must be properly initialized by calling the PacketInitPacket function.

Warning: The Buffer field of the PACKET structure is not set by this function.
The buffer must be allocated by the programmer, and associated to the PACKET
structure with a call to PacketInitPacket.
}

//------------------------------------------------------------------------------
// VOID PacketInitPacket(LPPACKET lpPacket, PVOID Buffer, UINT Length)
//------------------------------------------------------------------------------
PacketInitPacket: Procedure (pPacket:Ppacket;Buffer:Pointer;Length:LongWord); cdecl;
{
It initializes a structure PACKET. There are three input parameters:

* a pointer to the structure to initialize
* a pointer to the user-allocated buffer that will contain the packet data
* length of the buffer. This is the maximum length that will be transferred in a
  single read from the driver to the application.

Note: The dimension of the buffer associated with the PACKET structure is a
parameter that can sensibly influence the performances of the capture process.
This buffer will in fact receive the packets from the packet capture driver.
The driver is able to collect data from several packets, returning it with only
one read call (see the PacketReceivePacket function). The number of packets
that the driver can transfer to the application in a single call with this
method is limited only by the dimension of the buffer associated with the
PACKET structure used to perform the reading. Therefore setting a big buffer
with PacketInitPacket can throw down the number of system calls, improving the
capture speed. Notice also that, when the application performs a
PacketReceivePacket, it is usually NOT blocked until the buffer associated with
the PACKET structure full. The driver copies the data present in its buffer,
but awakes the application without filling its buffer if it has not enough data
at the moment. In this way, also with big buffers, the application works
efficiently when the data rate on the network is low.
}


//------------------------------------------------------------------------------
// VOID PacketFreePacket(LPPACKET lpPacket)
//------------------------------------------------------------------------------

PacketFreePacket: Procedure( pPacket:Ppacket); cdecl ;
{
This function frees the PACKET structure pointed by lpPacket.

Warning: The Buffer field of the PACKET structure is not deallocated by this
function, and must be deallocated by the programmer.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketReceivePacket(LPADAPTER AdapterObject, LPPACKET lpPacket,
//                             BOOLEAN Sync)
//------------------------------------------------------------------------------
PacketReceivePacket: Function (AdapterObject:Padapter;pPacket:PPacket;
         Sync:Boolean):Longbool; cdecl ;

{
This function performs the capture of a set of packets. It has the following
input parameters:

* a pointer to an ADAPTER structure identifying the network adapter from which
  the packets must be captured
* a pointer to a PACKET structure that will contain the packets
* a flag that indicates if the operation will be done in a synchronous or
  asynchronous way. If the operation is synchronous, the function blocks the
  program, returning only when the it is completed. If the operation is
  asynchronous, the function doesn’t block the program, and the PacketWaitPacket
  procedure must be used to verify the correct completion.

The number of packets received with this function cannot be known before the
call and can vary a lot. It depends on the number of packets actually stored in
the driver’s buffer, on the size of these packets, and on the size of the buffer
associated with the lpPacket parameter. Figure 3.1 shows the method used by the
driver in order to send the packets to the application.

                 [BPF_HDR]
                 [ DATA  ]
                 [PADDING]
                 [BPF_HDR]
                 [ DATA  ]
                 [PADDING]

                 Figure 3.1: method used to encode the packets

Packets are stored in the buffer associated with the lpPacket PACKET structure.
Each packet has a trailer consisting in a bpf_hdr structure that defines its
length and holds its timestamp. At the end of the packet there is a padding
used to word-align the data in the buffer (to increase the speed of the copies).
In order to extract the packets from the buffer the bh_datalen and bh_hdrlen of
the bpf_hdr structures should be used. An example can be seen in the sample
application provided in the developer's pack, or in the pcap_read() function in
the pcap-win32.c file (that can be found in the source distribution). Pcap
extracts correctly each incoming packet before passing it to the application,
so an application that uses it will not have to do this operation.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketWaitPacket(LPADAPTER AdapterObject, LPPACKET lpPacket)
//------------------------------------------------------------------------------
PacketWaitPacket: Function (AdapterObject:Padapter;lpPacket:Ppacket):LongBool; cdecl ;
{
This function is used to verify the completion of an I/O operation on the
packet capture driver. It is blocking if the operation has still to be
completed by the driver. The return value is TRUE if the operation was
successful, FALSE otherwise, and the SDK GetLastError function can be used in
order to retrieve the error code.
}



//------------------------------------------------------------------------------
// BOOLEAN PacketSendPacket(LPADAPTER AdapterObject, LPPACKET pPacket, BOOLEAN Sync)
//------------------------------------------------------------------------------
PacketSendPacket: Function ( AdapterObject:Padapter;pPacket:PPacket;Sync:boolean)
         :Longbool ;cdecl ;

{This function is used to send a packet to the network through the adapter
specified with the AdapterObject parameter. It has the same syntax of the
PacketReceivePacket function. This function can be used to send only a packet
at a time and the user will not have to put a bpf_hdr header before it.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketResetAdapter(LPADAPTER AdapterObject)
//------------------------------------------------------------------------------
PacketResetAdapter: Function ( AdapterObject:Padapter):Longbool; cdecl ;
{
It resets the adapter passed as input parameter. Returns TRUE if the operation
is performed successfully.
}




//------------------------------------------------------------------------------
// BOOLEAN PacketSetHwFilter(LPADAPTER AdapterObject, ULONG Filter)
//------------------------------------------------------------------------------
PacketSetHwFilter: Function( AdapterObject:Pointer;Filter:Longword):Longbool; cdecl ;
{
This function sets a hardware filter on the incoming packets. The constants
that define the filters are declared in the file ntddndis.h. The input
parameters are the adapter on which the filter must be defined, and the
identifier of the filter. The value returned is TRUE if the operation was
successful. Here is a list of the most useful filters:

NDIS_PACKET_TYPE_PROMISCUOUS: set the promiscuous mode. Every incoming packet is
                              accepted by the adapter.
NDIS_PACKET_TYPE_DIRECTED   : only the packets destined to the adapter are
                              accepted.
NDIS_PACKET_TYPE_BROADCAST  : only the broadcast packets are accepted.
NDIS_PACKET_TYPE_MULTICAST  : only the multicast packets belonging to the groups
                              of which this adapter is a member are accepted.
NDIS_PACKET_TYPE_ALL_MULTICAST: every multicast packet is accepted
}


//------------------------------------------------------------------------------
// BOOLEAN PacketRequest(LPADAPTER AdapterObject,BOOLEAN Set, PPACKET_OID_DATA
//                       OidData)
//------------------------------------------------------------------------------
PacketRequest: Function ( AdapterObject:Padapter;isSet:Longbool;OidData:
                        PPacket_oid_data ):Longbool;cdecl ;

{This function is used to perform a query/set operation on the adapter pointed
by AdapterObject. The second parameter defines if the operation is a set
(set=1) or a query (set=0). The third parameter is a pointer to a
PACKET_OID_DATA structure (see the section on the data structures).
The return value is true if the function is completed without errors.
The constants that define the operations are declared in the file ntddndis.h.
More details on the argument can be found in the documentation provided with
the DDK.

NOTE: not all the network adapters implement all the query/set functions.
There is a set of mandatory OID functions that is granted to be present on all
the adapters, and a set of facultative functions, no provided by all the
adapters (see the DDKs to see which functions are mandatory). If you use a
facultative function, please be careful and enclose it in an if statement to
check the result.
}


//------------------------------------------------------------------------------
//BOOLEAN PacketSetBuff(LPADAPTER AdapterObject, int dim)
//------------------------------------------------------------------------------
PacketSetBuff: Function (AdapterObject: Padapter;dim:integer) : Longbool; cdecl ;

{This function is used to set a new dimension of the driver’s circular buffer
associated with the adapter pointed by AdapterObject. dim is the new dimension
in bytes. The function returns TRUE if successfully completed, FALSE if there
is not enough memory to allocate the new buffer. When a new dimension is set,
the data in the old buffer is discarded and the packets stored in it are lost.

Note: the dimension of the driver’s buffer affects HEAVILY the performances of
the capture process. In fact, a capture application needs to make operations on
each packet while the CPU is shared with other tasks. Therefore the application
should not be able to work at network speed during heavy traffic or bursts,
especially in presence of high CPU load due to other applications. This problem
is more noticeable on slower machines. The driver, on the other hand, runs in
kernel mode and is written explicitly to capture packets, so it is very fast
and usually does not loose packets. Therefore, an adequate buffer in the driver
to store the packets while the application is busy can compensate the slowness
of the application and can avoid the loss of packets during bursts or high
network activity. When an instance of the driver is opened the dimension of the
buffer is set to 0. The programmer must remember to set it to a proper value.

Libpcap calls this functions and sets the buffer size to 1MB. Therefore programs
written using libpcap usually do not need to cope with this problem.
}


//------------------------------------------------------------------------------
// BOOLEAN PacketSetBpf(LPADAPTER AdapterObject, struct bpf_program *fp)
//------------------------------------------------------------------------------
PacketSetBpf: Function ( AdapterObject:Padapter;fp:Pbpf_program):Longbool; cdecl;

{This function associates a new BPF filter with the adapter AdapterObject.
The filter, pointed by fp, is a set of instructions that the BPF
register-machine of the driver will execute on each packet. Details can be
found into the chapter on the driver, or in [McCanne and Jacobson 1993].
This function returns TRUE if the driver is set successfully, FALSE if an
error occurs or if the filter program is not accepted. The driver performs a
check on every new filter in order to avoid system crashes due to bogus or
buggy programs, and it rejects the invalid filters.

If you need to create a filter, use the pcap_compile function of libpcap.
It converts a text filter with the syntax of WinDump (see the manual of
WinDump for more details) into a BPF program. If you don't want to use libpcap,
but you need to know the code of a filter, launch WinDump with the -d or -dd
or -ddd parameters.
}

//------------------------------------------------------------------------------
//  BOOLEAN PacketGetStats(LPADAPTER AdapterObject, struct bpf_stat *s)
//------------------------------------------------------------------------------
PacketGetStats: Function ( AdapterObject:Padapter;s: Pbpf_stat):Longbool; cdecl;

{With this function, the programmer can know the value of two internal variables
of the driver:

* the number of packets that have been received by the adapter AdapterObject,
  starting at the time in which it was opened.
* the number of packets received by the adapter but that have been dropped by
  the kernel. A packet is dropped when the application is not ready to get it
  and the buffer associated with the adapter is full.

The two values are copied by the driver in a bpf_stat structure (see section 3
of this manual) provided by the application. These values are very useful to
know the situation of the network and the behavior of the capture application.
They are also very useful to tune the capture stack and to choose the
dimension of the buffers. In fact:

a high value of the bs_recv variable means that there is a lot of traffic on the
network. If the application doesn’t need all the packets (for example a monitor
application may want to capture only the traffic generated by a particular
protocol, or by a single host), it is better to set a selective BPF filter,
to minimize the number of packets that the application has to process. Since
the filter works at kernel level, an appropriate filter increases the
performances of the application and decreases the load on the system. In
this way a non interesting packet does not need to be transferred from kernel
to user space, avoiding the memory copy and the context switch between kernel
and user mode.
If bs_drop is greater than zero, the application is too slow and is loosing
packets. The programmer can try, as a first solution, to set a greater buffer
in the driver with the PacketSetBuff function. A proper dimension of the buffer
often decreases dramatically the packet loss. Another solution is to speed up
the capture process associating a bigger buffer with the PACKET structure used
in the PacketReceivePacket call (see the PacketInitPacket function). This
decreases the number of system calls, improving the speed.
If the application keeps on loosing packets, probably it should be rewritten or
optimized. The driver is already very fast, and probably it is better to modify
the application than the driver, where the main optimization that can be done
is the implementation of the word-alignment.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetType (LPADAPTER AdapterObject,NetType *type)
//------------------------------------------------------------------------------
PacketGetNetType: Function (AdapterObject:Padapter; nettype:Pnet_Type):LongBool; cdecl;
{Returns the type of the adapter pointed by AdapterObject in a NetType structure.
The LinkType of the type paramter can be set to one of the following values:

NdisMedium802_3: Ethernet (802.3)
NdisMedium802_5: Token Ring (802.5)
NdisMediumFddi: FDDI
NdisMediumWan: WAN
NdisMediumLocalTalk: LocalTalk
NdisMediumDix: DIX
NdisMediumArcnetRaw: ARCNET (raw)
NdisMediumArcnet878_2: ARCNET (878.2)
NdisMediumWirelessWan: Various types of NdisWirelessXxx media.
The LinkSpeed field indicates the speed of the network in Bits per second.

The return value is TRUE if the operation is performed successfully.
}

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetType (LPADAPTER AdapterObject,NetType *type)
//------------------------------------------------------------------------------
PacketSetReadTimeout: Function (AdapterObject:Padapter;timeout:integer):boolean; cdecl;
{Sets the timeout value for the given adapter. }

//------------------------------------------------------------------------------
// PCHAR PacketGetDriverVersion  (    )   3.1 and later
//------------------------------------------------------------------------------
PacketGetDriverVersion: function: PAnsiChar ; cdecl;

{   Return a string with the version of the NPF.sys device driver.
Returns:
A char pointer to the version of the driver. }

//------------------------------------------------------------------------------
// PCHAR PacketGetVersion  (    )
//------------------------------------------------------------------------------
PacketGetVersion: function: PAnsiChar ; cdecl;

{   Return a string with the dll version.
Returns:
A char pointer to the version of the library. }

//------------------------------------------------------------------------------
// BOOLEAN PacketGetNetInfoEx  (  PCHAR  AdapterName,  npf_if_addr *  buffer,  PLONG  NEntries )
//------------------------------------------------------------------------------
PacketGetNetInfoEx: function (AdapterName: PAnsiChar; Buffer: Pnpf_if_addr;
                                             NEntries: PInteger): boolean ; cdecl ;
//PacketGetNetInfoEx: function (AdapterName: PChar; Buffer: PChar; NEntries: integer): boolean ; cdecl ;

{  Returns comprehensive information the addresses of an adapter.
Parameters:
 AdapterName  String that contains the name of the adapter.
 buffer  A user allocated array of npf_if_addr that will be filled by the function.
 NEntries  Size of the array (in npf_if_addr).

Returns:
If the function succeeds, the return value is nonzero.
This function grabs from the registry information like the IP addresses,
the netmasks and the broadcast addresses of an interface. The buffer passed
by the user is filled with npf_if_addr structures, each of which contains the
data for a single address. If the buffer is full, the reaming addresses are dropped,
therefore set its dimension to sizeof(npf_if_addr) if you want only the first address. }

//------------------------------------------------------------------------------
// BOOLEAN PacketSetMinToCopy  (  LPADAPTER  AdapterObject,  int  nbytes )
//------------------------------------------------------------------------------
PacketSetMinToCopy: function (AdapterObject: Padapter ; nbytes: integer): boolean ; cdecl ;

{   Defines the minimum amount of data that will be received in a read.

Parameters:
 AdapterObject  Pointer to an _ADAPTER structure
 nbytes  the minimum amount of data in the kernel buffer that will cause the driver
  to release a read on this adapter.

Returns:
If the function succeeds, the return value is nonzero.
In presence of a large value for nbytes, the kernel waits for the arrival of several
packets before copying the data to the user. This guarantees a low number of system
calls, i.e. lower processor usage, i.e. better performance, which is a good setting
for applications like sniffers. Vice versa, a small value means that the kernel will
copy the packets as soon as the application is ready to receive them. This is suggested
for real time applications (like, for example, a bridge) that need the better
responsiveness from the kernel.
note: this function has effect only in Windows NTx. The driver for Windows 9x doesn't
offer this possibility, therefore PacketSetMinToCopy is implemented under these systems
only for compatibility. }


//------------------------------------------------------------------------------
// INT PacketSetSnapLen  (  LPADAPTER  AdapterObject,  int  snaplen ) 3.1 and later
//------------------------------------------------------------------------------
PacketSetSnapLen: function (AdapterObject: Padapter ; snaplen: integer): integer ; cdecl ;

{  Sets the snap len on the adapters that allow it.
 Parameters:
 AdapterObject  Pointer to an _ADAPTER structure.
 snaplen  Desired snap len for this capture.

Returns:
If the function succeeds, the return value is nonzero and specifies the actual snaplen
that the card is using. If the function fails or if the card does't allow to set snap
length, the return value is 0.
The snap len is the amount of packet that is actually captured by the interface and
received by the application. Some interfaces allow to capture only a portion of any
packet for performance reasons.

Note:
: the return value can be different from the snaplen parameter, for example some
boards round the snaplen to 4 bytes. }

//------------------------------------------------------------------------------

var
    PacketDllModule: THandle;
    function LoadPacketDll: Boolean;

implementation

function LoadPacketDll: Boolean;
begin
    Result := True;
    if PacketDllModule <> 0 then Exit;

// open DLL
    PacketDllModule := LoadLibrary (DLL);
    if PacketDllModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    PacketGetAdapterNames := GetProcAddress (PacketDllModule, 'PacketGetAdapterNames') ;
    PacketOpenAdapter := GetProcAddress (PacketDllModule, 'PacketOpenAdapter') ;
    PacketCloseAdapter := GetProcAddress (PacketDllModule, 'PacketCloseAdapter') ;
    PacketAllocatePacket := GetProcAddress (PacketDllModule, 'PacketAllocatePacket') ;
    PacketInitPacket := GetProcAddress (PacketDllModule, 'PacketInitPacket') ;
    PacketFreePacket := GetProcAddress (PacketDllModule, 'PacketFreePacket') ;
    PacketReceivePacket := GetProcAddress (PacketDllModule, 'PacketReceivePacket') ;
    PacketWaitPacket := GetProcAddress (PacketDllModule, 'PacketWaitPacket') ;
    PacketSendPacket := GetProcAddress (PacketDllModule, 'PacketSendPacket') ;
    PacketResetAdapter := GetProcAddress (PacketDllModule, 'PacketResetAdapter') ;
    PacketSetHwFilter := GetProcAddress (PacketDllModule, 'PacketSetHwFilter') ;
    PacketRequest := GetProcAddress (PacketDllModule, 'PacketRequest') ;
    PacketSetBuff := GetProcAddress (PacketDllModule, 'PacketSetBuff') ;
    PacketSetBpf := GetProcAddress (PacketDllModule, 'PacketSetBpf') ;
    PacketGetStats := GetProcAddress (PacketDllModule, 'PacketGetStats') ;
    PacketGetNetType := GetProcAddress (PacketDllModule, 'PacketGetNetType') ;
    PacketSetReadTimeout := GetProcAddress (PacketDllModule, 'PacketSetReadTimeout') ;
    PacketGetVersion := GetProcAddress (PacketDllModule, 'PacketGetVersion') ;
    PacketGetNetInfoEx := GetProcAddress (PacketDllModule, 'PacketGetNetInfoEx') ;
    PacketSetMinToCopy := GetProcAddress (PacketDllModule, 'PacketSetMinToCopy') ;
    PacketGetDriverVersion := GetProcAddress (PacketDllModule, 'PacketGetDriverVersion') ;  // 3.1 and later
    PacketSetSnapLen := GetProcAddress (PacketDllModule, 'PacketSetSnapLen') ;              // 3.1 and later
end;

initialization
    PacketDllModule := 0 ;
finalization
    if PacketDllModule <> 0 then
    begin
        FreeLibrary (PacketDllModule) ;
        PacketDllModule := 0 ;
    end ;
end.

