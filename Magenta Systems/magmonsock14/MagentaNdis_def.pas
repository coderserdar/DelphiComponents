{ Magenta Systems Internet Packet Monitoring Components

********************************************************************************
--------------------------------------------------------------------------------
                                NDIS DEFINITIONS
                       Collection of Types/consts/macros
                  originally for the Plibcap function library

                         - By Lars Peter Christiansen
--------------------------------------------------------------------------------
 If you have any questions, requests, bug reports, etc., please contact me at
 the address given below.

Lars Peter Christiansen
Email  : lp@nzlab.dk
Website: http://www.nzlab.dk

Plibcap.c author:
old website: http://netgroup-serv.polito.it/windump
new website: http://www.winpcap.org/

Updated by Angus Robertson, Magenta Systems Ltd, England, v1.3 13th August 2010
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Some parts Copyright Magenta Systems Ltd

Angus - 29th October 2005 - added 802.3 Objects (Ethernet)
8 Aug 2008 - 1.2 - updated to support ICS V6 and V7, and Delphi 2009

--------------------------------------------------------------------------------

********************************************************************************
}
unit MagentaNdis_def;

interface
{
[Copied from Ndis.h]
the following constants are to be used to direct the
underlying NIC driver to choose which type of packet can be delivered to the
upper bound driver, that is, our snoop driver.
}
const
  NDIS_PACKET_TYPE_DIRECTED =           $0001;
  NDIS_PACKET_TYPE_MULTICAST =          $0002;
  NDIS_PACKET_TYPE_ALL_MULTICAST =      $0004;
  NDIS_PACKET_TYPE_BROADCAST =          $0008;
  NDIS_PACKET_TYPE_SOURCE_ROUTING =     $0010;
  NDIS_PACKET_TYPE_PROMISCUOUS =        $0020; //for snoop
  NDIS_PACKET_TYPE_SMT =                $0040;
  NDIS_PACKET_TYPE_MAC_FRAME =          $8000;
  NDIS_PACKET_TYPE_FUNCTIONAL =         $4000;
  NDIS_PACKET_TYPE_ALL_FUNCTIONAL =     $2000;
  NDIS_PACKET_TYPE_GROUP =              $1000;

Type

   // Taken from NTDDNDIS.H
     TNDIS_MEDIUM = (NdisMedium802_3,
                     NdisMedium802_5,
                     NdisMediumFddi,
                     NdisMediumWan,
                     NdisMediumLocalTalk,
                     NdisMediumDix, // defined for convenience, not a real medium
                     NdisMediumArcnetRaw,
                     NdisMediumArcnet878_2,
                     NdisMediumAtm,
                     NdisMediumWirelessWan,
                     NdisMediumIrda,
                     NdisMediumMax );


 DEVICE_TYPE  = LONGWORD;


Const

{  These can be un-remarked if any use is necessary.

 FILE_DEVICE_BEEP               = 1;
 FILE_DEVICE_CD_ROM             = 2;
 FILE_DEVICE_CD_ROM_FILE_SYSTEM = 3;
 FILE_DEVICE_CONTROLLER         = 4;
 FILE_DEVICE_DATALINK           = 5;
 FILE_DEVICE_DFS                = 6;
 FILE_DEVICE_DISK               = 7;
 FILE_DEVICE_DISK_FILE_SYSTEM   = 8;
 FILE_DEVICE_FILE_SYSTEM        = 9;
 FILE_DEVICE_INPORT_PORT        = $a;
 FILE_DEVICE_KEYBOARD           = $b;
 FILE_DEVICE_MAILSLOT           = $0c;
 FILE_DEVICE_MIDI_IN            = $0d;
 FILE_DEVICE_MIDI_OUT           = $0e;
 FILE_DEVICE_MOUSE              = $0f;
 FILE_DEVICE_MULTI_UNC_PROVIDER = $10;
 FILE_DEVICE_NAMED_PIPE         = $11;
 FILE_DEVICE_NETWORK            = $12;
 FILE_DEVICE_NETWORK_BROWSER    = $13;
 FILE_DEVICE_NETWORK_FILE_SYSTEM= $14;
 FILE_DEVICE_NULL               = $15;
 FILE_DEVICE_PARALLEL_PORT      = $16;
 FILE_DEVICE_PHYSICAL_NETCARD   = $17;
 FILE_DEVICE_PRINTER            = $18;
 FILE_DEVICE_SCANNER            = $19;
 FILE_DEVICE_SERIAL_MOUSE_PORT  = $1a;
 FILE_DEVICE_SERIAL_PORT        = $1b;
 FILE_DEVICE_SCREEN             = $1c;
 FILE_DEVICE_SOUND              = $1d;
 FILE_DEVICE_STREAMS            = $1e;
 FILE_DEVICE_TAPE               = $1f;
 FILE_DEVICE_TAPE_FILE_SYSTEM   = $20;
 FILE_DEVICE_TRANSPORT          = $21;
 FILE_DEVICE_UNKNOWN            = $22;
 FILE_DEVICE_VIDEO              = $23;
 FILE_DEVICE_VIRTUAL_DISK       = $24;
 FILE_DEVICE_WAVE_IN            = $25;
 FILE_DEVICE_WAVE_OUT           = $26;
 FILE_DEVICE_8042_PORT          = $27;
 FILE_DEVICE_NETWORK_REDIRECTOR = $28;
 FILE_DEVICE_BATTERY            = $29;
 FILE_DEVICE_BUS_EXTENDER       = $2a;
 FILE_DEVICE_MODEM              = $2b;
 FILE_DEVICE_VDM                = $2c;
 FILE_DEVICE_MASS_STORAGE       = $2d;
}




// Object Identifiers used by NdisRequest Query/Set Information
//
// Taken from PACKET95\INC\ntddndis.h
//
//
// General Objects
OID_GEN_CURRENT_PACKET_FILTER	   = $0001010E;
OID_GEN_MEDIA_IN_USE			   = $00010104;
OID_GEN_LINK_SPEED			       = $00010107;

// 802.3 Objects (Ethernet)
//
OID_802_3_PERMANENT_ADDRESS		   = $01010101;
OID_802_3_CURRENT_ADDRESS		   = $01010102;
OID_802_3_MULTICAST_LIST		   = $01010103;
OID_802_3_MAXIMUM_LIST_SIZE		   = $01010104;
OID_802_3_MAC_OPTIONS			   = $01010105;


// Defines the method codes for how buffers are passed for I/O and FS controls
METHOD_BUFFERED                 =0;
METHOD_IN_DIRECT                =1;
METHOD_OUT_DIRECT               =2;
METHOD_NEITHER                  =3;

// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
FILE_ANY_ACCESS           =0;
FILE_READ_ACCESS          =1;	// file & pipe
FILE_WRITE_ACCESS         =2;	// file & pipe

// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.

{  NOTE : CANNOT USE THESE MAKROS IN DELPHI! USE THE PRE-INITED VARS INSTEAD!

IOCTL_PROTOCOL_QUERY_OID  =  CTL_CODE(FILE_DEVICE_PROTOCOL, 0 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_SET_OID    =  CTL_CODE(FILE_DEVICE_PROTOCOL, 1 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_STATISTICS =  CTL_CODE(FILE_DEVICE_PROTOCOL, 2 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_RESET      =  CTL_CODE(FILE_DEVICE_PROTOCOL, 3 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_READ       =  CTL_CODE(FILE_DEVICE_PROTOCOL, 4 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_WRITE      =  CTL_CODE(FILE_DEVICE_PROTOCOL, 5 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_MACNAME    =  CTL_CODE(FILE_DEVICE_PROTOCOL, 6 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_OPEN                =  CTL_CODE(FILE_DEVICE_PROTOCOL, 7 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_CLOSE               =  CTL_CODE(FILE_DEVICE_PROTOCOL, 8 , METHOD_BUFFERED, FILE_ANY_ACCESS);
}
FILE_DEVICE_PROTOCOL      =  $8000;


var
IOCTL_PROTOCOL_QUERY_OID,
IOCTL_PROTOCOL_SET_OID,
IOCTL_PROTOCOL_STATISTICS,
IOCTL_PROTOCOL_RESET,
IOCTL_PROTOCOL_WRITE,
IOCTL_PROTOCOL_MACNAME,
IOCTL_CLOSE,
IOCTL_OPEN,
IOCTL_PROTOCOL_READ        : LongWord;



function CTL_CODE(Device,Func,Method,Access:LongWord):LongWord;
implementation
{ The original C Macro :

#define CTL_CODE( DeviceType, Function, Method, Access )
  (
    ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method)
  )

}
function CTL_CODE(Device,Func,Method,Access:LongWord):LongWord;
begin
 result :=( (Device shl 16) OR (Access shl 14) OR (func shl 2) OR method );
end;

initialization

IOCTL_PROTOCOL_QUERY_OID  :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 0 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_SET_OID    :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 1 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_STATISTICS :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 2 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_RESET      :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 3 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_READ       :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 4 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_WRITE      :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 5 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_PROTOCOL_MACNAME    :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 6 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_OPEN                :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 7 , METHOD_BUFFERED, FILE_ANY_ACCESS);
IOCTL_CLOSE               :=  CTL_CODE(FILE_DEVICE_PROTOCOL, 8 , METHOD_BUFFERED, FILE_ANY_ACCESS);

end.
