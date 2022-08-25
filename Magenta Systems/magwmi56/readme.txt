Magenta Systems WMI and SMART Component v5.6
============================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Compatible with Windows Vista, 2008, 7, 8, 2012, 10, 2016 and 2019.


Introduction
------------

Magenta Systems WMI and SMART Component contains WMI, SMART and SCSI Pass
Through functions, of particular use for getting hard disk information and
configuring network adaptors, but also for many other general uses.

MagWMI which allows access and update of windows system information using
Windows Management Instrumentation.  MagWMI provides general view access
to any WMI information using SQL like commands, and also a number of
dedicated function relating to TCP/IP configuration, such as setting the
adaptor IP addresses, the computer name, domain/workgroup, BIOS and disk
drive information.

There are also disk drive functions avoiding WMI, to map physical and SCSI
bus disk drives, getting disk information, sizes and serial numbers.
For ATA drives, SMART disk failing and fault information may be accessed.

Most functions return information in simple string arrays or structures,
for easy access in other applications.

A demo program illustrates the use of all the functions.

Note that WMI may be used to get information from both the local computer
and remote computers, the latter needed a valid logon.  WMI will only
run locally if the Windows Management Instrumentation service is running.
Running across a network also needs the RPC service running.


Function Overview
-----------------

MagWMI contains some general purpose functions for accessing any WMI class
information, and more specific functions for particular types of
information, which also illustrate how to use the former.  WMI often
returns significant qualities of information with multiple instance, for
instance multiple items about several disk drives.  To best return this
information, a two dimensional dynamic string array defined as:

T2DimStrArray = array of array of string ;

is used for WmiResults.  Functions are provided to search within this
array for specific rows of information.  The component converts all the
internal WMI data types to ASCII for the results.  Some result fields may
have multiple sub-results, which are pipe (|) delimited strings. The
result set may be thought of as being columns and rows in a grid, with
property names in the first column.  The demo program shows the results in
a multi-column TListView.

Some parameters are passed in a single dimension string array defined as:

StringArray = array of string;

This is used instead of TStrings so garbage collection is automatic.


General Purpose WMI Functions
-----------------------------

function MagWmiGetInfoEx (const Comp, NameSpace, User, Pass, Arg:  widestring ;
 var WmiResults: T2DimStrArray; var instances: integer; var errinfo: string): integer ;

This is the lowest level function for getting WMI information, it needs
a computer name (. for local computer), WMI name space (usually 'root\CIMV2'
or 'root\WMI'), logon user name and password (may be blank for local
computer) and the argument to pass to WMI which may be a simple WMI class
or a complex WMI WQL language query statement, see below for examples.
A variable WmiResults needs to be predefined for the results, see above,
and instances will return the number of columns of data, in addition to
column 0 being the properties in the data.  The function result is the
number of rows in the result, with 0 being the header.  Result -1 means
failure with the error in errinfo. The size of the dynamic array may
also be checked with Low/High functions and should match instances and
result + 1 each.

function MagWmiGetInfo (const Comp, NameSpace, User, Pass, Arg:  widestring ;
            var WmiResults: T2DimStrArray; var instances: integer): integer ;

Similar to MagWmiGetInfoEx, but does not return errinfo.


function MagWmiGetOneQ (const Arg, Prop: widestring ;
                                          var ResStr: string): integer ;

This is a simple function to get a single property for an argument on the
local computer only, it Arg is a WQL SELECT statement that should return
only a single instance, ResStr will then be filled from the field
specified by Prop.  Result is -1 for failure.


function MagWmiSearchIdx (const WmiResults: T2DimStrArray;
                                         const Prop: string): integer ;

This function searches the two dimensional array result from MagWmiGetInfo
for the row of a specific property, or zero if not found.


function MagWmiSearch1 (const WmiResults: T2DimStrArray;
                                    const Prop: string): string ;

This function searches the two dimensional array result from MagWmiGetInfo
for the value of a specific property, blank if not found.


Specific WMI Functions
----------------------

Note most of functions work for the local computer only, to avoid passing
more parameters than necessary.


function MagWmiGetBaseBoard: string ;

Returns the motherboard manufacturer and product name.


function MagWmiGetSMBIOS: string ;

Returns BIOS version information.


function MagWmiGetLastBootDT: TDateTime ;

Returns the date and time of the last PC boot.


function MagWmiGetDiskSerial (drive: integer): string ;

Returns the serial number for a specified physical disk drive, where
0 is the first drive, 1 the second, etc.  Note this function only
operates on Windows XP and later.  Use SMART or SCSI functions for
Windows 2000 or drives that return a blank.


function MagWmiGetDiskModel (drive: integer): string ;

Returns the disk model name for a specified physical disk drive, where
0 is the first drive, 1 the second, etc.


function MagWmiGetOSInfo (item: string): string ;

Returns a specific property from the Win32_OperatingSystem class, ie
SerialNumber, Locale, RegisteredUser, BootDevice, etc.


function MagWmiGetProcInfo (item: string): string ;

Returns a specific property from the Win32_Processor class, ie Version,
Manufacturer, ProcessorId, etc.


function MagWmiFindAdaptor (var AdapterName: string): integer ;

Returns the name and index of a the first unique enabled Ethernet 802.3
adaptor (except 1394 Net Adapter), ignoring hidden adapters.  Generally,
most PCs only have one working network connection and adaptor, and this
function will find it.  If there are multiple working adaptors, you'll
need to check the names more carefully, particularly if there are
duplicate names.


function MagWmiNetSetIPAddr (const AdapNum: integer; const IPAddresses,
                                     SubnetMasks: StringArray): integer ;

Changes the static IP addresses and subnet masks for a specific adaptor
number, found using MagWmiFindAdaptor.  Note the addresses are passed in
dynamic StringArrays so that multiple IP addresses and matching masks can
be configured. Return is -1 for error, 0 OK no reboot needed, 1 reboot
needed, 68 bad parameters, 84 using wrong adaptor.  This function
will disable DHCP.


function MagWmiNetSetGateway (const AdapNum: integer; const IPGateways:
            StringArray; const GatewayCosts: TIntegerArray): integer ;

Changes the static gateway IP addresses and costs for a specific adaptor
number, found using MagWmiFindAdaptor.  Note the addresses are passed in
dynamic StringArrays so that multiple IP addresses and matching costs can
be configured. Return is -1 for error, 0 OK no reboot needed, 1 reboot
needed, 68 bad parameters, 84 using wrong adaptor.  This function fails
if DHCP is enabled.


function MagWmiNetSetDHCP (const AdapNum: integer): integer ;

Enables DHCP for a specific adaptor number,  found using MagWmiFindAdaptor,
so it loses static IPs. Return is -1 for error, 0 OK no reboot needed, 1
reboot needed, 84 using wrong adaptor.


function MagWmiRenameComp (const NewName, UserName, Password:
                                                    string): integer ;

This functions allows the local computer to be renamed, it always needs
logon information.  This only works on Windows XP and later, and a PC
reboot is needed before the new name is recognised.


function MagWmiCloseWin (const Comp, User, Pass: widestring ; reboot:
                                boolean; var errinfo: string): integer ;

This function allows the local or remote PC to be closed down or rebooted.


function MagWmiGetMemory: TWmiMemoryRec ;

This function returns a record containing seven different memory sizes

    TWmiMemoryRec = Record
        FreePhysicalMemory: Int64 ;
        FreeSpaceInPagingFiles: Int64 ;
        FreeVirtualMemory: Int64 ;
        SizeStoredInPagingFiles: Int64 ;
        TotalSwapSpaceSize: Int64 ;
        TotalVirtualMemorySize: Int64 ;
        TotalVisibleMemorySize: Int64 ;
    end ;


function MagWmiSmartDiskInfo  (drivenr: integer; const deviceid: string;
                                    var DriveResult: TDriveResult): boolean ;

This function returns disk drive information for all physical disk drives,
including the bus to which they are connected, product, vendor, serial number,
capacity, etc.  For drives connected to ATA controllers, it also includes
ATA and SATA versions and speed and the full IdentifyDevice structure for all
other parameters. Still investigating getting drive data via Intel RAID
controllers using CSMI commands to see the drives making up arrays.
DriveNr is base 0, DeviceId may be left blank, or a Windows driver device
specified, ie \\.\PhysicalDrive0 for drive 0.

    TDriveResult = Record
        DriveNum: integer ;
        DeviceId: string ;
        BusTypeDisp: string ; // ATA. SCSI, USB, CSMI
        DevTypeDisp: string ; // disk, cd-rom, etc,
        ErrInfo: string ;
        VendorId: string ;
        ProductId: string ;
        ProductRev: string ;
        BusType: integer ;
        DeviceType: integer ;
        MediaType: integer ;
        VendorUnique: array [1..4] of USHORT ;
        SerialNumber: string ;
        FirmwareRev: string ;
        ModelNumber: string ;
        RemoveMedia: boolean ;
        SmartSupport: boolean ;
        SmartEnabled: boolean ;
        LBA48Support: boolean ;
        CapacityNum: int64 ;
        SectorNum: int64 ;
        MajorRev: integer ;  // ATA versions
        MinorRev: integer ;  // minor ATA versions
        AtaVersion: string ;
        SataVersion: string ;
        SecSizeLogical: integer ;
        SecSizePhysical: integer ;
        IdentifyDevice: TIdentifyDeviceData ;
    end ;


function MagWmiSmartDiskFail (drivenr: integer; var DriveResult: TDriveResult;
                                       var SmartResult: TSmartResult): boolean ;

This function returns similar information to MagWmiSmartDiskInfo. For drives
connected to ATA buses, it also returns a SMART result structure. SMART
information is really a historic record of drive usage and particular
attributes saved by the drive, such as maximum temperature, running time,
error rates, etc. Different manufacturers save differing attributes, using
different units and techniques, so making use of the results can be difficult.
See below for examples of the attribute results.

    TSmartResult = Record
        DriveNum: integer ;
        Temperature: integer ;
        TempWorst: integer ;
        TempLow: integer ;
        HoursRunning: integer ;
        ReallocSector: integer ;
        PowerCycles: integer ;
        SmartFailTot: integer ;
        SmartWarnTot: integer ;
        TotalAttrs: integer ;
        AttrNum: array [0..MaxSmartAttr] of integer ;
        AttrName: array [0..MaxSmartAttr] of string ;
        AttrPreFail: array [0..MaxSmartAttr] of boolean ;
        AttrEvents: array [0..MaxSmartAttr] of boolean ;
        AttrErrorRate: array [0..MaxSmartAttr] of boolean ;
        AttrCurValue: array [0..MaxSmartAttr] of integer ;
        AttrWorstVal: array [0..MaxSmartAttr] of integer ;
        AttrThreshold: array [0..MaxSmartAttr] of integer ;
        AttrRawValue: array [0..MaxSmartAttr] of Int64 ;
        AttrState: array [0..MaxSmartAttr] of string ;
    end ;


function MagWmiMapDrives (MaxNum: integer; var DriveResults: TDriveResults): integer ;

This function is designed to map all physical disk drives, it should give a
similar result to the WMI Win32_DiskDrive class.  Specify the maximum number of
drives to check, typically six to 12 (including USB memory sticks).  The function
simply calls MagWmiSmartDiskInfo repeatedly and returns a dynamic array of
DeviceResults with the function result being the total number of drives mapped.


function MagWmiSmartScsiBus (MaxNum: integer; var DriveResults: TDriveResults): integer ;

This function is designed to map all storage devices on SCSI buses which may be
disks, floppies, tapes, DVD-ROMs, etc, including all ATA and SATA devices which
internally Windows sees as SCSI. Specify the maximum number of SCSI adaptors to
check, typically six to 12.  Each adaptor may have multiple buses, with multiple
devices.  The function returns a dynamic array DeviceResults with the function
result being the total number of drives mapped.  DeviceId shows the adaptor,
ie SCSI4: while DriveNum is the port on the bus of that adaptor.  Currently it
only returns Vendor and Product Ids. it attempts to get drive information but
this seems to fail for most adaptors and when it works it return dubious
information.  Note this function is a work in progress.  Not currently supporting
SMART on these devices.


Example WMI Class and SELECT Statements
---------------------------------------

Listed below are the WMI class and WQL language select statements provided
by the demo program (in the drop down box).  Look at the Microsoft WMI
documentation for hundreds more classes.  Using a class on it's own
returns all information for all instances of the class.  A WQL select
statement (similar syntax to SQL) allows just the specifically required
data to be returned.

Win32_OperatingSystem
Win32_Keyboard
Win32_AutochkSetting
Win32_DiskDrive
Win32_DiskDrivePhysicalMedia
Win32_PhysicalMedia
Win32_LogicalDisk
Win32_MappedLogicalDisk
Win32_TapeDrive
Win32_BaseBoard
Win32_BIOS
Win32_Bus
Win32_DeviceSettings
Win32_IDEController
Win32_MemoryDevice
Win32_PhysicalMemory
Win32_PNPDevice
Win32_PortConnector
Win32_Processor
Win32_SerialPort
Win32_SerialPortConfiguration
Win32_SerialPortSetting
Win32_SoundDevice
Win32_SystemBIOS
Win32_USBController
Win32_USBControllerDevice
Win32_NetworkAdapter
Win32_NetworkAdapterConfiguration
Win32_NetworkAdapterSetting
Win32_Printer
Win32_PrinterConfiguration
Win32_POTSModem
Win32_POTSModemToSerialPort
Win32_DesktopMonitor
Win32_DisplayConfiguration
Win32_VideoController
Win32_VideoSettings
Win32_ActiveRoute
Win32_IP4PersistedRouteTable
Win32_IP4RouteTable
Win32_NetworkClient
Win32_NetworkConnection
Win32_BootConfiguration
Win32_ComputerSystem
Win32_MotherboardDevice
Win32_SystemUsers
Win32_SystemTimeZone
Win32_SystemSetting
Win32_Account
Win32_UserAccount
Win32_SystemAccount
Win32_Group
Win32_LogonSession
Win32_ComputerSystemWindowsProductActivation
Win32_WindowsProductActivation
Win32_FontIntoAction
Win32_ScheduledJob
Win32_Process
Win32_ServiceControl
Win32_Product
Win32_TemperatureProbe
Win32_StartupCommand
Win32_BaseService
Win32_Service
Win32_Desktop
Win32_Environment
Win32_LogicalProgramGroup
Win32_ProgramGroup
Win32_UninterruptiblePowerSupply
Win32_Battery
Win32_PortableBattery
Win32_CurrentProbe
Win32_VoltageProbe
Win32_PerfFormattedData_RemoteAccess_RASTotal
Win32_PerfFormattedData_RemoteAccess_RASPort
Win32_PerfRawData_RemoteAccess_RASPort
SELECT * FROM Win32_OperatingSystem
SELECT * FROM Win32_NetworkAdapter WHERE ConfigManagerErrorCode = 0
SELECT Name, MACAddress, ConfigManagerErrorCode, NetConnectionID
    FROM Win32_NetworkAdapter WHERE AdapterType = 'Ethernet 802.3'
SELECT Name, Model, InterfaceType, MediaType, Size from Win32_DiskDrive
SELECT Name, Description, DriveType, FileSystem, FreeSpace, Size,
    VolumeSerialNumber from Win32_LogicalDisk
SELECT Description, IPAddress, IPSubnet, IPConnectionMetric,
    MACAddress, DefaultIPGateway FROM Win32_NetworkAdapterConfiguration
    WHERE DefaultTTL > 1
(following are in Namespace root\wmi)
MSNdis_HardwareStatus
MSNdis_80211_TransmitPowerLevel
MSNdis_80211_ReceivedSignalStrength
MSNdis_MediaConnectStatus
MSTapeDriveParam
MSRedbook_DriverInformation
MSSerial_PortName
MSStorageDriver_FailurePredictStatus
MSStorageDriver_ATAPISmartData


Demonstration Application
-------------------------

A Windows demonstration application TESTWMI1.EXE is supplied, with source
and compiled program.  It illustrates the use of almost all the functions
described above.  Beware some are dangerous, such as rebooting the
computer, changing the IP address and renaming the computer. The edit
box for Get Class or Query contains a drop down with the lots of classes,
but others can be entered for testing.

SMART Results
-------------

Below are two sample results generated from the MagWmiSmartDiskFail function.

----------------------------------------------------------------------------------------------------
Drive 0 IC25N040ATMR04-0; Serial Number MRG254K2FUMHYP; Capacity 37.3G
SMART Test Passed OK

Attribute Name                 State       Cur Value  Worst Value  Threshold  Raw Value  Pre-Fail
1   Raw Read Error Rate        OK             97         97            62     196,611      Yes
2   Throughput Performance     OK             100        100           40     0            Yes
3   Spin Up Time               OK             158        158           33                  Yes
4   Start/Stop Count           -              99         99            0      2,696        No
5   Reallocated Sector Count   OK             100        100           5      0            Yes
7   Seek Error Rate            OK             100        100           67     0            Yes
8   Seek Time Performance      OK             100        100           40     0            Yes
9   Power On Count             -              94         94            0      2,912        No
10  Spin Retry Count           OK             100        100           60     0            Yes
12  Power Cycle Count          -              99         99            0      2,648        No
191 Unknown                    -              95         95            0      524,298      No
192 Emergency Retract Cycle    -              98         98            0      454          No
193 Load Cycle Count           -              97         97            0      39,926       No
194 Temperature Celsius        -              166        166           0                   No
196 Reallocation Event Count   -              100        100           0      237          No
197 Current Pending Sector     -              100        100           0      44           No
198 Off-line Uncorrectable     -              100        100           0      0            No
199 Ultra ATA CRC Error Rate   -              200        200           0      5            No

----------------------------------------------------------------------------------------------------
Drive 1 TOSHIBA MK1031GAS; Serial Number 75CQ1825S; Capacity 93.2G
SMART Test Passed OK

Attribute Name                 State       Cur Value  Worst Value  Threshold  Raw Value  Pre-Fail
1   Raw Read Error Rate        OK             100        100           50     0            Yes
2   Throughput Performance     OK             100        100           50     0            Yes
3   Spin Up Time               OK             100        100           1      1,272        Yes
4   Start/Stop Count           -              100        100           0      167          No
5   Reallocated Sector Count   OK             100        100           50     0            Yes
7   Seek Error Rate            OK             100        100           50     0            Yes
8   Seek Time Performance      OK             100        100           50     0            Yes
9   Power On Count             -              99         99            0      562          No
10  Spin Retry Count           OK             103        100           30     0            Yes
12  Power Cycle Count          -              100        100           0      167          No
192 Emergency Retract Cycle    -              100        100           0      2            No
193 Load Cycle Count           -              100        100           0      2,861        No
194 Temperature Celsius        -              100        100           0                   No
196 Reallocation Event Count   -              100        100           0      0            No
197 Current Pending Sector     -              100        100           0      0            No
198 Off-line Uncorrectable     -              100        100           0      0            No
199 Ultra ATA CRC Error Rate   -              200        200           0      0            No
220 Unknown                    -              100        100           0      74           No
222 Unknown                    -              99         99            0      415          No
223 Unknown                    -              100        100           0      0            No
224 Unknown                    -              100        100           0      0            No
226 Unknown                    -              100        100           0      346          No
240 Unknown                    OK             100        100           1      0            Yes
----------------------------------------------------------------------------------------------------


Files Enclosed
--------------

=MagWMI Demo Application
wmimain.dfm
wmimain.pas
testwmi1.dpr
testwmi1.exe

=MagWmi Component
magwmi.pas

=Support files
magsubs1.pas
WbemScripting_TLB.pas
smartapi.pas


Changes
-------

Release 5.1 - Removed widestrings for better compatibility with unicode in Delphi
2009, and using PAnsiChars and Bytes where necessary.  Tested with Delphi 2009.

Release 5.2 - Fixed memory leaks with OleVariants, thanks to Andy Whittles and
Luke Painter, added MagWmiGetInfoEx which returns exception error string as well
as -1 for better error handling.

Release 5.3 - Fixed some string cast warnings for Delphi 2009 and later
              Updates subroutines for Win64 support

Release 5.4 - 23rd January 2013

1- All ATA, SCSI, RAID, SATA and USB disk drive information is returned in a
TDriveResult structure, which for drives connected to ATA controllers includes
ATA and SATA versions and speed and the full IdentifyDevice structure for all
other parameters. Still investigating getting drive data via Intel RAID
controllers using CSMI commands to see the drives making up arrays.

2 - The SMART APIs were designed for PCs with only four IDE ATA drives and drives
less than 128 gig, whereas SATA PCs may have six or more drives. Removed support
for Win95/98, ignore bIDEDeviceMap with maximum four drives, support more than four
drives in MagWmiSmartDiskInfo and MagWmiSmartDiskFail.  Note that SMART failure
results are currently only available for disk drives connected to ATA controllers,
not for USB. RAID or SCSI controllers.  Some SATA controllers are seen my Windows
as SCSI, not ATA.

3 - Added MagWmiMapDrives which returns details of all physical disk drives in
an array of TDriveResult structures.

4 - Removed MagWmiScsiDiskInfo now part of MagWmiSmartDiskInfo.

5 - Added MagWmiSmartScsiBus which returns details of all devices on SCSI buses,
which may be disks, DVD-ROMs, etc, including all ATA devices, not currently supporting
SMART on these devices.

6 - Added a new WMI namespace of root\wmi which contains hundreds of new classes, mostly
related to Windows Server components, but also MSNdis_80211 and other network controllers.


Release 5.5 - 5th August 2013

1 - Fixed another WMI memory leak with OleVariants, thanks to Ekkehard Domning and anon.

2 - Tested with Delphi XE4.


Release 5.6 - 26 November 2018

1 - Added power related classes, ie Win32_Battery

2 - Tested with Delphi 10.3 Rio.



Copyright Information
---------------------

Magenta Systems WMI and SMART Component  is freeware, but is still
copyrighted by Magenta Systems Ltd who may change the status or
withdraw it at any time, without notice.

Magenta Systems WMI and SMART Component  may be freely distributed via
web pages, FTP sites, BBS and conferencing systems or on CD-ROM in
unaltered zip format, but no charge may be made other than
reasonable media or bandwidth cost.

Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: http2://www.magsys.co.uk/delphi/



