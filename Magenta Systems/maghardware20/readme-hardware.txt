Magenta Hardware Components v2.0
================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 2nd February 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd


Introduction
------------

Magenta Hardware Components is a set of six main components for Delphi 2007
to Delphi 11.0 and later, as follows:

1 - Magenta Serial Port Detection Component which contains serial COM port
enumeration functions, using several methods which can identify different
ports depending on how they are installed, all are combined and a sorted
array returned with friendly names and install information.

2 - Magenta Hardware Events Component that listens for Windows hardware
event messages and calls events handlers for device changes such as serial
ports, disk volume changes, low disk space events and power events.

3 - Directory Changes Monitoring Component, that notifies changes in a
directory such as file or directory Create/Delete/Modify/Rename.

4 - Magenta GPS and Location Component is designed to process GPS location
data from various sources with an event triggered when movement is detected.
GPS sources supported include Windows Location API, NMEA 0183 sentences,
GT02 GPS Tracker Protocol, TK102/103 Tracker Protocol and WondeX/TK5000
Tracker Protocol. Sample shows movement track on a Google map.

5 - Magenta Firewall Component has functions to search and list selected
Windows Defender Firewall rules and settings, and to add and remove such
rules.  There is also some code that may be used in Inno Setup scripts to
set-up firewall rules during application installation.

6 - Magenta Check Disk and Format Disk component used to format fixed or
removable disk drives and perform disk checks on Windows NT4 and later.


Sample Applications
-------------------

There are four sample applications to test and demonstrate the various
components, with binaries included in the distribution, built with
Delphi 11.0 and tested on Windows 11.

signals.dpr, signmain.pas/dfm test the TSerialPorts component in
MagSerPorts.pas, the THardwareEvents component in MagHardwareEvents.pas,
and the TDirChange component in MagDirChange.pas.  It needs the Async Pro
serial port component to be installed from https://github.com/TurboPack/AsyncPro
or GetIt to open serial ports.

sensortest.dpr, sensormain.pas/dfm, sensormap/sensoredgemap.pas/dfm test the
TMagGpsLoc component in MagGpsLoc.pas, it needs Async Pro (see above) to
access serial port devices and Internet Component Suite (ICS) from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download to access TCP/IP and
UDP/IP devices.  There are map units for MSIE or Edge (Delphi 10.4 and
later).

firewalltest.dpr, firewallmain.pas/dfm test functions in MagFirewall.pas
which uses COM Automation.

diskfmt.dpr, fmtmain.pas/dfm test the TMagFmtChkDsk component in
MagFmtDisk.pas.


Magenta Serial Port Detection Component
---------------------------------------

This unit contains serial COM port enumeration functions, using several
methods which can identify different ports depending on how they are
installed, all are combined and a sorted array returned with friendly
names and install information.

The EnumSerialPortsEx function does the following, in order:

1 - Enumerate HLM\HARDWARE\DEVICEMAP\SERIALCOMM registry
2 - Enumerate Device Installation Class 'Ports' which finds most serial
    ports including USB
3 - Optionally add Disabled 'Ports' (hardware not currently installed)
4 - Enumerate Device Installation Class 'CNCPorts' (used by com0com
    serial port emulator)
5 - Enumerate Device Installation Class 'Modem' which finds USB and
    internal modems

The function returns a dynamic array of records:

 TSerialPort = record
    ComName: string ;
    NumPort: string ;
    IntName: string ;
    FriendlyName: string ;
    Desc: string ;
    Manufacturer: string ;
    HardwareId: string ;
    Location: string ;
    StatInfo: string ;
    Enabled: boolean ;
 end;

The array contains the COM port name, friendly Windows description,
manufacturer name, internal name from registry, hardware ID, location and
whether the port is enabled (USB ports may be installed but unplugged).
Note this information may be filled in different ways by different hardware
vendors for different classes. For example on one development PC:

COM1, Enabled=Y, Communications Port (COM1), (Standard port types), Serial0, ACPI\PNP0501,
COM3, Enabled=N, Prolific USB-to-Serial Comm Port (COM3), Prolific, , USB\VID_067B&PID_2303&REV_0400, Port_#0001.Hub_#0003
COM4, Enabled=Y, HHD Software Virtual Serial Port (COM4), HHD Software Ltd., 0000005e, HHDVIRT\HHDVySer,
COM5, Enabled=Y, HHD Software Virtual Serial Port (COM5), HHD Software Ltd., 0000005f, HHDVIRT\HHDVySer,
COM6, Enabled=Y, HHD Software Virtual Serial Port (COM6), HHD Software Ltd., 00000060, HHDVIRT\HHDVySer,
COM11, Enabled=Y, D-Link DU-562M External Modem, CXT, Winachsf0, USB\VID_0572&PID_1300&REV_0100, Port_#0007.Hub_#0004
COM12, Enabled=Y, Enhanced Communication Port (COM12), Oxford Semiconductor, OXMF0, OXMF\*PNP0501, oxmf bus, port 0
COM13, Enabled=Y, Enhanced Communication Port (COM13), Oxford Semiconductor, OXMF3, OXMF\*PNP0501, oxmf bus, port 3
COM14, Enabled=Y, Enhanced Communication Port (COM14), Oxford Semiconductor, OXMF2, OXMF\*PNP0501, oxmf bus, port 2
COM15, Enabled=Y, Enhanced Communication Port (COM15), Oxford Semiconductor, OXMF1, OXMF\*PNP0501, oxmf bus, port 1
COM16, Enabled=Y, Enhanced Communication Port (COM16), Oxford Semiconductor, OXMF6, OXMF\*PNP0501, oxmf bus, port 2
COM17, Enabled=Y, Enhanced Communication Port (COM17), Oxford Semiconductor, OXMF5, OXMF\*PNP0501, oxmf bus, port 1
COM18, Enabled=Y, Enhanced Communication Port (COM18), Oxford Semiconductor, OXMF4, OXMF\*PNP0501, oxmf bus, port 0
COM19, Enabled=Y, Enhanced Communication Port (COM19), Oxford Semiconductor, OXMF7, OXMF\*PNP0501, oxmf bus, port 3
COM20, Enabled=Y, Prolific USB-to-Serial Comm Port (COM20), Prolific, ProlificSerial1, USB\VID_067B&PID_2303&REV_0400, Port_#0002.Hub_#0003
COM24, Enabled=Y, Standard Serial over Bluetooth link (COM24), Microsoft, BthModem0, BTHENUM\00001101-0000-1000-8000-00805f9b34fb_VID&0001000f_PID&1200,
COM26, Enabled=Y, USB Serial Port (COM26), FTDI, VCP0, FTDIBUS\COMPORT&VID_0403&PID_6001,
COM27, Enabled=N, USB-SERIAL CH340 (COM27), wch.cn, , USB\VID_1A86&PID_7523&REV_0254, Port_#0001.Hub_#0003
COM31, Enabled=N, Prolific USB-to-Serial Comm Port (COM31), Prolific, , USB\VID_067B&PID_2303&REV_0400, Port_#0003.Hub_#0006
COM32, Enabled=Y, Prolific USB-to-Serial Comm Port (COM32), Prolific, ProlificSerial0, USB\VID_067B&PID_2303&REV_0400, Port_#0003.Hub_#0005
CNCA0, Enabled=Y, com0com - serial port emulator (CNCA0), Vyacheslav Frolov, com0com10, com0com\port, CNCA0
CNCB0, Enabled=Y, com0com - serial port emulator (CNCB0), Vyacheslav Frolov, com0com20, com0com\port, CNCB0

On this PC, COM3 to COM10 were originally another 8-way serial card since
removed which is the reason for the missing numbers. For the 8-way card,
the internal name correctly describes the labelled ports (0 to 7) which the
location fails to do. Note some virtual ports are installed with alphabetic
names, ie CNCA0.

On a newer PC under Windows 11 (note needs Prolific WDF WHQL Driver: v3.9.1.0):

COM1, Enabled=Y, Communications Port (COM1), (Standard port types), Serial0, ACPI\VEN_PNP&DEV_0501,
COM2, Enabled=Y, PCIe to High Speed Serial Port (COM2), ASIX Electronics Corporation, StnSerial0, MCS9950MF\STN_CASCADE_COM,
COM3, Enabled=Y, PCIe to High Speed Serial Port (COM3), ASIX Electronics Corporation, StnSerial1, MCS9950MF\STN_CASCADE_COM,
COM4, Enabled=Y, Prolific USB-to-Serial Comm Port (COM4), Prolific, ProlificSerial0, USB\VID_067B&PID_2303&REV_0400, Port_#0004.Hub_#0007
COM5, Enabled=Y, Prolific USB-to-Serial Comm Port (COM5), Prolific, ProlificSerial1, USB\VID_067B&PID_2303&REV_0400, Port_#0001.Hub_#0007
COM6, Enabled=Y, Conexant USB CX93010 ACF Modem, Conexant, USBSER000, USB\VID_0572&PID_1329&REV_0100, Port_#0007.Hub_#0001
COM7, Enabled=Y, USB Serial Device (COM7), Microsoft, USBSER000, USB\VID_1546&PID_01A8&REV_0201, Port_#0002.Hub_#0007

Note some USB ports are unplugged, but have fixed COM numbers if replugged
into the same USB port.  The lists above were created using the function
ReportSerialPorts passing the SerialPorts array.

Note this unit needs three Jedi jwapi header files: setupapi, Cfg,
CfgMgr32, which are included.


Magenta Hardware Events Component
---------------------------------

This unit contains the THardwareEvents component that listens for Windows
hardware event messages and calls events handlers for device changes such
as serial ports, disk volume changes, low disk space events and, power events.

THardwareEvents is installable, and has events onLogEvent (Device Hardware
Event below), onComPortEvent, onVolumeEvent, onLowSpaceEvent, onPowerEvent
and onRawEvent (Device Change Message below). Power events return PBT_xx
values for suspend, resume, battery low, etc.  Note the component starts
monitoring as soon as it is created.

Device Change Message, event=0x8004, data=0x007A1828
Device Hardware Event:  devicetype=5
Device Change Message, event=0x8004, data=0x007AC960
Old Serial Port Removed: COM5
Device Hardware Event: Old Serial Port Removed: COM5
Device Change Message, event=0x0007, data=0x00000000

Device Change Message, event=0x8000, data=0x0078EF00
Device Hardware Event:  devicetype=5
Device Change Message, event=0x8000, data=0x0078EC68
New Serial Port Arrived: COM5
Device Hardware Event: New Serial Port Installed: COM5
Device Change Message, event=0x0007, data=0x00000000

Device Change Message, event=0x8000, data=0x0079E260
Device Hardware Event:  devicetype=5
Device Change Message, event=0x0007, data=0x00000000
Device Change Message, event=0x8000, data=0x0009E3C0
New Disk Arrived, Volume: I
Device Hardware Event: Disk Change - New Disk Volume Installed: I
Device Change Message, event=0x0007, data=0x00000000


Directory Changes Monitoring Component
--------------------------------------

This is an updated version of the DirMon component created by Damien Thouvenin,
phaeteon and LoLa 20 years ago, updated for unicode compilers.  It uses the
ReadDirectoryChanges Windows API to detect file and directory changes on a
specified volume or directory, optionally in sub-directories.

TDirChange is installable, with OnCreated, OnDeleted, OnModified and OnRenamed
events that fire for file or directory changes.  Before setting the component
Active, set the Path to be monitored, then WatchTree and optionally
WatchFilters for the type of events to be monitored, see Signals sample.
Note the component runs a thread internally, and events are called from
that thread.

DirChanged: Modified: ProgramData\Magenta-Systems\ComCap5\comcap5.current
DirChanged: Modified: Windows\Prefetch\SIGNALS.EXE-236B99F3.pf
DirChanged: Modified: Users\angus\AppData\Roaming\Mozilla\Firefox\Profiles\0hed00ly.default\datareporting\glean\db\data.safe.bin
DirChanged: Modified: Users\angus\AppData\Roaming\Microsoft\Windows\Themes\Transcoded_001


TMagGpsLoc component
--------------------

The TMagGpsLoc component is designed to process GPS location data from
various sources with an event triggered when movement is detected.

Sources supported are:

1 - Windows Location API, available on most Windows tablets and higher end
laptops with a GPS sensor.

2 - NMEA 0183 sentences commonly generated by GPS receivers, these are
text lines starting with $. NMEA 0183 sentences processed are: GGA, GSA,
GSV, RMC, GLL and VTG, others are ignored.

3 - GT02 GPS Tracker Protocol, used by Concox TR02 vehicle trackers that
combine GPS, GPRS and GSM is a small 12V driven package designed for
mounting in vehicles.

4 - TK102/103 Tracker Protocol, essentially the NMEA RMC sentence, preceded
by date/time and mobile number, followed by useful stuff from other NMEA
sentences like satellite count, mobile IMEI and cell station stuff.

5 - WondeX/TK5000 Tracker Protocol used by VT-10, VT300 and other devices
is a simple format with IMEI, time, co-ordinates, speed and direction.

Further description and samples of these protocols will be found in the
functions that decode them.

Some of these GPS protocols are also generated by Android and iOS mobile
apps for location tracking.

There is a sample application Sensortest that provides serial UDP and TCP
Server support for NMEA 0183 sentences, and TCP Server for all the other
protocols, showing position and movement on a Google map.

Most testing was with a GlobalSat BU-353-S4 USB GPS Receiver a two inch
diameter device with a roof magnet that presents as a Prolific serial port
(a version with a real serial connector is also available), a DIYmalls
VK-162 USB GPS/Glonass Dongle using an eighth generation U-Blox module,
and the Concox TR02 vehicle tracker.

Also tested were a battery operated GlobalSat BT-359 Bluetooth CoPilot GPS
device (but Bluetooth serial ports are not always very reliable) and NMEA
0183 streaming from Android tablets and phones.  GlobalSat, DIYmalls and
TK102/103 devices are available from Amazon.  Android GPSd Forwarder is
available in the Play store and sends raw NMEA packets to a UDP server
(no IMEI).

TMagGpsLoc is installable, with OnLocationChange and OnStatusChange events
triggered for movement and sensor changes.  To use the component, set
property GpsType to sensor type: GpsTypeLocApi, GpsTypeNMEA, GpsTypeGT02,
GpsTypeTK10X, GpsTypeWondeX, and set Active true.  Then start passing
sensor data to the SetRecvData method as an ASCII string. The component
tries to interpret the packet setting Latitude, Longitude and Altitude
properties, and many other properties depending on the sensor.  There is
a GetDistance function to get metres between two lat/longs. For NMEA,
the GetNmeaInfo method will list all the GPS, Glonass and Galileo
satellites seen by the sensor (if supported).

Satellites in View = 28
Satellites Used = 21
True Heading = 193
Magnetic Heading = 3.1
Error Radius = 0.7
Geoidal Separation = 47m
PDOP = 1
HDOP = 0.7
VDOP = 0.7
Constellation: GPS
Satellite 10, Azimuth 293, Elevation 27, S/N 21, Fix
Satellite 12, Azimuth 212, Elevation 38, S/N 23, Fix
Satellite 13, Azimuth 142, Elevation 22, S/N 31, Fix
Satellite 14, Azimuth 49, Elevation 9, S/N 16, Fix
Satellite 15, Azimuth 171, Elevation 44, S/N 26, Fix
Satellite 17, Azimuth 61, Elevation 30, S/N 26, Fix
Satellite 19, Azimuth 91, Elevation 33, S/N 28, Fix
Satellite 23, Azimuth 254, Elevation 26, S/N 19, Fix
Satellite 24, Azimuth 289, Elevation 78, S/N 20, Fix
Satellite 32, Azimuth 314, Elevation 3, S/N 14
Satellite 1, Azimuth 18, Elevation 3, S/N 0
Satellite 21, Azimuth 352, Elevation 1, S/N 0
Satellite 25, Azimuth 223, Elevation 4, S/N 0
Constellation: Glonass
Satellite 73, Azimuth 251, Elevation 19, S/N 14, Fix
Satellite 79, Azimuth 52, Elevation 42, S/N 22, Fix
Satellite 69, Azimuth 178, Elevation 20, S/N 25, Fix
Satellite 81, Azimuth 80, Elevation 18, S/N 28, Fix
Satellite 71, Azimuth 322, Elevation 31, S/N 14
Satellite 70, Azimuth 250, Elevation 61, S/N 0
Satellite 88, Azimuth 26, Elevation 18, S/N 0
Constellation: Galileo
Satellite 2, Azimuth 54, Elevation 14, S/N 20, Fix
Satellite 7, Azimuth 178, Elevation 51, S/N 27, Fix
Satellite 8, Azimuth 151, Elevation 6, S/N 16, Fix
Satellite 27, Azimuth 224, Elevation 52, S/N 21, Fix
Satellite 30, Azimuth 66, Elevation 65, S/N 26, Fix
Satellite 11, Azimuth 2, Elevation 5, S/N 0
Satellite 19, Azimuth 314, Elevation 17, S/N 0
Satellite 21, Azimuth 230, Elevation 2, S/N 0

The sensortest application reports positions:

Latitude 51.38318, Longitude -0.08601, Altitude 64m, Speed 0, Course 0, ID ,
PackSer 0, Time 26/01/2022 18:10:50
GB Eastings 533,289, GB Northings 166,640

and will also show raw data from the sensor:

$GNRMC,181054.00,A,5122.99113,N,00005.16031,W,0.272,,260122,,,A*70
$GNGGA,180831.00,5122.99572,N,00005.16048,W,1,09,1.47,74.3,M,45.5,M,,*66
$GNGSA,A,3,09,16,04,26,18,29,,,,,,,2.46,1.47,1.97*1D
$GPGSV,3,1,12,04,12,304,17,05,17,064,21,09,06,336,26,16,30,294,20*78
$GNVTG,,T,,M,1.017,N,1.883,K,A*38

Clicking the Map button displays a window with a Google map on which
position changes are plotted with markers.  The original unit sensormap.pas
uses the TWebBrowser component which uses the Internet Explorer engine.
Unfortunately Microsoft has removed Internet Explorer from Windows 11 so map
display is more problematic, currently it still works but Google displays a
warning about using non-supported browser and plans to remove support for
MSIE in August 2022. Delphi 10.4 and later has a new TEdgeBrowser component
that is used by the sensoredgemap.pas unit which should be used instead of
sensormap.pas, it is easier to use than TWebBrowser.

The sensortest application requires the Async Pro serial RS232 library to
access GPS devices with serial output https://github.com/TurboPack/AsyncPro,
and the Internet Component Suite library for TCP devices
http://wiki.overbyte.eu/wiki/index.php/Main_Page.

The MagGpsConv unit includes functions to convert World Geodetic System
WGS84 longitude and latitude in degrees (as double) into the British Ordnance
Survey National Grid reference system of eastings and northings in metres,
within a box covering Great Britain, zeros being the south west corner.


Magenta Firewall Component
--------------------------

Magenta Firewall Component has functions to search and list selected
Windows Defender Firewall rules and settings, and to add and remove such
rules. Note only administrators can add rules.

Currently, only rules enabling inbound access for an application with all
protocols, addresses and ports are added, adding more precise rules needs
more parameters to be passed and handled.

The TMagFireWall unit uses COM Automation, so the application should call
CoInitialize(nil); before calling any functions, and CoUninitialize; after,
however Windows 10 seems to work without these.

The function MagFireWallAppsEnum lists all application files that are
allowed through the firewall. New apps are added with MagFireWallRulesAdd
and can be removed by MagFireWallRulesAdd.

Application Name:   OverbyteIcsIpStmLogTst
Application Path:   C:\delpcomp\ics\samples\delphi\sslinternet\overbyteicsipstmlogtst.exe
IP Version:         Any
Scope:              All
RemoteAddresses:    *
Application Enabled: True

Similarly, function MagFireWallServEnum lists rules for Windows services.

Service Name:       Remote Desktop
Type:               Remote Desktop
IP Version:         Any
Scope:              All
RemoteAddresses:    *
Service Enabled:    True
Customized:         False

Function MagFireWallRulesEnum lists firewall settings, which profiles are
active, and lists all built-in rules for Windows.

Rule Group: @FirewallAPI.dll,-32752
Rule Name: Network Discovery (SSDP-In)
Description: Inbound rule for Network Discovery to allow use of the Simple Service Discovery Protocol. [UDP 1900]
Application Name: C:\WINDOWS\system32\svchost.exe
Service Name: Ssdpsrv
IP Protocol: UDP
Local Ports: 1900
Remote Ports: *
LocalAddresses: *
RemoteAddresses: LocalSubnet
Direction: In
Rule Enabled: False
Edge Traversal: False
Profile: Domain
Rule Action: Allow
Interface Types: All

The MagFireWall unit also has Inno Setup scripts code (commented out) that
may be used during application installation to add rules, specifically the
procedure MagFireWallRulesAdd.  Generally, an application creates a rule
for itself allowing internet access for itself during installation, since
setup runs at administrator level.


Magenta Check Disk and Format Disk component
--------------------------------------------

The Check Disk and Format Disk component is a Delphi wrapper around the
Windows APIs exported by fmifs.dll, which are used to format fixed or
removable disk drives and perform disk checks on Windows NT4 and later.
It supports any local disk drive that has been assigned a drive letter
by Windows, just like the normal format and check disk tools in Windows.

The component may be used to format disks which are not currently in-use,
and to verify the file system on any drives, but only fix it on drives
that are not in-use.  It may be useful for preparing removable backup
drives.  Supports exFAT for massive drives, if supported by the OS.

These fmifs.dll APIs are undocumented by Microsoft, but were reverse
engineering by System Internals.  The component is is based on the
command line Chkdskx and Formatx applications by Mark Russinovich
available from https://docs.microsoft.com/en-gb/sysinternals/

Please note that the fmifs.dll APIs are different from most other Windows
APIs and are intended to be interactive with the user, rather than
passively accepting input and providing simple output.  Specifically, they
return progress information that is nornmally seen in a Windows dialog or
in a command line window, showing the steps being taken and reporting any
errors or corruption found.  This component attempts to parse the messages
and provide simple return information.  A demo program illustrates the use
of all the functions.


Check Disk

Example progress doing Check Disk of a USB key. Note this varies depending
on the file system type, and whether any errors are discovered.

Starting Check Disk for H:
H:\ Volume Label: USBH, File System: NTFS
Volume label is USBH.
CHKDSK is verifying files (stage 1 of 3)...
File verification completed.
CHKDSK is verifying indexes (stage 2 of 3)...
Index verification completed.
CHKDSK is verifying security descriptors (stage 3 of 3)...
Security descriptor verification completed.
Windows has checked the file system and found no problems.
253167 KB total disk space.
4 KB in 8 indexes.
0 KB in bad sectors.
3042 KB in use by the system.
2544 KB occupied by the log file.
250121 KB available on disk.
512 bytes in each allocation unit.
506335 total allocation units on disk.
500243 allocation units available on disk.
Check Disk: Finished OK


Format Disk

Example progress formatting a USB key.

Starting Format Disk for H:
H:\ Checking Existing Drive Format
H:\ Volume Label: USBH, File System: NTFS
H:\ Starting to Format Drive
Format Disk: Structure Created OK
Format Disk: Finished OK
H:\ Checking New Drive Format
H:\ Volume Label: USBKEY, File System: NTFS
H:\ New Volume Space: 256124416


Release Notes
-------------

2nd February 2022 - 2.0 - first public release.




Copyright Information
---------------------

Magenta Hardware Components are freeware, but are still copyrighted
by Magenta Systems Ltd who may change the status or withdraw it at any
time, without notice.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: https://www.magsys.co.uk/delphi/



