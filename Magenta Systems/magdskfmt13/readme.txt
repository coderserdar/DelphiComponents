Magenta Systems Check Disk and Format Disk component v1.3
=========================================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
Compatible with Vista, 2008, 7, 8, 2012, 10, 2016 and 2019.
Supports both VCL Win32 and Win64.


Introduction
------------

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
available from http://www.sysinternals.com.

Please note that the fmifs.dll APIs are different from most other Windows
APIs and are intended to be interactive with the user, rather than
passively accepting input and providing simple output.  Specifically, they
return progress information that is nornmally seen in a Windows dialog or
in a command line window, showing the steps being taken and reporting any
errors or corruption found.  This component attempts to parse the messages
and provide simple return information.  A demo program illustrates the use
of all the functions.


Check Disk
----------

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
-----------

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


Class TMagFmtChkDsk
-------------------

The component may be installed on palette, but is non-visual so it's
usually easier to create it in code.

Types
-----

TMediaType = (mtHardDisk, mtFloppy) ;

TFileSystem = (fsNTFS, fsFAT, fsFAT32, fsEXFAT) ;


Functions
---------

LoadFmifs: boolean ;

Used to load the fmifs.dll APIs, the public variable MagFmifs_Loaded is
set True if this succeeds.  It's not necessary to call this, since it's
done before any other functions.


function FormatDisk (const DrvRoot: string; MediaType: TMediaType;
  FileSystem: TFileSystem; const DiskLabel: string; QuickFormat: boolean;
                                           ClusterSize: integer): boolean ;

This function causes a disk to be formatted.  DvrRoot could be ie 'd:\';
MediaType is mtHardDisk or mtFloppy; FileSystem is fsNTFS, fsFAT, fsFAT32
or fsEXFAT; DiskLabel is up to 16 characters, the QuickFormat flag may
cause faster formatting is overridden if the file system changes;
ClusterSize is 512, 1024, 2048, 4096, 8192, 16384, 32768 or 655536, but
format may fail if it's not supported by the file system.

Returns true if disk is formatted OK.  Calls onInfoEvent with ASCII
progress information and errors, see example above.  Calls onProgressEvent
with step percentage completed, to optionally update a progress bar.  Both
events allow formatting to be cancelled, but this may not be immediately
effective. An exception will be raised if the drive can not be found or if
it is in-use.


function CheckDisk (const DrvRoot: string; CorrectErrors, Verbose,
                       CheckOnlyIfDirty, ScanDrive: boolean): boolean ;

This function causes a disk check of the file system to be performed.
DvrRoot could be ie 'd:\';  Correct Errors is true if the disk can be
written to correct errors;  CheckOnlyIfDirty seems to be ignored;
ScanDrive is true is a full disk scan should be done.

Returns true if check disk passes OK. The  FileSysProblem will be true
if a problem was detected during Check Disk with the FirstErrorLine
property containing the first error line, note this may not always be
meaningful.  The FreeSpaceAlloc property will be true if 'free space marked
as allocated' is seen, but this does not fail Check Disk. Calls onInfoEvent
with ASCII progress information and errors, see example above.  Calls
onProgressEvent with step percentage completed, to optionally update a
progress bar.  Both events allow check disk to be cancelled, but this may
not be immediately effective.  An exception will be raised if the drive
can not be found or if it in-use and CorrectErrors is true.


function VolumeCompression (const DrvRoot: string; Enable: boolean): boolean ;

This function enables or disable volume compression, if the file system
supports compression. DvrRoot could be ie 'd:\'.
Returns true if compression state is changed OK.


Properties
----------

These are effectively results from Check Disk, see above.

property FileSysProblem: boolean ;
property FreeSpaceAlloc: boolean ;
property FirstErrorLine: string ;


Events
------

These events are optional, but allow progress to be displayed or logged.

onProgressEvent - ProgressEvent (Percent: integer; var Cancel: boolean) ;

onInfoEvent - InfoEvent (Info: string; var Cancel: boolean) ;


Demonstration Application
-------------------------

A Windows demonstration application DISKFMT.EXE is supplied, with source
and compiled program.


Files Enclosed
--------------

=Demo Application
fmtmain.dfm
fmtmain.pas
diskfmt.dpr
diskfmt.exe

=Component
magfmtdisk.pas
magsubs1.pas


Note this project needs Win 3.1/Delphi 1.0 Comptability palette components
from dcl31wXXX.bpl which may not be installed by default.

Changes
-------

Release 1.1 - better compatability with Vista and later, no longer shows
an unknown Check Disk callback, more callback messages handled.  Also
testing the program has administrator rights on start-up with a warning
if it will not function.

Release 1.2 - corrected progress message charset which was OEM (IBM-PC)
not ANSI or unicode, thanks to Francois Piette.

Release 1.2 - Updates subroutines for Win64 support.

Release 1.3 - Added exFAT support for massive drives, if supported by the
OS.  Added a Refresh Drives button to rebuild the drives box after changing
removable drives.


Copyright Information
---------------------

Magenta Systems Check Disk and Format Disk component is freeware, but is
still copyrighted by Magenta Systems Ltd who may change the status or
withdraw it at any time, without notice.

Magenta Systems Check Disk and Format Disk component may be freely
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



