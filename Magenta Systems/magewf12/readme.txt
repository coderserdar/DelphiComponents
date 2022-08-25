Windows XPE Enhanced Write Filter header conversion v1.2
========================================================

Updated by Angus Robertson, Magenta Systems Ltd, England, 11th September 2012
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd


Introduction
------------

The Windows XP Embedded Enhanced Write Filter (EWF) is used to protect one
or more disk volumes by intercepting write requests to the volume and
redirecting it to an overlay volume, either RAM or another disk. Note
the EWF header conversion is only useful on Windows XP Embedded with the
Enhanced Write Filter DLL installed. It is not available on Windows XP
Pro, or other operating systems.

EWF is designed to protect the Windows boot volume so effectively it's
write protected and boots up identically each time, with any changes made
to a protected drive being lost on power down. If changes need to be
saved, EWF API calls are needed followed by a reboot to actually update
the protected drive.

Microsoft provides a command line utility EWFMGR.EXE that reports any
protected volumes and provides various commands that will be processed on
the next restart of the PC.

The EWF API allows these commands to be made from Windows applications.
The Microsoft EWFAPI.H C header file is included, with the Microsoft
help file that details the various APIs.  The EWF API functions enable
applications to query and modify EWF configuration settings for protected
volumes. Typical usage scenarios for these functions include:

- Disabling/enabling EWF for a protected volume.
- Restoring or discarding an overlay level.
- Creating a checkpoint by adding an overlay level.
- Committing the current overlay.
- Persisting data through a reboot.

As well as a Delphi conversion of the EWF APIs, this package includes a
Windows demonstration application using the APIs.  Please note this
program has only been testing on Windows WP Embedded running on a flash
memory C drive (IDE interface) with a single overlay in RAM.  It has not
been tested with a hard disk which potentially offer multiple overlays.


Demonstration Application
-------------------------

A Windows demonstration application XPETEST.EXE is supplied, with source
and compiled program.  When started, it displays information about any
protected drives found, and provides buttons to Disable Overlay, Enable
Overlay, Commit Overlay, Save Persistent data and Clear Command.


Release Notes
-------------
1.0 - 20th October 2005 - baseline
1.1 - 5th August 2008   - updated to be compatible with Delphi 2009
1.2 - 8th April 2009    - added MINENUMSIZE 4 so that enumerated variables
are correct length for C++, thanks to Sven van Bruegge.  This fix means the
protected info should now be correct.


Files Enclosed
--------------

=Demo Application
xpemain.dfm
xpemain.pas
xpetest.dpr
xpetest.exe

=Headers and Components
ewfapi.pas
magsubs1.pas

=Microsoft Files
ewfapi.h
ewfapi.chm
ewfapi_relnotes.rtf


Requirements
------------

Compatible with Delphi 7/2005/2006/2007/2009/2010/XE/XE2/XE3, only useful
on Windows XP Embedded with EWF installed but may be compiled or run on
Windows 2000 and later.  Supports Unicode with Delphi 2009 and later.


Copyright Information
---------------------

The Windows XPE Enhanced Write Filter header conversion is freeware, but
is still copyrighted by Magenta Systems Ltd who may change the status or
withdraw it at any time, without notice.

The Windows XPE Enhanced Write Filter header conversion may be freely
distributed via web pages, FTP sites, BBS and conferencing systems or on
CD-ROM in unaltered zip format, but no charge may be made other than
reasonable media or bandwidth cost.


Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636
Fax 020 8656 8127, International Fax +44 20 8656 8127

Email: delphi@magsys.co.uk
Web: http://www.magsys.co.uk/delphi/



