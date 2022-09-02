======================================================================
                  FlashFiler Crystal Reports Driver
======================================================================


Introduction
============

This Dynamic Link Library is an add-on for FlashFiler v2.0x to enable
Seagate Software's Crystal Reports to directly access FlashFiler databases.
To use this database driver, you must already have installed Crystal Reports
v4.5 or v5.0 (32-bit only).  The driver ships as an authenticated
DLL which you will install as a Crystal Reports database driver.

Installation
============

The database driver is named:  P2BFFxxx.DLL, where <xxx> refers to the version 
of the build.

Before installing this driver, it is assumed that Crystal Reports has already 
been installed on the machine.  Copy the P2BFFxxx.DLL file into the 
C:\WINDOWS\CRYSTAL directory.This is the location where Crystal Reports stores 
most of its database drivers.  If you don't already have a C:\WINDOWS\CRYSTAL 
directory, and you're certain you've correctly installed Crystal Reports, 
search your hard disks for a native Crystal Reports database driver such as 
P2BPDX.DLL, P2BXBSE.DLL, or P2BBDE.DLL.  Copy your FlashFiler driver into 
that same directory.  Crystal Reports also keeps drivers in the 
C:\WINDOWS\SYSTEM directory and the CRW application directory.  You should
install the FlashFiler driver into C:\WINDOWS\CRYSTAL unless you have need to
move it to one of these other directories.

When Crystal Reports opens a data file, it scans the directory containing its
database drivers and loads each one until it finds one that responds positively
that it can recognize the data file given to it.  Unfortunately, some of the
native Crystal Reports drivers incorrectly respond that they can recognize
FlashFiler data files.  When this happens, the table structure displayed by
Crystal Reports usually contains only a single field called FIELD1.

You can verify whether Crystal has settled on an incorrect driver by selecting
File|Report Options from the main menu.  At the bottom of this dialog, above
the grayed out combo box is a grayed out label showing the name of the driver
that Crystal loaded to process this data file.  It should say PDBFFxxx.DLL.  
If it does not, then you've stumbled onto a native Crystal Reports driver that 
is not behaving robustly.  You must remove this errant driver from the 
directory (or rename it so that it no longer matches the pattern P2B*.DLL).

Remember, for 32-bit Crystal Reports, even if File|Report Options says it's
loaded the driver PDBBDE.DLL, it's really referring to P2BBDE.DLL.  All the
32-bit drivers are prefixed with P2B although this display always reports
PDB prefixes in both versions.

Setting the Network Configuration
=================================

Since the Crystal Reports database driver is actually a FlashFiler client
application, it needs to be aware of the network protocol to use to connect
to the FlashFiler server.  Use the FlashFiler Client Communcations Utility
to set the protocol and optional fixed server name values for each client
workstation.

Using Crystal Reports
=====================

Accessing FlashFiler data files through Crystal Reports is similar to accessing
desktop files.  You'll have to select the physical FFD file from a drive and
directory.  For example, select File|New from the main menu.  Click the
"Custom>>>" button.  Click "Data File".  Then select your FlashFiler data file.
Paths to other machines will be converted to Universal Naming Convention
format before being processed by the FlashFiler server.  Paths to the local
machine are only valid if the server is also running on the local machine.

You can change Crystal Report's default wildcard specifier to accomodate
FlashFiler datafiles as follows:

32-bit:   In the registry, change DatabaseSelector to "*.ff2" in the
          following key:

          HKEY_CURRENT_USER
            Software
              Crystal Software
                Crystal Reports
                  DatabaseOptions

Technical Support
=================

This driver was developed by TurboPower Software Company and is not supported
in any way by Seagate Software.  DO NOT CONTACT SEAGATE SOFTWARE FOR TECHNICAL
SUPPORT REGARDING THE FLASHFILER DATABASE DRIVER FOR CRYSTAL REPORTS.  Refer
all technical support questions related to the Crystal Reports driver directly
to TurboPower Software.

Technical support questions can be sent to support@turbopower.com, or you may
use our support newsgroup turbopower.public.support.flashfiler

