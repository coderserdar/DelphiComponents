TMAGRAS - DELPHI RAS COMPONENT - RELEASE 5.73
=============================================

Last Updated: 22nd November 2018, Release 5.73
by Angus Robertson, Magenta Systems Ltd, England

Email: delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, 2017.

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5,
XE6, XE7, XE8, 10 Seattle, 10.1 Berlin, 10.2 Tokyo and 10.3 Rio.
The last version of TMagRas compatible with Delphi 3 was 4.61. With
XE2 and later, Win64 is supported as a target, as well as Win32.

Note that release 5 only supports ANSI RAS APIs with Delphi 2009 and
later.

Compatible with Windows 2003, Vista, 2008, 7, 8, 2012, 10, 2016 and
2019, both 32-bit and 64-bit editions.

There is a separate TMagRas Release 6.3 which fully supports Unicode
under Delphi 2009 and later by using the RAS WideString APIs. A Unicode
demo application RASDEMO6.EXE is freely available, but otherwise
release 6.3 is only available to source code licensees, no free version
is available.


Overview
--------

TMagRas is a set of installable Delphi non-visual components,
supplied with several demo and example programs and a help file, for
accessing Dial Up Networking or Remote Access Services functions. It
allows Delphi developers to add full RAS functionality to their
applications, including dialling and monitoring multiple connections,
creating and editing phonebooks (without using Windows dialogs), and
getting performance information for connections.  TMagRas 5 supports
Windows 2003, Vista, 2008, 7, 8, 2012, 10 and 2016.

TMagRas is supplied as five separate installable Delphi components:
TMagRasCon includes dialling and monitoring and limited connection
editing, TMagRasPer has performance statistics, TMagRasEdt to create and
edit connections/phonebooks, TMagTapi lists TAPI devices and monitors
them (but does not make calls), and TMagRasAdm which supports the admin
interface to RAS Server for incoming connections.

These are supported by MagRasApi that includes the prototypes for all
RAS functions and loads them dynamically (in case RAS is not installed),
TMagTapiApi that includes prototypes for TAPI functions and loads them
dynamically, WinPerf has NT performance data structures, MagRasPdhApi
NT performance data helper, MagRasEnt to support phonebook entry lists,
MagRasStr with RAS string literals, MagTapiStr with TAPI string literals,
MagSubs1 with common non-RAS functions, and MagSubs2 has functions to add
shortcuts to the windows desktop, including non-file objects such as DUN
connections and printers.

A demo program illustrates use of the components, including monitoring
multiple connections, creating a simple new connection with minimal
properties, and editing detailed connection properties (seven tabs of
settings) including Windows 2000 and XP extensions.  Seven example
programs illustrate various aspects of the components and may be copied
to quickly add specific RAS functionality to applications.

TMagRas is copyrighted software, but the compiled component DCUs may be
used without cost in applications for use by the developer or within
their company but which are not otherwise distributed (including
freeware and shareware).  The component source code may be licensed for
a cost of £100 (UKP) (£117.50 with UK tax). Developers that license the
source may distribute applications using TMagRas without further cost.

The compiled TMagRas 5 components supporting ANSI only are available
for Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5, XE6, XE7, XE8,
10 Seattle and 10.1 Berlin.  component may be used with C++ Builder,
but currently OBJ files are not available so you will need to license
the source code and recompile. Earlier versions of the component are
available for Delphi 3, 4, 5 and 6. TMagRas 6.3 supporting Unicode with
Delphi 2009 and later and is only available to source code licensees.


RAS and Unicode
---------------

Windows NT4 and all later versions have supported Unicode internally,
effectively two byte wide characters with the potential ability to display
many thousands of symbols, including Greek, Russian and Far Eastern
languages at the same time, provided the Windows OpenType font contains
all the symbols.  Unicode characters may be used in many places, file
content, file names and paths, RAS entry names and logons, etc.

Although Windows itself supports Unicode, many applications are restricted
to working in ANSI, which is one byte wide characters, so interaction
with the Windows causes a translation to and from ANSI using the current
code page.  During conversion to ANSI, non-compatible Unicode characters
may be substituted by a failed character which is usually ? and illegal in
file names and paths.  So ANSI only applications are unable to open files
with non-compatible Unicode characters, and RAS entries will similarly
fail.

Delphi 2007 and earlier primarily supported ANSI strings and Windows APIs.
Although the widestring type was available, there was no wide TStrings for
lists and sorting and the VCL was ANSI, so it was really only possible to
support Unicode using third party component libraries.

Delphi 2009 and later fully support Unicode, with types String=UnicodeString
and Char=WideChar, and the VCL handles Unicode fully.  Beware updating
earlier applications from earlier Delphi releases needs careful checking
for all string and pointer usage, particularly PChar, Char, Chr, Move,
SizeOf and FillChar whose behaviour may be effected by Unicode.

TMagRas 5 and earlier use the ANSI versions of the RAS APIs, which
means entries can not be created with Unicode characters outside the
current ANSI code page.  Any entries created with such Unicode characters
by other applications will be listed by TMagRas with failed characters
(?) and will be unusable for calls to other RAS APIs.  TMagRas 5.40 and
later have been updated to support Delphi 2009 and later.

TMagRas 6 and later primarily support only Delphi 2009 and later, with
full Unicode support by using the WideString versions of the RAS APIs and
the UnicodeString type. 6.0 will work with earlier versions of Delphi,
but only with ANSI strings.  All the 6.0 widestring RAS units have W
suffixes to keep them separate from the old units, but the component names
are the same so only one version can be installed on toolbar at the time.
To simplify the source code, support has been removed from 6.0 for Windows
9.x and NT4, which are now 10 years old and rarely used, so TMagRas 5.50
should be retained if support for these is still needed. The main demo
application is now RASDEMO6.  TMagRas 6.0 and later are only available to
source code licensees, no free compiled objects version is available.
Existing licensees of the source code will need to pay a £50 upgrade fee
for access to TMagRas 6.0 and later, the first time an upgrade has not
been free for nine years.


Changes in TMagRas Release 5.73
-------------------------------

1 - Updated GetOSVersion (magsubs1) to recognise Windows 10 and Windows
2016. Updated magrascon to recognise Windows 10 and later.  Since this
is a cosmetic change, not all the packages have been rebuilt for 5.73,
but putting magsubs1.pas in the source directory will use the new version.
Updated manifest to support Windows 10 and add it as resource file.

2 - Added packages for Delphi XE7 which is version 21, Delphi XE8 which is
version 22, Delphi 10 Seattle which is version 23, Delphi 10.1 Berlin
which is version 24 and Delphi 10.2 Tokyo which is version 25.
Note only Windows platforms are supported.

3 - Added packages for Delphi 10.3 Rio which is version 26.  Display
Windows 2019 versison correctly.

4 - Windows 10 version 1703 (March 2017) broke RAS modem dialling, it
failed with error 633, port not available.  Microsoft eventually announced
a registry permissions fix that allowed modems to dial again which is
expected to be included in some future update.  This problem did not
effect VPN connections or those with NDIS drivers such as ISDN adaptors
and some 3G modems.

5 - To fix using regedit, key HLM\SYSTEM\CurrentControlSet\Services\RasMan,
Name: RequiredPrivileges, is a REG_MULTI_SZ with several strings and needs
SeLoadDriverPrivilege added to the end of the list, then a PC reboot.
Because this key is REG_MULTI_SZ, it can not be easily added using a REG
file, nor using Delphi registry functions like WriteString.

6 - We have provide a simple Windows Powershell script win10ras.ps1 which
will add the new privilege to the registry manually.



Changes in TMagRas Release 5.72
-------------------------------

1 - Updated GetOSVersion (magsubs1) to recognise Windows 8.1 and Windows
2012 R2. Note only magsubs1 dated March 2014 or later correctly detects
these two versions of Windows, Microsoft has changed an API to no longer
correctly return the OS version so a workaround was needed.

2 - Added packages for Delphi XE4 which is version 18, Delphi XE5
which is version 19.and Delphi XE6 which is version 20.  Note only Windows
platforms are supported.

3 - Corrected XE2 and XE3 packages to set Explicit Rebuild.

4 - No longer supporting Windows XP or earlier, although no code has
been removed yet.


Changes in TMagRas Release 5.71
-------------------------------

1 - Updated GetOSVersion (magsubs1) to recognise all editions of Windows 8
and Windows 2012.

2 - Added packages for Delphi XE3, which is version 17.  magsubs1 adds
MyFormatSettings to replace formatting public variables that have been
removed in XE3.

3 - MagRasOSVersion returns OS7 and OS8 but check >= OSVista since no
features added since Vista are supported in this version.


Changes in TMagRas Release 5.70
-------------------------------

1 - Fixes for 64-bit compiler, mainly in magrasapi where DWORD was often
used instead of pointers (which are 64-bits in Win64).  Note the various
RAS dialogs do not seem to display in Win64 applications, still under
investigation.

2 - CallbackId no longer published since it's a pointer, but a dummy
remains for backward compatibility. This was really for internal use.

3 - RegisterComponent is now in magrasreg.pas instead of each of the
units, to allow separate design and runtime packages to be built.
This means there are now separate MagRasx0r.dpr and MagRasx0d.dpr
projects for runtime and design packages, that both need to be built.

4 - Added SetDefDialLocation to avoid calling GetTransCaps to do the
same thing. Calling either before accessing dialling properties or
dialling a call stops Windows popping up a 'dial location' dialog.

5 - Updated GetOSVersion (magsubs1) to recognise Windows 8, although the
server version name is still uncertain.

6 - Added packages for Delphi XE2, which is version 16, there are
separate design and runtime packages since the Delphi IDE only supports
32-bit design packages, but needs 64-bit runtimes for Win64.

7 - Currently, only the Delphi 2007, 2010, XE and XE2 compiled objects
have been similarly updated for 5.70, other Delphi versions are still
5.60 or older objects.



Changes in TMagRas Release 5.60
-------------------------------

1 - Fixed cast warnings for Delphi 2009 and later

2 - Updated GetOSVersion (magsubs1) to use GetProductInfo for edition with
Vista and later so the correct edition names are always displayed.

3 - Added packages for Delphi XE, which is version 15.


Changes in TMagRas Release 5.50
-------------------------------

1 - GetTransCaps now creates a blank 'My Location' if missing to avoid
the Windows 'Location Information' dialog appearing the first time RAS is
accessed on a newly installed OS.  Note this feature needs administrator
rights to write to HLM registry keys and will silently fail otherwise

2 - DefCountryId is now set from LOCALE_ICOUNTRY using GetLcTypeInfo
instead of LOCALE_IDEFAULTCOUNTRY which is obsolete.

3 - Recognise Windows 7 and 2008 R2, but note that no Windows 7 RAS
extensions are currently supported in TMagRas, they mostly relate to
IP6 and VPNs, rather than dial-up. Internally, Windows 7 is handled as
Vista.

4 - Added packages for Delphi 2010, which is version 14.


Changes in TMagRas Release 5.40
-------------------------------

1 - Updated all the components and demos to support Delphi 2009. TMagRas
has only ever supported the ANSI RAS APIs to support western languages,
and not the Widestring/Unicode APIs needed for Asian countries. This
version has changed most String data types to AnsiString and Char to
AnsiChar, to be compatible with Delphi 2009 while remaining fully
compatible with Delphi 6 and later.  Beware this means Delphi 2009 will
generate lots of string cast warnings during compilation as Unicode and
ANSI strings are converted between each other.  A number of ANSI only
string functions have been added (TrimAnsi, UpperCaseAnsi, AscToIntAnsi,
etc) to avoid some of these conversions.

2 - Fixed a bug that meant MagRasGetEntryRecs did not read all the entry
properties for entries with non-ASCII characters, due the phonebook files
being saved in UTF-8 and not ANSI format.


Changes in TMagRas Release 5.30
-------------------------------

1 - Added bDefaultCreds property for TMagRasCon and TMagRasEdt, which
should be set true before calling PutDialProps, GetDialProps, PutDialParams
or GetDialParams functions to set user name and password for 'anyone who
uses this computer', which may be needed as well as setting Phonebook
Location to all users on Vista.


Changes in TMagRas Release 5.21
-------------------------------

1 - Ensure that the MagRasPhoneFiles array is completed with default
phonebook names when the TMagCon and TMagEdt components are created,
rather than when RAS is initialised, so setting PBLocation always sets
the PhonebookPath file correctly.

2 - It's no longer possible to set the PhonebookPath property except on
NT4, to avoid conflicts with PBLocation.

3 - Updated all the example applications with PBLocation so they all work
on Vista.

4 - Updated MSIEDefConn for Vista.


Changes in TMagRas Release 5.2
------------------------------

1 - Finally tested on Windows Vista RTM and Longhorn (Windows Server 2008)
beta 3.  Unfortunately Microsoft no longer supports several RAs APIs in
Vista, but has not documented this in MSDN.  Also, most application now run
without administrator rights which means connection entries in the 'All Users'
phonebook can not be edited.  So this release changes the default for new
entries to the 'Current User' phonebook for Vista and later.  Note that
Longhorn programs have administrator rights as standard.

2 - In the Demo4 program, moved Windows version information to the Network
tab and show Windows version information as well, also use IsProgAdmin (in
magsubs1) to show whether the application has admin rights and can
therefore edit 'All User' phonebook entries.  If a non-admin user attempts
to save a 'All User' entry, the RAS APIs should fail with 'access denied',
but this is not reliable and sometimes the API seem to work.  Note that
both Vista and Windows Server 2008 are Windows version 6.0 with the
ProductType differentiating them (similarly to NT4 Workstation and Server).

3 - TMagRasCon and TMagRasEdt each have a new PBLocation property which
should be set to REN_AllUsers or REN_User before calling most methods,
Win2K/XP default to REN_AllUsers, Vista/2008 to REN_User.  Generally, this
property is copied from the PBLocation in TEntryRec which is returned when
enumerating the connection entry list (MagRasGetEntryList).  Setting the
PBLocation property automatically sets the correct phonebook file name.
If your application specifically set the phonebook file path, this code
should be skipped and replaced by setting PBLocation.

4 - In the Demo4 program, Quick New now allows the phonebook location to be
set.  Note the example programs have not yet been updated with PBLocation.

5 - The CreatePhonebook and EditPhonebook methods that bring up Microsoft
dialogs now call RasEntryDlg since the original RAS functions no longer
work in Vista.  The Demo4 program also has new buttons to bring up the
Microsoft DUN and Dialling dialogs.

6 - PutDialProps and GetDialProps in MagRasEdt now use RasPutCredentials
and RasGetCredentials similarly to PutDialParams and GetDialParams in
MagRasCon, so work correctly with Vista.

7 - The IP Helper units have been removed, they now have their own
package distributed separately.  There is a new package MagentaSubs that
contains magsubs1, magsubs4 and magclasses, which is common to other
Magenta components.  Packages for Delphi 4, 5 and 2005 are no longer
supplied since these compilers are rarely used.  Delphi 2006 and 2007
both use the magras100.dpk.

Pending - allow for wrapping performance counters over 4 gigs.
Pending - update example projects and help


Changes in TMagRas Release 5.1
------------------------------

1 - Added initial support for Windows Vista (and maybe Longhorn), by
updating changed APIs.  Vista supports IPv6, but TMagRas does not yet, due
to lack of any way to test it.

2 - PutDialParams and GetDialParams withe NT4 and later now use the
RasPutCredentials and RasGetCredentials APIs instead of RasSetEntryDialProps
and RasGetEntryDialProps.  This change seems to allow passwords to be
sometimes saved in Vista Beta 2, but currently only for connections created
by some Vista dialogs.  Hopefully Microsoft will fix these issues for the
Vista release candidate due in September.

3 - Made some literals in magsubs1 conditional, which should allow TMagRas
to be used with C++ Builder.


Changes in TMagRas Release 5.0
------------------------------

1 - Validate the SubEntry before dialling.

2 - To simplify distribution, the IP Helper components are now
distributed separately, ditto the WMI and SMART components.


Changes in TMagRas Release 4.94
-------------------------------

1 - Fixed a problem on Windows XP and later that meant details of only
the first connection returned by GetConnections was accessible, this
only effected multi-link and VPN over dial-up.

2 - Added MSIEAutoDialOpt to set all three MSIE auto dial options, and
fixed MSIEDefConn to work properly on XP.  The Network tab of the RAS
Demo program reflects the new auto dial options.

3 - Added SCutSpecLinkEx and SCutAddLinkEx to create improved DUN short
cuts, with configurable icons and description, and to make the short
cut available to all logons.

4 - TMagRas has been thoroughly tested on Windows 2003, but there are no
new RAS features and no changes have been made.


Changes in TMagRas Release 4.91
-------------------------------

1 - TMagRas now supports an unlimited number of RAS devices, previously
there was a limit of 30 devices.  This change was primarily needed to
allow TMagRas to be supported on Windows 2003 beta (effectively
the server version of Windows XP), since about 260 devices are returned,
mostly identical VPN devices.  Something similar happened with W2K beta,
so these extra VPN devices may disappear before the final release.
Quick testing on Windows 2003 beta  did not show any other problems.


Changes in TMagRas Release 4.90
-------------------------------

1 - TMagRas now supports an unlimited number of connection entries,
previously there was a maximum limit of 100, after which none were
displayed atall.  This was done by making MagRasEntryRecs a dynamic
array (Delphi 4 and later).  There is a new test program to create
and delete multiple DUN connection entries, up to 999 at a time
(but don't try that many).  This is primarily to allow RAS applications
to be tested with hundreds of test entries.

2 - Added typeBroadband to TPType for Windows XP.

3 - Updated the main demo application to allow multi-link ISDN to be
tested.  A new 'ISDN link' drop down list allows all links to be dialled,
or a single link only.  The second link can then be dialled later, to
provide bandwidth on demand.  Note that not all ISPs will allow a second
link to be added later, it is usually a problem where the two calls get
answered by different access servers which can not multilink them.  There
are currently cosmetic problems in the demo monitoring the second link
because the connection handle changes during dialling, which seems very
bizarre.  More testing is being done.

4 - IPHelper and it's demo application have been improved to show DNS
server allocated to specific adaptors.



Changes in TMagRas Release 4.80
-------------------------------

1 - Added the ability to add shortcuts (or links) to the windows desktop
for both programs and non-file object such as DUN connections or
printers.  These functions are in magsubs2.  Special directories, such
as Printers, can also be indexed. There's a new demo/test program for
these functions called scuttest.

2 -  Added TConnectionList.CheckIndex which is used to validate the
argument to all Connections properties, to avoid possible 'listindex out
range' errors accessing connections that have already dropped.

3 - Moved MagRasOSVersion to magsubs1 so it's available without
initialising RAS APIs.  This may need changes to applications that check
MagRasOSVersion to import the magsubs1 instead of magrasapi.

4 - Corrected MSIEDefConn to set the correct registry key for a default
connection in Windows XP.

5 - Fixed major memory leaks in the IP Helper IpHlpIfTable and
Get_AdaptersInfo functions, created IpHlpAdaptersInfo which returns
TAdaptorRows.

6 - Currently MagRasAdm may only be used to monitor incoming calls to
Windows NT4 RAS Server.  With Windows 2000 (and .Net Server), RRAS
(Routing and Remote Access Service) is used instead.  Most of the work on
monitoring RRAS has now been done, unfortunately it has not been fully
tested since our Windows 2000 server with RRAS installed does not respond
as expected to the APIs.  It's not known if this is a problem with the
underlaying windows APIs or our implementation of them.  The new unit is
included with this distribution as magrasadm-new.pas in case any users
have RRAS working and need this functionality before we are able to
finish it properly.

Note that 4.80 is only available as source for registered users, and that
the help file has not been updated from 4.71.



Changes in TMagRas Release 4.71
-------------------------------

1 - Fixed potential bug in TMagRasCon.Create checking the RASAPI32.DLL
version failed if the DLL is not installed and raised an exception, bug
was introduced in 4.7.

2 - On W2K and XP, make sure that both phonebook files exist, creating
if necessary.  Create path if necessary before creating empty phonebook
file.

3 - Added DirectoryExists and ForceDirs to Magsubs1 to support previous
fix, which avoids linking in FileCtrl which brings in the dreaded Forms
and bloats many projects.



Changes in TMagRas Release 4.7
------------------------------

1 - Tested on Windows XP final build 2600, no changes needed since 4.62.

2 - Added IsDestReachable and IsNetAlive functions using the
Synchronization Manager SENSAPI.DLL that comes with MSIE5 and later. These
allow a quick test for internet connectivity, but are not totally reliable.

3 - Added MSIEAutoDial and MSIEDefConn which allow some MSIE Internet
Options to be set, effectively enabling auto dial for specified entry.

4 - No longer provided compiled DCUs for Delphi 3 since Int64 types are
now used in the demo program to display statistics to 4 gig.  4.61 will
remain available for D3, or the source can be licensed and modified to
remove Int64s.

5 - Installation of the compiled DCUs has changed slightly, all the files
necessary are now in the compiler version subdirectory, to avoid copying
any from the root (this means several duplicate files in the archive, but
eases installation).

6 - Added TCPIPMon to the demo programs to illustrates use of the IP
Helper APIs, corrected OS version check in EXPERF for Windows XP.



Changes in TMagRas Release 4.62
-------------------------------

Please note 4.62 is only available to registered users at present. The
help file has not yet updated for this release.

1 - Added support for Windows XP, tested on RC1 build 2505.  XP appears to
be fully backward compatible with Windows 2000 so old applications should
continue working OK on XP with earlier TMagRas releases.  But XP adds new
elements to the RASCONN and RASENTRY structures, which are TRasConnWXP and
TRasEntryWXP. MagRasOSVersion nows returns OSWXP for Windows XP, if your
application uses this (possibly for performance statistics) it will need
to be changed. TConnectionList has new Flags and LogonSessId properties to
support multiple users being logged onto windows at the same time.
TMagRasEdt has 10 new binary and 6 other properties, mostly seem to work,
but no proper MSDN documentation yet (most of the new stuff was in Windows
2000, but never supported by RASAPI32)

2 - Deleting subentries on Windows XP only, if the number of subentries is
reduced. On earlier OS, it's necessary to delete the entry before saving it
to delete old subentries, but this causes the password to be lost so is not
done automatically by this component.

3 - Corrected MagRasGetEntryRecs to convert ISDN11-0 to ISDN1 on Windows XP.

4 - In MagRasPer, performance statistics now allow for Win9x DWORD counters
wrapping after 4 gigs of total data since a reboot, but maximum data for a
session is 4 gigs.  NT4/W2K should have been 4 gigs already, but that
assumes the Microsoft APIs wrap correctly. Yes, 4 gigs on a modem is a lot,
but some ADSL and cable modems also use RAS and can do that in 24 hours.

5 - The TMagRas distribution now includes three new files, iphelper.pas,
iphlpapi.pas and dnshelper.pas.  These were not originated by Magenta
Systems, but have been heavily updated and tested on recent OSs. IP Helper
is only available for Win98, WinMe, W2K and XP, although some functions
work on NT4 with SP5 or greater.  It is included here since it allows
access to dynamic DNS server addresses and provides performance statistics
for network adaptors. dnshelper uses other techniques to get DNS info for
Win95 and NT4.  The DNS server address is primarily of interest to those
using a DNS lookup component.

6 - The RASDEMO4 program has been updated to support the new RAS features
in Windows XP.  The Online listView shows connection Flags and LUID (for
multiple logons).  Full Properties has a new XP tab showing new entry
options.  A new Network tab shows some stuff from the IP Helper API
including dynamic DNS addresses for the PC.



Changes in TMagRas Release 4.61
-------------------------------

1 - Recompiled with the final Delphi 6 release.

2 - Fixed a possible bug in MagRasGetEntryList that could have raised a
error if there are no installed connection entries.

3 - Added GetMACAddresses to return one or more network card MAC address
for the local or others PCs, see DEMO4.PAS for usage (it's not in the
help file yet).



Changes in TMagRas Release 4.60
-------------------------------

1 - Improved the means of listing phonebook entries (aka DUN connections),
in order to return extra information for W2K, and also to get common
information for all entries with a single function call.  This is all
done with new functions (not a component) in MagRasEnt, with the
MagRasEntryRecs data array being filled by MagRasGetEntryList (minimal
info) or MagRasGetEntryRecs (detailed info).  Use of the new functions
is illustrated on the Entry List tab in the demo program.  On NT4/W2K
the new functions read the physical phonebook INI files directly since
this is up to 100 faster than using API calls.  GetPhoneBookEntries is
still available, but now uses the new functions.

MagRasGetEntryList fills MagRasEntryRecs with sorted entry names and
phonebook paths (W2K only), MagRasGetEntryRecs is similar but also
fills phone number, devices and ports, MagRasGetPhoneBookFile loads
MagRasPhoneBookInf from rasphone.pbk files, MagRasIsPhoneBookNew checks
if rasphone.pbk files have changed since last read, and MagRasGetEntryKey
returns keyed data for an entry in MagRasPhoneBookInf.

2 - Finally got around to testing PhoneBookPath, simplified some related
code.  On W2K, PhoneBookPath must be used to save the entries in the
correct phonebook, PhoneBookPath is found from the new MagRasEntryRecs
array once MagRasGetEntryList or MagRasGetEntryRecs has been called.
To save a new entry on W2K, get the correct phonebook file name from
MagRasPhoneFiles (see Demo program).  Only administrators can save
entries in the All Users phonebook.

3 - Added a new component TMagRasAdm which uses the RasAdmin APIs to
monitor incoming RAS connections to NT4 Server, they don't work with NT4
Workstation incoming calls. These APIs provide similar functionality to
the Remote Access Admin application, showing which ports are configured
to answer calls, and the status of those in-use.  Note there is another
set of APIs for RRAS (Routing and Remote Access Service) that are not
currently supported by TMagRasAdm.  The APIs work in both NT4 and W2K,
but only appear to monitor NT4 Server (they work across a LAN).

4 - Moved some common functions to MagSubs1 and MagRasEnt to avoid
duplicated code.



Changes in TMagRas Release 4.51
-------------------------------

1 - Removed ConnectEx/ConnectNT 4.50 fix for fSubEntry not allowed as
zero so that W2K will again dial multilink.

2 - Default fSubEntry on creation to 1 so W2K does not dial multilink
(unless set to 0).

3 - Recognise Windows Whistler (aka 2002 or net), only basic testing so
far, seems OK.  Whistler added one new RAS API, RasDeleteSubEntry, but
this is not supported yet in TMagRasEdt.

4 - Corrected DisConnectEx so the timeout actually works and state
events are notified while waiting for timeout.  Existing applications may
now get one or more events that should have happened before, but didn't.

5 - Added TEncryptionType = encryptOptional which is the 'Typical'
default in W2K, and prevent the Windows dialog showing custom encryption.

6 - Added bCustomScript property for W2K.

7 - In TAPI event handler, only de-allocate calls in idle event, not
when disconnected.



Changes in TMagRas Release 4.50
-------------------------------

1 - All literals in the RAS and TAPI components have been moved to resource
strings for translation in magrasstr.pas and magtapistr.pas.  The Delphi
wizard can then be used to convert the strings into a resource file, or the
files could be changed to functions which return different languages
dynamically.

2 - Added onStateEvent (as an alternative to onStateChanged) which buffers
dial callback state messages to avoid dialling being blocked and possible
reentrancy issues (since callback can not be stopped).  It uses a queue to
avoid missing rapidly changing events. The event returns TRasStateRec with
all the state properties, rather than reading component properties that may
have already been changed by subsequent state changes.  Note that
CurDevName, CurDevType and ConnectPhoneNr are only set if the event source
is status (not dial).

This change has fixed a long term cosmetic problem in the demo program
where a second row appeared incorrectly in the ListView while a call was
being dialled.

3 - GetConnectStatusEx now updates CurDevName and CurDevType since the W2K
final now sets these correctly. ConnectPhoneNr is returned by NT4/W2K.
These settings are very useful to see if RAS connected using the device
specified in the phonebook entry (it may choose others) and to see what
phone number was really dialler (note it's the dialable format usually
starting with T (for tone dialling) and may include a calling card number,
so it not really suitable for displaying to users.

4 - Added ConnectNT similar to ConnectEx but NT4/W2K only, supports
dialextensions for paused states and speaker on/off.  The handle passed
must be zero to dial a new call, a non-zero handle will resume dialling a
call that has been paused for a callback number or authentication. Note
that paused states have not been tested yet!!

5 - Fixed an occasional problem in NT4 where performance statistics failed
by optionally using PDH.DLL (which must be redistributed) when the normal
registry version fails.  Tests show that only the RAS Total object returns
any data, so the RAS Port object code is currently disabled.

6 - In the TAPI component, on NT some ISDN had channel names starting from
0 not 1, so now allocate ports sequentially, 1/2/3/4 Win9x/NT, 2/1/4/3 W2K.
Also minor change to ensure call not returned if deallocated. Increased VPN
modems to 30. OnTapiStatus now sets tinfo2 to disconnect reason.



Changes in TMagRas Release 4.41
-------------------------------

1 - Corrected Str2IP so that it correctly converts IP addresses with a
single last digit and without trailing spaces (function rewitten).  This
problem meant that PutAllEntryProps sometimes ignored IP addresses,
unless they had trailing spaces (which TMaskEdit added in the demo
programs).

2 - Added IsIPStr to check an ASCII representation of an IP address is
valid.

3 - Improved validation of IP addresses, stop numbers > 255 and zero
first.

4 - In TMagRasCon, CreatePhonebook and EditPhonebook now both need
Application.Handle passed as a parameter.  This avoids the Forms unit
being included in some apps.  Graphics and Control units now conditional
as well to ease linking.


Changes in TMagRas Release 4.40
-------------------------------

1 - Added a help file and seven new simple example programs.

2 - Recognise Windows Millennium (aka WinMill)

3 - Compiled as TComponent again to allow use as an NT service.
Previously TMagRas was descended from TCustomControl to allow use in
an ActiveX (and may still be conditionally compiled by changing the
CUSTCNTL directive in the source). Warning: this change means that
applications compiled with TMagRas 4.0 to 4.3 will throw up errors on
the Height and Width properties that are now removed (but were never
used).

4 - Minor tidying up of unused properties and code.

5 - Added DPK package files to build a package from the TMagRas and
TMagTAPI components, to ease installation.  BPL/DPL/DCP files are not
supplied since these would double the zip size.


Changes in TMagRas Release 4.30
-------------------------------

1 - This version has been tested with Windows 2000 build 2195, which is
the final retail version.  GetDialParams no longer returns the
connection password, but what Microsoft call a 'handle' that is 16
asterisks. Passing this 'handle' back to SetDialParams causes the
existing password to be unchanged, and so Connect causes the existing
password to be used.  So if your applications checks that what is saved
is also returned, it will fail under W2K.  Avoided what appears to be a
bug in W2K whereby dialling properties were lost after saving an entry
and it's sub entries - now save a second time after doing the sub entries,
silly but it works.

2 - Added Str2IP to TMagRasEdt to convert and validate IP addresses.

3 - Added DUNVersion and DUNInfo properties to TMagRasCon to return the
DUN version.  A look-up table uses the build of RASAPI.DLL (in
DUNVersion) to create a version description like 'DUN 1.1/Win95/OSR2'
(in DUNInfo).

4 - Corrected PhoneNumber being ignored for dialling (error in 4.00 and
later), so the number dialled was always the default from the phonebook
entry.  You should set PhoneNumber to blank before calling Connect to
use the default number.

5 - GetEntryProperties now completes LocalPhoneNumber not PhoneNumber,
to avoid confusion when dialling calls.

6 - Added GetTransCaps which gets modem and country dialling properties
so we can offer better display of canonical numbers (we need to know the
local city code).  The function completes DialProps, DialLocation,
DialCard and DialCountry for the current dialling location.  Full
information on the content of these records is in the TAPI API info.

7 - Added UseCountryAndAreaCodes and PromoteAlternates properties for
use with dialling properties, and EntryOptions which is 32-bits of flags
where other properties are needed.



Changes in TMagRas Release 4.20
-------------------------------

1 - Make sure Win9x modem device configuration set correctly for new or
edited connections/phonebooks.  Previously an error would appear when
attempting to access modem properties when editing a phonebook using the
normal dialog.  Also set default modem device configuration when
updating Win9x connections, if it's missing.

2 - Structures are now available to edit the Win9x modem device info,
but the properties are not currently being processed.

3 - Corrected GetEntryProperties canonical phone number on NT4/W2K when
area code set but dialling properties disabled.


Changes in TMagRas Release 4.10
-------------------------------

1 - Performance statistics now support two adaptors for Win9x, so that
the devices used for the first two connections can now be monitored
separately.  Note that Windows does not say which adaptor is monitoring
which connected, so they must be allocated sequentially.

2 - A performance statistics reset now clears all arrays so 'all' stats
restart correctly on NT4.

3 - Added DefCountryId proper try to ease created connections/phonebooks.

4 - No longer store component version on form.

5 - TotSubEntries is now set as 2 for Win9x ISDN multilink, but there
are no modem/dial details since the RAS API was never completed by
Microsoft.

6 - Changed TConnectionList.Clear to ClearFree to avoid overwrite
problem in D3.


Changes in TMagRas Release 4.00
-------------------------------

1 - Added Ex versions of all main functions using a specified connection
handle rather than the internal one, to allow proper support for
multiple connections.  The application will need to keep track of the
connection handles for each connection, to allow status to be requested
separately and for hang-up.

2 - All functions now use NT4 and W2K extensions dynamically, so the
same compiled application will run on all three platforms ignoring stuff
not supported.  This means TMagRas will now start multilink ISDN calls
under NT4 and W2K.

3 - Added Subhandle handling to identify multilink ISDN calls separately
(GetSubHandle and GetSubHandles).

4 - Added some minimal TAPI functions to translate addresses
(TranslateAddr), but note there is no TAPI modem list so dialling
preferences always come from the first modem.  The separate Magenta TAPI
component does all this properly.

5 - The event handlers how have properties to identify whether dialling
or status events are being made (StateEventSource), the connection
handle (StateRasConn) and from which subentry with multilink ISDN calls
(StateSubEntry).  Note that, due to the requirement to handle multiple
calls, StateChanged events are no longer suppressed when they have not
changed, so the application must keep track of the status for each call.

6 - When listing active connections (GetConnections), the device name
and type, phonebook name, subentry and telephone number (NT only) are
now made available.  A flag is set (ConnChangedFlag) when the connection
list changes to save the application needing to keep checking.

7 - Performance statistics have been separated from the main component,
to avoid the overhead in applications not needing this functionality.
All the main connection functions are in TMagRasCon (magrascon.pas),
performance statistics are in TMagRasPer (magrasper.pas) while full
editing of phonebooks is in TMagRasEdt (magrasedt.pas).  Include files
are now magrasapi.pas, magtapiapi.pas and winperf.pas.

7 - Performance statistics now supports separate comms ports on NT4 and
separate connections (by handle) on W2K.  The previous properties return
combined information for all connections, while separate information is
available from several arrays: PerfXmitCur, PerfRecvCur and PerfConnSpd
(not NT4), with PerfPortNam showing the NT comm port, and PerfRasConn
and PerfSubEnt needing to be set for W2K.  Note that Win9x does not give
separate statistics for different connections and NT4 gives the same
statistics for each multilink connection.

8 - GetEntryProperties (in TMagRasCon) has been extended to return
AutodialDll, AutodialFunc, TotSubEntries (so you know whether to check
for multilink connection handles) and AltPhoneNrList which is a list of
alternate numbers to dial.  For multilink connections, arrays include
the subentry details, SubDeviceType, SubDeviceName, SubDevicePort,
SubLocalPhoneNumber and SubPhoneCanonical.

9 - PutEntryProperties (in TMagRasCon) has been added to update minimal
properties in a phonebook, just PhoneCanonical, AutodialDll and
AutodialFunc.  GetEntryProperties must be called before
PutEntryProperties.   These two functions are used on the 'Some Props'
page in the demo application.

10 - Full editing of phonebooks has been added with TMagRasEdt.
GetAllEntryProps gets all the properties for a specified connection
including sub entries.  DefaultProps clears all connection properties,
PPPDefault clears and then defaults to minimal PPP, while
PutAllEntryProps creates a new named connection or updates an existing
connection.  These functions are used in the 'Full Props' and 'Quick
New' pages of the demo application.  The former displays all the
connection properties including multilink sub entries and allows them to
be updated, it is very complicated!  The latter is requests just the
minimal information to create a new PPP connection and should be easily
understood.  Note that there is no validation in the connection of
properties being written to the connection, so saving may fail for
instance if you tick use a DNS address but leave it blank.  Precise
behaviours differs between platforms, but generally invalid properties
are simply ignored and reset.  Full details of the properties are
available in the Microsoft API and MSDN documentation, part of which is
contained in the file 'rasentry.txt'.


Installation
------------

Since TMagRas does not need design time properties, it need not be
installed on the component bar as such, but may be created in the
application as needed, as shown in the RASDEMO4 program.  However the
example programs do require TMagRas to be properly installed.

TMagRas is supplied in a zip file with common files such as the Help and
support DLLs in the root, the demo and example programs in /DEMO, and
further directories containing the DCU/RES/BPL/DCP files for each
supported version of Delphi.

The MagentaSubsx.BPL, MagRasx0r.BPL and MagRasx0d.BPL (and DCP, RES,
DPK, DPROJ) package library files are version specific, where x is
the compiler version:

    6 (v6)
    7 (v7)
    2006/2007 (v11)
    2009 (v12)
    2010 (v14)
    XE (v15)
    XE2 (v16)
    XE3 (v17)
    XE4 (v18)
    XE5 (v19)
    XE6 (v20)
    XE7 (v21)
    XE8 (v22)
    10 Seatle (v23)
    10.1 Berlin (v24)
    10.2 Tokyo (v25)
    10.3 Rio (v26)

which should then be installed using Install Packages, Add onto the
Component palette.  Once installed, open and compile one the example
programs to check for errors.

To install the component, the files from the directory appropriate for
your compiler should be copied into a library directory, typically a
common directory used for third party components (like LIBMISC).
This directory needs to be added to the Delphi Library Path in Tools,
Environment Options.  The design time MagRasx0r.BPL package library files
(where x is the compiler version) should then be installed using Install
Packages, Add onto the Component palette.  Once installed, open and
compile one the example programs to check for errors.
Delphi 2006 and 2007 DCUs are compatible, so only DCUs built with 2007
are supplied, but may be installed into 2006.

The RasDemo4 application needs the TSpin component installed on the
Samples tab.  Sometimes the samples package is not installed automatically
with Delphi, and needs to be added to the Component pallete manually,
using the package dclsmpx0.bpl (where x is the compiler version), spin.dcu
may also be needed in the LIB directory.

Please note that all Delphi DCU files have an internal signature added
by the compiler to ensure they are only linked with the correct compiler
version.  Delphi sometimes uses different DCU signatures for special
versions of Delphi, such as betas, demonstration and personal versions,
and the TMagRas DCUs may not then be used with these versions.  The
sympton is a request for the PAS module, so the DCU can be recompiled.
This will not be a problem for anyone that licensed the source code.


TAPI Functions
--------------

Note that Magenta Systems also has available some TAPI functions that
allow monitoring on modems and ISDN adaptors using events, avoiding
needing to continually poll using the RAS APIs.  TAPI also monitors
non-RAS modem usage and will monitor incoming calls.  A TAPI function is
also used to convert the canonical telephone number into a dialable
number according to telephony dialling properties (a basic version of
this is included in TMagRasCon).  TMagTAPI is available free of charge
to users that license the source to TMagRas. The TAPI functions do not
support making any calls at present.


Platform Differences and DLLs
-----------------------------

TMagRas works with all currently supported versions of Windows by
internally checking the version and using different APIs, DLLs and
structures as needed.  Some functionality is missing from earlier
versions, as detailed below. The DUNInfo property in TMagRasCon shows
the actual DUN version, based on the rasapi32.dll version number.

Windows 95 to Windows 2000 are no longer supported.

Windows XP - adds a RAS function to delete sub entries, also some extra
phonebook entry stuff allowing some of the new properties added in
Windows 2000 to be accessed via the RAS APIs.

Windows 2003 - no known differences from XP.

Windows Vista - RAS supports IPv6. RasEditPhonebookEntry
and RasCreatePhonebookEntry are no longer supported, RasSetEntryDialParams
does not set the password correctly.  Applications generally run without
administrator rights which prevents updating 'All Users' Phonebooks.

Windows Server 2008 - same as Vista, except programs have
administrator rights as standard.

Windows 7 - internally this is version 6.1 (Vista was 6.0) and no
substantial differences from Vista, just better IP6 and VPN support.

Windows 2008 R2 - same as Windows 7, except programs have
administrator rights as standard.

Windows 8 - internally this is version 6.2 and similar to 7.

Windows 2012 - same as Windows 8, except programs have
administrator rights as standard.

Windows 8.1 - internally this is version 6.3 and similar to 7.

Windows 2012 R2 - same as Windows 8.1, except programs have
administrator rights as standard.

Windows 10 - internally this is version 10.0 and similar to 7 and 8.
This is updated with a dated version twice a year, but this version
can only be read from the registry or determined from the build.
Windows 10 version 1703 (March 2017) broke RAS modem dialling, but
there is a Powershell script to fix.

Windows Server 2016 - same as Windows 10, except programs have
administrator rights as standard.

Windows Server 2019 - same as Windows 10 version 1809, except
programs  have administrator rights as standard.



Distribution
------------

TMagRas is distributed in various separate archives:

rasdem573.zip contains the executable demo and example programs only.
rasobj573.zip contains the compiled component objects, help, demo and
   example programs (no executables).
rassrc573.zip contains the component source code and help.
rassrc640.zip contains the component source code and help.

The first two archives may be downloaded from Magenta's web site at
http://www.magsys.co.uk/delphi/ and may be freely distributed via web
pages, FTP sites, BBS and conferencing systems or on CD-ROM in unaltered
zip format, but no charge may be made other than reasonable media or
bandwidth cost.  If you list the component somewhere, even as a link,
please let us know so you can be informed of new versions.

The source code zips are available to licensed users only from a
restricted area of the web site, and may not be distributed in any way.


Copyright Information
---------------------

TMagRas is copyright Magenta Systems Ltd, England, 2018.

Magenta Systems Ltd
9 Vincent Road
Croydon
CR0 6ED
United Kingdom

Phone 020 8656 3636, International Phone +44 20 8656 3636

Email: delphi@magsys.co.uk
Web: https://www.magsys.co.uk/delphi/



