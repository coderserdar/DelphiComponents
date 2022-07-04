                TCompLHA Component Set V6.0
              LHArc/LHA compatible compression
               for Delphi, Kylix and C++ Builder

  Copyright (c) 1995-2001  Baskin-Kerr Trust under license to SPIS Ltd
        Fax: +64-3-384-5138  Email: software@spis.co.nz
         Web:  http://www.spis.co.nz/compLHA.htm

>>>>> If you ordered the registered version of TCompLHA, see REGINFO.TXT

This component is fully functional SHAREWARE.

You are encouraged to pass it on, upload it to BBS, Web and 
ftp sites and generally spread it around as much as you can
-- but please keep all the files together!

If you find TCompLHA useful, registration will get you the
latest version, a personal ID to eliminate the occasional
reminder message, access to our highly regarded (and, I have
to say it, rarely needed) support AND a nice warm
fuzzy feeling.

Registration details are at the end of this file.

ALSO SEE COMPLHA.HLP AND LHAFAQ.TXT FOR MUCH MORE INFORMATION

1. Key features:

*  Standard support for ALl versions of Delphi, Kylix and C++ Builder
*  Create and manage multi-file compressed archives
*  Simple one-step methods for most operations, including backup/restore
*  Create segmented (multi-disk) archives with the TSegLHA component
*  Read/write archives made with LHArc and LHA (also shell utilities like WinZip)
*  Read compatible with TCompress archives
*  Compress any stream (e.g. memory, database blobs) directly to/from archives
*  Standard user interface provided, fully customizable
*  Encryption, wild card and self-extracting EXEs all supported
*  Built-in verify and "safe" modes -- ideal for Internet and communication applications
*  Thread-safe, NATIVE compression -- no DLL or OCX worries
*  Full component source code available

2. Quick Installation
   (This is for experienced people in a hurry. For detailed instructions,
    DO see the Installation section of COMPLHA.HLP.
    Also see the File List below for more details).

*   Unzip the appropriate DnDCUS.ZIP file (e.g. D6DCUS.ZIP is
    for Delphi 6.0, C5OBJS.ZIP is for C++ Builder 5.0 etc.)
*   Delphi 1.0/2.0: install COMPLHA.DCU into your component library
*   Delphi 3.0 or later: Build and install the CompLH60.dpk package
    into your component library (File|Open|CompLH60.dpk,
    Compile|Install)
*   C++ Builder 1: See the help file
*   C++ Builder 3.0 or later: Install the complh60.bpk package into
    your component library (File|Open|complh60.dpk, Compile|Install)
*   Load the LHADEMO project, compile and run it (if you are
    using Delphi 1.0, see the notes at the top of LHAMAIN.PAS)
*   Examine LHAMAIN.PAS (and other sample projects) for numerous examples
    of how to use TCompLHA

Note: TCompLHA provides very comprehensive archive management capabilities.
Most developers might only need to drop a TCompLHA component on their form,
set a couple of properties and call its Compress, Scan and Expand methods in order
to create and work with their own archives... Others might use the encryption,
disk-spanning or self-extracting archive features for a wide range of
archiving and backup-related tasks.

3.  Installing Help -- see the Installation section of COMPLHA.HLP

4. File list:

D1DCUs.ZIP     Delphi 1.0 DCU and DCR files
D2DCUs.ZIP     Delphi 2.0 DCU and DCR files
D3DCUs.ZIP     Delphi 3.0 DCU and DCR files
D4DCUs.ZIP     Delphi 4.0 DCU and DCR files
D5DCUs.ZIP     Delphi 5.0 DCU and DCR files
D5DCUs.ZIP     Delphi 6.0 DCU and DCR files
C1OBJS.ZIP     C++ Builder 1.0 OBJ, HPP, DCU and DCR files, and MAK/CPP files
               to go with each of the Pascal demonstration projects below
C3OBJS.ZIP     C++ Builder 3.0 package, DCU and DCR files, and BPR/CPP files
               to go with each of the Pascal demonstration 
projects below
C4OBJS.ZIP     As above, for C++ Builder 4
C5OBJS.ZIP     As above, for C++ Builder 5

***within each of the above files, you'll find:
COMPLHA.DCU   TCompLHA component (main compression component)
COMPLHA.DCR   Component palette icon for TCompLHA
SEGLHA.DCU    TSegLHA component (segmented compression, backup/restore)
SEGLHA.DCR    Palette icon

* D3DCUS.ZIP/D4DCUS.ZIP/D5DCUS.ZIP/D6DCUS.ZIP only:
COMPLH60.DPK     Package source for TCompLHA and TSegLHA components

* C1OBJS.ZIP only:
COMPLHA.OBJ/HPP C++ object and include files for TCompLHA
SEGLHA.OBJ/HPP  C++ object and include files for TSegLHA
   *.MAK        C++ project files for each of the DPR projects  noted below

* C3OBJS.ZIP, C4OBJS.ZIP and C5OBJS.ZIP only:
*.HPP     C++ include files for TCompLHA/TSegLHA
COMPLH40.*  Package files
*.BPR/.CPP      C++ project files for each of the DPR projects  noted below

***remaining files in main archive:
COMPLHA.HLP   Complete help file for TCompLHA component set
COMPLHA.KWF   Keyword file to integrate into Delphi 1.0/2.0's help

LHADEMO.DPR   TCompLHA drag-and-drop archive manager project file
LHAMAIN.PAS/DFM Source and form for the above (many examples here!)

COMPLHS.PAS  Full-source derived component example: TCompLHAStream.

SFX.DPR       Formless self-extracting archive extractor (see comments at top)
MAKEEXE.DPR   Project to create arbitrary self-extracting archives using SFX
MAKEEXEF.PAS/DFM  Source and form for the above (useful BuildTree method is worth a look!)

SEGDEMO.DPR   Project for creating segmented archives using TSegLHA
SEG*.PAS/DFM  Source and forms for the above

ADDRESS.DPR   Project showing easy database backup/restore with TSegLHA
ADRMAIN.PAS/DFM  Source and form for the above

HISTORY.TXT    Development history
LHAFAQ.TXT     Have a look at the Contents, at the very least!
README.TXT     This file


5. Synopsis (for Web/BBS listings etc):

Filename: COMPLHA.ZIP   (File date: May 2001  Time: 6:00)
1 line description:
TCompLHA v6.0 LHArc/LHA Compatible Compression Component for Delphi & C++ Builder
Uploader name & email: Peter Hyde, software@spis.co.nz
Surface address: SPIS Ltd PO Box 19-760, Christchurch, NEW ZEALAND
Special requirements: Any version of Delphi or C++ Builder
Shareware payment required from private users: $NZ149 (about $US59)
Source included: No (available separately)

Brief description:

TCompLHA helps you create and manage archives compatible with the popular freeware
LHArc and LHA utilities (and shell utilities like WinZip). Simple one-step methods
such as Scan, Compress, Expand, Delete and Verify make archive management very easy.

Key properties such as ArchiveName, CompressionMethod, Confirm, and FilesToProcess
provide easy control over how files are processed, including a fully customizable user
interface.

Extended features like encryption, wildcard handling, disk spanning, one-step
backup/restore and self-extracting archive support make TCompLHA a very versatile
compression, file management, backup and installation tool set.

TCompLHA comes with an extensive demonstration, source examples and comprehensive
help and keyword files.  Component source is optionally available for registered users.
Supports Delphi 1 to 6, C++ Builder 1/3/4/5 and Kylix
(i.e. all released versions of all three products).


6. Registration

If you are a registered user of an earlier TCompLHA version, please see
the special upgrade pricing in the Registration section of COMPRESS.HLP.

*** If you are a registered user of our TCompress component set, please see
the special crossgrade pricing in the Registration section of COMPLHA.HLP.

Full registration information and a handy form are in COMPLHA.HLP.
When you register, you will receive an updated version (if any), and
a personal registration ID to eliminate the occasional reminder dialog.

On registration, you can optionally also order the source code of the TCompLHA
component (COMPLHA.PAS) or TSegLHA component (SEGLHA.PAS).  Please specify when
ordering which source units you require (if any).

Registration prices (NZ residents add 12.5% GST):

Product Registration/License: $NZ149 (about $US59)
Optional CompLHA source (add): $NZ99 (about $US39)
Optional SegLHA source (add): $NZ99 (about $US39)
Optional airmailed disk  (add): $NZ10 (no charge if emailed)

Payments may be made by Mastercard or Visa (please specify card 
number, expiry date, and name of holder). You may also pay by a
bank check in your own currency to the equivalent value at
current exchange rates.

To register, please print or email the form in the Registration section of
COMPLHA.HLP or use the online registration at our Web site.

Contact: SPIS Ltd, PO Box 19-760, Christchurch, New Zealand
Fax: +64-3-384-5138   software@spis.co.nz    http://www.spis.co.nz
Online registration: https://usa.spis.co.nz/spis/services.dll?SV:SPISREGCCEntry::TCompLHA


**** Other products you should check out:

* TCompress File and Database Compression Components for Delphi and C++ Builder. Where
While TCompLHA is ideal for managing LHA-compatible archives, the TCompress component
suite handles a whole range of general purpose compression tasks -- compressing data
in memory, database text, rich text and image fields, program resources and single
or multi-file archives using two built-in compression algorithms -- plus you can
easily add your own!

Download from http://www.spis.co.nz/compress.htm

* TSegCompress by Ken McClain (nickelsoftware@mindspring.com). TSegCompress
builds on TCompress to allow you to create segmented archives, e.g. for saving
one archive onto several floppies. (Ken also wrote the TSegLHA component in TCompLHA).

Download from http://www.mindspring.com/~kmcclain/segment.htm

* FREE TurboNote on-screen sticky note program -- small, simple,
and incredibly convenient.
Download from: http://TurboPress.com/tbnote.htm


*** For late-breaking information, new versions, new components,
tips and tricks, visit our Web page at
http://www.spis.co.nz/compLHA.htm
