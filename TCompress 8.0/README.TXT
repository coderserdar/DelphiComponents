               TCompress Component Set V8.0
        File and Database Compression Components
             for Delphi, Kylix and C++ Builder

  Copyright (c) 1995-2004  South Pacific Information Services Ltd
        Fax: +64-3-384-5138  Email: software@spis.co.nz
         Web:  http://spis.co.nz/compress.htm

>>>>> If you ordered the registered version of TCompress, see REGINFO.TXT

These components are fully functional SHAREWARE.

You are encouraged to pass them on, upload them to Web and 
FTP sites and generally spread them around as much as you can
-- but please keep all the files together!

If you find TCompress useful, registration will get you the
latest version, a personal ID to eliminate the occasional
reminder message, access to our highly regarded (and, I have
to say it, rarely needed) support AND a nice warm
fuzzy feeling.

Registration details are at the end of this file.

===== ALSO SEE COMPRESS.HLP AND FAQ.TXT FOR MUCH MORE INFORMATION =====

1. Key features:

* Standard support for Delphi 6, 7, 8, 2005, Kylix 3 and C++ Builder 6
  (Delphi 1-5, C++ Builder 1-5 and Kylix 1-2 no longer officially supported)
* Thread-safe NATIVE compression -- no DLL, OCX worries
* Multi-file compressed archives using the TCompress component
* Files and archives up to 8 exabytes are supported (Note: memory streams 
  and their descendants are still limited to 2 GBytes in size)
* Database BLOB (memo, image) compression with TCDBMemo,
  TCDBimage and TCDBRichText
* In-memory compression using streams
  (file/memory/blob/resource<-->file/memory/blob)
* Event hooks for customizable user interaction
* Built-in LZH5 (Lempel-Ziv-Huffman) and RLE (Run-Length Encoding) compression
* Easy to add your own custom compression routines at any time
* Functions and examples to support compressed resources
  and self-extracting EXEs
* CompressString and ExpandString utility methods
* Key-based compressed data encryption
* Loads of example source included
* Full component source code available

2. Quick Installation
   (This is for experienced people in a hurry. For detailed instructions,
    DO see the Installation section of COMPRESS.HLP.
    Also see the File List below for more details).

*   Unzip the appropriate DnDCUS.ZIP file (e.g. D7DCUS.ZIP is
    for Delphi 7.0, C6OBJS.ZIP is for C++ Builder 6.0 etc.)
*   Delphi/Kylix: Install the compdb80.dpk package into your
    component library  (File|Open|compdb80.dpk, Compile|Install)
*   C++ Builder: Install the compdb70.bpk package into
    your component library (File|Open|compdb70.dpk, Compile|Install)
*   Load the COMPDEMO project, compile and run it
*   Examine COMPMAIN.PAS for numerous examples of how to use
    the compression components

Note: TCompress and its companions provide very comprehensive
data compression capabilities. Most developers might only need
to drop a TCompress component on their form and call its
ExpandFile/CompressFile methods in order to create and work with
their own multi-file archives...

Others might drop TCDBMemo/TCDBImage/TCDBRichText components
on as well and simply interact with them to have text and picture data
automatically compressed when they are stored in a database.

...and some developers might use the TCBlobfield class to store all kinds
of data (sound, numbers, video etc) directly into compressed database blobs
-- see the BLOB.DPR (BLOB.MAK for C++) demonstration for an example of this.


3.  Installing Help -- see the Installation section of COMPRESS.HLP


4. File list:

The files are split between 2 folders: DotNet (holding files and zipped DCU files needed for developing applications and running demo applications for Microsoft's .NET platform - Delphi 8 and Delphi 2005 for .NET) and Win32 (holding files and zipped DCU files needed for developing applications and running demo applications for Win32 and Linux platforms - Delphi 6,7 and Delphi 2005 for Win32, BCB6 and Kylix)

DnDCUs.ZIP     DCU and DCR files for Delphi version "n"
               Note: Delphi 2005 DCU files reside in separate archives
               depending onthe platform (ie. D9netdcu.zip for Delphi 2005
               for .NET and D9windcu.zip for Delphi 2005 for Win32)
CnOBJS.ZIP     Package, DCU and DCR files for C++ Builder version "n", plus
               BPR/CPP files to go with each of the Pascal demonstration
               projects below
Kncomp.tgz     DPU and DCR files for Kylix version "n", plus Kylix-specific sample project

NB: if you downloaded our COMP_NOC.ZIP archive, the C++ and Kylix files
    are not present. -- download COMPRESS.ZIP from our Web site to get
    the full file set.

***within each of the above files, you'll find:
COMPRESS.DCU   TCompress component
COMPRESS.DCR   Component palette icon for TCompress

COMPCTRL.DCU   TCDBImage, TCDBMemo and TCDBRichText (Delphi 3.0 + only)
COMPCTRL.DCR   Component palette icons for the above

* DnDCUS.ZIP:
COMPDB80.DPK   Package source for all TCompress components
COMP80.DPK     Package source for TCompress only (no BDE dependencies)

* CnOBJS.ZIP:
*.HPP          C++ include files for Compress/Compctrl units
COMPDB80.*     Package files for ALL TCompress components
COMP80.*       Package files for TCompress only (no BDE dependencies)
*.BPR/.CPP     C++ project files for each of the DPR projects  noted below

* Kncomp.tgz only:
CompCLX.PAS/DCR  GNU Public source of our CLX TCDBMemo/TCDBImage for Kylix
Also includes all files, including demo code, for Kylix.
NOTE: Due to Linux case sensitivity, please only use demo files included here

***remaining files in each corresponding subfolder:
COMPRESS.HLP   Complete help file for TCompress component set

COMPDEMO.DPR   TCompress demonstration project file
COMPMAIN.PAS   Source for the above (many examples here!)
COMPMAIN.DFM   Form for the above - Win32
COMPMAIN.NFM   Form for the above - DotNet

DBXDEMO.DPR    Example DbExpress project for storing and
XPRESS.PAS     displaying a dataset
XPRESS.XFM     NOTE: Only usable with Delphi 6+ and Kylix

BLOB.DPR       Example project for storing miscellaneous
BLOBDEMO.PAS   data (e.g. arrays, sound, AVI etc) in a
BLOBDEMO.DFM   compressed database blob. See comments at the top of BLOBDEMO.PAS - Win32
BLOBDEMO.NFM   compressed database blob. See comments at the top of BLOBDEMO.PAS - DotNet

RLECOMP.PAS    Example source showing how to implement a custom
               compression handler (for RLE in this case)

ARC2MEM.PAS    Example of expanding data directly from an
               archive to memory (drops into COMPMAIN.PAS)

ARC2BLOB.PAS   Example of copying compressed data directly
               from a file archive to a  compressed blob
               WITHOUT expanding it first (drops into COMPMAIN)

SELFEXTR.DPR   Example projects for creating self-extracting
SELFEXTF.PAS   archives -- in one case with a form providing
SELFEXTF.DFM   a user interface, in the other with minimum
DOINSTAL.PAS   possible overhead by using no form.
SELFXSML.DPR   Both projects tested with Delphi 2.0/3.0 only.
COMP_RES.RC

BMTEST.DPR     Example project for storing and loading
BMTESTF.PAS    compressed bitmaps from EXE or DLL resources
BMTESTF.DFM    instead of using external archives or
BITMAPS.RC     regular (uncompressed) resources

FAQ.TXT        Have a look at the Contents, at the very least!
README.TXT     This file


5. Synopsis (for Web listings etc):

Filename: COMPRESS.ZIP   (File date: November 2004,  Time: 8:00)
1 line description:
TCompress v8.0 File and Database Compression Components for Delphi, C++ Builder and Kylix
Uploader name & email: Peter Hyde, software@spis.co.nz
Surface address: SPIS Ltd PO Box 19-760, Christchurch, NEW ZEALAND
Special requirements: Delphi 6,7,8,2005, C++ Builder 6 or Kylix 3
Shareware payment required from private users: $US79
Source included: Some (full source available separately)

Brief description:

TCompress provides native components for the creation of 
multi-file compressed archives, as well as database, file,
resource and in-memory compression using streams.
Three compression methods (RLE, LZH and LZH5) are built in, with
"hooks" for the easy addition of custom compression formats.

TCompress also includes drop 'n play components for automatic
database blob, image, rich text and memo compression, based on the
VCL's TDBMemo, TDBRichText and TDBImage components.

Images compress by up to 99% when using LZH, hence there is a
massive saving in disk space and disk or network access when
using these components.

TCompress comes with an extensive demonstration, many source
examples and comprehensive help and keyword files. Supports 
Delphi 6, 7, 8, 2005, C++ Builder 6 and Kylix 3.


6. Registration

If you are a registered user of an earlier TCompress version, please see
the special upgrade pricing in the Registration section of COMPRESS.HLP.

*** If you are a registered user of our TCompLHA component set, please see
the special crossgrade pricing in the Registration section of COMPRESS.HLP.

Full registration information and a handy form are in COMPRESS.HLP.
When you register, you will receive an updated version (if any), and
a personal registration ID to eliminate the occasional reminder dialog.

On registration, you can optionally also order the source code
of the TCompress component (COMPRESS.PAS) and/or the
TCDBImage/TCDBMemo/TCDBRichText components (COMPCTRL.PAS).
Please specify when ordering if you also want these.

Registration prices (NZ residents add 12.5% GST):

Product Registration/License: $US79
Optional Compress source (add): $US59
Optional Compctrl source (add): $US59
Optional airmailed disk  (add): $US9 (no charge if emailed)

Payments may be made by Mastercard or Visa (please specify card 
number, expiry date, and name of holder). You may also pay by a
bank draft in New Zealand currency (we regret we cannot accept
company or personal cheques from non-NZ purchasers).

Please print or email the form in COMPRESS.HLP to register
your version, or use the online registration at our Web site.

Contact: SPIS Ltd, PO Box 19-760, Christchurch, New Zealand
Fax: +64-3-384-5138   software@spis.co.nz   http://spis.co.nz
Online registration: https://usa.spis.co.nz/spis/services.dll?SV:SPISREGCCEntry::TCompress


**** Other products you should check out:

* TSegCompress by Ken McClain (nickelsoftware@mindspring.com).
TSegCompress builds on TCompress to allow you to create segmented archives,
e.g. for saving one archive onto several floppies.

Download from http://www.softbreeze.net/nickel.htm

* TCompLHA LHarc/LHA Archive Manager
Whereas TCompress is crafted to handle a wide range of
general-purpose compression tasks, TCompLHA is specifically
tailored to create and manage archives compatible with the
popular freeware LHArc and LHA utilities (these archives are also
compatible with other utilities like WinZip). TCompLHA can also
read TCompress multi-file archives. An exceptionally easy interface
gives you maximum results with almost no development effort, but
powerful event hooks are provided to give you full control if needed.

Download from: http://spis.co.nz/compLHA.htm


* TurboNote+ on-screen sticky note program -- small, simple,
and incredibly convenient.

Download from: http://Turbonote.com

* Workpace -- for prevention of RSI/OOS -- keep healthy and in control

Download from http://workpace.com/spis


*** For late-breaking information, new versions, new components,
tips and tricks, visit our Web page at
http://spis.co.nz/compress.htm
