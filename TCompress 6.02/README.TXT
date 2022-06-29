               TCompress Component Set V6.01
        File and Database Compression Components
             for Delphi, Kylix and C++ Builder

  Copyright (c) 1995-2001  South Pacific Information Services Ltd
        Fax: +64-3-384-5138  Email: software@spis.co.nz
         Web:  http://www.spis.co.nz/compress.htm

>>>>> If you ordered the registered version of TCompress, see REGINFO.TXT

These components are fully functional SHAREWARE.

You are encouraged to pass them on, upload them to BBS, Web and 
ftp sites and generally spread them around as much as you can
-- but please keep all the files together!

If you find TCompress useful, registration will get you the
latest version, a personal ID to eliminate the occasional
reminder message, access to our highly regarded (and, I have
to say it, rarely needed) support AND a nice warm
fuzzy feeling.

Registration details are at the end of this file.

===== ALSO SEE COMPRESS.HLP AND FAQ.TXT FOR MUCH MORE INFORMATION =====

1. Key features:

* Standard support for ALL versions of Delphi, Kylix and C++ Builder
* Thread-safe NATIVE compression -- no DLL, OCX worries
* Multi-file compressed archives using the TCompress component
* Database BLOB (memo, image) compression with TCDBMemo,
  TCDBimage and TCDBRichText
* In-memory compression using streams
  (file/memory/blob/resource<-->file/memory/blob)
* Event hooks for customizable user interaction
* Built-in RLE (Run-Length Encoding) and LZH5
  (Lempel-Ziv-Huffman) compression
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

*   Unzip the appropriate DnDCUS.ZIP file (e.g. D6DCUS.ZIP is
    for Delphi 6.0, C5OBJS.ZIP is for C++ Builder 5.0)
*   Delphi 1.0/2.0: install COMPRESS.DCU and COMPCTRL.DCU
    into your component library
*   Delphi 3.0+: Install the compdb60.dpk package into your
    component library  (File|Open|compdb60.dpk, Compile|Install)
*   C++ Builder 3.0+: Install the compdb60.bpk package into
    your component library (File|Open|compdb60.dpk, Compile|Install)
*   C++ Builder 1.0: Install COMPRESS.OBJ/HPP and COMPCTRL.OBJ/HPP
     into your component library
*   Load the COMPDEMO project, compile and run it (if you are
    using Delphi 1.0, see the notes at the top of COMPMAIN.PAS)
*   Examine COMPMAIN.PAS for numerous examples of how to use
    the compression components

Note: TCompress and its companions provide very comprehensive
data compression capabilities. Most developers might only need
to drop a TCompress component on their form and call its ExpandFile/CompressFile
methods in order to create and work with their own multi-file archives...

Others might drop TCDBMemo/TCDBImage/TCDBRichText components
on as well and simply interact with them to have text and picture data
automatically compressed when they are stored in a database.

...and some developers might use the TCBlobfield class to store all kinds
of data (sound, numbers, video etc) directly into compressed database blobs
-- see the BLOB.DPR (BLOB.MAK for C++) demonstration for an example of this.


3.  Installing Help -- see the Installation section of COMPRESS.HLP


4. File list:

D1DCUs.ZIP     Delphi 1.0 DCU and DCR files (and a Delphi 1.0 copy of BLOBDEMO.DFM)
D2DCUs.ZIP     Delphi 2.0 DCU and DCR files
D3DCUs.ZIP     Delphi 3.0 DCU and DCR files and DPK package source
D4DCUs.ZIP     Delphi 4.0 DCU and DCR files and DPK package source
D5DCUs.ZIP     Delphi 5.0 DCU and DCR files and DPK package source
D6DCUs.ZIP     Delphi 6.0 DCU and DCR files and DPK package source
C1OBJS.ZIP     C++ Builder 1 OBJ, HPP, DCU and DCR files, and MAK/CPP files
               to go with each of the Pascal demonstration projects below
C3OBJS.ZIP      As above, for C++ Builder 3
C4OBJS.ZIP      As above, for C++ Builder 4
C5OBJS.ZIP      As above, for C++ Builder 5
K1DPUS.ZIP	    Kylix 1.0 DPU and DCR files and Kylix-specific sample project

NB: if you downloaded our COMP_NOC.ZIP archive, the C?OBJS.ZIP files are not present.
    -- download COMPRESS.ZIP from our Web site to get the full file set.

***within each of the above files, you'll find:
COMPRESS.DCU   TCompress component
COMPRESS.DCR   Component palette icon for TCompress

COMPCTRL.DCU   TCDBImage, TCDBMemo and TCDBRichText (Delphi 3.0 + only)
COMPCTRL.DCR   Component palette icons for the above

* D3DCUS.ZIP, D4DCUS.ZIP, D5DCUS.ZIP, D6DCUS.ZIP:
COMPDB60.DPK   Package source for all TCompress components
COMP60.DPK     Package source for TCompress only (no BDE dependencies)

* C1OBJS.ZIP only:
COMPRESS.OBJ/HPP C++ object and include files for Compress,
COMPCTRL.OBJ/HPP and Compctrl units
           *.MAK C++ project files for each of the DPR projects
                 noted below

* C3OBJS.ZIP, C4OBJS.zip, C5OBJS.zip:
*.HPP     C++ include files for Compress/Compctrl units
COMPDB51.* Package files for ALL TCompress components
COMP51.*   Package files for TCompress only (no BDE dependencies)
    *.BPR/.CPP    C++ project files for each of the DPR projects  noted below

* D1DCUS.ZIP only:
BLOBDEMO.DFM   Delphi 1 version of the form in the main archive

* K1DPUS only:
CompCLX.PAS/DCR  GNU Public source of our CLX TCDBMemo/TCDBImage for Kylix

***remaining files in main archive:
COMPRESS.HLP   Complete help file for TCompress component set
COMPRESS.KWF   Keyword file to integrate into Delphi 1.0/2.0's help

COMPDEMO.DPR   TCompress demonstration project file
COMPMAIN.PAS   Source for the above (many examples here!)
COMPMAIN.DFM   Form for the above

BLOB.DPR       Example project for storing miscellaneous
BLOBDEMO.PAS   data (e.g. arrays, sound, AVI etc) in a
BLOBDEMO.DFM   compressed database blob. See comments
               at the top of BLOBDEMO.PAS.

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

HISTORY.TXT    Development history
FAQ.TXT        Have a look at the Contents, at the very least!
README.TXT     This file


5. Synopsis (for Web/BBS listings etc):

Filename: COMPRESS.ZIP   (File date: June 2001,  Time: 6:01)
1 line description:
TCompress v6.01 File and Database Compression Components for Delphi, C++ Builder and Kylix
Uploader name & email: Peter Hyde, software@spis.co.nz
Surface address: SPIS Ltd PO Box 19-760, Christchurch, NEW ZEALAND
Special requirements: Any version of Delphi, C++ Builder or Kylix
Shareware payment required from private users: $NZ149 (about $US59)
Source included: Some (all available separately)

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
Delphi 1 to 6, C++ Builder 1/3/4/5 and Kylix
which is all released versions of all three products.


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

Product Registration/License: $NZ149 (about $US59)
Optional Compress source (add): $NZ99 (about $US39)
Optional Compctrl source (add): $NZ99 (about $US39)
Optional airmailed disk  (add): $NZ10 (no charge if emailed)

Payments may be made by Mastercard or Visa (please specify card 
number, expiry date, and name of holder). You may also pay by a
bank check in your own currency to the equivalent value at
current exchange rates.

Please print or email the form in COMPRESS.HLP to register
your version, or use the online registration at our Web site.

Contact: SPIS Ltd, PO Box 19-760, Christchurch, New Zealand
Fax: +64-3-384-5138   software@spis.co.nz   http://www.spis.co.nz
Online registration: https://usa.spis.co.nz/spis/services.dll?SV:SPISREGCCEntry::TCompress


**** Other products you should check out:

* TSegCompress by Ken McClain (nickelsoftware@mindspring.com).
TSegCompress builds on TCompress to allow you to create segmented archives,
e.g. for saving one archive onto several floppies.

Download from http://www.mindspring.com/~kmcclain/segment.htm

* TCompLHA LHarc/LHA Archive Manager
Whereas TCompress is crafted to handle a wide range of
general-purpose compression tasks, TCompLHA is specifically
tailored to create and manage archives compatible with the
popular freeware LHArc and LHA utilities (these archives are also
compatible with other utilities like WinZip). TCompLHA can also
read TCompress multi-file archives. An exceptionally easy interface
gives you maximum results with almost no development effort, but
powerful event hooks are provided to give you full control if needed.

Download from: http://www.spis.co.nz/compLHA.htm


* FREE TurboNote on-screen sticky note program -- small, simple,
and incredibly convenient.

Download from: http://TurboPress.com/tbnote.htm


*** For late-breaking information, new versions, new components,
tips and tricks, visit our Web page at
http://www.spis.co.nz/compress.htm
