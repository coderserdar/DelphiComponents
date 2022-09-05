                              FreeReport 2.33
            for Delphi 4/5/6/7/9 and C++Builder 4/5/6


              READ THIS FILE ACCURATELY BEFORE INSTALLING FRE2.3!

WARNING:
  If you are upgrade FreeReport from version >= 2.21.7 please read chapter 
"4. Backward compatibility" before install this version.

----------------------------------------------------------------------------
TABLE OF CONTENTS

1. Introduction
2. Capabilities
3. Install
4. Backward compatibility
5. License
6. Notes

----------------------------------------------------------------------------
1. INTRODUCTION

   FreeReport is reporting tool component. It consists of report engine,
designer and preview. Its capabilities comparable with in QuickReport 3,
ReportBuilder 3.52. It written on 100% Object Pascal and can be installed
in Delphi 4/5/6/7/9 and C++Builder 4/5/6.


----------------------------------------------------------------------------
2. CAPABILITIES

- Band-oriented report generator.
- Build-in powerful designer, also available in run-time.
- Preview like in MS Word.
- Fast like QuickReport1.
- Compact code - w/o designer smaller than QR1.
- Unlimited number of pages in prepared report.
- Multi-page reports; composite reports; subreports; groups;
  multi-column reports; master-detail-detail reports;
  cross-tab reports; two-pass reports.
- Full control over printing process; support all paper sizes.
- Set of most useful components: Text, Line, Picture, Shape, OLE object,
  RichText, RX Rich 2.0, Chart, Barcode.
- TXT, RTF, CSV, HTML export.
- Text search in prepared report.
- Add-in component TfrDataStorage intended for creating tables and queries
  in run-time like QRDesigner does. Especially for end-user reports. Supports
  Interbase Express (IBX) and ActiveX Data Objects (ADO).
- Editing pages of prepared report.
- Build-in Pascal-like interpreter for handling of building process.
- Report form can store in DFM resources, external file, BLOb field
  of DB table, or in stream.
- Ability of expanding functionality by own report components, wizards,
  function libraries.


----------------------------------------------------------------------------
3. INSTALL

   Before installing, remove any other version of FastReport from the Delphi
component palette. Create a folder under your Delphi folder, typically Fr230
and decompress the files in 230core.zip to this directory.
NOTE WHEN DECOMPRESSING MAKE SURE THE USE EXTRA FOLDER INFO OPTION IS CHECKED,
THIS WILL CREATE ANY SUB FOLDERS AUTOMATICALLY.

   FR2.3 includes full source code. Trial version contains almost all sources
(only one compiled unit). Before installing, you should copy this unit
from ..\FR\LIB_DX to ..\FR\SOURCE.

3.1. Delphi 5.0.
   If you want JPEG images support open the file \fr230\source\FR.inc
with any text editor and uncomment the required string, then save the file -
this will allow you to work with JPEG images.
   Run Delphi, choose "File/Open..." menu command and open package
Freerep5.dpk from ..\FR\SOURCE. In package editor, choose "Compile" button
to compile package. After compiling, press "Install" button to install
package into Delphi. After installing, "FreeReport" and "FR Tools" tabs
appears in Delphi's components palette.
   Choose "Tools/Environment options..." menu command. Go "Library" tab
and add path to ..\FR\SOURCE directory to the "Library path" string.
   For adding FR help file to the Delphi help system, you should:
- copy files from ..\FR\Help\English to ..\Delphi5\Help;
- add the following lines to ..\Delphi5\Help\Delphi5.cnt:
:Index FastReport Reference=FR.hlp
:Link FR.hlp

3.2. Delphi 4.0.
   If you want JPEG images support open the file \fr230\source\FR.inc
with any text editor and uncomment the required string, then save the file -
this will allow you to work with JPEG images.
   Run Delphi, choose "File/Open..." menu command and open package
Freerep4.dpk from ..\FR\SOURCE. In package editor, choose "Compile" button
to compile package. After compiling, press "Install" button to install
package into Delphi. After installing, "FreeReport" and "FR Tools" tabs
appears in Delphi's components palette.
   Choose "Tools/Environment options..." menu command. Go "Library" tab
and add path to ..\FR\SOURCE directory to the "Library path" string.
   For adding FR help file to the Delphi help system, you should:
- copy files from ..\FR\Help\English to ..\Delphi4\Help;
- add the following lines to ..\Delphi4\Help\Delphi4.cnt:
:Index FastReport Reference=FR.hlp
:Link FR.hlp

3.3. Delphi 3.0
   Copy file FR_Class.dcu from ..\FR\LIB_D3 to ..\FR\SOURCE (if you have
trial version of FR).
   If you want JPEG images support open the file \fr230\source\FR.inc
with any text editor and uncomment the required string, then save the file -
this will allow you to work with JPEG images.
   Run Delphi, choose "File/Open..." menu command and open package
Freerep3.dpk from ..\FR\SOURCE. In package editor, choose "Compile" button
to compile package. After compiling, press "Install" button to install
package into Delphi. After installing, "FreeReport" and "FR Tools" tabs
appears in Delphi's components palette.
   Choose "Tools/Environment options..." menu command. Go "Library" tab
and add path to ..\FR\SOURCE directory to the "Library path" string.
   For adding FR help file to the Delphi help system, you should:
- copy files from ..\FR\Help\English to ..\Delphi3\Help;
- add the following lines to ..\Delphi3\Help\Delphi3.cnt:
:Index FastReport Reference=FR.hlp
:Link FR.hlp

3.4. Delphi 2.0
   Copy file FR_Class.dcu from ..\FR\LIB_D2 to ..\FR\SOURCE (if you have
trial version of FR).
   If you have TChart or TeeChartPro open the file \fr230\source\FR.inc
with any text editor and uncomment the required string, then save the file -
this will allow the TfrChart component to be installed.
   Now open Delphi 2 and select close all from the file menu this will close
the default starting project. Select Install from the component menu select
add the select browse in the dialog box select the source folder under the
folder to which you installed FreeReport then select FR_REG.PAS file, click
Ok, when you return to the install dialog window click Ok again and your
library should recompile.
   If you have any problems see installing components in your Delphi Manual.
Note: You should make a backup copy of the Delphi CMPLIB32.DCL AND SAVE IT
IN A DIFFERENT FOLDER - THIS WILL ALLOW YOU TO REPLACE YOUR LIBRARY IF IT
FAILS TO COMPILE .
   After compiling, "FreeReport" and "FR Tools" tabs appears in Delphi's
components palette.
   For adding FR help file to the Delphi help system, you should:
- copy files from ..\FR\Help\English to ..\Delphi2\Help;
- make reserved copy of file ..\Delphi2\BIN\Delphi.hdx;
- run ..\Delphi2\HELP\TOOLS\HelpInst.exe;
- open file ..\Delphi2\BIN\Delphi.hdx and choose "Keywords/Add Keyword File";
- open file FR.kwf from ..\Delphi2\HELP;
- save all work by "File/Save" menu command;
- add the following lines to ..\Delphi2\Help\Delphi.cnt:
:Index FastReport Reference=FR.hlp
:Link FR.hlp

----------------------------------------------------------------------------
4. BACKWARD COMPATIBILITY

   .frf files from FR2.2 and FR2.3 is compatible. You can use FR2.2 .frf
in FR2.3; but you can't use FR2.3 .frf in FR2.2.
   FR2.3 has new format of .frp files. Old .frp files supported, but you
can't export data from it.
   FR2.1, FR2.2 and FR2.3 is fully compatible. You should check only
highight attributes when using old .frf files in FR2.3.

   Reports builded in FreeReport versions >= 2.21.7 can't readed in current 
release of FreeReport.

   For convert your reports to curent file forrmat do:
  
   (a) remove comments ('//') in  fr.inc at line 58 {$DEFINE FREEREP2217READ}
       to enable reading reports builded in FreeReport.
   (b) install package as described in chapter "3. INSTALL"
   (c) Open project and compile CnvReps.dpr located in DEMOS\CNVREPS\ folder.
   (d) Convert your reports using builded CnvReps.exe appliction.
   (e) Restore comments in fr.inc at line 58 {$DEFINE FREEREP2217READ}
       to disable reading reports builded in FreeReport.
   (f) Choose "Components" -> "Install Packages" menu command
   (g) Select in list "FreeReports 2.32 Components" and press "Edit" button
       to open FREEREPx.DPK
   (h) In package editor click on "Compile" button to recompile package.

----------------------------------------------------------------------------
5. LICENSE

  This version of FreeReport based on FastReport ver. 2.32 and released 
to public domain by author on LGPL license.

  Before starting using this library please read carefully license in file 
license.txt (or translated to russian in license-rus.txt).


----------------------------------------------------------------------------
6. NOTES

   All units written by author, except the following:
 - RichEdit editor taked from \DEMOS\RICHEDIT.
 - Delphi version determining taked from RX 2.60.
 - TfrSpeedButton component is based on RX TrxSpeedButton.
 - Barcode component (Barcode.pas) is written by Andreas Schmidt
   and friends (freeware) and adopted for FR by Olivier Guilbaud
   (FR_BarC.pas unit). See Barcode.pas and FR_BarC.pas units for details.
 - RoundRect add-in is written by Olivier Guilbaud. See FR_RRect.pas
   unit for details.

   Also removed some "borrowed" code:
 - Menu with bitmaps was copied from D4 sources, but now totally recoded.
 - Printer controlling (FR_Prntr.pas) was copied from QR1 sources, but now
   totally recoded.

   Images - MS Word, ReportBuilder, QuickReport, Delphi\Images.
   Ideology - ReportBuilder, QuickReport, 1S-Bookkeeping 6.0.


----------------------------------------------------------------------------
Author:    Alexander Tzyganenko

For support please contact:

Home pages:http://www.fast-report.com/en/fr23.php
           http://freereport.sf.net
           (english site)
E-mail:    free@fast-report.com

