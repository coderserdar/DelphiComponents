EhLib 3.4
---------

The Library contains components and classes for Borland Delphi 
versions 4, 5, 6, 7 & 8 and Borland C++ Builder versions 4, 5 & 6.

TABLE OF CONTENTS
-----------------
Overview
Installation Library
Installation Help
Demonstration Programs
Registering and Prices
Other information
About author


Overview
--------

The Library contains several components and objects.

TDBGridEh component
TDBGridEh provides all functionality of TDBGrid 
 and adds several new features as follows:
   Allows to select records, columns and rectangle areas.
   Special titles that can correspond to several/all columns.
   Footer that is able to show sum/count/other field values.
   Automatic column resizing to set grid width equal client width.
   Ability to change row and title height.
   Allows automatic broken of a single line long title and data row 
     to a multiline.
   Title can act as button and, optionally show a sort marker.
   Automatically sortmarking.
   Ability to truncate long text with ellipsis.
   Lookup list can show several fields.
   Incremental search in lookup fields.
   Frozen columns.
   DateTime picker support for TDateField and TDateTimeField.
   Allows to show bitmaps from TImageList depending on field value.
   Allows to hide and track horizontal or vertical scrollbars.
   Allows to hide columns.
   Allows to show 3D frame for frozen, footer and data rows.
   Allows to draw memo fields.
   Multiline inplace editor.
   Proportional scrolling independently of sequenced of dataset.
   Automatically show checkboxes for Boolean fields. Allows to show 
    checkboxes for other type of fields.
   Has a procedures to save and restore layout (visible columns, columns 
    order, columns width, sortmarkers, row height) in/from registry or ini file.
   Allows to show hint (ToolTips) for text that don't fit in the cell.
   Allows to export data to Text, Csv, HTML, RTF, XLS and internal formats.
   Allows to import data from Text and internal formats.
   Can sort data in various dataset's.
   Can filter data in various dataset's.


TDBLookupComboboxEh component
 Provides all functionality of TDBLookupCombobox and adds 
 several new features as follows:
   Can have flat style.  
   Allows assign values as to KeyValue property just and to 
     display Text property.
   Allows to type (assign) values to Text property not contained in data list
     (Style = csDropDownEh). 
   Allows to hold KeyValue and Text as not affecting to each other values. 
    Take effect when KeyField, ListField, ListSource, DataField and DataSource 
    properties is empty.
   Drop down list can:
     Show titles,
     Have sizing grip,
     Automaticaly set width as sum of DisplayWidth of the list fields (Width = -1),
     Automaticaly drops on user pressed the key.
   Edit button can:
     Show DropDown, Ellipsis or Bitmap image.
     Have specified width.
   Have additional events: OnKeyValueChanged, OnButtonClick.


TDBSumList component
This component is intended for totaling sums and amounts of records in a 
TDataSet with dynamic changes. Component keeps a list of TDBSum 
objects, which contains types of group operations (goSum or goCount) 
and name sum field (goCount name of field is unnecessary).


TPrintDBGridEh component
TPrintDBGridEh provides properties and routines for preview and 
  print of TDBGridEh component with several features:
    Ability to expand rows vertically until all text is printed.
    Ability to scale grid to fit it to page width.
    Ability to print/preview title for grid.
    Ability to print/preview page header and page footer where you can 
     specify macros for current page, current date, current time and/or static 
     text.
    Automatically print/preview multiselected area of TDBGridEh if it area 
     is not empty.
    Ability to print/preview rich text before and after grid.

TPreviewBox component
TPreviewBox lets you create a customizable runtime preview.


TPrinterPreview object
TPrinterPreview lets you to record printable data in buffer for following 
output them on screen and to printer. TPrinterPreview have all functions and 
properties as in TPrinter object. You can use TPrinterPreview object similarly 
of TPrinter except some details. In TPrinter Printer.Canvas.Handle and 
Printer.Handle is the same but in TPrinterPreview PrinterPreview.Canvas.Handle
represent the metafile in that is recored the data and PrinterPreview.Handle 
represent Printer.Handle. That is mean that you have to use 
PrinterPreview.Canvas.Handle for draw operation (DrawText, DrawTexteEx, e.t.c.) 
and use PrinterPreview.Handle in functions that return information about 
printer facilities (GetDeviceCaps, e.t.c.). Global function PrinterPreview 
returns default PrinterPreview object and shows data in default preview form.

TDBEditEh component 
represents a single or multi-line edit control that can display and edit a field 
in a dataset or can works as non data-aware edit control.

TDBDateTimeEditEh component 
represents a single-line date or time edit control that can display and edit 
a datetime field in a dataset or can works as non data-aware edit control.


TDBComboBoxEh component 
represents a single or multi-line edit control that combines an edit box 
with a scrollable list and can display and edit a field in a dataset or can 
works as non data-aware combo edit control.

TDBNumberEditEh component 
represents a single-line number edit control that can display and edit a numeric 
field in a dataset or can works as non data-aware edit control.


TPropStorageEh, TIniPropStorageManEh, TRegPropStorageManEh components
Components realize technology to store component properties to/from settings 
storage such as ini files, registry etc.

--------------------
Installation Library
--------------------

If you have executable installation program (for example, EhLibSetupD7Eval.exe)
then you only need to run program and follow installation process. Setup automatically
writes all units in necessary directory, installs packages and help files in IDE.

To install files from self-extracting EhLib archive follow next instructions:

1. Delphi 8.x

Uninstall previous or evaluation version of EhLib (Old version) from Delphi 
IDE. Remove or copy to other directory files of old version to prevent 
crossing old and new version of EhLib (Including EhLib.dll, DclEhLib.dll files). 

Create directory from which you will install EhLib library 
('EhLib directory') (for example, C:\Delphi8\EhLib).

Copy files from Common and Delphi8 directories of the EhLib archive
to 'EhLib directory'.

In Delphi IDE:

Add, (if needed) 'EhLib directory' in Component->Installed .NET Components ...->
  Environment Options->Assembly Search Paths.
Add, (if needed) 'EhLib directory' in Tools->Options->Environment Options->
Delphi Options->Library->Library Path.

Use "File\Open..." menu item of Delphi IDE to open the runtime package - 
EhLibXX.Dpk.
In "Project Manager..." window, click right button above 'EhLibXX.Dll' and 
select "Build" menu to compile package.

Use "File\Open..." menu item of Delphi IDE to open the runtime package - 
DclEhLibXX.Dpk.
In "Project Manager..." window, click right button above 'DclEhLibXX.Dll' and 
select "Install VCL Package" menu to install package.


2. Delphi 4.x - 7.x:
--------------------

Uninstall previous or evaluation version of EhLib (Old version) from Delphi 
IDE. Remove or copy to other directory files of old version to prevent 
crossing old and new version of EhLib (Including EhLib.bpl, EhLib.dcp or 
EhLibXX.bpl, EhLibXX.dcp files). 

Create directory from which you will install EhLib library 
('EhLib directory') (for example, C:\Delphi[X]\EhLib).

Copy files from Common and Delphi[X] directories of the EhLib archive
to 'EhLib directory'.

Use "File\Open..." menu item of Delphi IDE to open the runtime package - 
EhLibXX.Dpk.
In "Package..." window click "Compile" button to compile the package.

By default Delphi (5, 6 and 7) place compiled EhLibXX.BPL file to the 
<Delphi path>\Projects\Bpl directory and this directory already present 
in the search PATH. But if you overwrite default BPL directory then you need
put compiled EhLibXX.BPL file into directory that is accessible 
through the search PATH (i.e. DOS "PATH" environment variable; for example, 
in the Windows\System directory).

Add, (if needed) 'EhLib directory' in Tools->Environment Options->Library->
Library Path.

After compiling run-time package you must install design-time
package DclEhLibXX.BPL into the IDE.

Use "File\Open..." menu item to open design-time package DclEhLibXX.Dpk.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register EhLib components on
the component palette. 

EhLib components have to appear on 'EhLib' page of components palette.


3. C++Builder 4.x - 6.x:
------------------------

Uninstall previous or evaluation version of EhLib from C++Builder IDE.
Remove or copy to other directory files of old version to prevent 
crossing old and new version of EhLib (Including EhLib.bpl, EhLib.bpi or 
EhLibB[X].bpl, EhLibB[X].bpi files). 


Create directory from which you will install EhLib library 
('EhLib directory') (for example, C:\CBuilder[X]\EhLib).

Copy files from Common and BCB[X] directories of the EhLib archive
to 'EhLib directory'.

Use "File\Open..." menu item of C++Builder IDE to open the runtime package - 
EhLibB[X].Dpk.
In "Package..." window click "Compile" button to compile the package.

By default Builder (5 and 6) place compiled EhLibB[X].BPL file to the 
<Builder path>\Projects\Bpl directory and this directory already present 
in the search PATH. But if you overwrite default BPL directory then you need
put compiled EhLibB[X].BPL file into directory that is accessible 
through the search PATH (i.e. DOS "PATH" environment variable; for example, 
in the Windows\System directory).

Add, (if needed) 'EhLib directory' in Tools->Environment Options->Library->
Library Path.

After compiling run-time package you must install design-time
package DclEhLibB[X].BPL into the IDE.

Use "File\Open..." menu item to open design-time package DclEhLibB[X].BPL.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register EhLib components on
the component palette. 

EhLib components have to appear on 'EhLib' page of components palette.


4. Instalation note for users who had EhLib 1.X or 2.X:
------------------------------------------------------

Note that Delphi IDE does not move components to the new default 
place on components palette when you install new version of same 
component(s). So if you already has EhLib installed, then installation 
new (3.0) version does not move EhLib components to the 'EhLib' page. 
To force Delphi IDE to move compontes to the default 
place on components palette do next:
Open menu: Component->Configure Palette.
             Select '[All]' line in Pages listbox.
             Click 'Default Pages' button.


Installation Help
-----------------

1. This version of library doesn't have help files for Delphi8.

2. Delphi 4.x - 7.x:

Copy the EhLib.hlp and EhLib.cnt files to the Delphi HELP subdirectory.
Select Help|Customize to start the OpenHelp application. Add the EhLib.cnt 
file to the Contents page, add the EhLib.hlp file to the Index and Link pages.

3. C++Builder 4.x - 6.x: 

Copy the EhLib.hlp and EhLib.cnt files to the C++Builder HELP subdirectory.
Select Help|Customize to start the OpenHelp application. Add the EhLib.cnt 
file to the Contents page, add the EhLib.hlp file to the Index and Link pages.


Demonstration Programs
----------------------

Demonstration programs use tables from the DEMOS directory 
and BDE alias "DBDEMOS".

DEMOS/DEMO1              - Demonstration of use TDBGridEh, TPrintDBGridEh, 
                             TDBLookupComboboxEh and TPreviewBox.
                           Requare EhLibBDE.Pas file from DataService directory 
                           of EhLib archive. 
DEMOS/DEMO2              - Demonstration of use TDBSumList



Registering and Prices
----------------------

The EhLib is a Shareware product. If you find it useful and want to receive 
the latest versions please register your evaluation copy.

Ehlib - without source:  $54
EhLib - source included: $87

You can read detail information about registration in ORDERS directory
or at http://www.shareit.com/programs/102489.htm.

After registration you will receive (e-mail only) address of
registered version for downloading and password for unpacking.

By registering the components you get the following advantages:

1. You will be notified about new versions of the library.
2. You will receive new versions of EhLib FREE till version 4.0.
3. You encourage the author do make the components even better.



Other information
-----------------

(1)
Information for user who is going to install evaluation version of EhLib 
or Ehlib - without source:

To avoid problem with installation check that your Delphi or 
C++ Builder Build number is equal of Delphi or C++ Builder Build number 
under which EhLib was compiled. Otherwise you can get next error "Unit <one 
of EhLib unit name> was compiled with different version of <one of VCL unit 
name>".

See Build number of Delphi and C++ Builder under which EhLib was compiled
below:

C++ Builder 4 (Build 14.11) Update Pack 1  + "C++BUILDER 4 UPDATE PACK 2"
C++ Builder 5 (Build 12.34) Update Pack 1

Delphi 4 (Build 5.108) Update Pack 3
Delphi 5 (Build 6.18) Update Pack 1
Delphi 6 (Build 6.163) 


(2)
Information for user who already have old version of TDBGridEH or TDBSumList 
or EhLib installed:

Before installation this version of EhLib uninstall previous version of 
TDBGridEh or TDBSumList or EhLib from IDE and remove or copy this files
to other directory to prevent crossing of new and old files.


About author
------------

Contact me if you have any questions, comments or suggestions:
Programmer: Dmitry V. Bolshakov
www: http://www.ehlib.com
E-mail: support@ehlib.com
ICQ # 10785451