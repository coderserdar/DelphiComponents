
SQLMemTable for Delphi / C++ Builder: README
============================================

Please read this file carefully (especially the INSTALLATION chapter) 
before installing the program to your computer.


Contents
--------

  Program information
  Company information
  Description
  Specification
  Installation
  Purchasing / Registration
  Copyright and licenses
  Warranty and guarantee
  Technical support
  Important note
  Other products recommended to use


Program information
-------------------

Program Name:
  SQLMemTable
License Types:
  Personal   (for personal use, without source code)
  Com        (for 1 developer, without source code)
  Pro        (for 1 developer, with source code)
  Team4      (for 4 developers, with source code)
  Team8      (for 8 developers, with source code)
  Enterprise (Enterprise License - for entire company, with source code)
Program Version:
  15.00
Program Release Date:
  12/25/2018
Program Purpose:
  SQLMemTable is a compact, fast and powerful in-memory table with SQL support. 
Target Environment:
  Delphi 5, 6, 7, 2005, 2006, 2007, 2009, 2010, XE, XE2,XE3,XE4,XE5,XE6,XE7,XE8,10,10.1,10.2,10.3 and 
  C++ Builder 5, 6, 2006, 2007, 2009, 2010, XE, XE2.XE4,XE5,XE6,XE7,XE8,10,10.1,10.2,10.3
  Win 32 bit and Win 64 bit supported from XE2.

Company information
-------------------

Company Name:
  AidAim Software
Contact E-mail Address:
  support@aidaim.com
Contact WWW URL:
  http://www.aidaim.com


Description
-----------


SQLMemTable is a compact in-memory database with SQL support 
for Delphi and C++ Builder. 

SQLMemTable does not require BDE and provides all the TTable's functions
such as Master/Detail relationship, Filtering, Searching, Sorting, Key, 
Range, BLOB fields, and has some advanced features such as 
BLOB data compression, multi-indexes and table restructuring, 
shareable in memory capabilities. 
It is fully compatible with all standard DB controls, 
supports calculated and lookup fields as well as 
internationalization/localization and Unicode, 
provides data importing from and exporting to any dataset, 
includes SQLConsole utility with source code, comprehensive help and demos.

SQLMemTable provides the following services:

  - SQL'92 (DML & DDL) support 
  - Multiple in-memory tables inside in-memory databases
  - Multiple memory databases
  - Referential Integrity and foreign keys support (SQL-99 compliance) 
  - Master / detail relationship
  - Creating, renaming, emptying, deleting and restructuring tables
  - Creating, editing, deleting, navigating and searching for records
  - Creating and deleting multiple indexes
  - Calculated and lookup fields
  - Filtering support
  - Using BLOB fields with data compression ability
  - Importing from and exporting to any dataset in fast and easy way
  - Internationalization/Localization and Unicode support
  - BatchMove component

SQLMemTable does not require BDE or any external drivers and has small 
footprint. Its search performance is excellent and data access speed 
is extremely fast.


Specification
-------------

Data types: 
            ftAutoInc, ftInteger, ftSmallInt, ftFloat, ftDateTime, ftDate,
            ftTime, ftBLOB, ftMemo, ftGraphic, 
            ftString (any fixed length string),
            ftCurrency, ftWord, ftBoolean, ftLargeInt, ftFmtMemo,
            ftBytes, ftWideString.
Advanced data types:
            aftExtended, aftWideMemo

Default index page size: 4096 bytes (variable SQLMemDefaultMemoryPageSize in unit SQLMemConst)
Maximum records quantity: up to 2^31 (over 2 billions). 
Maximum fields per table: 2^31 (over 2 billions). 
Maximum indexes per table: 2^31 (over 2 billions). 
Maximum index fields per index: 2^31 (over 2 billions). 
Maximum index entry size: (PageSize - 128) / 2.
Maximum field name's length: 255 characters.
Maximum index name's length: 255 characters.
BLOB fields block size: > 1 byte, default 100 Kb. 
BLOB compression: ZLib, BZip, PPM 
Search operators: <,>,=,<>,<=,>=, like, not like, is null, is not null, 
                  and, or, not, (). 



Installation
------------

1) Make sure that all copies of Delphi (or C++ Builder) are currently 
   closed and not running on the target system. Also, if you are 
   replacing an existing version of SQLMemTable, please remove all files and
   the package of the prior version before running the new setup program.

2) Unpack zip archive containing SQLMemTable into any directory. 

3) Run the INSTALL.EXE from this directory.

4) Follow on-screen instructions of step-by-step setup wizard to install 
   SQLMemTable.

5) If after the installation SQLMemTable components
   don't appear in Components Palette of the IDE, please follow the next
   instructions:

  * From the IDE, select 'Component | Install Packages...'.
  * Click the 'Add' button.
  * In the ..\SQLMemTable\Lib\Delphi# (or ..\SQLMemTable\Lib\C++ Builder#) directory,
    select the dclSQLMemTableD#.BPL (or dclSQLMemTableB#.BPL) file.
  * Click the 'OK' button to close the dialog. 
  * Finally, select 'Tools | Environment Options' from the main menu. From this dialog,
    select the 'Library' tab and insure that the
    ..\SQLMemTable\Lib\Delphi# (or ..\SQLMemTable\Lib\C++ Builder#) directory is
    included in the 'Library Path' line.

6) If you use C++ Builder 2006 add path to <INSTALL_DIR>\Lib\Delphi 2006 to 
Environment Options\C++ Options\Paths and Directories\Library pah.


Purchasing / Registration
-------------------------

Visit our site http://www.aidaim.com to purchase or register the product.


Copyright and license
---------------------

See "license.txt" file.


Warranty and guarantee
----------------------

See "license.txt" file.


Technical support
-----------------

Before you contact us, please do the following:

  - Make sure you have performed all the required steps correctly. 
  - Look at the Help files and Demos: it may already contain an answer to
    your question. A lot of people ask us something like "how do I:", 
    though the complete information is there.
  - Visit our Internet site at http://www.aidaim.com. It's a good chance 
    that you'll find the newer version of our product there.

If the problem persists, please, inform us about the following:

  - SQLMemTable version.
  - Where did you obtain SQLMemTable (http or ftp site).
  - Compiler information: Delphi or C++ Builder, Version, Edition, Service 
    Pack
  - Environmental information: your OS and Service Pack
  - Description of your problem (as much information as possible to 
    retrieve the problem).
  - Attach a test project where the problem could be reproduced (it helps 
    us to solve your issue as soon as possible)

Typically AidAim Software Support Team answer messages in 24 hours, but 
depending on singularity and difficulty of your question it may take a bit 
longer.

Should you have any ideas on improving the existing functions of this 
product after you have downloaded and used it, be easy to e-mail us. 
All registered users who buy this product may also send their offers 
to add new possibilities and/or to change the product's functions.
We consider any ideas and we may take them into account while creating 
new versions of our products.


Other products AidAim Software recommended
-------------------------------------------

All the products in this chapter are fully compatible with SQLMemTable and 
recommended to work with our product. 

AidAim Software had tested all these products released by our partner 
companies for complete compatibility with SQLMemTable. In its turn, all the 
partner companies have carried out similar testing of the compatibility
of SQLMemTable with their own products. 

All the companies whose products present here are technology partners of 
AidAim Software. It means that both partner companies guarantee the 
correct joint work of the products. In case of encountering bugs in the 
time of joint work and owing to it, the partners incur the obligation to 
resolve such problems for their clients for FREE.


                         FastReport Software
                         -------------------
Advanced report generators for Delphi and Kylix.
Web Site: http://www.fast-report.com

FastReport VCL
--------------
FastReport VCL is reporting tool component for Borland Delphi 2-6 and 
Borland C++Builder 3-5. It consists of report engine, designer and 
preview. Some FastReport possibilities are really unique. The generator 
allows you to create tables, queries and databases in run-time. Built-in 
dialog designer allows you creation of dialog forms (they can be used for 
asking some parameters before printing a report). You also able to use 
built-in Pascal-like interpreter to do rather complex data processing. 
In spite of power of FastReport, its code is quite small. Probably 
FastReport is leader in correlation of functionality/size. The FastReport 
kernel (without designer) adds to your program the small footprint (less 
than QR3), but its functionality is like in ReportBuilder's one.

FastReport CLX edition
----------------------
FastReport CLX edition is powerfull of FastReport for Borland Delphi 6 and 
Borland Kylix 1-2 for Linux. It use new CLX-library. Do you want to create 
reports for Linux as well as for Windows? Use FastReport CLX edition!

FastReport VCL 2.3 lite
-----------------------
FreeWare version of FastReport.


                            9Rays.Net
                            ---------
9Rays.Net - Flexible and powerful Delphi/C++ Builder/ActiveX trees, 
grids and editors.
Web Site: http://www.9rays.net

DBFlyTreeView Suite
-------------------
Add grid and treeview functionality with a single component. DBFlyTreeView 
is a fully customizable, data-bound grid and tree view component that 
allows you to add huge arrays of nodes. OLE drag-and-drop is supported and 
the component is customizable at both design-time and runtime. 
DBFlyTreeView supports custom colors, fonts, and alignment for each cell, 
and you can even create scrollable background wallpapers without creating 
huge bitmaps.
Included: RapidTree, FlyTreeView, PropertiesTree, DBFlyTreeView controls.

DBFlyTreeViewPro Suite
----------------------
Enable your Delphi and C++Builder applications to display and browse 
hierarchical, table, and list data. DBFlyTreeViewPro Suite is a set of VCL 
components that are a direct descendent from FlyTreeViewPro. It supports 
more than 40 types of in place editors plus your custom format. It is 
data-aware and allows you to draw any array of cells directly to a 
printer, Metafile, or Bitmap canvas.
Included: RapidTree, IsPlugeditor, FlyTreeViewPro, PropertiesTreePro, 
DBFlyTreeViewPro controls.
