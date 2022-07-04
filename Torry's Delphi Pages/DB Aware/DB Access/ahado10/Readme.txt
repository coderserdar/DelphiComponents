******************************************************************************
                      FREEWARE Delphi Access Helper For Ado
                       Copyright (c) 2005 by Kiril Antonov
******************************************************************************

KADao README
------------------
Contents:
 1. Description
 2. Installation
 3. Usage 
 4. Disclaimer of warranty

DESCRIPTTION
------------
Access Helper For Ado is a simple component to help programmers 
easy to open all database formats supported by Microsoft JET Engine via ADO
Access Helper For Ado supports following ADO Database components:
1. TAdoConnection   by Borland
2. TaoADOConnection by Aloha Oi Software
3. TKAAdoDatabase   KA Ado

Installation for Delphi
------------------------
Open and install/compile AHADO.dpk

USAGE
-----
The following properies are defined:

AdoConnection      : TComponent
--------------------------------
Set this to point to one of the following components
1. TAdoConnection   by Borland
2. TaoADOConnection by Aloha Oi Software
3. TKAAdoDatabase   (KA Ado)


Database           : String
-----------------------------
Full Path to the database file or directory
For example: 
"D:\Northwind.mdb" for an MS Access Database
"D:\" for and Paradox Database

SystemDatabase     : String
---------------------------
Full Path to the system database file (optional, may be used only on protected MS Access Databases)

DatabaseType       : String
---------------------------
Set to the type of database you want to open
(all types are listed in DatabasesList property)

DatabasesList      : TStringList
--------------------------------
Contains list of all supported database formats:
Microsoft Access
dBase 5.0
dBase III
dBase IV
Excel 3.0
Excel 4.0
Excel 5.0
Excel 8.0
HTML Export
HTML Import
Lotus WJ2
Lotus WJ3
Lotus WK1
Lotus WK3
Lotus WK4
Paradox 3.X
Paradox 4.X
Paradox 5.X
Paradox 7.X
Text

ConnectionString   : String
-------------------------------
This property will give you the resulted Connection string which can be used to connect 
to the requested database

UseIndexes         : Boolean
----------------------------
When set to true this property will set CursorLocation property 
of the underlying Ado connection to be set to use Server side Cursor

UserName           : String
----------------------------
User Name (optional - default is "Admin")

Password           : String
----------------------------
Password (optional)

DatabasePassword   : String
---------------------------
Database Password (optional, may be used only on protected MS Access Databases)

AutoUpdate         : Boolean
----------------------------
When set to true will automatically set
ConnectionString and CursorLocation properties of the underlying Ado connection

AutoOpen           : Boolean
----------------------------
When set to true will aqutomatically set connected property 
of the underlying Ado connection to True

Activate           : Boolean
----------------------------
Generates the ConnectionString and depending of the values of 
AutoUpdate and AutoOpen will set properties of the underlying Ado connection

DISCLAIMER OF WARRANTY
----------------------
COMPONENTS ARE SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND. THE AUTHOR
DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
THE WARRANTIES OF MERCHANTABILITY AND OF FITNESS FOR ANY PURPOSE. THE AUTHOR
ASSUMES NO LIABILITY FOR DAMAGES, DIRECT OR CONSEQUENTIAL, WHICH MAY RESULT
FROM THE USE OF COMPONENTS.
USE THIS COMPONENTS AT YOUR OWN RISK

For contacts:
 my e-mail: kirila@abv.bg
 my site  : www.hadao.dir.bg

Best regards
   Kiril Antonov
   Sofia
   Bulgaria