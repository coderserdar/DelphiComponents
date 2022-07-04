KADao Controls README
---------------------

Contents:
 1. Description
 2. Installation
 3. Disclaimer of warranty
 4. Copyrigh

DESCRIPTTION
------------
KADao Controls are FREE pack for enchancing work with KADao
KADao Controls can work ONLY With KADao 7.0 or higher version
KADao Controls include:
  KADaoDBGrid
  KADaoSortByButton
  KADaoFilterByButton
  KADaoFindButton
  KADaoSeekButton
  KADaoSelectIndexButton
  KADaoCVFButton
  KADaoDateTime
  KADaoExportButton
  KADaoSearch
***********************************************************************************************************************************************************************************
KADaoDBGrid
-----------
KADaoDBGrid is a standart DBGrid descendant
Howewer you can make it descendant of your favorite dbGrid
Just open KADaoDBGrid.pas and change
TKADaoDBGrid = class(TDBGrid)
to
TKADaoDBGrid = class(MYFavoriteDBGrid)

KADaoDBGrid has an special menu attached to it
Just click on red arrows in the top left corner of the grid an a popup menu will appear

Menu contains the following topics:
Select Index            	- gives the user an opportunity to change current index (KADaoTable.TableType must be StandartTable)
Sort                    	- gives the user an opportunity to sort the table (KADaoTable.TableType must be DynasetTable) 
Filter                  	- gives the user an opportunity to filter the table (KADaoTable.TableType must be DynasetTable) 
Find                    	- gives the user an opportunity to search for specific data (KADaoTable.TableType must be DynasetTable) 
Seek                    	- gives the user an opportunity to search for specific data (KADaoTable.TableType must be StandartTable)
Quick Find              	- gives the user an opportunity to search for specific data based on currently selected column in the grid (KADaoTable.TableType must be DynasetTable)   
Quick Seek              	- gives the user an opportunity to search for specific data based on currently selected column in the grid (KADaoTable.TableType must be StandartTable) 
Show Memos              	- show memos content in the grid
Auto Size Columns		- resizes all coumns for best fit (since entire table is calculated do not use on big tables)
Fast Lookup             	- turns the property CacheLookups to True/False
Frame Index Fields      	- makes a red border across the fields that are parts of an index (works only if an index is selected) 
Choose Visible Fields   	- gives the user an opportunity to select which fields to be shown in the grid
Save as                 	- export as Excel, Paradox, DBF, TXT, HTML etc..
Select All			- Selects all records in the grid
DeSelect All			- Deselects all records in the grid
Cut				- Cut records to Clipboard	
Copy				- Copy records to Clipboard
Paste				- Paste records from Clipboard (MSOffice format fully supported)
Office-compatible Clipboard	- Turns MSOffice Clipbord formatting ON/OFF

Specific properties:

Property ShowGridMenu         : Boolean     Enables/disables ability to show the attached menu
Property MenuTitles           : TStringList Contains menu titles
                                            You can change the text of the titles simply by changing text in this property
                                            If you put an ! at the start of the menu title then this menu item will be INVISIBLE
Property FrameIndexFields     : Boolean     Wnables/disables ability to irame index fields 
Property AutoSizeColuns       : Boolean     When True resizes all coumns for best fit, when false restores DBGrid defaults 
Property FindFieldsOnPaste    : Boolean     When True all fields to be pasted are searched in target table otherwise paste is based on column order
Poperty OfficeClipboardFormat : Boolean     Turns MSOffice Clipbord formatting ON/OFF

***********************************************************************************************************************************************************************************
KADaoSortByButton
KADaoFilterByButton
KADaoFindButton
KADaoSeekButton
KADaoSelectIndexButton
KADaoCVFButton
KADaoDateTime
KADaoExportButton
---------------------
All these buttons are attached to the same function in the KADaoDBGrid 
Just set DataSource property to an DataSource attached to an kadao table and all will be ok
Each button has an CallBefore property
if CallBefore is true then first is called user-attached function of the button and then the hard coded
otherwise hard coded is called first when user clicks on the button and then user-attached function
You can set Visible property of the button to false and if you want to use button functionality just
call XXXButton.Click in your code.


KADaoDateTime
-------------
a specific component for DateTime conversion
set the property DateTimeString to an string and you will get
1. Date as TDate
2. DateTime as TDateTime
3. SQLString - useful for putting the text in an SQL query
4. SQLField  - useful for putting the text in an SQL query
***********************************************************************************************************************************************************************************

KADaoSearch
-----------
KADaoSearch is an ready template for performing searches based on just one field
Just set DataSource property to an DataSource attached to an kadao table and all will be ok
	In version 2.0 a new property IcrementalSearch is added
	When True search is done as you type 
***********************************************************************************************************************************************************************************

INSTALLATION
------------
Open KADaoControls.dpk and press install button
Thats all
And sorry no support by e-mail - these components are VERY EASY for use
I am very busy now
Just bug reports are welcome :-)
LONG LIVE FREE SOURCE!

DISCLAIMER OF WARRANTY
----------------------
COMPONENTS ARE SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND. THE AUTHOR
DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
THE WARRANTIES OF MERCHANTABILITY AND OF FITNESS FOR ANY PURPOSE. THE AUTHOR
ASSUMES NO LIABILITY FOR DAMAGES, DIRECT OR CONSEQUENTIAL, WHICH MAY RESULT
FROM THE USE OF COMPONENTS.
USE THIS COMPONENTS AT YOUR OWN RISK

COPYRIGHT
---------
Copyrigh (c) 2001-2002 By KirilAntonov, Sofia, Bulgaria. All Rights Reserved.
Sites  : delphi.pari.bg or www.delphi.pari.bg, www.kadao.8m.com, www.delphiwarrior.freeservers.com



