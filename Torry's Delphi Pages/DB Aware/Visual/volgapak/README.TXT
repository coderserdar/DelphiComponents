README.TXT for VolgaDB Controls library 3.8 - Released December, 2003

VolgaDB Controls library is Copyright (C) 2000-2003, Vlasova Olga. All rights reserved. 

E-mail: info@volgadb.com
More downloads and support are on http://www.volgadb.com

Contents:
  1.0 Introduction
  2.0 Updates
  3.0 Install and Uninstall
  4.0 Demo application
  5.0 License Agreement
 

1.0  INTRODUCTION

*******    VolgaDB Controls library contains TVolgaDBGrid, TVolgaDBEdit,  
*******    TVolgaCalendar, TVolgaPeriod, TVolgaFindEdit, TVolgaMeter
*******    for Delphi 4,5,6,7 and C++Builder 4,5,6
*******    Additional package for printing TVolgaDBGrid with FastReport

TVolgaDBGrid is improved DBGrid and fully replaces standard TDBGrid. Column button style of TVolgaDBGrid may be:
- cbsCombo - dropdown list of predefined text items from property PickList, with corresponding values in PickValues;
- cbsLookup - dropdown list from lookup table; you don't need to create new calculated or lookup field in viewing table;
- cbsCalendar - dropdown calendar for date field, used TVolgaCalendar;
- cbsCalculator - dropdown calculator;
- cbsCheck - check box in grid cell;
- cbsEllipsis - custom dialog is called;
- cbsNone - simple edit cell.
Other features: can edit calculated fields, title cells as buttons, long text as hint, Enter as Tab, can use special Selected column (with checkbox) in multiselected grids, properties FixedCols, RowsHeight, export to HTML,etc...
-----------------------------
TVolgaDBEdit replaces TDBEdit, TDBComboBox, TDBLookupCombo, TDBLookupTree andTDBDatePicker in one component. TVolgaDBEdit may be DB-aware and non DB-aware. It has following dialog styles:
vdsEdit - simple edit box;
vdsCustomDlg - simple edit box with [...] button for custom dialog call;
vdsCombo - dropdown list with predefined Items and corresponding Values;
vdsLookup - dropdown list with lookup Items from lookuptable;
vdsCalendar - dropdown calendar for date editing;
vdsCalculator - dropdown calculator;
vdsTree - dropdown treeview filled from lookup table.
TVolgaDBEdit supports ENTER and Up/Down keys for navigating between several TVolgaDBEdit components and Space key to drop dropdown list.
-----------------------------
TVolgaCalendar - component for date choice from calendar. It contains full calendar for one month with buttons for change date.You may change current month, current year, select month directly from dropdown list of months (by clicking on month name), select today date (by clicking on today panel) or select date from calendar. 
-----------------------------
TVolgaPeriod is component for change period (range) of date. It can turns over months, quarters, half years and whole years. Start and end date of current range are saved in properties. Kind of turned range may be changed at run time from popup menu.
-----------------------------
TVolgaFindEdit is component for incremental searching or filtering data in linked dataset.
-----------------------------
TVolgaMeter is simple progress bar with text.
-----------------------------
TFRVGrid is printing component for TVolgaDBGrid using FastReport

2.0  UPDATES

	See http://www.volgadb.com for news, updates and additions.

3.0 INSTALL and UNINSTALL

	To install VolgaDB Controls library unzip VolgaPak.zip in any folder, in Delphi IDE choose menu "File. Open...", then find runtime package VolgaPDx.dpk, then click button Compile. After this move VolgaPDx.bpl to Windows system folder. Then open design time package dVolgaPDx.dpk and click button Install. Add path in Environment to folder with Volga components. Additional package FRVD4.dpk is used for installing TFRVGrid component
	To uninstall VolgaDB Controls library in IDE choose menu "Component. Install packages", then find line "VolgaDB Controls library" and click button "Remove". Then delete folder with component and demo.

4.0 DEMO APPLICATION

	VolgaDB Controls library has demo application with full source code, exe-file and sample datatbase in Volga format. This application uses freeware VolgaDB Engine Lite edition. Please download it from http://www.volgadb.com/download.htm page for compile demo application. See how small may be an application with database support!

5.0 LICENSE AGREEMENT

VolgaDB Controls Library is freeware and includes full source code.
