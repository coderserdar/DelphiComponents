VG VCL Library 5.2                               
==================

The set of components for Borland Delphi 2, 3, 4 & 5 and C++ Builder 3 & 4.

100% source code.

TABLE OF CONTENTS
-----------------
Latest Changes
Overview
Components
Libraries
Installation
Credits
Notes

Latest Changes
--------------
See Changes.txt

Overview
--------
    VG VCL Library contains a large collection of components, objects and 
utility routines for Delphi. It is distrubuted with full source code and 
is compatible with Borland Delphi and C++ Builder.  

    Library includes English, Dutch, French, German, Russian and Spanish  
language resources.

    VG VCL Library contains a few components that reqire RX Library. These 
components are optional. To use them you should have RX Library 2.30 
or higher to be installed.

    VG VCL Library is a freeware product. Feel free to distribute the library 
as long as all files are unmodified and kept together.

    There is no help available.

Components
----------
    TvgThread component is the TThread wrapper that allows simple thread 
    programming without writing TThread descendents.

    TMoneyString translates numeric values into their verbal representation.

    TDateTimeStorage and TCurrencyStorage components allow to edit regional 
    settings variables to make an application behavior to be independent from 
    the system configuration.

    TFormLoader component simplifies merging forms into TWinControl descendents
    and is usable for creating multi-sheet forms and outlook user interfaces.

    TMSSCScript component wrapps Microsoft Script Control and enables you to
    write and execute VBScript and JavaScript scripts within your application.
    TObjectProxy allows to use key methods and properties of Delphi classes 
    from script.

    TvgTranslator component allows to create multilanguage applications.
    It stores language-specific string values in INI files or through 
    user-defined event handlers and allows users to create their own 
    translations of program interface.
    
    TAppIniFile is a customizable pseudo INI-file that allows to read and write
    configuration parameters of application into registry or INI-files. 

    TPropStorage component allows to save and load properties of the owner 
    form, frame or datamodule from the linked TAppIniFile component.

    TvgSplitter control has the same functionality as the standart TSplitter.

    TClickEdit controls looks like standart TEdit. It allows to align an edited 
    text horizontally and has an optional button at the right side.

    TTitler is a custom panel that shows auto-scrolling text.

    TvgPageControl is the TPageControl descendent that makes available a number 
    of advanced properties of the TPageControl control of Delphi 4. 

    TvgNoteBook controls is the TNotebook descendent with 3D view.

    TvgTabSet is a custom-draw TTabSet. It allows to edit tabs orientation.

    TvgLabel is a label with special visual effects such as rotation, shadow
    and other. It also allows to execute shell extensions on mouse click.

    TvgPanel is a TPanel descendent without caption and raised bevel.

    TvgListBox, TvgComboBox, TvgTreeView, TvgListView components support a 
    number of visual improvements.                                                                  
   
    TvgTreeViewCombo component is a TTreeView control in the form of drop-down 
    combo box.
    
    TExplorerNodes component and its descendents provides you with powerful 
    instrument of handling tree-like data structures. When using explorer 
    components developer can program a complex TTreeView or TListView 
    wrapping with just a few lines of code. Library includes over 10 full-
    functional descendents of the TExplorerNodes component.

    TExplorerSource component allows to set virtual root in the TExplorerNodes
    hierarchy to browse subtree of root inside explorer controls.

    Explorer controls represent tree-like structures in a number of different 
    ways. Library includes TExplorerTreeView, TExplorerListView, 
    TExplorerListBox and TExplorerTreeCombo controls that support all 
    advanced features of the TExplorerNodes component interface.

    TvgDatabase provides developer with advanced transaction control for
    multithreaded applications. This component is usable when writing 
    application servers in the apartment threading model. 

    TvgQuery extends functionality of the TRxQuery (RX Library) component. 
    It automatically refreshes when its params or macros are changed.

    TvgSQLScript executes multiple SQL statements. TvgSQLScript provides you 
    with macro extensions of SQL like TRxQuery does.

    TvgUpdateScript is the TUpdateObject descendent that allows to execute
    multiple SQL statements when applying cached updates.

    TFieldSource is the TFieldDataLink wrapper. It filters data events
    that send TDataSource to listed fields.

    TOpenTables component manages list of open datasets. It counts references 
    on the dataset and closes it when the dataset is no more needed.

    TBDEDataSetHook component assigns events of the given dataset to simplify
    buisness-logic coding of the large datamodule.

    TDBConverter, TBDEConverter provides you with codeless intelligent 
    techinque of converting old databases into databases with new structure.

    TvgDBGrid is the TRxDBGrid (RX Library) descendent that has miscelaneous
    visual enhancements. The two different row colors make grid to look better.

    TvgQuickSearch and TRxQuickSearch provide incremental search in the TDBGrid.
    When using these components user can copy contents of the dataset into 
    Clipboard pressing Ctrl+C.

    TvgDBLookupCombo component is the TRxDBLookupCombo descendent. It has 
    an optional small button. This button allows to browse and edit data 
    dictionaries on-the-fly.

    TvgDBTreeView control is the fast and smart component that navigates 
    through tree-like datasets.

    TvgDBTreeCombo component is the TvgDBTreeView control in the form of 
    drop-down combo box.

    TvgDBMenu component implements TDBNavigator functionality in the form 
    of popup menu.

    TDBRadioButton is the standalone radio button that doesn't require 
    TDBRadioGroup to work.

    TDBExpression implements a powerful expression evaluator. It allows to 
    evaluate string expressions in the way the TDataSet.Filter property works.
    Component supports about 20 functions including aggregate functions 
    SUM, MIN, MAX and AVG.

    TvgDBText is a DB-aware version of TvgLabel

    TvgDBListBox, TvgDBComboBox are DB-aware versions of TvgListBox and 
    TvgComboBox. Components have Values property and allow to display values 
    different from stored in database.     

    TvgWordPrint component family provides developers with powerfull open 
    architecture report system for Microsoft Word. Family includes full- 
   functional components that works with data of the BDE and client datasets.

Libraries
---------
  vgUtils.pas
      General-purpose routines. Component copying, cloning, iterating. 
      Nil-able TList routines. Variant comparing, streaming. Log files. 
      String and floating-point routines. Registry and ini-file utilities. 
      Resourcestring on-the-fly localization
  vgTlHelp.pas
      List of running process (Windows 95).
  vgVCLUtl.pas
      Cursor management, controls iterating, drawing routines.
  vgDBUtl.pas      
      Datasets and fields routines. Generation of SELECT and UPDATE SQL 
      statements. Field validation (CheckXXX functions). NVL-functions.
      Clipboard support routines. Bokmarks for primary keys, refresh.
      Full search locate.  String fields autosizing.
  vgDBPrms.pas
      TParams utilities. Update statement parameters support.
  vgBDEUtl.pas
      Transaction support. On-the-fly queries. Building TUpdateSQL.
      Editing CachedUpdates. Exception translation.
  vgCachUp.pas
      Advanced editing of CachedUpdates.
  vgOleUtl.pas
      TDispInvokeCache class speeds up OLE automation up to 2 times.
  vgComObj.pas
      Helper classes for COM servers (apartment threaded factories). 
  vgSystem.pas
      Special TThread descendents. Compression classes and routines. 
      Variant compression.
  vgMemory.pas
      Native WinAPI memory manager
  ConstsRC.pas
      On-the-fly resourcestring localization of Consts.pas.
  DBConstRC.pas
      On-the-fly resourcestring localization of DBConsts.pas.
  vgBCDUtl.pas
      BCD utility routines

Installation
------------
    1. Delphi 5.
    1.1 Library includes two design-time packages: vgDVCL5.DPK, vgDDB5.DPK. 
    1.2 Add path to compiled dcu files to the Library Path string.
    2.3 Add path to compiled packages to the PATH variable.
    1.4 Compile and install packages vgDVCL5.DPK, vgDDB5.DPK.
    1.5 If you have RX library 2.75 or higher compile and install package 
    vgDRXDB5.DPK.

    2. Delphi 4.
    2.1 Library includes three design-time packages: vgDVCL4.DPK, vgDDB4.DPK, 
    vgDRXDB4.DPK. 
    2.2 Add path to compiled dcu files to the Library Path string.
    2.3 Add path to compiled packages to the PATH variable.
    2.4 Compile and install packages vgDVCL4.DPK, vgDDB4.DPK.
    2.5 If you have RX library 2.50 or higher compile and install package 
    vgDRXDB4.DPK.

    3. C++ Builder 4.
    Perform steps listed above for vgDVCL4.BPK and vgDDB4.BPK and vgDRXDB.BPK.

    4. Delphi 3.
    Perform steps listed above for vgDVCL.DPK, vgDDB.DPK and vgDRXDB4.DPK
    packages.

    5. C++ Builder 3.
    Perform steps listed above for vgDVCL.BPK, vgDDB.BPK and vgDRXDB.BPK
    packages.

    6. Delphi 2.
    6.1 In "Install components" dialog choose vgVCLRg, vgDBRg.
    6.2 If you have RX library 2.30 or higher then in "Install components" 
    dialog choose vgRXDBRg.

Credits
-------
  Resource translations
    Dutch            Jaap, jaap@umbra.demon.nl
    French           Stephane Vidouse, steff.x@infonie.be
    German           Jens Liebermann, jens@jens-liebermann.de
                     Olray Dragon, olray-dragon@pop.gun.de
    Italian          Alex Zanello, alexza@mail.nauta.it
    Spanish          Antoni Aloy, aloy@ctv.es

  Portions of code
    ComCtl98.pas     Jean-Luc Mattei, jlucm@club-internet.fr    
    vgBCDUtl.pas     Vadim Lopushansky, notabene@dak.uame.com.ua
    vgScript.pas     Tolik Tentser, tolik@katren.ru
    lzh.pas          its authors
    lzss32.pas       its authots

  Special thanks to authors of RX Library.

Notes
-----
   VG VCL Library requires next files in the system path:
     MSVCRT.DLL
     VGLZSS32.DLL
     VGI0.DLL (only for Delphi 3 or C++ Builder 3)
     VGI4.DLL (only for Delphi 4 or C++ Builder 4)
     VGI5.DLL (only for Delphi 5)

   If RX Library is 2.40 or higher make sure that RX240 conditional define 
   is defined in the VG.INC include file.

   MS Script Control OCX is available at http://www.microsoft.com/scripting.

-------------------------------------------
Vladimir Gaitanoff
WWW:             http://www.tsinet.ru/~vg/
E-mail:          vg@tsinet.ru

