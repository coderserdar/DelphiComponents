Precision Language Suite 2.5
============================

Precision Language Suite is an easy to use set of localization tools for
Delphi and FreePascal/Lazarus development environments. It allows you to
create multilingual applications in any available framework (VCL, FireMonkey,
LCL, ...) and for all supported platforms (Win32, Win64, OSX, ...).

The product consists of: 
* PLS Engine - localization functions and components designed for inclusion into your applications 
* PLS Editor - a powerful tool for preparing your translation files
* PLS Translator - a free lightweight editor intended for translators of your localization files

Precision Language Suite uses a text format for distributed localization files by default.
Translations can therefore be easily adapted to the customer needs even without installing 
a special software. This also allows your users to switch a language without the need of 
restarting the application.


PLS Engine: 
-----------
The localization engine includes TplsController - a visual component
that is designed for easier implementation of PLS Engine into your applications,
and TLanguageManager - a base class that (together with some auxiliary methods,
functions and variables) handles all the needed localization operations.
Their usage is very simple and in addition, the concrete implementation of PLS Engine
for your concrete application is offered in the form of "source code suggestions"
within the PLS Editor.

An "embedded localization" feature of PLS Engine allows you to localize your
applications directly "on the fly". You can simply click the elements in your
application and enter the captions and texts in the currently selected language.
This feature is suitable for minor corrections, as well as for a complex
localization by an external translator, or also (in the so called UserMode) for
your end users.


PLS Editor:
-----------
PLS Editor is designed for convenient preparation of your translations. This 
includes an editing of all selected language texts in a table, items navigator, 
fast filtering and searching, possibility to use predefined identifiers and 
filters, text replacements, projects management, spell checking, and more.

Although it may be used completely independently of the development environment
(even non-existing identifiers could be entered), it offers a range of tools to 
increase your productivity, such as synchronization of language files and 
source code files, transfer of selected components from your VCL forms into 
the localization project by using the clipboard, source code suggestions for
implementing the localization, etc.


PLS Translator:
---------------
This tool is intended for translators of your existing projects. It has
some limitations in compare to PLS Editor, but it is provided completely
FREE OF CHARGE.


License:
--------
PLS Engine is distributed as FREEWARE, subject to the rights and restrictions
mentioned in the license agreement. PLS Editor and PLS Translator are
distributed as FREEWARE. Read License.txt for more infomations.


Supported IDEs:
---------------
  Embarcadero Delphi/RAD Studio
  FreePascal/Lazarus IDE


System requirements for PLS Editor and PLS Translator:
------------------------------------------------------
Hardware:
  IBM compatible PC
    1 GB RAM
  240 MB free disk space (20 MB app files / 220 MB dictionaries)

Operating system:
  Microsoft Windows 2000/XP/Vista/7/8


Installation:
-------------
Run installer and follow the setup wizard instructions. 
A portable version can be simply unpacked into desired folder.

Because all the files of Precision Language suite package are installed
under the selected folder, choose some writable directory as a destination
(this means not "C:\Program Files\", or so). In other case you will not be
able to compile examples, to save modified sample projects, etc.

All the source codes, documentation and examples can be found in the selected
installation folder (and its subfolders). Add the "Source" subfolder
into the Delphi Library Path and into the Dcc32.cfg file (if you use
the command-line compiler).

TplsController component can be installed into your Delphi (or Lazarus) IDE by
using an appropriate package file that you can find in the "Packages" subfolder.

PLS Editor (PrecisionLangSuite.exe) and PLS Translator (PLSTranslator.exe) tools
can be found in the "Editor" subfolder.

On Windows Vista and above you must allow UAC elevation to administrative rights
when registering the purchased license of PLS Editor.


Change log:
-----------
- Version 2.5.2 (2014-09-30)

  PLS Editor:
  * updated: DevExpress Class Library definition
  
  PLS Engine:  
  * added: Support for Delphi XE6 and XE7
  * fixed: Loading of collection item properties that are encapsulated under a child object (TPersistent) of target component  


- Version 2.5 (2013-10-20)

  PLS Editor:
  * updated: Microsoft Terminology Collection (a full dictionary is now included by default)
  * added: Precision Apps dictionary (a collection of terms used in Precision software & consulting products)
  * improved: Auto translation feature has been significantly improved, by:
       * Support for using any number of dictionaries at once, including prioritizing
  	   * API Keys support for web translation services (keys are stored as encrypted)
       * SSL support for web based translation services
       * Custom translation services can be defined and used
       * TMX (Translation Memory eXchange) files can be used as dictionaries
       * TBX (TermBase eXchange) files can be used as dictionaries
       * Report with warnings and errors is available after finishing the translation
  	   * Translation of one item can be done via selected dicitonary and the confirmation dialog
  	   * Translation is now available directly from item's edit dialog
       * Caching is now applied on all used dictionaries
  	   * New options to "Save project as dictionary" and "Open dictionary for editing"
  	   * A better handling of Delphi formatting macros (%d, %s, etc.)
  * added: Spell checking (using Hunspell)
  * added: Support for user-defined extensions (that can be written in JavaScript, VBScript, Pascal Script, and other scripting languages)
           including a full application interface for accessing data and functions of the editor, that is available via OLE Automation.
  * added: Extension Manager dialog (to setup your extensions)
  * added: "Extension Editor" tool (for creating new extensions)
  * added: Bookmarks on items
  * added: Turkish translation of PLS Editor
  * improved: Find dialog gets an initial text from current filter
  * improved: An option to include (or exclude) item identifiers into search and replace operations has been added to "Find dialog"
  * improved: You can use Enter key in "filter text box" to sequentially focus each matched item of current filter
  * fixed: Non-visible columns are not processed in search and replace operations anymore
  * improved: New parser for form files (DFM, LFM, FMX) has been implemented, that solves some minor issues of previous versions
  * changed: Syntax of library file section for declaring known properties of VCL components. Now this section is obsolete,
             so if you have defined your own classes, please made this definition in newly provided Class Library Manager.
  * added: Class Library Manager - allows a convenient editing of the list of known and/or ignored component classes and their properties, which are used during the analysis of source files (DFM, FMX, LFM)
  * added: Class Library Generator utility for initial generation of custom libraries (for parsing DFM/FMX/LFM source files) based on provided source codes
  * added: Support for Delphi XE3, XE4 and XE5 (correct namespace handling for string constant imports from "resourcestring" sections)
  * added: Default language can be changed by right-clicking the desired column while holding the Shift key
  * added: Header of the default language column is displayed in bold
  * improved: Logical sorting of indexed items (ie. collections)
  * improved: Tags style option has been added
  * added: Tags could be assigned directly in item's editing dialog now
  * added: Tags descriptions (for better understanding their meanings)
  * added: Option for adding all source files from selected folder (including subfolders)
  * added: Indication of changes in source files (new items, removed items, etc.)
  * added: Checking for modifications in source files that have been changed by other application
  * added: Reporting support
  * improved: Clipboard support (now you can copy and paste items also as a simple text)
  * improved: Exported data now includes only visible languages - hidden languages (columns) were exported before
  * added: Export into HTML format
  * fixed: Import of FastReport files (correct UTF-8 handling)
  * fixed: Generation of LangConsts.pas unit:
       * If no constants were in result, LanguageChanged procedure (with an empty body) was missing in implementation part
       * Indexed constants are now limited to 1024 items to avoid huge range and memory problems
  * improved: Source code suggestions (platform related, added support for custom templates)
  * fixed: Handling custom language names in Project Options dialog
  * fixed: Storing more than one special location for saving localization files
  * fixed: Status bar info (total count of items) refresh after removing language or source files
  * and other minor improvements and fixes ...

  PLS Translator:
  * A new tool in Precision Language Suite, intended for translators of your existing projects.
    It is provided completely FREE OF CHARGE and it has the following limitations in compare to fully-featured PLS Editor:
      - Adding, removing and duplicating items is not allowed
      - Import option is not supported (while Export is available)
      - Paste from clipboard option does not support objects copied from Delphi/Lazarus IDE
      - Source code suggestions are not available
      - Source files list is hidden, source files are not parsed, nor handled in any way (but of course the list is preserved in your project file)
      - Class Library Manager is not available
      - Options for adding predefined constants and / or messages are not available
      - LangConsts.pas file is not generated and all related options are hidden (but of course it is preserved in your project file)
      - User can change default language temporarily, but it will not be saved into the project file
      - Additional locations for saving copies of .LNG files are ignored (but of course the list is preserved in your project file)
      - Shell exec options "Before open" and "After save" are not allowed (but of course these options are preserved in your project file)
      - Platform selection (VCL/FMX/LCL) in Project options dialog is available only when creating a new project
      - You can create new projects in PLS Translator, but localization items can be added only by including an existing .LNG files
      - All the functions mentioned above are disabled also in scripting interface, so you can use all available PLS Editor add-ons, but their possibilities are limited to PLS Translator features.

  PLS Engine:
  * added: Support for Delphi XE3, XE4 and XE5
  * added: Windows 64bit support
  * added: FireMonkey support
  * added: Metropolis UI support
  * added: "Object pascal" mode support for Lazarus/FPC platform
  * added: TplsController component for even easier implementation of PLS Engine into your applications
  * added: TLanguageManager.LangForm method - an alias for LangVCL method, to avoid name confusions in other supported platforms (FMX, LCL, etc.)
  * added: TLanguageManager.LangString method - works the similar way like GNU gettext, so it returns the general localized string passed as parameter
  * added: A global function _ (underscore) - as alias for global LanguageManager.LangString method, to support GNU gettext style
  * improved: _EscapeText procedure is declared as public in plsLangMan unit now
  * improved: In plsDialogs there are now available overloaded functions MessageDlg and InputQuery (without LM suffix)
  * improved: All PLS Engine units have been redesigned to provide one source for all supported platforms (VCL, LCL, FMX)
  * changed: Embedded localization support is provided for free now (has been available only on purchase)
  * changed: Support for Delphi 5 & 6 has been dropped


- Version 2.2.7 (2011-09-25)

  PLS Editor:
  * added: Japanese translation
  * added: tag "recently imported" - it is assigned to all the imported items (including the clipboard operations with VCL) and is automatically removed
           when the project is closed (you can switch the using of this tag in Preferences)
  * added: tag "do not save" - it is implemented mainly to avoid storing unwanted properties from VCL forms into the resulting language files
  * added: "Conditional import" option, that prevents overwriting of already translated items - with this option checked, the import will overwrite only the blank items and the items with "translate" tag assigned
  * added: scriptable interface for import/export (FastReport xml files are supported as a first time example)
  * improved: when importing the DFM files (and when pasting the VCL components from the clipboard) the collections and their associated properties are automatically loaded now
  * improved: TShortCut properties are now imported into the editor in a text format (ie. "Ctrl+C")
  * improved: option "Always save items sorted by identifier" has been added
  * improved: a hint with the whole cell text is now displayed also for the first column (the column with identifier)
  * improved: known VCL components definition has been enhanced with some of the Bergsoft components (http://www.bergsoft.net/)
  * fixed: LanguageChanged procedure is now divided into several routines in LangConsts.pas file, if the number of constants is too large to fit and compile inside one procedure
  * fixed: support for importing TFrame DFM files and for importing DFM files with inline frames included

  PLS Engine:
  * added: compatibility with Delphi XE2
  * changed: AllowSpecialProps property is now True by default (due to user-friendly handling of special props, like TShortCut, in the editor)
  * added: support for inline frames (TFrame) within an "embedded localization"


- Version 2.2.6 (2011-07-21)

  PLS Editor:
  * added: Italian translation
  * added: An option to toggle a tag for selected items when double-clicking the tag color (in the list of tags)
  * improved: Filtering the items now takes into account the hidden langauges (item is not displayed when it match the filter, but its language is hidden)

  ... and the changes from an earlier beta versions and patches (v2.2.4.21 and v2.2.5.2):
  * improved: A new option "Show items without tags", that is available at the bottom of "Tags" panel
  * improved: A new filter in "Items navigator", that allows you to show "only localized items"
  * improved: Searching the list of localized items by mask, ie. wildcard characters (* or ?)
  * improved: A "close icon" is now displayed inside the searching fields, when they contain any text
  * fixed: An "Access violation ..." error message after trying to add a new localized item when the list is sorted by language (not by identifier, which is the default)
  * fixed: Importing DFM files - reading the properties of type TStrings, where the value contains apostrophes or special characters
  * fixed: Importing DFM files where string property values starting with an encoded character (ie. #286,#1086, etc.).
            For standard (ASCII) character at the beginning, there were no problems of decoding the last part of the string.
  * fixed: Displaying a checked state of selected translation service and/or selected custom dictionary on Windows XP/Vista/7 machines.
 
  PLS Engine:
  * fixed: Support for localizing the structured persistent objects (ie. collections with items that contain a next collection, etc.)

  ... and the changes from an earlier beta versions and patches (v2.2.4.21 and v2.2.5.2):
  * fixed: Wrong conditional define for Delphi 6 in the LangSuiteDemo and EmbeddedEditDemo projects (resulting in an unknown method ValueFromIndex for TStringList object).


- Version 2.2.4.19 (2011-01-02)

  PLS Editor:
  * added - "Microsoft Terminology Collection" translation dictionaries
  * added - new tutorials: "Localizing DevExpress components", "Localizing BusinessSkinForm constants" a "Migration from DKLang components" (see "Help - Tutorials and examples")
  * added - support for so called external language files (these are the standard LNG files, but with a different extension, that you can normally include into your project,
              but their loading in localized application depends on the developer instead of TLanguageManager class).
              Such a files can be successfully used for localization of third-party components, that have their own localization mechanism (ie. BusinessSkinForm, DynamicSkinForm, etc.).
              This implementation is based on the work of Mr. Calin Paiusan from the Soft Consulting West Team company, www.softwestteam.ro.
  * improved - new formats supported for import: DKLang, Ini, Lists of constants (in the form of identifier=text), and LNG / LNGU internal formats
  * added - option to "commit the default values" from source files (forms, resource strings)
  * added - support for marking the language as "bi-directional"
  * improved - translation via custom dictionaries now retains an upper/lower case at the beginning of the sentence, which also applies to the accelerators (&)
  * improved - support for renaming the forms and components directly in the "Items navigagor"
  * improved - sorting the custom dictionaries menu items by alphabet
  * improved - the list of default languages for the editor has been extended by the languages that are the part of "Microsoft Terminology Collection"
  * improved - the speed of loading and saving the projects, the speed of synchronization with source codes (support for work with very large projects.)
  * fixed - importing .RC and .DRC files, that have been modified by the Delphi Translation Editor (resp. by its newer versions).
            Translation Editor saves the modified files with an original extension .RC, but converts their format to .DRC and creates an additional .RCN file
            with informations, that were originally stored in the .RC file itself. Now you can import also these modified resource files into the Precision Language Suite.

  PLS Engine:
  * added - full support for FreePascal/Lazarus IDE
  * added - TLanguageManager.BiDiMode property
  * added - TLanguageManager.ActiveFiles property, that contains a list of currently loaded language files
  * improved - "ButtonElevation" parameter for MessageDlgLM function (author of this improvement is ouiouioui, www.toutenvrac.org)
  * added - unit "xplsDKLang.pas", that contains functions for easier migration from DKLang localization components (see also "Help - Tutorials and examples - Migration from DKLang components")
  * added - "embedded localization" now fully supports for FreePascal/Lazarus IDE
  * added - "embedded localization" now supports saving the changes into the compressed file format (lngz)


- Version 2.1.3.17 (2010-12-17)

  PLS Editor:
  * added - Hungarian translation
  * added - Tags for localized items, including the support of automatic tagging. You can use the default tags ("translate", "automatically translated",
            "do not localize", "exclude from langconsts"), as well, as you can define custom tags by creating your own tag library.
  * added - Custom dictionaries for automated translation. Customizable rules and possibility to simultaneous usage with a web translation services.
            Any Precision Language Suite project can be saved as your translation dictionary (by saving it into the "Templates\Dictionaries" folder).
  * added - Resource strings localization. You can import resources from PAS, INC, DPR, RC and DRC files.
  * added - One-time import of Delphi form files (DFM), implemented as an addition to automatic synchronization of Delphi form files and pasting the components from clipboard.
  * improved - Importing multiple files at once, including auto creation of languages defined in resource files (.RC)
  * improved - Automatic synchronization of VCL files now supports also PAS, INC and DPR formats
  * added - Pre-processing and post-processing support. You can execute any defined external application before opening and after saving the project.
  * added - Support for storing the localization files in a compressed (pk-zip compatible) file format (LNGZ extension).
            This applies for saving the files into an additional locations (see the "Project options" dialog). Your source files still remain in the standard LNG format.
  * improved - Duplicate function now creates the correct sequence of indices for an "array constants" and indexed names of other items.
  * improved - Saving the custom filters for Items navigator (by clicking the magnifier icon in a text search field)
  * improved - Language file version info (author, version, etc.) is synchronized with project file info
  * improved - Opening the libraries (.plp files) for editing directly from the Precision Language Suite IDE
  * improved - Hot keys for activating the search field (Ctrl+G) and turning off the filter (Ctrl+Q) have been implemented
  * added - Option for "start editing when typing" has been added, and you can use it instead of "incremental searching".
            It starts the editing of an item immediately when you start typing, without the need of pressing the F2 key.
  * fixed - Support for relative paths of VCL files, that are included into the project
  * fixed - Unit SysUtils is now correctly included in the LangConsts.pas generated file, when indexed constants are used
  * fixed - In the generated LangConsts.pas file, long texts (greater than 255 characters) are already correctly divided
  * fixed - Texts with apostrophes (') are already generated correctly for the LangConsts.pas file

  PLS Engine:
  * added - Delphi XE support
  * added - Resource strings localization. See LangResourceStr function in plsLangMan.pas, see the LangSuiteDemo project and an Import function in the editor.
  * added - Support for loading the localization files from a compressed (pk-zip compatible) file format (LNGZ extension). This feature can be enabled by defining a conditional directive "PLS_LNGZ".
  * fixed - Methods GetLangPrimaryCode and GetLangSubCode have been updated according to the current language identifiers specification (see http://msdn.microsoft.com/en-us/library/dd318693(VS.85).aspx)
  * added - GetLangCountry method, that returns a whole language and country identifier (ie. "en-US")
  * added - {$M+} directive
  * added - Support for loading localization files encoded in UTF-8 also in older versions of Delphi (7 through 2007).
            So, for these older versions of Delphi, there is no longer need to convert the language files created in the editor into the appropriate code page of the language.


- Version 2.0.1.2 (2010-06-10)

  PLS Editor:
  * added - Russian localization
  * added - French localization
  * added - Translating texts using a Bing service (Microsoft Translator)
  * added - Import and Export from and to the CSV format (as a next possibility
            of using an external translator)
  * added - An option to save the language files into additional folders
            (each time the project is saved)
  * improved - Support for loading an inherited forms and components from VCL
               files, including the example of localizing the inherited forms
  * improved - Support for loading the VCL files in binary format
  * improved - Like the language files, a constants file (LangConsts.pas) is
               now backed up when you save a project
  * fixed - Support for special characters (eg Czech or symbols) when loading
            VCL files
  * fixed - Property values, which contain multiple lines of text, are now supported
            when loading the VCL files
  * fixed - Support for loading properties of a type "Items.Strings", "Items.Text",
            "Lines.Strings" and "Lines.Text" from the VCL files
  * fixed - Fatal error ("Access violation ...") in the "Replace" function
  * improved - Using "Replace" you can now also change the names of item identifiers
               (forms, components, properties)
  * fixed - In the "Find dialog" the text labels now correspond to currently selected
            interface language, even if it is changed without restarting the editor
  * and more minor improvements and fixes

  PLS Engine:
  * added - "plsDialogs.InputQueryMemoLM" (function with multiline texts support)
  * added - Routines for "embedded localization" support
  * added - New examples (embedded localization, localization from scratch,
            performance demo)
  * added - Example of localizing the inherited forms (in the product help reference)
  * added - Embedded localization: this newly added feature allows you to localize your
            applications directly "on the fly". You can simply click the elements in your
            application and enter the captions and texts in the currently selected
            language (more information can be found in the product help reference).


- Version 1.0.1.83 (2009-09-22)

  * first publicly released version


Used third-party components and libraries:
------------------------------------------

PLS Editor:
- Embarcadero Delphi Professional, http://www.embarcadero.com/
- TMS Component Pack Professional, http://www.tmssoftware.com
- VirtualTree, http://www.soft-gems.net
- Precision Language Suite, http://www.be-precision.com/products/precision-langs/
- pdScript (embedded), http://www.be-precision.com/products/pdscript/
- PNG Components, http://www.thany.org
- THDDInfo Delphi component, http://artsoft.nm.ru, artsoft@nm.ru
- SynEdit, http://synedit.sourceforge.net/
- TmPasParser, Martin Waldenburg
- SciZipFile, Patrik Spanel - scilib@sendme.cz
- MSXML SDK, http://msdn.microsoft.com/en-us/library/ms760399(VS.85).aspx
- Open SSL, http://www.openssl.org/
- Hunspell library, http://hunspell.sourceforge.net/ (including various 3rd-party dictionaries)
- Google Translate API, https://developers.google.com/translate/
- Microsoft Translator HTTP interface (REST), http://www.microsofttranslator.com/dev/
- Microsoft Terminology Collection, http://www.microsoft.com/language/en-us/default.aspx
- Flot, http://www.flotcharts.org/
- InnoSetup, http://www.jrsoftware.org
- ISTool, http://www.istool.org/
- IcoFX, http://icofx.ro/
- Rumshot, http://www.shellscape.org/rumshot/
- Fugue Small Icons, http://www.pinvoke.com/
- Silk Icons, http://www.famfamfam.com/lab/icons/silk/
- Developers Icons, http://sekkyumu.deviantart.com/art/Developpers-Icons-63052312
- Human O2 Icons, http://schollidesign.deviantart.com/art/Human-O2-Iconset-105344123
- Oxygen Icons, http://www.oxygen-icons.org/

PLS Engine:
It does not use any third-party components, but the source code
contained in the standard Delphi (resp. FPC/Lazarus) development environment.
The only one exception is an optional support for packed file format:
- SciZipFile, Patrik Spanel - scilib@sendme.cz


Credits:
--------
- TplsController component is being developed in cooperation with Michal Mutl (MiTeC company), http://www.mitec.cz/
- Support for "elevation required" shields in plsDialogs unit has been implemented by Alexandre Veuillet, http://www.toutenvrac.org/


Contact:
--------
Precision software & consulting
e-mail:  info@be-precision.com
www:     http://www.be-precision.com
support: www.be-precision.com/support
         support@be-precision.com
forum:   www.be-precision.com/forum
rss:     www.be-precision.com/rss_en.xml


========================================================
Copyright (c) 2008-2014  Precision software & consulting
All rights reserved.
