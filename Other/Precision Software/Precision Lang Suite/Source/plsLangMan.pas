{------------------------------------------------------------------------------
  plsLangMan.pas

  Precision Language Suite

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    A core unit of PLS Engine - the localization system
              for Delphi and FreePascal/Lazarus

  Supported development environments:
    Embarcadero Delphi 7, 2005, 2006, 2007, 2009, 2010, XE, XE2, XE3, XE4, XE5, XE6, XE7
    FreePascal/Lazarus (tested on 1.0.8/2.6.2)

  Supported development platforms and techniques:
    Delphi VCL (Visual Component Library),
    Delphi FMX (FireMonkey components),
    Delphi Metropolis UI (VCL/FMX)
    Delphi strings (native variables),
    Delphi resource strings (localizable constants, MS Windows only)
    Lazarus LCL (Lazarus Component Library)
    FreePascal strings (native variables),
    FreePascal resource strings (localizable constants)
    Localization of general strings (GNU gettext analogy, mostly for translating 3rd-party DLL messages in run-time)

  Supported target OS platforms:
    Microsoft Windows (32 and 64-bit, via VCL/FMX/LCL)
    MacOSX (via FMX/LCL)
    Linux and others, supported by FreePascal compiler (not tested)
    iOS, Android (experimentaly, via FMX)

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.

  Copyright (c) 2008 - 2014  Precision software & consulting
  All rights reserved

------------------------------------------------------------------------------
  History:

  - Version: 2.5.2
    * added: Support for Delphi XE6 and XE7
    * fixed: Loading of collection item properties that are encapsulated under a child object (TPersistent ) of target component    

  - Version: 2.5.1
    * added: Support for Delphi XE3, XE4 and XE5
    * added: Windows 64bit support
    * added: FireMonkey support
    * added: Metropolis UI support
    * added: support for "Object Pascal" mode in FreePascal/Lazarus
    * added: TLanguageManager.LangForm method - it is an alias for LangVCL method, to avoid name confusions in other supported platforms (FMX, LCL, etc.)
    * added: TLanguageManager.LangString method - works the similar way like GNU gettext, so it returns the localized string passed as parameter
    * added: A global function _ (underscore) - as alias for global LanguageManager.LangString method, to support GNU gettext style
    * improved: _EscapeText procedure is declared as public now
    * improved: The unit has been redesigned to provide one source for all supported platforms (VCL, LCL, FMX)
    * changed: Support for Delphi 5 & 6 has been dropped
    * and other minor improvements ...

  - Version: 2.2.8
    * changed - Minor improvements

  - Version: 2.2.7
    * changed - AllowSpecialProps property is now True by default (due to user-friendly handling of special props, like TShortCut, in the editor)

  - Version: 2.2.4.19
    * added - BiDiMode property
    * added - ActiveFiles property

  - Version: 2.1.3.17
    * added - Support for localizing the resourcestrings (see LangResourceStr method and LangSuiteDemo\LangConsts.pas).
              It is recommended to use the LangConsts.pas code (or similar) for localizing the resourcestrings,
              because it ensures, that the localized text is stored (duplicated) in a persistant variable, that avoids a memory leaks.
    * added - Support for reading language files in LNGZ format (pk-zip compatible).
              You have to define PLS_LNGZ conditional directive to use it.

  - Version: 2.0.2.9
    * fixed - GetAvailableLanguages for Delphi 2007 and below (reading UTF-8 encoded files)
------------------------------------------------------------------------------}

{ A core unit of PLS Engine - the localization system for Delphi and FreePascal/Lazarus }
{$IFNDEF PLSFMX}
unit plsLangMan;
{$ENDIF}

{$M+}

interface

uses
  {$IFDEF MSWINDOWS} 
  Windows, 
  {$IFNDEF FPC} 
  Messages, 
  {$ENDIF} 
  {$ENDIF}
  Classes, IniFiles;

const
  {$IFDEF MSWINDOWS}
  // Internal message identifier to process the language change in child forms/frames/modules (for MS Windows only)
  WM_LANGUAGECHANGED = WM_APP + 512;
  {$ENDIF}
  // Suffix for the standard PLS Engine language file
  LANGFILEEXT = 'lng';
  // Suffix for compressed PLS Engine language file
  LANGFILEEXTZ = 'lngz';
  // Suffix for, so called, "user language file" of PLS Engine
  LANGFILEEXTU = 'lngu';
  // Main INI section of PLS Engine's language file
  lini_sec_main = 'precision language file';
  // INI section of PLS Engine's language file, that holds text constants and resource strings
  lini_sec_consts = 'constants';
  // INI section of PLS Engine's language file, that holds general strings (see also TLanguageManager.LangString)
  lini_sec_messages = 'messages';
  // Key for reading the language name (title)
  lini_key_name = 'languagename';
  // Key for "right to left" (bidirectional mode) flag
  lini_key_rtl = 'rtl';

type
  // A base class of PLS Engine, handles all needed localization operations
  TLanguageManager = class(TPersistent)
  private
    fAllowSpecialProps:Boolean;
    fOnLanguageChanged:TNotifyEvent;
    fRecursiveFolder:Boolean;
    fActiveFiles:TStringList;
    fFileNames:TStringList;
    fFolder:string;
    fLanguageCode:string;
    fLanguageName:string;
    fBiDiMode:TBiDiMode;
    fLangProps:TStringList;
    fLangConsts:TStringList;
    // List of all general strings
    fEMessList:TStringList;
    // List of all translations of general strings in current language
    fEMessConv:TStringList;
    fLangResStr:TStringList;
    fLastError:string;
    procedure SetLanguageCode(const Value:string);
    procedure FilterFileNames(const FileList:TStrings);
    function ReadLanguageData(Ini:TMemIniFile; IsUser:Boolean; var LangName:string):Boolean;
    function LoadLanguageFile(const FileName:string;var LangName:string;var ErrMsg:string):Boolean;
  public
    // Object constructor
    constructor Create;
    // Object destructor
    destructor Destroy; override;
    { Clears an internal table of constants, properties, texts and eventually also the list of active
      files (see ActiveFiles property). Resourcestring constants (started with @) are always cleared.

      Use this method (ie. Clear(True, False) ) in conjunction with global 'LangConsts.pas' unit,
      where the strings are assigned to appropriate variables of your application, because there is no need
      to store them in an internal table anymore. By clearing an unnecessary strings you will save some
      memory and some CPU time during the possible searching for property strings via LangText method.

      However, if you are relying on LangString method (gettext analogy) for translating text constants
      and resource strings too, never use Clear method. }
    procedure Clear(Consts:Boolean = True; Props:Boolean = True; Messages:Boolean = False; ActFiles: Boolean = False);
    // Returns all available languages from currently specified Folder in the form of "langcode=langname"
    function GetAvailableLanguages(const AvailableLanguages:TStringList):Boolean;
    { Returns all filenames that would be loaded for the given language code.
      PdkFiles is fullfilled with standard language files (.lng/.lngz), while UsrFiles is fullfilled with user-defined language files (.lngu). }
    procedure GetLanguageFiles(LngCode:string; const PdkFiles, UsrFiles:TStrings);
    // Reloads current language
    procedure Refresh;
    // Returns localized text for constant or property specified by ID. If it is not found, DefaultText is returned.
    function LangText(const ID:string; const DefaultText:string=''):string;
    { Localize the whole form, frame or module. The name of this method is a little bit misleading, but you can freely
      use this function for all supported platforms (VCL, FMX, LCL, ...). }
    function LangVCL(const Owner:TComponent; const AsClassName:String=''; const OnlyComponent:TComponent=nil):Boolean;
    { Localize the whole form, frame or module. In fact, it calls LangVCL method internally.
      It is declared to avoid confusions when TLanguageManager is used in other frameworks than VCL (ie FireMonkey, LCL). }
    function LangForm(const Owner:TComponent; const AsClassName:String=''; const OnlyComponent:TComponent=nil):Boolean;
    { Returns localized text for AString or returns itself. It works the similar way like GNU gettext, so it assumes,
      that you have defined general strings and their translations in your language files.
      Note: general strings are identified by the text (or mask), not by identifier (like properties, constants and resources are).
      However, you can freely use LangString method also for getting localized texts of defined properties, constants and resources. }
    function LangString(const AString:string):string;
    { Obsolete - use LangString method instead. It is provided for backward compatibility only. }
    function LangMess(const AMessage:string):string;
    {$IF Defined(MSWINDOWS) OR Defined(FPC)}
    // Localize the specified resourcestring - this means, it stores a new value to the resource (for MS Windows only, use inside the LanguageChanged event)
    procedure LangResourceStr(rs: PResStringRec; newStr: PChar);
    {$IFEND}
  published
    // List of currently loaded language files (including the full path). The list is populated automatically by setting the LanguageCode property.
    property ActiveFiles:TStringList read fActiveFiles;
    { Specifies the filter on file names you want to load every time the language is changed.
      Filenames may not include suffixes, but masks are allowed (ie. to load 'MyApp.en.lng', add 'MyApp' or 'My*' to the FileNames list). }
    property FileNames:TStringList read fFileNames;
    // Specifies folder where your language files are located ('AppExe\Langs' by default)
    property Folder:string read fFolder write fFolder;
    // If True, language files will be searched recursively in the specified folder
    property RecursiveFolder:Boolean read fRecursiveFolder write fRecursiveFolder default False;
    // User friendly name of currently loaded language
    property LanguageName:string read fLanguageName;
    // Loads given language [Primary or secondary language code (i.e. 'en','cs','US','CZ','GB',...) = first language file extension]
    { Returns or sets the current language code (i.e. 'en','cs','en-US','cs-CZ',...).
      This is the core property for switching language during run-time, as well as for loading an initial language on application start.
      Simply set a new value to this property to change language in a whole application. }
    property LanguageCode:string read fLanguageCode write SetLanguageCode;
    // BiDi mode of currently loaded language
    property BiDiMode:TBiDiMode read fBiDiMode;
    // List of all properties of current language
    property Properties:TStringList read fLangProps;
    // List of all constants of current language
    property Constants:TStringList read fLangConsts;
    // List of all resourcestring constants of current language
    property ResourceStrings:TStringList read fLangResStr;
    // If True then allows usage of special non-text properties like TShortCut (it slows loading of language files a little)
    property AllowSpecialProps:Boolean read fAllowSpecialProps write fAllowSpecialProps;
    // Holds the text of last LanguageManager processing error
    property LastError:string read fLastError;
    // Triggered after a new language is loaded, so you can apply it to your application
    property OnLanguageChanged:TNotifyEvent read fOnLanguageChanged write fOnLanguageChanged;
	end;

{$IF NOT DECLARED(LANGID)}
  LANGID = Word;
{$IFEND}

var
  // A predefined global instance of TLanguageManager. It is instantiated automatically, except for DLLs.
	LanguageManager:TLanguageManager;

// Returns the language code for current user locale
function GetDefaultLangCode:string;
// Returns primary language code for given locale
function GetLangPrimaryCode(aLangID:LANGID):String;
// Returns the language subcode for given locale
function GetLangSubCode(aLangID:LANGID):string;
// Returns the whole language code (ie. "en-US") for given locale
function GetLangCountry(aLangID:LANGID):string;

// Decodes the text previously encoded by escape sequences (CRLF, TAB and other special characters)
procedure _EscapeText(var Txt:string);

// Returns the text, encoded by escape sequences (CRLF, TAB and other special characters)
function _ECodeText(const Txt:string):string;

// Returns TMemIniFile based on passed .LNG file (this function ensures correct loading of UTF-8 encoded INI files in Delphi bellow 2009)
function _CreateLngIni(const AFileName:string):TMemIniFile;

// Alias for TLanguageManager.LangString method of global LanguageManager instance (to support GNU gettext convention)
function _(const AString:string):string;

implementation

uses
  SysUtils, TypInfo, Masks
  {$IFDEF PLSFMX}
    , StrUtils, FMX.Platform, FMX.Menus
  {$ELSE}
    {$IFDEF FPC} 
      , lclproc, variants
    {$ELSE} 
      , StrUtils, Menus 
    {$ENDIF}
  {$ENDIF}
  {$IFDEF PLS_LNGZ}, SciZipFile {$ENDIF} 
  ;

{$IFDEF FPC}
  {$UNDEF D2009UP}
{$ELSE}
  {$IF CompilerVersion>=20}
    {$DEFINE D2009UP}
  {$ELSE}
    {$UNDEF D2009UP}
  {$IFEND}
{$ENDIF}

const
  LANGDIR_RELPATH = 'Langs';

  CRLF = #13#10;
  Tab  = #9;
  V_CRLF = '\n';
  V_TAB = '\t';

  // Self object reference identifier to localize form, frame or module properties
  LC_Self = 'self';

  resELangFilesNotFound = 'Language not found';

{-----------------------------------------------------------------------------
  SUPPORT ROUTINES
-----------------------------------------------------------------------------}

function GetDefaultLangCode:String;
{$IF Defined(MSWINDOWS) OR Defined(FPC)}
begin
  Result := GetLangPrimaryCode(GetUserDefaultLangID);
{$ELSE}
var
  LocaleSvc: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocaleSvc)) then
    Result := LocaleSvc.GetCurrentLangID
  else
    Result:= 'en';
{$IFEND}
end;

function GetLangPrimaryCode(aLangID:LANGID):String;
begin
  case aLangID of
    $0436	: Result := 'af';
    $041c	: Result := 'sq';
    $0484	: Result := 'gsw';
    $045e	: Result := 'am';
    $1401,$3c01,$0c01,$0801,$2c01,$3401,$3001,$1001,$1801,$2001,$4001,$0401,$2801,$1c01,$3801,$2401
          : Result := 'ar';
    $042b	: Result := 'hy';
    $044d	: Result := 'as';
    $082c,$042c
        	: Result := 'az';
    $046d	: Result := 'ba';
    $042d	: Result := 'eu';
    $0423	: Result := 'be';
    $0445	: Result := 'bn';
    $781a, $201a, $141a
        	: Result := 'bs';
    $047e	: Result := 'br';
    $0402	: Result := 'bg';
    $0403	: Result := 'ca';
    $0c04,$1404,$1004,$0804,$0404
          : Result := 'zh';
    $0483	: Result := 'co';
    $041a,$101a
        	: Result := 'hr';
    $0405	: Result := 'cs';
    $0406	: Result := 'da';
    $048c	: Result := 'gbz';
    $0465	: Result := 'dv';
    $0813,$0413
        	: Result := 'nl';
    $0c09,$2809,$1009,$2409,$4009,$1809,$2009,$4409,$1409,$3409,$4809,$1c09,$2c09,$0809,$0409,$3009
          : Result := 'en';
    $0425	: Result := 'et';
    $0438	: Result := 'fo';
    $0464	: Result := 'fil';
    $040b	: Result := 'fi';
    $080c,$0c0c,$040c,$140c,$180c,$100c
        	: Result := 'fr';
    $0462	: Result := 'fy';
    $0456	: Result := 'gl';
    $0437	: Result := 'ka';
    $0c07,$0407,$1407,$1007,$0807
        	: Result := 'de';
    $0408	: Result := 'el';
    $046f	: Result := 'kl';
    $0447	: Result := 'gu';
    $0468	: Result := 'ha';
    $040d	: Result := 'he';
    $0439	: Result := 'hi';
    $040e	: Result := 'hu';
    $040f	: Result := 'is';
    $0470	: Result := 'ig';
    $0421	: Result := 'id';
    $085d,$045d
        	: Result := 'iu';
    $083c	: Result := 'ga';
    $0410,$0810
        	: Result := 'it';
    $0411	: Result := 'ja';
    $044b	: Result := 'kn';
    $043f	: Result := 'kk';
    $0453	: Result := 'kh';
    $0486	: Result := 'qut';
    $0487	: Result := 'rw';
    $0457	: Result := 'kok';
    $0412	: Result := 'ko';
    $0440	: Result := 'ky';
    $0454	: Result := 'lo';
    $0426	: Result := 'lv';
    $0427	: Result := 'lt';
    $082e	: Result := 'dsb';
    $046e	: Result := 'lb';
    $042f	: Result := 'mk';
    $083e,$043e
        	: Result := 'ms';
    $044c	: Result := 'ml';
    $043a	: Result := 'mt';
    $0481	: Result := 'mi';
    $047a	: Result := 'arn';
    $044e	: Result := 'mr';
    $047c	: Result := 'moh';
    $0450,$0850
        	: Result := 'mn';
    $0461	: Result := 'ne';
    $0414,$0814
        	: Result := 'no';
    $0482	: Result := 'oc';
    $0448	: Result := 'or';
    $0463	: Result := 'ps';
    $0429	: Result := 'fa';
    $0415	: Result := 'pl';
    $0416,$0816
        	: Result := 'pt';
    $0446	: Result := 'pa';
    $046b,$086b,$0c6b
        	: Result := 'quz';
    $0418	: Result := 'ro';
    $0417	: Result := 'rm';
    $0419	: Result := 'ru';
    $243b,$103b,$143b,$0c3b,$043b,$083b,$203b,$183b,$1c3b
        	: Result := 'se';
    $044f	: Result := 'sa';
    $7c1a,$181a,$0c1a,$081a
        	: Result := 'sr';
    $046c	: Result := 'ns';
    $0432	: Result := 'tn';
    $045b	: Result := 'si';
    $041b	: Result := 'sk';
    $0424	: Result := 'sl';
    $2c0a,$400a,$340a,$240a,$140a,$1c0a,$300a,$440a,$100a,$480a,$080a,$4c0a,$180a,$3c0a,$280a,$500a,$0c0a,$380a,$200a
        	: Result := 'es';
    $0441	: Result := 'sw';
    $081d,$041d
        	: Result := 'sv';
    $045a	: Result := 'syr';
    $0428	: Result := 'tg';
    $085f	: Result := 'tmz';
    $0449	: Result := 'ta';
    $0444	: Result := 'tt';
    $044a	: Result := 'te';
    $041e	: Result := 'th';
    $0451	: Result := 'bo';
    $041f	: Result := 'tr';
    $0442	: Result := 'tk';
    $0480	: Result := 'ug';
    $0422	: Result := 'uk';
    $042e	: Result := 'wen';
    $0820,$0420
        	: Result := 'ur';
    $0843,$0443
        	: Result := 'uz';
    $042a	: Result := 'vi';
    $0452	: Result := 'cy';
    $0488	: Result := 'wo';
    $0434	: Result := 'xh';
    $0485	: Result := 'sah';
    $0478	: Result := 'ii';
    $046a	: Result := 'yo';
    $0435	: Result := 'zu';
  else
    Result:='';
  end;
end;

function GetLangSubCode(aLangID:LANGID):String;
begin
  case aLangID of
    $0436	: Result := 'za';
    $041c	: Result := 'al';
    $0484	: Result := 'fr';
    $045e	: Result := 'et';
    $1401	: Result := 'dz';
    $3c01	: Result := 'bh';
    $0c01	: Result := 'eg';
    $0801	: Result := 'iq';
    $2c01	: Result := 'jo';
    $3401	: Result := 'kw';
    $3001	: Result := 'lb';
    $1001	: Result := 'ly';
    $1801	: Result := 'ma';
    $2001	: Result := 'om';
    $4001	: Result := 'qa';
    $0401	: Result := 'sa';
    $2801	: Result := 'sy';
    $1c01	: Result := 'tn';
    $3801	: Result := 'ae';
    $2401	: Result := 'ye';
    $042b	: Result := 'am';
    $044d	: Result := 'in';
    $082c,$042c
          : Result := 'az';
    $046d	: Result := 'ru';
    $042d	: Result := 'es';
    $0423	: Result := 'by';
    $201a,$141a
          : Result := 'ba';
    $047e	: Result := 'fr';
    $0402	: Result := 'bg';
    $0403	: Result := 'es';
    $0c04	: Result := 'hk';
    $1404	: Result := 'mo';
    $1004	: Result := 'sg';
    $0483	: Result := 'fr';
    $101a	: Result := 'ba';
    $041a	: Result := 'hr';
    $0405	: Result := 'cz';
    $0406	: Result := 'dk';
    $048c	: Result := 'af';
    $0465	: Result := 'mv';
    $0813	: Result := 'be';
    $0413	: Result := 'nl';
    $0c09	: Result := 'au';
    $2809	: Result := 'be';
    $1009	: Result := 'ca';
    $2409	: Result := '029';
    $4009	: Result := 'in';
    $1809	: Result := 'ie';
    $2009	: Result := 'jm';
    $4409	: Result := 'my';
    $1409	: Result := 'nz';
    $3409	: Result := 'ph';
    $4809	: Result := 'sg';
    $1c09	: Result := 'za';
    $2c09	: Result := 'tt';
    $0809	: Result := 'gb';
    $0409	: Result := 'us';
    $3009	: Result := 'zw';
    $0425	: Result := 'ee';
    $0438	: Result := 'fo';
    $0464	: Result := 'ph';
    $040b	: Result := 'fi';
    $080c	: Result := 'be';
    $0c0c	: Result := 'ca';
    $040c	: Result := 'fr';
    $140c	: Result := 'lu';
    $180c	: Result := 'mc';
    $100c	: Result := 'ch';
    $0462	: Result := 'nl';
    $0456	: Result := 'es';
    $0437	: Result := 'ge';
    $0c07	: Result := 'at';
    $0407	: Result := 'de';
    $1407	: Result := 'li';
    $1007	: Result := 'lu';
    $0807	: Result := 'ch';
    $0408	: Result := 'gr';
    $046f	: Result := 'gl';
    $0447	: Result := 'in';
    $0468	: Result := 'ng';
    $040d	: Result := 'il';
    $0439	: Result := 'in';
    $040e	: Result := 'hu';
    $040f	: Result := 'is';
    $0470	: Result := 'ng';
    $0421	: Result := 'id';
    $085d,$045d
          : Result := 'ca';
    $083c	: Result := 'ie';
    $0410	: Result := 'it';
    $0810	: Result := 'ch';
    $0411	: Result := 'jp';
    $044b	: Result := 'in';
    $043f	: Result := 'kz';
    $0453	: Result := 'kh';
    $0486	: Result := 'gt';
    $0487	: Result := 'rw';
    $0457	: Result := 'in';
    $0412	: Result := 'kr';
    $0440	: Result := 'kg';
    $0454	: Result := 'la';
    $0426	: Result := 'lv';
    $0427	: Result := 'lt';
    $082e	: Result := 'de';
    $046e	: Result := 'lu';
    $042f	: Result := 'mk';
    $083e	: Result := 'bn';
    $043e	: Result := 'my';
    $044c	: Result := 'in';
    $043a	: Result := 'mt';
    $0481	: Result := 'nz';
    $047a	: Result := 'cl';
    $044e	: Result := 'in';
    $047c	: Result := 'ca';
    $0450	: Result := 'mn';
    $0850	: Result := 'cn';
    $0461	: Result := 'np';
    $0414, $0814
          : Result := 'no';
    $0482	: Result := 'fr';
    $0448	: Result := 'in';
    $0463	: Result := 'af';
    $0429	: Result := 'ir';
    $0415	: Result := 'pl';
    $0416	: Result := 'br';
    $0816	: Result := 'pt';
    $0446	: Result := 'in';
    $046b	: Result := 'bo';
    $086b	: Result := 'ec';
    $0c6b	: Result := 'pe';
    $0418	: Result := 'ro';
    $0417	: Result := 'ch';
    $0419	: Result := 'ru';
    $243b	: Result := 'fi';
    $103b	: Result := 'no';
    $143b	: Result := 'se';
    $0c3b	: Result := 'fi';
    $043b	: Result := 'no';
    $083b	: Result := 'se';
    $203b	: Result := 'fi';
    $183b	: Result := 'no';
    $1c3b	: Result := 'se';
    $044f	: Result := 'in';
    $181a	: Result := 'ba';
    $0c1a, $081a
          : Result := 'cs';
    $046c	: Result := 'za';
    $0432	: Result := 'za';
    $045b	: Result := 'lk';
    $041b	: Result := 'sk';
    $0424	: Result := 'si';
    $2c0a	: Result := 'ar';
    $400a	: Result := 'bo';
    $340a	: Result := 'cl';
    $240a	: Result := 'co';
    $140a	: Result := 'cr';
    $1c0a	: Result := 'do';
    $300a	: Result := 'ec';
    $440a	: Result := 'sv';
    $100a	: Result := 'gt';
    $480a	: Result := 'hn';
    $080a	: Result := 'mx';
    $4c0a	: Result := 'ni';
    $180a	: Result := 'pa';
    $3c0a	: Result := 'py';
    $280a	: Result := 'pe';
    $500a	: Result := 'pr';
    $0c0a	: Result := 'es';
    $380a	: Result := 'uy';
    $200a	: Result := 've';
    $0441	: Result := 'ke';
    $081d	: Result := 'fi';
    $041d	: Result := 'se';
    $045a	: Result := 'sy';
    $0428	: Result := 'tj';
    $085f	: Result := 'dz';
    $0449	: Result := 'in';
    $0444	: Result := 'ru';
    $044a	: Result := 'in';
    $041e	: Result := 'th';
    $0451	: Result := 'cn';
    $041f	: Result := 'tr';
    $0442	: Result := 'tm';
    $0480	: Result := 'cn';
    $0422	: Result := 'ua';
    $042e	: Result := 'de';
    $0420	: Result := 'pk';
    $0843	: Result := 'uz';
    $0443	: Result := 'uz';
    $042a	: Result := 'vn';
    $0452	: Result := 'gb';
    $0488	: Result := 'sn';
    $0434	: Result := 'za';
    $0485	: Result := 'ru';
    $0478	: Result := 'cn';
    $046a	: Result := 'ng';
    $0435	: Result := 'za';
  else
    Result:='';
  end;
end;

function GetLangCountry(aLangID:LANGID):string;
begin
  Result:=UpperCase(GetLangSubCode(aLangID));
  if Length(Result)>0 then
    Result:=GetLangPrimaryCode(aLangID)+'-'+Result
  else
    Result:=GetLangPrimaryCode(aLangID);
end;

procedure _GetLangFiles(Dir:string;const PdkFiles,UsrFiles:TStrings;LangCode:string='';Recursive:Boolean=False);
var
	Found:integer;
	SR:TSearchRec;
begin
  {$WARNINGS OFF}
	Dir:=IncludeTrailingPathDelimiter(Dir);
  if Recursive then
  begin
    Found := FindFirst(Dir+'*.*', faAnyFile-faVolumeID, SR);
    while Found = 0 do
      begin
        if (sr.Attr and faDirectory>0) and (sr.Name<>'.') and (sr.Name<>'..') then
          _GetLangFiles(dir+sr.Name,PdkFiles,UsrFiles,LangCode,Recursive);
        Found := FindNext(SR);
      end;
    SysUtils.FindClose(SR);
  end;
  if LangCode='' then
    LangCode:='*';

  Found := FindFirst(Dir+'*.'+LangCode+'.'+LANGFILEEXT, faAnyFile-faDirectory-faVolumeID, SR);
  while Found = 0 do
    begin
      PdkFiles.Add(Dir+sr.Name);
      Found := FindNext(SR);
    end;
  SysUtils.FindClose(SR);

  Found := FindFirst(Dir+'*.'+LangCode+'.'+LANGFILEEXTZ, faAnyFile-faDirectory-faVolumeID, SR);
  while Found = 0 do
    begin
      PdkFiles.Add(Dir+sr.Name);
      Found := FindNext(SR);
    end;
  SysUtils.FindClose(SR);

  Found := FindFirst(Dir+'*.'+LangCode+'.'+LANGFILEEXTU, faAnyFile-faDirectory-faVolumeID, SR);
  while Found = 0 do
    begin
      UsrFiles.Add(Dir+sr.Name);
      Found := FindNext(SR);
    end;
  SysUtils.FindClose(SR);
  {$WARNINGS ON}
end;

// there is no rfIgnoreCase support, but when replacing ASCII characters only, it is about 10 times faster than StringReplace
procedure _FasterStringReplace(var S:string; const OldPattern, NewPattern: string; Flags: TReplaceFlags);
var
  NewStr: string;
  c:char;
  ol:Integer;
  p:Integer;
begin
  p:=Pos(OldPattern,S);
  if p>0 then
  begin
    if (Length(oldpattern)=1) and (Length(newpattern)=1) then
    begin
      if rfReplaceAll in Flags then
      begin
        c:=newpattern[1];
        while p>0 do
        begin
          s[p]:=c;
          p:=Pos(OldPattern,S);
        end;
      end
      else
        s[p]:=NewPattern[1];
    end
    else
    if rfReplaceAll in Flags then
    begin
      ol:=Length(OldPattern)-1;
      NewStr:='';
      while p>0 do
      begin
        NewStr:=NewStr+Copy(S,1,p-1)+NewPattern;
        Delete(S,1,p+ol);
        p:=Pos(OldPattern,S);
      end;
      S:=NewStr+S;
    end
    else
      S:=Copy(S,1,p-1)+NewPattern+Copy(S,p+Length(OldPattern),maxint)
  end;
end;

procedure _EscapeText(var Txt:string);
begin
  _FasterStringReplace(Txt,V_CRLF,CRLF,[rfReplaceAll]);
  _FasterStringReplace(Txt,V_TAB,TAB,[rfReplaceAll]);
end;

function _ECodeText(const Txt:string):string;
begin
  Result:=Txt;
  _FasterStringReplace(Result,CRLF,V_CRLF,[rfReplaceAll]);
  _FasterStringReplace(Result,TAB,V_TAB,[rfReplaceAll]);
end;

function _CreateLngIni(const AFileName:string):TMemIniFile;
{$IFNDEF D2009UP}
  function LoadUTF8File(var AString: UTF8String; const FileName: string; FileMode: word = fmOpenRead or fmShareDenyNone): boolean;
  var
    L: integer;
    FS: TFileStream;
    preamb:string;
  begin
    Result:=False;
    try
      FS := TFileStream.Create(FileName, FileMode);
      try
        L := FS.Size-3;
        if L > 0 then
        begin
          SetLength(preamb,3);
          FS.ReadBuffer( {$IFDEF FPC} preamb[1] {$ELSE} Pointer(preamb)^ {$ENDIF}, 3);
          if preamb=#$EF#$BB#$BF then
          begin
            SetLength(AString, L);
            FS.ReadBuffer( {$IFDEF FPC} AString[1] {$ELSE} Pointer(AString)^ {$ENDIF}, L);
            Result := true;
          end;
        end;
      finally
        FS.free
      end;
    except
      Result := false;
    end;
  end;

var
  u:UTF8String;
  WS:TStringList;
{$ENDIF}
begin
  {$IFDEF D2009UP}
  Result:=TMemIniFile.Create(AFileName);
  {$ELSE}
  u:='';
  if LoadUTF8File(u,AFileName) then
  begin
    Result:=TMemIniFile.Create('');
    WS:=TStringList.Create;
    try
      {$IFDEF FPC}
      WS.Text:=u;
      {$ELSE}
      WS.Text:=UTF8Decode(u);
      {$ENDIF}
      Result.SetStrings(WS);
      Result.Rename(AFileName, False);
    finally
      WS.Free;
    end;
  end
  else
    Result:=TMemIniFile.Create(AFileName);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  TLanguageManager
-----------------------------------------------------------------------------}

constructor TLanguageManager.Create;
begin
	inherited;
  fLangProps:=TStringList.Create;
  fLangConsts:=TStringList.Create;
  fEMessList:=TStringList.Create;
  fEMessConv:=TStringList.Create;
  fLangResStr:=TStringList.Create;
  fFileNames:=TStringList.Create;
  fActiveFiles:=TStringList.Create;
  fFolder:=ExtractFilePath(ParamStr(0))+LANGDIR_RELPATH;
  fOnLanguageChanged:=nil;
  fAllowSpecialProps:=True;
  fRecursiveFolder:=False;
  fLanguageCode:='';
  fLanguageName:='';
  fBiDiMode:=bdLeftToRight;
  fLastError:='';
end;

destructor TLanguageManager.Destroy;
begin
  Clear(True, True, True, True);
  fActiveFiles.Free;
  fFileNames.Free;
  fLangResStr.Free;
  fEMessList.Free;
  fEMessConv.Free;
  fLangProps.Free;
  fLangConsts.Free;
  inherited;
end;

procedure TLanguageManager.Clear(Consts:Boolean = True; Props:Boolean = True; Messages:Boolean = False; ActFiles: Boolean = False);
var
  i:Integer;
  n:^String;
begin
  if Consts then
  begin
    for i:=0 to fLangConsts.Count-1 do
    begin
      n:=Pointer(fLangConsts.Objects[i]);
      Dispose(n);
    end;
    fLangConsts.Clear;
  end;

  if Props then
  begin
    for i:=0 to fLangProps.Count-1 do
    begin
      n:=Pointer(fLangProps.Objects[i]);
      Dispose(n);
    end;
    fLangProps.Clear;
  end;

  if Messages then
  begin
    fEMessList.Clear;
    fEMessConv.Clear;
  end;

  for i:=0 to fLangResStr.Count-1 do
  begin
    n:=Pointer(fLangResStr.Objects[i]);
    Dispose(n);
  end;
  fLangResStr.Clear;

  if ActFiles then
    fActiveFiles.Clear;
end;

procedure TLanguageManager.FilterFileNames(const FileList:TStrings);
var
  i,j:Integer;
  f:Boolean;
begin
  if fFileNames.Count>0 then
    for i := FileList.Count - 1 downto 0 do
    begin
      f:=False;
      for j := 0 to fFileNames.Count - 1 do
        if MatchesMask(ExtractFileName(FileList[i]),fFileNames[j]+'.*.*') then
        begin
          f:=True;
          Break;
        end;
      if not f then
        FileList.Delete(i);
    end;
end;

function TLanguageManager.GetAvailableLanguages(const AvailableLanguages:TStringList):Boolean;
var
  i:Integer;
  PF,UF:TStringList;
  Ini:TMemIniFile;
  lc,lng:string;
  {$IFDEF PLS_LNGZ}
  Zip:TZipFile;
  {$ENDIF}
begin
  Result:=False;
  fLastError:='';
  try
    PF:=TStringList.Create;
    UF:=TStringList.Create;
    try
      AvailableLanguages.Clear;
      // get all langauge filenames in language folder
      if (fFolder<>'') and DirectoryExists(fFolder) then
        _GetLangFiles(fFolder,PF,UF,'',fRecursiveFolder)
      else
      begin
        _GetLangFiles(ExtractFilePath(ParamStr(0))+LANGDIR_RELPATH,PF,UF,'',fRecursiveFolder);
        if PF.Count+UF.Count=0 then
          _GetLangFiles(ExtractFilePath(ParamStr(0)),PF,UF,'',fRecursiveFolder);
      end;
      // filter by user defined filename identifiers
      PF.AddStrings(UF); // here it is not important to differentiate
      FilterFileNames(PF);
      for i := 0 to PF.Count - 1 do
      begin
        // extract language code from filename
        lc:=AnsiLowerCase(ExtractFileExt(ChangeFileExt(PF[i],'')));
        Delete(lc,1,1);
        if AvailableLanguages.IndexOfName(lc)=-1 then
        begin
          lng:='';
          try
            if lowercase(ExtractFileExt(PF[i]))='.'+LANGFILEEXTZ then
            begin
              {$IFDEF PLS_LNGZ}
              Zip:=TZipFile.Create;
              try
                Zip.LoadFromFile(PF[i]);
                {$IFDEF FPC}
                  lng:=Zip.ZipFileComment;
                {$ELSE}
                  lng:=UTF8Decode(Zip.ZipFileComment);
                {$ENDIF}
              finally
                Zip.Free;
              end;
              {$ENDIF}
            end
            else
            begin
              Ini:=_CreateLngIni(PF[i]);
              try
                lng:=Ini.ReadString(lini_sec_main,lini_key_name,'');
              finally
                Ini.Free;
              end;
            end;
          except
          end;
          if lng<>'' then
            AvailableLanguages.Add(lc+'='+lng);
        end;
      end;
      Result:=True;
    finally
      UF.Free;
      PF.Free;
    end;
  except
    on E:Exception do
      fLastError:=e.Message;
  end;
end;

procedure TLanguageManager.GetLanguageFiles(LngCode:string; const PdkFiles, UsrFiles:TStrings);
begin
  if (fFolder<>'') and DirectoryExists(fFolder) then
    _GetLangFiles(fFolder,PdkFiles,UsrFiles,LngCode,fRecursiveFolder)
  else
  begin
    _GetLangFiles(ExtractFilePath(ParamStr(0))+LANGDIR_RELPATH,PdkFiles,UsrFiles,LngCode,fRecursiveFolder);
    if PdkFiles.Count+UsrFiles.Count=0 then
      _GetLangFiles(ExtractFilePath(ParamStr(0)),PdkFiles,UsrFiles,LngCode,fRecursiveFolder);
  end;
  FilterFileNames(PdkFiles); FilterFileNames(UsrFiles); // filter by user defined filename identifiers
end;

function TLanguageManager.ReadLanguageData(Ini:TMemIniFile; IsUser:Boolean; var LangName:string):Boolean;
var
  sn,fn:String;
	n:^String;
  i,j,k:Integer;
  SL,SLN:TStringList;
begin
  Result:=False;
  if Assigned(Ini) then
  begin
    SL:=TStringList.Create;
    SLN:=TStringList.Create;
    try
      if LangName='' then
        LangName:=Ini.ReadString(lini_sec_main,lini_key_name,'');
      if (fBiDiMode<>bdRightToLeft) and Ini.ReadBool(lini_sec_main,lini_key_rtl,False) then
        fBiDiMode:=bdRightToLeft;

      // load constants
      Ini.ReadSectionValues(lini_sec_consts,SL);
      for i:=0 to SL.Count-1 do
      begin
        sn:=AnsiUpperCase(SL.Names[i]);
        if Length(SN)>0 then
        begin
          if SN[1]='@' then // it is a resourcestring constant
          begin
            if isUser and fLangResStr.Find(sn,k) then
            begin
              n:=Pointer(fLangResStr.Objects[k]);
              n^:=SL.ValueFromIndex[i];
              _EscapeText(n^);
            end
            else
            begin
              new(n);
              n^:=SL.ValueFromIndex[i];
              _EscapeText(n^);
              fLangResStr.AddObject(sn, {$IFDEF FPC} TObject(Pointer(n)) {$ELSE} Pointer(n) {$ENDIF} );
            end;
          end
          else
          begin
            if isUser and fLangConsts.Find(sn,k) then
            begin
              n:=Pointer(fLangConsts.Objects[k]);
              n^:=SL.ValueFromIndex[i];
              _EscapeText(n^);
            end
            else
            begin
              new(n);
              n^:=SL.ValueFromIndex[i];
              _EscapeText(n^);
              fLangConsts.AddObject(sn, {$IFDEF FPC} TObject(Pointer(n)) {$ELSE} Pointer(n) {$ENDIF});
            end;
          end;
        end;
      end;

      // load properties
      Ini.ReadSections(SLN);
      SLN.Sorted:=True;
      SLN.CaseSensitive:=False;
      i:=SLN.IndexOf(lini_sec_main); if i>=0 then SLN.Delete(i);
      i:=SLN.IndexOf(lini_sec_consts); if i>=0 then SLN.Delete(i);
      i:=SLN.IndexOf(lini_sec_messages); if i>=0 then SLN.Delete(i);
      for j := 0 to SLN.Count - 1 do
      begin
        SL.Clear;
        Ini.ReadSectionValues(SLN[j],SL);
        fn:=AnsiUpperCase(SLN[j])+'.';
        for i:=0 to SL.Count-1 do
        begin
          sn:=AnsiUpperCase(SL.Names[i]);
          if isUser and fLangProps.Find(fn+sn,k) then
          begin
            n:=Pointer(fLangProps.Objects[k]);
            n^:=SL.ValueFromIndex[i];
            _EscapeText(n^);
          end
          else
          begin
            new(n);
            n^:=SL.ValueFromIndex[i];
            _EscapeText(n^);
            fLangProps.AddObject(fn+sn, {$IFDEF FPC} TObject(Pointer(n)) {$ELSE} Pointer(n) {$ENDIF} );
          end;
        end;
      end;

      // load error messages
      SL.Clear;
      Ini.ReadSectionValues(lini_sec_messages,SL);
      i:=0;
      while i<SL.Count do
      begin
        fn:=trim(SL[i]);
        if (Length(fn)>0) and (Copy(fn,1,1)<>';') then
        begin
          Inc(i);
          if i<SL.Count then
          begin
            _EscapeText(fn);
            sn:=SL[i];
            _EscapeText(sn);
            if isUser then
              j:=fEMessList.IndexOf(fn)
            else
              j:=-1;
            if j>=0 then
              fEMessConv[j]:=sn
            else
            begin
              fEMessList.Add(fn);
              fEMessConv.Add(sn);
            end;
          end;
        end;
        Inc(i);
      end;
      Result:=True;
    finally
      SLN.Free;
      SL.Free;
    end;
  end;
end;

function TLanguageManager.LoadLanguageFile(const FileName:string;var LangName:string;var ErrMsg:string):Boolean;
var
  Ini:TMemIniFile;
  {$IFDEF PLS_LNGZ}
  Zip:TZipFile;
  i:Integer;
  MS:TMemoryStream;
  ZS:TStringList;
  d:AnsiString;
  {$ENDIF}
begin
  Result:=False;
  ErrMsg:='';
  try
    if lowercase(ExtractFileExt(FileName))='.'+LANGFILEEXTZ then
    begin
      {$IFDEF PLS_LNGZ}
      Zip:=TZipFile.Create;
      MS:=TMemoryStream.Create;
      ZS:=TStringList.Create;
      try
        Zip.LoadFromFile(FileName);
        Result:=True;
        for i:=0 To Zip.Count-1 do
        begin
          Ini:=TMemIniFile.Create('');
          d:=Zip.Data[i];
          MS.Clear;
          ZS.Clear;
          try
            {$IFDEF FPC}
            ZS.Text:=d;
            {$ELSE}
            {$IF CompilerVersion>=20}
            MS.Write(d[1],Length(d));
            MS.Position:=0;
            ZS.LoadFromStream(MS);
            {$ELSE}
            if Copy(d,1,3)=#$EF#$BB#$BF then
              Delete(d,1,3);
            ZS.Text:=UTF8Decode(d);
            {$IFEND}
            {$ENDIF}
            Ini.SetStrings(ZS);
            if not ReadLanguageData(Ini,AnsiLowerCase(ExtractFileExt(Zip.Name[i]))='.'+LANGFILEEXTU,LangName) then
              Result:=False;
          finally
            Ini.Free;
          end;
        end;
      finally
        ZS.Free;
        MS.Free;
        Zip.Free;
      end;
      {$ENDIF}
    end
    else
    begin
      Ini:=_CreateLngIni(FileName);
      try
        Result:=ReadLanguageData(Ini,AnsiLowerCase(ExtractFileExt(FileName))='.'+LANGFILEEXTU,LangName);
      finally
        Ini.Free;
      end;
    end;
    fActiveFiles.Add(FileName);
  except
    on E:Exception do
      ErrMsg:=e.Message;
  end;
end;

procedure TLanguageManager.SetLanguageCode(const Value:string);
var
  i:Integer;
  UF,PF:TStringList;
  err:string;
begin
  fLastError:='';
  if Value<>'' then
  begin
    try
      Clear(True, True, True, True);
      fLanguageCode:=Value;
      fLanguageName:='';
      fBiDiMode:=bdLeftToRight;
      fLangConsts.Sorted:=False;
      fLangProps.Sorted:=False;
      fLangResStr.Sorted:=False;
      PF:=TStringList.Create;
      UF:=TStringList.Create;
      try
        // get all product (PF) and user (UF) langauge filenames in language folder
        if (fFolder<>'') and DirectoryExists(fFolder) then
          _GetLangFiles(fFolder,PF,UF,Value,fRecursiveFolder)
        else
        begin
          _GetLangFiles(ExtractFilePath(ParamStr(0))+LANGDIR_RELPATH,PF,UF,Value,fRecursiveFolder);
          if PF.Count+UF.Count=0 then
            _GetLangFiles(ExtractFilePath(ParamStr(0)),PF,UF,Value,fRecursiveFolder);
        end;
        FilterFileNames(PF); FilterFileNames(UF); // filter by user defined filename identifiers

        if PF.count+UF.Count=0 then
        begin
          fLanguageCode:='';
          raise Exception.Create(resELangFilesNotFound + ' ('+Value+')');
        end;

        if PF.Count>0 then
        begin
          for i := 0 to PF.Count - 1 do
          begin
            {$IFDEF FPC} err:=''; {$ENDIF}
            if not LoadLanguageFile(PF[i],fLanguageName,err) then
              fLastError:=fLastError+CRLF+err
          end;
        end;
        if UF.Count>0 then
        begin
          fLangConsts.Sorted:=True;
          fLangProps.Sorted:=True;
          fLangResStr.Sorted:=True;
          for i := 0 to UF.Count - 1 do
          begin
            if not LoadLanguageFile(UF[i],fLanguageName,err) then
              fLastError:=fLastError+CRLF+err
          end;
        end;
        Delete(fLastError,1,2);
      finally
        if UF.Count=0 then
        begin
          fLangConsts.Sorted:=True;
          fLangProps.Sorted:=True;
          fLangResStr.Sorted:=True;
        end;
        UF.Free;
        PF.Free;
      end;
    except
      on E:Exception do
        fLastError:=fLastError+e.Message;
    end;
    if Assigned(fOnLanguageChanged) and (fLastError='') and (fLanguageCode=Value) then
      fOnLanguageChanged(Self);
  end;
end;

procedure TLanguageManager.Refresh;
begin
  LanguageCode:=fLanguageCode;
end;

function TLanguageManager.LangText(const ID:string;const DefaultText:string=''):string;
var
	i:Integer;
begin
  if Length(ID)=0 then
  begin
    Result:=DefaultText;
    _EscapeText(Result);
  end
  else
  begin
    if ID[1]='@' then // it is a resourcestring constant
    begin
      if fLangResStr.Find(UpperCase(ID),i) then
        Result:=PChar(Pointer(fLangResStr.Objects[i])^)
      else
      begin
        Result:=DefaultText;
        _EscapeText(Result);
      end
    end
    else
    if fLangConsts.Find(UpperCase(ID),i) then // text constant
      Result:=PChar(Pointer(fLangConsts.Objects[i])^)
    else
    begin
      if fLangProps.Find(UpperCase(ID),i) then  // property
        Result:=PChar(Pointer(fLangProps.Objects[i])^)
      else
      begin
        Result:=DefaultText;
        _EscapeText(Result);
      end;
    end;
  end;
end;

function TLanguageManager.LangVCL(const Owner:TComponent;const AsClassName:String='';const OnlyComponent:TComponent=nil):Boolean;
var
	i,j,p1,p2,c2,c3,c4:Integer;
	fm,pn,cln,cls:String;
	C:TComponent;
	CL:TCollectionItem;
  Coll:TCollection;
	GO:TObject;
	pvV:Variant;
  pvI:Integer;
	pv:String;
begin
  fLastError:='';
	try
		if AsClassName<>'' then
			fm:=AnsiUpperCase(AsClassName)+'.' // form/frame classname
		else
			fm:=AnsiUpperCase(Owner.ClassName)+'.';
		p1:=Length(fm);
		for i:=0 to fLangProps.Count-1 do
			if Copy(fLangProps[i],1,p1)=fm then
			begin // form found
				for j:=i to fLangProps.Count-1 do
				begin
					if Copy(fLangProps[j],1,p1)<>fm then
            break; // break on next form
          try
            p2:=LastDelimiter('.',fLangProps[j]);
            cln:=Copy(fLangProps[j],p1+1,p2-p1-1);
            c2:=Pos('.',cln);
            if c2>0 then
            begin // child object of component (i.e. Collection...)
              cls:=Copy(cln,1,c2-1);
              if CompareText(cls,LC_Self)=0 then
                c:=Owner
              else
                c:=Owner.findcomponent(cls); // component
              Delete(cln,1,c2); // now cln = child object name
              c2:=Pos('.',cln);
              if (c2>0) and (c<>nil) then
              begin
                GO:=nil;
                repeat
                  cls:=Copy(cln,1,c2-1);
                  c3:=Pos('[',cls);
                  if c3>0 then
                  begin // item of object specified (i.e. collectionitem)
                    c4:=strtointdef(Copy(cls,c3+1,Pos(']',cls)-c3-1),-1);
                    if c4>-1 then
                    begin
                      {$IFDEF FPC}
                      Coll:=TCollection({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(c,Copy(cls,1,c3-1))));
                      {$ELSE}
                      Coll:=TCollection(GetOrdProp(c,Copy(cls,1,c3-1)));
                      {$ENDIF}
                      CL:=Coll.FindItemID(c4);
                      if (CL=nil) and (c4<Coll.Count) then
                        CL:=Coll.Items[c4];
                      GO:=TObject(CL);
                      {$IFDEF FPC} {$WARNINGS OFF} {$ENDIF}
                      c:=TComponent(CL);
                      {$IFDEF FPC} {$WARNINGS ON}  {$ENDIF}
                    end;
                  end
                  else
                  begin
                    {$IFDEF FPC}
                    GO:=TObject({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(c,cls)));
                    {$ELSE}
                    GO:=TObject(GetOrdProp(c,cls));
                    {$ENDIF}
                    c:=TComponent(GO);
                  end;
                  Delete(cln,1,c2);
                  c2:=Pos('.',cln);
                until c2=0;
                c:=TComponent(GO);
              end;
              if (OnlyComponent<>nil) and (c<>OnlyComponent) then
                continue;
							if (c<>nil) then
              begin
                c2:=Pos('[',cln);
                if c2>0 then
                begin // item of object specified (i.e. collectionitem)
                  c3:=strtointdef(Copy(cln,c2+1,Pos(']',cln)-c2-1),-1);
                  if c3>-1 then
                  begin
                    {$IFDEF FPC}
                    Coll:=TCollection({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(c,Copy(cln,1,c2-1))));
                    {$ELSE}
                    Coll:=TCollection(GetOrdProp(c,Copy(cln,1,c2-1)));
                    {$ENDIF}
                    CL:=Coll.FindItemID(c3);
                    if (CL=nil) and (c3<Coll.Count) then
                      CL:=Coll.Items[c3];
                    //if Assigned(CL) then
                    begin
                      pn:=Copy(fLangProps[j],p2+1,maxint); // property name
                      pv:=PChar(pointer(fLangProps.Objects[j])^); // property value
                      SetPropValue(CL,pn,pv);
                    end;
                  end;
                end
                else
                begin // whole subobject
                  {$IFDEF FPC}
                  GO:=TObject({$IFDEF Win32}LongInt{$ELSE}Int64{$ENDIF}(GetOrdProp(c,cln)));
                  {$ELSE}
                  GO:=TObject(GetOrdProp(c,cln));
                  {$ENDIF}
                  pn:=Copy(fLangProps[j],p2+1,maxint); // property name
                  // solve not published properties, that are commonly used
                  if GO is TStrings then
                  begin
                    if pn='TEXT' then
                      TStrings(GO).Text:=PChar(Pointer(fLangProps.Objects[j])^)
                    else
                    if pn='COMMATEXT' then
                      TStrings(GO).CommaText:=PChar(Pointer(fLangProps.Objects[j])^)
                    else
                    if pn='DELIMITEDTEXT' then
                      TStrings(GO).DelimitedText:=PChar(Pointer(fLangProps.Objects[j])^)
                  end
                  else
                  begin // for other objects
                    pvV:=String(PChar(pointer(fLangProps.Objects[j])^)); // property value
                    SetPropValue(GO,pn,pvV);
                  end;
                end;
              end;
            end
            else
            begin // standard component
              if CompareText(cln,LC_Self)=0 then
                c:=Owner
              else
                c:=Owner.findcomponent(cln); // component
              if (OnlyComponent<>nil) and (c<>OnlyComponent) then
                continue;
              if c<>nil then
              begin
                pn:=Copy(fLangProps[j],p2+1,maxint); // property name
                pv:=PChar(pointer(fLangProps.Objects[j])^); // property value
                // some special handled properties
                if FAllowSpecialProps then
                begin
                  if pn='SHORTCUT' then
                  begin
                    pvI:=TextToShortCut(pv);
                    SetOrdProp(c,pn,pvI);
                  end
                  else
                    SetPropValue(c,pn,pv);
                end
                else
                  SetPropValue(c,pn,pv);
							end
            end;
					except
            on E:Exception do
              fLastError:=fLastError+CRLF+CRLF+Self.ClassName+' ('+fLangProps[j]+'):'+CRLF+'      '+e.Message;
          end;
        end;
				break;
			end;
  except
    on E2:Exception do
      fLastError:=fLastError+CRLF+CRLF+Self.ClassName+':'+CRLF+'      '+e2.Message;
  end;
  Result:=fLastError='';
//  Delete(fLastError,1,2);
end;

function TLanguageManager.LangForm(const Owner:TComponent; const AsClassName:String=''; const OnlyComponent:TComponent=nil):Boolean;
begin
  Result := LangVCL(Owner, AsClassName, OnlyComponent);
end;

function _StrLastPos(const SubStr, S: string): Integer;
var
  Last, Current: PChar;
begin
  Result := 0;
  Last := nil;
  Current := PChar(S);

  while (Current <> nil) and (Current^ <> #0) do
  begin
    Current := StrPos(PChar(Current), PChar(SubStr));
    if Current <> nil then
    begin
      Last := Current;
      Inc(Current);
    end;
  end;
  if Last <> nil then
    Result := Abs(PChar(S) - Last) + 1;
end;

// Returns localized message converted via mask or returns itself
function TLanguageManager.LangString(const AString:string):string;
var
	i:Integer;
  pl,pc,pu:Integer;
  eml,emc,emu:string;
  f:Boolean;
begin
  if Length(AString)=0 then
  begin
    Result:='';
    Exit;
  end;

  f:=False;
  // identify string by mask
  for i:=0 To fEMessList.Count-1 do
  begin
    eml:=fEMessList[i];
    if MatchesMask(AString,eml) then
    begin
      // convert message via translated mask
      emc:=fEMessConv[i];
      emu:=AString;
      Result:='';
      pc:=LastDelimiter('*?',emc);
      while pc>0 do
      begin
        Result:=Copy(emc,pc+1,maxint)+Result;
        Delete(emc,pc,maxint);
        pl:=LastDelimiter('*?',eml);
        Delete(emu,Length(emu)-Length(Copy(eml,pl+1,maxint))+1,maxint);
        Delete(eml,pl,maxint);

        pc:=LastDelimiter('*?',emc);
        if pc=0 then
        begin
          Delete(emu,1,Length(eml));
          Result:=emu+Result;
        end
        else
        begin
          pl:=LastDelimiter('*?',eml);
          pu:=_StrLastPos(Copy(eml,pl+1,maxint),emu);
          Result:=Copy(emu,pu+Length(eml)-pl,maxint)+Result;
          Delete(emu,pu+Length(eml)-pl,MaxInt);
        end;
      end;
      if Length(emc)>0 then
        Result:=emc+Result;
      f:=True;
      Break;
    end;
  end;
  if not f then
  begin
    // try to find constant or property
    if AString[1]='@' then // it could be a resourcestring constant
    begin
      if fLangResStr.Find(UpperCase(AString),i) then
      begin
        Result:=PChar(Pointer(fLangResStr.Objects[i])^);
        f:=True;
      end
    end
    else
    if fLangConsts.Find(UpperCase(AString),i) then // text constant
    begin
      Result:=PChar(Pointer(fLangConsts.Objects[i])^);
      f:=True;
    end;
    if not f then
    begin
      if fLangProps.Find(UpperCase(AString),i) then  // property
        Result:=PChar(Pointer(fLangProps.Objects[i])^)
      else
        Result:=AString;
    end;
  end;
end;

function TLanguageManager.LangMess(const AMessage:string):string;
begin
  Result:=LangString(AMessage);
end;

{$IF Defined(MSWINDOWS) OR Defined(FPC)}
// LangResourceStr - author of the algorithm for this method is TeamB (www.teamb.com)
procedure TLanguageManager.LangResourceStr(rs: PResStringRec; newStr: PChar);
{$IFNDEF FPC}
var
  oldprotect: DWORD;
{$ENDIF}
begin
  If rs = nil Then Exit;
  Assert( Assigned(newStr), 'newStr must not be nil');
  {$IFDEF FPC}
  rs^ := newStr;
  {$ELSE}
  VirtualProtect(rs, SizeOf(rs^), PAGE_EXECUTE_READWRITE, @oldProtect);
  rs^.Identifier := Integer(newStr);
  VirtualProtect(rs, SizeOf(rs^), oldProtect, @oldProtect);
  {$ENDIF}
end;
{$IFEND}

function _(const AString:string):string;
begin
  Result := LanguageManager.LangString(AString);
end;

initialization

  if not IsLibrary then
		LanguageManager:=TLanguageManager.Create;

finalization

  if (not IsLibrary) and Assigned(LanguageManager) then
    LanguageManager.Free;

end.
