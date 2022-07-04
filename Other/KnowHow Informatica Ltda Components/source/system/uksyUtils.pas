{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit uksyUtils;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, Messages, WinSock, SysUtils, ShellAPI, Classes, Controls, Graphics,
  Forms, Dialogs, ExtCtrls, TypInfo, uksyConsts, uksyTypes;

{
--------------------------------------------------------------------------------
------------------------------ Generic Globals ---------------------------------
--------------------------------------------------------------------------------
}

const
	faAllMasks        = SysUtils.faReadOnly or SysUtils.faHidden or
											SysUtils.faSysFile or SysUtils.faVolumeID;
	faAllDirectory    = SysUtils.faDirectory or faAllMasks;
	faAllArchive      = SysUtils.faArchive or faAllMasks;

	faAllDirectorySet = [SysUtils.faDirectory..faAllDirectory];
	faAllArchiveSet   = [SysUtils.faArchive..faAllArchive];

var
	ListSeparator: Char = CH_LIST_TOKEN;

{
--------------------------------------------------------------------------------
----------------------- Generic Exception Architecture -------------------------
--------------------------------------------------------------------------------
}

type

	{ EKnowHow }

	EKnowHow = class( Exception )
	private
		FLastError: DWORD;
		FRaiseAddress : Pointer;

	public
		constructor Create( const Message: string ); virtual;
		constructor CreateFmt( const Message: string; const Args: array of const ); virtual;

		constructor CreateAt( const Message: string; Address: Pointer ); virtual;
		constructor CreateAtFmt( const Message: string; Address: Pointer;
			const Args: array of const ); virtual;

    { The Dummy parameter is needed because the Delphi4 compatibility with C++ Builder.
      If you have a class with the same constructor parameters but with different
      functionality (ex: Create and CreateWin32). }
		constructor CreateWin32( const Message: string {$IFDEF DELPHI4}; Dummy: Integer = 0 {$ENDIF} ); virtual;
		constructor CreateWin32Fmt( const Message: string; const Args: array of const
			{$IFDEF DELPHI4}; Dummy: Integer = 0 {$ENDIF} ); virtual;

		property LastError: DWORD
						 read FLastError;
		property RaiseAddress: Pointer
						 read FRaiseAddress;

	end;

	EKnowHowClass = class of EKnowHow;

	TKExceptionDigestProc = procedure( AException: EKnowHow; var ExceptionMessage: string );

	EKLIB = class( EKnowHow )
	protected
		function GetReleaseDate: TDateTime; virtual;
		function GetVersion: string; virtual;

	public
		property ReleaseDate: TDateTime
						 read GetReleaseDate;
		property Version: string
						 read GetVersion;

	end;

{ Run-Time Package Main Exceptions }

	EKSystem = class( EKLIB );
	EKKernel = class( EKLIB );
	EKStd = class( EKLIB );
	EKExt = class( EKLIB );
	EKWinAPI = class( EKLIB );
	EKDB = class( EKLIB );
	EKDBCtrls = class( EKLIB );
	EKCOM = class( EKLIB );
	EKExperts = class( EKLIB );
	EKBarCode = class( EKLIB );
	EKCrypto = class( EKLIB );
	EKDialogs = class( EKLIB );
	EKDBDialogs = class( EKLIB );
	EKForms = class( EKLIB );
	EKCPL = class( EKLIB );

	EKSYUtils = class( EKSystem );

{ Application Specific Exceptions }

  EKApplication = class( EKnowHow );	

var
	ExceptionDigestProc: TKExceptionDigestProc = nil;

type
  
	TKLIBVersion = record
		Version: string;
		Release: TDateTime;
		Reserved: Integer;
	end;

	function KLIBVersion: TKLIBVersion;
	function KLIBBaseRegKey: string;

{
--------------------------------------------------------------------------------
---------------------- Generic Exception Handling Routines ---------------------
--------------------------------------------------------------------------------
}

procedure ExceptionDigest( AException: EKnowHow; var ExceptionMessage: string );

procedure RaiseException( EClass: EKnowHowClass; const Message: string );
procedure RaiseExceptionFmt( EClass: EKnowHowClass; const Message: string;
	const Args: array of const );
procedure RaiseExceptionAt( EClass: EKnowHowClass; const Message: string;
	Address: Pointer );
procedure RaiseExceptionAtFmt( EClass: EKnowHowClass; const Message: string;
	Address: Pointer; const Args: array of const );
procedure RaiseWin32Exception( EClass: EKnowHowClass; const Message: string );
procedure RaiseWin32ExceptionFmt( EClass: EKnowHowClass; const Message: string;
	const Args: array of const );

procedure FatalError( const Message: string );
procedure FatalErrorFmt( const Message: string; const Args: array of const );

{ Procedure/Pointers Handlers }

{ You can set this special AbstractError Handler for AbstractErrorProc abstract
  methods error handler in system.pas }
procedure AbstractError;

(*
  ExceptProc: Pointer;    { Unhandled exception handler }
  ErrorProc: Pointer;     { Error handler procedure }
  SafeCallErrorProc: Pointer; { Safecall error handler }
  AssertErrorProc: Pointer; { Assertion error handler }
  AbstractErrorProc: Pointer; { Abstract method error handler }
*)

{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

type

	TKPercent = 0..100;
	TKProgressInfoStyle = ( pisNormal, pisPath, pisEllipsis );

	TKDialogStyle = ( dsOK, dsCancel, dsOKCancel, dsYesNo, dsYesNoCancel,
		dsYesAllNoCancel, dsYesNoAllCancel, dsAbortRetryIgnore );

	TKBitmapOption = ( boInformation, boSoftBug, boHardBug,	boExclamation01,
		boExclamation02, boExclamation03, boQuestion01,	boQuestion02, boCustom );

	TKDialogShowProc = procedure( Sender: TCustomForm; Data: Pointer );

	TKDialogExecuteProc = procedure( Data: Pointer );

	TKProgressDialogCallBack = procedure( Data: Pointer;
		var Status: string; var Percent: TKPercent; var Cancel: Boolean;
		var ProgressInfoStyle: TKProgressInfoStyle );

	TKShowDialogFunc =
		function( const Title, Message: string;	Bitmap: TBitmap;
			Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
	TKShowDialogProc =
		procedure( const Title, Message: string;	Bitmap: TBitmap; Style: TKDialogStyle;
			BitmapOption: TKBitmapOption );
	TKExpireShowDialogFunc =
		function( const Title, Message: string;	SecTimeOut: Integer; Bitmap: TBitmap;
			Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
	TKExpireShowDialogProc =
		procedure( const Title, Message: string;	SecTimeOut: Integer; Bitmap: TBitmap;
			Style: TKDialogStyle;	BitmapOption: TKBitmapOption );

	TKInputDialogFunc =
		function( const Title, Prompt, EditMask: string;
			var Text: string ): Boolean;
	TKPasswordDialogFunc =
		function( const Title, Prompt, EditMask: string; PasswordChar: Char;
			var Text: string ): Boolean;

	TKInputListDialogFunc =
		function( const Title, Prompt: string; DefaultIndex: Integer; List: TStrings ): Integer;
	TKInputListDialogExFunc =
		function( const Title, Prompt: string; DefaultIndex: Integer; AcceptUserInput: Boolean;
			List: TStrings ): string;
	TKInputCheckListDialogFunc =
		function( const Caption: string; BackColor: TColor; Sorted: Boolean;
			Source: TStrings; ResultList: TBits ): Boolean;

	TKProgressDialogFunc =
		function( const Title, Message: string; StatusAlignment: TAlignment; AllowCancel,
			ProgressVisible: Boolean; Callback: TKProgressDialogCallBack;
			ExecutorProc: TKDialogExecuteProc; Data: Pointer ): Boolean;

	TKFontData = record
		Name: string;
		Size: Integer;
		Style: TFontStyles;
		Color: TColor;
	end;

	TKDialogData = record
		Top: Integer;
		Left: Integer;
		Centered: Boolean;
		RightAlign: Boolean;
		ToolWindow: Boolean;
		Font: TKFontData;
		swCallBack: TKDialogShowProc;
		UserData: Pointer;
	end;

procedure ResetDialogData;

procedure ClearSystemDialogProcs;

procedure AssignFontFromFontData( Font: TFont; FontData: TKFontData );
procedure AssignFontToFontData( Font: TFont; var FontData: TKFontData );

function Confirm( const Prompt: string ): Boolean;
function ConfirmFmt( const Prompt: string; const Args: array of const ): Boolean;

procedure Inform( const Prompt: string );
procedure InformFmt( const Prompt: string; const Args: array of const );

procedure Warn( const Prompt: string );
procedure WarnFmt( const Prompt: string; const Args: array of const );

procedure ShowError( const Prompt: string );
procedure ShowErrorFmt( const Prompt: string; const Args: array of const );

procedure InformOnTop( const Prompt: string );
procedure InformOnTopFmt( const Prompt: string; const Args: array of const );

procedure WarnOnTop( const Prompt: string );
procedure WarnOnTopFmt( const Prompt: string; const Args: array of const );

procedure ShowErrorOnTop( const Prompt: string );
procedure ShowErrorOnTopFmt( const Prompt: string; const Args: array of const );

function ExpireConfirm( SecTimeOut: Cardinal; const Prompt: string ): Boolean;
function ExpireConfirmFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const ): Boolean;

procedure ExpireInform( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireInformFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );

procedure ExpireWarn( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireWarnFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );

procedure ExpireShowError( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireShowErrorFmt( SecTimeOut: Cardinal; const Prompt: string;
  const Args: array of const );

procedure ExpireInformOnTop( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireInformOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );

procedure ExpireWarnOnTop( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireWarnOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );

procedure ExpireShowErrorOnTop( SecTimeOut: Cardinal; const Prompt: string );
procedure ExpireShowErrorOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );

function ShowDialog( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
function ShowDialogFmt( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const ): TModalResult;

procedure ShowDialogOnTop( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption );
procedure ShowDialogOnTopFmt( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption; const Args: array of const );

function ExpireShowDialog( const Title, Message: string; SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
function ExpireShowDialogFmt( const Title, Message: string;	SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const ): TModalResult;

procedure ExpireShowDialogOnTop( const Title, Message: string; SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption );
procedure ExpireShowDialogOnTopFmt( const Title, Message: string;	SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const );

function InputDialog( const Title, Prompt, EditMask: string;
	var Text: string ): Boolean;
function PasswordDialog( const Title, Prompt, EditMask: string; PasswordChar: Char; 
	var Text: string ): Boolean;

function InputListDialog( const Title, Prompt: string; DefaultIndex: Integer;
	List: TStrings ): Integer;
function InputListDialogEx( const Title, Prompt: string; DefaultIndex: Integer;
	AcceptUserInput: Boolean; List: TStrings ): string;

function InputDialogBox( const Title, Prompt, EditMask, Default: string ): string;

function InputCheckListDialog( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits ): Boolean;
function InputCheckListDialogFmt( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits; const Args: array of const ): Boolean;

function ProgressDialog( const Title, Message: string; StatusAlignment: TAlignment;
	AllowCancel, ProgressVisible: Boolean; Callback: TKProgressDialogCallBack;
	ExecutorProc: TKDialogExecuteProc; Data: Pointer ): Boolean;

var
	ShowDialogProc: TKShowDialogFunc = nil;
	ShowDialogOnTopProc: TKShowDialogProc = nil;
	ExpireShowDialogProc: TKExpireShowDialogFunc = nil;
	ExpireShowDialogOnTopProc: TKExpireShowDialogProc = nil;
	InputDialogProc: TKInputDialogFunc = nil;
  PasswordDialogProc: TKPasswordDialogFunc = nil;
	InputListDialogProc: TKInputListDialogFunc = nil;
	InputListDialogExProc: TKInputListDialogExFunc = nil;
	InputCheckListDialogFunc: TKInputCheckListDialogFunc = nil;
	ProgressDialogProc: TKProgressDialogFunc = nil;

	SYSDLG_BMP_RESNAMES: array[TKBitmapOption] of string = { Do not resource!!! }
	(
		'Info',
		'Bug01',
		'Bug02',
		'Exclamation01',
		'Exclamation02',
		'Exclamation03',
		'Question01',
		'Question02',
		''
	);

{ const }

	DialogData: TKDialogData =
	(
		Top: INVALID_POSITION;
		Left: INVALID_POSITION;
		Centered: true;
		RightAlign: false;
		ToolWindow: false;
		Font:
		(
			Name: 'Arial';
			Size: 8;
			Style: [];
			Color: clBlack;
		);
		swCallBack: nil;
		UserData: nil;
	);

{
--------------------------------------------------------------------------------
------------------------- Generic Information Dialogs --------------------------
--------------------------------------------------------------------------------
}

type

	TCustomFormClass = class of TCustomForm;

function SelectScreenDataModule( IsSorted: Boolean ): TDataModule;
function SelectScreenForm( FilterClass: TCustomFormClass; IsSorted: Boolean ): TCustomForm;

function GetControlPath( Control: TControl ): string;
function GetComponentPath( Component: TComponent ): string;

function SelectControl( Root: TControl; FilterClass: TControlClass;
	IsRecursive, IsSorted, IncludeRoot: Boolean ): TControl;
function SelectComponent( Root: TComponent; FilterClass: TComponentClass;
	IsRecursive, IsSorted, IncludeRoot: Boolean ): TComponent;

{
--------------------------------------------------------------------------------
------------------------- Generic File Dialog Routines -------------------------
--------------------------------------------------------------------------------
}

const
	DEFAULT_SELECT_LOADSAVE_FILES = [ofHideReadOnly];

function SelectLoadFile( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
function SelectLoadFiles( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
function SelectLoadPicture( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
function SelectLoadPictures( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;

function SelectSaveFile( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
function SelectSaveFiles( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
function SelectSavePicture( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
function SelectSavePictures( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;

{
--------------------------------------------------------------------------------
---------------------------- Generic RTTI Routines -----------------------------
--------------------------------------------------------------------------------
}

type

	TKEnumPubPropsFunc = function( Source: TObject; ppi: PPropInfo; Data: Pointer ): Boolean;

	TKAssignPubPropsErrorFunc = function( Source, Target: TObject;
		SourceInfo, TargetInfo: PPropInfo; E: Exception; Data: Pointer ): Boolean;

function EnumPubProps( Source: TObject; TypeKinds: TTypeKinds; Data: Pointer;
	CallBack: TKEnumPubPropsFunc ): Integer;
function AssignPubProps( Source, Target: TObject ): Integer;
function AssignPubPropsEx( Source, Target: TObject; Data: Pointer;
	ErrorCallBack: TKAssignPubPropsErrorFunc ): Integer;

procedure SetPropertyDefaults( Source: TObject );

function EnumName( Source: Cardinal; Info: PTypeInfo ): string;
procedure EnumNames( sl: TStrings; Info: PTypeInfo );
function SetToString( const Source; Info: PTypeInfo ): string;

procedure SetPropInfo( Source: TObject; ppi: PPropInfo; const PropValue );
function SetPubProp( Source: TObject; const PropName: string; const PropValue;
	ProcessChildren: Boolean ): Boolean;
function SetPubProps( Source: TObject; PropNameList: TStrings; const PropValue;
	ProcessChildren: Boolean ): Boolean;

type

	TKDesigner = {$IFDEF DELPHI4}IDesigner{$ELSE}TDesigner{$ENDIF};

function CheckDesigner( Source: TKDesigner ): Boolean;
procedure ForceDesigner( Source: TKDesigner );

{ For Delphi4 this function will not function well (force IDesigner is
  an IFormDesigner), because there is no meta interface definition :( }	
function CheckDesignerClass( Source: TKDesigner{$IFNDEF DELPHI4};
	AClass: TClass {$ENDIF}): Boolean;

procedure MarkDesigner( Source: TComponent );

type
	TKCompPathValue = record
		Obj: TPersistent;
		PropInfo: PPropInfo;
	end;

function GetPersistentFromPath( p: TPersistent; const Path: string ): TPersistent;
function GetCompPathValue( cOwner: TComponent; const Path: string;
  TypeKinds: TTypeKinds ): TKCompPathValue;
function GetCompPathOrdValue( c: TComponent; const Path: string ): Integer;
function GetCompPathStrValue( c: TComponent; const Path: string ): string;
function GetCompPathFloatValue( c: TComponent; const Path: string ): Extended;
function GetCompPathMethodValue( c: TComponent; const Path: string ): TMethod;
function GetCompPathVariantValue( c: TComponent; const Path: string ): Variant;

{
--------------------------------------------------------------------------------
------------------------------ Generic Set Routines ----------------------------
--------------------------------------------------------------------------------
}

type

	TKSetSize = 1..4;

function SetToBitMask( const Source; Size: TKSetSize ): LongInt;
procedure BitMaskToSet( var Source; BitMask: LongInt; Size: TKSetSize );

function SetsEqual( const Set1, Set2; Size1, Size2: TKSetSize ): Boolean;
function SetsIntersect( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;

function SetContains( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;
function SetContained( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;

{
--------------------------------------------------------------------------------
---------------------------- Generic Color Routines ----------------------------
--------------------------------------------------------------------------------
}

procedure KGetColorValues( Proc: TGetStrProc );
function KColorToIdent( Color: Integer; var Ident: string ): Boolean;
function KIdentToColor( const Ident: string; var Color: Integer ): Boolean;
function KColorToString( Color: TColor ): string;
function KStringToColor( const Color: string ): TColor;

{
--------------------------------------------------------------------------------
----------------------- Delphi Registry Access Routines ------------------------
--------------------------------------------------------------------------------
}

type
	TKDelphiRegInfo = ( driLibPath, driDPLDir, driDCPDir, driInstDPL{...} );
	TKDelphi32Version = ( dv200, dv300, dv400, dv500 );

	TKRegistryKey = ( rkClassRoot, rkCurrentUser, rkLocalMachine, rkUsers,
		rkPerformanceData, rkCurrentConfig, rkDynData );
	TKRegistryItem = ( riKeys, riNames, riValues, riNamesValues );

function GetDelphiRegInfo( DelphiRegInfo: TKDelphiRegInfo ): string;
function GetDelphiDCPOutPutDir: string;
function GetDelphiDPLOutPutDir: string;
function GetDelphiLibraryPath: string;
procedure GetDelphiInstaledPackages( sl: TStrings );

procedure SetDelphiRegInfo( DelphiRegInfo: TKDelphiRegInfo; const Value: string );
procedure SetDelphiDCPOutPutDir( const Value: string );
procedure SetDelphiDPLOutPutDir( const Value: string );
procedure SetDelphiLibraryPath( const Value: string );
procedure AppendDelphiLibraryPath( const Value: string );
procedure InstallDelphiPackage( const Value, Info: string );

function DelphiRootDir( Version: TKDelphi32Version ): string;
function CurrentDelphiRootDir: string;

function DelphiBaseRegKey( Version: TKDelphi32Version ): string;
function CurrentDelphiBaseRegKey: string;

function CurrentProjectPath: string;

function CheckRegistryPath( RootKey: TKRegistryKey; const Path: string ): Boolean;
procedure ForceRegistryPath( RootKey: TKRegistryKey; const Path: string );

function ReadFromRegistry( RootKey: TKRegistryKey; CanCreate: Boolean; const Path, Key: string ): string;
function WriteToRegistry( RootKey: TKRegistryKey; CanCreate: Boolean; const Path, Key, Value: string ): Boolean;

function ReadRegistryStrings( RootKey: TKRegistryKey; CanCreate: Boolean; const RootPath: string; Keys: TStrings ): Boolean;
function WriteRegistryStrings( RootKey: TKRegistryKey; CanCreate: Boolean; const RootPath: string; Keys: TStrings ): Boolean;

function EnumRegistryItems( RootKey: TKRegistryKey; const RootPath: string;
	RegItemKind: TKRegistryItem; Items: TStrings ): Boolean;

const

	REGISTRY_KEYS: array[TKRegistryKey] of DWORD =
	(
		HKEY_CLASSES_ROOT,
		HKEY_CURRENT_USER,
		HKEY_LOCAL_MACHINE,
		HKEY_USERS,
		HKEY_PERFORMANCE_DATA,
		HKEY_CURRENT_CONFIG,
		HKEY_DYN_DATA
	);

	REGISTRY_KEYS_NAMES: array[TKRegistryKey] of string = { Do not resource }
	(
		'HKEY_CLASSES_ROOT',
		'HKEY_CURRENT_USER',
		'HKEY_LOCAL_MACHINE',
		'HKEY_USERS',
		'HKEY_PERFORMANCE_DATA',
		'HKEY_CURRENT_CONFIG',
		'HKEY_DYN_DATA'
	);

{
--------------------------------------------------------------------------------
------------------------- Generic General Use Routines -------------------------
--------------------------------------------------------------------------------
}

type

	TKWinVersion = record
		case Integer of
			0: ( All:  LongInt );
			1: ( Windows, Dos: Word );
			2: ( WinMajor, WinMinor, DosMajor, DosMinor: Byte );
	end;

	TKWindowsShutDown = ( wsdForce, wsdLogOff, wsdPowerOff, wsdReboot, wsdShutDown );

const
	WinVersion: TKWinVersion = ( All: 0 );

procedure FreeClean( var Source );        { you can only pass TObject instances }
procedure FreeCleanPointer( var Source ); { do not pass the pointer data, but its address }

function CheckWinNT: Boolean;
procedure ForceWinNT( IsRunning: Boolean );
procedure DefaultSecurityDacl( ASecurity: PSecurityAttributes );

function GetWindowsVer: TKWinVersion;
function GetOSVer: TOSVersionInfo;
function OSVersion: string;

function ShutDownWindows( wsd: TKWindowsShutDown ): Boolean;

function UserName: string;
function DomainName: string;
function ComputerName: string;
function WorkgroupName: string;

procedure NotYetImplemented;
procedure NotYetImplementedEx( const AddInfo: string );

procedure Delay( MiliSecs: DWORD );

function FindGlobalComponent( const CompName: string ): TComponent;

function Designing( AComp: TComponent ): Boolean;
function Destroying( AComp: TComponent ): Boolean;
function Loading( AComp: TComponent ): Boolean;
function Reading( AComp: TComponent ): Boolean;
function Writing( AComp: TComponent ): Boolean;

function Delphi32Running( Version: TKDelphi32Version ): Boolean;
function CurrentDelphi32Running: Boolean;

{ Path support }

function LastSubDir( const SubDir: string ): string;
function RemoveLastPathSlash( const Path: string ): string;
function RemoveFirstPathSlash( const Path: string ): string;

{ Snapshot Support - These features are available only for Win95! }

const
	MAX_MODULE_NAME32 = 255;

type

	PProcessEntry32 = ^TProcessEntry32;
	TProcessEntry32 = record
		dwSize: DWORD;
		cntUsage: DWORD;
		th32ProcessID: DWORD;       // this process
		th32DefaultHeapID: DWORD;
		th32ModuleID: DWORD;        // associated exe
		cntThreads: DWORD;
		th32ParentProcessID: DWORD; // this process's parent process
		pcPriClassBase: Longint;	// Base priority of process's threads
		dwFlags: DWORD;
		szExeFile: array[0..MAX_PATH - 1] of Char;// Path
	end;

	PModuleEntry32 = ^TModuleEntry32;
	TModuleEntry32 = record
		dwSize: DWORD;
		th32ModuleID: DWORD;  // This module
		th32ProcessID: DWORD; // owning process
		GlblcntUsage: DWORD;  // Global usage count on the module
		ProccntUsage: DWORD;  // Module usage count in th32ProcessID's context
		modBaseAddr: PBYTE;   // Base address of module in th32ProcessID's context
		modBaseSize: DWORD;   // Size in bytes of module starting at modBaseAddr
		hModule: HMODULE;     // The hModule of this module in th32ProcessID's context
		szModule: array[0..MAX_MODULE_NAME32] of Char;
		szExePath: array[0..MAX_PATH - 1] of Char;
	end;

	PThreadEntry32 = ^TThreadEntry32;
	TThreadEntry32 = record
		dwSize: DWORD;             // Size of this structure
		cntUsage: DWORD;           // Thread reference count. When becames zero the thread terminates
		th32ThreadID: DWORD;       // this thread ID
		th32OwnerProcessID: DWORD; // Process this thread is associated with
		tpBasePri: Longint;        // BitMask indicating the Thread Base Priority
		tpDeltaPri: Longint;       // Delta indicating the diference between the base and actual thread priority
		dwFlags: DWORD;            // Reserved, not used
	end;

	th32ProcessID = Integer;
	
	th32EnumCallbackThread = function( Snap: THandle; Thread: TThreadEntry32;
		var Data ): Boolean;
	th32EnumCallbackModule = function( Snap: THandle; Module: TModuleEntry32;
		var Data ): Boolean;
	th32EnumCallbackProcess = function( Snap: THandle; Process: TProcessEntry32;
		var Data ): Boolean;

function th32EnumThreads( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackThread ): Integer;
function th32EnumModules( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackModule ): Integer;
function th32EnumProcesses( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackProcess ): Integer;

{ Process Status Support - These features are available only for WinNT with the
													 aditional PSPApi.dll, founded into Win32 SDK. }	

type
	ntProcessID = Integer;

const
	MAX_PROCESSES_RUNNING: DWORD = 32 * KB;
	MAX_MODULES_IN_PROCESS: DWORD = 32 * KB;
												
procedure ntEnumProcesses( ss: TStrings );
procedure ntEnumModules( pID: ntProcessID; ss: TStrings );

{ Process/Modules information support }

function SystemProcessCount( const EXEName: string ): Integer;
procedure SystemProcessList( ss: TStrings; EXENameOnly, Sorted: Boolean );

function CheckSystemProcess( const EXEName: string ): Boolean;
procedure ForceSystemProcess( const EXEName: string );

function GetProcessName( pID: THandle ): string;
function GetCurrentProcessName: string;

function CheckAllModules( const EXEName: string; const Modules: array of string ): Boolean;
function CheckAllModulesPartial( const EXEName: string; const Modules: array of string ): Boolean;
function CheckAnyModules( const EXEName: string; const Modules: array of string ): Boolean;
function CheckAnyModulesPartial( const EXEName: string; const Modules: array of string ): Boolean;

function CheckCurrentAllModules( const Modules: array of string ): Boolean;
function CheckCurrentAllModulesPartial( const Modules: array of string ): Boolean;
function CheckCurrentAnyModules( const Modules: array of string ): Boolean;
function CheckCurrentAnyModulesPartial( const Modules: array of string ): Boolean;

{ returns a list with the full path of system-wide processes running with all/any of the
	listed modules loaded in its address space }
function SystemWideAllModules( ss: TStrings; Sorted: Boolean; const Modules: array of string ): Boolean;
function SystemWideAnyModules( ss: TStrings; Sorted: Boolean; const Modules: array of string ): Boolean;

procedure GetModuleList( const EXEName: string; ss: TStrings; EXENameOnly, Sorted: Boolean );
procedure GetCurrentModuleList( ss: TStrings; EXENameOnly, Sorted: Boolean );

procedure GetThreadList( const EXEName: string; ss: TStrings );
procedure GetCurrentThreadList( ss: TStrings );

{ NetWork Resource Support }

type

	TNetResourceFunc = function( ParentNetResource, NetResource: PNetResource;
    Data: Pointer ): Boolean;

	PNetResourceTree = ^TNetResourceTree;
	TNetResourceTree = record
		ParentNetResource : TNetResource;
		NetResource				: TNetResource;
		HasParent					: Boolean;
	end;

function NetResourceEqual( R1, R2: PNetResourceA ): Boolean;
procedure EnumNetResources( Root: PNetResource; const Recursive: Boolean;
	CallBack: TNetResourceFunc; Data: Pointer );

procedure DisposeNetResourceEntry( pnrl: PNetResourceTree );
procedure ClearNetResourceList( lst: TList );	
procedure GetNetResourcesList( lst: TList );
procedure GetDomainNames( sl: TStrings );
procedure GetDomainServersList( lst: TList; const domain: string );

{ Singleton Component Support }

function CheckSingleton( AOwner: TComponent; AClass: TComponentClass ): Boolean;
procedure ForceSingleton( AOwner: TComponent; AClass: TComponentClass );

{ Extract Package Information - in a better way }

type

	TKPackageInfo = ( piNeverBuild, piDesignTime, piRunTime, piEXE, piDPL, piDLL, piUnknown );
	TKPackageInfos = set of TKPackageInfo;

	TKPackageUnitFlag = ( pufMainUnit, pufPackageUnit, pufWakePackageUnit, pufOriginalWakePU,
		pufImplicityImported );
	TKPackageUnitFlags = set of TKPackageUnitFlag;

	TKPackageInfoLoadType = ( piltPackage, piltLibrary );

{
	Call this function will return in ssUnits the ALL units used by this package
	and all required package for this package. At ssUnits.Objects, there is the
	UnitFlags described above, to retrive then, just call GetPackageUnitFlags with
	this TStrings. The Flags will be cleared at the initialization of the process
	and if PackageName are a valid delphi package, this flag are filled with the
	appropriated information (see TKPackageInfos set above).
	The LoadType determines if the functions should load the desired package as a
	true package or as a single DLL. This can be useful in some circumstances that
	the user do not (or cannot) want to call the initialization/finalization parts
	of the package contained units.
	The ssUnits and ssPackage can be ommited, doing so, only the flags will return!
}
procedure GetPackageInfoEx( const PackageName: string; ssUnits, ssPackages: TStrings;
	var Flags: TKPackageInfos; LoadType: TKPackageInfoLoadType );

procedure GetKnowhowPackageInfo( const PackageName: string; ssUnits, ssPackages: TStrings;
	var Flags: TKPackageInfos; LoadType: TKPackageInfoLoadType );

{ Just convert the ssUnits.Object[index] into a more readable type! }
function GetPackageUnitFlags( PackageInfoUnitsList: TStrings; Index: Integer ): TKPackageUnitFlags;

function GetPackageDescription( const PackageName: string ): string;

function MsgToMessage( const msg: TMsg ): TMessage;
function MessageToMsg( const Message: TMessage ): TMsg;

{
--------------------------------------------------------------------------------
---------------------------- Generic Form Routines -----------------------------
--------------------------------------------------------------------------------
}

function FormByHandle( Handle: HWnd ): TForm;
procedure FormShowOnce( FormClass: TFormClass );
function FormShowModal( FormClass: TFormClass ): TModalResult;

procedure CloseFindForm;
function FindFirstForm( FormClass: TFormClass ): TForm;
function FindNextForm( FormClass: TFormClass ): TForm;

function GetBaseOwnerComponent( AComp: TComponent;
	RestrictOwnerClass: TComponentClass ): TComponent;

{
--------------------------------------------------------------------------------
---------------------------- Generic Debug Routines ----------------------------
--------------------------------------------------------------------------------
}

function DebugLogSnapshot( const FileName: string ): Boolean;
function DebugLogSnapshotFmt( const FileName: string; const Args: array of const ): Boolean;
procedure UniqueDebugLogSnapshot( const FileName: string );
procedure UniqueDebugLogSnapshotFmt( const FileName: string; const Args: array of const );

function DebugLogClear: Integer;
function DebugLogClearEx( const FileName: string ): Integer;
procedure DebugLogMessage( const Message: string );
procedure DebugLogMessageEx( const FileName, Message: string );
procedure DebugShowMessage( const Message: string );
procedure DebugLogMessageFmt( const Message: string; const Args: array of const );
procedure DebugLogMessageExFmt( const FileName, Message: string; const Args: array of const );
procedure DebugShowMessageFmt( const Message: string; const Args: array of const );

{
--------------------------------------------------------------------------------
------------------- Generic Pointer/Object Checking Routines -------------------
--------------------------------------------------------------------------------
}

function ProcessArray( const Source: array of const; IsTrim: Boolean ): Integer;

{ Smart Check/Force Routines }

function Check( const Source: array of const ): Boolean;
function CheckTrim( const Source: array of const ): Boolean;

procedure Force( const Source: array of const );
procedure ForceTrim( const Source: array of const );

{ General Check/Force Routines }

function CheckWindow( Source: HWnd ): Boolean;
function CheckHandle( Source: THandle ): Boolean;
function CheckSocket( Source: TSocket ): Boolean;
function CheckWin32Library( Source: HModule ): Boolean;
function CheckVariant( Source: Variant ): Boolean;
function CheckVarArray( Source: Variant ): Boolean;
function CheckVariantType( Source: Variant; VType: Integer ): Boolean;
function CheckVarArrayType( Source: Variant; VType: Integer ): Boolean;
function CheckPointer( Source: Pointer ): Boolean;
function CheckReference( const Source ): Boolean;
function CheckFile( const Source: string ): Boolean;
function CheckPath( const Source: string ): Boolean;
function CheckStr( const Source: string ): Boolean;
function CheckPChar( Source: PChar ): Boolean;
function CheckTrimStr( const Source: string ): Boolean;
function CheckTrimPChar( Source: PChar ): Boolean;

procedure ForceWindow( Source: HWnd );
procedure ForceHandle( Source: THandle );
procedure ForceSocket( Source: TSocket );
procedure ForceWin32Library( Source: HModule );
procedure ForceVariant( Source: Variant );
procedure ForceVarArray( Source: Variant );
procedure ForceVariantType( Source: Variant; VType: Integer );
procedure ForceVarArrayType( Source: Variant; VType: Integer );
procedure ForcePointer( Source: Pointer );
procedure ForceReference( const Source );
procedure ForceFile( const Source: string );
procedure ForcePath( const Source: string );
procedure ForceStr( const Source: string );
procedure ForcePChar( Source: PChar );
procedure ForceTrimStr( const Source: string );
procedure ForceTrimPChar( Source: PChar );

{ Object Check/Force Routines }

function CheckObject( Source: TObject ): Boolean;
function CheckClass( Source: TClass ): Boolean;
function CheckList( Source: TList ): Boolean;
function CheckStream( Source: TStream ): Boolean;
function CheckStrings( Source: TStrings ): Boolean;
function CheckCollection( Source: TCollection ): Boolean;
function CheckMethod( Source: TMethod ): Boolean;

function CheckObjectClass( Source: TObject; AClass: TClass ): Boolean;
function CheckObjectClasses( Source: TObject; const Classes: array of TClass ): Boolean;
function CheckClassReference( Source, AClass: TClass ): Boolean;
function CheckObjectClassName( Source: TObject; AClassName: ShortString ): Boolean;
function CheckClassNameReference( Source: TClass; AClassName: ShortString ): Boolean;

procedure ForceObject( Source: TObject );
procedure ForceClass( Source: TClass );
procedure ForceList( Source: TList );
procedure ForceStream( Source: TStream );
procedure ForceStrings( Source: TStrings );
procedure ForceCollection( Source: TCollection );
procedure ForceMethod( Source: TMethod );

procedure ForceObjectClass( Source: TObject; AClass: TClass );
procedure ForceObjectClasses( Source: TObject; const Classes: array of TClass );
procedure ForceClassReference( Source, AClass: TClass );
procedure ForceObjectClassName( Source: TObject; AClassName: ShortString );
procedure ForceClassNameReference( Source: TClass; AClassName: ShortString );

{ Interface Check/Force Routines }

function CreateGUID: string;

function CheckInterface( Source: IUnknown ): Boolean;
function CheckInterfaceClass( Source: IUnknown; const GUID: TGUID ): Boolean;

procedure ForceInterface( Source: IUnknown );
procedure ForceInterfaceClass( Source: IUnknown; const GUID: TGUID );

{ Plural Check/Force Routines }

function CheckObjects( const Source: array of TObject ): Boolean;
function CheckHandles( const Source: array of THandle ): Boolean;
function CheckInterfaces( const Source: array of IUnknown ): Boolean;
function CheckVariants( const Source: array of Variant ): Boolean;
function CheckVariantsType( const Source: array of Variant; VType: Integer ): Boolean;
function CheckVariantsTypes( const Source: array of Variant; const VTypes: array of Integer ): Boolean;
function CheckVarRecType( const Source: array of const; VarRecType: Byte ): Boolean;
function CheckVarRecTypes( const Source: array of const; const VarRecTypes: array of Byte ): Boolean;
function CheckPointers( const Source: array of Pointer ): Boolean;
function CheckStrs( const Source: array of string ): Boolean;
function CheckPChars( const Source: array of PChar ): Boolean;
function CheckTrimStrs( const Source: array of string ): Boolean;
function CheckFiles( const Files: array of string ): Boolean;
function CheckPaths( const Paths: array of string ): Boolean;

procedure ForceObjects( const Source: array of TObject );
procedure ForceHandles( const Source: array of THandle );
procedure ForceInterfaces( const Source: array of IUnknown );
procedure ForceVariants( const Source: array of Variant );
procedure ForceVariantsType( const Source: array of Variant; VType: Integer );
procedure ForceVariantsTypes( const Source: array of Variant; const VTypes: array of Integer );
procedure ForceVarRecType( const Source: array of const; VarRecType: Byte );
procedure ForceVarRecTypes( const Source: array of const; const VarRecTypes: array of Byte );
procedure ForcePointers( const Source: array of Pointer );
procedure ForceStrs( const Source: array of string );
procedure ForcePChars( const Source: array of PChar );
procedure ForceTrimStrs( const Source: array of string );
procedure ForceFiles( const Files: array of string );
procedure ForcePaths( const Paths: array of string );

{
--------------------------------------------------------------------------------
--------------------------- Generic Memory Routines ----------------------------
--------------------------------------------------------------------------------
}

function CheckHeap: Boolean;
procedure ZeroMemory( Source: Pointer; Count: Integer );

function IncPtr( Source: Pointer; Count: Integer ): Pointer;
function ForceIncPtr( Source: Pointer; Count: Integer ): Pointer;

function HostEntToIP( he: PHostEnt ): Integer;
function HostEntToIPAddr( he: PHostEnt ): TInAddr;

procedure StartupWinSock;
procedure CleanupWinSock;

{
	ScanMem scans the memory pointed by Source for the memory pointed by Token with
	the respective sizes. Returns 0 or the memory position. The serach is byte based
}
function ScanMem( const Token, Source; TokenSize, SourceSize: Cardinal ): Cardinal; assembler;

{
	GoToNextTokenBinPos receives the memory stream and call ScanMem at the current
	position as the source pointer. It returns the old stream position (before search)
	or -1 if the token could be found. It also adjust the stream position to the position
	of the token occurance (to do the next iteration, you MUST increment position by TokenSize).
}
function GoToNextTokenBinPos( sm: TMemoryStream; Token: Pointer; TokenSize: Integer ): Integer;

{
	GoToNextTokenBinPosEx check the parameters and call ScanMem directly. But the return
	value is zero based (instead of one based as ScanMem). To do the next iteration, you
	MUST increment the Source pointer by TokenSize. The function return -1 if no tokens
	are found (ScanMem return 0)
}
function GoToNextTokenBinPosEx( Token, Source: Pointer; TokenSize, SourceSize: Integer ): Integer;

function PosEx( const SubStr, s: string; Count: Integer ): Integer;

function CompareMemBytes( P1, P2: Pointer; Size: Integer ): ShortInt; assembler;
function CopyMemGlobalData( Handle: HGlobal ): HGlobal;

function StringToArray( const sIn: string; Token: Char; sOut: PStringArray ): Integer;

{
--------------------------------------------------------------------------------
---------------------------- Generic GUI Routines ------------------------------
--------------------------------------------------------------------------------
}

type

  TKShowWindowStyle = ( ssHide, ssNormal, ssMinimized, ssMaximized );

const
	DEFAULT_TEXT_BLOCK_PRECISION: TKPercent = 97;

	SHOW_WINDOW_STYLE: array[TKShowWindowStyle] of WORD =
	(
		SW_HIDE,
		SW_SHOWNORMAL,
		SW_SHOWMINIMIZED,
		SW_SHOWMAXIMIZED
	 );
	 
function TextBlock( const Source: string; ACanvas: TCanvas; MaxLen: Cardinal ): string;

function PointDivBy( Point: TPoint; DivBy: Integer ): TPoint;
function PointMulBy( Point: TPoint; MulBy: Integer ): TPoint;

function RectDivBy( Rect: TRect; DivBy: Integer ): TRect;
function RectMulBy( Rect: TRect; MulBy: Integer ): TRect;

procedure CenterWindow( Wnd: HWnd );

function GetAveCharSizeDC( DC: HDC ): TPoint;
function GetAveCharSize( Canvas: TCanvas ): TPoint;

function DisplayColors: LongInt;
function GetDesktopWorkArea: TRect;

procedure GetScreenFonts( Items: TStrings );
procedure GetPrinterFonts( Items: TStrings );

{ Window Enumeration Routines }

procedure EnumWindows( AList: TList );
procedure EnumVisibleWindows( AList: TList );

function GetShellTrayWindow: HWnd;

function GetTimerHwnd( Timer: TTimer ): HWND;

{ Palette routines }

function LogPaletteIndex( Index: Word ): TColor;
function LogPaletteRGB( r, g, b: Byte ): TColor;

type

	TKDisplayElement = ( de3DDkShadow, de3dLight, deActiveBorder, deActiveCaption,
		deAppWorkSpace, deBackground, deBtnFace, deBtnHighlight, deBtnShadow, deBtnText,
		deCaptionText, deGrayText, deHighlight, deHighlightText, deInactiveBorder,
		deInactiveCaption, deInactiveCaptionText, deInfoBk, deInfoText, deMenu,
		deMenuText, deScrollbar, deWindow, deWindowFrame, deWindowText );

function GetSystemColor( de: TKDisplayElement ): TColor;
procedure SetSystemColor( de: TKDisplayElement; Value: TColor );

{
--------------------------------------------------------------------------------
--------------------------- Generic Variant Routines ---------------------------
--------------------------------------------------------------------------------
}

function ArrayOfConstToVar( const Args: array of const ): Variant;
function ArrayOfStringToVar( const Args: array of string ): Variant;

function GetFirstClass( const Args: array of TClass ): TClass;
function GetFirstObject( const Args: array of TObject ): TObject;
function GetFirstPointer( const Args: array of Pointer ): Pointer;

function VarItemCount( const V: Variant ): Integer;
function VarArrayOfType( const Source: array of Variant; VType: Integer ): Variant;

{
	This low-level function translates a Variant Array (items in array must be of
	the same type as indicated by VType) into an array of the associated VType.
	Pass p as nil to return the memory necessary to call the function. If p is
	not nil, the result is the array bound.
	For OleStr variant arrays, the size necessary for the PPWideChar include the
	individual zeros but do not include the last one. Other topic about this kind
	of call is addressed to the fact the the functions supposes a cleared buffer and
	do not append any #0, ohterwise, it just offset size plus one
	For variant arrays with dimension greater than one, it will be converted to a
	single linear array. Is Up to the caller to treat the pointer as well...
	The same policy apply for typed pointer functions....
}
function VarArrayToArrayOfType( const V: Variant; VType: Integer; p: Pointer ): Integer;

type
	TKCnvVarArrayVariantToArrayOfCallBack = function( UserData: Pointer;
		Index, VType: Integer; const V: Variant ): Boolean;

function CnvVarArrayVariantToArrayOfType( const V: Variant; VType: Integer;
	CnvVarCallBack: TKCnvVarArrayVariantToArrayOfCallBack; UserData: Pointer ): Integer;

function VarArrayToArrayOfSmallInt( const V: Variant; psi: PSmallIntArray ): Integer;
function VarArrayToArrayOfInteger( const V: Variant; pi: PIntegerArray ): Integer;
function VarArrayToArrayOfSingle( const V: Variant; ps: PSingleArray ): Integer;
function VarArrayToArrayOfDouble( const V: Variant; pd: PDoubleArray ): Integer;
function VarArrayToArrayOfCurrency( const V: Variant; pc: PCurrencyArray ): Integer;
function VarArrayToArrayOfDateTime( const V: Variant; pdt: PDateTimeArray ): Integer;
function VarArrayToArrayOfByte( const V: Variant; pb: PByteArray ): Integer;

procedure VarArrayToBits( const V: Variant; b: TBits );
procedure BitsToVarArray( b: TBits; var V: Variant );

procedure VarArrayToStrings( const V: Variant; sl: TStrings );
procedure StringsToVarArray( sl: TStrings; var V: Variant );

{
--------------------------------------------------------------------------------
----------------------------- Generic File Routines ----------------------------
--------------------------------------------------------------------------------
}

function ApplicationPath: string;
function ApplicationLikeName( const Extension: string ): string;
function ApplicationSiblingName( const FileName: string ): string;

function GetUniqueFileName( const FileName: string ): string;
function GetUniqueFileNameFmt( const FileName: string; const Args: array of const ): string;

function CheckDeleteFile( const FileName: string ): Boolean;
procedure ForceDeleteFile( const Source: string );

function GetTempPath: string;
function GetTempPathFile( const Prefix: string; CreateFile: Boolean ): string;
function NewTempFileName( const Path, Prefix: string; CreateFile: Boolean ): string;

function WaitForFile( const FileName: string; TimeOut: DWORD ): Boolean;

function GetFileSize( const FileName: string; pHighDWORD: PLongInt ): LongInt;

function GetFileDateCreated( const FileName: string ): TDateTime;
function GetFileDateLastAccess( const FileName: string ): TDateTime;
function GetFileDateLastModified( const FileName: string ): TDateTime;

function SetFileDateCreated( const FileName: string; ADate: TDateTime ): Integer;
function SetFileDateLastAccess( const FileName: string; ADate: TDateTime ): Integer;
function SetFileDateLastModified( const FileName: string; ADate: TDateTime ): Integer;

function ExpandRelativePath( const BasePath, PathName: string ): string;
function RelativePath( const BasePath, PathName: string ): string;

{
--------------------------------------------------------------------------------
---------------------------- Generic String Routines ---------------------------
--------------------------------------------------------------------------------
}

type

	TKStringType = ( stStrings, stNames, stValues );

function AbsPos( const Token, Source: string ): Integer;
function AbsAnsiPos( const Token, Source: string ): Integer;

function CheckAnsiStrEqual( const S1, S2: string ): Boolean;
function CheckAnsiCaseStrEqual( const S1, S2: string ): Boolean;

function CheckAnsiStrContains( const Token, Source: string ): Boolean;
procedure ForceAnsiStrContains( const Token, Source: string );

function CheckAnsiStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
function CheckAnsiStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
procedure ForceAnsiStrContainsAny( const Tokens: array of string; const Source: string );
procedure ForceAnsiStrContainsAll( const Tokens: array of string; const Source: string );

function CheckAnsiAbsStrContains( const Token, Source: string ): Boolean;
procedure ForceAnsiAbsStrContains( const Token, Source: string );

function CheckAnsiAbsStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
function CheckAnsiAbsStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
procedure ForceAnsiAbsStrContainsAny( const Tokens: array of string; const Source: string );
procedure ForceAnsiAbsStrContainsAll( const Tokens: array of string; const Source: string );

function CheckStrEqual( const S1, S2: string ): Boolean;
function CheckCaseStrEqual( const S1, S2: string ): Boolean;
procedure ForceStrEqual( const S1, S2: string );
procedure ForceCaseStrEqual( const S1, S2: string );

function CheckStrContains( const Token, Source: string ): Boolean;
procedure ForceStrContains( const Token, Source: string );

function CheckStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
function CheckStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
procedure ForceStrContainsAny( const Tokens: array of string; const Source: string );
procedure ForceStrContainsAll( const Tokens: array of string; const Source: string );

function CheckAbsStrContains( const Token, Source: string ): Boolean;
procedure ForceAbsStrContains( const Token, Source: string );

function CheckAbsStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
function CheckAbsStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
procedure ForceAbsStrContainsAny( const Tokens: array of string; const Source: string );
procedure ForceAbsStrContainsAll( const Tokens: array of string; const Source: string );

function StrLeftPad( const Source: string; Len: Integer; ch: Char ): string;
function StrRightPad( const Source: string; Len: Integer; ch: Char ): string;
function StrAdjustLeft( const Source: string; Len: Integer; ch: Char ): string;
function StrAdjustRight( const Source: string; Len: Integer; ch: Char ): string;

function CharTrim( const Source: string; ch: Char ): string;
function CharLTrim( const Source: string; ch: Char ): string;
function CharRTrim( const Source: string; ch: Char ): string;

function CheckStrCharSet( const Source: string; CharSet: TKCharset ): Boolean;
procedure ForceStrCharSet( const Source: string; CharSet: TKCharset );
function CheckStrCharSetEnum( const Source: string; CharSet: TKCharsetEnum ): Boolean;
procedure ForceStrCharSetEnum( const Source: string; CharSet: TKCharsetEnum );
procedure ForceValidIdent( const Source: string );

function IsValidString( const Source: string; CharSet: TKCharsetEnum ): Boolean;
function NormalizeStringCharSet( const Source: string; CharSet: TKCharSet ): string;
function NormalizeString( const Source: string; CharSet: TKCharsetEnum ): string;
function FixStringCharSet( const Source: string; CharSet: TKCharset; Token: Char ): string;
function FixString( const Source: string; CharSet: TKCharsetEnum; Token: Char ): string;

function RemoveChar( const Source: string; OldChar: Char ): string;
function ReplaceChar( const Source: string; OldChar, NewChar: Char ): string;
function ReplaceChars( const Source: string; const OldChars: array of Char; NewChar: Char ): string;
function ReplaceCtrlChars( const Source, Token: string ): string;
function RemoveString( const Source, Tokens: string ): string;

function StrToCharSet( const Source: string ): TKCharSet;
function CharSetToStr( const CharSet: TKCharSet ): string;
function CharSetToSetStr( const CharSet: TKCharSet ): string;

function InvertString( const Source: string ): string;

function AnsiProperCase( const Source: string; const Tokens: TKCharSet ): string;

procedure ExtractStrings( const Source: string; Token: Char; Strings: TStrings );
function MakeString( Source: TStrings; Token: Char; Option: TKStringType ): string;
function MakeStringEx( const Source: array of string; Token: Char ): string;

function CountTokens( const AText, AToken: string ): Integer;

function RemoveLastChar( const Str: string; Ch: Char ): string;
function RemoveFirstChar( const Str: string; Ch: Char ): string;

{$IFNDEF DELPHI4}

type
	TKReplaceFlags = set of ( krfReplaceAll, krfIgnoreCase );

const
	krfAll = [krfReplaceAll, krfIgnoreCase];

function StringReplace( const S, OldPattern, NewPattern: string;
	Flags: TKReplaceFlags ): string;
function StringReplaceFmt( const S, OldPattern, NewPattern: string; Flags: TKReplaceFlags;
	const Args: array of const ): string;

{$ELSE}

const
	krfReplaceAll = rfReplaceAll;
	krfIgnoreCase = rfIgnoreCase;
	krfAll = [rfReplaceAll, rfIgnoreCase];

{$ENDIF}

function GetFirstString( const Args: array of string ): string;
function GetFirstPChar( const Args: array of PChar ): PChar;
function GetFirstStrContains( const Token: string; const Source: array of string ): string;

{
--------------------------------------------------------------------------------
------------------------ Generic Integer/Float Routines ------------------------
--------------------------------------------------------------------------------
}

function Max( A, B: LongInt ): LongInt;
function Min( A, B: LongInt ): LongInt;
function MinInteger( const Values: array of Integer ): Integer;
function MaxInteger( const Values: array of Integer ): Integer;

function ValueBetween( const Value, Lower, Higher: LongInt;
	const Include: Boolean ): Boolean;

function IsBitOn( Value: Integer; Bit: TKBitEnum ): Boolean;
function TurnBitOn( Value: Integer; Bit: TKBitEnum ): Integer;
function TurnBitOff( Value: Integer; Bit: TKBitEnum ): Integer;

function HexToInt( const Source: string ): Integer;

{
	Size of OutBuffer *MUST BE* two times long than InBuffer to leave room
	for the hexa decimal conversion. The BufSize should be StrLen( InBuffer )
}
procedure BinToHex( InBuffer, OutBuffer: PChar; BufSize: Integer ); assembler;

{
	OutBuffer is the HexaConverted Buffer, and is two times long than
	InBuffer. The BufSize should be StrLen( OutBuffer ). The result are
	the size in bytes of the InBuffer converted from OutBuffer
}
function HexToBin( OutBuffer, InBuffer: PChar; BufSize: Integer ): Integer; assembler;

function StrBinToStrHex( const sIn: string ): string;
function StrHexToStrBin( const sIn: string ): string;

function StreamBinToStrHex( Stream: TStream ): string;
procedure StrHexToStreamBin( const sIn: string; sOut: TStream );

function StrToFloatDef( const s: string; Default: Extended ): Extended;

{
--------------------------------------------------------------------------------
-------------------------- Generic Financial Routines --------------------------
--------------------------------------------------------------------------------
}

type
	EKMath = class( EKSystem );

function SolveForF( P, i, N: Extended ): Extended;
function SolveForP( F, i, N: Extended ): Extended;
function SolveFori( P, F, N: Extended ): Extended;
function SolveForN( P, F, i: Extended ): Extended;

function RSolveForF( R, i, N: Extended ): Extended;
function RSolveForP( R, i, N: Extended ): Extended;
function RSolveForRFromP( P, i, N: Extended ): Extended;
function RSolveForRFromF( F, i, N: Extended ): Extended;
function RSolveForiFromRP( R, P, N: Extended ): Extended;
function RSolveForiFromRF( R, F, N: Extended ): Extended;
function RSolveForNFromRP( R, P, i: Extended ): Extended;
function RSolveForNFromRF( R, F, i: Extended ): Extended;

procedure SetFinancialMaxTolerance( Value: Extended );
procedure SetFinancialMaxIterations( Value: Cardinal );

{
--------------------------------------------------------------------------------
-------------------------- Generic TDateTime Routines --------------------------
--------------------------------------------------------------------------------
}

type
	TKDatePart = ( dpYear, dpMonth, dpDay );
	TKTimePart = ( tpHour, tpMinute, tpSecond, tpMSecond );

function EndOfDay( ADate: TDateTime ): TDateTime;
function StartOfDay( ADate: TDateTime ): TDateTime;

function DaysInYear( AYear: Word ): Word;
function DaysInMonth( AYear, AMonth: Word ): Word;

function DatePart( ADate: TDateTime; ADatePart: TKDatePart ): Word;
function TimePart( ATime: TDateTime; ATimePart: TKTimePart ): Word;

procedure SetDatePart( var ADate: TDateTime; ADatePart: TKDatePart; AValue: Word );
procedure SetTimePart( var ATime: TDateTime; ATimePart: TKTimePart; AValue: Word );

function CurrentDatePart( ADatePart: TKDatePart ): Word;
function CurrentTimePart( ATimePart: TKTimePart ): Word;

function CurrentYear: Word;
function CurrentMonth: Word;
function CurrentDay: Word;
function CurrentHour: Word;
function CurrentMinute: Word;
function CurrentSecond: Word;
function CurrentMSecond: Word;

function DateDiff( Date1, Date2: TDateTime; ADatePart: TKDatePart ): Cardinal;
function TimeDiff( DateTime1, DateTime2: TDateTime; ATimePart: TKTimePart ): Cardinal;

function TimeToMSeconds( ATime: TDateTime ): Cardinal;
function TimeToSeconds( ATime: TDateTime ): Cardinal;
function TimeToMinutes( ATime: TDateTime ): Cardinal;
function TimeToHours( ATime: TDateTime ): Cardinal;

function DaysBetween( Date1, Date2: TDateTime ): Cardinal;
function MonthsBetween( Date1, Date2: TDateTime ): Cardinal;
function YearsBetween( Date1, Date2: TDateTime ): Cardinal;
function HoursBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
function MinutesBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
function SecondsBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
function MSecondsBetween( DateTime1, DateTime2: TDateTime ): Cardinal;

function IncDate( ADate: TDateTime; Days, Months, Years: Integer ): TDateTime;
function IncTime( ATime: TDateTime; Hours, Minutes, Seconds, MSecs: Integer ): TDateTime;

function IncDay( ADate: TDateTime; Delta: Integer ): TDateTime;
function IncMonth( ADate: TDateTime; Delta: Integer ): TDateTime;
function IncYear( ADate: TDateTime; Delta: Integer ): TDateTime;
function IncHour( ATime: TDateTime; Delta: Integer ): TDateTime;
function IncMinute( ATime: TDateTime; Delta: Integer ): TDateTime;
function IncSecond( ATime: TDateTime; Delta: Integer ): TDateTime;
function IncMSec( ATime: TDateTime; Delta: Integer ): TDateTime;

function SysDateTimeInfo: TSystemTime;

function ShorDateToString( Date: TDateTime ): string;
function LongDateToString( Date: TDateTime ): string;

function DayOfWeek( Date: TDateTime ): TKWeekEnum;
function WeekDay( WeekEnum: TKWeekEnum ): string;

{
--------------------------------------------------------------------------------
----------------------- Generic PChar/TStrings Routines ------------------------
--------------------------------------------------------------------------------
}

{
	Conversions from PChar to TStrings and from TStrings to PChar.
	Very handy to use in WINAPI calls that expect a list of PChars.
	Passing nil to StringsToPCharList will set the result to the
	necessary allocation space for the PChar.
}

procedure TrimStrings( Source: TStrings );
procedure AdjustStringsForNames( sl: TStrings );
procedure AdjustStringsForValues( sl: TStrings );

procedure PCharListToStringsEx( pc: Pointer; Len: Integer; sl: TStrings );
function PCharListToStrings( pc: PChar; sl: TStrings ): Integer;

function StringsToPCharList( sl: TStrings; pc: PChar ): Integer;
function StringsToPCharListEx( sl: TStrings; LowBound, HighBound: Cardinal; pc: PChar ): Integer;
function StrArrayToPCharList( const Strings: array of string; pc: PChar ): Integer;

{ All return values are in WideChars, i.e. the byte needed is Result * SizeOf( WideChar )! }
function PWideCharListLen( pwc: PWideChar ): Integer;
procedure PWideCharListToStrings( pwc: PWideChar; sl: TStrings );
function StringsToPWideCharList( sl: TStrings; pwc: PWideChar ): Integer;
function StringsToPWideCharListEx( sl: TStrings; LowBound, HighBound: Cardinal; pwc: PWideChar ): Integer;

function StrAllocMem( Size: Cardinal ): PChar;
function StrBufSize( pc: PChar ): Cardinal;

function StrWLen( Str: PWideChar ): Cardinal;
function StrWUpper( Str: PWideChar ): PWideChar; assembler;
function StrWLower( Str: PWideChar): PWideChar; assembler;
function StrWAlloc( Size: Cardinal ): PWideChar;
function StrWAllocMem( Size: Cardinal ): PWideChar;
function StrWBufSize( Str: PWideChar ): Cardinal;
function StrWNew( Str: PWideChar ): PWideChar;
procedure StrWDispose( Str: PWideChar );

procedure GetSections( const FileName: string; sl: TStrings );
procedure GetSection( const FileName: string; const SecName: string; sl: TStrings );

procedure StringsIntersect( Source, Target: TStrings );
procedure StringsDifference( Source, Target: TStrings );

function SensitiveIndexOf( Source: TStrings; const S: string ): Integer;

function StrScanEx( Str, SubStr: PChar ): PChar;

{
--------------------------------------------------------------------------------
--------------------------- Generic Stream Routines ----------------------------
--------------------------------------------------------------------------------
}

function CheckStreamCopy( Source, Target: TStream ): Boolean;
procedure ForceStreamCopy( Source, Target: TStream );

function NextLine( sm: TStream ): ShortString;
function NextShortString( sm: TStream; const Token: ShortString ): ShortString;
function CountTokenByLine( ss: TStream; const Token: ShortString ): Integer;

function NextLineContaining( ss: TStream; Tokens: Variant ): ShortString;
procedure AllLinesContaining( st: TStrings; ss: TStream; Tokens: Variant );
procedure AllLinesContainingPos( st: TStrings; ss: TStream; Tokens: Variant );
function CountTokensByLine( ss: TStream; Tokens: Variant ): Integer;

procedure FindLineStrings( st: TStrings; ss: TStream; const Token: string );

{
	It will found tokens in plain ASCII text! If between text there is a #0,
	the string will be cut!!! If you want to interate thru a binary text,
	use GoToNextTokenBinPos
}
function GoToNextLinePos( ss: TStringStream ): Integer;
function GoToNextLineAnsiPos( ss: TStringStream ): Integer;
function GoToNextTokenPos( ss: TStringStream; const Token: string; CaseSensitive: Boolean ): Integer;
function GoToNextTokenAnsiPos( ss: TStringStream; const Token: string; CaseSensitive: Boolean ): Integer;

procedure AllLinesContainingEx( st: TStrings; ss: TStream; const Tokens: array of const );
procedure AllLinesContainingPosEx( st: TStrings; ss: TStream; const Tokens: array of const );
function NextLineContainingEx( ss: TStream; const Tokens: array of const ): ShortString;
function CountTokensByLineEx( ss: TStream; const Tokens: array of const ): Integer;

function NextLLine( sm: TStream; MaxLen: LongInt ): string;
function NextString( sm: TStream; const Token: string; MaxLen: LongInt ): string;
function NextToken( sm: TStream; const Token: string; MaxLen: Integer ): Integer;
function CountTokenByLLine( ss: TStream; const Token: string; MaxLen: LongInt ): Integer;

function NextLLineContaining( ss: TStream; Tokens: Variant; MaxLen: LongInt ): string;
procedure AllLLinesContaining( st: TStrings; ss: TStream; Tokens: Variant; MaxLen: LongInt );
procedure AllLLinesContainingPos( st: TStrings; ss: TStream; Tokens: Variant; MaxLen: LongInt );
function CountTokensByLLine( ss: TStream; Tokens: Variant; MaxLen: LongInt ): Integer;

function NextLLineContainingEx( ss: TStream; const Tokens: array of const;
	MaxLen: LongInt ): string;
procedure AllLLinesContainingEx( st: TStrings; ss: TStream; const Tokens: array of const;
	MaxLen: LongInt );
procedure AllLLinesContainingPosEx( st: TStrings; ss: TStream; const Tokens: array of const;
	MaxLen: LongInt );
function CountTokensByLLineEx( ss: TStream; const Tokens: array of const;
	MaxLen: LongInt ): Integer;

procedure GetSectionsFromStreamEx( sm: TStream; sl: TStrings; SecCharOpen, SecCharClose: Char );
procedure GetSectionFromStreamEx( sm: TStream; const SecName: string; sl: TStrings; SecCharOpen, SecCharClose: Char );

procedure GetSectionsFromStream( sm: TStream; sl: TStrings );
procedure GetSectionFromStream( sm: TStream; const SecName: string; sl: TStrings );

{
--------------------------------------------------------------------------------
---------------------------- Generic CRC Routines ------------------------------
--------------------------------------------------------------------------------
}

function CRC16( CRC: Word; const Data; szData: Cardinal ): Word; assembler;
function CRC32( CRC: LongInt; const Data; szData: Cardinal ): LongInt; assembler;

function StreamCRC16( Seed: Word; Source: TStream ): Word;
function StreamCRC32( Seed: LongInt; Source: TStream ): LongInt;

{
--------------------------------------------------------------------------------
-------------------------- Generic Disk/Printer Routines -----------------------
--------------------------------------------------------------------------------
}

{ Drive support }

function CheckDriveLetter( Drive: Char ): Boolean;
procedure ForceDriveLetter( Drive: Char );

function VolumeID( Drive: Char ): string;
function DriveType( Drive: Char ): string;
procedure DriveList( sl: TStrings );

function CheckDriveReady( Drive: Char ): Boolean;
procedure ForceDriveReady( Drive: Char );

function NetworkVolume( Drive: Char ): string;
function HDSerialNumber( Drive: Char ): string;

{ Printer Support }

procedure GetPrinter( var DeviceMode, DeviceNames: THandle );
procedure SetPrinter( DeviceMode, DeviceNames: THandle );

{
--------------------------------------------------------------------------------
---------------- Generic Disk/Printer Connection Dialog Routines ---------------
--------------------------------------------------------------------------------
}

procedure ConnectDiskDialog;
procedure ConnectPrinterDialog;
procedure DisconnectDiskDialog;
procedure DisconnectPrinterDialog;

{
--------------------------------------------------------------------------------
--------------------------- Generic Shell Routines -----------------------------
--------------------------------------------------------------------------------
}

type

	TShellFileOperation = ( sfoCopy, sfoDelte, sfoMove, sfoRename );

	TShellFileAttribute = ( sfaReadOnly, sfaHidden, sfaSystem,
		sfaDirectory,	sfaArchive, sfaNormal, sfaTemporary, sfaCompressed );

	TShellFileOperationFlag = ( fofAllowUndo, fofFilesOnly,
		fofMultiDestFiles, fofNoConfirmation, fofNoConfirmMkDir,
		fofNoErrorUI, fofRenameOnCollision, fofSilent,
		fofSimpleProgress, fofWantMappingHandle );

	TShellFileInfoFlag = ( fifAttributes, fifDisplayName, fifIcon,
		fifIconLocation, fifLargeIcon, fifLinkOverlay, fifOpenIcon,
		{fifPIDL, } fifSelected, fifShellIconSize, fifSmallIcon,
		fifSysIconIndex, fifTypeName, fifUseFileAttributes );

	TShellSpecialFolder = ( sfDesktop, sfPrograms, sfControls, sfPrinters,
		sfPersonal, sfFavorites, sfStartup, sfRecent, sfSendTo, sfBitBucket,
		sfStartMenu, sfDesktopDirectory, sfDrives, sfNetwork, sfNetHood,
		sfFonts, sfTemplates, sfCommonStartMenu, sfCommonPrograms,
		sfCommonStartup, sfCommonDesktopDirectory, sfAppData, sfPrintHood,
		sfCookies );

	TShellFileInfoFlags = set of TShellFileInfoFlag;
	TShellFileAttributes = set of TShellFileAttribute;
	TShellFileOperationFlags = set of TShellFileOperationFlag;

function MakeFileFlags( Flags: TShellFileOperationFlags ): Word;

function MakeFileAttributes( FileAttr: TShellFileAttributes ): DWORD;
function RetrieveFileAttributes( Value: DWORD ): TShellFileAttributes;

function MakeFileInfo( FileInfo: TShellFileInfoFlags ): DWORD;
function RetrieveFileInfo( Value: DWORD ): TShellFileInfoFlags;

function ShellBindAppOpenWith( const AppName: string ): Boolean;
function ShellBindExtension( const FExt, FIcon, FDesc, FApp, FCmd: string ): Boolean;

function ShellCreateLink( const FLink, FName, FPath, FArgs: string ): Boolean;
function ShellCreateDesktopLink( const FLink, FName, FPath, FArgs: string ): Boolean;
function ShellCreateStartupLink( const FLink, FName, FPath, FArgs: string ): Boolean;

function ShellClearRecentDocs: Boolean;
function ShellAddToRecentDocs( const FileName: string ): Boolean;
function ShellSpecialFolderPath( Folder: TShellSpecialFolder ): string;

function ShellFileOperation( Handle: Hwnd; const Title: string;
	Operation : TShellFileOperation; Source, Dest: TStrings;
	Flags: TShellFileOperationFlags; var NameMappings: Pointer;
	var AnyOpAborted: Boolean ): Boolean;

function ShellGetFileInfo( const FileName: string;
	Attributes: TShellFileAttributes; Flags: TShellFileInfoFlags;
	var sfi: TSHFileInfo ): Integer;

function ShellBrowseFolder( const Caption: string; const Root: WideString;
	out Directory: string ): Boolean;

procedure ShellFileTypesList( st: TStrings );

function GetSystemDir: string;
function GetWindowsDir: string;

type
	TKWebAction = ( waHTTP, waHTTPS, waFTP, waMailTo, waNews, waCustom );

const
	WEB_PROTOCOL_NAME: array[TKWebAction] of string =
		( sWAHTTP, sWAHTTPs, sWAFTP, sWAMAILTO, sWANNTP, ''{waCustom!} );

procedure PerformWebAction( const Command: string; WebAction: TKWebAction;
	ShowWindowStyle: TKShowWindowStyle );

{##NI##} { Not to Include - Token used to strip this part of code for
           interfaces extraction process }

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsSystem_Shareware: Boolean;

{##NI##}

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

function PackageUserName: string;
function PackageCompanyName: string;
function PackageVersion: TKLibVersion;

implementation

{$R brsyDialogs.res}
{$R srsyWinsock.res}

uses
	{$IFDEF DELPHI4}SysConst{$ELSE}Consts{$ENDIF}, ExptIntf, RegStr, ActiveX,
  ComObj, ShlObj, Registry, Printers, StdCtrls, Buttons, CommDlg, ExtDlgs,
  Mask, ComCtrls, Math, ClipBrd, uksyResStr, uksyClasses, uksyPackReg;

{
--------------------------------------------------------------------------------
----------------- Generic Exception Architecture :: EKnowHow -------------------
--------------------------------------------------------------------------------
}

constructor EKnowHow.Create( const Message: string );
var
	sMsg: string;
begin
	sMsg := Message;
	ExceptionDigest( Self, sMsg );
	inherited Create( sMsg );
end;

constructor EKnowHow.CreateFmt( const Message: string; const Args: array of const );
begin
	Create( Format( Message, Args ) );
end;

constructor EKnowHow.CreateAt( const Message: string; Address: Pointer );
begin
	FRaiseAddress := Address;
	Create( Message );
end;

constructor EKnowHow.CreateAtFmt( const Message: string; Address: Pointer;
	const Args: array of const );
begin
	FRaiseAddress := Address;
	CreateFmt( Message, Args );
end;

constructor EKnowHow.CreateWin32( const Message: string {$IFDEF DELPHI4};
	Dummy: Integer = 0 {$ENDIF} );
begin
	FLastError := GetLastError;
	CreateFmt( sErrKnowHowWin32Expt, [Message, FLastError, SysErrorMessage( FLastError )] );
end;

constructor EKnowHow.CreateWin32Fmt( const Message: string; const Args: array of const
	{$IFDEF DELPHI4}; Dummy: Integer = 0 {$ENDIF} );
begin
	FLastError := GetLastError;
	CreateWin32( Format( Message, Args ) );
end;

{
--------------------------------------------------------------------------------
------------------- Generic Exception Architecture :: EKLIB --------------------
--------------------------------------------------------------------------------
}

{ EKLib }

function EKLib.GetReleaseDate: TDateTime;
begin
	Result := KLIBVersion.Release;
end;

function EKLib.GetVersion: string;
begin
	Result := KLIBVersion.Version;
end;

function KLIBVersion: TKLIBVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( VER_RELEASE_DATE );
	Result.Version := VER;
	Result.Reserved := VER_INT;
end;

function KLIBBaseRegKey: string;
begin
	Result := KNOWHOW_PACKAGES_BASE_REGKEY;
end;

{
--------------------------------------------------------------------------------
---------------------- Generic Exception Handling Routines ---------------------
--------------------------------------------------------------------------------
}

function InternalShowDialog( const Title, Message: string; TimeOut: Integer;
	Bitmap: TBitmap; Style: TKDialogStyle; BitmapOption: TKBitmapOption ): TModalResult; forward;

procedure ExceptionDigest( AException: EKnowHow; var ExceptionMessage: string );
begin
	if CheckPointer( @ExceptionDigestProc ) then
		ExceptionDigestProc( AException, ExceptionMessage );
end;

procedure RaiseException( EClass: EKnowHowClass; const Message: string );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
	raise EClass.Create( Message );
end;

procedure RaiseExceptionFmt( EClass: EKnowHowClass; const Message: string;
	const Args: array of const );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
	raise EClass.CreateFmt( Message, Args );
end;

procedure RaiseExceptionAt( EClass: EKnowHowClass; const Message: string;
	Address: Pointer );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
	raise EClass.CreateAt( Message, Address ) at Address;
end;

procedure RaiseExceptionAtFmt( EClass: EKnowHowClass; const Message: string;
	Address: Pointer; const Args: array of const );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
	raise EClass.CreateAtFmt( Message, Address, Args ) at Address;
end;

procedure RaiseWin32Exception( EClass: EKnowHowClass; const Message: string );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
 	raise EClass.CreateWin32( Message );
end;

procedure RaiseWin32ExceptionFmt( EClass: EKnowHowClass; const Message: string;
	const Args: array of const );
begin
  if ( not CheckClass( EClass ) ) then
    EClass := EKnowHow;
	raise EClass.CreateWin32Fmt( Message, Args );
end;

procedure FatalError( const Message: string );
const
	FATAL_ERROR_TIMEOUT = 30;
begin
	InternalShowDialog( sFatalError, Message, FATAL_ERROR_TIMEOUT, nil,
		dsOk, boHardBug );
	TerminateProcess( GetCurrentProcess, HARD_EXIT_CODE );
end;

procedure FatalErrorFmt( const Message: string; const Args: array of const );
begin
	FatalError( Format( Message, Args ) );
end;

procedure AbstractError;
begin
	if IsConsole then
		WriteLn( PChar( SAbstractError ) )
	else
		MessageBox( 0, PChar( SAbstractError ), 'Error', MB_OK );
	Halt( 210 ); { Abstract Run-Time Error Code }
end;
{
--------------------------------------------------------------------------------
------------------------- Generic Dialog Architecture --------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
	ShowDlgOnTopList: TList = nil;

const
	IM_TOP = 15;
	IM_LEFT = 12;
	IM_WIDTH = 32;
	IM_HEIGHT = 32;
	BN_WIDTH = 82;
	BN_HEIGHT = 28;
	BN_GUTTER = 10;
	BN_DISTANCE = 14;

	FM_NAME = 'fmKowHowDlg';
	FM_WIDTH = 480;
	FM_HEIGHT = 240;
	FM_GUTTER = 86;

	LB_TOP = 15;
	LB_LEFT = 68;
	LB_HEIGHT = 16;
	LB_WIDTH = FM_WIDTH - 106;

	FM_LEFTMARGIN = IM_LEFT;
	FM_RIGHTMARGIN = FM_WIDTH - LB_LEFT - LB_WIDTH;

	MIN_DLG_TIMEOUT = 5000;

{-------------------------------- TKInternalForm --------------------------------}

type

	TKInternalForm = class( TKCustomInternalForm )
	private
		FImage: TImage;
		FTimer: TTimer;
		FBitmap: TBitmap;
		FBitmapOption: TKBitmapOption;
    
	protected
		procedure FixButtons; override;
		procedure PrepareForm; override;
		procedure DestroyCtrls; override;
		procedure DoPrepareShow; override;
		procedure DialogOnTimer( Sender: TObject ); dynamic;

		function CopyToClipBoard: Boolean; override;

	public
		destructor Destroy; override;
		constructor CreateDialog( AOwner: TComponent; ATimeOut: Integer ); dynamic;

		property Bitmap: TBitmap
						 read FBitmap write FBitmap;
		property BitmapOption: TKBitmapOption
						 read FBitmapOption write FBitmapOption;

	end;

constructor TKInternalForm.CreateDialog( AOwner: TComponent; ATimeOut: Integer );
begin
  inherited Create( AOwner );

	FBitmapOption := boInformation;

	ATimeOut := ( ATimeOut * SECOND_TO_MSECOND );
	if ( ATimeOut <> 0 ) and ( ATimeOut < MIN_DLG_TIMEOUT ) then
		ATimeOut := MIN_DLG_TIMEOUT;
	if ( ATimeOut >= MIN_DLG_TIMEOUT ) then
	begin
		FTimer := TTimer.Create( nil );
		FTimer.Interval := ATimeOut;
		FTimer.OnTimer := DialogOnTimer;
	end;
end;

destructor TKInternalForm.Destroy;
var
	i: Integer;
begin
	FTimer.Free;
	if ( not IsShowModal ) then
		for i := 0 to ShowDlgOnTopList.Count - 1 do
			if ( TKInternalForm( ShowDlgOnTopList[i] ) = Self ) then
			begin
			  ShowDlgOnTopList.Delete( i );
				Break;
			end;
	inherited Destroy;
end;

procedure TKInternalForm.DialogOnTimer( Sender: TObject );
begin
	FTimer.Enabled := false;
	if IsShowModal then
		ModalResult := mrCancel
	else
		Close;
end;

procedure TKInternalForm.DoPrepareShow;
begin
	if ( not IsShowModal ) then
	begin
		if ( not CheckObject( ShowDlgOnTopList ) ) then
			ShowDlgOnTopList := TList.Create;
		ShowDlgOnTopList.Add( Self );
	end;	
	inherited DoPrepareShow;
end;

procedure TKInternalForm.PrepareForm;
var
	rt: TRect;
	iMinWidth: Integer;
begin
	inherited PrepareForm;
	FImage := TImage.Create( nil );
	with FImage do
	begin
		Name := 'imLogo';
		Parent := Self;
		Stretch := true;
		AutoSize := false;
		Transparent := true;
		Top := ScaleY( IM_TOP );
		Left := ScaleX( IM_LEFT );
		Width := ScaleX( IM_WIDTH );
		Height := ScaleY( IM_HEIGHT );
		with Picture.Bitmap do
			if CheckObject( FBitmap ) then
				Assign( FBitmap )
			else
				LoadFromResourceName( HInstance, SYSDLG_BMP_RESNAMES[BitmapOption] );
	end;
	with lbMessage do
	begin
		Parent := Self;
		AutoSize := false;
		Caption := Self.Text;
		Top := ScaleY( LB_TOP );
		Left := ScaleX( LB_LEFT );
		Width := ScaleX( LB_WIDTH );
		Height := ScaleY( LB_HEIGHT );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > ScaleX( LB_WIDTH ) ) then
		begin
			Caption := TextBlock( Caption, Canvas, ScaleX( LB_WIDTH ) );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Width := rt.Right;
		Height := Max( rt.Bottom, 2 * Height );
		Self.Height := Top + Height + ScaleY( FM_GUTTER );
		Self.Width := Self.Width - ( ScaleX( LB_WIDTH ) - Width );
		with Buttons do
			iMinWidth := ScaleX( Count * BN_WIDTH + ( Count - 1 ) * BN_DISTANCE +
				FM_LEFTMARGIN + FM_RIGHTMARGIN );
		if ( Self.Width < iMinWidth ) then
			Self.Width := iMinWidth;
		DialogData.RightAlign := false;
	end;
end;

function TKInternalForm.CopyToClipBoard: Boolean;
begin
	Clipboard.AsText := Text;
	Result := True;
end;

procedure TKInternalForm.FixButtons;
begin
	inherited FixButtons;
	if CheckObject( FTimer ) then
		FTimer.Enabled := true;
end;

procedure TKInternalForm.DestroyCtrls;
begin
	FImage.Free;
	inherited DestroyCtrls;
end;

{------------------------------ TKInternalInputForm -----------------------------}

const
	ED_HEIGHT = 24;
	ED_GUTTER = 10;

	FM_EDWIDTH = 380;

type

	TKInternalInputForm = class( TKCustomInternalForm )
	private
		FEdit: TMaskEdit;
		FDefText: string;
		FEditMask: string;
		FModified: Boolean;
    FPasswordChar: Char;

	protected
		procedure PrepareForm; override;
		procedure UnprepareForm; override;

	public
    property PasswordChar: Char
             read FPasswordChar write FPasswordChar;
		property DefText: string
						 read FDefText write FDefText;
		property EditMask: string
						 read FEditMask write FEditMask;
		property Modified: Boolean
						 read FModified;

	end;

procedure TKInternalInputForm.PrepareForm;
var
	rt: TRect;
begin
	inherited PrepareForm;
	Width := FM_EDWIDTH;
	FEdit := TMaskEdit.Create( nil );
	if CheckTrimStr( FEditMask ) then
		FEdit.EditMask := FEditMask;
	with FEdit do
	begin
		Parent := Self;
		Text := DefText;
		Left := ScaleX( IM_LEFT );
		Width := Self.ClientWidth - 2 * Left;
		Height := ScaleY( ED_HEIGHT );
    PasswordChar := Self.PasswordChar;
	end;
	with lbMessage do
	begin
		Parent := Self;
		AutoSize := false;
		Caption := Text;
		Top := ScaleY( LB_TOP );
		Left := FEdit.Left;
		Width := FEdit.Width;
		Height := ScaleY( LB_HEIGHT );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > FEdit.Width ) then
		begin
			Caption := TextBlock( Caption, Canvas, FEdit.Width );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Height := rt.Bottom;
		FEdit.Top := Top + Height + ScaleY( ED_GUTTER );
		Self.Height := FEdit.Top + FEdit.Height + ScaleY( FM_GUTTER + ED_GUTTER );
		DialogData.RightAlign := false;
	end;
	FModified := false;
	ActiveControl := FEdit;
end;

procedure TKInternalInputForm.UnprepareForm;
begin
  FModified := FEdit.Modified;
	if Modified then
    FDefText := FEdit.Text;
  FreeClean( FEdit );
	inherited UnprepareForm;
end;

{---------------------------- TKInternalInputListForm ---------------------------}

type

	TKInternalInputListForm = class( TKCustomInternalForm )
	private
		FCombo: TComboBox;
		FStrings: TStrings;
		FItemIndex: Integer;
		FAcceptUserInput: Boolean;
		FResultString: string;

	protected
		procedure PrepareForm; override;
		procedure UnprepareForm; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure AssignList( List: TStrings );

		property ItemIndex: Integer
						 read FItemIndex write FItemIndex;
		property AcceptUserInput: Boolean
						 read FAcceptUserInput write FAcceptUserInput;
		property ResultString: string
						 read FResultString write FResultString;

	end;

constructor TKInternalInputListForm.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FStrings := TStringList.Create;
	FAcceptUserInput := False;
	FResultString := '';
	FItemIndex := -1;
end;

destructor TKInternalInputListForm.Destroy;
begin
	FreeClean( FStrings );
	FreeClean( FCombo );
	inherited Destroy;
end;

procedure TKInternalInputListForm.AssignList( List: TStrings );
begin
	FStrings.Assign( List );
end;

procedure TKInternalInputListForm.PrepareForm;
const
	COMBO_STYLE: array[Boolean] of TComboBoxStyle = ( csDropDownList, csDropDown );
var
	rt: TRect;
begin
	inherited PrepareForm;
	Width := FM_EDWIDTH;
	FCombo := TComboBox.Create( nil );
	with FCombo do
	begin
		Parent := Self;
		Text := '';
		Left := ScaleX( IM_LEFT );
		Width := Self.ClientWidth - 2 * Left;
		Height := ScaleY( ED_HEIGHT );
		Style := COMBO_STYLE[AcceptUserInput];
		Items.Assign( FStrings );
		ItemIndex := Self.ItemIndex;
		FResultString := Items[ItemIndex];
	end;
	with lbMessage do
	begin
		Parent := Self;
		AutoSize := false;
		Caption := Text;
		Top := ScaleY( LB_TOP );
		Left := FCombo.Left;
		Width := FCombo.Width;
		Height := ScaleY( LB_HEIGHT );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > FCombo.Width ) then
		begin
			Caption := TextBlock( Caption, Canvas, FCombo.Width );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Height := rt.Bottom;
		FCombo.Top := Top + Height + ScaleY( ED_GUTTER );
		Self.Height := FCombo.Top + FCombo.Height + ScaleY( FM_GUTTER + ED_GUTTER );
		DialogData.RightAlign := false;
	end;
	ActiveControl := FCombo;
end;

procedure TKInternalInputListForm.UnprepareForm;
begin
	ItemIndex := FCombo.ItemIndex;
	FResultString := FCombo.Text;
	AssignList( FCombo.Items );
	FreeClean( FCombo );
	inherited UnprepareForm;
end;

{----------------------------- TKInternalProgressForm ---------------------------}

const
  KM_CLOSEMODAL = KM_USER + 1;

type

	TKInternalProgressForm = class;

{ TKProgressThread }

	TInternalProgressThread = class( TThread )
	private
		FForm: TKInternalProgressForm;

	protected
		procedure Execute; override;
		procedure UserExecute; virtual;
		function StartCondition: Boolean; virtual;
		function ExecuteCondition: Boolean; virtual;

	public
		destructor Destroy; override;
		constructor Create( Form: TKInternalProgressForm );

	end;

	TKInternalProgressForm = class( TKCustomInternalForm )
	private
		FEvent: THandle;
		FStatus: TLabel;
		FProgress: TProgressBar;
		FProgressVisible: Boolean;
		FStatusAlignment: TAlignment;
		FData: Pointer;
		FUserCanceled: Boolean;
		FCancelEnabled: Boolean;
		FCallback: TKProgressDialogCallBack;
		FInfoStyle: TKProgressInfoStyle;
		FFormTerminated: Boolean;
		FThread: TInternalProgressThread;
		FProgressPosition: TKPercent;

{
		procedure WMSysCommand( var Message: TWMSysCommand );
							message WM_SYSCOMMAND;
}
		procedure KMCloseModal( var Message: TMessage );
							message KM_CLOSEMODAL;

		procedure BtnCloseEvent( Sender: TObject );
		procedure FormCloseQuery( Sender: TObject; var CanClose: Boolean );
		procedure FormPrgsClose( Sender: TObject; var CloseAction: TCloseAction ); 

		procedure PrepareThread;
		procedure FormTerminated;

	protected
		procedure Progress;
		procedure PrepareForm; override;
		procedure UnprepareForm; override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

	public
		property Callback: TKProgressDialogCallBack
						 read FCallback write FCallback;
		property CancelEnabled: Boolean
						 read FCancelEnabled write FCancelEnabled;
		property ProgressVisible: Boolean
						 read FProgressVisible write FProgressVisible;
		property Data: Pointer
						 read FData write FData;
		property StatusAlignment: TAlignment
						 read FStatusAlignment write FStatusAlignment;
		property ProgressPosition: TKPercent
						 read FProgressPosition;

	end;

{ TInternalProgressThread }

constructor TInternalProgressThread.Create( Form: TKInternalProgressForm );
begin
	FForm := Form;
	FreeOnTerminate := True;
	inherited Create( False );
end;

destructor TInternalProgressThread.Destroy;
begin
	inherited Destroy;
	SetEvent( FForm.FEvent );
end;

function TInternalProgressThread.StartCondition: Boolean;
begin
	Result := ( CheckObject( FForm ) and FForm.Visible and FForm.Showing );
end;

function TInternalProgressThread.ExecuteCondition: Boolean;
begin
	Result := ( StartCondition and ( not FForm.FFormTerminated ) and
		( ( FForm.ProgressPosition < DIALOG_PROGRESS_TERMINATED ) or
		( not FForm.FUserCanceled ) ) and ( FForm.ModalResult = mrNone ) );
end;

procedure TInternalProgressThread.Execute;
begin
	while ( not StartCondition ) do ; { Cool!!! Busy Wait }
	while ExecuteCondition do
		Synchronize( UserExecute );
end;

procedure TInternalProgressThread.UserExecute;
begin
	if ExecuteCondition then
		FForm.Progress;
end;

{ TKInternalProgressForm }

procedure TKInternalProgressForm.FormTerminated;
begin
	FFormTerminated := True;
end;

procedure TKInternalProgressForm.Progress;
const
	STATUS_STYLE: array[TKProgressInfoStyle] of DWORD =
	(
		DT_LEFT or DT_EXPANDTABS,
		DT_LEFT or DT_EXPANDTABS or DT_PATH_ELLIPSIS,
		DT_LEFT or DT_EXPANDTABS or DT_END_ELLIPSIS
	);
var
	rt: TRect;
	sStatus: string;
begin
	sStatus := FStatus.Caption;
	FProgressPosition := DIALOG_PROGRESS_DISABLED;
	if ( CheckObject( FProgress ) and FProgress.Visible ) then
		FProgressPosition := FProgress.Position;
	ExecutorProc( FData );
	Callback( FData, sStatus, FProgressPosition, FUserCanceled, FInfoStyle );
	if FUserCanceled then
	begin
		PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
		Exit;
	end
	else if ( FProgressPosition >= DIALOG_PROGRESS_TERMINATED ) then
	begin
		PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrOK ), 0 );
		Exit;
	end;
	if ( not CheckStrEqual( FStatus.Caption, sStatus ) ) then
	begin
		FStatus.Caption := sStatus;
		rt := FStatus.BoundsRect;
		Canvas.Brush.Color := Color;
		Canvas.FillRect( rt );
		DrawTextEx( Canvas.Handle, PChar( sStatus ), -1, rt, STATUS_STYLE[FInfoStyle], nil );
	end;
	if ( CheckObject( FProgress ) and FProgress.Visible and
		 ( FProgressPosition > FProgress.Position ) ) then
		FProgress.Position := FProgressPosition;
end;

procedure TKInternalProgressForm.PrepareThread;
begin
	FFormTerminated := False;
	FEvent := CreateEvent( nil, false, false, '' );
	FThread := TInternalProgressThread.Create( Self );
end;

procedure TKInternalProgressForm.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( Key = VK_ESCAPE ) or ( ( ssAlt in Shift ) and ( Key = VK_F4 ) ) then
		Key := 0;
	inherited KeyDown( Key, Shift );
end;

procedure TKInternalProgressForm.PrepareForm;
var
	rt: TRect;
begin
	inherited PrepareForm;
	FInfoStyle := pisNormal;
	FUserCanceled := false;
	ForceObject( TBitBtn( Buttons[0] ) );
	with TBitBtn( Buttons[0] ) do
	begin
		ModalResult := mrNone;
		Enabled := FCancelEnabled;
		OnClick := BtnCloseEvent;
	end;
	Width := FM_EDWIDTH;
	FProgress := TProgressbar.Create( nil );
	with FProgress do
	begin
		Name := 'pbProgress';
		Parent := Self;
		Left := ScaleX( IM_LEFT );
		Width := Self.ClientWidth - 2 * Left;
		Visible := ProgressVisible;
		Height := ScaleY( ED_HEIGHT );
	end;
	with lbMessage do
	begin
		Parent := Self;
		AutoSize := false;
		Caption := Text;
		Top := ScaleY( LB_TOP );
		Left := FProgress.Left;
		Width := FProgress.Width;
		Height := ScaleY( LB_HEIGHT );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > FProgress.Width ) then
		begin
			Caption := TextBlock( Caption, Canvas, FProgress.Width );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Text ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Height := rt.Bottom;
		FProgress.Top := Top + Height + ScaleY( ED_GUTTER );
		Self.Height := FProgress.Top + FProgress.Height + ScaleY( FM_GUTTER + ED_GUTTER );
		DialogData.RightAlign := false;
	end;                                                
	FStatus := TLabel.Create( nil );
	with FStatus do
	begin
		Parent := Self;
		Caption := '';
		AutoSize := false;
		Left := FProgress.Left;
		Width := FProgress.Width;
		Height := ScaleY( LB_HEIGHT );
		Top := FProgress.Top + FProgress.Height + ScaleY( ED_GUTTER );
		Self.Height := Self.Height + FProgress.Height + ScaleY( ED_GUTTER );
		Visible := false;
	end;
	OnClose := FormPrgsClose;
	OnCloseQuery := FormCloseQuery;
	PrepareThread;
end;

procedure TKInternalProgressForm.KMCloseModal( var Message: TMessage );
begin
	OnCloseQuery := nil;
	ModalResult := TModalResult( Message.WParam );
end;

(*
procedure TKInternalProgressForm.WMSysCommand( var Message: TWMSysCommand );
begin
	with Message do
		if ( CmdType = SC_CLOSE ) then
		begin
			Result := 0; 				{ Tell everyone that we already process this message }
			BtnCloseEvent( nil );  { Notify the late closing mechanism }
		end
		else
			inherited;
end;
*)

procedure TKInternalProgressForm.BtnCloseEvent( Sender: TObject );
begin
	PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
end;

procedure TKInternalProgressForm.FormCloseQuery( Sender: TObject; var CanClose: Boolean );
begin
	CanClose := false;
	PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
end;

procedure TKInternalProgressForm.FormPrgsClose( Sender: TObject; var CloseAction: TCloseAction );
begin
	FormTerminated;
	WaitForSingleObject( FEvent, INFINITE );
	CloseHandle( FEvent );
	FEvent := NULL_HANDLE_VALUE;
end;

procedure TKInternalProgressForm.UnprepareForm;
begin
	FStatus.Free;
	FProgress.Free;
	inherited UnprepareForm;
end;

{------------------------------ Internal Functions -----------------------------}

function InternalShowDialog( const Title, Message: string; TimeOut: Integer;
	Bitmap: TBitmap; Style: TKDialogStyle; BitmapOption: TKBitmapOption ): TModalResult;
var
	fm: TKInternalForm;
begin
	fm := TKInternalForm.CreateDialog( nil, TimeOut );
	try
		fm.Text := Message;
		if ( BitmapOption = boCustom ) then
  		fm.Bitmap := Bitmap;
		fm.IsShowModal := True;
		fm.Caption := Title;
		fm.DialogStyle := Style;
		fm.BitmapOption := BitmapOption;
		Result := fm.ShowModal;
	finally
		fm.Free;
	end;
end;

procedure InternalShowDialogOnTop( const Title, Message: string; TimeOut: Integer;
	Bitmap: TBitmap; Style: TKDialogStyle; BitmapOption: TKBitmapOption );
var
	fm: TKInternalForm;
begin
	fm := TKInternalForm.CreateDialog( nil, TimeOut );
	try
		fm.Text := Message;
		fm.IsShowModal := False;
		if ( BitmapOption = boCustom ) then
  		fm.Bitmap := Bitmap;
		fm.Caption := Title;
		fm.DialogStyle := Style;
		fm.BitmapOption := BitmapOption;
		fm.FormStyle := fsStayOnTop;
		fm.Show;
	except
		fm.Free;
		raise;
	end;
end;

function InternalInputDialog( const Title, Prompt, EditMask: string;
	var Text: string ): Boolean;
var
	fm: TKInternalInputForm;
begin
	fm := TKInternalInputForm.Create( nil );
	try
		fm.Text := Prompt;
		fm.DefText := Text;
		fm.Caption := Title;
		fm.EditMask := EditMask;
    fm.PasswordChar := CH_NULL;
		fm.DialogStyle := dsOKCancel;
		Result :=	( fm.ShowModal = mrOK );// and fm.Modified );
		if Result then
			Text := fm.DefText;
	finally
		fm.Free;
	end;
end;

function InternalPasswordDialog( const Title, Prompt, EditMask: string; PasswordChar: Char;
	var Text: string ): Boolean;
var
	fm: TKInternalInputForm;
begin
	fm := TKInternalInputForm.Create( nil );
	try
		fm.Text := Prompt;
		fm.DefText := Text;
		fm.Caption := Title;
		fm.EditMask := EditMask;
		fm.DialogStyle := dsOKCancel;
    fm.PasswordChar := PasswordChar;
		Result :=	( fm.ShowModal = mrOK );// and fm.Modified );
		if Result then
			Text := fm.DefText;
	finally
		fm.Free;
	end;
end;

function InternalInputListDialog( const Title, Prompt: string; DefaultIndex: Integer;
	List: TStrings ): Integer;
var
	fm: TKInternalInputListForm;
begin
  ForceObject( List );
	fm := TKInternalInputListForm.Create( nil );
	try
		fm.Text := Prompt;
		fm.AcceptUserInput := False;
		fm.ItemIndex := DefaultIndex;
		fm.Caption := Title;
		fm.DialogStyle := dsOKCancel;
		fm.AssignList( List );
		Result := -1;
		if ( fm.ShowModal = mrOk ) then
			Result := fm.ItemIndex;
	finally
		fm.Free;
	end;
end;

function InternalInputListDialogEx( const Title, Prompt: string; DefaultIndex: Integer;
	AcceptUserInput: Boolean; List: TStrings ): string;
var
	fm: TKInternalInputListForm;
begin
	ForceObject( List );
	fm := TKInternalInputListForm.Create( nil );
	try
		fm.Text := Prompt;
		fm.AcceptUserInput := AcceptUserInput;
		fm.ItemIndex := DefaultIndex;
		fm.Caption := Title;
		fm.DialogStyle := dsOKCancel;
		fm.AssignList( List );
		Result := '';
		if ( fm.ShowModal = mrOk ) then
			Result := fm.ResultString;
	finally
		fm.Free;
	end;
end;

function InternalProgressDialog( const Title, Message: string; StatusAlignment: TAlignment;
	AllowCancel, ProgressVisible: Boolean; Callback: TKProgressDialogCallBack;
	ExecutorProc: TKDialogExecuteProc; Data: Pointer ): Boolean;
var
	fm: TKInternalProgressForm;
begin
	ForceReference( @Callback );
	fm := TKInternalProgressForm.Create( nil );
	try
		fm.Data := Data;
		fm.Text := Message;
		fm.Caption := Title;
		fm.Callback := Callback;
		fm.DialogStyle := dsCancel;
		fm.CancelEnabled := AllowCancel;
		fm.ExecutorProc := ExecutorProc;
		fm.ProgressVisible := ProgressVisible;
		fm.StatusAlignment := StatusAlignment;
		Result := ( fm.ShowModal = mrOK ) and ( not fm.FUserCanceled );
	finally
		fm.Free;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure ResetDialogData;
begin
	with DialogData do
	begin
		Top := INVALID_POSITION;
		Left := INVALID_POSITION;
		Centered := true;
		Font.Name := 'Arial';
		Font.Size := 8;
		Font.Style := [];
		Font.Color := clBlack;
		RightAlign := false;
		ToolWindow := false;
		swCallBack := nil;
		UserData := nil;
	end;
end;

procedure ClearSystemDialogProcs;
begin
	ShowDialogProc := nil;
	ShowDialogOnTopProc := nil;
	ExpireShowDialogProc := nil;
	ExpireShowDialogOnTopProc := nil;
	InputDialogProc := nil;
  PasswordDialogProc := nil;
	InputListDialogProc := nil;
	InputListDialogExProc := nil;
	InputCheckListDialogFunc := nil;
	ProgressDialogProc := nil;
end;

procedure AssignFontFromFontData( Font: TFont; FontData: TKFontData );
begin
	ForceObject( Font );
	with Font do
	begin
		Name := FontData.Name;
		Size := FontData.Size;
		Style := FontData.Style;
		Color := FontData.Color;
	end;
end;

procedure AssignFontToFontData( Font: TFont; var FontData: TKFontData );
begin
	ForceObject( Font );
	ZeroMemory( @FontData, SizeOf( TKFontData ) );
	with FontData do
	begin
		Name := Font.Name;
		Size := Font.Size;
		Style := Font.Style;
		Color := Font.Color;
	end;
end;

function Confirm( const Prompt: string ): Boolean;
begin
	Result := ( ShowDialog( sConfirmation, Prompt, nil, dsYesNo, boInformation ) = mrYes );
end;

function ConfirmFmt( const Prompt: string; const Args: array of const ): Boolean;
begin
	Result := Confirm( Format( Prompt, Args ) );
end;

procedure Inform( const Prompt: string );
begin
	ShowDialog( sInformation, Prompt, nil, dsOk, boInformation );
end;

procedure InformFmt( const Prompt: string; const Args: array of const );
begin
	Inform( Format( Prompt, Args ) );
end;

procedure Warn( const Prompt: string );
begin
	ShowDialog( sWarning, Prompt, nil, dsOk, boExclamation01 );
end;

procedure WarnFmt( const Prompt: string; const Args: array of const );
begin
	Warn( Format( Prompt, Args ) );
end;

procedure ShowError( const Prompt: string );
begin
	ShowDialog( sError, Prompt, nil, dsOk, boHardBug );
end;

procedure ShowErrorFmt( const Prompt: string; const Args: array of const );
begin
	ShowError( Format( Prompt, Args ) );
end;

procedure InformOnTop( const Prompt: string );
begin
	ShowDialogOnTop( sInformation, Prompt, nil, dsOk, boInformation );
end;

procedure InformOnTopFmt( const Prompt: string; const Args: array of const );
begin
	InformOnTop( Format( Prompt, Args ) );
end;

procedure WarnOnTop( const Prompt: string );
begin
	ShowDialogOnTop( sWarning, Prompt, nil, dsOk, boExclamation01 );
end;

procedure WarnOnTopFmt( const Prompt: string; const Args: array of const );
begin
	WarnOnTop( Format( Prompt, Args ) );
end;

procedure ShowErrorOnTop( const Prompt: string );
begin
	ShowDialogOnTop( sError, Prompt, nil, dsOk, boHardBug );
end;

procedure ShowErrorOnTopFmt( const Prompt: string; const Args: array of const );
begin
	ShowErrorOnTop( Format( Prompt, Args ) );
end;

function ExpireConfirm( SecTimeOut: Cardinal; const Prompt: string ): Boolean;
begin
	Result := ( ExpireShowDialog( sConfirmation, Prompt, SecTimeOut, nil, dsYesNo,
	  boInformation ) = mrYes );
end;

function ExpireConfirmFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const ): Boolean;
begin
	Result := ExpireConfirm( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireInform( SecTimeOut: Cardinal; const Prompt: string );
begin
  ExpireShowDialog( sInformation, Prompt, SecTimeOut, nil, dsOk, boInformation );
end;

procedure ExpireInformFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireInform( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireWarn( SecTimeOut: Cardinal; const Prompt: string );
begin
  ExpireShowDialog( sWarning, Prompt, SecTimeOut, nil, dsOk, boExclamation01 );
end;

procedure ExpireWarnFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireWarn( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireShowError( SecTimeOut: Cardinal; const Prompt: string );
begin
	ExpireShowDialog( sError, Prompt, SecTimeOut, nil, dsOk, boHardBug );
end;

procedure ExpireShowErrorFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireShowError( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireInformOnTop( SecTimeOut: Cardinal; const Prompt: string );
begin
	ExpireShowDialogOnTop( sInformation, Prompt, SecTimeOut, nil, dsOk, boInformation );
end;

procedure ExpireInformOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireInformOnTop( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireWarnOnTop( SecTimeOut: Cardinal; const Prompt: string );
begin
	ExpireShowDialogOnTop( sWarning, Prompt, SecTimeOut, nil, dsOk, boExclamation01 );
end;

procedure ExpireWarnOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireWarnOnTop( SecTimeOut, Format( Prompt, Args ) );
end;

procedure ExpireShowErrorOnTop( SecTimeOut: Cardinal; const Prompt: string );
begin
	ExpireShowDialogOnTop( sError, Prompt, SecTimeOut, nil, dsOk, boHardBug );
end;

procedure ExpireShowErrorOnTopFmt( SecTimeOut: Cardinal; const Prompt: string;
	const Args: array of const );
begin
	ExpireShowErrorOnTop( SecTimeOut, Format( Prompt, Args ) );
end;

function ShowDialog( const Title, Message: string; Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
begin
	if CheckPointer( @ShowDialogProc ) then
		Result := ShowDialogProc( Title, Message, Bitmap, Style, BitmapOption )
	else
		Result := InternalShowDialog( Title, Message, 0, Bitmap, Style, BitmapOption );
end;

function ShowDialogFmt( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const ): TModalResult;
begin
	Result := ShowDialog( Title, Format( Message, Args ), Bitmap, Style, BitmapOption );
end;

procedure ShowDialogOnTop( const Title, Message: string; Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption );
begin
	if CheckPointer( @ShowDialogOnTopProc ) then
		ShowDialogOnTopProc( Title, Message, Bitmap, Style, BitmapOption )
	else
		InternalShowDialogOnTop( Title, Message, 0, Bitmap, Style, BitmapOption );
end;

procedure ShowDialogOnTopFmt( const Title, Message: string;	Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption; const Args: array of const );
begin
	ShowDialogOnTop( Title, Format( Message, Args ), Bitmap, Style, BitmapOption );
end;

function ExpireShowDialog( const Title, Message: string; SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
begin
	if CheckPointer( @ExpireShowDialogProc ) then
		Result := ExpireShowDialogProc( Title, Message, Integer( SecTimeOut ), Bitmap, Style, BitmapOption )
	else
		Result := InternalShowDialog( Title, Message, Integer( SecTimeOut ), Bitmap, Style, BitmapOption );
end;

function ExpireShowDialogFmt( const Title, Message: string;	SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const ): TModalResult;
begin
	Result := ExpireShowDialog( Title, Format( Message, Args ), SecTimeOut, Bitmap, Style, BitmapOption );
end;

procedure ExpireShowDialogOnTop( const Title, Message: string; SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption );
begin
	if CheckPointer( @ExpireShowDialogOnTopProc ) then
		ExpireShowDialogOnTopProc( Title, Message, SecTimeOut, Bitmap, Style, BitmapOption )
	else
		InternalShowDialogOnTop( Title, Message, Integer( SecTimeOut ), Bitmap, Style, BitmapOption );
end;

procedure ExpireShowDialogOnTopFmt( const Title, Message: string;	SecTimeOut: Cardinal;
	Bitmap: TBitmap; Style: TKDialogStyle;	BitmapOption: TKBitmapOption;
	const Args: array of const );
begin
	ExpireShowDialogOnTop( Title, Format( Message, Args ), SecTimeOut, Bitmap, Style, BitmapOption );
end;

function InputDialog( const Title, Prompt, EditMask: string;
	var Text: string ): Boolean;
begin
	if CheckPointer( @InputDialogProc ) then
		Result := InputDialogProc( Title, Prompt, EditMask, Text )
	else
		Result := InternalInputDialog( Title, Prompt, EditMask, Text );
end;

function PasswordDialog( const Title, Prompt, EditMask: string; PasswordChar: Char;
	var Text: string ): Boolean;
begin
	if CheckPointer( @PasswordDialogProc ) then
		Result := PasswordDialogProc( Title, Prompt, EditMask, PasswordChar, Text )
	else
		Result := InternalPasswordDialog( Title, Prompt, EditMask, PasswordChar, Text );
end;

function InputDialogBox( const Title, Prompt, EditMask, Default: string ): string;
begin
  Result := Default;
  InputDialog( Title, Prompt, EditMask, Result );  
end;  

function InputListDialog( const Title, Prompt: string; DefaultIndex: Integer;
	List: TStrings ): Integer;
begin
	if CheckStrings( List ) then
		if CheckPointer( @InputListDialogProc ) then
			Result := InputListDialogProc( Title, Prompt, DefaultIndex, List )
		else
			Result := InternalInputListDialog( Title, Prompt, DefaultIndex, List )
	else
	  Result := -1;		
end;

function InputListDialogEx( const Title, Prompt: string; DefaultIndex: Integer;
	AcceptUserInput: Boolean; List: TStrings ): string;
begin
	if CheckStrings( List ) then
		if CheckPointer( @InputListDialogExProc ) then
			Result := InputListDialogExProc( Title, Prompt, DefaultIndex, AcceptUserInput, List )
		else
			Result := InternalInputListDialogEx( Title, Prompt, DefaultIndex, AcceptUserInput, List )
	else
		Result := '';
end;

function InputCheckListDialog( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits ): Boolean;
begin
	Result := False;
	if CheckPointer( @InputCheckListDialogFunc ) then
		Result := InputCheckListDialogFunc( Caption, BackColor, Sorted, Source, ResultList );
end;

function InputCheckListDialogFmt( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits; const Args: array of const ): Boolean;
begin
	Result := InputCheckListDialog( Format( Caption, Args ), BackColor, Sorted, Source, ResultList );
end;

function ProgressDialog( const Title, Message: string; StatusAlignment: TAlignment;
	AllowCancel, ProgressVisible: Boolean; Callback: TKProgressDialogCallBack;
	ExecutorProc: TKDialogExecuteProc; Data: Pointer ): Boolean;
begin
	if CheckReference( @CallBack ) then
		if CheckPointer( @ProgressDialogProc ) then
			Result := ProgressDialogProc( Title, Message, StatusAlignment, AllowCancel,
				ProgressVisible, Callback, ExecutorProc, Data )
		else
			Result := InternalProgressDialog( Title, Message, StatusAlignment, AllowCancel,
				ProgressVisible, Callback, ExecutorProc, Data )
	else
	  Result := False;			
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Information Dialogs --------------------------
--------------------------------------------------------------------------------
}

function SelectScreenDataModule( IsSorted: Boolean ): TDataModule;
var
	i: Integer;
	ss: TStrings;
begin
	Result := nil;
	ss := TStringList.Create;
	try
		for i := 0 to Screen.DataModuleCount - 1 do
			ss.AddObject( Screen.DataModules[i].Name, Screen.DataModules[i] );
		TStringList( ss ).Sorted := IsSorted;
		i := InputListDialog( sSelDataModule, sSelNamePath, 0, ss );
		if ( i <> -1 ) then
			Result := ( ss.Objects[i] as TDataModule );
	finally
		ss.Free;
	end;
end;

function SelectScreenForm( FilterClass: TCustomFormClass; IsSorted: Boolean ): TCustomForm;
var
	i: Integer;
	ss: TStrings;
begin
	if ( not CheckClass( FilterClass ) ) then
		FilterClass := TCustomForm;
	Result := nil;
	ss := TStringList.Create;
	try
		for i := 0 to Screen.CustomFormCount - 1 do
			if CheckObjectClass( Screen.CustomForms[i], FilterClass ) then
				ss.AddObject( Screen.CustomForms[i].Name, Screen.CustomForms[i] );
		TStringList( ss ).Sorted := IsSorted;
		i := InputListDialog( sSelForm, sSelFormName, 0, ss );
		if ( i <> -1 ) then
			Result := ( ss.Objects[i] as TCustomForm );
	finally
		ss.Free;
	end;
end;

function GetControlPath( Control: TControl ): string;
begin
	ForceObject( Control );
	Result := Control.Name;
	while CheckObject( Control.Parent ) do
	begin
		Result := Control.Parent.Name + CH_DOTMARK + Result;
		Control := Control.Parent;
	end;
end;

function GetComponentPath( Component: TComponent ): string;
begin
	ForceObject( Component );
	Result := Component.Name;
	while CheckObject( Component.Owner ) do
	begin
		Result := Component.Owner.Name + CH_DOTMARK + Result;
		Component := Component.Owner;
	end;
end;

function SelectControl( Root: TControl; FilterClass: TControlClass;
	IsRecursive, IsSorted, IncludeRoot: Boolean ): TControl;

	procedure AddItems( WinControl: TWinControl; Strings: TStrings );
	var
		i: Integer;
	begin
		for i := 0 to WinControl.ControlCount - 1 do
			if CheckObjectClass( WinControl.Controls[i], FilterClass ) then
			begin
				Strings.AddObject( GetControlPath( WinControl.Controls[i] ), WinControl.Controls[i] );
				if ( IsRecursive and CheckObjectClass( WinControl.Controls[i], TWinControl ) ) then
					AddItems( ( WinControl.Controls[i] as TWinControl ), Strings );
			end;
	end;

var
	i: Integer;
	ss: TStrings;
begin
	ForceObject( Root );
	if ( not CheckClass( FilterClass ) ) then
		FilterClass := TControl;
	Result := nil;
	ss := TStringList.Create;
	try
		if ( IncludeRoot and CheckObjectClass( Root, FilterClass ) ) then
			ss.AddObject( GetControlPath( Root ), Root );
		if CheckObjectClass( Root, TWinControl ) then
   		AddItems( ( Root as TWinControl ), ss );
		TStringList( ss ).Sorted := IsSorted;
		i := InputListDialog( sSelControl, sSelNamePath, 0, ss );
		if ( i <> -1 ) then
			Result := ( ss.Objects[i] as TControl );
	finally
		ss.Free;
	end;
end;

function SelectComponent( Root: TComponent; FilterClass: TComponentClass;
	IsRecursive, IsSorted, IncludeRoot: Boolean ): TComponent;

	procedure AddItems( Comp: TComponent; Strings: TStrings );
	var
		i: Integer;
	begin
		for i := 0 to Comp.ComponentCount - 1 do
			if CheckObjectClass( Comp.Components[i], FilterClass ) then
			begin
				Strings.AddObject( GetComponentPath( Comp.Components[i] ), Comp.Components[i] );
				if IsRecursive then
					AddItems( Comp.Components[i], Strings );
			end;
	end;

var
	i: Integer;
	ss: TStrings;
begin
	ForceObject( Root );
	if ( not CheckClass( FilterClass ) ) then
		FilterClass := TComponent;
	Result := nil;
	ss := TStringList.Create;
	try
		if ( IncludeRoot and CheckObjectClass( Root, FilterClass ) ) then
			ss.AddObject( GetComponentPath( Root ), Root );
		AddItems( Root, ss );
		TStringList( ss ).Sorted := IsSorted;
		i := InputListDialog( sSelComp, sSelNamePath, 0, ss );
		if ( i <> -1 ) then
			Result := ( ss.Objects[i] as TComponent );
	finally
		ss.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic File Dialog Routines -------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TKSelectDialog = ( sdLoadFile, sdLoadPicture, sdSaveFile, sdSavePicture );

function InternalLoadSaveFile( AFilter: string; const ATitle, AInitDir, ADefExt: string;
	SelectDialog: TKSelectDialog; AOptions: TOpenOptions; AFileNames: TStrings ): TFileName;
const
	DLG_CLASS: array[TKSelectDialog] of TComponentClass =
	( TOpenDialog, TOpenPictureDialog, TSaveDialog, TSavePictureDialog );
begin
	Result := '';
	if ( SelectDialog in [sdLoadPicture, sdSavePicture] ) and ( not CheckTrimStr( AFilter ) ) then
    AFilter := sImageFilter;
	if ( ofAllowMultiSelect in AOptions ) then
		ForceObject( AFileNames );
	with ( DLG_CLASS[SelectDialog].Create( nil ) as TOpenDialog ) do
		try
			DefaultExt := ADefExt;
			Filter     := AFilter;
			Title      := ATitle;
			InitialDir := AInitDir;
			Options    := AOptions;
			if Execute then
			begin
				Result := FileName;
				if ( ofAllowMultiSelect in AOptions ) then
					AFileNames.Assign( Files );
			end;
		finally
			Free;
		end;
end;

{---------------------------- Public Implementation ----------------------------}

function SelectLoadFile( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdLoadFile,
		DEFAULT_SELECT_LOADSAVE_FILES, nil );
end;

function SelectLoadFiles( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdLoadFile,
		Options, FileNames );
end;

function SelectLoadPicture( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdLoadPicture,
		DEFAULT_SELECT_LOADSAVE_FILES, nil );
end;

function SelectLoadPictures( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdLoadPicture,
		Options, FileNames );
end;

function SelectSaveFile( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdSaveFile,
		DEFAULT_SELECT_LOADSAVE_FILES,  nil );
end;

function SelectSaveFiles( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdSaveFile,
		Options, FileNames );
end;

function SelectSavePicture( const AFilter, ATitle, AInitDir, ADefExt: string ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdSavePicture,
		DEFAULT_SELECT_LOADSAVE_FILES,  nil );
end;

function SelectSavePictures( const AFilter, ATitle, AInitDir, ADefExt: string;
	Options: TOpenOptions; FileNames: TStrings ): TFileName;
begin
	Result := InternalLoadSaveFile( AFilter, ATitle, AInitDir, ADefExt, sdSavePicture,
		Options, FileNames );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic RTTI Routines -----------------------------
--------------------------------------------------------------------------------
}

{ EnumPubProps }

function EnumPubProps( Source: TObject; TypeKinds: TTypeKinds; Data: Pointer;
	CallBack: TKEnumPubPropsFunc ): Integer;
var
	iCount: Integer;
	PropList: PPropList;
begin
	ForceObject( Source );
	ForceReference( @CallBack );
	Result := 0;
	iCount := GetPropList( Source.ClassInfo, TypeKinds, nil );
	if ( iCount > 0 ) then
	begin
		GetMem( PropList, SizeOf( PPropInfo ) * iCount );
		try
			GetPropList( Source.ClassInfo, TypeKinds, PropList );
			while ( Result < iCount ) do
			begin
				if ( not CallBack( Source, PropList^[Result], Data ) ) then
					Exit;
				Inc( Result );
			end;
		finally
			FreeMem( PropList, SizeOf( PPropInfo ) * iCount );
		end;
	end;
end;

{ SetPropertyDefaults }

function SetPropDefaultsProc( Source: TObject; ppi: PPropInfo; Data: Pointer ): Boolean;
const
	NO_DEFAULT = Integer($80000000);
begin
	Result := True;
	if ( ppi^.Default <> NO_DEFAULT ) then
		SetOrdProp( Source, ppi, ppi^.Default );
end;

procedure SetPropertyDefaults( Source: TObject );
const
	tkPropsWithDefault = [tkInteger, tkChar, tkSet, tkEnumeration];
begin
	EnumPubProps( Source, tkPropsWithDefault, nil, SetPropDefaultsProc );
end;

{ AssignPubProps }

type

	PKAssignPubPropData = ^TKAssignPubPropData;
	TKAssignPubPropData = record
		Target: TObject;
		pData: Pointer;
		ErrorCallBack: TKAssignPubPropsErrorFunc;
	end;

function AssignPubPropsProc( Source: TObject; SourcePPI: PPropInfo; Data: Pointer ): Boolean;
var
  ppi: PPropInfo;
begin
  Result := True;
	with PKAssignPubPropData( Data )^ do
	begin
		ppi := GetPropInfo( Target.ClassInfo, SourcePPI^.Name );
		if CheckPointer( ppi )	and
			( ppi^.PropType^^.Kind = SourcePPI^.PropType^^.Kind ) then
			try
				case SourcePPI^.PropType^^.Kind of
					tkInteger,
					tkChar,
					tkEnumeration,
					tkClass,
					tkSet,
					tkWChar    : SetOrdProp( Target, ppi, GetOrdProp( Source, SourcePPI ) );

					tkLString,
					tkWString,
					tkString   : SetStrProp( Target, ppi, GetStrProp( Source, SourcePPI ) );

					tkFloat    : SetFloatProp( Target, ppi, GetFloatProp( Source, SourcePPI ) );
					tkMethod   : SetMethodProp( Target, ppi, GetMethodProp( Source, SourcePPI ) );
					tkVariant  : SetVariantProp( Target, ppi, GetVariantProp( Source, SourcePPI ) );
				end;
			except
				on E: Exception do
					if ( CheckReference( @ErrorCallBack ) and ( not ErrorCallBack( Source, Target,
					  SourcePPI, ppi, E, pData ) ) ) then
						raise; { raise only if the user said that we can raise! }
					else
						{ ignore all exceptions... };
			end;
	end;
end;

function AssignPubPropsEx( Source, Target: TObject; Data: Pointer;
	ErrorCallBack: TKAssignPubPropsErrorFunc ): Integer;
var
	appd: TKAssignPubPropData;
begin
	ForceObject( Target ); { source will be tested in EnumPubProps }
	appd.Target := Target;
	appd.pData := Data;
	appd.ErrorCallBack := ErrorCallBack;
	Result := EnumPubProps( Source, tkAny, @appd, AssignPubPropsProc );
end;

function AssignPubProps( Source, Target: TObject ): Integer;
begin
	Result := AssignPubPropsEx( Source, Target, nil, nil );
end;

function EnumName( Source: Cardinal; Info: PTypeInfo ): string;
var
	Data: PTypeData;
begin
  ForcePointer( Info );
	if ( Info^.Kind <> tkEnumeration ) then
		Result := ''
	else
	begin
		Data := GetTypeData( Info );
		if ValueBetween( Source, Data^.MinValue, Data^.MaxValue, true ) then
			Result := GetEnumName( Info, Source )
		else
			Result := Format( ENUM_NOTFOUND_MASK, [Info^.Name, Source] );
	end;
end;

procedure EnumNames( sl: TStrings; Info: PTypeInfo );
var
  i: Integer;
	Data: PTypeData;
begin
	ForcePointer( Info );
	ForceObject( sl );
	sl.BeginUpdate;
	try
		sl.Clear;
		if ( Info^.Kind = tkEnumeration ) then
		begin
			Data := GetTypeData( Info );
			for i := Data^.MinValue to Data^.MaxValue do
				sl.Add( GetEnumName( Info, i ) );
		end;
	finally
		sl.EndUpdate;
	end;
end;

function SetToString( const Source; Info: PTypeInfo ): string;
var
	i: Integer;
	MaskValue: LongInt;
	Data: PTypeData;
	CompInfo: PTypeInfo;
	CompData: PTypeData;
begin
	if ( Info^.Kind <> tkSet ) then
		Result := ''
	else
	begin
		Data := GetTypeData( Info );
		CompInfo := Data^.CompType^;
		CompData := GetTypeData( CompInfo );
		MaskValue := -1;
		case Data.OrdType of
			otSByte, otUByte: MaskValue := Byte( Source );
			otSWord, otUWord: MaskValue := Word( Source );
			otSLong					: MaskValue := LongInt( Source );
		end;
		Result := CH_BRACES_OPEN;
		for i := CompData^.MinValue to CompData^.MaxValue do
		begin
			if ( Boolean( MaskValue and 1 ) ) then
				Result := Result + EnumName( i, CompInfo ) + CH_COMMA + CH_SPACE;
			MaskValue := MaskValue shr 1;
		end;
		if ( Length( Result ) > 1 ) and ( Result[Length( Result )-1] = CH_COMMA ) then
			Delete( Result, Length( Result )-1, 2 );
		Result := Result + CH_BRACES_CLOSE;
	end;
end;

procedure SetPropInfo( Source: TObject; ppi: PPropInfo; const PropValue );
begin
{
	In SetStrProp we must use PChar( @PropValue ) and not string( Pointer( @PropValue ) )
	because string reference couting problems...

	Take careful in the PropValue assignment! For example, if you pass a extended
	variable to set a ordinal property, you will not get the expected results
	because the float point memory mapping. Longint values are fetched linearly in
	memory. The Extended type has internal type representation and incompatibility
	from integer!
	}
	ForceObject( Source );
	ForcePointer( ppi );
	case ppi^.PropType^^.Kind of
		tkInteger,
		tkChar,
		tkEnumeration,
		tkClass,
		tkSet,
		tkWChar    : SetOrdProp( Source, ppi, LongInt( PropValue ) );

		tkLString,                                
		tkWString,
		tkString   : SetStrProp( Source, ppi, PChar( @PropValue ) );

		tkFloat    : SetFloatProp( Source, ppi, Extended( PropValue ) );
		tkMethod   : SetMethodProp( Source, ppi, TMethod( PropValue ) );
		tkVariant  : SetVariantProp( Source, ppi, Variant( PropValue ) );
	else
{ tkUnknown, tkArray, tkRecord, tkInterface }
		RaiseExceptionFmt( EKSYUtils, sErrInvPropTypeKind, [ Source.ClassName,
			EnumName( Integer( ppi^.PropType^^.Kind ), TypeInfo( TTypeKind ) ), ppi^.Name,
			LongInt( PropValue )] );
	end;
end;

function SetPubProp( Source: TObject; const PropName: string; const PropValue;
	ProcessChildren: Boolean ): Boolean;
var
	ppi: PPropInfo;
	i  : Integer;
begin
	ForceTrim( [Source, PropName ] );
	ppi := GetPropInfo( Source.ClassInfo, PropName );
	Result := CheckPointer( ppi );
	if Result then
	  SetPropInfo( Source, ppi, PropValue );
	if ( Source is TComponent ) and ( ProcessChildren ) then
    with ( Source as TComponent ) do
{$BOOLEVAL ON}
			for i := 0 to ComponentCount - 1 do
				Result := Result or SetPubProp( Components[i], PropName, PropValue,
					ProcessChildren );
{$BOOLEVAL OFF}
end;

function SetPubProps( Source: TObject; PropNameList: TStrings;
	const PropValue; ProcessChildren: Boolean ): Boolean;
var
	i: Integer;
begin
	ForceObject( PropNameList );
	ForceReference( PropValue );
	Result := true;
	for i := 0 to PropNameList.Count - 1 do
		Result := Result and SetPubProp( Source, PropNameList.Strings[i], PropValue,
			ProcessChildren );
end;

function CheckDesigner( Source: TKDesigner ): Boolean;
begin
	Result := {$IFDEF DELPHI4}CheckInterface( Source ){$ELSE}
		CheckObject( Source ){$ENDIF};
end;

procedure ForceDesigner( Source: TKDesigner );
begin
	{$IFDEF DELPHI4}
		ForceInterface( Source );
	{$ELSE}
		ForceObject( Source );
	{$ENDIF}	  
end;

function CheckDesignerClass( Source: TKDesigner{$IFNDEF DELPHI4}; AClass: TClass {$ENDIF} ): Boolean;
begin
	Result := {$IFDEF DELPHI4}CheckInterface( Source ){$ELSE}
		CheckObjectClass( Source, AClass ){$ENDIF};
end;

procedure MarkDesigner( Source: TComponent );
begin
	while CheckObject( Source ) do
		if CheckObjectClass( Source, TCustomForm ) then
		begin
			if CheckDesigner( TCustomForm( Source ).Designer ) then
				TCustomForm( Source ).Designer.Modified;
			Exit;
		end
		else
			Source := Source.Owner;
end;

type

	TCollectionSHack = class( TPersistent )
  private
  {$HINTS OFF}
    FItemClass: TCollectionItemClass; { D3, D4, D5 copmpliant Hack }
  {$HINTS ON}
    FItems: TList;

  end;
  
function GetPersistentFromPath( p: TPersistent; const Path: string ): TPersistent;
var
  i: Integer;
	pi: PPropInfo;
begin
  Result := nil;
  if CheckTrim( [p, Path] ) then
  begin
    if ( not CheckStrContains( CH_BRACES_OPEN, Path ) ) then
    begin
      pi := TypInfo.GetPropInfo( p.ClassInfo, Path );
      if CheckPointer( pi ) and ( pi^.PropType^^.Kind = tkClass ) then
        Result := TPersistent( TypInfo.GetOrdProp( p, pi ) );
    end
    else if CheckStrContainsAll( [CH_BRACES_OPEN, CH_BRACES_CLOSE], Path ) then
    { If we have a Path with braces, eg Panels[0], we need to get the Panels.Items[i].
      We need to asume that this is a collection item property and its name is Items.
      Since Items isn't a published property (but a public streamable property) we
      have no choice. }
    begin
      pi := GetPropInfo( p.ClassInfo, Copy( Path, 1, Pos( CH_BRACES_OPEN, Path ) - 1 ) );
      if CheckPointer( pi ) and ( pi^.PropType^^.Kind = tkClass ) then
        p := ( TObject( GetOrdProp( p, pi ) ) as TPersistent );
      if CheckObjectClass( p, TCollection ) then
      begin
        i := StrToIntDef( Copy( Path, Pos( CH_BRACES_OPEN, Path ) + 2, Pos( CH_BRACES_CLOSE, Path ) -
          1 - Pos( CH_BRACES_OPEN, Path ) - 2 ), 0 );
        { Very HARGHHH Hack! }
        if ( TCollectionSHack( p ).FItems.Count > i ) then
          Result := ( TObject( TCollectionSHack( p ).FItems.Items[i] ) as TCollectionItem );          
      end;
    end;
  end;
end;

function GetCompPathValue( cOwner: TComponent; const Path: string; TypeKinds: TTypeKinds ): TKCompPathValue;
const
  NOTOWNED_NOTFOUND = 0;
  NOTOWNED_FOUND    = 1;
  OWNED_NOTFOUND    = 2;
  OWNED_FOUND       = 3;

  SEARCH_FILTER: array[Boolean, Boolean] of Byte =
    ( ( NOTOWNED_NOTFOUND, NOTOWNED_FOUND ), ( OWNED_NOTFOUND, OWNED_FOUND ) );
    
var
	i: Integer;
	sl: TStrings;
	pi: PPropInfo;
	p1, p2: TPersistent;
  isOwned: Boolean;
begin
	ForceTrimStr( Path );
	ZeroMemory( @Result, SizeOf( TKCompPathValue ) );
  isOwned := CheckObject( cOwner );
  sl := TStringList.Create;
  try
    sl.Text := StringReplace( Path, CH_DOTMARK, CH_CRLF, krfAll );
    ForceStrings( sl );
    p2 := nil;
    if ( sl.Count > 1 ) then
    begin
      i := 0;
      p1 := cOwner;
      while ( i <= sl.Count - 2 ) do
			begin
				if ( not CheckObject( p2 ) ) then
				begin
          if CheckObject( p1 ) then
            if CheckObjectClass( p1, TComponent ) then
            	p2 := ( p1 as TComponent ).FindComponent( sl[i] )
            else
              p2 := GetPersistentFromPath( p1, sl[i] );
          {
          SEARCH_FILTER[isOwned, CheckObject( p2 )]
            False, False -> Search for global component: If (<> nil), will go to False,True;
                            else GetPersistentFromPath; Inc(i)
            False, True  -> Just Inc(i), it will go to get new p1 or raise an exeption
            True , False -> We are looking for a persistent or for a not founded component
            True , True  -> Just Inc(i), it will go to get new p1 or raise an exeption
          }
          case SEARCH_FILTER[isOwned, CheckObject( p2 )] of
            NOTOWNED_NOTFOUND:
            begin
              p2 := uksyUtils.FindGlobalComponent( sl[i] );
              if ( not CheckObject( p2 ) ) then
                if CheckObject( p1 ) then
                  p2 := GetPersistentFromPath( p1, sl[i] )
                else
                  RaiseExceptionFmt( EKSYUtils, sErrUnknownProperty, [Path] );
            end;
            OWNED_NOTFOUND   :
            begin
              if CheckObject( p1 ) then
              begin
                p2 := GetPersistentFromPath( p1, sl[i] );
                if ( not CheckObject( p2 ) ) then
                  RaiseExceptionFmt( EKSYUtils, sErrInvPropPath, [Path] );
              end
              else
                RaiseExceptionFmt( EKSYUtils, sErrUnknownComponent, [Path] );
            end;
            NOTOWNED_FOUND,
            OWNED_FOUND      : ;
          end;
          Inc( i );
				end
				else if CheckObjectClass( p2, TComponent ) then
				begin
					p1 := ( p2 as TComponent );
					p2 := nil;
				end
				else
					RaiseExceptionFmt( EKSYUtils, sErrUnknownProperty, [Path] );
			end;
		end
		else if isOwned then
			p2 := cOwner
    else
      RaiseExceptionFmt( EKSYUtils, sErrInvPropPath, [Path] );
    if ( not CheckObject( p2 ) ) then
      RaiseExceptionFmt( EKSYUtils, sErrInvPropPath, [Path] );
		pi := TypInfo.GetPropInfo( p2.ClassInfo, sl[sl.Count - 1] );
		if ( not ( CheckPointer( pi ) and ( pi^.PropType^^.Kind in TypeKinds ) ) ) then
			RaiseExceptionFmt( EKSYUtils, sErrUnknownProperty, [Path] );
		Result.Obj := p2;
		Result.PropInfo := pi;
	finally
		sl.Free;
	end;
end;

function GetCompPathOrdValue( c: TComponent; const Path: string ): Integer;
var
	cpv: TKCompPathValue;
begin
	cpv := GetCompPathValue( c, Path, [tkInteger, tkChar, tkEnumeration, tkClass,
	  tkSet, tkWChar, tkInterface] );
	Result := GetOrdProp( cpv.Obj, cpv.PropInfo );
end;

function GetCompPathStrValue( c: TComponent; const Path: string ): string;
var
	cpv: TKCompPathValue;
begin
	cpv := GetCompPathValue( c, Path, [tkString, tkLString, tkWString] );
	Result := GetStrProp( cpv.Obj, cpv.PropInfo );
end;

function GetCompPathFloatValue( c: TComponent; const Path: string ): Extended;
var
	cpv: TKCompPathValue;
begin
	cpv := GetCompPathValue( c, Path, [tkFloat] );
	Result := GetFloatProp( cpv.Obj, cpv.PropInfo );
end;

function GetCompPathMethodValue( c: TComponent; const Path: string ): TMethod;
var
	cpv: TKCompPathValue;
begin
	cpv := GetCompPathValue( c, Path, [tkMethod] );
	Result := GetMethodProp( cpv.Obj, cpv.PropInfo );
end;

function GetCompPathVariantValue( c: TComponent; const Path: string ): Variant;
var
	cpv: TKCompPathValue;
begin
	cpv := GetCompPathValue( c, Path, [tkVariant] );
	Result := GetVariantProp( cpv.Obj, cpv.PropInfo );
end;

{
--------------------------------------------------------------------------------
------------------------------- Generic Set Routines ---------------------------
--------------------------------------------------------------------------------
}

function SetToBitMask( const Source; Size: TKSetSize ): LongInt;
begin
	Result := 0;
	case Size of
		SizeOf( Byte )   : Result := Byte( Source );
		SizeOf( Word )   : Result := Word( Source );
		SizeOf( LongInt ): Result := LongInt( Source );
	end;
end;

procedure BitMaskToSet( var Source; BitMask: LongInt; Size: TKSetSize );
begin
	case Size of
		SizeOf( Byte )   : Byte( Source ) := Byte( BitMask );
		SizeOf( Word )   : Word( Source ) := Word( BitMask );
		SizeOf( LongInt ): LongInt( Source ) := LongInt( BitMask );
	end;
end;

function SetsEqual( const Set1, Set2; Size1, Size2: TKSetSize ): Boolean;
begin
	Result := ( SetToBitMask( Set1, Size1 ) = SetToBitMask( Set2, Size2 ) );
end;

function SetsIntersect( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;
begin
	Result := ( ( SetToBitMask( BaseSet, BaseSize ) and SetToBitMask( CompSet, CompSize ) ) > 0 );
end;

function SetContains( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;
begin
	Result := ( ( SetToBitMask( BaseSet, BaseSize ) and SetToBitMask( CompSet, CompSize ) ) =
		SetToBitMask( CompSet, CompSize ) );
end;

function SetContained( const BaseSet, CompSet; BaseSize, CompSize: TKSetSize ): Boolean;
begin
	Result := SetContains( CompSet, BaseSet, CompSize, BaseSize );
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Color Routines -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ------------------------- }

const
	ColorMap: array[0..MAXSIZE_KNOWHOWCOLORS - 1] of TIdentMapEntry =
	(
		( Value: clOrange;       Name: 'clOrange' 		 ),
		( Value: clMoneyGreen;   Name: 'clMoneyGreen'  ),
		( Value: clLightGreen;   Name: 'clLightGreen'  ),
		( Value: clLightYellow;  Name: 'clLightYellow' ),
		( Value: clLightRed;     Name: 'clLightRed' 	 ),
		( Value: clLightBlue;    Name: 'clLightBlue' 	 ),
		( Value: clDeepGreen;    Name: 'clDeepGreen'   ),
		( Value: clDeepYellow;   Name: 'clDeepYellow'  ),
		( Value: clDeepRed;      Name: 'clDeepRed'   	 ),
		( Value: clDeepBlue;     Name: 'clDeepBlue' 	 )
	);

{---------------------------- Public Implementation -------------------------- }

function KColorToIdent( Color: Integer; var Ident: string ): Boolean;
begin
	Result := IntToIdent( Color, Ident, ColorMap );
end;

function KIdentToColor( const Ident: string; var Color: Integer ): Boolean;
begin
	Result := IdentToInt( Ident, Color, ColorMap );
end;

function KColorToString( Color: TColor ): string;
begin
	if ( not KColorToIdent( Color, Result ) ) then
		Result := ColorToString( Color );
end;

function KStringToColor( const Color: string ): TColor;
begin
	if ( not KIdentToColor( Color, Integer( Result ) ) ) then
		Result := StringToColor( Color );
end;

procedure KGetColorValues( Proc: TGetStrProc );
var
	i: Integer;
begin
	for i := 0 to MAXSIZE_KNOWHOWCOLORS - 1 do
		Proc( ColorMap[i].Name );
end;

{
--------------------------------------------------------------------------------
--------------------- Delphi Registry Access Routines---------------------------
--------------------------------------------------------------------------------
}

function GetDelphiRegInfo( DelphiRegInfo: TKDelphiRegInfo ): string;
var
	Reg: TRegIniFile;
	sl: TStrings;
begin
	Reg := TRegIniFile.Create( sDelphiBaseInfoRegKey );
	try
		case DelphiRegInfo of
			driLibPath :
				Result := Reg.ReadString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_SEARCHPATH_SECTION, '' );
			driDPLDir  :
				Result := Reg.ReadString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_DPLDIR_SECTION, '' );
			driDCPDir  :
				Result := Reg.ReadString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_DCPDIR_SECTION, '' );
			driInstDPL :
			begin
				sl := TStringList.Create;
				try
					Reg.ReadSection( DELPHI_REG_KNOWNPACKAGES_SECTION, sl );
					Result := sl.Text;
				finally
					sl.Free;
				end;
			end;
		else
			Result := '';
		end;
	finally
		Reg.Free;
	end;
end;

function GetDelphiLibraryPath: string;
begin
	Result := GetDelphiRegInfo( driLibPath );
end;

function GetDelphiDPLOutPutDir: string;
begin
	Result := GetDelphiRegInfo( driDPLDir );
end;

function GetDelphiDCPOutPutDir: string;
begin
	Result := GetDelphiRegInfo( driDCPDir );
end;

procedure GetDelphiInstaledPackages( sl: TStrings );
begin
	ForceObject( sl );
	sl.BeginUpdate;
	try
		sl.Clear;
		sl.Text := GetDelphiRegInfo( driInstDPL );
	finally
		sl.EndUpdate;
	end;
end;

procedure SetDelphiRegInfo( DelphiRegInfo: TKDelphiRegInfo; const Value: string );
var
	Reg: TRegIniFile;
begin
  ForceTrimStr( Value );
	Reg := TRegIniFile.Create( sDelphiBaseInfoRegKey );
	try
		case DelphiRegInfo of
			driLibPath :
				Reg.WriteString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_SEARCHPATH_SECTION, Value );
			driDPLDir  :
				Reg.WriteString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_DPLDIR_SECTION, Value );
			driDCPDir  :
				Reg.WriteString( DELPHI_REG_LIBRARY_SECTION, DELPHI_REG_DCPDIR_SECTION, Value );
			driInstDPL :
				if ( CheckStrContains( CH_EQUAL_TOKEN, Value ) ) then
					Reg.WriteString( DELPHI_REG_KNOWNPACKAGES_SECTION, Copy( Value, 1,
						Pos( CH_EQUAL_TOKEN, Value )-1 ), Copy( Value, Pos( CH_EQUAL_TOKEN, Value ) + 1,
						MaxInt ) );
		end;
	finally
		Reg.Free;
	end;
end;

procedure SetDelphiDPLOutPutDir( const Value: string );
begin
	ForceTrimStr( Value );
	SetDelphiRegInfo( driDPLDir, Value );
end;

procedure SetDelphiDCPOutPutDir( const Value: string );
begin
	ForceTrimStr( Value );
	SetDelphiRegInfo( driDCPDir, Value );
end;

procedure SetDelphiLibraryPath( const Value: string );
begin
	ForceTrimStr( Value );
	if CurrentDelphi32Running then
		RaiseException( EKSYUtils, sErrSetLibPath );
	SetDelphiRegInfo( driLibPath, Value );
end;

procedure AppendDelphiLibraryPath( const Value: string );
begin
  ForceTrimStr( Value );
	SetDelphiLibraryPath( GetDelphiLibraryPath + CH_LIST_TOKEN + Value );
end;

procedure InstallDelphiPackage( const Value, Info: string );
begin
	ForceTrimStrs( [Value, Info] );
	if CurrentDelphi32Running then
		RaiseExceptionFmt( EKSYUtils, sErrInstallPackage, [ExtractFileName( Value )] );
	SetDelphiRegInfo( driInstDPL, Value + CH_EQUAL_TOKEN + Info );
end;

const
	VERSION_PATTERN: array[TKDelphi32Version] of string[3] = ( '2.0', '3.0', '4.0', '5.0' );

function DelphiBaseRegKey( Version: TKDelphi32Version ): string;
begin
	Result := Format( DELPHI_BASE_REGKEY, [VERSION_PATTERN[Version]] )
end;

function CurrentDelphiBaseRegKey: string;
begin
	Result := DelphiBaseRegKey( {$IFDEF DELPHI4} dv400 {$ELSE} {$IFDEF DELPHI3}
		dv300 {$ELSE} {$IFDEF DELPHI2} dv200 {$ENDIF} {$ENDIF} {$ENDIF} );
end;

function DelphiRootDir( Version: TKDelphi32Version ): string;
var
	Reg: TRegistry;
begin
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;
		if ( not Reg.OpenKey( DelphiBaseRegKey( Version ), false ) ) then
			RaiseExceptionFmt( EKSYUtils, sErrCannotOpenDelphiRegKey, [VERSION_PATTERN[Version]] );
		try
			Result := Reg.ReadString( DELPHI_REG_ROOTDIR_SECTION );
			ForceTrimStr( Result );
		finally
		  Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

function CurrentDelphiRootDir: string;
begin
	Result := DelphiRootDir( {$IFDEF DELPHI4} dv400 {$ELSE} {$IFDEF DELPHI3}
		dv300 {$ELSE} {$IFDEF DELPHI2} dv200 {$ENDIF} {$ENDIF} {$ENDIF} );
end;

function CurrentProjectPath: string;
begin
{ Dependent of the "Time" (Design/Run) get the correct path... }
	if CheckObject( ToolServices ) then
		Result := ExtractFilePath( ToolServices.GetProjectName )
	else
		Result := ApplicationPath;
end;

function CheckRegistryPath( RootKey: TKRegistryKey; const Path: string ): Boolean;
var
	rg : TRegistry;
begin
	rg := TRegistry.Create;
	try
		rg.RootKey := REGISTRY_KEYS[RootKey];
		Result := rg.OpenKey( RemoveLastPathSlash( Path ), False );
	finally
		rg.Free;
	end;
end;

procedure ForceRegistryPath( RootKey: TKRegistryKey; const Path: string );
begin
	if ( not CheckRegistryPath( RootKey, Path ) ) then
	  RaiseExceptionFmt( EKSYUtils, sErrInvRegPath, [REGISTRY_KEYS_NAMES[RootKey], Path] );
end;                              

function ReadFromRegistry( RootKey: TKRegistryKey; CanCreate: Boolean; const Path, Key: string ): string;
var
	rg : TRegistry;
begin
	Result := '';
	rg := TRegistry.Create;
	try
		rg.RootKey := REGISTRY_KEYS[RootKey];
		if rg.OpenKey( RemoveLastPathSlash( Path ), CanCreate ) then
			Result := rg.ReadString( Key );
	finally
		rg.Free;
	end;
end;

function WriteToRegistry( RootKey: TKRegistryKey; CanCreate: Boolean;
	const Path, Key, Value: string ): Boolean;
var
	rg : TRegistry;
begin
	rg := TRegistry.Create;
	try
		rg.RootKey := REGISTRY_KEYS[RootKey];
		Result := rg.OpenKey( RemoveLastPathSlash( Path ), CanCreate );
		if Result then
			rg.WriteString( Key, Value );
	finally
		rg.Free;
	end;
end;

function ReadRegistryStrings( RootKey: TKRegistryKey; CanCreate: Boolean; const RootPath: string;
	Keys: TStrings ): Boolean;
var
	s: string;
	i: Integer;
	rg : TRegistry;
	bOpen: Boolean;
begin
	ForceStrings( Keys );
	Keys.BeginUpdate;
	try
		rg := TRegistry.Create;
		try
			Result := True;
			rg.RootKey := REGISTRY_KEYS[RootKey];
			for i := 0 to Keys.Count - 1 do
			begin
				s := RemoveLastPathSlash( RootPath ) + CH_BACK_SLASH;
				if ( Keys.Names[i] <> CH_GRIDLING ) then
					s := s + RemoveFirstPathSlash( Keys.Names[i] );
				{ Try to open the last less one key under s
			
					eg. \Software\Test\Abc; Open \Software\Test and check value Abc

					To read the default value insert a key with the # as the name }	
				bOpen := rg.OpenKey( RemoveLastPathSlash( ExtractFilePath( s ) ), CanCreate );
				if bOpen then
				begin
					if ( Keys.Names[i] <> CH_GRIDLING ) then
					begin
						s := ExtractFileName( s );
						{ this will ensure the use of the negative for this function }
						bOpen := rg.ValueExists( s );
						if bOpen then
							s := rg.ReadString( s );
					end
					else
					begin
						{ for the default key, return false if CheckTrimStr return false }
						s := rg.ReadString( '' );
						bOpen := CheckStr( s );
					end;
					if bOpen then
						Keys[i] := ( Keys.Names[i] + CH_EQUAL_TOKEN + s )
					else
						Keys[i] := ( Keys.Names[i] + CH_EQUAL_TOKEN + '' );				  	
					rg.CloseKey;
				end;
				Result := ( Result and bOpen );
			end;
		finally
			rg.Free;
		end;
	finally
		Keys.EndUpdate;
	end;
end;

function WriteRegistryStrings( RootKey: TKRegistryKey; CanCreate: Boolean; const RootPath: string;
	Keys: TStrings ): Boolean;
var
	s: string;
	i: Integer;
	rg : TRegistry;
	bOpen: Boolean;
begin
	ForceStrings( Keys );
	Keys.BeginUpdate;
	try
		rg := TRegistry.Create;
		try
			Result := True;
			rg.RootKey := REGISTRY_KEYS[RootKey];
			AdjustStringsForValues( Keys );
			for i := 0 to Keys.Count - 1 do
			begin
				s := RemoveLastPathSlash( RootPath ) + CH_BACK_SLASH;
				if ( Keys.Names[i] <> CH_GRIDLING ) then
					s := s + RemoveFirstPathSlash( Keys.Names[i] );
				bOpen := rg.OpenKey( RemoveLastPathSlash( ExtractFilePath( s ) ), CanCreate );
				if bOpen then
				begin
					{ For the Default Value, ExtractFileName will return '' }
					rg.WriteString( ExtractFileName( s ), Keys.Values[Keys.Names[i]] );
					rg.CloseKey;
				end;
				Result := ( Result and bOpen ); 
			end;
		finally
			rg.Free;
		end;
	finally
	  Keys.EndUpdate;
	end;
end;

function EnumRegistryItems( RootKey: TKRegistryKey; const RootPath: string;
	RegItemKind: TKRegistryItem; Items: TStrings ): Boolean;

	procedure SetValues( rg: TRegistry; s1, s2: TStrings );
	var
		s: string;
		i: Integer;
	begin
		for i := 0 to s1.Count - 1 do
		begin
			s := s1[i];
			case RegItemKind of
				riValues     : s2[i] := rg.ReadString( s );
				riNamesValues: s2[i] := ( s + CH_EQUAL_TOKEN + rg.ReadString( s ) );
			end;
		end;
		s := rg.ReadString( '' ); { Default Key }
		if CheckStr( s ) then
			case RegItemKind of
				riValues     : s2.Insert( 0, s );
				riNamesValues: s2.Insert( 0, CH_GRIDLING + CH_EQUAL_TOKEN + s );
			end;
	end;

var
	sl: TStrings;
	rg: TRegistry;
begin
	Result := CheckObject( Items );
	if Result then
	begin
		rg := TRegistry.Create;
		try
			rg.RootKey := REGISTRY_KEYS[RootKey];
			Result := rg.OpenKey( CH_BACK_SLASH + RemoveFirstPathSlash( RemoveLastPathSlash(
				RootPath ) ), False );
			if Result then
			begin
				Items.BeginUpdate;
				try
					Items.Clear;
					case RegItemKind of
						riKeys:
							rg.GetKeyNames( Items );
						riNames:
							rg.GetValueNames( Items );
						riValues:
						begin
							sl := TStringList.Create;
							try
								Result := EnumRegistryItems( RootKey, RootPath, riNames, Items );
								if Result then
								  SetValues( rg, sl, Items );
							finally
								sl.Free;
							end;
						end;
						riNamesValues:
						begin
							Result := EnumRegistryItems( RootKey, RootPath, riNames, Items );
							if Result then
							  SetValues( rg, Items, Items );
						end;
					end;
					if ( not Result ) then
						Items.Clear;
				finally
				  Items.EndUpdate;
				end;
				rg.CloseKey;
			end;
		finally
			rg.Free;
		end;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic General Use Routines -------------------------
--------------------------------------------------------------------------------
}

{-------------------------------- Miscellaneous --------------------------------}

procedure FreeClean( var Source );
var
	Obj: TObject;
begin
	Obj := TObject( Source );
	TObject( Source ) := nil;
	Obj.Free;
end;

procedure FreeCleanPointer( var Source );
var
  p: Pointer;
begin
	p := Pointer( Source );
	Pointer( Source ) := nil;
	if CheckPointer( p ) then
		Dispose( p );
end;

function CheckWinNT: Boolean;
begin
  Result := ( GetOSVer.dwPlatformId = VER_PLATFORM_WIN32_NT );
end;

procedure ForceWinNT( IsRunning: Boolean );
begin
	if ( IsRunning and ( not CheckWinNT ) ) then
		RaiseException( EKSYUtils, sErrInvNotWinNT )
	else if ( ( not IsRunning ) and CheckWinNT ) then
		RaiseException( EKSYUtils, sErrInvWinNTCall )
end;

procedure DefaultSecurityDacl( ASecurity: PSecurityAttributes );
var
  sd: TSecurityDescriptor;
begin
	ForceWinNT( True );
	ForcePointer( ASecurity );
	ForcePointer( ASecurity.lpSecurityDescriptor );
	ZeroMemory( ASecurity, SizeOf( TSecurityAttributes ) );
	ZeroMemory( @sd, SizeOf( TSecurityDescriptor ) );
	InitializeSecurityDescriptor( @sd, SECURITY_DESCRIPTOR_REVISION );
	SetSecurityDescriptorDacl( @sd, True, nil, false );
	ASecurity.nLength := SizeOf( TSecurityAttributes );
	Move( sd, ASecurity.lpSecurityDescriptor, SizeOf( TSecurityDescriptor ) );
  ASecurity.bInheritHandle := False;
end;

function GetWindowsVer: TKWinVersion;
begin
	Result.All := GetVersion;
end;

function GetOSVer: TOSVersionInfo;
begin
	ZeroMemory( @Result, SizeOf( TOSVersionInfo ) );
	Result.dwOSVersionInfoSize := SizeOf( TOSVersionInfo );
	GetVersionEx( Result );
end;

function OSVersion: string;
begin
	with GetOSVer do
		Result := IntToStr( dwMajorVersion ) + CH_DOTMARK +
			Format( '%2.2d', [dwMinorVersion] ) + CH_DOTMARK +
			Format( '%3.3d', [dwBuildNumber and $FFFF] );
end;

function ShutDownWindows( wsd: TKWindowsShutDown ): Boolean;
const
	FILTER: array[TKWindowsShutDown] of Byte =
		( EWX_FORCE, EWX_LOGOFF, EWX_POWEROFF, EWX_REBOOT, EWX_SHUTDOWN );
begin
// see AdjustTokenPrivileges for REBOOT, POWEROFF, SHUTDOWN privilagies setting...
  Result := ExitWindowsEx( FILTER[wsd], 0 );  
end;

function UserName: string;
var
	pc: PChar;
	pdw: DWord;
begin
	pdw := MAXLEN_USERNAME + 1;
	pc := StrAlloc( pdw );
	try
		if GetUserName( pc, pdw ) then
			Result := StrPas( pc )
		else
			Result := '';
	finally
		StrDispose( pc );
	end;
end;

function DomainName: string;
const
	D_KEY: array[Boolean] of string = ( D95_KEY, DNT_KEY );
	D_PATH: array[Boolean] of string = ( D95_PATH, DNT_PATH );
var
	bNT: Boolean;
begin
	bNT := CheckWinNT;
	Result := ReadFromRegistry( rkLocalMachine, False, D_PATH[bNT], D_KEY[bNT] );
end;

function ComputerName: string;
var
	pc: PChar;
	pdw: DWord;
begin
	pdw := MAX_COMPUTERNAME_LENGTH + 1;
	pc := StrAlloc( pdw );
	try
		if GetComputerName( pc, pdw ) then
			Result := StrPas( pc )
		else
			Result := '';
	finally
		StrDispose( pc );
	end;
end;

function WorkgroupName: string;
begin
	if CheckWinNT then
	begin
		Result := DomainName;
		Exit;
	end;
	Result := ReadFromRegistry( rkLocalMachine, False, CW_PATH, CW_KEY );
end;

procedure NotYetImplemented;
begin
	MessageBeep( 0 );
	ShowDialog( sInformation, sNotImplemented, nil, dsOk, boInformation );
end;

procedure NotYetImplementedEx( const AddInfo: string );
begin
  MessageBeep( 0 );
	ShowDialog( sInformation, Format( sNotImplementedFmt, [AddInfo] ), nil, dsOk,
		boInformation );
end;

procedure Delay( MiliSecs: DWORD );
var
	iFirstTickCount: DWORD;
begin
	iFirstTickCount := GetTickCount;
	repeat
		Application.ProcessMessages;
	until ( ( GetTickCount-iFirstTickCount ) >= MiliSecs );
end;

function FindGlobalComponent( const CompName: string ): TComponent;
var
	i: Integer;
begin
	for i := 0 to Screen.FormCount - 1 do
	begin
		Result := Screen.Forms[i];
		if CheckStrEqual( CompName, Result.Name ) then
			Exit;
		Result := Result.FindComponent( CompName );
		if ( Result <> nil ) then
			Exit;
	end;
	for i := 0 to Screen.DataModuleCount - 1 do
	begin
		Result := Screen.DataModules[i];
		if CheckStrEqual( CompName, Result.Name ) then
			Exit;
		Result := Result.FindComponent( CompName );
		if ( Result <> nil ) then
			Exit;
	end;
	Result := nil;
end;

function Designing( AComp: TComponent ): Boolean;
begin
	ForceObject( AComp );
	Result := ( csDesigning in AComp.ComponentState );
end;

function Destroying( AComp: TComponent ): Boolean;
begin
	ForceObject( AComp );
	Result := ( csDestroying in AComp.ComponentState );
end;

function Loading( AComp: TComponent ): Boolean;
begin
	ForceObject( AComp );
	Result := ( csLoading in AComp.ComponentState );
end;

function Reading( AComp: TComponent ): Boolean;
begin
	ForceObject( AComp );
	Result := ( csReading in AComp.ComponentState );
end;

function Writing( AComp: TComponent ): Boolean;
begin
	ForceObject( AComp );
	Result := ( csWriting in AComp.ComponentState );
end;

function Delphi32Running( Version: TKDelphi32Version ): Boolean;
begin
	Result := false;
	if ( ( FindWindow( 'TApplication', 'Delphi' ) <= 0 ) and
			 ( FindWindow( 'TPropertyInspector', nil ) <= 0 ) and
			 ( FindWindow( 'TAppBuilder', nil ) <= 0 ) ) then
		Exit;
	if ( Version = dv400 ) then
		Result := CheckAllModules( sDelphi32, [sDelphi32DCC, sDelphi32MM, sDelphi32VCL40] )
	else if ( Version = dv300 ) then
		Result := CheckAllModules( sDelphi32, [sDelphi32DCC, sDelphi32MM, sDelphi32VCL30] )
	else
	begin
		Result := CheckAllModules( sDelphi32, [sDelphi32DCC, sDelphi32MM] );
		Result := Result and CheckAllModulesPartial( sDelphi32, [DELPHI_COMPILED_LIBRARY_EXT] );
	end;
end;

function CurrentDelphi32Running: Boolean;
begin
	Result := Delphi32Running( {$IFDEF DELPHI4} dv400 {$ELSE} {$IFDEF DELPHI3}
		dv300 {$ELSE} {$IFDEF DELPHI2} dv200 {$ENDIF} {$ENDIF} {$ENDIF} );
end;

function LastSubDir( const SubDir: string ): string;
begin
	Result := SubDir;
	while ( Pos( CH_BACK_SLASH, Result ) <> 0 ) do
		Result := Copy( Result, Pos( CH_BACK_SLASH, Result ) + 1, Length( Result ) );
end;

function RemoveLastPathSlash( const Path: string ): string;
begin
	Result := RemoveLastChar( Path, CH_BACK_SLASH );
end;

function RemoveFirstPathSlash( const Path: string ): string;
begin
  Result := RemoveFirstChar( Path, CH_BACK_SLASH );
end;

{------------------------------ Snapshot Support -------------------------------}

{--------------------------- Internal Implementation ---------------------------}

type

	TCreateToolhelp32SnapshotFunc = function( dwFlags, th32ProcessID: DWORD ): THandle; stdcall;
	TProcess32FirstFunc = function( hSnapshot: THandle; var lppe: TProcessEntry32 ): BOOL; stdcall;
	TProcess32NextFunc = function( hSnapshot: THandle; var lppe: TProcessEntry32 ): BOOL; stdcall;
	TModule32FirstFunc = function( hSnapshot: THandle; var lpme: TModuleEntry32 ): BOOL; stdcall;
	TModule32NextFunc = function( hSnapshot: THandle; var lpme: TModuleEntry32 ): BOOL; stdcall;
	TThread32FirstFunc = function( hSnapshot: THandle; var lpte: TThreadEntry32 ): BOOL; stdcall;
	TThread32NextFunc = function( hSnapshot: THandle; var lpte: TThreadEntry32 ): BOOL; stdcall;

	TToolHelp32Library = class( TKCustomLibraryMapping )
	private
		FCreateToolhelp32SnapshotFunc: TCreateToolhelp32SnapshotFunc;
		FProcess32FirstFunc: TProcess32FirstFunc;
		FProcess32NextFunc: TProcess32NextFunc;
		FModule32FirstFunc: TModule32FirstFunc;
		FModule32NextFunc: TModule32NextFunc;
		FThread32FirstFunc: TThread32FirstFunc;
		FThread32NextFunc: TThread32NextFunc;

		function GetCreateToolhelp32Snapshot: TCreateToolhelp32SnapshotFunc;
		function GetProcess32First: TProcess32FirstFunc;
		function GetProcess32Next: TProcess32NextFunc;
		function GetModule32First: TModule32FirstFunc;
		function GetModule32Next: TModule32NextFunc;
		function GetThread32First: TThread32FirstFunc;
		function GetThread32Next: TThread32NextFunc;

	public
		constructor Create; 

		property CreateToolhelp32Snapshot: TCreateToolhelp32SnapshotFunc
						 read GetCreateToolhelp32Snapshot;
		property Process32First: TProcess32FirstFunc
						 read GetProcess32First;
		property Process32Next: TProcess32NextFunc
						 read GetProcess32Next;
		property Module32First: TModule32FirstFunc
						 read GetModule32First;
		property Module32Next: TModule32NextFunc
						 read GetModule32Next;
		property Thread32First: TThread32FirstFunc
						 read GetThread32First;
		property Thread32Next: TThread32NextFunc
						 read GetThread32Next;

	end;

constructor TToolHelp32Library.Create;
begin
	inherited Create;
	SetLibraryName( sToolhelp32LibraryName );
end;

function TToolHelp32Library.GetCreateToolhelp32Snapshot: TCreateToolhelp32SnapshotFunc;
begin
	if CheckReference( @FCreateToolhelp32SnapshotFunc ) then
		Result := FCreateToolhelp32SnapshotFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryCreateToolhelp32Snapshot,
			@FCreateToolhelp32SnapshotFunc );
end;

function TToolHelp32Library.GetProcess32First: TProcess32FirstFunc;
begin
	if CheckReference( @FProcess32FirstFunc ) then
		Result := FProcess32FirstFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryProcess32First,
			@FProcess32FirstFunc );
end;

function TToolHelp32Library.GetProcess32Next: TProcess32NextFunc;
begin
	if CheckReference( @FProcess32NextFunc ) then
		Result := FProcess32NextFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryProcess32Next,
			@FProcess32NextFunc );
end;

function TToolHelp32Library.GetModule32First: TModule32FirstFunc;
begin
	if CheckReference( @FModule32FirstFunc ) then
		Result := FModule32FirstFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryModule32First,
			@FModule32FirstFunc );
end;

function TToolHelp32Library.GetModule32Next: TModule32NextFunc;
begin
	if CheckReference( @FModule32NextFunc ) then
		Result := FModule32NextFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryModule32Next,
			@FModule32NextFunc );
end;

function TToolHelp32Library.GetThread32First: TThread32FirstFunc;
begin
	if CheckReference( @FThread32FirstFunc ) then
		Result := FThread32FirstFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryThread32First,
			@FThread32FirstFunc );
end;

function TToolHelp32Library.GetThread32Next: TThread32NextFunc;
begin
	if CheckReference( @FThread32NextFunc ) then
		Result := FThread32NextFunc
	else
		Result := InternalLoadProc( sToolhelp32LibraryThread32Next,
			@FThread32NextFunc );
end;

var
	TH32Library: TToolHelp32Library = nil;

function TH32: TToolHelp32Library;
begin
	if ( not CheckObject( TH32Library ) ) then
		TH32Library := TToolHelp32Library.Create;
	Result := TH32Library;
end;

const
  TH32CS_SNAPHEAPLIST = $00000001;
	TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
	TH32CS_SNAPMODULE   = $00000008;
	TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
		TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
	TH32CS_INHERIT      = $80000000;

{---------------------------- Public Implementation ----------------------------}

function th32EnumThreads( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackThread ): Integer;
var
	bOK,
	bAbort: Boolean;
	Snap: THandle;
	Thread: TThreadEntry32;
begin
	Result := 0;
	bOK := CheckPointer( @Callback );
	Snap := TH32.CreateToolHelp32Snapshot( TH32CS_SNAPTHREAD, pID );
	if ( Snap = INFINITE ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvProcID, [pID] );
	try
		bAbort := false;
		ZeroMemory( @Thread, SizeOf( TThreadEntry32 ) );
		Thread.dwSize := SizeOf( TThreadEntry32 );
		if TH32.Thread32First( Snap, Thread ) then
			repeat
				Inc( Result );
				if bOK then
					bAbort := ( not Callback( Snap, Thread, Data ) );
			until ( bAbort or ( not TH32.Thread32Next( Snap, Thread ) ) );
	finally
		CloseHandle( Snap );
	end;
end;

function th32EnumModules( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackModule ): Integer;
var
	bOK,
	bAbort: Boolean;
	Snap: THandle;
	Module: TModuleEntry32;
begin
	Result := 0;
	bOK := CheckPointer( @Callback );
	Snap := TH32.CreateToolHelp32Snapshot( TH32CS_SNAPMODULE, pID );
	if ( Snap = INFINITE ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvProcID, [pID] );
	try
		bAbort := false;
		ZeroMemory( @Module, SizeOf( TModuleEntry32 ) );
		Module.dwSize := SizeOf( TModuleEntry32 );
		if TH32.Module32First( Snap, Module ) then
			repeat
				Inc( Result );
				if bOK then
					bAbort := ( not Callback( Snap, Module, Data ) );
			until ( bAbort or ( not TH32.Module32Next( Snap, Module ) ) );
	finally
		CloseHandle( Snap );
	end;
end;

function th32EnumProcesses( pID: th32ProcessID; Data: Pointer;
	Callback: th32EnumCallbackProcess ): Integer;
var
	bOK,
	bAbort: Boolean;
	Snap: THandle;
	Process: TProcessEntry32;
begin
	Result := 0;
	bOK := CheckPointer( @Callback );
	Snap := TH32.CreateToolHelp32Snapshot( TH32CS_SNAPPROCESS, pID );
	if ( Snap = INFINITE ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvProcID, [pID] );
	try
		bAbort := false;
		ZeroMemory( @Process, SizeOf( TProcessEntry32 ) );
		Process.dwSize := SizeOf( TProcessEntry32 );
		if TH32.Process32First( Snap, Process ) then
			repeat
				Inc( Result );
				if bOK then
					bAbort := ( not Callback( Snap, Process, Data ) );
			until ( bAbort or ( not TH32.Process32Next( Snap, Process ) ) );
	finally
		CloseHandle( Snap );
	end;
end;

{-------------------------- NT Processs Status Support -------------------------}

{--------------------------- Internal Implementation ---------------------------}

type

	TEnumProcessesFunc = function( pidList: Pointer; cb: Integer;
		var cbNeeded: Integer ): Boolean; stdcall;
	TEnumProcessModulesFunc = function( hProcess: THandle; ModuleList: Pointer;
		cb: Integer; var cbNeeded: Integer ) : Boolean; stdcall;
	TGetModuleFileNameExFunc = function( hProcess: THandle; Module: HInst;
		FileName: PChar; Size: Integer ): Integer; stdcall;

	TPSAPILibrary = class( TKCustomLibraryMapping )
	private
		FEnumProcesses: TEnumProcessesFunc;
		FEnumProcessModules: TEnumProcessModulesFunc;
		FGetModuleFileNameEx: TGetModuleFileNameExFunc;

		function GetEnumProcesses: TEnumProcessesFunc;
		function GetEnumProcessModules: TEnumProcessModulesFunc;
		function GetModuleFileNameEx: TGetModuleFileNameExFunc;

	public
		constructor Create;

		property EnumProcesses: TEnumProcessesFunc
						 read GetEnumProcesses;
		property EnumProcessModules: TEnumProcessModulesFunc
						 read GetEnumProcessModules;
		property ModuleFileNameEx: TGetModuleFileNameExFunc
						 read GetModuleFileNameEx;

	end;

constructor TPSAPILibrary.Create;
begin
	inherited Create;
	SetLibraryName( sNTPSAPILibraryName );
end;

function TPSAPILibrary.GetEnumProcesses: TEnumProcessesFunc;
begin
	if CheckReference( @FEnumProcesses ) then
		Result := FEnumProcesses
	else
		Result := InternalLoadProc( sNTPSAPILibraryEnumProcesses, @FEnumProcesses );
end;

function TPSAPILibrary.GetEnumProcessModules: TEnumProcessModulesFunc;
begin
	if CheckReference( @FEnumProcessModules ) then
		Result := FEnumProcessModules
	else
		Result := InternalLoadProc( sNTPSAPILibraryEnumProcessModules, @FEnumProcessModules );
end;

function TPSAPILibrary.GetModuleFileNameEx: TGetModuleFileNameExFunc;
begin
	if CheckReference( @FGetModuleFileNameEx ) then
		Result := FGetModuleFileNameEx
	else
		Result := InternalLoadProc( sNTPSAPILibraryGetModuleFileNameEx, @FGetModuleFileNameEx );
end;

var
	PSAPILibrary: TPSAPILibrary = nil;

function PSAPI: TPSAPILibrary;
begin
	if ( not CheckObject( PSAPILibrary ) ) then
		PSAPILibrary := TPSAPILibrary.Create;
	Result := PSAPILibrary;
end;

{---------------------------- Public Implementation ----------------------------}

procedure ntEnumProcesses( ss: TStrings );
var
	i,
	inProcesses: Cardinal;
	cName: array[0..MAX_PATH] of Char;
	cbNeeded: Integer;
	hMod: HModule;
	hProcess: THandle;
	pia: PLongIntArray;
begin
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
		pia := AllocMem( MAX_PROCESSES_RUNNING );
		try
			cbNeeded := 0;
			if PSAPI.EnumProcesses( @pia^[0], MAX_PROCESSES_RUNNING, cbNeeded ) then
			begin
				inProcesses := ( cbNeeded div SizeOf( DWORD ) );
				for i := 0 to inProcesses - 1 do
				begin
					hProcess := OpenProcess( PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
						false, pia^[i] );
					try
						if CheckHandle( hProcess ) then
						begin
							cbNeeded := 0;
							if PSAPI.EnumProcessModules( hProcess, @hMod, SizeOf( hMod ),
								cbNeeded ) then
							begin
								ZeroMemory( @cName, SizeOf( cName ) );
								PSAPI.ModuleFileNameEx( hProcess, hMod, cName, MAX_PATH );
								ss.Add( Format( ADDR_EQ_STR_LIST_PAT, [pia^[i], cName] ) );
							end
							else
								ss.Add( Format( ADDR_EQ_STR_LIST_PAT, [pia^[i], sNTProcSystem] ) );
						end
						else
							ss.Add( Format( ADDR_EQ_STR_LIST_PAT, [pia^[i], sNTProcAccessDenied] ) );
					finally
						CloseHandle( hProcess );
					end;
				end;
			end;
		finally
			FreeMem( pia, MAX_PROCESSES_RUNNING );
		end;
	finally
		ss.EndUpdate;
	end;
end;

procedure ntEnumModules( pID: ntProcessID; ss: TStrings );
var
	i,
	inModules: Cardinal;
	cName: array[0..MAX_PATH] of Char;
	cbNeeded: Integer;
	hProcess: THandle;
	pia: PLongIntArray;
begin
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
		hProcess := OpenProcess( PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
		  false, pID );
		try
			if CheckHandle( hProcess ) then
			begin
				cbNeeded := 0;
				pia := AllocMem( MAX_MODULES_IN_PROCESS );
				try
					if PSAPI.EnumProcessModules( hProcess, @pia^[0], MAX_MODULES_IN_PROCESS,
						cbNeeded ) then
					begin
						inModules := ( cbNeeded div SizeOf( DWORD ) );
						for i := 0 to inModules - 1 do
						begin
							ZeroMemory( @cName, SizeOf( cName ) );
							PSAPI.ModuleFileNameEx( hProcess, pia^[i], cName, MAX_PATH );
							ss.Add( Format( ADDR_EQ_STR_LIST_PAT, [pia^[i], cName] ) );
						end;
					end;
				finally
					FreeMem( pia, MAX_MODULES_IN_PROCESS );
				end;
			end;
		finally
			CloseHandle( hProcess );
		end;
	finally
		ss.EndUpdate;
	end;
end;

{---------- SystemProcessCount Callback Function ----------}

type
	PSPC_Data = ^SPC_Data;
	SPC_Data = record
		Count: LongInt;
		EXEName: ShortString;
	end;

function SPC_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with PSPC_Data( Data )^ do
		if ( CheckStrEqual( ExtractFileName( Process.szExeFile ), EXEName ) or
				 CheckStrEqual( Process.szExeFile,	EXEName ) ) then
			Inc( Count );
end;

function SystemProcessCount( const EXEName: string ): Integer;
var
	i: Integer;
	spc: SPC_Data;
	sl: TStrings;
begin
	ForceTrimStr( EXEName );
	if ( not CheckWinNT ) then
	begin
		spc.Count := 0;
		spc.EXEName := EXEName;
		th32EnumProcesses( 0, @spc, @SPC_ProcessCallback );
		Result := spc.Count;
	end
	else
	begin
		sl := TStringList.Create;
		try
			Result := 0;
			ntEnumProcesses( sl );
			for i := 0 to sl.Count - 1 do
				if ( CheckStrEqual( ExtractFileName( sl.Values[sl.Names[i]] ), EXEName ) or
						 CheckStrEqual( sl.Values[sl.Names[i]],	EXEName ) ) then
					Inc( Result );
		finally
			sl.Free;
		end;
	end;
end;

{---------- SystemProcessList Callback Function ----------}

type
	PSPL_Data = ^SPL_Data;
	SPL_Data = record
		Strings: TStrings;
		EXENameOnly: Boolean;
	end;

function SPL_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with PSPL_Data( Data )^ do
		if EXENameOnly then
			Strings.Add( ExtractFileName( Process.szExeFile ) )
		else
			Strings.Add( Process.szExeFile );
end;

procedure SystemProcessList( ss: TStrings; EXENameOnly, Sorted: Boolean );
var
	i: Integer;
	spl: SPL_Data;
begin
	ForceObject( ss );
	if CheckObjectClass( ss, TStringList ) then
	begin
		TStringList( ss ).Sorted := Sorted;
		TStringList( ss ).Duplicates := dupIgnore;
	end;
	ss.BeginUpdate;
	try
		if ( not CheckWinNT ) then
		begin
			spl.Strings := ss;
			spl.EXENameOnly := EXENameOnly;
			th32EnumProcesses( 0, @spl, @SPL_ProcessCallback );
		end
		else
		begin
			ntEnumProcesses( ss );
			if EXENameOnly then
				for i := 0 to ss.Count - 1 do
					ss.Values[ss.Names[i]] := ExtractFileName( ss.Values[ss.Names[i]] );
			for i := 0 to ss.Count - 1 do
				ss[i] := ss.Values[ss.Names[i]];
		end;
	finally
		ss.EndUpdate;
	end;
end;

function CheckSystemProcess( const EXEName: string ): Boolean;
begin
	Result := ( SystemProcessCount( EXEName ) > 0 );
end;

procedure ForceSystemProcess( const EXEName: string );
begin
	if ( not CheckSystemProcess( EXEName ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrForceProcess, [EXEName] );
end;

{---------- GetProcessName Callback Function ----------}

type
	PGPN_Data = ^GPN_Data;
	GPN_Data = record
		pID: THandle;
		EXEName: string;
	end;

function GPN_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with PGPN_Data( Data )^ do
		if ( pID = Process.th32ProcessID ) then
		begin
			EXEName := Process.szExeFile;
			Result := false;
		end;
end;

function GetProcessName( pID: THandle ): string;
var
	iPos: Integer;
	gpn: GPN_Data;
	sl: TStringList;
begin
	if ( not CheckWinNT ) then
	begin
		gpn.pID := pID;
		gpn.EXEName := '';
		th32EnumProcesses( 0, @gpn, @GPN_ProcessCallback );
		Result := gpn.EXEName;
	end
	else
	begin
		sl := TStringList.Create;
		try
			ntEnumProcesses( sl );
			iPos := sl.IndexOfName( IntToStr( pID ) );
			if ( iPos <> -1 ) then
				Result := sl.Values[sl.Names[iPos]]
			else
				Result := '';
		finally
			sl.Free;
		end;
	end;
end;

function GetCurrentProcessName: string;
begin
	Result := GetProcessName( GetCurrentProcessID );
	ForceTrimStr( Result );
end;

{---------- CheckModules Callback Functions ----------}

type
	PCM_Data = ^CM_Data;
	CM_Data = record
		OK: Boolean;
		ModCount: LongInt;
		Modules: Variant;
		EXEName: ShortString;
		ModuleCallback: th32EnumCallbackModule;
	end;

function CMP_ModuleCallback( Snap: THandle; Module: TModuleEntry32;
	var Data ): Boolean;
var
	i: Integer;
begin
	Result := true;
	with PCM_Data( Data )^ do
		for i := VarArrayLowBound( Modules, 1 ) to VarArrayHighBound( Modules, 1 ) do
			if ( Pos( Modules[i], Module.szModule ) > 0 ) then
			begin
				Inc( ModCount );
				Break;
			end;
end;

function CM_ModuleCallback( Snap: THandle; Module: TModuleEntry32;
	var Data ): Boolean;
var
	i: Integer;
begin
	Result := true;
	with PCM_Data( Data )^ do
		for i := VarArrayLowBound( Modules, 1 ) to VarArrayHighBound( Modules, 1 ) do
			if CheckStrEqual( Module.szModule, Modules[i] ) then
			begin
				Inc( ModCount );
				Break;
			end;
end;

function CM_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	PCM_Data( Data )^.ModCount := 0;
	with Process do
		if ( CheckStrEqual( ExtractFileName( szExeFile ), PCM_Data( Data )^.EXEName ) or
  			 CheckStrEqual( szExeFile,	PCM_Data( Data )^.EXEName )  ) then
			th32EnumModules( th32ProcessID, PCM_Data( Data ),
				PCM_Data( Data )^.ModuleCallback );
{
	Looking for AT LEAST ONE process with the given name and modules
	loaded in its address space; (Result=CONTINUE ENUMERATION) if a
	process with the given modules loaded is found, there is no need
	to keep on with the enumeration
}
	with PCM_Data( Data )^ do
	begin
		OK := ( Pred( ModCount ) = VarArrayHighBound( Modules, 1 ) );
		Result := ( not OK );
	end;
end;

function CheckAllModules( const EXEName: string; const Modules: array of string ): Boolean;
var
	i,
	j,
	k: Integer;
	cm: CM_Data;
	ssp,
	ssm: TStringList;
	bFound: Boolean;
begin
	ForceTrimStr( EXEName );
	ForceTrimStrs( Modules );
	if ( not CheckWinNT ) then
	begin
		cm.OK := false;
		cm.EXEName := EXEName;
		cm.Modules := ArrayOfStringToVar( Modules );
		cm.ModuleCallback := @CM_ModuleCallback;
		th32EnumProcesses( 0, @cm, @CM_ProcessCallback );
		Result := cm.OK;
	end
	else
	begin
		ssp := TStringList.Create;
		try
			Result := false;
			ntEnumProcesses( ssp );
			ssm := TStringList.Create;
			try
				for i := 0 to ssp.Count - 1 do
					if ( CheckStrEqual( ExtractFileName( ssp.Values[ssp.Names[i]] ), EXEName ) or
							 CheckStrEqual( ssp.Values[ssp.Names[i]],	EXEName ) ) then
					begin
						bFound := false;
						ntEnumModules( StrToInt( ssp.Names[i] ), ssm );
						for j := Low( Modules ) to High( Modules ) do
						begin
							bFound := false;
							for k := 0 to ssm.Count - 1 do
								bFound := ( bFound or
									CheckStrEqual( ssm.Values[ssm.Names[k]], Modules[j] ) or
									CheckStrEqual( ExtractFileName( ssm.Values[ssm.Names[k]] ), Modules[j] ) );
{ one of the modules was NOT found; don't need to continue looking: search already failed }
							if ( not bFound ) then
								Break;
						end;
						Result := Result or bFound;
{ one of process is found with all modules mappen on its address space, exit }
						if Result then
							Exit;
					end;
			finally
				ssm.Free;
			end;
		finally
			ssp.Free;
		end;
	end;
end;

function CheckAllModulesPartial( const EXEName: string; const Modules: array of string ): Boolean;
var
	i,
	j,
	k: Integer;
	cm: CM_Data;
	ssp,
	ssm: TStringList;
	bFound: Boolean;
begin
	ForceTrimStr( EXEName );
	ForceTrimStrs( Modules );
	if ( not CheckWinNT ) then
	begin
		cm.OK := false;
		cm.EXEName := EXEName;
		cm.Modules := ArrayOfStringToVar( Modules );
		cm.ModuleCallback := @CMP_ModuleCallback;
		th32EnumProcesses( 0, @cm, @CM_ProcessCallback );
		Result := cm.OK;
	end
	else
	begin
		ssp := TStringList.Create;
		try
			Result := false;
			ntEnumProcesses( ssp );
			ssm := TStringList.Create;
			try
				for i := 0 to ssp.Count - 1 do
					if ( CheckStrEqual( ExtractFileName( ssp.Values[ssp.Names[i]] ), EXEName ) or
							 CheckStrEqual( ssp.Values[ssp.Names[i]],	EXEName ) ) then
					begin
						bFound := false;
						ntEnumModules( StrToInt( ssp.Names[i] ), ssm );
						for j := Low( Modules ) to High( Modules ) do
						begin
							bFound := false;
							for k := 0 to ssm.Count - 1 do
								bFound := bFound or ( Pos( Modules[j], ssm.Values[ssm.Names[k]] ) > 0 );
{ one of the modules was NOT found; don't need to continue looking: search already failed }
							if ( not bFound ) then
								Break;
						end;
						Result := Result or bFound;
{ one of process is found with all modules mappen on its address space, exit }
						if Result then
							Exit;
					end;
			finally
				ssm.Free;
			end;
		finally
			ssp.Free;
		end;
	end;
end;

{---------- GetModuleList Callback Functions ----------}

type
	PGML_Data = ^GML_Data;
	GML_Data = record
		Strings: TStrings;
		EXEName: ShortString;
		EXENameOnly: Boolean;
	end;

function GML_ModuleCallback( Snap: THandle; Module: TModuleEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with PGML_Data( Data )^ do
		if EXENameOnly then    
			Strings.Add( Format( ADDR_EQ_STR_LIST_PAT, [Module.th32ModuleID,
				ExtractFileName( Module.szModule )] ) )
		else
			Strings.Add( Format( ADDR_EQ_STR_LIST_PAT, [Module.th32ModuleID,
				Module.szModule] ) );
end;

function GML_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with Process do
		if ( CheckStrEqual( ExtractFileName( szExeFile ), PGML_Data( Data )^.EXEName ) or
				 CheckStrEqual( szExeFile, PGML_Data( Data )^.EXEName ) ) then
		begin
			PGML_Data( Data )^.Strings.Add( Format( sProcessSeparator,
				[th32ProcessID, szExeFile] ) );
			th32EnumModules( th32ProcessID, PGML_Data( Data ),
				@GML_ModuleCallback );
		end;
end;

procedure GetModuleList( const EXEName: string; ss: TStrings; EXENameOnly, Sorted: Boolean );
var
	i,
	j: Integer;
	gml: GML_Data;
	ssp,
	ssm: TStringList;
begin
	ForceTrim( [EXEName, ss] );
	if CheckObjectClass( ss, TStringList ) then
	begin
		TStringList( ss ).Sorted := Sorted;
		TStringList( ss ).Duplicates := dupIgnore;
	end;
	if ( not CheckWinNT ) then
	begin
		ss.BeginUpdate;
		try
			ss.Clear;
			gml.Strings := ss;
			gml.EXEName := EXEName;
			gml.EXENameOnly := EXENameOnly;
			th32EnumProcesses( 0, @gml, @GML_ProcessCallback );
		finally
			ss.EndUpdate;
		end;
	end
	else
	begin
		ssp := TStringList.Create;
		try
			ntEnumProcesses( ssp );
			ssm := TStringList.Create;
			try
				ss.BeginUpdate;
				try
					ss.Clear;
					for i := 0 to ssp.Count - 1 do
						if ( CheckStrEqual( ExtractFileName( ssp.Values[ssp.Names[i]] ), EXEName ) or
								 CheckStrEqual( ssp.Values[ssp.Names[i]],	EXEName ) ) then
						begin
							ss.Add( Format( sProcessSeparator, [StrToInt( ssp.Names[i] ), ssp.Values[ssp.Names[i]]] ) );
							ntEnumModules( StrToInt( ssp.Names[i] ), ssm );
							for j := 0 to ssm.Count - 1 do
								if EXENameOnly then
									ss.Add( ssm.Names[j] + CH_EQUAL_TOKEN + ExtractFileName( ssm.Values[ssm.Names[j]] ) )
								else
									ss.Add( ssm[j] );
						end;
				finally
					ss.EndUpdate;
				end;
			finally
				ssm.Free;
			end;
		finally
			ssp.Free;
		end;
	end;
end;

procedure GetCurrentModuleList( ss: TStrings; EXENameOnly, Sorted: Boolean );
begin
	GetModuleList( GetCurrentProcessName, ss, EXENameOnly, Sorted );
end;

{---------- GetThreadList Callback Functions ----------}

type
	PGTL_Data = ^GTL_Data;
	GTL_Data = record
		Strings: TStrings;
		EXEName: ShortString;
	end;

function GTL_ThreadCallback( Snap: THandle; Thread: TThreadEntry32;
	var Data ): Boolean;
const
	THREAD_INFO_PAT = '$%.8x=$%.8x';
begin
	Result := true;
	with PGTL_Data( Data )^ do
		Strings.Add( Format( THREAD_INFO_PAT, [Thread.th32ThreadID, Thread.tpBasePri] ) );
end;

function GTL_ProcessCallback( Snap: THandle; Process: TProcessEntry32;
	var Data ): Boolean;
begin
	Result := true;
	with Process do
		if ( CheckStrEqual( ExtractFileName( szExeFile ), PGTL_Data( Data )^.EXEName ) or
				 CheckStrEqual( szExeFile, PGTL_Data( Data )^.EXEName ) ) then
		begin
			PGTL_Data( Data )^.Strings.Add( Format( sProcessSeparator,
				[th32ProcessID, szExeFile] ) );
			th32EnumThreads( th32ProcessID, PGTL_Data( Data ), @GTL_ThreadCallback );
		end;
end;

procedure GetThreadList( const EXEName: string; ss: TStrings );
var
	gtl: GTL_Data;
begin
	ForceTrim( [EXEName, ss] );
	ForceWinNT( False );
	ss.BeginUpdate;
	try
		ss.Clear;
		gtl.Strings := ss;
		gtl.EXEName := EXEName;
		th32EnumProcesses( 0, @gtl, @GTL_ProcessCallback );
	finally
		ss.EndUpdate;
	end;
end;

procedure GetCurrentThreadList( ss: TStrings );
begin
	GetThreadList( GetCurrentProcessName, ss );
end;

function CheckAnyModules( const EXEName: string; const Modules: array of string ): Boolean;
var
	i: Integer;
begin
	ForceTrimStrs( Modules );
	Result := false;
	for i := Low( Modules ) to High( Modules ) do
		Result := Result or CheckAllModules( EXEName, [Modules[i]] );
end;

function CheckAnyModulesPartial( const EXEName: string; const Modules: array of string ): Boolean;
var
	i: Integer;
begin
	ForceTrimStrs( Modules );
	Result := false;
	for i := Low( Modules ) to High( Modules ) do
		Result := Result or CheckAllModulesPartial( EXEName, [Modules[i]] );
end;

function CheckCurrentAllModules( const Modules: array of string ): Boolean;
begin
	Result := CheckAllModules( GetCurrentProcessName, Modules );
end;

function CheckCurrentAllModulesPartial( const Modules: array of string ): Boolean;
begin
	Result := CheckAllModulesPartial( GetCurrentProcessName, Modules );
end;

function CheckCurrentAnyModules( const Modules: array of string ): Boolean;
begin
	Result := CheckAnyModules( GetCurrentProcessName, Modules );
end;

function CheckCurrentAnyModulesPartial( const Modules: array of string ): Boolean;
begin
	Result := CheckAnyModulesPartial( GetCurrentProcessName, Modules );
end;

function SystemWideAllModules( ss: TStrings; Sorted: Boolean; const Modules: array of string ): Boolean;
var
	i: Integer;
begin
	ForceObject( ss );
	SystemProcessList( ss, false, Sorted );
	Result := CheckStrings( ss );
	if ( not Result ) then
		Exit;
	ss.BeginUpdate;
	try
		for i := ss.Count - 1 downto 0 do
			if ( not CheckAllModules( ss.Strings[i], Modules ) ) then
				ss.Delete( i );
		Result := CheckStrings( ss );
	finally
		ss.EndUpdate;
	end;
end;

function SystemWideAnyModules( ss: TStrings; Sorted: Boolean; const Modules: array of string ): Boolean;
var
	i: Integer;
begin
	ForceObject( ss );
	SystemProcessList( ss, false, Sorted );
	Result := CheckStrings( ss );
	if ( not Result ) then
		Exit;
	ss.BeginUpdate;
	try
		for i := ss.Count - 1 downto 0 do
			if ( not CheckAnyModules( ss.Strings[i], Modules ) ) then
				ss.Delete( i );
		Result := CheckStrings( ss );
	finally
		ss.EndUpdate;
	end;
end;

{-------------------------------- Net Resources --------------------------------}

function NetResourceEqual( R1, R2: PNetResourceA ): Boolean;
begin
	Result := ( CheckPointers( [R1, R2] ) and CompareMem( R1, R2, 4 * SizeOf( Integer ) ) );
	if CheckPChars(	[R1.lpLocalName, R2.lpLocalName] ) then
		Result := Result and ( CompareText( R1^.lpLocalName, R2^.lpLocalName ) = 0 );
	if CheckPChars(	[R1.lpRemoteName, R2.lpRemoteName] ) then
		Result := Result and ( CompareText( R1^.lpRemoteName, R2^.lpRemoteName ) = 0 );
	if CheckPChars(	[R1.lpComment, R2.lpComment] ) then
		Result := Result and ( CompareText( R1^.lpComment, R2^.lpComment ) = 0 );
	if CheckPChars(	[R1.lpProvider, R2.lpProvider] ) then
		Result := Result and ( CompareText( R1^.lpProvider, R2^.lpProvider ) = 0 );
end;

procedure EnumNetResources( Root: PNetResource; const Recursive: Boolean;
	CallBack: TNetResourceFunc; Data: Pointer );

var
	bStopNetEnum: Boolean;

	procedure InternalEnumNetResources( Root: PNetResource; const Recursive: Boolean;
		CallBack: TNetResourceFunc; Data: Pointer );
	const
		MAX_NET_RESOURCE = 11;
	var
		phEnum: THandle;
		NetResInfo: array[0..MAX_NET_RESOURCE-1] of TNetResource;
		i,
		dCount,
		dSize,
		dStatus : DWORD;
	begin
		if bStopNetEnum then
			Exit;
		ForcePointer( @CallBack );
		if ( WNetOpenEnum( RESOURCE_GLOBALNET, RESOURCETYPE_ANY, RESOURCEUSAGE_CONTAINER +
			RESOURCEUSAGE_CONNECTABLE, Root, phEnum ) = NO_ERROR ) then
			try
				repeat
					dCount := INFINITE;
					dSize := SizeOf( NetResInfo );
					ZeroMemory( @NetResInfo[0], dSize );
					dStatus := WNetEnumResource( phEnum, dCount, @NetResInfo[0], dSize );
					for i := 0 to dCount - 1 do
					begin
						if ( not CallBack( Root, @NetResInfo[i], Data ) ) then
						begin
							bStopNetEnum := True;
							Exit;
						end;
						if ( Recursive ) then
							InternalEnumNetResources( @NetResInfo[i], Recursive, CallBack, Data );
					end;
				until ( dStatus = ERROR_NO_MORE_ITEMS );
			finally
				WNetCloseEnum( phEnum );
			end;
	end;

begin
	bStopNetEnum := False;
	InternalEnumNetResources( Root, Recursive, CallBack, Data );
end;

function NRL_NetResourcesCallBack( ParentNetResource, NetResource: PNetResource;
	Data: Pointer ): Boolean;
var
	pnrl: PNetResourceTree;
begin
	ForcePointer( NetResource ); { Do not for ParentNetResource, can be nil... }
	pnrl := New( PNetResourceTree );
	try
		ZeroMemory( pnrl, SizeOf( TNetResourceTree ) );
		Move( NetResource^, pnrl^.NetResource, 4 * SizeOf( Integer ) );
		with NetResource^ do
		begin
			pnrl^.NetResource.lpLocalName := StrNew( lpLocalName );
			pnrl^.NetResource.lpRemoteName := StrNew( lpRemoteName );
			pnrl^.NetResource.lpComment := StrNew( lpComment );
			pnrl^.NetResource.lpProvider := StrNew( lpProvider );
		end;
		if CheckPointer( ParentNetResource ) then
		begin
			pnrl^.HasParent := True;
			Move( ParentNetResource^, pnrl^.ParentNetResource, 4 * SizeOf( Integer ) );
			with ParentNetResource^ do
			begin
				pnrl^.ParentNetResource.lpLocalName := StrNew( lpLocalName );
				pnrl^.ParentNetResource.lpRemoteName := StrNew( lpRemoteName );
				pnrl^.ParentNetResource.lpComment := StrNew( lpComment );
				pnrl^.ParentNetResource.lpProvider := StrNew( lpProvider );
			end;
		end;
		TList( Data ).Add( pnrl );
		Result := True;
	except
		DisposeNetResourceEntry( pnrl );
		raise;
	end;
end;

procedure DisposeNetResourceEntry( pnrl: PNetResourceTree );

	procedure DisposeNetResource( NetResource: TNetResource );
	begin
		with NetResource do
		begin
			if CheckPChar( lpLocalName ) then
				StrDispose( lpLocalName );
			if CheckPChar( lpRemoteName ) then
				StrDispose( lpRemoteName );
			if CheckPChar( lpComment ) then
				StrDispose( lpComment );
			if CheckPChar( lpProvider ) then
				StrDispose( lpProvider );
		end;
	end;

begin
	ForcePointer( pnrl );
	with pnrl^ do
	begin
		DisposeNetResource( NetResource );
		if ( HasParent ) then
			DisposeNetResource( ParentNetResource );
	end;
	Dispose( pnrl );
end;

procedure ClearNetResourceList( lst: TList );
begin
	ForceObject( lst );
	while CheckList( lst ) do
	begin
		DisposeNetResourceEntry( PNetResourceTree( lst.Items[lst.Count-1] ) );
		lst.Delete( lst.Count-1 );
	end;
end;

procedure GetNetResourcesList( lst: TList );
begin
	ForceObject( lst );
	lst.Clear;
	try
		EnumNetResources( nil, True, NRL_NetResourcesCallBack, Pointer( lst ) );
	except
		ClearNetResourceList( lst );
		raise;
	end;
end;

procedure GetDomainServersList( lst: TList; const domain: string );
var
	i: Integer;
	lt: TList;
	go_on: Boolean;
	pnr: PNetResourceA;
	pnrl: PNetResourceTree;
begin
	ForceObject( lst );
	lst.Clear;
	go_on := false;
	lt := TList.Create;
	try
		pnr := nil;
		GetNetResourcesList( lt );
		try
			for i := 0 to lt.Count - 1 do
				with PNetResourceTree( lt[i] )^.NetResource do
					if ( ( dwDisplayType = RESOURCEDISPLAYTYPE_DOMAIN ) and CheckPointer( lpRemoteName ) and
							 ( CompareText( domain, lpRemoteName ) = 0 ) ) then
					begin
						go_on := true;
						with PNetResourceTree( lt[i] )^ do
							pnr := @NetResource;
						Break;
					end;
			if go_on then
				for i := 0 to lt.Count - 1 do
					with PNetResourceTree( lt[i] )^ do
						if HasParent and
							 ( NetResource.dwDisplayType = RESOURCEDISPLAYTYPE_SERVER ) and
							 NetResourceEqual( pnr, @ParentNetResource ) then
						begin
							pnrl := New( PNetResourceTree );
							try
								pnrl^.HasParent := True;
								ZeroMemory( pnrl, SizeOf( TNetResourceTree ) );
								Move( NetResource, pnrl^.NetResource, 4 * SizeOf( Integer ) );
								with NetResource do
								begin
									pnrl^.NetResource.lpLocalName := StrNew( lpLocalName );
									pnrl^.NetResource.lpRemoteName := StrNew( lpRemoteName );
									pnrl^.NetResource.lpComment := StrNew( lpComment );
									pnrl^.NetResource.lpProvider := StrNew( lpProvider );
								end;
								lst.Add( pnrl );
							except
								DisposeNetResourceEntry( pnrl );
								raise;
							end;
						end;
		finally
			ClearNetResourceList( lt );
		end;
	finally
		lt.Free;
	end;
end;

procedure GetDomainNames( sl: TStrings );
var
	i: Integer;
	lst: TList;
begin
	ForceObject( sl );
	sl.BeginUpdate;
	try
		sl.Clear;
		lst := TList.Create;
		try
			GetNetResourcesList( lst );
			for i := 0 to lst.Count - 1 do
				with PNetResourceTree( lst[i] )^.NetResource do
					if ( ( dwDisplayType = RESOURCEDISPLAYTYPE_DOMAIN ) and CheckPointer( lpRemoteName ) and
							 ( sl.IndexOf( lpRemoteName ) = -1 ) ) then
						sl.Add( lpRemoteName );
		finally
			lst.Free;
		end;
	finally
		sl.EndUpdate;
	end;
end;

{---------------------------------- Singleton ----------------------------------}

function CheckSingleton( AOwner: TComponent; AClass: TComponentClass ): Boolean;

	function InternalCheckSingleton( AOwner: TComponent ): Boolean;
	var
		i: Integer;
	begin
		Result := true;
		for i := 0 to AOwner.ComponentCount - 1 do
			Result := ( Result and ( not CheckObjectClass( AOwner.Components[i], AClass ) ) and
				InternalCheckSingleton( AOwner.Components[i] ) );
	end;

begin
	Result := CheckClass( AClass ) and ( ( not CheckObject( AOwner ) ) or
	  InternalCheckSingleton( AOwner ) );
end;

procedure ForceSingleton( AOwner: TComponent; AClass: TComponentClass );
begin
	if ( not CheckSingleton( AOwner, AClass ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrForceSingleton, [GetFirstString(
			[AOwner.Name, AOwner.ClassName] ), AClass.ClassName] );
end;

{-------------------------- Extract Package Information ------------------------}

{---------------------------- Internal Implementation --------------------------}

{

	Package flags:

	bit     meaning
	-----------------------------------------------------------------------------------------
	0     | 1: never-build              0: always build
	1     | 1: design-time only         0: not design-time only      on => bit 2 = off
	2     | 1: run-time only            0: not run-time only         on => bit 1 = off
	3..29 | reserved
	30..31| 0: EXE, 1: Package DLL, 2: Library DLL, 3: undefined

	PackageUnitFlags:

	bit      meaning
	-----------------------------------------------------------------------------------------
	0      | main unit
	1      | package unit (dpk source)
	2      | $WEAKPACKAGEUNIT unit
	3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
	4      | implicitly imported
	5..7   | reserved
}

type

	PKPkInfos = ^TKPkInfos;
	TKPkInfos = record
		Units: TStrings;
		Packages: TStrings;
	end;

const
	BIT_NEVER_BUILD  = 0;
	BIT_DESIGN_TIME  = 1;
	BIT_RUN_TIME     = 2;
	BIT_PACKGE_FILE1 = 30;
	BIT_PACKGE_FILE2 = 31;

	BIT_EXE = 0;
	BIT_DPL = 1;
	BIT_DLL = 2;
	BIT_UNK = 3;

	BIT_FILTER: array[Boolean, Boolean] of Byte =
	  ( ( BIT_EXE, BIT_DPL ), ( BIT_DLL, BIT_UNK ) );

{ Flag are always 0 for NameType ntRequiresPackage. Param are user difined }
procedure InfoProc( const Name: string; NameType: TNameType; Flags: Byte;
	Param: PKPkInfos );
begin
	case NameType of
		ntContainsUnit:
			if CheckObject( Param^.Units ) then
				Param^.Units.AddObject( Name, TObject( Flags ) );
		ntRequiresPackage:
			if CheckObject( Param^.Packages ) then
  			Param^.Packages.Add( Name );
	end;
end;

{----------------------------- Public Implementation ---------------------------}

procedure GetPackageInfoEx( const PackageName: string; ssUnits, ssPackages: TStrings;
	var Flags: TKPackageInfos; LoadType: TKPackageInfoLoadType );
var
	hMod: HMODULE;
	iFlags: Integer;
	pkInfos: TKPkInfos;
begin
	Flags := [];
	if ( not ( CheckFile( PackageName ) and CheckStrEqual( DELPHI_PACKAGE_EXT, ExtractFileExt( PackageName ) ) ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvPackageName, [PackageName] );
	case LoadType of
		piltPackage: hMod := LoadPackage( PackageName );
		piltLibrary: hMod := LoadLibrary( PChar( PackageName ) );
	else
		hMod := INVALID_HANDLE_VALUE;
	end;
	ForceHandle( hMod );
	try
		iFlags := 0;
		if CheckObject( ssUnits ) then
			ssUnits.BeginUpdate;
		try
			if CheckObject( ssUnits ) then
				ssUnits.Clear;
			if CheckObject( ssPackages ) then
				ssPackages.BeginUpdate;
			try
				if CheckObject( ssPackages ) then
					ssPackages.Clear;
				pkInfos.Units := ssUnits;
				pkInfos.Packages := ssPackages;
				GetPackageInfo( hMod, @pkInfos, iFlags, TPackageInfoProc( @InfoProc ) );
				if IsBitOn( iFlags, BIT_NEVER_BUILD ) then
					Include( Flags, piNeverBuild );
				if IsBitOn( iFlags, BIT_DESIGN_TIME ) then
					Include( Flags, piDesignTime )
				else
					Include( Flags, piRunTime );
				if IsBitOn( iFlags, BIT_RUN_TIME ) then
					Include( Flags, piRunTime )
				else
					Include( Flags, piDesignTime );
				case BIT_FILTER[IsBitOn( iFlags, BIT_PACKGE_FILE2 ), IsBitOn( iFlags, BIT_PACKGE_FILE1 )] of
					BIT_EXE: Include( Flags, piEXE );
					BIT_DPL: Include( Flags, piDPL );
					BIT_DLL: Include( Flags, piDLL );
					BIT_UNK: Include( Flags, piUnknown );
				end;
			finally
				if CheckObject( ssPackages ) then
					ssPackages.EndUpdate;
			end;
		finally
			if CheckObject( ssUnits ) then
				ssUnits.EndUpdate;
		end;
	finally
		case LoadType of
			piltPackage: UnloadPackage( hMod );
			piltLibrary: FreeLibrary( hMod );
		end;
	end;
end;

procedure GetKnowhowPackageInfo( const PackageName: string; ssUnits, ssPackages: TStrings;
	var Flags: TKPackageInfos; LoadType: TKPackageInfoLoadType );
var
	i: Integer;
begin
	GetPackageInfoEx( PackageName, ssUnits, ssPackages, Flags, LoadType );
	if CheckStrings( ssUnits ) then
		for i := ssUnits.Count - 1 downto 0 do
			if ( not CheckStrContains( UpperCase( KNOWHOW_UNIT_PATTERN ), UpperCase( ssUnits[i] ) ) ) then
				ssUnits.Delete( i );
	if CheckStrings( ssPackages ) then
		for i := ssPackages.Count - 1 downto 0 do
			if ( not ( CheckStrContains( UpperCase( KNOWHOW_RTPPACKAGE_PATTERN ),
				UpperCase( ssPackages[i] ) ) or CheckStrContains( UpperCase(
				KNOWHOW_DTPPACKAGE_PATTERN ), UpperCase( ssPackages[i] ) ) ) ) then
				ssPackages.Delete( i );
end;

function GetPackageUnitFlags( PackageInfoUnitsList: TStrings; Index: Integer ): TKPackageUnitFlags;
begin
	ForceObject( PackageInfoUnitsList );
	Byte( Result ) := Byte( Integer( PackageInfoUnitsList.Objects[Index] ) );
end;

function GetPackageDescription( const PackageName: string ): string;
var
  hMod: HMODULE;
	pwc: PWideChar;
	rs: TResourceStream;
begin
	if ( not ( CheckFile( PackageName ) and CheckStrEqual( DELPHI_PACKAGE_EXT, ExtractFileExt( PackageName ) ) ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvPackageName, [PackageName] );
	hMod := LoadLibrary( PChar( PackageName ) );
	ForceHandle( hMod );
	try
		rs := TResourceStream.Create( hMod, DELPHI_PACKAGE_DESCRIPTION_RESNAME, RT_RCDATA );
		try
			Result := '';
			if CheckStream( rs ) then
			begin
				pwc := AllocMem( rs.Size );
				try
					rs.ReadBuffer( pwc^, rs.Size );
					Result := WideCharToString( pwc );
				finally
					FreeMem( pwc, rs.Size );
				end;
			end;	
		finally
			rs.Free;
		end;
	finally
	  FreeLibrary( hMod );
	end;
end;

function MsgToMessage( const msg: TMsg ): TMessage;
begin
  ZeroMemory( @Result, SizeOf( TMessage ) );
  Result.Msg := msg.message;
  Result.WParam := msg.WParam;
  Result.LParam := msg.LParam;
end;

function MessageToMsg( const Message: TMessage ): TMsg;
begin
  ZeroMemory( @Result, SizeOf( TMsg ) );
  Result.message := Message.Msg;
  Result.WParam := Message.WParam;
  Result.LParam := Message.LParam;
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Form Routines -----------------------------
--------------------------------------------------------------------------------
}

{--- FirstFormByClass, NextFormByClass, and CloseFindFormByClass Support ---}

type
	PFindForm = ^TFindForm;
	TFindForm = record
		Handle: HWnd;
		Next: PFindForm;
	end;

var
	TopFindForm: PFindForm = nil;
	BottomFindForm: PFindForm = nil;

procedure FindFormInclude( AHandle: HWnd );
var
	pff: PFindForm;
begin
	pff := TopFindForm;
	if ( not CheckPointer( pff ) ) then
{ this is a call to FindFirstForm }
	begin
		TopFindForm := New( PFindForm );
		TopFindForm.Handle := AHandle;
		TopFindForm.Next := nil;
		BottomFindForm := TopFindForm;
	end
	else
{ this is a call to FindNextForm }
	begin
		pff := New( PFindForm );
		pff.Handle := AHandle;
		pff.Next := nil;
		BottomFindForm.Next := pff;
		BottomFindForm := pff;
	end;
end;

function FindFormExists( AForm: TForm ): Boolean;
var
	pff: PFindForm;
begin
	Result := false;
	pff := TopFindForm;
	while CheckPointer( pff ) do
	begin
		if ( pff.Handle = AForm.Handle ) then
		begin
			Result := true;
			Exit;
		end;
		pff := pff.Next;
	end;
end;

{--------------------------- Form Search Methods ---------------------------}

function FormByHandle( Handle: HWnd ): TForm;
var
	c: TWinControl;
begin
	c := FindControl( Handle );
	if ( c is TForm ) then
		Result := ( c as TForm )
	else
		Result := nil;
end;

procedure FormShowOnce( FormClass: TFormClass );
var
	frm: TForm;
begin
	frm := FindFirstForm( FormClass );
	try
		if ( not CheckObject( frm ) ) then
			frm := FormClass.Create( Application );
	finally
		CloseFindForm;
	end;
	frm.Show;
end;

function FormShowModal( FormClass: TFormClass ): TModalResult;
var
	frm: TForm;
begin
	frm := FormClass.Create( Application );
	try
		Result := frm.ShowModal;
	finally
		frm.Free;
	end;
end;

procedure CloseFindForm;
var
	pff: PFindForm;
begin
{ we expect the top and bottom list pointers to be in sync, and become
	nil at the same time (preferrably, during this clean-up loop) }
	while Check( [TopFindForm, BottomFindForm] ) do
	begin
{ get the top of the list }
		pff := TopFindForm;
{ move the top to the next item }
		TopFindForm := TopFindForm.Next;
{ if we've reached the bottom, clear the bottom pointer }
		if ( pff = BottomFindForm ) then
			BottomFindForm := nil;
{ free this item }
		Dispose( pff );
	end;
	if CheckPointer( TopFindForm ) then
		RaiseException( EKSYUtils, sErrFindLeak );
end;

function FindFirstForm( FormClass: TFormClass ): TForm;
var
	i: Integer;
begin
	CloseFindForm;
	for i := 0 to Pred( Screen.FormCount ) do
		if ( Screen.Forms[i] is FormClass ) then
		begin
			Result := Screen.Forms[i];
			FindFormInclude( Result.Handle );
			Exit;
		end;
	Result := nil;
end;

function FindNextForm( FormClass: TFormClass ): TForm;
var
	i: Integer;
begin
	if ( not ( CheckPointer( TopFindForm ) and
			 CheckPointer( BottomFindForm ) ) ) then
		RaiseException( EKSYUtils, sErrFindNext );
	for i := 0 to Pred( Screen.FormCount ) do
		if ( Screen.Forms[i] is FormClass ) and
			 ( not FindFormExists( Screen.Forms[i] ) ) then
		begin
			Result := Screen.Forms[i];
			FindFormInclude( Result.Handle );
			Exit;
		end;
	Result := nil;
end;

function GetBaseOwnerComponent( AComp: TComponent;
	RestrictOwnerClass: TComponentClass ): TComponent;
var
	cp: TComponent;
begin
	Result := nil;
	if ( not CheckObject( AComp ) ) then
		Exit;
	cp := AComp.Owner;
	Result := cp;
	while ( cp <> nil ) and ( ( not CheckClass( RestrictOwnerClass ) ) or
	  CheckObjectClass( cp, RestrictOwnerClass ) ) do
	begin
		Result := cp;
		cp := cp.Owner;
	end;	
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic Debug Routines ----------------------------
--------------------------------------------------------------------------------
}

{.$IFDEF DEBUG}

var
	dlmfs: TFileStream = nil;

function DebugLogSnapshot( const FileName: string ): Boolean;
var
	fs: TFileStream;
begin
	Result := false;
	if ( not CheckObject( dlmfs ) ) then
		Exit;
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	fs := TFileStream.Create( FileName, fmCreate );
	try
		ForceStreamCopy( dlmfs, fs );
		dlmfs.Size := 0;
		Result := true;
	finally
		fs.Free;
	end;
end;

function DebugLogSnapshotFmt( const FileName: string; const Args: array of const ): Boolean;
begin
	Result := DebugLogSnapshot( Format( FileName, Args ) );
end;

procedure UniqueDebugLogSnapshot( const FileName: string );
begin
	DebugLogSnapshot( GetUniqueFileName( FileName ) );
end;

procedure UniqueDebugLogSnapshotFmt( const FileName: string; const Args: array of const );
begin
	DebugLogSnapshot( GetUniqueFileNameFmt( FileName, Args ) );
end;

function DebugLogClear: Integer;
begin
	Result := -1;
	if CheckObject( dlmfs ) then
	begin
		Result := dlmfs.Size;
		dlmfs.Size := 0;
	end;
end;

function DebugLogClearEx( const FileName: string ): Integer;
var
  fs: TFileStream;
begin
	Result := -1;
	if CheckFile( FileName ) then
	begin
		fs := TFileStream.Create( FileName, fmOpenWrite or fmShareExclusive );
		try
		  Result := fs.Size;
			fs.Size := 0;
		finally
			fs.Free;
		end;
	end;
end;

procedure DebugLogMessageEx( const FileName, Message: string );
const
	FILE_FLAGS: array[Boolean] of Word =
		( fmCreate or fmShareDenyWrite, fmOpenWrite or fmShareDenyWrite );
var
	sMsg: string;
	fs: TFileStream;
begin
	ForceTrimStr( FileName );
	if CheckStr( Message ) then
	begin
		fs := TFileStream.Create( FileName, FILE_FLAGS[CheckFile( FileName )] );
		try
			sMsg := Message + CH_CRLF;
			fs.Seek( 0, soFromEnd );
			fs.WriteBuffer( Pointer( sMsg )^, Length( sMsg ) );
		finally
			fs.Free;
		end;
	end;
end;

procedure DebugLogMessage( const Message: string );
var
	sMsg: string;
begin
	if ( not CheckObject( dlmfs ) ) and ( CheckStr( Message ) ) then
	begin
{
There is a very special case for debugging: between exe/dll based inter debugging.
The reference of the object dlmfs are cleared into the dll (suposed that the exe
write first to the file). But the file *MUST* be created with fmShareDenyWrite.
So we will use two files instead! see DebugLogMessageEx...
}
		ForceDeleteFile( ApplicationLikeName( DEBUG_EXT ) );
		dlmfs := TFileStream.Create( ApplicationLikeName( DEBUG_EXT ),
			fmCreate or fmShareDenyWrite );
	end;
	if CheckStr( Message ) then
	begin
		sMsg := Message + CH_CRLF;
		dlmfs.WriteBuffer( Pointer( sMsg )^, Length( sMsg ) );
	end;
end;

procedure DebugShowMessage( const Message: string );
begin
	MessageBox( 0, PChar( Message ), PChar( DEBUG_TITLE ), MB_OK );
end;

procedure DebugLogMessageFmt( const Message: string; const Args: array of const );
begin
	DebugLogMessage( Format( Message, Args ) );
end;

procedure DebugLogMessageExFmt( const FileName, Message: string; const Args: array of const );
begin
	DebugLogMessageEx( FileName, Format( Message, Args ) );
end;

procedure DebugShowMessageFmt( const Message: string; const Args: array of const );
begin
	DebugShowMessage( Format( Message, Args ) );
end;

{.$ENDIF}

{
--------------------------------------------------------------------------------
------------------- Generic Pointer/Object Checking Routines -------------------
--------------------------------------------------------------------------------
}

function ProcessArray( const Source: array of const; IsTrim: Boolean ): Integer;
var
	bOK: Boolean;
	StrProc: TKGetStrFunc;
	PCharProc: TKGetPCharFunc;
begin
	bOK := true;
	if IsTrim then
	begin
		StrProc := @CheckTrimStr;
		PCharProc := @CheckTrimPChar;
	end
	else
	begin
		StrProc := @CheckStr;
		PCharProc := @CheckPChar;
	end;
	for Result := Low( Source ) to High( Source ) do
	begin
		with TVarRec( Source[Result] ) do
			case VType of
				vtChar:
					bOK := bOK and StrProc( VChar );
				vtstring:
					bOK := bOK and StrProc( Vstring^ );
				vtWideChar:
					bOK := bOK and StrProc( VWideChar );
				vtPWideChar:
					bOK := bOK and StrProc( VPWideChar^ );
				vtVariant:
					bOK := bOK and CheckVariant( VVariant^ );
				vtPointer:
					bOK := bOK and CheckPointer( VPointer );
				vtPChar:
					bOK := bOK and PCharProc( VPChar );
				vtObject:
					bOK := bOK and CheckObject( VObject );
				vtClass:
					bOK := bOK and CheckReference( VClass );
				vtAnsistring:
					bOK := bOK and StrProc( PChar( VAnsistring ) );
				vtInterface:
					bOK := bOK and CheckInterface( IUnknown( VInterface ) );
				vtWidestring :
					bOK := bOK and StrProc( Widestring( VWidestring ) );
			else
				bOK := false;
			end;
		if ( not bOK ) then
			Exit;
	end;
  Result := -1;
end;

{------------------------- Smart Check/Force Routines --------------------------}

function Check( const Source: array of const ): Boolean;
begin
	Result := ( ProcessArray( Source, false ) = -1 );
end;

function CheckTrim( const Source: array of const ): Boolean;
begin
	Result := ( ProcessArray( Source, true ) = -1 );
end;

procedure Force( const Source: array of const );
var
	i: Integer;
begin
	i := ProcessArray( Source, false );
	if ( i <> -1 ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvElement, [i] );
end;

procedure ForceTrim( const Source: array of const );
var
	i: Integer;
begin
	i := ProcessArray( Source, true );
	if ( i <> -1 ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvElement, [i] );
end;

{------------------------ Generic Check/Force Routines -------------------------}

function CheckWindow( Source: HWnd ): Boolean;
begin
	Result := IsWindow( Source );
end;

function CheckHandle( Source: THandle ): Boolean;
begin
	Result := ( Source <> INVALID_HANDLE_VALUE ) and ( Source <> NULL_HANDLE_VALUE );
end;

function CheckSocket( Source: TSocket ): Boolean;
begin
  Result := ( Source <> INVALID_SOCKET );
end;

function CheckWin32Library( Source: HModule ): Boolean;
begin
{
	In a call to GetModuleHandle, or LoadLibrary,
		Result := ( Source <> NULL_HANDLE_VALUE );
	In a call to LoadModule,
		Result := not ( ( Source < HINSTANCE_ERROR ) and
										( Source >= NULL_HANDLE_VALUE ) );
}
	Result := ( Source <> NULL_HANDLE_VALUE );
end;

function CheckVariant( Source: Variant ): Boolean;
begin
	Result := ( not VarIsEmpty( Source ) );
end;

function CheckVarArray( Source: Variant ): Boolean;
begin
	Result := ( CheckVariant( Source ) and VarIsArray( Source ) );
end;

function CheckVariantType( Source: Variant; VType: Integer ): Boolean;
begin
	Result := ( VarType( Source ) = VType );
end;

function CheckVarArrayType( Source: Variant; VType: Integer ): Boolean;
begin
	Result := ( CheckVarArray( Source ) and ( VarType( Source ) = VType ) );
end;

function CheckPointer( Source: Pointer ): Boolean;
begin
	Result := ( Source <> nil );
end;

function CheckReference( const Source ): Boolean;
begin
	Result := ( Pointer( Source ) <> nil );
end;

function CheckFile( const Source: string ): Boolean;
begin
	Result := FileExists( Source );
end;

function CheckPath( const Source: string ): Boolean;
var
	iCode: Integer;
begin
	iCode := GetFileAttributes( PChar( Source ) );
	Result := ( iCode <> -1 ) and ( ( FILE_ATTRIBUTE_DIRECTORY and iCode ) <> 0 );
end;

function CheckStr( const Source: string ): Boolean;
begin
	Result := ( Source <> '' ) or ( Length( Source ) > 0 );
end;

function CheckPChar( Source: PChar ): Boolean;
begin
	Result := CheckPointer( Source ) and ( StrLen( Source ) > 0 );
end;

function CheckTrimStr( const Source: string ): Boolean;
begin
	Result := ( Trim( Source ) <> '' ) or ( Length( Trim( Source ) ) > 0 );
end;

function CheckTrimPChar( Source: PChar ): Boolean;
begin
	Result := CheckPChar( Source ) and CheckTrimStr( Source );
end;

procedure ForceWindow( Source: HWnd );
begin
	if ( not CheckWindow( Source ) ) then
		RaiseException( EKSYUtils, sErrInvWindow );
end;

procedure ForceHandle( Source: THandle );
begin
	if ( not CheckHandle( Source ) ) then
		RaiseException( EKSYUtils, sErrInvHandle );
end;

procedure ForceSocket( Source: TSocket );
begin
	if ( not CheckSocket( Source ) ) then
		RaiseException( EKSYUtils, sErrInvSocket );
end;

procedure ForceWin32Library( Source: HModule );
begin
	if ( not CheckWin32Library( Source ) ) then
		RaiseException( EKSYUtils, sErrInvWin32LibHandle );
end;

procedure ForceVariant( Source: Variant );
begin
	if ( not CheckVariant( Source ) ) then
		RaiseException( EKSYUtils, sErrInvVariant );
end;

procedure ForceVarArray( Source: Variant );
begin
	if ( not CheckVarArray( Source ) ) then
		RaiseException( EKSYUtils, sErrInvVarArray );
end;

procedure ForceVariantType( Source: Variant; VType: Integer );
begin
	if ( not CheckVariantType( Source, VType ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvVarType, [VType, VarType( Source )] );
end;

procedure ForceVarArrayType( Source: Variant; VType: Integer );
begin
	if ( not CheckVarArrayType( Source, VType ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvVarArrayType, [VType, VarType( Source )] );
end;

procedure ForcePointer( Source: Pointer );
begin
	if ( not CheckPointer( Source ) ) then
		RaiseException( EKSYUtils, sErrInvPointer );
end;

procedure ForceReference( const Source );
begin
	if ( not CheckReference( Source ) ) then
		RaiseException( EKSYUtils, sErrInvReference );
end;

procedure ForceFile( const Source: string );
begin
	if ( not CheckFile( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvFile, [Source] );
end;

procedure ForcePath( const Source: string );
begin
	if ( not CheckPath( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvPath, [Source] );
end;

procedure ForceStr( const Source: string );
begin
	if ( not CheckStr( Source ) ) then
		RaiseException( EKSYUtils, sErrInvStr );
end;

procedure ForcePChar( Source: PChar );
begin
	if ( not CheckPChar( Source ) ) then
		RaiseException( EKSYUtils, sErrInvPChar );
end;

procedure ForceTrimStr( const Source: string );
begin
	if ( not CheckTrimStr( Source ) ) then
		RaiseException( EKSYUtils, sErrInvTrimStr );
end;

procedure ForceTrimPChar( Source: PChar );
begin
	if ( not CheckTrimPChar( Source ) ) then
		RaiseException( EKSYUtils, sErrInvTrimStr );
end;

{------------------------ Object Check/Force Routines --------------------------}

function CheckObject( Source: TObject ): Boolean;
begin
	Result := Assigned( Source );
end;

function CheckClass( Source: TClass ): Boolean;
begin
	Result := CheckPointer( Pointer( Source ) );
end;

function CheckList( Source: TList ): Boolean;
begin
	Result := CheckObject( Source ) and ( Source.Count > 0 );
end;

function CheckStream( Source: TStream ): Boolean;
begin
	Result := CheckObject( Source ) and ( Source.Size > 0 );
end;

function CheckStrings( Source: TStrings ): Boolean;
begin
	Result := CheckObject( Source ) and ( Source.Count > 0 );
end;

function CheckCollection( Source: TCollection ): Boolean;
begin
	Result := CheckObject( Source ) and ( Source.Count > 0 );
end;

function CheckMethod( Source: TMethod ): Boolean;
begin
	Result := CheckPointer( Source.Code ) and CheckPointer( Source.Data );
end;

function CheckObjectClass( Source: TObject; AClass: TClass ): Boolean;
begin
	Result := CheckObject( Source ) and CheckClass( AClass ) and ( Source is AClass );
end;

function CheckObjectClasses( Source: TObject; const Classes: array of TClass ): Boolean;
var
  i: Integer;
begin
  Result := CheckObject( Source );
  if Result then
    for i := Low( Classes ) to High( Classes ) do
      Result := ( Result or CheckObjectClass( Source, Classes[i] ) );
end;

function CheckClassReference( Source, AClass: TClass ): Boolean;
begin
	Result := CheckClass( Source ) and CheckClass( AClass ) and Source.InheritsFrom( AClass );
end;

function CheckObjectClassName( Source: TObject; AClassName: ShortString ): Boolean;
begin
	Result := CheckObject( Source ) and CheckTrimStr( AClassName ) and Source.ClassNameIs( AClassName );
end;

function CheckClassNameReference( Source: TClass; AClassName: ShortString ): Boolean;
begin
	Result := CheckClass( Source ) and CheckTrimStr( AClassName ) and Source.ClassNameIs( AClassName );
end;

procedure ForceObject( Source: TObject );
begin
	if ( not CheckObject( Source ) ) then
		RaiseException( EKSYUtils, sErrInvObject );
end;

procedure ForceClass( Source: TClass );
begin
	if ( not CheckClass( Source ) ) then
		RaiseException( EKSYUtils, sErrInvClass );
end;

procedure ForceList( Source: TList );
begin
	if ( not CheckList( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvList, [Source.ClassName] );
end;

procedure ForceStream( Source: TStream );
begin
	if ( not CheckStream( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStream, [Source.ClassName] );
end;

procedure ForceStrings( Source: TStrings );
begin
	if ( not CheckStrings( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrings, [Source.ClassName] );
end;

procedure ForceCollection( Source: TCollection );
begin
	if ( not CheckCollection( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvCollection, [Source.ClassName] );
end;

procedure ForceMethod( Source: TMethod );
begin
	if ( not CheckMethod( Source ) ) then
		RaiseException( EKSYUtils, sErrInvMethod );
end;

procedure ForceObjectClass( Source: TObject; AClass: TClass );
begin
	if ( not CheckObjectClass( Source, AClass ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvObjectClass,
		[Source.ClassName, AClass.ClassName] );
end;

procedure ForceObjectClasses( Source: TObject; const Classes: array of TClass );
begin
	if ( not CheckObjectClasses( Source, Classes ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvObjectClasses, [Source.ClassName] );
end;

procedure ForceClassReference( Source, AClass: TClass );
begin
	if ( not CheckClassReference( Source, AClass ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvClassReference,
		[Source.ClassName, AClass.ClassName] );
end;

procedure ForceObjectClassName( Source: TObject; AClassName: ShortString );
begin
	if ( not CheckObjectClassName( Source, AClassName ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvObjectClassName,
		[Source.ClassName, AClassName] );
end;

procedure ForceClassNameReference( Source: TClass; AClassName: ShortString );
begin
	if ( not CheckClassNameReference( Source, AClassName ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvClassNameReference,
		[Source.ClassName, AClassName] );
end;

{----------------------- Interface Check/Force Routines ------------------------}

function CreateGUID: string;
var
  g: TGUID;
begin
	Result := '';
	if ( CoCreateGuid( g ) = S_OK ) then
		Result := GUIDToString( g );
end;

function CheckInterface( Source: IUnknown ): Boolean;
begin
	Result := CheckPointer( Pointer( Source ) );
end;

function CheckInterfaceClass( Source: IUnknown; const GUID: TGUID ): Boolean;
const
	E_NOINTERFACE = $80004002;
var
	obj: IUnknown;
begin
	ForceInterface( Source );
	Result := ( Source.QueryInterface( GUID, obj ) <> E_NOINTERFACE ) and
		CheckReference( obj );
end;

procedure ForceInterface( Source: IUnknown );
begin
	if ( not CheckInterface( Source ) ) then
		RaiseException( EKSYUtils, sErrInvInterface );
end;

procedure ForceInterfaceClass( Source: IUnknown; const GUID: TGUID );
begin
	if ( not CheckInterfaceClass( Source, GUID ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvInterfaceClass, [GUIDToString( GUID )] );
end;

{------------------------- Plural Check/Force Routines -------------------------}

function CheckObjects( const Source: array of TObject ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckObject( Source[i] );
end;

function CheckHandles( const Source: array of THandle ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckHandle( Source[i] );
end;

function CheckInterfaces( const Source: array of IUnknown ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckInterface( Source[i] );
end;

function CheckVariants( const Source: array of Variant ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckVariant( Source[i] );
end;

function CheckVariantsType( const Source: array of Variant; VType: Integer ): Boolean;
var
	i: Integer;
begin
{
	The compiler constructs a Variant as a Integer VarType, either if Source are
	less than 255 (for byte for example), to prevent this problem, manually type
	cast is the solution.
}
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := ( Result and (
			( ( ( VType = varSmallInt ) or ( VType = varVariant ) ) and
				CheckVariantType( Source[i], varInteger ) and
				ValueBetween( TVarData( Source[i] ).VInteger, Low( SmallInt ), High( SmallInt ), True ) ) or
			( ( ( VType = varByte ) or ( VType = varVariant ) ) and
				CheckVariantType( Source[i], varInteger ) and
				ValueBetween( TVarData( Source[i] ).VInteger, Low( Byte ), High( Byte ), True ) ) or
			( VType = varVariant ) or CheckVariantType( Source[i], VType ) ) );
end;

function CheckVariantsTypes( const Source: array of Variant; const VTypes: array of Integer ): Boolean;
var
	i: Integer;
begin
	Result := ( High( Source ) = High( VTypes ) );
	for i := Low( Source ) to High( Source ) do
		Result := ( Result and (
			( ( VTypes[i] = varSmallInt ) and CheckVariantType( Source[i], varInteger ) and
				ValueBetween( TVarData( Source[i] ).VInteger, Low( SmallInt ),
				High( SmallInt ), True ) ) or
			( ( VTypes[i] = varByte ) and CheckVariantType( Source[i], varInteger ) and
				ValueBetween( TVarData( Source[i] ).VInteger, Low( Byte ), High( Byte ), True ) ) or
			CheckVariantType( Source[i], VTypes[i] ) ) );
end;

function CheckVarRecType( const Source: array of const; VarRecType: Byte ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := ( Result and ( TVarRec( Source[i] ).VType = VarRecType ) );
end;

function CheckVarRecTypes( const Source: array of const; const VarRecTypes: array of Byte ): Boolean;
var
	i: Integer;
begin
	Result := ( High( Source ) = High( VarRecTypes ) );
	for i := Low( Source ) to High( Source ) do
		Result := ( Result and ( TVarRec( Source[i] ).VType = VarRecTypes[i] ) );
end;

function CheckPointers( const Source: array of Pointer ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckPointer( Source[i] );
end;

function CheckStrs( const Source: array of string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckStr( Source[i] );
end;

function CheckPChars( const Source: array of PChar ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckPChar( Source[i] );
end;

function CheckTrimStrs( const Source: array of string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Source ) to High( Source ) do
		Result := Result and CheckTrimStr( Source[i] );
end;

function CheckFiles( const Files: array of string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Files ) to High( Files ) do
		Result := Result and CheckFile( Files[i] );
end;

function CheckPaths( const Paths: array of string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Paths ) to High( Paths ) do
		Result := Result and CheckFile( Paths[i] );
end;

procedure ForceFiles( const Files: array of string );
begin
	if ( not CheckFiles( Files ) ) then
		RaiseException( EKSYUtils, sErrInvFileList );
end;

procedure ForcePaths( const Paths: array of string );
begin
	if ( not CheckPaths( Paths ) ) then
		RaiseException( EKSYUtils, sErrInvPathList );
end;

procedure ForceObjects( const Source: array of TObject );
begin
	if ( not CheckObjects( Source ) ) then
		RaiseException( EKSYUtils, sErrInvObject );
end;

procedure ForceHandles( const Source: array of THandle );
begin
	if ( not CheckHandles( Source ) ) then
		RaiseException( EKSYUtils, sErrInvHandle );
end;

procedure ForceInterfaces( const Source: array of IUnknown );
begin
	if ( not CheckInterfaces( Source ) ) then
		RaiseException( EKSYUtils, sErrInvInterface );
end;

procedure ForceVariants( const Source: array of Variant );
begin
	if ( not CheckVariants( Source ) ) then
		RaiseException( EKSYUtils, sErrInvVariant );
end;

procedure ForceVariantsType( const Source: array of Variant; VType: Integer );
begin
	if ( not CheckVariantsType( Source, VType ) ) then
		RaiseException( EKSYUtils, sErrInvVariant );
end;

procedure ForceVariantsTypes( const Source: array of Variant; const VTypes: array of Integer );
begin
	if ( not CheckVariantsTypes( Source, VTypes ) ) then
		RaiseException( EKSYUtils, sErrInvVariant );
end;

procedure ForceVarRecType( const Source: array of const; VarRecType: Byte );
begin
	if ( not CheckVarRecType( Source, VarRecType ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvVarRec, [VarRecType] );
end;

procedure ForceVarRecTypes( const Source: array of const; const VarRecTypes: array of Byte );
begin
	if ( not CheckVarRecTypes( Source, VarRecTypes ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvVarRec, [-1] );
end;

procedure ForcePointers( const Source: array of Pointer );
begin
	if ( not CheckPointers( Source ) ) then
		RaiseException( EKSYUtils, sErrInvPointer );
end;

procedure ForceStrs( const Source: array of string );
begin
	if ( not CheckStrs( Source ) ) then
		RaiseException( EKSYUtils, sErrInvStr );
end;

procedure ForcePChars( const Source: array of PChar );
begin
	if ( not CheckPChars( Source ) ) then
		RaiseException( EKSYUtils, sErrInvPChar );
end;

procedure ForceTrimStrs( const Source: array of string );
begin
	if ( not CheckTrimStrs( Source ) ) then
		RaiseException( EKSYUtils, sErrInvTrimStr );
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Memory Routines ----------------------------
--------------------------------------------------------------------------------
}

function CheckHeap: Boolean;
var
	hs: THeapStatus;
begin
	ZeroMemory( @hs, SizeOf( THeapStatus ) );
	hs := GetHeapStatus;
	Result := ( ( hs.TotalFree - ( hs.FreeSmall + hs.FreeBig + hs.Unused ) ) = 0 );
end;

procedure ZeroMemory( Source: Pointer; Count: Integer );
begin
	ForcePointer( Source );
	FillChar( Source^, Count, 0 );
end;

function IncPtr( Source: Pointer; Count: Integer ): Pointer;
begin
	Result := Pointer( LongInt( Source ) + Count );
end;

function ForceIncPtr( Source: Pointer; Count: Integer ): Pointer;
begin
	ForcePointer( Source );
	Result := IncPtr( Source, Count );
end;

function HostEntToIP( he: PHostEnt ): Integer;
begin
  Result := Integer( HostEntToIPAddr( he ) );
end;

function HostEntToIPAddr( he: PHostEnt ): TInAddr;
begin
  FillChar( Result, SizeOf( TInAddr ), INADDR_NONE );
  if ( CheckPointer( he ) and CheckPointer( he^.h_addr ) and
    ( StrLen( he^.h_addr^ ) > 3 ) ) then
    with Result.S_un_b, he^ do
    begin
      s_b1 := h_addr^[0];
      s_b2 := h_addr^[1];
      s_b3 := h_addr^[2];
      s_b4 := h_addr^[3];
    end;
end;

procedure StartupWinSock;
var
  WSAData: TWSAData;
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup( $0101, WSAData );
  if ( ErrorCode <> 0 ) then
    RaiseException( EKSYUtils, LoadStr( ErrorCode ) );
end;

procedure CleanupWinSock;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ( ErrorCode <> 0 ) then
    RaiseException( EKSYUtils, LoadStr( ErrorCode ) );
end;

function ScanMem( const Token, Source; TokenSize, SourceSize: Cardinal ): Cardinal; assembler;
const
	STACK_OFFSET = ( 8 + 3 * SizeOf( Integer ) );
{
	->EAX			Pointer to Token
		EDX			Pointer to Source
		ECX			TokenSize
		ESP+8   SourceSize
	<-EAX			Position of Token in Source or 0
}
{
	Just before the user call this function, the Compiler pushed SourceSize parameter
	in the stack. But when the function is called, the Compiler pushed the return
	address of the next instruction from the caller.
	The "asm" statement generates two asm lines, PUSH EBP, ESP and MOV EBP, ESP.
	So the stack until this moment will have, EBP, RET Addr, SourceSize. Since the
	stack addresses are 32 bits (processor word) based, we must get the pointer of
	the second (zero based index) address. So ESP + 8 !!!
	At the end of the procedure, the Compiler will pop EBP and restores the Stacks
	calling RET 4 (so return the stack -4 to the caller), doing so it frees the
	passed parameter.
}
asm
				TEST    EAX,EAX     				  { If token is nil, then return EAX (also nil = 0) }
				JE      @@NullToken

				TEST		EDX,EDX
				JE 			@@NullSource

				PUSH    EBX
				PUSH    ESI
				PUSH    EDI

				MOV     ESI,EAX          	   	{ Point ESI to Token      		  	}
				MOV     EDI,EDX       		  	{ Point EDI to Source     		 	  }
				MOV     EBX,ESP+STACK_OFFSET  { Point EBX to SourceSize 		  	}
				XCHG    ECX,EBX								{ Point ECX to SourceSize 		    }
																			{ Also points EBX to Token Size   }
																			{ This is necessary because SCASB }
																			{ uses the ECX value and not EBX! }

				(*  EDX will never change!

				PUSH    EDI                   { Remember Source Position to Calculate Index 		}
																			{ And to restore the EDX pointer if search failed }
				*)

				DEC     EBX                   { EBX = TokenSize - 1      				  }
				JS      @@Fail                { < 0 ? return 0           				  }
				MOV     AL,[ESI]              { AL = first byte of Token 				  }
				INC     ESI                   { Point ESI to 2'nd byte of Token   }

				SUB     ECX,EBX               { #positions in Source to look at   }
																			{ = SourceSize - TokenSize + 1      }
				JLE     @@Fail

@@Loop:
				REPNE   SCASB
				JNE     @@Fail
				PUSH    ECX                   { Save outer loop counter           }
				PUSH    ESI                   { Save outer loop Token pointer     }
				PUSH    EDI                   { Save outer loop Source pointer    }

				MOV     ECX,EBX
				REPE    CMPSB
				POP     EDI                   { Restore outer loop Source pointer }
				POP     ESI                   { Restore outer loop Token pointer  }
				POP     ECX                   { Restore outer loop counter        }
				JE      @@Found
				JMP     @@Loop

@@NullSource:
				XOR 		EAX, EAX     					{ If source is nil, then clear EAX  }
				JMP     @@NullToken

@@Fail:
			//POP     EDX                   { get rid of saved s pointer    		}
				XOR     EAX, EAX
				JMP     @@Exit

@@Found:
			//POP     EDX                   { Restore pointer to first char of Source }
				MOV     EAX,EDI               { EDI points of char after match          }
				SUB     EAX,EDX               { The difference is the correct index     }

@@Exit:
				POP     EDI
				POP     ESI
				POP     EBX

@@NullToken:

end;

function GoToNextTokenBinPos( sm: TMemoryStream; Token: Pointer; TokenSize: Integer ): Integer;
var
	p: Pointer;
	iPos: Integer;
begin
	Result := -1;
	if ( CheckStream( sm ) and CheckPointer( Token ) and ( sm.Position <> sm.Size ) and
	     ( TokenSize > 0 ) ) then
	begin
		p := IncPtr( sm.Memory, sm.Position );
		iPos := Integer( ScanMem( Token^, p^, Cardinal( TokenSize ), Cardinal( sm.Size - sm.Position ) ) );
		if ( iPos > 0 ) then
		begin
			Result := sm.Position; { Return the original position... }
			sm.Position := sm.Position + iPos - 1{+ TokenSize};
		end;
	end;
end;

function GoToNextTokenBinPosEx( Token, Source: Pointer; TokenSize, SourceSize: Integer ): Integer;
begin
	Result := -1;
	if ( CheckPointers( [Token, Source] ) and ( TokenSize > 0 ) and ( SourceSize > 0 ) ) then
		Result := ScanMem( Token^, Source^, TokenSize, SourceSize ) - 1;
end;

function PosEx( const SubStr, s: string; Count: Integer ): Integer;
var
	i: Integer;
	s1: string;
begin
	Result := 0;
	if ( Count <= 0 ) then
		Exit;
	s1 := s;
	repeat
		i := Pos( SubStr, s1 );
		Inc( Result, i );
		Dec( Count );
		Delete( s1, 1, i + Length( SubStr ) - 1 );
		if ( Count > 0 ) then
			Inc( Result, Length( SubStr ) - 1 );
	until ( ( Count <= 0 ) or ( i <= 0 ) or ( not CheckStr( s1 ) ) );
	if ( ( Count > 0 ) or ( i <= 0 ) ) then
		Result := 0;
end;

function CompareMemBytes( P1, P2: Pointer; Size: Integer ): ShortInt; assembler;
asm
				PUSH    ESI
				PUSH    EDI
				MOV     ESI,P1
				MOV     EDI,P2
				MOV     EDX,ECX
				XOR     EAX,EAX
				AND     EDX,3
				SHR     ECX,1
				SHR     ECX,1
				REPE    CMPSD
				JA      @@1
				JB	    @@2
				MOV     ECX,EDX
				REPE    CMPSB
				JZ      @@3
				JB      @@2
@@1:    INC     EAX
				JMP		  @@3
@@2:    DEC     EAX
@@3:    POP     EDI
				POP     ESI
end;

function CopyMemGlobalData( Handle: HGlobal ): HGlobal;
var
	Src,
	Dest: PChar;
	Size: Integer;
begin
	if CheckHandle( Handle ) then
	begin
		Size := GlobalSize( Handle );
		Result := GlobalAlloc( GHND, Size );
		try
			if CheckHandle( Result ) then
			begin
				Src := GlobalLock( Handle );
				try
					try
						Dest := GlobalLock( Result );
						if Check( [Src, Dest] ) then
							Move( Src^, Dest^, Size );
					finally
						GlobalUnlock( Result );
					end;
				finally
					GlobalUnlock( Handle );
				end;
			end;
		except
			GlobalFree( Result );
			raise;
		end;
	end
	else
		Result := NULL_HANDLE_VALUE;
end;

function StringToArray( const sIn: string; Token: Char; sOut: PStringArray ): Integer;
var
	i: Integer;
	sl: TStrings;
begin
	Result := -1;
	if ( not CheckTrimStr( sIn ) ) then
		Exit;
	sl := TStringList.Create;
	try
		ExtractStrings( sIn, Token, sl );
		if ( not CheckPointer( sOut ) ) then
			Result := sl.Count
		else { will assume enough memory in sOut }
		begin
			ZeroMemory( sOut, SizeOf( string ) * sl.Count );
			for i := 0 to sl.Count - 1 do
				sOut^[i] := sl[i];
			Result := sl.Count;	
		end;
	finally
	  sl.Free;
	end;
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic GUI Routines ------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const
	DISPLAY_ELEMENT: array[TKDisplayElement] of Integer =
	(
		COLOR_3DDKSHADOW,
		COLOR_3DLIGHT,
		COLOR_ACTIVEBORDER,
		COLOR_ACTIVECAPTION,
		COLOR_APPWORKSPACE,
		COLOR_BACKGROUND,
		COLOR_BTNFACE,
		COLOR_BTNHIGHLIGHT,
		COLOR_BTNSHADOW,
		COLOR_BTNTEXT,
		COLOR_CAPTIONTEXT,
		COLOR_GRAYTEXT,
		COLOR_HIGHLIGHT,
		COLOR_HIGHLIGHTTEXT,
		COLOR_INACTIVEBORDER,
		COLOR_INACTIVECAPTION,
		COLOR_INACTIVECAPTIONTEXT,
		COLOR_INFOBK,
		COLOR_INFOTEXT,
		COLOR_MENU,
		COLOR_MENUTEXT,
		COLOR_SCROLLBAR,
		COLOR_WINDOW,
		COLOR_WINDOWFRAME,
		COLOR_WINDOWTEXT
	);

{---------------------------- Public Implementation ----------------------------}

function TextBlock( const Source: string; ACanvas: TCanvas; MaxLen: Cardinal ): string;

	function InternalTextWidth( const s: string ): Integer;
	begin
		if CheckObject( ACanvas ) then
			Result := ACanvas.TextWidth( s )
		else
		  Result := Length( s );	
	end;

var
	s: string;
	i: Integer;
begin
	Result := '';
	ForceTrimStr( Source );
	i := 1;
	s := Source[1];
	MaxLen := MulDiv( MaxLen, DEFAULT_TEXT_BLOCK_PRECISION, 100 );
	while ( Length( Source ) >= i ) do
	begin
		Inc( i );
		{ Here we must TypeCast MaxLen to Integer because in Delphi4 cardinal
			violates the Integer Range and generates a warning here. Even, Length and
			TextWidth also returns Integer ranged values... }
		while ( ( Length( Source ) >= i ) and ( ( s[Length( s )] in [CH_CR, CH_LF, CH_TAB] ) or
			( InternalTextWidth( s ) < Integer( MaxLen ) ) ) ) do
		begin
			s := s + Source[i];
			Inc( i );
		end;
		Result := Result + s + CH_CRLF;
		if ( Length( Source ) >= i ) then
			s := Source[i];
	end;
	if ( CheckStr( Result ) and ( Result[Length( Result )] = CH_LF ) and
		 ( Result[Length( Result ) - 1] = CH_CR ) ) then
		Delete( Result, Length( Result ) - 1, Length( CH_CRLF ) );
end;

function PointDivBy( Point: TPoint; DivBy: Integer ): TPoint;
begin
	with Point do
  begin
    Result.x := x div DivBy;
    Result.y := y div DivBy;
  end;
end;

function PointMulBy( Point: TPoint; MulBy: Integer ): TPoint;
begin
	with Point do
  begin
		Result.x := x * MulBy;
    Result.y := y * MulBy;
	end;
end;

function RectDivBy( Rect: TRect; DivBy: Integer ): TRect;
begin
  with Rect do
	begin
		Result.TopLeft := PointDivBy( TopLeft, DivBy );
		Result.BottomRight := PointDivBy( BottomRight, DivBy );
	end;
end;

function RectMulBy( Rect: TRect; MulBy: Integer ): TRect;
begin
  with Rect do
	begin
    Result.TopLeft := PointMulBy( TopLeft, MulBy );
		Result.BottomRight := PointMulBy( BottomRight, MulBy );
  end;
end;

procedure CenterWindow( Wnd: HWnd );
var
	Rect: TRect;
begin
	GetWindowRect( Wnd, Rect );
	SetWindowPos( Wnd, 0,
		( GetSystemMetrics( SM_CXSCREEN ) - Rect.Right + Rect.Left ) div 2,
		( GetSystemMetrics( SM_CYSCREEN ) - Rect.Bottom + Rect.Top ) div 3,
		0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER );
end;

function GetAveCharSizeDC( DC: HDC ): TPoint;
var
	i: Integer;
	Buffer: array[0..51] of Char;
begin
	ForceHandle( DC );
	for i := 0 to 25 do
		Buffer[i] := Chr( i + Ord( 'A' ) );
	for i := 0 to 25 do
		Buffer[i + 26] := Chr( i + Ord( 'a' ) );
	GetTextExtentPoint( DC, Buffer, 52, TSize( Result ) );
	Result.X := Result.X div 52;
end;

function GetAveCharSize( Canvas: TCanvas ): TPoint;
begin
	Result := GetAveCharSizeDC( Canvas.Handle );
end;

function DisplayColors: LongInt;
var
	dw: Hwnd;
	DesktopDC: HDC;
	ColorPlanes,
	BitsPerPixel: LongInt;
begin
	Result := -1;
	dw := GetDesktopWindow;
	DesktopDC := GetDC( dw ) ;
	try
		ColorPlanes := GetDeviceCaps( DesktopDC, PLANES ) ;
		BitsPerPixel := GetDeviceCaps( DesktopDC, BITSPIXEL ) ;
		Result := Result shl ( ColorPlanes * BitsPerPixel );
	finally
		ReleaseDC( dw, DesktopDC ); 
	end;
end;

function GetDesktopWorkArea: TRect;
begin
	ZeroMemory( @Result, SizeOf( TRect ) );
	SystemParametersInfo( SPI_GETWORKAREA, 0, @Result, 0 );
end;

function EnumFontsProc( var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer ): Integer; stdcall;
var
	S: TStrings;
	sTemp: string;
begin
	S := TStrings( Data );
	sTemp := LogFont.lfFaceName;
	if ( S.Count = 0 ) or ( AnsiCompareText( S[S.Count-1], sTemp ) <> 0 ) then
		S.Add( sTemp );
	Result := 1;
end;

procedure GetFonts( DC: HDC; Items: TStrings );
var
	LFont: TLogFont;
begin
	Items.BeginUpdate;
	try
		Items.Clear;
		if CheckObjectClass( Items, TStringList ) then
			with TStringList( Items ) do
			begin
				Sorted := true;
				Duplicates := dupIgnore;
			end;
		ZeroMemory( @LFont, SizeOf( TLogFont ) );
		LFont.lfCharset := DEFAULT_CHARSET;
		EnumFontFamiliesEx( DC, LFont, @EnumFontsProc, LongInt( Items ), 0 );
	finally
		Items.EndUpdate;
	end;
end;

procedure GetScreenFonts( Items: TStrings );
var
	DC: HDC;
begin
  ForceObject( Items );
	DC := GetDC( 0 );
	try
		GetFonts( DC, Items );
	finally
		ReleaseDC( 0, DC );
	end;
end;

procedure GetPrinterFonts( Items: TStrings );
begin
	ForceObject( Items );
	GetFonts( Printer.Handle, Items );
end;

{ Window Enumeration Routines }

var
	EnumWindowsVisibleMode: Boolean = false;

function EnumWindowsCallback( AHandle: HWnd; LParam: Integer ): Boolean; stdcall;
begin
	Result := true;
	if ( ( not EnumWindowsVisibleMode ) or IsWindowVisible( AHandle ) ) then
		TList( LParam ).Add( Pointer( AHandle ) );
end;

procedure EnumVisibleWindows( AList: TList );
begin
	ForceObject( AList );
	AList.Clear;
	EnumWindowsVisibleMode := true;
	Windows.EnumWindows( @EnumWindowsCallback, Integer( AList ) );
end;

procedure EnumWindows( AList: TList );
begin
	ForceObject( AList );
	AList.Clear;
	EnumWindowsVisibleMode := false;
	Windows.EnumWindows( @EnumWindowsCallback, Integer( AList ) );
end;

function GetShellTrayWindow: HWnd;
var
	ls: TList;
	i,
	c: Integer;
	s: string;
begin
	Result := HWnd( -1 );
	ls := TList.Create;
	try
		EnumWindows( ls );
		for i := 0 to ls.Count - 1 do
		begin
			SetLength( s, 255 );
			c := GetClassName( Integer( ls[i] ), PChar( s ), 255 );
			if ( c > 0 ) then
			begin
				SetLength( s, c );
				if CheckStrEqual( s, 'Shell_TrayWnd' ) then { do not resource }
				begin
					Result := Integer( ls[i] );
					Exit;
				end;
			end;
		end;
	finally
		ls.Free;
	end
end;

type
	TKTimerSHack = class( TComponent )
	private
{$HINTS OFF}
		FInterval: Cardinal;
{$HINTS ON}
		FWindowHandle: HWND;

	end;

function GetTimerHwnd( Timer: TTimer ): HWND;
begin
	if CheckObject( Timer ) then
		Result := TKTimerSHack( Timer ).FWindowHandle
	else
	  Result := INVALID_HANDLE_VALUE;	
end;

{ Palette routines }

function LogPaletteIndex( Index: Word ): TColor;
begin
	Result := PaletteIndex( Index );
end;

function LogPaletteRGB( r, g, b: Byte ): TColor;
begin
	Result := PaletteRGB( r, g, b );
end;

function GetSystemColor( de: TKDisplayElement ): TColor;
begin
	Result := GetSysColor( DISPLAY_ELEMENT[de] );
end;

procedure SetSystemColor( de: TKDisplayElement; Value: TColor );
begin
	SetSysColors( 1, DISPLAY_ELEMENT[de], Value );
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Variant Routines ---------------------------
--------------------------------------------------------------------------------
}

function ArrayOfConstToVar( const Args: array of const ): Variant;
var
	i: Integer;
begin
	Result := VarArrayCreate( [Low( Args ), High( Args )], varVariant );
	for i := Low( Args ) to High( Args ) do
		with TVarRec( Args[i] ) do
			case VType of
				vtInteger    : Result[i] := VInteger;
				vtBoolean    : Result[i] := VBoolean;
				vtChar       : Result[i] := VChar;
				vtExtended   : Result[i] := VExtended^;
				vtString     : Result[i] := VString^;
				vtPChar      : Result[i] := string( VPChar );
				vtWideChar   : Result[i] := VWideChar;
				vtPWideChar  : Result[i] := VPWideChar^;
				vtAnsiString : Result[i] := string( VAnsiString );
				vtCurrency   : Result[i] := VCurrency^;
				vtVariant    : Result[i] := VVariant^;
				vtInterface  : Result[i] := ( IUnknown( VInterface ) as IDispatch );
				vtWideString : Result[i] := WideString( VWideString );
				vtPointer    ,
				vtObject     ,
				vtClass      :
					RaiseExceptionFmt( EKSYUtils, sVariantConvertError, [VType] );
			end;
end;

function ArrayOfStringToVar( const Args: array of string ): Variant;
var
	i: Integer;
begin
	Result := VarArrayCreate( [Low( Args ), High( Args )], varVariant );
	for i := Low( Args ) to High( Args ) do
		Result[i] := Args[i];
end;

function GetFirstClass( const Args: array of TClass ): TClass;
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		if CheckClass( Args[i] ) then
		begin
			Result := Args[i];
			Exit;
		end;
	Result := nil;
end;

function GetFirstObject( const Args: array of TObject ): TObject;
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		if CheckObject( Args[i] ) then
		begin
			Result := Args[i];
			Exit;
		end;
	Result := nil;
end;

function GetFirstPointer( const Args: array of Pointer ): Pointer;
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		if CheckPointer( Args[i] ) then
		begin
			Result := Args[i];
			Exit;
		end;
	Result := nil;
end;

function VarItemCount( const V: Variant ): Integer;
var
	i,
	j: Integer;
begin
	if VarIsArray( V ) then
	begin
		Result := 0;
		i := VarArrayDimCount( V );
		for j := 1 to i do
			Inc( Result, ( VarArrayHighBound( V, j ) - VarArrayLowBound( V, j ) + 1 ) );
	end
	else
		Result := 1;
end;

function VarArrayOfType( const Source: array of Variant; VType: Integer ): Variant;
var
	i: Integer;
begin
	if ( ( VType = varError ) or ( VType = varString ) or ( VType = varTypeMask ) or
		 ( VType = varArray ) or ( VType = varByRef ) ) then
		RaiseException( EKSYUtils, sErrInvVariant );
	ForceVariantsType( Source, VType );
	Result := VarArrayCreate( [Low( Source ), High( Source )], VType );
	for i := Low( Source ) to High( Source ) do
		Result[i] := Source[I];
end;

function VarArrayToArrayOfType( const V: Variant; VType: Integer; p: Pointer ): Integer;
var
	i,
	j,
	k: Integer;
	W: Variant;
	bTypeArrayVar: Boolean;
begin
	ForceVarArray( V );
	if ( ( VType = varError ) or ( VType = varString ) or ( VType = varTypeMask ) or
		 ( VType = varArray ) or ( VType = varByRef ) ) then
		VType := varEmpty;
	if ( VarType( V ) and varTypeMask <> varVariant ) then
		ForceVarArrayType( V, VType )
	else if ( VType <> ( varArray or varVariant ) ) then { garantir que quem chamou sabe o que está fazendo }
		RaiseExceptionFmt( EKSYUtils, sErrInvVarArrayType, [( varVariant or varArray ), VType] );
  i := VarArrayDimCount( V );
	Result := VarItemCount( V );
	if ( not CheckPointer( p ) ) then
	begin
		case ( VarType( V ) and varTypeMask ) of
			varSmallint: Result := Result * SizeOf( SmallInt );
			varInteger : Result := Result * SizeOf( Integer );
			varSingle  : Result := Result * SizeOf( Single );
			varDouble  : Result := Result * SizeOf( Double );
			varCurrency: Result := Result * SizeOf( Currency );
			varDate    : Result := Result * SizeOf( Date );
			varOleStr  :
				for j := 1 to i do
					for k := VarArrayLowBound( V, j ) to VarArrayHighBound( V, j ) do
						if ( j = 1 ) then
							Inc( Result, StrWLen( TVarData( V[k] ).VOleStr ) + 1 )
						else
							Inc( Result, StrWLen( TVarData( V[j][k] ).VOleStr ) + 1 );
			varDispatch: Result := Result * SizeOf( IDispatch );
			varBoolean : Result := Result * SizeOf( Boolean );
			varVariant : Result := Result * SizeOf( Variant );
			varUnknown : Result := Result * SizeOf( IUnknown );
			varByte    : Result := Result * SizeOf( Byte );
		else
			Result := -1;
		end;
	end
	else
	begin
		{ After all can use, VarArrayLock/VarArrayUnLock... }
		bTypeArrayVar := ( VType = ( varArray or varVariant ) );
		for j := 1 to i do
			for k := VarArrayLowBound( V, j ) to VarArrayHighBound( V, j ) do
			begin
				if ( j = 1 ) then
					W := V[k]
				else
					W := V[j][k];
				if bTypeArrayVar then
				begin
					VarCopy( PVariant( p )^, W );
					p := IncPtr( p, SizeOf( TVarData ) );
				end
				else
					with TVarData( W ) do
						case ( VarType( W ) and varTypeMask ) of
							varSmallint:
							begin
								Move( VSmallint, p^, SizeOf( SmallInt ) );
								p := IncPtr( p, SizeOf( SmallInt ) );
							end;
							varInteger:
							begin
								Move( VInteger, p^, SizeOf( Integer ) );
								p := IncPtr( p, SizeOf( Integer ) );
							end;
							varSingle:
							begin
								Move( VSingle, p^, SizeOf( Single ) );
								p := IncPtr( p, SizeOf( Single ) );
							end;
							varDouble:
							begin
								Move( VDouble, p^, SizeOf( Double ) );
								p := IncPtr( p, SizeOf( Double ) );
							end;
							varCurrency:
							begin
								Move( VCurrency, p^, SizeOf( Currency ) );
								p := IncPtr( p, SizeOf( Currency ) );
							end;
							varDate:
							begin
								Move( VDate, p^, SizeOf( TDateTime ) ); { the VDate field has the same }
								p := IncPtr( p, SizeOf( TDateTime ) );  { size as TDateTime type...    }
							end;
							varOleStr:
							begin
								Move( VOleStr^, p^, ( StrWLen( VOleStr ) * SizeOf( WideChar ) ) );
								p := IncPtr( p, ( ( StrWLen( VOleStr ) + 1 ) * SizeOf( WideChar ) ) );
							end;
							varDispatch:
							begin
								Move( VDispatch^, p^, SizeOf( VDispatch ) );
								p := IncPtr( p, SizeOf( VDispatch ) );
							end;
							varBoolean:
							begin
								Move( VBoolean, p^, SizeOf( Boolean ) );
								p := IncPtr( p, SizeOf( Boolean ) );
							end;
							varUnknown:
							begin
								Move( VUnknown^, p^, SizeOf( VUnknown ) );
								p := IncPtr( p, SizeOf( VUnknown ) );
							end;
							varByte:
							begin
								Move( VByte, p^, SizeOf( Byte ) );
								p := IncPtr( p, SizeOf( Byte ) );
							end;
						else
							RaiseExceptionFmt( EKSYUtils, sErrInvVarType, [VType, VarType( W )] );
						end;
			end;
	end;
end;

function CnvVarArrayVariantToArrayOfType( const V: Variant; VType: Integer;
	CnvVarCallBack: TKCnvVarArrayVariantToArrayOfCallBack; UserData: Pointer ): Integer;
var
	i,
	iSz: Integer;
	pvc: PVariantArray;
begin
	ForceReference( @CnvVarCallBack );
	ForceVarArrayType( V, varArray or varVariant );
	iSz := VarArrayToArrayOfType( V, varArray or varVariant, nil );
	pvc := AllocMem( iSz );
	try
		Result := VarArrayToArrayOfType( V, varArray or varVariant, pvc );
		ForceVariantsType( Slice( pvc^, Result ), VType );
		{ If forceVariantaType succeed, the pvc^[i] does not violate the type boundaries
			and this type conversion is safe. This conversion is also needed because the
			same reason described above in CheckVariantsType. }
		if ( VType in [varByte, varSmallInt] ) then
			for i := 0 to Result - 1 do
			  pvc^[i] := VarAsType( pvc^[i], VType ); 
		for i := 0 to Result - 1 do
			if ( not CnvVarCallBack( UserData, i, VType, pvc^[i] ) ) then
				Exit;
	finally
		FreeMem( pvc, iSz );
	end;
end;

function CnvArrayToAll( UserData: Pointer; Index, VType: Integer;
  const V: Variant ): Boolean;
begin
	Result := True;
	ForceVariantType( V, VType );
	case VType of
		varSmallint:
		  PSmallIntArray( UserData )^[Index] := TVarData( V ).VSmallInt;
		varInteger:
			PIntegerArray( UserData )^[Index] := TVarData( V ).VInteger;
		varSingle:
			PSingleArray( UserData )^[Index] := TVarData( V ).VSingle;
		varDouble:
		  PDoubleArray( UserData )^[Index] := TVarData( V ).VDouble;
		varCurrency:
			PCurrencyArray( UserData )^[Index] := TVarData( V ).VCurrency;
		varDate:
			PDateTimeArray( UserData )^[Index] := TVarData( V ).VDate;
		varOleStr:
			TStrings( UserData ).Add( WideCharToString( TVarData( V ).VOleStr ) );
		varBoolean:
			TBits( UserData ).Bits[Index] := TVarData( V ).VBoolean;
		varByte:
			PByteArray( UserData )^[Index] := TVarData( V ).VByte;
		{
		varDispatch: ;
		varUnknown:  ;
		}
	else
		RaiseExceptionFmt( EKSYUtils, sErrInvVarType, [VType, VarType( V )] );
	end;
end;

function VarArrayToArrayOfSmallInt( const V: Variant; psi: PSmallIntArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varSmallInt, @CnvArrayToAll, psi )
	else
		Result := VarArrayToArrayOfType( V, varArray or varSmallInt, psi );
end;

function VarArrayToArrayOfInteger( const V: Variant; pi: PIntegerArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varInteger, @CnvArrayToAll, pi )
	else
		Result := VarArrayToArrayOfType( V, varArray or varInteger, pi );
end;

function VarArrayToArrayOfSingle( const V: Variant; ps: PSingleArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varSingle, @CnvArrayToAll, ps )
	else
		Result := VarArrayToArrayOfType( V, varArray or varSingle, ps );
end;

function VarArrayToArrayOfDouble( const V: Variant; pd: PDoubleArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varDouble, @CnvArrayToAll, pd )
	else
		Result := VarArrayToArrayOfType( V, varArray or varDouble, pd );
end;

function VarArrayToArrayOfCurrency( const V: Variant; pc: PCurrencyArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varCurrency, @CnvArrayToAll, pc )
	else
		Result := VarArrayToArrayOfType( V, varArray or varCurrency, pc );
end;

function VarArrayToArrayOfDateTime( const V: Variant; pdt: PDateTimeArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varDate, @CnvArrayToAll, pdt )
	else
		Result := VarArrayToArrayOfType( V, varArray or varDate, pdt );
end;

function VarArrayToArrayOfByte( const V: Variant; pb: PByteArray ): Integer;
begin
	if ( VarType( V ) and varTypeMask = varVariant ) then
		Result := CnvVarArrayVariantToArrayOfType( V, varByte, @CnvArrayToAll, pb )
	else
		Result := VarArrayToArrayOfType( V, varArray or varByte, pb );
end;

procedure VarArrayToBits( const V: Variant; b: TBits );
var
	i: Integer;
	pb: PBooleanArray;
begin
	ForceObject( b );
	ForceVarArray( V );
	if ( VarType( V ) and varTypeMask = varVariant ) then
	begin
		b.Size := ( VarArrayToArrayOfType( V, varArray or varVariant, nil ) div SizeOf( Variant ) );
		CnvVarArrayVariantToArrayOfType( V, varBoolean, @CnvArrayToAll, Pointer( b ) );
	end
	else
	begin
		b.Size := ( VarArrayToArrayOfType( V, varArray or varBoolean, nil ) div SizeOf( Boolean ) );
		pb := AllocMem( b.Size );
		try
			VarArrayToArrayOfType( V, varArray or varBoolean, pb );
			for i := 0 to b.Size - 1 do
				b[i] := pb^[i];
		finally
			FreeMem( pb, b.Size );
		end;
	end;
end;

procedure BitsToVarArray( b: TBits; var V: Variant );
var
	i: Integer;
begin
	ForceObject( b );
	if ( b.Size > 0 ) then
	begin
		V := VarArrayCreate( [0, b.Size - 1], varBoolean );
		for i := 0 to b.Size - 1 do
			V[i] := b[i];
	end
	else
		V := Null;
end;

procedure VarArrayToStrings( const V: Variant; sl: TStrings );
var
	iSz: Integer;
	pwc: PWideChar;
begin
	ForceObject( sl );
	ForceVarArray( V );
	sl.BeginUpdate;
	try
		sl.Clear;
		if ( VarType( V ) and varTypeMask = varVariant ) then
			CnvVarArrayVariantToArrayOfType( V, varOleStr, @CnvArrayToAll, Pointer( sl ) )
		else
		begin
			iSz := VarArrayToArrayOfType( V, varArray or varOleStr, nil );
			pwc := StrWAllocMem( iSz + 1 );
			try
				VarArrayToArrayOfType( V, varArray or varOleStr, pwc );
				PWideCharListToStrings( pwc, sl );
			finally
				StrWDispose( pwc );
			end;
		end;
	finally
		sl.EndUpdate;
	end;
end;

procedure StringsToVarArray( sl: TStrings; var V: Variant );
var
	i: Integer;
begin
	ForceStrings( sl );
	V := VarArrayCreate( [0, sl.Count - 1], varOleStr );
	for i := 0 to sl.Count - 1 do
{ PWideChars are automatic compatible with WideStrings as PChar are automatic
  compatible with Strings (AnsiString) }	
		V[i] := WideString( sl[i] );
end;

{
--------------------------------------------------------------------------------
----------------------------- Generic File Routines ----------------------------
--------------------------------------------------------------------------------
}

function ApplicationPath: string;
begin
	Result := ExtractFilePath( ParamStr( 0 ) );
end;

function ApplicationLikeName( const Extension: string ): string;
begin
	Result := ChangeFileExt( ParamStr( 0 ), Extension );
end;

function ApplicationSiblingName( const FileName: string ): string;
begin
	Result := ApplicationPath;
	if ( AnsiLastChar( Result ) <> CH_BACK_SLASH ) then
		Result := Result + CH_BACK_SLASH;
	Result := Result + FileName;
end;

function GetUniqueFileName( const FileName: string ): string;
var
  i: Integer;
begin
	ForceTrimStr( FileName );
	Result := FileName;
	if CheckFile( Result ) then
	begin
		Result := InvertString( Result );
		if CheckStrContains( CH_DOTMARK, Result ) then
			Insert( 'd%', Result, Pos( CH_DOTMARK, Result ) + 1 )
		else
			Insert( 'd%', Result, 1 );
		Result := InvertString( Result );
		i := 0;
		while CheckFile( Format( Result , [i] ) ) do
			Inc( i );
		Result := Format( Result , [i] );
	end
end;

function GetUniqueFileNameFmt( const FileName: string; const Args: array of const ): string;
begin
	Result := GetUniqueFileName( Format( FileName, Args ) );
end;

function CheckDeleteFile( const FileName: string ): Boolean;
var
	bExists: Boolean;
begin
	bExists := CheckFile( FileName );
	Result := ( not bExists ) or ( bExists and DeleteFile( FileName ) );
end;

procedure ForceDeleteFile( const Source: string );
begin
	if ( not CheckDeleteFile( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvDeleteFile, [Source] );
end;

function GetTempPath: string;
var
	cChar: array[0..MAX_PATH] of Char;
begin
	ZeroMemory( @cChar[0], SizeOf( cChar ) );
	SetString( Result, cChar, Windows.GetTempPath( MAX_PATH, cChar ) );
end;

function GetTempPathFile( const Prefix: string; CreateFile: Boolean ): string;
var
	cChar: array[0..MAX_PATH] of Char;
begin
	ZeroMemory( @cChar[0], SizeOf( cChar ) );
	Result := NewTempFileName( GetTempPath, Prefix, CreateFile );
	if ( ( not CreateFile ) and FileExists( Result ) ) then
		Result := GetTempPathFile( Prefix + CH_NAMEFIX, CreateFile );
end;

function NewTempFileName( const Path, Prefix: string; CreateFile: Boolean ): string;
var
	hFile: Integer;
begin
	Result := Path;
	if ( AnsiLastChar( Path ) <> CH_BACK_SLASH ) then
		Result := Result + CH_BACK_SLASH;
	Result := Result + Copy( Prefix, 1, 3 ) + IntToHex( GetTickCount and $00FFFFFF, 5 ) + DELPHI_TEMPORARY_EXT;
	if CreateFile then
	begin
		hFile := FileCreate( Result );
		if ( hFile = -1 ) then
			Result := ''
		else
			FileClose( hFile );
	end;
end;

function WaitForFile( const FileName: string; TimeOut: DWORD ): Boolean;
var
	Limit,
	Initial: DWORD;
begin
	Result := CheckFile( FileName );
	if Result then
		Exit;
	if ( TimeOut <= 0 ) then
		Limit := DEFAULT_WAITFILE_TIMEOUT
	else
		Limit := TimeOut;
	Initial := GetTickCount;
	repeat
		Result := CheckFile( FileName );
		if CheckObject( Application ) then
			Application.ProcessMessages;
	until ( Result or ( ( GetTickCount - Initial ) > Limit ) );
end;

function GetFileSize( const FileName: string; pHighDWORD: PLongInt ): LongInt;
var
	sr: TSearchRec;
	bFound: Boolean;
begin
	Result := -1;
	bFound := ( SysUtils.FindFirst( FileName, SysUtils.faReadOnly or
		SysUtils.faHidden or SysUtils.faSysFile or SysUtils.faArchive or
		SysUtils.faAnyFile, sr ) = 0 );
	try
		with sr, FindData do
			if bFound then
			begin
				Result := nFileSizeLow;
				if CheckPointer( pHighDWORD ) then
  				pHighDWORD^ := nFileSizeHigh;
			end
			else if ( not bFound ) then
				RaiseExceptionFmt( EKSYUtils, sInvalidFileName, [FileName] );
	finally
		SysUtils.FindClose( sr );
	end;
end;

function GetFileDateCreated( const FileName: string ): TDateTime;
var
	idt: Integer;
	lft: TFileTime;
	sr: TSearchRec;
	bFound: Boolean;
begin
  Result := 0; 
	bFound := ( SysUtils.FindFirst( FileName, SysUtils.faReadOnly or
		SysUtils.faHidden or SysUtils.faSysFile or SysUtils.faArchive or
		SysUtils.faAnyFile, sr ) = 0 );
	try
		with sr, FindData do
			if bFound and FileTimeToLocalFileTime( ftCreationTime, lft ) and
				FileTimeToDosDateTime( lft, LongRec( idt ).Hi, LongRec( idt ).Lo ) then
				Result := FileDateToDateTime( idt )
			else if ( not bFound ) then
				RaiseExceptionFmt( EKSYUtils, sInvalidFileName, [FileName] )
			else
				RaiseExceptionFmt( EKSYUtils, sErrorProcessingFile, [FileName] );
	finally
		SysUtils.FindClose( sr );
	end;
end;

function GetFileDateLastAccess( const FileName: string ): TDateTime;
var
	idt: Integer;
	lft: TFileTime;
	sr: TSearchRec;
	bFound: Boolean;
begin
	Result := 0;
	bFound := ( SysUtils.FindFirst( FileName, SysUtils.faReadOnly or
		SysUtils.faHidden or SysUtils.faSysFile	or SysUtils.faArchive or
		SysUtils.faAnyFile, sr ) = 0 );
	try
		with sr, FindData do
			if bFound and FileTimeToLocalFileTime( ftLastAccessTime, lft ) and
				FileTimeToDosDateTime( lft, LongRec( idt ).Hi, LongRec( idt ).Lo ) then
				Result := FileDateToDateTime( idt )
			else if ( not bFound ) then
				RaiseExceptionFmt( EKSYUtils, sInvalidFileName, [FileName] )
			else
				RaiseExceptionFmt( EKSYUtils, sErrorProcessingFile, [FileName] );
	finally
		SysUtils.FindClose( sr );
	end;
end;

function GetFileDateLastModified( const FileName: string ): TDateTime;
var
	idt: Integer;
	lft: TFileTime;
	sr: TSearchRec;
	bFound: Boolean;
begin
	Result := 0;
	bFound := ( SysUtils.FindFirst( FileName, SysUtils.faReadOnly or
		SysUtils.faHidden or SysUtils.faSysFile	or SysUtils.faArchive or
		SysUtils.faAnyFile, sr ) = 0 );
	try
		with sr, FindData do
			if bFound and FileTimeToLocalFileTime( ftLastWriteTime, lft ) and
				FileTimeToDosDateTime( lft, LongRec( idt ).Hi, LongRec( idt ).Lo ) then
				Result := FileDateToDateTime( idt )
			else if ( not bFound ) then
				RaiseExceptionFmt( EKSYUtils, sInvalidFileName, [FileName] )
			else
				RaiseExceptionFmt( EKSYUtils, sErrorProcessingFile, [FileName] );
	finally
		SysUtils.FindClose( sr );
	end;
end;

function SetFileDateCreated( const FileName: string; ADate: TDateTime ): Integer;
var
	hf: THandle;
	lft,
	ft: TFileTime;
	Age: Integer;
	AgeHi,
	AgeLo: Word;
begin
	Result := -1;
	hf := CreateFile( PChar( FileName ), GENERIC_WRITE,
		FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,	0 );
	if CheckHandle( hf ) then
		try
			Age := DateTimeToFileDate( ADate );
			AgeHi := LongRec( Age ).Hi;
			AgeLo := LongRec( Age ).Lo;
			if DosDateTimeToFileTime( AgeHi, AgeLo, lft ) and
				 LocalFileTimeToFileTime( lft, ft ) and
				 SetFileTime( hf, @ft, nil, nil ) then
				Result := 0
			else
				Result := GetLastError;
		finally
			CloseHandle( hf );
		end;
end;

function SetFileDateLastAccess( const FileName: string; ADate: TDateTime ): Integer;
var
	hf: THandle;
	lft,
	ft: TFileTime;
	Age: Integer;
	AgeHi,
	AgeLo: Word;
begin
	Result := -1;
	hf := CreateFile( PChar( FileName ), GENERIC_WRITE,
		FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,	0 );
	if CheckHandle( hf ) then
		try
			Age := DateTimeToFileDate( ADate );
			AgeHi := LongRec( Age ).Hi;
			AgeLo := LongRec( Age ).Lo;
			if DosDateTimeToFileTime( AgeHi, AgeLo, lft ) and
				 LocalFileTimeToFileTime( lft, ft ) and
				 SetFileTime( hf, nil, @ft, nil ) then
				Result := 0
			else
				Result := GetLastError;
		finally
			CloseHandle( hf );
		end;
end;

function SetFileDateLastModified( const FileName: string; ADate: TDateTime ): Integer;
var
	hf: THandle;
	lft,
	ft: TFileTime;
	Age: Integer;
	AgeHi,
	AgeLo: Word;
begin
	Result := -1;
	hf := CreateFile( PChar( FileName ), GENERIC_WRITE,
		FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,	0 );
	if CheckHandle( hf ) then
		try
			Age := DateTimeToFileDate( ADate );
			AgeHi := LongRec( Age ).Hi;
			AgeLo := LongRec( Age ).Lo;
			if DosDateTimeToFileTime( AgeHi, AgeLo, lft ) and
				 LocalFileTimeToFileTime( lft, ft ) and
				 SetFileTime( hf, nil, nil, @ft ) then
				Result := 0
			else
				Result := GetLastError;
		finally
			CloseHandle( hf );
		end;
end;

function ExpandRelativePath( const BasePath, PathName: string ): string;
var
	s: string;
begin
	s := GetCurrentDir;
	try
		SetCurrentDir( BasePath );
		Result := ExpandFileName( ExtractFileName( PathName ) );
	finally
		SetCurrentDir( s );
	end;
end;

function RelativePath( const BasePath, PathName: string ): string;
begin
	ForcePath( BasePath );
	Result := { '..\' +} ExtractRelativePath( BasePath, PathName );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic String Routines ---------------------------
--------------------------------------------------------------------------------
}

function AbsPos( const Token, Source: string ): Integer;
begin
  Result := Pos( UpperCase( Token ), UpperCase( Source ) );
end;

function AbsAnsiPos( const Token, Source: string ): Integer;
begin
  Result := AnsiPos( AnsiUpperCase( Token ), AnsiUpperCase( Source ) );
end;

function CheckAnsiStrEqual( const S1, S2: string ): Boolean;
begin
	Result := ( ( Length( S1 ) = Length( S2 ) ) and ( AnsiCompareText( S1, S2 ) = 0 ) );
end;

function CheckAnsiCaseStrEqual( const S1, S2: string ): Boolean;
begin
	Result := ( ( Length( S1 ) = Length( S2 ) ) and ( AnsiCompareStr( S1, S2 ) = 0 ) );
end;

function CheckAnsiStrContains( const Token, Source: string ): Boolean;
begin
	Result := ( AnsiPos( Token, Source  ) > 0 );
end;

function CheckAnsiStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result or CheckAnsiStrContains( Tokens[i], Source );
end;

function CheckAnsiStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result and CheckAnsiStrContains( Tokens[i], Source );
end;

procedure ForceAnsiStrContains( const Token, Source: string );
begin
	if ( not CheckAnsiStrContains( Token, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContains, [Token, Source] );
end;

procedure ForceAnsiStrContainsAny( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAnsiStrContainsAny( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAny, [Source] );
end;

procedure ForceAnsiStrContainsAll( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAnsiStrContainsAll( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAll, [Source] );
end;

function CheckAnsiAbsStrContains( const Token, Source: string ): Boolean;
begin
  Result := ( AbsAnsiPos( Token, Source ) > 0 );
end;

procedure ForceAnsiAbsStrContains( const Token, Source: string );
begin
	if ( not CheckAnsiAbsStrContains( Token, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContains, [Token, Source] );
end;

function CheckAnsiAbsStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result or CheckAnsiAbsStrContains( Tokens[i], Source );
end;

function CheckAnsiAbsStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result and CheckAnsiAbsStrContains( Tokens[i], Source );
end;

procedure ForceAnsiAbsStrContainsAny( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAnsiAbsStrContainsAny( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAny, [Source] );
end;

procedure ForceAnsiAbsStrContainsAll( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAnsiAbsStrContainsAll( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAll, [Source] );
end;

function CheckStrEqual( const S1, S2: string ): Boolean;
begin
	Result := ( ( Length( S1 ) = Length( S2 ) ) and ( CompareText( S1, S2 ) = 0 ) );
end;

function CheckCaseStrEqual( const S1, S2: string ): Boolean;
begin
	Result := ( ( Length( S1 ) = Length( S2 ) ) and ( CompareStr( S1, S2 ) = 0 ) );
end;

procedure ForceStrEqual( const S1, S2: string );
begin
	if ( not CheckStrEqual( S1, S2 ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrEqual, [S1, S2] );
end;                              

procedure ForceCaseStrEqual( const S1, S2: string );
begin
	if ( not CheckCaseStrEqual( S1, S2 ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvCaseStrEqual, [S1, S2] );
end;

function CheckStrContains( const Token, Source: string ): Boolean;
begin
	Result := ( Pos( Token, Source  ) > 0 );
end;

function CheckStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result or CheckStrContains( Tokens[i], Source );
end;

function CheckStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := true;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result and CheckStrContains( Tokens[i], Source );
end;

procedure ForceStrContains( const Token, Source: string );
begin
	if ( not CheckStrContains( Token, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContains, [Token, Source] );
end;

procedure ForceStrContainsAny( const Tokens: array of string; const Source: string );
begin
	if ( not CheckStrContainsAny( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAll, [Source] );
end;

procedure ForceStrContainsAll( const Tokens: array of string; const Source: string );
begin
	if ( not CheckStrContainsAll( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAll, [Source] );
end;

function CheckAbsStrContains( const Token, Source: string ): Boolean;
begin
  Result := ( AbsPos( Token, Source ) > 0 );
end;

procedure ForceAbsStrContains( const Token, Source: string );
begin
	if ( not CheckAbsStrContains( Token, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContains, [Token, Source] );
end;

function CheckAbsStrContainsAny( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result or CheckAbsStrContains( Tokens[i], Source );
end;

function CheckAbsStrContainsAll( const Tokens: array of string; const Source: string ): Boolean;
var
	i: Integer;
begin
	Result := false;
	for i := Low( Tokens ) to High( Tokens ) do
		Result := Result and CheckAbsStrContains( Tokens[i], Source );
end;

procedure ForceAbsStrContainsAny( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAbsStrContainsAny( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAny, [Source] );
end;

procedure ForceAbsStrContainsAll( const Tokens: array of string; const Source: string );
begin
	if ( not CheckAbsStrContainsAll( Tokens, Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrContainsAll, [Source] );
end;

function StrLeftPad( const Source: string; Len: Integer; ch: Char ): string;
var
	i,
	iLen: Integer;
begin
	Result := Source;
	iLen := Length( Result );
	if ( Len > iLen ) then
		for i := ( iLen + 1 ) to Len do
			Result := ch + Result;
end;

function StrRightPad( const Source: string; Len: Integer; ch: Char ): string;
var
	i,
	iLen: Integer;
begin
	Result := Source;
	iLen := Length( Result );
	if ( Len > iLen ) then
		for i := ( iLen + 1 ) to Len do
			Result := Result + ch;
end;

function StrAdjustLeft( const Source: string; Len: Integer; ch: Char ): string;
var
	iLen: Integer;
begin
	Result := Source;
	iLen := Length( Result );
	if ( iLen > Len ) then
		Result := Copy( Result, 1, Len )
	else if ( iLen < Len ) then
		Result := StrLeftPad( Result, Len, ch );
end;

function StrAdjustRight( const Source: string; Len: Integer; ch: Char ): string;
var
	iLen: Integer;
begin
	Result := Source;
	iLen := Length( Result );
	if ( iLen > Len ) then
		Result := Copy( Result, 1, Len )
	else if ( iLen < Len ) then
		Result := StrRightPad( Result, Len, ch );
end;

function CharTrim( const Source: string; ch: Char ): string;
var
	i,
	l: Integer;
begin
	l := Length( Source );
	i := 1;
	while ( i <= l ) and ( Source[i] <= ch ) do
		Inc( i );
	if ( i > L ) then
		Result := ''
	else
	begin
		while ( Source[l] <= ch ) do
			Dec( l );
		Result := Copy( Source, i, l - i + 1 );
	end;
end;

function CharLTrim( const Source: string; ch: Char ): string;
var
	i,
	l: Integer;
begin
	l := Length( Source );
	i := 1;
	while ( i <= l ) and ( Source[i] <= ch ) do
		Inc( i );
	Result := Copy( Source, i, Maxint );
end;

function CharRTrim( const Source: string; ch: Char ): string;
var
	i: Integer;
begin
	i := Length( Source );
	while ( i > 0 ) and ( Source[i] <= ch ) do
		Dec( i );
	Result := Copy( Source, 1, i );
end;

function CheckStrCharSet( const Source: string; CharSet: TKCharset ): Boolean;
var
	i,
	iLen: Integer;
begin
	Result := CheckStr( Source );
	if ( not Result ) then
		Exit;
  Result := False;
	iLen := Length( Source );
	for i := 1 to iLen do
		if ( not ( Source[i] in CharSet ) ) then
			Exit;
	Result := true;
end;

function CheckStrCharSetEnum( const Source: string; CharSet: TKCharsetEnum ): Boolean;
begin
	Result := CheckStrCharSet( Source, CHARSETS[CharSet] );
end;

procedure ForceStrCharSet( const Source: string; CharSet: TKCharset );
begin
	if ( not CheckStrCharSet( Source, CharSet ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrCharSet, [Source] );
end;

procedure ForceStrCharSetEnum( const Source: string; CharSet: TKCharsetEnum );
begin
	if ( not CheckStrCharSetEnum( Source, CharSet ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStrCharSet, [Source] );
end;

procedure ForceValidIdent( const Source: string );
begin
	if ( not IsValidIdent( Source ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvIdent, [Source] );
end;

function IsValidString( const Source: string; CharSet: TKCharsetEnum ): Boolean;
begin
	Result := CheckStrCharSet( Source, CHARSETS[CharSet] );
end;

function NormalizeStringCharSet( const Source: string; CharSet: TKCharSet ): string;
var
	i,
	iLen: Integer;
begin
	Result := '';
	iLen := Length( Source );
	for i := 1 to iLen do
		if ( Source[i] in CharSet ) then
			Result := Result + Source[i];
end;

function NormalizeString( const Source: string; CharSet: TKCharsetEnum ): string;
begin
	Result := NormalizeStringCharSet( Source, CHARSETS[CharSet] );
end;

function FixStringCharSet( const Source: string; CharSet: TKCharset; Token: Char ): string;
var
	i,
	iLen: Integer;
begin
	if CheckStrCharSet( Source, CharSet ) then
	begin
		Result := Source;
		Exit;
	end;
	Result := '';
	iLen := Length( Source );
	for i := 1 to iLen do
		if ( Source[i] in CharSet ) then
			Result := Result + Source[i]
		else { if ( Token <> CH_NULL ) then  ? added and removed in 14/06/1999 ?}
			Result := Result + Token;
end;

function FixString( const Source: string; CharSet: TKCharsetEnum; Token: Char ): string;
var
	i,
	iLen: Integer;
begin
	if IsValidString( Source, CharSet ) then
	begin
		Result := Source;
		Exit;
	end;
	Result := '';
	iLen := Length( Source );
	for i := 1 to iLen do
		if ( Source[i] in CHARSETS[CharSet] ) then
			Result := Result + Source[i]
		else { if ( Token <> CH_NULL ) then  ? added and removed in 14/06/1999 ?}
			Result := Result + Token;
end;

function RemoveChar( const Source: string; OldChar: Char ): string;
var
	iPos: Integer;
begin
	Result := Source;
	repeat
		iPos := Pos( OldChar, Result );
		if ( iPos > 0 ) then
			Delete( Result, iPos, 1 );
	until ( iPos = 0 );
end;

function RemoveString( const Source, Tokens: string ): string;
var
	iPos: Integer;
begin
	Result := Source;
	repeat
		iPos := Pos( Tokens, Result );
		if ( iPos > 0 ) then
			Delete( Result, iPos, Length( Tokens ) );
	until ( iPos = 0 );
end;

function ReplaceChar( const Source: string; OldChar, NewChar: Char ): string;
var
	iPos: Integer;
begin
	Result := Source;
	repeat
		iPos := Pos( OldChar, Result );
		if ( iPos > 0 ) then
		begin
			Delete( Result, iPos, 1 );
			Insert( NewChar, Result, iPos ); { if ( NewChar <> CH_NULL ) then  ? added and removed in 14/06/1999 ?}
		end;
	until ( iPos = 0 );
end;

function ReplaceChars( const Source: string; const OldChars: array of Char; NewChar: Char ): string;
var
	i,
	iPos: Integer;
begin
	Result := Source;
	for i := Low( OldChars ) to High( OldChars ) do
		repeat
			iPos := Pos( OldChars[i], Result );
			if ( iPos > 0 ) then
			begin
				Delete( Result, iPos, 1 );
				Insert( NewChar, Result, iPos );
			end;
		until ( iPos = 0 );
end;

function ReplaceCtrlChars( const Source, Token: string ): string;
const
	CH_SET = ( CHARSET_ASCIICTRL + CHARSET_ASCIINODISP );
var
	s: string;
	i: Integer;
begin
	Result := '';
	if CheckStr( Source ) then
		for i := 1 to Length( Source ) do
			if ( Source[i] in CH_SET ) then
			begin
				Str( Ord( Source[i] ), s );
				Result := ( Result + Token + s );
			end
			else
				Result := ( Result + Source[i] );
end;

function StrToCharSet( const Source: string ): TKCharSet;
var
	i,
	iLen: Integer;
begin
	Result := [];
	iLen := Length( Source );
	for i := 1 to iLen do
		Include( Result, Source[i] );
end;

function CharSetToStr( const CharSet: TKCharSet ): string;
var
	i: Char;
begin
	Result := '';
	for i := Low( Char ) to High( Char ) do
		if ( i in CharSet ) then
			Result := Result + i;
end;

function CharSetToSetStr( const CharSet: TKCharSet ): string;
var
	i: Char;
begin
	Result := CH_BRACES_OPEN;
	for i := Low( Char ) to High( Char ) do
		if ( i in CharSet ) then
		begin
			if ( i in CHARSET_ASCIICTRL ) or ( i in CHARSET_ASCIINODISP ) then
				Result := Result + CH_GRIDLING + IntToStr( Byte( i ) ) + CH_COMMA + CH_SPACE
			else
				Result := Result + i + CH_COMMA + CH_SPACE;
		end;
	if ( Length( Result ) > 1 ) and ( Result[Length( Result )-1] = CH_COMMA ) then
		Delete( Result, Length( Result )-1, 2 );
	Result := Result + CH_BRACES_CLOSE;
end;

function InvertString( const Source: string ): string;
var
	i: Integer;
begin
	Result := '';
	for i := Length( Source ) downto 1 do
		Result := Result + Source[i];
end;

function AnsiProperCase( const Source: string; const Tokens: TKCharSet ): string;
var
	i,
	iLen: Cardinal;
begin
	Result := AnsiLowerCase( Source );
	i := 1;
	iLen := Length( Result );
	while ( i <= iLen ) do
	begin
{ step through the tokens }
		while ( i <= iLen ) and ( Result[i] in Tokens ) do
			Inc( i );
{ after processing tokens, there must be a char to change case }
		if ( i <= iLen ) then
			Result[i] := AnsiUpperCase( Result[i] )[1];
{ after processing the first upper case char, step through common chars }
		while ( i <= iLen ) and ( not ( Result[i] in Tokens ) ) do
			Inc( i );
	end;
end;

procedure ExtractStrings( const Source: string; Token: Char; Strings: TStrings );
var
	S: string;
	iPos: Integer;
	bSpecialToken: Boolean;
begin
	ForceObject( Strings );
	if ( not CheckStr( Source ) ) then
		Exit;
	Strings.BeginUpdate;
	try
		Strings.Clear;
		S := Source;
		if CheckStrContains( Token, S ) then
		begin
			repeat
				 iPos := Pos( Token, S );
				 bSpecialToken := false;
{For the case of the token! ..";"..}
				 if ( iPos > 1 ) and
						( S[iPos-1] = CH_DBLQUOTE ) and
						( ( iPos + 1 ) <= Length( S ) ) and
						( S[iPos+1] = CH_DBLQUOTE ) then
				 begin
					 Inc( iPos, 2 );
					 bSpecialToken := true;
				 end;
				 if ( iPos > 0 ) then
				 begin
					 if ( not bSpecialToken ) then
						 Strings.Add( Copy( S, 1, iPos-1 ) )
					 else
						 Strings.Add( Copy( S, 2, 1 ) );
					 S:= Copy( S, iPos + 1, Length( S ) );
				 end
				 else if CheckStr( S ) then
					 Strings.Add( S );
			until ( iPos = 0 );
		end
		else if CheckStr( S ) then
			Strings.Add( S );
	finally
		Strings.EndUpdate;
	end;
end;

function MakeString( Source: TStrings; Token: Char; Option: TKStringType ): string;
var
	i: Integer;
begin
	Result := '';
	ForceObject( Source );
	for i := 0 to Source.Count - 1 do
		case Option of
			stStrings: Result := Result + Source.Strings[I] + Token;
			stNames  : Result := Result + Source.Names[I] + Token;
			stValues : Result := Result + Source.Values[Source.Names[I]] + Token;
		end;
	if ( Length( Result ) > 0 ) then
		Delete( Result, Length( Result ), 1 );
end;

function MakeStringEx( const Source: array of string; Token: Char ): string;
var
	i: Integer;
begin
  Result := '';
	for i := Low( Source ) to High( Source ) do
		if CheckStr( Source[i] ) then
			Result := ( Result + Source[i] + Token );
	if CheckStr( Result ) then
	  Delete( Result, Length( Result ), Length( Token ) );	
end;

function CountTokens( const AText, AToken: string ): Integer;
var
	iPos: Integer;
	sString: string;
begin
	Result := 0;
	if ( ( not ( CheckStrs( [AText, AToken] ) and CheckStrContains( AToken, AText ) ) ) or
		 ( Length( AToken ) > Length( AText ) ) ) then
		Exit;
	sString := AText;
	repeat
		iPos := Pos( AToken, sString );
		Inc( Result, Ord( iPos > 0 ) );
		Delete( sString, iPos, Length( AToken ) );
	until ( ( iPos = 0 ) or ( not CheckStr( sString ) ) or
					( Length( AToken ) > Length( sString ) ) );
end;

function RemoveLastChar( const Str: string; Ch: Char ): string;
begin
	Result := Str;
	if ( CheckStr( Result ) and ( AnsiLastChar( Result ) = Ch ) ) then
		Delete( Result, Length( Result ), 1 );
end;

function RemoveFirstChar( const Str: string; Ch: Char ): string;
begin
	Result := Str;
	if ( CheckStr( Result ) and ( Result[1] = Ch ) ) then
		Delete( Result, 1, 1 );
end;

{$IFNDEF DELPHI4}

function StringReplace( const S, OldPattern, NewPattern: string;
	Flags: TKReplaceFlags ): string;
var
	Patt,
	NewStr,
	SearchStr: string;
	Offset: Integer;
begin
	if ( krfIgnoreCase in Flags ) then
	begin
		SearchStr := AnsiUpperCase( S );
		Patt := AnsiUpperCase( OldPattern );
	end
	else
	begin
		SearchStr := S;
		Patt := OldPattern;
	end;
	NewStr := S;
	Result := '';
	while CheckStr( SearchStr ) do
	begin
		Offset := AnsiPos( Patt, SearchStr );
		if ( Offset = 0 ) then
		begin
			Result := Result + NewStr;
			Break;
		end;
		Result := Result + Copy( NewStr, 1, Offset - 1 ) + NewPattern;
		NewStr := Copy( NewStr, Offset + Length( OldPattern ), MaxInt );
		if ( not ( krfReplaceAll in Flags ) ) then
		begin
			Result := Result + NewStr;
			Break;
		end;
		SearchStr := Copy( SearchStr, Offset + Length( Patt ), MaxInt );
	end;
end;

function StringReplaceFmt( const S, OldPattern, NewPattern: string; Flags: TKReplaceFlags;
	const Args: array of const ): string;
begin
	Result := StringReplace( S, OldPattern, Format( NewPattern, Args ), Flags );
end;

{$ENDIF}

function GetFirstString( const Args: array of string ): string;
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		if CheckTrimStr( Args[i] ) then
		begin
			Result := Args[i];
			Exit;
		end;
	Result := '';
end;

function GetFirstPChar( const Args: array of PChar ): PChar;
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		if CheckTrimPChar( Args[i] ) then
		begin
			Result := Args[i];
			Exit;
		end;
	Result := nil;
end;

function GetFirstStrContains( const Token: string; const Source: array of string ): string;
var
	i: Integer;
begin
	for i := Low( Source ) to High( Source ) do
		if CheckStrContains( Token, Source[i] ) then
		begin
			Result := Source[i];
			Exit;
		end;
	Result := '';
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Integer Routines ---------------------------
--------------------------------------------------------------------------------
}

function Max( A, B: LongInt ): LongInt;
begin
	if ( A > B ) then
		Result := A
	else
		Result := B;
end;

function Min( A, B: LongInt ): LongInt;
begin
	if ( A < B ) then
		Result := A
	else
		Result := B;
end;

function MaxInteger( const Values: array of Integer ): Integer;
var
	i: Cardinal;
begin
	Result := Values[0];
	for i := 0 to High( Values ) do
		if ( Values[i] > Result ) then
			Result := Values[i];
end;

function MinInteger( const Values: array of Integer ): Integer;
var
	i: Cardinal;
begin
	Result := Values[0];
	for i := 0 to High( Values ) do
		if ( Values[i] < Result ) then
			Result := Values[i];
end;

function ValueBetween( const Value, Lower, Higher: LongInt;
	const Include: Boolean ): Boolean;
begin
	if Include then
		Result := ( Value >= Lower ) and ( Value <= Higher )
	else
		Result := ( Value > Lower ) and ( Value < Higher );
end;

function IsBitOn( Value: Integer; Bit: TKBitEnum ): Boolean;
begin
	Result := ( Value and ( 1 shl Bit ) ) <> 0;
end;

function TurnBitOn( Value: Integer; Bit: TKBitEnum ): Integer;
begin
	Result := Value or ( 1 shl Bit );
end;

function TurnBitOff( Value: Integer; Bit: TKBitEnum ): Integer;
begin
	Result := Value and not ( 1 shl Bit );
end;

function HexToInt( const Source: String ): Integer;
var
	i,
	hex: Integer;
begin
	Result := 0;
	if ( not CheckTrimStr( Source ) ) then
	  Exit;
	hex := 1;
	for i := Length( Source ) downto 1 do
	begin
		case UpCase( Source[i] ) of
			'0'..'9': Result := Result + hex * StrToInt( Source[i] );
			'A'..'F': Result := Result + hex * ( Ord( UpCase( Source[i] ) ) - Ord( 'A' ) + 10 );
			CH_HEXA_TOKEN: { nothing };
		else
			RaiseExceptionFmt( EKSYUtils, sErrInvHexDigit, [Source[i]] );
		end;
		hex := hex * 16;
	end;
end;

procedure BinToHex( InBuffer, OutBuffer: PChar; BufSize: Integer ); assembler;
asm
				PUSH    ESI                  { Save Source/Dest Extra area Registers }
				PUSH    EDI
				MOV     ESI,EAX              { ESI := InBuffer  }
				MOV     EDI,EDX              { EDI := OutBuffer }
				XOR			EDX, EDX             { EDX := 0         }
				JMP     @@1                  { ECX := BufSize   }
@@0:    DB      '0123456789ABCDEF'
@@1:    LODSB                        { Load Byte from ESI in AL  }
				MOV     DL,AL                { Copy AL to DL to convert  }
				AND     DL,0FH               { it to Hexa, using the @@0 }
				MOV     AH,@@0.Byte[EDX]     { table, byte per byte      }
				MOV     DL,AL
				SHR     DL,4
				MOV     AL,@@0.Byte[EDX]
				STOSW                        { Store the EAX word with   }
				DEC     ECX                  { the converted value until }
				JNE     @@1                  { BufSize 									 }
        POP     EDI
				POP     ESI                  { PS: OutBuffer must be two    }
																		 {     times long than InBuffer }
end;

function HexToBin( OutBuffer, InBuffer: PChar; BufSize: Integer ): Integer; assembler;
asm
				PUSH    ESI
				PUSH    EDI
				PUSH    EBX
				MOV     ESI,EAX
				MOV     EDI,EDX
				MOV     EBX,EDX
				MOV     EDX,0
				JMP     @@1
@@0:    DB       0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1
        DB      -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1
        DB      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        DB      -1,10,11,12,13,14,15
@@1:    LODSW
				CMP     AL,'0'
        JB      @@2
				CMP     AL,'f'
				JA      @@2
				MOV     DL,AL
				MOV     AL,@@0.Byte[EDX-'0']
				CMP     AL,-1
				JE      @@2
        SHL     AL,4
				CMP     AH,'0'
				JB      @@2
				CMP     AH,'f'
        JA      @@2
        MOV     DL,AH
				MOV     AH,@@0.Byte[EDX-'0']
				CMP     AH,-1
        JE      @@2
        OR      AL,AH
        STOSB
        DEC     ECX
        JNE     @@1
@@2:    MOV     EAX,EDI
        SUB     EAX,EBX
        POP     EBX
				POP     EDI
				POP     ESI
end;

function StrBinToStrHex( const sIn: string ): string;
begin
	SetString( Result, nil, ( Length( sIn ) * 2 ) );
	if CheckStr( sIn ) then
		BinToHex( PChar( sIn ), PChar( Result ), Length( sIn ) );
end;

function StrHexToStrBin( const sIn: string ): string;
begin
	if ( CheckStr( sIn ) and ( not Odd( Length( sIn ) ) ) ) then
	begin
		SetString( Result, nil, ( Length( sIn ) div 2 ) );
		SetLength( Result, HexToBin( PChar( sIn ), PChar( Result ), Length( Result ) ) );
	end
	else
		Result := '';
end;

function StreamBinToStrHex( Stream: TStream ): string;
const
	STREAMBIN_TO_STRHEX_BUFFSIZE = ( 4 * KB );
var
	iRead: Integer;
	cBufIn: array[0..STREAMBIN_TO_STRHEX_BUFFSIZE - 1] of Char;
begin
	ForceObject( Stream );
	Result := '';
	if ( Stream.Size <> 0 ) then
	begin
		Stream.Position := 0;
		SetString( Result, nil, ( Stream.Size * 2 ) );
		while ( Stream.Position <> Stream.Size ) do
		begin
			iRead := Stream.Read( cBufIn, STREAMBIN_TO_STRHEX_BUFFSIZE );
			BinToHex( @cBufIn, PChar( PChar( Result ) + ( ( Stream.Position - iRead ) * 2 ) ), iRead );
		end;
	end;
end;

procedure StrHexToStreamBin( const sIn: string; sOut: TStream );
var
	i: Integer;
	pBuffer: PChar;
begin
	if ( CheckStr( sIn ) and ( not Odd( Length( sIn ) ) ) ) then
	begin
		ForceObject( sOut );
		sOut.Size := 0;
		sOut.Size := ( Length( sIn ) div 2 );
		ForceStream( sOut );
		sOut.Position := 0;
		pBuffer := StrAlloc( sOut.Size + 1 );
		try
			i := HexToBin( PChar( sIn ), pBuffer, sOut.Size );
			sOut.WriteBuffer( pBuffer^, i );
		finally
			StrDispose( pBuffer );
		end;
	end;	
end;

function StrToFloatDef( const s: string; Default: Extended ): Extended;
begin
	try
	  Result := StrToFloat( s );
	except
		on E: EConvertError do
			Result := Default
		else
		  raise;	
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------- Generic Financial Routines --------------------------
--------------------------------------------------------------------------------
}

var
	_FinancialMaxTolerance: Extended = DEFAULT_FINANCIAL_TOLERANCE;
	_FinancialMaxIterations: Extended = DEFAULT_FINANCIAL_MAX_ITERATIONS;

procedure SetFinancialMaxTolerance( Value: Extended );
begin
	if ( Value > 0 ) then
		_FinancialMaxTolerance := Value;
end;

procedure SetFinancialMaxIterations( Value: Cardinal );
begin
	if ( Value > 0 ) then
		_FinancialMaxIterations := Value;
end;

function NewtonIteration( f, df, xn: Extended ): Extended;
begin
	Result := xn - f / df;
end;

{ Cálculos envolvendo juros compostos }

function SolveForF( P, i, N: Extended ): Extended;
begin
	Result := P * Power( ( 1 + i ), N );
end;

function SolveForP( F, i, N: Extended ): Extended;
begin
	Result := F * Power( ( 1 + i ), -N );
end;

function SolveFori( P, F, N: Extended ): Extended;
begin
	Result := Power( F / P, 1 / N ) - 1;
end;

function SolveForN( P, F, i: Extended ): Extended;
begin
	Result := Ln( F / P ) / Ln( 1 + i );
end;

{ Com depósitos regulares, R }

function RSolveForF( R, i, N: Extended ): Extended;
begin
	Result := R * ( Power( ( 1 + i ), N ) - 1 ) / i;
end;

function RSolveForP( R, i, N: Extended ): Extended;
begin
	Result := R * ( 1 - Power( ( 1 + i ), -N ) ) / i;
end;

function RSolveForRFromP( P, i, N: Extended ): Extended;
begin
	Result := P * i / ( 1 - Power( ( 1 + i ), -N ) );
end;

function RSolveForRFromF( F, i, N: Extended ): Extended;
begin
	Result := F * i / ( Power( ( 1 + i ), N ) - 1 );
end;

function RSolveForiFromRP( R, P, N: Extended ): Extended;

	function Fx( i: Extended ): Extended;
	begin
		Result := R * ( 1 - Power( 1 + i, -N ) ) / i - P;
	end;

	function dFx( i: Extended ): Extended;
	begin
		Result := R * ( Power( 1 + i, -N - 1 ) * ( -N ) * i -
										( 1 - Power( 1 + i, -N ) ) ) / ( i * i );
	end;

var
	e,
	xn: Extended;
	Iterations: Cardinal;
	bFinished: Boolean;
begin
	xn := SolveFori( P, N * R, N );
	Iterations := 0;
	repeat
		Result := NewtonIteration( Fx( xn ), dFx( xn ), xn );
		e := Abs( Result - xn );
		xn := Result;
		Inc( Iterations );
		bFinished := ( e <= _FinancialMaxTolerance );
	until ( bFinished or ( Iterations > _FinancialMaxIterations ) );
	if ( not bFinished ) then
		RaiseExceptionFmt( EKMath, sErrFinDivergence, [_FinancialMaxIterations,
			_FinancialMaxTolerance] );
end;

function RSolveForiFromRF( R, F, N: Extended ): Extended;

	function Fx( i: Extended ): Extended;
	begin
		Result := R * ( Power( 1 + i, N ) - 1 ) / i - F;
	end;

	function dFx( i: Extended ): Extended;
	begin
		Result := R * ( Power( 1 + i, N - 1 ) * ( N ) * i -
										( Power( 1 + i, N ) - 1 ) ) / ( i * i );
	end;

var
	e,
	xn: Extended;
	Iterations: Cardinal;
	bFinished: Boolean;
begin
	xn := SolveFori( N * R, F, N );
	Iterations := 0;
	repeat
		Result := NewtonIteration( Fx( xn ), dFx( xn ), xn );
		e := Abs( Result - xn );
		xn := Result;
		Inc( Iterations );
		bFinished := ( e <= _FinancialMaxTolerance );
	until ( bFinished or ( Iterations > _FinancialMaxIterations ) );
	if ( not bFinished ) then
		RaiseExceptionFmt( EKMath, sErrFinDivergence, [_FinancialMaxIterations,
			_FinancialMaxTolerance] );
end;

function RSolveForNFromRP( R, P, i: Extended ): Extended;
begin
	Result := - Ln( 1 - P * i / R ) / Ln( 1 + i );
end;

function RSolveForNFromRF( R, F, i: Extended ): Extended;
begin
	Result := Ln( 1 + F * i / R ) / Ln( 1 + i );
end;

{
--------------------------------------------------------------------------------
-------------------------- Generic TDateTime Routines --------------------------
--------------------------------------------------------------------------------
}

function EndOfDay( ADate: TDateTime ): TDateTime;
var
	dt: TDateTime;
begin
	dt := EncodeTime( 23, 59, 59, 999 );
	Result := Trunc( ADate ) + Frac( dt );
end;

function StartOfDay( ADate: TDateTime ): TDateTime;
begin
	Result := Trunc( ADate );
end;

function DaysInYear( AYear: Word ): Word;
begin
	Result := YEAR_TO_DAY + Ord( IsLeapYear( AYear ) );
end;

function DaysInMonth( AYear, AMonth: Word ): Word;
begin
	Result := DAYS_IN_MONTH[IsLeapYear( AYear ), AMonth];
end;

function DatePart( ADate: TDateTime; ADatePart: TKDatePart ): Word;
var
	wY,
	wM,
	wD: Word;
begin
	DecodeDate( ADate, wY, wM, wD );
	case ADatePart of
		dpYear  : Result := wY;
		dpMonth : Result := wM;
	else
		Result := wD;
	end;
end;

function TimePart( ATime: TDateTime; ATimePart: TKTimePart ): Word;
var
	wH,
	wM,
	wS,
	wMS: Word;
begin
	DecodeTime( ATime, wH, wM, wS, wMS );
	case ATimePart of
		tpHour   : Result := wH;
		tpMinute : Result := wM;
		tpSecond : Result := wS;
	else
		Result := wMS;
	end;
end;

procedure SetDatePart( var ADate: TDateTime; ADatePart: TKDatePart; AValue: Word );
var
	wY,
	wM,
	wD: Word;
begin
	DecodeDate( ADate, wY, wM, wD );
	case ADatePart of
		dpYear  : ADate := EncodeDate( AValue, wM, wD );
		dpMonth : ADate := EncodeDate( wY, AValue, wD );
	else
		ADate := EncodeDate( wY, wM, AValue );
	end;
end;

procedure SetTimePart( var ATime: TDateTime; ATimePart: TKTimePart; AValue: Word );
var
	wH,
	wM,
	wS,
	wMS: Word;
begin
	DecodeTime( ATime, wH, wM, wS, wMS );
	case ATimePart of
		tpHour   : ATime := EncodeTime( AValue, wM, wS, wMS );
		tpMinute : ATime := EncodeTime( wH, AValue, wS, wMS );
		tpSecond : ATime := EncodeTime( wH, wM, AValue, wMS );
	else
		ATime := EncodeTime( wH, wM, wS, AValue );
	end;
end;

function CurrentDatePart( ADatePart: TKDatePart ): Word;
begin
	Result := DatePart( Date, ADatePart );
end;

function CurrentTimePart( ATimePart: TKTimePart ): Word;
begin
	Result := TimePart( Time, ATimePart );
end;

function CurrentYear: Word;
begin
	Result := CurrentDatePart( dpYear );
end;

function CurrentMonth: Word;
begin
	Result := CurrentDatePart( dpMonth );
end;

function CurrentDay: Word;
begin
	Result := CurrentDatePart( dpDay );
end;

function CurrentHour: Word;
begin
	Result := CurrentTimePart( tpHour );
end;

function CurrentMinute: Word;
begin
	Result := CurrentTimePart( tpMinute );
end;

function CurrentSecond: Word;
begin
	Result := CurrentTimePart( tpSecond );
end;

function CurrentMSecond: Word;
begin
	Result := CurrentTimePart( tpMSecond );
end;

function DateDiff( Date1, Date2: TDateTime; ADatePart: TKDatePart ): Cardinal;
var
	wYH, wYL,
	wMH, wML,
	wDH, wDL: Word;
	dtHi, dtLo: TDateTime;
begin
	if ( Date1 > Date2 ) then
	begin
		dtHi := Date1;
		dtLo := Date2;
	end
	else
	begin
		dtHi := Date2;
		dtLo := Date1;
	end;
	if ( ADatePart = dpDay ) then
		Result := Trunc( dtHi - dtLo )
	else
	begin
		Result := 0;
		DecodeDate( dtHi, wYH, wMH, wDH );
		DecodeDate( dtLo, wYL, wML, wDL );
		if ( ADatePart = dpYear ) then
		begin
			Result := wYH - wYL;
			if ( wMH < wML ) or ( ( wMH = wML ) and ( wDH < wDL ) ) then
				Dec( Result );
		end
		else if ( ADatePart = dpMonth ) then
		begin
			Result := ( wYH - wYL ) * 12 + ( wMH - wML );
			if ( wDH < wDL ) then
				Dec( Result );
		end;
	end;
end;

function TimeDiff( DateTime1, DateTime2: TDateTime; ATimePart: TKTimePart ): Cardinal;
var
	wHH, wHL,
	wMH, wML,
	wSH, wSL,
	wMSH, wMSL: Word;
	dtHi, dtLo: TDateTime;
begin
	Result := DateDiff( DateTime1, DateTime2, dpDay ) * DAY_TO_HOUR;
	if ( DateTime1 > DateTime2 ) then
	begin
		dtHi := DateTime1;
		dtLo := DateTime2;
	end
	else
	begin
		dtHi := DateTime2;
		dtLo := DateTime1;
	end;
{ The date part has been processed; let's process the time part }
	DecodeTime( dtHi, wHH, wMH, wSH, wMSH );
	DecodeTime( dtLo, wHL, wML, wSL, wMSL );
	if ( ATimePart = tpMSecond ) then
	begin
{ The difference will never be less than zero because of the test above.
  But the typecast is necessary for delphi4 complaint! }
		Result := ( Result * HOUR_TO_MSECOND ) +
							Cardinal( wHH - wHL ) * HOUR_TO_MSECOND +
							Cardinal( wMH - wML ) * MINUTE_TO_MSECOND +
							Cardinal( wSH - wSL ) * SECOND_TO_MSECOND + Cardinal( wMSH - wMSL );
	end
	else if ( ATimePart = tpSecond ) then
	begin
		Result := ( Result * HOUR_TO_SECOND ) +
							Cardinal( wHH - wHL ) * HOUR_TO_SECOND +
							Cardinal( wMH - wML ) * MINUTE_TO_SECOND + Cardinal( wSH - wSL );
		if ( wMSH < wMSL ) then
			Dec( Result );
	end
	else if ( ATimePart = tpMinute ) then
	begin
		Result := ( Result * HOUR_TO_MINUTE ) +
							Cardinal( wHH - wHL ) * HOUR_TO_MINUTE + Cardinal( wMH - wML );
		if ( wSH < wSL ) or ( ( wSH = wSL ) and ( wMSH < wMSL ) ) then
			Dec( Result );
	end
	else if ( ATimePart = tpHour ) then
	begin
		Result := Result + Cardinal( wHH - wHL );
		if ( wMH < wML ) or
			 ( ( wMH = wML ) and ( wSH < wSL ) ) or
			 ( ( wMH = wML ) and ( wSH = wSL ) and ( wMSH < wMSL ) ) then
			Dec( Result );
	end;
end;

function TimeToMSeconds( ATime: TDateTime ): Cardinal;
var
	wHour,
	wMin,
	wSec,
	wMSec: Word;
begin
	DecodeTime( ATime, wHour, wMin, wSec, wMSec );
	Result := wMSec + ( wSec * SECOND_TO_MSECOND ) + ( wMin * MINUTE_TO_MSECOND ) +
	 ( wHour * HOUR_TO_MSECOND );
end;

function TimeToSeconds( ATime: TDateTime ): Cardinal;
var
	wHour,
	wMin,
	wSec,
	wMSec: Word;
begin
	DecodeTime( ATime, wHour, wMin, wSec, wMSec );
	Result := wSec + ( wMin * MINUTE_TO_SECOND ) + ( wHour * HOUR_TO_SECOND );
end;

function TimeToMinutes( ATime: TDateTime ): Cardinal;
var
	wHour,
	wMin,
	wSec,
	wMSec: Word;
begin
	DecodeTime( ATime, wHour, wMin, wSec, wMSec );
	Result := wMin + ( wHour * HOUR_TO_MINUTE );
end;

function TimeToHours( ATime: TDateTime ): Cardinal;
var
	wHour,
	wMin,
	wSec,
	wMSec: Word;
begin
	DecodeTime( ATime, wHour, wMin, wSec, wMSec );
	Result := wHour;
end;

function DaysBetween( Date1, Date2: TDateTime ): Cardinal;
begin
	Result := DateDiff( Date1, Date2, dpDay );
end;

function MonthsBetween( Date1, Date2: TDateTime ): Cardinal;
begin
	Result := DateDiff( Date1, Date2, dpMonth );
end;

function YearsBetween( Date1, Date2: TDateTime ): Cardinal;
begin
	Result := DateDiff( Date1, Date2, dpYear );
end;

function HoursBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
begin
	Result := TimeDiff( DateTime1, DateTime2, tpHour );
end;

function MinutesBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
begin
	Result := TimeDiff( DateTime1, DateTime2, tpMinute );
end;

function SecondsBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
begin
	Result := TimeDiff( DateTime1, DateTime2, tpSecond );
end;

function MSecondsBetween( DateTime1, DateTime2: TDateTime ): Cardinal;
begin
	Result := TimeDiff( DateTime1, DateTime2, tpMSecond );
end;

function IncDate( ADate: TDateTime; Days, Months, Years: Integer ): TDateTime;
var
	wD,
	wM,
	wY: Word;
	iDay,
	iMonth,
	iYear: Integer;
begin
	DecodeDate( ADate + Days, wY, wM, wD );
	iDay := wD;
	iYear := wY;
	iMonth := wM;
	Inc( iYear, Years );
	Inc( iYear, Months div 12 );
	Inc( iMonth, Months mod 12 );
	if ( iMonth < 1 ) then
	begin
		Inc( iMonth, 12 );
		Dec( iYear );
	end;
	if ( iMonth > 12 ) then
	begin
		Dec( iMonth, 12 );
		Inc( iYear );
	end;
	if ( iDay > DAYS_IN_MONTH[IsLeapYear( iYear ), iMonth] ) then
	begin
		iDay := iDay - DAYS_IN_MONTH[IsLeapYear( iYear ), iMonth];
		Inc( iMonth );
		if ( iMonth > 12 ) then
		begin
			Dec( iMonth, 12 );
			Inc( iYear );
		end;
	end;
	Result := EncodeDate( iYear, iMonth, iDay ) + Frac( ADate );
end;

function IncTime( ATime: TDateTime; Hours, Minutes, Seconds, MSecs: Integer ): TDateTime;
begin
{ Here the inverse typecast for the constants is needed because DAY_TO_HOUR as
	a Cardinal (unsigned) could not operate within an integer (signed) value. So
	i do not here which type the compiler will "grow"! }
	Result := ATime +
					 ( Hours div Integer( DAY_TO_HOUR ) ) +
					 ( ( Hours mod Integer( DAY_TO_HOUR )* Integer( HOUR_TO_MSECOND )+
							 Minutes * Integer( MINUTE_TO_MSECOND ) +
							 Seconds * Integer( SECOND_TO_MSECOND ) +
							 MSecs ) / MSecsPerDay );
end;

function IncDay( ADate: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncDate( ADate, Delta, 0, 0 );
end;

function IncMonth( ADate: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncDate( ADate, 0, Delta, 0 );
end;

function IncYear( ADate: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncDate( ADate, 0, 0, Delta );
end;

function IncHour( ATime: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncTime( ATime, Delta, 0, 0, 0 );
end;

function IncMinute( ATime: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncTime( ATime, 0, Delta, 0, 0 );
end;

function IncSecond( ATime: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncTime( ATime, 0, 0, Delta, 0 );
end;

function IncMSec( ATime: TDateTime; Delta: Integer ): TDateTime;
begin
	Result := IncTime( ATime, 0, 0, 0, Delta );
end;

function SysDateTimeInfo: TSystemTime;
begin
	GetLocalTime( Result );
end;

function ShorDateToString( Date: TDateTime ): string;
begin
	SetString( Result, PChar( FormatDateTime( ShortDateFormat, Date ) ),
		Length( ShortDateFormat ) );
end;

function LongDateToString( Date: TDateTime ): string;
begin
	SetString( Result, PChar( FormatDateTime( LongDateFormat, Date ) ),
	  Length( LongDateFormat ) );
end;

function DayOfWeek( Date: TDateTime ): TKWeekEnum;
begin
  { SysUtils.DayOfWeek are one based and not zero based! }
	Result := TKWeekEnum( SysUtils.DayOfWeek( Date ) - 1 );
end;

function WeekDay( WeekEnum: TKWeekEnum ): string;
begin
  Result := Copy( EnumName( Cardinal( WeekEnum ), TypeInfo( TKWeekEnum ) ), 3, MaxInt );  
end;

{
--------------------------------------------------------------------------------
----------------------- Generic PChar/TStrings Routines ------------------------
--------------------------------------------------------------------------------
}

procedure TrimStrings( Source: TStrings );
var
	i: Integer;
begin
	ForceObject( Source );
	for i := Source.Count - 1 downto 0 do
		if ( not CheckTrimStr( Source.Strings[i] ) ) then
			Source.Delete( i );
end;

procedure AdjustStringsForValues( sl: TStrings );
var
	i : Integer;
begin
	if CheckStrings( sl ) then
		for i := 0 to sl.Count - 1 do
			if ( not CheckStrContains( CH_EQUAL_TOKEN, sl[i] ) ) then
				sl[i] := CH_EQUAL_TOKEN + sl[i];
end;

procedure AdjustStringsForNames( sl: TStrings );
var
	i : Integer;
begin
	if CheckStrings( sl ) then
		for i := 0 to sl.Count - 1 do
			if ( not CheckStrContains( CH_EQUAL_TOKEN, sl[i] ) ) then
				sl[i] := sl[i] + CH_EQUAL_TOKEN;
end;

procedure PCharListToStringsEx( pc: Pointer; Len: Integer; sl: TStrings );
var
  iLen: Integer;
begin
	Force( [pc, sl] );
	sl.BeginUpdate;
	try
		sl.Clear;
		while ( Len > 0 ) do
		begin
			iLen := StrLen( PChar( pc ) );
			Dec( Len, iLen + 1 );
			while ( iLen > 0 ) do
			begin
				sl.Add( PChar( pc ) );
				pc := IncPtr( pc, iLen + 1 );
				iLen := StrLen( PChar( pc ) );
				Dec( Len, iLen + 1 );
			end;
			if ( Len > 0 ) then
			begin
				Dec( Len, 1 );
				sl.Add( PChar( pc ) ); { null line }
				pc := IncPtr( pc, 1 );
			end;					
		end;
	finally
		sl.EndUpdate;
	end;
end;

function PCharListToStrings( pc: PChar; sl: TStrings ): Integer;
var
	iLen: Integer;
begin
	Result := -1;
	if CheckObject( sl ) then
	begin
	  ForcePointer( pc );
		sl.BeginUpdate;
		try
			sl.Clear;
			iLen := StrLen( pc );
			while ( iLen > 0 ) do
			begin
				sl.Add( PChar( pc ) );
				Inc( pc, iLen + 1 );
				iLen := StrLen( pc );
			end;
			Result := sl.Count;
		finally
			sl.EndUpdate;
		end;
	end
	else if CheckPointer( pc ) then
	begin
		Result := 0;
		iLen := StrLen( pc );
		while ( iLen > 0 ) do
		begin
			Inc( pc, iLen + 1 );
			Inc( Result, iLen + 1 );
			iLen := StrLen( pc );
		end;
		Inc( Result );
	end;
end;

function StrArrayToPCharList( const Strings: array of string; pc: PChar ): Integer;
var
	i: Integer;
	pi: PChar;
begin
	Result := 0;
	if ( not CheckPointer( pc ) ) then
	begin
		for i := Low( Strings ) to High( Strings ) do
			Inc( Result, Length( Strings[i] ) + 1 );
		Inc( Result, 1 );
	end
	else
	begin
	  ZeroMemory( pc, StrArrayToPCharList( Strings, nil ) );
		pi := pc;
		Result := High( Strings );
		for i := Low( Strings ) to High( Strings ) do
		begin
			StrLCopy( pi, PChar( Strings[i] ), Length( strings[i] ) );
			Inc( pi, Length( Strings[i] ) + 1 );
		end;
		pi^ := CH_NULL;
	end;
end;

function StringsToPCharList( sl: TStrings; pc: PChar ): Integer;
begin
	Result := StringsToPCharListEx( sl, 0, sl.Count - 1, pc );
end;

function StringsToPCharListEx( sl: TStrings; LowBound, HighBound: Cardinal; pc: PChar ): Integer;
var
	i: Integer;
	pi: PChar;
begin
	Result := 0;
	ForceObject( sl );
	if ( ( LowBound <= HighBound ) and ( Integer( HighBound ) < sl.Count ) ) then
	begin
	{ if pwc is nil, calculate the size, in bytes, needed for the PChar }
		if ( not CheckPointer( pc ) ) then
		begin
			for i := LowBound to HighBound do
				Inc( Result, Length( sl.Strings[i] ) + 1 );
			Inc( Result, 1 );
		end
		else
		begin
			ZeroMemory( pc, StringsToPCharListEx( sl, LowBound, HighBound, nil ) );
			pi := pc;
			Result := sl.Count;
			for i := LowBound to HighBound do
			begin
				StrLCopy( pi, PChar( sl.Strings[i] ), Length( sl.strings[i] ) );
				Inc( pi, Length( sl.Strings[i] ) + 1 );
			end;
			pi^ := CH_NULL;
		end;
	end;
end;

function PWideCharListLen( pwc: PWideChar ): Integer;
var
	iLen: Integer;
begin
	Result := -1;
	if CheckPointer( pwc ) then
	begin
		Result := 0;
		iLen := StrWLen( pwc );
		while ( iLen > 0 ) do
		begin
			Inc( pwc, iLen + 1 );
			Inc( Result, iLen + 1 );
			iLen := StrWLen( pwc );
		end;
		Inc( Result );
	end;
end;

procedure PWideCharListToStrings( pwc: PWideChar; sl: TStrings );
var
	iLen: Integer;
begin
	Force( [sl, pwc] );
	sl.BeginUpdate;
	try
		sl.Clear;
		iLen := StrWLen( pwc );
		while ( iLen > 0 ) do
		begin
			sl.Add( WideCharToString( pwc ) );
			Inc( pwc, iLen + 1 );
			iLen := StrWLen( pwc );
		end;
	finally
		sl.EndUpdate;
	end;
end;

function StringsToPWideCharList( sl: TStrings; pwc: PWideChar ): Integer;
begin
	Result := StringsToPWideCharListEx( sl, 0, sl.Count - 1, pwc );
end;

function StringsToPWideCharListEx( sl: TStrings; LowBound, HighBound: Cardinal; pwc: PWideChar ): Integer;
var
	i,
	k: Integer;
	pwi,
	pwj: PWideChar;
begin
	Result := 0;
	ForceObject( sl );
	if ( ( LowBound < HighBound ) and ( Integer( HighBound ) < sl.Count ) ) then
	begin
	{ if pwc is nil, calculate the size, in bytes, needed for the PWideChar }
		if ( not CheckPointer( pwc ) ) then
		begin
			for i := LowBound to HighBound do
				Inc( Result, Length( sl.Strings[i] ) + 1 );
			Inc( Result, 1 );
		end
		else
		begin
			ZeroMemory( pwc, StringsToPWideCharListEx( sl, LowBound, HighBound, nil ) );
			pwi := pwc;
			Result := sl.Count;
			for i := LowBound to HighBound do
			begin
				k := Length( sl.Strings[i] );
				pwj := AllocMem( ( ( k + 1 ) * SizeOf( WideChar ) ) );
				try
					StringToWideChar( sl.Strings[i], pwj, ( k * SizeOf( WideChar ) ) );
					Move( pwj^, pwi^, ( k * SizeOf( WideChar ) ) );
					Inc( pwi, ( k + 1 ) );
				finally
					FreeMem( pwj, ( ( k + 1 ) * SizeOf( WideChar ) ) );
				end;
			end;
			pwi^ := WideChar( CH_NULL );
		end;
	end;
end;

function StrAllocMem( Size: Cardinal ): PChar;
begin
	Result := StrAlloc( Size );
	ZeroMemory( Result, Size );
end;

function StrBufSize( pc: PChar ): Cardinal;
begin
	if CheckPointer( pc ) then
	begin
		Dec( pc, SizeOf( Cardinal ) );
		Result := ( Cardinal( Pointer( pc )^ ) - SizeOf( Cardinal ) );
	end
	else
		Result := 0;
end;

function StrWLen( Str: PWideChar ): Cardinal;
begin
	Result := 0;
	while ( Word( Str^ ) <> 0 ) do
	begin
		Inc( Result );
		Inc( Str );
	end;
end;

function StrWUpper( Str: PWideChar ): PWideChar; assembler;
asm
				PUSH    ESI
				MOV     ESI,Str
				MOV     EDX,Str
@@1:    LODSW
				OR      AX,AX
				JE      @@2
				CMP     AX,'a'
				JB      @@1
				CMP     AX,'z'
				JA      @@1
				SUB     AX,20H
				MOV     [ESI-2],AX
				JMP     @@1
@@2:    XCHG    EAX,EDX
				POP     ESI
end;

function StrWLower( Str: PWideChar): PWideChar; assembler;
asm
				PUSH    ESI
				MOV     ESI,Str
				MOV     EDX,Str
@@1:    LODSW
				OR      AX,AX
				JE      @@2
				CMP     AX,'A'
				JB      @@1
				CMP     AX,'Z'
				JA      @@1
				ADD     AX,20H
				MOV     [ESI-2],AX
				JMP     @@1
@@2:    XCHG    EAX,EDX
				POP     ESI
end;

function StrWAlloc( Size: Cardinal ): PWideChar;
begin
	GetMem( Result, ( Size * SizeOf( WideChar ) + SizeOf( Cardinal ) ) );
	Inc( Size, ( SizeOf( Cardinal ) div SizeOf( WideChar ) ) );
	Cardinal( Pointer( Result )^ ) := Size;
	Inc( Result, ( SizeOf( Cardinal ) div SizeOf( WideChar ) ) );
end;

function StrWAllocMem( Size: Cardinal ): PWideChar;
begin
	Result := StrWAlloc( Size );
	ZeroMemory( Result, ( Size * SizeOf( WideChar ) ) );
end;

function StrWBufSize( Str: PWideChar ): Cardinal;
begin
	if CheckPointer( Str ) then
	begin
		Dec( Str, ( SizeOf( Cardinal ) div SizeOf( WideChar ) ) );
		Result := ( Cardinal( Pointer( Str )^ ) - ( SizeOf( Cardinal ) div SizeOf( WideChar ) ) );
	end
	else
	  Result := 0;	
end;

function StrWNew( Str: PWideChar ): PWideChar;
var
	Size: Cardinal;
begin
	if ( not CheckPointer( Str ) ) then
		Result := nil
	else
	begin
		Size := StrWLen( Str ) + 1;
		Result := StrWAlloc( Size );
		try
			Move( Str^, Result^, ( ( Size - 1 ) * SizeOf( WideChar ) ) );
		except
			StrWDispose( Result );
			raise;
		end;
	end;
end;

procedure StrWDispose( Str: PWideChar );
begin
	if CheckPointer( Str ) then
	begin
		Dec( Str, ( SizeOf( Cardinal ) div SizeOf( WideChar ) ) );
		FreeMem( Str, ( Cardinal( Pointer( Str )^ ) * SizeOf( WideChar ) ) );
  end;
end;

procedure GetSections( const FileName: string; sl: TStrings );
var
	tf: TextFile;
	sIn: string;
	bOk: Boolean;
begin
  ForceObject( sl );
	ForceFile( FileName );
	AssignFile( tf, FileName );
	try
		{$I-}
		Reset( tf );
		{$I+}
		bOk := ( IOResult = 0 );
		if bOk then
		begin
			sl.Clear;
			sl.BeginUpdate;
			try
				while ( not EOF( tf ) ) do
				begin
					ReadLn( tf, sIn );
					if ( CheckStr( sIn ) ) and
             ( sIn[1] = CH_BRACES_OPEN ) and
						 ( sIn[Length( sIn )] = CH_BRACES_CLOSE ) then
						sl.Add( Copy( sIn, 2, Length( sIn ) - 2 ) );
				end;
			finally
				sl.EndUpdate;
			end;
		end;
	finally
		CloseFile( tf );
	end;
	if ( not bOk ) then
		RaiseExceptionFmt( EKSYUtils, sErrFileReset, [FileName] );
end;

procedure GetSection( const FileName: string; const SecName: string; sl: TStrings );
var
	tf: TextFile;
	sIn: string;
	bOk: Boolean;
begin
	ForceObject( sl );
	ForceFile( FileName );
	AssignFile( tf, FileName );
	try
		{$I-}
		Reset( tf );
		{$I+}
		bOk := ( IOResult = 0 );
		if bOk then
		begin
			sl.Clear;
			sIn := '';
{ Process all section headers and items up to the proper Section }
			while ( not EOF( tf ) ) and
						( not CheckStrContains( CH_BRACES_OPEN + SecName + CH_BRACES_CLOSE, sIn ) ) do
				ReadLn( tf, sIn );
			if ( not EOF( tf ) ) then
			begin
				sl.BeginUpdate;
				try
					ReadLn( tf, sIn );
{ Process until we reach another section }
					while ( not EOF( tf ) ) and
                ( CheckStr( sIn ) ) and
								( sIn[1] <> CH_BRACES_OPEN ) and
								( sIn[Length(sIn)] <> CH_BRACES_CLOSE ) do
					begin
{ Do not include empty strings }
						if CheckTrimStr( sIn ) then
							sl.Add( sIn );
						ReadLn( tf, sIn );
					end;
				finally
					sl.EndUpdate;
				end;
			end;
		end;
	finally
		CloseFile( tf );
	end;
	if ( not bOk ) then
		RaiseExceptionFmt( EKSYUtils, sErrFileReset, [FileName] );
end;

procedure StringsIntersect( Source, Target: TStrings );
var
	i: Integer;
begin
	ForceObjects( [Source, Target] );
	for i := Target.Count - 1 downto 0 do
		if ( Source.IndexOf( Target[i] ) = -1 ) then
			Target.Delete( i );
end;

procedure StringsDifference( Source, Target: TStrings );
var
	i: Integer;
begin
	ForceObjects( [Source, Target] );
	for i := Target.Count - 1 downto 0 do
		if ( Source.IndexOf( Target[i] ) <> -1 ) then
			Target.Delete( i );
end;

function SensitiveIndexOf( Source: TStrings; const S: string ): Integer;
begin
	ForceTrim( [Source, S] );
	for Result := 0 to Source.Count-1 do
		if ( CompareStr( Source.Strings[Result], S ) = 0 ) then
			Exit;
	Result := -1;
end;

function StrScanEx( Str, SubStr: PChar ): PChar;
var
	ch: Char;
	iLen: Cardinal;
	bFound: Boolean;
begin
	if ( not CheckPChars( [Str, SubStr] ) ) then
	begin
		Result := nil;
		Exit;
	end;
	ch := SubStr^;
	iLen := StrLen( SubStr );
	Result := Str;
	repeat
		Result := StrScan( Result, ch );
		if ( not CheckPChar( Result ) ) then
		begin
			Result := nil;
			Exit;
		end;
		bFound := ( StrLComp( Result, SubStr, iLen ) = 0 );
		if ( not bFound ) and ( StrLen( Result ) >= iLen ) then
			Inc( Result );
	until ( bFound or ( StrLen( Result ) < iLen ) );
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Stream Routines ----------------------------
--------------------------------------------------------------------------------
}

function CheckStreamCopy( Source, Target: TStream ): Boolean;
begin
	ForceObjects( [Source, Target] );
	Result := ( Target.CopyFrom( Source, 0 ) = Source.Size );
end;

procedure ForceStreamCopy( Source, Target: TStream );
begin
	if ( not CheckStreamCopy( Source, Target ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvStreamCopy,
		[Source.ClassName, Target.ClassName] );
end;

function NextLine( sm: TStream ): ShortString;
begin
	Result := NextShortString( sm, CH_CRLF );
end;

function NextShortString( sm: TStream; const Token: ShortString ): ShortString;
var
	i,
	iRead: Integer;
	ls: ShortString;
begin
	Result := '';
	Force( [sm, Token] );
	if ( sm.Size = sm.Position ) then
		Exit;
{ read a ShortString }
	iRead := sm.Read( ls[1], SizeOf( ShortString ) - 1 );
	SetLength( ls, iRead );
{ Lets go back to the place we started at }
	sm.Position := sm.Position - iRead;
{ look for the ShortString token }
	i := Pos( Token, ls );
{
	if the token is found:
		.copy part of ls to result (up to the token, but not including it)
		.update the stream position to the byte after the token
}
	if ( i > 0 ) then
	begin
		Result := Copy( ls, 1, i - 1 );
		sm.Position := sm.Position + ( i - 1 ) + Length( Token );
	end
{
	if the token is not found:
		.copy ls to result (all of it)
		.update the stream position to the byte after ls
}
	else
	begin
		Result := ls;
		sm.Position := sm.Position + Length( ls );
	end;
end;

function CountTokenByLine( ss: TStream; const Token: ShortString ): Integer;
begin
	Result := 0;
	if ( not ( CheckStr( Token ) and CheckObject( ss ) and ( ss.Size <> ss.Position ) ) ) then
		Exit;
	while ( ss.Size <> ss.Position ) do
		if CheckStrContains( Token, NextLine( ss ) ) then
			Inc( Result );
end;

function NextLineContaining( ss: TStream; Tokens: Variant ): ShortString;
var
	s: string;
	i: Integer;
	bFOund: Boolean;
begin
	Result := '';
	if ( not VarIsArray( Tokens ) ) or ( not CheckObject( ss ) ) or
		 ( ss.Size = ss.Position ) then
		Exit;
	bFound := false;
	while ( ss.Size <> ss.Position ) and ( not bFound ) do
	begin
		bFound := true;
		s := NextLine( ss );
		for i := VarArrayLowBound( Tokens, 1 ) to VarArrayHighBound( Tokens, 1 ) do
			bFound := ( bFound and CheckStrContains( Tokens[i], s ) );
		if bFound then
			Result := s;
	end;
end;

procedure AllLinesContaining( st: TStrings; ss: TStream; Tokens: Variant );
var
  i: Integer;
begin
	if ( not ( CheckObjects( [st, ss] ) and ( ss.Size <> ss.Position ) and
		VarIsArray( Tokens ) ) ) then
		Exit;
	st.BeginUpdate;
	try
		while ( ss.Size <> ss.Position ) do
		begin
			i := st.Add( NextLineContaining( ss, Tokens ) );
			st.Objects[i] := TObject( ss.Position - Length( st[i] ) - 2 );
		end;
		TrimStrings( st );
	finally
		st.EndUpdate;
	end;
end;

procedure AllLinesContainingPos( st: TStrings; ss: TStream; Tokens: Variant );
var
	i: Integer;
begin
	if ( not ( CheckObjects( [st, ss] ) and ( ss.Size <> ss.Position ) and
		VarIsArray( Tokens ) ) ) then
		Exit;
	st.BeginUpdate;
	try
		while ( ss.Size <> ss.Position ) do
		begin
			i := st.Add( NextLineContaining( ss, Tokens ) );
			st.Objects[i] := TObject( ss.Position - Length( st[i] ) - 2 );
		end;
		TrimStrings( st );
	finally
		st.EndUpdate;
	end;
end;

function CountTokensByLine( ss: TStream; Tokens: Variant ): Integer;
var
	s: string;
	i: Integer;
	bFOund: Boolean;
begin
	Result := 0;
	if ( not ( VarIsArray( Tokens ) and CheckObject( ss ) and ( ss.Size <> ss.Position ) ) ) then
		Exit;
	while ( ss.Size <> ss.Position ) do
	begin
		bFound := true;
		s := NextLine( ss );
		for i := VarArrayLowBound( Tokens, 1 ) to VarArrayHighBound( Tokens, 1 ) do
			bFound := ( bFound and CheckStrContains( Tokens[i], s ) );
		if bFound then
			Inc( Result );
	end;
end;

procedure FindLineStrings( st: TStrings; ss: TStream; const Token: string );
var
	i: Integer;
begin
	Force( [st, ss, Token] );
	AllLinesContainingPosEx( st, ss, [Token] );
	for i := 0 to st.Count - 1 do
		st.Objects[i] := TObject( Integer( st.Objects[i] ) + Pos( Token, st.Strings[i] ) - 1 );
end;

function GoToNextTokenAnsiPos( ss: TStringStream; const Token: string;
	CaseSensitive: Boolean ): Integer;
var
	iPos: Integer;
begin
	Result := -1;
	if ( CheckStream( ss ) and CheckStr( Token ) and ( ss.Position <> ss.Size ) ) then
	begin
    { It will found tokens in plain ASCII text! If between
			ss.DataString[ss.Position + 1]..ss.Datastring[ss.Size], there is a #0,
			PChar( ... ) will cut the string!!! If you want to interate thru an
			binary text, use GoToNextTokenBinPos (that uses ScanMem) }
		if CaseSensitive then
			iPos := AnsiPos( Token, PChar( @ss.DataString[ss.Position + 1] ) )
		else
			iPos := AnsiPos( AnsiUpperCase( Token ), AnsiUpperCase( PChar(
				@ss.DataString[ss.Position + 1] ) ) );
		if ( iPos > 0 ) then
		begin
			Result := ss.Position; { Return the original position... }
			ss.Position := ss.Position + iPos - 1{+ Length( Token )};
		end;
	end;
end;

function GoToNextLineAnsiPos( ss: TStringStream ): Integer;
begin
	Result := GoToNextTokenAnsiPos( ss, CH_CRLF, False );
end;

function GoToNextTokenPos( ss: TStringStream; const Token: string;
	CaseSensitive: Boolean ): Integer;
var
	iPos: Integer;
begin
	Result := -1;
	if ( CheckStream( ss ) and CheckStr( Token ) and ( ss.Position <> ss.Size ) ) then
	begin
		{ It will found tokens in plain ASCII text! If between
			ss.DataString[ss.Position + 1]..ss.Datastring[ss.Size], there is a #0,
			PChar( ... ) will cut the string!!! If you want to interate thru an
			binary text, use GoToNextTokenBinPos (that uses ScanMem) }
		if CaseSensitive then
			iPos := Pos( Token, PChar( @ss.DataString[ss.Position + 1] ) )
		else
			iPos := Pos( UpperCase( Token ), UpperCase( PChar( @ss.DataString[ss.Position + 1] ) ) );
		if ( iPos > 0 ) then
		begin
			Result := ss.Position; { Return the original position... }
			ss.Position := ss.Position + iPos - 1{+ Length( Token )};
		end;
	end;
end;

function GoToNextLinePos( ss: TStringStream ): Integer;
begin
	Result := GoToNextTokenPos( ss, CH_CRLF, False );
end;

function NextLineContainingEx( ss: TStream; const Tokens: array of const ): ShortString;
begin
	Result := NextLineContaining( ss, ArrayOfConstToVar( Tokens ) );
end;

procedure AllLinesContainingEx( st: TStrings; ss: TStream; const Tokens: array of const );
begin
	AllLinesContaining( st, ss, ArrayOfConstToVar( Tokens ) );
end;

procedure AllLinesContainingPosEx( st: TStrings; ss: TStream; const Tokens: array of const );
begin
	AllLinesContainingPos( st, ss, ArrayOfConstToVar( Tokens ) );
end;

function CountTokensByLineEx( ss: TStream; const Tokens: array of const ): Integer;
begin
	Result := CountTokensByLine( ss, ArrayOfConstToVar( Tokens ) );
end;

function NextLLine( sm: TStream; MaxLen: LongInt ): string;
begin
	Result := NextString( sm, CH_CRLF, MaxLen );
end;

function NextToken( sm: TStream; const Token: string; MaxLen: Integer ): Integer;
var
	ls: string;
	iRead: Integer;
begin
	Result := -1;
	Force( [sm, Token] );
	if ( sm.Size = sm.Position ) or ( MaxLen = 0 )  then
		Exit;
	if ( MaxLen < 0 ) then
	  MaxLen := sm.Size; {Length( Token );}	
{ read a ShortString }
	SetString( ls, nil, MaxLen );
	iRead := sm.Read( Pointer( ls )^, MaxLen );
	if ( iRead <> MaxLen ) then
		SetLength( ls, Min( iRead, MaxLen ) );
{ Lets go back to the place we started at }
	sm.Position := sm.Position - Min( iRead, MaxLen );
{ look for the String token }
	Result := Pos( Token, ls ) - 1;
{
	if the token is found:
		.copy part of ls to result (up to the token, but not including it)
		.update the stream position to the byte after the token
}
	if ( Result >= 0 ) then
		sm.Position := sm.Position + Result + Length( Token )
{
	if the token is not found:
		.copy ls to result (all of it)
		.update the stream position to the byte after ls
}
	else
		sm.Position := sm.Position + Length( ls );
end;

function NextString( sm: TStream; const Token: string; MaxLen: LongInt ): string;
var
	i,
	iRead: Integer;
	ls: string;
begin
	Result := '';
	Force( [sm, Token] );
	if ( sm.Size = sm.Position ) or ( MaxLen = 0 )  then
		Exit;
	if ( MaxLen < 0 ) then
	  MaxLen := sm.Size; {Length( Token );}	
{ read a ShortString }
	SetString( ls, nil, MaxLen );
	iRead := sm.Read( Pointer( ls )^, MaxLen );
	if ( iRead <> MaxLen ) then
		SetLength( ls, Min( iRead, MaxLen ) );
{ Lets go back to the place we started at }
	sm.Position := sm.Position - Min( iRead, MaxLen );
{ look for the String token }
	i := Pos( Token, ls );
{
	if the token is found:
		.copy part of ls to result (up to the token, but not including it)
		.update the stream position to the byte after the token
}
	if ( i > 0 ) then
	begin
		Result := Copy( ls, 1, i - 1 );
		sm.Position := sm.Position + ( i - 1 ) + Length( Token );
	end
{
	if the token is not found:
		.copy ls to result (all of it)
		.update the stream position to the byte after ls
}
	else
	begin
		Result := ls;
		sm.Position := sm.Position + Length( ls );
	end;
end;

function CountTokenByLLine( ss: TStream; const Token: string; MaxLen: LongInt ): Integer;
begin
	Result := 0;
	if ( not ( CheckStr( Token ) and CheckObject( ss ) and ( ss.Size <> ss.Position ) and
		 ( MaxLen > 0 ) ) ) then
		Exit;
	while ( ss.Size <> ss.Position ) do
		if CheckStrContains( Token, NextLLine( ss, MaxLen ) ) then
			Inc( Result );
end;

function NextLLineContaining( ss: TStream; Tokens: Variant; MaxLen: LongInt ): string;
var
	s: string;
	i: Integer;
	bFOund: Boolean;
begin
	Result := '';
	if ( not VarIsArray( Tokens ) ) or ( not CheckObject( ss ) ) or ( MaxLen <= 0 ) or
		 ( ss.Size = ss.Position ) then
		Exit;
	bFound := false;
	while ( ss.Size <> ss.Position ) and ( not bFound ) do
	begin
		bFound := true;
		s := NextLLine( ss, MaxLen );
		for i := VarArrayLowBound( Tokens, 1 ) to VarArrayHighBound( Tokens, 1 ) do
			bFound := ( bFound and CheckStrContains( Tokens[i], s ) );
		if bFound then
			Result := s;
	end;
end;

procedure AllLLinesContaining( st: TStrings; ss: TStream; Tokens: Variant; MaxLen: LongInt );
begin
	if ( not ( CheckObjects( [st, ss] ) and ( ss.Size <> ss.Position ) and
		VarIsArray( Tokens ) and ( MaxLen > 0 ) ) ) then
		Exit;
	st.BeginUpdate;
	try
		while ( ss.Size <> ss.Position ) do
			st.Add( NextLLineContaining( ss, Tokens, MaxLen ) );
		TrimStrings( st );
	finally
		st.EndUpdate;
	end;
end;

procedure AllLLinesContainingPos( st: TStrings; ss: TStream; Tokens: Variant; MaxLen: LongInt );
var
	i: Integer;
begin
	if ( not ( CheckObjects( [st, ss] ) and ( ss.Size <> ss.Position ) and
		VarIsArray( Tokens ) and ( MaxLen > 0 ) ) ) then
		Exit;
	st.BeginUpdate;
	try
		while ( ss.Size <> ss.Position ) do
		begin
			i := st.Add( NextLLineContaining( ss, Tokens, MaxLen ) );
			st.Objects[i] := TObject( ss.Position - Length( st[i] ) - 2 );
		end;
		TrimStrings( st );
	finally
		st.EndUpdate;
	end;
end;

function CountTokensByLLine( ss: TStream; Tokens: Variant; MaxLen: LongInt ): Integer;
var
	s: string;
	i: Integer;
	bFOund: Boolean;
begin
	Result := 0;
	if ( not ( VarIsArray( Tokens ) and CheckObject( ss ) and ( ss.Size <> ss.Position ) and
		( MaxLen > 0 ) ) ) then
		Exit;
	while ( ss.Size <> ss.Position ) do
	begin
		bFound := true;
		s := NextLLine( ss, MaxLen );
		for i := VarArrayLowBound( Tokens, 1 ) to VarArrayHighBound( Tokens, 1 ) do
			bFound := ( bFound and CheckStrContains( Tokens[i], s ) );
		if bFound then
			Inc( Result );
	end;
end;

function NextLLineContainingEx( ss: TStream; const Tokens: array of const;
	MaxLen: LongInt ): string;
begin
	Result := NextLLineContaining( ss, ArrayOfConstToVar( Tokens ), MaxLen );
end;

procedure AllLLinesContainingEx( st: TStrings; ss: TStream; const Tokens: array of const;
	MaxLen: LongInt );
begin
	AllLLinesContaining( st, ss, ArrayOfConstToVar( Tokens ), MaxLen );
end;

procedure AllLLinesContainingPosEx( st: TStrings; ss: TStream; const Tokens: array of const;
	MaxLen: LongInt );
begin
	AllLLinesContainingPos( st, ss, ArrayOfConstToVar( Tokens ), MaxLen );
end;

function CountTokensByLLineEx( ss: TStream; const Tokens: array of const;
	MaxLen: LongInt ): Integer;
begin
	Result := CountTokensByLLine( ss, ArrayOfConstToVar( Tokens ), MaxLen );
end;

procedure GetSectionsFromStreamEx( sm: TStream; sl: TStrings; SecCharOpen, SecCharClose: Char );
var
	sIn: string;
begin
	ForceObjects( [sm, sl] );
	sm.Position := 0;
	sl.Clear;
	sl.BeginUpdate;
	try
		while ( sm.Position <> sm.Size ) do
		begin
			sIn := NextLLine( sm, ( sm.Size - sm.Position ) );
			if ( CheckStr( sIn ) ) and
				 ( sIn[1] = SecCharOpen ) and
				 ( sIn[Length( sIn )] = SecCharClose ) then
				sl.Add( Copy( sIn, 2, Length( sIn ) - 2 ) );
		end;
	finally
		sl.EndUpdate;
	end;
end;

procedure GetSectionFromStreamEx( sm: TStream; const SecName: string; sl: TStrings; SecCharOpen, SecCharClose: Char );
var
	sIn: string;
begin
	ForceObjects( [sm, sl] );
	{ sm.Position := 0; Dot not do this! Must see the ukeEngines.TKExcept reflection...
											There is one bug: when the section are founded and all strings
											are included (until the next '['), then we must stop searching! }
	sl.BeginUpdate;
	try
		sl.Clear;
		while ( sm.Position <> sm.Size ) do
		begin
	{ Process all section headers and items up to the proper Section }
			sIn := '';
			while ( sm.Position <> sm.Size ) and
						( not CheckStrContains( ( SecCharOpen + SecName + SecCharClose ), sIn ) ) do
				sIn := NextLLine( sm, ( sm.Size - sm.Position ) );
	{ Process until we reach another section }
			sIn := NextLLine( sm, ( sm.Size - sm.Position ) );
			if ( sm.Position <> sm.Size ) then
			begin
				while ( CheckStr( sIn ) ) and
							( sIn[1] <> SecCharOpen ) and
							( sm.Position <> sm.Size ) and
							( sIn[Length( sIn )] <> SecCharClose ) do
				begin
					if CheckTrimStr( sIn ) then
						sl.Add( sIn );
					sIn := NextLLine( sm, ( sm.Size - sm.Position ) );
				end;
				{ This line is inserted because the logic above allways cut the last line! - 29/10/99 }
				if ( ( sm.Position = sm.Size ) and CheckStr( sIn ) ) then
				  sl.Add( sIn );
			end;
			if CheckStrings( sl ) then
				Break;
		end;
	finally
		sl.EndUpdate;
	end;
end;

procedure GetSectionsFromStream( sm: TStream; sl: TStrings );
begin
	GetSectionsFromStreamEx( sm, sl, CH_BRACES_OPEN, CH_BRACES_CLOSE );
end;

procedure GetSectionFromStream( sm: TStream; const SecName: string; sl: TStrings );
begin
	GetSectionFromStreamEx( sm, SecName, sl, CH_BRACES_OPEN, CH_BRACES_CLOSE );
end;

{
--------------------------------------------------------------------------------
---------------------------- Generic CRC Routines ------------------------------
--------------------------------------------------------------------------------
}

function CRC16( CRC: Word; const Data; szData: Cardinal ): Word;
const
	Lookup: array[Byte] of WORD =
	(
	$0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
	$C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
	$CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
	$0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
	$D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
	$1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
	$1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
	$D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
	$F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
	$3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
	$3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
	$FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
	$2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
	$EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
	$E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
	$2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
	$A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
	$6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
	$6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
	$AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
	$7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
	$BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
	$B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
	$7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
	$5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
	$9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
	$9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
	$5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
	$8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
	$4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
	$4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
	$8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
	);
var
	i: Cardinal;
begin
	Result := CRC;
	for i := 0 to szData - 1 do
		Result := ( Result shr 8 ) xor
							Lookup[PByteArray( @Data )^[i] xor ( Result and $00FF )];
end;

function CRC32( CRC: LongInt; const Data; szData: Cardinal ): LongInt;
const
	Lookup: array[Byte] of DWORD =
	(
	$00000000, $77073096, $EE0E612C, $990951BA,
	$076DC419, $706AF48F, $E963A535, $9E6495A3,
	$0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
	$09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
	$1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
	$1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
	$136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
	$14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
	$3B6E20C8, $4C69105E, $D56041E4, $A2677172,
	$3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
	$35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
	$32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
	$26D930AC, $51DE003A, $C8D75180, $BFD06116,
	$21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
	$2802B89E, $5F058808, $C60CD9B2, $B10BE924,
	$2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

	$76DC4190, $01DB7106, $98D220BC, $EFD5102A,
	$71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
	$7807C9A2, $0F00F934, $9609A88E, $E10E9818,
	$7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
	$6B6B51F4, $1C6C6162, $856530D8, $F262004E,
	$6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
	$65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
	$62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
	$4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
	$4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
	$4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
	$44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
	$5005713C, $270241AA, $BE0B1010, $C90C2086,
	$5768B525, $206F85B3, $B966D409, $CE61E49F,
	$5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
	$59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

	$EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
	$EAD54739, $9DD277AF, $04DB2615, $73DC1683,
	$E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
	$E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
	$F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
	$F762575D, $806567CB, $196C3671, $6E6B06E7,
	$FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
	$F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
	$D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
	$D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
	$D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
	$DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
	$CB61B38C, $BC66831A, $256FD2A0, $5268E236,
	$CC0C7795, $BB0B4703, $220216B9, $5505262F,
	$C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
	$C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

	$9B64C2B0, $EC63F226, $756AA39C, $026D930A,
	$9C0906A9, $EB0E363F, $72076785, $05005713,
	$95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
	$92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
	$86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
	$81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
	$88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
	$8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
	$A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
	$A7672661, $D06016F7, $4969474D, $3E6E77DB,
	$AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
	$A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
	$BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
	$BAD03605, $CDD70693, $54DE5729, $23D967BF,
	$B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
	$B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
	);
var
	i: Cardinal;
begin
	Result := CRC;
	for i := 0 to szData - 1 do
		Result := ( Result shr 8 ) xor
							( LongInt( Lookup[PByteArray( @Data )^[i] xor ( Result and LongInt( $000000FF ) )] ) );
end;

function StreamCRC16( Seed: Word; Source: TStream ): Word;
var
	p: Pointer;
begin
	ForceStream( Source );
	GetMem( p, Source.Size );
	try
		Source.Position := 0;
		Source.ReadBuffer( p^, Source.Size );
		Result := CRC16( Seed, p^, Source.Size );
	finally
		FreeMem( p, Source.Size );
	end;
end;

function StreamCRC32( Seed: LongInt; Source: TStream ): LongInt;
var
	p: Pointer;
begin
	ForceStream( Source );
	GetMem( p, Source.Size );
	try
		Source.Position := 0;
		Source.ReadBuffer( p^, Source.Size );
		Result := CRC32( Seed, p^, Source.Size );
	finally
		FreeMem( p, Source.Size );
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------- Generic Disk/Printer Routines -----------------------
--------------------------------------------------------------------------------
}

{-------------------------------- Drive Support --------------------------------}

function VolumeID( Drive: Char ): string;
var
	OldErrorMode: Integer;
	NotUsed, VolFlags: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
	Buf: TCharArray;
begin
	OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
	try
		if GetVolumeInformation( PChar( Drive + ':\' ), Buf, SizeOf( Buf ), nil,
			NotUsed, VolFlags, nil, 0 ) then
			SetString( Result, Buf, StrLen( Buf ) )
		else
			Result := '';
		if ( Drive < 'a' ) then
			Result := AnsiUpperCaseFileName( Result )
		else
			Result := AnsiLowerCaseFileName( Result );
		Result := Format( VOLUME_FORMAT, [Result] );
	finally
		SetErrorMode( OldErrorMode );
	end;
end;

function CheckDriveLetter( Drive: Char ): Boolean;
begin
	if ( Drive in CHARSET_LOWERALPHA ) then
		Dec( Drive, $20 );
	Result := ( Drive in CHARSET_UPPERALPHA );
end;

procedure ForceDriveLetter( Drive: Char );
begin
	if ( not CheckDriveLetter( Drive ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrInvDrive, [Drive] );
end;

function DriveType( Drive: Char ): string;
begin
	ForceDriveLetter( Drive );
	case GetDriveType( PChar( Drive + ':\' ) ) of
		DRIVE_REMOVABLE : Result := sDriveFloppy;
		DRIVE_FIXED     : Result := sDriveHard;
		DRIVE_REMOTE    : Result := sDriveNet;
		DRIVE_CDROM     : Result := sDriveCD;
	else
			Result := sDriveNotExists;
	end;
end;

procedure DriveList( sl: TStrings );
var
	iSize: Integer;
	pc: PChar;
begin
	ForceObject( sl );
	iSize := GetLogicalDriveStrings( 0, nil );
	pc := StrAlloc( iSize + 1 );
	try
		GetLogicalDriveStrings( iSize, pc );
		PCharListToStrings( pc, sl );
	finally
		StrDispose( pc );
	end;
end;

function CheckDriveReady( Drive: Char ): Boolean;
var
	ErrorMode: Word;
begin
	ForceDriveLetter( Drive );
	ErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
	try
		Result := ( DiskSize( Ord( Drive ) - $40 ) <> -1 );
	finally
		SetErrorMode( ErrorMode );
	end;
end;

procedure ForceDriveReady( Drive: Char );
begin
	if ( not CheckDriveReady( Drive ) ) then
		RaiseExceptionFmt( EKSYUtils, sErrDriveNotReady, [Drive] );
end;

function NetworkVolume( Drive: Char ): string;
var
	Buf: TCharArray;
	DriveStr: array [0..3] of Char;
	BufferSize: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
begin
	ForceDriveLetter( Drive );
	BufferSize := SizeOf( Buf );
	DriveStr[0] := UpCase( Drive );
	DriveStr[1] := CH_COLON;
	DriveStr[2] := CH_NULL;
	if ( WNetGetConnection( DriveStr, Buf, BufferSize ) = WN_SUCCESS ) then
	begin
		SetString( Result, Buf, BufferSize );
		if ( Drive < 'a' ) then
			Result := AnsiUpperCaseFileName( Result )
		else
			Result := AnsiLowerCaseFileName( Result );
	end
	else
		Result := VolumeID( Drive );
end;

function HDSerialNumber( Drive: Char ): string;
var
	Aux1,
	Aux2,
	SerialNum : DWORD;
	Buffer    : TCharArray;
begin
	ForceDriveReady( Drive );
	Result := '';
	if GetVolumeInformation( PChar( Drive + ':\' ), Buffer, SizeOf( Buffer ),
		@SerialNum, Aux1, Aux2, nil, 0 ) then
		Result := IntToStr( SerialNum );
end;

{------------------------------- Printer Support -------------------------------}

procedure GetPrinter( var DeviceMode, DeviceNames: THandle );
var
	Device,
	Driver,
	Port: array[0..79] of char;
	DevNames: PDevNames;
	Offset: PChar;
begin
	Printer.GetPrinter( Device, Driver, Port, DeviceMode );
	if CheckHandle( DeviceMode ) then
	begin
		DeviceNames := GlobalAlloc( GHND, SizeOf( TDevNames ) + StrLen( Device ) +
			StrLen( Driver ) + StrLen( Port ) + 3 );
		try
			DevNames := PDevNames( GlobalLock( DeviceNames ) );
			try
				Offset := PChar( DevNames ) + SizeOf( TDevnames );
				with DevNames^ do
				begin
					wDriverOffset := Longint( Offset ) - Longint( DevNames );
					Offset := StrECopy( Offset, Driver ) + 1;
					wDeviceOffset := Longint( Offset ) - Longint( DevNames );
					Offset := StrECopy( Offset, Device ) + 1;
					wOutputOffset := Longint( Offset ) - Longint( DevNames );;
					StrCopy( Offset, Port );
				end;
			finally
				GlobalUnlock( DeviceNames );
			end;
		except
			GlobalFree( DeviceNames );
			raise;
		end;
	end;
end;

procedure SetPrinter( DeviceMode, DeviceNames: THandle );
var
	DevNames: PDevNames;
begin
	DevNames := PDevNames( GlobalLock( DeviceNames ) );
	try
		try
			with DevNames^ do
				Printer.SetPrinter( PChar( DevNames ) + wDeviceOffset, PChar( DevNames ) +
					wDriverOffset, PChar( DevNames ) + wOutputOffset, DeviceMode );
		finally
			GlobalUnlock( DeviceNames );
		end;
	finally
		GlobalFree( DeviceNames );
	end;
end;

{
--------------------------------------------------------------------------------
---------------- Generic Disk/Printer Connection Dialog Routines ---------------
--------------------------------------------------------------------------------
}

procedure ConnectDiskDialog;
begin
	Win32Check( ValueBetween( WNetConnectionDialog( 0, RESOURCETYPE_DISK ), -1, NO_ERROR, True ) );
end;

procedure ConnectPrinterDialog;
begin
	Win32Check( ValueBetween( WNetConnectionDialog( 0, RESOURCETYPE_PRINT ), -1, NO_ERROR, True ) );
end;

procedure DisconnectDiskDialog;
begin
	Win32Check( ValueBetween( WNetDisconnectDialog( 0, RESOURCETYPE_DISK ), -1, NO_ERROR, True ) );
end;

procedure DisconnectPrinterDialog;
begin
	Win32Check( ValueBetween( WNetDisconnectDialog( 0, RESOURCETYPE_PRINT ), -1, NO_ERROR, True ) );
end;

{
--------------------------------------------------------------------------------
--------------------------- Generic Shell Routines -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const

	FILE_ATTRIBUTE: array[TShellFileAttribute] of DWORD =
	(
		FILE_ATTRIBUTE_READONLY,
		FILE_ATTRIBUTE_HIDDEN,
		FILE_ATTRIBUTE_SYSTEM,
		FILE_ATTRIBUTE_DIRECTORY,
		FILE_ATTRIBUTE_ARCHIVE,
		FILE_ATTRIBUTE_NORMAL,
		FILE_ATTRIBUTE_TEMPORARY,
		FILE_ATTRIBUTE_COMPRESSED
	);

	FILE_INFO: array[TShellFileInfoFlag] of DWORD =
	(
		SHGFI_ATTRIBUTES,
		SHGFI_DISPLAYNAME,
		SHGFI_ICON,
		SHGFI_ICONLOCATION,
		SHGFI_LARGEICON,
		SHGFI_LINKOVERLAY,
		SHGFI_OPENICON,
//		SHGFI_PIDL,
		SHGFI_SELECTED,
		SHGFI_SHELLICONSIZE,
		SHGFI_SMALLICON,
		SHGFI_SYSICONINDEX,
		SHGFI_TYPENAME,
		SHGFI_USEFILEATTRIBUTES
	);

	FILE_OPERATION: array[TShellFileOperation] of DWORD =
	(
		FO_COPY,
		FO_DELETE,
		FO_MOVE,
		FO_RENAME
	);

	FILEOP_FLAGS: array[TShellFileOperationFlag] of Word =
	(
		FOF_ALLOWUNDO,
		FOF_FILESONLY,
		FOF_MULTIDESTFILES,
		FOF_NOCONFIRMATION,
		FOF_NOCONFIRMMKDIR,
		FOF_NOERRORUI,
		FOF_RENAMEONCOLLISION,
		FOF_SILENT,
		FOF_SIMPLEPROGRESS,
		FOF_WANTMAPPINGHANDLE
	);

	SPECIAL_FOLDER: array[TShellSpecialFolder] of DWORD =
	( CSIDL_DESKTOP, CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS, CSIDL_PERSONAL,
		CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET,
		CSIDL_STARTMENU, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK,
		CSIDL_NETHOOD, CSIDL_FONTS, CSIDL_TEMPLATES, 0, 0, 0, 0, 0, 0, 0 );

	SPECIAL_FOLDER_KEY = REGSTR_PATH_EXPLORER + '\Shell Folders';

	FOLDER_KEYNAME: array[sfCommonStartMenu..sfCookies] of String =
	( 'CommonStartMenu', 'CommonPrograms', 'CommonStartup', 'CommonDesktopDirectory',
		'AppData', 'PrintHood', 'Cookies' );

{---------------------------- Public Implementation ----------------------------}

function RetrieveFileInfo( Value: DWORD ): TShellFileInfoFlags;
var
	i: TShellFileInfoFlag;
begin
	Result := [];
	for i := Low( TShellFileInfoFlag ) to High( TShellFileInfoFlag ) do
		if ( ( FILE_INFO[i] and Value ) = FILE_INFO[i] ) then
			Include( Result, i );
end;

function RetrieveFileAttributes( Value: DWORD ): TShellFileAttributes;
var
	i: TShellFileAttribute;
begin
	Result := [];
	for i := Low( TShellFileAttribute ) to High( TShellFileAttribute ) do
		if ( ( FILE_ATTRIBUTE[i] and Value ) = FILE_ATTRIBUTE[i] ) then
			Include( Result, i );
end;

function MakeFileFlags( Flags: TShellFileOperationFlags ): Word;
var
	i: TShellFileOperationFlag;
begin
	Result := 0;
	for i := Low( TShellFileOperationFlag ) to High( TShellFileOperationFlag ) do
		if ( i in Flags ) then
			Result := Result or FILEOP_FLAGS[i];
end;

function MakeFileInfo( FileInfo: TShellFileInfoFlags ): DWORD;
var
	i: TShellFileInfoFlag;
begin
	Result := 0;
	for i := Low( TShellFileInfoFlag ) to High( TShellFileInfoFlag ) do
		if ( i in FileInfo ) then
			Result := ( Result or FILE_INFO[i] );
end;

function MakeFileAttributes( FileAttr: TShellFileAttributes ): DWORD;
var
	i: TShellFileAttribute;
begin
	Result := 0;
	for i := Low( TShellFileAttribute ) to High( TShellFileAttribute ) do
		if ( i in FileAttr ) then
			Result := Result or FILE_ATTRIBUTE[i];
end;

function ShellBindAppOpenWith( const AppName: string ): Boolean;
begin
	ForceFile( AppName );
	Result := ( WriteToRegistry( rkLocalMachine, True, BINDAPPOPENWITH_REGPATH +
		ExtractFileName( AppName ), '', AppName ) and
		WriteToRegistry( rkLocalMachine, True, BINDAPPOPENWITH_REGPATH +
		ExtractFileName( AppName ), 'Path', ExtractFilePath( AppName ) ) );
end;

function ShellBindExtension( const FExt, FIcon, FDesc, FApp, FCmd: string ): Boolean;
var
	reg: TRegistry;
	sExt,
	sApp,
	sCmd,
	sKey,
	sIco,
	sDesc: string;
begin
	Result := false;
{ fix the file extension (no periods, please) }
	sExt := Trim( AnsiLowerCase( FExt ) );
	if ( not CheckStr( sExt ) ) or ( CheckStrContains( CH_DOTMARK, sExt ) ) then
		Exit;
{ fix the file name }
	sApp := Trim( AnsiLowerCase( FApp ) );
	if ( not CheckStr( sApp ) ) then
		Exit;
	sCmd := Trim( FCmd );
	if ( not CheckStr( sCmd ) ) then
		sCmd := ' %1';
{ fix the icon reference }
	sIco := Trim( AnsiLowerCase( FIcon ) );
	if ( not CheckStr( sIco ) ) then
		sIco := sApp;
{ the registry key under which the file extension will be created }
	sKey := sExt + '_auto_file';
{ fix the file description }
	sDesc := Trim( FDesc );
{ create the registry entries for the new association }
	reg := TRegistry.Create;
	try
{ now, it is time to fix the extension's period }
		sExt := CH_DOTMARK + sExt;
{ where does all the registration magic occur? }
		reg.RootKey := HKEY_CLASSES_ROOT;
{ a reverse resolution pointer, similar to DNS }
		Result := reg.OpenKey( sExt, true );
		if Result then
		begin
{ the default description for files with this extension }
			reg.WriteString( '', sKey );
{ the actual association- first, the file description key }
			Result := reg.OpenKey( CH_BACK_SLASH + sKey, true );
			if Result then
			begin
{ again, the default description for files with this extension }
				reg.WriteString( '', sDesc );
{ the open command for this file type }
				Result := reg.OpenKey( CH_BACK_SLASH + sKey + '\DefaultIcon', true );
				if Result then
				begin
{ the default icon for files with this extension }
					reg.WriteString( '', sIco );
{ the open command for this file type }
					Result := reg.OpenKey( CH_BACK_SLASH + sKey + '\Shell\Open\Command', true );
{ the default open command is the application associated with the extension }
					if Result then
						reg.WriteString( '', sApp + sCmd );
				end;
			end;
		end;
	finally
		reg.Free;
	end;
end;

function ShellCreateLink( const FLink, FName, FPath, FArgs: string ): Boolean;
var
	IObj: IUnknown;
	ILink: IShellLink;
	IPFile: IPersistFile;
	sLink,
	sName,
	sPath,
	sArgs: string;
	wsFileName: WideString;
begin
  ForceWinNT( False ); 
	Result := false;
{ fix the link name- if the link name is not provided, no link will be built }
	sLink := Trim( FLink );
	if ( not CheckStrContains( CH_DOTMARK, sLink ) ) then
		sLink := sLink + SHELL_LINK_EXT;
	if CheckStrEqual( ExtractFileExt( sLink ), SHELL_LINK_EXT ) then
		sLink := Copy( sLink, 1, Pos( CH_DOTMARK, sLink ) - 1 ) + SHELL_LINK_EXT;
	if ( not CheckStr( sLink ) ) then
		Exit;
{ fix the file name- if no file name is provided, no link will be built }
	sName := Trim( FName );
	if ( not CheckStr( sName ) ) then
		Exit;
{ fix the file path- if it is blank, set it to the path of the file name }
	sPath := Trim( FPath );
	if ( not CheckStr( sPath ) ) then
		sPath := ExtractFilePath( FName );
{ don't fix the execution arguments- it is OK if they're blank }
	sArgs := Trim( FArgs );
{ The tricky part: create a link object }
	IObj := CreateComObject( CLSID_ShellLink );
{ Get a ShellLink interface }
	ILink := IObj as IShellLink;
{ Get a PersistFile interface }
	IPFile := IObj as IPersistFile;
{ Set property values }
	with ILink do
	begin
{ arguments }
		SetArguments( PChar( sArgs ) );
{ the application path }
		SetPath( PChar( sName ) );
{ the working directory }
		SetWorkingDirectory( PChar( sPath ) );
	end;
	wsFileName := sLink;
	IPFile.Save( PWChar( wsFileName ), false );
	Result := true;
end;

function ShellCreateDesktopLink( const FLink, FName, FPath, FArgs: string ): Boolean;
var
	ini: TRegIniFile;
	sLink: string;
begin
	ForceWinNT( False );
	sLink := ExtractFileName( Trim( FLink ) );
	ini := TRegIniFile.Create( REGSTR_PATH_EXPLORER );
	try
		sLink := ini.ReadString( 'Shell Folders', 'Desktop', '' ) + CH_BACK_SLASH + sLink;
	finally
		ini.Free;
	end;
	Result := ShellCreateLink( sLink, FName, FPath, FArgs );
end;

function ShellCreateStartupLink( const FLink, FName, FPath, FArgs: string ): Boolean;
var
	ini: TRegIniFile;
	sLink: string;
begin
	ForceWinNT( False );
	sLink := ExtractFileName( Trim( FLink ) );
	ini := TRegIniFile.Create( REGSTR_PATH_EXPLORER );
	try
		sLink := ini.ReadString( 'Shell Folders', 'Start Menu', '' ) + CH_BACK_SLASH + sLink;
	finally
		ini.Free;
	end;
	Result := ShellCreateLink( sLink, FName, FPath, FArgs );
end;

function ShellClearRecentDocs: Boolean;
begin
	ForceWinNT( False );
	Result := true;
	SHAddToRecentDocs( SHARD_PATH, nil );
end;

function ShellAddToRecentDocs( const FileName: string ): Boolean;
begin
  ForceWinNT( False );
	Result := true;
	SHAddToRecentDocs( SHARD_PATH, PChar( FileName ) );
end;

function ShellSpecialFolderPath( Folder: TShellSpecialFolder ): string;
var
	ini: TRegistry;
	err: HResult;
	pcPath: PChar;
	piil: PItemIDList;
begin
	ForceWinNT( False );
	Result := '';
	if ( Folder <= sfTemplates ) then
	begin
		pcPath := StrAlloc( MAX_PATH + 1 );
		try
			err := SHGetSpecialFolderLocation( 0, SPECIAL_FOLDER[Folder], piil );
			if ( err <> NOERROR ) then
				RaiseLastWin32Error;
			SHGetPathFromIDList( piil, pcPath );
			Result := StrPas( pcPath );
		finally
			StrDispose( pcPath );
		end;
	end
	else
	begin
		ini := TRegistry.Create;
		try
			ini.OpenKey( SPECIAL_FOLDER_KEY, false );
			try
				Result := ini.ReadString( FOLDER_KEYNAME[Folder] );
			finally
				ini.CloseKey;
			end;
		finally
			ini.Free;
		end;
	end;
end;

function ShellFileOperation( Handle: Hwnd; const Title: string;
	Operation : TShellFileOperation; Source, Dest: TStrings;
	Flags: TShellFileOperationFlags; var NameMappings: Pointer;
	var AnyOpAborted: Boolean ): Boolean;
var
	foi: TSHFileOpStruct;
	pcSrc,
	pcDst: PChar;
	iSrcSize,
	iDstSize: Integer;
begin
	ForceWinNT( False );
	AnyOpAborted := false;
	ForceObjects( [Source, Dest] );
	iSrcSize := StringsToPCharList( Source, nil );
	iDstSize := StringsToPCharList( Dest, nil );
	pcSrc := StrAlloc( iSrcSize );
	try
		pcDst := StrAlloc( iDstSize );
		try
			StringsToPCharList( Source, pcSrc );
			StringsToPCharList( Dest, pcDst );
			ZeroMemory( @foi, SizeOf( TSHFileOpStruct ) );
			with foi do
			begin
				Wnd := Handle;
				wFunc := FILE_OPERATION[Operation];
				pFrom := pcSrc;
				pTo := pcDst;
				fFlags := MakeFileFlags( Flags );
				lpszProgressTitle := PAnsiChar( Title );
				hNameMappings := NameMappings;
			end;
			Result := ( SHFileOperation( foi ) = NOERROR );
			AnyOpAborted := foi.fAnyOperationsAborted;
			NameMappings := foi.hNameMappings;
		finally
			StrDispose( pcDst );
		end;
	finally
		StrDispose( pcSrc );
	end
end;

function ShellGetFileInfo( const FileName: string;
	Attributes: TShellFileAttributes; Flags: TShellFileInfoFlags;
	var sfi: TSHFileInfo ): Integer;
begin
	ForceWinNT( False );
	ZeroMemory( @sfi, SizeOf( TSHFileInfo ) );
	Result := SHGetFileInfo( PChar( FileName ),
		MakeFileAttributes( Attributes ),	sfi, SizeOf( TSHFileInfo ),
		MakeFileInfo( Flags ) );
end;

function ShellBrowseFolder( const Caption: string; const Root: WideString;
	out Directory: string ): Boolean;
{.$IFNDEF DELPHI4}
var
	BrowseInfo: TBrowseInfo;
	Buffer: PChar;
	RootItemIDList, ItemIDList: PItemIDList;
	ShellMalloc: IMalloc;
	IDesktopFolder: IShellFolder;
	Eaten, Flags: {$IFDEF DELPHI4}Cardinal{$ELSE}Integer{$ENDIF};
begin
	if CheckWinNT then
		Result := InputDialog( Caption, '', '', Directory )
		//Result := SelectDirectory( Directory, [], 0 );  cannot use FileCtrl, Warg.... 
	else
	begin
		Result := false;
		Directory := '';
		if ( ShGetMalloc( ShellMalloc  ) = S_OK  ) and CheckInterface( ShellMalloc ) then
		begin
			Buffer := ShellMalloc.Alloc( MAX_PATH );
			try
				SHGetDesktopFolder( IDesktopFolder );
				IDesktopFolder.ParseDisplayName( Application.Handle, nil,
					POleStr( Root ), Eaten, RootItemIDList, Flags );
				ZeroMemory( @BrowseInfo, SizeOf( BrowseInfo ) );
				with BrowseInfo do
				begin
					hwndOwner := Application.Handle;
					pidlRoot := RootItemIDList;
					pszDisplayName := Buffer;
					lpszTitle := PChar( Caption );
					ulFlags := BIF_RETURNONLYFSDIRS;
				end;
				ItemIDList := ShBrowseForFolder( BrowseInfo );
				Result :=  ItemIDList <> nil;
				if Result then
				begin
					ShGetPathFromIDList( ItemIDList, Buffer );
					ShellMalloc.Free( ItemIDList );
					Directory := Buffer;
				end;
			finally
				ShellMalloc.Free( Buffer );
			end;
		end;
	end;
end;
{.$ELSE}
{
begin
	Result := SelectDirectory( Caption, Root, Directory );
end;
  Also in Delphi4 to avoid use FileCtrl and use VCLX40.BPL!
}
{.$ENDIF}

procedure ShellFileTypesList( st: TStrings );
var
	s: string;
	i: Integer;
	s1: TStrings;
	Reg: TRegistry;
begin
	ForceObject( st );
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;
		st.BeginUpdate;
		try
			st.Clear;
			s1 := TStringList.Create;
			try
				TStringList( s1 ).Sorted := True;
				if ( not Reg.OpenKey( '', False ) ) then
					Exit;
				Reg.GetKeyNames( s1 );
        Reg.CloseKey;
				TrimStrings( s1 );
				for i := 0 to s1.Count - 1 do
					if ( ( s1[i][1] = CH_DOTMARK ) and Reg.OpenKey( s1[i], False ) ) then
					begin
						s := Reg.ReadString( '' );
						Reg.CloseKey;
						if ( ( s1.IndexOf( s ) <> -1 ) and Reg.OpenKey( s, False ) ) then
						begin
							st.Add( s1[i] + CH_EQUAL_TOKEN + GetFirstString( [Reg.ReadString( '' ), sShellFileTypeUnDef] ) );
							Reg.CloseKey;
						end;
					end;
			finally
				s1.Free;
			end;
		finally
			st.EndUpdate;
		end;
	finally
		Reg.Free;
	end;
end;

function GetSystemDir: string;
var
	cBuffer: array[0..MAX_PATH - 1] of char;
begin
	ZeroMemory( @cBuffer, SizeOf( cBuffer ) );
	SetString( Result, cBuffer, GetSystemDirectory( cBuffer, MAX_PATH ) );
end;

function GetWindowsDir: string;
var
	cBuffer: array[0..MAX_PATH - 1] of char;
begin
	ZeroMemory( @cBuffer, SizeOf( cBuffer ) );
	SetString( Result, cBuffer, GetWindowsDirectory( cBuffer, MAX_PATH ) );
end;

procedure PerformWebAction( const Command: string; WebAction: TKWebAction;
	ShowWindowStyle: TKShowWindowStyle );
var
	sCommand: string;
begin
	sCommand := WEB_PROTOCOL_NAME[WebAction] + Command;
	ShellExecute( 0, PChar( sWAOpen ), PChar( sCommand ), nil, nil,
	  SHOW_WINDOW_STYLE[ShowWindowStyle] );
end;


{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TSignature	 = TUserName;
	TKey				 = TUserName;

{$IFNDEF INTERNAL_VERSION}

	PKInstallInfo = ^TKInstallInfo;
	TKInstallInfo = record
		Signature: TSignature;
		Key: TKey;
	end;

const

	(* KLIB100_REGISTRY_SIGNATURE = '{09536FA0-BF69-11D2-B212-00C0DFE081C4}' *)

	KnowHowInstallInfo: TKInstallInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
	);

{$ENDIF}

function IsSystem_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetSystemRegistryInfo,
	  LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure RegisterSystemUnits;
begin
	RegisterRunningPackage( perSystem, $EE8B6391 ); { do not const... }
	RegisterRunningPackage( pedSystem, $BC79BBB1 );
end;

procedure UnregisterSystemUnits;
begin
	UnregisterRunningPackage( perSystem, $EE8B6391 ); { do not const... }
	UnregisterRunningPackage( pedSystem, $BC79BBB1 );
end;

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

type

	PKRegistryInfo = ^TKRegistryInfo;
	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

function PackageUserName: string;
begin
	Result := Trim( PKRegistryInfo( GetSystemRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetSystemRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( SYSTEM_VER_RELEASE_DATE );
	Result.Version := SYSTEM_VER;
	Result.Reserved := SYSTEM_VER_INT;
end;

procedure CloseShowDlgOnTopList;
var
	obj: TObject;
begin
	while CheckList( ShowDlgOnTopList ) do
	begin
		obj := TObject( ShowDlgOnTopList[0] );
		ShowDlgOnTopList.Delete( 0 );
		obj.Free;
	end;
	FreeClean( ShowDlgOnTopList );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterSystemUnits;
	TestSystemShareWareVersion;
	CreateRegCheckerThread( perSystem );
	ListSeparator := GetLocaleChar( GetThreadLocale, LOCALE_SLIST, CH_LIST_TOKEN );
{ Integer Constants }
	RegisterIntegerConsts( TypeInfo( TColor ), KIdentToColor, KColorToIdent );
end;

procedure Done;
begin
	UnregisterSystemUnits;
	CloseFindForm;
	CloseShowDlgOnTopList;
	FreeClean( TH32Library );
	FreeClean( PSAPILibrary );
	FreeClean( dlmfs );
end;

initialization
	Init;

finalization
	Done;

end.
