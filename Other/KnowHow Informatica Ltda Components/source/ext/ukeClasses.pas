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

unit ukeClasses;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, SysUtils, Messages, Classes, Controls, Forms, Graphics, ExtCtrls,
	TypInfo, uksyTypes, uksyConsts, uksyUtils, uksyClasses, ukrClasses,
  ukrMessages, ukeConsts, ukeTypes;
  
type

	EKEClasses = class( EKExt );

{
--------------------------------------------------------------------------------
--------------------------------- TKMessageSpy ---------------------------------
--------------------------------------------------------------------------------
}

	EKMessageSpy = class( EKEClasses );

	TKMessageSpy = class;

{ TKMessageGroup }

	TKMessageGroup = class( TPersistent )
	private
		FOwner: TKMessageSpy;
		FMsgEnums: TIdentMsgMapEntryEnums;

		function GetMsgEnumsAsString: string;
		procedure SetMsgEnumsAsString( const Value: string );
		procedure WriteData( Writer: TWriter );
		procedure ReadData( Reader: TReader );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}	
		procedure DefineProperties( Filer: TFiler ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TKMessageSpy );

		procedure Assign( Source: TPersistent ); override;

		property Spy: TKMessageSpy
						 read FOwner;
		property MsgEnumsAsString: string
						 read GetMsgEnumsAsString write SetMsgEnumsAsString;
		property MsgEnums: TIdentMsgMapEntryEnums
						 read FMsgEnums write FMsgEnums;

	end;

	TKSpyLogType = ( sltWndProc, sltDefWndProc, sltBoth );

	TKSpyConfirmLog = procedure( Sender: TKMessageSpy; const Message: TMessage;
		const sMsg: string; IsWndProc: Boolean; var Confirm: Boolean ) of object;

{ TKMessageSpy }

	TKMessageSpy = class( TKCustomLinkable )
	private
		FActive: Boolean;
		FLogParams: Boolean;
		FLogUntranslatedMsg: Boolean;
		FMessages: TKMessageGroup;
		FDefaultName: Boolean;
		FClearFile: Boolean;
		FDirectory: string;
		FControlName: string;
		FControl: TControl;
		FWndProc: TWndMethod;
		FDefWndProc: Pointer;
		FOldDefWndProc: Pointer;
		FStream: TKStringStream;
		FLogType: TKSpyLogType;
		FOnConfirmLog: TKSpyConfirmLog;

		procedure SetActive( const Value: Boolean );
		procedure SetMessages( const Value: TKMessageGroup );
		procedure SetControl( const Value: TControl );
		procedure CheckWndProc;
		procedure CheckDefWndProc;
		procedure CheckDirectory;

	protected
		procedure AssignTo( Dest: TPersistent ); override;
		procedure Notification( AComponent: TComponent; AOperation: TOperation ); override;

		procedure WrapperWndProc( var Message: TMessage ); dynamic;
		procedure InternalWndProc( var Message: TMessage ); dynamic;

    function GetFileName: string; virtual;

		function DoConfirmLog( const Message: TMessage; const sMsg: string;
			IsWndProc: Boolean ): Boolean; dynamic;

		procedure CreateStream; dynamic;
		procedure FreeStream; dynamic;

		function CheckLog( const Message: TMessage; var S: string ): Boolean; virtual;
		procedure LogMessage( const Message: TMessage; const sMsg: string;
			IsWndProc: Boolean ); virtual;

		property FileName: string
						 read GetFileName;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure SaveToStrings( sl: TStrings ); virtual;

	published
		property Active: Boolean
						 read FActive write SetActive default False;
		property ClearFile: Boolean
						 read FClearFile write FClearFile default True;
		property LogUntranslatedMsg: Boolean
						 read FLogUntranslatedMsg write FLogUntranslatedMsg default False;
		property LogMsgParams: Boolean
						 read FLogParams write FLogParams default False;
		property Messages: TKMessageGroup
						 read FMessages write SetMessages;
		property Control: TControl
						 read FControl write SetControl;
		property Directory: string
						 read FDirectory write FDirectory;
		property LogType: TKSpyLogType
						 read FLogType write FLogType default sltBoth;

		property OnConfirmLog: TKSpyConfirmLog
						 read FOnConfirmLog write FOnConfirmLog;

	end;

{
--------------------------------------------------------------------------------
-------------------------------- TKFileScanner ---------------------------------
--------------------------------------------------------------------------------
}

	EKMaskList = class( EKEClasses );
	EKFileScanner = class( EKEClasses );

	TKFileScanner = class;

	TKFileAttribute = ( atReadOnly, atHidden, atSysFile, atVolumeID,
										 atDirectory, atArchive, atAnyFile );
	TKFileAttributes = set of TKFileAttribute;

	PKFileInfo = ^TKFileInfo;
	TKFileInfo = record
		Size: Integer;
		Time: TDateTime;
		LongName: TFileName;
		ShortName: TFileName;
		Attributes: TKFileAttributes;
		FindData: TWin32FindData;
	end;

	TKScanEvent = procedure( Sender: TKFileScanner; const FilePath: string;
		CurLevel: Word; FileInfo: TKFileInfo; var Cancel: Boolean ) of object;

{ TKMask }

	TKMask = class
	private
		FMask: Pointer;
		FSize: Integer;

	public
		destructor Destroy; override;
		constructor Create( const MaskValue: string );

		function Matches( const Filename: string ): Boolean;

	end;

{ TKMaskList }

	TKMaskList = class
	private
		FMasks: TStrings;
		FMaskValue: string;

		procedure BuildMaskList;
		function GetCount: Integer;
		function GetItems( iIndex: Integer ): string;
		procedure SetMaskValue( const Value: string );

	public
		constructor Create;
		destructor Destroy; override;

		procedure Clear;
		function MatchesAll( const FileName: string ): Boolean;
		function MatchesAny( const FileName: string ): Boolean;
		function Matches( iIndex: Integer; const FileName: string ): Boolean;

		property Count: Integer read GetCount;
		property Items[iIndex: Integer]: string
						 read GetItems; default;
		property MaskValue: string
						 read FMaskValue write SetMaskValue;

	end;

{ TKFileScannerLink }

	TKFileScannerLink = class( TKCustomLink )
	private
		FAfterScan: TKScanEvent;
		FBeforeScan: TKScanEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	public
		property AfterScan: TKScanEvent
						 read FAfterScan write FAfterScan;
		property BeforeScan: TKScanEvent
						 read FBeforeScan write FBeforeScan;
		property Owner;
		
	end;

{ TKFileScanner }

	TKFileScanner = class( TKCustomLinkable )
	private
		FCurLevel: Word;
		FRecLevel: Word;
		FScanning: Boolean;
		FRecursive: Boolean;
		FScanCanceled: Boolean;
		FShowProgress: Boolean;
		FSearchAttr: Integer;
		FMaskList: TKMaskList;
		FProgressCaption: string;
		FProgressPrompt: string;
		FCurrentScannedFile: string;
		FProgressStayOnTop: Boolean;
		FProgressAllowCancel: Boolean;
		FAttributes: TKFileAttributes;

		FAfterStartScan: TNotifyEvent;
		FBeforeStartScan: TNotifyEvent;
		FOnScan: TKScanEvent;

		FPathList: TStrings;
		FCurrentPath: string;
		FCurrentResult: Integer;
		FScanningStarted: Boolean;
		FCurrentSearchRec: TSearchRec;

		procedure cbExecutor;
		procedure InternalScan;
		function GetMaskValue: string;
		function GetNeedScanning: Boolean;
		procedure SetRecursive( Value: Boolean );
		procedure SetMaskValue( const Value: string );
		procedure SetAttributes( Value: TKFileAttributes );

	protected
		procedure cbProgress( var Status: string; var Percent: TKPercent;
			var Cancel: Boolean; var ProgressInfoStyle: TKProgressInfoStyle );
		procedure DoScan( const FilePath: string; CurLevel: Word; SearchRec: TSearchRec ); dynamic;
		procedure DoAfterStartScan; dynamic;
		procedure DoBeforeStartScan; dynamic;

		property NeedScanning: Boolean
						 read GetNeedScanning;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure StartScan( const StartPath: string );

		property Scanning: Boolean
						 read FScanning;
		property ScanCanceled: Boolean
						 read FScanCanceled;
		property CurrentScannedFile: string
						 read FCurrentScannedFile;
		property CurLevel: Word
						 read FCurLevel;

	published
		property Attributes: TKFileAttributes
						 read FAttributes write SetAttributes default [atAnyFile, atDirectory];
		property MaskValue: string
						 read GetMaskValue write SetMaskValue;
		property RecLevel: Word
						 read FRecLevel write FRecLevel default 0;
		property Recursive: Boolean
						 read FRecursive write SetRecursive default False;
		property ProgressCaption: string
						 read FProgressCaption write FProgressCaption;
		property ProgressPrompt: string
						 read FProgressPrompt write FProgressPrompt;
		property ProgressAllowCancel: Boolean
						 read FProgressAllowCancel write FProgressAllowCancel default True;
		property ProgressStayOnTop: Boolean
						 read FProgressStayOnTop write FProgressStayOnTop default True;
		property ShowProgress: Boolean
						 read FShowProgress write FShowProgress default True;

		property AfterStartScan: TNotifyEvent
						 read FAfterStartScan write FAfterStartScan;
		property BeforeStartScan: TNotifyEvent
						 read FBeforeStartScan write FBeforeStartScan;
		property OnScan: TKScanEvent
						 read FOnScan write FOnScan;


	end;

{
--------------------------------------------------------------------------------
------------------------------ Shell CommandLine -------------------------------
--------------------------------------------------------------------------------
}

{ TKCommandLine }

	TKCommandLine = class( TObject )
	private
		FCommand: string;
		FParams: TStrings;
		FCommandLine: string;
		FOpenQuote: Boolean;
		FQuotedItems: Boolean;

		function GetParamCount: Cardinal;
		function GetParams( Index: Cardinal ): string;
		procedure SetQuotedItems( Value: Boolean );
		procedure SetCommandLine( const Value: string );

	protected
		procedure ParseCommandLine; virtual;
		function StripQuotes( const S: string ): string; dynamic;
		procedure RemoveContinuers( var Value: string ); dynamic;

		function IsQuote( ch: Char ): Boolean; dynamic;
		function IsSeparator( ch: Char ): Boolean; dynamic;
		function IsValidChar( ch: Char ): Boolean; dynamic;
		function IsContinuer( ch: Char ): Boolean; dynamic;
		function IsInQuoteChar( ch: Char ): Boolean; dynamic;

	public
		destructor Destroy; override;
		constructor Create; virtual;

		property Command: string
						 read FCommand;
		property CommandLine: string
						 read FCommandLine write SetCommandLine;
		property Params[Index: Cardinal]: string
						 read GetParams;
		property ParamCount: Cardinal
						 read GetParamCount;
		property QuotedItems: Boolean
						 read FQuotedItems write SetQuotedItems default true;

	end;

{ TKShell }

  EKShell = class( EKEClasses );

	TKShell = class;

	TKShellCommandProc = procedure( Command: TKCommandLine; Data: Pointer;
		var ExitCode: Integer );

	TKShellGetPromptEvent = procedure( Sender: TKShell; var APrompt: string ) of object;

	TKShell = class( TObject )
	private
		FPrompt: string;
		FExitCode: Integer;
		FHelpEnabled: Boolean;
		FOnGetPrompt: TKShellGetPromptEvent;
		FOnCtrlC: TNotifyEvent;
		FOnCtrlBreak: TNotifyEvent;

		function GetPrompt: string;
		function GetCaption: string;
		procedure SetCaption( const Value: string );

	protected
		procedure CtrlC; virtual;
		procedure CtrlBreak; virtual;
		function GetDefaultPrompt: string; virtual;
		function IsHelpCommand( const Command: string ): Boolean; virtual;

		function GetHelp: string; dynamic;
		procedure HelpProc( Command: TKCommandLine ); dynamic;

	public
		constructor Create; virtual;

		procedure Run;
		procedure ShowHelp( Command: TKCommandLine );

		property ExitCode: Integer
						 read FExitCode;

		property Caption: string
						 read GetCaption write SetCaption;
		property HelpEnabled: Boolean
						 read FHelpEnabled write FHelpEnabled default true;
		property Prompt: string
						 read GetPrompt write FPrompt;

		property OnCtrlC: TNotifyEvent
						 read FOnCtrlC write FOnCtrlC;
		property OnCtrlBreak: TNotifyEvent
						 read FOnCtrlBreak write FOnCtrlBreak;
		property OnGetPrompt: TKShellGetPromptEvent
						 read FOnGetPrompt write FOnGetPrompt;

	end;

function GetUsage( const ACommand: string ): string;
procedure RegisterShellCommand( const ACommand, AUsage: string; AProc: TKShellCommandProc;
	Data: Pointer );

procedure InitShell;
procedure DoneShell;

var
	CommandShell: TKShell = nil;

{
--------------------------------------------------------------------------------
--------------------------------- TKWordParser ---------------------------------
--------------------------------------------------------------------------------
}

type

	EKWordParser = class( EKEClasses );

	TKWordCase = ( wcUpCase, wcLowCase, wcIgnore );
	TKWordSeparator = ( wsSpace, wsSemiColumn, wsDash, wsComma, wsSlash,
		wsBackSlash, wsPeriod, wsUnderscore, wsCustom );

{ TKWordParser }

	TKWordParser = class( TKCustomLinkable )
	private
		FText: string;
		FCount: Integer;
		FOutputChar: Char;
		FWords: TStringList;
		FInputChars: string;
		FResultCase: TKWordCase;
		FInputSeparator: TKWordSeparator;
		FOutputSeparator: TKWordSeparator;

		function GetText: string;
		function GetCount: Integer;
		function GetSorted: Boolean;
		function GetDuplicates: TDuplicates;
		function GetWords( Index: Integer ): string;

		procedure SetSorted( Value: Boolean );
		procedure	SetOutputChar( Value: Char );
		procedure	SetInputChars( const S: string );
		procedure SetResultCase( Value: TKWordCase );
		procedure SetDuplicates( Value: TDuplicates );
		procedure SetInputSeparator( Value: TKWordSeparator );
		procedure SetOutputSeparator( Value: TKWordSeparator );

	protected
		function IsSeparator( Value: Char ): Boolean; virtual;
		function ExtractWord( var S: string ): string; virtual;
		function RemoveSeparators( var S: string ): Boolean; virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure WordParse( const S: string ); virtual;
		procedure ParseAppend( const S: string ); virtual;

		property Count: Integer
						 read GetCount;
		property Items: TStringList
						 read FWords write FWords;
		property Words[Index: Integer]: string
						 read GetWords;
		property Text: string
						 read GetText;

	published
		property Duplicates: TDuplicates
						 read GetDuplicates write SetDuplicates;
		property Sorted: Boolean
						 read GetSorted write SetSorted;
		property ResultCase: TKWordCase
						 read FResultCase write SetResultCase default wcIgnore;
		property InputChars: string
						 read FInputChars write SetInputChars;
		property InputSeparator: TKWordSeparator
						 read FInputSeparator write SetInputSeparator default wsSpace;
		property OutputChar: Char
						 read FOutputChar write SetOutputChar default #32;
		property OutputSeparator: TKWordSeparator
						 read FOutputSeparator write SetOutputSeparator default wsSpace;

	end;

{
--------------------------------------------------------------------------------
------------------------------ TKStringsComponent ------------------------------
--------------------------------------------------------------------------------
}

{ TKStringsComponent }

	TKStringsComponent = class( TKCustomLinkable )
	private
		FStrings: TKStrings;

		procedure SetStrings( Value: TKStrings );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property Strings: TKStrings
						 read FStrings write SetStrings;

	end;

{
--------------------------------------------------------------------------------
---------------------------- TKStringsArrayComponent ---------------------------
--------------------------------------------------------------------------------
}

{ TKStringsArrayComponent }

	TKStringsArrayComponent = class( TKCustomLinkable )
	private
		FStringsArray: TKStringsArray;

		procedure SetStringsArray( Value: TKStringsArray );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property StringsArray: TKStringsArray
						 read FStringsArray write SetStringsArray;

	end;

{
--------------------------------------------------------------------------------
--------------------------------- TKExpiration ---------------------------------
--------------------------------------------------------------------------------
}

	EKExpiration = class( EKEClasses );

	TKExpirationType = ( etTrial, etBeta, etAlpha, etFinal );

	TKExpiration = class;

	TKExpirationNotifyEvent = procedure ( Sender: TKExpiration; var Message: string ) of object;

{ TKExpiration } 

	TKExpiration = class( TKCustomLinkable )
	private
		FTimer: TTimer;
		FCheckInterval: Cardinal;
		FValidDateTime: TDateTime;
		FExpirationType: TKExpirationType;
		FOnExpire: TKExpirationNotifyEvent;

		function GetCheckInterval: Cardinal;
		procedure SetCheckInterval( Value: Cardinal );
		procedure SetExpirationType( Value: TKExpirationType );

		procedure TimerEvent( Sender: TObject );
		
	protected
		procedure DoTimer; dynamic;
		procedure DoExpire( var Message: string ); dynamic;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

	published
		property ExpirationType: TKExpirationType
						 read FExpirationType write SetExpirationType default etTrial;
		property CheckInterval: Cardinal
						 read GetCheckInterval write SetCheckInterval default 60;
		property ValidDateTime: TDateTime
						 read FValidDateTime write FValidDateTime;
		property OnExpire: TKExpirationNotifyEvent
						 read FOnExpire write FOnExpire;

	end;

{
--------------------------------------------------------------------------------
------------------------------- TKVersionControl -------------------------------
--------------------------------------------------------------------------------
}

	EKVersionControl = class( EKEClasses );

	TKVersionType = ( vtTrial, vtBeta, vtAlfa, vtFreeware, vtShareware, vtComercial );

	TKCompileNumber = 0..99999;
	TKMinMaxVersionNumber = 0..99;

{ TKVersionControl }

	TKVersionControl = class( TKCustomLinkable )
	private
		FAuxObject: TObject; { Cool!!! }
		FCompileTime: Boolean;
		FActive: Boolean;
		FCompany: string;
		FCredits: TStrings;
		FCompileNumber: TKCompileNumber;
		FMajorVersion: TKMinMaxVersionNumber;
		FMinorVersion: TKMinMaxVersionNumber;
		FVersionType: TKVersionType;

		function  GetVersion: string;
		procedure SetActive( Value: Boolean );
		procedure SetCredits( Value: TStrings );
		procedure SetCompany( const Value: string );
		procedure SetVersion( const Value: string );
		procedure SetCompileNumber( Value: TKCompileNumber );
		procedure SetMajorVersion( Value: TKMinMaxVersionNumber );
		procedure SetMinorVersion( Value: TKMinMaxVersionNumber );
		procedure SetVersionType( Value: TKVersionType );

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure DoCompileNumberChange; virtual; { Just for Linker Otimization }

		property CompileTime: Boolean
						 read FCompileTime;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property Active: Boolean
						 read FActive write SetActive default True;
		property Company: string
						 read FCompany write SetCompany;
		property Credits: TStrings
						 read FCredits write SetCredits;
		property VersionType: TKVersionType
						 read FVersionType write SetVersionType default vtBeta;
		property MajorVersion: TKMinMaxVersionNumber
						 read FMajorVersion write SetMajorVersion;
		property MinorVersion: TKMinMaxVersionNumber
						 read FMinorVersion write SetMinorVersion;
		property CompileNumber: TKCompileNumber
						 read FCompileNumber write SetCompileNumber;
		property Version: string
						 read GetVersion write SetVersion stored false;

	end;

{------------------------------ TKCustomCreateProcess --------------------------}

{.$DEFINE USE_PIPES}

	EKCreateProcess = class( EKEClasses );

	TKCustomCreateProcess = class;

	TKCommandStyle = ( csNone, csContinue, csReturn );

	TKCreateProcessExecuteEvent = procedure( Sender: TKCustomCreateProcess;
		pi: TProcessInformation ) of object;

{ TKCustomCreateProcess }

	TKCustomCreateProcess = class( TKCustomLinkable )
	private
		FTimeOut: Cardinal;
		FCaptureOutputResult: TStrings;
		FCommandLine: string;
		FCommandPath: string;
		FTempDirectory: string;
		FInitialDirectory: string;
		FEnvironment: TStrings;
		FCaptureOutput: Boolean;
		FCommandStyle: TKCommandStyle;
		FShowWindowStyle: TKShowWindowStyle;
		FProcessPriority: TKProcessPriority;
		FOnExecute: TKCreateProcessExecuteEvent;
		FProcessInformation: TProcessInformation;

		function GetProcessID: Integer;
		function GetProcessHandle: THandle;

		function NTExecute: Boolean;
		 
		procedure SetCommandPath( const Value: string );
		procedure SetCommandStyle( Value: TKCommandStyle );
		procedure SetInitialDirectory( const Value: string );

	protected
		procedure DoExecute; dynamic;

		property CaptureOutputResult: TStrings
						 read FCaptureOutputResult;

		property InitialDirectory: string
						 read FInitialDirectory write SetInitialDirectory;
		property EnvironmentVars: TStrings
						 read FEnvironment write FEnvironment;
		property CaptureOutput: Boolean
						 read FCaptureOutput write FCaptureOutput default true;
		property CommandLine: string
						 read FCommandLine write FCommandLine;
		property CommandStyle: TKCommandStyle
						 read FCommandStyle write SetCommandStyle default csReturn; { need to be before commandpath }
		property CommandPath: string
						 read FCommandPath write SetCommandPath;
		property ProcessPriority: TKProcessPriority
						 read FProcessPriority write FProcessPriority default ppNormal;
		property ShowWindowStyle: TKShowWindowStyle
						 read FShowWindowStyle write FShowWindowStyle default ssNormal;
		property TempDirectory: string
						 read FTempDirectory write FTempDirectory;
		property TimeOut: Cardinal
						 read FTimeOut write FTimeOut default Cardinal( INFINITE );
		property ProcessInformation: TProcessInformation
						 read FProcessInformation;
		property ProcessHandle: THandle
						 read GetProcessHandle;
		property ProcessID: Integer
						 read GetProcessID;
		property OnExecute: TKCreateProcessExecuteEvent
						 read FOnExecute write FOnExecute;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function Execute: Boolean; virtual;

	end;

{ TKCreateProcess }

	TKCreateProcess = class( TKCustomCreateProcess )
	public
		property CaptureOutputResult;
		property ProcessHandle;
		property ProcessID;
		
	published
		property InitialDirectory;
		property EnvironmentVars;
		property CaptureOutput;
		property CommandLine;
    property CommandPath;
		property CommandStyle;
		property ProcessPriority;
		property ShowWindowStyle;
		property TempDirectory;
		property TimeOut;

		property OnExecute;

	end;

{
--------------------------------------------------------------------------------
----------------------------------- TKDCC32 ------------------------------------
--------------------------------------------------------------------------------
}

type

	TKDCC32Version = ( dcc90, dcc100, dcc120 );

	TKDCC32Switch = ( dsAlign, dsBoolEval, dsAssertions, dsDCUDebuginfo, dsImporteddata,
		dsLongStrings, dsIOChecks, dsWriteableConst, dsLocalSymbols, dsTypeInfo,
		dsOptimization, dsOpenStrings, dsOverFlowChecks, dsRangeChecks, dsTypedAddress,
		dsSafeDivide, dsVarStringChecks, dsStackFrames, dsExtendedSyntax, dsReferenceInfo );
	TKDCC32Switches = set of TKDCC32Switch;

	TKDCC32EnumType = ( detZ1, detZ2, detZ4 );

	TKDCC32Option = ( doConsoleAPP, doHints, doWarnings, doBuildAll, doEXEDebugInfo,
		doImplicitBuild, doQuiet );
	TKDCC32Options = set of TKDCC32Option;

	TKDCC32MapFile = ( dmfNone, dmfDetailed, dmfPublics, dmfSegments );
	TKDCC32CompiledFilesType = ( dcfDCU, dcfObj, dcfCPPObj );

	TKDCC32CompileErrorAction = ( dceaIgnore, dceaAbortAll, dceaAbortGroup );

	TKDCC32CompiledData = record
		Lines: Cardinal;
		MSecs: Cardinal;
		CodeBytes: Cardinal;
		DataBytes: Cardinal;
	end;

	TKCompileFileNameType = ( cfntProject, cfntPackage, cfntUnit, cfntUnKnown );

	TKPackageEnvironment = ( peRunTime, peDesignTime );
	TKPackageEnvironments = set of TKPackageEnvironment;

const
	DEFAULT_DCC32SWITCHES = [dsAlign, dsAssertions, dsDCUDebuginfo, dsImporteddata,
		dsLongStrings, dsIOChecks, dsWriteableConst, dsLocalSymbols, dsOptimization,
		dsOpenStrings, dsVarStringChecks, dsExtendedSyntax];

	DEFAULT_DCC32OPTIONS = [doHints, doWarnings, doBuildAll];

	DEFAULT_PACKAGE_ENVIRONMENTS = [peRunTime, peDesignTime];

	DEFAULT_IMAGEBASEADDR = $00400000;
	DEFAULT_MINSTACKSIZE  = $00004000;
	DEFAULT_MAXSTACKSIZE  = $00100000;

	DEFAULT_UNITALIASES_TEXT: string = 'WinTypes=Windows'#13#10'WinProcs=Windows'#13#10+
		'DbiTypes=BDE'#13#10'DbiProcs=BDE'#13#10'DbiErrs=BDE'#13#10;

	COMPILE_ITEMS_COMPFNTYPE: array[TKCompileFileNameType] of string[7] =
		( 'Project', 'Package', 'Unit', '' );

type

	EKDCC32 = class( EKCreateProcess );

	TKCompileItem = class;
	TKCompileItems = class;
	TKDCC32 = class;

{ TKCompileItem }

	TKCompileItem = class( TKCustomCollectionItem )
	private
		FHasStatistics: Boolean;
		FDCC32Version: TKDCC32Version;
		FDCC32Switches: TKDCC32Switches;
		FDCC32EnumType: TKDCC32EnumType;
		FDCC32Options: TKDCC32Options;
		FDCC32MapFile: TKDCC32MapFile;
		FPackageEnvironments: TKPackageEnvironments;
		FDCC32CompiledFilesType: TKDCC32CompiledFilesType;
		FImageBaseAddr: Cardinal;
		FMinStackSize: Cardinal;
		FMaxStackSize: Cardinal;
		FDCUOutPutDir: string;
		FEXEOutPutDir: string;
		FInitialDirectory: string;
		FCompileFileName: string;
		FTargetExtension: string;
		FUnitPaths: TStrings;
		FResPaths: TStrings;
		FIncPaths: TStrings;
		FObjPaths: TStrings;
		FSymbolDefines: TStrings;
		FUnitAliases: TStrings;
		FRunTimePackages: TStrings;
		FItemOptions: Byte;
		FErrorAction: TKDCC32CompileErrorAction;
		FBeforeCompile: TNotifyEvent;
		FAfterCompile: TNotifyEvent;

		FHints: TStrings;
		FWarnings: TStrings;
		FUnits: TStrings;
		FErrors: TStrings;
		FDCC32Path: string;
		FCompiledData: TKDCC32CompiledData;

		function GetOwnerCollection: TKCompileItems;

		function BuildSymbolDefines: string;
		function BuildUnitAliases: string;
		function BuildUnitPaths: string;
		function BuildObjectPaths: string;
		function BuildResourcePaths: string;
		function BuildIncludePaths: string;
		function BuildRunTimePackages: string;
		function BuildCompilerOptions: string;
		function BuildCompilerDirectives: string;
		function GetCompilerCommandLine: string;
		procedure BuildStatistics( const Value: string );

		procedure	PathsChange( Sender: TObject );
		procedure	SymbolsChange( Sender: TObject );
		procedure	UnitAliasesChange( Sender: TObject );
		procedure	RTPackagesChange( Sender: TObject );

		procedure SetHasStatistics( Value: Boolean );
		procedure SetCompileFileName( const Value: string );
		procedure SetTargetExtension( const Value: string );
		procedure SetDCC32Options( Value: TKDCC32Options );
		procedure SetDCC32Switches( Value: TKDCC32Switches );
		procedure SetDCC32EnumType( Value: TKDCC32EnumType );
		procedure SetDCC32Version( Value: TKDCC32Version );
		procedure SetDCUOutPutDir( const Value: string );
		procedure SetEXEOutPutDir( const Value: string );
		procedure SetInitialDirectory( const Value: string );
		procedure SetImageBaseAddr( Value: Cardinal );
		procedure SetIncPaths( Value: TStrings );
		procedure SetMaxStackSize( Value: Cardinal );
		procedure SetMinStackSize( Value: Cardinal );
		procedure SetObjPaths( Value: TStrings );
		procedure SetResPaths( Value: TStrings );
		procedure SetRunTimePackages( Value: TStrings );
		procedure SetSymbolDefines( Value: TStrings );
		procedure SetUnitAliases( Value: TStrings );
		procedure SetUnitPaths( Value: TStrings );
		procedure SetErrorAction( Value: TKDCC32CompileErrorAction );
		procedure SetPackageEnvironments( Value: TKPackageEnvironments );

	{$IFDEF DELPHI4}
	public
	{$ELSE}
	protected
	{$ENDIF}
		function GetNamePath: string; override;

	protected
		function GetCurrentDCC32Version: TKDCC32Version; dynamic;

		procedure CheckCompileFileName( const Value: string ); dynamic;
		procedure CheckDCC32File( Value: TKDCC32Version ); dynamic;
		procedure CheckUnitAliases( Value: TStrings ); dynamic;
		procedure CheckInternalStrings( Value: TStrings ); dynamic;

		procedure NormalizePaths( Value: TStrings ); dynamic;
		procedure NormalizeSymbolDefines( Value: TStrings ); dynamic;

		procedure SetUpDCC32; virtual;
		function ParseDCC32Result( ss: TStrings ): Boolean; virtual;
		procedure DoAfterCompile; dynamic;
		procedure DoBeforeCompile; dynamic;

		function DoGroupAbort: Boolean; dynamic;
		function PerformErrorAction: Boolean; virtual;

		procedure LoadFromStream( Stream: TStream ); override;
		procedure SaveToStream( Stream: TStream ); override;

	public
		destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		procedure ClearStatistics;
		procedure ClearResults;

		property Owner: TKCompileItems
						 read GetOwnerCollection;
		property CompiledData: TKDCC32CompiledData
						 read FCompiledData;

		property Errors: TStrings
						 read FErrors;
		property Hints: TStrings
						 read FHints;
		property Warnings: TStrings
						 read FWarnings;
		property CompiledUnits: TStrings
						 read FUnits;
		property CompilerCommandLine: string
						 read GetCompilerCommandLine;
    property Data;
    
	published
		property Name;
    property Enabled;
    property GroupIndex;

		property HasStatistics: Boolean
						 read FHasStatistics write SetHasStatistics default True;
		property DCC32Version: TKDCC32Version
						 read FDCC32Version write SetDCC32Version
						 default {$IFDEF DELPHI4}dcc120{$ELSE}dcc100{$ENDIF};
		property DCC32Switches: TKDCC32Switches
						 read FDCC32Switches write SetDCC32Switches default DEFAULT_DCC32SWITCHES;
		property DCC32EnumType: TKDCC32EnumType
						 read FDCC32EnumType write SetDCC32EnumType default detZ1;
		property DCC32Options: TKDCC32Options
						 read FDCC32Options write SetDCC32Options default DEFAULT_DCC32OPTIONS;
		property DCC32MapFile: TKDCC32MapFile
						 read FDCC32MapFile write FDCC32MapFile default dmfNone;
		property DCC32CompiledFilesType: TKDCC32CompiledFilesType
						 read FDCC32CompiledFilesType write FDCC32CompiledFilesType default dcfDCU;
		property ImageBaseAddr: Cardinal
						 read FImageBaseAddr write SetImageBaseAddr default DEFAULT_IMAGEBASEADDR;
		property MinStackSize: Cardinal
						 read FMinStackSize write SetMinStackSize default DEFAULT_MINSTACKSIZE;
		property MaxStackSize: Cardinal
						 read FMaxStackSize write SetMaxStackSize default DEFAULT_MAXSTACKSIZE;
{ Just a place holder for the component editor as a property editor... }
		property ItemOptions: Byte
						 read FItemOptions write FItemOptions stored False;
		property PackageEnvironments: TKPackageEnvironments
						 read	FPackageEnvironments write SetPackageEnvironments
						 default DEFAULT_PACKAGE_ENVIRONMENTS;

{ MUST BE IN THIS ORDER FOR CORRECT STREAMING }
		property CompileFileName: string
						 read FCompileFileName write SetCompileFileName;
		property DCUOutPutDir: string
						 read FDCUOutPutDir write SetDCUOutPutDir;
		property EXEOutPutDir: string
						 read FEXEOutPutDir write SetEXEOutPutDir;
		property InitialDirectory: string
						 read FInitialDirectory write SetInitialDirectory;

		property UnitPaths: TStrings
						 read FUnitPaths write SetUnitPaths;
		property ResourcePaths: TStrings
						 read FResPaths write SetResPaths;
		property IncludePaths: TStrings
						 read FIncPaths write SetIncPaths;
		property ObjectPaths: TStrings
						 read FObjPaths write SetObjPaths;
		property SymbolDefines: TStrings
						 read FSymbolDefines write SetSymbolDefines;
		property UnitAliases: TStrings
						 read FUnitAliases write SetUnitAliases;
		property RunTimePackages: TStrings
						 read FRunTimePackages write SetRunTimePackages;
		property TargetExtension: string
						 read FTargetExtension write SetTargetExtension;
		property ErrorAction: TKDCC32CompileErrorAction
						 read FErrorAction write SetErrorAction default dceaAbortGroup;

		property BeforeCompile: TNotifyEvent
						 read FBeforeCompile write FBeforeCompile;
		property AfterCompile: TNotifyEvent
						 read FAfterCompile write FAfterCompile;

	end;

{ TKCompileItems }

	TKCompileItems = class( TKCustomCollection )
	private
		FLastCompiledItem: TKCompileItem;

		function GetOwnerComp: TKDCC32;

		function GetCompiledData: TKDCC32CompiledData;

		procedure SetItem( Index: Integer; AItem: TKCompileItem );
		function GetItem( Index: Integer ): TKCompileItem;

		function GetItemByName( const AName: string ): TKCompileItem;

	protected
		procedure Update( Item: TCollectionItem ); override;

		{ Design-time editor support }
		function GetAttrCount: Integer; override;
		function GetAttr( Index: Integer ): string; override;
		function GetItemAttr( Index, ItemIndex: Integer ): string; override;

	public
		constructor Create( AComp: TKDCC32 ); virtual;

		function GetCompileFileNameType( Item: TKCompileItem ): TKCompileFileNameType;

		procedure ClearStatistics( Group: Cardinal );
		procedure ClearResults( Group: Cardinal );

		function Add: TKCompileItem; virtual;

		property Items[Index: Integer]: TKCompileItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKCompileItem
						 read GetItemByName;
		property CompiledData: TKDCC32CompiledData
						 read GetCompiledData;
		property DCC32Comp: TKDCC32
						 read GetOwnerComp;
		property LastCompiledItem: TKCompileItem
						 read FLastCompiledItem;

		property Names;

	end;

{ TKDCC32Link }

  TKDCC32Link = class;

	TKDCC32LinkEvent = procedure ( Sender: TKDCC32; Link: TKDCC32Link;
		ItemIndex: Cardinal ) of object;

	TKDCC32Link = class( TKCustomLink )
	private
		FBeforeExecAll: TKDCC32LinkEvent;
		FBeforeExecGroup: TKDCC32LinkEvent;
		FBeforeExecItem: TKDCC32LinkEvent;
		FAfterExecItem: TKDCC32LinkEvent;
		FAfterExecGroup: TKDCC32LinkEvent;
		FAfterExecAll: TKDCC32LinkEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
  	procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	public
		property BeforeExecAll: TKDCC32LinkEvent
						 read FBeforeExecAll write FBeforeExecAll;
		property BeforeExecGroup: TKDCC32LinkEvent
						 read FBeforeExecGroup write FBeforeExecGroup;
		property BeforeExecItem: TKDCC32LinkEvent
						 read FBeforeExecItem write FBeforeExecItem;
		property AfterExecItem: TKDCC32LinkEvent
						 read FAfterExecItem write FAfterExecItem;
		property AfterExecGroup: TKDCC32LinkEvent
						 read FAfterExecGroup write FAfterExecGroup;
		property AfterExecAll: TKDCC32LinkEvent
						 read FAfterExecAll write FAfterExecAll;

		property Owner;

	end;

{ TKDCC32 }

	TKDCC32LoadIniStatus = ( lisNoCounter, lisInvCounter, lisCountSync, lisCountGreater,
	  lisCountLower );

	TKDCC32LoadItemErrorEvent = procedure ( Sender: TKDCC32; Item: TKCompileItem;
		const GroupIndex: Cardinal ) of object;

	TKDCC32GroupAbortEvent = procedure ( Sender: TKDCC32; Item: TKCompileItem;
		const GroupIndex: Cardinal; var AbortGroup: Boolean ) of object;

	TKDCC32 = class( TKCustomCreateProcess )
	private
		FSaveIniLoadType: Boolean;
		FQuitting: Boolean;
		FIsLoading: Boolean;
		FCompileItems : TKCompileItems;
		FOnGroupAbort: TKDCC32GroupAbortEvent;
		FOnLoadError: TKDCC32LoadItemErrorEvent;

		procedure SetCompileItems( Value: TKCompileItems );

		function CompileItem( Item: TKCompileItem ): Boolean;

		property ShowWindowStyle;
		property CommandLine;
		property CommandPath;
		property CommandStyle;
{$HINTS OFF}
		property ProcessInformation;
		property ProcessHandle;
		property ProcessID;
		property OnExecute;
{$HINTS ON}

	protected
		procedure UpdateItem( Item: TKCompileItem ); virtual;

		function LoadFromIniStream( Stream: TStream ): TKDCC32LoadIniStatus; dynamic;
		procedure SaveToBatchStream( Stream: TStream ); dynamic;
		procedure SaveToIniStream( Stream: TStream ); dynamic;

		procedure MergeFromStream( Stream: TStream ); dynamic;
		procedure LoadFromStream( Stream: TStream ); dynamic;
		procedure SaveToStream( Stream: TStream ); dynamic;
		procedure DoLoadError( Item: TKCompileItem ); dynamic;

		property IsLoading: Boolean
						 read FIsLoading;
		property Quitting: Boolean
						 read FQuitting;
		property SaveIniLoadType: Boolean
						 read FSaveIniLoadType;
		property EnvironmentVars;
		property CaptureOutput;
		property ProcessPriority;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Assign( Source: TPersistent ); override;

		function Execute: Boolean; override;
		function ExecuteGroup( GroupIndex: Cardinal ): Boolean; dynamic;
		function ExecuteItems( const Items: array of Cardinal ): Boolean; dynamic;
		function ExecuteItemsEx( const Items: Variant ): Boolean; dynamic;

		procedure Quit;

		procedure MergeFromFile( const FileName: string );
		procedure LoadFromFile( const FileName: string );
		function LoadFromIniFile( const FileName: string ): TKDCC32LoadIniStatus;
		procedure SaveToFile( const FileName: string );
		procedure SaveToBatchFile( FileName: string ); dynamic;
		procedure SaveToIniFile( const FileName: string; IsLoad: Boolean );

	published
		property InitialDirectory;

		property CompileItems: TKCompileItems
						 read FCompileItems write SetCompileItems;
		property OnGroupAbort: TKDCC32GroupAbortEvent
						 read FOnGroupAbort write FOnGroupAbort;
		property OnLoadError: TKDCC32LoadItemErrorEvent
						 read FOnLoadError write FOnLoadError;

	end;

procedure ParseDOFFileToCompileItem( Item: TKCompileItem );

{
--------------------------------------------------------------------------------
---------------------------------- TKDFMData -----------------------------------
--------------------------------------------------------------------------------
}

type

{ TKDFMData }

	TKDFMData = class( TKCustomLinkable )
	private
		FData: Pointer;
		FDataSize: Cardinal;

		procedure FreeData;
		function GetSignature: TGUID;
		procedure ReadSignature( Reader: TReader );
		procedure WriteSignature( Writer: TWriter );

	protected
		procedure DefineProperties( Filer: TFiler ); override;

		function GetData: Pointer; virtual;
		procedure ReadData( Stream: TStream ); dynamic;
		procedure WriteData( Stream: TStream ); dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Data: Pointer
						 read GetData;
		property Signature: TGUID
						 read GetSignature;

	published
		property DataSize: Cardinal
						 read FDataSize write FDataSize;

	end;

{
--------------------------------------------------------------------------------
------------------------------- TKDFMResource ----------------------------------
--------------------------------------------------------------------------------
}

	TKDFMResource = class;

	{ TKDFMResourceStream }

	TKDFMResourceStream = class( TPersistent )
	private
		FOwner: TKDFMResource;
		FStream: TMemoryStream;
		FReadSpecialData: Boolean;

	protected
		procedure DefineProperties( Filer: TFiler ); override;

		procedure ReadData( Stream: TStream ); dynamic;
		procedure WriteData( Stream: TStream ); dynamic;
		procedure SetResource( Value: TStream ); virtual;

		property Owner: TKDFMResource
						 read FOwner;

	public
		destructor Destroy; override;
		constructor Create( ADFMResource: TKDFMResource ); virtual;

		property Stream: TMemoryStream
						 read FStream;

	end;

	TKDFMResourceType = ( drtBitmap, drtIcon, drtMetaFile, drtPointer, drtString,
		drtStream, drtWAVFile, drtAVIFile );

	{ TKDFMResource }

	TKDFMResource = class( TKCustomLinkable )
	private
		FIcon: TIcon;
		FBitmap: TBitmap;
		FStrData: string;
		FPointer: Pointer;
		FMetaFile: TMetaFile;
		FResourceType: TKDFMResourceType;
		FDFMResourceStream: TKDFMResourceStream;

		function GetAsBitmap: TBitmap;
		function GetAsIcon: TIcon;
		function GetAsMetaFile: TMetaFile;
		function GetAsPointer: Pointer;
		function GetAsString: string;
		function GetAsStream: TMemoryStream;
		function GetAsWAVFile: string;
		function GetAsAVIFile: string;
		function GetDataSize: Cardinal;
		procedure SetResource( Value: TKDFMResourceStream );
		procedure SetResourceType( Value: TKDFMResourceType );

	protected
		procedure GetData; dynamic;
		procedure FreeData; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property AsBitmap: TBitmap
						 read GetAsBitmap;
		property AsIcon: TIcon
						 read GetAsIcon;
		property AsMetaFile: TMetaFile
						 read GetAsMetaFile;
		property AsPointer: Pointer
						 read GetAsPointer;
		property AsString: string
						 read GetAsString;
		property AsStream: TMemoryStream
						 read GetAsStream;
		property AsWavFile: string
						 read GetAsWAVFile;
		property AsAVIFile: string
						 read GetAsAVIFile;
		property DataSize: Cardinal
						 read GetDataSize;

	published
		property ResourceType: TKDFMResourceType
						 read FResourceType write SetResourceType default drtBitmap;
		property Resource: TKDFMResourceStream
						 read FDFMResourceStream write SetResource;

	end;

{
--------------------------------------------------------------------------------
--------------------------------- TKCustomDump ---------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomDump }

	EKCustomDump = class( EKEClasses );

	TKCustomDump = class( TCustomControl )
	private
		FActive: Boolean;
		FAddress: Pointer;
		FDataSize: Integer;
		FSearching: Boolean;
		FPainting: Boolean;
		FPrinting: Boolean;
		FFileName: string;
		FItemHeight: Integer;
		FItemWidth: Integer;
		FBorder: TBorderStyle;
		FSelDataColor: TColor;
		FSelDataBackColor: TColor;
		FMemoryStream: TMemoryStream;
		FTopLine: Integer;
		FLineCount: Integer;
		FBytesPerLine: Byte;
		FVisibleLines: Integer;
		FCurrentLine: Integer;
		FBackLineOffSet: ShortInt;
		FOnSearchAddress: TNotifyEvent;

		procedure CalcPaintParams;
		procedure AdjustScrollBars;
		procedure SetItemMetrics;

		procedure SetFileName( const Value: string );
		procedure SetTopLine( Value: Integer );
		procedure SetCurrentLine( Value: Integer );
		procedure SetBorder( Value: TBorderStyle );
		procedure SetAddress( Value: Pointer );
		procedure SetDataSize( Value: Integer );
		procedure SetStrAddress( const Value: string );
		function GetStrAddress: string; 

		procedure CMParentFontChanged( var Message: TMessage );
							message CM_PARENTFONTCHANGED;
		procedure CMFontChanged( var Message: TMessage );
							message CM_FONTCHANGED;
		procedure CMEnter( var Message: TCMGotFocus );
							message CM_ENTER;
		procedure CMExit( var Message: TCMLostFocus );
							message CM_EXIT;
		procedure WMSize( var Message: TWMSize );
							message WM_SIZE;
		procedure WMGetDlgCode( var Message: TWMGetDlgCode );
							message WM_GETDLGCODE;
		procedure WMVScroll( var Message: TWMVScroll );
							message WM_VSCROLL;

	protected
		procedure Paint; override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
		procedure CreateParams( var Params: TCreateParams ); override;

		function GetDataColor( Index: Integer ): TColor; virtual;
		procedure SetDataColor( Index: Integer; Value: TColor ); virtual;

		procedure ForceAddress; dynamic;
		procedure ForceOffSet( OffSet: Integer ); dynamic;

		procedure DoSearchAddress; dynamic;
		procedure InvalidateLineMarker; virtual;

		function CalcLineCount: Integer; virtual;
		function CalcVisibleLines: Integer; virtual;
		function CalcBytesPerLine: Byte; virtual; abstract;

		function PreparePaint( PaintSearch: Boolean; var rt: TRect ): Boolean; virtual; abstract;
		function ProcessPaint( PaintSearch: Boolean; var rt: TRect; Line: Integer ): Boolean; virtual; abstract;

		function ScrollIntoView: Boolean;

		procedure LoadFromStream( Stream: TStream ); dynamic;
		procedure SaveToTextStream( Stream: TStream; ABytesPerLine: Byte
			{$IFDEF DELPHI4}= 0{$ENDIF} ); dynamic;
		procedure PrintLine( Stream: TStream; Line: Integer; ABytesPerLine: Byte
			{$IFDEF DELPHI4}= 0{$ENDIF} ); virtual; abstract;

		property Active: Boolean
						 read FActive;
		property Address: Pointer
						 read FAddress write SetAddress;
		property StrAddress: string
						 read GetStrAddress write SetStrAddress;
		property CurrentLine: Integer
						 read FCurrentLine write SetCurrentLine;
		property DataSize: Integer
						 read FDataSize write SetDataSize;
		property TopLine: Integer
						 read FTopLine write SetTopLine;
		property VisibleLines: Integer
						 read FVisibleLines;
		property LineCount: Integer
						 read FLineCount;
		property BytesPerLine: Byte
						 read FBytesPerLine;
		property ItemHeight: Integer
						 read FItemHeight;
		property ItemWidth: Integer
						 read FItemWidth;
		property Searching: Boolean
						 read FSearching;
		property Painting: Boolean
						 read FPainting;
		property Printing: Boolean
						 read FPrinting;
		property FileName: string
						 read FFileName write SetFileName;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure LoadFromFile( const AFileName: string );
		procedure SaveToTextFile( const AFileName: string; ABytesPerLine: Byte {$IFDEF DELPHI4}= 0{$ENDIF} );

		function FindRawAddress( Token: Pointer; TokenSize, OffSet: Integer ): Integer; virtual;
		function FindStrAddress( const Token: string; OffSet: Integer ): Integer; virtual;
		function SearchStrAddress( const Token: string; GoForward: Boolean ): Boolean; virtual;
		function SearchRawAddress( Token: Pointer; TokenSize: Integer; GoForward: Boolean ): Boolean; virtual;

	published
		property Align;
		property Color default clWhite;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property Constraints;
		property DragKind;
		property DockSite;
		property ParentBiDiMode;
	{$ENDIF}

		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnDockDrop;
		property OnDockOver;
		property OnEndDock;
		property OnGetSiteInfo;
		property OnStartDock;
		property OnUnDock;
	{$ENDIF}

		property BackLineOffSet: ShortInt
						 read FBackLineOffSet write FBackLineOffSet default 3;
		property Border: TBorderStyle
						 read FBorder write SetBorder default bsSingle;
		property SelDataColor: TColor
						 index SELDATA_COLOR_IDX read GetDataColor write SetDataColor default clCaptionText;
		property SelDataBackColor: TColor
						 index SELDATA_BACK_COLOR_IDX read GetDataColor write SetDataColor default clActiveCaption;
		property OnSearchAddress: TNotifyEvent
						 read FOnSearchAddress write FOnSearchAddress;

	end;

{ TKHexDump }

	EKHexDump = class( EKCustomDump );

	TKHexStr = array[0..2] of Char;
	TKLineAddress = array[0..MAX_HEXDUMP_DIGITS - 1] of Char;
	TKHexStrArray = array[0..MAX_HEXDUMP_DIGITS - 1] of TKHexStr;

	TKHexDump = class( TKCustomDump )
	private
		FShowCharacters: Boolean;
		FShowAddress: Boolean;
		FAddrColor: TColor;
		FHexDataColor: TColor;
		FAnsiCharColor: TColor;
		FHexData: TKHexStrArray;
		FLineAddr: TKLineAddress;
		FPaintTbStop: Integer;
		FPaintAddrWidth: Integer;
		FPaintBtnPerLine: Byte;

		procedure SetShowCharacters( Value: Boolean );
		procedure SetShowAddress( Value: Boolean );

		function LineAddr( Index: Integer ): PChar;   { Cool!!! and Fassst!!! }
		function LineData( Index: Integer ): PChar;   { Cool!!! and Fassst!!! }
		function LineChars( Index: Integer ): PChar;  { Cool!!! and Fassst!!! }

	protected
		function CalcBytesPerLine: Byte; override;
		procedure PrintLine( Stream: TStream; Line: Integer; ABytesPerLine: Byte
			{$IFDEF DELPHI4}= 0{$ENDIF} ); override;
		function PreparePaint( PaintSearch: Boolean; var rt: TRect ): Boolean; override;
		function ProcessPaint( PaintSearch: Boolean; var rt: TRect; Line: Integer ): Boolean; override;

		function GetDataColor( Index: Integer ): TColor; override;
		procedure SetDataColor( Index: Integer; Value: TColor ); override;

		function ForceHexAddress( const Token: string ): string; dynamic;

	public
		constructor Create( AOwner: TComponent ); override;

		function FindHexAddress( const Token: string; OffSet: Integer ): Integer; virtual;
		function SearchHexAddress( const Token: string; GoForward: Boolean ): Boolean; virtual;

		property Active;
		property Address;
		property StrAddress;
		property CurrentLine;
		property DataSize;
		property TopLine;
		property VisibleLines;
		property LineCount;
		property BytesPerLine;
		property ItemHeight;
		property ItemWidth;
		property Searching;
		property Painting;
		property Printing;

	published
		property FileName;

		property ShowAddress: Boolean
						 read FShowAddress write SetShowAddress default True;
		property ShowCharacters: Boolean
						 read FShowCharacters write SetShowCharacters default True;
		property AnsiCharColor: TColor
						 index ANSICHAR_COLOR_IDX read GetDataColor write SetDataColor default clBlack;
		property AddressColor: TColor
						 index ADDR_COLOR_IDX read GetDataColor write SetDataColor default clBlack;
		property HexDataColor: TColor
						 index HEXDATA_COLOR_IDX read GetDataColor write SetDataColor default clBlack;

	end;

{ TKTextDump }

	TKTextDump = class( TKCustomDump )
	private
		FTextColor: TColor;
		FTextBlockLength: Byte;
		FTextAlignment: TAlignment;
		FWantTabs: Boolean;
		FWordBreak: Boolean;
		FWantReturns: Boolean;
		FTabStopCount: Byte;
		FPaintBtnPerLine: Byte;

		function LineText( Index: Integer ): PChar;

	protected
		function CalcLineCount: Integer; override;
		function CalcVisibleLines: Integer; override;
		function CalcBytesPerLine: Byte; override;
		procedure PrintLine( Stream: TStream; Line: Integer; ABytesPerLine: Byte
			{$IFDEF DELPHI4}= 0{$ENDIF} ); override;
		function PreparePaint( PaintSearch: Boolean; var rt: TRect ): Boolean; override;
		function ProcessPaint( PaintSearch: Boolean; var rt: TRect; Line: Integer ): Boolean; override;

		function GetDataColor( Index: Integer ): TColor; override;
		procedure SetDataColor( Index: Integer; Value: TColor ); override;

	public
		constructor Create( AOwner: TComponent ); override;

    property Active;
		property Address;
		property StrAddress;
		property CurrentLine;
		property DataSize;
		property TopLine;
		property VisibleLines;
		property LineCount;
		property BytesPerLine;
		property ItemHeight;
		property ItemWidth;
		property Searching;
		property Painting;
		property Printing;

	published
		property FileName;

		property TextColor: TColor
						 index TEXT_COLOR_IDX read GetDataColor write SetDataColor default clBlack;
		property TextBlockLength: Byte
						 read FTextBlockLength write FTextBlockLength default 80;
		property TextAlignment: TAlignment
						 read FTextAlignment write FTextAlignment default taLeftJustify;
		property TabStopCount: Byte
						 read FTabStopCount write FTabStopCount default 2;
		property WantTabs: Boolean
						 read FWantTabs write FWantTabs default True;
		property WordBreak: Boolean
						 read FWordBreak write FWordBreak default True;
		property WantReturns: Boolean
						 read FWantReturns write FWantReturns default True;
	end;

{
--------------------------------------------------------------------------------
-------------------------- Command Line Parser/Lexer ---------------------------
--------------------------------------------------------------------------------
}

	TKCmdLineToken = ( cltNone, cltCommand, cltMoreOptOpen, cltMoreOptClose,
		cltMoreOptItem );

  { TKCmdLineParsingFrame }

  TKCmdLineParsingFrame = class( TKParsingFrame )
  private
    FCmdLineToken: TKCmdLineToken;

  public
    constructor Create( AParser: TKCustomParser ); override;
    procedure RestoreFrame; override;

  end;

	{ TKCmdLineParser }

	TKCmdLineParser = class( TKCustomParser )
	private
		FCmdLineToken: TKCmdLineToken;

	protected
		function CheckHexaToken( P: PChar ): Boolean; override;
		function CheckSpecialToken( P: PChar ): Boolean; override;

		function GetStringCharSet: TKCharSet; override;
		function GetIntegerCharSet: TKCharSet; override;
		function GetFloatCharSet: TKCharSet; override;
		function GetBlankCharSet: TKCharSet; override;
		function GetSpecialCharSet: TKCharSet; override;

		function ProcessSymbol( var P: PChar ): TKTokenType; override;
		function ProcessString( var P: PChar ): TKTokenType; override;
		function ProcessSpecialToken( var P: PChar ): TKTokenType; override;

    function GetParsingFrameClass: TKParsingFrameClass; override;

	public
		constructor Create( AStream: TStringStream ); virtual;

		function CheckCmdLine( ACmdLineToken: TKCmdLineToken ): Boolean; virtual;
		procedure ForceCmdLine( ACmdLineToken: TKCmdLineToken ); virtual;

		property CmdLineToken: TKCmdLineToken
						 read FCmdLineToken write FCmdLineToken;

	end;

	{ TKCmdLineLexer }

	EKCmdLineLexer = class( EKLexer );

	TKCmdLineLexer = class( TKCustomLexer )
	private
		FCmds: TStrings;
		FCurrentCmd: Integer;
		FApplicationName: string;

		function GetStream: TStringStream;
		function GetCmdCount: Integer;
		function GetCmdValueByIndex( Value: Integer ): string;
		function GetCmdValue( const Value: string ): string;
		function GetCmdName( Value: Integer ): string;

		function DetectSwitch: Boolean;

		procedure AppName;
		procedure Options;
		function AddValues: Boolean;
		function Switches: Boolean;
		function Command: Boolean;
		function CmdName: Boolean;
		function MoreOpt: Boolean;
		function Opt: Boolean;
		function StringDef: Boolean;
		function Number: Boolean;
		function Ident: Boolean;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure Goal; override;

	protected
		function GetParser: TKCmdLineParser;
		function GetDefaultErrorClass: EKLexerClass; override;

		procedure MatchCmdLine( ACmdLineToken: TKCmdLineToken ); virtual;

		procedure CreateParser; override;

		property Parser: TKCmdLineParser
						 read GetParser;
		property Stream: TStringStream
						 read GetStream;

	public
		destructor Destroy; override;
		constructor CreateFromCmdLine( const ACmdLine: string ); virtual;
		constructor CreateDefault; virtual;

		procedure Reset; override;

		property Commands: TStrings
						 read FCmds;

		property ApplicationName: string
						 read FApplicationName;
		property CommandCount: Integer
						 read GetCmdCount;
		property Names[Index: Integer]: string
						 read GetCmdName;
		property Values[const Name: string]: string
						 read GetCmdValue;
		property ValuesByIndex[Index: Integer]: string
						 read GetCmdValueByIndex;

	end;

{
--------------------------------------------------------------------------------
-------------------------- Math Parser/Lexer/Solver ----------------------------
--------------------------------------------------------------------------------
}

{$M+}
	TKMathOp = ( moNone, moPlus, moMinus, moMul, moDiv, moMod, moDivInt, moPower, moShl,
		moShr, moOr, moAnd, moXor { , moNot } );
{$M-}

	TKMathToken = ( mtNone, mtEndOfExpression, mtParentesisOpen, mtBracesOpen,
    mtBracketsOpen, mtParentesisClose, mtBracesClose, mtBracketsClose, mtFuncExprListSep );

  { TKMathParsingFrame }

  TKMathParsingFrame = class( TKParsingFrame )
  private
    FMathOp: TKMathOp;
		FMathToken: TKMathToken;

  public
    constructor Create( AParser: TKCustomParser ); override;
    procedure RestoreFrame; override;

  end;

{ TKMathParser }

	{$IFDEF DELPHI4}
	{$HINTS OFF}
	{$ENDIF}

	TKMathParser = class( TKCustomParser )
	private
		FMathOp: TKMathOp;
		FMathToken: TKMathToken;

		function CheckSymbolToken( P: PChar ): Boolean; override;
		function CheckStringToken( P: PChar ): Boolean; override;
		function CheckRelOpToken( P: PChar ): Boolean; override;
		function CheckCommentToken( P: PChar ): Boolean; override;

		function CheckHexaToken( P: PChar ): Boolean; override;

		function GetSymbolCharSet: TKCharSet; override;
		function GetStringCharSet: TKCharSet; override;
		function GetIntegerCharSet: TKCharSet; override;
		function GetFloatCharSet: TKCharSet; override;
		function GetCommentCharSet: TKCharSet; override;
		function GetRelOpCharSet: TKCharSet; override;
		function GetBlankCharSet: TKCharSet; override;
		function InternalNextToken( var P: PChar ): TKTokenType; override;

	protected
		function CheckSpecialToken( P: PChar ): Boolean; override;
		function GetSpecialCharSet: TKCharSet; override;

		function CheckMathOpToken( P: PChar ): Boolean; virtual;
		class function GetMathOpCharSet: TKCharSet; virtual;

		function ProcessSpecialToken( var P: PChar ): TKTokenType; override;
		function ProcessMathOpToken( var P: PChar ): TKTokenType; virtual;

    function GetParsingFrameClass: TKParsingFrameClass; override;

	public
		constructor Create( AExpr: TStringStream ); virtual;

		function CheckString( const AString: string ): Boolean; override;
		function CheckSymbol( const Symbol: string ): Boolean; override;
		function CheckRelOp( ARelOp: TKRelOp ): Boolean; override;

		function CheckMathOp( AMathOp: TKMathOp ): Boolean; virtual;
		function CheckMathToken( AMathToken: TKMathToken ): Boolean; virtual;
		function CheckIdent( const Ident: string ): Boolean; virtual;

		procedure ForceMathOp( AMathOp: TKMathOp ); virtual;
		procedure ForceMathToken( AMathToken: TKMathToken ); virtual;
		procedure ForceIdent( const Ident: string ); virtual;

		function TokenIdent: string; virtual;

		property MathOp: TKMathOp
						 read FMathOp write FMathOp;
		property MathToken: TKMathToken
						 read FMathToken write FMathToken;

	end;
  {$IFDEF DELPHI4}
	{$HINTS ON}
	{$ENDIF}


{ TKMathLexer }

	TKMathExpressions = class;
	TKMathLexer = class;
	EKMathLexer = class( EKLexer );

	TKMathExprTermKind = ( mtkMathOp, mtkNumber, mtkIdent, mtkFunc, mtkFuncExpr );

	PKMathExprTerm = ^TKMathExprTerm;
	TKMathExprTerm = packed record
		case MathExprTermKind: TKMathExprTermKind of
			mtkMathOp  : ( MathOp: TKMathOp );
			mtkNumber  : ( nValue: Extended );
			mtkIdent   : ( iValue: Extended; IdentName: PChar );
			mtkFunc    : ( fValue: Extended; FuncName: PChar );
			mtkFuncExpr: ( ExprIdx: Integer
										{ ExprPos: Word; ExprCnt: Byte; RefFuncName: PChar } );
	end;

{ TKMathExpression }

	TKMathExprKind = ( mekNormal, mekFuncExpr );

	TKMathExpression = class( TObject )
	private
		FData: Pointer;
    FOwnData: Boolean;
		FIndex: Integer;
		FMathExprTermList: TList;
		FOwner: TKMathExpressions;
		FMathExpressionKind: TKMathExprKind;

		function GetCounters( GetIdx: Integer ): Integer;
		function GetMathExprTermCount: Integer;
		function GetMathOp( Index: Integer ): TKMathOp;
		function GetNumber( Index: Integer ): Extended;
		function GetIdentName( Index: Integer ): string;
		function GetIdentValue( Index: Integer ): Extended;
		function GetFuncName( Index: Integer ): string;
		function GetFuncValue( Index: Integer ): Extended;
		function GetMathTerms( Index, GetIdx: Integer ): TKMathExprTerm;

		procedure SetIdentValue( Index: Integer; Value: Extended );
		procedure SetFuncValue( Index: Integer; Value: Extended );

	protected
		function GetMathTermsIndex( Index: Integer; GetIdx: TKMathExprTermKind ): Integer; dynamic;
    procedure DisposeData; dynamic;

		property MathExprTermCount: Integer
						 read GetMathExprTermCount;
		property MathExprTerms[Index: Integer]: TKMathExprTerm
						 index 0 read GetMathTerms;
		property MathOpTerms[Index: Integer]: TKMathExprTerm
						 index 1 read GetMathTerms;
		property NumberTerms[Index: Integer]: TKMathExprTerm
						 index 2 read GetMathTerms;
		property IdentTerms[Index: Integer]: TKMathExprTerm
						 index 3 read GetMathTerms;
		property FuncTerms[Index: Integer]: TKMathExprTerm
						 index 4 read GetMathTerms;

	public
		destructor Destroy; override;
		constructor Create( AMathExpr: TKMathExpressions ); virtual;

		function Solve: Extended; virtual; { after all can have an abstract base class... }

		function IndexOfFuncName( const AFuncName: string ): Integer; virtual;
		function IndexOfIdentName( const AnIdentName: string ): Integer; virtual;

		procedure Clear; virtual;
		procedure AddMathOp( AMathOp: TKMathOp ); virtual;
		procedure AddNumber( Parser: TKMathParser ); virtual;
		procedure AddFuncExpr( FuncExprIdx: Integer ); virtual;
		procedure AddIdent( const Ident: string ); virtual;
		procedure IdentToFunc( IdentId: Integer ); virtual;

		property Owner: TKMathExpressions
						 read FOwner;
		property Index: Integer
						 read FIndex;
		property MathOp[Index: Integer]: TKMathOp
						 read GetMathOp;
		property Numbers[Index: Integer]: Extended
						 read GetNumber;
		property IdentName[Index: Integer]: string
						 read GetIdentName;
		property IdentValue[Index: Integer]: Extended
						 read GetIdentValue write SetIdentValue;
		property FuncName[Index: Integer]: string
						 read GetFuncName;
		property FuncValue[Index: Integer]: Extended
						 read GetFuncValue write SetFuncValue;

		property MathOpCount: Integer
						 index 0 read GetCounters;
		property NumberCount: Integer
						 index 1 read GetCounters;
		property IdentCount: Integer
						 index 2 read GetCounters;
		property FuncCount: Integer
						 index 3 read GetCounters;

		property MathExpressionKind: TKMathExprKind
						 read FMathExpressionKind;

		property Data: Pointer
						 read FData write FData;
    property OwnData: Boolean
             read FOwnData write FOwnData;

	end;

{ TKMathExpressions }

	PKMathExprUserFunc = ^TKMathExprUserFunc;
	TKMathExprUserFunc = function ( Sender: TKMathExpression; const FuncName: string;
		FuncParams: TKExtendedStack ): Extended of object;

	TKMathExprSolveEvent = procedure( Expr: TKMathExpression; ExprKind: TKMathExprKind;
		const StrExpr: string; const ReturnValue: Extended ) of object;
	TKMathExprSolveIdentEvent = procedure( Expr: TKMathExpression; const IdentName: string;
		var IdentValue: Extended ) of object;
	TKMathExprSolveFuncEvent = procedure( Expr: TKMathExpression; const FuncName: string;
		FuncParams: TKExtendedStack; var FuncReturn: Extended ) of object;

	TKMathExpressions = class( TObject )
	private
		FOwner: TKMathLexer;
		FExpressions: TStrings;
		FDefFuncResult: Extended;
		FDefIdentResult: Extended;
		FOnSolve: TKMathExprSolveEvent;
		FOnSolveFunc: TKMathExprSolveFuncEvent;
		FOnSolveIdent: TKMathExprSolveIdentEvent;

		function GetExpr( Index, GetIdx: Integer ): TKMathExpression;
		function GetStrExpr( Index, GetIdx: Integer ): string;
		function GetExprCount( GetIdx: Integer ): Integer;
		procedure SetStrExpr( Index, GetIdx: Integer; const Value: string );

	protected
		function GetExprIdx( Index: Integer; GetIdx: TKMathExprKind ): Integer; dynamic;

		procedure DoSolve( ExprIdx: Integer; const ReturnValue: Extended ); dynamic;
		function DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended; dynamic;
		function DoSolveFunc( ExprIdx: Integer; MathTermIdx: Integer;
			FuncParams: TKExtendedStack ): Extended; dynamic; { the MathTermIdx is absolute }

		property OnSolve: TKMathExprSolveEvent
						 read FOnSolve write FOnSolve;
		property OnSolveIdent: TKMathExprSolveIdentEvent
						 read FOnSolveIdent write FOnSolveIdent;
		property OnSolveFunc: TKMathExprSolveFuncEvent
						 read FOnSolveFunc write FOnSolveFunc;

	public
		destructor Destroy; override;
		constructor Create( AMathLexer: TKMathLexer ); virtual;

		function NewExpr: Integer; virtual;
		function NewFuncExpr( ExprIdx: Integer ): Integer; virtual;
		procedure Clear; virtual;

		property Owner: TKMathLexer
						 read FOwner;
		property StrExprs[Index: Integer]: string
						 index 0 read GetStrExpr write SetStrExpr;

		property NormalStrExprs[Index: Integer]: string
						 index 1 read GetStrExpr write SetStrExpr;
		property FunctionStrExprs[Index: Integer]: string
						 index 2 read GetStrExpr write SetStrExpr;

		property Expressions[Index: Integer]: TKMathExpression
						 index 0 read GetExpr; default;
		property ExpressionCount: Integer
						 index 0 read GetExprCount;

		property NormalExpressions[Index: Integer]: TKMathExpression
						 index 1 read GetExpr;
		property NormalExpressionCount: Integer
						 index 1 read GetExprCount;
		property FuncExpressions[Index: Integer]: TKMathExpression
						 index 2 read GetExpr;
		property FuncExpressionCount: Integer
						 index 2 read GetExprCount;

		property DefFuncResult: Extended
						 read FDefFuncResult write FDefFuncResult;
		property DefIdentResult: Extended
						 read FDefIdentResult write FDefIdentResult;

	end;

	TKMathLexer = class( TKCustomLexer )
	private
		FLastMathOp: TKMathOp;
		FExprList: TKMathExpressions;

		function GetStream: TStringStream;

		{ Grammar }

		procedure List;
		procedure Expr( ExprIdx: Integer );
		procedure Term( ExprIdx: Integer );
		procedure MoreTerms( ExprIdx: Integer );
		procedure MoreFactors( ExprIdx: Integer );
		procedure Factor( ExprIdx: Integer );
		procedure Func( ExprIdx: Integer );
		procedure FuncExpr( ExprIdx: Integer; var FcnExprList: string );
		function Number: Boolean;
		function Ident: Boolean;
		procedure ProcessExpr( Action: ShortInt; ExprIdx: Integer );

	protected
		procedure Goal; override;
		function GetParser: TKMathParser;
		function GetDefaultErrorClass: EKLexerClass; override;
		function GetValidTokenTypes: TKCharSet; override;

		procedure MatchMathToken( AMathToken: TKMathToken ); virtual;
		procedure MatchMathOp( AMathOp: TKMathOp ); virtual;
		procedure MatchIdent( const Ident: string ); virtual;

		procedure CreateParser; override;

	protected
		procedure InternalSetExpression( const ExprList: string ); virtual;

		property Parser: TKMathParser
						 read GetParser;
		property Stream: TStringStream
						 read GetStream;

	public
		destructor Destroy; override;
		constructor CreateDefault; virtual;
		constructor CreateFromExpression( const AExpr: string ); virtual;
		constructor CreateFromExpressions( AExprList: TStrings ); virtual;

		procedure SetExpression( const Expr: string ); virtual;
		procedure SetExpressions( ExprList: TStrings; Option: TKStringType ); virtual;

		procedure Reset; override;

		property ExprList: TKMathExpressions
						 read FExprList;

	end;

{ TKMathExprRegTerm }

  TKMathExprRegTerm = class( TObject )
  private
    FGroupID: ShortInt;
    FComment: string;

    constructor Create( AGroupID: ShortInt; const AComment: string );

  public
    property Comment: string
             read FComment;
    property GroupID: ShortInt
             read FGroupID;

  end;

{ TKMathExprRegGroup }

  TKMathExprRegGroup = class( TKMathExprRegTerm )
  private
    FGroupName: string;

  protected
    constructor Create( AGroupID: ShortInt; const AGroupName, AComment: string ); virtual;

  public
    property GroupID;
    property Comment;

    property GroupName: string
             read FGroupName;
    
  end;

{ TKMathExprRegFunc }

  TKMathExprRegFunc = class( TKMathExprRegTerm )
  private
    FFormula: string;
    FFuncName: string;
    FParamCount: Integer;
    FParamNames: TStrings;
    FUserFunc: TKMathExprUserFunc;

    function GetParamNames( Index: Integer ): string;

    constructor Create( AGroupID: ShortInt; const AFuncName, AFormula, AComment,
      AParamNames: string; AParamCount: Integer; AUserFunc: TKMathExprUserFunc ); virtual;

  public
    destructor Destroy; override;

    property Comment;
    property GroupID;
    
    property FuncName: string
             read FFuncName;
    property Formula: string
             read FFormula;
    property ParamCount: Integer
             read FParamCount;
    property ParamNames[Index: Integer]: string
             read GetParamNames;
    property UserFunc: TKMathExprUserFunc
             read FUserFunc;

  end;

{ TKMathExprRegIdent }

  TKMathExprRegIdent = class( TKMathExprRegTerm )
  private
    FIdentName: string;
    FIdentValue: Extended;

    constructor Create( AGroupID: ShortInt; const AIdentName, AComment: string;
      AIdentValue: Extended ); virtual;

  public
    property Comment;
    property GroupID;
    
    property IdentName: string
             read FIdentName;
    property IdentValue: Extended
             read FIdentValue;

  end;

{ TKMathExprTermStrings }

	TKMathExprTermStrings = class( TStringList )
	public
		destructor Destroy; override;
    procedure Clear; override;
    procedure Delete( Index: Integer ); override;

	end;

{ TKCustomMathSolver }

	EKMathSolver = class( EKRClasses );
	TKCustomMathSolver = class;

  TKMathExprRegType = ( mertGroup, mertFunc, mertIdent );
  TKMathSolverExprKind = ( msekAll, msekNormal, msekFuncExpr );

	TKMathSolverEvent = procedure( Sender: TKCustomMathSolver; Expr: TKMathExpression;
		ExprKind: TKMathExprKind; const StrExpr: string; const ReturnValue: Extended ) of object;
	TKMathSolverIdentEvent = procedure( Sender: TKCustomMathSolver; Expr: TKMathExpression;
		const IdentName: string; var IdentValue: Extended ) of object;
	TKMathSolverFuncEvent = procedure( Sender: TKCustomMathSolver; Expr: TKMathExpression;
		const FuncName: string; FuncParams: TKExtendedStack; var FuncReturn: Extended ) of object;

  TKMathExprRegTermCallBack = function( Index: Integer; RegInfoType: TKMathExprRegType;
    RegItem: TKMathExprRegTerm; Data: Pointer ): Boolean of object;

	TKCustomMathSolver = class( TKCustomLinkable )
	private
		FActive: Boolean;
    FSolvingAll: Boolean;
		FAutoReOpen: Boolean;
		FDefFuncResult: Extended;
		FDefIdentResult: Extended;
		FIdentifiers: TStrings;
		FExpressions: TStrings;
		FMathLexer: TKMathLexer;
    FSolveStrMathLexer: TKMathLexer;
		FExpressionType: TKStringType;
		FOnSolve: TKMathSolverEvent;
		FOnExpressionsChanged: TNotifyEvent;
		FOnIdentifiersChanged: TNotifyEvent;
		FOnSolveFunc: TKMathSolverFuncEvent;
		FOnSolveIdent: TKMathSolverIdentEvent;
		FSolverExprKind: TKMathSolverExprKind;

		function GetIdentCount: Integer;
		function GetExpressionCount: Integer;
		function GetExprList: TKMathExpressions;
		function GetExpression( Index: Integer ): string;
		function GetIdentNames( Index: Integer ): string;
		function GetIdentValues( const Index: string ): Extended;
		procedure SetIdentNames( Index: Integer; const Value: string );
		procedure SetIdentValues( const Index: string; Value: Extended );
		procedure SetActive( Value: Boolean );
		procedure IdentifiersChanged( Sender: TObject );
		procedure ExpressionsChanged( Sender: TObject );

		procedure SolveEvent( Sender: TKMathExpression; ExprKind: TKMathExprKind;
			const Expr: string; const ReturnValue: Extended );
		procedure SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
			var IdentValue: Extended );
		procedure SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
			FuncParams: TKExtendedStack; var FuncReturn: Extended );

		function DefaultFunctionHandler( Sender: TKMathExpression; const FuncName: string;
			FuncParams: TKExtendedStack ): Extended;
      
	protected
    class function GetRegGroupCount: Integer;
    class function IndexRegGroupOfGroupName( GroupID: ShortInt; const GroupName: string ): Integer;
    class function GroupRegistered( GroupID: ShortInt; const GroupName: string ): Boolean;
    class function GroupIDRegistered( GroupID: ShortInt ): Boolean;
    class function GetRegGroups( Index: Integer ): TKMathExprRegGroup;

		class function GetRegFunctionCount: Integer;
		class function IndexRegFuncOfFuncName( const FuncName: string ): Integer;
		class function FunctionRegistered( const FuncName: string ): Boolean;
    class function GetRegFuncs( Index: Integer ): TKMathExprRegFunc;

		class function GetRegIdentCount: Integer;
		class function IndexRegIdentOfIdentName( const IdentName: string ): Integer;
		class function IdentRegistered( const IdentName: string ): Boolean;
    class function GetRegIdents( Index: Integer ): TKMathExprRegIdent;

    procedure SetExpressions( Value: TStrings ); virtual;
		procedure SetIdentifiers( Value: TStrings ); virtual;

    function GetExpressionsClass: TStringListClass; virtual; abstract;
    function GetIdentifiersClass: TStringListClass; virtual; abstract;

    procedure CreateIdentifiers; virtual;
    procedure CreateExpressions; virtual;

    class procedure RegisterDefaultGroups; dynamic;
		class procedure RegisterDefaultIdentifiers; dynamic;
    procedure RegisterDefaultFunctions; dynamic;

		procedure ReOpen; dynamic;
		procedure DoIdentifiersChanged; dynamic;
		procedure DoExpressionsChanged; dynamic;
		procedure DoSolve( ExprIdx: Integer; const ReturnValue: Extended ); dynamic;
		function DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended; dynamic;
		function DoSolveFunc( ExprIdx: Integer; const FuncName: string;
			FuncParams: TKExtendedStack ): Extended; dynamic;

{
    property RegGroupCount: Integer
             read GetRegGroupCount;
    property RegGroups[Index: Integer]: TKMathExprRegGroup
             read GetRegGroups;           
		property RegFuncCount: Integer
						 read GetRegFunctionCount;
    property RegFuncs[Index: Integer]: TKMathExprRegFunc
             read GetRegFuncs;
		property RegIdentCount: Integer
						 read GetRegIdentCount;
    property RegIdents[Index: Integer]: TKMathExprRegIdent
             read GetRegIdents;
}

		property MathLexer: TKMathLexer
						 read FMathLexer;
		property ExprList: TKMathExpressions
						 read GetExprList;
		property Expression[Index: Integer]: string
						 read GetExpression; default;
		property ExpressionCount: Integer
						 read GetExpressionCount;
		property IdentNames[Index: Integer]: string
						 read GetIdentNames write SetIdentNames;
		property IdentValues[const Index: string]: Extended
						 read GetIdentValues write SetIdentValues;
		property IdentCount: Integer
						 read GetIdentCount;
    property SolvingAll: Boolean
             read FSolvingAll;

		property AutoReOpen: Boolean
						 read FAutoReOpen write FAutoReOpen default True;
		property DefFuncResult: Extended
						 read FDefFuncResult write FDefFuncResult;
		property DefIdentResult: Extended
						 read FDefIdentResult write FDefIdentResult;
		property Identifiers: TStrings
						 read FIdentifiers write SetIdentifiers;
		property ExpressionType: TKStringType
						 read FExpressionType write FExpressionType default stStrings;
		property Expressions: TStrings
						 read FExpressions write SetExpressions;
		property SolverExprKind: TKMathSolverExprKind
						 read FSolverExprKind write FSolverExprKind default msekAll;

{
  MUST BE DECLARED AFTER EXPRESSIONS AND IDENTIFIERS TO STREAMING SYSTEM MAKE
  THEIR WORK WELL. WE TRUST THE VALUE OF EXPRESSIONS TO READ IT!
}
		property Active: Boolean
						 read FActive write SetActive default False;

		property OnSolve: TKMathSolverEvent
						 read FOnSolve write FOnSolve;
		property OnSolveIdent: TKMathSolverIdentEvent
						 read FOnSolveIdent write FOnSolveIdent;
		property OnSolveFunc: TKMathSolverFuncEvent
						 read FOnSolveFunc write FOnSolveFunc;
		property OnExpressionsChanged: TNotifyEvent
						 read FOnExpressionsChanged write FOnExpressionsChanged;
		property OnIdentifiersChanged: TNotifyEvent
						 read FOnIdentifiersChanged write FOnIdentifiersChanged;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function SolveAll: Extended; virtual;
		function SolveExpr( ExprIdx: Integer ): Extended; virtual;
    function SolveStrExpr( const StrExpr: string ): Extended; virtual;

		procedure Open; virtual;
		procedure Close; virtual;

    class function RegisterGroup( GroupID: ShortInt; const GroupName, Comment: string ): Boolean;
    class function UnRegisterGroup( GroupID: ShortInt; const GroupName: string ): Boolean;

		class function RegisterFunction( GroupID: ShortInt; const FuncName, Formula,
      Comment, ParamNames: string; ParamCount: Integer; UserFunc: TKMathExprUserFunc ): Boolean; virtual;
		class function UnRegisterFunction( const FuncName: string ): Boolean; virtual;

		class function RegisterIdent( GroupID: ShortInt; const IdentName, Comment: string;
      IdentValue: Extended ): Boolean; virtual;
		class function UnRegisterIdent( const IdentName: string ): Boolean; virtual;

    class function EnumRegInfos( RegInfoType: TKMathExprRegType; Data: Pointer;
      CallBack: TKMathExprRegTermCallBack ): Integer;

	end;

{ TKMathSolver }

	TKMathSolver = class( TKCustomMathSolver )
  protected
    function GetExpressionsClass: TStringListClass; override;
    function GetIdentifiersClass: TStringListClass; override;

	public
    property SolvingAll;
	  property MathLexer;
		property ExprList;
		property Expression;
		property ExpressionCount;
		property IdentNames;
		property IdentValues;
		property IdentCount;

	published
		property AutoReOpen;
		property DefFuncResult;
		property DefIdentResult;
		property Identifiers;
		property ExpressionType;
		property Expressions;
		property SolverExprKind;

{
  MUST BE DECLARED AFTER EXPRESSIONS AND IDENTIFIERS TO STREAMING SYSTEM MAKE
  THEIR WORK WELL. WE TRUST THE VALUE OF EXPRESSIONS TO READ IT!
}
		property Active;

		property OnSolve;
		property OnSolveIdent;
		property OnSolveFunc;
		property OnExpressionsChanged;
		property OnIdentifiersChanged;

	end;

	TKPropertyExpression = class;
	EKPropertyExpression = class( EKEClasses );

{ TKPropExprTermStrings }

  TKPropExprTermStrings = class( TStringList )
  private
    FOwner: TKPropertyExpression;

 		procedure ReadExprs( Reader: TReader );
		procedure WriteExprs( Writer: TWriter );

  protected
    procedure DefineProperties( Filer: TFiler ); override;

    procedure SetOwner( AOwner: TKPropertyExpression );

  public
    property Owner: TKPropertyExpression
             read FOwner;

  end;

{ TKPropertyExpression }

	TKPropertyExpressionNotifySolveEvent = procedure( Sender: TKPropertyExpression;
		ExprIdx: Integer; Obj: TPersistent; PropInfo: PPropInfo; const EvalValue: Extended )
		of object;

	TKPropertyExpression = class( TKCustomMathSolver )
	private
		{$HINTS OFF}
		FAuxObject: TObject; { Cool!!! }
		{$HINTS ON}
    FLocked: Boolean;
    FAutoAdjustExpr: Boolean;
    FIDEHitCount: Integer;
    FIDENotify: TObject; { VERY COOL!!! }
    FOnLock: TNotifyEvent;
    FOnUnLock: TNotifyEvent;
		FOnNotifySolve: TKPropertyExpressionNotifySolveEvent;

    procedure SetLocked( Value: Boolean );

    function AppWndProc( var Message: TMessage ): Boolean;

    function DefaultPropExprFunctionHandler( Sender: TKMathExpression;
      const FuncName: string; FuncParams: TKExtendedStack ): Extended;

		property AutoReOpen;
		property SolverExprKind;
		property ExpressionType;

{$IFDEF DELPHI4}
  protected
{$ENDIF}
		procedure SetExpressions( Value: TStrings ); override;

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; AOperation: TOperation ); override;

{ Used in property editor }
		procedure SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
			var IdentValue: Extended );
		procedure SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
			FuncParams: TKExtendedStack; var FuncReturn: Extended );
    
{
  We need this level of indirection because the Delphi Hidden property streaming
  system. The default TStrings object stream properties via normal Add and do not
  call SetExpressions (for instance), because the TStrings property does not have
  any published property, instead it has a Hidden Strings property
}
    function GetIdentifiersClass: TStringListClass; override;
    function GetExpressionsClass: TStringListClass; override;
    procedure CreateExpressions; override;

		function DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended; override;
		function DoSolveFunc( ExprIdx: Integer; const FuncName: string;
			FuncParams: TKExtendedStack ): Extended; override;

		procedure DoSolve( ExprIdx: Integer; const Value: Extended ); override;

    procedure DoLock; dynamic;
    procedure DoUnLock; dynamic;
		procedure DoNotifySolve( ExprIdx: Integer; Obj: TPersistent; PropInfo: PPropInfo;
			const Value: Extended ); dynamic;

    procedure RegisterDefaultFunctions; override;
      
	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure RemoveExpr( const CompName, PropPath: string ); virtual;
		function AddExpr( const CompName, PropPath, Expr: string ): Integer; virtual;
		procedure RemoveAllExprs( const CompName: string ); virtual;
		procedure RenameAllExprs( const OldCompName, NewCompName: string ); virtual;
    procedure RemoveAllLeftSideExprs( const CompName: string ); virtual;
    procedure RemoveAllRightSideExprs( const CompName: string ); virtual;
    procedure AdjustRelativeExpr( const CompNameList: string ); virtual;

    procedure Lock; virtual;
    procedure UnLock; virtual;

		property Active;
    property SolvingAll;
		property MathLexer;
		property ExprList;
		property Expression;
		property ExpressionCount;
		property IdentNames;
		property IdentValues;
		property IdentCount;

	published
		property DefFuncResult;
		property DefIdentResult;
		property Identifiers;
		property Expressions;

    property Locked: Boolean
             read FLocked write SetLocked default True;
    property AutoAdjustExpr: Boolean
             read FAutoAdjustExpr write FAutoAdjustExpr default True;
		property OnNotifySolve: TKPropertyExpressionNotifySolveEvent
						 read FOnNotifySolve write FOnNotifySolve;
    property OnLock: TNotifyEvent
             read FOnLock write FOnLock;
    property OnUnLock: TNotifyEvent
             read FOnUnLock write FOnUnLock;

	end;

{
--------------------------------------------------------------------------------
-------------------------- Code Parser Architecture ----------------------------
--------------------------------------------------------------------------------
}

{
  This First parser is an improved version of Marco Cantù DDH book (chap2) parser
  The parser was adopted to become compatible our parser architecture. see ukrClasses.
  (Applies to TKCustomPascalParser and its subclasses)
}

  { TKCustomPascalParsingFrame }

	TKPascalToken = ( ptNone, ptDotDot, ptParensDot, ptDotParens, ptAssign );

  TKCustomPascalParsingFrame = class( TKCodeParsingFrame )
  private
		FPascalToken: TKPascalToken;

  public
    constructor Create( AParser: TKCustomParser ); override;
    procedure RestoreFrame; override;

  end;

	{ TKCustomPascalParser }

	EKPascalParser = class( EKCodeParser );

	TKCustomPascalParser = class( TKCustomCodeParser )
	private
		FPascalToken: TKPascalToken;
		FBeforeRelOp: TKBeforeCodeEvent;
		FAfterRelOp: TKCodeEvent;
		FBeforePascalToken: TKBeforeCodeEvent;
		FAfterPascalToken: TKCodeEvent;

{$IFDEF DELPHI4}
  protected
{$ENDIF}
	{ derived from TKCustomParser }
		function GetStringCharSet: TKCharSet; override;
		function GetIntegerCharSet: TKCharSet; override;
		function GetFloatCharSet: TKCharSet; override;
		function GetRelOpCharSet: TKCharSet; override;
		function GetSpecialCharSet: TKCharSet; override;
		function GetBlankCharSet: TKCharSet; override;

	protected
	{ derived from TKCustomParser }
		function GetDefaultErrorClass: EKParserClass; override;
    function GetParsingFrameClass: TKParsingFrameClass; override;

		function CheckHexaToken( P: PChar ): Boolean; override;
		function CheckRelOpToken( P: PChar ): Boolean; override;
		function CheckSpecialToken( P: PChar ): Boolean; override;
    function CheckSymbolToken( P: PChar ): Boolean; override;

		function ProcessString( var P: PChar ): TKTokenType; override;
		function ProcessRelOpToken( var P: PChar ): TKTokenType; override;
		function ProcessSpecialToken( var P: PChar ): TKTokenType; override;

	{ derived from TKCustomCodeParser }
		procedure DoBeforeRelOp( const ARelOp: string ); virtual;
		procedure DoAfterRelOp; virtual;
		procedure DoBeforePascalToken( const ASpecial: string ); virtual;
		procedure DoAfterPascalToken; virtual;

		procedure ProcessConversion( const s: string ); override;
		function MakeStringLegal( const S: string ): string; override;

	public
		function CheckPascalToken( APascalToken: TKPascalToken ): Boolean; virtual;
		procedure ForcePascalToken( APascalToken: TKPascalToken ); virtual;

		property OutPutString;
		property Line;
		property Position;
		property KeyWords;
		property FileName;
		property Target;
		property RelOp;

		property OnBuildFileHeader;
		property OnBuildFileFooter;
		property BeforeString;
		property AfterString;
		property BeforeKeyword;
		property AfterKeyword;
		property BeforeSymbol;
		property AfterSymbol;
		property BeforeComment;
		property AfterComment;
		property BeforeInteger;
		property AfterInteger;
		property BeforeHexa;
		property AfterHexa;
		property BeforeFloat;
		property AfterFloat;
		property OnCheckString;
		property OnCheckComment;
		property OnUnknownToken;
		property OnEndOfStream;
		property OnReadBuffer;

		property PascalToken: TKPascalToken
						 read FPascalToken write FPascalToken;
		property BeforeRelOp: TKBeforeCodeEvent
						 read FBeforeRelOp write FBeforeRelOp;
		property AfterRelOp: TKCodeEvent
						 read FAfterRelOp write FAfterRelOp;
		property BeforePascalToken: TKBeforeCodeEvent
						 read FBeforePascalToken write FBeforePascalToken;
		property AfterPascalToken: TKCodeEvent
						 read FAfterPascalToken write FAfterPascalToken;

	end;

	TKCommentType = ( ctNone, ctBraket, ctParentesis, ctLine );
    
  { TKPascalParsingFrame }

  TKPascalParsingFrame = class( TKCustomPascalParsingFrame )
  private
		FCommentType: TKCommentType;

  public
    constructor Create( AParser: TKCustomParser ); override;
    procedure RestoreFrame; override;

  end;

	{ TKPascalParser }

	TKPascalParser = class( TKCustomPascalParser )
	private
		FCommentType: TKCommentType;

	protected
	{ derived from TKCustomParser }
		function GetCommentCharSet: TKCharSet; override;
		function CheckCommentToken( P: PChar ): Boolean; override;
		function ProcessComment( var P: PChar ): TKTokenType; override;

	{ derived from TKCustomCodeParser }
		function GetDefaultKeyWords: string; override;
    function GetParsingFrameClass: TKParsingFrameClass; override;
    
		function GetCommentType( P: PChar ): TKCommentType; virtual;

	public
		function TokenComponentIdent: string; virtual;

		property CommentType: TKCommentType
						 read FCommentType;

	end;

	{ TKDFMParser }

	TKDFMParser = class( TKCustomPascalParser )
	protected
	{ derived from TKCustomCodeParser }
		function GetDefaultKeyWords: string; override;

		function GetBinaryEndToken: Char; virtual;

	public
		procedure HexToBinary( AStream: TStream ); virtual;

	end;

	{ TKPascalToHtmlConverter }

	TKPascalToHtmlConverter = class( TKPascalParser )
	private
		FOnHtmlFooter: TKCodeEvent;
		FOnHtmlHeader: TKCodeEvent;

	protected
	{ derived from TKCustomCodeParser }
		procedure DoBuildHeader; override;
		procedure DoBuildFooter; override;
		procedure DoBeforeString( const Str: string ); override;
		procedure DoAfterString; override;
		procedure DoBeforeKeyword( const KeyWord: string ); override;
		procedure DoAfterKeyword; override;
		procedure DoBeforeSymbol( const Symbol: string ); override;
		procedure DoAfterSymbol; override;
		procedure DoBeforeComment( const Comment: string ); override;
		procedure DoAfterComment; override;
		procedure DoBeforeNumber( AType: TKTokenType; const Number: string ); override;
		procedure DoAfterNumber( AType: TKTokenType ); override;

		function ValidateSpecialToken( Ch: Char ): string; override;

		procedure DoHtmlHeader; dynamic;
		procedure DoHtmlFooter; dynamic;
		procedure DefaultHtmlHeader; virtual;
		procedure DefaultHtmlFooter; virtual;

	public
		property OnHtmlFooter: TKCodeEvent
						 read FOnHtmlFooter write FOnHtmlFooter;
		property OnHtmlHeader: TKCodeEvent
						 read FOnHtmlHeader write FOnHtmlHeader;

	end;

	{ TKDFMToHtmlConverter }

	TKDFMToHtmlConverter = class( TKDFMParser )
	private
		FOnHtmlFooter: TKCodeEvent;
		FOnHtmlHeader: TKCodeEvent;

	protected
	{ derived from TKCustomCodeParser }
		procedure DoBuildHeader; override;
		procedure DoBuildFooter; override;
		procedure DoBeforeString( const Str: string ); override;
		procedure DoAfterString; override;
		procedure DoBeforeKeyword( const KeyWord: string ); override;
		procedure DoAfterKeyword; override;
		procedure DoBeforeSymbol( const Symbol: string ); override;
		procedure DoAfterSymbol; override;
		procedure DoBeforeComment( const Comment: string ); override;
		procedure DoAfterComment; override;
		procedure DoBeforeNumber( AType: TKTokenType; const Number: string ); override;
		procedure DoAfterNumber( AType: TKTokenType ); override;

		function ValidateSpecialToken( Ch: Char ): string; override;

		procedure DoHtmlHeader; dynamic;
		procedure DoHtmlFooter; dynamic;
		procedure DefaultHtmlHeader; virtual;
		procedure DefaultHtmlFooter; virtual;

	public
		property OnHtmlFooter: TKCodeEvent
						 read FOnHtmlFooter write FOnHtmlFooter;
		property OnHtmlHeader: TKCodeEvent
						 read FOnHtmlHeader write FOnHtmlHeader;
						 	  
	end;

const

	tpMathOp = TKTokenType( 10 );

implementation

uses
	Consts, ComObj, ShellAPI, Math, EditIntf, ExptIntf, uksyResStr, ukrConsts, ukrResStr,
  ukeResStr, ukeUtils;

{
--------------------------------------------------------------------------------
--------------------------------- TKMessageSpy ---------------------------------
--------------------------------------------------------------------------------
}

type
	TWriterHack = class( TWriter );
	TControlHack = class( TControl );
	TWinControlHack = class( TWinControl );

{ TKMessageGroup }

constructor TKMessageGroup.Create( AOwner: TKMessageSpy );
begin
	inherited Create;
	FOwner := AOwner;
	FMsgEnums := imeDefMaps;
end;

destructor TKMessageGroup.Destroy;
begin
	FOwner := nil;
	inherited Destroy;
end;

procedure TKMessageGroup.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKMessageGroup ) then
		MsgEnums := TKMessageGroup( Source ).MsgEnums
	else
		inherited Assign( Source );
end;

function TKMessageGroup.GetMsgEnumsAsString: string;
var
	I: TIdentMsgMapEntryEnum;
begin
	Result := CH_BRACES_OPEN;
	for I := Low( TIdentMsgMapEntryEnum ) to High( TIdentMsgMapEntryEnum ) do
		if ( I in MsgEnums ) then
			Result := Result + imeNames[I] + CH_COMMA + CH_SPACE;
	if ( Length( Result ) > 1 ) then
		Delete( Result, Length( Result ) - 1, 2 );
	Result := Result + CH_BRACES_CLOSE;
end;

procedure TKMessageGroup.SetMsgEnumsAsString( const Value: string );
var
	i: Integer;
	bOk: Boolean;
	sl: TStrings;
	X: TIdentMsgMapEntryEnums;
begin
{The value must be <> '' and in the format: '[xx,xx,xx...]'}
	bOk := ( CheckStr( Value ) and ( Value[1] = CH_BRACES_OPEN ) and
				 ( Value[Length( Value )] = CH_BRACES_CLOSE ) );
	if ( not bOk ) then
		RaiseExceptionFmt( EKMessageSpy, sErrInvMsgEnum, [Value] );
	sl := TStringList.Create;
	try
		X := [];
		ExtractStrings( Copy( Value, 2, Length( Value ) - 2 ), CH_COMMA, sl );
		for i := 0 to sl.Count - 1 do
			Include( X, StringToMsgEnum( Trim( sl[i] ) ) );
		if ( X <> MsgEnums ) then
			MsgEnums := X;
	finally
		sl.Free;
	end;
end;

procedure TKMessageGroup.ReadData( Reader: TReader );

	procedure SkipSetBody;
	begin
		while ( Reader.ReadStr <> '' ) do ;
	end;

var
	EnumName: string;
	X: TIdentMsgMapEntryEnums;
begin
{Copied ( adapted ) from TReader.ReadSet private method }
	try
		if ( Reader.ReadValue <> vaSet ) then
			RaiseException( EKMessageSpy, SInvalidPropertyValue );
		X := [];
		while True do
		begin
			EnumName := Reader.ReadStr;
			if ( not CheckStr( EnumName ) ) then
				Break;
			Include( X, StringToMsgEnum( EnumName ) );
		end;
		MsgEnums := X;
	except
		SkipSetBody;
		raise;
	end;
end;

procedure TKMessageGroup.WriteData( Writer: TWriter );
var
	i: TIdentMsgMapEntryEnum;
begin
	with TWriterHack( Writer ) do
	begin
{ Writer mechanism for set properties streaming. See TWriter.WriteProperty }
		WriteValue( vaSet );
		for i:= Low( TIdentMsgMapEntryEnum ) to High( TIdentMsgMapEntryEnum ) do
			if ( i in MsgEnums ) then
				WriteStr( imeNames[i] );
		WriteStr( '' );
	end;
end;

procedure TKMessageGroup.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if ( Filer.Ancestor <> nil ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKMessageGroup ) then
				Result := ( MsgEnums <> TKMessageGroup( Filer.Ancestor ).MsgEnums );
		end
		else
			Result := ( MsgEnums <> imeDefMaps );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'MsgEnums', ReadData, WriteData, DoWrite );
end;

{ TKMessageSpy }

constructor TKMessageSpy.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FMessages := TKMessageGroup.Create( Self );
	FDefaultName := True;
	FClearFile := True;
	FActive := False;
	FLogParams := False;
	FLogUntranslatedMsg := False;
	FLogType := sltBoth;
	FDirectory := GetCurrentDir;
	FControlName := '';
	FOldDefWndProc := nil;
	FControl := nil;
	FWndProc := nil;
	FStream := nil;
	if ( not Designing( Self ) ) then
		FDefWndProc := MakeObjectInstance( InternalWndProc )
	else
		FDefWndProc := nil;
end;

destructor TKMessageSpy.Destroy;
begin
	FMessages.Free;
	if CheckObject( FControl ) then
	begin
		if CheckReference( @FWndProc ) then
			FControl.WindowProc := FWndProc;
		if CheckObjectClass( FControl, TWinControl ) and
			 CheckPointer( FOldDefWndProc ) then
			TWinControlHack( FControl ).DefWndProc := FOldDefWndProc;
	end;
	FControl := nil;
	FWndProc := nil;
	FOldDefWndProc := nil;
	if CheckPointer( FDefWndProc ) then
		FreeObjectInstance( FDefWndProc );
	FDefWndProc := nil;
	FreeStream;
	FControlName := '';
	inherited Destroy;
end;

procedure TKMessageSpy.Notification( AComponent: TComponent; AOperation: TOperation );
begin
	inherited Notification( AComponent, AOperation );
	if ( AComponent = FControl ) and ( AOperation = opRemove ) then
	begin
		FControl := nil;
		FWndProc := nil;
		FOldDefWndProc := nil;
	end;
end;

procedure TKMessageSpy.CheckWndProc;
begin
	if ( not ( CheckObject( FControl ) and CheckReference( @FWndProc ) ) ) then
		RaiseException( EKMessageSpy, sErrMSInvWndProc );
end;

procedure TKMessageSpy.CheckDefWndProc;
begin
	if ( not ( CheckObject( FControl ) and CheckPointer( FOldDefWndProc ) ) ) then
		RaiseException( EKMessageSpy, sErrMSInvDefWndProc );
end;

procedure TKMessageSpy.CheckDirectory;
begin
	if ( not CheckTrimStr( FDirectory ) ) then
		RaiseException( EKMessageSpy, sErrMSInvFileName );
end;

function TKMessageSpy.GetFileName: string;
begin
	CheckDirectory;
	Result := ( FDirectory + FControlName + SPY_MSG_EXT );
end;

procedure TKMessageSpy.CreateStream;
{
const
	wMode: array[Boolean] of Word =
		( ( fmCreate or fmShareExclusive ), ( fmShareExclusive or fmOpenRead ) );
}
begin
	Assert( FStream = nil );
	FStream := TKStringStream.CreateFromFile( FileName {, wMode[FileExists( FileName )] } );
	if ( FClearFile ) then
		FStream.Size := 0;
end;

procedure TKMessageSpy.FreeStream;
begin
	if CheckObject( FStream ) then
		try
			if FileExists( FileName ) and ( FClearFile ) then
				DeleteFile( FileName );
			FStream.SaveToFile( FileName );
		finally
		  FreeClean( FStream );
		end;
end;

procedure TKMessageSpy.WrapperWndProc( var Message: TMessage );
var
	s: string;
begin
	CheckWndProc;
	if ( Active and ( LogType in [sltWndProc, sltBoth] ) and CheckLog( Message, s ) ) then
		LogMessage( Message, s, True );
	FWndProc( Message );	
end;

procedure TKMessageSpy.InternalWndProc( var Message: TMessage );
var
	s: string;
begin
	CheckDefWndProc;
	if ( Active and ( LogType in [sltDefWndProc, sltBoth] ) and CheckLog( Message, s ) ) then
		LogMessage( Message, s, False );
	with Message do
		Result := CallWindowProc( FOldDefWndProc, TWinControlHack( FControl ).Handle,
			Msg, WParam, LParam );
end;

procedure TKMessageSpy.SaveToStrings( sl: TStrings );
var
	p: Integer;
begin
	if ( not CheckObject( FStream ) ) then
		RaiseException( EKMessageSpy, sErrMSInvStream );
	ForceObject( sl );
	sl.BeginUpdate;
	try
		sl.Clear;
		p := FStream.Position;{FStream.PushPos;}
		try
			FStream.Position := 0;
			sl.LoadFromStream( FStream );
		finally
			{FStream.PopPos;}
			FStream.Position := p;
		end;
	finally
		sl.EndUpdate;
	end;
end;

function TKMessageSpy.CheckLog( const Message: TMessage; var S: string ): Boolean;
begin
	S := MessageToStringEnums( Message.Msg, Messages.MsgEnums, Result );
	Result := ( Result or LogUnTranslatedMsg );
end;

procedure TKMessageSpy.LogMessage( const Message: TMessage; const sMsg: string;
	IsWndProc: Boolean );
const
	Value: array[Boolean] of string[3] = ( 'Def', 'Wnd' ); { Do not resource }
	MaxMsgLen = Length( 'WM_INPUTLANGCHANGEREQUEST' );
var
	s: string;
begin
	if ( not CheckObject( FStream ) ) then
		CreateStream;
	if ( DoConfirmLog( Message, sMsg, IsWndProc ) ) then
		with Message do
		begin
			s := sMsg;
			if ( not FLogParams ) then
				FStream.FormatLn( SPY_MSG_LOG_PATTERN , [Value[IsWndProc], MaxMsgLen, sMsg] )
			else
				FStream.FormatLn( SPY_MSG_LOG_PATTERN_EX , [Value[IsWndProc], MaxMsgLen,
					StrRightPad( s, MaxMsgLen + 2, CH_DOTMARK ), WParam, LParam, Result] );
		end;
end;

function TKMessageSpy.DoConfirmLog( const Message: TMessage; const sMsg: string;
	IsWndProc: Boolean ): Boolean;
begin
	Result := True;
	if Assigned( FOnConfirmLog ) then
		FOnConfirmLog( Self, Message, sMsg, IsWndProc, Result );
end;

procedure TKMessageSpy.AssignTo( Dest: TPersistent );
begin
{
  TKMessageGroup don't knows how to manage TKMessageSpy, but TKMessageSpy need this
	managment and it knows how to do it, so do it! Here is the place.
	PS: In the case i still have the best linkage policy ( see DanyThorpe chap 10 )!
}
	if CheckObjectClass( Dest, TKMessageGroup ) then
		( Dest as TKMessageGroup ).MsgEnums := FMessages.MsgEnums
	else
		inherited AssignTo( Dest );
end;

procedure TKMessageSpy.SetActive( const Value: Boolean );
begin
	FActive := Value;
end;

procedure TKMessageSpy.SetMessages( const Value: TKMessageGroup );
begin
	FMessages.Assign( Value );
end;

procedure TKMessageSpy.SetControl( const Value: TControl );
var
  bClear: Boolean;
begin
	if ( FControl <> Value ) then
	begin
		if CheckObject( FControl ) then
		begin
			if CheckReference( @FWndProc ) then
				FControl.WindowProc := FWndProc; {Restore old WndProc}
			if CheckObjectClass( FControl, TWinControl ) and
				 CheckPointer( FOldDefWndProc ) then
				TWinControlHack( FControl ).DefWndProc := FOldDefWndProc;
			FWndProc := nil;
			FOldDefWndProc := nil;
		end;
		bClear := True;
		FControl := Value;
		if CheckObject( FControl ) and ( not CheckObjectClass( FControl, TCustomForm ) ) then
			FControl.FreeNotification( Self );
		if ( not Designing( Self ) ) then
		begin
			FreeStream;
			if CheckObject( FControl ) then
			begin
				FWndProc := FControl.WindowProc;     {Assign and Save WndProc}
				FControl.WindowProc := WrapperWndProc;
				if CheckObjectClass( FControl, TWinControl ) then
				begin
					FOldDefWndProc := TWinControlHack( FControl ).DefWndProc;
					TWinControlHack( FControl ).DefWndProc := FDefWndProc;
				end;
				FControlName := FControl.Name;
				bClear := False;
			end;
		end;
		if bClear then
			FControlName := '';
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------------- TKFileScanner ---------------------------------
--------------------------------------------------------------------------------
}

{
	class TKFileScanner;

	Attributes 	 Atributos específicos para filtar os arquivos processados;
	Mask   		 	 Máscara para procura de arquivos ( ver TKMask );
	RecLevel	 	 Nível de recursividade. Utilizado se Recursive for true.
							 Sendo nulo, o nível de recursividade não é controlado;
	Recursive  	 Se True, processa sub-diretórios recursivamente;
	Scanning	   Retorna True se o evento OnScan está sendo executado;
	OnScan     	 Evento chamado para processar as informações;
							 Recebe o path e o nível de recursividade correntes e um
							 registro TSearchRec com as informações sobre o arquivo sendo
							 processado no momento. Se o parâmetro Cancel for mudado
							 para True, interromperá o processamento do Scan;
}

{--------------------------- Internal Implementation ---------------------------}

const
  MaxCards = 30;

type
	PKMaskSet = ^TKMaskSet;
	TKMaskSet = set of Char;
	TKMaskStates = ( msLiteral, msAny, msSet, msMBCSLiteral );
	TKMaskState = record
		SkipTo: Boolean;
		case State: TKMaskStates of
			msLiteral: ( Literal: Char );
			msAny: (  );
			msSet: (
				Negate: Boolean;
				CharSet: PKMaskSet );
			msMBCSLiteral: ( LeadByte, TrailByte: Char );
	end;
	PKMaskStateArray = ^TKMaskStateArray;
	TKMaskStateArray = array[0..128] of TKMaskState;

function InitMaskStates( const Mask: string; var MaskStates: array of TKMaskState ): Integer;
var
  I: Integer;
  SkipTo: Boolean;
  Literal: Char;
  LeadByte, TrailByte: Char;
	P: PChar;
  Negate: Boolean;
  CharSet: TKMaskSet;
  Cards: Integer;

	procedure InvalidMask;
	begin
		RaiseExceptionFmt( EKMaskList, sMInvalidMask, [Mask, P - PChar( Mask ) + 1] );
  end;

	procedure Reset;
  begin
		SkipTo := False;
		Negate := False;
		CharSet := [];
  end;

  procedure WriteScan( MaskState: TKMaskStates );
  begin
    if ( I <= High( MaskStates ) ) then
    begin
			if SkipTo then
			begin
				Inc( Cards );
        if Cards > MaxCards then InvalidMask;
      end;
      MaskStates[I].SkipTo := SkipTo;
      MaskStates[I].State := MaskState;
      case MaskState of
				msLiteral: MaskStates[I].Literal := UpCase( Literal );
        msSet:
					begin
            MaskStates[I].Negate := Negate;
            New( MaskStates[I].CharSet );
						MaskStates[I].CharSet^ := CharSet;
					end;
        msMBCSLiteral:
          begin
						MaskStates[I].LeadByte := LeadByte;
						MaskStates[I].TrailByte := TrailByte;
					end;
      end;
		end;
    Inc( I );
    Reset;
  end;

  procedure ScanSet;
  var
    LastChar: Char;
    C: Char;
  begin
		Inc( P );
    if ( P^ = CH_EXCLAMATION ) then
    begin
			Negate := True;
			Inc( P );
		end;
		LastChar := CH_NULL;
		while not ( P^ in [CH_NULL, CH_BRACES_CLOSE] ) do
		begin
{ MBCS characters not supported in msSet }
			if ( P^ in LeadBytes ) then
				 Inc( P )
			else
				case P^ of
					CH_DASH:
						if ( LastChar = CH_NULL ) then
							InvalidMask
						else
						begin
							Inc( P );
							for C := LastChar to UpCase( P^ ) do
								Include( CharSet, C );
						end;
				else
					LastChar := UpCase( P^ );
					Include( CharSet, LastChar );
				end;
			Inc( P );
		end;
		if ( P^ <> CH_BRACES_CLOSE ) or ( CharSet = [] ) then
			InvalidMask;
		WriteScan( msSet );
	end;

begin
	P := PChar( Mask );
	I := 0;
	Cards := 0;
	Reset;
	while ( P^ <> CH_NULL ) do
	begin
		case P^ of
			CH_ASTERISK: SkipTo := True;
			CH_QUESTIONMARK:
				if ( not SkipTo ) then
					WriteScan( msAny );
			CH_BRACES_OPEN:  ScanSet;
		else
			if ( P^ in LeadBytes ) then
			begin
				LeadByte := P^;
				Inc( P );
				TrailByte := P^;
				WriteScan( msMBCSLiteral );
			end
			else
			begin
				Literal := P^;
				WriteScan( msLiteral );
			end;
		end;
		Inc( P );
	end;
	Literal := CH_NULL;
	WriteScan( msLiteral );
	Result := I;
end;

function MatchesMaskStates( const Filename: string; MaskStates: array of TKMaskState ): Boolean;
type
  TStackRec = record
		sP: PChar;
    sI: Integer;
  end;
var
	T: Integer;
	S: array[0..MaxCards - 1] of TStackRec;
	I: Integer;
  P: PChar;

  procedure Push( P: PChar; I: Integer );
  begin
    with S[T] do
		begin
      sP := P;
      sI := I;
    end;
		Inc( T );
	end;

  function Pop( var P: PChar; var I: Integer ): Boolean;
  begin
		if ( T = 0 ) then
			Result := False
		else
    begin
      Dec( T );
      with S[T] do
      begin
        P := sP;
				I := sI;
      end;
      Result := True;
    end;
  end;

  function Matches( P: PChar; Start: Integer ): Boolean;
  var
		I: Integer;
	begin
		Result := False;
		for I := Start to High( MaskStates ) do
      with MaskStates[I] do
			begin
        if SkipTo then
        begin
          case State of
						msLiteral:
							while ( P^ <> CH_NULL ) and
										( UpperCase( P^ ) <> Literal ) do
								Inc( P );
            msSet:
							while ( P^ <> CH_NULL ) and
										( not ( Negate xor ( UpCase( P^ ) in CharSet^ ) ) ) do
								Inc( P );
            msMBCSLiteral:
							while ( P^ <> CH_NULL ) do
							begin
								if ( P^ <> LeadByte ) then
									Inc( P, 2 )
								else
								begin
									Inc( P );
									if ( P^ = TrailByte ) then
										Break;
									Inc( P );
								end;
							end;
					end;
					if ( P^ <> CH_NULL ) then
						Push( @P[1], I );
				end;
				case State of
					msLiteral:
						if ( UpperCase( P^ ) <> Literal ) then
							Exit;
					msSet:
						if ( not ( Negate xor ( UpCase( P^ ) in CharSet^ ) ) ) then
							Exit;
					msMBCSLiteral:
						begin
							if ( P^ <> LeadByte ) then
								Exit;
							Inc( P );
							if ( P^ <> TrailByte ) then
								Exit;
						end;
				end;
				Inc( P );
			end;
		Result := True;
	end;

begin
	Result := True;
	T := 0;
	P := PChar( Filename );
	I := Low( MaskStates );
	repeat
		if Matches( P, I ) then
			Exit;
	until ( not Pop( P, I ) );
	Result := False;
end;

procedure DoneMaskStates( var MaskStates: array of TKMaskState );
var
	I: Integer;
begin
	for I := Low( MaskStates ) to High( MaskStates ) do
		if MaskStates[I].State = msSet then
			Dispose( MaskStates[I].CharSet );
end;

function FileAttrToAttr( FileAttr: TKFileAttributes ): Integer;
const
	Attrs: array[TKFileAttribute] of Integer = ( SysUtils.faReadOnly, SysUtils.faHidden,
		SysUtils.faSysFile, SysUtils.faVolumeID, SysUtils.faDirectory, SysUtils.faArchive,
		SysUtils.faAnyFile );
var
	i: TKFileAttribute;
begin
	Result := 0;
	for i := Low( TKFileAttribute ) to High( TKFileAttribute ) do
		if ( i in FileAttr ) then
			Result := Result or Attrs[i];
	if ( atAnyFile in FileAttr ) then
		Result := Result or FILE_ATTRIBUTE_NORMAL;
end;

function AttrToFileAttr( Attr: Integer ): TKFileAttributes;
const
	Attrs: array[TKFileAttribute] of Integer = ( SysUtils.faReadOnly, SysUtils.faHidden,
		SysUtils.faSysFile, SysUtils.faVolumeID, SysUtils.faDirectory, SysUtils.faArchive,
		SysUtils.faAnyFile );
var
	i: TKFileAttribute;
begin
	Result := [];
	for i := Low( TKFileAttribute ) to High( TKFileAttribute ) do
		if ( ( Attr and Attrs[i] ) > 0 ) then
			Result := Result + [i];
	if ( ( Attr and FILE_ATTRIBUTE_NORMAL ) > 0 ) then
		Result := Result + [atAnyFile];
end;

{---------------------------- Public Implementation ----------------------------}

{ TKMask }

constructor TKMask.Create( const MaskValue: string );
var
	A: array[0..0] of TKMaskState;
begin
	FSize := InitMaskStates( MaskValue, A );
	FMask := AllocMem( FSize * SizeOf( TKMaskState ) );
	InitMaskStates( MaskValue, Slice( PKMaskStateArray( FMask )^, FSize ) );
end;

destructor TKMask.Destroy;
begin
  if CheckPointer( FMask ) then
  begin
		DoneMaskStates( Slice( PKMaskStateArray( FMask )^, FSize ) );
		FreeMem( FMask, FSize * SizeOf( TKMaskState ) );
  end;
end;

function TKMask.Matches( const Filename: string ): Boolean;
begin
	Result := MatchesMaskStates( Filename, Slice( PKMaskStateArray( FMask )^, FSize ) );
end;

{ TKMaskList }

constructor TKMaskList.Create;
begin
	inherited Create;
	FMasks := TStringList.Create;
	FMaskValue := '*.*';
	BuildMaskList;
end;

destructor TKMaskList.Destroy;
begin
	Clear;
	FMasks.Free;
	inherited Destroy;
end;

procedure TKMaskList.BuildMaskList;
var
	i: Integer;
	sMask: string;
begin
	Clear;
	sMask := '';
	FMasks.BeginUpdate;
	for i := 1 to Length( FMaskValue ) do
		if ( FMaskValue[i] <> CH_LIST_TOKEN ) then
			sMask := sMask + FMaskValue[i]
		else
		begin
			FMasks.AddObject( sMask, TKMask.Create( sMask ) );
			sMask := '';
		end;
	if CheckStr( sMask ) then
		FMasks.AddObject( sMask, TKMask.Create( sMask ) );
	FMasks.EndUpdate;
end;

function TKMaskList.GetCount: Integer;
begin
	Result := FMasks.Count;
end;

function TKMaskList.GetItems( iIndex: Integer ): string;
begin
	if ( iIndex < 0 ) or ( iIndex >= FMasks.Count ) then
		RaiseException( EKMaskList, sErrMLIndexOutRange );
	Result := FMasks[iIndex];
end;

procedure TKMaskList.SetMaskValue( const Value: string );
begin
	if ( not CheckStr( Value ) ) then
		FMaskValue := '*.*'
	else if ( AnsiCompareText( FMaskValue, Value ) <> 0 ) then
	begin
		FMaskValue := Value;
		BuildMaskList;
	end;
end;

procedure TKMaskList.Clear;
var
	i: Integer;
begin
	for i := 0 to Pred( FMasks.Count ) do
		TKMask( FMasks.Objects[i] ).Free;
	FMasks.Clear;
end;

function TKMaskList.Matches( iIndex: Integer; const FileName: string ): Boolean;
begin
	if ( iIndex < 0 ) or ( iIndex >= FMasks.Count ) then
		RaiseException( EKMaskList, sErrMLIndexOutRange );
	Result := TKMask( FMasks.Objects[iIndex] ).Matches( FileName );
end;

function TKMaskList.MatchesAll( const FileName: string ): Boolean;
var
	i: Integer;
begin
	Result := ( FMasks.Count > 0 );
	for i := 0 to Pred( FMasks.Count ) do
		Result := Result and TKMask( FMasks.Objects[i] ).Matches( FileName );
end;

function TKMaskList.MatchesAny( const FileName: string ): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i := 0 to Pred( FMasks.Count ) do
		Result := Result or TKMask( FMasks.Objects[i] ).Matches( FileName );
end;

{ TKFileScanner }

var
	OldUserData: Pointer = nil;
	OldCallBack: TKDialogShowProc = nil;

procedure DialogDataCallBack( Sender: TCustomForm; Data: Pointer );
begin
	if TKFileScanner( Data ).ProgressStayOnTop then
		TForm( Sender ).FormStyle := fsStayOnTop;
	DialogData.UserData := OldUserData;
	DialogData.swCallBack := OldCallBack;
end;

procedure ProgressCallBack( Data: Pointer; var Status: string; var Percent: TKPercent;
	var Cancel: Boolean; var ProgressInfoStyle: TKProgressInfoStyle );
begin
	TKFileScanner( Data ).cbProgress( Status, Percent, Cancel, ProgressInfoStyle );
end;

procedure ExecutorProc( Data: Pointer );
begin
	TKFileScanner( Data ).cbExecutor;
end;

{ TKFileScannerLink }

type

	PKFileScannerLinkData = ^TKFileScannerLinkData;
	TKFileScannerLinkData = record
		FileInfo: TKFileInfo;
		Cancel: Boolean;
	end;

const

{ LinkEvents numbers for TKFileScanner! }

	leBeforeScan = $00;
	leAfterScan = $01;

procedure TKFileScannerLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	case LinkEvent of
		leBeforeScan:
			if Assigned( FBeforeScan ) then
				with ( Owner as TKFileScanner ), PKFileScannerLinkData( Data )^ do
					FBeforeScan( ( Owner as TKFileScanner ), ExtractFilePath( CurrentScannedFile ),
					  CurLevel, FileInfo, Cancel );
		leAfterScan:
			if Assigned( FAfterScan ) then
				with ( Owner as TKFileScanner ), PKFileScannerLinkData( Data )^ do
					FAfterScan( ( Owner as TKFileScanner ), ExtractFilePath( CurrentScannedFile ),
					  CurLevel, FileInfo, Cancel );
		else
			inherited DoLinkEvent( LinkEvent, Data );
	end;
end;

{ TKFileScanner }

constructor TKFileScanner.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FRecLevel := 0;	// Sem limite de recursividade
	FScanning := False;
	FRecursive := False;
	FScanCanceled := False;
	FShowProgress := True;
	FCurrentScannedFile := '';
	FProgressStayOnTop := True;
	FProgressCaption := sFSProgressTitle;
	FProgressPrompt := sFSProgressPrompt;
	FProgressAllowCancel := True;
	FMaskList := TKMaskList.Create;
	FAttributes := [atAnyFile, atDirectory];
	FSearchAttr := SysUtils.faAnyFile or SysUtils.faDirectory or FILE_ATTRIBUTE_NORMAL;

	FCurrentPath := '';
	FCurrentResult := -1;
	FScanningStarted := false;
	FPathList := TStringList.Create;
	ZeroMemory( @FCurrentSearchRec, SizeOf( TSearchRec ) );
end;

destructor TKFileScanner.Destroy;
begin
  FPathList.Free;
	FMaskList.Free;
	inherited Destroy;
end;

function TKFileScanner.GetNeedScanning: Boolean;
begin
	Result := ( FCurrentResult = 0 ) or ( CheckStrings( FPathList ) );
end;

function TKFileScanner.GetMaskValue: string;
begin
	Result := FMaskList.MaskValue;
end;

procedure TKFileScanner.SetMaskValue( const Value: string );
begin
	if FScanning then
		Exit;
	FMaskList.MaskValue := Value;
end;

procedure TKFileScanner.SetAttributes( Value: TKFileAttributes );
begin
	if FScanning then
		Exit;
	FAttributes := Value;
	FSearchAttr := FileAttrToAttr( FAttributes );
	if FRecursive then
		FSearchAttr := FSearchAttr or SysUtils.faDirectory
end;

procedure TKFileScanner.SetRecursive( Value: Boolean );
begin
	if FScanning then
		Exit;
	FRecursive := Value;
	if FRecursive then
		FSearchAttr := FSearchAttr or SysUtils.faDirectory
	else if ( not ( atDirectory in FAttributes ) ) then
		FSearchAttr := FSearchAttr and not SysUtils.faDirectory;
end;

procedure TKFileScanner.cbExecutor;
begin
	if FScanCanceled or ( not FScanning ) then
		Exit;
	if NeedScanning then
		InternalScan
	else
		FScanning := false;
end;

procedure TKFileScanner.cbProgress( var Status: string; var Percent: TKPercent;
	var Cancel: Boolean; var ProgressInfoStyle: TKProgressInfoStyle );
begin
	if ( not ( FScanning or FScanCanceled ) ) then
		Percent := 100;
	ProgressInfoStyle := pisPath;
	Status := FCurrentScannedFile;
	if ProgressAllowCancel then
		Cancel := FScanCanceled;
end;

procedure TKFileScanner.InternalScan;
var
	bProcess: Boolean;
begin
	if ( not FScanningStarted ) then
	begin
		FCurrentPath := FPathList[0];
		FCurLevel := Integer( FPathList.Objects[0] );
		FPathList.Delete( 0 );
		FScanningStarted := true;
		FCurrentResult := FindFirst( FCurrentPath + '*.*', FSearchAttr, FCurrentSearchRec );
	end;
	if ( FCurrentResult = 0 ) then
	begin
		if ( ( FCurrentSearchRec.Attr and SysUtils.faDirectory ) > 0 ) then
			bProcess := ( atDirectory in FAttributes )
		else
			bProcess := ( FCurrentSearchRec.Attr and FSearchAttr > 0 ) and
									( FMaskList.MatchesAny( FCurrentSearchRec.Name ) );

		if bProcess then
		begin
			FCurrentScannedFile := FCurrentPath + FCurrentSearchRec.Name;
			DoScan( FCurrentPath, FCurLevel + 1, FCurrentSearchRec );
			if FScanCanceled then
			begin
				FCurrentResult := -1;
				FScanningStarted := false;
				FindClose( FCurrentSearchRec );
				Exit;
			end;
		end;
		bProcess := FRecursive and
								( ( FRecLevel = 0 ) or ( FCurLevel < FRecLevel - 1 ) ) and
								( FCurrentSearchRec.Attr and SysUtils.faDirectory > 0 ) and
								( FCurrentSearchRec.Name <> CH_DOTMARK ) and
								( FCurrentSearchRec.Name <> CH_DOTMARK + CH_DOTMARK );
		if bProcess then
			FPathList.AddObject( FCurrentPath + FCurrentSearchRec.Name + CH_BACK_SLASH, TObject( FCurLevel + 1 ) );
		FCurrentResult := FindNext( FCurrentSearchRec );
	end
	else
	begin
		FindClose( FCurrentSearchRec );
		FScanningStarted := false;
	end;
end;

procedure TKFileScanner.DoAfterStartScan;
begin
	if Assigned( FAfterStartScan ) then
		FAfterStartScan( Self );
end;

procedure TKFileScanner.DoBeforeStartScan;
begin
	if Assigned( FBeforeStartScan ) then
		FBeforeStartScan( Self );
end;

procedure TKFileScanner.StartScan( const StartPath: string );
begin
	if ( not CheckPath( StartPath ) ) then
		RaiseExceptionFmt( EKFileScanner, sErrFSInvPath, [StartPath] );
	FCurrentPath := StartPath;
	if ( CheckStr( FCurrentPath ) and ( AnsiLastChar( FCurrentPath ) <> CH_BACK_SLASH ) ) then
		FCurrentPath := FCurrentPath + CH_BACK_SLASH;
	FCurLevel := 0;
	FScanning := true;
	FScanCanceled := false;
	FScanningStarted := false;
	FCurrentScannedFile := FCurrentPath;
	DoBeforeStartScan;
	FPathList.AddObject( FCurrentPath, TObject( FCurLevel ) );
	try
		if FShowProgress then
		begin
			OldUserData := DialogData.UserData;
			OldCallBack := DialogData.swCallBack;
			try
				DialogData.UserData := Self;
				DialogData.swCallBack := DialogDataCallBack;
				FScanCanceled := ( not ProgressDialog( ProgressCaption, ProgressPrompt,
					taLeftJustify, ProgressAllowCancel, False, ProgressCallBack,
					ExecutorProc, Self ) );
			finally
				DialogData.UserData := OldUserData;
				DialogData.swCallBack := OldCallBack;
			end;
		end
		else
			while ( NeedScanning and ( not FScanCanceled ) ) do
			begin
				InternalScan;
				Application.ProcessMessages;
			end;
	finally
		FPathList.Clear;
		FScanning := False;
	end;
	DoAfterStartScan;
end;

procedure TKFileScanner.DoScan( const FilePath: string; CurLevel: Word;
	SearchRec: TSearchRec );
var
	fsld: TKFileScannerLinkData;
begin
	if ( FScanCanceled or ( not Assigned( FOnScan ) ) ) then
		Exit;
{ Translate the SearchProc information to FieldInfo }
	fsld.Cancel := False;
	with fsld.FileInfo do
	begin
		Size := SearchRec.Size;
		LongName := SearchRec.Name;
		ShortName := StrPas( SearchRec.FindData.cAlternateFileName );
		Attributes := AttrToFileAttr( SearchRec.Attr );
		Time := FileDateToDateTime( SearchRec.Time );
		FindData := SearchRec.FindData;
	end;
	fsld.Cancel := FScanCanceled;
	NotifyLinks( leBeforeScan, Integer( Pointer( @fsld ) ) );
	FScanCanceled := fsld.Cancel;
	if ( not FScanCanceled ) then
	begin
		FOnScan( Self, FilePath, CurLevel, fsld.FileInfo, FScanCanceled );
		fsld.Cancel := FScanCanceled;
		NotifyLinks( leAfterScan, Integer( Pointer( @fsld ) ) );
		FScanCanceled := fsld.Cancel;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------ Shell CommandLine -------------------------------
--------------------------------------------------------------------------------
}

function HandlerRoutine( dwCtrlType: Longint ): BOOL; stdcall; forward;
procedure HelpProc( Command: TKCommandLine; Data: Pointer; var ExitCode: Integer ); forward;

constructor TKCommandLine.Create;
begin
	inherited Create;
	FCommand := '';
	FCommandLine := '';
	FOpenQuote := false;
	FQuotedItems := true;
	FParams := TStringList.Create;
end;

destructor TKCommandLine.Destroy;
begin
	FParams.Free;
	inherited Destroy;
end;

function TKCommandLine.GetParamCount: Cardinal;
begin
	Result := FParams.Count;
end;

function TKCommandLine.GetParams( Index: Cardinal ): string;
begin
{ Here we must TypeCast Index to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, FParams.Count
	also returns Integer ranged values... }
	if ( Integer( Index ) > FParams.Count - 1 ) then
		TList.Error( SListIndexError, Index );
	Result := FParams[Index];
end;

procedure TKCommandLine.SetQuotedItems( Value: Boolean );
begin
	if ( FQuotedItems <> Value ) then
	begin
		FQuotedItems := Value;
		FOpenQuote := false;
	end;
end;

procedure TKCommandLine.SetCommandLine( const Value: string );
begin
	if ( CompareText( FCommandLine, Value ) <> 0 ) then
	begin
		FCommandLine := Value;
		ParseCommandLine;
	end;
end;

procedure TKCommandLine.ParseCommandLine;
var
	iPos: Integer;
	sLine,
	sItem: string;
	bIsCommand: Boolean;
begin
	iPos := 1;
	FParams.Clear;
	bIsCommand := true;
	sLine := FCommandLine;
	RemoveContinuers( sLine );
	while ( not CheckStr( sLine ) ) do
	begin
{ eliminate separators }
		while ( iPos <= Length( sLine ) ) and IsSeparator( sLine[iPos] ) do
			Inc( iPos );
		Delete( sLine, 1, iPos - 1 );
		iPos := 1;
{ pick the next item }
		sItem := '';
		while ( iPos <= Length( sLine ) ) and
					( not IsSeparator( sLine[iPos] ) ) and
					( ( IsValidChar( sLine[iPos] ) or
						( FQuotedItems and IsQuote( sLine[iPos] ) ) or
						( FQuotedItems and FOpenQuote and IsInQuoteChar( sLine[iPos] ) ) ) ) do
		begin
			if ( FQuotedItems and IsQuote( sLine[iPos] ) ) then
				FOpenQuote := ( not FOpenQuote );
			Inc( iPos );
		end;
		sItem := Copy( sLine, 1, iPos - 1 );
		Delete( sLine, 1, iPos - 1 );
		if FOpenQuote then
		begin
			FOpenQuote := false;
			RaiseExceptionFmt( EKShell, sErrCmdLnUnCloseQuote, [sItem] );
		end;
		iPos := 1;
		if bIsCommand then
		begin
			FCommand := sItem;
			bIsCommand := false;
		end
		else
			FParams.Add( StripQuotes( sItem ) );
	end;
end;

procedure TKCommandLine.RemoveContinuers( var Value: string );
var
	i: Integer;
	sValue: string;
begin
	sValue := '';
	for i := 1 to Length( Value ) do
		if ( not IsContinuer( Value[i] ) ) then
			sValue := sValue + Value[i];
end;

function TKCommandLine.StripQuotes( const S: string ): string;
var
	bLastQuote: Boolean;
	bFirstQuote: Boolean;
begin
	Result := S;
	if CheckStr( S ) then
	begin
		bFirstQuote := IsQuote( S[1] );
		bLastQuote := ( CheckStr( S ) and IsQuote( AnsiLastChar( S )^ ) );
		Result := Copy( S, 1 + Ord( bFirstQuote ), Length( S ) - Ord( bFirstQuote ) -
			Ord( bLastQuote ) );
	end;
end;

function TKCommandLine.IsQuote( ch: Char ): Boolean;
begin
	Result := ( ch = CH_DBLQUOTE );
end;

function TKCommandLine.IsSeparator( ch: Char ): Boolean;
begin
	Result := ( ch in [CH_TAB, CH_COMMA] ) or ( ( ch = CH_SPACE ) and ( not FOpenQuote ) );
end;

function TKCommandLine.IsValidChar( ch: Char ): Boolean;
begin
	Result := ( ( ch > CH_SPACE ) and ( ch < #128 ) ) or
		( ( ch = CH_SPACE ) and FQuotedItems and FOpenQuote );
end;

function TKCommandLine.IsContinuer( ch: Char ): Boolean;
begin
	Result := ( ch in [CH_LF, CH_CR] );
end;

function TKCommandLine.IsInQuoteChar( ch: Char ): Boolean;
begin
	Result := ( ch = CH_SPACE );
end;

{ Modified Glenn Why's Shell }

type

	PKShellCommandRecord = ^TKShellCommandRecord;
	TKShellCommandRecord = record
		Usage: string;
		Data: Pointer;
		Proc: TKShellCommandProc;
	end;

var
	_CommandList: TStringList = nil;

function CommandList: TStringList;
begin
	if ( not CheckObject( _CommandList ) ) then
	begin
		_CommandList := TStringList.Create;
		_CommandList.Duplicates := dupError;
	end;
	Result := _CommandList;
end;

function MakeCommandRecord( AObject: TObject ): TKShellCommandRecord;
begin
	Result.Data := PKShellCommandRecord( AObject )^.Data;
	Result.Usage := PKShellCommandRecord( AObject )^.Usage;
	Result.Proc := PKShellCommandRecord( AObject )^.Proc;
end;

function MakeCommandObject( const Usage: string; Proc: TKShellCommandProc;
	Data: Pointer ): TKShellCommandRecord;
begin
	Result.Data := Data;
	Result.Proc := Proc;
	Result.Usage := Usage;
end;

function GetUsage( const ACommand: string ): string;
begin
	Result := '';
	if CheckObject( _CommandList ) and
		 ( _CommandList.IndexOf( ACommand ) <> -1 ) then
		Result := MakeCommandRecord( _CommandList.Objects[_CommandList.IndexOf( ACommand )] ).Usage;
end;

procedure RegisterShellCommand( const ACommand, AUsage: string;
	AProc: TKShellCommandProc; Data: Pointer );
var
	pco: PKShellCommandRecord;
begin
	ForceReference( AProc );
	with CommandList do
	begin
		if ( IndexOf( ACommand ) <> -1 ) then
			Delete( IndexOf( ACommand ) );
		pco := New( PKShellCommandRecord );
		try
			pco^ := MakeCommandObject( AUsage, AProc, Data );
			AddObject( ACommand, TObject( pco ) );
		except
			Dispose( pco );
		end;
	end;
end;

{ TKShell }

constructor TKShell.Create;
begin
	if ( not IsConsole ) then
		RaiseException( EKShell, sErrShellNotConsole );
	if CheckObject( CommandShell ) then
		RaiseException( EKShell, sErrShellConsoleExists );
	inherited Create;
	FPrompt := '';
	FHelpEnabled := true;
end;

function TKShell.GetDefaultPrompt: string;
begin
	Result := GetCurrentDir + CH_GREATERTHAN;
end;

function TKShell.GetPrompt: string;
begin
	if Assigned( FOnGetPrompt ) then
		FOnGetPrompt( Self, FPrompt );
	if ( not CheckStr( FPrompt ) ) then
		FPrompt := GetDefaultPrompt;
	Result := FPrompt;
end;

function TKShell.GetCaption: string;
const
	MAX_CONSOLE_TITLE = 255;
begin
	SetLength( Result, MAX_CONSOLE_TITLE );
	SetLength( Result, GetConsoleTitle( PChar( Result ), MAX_CONSOLE_TITLE ) );
end;

procedure TKShell.SetCaption( const Value: string );
begin
 SetConsoleTitle( PChar( Value ) );
end;

function TKShell.IsHelpCommand( const Command: string ): Boolean;
begin
	Result := CheckStrEqual( Command, '/help' ) or CheckStrEqual( Command, '/?' ) or
						CheckStrEqual( Command, '-h' ) or CheckStrEqual( Command, '/h' ) or
						CheckStrEqual( Command, '-?' );
end;

function TKShell.GetHelp: string;
begin
	Result := sShellHelp;
end;

procedure TKShell.ShowHelp( Command: TKCommandLine );
begin
	Command.CommandLine := Format( '%s %s', [GetHelp, Command.Command] );
	HelpProc( Command );
end;

procedure TKShell.HelpProc( Command: TKCommandLine );
begin
	if ( Command.ParamCount = 1 ) then
	begin
		if ( CommandList.IndexOf( Command.Params[0] ) <> -1 ) then
			Write( Format( sShellHelpCmd, [Command.Params[0], GetFirstString(
				[GetUsage( Command.Params[0] ), sShellNoHelpCmd] )] ) )
		else
			Write( Format( sShellUnknownCmd, [Command.Params[0]] ) );
	end
	else
		Write( Format( sShellCmdSyntaxError, [GetHelp, GetUsage( GetHelp )] ) );
end;

procedure TKShell.Run;
var
	i: Integer;
	sCommand: string;
	Command: TKCommandLine;
	cr: TKShellCommandRecord;
begin
	FExitCode := SHELL_EXITCODE;
	Command := TKCommandLine.Create;
	try
		repeat
			Write( Prompt );
			Readln( sCommand );
			if ( not CheckTrimStr( sCommand ) ) then
				Continue;
			Command.CommandLine := sCommand;
			i := CommandList.IndexOf( Command.Command );
			if ( i < 0 ) then
				WriteLn( Format( sShellUnknownCmd, [Command.Command] ) )
			else
				try
					cr := MakeCommandRecord( CommandList.Objects[i] );
					if ( ( Command.ParamCount = 1 ) and IsHelpCommand( Command.Params[0] ) ) then
					begin
						ShowHelp( Command );
						Continue;
					end;
					cr.Proc( Command, cr.Data, FExitCode );
				except
					on E: Exception do
						WriteLn( Format( sErrShellException, [E.ClassName, Command.Command, E.Message] ) );
				end;
		until ( ExitCode <> SHELL_EXITCODE );
	finally
		Command.Free;
	end;
end;

procedure TKShell.CtrlC;
begin
	if Assigned( FOnCtrlC ) then
		FOnCtrlC( Self );
end;

procedure TKShell.CtrlBreak;
begin
	if Assigned( FOnCtrlBreak ) then
		FOnCtrlBreak( Self );
end;

function HandlerRoutine( dwCtrlType: Longint ): BOOL; stdcall;
begin
	Result := false;
	case dwCtrlType of
		CTRL_C_EVENT: CommandShell.CtrlC;
		CTRL_BREAK_EVENT: CommandShell.CtrlBreak;
	else
		Exit;
	end;
	Result := true;
end;

procedure HelpProc( Command: TKCommandLine; Data: Pointer; var ExitCode: Integer );
begin
	TKShell( Data ).HelpProc( Command );
end;

procedure InitShell;
begin
	CommandShell := TKShell.Create;
	try
		WriteLn( sKnowhowShell );
		WriteLn( sKnowhowSep );
		WriteLn;
		RegisterShellCommand( CommandShell.GetHelp, Format( sShellHelpCmdUsage, [CommandShell.GetHelp] ),
			HelpProc, Pointer( CommandShell ) );
		Win32Check( SetConsoleCtrlHandler( @HandlerRoutine, true ) );
	except
		FreeClean( CommandShell );
		raise;
	end;
end;

procedure DoneShell;
begin
	while CheckStrings( _CommandList ) do
		with _CommandList do
		begin
			Dispose( PKShellCommandRecord( Objects[Count - 1] ) );
			Delete( Count - 1 );
		end;
	_CommandList.Free;
	FreeClean( CommandShell );
	Win32Check( SetConsoleCtrlHandler( @HandlerRoutine, false ) );
end;

{
--------------------------------------------------------------------------------
--------------------------------- TKWordParser ---------------------------------
--------------------------------------------------------------------------------
}

const
	INTERNAL_SEPARATOR: array[TKWordSeparator] of Char =
		( CH_SPACE, CH_LIST_TOKEN, CH_DASH, CH_COMMA, CH_SLASH, CH_BACK_SLASH, CH_DOTMARK,
		  CH_UNDERSCORE, CH_NULL );
		
{ TKWordParser }

constructor TKWordParser.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FText := '';
	FCount := 0;
	FInputChars := '';
	FOutputChar := CH_SPACE;
	FResultCase := wcIgnore;
	FInputSeparator := wsSpace;
	FOutputSeparator := wsSpace;
	FWords := TStringList.Create;
end;

destructor TKWordParser.Destroy;
begin
	FWords.Clear;
	FWords.Destroy;
	inherited Destroy;
end;

function TKWordParser.GetText: string;
var
	c: Char;
	i: Integer;
begin
	Result := '';
	if ( FOutputSeparator = wsCustom ) then
		c := FOutputChar
	else
		c := INTERNAL_SEPARATOR[FOutputSeparator];
	for i := 0 to Pred( FWords.Count ) do
		Result := Result + Words[i] + c;
	Result := Copy( Result, 1, Pred( Length( Result ) ) );
end;

function TKWordParser.GetCount: Integer;
begin
	Result := FWords.Count;
end;

function TKWordParser.GetSorted: Boolean;
begin
	Result := FWords.Sorted;
end;

function TKWordParser.GetDuplicates: TDuplicates;
begin
	Result := FWords.Duplicates;
end;

function TKWordParser.GetWords( Index: Integer ): string;
begin
	if ( ( Index > Pred( FWords.Count ) ) or ( Index < 0 ) ) then
		RaiseException( EKWordParser, sErrMLIndexOutRange );
	case FResultCase of
		wcUpCase:
			Result := AnsiUpperCase( FWords[Index] );
		wcLowCase:
			Result := AnsiLowerCase( FWords[Index] );
	else
		Result := FWords[Index];
	end;
end;

procedure TKWordParser.SetSorted( Value: Boolean );
begin
	FWords.Sorted := Value;
end;

procedure	TKWordParser.SetOutputChar( Value: Char );
begin
	FOutputChar := Value;
	FOutputSeparator := wsCustom;
end;

procedure	TKWordParser.SetInputChars( const S: string );
begin
	if ( not CheckStr( S ) ) then
	begin
		FInputSeparator := wsSpace;
		Exit;
	end;
	FInputChars := S;
	FInputSeparator := wsCustom;
end;

procedure TKWordParser.SetResultCase( Value: TKWordCase );
begin
	FResultCase := Value;
end;

procedure TKWordParser.SetDuplicates( Value: TDuplicates );
begin
	FWords.Duplicates := Value;
end;

procedure TKWordParser.SetInputSeparator( Value: TKWordSeparator );
begin
	FInputSeparator := Value;
	if ( FInputSeparator <> wsCustom ) then
		FInputChars := CH_SPACE;
end;

procedure TKWordParser.SetOutputSeparator( Value: TKWordSeparator );
begin
	FOutputSeparator := Value;
	if ( FOutputSeparator <> wsCustom ) then
		FOutputChar := INTERNAL_SEPARATOR[FOutputSeparator];
end;

function TKWordParser.IsSeparator( Value: Char ): Boolean;
var
	l: Integer;
begin
	if ( FInputSeparator <> wsCustom ) then
		Result := ( Value = INTERNAL_SEPARATOR[FInputSeparator] )
	else
	begin
		Result := false;
		for l := 1 to Length( FInputChars ) do
			Result := Result or ( Value = FInputChars[l] );
	end;
end;

function TKWordParser.ExtractWord( var S: string ): string;
var
	lSize,
	lPointer: Integer;
begin
	lPointer := 1;
	lSize := Length( S );
	while ( lSize > lPointer ) and ( not IsSeparator( S[lPointer] ) ) do
		inc( lPointer );
	if ( lPointer = lSize ) then
		Result := S
	else
		Result := Copy( S, 1, Pred( lPointer ) );
	S := Copy( S, lPointer, Length( S ) - Pred( lPointer ) );
end;

function TKWordParser.RemoveSeparators( var S: string ): Boolean;
var
	lSize,
	lPointer: Integer;
begin
	Trim( S );
	lPointer := 1;
	lSize := Length( S );
	while ( ( lSize > lPointer ) and IsSeparator( S[lPointer] ) ) do
		inc( lPointer );
	if ( lPointer = lSize ) then
		S := ''
	else
		S := Copy( S, lPointer, Length( S ) - Pred( lPointer ) );
	Result := CheckStr( S );
end;

procedure TKWordParser.WordParse( const S: string );
begin
	if ( not CheckStr( S ) ) then
		Exit;
	FText := '';
	FWords.Clear;
	ParseAppend( S );
end;

procedure TKWordParser.ParseAppend( const S: string );
var
	sScratch: string;
	bNeedSeparator: Boolean;
begin
	if ( not CheckStr( S ) ) then
		Exit;
	bNeedSeparator := CheckStr( FText );
	if bNeedSeparator then
		if ( FInputSeparator <> wsCustom ) then
			FText := FText + INTERNAL_SEPARATOR[FInputSeparator]
		else
			FText := FText + FInputChars[1];
	sScratch := S;
	repeat
		if RemoveSeparators( sScratch ) then
			FWords.Add( ExtractWord( sScratch ) );
	until ( not CheckStr( sScratch ) );
	FText := FText + S;
end;

{
--------------------------------------------------------------------------------
------------------------------ TKStringsComponent ------------------------------
--------------------------------------------------------------------------------
}

constructor TKStringsComponent.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FStrings := TKStrings.Create;
end;

destructor TKStringsComponent.Destroy;
begin
	FStrings.Free;
	inherited Destroy;
end;

procedure TKStringsComponent.SetStrings( Value: TKStrings );
begin
	FStrings.Assign( Value );
end;

{
--------------------------------------------------------------------------------
---------------------------- TKStringsArrayComponent ---------------------------
--------------------------------------------------------------------------------
}

constructor TKStringsArrayComponent.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FStringsArray := TKStringsArray.Create( CheckObject( AOwner ) );
end;

destructor TKStringsArrayComponent.Destroy;
begin
	FStringsArray.Free;
	inherited Destroy;
end;

procedure TKStringsArrayComponent.SetStringsArray( Value: TKStringsArray );
{
	Toda classe que for usar um TKMultiStirngList como derivada DEVE
	usar como nome da property StringsArray, pois todo o gerenciamento
	do Property/Component Editor toma cuidados padronizados para property
	com esse nome! ( Ex: DefaultEditor Enabled para StringsArray ( DblClick ) )
}
begin
	FStringsArray.Assign( Value );
end;

{
--------------------------------------------------------------------------------
--------------------------------- TKExpiration ---------------------------------
--------------------------------------------------------------------------------
}

constructor TKExpiration.Create( AOwner: TComponent );
begin
	ForceSingleton( AOwner, TKExpiration );
	inherited Create( AOwner );
	if ( not Designing( Self ) ) then
	begin
		FTimer := TTimer.Create( nil );
		FTimer.OnTimer := TimerEvent;
		CheckInterval := 60;
		FTimer.Enabled := True;
	end
	else
		FCheckInterval := 60;
	FValidDateTime := IncMonth( Now, 1 );
	FExpirationType := etTrial;
end;

destructor TKExpiration.Destroy;
begin
	if ( CheckObject( FTimer ) and ( not Designing( Self ) ) ) then
	begin
		FTimer.Enabled := False;
		FTimer.Free;
	end;
	inherited Destroy;
end;

function TKExpiration.GetCheckInterval: Cardinal;
begin
	Result := FCheckInterval;
end;

procedure TKExpiration.SetExpirationType( Value: TKExpirationType );
begin
	if ( Value <> FExpirationType ) then
	begin
		if ( Designing( Self ) or Loading( Self ) ) then
			FExpirationType := Value;
		if ( not Designing( Self ) ) then
			FTimer.Enabled := ( FExpirationType <> etFinal );
	end;
end;

procedure TKExpiration.SetCheckInterval( Value: Cardinal );
begin
	if ( Value <> FCheckInterval ) then
	begin
		FCheckInterval := Value;
		if ( not Designing( Self ) ) then
			FTimer.Interval := ( FCheckInterval * SECOND_TO_MSECOND );
	end;
end;

procedure TKExpiration.TimerEvent( Sender: TObject );
begin
  DoTimer;
end;

procedure TKExpiration.DoTimer;
const
	BITMAP_OPTION: array[Boolean] of TKBitMapOption = ( boHardBug, boSoftBug );
var
	sMessage1,
	sMessage2: string;
begin
	if ( uksyUtils.Destroying( Self ) ) then
		Exit;
	FTimer.Enabled := False;
	try
		if ( ( ExpirationType <> etFinal ) and ( Now >= FValidDateTime ) ) then
		begin
			sMessage1 := Format( sErrExprTimeExpr, [Copy( EnumName( Cardinal( ExpirationType ),
				TypeInfo( TKExpirationType ) ), 3, MaxInt ) ] );
			sMessage2 := sMessage1;
			DoExpire( sMessage2 );
			if ( not CheckTrimStr( sMessage2 ) ) then
			  sMessage2 := sMessage1;
			case ExpirationType of
				etTrial: FatalError( sMessage2 );
				etAlpha,
				etBeta :
				begin
					MessageBeep( 0 );
					ShowDialog( sWarning, sMessage2, nil, dsOk, BITMAP_OPTION[
					  ( ExpirationType = etBeta )] );
				end;
			end;
		end;	
	finally
		FTimer.Enabled := ( ExpirationType <> etFinal );
	end;
end;

procedure TKExpiration.DoExpire( var Message: string );
begin
	if Assigned( FOnExpire ) then
	  FOnExpire( Self, Message );
end;

{
--------------------------------------------------------------------------------
------------------------------- TKVersionControl -------------------------------
--------------------------------------------------------------------------------
}

constructor TKVersionControl.Create( AOwner: TComponent );
begin
	ForceSingleton( AOwner, TKVersionControl );
	FCompileTime := False;
	inherited Create( AOwner );
	FActive := True;
	FVersionType := vtBeta;
	FCredits := TStringList.Create;
end;

destructor TKVersionControl.Destroy;
begin
	FCredits.Free;
	FreeClean( FAuxObject );
	inherited Destroy;
end;

procedure TKVersionControl.Loaded;
begin
	inherited Loaded;
	Active := ( FActive and Designing( Self ) );
end;

procedure TKVersionControl.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opInsert ) and
		 CheckObjectClass( AComponent, TKVersionControl ) and
		 ( TKVersionControl( AComponent ) <> Self ) then
		RaiseException( EKVersionControl, sErrVCInvInstance );
end;

procedure TKVersionControl.SetActive( Value: Boolean );
begin
	if ( Designing( Self ) and ( FActive <> Value ) ) then
		FActive := Value;
end;

function TKVersionControl.GetVersion: string;
begin
	Result := Format( sVCVersionPattern, [MajorVersion, MinorVersion, FCompileNumber,
		Copy( EnumName( Integer( VersionType ), TypeInfo( TKVersionType ) ), 3, MaxInt )] );
end;

procedure TKVersionControl.SetCredits( Value: TStrings );
begin
	if ( Designing( Self ) or Loading( Self ) ) then
		FCredits.Assign( Value );
end;

procedure TKVersionControl.SetCompany( const Value: string );
begin
	if ( Designing( Self ) or Loading( Self ) ) then
	  FCompany := Value;
end;

procedure TKVersionControl.SetVersion( const Value: string );
begin
	{ Do Nothing - ReadOnly property - just for design-time viewing }
end;

procedure TKVersionControl.SetCompileNumber( Value: TKCompileNumber );
begin
	if ( Designing( Self ) and CompileTime ) or ( Loading( Self ) ) or
		( csReading in ComponentState ) then
		FCompileNumber := Value;
end;

procedure TKVersionControl.SetMajorVersion( Value: TKMinMaxVersionNumber );
begin
	if ( Designing( Self ) and ( FMajorVersion <> Value ) ) then
	begin
		FMajorVersion := Value;
		MinorVersion := 0;
		CompileNumber := 0;
	end;
end;

procedure TKVersionControl.SetMinorVersion( Value: TKMinMaxVersionNumber );
begin
	if ( Designing( Self ) and ( FMinorVersion <> Value ) ) then
	begin
		FMinorVersion := Value;
		CompileNumber := 0;
	end;
end;

procedure TKVersionControl.SetVersionType( Value: TKVersionType );
begin
	if ( Designing( Self ) and ( FVersionType <> Value ) ) then
	begin
		FVersionType := Value;
		MajorVersion := 0;
		MinorVersion := 0;
		CompileNumber := 0;
	end;
end;

procedure TKVersionControl.DoCompileNumberChange;
{ This method was called at Design Time only by the AddInNotifier... }
begin
	if Designing( Self ) then
	begin
		if ( ( CompileNumber + 1 ) <= High( TKCompileNumber ) ) then
			CompileNumber := CompileNumber + 1
		else if ( ( MinorVersion + 1 ) <= High( TKMinMaxVersionNumber ) ) then
			MinorVersion := MinorVersion + 1
		else if ( ( MajorVersion + 1 ) <= High( TKMinMaxVersionNumber ) ) then
			MajorVersion := MajorVersion + 1
		else
			RaiseException( EKVersionControl, sErrTooMuchCompilation );
		MarkDesigner( Self );
	end;
end;

{---------------------------- TKCustomCreateProcess ----------------------------}

const
	COMMAND_STYLE: array[Boolean, TKCommandStyle] of string =
		(
			( '', 'command.com /k ', 'command.com /c ' ),
			( { 'cmd.exe /c ' }'', 'cmd.exe /k ', 'cmd.exe /c ' )
		);

constructor TKCustomCreateProcess.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FCommandLine := '';
	FCommandPath := '';
	FTimeOut := Cardinal( INFINITE );
	FCaptureOutput := true;
	FCommandStyle := csReturn;
	FShowWindowStyle := ssNormal;
	FProcessPriority := ppNormal;
	FTempDirectory := GetTempPath;
	FInitialDirectory := CurrentProjectPath;
	FCaptureOutputResult := TStringList.Create;
	FEnvironment := TStringList.Create;
	ZeroMemory( @FProcessInformation, SizeOf( TProcessInformation ) );
end;

destructor TKCustomCreateProcess.Destroy;
begin
	FreeClean( FCaptureOutputResult );
	FreeClean( FEnvironment );
	inherited Destroy;
end;

procedure TKCustomCreateProcess.DoExecute;
begin
	if Assigned( FOnExecute ) then
	  FOnExecute( Self, FProcessInformation );
end;

procedure TKCustomCreateProcess.SetCommandStyle( Value: TKCommandStyle );
begin
	if ( Value <> FCommandStyle ) then
	begin
		FCommandStyle := Value;
		{
		if ( FCommandStyle = csNone ) then
			FCommandPath := '';
		}	
	end;
end;

function TKCustomCreateProcess.GetProcessID: Integer;
begin
  Result := FProcessInformation.dwProcessId;
end;

function TKCustomCreateProcess.GetProcessHandle: THandle;
begin
	Result := FProcessInformation.hProcess;
end;

procedure TKCustomCreateProcess.SetCommandPath( const Value: string );
begin
	if ( not CheckStrEqual( FCommandPath, Value ) ) then
	begin
		FCommandPath := Trim( Value );
		if ( CheckStr( FCommandPath ) and ( AnsiLastChar( FCommandPath ) <> CH_BACK_SLASH ) ) then
			FCommandPath := FCommandPath + CH_BACK_SLASH;
	end;
end;

procedure TKCustomCreateProcess.SetInitialDirectory( const Value: string );
begin
	if ( not CheckStrEqual( FInitialDirectory, Value ) ) then
		FInitialDirectory := Trim( Value );
end;

{$IFDEF USE_PIPES}
function TKCustomCreateProcess.NTExecute: Boolean;
var
	hStdOut: THandle;
	Pipe: TKSimplePipe;
	ms: TMemoryStream;
	si: TStartUpInfo;
	pc: PChar;
	saAttr: TSecurityAttributes;
begin
	if FCaptureOutPut then
		hStdOut := GetStdHandle( STD_OUTPUT_HANDLE )
	else
		hStdOut := INVALID_HANDLE_VALUE;
	try
		if FCaptureOutPut then
			Pipe := TKSimplePipe.CreateSimple
		else
		  Pipe := nil;	
		try
			if FCaptureOutPut then
				Result := SetStdHandle( STD_OUTPUT_HANDLE, Pipe.hWrite )
			else
			  Result := True;
			if ( not Result ) then
				Exit;
			try
				if CheckStrings( FEnvironment ) then
				begin
					pc := StrAlloc( StringsToPCharList( FEnvironment, nil ) );
					StringsToPCharList( FEnvironment, pc );
				end
				else
				  pc := nil;
				ZeroMemory( @si, SizeOf( TStartUpInfo ) );
				ZeroMemory( @FProcessInformation, SizeOf( TProcessInformation ) );
				with si do
				begin
					cb := SizeOf( TStartupInfo );
					dwFlags := ( STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK );
					wShowWindow := SHOW_WINDOW_STYLE[FShowWindowStyle];
				end;
				Result := CreateProcess( nil, PChar( FCommandPath +
					COMMAND_STYLE[CheckWinNT, FCommandStyle] + FCommandLine ), nil, nil,
					true, PROCESS_PRIORITY[FProcessPriority], pc, PChar( FInitialDirectory ),
					si, FProcessInformation );
			finally
				if ( FCaptureOutPut and ( not SetStdHandle( STD_OUTPUT_HANDLE, hStdOut ) ) ) then
					RaiseLastWin32Error;
			end;
			if ( Result and FCaptureOutPut ) then
			begin
				saAttr.nLength := SizeOf( TSecurityAttributes );
				saAttr.bInheritHandle := True;
				saAttr.lpSecurityDescriptor := nil;
				ms := TMemoryStream.Create;
				try
					Pipe.ReadToStream( ms, 0 );
					ms.Position := 0;
					FCaptureOutputResult.LoadFromStream( ms );
				finally
					ms.Free;
				end;
				DoExecute;
				WaitForSingleObject( FProcessInformation.hProcess, FTimeOut );
				CloseHandle( FProcessInformation.hThread );
				CloseHandle( FProcessInformation.hProcess );
			end;
		finally
			if FCaptureOutPut then
				Pipe.Free;
		end;
	finally
		if FCaptureOutPut then
			CloseHandle( hStdOut ); { beveridge page 191 sugest this, insted of win32 }
	end;
end;

{$ELSE}

function TKCustomCreateProcess.NTExecute: Boolean;
var
	pc: PChar;
	sBat,
	sFile,
	sData,
	sAppend: string;
	psi: PStartupInfo;
	fs: TFileStream;
begin
	pc := nil;
	sAppend := '';
	sFile := NewTempFileName( FTempDirectory, 'co', False );
	sBat := ChangeFileExt( GetTempPathFile( 'bt', False ), '.bat' );

	if FCaptureOutput then
	begin
		sAppend := Format( '>>%s', [sFile] );
		ForceDeleteFile( sBat );
		fs := TFileStream.Create( sBat, fmCreate or fmShareExclusive );
		try
			sData := ExtractFileDrive( FInitialDirectory ) + CH_CRLF;
			sData := sData + 'cd ' + ExtractFilePath( FInitialDirectory ) + CH_CRLF;
			sData := sData + FCommandPath + FCommandLine + sAppend;
			fs.WriteBuffer( Pointer( sData )^, Length( sData ) );
		finally
			fs.Free;
		end;
	end;

{ alloc space for environment variables }
	if CheckStrings( FEnvironment ) then
	begin
		pc := StrAlloc( StringsToPCharList( FEnvironment, nil ) );
		StringsToPCharList( FEnvironment, pc );
	end;

	try
		psi := New( PStartupInfo );
		try
			ZeroMemory( psi, SizeOf( TStartupInfo ) );
			ZeroMemory( @FProcessInformation, SizeOf( TProcessInformation ) );
			with psi^ do
			begin
				cb := SizeOf( TStartupInfo );
				dwFlags := ( STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK );
				wShowWindow := SHOW_WINDOW_STYLE[FShowWindowStyle];
			end;
{ create the child process }
			Result := CreateProcess( nil, PChar( COMMAND_STYLE[true, FCommandStyle] +
				sBat ),	nil, nil, false, PROCESS_PRIORITY[FProcessPriority], pc,
				PChar( FInitialDirectory ),	psi^, FProcessInformation );
			ForceDeleteFile( sBat );
		finally
			Dispose( psi );
		end;

{ wait for the child process to finish up }
		if Result then
		begin
			WaitForSingleObject( FProcessInformation.hProcess, FTimeOut );
			DoExecute;
			CloseHandle( FProcessInformation.hThread );
			CloseHandle( FProcessInformation.hProcess );

{ might have a delay waiting for the SO to flush the file to disk... }
			if FCaptureOutput then
			begin
			  Sleep( 1000 ); { Leave SO to flush the file }
				if WaitForFile( sFile, 0 ) then
				begin
					FCaptureOutputResult.LoadFromFile( sFile );
					ForceDeleteFile( sFile );
				end
				{$IFDEF DEBUG}
				else
					DebugLogMessage( 'not found: %s', [sFile] );
				{$ENDIF}
			end;
		end;

	finally
{ dispose environment variables }
		if CheckPointer( pc ) then
			StrDispose( pc );
	end;
end;

{$ENDIF}

function TKCustomCreateProcess.Execute: Boolean;
var
	pc: PChar;
	sFile: string;
	hFile: Integer;
	psi: PStartupInfo;
begin
	if CheckWinNT then
	begin
		Result := NTExecute;
		Exit;
	end;
	pc := nil;
	hFile := 0;
	Result := false;
	psi := New( PStartupInfo );
	try
		ZeroMemory( psi, SizeOf( TStartupInfo ) );
		ZeroMemory( @FProcessInformation, SizeOf( TProcessInformation ) );
		if FCaptureOutput then
		begin
			if ( not CheckPath( FTempDirectory ) ) then
				FTempDirectory := GetTempPath;
			sFile := NewTempFileName( FTempDirectory, 'co', False );
			hFile := FileCreate( sFile );
		end;
		if ( not FCaptureOutput ) or ( hFile > 0 ) then
		begin
			with psi^ do
			begin
				cb := SizeOf( TStartupInfo );
				dwFlags := ( STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK );
				wShowWindow := SHOW_WINDOW_STYLE[FShowWindowStyle];
				if FCaptureOutput then
				begin
					dwFlags := dwFlags or STARTF_USESTDHANDLES;
					hStdInput := INVALID_HANDLE_VALUE;
					hStdOutput := hFile;
					hStdError := INVALID_HANDLE_VALUE;
				end;
			end;
			try
				if CheckStrings( FEnvironment ) then
				begin
					pc := StrAlloc( StringsToPCharList( FEnvironment, nil ) );
					StringsToPCharList( FEnvironment, pc );
				end;
				Result := CreateProcess( nil, PChar( FCommandPath +
					COMMAND_STYLE[false, FCommandStyle] + FCommandLine ), nil, nil,
					true, PROCESS_PRIORITY[FProcessPriority], pc, PChar( FInitialDirectory ),
					psi^, FProcessInformation );
				if Result then
				begin
					WaitForSingleObject( FProcessInformation.hProcess, FTimeOut );
					DoExecute;
					if FCaptureOutput then
						FileClose( hFile );
					CloseHandle( FProcessInformation.hThread );
					CloseHandle( FProcessInformation.hProcess );
				end;
			finally
				if CheckPointer( pc ) then
					StrDispose( pc );
			end;
			if CheckFile( sFile ) then
			begin
				FCaptureOutputResult.LoadFromFile( sFile );
				ForceDeleteFile( sFile );
			end;
		end;
	finally
		Dispose( psi );
	end;
end;

{
--------------------------------------------------------------------------------
----------------------------------- TKDCC32 ------------------------------------
--------------------------------------------------------------------------------
}

{ TKCompileItem }

constructor TKCompileItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKCompileItems );

{ Before inherited to allow notification }
	FHasStatistics := True;
	FDCC32Switches := DEFAULT_DCC32SWITCHES;
	FDCC32Options := DEFAULT_DCC32OPTIONS;
	FDCC32EnumType := detZ1;
	FDCC32MapFile := dmfNone;
	FDCC32CompiledFilesType := dcfDCU;
	FImageBaseAddr := DEFAULT_IMAGEBASEADDR;
	FMinStackSize := DEFAULT_MINSTACKSIZE;
	FMaxStackSize := DEFAULT_MAXSTACKSIZE;
	FErrorAction := dceaAbortGroup;
	FPackageEnvironments := DEFAULT_PACKAGE_ENVIRONMENTS;

{ explicity call to the write method - will set up FDCC32Path properly }
	DCC32Version := GetCurrentDCC32Version;

	inherited Create( ACollection );
	ForceObject( Owner.DCC32Comp );

	FDCUOutPutDir := Owner.DCC32Comp.InitialDirectory;
	FEXEOutPutDir := Owner.DCC32Comp.InitialDirectory;
	FInitialDirectory := Owner.DCC32Comp.InitialDirectory;

	FUnitPaths := TStringList.Create;
	FResPaths := TStringList.Create;
	FIncPaths := TStringList.Create;
	FObjPaths := TStringList.Create;
	FSymbolDefines := TStringList.Create;
	FUnitAliases := TStringList.Create;
	FRunTimePackages := TStringList.Create;
	FUnitPaths.Text := StringReplace( GetDelphiLibraryPath, CH_LIST_TOKEN, CH_CRLF, krfAll );
	FUnitAliases.Text := DEFAULT_UNITALIASES_TEXT;

	TStringList( FUnitPaths ).OnChange := PathsChange;
	TStringList( FResPaths ).OnChange := PathsChange;
	TStringList( FIncPaths ).OnChange := PathsChange;
	TStringList( FObjPaths ).OnChange := PathsChange;
	TStringList( FSymbolDefines ).OnChange := SymbolsChange;
	TStringList( FUnitAliases ).OnChange := UnitAliasesChange;
	TStringList( FRunTimePackages ).OnChange := RTPackagesChange;

	FHints := TStringList.Create;
	FWarnings := TStringList.Create;
	FUnits := TStringList.Create;
	FErrors := TStringList.Create;
end;

destructor TKCompileItem.Destroy;
begin
	FreeClean( FUnitPaths );
	FreeClean( FResPaths );
	FreeClean( FIncPaths );
	FreeClean( FObjPaths );
	FreeClean( FSymbolDefines );
	FreeClean( FUnitAliases );
	FreeClean( FRunTimePackages );

	FreeClean( FHints );
	FreeClean( FWarnings );
	FreeClean( FUnits );
	FreeClean( FErrors );

	inherited Destroy;
end;

function TKCompileItem.GetNamePath: string;
const
	COMPILE_ITEMS_NAMEPATH_PATTERN = '%s.%s';
begin
	Result := Format( COMPILE_ITEMS_NAMEPATH_PATTERN, [Owner.GetNamePath, Name] );
	if ( not CheckTrimStr( Result ) ) then
		Result := inherited GetNamePath;
end;

function TKCompileItem.GetCurrentDCC32Version: TKDCC32Version;
begin
{ This function was created as a Class function with conditional compilation returning
	the value of the current compiler version ( the compiler that compile the package ).
	But this procedure give to us the Internal Error U931 :( }
	Result := {$IFDEF DELPHI4}dcc120{$ELSE}dcc100{$ENDIF};
end;

procedure TKCompileItem.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKCompileItem ) then
		with ( Source as TKCompileItem ) do
		begin
			Self.Owner.DCC32Comp.FIsLoading := True; { Hard Couple! }
			try
				Self.HasStatistics := HasStatistics;
				Self.DCC32Version := DCC32Version;
				Self.DCC32Switches := DCC32Switches;
				Self.DCC32EnumType := DCC32EnumType;
				Self.DCC32Options := DCC32Options;
				Self.DCC32MapFile := DCC32MapFile;
				Self.DCC32CompiledFilesType := DCC32CompiledFilesType;
				Self.PackageEnvironments := PackageEnvironments;
				Self.ImageBaseAddr := ImageBaseAddr;
				Self.MinStackSize := MinStackSize;
				Self.MaxStackSize := MaxStackSize;
				Self.CompileFileName := CompileFileName;
				Self.InitialDirectory := InitialDirectory;
				Self.DCUOutPutDir := DCUOutPutDir;
				Self.EXEOutPutDir := EXEOutPutDir;
				Self.ErrorAction := ErrorAction;
				Self.TargetExtension := TargetExtension;
				Self.UnitPaths := UnitPaths;
				Self.ResourcePaths := ResourcePaths;
				Self.IncludePaths := IncludePaths;
				Self.ObjectPaths := ObjectPaths;
				Self.SymbolDefines := SymbolDefines;
				Self.UnitAliases := UnitAliases;
				Self.RunTimePackages := RunTimePackages;
				Self.BeforeCompile := BeforeCompile;
				Self.AfterCompile := AfterCompile;
				FHints.Clear;
				FWarnings.Clear;
				FUnits.Clear;
				FErrors.Clear;
			finally
				Self.Owner.DCC32Comp.FIsLoading := False; { Hard Couple! }
			end;
		end;
end;

function TKCompileItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( inherited Equals( Item ) );
	if Result then
		with ( Item as TKCompileItem ) do
			Result := ( ( Self.HasStatistics = HasStatistics ) and
				( Self.DCC32Version = DCC32Version ) and
				( Self.DCC32Switches = DCC32Switches ) and
				( Self.DCC32EnumType = DCC32EnumType ) and
				( Self.DCC32Options = DCC32Options ) and
				( Self.DCC32MapFile = DCC32MapFile ) and
				( Self.ImageBaseAddr = ImageBaseAddr ) and
				( Self.DCC32CompiledFilesType = DCC32CompiledFilesType ) and
				( Self.MinStackSize = MinStackSize ) and
				( Self.MaxStackSize = MaxStackSize ) and
				( Self.ErrorAction = ErrorAction ) and
				CheckStrEqual( Self.DCUOutPutDir, DCUOutPutDir ) and
				CheckStrEqual( Self.EXEOutPutDir, EXEOutPutDir ) and
				CheckStrEqual( Self.InitialDirectory, InitialDirectory ) and
				Self.UnitPaths.Equals( UnitPaths ) and
				Self.ResourcePaths.Equals( ResourcePaths ) and
				Self.IncludePaths.Equals( IncludePaths ) and
				Self.ObjectPaths.Equals( ObjectPaths ) and
				Self.SymbolDefines.Equals( SymbolDefines ) and
				Self.UnitAliases.Equals( UnitAliases ) and
				Self.RunTimePackages.Equals( RunTimePackages ) );
end;

function TKCompileItem.GetOwnerCollection: TKCompileItems;
begin
	Result := TKCompileItems( inherited GetOwnerCollection );
end;

function TKCompileItem.BuildSymbolDefines: string;
const
	COMPILER_SYMBOL_DEFINES = '-D%s ';
begin
	Result := '';
	if CheckStrings( FSymbolDefines ) then
		Result := Result + Format( COMPILER_SYMBOL_DEFINES, [
			StringReplace( FSymbolDefines.Text, CH_CRLF, CH_LIST_TOKEN, krfAll )] );
end;

function TKCompileItem.BuildUnitAliases: string;
const
	COMPILER_UNIT_ALIASES = '-A%s ';
begin
	Result := '';
	if CheckStrings( FUnitAliases ) then
		Result := Result + Format( COMPILER_UNIT_ALIASES, [
			StringReplace( FUnitAliases.Text, CH_CRLF, CH_LIST_TOKEN, krfAll )] );
end;

function TKCompileItem.BuildUnitPaths: string;
const
	COMPILER_UNIT_ALIASES = '-U"%s" ';
var
	s: string;
begin
	Result := '';
	if CheckStrings( FUnitPaths ) then
	begin
		s := FUnitPaths.Text;
		Result := Result + Format( COMPILER_UNIT_ALIASES, [
			StringReplace( Copy( s, 1, Length( s ) - 2 ), CH_CRLF, CH_DBLQUOTE + CH_LIST_TOKEN + CH_DBLQUOTE,
			krfAll )] );
	end;
{
	This formating and transformation will convert a given string list into a valid
	path list. The example above give the a better ideia.
	Don't worry about the correctness of the paths (i.e, invalid paths, or paths with
	'"') because at property setting the NormalizePaths routine take any validation/
	reformating algorithms

	S(1)#13#10S(2)#13#10..#13#10S(n-1)#13#10S(n)#13#10
	"S(1)";"S(2)";"..";"S(n-1)";"S(n)"
}
end;

function TKCompileItem.BuildObjectPaths: string;
const
	COMPILER_OBJECT_ALIASES = '-O"%s" ';
var
	s: string;
begin
	Result := '';
	if CheckStrings( FObjPaths ) then
	begin
		s := FObjPaths.Text;
		Result := Result + Format( COMPILER_OBJECT_ALIASES, [
			StringReplace( Copy( s, 1, Length( s ) - 2 ), CH_CRLF, CH_DBLQUOTE + CH_LIST_TOKEN + CH_DBLQUOTE,
			krfAll )] );
	end;
end;

function TKCompileItem.BuildResourcePaths: string;
const
	COMPILER_RESOURCE_ALIASES = '-R"%s" ';
var
	s: string;
begin
	Result := '';
	if CheckStrings( FResPaths ) then
	begin
		s := FResPaths.Text;
		Result := Result + Format( COMPILER_RESOURCE_ALIASES, [
			StringReplace( Copy( s, 1, Length( s ) - 2 ), CH_CRLF, CH_DBLQUOTE + CH_LIST_TOKEN + CH_DBLQUOTE,
			krfAll )] );
	end;
end;

function TKCompileItem.BuildIncludePaths: string;
const
	COMPILER_INCLUDE_ALIASES = '-I"%s" ';
var
	s: string;
begin
	Result := '';
	if CheckStrings( FIncPaths ) then
	begin
		s := FIncPaths.Text;
		Result := Result + Format( COMPILER_INCLUDE_ALIASES, [
			StringReplace( Copy( s, 1, Length( s ) - 2 ), CH_CRLF, CH_DBLQUOTE + CH_LIST_TOKEN + CH_DBLQUOTE,
			krfAll )] );
	end;
end;

function TKCompileItem.BuildRunTimePackages: string;
const
	COMPILER_RUNTIME_PACKAGES = '-LU"%s" ';
var
	s: string;
begin
	Result := '';
	if CheckStrings( FRunTimePackages ) then
	begin
		s := FRunTimePackages.Text;
		Result := Result + Format( COMPILER_RUNTIME_PACKAGES, [
			StringReplace( Copy( s, 1, Length( s ) - 2 ), CH_CRLF, CH_DBLQUOTE + CH_LIST_TOKEN + CH_DBLQUOTE,
			krfAll )] );
	end;
end;

function TKCompileItem.BuildCompilerOptions: string;
const
	COMPILER_OPTION = '%s';
	COMPILER_IMAGEBASE_ADDR = '-K%d ';
	COMPILER_MINMAX_STACKSIZE = '-$M%d,%d ';
	COMPILER_COMP_OUTPUT_DIR: array[Boolean] of string[8] = ( '-N"%s" ', '-LN"%s" ' );
	COMPILER_EXEC_OUTPUT_DIR: array[Boolean] of string[8] = ( '-E"%s" ', '-LE"%s" ' );
	COMPILER_TARGET_EXTENSION: array[Boolean] of string[6] = ( '', '-TX%s ' );
	COMPILER_OPTION_MAP: array[TKDCC32Option] of string[4] =
		(
			'-CC ', { doConsoleAPP    } { if not present, -CG was assumed as default }
			'-H ' , { doHints         }
			'-W ' , { doWarnings      }
			'-B ' , { doBuildAll      } { if not present, -M was assumed as default  }
			'-V ' , { doEXEDebugInfo  }
			'-Z ' , { doImplicitBuild }
			'-Q '   { doQuiet         }
		);
	COMPILER_MAPFILE_MAP: array[TKDCC32MapFile] of string[4] =
		(
			' '   , { dmfNone     }
			'-GD ', { dmfDetailed }
			'-GP ', { dmfPublics  }
			'-GS '  { dmfSegments }
		);
	COMPILER_FILETYPE_MAP: array[TKDCC32CompiledFilesType] of string[4] =
		(
			' '   , { dcfDCU    }
			'-J ' , { dcfObj    }
			'-JP '  { dcfCPPObj }
		);
var
	i: TKDCC32Option;
begin
	Result := '';
	for i := Low( TKDCC32Option ) to High( TKDCC32Option ) do
		if ( i in DCC32Options ) then
			Result := Result + Format( COMPILER_OPTION, [COMPILER_OPTION_MAP[i]] );
	Result := Result + Format( COMPILER_OPTION, [COMPILER_MAPFILE_MAP[DCC32MapFile]] );
	Result := Result + Format( COMPILER_OPTION, [COMPILER_FILETYPE_MAP[DCC32CompiledFilesType]] );
	ForceTrimStr( Result );
	Result := Result + BuildSymbolDefines;
	Result := Result + BuildUnitAliases;
	Result := Result + BuildUnitPaths;
	Result := Result + BuildObjectPaths;
	Result := Result + BuildResourcePaths;
	Result := Result + BuildIncludePaths;
	Result := Result + BuildRunTimePackages;
	Result := Result + Format( COMPILER_COMP_OUTPUT_DIR[
		CheckStrContains( DELPHI_PACKAGESOURCE_EXT, CompileFileName )], [DCUOutPutDir] );
	Result := Result + Format( COMPILER_EXEC_OUTPUT_DIR[
		CheckStrContains( DELPHI_PACKAGESOURCE_EXT, CompileFileName )], [EXEOutPutDir] );
	Result := Result + Format( COMPILER_TARGET_EXTENSION[( CheckTrimStr( TargetExtension ) and
		CheckStrContains( CH_DOTMARK, TargetExtension ) )], [FTargetExtension] );
	Result := Result + Format( COMPILER_IMAGEBASE_ADDR, [FImageBaseAddr] );
	Result := Result + Format( COMPILER_MINMAX_STACKSIZE, [FMinStackSize, FMaxStackSize] );
end;

function TKCompileItem.BuildCompilerDirectives: string;
const
	COMPILER_DIRECTIVE = '-$%s%s ';
	COMPILER_DIRECTIVE_SWITCH_STATE: array[Boolean] of Char = ( CH_MINUS_SIGN, CH_PLUS_SIGN );
	COMPILER_DIRECTIVE_ENUMTYPE_MAP: array[TKDCC32EnumType] of string[2] =
		( 'Z1' { detZ1 }, 'Z2' { detZ2 }, 'Z4' { detZ4 } ); { explicity do not use GetEnumName }
	COMPILER_DIRECTIVE_SWITCH_MAP: array[TKDCC32Switch] of Char =
		(
			'A', { dsAlign           } 'B', { dsBoolEval        } 'C', { dsAssertions      }
			'D', { dsDCUDebuginfo    } 'G', { dsImporteddata    } 'H', { dsLongStrings 	   }
			'I', { dsIOChecks        } 'J', { dsWriteableConst  } 'L', { dsLocalSymbols    }
			'M', { dsTypeInfo        } 'O', { dsOptimization    } 'P', { dsOpenStrings     }
			'Q', { dsOverFlowChecks  } 'R', { dsRangeChecks     } 'T', { dsTypedAddress    }
			'U', { dsSafeDivide      } 'V', { dsVarStringChecks } 'W', { dsStackFrames     }
			'X', { dsExtendedSyntax  } 'Y'  { dsReferenceInfo   }
		);
var
	i: TKDCC32Switch;
begin
	Result := '';
	for i := Low( TKDCC32Switch ) to High( TKDCC32Switch ) do
		Result := Result + Format( COMPILER_DIRECTIVE, [COMPILER_DIRECTIVE_SWITCH_MAP[i],
			COMPILER_DIRECTIVE_SWITCH_STATE[( i in DCC32Switches )]] );
	Result := Result + Format( COMPILER_DIRECTIVE, [COMPILER_DIRECTIVE_ENUMTYPE_MAP[DCC32EnumType], CH_SPACE] );
	ForceTrimStr( Result );
	Delete( Result, Length( Result ), 1 ); { remove the last space... }
end;

function TKCompileItem.GetCompilerCommandLine: string;
const
	COMPILER_EXE_NAME = 'dcc32.exe';
	COMPILER_COMMANDLINE = '"%s\%s" "%s" %s %s';
begin
	CheckCompileFileName( CompileFileName );
	Result := Trim( Format( COMPILER_COMMANDLINE, [FDCC32Path, COMPILER_EXE_NAME,
		CompileFileName, BuildCompilerOptions, BuildCompilerDirectives] ) );
end;

procedure	TKCompileItem.PathsChange( Sender: TObject );
begin
	TStringList( Sender ).OnChange := nil;
	try
		try
			if CheckStrings( TStrings( Sender ) ) then
			begin
				CheckInternalStrings( TStrings( Sender ) );
				NormalizePaths( TStrings( Sender ) );
				Changed( False );
			end;
		except
			TStrings( Sender ).Clear;
			raise;
		end;
	finally
		TStringList( Sender ).OnChange := PathsChange;
	end;
end;

procedure	TKCompileItem.SymbolsChange( Sender: TObject );
begin
	TStringList( Sender ).OnChange := nil;
	try
		try
			if CheckStrings( TStrings( Sender ) ) then
			begin
				CheckInternalStrings( TStrings( Sender ) );
				NormalizeSymbolDefines( TStrings( Sender ) );
				Changed( False );
			end;
		except
			TStrings( Sender ).Clear;
			raise;
		end;
	finally
		TStringList( Sender ).OnChange := SymbolsChange;
	end;
end;

procedure	TKCompileItem.UnitAliasesChange( Sender: TObject );
begin
	TStringList( Sender ).OnChange := nil;
	try
		try
			if CheckStrings( TStrings( Sender ) ) then
			begin
				CheckUnitAliases( TStrings( Sender ) );
				Changed( False );
			end;
		except
			TStrings( Sender ).Clear;
			raise;
		end;
	finally
		TStringList( Sender ).OnChange := UnitAliasesChange;
	end;
end;

procedure	TKCompileItem.RTPackagesChange( Sender: TObject );
begin
	TStringList( Sender ).OnChange := nil;
	try
		try
			if CheckStrings( TStrings( Sender ) ) then
			begin
				CheckInternalStrings( TStrings( Sender ) );
				NormalizePaths( TStrings( Sender ) );
				Changed( False );
			end;
		except
			TStrings( Sender ).Clear;
			raise;
		end;
	finally
		TStringList( Sender ).OnChange := RTPackagesChange;
	end;
end;

procedure TKCompileItem.SetHasStatistics( Value: Boolean );
begin
	if ( Value <> FHasStatistics ) then
	begin
		FHasStatistics := Value;
		ClearStatistics;
		Changed( False );
	end;
end;

procedure TKCompileItem.ClearStatistics;
begin
  ZeroMemory( @FCompiledData, SizeOf( TKDCC32CompiledData ) );
end;

procedure TKCompileItem.ClearResults;
begin
	FUnits.Clear;
	FHints.Clear;
	FErrors.Clear;
	FWarnings.Clear;
end;

procedure TKCompileItem.SetCompileFileName( const Value: string );
var
	sValue: string;
	cValue: array[0..MAX_PATH - 1] of Char;
begin
	if ( not CheckStrEqual( FCompileFileName, Value ) ) then
	begin
		if CheckTrimStr( Value ) then
		begin
			CheckCompileFileName( Value );
			if CheckStrContains( DELPHI_PACKAGESOURCE_EXT, Value ) then
			begin
				if ( peDesignTime in PackageEnvironments ) then { for design time packages }
				begin
					if CheckTrimStr( DCUOutPutDir ) then { ? Why not "not Check..." ? }
						DCUOutPutDir := GetDelphiDCPOutPutDir;
					if CheckTrimStr( EXEOutPutDir ) then
						EXEOutPutDir := GetDelphiDPLOutPutDir;
				end
				else  { for run time packages... }
				begin
					if CheckTrimStr( DCUOutPutDir ) then { ? Why not "not Check..." ? }
						DCUOutPutDir := ExtractFilePath( Value ); { Same as InitialDirectory }
					if CheckTrimStr( EXEOutPutDir ) then
					begin
            ZeroMemory( @cValue, SizeOf( cValue ) );
						SetString( sValue, PChar( @cValue[0] ),
							GetSystemDirectory( PChar( @cValue[0] ),  SizeOf( cValue ) ) );
						EXEOutPutDir := sValue;
					end;	
				end;
			end
			else
			begin
				sValue := ExtractFilePath( Value );
				if CheckTrimStr( DCUOutPutDir ) then { ? Why not "not Check..." ? }
					DCUOutPutDir := sValue;
				if CheckTrimStr( EXEOutPutDir ) then
					EXEOutPutDir := sValue;
			end;
			InitialDirectory := ExtractFilePath( Value );
    	if ( CheckStrContains( Owner.GetDefaultItemName( Self ), Name ) and
				 ( not CheckObject( Owner.FindItem( ChangeFileExt( ExtractFileName(
					 Value ), '' ) ) ) ) ) then
				Name := ChangeFileExt( ExtractFileName( Value ), '' );
		end;
		FCompileFileName := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetTargetExtension( const Value: string );
begin
	if ( not CheckStrEqual( FTargetExtension, Value ) ) and
		 ( not ( CheckStrContains( DELPHI_UNIT_EXT, Value ) or
							 CheckStrContains( DELPHI_PROJECT_EXT, Value ) or
							 CheckStrContains( DELPHI_PACKAGESOURCE_EXT, Value ) ) ) then
{ only non default extension target values allowed }
		FTargetExtension := Value;
end;

procedure TKCompileItem.SetDCC32Options( Value: TKDCC32Options );
begin
	if ( Value <> FDCC32Options ) then
	begin
		FDCC32Options := Value;
		if ( doQuiet in Value ) then
		begin
			FHints.Clear;
			FWarnings.Clear;
			FUnits.Clear;
		end
		else
		begin
			if ( not ( doHints in Value ) ) then
				FHints.Clear;
			if ( not ( doWarnings in Value ) ) then
				FWarnings.Clear;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetDCC32Switches( Value: TKDCC32Switches );
begin
	if ( Value <> FDCC32Switches ) then
	begin
		FDCC32Switches := Value;
		if ( not ( dsDCUDebuginfo in FDCC32Switches ) ) then
		begin
			Exclude( FDCC32Switches, dsLocalSymbols );
			Exclude( FDCC32Switches, dsReferenceInfo );
		end;
		if ( not ( dsLongStrings in FDCC32Switches ) ) then
			Exclude( FDCC32Switches, dsOpenStrings )
		else
			Exclude( FDCC32Switches, dsVarStringChecks );
		Changed( False );
	end;
end;

procedure TKCompileItem.SetDCC32EnumType( Value: TKDCC32EnumType );
begin
	if ( Value <> FDCC32EnumType ) then
	begin
		FDCC32EnumType := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetDCC32Version( Value: TKDCC32Version );
begin
	if ( Value <> FDCC32Version ) then
	begin
		CheckDCC32File( Value );
		FDCC32Version := Value;
		Changed( False ); 
	end;
end;

procedure TKCompileItem.SetDCUOutPutDir( const Value: string );
begin
	if ( not CheckStrEqual( FDCUOutPutDir, Value ) ) then
	begin
		FDCUOutPutDir := Trim( Value );
{
		if CheckTrimStr( Value ) and ( Reading( Self ) or Loading( Self ) or
			Designing( Self ) ) and ( not CheckPath( Value ) ) then
			FDCUOutPutDir := '';
}
		if CheckTrimStr( FDCUOutPutDir ) and ( AnsiLastChar( FDCUOutPutDir ) = CH_BACK_SLASH ) then
			Delete( FDCUOutPutDir, Length( FDCUOutPutDir ), 1 );
		Changed( False );
	end;
end;

procedure TKCompileItem.SetInitialDirectory( const Value: string );
begin
	if ( not CheckStrEqual( FInitialDirectory, Value ) ) then
	begin
		FInitialDirectory := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetEXEOutPutDir( const Value: string );
begin
	if ( not CheckStrEqual( FEXEOutPutDir, Value ) ) then
	begin
		FEXEOutPutDir := Trim( Value );
		if CheckTrimStr( FEXEOutPutDir ) and ( AnsiLastChar( FEXEOutPutDir ) = CH_BACK_SLASH ) then
			Delete( FEXEOutPutDir, Length( FEXEOutPutDir ), 1 );
		Changed( False );
	end;
end;

procedure TKCompileItem.SetImageBaseAddr( Value: Cardinal );
begin
	if ( Value <> FImageBaseAddr ) then
	begin
		FImageBaseAddr := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetMaxStackSize( Value: Cardinal );
begin
	if ( Value <> FMaxStackSize ) then
	begin
		if ( Value <= ( FMinStackSize + DEFAULT_MINSTACKSIZE ) ) then
			Value := DEFAULT_MAXSTACKSIZE;
		FMaxStackSize := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetMinStackSize( Value: Cardinal );
begin
	if ( Value <> FMinStackSize ) then
	begin
		if ( ( Value + DEFAULT_MINSTACKSIZE ) >= FMaxStackSize ) then
			Value := DEFAULT_MINSTACKSIZE;
		FMinStackSize := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetIncPaths( Value: TStrings );
begin
	if ( not FIncPaths.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FIncPaths ).OnChange := nil;
		try
			FIncPaths.Assign( Value );
			NormalizePaths( FIncPaths );
		finally
			TStringList( FIncPaths ).OnChange := PathsChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetObjPaths( Value: TStrings );
begin
	if ( not FObjPaths.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FObjPaths ).OnChange := nil;
		try
			FObjPaths.Assign( Value );
			NormalizePaths( FObjPaths );
		finally
			TStringList( FObjPaths ).OnChange := PathsChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetResPaths( Value: TStrings );
begin
	if ( not FResPaths.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FResPaths ).OnChange := nil;
		try
			FResPaths.Assign( Value );
			NormalizePaths( FResPaths );
		finally
			TStringList( FResPaths ).OnChange := PathsChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetUnitPaths( Value: TStrings );
begin
	if ( not FUnitPaths.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FUnitPaths ).OnChange := nil;
		try
			FUnitPaths.Assign( Value );
			NormalizePaths( FUnitPaths );
		finally
			TStringList( FUnitPaths ).OnChange := PathsChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetRunTimePackages( Value: TStrings );
begin
	if ( not FRunTimePackages.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FRunTimePackages ).OnChange := nil;
		try
			FRunTimePackages.Assign( Value );
{
	Package name normalization. No obligation for existence are need until now. This is
	the dcc32 task... (can be built using the library path in the registry and the path
	environment variable for searching the requested package files)
}
			NormalizePaths( FRunTimePackages );
		finally
			TStringList( FRunTimePackages ).OnChange := RTPackagesChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetSymbolDefines( Value: TStrings );
begin
	if ( not FSymbolDefines.Equals( Value ) ) then
	begin
		CheckInternalStrings( Value );
		TStringList( FSymbolDefines ).OnChange := nil;
		try
			FSymbolDefines.Assign( Value );
			NormalizeSymbolDefines( FSymbolDefines );
		finally
			TStringList( FSymbolDefines ).OnChange := SymbolsChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetUnitAliases( Value: TStrings );
begin
	if ( not FUnitAliases.Equals( Value ) ) then
	begin
		CheckUnitAliases( Value );
		TStringList( FUnitAliases ).OnChange := nil;
		try
			FUnitAliases.Assign( Value );
		finally
			TStringList( FUnitAliases ).OnChange := UnitAliasesChange;
		end;
		Changed( False );
	end;
end;

procedure TKCompileItem.SetErrorAction( Value: TKDCC32CompileErrorAction );
begin
	if ( Value <> FErrorAction ) then
	begin
		FErrorAction := Value;
		Changed( True );
	end;
end;

procedure TKCompileItem.SetPackageEnvironments( Value: TKPackageEnvironments );
begin
	if ( Value <> FPackageEnvironments ) then
	begin
		FPackageEnvironments := Value;
		Changed( False );
	end;
end;

procedure TKCompileItem.CheckCompileFileName( const Value: string );
var
	b: Boolean;
begin
	b := ( CheckTrimStr( Value ) and
	( CheckStrContains( DELPHI_PROJECT_EXT, Value ) or
		CheckStrContains( DELPHI_PACKAGESOURCE_EXT, Value ) or
		CheckStrContains( DELPHI_UNIT_EXT, Value )
	) and CheckFile( Value ) );
	if ( not b ) then
	begin
		if Owner.DCC32Comp.IsLoading then
			Owner.DCC32Comp.DoLoadError( Self )
		else
			RaiseExceptionFmt( EKDCC32, sErrCIInvFileName, [Value] );
	end;
end;

procedure TKCompileItem.CheckDCC32File( Value: TKDCC32Version );
begin
	FDCC32Path := DelphiRootDir( TKDelphi32Version( Value ) ) + '\Bin';
	try
		ForcePath( FDCC32Path );
	except
		FDCC32Path := '';
		raise;
	end;
end;

procedure TKCompileItem.CheckUnitAliases( Value: TStrings );
var
	i: Integer;
	bOk: Boolean;
begin
	ForceObject( Value );
	if ( not CheckStrings( Value ) ) then
		Exit;
{ do not use ListSeparator or CH_LIST_TOKEN, the ';' is a compiler definition }
	bOk := ( not CheckStrContains( CH_LIST_TOKEN, Value.Text ) );
	if ( not bOk ) then
		Value.Text := StringReplace( Value.Text, CH_LIST_TOKEN, CH_CRLF, krfAll );
	bOk := ( not CheckStrContains( CH_LIST_TOKEN, Value.Text ) );
	TrimStrings( Value );
{ IsValidIdent (SysUtils.pas) already checks for a empty value... }
	for i := 0 to Value.Count - 1 do
		bOk := ( bOk and IsValidIdent( Value.Names[i] ) and IsValidIdent( Value.Values[Value.Names[i]] ) );
	if ( not bOk ) then
	begin
		if Owner.DCC32Comp.IsLoading then
			Owner.DCC32Comp.DoLoadError( Self )
		else
			RaiseException( EKDCC32, sErrCIInvUnitAliases );
	end;
{ duplicate values are ignored by the compiler (only the first values are used) }
end;

procedure TKCompileItem.CheckInternalStrings( Value: TStrings );
begin
{ do not use ListSeparator or CH_LIST_TOKEN, the ';' is a compiler definition }
	if ( CheckStrings( Value ) and CheckStrContains( CH_LIST_TOKEN, Value.Text ) ) then
		Value.Text := StringReplace( Value.Text, CH_LIST_TOKEN, CH_CRLF, krfAll );
end;

procedure TKCompileItem.NormalizePaths( Value: TStrings );
begin
{ The paths will be correctly quotted when needed... }
	TrimStrings( Value );
	if CheckStrings( Value ) then
	begin
		Value.Text := StringReplace( Value.Text, CH_DBLQUOTE, '', krfAll );
		Value.Text := StringReplace( Value.Text, CH_BACK_SLASH + CH_CRLF, CH_CRLF, krfAll );
	end;
end;

procedure TKCompileItem.NormalizeSymbolDefines( Value: TStrings );
var
	i: Integer;
begin
{ The SymbolDefines will be normalized to be at least MAX_PASCAL_ID Length and
	is a valid pascal identifier }
	TrimStrings( Value );
	if CheckStrings( Value ) then
		for i := 0 to Value.Count - 1 do
			Value.Strings[i] := Copy( NormalizeStringCharSet( Value.Strings[i],
				CHARSET_IDENTIFIER ), 1, MAXLEN_PASCALID );
end;

procedure TKCompileItem.SetUpDCC32;
begin
	FHints.Clear;
	FWarnings.Clear;
	FUnits.Clear;
	ClearGroupAborted;
	ZeroMemory( @FCompiledData, SizeOf( TKDCC32CompiledData ) );
	Owner.DCC32Comp.InitialDirectory := InitialDirectory;
	Owner.DCC32Comp.CaptureOutputResult.Clear;
	Owner.DCC32Comp.CommandLine := CompilerCommandLine; { Hard Couple! }
end;

procedure TKCompileItem.BuildStatistics( const Value: string );
var
	sValue: string;
begin
	sValue := Trim( Value );
	ZeroMemory( @FCompiledData, SizeOf( TKDCC32CompiledData ) );
	with FCompiledData do
	begin
		Lines := StrToIntDef( Trim( Copy( sValue, 1, Pos( CH_SPACE, sValue ) - 1 ) ), 0 );
		Delete( sValue, 1, Pos( CH_COMMA, sValue ) + 1 );

		sValue := StringReplace( sValue, CH_DOTMARK, '', krfAll );
		MSecs := StrToIntDef( Trim( Copy( sValue, 1, Pos( CH_SPACE, sValue ) - 1 ) ), 0 ) * 10;
		Delete( sValue, 1, Pos( CH_COMMA, sValue ) + 1 );

		CodeBytes := StrToIntDef( Trim( Copy( sValue, 1, Pos( CH_SPACE, sValue ) - 1 ) ), 0 );
		Delete( sValue, 1, Pos( CH_COMMA, sValue ) + 1 );

		DataBytes := StrToIntDef( Trim( Copy( sValue, 1, Pos( CH_SPACE, sValue ) - 1 ) ), 0 );
		Delete( sValue, 1, Pos( CH_COMMA, sValue ) + 1 );
	end;
end;

function TKCompileItem.ParseDCC32Result( ss: TStrings ): Boolean;

	function GetFileName( const s: string ): string;
	begin
		Result := Copy( s, 1, Pos( CH_PARENTHESIS_OPEN, s ) - 1 );
	end;

	function GetLineNumbers( const s: string ): string;
	begin
		Result := Copy( s, Pos( CH_PARENTHESIS_OPEN, s ) + 1, Pos( CH_PARENTHESIS_CLOSE, s ) -
		  Pos( CH_PARENTHESIS_OPEN, s ) - 1 );
	end;

const
	COMPILER_VERSION_LINE = 0;
	COMPILER_WARNING_TOKEN = 'Warning:';
	COMPILER_HINT_TOKEN = 'Hint:';
	COMPILER_ERROR_TOKEN = 'Error:';
	COMPILER_FATAL_TOKEN = 'Fatal:';
	COMPILER_STATISTICS_TOKEN = 'seconds,';
var
	i: Integer;
begin
	TrimStrings( ss );
	for i := ss.Count - 1 downto COMPILER_VERSION_LINE + 1 do
		if CheckStrContains( COMPILER_WARNING_TOKEN, ss.Strings[i] ) then
			FWarnings.AddObject( GetFileName( ss.Strings[i] ) + CH_EQUAL_TOKEN + Copy( ss.Strings[i],
				Pos( COMPILER_WARNING_TOKEN, ss.Strings[i] ) + Length( COMPILER_WARNING_TOKEN ),
				MaxInt ), TObject( StrToIntDef( GetLineNumbers( ss.Strings[i] ), -1 ) ) )
		else if CheckStrContains( COMPILER_HINT_TOKEN, ss.Strings[i] ) then
			FHints.AddObject( GetFileName( ss.Strings[i] ) + CH_EQUAL_TOKEN + Copy( ss.Strings[i],
				Pos( COMPILER_HINT_TOKEN, ss.Strings[i] ) + Length( COMPILER_HINT_TOKEN ),
				MaxInt ), TObject( StrToIntDef( GetLineNumbers( ss.Strings[i] ), -1 ) ) )
		else if CheckStrContains( COMPILER_ERROR_TOKEN, ss.Strings[i] ) then
			FErrors.AddObject( GetFileName( ss.Strings[i] ) + CH_EQUAL_TOKEN + Copy( ss.Strings[i], Pos(
				COMPILER_ERROR_TOKEN, ss.Strings[i] ) + Length( COMPILER_ERROR_TOKEN ),
				MaxInt ), TObject( StrToIntDef( GetLineNumbers( ss.Strings[i] ), -1 ) ) )
		else if CheckStrContains( COMPILER_FATAL_TOKEN, ss.Strings[i] ) then
			FErrors.AddObject( GetFileName( ss.Strings[i] ) + CH_EQUAL_TOKEN + Copy( ss.Strings[i], Pos(
				COMPILER_FATAL_TOKEN, ss.Strings[i] ) + Length( COMPILER_FATAL_TOKEN ),
				MaxInt ), TObject( StrToIntDef( GetLineNumbers( ss.Strings[i] ), -1 ) ) )
		else if HasStatistics and CheckStrContains( COMPILER_STATISTICS_TOKEN, ss.Strings[i] ) and
					 ( not CheckFile( ss.Strings[i] ) ) then
			BuildStatistics( ss.Strings[i] )
		else if ( CheckStrContains( DELPHI_PROJECT_EXT, ExtractFileExt( ss.Strings[i] ) ) or
							CheckStrContains( DELPHI_PACKAGESOURCE_EXT, ExtractFileExt( ss.Strings[i] ) ) or
							CheckStrContains( DELPHI_UNIT_EXT, ExtractFileExt( ss.Strings[i] ) ) or
							CheckStrContains( DELPHI_INCLUDE_EXT, ExtractFileExt( ss.Strings[i] ) ) ) and
							( FUnits.IndexOfName( GetFileName( ss.Strings[i] ) ) = -1 ) then
			FUnits.Add( GetFileName( ss.Strings[i] ) + CH_EQUAL_TOKEN + GetLineNumbers( ss.Strings[i] ) );
	Result := ( not CheckStrings( FErrors ) );
end;

function TKCompileItem.PerformErrorAction: Boolean;
begin
	ClearStatistics;
	Result := False;
	case ErrorAction of
		dceaIgnore    : Result := True;
		dceaAbortAll  : RaiseExceptionFmt( EKDCC32, sErrCIAbortCompile, [Name] );
		dceaAbortGroup:
		begin
			Result := ( GroupIndex = COMPILER_GROUP_NULL );
			if ( not Result ) then
			begin
				Result := ( not DoGroupAbort );
				if ( not Result ) then
					MarkGroupAborted;
			end;		
		end;
	end;
end;

procedure TKCompileItem.DoAfterCompile;
begin
	if Assigned( FAfterCompile ) then
		FAfterCompile( Self );
end;

procedure TKCompileItem.DoBeforeCompile;
begin
	if Assigned( FBeforeCompile ) then
		FBeforeCompile( Self );
end;

function TKCompileItem.DoGroupAbort: Boolean;
begin
  Result := True;
	if Assigned( Owner.DCC32Comp.OnGroupAbort ) then
		Owner.DCC32Comp.OnGroupAbort( Owner.DCC32Comp, Self, GroupIndex, Result );
end;

procedure TKCompileItem.LoadFromStream( Stream: TStream );
var
	Reader: TReader;
	ColItems: TKCompileItems;
begin
	Reader := TReader.Create( Stream, ( 4 * KB ) );
	try
    Reader.Root := ( GetFirstObject( [Owner.DCC32Comp.Owner, Owner.DCC32Comp] ) as TComponent );
		ColItems := TKCompileItems.Create( Owner.DCC32Comp );
		try
			Reader.ReadValue;
			Reader.ReadCollection( ColItems );
			if ( not CheckCollection( ColItems ) ) or ( ColItems.Count > 1 ) then
				RaiseException( EKDCC32, sErrCICannotLoadFromStream );
			Assign( ColItems.Items[0] );
			if ( ( not Owner.AllowDuplicateNames ) and CheckTrimStr( Name ) ) then
				Owner.SetItemName( Self );
		finally
			ColItems.Free;
		end;
	finally
		Reader.Free;
	end;
end;

procedure TKCompileItem.SaveToStream( Stream: TStream );
var
	Writer: TWriter;
	ColItems: TKCompileItems;
begin
	Writer := TWriter.Create( Stream, ( 4 * KB ) );
	try
		Owner.DCC32Comp.FIsLoading := True; { Hard Couple! }
		try
		  Writer.Root := ( GetFirstObject( [Owner.DCC32Comp.Owner, Owner.DCC32Comp] ) as TComponent );
			ColItems := TKCompileItems.Create( Owner.DCC32Comp );
			try
				ColItems.Add.Assign( Self );
				{
				ColItems[0].AfterCompile := nil;
				ColItems[0].BeforeCompile := nil;
				}
				Writer.WriteCollection( ColItems );
			finally
				ColItems.Free;
			end;
		finally
			Owner.DCC32Comp.FIsLoading := False; { Hard Couple! }
		end;
	finally
		Writer.Free;
	end;
end;

{ TKCompileItems }

constructor TKCompileItems.Create( AComp: TKDCC32 );
begin
	ForceObject( AComp );
	FLastCompiledItem := nil;
	inherited Create( AComp, TKCompileItem, True );
end;

function TKCompileItems.Add: TKCompileItem;
begin
	Result := TKCompileItem( inherited Add );
	if CheckTrimStr( DCC32Comp.InitialDirectory ) then
	begin
		Result.InitialDirectory := DCC32Comp.InitialDirectory;
		Result.DCUOutPutDir := DCC32Comp.InitialDirectory;
		Result.EXEOutPutDir := DCC32Comp.InitialDirectory;
	end;
end;

function TKCompileItems.GetItem( Index: Integer ): TKCompileItem;
begin
	Result := TKCompileItem( inherited GetItem( Index ) );
end;

function TKCompileItems.GetItemByName( const AName: string ): TKCompileItem;
begin
	Result := TKCompileItem( inherited GetItemByName( AName ) );
end;

function TKCompileItems.GetOwnerComp: TKDCC32;
begin
	Result := TKDCC32( inherited GetOwnerComp );
end;

procedure TKCompileItems.SetItem( Index: Integer; AItem: TKCompileItem );
begin
	inherited SetItem( Index, AItem );
end;

procedure TKCompileItems.Update( Item: TCollectionItem );
var
	i: Integer;
begin
	if CheckObject( Item ) then
		DCC32Comp.UpdateItem( TKCompileItem( Item ) )
	else
		for i := 0 to Count - 1 do
			DCC32Comp.UpdateItem( Items[i] );
end;

function TKCompileItems.GetAttrCount: Integer;
begin
	Result := COMPILE_ITEMS_ATTRCOUNT;
end;

function TKCompileItems.GetAttr( Index: Integer ): string;
begin
	case Index of
		0: Result := COLLECTION_ITEM_INDEX;
		1: Result := COLLECTION_ITEM_NAME;
		2: Result := COLLECTION_ITEM_GRPIDX;
		3: Result := COMPILE_ITEMS_COMPFNTYP;
	else
			Result := inherited GetAttr( Index );
	end;
end;

function TKCompileItems.GetCompileFileNameType( Item: TKCompileItem ): TKCompileFileNameType;
begin
	Result := cfntUnKnown;
	if CheckStrContains( DELPHI_PROJECT_EXT, Item.CompileFileName ) then
		Result := cfntProject
	else if CheckStrContains( DELPHI_PACKAGESOURCE_EXT, Item.CompileFileName ) then
		Result := cfntPackage
	else if	CheckStrContains( DELPHI_UNIT_EXT, Item.CompileFileName ) then
		Result := cfntUnit;
end;

function TKCompileItems.GetItemAttr( Index, ItemIndex: Integer ): string;
begin
	case Index of
		0: Result := Format( '%-d', [Items[ItemIndex].ID] );
		1: Result := Items[ItemIndex].Name;
		2: Result := Format( '%-d', [Items[ItemIndex].GroupIndex] );
		3: Result := COMPILE_ITEMS_COMPFNTYPE[GetCompileFileNameType( Items[ItemIndex] )];
	else
		Result := inherited GetItemAttr( Index, ItemIndex );
	end;
end;

procedure TKCompileItems.ClearStatistics( Group: Cardinal );
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		if ( ( Group = Cardinal( COMPILER_GROUP_ALL ) ) or ( Group = Items[i].GroupIndex ) ) then
			Items[i].ClearStatistics;
end;

procedure TKCompileItems.ClearResults( Group: Cardinal );
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		if ( ( Group = Cardinal( COMPILER_GROUP_ALL ) ) or ( Group = Items[i].GroupIndex ) ) then
			Items[i].ClearResults;
end;

function TKCompileItems.GetCompiledData: TKDCC32CompiledData;
var
	i: Integer;
begin
	ZeroMemory( @Result, SizeOf( TKDCC32CompiledData ) );
	if CheckCollection( Self ) then
		for i := 0 to Count - 1 do
			if Items[i].Enabled then
			begin
				Result.Lines := Result.Lines + Items[i].CompiledData.Lines;
				Result.MSecs := Result.MSecs + Items[i].CompiledData.MSecs;
				Result.CodeBytes := Result.CodeBytes + Items[i].CompiledData.CodeBytes;
				Result.DataBytes := Result.DataBytes + Items[i].CompiledData.DataBytes;
			end;
end;

{ TKDCC32Link }

const
	leBeforeExecAll   = 0;
	leBeforeExecGroup = 1;
	leBeforeExecItem  = 2;
	leAfterExecItem   = 3;
	leAfterExecGroup  = 4;
	leAfterExecAll    = 5;

procedure TKDCC32Link.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	case LinkEvent of
		leBeforeExecAll:
			if Assigned( FBeforeExecAll ) then
				FBeforeExecAll( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		leBeforeExecGroup:
			if Assigned( FBeforeExecGroup ) then
				FBeforeExecGroup( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		leBeforeExecItem:
			if Assigned( FBeforeExecItem ) then
				FBeforeExecItem( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		leAfterExecItem:
			if Assigned( FAfterExecItem ) then
				FAfterExecItem( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		leAfterExecGroup:
			if Assigned( FAfterExecGroup ) then
				FAfterExecGroup( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		leAfterExecAll:
			if Assigned( FAfterExecAll ) then
				FAfterExecAll( ( Owner as TKDCC32 ), Self, Cardinal( Data ) );
		else
			inherited DoLinkEvent( LinkEvent, Data );
	end;
end;

{ TKDCC32 }

constructor TKDCC32.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
{ properties redeclared as private }
	ShowWindowStyle := ssHide;
	CaptureOutput := True;
	FQuitting := False;
	FIsLoading := False;
	EnvironmentVars.Clear;
	CommandPath := '';
	CommandStyle := csNone; { csReturn!!! oops! }
	ProcessPriority := ppNormal;
	InitialDirectory := CurrentProjectPath;
	FCompileItems := TKCompileItems.Create( Self );
end;

destructor TKDCC32.Destroy;
begin
	FreeClean( FCompileItems );
	inherited Destroy;
end;

procedure TKDCC32.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKDCC32 ) then
		with ( Source as TKDCC32 ) do
		begin
			Self.InitialDirectory := InitialDirectory;
			Self.CompileItems := CompileItems;
			Self.OnLoadError := OnLoadError;
			Self.OnGroupAbort := OnGroupAbort;
			Self.Tag := Tag;
		end
	else
		inherited Assign( Source );
end;

procedure TKDCC32.SetCompileItems( Value: TKCompileItems );
begin
	FCompileItems.Assign( Value );
end;

procedure TKDCC32.UpdateItem( Item: TKCompileItem );
begin
{
	Receive notifications for any property changed.
	Broadcast are received (a call for each item) if the GroupIndex has changed.
}
end;

function CompareProc( Item1, Item2: Pointer ): Integer;
begin
	if ( Cardinal( Item1 ) < Cardinal( Item2 ) ) then
		Result := -1
	else 	if ( Cardinal( Item1 ) > Cardinal( Item2 ) ) then
		Result := 1
	else
		Result := 0;
end;

procedure TKDCC32.Quit;
begin
	FQuitting := True;
end;

procedure TKDCC32.DoLoadError( Item: TKCompileItem );
begin
	if Assigned( FOnLoadError ) then
		FOnLoadError( Self, Item, Item.GroupIndex );
end;

function TKDCC32.CompileItem( Item: TKCompileItem ): Boolean;
begin
	Result := true;
	if ( Item.Enabled and ( not ( Item.GroupAborted or FQuitting ) ) ) then
	begin
		Item.SetUpDCC32;
		Item.DoBeforeCompile;
		Result := ( inherited Execute );
		if Result then
			CompileItems.FLastCompiledItem := Item; { Hard Couple! }
		if ( not ( Result and Item.ParseDCC32Result( CaptureOutputResult ) ) ) then
			Result := Item.PerformErrorAction;
{ if GroupAbort is dceaIgnore, notify! (antes tava como else...) }
		if Result then
			Item.DoAfterCompile;
	end;
end;

function TKDCC32.ExecuteGroup( GroupIndex: Cardinal ): Boolean;

	function GetCompileItems( GroupIndex: Cardinal ): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 0 to CompileItems.Count - 1 do
			Inc( Result, Ord( CompileItems.Items[i].GroupIndex = GroupIndex ) );
	end;

var
	i: Integer;
	sSavedInitialDirectory: string;
begin
	FQuitting := False;
	i := GetCompileItems( GroupIndex );
	Result := CheckCollection( CompileItems ) and ( i > 0 );
	if Result then
	begin
		CompileItems.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, i );
		sSavedInitialDirectory := InitialDirectory;
		try
			NotifyLinks( leBeforeExecGroup, GroupIndex ); { Can reveive the new ItemIndex }
			CompileItems.FLastCompiledItem := nil; { Hard Couple! }
			for i := 0 to CompileItems.Count - 1 do
			begin
				if FQuitting then
					Exit;
				if ( CompileItems.Items[i].GroupIndex = GroupIndex ) then
				begin
					NotifyLinks( leBeforeExecItem, i );
{$BOOLEVAL ON}
					Result := ( Result and CompileItem( CompileItems.Items[i] ) );
{$BOOLEVAL OFF}
					NotifyLinks( leAfterExecItem, i );
				end;
			end;
			NotifyLinks( leAfterExecGroup, GroupIndex );
		finally
			CommandLine := '';
			InitialDirectory := sSavedInitialDirectory;
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

function TKDCC32.ExecuteItems( const Items: array of Cardinal ): Boolean;
var
	i: Integer;
	sSavedInitialDirectory: string;
begin
{
	Can use ArrayOfConstToVar...
	V := ArrayOfConstToVar( Items );
	Result := ExecuteItemsEx( V );
}
	FQuitting := False;
	Result := CheckCollection( CompileItems );
	if Result then
	begin
		CompileItems.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, High( Items ) );
		sSavedInitialDirectory := InitialDirectory;
		try
			CompileItems.FLastCompiledItem := nil; { Hard Couple! }
			for i := Low( Items ) to High( Items ) do
			begin
				if FQuitting then
					Exit;
			{ Here we must TypeCast Items to Integer because in Delphi4 cardinal
				violates the Integer Range and generates a warning here. Even, CompileItems.Count
				also returns Integer ranged values... }
				if ( Integer( Items[i] ) >= CompileItems.Count ) then
					RaiseExceptionFmt( EKDCC32, sErrCIInvArrayCompileIndex, [Items[i]] );
				NotifyLinks( leBeforeExecItem, Items[i] );
{$BOOLEVAL ON}
				Result := ( Result and CompileItem( CompileItems[Items[i]] ) );
{$BOOLEVAL OFF}
				NotifyLinks( leAfterExecItem, Items[i] );
			end;
		finally
			CommandLine := '';
			InitialDirectory := sSavedInitialDirectory;
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

function TKDCC32.ExecuteItemsEx( const Items: Variant ): Boolean;
var
	i,
	j,
	lo,
	hi: Integer;
	sSavedInitialDirectory: string;
begin
	FQuitting := False;
	Result := CheckCollection( CompileItems ) and
		( not ( VarIsEmpty( Items ) or VarIsNull( Items ) ) );
	if ( not Result ) then
		Exit;
	lo := VarArrayLowBound( Items, 1 );
	hi := VarArrayHighBound( Items, 1 );
	Result := ( lo < hi ) and ( lo >= 0 );
	if Result then
	begin
		CompileItems.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, hi );
		sSavedInitialDirectory := InitialDirectory;
		try
			CompileItems.FLastCompiledItem := nil; { Hard Couple! }
			for i := lo to hi do
			begin
				if FQuitting then
					Exit;
				j := TVarData( Items[i] ).VInteger;
				if ( j >= CompileItems.Count ) then
					RaiseExceptionFmt( EKDCC32, sErrCIInvArrayCompileIndex, [j] );
				NotifyLinks( leBeforeExecItem, j );
{$BOOLEVAL ON}
				Result := ( Result and CompileItem( CompileItems[j] ) );
{$BOOLEVAL OFF}
				NotifyLinks( leAfterExecItem, j );
			end;
		finally
			CommandLine := '';
			InitialDirectory := sSavedInitialDirectory;
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

function TKDCC32.Execute: Boolean;
const
	START_GROUP_INDEX: array[Boolean] of Byte = ( 0, 1 );
var
	i,
	j: Integer;
	GroupList: TList;
	sSavedInitialDirectory: string;
begin
  FQuitting := False;
	Result := CheckCollection( CompileItems );
	if Result then
	begin
		CompileItems.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, CompileItems.Count );
		sSavedInitialDirectory := InitialDirectory;
		try
			CompileItems.FLastCompiledItem := nil; { Hard Couple! }
			GroupList := TList.Create;
			try
				for i := 0 to CompileItems.Count - 1 do
					if ( GroupList.IndexOf( Pointer( CompileItems.Items[i].GroupIndex ) ) = -1 ) then
						GroupList.Add( Pointer( CompileItems.Items[i].GroupIndex ) );
				GroupList.Sort( CompareProc );
{ If there is any NULL_GROUP item, start at group one..., otherwise start a zero! (groupidx 1) }
				i := START_GROUP_INDEX[( Cardinal( GroupList.Items[0] ) = COMPILER_GROUP_NULL )];
				for i := i to GroupList.Count - 1 do
				begin
					if FQuitting then
						Exit;
					NotifyLinks( leBeforeExecGroup, i ); { Can reveive the new ItemIndex }
					for j := 0 to CompileItems.Count - 1 do
					begin
						if FQuitting then
							Exit;
{ DO NOT compile 0-based groups }
						if ( Cardinal( GroupList.Items[i] ) <> COMPILER_GROUP_NULL ) and
							 ( Cardinal( GroupList.Items[i] ) = CompileItems.Items[j].GroupIndex ) then
						begin
							NotifyLinks( leBeforeExecItem, j );
{$BOOLEVAL ON}
							Result := ( Result and CompileItem( CompileItems.Items[j] ) );
{$BOOLEVAL OFF}
							NotifyLinks( leAfterExecItem, j );
						end;
					end;
					NotifyLinks( leAfterExecGroup, i );
				end;
{ Late compiling of 0-based groups }
				if ( Cardinal( GroupList.Items[0] ) = COMPILER_GROUP_NULL ) then
				begin
					NotifyLinks( leBeforeExecGroup, COMPILER_GROUP_NULL );
					for j := 0 to CompileItems.Count - 1 do
						if ( CompileItems.Items[j].GroupIndex = COMPILER_GROUP_NULL ) then
						begin
							NotifyLinks( leBeforeExecItem, j );
{$BOOLEVAL ON}
							Result := ( Result and CompileItem( CompileItems.Items[j] ) );
{$BOOLEVAL OFF}
							NotifyLinks( leAfterExecItem, j );
						end;
					NotifyLinks( leAfterExecGroup, COMPILER_GROUP_NULL );
				end;
			finally
				GroupList.Free;
			end;
		finally
			CommandLine := '';
			InitialDirectory := sSavedInitialDirectory;
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

procedure TKDCC32.MergeFromStream( Stream: TStream );
var
	DCC32: TKDCC32;
begin
	if CheckCollection( CompileItems ) then
	begin
		FIsLoading := True;
		try
			DCC32 := TKDCC32.Create( nil );
			try
			  DCC32.OnLoadError := OnLoadError;
        DCC32.FIsLoading := True;
				Stream.ReadComponent( DCC32 );
				Tag := DCC32.Tag;
				InitialDirectory := DCC32.InitialDirectory;
				CompileItems.AddItems( DCC32.CompileItems );
			finally
				DCC32.Free;
			end;
		finally
		  FIsLoading := False;
		end;
	end
	else
		LoadFromStream( Stream );
end;

procedure TKDCC32.LoadFromStream( Stream: TStream );
var
	DCC32: TKDCC32;
begin
{ We must explicity create a new instance an pass it to Stream because the Self
	assignment causes some naming reference problems on the DCC32 Owner }
	FIsLoading := True;
	try
		DCC32 := TKDCC32.Create( nil );
		try
			DCC32.OnLoadError := OnLoadError;
			DCC32.FIsLoading := True;
			Stream.ReadComponent( DCC32 );
			Assign( DCC32 );
		finally
			DCC32.Free;
		end;
	finally
		FIsLoading := False;
	end;
end;

function TKDCC32.LoadFromIniStream( Stream: TStream ): TKDCC32LoadIniStatus;
const
	DCC32_INV_COUNT = -3;
	DCC32_NO_COUNTER = -2;

	DCC32_INIDIR = 0;
	DCC32_TAG    = 1;
	DCC32_COUNT  = 2;
	DCC32_LOADTP = 3;
	DCC32_KLIBVR = 4;

	COMPILEITEM_CFILENAME = 0;
	COMPILEITEM_FILESTYPE = 1;
	COMPILEITEM_ENUMTYPE  = 2;
	COMPILEITEM_MAPFILE   = 3;
	COMPILEITEM_OPTIONS   = 4;
	COMPILEITEM_SWITCHES  = 5;
	COMPILEITEM_VERSION   = 6;
	COMPILEITEM_DCUOUTPUT = 7;
	COMPILEITEM_ENABLED   = 8;
	COMPILEITEM_ERRORACT  = 9;
	COMPILEITEM_EXEOUTPUT = 10;
	COMPILEITEM_GRPIDX    = 11;
	COMPILEITEM_STAT      = 12;
	COMPILEITEM_IMGBSADDR = 13;
	COMPILEITEM_INCPATHS  = 14;
	COMPILEITEM_INITDIR   = 15;
	COMPILEITEM_MAXSTSIZE = 16;
	COMPILEITEM_MINSTSIZE = 17;
	COMPILEITEM_OBJPATHS  = 18;
	COMPILEITEM_PACKENVS  = 19;
	COMPILEITEM_RESPATHS  = 20;
	COMPILEITEM_RUNTIMEPK = 21;
	COMPILEITEM_SYMBOLDEF = 22;
	COMPILEITEM_TARGETEXT = 23;
	COMPILEITEM_UNITALIAS = 24;
	COMPILEITEM_UNITPATHS = 25;

	function RetrieveList( sl: TKStrings; NameIndex, ValueIndex: Integer ): string;
	var
		i: Integer;
		sName: string;
	begin
		sName := '';
		Result := '';
		if ( NameIndex < ( COMPILEITEM_INI_MAXNAME_COUNT - 1 ) ) then
			sName := COMPILEITEM_INI_NAMES[NameIndex + 1];
		if CheckStr( sl.ValuesByIndex[ValueIndex] ) then
		begin
			Result := sl.ValuesByIndex[ValueIndex] + CH_CRLF;
			if ( Pos( CH_BRACKET_OPEN, Result ) = 1 ) then
				Delete( Result, 1, 1 );
		end;
		i := ( ValueIndex + 1 );
		while ( ( i < sl.Count ) and ( not CheckStrContains( sl.ValuesByIndex[i],
			CH_BRACKET_CLOSE ) ) and ( sl.IndexOfName( sName ) <> i ) ) do
		begin
			Result := Result + sl.Strings[i] + CH_CRLF;
			Inc( i );
		end;
		if CheckStr( Result ) then
		begin
			if ( ( Result[Length( Result ) - 1] = CH_CR ) and
					 ( Result[Length( Result )] = CH_LF ) ) then
				Delete( Result, Length( Result ) - 1, 2 );
			i := Pos( CH_BRACKET_CLOSE, Result );
			if ( i > 0 ) then
				Delete( Result, i, 1 );
		end;
	end;

	procedure RetriveOptions( ci: TKCompileItem; SetValue: Integer );
	var
		i: TKDCC32Option;
		opt: TKDCC32Options;
	begin
		if ( SetValue < 0 ) then
			Exit
		else
		begin
			opt := [];
			for i := Low( TKDCC32Option ) to High( TKDCC32Option ) do
				if ( Integer( i ) in TKIntegerSet( SetValue ) ) then
					Include( opt, i );
			ci.DCC32Options := opt;
		end;
	end;

	procedure RetriveSwitches( ci: TKCompileItem; SetValue: Integer );
	var
		i: TKDCC32Switch;
		sw: TKDCC32Switches;
	begin
		if ( SetValue < 0 ) then
			Exit
		else
		begin
			sw := [];
			for i := Low( TKDCC32Switch ) to High( TKDCC32Switch ) do
				if ( Integer( i ) in TKIntegerSet( SetValue ) ) then
					Include( sw, i );
			ci.DCC32Switches := sw;
		end;
	end;

	procedure RetrivePackEnv( ci: TKCompileItem; SetValue: Integer );
	var
		i: TKPackageEnvironment;
		pe: TKPackageEnvironments;
	begin
		if ( SetValue < 0 ) then
			Exit
		else
		begin
			pe := [];
			for i := Low( TKPackageEnvironment ) to High( TKPackageEnvironment ) do
				if ( Integer( i ) in TKIntegerSet( SetValue ) ) then
					Include( pe, i );
			ci.PackageEnvironments := pe;
		end;
	end;

var
	sec,
	sec_i: TKStrings;
	i,
	j,
	k,
	iCount,
	iError: Integer;
	bLoad,
	bHasMainSec: Boolean;
	ci: TKCompileItem;
	sErrorMsg: string;
begin
	sec := TKStrings.Create;
	try
		Result := lisNoCounter;
		GetSectionsFromStream( Stream, sec );
		if CheckStrings( sec ) then
		begin
			sec_i := TKStrings.Create;
			try
				bLoad := True;
				iCount := DCC32_NO_COUNTER;
				Stream.Position := 0;
				bHasMainSec := False;
				if CheckStrContains( Format( DCC32MIN_INI_FMT, [CURRENT_DCC32_VERSION, ''] ),
					sec.Strings[DCC32_MAIN_INI_SEC_IDX] ) then
				begin
					GetSectionFromStream( Stream, sec.Strings[DCC32_MAIN_INI_SEC_IDX], sec_i );
					for i := 0 to DCC32MAIN_INI_MAXNAME_COUNT - 1 do
					begin
						j := sec_i.IndexOfName( DCC32MAIN_INI_NAMES[i] );
						if ( j <> -1 ) then
							case i of
								DCC32_INIDIR: InitialDirectory := sec_i.ValuesByIndex[j];
								DCC32_TAG   : Tag := StrToIntDef( sec_i.ValuesByIndex[j], Tag );
								DCC32_COUNT : iCount := StrToIntDef( sec_i.ValuesByIndex[j], DCC32_INV_COUNT );
								DCC32_LOADTP: bLoad := Boolean( StrToIntDef( sec_i.ValuesByIndex[j], Byte( bLoad ) ) );
								DCC32_KLIBVR: { Ignore KLibVersion! };
							end;
					end;
					bHasMainSec := True;
				end;
				if bLoad then
					CompileItems.ClearItems;
				iError := 0;
				sErrorMsg := '';
{
				sec.Delete( DCC32_MAIN_INI_SEC_IDX );
				sec.AdjustForValues;
				SortValues( sec );
				sec.SortEx( stValues );
				for i := 0 to sec.Count - 1 do
				.... see code after end of file ....
}
				for i := ( DCC32_MAIN_INI_SEC_IDX + Ord( bHasMainSec ) ) to sec.Count - 1 do
					try
						GetSectionFromStream( Stream, sec.Strings[i], sec_i );
						ci := CompileItems.Add;
						if ( iCount >= 0 ) then
							Dec( iCount );
						ci.Name := sec.Names[i];
						for j := 0 to COMPILEITEM_INI_MAXNAME_COUNT - 1 do
						begin
							k := sec_i.IndexOfName( COMPILEITEM_INI_NAMES[j] );
							if ( k <> -1 ) then
								case j of
									COMPILEITEM_CFILENAME:
										ci.CompileFileName := sec_i.ValuesByIndex[k];
									COMPILEITEM_FILESTYPE:
										ci.DCC32CompiledFilesType := TKDCC32CompiledFilesType( StrToIntDef(
											sec_i.ValuesByIndex[k], Byte( ci.DCC32CompiledFilesType ) ) );
									COMPILEITEM_ENUMTYPE :
										ci.DCC32EnumType := TKDCC32EnumType( StrToIntDef(
											sec_i.ValuesByIndex[k], Byte( ci.DCC32EnumType ) ) );
									COMPILEITEM_MAPFILE  :
										ci.DCC32MapFile := TKDCC32MapFile( StrToIntDef(
											sec_i.ValuesByIndex[k], Byte( ci.DCC32MapFile ) ) );
									COMPILEITEM_OPTIONS  :
										RetriveOptions( ci, StrToIntDef( sec_i.ValuesByIndex[k], -1 ) );
									COMPILEITEM_SWITCHES :
										RetriveSwitches( ci, StrToIntDef( sec_i.ValuesByIndex[k], -1 ) );
									COMPILEITEM_VERSION  :
										ci.DCC32Version := TKDCC32Version( StrToIntDef(
											sec_i.ValuesByIndex[k], Byte( ci.DCC32Version ) ) );
									COMPILEITEM_DCUOUTPUT:
										ci.DCUOutPutDir := sec_i.ValuesByIndex[k];
									COMPILEITEM_ENABLED  :
										ci.Enabled := Boolean( StrToIntDef( sec_i.ValuesByIndex[k], Byte( ci.Enabled ) ) );
									COMPILEITEM_ERRORACT :
										ci.ErrorAction := TKDCC32CompileErrorAction( StrToIntDef(
											sec_i.ValuesByIndex[k], Byte( ci.ErrorAction ) ) );
									COMPILEITEM_EXEOUTPUT:
										ci.EXEOutPutDir := sec_i.ValuesByIndex[k];
									COMPILEITEM_GRPIDX   :
										ci.GroupIndex := StrToIntDef( sec_i.ValuesByIndex[k], ci.GroupIndex );
									COMPILEITEM_STAT     :
										ci.HasStatistics := Boolean( StrToIntDef( sec_i.ValuesByIndex[k], Byte( ci.HasStatistics ) ) );
									COMPILEITEM_IMGBSADDR:
										ci.ImageBaseAddr := StrToIntDef( sec_i.ValuesByIndex[k], ci.ImageBaseAddr );
									COMPILEITEM_INCPATHS :
										ci.IncludePaths.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_INITDIR  :
										ci.InitialDirectory := sec_i.ValuesByIndex[k];
									COMPILEITEM_MAXSTSIZE:
										ci.MaxStackSize := StrToIntDef( sec_i.ValuesByIndex[k], ci.MaxStackSize );
									COMPILEITEM_MINSTSIZE:
										ci.MinStackSize := StrToIntDef( sec_i.ValuesByIndex[k], ci.MinStackSize );
									COMPILEITEM_OBJPATHS :
										ci.ObjectPaths.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_PACKENVS :
										RetrivePackEnv( ci, StrToIntDef( sec_i.ValuesByIndex[k], -1 ) );
									COMPILEITEM_RESPATHS :
										ci.ResourcePaths.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_RUNTIMEPK:
										ci.RunTimePackages.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_SYMBOLDEF:
										ci.SymbolDefines.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_TARGETEXT:
										ci.TargetExtension := sec_i.ValuesByIndex[k];
									COMPILEITEM_UNITALIAS:
										ci.UnitAliases.Text := RetrieveList( sec_i, j, k );
									COMPILEITEM_UNITPATHS:
										ci.UnitPaths.Text := RetrieveList( sec_i, j, k );
								end;
						end;
					except
						on E: EKLIB do
						begin
							FreeClean( ci );
							Inc( iError );
							sErrorMsg := sErrorMsg + Format( '%d: %s.%s' + CH_CRLF, [iError, E.ClassName, E.Message] );
						end
						else
						begin
							FreeClean( ci );
							raise;
						end;	
					end;
				if ( iError > 0 ) then
					ShowErrorFmt( sErrDCC32InvLoadIniStrm, [iError, sErrorMsg] );
				case iCount of
					DCC32_INV_COUNT : Result := lisInvCounter;
					DCC32_NO_COUNTER: Result := lisNoCounter;
				else
					if ( iCount = 0 ) then
						Result := lisCountSync
					else if ( iCount < 0 ) then
						Result := lisCountLower
					else
						Result := lisCountGreater;
				end;
			finally
				sec_i.Free;
			end;
		end;
	finally
		sec.Free;
	end;
end;

procedure TKDCC32.SaveToStream( Stream: TStream );
begin
	Stream.WriteComponent( Self );
end;

procedure TKDCC32.SaveToIniStream( Stream: TStream );
{
This method follow the rules defined in DCC32_Ini_Grammar.txt
}
var
	s: string;
	i: Integer;
begin
	s := DCC32_INI_COMMENT;
	Stream.WriteBuffer( Pointer( s )^, Length( s ) );
	s := Format( DCC32MAIN_INI_SECTION, [ Format( DCC32MIN_INI_FMT, [CURRENT_DCC32_VERSION,
		FormatDateTime( DCC32_INI_DATETIME_FMT, Now )] ), InitialDirectory, Tag,
		CompileItems.Count, Byte( FSaveIniLoadType ), KLIBVersion.Version,
		FormatDateTime( DCC32_INI_DATETIME_FMT, KLIBVersion.Release )] );
	Stream.WriteBuffer( Pointer( s )^, Length( s ) );
	for i := 0 to CompileItems.Count - 1 do
		with CompileItems.Items[i] do
		begin
{ In line 3 and 8 of the statement bellow, we must playing a "old dirty trick" to
	assign a one byte structure directly with a four byte structure without a compiler
	semantical error!!! }
			s := Format( COMPILEITEM_INI_SECTION, [Name, ID, CompileFileName,
				Byte( DCC32CompiledFilesType ), Byte( DCC32EnumType ), Byte( DCC32MapFile ),
				Byte( Integer( PKIntegerSet( @DCC32Options )^ ) ),
				Integer( TKIntegerSet( DCC32Switches ) ), Byte( DCC32Version ), DCUOutPutDir,
				Byte( Enabled ), Byte( ErrorAction ), EXEOutPutDir, GroupIndex,
				Byte( HasStatistics ), ImageBaseAddr, BuildCmdLineList( IncludePaths ), InitialDirectory,
				MaxStackSize, MinStackSize, BuildCmdLineList( ObjectPaths ),
				Byte( Integer( PKIntegerSet( @PackageEnvironments )^ ) ),
				BuildCmdLineList( ResourcePaths ), BuildCmdLineList( RunTimePackages ),
				BuildCmdLineList( SymbolDefines ), TargetExtension, BuildCmdLineList( UnitAliases ),
				BuildCmdLineList( UnitPaths )] );
			Stream.WriteBuffer( Pointer( s )^, Length( s ) );
		end;
end;

procedure TKDCC32.MergeFromFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
	try
		MergeFromStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKDCC32.LoadFromFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
	try
		LoadFromStream( Stream );
	finally
		Stream.Free;
	end;
end;

function TKDCC32.LoadFromIniFile( const FileName: string ): TKDCC32LoadIniStatus;
var
	Stream: TStream;
begin
	ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead or fmShareDenyWrite );
	try
		Result := LoadFromIniStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKDCC32.SaveToFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	Stream := TFileStream.Create( FileName, fmCreate or fmShareExclusive );
	try
		SaveToStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKDCC32.SaveToBatchStream( Stream: TStream );
const
	BATCH_COMMAND_CD = 'cd "%s"'#13#10;
var
	i: Integer;
	s: string;
begin
	if CheckCollection( CompileItems ) then
		with CompileItems, Stream do
			for i := 0 to Count - 1 do
				with Items[i] do
					if ( Enabled and CheckTrimStr( CompileFileName ) ) then
					begin
						s := ExtractFileDrive( CompileFileName ) + CH_CRLF;
						s := s + Format( BATCH_COMMAND_CD, [ExtractFilePath( CompileFileName )] );
						s := s + CompilerCommandLine + CH_CRLF;
						Stream.WriteBuffer( Pointer( s )^, Length( s ) );
					end;
end;

procedure TKDCC32.SaveToBatchFile( FileName: string );
var
	Stream: TStream;
begin
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	Stream := TFileStream.Create( FileName, fmCreate or fmShareExclusive );
	try
		SaveToBatchStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKDCC32.SaveToIniFile( const FileName: string; IsLoad: Boolean );
var
	Stream: TStream;
begin
  FSaveIniLoadType := IsLoad;
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	Stream := TFileStream.Create( FileName, fmCreate or fmShareExclusive );
	try
		SaveToIniStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure ParseDOFFileToCompileItem( Item: TKCompileItem );
const

{ DOF File Sections }
	COMPILER_SECTION_0 = 'Compiler';
	LINKER_SECTION_1 = 'Linker';
  DIRECTORIES_SECTION_2 = 'Directories';
{
  PARAMETERS_SECTION_3 = 'Parameters';                Unused DOF sections
  VERINFO_SECTION_4 = 'Version Info';
  VERINFO_KEYS_SECTION_5 = 'Version Info Keys';
  EXCPACK_SECTION_6 = 'Excluded Packages';
}

{ CompileItem Types Maps }

  COMPILER_DIRECTIVE_SWITCH_MAP: array[TKDCC32Switch] of Char =
		(
			'A', { dsAlign           } 'B', { dsBoolEval        } 'C', { dsAssertions      }
			'D', { dsDCUDebuginfo    } 'G', { dsImporteddata    } 'H', { dsLongStrings 	   }
			'I', { dsIOChecks        } 'J', { dsWriteableConst  } 'L', { dsLocalSymbols    }
			'M', { dsTypeInfo        } 'O', { dsOptimization    } 'P', { dsOpenStrings     }
			'Q', { dsOverFlowChecks  } 'R', { dsRangeChecks     } 'T', { dsTypedAddress    }
			'U', { dsSafeDivide      } 'V', { dsVarStringChecks } 'W', { dsStackFrames     }
			'X', { dsExtendedSyntax  } 'Y'  { dsReferenceInfo   }
		);

  COMPILER_MAPFILE_MAP: array[0..3] of TKDCC32MapFile =
    ( dmfNone, dmfSegments, dmfPublics, dmfDetailed );
  COMPILER_FILETYPE_MAP: array[Boolean] of TKDCC32CompiledFilesType =
    ( dcfDCU, dcfObj );

{ DOF Compiler Section Names }
  SHOW_HINTS = 'ShowHints';
  SHOW_WARNINGS = 'ShowWarnings';
  UNIT_ALIASES = 'UnitAliases';

{ DOF Linked Section Names }
  MAP_FILE = 'MapFile';
  OUTOPUT_OBJECTS = 'OutputObjs';
	CONSOLE_APP = 'ConsoleApp';
  DEBUG_INFO = 'DebugInfo';
  MIN_STACK_SIZE = 'MinStackSize';
  MAX_STACK_SIZE = 'MaxStackSize';
  IMAGE_BASE_ADDR = 'ImageBase';
{ EXE_DESCRIPTION = 'ExeDescription'; }

{ DOF Directories Section Names }
  EXE_OUTPUT_DIR = 'OutputDir';
  DCU_OUTPUT_DIR = 'UnitOutputDir';
  SEARCH_PATH = 'SearchPath';
  RT_PACKAGES = 'Packages';
  SYMBOL_DEFINES = 'Conditionals';
{ DEBUG_SEARCH_PATH = 'DebugSourceDirs'; }
  COMPILE_RT_PACKAGES = 'UsePackages';

	DOF_EXT = '.dof';
{
	ALTERAR: Adicionar uma propriedade para usar ou não os RTP ao invés de apenas
					 testar se estão ou não com strings definidas. (for future...)
}

var
	FileName: string;
	j: TKDCC32Switch;
	k: TKDCC32Switches;
	sm: TKStringStream;
	sec, sec_i: TStrings;
begin
	ForceObject( Item );
	ForceFile( Item.CompileFileName );
	if ( Item.Owner.GetCompileFileNameType( Item ) = cfntProject ) then
		FileName := ChangeFileExt( Item.CompileFileName, DOF_EXT )
	else
		RaiseExceptionFmt( EKDCC32, sErrCICouldNotParseDOF, [Item.CompileFileName] );
	ForceFile( FileName );
	sm := TKStringStream.CreateFromFile( FileName { , fmOpenRead or fmShareDenyNone } );
	try
		ForceStream( sm );
		sec := TStringList.Create;
		try
			GetSectionsFromStream( sm, sec );
			ForceStrings( sec );
			sec_i := TStringList.Create;
			try
{ Compiler Switches }
				GetSectionFromStream( sm, sec[sec.IndexOf( COMPILER_SECTION_0 )], sec_i );
				ForceStrings( sec_i );
				k := [];
				for j := Low( TKDCC32Switch ) to High( TKDCC32Switch ) do
					with sec_i do
						if Boolean( Byte( StrToInt( Values[Names[IndexOfName( COMPILER_DIRECTIVE_SWITCH_MAP[j] )]] ) ) ) then
							Include( k, j );
        Item.DCC32Switches := k;
{ Compiler Options }
        with sec_i do
        begin
          if Boolean( Byte( StrToInt( Values[Names[IndexOfName( SHOW_HINTS )]] ) ) ) then
            Item.DCC32Options := Item.DCC32Options + [doHints];
          if Boolean( Byte( StrToInt( Values[Names[IndexOfName( SHOW_WARNINGS )]] ) ) ) then
            Item.DCC32Options := Item.DCC32Options + [doWarnings];
          Item.UnitAliases.Text := Values[Names[IndexOfName( UNIT_ALIASES )]];
        end;
        sec_i.Clear;
        GetSectionFromStream( sm, sec[sec.IndexOf( LINKER_SECTION_1 )], sec_i );
        ForceStrings( sec_i );
        with sec_i do
        begin
					Item.DCC32MapFile := COMPILER_MAPFILE_MAP[StrToInt( Values[Names[IndexOfName(
            MAP_FILE )]] )];
          Item.DCC32CompiledFilesType := COMPILER_FILETYPE_MAP[Boolean( Byte( StrToInt(
            Values[Names[IndexOfName( OUTOPUT_OBJECTS )]] ) ) )];
          if ( not Boolean( Byte( StrToInt( Values[Names[IndexOfName( CONSOLE_APP )]] ) ) ) ) then
            Item.DCC32Options := Item.DCC32Options + [doConsoleAPP];
          if Boolean( Byte( StrToInt( Values[Names[IndexOfName( DEBUG_INFO )]] ) ) ) then
            Item.DCC32Options := Item.DCC32Options + [doEXEDebugInfo];
          Item.MinStackSize := StrToInt( Values[Names[IndexOfName( MIN_STACK_SIZE )]] );
          Item.MaxStackSize := StrToInt( Values[Names[IndexOfName( MAX_STACK_SIZE )]] );
          Item.ImageBaseAddr := StrToInt( Values[Names[IndexOfName( IMAGE_BASE_ADDR )]] );
          { The EXE Description Information could not be used at the DCC32! (not known :< ) }
        end;
{ Compiler Directories - Extended Options }
        sec_i.Clear;
        GetSectionFromStream( sm, sec[sec.IndexOf( DIRECTORIES_SECTION_2 )], sec_i );
        ForceStrings( sec_i );
        with sec_i do
        begin
          Item.DCUOutPutDir := Values[Names[IndexOfName( DCU_OUTPUT_DIR )]];
					Item.EXEOutPutDir := Values[Names[IndexOfName( EXE_OUTPUT_DIR )]];
{ The same as the .dpr file by default }          
          Item.InitialDirectory := ExtractFilePath( FileName );
          Item.UnitPaths.Text := Values[Names[IndexOfName( SEARCH_PATH )]];
          Item.SymbolDefines.Text := Values[Names[IndexOfName( SYMBOL_DEFINES )]];
          { Fill RunTimePackages only if UseRunTimePackages was switched on. }
          if Boolean( Byte( StrToInt( Values[Names[IndexOfName( COMPILE_RT_PACKAGES )]] ) ) ) then
            Item.RunTimePackages.Text := Values[Names[IndexOfName( RT_PACKAGES )]];
          { The DebugSearchPath Information could not be used at the DCC32! (not known :< ) }
        end;
			finally
        sec_i.Free;
      end;
    finally
      sec.Free;
    end;
  finally
    sm.Free;
  end;
end;

{
--------------------------------------------------------------------------------
---------------------------------- TKDFMData -----------------------------------
--------------------------------------------------------------------------------
}

{ TKDFMData }

constructor TKDFMData.Create( AOwner: TComponent );
begin
	ForceSingleton( AOwner, TKDFMData );
	inherited Create( AOwner );
	FData := nil;
end;

destructor TKDFMData.Destroy;
begin
	if CheckPointer( FData ) then
		FreeData;
	inherited Destroy;
end;

function TKDFMData.GetData: Pointer;
begin
	Result := FData;
end;

function TKDFMData.GetSignature: TGUID;
begin
	Result := StringToGUID( KDFMDATA_SIGNATURE );
end;

procedure TKDFMData.ReadData( Stream: TStream );
begin
	if CheckPointer( FData ) then
		FreeData;
	GetMem( FData, FDataSize );
	try
		Stream.ReadBuffer( FData^, FDataSize );
	except
		FreeData;
		raise;
	end;
end;

procedure TKDFMData.WriteData( Stream: TStream );
var
	i: Integer;
	bPad: Byte;
begin
	bPad := Byte( CH_NULL );
	for i := 0 to FDataSize - 1 do
		Stream.WriteBuffer( bPad, 1 );
end;

procedure TKDFMData.ReadSignature( Reader: TReader );
begin
	ForceStrEqual( GUIDToString( GetSignature ), Reader.ReadString );
end;

procedure TKDFMData.WriteSignature( Writer: TWriter );
begin
	Writer.WriteString( GUIDToString( GetSignature ) );
end;

procedure TKDFMData.DefineProperties( Filer: TFiler );
begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'Signature', ReadSignature, WriteSignature, True );
	Filer.DefineBinaryProperty( 'Data', ReadData, WriteData, ( FDataSize > 0 ) );
end;

procedure TKDFMData.FreeData;
begin
	ForcePointer( FData );
	FreeMem( FData, FDataSize );
	FData := nil;
end;

{
--------------------------------------------------------------------------------
------------------------------- TKDFMResource ----------------------------------
--------------------------------------------------------------------------------
}

{ TKDFMResourceStream }

constructor TKDFMResourceStream.Create( ADFMResource: TKDFMResource );
begin
	ForceObject( ADFMResource );
	inherited Create;
	FOwner := ADFMResource;
	FStream := TMemoryStream.Create;
	FReadSpecialData := False;
end;

destructor TKDFMResourceStream.Destroy;
begin
	FStream.Free;
	inherited Destroy;
end;

procedure TKDFMResourceStream.DefineProperties( Filer: TFiler );

	function HasData: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKDFMResource ) then
				Result := ( FStream.Size <> TKDFMResourceStream( Filer.Ancestor ).FStream.Size ); { Soft Couple! }
		end
		else
			Result := ( FStream.Size > 0 );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineBinaryProperty( 'Resource', ReadData, WriteData, HasData ); //True );//
end;

procedure TKDFMResourceStream.ReadData( Stream: TStream );
var
	iSize: Integer;
begin
	if ( Owner.ResourceType in [drtPointer, drtString, drtStream, drtWAVFile, drtAVIFile] ) then
	begin
		Stream.ReadBuffer( iSize, SizeOf( Integer ) );
		FStream.Size := iSize;
		FReadSpecialData := True;
	end;
	SetResource( Stream );
	FReadSpecialData := False;
end;

procedure TKDFMResourceStream.WriteData( Stream: TStream );
var
	iSize: Integer;
begin
	FStream.Position := 0;
	iSize := FStream.Size;
	if ( Owner.ResourceType in [drtPointer, drtString, drtStream, drtWAVFile, drtAVIFile] ) then
		Stream.WriteBuffer( iSize, SizeOf( Integer ) );
	Stream.CopyFrom( FStream, iSize );
	FStream.Position := 0;	
end;

procedure TKDFMResourceStream.SetResource( Value: TStream );
const
	GRAPHIC_CLASS: array[drtBitMap..drtMetaFile] of TGraphicClass =
		( TBitmap, TIcon, TMetaFile );
var
	grp: TGraphic;
begin
	if ( Designing( Owner ) or Loading( Owner ) ) then
	begin
		ForceObject( Value );
		FStream.Position := 0;
		case Owner.ResourceType of
			drtBitmap,
			drtIcon,
			drtMetaFile:
			begin
				grp := GRAPHIC_CLASS[Owner.ResourceType].Create;
				try
					grp.LoadFromStream( Value );
          FStream.Size := Value.Size;
					grp.SaveToStream( FStream );
				finally
					grp.Free;
				end;
			end;
			drtPointer,
			drtString,
			drtStream,
			drtWAVFile,
			drtAVIFile:
				if FReadSpecialData then
					FStream.CopyFrom( Value, FStream.Size )
				else
					FStream.CopyFrom( Value, Value.Size );
		end;
		FStream.Position := 0;
	end;
end;

{ TKDFMResource }

constructor TKDFMResource.Create( AOwner: TComponent );
begin
	ForceObject( AOwner );
	inherited Create( AOwner );
	FResourceType := drtBitmap;
	FDFMResourceStream := TKDFMResourceStream.Create( Self );
end;

destructor TKDFMResource.Destroy;
begin
	FreeData;
	inherited Destroy;
end;

procedure TKDFMResource.FreeData;
begin
	FreeClean( FIcon );
	FreeClean( FBitmap );
	FreeClean( FMetaFile );
	if ( DataSize > 0 ) and CheckPointer( FPointer ) then
		FreeMem( FPointer, DataSize );
	FPointer := nil;
	if ( FResourceType in [drtWAVFile, drtAVIFile] ) then
		ForceDeleteFile( FStrData );
	FStrData := '';
	if ( Designing( Self ) or Loading( Self ) ) then
  	Resource.Stream.Size := 0;
end;

procedure TKDFMResource.GetData;
const
	FILE_EXT: array[Boolean] of string[4] = ( RES_AVI_EXT, RES_WAV_EXT );
var
	fs: TFileStream;
begin
	FreeData;
	if ( DataSize > 0 ) then
	begin
		Resource.Stream.Position := 0;
		case FResourceType of
			drtBitmap:
			begin
				if ( not CheckObject( FBitmap ) ) then
					FBitmap := TBitmap.Create;
				FBitmap.LoadFromStream( Resource.Stream );
			end;
			drtIcon:
			begin
				if ( not CheckObject( FIcon ) ) then
					FIcon := TIcon.Create;
				FIcon.LoadFromStream( Resource.Stream );
			end;
			drtMetaFile:
			begin
				if ( not CheckObject( FMetaFile ) ) then
					FMetaFile := TMetaFile.Create;
				FMetaFile.LoadFromStream( Resource.Stream );
			end;
			drtPointer:
			begin
				if ( not CheckPointer( FPointer ) ) then
					FPointer := AllocMem( DataSize );
				Resource.Stream.ReadBuffer( FPointer^, DataSize );
			end;
			drtString:
			begin
				SetLength( FStrData, DataSize );
				Resource.Stream.ReadBuffer( Pointer( FStrData )^, Resource.Stream.Size );
			end;
			drtStream:
				{ do nothing, streaming already taken place };
			drtWAVFile,
			drtAVIFile:
			begin
				FStrData := uksyUtils.GetTempPath;
				if ( CheckStr( FStrData ) and ( AnsiLastChar( FStrData ) <> CH_BACK_SLASH ) ) then
					FStrData := ( FStrData + CH_BACK_SLASH );
				FStrData := GetUniqueFileName( FStrData + DEFAULT_DFMRES_FILENAME +
					FILE_EXT[( FResourceType = drtWAVFile )] );
				fs := TFileStream.Create( FStrData, fmCreate or fmShareExclusive );
				try
					ForceStreamCopy( Resource.Stream, fs );
				finally
					fs.Free;
				end;
			end;
		end;
	end;
end;

procedure TKDFMResource.SetResourceType( Value: TKDFMResourceType );
begin
	if ( Designing( Self ) or Loading( Self ) ) and ( Value <> FResourceType ) then
	begin
		FreeData;
		FResourceType := Value;
	end;
end;

function TKDFMResource.GetDataSize: Cardinal;
begin
	Result := FDFMResourceStream.Stream.Size;
end;

procedure TKDFMResource.SetResource( Value: TKDFMResourceStream );
begin
	if ( Designing( Self ) or Loading( Self ) ) then
		FDFMResourceStream.SetResource( Value.Stream );
end;

function TKDFMResource.GetAsBitmap: TBitmap;
begin
	GetData;
	Result := FBitmap;
end;

function TKDFMResource.GetAsIcon: TIcon;
begin
	GetData;
	Result := FIcon;
end;

function TKDFMResource.GetAsMetaFile: TMetaFile;
begin
	GetData;
	Result := FMetaFile;
end;

function TKDFMResource.GetAsPointer: Pointer;
begin
	GetData;
	Result := FPointer;
end;

function TKDFMResource.GetAsString: string;
begin
	GetData;
	Result := FStrData;
end;

function TKDFMResource.GetAsStream: TMemoryStream;
begin
	GetData;
	Result := Resource.Stream;
end;

function TKDFMResource.GetAsWAVFile: string;
begin
	if ( CheckFile( FStrData ) and CheckStrEqual( ExtractFileExt( FStrData ), RES_WAV_EXT ) ) then
		Result := FStrData
	else
	begin
		GetData;                     
		Result := FStrData;
	end;
end;

function TKDFMResource.GetAsAVIFile: string;
begin
	if ( CheckFile( FStrData ) and CheckStrEqual( ExtractFileExt( FStrData ), RES_AVI_EXT ) ) then
		Result := FStrData
	else
	begin
		GetData;
		Result := FStrData;
	end;	
end;

{
--------------------------------------------------------------------------------
--------------------------------- TKCustomDump ---------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomDump }

constructor TKCustomDump.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ControlStyle := [csFramed];
	FBorder := bsSingle;
	Color := clWhite;
	FPainting := False;
	FPrinting := False;
	FSearching := False;
	FSelDataColor := clCaptionText;
	FSelDataBackColor := clActiveCaption;
	Font.Name := 'FixedSys';
	FBackLineOffSet := 3;
	Width := 300;
	Height := 200;
end;

destructor TKCustomDump.Destroy;
begin
	FAddress := nil;
	FreeClean( FMemoryStream );
	inherited Destroy;
end;

procedure TKCustomDump.CreateParams( var Params: TCreateParams );
begin
	inherited CreateParams( Params );
	with Params do
	begin
		if ( FBorder = bsSingle ) then
			Style := Style or WS_BORDER;
		Style := Style or WS_VSCROLL;
	end;
end;

procedure TKCustomDump.ForceAddress;
begin
	if ( not CheckPointer( Address ) ) then
		RaiseException( EKCustomDump, sErrInvCustomDumpAddr );
end;

procedure TKCustomDump.ForceOffSet( OffSet: Integer );
begin
	if ( not ValueBetween( OffSet, 0, ( DataSize - 1 ), True ) ) then
		RaiseExceptionFmt( EKCustomDump, sErrInvCustomDumpOffSet, [OffSet, ( DataSize - 1 )] );
end;

procedure TKCustomDump.SetFileName( const Value: string );
begin
	if ( not CheckStrEqual( Value, FFileName ) ) then
	begin
		if ( not CheckTrimStr( Value ) ) then
		begin
			FFileName := '';
			Address := nil;
			DataSize := 0;
		end
		else
			LoadFromFile( Value ); { Will force file to exists! }
	end;
end;

procedure TKCustomDump.SetTopLine( Value: Integer );
var
	rt: TRect;
	LinesMoved: Integer;
begin
	if ( Value <> FTopLine ) then
	begin
		if ( Value < 0 ) then
			Value := 0;
		if ( Value >= FLineCount ) then
			Value := ( FLineCount - 1 );
		LinesMoved := ( FTopLine - Value );
		FTopLine := Value;
		SetScrollPos( Handle, SB_VERT, FTopLine, True );
		if ( Abs( LinesMoved ) = 1 ) then
		begin
			rt := Bounds( 1, 0, ClientWidth, ClientHeight - FItemHeight );
			if ( LinesMoved = 1 ) then
				OffsetRect( rt, 0, FItemHeight );
			ScrollWindow( Handle, 0, ( FItemHeight * LinesMoved ), @rt, nil );
			if ( LinesMoved = -1 ) then
			begin
				rt.Top := ClientHeight - FItemHeight;
				rt.Bottom := ClientHeight;
			end
			else
			begin
				rt.Top := 0;
				rt.Bottom := FItemHeight;
			end;
			InvalidateRect( Handle, @rt, False );
		end
		else
			Invalidate;
	end;
end;

procedure TKCustomDump.SetCurrentLine( Value: Integer );
var
	rt: TRect;
begin
	if ( Value <> FCurrentLine ) then
	begin
		if ( Value < 0 ) then
			Value := 0;
		if ( Value >= FLineCount ) then
			Value := ( FLineCount - 1 );
		if ( FCurrentLine >= FTopLine ) and ( FCurrentLine < FTopLine + FVisibleLines - 1 ) then
		begin
			rt := Bounds( 0, 0, 1, FItemHeight );
			OffsetRect( rt, 0, ( FCurrentLine - FTopLine ) * FItemHeight );
			Windows.InvalidateRect( Handle, @rt, True );
		end;
		FCurrentLine := Value;
		rt := Bounds( 0, 0, 1, FItemHeight );
		OffsetRect( rt, 0, ( FCurrentLine - FTopLine ) * FItemHeight );
		Windows.InvalidateRect( Handle, @rt, True );
		ScrollIntoView;
	end;
end;

procedure TKCustomDump.SetBorder( Value: TBorderStyle );
begin
	if ( Value <> FBorder ) then
	begin
		FBorder := Value;
		RecreateWnd;
	end;
end;

procedure TKCustomDump.SetDataColor( Index: Integer; Value: TColor );
begin
	case Index of
		SELDATA_COLOR_IDX:
			if ( FSelDataColor <> Value ) then
			begin
				FSelDataColor := Value;
				Invalidate;
			end;
		SELDATA_BACK_COLOR_IDX:
			if ( FSelDataBackColor <> Value ) then
			begin
				FSelDataBackColor := Value;
				Invalidate;
			end;
	end;
end;

function TKCustomDump.GetDataColor( Index: Integer ): TColor;
begin
	case Index of
		SELDATA_COLOR_IDX:
		  Result := FSelDataColor;
		SELDATA_BACK_COLOR_IDX:
			Result := FSelDataBackColor;
	else
		Result := Self.Color;
	end;		
end;

procedure TKCustomDump.SetAddress( Value: Pointer );
begin
	FActive := CheckPointer( Value );
  FAddress := Value;
	Invalidate;
end;

procedure TKCustomDump.SetDataSize( Value: Integer );
begin
	FDataSize := Value;
	CalcPaintParams;
	Invalidate;
	AdjustScrollBars;
end;

procedure TKCustomDump.SetStrAddress( const Value: string );
begin
	if ( not ( ( Length( Value ) = DataSize ) or CompareMem( Address,
		Pointer( Value ), DataSize ) ) ) then
	begin
	  DataSize := Length( Value );
		if ( DataSize > 0 ) then
		  Address := Pointer( Value )
		else
		  Address := nil;
	end;
end;

function TKCustomDump.GetStrAddress: string;
begin
  Result := '';
	if ( CheckPointer( Address ) and ( DataSize > 0 ) ) then
	  SetString( Result, PChar( Address ), DataSize );
end;

procedure TKCustomDump.LoadFromStream( Stream: TStream );
begin
	ForceStream( Stream );
	Address := nil;
	DataSize := 0;
	FreeClean( FMemoryStream );
	FMemoryStream := TMemoryStream.Create;
	try
		Stream.Position := 0;
		ForceStreamCopy( Stream, FMemoryStream );
		Address := FMemoryStream.Memory;
		DataSize := FMemoryStream.Size;
	except
		FreeClean( FMemoryStream );
		raise;
	end;
end;

procedure TKCustomDump.SaveToTextStream( Stream: TStream; ABytesPerLine: Byte );
var
	i: Integer;
	bOldBytesPerLine: Byte;
begin
	ForceAddress;
	ForceObject( Stream );
	bOldBytesPerLine := FBytesPerLine;
	try
		if ( ABytesPerLine <> 0 ) then
			FBytesPerLine := ABytesPerLine
		else
			ABytesPerLine := FBytesPerLine;
		FPrinting := True;
		try
			FLineCount := CalcLineCount;
			for i := 0 to FLineCount - 2 do
				PrintLine( Stream, i, ABytesPerLine );
			{ Last Line... }
			FBytesPerLine := Byte( DataSize mod FBytesPerLine );
			PrintLine( Stream, FLineCount - 1, ABytesPerLine );
		finally
		  FPrinting := False;
		end;
	finally
		FBytesPerLine := bOldBytesPerLine;
		FLineCount := CalcLineCount;
	end;
end;

procedure TKCustomDump.LoadFromFile( const AFileName: string );
var
	fs: TFileStream;
begin
	ForceFile( AFileName );
	FFileName := AFileName;
	fs := TFileStream.Create( AFileName, fmOpenRead or fmShareDenyWrite );
	try
		LoadFromStream( fs );
	finally
		fs.Free;
	end;
end;

procedure TKCustomDump.SaveToTextFile( const AFileName: string; ABytesPerLine: Byte );
var
	fs: TFileStream;
begin
	ForceTrimStr( AFileName );
	ForceDeleteFile( AFileName );
	fs := TFileStream.Create( AFileName, fmCreate or fmShareDenyWrite );
	try
		SaveToTextStream( fs, ABytesPerLine );
	finally
		fs.Free;
	end;
end;

procedure TKCustomDump.SetItemMetrics;
begin
	if CheckObject( Parent ) then
	begin
		Canvas.Font := Font;
		FItemHeight := Canvas.TextHeight( 'A' ) + 2;
		FItemWidth := Canvas.TextWidth( 'D' ) + 1;
		CalcPaintParams;
		AdjustScrollBars;
	end;
end;

procedure TKCustomDump.InvalidateLineMarker;
begin
	{ NotYetImplemented; - should select currentline ? }
end;

procedure TKCustomDump.CMParentFontChanged( var Message: TMessage );
begin
	inherited;
	SetItemMetrics;
end;

procedure TKCustomDump.CMFontChanged( var Message: TMessage );
begin
	inherited;
	SetItemMetrics;
end;

procedure TKCustomDump.CMEnter;
begin
	inherited;
	InvalidateLineMarker;
end;

procedure TKCustomDump.CMExit;
begin
	inherited;
	InvalidateLineMarker;
end;

procedure TKCustomDump.WMSize( var Message: TWMSize );
begin
	inherited;
	CalcPaintParams;
	AdjustScrollBars;
end;

procedure TKCustomDump.WMGetDlgCode( var Message: TWMGetDlgCode );
begin
	Message.Result := DLGC_WANTARROWS;
end;

procedure TKCustomDump.WMVScroll( var Message: TWMVScroll );
var
	rt: TRect;
	NewTopLine,
	LinesMoved: Integer;
begin
	inherited;
	NewTopLine := FTopLine;
	case Message.ScrollCode of
		SB_LINEDOWN	 : Inc( NewTopLine );
		SB_LINEUP		 : Dec( NewTopLine );
		SB_PAGEDOWN	 : Inc( NewTopLine, FVisibleLines - 1 );
		SB_PAGEUP		 : Dec( NewTopLine, FVisibleLines - 1 );
		SB_THUMBPOSITION,
		SB_THUMBTRACK: NewTopLine := Message.Pos;
	end;
	if ( NewTopLine < 0 ) then
		NewTopLine := 0;
	if ( NewTopLine >= FLineCount ) then
		NewTopLine := FLineCount - 1;
	if ( NewTopLine <> FTopLine ) then
	begin
		LinesMoved := FTopLine - NewTopLine;
		FTopLine := NewTopLine;
		SetScrollPos( Handle, SB_VERT, FTopLine, True );
		if ( Abs( LinesMoved ) = 1 ) then
		begin
			rt := Bounds( 0, 0, ClientWidth, ClientHeight - FItemHeight );
			if ( LinesMoved = 1 ) then
				OffsetRect( rt, 0, FItemHeight );
			ScrollWindow( Handle, 0, ( FItemHeight * LinesMoved ), @rt, nil );
			if ( LinesMoved = -1 ) then
			begin
				rt.Top := ClientHeight - FItemHeight;
				rt.Bottom := ClientHeight;
			end
			else
			begin
				rt.Top := 0;
				rt.Bottom := FItemHeight;
			end;
			Windows.InvalidateRect( Handle, @rt, False );
		end
		else
			Invalidate;
	end;
end;

procedure TKCustomDump.CalcPaintParams;
begin
	if ( FItemHeight < 1 ) then
		Exit;
	FBytesPerLine := CalcBytesPerline;
	FVisibleLines := CalcVisibleLines;
	FLineCount := CalcLineCount;
end;

function TKCustomDump.CalcVisibleLines: Integer;
begin
	Result := ( ( ClientHeight div FItemHeight ) + 1 );
end;

function TKCustomDump.CalcLineCount: Integer;
begin
	Result := ( DataSize div FBytesPerLine );
	if Boolean( DataSize mod FBytesPerLine ) then
		Inc( Result );
end;

procedure TKCustomDump.AdjustScrollBars;
begin
	SetScrollRange( Handle, SB_VERT, 0, FLineCount - 1, True );
end;

function TKCustomDump.ScrollIntoView: Boolean;
begin
	Result := False;
	if ( FCurrentLine < FTopLine ) then
	begin
		Result := True;
		SetTopLine( FCurrentLine );
	end
	else if ( FCurrentLine >= ( FTopLine + FVisibleLines ) - 1 ) then
	begin
		SetTopLine( FCurrentLine - ( FVisibleLines - 2 ) );
		Result := True;
	end;
end;

procedure TKCustomDump.DoSearchAddress;
begin
	if Assigned( FOnSearchAddress ) then
		FOnSearchAddress( Self );
end;

function TKCustomDump.FindRawAddress( Token: Pointer; TokenSize, OffSet: Integer ): Integer;
begin
	ForceAddress;
	ForceOffSet( OffSet );
	Result := GoToNextTokenBinPosEx( Token, IncPtr( Address, OffSet ), TokenSize, ( DataSize - OffSet ) );
end;

function TKCustomDump.FindStrAddress( const Token: string; OffSet: Integer ): Integer;
begin
	ForceStr( Token );
	Result := FindRawAddress( Pointer( Token ), Length( Token ), OffSet );
end;

function TKCustomDump.SearchRawAddress( Token: Pointer; TokenSize: Integer; GoForward: Boolean ): Boolean;
const
	SEARCH_DIRECTION: array[Boolean] of ShortInt = ( -1, 1 );
var
	iOffSet,
	iPos: Integer;
begin
	iOffSet := Abs( ( ( CurrentLine + SEARCH_DIRECTION[GoForward] ) * FBytesPerLine ) -
		( BackLineOffSet * FBytesPerLine ) );
	if ( iOffSet = FBytesPerLine ) then
		iOffSet := 0;
	iPos := FindRawAddress( Token, TokenSize, iOffSet );
	Result := ( iPos <> -1 );
	if Result then
	begin
		FSearching := True;
		try
			Inc( iPos, iOffSet );
			iPos := ( iPos div FBytesPerLine );
			TopLine := Max( 1, iPos - ( FVisibleLines div 2 ) );
			CurrentLine := iPos;
			DoSearchAddress;
			Application.ProcessMessages;
		finally
			FSearching := False;
		end;
	end;
end;

function TKCustomDump.SearchStrAddress( const Token: string; GoForward: Boolean ): Boolean;
begin
	ForceStr( Token );
	Result := SearchRawAddress( Pointer( Token ), Length( Token ), GoForward );
end;

procedure TKCustomDump.KeyDown( var Key: Word; Shift: TShiftState );
begin
	inherited KeyDown( Key, Shift );
	if ( not FActive ) then
		Exit;
	case Key of
		VK_DOWN : CurrentLine := ( CurrentLine + 1 );
		VK_UP		: CurrentLine := ( CurrentLine - 1 );
		VK_NEXT : CurrentLine := ( CurrentLine + FVisibleLines );
		VK_PRIOR: CurrentLine := ( CurrentLine - FVisibleLines );
		VK_HOME : CurrentLine := 0;
		VK_END  : CurrentLine := ( FLineCount - 1 );
	end;
end;

procedure TKCustomDump.MouseDown( Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer );
begin
	inherited MouseDown( Button, Shift, X, Y );
	if ( not Focused ) then
		SetFocus;
	if ( ( Button = mbLeft ) and FActive ) then
		CurrentLine := ( FTopLine + ( Y div FItemHeight ) );
end;

procedure TKCustomDump.Paint;
var
	rt: TRect;
	i: Integer;
	bPaintSearch: Boolean;
begin
	inherited Paint;
	Canvas.Brush.Color := Self.Color;
	rt := Bounds( 1, 0, ClientWidth, FItemHeight );
	bPaintSearch := ( FSearching and ( CurrentLine = FTopLine ) );
	FPainting := True;
	try
		if ( not PreparePaint( bPaintSearch, rt ) ) then
			Exit;
		for i := 0 to FVisibleLines - 1 do
		begin
			rt.Left := 1;
			if ( i + FTopLine < FLineCount ) then
			begin
				bPaintSearch := ( FSearching and ( CurrentLine = ( i + FTopLine ) ) );
				if ( not ProcessPaint( bPaintSearch, rt, i ) ) then
					Exit;
			end
			else
				ExtTextOut( Canvas.Handle, rt.Left, rt.Top, ( ETO_OPAQUE or ETO_CLIPPED ), @rt, nil, 0, nil );
			OffsetRect( rt, 0, FItemHeight );
		end;
	finally
	  FPainting := False;
	end;
end;

{ TKHexDump }

constructor TKHexDump.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAddrColor := clBlack;
	FHexDataColor := clBlack;
	FAnsiCharColor := clBlack;
	FShowAddress := True;
	FShowCharacters := True;
end;

function TKHexDump.LineAddr( Index: Integer ): PChar;
begin
	ZeroMemory( @FLineAddr, SizeOf( FLineAddr ) );
	Result := StrFmt( FLineAddr, '%p:', [Pointer( PChar( Address ) + Index * BytesPerLine )] );
end;

function TKHexDump.LineData( Index: Integer ): PChar;

	procedure SetData( P: PChar );
	const
		HexDigits : array[0..MAX_HEXDUMP_DIGITS - 1] of Char = '0123456789ABCDEF';
	var
		i: Integer;
		B: Byte;
	begin
		for i := 0 to BytesPerLine - 1 do
		begin
			try
				B := Byte( P[i] );
				FHexData[i][0] := HexDigits[( B shr $04 )];
				FHexData[i][1] := HexDigits[( B and $0F )];
				FHexData[i][2] := CH_TAB;
			except
			{
				on EInvalidPointer do
				on EOutOfMemory do
				on EAccessViolation do
			}
				FHexData[i][0] := CH_SPACE;
				FHexData[i][1] := CH_SPACE;
				FHexData[i][2] := CH_NULL;
			end;
		end;
	end;

begin
	ZeroMemory( @FHexData, SizeOf( FHexData ) );
	SetData( PChar( Address ) + Index * BytesPerLine );
	Result := FHexData[0];
end;

function TKHexDump.LineChars( Index: Integer ): PChar;
begin
	Result := ( PChar( Address ) + Index * BytesPerLine );
end;

procedure TKHexDump.SetShowAddress( Value: Boolean );
begin
	if ( FShowAddress <> Value ) then
	begin
		FShowAddress := Value;
		Invalidate;
	end;
end;

procedure TKHexDump.SetShowCharacters( Value: Boolean );
begin
	if ( Value <> FShowCharacters ) then
	begin
		FShowCharacters := Value;
		Invalidate;
	end;
end;

procedure TKHexDump.SetDataColor( Index: Integer; Value: TColor );
begin
	case Index of
		ANSICHAR_COLOR_IDX:
			if ( FAnsiCharColor <> Value ) then
			begin
				FAnsiCharColor := Value;
				Invalidate;
			end;
		ADDR_COLOR_IDX:
			if ( FAddrColor <> Value ) then
			begin
				FAddrColor := Value;
				Invalidate;
			end;
		HEXDATA_COLOR_IDX:
			if ( FHexDataColor <> Value ) then
			begin
				FHexDataColor := Value;
				Invalidate;
			end;
	else
		inherited SetDataColor( Index, Value );
	end;
end;

function TKHexDump.GetDataColor( Index: Integer ): TColor;
begin
	case Index of
		ANSICHAR_COLOR_IDX:
			Result := FAnsiCharColor;
		ADDR_COLOR_IDX    :
			Result := FAddrColor;
		HEXDATA_COLOR_IDX :
			Result := FHexDataColor;
	else
		Result := inherited GetDataColor( Index );
	end;
end;

function TKHexDump.ForceHexAddress( const Token: string ): string;
begin
	Result := StrHexToStrBin( Token ); { if Token is not a valid HexAddr string, it will return '' }
	if ( not CheckTrimStr( Result ) ) then
		RaiseExceptionFmt( EKHexDump, sErrInvHexDumpHexAddr, [Token] );
end;

function TKHexDump.FindHexAddress( const Token: string; OffSet: Integer ): Integer;
var
	s: string;
begin
	s := ForceHexAddress( Token );
	Result := FindRawAddress( Pointer( s ), Length( s ), OffSet );
end;

function TKHexDump.SearchHexAddress( const Token: string; GoForward: Boolean ): Boolean;
var
	s: string;
begin
	s := ForceHexAddress( Token );
	Result := SearchRawAddress( Pointer( s ), Length( s ), GoForward );
end;

procedure TKHexDump.PrintLine( Stream: TStream; Line: Integer; ABytesPerLine: Byte );
const
	NUM_SIZE = ( 2 * SizeOf( Byte ) );
	SPACE_COUNT = NUM_SIZE;
var
	s,
	s1: string;
begin
	s := '';
	if FShowAddress then
		s := s + LineAddr( Line ) + CH_SPACE + CH_TAB;
	s := s + StringReplace( LineData( Line ), CH_TAB, CH_SPACE + CH_SPACE, krfAll );
	{ Is this the last line and the last line has less bytes than others ? }
	if ( ( ABytesPerLine - BytesPerLine ) <> 0 ) then
		s := s + StringOfChar( CH_SPACE, ( NUM_SIZE * SPACE_COUNT * ( ABytesPerLine - BytesPerLine ) ) );
	if FShowCharacters then
	begin
		SetString( s1, LineChars( Line ), BytesPerLine );
		s := s + CH_TAB + ReplaceCtrlChars( s1, CH_SPACE + CH_GRIDLING ) + CH_TAB;
	end                                
	else
		s := TrimRight( s );
	s := ( s + CH_CRLF );
	Stream.WriteBuffer( Pointer( s )^, Length( s ) );
end;

function TKHexDump.CalcBytesPerLine: Byte;
const
	DIVISOR: array[Boolean] of Integer = ( 3,4 );
var
	CharsPerLine: Integer;
begin
	CharsPerLine := ( ClientWidth div ItemWidth );
	if FShowAddress then
		Dec( CharsPerLine, 10 );
	Result := ( CharsPerLine div Divisor[FShowCharacters] );
	if ( Result > MAX_HEXDUMP_DIGITS ) then
		Result := MAX_HEXDUMP_DIGITS
	else if ( Result = 0 ) then
		Result := 1;
end;

function TKHexDump.PreparePaint( PaintSearch: Boolean; var rt: TRect ): Boolean;
begin
	if FShowAddress then
		FPaintAddrWidth := ( ItemWidth * 10 )
	else
		FPaintAddrWidth := 0;
	FPaintTbStop := ( ItemWidth * 3 );
	FPaintBtnPerLine := BytesPerLine;
	if ( not PaintSearch ) then
		Canvas.Font.Color := HexDataColor
	else
		Canvas.Font.Color := SelDataColor;
	Result := ( BytesPerLine > 0 );
end;

function TKHexDump.ProcessPaint( PaintSearch: Boolean; var rt: TRect; Line: Integer ): Boolean;
begin
	if ( not PaintSearch ) then
		Canvas.Brush.Color := Self.Color
	else
		Canvas.Brush.Color := SelDataBackColor;
	if FShowAddress then
	begin
		if ( not PaintSearch ) then
			Canvas.Font.Color := AddressColor
		else
			Canvas.Font.Color := SelDataColor;
		rt.Right := ( rt.Left + FPaintAddrWidth );
		ExtTextOut( Canvas.Handle, rt.Left, rt.Top, ( ETO_OPAQUE or ETO_CLIPPED ), @rt,
			LineAddr( Line + TopLine ), 9, nil );
		rt.Left := rt.Right;
		rt.Right := ClientWidth;
	end;
	if ( not PaintSearch ) then
		Canvas.Font.Color := HexDataColor
	else
		Canvas.Font.Color := SelDataColor;
	if ( Line + TopLine = LineCount - 1 ) and ( ( DataSize mod BytesPerLine ) > 0 ) then
		FPaintBtnPerLine := Byte( DataSize mod BytesPerLine );
	TabbedTextOut( Canvas.Handle, rt.Left, rt.Top, LineData( Line + TopLine ),
		( FPaintBtnPerLine * 3 ) - 1, 1, FPaintTbStop, rt.Left );
	if FShowCharacters then
	begin
		rt.Left := ( FPaintAddrWidth + ( ItemWidth * ( BytesPerLine * 3 ) ) );
		if ( not PaintSearch ) then
			Canvas.Font.Color := AnsiCharColor
		else
			Canvas.Font.Color := SelDataColor;
		ExtTextOut( Canvas.Handle, rt.Left, rt.Top, ( ETO_OPAQUE or ETO_CLIPPED ), @rt,
			LineChars( Line + TopLine ), FPaintBtnPerLine, nil );
	end;
	Result := True;
end;

{ TKTextDump }

constructor TKTextDump.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTextColor := clBlack;
	FWantTabs := True;
	FWordBreak := True;
	FWantReturns := True;
	FTextBlockLength := 80;
	FTabStopCount := 2;
	FTextAlignment := taLeftJustify;
end;

procedure TKTextDump.SetDataColor( Index: Integer; Value: TColor );
begin
	case Index of
		TEXT_COLOR_IDX:
			if ( FTextColor <> Value ) then
			begin
				FTextColor := Value;
				Invalidate;
			end;
	else
		inherited SetDataColor( Index, Value );
	end;
end;

function TKTextDump.GetDataColor( Index: Integer ): TColor;
begin
	case Index of
		TEXT_COLOR_IDX:
			Result := FTextColor;
	else
		Result := inherited GetDataColor( Index );
	end;
end;

function TKTextDump.LineText( Index: Integer ): PChar;
var
	i: Integer;
begin
	if ( not Active ) then
	begin
		Result := nil;
		Exit;
	end;
	i := PosEx( CH_CRLF, PChar( Address ), Index );
	if ( i > 0 ) then
	  Result := PChar( Address ) + i + Length( CH_CRLF )
	else
		Result := ( PChar( Address ) + Index * BytesPerLine );
	{ Adjust for tab chars }
	i := CountTokens( Copy( Result, 1, BytesPerLine ), CH_TAB );
	if ( i > 0 ) then
	begin
		Result := PChar( StringReplace( Result, CH_TAB, StringOfChar( CH_SPACE,
			TabStopCount ), krfAll ) );
		if Painting then
			Dec( FPaintBtnPerLine, ( ( TabStopCount * i ) - i ) );
	end;
end;

function TKTextDump.CalcVisibleLines: Integer;
var
	s: string;
begin
	Result := inherited CalcVisibleLines;
	SetString( s, LineText( TopLine + CurrentLine ), Result * BytesPerLine );
	Result := CountTokens( s, CH_CRLF );
end;

function TKTextDump.CalcLineCount: Integer;
begin
	Result := CountTokens( StrAddress, CH_CRLF );
	if ( Result <= 0 ) then
		Result := inherited CalcLineCount;
end;

function TKTextDump.CalcBytesPerLine: Byte;
var
	CharsPerLine: Integer;
begin
	CharsPerLine := ( ClientWidth div ItemWidth );
	if ValueBetween( CharsPerLine, Low( Byte ), High( Byte ) + 1, False ) then
		Result := Byte( CharsPerLine )
	else
		Result := 1;
end;

procedure TKTextDump.PrintLine( Stream: TStream; Line: Integer; ABytesPerLine: Byte );
var
	s,
	s1: string;
begin
	s := '';
	SetString( s1, LineText( Line ), BytesPerLine );
	s := ( ReplaceCtrlChars( s1, CH_SPACE + CH_GRIDLING ) + CH_CRLF );
	Stream.WriteBuffer( Pointer( s )^, Length( s ) );
end;

function TKTextDump.PreparePaint( PaintSearch: Boolean; var rt: TRect ): Boolean;
begin
	if ( not PaintSearch ) then
		Canvas.Font.Color := TextColor
	else
		Canvas.Font.Color := SelDataColor;
	FPaintBtnPerLine := BytesPerLine;
	Result := ( BytesPerLine > 0 );
end;

function TKTextDump.ProcessPaint( PaintSearch: Boolean; var rt: TRect; Line: Integer ): Boolean;
const
	WANT_TABS   : array[Boolean] of Integer    = ( 0, DT_EXPANDTABS );
	WORD_BREAK  : array[Boolean] of Integer    = ( 0, DT_WORDBREAK );
	WANT_RETURNS: array[Boolean] of Integer    = ( DT_SINGLELINE, 0 );
	TEXT_ALIGN  : array[TAlignment] of Integer = ( DT_LEFT, DT_RIGHT, DT_CENTER );

	DEFAULT_TABCHAR_SIZE = 8;

var
	pc: PChar;
begin
	if ( not PaintSearch ) then
		Canvas.Brush.Color := Self.Color
	else
		Canvas.Brush.Color := SelDataBackColor;
	if ( Line + TopLine = LineCount - 1 ) and ( ( DataSize mod BytesPerLine ) > 0 ) then
		FPaintBtnPerLine := Byte( DataSize mod BytesPerLine );
	pc := LineText( Line + TopLine );
	ExtTextOut( Canvas.Handle, rt.Left, rt.Top, ( ETO_OPAQUE or ETO_CLIPPED ), @rt, pc,
  	FPaintBtnPerLine, nil ); 
	Result := True;
(*
	{ Adjust FPaingBtnPerLine if WantTabs..., for each tab subtract 8 bytes }
	if WantTabs then
	begin
		i := CountTokens( s, CH_TAB );
		Dec( FPaintBtnPerLine, i * DEFAULT_TABCHAR_SIZE );
		FPaintBtnPerLine := Max( FPaintBtnPerLine, i ); { for tab chars only }
		if ( FPaintBtnPerLine <> Length( s ) ) then
			SetString( s, LineText( Line + TopLine ), FPaintBtnPerLine );
	end;
	DrawText( Canvas.Handle, PChar( s ), FPaintBtnPerLine, rt, DT_NOPREFIX or DT_NOCLIP or
		TEXT_ALIGN[TextAlignment] or { WORD_BREAK[WordBreak] or } WANT_RETURNS[WantReturns] or
		WANT_TABS[WantTabs] );
	{ Adjust the rectanble }
	if WantReturns then
		Inc( rt.Top, CountTokens( s, CH_CRLF ) * ItemHeight );
*)
end;

{
--------------------------------------------------------------------------------
-------------------------- Command Line Parser/Lexer ---------------------------
--------------------------------------------------------------------------------
}

{ TKCmdLineParsingFrame }

constructor TKCmdLineParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObjectClass( AParser, TKCmdLineParser );
  inherited Create( AParser );
  with ( AParser as TKCmdLineParser ) do
    Self.FCmdLineToken := FCmdLineToken;
end;

procedure TKCmdLineParsingFrame.RestoreFrame;
begin
  inherited RestoreFrame;
  with ( Parser as TKCmdLineParser ) do
    FCmdLineToken := Self.FCmdLineToken;
end;

{ TKCmdLineParser }

constructor TKCmdLineParser.Create( AStream: TStringStream );
begin
	FCmdLineToken := cltNone;
	inherited Create( AStream, False );
end;

function TKCmdLineParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKCmdLineParsingFrame;
end;

function TKCmdLineParser.CheckHexaToken( P: PChar ): Boolean;
begin
	Result := ( P^ = CH_HEXA_TOKEN );
end;

function TKCmdLineParser.CheckSpecialToken( P: PChar ): Boolean;
begin
	Result := ( P^ in CharSets[SPECIAL_CHARSET_IDX] );
end;

function TKCmdLineParser.GetStringCharSet: TKCharSet;
begin
	Result := [CH_DBLQUOTE];
end;

function TKCmdLineParser.GetSpecialCharSet: TKCharSet;
begin
	Result := [CH_SPACE, CH_TAB, CH_SLASH, CH_BRACES_OPEN, CH_BRACES_CLOSE, CH_COMMA];
end;

function TKCmdLineParser.GetIntegerCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_INTEGER_CHARSET; { '-', '0'..'9' }
end;

function TKCmdLineParser.GetFloatCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_FLOAT_CHARSET; { '-', '0'..'9', 'e', 'E', '+' }
end;

function TKCmdLineParser.GetBlankCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_BLANK_CHARSET - GetSpecialCharSet;
end;

function TKCmdLineParser.ProcessSymbol( var P: PChar ): TKTokenType;
begin
	repeat
		Inc( P );
	until ( P^ in ( CharSets[BLANK_CHARSET_IDX] + CharSets[STRING_CHARSET_IDX] +
		CharSets[SPECIAL_CHARSET_IDX] + [CH_HEXA_TOKEN] ) );
	Result := tpSymbol;
end;

function TKCmdLineParser.ProcessString( var P: PChar ): TKTokenType;
var
	S: PChar;
begin
	S := P;
	case P^ of
		CH_DBLQUOTE:
		begin
			Inc( P );
			while True do
			begin
				case P^ of
					CH_NULL, CH_LF, CH_CR:
						Error( sErrParseInvStr );
					CH_DBLQUOTE:
					begin
						Inc( P );
						if ( P^ <> CH_DBLQUOTE ) then
							Break;
					end;
				end;
				S^ := P^;
				Inc( S );
				Inc( P );
			end;
		end;
	else
		Error( sErrParseInvStr );
	end;
	Result := inherited ProcessString( S );
end;

function TKCmdLineParser.ProcessSpecialToken( var P: PChar ): TKTokenType;
begin
	FCmdLineToken := cltNone;
	while True do
		case P^ of
		  CH_TAB,
			CH_SPACE:
			begin
				Inc( P );
				if ( P^ = CH_MINUS_SIGN ) then
				begin
					FCmdLineToken := cltCommand;
					Inc( P );
					Break;
				end
				else if ( P^ in [CH_SPACE, CH_TAB] ) then
				begin
					repeat
						Inc( P );
					until ( not ( P^ in [CH_SPACE, CH_TAB] ) );
					Dec( P );
				end
				else if ( P^ = CH_NULL ) then
					Break;
			end;
			CH_COMMA:
			begin
				Inc( P );
				repeat
					Inc( P );
				until ( P^ in ( CharSets[BLANK_CHARSET_IDX] + CharSets[STRING_CHARSET_IDX] +
					CharSets[SYMBOL_CHARSET_IDX] + CharSets[SPECIAL_CHARSET_IDX] + [CH_HEXA_TOKEN, CH_NULL] -
					[CH_SPACE] ) );
				FCmdLineToken := cltMoreOptItem;
				Break;
			end;
			CH_SLASH:
			begin
				Inc( P );
				FCmdLineToken := cltCommand;
				Break;
			end;
			CH_BRACES_OPEN:
			begin
				Inc( P );
				FCmdLineToken := cltMoreOptOpen;
				Break;
			end;
			CH_BRACES_CLOSE:
			begin
				Inc( P );
				FCmdLineToken := cltMoreOptClose;
				Break;
			end;
		else
		begin
			TokenPtr := P;
			Result := InternalNextToken( P );
			Exit;{ Error( sErrInvSpecial ); }
		end;
		end;
	Result := inherited ProcessSpecialToken( P );
end;

function TKCmdLineParser.CheckCmdLine( ACmdLineToken: TKCmdLineToken ): Boolean;
begin
	Result := ( CheckToken( tpSpecial ) and ( CmdLineToken = ACmdLineToken ) );
end;

procedure TKCmdLineParser.ForceCmdLine( ACmdLineToken: TKCmdLineToken );
begin
	if ( not CheckCmdLine( ACmdLineToken ) ) then
		Error( sErrInvSpecial );
end;

{ TKCmdLineLexer }

constructor TKCmdLineLexer.CreateFromCmdLine( const ACmdLine: string );
begin
	ForceTrimStr( ACmdLine );
	Source := TStringStream.Create( ACmdLine );
	inherited Create( nil, nil );
	FCmds := TStringList.Create;
	FCurrentCmd := -1;
end;

constructor TKCmdLineLexer.CreateDefault;
begin
	CreateFromCmdLine( Trim( Windows.GetCommandLine ) );
end;

destructor TKCmdLineLexer.Destroy;
begin
	FCmds.Free;
	inherited Destroy;
	Source.Free;
end;

procedure TKCmdLineLexer.MatchCmdLine( ACmdLineToken: TKCmdLineToken );
begin
	Parser.ForceCmdLine( ACmdLineToken );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in ValidTokenTypes ) or DetectEOF );
end;

function TKCmdLineLexer.GetDefaultErrorClass: EKLexerClass;
begin
	Result := EKCmdLineLexer;
end;

function TKCmdLineLexer.GetStream: TStringStream;
begin
	Result := ( Source as TStringStream );
end;

function TKCmdLineLexer.GetParser: TKCmdLineParser;
begin
	Result := TKCmdLineParser( inherited GetParser );
end;

procedure TKCmdLineLexer.CreateParser;
begin
	inherited Parser := TKCmdLineParser.Create( Stream );
end;

procedure TKCmdLineLexer.Reset;
begin
	FCmds.Clear;
	FCurrentCmd := -1;
	FApplicationName := '';
  Stream.Position := 0;
{ We MUST call the inherited method to recreate the parser object }
	inherited Reset;
end;

function TKCmdLineLexer.DetectSwitch: Boolean;
begin
	Result := Parser.CheckCmdLine( cltCommand );
end;

function TKCmdLineLexer.GetCmdCount: Integer;
begin
	Result := FCmds.Count;
end;

function TKCmdLineLexer.GetCmdName( Value: Integer ): string;
begin
	Result := FCmds.Names[Value];
end;

function TKCmdLineLexer.GetCmdValue( const Value: string ): string;
begin
	if ( FCmds.IndexOfName( Value ) <> -1 ) then
		Result := FCmds.Values[Value]
	else
		Result := '';
end;

function TKCmdLineLexer.GetCmdValueByIndex( Value: Integer ): string;
begin
	Result := GetCmdValue( GetCmdName( Value ) ); { to avoid TStrings malfunctioning }
end;

procedure TKCmdLineLexer.Goal;
{
Goal -> ( AppName [Options] )
}
begin
	AppName;
	Options;
	if ( not DetectEOF ) then
		Error( sErrInvSwitch );
end;

procedure TKCmdLineLexer.Options;
{
Options -> ( Switches | AddValues )
				-> ( AddValues | Switches )
}
begin
  while ( AddValues and Switches and ( not DetectEOF ) ) do ;
end;

procedure TKCmdLineLexer.AppName;
{
AppName -> Ident
				-> String
}
begin
	if ( not ( StringDef or Ident ) ) then
		ErrorFmt( sErrInvAppName, [Parser.TokenString] );
	FApplicationName := Parser.TokenString;
	case Parser.Token of
		tpString: MatchString( Parser.TokenString );
		tpSymbol: MatchSymbol( Parser.TokenString );
	end;
end;

function TKCmdLineLexer.AddValues: Boolean;
{
Values -> CmdName...
}
begin
	Result := True;
	while ( Result and ( not ( DetectSwitch or DetectEOF ) ) ) do
		Result := CmdName;
{
	if ( not Result ) then
		Error( sErrInvSwitch );
}
end;

function TKCmdLineLexer.Switches: Boolean;
{
Switch -> ( ' '-Command | /Command )...
}
begin
	Result := True;
	while ( Result and DetectSwitch and ( not DetectEOF ) ) do
	begin
		MatchCmdLine( cltCommand );
		Result := Command;
	end;
{
	if ( not Result ) then
		Error( sErrInvSwitch );
}
end;

function TKCmdLineLexer.Command: Boolean;
{
Command -> CmdName [( '"' Opt '"' |  '[' MoreOpt ']' )]
}
begin
	Result := CmdName;
	if Result then
	begin
		if Parser.CheckCmdLine( cltMoreOptOpen ) then
			Result := MoreOpt
		else if Parser.CheckToken( tpString ) then
			Result := Opt;
	end;
end;

function TKCmdLineLexer.CmdName: Boolean;
{
CmdName -> Ident
				-> String
				-> Number
				-> '?'
}
begin
	Result := ( StringDef or Ident or Number or Parser.CheckToken( CH_QUESTIONMARK ) );
	if Result then
	begin
		FCurrentCmd := FCmds.Add( Parser.TokenString );
		case Parser.Token of
			tpString: MatchString( Parser.TokenString );
			tpSymbol: MatchSymbol( Parser.TokenString );
			tpInteger,
			tpHexa,
			tpFloat: MatchNumber( Parser.Token );
			CH_QUESTIONMARK: MatchToken( CH_QUESTIONMARK );	
		end;
	end;
end;

function TKCmdLineLexer.MoreOpt: Boolean;
{
MoreOpt -> Opt/','...
}
var
	s: string;
begin
	MatchCmdLine( cltMoreOptOpen );
	Result := Opt;
	while ( Result and Parser.CheckCmdLine( cltMoreOptItem ) ) do
	begin
		FCmds.Strings[FCurrentCmd] := ( FCmds.Strings[FCurrentCmd] + CH_COMMA );
		MatchCmdLine( cltMoreOptItem );
		Result := Opt;
	end;
	if Result then
	begin
		s := FCmds.Strings[FCurrentCmd];
		if ( AnsiLastChar( s ) = CH_COMMA ) then
		begin
			Delete( s, Length( s ), 1 );
			FCmds.Strings[FCurrentCmd] := s;
		end;
		MatchCmdLine( cltMoreOptClose );
	end;
end;

function TKCmdLineLexer.Opt: Boolean;
{
Opt -> Ident
		-> String
		-> Number
}
var
  s: string;
begin
	Result := ( DetectEOF or Ident or StringDef or Number ); { Opt is optional }
	if Result then
	begin
		s := Parser.TokenString;
		if ( Parser.Token = tpHexa ) then
		  s := IntToStr( HexToInt( s ) ); 
		if ( not CheckStr( FCmds.Values[FCmds.Names[FCurrentCmd]] ) ) then
			FCmds.Strings[FCurrentCmd] := ( FCmds.Strings[FCurrentCmd] + CH_EQUAL_TOKEN + s )
		else
			FCmds.Strings[FCurrentCmd] := ( FCmds.Strings[FCurrentCmd] + s );
		case Parser.Token of
			tpSymbol:
				MatchSymbol( s );
			tpString:
				MatchString( s );
			tpInteger, tpFloat, tpHexa:
				MatchNumber( Parser.Token );
		end;
	end;
end;

function TKCmdLineLexer.StringDef: Boolean;
{
String -> '"' <valid-string> '"'
}
begin
	Result := Parser.CheckToken( tpString );
end;

function TKCmdLineLexer.Number: Boolean;
{
Number -> <valid-intnumber>
			 -> <valid-floatnumber>
			 -> <valid-hexanumber>
}
begin
	Result := ( Parser.CheckToken( tpInteger ) or Parser.CheckToken( tpHexa ) or
		Parser.CheckToken( tpFloat ) );
end;

function TKCmdLineLexer.Ident: Boolean;
{
Ident -> <valid-identifer>
}
begin
	Result := Parser.CheckToken( tpSymbol ) and CheckStr( Parser.TokenString ) and
	  ( Parser.TokenString[1] in CHARSET_IDENTIFIER );
end;

{
--------------------------------------------------------------------------------
-------------------------- Math Parser/Lexer/Solver ----------------------------
--------------------------------------------------------------------------------
}

{ TKMathParsingFrame }

constructor TKMathParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObjectClass( AParser, TKMathParser );
  inherited Create( AParser );
  with ( AParser as TKMathParser ) do
  begin
    Self.FMathOp := FMathOp;
    Self.FMathToken := FMathToken;
  end;
end;

procedure TKMathParsingFrame.RestoreFrame;
begin
  inherited RestoreFrame;
  with ( Parser as TKMathParser ) do
  begin
    FMathOp := Self.FMathOp;
    FMathToken := Self.FMathToken;
  end;
end;

{ TKMathParser }

constructor TKMathParser.Create( AExpr: TStringStream );
begin
	FMathOp := moNone;
	FMathToken := mtNone;
	inherited Create( AExpr, False );
end;

function TKMathParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKMathParsingFrame;
end;

function TKMathParser.CheckSymbolToken( P: PChar ): Boolean;
begin
	Result := ( inherited CheckSymbolToken( P ) ) and ( not ( P^ in ( CHARSET_DIGIT + [CH_DOTMARK] ) ) ); 
end;

function TKMathParser.CheckStringToken( P: PChar ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckRelOpToken( P: PChar ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckCommentToken( P: PChar ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckHexaToken( P: PChar ): Boolean;
begin
	Result := ( P^ = CH_HEXA_TOKEN );
end;

function TKMathParser.CheckSpecialToken( P: PChar ): Boolean;
begin
	Result := ( P^ in CharSets[SPECIAL_CHARSET_IDX] );
end;

function TKMathParser.GetSymbolCharSet: TKCharSet;
begin
	Result := ( inherited GetSymbolCharSet + [CH_DOTMARK] );
end;

function TKMathParser.GetStringCharSet: TKCharSet;
begin
	Result := [];
end;

function TKMathParser.GetBlankCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_BLANK_CHARSET - GetSpecialCharSet;
end;

function TKMathParser.GetIntegerCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_INTEGER_CHARSET - [CH_PLUS_SIGN, CH_MINUS_SIGN]; { '0'..'9' }
end;

function TKMathParser.GetFloatCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_FLOAT_CHARSET - [CH_PLUS_SIGN, CH_MINUS_SIGN]; { '0'..'9', 'e', 'E', '.' }
end;

function TKMathParser.GetCommentCharSet: TKCharSet;
begin
	Result := [];
end;

function TKMathParser.GetRelOpCharSet: TKCharSet;
begin
	Result := [];
end;

function TKMathParser.GetSpecialCharSet: TKCharSet;
begin
	Result := [CH_LIST_TOKEN, CH_PARENTHESIS_OPEN, CH_BRACES_OPEN, CH_BRACKET_OPEN,
		CH_PARENTHESIS_CLOSE, CH_BRACES_CLOSE, CH_BRACKET_CLOSE, CH_COMMA];
end;

class function TKMathParser.GetMathOpCharSet: TKCharSet;
begin
	Result := [CH_PLUS_SIGN, CH_MINUS_SIGN, CH_MUL_SIGN, CH_DIV_SIGN, CH_CIRCUMFLEX,
		'a', 'A', 'd', 'D', 'm', 'M', 'n', 'N', 'o', 'O', 's', 'S', 't', 'T', 'x', 'X'];
end;

function TKMathParser.CheckMathOpToken( P: PChar ): Boolean;
begin
	Result := ( P^ in GetMathOpCharSet );
	if ( Result and ( P^ in ['a', 'A', 'd', 'D', 'm', 'M', 'n', 'N', 'o', 'O', 's', 'S', 't', 'T', 'x', 'X'] ) ) then
		case P^ of
			'd', 'D': Result := ( ( ( P + 1 )^ in ['i', 'I'] ) and ( ( P + 2 )^ in ['v', 'V'] ) );
 {    'n', 'N': Result := ( ( ( P + 1 )^ in ['o', 'o'] ) and ( ( P + 2 )^ in ['t', 'T'] ) ); }
			'm', 'M': Result := ( ( ( P + 1 )^ in ['o', 'O'] ) and ( ( P + 2 )^ in ['d', 'D'] ) );
			's', 'S': Result := ( ( ( P + 1 )^ in ['h', 'H'] ) and ( ( ( P + 2 )^ in ['l', 'L'] ) or ( ( P + 2 )^ in ['r', 'R'] ) ) );
			'a', 'A': Result := ( ( ( P + 1 )^ in ['n', 'N'] ) and ( ( P + 2 )^ in ['d', 'D'] ) );
			'x', 'X': Result := ( ( ( P + 1 )^ in ['o', 'O'] ) and ( ( P + 2 )^ in ['r', 'R'] ) );
			'o', 'O': Result := ( ( P + 1 )^ in ['r', 'R'] );
    else
      Result := False;  
		end;
end;

function TKMathParser.ProcessSpecialToken( var P: PChar ): TKTokenType;
begin
	FMathToken := mtNone;
	case P^ of
		CH_LIST_TOKEN       : FMathToken := mtEndOfExpression;
		CH_PARENTHESIS_OPEN : FMathToken := mtParentesisOpen;
		CH_BRACES_OPEN      : FMathToken := mtBracesOpen;
		CH_BRACKET_OPEN     : FMathToken := mtBracketsOpen;
		CH_PARENTHESIS_CLOSE: FMathToken := mtParentesisClose;
		CH_BRACES_CLOSE     : FMathToken := mtBracesClose;
		CH_BRACKET_CLOSE    : FMathToken := mtBracketsClose;
		CH_COMMA						: FMathToken := mtFuncExprListSep;
	else
		Error( sErrInvSpecial );
	end;
	Inc( P );
	Result := inherited ProcessSpecialToken( P );
end;

function TKMathParser.ProcessMathOpToken( var P: PChar ): TKTokenType;
begin
	FMathOp := moNone;
	case P^ of
		CH_PLUS_SIGN:
		begin
			Inc( P );
			FMathOp := moPlus;
		end;
		CH_MINUS_SIGN:
		begin
			Inc( P );
			FMathOp := moMinus;
		end;
		CH_MUL_SIGN:
		begin
			Inc( P );
			FMathOp := moMul;
		end;
		CH_DIV_SIGN:
		begin
			Inc( P );
			FMathOp := moDiv;
		end;
		CH_CIRCUMFLEX:
		begin
			Inc( P );
			FMathOp := moPower;
		end;
		'a', 'A':
		begin
			Inc( P, 3 );
			FMathOp := moAnd;
		end;
	  'o', 'O':
		begin
			Inc( P, 2 );
			FMathOp := moOr;
		end;
		'x', 'X':
		begin
			Inc( P, 3 );
			FMathOp := moXor;
		end;
		'd', 'D':
		begin
			Inc( P, 3 );
			FMathOp := moDivInt;
		end;
{
    'n', 'N':
    begin
      Inc( P, 3 );
      FMathOp := moNot;
    end;
}
		'm', 'M':
		begin
			Inc( P, 3 );
			FMathOp := moMod;
		end;
		's', 'S':
		begin
			Inc( P, 2 );
			case P^ of
				'l', 'L': FMathOp := moShl;
				'h', 'H': FMathOp := moShr;
			else
				Error( sErrInvMathOp );
			end;
			Inc( P );
		end;
	else
		Error( sErrInvMathOp );
	end;
	Result := tpMathOp;
end;

function TKMathParser.InternalNextToken( var P: PChar ): TKTokenType;
begin
	if CheckMathOpToken( P ) then
		Result := ProcessMathOpToken( P )
	else
		Result := inherited InternalNextToken( P );
end;

function TKMathParser.CheckString( const AString: string ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckSymbol( const Symbol: string ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckRelOp( ARelOp: TKRelOp ): Boolean;
begin
	Result := False;
end;

function TKMathParser.CheckMathOp( AMathOp: TKMathOp ): Boolean;
begin
	Result := ( ( Token = tpMathOp ) and ( MathOp = AMathOp ) );
end;

procedure TKMathParser.ForceMathOp( AMathOp: TKMathOp );
begin
	if ( not CheckMathOp( AMathOp ) ) then
		Error( sErrInvMathOp );
end;

function TKMathParser.CheckMathToken( AMathToken: TKMathToken ): Boolean;
begin
	Result := ( CheckToken( tpSpecial ) and ( MathToken = AMathToken ) );
end;

procedure TKMathParser.ForceMathToken( AMathToken: TKMathToken );
begin
	if ( not CheckMathToken( AMathToken ) ) then
		Error( sErrInvSpecial );
end;

function TKMathParser.CheckIdent( const Ident: string ): Boolean;
begin
	Result := ( CheckToken( tpSymbol ) and CheckStrEqual( Ident, TokenIdent ) );
end;

procedure TKMathParser.ForceIdent( const Ident: string );
begin
	if ( not CheckIdent( Ident ) ) then
		Error( sErrIdentExpected );
end;

function TKMathParser.TokenIdent: string;
begin
	if CheckToken( tpSymbol ) then
		Result := TokenString
	else
		Result := '';
end;

{ TKMathExpression }

constructor TKMathExpression.Create( AMathExpr: TKMathExpressions );
begin
	ForceObject( AMathExpr );
	inherited Create;
  FOwnData := False;
	FOwner := AMathExpr;
	FMathExprTermList := TList.Create;
	FMathExpressionKind := mekNormal;
end;

destructor TKMathExpression.Destroy;
begin
	Clear;
	FMathExprTermList.Free;
	inherited Destroy;
end;

procedure TKMathExpression.Clear;
begin
	while CheckList( FMathExprTermList ) do
	begin
		{
			Here is just to logical meaning, record alignment will ensure that either
			case will be valid!
		}
		case PKMathExprTerm( FMathExprTermList.Items[FMathExprTermList.Count - 1] )^.MathExprTermKind of
			mtkIdent: StrDispose( PKMathExprTerm( FMathExprTermList.Items[FMathExprTermList.Count - 1] )^.IdentName );
			mtkFunc : StrDispose( PKMathExprTerm( FMathExprTermList.Items[FMathExprTermList.Count - 1] )^.FuncName );
		end;
		Dispose( PKMathExprTerm( FMathExprTermList.Items[FMathExprTermList.Count - 1] ) );
		FMathExprTermList.Delete( FMathExprTermList.Count - 1 );
	end;
  if ( CheckPointer( Data ) and OwnData ) then
    DisposeData;
end;

procedure TKMathExpression.DisposeData;
begin
  Dispose( Data );
end;

procedure TKMathExpression.AddMathOp( AMathOp: TKMathOp );
var
	pmet: PKMathExprTerm;
begin
	pmet := New( PKMathExprTerm );
	try
		ZeroMemory( pmet, SizeOf( TKMathExprTerm ) );
		pmet^.MathExprTermKind := mtkMathOp;
		pmet^.MathOp := AMathOp;
	except
		Dispose( pmet );
		raise;
	end;
	FMathExprTermList.Add( pmet );
end;

procedure TKMathExpression.AddNumber( Parser: TKMathParser );
var
	pmet: PKMathExprTerm;
begin
	ForceObject( Parser );
	pmet := New( PKMathExprTerm );
	try
		ZeroMemory( pmet, SizeOf( TKMathExprTerm ) );
		pmet^.MathExprTermKind := mtkNumber;
		case Parser.Token of
			tpInteger: pmet^.nValue := Parser.TokenInt;
			tpHexa   : pmet^.nValue := Parser.TokenHexa;
			tpFloat  : pmet^.nValue := Parser.TokenFloat;
		else
			Owner.Owner.Error( sErrNumExpected );
		end;
	except
		Dispose( pmet );
		raise;
	end;
	FMathExprTermList.Add( pmet );
end;

procedure TKMathExpression.AddFuncExpr( FuncExprIdx: Integer );
var
	pmet: PKMathExprTerm;
begin
	pmet := New( PKMathExprTerm );
	try
		ZeroMemory( pmet, SizeOf( TKMathExprTerm ) );
		pmet^.MathExprTermKind := mtkFuncExpr;
		pmet^.ExprIdx := FuncExprIdx;
	except
		Dispose( pmet );
		raise;
	end;
	FMathExprTermList.Add( pmet );
end;

procedure TKMathExpression.AddIdent( const Ident: string );
var
	pmet: PKMathExprTerm;
begin
	ForceTrimStr( Ident );
	pmet := New( PKMathExprTerm );
	try
		ZeroMemory( pmet, SizeOf( TKMathExprTerm ) );
		pmet^.MathExprTermKind := mtkIdent;
		pmet^.IdentName := StrAllocMem( Length( Ident ) + 1 );
		StrLCopy( pmet^.IdentName, PChar( Ident ), Length( Ident ) );
	except
		if CheckPointer( pmet^.IdentName ) then
			StrDispose( pmet^.IdentName );
		Dispose( pmet );
		raise;
	end;
	FMathExprTermList.Add( pmet );
end;

procedure TKMathExpression.IdentToFunc( IdentId: Integer );
begin
	with PKMathExprTerm( FMathExprTermList.Items[GetMathTermsIndex( IdentId, mtkIdent )] )^ do
	begin
		fValue := 0;
		MathExprTermKind := mtkFunc;
	end;
end;

function TKMathExpression.GetMathExprTermCount: Integer;
begin
	Result := FMathExprTermList.Count;
end;

function TKMathExpression.GetCounters( GetIdx: Integer ): Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to FMathExprTermList.Count - 1 do
		if ( TKMathExprTermKind( GetIdx ) = PKMathExprTerm( FMathExprTermList.Items[i] )^.MathExprTermKind ) then
			Inc( Result );
end;

function TKMathExpression.GetMathTermsIndex( Index: Integer; GetIdx: TKMathExprTermKind ): Integer;
var
	i: Integer;
begin
	for i := 0 to FMathExprTermList.Count - 1 do
		if ( GetIdx = PKMathExprTerm( FMathExprTermList.Items[i] )^.MathExprTermKind ) then
		begin
			Dec( Index );
			if ( Index < 0 ) then
			begin
				Result := i;
				Exit;
			end;
		end;
	Result := -1;
end;

function TKMathExpression.GetMathTerms( Index, GetIdx: Integer ): TKMathExprTerm;
var
	i: Integer;
begin
	ZeroMemory( @Result, SizeOf( TKMathExprTerm ) );
	case GetIdx of
		0: Result := PKMathExprTerm( FMathExprTermList.Items[Index] )^;
	else
		if ( ( GetIdx - 1 ) <= Integer( High( TKMathExprTermKind ) ) ) then
		begin
			i := GetMathTermsIndex( Index, TKMathExprTermKind( GetIdx - 1 ) );
			if ( i <> -1 ) then
				Result := PKMathExprTerm( FMathExprTermList.Items[i] )^;
		end;
	end;
end;

function TKMathExpression.GetMathOp( Index: Integer ): TKMathOp;
begin
	Result := MathOpTerms[Index].MathOp;
end;

function TKMathExpression.GetNumber( Index: Integer ): Extended;
begin
	Result := NumberTerms[Index].nValue;
end;

function TKMathExpression.GetIdentName( Index: Integer ): string;
begin
	Result := IdentTerms[Index].IdentName;
end;

function TKMathExpression.GetIdentValue( Index: Integer ): Extended;
begin
	Result := IdentTerms[Index].iValue;
end;

procedure TKMathExpression.SetIdentValue( Index: Integer; Value: Extended );
begin
	PKMathExprTerm( FMathExprTermList.Items[GetMathTermsIndex( Index, mtkIdent )] )^.iValue := Value;
end;

function TKMathExpression.GetFuncName( Index: Integer ): string;
begin
	Result := FuncTerms[Index].FuncName;
end;

function TKMathExpression.GetFuncValue( Index: Integer ): Extended;
begin
	Result := FuncTerms[Index].iValue;
end;

procedure TKMathExpression.SetFuncValue( Index: Integer; Value: Extended );
begin
	PKMathExprTerm( FMathExprTermList.Items[GetMathTermsIndex( Index, mtkFunc )] )^.fValue := Value;
end;

function TKMathExpression.IndexOfIdentName( const AnIdentName: string ): Integer;
begin
	for Result := 0 to IdentCount - 1 do
		if CheckStrEqual( IdentName[Result], AnIdentName ) then
			Exit;
	Result := -1;
end;

function TKMathExpression.IndexOfFuncName( const AFuncName: string ): Integer;
begin
	for Result := 0 to FuncCount - 1 do
		if CheckStrEqual( FuncName[Result], AFuncName ) then
			Exit;
	Result := -1;
end;

function TKMathExpression.Solve: Extended;
var
	e: Extended;
	i,
	iCurFcnIdx,
	iMinusOp: Integer;
	mo: TKMathOp;
	es,
	efs: TKExtendedStack;
begin
	Result := 0;
	iCurFcnIdx := -1;
	iMinusOp := 0;
	es := TKExtendedStack.Create;
	try
		efs := nil;
		try
			for i := 0 to FMathExprTermList.Count - 1 do
			begin
				if ( ( MathExprTerms[i].MathExprTermKind <> mtkFuncExpr ) and CheckObject( efs ) ) then
				begin
					es.Push( Owner.DoSolveFunc( Index, iCurFcnIdx, efs ) );
					iCurFcnIdx := -1;
					FreeClean( efs );
				end;
				case MathExprTerms[i].MathExprTermKind of
					mtkMathOp  :
					begin
					  mo := MathExprTerms[i].MathOp;
						if ( es.Count <= 1 ) then
							case mo of
								moPlus : { do nothing... };
								moMinus: Inc( iMinusOp );
							else
								RaiseExceptionFmt( EKMathLexer, sErrInvMathOpKind, [EnumName( Cardinal(
									mo ), TypeInfo( TKMathOp ) )] );
							end
						{
							Need this test for the case of repeated math operators. (e.g.: 5*8++++++7)
							The list MUST have at least to operators!
						}
						else if ( es.Count > 1 ) then
						begin
							{ If there were Odd unary moMinus sign plus one binary moMinus, the
								correct operation is moPlus }
							if ( Odd( iMinusOp ) and ( mo = moMinus ) ) then
								mo := moPlus;
							{ iMinusOp := 0; }
							e := es.Pop;
							case mo of
								moPlus  : e := ( es.Pop + e );
								moMinus : e := ( es.Pop - e );
								moMul   : e := ( es.Pop * e );
								moDiv   : e := ( es.Pop / e );
								moMod   : e := ( Round( es.Pop ) mod Round( e ) );
								moDivInt: e := ( Round( es.Pop ) div Round( e ) );
								moPower : e := Power( es.Pop, e );
								moShl   : e := ( Round( es.Pop ) shl Round( e ) );
								moShr   : e := ( Round( es.Pop ) shr Round( e ) );
								moOr    : e := ( Round( es.Pop ) or  Round( e ) );
								moAnd   : e := ( Round( es.Pop ) and Round( e ) );
								moXor   : e := ( Round( es.Pop ) xor Round( e ) );
							else
								Owner.Owner.Error( sErrInvMathOp );
							end;
							es.Push( e );
						end;
					end;
					mtkNumber  : es.Push( MathExprTerms[i].nValue );
					mtkIdent   : es.Push( Owner.DoSolveIdent( Index, MathExprTerms[i].IdentName ) );
					mtkFunc    :
					begin
						iCurFcnIdx := i;
						efs := TKExtendedStack.Create;
					end;
					mtkFuncExpr:
					begin
						if ( not CheckObject( efs ) ) then
							Owner.Owner.ErrorFmt( sErrInvMathFuncExprSolve, [Owner.StrExprs[Index]] );
						efs.Push( Owner.Expressions[MathExprTerms[i].ExprIdx].Solve );
					end;
				end;
			end;
			{
				This solve the case of nested functions as expressions of other functions

				eg. (500 div 5 mod 8)+E( A( 5*8 ), B( 10 + 5 ), 5 + 8 ).
						(500 div 5 mod 8)+E( A( 5*8 ) + 50, B( 10 + 5 ), 5 + 8 ).

				This occured because nested functions could not have operators after their
				declarations (as pos-fixed expressions)!

				eg. 500 5 div 8 mod E( A( 5 8 * ), B( 10 5 + ), 5 8 + ) +
						500 5 div 8 mod E( A( 5 8 * ) 50 +, B( 10 5 + ), 5 8 + ) +

				The function A does not have operators in the first case, so it will enter here,
				any other case, the function will be evaluated as if it is a "normal" function.
			}
			if CheckObject( efs ) then
				es.Push( Owner.DoSolveFunc( Index, iCurFcnIdx, efs ) );
		finally
			efs.Free;
		end;
		if ( es.Count > 0 ) then
			Result := es.Pop;
		if Odd( iMinusOp ) then
			Result := -Result;
		Owner.DoSolve( Index, Result );
	finally
		es.Free;
	end;
end;

{ TKMathExpressions }

constructor TKMathExpressions.Create( AMathLexer: TKMathLexer );
begin
	ForceObject( AMathLexer );
	inherited Create;
	FOwner := AMathLexer;
	FDefFuncResult := 0;
	FDefIdentResult := 0;
	FExpressions := TStringList.Create;
end;

destructor TKMathExpressions.Destroy;
begin
	Clear;
	FExpressions.Free;
	inherited Destroy;
end;

function TKMathExpressions.GetExprIdx( Index: Integer; GetIdx: TKMathExprKind ): Integer;
var
	i: Integer;
begin
	for i := 0 to FExpressions.Count - 1 do
		if ( GetIdx = TKMathExpression( FExpressions.Objects[i] ).MathExpressionKind ) then
		begin
			Dec( Index );
			if ( Index < 0 ) then
			begin
				Result := i;
				Exit;
			end;
		end;
	Result := -1;	
end;

function TKMathExpressions.GetStrExpr( Index, GetIdx: Integer ): string;
var
	i: Integer;
begin
	Result := '';
	case GetIdx of
		0: Result := FExpressions.Strings[Index];
	else
		if ( ( GetIdx - 1 ) <= Integer( High( TKMathExprKind ) ) ) then
		begin
			i := GetExprIdx( Index, TKMathExprKind( GetIdx - 1 ) );
			if ( i <> -1 ) then
				Result := FExpressions.Strings[i];
		end;
	end;
end;

function TKMathExpressions.GetExpr( Index, GetIdx: Integer ): TKMathExpression;
var
	i: Integer;
begin
	Result := nil;
	case GetIdx of
		0: Result := TKMathExpression( FExpressions.Objects[Index] );
	else
		if ( ( GetIdx - 1 ) <= Integer( High( TKMathExprKind ) ) ) then
		begin
			i := GetExprIdx( Index, TKMathExprKind( GetIdx - 1 ) );
			if ( i <> -1 ) then
				Result := TKMathExpression( FExpressions.Objects[i] );
		end;
	end;
end;

function TKMathExpressions.GetExprCount( GetIdx: Integer ): Integer;
var
	i: Integer;
begin
	Result := 0;
	case GetIdx of
		0: Result := FExpressions.Count;
	else
		for i := 0 to FExpressions.Count - 1 do
			if ( TKMathExprKind( GetIdx - 1 ) = TKMathExpression( FExpressions.Objects[i] ).MathExpressionKind ) then
				Inc( Result );
	end;
end;

procedure TKMathExpressions.SetStrExpr( Index, GetIdx: Integer; const Value: string );
var
	i: Integer;
begin
	case GetIdx of
		0: FExpressions.Strings[Index] := Value;
	else
		if ( ( GetIdx - 1 ) <= Integer( High( TKMathExprKind ) ) ) then
		begin
			i := GetExprIdx( Index, TKMathExprKind( GetIdx - 1 ) );
			if ( i <> -1 ) then
				FExpressions.Strings[i] := Value;
		end;
	end;
end;

function TKMathExpressions.NewExpr: Integer;
begin
	Result := FExpressions.AddObject( '', TKMathExpression.Create( Self ) );
	Expressions[Result].FIndex := Result; { Hard Couple!!! }
end;

function TKMathExpressions.NewFuncExpr( ExprIdx: Integer ): Integer;
begin
	Result := NewExpr;
	Expressions[Result].FMathExpressionKind := mekFuncExpr; { Hard Couple!!! }
{
	For Function Expressions, we MUST append this information into the original
	function expression (i.e. the expression that the function are declared)
}
	Expressions[ExprIdx].AddFuncExpr( Expressions[Result].Index );
end;

procedure TKMathExpressions.Clear;
begin
	while CheckStrings( FExpressions ) do
	begin
		FExpressions.Objects[FExpressions.Count - 1].Free;
		FExpressions.Delete( FExpressions.Count - 1 );
	end;
end;

procedure TKMathExpressions.DoSolve( ExprIdx: Integer; const ReturnValue: Extended );
var
	me: TKMathExpression;
begin
	if Assigned( FOnSolve ) then
	begin
		me := Expressions[ExprIdx];
		FOnSolve( me, me.MathExpressionKind, StrExprs[ExprIdx], ReturnValue );
	end;
end;

function TKMathExpressions.DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended;
var
	me: TKMathExpression;
begin
	Result := DefIdentResult;
	me := Expressions[ExprIdx];
  if Assigned( FOnSolveIdent ) then
		FOnSolveIdent( me, IdentName, Result );
	me.IdentValue[me.IndexOfIdentName( IdentName )] := Result;
end;

function TKMathExpressions.DoSolveFunc( ExprIdx: Integer; MathTermIdx: Integer;
	FuncParams: TKExtendedStack ): Extended;
var
	sFcnName: string;
	me: TKMathExpression;
begin
	Result := DefFuncResult;
	me := Expressions[ExprIdx];
	sFcnName := me.MathExprTerms[MathTermIdx].FuncName;
	if Assigned( FOnSolveFunc ) then
		FOnSolveFunc( me, sFcnName, FuncParams, Result );
	me.FuncValue[me.IndexOfFuncName( sFcnName )] := Result;
end;

{ TKMathLexer }

constructor TKMathLexer.CreateFromExpression( const AExpr: string );
begin
	ForceTrimStr( AExpr );
	Source := TStringStream.Create( AExpr );
	CreateDefault;
end;

constructor TKMathLexer.CreateFromExpressions( AExprList: TStrings );
begin
	SetExpressions( AExprList, stStrings );
	CreateDefault;
end;

constructor TKMathLexer.CreateDefault;
begin
	FLastMathOp := moNone;
	FExprList := TKMathExpressions.Create( Self );
	inherited Create( nil, nil );
end;

destructor TKMathLexer.Destroy;
begin
	FExprList.Free;
	inherited Destroy;
	Source.Free;
end;

function TKMathLexer.GetValidTokenTypes: TKCharSet;
begin
	Result := ( inherited GetValidTokenTypes - TKMathParser.GetMathOpCharSet +
		[CH_PLUS_SIGN, CH_MINUS_SIGN] );
end;

procedure TKMathLexer.InternalSetExpression( const ExprList: string );
begin
	if CheckObject( Source ) then
	begin
		Source.Size := 0;
		Stream.WriteString( ExprList );
	end
	else
		Source := TStringStream.Create( ExprList );
end;

procedure TKMathLexer.SetExpression( const Expr: string );
begin
	ForceTrimStr( Expr );
	InternalSetExpression( Expr );
end;

procedure TKMathLexer.SetExpressions( ExprList: TStrings; Option: TKStringType );
var
	s: string;
	i: Integer;
begin
	ForceStrings( ExprList );
	s := '';
	for i := 0 to ExprList.Count - 1 do
		case Option of
			stStrings: s := s + ExprList[i] + CH_LIST_TOKEN;
			stNames  : s := s + ExprList.Names[i] + CH_LIST_TOKEN;
			stValues : s := s + ExprList.Values[ExprList.Names[i]] + CH_LIST_TOKEN;
		end;
	InternalSetExpression( s );	
end;

procedure TKMathLexer.MatchMathToken( AMathToken: TKMathToken );
begin
	Parser.ForceMathToken( AMathToken );
	Parser.MathToken := mtNone;
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in ValidTokenTypes ) or DetectEOF );
end;

procedure TKMathLexer.MatchMathOp( AMathOp: TKMathOp );
begin
	Parser.ForceMathOp( AMathOp );
	Parser.MathOp := moNone;
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in ValidTokenTypes ) or DetectEOF );
end;

procedure TKMathLexer.MatchIdent( const Ident: string );
begin
	Parser.ForceToken( tpSymbol );
	Parser.ForceIdent( Ident );
	repeat
		Parser.NextToken;
	until ( ( Parser.Token in ValidTokenTypes ) or DetectEOF );
end;

function TKMathLexer.GetDefaultErrorClass: EKLexerClass;
begin
	Result := EKMathLexer;
end;

function TKMathLexer.GetStream: TStringStream;
begin
	Result := ( Source as TStringStream );
end;

function TKMathLexer.GetParser: TKMathParser;
begin
	Result := TKMathParser( inherited GetParser );
end;

procedure TKMathLexer.CreateParser;
begin
	inherited Parser := TKMathParser.Create( Stream );
end;

procedure TKMathLexer.Reset;
begin
	FLastMathOp := moNone;
	FExprList.Clear;
	{ Reseting without a valid stream reference should happens if the lexer was
		created but early destroyed before it's real use! So do not propagate the
		reset process. }
	if CheckObject( Stream ) then
	begin
		Stream.Position := 0;
		inherited Reset;
	end;	       	
end;

procedure TKMathLexer.ProcessExpr( Action: ShortInt; ExprIdx: Integer );
const
	MATHOP_TO_STRING: array[TKMathOp] of string =
		(
			'', CH_PLUS_SIGN, CH_MINUS_SIGN, CH_MUL_SIGN, CH_DIV_SIGN, 'mod', 'div',
			CH_CIRCUMFLEX, 'shl', 'shr', 'or', 'and', 'xor' { , 'not' }
		);
begin
	with FExprList do
		case TKMathOp( Action ) of
			moPlus, moMinus, moMul, moDiv, moMod, moDivInt, moPower, moShl,
			moShr, moOr, moAnd, moXor:
			begin
				StrExprs[ExprIdx] := StrExprs[ExprIdx] +
					MATHOP_TO_STRING[TKMathOp( Action )] + CH_SPACE;
				Expressions[ExprIdx].AddMathOp( TKMathOp( Action ) );
			end;
		else
			case Action of
				eiNumber:
					if Parser.CheckNumber( Parser.TokenString ) then
					begin
						StrExprs[ExprIdx] := StrExprs[ExprIdx] + Parser.TokenString + CH_SPACE;
						Expressions[ExprIdx].AddNumber( Parser );
					end
					else
						Error( sErrNumExpected );
				eiIdent :
					if Parser.CheckIdent( Parser.TokenIdent ) then
					begin
						StrExprs[ExprIdx] := StrExprs[ExprIdx] + Parser.TokenString + CH_SPACE;
						Expressions[ExprIdx].AddIdent( Parser.TokenIdent );
					end
					else
						Error( sErrIdentExpected );
			else
				Error( sErrInvMathExpr );
			end;
		end;
end;

{ Grammar Definition }

procedure TKMathLexer.Goal;
{
Goal -> ( List )
}
begin
	List;
	if ( not DetectEOF ) then
		Error( sErrInvMathExpr );
end;

procedure TKMathLexer.List;
{
List -> ( Expr [( ';' List )] )
}
var
  iExpr: Integer;
	bMoreExpr: Boolean;
begin
	bMoreExpr := True;
	while ( bMoreExpr and ( not DetectEOF ) ) do
	begin
		iExpr := FExprList.NewExpr;
		Expr( iExpr );
		bMoreExpr := Parser.CheckMathToken( mtEndOfExpression );
		if bMoreExpr then
			MatchMathToken( mtEndOfExpression );
	end;
end;

procedure TKMathLexer.Expr( ExprIdx: Integer );
{
Expr -> [( Term MoreTerms )]
}
begin
	Term( ExprIdx );
	MoreTerms( ExprIdx );
end;

procedure TKMathLexer.Term( ExprIdx: Integer );
begin
	Factor( ExprIdx );
	MoreFactors( ExprIdx );
end;

procedure TKMathLexer.MoreTerms( ExprIdx: Integer );
{
MoreTerms -> [( AddOp Term MoreTerms )]
}
var
	mo: TKMathOp;
begin
	if ( ( Parser.MathOp in [moPlus, moMinus, moOr, moXor] ) and
		 ( not ( Parser.MathToken = mtEndOfExpression ) ) ) then
	begin
		mo := Parser.MathOp;
		FLastMathOp := mo;
		MatchMathOp( mo );
		Term( ExprIdx );
		ProcessExpr( ShortInt( mo ), ExprIdx );
		MoreTerms( ExprIdx );
	end;
{ else
		Error( sErrInvTerm ); , can be null []... }
end;

procedure TKMathLexer.MoreFactors( ExprIdx: Integer );
{
MoreFactors -> [( MulOp Factor MoreFactors )]
}
var
	mo: TKMathOp;
begin
	if ( ( Parser.MathOp in [moMul, moDiv, moMod, moDivInt, moPower, moShl, moShr, moAnd] ) and
		 ( not ( Parser.MathToken = mtEndOfExpression ) ) ) then
	begin
		mo := Parser.MathOp;
		FLastMathOp := mo;
		MatchMathOp( mo );
		Factor( ExprIdx );
		ProcessExpr( ShortInt( mo ), ExprIdx );
		MoreFactors( ExprIdx );
	end;
{ else
		Error( sErrInvFactor ); , can be null []... }
end;

procedure TKMathLexer.Factor( ExprIdx: Integer );
{
Factor -> ( ExprSepOpen Expr ExprSepClose ) | Number | Func | Ident |
}
begin
	if ( not ( Parser.MathToken = mtEndOfExpression ) ) then
		if ( Parser.MathToken in [mtParentesisOpen, mtBracesOpen, mtBracketsOpen] ) then
		begin
			MatchMathToken( Parser.MathToken );
			Expr( ExprIdx );
			if ( not ( Parser.MathToken in [mtParentesisClose, mtBracesClose, mtBracketsClose] ) ) then
				MatchMathToken( mtNone );
			MatchMathToken( Parser.MathToken );
		end
		else if Number then
		begin
			if ( ( FLastMathOp in [moMod, moDivInt, moShl, moShr, moAnd] ) and Parser.CheckToken( tpFloat ) ) then
				Error( sErrInvFloatOp );
			ProcessExpr( eiNumber, ExprIdx );
			MatchNumber( Parser.Token );
		end
		else if Ident then
		begin
			ProcessExpr( eiIdent, ExprIdx );
			MatchIdent( Parser.TokenIdent );
			if ( Parser.MathToken = mtParentesisOpen ) then
				Func( ExprIdx );
		end;
	{ else
			Error( sErrInvFactor ); , can be null []... }
end;

procedure TKMathLexer.Func( ExprIdx: Integer );
{
Func -> ( PreDefFunc | UserFunc )

PreDefFunc -> FuncIdent '(' FuncExpr ')'

UserFunc -> FuncIdent* '(' FuncExpr ')'
}
var
	sFncExprList: string;
begin
{
	The function identifer are consumed at Factor procedure for sematical otimization.
	So we need to adjust some informations....
}
	MatchMathToken( mtParentesisOpen );
	FExprList.StrExprs[ExprIdx] := ( Trim( FExprList.StrExprs[ExprIdx] ) + CH_PARENTHESIS_OPEN + CH_SPACE );
	FExprList.Expressions[ExprIdx].IdentToFunc( FExprList.Expressions[ExprIdx].IdentCount - 1 );
	sFncExprList := '';
	FuncExpr( ExprIdx, sFncExprList );
	MatchMathToken( mtParentesisClose );
	FExprList.StrExprs[ExprIdx] := ( FExprList.StrExprs[ExprIdx] + Trim( sFncExprList ) +
		CH_SPACE + CH_PARENTHESIS_CLOSE + CH_SPACE );
end;

procedure TKMathLexer.FuncExpr( ExprIdx: Integer; var FcnExprList: string );
{
FuncExpr -> ( [Expr [',' Expr]] )$
}
var
	iExpr: Integer;
begin
{
	This is the most complex function: it solves many problemas and can be called
	recursivelly. So be careful in reading it....
}
	iExpr := FExprList.NewFuncExpr( ExprIdx );
	Expr( iExpr );
	FcnExprList := Trim( FExprList.StrExprs[iExpr] );
	while Parser.CheckMathToken( mtFuncExprListSep ) do
	begin
		MatchMathToken( mtFuncExprListSep );
		FcnExprList := ( FcnExprList + CH_COMMA + CH_SPACE );
		iExpr := FExprList.NewFuncExpr( ExprIdx );
		Expr( iExpr );
		FcnExprList := Trim( FcnExprList + FExprList.StrExprs[iExpr] );
	end;
end;

function TKMathLexer.Number: Boolean;
{
Number -> <valid-intnumber>
			 -> <valid-floatnumber>
			 -> <valid-hexanumber>
}
begin
	Result := ( Parser.CheckToken( tpInteger ) or Parser.CheckToken( tpHexa ) or
		Parser.CheckToken( tpFloat ) );
end;

function TKMathLexer.Ident: Boolean;
{
Ident  -> <valid-identifer>
}
begin
	Result := Parser.CheckToken( tpSymbol );
end;

{ TKMathExprRegTerm }

constructor TKMathExprRegTerm.Create( AGroupID: ShortInt; const AComment: string );
begin
{
  if ( AGroupID = DEFAULT_GROUP_ID ) then  Property Editor will create a group of this kind
    Free;
}    
  inherited Create;
  FGroupID := AGroupID;
  FComment := AComment;
end;

{ TKMathExprRegGroup }

constructor TKMathExprRegGroup.Create( AGroupID: ShortInt; const AGroupName, AComment: string );
begin
  ForceTrimStr( AGroupName );
  inherited Create( AGroupID, AComment );
  FGroupName := AGroupName;
end;

{ TKMathExprRegFunc }

constructor TKMathExprRegFunc.Create( AGroupID: ShortInt; const AFuncName, AFormula, AComment, AParamNames: string;
  AParamCount: Integer; AUserFunc: TKMathExprUserFunc );
begin
  ForceTrimStr( AFuncName );
  ForceReference( AUserFunc );
  inherited Create( AGroupID, AComment );
  FParamNames := nil;
  if ( AParamCount > 0 ) or ( AParamCount = OPEN_ARRAY_PARAMS ) then
  begin
    FParamNames := TStringList.Create;
    ExtractStrings( AParamNames, CH_LIST_TOKEN, FParamNames );
    if ( ( AParamCount = OPEN_ARRAY_PARAMS ) and ( not CheckStrings( FParamNames ) ) ) then
      RaiseExceptionFmt( EKMathSolver, sErrInvMathExprRegFuncOpenParams, [AFuncName] )
    else if ( ( AParamCount > 0 ) and ( FParamNames.Count <> AParamCount ) ) then
      RaiseExceptionFmt( EKMathSolver, sErrInvMathExprRegFuncParamCnt, [AFuncName,
        AParamCount, FParamNames.Count] );
  end;
  FFuncName := AFuncName;
  FFormula := AFormula;
  FParamCount := AParamCount;
  FUserFunc := AUserFunc;
end;

destructor TKMathExprRegFunc.Destroy;
begin
  FreeClean( FParamNames );
  inherited Destroy;
end;

function TKMathExprRegFunc.GetParamNames( Index: Integer ): string;
begin
  if CheckObject( FParamNames ) then
    Result := FParamNames[Index]
  else
    Result := '';
end;

{ TKMathExprRegIdent }

constructor TKMathExprRegIdent.Create( AGroupID: ShortInt; const AIdentName,
  AComment: string; AIdentValue: Extended );
begin
  ForceTrimStr( AIdentName );
  inherited Create( AGroupID, AComment );
  FIdentName := AIdentName;
  FIdentValue := AIdentValue;
end;

{ TKMathExprTermStrings }

destructor TKMathExprTermStrings.Destroy;
begin
  Clear;
	inherited Destroy;
end;

procedure TKMathExprTermStrings.Clear;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Objects[i].Free;
  inherited Clear;
end;

procedure TKMathExprTermStrings.Delete( Index: Integer );
begin
  Objects[Index].Free;
  inherited Delete( Index );
end;

{ TKCustomMathSolver }

var
  FRegGroup: TKMathExprTermStrings = nil;
	FRegFunc: TKMathExprTermStrings = nil;
	FRegIdent: TKMathExprTermStrings = nil;

constructor TKCustomMathSolver.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FActive := False;
  FSolvingAll := False;
	FAutoReOpen := True;
	FDefFuncResult := 0;
	FDefIdentResult := 0;
	FSolverExprKind := msekAll;
	FExpressionType := stStrings;
  CreateIdentifiers;
  CreateExpressions;
  TStringList( FIdentifiers ).OnChange := IdentifiersChanged;
  TStringList( FExpressions ).OnChange := ExpressionsChanged;
	FMathLexer := TKMathLexer.CreateDefault;
	FMathLexer.ExprList.OnSolve := SolveEvent;
	FMathLexer.ExprList.OnSolveIdent := SolveIdentEvent;
	FMathLexer.ExprList.OnSolveFunc := SolveFuncEvent;
  RegisterDefaultGroups;
	RegisterDefaultFunctions;
	RegisterDefaultIdentifiers;
end;

destructor TKCustomMathSolver.Destroy;
begin
	Close;
  FIdentifiers.Clear;
	FExpressions.Clear;
	FreeClean( FIdentifiers );
	FreeClean( FExpressions );
	FreeClean( FMathLexer );
  FreeClean( FSolveStrMathLexer );
	inherited Destroy;
end;

class function TKCustomMathSolver.GetRegGroupCount: Integer;
begin
	Result := FRegGroup.Count;
end;

class function TKCustomMathSolver.IndexRegGroupOfGroupName( GroupID: ShortInt; const GroupName: string ): Integer;
begin
	Result := FRegGroup.IndexOf( IntToStr( GroupID ) + CH_EQUAL_TOKEN + GroupName );
end;

class function TKCustomMathSolver.GroupRegistered( GroupID: ShortInt; const GroupName: string ): Boolean;
begin
	Result := ( IndexRegGroupOfGroupName( GroupID, GroupName ) <> -1 );
end;

class function TKCustomMathSolver.GroupIDRegistered( GroupID: ShortInt ): Boolean;
begin
	Result := ( FRegGroup.IndexOfName( IntToStr( GroupID ) ) <> -1 );
end;

class function TKCustomMathSolver.GetRegGroups( Index: Integer ): TKMathExprRegGroup;
begin
  Result := TKMathExprRegGroup( FRegGroup.Objects[Index] );
end;

class function TKCustomMathSolver.RegisterGroup( GroupID: ShortInt; const GroupName, Comment: string ): Boolean;
begin
	Result := ( CheckTrimStr( GroupName ) and ( GroupID <> DEFAULT_GROUP_ID ) and
    ( not GroupRegistered( GroupID, GroupName ) ) );
	if Result then
		FRegGroup.AddObject( IntToStr( GroupID ) + CH_EQUAL_TOKEN + GroupName, TKMathExprRegGroup.Create( GroupID, GroupName,
      GetFirstString( [Comment, sDefGroupComment] ) ) );
end;

class function TKCustomMathSolver.UnRegisterGroup( GroupID: ShortInt;
  const GroupName: string ): Boolean;
var
	i: Integer;
begin
	Result := ( CheckTrimStr( GroupName ) and GroupRegistered( GroupID, GroupName ) ) ;
	if Result then
	begin
		i := IndexRegGroupOfGroupName( GroupID, GroupName );
    Result := ( i <> -1 );
    if Result then
    begin
		  FRegGroup.Objects[i].Free;
  		FRegGroup.Delete( i );
    end;
	end;
end;

class function TKCustomMathSolver.GetRegFunctionCount: Integer;
begin
	Result := FRegFunc.Count;
end;

class function TKCustomMathSolver.IndexRegFuncOfFuncName( const FuncName: string ): Integer;
begin
	Result := FRegFunc.IndexOf( FuncName )
end;

class function TKCustomMathSolver.FunctionRegistered( const FuncName: string ): Boolean;
begin
	Result := ( IndexRegFuncOfFuncName( FuncName ) <> -1 );
end;

class function TKCustomMathSolver.GetRegFuncs( Index: Integer ): TKMathExprRegFunc;
begin
  Result := TKMathExprRegFunc( FRegFunc.Objects[Index] );
end;

class function TKCustomMathSolver.RegisterFunction( GroupID: ShortInt; const FuncName,
  Formula, Comment, ParamNames: string; ParamCount: Integer; UserFunc: TKMathExprUserFunc ): Boolean;
begin
	Result := ( IsValidIdent( FuncName ) and Assigned( UserFunc ) and ( not FunctionRegistered( FuncName ) ) );
	if Result then
  begin
    if ( not GroupIDRegistered( GroupID ) ) then
      GroupID := DEFAULT_GROUP_ID;
		FRegFunc.AddObject( FuncName, TKMathExprRegFunc.Create( GroupID, FuncName,
      GetFirstString( [Formula, sDefFuncFormula] ), GetFirstString( [Comment,
      sDefFuncComment] ), ParamNames, ParamCount, UserFunc ) );
  end;    
end;

class function TKCustomMathSolver.UnRegisterFunction( const FuncName: string ): Boolean;
var
	i: Integer;
begin
	Result := ( IsValidIdent( FuncName ) and FunctionRegistered( FuncName ) );
	if Result then
	begin
		i := IndexRegFuncOfFuncName( FuncName );
    Result := ( i <> -1 );
    if Result then
    begin
		  FRegFunc.Objects[i].Free;
  		FRegFunc.Delete( i );
    end;
	end;
end;

class function TKCustomMathSolver.IndexRegIdentOfIdentName( const IdentName: string ): Integer;
begin
	Result := FRegIdent.IndexOf( IdentName );
end;

class function TKCustomMathSolver.IdentRegistered( const IdentName: string ): Boolean;
begin
	Result := ( IndexRegIdentOfIdentName( IdentName ) <> -1 );
end;

class function TKCustomMathSolver.GetRegIdentCount: Integer;
begin
  Result := FRegIdent.Count;
end;

class function TKCustomMathSolver.GetRegIdents( Index: Integer ): TKMathExprRegIdent;
begin
  Result := TKMathExprRegIdent( FRegIdent.Objects[Index] );
end;

class function TKCustomMathSolver.RegisterIdent( GroupID: ShortInt; const IdentName,
  Comment: string; IdentValue: Extended ): Boolean;
begin
	Result := ( IsValidIdent( IdentName ) and ( not IdentRegistered( IdentName ) ) );
	if Result then
  begin
    if ( not GroupIDRegistered( GroupID ) ) then
      GroupID := DEFAULT_GROUP_ID;
		FRegIdent.AddObject( IdentName, TKMathExprRegIdent.Create( GroupID, IdentName,
      GetFirstString( [Comment, sDefIdentComment] ), IdentValue ) );
  end;
end;

class function TKCustomMathSolver.UnRegisterIdent( const IdentName: string ): Boolean;
var
	i: Integer;
begin
	Result := ( IsValidIdent( IdentName ) and IdentRegistered( IdentName ) );
	if Result then
	begin
		i := IndexRegIdentOfIdentName( IdentName );
    Result := ( i <> -1 );
    if Result then
    begin
  		FRegIdent.Objects[i].Free;
  		FRegIdent.Delete( i );
    end;
	end;
end;

class function TKCustomMathSolver.EnumRegInfos( RegInfoType: TKMathExprRegType;
  Data: Pointer; CallBack: TKMathExprRegTermCallBack ): Integer;
var
  i: Integer;
  sl: TStrings;
begin
  Result := 0;
  case RegInfoType of
    mertGroup: sl := FRegGroup;
    mertFunc : sl := FRegFunc;
    mertIdent: sl := FRegIdent;
  else
    sl := nil;  
  end;
  if ( not CheckReference( CallBack ) ) then
    Result := sl.Count
  else
    for i := 0 to sl.Count - 1 do
    begin
      if ( not CallBack( i, RegInfoType, TKMathExprRegTerm( sl.Objects[i] ), Data ) ) then
        Exit;
      Inc( Result );
    end;
end;

function TKCustomMathSolver.DefaultFunctionHandler( Sender: TKMathExpression;
	const FuncName: string; FuncParams: TKExtendedStack ): Extended;

	function GetDefaultMathFunc( const FuncName: string ): TKDefaultMathFunc;
	var
		i: TKDefaultMathFunc;
	begin
		Result := Low( TKDefaultMathFunc ); { Foo Value to avoid warning }
		for i := Low( TKDefaultMathFunc ) to High( TKDefaultMathFunc ) do
			if CheckStrEqual( FuncName, REG_FUNC_INFOS[i].Name ) then
			begin
				Result := i;
				Exit;
			end;
    RaiseExceptionFmt( EKMathSolver, sErrInvDefFunc, [FuncName] );
	end;

var
	i,
	j: Integer;
	{$IFDEF DELPHI4}
	k: Int64;
	{$ENDIF}
	e,
	e2: Extended;
	pda: PDoubleArray;
	dmf: TKDefaultMathFunc;
begin
	Result := 0;
	dmf := GetDefaultMathFunc( FuncName );
	case dmf of
		dmfSin  : Result := Sin( FuncParams.Pop );
		dmfCos  : Result := Tan( FuncParams.Pop );
		dmfTan  : Result := Cos( FuncParams.Pop );
		dmfCoTan: Result := CoTan( FuncParams.Pop );

		dmfArcSin : Result := ArcSin( FuncParams.Pop );
		dmfArcCos : Result := ArcCos( FuncParams.Pop );
		dmfArcTan : Result := ArcTan( FuncParams.Pop );
		dmfArcTan2:
		begin
			e := FuncParams.Pop;
			Result := ArcTan2( FuncParams.Pop, e );
		end;

		dmfSinh: Result := Sinh( FuncParams.Pop );
		dmfCosh: Result := Cosh( FuncParams.Pop );
		dmfTanh: Result := Tanh( FuncParams.Pop );

		dmfArcSinh: Result := Sinh( FuncParams.Pop );
		dmfArcCosh: Result := ArcCosh( FuncParams.Pop );
		dmfArcTanh: Result := ArcTanh( FuncParams.Pop );

		dmfDegToRad  : Result := DegToRad( FuncParams.Pop );
		dmfRadToDeg  : Result := RadToDeg( FuncParams.Pop );
		dmfGradToRad : Result := GradToRad( FuncParams.Pop );
		dmfRadToGrad : Result := RadToGrad( FuncParams.Pop );
		dmfCycleToRad: Result := CycleToRad( FuncParams.Pop );
		dmfRadToCycle: Result := RadToCycle( FuncParams.Pop );

		dmfAbs   : Result := Abs( FuncParams.Pop );
		dmfInt   : Result := Int( FuncParams.Pop );
		dmfFrac  : Result := Frac( FuncParams.Pop );
		dmfTrunc : Result := Trunc( FuncParams.Pop );
		dmfRound : Result := Round( FuncParams.Pop );
		dmfSqr   : Result := Sqr( FuncParams.Pop );
		dmfSqrt  : Result := Sqrt( FuncParams.Pop );
		dmfExp   : Result := Exp( FuncParams.Pop );
		dmfLn    : Result := Ln( FuncParams.Pop );
		dmfInc   :
		begin
			i := Round( FuncParams.Pop );
			j := Round( FuncParams.Pop );
			Inc( j, i );
			Result := j;
		end;
		dmfDec   :
		begin
			i := Round( FuncParams.Pop );
			j := Round( FuncParams.Pop );
			Dec( j, i );
			Result := j;
		end;

{$IFDEF DELPHI4}
{ Here we need a this special implementation because of the C4019 internal error
	when directly assign a Int64Rec( x ).Hi/Lo as an extended result value }
		dmfHiDWord, dmfLoDWord, dmfHiHiWord, dmfHiLoWord, dmfLoHiWord, dmfLoLoWord,
		dmfHiHiHiByte, dmfHiHiLoByte, dmfHiLoHiByte, dmfHiLoLoByte, dmfLoHiHiByte,
		dmfLoHiLoByte, dmfLoLoHiByte, dmfLoLoLoByte:
		begin
			k := Round( FuncParams.Pop );
			case dmf of
				dmfHiDWord    : Result := Int64Rec( k ).Hi;
				dmfLoDWord    : Result := Int64Rec( k ).Lo;
				dmfHiHiWord   : Result := LongRec( Int64Rec( k ).Hi ).Hi;
				dmfHiLoWord   : Result := LongRec( Int64Rec( k ).Hi ).Lo;
				dmfLoHiWord   : Result := LongRec( Int64Rec( k ).Lo ).Hi;
				dmfLoLoWord   : Result := LongRec( Int64Rec( k ).Lo ).Lo;
				dmfHiHiHiByte : Result := WordRec( LongRec( Int64Rec( k ).Hi ).Hi ).Hi;
				dmfHiHiLoByte : Result := WordRec( LongRec( Int64Rec( k ).Hi ).Hi ).Lo;
				dmfHiLoHiByte : Result := WordRec( LongRec( Int64Rec( k ).Hi ).Lo ).Hi;
				dmfHiLoLoByte : Result := WordRec( LongRec( Int64Rec( k ).Hi ).Lo ).Lo;
				dmfLoHiHiByte : Result := WordRec( LongRec( Int64Rec( k ).Lo ).Hi ).Hi;
				dmfLoHiLoByte : Result := WordRec( LongRec( Int64Rec( k ).Lo ).Hi ).Lo;
				dmfLoLoHiByte : Result := WordRec( LongRec( Int64Rec( k ).Lo ).Lo ).Hi;
				dmfLoLoLoByte : Result := WordRec( LongRec( Int64Rec( k ).Lo ).Lo ).Lo;
			end;
		end;
{$ENDIF}
		dmfHiWord  : Result := LongRec( Integer( Round( FuncParams.Pop ) ) ).Hi;
		dmfLoWord  : Result := LongRec( Integer( Round( FuncParams.Pop ) ) ).Lo;
		dmfHiHiByte: Result := WordRec( LongRec( Integer( Round( FuncParams.Pop ) ) ).Hi ).Hi;
		dmfHiLoByte: Result := WordRec( LongRec( Integer( Round( FuncParams.Pop ) ) ).Hi ).Lo;
		dmfLoHiByte: Result := WordRec( LongRec( Integer( Round( FuncParams.Pop ) ) ).Lo ).Hi;
		dmfLoLoByte: Result := WordRec( LongRec( Integer( Round( FuncParams.Pop ) ) ).Lo ).Lo;

		dmfHyPot  :
		begin
			e := FuncParams.Pop;
			Result := HyPot( FuncParams.Pop, e );
		end;
		dmfFPowerN:
		begin
			e := FuncParams.Pop;  { Expoent }
			e2 := FuncParams.Pop; { Base    }
			case Round( e2 ) of
				 2: Result := Ldexp( FuncParams.Pop, Round( e ) );
			{ 10: Result := FPower10( FuncParams.Pop, e ); ? Help/Impl Error ? }
			else
				Result := ( FuncParams.Pop * Power( e2, e ) );
			end;
		end;
		dmfLog10  : Result := Log10( FuncParams.Pop );
		dmfLog2   : Result := Log2( FuncParams.Pop );
		dmfLogN   :
		begin
			e := FuncParams.Pop;
			Result := LogN( FuncParams.Pop, e );
		end;

		dmfMin, dmfMax, dmfMean, dmfSum,
		dmfSumSqr, dmfStdDev, dmfNorm:
		begin
			i := FuncParams.Count;
			pda := AllocMem( i * SizeOf( Double ) );
			try
				for j := i - 1 downto 0 do
					pda^[j] := FuncParams.Pop;
				case dmf of
					dmfMin   : Result := MinValue( Slice( pda^, i ) );
					dmfMax   : Result := MaxValue( Slice( pda^, i ) );
					dmfMean  : Result := Mean( Slice( pda^, i ) );
					dmfSum   : Result := Sum( Slice( pda^, i ) );
					dmfSumSqr: Result := SumOfSquares( Slice( pda^, i ) );
					dmfStdDev: Result := StdDev( Slice( pda^, i ) );
					dmfNorm  : Result := Norm( Slice( pda^, i ) );
				end;
			finally
				FreeMem( pda, i * SizeOf( Double ) );
			end;
		end;

		dmfRandG:
		begin
			e := FuncParams.Pop;
			Result := RandG( FuncParams.Pop, e );
		end;
		dmfRandom:
		begin
			Randomize;
			Result := Random( Round( FuncParams.Pop ) );
		end;

		dmfCeil : Result := Ceil( FuncParams.Pop );
		dmfFloor: Result := Floor( FuncParams.Pop );
		dmfSign :
		begin
			e := FuncParams.Pop;
			if ( e >= 0 ) then
				Result := 1
			else
				Result := -1
		end;
		dmfNot  : Result := ( not Round( FuncParams.Pop ) );
	end;
end;

class procedure TKCustomMathSolver.RegisterDefaultGroups;
var
  i: ShortInt;
begin
  for i := FIRST_GROUP_ID to LAST_GROUP_ID do
    with REG_GROUP_INFOS[i] do
  		RegisterGroup( GroupID, Name, Comment );  
end;

procedure TKCustomMathSolver.RegisterDefaultFunctions;
var
	i: TKDefaultMathFunc;
begin
	for i := Low( TKDefaultMathFunc ) to High( TKDefaultMathFunc ) do
    with REG_FUNC_INFOS[i] do
  		RegisterFunction( GroupID, Name, Formula, Comment, ParamNames, ParamCount,
        DefaultFunctionHandler );
end;

class procedure TKCustomMathSolver.RegisterDefaultIdentifiers;
var
	i: TKDefaultMathIdent;
begin
	for i := Low( TKDefaultMathIdent ) to High( TKDefaultMathIdent ) do
    with REG_IDENT_INFOS[i] do
  		RegisterIdent( GroupID, Name, Comment, Value );
end;

procedure TKCustomMathSolver.CreateIdentifiers;
begin
  FIdentifiers := GetIdentifiersClass.Create;
end;

procedure TKCustomMathSolver.CreateExpressions;
begin
	FExpressions := GetExpressionsClass.Create;
end;

function TKCustomMathSolver.GetIdentCount: Integer;
begin
	Result := FIdentifiers.Count;
end;

function TKCustomMathSolver.GetIdentNames( Index: Integer ): string;
begin
	Result := FIdentifiers.Names[Index];
end;

function TKCustomMathSolver.GetIdentValues( const Index: string ): Extended;
begin
	Result := StrToFloatDef( FIdentifiers.Values[Index], 0 );
end;

procedure TKCustomMathSolver.SetIdentNames( Index: Integer; const Value: string );
begin
	if ( ( not ( ( FIdentifiers.IndexOfName( Value ) = -1 ) or
								CheckStrEqual( FIdentifiers.Names[Index], Value ) )
			 ) and IsValidIdent( Value ) ) then
		FIdentifiers.Strings[Index] := ( Value + CH_EQUAL + FIdentifiers.Values[FIdentifiers.Names[Index]] );
end;

procedure TKCustomMathSolver.SetIdentValues( const Index: string; Value: Extended );
begin
	if ( CheckTrimStr( Index ) and ( GetIdentValues( Index ) <> Value ) ) then
		FIdentifiers.Values[Index] := FloatToStr( Value );
end;

function TKCustomMathSolver.GetExpression( Index: Integer ): string;
begin
	if ( FActive and CheckObject( ExprList ) ) then
		Result := ExprList.StrExprs[Index]
	else
		Result := '';
end;

function TKCustomMathSolver.GetExpressionCount: Integer;
begin
  Result := -1;
	if ( FActive and CheckObject( ExprList ) ) then
    case FSolverExprKind of
		  msekAll     : Result := ExprList.ExpressionCount;
  		msekNormal  : Result := ExprList.NormalExpressionCount;
	  	msekFuncExpr: Result := ExprList.FuncExpressionCount;
  	end;
end;

function TKCustomMathSolver.GetExprList: TKMathExpressions;
begin
	if FActive then
		Result := FMathLexer.ExprList
	else
	  Result := nil;	
end;

procedure TKCustomMathSolver.SetIdentifiers( Value: TStrings );
var
  i: Integer;
begin
	if ( not CheckObject( Value ) ) then
  begin
    Close;
		FIdentifiers.Clear;
  end
	else if ( not FIdentifiers.Equals( Value ) ) then
	begin
    Close;
		FIdentifiers.Assign( Value );
		TrimStrings( FIdentifiers );
		if CheckStrings( FIdentifiers ) then
			FIdentifiers.Text := StringReplace( FIdentifiers.Text, CH_LIST_TOKEN, CH_CRLF, krfAll );
    AdjustStringsForValues( FIdentifiers );
		for i := FIdentifiers.Count - 1 downto 0 do
      if ( not CheckTrimStr( FIdentifiers.Names[i] ) ) then
        FIdentifiers.Delete( i )
      else if ( not CheckTrimStr( FIdentifiers.Values[FIdentifiers.Names[i]] ) ) then
				FIdentifiers.Values[FIdentifiers.Names[i]] := FormatFloat( '0', DefIdentResult );
	end;
end;

procedure TKCustomMathSolver.SetExpressions( Value: TStrings );
begin
  if ( not CheckObject( Value ) ) then
  begin
  	Close;
	  FExpressions.Clear;
  end
  else if ( not FExpressions.Equals( Value ) ) then
	begin
    Close;
		FExpressions.Assign( Value );
		TrimStrings( FExpressions );
		if CheckStrings( FExpressions ) then
			FExpressions.Text := StringReplace( FExpressions.Text, CH_LIST_TOKEN, CH_CRLF, krfAll );
	end;
end;

procedure TKCustomMathSolver.SetActive( Value: Boolean );
begin
	if ( Value <> FActive ) then
		if Value then
			Open
		else
			Close;
end;

procedure TKCustomMathSolver.ReOpen;
begin
	Close;
	Open;
end;

procedure TKCustomMathSolver.Open;
begin
	if ( not ( FActive or uksyUtils.Destroying( Self ) ) ) then
	begin
    if ( not CheckStrings( FExpressions ) ) then
      RaiseExceptionFmt( EKMathSolver, sErrInvMathSolverExpr, [ClassName] );
		Close;
		FMathLexer.ExprList.DefFuncResult := DefFuncResult;
		FMathLexer.ExprList.DefIdentResult := DefIdentResult;
		FMathLexer.SetExpressions( FExpressions, FExpressionType );
		FMathLexer.Lexer;
		FActive := True;
	end;
end;

procedure TKCustomMathSolver.Close;
begin
	FActive := False;
  FMathLexer.Reset;
end;

procedure TKCustomMathSolver.IdentifiersChanged( Sender: TObject );
begin
	DoIdentifiersChanged;
end;

procedure TKCustomMathSolver.ExpressionsChanged( Sender: TObject );
begin
  DoExpressionsChanged;
end;

procedure TKCustomMathSolver.SolveEvent( Sender: TKMathExpression; ExprKind: TKMathExprKind;
	const Expr: string; const ReturnValue: Extended );
begin
	DoSolve( Sender.Index, ReturnValue );
end;

procedure TKCustomMathSolver.SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
	var IdentValue: Extended );
var
	s: string;
	i: Integer;
begin
  if IdentRegistered( IdentName ) then
		IdentValue := GetRegIdents( IndexRegIdentOfIdentName( IdentName ) ).IdentValue
	else
  begin
    i := FIdentifiers.IndexOfName( IdentName );
    if ( i <> -1 ) then
    begin
      s := FIdentifiers.Values[FIdentifiers.Names[i]];
      s := StringReplace( s, CH_COMMA, DecimalSeparator, krfAll );
      s := StringReplace( s, CH_DOTMARK, DecimalSeparator, krfAll );
      try
        IdentValue := StrToFloat( s );
      except
        on EConvertError do
          IdentValue := DoSolveIdent( Sender.Index, IdentName )
        else
          raise;
      end;
    end
    else
      IdentValue := DoSolveIdent( Sender.Index, IdentName );
  end;    
end;

procedure TKCustomMathSolver.SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
	FuncParams: TKExtendedStack; var FuncReturn: Extended );
var
  merf: TKMathExprRegFunc;
begin
  if FunctionRegistered( FuncName ) then
	begin
    merf := GetRegFuncs( IndexRegFuncOfFuncName( FuncName ) );
    if ( not ( ( FuncParams.Count = merf.ParamCount ) or
  		( ( merf.ParamCount = OPEN_ARRAY_PARAMS ) and ( FuncParams.Count > 0 ) ) ) ) then
				RaiseExceptionFmt( EKMathSolver, sErrInvFuncParamCount, [merf.ParamCount,
          FuncName, FuncParams.Count] );
    FuncReturn := merf.UserFunc( Sender, FuncName, FuncParams );
	end
	else
  	FuncReturn := DoSolveFunc( Sender.Index, FuncName, FuncParams );
end;

procedure TKCustomMathSolver.DoIdentifiersChanged;
begin
	if Assigned( FOnIdentifiersChanged ) then
		FOnIdentifiersChanged( Self );
	if ( AutoReOpen and CheckStrings( FExpressions ) ) then
		ReOpen;
end;

procedure TKCustomMathSolver.DoExpressionsChanged;
begin
	if Assigned( FOnExpressionsChanged ) then
		FOnExpressionsChanged( Self );
	if ( AutoReOpen and CheckStrings( FExpressions ) ) then
		ReOpen;
end;

procedure TKCustomMathSolver.DoSolve( ExprIdx: Integer; const ReturnValue: Extended );
var
  me: TKMathExpression;
begin
	if Assigned( FOnSolve ) then
	begin
		me := FMathLexer.ExprList[ExprIdx];
		FOnSolve( Self, me, me.MathExpressionKind, me.Owner.StrExprs[ExprIdx], ReturnValue );
	end;
end;

function TKCustomMathSolver.DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended;
var
	me: TKMathExpression;
begin
	Result := DefIdentResult;
	if Assigned( FOnSolveIdent ) then
	begin
    if ( not CheckObject( FSolveStrMathLexer ) ) then
    { Used only in SolveStrExpr function }
      me := FMathLexer.ExprList[ExprIdx]
    else
      me := FSolveStrMathLexer.ExprList[ExprIdx];
		FOnSolveIdent( Self, me, IdentName, Result );
	end;
end;

function TKCustomMathSolver.DoSolveFunc( ExprIdx: Integer; const FuncName: string;
	FuncParams: TKExtendedStack ): Extended;
var
	me: TKMathExpression;
begin
	Result := DefFuncResult;
	if Assigned( FOnSolveFunc ) then
	begin
    if ( not CheckObject( FSolveStrMathLexer ) ) then
    { Used only in SolveStrExpr function }
      me := FMathLexer.ExprList[ExprIdx]
    else
      me := FSolveStrMathLexer.ExprList[ExprIdx];
		FOnSolveFunc( Self, me, FuncName, FuncParams, Result );
	end;
end;

function TKCustomMathSolver.SolveExpr( ExprIdx: Integer ): Extended;
begin
	Open;
	Result := 0;
	case FSolverExprKind of
		msekAll     : Result := ExprList.Expressions[ExprIdx].Solve;
		msekNormal  : Result := ExprList.NormalExpressions[ExprIdx].Solve;
		msekFuncExpr: Result := ExprList.FuncExpressions[ExprIdx].Solve;
	end;
end;

function TKCustomMathSolver.SolveStrExpr( const StrExpr: string ): Extended;
begin
  FSolveStrMathLexer := TKMathLexer.CreateFromExpression( StrExpr );
  try
    FSolveStrMathLexer.ExprList.DefFuncResult := DefFuncResult;
		FSolveStrMathLexer.ExprList.DefIdentResult := DefIdentResult;
    FSolveStrMathLexer.ExprList.OnSolve := SolveEvent;
  	FSolveStrMathLexer.ExprList.OnSolveIdent := SolveIdentEvent;
	  FSolveStrMathLexer.ExprList.OnSolveFunc := SolveFuncEvent;
    FSolveStrMathLexer.Lexer;
    Result := FSolveStrMathLexer.ExprList.NormalExpressions[0].Solve;
  finally
    FreeClean( FSolveStrMathLexer );
  end;
end;

function TKCustomMathSolver.SolveAll: Extended;
var
	i: Integer;
begin
  FSolvingAll := True;
  try
  	Open;
  	Result := 0;
    for i := 0 to ExpressionCount - 1 do
      Result := ( Result + SolveExpr( i ) );
  finally
    FSolvingAll := False;
  end;
end;

{ TKMathSolver }

function TKMathSolver.GetExpressionsClass: TStringListClass;
begin
  Result := TKMathExprTermStrings;
end;

function TKMathSolver.GetIdentifiersClass: TStringListClass;
begin
  Result := TKMathExprTermStrings;
end;

{ TKPropExprTermStrings }

procedure TKPropExprTermStrings.SetOwner( AOwner: TKPropertyExpression );
begin
  ForceObject( AOwner );
  FOwner := AOwner;
end;

procedure TKPropExprTermStrings.DefineProperties( Filer: TFiler );

	function HasData: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKPropExprTermStrings ) then
				Result := ( not Equals( TKPropExprTermStrings( Filer.Ancestor ) ) );
		end
		else
			Result := ( Count > 0 );
	end;

begin
	{
  do not call inherited method, we want to explicity override the defaul TStrings streaming

  inherited DefineProperties( Filer );
  }
	Filer.DefineProperty( 'Exprs', ReadExprs, WriteExprs, HasData );
end;

procedure TKPropExprTermStrings.ReadExprs( Reader: TReader );
var
  sl: TStrings;
begin
{
  After all we found that the real problem is the FindComponent and not the
  Hidden Add! But, with this, we can gain some property set robustness. We
  can validate the expressions set via form text!
}
	Reader.ReadListBegin;
  sl := TStringList.Create;
  try
    while ( not Reader.EndOfList ) do
      sl.Add( Reader.ReadString );
    { This will validade and generate a warning if applicable }
    FOwner.Expressions := sl;
  finally
    sl.Free;
  end;
  { SolveAll; do not solve here, all components should not yet be read! }
	Reader.ReadListEnd;
end;

procedure TKPropExprTermStrings.WriteExprs( Writer: TWriter );
var
	i: Integer;
begin
	Writer.WriteListBegin;
	for i := 0 to Count - 1 do
		Writer.WriteString( Strings[i] );
	Writer.WriteListEnd;
end;

{ TKIDENotify }

type

  TKIDENotify = class( TObject )
  private
    FOwner: TKPropertyExpression;
  public
    constructor Create( AOwner: TKPropertyExpression );
    procedure NotifyLoaded;

    property Owner: TKPropertyExpression
             read FOwner;
  end;

constructor TKIDENotify.Create( AOwner: TKPropertyExpression );
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TKIDENotify.NotifyLoaded;
var
  fi: TIFormInterface;
  mi: TIModuleInterface;
  ci: TIComponentInterface;
begin
  mi := ToolServices.GetModuleInterface( ChangeFileExt( ToolServices.GetCurrentFile, DELPHI_UNIT_EXT ) );
  if CheckObject( mi ) then
    try
      fi := mi.GetFormInterface;
      if CheckObject( fi ) then
        try
          ci := fi.FindComponent( Owner.Name );
          if CheckObject( ci ) then
            try
//              DebugLogMessageEx( 'c:\teste.not', 'TKIDEN.NotifyLoaded - get ci' );
//              DebugLogMessageEx( 'c:\teste.not', 'TKIDEN.NotifyLoaded - ci.Select ? ' + BOOL_NAME[ci.Select] );
              ci.Select;
            finally
              ci.Free;
            end;
        finally
          fi.Free;
        end;
    finally
      mi.Free;
    end;
end;

{ TKPropertyExpression }

constructor TKPropertyExpression.Create( AOwner: TComponent );
begin
	ForceObject( AOwner );
	ForceSingleton( AOwner, TKPropertyExpression );
	inherited Create( AOwner );
  FLocked := True;
  FAutoAdjustExpr := True;
	AutoReOpen := False;
	ExpressionType := stValues;
	SolverExprKind := msekNormal;
  if Designing( Self ) then
  begin
    FIDEHitCount := 5;
    FIDENotify := TKIDENotify.Create( Self );
    Application.HookMainWindow( AppWndProc );
  end
  else
    FIDEHitCount := 0;  
end;

destructor TKPropertyExpression.Destroy;
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKPE.Destroy' );
	{
    We need to destroy the notifier here to avoid synchronization problems. There
		are some differences like: ComponentRenamend did not happen for the PropertyExpr
		component and the module will be deleted before the module interface is freed
		(a uncommon beheaviour).
    After all we put the destroying code in all place, just to gain security.
    For example, we comment the lines above, and when delete the module, the
    notify code was not generated (??), so we loose the module interface ref.
    We saw this beheaviour when the module was deleted but there is no changes
    in it!
  }
  RemoveAllExprs( '' );
  if Designing( Self ) then
  begin
    Application.UnHookMainWindow( AppWndProc );
   	FreeClean( FAuxObject );
    FreeClean( FIDENotify );
  end;  
	inherited Destroy;
end;

procedure TKPropertyExpression.Loaded;
var
  sl: TStrings;
begin
	inherited Loaded;
	if CheckStrings( Expressions ) { and ( not SolvingAll ) } then
  begin
    {
      We need to reevaluate the expressions here. At this point we make sure that
      the streamming system end its job and we can trust in the FindComponent at
      AddExpr.
      But still there is another problem: the IDE just create the component editor
      when the component was selected. The component editor after that will create
      the module notifier that will alter and resolve all!
      There is no need to solve all here! We need only to notify the IDE about the
      property editor, so mark the designer.
    }
    sl := TStringList.Create;
    try
      sl.Assign( Expressions );
      Expressions := nil; { Close and Clear }
      Expressions := sl;  { Reassign with correct values }
    finally
      sl.Free;
    end;
    { Notify IDE to create the component editor! 

      Other interesting issue. When the component is loaded, the IDE read all properties,
      etc... But it only creates the component and property editors when the component
      is selected.
      Because of this, the component editor wasn't create and the notifier either of course.
      So, the sensitive component recalculating does not happen!
      To solve this we just maintain a hacker class created only at dsgn time and use the
      tools api to select the component!
    }
    if Designing( Self ) then
    begin
//      DebugLogMessageEx( 'c:\teste.not', 'TKPE.Loaded - PostMessage' );
      PostMessage( Application.Handle, KM_PROPEXPR_DSGN_LOADED, FIDEHitCount, 0 );
    end;  
    {
    if ( not SolvingAll ) then
  		SolveAll;
    }
  end;
end;

function TKPropertyExpression.AppWndProc( var Message: TMessage ): Boolean;
begin
  Result := ( Designing( Self ) and ( Message.Msg = KM_PROPEXPR_DSGN_LOADED ) );
  if Result then
  begin
    Dec( Message.WParam );
    if ( Message.WParam > 0 ) then
      PostMessage( Application.Handle, Message.Msg, Message.WParam, Message.LParam )
    else
      TKIDENotify( FIDENotify ).NotifyLoaded;
  end;
end;

function TKPropertyExpression.GetExpressionsClass: TStringListClass;
begin
  Result := TKPropExprTermStrings;
end;

function TKPropertyExpression.GetIdentifiersClass: TStringListClass;
begin
  Result := TKMathExprTermStrings;
end;

procedure TKPropertyExpression.CreateExpressions;
begin
  inherited CreateExpressions;
  TKPropExprTermStrings( Expressions ).SetOwner( Self );
end;

procedure TKPropertyExpression.SetLocked( Value: Boolean );
begin
  if ( Value <> FLocked ) then
    if Value then
      Lock
    else
      UnLock;  
end;

procedure TKPropertyExpression.SetExpressions( Value: TStrings );
var
	s: string;
	i, j, k: Integer;
begin
{
  if Loading( Self ) then   DO NOT UNCOMMENT THIS!@
    Exit;
}    
  if ( not CheckObject( Value ) ) then
    inherited SetExpressions( Value )
  else if ( not Expressions.Equals( Value ) ) then
  begin
    Close;
    Expressions.Clear;
    if CheckStrings( Value ) then
			Value.Text := StringReplace( Value.Text, CH_LIST_TOKEN, CH_CRLF, krfAll );
    for i := 0 to Value.Count - 1 do
      { MUST have at least one dot mark and one equal token }
      if CheckStrContainsAll( [CH_EQUAL_TOKEN, CH_DOTMARK], Value[i] ) then
      begin
        s := Value[i];
        j := Pos( CH_DOTMARK, s );
        k := Pos( CH_EQUAL_TOKEN, s );
				AddExpr( Copy( s, 1, j - 1 ), Copy( s, j + 1, k - j - 1 ), Copy( s, k + 1, MaxInt ) );
      end;
		if ( Designing( Self ) and ( Expressions.Count <> Value.Count ) ) then
			WarnFmt( sErrPEInvExprAssign, [Value.Count, Expressions.Count] );
	end;
end;

procedure TKPropertyExpression.Notification( AComponent: TComponent;
	AOperation: TOperation );
begin
	inherited Notification( AComponent, AOperation );
	if ( AOperation = opInsert ) and CheckObjectClass( AComponent, TKPropertyExpression ) and
		 ( TKPropertyExpression( AComponent ) <> Self ) then
		RaiseException( EKPropertyExpression, sErrPEInvInstance );
{ Until here we do not have the csDestroying flag :( }
{  DebugLogMessageEx( 'c:\teste.not', 'TKPE.Notification ' + AComponent.Name + ' - ' + EnumName( Cardinal( AOperation ), TypeInfo( TOperation ) ) );}
	if ( ( AOperation = opRemove ) and CheckObjects( [AComponent, Expressions] ) and
		 IsValidIdent( AComponent.Name ) and ( not Designing( Self ) ) ) then
		RemoveAllExprs( AComponent.Name );
end;

procedure TKPropertyExpression.RegisterDefaultFunctions;
var
  i: TKDefaultPropExprFunc;
begin
  inherited RegisterDefaultFunctions;
	for i := Low( TKDefaultPropExprFunc ) to High( TKDefaultPropExprFunc ) do
    with REG_PROPEXPR_FUNC_INFOS[i] do
  		RegisterFunction( GroupID, Name, Formula, Comment, ParamNames, ParamCount,
        DefaultPropExprFunctionHandler );
end;

type
  TCustomControlHack = class( TCustomControl );
  TGraphicControlHack = class( TGraphicControl );

function TKPropertyExpression.DefaultPropExprFunctionHandler( Sender: TKMathExpression;
 const FuncName: string; FuncParams: TKExtendedStack ): Extended;

	function GetDefaultPropExprFunc( const FuncName: string ): TKDefaultPropExprFunc;
	var
		i: TKDefaultPropExprFunc;
	begin
		Result := Low( TKDefaultPropExprFunc ); { Foo Value to avoid warning }
		for i := Low( TKDefaultPropExprFunc ) to High( TKDefaultPropExprFunc ) do
			if CheckStrEqual( FuncName, REG_PROPEXPR_FUNC_INFOS[i].Name ) then
			begin
				Result := i;
				Exit;
			end;
    RaiseExceptionFmt( EKPropertyExpression, sErrInvDefFunc, [FuncName] );
	end;

  function GetOwnerCanvas: TCanvas;
  begin
    Result := nil;
    if CheckObjectClass( Owner, TCustomForm ) then
      Result := ( Owner as TCustomForm ).Canvas
    else if CheckObjectClass( Owner, TCustomControl ) then
      Result := TCustomControlHack( Owner ).Canvas
    else if CheckObjectClass( Owner, TGraphicControl ) then
      Result := TGraphicControlHack( Owner ).Canvas
    else      
      RaiseExceptionFmt( EKPropertyExpression, sErrInvPEDefFuncOwner, [Owner.ClassName] );
  end;

var
  pc: PChar;
  cv: TCanvas;
	dpef: TKDefaultPropExprFunc;
begin
	Result := 0;
  cv := GetOwnerCanvas;
	dpef := GetDefaultPropExprFunc( FuncName );
  pc := PChar( Trunc( FuncParams.Pop ) );
  if CheckPChar( pc ) then
  begin
    case dpef of
      dpefTextWidth : Result := cv.TextWidth( pc );
      dpefTextHeight: Result := cv.TextHeight( pc );
    end;
    StrDispose( pc );
  end;
end;

procedure TKPropertyExpression.RemoveExpr( const CompName, PropPath: string );
var
	i: Integer;
begin
	if CheckTrimStrs( [CompName, PropPath] ) then
	begin
		i := Expressions.IndexOfName( CompName + CH_DOTMARK + PropPath );
		if ( i <> -1 ) then
		begin
			Close;
			Expressions.Delete( i );
		end;	
	end;
end;

procedure TKPropertyExpression.RemoveAllLeftSideExprs( const CompName: string );
var
  i: Integer;
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKPE.RemoveLSideExprs - ' +  CompName );
  if IsValidIdent( CompName ) then
    for i := Expressions.Count - 1 downto 0 do
      if CheckAbsStrContains( CompName, Expressions.Names[i] ) then
      begin
        if Active then
          Close;
        Expressions.Delete( i );
//        DebugLogMessageEx( 'c:\teste.not', 'TKPE.RemoveLSideExprs.DeleteExpr(' + IntToStr( i ) +')');
      end;
end;

procedure TKPropertyExpression.RemoveAllRightSideExprs( const CompName: string );
var
	i, j: Integer;
	sName, sValue: string;
	me: TKMathExpression;
begin
  for i := Expressions.Count - 1 downto 0 do
  begin
    sName := Expressions.Names[i];
    if CheckAbsStrContains( CompName, Expressions.Values[sName] ) then
    begin
      if ( not Active ) then
        Open;
      me := ExprList.Expressions[i];
      sValue := Expressions.Values[sName];
//      DebugLogMessageEx( 'c:\teste.not', 'TKPE.RemoveAllExprs Bf: ' + sName + '=' + sValue );
      { Here we need to alter the IdentName by the IdentValue }
      for j := 0 to me.IdentCount - 1 do
        if CheckAbsStrContains( CompName, me.IdentName[j] ) then
          sValue := StringReplace( sValue, me.IdentName[j], FormatFloat( '0', me.IdentValue[j] ), krfAll );
//      DebugLogMessageEx( 'c:\teste.not', 'TKPE.RemoveAllExprs Af: ' + sName + '=' + sValue );
      j := Pos( CH_DOTMARK, sName );
      AddExpr( Copy( sName, 1, j - 1 ), Copy( sName, j + 1, MaxInt ), sValue );
      { Expressions[i] := Expressions.Names[i] + CH_EQUAL_TOKEN + sValue; }
    end;
  end;  
end;

procedure TKPropertyExpression.RemoveAllExprs( const CompName: string );
begin
//  DebugLogMessageEx( 'c:\teste.not', 'TKPE.RemoveAllExprs Start: ' + CompName + ' ? ' + BOOL_NAME[IsValidIdent( CompName )]);
{
  if CheckObject( Expressions ) then
    DebugLogMessageEx( 'c:\teste.not', #9 + StringReplace( Expressions.Text, CH_CRLF, CH_CRLF + CH_TAB, krfAll ) );
}
	if IsValidIdent( CompName ) then
  begin
{
  When we delete a component, if it make part of a left side expression, we need to
  remove the expression at all. If it make part of a right side expression, we need
  to assign to it the last calculated value (if already active - should always be at
  this point) or open it to recalculate.
  So, we need to evaluate first of all the right side expressions, to avoid deleting
  left side expressions (close the lexer) and loose the calculated values for the
  already deleted component. If could take more or unecessary work (right side expressions
  that will be deleted), but it's the price.
  eg:

  a) bn2.Left=bn1.Left
  b) bn2.Top=bn1.Top+bn1.Height+memo1.tag
  c) memo1.Top=bn1.top+memo1.tag
  d) memo1.Left=bn1.left+bn1.width+3

  In this case if we delete the memo component, we need do delete all expressions
  c) and d) and only part of the right side of the expression b).
  With the algorithm above we will: Substitute all memo1.tag with their value (eg 2)
  and after all delete c), d). Note that there isn't necessary to evaluate the right side
  of the c) expression! (This is the overhead).

  PS: To evaluate the new expression result we need to use other lexer in order to
      do not affect the saved values. If the returned solved value contains only
      one term (i.e. one result number), delete the expression at all!
      eg: bn2.Left will become bn2.Left=???, so, delete it to do not lock the
          control.
      Actually we delete all the expression, and not only the affected part
}
{ First check the right side expressions }
    RemoveAllRightSideExprs( CompName );
{ Second, of all check the left side expressions }
    RemoveAllLeftSideExprs( CompName );
  end
{ Delete all expressions if the CompName is empty }
  else if ( not CheckTrimStr( CompName ) ) then
    Expressions := nil;
end;

procedure TKPropertyExpression.RenameAllExprs( const OldCompName, NewCompName: string );
begin
//	DebugLogMessageEx( 'c:\teste.not', 'TKPE.RenameAllExprs (o;n) = ' + OldCompName + ';' + NewCompName );
	if ( IsValidIdent( OldCompName ) and IsValidIdent( NewCompName ) ) then
	begin
    Close;
		Expressions.Text := StringReplace( Expressions.Text, OldCompName, NewCompName, krfAll );
	end;
end;

function TKPropertyExpression.AddExpr( const CompName, PropPath, Expr: string ): Integer;
var
	c: TComponent;
//  e: extended;
begin
	Result := -1;
	if CheckTrimStrs( [CompName, PropPath, Expr] ) then
	begin
  {
    Here we have a problem: When comming from an Alt+F12, the loaded properties - DefineProperties,
    The components should not be yet created. So the expressions are lossed. See Loaded!
  }
		c := Owner.FindComponent( CompName );
		if CheckObject( c ) then
		begin
			Close;
			Result := Expressions.IndexOfName( CompName + CH_DOTMARK + PropPath );
//			DebugLogMessageEx( 'c:\teste.not', 'TKPE.AddExpr(' + IntToStr( Result ) + ') = ' + CompName + '.' + PropPath + '=' + Expr );
			if ( Result = -1 ) then
			begin
				Result := Expressions.Add( CompName + CH_DOTMARK + PropPath + CH_EQUAL + Expr );
        if ( not Designing( Self ) ) then
  				c.FreeNotification( Self );
			end
			else if ( not CheckStrEqual( Expressions.Values[Expressions.Names[Result]], Expr ) ) then
			begin
//        DebugLogMessageEx( 'c:\teste.not', 'TKPE.AddExpr(' + IntToStr( Result ) + ') b = ' + Expressions[Result] );
				Expressions.Values[Expressions.Names[Result]] := Expr;
//        DebugLogMessageEx( 'c:\teste.not', 'TKPE.AddExpr(' + IntToStr( Result ) + ') a = ' + Expressions[Result] );
				{e := }SolveExpr( Result );
//        DebugLogMessageEx( 'c:\teste.not', 'TKPE.AddExpr(' + IntToStr( Result ) + ') solved = ' + FormatFloat( '0',  e ) );
			end;
		end;
	end;
end;

procedure TKPropertyExpression.AdjustRelativeExpr( const CompNameList: string );
var
  i, j: Integer;
  slComps: TStrings;
begin
  slComps := TStringList.Create;
  try
    ExtractStrings( CompNameList, CH_LIST_TOKEN, slComps );
//    DebugLogMessageExFmt( 'c:\teste.not', 'TKPE.AdjustRelativeExpr - s = %s; sl = %d', [CompNameList, slComps.count] );
//    DebugLogMessageEx( 'c:\teste.not', #9 + StringReplace( slComps.Text, CH_CRLF, CH_CRLF + CH_TAB, krfAll ) );
    TrimStrings( slComps );
    for i := 0 to slComps.Count - 1 do
      if AutoAdjustExpr then
      { This will remove all left side expressions and adjust all right side expressions }
        RemoveAllExprs( slComps[i] )
      else
      begin
      { This will remove all left and right side expressions }
        RemoveAllLeftSideExprs( slComps[i] );
        if Active then
          Close;
        { Delete the right side without readjusting it }  
        for j := 0 to Expressions.Count - 1 do
          if CheckAbsStrContains( slComps[i], Expressions.Values[Expressions.Names[j]] ) then
            Expressions.Delete( i );
      end;
  finally
    slComps.Free;
  end;
end;

procedure TKPropertyExpression.Lock;
begin
  if ( not FLocked ) then
  begin
//    DebugLogMessageEx( 'c:\teste.not', 'TKPE.Lock' );
    FLocked := True;
    DoLock;
  end;
end;

procedure TKPropertyExpression.UnLock;
begin
  if FLocked then
  begin
//    DebugLogMessageEx( 'c:\teste.not', 'TKPE.Unlock' );
    FLocked := False;
    DoUnLock;
  end;
end;

procedure TKPropertyExpression.DoLock;
begin
  if Assigned( FOnLock ) then
    FOnLock( Self );
end;

procedure TKPropertyExpression.DoUnLock;
begin
  if Assigned( FOnUnLock ) then
    FOnUnLock( Self );
end;

procedure TKPropertyExpression.SolveIdentEvent( Sender: TKMathExpression; const IdentName: string;
	var IdentValue: Extended );
begin
	IdentValue := DoSolveIdent( Sender.Index, IdentName );
end;

procedure TKPropertyExpression.SolveFuncEvent( Sender: TKMathExpression; const FuncName: string;
	FuncParams: TKExtendedStack; var FuncReturn: Extended );
begin
  FuncReturn := DoSolveFunc( Sender.Index, FuncName, FuncParams );
end;

function TKPropertyExpression.DoSolveIdent( ExprIdx: Integer; const IdentName: string ): Extended;
var
  me: TKMathExpression;
	cpv: TKCompPathValue;
begin
  Result := DefIdentResult;
  cpv := GetCompPathValue( Owner, IdentName, PROPEXPR_TYPEKIND_SET );
  Force( [cpv.Obj, cpv.PropInfo] );
  case cpv.PropInfo^.PropType^^.Kind of
    tkInteger, tkChar, tkEnumeration,
    tkSet, tkClass, tkWChar, tkInterface:
      Result := GetOrdProp( cpv.Obj, cpv.PropInfo );
    tkFloat:
      Result := GetFloatProp( cpv.Obj, cpv.PropInfo );
    tkString, tkLString, tkWString:
    begin
      me := ExprList.Expressions[ExprIdx];
      { for this especial case, all this conditions MUST occur! }
      if ( ( me.MathExpressionKind = mekFuncExpr ) and ( me.MathOpCount = 0 ) and
        ( me.NumberCount = 0 ) and ( me.IdentCount = 1 ) and ( me.FuncCount = 0 ) ) then
      begin
        { me.OwnData := True; }
        { me.Data := StrNew( PChar( GetStrProp( cpv.Obj, cpv.PropInfo ) ) ); }
        { will dispose at the function def solver }
        Result := LongInt( StrNew( PChar( GetStrProp( cpv.Obj, cpv.PropInfo ) ) ) );
      end
      else
        RaiseExceptionFmt( EKPropertyExpression, sErrInvMathIdentType, [IdentName,
          ExprIdx + Byte( Ord( Designing( Self ) ) ), Byte( cpv.PropInfo^.PropType^^.Kind )] )
    end;      
  else
    RaiseExceptionFmt( EKPropertyExpression, sErrInvMathIdentType, [IdentName,
      ExprIdx + Byte( Ord( Designing( Self ) ) ), Byte( cpv.PropInfo^.PropType^^.Kind )] );
  end;
end;

function TKPropertyExpression.DoSolveFunc( ExprIdx: Integer; const FuncName: string;
	FuncParams: TKExtendedStack ): Extended;
begin
  Result := DefFuncResult;
	RaiseExceptionFmt( EKPropertyExpression, sErrInvMathFunc, [FuncName,
    ExprIdx + Byte( Ord( Designing( Self ) ) )] );
end;

procedure TKPropertyExpression.DoSolve( ExprIdx: Integer; const Value: Extended );
var
	cpv: TKCompPathValue;
begin
  if ( CheckObject( ExprList ) and ( ExprList.Expressions[ExprIdx].MathExpressionKind = mekNormal ) ) then
  begin
  	cpv := GetCompPathValue( Owner, Expressions.Names[ExprIdx], [tkInteger, tkChar,
	  	tkEnumeration, tkClass, tkSet, tkWChar, tkInterface, tkFloat] );
    Force( [cpv.Obj, cpv.PropInfo] );
	  DoNotifySolve( ExprIdx, cpv.Obj, cpv.PropInfo, Value );
  end;  
  inherited DoSolve( ExprIdx, Value );
end;

procedure TKPropertyExpression.DoNotifySolve( ExprIdx: Integer; Obj: TPersistent;
	PropInfo: PPropInfo; const Value: Extended );
begin
	if Designing( Self ) then
	begin
    { DebugLogMessageExFmt( 'c:\teste.not', 'TKPE.DoNotifySolve: %d - %s.%s = %s', [ExprIdx, Obj.ClassName,
      PropInfo^.Name, FormatFloat( '0', Value )]);}
		if ( PropInfo^.PropType^^.Kind = tkFloat ) then
			SetFloatProp( Obj, PropInfo, Value )
		else if ( PropInfo^.PropType^^.Kind in PROPEXPR_TYPEKIND_ORDSET ) then
			SetOrdProp( Obj, PropInfo, Trunc( Value ) )
    else
      RaiseExceptionFmt( EKPropertyExpression, sErrInvMathExprType, [Expressions.Names[ExprIdx],
        ( ExprIdx + 1 ), Byte( PropInfo^.PropType^^.Kind )] );
		{ MarkDesigner( Self ); ! no need more }
	end
	else if Assigned( FOnNotifySolve ) then
		FOnNotifySolve( Self, ExprIdx, Obj, PropInfo, Value );
end;

{
--------------------------------------------------------------------------------
-------------------------- Code Parser Architecture ----------------------------
--------------------------------------------------------------------------------
}

{ TKCustomPascalParsingFrame }

constructor TKCustomPascalParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObjectClass( AParser, TKCustomPascalParser );
  inherited Create( AParser );
  with ( AParser as TKCustomPascalParser ) do
    Self.FPascalToken := FPascalToken;
end;

procedure TKCustomPascalParsingFrame.RestoreFrame;
begin
  inherited RestoreFrame;
  with ( Parser as TKCustomPascalParser ) do
    FPascalToken := Self.FPascalToken;
end;

{ TKCustomPascalParser }

function TKCustomPascalParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKCustomPascalParsingFrame;
end;

function TKCustomPascalParser.CheckPascalToken( APascalToken: TKPascalToken ): Boolean;
begin
	Result := ( CheckToken( tpSpecial ) and ( PascalToken = APascalToken ) );
end;

procedure TKCustomPascalParser.ForcePascalToken( APascalToken: TKPascalToken );
begin
	if ( not CheckPascalToken( APascalToken ) ) then
		Error( sErrInvSpecial );
end;

function TKCustomPascalParser.GetDefaultErrorClass: EKParserClass;
begin
	Result := EKPascalParser;
end;

function TKCustomPascalParser.GetStringCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_STRING_CHARSET; { '#', '''' }
end;

function TKCustomPascalParser.GetIntegerCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_INTEGER_CHARSET; { '-', '0'..'9' }
end;

function TKCustomPascalParser.GetFloatCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_FLOAT_CHARSET; { '-', '0'..'9', 'e', 'E', '+' }
end;

function TKCustomPascalParser.GetRelOpCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_RELOP_CHARSET; { '>', '<', '=' }
end;

function TKCustomPascalParser.GetSpecialCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_SPECIAL_CHARSET; { '(', ':', '.' }
end;

function TKCustomPascalParser.GetBlankCharSet: TKCharSet;
begin
	Result := DEFAULT_PARSER_BLANK_CHARSET; { #0..#32 }
end;

function TKCustomPascalParser.CheckHexaToken( P: PChar ): Boolean;
begin
	Result := ( P^ = CH_HEXA_TOKEN );
{ Result := ( P^ = '0' ) and ( ( P + 1 ) )^ = 'x' ) // for C/C++ parsers }
end;

function TKCustomPascalParser.CheckRelOpToken( P: PChar ): Boolean;
begin
	Result := ( P^ in CharSets[RELOP_CHARSET_IDX] );
	if Result then
		Result := ( ( P^ = CH_EQUAL_TOKEN ) or
			( ( P^ <> CH_EQUAL_TOKEN ) and ( ( P + 1 )^ = CH_EQUAL_TOKEN ) ) or
			( ( P^ = CH_LOWERTHAN ) and ( ( P + 1 )^ = CH_GREATERTHAN ) ) );
end;

function TKCustomPascalParser.CheckSpecialToken( P: PChar ): Boolean;
begin
	Result := ( P^ in CharSets[SPECIAL_CHARSET_IDX] );
	if Result then
		Result := ( ( ( P^ = CH_COLON ) and ( ( P + 1 )^ = CH_EQUAL_TOKEN ) ) or
			( ( P^ = CH_PARENTHESIS_OPEN ) and ( ( P + 1 )^ = CH_DOTMARK ) ) or
			( ( P^ = CH_DOTMARK ) and ( ( P + 1 )^ in [CH_DOTMARK, CH_PARENTHESIS_CLOSE] ) ) );
end;

function TKCustomPascalParser.CheckSymbolToken( P: PChar ): Boolean;
begin
	Result := ( inherited CheckSymbolToken( P ) ) and ( not ( P^ in ( CHARSET_DIGIT + [CH_DOTMARK] ) ) );
end;

function TKCustomPascalParser.ProcessString( var P: PChar ): TKTokenType;
var
	S: PChar;
	i: Integer;
begin
	S := P;
	while True do
		case P^ of
			CH_GRIDLING:
			begin
				Inc( P );
				i := 0;
				while ( P^ in CHARSET_DIGIT ) do
				begin
					i := ( i * 10 ) + ( Ord( P^ ) - Ord( '0' ) );
					Inc( P );
				end;
				S^ := Chr( i );
				Inc( S );
			end;
			CH_PLICK:
			begin
				Inc( P );
				while True do
				begin
					case P^ of
						CH_NULL, CH_LF, CH_CR:
							Error( sErrParseInvStr );
						CH_PLICK:
						begin
							Inc( P );
							if ( P^ <> CH_PLICK ) then
								Break;
						end;
					end;
					S^ := P^;
					Inc( S );
					Inc( P );
				end;
			end;
		else
			Break;
		end;
	Result := inherited ProcessString( S );
end;

function TKCustomPascalParser.ProcessRelOpToken( var P: PChar ): TKTokenType;
begin
	RelOp := roNone;
	case P^ of
		CH_LOWERTHAN:
		begin
			Inc( P );
			if ( P^ = CH_EQUAL_TOKEN ) then
				RelOp := roLessEqual
			else if ( P^ = CH_GREATERTHAN ) then
				RelOp := roDifferent
			else
				RelOp := roLessThan;
			if ( RelOp in [roLessEqual, roDifferent] ) then
				Inc( P );
		end;
		CH_EQUAL_TOKEN:
		begin
			Inc( P );
			RelOp := roEqual;
		end;
		CH_GREATERTHAN:
		begin
			Inc( P );
			if ( P^ = CH_EQUAL_TOKEN ) then
				RelOp := roGreaterEqual
			else
				RelOp := roGreaterThan;
			if ( RelOp = roGreaterEqual ) then
				Inc( P );
		end;
	end;
	Result := inherited ProcessRelOpToken( P )
end;

function TKCustomPascalParser.ProcessSpecialToken( var P: PChar ): TKTokenType;
begin
	FPascalToken := ptNone;
	case P^ of
		CH_DOTMARK:
		begin
			Inc( P );
			case P^ of
				CH_DOTMARK:
					FPascalToken := ptDotDot;
				CH_PARENTHESIS_CLOSE:
					FPascalToken := ptDotParens;
			end;
			Inc( P );
		end;
		CH_COLON:
		begin
			Inc( P, 2 );
			FPascalToken := ptAssign;
		end;
		CH_PARENTHESIS_OPEN:
		begin
			Inc( P, 2 );
			FPascalToken := ptParensDot;
		end;
	end;
	Result := inherited ProcessSpecialToken( P );
end;

function TKCustomPascalParser.MakeStringLegal( const s: string ): string;
var
	i: Integer;
begin
	if CheckStr( s ) then
	begin
    Result := '';
		if ( s[1] > #31 ) then { Check first char for special tokens }
			Result := CH_PLICK;
		for i := 1 to Length( s ) do
			case s[i] of
				CH_PLICK:
				begin
					Result := Result + CH_PLICK + CH_PLICK; { Plicks must be doubled }
					Position := Position + 1;
				end;
				#0..#31:
				begin
					Position := Position + Length( IntToStr( Ord( s[i] ) ) );
				{ Check preceding char. For plain ones, close the string }
					if ( i > 1 ) and ( s[i-1] > #31 ) then
						Result := Result + CH_PLICK;
				{ Append the '#' token }
					Result := Result + CH_GRIDLING + IntToStr( Ord( s[i] ) );
				{ Check following char. For plain ones, open the string }
					if ( ( i < ( Length( s ) - 1 ) ) and ( s[i + 1] > #31 ) ) then
					  Result := Result + CH_PLICK;
				end;
			else
				Result := Result + ValidateSpecialToken( s[i] );
		end;
		{ Check last char. For special ones, close the string }
		if ( s[Length( s )] > #31 ) then
			Result := Result + CH_PLICK;
	end
	else
		Result := CH_PLICK + CH_PLICK;
	Result := inherited MakeStringLegal( Result );
end;

procedure TKCustomPascalParser.DoBeforeRelOp( const ARelOp: string );
var
  s: string;
begin
	if Assigned( FBeforeRelOp ) then
  begin
    s := OutPutString;
		FBeforeRelOp( Self, s, ARelOp );
    OutPutString := s;
  end;  
end;

procedure TKCustomPascalParser.DoAfterRelOp;
var
  s: string;
begin
	if Assigned( FAfterRelOp ) then
  begin
    s := OutPutString;
		FAfterRelOp( Self, s );
    OutPutString := s;
  end;  
end;

procedure TKCustomPascalParser.DoBeforePascalToken( const ASpecial: string );
var
  s: string;
begin
	if Assigned( FBeforePascalToken ) then
  begin
    s := OutPutString;
		FBeforePascalToken( Self, s, ASpecial );
    OutPutString := s;
  end;
end;

procedure TKCustomPascalParser.DoAfterPascalToken;
var
  s: string;
begin
	if Assigned( FAfterPascalToken ) then
  begin
    s := OutPutString;
		FAfterPascalToken( Self, s );
    OutPutString := s;
  end;
end;

procedure TKCustomPascalParser.ProcessConversion( const s: string );
begin
	case Token of
		tpRelOp:
		begin
			DoBeforeRelOp( s );
			OutPutString := OutPutString + s;
			DoAfterRelOp;
		end;
		tpSpecial:
		begin
			DoBeforePascalToken( s );
			OutPutString := OutPutString + s;
			DoAfterPascalToken;
		end;
	else
		inherited ProcessConversion( s );
	end;
end;

{ TKPascalParsingFrame }

constructor TKPascalParsingFrame.Create( AParser: TKCustomParser );
begin
  ForceObjectClass( AParser, TKPascalParser );
  inherited Create( AParser );
  with ( AParser as TKPascalParser ) do
    Self.FCommentType := FCommentType;
end;

procedure TKPascalParsingFrame.RestoreFrame;
begin
  inherited RestoreFrame;
  with ( Parser as TKPascalParser ) do
    FCommentType := Self.FCommentType;
end;

{ TKPascalParser }

function TKPascalParser.GetParsingFrameClass: TKParsingFrameClass;
begin
  Result := TKPascalParsingFrame;
end;

function TKPascalParser.GetDefaultKeyWords: string;
begin
	Result := PASCAL_PARSER_KEYWORDS;
end;

function TKPascalParser.GetCommentType( P: PChar ): TKCommentType;
begin
	Result := ctNone;
	case P^ of
		CH_BRACKET_OPEN:
			Result := ctBraket;
		CH_PARENTHESIS_OPEN:
			if ( ( P + 1 ) ^ <> toEOF ) and ( ( P + 1 )^ = CH_ASTERISK ) then
				Result := ctParentesis;
		CH_SLASH:
			if ( ( P + 1 ) ^ <> toEOF ) and ( ( P + 1 )^ = CH_SLASH ) then
				Result := ctLine;
	end;
end;

function TKPascalParser.GetCommentCharSet: TKCharSet;
begin
	Result := DEFAULT_PASCALPARSER_COMMENT_CHARSET; (* '{', '}', '/', '(', ')', '*' *)
end;

function TKPascalParser.CheckCommentToken( P: PChar ): Boolean;
begin
	Result := ( GetCommentType( P ) <> ctNone );
end;

function TKPascalParser.ProcessComment( var P: PChar ): TKTokenType;
begin
	FCommentType := GetCommentType( P );
	case FCommentType of
		ctBraket:
		begin
			while ( P^ <> CH_BRACKET_CLOSE ) and ( P^ <> toEOF ) do
				Inc( P );
			if ( P^ <> toEOF ) then
				Inc( P );
			Result := inherited ProcessComment( P );
		end;
		ctParentesis:
		begin
			while ( P^ <> CH_ASTERISK ) and ( ( P + 1 )^ <> toEOF ) and
						( ( P + 1 )^ <> CH_PARENTHESIS_CLOSE ) do
				Inc( P );
			if ( P^ <> toEOF ) then
				Inc( P );
			Result := inherited ProcessComment( P );
		end;
		ctLine:
		begin
			while ( P^ <> CH_CR ) and ( P^ <> toEOF ) do
				Inc( P );
			Result := inherited ProcessComment( P );
		end;
	else
		Result := P^;
	end;
end;

function TKPascalParser.TokenComponentIdent: String;
var
	P: PChar;
begin
	ForceToken( tpSymbol );
	P := SourcePtr;
	while ( P^ = CH_DOTMARK ) do
	begin
		Inc( P );
		if ( not ( P^ in CharSets[SYMBOL_CHARSET_IDX] ) ) then
			Error( sErrIdentExpected );
		repeat
			Inc( P )
		until ( not ( P^ in CharSets[SYMBOL_CHARSET_IDX] ) );
	end;
	SourcePtr := P;
	Result := TokenString;
end;

{ TKDFMParser }

function TKDFMParser.GetDefaultKeyWords: string;
begin
	Result := DFM_PARSER_KEYWORDS;
end;

function TKDFMParser.GetBinaryEndToken: Char;
begin
	Result := CH_BRACKET_CLOSE;
end;

procedure TKDFMParser.HexToBinary( AStream: TStream );
var
	iCount: Integer;
	cBuffer: array[Byte] of Char;
begin
	ForceObject( AStream );
	ZeroMemory( @cBuffer, SizeOf( cBuffer ) );
	ProcessBlank;
	while ( SourcePtr^ <> GetBinaryEndToken ) do
	begin
		iCount := HexToBin( SourcePtr, cBuffer, SizeOf( cBuffer ) );
		if ( iCount = 0 ) then
			Error( sErrParseInvBin );
		AStream.WriteBuffer( cBuffer, iCount ); //? Write* or WriteBuffer
		SourcePtr := SourcePtr + ( iCount * 2 ); // ? Why 2*Count
		ProcessBlank;
	end;
	NextToken;
end;

{ TKPascalToHtmlConverter }

procedure TKPascalToHtmlConverter.DoBuildHeader;
begin
	DoHtmlHeader;
	inherited DoBuildHeader;
end;

procedure TKPascalToHtmlConverter.DoBuildFooter;
begin
	DoHtmlFooter;
	inherited DoBuildFooter;
end;

procedure TKPascalToHtmlConverter.DoBeforeString( const Str: string );
begin
	inherited DoBeforeString( Str );
end;

procedure TKPascalToHtmlConverter.DoAfterString;
begin
	inherited DoAfterString;
end;

procedure TKPascalToHtmlConverter.DoBeforeKeyword( const KeyWord: string );
begin
	OutPutString := OutPutString + '<B>';
	inherited DoBeforeKeyword( KeyWord );
end;

procedure TKPascalToHtmlConverter.DoAfterKeyword;
begin
  OutPutString := OutPutString + '</B>';
	inherited DoAfterKeyword;
end;

procedure TKPascalToHtmlConverter.DoBeforeSymbol( const Symbol: string );
begin
	inherited DoBeforeSymbol( Symbol );
end;

procedure TKPascalToHtmlConverter.DoAfterSymbol;
begin
	inherited DoAfterSymbol;
end;

procedure TKPascalToHtmlConverter.DoBeforeComment( const Comment: string );
begin
	OutPutString := OutPutString + '<FONT COLOR="#000080"><I>';
	inherited DoBeforeComment( Comment );
end;

procedure TKPascalToHtmlConverter.DoAfterComment;
begin
  OutPutString := OutPutString + '</I></FONT>';
	inherited DoAfterComment;
end;

procedure TKPascalToHtmlConverter.DoBeforeNumber( AType: TKTokenType; const Number: string );
begin
	inherited DoBeforeNumber( AType, Number );
end;

procedure TKPascalToHtmlConverter.DoAfterNumber( AType: TKTokenType );
begin
	inherited DoAfterNumber( AType );
end;

function TKPascalToHtmlConverter.ValidateSpecialToken( Ch: Char ): string;
begin
	case Ch of
		'<': Result := '&lt;';
		'>': Result := '&gt;';
		'&': Result := '&amp;';
		'"': Result := '&quot;';
	else
		Result := inherited ValidateSpecialToken( Ch );
	end;
end;

procedure TKPascalToHtmlConverter.DoHtmlHeader;
var
	s: string;
begin
	s := OutputString;
	if Assigned( FOnHtmlHeader ) then
		FOnHtmlHeader( Self, s )
	else if ( not Assigned( FOnHtmlFooter ) ) then
		DefaultHtmlHeader;
	OutputString := s;
	OutPutString := OutPutString + '<PRE>' + CH_CRLF;
end;

procedure TKPascalToHtmlConverter.DoHtmlFooter;
var
	s: string;
begin
	s := OutputString + '</PRE>';
	if Assigned( FOnHtmlFooter ) then
		FOnHtmlFooter( Self, s )
	else if ( not Assigned( FOnHtmlHeader ) ) then
	  DefaultHtmlFooter;	
	OutputString := s;
end;

procedure TKPascalToHtmlConverter.DefaultHtmlHeader;
var
	s: string;
begin
  s := ExtractFileName( Filename );
	OutPutString := OutPutString + '<HTML><HEAD>'#13#10;
	if CheckStr( s ) then
		OutPutString := OutPutString + '<TITLE>File: ' +  s + '</TITLE>'#13#10;
	OutPutString := OutPutString + '<META NAME="Pascal to Html Converter" ' +
	  'CONTENT="Knowhow® Informatica 1999">'#13#10 +
		'</HEAD>'#13#10'<BODY BGCOLOR="#FFFFFF">'#13#10;
	if CheckStr( s ) then
		OutPutString := OutPutString + Format( '<A NAME=%0:s><H3>%0:s</H3></A>'#13#10#13#10, [s] );
end;

procedure TKPascalToHtmlConverter.DefaultHtmlFooter;
var
	s: string;
begin
	s := ExtractFileName( Filename );
	OutPutString := OutPutString + '<HR><CENTER<I>Generated by Pascal to Html Converter<P>'#13#10 +
		'CopyRight© KnowHow® Informatica 1999. Demian Lessa / Leonardo Freitas </CENTER></I>'#13#10 +
		'</BODY> </HTML>';
end;

{ TKDFMToHtmlConverter }

procedure TKDFMToHtmlConverter.DoBuildHeader;
begin
	DoHtmlHeader;
	inherited DoBuildHeader;
end;

procedure TKDFMToHtmlConverter.DoBuildFooter;
begin
	DoHtmlFooter;
	inherited DoBuildFooter;
end;

procedure TKDFMToHtmlConverter.DoBeforeString( const Str: string );
begin
	inherited DoBeforeString( Str );
end;

procedure TKDFMToHtmlConverter.DoAfterString;
begin
	inherited DoAfterString;
end;

procedure TKDFMToHtmlConverter.DoBeforeKeyword( const KeyWord: string );
begin
	OutPutString := OutPutString + '<B>';
	inherited DoBeforeKeyword( KeyWord );
end;

procedure TKDFMToHtmlConverter.DoAfterKeyword;
begin
  OutPutString := OutPutString + '</B>';
	inherited DoAfterKeyword;
end;

procedure TKDFMToHtmlConverter.DoBeforeSymbol( const Symbol: string );
begin
	inherited DoBeforeSymbol( Symbol );
end;

procedure TKDFMToHtmlConverter.DoAfterSymbol;
begin
	inherited DoAfterSymbol;
end;

procedure TKDFMToHtmlConverter.DoBeforeComment( const Comment: string );
begin
	OutPutString := OutPutString + '<FONT COLOR="#000080"><I>';
	inherited DoBeforeComment( Comment );
end;

procedure TKDFMToHtmlConverter.DoAfterComment;
begin
	OutPutString := OutPutString + '</I></FONT>';
	inherited DoAfterComment;
end;

procedure TKDFMToHtmlConverter.DoBeforeNumber( AType: TKTokenType; const Number: string );
begin
	inherited DoBeforeNumber( AType, Number );
end;

procedure TKDFMToHtmlConverter.DoAfterNumber( AType: TKTokenType );
begin
	inherited DoAfterNumber( AType );
end;

function TKDFMToHtmlConverter.ValidateSpecialToken( Ch: Char ): string;
begin
	case Ch of
		'<': Result := '&lt;';
		'>': Result := '&gt;';
		'&': Result := '&amp;';
		'"': Result := '&quot;';
	else
		Result := inherited ValidateSpecialToken( Ch );
	end;
end;

procedure TKDFMToHtmlConverter.DoHtmlHeader;
var
	s: string;
begin
	s := OutputString;
	if Assigned( FOnHtmlHeader ) then
		FOnHtmlHeader( Self, s )
	else if ( not Assigned( FOnHtmlFooter ) ) then
		DefaultHtmlHeader;
	OutputString := s;
	OutPutString := OutPutString + '<PRE>' + CH_CRLF;
end;

procedure TKDFMToHtmlConverter.DoHtmlFooter;
var
	s: string;
begin
	s := OutputString + '</PRE>';
	if Assigned( FOnHtmlFooter ) then
		FOnHtmlFooter( Self, s )
	else if ( not Assigned( FOnHtmlHeader ) ) then
		DefaultHtmlFooter;
	OutputString := s;
end;

procedure TKDFMToHtmlConverter.DefaultHtmlHeader;
var
	s: string;
begin
	s := ExtractFileName( Filename );
	OutPutString := OutPutString + '<HTML><HEAD>'#13#10;
	if CheckStr( s ) then
		OutPutString := OutPutString + '<TITLE>File: ' +  s + '</TITLE>'#13#10;
	OutPutString := OutPutString + '<META NAME="DFM to Html Converter" ' +
		'CONTENT="Knowhow® Informatica 1999">'#13#10 +
		'</HEAD>'#13#10'<BODY BGCOLOR="#FFFFFF">'#13#10;
	if CheckStr( s ) then
		OutPutString := OutPutString + Format( '<A NAME=%0:s><H3>%0:s</H3></A>'#13#10#13#10, [s] );
end;

procedure TKDFMToHtmlConverter.DefaultHtmlFooter;
var
	s: string;
begin
	s := ExtractFileName( Filename );
	OutPutString := OutPutString + '<HR><CENTER<I>Generated by DFM to Html Converter<P>'#13#10 +
		'CopyRight© KnowHow® Informatica 1999. Demian Lessa / Leonardo Freitas </CENTER></I>'#13#10 +
		'</BODY> </HTML>';
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
  FRegGroup := TKMathExprTermStrings.Create;
	FRegFunc := TKMathExprTermStrings.Create;
	FRegIdent := TKMathExprTermStrings.Create;
end;

procedure Done;
begin
  FRegIdent.Free;
	FRegFunc.Free;
  FRegGroup.Free;
	if CheckObject( CommandShell ) then
	  DoneShell;
end;

initialization
	Init;

finalization
	Done;

(*

PS: This text must not be after END., because of a Delphi4 warning...
		This text is here for future analysis (see DCC32 ini file architecture)...

	procedure SortValues( sl: TKStrings );

		function FindGreaterId: Integer;
		var
			i,
			j: Integer;
		begin
			Result := -1;
			for i := 0 to sl.Count - 1 do
				if CheckTrimStr( sl.ValuesByIndex[i] ) then
				begin
					j := StrToIntDef( sl.ValuesByIndex[i], -1 );
					if ( j > Result ) then
						Result := j;
				end;
		end;

		procedure NormalizeStrings;
		var
			i,
			j: Integer;
		begin
			j := ( FindGreaterId + 1 );
			for i := 0 to sl.Count - 1 do
				if ( not CheckTrimStr( sl.ValuesByIndex[i] ) ) then
				begin
					sl.ValuesByIndex[i] := j;
					Inc( j );
				end;
		end;

		procedure InternalSort( l, r: Integer );
		var
			i,
			j,
			k,
			ki,
			kj: Integer;
		begin
			repeat
				i := l;
				j := r;
				k := StrToInt( ValuesByIndex[( l + r ) shr 1] );
				ki :=
				kj :=
				repeat
					while ( AnsiCompareText( si, s ) < 0 ) do
						Inc( i );
					while ( AnsiCompareText( sj, s ) > 0 ) do
						Dec( j );
					if ( i <= j ) then
					begin
						Exchange( i, j );
						Inc( i );
						Dec( j );
					end;
				until ( i > j );
				if ( l < j ) then
					QuickSort( l, j, SortType );
				l := i;
			until ( i >= r );

		end;

	begin
		NormalizeStrings;
		InternalSort( 0, sl.Count - 1 );
	end;
*)

end.
