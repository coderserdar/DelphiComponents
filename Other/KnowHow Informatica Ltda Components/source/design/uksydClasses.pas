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

unit uksydClasses;

{$I s:\v100\include\iKLIB100.inc}

{$BOOLEVAL OFF}

interface

uses
	Windows, SysUtils, Classes, {$IFDEF DELPHI4}ActiveX,{$ENDIF} Menus, Dialogs,
	TypInfo, DsgnIntf, VirtIntf, ToolIntf, EditIntf, FileIntf, IStreams, ExptIntf,
	ExtCtrls, uksydUtils, uksyClasses;

type

	EKSYDClasses = class( EKDSystem );

{
================================================================================
=========================== Open Tools API Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
----------------------- Open Tools API Stream Support --------------------------
--------------------------------------------------------------------------------
}

{ TKIEditReaderStream }

	TKIEditReaderStream = class( TStream )
	private
		fSize: LongInt;
		fPosition: LongInt;
		fReader: TIEditReader;
		function GetSize: LongInt;

	public
		destructor Destroy; override;
		constructor Create( EditIntf: TIEditorInterface );
		function Read( var Buffer; Count: LongInt ): LongInt; override;
		function Seek( Offset: LongInt; Origin: Word ): LongInt; override;
		function Write( const Buffer; Count: LongInt ): LongInt; override;
		property Size: LongInt
		 				 read GetSize;

	end;

{ TKIResEntryStream }

	TKIResEntryStream = class( TCustomMemoryStream )
	public
		constructor Create( Entry: TIResourceEntry );
		function Write( const Buffer; Count: LongInt ): LongInt; override;

	end;

{ TKIStringStream }

	TKIStringStream = class( TIStreamAdapter )
	private
		FMode: Word;
		FFileName: string;
		FOnFlush: TNotifyEvent;

	protected
		function GetStringStream: TKCustomStringStream;

	public
		constructor Create( AStringStream: TKCustomStringStream ); virtual;
		constructor CreateLinked( AFlushEvent: TNotifyEvent ); virtual;

		// No problems! Internal i define a new beheaviour for this function...
		procedure Flush; {$IFNDEF DELPHI4}override;{$ENDIF}
        
		{$IFDEF DELPHI4}
		function Write( pv: Pointer; cb: Longint; pcbWritten: PLongint ): HResult; override;
		{$ELSE}
		function Write( const Buffer; Count: Integer ): Integer; override;
		{$ENDIF}

		property FileName: string
						 read FFileName write FFileName;
		property Mode: Word
						 read FMode write FMode;
		property StringStream: TKCustomStringStream
						 read GetStringStream;
		property OnFlush: TNotifyEvent
						 read FOnFlush write FOnFlush;

	end;

	TKIStringStreamClass = class of TKIStringStream;

{
--------------------------------------------------------------------------------
---------------- Open Tools API Module/Project Creation Support ----------------
--------------------------------------------------------------------------------
}

	TKIExpert = class;
	TKIProjectExpert = class;

	TKIModuleCreator = class;

{ TKIModuleCreator }

	TKIModuleCreator = class( TIModuleCreator )
	private
		FExpert: TKIExpert;

	protected
		function GetUsesList: string; virtual; stdcall;
		function GetCustomModuleClassName: string; virtual; stdcall; 

		procedure FillModuleSource( StringStream: TKCustomStringStream;
			UnitName, Form, Ancestor: string ); virtual;

	public
		constructor Create( AnExpert: TKIExpert ); virtual;
		procedure Initialize; virtual;

		function Existing: Boolean; override;
		function GetAncestorName: string; override;
		function GetFileName: string; override;
		function GetFileSystem: string; override;
		function GetFormName: string; override;

		function NewModuleSource( {$IFDEF DELPHI4}const{$ENDIF}UnitName, Form,
		  Ancestor: string ): string; override;
		procedure FormCreated( Form: TIFormInterface ); override;

		property Expert: TKIExpert
						 read FExpert;

	end;

	TKIModuleCreatorClass = class of TKIModuleCreator;

	TKIProjectCreator = class;

{ TKIProjectCreator }

	TKIProjectCreator = class( TIProjectCreator )
	private
		FExpert: TKIProjectExpert;
		FIModuleCreatorClass: TKIModuleCreatorClass;
		FCreateModuleFlags: TCreateModuleFlags;

	protected
		function GetUsesList: string; virtual;
		procedure FillProjectSource( StringStream: TKCustomStringStream;
			ProjectName: string ); virtual;

	public
		constructor Create( AnExpert: TKIProjectExpert;
			AModuleCreatorClass: TKIModuleCreatorClass ); virtual;
		procedure Initialize; virtual;

		function Existing: Boolean; override;
		function GetFileName: string; override;
		function GetFileSystem: string; override;
		function NewProjectSource( const ProjectName: string ): string;	override;
		procedure NewDefaultModule; override;
		procedure NewProjectResource( Module: TIModuleInterface ); override;

		property Expert: TKIProjectExpert
						 read FExpert;
		property ModuleCreatorClass: TKIModuleCreatorClass
						 read FIModuleCreatorClass;
		property CreateModuleFlags: TCreateModuleFlags
						 read FCreateModuleFlags write FCreateModuleFlags;

	end;

	TKIProjectCreatorClass = class of TKIProjectCreator;

{
--------------------------------------------------------------------------------
------------------------- Open Tools API Expert Support ------------------------
--------------------------------------------------------------------------------
}

{ TKIExpert } 

	TKIExpert = class( TIExpert )
	private
		FName: string;
		FComment: string;
		FPage: string;
		FGlyph: HICON;
		FStyle: TExpertStyle;
		FState: TExpertState;
		FMenuText: string;
	public
		function GetName: string; override;
		function GetAuthor: string; override;
		function GetComment: string; override;
		function GetPage: string; override;
		function GetGlyph: HICON; override;
		function GetStyle: TExpertStyle; override;
		function GetState: TExpertState; override;
		function GetIDString: string; override;
		function GetMenuText: string; override;
		procedure Execute; override;

		constructor Create( Style: TExpertStyle );

		procedure Initialize; virtual;

		property Name: string
						 read GetName write FName;
		property Author: string
						 read GetAuthor;
		property Comment: string
						 read GetComment write FComment;
		property Page: string
						 read GetPage write FPage;
		property Glyph: HICON
						 read GetGlyph write FGlyph;
		property Style: TExpertStyle
						 read GetStyle;
		property State: TExpertState
						 read GetState write FState;
		property IDString: string
						 read GetIDString;
		property MenuText: string
						 read GetMenuText write FMenuText;

	end;

  TKIExpertClass = class of TKIExpert;

{ TKIAddInExpert }

	TKIAddInExpert = class( TKIExpert )
	private
		FMenuItem: TIMenuItemIntf;

		function GetCaption: string;
		procedure SetCaption( const Value: string );
		function GetHint: string;
		procedure SetHint( const Value: string );
		function GetShortCut: Integer;
		procedure SetShortCut( Value: Integer );
		function GetFlags: TIMenuFlags;
		procedure SetFlags( Mask: TIMenuFlags; Value: TIMenuFlags );

	protected
		procedure MenuClick( Sender: TIMenuItemIntf ); virtual; abstract;

		property MenuCaption: string
						 read GetCaption write SetCaption;
		property MenuHint: string
						 read GetHint write SetHint;
		property MenuShortCut: Integer
						 read GetShortCut write SetShortCut;
		property ViewMenuFlags: TIMenuFlags
						 read GetFlags;
		property ChangeMenuFlags[Mask: TIMenuFlags]: TIMenuFlags
						 write SetFlags;

	public
		constructor Create; virtual;
		destructor Destroy; override;

		procedure InstallExpert( Action: TKInsertAction; const TargetName, Caption,
			Name, Hint: string; ShortCut, Context, GroupIndex: Integer;
			Flags: TIMenuFlags );

	end;

	TKAddInExpertClass = class of TKIAddInExpert;

{ TKIFormExpert }

	TKIFormExpert = class( TKIExpert )
	private
		FIModuleCreatorClass: TKIModuleCreatorClass;
		FCreateModuleFlags: TCreateModuleFlags;

	public
		constructor Create( AModuleCreatorClass: TKIModuleCreatorClass ); virtual;
		procedure Initialize; override;
		procedure Execute; override;

		property ModuleCreatorClass: TKIModuleCreatorClass
						 read FIModuleCreatorClass;
		property CreateModuleFlags: TCreateModuleFlags
						 read FCreateModuleFlags write FCreateModuleFlags;

{ must set: Name, Comment, Page and Glyph properties... }
	end;

	TKIFormExpertClass = class of TKIFormExpert;

{ TKIProjectExpert }

	TKIProjectExpert = class( TKIExpert )
	private
		FIProjectCreatorClass: TKIProjectCreatorClass;
		FIModuleCreatorClass: TKIModuleCreatorClass;
		FCreateModuleFlags: TCreateModuleFlags;
		FCreateProjectFlags: TCreateProjectFlags;

	public
		constructor Create( AProjectCreatorClass: TKIProjectCreatorClass;
			AModuleCreatorClass: TKIModuleCreatorClass ); virtual;
		procedure Initialize; override;
		procedure Execute; override;

		property ProjectCreatorClass: TKIProjectCreatorClass
						 read FIProjectCreatorClass;
		property CreateProjectFlags: TCreateProjectFlags
						 read FCreateProjectFlags write FCreateProjectFlags;
		property ModuleCreatorClass: TKIModuleCreatorClass
						 read FIModuleCreatorClass;
		property CreateModuleFlags: TCreateModuleFlags
						 read FCreateModuleFlags write FCreateModuleFlags;

{ must set: Name, Comment, Page and Glyph properties... }
	end;

	TKIProjectExpertClass = class of TKIProjectExpert;

{ TKIStandardExpert }

	TKIStandardExpert = class( TKIExpert )
	public
		constructor Create( const AnExpertName: string ); virtual;

		{ must set: Name, MenuText and State properties.
			must override: Execute Method ( equal to HelpMenuItem click )
			must override: Initialize Method and set the properties above

			PS: AState is a set indicating enabled and/or checked state }
	end;

	TKIStandardExpertClass = class of TKIStandardExpert;

{##NI##}
{ DLL Expert registration Support }

var
	GetKExpertInstanceProc: function: TKIExpert = nil;
	TerminateExpertProc: procedure far = nil;

function InitExpert( AToolServices: TIToolServices; RegisterProc: TExpertRegisterProc;
	var Terminate: TExpertTerminateProc ): Boolean; stdcall;
{##NI##}

{##RI##}
{
--------------------------------------------------------------------------------
------------------ Open Tools API Virtual File System Support ------------------
--------------------------------------------------------------------------------
}

type

	EKVirtualFileSystem = class( EKSYDClasses );

{ TKIVirtualFileSystem }

	TKIVirtualFileSystem = class( TIVirtualFileSystem )
	private
		FAge: LongInt;
		FLastMode: LongInt;
		FModules: TStrings;
		FStreamClass: TKIStringStreamClass;

		function GetModules( ModIndex: Integer ): string;
		function GetObjects( ModIndex: Integer ): TPersistent;
		function GetPropInfo( ModIndex: Integer ): PPropInfo;
		function GetOriginalFileName( ModIndex: Integer ): string;

	protected
		class function GetDefaultNamePrefix: string; dynamic;
		class function GetFileSystemName: string; dynamic;
		procedure FlushEvent( Sender: TObject ); dynamic;

		procedure SetPropValue( PropValue: Pointer; Stream: TKIStringStream;
			ModIndex: Integer ); virtual;

		procedure MarkModified( Stream: TKIStringStream; ModIndex: Integer ); virtual; abstract;
		procedure GetPropValue( var PropValue; Count: Integer; Stream: TKIStringStream;
			ModIndex: Integer ); virtual; abstract;
		function GetPropValueSize( Stream: TKIStringStream;
			ModIndex: Integer ): Integer; virtual;

		constructor Create( AStreamClass: TKIStringStreamClass ); virtual;

		property Modules[ModIndex: Integer]: string
						 read GetModules;
		property Objects[ModIndex: Integer]: TPersistent
						 read GetObjects;
		property PropInfo[ModIndex: Integer]: PPropInfo
						 read GetPropInfo;
		property OriginalFileName[ModIndex: Integer]: string
						 read GetOriginalFileName;

	public
		procedure Initialize; virtual; stdcall;
		function GetFileStream( const AFileName: TFileName; AMode: Integer ):
			{$IFDEF DELPHI4}IStream{$ELSE}TIStream{$ENDIF}; override;
		function FileAge( const FileName: TFileName ): Longint; override;
		function RenameFile( const OldName, NewName: TFileName ): Boolean; override;
		function IsReadonly( const FileName: TFileName ): Boolean; override;
		function IsFileBased: Boolean; override;
		function DeleteFile( const FileName: TFileName ): Boolean; override;
		function FileExists( const FileName: TFileName ): Boolean; override;
		function GetTempFileName( const FileName: TFileName ): TFileName; override;
		function GetBackupFileName( const FileName: TFileName ): TFileName; override;
		function GetIDString: string; override;

		property StreamClass: TKIStringStreamClass
						 read FStreamClass;

	end;

	TKIVirtualFileSystemClass = class of TKIVirtualFileSystem;

	TKIStringVFS = class( TKIVirtualFileSystem )
	protected
		procedure MarkModified( Stream: TKIStringStream; ModIndex: Integer ); override;
		function GetPropValueSize( Stream: TKIStringStream;
			ModIndex: Integer ): Integer; override;
		procedure GetPropValue( var PropValue; Count: Integer; Stream: TKIStringStream;
			ModIndex: Integer ); override;

	end;

	TKITStringsVFS = class( TKIStringVFS )
	protected
		procedure SetPropValue( PropValue: Pointer; Stream: TKIStringStream;
			ModIndex: Integer ); override;

	end;

{ Automatic File System Registering/Creation/Management Support }

function CheckVFSPropInfoKind( Source: TPersistent; ppi: PPropInfo ): Boolean;
procedure ForceVFSPropInfoKind( Source: TPersistent; ppi: PPropInfo );

function RegisterFileSystem( AVFSClass: TKIVirtualFileSystemClass;
	AStreamClass: TKIStringStreamClass ): Boolean;
function UnregisterFileSystem( AVFSClass: TKIVirtualFileSystemClass ): Boolean;

function IsFileSystemRegistered( AVFSClass: TKIVirtualFileSystemClass ): Boolean;
function GetFileSystemName( AVFSClass: TKIVirtualFileSystemClass ): string;

function RegisterModuleForVFS( AVFSClass: TKIVirtualFileSystemClass;
	const OriginalFileName, VFSModuleName: string; AObject: TPersistent;
	APropInfo: PPropInfo ): Boolean;
function ModuleRegisteredForVFS( AVFSClass: TKIVirtualFileSystemClass;
	const ModuleName: string ): Boolean;
{##RI##}
{
================================================================================
========================== Property Editor Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
---------------------------- TKColorProperty Editor ----------------------------
--------------------------------------------------------------------------------
}

type

{ TKColorProperty }

	TKColorProperty = class( TColorProperty )
	public
		function GetValue: string; override;
		procedure GetValues( proc: TGetStrProc ); override;
		procedure SetValue( const Value: string ); override;

	end;

{
--------------------------------------------------------------------------------
------------------------- TKLongStringProperty Editors -------------------------
--------------------------------------------------------------------------------
}

{ TKLongStringProperty }

	TKLongStringProperty = class( TStringProperty )
	private
		FVFSFileName: string;
		function GetVFSFileName: string;

	protected
		function GetSyntaxHighLighter: TSyntaxHighlighter; dynamic;
		procedure SetEditorValue( Module: TIModuleInterface; Editor: TIEditorInterface;
			Data: Pointer ); dynamic;
		procedure AdjustVFSFileName; dynamic;

		property VFSFileName: string
						 read GetVFSFileName;

	public
		procedure Initialize; override;
		procedure Edit; override;
		procedure SetValue( const Value: string ); override;
		function GetAttributes: TPropertyAttributes; override;

	end;

{ TKLongCaptionProperty }

	TKLongCaptionProperty = class( TKLongStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{
--------------------------------------------------------------------------------
------------------------ TKDialogClassProperty Editor --------------------------
--------------------------------------------------------------------------------
}

{ TKDialogClassProperty }

	TKDialogClassProperty = class( TClassProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{
--------------------------------------------------------------------------------
----------------------- TKCustomGradientProperty Editor ------------------------
--------------------------------------------------------------------------------
}

{ TKCustomGradientProperty }

	TKCustomGradientProperty = class( TClassProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

{
--------------------------------------------------------------------------------
----------------------------- TKGradientStyleProperty --------------------------
--------------------------------------------------------------------------------
}

	{ TKUnSortedEnumProperty }
	
	TKUnSortedEnumProperty = class( TEnumProperty )
	public
		function GetAttributes: TPropertyAttributes; override;

	end;

	{ TKThreadPriorityProperty }

	TKThreadPriorityProperty = class( TKUnSortedEnumProperty )
	public
		procedure GetValues( Proc: TGetStrProc ); override;

	end;


{ TKGradientStyleProperty }

	TKGradientStyleProperty = class( TKUnSortedEnumProperty );

{
--------------------------------------------------------------------------------
------------------------------- TKTStringsProperty -----------------------------
--------------------------------------------------------------------------------
}

{ TKTStringsProperty }

	TKTStringsProperty = class( TKDialogClassProperty )
	private
		FVFSFileName: string;
		function GetVFSFileName: string;

	protected
		function GetSyntaxHighLighter: TSyntaxHighlighter; dynamic;
		procedure AdjustVFSFileName; dynamic;

		property VFSFileName: string
						 read GetVFSFileName;

	public
		procedure Initialize; override;
		procedure Edit; override;

	end;

{ TKSQLStringsProperty }

	TKSQLStringsProperty = class( TKTStringsProperty )
	public
		function GetSyntaxHighLighter: TSyntaxHighlighter; override;

	end;

{ TKPascalStringsProperty }

	TKPascalStringsProperty = class( TKTStringsProperty )
	public
		function GetSyntaxHighLighter: TSyntaxHighlighter; override;

	end;


{
--------------------------------------------------------------------------------
------------------------- TKFileNameProperty Editors ---------------------------
--------------------------------------------------------------------------------
}

{ TKCustomFileNameProperty }

	TKCustomFileNameProperty = class( TStringProperty )
	protected
		function IsReadOnly: Boolean; virtual;
		function GetFileDlgOptions: TOpenOptions; virtual;
		function GetTitle: string; virtual;
		function GetInitDir: string; virtual;

	public
		function GetAttributes: TPropertyAttributes; override;

		property InitDir: string
						 read GetInitDir;
		property Title: string
						 read GetTitle;

	end;

{ TKFileNameProperty }

	TKFileNameProperty = class( TKCustomFileNameProperty )
	protected
		function GetFileDlgOptions: TOpenOptions; override;

		function GetFilter: string; virtual;
		function GetDefExt: string; virtual;

	public
		procedure Edit; override;

		property Filter: string
						 read GetFilter;
		property DefExt: string
						 read GetDefExt;

	end;

{ TKReadOnlyFileNameProperty }

	TKReadOnlyFileNameProperty = class( TKFileNameProperty )
	{$IFDEF DELPHI4}
	protected
	{$ELSE}
	private
	{$ENDIF}
		function IsReadOnly: Boolean; override;

	end;

{ TKDirectoryProperty }

	TKDirectoryProperty = class( TKCustomFileNameProperty )
	protected
		function IsReadOnly: Boolean; override;
		function GetRoot: string; virtual;

	public
		procedure Edit; override;

		property InitDir;
		property Title;
		property Root: string
						 read GetRoot;

	end;

{ TKReadOnlyDirectoryProperty }

	TKReadOnlyDirectoryProperty = class( TKDirectoryProperty )
	protected
		function IsReadOnly: Boolean; override;

	end;

{ TKFindFirstProperty }

	TKFindFirstProperty = class( TKFileNameProperty )
	protected
		procedure ProcessValue( Proc: TGetStrProc; const Search: TSearchRec ); virtual;
		function GetSearchPath: string; virtual;

	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

		property SearchPath: string
						 read GetSearchPath;

	end;

{
--------------------------------------------------------------------------------
--------------------------- TKStringsProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

{ TKStringsProperty }

	TKStringsProperty = class( TStringProperty )
	protected
		procedure GetValueList( List: TStrings ); virtual; abstract;
		function GetOwner: TPersistent;
		function IsSorted: Boolean; virtual;
		function IsReadOnly: Boolean; virtual;

		property Owner: TPersistent
						 read GetOwner;

	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKTypedStringsProperty }

	TKTypedStringsProperty = class( TKStringsProperty )
	protected
		procedure GetValueList( List: TStrings ); override;

		function GetPropertyName: string; virtual; abstract;
		function GetPropertyTypeKind: TTypeKind; virtual; abstract;

		procedure ProcessValueList( Obj: TPersistent; List: TStrings );
			virtual; abstract;

	end;

{
--------------------------------------------------------------------------------
-------------------------- TKCompListProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

{ TKCompListProperty }

	TKCompListProperty = class( TComponentProperty )
	protected
		procedure GetValueList( List: TStrings ); virtual; abstract;
		function GetOwner: TPersistent;
		function IsReadOnly: Boolean; virtual;

		property Owner: TPersistent
						 read GetOwner;

	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKTypedCompListProperty }

	TKTypedCompListProperty = class( TKCompListProperty )
	protected
		procedure GetValueList( List: TStrings ); override;

		function GetPropertyName: string; virtual; abstract;
		procedure ProcessValueList( Obj: TPersistent; List: TStrings );
			virtual; abstract;

	end;

{
--------------------------------------------------------------------------------
------------------------ TKComponentProperty Editors ---------------------------
--------------------------------------------------------------------------------
}

{ TKCompCheckProperty }

	TKCompCheckProperty = class( TComponentProperty )
	private
		FCheckProc: TGetStrProc;

	protected
		procedure CheckComponent( const Value: string ); virtual;
		function ProcessValue( GetComp_i: TPersistent; BaseComp: TComponent;
			const Value: string ): Boolean; virtual; abstract;

		property CheckProc: TGetStrProc
						 read FCheckProc write FCheckProc;

	public
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKCompNameProperty }

	TKCompNameProperty = class( TKCompCheckProperty )
	protected
		function GetFilterClass: TComponentClass; virtual; abstract;

		function ProcessDesigner: Boolean; virtual;
		function ProcessValue( GetComp_i: TPersistent; BaseComp: TComponent;
			const Value: string ): Boolean; override;

	public
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{
--------------------------------------------------------------------------------
----------------------------- TKShortCutProperty Editor ------------------------
--------------------------------------------------------------------------------
}

{ TKShortCutProperty }

	TKShortCutProperty = class( TShortCutProperty )
	public
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

procedure	RegisterShortCuts( const ShortCuts: array of TShortCut );

{
--------------------------------------------------------------------------------
----------------------------- TKFloatProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

type

{ TKFloatProperty }

	TKFloatProperty = class( TFloatProperty )
	private
		FFloatFormat: string;

	protected
		function GetFloatFormat: string; virtual;

		property FloatFormat: string
						 read GetFloatFormat;

	public
		function GetValue: string; override;
		procedure SetValue( const Value: string ); override;

	end;

{ TKMMFloatProperty }

	TKMMFloatProperty = class( TKFloatProperty )
	public
		function GetFloatFormat: string; override;

	end;

{
--------------------------------------------------------------------------------
---------------------------- TKIntegerProperty Editor --------------------------
--------------------------------------------------------------------------------
}

type

{ TKIntegerProperty }

	TKIntegerProperty = class( TIntegerProperty )
	protected
		function GetIntegerFormat: string; virtual;

		property IntegerFormat: string
						 read GetIntegerFormat;

	public
		function GetValue: string; override;
		procedure SetValue( const Value: string ); override;

	end;

{
--------------------------------------------------------------------------------
------------------------------ TKHexaProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

{ TKHexaProperty }

	TKHexaProperty = class( TIntegerProperty )
	protected
		function GetHexaDigits: Integer; virtual;

	public
		function GetValue: string; override;
		procedure SetValue( const Value: string ); override;

	end;

{
--------------------------------------------------------------------------------
--------------------------- DateTime Property Editors --------------------------
--------------------------------------------------------------------------------
}

{ TKDateTimeProperty }

	TKDateTimeProperty = class( TPropertyEditor )
	protected
		function GetDateTimeFormat: string; virtual;
		procedure GetDateProperties( Proc: TGetPropEditProc ); virtual;
		procedure GetTimeProperties( Proc: TGetPropEditProc ); virtual;

	public
		function GetValue: string; override;
		procedure SetValue( const Value: string ); override;
		function GetAttributes: TPropertyAttributes; override;
		procedure GetProperties( Proc: TGetPropEditProc ); override;

	end;

{ TKDateProperty }

	TKDateProperty = class( TKDateTimeProperty )
	protected
		function GetDateTimeFormat: string; override;
		procedure GetTimeProperties( Proc: TGetPropEditProc ); override;

	public
		procedure SetValue( const Value: string ); override;

	end;

{ TKTimeProperty }

	TKTimeProperty = class( TKDateTimeProperty )
	protected
		function GetDateTimeFormat: string; override;
		procedure GetDateProperties( Proc: TGetPropEditProc ); override;

	public
		procedure SetValue( const Value: string ); override;
		
	end;

{##RI##}
{
--------------------------------------------------------------------------------
--------------------------- TKMethodProperty Editor ----------------------------
--------------------------------------------------------------------------------
}

  EKMethodProperty = class( EKPropertyEditor );

{ TKCustomMethodProperty }

	TKCustomMethodProperty = class( TMethodProperty )
	protected
		function GetCommentString: string; virtual;

		property CommentString: string
						 read GetCommentString;

	end;

	TKMethodPropertyClass = class of TKCustomMethodProperty;

{ TKMethodComment }

	TKMethodProperty = class;

	TKMethodComment = class( TObject )
	private
		FStream: TKIEditReaderStream;
		fMethodData: PTypeData;
		fProp: TKMethodProperty;
		fMethodName: string;
		fMethod: TMethod;

	protected
		procedure MakeComment( Module: TIModuleInterface;
			Editor: TIEditorInterface; Data: Pointer ); virtual; 

	public
		constructor Create( PropEd: TKMethodProperty ); virtual;
		destructor Destroy; override;
		procedure CommentMethod( const MethodName: string; Method: TMethod;
			MethodData: PTypeData ); virtual;

		property CommentedMethod: TMethod
						 read FMethod;
		property CommentedMethodName: string
						 read FMethodName;
		property CommentedMethodData: PTypeData
						 read FMethodData;

	end;

{ TKMethodProperty }

	TKMethodProperty = class( TKCustomMethodProperty )
	private
		fMethodComment: TKMethodComment;

	public
		destructor Destroy; override;
		procedure Initialize; override;
		procedure Edit; override;

	end;

{
--------------------------------------------------------------------------------
------------------------------- TKMethodExpert ---------------------------------
--------------------------------------------------------------------------------
}

	TKIMethodExpert = class( TKIAddInExpert )
	private
		function GetStateFlags: TIMenuFlags;
		 
	protected
		procedure AdjustRegistry( Enable: Boolean ); dynamic;

	public
		procedure Initialize; override;
		procedure MenuClick( Sender: TIMenuItemIntf ); override;
		
	end;
{##RI##}

{
--------------------------------------------------------------------------------
------------------------ RegisterMethodComment Support -------------------------
--------------------------------------------------------------------------------
}

type

	TKRegisterMethodCommentProc = procedure( pmti: PTypeInfo;
		AClass: TPersistentClass; const AName, AComment: string;
		AEditorClass: TKMethodPropertyClass );

procedure RegisterMethodInfo( mpti: PTypeInfo; AClass: TPersistentClass;
	const AName, AComment: string );

procedure RegisterMethod( mti: PTypeInfo; MethodObjClass: TPersistentClass;
	const MethodName, MethodComment: string; MethodEditorClass: TKMethodPropertyClass );

procedure RegisterDefaultMethod( mti: PTypeInfo; MethodObjClass: TPersistentClass;
	const MethodName, MethodComment: string );

function MethodCommentsEnabled: Boolean;

procedure RegisterMethodShareWare( const Args: array of PTypeInfo );

{##NI##}
var
	RegisterMethodCommentProc: TKRegisterMethodCommentProc = nil;
{##NI##}

{
================================================================================
========================= Component Editor Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
------------------------------- TKDefultEditor ---------------------------------
--------------------------------------------------------------------------------
}

type

{ TKDefaultEditor }

	TKDefaultEditor = class( TDefaultEditor )
	protected
		procedure EditProperty( PropertyEditor: TPropertyEditor;
			var Continue, FreeEditor: Boolean ); override;

		function GetComparePropName: string; virtual; abstract;

	end;

{
================================================================================
=========================== Custom Module Support ==============================
================================================================================
}

{
--------------------------------------------------------------------------------
------------------------------- TKCustomModule ---------------------------------
--------------------------------------------------------------------------------
}

{ TKCustomModule }

	TKCustomModule = class( TCustomModule )
	private
		FCustomModuleAttributes: TCustomModuleAttributes;

	protected
		{
			Returns 0 for Form based CustomModule (Root is TCustomForm descendent)
			Returns 1 for Component based CustomModule (Root is TComponent descendent)
			Returns -1 in any another case (No designer available, GetDesigner will return nil)
		}
		function CheckDesigner: Integer; dynamic;
		function GetDesigner: TKFormDesigner; virtual;
    function GetRootComponent: TComponent; virtual;

		property Designer: TKFormDesigner
						 read GetDesigner;

	public
		constructor Create( ARoot: {$IFDEF DELPHI4}IComponent{$ELSE}TComponent{$ENDIF} ); override;
		function GetAttributes: TCustomModuleAttributes; override;
		property RootComponent: TComponent
						 read GetRootComponent;

	end;

const
	NO_DESIGNER   = -1;
	FORM_DESIGNER =  0;
	COMP_DESIGNER =  1;

implementation

uses
	Consts, Graphics, Forms, Controls, Registry, uksyTypes, uksyConsts, uksyResStr,
	uksyShortCuts, uksyPackReg, uksyUtils, uksydConsts, uksydInternal, uksydfStrEdit;

{
================================================================================
=========================== Open Tools API Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
----------------------- Open Tools API Stream Support --------------------------
--------------------------------------------------------------------------------
}

{ TKIEditReaderStream }
	
constructor TKIEditReaderStream.Create( EditIntf: TIEditorInterface );
begin
	ForceReference( EditIntf );
	inherited Create;
	fReader := EditIntf.CreateReader;
	fSize := -1; { size is unknown at construction }
end;

destructor TKIEditReaderStream.Destroy;
begin
	fReader.Free;
	inherited Destroy;
end;

function TKIEditReaderStream.Read( var Buffer; Count: LongInt ): LongInt;
const
	MaxCount = ( 31 * KB );
var
	NRead: Integer;
	NRequest: Integer;
	BufPtr: PChar;
begin
	Result := 0;
	BufPtr := @Buffer;
	while ( Count > 0 ) do
	begin
{ Buffered read by 31 KB size }
		if Count > MaxCount then
			NRequest := MaxCount
		else
			NRequest := Count;
		NRead := fReader.GetText( fPosition, BufPtr, NRequest );
		Inc( fPosition, NRead );
		Inc( BufPtr, NRead );
		Inc( Result, NRead );
		Dec( Count, NRead );
{ Readjust the size if it does not was computed before the first reading! }
		if ( fSize < 0 ) and ( NRead > 0 ) and ( NRead < NRequest ) then
			fSize := fPosition;
	end;
end;

function TKIEditReaderStream.Seek( Offset: LongInt; Origin: Word ): LongInt;
begin
	case Origin of
		soFromBeginning: fPosition := Offset;
		soFromCurrent  : fPosition := fPosition + Offset;
		soFromEnd      : fPosition := Size + Offset;
	else
		RaiseExceptionFmt( EKOTA, sErrInvStreamSeekOrigin, [Origin] );
	end;
	Result := fPosition;
end;

function TKIEditReaderStream.Write( const Buffer; Count: LongInt ): LongInt;
begin
	RaiseException( EKOTA, sErrInvStreamWrite );
	Result := -1; { To aviod package warning! Value will never be setted }
end;

function TKIEditReaderStream.GetSize: LongInt;
const
	KB_4 = ( 4 * KB );
var
	iRead: LongInt;
	ch: array[0..KB_4 - 1] of char;
begin
	if ( fSize < 0 ) then
	begin
		fSize := 0;
		repeat
			iRead := fReader.GetText( fSize, @ch[0], KB_4 );
			Inc( fSize, iRead );
		until ( iRead <> KB_4 );
	end;
	Result := fSize;
end;

{ TKIResEntryStream }

constructor TKIResEntryStream.Create( Entry: TIResourceEntry );
begin
	ForceObject( Entry );
	inherited Create;
	SetPointer( Entry.GetData, Entry.GetDataSize );
end;

function TKIResEntryStream.Write( const Buffer; Count: LongInt ): LongInt;
begin
	ForceReference( Buffer );
	if ( ( Position + Count ) > Size ) then
		Count := ( Size - Position );
	Move( Buffer, PChar( Memory )[Position], Count );
	Position := ( Position + Count );
	Result := Count;
end;

{ TKIStringStream }

constructor TKIStringStream.Create( AStringStream: TKCustomStringStream );
begin
	{$IFDEF DELPHI4}
		StreamOwnership := TStreamOwnership( not CheckObject( AStringStream ) );
	{$ELSE}
		OwnStream := ( not CheckObject( AStringStream ) );
	{$ENDIF}
	if ( not CheckObject( AStringStream ) ) then
		AStringStream := TKCustomStringStream.Create( '' );
	inherited Create( AStringStream, {$IFDEF DELPHI4}StreamOwnership{$ELSE}OwnStream{$ENDIF} );
end;

constructor TKIStringStream.CreateLinked( AFlushEvent: TNotifyEvent );
begin
	Create( nil );
	FOnFlush := AFlushEvent;
end;

function TKIStringStream.GetStringStream: TKCustomStringStream;
begin
{ For delphi 3 this stream is a protected member of a ancestor class
	TIVclStreamAdapter.FStream! (TKIStringStream->TIStreamAdapter->TIVclStreamAdapter).
	For delphi 4 this stream is a public member of a ancestor class
	TStreamAdapter (that is a COM Implementation interface for IStream)!}
	Result := TKCustomStringStream( Stream );
end;

{$IFDEF DELPHI4}
{
function TKIStringStream.Write( pv: Pointer; cb: Longint; pcbWritten: PLongint ): HResult;

Do not replace the declaration bellow for the declaration above during a
posible compiler error!
}
function TKIStringStream.Write;
begin
	Result := inherited Write( pv, cb, pcbWritten );
	Flush;
end;
{$ELSE}
function TKIStringStream.Write( const Buffer; Count: Integer ): Integer;
begin
	Result := inherited Write( Buffer, Count );
	Flush;
end;
{$ENDIF}

procedure TKIStringStream.Flush;
begin
{$IFNDEF DELPHI4}
  inherited Flush;
{$ENDIF}
	if Assigned( FOnFlush ) then
		FOnFlush( Self );
end;

{
--------------------------------------------------------------------------------
---------------- Open Tools API Module/Project Creation Support ----------------
--------------------------------------------------------------------------------
}

{ TKIModuleCreator }

constructor TKIModuleCreator.Create( AnExpert: TKIExpert );
begin
	ForceObject( AnExpert );
  inherited Create;
  FExpert := AnExpert;
  Initialize;
end;

procedure TKIModuleCreator.Initialize;
begin
  { do nothing here... }
end;

function TKIModuleCreator.Existing: Boolean;
begin
	Result := False;
end;

function TKIModuleCreator.GetAncestorName: string;
begin
	Result := GetCustomModuleClassName;
  if ( ( CheckTrimStr( Result ) ) and ( Length( Result ) > 2 ) ) then
    Result := Copy( Result, 2, Length( Result ) );
end;

function TKIModuleCreator.GetFileName: string;
begin
	Result := '';
end;

function TKIModuleCreator.GetFileSystem: string;
begin
	Result := '';
end;

function TKIModuleCreator.GetFormName: string;
begin
	Result := '';
end;

function TKIModuleCreator.GetCustomModuleClassName: string;
begin
  Result := '';
end;

function TKIModuleCreator.GetUsesList: string;
begin
	Result := DEFAULT_MODULECREATOR_USESLIS;
end;

procedure TKIModuleCreator.FillModuleSource( StringStream: TKCustomStringStream;
	UnitName, Form, Ancestor: string );
begin
	with StringStream do
	begin
		FormatLn( 'unit %s;', [UnitName] );
		NewLine;
		WriteLn( 'interface' );
		NewLine;
		WriteLn( 'uses' );
		WriteLn( GetUsesList );
		NewLine;
		WriteLn( 'type' );
		FormatLn( '  T%s = class( T%s )', [Form, Ancestor] );
		WriteLn( '  private' );
		WriteLn( '    { private declarations }' );
		WriteLn( '  public' );
		WriteLn( '    { public declarations }' );
		WriteLn( '  end;' );
		NewLine;
		WriteLn( 'var' );
		FormatLn( '  %0:s: T%0:s;', [Form] );
		NewLine;
		WriteLn( 'implementation' );
		NewLine;
		WriteLn( ' {$R *.DFM} ' );
		NewLine;
		WriteLn( 'end.' );
	end;
end;


function TKIModuleCreator.NewModuleSource( {$IFDEF DELPHI4}const{$ENDIF}UnitName,
	Form, Ancestor: string ): string; 
var
	ss: TKCustomStringStream;
begin
	ss := TKCustomStringStream.Create( '' );
	with ss do
		try
			if IsSystem_ShareWare then
			begin
				WriteLn( sExpertsShareWare );
				NewLine;
      end;
			try
				FillModuleSource( ss, UnitName, Form, Ancestor );
			finally
				if ( ( not CheckStrContains( sExpertsShareWare, DataString ) ) and IsSystem_ShareWare ) then
				begin
					Position := 0;
					WriteLn( sExpertsShareWare );
					NewLine;
  			end;
				Result := DataString;
			end;
		finally
			Free;
		end;
end;

procedure TKIModuleCreator.FormCreated( Form: TIFormInterface );
begin
	Form.Free;
end;

{ TKIProjectCreator }

constructor TKIProjectCreator.Create( AnExpert: TKIProjectExpert;
  AModuleCreatorClass: TKIModuleCreatorClass );
begin
  ForceObject( AnExpert );
  inherited Create;
  FExpert := AnExpert;
  FIModuleCreatorClass := AModuleCreatorClass;
  FCreateModuleFlags := DEFAULT_MODULE_CREATION_FLAGS;
  Initialize;
end;

procedure TKIProjectCreator.Initialize;
begin
  { do nothing here... }
end;

function TKIProjectCreator.Existing: Boolean;
begin
	Result := False;
end;

function TKIProjectCreator.GetFileName: string;
begin
	Result := '';
end;

function TKIProjectCreator.GetFileSystem: string;
begin
	Result := '';
end;

function TKIProjectCreator.GetUsesList: string;
begin
	Result := '  Forms;';
end;

procedure TKIProjectCreator.FillProjectSource( StringStream: TKCustomStringStream;
  ProjectName: string );
begin
	with StringStream do
	begin
		FormatLn( 'program %s;', [ProjectName] );
		NewLine;
		WriteLn( 'uses' );
		WriteLn( GetUsesList );
		NewLine;
		WriteLn( '{$R *.RES}' );
		NewLine;
		WriteLn( 'begin' );
		WriteLn( '  Application.Initialize;' );
		WriteLn( '  Application.Run;' );
		WriteLn( 'end.' );
	end;	
end;

function TKIProjectCreator.NewProjectSource( const ProjectName: string ): string;
var
	ss: TKCustomStringStream;
begin
	ss := TKCustomStringStream.Create( '' );
	with ss do
		try
			if IsSystem_ShareWare then
			begin
				WriteLn( sExpertsShareWare );
				NewLine;
			end;
			try
				FillProjectSource( ss, ProjectName );
			finally
				if ( ( not CheckStrContains( sExpertsShareWare, DataString ) ) and IsSystem_ShareWare ) then
				begin
					Position := 0;
					WriteLn( sExpertsShareWare );
					NewLine;
				end;
				Result := DataString;
			end;
		finally
			Free;
		end;
end;

procedure TKIProjectCreator.NewDefaultModule;
var
  ModIntf: TIModuleInterface;
	ModCreator: TKIModuleCreator;
begin
  if CheckClass( ModuleCreatorClass ) then
  begin
    ModCreator := ModuleCreatorClass.Create( Expert );
    try
			ModIntf := ToolServices.ModuleCreate( ModCreator, CreateModuleFlags );
      if CheckObject( ModIntf ) then
        ModIntf.Free;
    finally
      ModCreator.Free;
    end;
	end;
end;

procedure TKIProjectCreator.NewProjectResource( Module: TIModuleInterface );
begin
	Module.Free;
end;

{
--------------------------------------------------------------------------------
------------------------- Open Tools API Expert Support ------------------------
--------------------------------------------------------------------------------
}

{ TKIExpert }

constructor TKIExpert.Create( Style: TExpertStyle );
begin
	inherited Create;
	FName := '';
	FStyle := Style;
	FPage := sKnowHowPage;
	Initialize;
end;

procedure TKIExpert.Initialize;
begin
	FPage := sKnowHowPage;
end;

function TKIExpert.GetName: string;
begin
	Result := GetFirstString( [FName, GetUniqueFileName( sKnowHowPage )] );
end;

function TKIExpert.GetAuthor: string;
begin
	Result := sAuthors;
end;

function TKIExpert.GetComment: string;
begin
	Result := FComment;
end;

function TKIExpert.GetPage: string;
begin
	Result := FPage;
end;

function TKIExpert.GetGlyph: HICON;
begin
	Result := FGlyph;
end;

function TKIExpert.GetStyle: TExpertStyle;
begin
	Result := FStyle;
end;

function TKIExpert.GetState: TExpertState;
begin
	Result := FState;
end;

function TKIExpert.GetIDString: string;
begin
	Result := sKnowHowPage + CH_DOTMARK + GetName;
end;

function TKIExpert.GetMenuText: string;
begin
	Result := FMenuText;
end;

procedure TKIExpert.Execute;
begin
	{ do nothing... }
end;

{ TKIAddInExpert }

constructor TKIAddInExpert.Create;
begin
	inherited Create( esAddIn );
end;

destructor TKIAddInExpert.Destroy;
begin
	FreeClean( FMenuItem );
	inherited Destroy;
end;

procedure TKIAddInExpert.InstallExpert( Action: TKInsertAction; const TargetName,
	Caption, Name, Hint: string; ShortCut, Context, GroupIndex: Integer; Flags: TIMenuFlags );
begin
  ForceTrimStrs( [Caption, Name, TargetName] ); 
	if CheckObject( FMenuItem ) then
		RaiseException( EKOTA, sErrAddInExptInvMenuItem );
	if ( Action <> iaNone ) then
		FMenuItem := uksydUtils.InsertMenuItem( Action, TargetName, Caption, sKnowHowPage +
			CH_UNDERSCORE + Name, Hint, ShortCut, Context, GroupIndex, Flags, MenuClick );
end;

function TKIAddInExpert.GetCaption: string;
begin
	ForceObject( FMenuItem );
	Result := FMenuItem.GetCaption;
end;

procedure TKIAddInExpert.SetCaption( const Value: string );
begin
	ForceObject( FMenuItem );
	FMenuItem.SetCaption( Value );
end;

function TKIAddInExpert.GetHint: string;
begin
	ForceObject( FMenuItem );
	Result := FMenuItem.GetHint;
end;

procedure TKIAddInExpert.SetHint( const Value: string );
begin
	ForceObject( FMenuItem );
	FMenuItem.SetHint( Value );
end;

function TKIAddInExpert.GetShortCut: Integer;
begin
	ForceObject( FMenuItem );
	Result := FMenuItem.GetShortCut;
end;

procedure TKIAddInExpert.SetShortCut( Value: Integer );
begin
	ForceObject( FMenuItem );
	FMenuItem.SetShortCut( Value );
end;

function TKIAddInExpert.GetFlags: TIMenuFlags;
begin
	ForceObject( FMenuItem );
	Result := FMenuItem.GetFlags;
end;

procedure TKIAddInExpert.SetFlags( Mask: TIMenuFlags; Value: TIMenuFlags );
begin
	ForceObject( FMenuItem );
	FMenuItem.SetFlags( Mask, Value );
end;

{ TKIFormExpert }

constructor TKIFormExpert.Create( AModuleCreatorClass: TKIModuleCreatorClass );
begin
	ForceClass( AModuleCreatorClass );
	FIModuleCreatorClass := AModuleCreatorClass;
	inherited Create( esForm );
end;

procedure TKIFormExpert.Initialize;
begin
	inherited Initialize;
	FCreateModuleFlags := DEFAULT_MODULE_CREATION_FLAGS;
	Name := ModuleCreatorClass.ClassName;
end;

procedure TKIFormExpert.Execute;
var
	ModIntf: TIModuleInterface;
	ModCreator: TKIModuleCreator;
begin
	ModCreator := ModuleCreatorClass.Create( Self );
	try
		ModIntf := ToolServices.ModuleCreate( ModCreator, CreateModuleFlags );
		if CheckObject( ModIntf ) then
			ModIntf.Free;
	finally
		ModCreator.Free;
	end;
end;

{ TKIProjectExpert }

constructor TKIProjectExpert.Create( AProjectCreatorClass: TKIProjectCreatorClass;
  AModuleCreatorClass: TKIModuleCreatorClass );
begin
  ForceClass( AProjectCreatorClass );
  FIProjectCreatorClass := AProjectCreatorClass;
{ The project can or cannot has any module... }
  FIModuleCreatorClass := AModuleCreatorClass;
  inherited Create( esForm );
end;

procedure TKIProjectExpert.Initialize;
begin
  inherited Initialize;
  FCreateProjectFlags := DEFAULT_PROJECT_CREATION_FLAGS;
	FCreateModuleFlags := DEFAULT_MODULE_CREATION_FLAGS;
	Name := ProjectCreatorClass.ClassName;
end;

procedure TKIProjectExpert.Execute;
var
	ModIntf: TIModuleInterface;
	ProjCreator: TKIProjectCreator;
begin
	ProjCreator := ProjectCreatorClass.Create( Self, ModuleCreatorClass );
	try
		ProjCreator.CreateModuleFlags := CreateModuleFlags;
		ModIntf := ToolServices.ProjectCreate( ProjCreator, CreateProjectFlags );
		if CheckObject( ModIntf ) then
			ModIntf.Free;
	finally
		ProjCreator.Free;
	end;
end;

{ TKIStandardExpert }

constructor TKIStandardExpert.Create( const AnExpertName: string );
begin
	ForceTrimStr( AnExpertName );
	Name := AnExpertName;
	inherited Create( esStandard );
end;

{ DLL Expert registration Support }

function InitExpert( AToolServices: TIToolServices; RegisterProc: TExpertRegisterProc;
	var Terminate: TExpertTerminateProc ): Boolean; stdcall;
begin
	ForceReference( @GetKExpertInstanceProc );
	ExptIntf.ToolServices := AToolServices;
	Result := True;
	if IsLibrary then
		Application.Handle := AToolServices.GetParentHandle;
	RegisterProc( GetKExpertInstanceProc );
	if CheckReference( @TerminateExpertProc ) then
    Terminate := TerminateExpertProc;
end;

{
--------------------------------------------------------------------------------
------------------ Open Tools API Virtual File System Support ------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	PFileSystem = ^TFileSystem;
	TFileSystem = record
{
		Instance: TKIVirtualFileSystem;
		- avoid instance referece to avoid instance reference counting management...
}
		ClassType: TKIVirtualFileSystemClass;
		IDString: string;
		ModuleList: TStrings;
	end;

  TKVFSModuleModNotifier = class;

	PVFSPropInfo = ^TVFSPropInfo;
	TVFSPropInfo = record
		Source: TPersistent;
		ppInfo: PPropInfo;
		Validated: Boolean;
		OriginalFileName: string;
	end;

	TKVFSModuleAddInNotifier = class( TIAddInNotifier )
	private
		procedure CloseAllVFSModules;
		
	public
		constructor Create;
		destructor Destroy; override;

		procedure FileNotification( NotifyCode: TFileNotification;
			const FileName: string; var Cancel: Boolean ); override;
		procedure EventNotification( NotifyCode: TEventNotification;
			var Cancel: Boolean ); override;

	end;

{ This module Notifier is for the OriginalFileSystem to ping the VFSModule! }
	TKVFSModuleModNotifier = class( TIModuleNotifier )
	private
		FModIntf: TIModuleInterface;
		FVFSModuleName: string;

		procedure CloseVFSModule;
	public
		constructor Create( const VFSModuleName: string; ModIntf: TIModuleInterface );

		destructor Destroy; override;

		procedure Notify( NotifyCode: TNotifyCode ); override;
		procedure ComponentRenamed( ComponentHandle: Pointer;
			const OldName, NewName: string ); override;

	end;

var
	FileSystems: TList = nil;
	ModuleNotifierList: TList = nil;
	FileSystemAddIn: TKVFSModuleAddInNotifier = nil;

procedure ClearTStringsVFSPropInfo( var sl: TStrings );
var
	k: Integer;
	pVFSpi: PVFSPropInfo;
	mi: TIModuleInterface;
begin
	for k := sl.Count - 1 downto 0 do
	begin
		mi := ToolServices.GetModuleInterface( sl.Strings[k] );
{ if when the package was uninstaling the module interface was opened, close it before }
		if CheckObject( mi ) then
			try
				mi.Close;
			finally
				mi.Free;
			end;
		pVFSPi := PVFSPropInfo( sl.Objects[k] );
		if CheckPointer( pVFSPi ) then
		begin
			pVFSPi^.Source := nil;
			pVFSPi^.ppInfo := nil;
			FreeCleanPointer( pVFSpi );
		end;
		sl.Objects[k] := nil;
	end;
	FreeClean( sl );
end;

procedure ClearFileSystemList;
var
	i,
	j: Integer;
	bOk: Boolean;
	pfs: PFileSystem;
begin
	if ( not CheckObject( FileSystems ) ) then
		Exit;
	j := -1;
	bOk := True;
	while CheckList( FileSystems ) do
	begin
		i := ( FileSystems.Count - 1 );
		pfs := FileSystems.Items[i];
{$BOOLEVAL ON}
		bOk := bOk and ToolServices.UnRegisterFileSystem( pfs^.IDString );
		if ( not bOk ) then
			j := i;
{$BOOLEVAL OFF}
		if CheckObject( pfs^.ModuleList ) then
			ClearTStringsVFSPropInfo( pfs^.ModuleList );
		FreeCleanPointer( pfs );
		FileSystems.Delete( i );
	end;
	FreeClean( FileSystems );
	ForceObject( FileSystemAddIn );
	FreeClean( FileSystemAddIn );
	if ( not bOk ) then
		RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSUnReg, [j] );
end;

function GetFileSystemModuleList( AVFSClass: TKIVirtualFileSystemClass ): TStrings;
var
	i: Integer;
	pfs: PFileSystem;
begin
	Result := nil;
	if ( CheckClass( AVFSClass ) and CheckList( FileSystems ) ) then
		for i := FileSystems.Count - 1 downto 0 do
		begin
			pfs := FileSystems.Items[i];
			ForcePointer( pfs );
			if CheckStrEqual( pfs^.IDString, GetFileSystemName( AVFSClass ) ) then
			begin
				if ( not CheckObject( pfs^.ModuleList ) ) then
					pfs^.ModuleList := TStringList.Create;
				Result := pfs^.ModuleList;
			end;
		end;
end;

function InternalModuleRegisteredForVFS( const ModuleName: string;
	var Position: Integer ): Boolean;
var
	i: Integer;
	s: string;
	pfs: PFileSystem;
	mi: TIModuleInterface;
begin
	Position := -1;
	ForceTrimStr( ModuleName );
	Result := CheckList( FileSystems );
	if ( not Result ) then
		Exit;
{ Serach into filesystem lis for the module }
	for i := FileSystems.Count - 1 downto 0 do
	begin
		pfs := FileSystems.Items[i];
		ForcePointer( pfs );
		Result := CheckObject( pfs^.ModuleList );
		if ( not Result ) then
			Exit;
{ if i find the module, i must ensure that this module use my file system
	The filename for non file based file systems must be unique because of naming
	clashing (eg. Button1.Hint for Form1 and Form2 ! ) }
		Result := ( pfs^.ModuleList.IndexOf( ModuleName ) <> -1 );
		if ( not Result ) then
			Exit;
		s := '';
		mi := ToolServices.GetModuleInterface( ModuleName );
		try
			Result := CheckObject( mi );
			if ( not Result ) then
				Exit;
{ if i can't get the file system raise }
			if ( not mi.GetFileSystem( s ) ) then
				RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSSearchModGetVFS, [ModuleName] );
		finally
			if Result then
				mi.Free;
		end;
{ must not be the default file system (empty string) }
		Result := CheckTrimStr( s );
		if Result then
		begin
			Position := i;
			Exit;
		end;
	end;
end;

function UnregisterModuleForVFS( const ModuleName: string;
	Position: Integer ): Boolean;
var
	i: Integer;
	pfs: PFileSystem;
	pVFSpi: PVFSPropInfo;
begin
	ForceTrimStr( ModuleName );
	ForceList( FileSystems );
	Result := false;
	if ( Position < -1 ) then
		Exit;
	pfs := FileSystems.Items[Position];
	ForcePointer( pfs );
	ForceStrings( pfs^.ModuleList );
	i := pfs^.ModuleList.IndexOf( ModuleName );
	if ( i = -1 ) then
		RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSModuleUnReg, [ModuleName,
			pfs^.IDString] );
	pVFSpi := PVFSPropInfo( pfs^.ModuleList.Objects[i] );
	if CheckPointer( pVFSpi ) then
	begin
		pVFSpi^.Source := nil;
		pVFSpi^.ppInfo := nil;
		FreeCleanPointer( pVFSpi );
	end;
	pfs^.ModuleList.Delete( i );
	Result := true;
end;

procedure ClearModuleNotifierList;
begin
	if ( not CheckObject( ModuleNotifierList ) ) then
		Exit;
	while CheckList( ModuleNotifierList ) do
	begin
		TKVFSModuleModNotifier( ModuleNotifierList.Last ).Free;
		ModuleNotifierList.Delete( ModuleNotifierList.Count - 1 );
	end;
	FreeClean( ModuleNotifierList );
end;

function GetFirstNamePartForVFS( const FileName: string ): string;
begin
	ForceTrimStr( FileName );
	Result := Copy( FileName, 1, Pos( '.', FileName ) - 1 )
end;

function InternalComponentRegisteredForVFS( const VFSModName, CompName: string;
	var VFSPos, CompPos: Integer ): Boolean;
var
	i: Integer;
	sl : TStrings;
	pfs: PFileSystem;
	pVFSpi: PVFSPropInfo;
	sFName: string;
begin
	VFSPos := -1;
	CompPos := -1;
	ForceTrimStrs( [VFSModName, CompName] );
	Result := CheckList( FileSystems );
	if ( not Result ) then
		Exit;
	Result := ( InternalModuleRegisteredForVFS( VFSModName, VFSPos ) and ( VFSPos <> -1 ) );
	if ( not Result ) then
	begin
		VFSPos := -1;
		Exit;
	end;
	pfs := PFileSystem( FileSystems.Items[VFSPos] );
	ForcePointer( pfs );
	sl := pfs^.ModuleList;
	Result := CheckStrings( sl );
	if ( not Result ) then
		Exit;
	for i := sl.Count - 1 downto 0 do
	begin
		pVFSpi := PVFSPropInfo( sl.Objects[i] );
		ForcePointer( pVFSpi );
		sFName := GetFirstNamePartForVFS( sl.Strings[i] );
		Result := CheckStrEqual( sFName, CompName );
		if Result then
		begin
			CompPos := i;
			Exit;
		end;
		Result := CheckObjectClass( pVFSpi^.Source, TComponent );
		if ( not Result ) then
			Exit;
		sFName := TComponent( pVFSpi^.Source ).Name;
		Result := CheckStrEqual( sFName, CompName );
		if Result then
		begin
			CompPos := i;
			Exit;
		end;
	end;
end;

procedure InternalMarkModVFSValidated( VFSPos, CompPos: Integer; bValidate: Boolean );
var
	pfs: PFileSystem;
begin
	if ( ( VFSPos = -1 ) or ( CompPos = -1 ) ) then
		Exit;
	ForceList( FileSystems );
	pfs := PFileSystem( FileSystems.Items[VFSPos] );
	ForcePointer( pfs );
	ForceObject( pfs^.ModuleList );
	ForcePointer( PVFSPropInfo( pfs^.ModuleList.Objects[CompPos] ) );
	with PVFSPropInfo( pfs^.ModuleList.Objects[CompPos] )^ do
		Validated := bValidate;
end;

function IsSyncVFS_ModIntf( const VFSModName, CompName: string ): Boolean;
begin
{ Used to synchronize the VFSModule with de component Name! Because all notiferes
	recieves the ComponentRenamed, but only one must realy close the VFSModule! }
	Result := CheckStrContains( CompName, VFSModName );
end;

procedure RenameComponent_VFSAdjust( mi: TIModuleInterface;
	const OldVFSModuleName, OldCompName, NewCompName: string;
	var NewVFSModuleName: string; VFSPos, CompPos: Integer );
var
	pfs: PFileSystem;
	mmi: TIModuleInterface;
begin
	if ( ( VFSPos = -1 ) or ( CompPos = -1 ) ) then
		Exit;
{ The OldVFSModuleName is the FullName, while the NewCompName can be the for or
	Component name ( first, second part! ) }
	ForceTrimStrs( [OldVFSModuleName, OldCompName, NewCompName] );
	//mi.AddRef;
	mmi := ToolServices.GetModuleInterface( OldVFSModuleName );
	if CheckObject( mmi ) then
		try
			if IsModIntfBufferModified( OldVFSModuleName ) then
				SaveModuleInterface( OldVFSModuleName, True, True );
			NewVFSModuleName := OldVFSModuleName;
			NewVFSModuleName := StringReplace( NewVFSModuleName, OldCompName, NewCompName,
				[{$IFDEF DELPHI4}rfIgnoreCase{$ELSE}krfIgnoreCase{$ENDIF}] );
			mmi.Rename( NewVFSModuleName );
			ForceList( FileSystems );
			pfs := PFileSystem( FileSystems.Items[VFSPos] );
			ForcePointer( pfs );
			ForceObject( pfs^.ModuleList );
			pfs^.ModuleList.Strings[CompPos] := NewVFSModuleName;
		finally
			mmi.Free;
		end;
end;

{ TKVFSModuleAddInNotifier }

constructor TKVFSModuleAddInNotifier.Create;
begin
	inherited Create;
	if ( not ToolServices.AddNotifierEx( Self ) ) then
		RaiseException( EKVirtualFileSystem, sErrVFSModAddInRegError );
end;

destructor TKVFSModuleAddInNotifier.Destroy;
begin
	if ( not ToolServices.RemoveNotifier( Self ) ) then
		RaiseException( EKVirtualFileSystem, sErrVFSModAddInUnRegError );
	inherited Destroy;
end;

procedure TKVFSModuleAddInNotifier.CloseAllVFSModules;
var
	i: Integer;
	mi: TIModuleInterface;
	mn: TKVFSModuleModNotifier;
begin
	if ( not CheckObject( ModuleNotifierList ) ) then
		Exit;
	for i := ModuleNotifierList.Count - 1 downto 0 do
	begin
		mn := TKVFSModuleModNotifier( ModuleNotifierList.Last );
	{ if when the package was uninstaling the module interface was opened, close it before }
		mi := ToolServices.GetModuleInterface( mn.FVFSModuleName );
		if CheckObject( mi ) then
			try
				mi.Close;
			finally
				mi.Free;
			end;
	end;
end;

procedure TKVFSModuleAddInNotifier.FileNotification( NotifyCode: TFileNotification;
	const FileName: string; var Cancel: Boolean );
var
	i: Integer;
begin
	case NotifyCode of
		fnFileOpening : ;
		fnFileOpened  : ;
		fnFileClosing,
		fnProjectClosing :
			if ( InternalModuleRegisteredForVFS( FileName, i ) ) then
				UnregisterModuleForVFS( FileName, i );
		fnProjectOpening : ;
		fnProjectOpened  : ;
		fnAddedToProject : ;
		fnRemovedFromProject : ;
		fnDefaultDesktopLoad : ;
		fnDefaultDesktopSave : ;
		fnProjectDesktopLoad : ;
		fnprojectDesktopSave : ;
		fnPackageInstalled   : ;
		fnPackageUninstalled :
			if ( CheckStrEqual( ExtractFileName( FileName ), GetPackageName( perSystem ) ) or
					 CheckStrEqual( ExtractFileName( FileName ), GetPackageName( pedSystem ) ) ) then
				CloseAllVFSModules;
	end;
end;

procedure TKVFSModuleAddInNotifier.EventNotification( NotifyCode: TEventNotification;
	var Cancel: Boolean );
begin
	{ do nothing, we are looking only for file notifications... }
end;

{ TKVFSModuleModNotifier }

constructor TKVFSModuleModNotifier.Create( const VFSModuleName: string;
	ModIntf: TIModuleInterface );
begin
	inherited Create;
	ForceObject( ModIntf );
	ForceTrimStr( VFSModuleName );
	FModIntf := ModIntf;
	FModIntf.AddRef;
	FVFSModuleName := VFSModuleName;
	FModIntf.AddNotifier( Self );
end;

destructor TKVFSModuleModNotifier.Destroy;
begin
	if ( not CheckList( ModuleNotifierList ) ) then
	begin
		ModuleNotifierList.Remove( Pointer( Self ) );
		if ( not CheckList( ModuleNotifierList ) ) then
  		FreeClean( ModuleNotifierList );
	end;
	if CheckObject( FModIntf ) then
		FModIntf.RemoveNotifier( Self );
	FModIntf.Free;
	inherited Destroy;
end;

procedure TKVFSModuleModNotifier.CloseVFSModule;
var
	mi: TIModuleInterface;
begin
	mi := ToolServices.GetModuleInterface( FVFSModuleName );
	if CheckObject( mi ) then
		try
			if IsModIntfBufferModified( FVFSModuleName ) then
				mi.Save( True );
			mi.Close;
		finally
			mi.Free;
		end;
end;

procedure TKVFSModuleModNotifier.Notify( NotifyCode: TNotifyCode );
begin
	case NotifyCode of
		ncModuleDeleted:
		begin
			CloseVFSModule;
			Self.Free;
		end;
//	ncModuleRenamed   : InternalVFSModOriginalFileReName( FModIntf, FVFSModuleName );
		ncEditorModified  : ;
		ncFormModified    : ;
		ncEditorSelected  : ;
		ncFormSelected    : ;
		ncBeforeSave      :
			if IsModIntfBufferModified( FVFSModuleName ) then
				SaveModuleInterface( FVFSModuleName, True, True );
		ncAfterSave			  : ;
		ncFormSaving      : ;
		ncProjResModified : ;
	end;
end;

procedure TKVFSModuleModNotifier.ComponentRenamed( ComponentHandle: Pointer;
	const OldName, NewName: string );
const
{
	 CheckTrimStr( NewName ), CheckTrimStr( OldName )

				False									 False  -> ? Cannot Happen!
				False									 True   -> Comp OldName Deleted
				True									 False  -> Comp NewName Added
				True									 True   -> Comp OldName Renamed to NewName
}

	COMP_ERROR   = 0;
	COMP_DELETED = 1;
	COMP_ADDED   = 2;
	COMP_RENAMED = 3;

	Filter: array[Boolean, Boolean] of ShortInt = (
		( COMP_ERROR, COMP_DELETED ), ( COMP_ADDED, COMP_RENAMED ) );

var
	VFSPos,
	CompPos,
	j: Integer;
begin
{ AFTER ALL WE CAN SIMPLIFY THIS MECHANISM BY USE ONLY TICOMPONENTINTERFACES.
	AND DOESN'T USE PROPINFOS... CAN USE THE FORMSELECTED/MODIFIED NOTIFY CODES
	FOR MAKE THE ONFLUSH ADJUSTING/RESYNC.}

{ in the COMP_DELETED/ADDED we will mark the reference as invalid/valid if it exists
	because of a very particular situation: renaming a component to a empty string and
	rename it again to a properly name. If the user forgot to adjust this problem, the
	proxy doesn't stream the component correctly raise a run time error. But nothing
	happens in Design-Time. }

	j := Filter[CheckTrimStr( NewName ), CheckTrimStr( OldName )];

	case j of
		COMP_ERROR   : { do nothing ... Error condition };
		COMP_DELETED : { mark reference invalid if exists and close the VFSmodule! }
			if ( IsSyncVFS_ModIntf( FVFSModuleName, OldName ) and
				 InternalComponentRegisteredForVFS( FVFSModuleName, OldName, VFSPos, CompPos ) ) then
			begin
				InternalMarkModVFSValidated( VFSPos, CompPos, false );
				CloseVFSModule;
			end;
		COMP_ADDED   : { mark reference valid if exists! }
			if ( not IsSyncVFS_ModIntf( FVFSModuleName, NewName ) and
				 InternalComponentRegisteredForVFS( FVFSModuleName, NewName, VFSPos, CompPos ) ) then
				InternalMarkModVFSValidated( VFSPos, CompPos, true ); { never happens... i hope... }
		COMP_RENAMED : { rename references if exists }
			if ( IsSyncVFS_ModIntf( FVFSModuleName, OldName ) and
				InternalComponentRegisteredForVFS( FVFSModuleName, OldName, VFSPos, CompPos ) ) then
			begin
				RenameComponent_VFSAdjust( FModIntf, FVFSModuleName, OldName, NewName,
				FVFSModuleName, VFSPos, CompPos );
			end;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

function CheckVFSPropInfoKind( Source: TPersistent; ppi: PPropInfo ): Boolean;
begin
	ForceObject( Source );
	ForcePointer( ppi );
	Result := ( ppi^.PropType^^.Kind in VALID_VFS_PROP_TYPEKINDS );
end;

procedure ForceVFSPropInfoKind( Source: TPersistent; ppi: PPropInfo );
begin
	if ( not CheckVFSPropInfoKind( Source, ppi ) ) then
		RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSRegModIntfPropInfo, [
			Source.ClassName, EnumName( Integer( ppi^.PropType^^.Kind ),
			TypeInfo( TTypeKind ) ), ppi^.Name] );
end;

function RegisterFileSystem( AVFSClass: TKIVirtualFileSystemClass;
	AStreamClass: TKIStringStreamClass ): Boolean;
var
	i: Integer;
	pfs: PFileSystem;
begin
	Result := IsFileSystemRegistered( AVFSClass );
	if Result then
		Exit;
	if ( not CheckObject( FileSystems ) ) then
	begin
		FileSystems := TList.Create;
		FileSystemAddIn := TKVFSModuleAddInNotifier.Create;
	end;
	ForceClass( AVFSClass );
	i := -1;
	pfs := New( PFileSystem );
	try
		ZeroMemory( pfs, SizeOf( TFileSystem ) );
		pfs^.ClassType := AVFSClass;
		pfs^.IDString := AVFSClass.GetFileSystemName;
		pfs^.ModuleList := nil;
		if ( not ToolServices.RegisterFileSystem( AVFSClass.Create( AStreamClass ) ) ) then
			RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSReg, [pfs^.IDString] );
		i := FileSystems.Add( pfs );
		Result := true;
	except
		Dispose( pfs );
		if ( i > -1 ) then
			FileSystems.Delete( i );
		raise;
	end;
end;

function UnregisterFileSystem( AVFSClass: TKIVirtualFileSystemClass ): Boolean;
var
	i: Integer;
	pfs: PFileSystem;
begin
	Result := true;
	if ( not ( CheckObject( FileSystems ) and IsFileSystemRegistered( AVFSClass ) ) ) then
		Exit;
	ForceClass( AVFSClass );
	try
		for i := FileSystems.Count - 1 downto 0 do
		begin
			pfs := FileSystems.Items[i];
			if CheckStrEqual( pfs^.IDString, GetFileSystemName( AVFSClass ) ) then
			begin
				if ( not ToolServices.UnRegisterFileSystem( pfs^.IDString ) ) then
					RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSUnReg, [pfs^.IDString] );
				if CheckObject( pfs^.ModuleList ) then
					ClearTStringsVFSPropInfo( pfs^.ModuleList );
				FreeCleanPointer( pfs );
				FileSystems.Delete( i );
				Exit;
			end;
		end;
		Result := False;
	finally
		if ( not CheckList( FileSystems ) ) then
		begin
			FreeClean( FileSystems );
			ForceObject( FileSystemAddIn );
			FreeClean( FileSystemAddIn );
		end;
	end;
end;

function IsFileSystemRegistered( AVFSClass: TKIVirtualFileSystemClass ): Boolean;
var
	i: Integer;
	pfs: PFileSystem;
begin
{ True here only if AVFSClass is nil - Default File system }
	Result := ( not CheckClass( AVFSClass ) );
	if ( not CheckObject( FileSystems ) ) then
		Exit;
	ForceClass( AVFSClass );
	for i := FileSystems.Count - 1 downto 0 do
	begin
		pfs := FileSystems.Items[i];
		if CheckStrEqual( pfs^.IDString, GetFileSystemName( AVFSClass ) ) then
		begin
			Result := True;
			Exit;
		end;
	end;
end;

function GetFileSystemName( AVFSClass: TKIVirtualFileSystemClass ): string;
begin
	Result := '';
	if CheckClass( AVFSClass ) then
		Result := AVFSClass.GetFileSystemName;
end;

function RegisterModuleForVFS( AVFSClass: TKIVirtualFileSystemClass;
	const OriginalFileName, VFSModuleName: string; AObject: TPersistent; APropInfo: PPropInfo ): Boolean;
var
	mi: TIModuleInterface;
	sl: TStrings;
	pVFSpi: PVFSPropInfo;
begin
	ForceClass( AVFSClass );
	ForceTrimStrs( [OriginalFileName, VFSModuleName] );
	ForceVFSPropInfoKind( AObject, APropInfo );
	Result := IsFileSystemRegistered( AVFSClass );
	if ( not CheckObject( ModuleNotifierList ) ) then
		ModuleNotifierList := TList.Create;
	if ( not Result ) then
		Exit;
	mi := ToolServices.GetModuleInterface( VFSModuleName );
	try
		Result := CheckObject( mi );
		if ( not Result ) then
			Exit;
	finally
		if Result then
			mi.Free;
	end;
	mi := ToolServices.GetModuleInterface( OriginalFileName );
	try
		Result := CheckObject( mi );
		if ( not Result ) then
			Exit;
		ModuleNotifierList.Add( TKVFSModuleModNotifier.Create( VFSModuleName, mi ) );
	finally
		if Result then
			mi.Free;
	end;
	sl := GetFileSystemModuleList( AVFSClass );
	ForceObject( sl );
	pVFSpi := New( PVFSPropInfo );
	try
		ZeroMemory( pVFSpi, SizeOf( TVFSPropInfo ) );
		pVFSpi^.Source := AObject;
		pVFSpi^.ppInfo := APropInfo;
		pVFSpi^.Validated := True;
		pVFSpi^.OriginalFileName := OriginalFileName;
		sl.AddObject( VFSModuleName, TObject( pVFSpi ) );
	except
		FreeCleanPointer( pVFSpi );
		raise;
	end;
end;

function ModuleRegisteredForVFS( AVFSClass: TKIVirtualFileSystemClass;
	const ModuleName: string ): Boolean;
var
	sl: TStrings;
begin
	ForceTrimStr( ModuleName );
	Result := CheckClass( AVFSClass );
	if ( not Result ) then
		Exit;
	sl := GetFileSystemModuleList( AVFSClass );
	Result := ( CheckStrings( sl ) and ( sl.IndexOf( ModuleName ) <> -1 ) ); 	
end;

{ TKIVirtualFileSystem }

constructor TKIVirtualFileSystem.Create( AStreamClass: TKIStringStreamClass );
begin
	inherited Create;
	FLastMode := -1;
  FAge := -1;
	if ( not CheckClass( AStreamClass ) ) then
		FStreamClass := TKIStringStream
	else
		FStreamClass := AStreamClass;
	Initialize;
end;

procedure TKIVirtualFileSystem.Initialize;
begin
	FModules := nil;
end;

function TKIVirtualFileSystem.GetModules( ModIndex: Integer ): string;
begin
	Result := '';
	if CheckStrings( FModules ) then
		Result := FModules.Strings[ModIndex];
end;

function TKIVirtualFileSystem.GetObjects( ModIndex: Integer ): TPersistent;
var
	pVFSpi: PVFSPropInfo;
begin
	Result := nil;
	if CheckStrings( FModules ) then
	begin
		pVFSpi := PVFSPropInfo( FModules.Objects[ModIndex] );
		if CheckPointer( pVFSpi ) and ( pVFSpi^.Validated ) then
			Result := pVFSpi^.Source;
	end;
end;

function TKIVirtualFileSystem.GetPropInfo( ModIndex: Integer ): PPropInfo;
var
	pVFSpi: PVFSPropInfo;
begin
	Result := nil;
	if CheckObject( FModules ) then
	begin
		pVFSpi := PVFSPropInfo( FModules.Objects[ModIndex] );
		if CheckPointer( pVFSpi ) and ( pVFSpi^.Validated ) then
			Result := pVFSpi^.ppInfo;
	end;
end;

function TKIVirtualFileSystem.GetOriginalFileName( ModIndex: Integer ): string;
var
	pVFSpi: PVFSPropInfo;
begin
	Result := '';
	if CheckStrings( FModules ) then
	begin
		pVFSpi := PVFSPropInfo( FModules.Objects[ModIndex] );
		if CheckPointer( pVFSpi ) then
			Result := pVFSpi^.OriginalFileName;
	end;
end;

function TKIVirtualFileSystem.IsFileBased: Boolean;
begin
	Result := False;
end;

function TKIVirtualFileSystem.GetFileStream( const AFileName: TFileName;
	AMode: Integer ): {$IFDEF DELPHI4}IStream{$ELSE}TIStream{$ENDIF}; 
begin
	{if ( not ( ( AMode = fmCreate ) or ( ( AMode and $F ) = fmOpenWrite ) or
		( ( AMode and $F ) = fmOpenRead ) ) ) then
		Exit;}

	{ Delphi protects this exception to be raised... }
	{if ( not ( AMode = fmCreate ) ) then
		Exit;}
	Result := StreamClass.Create( nil );
	try
		with ( Result as StreamClass ) do
		begin
			FileName := AFileName;
			Mode := AMode;
			FLastMode := AMode;
			OnFlush := FlushEvent;
		end;
	except
		FreeClean( Result );
		raise;
	end;
end;

function TKIVirtualFileSystem.FileAge( const FileName: TFileName ): Longint;
var
	Search: TSearchRec;
begin
	Result := FAge;
	if IsFileBased then
	begin
		if ( FindFirst( FileName, faAnyFile, Search ) = 0 ) then
		begin
			FindClose( Search );
			Result := Search.Time;
		end;
	end
	else if ( FAge = -1 ) then //( ( FLastMode = fmCreate ) or ( FLastMode = -1 ) ) then
	begin
		Result := DateTimeToFileDate( Now );
		FAge := Result;
	end;
end;

function TKIVirtualFileSystem.RenameFile( const OldName, NewName: TFileName ): Boolean;
begin
	Result := ( ( ( not IsFileBased ) or SysUtils.RenameFile( OldName, NewName ) ) and ( FLastMode = fmCreate ) );
end;

function TKIVirtualFileSystem.IsReadonly( const FileName: TFileName ): Boolean;
var
	Search: TSearchRec;
begin
	if IsFileBased then
	begin
		Result := ( FindFirst( FileName, faAnyFile, Search ) <> 0 );
		if Result then
		begin
			FindClose( Search );
			Result := ( ( SysUtils.faReadOnly and Search.Attr ) <> 0 );
		end;
	end
	else
		Result := False;//( FLastMode <> fmCreate ) and ( FLastMode <> -1 );
end;

function TKIVirtualFileSystem.DeleteFile( const FileName: TFileName ): Boolean;
begin
	if IsFileBased then
		Result := CheckDeleteFile( FileName )
	else
		Result := CheckStrContains( '.~', ExtractFileExt( FileName ) );
end;

function TKIVirtualFileSystem.FileExists( const FileName: TFileName ): Boolean;
begin
	Result := True;
	if IsFileBased then
		Result := CheckFile( FileName );
end;

function TKIVirtualFileSystem.GetTempFileName( const FileName: TFileName ): TFileName;
begin
{ Idependtly of IsFileBased, we must provide a file name because delphi will use it
	when call GetFileStream... }
	Result := '';
	ForceTrimStr( FileName );
	if IsFileBased then
		Result := GetTempPathFile( ChangeFileExt( ExtractFileName( FileName ), '' ), True )
	else
		Result := FileName;
end;

function TKIVirtualFileSystem.GetBackupFileName( const FileName: TFileName ): TFileName;
begin
{ Idependtly of IsFileBased, we must provide a file name because delphi will use it
	when call GetFileStream... }
	Result := '';
	ForceTrimStr( FileName );
	Result := ChangeFileExt( FileName, '.~' + Copy( ExtractFileExt( FileName ), 2,
		Length( ExtractFileExt( FileName ) ) - 2 ) );
end;

function TKIVirtualFileSystem.GetIDString: string;
begin
	Result := GetFileSystemName;
end;

class function TKIVirtualFileSystem.GetDefaultNamePrefix: string;
begin
	Result := sKnowHowPage + '.';
end;

class function TKIVirtualFileSystem.GetFileSystemName: string;
begin
	Result := GetDefaultNamePrefix + ClassName;
end;

function TKIVirtualFileSystem.GetPropValueSize( Stream: TKIStringStream;
	ModIndex: Integer ): Integer;
begin
	Result := -1;
end;

procedure TKIVirtualFileSystem.FlushEvent( Sender: TObject );
var
	i,
	iSize: Integer;
	p: Pointer;
begin
	ForceObject( Sender );
	FModules := GetFileSystemModuleList( TKIVirtualFileSystemClass( ClassType ) );
	ForceObject( FModules );
	i := -1;
	with ( Sender as StreamClass ) do
		if ( InternalModuleRegisteredForVFS( FileName, i ) and ( i <> -1 ) ) then
		begin
			i := FModules.IndexOf( FileName );
			if ( i = -1 ) then
				RaiseExceptionFmt( EKVirtualFileSystem, sErrVFSModuleUnReg, [FileName, GetFileSystemName] );
{ Prevent any early instance destruction ... }
			if ( not ( CheckObject( Self.Objects[i] ) and CheckPointer( Self.PropInfo[i] ) ) ) then
				Exit;
			iSize := GetPropValueSize( ( Sender as StreamClass ), i );
			if ( iSize <> -1 ) then
			begin
				GetMem( p, iSize );
				try
					ZeroMemory( p, iSize );
					GetPropValue( p^, iSize, ( Sender as StreamClass ), i );
					SetPropValue( p, ( Sender as StreamClass ), i );
					MarkModified( ( Sender as StreamClass ), i );
				finally
					FreeMem( p, iSize );
				end;
			end;
		end;
end;

procedure TKIVirtualFileSystem.SetPropValue( PropValue: Pointer;
  Stream: TKIStringStream; ModIndex: Integer );
begin
{ At this point I have sure that Objects and PropInfo are valid }
	SetPropInfo( Objects[ModIndex], PropInfo[ModIndex], PropValue^ );
end;

{ TKIStringVFS }

procedure TKIStringVFS.MarkModified( Stream: TKIStringStream; ModIndex: Integer );
var
	i: Integer;
	slFile,
	slForm: TStrings;
begin
	slFile := TStringList.Create;
	try
		slForm := TStringList.Create;
		try
			GetProjectInfo( slFile, nil, slForm );
			i := slForm.IndexOf( GetFirstNamePartForVFS( Stream.FileName ) );
			if ( i <> -1 ) then
			begin
				ForceTrimStr( slFile.Strings[i] );
				ModifyFormInterface( slFile.Strings[i] )
			end;
		finally
			slForm.Free;
		end;
	finally
		slFile.Free;
	end;
end;

function TKIStringVFS.GetPropValueSize( Stream: TKIStringStream; ModIndex: Integer ): Integer;
begin
	Result := inherited GetPropValueSize( Stream, ModIndex );
	if ( Result = -1 ) then
		Result := Stream.StringStream.Size + 1;
end;

procedure TKIStringVFS.GetPropValue( var PropValue; Count: Integer;
	Stream: TKIStringStream; ModIndex: Integer );
var
	s : string;
begin
{
	We must use this variable because of string reference count problems...
	We must use PChar( s )^ and not Pointer, because this value will be
	dereferenciated as PChar in SetPropInfo.
	In SetPropInfo we must use PChar( @PropValue ) and not string( Pointer( @PropValue ) )
	because of string reference count problems...
}
	s := Stream.StringStream.DataString;
	Move( PChar( s )^, PropValue, Length( s ) );
end;

{ TKITStringsVFS }

procedure TKITStringsVFS.SetPropValue( PropValue: Pointer; Stream: TKIStringStream;
	ModIndex: Integer );
var
	sl: TStrings;
begin
{ At this point I have sure that Objects and PropInfo are valid }
	sl := TStringList.Create;
	try
    sl.Text := PChar( PropValue );
		SetPropInfo( Objects[ModIndex], PropInfo[ModIndex], LongInt( sl ) );
	finally
	  sl.Free;
	end;
end;

{
================================================================================
========================== Property Editor Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
---------------------------- TKColorProperty Editor ----------------------------
--------------------------------------------------------------------------------
}

function TKColorProperty.GetValue: string;
begin
	if ( not KColorToIdent( GetOrdValue, Result ) ) then
		Result := inherited GetValue;
end;

procedure TKColorProperty.GetValues( Proc: TGetStrProc );
begin
	KGetColorValues( Proc );
	inherited GetValues( Proc );
end;

procedure TKColorProperty.SetValue( const Value: string );
var
	NewValue: Longint;
begin
	if KIdentToColor( Value, NewValue ) then
		SetOrdValue( NewValue )
	else
		inherited SetValue( Value );
end;

{
--------------------------------------------------------------------------------
------------------------- TKLongStringProperty Editors -------------------------
--------------------------------------------------------------------------------
}

{---------------------------- TKLongStringProperty -----------------------------}

procedure TKLongStringProperty.Initialize;
begin
	inherited Initialize;
	FVFSFileName := '';
	AdjustVFSFileName;
end;

function TKLongStringProperty.GetSyntaxHighLighter: TSyntaxHighlighter;
begin
	Result := shNone;
end;

procedure TKLongStringProperty.AdjustVFSFileName;
var
	sRoot: string;
begin
	if CheckObjectClass( GetComponent( 0 ), TComponent ) then
		FVFSFileName := TComponent( GetComponent( 0 ) ).Name
	else
		FVFSFileName := GetComponent( 0 ).GetNamePath;
	sRoot := GetDesignerRootCompName( Designer );
	if CheckStrEqual( sRoot, FVFSFileName ) then
		FVFSFileName := FVFSFileName + '.' + GetName
	else
		FVFSFileName := sRoot + '.' + FVFSFileName + '.' + GetName;
end;

function TKLongStringProperty.GetVFSFileName: string;
begin
	AdjustVFSFileName;
	Result := FVFSFileName;  
end;

function TKLongStringProperty.GetAttributes: TPropertyAttributes;
begin
{ Do not paMultiSelect (VFS Architectural grieves :( , we can only check to avoid
  button enabled at form (code editor)... ) }
	Result := inherited GetAttributes + [paDialog, paMultiSelect];
end;

procedure TKLongStringProperty.SetEditorValue( Module: TIModuleInterface;
	Editor: TIEditorInterface; Data: Pointer );
begin
	ClearEditor( Editor );
	if CheckPChar( PChar( Data ) ) then
		InsertCode( Editor, 0, PChar( Data ) )
	else
		inherited SetValue( '' );
	if Editor.BufferModified then
		SaveModuleInterface( Editor.FileName, True, True );
end;

procedure TKLongStringProperty.SetValue( const Value: string );
begin
	if ModuleRegisteredForVFS( TKIStringVFS, VFSFileName ) then
	begin
		ExecuteEditorInterfaceByMethod( VFSFileName, SetEditorValue, False, PChar( Value ) );
		Modified;
	end
	else
		inherited SetValue( Value );
end;

procedure TKLongStringProperty.Edit;
var
	s: string;
begin
	s := GetStrValue;
	if EditString( GetComponent( 0 ), GetName, VFSFileName, Designer, s,
	  GetSyntaxHighlighter, ( PropCount > 1 ) ) then
		SetStrValue( s );
end;

{------------------------------- TKLongCaption ---------------------------------}

function TKLongCaptionProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paAutoUpdate];
end;

{
--------------------------------------------------------------------------------
------------------------------ TKDialogClassProperty ---------------------------
--------------------------------------------------------------------------------
}

function TKDialogClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result := ( inherited GetAttributes ) + [paDialog] - [paSubProperties];
end;

{
--------------------------------------------------------------------------------
----------------------------- TKCustomGradientProperty -------------------------
--------------------------------------------------------------------------------
}

function TKCustomGradientProperty.GetAttributes: TPropertyAttributes;
begin
	Result := ( inherited GetAttributes ) + [paDialog];
end;

{
--------------------------------------------------------------------------------
----------------------------- TKUnSortedEnumProperty --------------------------
--------------------------------------------------------------------------------
}

function TKUnSortedEnumProperty.GetAttributes: TPropertyAttributes;
begin
	Result := ( inherited GetAttributes ) - [paSortList];
end;

{ TKThreadPriorityProperty }

procedure TKThreadPriorityProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
	EnumType: PTypeInfo;
begin
	EnumType := GetPropType;
	with GetTypeData( EnumType )^ do
		for i := MinValue to MaxValue do
			if ( TKThreadPriority( i ) <> ktpError ) then
  		  Proc( GetEnumName( EnumType, i ) );
end;

{
--------------------------------------------------------------------------------
------------------------------- TKTStringsProperty -----------------------------
--------------------------------------------------------------------------------
}

{----------------------------- TKTStringsProperty ------------------------------}

procedure TKTStringsProperty.Initialize;
begin
	inherited Initialize;
	FVFSFileName := '';
	AdjustVFSFileName;
end;

function TKTStringsProperty.GetSyntaxHighLighter: TSyntaxHighlighter;
begin
	Result := shNone;
end;

procedure TKTStringsProperty.AdjustVFSFileName;
var
	sRoot: string;
begin
	if CheckObjectClass( GetComponent( 0 ), TComponent ) then
		FVFSFileName := TComponent( GetComponent( 0 ) ).Name
	else
		FVFSFileName := GetComponent( 0 ).GetNamePath;
	sRoot := GetDesignerRootCompName( Designer );
	if CheckStrEqual( sRoot, FVFSFileName ) then
		FVFSFileName := FVFSFileName + '.' + GetName
	else
		FVFSFileName := sRoot + '.' + FVFSFileName + '.' + GetName;
end;

function TKTStringsProperty.GetVFSFileName: string;
begin
	AdjustVFSFileName;
	Result := FVFSFileName;
end;

procedure TKTStringsProperty.Edit;
var
	sl: TStrings;
begin
	sl := TStrings( GetOrdValue );
	if EditTStrings( GetComponent( 0 ), GetName, VFSFileName, TKITStringsVFS,
		Designer, sl, GetSyntaxHighlighter, ( PropCount > 1 ) ) then
	begin
		SetOrdValue( LongInt( sl ) ); {?}
		Modified;		
	end;
end;

{---------------------------- TKSQLStringsProperty -----------------------------}

function TKSQLStringsProperty.GetSyntaxHighLighter: TSyntaxHighlighter;
begin
	Result := shSQL;
end;

{--------------------------- TKPascalStringsProperty ---------------------------}

function TKPascalStringsProperty.GetSyntaxHighLighter: TSyntaxHighlighter;
begin
	Result := shPascal;
end;

{
--------------------------------------------------------------------------------
--------------------------- TKFileNameProperty Editor --------------------------
--------------------------------------------------------------------------------
}

{ TKCustomFileNameProperty }

function TKCustomFileNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paDialog];
	if ( IsReadOnly ) then
		Include( Result, paReadOnly )
	else
		Exclude( Result, paReadOnly );
end;

function TKCustomFileNameProperty.IsReadOnly: Boolean;
begin
	Result := False;
end;

function TKCustomFileNameProperty.GetTitle: string;
begin
	Result := sDefDlgTitle;
end;

function TKCustomFileNameProperty.GetInitDir: string;
begin
	Result := CurrentProjectPath;
end;

function TKCustomFileNameProperty.GetFileDlgOptions: TOpenOptions;
begin
	Result := [ofHideReadOnly, ofPathMustExist];
end;

{----------------------------- TKFileNameProperty -----------------------------}

function TKFileNameProperty.GetFilter: string;
begin
	Result := sDefDlgFilter;
end;

function TKFileNameProperty.GetDefExt: string;
begin
	Result := '';
end;

function TKFileNameProperty.GetFileDlgOptions: TOpenOptions;
begin
	Result := inherited GetFileDlgOptions + [ofFileMustExist];
end;

procedure TKFileNameProperty.Edit;
var
  sInitDir,
	sFileName: string;
begin
	sFileName := GetStrValue;
	sInitDir := GetInitDir;
	if CheckTrimStr( sFileName ) then
		sInitDir := ExtractFilePath( sFileName );
	sFileName := SelectLoadFiles( Filter, Title, sInitDir, DefExt, GetFileDlgOptions, nil );
	if ( CheckTrimStr( sFileName ) and ( not CheckStrEqual( sFileName, GetStrValue ) ) ) then
		SetStrValue( sFileName );
end;

{------------------------- TKReadOnlyFileNameProperty -------------------------}

function TKReadOnlyFileNameProperty.IsReadOnly: Boolean;
begin
	Result := True;
end;

{---------------------------- TKDirectoryProperty -----------------------------}

procedure TKDirectoryProperty.Edit;
var
	s: string;
begin
	s := GetStrValue;
	if ( ShellBrowseFolder( Title, Root, s ) and ( not CheckStrEqual( s, GetStrValue ) ) and
			 CheckPath( s ) ) then
	begin
		SetStrValue( s );
		Modified;
	end;
end;

function TKDirectoryProperty.GetRoot: string;
begin
	Result := '';
end;

function TKDirectoryProperty.IsReadOnly: Boolean;
begin
	Result := False;
end;

{------------------------- TKReadOnlyDirectoryProperty -------------------------}

function TKReadOnlyDirectoryProperty.IsReadOnly: Boolean;
begin
	Result := True;
end;

{---------------------------- TKFindFirstProperty -----------------------------}

function TKFindFirstProperty.GetSearchPath: string;
begin
	Result := InitDir;
end;

function TKFindFirstProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paValueList, paSortList] - [paDialog];
end;

procedure TKFindFirstProperty.GetValues(Proc: TGetStrProc);
var
	iFind: Integer;
	Search: TSearchRec;
begin
	iFind := FindFirst( SearchPath+'\'+DefExt, faAnyFile, Search );
	if ( iFind = 0 ) then
		try
			repeat
				ProcessValue( Proc, Search );
			until ( FindNext( Search ) <> 0 );
		finally
			FindClose( Search );
		end;
end;

procedure TKFindFirstProperty.ProcessValue( Proc: TGetStrProc;
	const Search: TSearchRec );
begin
	Proc( Search.Name );
end;

{
--------------------------------------------------------------------------------
--------------------------- TKStringsProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

function TKStringsProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paValueList];
	if IsReadOnly then
		Include( Result, paReadOnly )
	else
		Exclude( Result, paReadOnly );
	if IsSorted then
		Include( Result, paSortList )
	else
	  Exclude( Result, paSortList );		
end;

procedure TKStringsProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
	sl: TStrings;
begin
	sl := TStringList.Create;
	try
		GetValueList( sl );
		for i := 0 to sl.Count - 1 do
			Proc( sl.Strings[i] );
	finally
		sl.Free;
	end;
end;

function TKStringsProperty.IsSorted: Boolean;
begin
	Result := True;
end;

function TKStringsProperty.IsReadOnly: Boolean;
begin
	Result := True;
end;

function TKStringsProperty.GetOwner: TPersistent;
begin
	Result := nil;
end;

{--------------------------- TKTypedStringsProperty ---------------------------}

procedure TKTypedStringsProperty.GetValueList( List: TStrings );
var
	pp: PPropInfo;
	obj: TObject;
	Instance: TPersistent;
begin
	Instance := GetComponent( 0 );
	pp := TypInfo.GetPropInfo( Instance.ClassInfo, GetPropertyName );
	if CheckPointer( pp ) and ( pp^.PropType^.Kind = GetPropertyTypeKind ) then
	begin
		obj := TObject( GetOrdProp( Instance, pp ) );
		if CheckObject( obj ) then
		begin
			ForceObjectClass( obj, TPersistent );
			ProcessValueList( TPersistent( obj ), List );
		end;
	end;
end;

{
--------------------------------------------------------------------------------
-------------------------- TKCompListProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

function TKCompListProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paSortList];
	if IsReadOnly then
		Include( Result, paReadOnly )
	else
		Exclude( Result, paReadOnly );
end;

procedure TKCompListProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
	sl: TStrings;
begin
	sl := TStringList.Create;
	try
		GetValueList( sl );
		for i := 0 to sl.Count - 1 do
			Proc( sl.Strings[i] );
	finally
		sl.Free;
	end;
end;

function TKCompListProperty.IsReadOnly: Boolean;
begin
	Result := True;
end;

function TKCompListProperty.GetOwner: TPersistent;
begin
	Result := nil;
end;

{--------------------------- TKTypedCompListProperty ---------------------------}

procedure TKTypedCompListProperty.GetValueList( List: TStrings );
var
	pp: PPropInfo;
	obj: TObject;
	Instance: TPersistent;
begin
	Instance := GetComponent( 0 );
	pp := TypInfo.GetPropInfo( Instance.ClassInfo, GetPropertyName );
	if CheckPointer( pp ) and ( pp^.PropType^.Kind = tkClass ) then
	begin
		obj := TObject( GetOrdProp( Instance, pp ) );
		if CheckObject( obj ) then
		begin
			ForceObjectClass( obj, TPersistent );
			ProcessValueList( TPersistent( obj ), List );
		end;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------ TKComponentProperty Editors ---------------------------
--------------------------------------------------------------------------------
}

procedure TKCompCheckProperty.CheckComponent( const Value: string );
var
	i: Integer;
	comp: TComponent;
begin
	comp := Designer.GetComponent( Value );
	for i := 0 to PropCount - 1 do
		if ( not ProcessValue( GetComponent( i ), comp, Value ) ) then
			Exit;
	CheckProc( Value );
end;

procedure TKCompCheckProperty.GetValues( Proc: TGetStrProc );
begin
	CheckProc := Proc;
	inherited GetValues( CheckComponent );
end;

{--------------------------- TKCompNameProperty -------------------------------}

function TKCompNameProperty.ProcessValue( GetComp_i: TPersistent; BaseComp: TComponent;
	const Value: string ): Boolean;
begin
	Result := True;
end;

function TKCompNameProperty.ProcessDesigner: Boolean;
begin
	Result := True;
end;

procedure TKCompNameProperty.GetValues( Proc: TGetStrProc );

var
	i: Integer;
	Component: TComponent;
begin
	CheckProc := Proc;
	Component := GetDesignerRootComp( Designer );
	if ( ProcessDesigner and ProcessValue( nil, Component, Component.Name ) ) then
		CheckProc( Designer.Form.Name );
	for i := 0 to Designer.Form.ComponentCount - 1 do
	begin
		Component := Designer.Form.Components[i];
		if CheckTrimStr( Component.Name ) and CheckObjectClass( Component, GetFilterClass ) then
			CheckComponent( Component.Name );
	end;
end;

{
--------------------------------------------------------------------------------
----------------------------- TKShortCutProperty Editor ------------------------
--------------------------------------------------------------------------------
}

var
	ShortCutList: TList = nil;

procedure TKShortCutProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
begin
	if CheckObject( ShortCutList ) then
		for i := 0 to ShortCutList.Count - 1 do
		begin
			if ( TShortCut( ShortCutList[i] ) <> SC_NULL ) then
				Proc( ShortCutToText( TShortCut( ShortCutList[i] ) ) )
			else
				Proc( srNone );
		end
	else
		inherited GetValues( Proc );
end;

procedure	RegisterShortCuts( const ShortCuts: array of TShortCut );
const
	SHORTCUT_OFFSET = $0000FFFF;
var
	i: Integer;
begin
	if ( not CheckObject( ShortCutList ) ) then
		ShortCutList := TList.Create;
	for i := Low( ShortCuts ) to High( ShortCuts ) do
		if ( ShortCutList.IndexOf( Pointer( SHORTCUT_OFFSET and ShortCuts[i] ) ) = -1 ) then
			ShortCutList.Add( Pointer( SHORTCUT_OFFSET and ShortCuts[i] ) );
end;

{
--------------------------------------------------------------------------------
----------------------------- TKFloatProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

function TKFloatProperty.GetFloatFormat: string;
begin
	FFloatFormat := DEFAULT_FLOAT_FORMAT;
end;

function TKFloatProperty.GetValue: string;
begin
	Result := FormatFloat( FloatFormat, GetFloatValue );
end;

procedure TKFloatProperty.SetValue( const Value: string );
begin
	if CheckStr( Value ) then
		inherited SetValue( Value )
	else
		inherited SetValue( '0' );
end;

{-------------------------- TKMMFloatProperty ----------------------------------}

type
	TKCustomMeasuresHack = class( TKCustomMeasures );

function TKMMFloatProperty.GetFloatFormat: string;
const
	FLT_PATTERN: array[TKMeasurements] of string[5] = ( '#0', DEFAULT_FLOAT_FORMAT );
var
	ppi: PPropInfo;
	obj: TObject;
	cm: TKCustomMeasures;
begin
	cm := ( GetComponent( 0 ) as TKCustomMeasures );
	ppi := nil;
	obj := TKCustomMeasuresHack( cm ).Owner;
	if CheckObject( obj ) then
		ppi := TypInfo.GetPropInfo( obj.ClassInfo, sPropNameMeasurements );
	if CheckPointer( ppi ) then
		Result := FLT_PATTERN[TKMeasurements( TypInfo.GetOrdProp( obj, ppi ) )]
	else
		Result := inherited GetFloatFormat;
end;

{
--------------------------------------------------------------------------------
---------------------------- TKIntegerProperty Editor --------------------------
--------------------------------------------------------------------------------
}

function TKIntegerProperty.GetIntegerFormat: string;
begin
	Result := '%d';
end;

function TKIntegerProperty.GetValue: string;
begin
	Result := Format( IntegerFormat, [GetOrdValue] );
end;

procedure TKIntegerProperty.SetValue( const Value: string );
begin
	inherited SetValue( IntToStr( StrToIntDef( Value, 0 ) ) );
end;

{
--------------------------------------------------------------------------------
------------------------------ TKHexaProperty Editor ---------------------------
--------------------------------------------------------------------------------
}

{ TKHexaProperty }

function TKHexaProperty.GetHexaDigits: Integer;
begin
  Result := DEFUALT_HEXA_PROPERTY_DIGITS;
end;

function TKHexaProperty.GetValue: string;
begin
	Result := Format( DEFAULT_HEXA_PROPERTY_PATTERN, [GetHexaDigits, GetOrdValue] );
end;

procedure TKHexaProperty.SetValue( const Value: string );
var
	sValue: string;
begin
  sValue := Trim( Value );
	if CheckTrimStr( sValue ) and ( Length( sValue ) > 1 ) and
		 ( sValue[1] = CH_HEXA_TOKEN ) then
		inherited SetValue( IntToStr( HexToInt( Copy( sValue, 2, MaxInt ) ) ) )
	else
		inherited SetValue( sValue );
end;

{
--------------------------------------------------------------------------------
------------------------ RegisterMethodComment Support -------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	PMethodCommentRec = ^TMethodCommentRec;
	TMethodCommentRec = record
		mpti: PTypeInfo;
		mName: ShortString;
		mClass: TPersistentClass;
		mComment: string;
	end;

var
	MethodCommentList: TList = nil;

procedure RegisterMethodInfo( mpti: PTypeInfo; AClass: TPersistentClass;
	const AName, AComment: string );
const
	SW_PATTERN: array[Boolean] of string[2] = ( '', CH_CRLF );
var
	M: PMethodCommentRec;
begin
	if ( not CheckObject( MethodCommentList ) ) then
		MethodCommentList := TList.Create;
	M := New( PMethodCommentRec );
	try
		ZeroMemory( M, SizeOf( TMethodCommentRec ) );
		M^.mpti := mpti;
		M^.mClass := AClass;
		M^.mName := '';
		M^.mComment := sMethodPropDefaultComment; 
		if AnyPackage_Shareware then
			M^.mComment := sMethodPropShareWareComment + SW_PATTERN[CheckTrimStr( AComment )] + AComment
		else if CheckTrimStr( AComment ) then
			M^.mComment := AComment;
		if CheckClass( AClass ) then
			M^.mName := AName;
		MethodCommentList.Insert( 0, M );
	except
		MethodCommentList.Remove( M );
		Dispose( M );
		raise;
	end;
end;

procedure FreeMethodComments;
begin
	if ( not CheckObject( MethodCommentList ) ) then
		Exit;
	while CheckList( MethodCommentList ) do
	begin
		Dispose( PMethodCommentRec( MethodCommentList.Last ) );
		MethodCommentList.Delete( MethodCommentList.Count - 1 );
	end;
	FreeClean( MethodCommentList );
end;

function GetMethodComment( PropInfo: PPropInfo; Obj: TPersistent ): string;
var
	PropType: PTypeInfo;
	M, C: PMethodCommentRec;
	i: Integer;
begin
	Force( [PropInfo, Obj] );
	Result := '';
	if ( not CheckObject( MethodCommentList ) ) then
		Exit;
	PropType := PropInfo^.PropType^;
	i := 0;
	C := nil;
	while ( i < MethodCommentList.Count ) do
	begin
		M := MethodCommentList[I];
		if ( M^.mpti = PropType ) and
			 ( ( not CheckClass( M^.mClass ) ) or ( Obj.InheritsFrom( M^.mClass ) ) ) and
			 ( ( not CheckStr( M^.mName ) ) or ( CheckStrEqual( PropInfo^.Name, M^.mName ) ) ) then
			if ( not CheckPointer( C ) ) or
				 ( ( not CheckClass( M^.mClass ) ) and CheckClass( M^.mClass ) ) or
				 ( ( not CheckStr( C^.mName ) ) and CheckStr( M^.mName ) ) then
				C := M;
		Inc( i );
	end;
	if CheckPointer( C ) then
		Result := C^.mComment;
end;

procedure InternalRegisterMethodComment( pmti: PTypeInfo;
	AClass: TPersistentClass; const AName: ShortString;
	const AComment: string; AEditorClass: TKMethodPropertyClass );
begin
  Force( [pmti, AEditorClass] );
	if ( not CheckTrimStr( AComment ) ) then
	  Exit;
	RegisterMethodInfo( pmti,  AClass, AName, AComment );
	RegisterPropertyEditor( pmti, AClass, AName, AEditorClass );
end;

{---------------------------- Public Implementation ----------------------------}

procedure RegisterMethod( mti: PTypeInfo; MethodObjClass: TPersistentClass;
	const MethodName, MethodComment: string; MethodEditorClass: TKMethodPropertyClass );
begin
	if CheckPointer( @RegisterMethodCommentProc ) then
		RegisterMethodCommentProc( mti, MethodObjClass, MethodName, MethodComment,
			MethodEditorClass )
	else
		InternalRegisterMethodComment( mti, MethodObjClass, MethodName, MethodComment,
			MethodEditorClass );
end;

procedure RegisterDefaultMethod( mti: PTypeInfo; MethodObjClass: TPersistentClass;
	const MethodName, MethodComment: string );
begin
  RegisterMethod( mti, MethodObjClass, MethodName, MethodComment, TKMethodProperty );
end;

function MethodCommentsEnabled: Boolean;
var
	Reg: TRegIniFile;
begin
	Result := AnyPackage_ShareWare;
	if Result then
		Exit;
	Reg := TRegIniFile.Create( sKnowHowBaseInfoRegKey );
	try
		Result := Reg.ReadBool( sKnowHow_IDEConfig, sIDEEnableComment, True );
	finally
		Reg.Free;
	end;
end;

procedure RegisterMethodShareWare( const Args: array of PTypeInfo );
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		RegisterMethod( Args[i], nil, '', '', TKMethodProperty );
end;

{
--------------------------------------------------------------------------------
--------------------------- TMethod Property Editor ----------------------------
--------------------------------------------------------------------------------
}

{ TKCustomMethodProperty }

function TKCustomMethodProperty.GetCommentString: string;
begin
	Result := Format( METHOD_COMMENT_PATTERN, [ GetMethodComment( GetPropInfo, GetComponent( 0 ) ) ] );
end;

{ TKMethodComment }

constructor TKMethodComment.Create( PropEd: TKMethodProperty );
begin
	inherited Create;
	FProp := PropEd;
end;

destructor TKMethodComment.Destroy;
begin
	FProp := nil;
	inherited Destroy;
end;

procedure TKMethodComment.MakeComment( Module: TIModuleInterface;
	Editor: TIEditorInterface; Data: Pointer );
var
  sComment,
	Buffer: string;
	iPos,
	iEdPos: Integer;
begin
	sComment := FProp.CommentString;
	ForceTrimStr( sComment );
	FStream := TKIEditReaderStream.Create( Editor );
	try
		FStream.Position := 0;
		iEdPos := FStream.Size;
		SetString( Buffer, nil, iEdPos );
		FStream.ReadBuffer( Pointer( Buffer )^, iEdPos );
	finally
		FStream.Free;
	end;
	ForceTrimStr( Buffer );
	iEdPos := Pos( '.' + CommentedMethodName, Buffer );
	if ( iEdPos <= 0 ) then
		RaiseExceptionFmt( EKMethodProperty, sErrMethodPropInvMehotName, [CommentedMethodName] );
	Buffer := Copy( Buffer, iEdPos + 1, Length( Buffer ) - iEdPos + 1 );
	iPos := Pos( METHOD_START_POINT, Buffer );
	if ( iPos <= 0 ) then
		RaiseExceptionFmt( EKMethodProperty, sErrMethodPropInvStartPoint, [CommentedMethodName] );
	Inc( iEdPos, iPos - 1 );
	InsertCode( Editor, iEdPos, sComment );
end;

procedure TKMethodComment.CommentMethod( const MethodName: string;
	Method: TMethod; MethodData: PTypeData );
begin
	ForceTrimStr( MethodName );
	ForcePointer( MethodData );
	ForceMethod( Method );
	FMethodName := MethodName;
	FMethod := Method;
	FMethodData := MethodData;
	ExecuteEditorInterfaceByMethod( ChangeFileExt( ToolServices.GetCurrentFile,
		DELPHI_UNIT_EXT ), MakeComment, True, nil );
	FMethodName := '';
	ZeroMemory( @FMethod, SizeOf( TMethod ) );
	FMethodData := nil;
end;

{ TKMethodProperty }

destructor TKMethodProperty.Destroy;
begin
	FMethodComment.Free;
	inherited Destroy;
end;

procedure TKMethodProperty.Initialize;
begin
	inherited Initialize;
	FMethodComment := TKMethodComment.Create( Self );
end;

procedure TKMethodProperty.Edit;
var
	FormMethodName: string;
	Method: TMethod;
begin
	if MethodCommentsEnabled then
	begin
		FormMethodName := GetValue;
		if ( not CheckTrimStr( FormMethodName ) ) or
			Designer.MethodFromAncestor( GetMethodValue ) then
		begin
			if ( not CheckTrimStr( FormMethodName ) ) then
				FormMethodName := GetFormMethodName;
			if ( not CheckTrimStr( FormMethodName ) ) then
				RaiseException( EKMethodProperty, SCannotCreateName );
			Method := Designer.CreateMethod( FormMethodName, GetTypeData( GetPropType ) );
			SetMethodValue( Method );
{
	The CommentMethod method was called after the ShowMethod because at this time the
	IDE CurrentFile is the source file of the edited form. This sometimes ("imprevissível")
	cause an access violation in the "DFWEdit.dll" file.
	To avoid this, we decided to call this method BEFORE the ShowMethod of the designer,
	so we need to adjust the current(form)file (.dfm) to point to the correspondent source
	file (.p as). There is no problem in forcing this (see TKMethodComment.CommentMethod)
	because the IDE form/source files MUST BE synchronized with the same name signature.
}
			fMethodComment.CommentMethod( FormMethodName, Method, GetTypeData( GetPropType ) );
			Designer.Modified;
			Designer.ShowMethod( FormMethodName );
		end
		else
			Designer.ShowMethod( FormMethodName );
	end
	else
		inherited Edit;
end;

{ TKIMethodExpert }

function TKIMethodExpert.GetStateFlags: TIMenuFlags;
const
	FLAG_PATTERN: array[Boolean] of TIMenuFlags = ( [mfEnabled, mfVisible,
		mfChecked], [mfEnabled, mfVisible, mfChecked] );
begin
	Result := FLAG_PATTERN[AnyPackage_ShareWare];
end;

procedure TKIMethodExpert.AdjustRegistry( Enable: Boolean );
var
	Reg: TRegIniFile;
begin
	Reg := TRegIniFile.Create( sKnowHowBaseInfoRegKey );
	try
		Reg.WriteBool( sKnowHow_IDEConfig, sIDEEnableComment, Enable );
	finally
		Reg.Free;
	end;
end;

procedure TKIMethodExpert.Initialize;
var
	Flags: TIMenuFlags;
begin
  inherited Initialize;
	Name := sMethodExptName;
	Flags := GetStateFlags;
	if MethodCommentsEnabled then
		Include( Flags, mfChecked )
	else
		Exclude( Flags, mfChecked );
	InstallExpert( iaAfter, sMethodExptTargetMenuPoint, sMethodExptMenuText,
		sMethodExptMenuName, sMethodExptMenuHint, 0, 0, 0, GetStateFlags );
end;

procedure TKIMethodExpert.MenuClick( Sender: TIMenuItemIntf );
const
	FLAG_PATTERN: array[Boolean] of TIMenuFlags = ( [], [mfChecked] );
begin
	AdjustRegistry( ( not ( mfChecked in ViewMenuFlags ) ) );
	ChangeMenuFlags[[mfChecked]] := FLAG_PATTERN[( not ( mfChecked in ViewMenuFlags ) )];
end;

{
================================================================================
========================= Component Editor Support =============================
================================================================================
}

{
--------------------------------------------------------------------------------
------------------------------- TKDefultEditor ---------------------------------
--------------------------------------------------------------------------------
}

procedure TKDefaultEditor.EditProperty( PropertyEditor: TPropertyEditor;
	var Continue, FreeEditor: Boolean );
var
	PropName: string;
begin
	PropName := PropertyEditor.GetName;
	if CheckStrEqual( PropName, GetComparePropName ) then
	begin
		PropertyEditor.Edit;
		Continue := False;
	end
	else
		inherited EditProperty( PropertyEditor, Continue, FreeEditor );
end;

{
================================================================================
=========================== Custom Module Support ==============================
================================================================================
}

{
--------------------------------------------------------------------------------
------------------------------- TKCustomModule ---------------------------------
--------------------------------------------------------------------------------
}

constructor TKCustomModule.Create( ARoot: {$IFDEF DELPHI4}IComponent{$ELSE}TComponent{$ENDIF} );
const
	CUSTOM_MODULE_ATTRIBUTES: array[Boolean] of TCustomModuleAttributes =
	  ( [], [cmaVirtualSize] );
begin
	inherited Create( ARoot );
	FCustomModuleAttributes := CUSTOM_MODULE_ATTRIBUTES[{$IFDEF DELPHI4}
		( CheckInterface( ARoot ) and ARoot.IsWinControl )
		{$ELSE}CheckObjectClass( ARoot, TWinControl ){$ENDIF}];
end;

function TKCustomModule.GetAttributes: TCustomModuleAttributes;
begin
	Result := FCustomModuleAttributes;
end;

function TKCustomModule.GetRootComponent: TComponent;
begin
{$IFDEF DELPHI4}
	Result := TryExtractComponent( Root );
{$ELSE}
	Result := Root;
{$ENDIF}
end;

function TKCustomModule.CheckDesigner: Integer;
begin
	Result := NO_DESIGNER;
	if CheckObject( RootComponent ) then
		if ( CheckObjectClass( RootComponent, TCustomForm ) and
			CheckDesignerClass( ( RootComponent as TCustomForm ).Designer{$IFNDEF DELPHI4}, TFormDesigner {$ENDIF} ) ) then
			Result := FORM_DESIGNER
		else if ( CheckObject( RootComponent.Owner ) and
			CheckObjectClass( RootComponent.Owner, TCustomForm ) and
			CheckDesignerClass( ( RootComponent as TCustomForm ).Designer{$IFNDEF DELPHI4}, TFormDesigner {$ENDIF} ) ) then
			Result := COMP_DESIGNER;
end;

function TKCustomModule.GetDesigner: TKFormDesigner;
begin
	case CheckDesigner of
		FORM_DESIGNER : Result := ( ( RootComponent as TCustomForm ).Designer as TKFormDesigner );
		COMP_DESIGNER : Result := ( ( RootComponent.Owner as TCustomForm ).Designer as TKFormDesigner );
	{ NO_DESIGNER   : Result := nil; }
	else
		Result := nil;
	end
end;

{
--------------------------------------------------------------------------------
--------------------------- DateTime Property Editors --------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

{$M+}
	TKDateTimeKind = ( dtkDay, dtkMonth, dtkYear, dtkMSec, dtkSec, dtkMin, dtkHour );
{$M-}

{ TKDateTimeHackProperty }

	TKDateTimePartProperty = class( TPropertyEditor )
	private
		FKind: TKDateTimeKind;

		function GetPartName( AKind: TKDateTimeKind ): string;
		
	public
		function GetName: string; override;
		function GetValue: string; override;
		procedure SetValue( const Value: string ); override;

	end;

function TKDateTimePartProperty.GetPartName( AKind: TKDateTimeKind ): string;
begin
	Result := Copy( EnumName( Cardinal( AKind ), TypeInfo( TKDateTimeKind ) ), 4, MaxInt );
end;

function TKDateTimePartProperty.GetName: string;
begin
	Result := GetPartName( FKind );
end;

function TKDateTimePartProperty.GetValue: string;
var
	iValue: Word;
	dt: TDateTime;
begin
	Result := '';
	dt := GetFloatValue;
	iValue := Ord( FKind <= dtkYear );
	case FKind of
		dtkDay   : iValue := DatePart( dt, dpDay );
		dtkMonth : iValue := DatePart( dt, dpMonth );
		dtkYear  : iValue := DatePart( dt, dpYear );
		dtkMSec  : iValue := TimePart( dt, tpMSecond );
		dtkSec   : iValue := TimePart( dt, tpSecond );
		dtkMin   : iValue := TimePart( dt, tpMinute );
		dtkHour  : iValue := TimePart( dt, tpHour );
	end;
	Result := IntToStr( iValue );
end;

procedure TKDateTimePartProperty.SetValue( const Value: string );
var
	iMax,
	iValue: Word;
	dt: TDateTime;
	aValues: array[dtkDay..dtkYear] of Word;
begin
	dt := GetFloatValue;
	iValue := StrToIntDef( Value, Ord( FKind <= dtkYear ) );
	if ( FKind <= dtkYear ) then
	begin
		aValues[dtkDay] := DatePart( dt, dpDay );
		aValues[dtkMonth] := DatePart( dt, dpMonth );
		aValues[dtkYear] := DatePart( dt, dpYear );
		aValues[FKind] := iValue;
		if ( not ValueBetween( aValues[dtkYear], Low( TKYear ), High( TKYear ), True ) ) then
			RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkYear ),
				Low( TKYear ), High( TKYear )] );
		if ( not ValueBetween( aValues[dtkMonth], Low( TKMonth ), High( TKMonth ), True ) ) then
			RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkMonth ),
				Low( TKMonth ), High( TKMonth )] );
		iMax := DAYS_IN_MONTH[IsLeapYear( aValues[dtkYear] ), aValues[dtkMonth]];
		if ( not ValueBetween( aValues[dtkDay], Low( TKDay ), iMax, True ) ) then
			RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkDay ),
				Low( TKDay ), iMax] );
	end;
	case FKind of
		dtkDay   : SetDatePart( dt, dpDay, iValue );
		dtkMonth : SetDatePart( dt, dpMonth, iValue );
		dtkYear  : SetDatePart( dt, dpYear, iValue );
		dtkMSec  :
			if ( not ValueBetween( iValue, Low( TKMSeconds ), High( TKMSeconds ), True ) ) then
				RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkMSec ),
					Low( TKMSeconds ), High( TKMSeconds )] )
			else
				SetTimePart( dt, tpMSecond, iValue );
		dtkSec   :
			if ( not ValueBetween( iValue, Low( TKSeconds ), High( TKSeconds ), True ) ) then
				RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkSec ),
					Low( TKSeconds ), High( TKSeconds )] )
			else
				SetTimePart( dt, tpSecond, iValue );
		dtkMin   :
			if ( not ValueBetween( iValue, Low( TKMinute ), High( TKMinute ), True ) ) then
				RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkMin ),
					Low( TKMinute ), High( TKMinute )] )
			else
				SetTimePart( dt, tpMinute, iValue );
		dtkHour  :
			if ( not ValueBetween( iValue, Low( TKHour ), High( TKHour ), True ) ) then
				RaiseExceptionFmt( EKSYDClasses, sErrInvDateTimePart, [GetPartName( dtkHour ),
					Low( TKHour ), High( TKHour )] )
			else
				SetTimePart( dt, tpHour, iValue );
	end;
	SetFloatValue( dt );
end;

{---------------------------- Public Implementation ----------------------------}

{ TKDateTimeProperty }

procedure TKDateTimeProperty.GetDateProperties( Proc: TGetPropEditProc );
var
	i: TKDateTimeKind;
	kpe: TKPropertyEditor;
begin
	for i := dtkDay to dtkYear do
	begin
		kpe := TKPropertyEditor.Create( TKDateTimePartProperty, Designer, PropCount );
		try
			( kpe.Editor as TKDateTimePartProperty ).FKind := i;
			kpe.SetPropEntry( 0, GetComponent( 0 ), GetPropInfo );
			Proc( kpe.Editor );
		finally
			kpe.Free;
		end;
	end;
end;

procedure TKDateTimeProperty.GetTimeProperties( Proc: TGetPropEditProc );
var
	i: TKDateTimeKind;
	kpe: TKPropertyEditor;
begin
	for i := dtkHour downto dtkMSec do
	begin
		kpe := TKPropertyEditor.Create( TKDateTimePartProperty, Designer, PropCount );
		try
			( kpe.Editor as TKDateTimePartProperty ).FKind := i;
			kpe.SetPropEntry( 0, GetComponent( 0 ), GetPropInfo );
			Proc( kpe.Editor );
		finally
			kpe.Free;
		end;
	end;
end;

function TKDateTimeProperty.GetAttributes: TPropertyAttributes;
begin
	Result := ( inherited GetAttributes + [paSubProperties] );
end;

function TKDateTimeProperty.GetDateTimeFormat: string;
begin
  Result := ShortDateFormat + ' ' + ShortTimeFormat; 
end;

function TKDateTimeProperty.GetValue: string;
begin
	Result := FormatDateTime( GetDateTimeFormat, GetFloatValue );
end;

procedure TKDateTimeProperty.SetValue( const Value: string );
begin
	SetFloatValue( StrToDateTime( Value ) );
end;

procedure TKDateTimeProperty.GetProperties( Proc: TGetPropEditProc );
begin
	GetDateProperties( Proc );
	GetTimeProperties( Proc );  
end;

{ TKDateProperty }

procedure TKDateProperty.GetTimeProperties( Proc: TGetPropEditProc );
begin
  { do not create the time properties }
end;

function TKDateProperty.GetDateTimeFormat: string;
begin
	Result := ShortDateFormat;
end;

procedure TKDateProperty.SetValue( const Value: string );
begin
	SetFloatValue( StrToDate( Value ) );
end;

{ TKTimeProperty }

procedure TKTimeProperty.GetDateProperties( Proc: TGetPropEditProc );
begin
	{ do not create the date properties }
end;

function TKTimeProperty.GetDateTimeFormat: string;
begin
	Result := ShortTimeFormat;
end;

procedure TKTimeProperty.SetValue( const Value: string );
begin
	SetFloatValue( StrToTime( Value ) );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
end;

procedure Done;
begin
	FreeMethodComments;
	RegisterMethodCommentProc := nil;
	ClearFileSystemList;
	ClearModuleNotifierList;
	FreeClean( ShortCutList );
end;

initialization
	Init;

finalization
	Done;

end.
