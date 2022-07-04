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

unit ukrEngines;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, SysUtils, Classes, uksyConsts, uksyTypes, uksyUtils,
  uksyClasses, ukrConsts, ukrClasses;

const

{ Property Indexes }
	FIXED_PERSIST_FLD 		= 0;
	PERSIST_FLD 					= 1;
	VIRTUAL_NOPERSIST_FLD = 2;

{ Property Default Values }
	SIZE_500KB = ( 500 * KB );

type

	EKREngines = class( EKKernel );
	EKLogAPI = class( EKREngines );

{
---------------------------------------------------------------------------------
------------------------- Generic Log File Architecture -------------------------
---------------------------------------------------------------------------------
}

	EKLogFile = class( EKLogAPI );

	TKGetFixedFieldProc = function: string of object;
	TKGetVirtualFieldProc = function( Index: Integer ): string of object;

	TKCustomLogFile = class;

	TKLogErrorAction = ( laSkip, laRetry, laRaise );

	TKLogFileErrorEvent = procedure( Sender: TKCustomLogFile; Data: Pointer;
		var LogErrorAction: TKLogErrorAction ) of object;

{ TKCustomLogFile }

	TKCustomLogFile = class( TPersistent )
	private
		FVersion: Integer;
		FSignature: Integer;
		FRecordCount: Integer;
		FFileName: TFileName;
		FOwner: TComponent;
		FHandle: THandle;
		FCreateToClean: Boolean;
		FFixedFields: TStringList;
		FCustomFields: TStringList;
		FDefFileExt: string;
		FInternalMessage: string;
		FOnLogFileLogError: TKLogFileErrorEvent;

		function GetUserName: string;
		function GetDateTime: string;
		function GetAppHandle: string;
		function GetComputerName: string;
		function GetInternalMessage: string;

		function GetHeaderSize: Integer;
		function GetLogRecordSize: Integer;
		function GetFieldCount: Integer;
		function GetFixedFieldCount: Integer;

		function MakeLogEntry( sl: TStrings ): string;

	protected
    constructor CreateToClean( const AFileName: TFileName ); virtual;

		procedure CreateHandle; virtual; 
		procedure HandleNeeded; dynamic;
		function GetHandle: THandle; virtual;

		procedure SetFileName( const Value: TFileName ); virtual;
		procedure SetDefFileExt( const Value: string ); virtual;

		function GetLogOwner: TComponent;

		procedure InternalLogData( LogRecord: TStrings; Data: Pointer ); virtual;

		procedure DoOnLogFileLogError( Data: Pointer;
		  var LogErrorAction: TKLogErrorAction ); dynamic;

		procedure WriteHeader( Stream: TStream );
		procedure WriteSignature( Stream: TStream );
		procedure WriteVersion( Stream: TStream );
		procedure WriteFieldCount( Stream: TStream );
		procedure WriteFieldsTable( Stream: TStream );
		procedure WriteRecordCount( Stream: TStream );

		procedure ValidateHeader( Stream: TStream );
		procedure ValidateFieldCount( Stream: TStream );
		procedure ValidateFieldsTable( Stream: TStream );
		procedure ValidateRecordCount( Stream: TStream );

		procedure ValidateLogRecord( LogRecord: TStrings );
		procedure AdjustRecordCount( Stream: TStream );
		procedure AddLogEntry( Stream: TStream; const LogEntry: string );

		procedure DefineFixedField( const AFieldName: TFieldName; Size: Byte;
			Proc: TKGetFixedFieldProc );
		procedure DefineFixedFields; dynamic;
		procedure DefineCustomField( const AFieldName: TFieldName; Size: Byte );
		procedure DefineCustomFields; virtual;

		property Handle: THandle
						 read GetHandle write FHandle;
		property HeaderSize: Integer
						 read GetHeaderSize;
		property LogRecordSize: Integer
						 read GetLogRecordSize;
		property FieldCount: Integer
						 read GetFieldCount;
		property FixedFieldCount: Integer
						 read GetFixedFieldCount;
		property FileName: TFileName
						 read FFileName write SetFileName;
		property DefFileExt: string
						 read FDefFileExt write SetDefFileExt;
		property Owner: TComponent
						 read GetLogOwner;
		property InternalMessage: string
						 read GetInternalMessage;
		property OnLogFileLogError: TKLogFileErrorEvent
						 read FOnLogFileLogError write FOnLogFileLogError;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent; const AFileName: TFileName ); virtual;

		function CreateEmptyLogRecord: TStrings; virtual;
		procedure LogData( LogRecord: TStrings; Data: Pointer ); dynamic; 

	end;

	TKCustomLogFileClass = class of TKCustomLogFile;

{ TKLogFile }

	TKLogFile = class( TKCustomLogFile )
	public
		property Owner;
		property DefFileExt;
		property FieldCount;
		property FileName;
		property FixedFieldCount;
		property LogRecordSize;
		property OnLogFileLogError;

	end;

{
---------------------------------------------------------------------------------
------------------------ Generic Log Engine Architecture ------------------------
---------------------------------------------------------------------------------
}

	EKLogEngine = class( EKLogAPI );

	TLogClearType = ( lctAll, lctEntries );
	TClearDirection = ( cdFromBegining, cdFromEnd );
	TAutoClearLogType = ( aclDate, aclSizeFromBeginning, aclSizeFromEnd );

  TKLogEngineFieldKind = ( lefkFixed, lefkVirtual, lefkCustom );
  TKLogEngineFieldKinds = set of TKLogEngineFieldKind;

{ TKCustomLogEngine }

	TKCustomLogEngine = class( TKCustomLinkable )
	private
		FFileName: TFileName;
		FAutoLoad: Boolean;
		FDefFileExt: string;
		FSignature: Integer;
		FVersion: Integer;
		FDaysToKeep: Byte;
		FMaxByteSize: Cardinal;
		FAutoClear: Boolean;
		FFields: TList;
		FVirtualFields: TStrings;
		FAutoClearLogType: TAutoClearLogType;
		FRecordCount : Integer;
		FFieldCount : Integer;
		FFixedFieldCount: Integer;
		FRecords: TStrings;

    function IsDesigning: Boolean;
		function GetLogRecordSize: Integer;
		function GetHeaderSize: Integer;
		function GetLogRecords( const FieldName: TFieldName; Index: Integer ): string;
		function GetLogFields( Index: Integer ): TFieldName;
		function GetLogFieldsSize( Index: Integer ): Word;
		function GetFileLoaded: Boolean;
		function GetFieldCount( Index: Integer ): Integer;

		function GetVirtualRecords( const vFldName: TFieldName; Index : Integer ): string;
		function GetVirtualFields( Index: Integer ): TFieldName;
		function GetVirtualFieldsSize( Index: Integer ): Word;

		function GetHighFieldLength: Byte;

		procedure ValidateRecordCount( Stream: TStream; NewRecordCount: Integer );
		procedure WriteRecordCount( Stream: TStream; NewRecordCount: LongInt );
		procedure AdjustRecordCount( LogFile: TKCustomLogFile;
			Stream: TStream; NewRecordCount: Integer );

	protected
		procedure SetFileName( const Value: TFileName ); virtual;

		procedure AutoCleanLog; dynamic;

		function GetLogFileClass: TKCustomLogFileClass; virtual; abstract;

		procedure ClearFieldsList; dynamic;
		procedure ClearRecordsList; dynamic;
		procedure CheckFields( IsEmpty: Boolean ); dynamic;
		procedure CheckRecords( IsEmpty: Boolean ); dynamic;

		procedure LoadFromStream( Stream: TStream ); virtual;
		procedure ValidateHeader( Stream: TStream ); virtual;
		procedure ReadFieldCount( Stream: TStream ); virtual;
		procedure ReadFieldsTable( Stream: TStream ); virtual;

		procedure ReadRecordCount( Stream: TStream ); virtual;
		procedure ReadLogRecords( Stream: TStream ); virtual;
		procedure ReadLogFields( Buffer: string ); virtual;

		procedure DefineVirtualFields; virtual;
		procedure DefineVirtualField( const AFieldName: TFieldName; Size: Word;
			Proc: TKGetVirtualFieldProc );

		property VirtualRecords[const vFldName: TFieldName; Index: Integer]: string
						 read GetVirtualRecords;
		property VirtualFields[Index: Integer]: TFieldName
						 read GetVirtualFields;
		property VirtualFieldsSize[Index: Integer]: Word
						 read GetVirtualFieldsSize;
		property DefFileExt: string
						 read FDefFileExt write FDefFileExt;

		property AutoClearLog: Boolean
						 read FAutoClear write FAutoClear default False;
		property AutoClearLogType: TAutoClearLogType
						 read FAutoClearLogType write FAutoClearLogType default aclDate;
		property AutoLoad: Boolean
						 read FAutoLoad write FAutoLoad default False;
		property DaysToKeep: Byte
						 read FDaysToKeep write FDaysToKeep default WEEK_TO_DAY;
		property FileName: TFileName
						 read FFileName write SetFileName;
		property HeaderSize: Integer
						 read GetHeaderSize;
		property HighFieldLengh: Byte
						 read GetHighFieldLength;
		property FieldCount: Integer
						 index PERSIST_FLD read GetFieldCount;
		property FileLoaded: Boolean
						 read GetFileLoaded;
		property FixedFieldCount: Integer
						 index FIXED_PERSIST_FLD read GetFieldCount;
		property LogRecords[const FieldName: TFieldName; Index: Integer]: string
						 read	GetLogRecords;
		property LogFields[Index: Integer]: TFieldName
						 read GetLogFields;
		property LogFieldsSize[Index: Integer]: Word
						 read GetLogFieldsSize;
		property LogRecordSize: Integer
						 read GetLogRecordSize;
		property MaxByteSize: Cardinal
						 read FMaxByteSize write FMaxByteSize default SIZE_500KB;
		property RecordCount: Integer
						 read FRecordCount;
		property VirtualFieldCount: Integer
						 index VIRTUAL_NOPERSIST_FLD read GetFieldCount;
                   
	public
		constructor Create( AOwner : TComponent ); override;
		destructor Destroy; override;

		procedure LoadFromFile( const AFileName: TFileName ); virtual;

		procedure RecordsFillStrings( ss: TStrings; RecCount: Integer;
			ListSeparator: Char ); virtual;
		procedure FieldsFillStrings( ss: TStrings; ListSeparator: Char ); virtual;
    procedure RetrieveFields( sl: TStrings; FieldKinds: TKLogEngineFieldKinds ); virtual;
    procedure RetrieveRecord( sl: TStrings; RecNo: Integer;
      FieldKinds: TKLogEngineFieldKinds ); virtual;

		procedure Clear( LogClearType: TLogClearType ); virtual;

		procedure ClearByDate( From_Creation_To_DateTime: TDateTime ); virtual;
		procedure ClearBySize( AMaxByteSize: Cardinal;
			ADirection: TClearDirection ); virtual;

	end;

{ TKCustomLogEngineLink }

	TKCustomLogEngineLink = class( TKCustomLink )
	protected
		procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	end;

{
---------------------------------------------------------------------------------
------------------------- Generic Auditory Architecture -------------------------
---------------------------------------------------------------------------------
}

	EKAuditory = class( EKLogAPI );

	TKAuditEvent = type Byte;
	TKCustomAuditable = class;

{ TKCustomAuditoryLog }

	TKCustomAuditoryLog = class( TKCustomLogFile )
	protected
		procedure DefineFixedFields; override;
		procedure CreateHandle; override;

		procedure InternalLogData( LogRecord: TStrings; Data: Pointer ); override;

		function GetLogOwner: TKCustomAuditable;
		function GetAppUserName: string;

	public
		constructor Create( AOwner: TComponent; const AFileName: TFileName ); override;

		property DefFileExt;
		property FieldCount;
		property FileName;
		property FixedFieldCount;
		property LogRecordSize;

		property Owner: TKCustomAuditable
						 read GetLogOwner;

	end;

{ TKCustomAuditoryLogClass }

	TKCustomAuditoryLogClass = class of TKCustomAuditoryLog;

	TKCustomAuditoryLogEngine = class( TKCustomLogEngine )
	{$IFDEF DELPHI4}
	protected
	{$ELSE}
	private
	{$ENDIF}
		function GetLogFileClass: TKCustomLogFileClass; override;

	protected
		function GetAuditoryLogClass: TKCustomAuditoryLogClass; virtual; abstract;

	end;

	TKAuditoryEvent = procedure( Sender: TObject; Event: TKAuditEvent;
		WParam, LParam: LongInt ) of object;

{ TKCustomAuditable }

	TKCustomAuditable = class( TKCustomLinkable )
	private
		FEnabled: Boolean;
		FAppUserName: string;

		FAuditoryEvent: TKAuditoryEvent;
		FKAuditoryLog: TKCustomAuditoryLog;

		function GetHandle: THandle;

	protected
		procedure Notification( AComponent: TComponent;
			AOperation: TOperation ); override;

		function WndProc( var Message: TMessage ): Boolean; virtual;

		function GetAuditoryLogClass: TKCustomAuditoryLogClass; virtual; abstract;
		function GetAuditoryLog: TKCustomAuditoryLog;

		function GetAuditoryFileName: TFileName;
		function GetAppUserName: string;
		procedure SetAuditoryFileName( const Value: TFileName );
		procedure SetAppUserName( const Value: string );

		procedure DoAuditory( Event: TKAuditEvent; WParam, LParam: LongInt ); virtual;
		procedure DoRequestFileName( Data: Pointer ); virtual;

		property AuditoryLog: TKCustomAuditoryLog
						 read GetAuditoryLog;

		property OnAuditory: TKAuditoryEvent
						 read FAuditoryEvent write FAuditoryEvent;

		property AppUserName: string
						 read FAppUserName write SetAppUserName;
		property Enabled: Boolean
						 read FEnabled write FEnabled default true;
		property FileName: TFileName
						 read GetAuditoryFileName write SetAuditoryFileName;
		property Handle: THandle
						 read GetHandle;
						 				 
	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

	TKCustomAuditableClass = class of TKCustomAuditable;

{ TKCustomAggregateAuditable }

	TKCustomAggregateAuditable = class( TPersistent )
	private
		FAuditComp: TKCustomAuditable;
		FAuditorClass: TKCustomAuditableClass;

		function GenericBoolGet( Index: Integer ): Boolean;
		function GenericStrGet( Index: Integer ): string;
		procedure GenericBoolSet( Index: Integer; Value: Boolean );
		procedure GenericStrSet( Index: Integer; const Value: string );

	protected
		function GetAuditoryOwner: TKCustomAuditable;

		property AuditorClass: TKCustomAuditableClass
						 read FAuditorClass;

	public
		constructor Create( AClass: TKCustomAuditableClass ); virtual;
		destructor Destroy; override;

		procedure Assign( Source: TPersistent ); override;

		property Owner: TKCustomAuditable
						 read GetAuditoryOwner;

	published
		property AppUserName: string
						 index 0 read GenericStrGet write GenericStrSet;
		property Enabled: Boolean
						 index 0 read GenericBoolGet write GenericBoolSet default true;

	end;

{
---------------------------------------------------------------------------------
--------------- Generic Stream Data ( Bin Flat File ) Architecture ----------------
---------------------------------------------------------------------------------
}

	EKStreamData = class( EKREngines );

	TKStreamData = class;
	TKStreamDataClass = class of TKStreamData;

  { Stream Data events }

	TKNormalizeEvent = procedure( Sender: TObject; Data: Pointer;
		const NormalizeID: Integer; var Continue: Boolean ) of object;
	TKBufferChangeEvent = procedure ( Sender: TKStreamData;
		OldData, NewData: Pointer ) of object;

	{ Stream Data CallBacks }

	TKStreamCompareBytesProc = function( Pos1, Pos2,
		Count: Integer ): ShortInt of object;
	TKStreamCompareBufferProc = function( const ABuffer; Pos,
		Count: Integer ): ShortInt of object;
	TKStreamSwapProc = procedure( Po1, Pos2, Count: Integer ) of object;

{ TKStreamData }

	TKStreamData = class( TObject )
	private
		FBuffer: Pointer;
		FOwner: TStream;
		FOnNormalize: TKNormalizeEvent;
		FOnBufferChange: TKBufferChangeEvent;

		function GetAsBoolean: Boolean;
		function GetAsCurrency: Currency;
		function GetAsDateTime: TDateTime;
		function GetAsFloat: Double;
		function GetAsInteger: Longint;
		function GetAsWord: Word;
		function GetAsByte: Byte;
		function GetAsString( Count: Integer ): string;

		function GetDataString: string;

		procedure SetAsBoolean( Value: Boolean );
		procedure SetAsCurrency( Value: Currency );
		procedure SetAsDateTime( Value: TDateTime );
		procedure SetAsFloat( Value: Double );
		procedure SetAsInteger( Value: Longint );
		procedure SetAsWord( Value: Word );
		procedure SetAsByte( Value: Byte );
		procedure SetAsString( Count: Integer; const Value: string );

		function GetInt( Index: Integer ): Integer;
		procedure SetInt( Index: Integer; const Value: Integer );

		procedure CheckBuffer;
		procedure CheckOwner;
		procedure CheckNilBuffer;
		procedure CheckOwnerClass;

	protected
		FBufferModified : Boolean;
		FBufferFilled: Boolean;

		class function GetBufferSize: Cardinal; virtual;

		function GetBufferModified: Boolean; virtual;
		function GetBufferFilled: Boolean; virtual;

		function GetRecCount: Integer; virtual;
		procedure SetBuffer( Value: Pointer ); virtual;
		function GetBuffer: Pointer; virtual;
		function CompareBytes( Pos1, Pos2, Count: Integer ): ShortInt; dynamic;
		function CompareBuffer( const ABuffer; Pos, Count: Integer ): ShortInt; dynamic;

		function DoNormalize( const NormalizeID: Integer; Data: Pointer ): Boolean; dynamic;
		procedure DoBufferChange( OldData, NewData: Pointer ); dynamic;

		property Owner: TStream
						 read FOwner write FOwner;

	public
		destructor Destroy; override;

		constructor Create( AOwner: TStream ); virtual;
		constructor CreateDefault( AOwner: TStream; ABuffer: Pointer ); virtual;
		constructor CreateLinked( AOwner: TStream; AEvent: TKNormalizeEvent ); virtual;

		procedure InitBuffer; virtual;
		procedure AssignBufferTo( Dest: TKStreamData ); virtual;
		procedure AssignBufferFrom( Source: TKStreamData ); virtual;
		procedure AssignData( Source: Pointer ); virtual;

		procedure Clear; dynamic;

		function DupBuffer: Pointer; dynamic;
		function DupBufferEx( AStreamDataClass: TKStreamDataClass ): TKStreamData; dynamic;
		function GetBufferAt( Pos, Count: Integer ): Pointer; dynamic;

		function Seek( Offset: Integer; Origin: Word ): Integer; virtual;

		function ReadData( Pos: Integer; var ABuffer; Count: Integer ): Integer; dynamic;
		function WriteData( Pos: Integer; const ABuffer; Count: Integer ): Integer; dynamic;
		procedure ReadDataBuffer( Pos: Integer; var ABuffer; Count: Integer ); dynamic;
		procedure WriteDataBuffer( Pos: Integer; const ABuffer; Count: Integer ); dynamic;
		procedure QuickSortBytes( StartPos, EndPos, Count: Integer;
			CompareProc: TKStreamCompareBufferProc; SwapProc: TKStreamSwapProc ); dynamic;
		function BinarySearchBytes( const ABuffer; StartPos, EndPos,
			Count: Integer; CompareProc: TKStreamCompareBufferProc ) : Boolean; dynamic;

		procedure DeleteBytes( Pos, Count: Integer ); dynamic;
		procedure InsertBytes( Pos: Integer; const ABuffer; Count: Integer ); dynamic;
		procedure ExchangeBytes( Pos1, Pos2, Count: Integer ); dynamic;
		function LocateBytes( const Buffer; Count: Integer ): Boolean; dynamic;

		function EOF: Boolean; virtual;
		function BOF: Boolean; virtual;

		function Write: Integer; virtual;
		function Read: Integer; virtual;
		procedure WriteBuffer;
		procedure ReadBuffer;
		procedure WriteAt( Pos: Integer );
		procedure ReadAt( Pos: Integer );
		procedure StaticWrite;
		procedure StaticRead;

		procedure WriteTo( Stream: TStream ); virtual;
		procedure ReadFrom( Stream: TStream ); virtual;

		procedure Delete; virtual;
		procedure DeleteAt( Pos: Integer );

		procedure Insert; virtual;
		procedure InsertAt( Pos: Integer );

		procedure Append; virtual;

		procedure Exchange( Pos: Integer ); virtual;
		procedure ExchangeAt( Origin, Pos: Integer );

		procedure Prior; virtual;
		procedure Next; virtual;
		procedure First; virtual;
		procedure Last; virtual;
		function MoveBy( Distance: Integer ): Integer; virtual;

		function Locate( ABuffer: Pointer ): Boolean; virtual;

		procedure QuickSort( StartPos, BuffCount: Integer;
			CompareProc: TKStreamCompareBufferProc; SwapProc: TKStreamSwapProc ); virtual;
		function BinarySearch( StartPos, BuffCount: Integer;
			CompareProc: TKStreamCompareBufferProc  ): Boolean; virtual;

		procedure NormalizeItems( StartPosOfs, BufStart, BufCount: Integer;
			Data: Pointer ); virtual;
		procedure NormalizeItemsEx( Stream: TStream; StartPosOfs, BufStart, BufCount: Integer;
			Data: Pointer ); virtual;

		property AsBoolean: Boolean
						 read GetAsBoolean write SetAsBoolean;
		property AsByte: Byte
						 read GetAsByte write SetAsByte;
		property AsCurrency: Currency
						 read GetAsCurrency write SetAsCurrency;
		property AsDateTime: TDateTime
						 read GetAsDateTime write SetAsDateTime;
		property AsFloat: Double
						 read GetAsFloat write SetAsFloat;
		property AsInteger: Longint
						 read GetAsInteger write SetAsInteger;
		property AsString[Count: Integer]: string
						 read GetAsString write SetAsString;
		property AsWord: Word
						 read GetAsWord write SetAsWord;
		property Buffer: Pointer
						 read GetBuffer write SetBuffer;
		property BufferSize: Cardinal
						 read GetBufferSize;
		property BufferModified: Boolean
						 read GetBufferModified;
		property BufferFilled: Boolean
						 read GetBufferFilled;
		property DataString: string
						 read GetDataString;
		property RecordCount: Integer
						 read GetRecCount;
		property Position: Integer
						 index 0 read GetInt write SetInt;
		property Size: Integer
						 index 1 read GetInt write SetInt;

		property OnNormalize: TKNormalizeEvent
						 read FOnNormalize write FOnNormalize;
		property OnBufferChange: TKBufferChangeEvent
						 read FOnBufferChange write FOnBufferChange;

	end;

const
  ALL_LOGENGINEFLDKINDS = [lefkFixed, lefkVirtual, lefkCustom];

implementation

uses
	Forms, uksyResStr, ukrResStr, ukrUtils;

{--------------------------- Internal Implementation ---------------------------}

type

	PKLogFieldInfo = ^TKLogFieldInfo;
	TKLogFieldInfo = record
		FieldName: TFieldName;
		FieldSize: Word;
	end;

procedure ValidateHeader( Stream: TStream; Signature, Version: Integer;
	EClass: EKnowHowClass );
var
	iCount,
	iVersion,
	iSignature: Integer;
begin
	Stream.Seek( 0, soFromBeginning );
	iCount := Stream.Read( iSignature, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EClass, sErrLFReadSignature );
	if ( iSignature <> Signature ) then
		RaiseException( EClass, sErrLFInvalidSignature );
	iCount := Stream.Read( iVersion, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EClass, sErrLFReadVersion );
	if ( iVersion <> Version ) then
		RaiseException( EClass, sErrLFInvalidVersion );
end;

procedure ValidateFieldsTable( Stream: TStream; FldList: TStrings;
	FldCount: Integer; EClass: EKnowHowClass );
var
	i,
	iCount: Integer;
	pki: PKLogFieldInfo;
begin
	if ( FldList.Count <> FldCount ) or ( FldCount <= 0 ) then
		RaiseException( EClass, sErrLFInvFldTblSize );
	pki := New( PKLogFieldInfo );
	try
		for i := 0 to FldCount - 1 do
		begin
			ZeroMemory( pki, SizeOf( TKLogFieldInfo ) );
			iCount := Stream.Read( pki^, SizeOf( TKLogFieldInfo ) );
			if ( iCount <> SizeOf( TKLogFieldInfo ) ) then
				RaiseException( EClass, sErrLFReadFieldsTable );
			if ( not CheckStrEqual( pki^.FieldName, FldList.Names[i] ) ) then
				RaiseExceptionFmt( EClass, sErrLFInvFldTblFldName, [pki^.FieldName,
					FldList.Names[i]] );
			if ( pki^.FieldSize <> StrToInt( FldList.Values[FldList.Names[i]] ) ) then
				RaiseExceptionFmt( EClass, sErrLFInvFldTblFldSize, [pki^.FieldName,
					pki^.FieldSize, FldList.Values[FldList.Names[i]]] );
		end;
	finally
		Dispose( pki );
	end;
end;

{
---------------------------------------------------------------------------------
------------------------- Generic Log File Architecture -------------------------
---------------------------------------------------------------------------------
}

{
	TKCustomLogFile
	---------------

	PROTECTED METHODS
	--------- -------

	» For derived classes that want to implement a different behaviour
		for filename and file name extension policy:

		procedure SetFileName( const Value: TFileName ); virtual;
		procedure SetDefFileExt( const Value: string ); virtual;

	» Get method for Owner property. Derived classes can subscribe this
		method to define a new Owner property with the proper type and gain
		access to the Private field through an inherited call to this method:

		function GetLogOwner: TComponent;

	» Validate the CustomFields in LogRecord. If the fields are not valid
		log empty custom fields and set InternalMessage to
		IM_INVALID_LOGRECORD:

		procedure ValidateLogRecord( LogRecord: TStrings );

	» Called in the constructor to create Fixed fields. Derive this
		method only if you have/need a fixed field, that is, you are going to
		give the log value by a callback function. Derive this method in a
		descendent class to add more fixed ( component provided ) fields to
		the log fields table ( it's seldom used. Only in special situations ).
		DON'T CALL IT DIRECTLY !

		procedure DefineFixedFields; dynamic;

	» Called in the constructor to create Custom fields. Here you just
		have to call DefineCustomField to add a custom field. Derive this
		method in a descendent class to add more field definitions.
		DON'T CALL IT DIRECTLY !

		procedure DefineCustomFields; virtual;

	PUBLIC METHODS
	------ -------

	» This function creates and returns a TStrings with the CustomFields
		as the Names so the user can fill in the Values:

		WARNING: It is the caller's responsibility to free the created
						 TStrings.

		function CreateEmptyLogRecord: TStrings;

	» Add a record to the log file. You can combine calls to:
		CreateEmptyLogRecord and FillLogRecord and LogData in that order.

		procedure LogData( LogRecord: TStrings );


	LOG FILE FORMAT: Header + LogEntries

	 ------------------- --------------- ---------  ----------- ----------
	| SIGNATURE+VERSION | FixedFldCount | FldCount | FldsTable | RecCount |
	 ------------------- --------------- ---------  ----------- ----------
	 ----------- -----------         -----------
	| LogEntry1 | LogEntry2 |       | LogEntryN |
	 ----------- -----------  .....  -----------

}

{--------------------------- Internal Implementation ---------------------------}

const
	LOG_VERSION_100   = 100;
	LOG_SIGNATURE_100 = $0A0B0C0D;

	IM_NOMESSAGE         = 'IM_NOMESSAGE';
	IM_INVALID_LOGRECORD = 'IM_INVALID_LOGRECORD';
	IM_ONLYFIXEDFIELDS   = 'IM_ONLYFIXEDFIELDS';

	KLOG_DATETIME_FORMAT = 'dd/mm/yyyy hh:nn:ss';

  USER_NAME_FIELD  = 'UserName';
	COMP_NAME_FIELD  = 'ComputerName';
	DATE_TIME_FIELD  = 'DateTime';
	APP_HANDLE_FIELD = 'AppHandle';
	INT_MSG_FIELD    = 'InternalMsg';

	USER_NAME_FIELD_LENGTH  = 20;
	COMP_NAME_FIELD_LENGTH  = 15;
	DATE_TIME_FIELD_LENGTH  = 19;
	APP_HANDLE_FIELD_LENGTH =  8;
	INT_MSG_FIELD_LENGTH    = 30;
  
type

	PKFixedFieldProc = ^TKFixedFieldProc;
	TKFixedFieldProc = record
		GetProc: TKGetFixedFieldProc;
	end;

{---------------------------- Public Implementation ----------------------------}

constructor TKCustomLogFile.Create( AOwner: TComponent; const AFileName: TFileName );
begin
{
  if ( not FCreateToClean ) then
  	ForceObject( AOwner );
}
	inherited Create;
	FOwner := AOwner;
	FFileName := AFileName;
	FInternalMessage := IM_NOMESSAGE;
	FVersion := LOG_VERSION_100;
	FSignature := LOG_SIGNATURE_100;
	FDefFileExt := K_DEF_LOGEXT;
	if ( FCreateToClean or ( not ( CheckObject( Owner ) and Designing( Owner ) ) ) ) then
	begin
		FFixedFields := TStringList.Create;
		FCustomFields := TStringList.Create;
		DefineFixedFields;
		DefineCustomFields;
	end;
end;

constructor TKCustomLogFile.CreateToClean( const AFileName: TFileName );
begin
  ForceTrimStr( AFileName );
	FCreateToClean := True;
	Create( nil, AFileName );
end;

destructor TKCustomLogFile.Destroy;
begin
	if ( FCreateToClean or ( not ( CheckObject( Owner ) and Designing( Owner ) ) ) ) then
	begin
		while CheckStrings( FFixedFields ) do
			with FFixedFields do
			begin
				Dispose( PKFixedFieldProc( Objects[Count - 1] ) );
				Objects[Count - 1] := nil;
				Delete( Count - 1 );
			end;
		FFixedFields.Free;
		FCustomFields.Free;
	end;
	inherited Destroy;
end;

{ Field Definitions }

procedure TKCustomLogFile.DefineFixedField( const AFieldName: TFieldName;
	Size: Byte; Proc: TKGetFixedFieldProc );
var
	i: Integer;
	pff: PKFixedFieldProc;
begin
	ForceTrimStr( AFieldName );
	ForceReference( @Proc );
	if ( Size = 0 ) then
		RaiseException( EKLogFile, sErrLFInvalidFldSize );
	if ( FFixedFields.IndexOfName( AFieldName ) <> -1 ) then
		RaiseExceptionFmt( EKLogFile, sErrLFDuplicatedFldName, [AFieldName] );
	i := -1;
	pff := New( PKFixedFieldProc );
	try
		ZeroMemory( pff, SizeOf( TKFixedFieldProc ) );
		pff^.GetProc := Proc;
		i := FFixedFields.AddObject( AFieldName + CH_EQUAL_TOKEN + IntToStr( Size ),
			TObject( pff ) );
	except
		Dispose( pff );
		if ( i > -1 ) then
			FFixedFields.Delete( i );
		raise;
	end;
end;

procedure TKCustomLogFile.DefineFixedFields;
begin
{ Do not resource Field Names }
	DefineFixedField( USER_NAME_FIELD, USER_NAME_FIELD_LENGTH, GetUserName );
	DefineFixedField( COMP_NAME_FIELD, COMP_NAME_FIELD_LENGTH, GetComputerName );
	DefineFixedField( DATE_TIME_FIELD, DATE_TIME_FIELD_LENGTH, GetDateTime );
	DefineFixedField( APP_HANDLE_FIELD, APP_HANDLE_FIELD_LENGTH, GetAppHandle );
{ For self corrected logs, here we'll go to put, for example, a invalid entry if log
	record not in the proper format ( based on CustomFields )! }
	DefineFixedField( INT_MSG_FIELD, INT_MSG_FIELD_LENGTH, GetInternalMessage );
end;

procedure TKCustomLogFile.DefineCustomField( const AFieldName: TFieldName;
	Size: Byte );
begin
	ForceTrimStr( AFieldName );
	if ( Size = 0 ) then
		RaiseException( EKLogFile, sErrLFInvalidFldSize );
	if ( FCustomFields.IndexOfName( AFieldName ) <> -1 ) then
		RaiseExceptionFmt( EKLogFile, sErrLFDuplicatedFldName, [AFieldName] );
	FCustomFields.Add( AFieldName + CH_EQUAL_TOKEN + IntToStr( Size ) );
end;

procedure TKCustomLogFile.DefineCustomFields;
begin
{
	let descendant objects implement this method:

		DefineCustomField( 'FieldName_1', FieldSize_1 );
		DefineCustomField( 'FieldName_2', FieldSize_2 );

	and so on...
}
end;

{ Gets/Sets Information }

procedure TKCustomLogFile.CreateHandle;
begin
	FHandle := 0;
end;

procedure TKCustomLogFile.HandleNeeded;
begin
	if ( not CheckHandle( FHandle ) ) then
	  CreateHandle;
end;

function TKCustomLogFile.GetHandle: THandle;
begin
	HandleNeeded;
	Result := FHandle;
end;

function TKCustomLogFile.GetLogOwner: TComponent;
begin
	Result := FOwner;
end;

function TKCustomLogFile.GetUserName: string;
begin
	Result := UserName;
end;

function TKCustomLogFile.GetComputerName: string;
begin
	Result := ComputerName;
end;

function TKCustomLogFile.GetDateTime: string;
begin
	Result := FormatDateTime( KLOG_DATETIME_FORMAT, Now );
end;

function TKCustomLogFile.GetAppHandle: string;
begin
	Result := IntToHex( Application.Handle, 8 );
end;

function TKCustomLogFile.GetInternalMessage: string;
begin
	Result := FInternalMessage;
end;

function TKCustomLogFile.GetLogRecordSize: Integer;
var
	i: Integer;
begin
	Result := 0;
	if ( FCreateToClean or ( not ( CheckObject( Owner ) and Designing( Owner ) ) ) ) then
	begin
		with FFixedFields do
			for i := 0 to Count - 1  do
				Inc( Result, StrToInt( Values[Names[i]] ) );
		with FCustomFields do
			for i := 0 to Count - 1  do
				Inc( Result, StrToInt( Values[Names[i]] ) );
	end;
end;

function TKCustomLogFile.GetFieldCount: Integer;
begin
	Result := 0;
	if ( FCreateToClean or ( not ( CheckObject( Owner ) and Designing( Owner ) ) ) ) then
	begin
		Inc( Result, FFixedFields.Count );
		Inc( Result, FCustomFields.Count );
	end;
end;

function TKCustomLogFile.GetFixedFieldCount: Integer;
begin
	Result := 0;
	if ( FCreateToClean or ( not ( CheckObject( Owner ) and Designing( Owner ) ) ) ) then
		Inc( Result, FFixedFields.Count );
end;

procedure TKCustomLogFile.SetDefFileExt( const Value: string );
begin
	if ( Value <> FDefFileExt ) then
	begin
		FDefFileExt := Value;
		if CheckTrimStr( FFileName ) then
			FFileName := ChangeFileExt( FFileName, FDefFileExt );
	end;
end;

procedure TKCustomLogFile.SetFileName( const Value: TFileName );
begin
	if ( not CheckStrEqual( Value, FFileName ) ) then
	begin
		FFileName := Value;
		if ( CheckTrimStr( FFileName ) and ( not CheckStr( ExtractFileExt( FFileName ) ) ) ) then
			FFileName := ChangeFileExt( FFileName, FDefFileExt );
	end;
end;

function TKCustomLogFile.GetHeaderSize: Integer;
begin
{ Signature + Version + FldCount + FixedFldCount + RecCount + FldsTbl }
	Result := SizeOf( FSignature ) + SizeOf( FVersion ) + SizeOf( Integer ) +
						SizeOf( Integer ) + SizeOf( Integer ) +
						( FieldCount * SizeOf( TKLogFieldInfo ) );
end;

{ Log streaming system }

function TKCustomLogFile.MakeLogEntry( sl: TStrings ): string;
var
	s: string;
	i: Integer;
begin
	Result := '';
	with FFixedFields do
		for i := 0 to Count - 1  do
		begin
			s := PKFixedFieldProc( Objects[I] )^.GetProc;
			Result := Result + StrAdjustRight( s, StrToInt( Values[Names[i]] ),
				CH_SPACE );
		end;
	with FCustomFields do
		for i := 0 to Count - 1  do
		begin
			s := sl.Values[Names[i]];
			Result := Result + StrAdjustRight( s, StrToInt( Values[Names[i]] ),
				CH_SPACE );
		end;
end;

procedure TKCustomLogFile.WriteHeader( Stream: TStream );
begin
	WriteSignature( Stream );
	WriteVersion( Stream );
end;

procedure TKCustomLogFile.WriteSignature( Stream: TStream );
var
	iCount: Integer;
begin
	iCount := Stream.Write( FSignature, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLFWriteSignature );
end;

procedure TKCustomLogFile.WriteVersion( Stream: TStream );
var
	iCount: Integer;
begin
	iCount := Stream.Write( FVersion, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLFWriteVersion );
end;

procedure TKCustomLogFile.WriteFieldCount( Stream: TStream );
var
	iCount,
	iFieldCount : Integer;
begin
	iFieldCount := FixedFieldCount;
	iCount := Stream.Write( iFieldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLFWriteFixedFieldCount );
	iFieldCount := FieldCount;
	iCount := Stream.Write( iFieldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLFWriteFieldCount );
end;

procedure TKCustomLogFile.WriteFieldsTable( Stream: TStream );
var
	i,
	iCount: Integer;
	pfi: PKLogFieldInfo;
begin
	pfi := New( PKLogFieldInfo );
	try
		with FFixedFields do
			for i := 0 to Count - 1 do
			begin
				ZeroMemory( pfi, SizeOf( TKLogFieldInfo ) );
				with pfi^ do
				begin
					FieldName := Names[i];
					FieldSize := StrToIntDef( Values[Names[i]], 0 );
				end;
				iCount := Stream.Write( pfi^, SizeOf( TKLogFieldInfo ) );
				if ( iCount <> SizeOf( TKLogFieldInfo ) ) then
					RaiseExceptionFmt( EKLogFile, sErrLFWriteField, [pfi^.FieldName] );
			end;
		with FCustomFields do
			for i := 0 to Count - 1 do
			begin
				ZeroMemory( pfi, SizeOf( TKLogFieldInfo ) );
				with pfi^ do
				begin
					FieldName := Names[i];
					FieldSize := StrToIntDef( Values[Names[i]], 0 );
				end;
				iCount := Stream.Write( pfi^, SizeOf( TKLogFieldInfo ) );
				if ( iCount <> SizeOf( TKLogFieldInfo ) ) then
					RaiseExceptionFmt( EKLogFile, sErrLFWriteField, [pfi^.FieldName] );
			end;
	finally
		Dispose( pfi );
	end;
end;

procedure TKCustomLogFile.WriteRecordCount( Stream: TStream );
var
	iCount : Integer;
begin
	iCount := Stream.Write( FRecordCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLFWriteRecordCount );
end;

{ Log Streaming system validation }

procedure TKCustomLogFile.ValidateHeader( Stream: TStream );
begin
	ukrEngines.ValidateHeader( Stream, FSignature, FVersion, EKLogFile );
end;

procedure TKCustomLogFile.ValidateFieldCount( Stream: TStream );
var
	iCount,
	iFldCount: Integer;
begin
	iCount := Stream.Read( iFldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLEReadFixedFieldCount );
{ This data was written in the log file for future use in the log engine
	for reporting! }
	if ( iFldCount <> FixedFieldCount ) then
		RaiseException( EKLogFile, sErrLEReadFixedFieldCount );
	iCount := Stream.Read( iFldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogFile, sErrLEReadFieldCount );
	if ( iFldCount <> FieldCount ) or ( iFldCount <= 0 ) then
		RaiseException( EKLogFile, sErrLFInvFldTblSize );
end;

procedure TKCustomLogFile.ValidateFieldsTable( Stream: TStream );
var
	i: Integer;
	FFields: TStrings;
begin
	FFields := TStringList.Create;
	try
		FFields.AddStrings( FFixedFields );
		FFields.AddStrings( FCustomFields );
		ukrEngines.ValidateFieldsTable( Stream, FFields, FieldCount,
		  EKLogFile );
	finally
{ Clear callback references }
		for i := 0 to FFixedFields.Count - 1 do
			FFields.Objects[i] := nil;
		FFields.Free;
	end;
end;

procedure TKCustomLogFile.ValidateRecordCount( Stream: TStream );
var
	iCount: Integer;
begin
	iCount := Stream.Read( FRecordCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) or ( FRecordCount < 0 ) then
		RaiseException( EKLogFile, sErrLEReadRecordCount );
end;

procedure TKCustomLogFile.AdjustRecordCount( Stream: TStream );
begin
	ValidateHeader( Stream );
	ValidateFieldCount( Stream );
	ValidateFieldsTable( Stream );
	Inc( FRecordCount );
	WriteRecordCount( Stream );
end;

procedure TKCustomLogFile.ValidateLogRecord( LogRecord: TStrings );
const
	IMTable: array[Boolean] of string = ( IM_NOMESSAGE , IM_INVALID_LOGRECORD );
begin
	if ( not CheckObject( LogRecord ) ) then
{ add a log with just fixed info ( FixedFields + Empty CustomFields )
	if sl = nil!
}
		FInternalMessage := IM_ONLYFIXEDFIELDS
	else
{ try to add a log, some fields, if match, will be printed, if not,
	will print spaces and the internal message will be IM_INVALID_LOGRECORD.
}
		FInternalMessage := IMTable[( FCustomFields.Equals( LogRecord ) )];
end;

procedure TKCustomLogFile.AddLogEntry( Stream: TStream; const LogEntry: string );
var
	iCount: Integer;
begin
	if ( Length( LogEntry ) <> LogRecordSize ) then
		RaiseExceptionFmt( EKLogFile, sErrLFInvalidLogEntry,
			[LogRecordSize, Length( LogEntry )] );
	iCount := Stream.Write( Pointer( LogEntry )^, LogRecordSize );
	if ( iCount <> LogRecordSize ) then
		RaiseException( EKLogFile, sErrLFWriteLogEntry );
end;

function TKCustomLogFile.CreateEmptyLogRecord: TStrings;
{
 Create and fill a TStrings with the CustomField correct structures.
 The caller is responsable to free the TStrings.

 The resulting stringlist have the following format:

 FieldName_1=
 FieldName_2=
 FieldName_3=
 ...
}
var
	i: Integer;
begin
	Result := TStringList.Create;
	try
		for i := 0 to FCustomFields.Count - 1 do
			Result.Add( FCustomFields.Names[i] + CH_EQUAL_TOKEN );
	except
		Result.Free;
		raise;
	end;
end;

procedure TKCustomLogFile.InternalLogData( LogRecord: TStrings; Data: Pointer );
{
	LogRecord is expected to have the following format:

		FieldName_1=Data_1
		FieldName_2=Data_2

	and so on...
}
const
	wMode: array[Boolean] of Word =
		( ( fmCreate or fmShareDenyWrite ), ( fmShareDenyWrite or fmOpenReadWrite ) );
var
	fs: TFileStream;
	sEntry: string;
	bExist: Boolean;
begin
	ForceTrimStr( FFileName );
	bExist := CheckFile( FFileName );
	try
		fs := TFileStream.Create( FFileName, wMode[bExist] );
		try
			if ( not bExist ) then
			begin
{ if the file doesn't exists, write the reader to the file }
				WriteHeader( fs );
				WriteFieldCount( fs );
				WriteFieldsTable( fs );
				FRecordCount := 0;
				WriteRecordCount( fs );
			end
			else
			begin
{ if the file already exists, Read and Valid the Header! }
				ValidateHeader( fs );
				ValidateFieldCount( fs );
				ValidateFieldsTable( fs );
				ValidateRecordCount( fs );
			end;

{ to add a new log entry, position the cursor at the end of the stream }
			fs.Seek( 0, soFromEnd );
{
	prepare a string with the proper fixed and custom log infos and then
	log the string; remember to pad empty spaces whenever necessary
}
			ValidateLogRecord( LogRecord );
			sEntry := MakeLogEntry( LogRecord );
			AddLogEntry( fs, sEntry );
			AdjustRecordCount( fs );
		finally
			fs.Free;
		end;
	except
		on EKLogFile do
		begin
			if ( not bExist ) and CheckFile( FFileName ) then
				DeleteFile( FFileName );
			raise;
		end;
	end;
end;

procedure TKCustomLogFile.DoOnLogFileLogError( Data: Pointer;
	var LogErrorAction: TKLogErrorAction );
begin
	if Assigned( FOnLogFileLogError ) then
		FOnLogFileLogError( Self, Data, LogErrorAction );
end;

procedure TKCustomLogFile.LogData( LogRecord: TStrings; Data: Pointer );
const
	DEFAULT_LOGERRORACTION: array[Boolean] of TKLogErrorAction = ( laRaise, laRetry );
var
	bDone: Boolean;
	LogErrorAction: TKLogErrorAction;
begin
	bDone := False;
  repeat
    try
      InternalLogData( LogRecord, Data );
      bDone := True;
    except
      on E: Exception do
      { If there was any ohter instance trying to open or create the specified file
        report this to the user and ask him what to do. The dafault behaviour is raise }
        if CheckObjectClasses( E, [EFCreateError, EFOpenError] ) then
        begin
          LogErrorAction := DEFAULT_LOGERRORACTION[Assigned( FOnLogFileLogError )];
          DoOnLogFileLogError( Data, LogErrorAction );
          case LogErrorAction of
            laSkip : bDone := True;
            laRetry: { do nothing };
            laRaise: raise;
          end;
        end;
    end;
  until bDone;
end;

{
---------------------------------------------------------------------------------
------------------------ Generic Log Engine Architecture ------------------------
---------------------------------------------------------------------------------
}

{
	TLogClearType
	-------------

	Choose which type of clear to perform.

	lctAll     » Clear the entire log file.
	lctEntries » Clear the entire log file ( but the file structure ) and
							 sets the recordcount to zero.

	TAutoClearLogType
	-----------------

	aclDate              » from the CurrentDate-DaysToKeep property
												 It calls the ClearByDate with the proper
												 calculated DateTimeValue.
	aclSizeFromBeginning
	aclSizeFromEnd       » clear the file from the begin/end keeping
												 MaxByteSize bytes of log records ( without
												 file header/footer ).

	TKCustomLogEngine
	-----------------

	PRIVATE METHODS
	------- -------

	» Used only into ClearByDate/Size routines for recordcount adjusting...

		procedure ValidateRecordCount( Stream: TStream; NewRecordCount: Integer );
		procedure WriteRecordCount( Stream: TStream; NewRecordCount: LongInt );
		procedure AdjustRecordCount( LogFile: TKCustomLogFile;
			Stream: TStream; NewRecordCount: Integer );

	PROTECTED METHODS
	--------- -------

	» Method called in the destructor, when AutoCleanLog is true, to clean
		the log file with the current selected TAutoCleanLogType.

		procedure AutoCleanLog; dynamic;

	» Virtual fields support to allow for aditional information obtained
		from the information contained in the log file. This method is called
		in the LoadFromStream method.
		DON'T CALL IT DIRECTLY !

		procedure DefineVirtualFields; virtual;

	PROPERTIES
	----------

	» The fileloaded property determines if the record/fields lists are empty or
		not. It does not represent a file openned or in use.

		property FileLoaded: Boolean
						 read GetFileLoaded;	
}

{--------------------------- Internal Implementation ---------------------------}

type

	PKVirtualFieldProc = ^TKVirtualFieldProc;
	TKVirtualFieldProc = record
		GetProc: TKGetVirtualFieldProc;
	end;

function StrFldPad( const Fld: string; Len: Integer; Ch: Char ): string;
begin
	SetString( Result, PChar( Fld ), Length( Fld ) );
	Result := StrRightPad( Result, Len, Ch );
end;

{---------------------------- Public Implementation ----------------------------}

constructor TKCustomLogEngine.Create( AOwner : TComponent );
begin
	inherited Create( AOwner );
	FSignature := LOG_SIGNATURE_100;
	FVersion := LOG_VERSION_100;
	FDaysToKeep := WEEK_TO_DAY;
	FMaxByteSize := SIZE_500KB;
	FAutoClearLogType := aclDate;
	FDefFileExt := K_DEF_LOGEXT;
end;

destructor TKCustomLogEngine.Destroy;
begin
{ if there is no owner or there is an owner and it is run-time and the structures are loaded }
	if ( ( not IsDesigning ) and FileLoaded ) then
	begin
		if ( CheckFile( FFileName ) and AutoClearLog ) then
			AutoCleanLog;
		while CheckStrings( FVirtualFields ) do
			with FVirtualFields do
			begin
				Dispose( PKVirtualFieldProc( Objects[Count - 1] ) );
				Objects[Count - 1] := nil;
				Delete( Count - 1 );
			end;
	end;
	FVirtualFields.Free;
	ClearFieldsList;
	ClearRecordsList;
	FFields.Free;
	FRecords.Free;
	inherited Destroy;
end;

function TKCustomLogEngine.IsDesigning: Boolean;
begin
	Result := ( CheckObject( Owner ) and Designing( Owner ) );
end;

procedure TKCustomLogEngine.SetFileName( const Value: TFileName );
begin
	if ( not CheckStrEqual( Value, FFileName ) ) or ( not FileLoaded ) then
	begin
		FFileName := Value;
		if ( CheckTrimStr( FFileName ) or ( not CheckStr( ExtractFileExt( FFileName ) ) ) ) then
			FFileName := ChangeFileExt( FFileName, FDefFileExt );
{ If the file is already loaded the LoadFromStream ( called in  LoadFromFile )
	garantees the proper file load. }
		if CheckFile( FFileName ) then
		begin
			if ( ( not IsDesigning ) and FAutoLoad ) then
				try
					LoadFromFile( FFileName );
				except
					FFileName := '';
					raise;
				end;
		end
		else
			FFileName := '';
	end;
end;

function TKCustomLogEngine.GetLogRecordSize: Integer;
var
	i: Integer;
begin
	if ( not CheckList( FFields ) ) then
		Result := -1
	else
	begin
		Result := 0;
		for i := 0 to FFields.Count - 1 do
			Inc( Result, PKLogFieldInfo( FFields.Items[i] )^.FieldSize );
	end;
end;

function TKCustomLogEngine.GetHeaderSize: Integer;
begin
	CheckFields( False );
	{ Signature + Version + FldCount + FixedFldCount + RecCount + FldsTbl}
	Result := SizeOf( FSignature ) + SizeOf( FVersion ) + SizeOf( Integer ) +
						SizeOf( Integer ) + SizeOf( Integer ) +
						( FieldCount * SizeOf( TKLogFieldInfo ) );
end;

function TKCustomLogEngine.GetLogRecords( const FieldName: TFieldName;
	Index: Integer ): string;
var
	s1, s2: string;
	iFieldIndex: Integer;
begin
	if ( not ( CheckStrings( FRecords ) and CheckList( FFields ) and
		CheckStr( FieldName ) ) ) then
		Result := ''
	else
	begin
		iFieldIndex := 0;
		repeat
			s1 := PKLogFieldInfo( FFields.Items[iFieldIndex] )^.FieldName;
			s2 := FieldName;
			if CheckStrEqual( s1, s2 ) then
				Break;
			Inc( iFieldIndex );
		until ( iFieldIndex >= FFields.Count );
		if ( iFieldIndex >= FFields.Count ) then
			RaiseExceptionFmt( EKLogEngine, sErrLEInvalidLogFieldName,
				[FieldName] );

		{
			Get the Value for the Field FieldName in the log collection values.

					FieldCount ==>     0						1						2
			Ex: FieldNames ==> AppUserName, AppHandle , UserComment;
					Records 	 ==> 'Leonardo' , '00000000', 'abcdef'
												 'Demian'   , '00000001', 'ghijkl'
												 'Helio'    , '00000002', 'mnopqr'

												 K

						 | Leonardo  0
			Index	 | 00000000  1  ( FRecords.Strings[K] )
				0    | abcdef    2

						 | Demian    3
			Index	 | 00000001  4
				1		 | ghijkl    5

			if you try to get AppUserName at index 1, the value is 'Demian'. And that value
			is in the position 3 of the FRecords TStrings.

			So, we get the formula:  K := ( FieldCount * Index ) + iFieldIndex ;
		 }
		Result := FRecords.Strings[( ( FFields.Count * Index ) + iFieldIndex )];
	end;
end;

function TKCustomLogEngine.GetLogFields( Index: Integer ): TFieldName;
begin
	if ( not CheckList( FFields ) ) then
		Result := ''
	else
		Result := PKLogFieldInfo( FFields.Items[Index] )^.FieldName;
end;

function TKCustomLogEngine.GetLogFieldsSize( Index: Integer ): Word;
begin
	if ( not CheckList( FFields ) ) then
		Result := 0
	else
		Result := PKLogFieldInfo( FFields.Items[Index] )^.FieldSize;
end;

function TKCustomLogEngine.GetFileLoaded: Boolean;
begin
{ FileLoaded means the structures loaded }
	Result := ( CheckList( FFields ) and CheckStrings( FRecords ) );
end;

function TKCustomLogEngine.GetFieldCount( Index: Integer ): Integer;
begin
	case Index of
		FIXED_PERSIST_FLD 	  : Result := FFixedFieldCount;
		PERSIST_FLD           : Result := FFieldCount;
		VIRTUAL_NOPERSIST_FLD : Result := FVirtualFields.Count;
		else
			Result := -1;
	end;
end;

function TKCustomLogEngine.GetVirtualRecords( const vFldName: TFieldName;
	Index: Integer ): string;
var
	s: string;
	iFldIndex: Integer;
begin
  ForceTrimStr( vFldName );
	if CheckObject( FVirtualFields ) then
		with FVirtualFields do
		begin
			s := vFldName;
			iFldIndex := FVirtualFields.IndexOfName( s );
			if ( iFldIndex = -1 ) then
				RaiseExceptionFmt( EKLogEngine, sErrLEInvalidLogFieldName, [vFldName] );
			s := PKVirtualFieldProc( Objects[iFldIndex] )^.GetProc( Index );
			Result := StrAdjustRight( s, StrToInt( Values[Names[iFldIndex]] ), CH_SPACE );
		end
	else                        
		Result := '';
end;

function TKCustomLogEngine.GetVirtualFields( Index: Integer ): TFieldName;
begin
	if CheckObject( FVirtualFields ) then
		Result := FVirtualFields.Names[Index]
	else
		Result := '';
end;

function TKCustomLogEngine.GetVirtualFieldsSize( Index: Integer ): Word;
begin
	if CheckObject( FVirtualFields ) then
		with FVirtualFields do
			Result := StrToIntDef( Values[Names[Index]], 0 )
	else
		Result := 0;
end;

procedure TKCustomLogEngine.DefineVirtualFields;
begin
{
	Put custom code here to define the virtual fields!
	Call DefineVirtualField for each field.
}
end;

procedure TKCustomLogEngine.DefineVirtualField( const AFieldName: TFieldName;
	Size: Word; Proc: TKGetVirtualFieldProc );
var
	i: Integer;
	pvf: PKVirtualFieldProc;
begin
	ForceTrimStr( AFieldName );
	ForceReference( @Proc );
	if ( Size = 0 ) then
		RaiseException( EKLogEngine, sErrLFInvalidFldSize );
	if ( FVirtualFields.IndexOfName( AFieldName ) <> -1 ) then
		RaiseExceptionFmt( EKLogEngine, sErrLFDuplicatedFldName, [AFieldName] );
	i := -1;
	pvf := New( PKVirtualFieldProc );
	try
		ZeroMemory( pvf, SizeOf( TKVirtualFieldProc ) );
		pvf^.GetProc := Proc;
		i := FVirtualFields.AddObject( AFieldName + CH_EQUAL_TOKEN + IntToStr( Size ),
			TObject( pvf ) );
	except
		Dispose( pvf );
		if ( i <> -1 ) then
		  FVirtualFields.Delete( i ); 
		raise;
	end;
end;

procedure TKCustomLogEngine.AutoCleanLog;
var
	dtDate: TDateTime;
begin
	case AutoClearLogType of
		aclDate:
			if ( DaysToKeep > 0 ) then
			begin
				dtDate := IncDate( SysUtils.Date, -DaysToKeep, 0, 0 );
				ClearByDate( EndOfDay( dtDate ) );
			end;
		aclSizeFromBeginning:
			if ( MaxByteSize > 0 ) then
				ClearBySize( MaxByteSize, cdFromBegining );
		aclSizeFromEnd	   :
			if ( MaxByteSize > 0 ) then
				ClearBySize( MaxByteSize, cdFromEnd );
	end;
end;

procedure TKCustomLogEngine.ClearFieldsList;
begin
	while CheckList( FFields ) do
	begin
		Dispose( PKLogFieldInfo( FFields.Last ) );
		FFields.Delete( FFields.Count - 1 );
	end;
end;

procedure TKCustomLogEngine.ClearRecordsList;
begin
	if CheckObject( FRecords ) then
		FRecords.Clear;
	FRecordCount := 0;
end;

procedure TKCustomLogEngine.CheckFields( IsEmpty: Boolean );
begin
	if ( IsEmpty and CheckList( FFields ) ) then
		RaiseException( EKLogEngine, sErrLEFieldsNotEmpty )
	else if ( not ( IsEmpty or CheckList( FFields ) ) ) then
		RaiseException( EKLogEngine, sErrLEFieldsEmpty );
end;

procedure TKCustomLogEngine.CheckRecords( IsEmpty: Boolean );
begin
	if ( IsEmpty and CheckStrings( FRecords ) ) then
		RaiseException( EKLogEngine, sErrLERecordsNotEmpty )
	else if ( not ( IsEmpty or CheckStrings( FRecords ) ) ) then
		RaiseException( EKLogEngine, sErrLERecordsEmpty );
end;

procedure TKCustomLogEngine.ValidateHeader( Stream: TStream );
begin
	ukrEngines.ValidateHeader( Stream, FSignature, FVersion, EKLogEngine );
end;

{
procedure TKCustomLogEngine.ValidateFieldsTable( Stream: TStream );
begin
	CheckFields( False );
	ukrEngines.ValidateFieldsTable( Stream, FFields, EKLogEngine );
end;
}

procedure TKCustomLogEngine.ReadFieldCount( Stream: TStream );
var
	iCount: Integer;
begin
	iCount := Stream.Read( FFixedFieldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogEngine, sErrLEReadFixedFieldCount );
	iCount := Stream.Read( FFieldCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogEngine, sErrLEReadFieldCount );
end;

procedure TKCustomLogEngine.ReadFieldsTable( Stream: TStream );
var
	i: Integer;
	pki: PKLogFieldInfo;
begin
	if ( not CheckObject( FFields ) ) then
		FFields := TList.Create;
	CheckFields( True );
{ Trust in FldCount from the log file }
	try
		for i := 0 to FFieldCount - 1 do
		begin
			pki := New( PKLogFieldInfo );
			ZeroMemory( pki, SizeOf( TKLogFieldInfo ) );
			Stream.ReadBuffer( pki^, SizeOf( TKLogFieldInfo ) );
			FFields.Add( pki );
		end;
	except
		ClearFieldsList;
		raise;
	end;
end;

procedure TKCustomLogEngine.ReadRecordCount( Stream: TStream );
var
	iCount: Integer;
begin
	iCount := Stream.Read( FRecordCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogEngine, sErrLEReadRecordCount );
end;

procedure TKCustomLogEngine.ReadLogRecords( Stream: TStream );
const
	MaxBufSize = $F000;
var
	iCount,
	iLogSize,
	iRecCount: Integer;
	pBuffer: PChar;
begin
	if ( not CheckObject( FRecords ) ) then
		FRecords := TStringList.Create;
	CheckRecords( True );
	iLogSize := LogRecordSize;
{ For records greater than 60Kb, raise an error.
	Too much data for the GetMem buffer }
	if ( iLogSize > MaxBufSize ) then
		RaiseException( EKLogEngine, sErrLERecordTooLong );
	pBuffer := nil;
	iRecCount := 0;
	if ( Stream.Size <> Stream.Position ) then
		GetMem( pBuffer, iLogSize + 1 );
	try
		while ( Stream.Size <> Stream.Position ) do
		begin
			ZeroMemory( pBuffer, iLogSize + 1 );
			iCount := Stream.Read( pBuffer^, iLogSize );
			if ( iCount <> iLogSize ) then
				RaiseException( EKLogEngine, sErrLEReadLogEntry );
			Inc( iRecCount );
			ReadLogFields( pBuffer );
		end;
	finally
		if CheckPointer( pBuffer ) then
			FreeMem( pBuffer, iLogSize + 1 );
	end;
	if ( iRecCount <> FRecordCount ) then
		RaiseExceptionFmt( EKLogEngine, sErrLERecordCountMismatch,
			[iRecCount, FRecordCount] );
end;

procedure TKCustomLogEngine.ReadLogFields( Buffer: string );
var
	i : Integer;
	pki: PKLogFieldInfo;
	sValue: string;
begin
	CheckFields( False );
	for i := 0 to FFields.Count - 1 do
	begin
		pki := PKLogFieldInfo( FFields.Items[i] );
		sValue := Copy( Buffer, 1, pki^.FieldSize );
		Delete( Buffer, 1, pki^.FieldSize );
		FRecords.Add( Trim( sValue ) );
	end;
end;

procedure TKCustomLogEngine.LoadFromStream( Stream: TStream );
var
	ss: TStringStream;
begin
	ClearFieldsList;
	ClearRecordsList;
	ss := TStringStream.Create( '' );
	try
		ForceStreamCopy( Stream, ss );
		ValidateHeader( ss );
		if ( ss.Size = ss.Position ) then
			RaiseExceptionFmt( EKLogEngine, sErrLEFldTblNotFound, [FileName] );
		ReadFieldCount( ss );
		ReadFieldsTable( ss );
		if ( ss.Size = ss.Position ) then
			Exit;
		ReadRecordCount( ss );
		ReadLogRecords( ss );
		if ( not CheckObject( FVirtualFields ) ) then
		begin
			FVirtualFields := TStringList.Create;
			DefineVirtualFields;
		end;
	finally
		ss.Free;
	end;
end;

procedure TKCustomLogEngine.LoadFromFile( const AFileName: TFileName );
var
	fs: TFileStream;
begin
	ForceFile( AFileName );
	FFileName := AFileName;
	fs := TFileStream.Create( FileName, fmOpenRead or fmShareExclusive );
	try
	  LoadFromStream( fs );
	finally
		fs.Free;
	end;
end;

procedure TKCustomLogEngine.Clear( LogClearType: TLogClearType );
var
	fs: TFileStream;
	bDelete : Boolean;
begin
	if CheckFile( FileName ) then
	begin
		if ( not FileLoaded ) then
			RaiseExceptionFmt( EKLogEngine, sErrLECannotClearNotLoadedFile,
				[FileName] );
		bDelete := False;
		fs := TFileStream.Create( FileName, fmOpenReadWrite or fmShareExclusive );
		try
			case LogClearType of
				lctAll	  :
				begin
					bDelete := True;
					ClearFieldsList;
				end;
				lctEntries:
				  fs.Size := HeaderSize;
			end;
			ClearRecordsList;
		finally
			fs.Free;
			if bDelete then
				ForceDeleteFile( FileName );
		end;
	end;
end;

procedure TKCustomLogEngine.ValidateRecordCount( Stream: TStream;
  NewRecordCount: Integer );
var
	iCount,
	iRecCount: Integer;
begin
	iCount := Stream.Read( iRecCount, SizeOf( Integer ) );
	if ( ( iCount <> SizeOf( Integer ) ) or ( FRecordCount <> iRecCount ) or
	     ( FRecordCount < 0 ) ) then
		RaiseException( EKLogEngine, sErrLEReadRecordCount );
end;

procedure TKCustomLogEngine.WriteRecordCount( Stream: TStream; NewRecordCount: LongInt );
var
	iCount : Integer;
begin
	iCount := Stream.Write( NewRecordCount, SizeOf( Integer ) );
	if ( iCount <> SizeOf( Integer ) ) then
		RaiseException( EKLogEngine, sErrLFWriteRecordCount );
end;

procedure TKCustomLogEngine.AdjustRecordCount( LogFile: TKCustomLogFile;
	Stream: TStream; NewRecordCount: Integer );
begin
	with LogFile do
	begin
		ValidateHeader( Stream );
		ValidateFieldCount( Stream );
		ValidateFieldsTable( Stream );
{ Now we have the stream validated and positioned at the RecordCount point, just
	for safity, compare this value with actual Self.RecordCount property }
		Self.ValidateRecordCount( Stream, NewRecordCount );
{ After validation seek back to record count original position for writing the
	new value! }
		Stream.Seek( -SizeOf( Integer ), soFromCurrent );
		Self.WriteRecordCount( Stream, NewRecordCount );
	end;
end;

procedure TKCustomLogEngine.ClearByDate( From_Creation_To_DateTime: TDateTime );
var
	iNumReg,
	iDelPos: Integer;
	dDate: TDateTime;
	fs: TFileStream;
	lf: TKCustomLogFile;
	ss: TKStringStream;
begin
	if CheckFile( FileName ) and ( From_Creation_To_DateTime <= Now ) then
	begin
		ss := nil;
		try
			fs := TFileStream.Create( FileName, fmOpenReadWrite or fmShareExclusive );
			try
				if ( not FileLoaded ) then
	{ Load the LogFields, LogRecord, FieldCount and RecordCount }
					LoadFromStream( fs );

				lf := GetLogFileClass.CreateToClean( FileName );
				try

					if ( lf.FieldCount <> FieldCount ) then
						RaiseExceptionFmt( EKLogEngine, sErrLEFldCountMismatch, [GetLogFileClass.ClassName,
							GetFirstString( [Name, ClassName] ), lf.FieldCount, FieldCount] );

	{ if the logrecordsize differs the log file has more fixedfields
		than the default log file. }

					if ( lf.LogRecordSize <> LogRecordSize ) then
						RaiseException( EKLogEngine, sErrLEInvLogRecSize );

					if ( lf.HeaderSize <> HeaderSize ) then
						RaiseException( EKLogEngine, sErrLEInvLogHeaderSize );

					iNumReg := 0;
					while ( iNumReg <= RecordCount - 1 ) do
					begin
	{ for each record, add a new entry on the lf new log file only if dDate
		is Newer than From_Creation_To_DateTime }
						dDate := StrToDateTime( LogRecords[DATE_TIME_FIELD, iNumReg] );
						if ( dDate < From_Creation_To_DateTime ) then
							Inc( iNumReg )
						else
							Break;
					end;

	 { There are no records to delete }
					if ( iNumReg = 0 ) then
						Exit;

					iDelPos := lf.HeaderSize + ( iNumReg * lf.LogRecordSize );

					if ( iDelPos > lf.HeaderSize ) then
					begin
	{ Now we will delete the entries... }
						ss := TKStringStream.Create( '' );
						ForceStreamCopy( fs, ss );
						ss.CopyBufferEx( iDelPos, lf.HeaderSize, ss.Size - iDelPos );
						AdjustRecordCount( lf, ss, RecordCount - iNumReg );
					end;
				finally
					lf.Free;
				end;
			finally
				fs.Free;
			end;
			if CheckObject( ss ) then
			begin
				ForceDeleteFile( FileName );
				ss.SaveToFile( FileName );
			end;	
		finally
			if CheckObject( ss ) then
				ss.Free;
		end;
	end;
end;

procedure TKCustomLogEngine.ClearBySize( AMaxByteSize: Cardinal;
	ADirection: TClearDirection );
var
	iSize,
	iLogSize: Integer;
	fs: TFileStream;
	lf: TKCustomLogFile;
	ss: TKStringStream;
begin
 { MaxByteSize does not consider the HeaderSize of the file, that is:
	 MaxByteSize of log entries ( not all log file ) }
	iLogSize := ( LogRecordSize * RecordCount );
{ Here we must TypeCast AMaxByteSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Size
	also returns Integer ranged values... }
	if CheckFile( FileName ) and ( iLogSize > Integer( AMaxByteSize ) ) then
	begin
		ss := nil;
		try
			fs := TFileStream.Create( FileName, fmShareExclusive or fmOpenReadWrite );
			try
				if ( not FileLoaded ) then
					LoadFromStream( fs );
				lf := GetLogFileClass.CreateToClean( FileName );
				try
	{ Load the LogFields, LogRecord, FieldCount and RecordCount }
					if ( lf.FieldCount <> FieldCount ) then
						RaiseExceptionFmt( EKLogEngine, sErrLEFldCountMismatch, [GetLogFileClass.ClassName,
							GetFirstString( [Name, ClassName] ), lf.FieldCount, FieldCount] );

				 {if the logrecordsize differ, so, the log file has more fixedfields than the
					 default log file.}
					if ( lf.LogRecordSize <> LogRecordSize ) then
						RaiseException( EKLogEngine, sErrLEInvLogRecSize );

          if ( lf.HeaderSize <> HeaderSize ) then
						RaiseException( EKLogEngine, sErrLEInvLogHeaderSize );

					ss := TKStringStream.Create( '' );
					ForceStreamCopy( fs, ss );
					{ Here we must TypeCast AMaxByteSize to Integer because in Delphi4 cardinal
						violates the Integer Range and generates a warning here. Even, Size
						also returns Integer ranged values... }
					iSize := ( Integer( AMaxByteSize ) div LogRecordSize ) * LogRecordSize;
					case ADirection of
						cdFromBegining:
						begin
							Inc( iSize, lf.HeaderSize );
							ss.Size := iSize;
						end;
						cdFromEnd:
							ss.CopyBufferEx( ss.Size - iSize, lf.HeaderSize, iSize );
					end;
					{ Here we must TypeCast AMaxByteSize to Integer because in Delphi4 cardinal
						violates the Integer Range and generates a warning here. Even, Size
						also returns Integer ranged values... }
					AdjustRecordCount( lf, ss, ( Integer( AMaxByteSize ) div LogRecordSize ) );
				finally
					lf.Free;
				end;
			finally
				fs.Free;
			end;
			if CheckObject( ss ) then
			begin
				ForceDeleteFile( FileName );
				ss.SaveToFile( FileName );
			end;
		finally
			if CheckObject( ss ) then
				ss.Free;
		end;
	end;
end;

function TKCustomLogEngine.GetHighFieldLength: Byte;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to FieldCount - 1 do
		if ( Length( LogFields[i] ) > Result ) then
			Result := Length( LogFields[i] );
	for i := 0 to VirtualFieldCount - 1 do
		if ( Length( VirtualFields[i] ) > Result ) then
			Result := Length( VirtualFields[i] );
	Inc( Result , 5 );
	if ( Result > MAXLEN_FIELDNAME ) then
		Result := MAXLEN_FIELDNAME;
end;

procedure TKCustomLogEngine.RecordsFillStrings( ss: TStrings; RecCount: Integer;
	ListSeparator: Char );
{
	File OutPut format:

	Log record Nº » 1
	________________________

	UserName................ = chester
	ComputerName............ = TRABALHO01
	DateTime................ = 17/10/1998 16:08:25
	AppHandle............... = 00000C88
	InternalMsg............. = IM_NOMESSAGE
	Exception.ExceptAddr.... = 0006E50A
	Exception.Module........ = uTest
	Exception.Routine....... = TfrmTest.EKnowHowClick
	Exception.Line.......... = 97
	Exception.Class......... = EKnowHow
	Exception.Message....... = Mais um Teste.
	AppUserName............. = chester
	UserComment............. = <No user comment>
	________________________

	Log record Nº » 2
	________________________

	UserName................ = chester
	ComputerName............ = TRABALHO01
	DateTime................ = 17/10/1998 16:08:26
	AppHandle............... = 00000C88
	InternalMsg............. = IM_NOMESSAGE
	Exception.ExceptAddr.... = 0006E53A
	Exception.Module........ = uTest
	Exception.Routine....... = TfrmTest.EDBEngineErrorClick
	Exception.Line.......... = 102
	Exception.Class......... = EDBEngineError
	Exception.Message....... = O campo %s deve ser preenchido.
	AppUserName............. = chester
	UserComment............. = <No user comment>
	________________________


	Total log records reported » 2
}

const
{
	To print the report the FieldName is Left aligned and the with reserved for FieldName
	is calculated based on the higher fieldname lenght provided by fixed, custom and virtual
	fields. The right part is the log record value from the log file!
}
	FIELDS_PATTERN = ( '%-*s = %s' );
	HEADER_PATTERN = ( '%s » %d' );
	LINE_PATTERN   = ( '%-*s' );
	FOOTER_PATTERN = ( '%s » %d' );

var
	i,
	j : Integer;
	yHiFldLen: Byte;
begin
	if ( ListSeparator = CH_NULL ) then
		ListSeparator := CH_NAMEFIX;
	if ( RecordCount <= 0 ) then
		RaiseExceptionFmt( EKLogEngine, sErrLEEmptyLogEntries, [sLERecords, FileName] );
{ List Preparing }
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
{ Default RecordCount support }
		if ( RecCount <= 0 ) then
			RecCount := RecordCount;
		yHiFldLen := GetHighFieldLength;
		for i := 0 to RecCount - 1 do
		begin
{ Header Reporting }
			ss.Add( Format( HEADER_PATTERN, [sRECORD_LOG_HEADER_TEXT, i+1] ) );
			ss.Add( Format( LINE_PATTERN, [yHiFldLen, StrFldPad( ListSeparator, yHiFldLen,
				ListSeparator )] ) );
			ss.Add( '' );
{ Fixed Field Reporting }
			for j := 0 to FixedFieldCount - 1 do
				ss.Add( Format( FIELDS_PATTERN, [yHiFldLen, StrFldPad( LogFields[j],
					yHiFldLen, CH_DOTMARK ), LogRecords[LogFields[j], i]] ) );
{ Virtual Fields Reporting }
			for j := 0 to VirtualFieldCount - 1 do
				ss.Add( Format( FIELDS_PATTERN, [yHiFldLen, StrFldPad( VirtualFields[j],
					yHiFldLen, CH_DOTMARK ), VirtualRecords[VirtualFields[j], i]] ) );
{ Custom Fields Reporting }
			for j := FixedFieldCount to FieldCount - 1 do
				ss.Add( Format( FIELDS_PATTERN, [yHiFldLen, StrFldPad( LogFields[j], yHiFldLen,
					CH_DOTMARK ), LogRecords[LogFields[j], i]] ) );
{ Separator Reporting }
			ss.Add( Format( LINE_PATTERN, [yHiFldLen, StrFldPad( ListSeparator, yHiFldLen,
				ListSeparator )] ) );
			ss.Add( '' );
		end;
{ Footer Reporting }
		ss.Add( '' );
		ss.Add( Format( HEADER_PATTERN, [sRECORD_LOG_FOOTER_TEXT, RecCount] ) );
	finally
		ss.EndUpdate;
	end;
end;

procedure TKCustomLogEngine.FieldsFillStrings( ss: TStrings; ListSeparator: Char );

{
	File OutPut format:

	Log Fields Report
	________________________

	UserName................ = ?
	ComputerName............ = ?
	DateTime................ = ?
	AppHandle............... = ?
	InternalMsg............. = ?
	Exception.ExceptAddr.... = ?
	Exception.Module........ = ?
	Exception.Routine....... = ?
	Exception.Line.......... = ?
	Exception.Class......... = ?
	Exception.Message....... = ?
	AppUserName............. = ?
	UserComment............. = ?
	________________________


	Total log fields reported » x
	}

const
	HEADER_PATTERN = ( '%s' );
	FOOTER_PATTERN = ( '%s » %d' );
	LINE_PATTERN   = ( '%-*s = %3.3d' );

var
	i: Integer;
	yHiFldLen: Byte;

begin
	if ( ListSeparator = CH_NULL ) then
		ListSeparator := CH_NAMEFIX;
	if ( FieldCount <= 0 ) then
		RaiseExceptionFmt( EKLogEngine, sErrLEEmptyLogEntries, [sLEFields, FileName] );
{ List Preparing }
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
{ Default FieldCount support }
		yHiFldLen := GetHighFieldLength;
{ Header Reporting }
		ss.Add( Format( HEADER_PATTERN, [sFIELDS_LOG_HEADER_TEXT] ) );
		ss.Add( StrFldPad( ListSeparator, yHiFldLen, ListSeparator ) +
		  sFIELDS_LOG_FLDSIZE_TEXT );
		ss.Add( '' );
{ Fixed Fields Reporting }
    for i := 0 to FixedFieldCount - 1 do
      ss.Add( Format( LINE_PATTERN, [yHiFldLen, StrFldPad( LogFields[i], yHiFldLen,
					CH_DOTMARK ), LogFieldsSize[I] ] ) );
{ Virtual Fields Reporting }
    for i := 0 to VirtualFieldCount - 1 do
      ss.Add( Format( LINE_PATTERN, [yHiFldLen, StrFldPad( VirtualFields[i],
        yHiFldLen, CH_DOTMARK ), VirtualFieldsSize[i]] ) );
{ Custom Fields Reporting }
    for i := FixedFieldCount to FieldCount - 1 do
      ss.Add( Format( LINE_PATTERN, [yHiFldLen, StrFldPad( LogFields[i], yHiFldLen,
        CH_DOTMARK ), LogFieldsSize[i]] ) );
{ Footer Reporting }
		ss.Add( StrFldPad( ListSeparator, yHiFldLen, ListSeparator ) );
		ss.Add( '' );
		ss.Add( Format( FOOTER_PATTERN, [sFIELDS_LOG_FOOTER_TEXT, FieldCount + VirtualFieldCount] ) );
	finally
		ss.EndUpdate;
	end;
end;

procedure TKCustomLogEngine.RetrieveFields( sl: TStrings; FieldKinds: TKLogEngineFieldKinds );
var
  j: Integer;
  i: TKLogEngineFieldKind;
begin
  ForceObject( sl );
  sl.BeginUpdate;
  try
    sl.Clear;
    for i := Low( TKLogEngineFieldKind ) to High( TKLogEngineFieldKind ) do
      if ( i in FieldKinds ) then
        case i of
          lefkFixed:
            for j := 0 to FixedFieldCount - 1 do
              sl.AddObject( LogFields[j], TObject( LogFieldsSize[j] ) );
          lefkVirtual:
            for j := 0 to VirtualFieldCount - 1 do
              sl.AddObject( VirtualFields[j], TObject( VirtualFieldsSize[j] ) );
          lefkCustom:
            for j := FixedFieldCount to FieldCount - 1 do
              sl.AddObject( LogFields[j], TObject( LogFieldsSize[j] ) );
        end;
    AdjustStringsForValues( sl );
  finally
    sl.EndUpdate;
  end;
end;

procedure TKCustomLogEngine.RetrieveRecord( sl: TStrings; RecNo: Integer;
  FieldKinds: TKLogEngineFieldKinds );
var
  j,
  iCnt,
  iInc: Integer;
  i: TKLogEngineFieldKind;
begin
  if ( not ValueBetween( RecNo, 0, RecordCount - 1, True ) ) then
    RaiseExceptionFmt( EKLogEngine, sErrInvLERetriveRec, [RecNo, FileName] );
  ForceObject( sl );
  sl.BeginUpdate;
  try
    sl.Clear;
    iInc := 0;
    iCnt := sl.Count;
    for i := Low( TKLogEngineFieldKind ) to High( TKLogEngineFieldKind ) do
      if ( i in FieldKinds ) then
        case i of
          lefkFixed:
          begin
            Dec( iCnt, FixedFieldCount );
            if ( iCnt < 0 ) then
              RaiseExceptionFmt( EKLogEngine, sErrInvLERetriveRec, [RecNo, FileName] );
            for j := 0 to FixedFieldCount - 1 do
              sl.Values[sl.Names[j]] := LogRecords[sl.Names[j], RecNo];
          end;
          lefkVirtual:
          begin
            Dec( iCnt, VirtualFieldCount );
            if ( iCnt < 0 ) then
              RaiseExceptionFmt( EKLogEngine, sErrInvLERetriveRec, [RecNo, FileName] );
            if ( lefkFixed in FieldKinds ) then
              iInc := FixedFieldCount;
            for j := 0 to VirtualFieldCount - 1 do
              sl.Values[sl.Names[j + iInc]] := VirtualRecords[sl.Names[j + iInc], RecNo];
          end;
          lefkCustom:
          begin
            Dec( iCnt, FixedFieldCount );
            if ( iCnt < 0 ) then
              RaiseExceptionFmt( EKLogEngine, sErrInvLERetriveRec, [RecNo, FileName] );
            if ( lefkVirtual in FieldKinds ) then
              iInc := VirtualFieldCount;
            if ( not ( lefkFixed in FieldKinds ) ) then
              Dec( iInc, FixedFieldCount );
            for j := FixedFieldCount to FieldCount - 1 do
              sl.Values[sl.Names[j + iInc]] := LogRecords[sl.Names[j + iInc], RecNo];
          end;
        end;
  finally
    sl.EndUpdate;
  end;
end;

{---------------------------- TKCustomLogEngineLink ----------------------------}

procedure TKCustomLogEngineLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	inherited DoLinkEvent( LinkEvent, Data );
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Auditory Architecture ------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const
	APP_USER_NAME_FIELD = 'AppUserName';

{---------------------------- Public Implementation ----------------------------}

{------------------------------ TKCustomAuditorLog -----------------------------}

constructor TKCustomAuditoryLog.Create( AOwner: TComponent; const AFileName: TFileName );
begin
  if ( not FCreateToClean ) then { ugh! Hard Couple! } 
  	ForceObjectClass( AOwner, TKCustomAuditable );
	inherited Create( AOwner, AFileName );
end;

function TKCustomAuditoryLog.GetLogOwner: TKCustomAuditable;
begin
	Result := ( inherited GetLogOwner as TKCustomAuditable );
end;

procedure TKCustomAuditoryLog.DefineFixedFields;
begin
	inherited DefineFixedFields;
	{ Do not resource }
	DefineFixedField( APP_USER_NAME_FIELD, MAXLEN_FIELDNAME, GetAppUserName );
end;

procedure TKCustomAuditoryLog.CreateHandle;
begin
	Handle := Owner.Handle;
end;

function TKCustomAuditoryLog.GetAppUserName: string;
begin
	if CheckObject( Owner ) then
		Result := Owner.GetAppUserName
	else
	  Result := UserName;	
end;

procedure TKCustomAuditoryLog.InternalLogData( LogRecord: TStrings; Data: Pointer );
begin
	Owner.DoAuditory( aeRequestFileName, LongInt( Data ), 0 );
	inherited InternalLogData( LogRecord, Data );
end;

{--------------------------- TKCustomAuditoryLogEngine -------------------------}

function TKCustomAuditoryLogEngine.GetLogFileClass: TKCustomLogFileClass;
begin
	Result := GetAuditoryLogClass;
end;

{------------------------------- TKCustomAuditable -----------------------------}

constructor TKCustomAuditable.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FKAuditoryLog := GetAuditoryLogClass.Create( Self, '' );
	FAppUserName := UserName;
	FEnabled := True;
	if ( not Designing( Self ) ) then
	begin
		Application.HookMainWindow( WndProc );
		FKAuditoryLog.FileName := ApplicationLikeName( FkAuditoryLog.DefFileExt );
	end;
end;

destructor TKCustomAuditable.Destroy;
begin
  if ( not Designing( Self ) ) then
		Application.UnhookMainWindow( WndProc );
	FKAuditoryLog.Free;
	inherited Destroy;
end;

procedure TKCustomAuditable.Notification( AComponent: TComponent; AOperation: TOperation );
begin
	inherited Notification( AComponent, AOperation );
	if ( AOperation = opInsert ) and ( AComponent <> Self ) and
		 CheckObjectClass( AComponent, ClassType ) then
	begin
		if CheckObject( Owner ) then
			RaiseExceptionFmt( EKAuditory, sErrForceSingleton, [GetFirstString( 
				[Owner.Name, Owner.ClassName] ), ClassName] )
		else
			RaiseExceptionFmt( EKAuditory, sErrForceSingleton, [sNil, ClassName] )
	end;
end;

function TKCustomAuditable.WndProc( var Message: TMessage ): Boolean;
begin
{ Leave the application process this message }
	Result := False;
end;

function TKCustomAuditable.GetHandle: THandle;
begin
	Result := Application.Handle;
end;

procedure TKCustomAuditable.DoRequestFileName( Data: Pointer );
begin
{ Default implementation doesn't define any custom name setting. }
	if ( not CheckTrimStr( FileName ) ) then
  	FileName := ApplicationLikeName( FKAuditoryLog.DefFileExt );
end;

procedure TKCustomAuditable.DoAuditory( Event: TKAuditEvent; WParam, LParam: LongInt );
begin
	case Event of
		aeRequestFileName: DoRequestFileName( Pointer( WParam ) );		
	else
	{ In this custom event, the event handle is responsable for logging the data! }
		if Assigned( FAuditoryEvent ) then
			FAuditoryEvent( Self, Event, WParam, LParam );
	end;		
end;

procedure TKCustomAuditable.SetAuditoryFileName( const Value: TFileName );
begin
	if ( not CheckTrimStr( Value ) ) then
		FKAuditoryLog.FileName := ApplicationLikeName( FKAuditoryLog.DefFileExt )
	else
  	FKAuditoryLog.FileName := Value;
end;

function TKCustomAuditable.GetAuditoryFileName: TFileName;
begin
	Result := FKAuditoryLog.FileName;
end;

function TKCustomAuditable.GetAuditoryLog: TKCustomAuditoryLog;
begin
	Result := FKAuditoryLog;
end;

procedure TKCustomAuditable.SetAppUserName( const Value: string ) ;
begin
	FAppUserName := Trim( Value );
end;

function TKCustomAuditable.GetAppUserName: string;
begin
	if CheckStr( FAppUserName ) then
		Result := FAppUserName
	else
		Result := Trim ( UserName );
end;

{------------------------- TKCustomAggregateAuditable --------------------------}

constructor TKCustomAggregateAuditable.Create( AClass: TKCustomAuditableClass );
begin
	ForceClass( AClass );
	inherited Create;
	FAuditorClass := AClass;
	FAuditComp := FAuditorClass.Create( nil );
end;

destructor TKCustomAggregateAuditable.Destroy;
begin
	FAuditComp.Free;
	inherited Destroy;
end;

procedure TKCustomAggregateAuditable.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomAuditable ) then
		with ( Source as TKCustomAuditable ) do
		begin
			FileName := FAuditComp.FileName;
			AppUserName := FAuditComp.AppUserName;
			Enabled := FAuditComp.Enabled;
		end
	else
		inherited Assign( Source );
end;

function TKCustomAggregateAuditable.GetAuditoryOwner: TKCustomAuditable;
begin
	Result := FAuditComp;
end;

procedure TKCustomAggregateAuditable.GenericBoolSet( Index: Integer; Value: Boolean );
begin
	case Index of
		0: FAuditComp.Enabled := Value;
	end;
end;

procedure TKCustomAggregateAuditable.GenericStrSet( Index: Integer; const Value: string );
begin
	case Index of
		0: FAuditComp.AppUserName := Value;
	end;
end;

function TKCustomAggregateAuditable.GenericBoolGet( Index: Integer ): Boolean;
begin
	case Index of
		0: Result := FAuditComp.Enabled;
	else
		Result := False;
	end;
end;

function TKCustomAggregateAuditable.GenericStrGet( Index: Integer ): string;
begin
	case Index of
		0: Result := FAuditComp.AppUserName;
	else
		Result := '';
	end;
end;

{
---------------------------------------------------------------------------------
--------------- Generic Stream Data ( Bin Flat File ) Architecture ----------------
---------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

function CalcHalfPos( iStart, iEnd, iCount: Integer ): Integer;
begin
	Result := ( ( iStart + iEnd ) div 2 );
	Dec( Result, ( ( ( Result-iStart ) mod iCount ) ) );
end;

{---------------------------- Public Implementation ----------------------------}

constructor TKStreamData.Create( AOwner: TStream );
begin
	inherited Create;
	FOwner := AOwner;
	FBufferFilled := False;
	FBufferModified := False;
	InitBuffer;
end;

constructor TKStreamData.CreateDefault( AOwner: TStream; ABuffer: Pointer );
begin
	Create( AOwner );
{ This will call SetBuffer... }	
	Buffer := ABuffer;
end;

constructor TKStreamData.CreateLinked( AOwner: TStream; AEvent: TKNormalizeEvent );
begin
	Create( AOwner );
	FOnNormalize := AEvent;
end;

destructor TKStreamData.Destroy;
begin
	if CheckPointer( FBuffer ) then
		FreeMem( FBuffer, BufferSize );
	inherited Destroy;
end;

procedure TKStreamData.InitBuffer;
begin
	if ( BufferSize > 0 ) and ( not CheckPointer( FBuffer ) ) then
	begin
		GetMem( FBuffer, BufferSize );
		FBufferFilled := True;
		FBufferModified := True;     { make sure Clear works as expected }
		Clear;
		{DoBufferChange( nil );}
	end;
end;

procedure TKStreamData.AssignData( Source: Pointer );
begin
	CheckBuffer;
	Clear;
	if CheckPointer( Source ) then
    Buffer := Source;
end;

procedure TKStreamData.AssignBufferTo( Dest: TKStreamData );
begin
	ForceObject( Dest );
	Dest.AssignBufferFrom( Self );
end;

procedure TKStreamData.AssignBufferFrom( Source: TKStreamData );
begin
	CheckBuffer;
	if ( Source = Self ) then
		Exit;
	Clear;
	if ( not CheckPointer( Source ) ) then
		Exit;
	Move( Source.Buffer^, Buffer^, Integer( BufferSize ) );
	FBufferModified := True;
	FBufferFilled := True;
end;

procedure TKStreamData.CheckOwnerClass;
begin
  CheckOwner;
	ForceObjectClass( Owner, TStringStream );
end;

procedure TKStreamData.CheckOwner;
begin
  ForceObject( Owner );
end;

procedure TKStreamData.CheckBuffer;
begin
	if not ( CheckPointer( FBuffer ) or FBufferFilled ) then
		RaiseException( EKStreamData, sErrSDInvBuffer );
end;

class function TKStreamData.GetBufferSize: Cardinal;
begin
	RaiseException( EKStreamData, sErrSDInvBufferSize );
	Result := 0; { just to avoid warning... }
end;

function TKStreamData.GetBufferModified: Boolean;
begin
	Result := ( BufferFilled and FBufferModified );
end;

function TKStreamData.GetBufferFilled: Boolean;
begin
  Result := FBufferFilled;
end;

function TKStreamData.GetInt( Index: Integer ): Integer;
begin
	CheckOwner;
	case Index of
		0: Result := Owner.Position;
		1: Result := Owner.Size;
	else
	  Result := -1;
	end;
end;

function TKStreamData.GetRecCount: Integer;
begin
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Size
	also returns Integer ranged values... }
	Result := ( Size div Integer( BufferSize ) );
end;

procedure TKStreamData.SetInt( Index: Integer; const Value: Integer );
begin
	CheckOwner;
	case Index of
		0: Owner.Position := Value;
		1: Owner.Size := Value;
	end;
end;

function TKStreamData.GetDataString: string;
begin
	CheckOwnerClass;
	Result := ( Owner as TStringStream ).DataString;
end;

function TKStreamData.GetAsBoolean: Boolean;
begin
	ReadDataBuffer( Position, Result, SizeOf( Boolean ) );
end;

function TKStreamData.GetAsCurrency: Currency;
begin
	ReadDataBuffer( Position, Result, SizeOf( Currency ) );
end;

function TKStreamData.GetAsDateTime: TDateTime;
begin
	ReadDataBuffer( Position, Result, SizeOf( TDateTime ) );
end;

function TKStreamData.GetAsFloat: Double;
begin
	ReadDataBuffer( Position, Result, SizeOf( Double ) );
end;

function TKStreamData.GetAsInteger: Longint;
begin
	ReadDataBuffer( Position, Result, SizeOf( Longint ) );
end;

function TKStreamData.GetAsWord: Word;
begin
	ReadDataBuffer( Position, Result, SizeOf( Word ) );
end;

function TKStreamData.GetAsByte: Byte;
begin
	ReadDataBuffer( Position, Result, SizeOf( Byte ) );
end;

function TKStreamData.GetAsString( Count: Integer ): string;
begin
	if ( Count > 0 ) then
	begin
		SetLength( Result, Count );
		ReadDataBuffer( Position, Pointer( Result )^, Count );
	end
	else
		Result := '';
end;

procedure TKStreamData.SetAsBoolean( Value: Boolean );
begin
	WriteDataBuffer( Position, Value, SizeOf( Boolean ) );
end;

procedure TKStreamData.SetAsCurrency( Value: Currency );
begin
	WriteDataBuffer( Position, Value, SizeOf( Currency ) );
end;

procedure TKStreamData.SetAsDateTime( Value: TDateTime );
begin
	WriteDataBuffer( Position, Value, SizeOf( TDateTime ) );
end;

procedure TKStreamData.SetAsFloat( Value: Double );
begin
	WriteDataBuffer( Position, Value, SizeOf( Double ) );
end;

procedure TKStreamData.SetAsInteger( Value: Longint );
begin
	WriteDataBuffer( Position, Value, SizeOf( LongInt ) );
end;

procedure TKStreamData.SetAsWord( Value: Word );
begin
	WriteDataBuffer( Position, Value, SizeOf( Word ) );
end;

procedure TKStreamData.SetAsByte( Value: Byte );
begin
	WriteDataBuffer( Position, Value, SizeOf( Byte ) );
end;

procedure TKStreamData.SetAsString( Count: Integer; const Value: string );
begin
	if ( Count > 0 ) and CheckStr( Value ) then
	begin
		if ( Count > Length( Value ) ) then
			Count := Length( Value );
		WriteDataBuffer( Position, Pointer( Value )^, Count );
	end;
end;

function TKStreamData.EOF: Boolean;
begin
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Size and
	position also returns Integer ranged values... }
	Result := ( Position > ( Size - Integer( BufferSize ) ) );
end;

function TKStreamData.BOF: Boolean;
begin
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	Result := ( Position < Integer( BufferSize ) );
end;

function TKStreamData.Seek( Offset: Integer; Origin: Word ): Integer;
begin
	CheckOwner;
	Result := Owner.Seek( OffSet, Origin );
end;

function TKStreamData.ReadData( Pos: Integer; var ABuffer; Count: Integer ): Integer;
begin
	CheckOwner;
	Position := Pos;
	Result := Owner.Read( ABuffer, Count );
end;

function TKStreamData.WriteData( Pos: Integer; const ABuffer; Count: Integer ): Integer;
begin
	CheckOwner;
	Position := Pos;
	Result := Owner.Write( ABuffer, Count );
end;

procedure TKStreamData.ReadDataBuffer( Pos: Integer; var ABuffer; Count: Integer );
begin
	if ( ReadData( Pos, ABuffer, Count ) <> Count ) then
		RaiseExceptionFmt( EKStreamData, sErrSDRead, [ClassName] );
end;

procedure TKStreamData.WriteDataBuffer( Pos: Integer; const ABuffer; Count: Integer );
begin
	if ( WriteData( Pos, ABuffer, Count ) <> Count ) then
		RaiseExceptionFmt( EKStreamData, sErrSDWrite, [ClassName] );
end;

procedure TKStreamData.DeleteBytes( Pos, Count: Integer );
begin
	CheckOwner;
	CopyBufferEx( Owner, ( Pos + Count ), Pos, ( Size - ( Count + Pos ) ) );
end;

procedure TKStreamData.InsertBytes( Pos: Integer; const ABuffer; Count: Integer );
begin
	CheckOwner;
	CopyBuffer( Owner, Pos, ( Pos + Count ), ( Size - Pos ) );
	WriteDataBuffer( Pos, ABuffer, Count );
end;

procedure TKStreamData.ExchangeBytes( Pos1, Pos2, Count: Integer );
var
	p1,
	p2: Pointer;
begin
	if ( Pos1 = Pos2 ) or ( Count = 0 ) then
		Exit;
	GetMem( p1, Count );
	try
		ZeroMemory( p1, Count );
		GetMem( p2, Count );
		try
			ZeroMemory( p2, Count );
			ReadDataBuffer( Pos1, p1^, Count );
			ReadDataBuffer( Pos2, p2^, Count );
			WriteDataBuffer( Pos1, p2^, Count );
			WriteDataBuffer( Pos2, p1^, Count );
		finally
			FreeMem( p2, Count );
		end;
	finally
		FreeMem( p1, Count );
	end;
end;

procedure TKStreamData.Clear;
begin
	if not ( FBufferFilled or FBufferModified ) then
		Exit;
	CheckBuffer;
	ZeroMemory( Buffer, BufferSize );
	FBufferModified := False;
	FBufferFilled := False;
end;

function TKStreamData.Write: Integer;
begin
	CheckBuffer;
	//Position := Position-BufferSize;
	Result := WriteData( Position, Buffer^, BufferSize );
	FBufferModified := False;
end;

function TKStreamData.Read: Integer;
begin
	CheckBuffer;
	//Position := Position-BufferSize;
	Result := ReadData( Position, Buffer^, BufferSize );
	FBufferFilled := True;
	FBufferModified := True;
end;

procedure TKStreamData.WriteBuffer;
var
	bSave: Boolean;
begin
	bSave := FBufferModified;
	try
	{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
		violates the Integer Range and generates a warning here. Even, Write
		also returns Integer ranged values... }
		if ( Write <> Integer( BufferSize ) ) then
			RaiseExceptionFmt( EKStreamData, sErrSDWrite, [ClassName] );
	except
		FBufferModified := bSave;
		raise;
	end;
end;

procedure TKStreamData.ReadBuffer;
var
	bSave: Boolean;
begin
	bSave := FBufferModified;
	try
	{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
		violates the Integer Range and generates a warning here. Even, Read
		also returns Integer ranged values... }
		if ( Read <> Integer( BufferSize ) ) then
			RaiseExceptionFmt( EKStreamData, sErrSDRead, [ClassName] );
	except
		FBufferModified := bSave;
		raise;
	end;
end;

procedure TKStreamData.ReadAt( Pos: Integer );
begin
	Position := Pos;
	ReadBuffer;
end;

procedure TKStreamData.WriteAt( Pos: Integer );
begin
	Position := Pos;
	WriteBuffer;
end;

procedure TKStreamData.StaticWrite;
begin
	WriteBuffer;
	Seek( -BufferSize, soFromCurrent );
end;

procedure TKStreamData.StaticRead;
begin
	ReadBuffer;
	Seek( -BufferSize, soFromCurrent );
end;

procedure TKStreamData.WriteTo( Stream: TStream );
begin
  ForceObject( Stream );
	CheckBuffer;
	Stream.WriteBuffer( Buffer^, BufferSize );
	FBufferModified := False; {??}
end;

procedure TKStreamData.ReadFrom( Stream: TStream );
begin
  ForceObject( Stream );
	CheckBuffer;
	Clear;
	Stream.ReadBuffer( Buffer^, BufferSize );
	FBufferFilled := True;
	FBufferModified := True;
end;

procedure TKStreamData.Delete;
begin
	CheckBuffer;
	DeleteBytes( Position, BufferSize );
	FBufferFilled := False;
end;

procedure TKStreamData.DeleteAt( Pos: Integer );
begin
	CheckBuffer;
	Position := Pos;
	Delete;
end;

procedure TKStreamData.Insert;
begin
	CheckBuffer;
	InsertBytes( Position, Buffer^, BufferSize );
	FBufferFilled := True;
	FBufferModified := True;
end;

procedure TKStreamData.InsertAt( Pos: Integer );
begin
	CheckBuffer;
	Position := Pos;
	Insert;
end;

procedure TKStreamData.Append;
begin
	CheckBuffer;
	Position := Size;
	WriteBuffer;
end;

procedure TKStreamData.Exchange( Pos: Integer );
begin
	CheckBuffer;
	ExchangeBytes( Position, Pos, BufferSize );
	FBufferModified := True;
end;

procedure TKStreamData.ExchangeAt( Origin, Pos: Integer );
begin
	CheckBuffer;
	Position := Origin;
	Exchange( Pos );
end;

procedure TKStreamData.SetBuffer( Value: Pointer );
begin
	CheckBuffer;
	if CheckPointer( Value ) then
	begin
		Clear;
		DoBufferChange( Buffer, Value );
		Move( Value^, Buffer^, BufferSize );
		FBufferModified := True;
		FBufferFilled := True;
	end;
end;

function TKStreamData.GetBuffer: Pointer;
begin
	InitBuffer;
	CheckBuffer;
	Result := FBuffer;
	DoBufferChange( nil, FBuffer );
end;

procedure TKStreamData.Prior;
begin
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	and Seek() also returns Integer ranged values... }
	if ( Position - ( 2 * Integer( BufferSize ) ) < 0 ) then
	begin
		Position := 0;
		Exit;
	end;
	Seek( ( -2 * Integer( BufferSize ) ), soFromCurrent );
	if ( Position <= ( Size - Integer( BufferSize ) ) ) then
		ReadBuffer;
end;

procedure TKStreamData.Next;
var
	bBOF: Boolean;
begin
	bBOF := BOF;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	if ( Position <= ( Size - Integer( BufferSize ) ) ) then
		ReadBuffer
	else
		Position := Size;
	if bBOF then
		Next;
end;

procedure TKStreamData.First;
begin
	Position := 0;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	also returns Integer ranged values... }
	if ( Position <= ( Size - Integer( BufferSize ) ) ) then
		StaticRead;
end;

procedure TKStreamData.Last;
begin
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	and Seek() also returns Integer ranged values... }
	Seek( - Integer( BufferSize ), soFromEnd );
	if ( Position <= ( Size - Integer( BufferSize ) ) ) then
		ReadBuffer;
end;

function TKStreamData.MoveBy( Distance: Integer ): Integer;
var
	iOffSet,
	iPosition: Integer;
begin
	Result := 0;
	if ( Distance = 0 ) or ( EOF and BOF ) or
		 ( BOF and ( Distance < 0 ) ) or
		 ( EOF and ( Distance > 0 ) ) then
		Exit;
{ Reposition the stream accounting for the BOF and EOF cracks }
	if BOF then
		ReadBuffer;
	iPosition := Position;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Seek() and
	position also returns Integer ranged values... }
	iOffSet := ( Integer( BufferSize ) * ( Distance - Ord( not BOF ) ) );
	if ( ( iOffset + Position ) >= Size ) then
		iOffSet := ( ( Size - Position ) - Integer( BufferSize ) )
	else if ( ( iOffset + Position ) < 0 ) then
		iOffSet := - Position;
	Seek( iOffSet, soFromCurrent );
	if ( Position <= ( Size - Integer( BufferSize ) ) ) then
	begin
		if ( Position = 0 ) and ( iOffset < 0 ) then
		begin
			StaticRead;
			dec( iPosition, BufferSize );
		end
		else
			ReadBuffer;
	end;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, MoveBy
	also returns Integer ranged values... }
	Result := ( Abs( iPosition - Position ) div Integer( BufferSize ) );
end;

function TKStreamData.LocateBytes( const Buffer; Count: Integer ): Boolean;
var
	iSavePos: Integer;
	p1,
	p2: Pointer;
begin
	Result := False;
	if ( not CheckReference( Buffer ) ) then
		Exit;
	if ( Count <= 0 ) or ( Count > Size ) then
		Count := Size;
	iSavePos := Position;
	try
		GetMem( p1, Count );
		try
			ZeroMemory( p1, Count );
			p2 := @Buffer;
			{Position := 0;}
			while ( not ( Result or EOF ) ) do
			begin
				ReadDataBuffer( Position, p1^, Count );
				Result := CompareMem( p1, p2, Count );
			end;
		finally
			FreeMem( p1, Count );
		end;
	finally
		if ( not Result ) then
			Position := iSavePos;
	end;
end;

function TKStreamData.Locate( ABuffer: Pointer ): Boolean;
begin
	CheckBuffer;
	Result := LocateBytes( ABuffer^, BufferSize );
	if Result then
		Seek( -BufferSize, soFromCurrent );
	FBufferModified := True;
	FBufferFilled := True;	
end;

procedure TKStreamData.DoBufferChange( OldData, NewData: Pointer );
begin
	if Assigned( FOnBufferChange ) then
		FOnBufferChange( Self, OldData, NewData );
end;

function TKStreamData.DoNormalize( const NormalizeID: Integer;
	Data: Pointer ): Boolean;
begin
	Result := Assigned( FOnNormalize );
	if Result then
		FOnNormalize( Self, Data, NormalizeID, Result );
end;

procedure TKStreamData.NormalizeItems( StartPosOfs, BufStart, BufCount: Integer;
	Data: Pointer );
var
	i: Integer;
begin
	CheckBuffer;
	i := Size;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, NormalizeItems
	and position also uses Integer ranged values... }
	if  ( ( ( ( BufStart + BufCount ) * Integer( BufferSize ) ) + StartPosOfs ) > i ) or
			( ( ( BufStart * Integer( BufferSize ) ) + StartPosOfs ) > i ) then
		RaiseExceptionFmt( EKStreamData, sErrSDInvNormalizeOfs, [ClassName, i, BufStart,
			BufCount] );
	Position := ( BufStart * Integer( BufferSize ) );
	FBufferModified := True;
	for i := 0 to BufCount - 1 do
	begin
		StaticRead;
		if ( not DoNormalize( ( i + BufStart ), Data ) ) then
			Break;
		WriteBuffer;
	end;
end;

procedure TKStreamData.NormalizeItemsEx( Stream: TStream; StartPosOfs, BufStart,
  BufCount: Integer; Data: Pointer );
var
	i: Integer;
	pSave: Pointer;
begin
	ForceObject( Stream );
	CheckBuffer;
	pSave := DupBuffer;
	try
		i := Size;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, Position
	and Normalize also returns Integer ranged values... }
		if  ( ( ( ( BufStart + BufCount ) * Integer( BufferSize ) ) + StartPosOfs ) > i ) or
				( ( ( BufStart * Integer( BufferSize ) ) + StartPosOfs ) > i ) then
			RaiseExceptionFmt( EKStreamData, sErrSDInvNormalizeOfs, [ClassName, i, BufStart,
				BufCount] );
		Position := ( BufStart * Integer( BufferSize ) );
		FBufferModified := True;
		for i := 0 to BufCount - 1 do
		begin
			Stream.ReadBuffer( Buffer^, BufferSize );  { StaticRead... }
			Stream.Seek( -BufferSize, soFromCurrent );
			if ( not DoNormalize( ( i + BufStart ), Data ) ) then
				Break;
			Stream.WriteBuffer( Buffer^, BufferSize );
		end;
	finally
		Buffer := pSave;
		FreeMem( pSave, BufferSize );
	end;
end;

procedure TKStreamData.CheckNilBuffer;
begin
	if ( not CheckPointer( Buffer ) ) then
		RaiseException( EKStreamData, sErrSDCreateNewNilBuffer );
end;

function TKStreamData.DupBuffer: Pointer;
begin
	CheckNilBuffer;
	GetMem( Result, BufferSize );
	try
		ZeroMemory( Result, BufferSize );
		Move( Buffer^, Result^, BufferSize );
	except
		FreeMem( Result, BufferSize );
		raise;
	end;
end;

function TKStreamData.DupBufferEx( AStreamDataClass: TKStreamDataClass ): TKStreamData;
begin
	CheckNilBuffer;
	ForceClass( AStreamDataClass );
	Result := AStreamDataClass.Create( Owner );
	try
		AssignBufferTo( Result );
	except
		Result.Free;
		raise;
	end;
end;

function TKStreamData.CompareBytes( Pos1, Pos2, Count: Integer ): ShortInt;
var
	p1,
	p2: Pointer;
begin
	if ( Pos1 = Pos2 ) or ( Count = 0 ) then
	begin
		Result := 0;
		Exit;
	end;
	GetMem( p1, Count );
	try
		ZeroMemory( p1, Count );
		GetMem( p2, Count );
		try
			ZeroMemory( p2, Count );
			ReadDataBuffer( Pos1, p1^, Count );
			ReadDataBuffer( Pos2, p2^, Count );
			Result := CompareMemBytes( p1, p2, Count );
		finally
			FreeMem( p2, Count );
		end;
	finally
		FreeMem( p1, Count );
	end;
end;

function TKStreamData.GetBufferAt( Pos, Count: Integer ): Pointer;
begin
	GetMem( Result, Count );
	try
		ZeroMemory( Result, Count );
		ReadDataBuffer( Pos, Result^, Count );
	except
		FreeMem( Result, Count );
		raise;
	end;
end;

procedure TKStreamData.QuickSortBytes( StartPos, EndPos, Count: Integer;
	CompareProc: TKStreamCompareBufferProc; SwapProc: TKStreamSwapProc );

	procedure InternalQuickSort( iStart, iEnd, iCount: Integer );
	var
		iAfter,
		iBefore,
		iHalf: Integer;
		pItem: Pointer;
	begin
		if ( iStart >= iEnd ) or ( ( iEnd + iCount ) > Size ) or
			( ( iStart + iCount ) > iEnd ) then
			RaiseExceptionFmt( EKStreamData, sErrSDInvQuickSortOfs, [iStart, iEnd] );
		iBefore := iStart;
		iAfter := iEnd;
		iHalf := CalcHalfPos( iBefore, iAfter, iCount );
		pItem := GetBufferAt( iHalf, iCount );
		try
			repeat
{ Buffer[iBefore] < pItem ? <-1;>1;=0 }
				while ( CompareProc( pItem^, iBefore, iCount ) < 0 ) do
					Inc( iBefore, iCount );
{ Buffer[iAfter] < pItem ? <-1;>1;=0 }
				while ( CompareProc( pItem^, iAfter, iCount ) > 0 ) do
					Dec( iAfter, iCount );
				if ( iBefore <= iAfter ) then
				begin
					SwapProc( iBefore, iAfter, iCount );
					Inc( iBefore, iCount );
					Dec( iAfter, iCount );
				end;
			until ( iBefore > iAfter );
		finally
			FreeMem( pItem, iCount );
		end;
{ See Wirth Niklaus, Algorithms and Data Structures, pag.71 }
		if ( iStart < iAfter ) then
			InternalQuickSort( iStart, iAfter, iCount );
		if ( iBefore < iEnd ) then
			InternalQuickSort( iBefore, iEnd, iCount );
	end;

begin
	if ( ( ( EndPos - StartPos ) mod Count ) <> 0 ) then
		RaiseException( EKStreamData, sErrSDInvQuickSortRegion );
	{
	Dec( StartPos, ( StartPos mod Count ) );
	Dec( EndPos, ( EndPos mod Count ) );
	}
	if ( not CheckReference( @SwapProc ) ) then
		SwapProc := ExchangeBytes;
	if ( not CheckReference( @CompareProc ) ) then
		CompareProc := CompareBuffer;
	Position := StartPos;
	InternalQuickSort( StartPos, EndPos, Count );
end;

procedure TKStreamData.QuickSort( StartPos, BuffCount: Integer;
	CompareProc: TKStreamCompareBufferProc; SwapProc: TKStreamSwapProc );
begin
	CheckBuffer;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, QuickSortBytes
	also uses Integer ranged values... }
	QuickSortBytes( StartPos, StartPos + ( Integer( BufferSize )* BuffCount ), Integer( BufferSize ),
		CompareProc, SwapProc );
	FBufferModified := True;
end;

function TKStreamData.CompareBuffer( const ABuffer; Pos, Count: Integer ): ShortInt;
var
	p1,
	p2: Pointer;
begin
{ What will happen if ABuffer is nil? It is the caller's responsibility to set
	this parameter correctly... }
	GetMem( p1, Count );
	try
		ZeroMemory( p1, Count );
		p2 := @ABuffer;
		ReadDataBuffer( Pos, p1^, Count );
		Result := CompareMemBytes( p1, p2, Count );
	finally
		FreeMem( p1, Count );
	end;
end;

function TKStreamData.BinarySearchBytes( const ABuffer; StartPos, EndPos, Count: Integer;
	CompareProc: TKStreamCompareBufferProc ) : Boolean;
var
	iMid,
	iResp: Integer;
begin
	if ( ( ( EndPos - StartPos ) mod Count ) <> 0 ) then
		RaiseException( EKStreamData, sErrSDInvBynarySearchRegion );
	ForceReference( ABuffer );
	if ( not CheckReference( @CompareProc ) ) then
		CompareProc := CompareBuffer;
	Result := False;
	iMid := CalcHalfPos( StartPos, EndPos, Count );
	while ( StartPos <= EndPos ) and ( iMid <> EndPos ) and ( iMid <> StartPos ) do
	begin
{ Buffer[imid] < Abuffer? }
		iResp := CompareProc( ABuffer, iMid, Count );
		if ( iResp > 0 ) then
			EndPos := iMid
		else if ( iResp < 0 ) then
			StartPos := iMid
		else
		begin
			Result := True;
			Position := iMid;
			Exit;
		end;
		iMid := CalcHalfPos( StartPos, EndPos, Count );
	end;
end;

function TKStreamData.BinarySearch( StartPos, BuffCount: Integer;
	CompareProc: TKStreamCompareBufferProc ): Boolean;
begin
	CheckBuffer;
{ Here we must TypeCast BufferSize to Integer because in Delphi4 cardinal
	violates the Integer Range and generates a warning here. Even, BinarySearchBytes
	also uses Integer ranged values... }
	Result := BinarySearchBytes( Buffer^, StartPos, StartPos +
		( Integer( BufferSize ) * BuffCount ), Integer( BufferSize ),
	 	CompareProc );
	FBufferModified := True;
end;

end.
