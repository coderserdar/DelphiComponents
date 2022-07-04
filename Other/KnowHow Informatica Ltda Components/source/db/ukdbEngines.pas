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

unit ukdbEngines;

{$I s:\v100\include\iKLIB100.inc}
{$BOOLEVAL OFF}

interface

uses
	Windows, Messages, SysUtils, Classes, DB, DBTables, uksyConsts, uksyShortCuts,
  uksyTypes, uksyUtils, uksyClasses, ukrConsts, ukrClasses, ukrEngines, ukdbConsts,
  ukdbClasses;

type

	EKDBEngines = class( EKDB );
	EKDatasetAuditory = class( EKDBEngines );
	TKCustomDatasetAuditor = class;

{
---------------------------------------------------------------------------------
---------------------------- DB Log File Architecture ---------------------------
---------------------------------------------------------------------------------
}

	TKADataset = record
		Msg: Cardinal;
		Dataset: TDataset; 		 {WParam}
		KDSAuditInfo: Pointer; {LParam}
		Result: Integer;
	end;

	TKCustomDatasetLog = class;

	TKDatasetLogErrorEvent = procedure( Sender: TKCustomDatasetAuditor;
		Dataset: TDataset; var LogErrorAction: TKLogErrorAction ) of object;

{ TKCustomDatasetLog }

	TKCustomDatasetLog = class( TKCustomAuditoryLog )
	private
		FOnDatasetLogError: TKDatasetLogErrorEvent;

		function GetDatasetClass: string;
		function GetDatasetName: string;
		function GetDatasetOwnerName: string;
		function GetDatasetDataBaseName: string;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoOnLogFileLogError( Data: Pointer;
			var LogErrorAction: TKLogErrorAction ); override;

	protected
{ Each Dataset will provide a properly action name }
		function GetDatasetCurrentAction: string; virtual; abstract;
		function GetDatasetDetDefExt: string; virtual; abstract;
		function GetDataSetAuditGrpIdx: string; virtual; abstract;

		procedure DoOnDatasetLogError( Dataset: TDataset;
			var LogErrorAction: TKLogErrorAction ); dynamic;

		procedure DefineFixedFields; override;
		procedure DefineCustomFields; override;

		function GetLogOwner: TKCustomDatasetAuditor; virtual;

		property OnDatasetLogError: TKDatasetLogErrorEvent
						 read FOnDatasetLogError write FOnDatasetLogError;

	public
		constructor Create( AOwner: TComponent; const AFileName: TFileName ); override;

		property Owner: TKCustomDatasetAuditor
						 read GetLogOwner;

	end;

{ TKTableLog }

	TKTableLog = class( TKCustomDatasetLog )
	private
		function GetDatasetIndexFldName: string;
		function GetDatasetTableName: string;

	protected
		function GetDatasetCurrentAction: string; override;
		function GetDatasetDetDefExt: string; override;
		function GetDataSetAuditGrpIdx: string; override;

		procedure DefineFixedFields; override;

	public
		property OnDatasetLogError;

	end;

{ TKQueryLog }

	TKQueryLog = class( TKCustomDatasetLog )
	private
		function GetDatasetKeyFields: string;

	protected
		function GetDatasetCurrentAction: string; override;
		function GetDatasetDetDefExt: string; override;
		function GetDataSetAuditGrpIdx: string; override;

		procedure DefineFixedFields; override;

	public
	  property OnDatasetLogError;	

	end;

{
---------------------------------------------------------------------------------
--------------------------- DB Log Engine Architecture --------------------------
---------------------------------------------------------------------------------
}

{ TKCustomDatasetLogEngine }

	TKCustomDatasetLogEngine = class( TKCustomAuditoryLogEngine )
	public
		constructor Create( AOwner: TComponent ); override;

		property HeaderSize;
		property HighFieldLengh;
		property FieldCount;
		property FileLoaded;
		property FixedFieldCount;
		property LogRecords;
		property LogFields;
		property LogFieldsSize;
		property LogRecordSize;
		property RecordCount;
		property VirtualFieldCount;

	published
		property AutoClearLog; 
		property AutoClearLogType;
		property AutoLoad default True;
		property DaysToKeep;
		property FileName;
		property MaxByteSize;

	end;

{ TKTableLogEngine }

	TKTableLogEngine = class( TKCustomDatasetLogEngine )
	protected
		function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKQueryLogEngine }

	TKQueryLogEngine = class( TKCustomDatasetLogEngine )
	protected
		function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;
		
	public
		constructor Create( AOwner: TComponent ); override;

	end;

{
---------------------------------------------------------------------------------
-------------------------- DB Log Auditor Architecture --------------------------
---------------------------------------------------------------------------------
}

{------------------------------ Dataset Link Class -----------------------------}

	TKDatasetLink = class;

	TLinkPrepareDatasetLog = procedure ( Sender: TKCustomDatasetAuditor; Link: TKDatasetLink;
		Dataset: TDataset ) of object;
	TLinkDatasetLog = procedure ( Sender: TKCustomDatasetAuditor; Link: TKDatasetLink;
		LogRecords: TStrings ) of object;

	TKDatasetLink = class( TKCustomLink )
	private
		FBeforePrepareDatasetLog : TLinkPrepareDatasetLog;
		FAfterPrepareDatasetLog : TLinkPrepareDatasetLog;
		FBeforeDatasetLog : TLinkDatasetLog;
		FAfterDatasetLog : TLinkDatasetLog;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	public
		property Owner;

		property BeforePrepareDatasetLog: TLinkPrepareDatasetLog
						 read FBeforePrepareDatasetLog write FBeforePrepareDatasetLog;
		property AfterPrepareDatasetLog: TLinkPrepareDatasetLog
						 read FAfterPrepareDatasetLog write FAfterPrepareDatasetLog;
		property BeforeDatasetLog: TLinkDatasetLog
						 read FBeforeDatasetLog write FBeforeDatasetLog;
		property AfterDatasetLog: TLinkDatasetLog
						 read FAfterDatasetLog write FAfterDatasetLog;

	end;

{----------------- Dataset Audtiory Actions Aggregate Object -------------------}

{ 0 from 31 to get the RTTI streaming ability! Same as TIntegerSet }

	TKDatasetAction = 0..SizeOf( Integer ) * BITS_PER_BYTE - 1;

{ not automatically RTTI able! }

	TKDatasetActions = set of TKDatasetAction;

{ TKAggregateDatasetActions }

	TKAggregateDatasetActions = class( TPersistent )
	private
		FOwner: TDataset;
		FDatasetActions: TKDatasetActions;

		function GetDatasetActionsAsString: string;
		procedure SetDatasetActionsAsString( const Value: string );

    procedure WriteData( Writer: TWriter );
		procedure ReadData( Reader: TReader );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DefineProperties( Filer: TFiler ); override;

	public
		constructor Create( AOwner: TDataset ); virtual;

		procedure Assign( Source: TPersistent ); override;

		property Dataset: TDataset
						 read FOwner;
		property AsString: string
						 read GetDatasetActionsAsString write SetDatasetActionsAsString;
		property DSActions: TKDatasetActions
						 read FDatasetActions write FDatasetActions;

	end;

{ TKCustomDatasetAuditor }
  
	TKDatasetRequestFileNameEvent = procedure( Sender: TKCustomDatasetAuditor;
		Dataset: TDataset; var FileName: string; const FileDate: TDateTime;
		const FileSize: LongInt ) of object;

	TKCustomDatasetAuditor = class( TKCustomAuditable )
	private
		FDataset: TDataset;
		FLogRecords: TStrings;
		FKDSRequestFileName: TKDatasetRequestFileNameEvent;

		function GetDatasetLogError: TKDatasetLogErrorEvent;
		procedure SetDatasetLogError( Value: TKDatasetLogErrorEvent );

		{ Derived classes can implement this method to take custom actions}
		procedure KADataset( var Message: TKADataset ); message KA_Dataset;

{$HINTS OFF}
		property AuditoryLog;
{$HINTS ON}

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function WndProc( var Message: TMessage ): Boolean; override;

	protected
		procedure DoAuditory( Event: TKAuditEvent; WParam, LParam: LongInt ); override;
		procedure DoRequestFileName( Data: Pointer ); override;

		function GetDataSet: TDataSet;
		function GetAuditoryLog: TKCustomDatasetLog;

		procedure VerifyAndAdjustCustomFields; virtual;

		function GetAuditoryDataSetClass: TDataSetClass; virtual; abstract;
		procedure InternalAnnounceDataSetLog( sl: TStrings ); virtual; abstract;
		function InternalPrepareDataSetLog: Boolean; virtual; abstract;

		property Dataset: TDataset
						 read GetDataSet;
		property LogRecords: TStrings
						 read FLogRecords;
		property OnRequestFileName: TKDatasetRequestFileNameEvent
						 read FKDSRequestFileName write FKDSRequestFileName;
    property OnDatasetLogError: TKDatasetLogErrorEvent
						 read GetDatasetLogError write SetDatasetLogError;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

		property DatasetLog: TKCustomDatasetLog
						 read GetAuditoryLog;

		property Handle;
    property FileName;
		property AppUserName;
		
	end;

{ TKCustomAggregateDatasetAuditor }

	TKCustomAggregateDatasetAuditor = class( TKCustomAggregateAuditable )
	protected
		function GetDatasetAuditor: TKCustomDatasetAuditor; virtual;

	public
		constructor Create( AClass: TKCustomAuditableClass ); override;

		property Owner: TKCustomDatasetAuditor
						 read GetDatasetAuditor;

	end;

	TKCustomAggregateDatasetAuditorClass = class of TKCustomAggregateDatasetAuditor;

{ TKTableAuditor }

	TKTableAuditor = class( TKCustomDatasetAuditor )
	protected
		function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;
		function GetAuditoryDataSetClass: TDataSetClass; override;
		procedure InternalAnnounceDataSetLog( sl: TStrings ); override;
		function InternalPrepareDataSetLog: Boolean; override;

	public
    property DataSet;

	published
		property Enabled;
		property OnRequestFileName;
		property OnDatasetLogError;

	end;

{ TKQueryAuditor }

	TKQueryAuditor = class( TKCustomDatasetAuditor )
	protected
		function GetAuditoryLogClass: TKCustomAuditoryLogClass; override;
		function GetAuditoryDataSetClass: TDataSetClass; override;
		procedure InternalAnnounceDataSetLog( sl: TStrings ); override;
		function InternalPrepareDataSetLog: Boolean; override;

	public
		property DataSet;
		
	published
		property Enabled;
		property OnRequestFileName;
		property OnDatasetLogError;

	end;

{ TKCustomDBLogEngine }

  EKDBLogEngine = class( EKDBEngines );

  TKCustomDBLogEngine = class;

  TKDBLogEngineSQLEvent = procedure( Sender: TKCustomDBLogEngine;
    LogEngine: TKCustomLogEngine; var SQL: string; var Continue: Boolean ) of object;
  TKDBLogEngineFillParamsEvent = procedure( Sender: TKCustomDBLogEngine;
    Query: TQuery; const RecordNo: Integer; Params: TStrings;
    var Continue: Boolean ) of object;
  TKDBLogEngineSelectFields = procedure( Sender: TKCustomDBLogEngine; Fields: TStrings;
    var Continue: Boolean ) of object;

  TKCustomDBLogEngine = class( TKCustomLinkable )
  private
    FData: Pointer;
    FQuery: TQuery;
    FSQLFields: TStrings;
    FSQLParams: TStrings;
    FDataBase: TDataBase;
    FTableName: string;
    FTransactionSafe: Boolean;
    FCloseDataBase: Boolean;
    FInsertingDBLog: Boolean;
    FCreatingDBLog: Boolean;
    FQueryValidated: Boolean;
    FLogEngineValidated: Boolean;
    FLogEngine: TKCustomLogEngine;
    FOnAbort: TNotifyEvent;
    FOnCreateTable: TKDBLogEngineSQLEvent;
    FOnInsertRecords: TKDBLogEngineSQLEvent;
    FOnSelectFields: TKDBLogEngineSelectFields;
    FOnFillParams: TKDBLogEngineFillParamsEvent;

    procedure SetDatabase( Value: TDataBase );
    procedure SetTableName( const Value: string );
    procedure SetLogEngine( Value: TKCustomLogEngine );

    procedure GetSQLParamsValues( RecNo: Integer );
    procedure GetLogFields;

  protected
    procedure ValidateQuery; dynamic;
    procedure ValidateLogEngine; dynamic;
    procedure ValidateFieldParamsValues; dynamic;

    procedure DoAbort; dynamic;
    function DoCreateTable( var SQL: string ): Boolean; dynamic;
    function DoInsertRecords( var SQL: string ): Boolean; dynamic;
    function DoFillParams( RecordNo: Integer; Params: TStrings ): Boolean; dynamic;
    function DoSelectFields( Fields: TStrings ): Boolean; dynamic;

    procedure GetInsertSQL( var sFields, sParams: string ); virtual;
    function CreateDDLSQLFields: string; virtual;
    procedure CreateTable( sSQL: string ); virtual;
    procedure CreateInsertSQL; virtual;
    procedure InsertTableRecords; virtual;
    procedure CleanUp; virtual;
    procedure InitEngine; virtual;
    procedure DropTable; virtual;

    property QueryValidated: Boolean
             read FQueryValidated;
    property LogEngineValidated: Boolean
             read FLogEngineValidated;

    property Data: Pointer
             read FData write FData;

    property SQLFields: TStrings
             read FSQLFields;
    property SQLParams: TStrings
             read FSQLParams;

    property Query: TQuery
             read FQuery;

    property CloseDataBase: Boolean
             read FCloseDataBase write FCloseDataBase default True;
{ We MUST use the database component instead of string name because the need to
  check if it's SQL Based. The query.database at that point was not yet set }
    property Database: TDataBase
             read FDataBase write SetDatabase;
    property LogEngine: TKCustomLogEngine
             read FLogEngine write SetLogEngine;
    property TableName: string
             read FTableName write SetTableName;
    property TransactionSafe: Boolean
             read FTransactionSafe write FTransactionSafe default True;
    property OnAbort: TNotifyEvent
             read FOnAbort write FOnAbort;
    property OnCreateTable: TKDBLogEngineSQLEvent
             read FOnCreateTable write FOnCreateTable;
    property OnInsertRecords: TKDBLogEngineSQLEvent
             read FOnInsertRecords write FOnInsertRecords;
    property OnFillParams: TKDBLogEngineFillParamsEvent
             read FOnFillParams write FOnFillParams;
    property OnSelectFields: TKDBLogEngineSelectFields
             read FOnSelectFields write FOnSelectFields;

  public
    destructor Destroy; override;
    constructor Create( AOwner: TComponent ); override;

    procedure CreateDBLog; virtual;
    procedure InsertDBLog; virtual;

    property InsertingDBLog: Boolean
             read FInsertingDBLog;
    property CreatingDBLog: Boolean
             read FCreatingDBLog;
    
  end;

{ TKDBLogEngine }

  TKDBLogEngine = class( TKCustomDBLogEngine )
  public
    property QueryValidated;
    property LogEngineValidated;

    property SQLFields;
    property SQLParams;

    property Query;

  published
    property CloseDataBase;
    property Database;
    property LogEngine;
    property TableName;
    property TransactionSafe;
    property OnAbort;
    property OnCreateTable;
    property OnInsertRecords;
    property OnFillParams;
    property OnSelectFields;

  end;

{ TKDBLogManager }

  EKDBLogManager = class( EKDBEngines );

  TKDBLog = class;
  TKDBLogs = class;
  TKDBLogManager = class;

{ TKDBLog }

  TKDBLogType = ( dbltCreateInsert, dbltInsert );
  TKDBLogAbortType = ( dblatIgnore, dbltRetry, dblatAbortGroup, dblatAbortAll );
  TKDBLogFileAction = ( dblfaDelete, dblfaClear, dblfaNone );
             
  TKDBLogManagerSQLEvent = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
    LogEngine: TKCustomLogEngine; var SQL: string; var Continue: Boolean ) of object;
  TKDBLogManagerFillParamsEvent = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
    Query: TQuery; const RecordNo: Integer; Params: TStrings;
    var Continue: Boolean ) of object;
  TKDBLogManagerSelectFields = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
    Fields: TStrings; var Continue: Boolean ) of object;
  TKDBLogManagerGetLogEngineEvent = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
    var LogEngine: TKCustomLogEngine ) of object;

	TKDBLog = class( TKCustomCollectionItem )
	private
    FFileName: string;
    FTableName: string;
    FCloseDataBase: Boolean;
    FTransactionSafe: Boolean;
    FDBLogType: TKDBLogType;
    FLogEngine: TKCustomLogEngine;
    FLogAbortType: TKDBLogAbortType;
    FDBLogFileAction: TKDBLogFileAction;
    FEngineOwnerShip: TKObjectOwnerShip;
    FOnAbort: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FOnCreateTable: TKDBLogManagerSQLEvent;
    FOnInsertRecords: TKDBLogManagerSQLEvent;
    FOnSelectFields: TKDBLogManagerSelectFields;
    FOnFillParams: TKDBLogManagerFillParamsEvent;
    FOnGetLogEngine: TKDBLogManagerGetLogEngineEvent;

    function GetQuery: TQuery;
		function GetOwnerCollection: TKDBLogs;

    procedure SetTableName( const Value: string );
    procedure SetFileName( const Value: string );

	protected
    function DoGroupAbort: Boolean; dynamic;
    function DoGetLogEngine: TKCustomLogEngine; dynamic;
    function DoLogAbortType( E: Exception ): TKDBLogAbortType; dynamic;

    procedure DoAbort; dynamic;
		procedure DoAfterExecute; dynamic;
		procedure DoBeforeExecute; dynamic;
    function DoCreateTable( var SQL: string ): Boolean; dynamic;
    function DoInsertRecords( var SQL: string ): Boolean; dynamic;
    function DoFillParams( RecordNo: Integer; Query: TQuery; Params: TStrings ): Boolean; dynamic;
    function DoSelectFields( Fields: TStrings ): Boolean; dynamic;

    procedure FreeEngine; virtual;

    property LogEngine: TKCustomLogEngine
             read FLogEngine;
    property LogAbortType: TKDBLogAbortType
             read FLogAbortType;

	public
    destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

    function Execute: Boolean; virtual;

		property Owner: TKDBLogs
						 read GetOwnerCollection;
    property Query: TQuery
             read GetQuery;

	published
		property Name;
    property Enabled;
    property GroupIndex;

    property DBLogFileAction: TKDBLogFileAction
             read FDBLogFileAction write FDBLogFileAction default dblfaDelete;
    property DBLogType: TKDBLogType
             read FDBLogType write FDBLogType default dbltCreateInsert;
    property EngineOwnerShip: TKObjectOwnerShip
             read FEngineOwnerShip write FEngineOwnerShip default oosOwned;
    property CloseDataBase: Boolean
             read FCloseDataBase write FCloseDataBase default True;
    property FileName: string
             read FFileName write SetFileName;
    property TableName: string
             read FTableName write SetTableName;
    property TransactionSafe: Boolean
             read FTransactionSafe write FTransactionSafe default True;

    property OnAbort: TNotifyEvent
             read FOnAbort write FOnAbort;
		property BeforeExecute: TNotifyEvent
						 read FBeforeExecute write FBeforeExecute;
		property AfterExecute: TNotifyEvent
						 read FAfterExecute write FAfterExecute;
    property OnCreateTable: TKDBLogManagerSQLEvent
             read FOnCreateTable write FOnCreateTable;
    property OnInsertRecords: TKDBLogManagerSQLEvent
             read FOnInsertRecords write FOnInsertRecords;
    property OnFillParams: TKDBLogManagerFillParamsEvent
             read FOnFillParams write FOnFillParams;
    property OnSelectFields: TKDBLogManagerSelectFields
             read FOnSelectFields write FOnSelectFields;
    property OnGetLogEngine: TKDBLogManagerGetLogEngineEvent
             read FOnGetLogEngine write FOnGetLogEngine;

	end;

{ TKDBLogs }

  TKDBLogFunc = function( Item: TKDBLog; Data: Pointer ): Boolean of object;

	TKDBLogs = class( TKCustomCollection )
	private
    FLastDBLog: TKDBLog;

		function GetOwnerComp: TKDBLogManager;

		procedure SetItem( Index: Integer; AItem: TKDBLog );
		function GetItem( Index: Integer ): TKDBLog;

		function GetItemByName( const AName: string ): TKDBLog;

    procedure CreateTableEvent( Sender: TKCustomDBLogEngine; LogEngine: TKCustomLogEngine;
      var SQL: String; var Continue: Boolean );
    procedure InsertRecordsEvent( Sender: TKCustomDBLogEngine; LogEngine: TKCustomLogEngine;
      var SQL: String; var Continue: Boolean );
    procedure FillParamsEvent( Sender: TKCustomDBLogEngine; Query: TQuery;
      const RecordNo: Integer; Params: TStrings; var Continue: Boolean );
    procedure SelectFieldsEvent( Sender: TKCustomDBLogEngine; Fields: TStrings;
      var Continue: Boolean );
    procedure AbortEvent( Sender: TObject );

	protected
    procedure Update( Item: TCollectionItem ); override;

		{ Design-time editor support }
		function GetAttrCount: Integer; override;
		function GetAttr( Index: Integer ): string; override;
		function GetItemAttr( Index, ItemIndex: Integer ): string; override;

	public
		constructor Create( AComp: TKDBLogManager ); virtual;

		function Add: TKDBLog; virtual;

    property Component: TKDBLogManager
						 read GetOwnerComp;
		property Items[Index: Integer]: TKDBLog
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKDBLog
						 read GetItemByName;
    property LastDBLog: TKDBLog
             read FLastDBLog;
		property Names;

	end;

{ TKDBLogManagerLink }

  TKDBLogManagerLink = class;

	TKDBLogManagerLinkEvent = procedure ( Sender: TKDBLogManager; Link: TKDBLogManagerLink;
		ItemIndex: Cardinal ) of object;

	TKDBLogManagerLink = class( TKCustomLink )
	private
		FBeforeExecAll: TKDBLogManagerLinkEvent;
		FBeforeExecGroup: TKDBLogManagerLinkEvent;
		FBeforeExecItem: TKDBLogManagerLinkEvent;
		FAfterExecItem: TKDBLogManagerLinkEvent;
		FAfterExecGroup: TKDBLogManagerLinkEvent;
		FAfterExecAll: TKDBLogManagerLinkEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
  	procedure DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt ); override;

	public
		property BeforeExecAll: TKDBLogManagerLinkEvent
						 read FBeforeExecAll write FBeforeExecAll;
		property BeforeExecGroup: TKDBLogManagerLinkEvent
						 read FBeforeExecGroup write FBeforeExecGroup;
		property BeforeExecItem: TKDBLogManagerLinkEvent
						 read FBeforeExecItem write FBeforeExecItem;
		property AfterExecItem: TKDBLogManagerLinkEvent
						 read FAfterExecItem write FAfterExecItem;
		property AfterExecGroup: TKDBLogManagerLinkEvent
						 read FAfterExecGroup write FAfterExecGroup;
		property AfterExecAll: TKDBLogManagerLinkEvent
						 read FAfterExecAll write FAfterExecAll;

		property Owner;

	end;
  
{ TKDBLogManager }

  TKDBLogManagerLogAbortEvent = procedure( Sender: TKDBLogManager; E: Exception;
    DBLog: TKDBLog; var AbortType: TKDBLogAbortType ) of object;
	TKDBLogManagerGroupAbortEvent = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
		const GroupIndex: Cardinal; var AbortGroup: Boolean ) of object;
  TKDBLogManagerLoadErrorEvent = procedure( Sender: TKDBLogManager; DBLog: TKDBLog;
		const GroupIndex: Cardinal ) of object;

	TKDBLogManager = class( TKCustomLinkable )
	private
		FDBLogs: TKDBLogs;
    FQuitting: Boolean;
    FIsLoading: Boolean;
    FDBLogEngine: TKCustomDBLogEngine;
    FOnLogAbort: TKDBLogManagerLogAbortEvent;
    FOnLoadError: TKDBLogManagerLoadErrorEvent;
    FOnGroupAbort: TKDBLogManagerGroupAbortEvent;

    function GetDataBase: TDataBase;
		procedure SetDBLogs( Value: TKDBLogs );
    procedure SetDataBase( Value: TDataBase );

  protected
    procedure UpdateItem( Item: TKDBLog ); virtual;

    procedure DoLoadError( Item: TKDBLog ); dynamic;
 		procedure MergeFromStream( Stream: TStream ); dynamic;
		procedure LoadFromStream( Stream: TStream ); dynamic;
		procedure SaveToStream( Stream: TStream ); dynamic;

    property DBLogEngine: TKCustomDBLogEngine
             read FDBLogEngine;
    property Quiting: Boolean
             read FQuitting;
    property IsLoading: Boolean
             read FIsLoading;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

		function Execute: Boolean; virtual;
		function ExecuteGroup( GroupIndex: Cardinal ): Boolean; dynamic;
		function ExecuteItems( const Items: array of Cardinal ): Boolean; dynamic;

		procedure Quit;

		procedure MergeFromFile( const FileName: string );
		procedure LoadFromFile( const FileName: string );
		procedure SaveToFile( const FileName: string );

	published
		property DBLogs: TKDBLogs
						 read FDBLogs write SetDBLogs;
    property Database: TDataBase
             read GetDataBase write SetDatabase;
    property OnLogAbort: TKDBLogManagerLogAbortEvent
             read FOnLogAbort write FOnLogAbort;
    property OnGroupAbort: TKDBLogManagerGroupAbortEvent
             read FOnGroupAbort write FOnGroupAbort;
		property OnLoadError: TKDBLogManagerLoadErrorEvent
						 read FOnLoadError write FOnLoadError;

	end;

function KDatasetActionToIdent( DSAction: Integer; var Ident: string ): Boolean;
function IdentToKDatasetAction( const Ident: string; var DSAction: Integer ): Boolean;
function KDatasetActionToString( const DSAction: TKDatasetAction ): string;
function StringToKDatasetAction( const Source: string ): TKDatasetAction;

procedure ForceKDSActionByValue( const Value: TKDatasetAction );
procedure ForceKDSActionByName( const Value: string );

procedure GetKDatasetActionValues( Proc: TGetStrProc );

implementation

uses
	Consts, ukrUtils, ukrDBUtils, ukdbResStr, ukdbUtils, ukdbTables;

{
---------------------------------------------------------------------------------
---------------------------- DB Log File Architecture ---------------------------
---------------------------------------------------------------------------------
}

{----------------------------- TKCustomDatasetLog ------------------------------}

constructor TKCustomDatasetLog.Create( AOwner: TComponent; const AFileName: TFileName );
begin
	inherited Create( AOwner, AFileName );
	DefFileExt := GetDatasetDetDefExt;
end;

function TKCustomDatasetLog.GetLogOwner: TKCustomDatasetAuditor;
begin
	Result := TKCustomDatasetAuditor( inherited GetLogOwner );
end;

function TKCustomDatasetLog.GetDatasetClass: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := Owner.Dataset.ClassName
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKCustomDatasetLog.GetDatasetName: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := Owner.Dataset.Name
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKCustomDatasetLog.GetDatasetOwnerName: string;
begin
	if CheckObject( Owner.Dataset ) then
	begin
		if CheckObject( Owner.Dataset.Owner ) then
			Result := Owner.Dataset.Owner.Name
		else
			Result := DSL_DATASETOWNER_NOTASSIGNED;
	end
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKCustomDatasetLog.GetDatasetDataBaseName: string;
begin
	if CheckObject( Owner.Dataset ) then
	begin
		if CheckObjectClass( Owner.Dataset, TDBDataset ) then
			Result := ( Owner.Dataset as TDBDataset ).DataBaseName
		else
			Result := DSL_DATASET_NOTDBDATASET;
	end
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

procedure TKCustomDatasetLog.DefineFixedFields;
begin
	inherited DefineFixedFields;
{ Do not resource Field Names }
	DefineFixedField( DSA_CLASSNAME_FIELD, MAXSIZE_PASCALID, GetDatasetClass );
	DefineFixedField( DSA_NAME_FIELD, MAXSIZE_PASCALID, GetDatasetName );
	DefineFixedField( DSA_OWNER_NAME_FIELD, MAXSIZE_PASCALID, GetDatasetOwnerName );
	DefineFixedField( DSA_CURACTION_FIELD, MAXSIZE_PASCALID, GetDatasetCurrentAction );
	DefineFixedField( DSA_DBNAME_FIELD, MAXSIZE_PASCALID, GetDatasetDataBaseName );
	DefineFixedField( DSA_AUDITGRPIDX_FIELD, MAXSIZE_INTEGER_STR, GetDataSetAuditGrpIdx );
end;

procedure TKCustomDatasetLog.DefineCustomFields;
begin
	inherited DefineCustomFields;
{ Do not resource Field Names }
	DefineCustomField( DSA_ADDINFO_FIELD, DSA_ADDINFO_FIELD_LEN );
end;

procedure TKCustomDatasetLog.DoOnLogFileLogError( Data: Pointer;
	var LogErrorAction: TKLogErrorAction );
begin
	DoOnDatasetLogError( TDataset( Data ), LogErrorAction );
end;

procedure TKCustomDatasetLog.DoOnDatasetLogError( Dataset: TDataset;
	var LogErrorAction: TKLogErrorAction );
begin
	if Assigned( FOnDatasetLogError ) and ( not Designing( Owner ) ) then
	  FOnDatasetLogError( Owner, Dataset, LogErrorAction );
end;

{--------------------------------- TKTableLog ----------------------------------}

function TKTableLog.GetDataSetAuditGrpIdx: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := IntToStr( ( Owner.DataSet as TKTable ).AuditGroupIndex )
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKTableLog.GetDatasetCurrentAction: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := dsaNames[( Owner.Dataset as TKTable ).CurrentAction]
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKTableLog.GetDatasetTableName: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := ( Owner.Dataset as TKTable ).TableName
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKTableLog.GetDatasetIndexFldName: string;
begin
	if CheckObject( Owner.Dataset ) then
	begin
		Result := ( Owner.Dataset as TKTable ).IndexFieldNames;
		if ( not CheckTrimStr( Result ) ) then
			Result := ( Owner.Dataset as TKTable ).IndexName;
	end
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKTableLog.GetDatasetDetDefExt: string;
begin
	Result := DSL_TABLE_LOGEXT;
end;

procedure TKTableLog.DefineFixedFields;
begin
	inherited DefineFixedFields;
{ Do not resource Field Names }
	DefineFixedField( TBLA_TABLENAME_FIELD, MAXSIZE_PASCALID, GetDatasetTableName );
	DefineFixedField( TBLA_IDXFLDNAME_FIELD, MAXSIZE_PASCALID, GetDatasetIndexFldName );
end;

{--------------------------------- TKQueryLog ----------------------------------}

function TKQueryLog.GetDataSetAuditGrpIdx: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := IntToStr( ( Owner.DataSet as TKQuery ).AuditGroupIndex )
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKQueryLog.GetDatasetCurrentAction: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := dsaNames[( Owner.Dataset as TKQuery ).CurrentAction]
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKQueryLog.GetDatasetKeyFields: string;
begin
	if CheckObject( Owner.Dataset ) then
		Result := ( Owner.Dataset as TKQuery ).KeyFields
	else
		Result := DSL_DATASET_NOTASSIGNED;
end;

function TKQueryLog.GetDatasetDetDefExt: string;
begin
	Result := DSL_QUERY_LOGEXT;
end;

procedure TKQueryLog.DefineFixedFields;
begin
	inherited DefineFixedFields;
{ Do not resource Field Names }
	DefineFixedField( QUYA_KEYFIELDS_FIELD, QUYA_KEYFIELDS_FIELD_LEN, GetDatasetKeyFields );
end;

{
---------------------------------------------------------------------------------
--------------------------- DB Log Engine Architecture --------------------------
---------------------------------------------------------------------------------
}

{-------------------------- TKCustomDatasetLogEngine ---------------------------}

constructor TKCustomDatasetLogEngine.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	AutoLoad := True;
end;

{------------------------------ TKTableLogEngine -------------------------------}

constructor TKTableLogEngine.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	DefFileExt := DSL_TABLE_LOGEXT;
end;

function TKTableLogEngine.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKTableLog;
end;

{------------------------------ TKQueryLogEngine -------------------------------}

constructor TKQueryLogEngine.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	DefFileExt := DSL_QUERY_LOGEXT;
end;

function TKQueryLogEngine.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKQueryLog;
end;

{
---------------------------------------------------------------------------------
-------------------------- DB Log Auditor Architecture --------------------------
---------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	PKDSAuditory = ^TKDSAuditory;
	TKDSAuditory = record
		AuditEvent: TKAuditEvent;
		Handled: Boolean;
		CurrentAction: TKDatasetAction;
		usSize: Cardinal;
		usBuff: Pointer;
	end;

	TWriterHack = class( TWriter );

{---------------------------- Public Implementation ----------------------------}

{------------------------------ Dataset Link Class -----------------------------}

procedure TKDatasetLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	case LinkEvent of
		leBeforePrepareDatasetLog:
			if Assigned( FBeforePrepareDatasetLog ) then
				FBeforePrepareDatasetLog( ( Owner as TKCustomDatasetAuditor ), Self, TDataset( Data ) );
		leAfterPrepareDatasetLog:
			if Assigned( FAfterPrepareDatasetLog ) then
				FAfterPrepareDatasetLog( ( Owner as TKCustomDatasetAuditor ), Self, TDataset( Data ) );
		leBeforeDatasetLog:
			if Assigned( FBeforeDatasetLog ) then
				FBeforeDatasetLog( ( Owner as TKCustomDatasetAuditor ), Self, TStrings( Data ) );
		leAfterDatasetLog:
			if Assigned( FAfterDatasetLog ) then
				FAfterDatasetLog( ( Owner as TKCustomDatasetAuditor ), Self, TStrings( Data ) );
	else
		inherited DoLinkEvent( LinkEvent, Data );
	end;
end;

{----------------- Dataset Audtiory Actions Aggregate Object -------------------}

constructor TKAggregateDatasetActions.Create( AOwner: TDataset );
begin
	inherited Create;
	FOwner := AOwner;
end;

procedure TKAggregateDatasetActions.Assign( Source: TPersistent );
begin
{
	Derived KnowHow Dataset can implement the assign mechanism indirectly by overriding the
	AssignTo method and make the assign work!
}
	if CheckObjectClass( Source, TKAggregateDatasetActions ) then
		DSActions := TKAggregateDatasetActions( Source ).DSActions
	else
		inherited Assign( Source );
end;

function TKAggregateDatasetActions.GetDatasetActionsAsString: string;
var
	i: TKDatasetAction;
begin
	Result := '[';
	for i := Low( TKDatasetAction ) to High( TKDatasetAction ) do
		if ( i in DSActions ) then
			Result := Result + dsaNames[i] + ', ';
	if ( Length( Result ) > 1 ) then
		Delete( Result, Length( Result ) - 1, 2 );
	Result := Result + ']';
end;

procedure TKAggregateDatasetActions.SetDatasetActionsAsString( const Value: string );
var
	i: Integer;
	sl: TStrings;
	iSet: TKDatasetActions;
begin
	if ( Length( Value ) > 1 ) and ( Value[1] = '[' ) and ( Value[Length( Value )] = ']' ) then
		RaiseExceptionFmt( EKDatasetAuditory, sErrDSAInvValue, [Value] );
	sl := TStringList.Create;
	try
		iSet := [];
		ExtractStrings( Copy( Value, 2, Length( Value ) - 2 ), ',', sl );
		for i := 0 to sl.Count - 1 do
			Include( iSet, StringToKDatasetAction( Trim( sl[i] ) ) );
		if ( iSet <> DSActions ) then
			DSActions := iSet;
	finally
		sl.Free;
	end;
end;

procedure TKAggregateDatasetActions.WriteData( Writer: TWriter );
var
	i: TKDatasetAction;
begin
	with TWriterHack( Writer ) do
	begin
{ Writer mechanism for set properties streaming. See TWriter.WriteProperty }
		WriteValue( vaSet );
		for i:= Low( TKDatasetAction ) to High( TKDatasetAction ) do
			if ( i in DSActions ) then
				WriteStr( dsaNames[i] );
		WriteStr( '' );
	end;
end;

procedure TKAggregateDatasetActions.ReadData( Reader: TReader );

	procedure SkipSetBody;
	begin
		while CheckStr( Reader.ReadStr ) do ;
	end;

var
	EnumName: string;
	iSet: TKDatasetActions;
begin
{ Copied ( adapted ) from TReader.ReadSet private method }
	try
		if ( Reader.ReadValue <> vaSet ) then
			RaiseException( EKDatasetAuditory, SInvalidPropertyValue );
		iSet := [];
		while True do
		begin
			EnumName := Reader.ReadStr;
			if ( not CheckStr( EnumName ) ) then
				Break;
			Include( iSet, StringToKDatasetAction( EnumName ) );
		end;
		DSActions := iSet;
	except
		SkipSetBody;
		raise;
	end;
end;

procedure TKAggregateDatasetActions.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKAggregateDatasetActions ) then
				Result := ( DSActions <> TKAggregateDatasetActions( Filer.Ancestor ).DSActions );
		end
		else
			Result := ( DSActions <> dsaDefActions );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'DatasetActions', ReadData, WriteData, DoWrite );
end;

{---------------------------- TKCustomDatasetAuditor ---------------------------}

const
	DSL_DATA_ADDINFO_INDEX = 0;

constructor TKCustomDatasetAuditor.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FLogRecords := TStringList.Create;
end;

destructor TKCustomDatasetAuditor.Destroy;
begin
	FLogRecords.Free;
	inherited Destroy;
end;

function TKCustomDatasetAuditor.WndProc( var Message: TMessage ): Boolean;
begin
	Result := inherited WndProc( Message );
	if Enabled then
		case Message.Msg of
			KA_DATASET:
			begin
				Dispatch( Message );
{ Just leave other auditor to process this message if the user doesn't log it }				
				Result := ( Message.Result = 1 );
			end;
		end;
end;

procedure TKCustomDatasetAuditor.KADataset( var Message: TKADataset );
begin
	inherited;
{ Anyone knows about that message... }	
	Message.Result := 0;
	if CheckPointer( Message.KDSAuditInfo ) then
		with Message, PKDSAuditory( KDSAuditInfo )^ do
		begin
			DoAuditory( AuditEvent, LongInt( Dataset ), LongInt( KDSAuditInfo ) );
{ The Message was handled by someone... }
			Result := Integer( Handled );
		end;
end;

function TKCustomDatasetAuditor.GetDatasetLogError: TKDatasetLogErrorEvent;
begin
	Result := DatasetLog.OnDatasetLogError;
end;

procedure TKCustomDatasetAuditor.SetDatasetLogError( Value: TKDatasetLogErrorEvent );
begin
	DatasetLog.OnDatasetLogError := Value;
end;

procedure TKCustomDatasetAuditor.DoRequestFileName( Data: Pointer );
var
	sFileName: string;
	dFileDate: TDateTime;
	iFileSize: LongInt;
begin
	inherited DoRequestFileName( Data );
	if Assigned( FKDSRequestFileName ) then
	begin
		sFileName := FileName;
		if CheckFile( sFileName ) then
		begin
			dFileDate := GetFileDateCreated( sFileName );
			iFileSize := GetFileSize( sFileName, nil );
		end
		else
		begin
			dFileDate := Now;
			iFileSize := 0;
		end;
		FKDSRequestFileName( Self, TDataset( Data ){or FDataset...}, sFileName, dFileDate,
			iFileSize );
		if CheckTrimStr( sFileName ) then
			FileName := sFileName;
	end;
end;

function TKCustomDatasetAuditor.GetDataSet: TDataSet;
begin
	Result := FDataSet;
end;

procedure TKCustomDatasetAuditor.VerifyAndAdjustCustomFields;
var
	sl: TStrings;
	sNameIdx: string;
begin
	if ( not CheckStrings( FLogRecords ) ) then
	begin
{ just to ensure any user error! }
		sl := DatasetLog.CreateEmptyLogRecord;
		try
			FLogRecords.Assign( sl );
		finally
			sl.Free;
		end;
	end;
	sNameIdx := FLogRecords.Names[DSL_DATA_ADDINFO_INDEX];
	if ( not CheckTrimStr( FLogRecords.Values[sNameIdx] ) ) then
		FLogRecords.Values[sNameIdx] := DSL_DATASET_NOCUSTOMINFO;
	FLogRecords.Values[sNameIdx] := StringReplace( FLogRecords.Values[sNameIdx],
		CH_CRLF, CH_CRLF + CH_TAB, krfAll );
{
	Derived classes can override this method to implement verification into
	custom fields defined there
}
end;

procedure TKCustomDatasetAuditor.DoAuditory( Event: TKAuditEvent; WParam, LParam: LongInt );
var
	sl: TStrings;
	bLogData: Boolean;
begin
	ForceObject( TDataSet( WParam ) );
	case Event of
		aeNotifyData:
		begin
			FDataSet := TDataSet( WParam );
			try
{ If the dataset is curently associated with the auditor implementation, then proceed }
				if CheckObjectClass( FDataSet, GetAuditoryDataSetClass ) then
				begin
					sl := DatasetLog.CreateEmptyLogRecord;
					try
						InternalAnnounceDataSetLog( sl );
						FLogRecords.Assign( sl );
{
	Independent of the user choice, we doesn't propagete this message to other auditor.
	In other words, the final user (application) says: "don't log it, please", with this
	iformation, the message was properly handled but not logged.
}
						PKDSAuditory( LParam )^.Handled := True;
					finally
						sl.Free;
					end;
				end;
			finally
				FDataset := nil;
			end;
		end;
		aeLogData:
		begin
{
	If TKTableAuditor creates a new Custom defined field, you can use a link to set
	it or just override this method.
}
			FDataSet := TDataSet( WParam );
			try
{ If the dataset is curently associated with the auditor implementation, then proceed }			
				if CheckObjectClass( FDataSet, GetAuditoryDataSetClass ) then
				begin
					NotifyLinks( leBeforePrepareDatasetLog, LongInt( FDataSet ) );
					bLogData := InternalPrepareDataSetLog;
					VerifyAndAdjustCustomFields;
					NotifyLinks( leAfterPrepareDatasetLog, LongInt( FDataSet ) );
					if bLogData then
					begin
						NotifyLinks( leBeforeDatasetLog, LongInt( FLogRecords ) );
						DatasetLog.LogData( FLogRecords, FDataset );
						NotifyLinks( leAfterDatasetLog, LongInt( FLogRecords ) );
					end;
{
	Independent of the user choice, we doesn't propagete this message to other auditor.
	In other words, the final user (application) says: "don't log it, please", with this
	iformation, the message was properly handled but not logged.
}
					PKDSAuditory( LParam )^.Handled := True;
				end;
			finally
				FDataset := nil;
			end;
		end;
	else
		inherited DoAuditory( Event, WParam, LParam );
	end;
end;

function TKCustomDatasetAuditor.GetAuditoryLog: TKCustomDatasetLog;
begin
	Result := TKCustomDatasetLog( inherited GetAuditoryLog );
end;

{----------------------- TKCustomAggregateDatasetAuditor -----------------------}

constructor TKCustomAggregateDatasetAuditor.Create( AClass: TKCustomAuditableClass );
begin
	ForceClassReference( AClass, TKCustomDatasetAuditor );
	inherited Create( AClass );
end;

function TKCustomAggregateDatasetAuditor.GetDatasetAuditor: TKCustomDatasetAuditor;
begin
	Result := TKCustomDatasetAuditor( inherited GetAuditoryOwner );
end;

{------------------------------- TKTableAuditor --------------------------------}

function TKTableAuditor.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKTableLog;
end;

function TKTableAuditor.GetAuditoryDataSetClass: TDataSetClass; 
begin
	Result := TKTable;
end;

function TKTableAuditor.InternalPrepareDataSetLog: Boolean;
begin
{ Make sure that the dataset was assigned }
	Result := True;
	if Assigned( ( DataSet as TKTable ).AfterDatasetAction ) and
		 ( not ( csDestroying in DataSet.ComponentState ) ) then
		( DataSet as TKTable ).AfterDatasetAction( Self, DataSet, Result, LogRecords );
end;

procedure TKTableAuditor.InternalAnnounceDataSetLog( sl: TStrings );
begin
{ Make sure that the dataset was assigned }
	if Assigned( ( DataSet as TKTable ).BeforeDatasetAction ) and
		( not ( csDestroying in DataSet.ComponentState ) ) then
		( DataSet as TKTable ).BeforeDatasetAction( Self, DataSet, sl );
end;

{------------------------------- TKQueryAuditor --------------------------------}

function TKQueryAuditor.GetAuditoryLogClass: TKCustomAuditoryLogClass;
begin
	Result := TKQueryLog;
end;

function TKQueryAuditor.GetAuditoryDataSetClass: TDataSetClass;
begin
	Result := TKQuery;
end;

procedure TKQueryAuditor.InternalAnnounceDataSetLog( sl: TStrings );
begin
{ Make sure that the dataset was assigned }
	if Assigned( ( DataSet as TKQuery ).BeforeDatasetAction ) and
		( not ( csDestroying in DataSet.ComponentState ) ) then
		( DataSet as TKQuery ).BeforeDatasetAction( Self, DataSet, sl );
end;

function TKQueryAuditor.InternalPrepareDataSetLog: Boolean;
begin
{ Make sure that the dataset was assigned }
	Result := True;
	if Assigned( ( DataSet as TKQuery ).AfterDatasetAction ) and
		 ( not ( csDestroying in DataSet.ComponentState ) ) then
		( DataSet as TKQuery ).AfterDatasetAction( Self, DataSet, Result, LogRecords );
end;

{ TKCustomDBLogEngine }

constructor TKCustomDBLogEngine.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FCloseDataBase := True;
  FTransactionSafe := True;
  FQueryValidated := False;
  FLogEngineValidated := False;
  FQuery := TQuery.Create( nil );
  FSQLFields := TStringList.Create;
  FSQLParams := TStringList.Create;
end;

destructor TKCustomDBLogEngine.Destroy;
begin
  FQuery.Free;
  FSQLFields.Free;
  FSQLParams.Free;
  inherited Destroy;
end;

procedure TKCustomDBLogEngine.SetDatabase( Value: TDataBase );
begin
  FDataBase := Value;
  FQueryValidated := False;
  if CheckObject( Value ) then
  begin
    Value.FreeNotification( Self );
    FQuery.DataBaseName := FDataBase.DatabaseName;
  end
  else
    FQuery.DataBaseName := '';
end;

procedure TKCustomDBLogEngine.SetTableName( const Value: string );
begin
  if ( not CheckStrEqual( Value, FTableName ) ) then
  begin
    FTableName := Value;
    FQueryValidated := False;
  end;
end;

procedure TKCustomDBLogEngine.SetLogEngine( Value: TKCustomLogEngine );
begin
  FLogEngine := Value;
  FLogEngineValidated := False;
  if CheckObject( Value ) then
    Value.FreeNotification( Self );
end;

function TKCustomDBLogEngine.DoCreateTable( var SQL: string ): Boolean;
begin
  Result := True;
  if Assigned( FOnCreateTable ) then
    FOnCreateTable( Self, LogEngine, SQL, Result );
end;

function TKCustomDBLogEngine.DoInsertRecords( var SQL: string ): Boolean;
begin
  Result := True;
  if Assigned( FOnInsertRecords ) then
    FOnInsertRecords( Self, LogEngine, SQL, Result );
end;

function TKCustomDBLogEngine.DoFillParams( RecordNo: Integer; Params: TStrings ): Boolean;
begin
  Result := True;
  if Assigned( FOnFillParams ) then
    FOnFillParams( Self, FQuery, RecordNo, Params, Result );
end;

function TKCustomDBLogEngine.DoSelectFields( Fields: TStrings ): Boolean;
begin
  Result := True;
  if Assigned( FOnSelectFields ) then
    FOnSelectFields( Self, Fields, Result );
end;

procedure TKCustomDBLogEngine.DoAbort;
begin
  if Assigned( FOnAbort ) then
    FOnAbort( Self );
  Abort;  
end;

type
  TKCustomLogEngineHack = class( TKCustomLogEngine );

procedure TKCustomDBLogEngine.ValidateQuery;
begin
{
  We remove this strict checking for performance sake and because of DB Name
  refreshing problems. The only penalty about this is the lost of the detailed
  exception message. Now we will have one exception per database error!
}
  if ( ( not ( CheckObject( FDataBase ) and FDataBase.Connected and
    CheckTrimStrs( [FQuery.DataBaseName, FTableName] ) ) ) { or
    ( FCreatingDBLog and CheckTableName( FQuery.DatabaseName, TableName ) ) or
    ( FInsertingDBLog and ( not CheckTableName( FQuery.DataBaseName, TableName ) ) ) } ) then
    RaiseException( EKDBLogEngine, sErrDBLEInvQuery );
  FQueryValidated := True;
end;

procedure TKCustomDBLogEngine.ValidateLogEngine;
begin
  if ( ( not CheckObject( FLogEngine ) ) or ( TKCustomLogEngineHack( FLogEngine ).FieldCount <= 0 ) or
    ( TKCustomLogEngineHack( FLogEngine ).RecordCount <= 0 ) ) then
    RaiseException( EKDBLogEngine, sErrDBLEInvLogEngine );
  FLogEngineValidated := True;
end;

procedure TKCustomDBLogEngine.ValidateFieldParamsValues;
begin
  if ( not ( FQueryValidated and FLogEngineValidated and CheckStrings( FSQLFields ) and
    ( FSQLFields.Count = FSQLParams.Count ) and ( FSQLParams.Count = FQuery.Params.Count ) ) ) then
    RaiseException( EKDBLogEngine, sErrDBLEInvFieldParams );
end;

procedure TKCustomDBLogEngine.GetLogFields;
var
  i: Integer;
begin
  if ( not FLogEngineValidated ) then
    ValidateLogEngine;
  FSQLFields.Clear;
  with TKCustomLogEngineHack( FLogEngine ) do
  begin
  { Fixed Fields Reporting }
    for i := 0 to FixedFieldCount - 1 do
      FSQLFields.AddObject( 'F' + CH_UNDERSCORE + LogFields[i], TObject( LogFieldsSize[i] ) );
  { Virtual Fields Reporting }
    for i := 0 to VirtualFieldCount - 1 do
      FSQLFields.AddObject( 'V' + CH_UNDERSCORE + VirtualFields[i], TObject( VirtualFieldsSize[i] ) );
  { Custom Fields Reporting }
    for i := FixedFieldCount to FieldCount - 1 do
      FSQLFields.AddObject( 'C' + CH_UNDERSCORE + LogFields[i], TObject( LogFieldsSize[i] ) );
  end;
  if ( not ( DoSelectFields( FSQLFields ) and CheckStrings( FSQLFields ) ) ) then
    DoAbort;
end;

procedure TKCustomDBLogEngine.GetInsertSQL( var sFields, sParams: string );
var
  s: string;
  i, j: Integer;
begin
  s := '';
  sFields := '';
  sParams := '';
  FSQLParams.Clear;
{ for each field create the parameter names }
  if FDataBase.IsSQLBased then
    for i := 0 to FSQLFields.Count - 1 do
    begin
      s := FSQLFields[i];
      j := FSQLParams.AddObject( CH_COLON + 'P' + CH_UNDERSCORE + s + CH_EQUAL_TOKEN,
        FSQLFields.Objects[i] );
      sParams := sParams + FSQLParams.Names[j] + CH_COMMA + CH_SPACE;
      sFields := sFields + CH_DBLQUOTE + s + CH_DBLQUOTE + CH_COMMA + CH_SPACE;
    end
  else
    for i := 0 to FSQLFields.Count - 1 do
    begin
      s := FSQLFields[i];
      j := FSQLParams.AddObject( CH_COLON + 'P' + CH_UNDERSCORE + s + CH_EQUAL_TOKEN,
        FSQLFields.Objects[i] );
      sParams := sParams + FSQLParams.Names[j] + CH_COMMA + CH_SPACE;
      sFields := sFields + CH_DBLQUOTE + FTableName + CH_DBLQUOTE + CH_DOTMARK +
        CH_DBLQUOTE + s + CH_DBLQUOTE + CH_COMMA + CH_SPACE;
    end;
  if CheckStr( sFields ) then
    Delete( sFields, Length( sFields ) - 1, 2 );
  if CheckStr( sParams ) then
    Delete( sParams, Length( sParams ) - 1, 2 );
end;

procedure TKCustomDBLogEngine.GetSQLParamsValues( RecNo: Integer );
var
  ch: Char;
  pc: PChar;
  s: string;
  i: Integer;
begin
  with TKCustomLogEngineHack( FLogEngine ) do
  begin
    for i := 0 to FSQLFields.Count - 1 do
    begin
      pc := PChar( FSQLFields[i] );
      ch := pc^;
      Inc( pc, 2 );
      case ch of                          
        'C',
        'F':
        begin
          s := Trim( LogRecords[pc, RecNo] );
          { to avoid the bad TStrings.SetValue beheviour: delete string when value = '' }
          if CheckStr( s ) then
            FSQLParams.Values[FSQLParams.Names[i]] := s;
        end;
        'V':
        begin
          s := Trim( VirtualRecords[pc, RecNo] );
          { to avoid the bad TStrings.SetValue beheviour: delete string when value = '' }
          if CheckStr( s ) then
            FSQLParams.Values[FSQLParams.Names[i]] := s;
        end;    
      end;
    end;
  end;  
end;

function TKCustomDBLogEngine.CreateDDLSQLFields: string;
var
  i, j: Integer;
begin
  Result := '';
  if ( not CheckStrings( FSQLFields ) ) then
    GetLogFields;
{ Build the fields list }
  if FDataBase.IsSQLBased then
    for i := 0 to FSQLFields.Count - 1 do
    begin
      j := Integer( FSQLFields.Objects[i] );
      if ( j <= High( Byte ) ) then
        Result := Result + Format( sDBLogEngineCrtTblFieldsSQL, [FSQLFields[i], j] )
      else if FDatabase.IsSQLBased then
        Result := Result + Format( sDBLogEngineCrtTblTextFieldsSQL, [FSQLFields[i]] )
    end
  else
    for i := 0 to FSQLFields.Count - 1 do
    begin
      j := Integer( FSQLFields.Objects[i] );
      if ( j <= High( Byte ) ) then
        Result := Result + Format( sDBLogEngineCrtTblFieldsLocal, [FTableName, FSQLFields[i], j] )
      else
        Result := Result + Format( sDBLogEngineCrtTblBlobFieldsLocal, [FTableName, FSQLFields[i]] );
    end;
{ Build the final DDL SQL }        
  if CheckStr( Result ) then
    Delete( Result, Length( Result ) - 3, 4 );
end;

procedure TKCustomDBLogEngine.CreateTable( sSQL: string );
begin
  if ( DoCreateTable( sSQL ) and CheckStr( sSQL ) ) then
  begin
    if ( not FQueryValidated ) then
      ValidateQuery;
    FQuery.SQL.Clear;
    FQuery.SQL.Text := sSQL;
    FQuery.ExecSQL;
    FQuery.Close;
    FDatabase.FlushSchemaCache( TableName );
  end
  else
    DoAbort;
end;

procedure TKCustomDBLogEngine.CreateInsertSQL;
var
  i: Integer;
  sSQL,
  sFields,
  sParams: string;
begin
  if ( not CheckStrings( FSQLFields ) ) then
    GetLogFields;
  GetInsertSQL( sFields, sParams );
  sSQL := Format( sDBLogEngineInsertIntoTbl, [FTableName, sFields, sParams] );
  if ( DoInsertRecords( sSQL ) and CheckTrimStr( sSQL ) ) then
  begin
    if ( not FQueryValidated ) then
      ValidateQuery;
    FQuery.Close;
    FQuery.SQL.Clear;
    FQuery.Params.Clear;
    FQuery.SQL.Text := sSQL;
    for i := 0 to FQuery.Params.Count - 1 do
    begin
      FQuery.Params[i].ParamType := ptInput;
      FQuery.Params[i].DataType := ftString;
    end;
    FQuery.Prepare;
  end
  else
    DoAbort;
end;

procedure TKCustomDBLogEngine.InsertTableRecords;
var
  i, j: Integer;
begin
  if ( not FLogEngineValidated ) then
    ValidateLogEngine;
  if ( not FQueryValidated ) then
    ValidateQuery;
  with TKCustomLogEngineHack( FLogEngine ) do
    for i := 0 to RecordCount - 1 do
    begin
      GetSQLParamsValues( i );
      if ( DoFillParams( i, FSQLParams ) and CheckStrings( FSQLParams ) ) then
      begin
        ValidateFieldParamsValues;
        for j := 0 to FQuery.Params.Count - 1 do
          FQuery.Params.Items[j].AsString := FSQLParams.Values[FSQLParams.Names[j]];
        FQuery.ExecSQL;
      end
      else
        DoAbort;
    end;
end;

procedure TKCustomDBLogEngine.CleanUp;
begin
  FQuery.Close;
  if CloseDataBase then
    FDataBase.Close;
  FSQLFields.Clear;
  FSQLParams.Clear;
  FQueryValidated := False;
  FLogEngineValidated := False;
end;

procedure TKCustomDBLogEngine.DropTable;
begin
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Text := Format( sDBLogEngineDropTable, [FTableName] );
  FQuery.ExecSQL;
  CleanUp;
end;

procedure TKCustomDBLogEngine.InitEngine;
begin
  CleanUp;
  ValidateLogEngine;
  with TKCustomLogEngineHack( FLogEngine ) do
    FTableName := FixStringCharSet( GetFirstString( [FTableName, ChangeFileExt(
      FileName, '' )] ), CHARSET_ALPHANUM + [CH_DOTMARK, CH_DBLQUOTE, CH_QUOTE],
      CH_UNDERSCORE );
  if ( CheckStr( FTableName ) and ( FTableName[1] = CH_DBLQUOTE ) and
    ( FTableName[Length( FTableName )] = CH_DBLQUOTE ) ) then
  begin
    { To avoid names with "dbo"."teste" to become ""dbo"."teste""! }
    Delete( FTableName, 1, 1 );
    Delete( FTableName, Length( FTableName ), 1 );
  end;
  FDataBase.Open;
  ValidateQuery;
end;

procedure TKCustomDBLogEngine.CreateDBLog;
begin
  FCreatingDBLog := True;
  try
  { Init Engine }
    InitEngine;
  { Creating the SQL Table }
    CreateTable( Format( sDBLogEngineCreateTable, [FTableName, CreateDDLSQLFields] ) );
    try
    { Creating Insert SQL }
      CreateInsertSQL;
    { Insert Table Records }
      InsertTableRecords;
    { CleanUp Internal Props }
      CleanUp;
    except
      on E: Exception do
        if CheckObjectClasses( E, [EDataBaseError, EKnowHow] ) then
        begin
        { Drop table if there was any problem }
          DropTable;
          raise;
        end;
    end;
  finally
    FCreatingDBLog := False;
  end;
end;

procedure TKCustomDBLogEngine.InsertDBLog;
var
  isolLevel: TTransIsolation;
begin
  FInsertingDBLog := True;
  try
    InitEngine;
    isolLevel := FDataBase.TransIsolation;
    try
      { transaction safe }
      if FTransactionSafe then
      begin
        if ( not FDataBase.IsSQLBased ) then
          FDataBase.TransIsolation := tiDirtyRead;
        FDataBase.StartTransaction;
      end;
      try
      { Creating Insert SQL }
        CreateInsertSQL;
      { Insert Table Records }
        InsertTableRecords;
      { CleanUp Internal Props }
        CleanUp;
      { Commit transatcion }
        if FDataBase.InTransaction then
          FDataBase.Commit;
      except
        on E: Exception do
          if CheckObjectClasses( E, [EDataBaseError, EKnowHow] ) then
          begin
          { Rollback transaction and clean up everything if there was any problem }
            if FDataBase.InTransaction then
              FDataBase.Rollback;
            CleanUp;
            raise;
          end;
      end;
    finally
      { transaction safe }
      if ( FTransactionSafe and ( not ( FDatabase.InTransaction or
        FDataBase.IsSQLBased ) ) ) then
        FDataBase.TransIsolation := isolLevel;
    end;
  finally
    FInsertingDBLog := False;
  end;
end;

{ TKDBLog }

constructor TKDBLog.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKDBLogs );
	inherited Create( ACollection );
	ForceObject( Owner.Component );
  FCloseDataBase := True;
  FTransactionSafe := True;
  FDBLogType := dbltCreateInsert;
  FDBLogFileAction := dblfaDelete;
  FEngineOwnerShip := oosOwned;
end;

destructor TKDBLog.Destroy;
begin
  FreeEngine;
  inherited Destroy;
end;

procedure TKDBLog.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKDBLog ) then
  begin
    FileName := TKDBLog( Source ).FileName;
    TableName := TKDBLog( Source ).TableName;
    CloseDataBase := TKDBLog( Source ).CloseDataBase;
    TransactionSafe := TKDBLog( Source ).TransactionSafe;
    DBLogType := TKDBLog( Source ).DBLogType;
    DBLogFileAction := TKDBLog( Source ).DBLogFileAction;
  end;
end;

procedure TKDBLog.FreeEngine;
begin
  if ( FEngineOwnerShip = oosOwned ) then
    FLogEngine.Free;
  FLogEngine := nil;
end;

function TKDBLog.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
  Result := ( ( inherited Equals( Item ) ) and ( CloseDataBase = TKDBLog( Item ).CloseDataBase ) and
    ( TransactionSafe = TKDBLog( Item ).TransactionSafe ) and ( DBLogType =
    TKDBLog( Item ).DBLogType ) and CheckStrEqual( FFileName, TKDBLog( Item ).FileName ) and
    CheckStrEqual( FTableName, TKDBLog( Item ).TableName ) and ( DBLogFileAction =
    TKDBLog( Item ).DBLogFileAction ) and ( ( not CheckObjects( [FLogEngine, TKDBLog( Item ).LogEngine] ) ) or
    ( CheckObjects( [FLogEngine, TKDBLog( Item ).LogEngine] ) and ( FLogEngine.ClassType =
    TKDBLog( Item ).LogEngine.ClassType ) ) ) );
end;

function TKDBLog.GetQuery: TQuery;
begin
  Result := Owner.Component.DBLogEngine.Query;
end;

function TKDBLog.GetOwnerCollection: TKDBLogs;
begin
  Result := TKDBLogs( inherited GetOwnerCollection );
end;

procedure TKDBLog.SetTableName( const Value: string );
begin
  if ( not CheckStrEqual( FTableName, Value ) ) then
    FTableName := Value;
end;

procedure TKDBLog.SetFileName( const Value: string );
begin
  if ( not CheckStrEqual( FFileName, Value ) ) then
  begin
    FFileName := Value;
    FreeEngine;
  end;
end;

procedure TKDBLog.DoAbort;
begin
  if Assigned( FOnAbort ) then
    FOnAbort( Self );
end;

procedure TKDBLog.DoAfterExecute;
begin
	if Assigned( FAfterExecute ) then
		FAfterExecute( Self );
end;

procedure TKDBLog.DoBeforeExecute;
begin
	if Assigned( FBeforeExecute ) then
		FBeforeExecute( Self );
end;

function TKDBLog.DoCreateTable( var SQL: string ): Boolean;
begin
  Result := True;
  if Assigned( FOnCreateTable ) then
    FOnCreateTable( Owner.Component, Self, LogEngine, SQL, Result );
end;

function TKDBLog.DoInsertRecords( var SQL: string ): Boolean;
begin
  Result := True;
  if Assigned( FOnInsertRecords ) then
    FOnInsertRecords( Owner.Component, Self, LogEngine, SQL, Result );
end;

function TKDBLog.DoFillParams( RecordNo: Integer; Query: TQuery; Params: TStrings ): Boolean;
begin
  Result := True;
  if Assigned( FOnFillParams ) then
    FOnFillParams( Owner.Component, Self, Query, RecordNo, Params, Result );
end;

function TKDBLog.DoSelectFields( Fields: TStrings ): Boolean;
begin
  Result := True;
  if Assigned( FOnSelectFields ) then
    FOnSelectFields( Owner.Component, Self, Fields, Result );
end;

function TKDBLog.DoGroupAbort: Boolean;
begin
  Result := True;
	if Assigned( Owner.Component.OnGroupAbort ) then
		Owner.Component.OnGroupAbort( Owner.Component, Self, GroupIndex, Result );
end;

function TKDBLog.DoLogAbortType( E: Exception ): TKDBLogAbortType;
begin
  Result := dblatIgnore;
  if Assigned( Owner.Component.OnLogAbort ) then
    Owner.Component.OnLogAbort( Owner.Component, E, Self, Result );
end;

function TKDBLog.DoGetLogEngine: TKCustomLogEngine;
begin
  FreeEngine;
  if Assigned( FOnGetLogEngine ) then
    FOnGetLogEngine( Owner.Component, Self, FLogEngine );
  Result := FLogEngine;
  ForceObject( Result );
end;

function TKDBLog.Execute: Boolean;
begin
  Result := False;
  DoBeforeExecute;
  repeat
    try
      Owner.FLastDBLog := Self;
      with Owner.Component do
      begin
        DBLogEngine.LogEngine := DoGetLogEngine;
        DBLogEngine.TableName := TableName;
        DBLogEngine.CloseDataBase := CloseDataBase;
        DBLogEngine.TransactionSafe := TransactionSafe;
        case DBLogType of
          dbltCreateInsert: DBLogEngine.CreateDBLog;
          dbltInsert      : DBLogEngine.InsertDBLog;
        end;
        case DBLogFileAction of
          dblfaDelete: ForceDeleteFile( FFileName );
          dblfaClear : DBLogEngine.LogEngine.Clear( lctEntries );
          dblfaNone  : { do nothing };
        end;
      end;
      Result := True;
    except
      on E: Exception do
        if CheckObjectClasses( E, [EDataBaseError, EKnowHow, EAbort] ) then
        begin
          FLogAbortType := DoLogAbortType( E );
          case FLogAbortType of
            dblatIgnore    : Result := True;
            dbltRetry      : { do nothing };
            dblatAbortGroup:
            begin
            	Result := ( GroupIndex = DBLOG_GROUP_NULL );
              if ( not Result ) then
              begin
                Result := ( not DoGroupAbort );
                if ( not Result ) then
                  MarkGroupAborted;
              end;
            end;
            dblatAbortAll  : RaiseExceptionFmt( EKDBLogManager, sErrInvDBLogExecute, [Name] );
          end;
        end
        else
          raise;
    end;
  until Result;
  if Result then
    DoAfterExecute;
end;

{ TKDBLogs }

constructor TKDBLogs.Create( AComp: TKDBLogManager );
begin
  ForceObject( AComp );
	inherited Create( AComp, TKDBLog, False ); 
end;

function TKDBLogs.Add: TKDBLog;
begin
  Result := TKDBLog( inherited Add ); 
end;

function TKDBLogs.GetItem( Index: Integer ): TKDBLog;
begin
  Result := TKDBLog( inherited GetItem( Index ) );
end; 

function TKDBLogs.GetItemByName( const AName: string ): TKDBLog;
begin
	Result := TKDBLog( inherited GetItemByName( AName ) ); 
end;

function TKDBLogs.GetOwnerComp: TKDBLogManager;
begin
  Result := TKDBLogManager( inherited GetOwnerComp ); 
end;

procedure TKDBLogs.SetItem( Index: Integer; AItem: TKDBLog );
begin
  inherited SetItem( Index, AItem );
end;

procedure TKDBLogs.Update( Item: TCollectionItem );
var
  i: Integer;
begin
  if CheckObject( Item ) then
    Component.UpdateItem( TKDBLog( Item ) )
  else
    for i := 0 to Count - 1 do
      Component.UpdateItem( Items[i] );
end;

procedure TKDBLogs.CreateTableEvent( Sender: TKCustomDBLogEngine; LogEngine: TKCustomLogEngine;
  var SQL: String; var Continue: Boolean );
begin
  Continue := TKDBLog( Sender.Data ).DoCreateTable( SQL );
end;

procedure TKDBLogs.InsertRecordsEvent( Sender: TKCustomDBLogEngine; LogEngine: TKCustomLogEngine;
  var SQL: String; var Continue: Boolean );
begin
  Continue := TKDBLog( Sender.Data ).DoInsertRecords( SQL );
end;

procedure TKDBLogs.FillParamsEvent( Sender: TKCustomDBLogEngine; Query: TQuery;
  const RecordNo: Integer; Params: TStrings; var Continue: Boolean );
begin
  Continue := TKDBLog( Sender.Data ).DoFillParams( RecordNo, Query, Params );
end;

procedure TKDBLogs.SelectFieldsEvent( Sender: TKCustomDBLogEngine; Fields: TStrings;
  var Continue: Boolean );
begin
  Continue := TKDBLog( Sender.Data ).DoSelectFields( Fields );
end;

procedure TKDBLogs.AbortEvent( Sender: TObject );
begin
  TKDBLog( TKCustomDBLogEngine( Sender ).Data ).DoAbort;
end;

function TKDBLogs.GetAttrCount: Integer;
begin
	Result := DBLOG_ITEMS_ATTRCOUNT;
end;

function TKDBLogs.GetAttr( Index: Integer ): string;
begin
	case Index of
		0: Result := COLLECTION_ITEM_INDEX;
		1: Result := COLLECTION_ITEM_NAME;
		2: Result := COLLECTION_ITEM_GRPIDX;
	else
			Result := inherited GetAttr( Index );
	end;
end;

function TKDBLogs.GetItemAttr( Index, ItemIndex: Integer ): string;
begin
	case Index of
		0: Result := Format( '%-d', [Items[ItemIndex].ID] );
		1: Result := Items[ItemIndex].Name;
		2: Result := Format( '%-d', [Items[ItemIndex].GroupIndex] );
	else
		Result := inherited GetItemAttr( Index, ItemIndex );
	end;
end;

{ TKDBLogManagerLink }

const
	leBeforeExecAll   = 0;
	leBeforeExecGroup = 1;
	leBeforeExecItem  = 2;
	leAfterExecItem   = 3;
	leAfterExecGroup  = 4;
	leAfterExecAll    = 5;

procedure TKDBLogManagerLink.DoLinkEvent( LinkEvent: TLinkEvent; Data: LongInt );
begin
	case LinkEvent of
		leBeforeExecAll:
			if Assigned( FBeforeExecAll ) then
				FBeforeExecAll( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		leBeforeExecGroup:
			if Assigned( FBeforeExecGroup ) then
				FBeforeExecGroup( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		leBeforeExecItem:
			if Assigned( FBeforeExecItem ) then
				FBeforeExecItem( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		leAfterExecItem:
			if Assigned( FAfterExecItem ) then
				FAfterExecItem( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		leAfterExecGroup:
			if Assigned( FAfterExecGroup ) then
				FAfterExecGroup( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		leAfterExecAll:
			if Assigned( FAfterExecAll ) then
				FAfterExecAll( ( Owner as TKDBLogManager ), Self, Cardinal( Data ) );
		else
			inherited DoLinkEvent( LinkEvent, Data );
	end;
end;

{ TKDBLogManager }

constructor TKDBLogManager.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FDBLogs := TKDBLogs.Create( Self );
  FDBLogEngine := TKCustomDBLogEngine.Create( nil );
  FDBLogEngine.OnAbort := FDBLogs.AbortEvent;
  FDBLogEngine.OnCreateTable := FDBLogs.CreateTableEvent;
  FDBLogEngine.OnInsertRecords := FDBLogs.InsertRecordsEvent;
  FDBLogEngine.OnFillParams := FDBLogs.FillParamsEvent;
  FDBLogEngine.OnSelectFields := FDBLogs.SelectFieldsEvent;
end;

destructor TKDBLogManager.Destroy;
begin
  FreeClean( FDBLogs );
  FreeClean( FDBLogEngine );
  inherited Destroy;
end;

procedure TKDBLogManager.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKDBLogManager ) then
		with ( Source as TKDBLogManager ) do
		begin
			Self.DBLogs := DBLogs;
			Self.Database := Database;
			Self.OnLogAbort := OnLogAbort;
			Self.OnGroupAbort := OnGroupAbort;
			Self.Tag := Tag;
		end
	else
		inherited Assign( Source );
end;

procedure TKDBLogManager.SetDBLogs( Value: TKDBLogs );
begin
  FDBLogs.Assign( Value );
end;

function TKDBLogManager.GetDataBase: TDataBase;
begin
  Result := FDBLogEngine.Database;
end;

procedure TKDBLogManager.SetDatabase( Value: TDataBase );
begin
  FDBLogEngine.DataBase := Value;
  if CheckObject( Value ) then
    Value.FreeNotification( Self );
end;

procedure TKDBLogManager.UpdateItem( Item: TKDBLog );
begin
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

function TKDBLogManager.Execute: Boolean;
const
	START_GROUP_INDEX: array[Boolean] of Byte = ( DBLOG_GROUP_NULL, DBLOG_GROUP_NULL + 1 );
var
	i,
	j: Integer;
	GroupList: TList;
begin
  FQuitting := False;
	Result := CheckCollection( DBLogs );
	if Result then
	begin
  	DBLogs.ClearGroupAborted; { Hard Couple! }
    DBLogs.FLastDBLog := nil; { Hard Couple! }
 	  NotifyLinks( leBeforeExecAll, DBLogs.Count );
    GroupList := TList.Create;
    try
      for i := 0 to DBLogs.Count - 1 do
        if ( GroupList.IndexOf( Pointer( DBLogs.Items[i].GroupIndex ) ) = -1 ) then
          GroupList.Add( Pointer( DBLogs.Items[i].GroupIndex ) );
      GroupList.Sort( CompareProc );
{ If there is any NULL_GROUP item, start at group one..., otherwise start a zero! (groupidx 1) }
   		i := START_GROUP_INDEX[( Integer( GroupList.Items[0] ) = DBLOG_GROUP_NULL )];
      for i := i to GroupList.Count - 1 do
      begin
        if FQuitting then
          Exit;
        NotifyLinks( leBeforeExecGroup, i ); { Can reveive the new ItemIndex }
        for j := 0 to DBLogs.Count - 1 do
        begin
          if FQuitting then
            Exit;
{ DO NOT compile 0-based groups }
          if ( Cardinal( GroupList.Items[i] ) <> DBLOG_GROUP_NULL ) and
             ( Cardinal( GroupList.Items[i] ) = DBLogs.Items[j].GroupIndex ) then
          begin
            NotifyLinks( leBeforeExecItem, j );
{$BOOLEVAL ON}
            Result := ( Result and DBLogs.Items[j].Execute );
{$BOOLEVAL OFF}
            NotifyLinks( leAfterExecItem, j );
          end;
        end;
        NotifyLinks( leAfterExecGroup, i );
      end;
      { Late compiling of 0-based groups }
      if ( Cardinal( GroupList.Items[0] ) = DBLOG_GROUP_NULL ) then
      begin
        NotifyLinks( leBeforeExecGroup, DBLOG_GROUP_NULL );
        for j := 0 to DBLogs.Count - 1 do
          if ( DBLogs.Items[j].GroupIndex = DBLOG_GROUP_NULL ) then
          begin
            NotifyLinks( leBeforeExecItem, j );
{$BOOLEVAL ON}
            Result := ( Result and DBLogs.Items[j].Execute );
{$BOOLEVAL OFF}
            NotifyLinks( leAfterExecItem, j );
          end;
        NotifyLinks( leAfterExecGroup, DBLOG_GROUP_NULL );
      end;
    finally
      GroupList.Free;
      NotifyLinks( leAfterExecAll, 0 );
    end;
  end;
end;

function TKDBLogManager.ExecuteGroup( GroupIndex: Cardinal ): Boolean;

	function GetDBLogItems( GroupIndex: Cardinal ): Integer;
	var
		i: Integer;
	begin
		Result := 0;
		for i := 0 to DBLogs.Count - 1 do
			Inc( Result, Ord( DBLogs.Items[i].GroupIndex = GroupIndex ) );
	end;

var
	i: Integer;
begin
	FQuitting := False;
	i := GetDBLogItems( GroupIndex );
	Result := CheckCollection( DBLogs ) and ( i > 0 );
	if Result then
	begin
		DBLogs.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, i );
		try
			NotifyLinks( leBeforeExecGroup, GroupIndex ); { Can reveive the new ItemIndex }
			DBLogs.FLastDBLog := nil; { Hard Couple! }
			for i := 0 to DBLogs.Count - 1 do
			begin
				if FQuitting then
					Exit;
				if ( DBLogs.Items[i].GroupIndex = GroupIndex ) then
				begin
					NotifyLinks( leBeforeExecItem, i );
{$BOOLEVAL ON}
					Result := ( Result and DBLogs.Items[i].Execute );
{$BOOLEVAL OFF}
					NotifyLinks( leAfterExecItem, i );
				end;
			end;
			NotifyLinks( leAfterExecGroup, GroupIndex );
		finally
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

function TKDBLogManager.ExecuteItems( const Items: array of Cardinal ): Boolean;
var
	i: Integer;
begin
{
	Can use ArrayOfConstToVar...
	V := ArrayOfConstToVar( Items );
	Result := ExecuteItemsEx( V );
}
	FQuitting := False;
	Result := CheckCollection( DBLogs );
	if Result then
	begin
		DBLogs.ClearGroupAborted; { Hard Couple! }
		NotifyLinks( leBeforeExecAll, High( Items ) );
		try
			DBLogs.FLastDBLog := nil; { Hard Couple! }
			for i := Low( Items ) to High( Items ) do
			begin
				if FQuitting then
					Exit;
			{ Here we must TypeCast Items to Integer because in Delphi4 cardinal
				violates the Integer Range and generates a warning here. Even, CompileItems.Count
				also returns Integer ranged values... }
				if ( Integer( Items[i] ) >= DBLogs.Count ) then
					RaiseExceptionFmt( EKDBLogManager, sErrDBLOGInvArrayIndex, [Items[i]] );
				NotifyLinks( leBeforeExecItem, Items[i] );
{$BOOLEVAL ON}
				Result := ( Result and DBLogs.Items[i].Execute );
{$BOOLEVAL OFF}
				NotifyLinks( leAfterExecItem, Items[i] );
			end;
		finally
			NotifyLinks( leAfterExecAll, 0 );
		end;
	end;
end;

procedure TKDBLogManager.DoLoadError( Item: TKDBLog );
begin
	if Assigned( FOnLoadError ) then
		FOnLoadError( Self, Item, Item.GroupIndex );
end;

procedure TKDBLogManager.Quit;
begin
	FQuitting := True;
end;

procedure TKDBLogManager.MergeFromStream( Stream: TStream );
var
	DBLogManager: TKDBLogManager;
begin
	if CheckCollection( DBLogs ) then
	begin
		FIsLoading := True;
		try
			DBLogManager := TKDBLogManager.Create( nil );
			try
			  DBLogManager.OnLoadError := OnLoadError;
        DBLogManager.FIsLoading := True;
				Stream.ReadComponent( DBLogManager );
				Tag := DBLogManager.Tag;
        DataBase := DBLogManager.DataBase;
			  OnLogAbort := DBLogManager.OnLogAbort;
			  OnGroupAbort := DBLogManager.OnGroupAbort;
        DBLogs.AddItems( DBLogManager.DBLogs );
			finally
				DBLogManager.Free;
			end;
		finally
		  FIsLoading := False;
		end;
	end
	else
		LoadFromStream( Stream );
end;

procedure TKDBLogManager.LoadFromStream( Stream: TStream );
var
	DBLogManager: TKDBLogManager;
begin
{ We must explicity create a new instance an pass it to Stream because the Self
	assignment causes some naming reference problems on the DCC32 Owner }
	FIsLoading := True;
	try
		DBLogManager := TKDBLogManager.Create( nil );
		try
			DBLogManager.OnLoadError := OnLoadError;
			DBLogManager.FIsLoading := True;
			Stream.ReadComponent( DBLogManager );
			Assign( DBLogManager );
		finally
			DBLogManager.Free;
		end;
	finally
		FIsLoading := False;
	end;
end;

procedure TKDBLogManager.SaveToStream( Stream: TStream );
begin
	Stream.WriteComponent( Self );
end;

procedure TKDBLogManager.MergeFromFile( const FileName: string );
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

procedure TKDBLogManager.LoadFromFile( const FileName: string );
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

procedure TKDBLogManager.SaveToFile( const FileName: string );
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

{---------------------- General Dataset Actions Functions ----------------------}

function KDatasetActionToIdent( DSAction: Integer; var Ident: string ): Boolean;
begin
	Result := ( ValueBetWeen( DSAction, Low( TKDatasetAction ), High( TKDatasetAction ), True ) and
		IntToIdent( DSAction, Ident, dsaMapEntry ) );
end;

function IdentToKDatasetAction( const Ident: string; var DSAction: Integer ): Boolean;
begin
	Result := ( IdentToInt( Ident, DSAction, dsaMapEntry ) and
		ValueBetWeen( DSAction, Low( TKDatasetAction ), High( TKDatasetAction ), True ) );
end;

function KDatasetActionToString( const DSAction: TKDatasetAction ): string;
begin
	if ( not KDatasetActionToIdent( DSAction, Result ) ) then
		RaiseExceptionFmt( EKDatasetAuditory, sErrDSAInvValue, [DSAction] );
end;

function StringToKDatasetAction( const Source: string ): TKDatasetAction;
var
	DSAction: Integer;
begin
	if ( not IdentToKDatasetAction( Source, DSAction ) ) then
		RaiseExceptionFmt( EKDatasetAuditory, sErrDSAInvName, [Source] );
	Result := TKDatasetAction( DSAction );
end;

procedure ForceKDSActionByValue( const Value: TKDatasetAction );
begin
	KDatasetActionToString( Value );
end;

procedure ForceKDSActionByName( const Value: string );
begin
	StringToKDatasetAction( Value );
end;

procedure GetKDatasetActionValues( Proc: TGetStrProc );
var
	I: Integer;
begin
	for I := Low( dsaMapEntry ) to High( dsaMapEntry ) do
		 Proc( dsaMapEntry[I].Name );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
{ Integer Constants }
	RegisterIntegerConsts( TypeInfo( TKDataSetAction ), IdentToKDataSetAction, KDataSetActionToIdent );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
