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

unit ukdbScript;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, DB, DBTables, BDE, StdVCL, uksyUtils, uksyClasses, ukrClasses,
	ukdbConsts, ukdbClasses, ukdbTables, ukdbEngines;

type

	EKSQLScript = class( EKDB );
	
{
--------------------------------------------------------------------------------
------------------------------ SQL Script Classes ------------------------------
--------------------------------------------------------------------------------
}

	TKSQLScript = class;
	TKQueryCollection = class;
	TKQueryItem = class;

	TKExecMode = ( emOpen, emExecSQL );

{ TKPUpdateObject }

	TKPUpdateObject = class( TPersistent )
	private
	  FOwner: TKQueryItem;
		FUpdateSQL: TUpdateSQL;

		function GetDeleteSQL: TStrings;
		procedure SetDeleteSQL( Value: TStrings );
		function GetInsertSQL: TStrings;
		procedure SetInsertSQL( Value: TStrings );
		function GetModifySQL: TStrings;
		procedure SetModifySQL( Value: TStrings );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TKQueryItem );

		function GetNamePath: string; override;

		property UpdateSQL: TUpdateSQL
						 read FUpdateSQL;
		property Owner: TKQueryItem
						 read FOwner;

	published
		property DeleteSQL: TStrings
						 read GetDeleteSQL write SetDeleteSQL;
		property InsertSQL: TStrings
						 read GetInsertSQL write SetInsertSQL;
		property ModifySQL: TStrings
						 read GetModifySQL write SetModifySQL;

	end;

{##RI##}

{ TKFieldsData }

	TKFieldsData = class( TPersistent )
	private
		FOwner: TKQueryItem;

		procedure ReadFields( Stream: TStream );
		procedure WriteFields( Stream: TStream );

	protected
		procedure DefineProperties( Filer: TFiler ); override;

	public
		constructor Create( AOwner: TKQueryItem );

		property Owner: TKQueryItem
						 read FOwner;

	end;

{##RI##}

{ TKQueryItem }

	TKQueryItem = class( TKCustomCollectionItem )
	private
		FQuery: TKQuery;
		FCreating: Boolean;
		FFields: TKFieldsData;
		FLinkTo: TComponent;
		FExecMode: TKExecMode;
		FUpdateObject: TKPUpdateObject;

		procedure UpdateObjectNeeded;
		function GetQueryName: string;
		procedure SetQueryName( const ItemName: string );
		function GetLinkTo: TComponent;
		procedure SetLinkTo( const Value: TComponent );
		procedure SetExecMode( Value: TKExecMode );

		function GetOwnerCollection: TKQueryCollection;

{ beginning of private mappings }

{ ::TDataSet }
		function GetConstraints: TCheckConstraints;
		procedure SetConstraints( Value: TCheckConstraints );
		function GetBOF: Boolean;
		function GetBookmarkStr: TBookmarkStr;
		procedure SetBookmarkStr( Value: TBookmarkStr );
		function GetCanModify: Boolean;
		function GetDefaultFields: Boolean;
		function GetDesigner: TDataSetDesigner;
		function GetEOF: Boolean;
		function GetFieldCount: Integer;
		function GetFieldDefs: TFieldDefs;
		procedure SetFieldDefs( Value: TFieldDefs );
		function GetField( Index: Integer ): TField;
		procedure SetField( Index: Integer; Value: TField );
		function GetFieldValue( const Index: string ): Variant;
		procedure SetFieldValue( const Index: string; Value: Variant );
		function GetFound: Boolean;
		function GetModified: Boolean;
		function GetRecordCount: Integer;
		function GetRecNo: Integer;
		procedure SetRecNo( Value: Integer );
		function GetRecordSize: Word;
		function GetState: TDatasetState;
		function GetActive: Boolean;
		procedure SetActive( Value: Boolean );
		function GetAutoCalcFields: Boolean;
		procedure SetAutoCalcFields( Value: Boolean );
		function GetDataSource: TDataSource;
		procedure SetDataSource( Value: TDataSource );
		function GetFilterText: string;
		procedure SetFilterText( const Value: string );
		function GetFiltered: Boolean;
		procedure SetFiltered( Value: Boolean );
		function GetFilterOptions: TFilterOptions;
		procedure SetFilterOptions( Value: TFilterOptions );
		function GetBeforeOpen: TDataSetNotifyEvent;
		procedure SetBeforeOpen( Value: TDataSetNotifyEvent );
		function GetAfterOpen: TDataSetNotifyEvent;
		procedure SetAfterOpen( Value: TDataSetNotifyEvent );
		function GetBeforeClose: TDataSetNotifyEvent;
		procedure SetBeforeClose( Value: TDataSetNotifyEvent );
		function GetAfterClose: TDataSetNotifyEvent;
		procedure SetAfterClose( Value: TDataSetNotifyEvent );
		function GetBeforeInsert: TDataSetNotifyEvent;
		procedure SetBeforeInsert( Value: TDataSetNotifyEvent );
		function GetAfterInsert: TDataSetNotifyEvent;
		procedure SetAfterInsert( Value: TDataSetNotifyEvent );
		function GetBeforeEdit: TDataSetNotifyEvent;
		procedure SetBeforeEdit( Value: TDataSetNotifyEvent );
		function GetAfterEdit: TDataSetNotifyEvent;
		procedure SetAfterEdit( Value: TDataSetNotifyEvent );
		function GetBeforePost: TDataSetNotifyEvent;
		procedure SetBeforePost( Value: TDataSetNotifyEvent );
		function GetAfterPost: TDataSetNotifyEvent;
		procedure SetAfterPost( Value: TDataSetNotifyEvent );
		function GetBeforeCancel: TDataSetNotifyEvent;
		procedure SetBeforeCancel( Value: TDataSetNotifyEvent );
		function GetAfterCancel: TDataSetNotifyEvent;
		procedure SetAfterCancel( Value: TDataSetNotifyEvent );
		function GetBeforeDelete: TDataSetNotifyEvent;
		procedure SetBeforeDelete( Value: TDataSetNotifyEvent );
		function GetAfterDelete: TDataSetNotifyEvent;
		procedure SetAfterDelete( Value: TDataSetNotifyEvent );
		function GetBeforeScroll: TDataSetNotifyEvent;
		procedure SetBeforeScroll( Value: TDataSetNotifyEvent );
		function GetAfterScroll: TDataSetNotifyEvent;
		procedure SetAfterScroll( Value: TDataSetNotifyEvent );
		function GetOnCalcFields: TDataSetNotifyEvent;
		procedure SetOnCalcFields( Value: TDataSetNotifyEvent );
		function GetOnDeleteError: TDataSetErrorEvent;
		procedure SetOnDeleteError( Value: TDataSetErrorEvent );
		function GetOnEditError: TDataSetErrorEvent;
		procedure SetOnEditError( Value: TDataSetErrorEvent );
		function GetOnFilterRecord: TFilterRecordEvent;
		procedure SetOnFilterRecord( Value: TFilterRecordEvent );
		function GetOnNewRecord: TDataSetNotifyEvent;
		procedure SetOnNewRecord( Value: TDataSetNotifyEvent );
		function GetOnPostError: TDataSetErrorEvent;
		procedure SetOnPostError( Value: TDataSetErrorEvent );

{ ::TBDEDataSet }
		function GetCacheBlobs: Boolean;
		procedure SetCacheBlobs( Value: Boolean );
		function GetExpIndex: Boolean;
		function GetHandle: HDBICur;
		function GetKeySize: Word;
		function GetLocale: TLocale;
		function GetUpdatesPending: Boolean;
		function GetCachedUpdates: Boolean;
		procedure SetCachedUpdates( Value: Boolean );
		{$IFNDEF DELPHI4}
		function GetOnServerYield: TOnServerYieldEvent;
		procedure SetOnServerYield( Value: TOnServerYieldEvent );
		{$ENDIF}
		function GetOnUpdateError: TUpdateErrorEvent;
		procedure SetOnUpdateError( Value: TUpdateErrorEvent );
		function GetOnUpdateRecord: TUpdateRecordEvent;
		procedure SetOnUpdateRecord( Value: TUpdateRecordEvent );
		function GetUpdateRecordSet: TUpdateRecordTypes;
		procedure SetUpdateRecordSet( Value: TUpdateRecordTypes );

{ changed a little... }
		function GetUpdateObject: TKPUpdateObject;
		procedure SetUpdateObject( Value: TKPUpdateObject );

{ ::TDBDataSet }
		function GetDatabase: TDatabase;
		function GetDBHandle: HDBIDB;
		function GetDBLocale: TLocale;
		function GetDBSession: TSession;
		function GetProvider: IProvider;
		function GetDatabaseName: string;
		procedure SetDatabaseName( const Value: string );
		function GetSessionName: string;
		procedure SetSessionName( const Value: string );

{ ::TBDEDataSet }
		function GetPrepared: Boolean;
		procedure SetPrepare( Value: Boolean );
		function GetParamsCount: Word;
		function GetLocal: Boolean;
		function GetStmtHandle: HDBIStmt;
		function GetText: string;
		function GetRowsAffected: Integer;
		function GetSQLBinary: PChar;
		procedure SetSQLBinary( Value: PChar );

{ ::TQuery }
		function GetConstrained: Boolean;
		procedure SetConstrained( Value: Boolean );
		function GetParamCheck: Boolean;
		procedure SetParamCheck( Value: Boolean );
		function GetParams: TParams;
		procedure SetParamsList( Value: TParams );
		function GetRequestLive: Boolean;
		procedure SetRequestLive( Value: Boolean );
		function GetSQL: TStrings;
		procedure SetSQL( Value: TStrings );
		function GetUniDirectional: Boolean;
		procedure SetUniDirectional( Value: Boolean );
		function GetUpdateMode: TUpdateMode;
		procedure SetUpdateMode( Value: TUpdateMode );

{ ::TKQuery }
		function GetActionKinds: TKDataSetActionKind;
		function GetAfterCommit: TDataSetNotifyEvent;
		function GetAfterDatasetAction: TKDataSetAfterActionEvent;
		function GetAggregateDataSetActions: TKAggregateDataSetActions;
		function GetAuditory: Boolean;
		function GetAutoUpdate: Boolean;
		procedure SetAutoUpdate(const Value: Boolean);
		function GetAutoCommit: Boolean;
		procedure SetAutoCommit( const Value: Boolean);
		function GetBeforeCommit: TDataSetNotifyEvent;
		function GetBeforeDatasetAction: TKDataSetBeforeActionEvent;
    function GetCancelOnRollback: Boolean;
		function GetConfirmDelete: Boolean;
		function GetCommitCacheCount: Integer;
		function GetCacheIntegrity: TKCacheIntegrity;
		function GetCommitting: Boolean;
		function GetOverlappedTransaction: Boolean;
		function GetLocalOverlappedDepth: Integer;
		function GetGlobalOverlappedDepth: Integer;
		function GetOnOverlappedCommit: TKQueryOverlappedEvent;
		procedure SetOnOverlappedCommit( Value: TKQueryOverlappedEvent );
		function GetOnOverlappedEnd: TKQueryOverlappedEvent;
		procedure SetOnOverlappedEnd( Value: TKQueryOverlappedEvent );
		function GetOnOverlappedRollback: TKQueryOverlappedEvent;
		procedure SetOnOverlappedRollback( Value: TKQueryOverlappedEvent );
		function GetCurrentAction: TKDataSetAction;
    function GetDSActions: TKDataSetActions;
    function GetKeyFields: string;
    function GetKKeyActions: TKKeyActions;
		function GetOnExecSQL: TDataSetNotifyEvent;
		function GetOnKeyDown: TKDataSetKeyEvent;
		function GetOnRollBackChildren: TKDataSetRollbackEvent;
		function GetOnUpdateChildren: TDatasetNotifyEvent;
		function GetCommitCacheSize: Word;

		function GetCheckKeyAction: Boolean;
		function GetTransactionGroupIndex: Cardinal;
		function GetAfterChildrenApplyUpdates: TDatasetNotifyEvent;
		function GetBeforeChildrenApplyUpdates: TDatasetNotifyEvent;
		function GetOnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent;

		procedure SetActionKinds(const Value: TKDataSetActionKind);
		procedure SetAfterCommit(const Value: TDataSetNotifyEvent);
		procedure SetAfterDatasetAction(
			const Value: TKDataSetAfterActionEvent);
		procedure SetAggregateDataSetActions(
			const Value: TKAggregateDataSetActions);
		procedure SetAuditory(const Value: Boolean);
		procedure SetBeforeCommit(const Value: TDataSetNotifyEvent);
		procedure SetBeforeDatasetAction(
			const Value: TKDataSetBeforeActionEvent);
		procedure SetCancelOnRollback(const Value: Boolean);
		procedure SetConfirmDelete(const Value: Boolean);
		procedure SetDSActions(const Value: TKDataSetActions);
		procedure SetKeyActions(const Value: TKKeyActions);
		procedure SetKeyFields(const Value: string);
		procedure SetOnExecSQL(const Value: TDataSetNotifyEvent);
		procedure SetOnKeyDown(const Value: TKDataSetKeyEvent);
		procedure SetOnRollBackChildren(const Value: TKDataSetRollbackEvent);
		procedure SetCommitCacheSize(const Value: Word);
		procedure SetOnUpdateChildren( Value: TDatasetNotifyEvent );

		procedure SetCheckKeyAction( Value: Boolean );
		procedure SetTransactionGroupIndex( Value: Cardinal );
		procedure SetAfterChildrenApplyUpdates( Value: TDatasetNotifyEvent );
		procedure SetBeforeChildrenApplyUpdates( Value: TDatasetNotifyEvent );
		procedure SetOnCheckCacheIntegrity( Value: TKDatasetCacheIntegrityEvent );

{ end of private mappings }

	protected
		FActive: Boolean;

		procedure SetDisplayName( const Value: string ); override;
    function IsStored: Boolean; virtual;

	public
		destructor Destroy; override;
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property Query: TKQuery
						 read FQuery;

		property Owner: TKQueryCollection
						 read GetOwnerCollection;
		property QueryName: string
						 read GetQueryName;
						 
	published
		property Name;
		property FieldsData: TKFieldsData
						 read FFields write FFields;

		property ExecMode: TKExecMode
						 read FExecMode write SetExecMode default emOpen;
		property LinkTo: TComponent
						 read GetLinkTo write SetLinkTo;

{ Mapping from TDataSet to TKQueryItem }

	public
		function ActiveBuffer: PChar;
		procedure Append;
		procedure AppendRecord( const Values: array of const );
		function BookmarkValid( Bookmark: TBookmark ): Boolean; virtual;
		procedure Cancel; virtual;
		procedure CheckBrowseMode;
		procedure ClearFields;
		procedure Close;
		function  ControlsDisabled: Boolean;
		function CompareBookmarks( Bookmark1, Bookmark2: TBookmark ): Integer; virtual;
		function CreateBlobStream( Field: TField; Mode: TBlobStreamMode ): TStream; virtual;
		procedure CursorPosChanged;
		procedure Delete;
		procedure DisableControls;
		procedure Edit;
		procedure EnableControls;
		function FieldByName( const FieldName: string ): TField;
		function FindField( const FieldName: string ): TField;
		function FindFirst: Boolean;
		function FindLast: Boolean;
		function FindNext: Boolean;
		function FindPrior: Boolean;
		procedure First;
		procedure FreeBookmark( Bookmark: TBookmark ); virtual;
		function GetBookmark: TBookmark; virtual;
		function GetCurrentRecord( Buffer: PChar ): Boolean; virtual;
		procedure GetFieldList( List: TList; const FieldNames: string );
		procedure GetFieldNames( List: TStrings );
		procedure GotoBookmark( Bookmark: TBookmark );
		procedure Insert;
		procedure InsertRecord( const Values: array of const );
		function IsEmpty: Boolean;
		function IsLinkedTo( DataSource: TDataSource ): Boolean;
		function IsSequenced: Boolean; virtual;
		procedure Last;
		function Locate( const KeyFields: string; const KeyValues: Variant;
			Options: TLocateOptions ): Boolean; virtual;
		function Lookup( const KeyFields: string; const KeyValues: Variant;
			const ResultFields: string ): Variant; virtual;
		function MoveBy( Distance: Integer ): Integer;
		procedure Next;
		procedure Open;
		procedure Post; virtual;
		procedure Prior;
		procedure Resync( Mode: TResyncMode ); virtual;
		procedure SetFields( const Values: array of const );
		procedure Translate( Src, Dest: PChar; ToOem: Boolean ); virtual;
		procedure UpdateCursorPos;
		procedure UpdateRecord;

		property BOF: Boolean
						 read GetBOF;
		property Bookmark: TBookmarkStr
						 read GetBookmarkStr write SetBookmarkStr;
		property CanModify: Boolean
						 read GetCanModify;
		property DefaultFields: Boolean
						 read GetDefaultFields;
		property Designer: TDataSetDesigner
						 read GetDesigner;
		property EOF: Boolean
						 read GetEOF;
		property FieldCount: Integer
						 read GetFieldCount;
		property FieldDefs: TFieldDefs
						 read GetFieldDefs write SetFieldDefs;
		property Fields[Index: Integer]: TField
						 read GetField write SetField;
		property FieldValues[const FieldName: string]: Variant
						 read GetFieldValue write SetFieldValue; default;
		property Found: Boolean
						 read GetFound;
		property Modified: Boolean
						 read GetModified;
		property RecordCount: Integer
						 read GetRecordCount;
		property RecNo: Integer
						 read GetRecNo write SetRecNo;
		property RecordSize: Word
						 read GetRecordSize;
		property State: TDataSetState
						 read GetState;

	published
{ Query Specific porperties }	
		property Active: Boolean
						 read GetActive write SetActive stored IsStored default False;
		property AutoCalcFields: Boolean
						 read GetAutoCalcFields write SetAutoCalcFields stored IsStored default True;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource stored IsStored;
		property Filter: string
						 read GetFilterText write SetFilterText stored IsStored;
		property Filtered: Boolean
						 read GetFiltered write SetFiltered stored IsStored default False;
		property FilterOptions: TFilterOptions
						 read GetFilterOptions write SetFilterOptions stored IsStored default [];

		property BeforeOpen: TDataSetNotifyEvent
						 read GetBeforeOpen write SetBeforeOpen stored IsStored;
		property AfterOpen: TDataSetNotifyEvent
						 read GetAfterOpen write SetAfterOpen stored IsStored;
		property BeforeClose: TDataSetNotifyEvent
						 read GetBeforeClose write SetBeforeClose stored IsStored;
		property AfterClose: TDataSetNotifyEvent
						 read GetAfterClose write SetAfterClose stored IsStored;
		property BeforeInsert: TDataSetNotifyEvent
						 read GetBeforeInsert write SetBeforeInsert stored IsStored;
		property AfterInsert: TDataSetNotifyEvent
						 read GetAfterInsert write SetAfterInsert stored IsStored;
		property BeforeEdit: TDataSetNotifyEvent
						 read GetBeforeEdit write SetBeforeEdit stored IsStored;
		property AfterEdit: TDataSetNotifyEvent
						 read GetAfterEdit write SetAfterEdit stored IsStored;
		property BeforePost: TDataSetNotifyEvent
						 read GetBeforePost write SetBeforePost stored IsStored;
		property AfterPost: TDataSetNotifyEvent
						 read GetAfterPost write SetAfterPost stored IsStored;
		property BeforeCancel: TDataSetNotifyEvent
						 read GetBeforeCancel write SetBeforeCancel stored IsStored;
		property AfterCancel: TDataSetNotifyEvent
						 read GetAfterCancel write SetAfterCancel stored IsStored;
		property BeforeDelete: TDataSetNotifyEvent
						 read GetBeforeDelete write SetBeforeDelete stored IsStored;
		property AfterDelete: TDataSetNotifyEvent
						 read GetAfterDelete write SetAfterDelete stored IsStored;
		property BeforeScroll: TDataSetNotifyEvent
						 read GetBeforeScroll write SetBeforeScroll stored IsStored;
		property AfterScroll: TDataSetNotifyEvent
						 read GetAfterScroll write SetAfterScroll stored IsStored;
		property OnCalcFields: TDataSetNotifyEvent
						 read GetOnCalcFields write SetOnCalcFields stored IsStored;
		property OnDeleteError: TDataSetErrorEvent
						 read GetOnDeleteError write SetOnDeleteError stored IsStored;
		property OnEditError: TDataSetErrorEvent
						 read GetOnEditError write SetOnEditError stored IsStored;
		property OnFilterRecord: TFilterRecordEvent
						 read GetOnFilterRecord write SetOnFilterRecord stored IsStored;
		property OnNewRecord: TDataSetNotifyEvent
						 read GetOnNewRecord write SetOnNewRecord  stored IsStored;
		property OnPostError: TDataSetErrorEvent
						 read GetOnPostError write SetOnPostError stored IsStored;

{ Mapping from TBDEDataSet to TKQueryItem }

	public
		procedure ApplyUpdates;
		procedure CancelUpdates;
		procedure CommitUpdates;
		function ConstraintCallBack( Req: DsInfoReq; var ADataSources: DataSources ): DBIResult;
		function ConstraintsDisabled: Boolean;
		procedure DisableConstraints;
		procedure EnableConstraints;
		procedure FetchAll;
		procedure FlushBuffers;
		procedure GetIndexInfo;
		procedure RevertRecord;
		function UpdateStatus: TUpdateStatus;

		property CacheBlobs: Boolean
						 read GetCacheBlobs write SetCacheBlobs default True;
		property ExpIndex: Boolean
						 read GetExpIndex;
		property Handle: HDBICur
						 read GetHandle;
		property KeySize: Word
						 read GetKeySize;
		property Locale: TLocale
						 read GetLocale;
		property UpdatesPending: Boolean
						 read GetUpdatesPending;
		property UpdateRecordTypes: TUpdateRecordTypes
						 read GetUpdateRecordSet write SetUpdateRecordSet;

	published
		property CachedUpdates: Boolean
						 read GetCachedUpdates write SetCachedUpdates stored IsStored default False;
		property UpdateObject: TKPUpdateObject
						 read GetUpdateObject write SetUpdateObject stored IsStored;

		{$IFNDEF DELPHI4}
		property OnServerYield: TOnServerYieldEvent
						 read GetOnServerYield write SetOnServerYield stored IsStored;
		{$ENDIF}
		property OnUpdateError: TUpdateErrorEvent
						 read GetOnUpdateError write SetOnUpdateError stored IsStored;
		property OnUpdateRecord: TUpdateRecordEvent
						 read GetOnUpdateRecord write SetOnUpdateRecord stored IsStored;

{ Mapping from TDBDataSet to TKQueryItem }

	public
		function OpenDatabase: TDatabase;
		procedure CloseDatabase( Database: TDatabase );
		function CheckOpen( Status: DBIResult ): Boolean;

		property Database: TDatabase
						 read GetDatabase;
		property DBHandle: HDBIDB
						 read GetDBHandle;
		property DBLocale: TLocale
						 read GetDBLocale;
		property DBSession: TSession
						 read GetDBSession;
		property Provider: IProvider
						 read GetProvider;

	published
		property DatabaseName: string
						 read GetDatabaseName write SetDatabaseName stored IsStored;
		property SessionName: string
						 read GetSessionName write SetSessionName stored IsStored;

{ Mapping from TQuery to TKQueryItem }

	public
		procedure ExecSQL;
		function ParamByName( const Value: string ): TParam;
		procedure Prepare;
		procedure UnPrepare;

		property Prepared: Boolean
						 read GetPrepared write SetPrepare;
		property ParamCount: Word
						 read GetParamsCount;
		property Local: Boolean
						 read GetLocal;
		property StmtHandle: HDBIStmt
						 read GetStmtHandle;
		property Text: string
						 read GetText;
		property RowsAffected: Integer
						 read GetRowsAffected;
		property SQLBinary: PChar
						 read GetSQLBinary write SetSQLBinary;

	published
		property Constrained: Boolean
						 read GetConstrained write SetConstrained stored IsStored default False;
		property Constraints: TCheckConstraints
						 read GetConstraints write SetConstraints stored IsStored;
		property ParamCheck: Boolean
						 read GetParamCheck write SetParamCheck stored IsStored default True;
		property Params: TParams
						 read GetParams write SetParamsList stored IsStored;
		property RequestLive: Boolean
						 read GetRequestLive write SetRequestLive stored IsStored default False;
		property SQL: TStrings
						 read GetSQL write SetSQL stored IsStored;
		property UniDirectional: Boolean
						 read GetUniDirectional write SetUniDirectional stored IsStored default False;
		property UpdateMode: TUpdateMode
						 read GetUpdateMode write SetUpdateMode stored IsStored default upWhereAll;

{ Mapping from TKQuery to TKQueryItem }

	public
		procedure Update; virtual;
		procedure Refresh; virtual;

		function ActionAllowed( Action: TKDataSetAction ): Boolean; dynamic;
		function DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean; dynamic;

		property CommitCacheCount: Integer
						 read GetCommitCacheCount;
		property CacheIntegrity: TKCacheIntegrity
						 read GetCacheIntegrity;
		property Committing: Boolean
						 read GetCommitting;
		property CurrentAction: TKDataSetAction
						 read GetCurrentAction;
		property DSActions: TKDataSetActions
						 read GetDSActions write SetDSActions;

{ Overlapped transaction support }

		procedure ErrTransaction; dynamic;
		procedure PushTransaction; dynamic;

		property OverlappedTransaction: Boolean
						 read GetOverlappedTransaction;
		property LocalOverlappedDepth: Integer
						 read GetLocalOverlappedDepth;
		property GlobalOverlappedDepth: Integer
						 read GetGlobalOverlappedDepth;

{ End of overlapped transaction support }

	published
{ Overlapped transaction support }
		property CommitCacheSize: Word
						 read GetCommitCacheSize write SetCommitCacheSize stored IsStored default DEFAULT_COMMITCACHESIZE;
		property OnOverlappedCommit: TKQueryOverlappedEvent
						 read GetOnOverlappedCommit write SetOnOverlappedCommit stored IsStored;
		property OnOverlappedEnd: TKQueryOverlappedEvent
						 read GetOnOverlappedEnd write SetOnOverlappedEnd stored IsStored;
		property OnOverlappedRollback: TKQueryOverlappedEvent
						 read GetOnOverlappedRollback write SetOnOverlappedRollback stored IsStored;
{ End of overlapped transaction support }

		property AutoUpdate: Boolean
						 read GetAutoUpdate write SetAutoUpdate stored IsStored default true;
		property AutoCommit: Boolean
						 read GetAutoCommit write SetAutoCommit stored IsStored default true;
		property CancelOnRollback: Boolean
						 read GetCancelOnRollback write SetCancelOnRollback stored IsStored  default false;
		property ConfirmDelete: Boolean
						 read GetConfirmDelete write SetConfirmDelete stored IsStored default false;

		property CheckKeyAction: Boolean
						 read GetCheckKeyAction write SetCheckKeyAction stored IsStored default true;
		property TransactionGroupIndex: Cardinal
						 read GetTransactionGroupIndex write SetTransactionGroupIndex stored IsStored;

		property Auditory: Boolean
						 read GetAuditory write SetAuditory stored IsStored default false;
		property KeyActions: TKKeyActions
						 read GetKKeyActions write SetKeyActions stored IsStored;
		property DataSetActions: TKAggregateDataSetActions
						 read GetAggregateDataSetActions write SetAggregateDataSetActions stored IsStored;
		property KeyFields: string
						 read GetKeyFields write SetKeyFields stored IsStored;
		property ActionKinds: TKDataSetActionKind
						 read GetActionKinds write SetActionKinds stored IsStored default dakNone;

		property AfterCommit: TDataSetNotifyEvent
						 read GetAfterCommit write SetAfterCommit stored IsStored;
		property BeforeCommit: TDataSetNotifyEvent
						 read GetBeforeCommit write SetBeforeCommit stored IsStored;
		property BeforeDatasetAction: TKDataSetBeforeActionEvent
						 read GetBeforeDatasetAction write SetBeforeDatasetAction stored IsStored;
		property AfterDatasetAction: TKDataSetAfterActionEvent
						 read GetAfterDatasetAction write SetAfterDatasetAction stored IsStored;
		property OnExecSQL: TDataSetNotifyEvent
						 read GetOnExecSQL write SetOnExecSQL stored IsStored;
		property OnKeyDown : TKDataSetKeyEvent
						 read GetOnKeyDown write SetOnKeyDown stored IsStored;
		property OnRollBackChildren: TKDatasetRollbackEvent
						 read GetOnRollBackChildren write SetOnRollBackChildren stored IsStored;
		property OnUpdateChildren: TDatasetNotifyEvent
						 read GetOnUpdateChildren write SetOnUpdateChildren stored IsStored;

		property AfterChildrenApplyUpdates: TDatasetNotifyEvent
						 read GetAfterChildrenApplyUpdates write SetAfterChildrenApplyUpdates stored IsStored;
		property BeforeChildrenApplyUpdates: TDatasetNotifyEvent
						 read GetBeforeChildrenApplyUpdates write SetBeforeChildrenApplyUpdates stored IsStored;
		property OnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent
						 read GetOnCheckCacheIntegrity write SetOnCheckCacheIntegrity stored IsStored;

	end;

{ TKQueryCollection }

	TKQueryCollection = class( TKCustomCollection )
	private
		function GetOwnerComp: TKSQLScript;

		function GetItem( Index: Integer ): TKQueryItem;
		procedure SetItem( Index: Integer; Value: TKQueryItem );

		function GetItemByName( const AName: string ): TKQueryItem;

	protected
		procedure Update( Item: TCollectionItem ); override;
		class function GetDefaultItemName( Item: TKCustomCollectionItem ): string; override;

	public
		constructor Create( AOwner: TKSQLScript ); virtual;

		function Add: TKQueryItem; virtual;

		property Items[Index: Integer]: TKQueryItem
						 read GetItem write SetItem; default;
		property ItemByName[const AName: string]: TKQueryItem
						 read GetItemByName;
		property Names;

		property Component: TKSQLScript
						 read GetOwnerComp;

	end;

{ TKSQLScript }

	TKSQLScript = class( TKCustomLinkable )
	private
		FExclusiveMode: Boolean;
		FQueryCollection: TKQueryCollection;

		function GetQueries( Index: string ): TQuery;
		procedure SetQueryCollection( Value: TKQueryCollection );
		procedure UpdateItems;

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure UpdateItem( Item: TKQueryItem ); virtual;

		procedure LoadItemsFromStream( Stream: TStream ); virtual;
		procedure SaveItemsToStream( Stream: TStream ); virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
    procedure Assign( Source: TPersistent ); override;
		procedure LoadItemsFromFile( const FileName: string );
		procedure SaveItemsToFile( const FileName: string );

		procedure RunScript;
		procedure RunScriptEx( StartPos, EndPos: Word );
		procedure RunTransactionOn( ADatabase: TDatabase );
		procedure RunTransactionOnEx( ADatabase: TDatabase; StartPos, EndPos: Word );
		function FindQuery( const AQuery: string ): TQuery;
		function QueryByName( const AQuery: string ): TQuery;

		property Queries[Index: string]: TQuery
						 read GetQueries; default;

	published
		property ExclusiveMode: Boolean
						 read FExclusiveMode write FExclusiveMode default false;
		property QueryCollection: TKQueryCollection
						 read FQueryCollection write SetQueryCollection;

	end;

{
--------------------------------------------------------------------------------
------------------------- Extended SQL Script Classes --------------------------
--------------------------------------------------------------------------------
}

	TKSQLScriptEx = class;

	TKSQLScritTableBuildEvent = procedure( Sender: TKSQLScriptEx;
		SQLBuilder: TKDBLocalSQLBuild; TableID: Integer ) of object;

	TKSQLScritIndexBuildEvent = procedure( Sender: TKSQLScriptEx;
		SQLBuilder: TKDBLocalSQLBuild; IndexID: Integer ) of object;

{ TKSQLScriptEx }

	TKSQLScriptEx = class( TKSQLScript )
	private
		FScript: string;
		FScriptCleared: Boolean;
		FSQLScriptType: TKSQLScriptType;
		FAutomaticBuildItems: Boolean;
		FDBSQLB: TKDBAnsiSQLBuild;
		FKSQLScritTableBuildEvent: TKSQLScritTableBuildEvent;
		FKSQLScritIndexBuildEvent: TKSQLScritIndexBuildEvent;

    procedure AdjustSQLBuildClass;
		procedure CheckSQLScriptForBuildScript;
		procedure CheckSQLScriptForCreateDataBase( const DestDBName: string );

		procedure SetInfo( Index: Integer; const Value: string );
		function GetInfo( Index: Integer ): string;
		procedure SetBoolInfo( Index: Integer; const Value: Boolean );
		function GetBoolInfo( Index: Integer ): Boolean;

	protected
		procedure BuildEvent( Sender: TKCustomSQLBuild; IteratorID, SQLStringsID: Integer;
			SQLBuildType: TKSQLBuildType ); dynamic;
		procedure BuildQueryItem( Sender: TKCustomSQLBuild; IteratorID, SQLStringsID: Integer;
			SQLBuildType: TKSQLBuildType ); dynamic;

		procedure ScriptSections( const Section: string ); virtual;
		procedure NormalizeDatabaseName; dynamic;
		procedure LoadScriptsFromStream( Stream: TStream ); virtual;
		procedure SaveScriptsToStream( Stream: TStream ); virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure Assign( Source: TPersistent ); override;
		procedure LoadScriptsFromFile( const FileName: string ); virtual;
		procedure SaveScriptsToFile( const FileName: string ); virtual;

		procedure BuildScript; virtual;
		procedure CreateDataBase( const DestDBName: string ); virtual;

		property DBSQLB: TKDBAnsiSQLBuild
						 read FDBSQLB;
		property Script: string
						 index 3 read GetInfo;

	published
		property DataBaseName: string
						 index 0 read GetInfo write SetInfo;
		property SessionName: string
						 index 1 read GetInfo write SetInfo;
		property TablePatterns: string
						 index 2 read GetInfo write SetInfo;
		property SQLScriptType: TKSQLScriptType
						 read FSQLScriptType write FSQLScriptType default sstLocal;
		property SystemTables: Boolean
						 index 0 read GetBoolInfo write SetBoolInfo default False;
		property Extensions: Boolean
						 index 1 read GetBoolInfo write SetBoolInfo default True;
	 {property BuildItems: Boolean
						 index 2 read GetBoolInfo write SetBoolInfo default True;}

		property OnTableBuild: TKSQLScritTableBuildEvent
						 read FKSQLScritTableBuildEvent write FKSQLScritTableBuildEvent;
		property OnIndexBuild: TKSQLScritIndexBuildEvent
						 read FKSQLScritIndexBuildEvent write FKSQLScritIndexBuildEvent;

	end;

procedure PrintSQLScripts( SQLScript: TKSQLScript );
procedure PrintSQLScriptsEx( SQLScript: TKSQLScript; StartPos, EndPos: Word );

implementation

uses
	SysUtils, Forms, TypInfo, Graphics, Printers, uksyConsts, ukrUtils, ukrDBUtils,
	ukdbResStr, ukdbUtils;

{
--------------------------------------------------------------------------------
------------------------------ SQL Script Classes ------------------------------
--------------------------------------------------------------------------------
}

{------------------------------- TKPUpdateObject -------------------------------}


constructor TKPUpdateObject.Create( AOwner: TKQueryItem );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
	FUpdateSQL := TUpdateSQL.Create( FOwner.Owner.Component );
end;

destructor TKPUpdateObject.Destroy;
begin
	FUpdateSQL.Free;
	inherited Destroy;
end;

function TKPUpdateObject.GetNamePath: string;
begin
	Result := Owner.GetNamePath + '.' + Copy( ClassName, 4, SizeOf( ShortString ) ); 
end;

function TKPUpdateObject.GetDeleteSQL: TStrings;
begin
	Result := FUpdateSQL.DeleteSQL;
end;

procedure TKPUpdateObject.SetDeleteSQL( Value: TStrings );
begin
	FUpdateSQL.DeleteSQL := Value;
end;

function TKPUpdateObject.GetInsertSQL: TStrings;
begin
	Result := FUpdateSQL.InsertSQL;
end;

procedure TKPUpdateObject.SetInsertSQL( Value: TStrings );
begin
	FUpdateSQL.InsertSQL := Value;
end;

function TKPUpdateObject.GetModifySQL: TStrings;
begin
	Result := FUpdateSQL.ModifySQL;
end;

procedure TKPUpdateObject.SetModifySQL( Value: TStrings );
begin
	FUpdateSQL.ModifySQL := Value;
end;

type

	TKHackQuery = class( TKQuery )
	protected
		procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

	published
		property UpdateObject stored False;

	end;

procedure TKHackQuery.GetChildren( Proc: TGetChildProc; Root: TComponent );
var
	i: Integer;
begin
	if CheckObject( Root ) then
		for i := 0 to FieldCount - 1 do
			if ( ( CheckObjectClass( Root, TCustomForm ) and ( Fields[i].Owner = Root ) ) or
				( CheckObject( Root.Owner ) and ( Fields[i].Owner = Root.Owner ) ) ) then
				Proc( Fields[i] );
end;

{-------------------------------- TKFieldsData ---------------------------------}

constructor TKFieldsData.Create( AOwner: TKQueryItem );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
end;

procedure TKFieldsData.ReadFields( Stream: TStream );
var
	i,
	iChildPos,
	iSavePos,
	iFldCount: Integer;
	Field: TField;
	Reader: TReader;
	FilerFlags: TFilerFlags;
begin
	iFldCount := 0;
	Stream.ReadBuffer( iFldCount, SizeOf( Integer ) );
	for i := 0 to iFldCount - 1 do
	begin
		iSavePos := Stream.Position;
		try
			Reader := TReader.Create( Stream, 4 * KB );
			try
				Reader.ReadSignature;
				Reader.ReadPrefix( FilerFlags, iChildPos );
//Field := ( TComponentClass( FindClass( Reader.ReadStr ) ).Create( FOwner.Owner.Component.Owner ) as TFieldClass );
				Field := ( TComponentClass( FindClass( Reader.ReadStr ) ).Create( FOwner.Owner.Component.Owner ) as TField );
				try
					if Designing( Field ) then
						Field.Name := Reader.ReadStr;
				except
					Field.Free;
					raise;
				end;
			finally
				Reader.Free;
			end;
		finally
			Stream.Position := iSavePos;
		end;
		Stream.ReadComponent( Field );
		Field.DataSet := FOwner.Query;
	end;
end;

procedure TKFieldsData.WriteFields( Stream: TStream );
var
	i: Integer;
begin
	i := FOwner.Query.FieldCount;
	Stream.WriteBuffer( i, SizeOf( Integer ) );
	for i := 0 to FOwner.Query.FieldCount - 1 do
		Stream.WriteComponent( FOwner.Query.Fields[i] );
end;

procedure TKFieldsData.DefineProperties( Filer: TFiler );

	function DoWrite: Boolean;
	begin

		if CheckObject( Filer.Ancestor ) then
			Result := CheckObjectClass( Filer.Ancestor, TKFieldsData )
		else
		  Result := ( FOwner.Query.FieldCount > 0 );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineBinaryProperty( 'FieldsData', ReadFields, WriteFields, DoWrite );
end;

{-------------------------------- TKQueryItem ----------------------------------}

const
	DATASET_PROPERTY_NAME = 'Dataset';

type
	
	TQuerySHack = class( TPersistent )
	private
		FOwner: TComponent;

	end;

{

	Let Constraints be public for now. It has taken me over 8 hours

	trying to hack a way through TCheckConstraintsProperty property
	editor- with no success. No new editor, no brute force typecast,
	no methods' addresses exchange, nothing!!! This is a tough one!

	No wonder why Borland has made it possible to access the data
	dictionary through TCheckConstraintsProperty property editor-
	it is very difficult to get in the code!!!

	So far, it has been possible just to find the unit and the
	class name of the property editor; it has also come to
	knowledge that GetComponent( 0 ) called from the editor
	expects a TQuery object- one this class cannot offer.

	The solution that seems to work must be written in assembly.
	The idea is to identify the position in memory that the
	address to GetComponent( 0 ), and substitute it for a new
	function address, one that will correctly returns a query
	when invoked by the property inspector.

	MADE IT!!! A Super Hack has made it possible to get the editor
	to work for constraints. A descendant editor was invoked normally,
	with an overriden Edit procedure. In the edit procedure, we
	changed the FPropList^[0] so it pointed to the correct
	( Instance, PropInfo ) pair. The original GetComponent( 0 )
	returned a TQueryItem object; TCheckConstraintsProperty expected
	a TDataset object from GetComponent( 0 ), so I changed
	FPropList^[0] to point to TQueryItem.Query Instance and PropInfo.

}

{ TKQueryItem }

constructor TKQueryItem.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKQueryCollection );
	FCreating := True;
	try
		inherited Create( ACollection );
	finally
		FCreating := False;
	end;
	ForceObject( Owner.Component );
	FQuery := TKHackQuery.Create( Owner.Component );
	SetQueryName( Name );
	FFields := TKFieldsData.Create( Self );
	FLinkTo := nil;
	FActive := false;
	FExecMode := emOpen;
	FUpdateObject := nil;
end;

destructor TKQueryItem.Destroy;
begin
	if CheckObject( FQuery ) then
	begin
		FQuery.Active := false;
		FreeClean( FQuery );
	end;
	FreeClean( FFields );
	FreeClean( FUpdateObject );
	inherited Destroy;
end;

procedure TKQueryItem.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKQueryItem ) then
		FQuery.Assign( TKQueryItem( Source ).Query );
end;

function TKQueryItem.GetOwnerCollection: TKQueryCollection;
begin
	Result := TKQueryCollection( inherited GetOwnerCollection );
end;

function TKQueryItem.GetQueryName: string;
begin
	Result := FQuery.Name;
end;

function TKQueryItem.IsStored: Boolean;
begin
	Result := True;
end;

procedure TKQueryItem.SetQueryName( const ItemName: string );
var
	bNewOwner: Boolean;
	cOldOwner: TComponent;
begin
  cOldOwner := nil;
	bNewOwner := False;
	try
{ if the fields editor is open... }
		if Designing( Owner.Component ) and
			 ( not CheckObjectClass( FQuery.Owner, Owner.Component.ClassType ) ) then
		begin
			bNewOwner := True;
			cOldOwner := Query.Owner;
			TQuerySHack( Query ).FOwner := Owner.Component;
		end;
		Query.Tag := ID;
		Query.Name := ItemName;
	finally
		if bNewOwner then
			TQuerySHack( Query ).FOwner := cOldOwner;
	end;
end;

procedure TKQueryItem.SetDisplayName( const Value: string );
begin
	if ( not CheckStrEqual( Name, Value ) ) then
	begin
		inherited SetDisplayName( Value );
		if ( not FCreating ) then
			SetQueryName( Value );
	end;
end;

function TKQueryItem.GetLinkTo: TComponent;
var
	pi: PPropInfo;
begin
	Result := FLinkTo;
	if CheckObject( Result ) then
	begin
		pi := GetPropInfo( Result.ClassInfo, DATASET_PROPERTY_NAME );
		if CheckPointer( pi ) then
		begin
			if ( GetOrdProp( Result, pi ) <> LongInt( FQuery ) ) then
			begin
				FLinkTo := nil;
				Result := FLinkTo;
			end;
		end;
	end;
end;

procedure TKQueryItem.SetLinkTo( const Value: TComponent );
var
	pi: PPropInfo;
begin
	if ( not CheckObject( Value ) ) then
	begin
		if CheckObject( FLinkTo ) then
		begin
			pi := GetPropInfo( FLinkTo.ClassInfo, DATASET_PROPERTY_NAME );
			if CheckPointer( pi ) then
				SetOrdProp( FLinkTo, pi, LongInt( nil ) );
		end;
		FLinkTo := nil;
		Exit;
	end;
	pi := GetPropInfo( Value.ClassInfo, DATASET_PROPERTY_NAME );
	if ( not CheckPointer( pi ) ) then
	begin
		FLinkTo := nil;
		Exit;
	end;
	FLinkTo := Value;
	SetOrdProp( FLinkTo, pi, LongInt( FQuery ) );
	FLinkTo.FreeNotification( Owner.Component );
end;

procedure TKQueryItem.SetExecMode( Value: TKExecMode );
begin
	FExecMode := Value;
end;

function TKQueryItem.GetConstraints: TCheckConstraints;
begin
	Result := FQuery.Constraints;
end;

procedure TKQueryItem.SetConstraints( Value: TCheckConstraints );
begin
	FQuery.Constraints := Value;
end;

function TKQueryItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( ( inherited Equals( Item ) ) and CheckObjectClass( Item, TKQueryItem ) and
		( Query = ( Item as TKQueryItem ).Query ) );
end;

{ beginning of private mappings }

{ ::TDataSet }
function TKQueryItem.GetBOF: Boolean;
begin
	Result := FQuery.BOF;
end;

function TKQueryItem.GetBookmarkStr: TBookmarkStr;
begin
	Result := FQuery.Bookmark;
end;

procedure TKQueryItem.SetBookmarkStr( Value: TBookmarkStr );
begin
	FQuery.Bookmark := Value;
end;

function TKQueryItem.GetCanModify: Boolean;
begin
	Result := FQuery.CanModify;
end;

function TKQueryItem.GetDefaultFields: Boolean;
begin
	Result := FQuery.DefaultFields;
end;

function TKQueryItem.GetDesigner: TDataSetDesigner;
begin
	Result := FQuery.Designer;
end;

function TKQueryItem.GetEOF: Boolean;
begin
	Result := FQuery.EOF;
end;

function TKQueryItem.GetFieldCount: Integer;
begin
	Result := FQuery.FieldCount;
end;

function TKQueryItem.GetFieldDefs: TFieldDefs;
begin
	Result := FQuery.FieldDefs;
end;

procedure TKQueryItem.SetFieldDefs( Value: TFieldDefs );
begin
	FQuery.FieldDefs := Value;
end;

function TKQueryItem.GetField( Index: Integer ): TField;
begin
	Result := FQuery.Fields[Index];
end;

procedure TKQueryItem.SetField( Index: Integer; Value: TField );
var
	i: Integer; // hint...
begin
{$IFNDEF DELPHI4}
	FQuery.Fields[Index] := Value;
{$ELSE}
  //error !S?S?SS  
{$ENDIF}
end;

function TKQueryItem.GetFieldValue( const Index: string ): Variant;
begin
	Result := FQuery[Index].Value;
end;

procedure TKQueryItem.SetFieldValue( const Index: string; Value: Variant );
begin
	FQuery[Index].Value := Value;
end;

function TKQueryItem.GetFound: Boolean;
begin
	Result := FQuery.Found;
end;

function TKQueryItem.GetModified: Boolean;
begin
	Result := FQuery.Modified;
end;

function TKQueryItem.GetRecordCount: Integer;
begin
	Result := FQuery.RecordCount;
end;

function TKQueryItem.GetRecNo: Integer;
begin
	Result := FQuery.RecNo;
end;

procedure TKQueryItem.SetRecNo( Value: Integer );
begin
	FQuery.RecNo := Value;
end;

function TKQueryItem.GetRecordSize: Word;
begin
	Result := FQuery.RecordSize;
end;

function TKQueryItem.GetState: TDatasetState;
begin
	Result := FQuery.State;
end;

function TKQueryItem.GetActive: Boolean;
begin
	Result := FQuery.Active;
end;

procedure TKQueryItem.SetActive( Value: Boolean );
var
	i: Integer;
begin
	if ( not Loading( Owner.Component ) ) then
	begin
		if Value then
			with Owner do
				if Component.ExclusiveMode then
					for i := 0 to Count - 1 do
						if ( Items[i] <> Self ) and ( Items[i].Active ) then
							Items[i].Close;
		FQuery.Active := Value;
	end
	else
		FActive := Value;
end;

function TKQueryItem.GetAutoCalcFields: Boolean;
begin
	Result := FQuery.AutoCalcFields;
end;

procedure TKQueryItem.SetAutoCalcFields( Value: Boolean );
begin
	FQuery.AutoCalcFields := Value;
end;

function TKQueryItem.GetDataSource: TDataSource;
begin
	Result := FQuery.DataSource;
end;

procedure TKQueryItem.SetDataSource( Value: TDataSource );
begin
	FQuery.DataSource := Value;
end;

function TKQueryItem.GetFilterText: string;
begin
	Result := FQuery.Filter;
end;

procedure TKQueryItem.SetFilterText( const Value: string );
begin
	FQuery.Filter := Value;
end;

function TKQueryItem.GetFiltered: Boolean;
begin
	Result := FQuery.Filtered;
end;

procedure TKQueryItem.SetFiltered( Value: Boolean );
begin
	FQuery.Filtered := Value;
end;

function TKQueryItem.GetFilterOptions: TFilterOptions;
begin
	Result := FQuery.FilterOptions;
end;

procedure TKQueryItem.SetFilterOptions( Value: TFilterOptions );
begin
	FQuery.FilterOptions := Value;
end;

function TKQueryItem.GetBeforeOpen: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeOpen;
end;

procedure TKQueryItem.SetBeforeOpen( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeOpen := Value;
end;

function TKQueryItem.GetAfterOpen: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterOpen;
end;

procedure TKQueryItem.SetAfterOpen( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterOpen := Value;
end;

function TKQueryItem.GetBeforeClose: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeClose;
end;

procedure TKQueryItem.SetBeforeClose( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeClose := Value;
end;

function TKQueryItem.GetAfterClose: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterClose;
end;

procedure TKQueryItem.SetAfterClose( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterClose := Value;
end;

function TKQueryItem.GetBeforeInsert: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeInsert;
end;

procedure TKQueryItem.SetBeforeInsert( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeInsert := Value;
end;

function TKQueryItem.GetAfterInsert: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterInsert;
end;

procedure TKQueryItem.SetAfterInsert( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterInsert := Value;
end;

function TKQueryItem.GetBeforeEdit: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeEdit;
end;

procedure TKQueryItem.SetBeforeEdit( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeEdit := Value;
end;

function TKQueryItem.GetAfterEdit: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterEdit;
end;

procedure TKQueryItem.SetAfterEdit( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterEdit := Value;
end;

function TKQueryItem.GetBeforePost: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforePost;
end;

procedure TKQueryItem.SetBeforePost( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforePost := Value;
end;

function TKQueryItem.GetAfterPost: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterPost;
end;

procedure TKQueryItem.SetAfterPost( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterPost := Value;
end;

function TKQueryItem.GetBeforeCancel: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeCancel;
end;

procedure TKQueryItem.SetBeforeCancel( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeCancel := Value;
end;

function TKQueryItem.GetAfterCancel: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterCancel;
end;

procedure TKQueryItem.SetAfterCancel( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterCancel := Value;
end;

function TKQueryItem.GetBeforeDelete: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeDelete;
end;

procedure TKQueryItem.SetBeforeDelete( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeDelete := Value;
end;

function TKQueryItem.GetAfterDelete: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterDelete;
end;

procedure TKQueryItem.SetAfterDelete( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterDelete := Value;
end;

function TKQueryItem.GetBeforeScroll: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeScroll;
end;

procedure TKQueryItem.SetBeforeScroll( Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeScroll := Value;
end;

function TKQueryItem.GetAfterScroll: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterScroll;
end;

procedure TKQueryItem.SetAfterScroll( Value: TDataSetNotifyEvent );
begin
	FQuery.AfterScroll := Value;
end;

function TKQueryItem.GetOnCalcFields: TDataSetNotifyEvent;
begin
	Result := FQuery.OnCalcFields;
end;

procedure TKQueryItem.SetOnCalcFields( Value: TDataSetNotifyEvent );
begin
	FQuery.OnCalcFields := Value;
end;

function TKQueryItem.GetOnDeleteError: TDataSetErrorEvent;
begin
	Result := FQuery.OnDeleteError;
end;

procedure TKQueryItem.SetOnDeleteError( Value: TDataSetErrorEvent );
begin
	FQuery.OnDeleteError := Value;
end;

function TKQueryItem.GetOnEditError: TDataSetErrorEvent;
begin
	Result := FQuery.OnEditError;
end;

procedure TKQueryItem.SetOnEditError( Value: TDataSetErrorEvent );
begin
	FQuery.OnEditError := Value;
end;

function TKQueryItem.GetOnFilterRecord: TFilterRecordEvent;
begin
	Result := FQuery.OnFilterRecord;
end;

procedure TKQueryItem.SetOnFilterRecord( Value: TFilterRecordEvent );
begin
	FQuery.OnFilterRecord := Value;
end;

function TKQueryItem.GetOnNewRecord: TDataSetNotifyEvent;
begin
	Result := FQuery.OnNewRecord;
end;

procedure TKQueryItem.SetOnNewRecord( Value: TDataSetNotifyEvent );
begin
	FQuery.OnNewRecord := Value;
end;

function TKQueryItem.GetOnPostError: TDataSetErrorEvent;
begin
	Result := FQuery.OnPostError;
end;

procedure TKQueryItem.SetOnPostError( Value: TDataSetErrorEvent );
begin
	FQuery.OnPostError := Value;
end;

{ ::TBDEDataSet }
function TKQueryItem.GetCacheBlobs: Boolean;
begin
	Result := FQuery.CacheBlobs;
end;

procedure TKQueryItem.SetCacheBlobs( Value: Boolean );
begin
	FQuery.CacheBlobs := Value;
end;

function TKQueryItem.GetExpIndex: Boolean;
begin
	Result := FQuery.ExpIndex;
end;

function TKQueryItem.GetHandle: HDBICur;
begin
	Result := FQuery.Handle;
end;

function TKQueryItem.GetKeySize: Word;
begin
	Result := FQuery.KeySize;
end;

function TKQueryItem.GetLocale: TLocale;
begin
	Result := FQuery.Locale;
end;

function TKQueryItem.GetUpdatesPending: Boolean;
begin
	Result := FQuery.UpdatesPending;
end;

function TKQueryItem.GetCachedUpdates: Boolean;
begin
	Result := FQuery.CachedUpdates;
end;

procedure TKQueryItem.SetCachedUpdates( Value: Boolean );
begin
	FQuery.CachedUpdates := Value;
	if FQuery.CachedUpdates then
		FQuery.UpdateObject := UpdateObject.UpdateSQL
	else
		FQuery.UpdateObject := nil;
end;

procedure TKQueryItem.UpdateObjectNeeded;
begin
	if ( not CheckObject( FUpdateObject ) ) then
		FUpdateObject := TKPUpdateObject.Create( Self );
end;

function TKQueryItem.GetUpdateObject: TKPUpdateObject;
begin
	UpdateObjectNeeded;
	Result := FUpdateObject;
end;

procedure TKQueryItem.SetUpdateObject( Value: TKPUpdateObject );
begin
	UpdateObjectNeeded;
	FUpdateObject := Value;
end;

{$IFNDEF DELPHI4}
function TKQueryItem.GetOnServerYield: TOnServerYieldEvent;
begin
	Result := FQuery.OnServerYield;
end;

procedure TKQueryItem.SetOnServerYield( Value: TOnServerYieldEvent );
begin
	FQuery.OnServerYield := Value;
end;
{$ENDIF}

function TKQueryItem.GetOnUpdateError: TUpdateErrorEvent;
begin
	Result := FQuery.OnUpdateError;
end;

procedure TKQueryItem.SetOnUpdateError( Value: TUpdateErrorEvent );
begin
	FQuery.OnUpdateError := Value;
end;

function TKQueryItem.GetOnUpdateRecord: TUpdateRecordEvent;
begin
	Result := FQuery.OnUpdateRecord;
end;

procedure TKQueryItem.SetOnUpdateRecord( Value: TUpdateRecordEvent );
begin
	FQuery.OnUpdateRecord := Value;
end;

function TKQueryItem.GetUpdateRecordSet: TUpdateRecordTypes;
begin
	Result := FQuery.UpdateRecordTypes;
end;

procedure TKQueryItem.SetUpdateRecordSet( Value: TUpdateRecordTypes );
begin
	FQuery.UpdateRecordTypes := Value;
end;

{ ::TBDEDataSet }
function TKQueryItem.GetDatabase: TDatabase;
begin
	Result := FQuery.Database;
end;

function TKQueryItem.GetDBHandle: HDBIDB;
begin
	Result := FQuery.DBHandle;
end;

function TKQueryItem.GetDBLocale: TLocale;
begin
	Result := FQuery.DBLocale;
end;

function TKQueryItem.GetDBSession: TSession;
begin
	Result := FQuery.DBSession;
end;

function TKQueryItem.GetProvider: IProvider;
begin
	Result := FQuery.Provider;
end;

function TKQueryItem.GetDatabaseName: string;
begin
	Result := FQuery.DatabaseName;
end;

procedure TKQueryItem.SetDatabaseName( const Value: string );
begin
	FQuery.DatabaseName := Value;
end;

function TKQueryItem.GetSessionName: string;
begin
	Result := FQuery.SessionName;
end;

procedure TKQueryItem.SetSessionName( const Value: string );
begin
	FQuery.SessionName := Value;
end;

{ ::TBDEDataSet }
function TKQueryItem.GetPrepared: Boolean;
begin
	Result := FQuery.Prepared;
end;

procedure TKQueryItem.SetPrepare( Value: Boolean );
begin
	FQuery.Prepared := Value;
end;

function TKQueryItem.GetParamsCount: Word;
begin
	Result := FQuery.ParamCount;
end;

function TKQueryItem.GetLocal: Boolean;
begin
	Result := FQuery.Local;
end;

function TKQueryItem.GetStmtHandle: HDBIStmt;
begin
	Result := FQuery.StmtHandle;
end;

function TKQueryItem.GetText: string;
begin
	Result := FQuery.Text;
end;

function TKQueryItem.GetRowsAffected: Integer;
begin
	Result := FQuery.RowsAffected;
end;

function TKQueryItem.GetSQLBinary: PChar;
begin
	Result := FQuery.SQLBinary;
end;

procedure TKQueryItem.SetSQLBinary( Value: PChar );
begin
	FQuery.SQLBinary := Value;
end;

function TKQueryItem.GetConstrained: Boolean;
begin
	Result := FQuery.Constrained;
end;

procedure TKQueryItem.SetConstrained( Value: Boolean );
begin
	FQuery.Constrained := Value;
end;

function TKQueryItem.GetParamCheck: Boolean;
begin
	Result := FQuery.ParamCheck;
end;

procedure TKQueryItem.SetParamCheck( Value: Boolean );
begin
	FQuery.ParamCheck := Value;
end;

function TKQueryItem.GetParams: TParams;
begin
	Result := FQuery.Params;
end;

procedure TKQueryItem.SetParamsList( Value: TParams );
begin
	FQuery.Params := Value;
end;

function TKQueryItem.GetRequestLive: Boolean;
begin
	Result := FQuery.RequestLive;
end;

procedure TKQueryItem.SetRequestLive( Value: Boolean );
begin
	FQuery.RequestLive := Value;
end;

function TKQueryItem.GetSQL: TStrings;
begin
	Result := FQuery.SQL;
end;

procedure TKQueryItem.SetSQL( Value: TStrings );
begin
	FQuery.SQL := Value;
end;

function TKQueryItem.GetUniDirectional: Boolean;
begin
	Result := FQuery.UniDirectional;
end;

procedure TKQueryItem.SetUniDirectional( Value: Boolean );
begin
	FQuery.UniDirectional := Value;
end;

function TKQueryItem.GetUpdateMode: TUpdateMode;
begin
	Result := FQuery.UpdateMode;
end;

procedure TKQueryItem.SetUpdateMode( Value: TUpdateMode );
begin
	FQuery.UpdateMode := Value;
end;

{ end of private mappings }

{ Mapping from TDataSet to TKQueryItem }

function TKQueryItem.ActiveBuffer: PChar;
begin
	Result := FQuery.ActiveBuffer;
end;

procedure TKQueryItem.Append;
begin
	FQuery.Append;
end;

procedure TKQueryItem.AppendRecord( const Values: array of const );
begin
	FQuery.AppendRecord( Values );
end;

function TKQueryItem.BookmarkValid( Bookmark: TBookmark ): Boolean;
begin
	Result := FQuery.BookmarkValid( Bookmark );
end;

procedure TKQueryItem.Cancel;
begin
	FQuery.Cancel;
end;

procedure TKQueryItem.CheckBrowseMode;
begin
	FQuery.CheckBrowseMode;
end;

procedure TKQueryItem.ClearFields;
begin
	FQuery.ClearFields;
end;

procedure TKQueryItem.Close;
begin
	FQuery.Close;
end;

function TKQueryItem.ControlsDisabled: Boolean;
begin
	Result := FQuery.ControlsDisabled;
end;

function TKQueryItem.CompareBookmarks( Bookmark1, Bookmark2: TBookmark ): Integer;
begin
	Result := FQuery.CompareBookmarks( Bookmark1, Bookmark2 );
end;

function TKQueryItem.CreateBlobStream( Field: TField; Mode: TBlobStreamMode ): TStream;
begin
	Result := FQuery.CreateBlobStream( Field, Mode );
end;

procedure TKQueryItem.CursorPosChanged;
begin
	FQuery.CursorPosChanged;
end;

procedure TKQueryItem.Delete;
begin
	FQuery.Delete;
end;

procedure TKQueryItem.DisableControls;
begin
	FQuery.DisableControls;
end;

procedure TKQueryItem.Edit;
begin
	FQuery.Edit;
end;

procedure TKQueryItem.EnableControls;
begin
	FQuery.EnableControls;
end;

function TKQueryItem.FieldByName( const FieldName: string ): TField;
begin
	Result := FQuery.FieldByName( FieldName );
end;

function TKQueryItem.FindField( const FieldName: string ): TField;
begin
	Result := FQuery.FindField( FieldName );
end;

function TKQueryItem.FindFirst: Boolean;
begin
	Result := FQuery.FindFirst;
end;

function TKQueryItem.FindLast: Boolean;
begin
	Result := FQuery.FindLast;
end;

function TKQueryItem.FindNext: Boolean;
begin
	Result := FQuery.FindNext;
end;

function TKQueryItem.FindPrior: Boolean;
begin
	Result := FQuery.FindPrior;
end;

procedure TKQueryItem.First;
begin
	FQuery.First;
end;

procedure TKQueryItem.FreeBookmark( Bookmark: TBookmark );
begin
	FQuery.FreeBookmark( Bookmark );
end;

function TKQueryItem.GetBookmark: TBookmark;
begin
	Result := FQuery.GetBookmark;
end;

function TKQueryItem.GetCurrentRecord( Buffer: PChar ): Boolean;
begin
	Result := FQuery.GetCurrentRecord( Buffer );
end;

procedure TKQueryItem.GetFieldList( List: TList; const FieldNames: string );
begin
	FQuery.GetFieldList( List, FieldNames );
end;

procedure TKQueryItem.GetFieldNames( List: TStrings );
begin
	FQuery.GetFieldNames( List );
end;

procedure TKQueryItem.GotoBookmark( Bookmark: TBookmark );
begin
	FQuery.GotoBookmark( Bookmark );
end;

procedure TKQueryItem.Insert;
begin
	FQuery.Insert;
end;

procedure TKQueryItem.InsertRecord( const Values: array of const );
begin
	FQuery.InsertRecord( Values );
end;

function TKQueryItem.IsEmpty: Boolean;
begin
	Result := FQuery.IsEmpty
end;

function TKQueryItem.IsLinkedTo( DataSource: TDataSource ): Boolean;
begin
	Result := FQuery.IsLinkedTo( DataSource );
end;

function TKQueryItem.IsSequenced: Boolean;
begin
	Result := FQuery.IsSequenced;
end;

procedure TKQueryItem.Last;
begin
	FQuery.Last;
end;

function TKQueryItem.Locate( const KeyFields: string; const KeyValues: Variant;
	Options: TLocateOptions ): Boolean;
begin
	Result := FQuery.Locate( KeyFields, KeyValues, Options );
end;

function TKQueryItem.Lookup( const KeyFields: string; const KeyValues: Variant;
	const ResultFields: string ): Variant;
begin
	Result := FQuery.Lookup( KeyFields, KeyValues, ResultFields );
end;

function TKQueryItem.MoveBy( Distance: Integer ): Integer;
begin
	Result := FQuery.MoveBy( Distance );
end;

procedure TKQueryItem.Next;
begin
	FQuery.Next;
end;

procedure TKQueryItem.Open;
var
	i: Integer;
begin
	with Owner do
		if Component.ExclusiveMode then
			for i := 0 to Count - 1 do
				if ( Items[i] <> Self ) and ( Items[i].Active ) then
					Items[i].Close;
	FQuery.Open;
end;

procedure TKQueryItem.Post;
begin
	FQuery.Post;
end;

procedure TKQueryItem.Prior;
begin
	FQuery.Prior;
end;

procedure TKQueryItem.Resync( Mode: TResyncMode );
begin
	FQuery.Resync( Mode );
end;

procedure TKQueryItem.SetFields( const Values: array of const );
begin
	FQuery.SetFields( Values );
end;

procedure TKQueryItem.Translate( Src, Dest: PChar; ToOem: Boolean );
begin
	FQuery.Translate( Src, Dest, ToOem );
end;

procedure TKQueryItem.UpdateCursorPos;
begin
	FQuery.UpdateCursorPos;
end;

procedure TKQueryItem.UpdateRecord;
begin
	FQuery.UpdateRecord;
end;

{ Mapping from TBDEDataSet to TKQueryItem }

procedure TKQueryItem.ApplyUpdates;
begin
	FQuery.ApplyUpdates;
end;

procedure TKQueryItem.CancelUpdates;
begin
	FQuery.CancelUpdates;
end;

procedure TKQueryItem.CommitUpdates;
begin
	FQuery.CommitUpdates;
end;

function TKQueryItem.ConstraintCallBack( Req: DsInfoReq; var ADataSources: DataSources ): DBIResult;
begin
	Result := FQuery.ConstraintCallBack( Req, ADataSources );
end;

function TKQueryItem.ConstraintsDisabled: Boolean;
begin
	Result := FQuery.ConstraintsDisabled;
end;

procedure TKQueryItem.DisableConstraints;
begin
	FQuery.DisableConstraints;
end;

procedure TKQueryItem.EnableConstraints;
begin
	FQuery.EnableConstraints;
end;

procedure TKQueryItem.FetchAll;
begin
	FQuery.FetchAll;
end;

procedure TKQueryItem.FlushBuffers;
begin
	FQuery.FlushBuffers;
end;

procedure TKQueryItem.GetIndexInfo;
begin
	FQuery.GetIndexInfo;
end;

procedure TKQueryItem.RevertRecord;
begin
	FQuery.RevertRecord;
end;

function TKQueryItem.UpdateStatus: TUpdateStatus;
begin
	Result := FQuery.UpdateStatus;
end;

{ Mapping from TDBDataSet to TKQueryItem }

function TKQueryItem.OpenDatabase: TDatabase;
begin
	Result := FQuery.OpenDatabase;
end;

procedure TKQueryItem.CloseDatabase( Database: TDatabase );
begin
	FQuery.CloseDatabase( Database );
end;

function TKQueryItem.CheckOpen( Status: DBIResult ): Boolean;
begin
	Result := FQuery.CheckOpen( Status );
end;

{ Mapping from TQuery to TKQueryItem }

procedure TKQueryItem.ExecSQL;
begin
	FQuery.ExecSQL;
end;

function TKQueryItem.ParamByName( const Value: string ): TParam;
begin
	Result := FQuery.ParamByName( Value );
end;

procedure TKQueryItem.Prepare;
begin
	FQuery.Prepare;
end;

procedure TKQueryItem.UnPrepare;
begin
	FQuery.UnPrepare;
end;

{ Mapping from TKQuery to TKQueryItem }

procedure TKQueryItem.Refresh;
begin
	FQuery.Refresh;
end;

function TKQueryItem.ActionAllowed( Action: TKDataSetAction ): Boolean;
begin
	Result := FQuery.ActionAllowed( Action );
end;

function TKQueryItem.DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean;
begin
	Result := FQuery.DispatchShortCut( Key, Shift );
end;

procedure TKQueryItem.Update;
begin
	FQuery.Update;
end;

procedure TKQueryItem.ErrTransaction;
begin
	FQuery.ErrTransaction;
end;

procedure TKQueryItem.PushTransaction;
begin
	FQuery.PushTransaction;
end;

function TKQueryItem.GetActionKinds: TKDataSetActionKind;
begin
	Result := FQuery.ActionKinds;
end;

function TKQueryItem.GetAfterCommit: TDataSetNotifyEvent;
begin
	Result := FQuery.AfterCommit;
end;

function TKQueryItem.GetAfterDatasetAction: TKDataSetAfterActionEvent;
begin
	Result := FQuery.AfterDatasetAction;
end;

function TKQueryItem.GetAggregateDataSetActions: TKAggregateDataSetActions;
begin
	Result := FQuery.DataSetActions;
end;

function TKQueryItem.GetAuditory: Boolean;
begin
	Result := FQuery.Auditory;
end;

function TKQueryItem.GetAutoUpdate: Boolean;
begin
	Result := FQuery.AutoUpdate;
end;

function TKQueryItem.GetAutoCommit: Boolean;
begin
	Result := FQuery.AutoCommit;
end;

function TKQueryItem.GetBeforeCommit: TDataSetNotifyEvent;
begin
	Result := FQuery.BeforeCommit;
end;

function TKQueryItem.GetBeforeDatasetAction: TKDataSetBeforeActionEvent;
begin
	Result := FQuery.BeforeDatasetAction;
end;

function TKQueryItem.GetCancelOnRollback: Boolean;
begin
	Result := FQuery.CancelOnRollback;
end;

function TKQueryItem.GetConfirmDelete: Boolean;
begin
	Result := FQuery.ConfirmDelete;
end;

function TKQueryItem.GetCommitCacheCount: Integer;
begin
	Result := FQuery.CommitCacheCount;
end;

function TKQueryItem.GetCacheIntegrity: TKCacheIntegrity;
begin
	Result := FQuery.CacheIntegrity;
end;

function TKQueryItem.GetCommitting: Boolean;
begin
	Result := FQuery.Committing;
end;

function TKQueryItem.GetOverlappedTransaction: Boolean;
begin
	Result := FQuery.OverlappedTransaction;
end;

function TKQueryItem.GetLocalOverlappedDepth: Integer;
begin
	Result := FQuery.LocalOverlappedDepth;
end;

function TKQueryItem.GetGlobalOverlappedDepth: Integer;
begin
	Result := FQuery.GlobalOverlappedDepth;
end;

function TKQueryItem.GetOnOverlappedCommit: TKQueryOverlappedEvent;
begin
	Result := FQuery.OnOverlappedCommit;
end;

procedure TKQueryItem.SetOnOverlappedCommit( Value: TKQueryOverlappedEvent );
begin
	FQuery.OnOverlappedCommit := Value;
end;

function TKQueryItem.GetOnOverlappedEnd: TKQueryOverlappedEvent;
begin
	Result := FQuery.OnOverlappedEnd;
end;

procedure TKQueryItem.SetOnOverlappedEnd( Value: TKQueryOverlappedEvent );
begin
	FQuery.OnOverlappedEnd := Value;
end;

function TKQueryItem.GetOnOverlappedRollback: TKQueryOverlappedEvent;
begin
	Result := FQuery.OnOverlappedRollback;
end;

procedure TKQueryItem.SetOnOverlappedRollback( Value: TKQueryOverlappedEvent );
begin
	FQuery.OnOverlappedRollback := Value;
end;

function TKQueryItem.GetCurrentAction: TKDataSetAction;
begin
	Result := FQuery.CurrentAction;
end;

function TKQueryItem.GetDSActions: TKDataSetActions;
begin
	Result := FQuery.DSActions;
end;

function TKQueryItem.GetKeyFields: string;
begin
	Result := FQuery.KeyFields;
end;

function TKQueryItem.GetKKeyActions: TKKeyActions;
begin
	Result := FQuery.KeyActions;
end;

function TKQueryItem.GetOnRollBackChildren: TKDatasetRollbackEvent;
begin
	Result := FQuery.OnRollBackChildren;
end;

function TKQueryItem.GetOnUpdateChildren: TDataSetNotifyEvent;
begin
	Result := FQuery.OnUpdateChildren;
end;

function TKQueryItem.GetOnExecSQL: TDataSetNotifyEvent;
begin
	Result := FQuery.OnExecSQL;
end;

function TKQueryItem.GetOnKeyDown: TKDataSetKeyEvent;
begin
	Result := FQuery.OnKeyDown;
end;

function TKQueryItem.GetCommitCacheSize: Word;
begin
	Result := FQuery.CommitCacheSize;
end;

function TKQueryItem.GetCheckKeyAction: Boolean;
begin
	Result := FQuery.CheckKeyAction;
end;

function TKQueryItem.GetTransactionGroupIndex: Cardinal;
begin
	Result := FQuery.TransactionGroupIndex;
end;

function TKQueryItem.GetAfterChildrenApplyUpdates: TDatasetNotifyEvent;
begin
	Result := FQuery.AfterChildrenApplyUpdates;
end;

function TKQueryItem.GetBeforeChildrenApplyUpdates: TDatasetNotifyEvent;
begin
	Result := FQuery.BeforeChildrenApplyUpdates;
end;

function TKQueryItem.GetOnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent;
begin
	Result := FQuery.OnCheckCacheIntegrity;
end;

procedure TKQueryItem.SetAutoUpdate( const Value: Boolean );
begin
	FQuery.AutoUpdate := Value;
end;

procedure TKQueryItem.SetAutoCommit( const Value: Boolean);
begin
	FQuery.AutoCommit := Value;
end;

procedure TKQueryItem.SetActionKinds( const Value: TKDataSetActionKind );
begin
	FQuery.ActionKinds := Value;
end;

procedure TKQueryItem.SetAfterCommit( const Value: TDataSetNotifyEvent );
begin
	FQuery.AfterCommit := Value;
end;

procedure TKQueryItem.SetAfterDatasetAction( const Value: TKDataSetAfterActionEvent );
begin
	FQuery.AfterDatasetAction := Value;
end;

procedure TKQueryItem.SetAggregateDataSetActions( const Value: TKAggregateDataSetActions );
begin
	FQuery.DataSetActions := Value;
end;

procedure TKQueryItem.SetAuditory( const Value: Boolean );
begin
	FQuery.Auditory := Value;
end;

procedure TKQueryItem.SetBeforeCommit( const Value: TDataSetNotifyEvent );
begin
	FQuery.BeforeCommit := Value;
end;

procedure TKQueryItem.SetBeforeDatasetAction( const Value: TKDataSetBeforeActionEvent );
begin
	FQuery.BeforeDatasetAction := Value;
end;

procedure TKQueryItem.SetCancelOnRollback( const Value: Boolean );
begin
	FQuery.CancelOnRollBack := Value;
end;

procedure TKQueryItem.SetConfirmDelete( const Value: Boolean );
begin
	FQuery.ConfirmDelete := Value;
end;

procedure TKQueryItem.SetDSActions( const Value: TKDataSetActions );
begin
	FQuery.DSActions := Value;
end;

procedure TKQueryItem.SetKeyActions( const Value: TKKeyActions );
begin
	FQuery.KeyActions := Value;
end;

procedure TKQueryItem.SetKeyFields( const Value: string );
begin
	FQuery.KeyFields := Value;
end;

procedure TKQueryItem.SetOnExecSQL( const Value: TDataSetNotifyEvent );
begin
	FQuery.OnExecSQL := Value;
end;

procedure TKQueryItem.SetOnKeyDown( const Value: TKDataSetKeyEvent );
begin
	FQuery.OnKeyDown := Value;
end;

procedure TKQueryItem.SetOnRollBackChildren( const Value: TKDataSetRollbackEvent );
begin
	FQuery.OnRollBackChildren := Value;
end;

procedure TKQueryItem.SetOnUpdateChildren( Value: TDatasetNotifyEvent );
begin
	FQuery.OnUpdateChildren := Value;
end;

procedure TKQueryItem.SetCommitCacheSize( const Value: Word );
begin
	FQuery.CommitCacheSize := Value;
end;

procedure TKQueryItem.SetCheckKeyAction( Value: Boolean );
begin
	FQuery.CheckKeyAction := Value;
end;

procedure TKQueryItem.SetTransactionGroupIndex( Value: Cardinal );
begin
	FQuery.TransactionGroupIndex := Value;
end;

procedure TKQueryItem.SetAfterChildrenApplyUpdates( Value: TDatasetNotifyEvent );
begin
	FQuery.AfterChildrenApplyUpdates := Value;
end;

procedure TKQueryItem.SetBeforeChildrenApplyUpdates( Value: TDatasetNotifyEvent );
begin
	FQuery.BeforeChildrenApplyUpdates := Value;
end;

procedure TKQueryItem.SetOnCheckCacheIntegrity( Value: TKDatasetCacheIntegrityEvent );
begin
	FQuery.OnCheckCacheIntegrity := Value;
end;

{------------------------------ TKQueryCollection ------------------------------}


constructor TKQueryCollection.Create( AOwner: TKSQLScript );

begin
	ForceObject( AOwner );
	inherited Create( AOwner, TKQueryItem, false );
end;

function TKQueryCollection.GetItem( Index: Integer ): TKQueryItem;
begin
	Result := TKQueryItem( inherited GetItem( Index ) );
end;

procedure TKQueryCollection.SetItem( Index: Integer; Value: TKQueryItem );
begin
	inherited SetItem( Index, Value );
end;

function TKQueryCollection.Add: TKQueryItem;
begin
	Result := TKQueryItem( inherited Add );
end;

function TKQueryCollection.GetItemByName( const AName: string ): TKQueryItem;
begin
	Result := TKQueryItem( inherited GetItemByName( AName ) );
end;

function TKQueryCollection.GetOwnerComp: TKSQLScript;
begin
 Result := TKSQLScript( inherited GetOwnerComp );
end;

class function TKQueryCollection.GetDefaultItemName( Item: TKCustomCollectionItem ): string;
begin
	Result := Copy( inherited GetDefaultItemName( Item ), 3, SizeOf( ShortString ) );
end;

procedure TKQueryCollection.Update( Item: TCollectionItem );
begin
	if CheckObject( Item ) then
		Component.UpdateItem( TKQueryItem( Item ) )
	else
		Component.UpdateItems; { Hard Couple! }
end;

{--------------------------------- TKSQLScript ---------------------------------}


constructor TKSQLScript.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FExclusiveMode := false;
	FQueryCollection := TKQueryCollection.Create( Self );
//	SQLList.Add( Self );
end;

destructor TKSQLScript.Destroy;
begin
	FQueryCollection.Free;
//	SQLList.Remove( Self );
	inherited Destroy;
end;
                      
procedure TKSQLScript.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKSQLScript ) then
	begin
	  Tag := ( Source as TKSQLScript ).Tag;
		ExclusiveMode := ( Source as TKSQLScript ).ExclusiveMode;
		QueryCollection := ( Source as TKSQLScript ).QueryCollection;			  
	end
	else
		inherited Assign( Source );
end;

procedure TKSQLScript.Loaded;
var
	i: Integer;
begin
	inherited Loaded;
	with FQueryCollection do
		for i := 0 to Count - 1 do
			with Items[i] do
				if FActive then
					Query.Open;
end;

procedure TKSQLScript.Notification( AComponent: TComponent; Operation: TOperation );
var
	i: Integer;
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) then
		with FQueryCollection do
			for i := 0 to Count - 1 do
				if ( Items[i].LinkTo = AComponent ) then
					Items[i].LinkTo := nil;
end;

function TKSQLScript.GetQueries( Index: string ): TQuery;
begin
	Result := QueryByName( Index );
end;

procedure TKSQLScript.SetQueryCollection( Value: TKQueryCollection );
begin
	FQueryCollection.Assign( Value );
end;

procedure TKSQLScript.UpdateItems;
var
	i: Integer;
begin
	for i := 0 to FQueryCollection.Count - 1 do
		UpdateItem( FQueryCollection.Items[i] );
end;

procedure TKSQLScript.UpdateItem( Item: TKQueryItem );
begin
	{ Default update item code, do nothing }
end;

procedure TKSQLScript.RunScript;
begin
	RunScriptEx( 0, FQueryCollection.Count - 1 );
end;

procedure TKSQLScript.RunScriptEx( StartPos, EndPos: Word );
var
	i: Integer;
begin
	if ( StartPos > EndPos ) or ( EndPos >= FQueryCollection.Count ) then
		RaiseExceptionFmt( EKSQLScript, sErrSSInvScriptRange, [StartPos, EndPos] );
	with FQueryCollection do
		for i := StartPos to EndPos do
			if ( Items[i].ExecMode = emOpen ) then
			begin
				if ( not FExclusiveMode	) then
					Items[i].Refresh;
			end
			else
				Items[i].ExecSQL;
end;

function TKSQLScript.FindQuery( const AQuery: string ): TQuery;
var
	i: Integer;
begin
	Result := nil;
	with FQueryCollection do
		for i := 0 to Count - 1 do
			if CheckStrEqual( Items[i].QueryName, AQuery ) then
			begin
				Result := Items[i].Query;
				Break;
			end;
end;

function TKSQLScript.QueryByName( const AQuery: string ): TQuery;
begin
	Result := FindQuery( AQuery );
	if ( not CheckObject( Result ) ) then
		RaiseExceptionFmt( EKSQLScript, sErrSSInvQueryName, [AQuery] );
end;

procedure TKSQLScript.RunTransactionOn( ADatabase: TDatabase );
begin
	RunTransactionOnEx( ADatabase, 0, FQueryCollection.Count - 1 );
end;

procedure TKSQLScript.RunTransactionOnEx( ADatabase: TDatabase; StartPos, EndPos: Word );
var
	i: Integer;
begin
	if ( not CheckObject( ADatabase ) ) then
		Exit;
	if ( StartPos > EndPos ) or ( EndPos >= FQueryCollection.Count ) then
		RaiseExceptionFmt( EKSQLScript, sErrSSInvScriptRange, [StartPos, EndPos] );
	ADatabase.StartTransaction;
	try
		with QueryCollection do
			for i := StartPos to EndPos do
				with Items[i] do
					if ( ADatabase = Database ) and ( ExecMode = emExecSQL ) then
						ExecSQL;
		ADatabase.Commit;
	except
		ADatabase.Rollback;
	end;
end;

procedure TKSQLScript.LoadItemsFromStream( Stream: TStream );
var
	SQLS: TKSQLScript;
begin
{ We must explicity create a new instance an pass it to Stream because the Self
	assignment causes some naming reference problems on the SQLScript Owner }
	SQLS := TKSQLScript.Create( nil );
	try
		Stream.ReadComponent( SQLS );
		Assign( SQLS );
	finally
		SQLS.Free;
	end;
end;

procedure TKSQLScript.SaveItemsToStream( Stream: TStream );
begin
	Stream.WriteComponent( Self );
end;

procedure TKSQLScript.LoadItemsFromFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead );
	try
		LoadItemsFromStream( Stream );
	finally
		Stream.Free;
	end;
end;

procedure TKSQLScript.SaveItemsToFile( const FileName: string );
var
	Stream: TStream;
begin
	ForceTrimStr( FileName );
	ForceDeleteFile( FileName );
	Stream := TFileStream.Create( FileName, fmCreate );
	try
		SaveItemsToStream( Stream );
	finally
		Stream.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Extended SQL Script Classes --------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

{ TKDBAnsiBuildEx }

	TKDBAnsiBuildEx = class( TKDBAnsiSQLBuild )
	private
		function GetOwnerObject: TKSQLScriptEx;

	protected
		procedure ClearSQLScript; override;
		function GetScriptCleared: Boolean; override;

	public
		property Owner: TKSQLScriptEx
						 read GetOwnerObject;
	end;

{ TKDBLocalSQLBuildEx }

	TKDBLocalSQLBuildEx = class( TKDBLocalSQLBuild )
	private
		function GetOwnerObject: TKSQLScriptEx;

	protected
		procedure ClearSQLScript; override;
		function GetScriptCleared: Boolean; override;

	public
		property Owner: TKSQLScriptEx
						 read GetOwnerObject;
	end;

{ TKDBMSSQLBuildEx }

	TKDBMSSQLBuildEx = class( TKDBMSSQLBuild )
	private
		function GetOwnerObject: TKSQLScriptEx;

	protected
		procedure ClearSQLScript; override;
		function GetScriptCleared: Boolean; override;

	public
		property Owner: TKSQLScriptEx
						 read GetOwnerObject;
	end;

{ TKDBInterBSQLBuildEx }

	TKDBInterBSQLBuildEx = class( TKDBInterBSQLBuild )
	private
		function GetOwnerObject: TKSQLScriptEx;

	protected
		procedure ClearSQLScript; override;
		function GetScriptCleared: Boolean; override;

	public
		property Owner: TKSQLScriptEx
						 read GetOwnerObject;
	end;

{ TKDBAnsiBuildEx }

function TKDBAnsiBuildEx.GetScriptCleared: Boolean;
begin
	Result := inherited GetScriptCleared;
	Owner.FScriptCleared := Result; {Hard Couple!}
end;

procedure TKDBAnsiBuildEx.ClearSQLScript;
begin
	inherited ClearSQLScript;
	Owner.FScriptCleared := True; {Hard Couple!}
end;

function TKDBAnsiBuildEx.GetOwnerObject: TKSQLScriptEx;
begin
	Result := TKSQLScriptEx( inherited GetOwnerObject );
end;

{ TKDBLocalSQLBuildEx }

function TKDBLocalSQLBuildEx.GetScriptCleared: Boolean;
begin
	Result := inherited GetScriptCleared;
	Owner.FScriptCleared := Result; {Hard Couple!}
end;

procedure TKDBLocalSQLBuildEx.ClearSQLScript;
begin
	inherited ClearSQLScript;
	Owner.FScriptCleared := True; {Hard Couple!}
end;

function TKDBLocalSQLBuildEx.GetOwnerObject: TKSQLScriptEx;
begin
	Result := TKSQLScriptEx( inherited GetOwnerObject );
end;

{ TKDBMSSQLBuildEx }

function TKDBMSSQLBuildEx.GetScriptCleared: Boolean;
begin
	Result := inherited GetScriptCleared;
	Owner.FScriptCleared := Result; {Hard Couple!}
end;

procedure TKDBMSSQLBuildEx.ClearSQLScript;
begin
	inherited ClearSQLScript;
	Owner.FScriptCleared := True; {Hard Couple!}
end;

function TKDBMSSQLBuildEx.GetOwnerObject: TKSQLScriptEx;
begin
	Result := TKSQLScriptEx( inherited GetOwnerObject );
end;

{ TKDBLocalSQLBuildEx }

function TKDBInterBSQLBuildEx.GetScriptCleared: Boolean;
begin
	Result := inherited GetScriptCleared;
	Owner.FScriptCleared := Result; {Hard Couple!}
end;

procedure TKDBInterBSQLBuildEx.ClearSQLScript;
begin
	inherited ClearSQLScript;
	Owner.FScriptCleared := True; {Hard Couple!}
end;

function TKDBInterBSQLBuildEx.GetOwnerObject: TKSQLScriptEx;
begin
	Result := TKSQLScriptEx( inherited GetOwnerObject );
end;

const
	SQLBUILDEX_DB_CLASS_MAP: array[TKSQLScriptType] of TKDBAnsiSQLBuildClass =
	(
    nil, 
		TKDBAnsiBuildEx,      { sstAnsi    }
		TKDBMSSQLBuildEx,     { sstMSSql   }
		TKDBInterBSQLBuildEx, { sstIntB    }
		TKDBLocalSQLBuildEx	  { sstLocal   }
	);

{---------------------------- Public Implementation ----------------------------}

constructor TKSQLScriptEx.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSQLScriptType := sstLocal;
	FDBSQLB := SQLBUILDEX_DB_CLASS_MAP[FSQLScriptType].CreateOwned( Self, sstLocal );
  FDBSQLB.SystemTables := False;
  FDBSQLB.Extensions := True;
  TKDBLocalSQLBuildEx( FDBSQLB ).OnBuild := BuildEvent;
  FScriptCleared := True;
  FAutomaticBuildItems := True;
end;

destructor TKSQLScriptEx.Destroy;
begin
	FDBSQLB.Free;
	inherited Destroy;
end;

procedure TKSQLScriptEx.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKSQLScriptEx ) then
		with ( Source as TKSQLScriptEx ) do
		begin
			Self.SessionName   := SessionName;
			Self.DataBaseName  := DataBaseName;
			Self.TablePatterns := TablePatterns;
			Self.SystemTables  := SystemTables;
			Self.SQLScriptType := SQLScriptType; 
			Self.Extensions    := Extensions;
			Self.OnTableBuild  := OnTableBuild;
			Self.OnIndexBuild  := OnIndexBuild;
		end;
end;

procedure TKSQLScriptEx.SetInfo( Index: Integer; const Value: string );
begin
	case Index of
		0: FDBSQLB.DataBaseName := Value;
		1: FDBSQLB.SessionName := Value;
		2: FDBSQLB.TablePatterns := Value;
	end;
end;

function TKSQLScriptEx.GetInfo( Index: Integer ): string;
begin
	case Index of
		0: Result := FDBSQLB.DataBaseName;
		1: Result := FDBSQLB.SessionName;
		2: Result := FDBSQLB.TablePatterns;
		3:
		begin
		  AdjustSQLBuildClass;
			if FDBSQLB.ScriptCleared then
				FScript := ''
			else
				FScript := FDBSQLB.SQLScript;
			Result := FScript;
    end;
	else
    Result := '';
  end;
end;

function TKSQLScriptEx.GetBoolInfo( Index: Integer ): Boolean;
begin
  case Index of
		0: Result := FDBSQLB.SystemTables;
    1: Result := FDBSQLB.Extensions;
    2: Result := FAutomaticBuildItems;
	else
		Result := False;
	end;
end;

procedure TKSQLScriptEx.SetBoolInfo( Index: Integer; const Value: Boolean );
begin
	case Index of
		0: FDBSQLB.SystemTables := Value;
		1: FDBSQLB.Extensions := Value;
		2: FAutomaticBuildItems := Value;
	end;
end;

procedure TKSQLScriptEx.BuildEvent( Sender: TKCustomSQLBuild;
	IteratorID, SQLStringsID: Integer; SQLBuildType: TKSQLBuildType );
begin
	BuildQueryItem( Sender, IteratorID, SQLStringsID, SQLBuildType );
	case SQLBuildType of
		sbtTable:
			if Assigned( FKSQLScritTableBuildEvent ) then
				FKSQLScritTableBuildEvent( Self, ( Sender as TKDBLocalSQLBuild ), IteratorID );
		sbtIndex:
			if Assigned( FKSQLScritIndexBuildEvent ) then
				FKSQLScritIndexBuildEvent( Self, ( Sender as TKDBLocalSQLBuild ), IteratorID );
	end;
end;

procedure TKSQLScriptEx.BuildQueryItem( Sender: TKCustomSQLBuild; IteratorID,
	SQLStringsID: Integer; SQLBuildType: TKSQLBuildType );
begin
	with QueryCollection.Add, Sender do
	begin
		ExecMode := emExecSQL;
		SessionName := Self.SessionName;
		DatabaseName := Self.DataBaseName;
		case SQLBuildType of
			sbtTable: SQL.Text := Tables.NormalizeSQL( Tables.SubScripts[SQLStringsID] );
			sbtIndex: SQL.Text := Indexes.NormalizeSQL( Indexes.SubScripts[SQLStringsID] );
		end;
	end;
end;

procedure TKSQLScriptEx.ScriptSections( const Section: string );
begin
	with QueryCollection.Add do
	begin
		ExecMode := emExecSQL;
		SessionName := Self.SessionName;
		DatabaseName := Self.DataBaseName;
		SQL.Text := Section;
	end;
end;

procedure TKSQLScriptEx.CheckSQLScriptForBuildScript;
begin
	if ( not ( CheckTrimStr( SessionName ) and CheckTrimStr( DataBaseName ) ) ) then
		RaiseException( EKSQLScript, sErrSSEInvBuildInfos );
end;

procedure TKSQLScriptEx.AdjustSQLBuildClass;
var
	TempSQLBuild: TKDBAnsiSQLBuild;
begin
	if ( FDBSQLB.SQLScriptType <> SQLScriptType ) and ( SQLScriptType <> sstUnknown ) then
	begin
		TempSQLBuild := SQLBUILDEX_DB_CLASS_MAP[FSQLScriptType].CreateOwned( Self, SQLScriptType );
		try
			TempSQLBuild.Assign( FDBSQLB );
      FreeClean( FDBSQLB );
			FDBSQLB := TempSQLBuild;
		except
      TempSQLBuild.Free;
			raise;
		end;
	end;
end;

procedure TKSQLScriptEx.BuildScript;
begin
	CheckSQLScriptForBuildScript;
  AdjustSQLBuildClass;
	FDBSQLB.ClearSQLScript;
	FScript := FDBSQLB.SQLScript;
	if Designing( Self ) then
		MarkDesigner( Self );
end;

procedure TKSQLScriptEx.LoadScriptsFromStream( Stream: TStream );
var
	sScript,
	sDBName: string;
	iPos1,
	iPos2: Integer;
	ss: TKStringStream;
begin
	ss := TKStringStream.Create( '' );
	try
		ForceStreamCopy( Stream, ss );
		FDBSQLB.ClearSQLScript;
		sScript := ss.DataString;
		iPos1 := Pos( SQLBUILD_ENVVAR_DBNAME_TOKEN, sScript )+
			Length( SQLBUILD_ENVVAR_DBNAME_TOKEN );
		iPos2 := Pos( '*/'#13#10#13#10, Copy( sScript, iPos1, Length( sScript ) - iPos1 ) ) - 2;
		sDBName := Trim( Copy( sScript, iPos1, iPos2 ) );
		DataBaseName := '';
		GetScriptSections( sScript, ScriptSections );
		try
      DatabaseName := sDBName;
			NormalizeDatabaseName;
    except
      on E: EDatabaseError do
				RaiseExceptionFmt( EKSQLScript, sErrSSEInvDBNameLoading, [sDBName] );
    end;
    FScript := sScript;
		FScriptCleared := False;
  finally
    ss.Free;
  end;
end;

procedure TKSQLScriptEx.LoadScriptsFromFile( const FileName: string );
var
  Stream: TStream;
begin
  ForceFile( FileName );
	Stream := TFileStream.Create( FileName, fmOpenRead );
  try
    LoadScriptsFromStream( Stream );
  finally
    Stream.Free;
  end;
end;

procedure TKSQLScriptEx.SaveScriptsToStream( Stream: TStream );
var
	s1,
	s2: string;
	i: Integer;
begin
	s1 := FDBSQLB.BuildHeader;
	s2 := FDBSQLB.BuildFooter;
	s1 := StringReplace( s1, SQLBUILD_ENVVAR_FOOTER, s2, krfAll );
	s2 := Format( SQLBUILD_ENVVAR_DBNAME, [DataBaseName] );
	s1 := StringReplace( s1, SQLBUILD_ENVVAR_DBNAME, s2 , krfAll );
	s2 := '';
	for i := 0 to QueryCollection.Count - 1 do
		s2 := s2 + '/*' + SQLBUILD_ENVVAR_START + '*/'#13#10 +
			QueryCollection.Items[i].SQL.Text + '/*' + SQLBUILD_ENVVAR_RUN + '*/'#13#10#13#10;
	s1 := StringReplace( s1, SQLBUILD_ENVVAR_TABLES, s2, krfAll );
	s1 := StringReplace( s1, SQLBUILD_ENVVAR_INDEXES, '', krfAll );
  s1 := StringReplace( s1, SQLBUILD_ENVVAR_START, SQLBUILD_START_COMMAND, krfAll );
	s1 := StringReplace( s1, SQLBUILD_ENVVAR_RUN, SQLBUILD_RUN_COMMAND, krfAll );
	if CheckStr( s1 ) then
    Stream.WriteBuffer( Pointer( s1 )^, Length( s1 ) );
end;

procedure TKSQLScriptEx.SaveScriptsToFile( const FileName: string );
var
	Stream: TStream;
	sName: string;
begin
	sName := GetFirstString( [FileName, ( ExtractFilePath( Application.EXEName ) +
		DataBaseName + SQL_DEFAULT_FILE_EXT ), ApplicationLikeName( SQL_DEFAULT_FILE_EXT )] );
	ForceDeleteFile( sName );
	Stream := TFileStream.Create( sName, fmCreate );
  try
    SaveScriptsToStream( Stream );
  finally
    Stream.Free;
  end;
end;

procedure TKSQLScriptEx.NormalizeDatabaseName;
var
	i: Integer;
begin
	for i := 0 to QueryCollection.Count - 1 do
	begin
		QueryCollection.Items[i].SessionName := SessionName;
		QueryCollection.Items[i].DatabaseName := DataBaseName;
	end;
end;

procedure TKSQLScriptEx.CheckSQLScriptForCreateDataBase( const DestDBName: string );
begin
	CheckSQLScriptForBuildScript;
	ForceDataBaseName( DestDBName ); 
	if CheckStrEqual( DataBaseName, DestDBName ) then
		RaiseException( EKSQLScript, sErrSSEInvCreateDBName );
end;

procedure TKSQLScriptEx.CreateDataBase( const DestDBName: string );
var
  TempDBName: string;
begin
  CheckSQLScriptForCreateDataBase( DestDBName );
  if ( QueryCollection.Count = 0 ) then
    BuildScript;
	TempDBName := DataBaseName;
	try
		DataBaseName := DestDBName;
		NormalizeDatabaseName;
		RunScript;
	finally
		DataBaseName := TempDBName;
		NormalizeDatabaseName;
	end;
end;

procedure PrintSQLScriptsEx( SQLScript: TKSQLScript; StartPos, EndPos: Word );

	function DupStr( ch: Char; Count: Byte ): ShortString;
	var
		i: Integer;
	begin
		Result := '';
		for i := 1 to Count do
			Result := Result + ch;
	end;

	procedure Normalize( ss: TStringList );
	var
		i,
		iPos: Integer;
		s: ShortString;
	begin
		for i := 0 to ss.Count - 1 do
		begin
			iPos := Pos( CH_TAB, ss[i] );
			while ( iPos > 0 ) do
			begin
				s := ss[i];
				Delete( s, iPos, 1 );
				Insert( '   ', s, iPos );
				ss[i] := s;
				iPos := Pos( CH_TAB, ss[i] );
			end;
		end;
	end;

var
	i,
	j,
	iP,
	iX,
	iY,
	iPos,
	iDeltaY: Integer;
	s: string;
	ss: TStringList;
begin
	if ( not ValueBetween( StartPos, 0, EndPos, True ) ) then
		Exit;
	ForceObject( SQLScript );
	with SQLScript do
	begin
    ss := TStringList.Create;
    try
      ss.Add( '' );
      with QueryCollection do
        for i := StartPos to EndPos do
        begin
          s := 'SQL for ' + Items[i].Name;
          ss.Add( s );
          ss.Add( DupStr( '¯', Length( s ) ) );
          for j := 0 to ( Items[i].SQL.Count - 1 ) do
            ss.Add( Items[i].SQL[j] );
          ss.Add( '' );
        end;
			Normalize( ss );
      iX := 250;
      iY := 400;
			Printer.BeginDoc;
			try
        with Printer.Canvas do
        begin
          iDeltaY := Trunc( 1.2 * TextHeight( 'Wg' ) );
          Font.Name := 'Courier New';
          Font.Size := 9;
          Font.Style := [fsBold];
          i := 0;
          iP := 1;
          iPos := iY;
          while ( i <= ss.Count - 1 ) do
          begin
            if ( iPos = iY ) then
            begin
              s := Format( '%s - page %d', [Name, iP] );
              TextOut( iX, iPos, s );
              inc( iPos, iDeltaY );
              TextOut( iX, iPos, DupStr( '¯', Length( s ) ) );
              inc( iPos, iDeltaY );
						end;
						TextOut( iX, iPos, ss[i] );
            inc( iPos, iDeltaY );
            inc( i );
            if ( iPos > ( Printer.PageHeight - iY ) ) then
            begin
              Printer.NewPage;
              iPos := iY;
              inc( iP );
            end;
          end;
        end;
      finally
        Printer.EndDoc;
      end;
    finally
      ss.Free;
    end;
  end;
end;

procedure PrintSQLScripts( SQLScript: TKSQLScript );
begin
  ForceObject( SQLScript );
	PrintSQLScriptsEx( SQLScript, 0, SQLScript.QueryCollection.Count - 1 );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterClasses(
	[
		TStringField,
		TSmallintField,
		TIntegerField,
		TWordField,
		TBooleanField,
		TFloatField,
		TCurrencyField,
		TBCDField,
		TDateField,
		TTimeField,
		TDateTimeField,
		TBytesField,
		TVarBytesField,
		TAutoIncField,
		TBlobField,
		TMemoField,
		TGraphicField
	] );
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
