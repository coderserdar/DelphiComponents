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

unit ukdbTables;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	SysUtils, Windows, Messages, Classes, Controls, Forms, Menus,
	DB, DBTables, uksyShortCuts, uksyUtils, uksyClasses, ukrConsts,
	ukrClasses, ukdbConsts, ukdbClasses, ukdbEngines;

type

	EKDBTables = class( EKDB );

const
	loKeyAction = 0;
	hiKeyAction = 11;

{
--------------------------------------------------------------------------------
-------------------- General Referencial Integrity Classes ---------------------
--------------------------------------------------------------------------------
}

type

	EKDBIntegrity = class( EKDBTables );

	TKCustomDBIntegrity = class;

{ TKCustomDBIntegrityItem }

	TKCustomDBIntegrityItem = class( TKCustomCollectionItem )
	private
		FDataBaseName: string;
		FLinkedFields: TKStrings;

		procedure ForceCollectionClass( ACollection: TCollection );

	protected
		function GetOwnerCollection: TKCustomDBIntegrity;
		function GetLinkedFields: TKStrings; virtual;
		procedure SetLinkedFields( const Value: TKStrings ); virtual;

	{$IFDEF DELPHI4}
	public
	{$ENDIF}
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

	public
		constructor Create( ACollection: TCollection ); override;
		destructor Destroy; override;
		procedure Assign( Source: TPersistent ); override;

		property Owner: TKCustomDBIntegrity
						 read GetOwnerCollection;

	published
		property Name;
		property DataBaseName: string
						 read FDataBaseName write FDataBaseName;
		property LinkedFields: TKStrings
						 read GetLinkedFields write SetLinkedFields;

	end;

	TKCustomDBIntegrityItemClass = class of TKCustomDBIntegrityItem;

{ TKCustomDBIntegrity }

	TKCustomDBIntegrity = class( TKCustomCollection )
	protected
		procedure SetItem( Index: Integer; const AItem: TKCustomDBIntegrityItem );
		function GetItem( Index: Integer ): TKCustomDBIntegrityItem;
		function GetItemByName( const AName: string ): TKCustomDBIntegrityItem;

		function GetOwnerComp: TDBDataset;
		function Add: TKCustomDBIntegrityItem;

		property ItemByName[const AName: string]: TKCustomDBIntegrityItem
						 read GetItemByName;
		property Items[Index: Integer]: TKCustomDBIntegrityItem
						 read GetItem write SetItem;

	public
		constructor Create( ADataset: TDBDataset;
			AItemClass: TKCustomDBIntegrityItemClass );

		property Dataset: TDBDataset
						 read GetOwnerComp;

	end;

{
--------------------------------------------------------------------------------
------------------ General KTable Referencial Integrity Classes ----------------
--------------------------------------------------------------------------------
}

	TKTable = class;
	TKTableDBIntegrity = class;
	TKTableDBIntegrityItem = class;

	TKQueryAction = ( qaOpen, qaExecSQL, qaNone );

	TKOnEvaluateSQL = procedure ( Sender: TKTable; Item: TKTableDBIntegrityItem;
		EvalSQL: TStrings ) of object;

{ TKTableDBIntegrityItem } 

	TKTableDBIntegrityItem = class( TKCustomDBIntegrityItem )
	private
		FTableName: TFileName;
		FOnEvaluateSQL: TKOnEvaluateSQL;

		procedure SetTableName( const Value: TFileName );
		function GetOwnerCollection: TKTableDBIntegrity;
		procedure PerformQueryAction;
		procedure FillQuery;

	protected
		function GetDefaultSQLStatement: string; virtual;
		function GetEvaluationSQL: TStrings; dynamic;
		procedure DoOnEvaluateSQL( EvalSQL: TStrings ); virtual;
		function PerformItemIntegrity: Boolean; virtual; abstract;
		function QueryAction: TKQueryAction; dynamic;

		property OnEvaluateSQL: TKOnEvaluateSQL
						 read FOnEvaluateSQL write FOnEvaluateSQL;

	public
		constructor Create( ACollection: TCollection ); override;
		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property Owner: TKTableDBIntegrity
						 read GetOwnerCollection;

	published
		property TableName: TFileName
						 read FTableName write SetTableName;

	end;

	TKIntegrityError = ( ieSkip, ieAbort, ieFail );

	TKOnIntegrityError = procedure ( Sender: TKTable; Item: TKTableDBIntegrityItem;
		var IntegrityError: TKIntegrityError ) of object;

{ TKTableDBIntegrity }

	TKTableDBIntegrity = class( TKCustomDBIntegrity )
	private
		FEvaluate: TQuery;
		FOnEvaluateSQL: TKOnEvaluateSQL;
		FOnIntegrityError: TKOnIntegrityError;

		function GetItemByName( const AName: string ): TKTableDBIntegrityItem;
		function GetOwnerComp: TKTable;

	protected
		procedure Update( Item: TCollectionItem ); override;

		procedure SetItem( Index: Integer; const AItem: TKTableDBIntegrityItem );
		function GetItem( Index: Integer ): TKTableDBIntegrityItem;

		procedure DoOnIntegrityError( Item: TKTableDBIntegrityItem ); dynamic;
		class function GetDefaultItemName( Item: TKCustomCollectionItem ): string; override;

		property Items[Index: Integer]: TKTableDBIntegrityItem
						 read GetItem write SetItem;
		property ItemByName[const AName: string]: TKTableDBIntegrityItem
						 read GetItemByName;

		property OnIntegrityError: TKOnIntegrityError
						 read FOnIntegrityError write FOnIntegrityError;

	public
		destructor Destroy; override;
		constructor CreateLinked( ADataset: TKTable; AItemClass: TKCustomDBIntegrityItemClass;
			AEvent: TKOnEvaluateSQL ); virtual;

		function Add: TKTableDBIntegrityItem;
		procedure PerformIntegrity; dynamic;

		property Dataset: TKTable
						 read GetOwnerComp;

	end;

{
--------------------------------------------------------------------------------
----------------------- Table Referencial Integrity Classes --------------------
--------------------------------------------------------------------------------
}

{ TKProhibitItem }

	TKProhibit = class;

	TKProhibitItem = class( TKTableDBIntegrityItem )
	protected
		function GetDefaultSQLStatement: string; override;
		function PerformItemIntegrity: Boolean; override;

	published
		property OnEvaluateSQL;

	end;

{ TKProhibit }

	TKProhibit = class( TKTableDBIntegrity )
	private
		procedure SetItem( Index: Integer; const AItem: TKProhibitItem );
		function GetItem( Index: Integer ): TKProhibitItem;

	public
		constructor Create( ATable: TKTable );
		function Add: TKProhibitItem; virtual;

		property Items[Index: Integer]: TKProhibitItem
						 read GetItem write SetItem; default;
		property ItemByName;
		property Names;

	end;

{ TKCascadeItem }

	TKCascade = class;

	TKCascadeItem = class( TKTableDBIntegrityItem )
	private
		FCheckDeletion: Boolean;
		FAfterDeletion: TNotifyEvent;
		FInternalCheck: Byte;

		function InternalCheckDeletion: Boolean;

	protected
		function GetDefaultSQLStatement: string; override;
		function PerformItemIntegrity: Boolean; override;
		procedure DoAfterDeletion; dynamic;

	public
		constructor Create( ACollection: TCollection ); override;

	published
		property CheckDeletion: Boolean
						 read FCheckDeletion write FCheckDeletion default true;

		property AfterDeletion: TNotifyEvent
						 read FAfterDeletion write FAfterDeletion;
		property OnEvaluateSQL;

	end;

{ TKCascade }

	TKCascade = class( TKTableDBIntegrity )
	private
		procedure SetItem( Index: Integer; const AItem: TKCascadeItem );
		function GetItem( Index: Integer ): TKCascadeItem;

	public
		constructor Create( ATable: TKTable ); 

		procedure AdjustLinkedFieldsForTransaction;
		function Add: TKCascadeItem; virtual;

		property Items[Index: Integer]: TKCascadeItem
						 read GetItem write SetItem; default;
		property ItemByName;
		property Names;

	end;

{
--------------------------------------------------------------------------------
--------------------- Table Index Additional Info Classes ----------------------
--------------------------------------------------------------------------------
}

	TKTableIndexInfos = class;

{ TKTableIndexInfo }

	TKTableIndexInfo = class( TKCustomCollectionItem )
	private
		FIndexInfo: string;
		FIndexAlias: string;
		
		function GetOwnerCollection: TKTableIndexInfos;

	protected

	public
		constructor Create( ACollection: TCollection ); override;

		procedure Assign( Source: TPersistent ); override;
		function Equals( Item: TKCustomCollectionItem ): Boolean; override;

		property Owner: TKTableIndexInfos
						 read GetOwnerCollection;

	published
		property IndexInfo: string
						 read FIndexInfo write FIndexInfo;
		property IndexAlias: string
						 read FIndexAlias write FIndexAlias;

	end;

{ TKTableIndexInfos }

	TKTableIndexInfos = class( TKCustomCollection ) 
	private
		function GetOwnerComp: TKTable;

		procedure SetItem( Index: Integer; AItem: TKTableIndexInfo ); 
		function GetItem( Index: Integer ): TKTableIndexInfo;

	protected
	 procedure Update( Item: TCollectionItem ); override;

		property Dataset: TKTable
						 read GetOwnerComp;

	public
		constructor Create( AComp: TKTable ); virtual;

		function Add: TKTableIndexInfo; virtual;
		property Items[Index: Integer]: TKTableIndexInfo
						 read GetItem write SetItem; default;
						 
	end;

{
--------------------------------------------------------------------------------
------------------------------- Dataset Classes --------------------------------
--------------------------------------------------------------------------------
}

	TKeyArray = array[loKeyAction..hiKeyAction] of TShortCut;

{ TKKeyActions }

	TKKeyActions = class( TPersistent )
	private
		FDataset: TDBDataset;
		FNullDataField: string;
		FKeyAction: TKeyArray;

		procedure SetKey( Index: Integer; const Value: TShortCut );
		function GetKey( Index: Integer ): TShortCut;

	protected
		function IndexOfShortCut( sc: TShortCut ): Integer; dynamic;

	public
		constructor Create( ADBDataset: TDBDataset ); virtual;

		procedure NullCommandAction; dynamic;
		procedure Assign( Source: TPersistent ); override;
		function PerformAction( sc: TShortCut ): Boolean; virtual;
		procedure SetDefaults( const Args: array of TShortCut );
		function ShortCutToDataSetAction( sc: TShortCut ): TKDataSetAction; virtual;

		property Dataset: TDBDataset
						 read FDataset;
		property NullDataField: string
						 read FNullDataField;

	published
		property First: TShortCut
						 index 0  read GetKey write SetKey default SC_CTRL_H;
		property Prior: TShortCut
						 index 1  read GetKey write SetKey default SC_CTRL_J;
		property Next: TShortCut
						 index 2  read GetKey write SetKey default SC_CTRL_K;
		property Last: TShortCut
						 index 3  read GetKey write SetKey default SC_CTRL_L;
		property Insert: TShortCut
						 index 4  read GetKey write SetKey default SC_CTRL_EQUAL;
		property Append: TShortCut
						 index 5  read GetKey write SetKey default SC_F7;
		property Edit: TShortCut
						 index 6  read GetKey write SetKey default SC_F2;
		property Post: TShortCut
						 index 7  read GetKey write SetKey default SC_F8;
		property Cancel: TShortCut
						 index 8  read GetKey write SetKey default SC_F9;
		property Delete: TShortCut
						 index 9  read GetKey write SetKey default SC_CTRL_SUB;
		property Refresh: TShortCut
						 index 10 read GetKey write SetKey default SC_F5;
		property NullCommand : TShortCut
						 index 11 read GetKey write SetKey default SC_CTRL_N;

	end;

	EKQuery = class( EKDBTables );
	EKTable = class( EKDBTables );

	TKMDelayedDBError = record
		Msg: Cardinal;
		Reserved: LongInt;
		ErrorMessage: string;
		Result: Integer;
	end;

	TKCacheIntegrity = ( ciClean, ciError );

	TKCurrentOperation = ( coPost, coCancel, coDelete );

	TKDatasetActionKind = ( dakAll, dakEditModes, dakError, dakNone );

	TKDatasetKeyEvent = procedure( Sender: TObject; var Key: Word; Shift: TShiftState;
		var Handled: Boolean ) of object;

	TKDatasetRollBackEvent = procedure( Sender: TDataset; E: Exception;
		IsLastPost: Boolean; var IsEdit, IsCancel, IsRaise: Boolean ) of object;

	TKDatasetBeforeActionEvent = procedure( Sender: TObject; Dataset: TDataset;
		LogRecords: TStrings ) of object;

	TKDatasetAfterActionEvent = procedure( Sender: TObject; Dataset: TDataset;
		var LogData: Boolean; LogRecords: TStrings ) of object;

	TKDatasetCacheIntegrityEvent = procedure( Sender: TObject; op: TKCurrentOperation;
		var Integrity: TKCacheIntegrity ) of object;

{ TKQuery }

	TKQueryOverlappedEvent = procedure( Dataset: TDataset;
		var CanExecute: Boolean ) of object;

	TKQuery = class( TQuery )
	private
{ Overlapped transaction support }
		FGlobalModified: Boolean;
		FAsyncResourceCount: Cardinal;
		FLocalOverlappedDepth: Integer;
		FGlobalOverlappedDepth: Integer;
		FOverlappedTransaction: Boolean;
		FTransactionGroupIndex: Cardinal;
		FOnOverlappedEnd: TKQueryOverlappedEvent;
		FOnOverlappedCommit: TKQueryOverlappedEvent;
		FOnOverlappedRollback: TKQueryOverlappedEvent;
{ End of overlapped transaction support }

		FClosing: Boolean;
		FAuditGroupIndex: Integer;
		FCancelOnRollback: Boolean;
		FInternalResync: Boolean;
		FCustomRefresh: Boolean;
		FKeyFields: string;
		FErrorMessage: string;
		FKStrings: TKStrings;
		FUpdating: Boolean;
		FAutoUpdate: Boolean;
		FAutoCommit: Boolean;
		FCheckKeyAction: Boolean;
		FConfirmDelete: Boolean;
		FCurrentAction: TKDatasetAction;
		FCommitCacheSize: Word;
		FCommitting: Boolean;
		FLastOpPost: Boolean;
		FCommitCacheCount: Integer;
		FAfterCommit: TDatasetNotifyEvent;
		FBeforeCommit: TDatasetNotifyEvent;
		FAuditory: Boolean;
		FDatasetActions: TKAggregateDatasetActions;
		FActionKinds: TKDatasetActionKind;
		FKKeyActions: TKKeyActions;
		FCacheIntegrity: TKCacheIntegrity;

		FOnKeyDown: TKDatasetKeyEvent;
		FOnExecSQL: TDatasetNotifyEvent;
		FOnUpdateChildren: TDatasetNotifyEvent;
		FBeforeChildrenApplyUpdates: TDatasetNotifyEvent;
		FAfterChildrenApplyUpdates: TDatasetNotifyEvent;
		FOnRollBackChildren: TKDatasetRollBackEvent;
		FAfterDatasetAction: TKDatasetAfterActionEvent;
		FBeforeDatasetAction: TKDatasetBeforeActionEvent;
		FOnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent;

{ ----- Global overlapped support ----- }

		procedure SetTransactionGroupIndex( Value: Cardinal );
		
{ ----- End of overlapped transaction support ----- }

		procedure SetQuery( Value: TStrings );
		procedure SetCommitCacheSize( Value: Word );
		procedure SetAutoCommit( Value: Boolean );

		function GetDSActions: TKDatasetActions;
		procedure SetDSActions( const Value: TKDatasetActions );
		procedure SetAggregateDatasetActions( Value: TKAggregateDatasetActions );
		procedure SetKeyActions( Value: TKKeyActions );
		procedure SetActionKinds( const Value: TKDatasetActionKind );

		procedure SetKeyFields( const Value: string );

		procedure CMAppKeyDown( var Message: TWMKeyDown); message CM_APPKEYDOWN;
		procedure KMDelayedDBError( var Message: TKMDelayedDBError ); message KM_DELAYED_DBERROR;

	protected
{ ----- Overlapped transaction support ----- }

		procedure PopTransaction; dynamic;
		function CanCommit: Boolean; dynamic;

		procedure OT_NotifyDecrement; dynamic;
		procedure OT_NotifyIncrement; dynamic;
		procedure OT_NotifyTransactionEnd; dynamic;
		procedure OT_NotifyTransactionStart; dynamic;
		procedure OT_NotifyTransactionError; dynamic;

		procedure OverlappedTransactionCommit; dynamic;
		procedure OverlappedTransactionRollback; dynamic;
		procedure OverlappedDecrement( var Addend: Integer ); dynamic;
		procedure OverlappedIncrement( var Addend: Integer ); dynamic;
		procedure OverlappedTransactionStart( Notify: Boolean ); dynamic;
		procedure OverlappedTransactionEnd( Notify: Boolean );  dynamic;

		procedure DoOverlappedCommit; dynamic;
		procedure DoOverlappedRollback; dynamic;

{ ----- End of overlapped transaction support ----- }

		procedure DoInternalAfterClose;
		procedure DoInternalBeforeClose;

		procedure DoKey( var Message: TWMKey ); dynamic;
		function WndProc( var Message: TMessage ): Boolean; dynamic;
		procedure KeyDown( var Key: Word; Shift: TShiftState; var Handled: Boolean ); dynamic;

		procedure CommitCache; virtual;

		procedure InternalRefresh; override;
		procedure InternalEdit; override;
		procedure InternalPost; override;
		procedure InternalDelete; override;

		procedure DoCheckCacheIntegrity( op: TKCurrentOperation ); dynamic;

		procedure DoBeforeClose; override;
		procedure DoBeforeDelete; override;
		procedure DoBeforeCommit; virtual;

		procedure DoBeforePost; override;
		procedure DoBeforeCancel; override;
		procedure DoBeforeEdit; override;
		procedure DoBeforeInsert; override;
		procedure DoBeforeOpen; override;
		procedure DoBeforeScroll; override;

		procedure DoAfterDelete; override;
		procedure DoAfterClose; override;

		procedure DoAfterPost; override;
		procedure DoAfterCancel; override;
		procedure DoAfterEdit; override;
		procedure DoAfterInsert; override;
		procedure DoAfterOpen; override;
		procedure DoAfterScroll; override;
		procedure DoAfterCommit; virtual;

		procedure DoOnNewRecord; override;
		procedure DoOnCalcFields; override;

		procedure DoExecSQL; dynamic;
		procedure DoUpdateChildren; dynamic;
		procedure DoChildrenApplyUpdates( IsBefore: Boolean ); dynamic;
		procedure DoRollBackChildren( E: Exception; var DoRaise: Boolean;
			OldCommitCacheSize: Integer ); dynamic;

		procedure InternalBeforeRefresh; virtual;
		procedure InternalAfterRefresh; virtual;

		procedure DataEvent( Event: TDataEvent; Info: LongInt ); override;

		procedure AssignTo( Dest: TPersistent ); override;

		procedure DoDatasetLog( ACurrentAction: TKDatasetAction; IsAfter: Boolean ); dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Update; virtual;
		procedure Refresh; virtual;
		procedure ExecSQL; virtual;
		procedure CommitUpdates; virtual;
		procedure CancelUpdates; virtual;

		function ActionAllowed( Action: TKDataSetAction ): Boolean; dynamic;
		function DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean; dynamic;

		procedure Resync( Mode: TResyncMode ); override;

{ Overlapped transaction support }

		procedure ErrTransaction; dynamic;
		procedure PushTransaction; dynamic;

		property OverlappedTransaction: Boolean
						 read FOverlappedTransaction;
		property LocalOverlappedDepth: Integer
						 read FLocalOverlappedDepth;
		property GlobalOverlappedDepth: Integer
						 read FGlobalOverlappedDepth;

{ End of overlapped transaction support }

		property CommitCacheCount: Integer
						 read FCommitCacheCount;
		property CacheIntegrity: TKCacheIntegrity
						 read FCacheIntegrity;
		property Committing: Boolean
						 read FCommitting;
		property CurrentAction: TKDatasetAction
						 read FCurrentAction;
		property DSActions: TKDatasetActions
						 read GetDSActions write SetDSActions;

	published
{ Overlapped transaction support }
		property CommitCacheSize: Word
						 read FCommitCacheSize write SetCommitCacheSize default DEFAULT_COMMITCACHESIZE; 
		property OnOverlappedCommit: TKQueryOverlappedEvent
						 read FOnOverlappedCommit write FOnOverlappedCommit;
		property OnOverlappedEnd: TKQueryOverlappedEvent
						 read FOnOverlappedEnd write FOnOverlappedEnd;
		property OnOverlappedRollback: TKQueryOverlappedEvent
						 read FOnOverlappedRollback write FOnOverlappedRollback;
{ End of overlapped transaction support }

		property SQL
						 write SetQuery;
		property AuditGroupIndex: Integer
						 read FAuditGroupIndex write FAuditGroupIndex;
		property AutoUpdate: Boolean
						 read FAutoUpdate write FAutoUpdate default true;
		property AutoCommit: Boolean
						 read FAutoCommit write SetAutoCommit default true;
		property CancelOnRollback: Boolean
						 read FCancelOnRollback write FCancelOnRollback default false;
		property CheckKeyAction: Boolean
						 read FCheckKeyAction write FCheckKeyAction default true;
		property ConfirmDelete: Boolean
						 read FConfirmDelete write FConfirmDelete default false;
		property TransactionGroupIndex: Cardinal
						 read FTransactionGroupIndex write SetTransactionGroupIndex;

		property Auditory: Boolean
						 read FAuditory write FAuditory default false;
		property KeyActions: TKKeyActions
						 read FKKeyActions write SetKeyActions;
		property DatasetActions: TKAggregateDatasetActions
						 read FDatasetActions write SetAggregateDatasetActions;
		property KeyFields: string
						 read FKeyFields write SetKeyFields;
		property ActionKinds: TKDatasetActionKind
						 read FActionKinds write SetActionKinds default dakNone;

		property AfterCommit: TDatasetNotifyEvent
						 read FAfterCommit write FAfterCommit;
		property BeforeCommit: TDatasetNotifyEvent
						 read FBeforeCommit write FBeforeCommit;
		property BeforeDatasetAction: TKDatasetBeforeActionEvent
						 read FBeforeDatasetAction write FBeforeDatasetAction;
		property AfterDatasetAction: TKDatasetAfterActionEvent
						 read FAfterDatasetAction write FAfterDatasetAction;
		property AfterChildrenApplyUpdates: TDatasetNotifyEvent
						 read FAfterChildrenApplyUpdates write FAfterChildrenApplyUpdates;
		property BeforeChildrenApplyUpdates: TDatasetNotifyEvent
						 read FBeforeChildrenApplyUpdates write FBeforeChildrenApplyUpdates;

		property OnCheckCacheIntegrity: TKDatasetCacheIntegrityEvent
						 read FOnCheckCacheIntegrity write FOnCheckCacheIntegrity;
		property OnExecSQL: TDatasetNotifyEvent
						 read FOnExecSQL write FOnExecSQL;
		property OnKeyDown : TKDatasetKeyEvent
						 read FOnKeyDown write FOnKeyDown;
		property OnRollBackChildren: TKDatasetRollbackEvent
						 read FOnRollBackChildren write FOnRollBackChildren;
		property OnUpdateChildren: TDatasetNotifyEvent
						 read FOnUpdateChildren write FOnUpdateChildren;

	end;

{ TKTable }

	TDittoType = ( dtAutomatic, dtManual, dtNone );

	PStoreRec = ^TStoreRec;
	TStoreRec = Record
		Buff: Pointer;
		IsEmpty: Boolean;
		IsBlob: Boolean;
	end;

	TKTable = class( TTable )
	private
		FMemory: TList;
		FErrorMessage: string;
		FTableInfo: string;
		FTableAlias: string;
		FAuditGroupIndex: Integer;
		FRecordBufferEmpty: Boolean;
		FDitto: TDittoType;
		FAuditory: Boolean;
		FDatasetActions: TKAggregateDatasetActions;
		FCurrentAction: TKDatasetAction;
		FFlushToDisk: Boolean;
		FAutoRefresh: Boolean;
		FCheckKeyAction: Boolean;
		FConfirmDelete: Boolean;
		FActionKinds: TKDatasetActionKind;
		FProhibit: TKProhibit;
		FCascade: TKCascade;
		FTableIndexInfos: TKTableIndexInfos;
		FFlushCount: Integer;
		FFlushResource: Integer;
		FKKeyActions: TKKeyActions;

		FOnKeyDown: TKDatasetKeyEvent;
		FOnDitto: TDatasetNotifyEvent;
		FOnCopyRecord: TDatasetNotifyEvent;
		FOnPasteRecord: TDatasetNotifyEvent;
		FAfterDatasetAction: TKDatasetAfterActionEvent;
		FOnIntegrityError: TKOnIntegrityError;
		FBeforeDatasetAction: TKDatasetBeforeActionEvent;

		function GetDSActions: TKDatasetActions;
		procedure SetDSActions( const Value: TKDatasetActions );
		procedure SetAggregateDatasetActions( Value: TKAggregateDatasetActions );
		procedure SetActionKinds( const Value: TKDatasetActionKind );
		procedure SetKeyActions( Value: TKKeyActions );

		procedure BuildRecordList;
		procedure FreeRecordList;
		procedure UpdateRecordList;
		function PasteNewRecord: Boolean;

		function GetCascade: TKCascade;
		procedure SetCascade( Value: TKCascade );
		function GetProhibit: TKProhibit;
		procedure SetProhibit( Value: TKProhibit );

		procedure SetTableIndexInfos( Value: TKTableIndexInfos );

		procedure SetTableName( const Value: TFileName );

		procedure CMAppKeyDown( var Message: TWMKeyDown ); message CM_APPKEYDOWN;
		procedure KMDelayedDBError( var Message: TKMDelayedDBError ); message KM_DELAYED_DBERROR;

	protected
		procedure DoInternalAfterClose;

		procedure UpdateIntegrityItem( Item: TKTableDBIntegrityItem ); dynamic;
		procedure UpdateIndexInfoItem( Item: TKTableIndexInfo ); virtual;

		procedure DoKey( var Message: TWMKey ); dynamic;
		function WndProc( var Message: TMessage ): Boolean; dynamic;
		procedure KeyDown( var Key: Word; Shift: TShiftState; var Handled: Boolean ); dynamic;

		procedure InternalEdit; override;
		procedure InternalPost; override;
		procedure InternalDelete; override;

		procedure DoAfterPost; override;
		procedure DoAfterDelete; override;
		procedure DoAfterCancel; override;
		procedure DoAfterClose; override;
		procedure DoAfterEdit; override;
		procedure DoAfterInsert; override;
		procedure DoAfterOpen; override;
		procedure DoAfterScroll; override;
		procedure DoOnNewRecord; override;
		procedure DoOnCalcFields; override;

		procedure DoBeforePost; override;
		procedure DoBeforeCancel; override;
		procedure DoBeforeEdit; override;
		procedure DoBeforeInsert; override;
		procedure DoBeforeOpen; override;
		procedure DoBeforeScroll; override;

		procedure DoBeforeDelete; override;
		procedure DoBeforeClose; override;

		procedure DataEvent( Event: TDataEvent; Info: LongInt ); override;

		procedure AssignTo( Dest: TPersistent ); override;

		procedure DoAutoRefresh; dynamic;
		procedure DoFlushToDisk; dynamic;
		procedure DoDatasetLog( ACurrentAction: TKDatasetAction; IsAfter: Boolean ); dynamic;

		procedure Ditto; dynamic;
		procedure DoDitto; dynamic;
		procedure DoCopyRecord; dynamic;
		procedure DoPasteRecord; dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Pack;
		procedure CopyRecord; dynamic;
		procedure PasteRecord; dynamic;

		function ActionAllowed( Action: TKDataSetAction ): Boolean; dynamic;
		function DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean; dynamic;

		property CurrentAction: TKDatasetAction
						 read FCurrentAction;
		property RecordBufferEmpty: Boolean
						 read FRecordBufferEmpty;
		property DSActions: TKDatasetActions
						 read GetDSActions write SetDSActions;
						 
	published
		property AuditGroupIndex: Integer
						 read FAuditGroupIndex write FAuditGroupIndex;
		property AutoRefresh: Boolean
						 read FAutoRefresh write FAutoRefresh default true;
		property CheckKeyAction: Boolean
						 read FCheckKeyAction write FCheckKeyAction default true;
		property ConfirmDelete: Boolean
						 read FConfirmDelete write FConfirmDelete default false;
		property DittoType: TDittoType
						 read FDitto write FDitto default dtNone;
		property FlushToDisk: Boolean
						 read FFlushToDisk write FFlushToDisk default true;
		property FlushCount: Integer
						 read FFlushCount write FFlushCount default DEFAULT_FLUSH_COUNT;
		property Auditory: Boolean
						 read FAuditory write FAuditory default false;
		property KeyActions: TKKeyActions
						 read FKKeyActions write SetKeyActions;
		property DatasetActions: TKAggregateDatasetActions
						 read FDatasetActions write SetAggregateDatasetActions;
		property ActionKinds: TKDatasetActionKind
						 read FActionKinds write SetActionKinds default dakNone;
		property Cascade: TKCascade
						 read GetCascade write SetCascade;
		property Prohibit: TKProhibit
						 read GetProhibit write SetProhibit;
		property TableIndexInfos: TKTableIndexInfos
						 read FTableIndexInfos write SetTableIndexInfos;
		property TableInfo: string
						 read FTableInfo write FTableInfo;
		property TableAlias: string
						 read FTableAlias write FTableAlias;
		property TableName
						 write SetTableName;

		property BeforeDatasetAction: TKDatasetBeforeActionEvent
						 read FBeforeDatasetAction write FBeforeDatasetAction;
		property AfterDatasetAction: TKDatasetAfterActionEvent
						 read FAfterDatasetAction write FAfterDatasetAction;
		property OnCopyRecord: TDatasetNotifyEvent
						 read FOnCopyRecord write FOnCopyRecord;
		property OnDitto: TDatasetNotifyEvent
						 read FOnDitto write FOnDitto;
		property OnKeyDown : TKDatasetKeyEvent
						 read FOnKeyDown write FOnKeyDown;
		property OnIntegrityError: TKOnIntegrityError
						 read FOnIntegrityError write FOnIntegrityError;
		property OnPasteRecord: TDatasetNotifyEvent
						 read FOnPasteRecord write FOnPasteRecord;

	end;

const

	DEFAULT_KEYACTIONS: TKeyArray =
	(
		SC_CTRL_H,     { First       }
		SC_CTRL_J,     { Prior       }
		SC_CTRL_K,     { Next        }
		SC_CTRL_L,     { Last    		 }
		SC_CTRL_EQUAL, { Insert    	 }
		SC_F7,         { Append    	 }
		SC_F2,         { Edit    		 }
		SC_F8,         { Post        }
		SC_F9,         { Cancel      }
		SC_CTRL_SUB,   { Delete      }
		SC_F5,         { Refresh     }
		SC_CTRL_N      { NullCommand }
	 );

implementation

uses
	BDE, BDEConst, uksyResStr, uksyConsts, ukrUtils, ukrDBUtils,
	ukrEngines, ukdbResStr, ukdbUtils;

{
--------------------------------------------------------------------------------
-------------------- General Referencial Integrity Classes ---------------------
--------------------------------------------------------------------------------
}

{--------------------------- TKCustomDBIntegrityItem ---------------------------}

constructor TKCustomDBIntegrityItem.Create( ACollection: TCollection );
begin
	ForceCollectionClass( ACollection );
	FLinkedFields := TKStrings.Create;
	inherited Create( ACollection );
	ForceObject( Owner.Dataset );
	FDataBaseName := Owner.Dataset.DatabaseName;
end;

destructor TKCustomDBIntegrityItem.Destroy;
begin
	FLinkedFields.Free;
	inherited Destroy;
end;

procedure TKCustomDBIntegrityItem.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomDBIntegrityItem ) then
	begin
		DataBaseName := ( Source as TKCustomDBIntegrityItem ).DataBaseName;
		LinkedFields.Assign( ( Source as TKCustomDBIntegrityItem ).LinkedFields );
	end;
	inherited Assign( Source );
end;

procedure TKCustomDBIntegrityItem.ForceCollectionClass( ACollection: TCollection );
begin
	if ( not CheckObjectClass( ACollection, TKCustomDBIntegrity ) ) then
		RaiseException( EKDBIntegrity, sErrDBIInvDBIClass );
end;

function TKCustomDBIntegrityItem.GetLinkedFields: TKStrings;
begin
	Result := FLinkedFields;
end;

procedure TKCustomDBIntegrityItem.SetLinkedFields( const Value: TKStrings );
begin
{ To prevent Design-Time setting by Textual form description or Run-Time directly assignment. }
	if ( not Loading( Owner.Dataset ) ) then
		ForceFieldsEx( Owner.Dataset, Value, VALID_LINKEDFIELD_TYPES, stValues );
	FLinkedFields.Assign( Value );
end;

function TKCustomDBIntegrityItem.GetOwnerCollection: TKCustomDBIntegrity;
begin
	Result := TKCustomDBIntegrity( inherited GetOwnerCollection );
end;

function TKCustomDBIntegrityItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	with ( Item as TKCustomDBIntegrityItem ) do
		Result := inherited Equals( Item ) and
			CheckStrEqual( Self.DataBaseName, DataBaseName ) and
			Self.LinkedFields.Equals( LinkedFields );
end;

{----------------------------- TKCustomDBIntegrity -----------------------------}

constructor TKCustomDBIntegrity.Create( ADataset: TDBDataset; AItemClass: TKCustomDBIntegrityItemClass );
begin
	ForceObject( ADataset );
	inherited Create( ADataset, AItemClass, false );
end;

function TKCustomDBIntegrity.GetOwnerComp: TDBDataset;
begin
	Result := TDBDataset( inherited GetOwnerComp );
end;

procedure TKCustomDBIntegrity.SetItem( Index: Integer; const AItem: TKCustomDBIntegrityItem );
begin
	inherited SetItem( Index, AItem );
end;

function TKCustomDBIntegrity.GetItem( Index: Integer ): TKCustomDBIntegrityItem;
begin
	Result := TKCustomDBIntegrityItem( inherited GetItem( Index ) );
end;

function TKCustomDBIntegrity.GetItemByName( const AName: string ): TKCustomDBIntegrityItem;
begin
	Result := TKCustomDBIntegrityItem( inherited GetItemByName( AName ) );
end;

function TKCustomDBIntegrity.Add: TKCustomDBIntegrityItem;
begin
	Result := TKCustomDBIntegrityItem( inherited Add );
	Result.DataBaseName := Dataset.DatabaseName;
end;

{
--------------------------------------------------------------------------------
------------------- General Table Referencial Integrity Classes ----------------
--------------------------------------------------------------------------------
}

{--------------------------- TKTableDBIntegrityItem ----------------------------}

constructor TKTableDBIntegrityItem.Create( ACollection: TCollection );
begin
	inherited Create( ACollection );
	if ( not CheckObject( Owner.FEvaluate ) ) then
		Owner.FEvaluate := TQuery.Create( nil );
end;

procedure TKTableDBIntegrityItem.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKTableDBIntegrityItem ) then
		TableName := ( Source as TKTableDBIntegrityItem ).TableName;
end;

procedure TKTableDBIntegrityItem.SetTableName( const Value: TFileName );
begin
	if ( not CheckStrEqual( Value, TableName ) ) then
	begin
		FTableName := Value;
		{Owner.SetItemName( Self );}
		if ( not Loading( Owner.Dataset ) ) then
			LinkedFields.Clear;
	end;
end;

function TKTableDBIntegrityItem.GetOwnerCollection: TKTableDBIntegrity;
begin
	Result := TKTableDBIntegrity( inherited GetOwnerCollection );
end;

function TKTableDBIntegrityItem.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := inherited Equals( Item ) and
		CheckStrEqual( TableName, ( Item as TKTableDBIntegrityItem ).TableName );
end;

function TKTableDBIntegrityItem.GetDefaultSQLStatement: string;
begin
  Result := '';
end;

procedure TKTableDBIntegrityItem.DoOnEvaluateSQL( EvalSQL: TStrings );
begin
	if Assigned( FOnEvaluateSQL ) then
		FOnEvaluateSQL( Owner.Dataset, Self, EvalSQL );
end;

function TKTableDBIntegrityItem.GetEvaluationSQL: TStrings;
begin
	Result := TStringList.Create;
	try
		Result.Text := GetDefaultSQLStatement;
		DoOnEvaluateSQL( Result );
	except
		Result.Free;
		raise;
	end;
end;

function TKTableDBIntegrityItem.QueryAction: TKQueryAction;
var
	i,
	j,
	k,
	w: Integer;
	s: string;
begin
	Result := qaNone;
	s := Owner.FEvaluate.SQL.Text;
	i := Pos( SQL_DML_DELETE, s );
	j := Pos( SQL_DML_INSERT, s );
	k := Pos( SQL_DML_UPDATE, s );
	w := Pos( SQL_DML_SELECT, s );
	if ( i <> 0 ) or ( j <> 0 ) or ( k <> 0 ) then
		Result := qaExecSQL
	else if ( w <> 0 ) then
		Result := qaOpen;
end;

procedure TKTableDBIntegrityItem.PerformQueryAction;
begin
	try
	  Owner.FEvaluate.DataBaseName := DataBaseName;
		case QueryAction of
			qaOpen   : Owner.FEvaluate.Open;
			qaExecSQL: Owner.FEvaluate.ExecSQL;
			qaNone   : RaiseException( EKDBIntegrity, sErrDBIInvQueryAction );
		end;
	except
		on EDataBaseError do
			RaiseException( EKDBIntegrity, SEmptySQLStatement );
		on E: EDBEngineError do
		begin
			if ( E.ErrorCount > 0 ) then
{ The numbers listed above are BDE Error Codes... }
				case E.Errors[0].ErrorCode of
					$271E: RaiseException( EKDBIntegrity, sErrDSInvAlias );
					$2E5D: RaiseExceptionFmt( EKDBIntegrity, sErrDSInvSQLStatementEx, [E.Errors[0].Message] );
				else
					raise;
				end
			else
				raise;
		end;
	end;
end;

procedure TKTableDBIntegrityItem.FillQuery;
var
	sl: TStrings;
begin
	sl := GetEvaluationSQL;
	try
		if ( not ( CheckStrings( sl ) and CheckStrings( LinkedFields ) ) ) then
			RaiseException( EKDBIntegrity, sErrDSInvSQLStatement );
	 {Owner.FEvaluate.Close;}
		Owner.FEvaluate.SQL.Assign( sl );
		PerformQueryAction;
	finally
		sl.Free;
	end;
end;

{----------------------------- TKTableDBIntegrity ------------------------------}

constructor TKTableDBIntegrity.CreateLinked( ADataset: TKTable;
	AItemClass: TKCustomDBIntegrityItemClass; AEvent: TKOnEvaluateSQL );
begin
	inherited Create( ADataset, AItemClass );
	FOnEvaluateSQL := AEvent;
end;

destructor TKTableDBIntegrity.Destroy;
begin
	if CheckObject( FEvaluate ) then
	begin
		FEvaluate.Close;
		FEvaluate.Free;
	end;
	inherited Destroy;
end;

procedure TKTableDBIntegrity.Update( Item: TCollectionItem );
var
	i: Integer;
begin
	if CheckObject( Item ) then
		Dataset.UpdateIntegrityItem( TKTableDBIntegrityItem( Item ) )
	else
		for i := 0 to Count - 1 do
			Dataset.UpdateIntegrityItem( Items[i] ); 
end;

procedure TKTableDBIntegrity.SetItem( Index: Integer; const AItem: TKTableDBIntegrityItem );
begin
	inherited SetItem( Index, AItem );
end;

function TKTableDBIntegrity.GetItem( Index: Integer ): TKTableDBIntegrityItem;
begin
	Result := TKTableDBIntegrityItem( inherited GetItem( Index ) );
end;

function TKTableDBIntegrity.GetOwnerComp: TKTable;
begin
	Result := TKTable( inherited GetOwnerComp );
end;

function TKTableDBIntegrity.GetItemByName( const AName: string ): TKTableDBIntegrityItem;
begin
	Result := TKTableDBIntegrityItem( inherited GetItemByName( AName ) );
end;

function TKTableDBIntegrity.Add: TKTableDBIntegrityItem;
begin
	if ( not CheckObject( FEvaluate ) ) then
		FEvaluate := TQuery.Create( nil );
	Result := TKTableDBIntegrityItem( inherited Add );
	if Assigned( FOnEvaluateSQL ) then
		Result.OnEvaluateSQL := FOnEvaluateSQL;
end;

procedure TKTableDBIntegrity.DoOnIntegrityError( Item: TKTableDBIntegrityItem );
var
	ie: TKIntegrityError;
begin
	if Assigned( FOnIntegrityError ) then
	begin
    ie := ieFail;
		FOnIntegrityError( Dataset, Item, ie );
		case ie of
			ieSkip:  Exit;
			ieAbort: SysUtils.Abort;
		end;
	end;
	RaiseExceptionFmt( EKDBIntegrity, sErrDBIIntegrityError, [
		Copy( ClassName, 3, SizeOf( ShortString ) ), Item.Name] );
end;

procedure TKTableDBIntegrity.PerformIntegrity;
var
	i: Integer;
begin
	for i := 0 to Count - 1 do
		if ( not Items[i].PerformItemIntegrity ) then
			DoOnIntegrityError( Items[i] );
end;

class function TKTableDBIntegrity.GetDefaultItemName( Item: TKCustomCollectionItem ): string;
var
	tbName: string;
begin
	tbName := ( Item as TKTableDBIntegrityItem ).TableName ;
	if ( not CheckStr( tbName ) ) then
		Result := Copy( inherited GetDefaultItemName( Item ), 3, SizeOf( ShortString ) )
	else if CheckStr( ExtractFileExt( tbName ) ) then
		Result := Copy( tbName, 1, Pos( '.', tbName ) -1 )
	else
		Result := Copy( tbName, 1, Length( tbName ) );
end;

{------------------------------- TKProhibitItem --------------------------------}

function TKProhibitItem.GetDefaultSQLStatement: string;
const
	SQL_DEFAULT_PROHIBIT = 'SELECT COUNT( * ) FROM ":%s:%s" KT'#13#10'WHERE ( %s )';
begin
	Result := SQL_DEFAULT_PROHIBIT;
	if ( not CheckStrings( LinkedFields ) ) then
		RaiseExceptionFmt( EKDBIntegrity, sErrDBIInvLinkedFlds, [Name] );
	ForceFieldsEx( Owner.Dataset, LinkedFields, VALID_LINKEDFIELD_TYPES, stValues );
	FmtStr( Result, Result, [DataBaseName, TableName, MountLinkedFieldsSQL( Owner.Dataset,
		LinkedFields, 'and', 'KT' )] );
end;

function TKProhibitItem.PerformItemIntegrity: Boolean;
begin
	FillQuery;
	Result := ( Owner.FEvaluate.Fields[0].AsInteger = 0 );
end;

{--------------------------------- TKProhibit ----------------------------------}

constructor TKProhibit.Create( ATable: TKTable );
begin
	inherited Create( ATable, TKProhibitItem );
end;

procedure TKProhibit.SetItem( Index: Integer; const AItem: TKProhibitItem );
begin
	inherited Items[Index] := AItem;
end;

function TKProhibit.GetItem( Index: Integer ): TKProhibitItem;
begin
	Result := TKProhibitItem( inherited Items[Index] );
end;

function TKProhibit.Add: TKProhibitItem;
begin
	Result := TKProhibitItem( inherited Add );
end;

{------------------------------- TKCascadeItem ---------------------------------}

{ Internal state values }
const
	NOT_CHECKED_YET = 0;
	CHECKED_NOT_OK  = 1;
	CHECKED_OK			= 2;

constructor TKCascadeItem.Create( ACollection: TCollection );
begin
	FCheckDeletion := true;
	FInternalCheck := NOT_CHECKED_YET;
	inherited Create( ACollection );
end;

function TKCascadeItem.InternalCheckDeletion: Boolean;
const
	SQL_INTERNAL_CHECK_DELETION = 'SELECT COUNT( * ) FROM ":%s:%s" KT'#13#10'WHERE ( %s )';
var
	s: string;
begin
	s := SQL_INTERNAL_CHECK_DELETION;
	if ( not CheckStrings( LinkedFields ) ) then
		RaiseExceptionFmt( EKDBIntegrity, sErrDBIInvLinkedFlds, [Name] );
	ForceFieldsEx( Owner.Dataset, LinkedFields, VALID_LINKEDFIELD_TYPES, stValues );
	FmtStr( s, s, [DataBaseName, TableName, MountLinkedFieldsSQL( Owner.Dataset,
		LinkedFields, 'and', 'KT' )] );
	Owner.FEvaluate.SQL.Clear;
	Owner.FEvaluate.SQL.Text := s;
	PerformQueryAction; { Hard Couple! }
	Result := ( Owner.FEvaluate.Fields[0].AsInteger = 0 );
	FInternalCheck := Byte( Result ) + 1;
end;

function TKCascadeItem.GetDefaultSQLStatement: string;
const
	SQL_DEFAULT_CASCADE = 'DELETE FROM "%0:s" KT'#13#10 +
		'WHERE'#13#10 +
		'   ( ( SELECT COUNT( * ) FROM "%0:s" KT1 WHERE '#13#10 +
		'       ( %1:s ) ) > 0 ) and '#13#10 +
		'   %2:s ';
begin
	Result := SQL_DEFAULT_CASCADE;
	if ( not CheckStrings( LinkedFields ) ) then
		RaiseExceptionFmt( EKDBIntegrity, sErrDBIInvLinkedFlds, [Name] );
	ForceFieldsEx( Owner.Dataset, LinkedFields, VALID_LINKEDFIELD_TYPES, stValues );
	FmtStr( Result, Result, [TableName,
		MountLinkedFieldsSQL( Owner.Dataset, LinkedFields, 'and', 'KT1' ),
		MountLinkedFieldsSQL( Owner.Dataset, LinkedFields, 'and', 'KT' )] );
end;

function TKCascadeItem.PerformItemIntegrity: Boolean;
begin
	FillQuery;
{ CheckDeletion is a published property with default value true }
	Result := ( not CheckDeletion ) or ( CheckDeletion and InternalCheckDeletion );
	if Result then
  	DoAfterDeletion;
end;

procedure TKCascadeItem.DoAfterDeletion;
begin
	if CheckReference( FAfterDeletion ) then
	  FAfterDeletion( Self );
end;

{--------------------------------- TKCascade -----------------------------------}

constructor TKCascade.Create( ATable: TKTable );
begin
	inherited Create( ATable, TKCascadeItem );
end;

procedure TKCascade.SetItem( Index: Integer; const AItem: TKCascadeItem );
begin
	inherited Items[Index] := AItem;
end;

function TKCascade.GetItem( Index: Integer ): TKCascadeItem;
begin
	Result := TKCascadeItem( inherited Items[Index] );
end;

function TKCascade.Add: TKCascadeItem;
begin
	Result := TKCascadeItem( inherited Add );
end;

procedure TKCascade.AdjustLinkedFieldsForTransaction;
var
	i,
	j: Integer;
	bModified: Boolean;
begin
{
	Validate each LinkedFields, for each integrity item, if one of them was linked with a non
	primary key field of the owner Dataset, clear only this entry into the Linked fields until
	all non primary key fields were deleted. If the linked fields list was emptied, the result
  will be -1 and the loop continues.
}
	bModified := false;
	for i := 0 to Count - 1 do
	begin
		j := CheckPrimaryKeyFieldsExPos( Dataset, Items[i].LinkedFields, VALID_KEYFIELD_TYPES,
		  stValues );
		while ( j <> -1 ) do
		begin
			Items[i].LinkedFields.Delete( j );
			j := CheckPrimaryKeyFieldsExPos( Dataset, Items[i].LinkedFields, VALID_KEYFIELD_TYPES,
			  stValues );
			bModified := true;
		end;
	end;
{
	Mark the FormDesigner of the Dataset as modified
	( for both DataModule and Form )
}
	if ( bModified and Designing( Dataset ) ) then
		MarkDesigner( Dataset );
end;

{
--------------------------------------------------------------------------------
--------------------- Table Index Additional Info Classes ----------------------
--------------------------------------------------------------------------------
}

{ TKTableIndexInfo }

constructor TKTableIndexInfo.Create( ACollection: TCollection );
begin
	ForceObjectClass( ACollection, TKTableIndexInfos );
	inherited Create( ACollection );
end;

procedure TKTableIndexInfo.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKTableIndexInfo ) then
		with ( Source as TKTableIndexInfo ) do
		begin
			Self.IndexInfo := IndexInfo;
			Self.IndexAlias := IndexAlias;
		end;
end;

function TKTableIndexInfo.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
	Result := ( inherited Equals( Item ) and
		CheckStrEqual( IndexInfo, ( Item as TKTableIndexInfo ).IndexInfo ) and
		CheckStrEqual( IndexAlias, ( Item as TKTableIndexInfo ).IndexAlias ) );
end; 

function TKTableIndexInfo.GetOwnerCollection: TKTableIndexInfos;
begin
  Result := TKTableIndexInfos( inherited GetOwnerCollection ); 
end; 

{ TKTableIndexInfos } 

constructor TKTableIndexInfos.Create( AComp: TKTable );
begin
	inherited Create( AComp, TKTableIndexInfo, True );
end; 

function TKTableIndexInfos.Add: TKTableIndexInfo;
begin
  Result := TKTableIndexInfo( inherited Add ); 
end; 

function TKTableIndexInfos.GetItem( Index: Integer ): TKTableIndexInfo;
begin
  Result := TKTableIndexInfo( inherited GetItem( Index ) );
end;

function TKTableIndexInfos.GetOwnerComp: TKTable;
begin
	Result := TKTable( inherited GetOwnerComp );
end; 

procedure TKTableIndexInfos.SetItem( Index: Integer; AItem: TKTableIndexInfo );
begin
  inherited SetItem( Index, AItem );
end; 

procedure TKTableIndexInfos.Update( Item: TCollectionItem ); 
var
  i: Integer;
begin
	if CheckObject( DataSet ) then
		if CheckObject( Item ) then
			Dataset.UpdateIndexInfoItem( TKTableIndexInfo( Item ) )
		else
			for i := 0 to Count - 1 do
				Dataset.UpdateIndexInfoItem( Items[i] );
end;

{
--------------------------------------------------------------------------------
------------------------------- Dataset Classes --------------------------------
--------------------------------------------------------------------------------
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

const

	ACTIONKIND_SETS: array[TKDatasetActionKind] of TKDatasetActions =
	( 
		dsaAll,       { dakAll       }
		dsaEditModes, { dakEditModes }
		dsaError,     { dakError     }
		dsaNone       { dakNone      }
	 );

{---------------------------- Public Implementation ----------------------------}

{-------------------------------- TKKeyActions ---------------------------------}

constructor TKKeyActions.Create( ADBDataset: TDBDataset );
begin
	ForceObject( ADBDataset );
	inherited Create;
	FDataset := ADBDataset;
	SetDefaults( DEFAULT_KEYACTIONS );
	FNullDataField := '';
end;

procedure TKKeyActions.Assign( Source: TPersistent );
var
	i: Integer;
begin
	if CheckObjectClass( Source, TKKeyActions ) then
		for i := loKeyAction to hiKeyAction do
			FKeyAction[i] := ( Source as TKKeyActions ).FKeyAction[i]
	else
		inherited Assign( Source );
end;

procedure TKKeyActions.SetKey( Index: Integer; const Value: TShortCut );
begin
	if ValueBetween( Index, loKeyAction, hiKeyAction, true ) then
		FKeyAction[Index] := Value;
end;

function TKKeyActions.GetKey( Index: Integer ): TShortCut;
begin
	if ValueBetween( Index, loKeyAction, hiKeyAction, true ) then
		Result := FKeyAction[Index]
	else
		Result := SC_NULL;
end;

procedure TKKeyActions.SetDefaults( const Args: array of TShortCut );
var
	i: Integer;
begin
	for i := Low( Args ) to High( Args ) do
		SetKey( i, Args[i] );
end;

function TKKeyActions.IndexOfShortCut( sc: TShortCut ): Integer;
var
	i: Integer;
begin
	Result := -1;
	for i := loKeyAction to hiKeyAction do
		if ( FKeyAction[i] = sc ) then
		begin
			Result := i;
			Exit;
		end;
end;

function TKKeyActions.PerformAction( sc: TShortCut ): Boolean;
begin
	Result := false;
	case IndexOfShortCut( sc ) of
		 0: Dataset.First;
		 1: Dataset.Prior;
		 2: Dataset.Next;
		 3: Dataset.Last;
		 4: Dataset.Insert;
		 5: Dataset.Append;
		 6: Dataset.Edit;
		 7: Dataset.Post;
		 8: Dataset.Cancel;
		 9:
		 try
			 Dataset.Delete;
		 except
			 on EAbort do { nothing };
		 end;
		10: Dataset.Refresh;
		11: NullCommandAction;
	else
		Exit;
	end;
	Result := true;
end;

function TKKeyActions.ShortCutToDataSetAction( sc: TShortCut ): TKDataSetAction;
begin
	case IndexOfShortCut( sc ) of
		 0,
		 1,
		 2,
		 3: Result := dsaScroll;
		 4,
		 5: Result := dsaInsert;
		 6: Result := dsaEdit;
		 7: Result := dsaPost;
		 8: Result := dsaCancel;
		 9: Result := dsaDelete;
		10: Result := dsaUnknown; //refresh
		11: Result := dsaEdit;
	else
		Result := dsaUnknown;
	end;
end;

procedure TKKeyActions.NullCommandAction;
begin
	if Dataset.Active then
		if ( CheckActiveCtrlLinkedDataField( Dataset, FNullDataField ) or
			 CheckActiveDBGridLinked( Dataset, FNullDataField ) ) then
		begin
			if ( not ( Dataset.State in dsEditModes ) ) then
				Dataset.Edit;
			Dataset.FieldByName( NullDataField ).Clear
		end;
end;

{----------------------------------- TKQuery -----------------------------------}

constructor TKQuery.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
{ Overlapped transaction support }

	FGlobalModified := false;
	FAsyncResourceCount := 0;
	FLocalOverlappedDepth := 0;
	FGlobalOverlappedDepth := 0;
	FTransactionGroupIndex := 0;
	FOverlappedTransaction := false;

{ End of overlapped transaction support }

	FClosing := false;
	FCommitting := false;
	FLastOpPost := false;
	FCancelOnRollback := false;
	FUpdating := false;
	FInternalResync := true;
	FAutoUpdate := true;
	FAutoCommit := true;
	FAuditory := false;
	FCheckKeyAction := true;
	FConfirmDelete := false;
	FCommitCacheSize := DEFAULT_COMMITCACHESIZE;
	FCommitCacheCount := 0;
	FAuditGroupIndex := 0;
	FCurrentAction := dsaUnknown;
	FKeyFields := '';
	FCacheIntegrity := ciClean;
	FDatasetActions := TKAggregateDatasetActions.Create( Self );
	FDatasetActions.DSActions := dsaDefActions;
	FActionKinds := dakNone;
	FKKeyActions := TKKeyActions.Create( Self );
	if ( not Designing( Self ) ) then
		Application.HookMainWindow( WndProc );
end;

destructor TKQuery.Destroy;
begin
{ This premature close is invoked to force the Close auditory log event
	to happen before the csDestroying flag is set. }
	Close;
	FreeClean( FKStrings );
	FKKeyActions.Free;
	if ( not Designing( Self ) ) then
		Application.UnHookMainWindow( WndProc );
	inherited Destroy;
end;

procedure TKQuery.SetAutoCommit( Value: Boolean );
begin
	if ( FAutoCommit <> Value ) then
	begin
		if ( ( not Designing( Self ) ) and FAutoCommit ) then
			CommitCache;
		FAutoCommit := Value;
	end;
end;

procedure TKQuery.SetCommitCacheSize( Value: Word );
begin
	CheckInactive;
	if ( Value > 0 ) then
	begin
		FCommitCacheSize := Value;
		FCommitCacheCount := 0;
	end;
end;

{ ----- Global overlapped support ----- }

procedure TKQuery.SetTransactionGroupIndex( Value: Cardinal );
begin
	if FOverlappedTransaction then
		RaiseException( EKQuery, sErrQUYInvOverlappedOperation );
	FTransactionGroupIndex := Value;
end;

{ ----- End of overlapped transaction support ----- }

procedure TKQuery.SetQuery( Value: TStrings );
begin
	if ( not SQL.Equals( Value ) ) then
	begin
		KeyFields := '';
		inherited SQL := Value;
	end;
end;

procedure TKQuery.DoChildrenApplyUpdates( IsBefore: Boolean );
begin
	if IsBefore and Assigned( FBeforeChildrenApplyUpdates ) and ( not ( csDestroying in ComponentState ) ) then
		FBeforeChildrenApplyUpdates( Self )
	else if ( not IsBefore ) and Assigned( FAfterChildrenApplyUpdates ) and ( not ( csDestroying in ComponentState ) ) then
		FAfterChildrenApplyUpdates( Self );
end;

procedure TKQuery.DoRollBackChildren( E: Exception; var DoRaise: Boolean;
	OldCommitCacheSize: Integer );
var
	bEdit,
	bCancel: Boolean;
begin
	bEdit := FLastOpPost;
	bCancel := FCancelOnRollback;
	if Assigned( FOnRollBackChildren ) and ( not ( csDestroying in ComponentState ) ) then
		FOnRollBackChildren( Self, E, FLastOpPost, bEdit, bCancel, DoRaise );
	if bCancel then
{ Flush the cache, returning the Dataset to its original state }
	begin
		CancelUpdates;
		if ( FAutoUpdate and ( not FUpdating ) and ( not FOverlappedTransaction ) ) then
			Update;
		Exit;
	end
	else
		FCommitCacheCount := OldCommitCacheSize;
	if bEdit and ( not CheckDatasetEmpty( Self ) ) then
		Edit;
end;

{ ----- Overlapped Transaction Support ----- }

procedure TKQuery.PopTransaction;
var
	rtl: TKCriticalSection;
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	rtl := TKCriticalSection.Create( true );
	try
{ A TKQuery is only allowed to pop a transaction if it has pushed any; if
	the transaction is left open by some misbehaving TKQuery, the transaction
	will be left open... (non overlapped transactions carried on the same
	database WILL commit the open overlapped transaction). }
		if ( FLocalOverlappedDepth > 0 ) then
		begin
			OverlappedDecrement( FLocalOverlappedDepth );
			OverlappedDecrement( FGlobalOverlappedDepth );
			OT_NotifyDecrement;
		end;
	finally
		rtl.Free;
	end;
end;

procedure TKQuery.PushTransaction;
var
	rtl: TKCriticalSection;
begin
	rtl := TKCriticalSection.Create( true );
	try
		OverlappedTransactionStart( true );
		OverlappedIncrement( FLocalOverlappedDepth );
		OverlappedIncrement( FGlobalOverlappedDepth );
	finally
		rtl.Free;
	end;
end;

procedure TKQuery.ErrTransaction;
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	try
		if CheckObject( Database ) and Database.InTransaction then
		begin
			Database.Rollback;
			OverlappedTransactionRollback;
		end;
	finally
		OverlappedTransactionEnd( true );
	end;
end;

procedure TKQuery.DoOverlappedCommit;
var
	bCommit: Boolean;
begin
	if ( not FGlobalModified ) then
		Exit;
	bCommit := true;
	if Assigned( FOnOverlappedCommit ) then
		FOnOverlappedCommit( Self, bCommit );
	if bCommit then
		CommitUpdates;
end;

procedure TKQuery.DoOverlappedRollback;
var
	bCancel: Boolean;
begin
	if ( not FGlobalModified ) then
		Exit;
	bCancel := true;
	if Assigned( FOnOverlappedRollback ) then
		FOnOverlappedRollback( Self, bCancel );
	if bCancel then
		CancelUpdates;
end;

procedure TKQuery.OT_NotifyDecrement;
begin
	SendMessage( Application.Handle, KM_OTRAN_DEC, LongInt( Self ), 0 );
end;

procedure TKQuery.OT_NotifyIncrement;
begin
	SendMessage( Application.Handle, KM_OTRAN_INC, LongInt( Self ), 0 );
end;

procedure TKQuery.OT_NotifyTransactionEnd;
begin
	SendMessage( Application.Handle, KM_OTRAN_END, LongInt( Self ), 0 );
end;

procedure TKQuery.OT_NotifyTransactionStart;
begin
	SendMessage( Application.Handle, KM_OTRAN_START, LongInt( Self ), 0 );
end;

procedure TKQuery.OT_NotifyTransactionError;
begin
	SendMessage( Application.Handle, KM_OTRAN_ERROR, LongInt( Self ), 0 );
end;

procedure TKQuery.OverlappedTransactionEnd( Notify: Boolean );
var
	rtl: TKCriticalSection;
	bUpdate: Boolean;
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	bUpdate := FGlobalModified and FAutoUpdate;
	rtl := TKCriticalSection.Create( true );
	try
		try
			if Notify then
				OT_NotifyTransactionEnd;
		finally
			FGlobalModified := false;
			FLocalOverlappedDepth := 0;
			FGlobalOverlappedDepth := 0;
			FOverlappedTransaction := false;
		end;
	finally
		rtl.Free;
	end;
	if Assigned( FOnOverlappedEnd ) then
		FOnOverlappedEnd( Self, bUpdate );
	if ( FAutoUpdate and ( not FUpdating ) and bUpdate ) then
		Update;
end;

procedure TKQuery.OverlappedTransactionStart( Notify: Boolean );
var
	rtl: TKCriticalSection;
begin
	if FOverlappedTransaction then
		Exit;
	rtl := TKCriticalSection.Create( true );
	try
		FGlobalModified := false;
		FLocalOverlappedDepth := 0;
		FGlobalOverlappedDepth := 0;
		FOverlappedTransaction := true;
		if Notify then
			OT_NotifyTransactionStart;
	finally
		rtl.Free;
	end;
end;

procedure TKQuery.OverlappedTransactionRollback;
var
	rtl: TKCriticalSection;
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	rtl := TKCriticalSection.Create( true );
	try
		DoOverlappedRollback;
	finally
		rtl.Free;
	end;
end;

procedure TKQuery.OverlappedTransactionCommit;
var
	rtl: TKCriticalSection;
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	rtl := TKCriticalSection.Create( true );
	try
		DoOverlappedCommit;
	finally
		rtl.Free;
	end;
end;

procedure TKQuery.OverlappedDecrement( var Addend: Integer );
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	InterlockedDecrement( Addend );
end;

procedure TKQuery.OverlappedIncrement( var Addend: Integer );
begin
	if ( not FOverlappedTransaction ) then
		Exit;
	InterlockedIncrement( Addend );
end;

{ ----- End of Overlapped Transaction Support ----- }

function TKQuery.CanCommit: Boolean;
begin
{ no overlapped transaction, or the last transaction operation }
	Result := ( not FOverlappedTransaction ) or ( FGlobalOverlappedDepth = 0 );
end;

procedure TKQuery.CommitCache;
var
	db: TDataBase;
	bRaise,
	bOpenTran: Boolean;
	iSaveUpdate: Integer;
begin
	if ( FCommitting or FUpdating ) then
		Exit;
	if ( not CheckObject( DataBase ) ) then
		RaiseExceptionFmt( EKQuery, sErrQUYNoDatabase, [Name] );

	bRaise := true;
	db := DataBase;
	bOpenTran := db.InTransaction;

{ flags with local scope meaning are initialized to default values }
	FCommitting := true;
	FGlobalModified := true;

	DoBeforeCommit;
	Screen.Cursor := crHourGlass;
	try
		DisableControls;
		try
			iSaveUpdate := FCommitCacheCount;
			FAsyncResourceCount := FCommitCacheCount;
{ Check for any active transactions before starting a new one }
			try
				if ( not bOpenTran ) then
				begin
{ For local drivers, the TransIsolation level MUST be tiDirtyRead! }
					if ( not db.IsSQLBased ) then
						db.TransIsolation := tiDirtyRead;
					db.StartTransaction;
				end;
				DoChildrenApplyUpdates( true );
				ApplyUpdates;
{ Give the user a convenient point to apply the updates of its cached children }
				DoChildrenApplyUpdates( false );
{ Commit the changes to the database }
				if CanCommit then
					db.Commit;
			except
				on E: Exception do
				begin
{ if the commit operation fails, rollback the transaction; but be careful
	with nested calls to CommitCache in different TKQuery objects using the
	same DataBase reference- transactions DO NOT nest due to BDE limitations,
	so there is in fact	just one transcation- so, only one RollBack can be
	generated, no matter	where the exception was caght. This situation might
	happen as	a result of a misbehaved OnChildrenApplyUpdates event (users, hmpf!). }
					if db.InTransaction then
						db.Rollback;
{ Signal cache integrity }
					FCacheIntegrity := ciError;
{ DO NOT RAISE if the Query is being destroyed or closed }
					if ( ( csDestroying in ComponentState ) or FClosing ) then
						Exit;
{ give the user a convenient point to manage the dataset and its dependencies:
	the user will decide on whether to cancel the updates or not, whether the
	dataset will be in edit or browse mode on return, and whether the exception
	should be raised or not }
					if FOverlappedTransaction then
						OverlappedTransactionRollback
					else
						DoRollBackChildren( E, bRaise, iSaveUpdate );
{ in overlapped transaction mode, or if overlapped transaction has been
	left uncommited, notify all grouped queries there has been an error and
	finish the transaction }
					if FOverlappedTransaction or
						 ( bOpenTran and ( not FOverlappedTransaction ) ) then
					begin
						try
							OT_NotifyTransactionError;
						finally
							OverlappedTransactionEnd( false );
						end;
					end;
{ do we raise this exception? very often this will be an EAbort error
	(Delphi calls Application.HandleException many times in the database
	 support source code). }
					if bRaise then
						raise
					else
						Exit;
				end;
			end;
{ Flush the cache, returning the Dataset to its commited state }
			if CanCommit then
			begin
				CommitUpdates;
				if ( FAutoUpdate and ( not FUpdating ) ) then
					Update;
				DoAfterCommit;
			end;
		finally
			EnableControls;
		end;
	finally
		FCommitting := false;
		Screen.Cursor := crDefault;
{ if in overlapped transaction mode, and if this is not the last transaction
	(the one responsible for the commit/rollback method calls, pop the transaction
	 count to force FGlobalOverlappedDepth to decrease). }
		if ( FOverlappedTransaction and ( FGlobalOverlappedDepth > 0 ) ) then
			PopTransaction
{	End the transaction if either one is true:
	 .Overlapped transactions have finished;
	 .An overlapped transaction was previousely left uncommited. }
		else if ( FOverlappedTransaction and ( FGlobalOverlappedDepth = 0 ) ) or
						( bOpenTran and ( not FOverlappedTransaction ) ) then
			OverlappedTransactionEnd( true );
	end;
end;

procedure TKQuery.DoBeforeCommit;
begin
	if ( not ( csDestroying in ComponentState ) ) then
	begin
		if Assigned( FBeforeCommit ) then
			FBeforeCommit( Self );
		if ( dsaCommitCache in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
			DoDatasetLog( dsaCommitCache, false );
	end;
end;

procedure TKQuery.DoAfterCommit;
begin
	if Assigned( FAfterCommit ) and ( not ( csDestroying in ComponentState ) ) then
		FAfterCommit( Self );
	if ( dsaCommitCache in DSActions ) then
		DoDatasetLog( dsaCommitCache, true );
end;

procedure TKQuery.AssignTo( Dest: TPersistent );
begin
{
	TKAggregateDatasetActions objects don't know how to manage TKTables, but TKTables
	need this managment and know how to do it- so do it! Here is the place.
	PS: In the case I still have the best linkage policy ( see DanyThorpe chap 10 )!
}
	if CheckObjectClass( Dest, TKAggregateDatasetActions ) then
		( Dest as TKAggregateDatasetActions ).DSActions := DSActions
	else
		inherited AssignTo( Dest );
end;

procedure TKQuery.DataEvent( Event: TDataEvent; Info: LongInt );
begin
	inherited DataEvent( Event, Info );
{ This is the ONE AND ONLY place UpdateRecord could be captured }
	if ( Event = deUpdateState ) and ( dsaUpdateRecord in DSActions ) then
		DoDatasetLog( dsaUpdateRecord, true );
end;

procedure TKQuery.SetKeyActions( Value: TKKeyActions );
begin
	FKKeyActions.Assign( Value );
end;

procedure TKQuery.InternalEdit;
begin
	if ( dsaEditError in DSActions ) then
	begin
		try
			inherited InternalEdit;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaEditError, true );
			raise;
		end;
	end
	else
		inherited InternalEdit;
end;

procedure TKQuery.InternalPost;
begin
	if ( dsaPostError in DSActions ) then
	begin
		try
			inherited InternalPost;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaPostError, true );
			raise;
		end;
	end
	else
		inherited InternalPost;
end;

procedure TKQuery.InternalDelete;
begin
	if ( dsaDeleteError in DSActions ) then
	begin
		try
			inherited InternalDelete;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaDeleteError, true );
			raise;
		end;
	end
	else
		inherited InternalDelete;
end;

procedure TKQuery.DoInternalBeforeClose;
var
	i: Integer;
	bPending,
	bHideError: Boolean;
begin
{ be sure to post pending cached records }
	if CachedUpdates and ( not ( csDestroying in ComponentState ) ) then
	begin
		bPending := false;
{ UpdatesPending is sometimes BOGUS. This bogus situation is easily reproduced
	in TQuery. It is a BDE crippleware. }
		try
			bPending := UpdatesPending;
		except
			on E: EDBEngineError do
			begin
				bHideError := false;
				for i := 0 to E.ErrorCount - 1 do
					bHideError := bHideError or ( E.Errors[i].ErrorCode = DBIERR_NOTSUPPORTED	);
				if ( not bHideError ) then
					raise;
			end;
		end;
		if ( bPending and AutoCommit ) then
			CommitCache;
	end;
	if ( dsaClose in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaClose, false );
end;

procedure TKQuery.DoCheckCacheIntegrity( op: TKCurrentOperation );
begin
	if ( CachedUpdates and ( CacheIntegrity = ciError ) and
			 Assigned( FOnCheckCacheIntegrity ) ) then
		FOnCheckCacheIntegrity( Self, op, FCacheIntegrity );
end;

procedure TKQuery.DoBeforeClose;
begin
	inherited DoBeforeClose;
	FClosing := true;
	DoInternalBeforeClose;
end;

procedure TKQuery.DoBeforePost;
begin
	DoCheckCacheIntegrity( coPost );
	inherited DoBeforePost;
	if ( dsaPost in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaPost, false );
end;

procedure TKQuery.DoBeforeCancel;
begin
	inherited DoBeforeCancel;
	if ( dsaCancel in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaCancel , false );
end;

procedure TKQuery.DoBeforeEdit;
begin
	inherited DoBeforeEdit;
	if ( dsaEdit in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaEdit, false );
end;

procedure TKQuery.DoBeforeInsert;
begin
	inherited DoBeforeInsert;
	if ( dsaInsert in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaInsert, false );
end;

procedure TKQuery.DoBeforeOpen;
begin
  FCacheIntegrity := ciClean;
	inherited DoBeforeOpen;
	if ( dsaOpen in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaOpen, false );
end;

procedure TKQuery.DoBeforeScroll;
begin
	inherited DoBeforeScroll;
	if ( dsaScroll in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaScroll, false );
end;

procedure TKQuery.DoBeforeDelete;
begin
	if ( not FConfirmDelete ) or ( ShowDialog( sWarning, sDBConfirmDelete,
		nil, dsYesNo, boQuestion02 ) = mrYes ) then
	begin
		inherited DoBeforeDelete;
		if ( dsaDelete in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
			DoDatasetLog( dsaDelete, false );
	end
	else
		SysUtils.Abort;
end;

procedure TKQuery.DoAfterDelete;
begin
	inherited DoAfterDelete;
	FLastOpPost := false;
{ be sure to post pending cached records when the count has reached }
	if ( CachedUpdates and UpdatesPending ) and
		 ( FCommitCacheCount < FCommitCacheSize ) then
		Inc( FCommitCacheCount );
	if ( FCommitCacheCount = FCommitCacheSize ) and AutoCommit then
		CommitCache;
	DoCheckCacheIntegrity( coDelete );
	if ( dsaDelete in DSActions ) then
		DoDatasetLog( dsaDelete, true );
end;

procedure TKQuery.DoAfterPost;
begin
	inherited DoAfterPost;
  FLastOpPost := true;
{ be sure to post pending cached records when the count has reached }
	if ( CachedUpdates and UpdatesPending ) and
		 ( FCommitCacheCount < FCommitCacheSize ) then
		Inc( FCommitCacheCount );
	if ( FCommitCacheCount = FCommitCacheSize ) and AutoCommit then
		CommitCache;
	if ( dsaPost in DSActions ) then
		DoDatasetLog( dsaPost, true );
end;

procedure TKQuery.DoOnNewRecord;
begin
{ here we are in dsInser state from TDataset and the MasterFields was setted in the
	inherited call. }
	inherited DoOnNewRecord;
	if ( dsaNewRecord in DSActions ) then
		DoDatasetLog( dsaNewRecord, true );
end;

procedure TKQuery.DoAfterCancel;
begin
	inherited DoAfterCancel;
	DoCheckCacheIntegrity( coCancel );
	if ( dsaCancel in DSActions ) then
		DoDatasetLog( dsaCancel, true );
end;

procedure TKQuery.DoInternalAfterClose;
begin
	FCommitCacheCount := 0;
	if ( dsaClose in DSActions ) then
		DoDatasetLog( dsaClose, true );
end;

procedure TKQuery.DoAfterClose;
begin
	FClosing := false;
  FCacheIntegrity := ciClean;
	inherited DoAfterClose;
	DoInternalAfterClose;
end;

procedure TKQuery.DoAfterEdit;
begin
	inherited DoAfterEdit;
	if ( dsaEdit in DSActions ) then
		DoDatasetLog( dsaEdit, true );
end;

procedure TKQuery.DoAfterInsert;
begin
	inherited DoAfterInsert;
	if ( dsaInsert in DSActions ) then
		DoDatasetLog( dsaInsert, true );
end;

procedure TKQuery.DoAfterOpen;
begin
	inherited DoAfterOpen;
	if ( dsaOpen in DSActions ) then
		DoDatasetLog( dsaOpen, true );
end;

procedure TKQuery.DoAfterScroll;
begin
	inherited DoAfterScroll;
	if ( dsaScroll in DSActions ) then
		DoDatasetLog( dsaScroll, true );
end;

procedure TKQuery.DoOnCalcFields;
begin
	inherited DoOnCalcFields;
	if ( dsaCalcFields in DSActions ) then
		DoDatasetLog( dsaCalcFields, true );
end;

procedure TKQuery.DoDatasetLog( ACurrentAction: TKDatasetAction; IsAfter: Boolean );
const
	AUDIT_EVENT: array[Boolean] of Byte = ( aeNotifyData, aeLogData );
var
	pds: PKDSAuditory;
begin
	if Designing( Self ) or uksyUtils.Destroying( Self ) or ( not FAuditory ) then
		Exit;
	pds := New( PKDSAuditory );
	try
		ZeroMemory( pds, SizeOf( TKDSAuditory ) );
		pds^.AuditEvent := AUDIT_EVENT[IsAfter];
		pds^.Handled := False;
		pds^.CurrentAction := ACurrentAction;
		FCurrentAction := ACurrentAction;
{ The result return value is 0 or 1 ( false or true ) for the LogData Field. }
		SendAppMessage( KA_DATASET, LongInt( Self ), LongInt( pds ) );
	finally
		Dispose( pds );
	end;
end;

function TKQuery.GetDSActions: TKDatasetActions;
begin
	Result := FDatasetActions.DSActions;
end;

procedure TKQuery.SetDSActions( const Value: TKDatasetActions );
{ if the user sets the DatasetActions property from the editor,
	Value might have the element dsaCommitCache and dsaExecSQL; test it to synchronize
	FActionKinds correctly }
begin
	FDatasetActions.DSActions := Value;
	if ( Value = dsaAll + [dsaCommitCache, dsaExecSQL] ) then
		FActionKinds := dakAll
	else if ( Value = dsaEditModes ) then
		FActionKinds := dakEditModes
	else if ( Value = dsaError ) then
		FActionKinds := dakError
	else
		FActionKinds := dakNone
end;

procedure TKQuery.SetAggregateDatasetActions( Value: TKAggregateDatasetActions );
begin
	FDatasetActions.Assign( Value );
end;

procedure TKQuery.SetActionKinds( const Value: TKDatasetActionKind );
begin
	if ( Value = FActionKinds ) then
		Exit;
	FActionKinds := Value;
	if ( ACTIONKIND_SETS[Value] = dsaAll ) then
{ for TKQuery the dsaAll must include dsaCommitCache and dsaExecSQL! }
		DSActions := dsaAll + [dsaCommitCache, dsaExecSQL]
	else
		DSActions := ACTIONKIND_SETS[Value];
end;

procedure TKQuery.Refresh;
begin
	FCustomRefresh := true;
	try
		InternalRefresh;
	finally
		FCustomRefresh := false;
	end;
end;

procedure TKQuery.ExecSQL;
begin
	if ( dsaExecSQL in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaExecSQL, false );
	inherited ExecSQL;
	DoExecSQL;
	if ( dsaExecSQL in DSActions ) then
		DoDatasetLog( dsaExecSQL, True );
end;

procedure TKQuery.CommitUpdates;
begin
	inherited CommitUpdates;
{ No more items currently in cache }
	FCommitCacheCount := 0;
	FCacheIntegrity := ciClean;
end;

procedure TKQuery.CancelUpdates;
begin
	inherited CancelUpdates;
{ No more items currently in cache }
	FCommitCacheCount := 0;
	FCacheIntegrity := ciClean;
end;

procedure TKQuery.DoExecSQL;
begin
	if Assigned( FOnExecSQL ) then
		FOnExecSQL( Self );
end;

procedure TKQuery.Resync( Mode: TResyncMode );
begin
	if FInternalResync then
		inherited Resync( Mode );
	FInternalResync := true;
end;

procedure TKQuery.DoUpdateChildren;
begin
	if Assigned( FOnUpdateChildren ) then
		FOnUpdateChildren( Self );
end;

procedure TKQuery.Update;
begin
	CheckActive;
	if FUpdating then
		Exit;
	if ( CachedUpdates and UpdatesPending and AutoCommit ) then
		CommitCache;
	FUpdating := true;
	Screen.Cursor := crHourGlass;
	try
		Refresh;
	finally
		FUpdating := false;
		Screen.Cursor := crDefault;
	end;
	DoUpdateChildren;
end;

procedure TKQuery.SetKeyFields( const Value: string );
begin
	if ( not CheckStrEqual( Value, FKeyFields ) ) then
	begin
{ To prevent Design-Time setting by Textual form description or Run-Time directly assignment. }
		if ( not Loading( Self ) ) then
			ForceFields( Self, Value, VALID_KEYFIELD_TYPES );
		FKeyFields := Value;
		if ( ( not Designing( Self ) ) and CheckStr( FKeyFields ) ) then
		begin
			if ( not CheckObject( FKStrings ) ) then
				FKStrings := TKStrings.Create;
			ExtractStrings( FKeyFields, CH_LIST_TOKEN, FKStrings );
			FKStrings.AdjustForValues;
		end;
	end;
end;

procedure TKQuery.InternalBeforeRefresh;
begin
	if ( not CheckObject( FKStrings ) ) then
		Exit;
{ Fill the aux list with the default values before close! }
	FillFieldValues( Self, FKStrings );
end;

procedure TKQuery.InternalAfterRefresh;
var
	i: Integer;
	vKeyValues: Variant;
begin
	if ( not CheckStrings( FKStrings ) ) then
		Exit; 
	with FKStrings do
		if ( Count > 1 ) then
		begin
			vKeyValues := VarArrayCreate( [0, Count - 1], varVariant );
			for i := 0 to Count - 1 do
				vKeyValues[i] := ValuesByIndex[i];
		end
		else if ( Count = 1 ) then
			vKeyValues := ValuesByIndex[0];
	FInternalResync := true;
	try
		try
			Locate( FKeyFields, vKeyValues, [] );
		except
			{ ignore Locate error messages }
		end;
	finally
		FInternalResync := false;
	end;
end;

procedure TKQuery.InternalRefresh;
var
	bCallInternal: Boolean;
begin
{ do not inherited: needed to redefine }
	FInternalResync := false;
	DisableControls;
	try
{ if the table was previously opened call the internal refresh synchronism mechanism }
		bCallInternal := ( not ( State  in [dsInactive] ) );
		if bCallInternal then
			InternalBeforeRefresh;
		Close;
		Open;
		if bCallInternal then
			InternalAfterRefresh;
	finally
		EnableControls;
	end;
end;

function TKQuery.WndProc( var Message: TMessage ): Boolean;
begin
	Result := false;
	if ( not Active ) then
		Exit;
	case Message.Msg of
{ Overlapped transaction support }
		KM_OTRAN_ERROR:
		begin
			if ( TKQuery( Message.WParam ) <> Self ) then
				with TKQuery( Message.WParam ) do
					if ( TransactionGroupIndex = Self.TransactionGroupIndex ) then
						try
							Self.OverlappedTransactionRollback;
						finally
							Self.OverlappedTransactionEnd( false );
						end;
		end;

		KM_OTRAN_INC:
		begin
			if ( TKQuery( Message.WParam ) <> Self ) then
				if ( TKQuery( Message.WParam ).TransactionGroupIndex = TransactionGroupIndex ) then
					OverlappedIncrement( FGlobalOverlappedDepth );
		end;

		KM_OTRAN_DEC:
		begin
			if ( TKQuery( Message.WParam ) <> Self ) then
				if ( TKQuery( Message.WParam ).TransactionGroupIndex = TransactionGroupIndex ) then
					OverlappedDecrement( FGlobalOverlappedDepth );
		end;

		KM_OTRAN_START:
		begin
			if ( TKQuery( Message.WParam ) <> Self ) then
				if ( TKQuery( Message.WParam ).TransactionGroupIndex = TransactionGroupIndex ) then
				begin
					OverlappedTransactionStart( false );
					OverlappedIncrement( FGlobalOverlappedDepth );
				end;
		end;

		KM_OTRAN_END:
		begin
			if ( TKQuery( Message.WParam ) <> Self ) then
				if ( TKQuery( Message.WParam ).TransactionGroupIndex = TransactionGroupIndex ) then
					try
						OverlappedTransactionCommit;
					finally
						OverlappedTransactionEnd( false );
					end;
		end;
{ End of overlapped transaction support }

		CM_APPKEYDOWN:
			try
				Message.Result := 0;
				if IsActiveCtrlLinked( Self ) then
					Dispatch( Message );
{ If Message was processed do not let the application instance treat this message }
				Result := ( Message.Result = 1 );
			except
				on E: EDataBaseError do
				begin
{ to allow char code message processing... }
					FErrorMessage := E.Message;
					PostMessage( Application.Handle, KM_DELAYED_DBERROR, 0, LongInt( PChar( FErrorMessage ) ) );
					TWMKeyDown( Message ).CharCode := 0;
					Message.Result := 1;
					Result := True;
				end;                        
			end;
		KM_DELAYED_DBERROR:
			Dispatch( Message );
	end;		
end;

procedure TKQuery.KMDelayedDBError( var Message: TKMDelayedDBError );
begin
	inherited;
	with Message do
		DatabaseError( ErrorMessage );
end;

function TKQuery.ActionAllowed( Action: TKDataSetAction ): Boolean;
var
	bDesign,
	bUpEnable,
	bDnEnable,
	bActiveEditing,
	bActiveNotEditing: Boolean;
begin
	bDesign := Designing( Self );
	bActiveEditing := ( Active ) and ( CanModify ) and ( not bDesign ) and
										( State in dsEditModes );
	bActiveNotEditing := ( Active ) and ( CanModify ) and ( not bDesign ) and
											 ( not ( State in dsEditModes ) );
	bUpEnable := ( bActiveNotEditing and ( not BOF ) );
	bDnEnable := ( bActiveNotEditing and ( not EOF ) );

{ ***** Set common availability for operations ***** }

	case Action of
		dsaPost,
		dsaCancel: Result := bActiveEditing;
		dsaEdit: Result := bActiveNotEditing and ( not IsEmpty );
		dsaInsert: Result := bActiveNotEditing;
		dsaDelete: Result := bActiveNotEditing and ( not IsEmpty );
		dsaScroll: Result := ( bDnEnable or bUpEnable );
	else
		Result := true;
	end;
end;

function TKQuery.DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean;
var
	sc: TShortCut;
begin
	sc := ShortCut( Key, Shift );
	with KeyActions do
	begin
//		Result := ( not FCheckKeyAction ) or ActionAllowed( ShortCutToDataSetAction( sc ) );
		Result := ( FCheckKeyAction and ActionAllowed( ShortCutToDataSetAction( sc ) ) );
		if Result then
			Result := PerformAction( sc );
	end;
end;

procedure TKQuery.CMAppKeyDown( var Message: TWMKeyDown );
begin
	inherited;
	DoKey( Message );
end;

procedure TKQuery.DoKey( var Message: TWMKey );
var
	Key: Word;
	ShiftState: TShiftState;
	bHandled: Boolean;
begin
	with Message do
	begin         
		ShiftState := KeyDataToShiftState( KeyData );
		bHandled := false;
		KeyDown( CharCode, ShiftState, bHandled );
{ Let application process this message only if it isn't handled or is handled
	and CharCode <> 0 }
		Result := Integer( ( ( bHandled ) and ( CharCode <> 0 ) ) );
{ If there is no event or if the event don't handle the message, try to perform the
	requested action ( by ShortCut ) and return the appropriate value. Remember that,
	here 0 = true ! }
		if ( Result = 0 ) then
		begin
			Key := CharCode;
			CharCode := 0;
			Result := Integer( DispatchShortCut( Key, ShiftState ) );
			if ( Result <> 1 ) then
				CharCode := Key;
		end;
	end;
end;

procedure TKQuery.KeyDown( var Key: Word; Shift: TShiftState; var Handled: Boolean );
begin
	if Assigned( FOnKeyDown ) then
		FOnKeyDown( Self, Key, Shift, Handled );
end;

{----------------------------------- TKTable -----------------------------------}

{
	DITTO:
	------

	This method can only be called if the Dataset is in dsInsert or dsBrowse state.
	If AutoDitto is enabled, this method is called after the inherited calls of
	DoOnNewRecord for dsInsert state and DoAfterPost for dsBrowse state.
	For the ditto operation to take effect, we need to first call Ditto in dsBrowse mode
	to fill in the copy buffer with the last inserted record. For this reason we test
	if the Dataset is in dsBrowse state for a ditto operation- that is, a Dataset must
	have at	least one inserted record to cache for ditto.
}

constructor TKTable.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDitto := dtNone;
	FRecordBufferEmpty := true;
	FDatasetActions := TKAggregateDatasetActions.Create( Self );
	FDatasetActions.DSActions := dsaDefActions;
	FActionKinds := dakNone;
	FAutoRefresh := true;
	FFlushToDisk := true;
	FFlushCount := DEFAULT_FLUSH_COUNT;
	FFlushResource := 0;
	FAuditGroupIndex := 0;
	FAuditory := false;
	FCheckKeyAction := true;
	FCurrentAction := dsaUnknown;
	FMemory := TList.Create; { list to contain record info }
	FProhibit := TKProhibit.Create( Self );
	FProhibit.OnIntegrityError := FOnIntegrityError;
	FCascade := TKCascade.Create( Self );
	FCascade.OnIntegrityError := FOnIntegrityError;
	FTableIndexInfos := TKTableIndexInfos.Create( Self );
	FKKeyActions := TKKeyActions.Create( Self );
	if ( not Designing( Self ) ) then
		Application.HookMainWindow( WndProc );
end;

destructor TKTable.Destroy;
begin
{
	This Premature close was called just to take the last close auditory log event
	without the cdDestroying flag.
}
	Close;
	FreeRecordList;
	FDatasetActions.Free;
	FMemory.Free;
	FProhibit.Free;
	FCascade.Free;
	FTableIndexInfos.Free;
	FKKeyActions.Free;
	if ( not Designing( Self ) ) then
		Application.UnHookMainWindow( WndProc );
	inherited Destroy;
end;

procedure TKTable.AssignTo( Dest: TPersistent );
begin
{
	TKAggregateDatasetActions objects don't know how to manage TKTables, but TKTables
	need this managment and know how to do it- so do it! Here is the place.
	PS: In the case I still have the best linkage policy ( see DanyThorpe chap 10 )!
}
	if CheckObjectClass( Dest, TKAggregateDatasetActions ) then
		( Dest as TKAggregateDatasetActions ).DSActions := DSActions
	else
		inherited AssignTo( Dest );
end;

procedure TKTable.UpdateIntegrityItem( Item: TKTableDBIntegrityItem );
begin
	{ Default update item code }
end;

procedure TKTable.SetTableIndexInfos( Value: TKTableIndexInfos );
begin
	FTableIndexInfos.Assign( Value );
end;

procedure TKTable.UpdateIndexInfoItem( Item: TKTableIndexInfo );
begin
  { Do nothing }
end;

procedure TKTable.DataEvent( Event: TDataEvent; Info: LongInt );
begin
	inherited DataEvent( Event, Info );
{ This is the ONE AND ONLY place UpdateRecord could be captured }
	if ( Event = deUpdateState ) and ( dsaUpdateRecord in DSActions ) then
		DoDatasetLog( dsaUpdateRecord, true );
end;

procedure TKTable.InternalEdit;
begin
	if ( dsaEditError in DSActions ) then
	begin
		try
			inherited InternalEdit;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaEditError, true );
			raise;
		end;
	end
	else
		inherited InternalEdit;
end;

procedure TKTable.InternalPost;
begin
	if ( dsaPostError in DSActions ) then
	begin
		try
			inherited InternalPost;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaPostError, true );
			raise;
		end;
	end
	else
		inherited InternalPost;
end;

procedure TKTable.InternalDelete;
begin
	if ( dsaDeleteError in DSActions ) then
	begin
		try
			inherited InternalDelete;
		except
{ Any errors are logged: abort, retry and fail }
			DoDatasetLog( dsaDeleteError, true );
			raise;
		end;
	end
	else
		inherited InternalDelete;
end;

procedure TKTable.CopyRecord;
begin
{
	CopyRecord is not allowed if either the tables is not in browse mode,
	or the table is empty.
}
	if ( State <> dsBrowse ) then
		RaiseException( EKTable, sErrTBInvState );
	if ( not CheckDatasetEmpty( Self ) ) then
	begin
{ manages memory for record data buffering ( might be overkill ) }
		FreeRecordList;
		BuildRecordList;
		UpdateRecordList;
	end
	else
		RaiseException( EKTable, sErrTBInvDittoEmptyTable );
	DoCopyRecord;
end;

procedure TKTable.PasteRecord;
begin
	if ( State in [dsBrowse, dsInsert] ) then
	begin
		if ( State = dsBrowse ) then
			Append;
		if ( not PasteNewRecord ) then
			Cancel
		else
			DoPasteRecord;
	end
	else
		RaiseException( EKTable, sErrTBInvState );
end;

procedure TKTable.Ditto;
begin
	if ( fDitto = dtNone ) then
		Exit;
	case State of
		dsInsert:
			if ( not FRecordBufferEmpty ) then
				PasteRecord
			else
{ There is no need for DoDitto }
				Exit;
		dsBrowse: CopyRecord;
	else
		RaiseException( EKTable, sErrTBInvDittoState );
	end;
	DoDitto;
end;

procedure TKTable.DoDatasetLog( ACurrentAction: TKDatasetAction; IsAfter: Boolean );
const
	AUDIT_EVENT: array[Boolean] of Byte = ( aeNotifyData, aeLogData );
var
	pds: PKDSAuditory;
begin
	if Designing( Self ) or uksyUtils.Destroying( Self ) or ( not FAuditory ) then
		Exit;
	pds := New( PKDSAuditory );
	try
		ZeroMemory( pds, SizeOf( TKDSAuditory ) );
		pds^.AuditEvent := AUDIT_EVENT[IsAfter];
		pds^.Handled := False;
		pds^.CurrentAction := ACurrentAction;
		FCurrentAction := ACurrentAction;
{ The result return value is 0 or 1 ( false or true ) for the LogData Field. }
		SendAppMessage( KA_DATASET, LongInt( Self ), LongInt( pds ) );
	finally
		Dispose( pds );
	end;
end;

procedure TKTable.SetKeyActions( Value: TKKeyActions );
begin
	FKKeyActions.Assign( Value );
end;

function TKTable.GetCascade: TKCascade;
begin
	Result := FCascade;
end;

procedure TKTable.SetCascade( Value: TKCascade );
begin
	FCascade.Assign( Value );
end;

function TKTable.GetProhibit: TKProhibit;
begin
	Result := FProhibit;
end;

procedure TKTable.SetProhibit( Value: TKProhibit );
begin
	FProhibit.Assign( Value );
end;

procedure TKTable.SetTableName( const Value: TFileName );
var
	i: Integer;
begin
	if ( not CheckStrEqual( Value, TableName ) ) then
	begin
		with Prohibit do
			for i := 0 to Count - 1 do
				Items[i].TableName := '';
		with Cascade do
			for i := 0 to Count - 1 do
				Items[i].TableName := '';
		inherited TableName := Value;
	end;
end;

function TKTable.GetDSActions: TKDatasetActions;
begin
	Result := FDatasetActions.DSActions;
end;

procedure TKTable.SetDSActions( const Value: TKDatasetActions );
begin
	FDatasetActions.DSActions := Value;
	if ( Value = dsaAll ) then
		FActionKinds := dakAll
	else if ( Value = dsaEditModes ) then
		FActionKinds := dakEditModes
	else if ( Value = dsaError ) then
		FActionKinds := dakError
	else
		FActionKinds := dakNone
end;

procedure TKTable.SetAggregateDatasetActions( Value: TKAggregateDatasetActions );
begin
	FDatasetActions.Assign( Value );
end;

procedure TKTable.SetActionKinds( const Value: TKDatasetActionKind );
begin
	if ( Value <> FActionKinds ) then
	begin
		FActionKinds := Value;
		DSActions := ACTIONKIND_SETS[Value];
	end;
end;

procedure TKTable.DoAutoRefresh;
begin
{ refresh table data ( good for C/S environment ) }
	if FAutoRefresh then
	begin
		DisableControls;
		try
			Refresh;
		finally
			EnableControls;
		end;
	end;
end;

procedure TKTable.DoFlushToDisk;
begin
{
	Flush BDE cache to disk; watch out: if cached updates is enabled, flushing
	BDE cached data to disk causes temporary DB files to remain undeleted in the
	EXE directory (and might as well disrupt normal cached updates logic).
}
	if ( FFlushToDisk and ( not CachedUpdates ) ) then
	begin
		if ( FFlushResource >= FFlushCount ) then
		begin
			FFlushResource := 0;
			DbiSaveChanges( Handle );
		end
		else
			Inc( FFlushResource );
	end;
end;

procedure TKTable.BuildRecordList;
var
	i: Integer;
	psr: PStoreRec;
begin
{ make sure there is no memory allocated- memory leaks are undesired! }
	if ( not FRecordBufferEmpty ) then
		Exit;
	for i := 0 to FieldCount - 1 do
		if ( Fields[i].FieldKind = fkData ) then
		begin
			psr := New( PStoreRec );
			try
				ZeroMemory( psr, SizeOf( TStoreRec ) );
				if Fields[i].IsBlob then
				begin
					psr^.Buff := TMemoryStream.Create;
					psr^.IsBlob := true;
				end
				else
					GetMem( psr^.Buff, Fields[i].DataSize );
			except
				if CheckPointer( psr^.Buff ) then
					if Fields[i].IsBlob then
						TMemoryStream( psr^.Buff ).Free
					else
						FreeMem( psr^.Buff, Fields[i].DataSize );
				Dispose( psr );
				raise;
			end;
			FMemory.Add( psr );
		end;
end;

procedure TKTable.FreeRecordList;
var
	i: Integer;
begin
{ make sure there is memory to be released }
	if FRecordBufferEmpty then
		Exit;
	for i := FMemory.Count - 1 downto 0 do
	begin
		if CheckPointer( PStoreRec( FMemory[i] )^.Buff ) then
			if PStoreRec( FMemory[i] )^.IsBlob then
				TMemoryStream( PStoreRec( FMemory[i] )^.Buff ).Free
			else
				FreeMem( PStoreRec( FMemory[i] )^.Buff );
		Dispose( FMemory[i] );
		FMemory[i] := nil;
	end;
	FMemory.Pack;
{ all should know there is no record in memory }
	FRecordBufferEmpty := true;
end;

function TKTable.PasteNewRecord: Boolean;
var
	i: Integer;
begin
	Result := false;
	if FRecordBufferEmpty then
		Exit;
{ get existing record data from memory and offer as default data }
	for i := 0 to FieldCount - 1 do
		with Fields[i], PStoreRec( FMemory[i] )^ do
{ use buffered data for NULL fields only- user might have defined its own field default
	in OnNewARecord event handler }
			if ( FieldKind = fkData ) then
				if ( IsNull and ( not IsEmpty ) and CheckPointer( Buff ) ) then
					if IsBlob then
						TBlobField( Fields[i] ).LoadFromStream( TMemoryStream( Buff ) )
					else
						SetData( Buff );
	Result := true;
end;

procedure TKTable.DoCopyRecord;
begin
	if Assigned( FOnCopyRecord ) then
		FOnCopyRecord( Self );
end;

procedure TKTable.DoPasteRecord;
begin
	if Assigned( FOnPasteRecord ) then
		FOnPasteRecord( Self );
end;

procedure TKTable.DoDitto;
begin
	if Assigned( FOnDitto ) then
		FOnDitto( Self );
end;

procedure TKTable.UpdateRecordList;
var
	i: Integer;
	id: TIndexDef;
begin
	FieldDefs.Update;
	IndexDefs.Update;
{ buffers current record data to memory ( except for AutoInc fields ) }
	for i := 0 to FieldCount - 1 do
		with Fields[i], PStoreRec( FMemory[i] )^ do
			if ( FieldKind = fkData ) then
				if ( IsNull or ( DataType = ftAutoInc ) or ( not CheckPointer( Buff ) ) ) then
					IsEmpty := true
				else if IsBlob then
				begin
					IsEmpty := false;
					TBlobField( Fields[i] ).SaveToStream( TMemoryStream( Buff ) );
				end
				else
				begin
					id := IndexDefs.GetIndexForFields( FieldName, False );
{ do not copy data from fields that are part of the primary key }
					IsEmpty := ( CheckObject( id ) and ( id.Options * [ixPrimary] = [ixPrimary] ) );
					if ( not IsEmpty ) then
						IsEmpty := ( not GetData( Buff ) );
				end;
{ There is ONE record now!  }
	FRecordBufferEmpty := false;
end;

procedure TKTable.DoOnNewRecord;
begin
{ here we are in dsInser state from TDataset and the MasterFields was setted in the
	inherited call. }
	inherited DoOnNewRecord;
	if ( dsaNewRecord in DSActions ) then
		DoDatasetLog( dsaNewRecord, true );
	if ( FDitto = dtAutomatic ) then
		Ditto;
end;

procedure TKTable.DoAfterPost;
begin
{ here we are in dsBrowse state from TDataset! }
	inherited DoAfterPost;
	DoAutoRefresh;
	DoFlushToDisk;
	if ( FDitto = dtAutomatic ) then
		Ditto;
	if ( dsaPost in DSActions ) then
		DoDatasetLog( dsaPost, true );
end;

procedure TKTable.DoBeforeDelete;
begin
	if ( not FConfirmDelete ) or ( ShowDialog( sWarning, sDBConfirmDelete, nil,
		dsYesNo, boQuestion02 ) = mrYes ) then
	begin
		Prohibit.PerformIntegrity;
		Cascade.PerformIntegrity;
		inherited DoBeforeDelete;
		if ( dsaDelete in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
			DoDatasetLog( dsaDelete, false );
	end
	else
		SysUtils.Abort;
end;

procedure TKTable.DoBeforeClose;
begin
	inherited DoBeforeClose;
	FFlushResource := FFlushCount;
	DoFlushToDisk;
	FFlushResource := 0; { FlushToDisk may be false at this point and can be true again }
	if ( dsaClose in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaClose, false );
end;

procedure TKTable.DoBeforePost;
begin
	inherited DoBeforePost;
	if ( dsaPost in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaPost, false );
end;

procedure TKTable.DoBeforeCancel;
begin
	inherited DoBeforeCancel;
	if ( dsaCancel in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaCancel , false );
end;

procedure TKTable.DoBeforeEdit;
begin
	inherited DoBeforeEdit;
	if ( dsaEdit in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaEdit, false );
end;

procedure TKTable.DoBeforeInsert;
begin
	inherited DoBeforeInsert;
	if ( dsaInsert in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaInsert, false );
end;

procedure TKTable.DoBeforeOpen;
begin
	inherited DoBeforeOpen;
	if ( dsaOpen in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaOpen, false );
end;

procedure TKTable.DoBeforeScroll;
begin
	inherited DoBeforeScroll;
	if ( dsaScroll in DSActions ) and ( Assigned( FBeforeDatasetAction ) ) then
		DoDatasetLog( dsaScroll, false );
end;

procedure TKTable.DoAfterDelete;
begin
	inherited DoAfterDelete;
	if ( dsaDelete in DSActions ) then
		DoDatasetLog( dsaDelete, true );
	DoAutoRefresh;
	DoFlushToDisk;
end;

procedure TKTable.DoAfterCancel;
begin
	inherited DoAfterCancel;
	if ( dsaCancel in DSActions ) then
		DoDatasetLog( dsaCancel, true );
end;

procedure TKTable.DoInternalAfterClose;
begin
	if ( dsaClose in DSActions ) then
		DoDatasetLog( dsaClose, true );
end;

procedure TKTable.DoAfterClose;
begin
	inherited DoAfterClose;
	DoInternalAfterClose;
end;

procedure TKTable.DoAfterEdit;
begin
	inherited DoAfterEdit;
	if ( dsaEdit in DSActions ) then
		DoDatasetLog( dsaEdit, true );
end;

procedure TKTable.DoAfterInsert;
begin
	inherited DoAfterInsert;
	if ( dsaInsert in DSActions ) then
		DoDatasetLog( dsaInsert, true );
end;

procedure TKTable.DoAfterOpen;
begin
	inherited DoAfterOpen;
	if ( dsaOpen in DSActions ) then
		DoDatasetLog( dsaOpen, true );
end;

procedure TKTable.DoAfterScroll;
begin
	inherited DoAfterScroll;
	if ( dsaScroll in DSActions ) then
		DoDatasetLog( dsaScroll, true );
end;

procedure TKTable.DoOnCalcFields;
begin
	inherited DoOnCalcFields;
	if ( dsaCalcFields in DSActions ) then
		DoDatasetLog( dsaCalcFields, true );
end;

procedure TKTable.Pack;
var
	Error: DBIMsg;
	bOldActiveState,
	bOldExclusiveState: Boolean;
	TableDescription: pCRTblDesc;
	TempResult: DBIResult;
	TempDBHandle: hDBIdb;
	ExtType: TTableType;
begin
{ Save table current states }
	bOldActiveState := Active;
	try
		bOldExclusiveState := Exclusive;
		try
			DisableControls;
			try
{ Prepare to Pack }
				if ( Active and ( not Exclusive ) ) then
					Close;
				if ( not Exclusive ) then
					Exclusive := true;
				if ( not Active ) then
					Open;
				TempDBHandle := DBHandle;
{ Get the table file name extension }
				{$IFDEF DELPHI4}
				ExtType := GetTableType;
				{$ELSE}
				ExtType := GetTableType( TableName );
				{$ENDIF}
				if ( ExtType = ttDefault ) then
					ExtType := ttParadox;
{ Pack the table }
				Screen.Cursor := crHourGlass;
				try
					TempResult := DBIERR_NOTSUPPORTED;
					if ( TableType = ttDBase ) or
						 ( ( ExtType = ttDBase ) and
							 ( TableType = ttDefault ) ) then
						TempResult := DbiPackTable( DBHandle, Handle, nil, nil, true )
					else if ( TableType = ttParadox ) or
									( ( ExtType = ttParadox ) and
										( TableType = ttDefault ) ) then
					begin
						TableDescription := New( pCRTblDesc );
						try
							ZeroMemory( TableDescription, SizeOf( TableDescription^ ) );
							with TableDescription^ do
							begin
								bPack := true;
								StrPCopy( szTblName, TableName );
								szTblType := szParadox;
							end;
							Close;
							TempResult := DbiDoRestructure( TempDBHandle, 1, TableDescription,
								nil, nil, nil, false );
						finally
							Dispose( TableDescription );
						end;
					end;
				finally
					Screen.Cursor := crDefault;
				end;
{ Any errors ? }
				if ( TempResult <> DBIERR_NONE ) then
				begin
					DbiGetErrorString( TempResult, Error );
					RaiseException( EKTable, Error );
				end;
{ Reset old table states }
				Close;
			finally
				EnableControls;
			end;
		finally
			Exclusive := bOldExclusiveState;
		end;
	finally
		Active := bOldActiveState;
	end;
end;

function TKTable.WndProc( var Message: TMessage ): Boolean;
begin
	Result := false;
	if ( not Active ) then
		Exit;
	case Message.Msg of
		CM_APPKEYDOWN:
			try
				Message.Result := 0;
				if IsActiveCtrlLinked( Self ) then
					Dispatch( Message );
{ If Message was processed do not let the application instance treat this message }
				Result := ( Message.Result = 1 );
			except
				on E: EDataBaseError do
				begin
{ to allow char code message processing... }
					FErrorMessage := E.Message;
					PostMessage( Application.Handle, KM_DELAYED_DBERROR, 0, LongInt( PChar( FErrorMessage ) ) );
					TWMKeyDown( Message ).CharCode := 0;
					Message.Result := 1;
					Result := True;
				end;
			end;
		KM_DELAYED_DBERROR:
			Dispatch( Message );
	end;		
end;

procedure TKTable.KMDelayedDBError( var Message: TKMDelayedDBError );
begin
	inherited;
	with Message do
		DatabaseError( ErrorMessage );
end;

function TKTable.ActionAllowed( Action: TKDataSetAction ): Boolean;
var
	bDesign,
	bUpEnable,
	bDnEnable,
	bActiveEditing,
	bActiveNotEditing: Boolean;
begin
	bDesign := Designing( Self );
	bActiveEditing := ( Active ) and ( CanModify ) and ( not bDesign ) and
										( State in dsEditModes );
	bActiveNotEditing := ( Active ) and ( CanModify ) and ( not bDesign ) and
											 ( not ( State in dsEditModes ) );
	bUpEnable := ( bActiveNotEditing and ( not BOF ) );
	bDnEnable := ( bActiveNotEditing and ( not EOF ) );

{ ***** Set common availability for operations ***** }

	case Action of
		dsaPost,
		dsaCancel: Result := bActiveEditing;
		dsaEdit: Result := bActiveNotEditing and ( not IsEmpty );
		dsaInsert: Result := bActiveNotEditing;
		dsaDelete: Result := bActiveNotEditing and ( not IsEmpty );
		dsaScroll: Result := ( bDnEnable or bUpEnable );
	else
		Result := true;
	end;
end;

function TKTable.DispatchShortCut( Key: Word; Shift: TShiftState ): Boolean;
var
	sc: TShortCut;
begin
	sc := ShortCut( Key, Shift );
	with KeyActions do
	begin
//		Result := ( not FCheckKeyAction ) or ActionAllowed( ShortCutToDataSetAction( sc ) );
		Result := ( FCheckKeyAction and ActionAllowed( ShortCutToDataSetAction( sc ) ) );
		if Result then
			Result := PerformAction( sc );
	end;
end;

procedure TKTable.CMAppKeyDown( var Message: TWMKeyDown );
begin
	inherited;
	DoKey( Message );
end;

procedure TKTable.DoKey( var Message: TWMKey );
var
	Key: Word;
	ShiftState: TShiftState;
	bHandled: Boolean;
begin
	with Message do
	begin
		ShiftState := KeyDataToShiftState( KeyData );
		bHandled := false;
		KeyDown( CharCode, ShiftState, bHandled );
{ Let application process this message only if it isn't handled or is handled
	and CharCode <> 0 }
		Result := Integer( ( ( bHandled ) and ( CharCode <> 0 ) ) );
{ If there is no event or if the event don't handle the message, try to perform the
	requested action ( by ShortCut ) and return the appropriate value. Remember that,
	here 0 = true ! }
		if ( Result = 0 ) then
		begin
			Key := CharCode;
			CharCode := 0;
			Result := Integer( DispatchShortCut( Key, ShiftState ) );
			if ( Result <> 1 ) then
				CharCode := Key;
		end;
	end;
end;

procedure TKTable.KeyDown( var Key: Word; Shift: TShiftState; var Handled: Boolean );
begin
	if Assigned( FOnKeyDown ) then
		FOnKeyDown( Self, Key, Shift, Handled );
end;

end.
