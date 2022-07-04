{*******************************************************}
{File:      NCOciDB.PAS                                 }
{Revision:  0.10.01 / 13.04.2005                        }
{Comment:   NC OCI8 VCL: TOCICustomDatabase, TOCIQuery, }
{           TOCIStoredProc, ...                         }
{Copyright: (c) 1999-2005, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}
{CE_Desc_Include(HelpText\NCOciDB.txt)}

unit NCOciDB;

interface

Uses Windows, SysUtils, Classes, Controls, DB, NCOci, NCOciWrapper, NCOciBuff,
     NCOciParams, NCOciFilter, NCSQLMon;

type
    TOCIStringList = class;
    TOCIFetchParams = class;
    TOCIDataFormat = class;
    TOCICustomDatabase = class;
    TOCIImpHndlDatabase = class;
    TOCITransactionManager = class;
    TOCILongStream = class;
    TOCILOBStream = class;
    TOCIFILEStream = class;
    TOCIILOBStream = class;
    TOCIFieldDef = class;
    TOCIFieldDefs = class;
    TOCIUpdateObject = class;
    TOCIDataSet = class;
    TOCIStatementDataSet = class;
    TOCICustomQuery = class;
    TOCIQuery = class;
    TOCIStoredProc = class;
    TOCISequence = class;
    TOCINestedDataSet = class;

{$IFDEF OCI_D6}
    TOCIStringList = class(TStringList)
    public
        property UpdateCount;
    end;
{$ELSE}
    TOCIStringList = class(TStringList)
    private
        function GetUpdateCount: Integer;
    public
        property UpdateCount: Integer read GetUpdateCount;
    end;
{$ENDIF}    

    TOCIFetchParamsValue = (pvPieceBuffSize, pvPieceBuffOwn, pvRowsetSize,
        pvFetchExact, pvFetchAll, pvInrecDataSize);
    TOCIFetchParamsValues = set of TOCIFetchParamsValue;
    TOCIFetchParams = class(TPersistent)
    private
        FAssignedValues: TOCIFetchParamsValues;
        FOwner: TComponent;
        FPieceBuffSize: sb4;
        FPieceBuffOwn: Boolean;
        FRowsetSize: sb4;
        FFetchExact: sb4;
        FInrecDataSize: sb4;
        function GetFetchAll: Boolean;
        procedure SetFetchAll(const Value: Boolean);
        function GetRowsetSize: sb4;
        function GetFetchExact: sb4;
        function GetInrecDataSize: sb4;
        function GetPieceBuffOwn: Boolean;
        function GetPieceBuffSize: sb4;
        procedure SetFetchExact(const Value: sb4);
        procedure SetInrecDataSize(const Value: sb4);
        procedure SetPieceBuffOwn(const Value: Boolean);
        procedure SetPieceBuffSize(const Value: sb4);
        procedure SetRowsetSize(const Value: sb4);
        function GetParentParams: TOCIFetchParams;
        function IsValueOwn(AValue: TOCIFetchParamsValue): Boolean;
        function IsPBFS: Boolean;
        function IsPBOS: Boolean;
        function IsRSS: Boolean;
        function IsFES: Boolean;
        function IsIDSS: Boolean;
    protected
        property ParentParams: TOCIFetchParams read GetParentParams;
    public
        constructor Create(AOwner: TComponent);
        procedure RestoreDefaults;
        procedure Assign(ASource: TPersistent); override;
        procedure TuneVar(AValue: TOCIVariable);
        procedure TuneStmt(AValue: TOCIStatement);
        property AssignedValues: TOCIFetchParamsValues read FAssignedValues;
    published
        // TOCIStatement
        property PieceBuffSize: sb4 read GetPieceBuffSize write SetPieceBuffSize
            stored IsPBFS default IDefPieceBuffLen;
        property PieceBuffOwn: Boolean read GetPieceBuffOwn write SetPieceBuffOwn
            stored IsPBOS default False;
        property RowsetSize: sb4 read GetRowsetSize write SetRowsetSize
            stored IsRSS default IDefRowSetSize;
        property FetchExact: sb4 read GetFetchExact write SetFetchExact
            stored IsFES default 0;
        property FetchAll: Boolean read GetFetchAll write SetFetchAll
            stored False default False;
        // TOCIVariable
        property InrecDataSize: sb4 read GetInrecDataSize write SetInrecDataSize
            stored IsIDSS default IDefInrecDataSize;
    end;

    TOCIDataFormatValue = (fvEInteger, fvEBCD, fvENumber, fvELongString,
        fvEFixedString, fvOBoolType, fvOBoolSize, fvOBoolTrue, fvOBoolFalse,
        fvStrsTrim, fvStrsEmpty2Null, fvDateFormat, fvDateTimeFormat,
        fvAssignDateFormat, fvAssignDateTimeFormat, fvFixedMacroChar,
        fvSQLMacroChar, fvERequired);
    TOCIDataFormatValues = set of TOCIDataFormatValue;
    TOCIDataFormat = class(TPersistent)
    private
        FOwner: TComponent;
        FAssignedValues: TOCIDataFormatValues;
        FEnableInteger: Boolean;
        FEnableBCD: Boolean;
        FEnableNumber: Boolean;
        FEnableLongString: Boolean;
        FEnableFixedString: Boolean;
        FEnableRequired: Boolean;
        FOBoolType: TOCIVarDataType;
        FOBoolSize: Integer;
        FOBoolTrue: Variant;
        FOBoolFalse: Variant;
        FStrsTrim: Boolean;
        FStrsEmpty2Null: Boolean;
        FDateFormat: string;
        FDateTimeFormat: string;
        FAssignDateFormat: string;
        FAssignDateTimeFormat: string;
        FFixedMacroChar: Char;
        FSQLMacroChar: Char;
        function GetEnableBCD: Boolean;
        function GetEnableFixedString: Boolean;
        function GetEnableInteger: Boolean;
        function GetEnableLongString: Boolean;
        function GetOBoolFalse: Variant;
        function GetOBoolSize: Integer;
        function GetOBoolTrue: Variant;
        function GetOBoolType: TOCIVarDataType;
        function GetParentParams: TOCIDataFormat;
        procedure SetEnableBCD(const Value: Boolean);
        procedure SetEnableFixedString(const Value: Boolean);
        procedure SetEnableInteger(const Value: Boolean);
        procedure SetEnableLongString(const Value: Boolean);
        procedure SetOBoolFalse(const Value: Variant);
        procedure SetOBoolSize(const Value: Integer);
        procedure SetOBoolTrue(const Value: Variant);
        procedure SetOBoolType(const Value: TOCIVarDataType);
        function IsValueOwn(AValue: TOCIDataFormatValue): Boolean;
        function IsEIS: Boolean;
        function IsEBCDS: Boolean;
        function IsELSS: Boolean;
        function IsEFSS: Boolean;
        function IsOBTS: Boolean;
        function IsOBSS: Boolean;
        function IsTS: Boolean;
        function IsFS: Boolean;
        function GetStrsTrim: Boolean;
        procedure SetStrsTrim(AValue: Boolean);
        function IsSTS: Boolean;
        function GetStrsEmpty2Null: Boolean;
        procedure SetStrsEmpty2Null(AValue: Boolean);
        function IsSENS: Boolean;
        function GetEnableNumber: Boolean;
        function IsENS: Boolean;
        procedure SetEnableNumber(const Value: Boolean);
        procedure InvalidateFieldDefs;
        function GetAssignDateFormat: String;
        function GetAssignDateTimeFormat: String;
        function GetDateFormat: String;
        function GetDateTimeFormat: String;
        function GetFixedMacroChar: Char;
        function GetSQLMacroChar: Char;
        function IsADFS: Boolean;
        function IsADTFS: Boolean;
        function IsDFS: Boolean;
        function IsDTFS: Boolean;
        procedure SetAssignDateFormat(const Value: String);
        procedure SetAssignDateTimeFormat(const Value: String);
        procedure SetDateFormat(const Value: String);
        procedure SetDateTimeFormat(const Value: String);
        function IsFMCS: Boolean;
        function IsSMCS: Boolean;
        procedure SetFixedMacroChar(const Value: Char);
        procedure SetSQLMacroChar(const Value: Char);
        function GetEnableRequired: Boolean;
        procedure SetEnableRequired(AValue: Boolean);
        function IsERS: Boolean;
    protected
        property ParentParams: TOCIDataFormat read GetParentParams;
    public
        constructor Create(AOwner: TComponent);
        procedure RestoreDefaults;
        procedure Assign(ASource: TPersistent); override;
        function Value2OBool(AValue: Variant): Boolean;
        function OBool2Value(AValue: Boolean): Variant;
        function OBool2ValuePLSQL(const ALeft, ARight: String): String;
        function Value2OBoolPLSQL(const ALeft, ARight: String): String;
        procedure TuneSelItem(AValue: TOCISelectItem);
        procedure TuneVar(AValue: TOCIVariable);
        property AssignedValues: TOCIDataFormatValues read FAssignedValues;
    published
        // TOCISelectItem
        property EnableInteger: Boolean read GetEnableInteger write SetEnableInteger
            stored IsEIS default False;
        property EnableBCD: Boolean read GetEnableBCD write SetEnableBCD
            stored IsEBCDS default False;
        property EnableNumber: Boolean read GetEnableNumber write SetEnableNumber
            stored IsENS default False;
        property EnableLongString: Boolean read GetEnableLongString write SetEnableLongString
            stored IsELSS default False;
        property EnableFixedString: Boolean read GetEnableFixedString write SetEnableFixedString
            stored IsEFSS default False;
        property EnableRequired: Boolean read GetEnableRequired write SetEnableRequired
            stored IsERS default True;
        // TOCIParam, TOCIMacro, TOCIStoredProc
        property OBoolType: TOCIVarDataType read GetOBoolType write SetOBoolType
            stored IsOBTS default otInteger;
        property OBoolSize: Integer read GetOBoolSize write SetOBoolSize
            stored IsOBSS default 0;
        property OBoolTrue: Variant read GetOBoolTrue write SetOBoolTrue
            stored IsTS;
        property OBoolFalse: Variant read GetOBoolFalse write SetOBoolFalse
            stored IsFS;
        // TOCIMacro
        property DateFormat: String read GetDateFormat write SetDateFormat
            stored IsDFS;
        property DateTimeFormat: String read GetDateTimeFormat
            write SetDateTimeFormat stored IsDTFS;
        property AssignDateFormat: String read GetAssignDateFormat
            write SetAssignDateFormat stored IsADFS;
        property AssignDateTimeFormat: String read GetAssignDateTimeFormat
            write SetAssignDateTimeFormat stored IsADTFS;
        property FixedMacroChar: Char read GetFixedMacroChar
            write SetFixedMacroChar stored IsFMCS default '&';
        property SQLMacroChar: Char read GetSQLMacroChar
            write SetSQLMacroChar stored IsSMCS default '%';
        // TOCIParam, TField
        property StrsTrim: Boolean read GetStrsTrim write SetStrsTrim
            stored IsSTS default True;
        property StrsEmpty2Null: Boolean read GetStrsEmpty2Null write SetStrsEmpty2Null
            stored IsSENS default True;
    end;

    TOCILoginField = (lfService, lfMode, lfProfile);
    TOCILoginFields = set of TOCILoginField;
    TOCIObjKind = (okService, okDatabase, okSelectable, okPackage, okProc,
      okSeqs, okFields);
    TOCIConnectStringUsage = (suDesignOnly, suStorePassword);
    TOCIConnectStringUsages = set of TOCIConnectStringUsage;
{$IFNDEF OCI_D5}
    TLoginEvent = procedure(Sender: TObject; const Username, Password: string) of object;
{$ENDIF}
    TOCIDatabaseNameExpandMode = (deNone, deUseThread, deUseOwner);

{$IFDEF OCI_D5}
    TOCICustomDatabase = class(TCustomConnection)
{$ELSE}
    TOCICustomDatabase = class(TOCICustomWrappedComponent)
    private
        FDataSets: TList;
        FLoginPrompt: Boolean;
        FStreamedConnected: Boolean;
        FAfterConnect: TNotifyEvent;
        FAfterDisconnect: TNotifyEvent;
        FBeforeConnect: TNotifyEvent;
        FBeforeDisconnect: TNotifyEvent;
        FOnLogin: TLoginEvent;
{$ENDIF}
    private
        FDefaultTransactionManager: TOCITransactionManager;
        FTransactionManager: TOCITransactionManager;
        FSession: TOCISession;
        FServer: TOCIServer;
        FService: TOCIService;
        FEnv: TOCIEnvironment;
        FNonBlockingMode: Boolean;
        FDatabaseName, FExpandedDBName: String;
        FServerName: String;
        FUserName: String;
        FPassword: String;
        FAuthentMode: TOCIAuthentMode;
        FInitModes: TOCIDatabaseModes;
        FSQLBased: Boolean;
        FOnChangePassword: TNotifyEvent;
        FDBUseCount: Integer;
        FDefaultFetchParams: TOCIFetchParams;
        FLoginRetries: Integer;
        FLoginFields: TOCILoginFields;
        FInInteractiveOpen: Boolean;
        FWaitCount: Integer;
        FWaitCursor: TCursor;
        FDefaultDataFormat: TOCIDataFormat;
        FSilentMode: Boolean;
        FConnectStringUsages: TOCIConnectStringUsages;
        FLock: TRTLCriticalSection;
        FKeepConnection: Boolean;
        FSQLMonitor: TNCSQLMonitorClient;
        FTransactionManagersSerialize: Boolean;
        FOnYield: TNotifyEvent;
        FBeforeTMConnect: TNotifyEvent;
        FAfterTMConnect: TNotifyEvent;
        FSeqCache: TOCIStringList;
        FMaxCursors: Integer;
        FServerVersion: Integer;
        FOnError: TOCIErrorEvent;
        FOnWarning: TOCIWarningEvent;
        FUpdatesJournal: TOCIUpdatesJournal;
        FCachedUpdates: Boolean;
        FShowWaitForm: Boolean;
        FInLoaded: Boolean;

        function GetConnectString: String;
        procedure SetAuthentMode(const Value: TOCIAuthentMode);
        procedure SetConnectString(const Value: String);
        procedure SetDatabaseName(const Value: String);
        procedure RemoveDatabaseName;
        procedure SetInitModes(const Value: TOCIDatabaseModes);
        procedure SetNonBlockingMode(const Value: Boolean);
        procedure SetPassword(const Value: String);
        procedure SetServerName(const Value: String);
        procedure SetUserName(const Value: String);
{$IFNDEF OCI_D5}
        function GetConnected: Boolean;
        procedure SetConnected(Value: Boolean);
        function GetDataSetCount: Integer;
{$ENDIF}
        function GetOCIDataSet(Index: Integer): TOCIDataSet;
        procedure CheckActive;
        procedure CheckInactive;
        procedure FreeHandles;
        function GetAutoCommit: Boolean;
        procedure SetDefaultFetchParams(AValue: TOCIFetchParams);
        function GetServerVersion: String;
        class procedure GetServicesList(AList: TStrings);
        procedure SetDefaultDataFormat(AValue: TOCIDataFormat);
        procedure DoYield(ASender: TObject);
        function IsDBNS: Boolean;
        procedure SetKeepConnection(Value: Boolean);
        function GetActualTransactionManager: TOCITransactionManager;
        procedure SetTransactionManager(AValue: TOCITransactionManager);
        function GetInTransaction: Boolean;
        function GetHTransaction: TOCITransaction;
        procedure SetAutoCommit(AValue: Boolean);
        function GetTransIsolation: TOCITransactionMode;
        procedure SetTransIsolation(AValue: TOCITransactionMode);
        function GetAfterCommit: TNotifyEvent;
        function GetAfterRollback: TNotifyEvent;
        procedure SetAfterStartTransaction(const Value: TNotifyEvent);
        function GetAfterStartTransaction: TNotifyEvent;
        function GetBeforeCommit: TNotifyEvent;
        function GetBeforeRollback: TNotifyEvent;
        function GetBeforeStartTransaction: TNotifyEvent;
        procedure SetAfterCommit(const Value: TNotifyEvent);
        procedure SetAfterRollback(const Value: TNotifyEvent);
        procedure SetBeforeCommit(const Value: TNotifyEvent);
        procedure SetBeforeRollback(const Value: TNotifyEvent);
        procedure SetBeforeStartTransaction(const Value: TNotifyEvent);
        procedure SetSQLMonitor(const Value: TNCSQLMonitorClient);
        function GetServerVersionNo: Integer;
        function GetClientVersionNo: Integer;
        function IsTXS: Boolean;
        function GetHError: TOCIError;
        function GetWarning: EOCINativeError;
        function GetSilent: Boolean;
        procedure SetCachedUpdates(AValue: Boolean);
        function ProcessJournal(ADataSets: array of TOCIDataSet; AVersion: Integer;
            AOperation: TOCIUpdatesJournalOperation): Integer;
        function GetUpdatesPending: Boolean;
        function GetUpdatesCount: Integer;
        procedure SetUpdateVersion(AValue: Integer);
        function GetUpdateVersion: Integer;

    protected
        procedure Loaded; override;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;

        function InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
            hErr, hTX: pOCIHandle): Boolean; virtual;
        procedure ReleaseHandles; virtual;

        procedure ReleaseCursors(AExclude: TOCIDataSet); virtual;
        procedure AttachDataSet(ADS: TOCIDataSet); virtual;
        procedure DetachDataSet(ADS: TOCIDataSet); virtual;
{$IFDEF OCI_D5}
        procedure DoConnect; override;
        procedure DoDisconnect; override;
        function GetConnected: Boolean; override;
        procedure SetConnected(Value: Boolean); override;
{$ENDIF}
        function GetSeq(const AName: String): TOCISequence;
        procedure ClearSeqCache;

        procedure DoError(ASender: TObject; var E: Exception); virtual;
        procedure DoWarning(ASender: TObject; var E: Exception); virtual;

        property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates
            default False; 
        property DatabaseName: String read FDatabaseName write SetDatabaseName
            stored IsDBNS;

        property NonBlockingMode: Boolean read FNonBlockingMode
            write SetNonBlockingMode default False;
        property InitModes: TOCIDatabaseModes read FInitModes
            write SetInitModes default [];

        property AuthentMode: TOCIAuthentMode read FAuthentMode
            write SetAuthentMode default amDefault;
        property UserName: String read FUserName write SetUserName;
        property Password: String read FPassword write SetPassword;
        property ServerName: String read FServerName write SetServerName;
        property ConnectString: String read GetConnectString
            write SetConnectString stored False;
        property ConnectStringUsages: TOCIConnectStringUsages
            read FConnectStringUsages write FConnectStringUsages
            default [suStorePassword];
{$IFNDEF OCI_D5}
        property Connected: Boolean read GetConnected write SetConnected
            default False;
        property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt
            default True;
{$ELSE}
        property Connected;
        property LoginPrompt default True;
{$ENDIF}
        property LoginRetries: Integer read FLoginRetries write FLoginRetries
            default 3;
        property LoginFields: TOCILoginFields read FLoginFields write FLoginFields
            default [lfService, lfMode, lfProfile];
        property KeepConnection: Boolean read FKeepConnection write SetKeepConnection
            default True;

        property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit
            stored IsTXS default True;
        property TransIsolation: TOCITransactionMode read GetTransIsolation
            write SetTransIsolation stored IsTXS default tmReadWrite;
        property TransactionManager: TOCITransactionManager read FTransactionManager
            write SetTransactionManager;
        property TransactionManagersSerialize: Boolean read FTransactionManagersSerialize
            write FTransactionManagersSerialize default False;

        property DefaultFetchParams: TOCIFetchParams read FDefaultFetchParams
            write SetDefaultFetchParams;
        property DefaultDataFormat: TOCIDataFormat read FDefaultDataFormat
            write SetDefaultDataFormat;

        property SilentMode: Boolean read FSilentMode write FSilentMode
            default False;
        property WaitCursor: TCursor read FWaitCursor write FWaitCursor
            default crSQLWait;
        property ShowWaitForm: Boolean read FShowWaitForm write FShowWaitForm
            default True;

        property SQLMonitor: TNCSQLMonitorClient read FSQLMonitor
            write SetSQLMonitor;

        property MaxCursors: Integer read FMaxCursors write FMaxCursors
            default -1;

{$IFNDEF OCI_D5}
        property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
        property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
        property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
        property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
        property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
{$ELSE}
        property OnLogin;
        property BeforeConnect;
        property AfterConnect;
        property BeforeDisconnect;
        property AfterDisconnect;
{$ENDIF}
        property OnChangePassword: TNotifyEvent read FOnChangePassword write FOnChangePassword;

        property BeforeStartTransaction: TNotifyEvent read GetBeforeStartTransaction
            write SetBeforeStartTransaction stored IsTXS;
        property AfterStartTransaction: TNotifyEvent read GetAfterStartTransaction
            write SetAfterStartTransaction stored IsTXS;
        property BeforeCommit: TNotifyEvent read GetBeforeCommit
            write SetBeforeCommit stored IsTXS;
        property AfterCommit: TNotifyEvent read GetAfterCommit
            write SetAfterCommit stored IsTXS;
        property BeforeRollback: TNotifyEvent read GetBeforeRollback
            write SetBeforeRollback stored IsTXS;
        property AfterRollback: TNotifyEvent read GetAfterRollback
            write SetAfterRollback stored IsTXS;
        property OnYield: TNotifyEvent read FOnYield write FOnYield;
        property BeforeTMConnect: TNotifyEvent read FBeforeTMConnect write FBeforeTMConnect;
        property AfterTMConnect: TNotifyEvent read FAfterTMConnect write FAfterTMConnect;

        property OnError: TOCIErrorEvent read FOnError write FOnError;
        property OnWarning: TOCIWarningEvent read FOnWarning write FOnWarning;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure StartAccess;
        procedure EndAccess;

{$IFNDEF OCI_D5}
        procedure Close;
        procedure Open;
{$ENDIF}
        procedure ChangePassword(const ANewPassword: String);
        function InteractiveOpen: Boolean;

        procedure CloseDataSets;
        procedure StartTransaction;
        procedure Commit;
        procedure Rollback;
        procedure RollbackAll;
        procedure CommitAll;
        procedure Break;
        procedure ExecSQL(const ASQL: String);

        function ApplyUpdates(ADataSets: array of TOCIDataSet;
            AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
        function ApplyAllUpdates: Integer;
        function CommitUpdates(ADataSets: array of TOCIDataSet;
            AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
        function CommitAllUpdates: Integer;
        function CancelUpdates(ADataSets: array of TOCIDataSet;
            AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
        function CancelAllUpdates: Integer;

        procedure StartWait;
        procedure AbortWait;
        procedure EndWait;

        class function ExpandDatabaseName(AValue: String;
            AForComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF}): String;
        class function FindDatabase(const AName: String;
            AForComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF}): TOCICustomDatabase;
        class function DatabaseByName(const AName: String;
            AForComponent: TComponent {$IFDEF OCI_D4} = nil {$ENDIF}): TOCICustomDatabase;
        class procedure GetObjectsList(const ADatabase: String; AList: TStrings;
            const AParentObject: String; AObjKind: TOCIObjKind; AIncludeSystem: Boolean);

        function DbgActive: Boolean;
        procedure DbgOut(AType: TNCTraceFlag; const AStmt: String);
        procedure ClearWarning;

{$IFNDEF D5}
        property DataSetCount: Integer read GetDataSetCount;
{$ENDIF}
        property DataSets[Index: Integer]: TOCIDataSet read GetOCIDataSet;

        property IsSQLBased: Boolean read FSQLBased;
        property InTransaction: Boolean read GetInTransaction;
        property ServerVersion: String read GetServerVersion;
        property ServerVersionNo: Integer read GetServerVersionNo;
        property ClientVersionNo: Integer read GetClientVersionNo;
        property ActualTransactionManager: TOCITransactionManager
            read GetActualTransactionManager;

        property HTransaction: TOCITransaction read GetHTransaction;
        property HSession: TOCISession read FSession;
        property HServer: TOCIServer read FServer;
        property HService: TOCIService read FService;
        property HEnvironment: TOCIEnvironment read FEnv;
        property HError: TOCIError read GetHError;

        property Warning: EOCINativeError read GetWarning;
        property Silent: Boolean read GetSilent;

        property UpdatesPending: Boolean read GetUpdatesPending;
        property UpdatesCount: Integer read GetUpdatesCount;
        property UpdateVersion: Integer read GetUpdateVersion write SetUpdateVersion;
    end;

    TOCIDatabase = class(TOCICustomDatabase)
    published
        property CachedUpdates;
        property DatabaseName;
        property NonBlockingMode;
        property InitModes;

        property AuthentMode;
        property UserName;
        property Password;
        property ServerName;
        property ConnectString;
        property ConnectStringUsages;
        property Connected;
        property LoginPrompt;
        property LoginRetries;
        property LoginFields;
        property KeepConnection;

        property AutoCommit;
        property TransIsolation;
        property TransactionManager;
        property TransactionManagersSerialize;

        property DefaultFetchParams;
        property DefaultDataFormat;

        property SilentMode;
        property WaitCursor;
        property ShowWaitForm;

        property SQLMonitor;

        property MaxCursors;

        property OnLogin;
        property BeforeConnect;
        property AfterConnect;
        property BeforeDisconnect;
        property AfterDisconnect;
        property OnChangePassword;
        property BeforeStartTransaction;
        property AfterStartTransaction;
        property BeforeCommit;
        property AfterCommit;
        property BeforeRollback;
        property AfterRollback;
        property OnYield;
        property BeforeTMConnect;
        property AfterTMConnect;

        property OnError;
        property OnWarning;
    end;

    TOCIImpHndlDatabase = class(TOCICustomDatabase)
    private
        FhImpTX: pOCIHandle;
        FhImpSes: pOCIHandle;
        FhImpSrv: pOCIHandle;
        FhImpSvc: pOCIHandle;
        FhImpEnv: pOCIHandle;
        FhImpErr: pOCIHandle;
    protected
        function InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
            hErr, hTX: pOCIHandle): Boolean; override;
    public
        property hImpTX: pOCIHandle read FhImpTX write FhImpTX;
        property hImpSes: pOCIHandle read FhImpSes write FhImpSes;
        property hImpSrv: pOCIHandle read FhImpSrv write FhImpSrv;
        property hImpSvc: pOCIHandle read FhImpSvc write FhImpSvc;
        property hImpEnv: pOCIHandle read FhImpEnv write FhImpEnv;
        property hImpErr: pOCIHandle read FhImpErr write FhImpErr;
    published
        property CachedUpdates;
        property DatabaseName;
        property NonBlockingMode;

        property Connected;
        property KeepConnection;

        property AutoCommit;
        property TransIsolation;
        property TransactionManager;
        property TransactionManagersSerialize;

        property DefaultFetchParams;
        property DefaultDataFormat;

        property SilentMode;
        property WaitCursor;
        property ShowWaitForm;

        property SQLMonitor;

        property MaxCursors;

        property BeforeConnect;
        property AfterConnect;
        property BeforeDisconnect;
        property AfterDisconnect;
        property BeforeStartTransaction;
        property AfterStartTransaction;
        property BeforeCommit;
        property AfterCommit;
        property BeforeRollback;
        property AfterRollback;
        property OnYield;
        property BeforeTMConnect;
        property AfterTMConnect;

        property OnError;
        property OnWarning;
    end;

    TOCITransactionState = (tsInactive, tsActive, tsSuspend, tsPrepared);
    TOCITMConnectAction = (tcNone, tcResume, tcStartTransaction, tcSmart);
    TOCITMDisConnectAction = (tdNone, tdSuspend, tdCommit, tdRollback, tdSmart);
    TOCITransactionManager = class(TOCICustomWrappedComponent)
    private
        FDatabase: TOCICustomDatabase;
        FDatabaseName: String;
        FTransaction: TOCITransaction;
        FXID: TOCIXid;
        FTransIsolation: TOCITransactionMode;
        FAutoCommit: Boolean;
        FSavepoints: TOCIStringList;
        FAfterCommit: TNotifyEvent;
        FAfterRollback: TNotifyEvent;
        FAfterStartTransaction: TNotifyEvent;
        FBeforeCommit: TNotifyEvent;
        FBeforeRollback: TNotifyEvent;
        FBeforeStartTransaction: TNotifyEvent;
        FBeforeSuspendTransaction: TNotifyEvent;
        FAfterSuspendTransaction: TNotifyEvent;
        FBeforeResumeTransaction: TNotifyEvent;
        FAfterResumeTransaction: TNotifyEvent;
        FBeforePrepareTransaction: TNotifyEvent;
        FAfterPrepareTransaction: TNotifyEvent;
        FBeforeForget: TNotifyEvent;
        FAfterForget: TNotifyEvent;
        FInactiveTimeOut: ub4;
        FResumeTimeOut: ub4;
        FState: TOCITransactionState;
        FCoupleMode: TOCICoupleMode;
        FIsDefault: Boolean;
        FConnectAction: TOCITMConnectAction;
        FDisConnectAction: TOCITMDisConnectAction;
        FRollbackSegment: String;
        FOnStateChanged: TNotifyEvent;
        function GetAutoCommit: Boolean;
        procedure SetAutoCommit(const Value: Boolean);
        procedure SetDatabaseName(const Value: String);
        procedure SetXID(const Value: TOCIXid);
        procedure XIDChanging(ASender: TObject);
        procedure XIDChanged(ASender: TObject);
        function GetInTransaction: Boolean;
        function GetIsGlobal: Boolean;
        procedure InternalCommit(A2PC: Boolean; AAll: Boolean);
        procedure InternalRollback(AAll: Boolean);
        function GetHandleAllocated: Boolean;
        procedure CheckInActive;
        procedure CheckDBNameMatch(const AName: String);
        procedure SetTransIsolation(const Value: TOCITransactionMode);
        procedure SetRollbackSegment(const Value: String);
        procedure UpdateToLocal;
        procedure UpdateToGlobal;
        procedure SetState(AState: TOCITransactionState);
    protected
        procedure HandleNeeded(hTX: pOCIHandle); virtual;
        procedure FreeHandle; virtual;
        procedure Connect; virtual;
        procedure ConnectUsingHandle(hTX: pOCIHandle); virtual;
        procedure Disconnect; virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure StartTransaction; virtual;
        procedure Suspend; virtual;
        procedure Resume; virtual;
        procedure Commit; virtual;
        procedure CommitAll; virtual;
        procedure Rollback; virtual;
        procedure RollbackAll; virtual;
        procedure Commit2PC; virtual;
        procedure Prepare2PC; virtual;
        procedure Forget; virtual;
        property State: TOCITransactionState read FState;
        property InTransaction: Boolean read GetInTransaction;
        property IsGlobal: Boolean read GetIsGlobal;
        property IsDefault: Boolean read FIsDefault;
        property HandleAllocated: Boolean read GetHandleAllocated;
        property Database: TOCICustomDatabase read FDatabase;
        property HTransaction: TOCITransaction read FTransaction;
    published
        property DatabaseName: String read FDatabaseName write SetDatabaseName;
        property AutoCommit: Boolean read GetAutoCommit write SetAutoCommit
            default True;
        property TransIsolation: TOCITransactionMode read FTransIsolation
            write SetTransIsolation default tmReadWrite;
        property XID: TOCIXid read FXID write SetXID;
        property InactiveTimeOut: ub4 read FInactiveTimeOut
            write FInactiveTimeOut default MAXINT;
        property ResumeTimeOut: ub4 read FResumeTimeOut
            write FResumeTimeOut default 10;
        property CoupleMode: TOCICoupleMode read FCoupleMode write FCoupleMode
            default omDefault;
        property ConnectAction: TOCITMConnectAction read FConnectAction
            write FConnectAction default tcSmart;
        property DisConnectAction: TOCITMDisConnectAction read FDisConnectAction
            write FDisConnectAction default tdSmart;
        property RollbackSegment: String read FRollbackSegment write
            SetRollbackSegment; 
        property BeforeSuspendTransaction: TNotifyEvent read FBeforeSuspendTransaction
            write FBeforeSuspendTransaction;
        property AfterSuspendTransaction: TNotifyEvent read FAfterSuspendTransaction
            write FAfterSuspendTransaction;
        property BeforeResumeTransaction: TNotifyEvent read FBeforeResumeTransaction
            write FBeforeResumeTransaction;
        property AfterResumeTransaction: TNotifyEvent read FAfterResumeTransaction
            write FAfterResumeTransaction;
        property BeforePrepareTransaction: TNotifyEvent read FBeforePrepareTransaction
            write FBeforePrepareTransaction;
        property AfterPrepareTransaction: TNotifyEvent read FAfterPrepareTransaction
            write FAfterPrepareTransaction;
        property BeforeStartTransaction: TNotifyEvent read FBeforeStartTransaction
            write FBeforeStartTransaction;
        property AfterStartTransaction: TNotifyEvent read FAfterStartTransaction
            write FAfterStartTransaction;
        property BeforeCommit: TNotifyEvent read FBeforeCommit write FBeforeCommit;
        property AfterCommit: TNotifyEvent read FAfterCommit write FAfterCommit;
        property BeforeRollback: TNotifyEvent read FBeforeRollback write FBeforeRollback;
        property AfterRollback: TNotifyEvent read FAfterRollback write FAfterRollback;
        property BeforeForget: TNotifyEvent read FBeforeForget write FBeforeForget;
        property AfterForget: TNotifyEvent read FAfterForget write FAfterForget;
        property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    end;

    TOCILongStream = class(TMemoryStream)
    private
        FField: TBlobField;
        FMode: TBlobStreamMode;
        FDataSet: TOCIDataSet;
        FRecBuf: PChar;
        FOpened: Boolean;
        FModified: Boolean;
        FParam: TOciParam;
        FIndex: Integer;
        procedure CheckCanModify;
    public
        constructor Create(Field: TBLobField; Mode: TBlobStreamMode);
        constructor CreateUsingParam(Param: TOciParam; AIndex: Integer; Mode: TBlobStreamMode);
        destructor Destroy; override;
        procedure Clear;
        procedure SetSize(NewSize: Longint); override;
        function Write(const Buffer; Count: Longint): Longint; override;
        procedure Truncate;
        procedure Assign(ASource: TObject);
    end;

    TOCILOBStream = class(TStream)
    private
        FOCILobLocator: TOCILobLocator;
        FPosition: sb4;
        FMode: TBlobStreamMode;
        FModified: Boolean;
        FField: TBlobField;
        FDataSet: TOCIDataSet;
        FParam: TOciParam;
        FIndex: Integer;
        procedure CheckOpen;
        procedure CheckWrite;
        constructor Create(Field: TBLobField; Mode: TBlobStreamMode;
            ALobLocatorClass: TOCILobLocatorClass);
        constructor CreateUsingParam(Param: TOciParam; AIndex: Integer;
            Mode: TBlobStreamMode; ALobLocatorClass: TOCILobLocatorClass);
        constructor CreateAlone(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType;
            ALobLocatorClass: TOCILobLocatorClass);
        constructor CreateTemporary(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType;
            ALobLocatorClass: TOCILobLocatorClass; ADummy: Integer = 0);
    public
        destructor Destroy; override;
        function IsNull: Boolean;
        function IsTemporary: Boolean;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Write(const Buffer; Count: Longint): Longint; override;
        function Seek(Offset: Longint; Origin: Word): Longint; override;
        procedure SetSize(NewSize: Longint); override;
        procedure Truncate; virtual;
        procedure Assign(ASource: TObject);
        property HLocator: TOCILobLocator read FOCILobLocator;
    end;

    TOCIFILEStream = class(TOCILOBStream)
    private
        function GetFileName: String;
        procedure SetFileName(const AValue: String);
        function GetDirectory: String;
        procedure SetDirectory(const AValue: String);
        function GetFileExists: Boolean;
        function GetLocator: TOCIExtLocator;
    public
        constructor Create(Field: TBLobField; Mode: TBlobStreamMode);
        constructor CreateUsingParam(Param: TOciParam; AIndex: Integer;
            Mode: TBlobStreamMode);
        constructor CreateAlone(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType);
        constructor CreateTemporary(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType; ADummy: Integer = 0);
        property FileExists: Boolean read GetFileExists;
        property FileName: String read GetFileName write SetFileName;
        property Directory: String read GetDirectory write SetDirectory;
        property HLocator: TOCIExtLocator read GetLocator;
    end;

    TOCIILOBStream = class(TOCILOBStream)
    private
        function GetLocator: TOCIIntLocator;
    public
        constructor Create(Field: TBLobField; Mode: TBlobStreamMode);
        constructor CreateUsingParam(Param: TOciParam; AIndex: Integer;
            Mode: TBlobStreamMode);
        constructor CreateAlone(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType);
        constructor CreateTemporary(ADatabase: TOCICustomDatabase;
            Mode: TBlobStreamMode; AODataType: TOCIVarDataType; ADummy: Integer = 0);
        function Write(const Buffer; Count: Longint): Longint; override;
        procedure SetSize(NewSize: Integer); override;
        property HLocator: TOCIIntLocator read GetLocator;
    end;

    TOCIFieldDef = class(TCollectionItem)
    private
        FName: String;
        FIsNull: Boolean;
        FDataType: TOCIVarDataType;
        FDataSize: ub4;
        FScale: ub4;
        FPrecision: ub4;
        FFieldNo: sb4;
    public
        procedure Assign(AObject: TPersistent); override;
        property Name: String read FName;
        property IsNull: Boolean read FIsNull;
        property DataType: TOCIVarDataType read FDataType;
        property DataSize: ub4 read FDataSize;
        property Scale: ub4 read FScale;
        property Precision: ub4 read FPrecision;
        property FieldNo: sb4 read FFieldNo;
    end;

    TOCIFieldDefs = class(TCollection)
    private
        function GetFieldDef(AIndex: Integer): TOCIFieldDef;
        procedure SetFieldDef(AIndex: Integer; const Value: TOCIFieldDef);
    public
        constructor Create;
        function FindDef(const AName: String): TOCIFieldDef;
        function FindByNo(AFieldNo: Integer): TOCIFieldDef;
        function Add: TOCIFieldDef;
        property Items[AIndex: Integer]: TOCIFieldDef read GetFieldDef write SetFieldDef; default;
    end;

    TOCISQLKind = (skLock, skUnlock, skUpdate, skInsert, skDelete, skRefresh);
    TOCISQLKinds = set of TOCISQLKind;
    TOCISQLKindOption = (soImmediate, soSave, soBlobsOnly, soNoRecord);
    TOCISQLKindOptions = set of TOCISQLKindOption;
{$IFNDEF OCI_D4}
    TUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
{$ENDIF}
    TOCIUpdateObject = class(TComponent)
    private
        FDataSet: TOCIDataSet;
        FUpdateMode: TUpdateMode;
        FApplyOptions: TOCISQLKindOptions;
    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure CheckNoUpdatesPending;
        function GetSupported(ASQLKind: TOCISQLKind): Boolean; virtual; abstract;
        procedure SetUpdateMode(const AValue: TUpdateMode); virtual;
        procedure InternalApply(ASQLKind: TOCISQLKind); virtual; abstract;
    public
        destructor Destroy; override;
        procedure Apply(ASQLKind: TOCISQLKind; AOptions: TOCISQLKindOptions);
        procedure DisconnectAll; virtual; abstract;
        property DataSet: TOCIDataSet read FDataSet;
        property Supported[ASQLKind: TOCISQLKind]: Boolean read GetSupported;
        property ApplyOptions: TOCISQLKindOptions read FApplyOptions;
        property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode;
    end;

    TOCIRecordCountMode = (cmFetched, cmExactOnOpen, cmExactAllways);
    TOCIUpdateRecordType = (rtModified, rtInserted, rtDeleted, rtUnmodified);
    TOCIUpdateRecordTypes = set of TOCIUpdateRecordType;
    TOCIUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TOCISQLKind;
        var UpdateAction: TOCIUpdateAction) of object;
    TOCIUpdateErrorEvent = procedure(DataSet: TOCIDataSet; E: EDatabaseError;
        UpdateKind: TOCISQLKind; var UpdateAction: TOCIUpdateAction) of object;
    TOCICachedUpdatesArea = (caDefault, caDataSet, caDatabase);
    TOCIExprKind = (ekRecCons, ekFieldCons, ekFieldDef);
    TOCIExprKinds = set of TOCIExprKind;
{$IFNDEF OCI_D4}
    TFieldKinds = set of TFieldKind;
{$ENDIF}

    TOCIDataSet = class(TOCICustomWrappedDataSet)
    private
        FDatabase: TOCICustomDatabase;
        FTransactionManager: TOCITransactionManager;
        FDatabaseName: String;
        FOCICursor: TOCICursor;
        FRecordSize: Word;
        FRecInfoOfs: Word;
        FRecBufSize: Word;
        FDBUseCount: Integer;
        FCachedUpdates: Boolean;
        FCachedUpdatesArea: TOCICachedUpdatesArea;
        FFetchParams: TOCIFetchParams;
        FDataFormat: TOCIDataFormat;
        FHaveBLOBs: Boolean;
        FFilters: TOCIFilters;
        FFilterBuffer: PChar;
        FInFindRecord: Boolean;
        FROWIDFieldNo: Integer;
        FRecordCount: Integer;
        FRecordCountMode: TOCIRecordCountMode;
        FBeforeRecordCount: TDataSetNotifyEvent;
        FAfterRecordCount: TDataSetNotifyEvent;
        FFilterUseCalcs: Boolean;
        FUnidirectional: Boolean;
        FOFieldDefs: TOCIFieldDefs;
        FUpdateObject: TOCIUpdateObject;
        FOnUpdateRecord: TOCIUpdateRecordEvent;
        FOnUpdateError: TOCIUpdateErrorEvent;
        FLastCommand: TOCISQLKind;
        FReadOnly: Boolean;
        FExpressions: TOCIStringList;
        FCurValidate: TFieldNotifyEvent;
        FExprKinds: TOCIExprKinds;
        FConstDisableCount: Integer;
        FPostSeqList: TList;
        FUsageCount: Integer;
        FDisconnectable: Boolean;
        FKeyFields: String;
        FAllowedOperations: TOCISQLKinds;
        FOnError: TOCIErrorEvent;
        FOnWarning: TOCIWarningEvent;

        procedure SetDatabaseName(const Value: String);
        procedure InitBufferPointers;
        function OCIrs2ds(ARS: TOCIRecordStatus): TUpdateStatus;
        procedure SetCachedUpdates(AValue: Boolean);
        procedure SetFetchParams(AValue: TOCIFetchParams);
        function GetOItem(AFieldNo: Integer): TOCIFieldDef; 
        function GetOName(AFieldNo: Integer): String;
        function GetODataType(AFieldNo: Integer): TOCIVarDataType;
        function GetODataSize(AFieldNo: Integer): ub4;
        procedure SetFilters(AValue: TOCIFilters);
        procedure ExecuteFilter(ASender: TOCICursor; var AAccept: Boolean);
        procedure SetDataFormat(const Value: TOCIDataFormat);
        function IsFS: Boolean;
        function IsDBNS: Boolean;
        procedure SetTransactionManager(const Value: TOCITransactionManager);
        procedure SetUnidirectional(const Value: Boolean);
        procedure UpdateRecordHandler(Sender: TOCICursor; var AAction: TOCIUpdateAction);
        procedure UpdateErrorHandler(Sender: TOCICursor; E: Exception; var AAction: TOCIUpdateAction);
        procedure InternalUpdateProcessor(ACommand: TOCISQLKind;
            var AAction: TOCIUpdateAction; AOptions: TOCISQLKindOptions);
        procedure SetUpdateObject(const Value: TOCIUpdateObject);
        procedure SetUpdateRecordTypes(const Value: TOCIUpdateRecordTypes);
        function GetUpdateRecordTypes: TOCIUpdateRecordTypes;
        function GetUpdatesPending: Boolean;
        procedure CheckCachedUpdates;
        procedure SetOnUpdateError(const Value: TOCIUpdateErrorEvent);
        procedure SetOnUpdateRecord(const Value: TOCIUpdateRecordEvent);
        procedure SetReadOnly(const Value: Boolean);
        function GetInApplyUpdates: Boolean;
        procedure DoValidate(ASender: TField);
        function IsCS: Boolean;
        function GetDescribed: Boolean;
        function GetUpdateMode: TUpdateMode;
        function IsUMs: Boolean;
        procedure SetUpdateMode(const Value: TUpdateMode);
        function GetRecordLocked: Boolean;
        procedure SetRecordLocked(AValue: Boolean);
        function GetPointedDatabase: TOCICustomDatabase;
        procedure CheckAllowedOperations(AOperation: TOCISQLKind);
        function GetFieldIsChanged(AIndex: Integer): Boolean;
        procedure SetCachedUpdatesArea(AValue: TOCICachedUpdatesArea);
        procedure UpdateCachedUpdatesProp(ACachedUpdates: Boolean;
            ACachedUpdatesArea: TOCICachedUpdatesArea);
    protected
        function AllocRecordBuffer: PChar; override;
        procedure FreeRecordBuffer(var Buffer: PChar); override;
        procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
        function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
        function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
        function GetRecordSize: Word; override;
        procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
        procedure InternalClose; override;
        procedure InternalDelete; override;
        procedure InternalFirst; override;
        procedure InternalGotoBookmark(Bookmark: Pointer); override;
        procedure InternalHandleException; override;
        procedure InternalInitFieldDefs; override;
        procedure InternalInitRecord(Buffer: PChar); override;
        procedure InternalLast; override;
        procedure InternalOpen; override;
        procedure DoAfterOpen; override;
        procedure InternalPost; override;
        procedure InternalSetToRecord(Buffer: PChar); override;
        procedure InternalCancel; override;
        procedure InternalEdit; override;
        procedure DoBeforeInsert; override;
        procedure UpdateProcessor(ACommand: TOCISQLKind; AOptions: TOCISQLKindOptions);
        function IsCursorOpen: Boolean; override;
        procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
        procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
        procedure SetFieldData(Field: TField; Buffer: Pointer); override;
        procedure InitRecord(Buffer: PChar); override;
        procedure DoNavigationalCount;
        function GetRecordCount: Integer; override;
        function GetRecNo: Integer; override;
        procedure SetRecNo(Value: Integer); override;
        procedure CloseBlob(Field: TField); override;
{$IFNDEF OCI_D5}
        function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; override;
        function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
            Decimals: Integer): Boolean; override;
{$ELSE}            
        procedure DataConvert(Field: TField; Source, Dest: Pointer;
            ToNative: Boolean); override;
{$ENDIF}
        procedure ClearCalcFields(Buffer: PChar); override;
        function GetCanModify: Boolean; override;
        procedure SetFiltered(Value: Boolean); override;
        procedure SetFilterOptions(Value: TFilterOptions); override;
        procedure SetFilterText(const Value: string); override;
        procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
        procedure IncDBUse;
        procedure DecDBUse;
        procedure OBindFields(Binding: Boolean);
        procedure UpdateFiltering(AFilterChanged, ACursorActive: Boolean);
        procedure RegisterFilterField(AField: TField);
        function FindRecord(Restart, GoForward: Boolean): Boolean; override;
        function LocateRecord(const AKeyFields: string; const AKeyValues: Variant;
            AOptions: TLocateOptions): Boolean;
        procedure OpenCursor(InfoQuery: Boolean); override;
        procedure CloseCursor; override;
        procedure DescribeOCICursor; virtual; abstract;
        procedure CreateOCICursor; virtual; abstract;
        procedure DestroyOCICursor; virtual; abstract;
        procedure UpdateRecordCount; virtual;
        procedure AcquireTM; virtual;
        procedure ReleaseTM; virtual;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        function GetTableName: String; virtual;
        procedure CheckNoUpdatesPending;
        function GetActualUpdateObject: TOCIUpdateObject; virtual;
        procedure ClearExpressions;
        procedure EvaluteChecks(AField: TField);
        procedure EvaluteDefaults(AFieldKinds: TFieldKinds);
        procedure PrepareExpressions;
        procedure DoOnNewRecord; override;
        procedure InternalRefreshRecord; virtual;
        procedure InternalRefresh; override;
        procedure DoOnCalcFields; override;
        procedure DoAfterPost; override;
        procedure DoBeforePost; override;
        function GetSeq(const AName: String; APostSeqOnPost: Boolean): TOCISequence;
        procedure DoError(ASender: TObject; var E: Exception); virtual;
        procedure DoWarning(ASender: TObject; var E: Exception); virtual;
        property RecordCountMode: TOCIRecordCountMode read FRecordCountMode
            write FRecordCountMode default cmFetched;
        property Unidirectional: Boolean read FUnidirectional
            write SetUnidirectional default False;
        property LockMark: Boolean read GetRecordLocked write SetRecordLocked;
{$IFDEF OCI_D5}
    protected
        function PSInTransaction: Boolean; override;
        procedure PSStartTransaction; override;
        procedure PSEndTransaction(Commit: Boolean); override;
        function PSExecuteStatement(const ASQL: string; AParams: TParams;
            ResultSet: Pointer = nil): Integer; override;
        function PSGetQuoteChar: string; override;
        function PSGetTableName: string; override;
        function PSIsSQLBased: Boolean; override;
        function PSIsSQLSupported: Boolean; override;
        procedure PSReset; override;
        function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
        function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
{$ENDIF}
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Disconnect; virtual;
        function GetCurrentRecord(Buffer: PChar): Boolean; override;
        procedure Resync(Mode: TResyncMode); override;
{$IFDEF OCI_D4}
        function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
        function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
{$ELSE}
        function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
        function GetFieldDataByNo(FieldNo: Integer; Buffer: Pointer): Boolean;
{$ENDIF}
        function GetActiveRecBuf(var RecBuf: PChar): Boolean;
        function GetEditBookmark(Buffer: PChar): POCIBookmark;
        procedure FetchAll;
        procedure OpenExact(ANRows: sb4);
        function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
        procedure BlobModified(AField: TField; AIsNull: Boolean);
        procedure ResetLOBs;
        function IsSequenced: Boolean; override;
        function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
        function BookmarkValid(Bookmark: TBookmark): Boolean; override;
        function Lookup(const KeyFields: string; const KeyValues: Variant;
            const ResultFields: string): Variant; override;
        function Locate(const KeyFields: string; const KeyValues: Variant;
            Options: TLocateOptions): Boolean; override;
        procedure ApplyUpdates;
        procedure CancelUpdates;
        procedure CommitUpdates;
        procedure RevertRecord;
        procedure RefreshRecord;
        function UpdateStatus: TUpdateStatus; {$IFDEF OCI_D4} override; {$ENDIF}
        function RecordsEqual(ADataSet: TOCIDataSet; AThisValType,
            ADataSetValType: TDataSetState): Boolean;
        function ConstraintsDisabled: Boolean;
        procedure DisableConstraints;
        procedure EnableConstraints;
        procedure LoadConstraints(const AOwner, ATableName: String);
        procedure LoadDefaults(const AOwner, ATableName: String);
        procedure CloseDatabase(Database: TOCICustomDatabase);
        function OpenDatabase: TOCICustomDatabase;
        property Database: TOCICustomDatabase read FDatabase;
        property PointedDatabase: TOCICustomDatabase read GetPointedDatabase;
        property OFieldDefs: TOCIFieldDefs read FOFieldDefs;
        property OName[AFieldNo: Integer]: String read GetOName;
        property ODataType[AFieldNo: Integer]: TOCIVarDataType read GetODataType;
        property ODataSize[AFieldNo: Integer]: ub4 read GetODataSize;
        property OItem[AFieldNo: Integer]: TOCIFieldDef read GetOItem;
        property HaveBLOBs: Boolean read FHaveBLOBs;
        property Described: Boolean read GetDescribed;
        property TableName: String read GetTableName;
        property HCursor: TOCICursor read FOCICursor;
        property Filters: TOCIFilters read FFilters write SetFilters stored IsFS;
        property Filter stored False;
        property Filtered stored False;
        property FilterOptions stored False;
        property Constraints stored IsCS;
        property KeyFields: String read FKeyFields;
        property UsageCount: Integer read FUsageCount;
        property ActualUpdateObject: TOCIUpdateObject read GetActualUpdateObject;
        property InApplyUpdates: Boolean read GetInApplyUpdates;
        property UpdatesPending: Boolean read GetUpdatesPending;
        property UpdateRecordTypes: TOCIUpdateRecordTypes read GetUpdateRecordTypes
            write SetUpdateRecordTypes;
        property CachedUpdates: Boolean read FCachedUpdates
            write SetCachedUpdates default False;
        property CachedUpdatesArea: TOCICachedUpdatesArea read FCachedUpdatesArea
            write SetCachedUpdatesArea default caDefault;
        property UpdateObject: TOCIUpdateObject read FUpdateObject
            write SetUpdateObject;
        property UpdateMode: TUpdateMode read GetUpdateMode write SetUpdateMode
            stored IsUMs default upWhereKeyOnly;
        property OnUpdateRecord: TOCIUpdateRecordEvent read FOnUpdateRecord
            write SetOnUpdateRecord;
        property OnUpdateError: TOCIUpdateErrorEvent read FOnUpdateError
            write SetOnUpdateError;
        property FieldIsChanged[AIndex: Integer]: Boolean read GetFieldIsChanged;
        property DatabaseName: String read FDatabaseName write SetDatabaseName
            stored IsDBNS;
        property TransactionManager: TOCITransactionManager read FTransactionManager
            write SetTransactionManager;
//???        property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh default False;
    published
        property Active;
        property AutoCalcFields;
        property BeforeOpen;
        property AfterOpen;
        property BeforeClose;
        property AfterClose;
        property BeforeInsert;
        property AfterInsert;
        property BeforeEdit;
        property AfterEdit;
        property BeforePost;
        property AfterPost;
        property BeforeCancel;
        property AfterCancel;
        property BeforeDelete;
        property AfterDelete;
        property BeforeScroll;
        property AfterScroll;
{$IFDEF OCI_D5}
        property BeforeRefresh;
        property AfterRefresh;
{$ENDIF}
        property OnCalcFields;
        property OnDeleteError;
        property OnEditError;
        property OnNewRecord;
        property OnPostError;
        property OnError: TOCIErrorEvent read FOnError write FOnError;
        property OnWarning: TOCIWarningEvent read FOnWarning write FOnWarning;
        property BeforeRecordCount: TDataSetNotifyEvent read FBeforeRecordCount
            write FBeforeRecordCount;
        property AfterRecordCount: TDataSetNotifyEvent read FAfterRecordCount
            write FAfterRecordCount;
        property FetchParams: TOCIFetchParams read FFetchParams
            write SetFetchParams;
        property DataFormat: TOCIDataFormat read FDataFormat
            write SetDataFormat;
        property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
        property AllowedOperations: TOCISQLKinds read FAllowedOperations
            write FAllowedOperations default [skLock .. skRefresh];
        property Disconnectable: Boolean read FDisconnectable write FDisconnectable
            default False;
    end;

    TOCIStatementType = (stUnknown, stSelect, stUpdate, stDelete, stInsert,
        stCreate, stDrop, stAlter, stBegin, stDeclare);
    TOCIExecAction = (eaFail, eaAbort, eaSkip, eaRetry, eaApplied, eaReRaise,
        eaExitSuccess, eaExitFailure);
    TOCIExecErrorEvent = procedure (ASender: TObject; ATimes, AOffset: Integer;
        E: Exception; var AExecAction: TOCIExecAction) of object;
    TOCIStatementDataSet = class(TOCIDataSet)
    private
        FStreamedPrepared: Boolean;
        FPrepared: Boolean;
        FHaveBLOBs: Boolean;
        FBeforePrepare: TNotifyEvent;
        FAfterPrepare: TNotifyEvent;
        FBeforeUnprepare: TNotifyEvent;
        FAfterUnprepare: TNotifyEvent;
        FLocal: Boolean;
        FRowsAffected: Integer;
        FInBindParamDataSet: Boolean;
        FOnExecError: TOCIExecErrorEvent;
        FCachedOCICursor: TOCICursor;
        procedure SetPrepared(Value: Boolean);
        procedure CreateStatement;
        procedure FreeStatement;
        function GetStatementType: TOCIStatementType;
    protected
        FStatement: TOCIStatement;
        FCrsStatement: TOCIStatement;
        procedure Loaded; override;
{$IFDEF OCI_D4}
        procedure DataEvent(Event: TDataEvent; Info: Integer); override;
{$ENDIF}        
        procedure DescribeOCICursor; override;
        procedure CreateOCICursor; override;
        procedure DestroyOCICursor; override;
        procedure InternalCreateStatement; virtual;
        procedure InternalFreeStatement; virtual;
        procedure InternalStatementBind(AForOpen: Boolean); virtual;
        procedure InternalStatementUnBind; virtual;
        function GetStatementText: String; virtual;
        procedure BindParamDataSet(ABindTo: Boolean); virtual;
        function ExecArray(ATimes, AOffset: Integer): Boolean;
        function Exec {$IFDEF OCI_D4} (ATimes: Integer = 0; AOffset: Integer = 0) {$ENDIF}: Boolean;
        property RowsAffected: Integer read FRowsAffected;
        property OnExecError: TOCIExecErrorEvent read FOnExecError write FOnExecError;
    public
        constructor Create(AOwner: TComponent); override;
        procedure Disconnect; override;
        procedure Prepare;
        procedure UnPrepare;
        property HStatement: TOCIStatement read FStatement;
        property HCrsStatement: TOCIStatement read FCrsStatement;
        property StatementType: TOCIStatementType read GetStatementType;
        property StatementText: String read GetStatementText;
        property Local: Boolean read FLocal;
    published
        property Prepared: Boolean read FPrepared write SetPrepared default False;
        property BeforePrepare: TNotifyEvent read FBeforePrepare write FBeforePrepare;
        property AfterPrepare: TNotifyEvent read FAfterPrepare write FAfterPrepare;
        property BeforeUnprepare: TNotifyEvent read FBeforeUnprepare write FBeforeUnprepare;
        property AfterUnprepare: TNotifyEvent read FAfterUnprepare write FAfterUnprepare;
        property Filtered;
        property FilterOptions;
        property Filter;
        property Filters;
        property Constraints;
        property OnFilterRecord;
        property CachedUpdates;
        property CachedUpdatesArea;
        property UpdateObject;
{$IFDEF OCI_D4}
        property ObjectView default True;
{$ENDIF}
        property OnUpdateRecord;
        property OnUpdateError;
    end;

{$IFNDEF OCI_D4}
    TDetailDataLink = class(TDataLink)
    protected
        function GetDetailDataSet: TDataSet; virtual;
    public
        property DetailDataSet: TDataSet read GetDetailDataSet;
    end;
{$ENDIF}

    TOCIQueryDataLink = class(TDetailDataLink)
    private
        FQuery: TOCICustomQuery;
    protected
        procedure ActiveChanged; override;
        procedure RecordChanged(Field: TField); override;
        function GetDetailDataSet: TDataSet; override;
        procedure CheckBrowseMode; override;
    public
        constructor Create(AQuery: TOCICustomQuery);
    end;

    TOCIChangingKind = (ckMacros, ckSQL, ckLockParse);
    TOCIChangingKinds = set of TOCIChangingKind;
    TOCICustomQuery = class(TOCIStatementDataSet)
    private
        FSQL: TStrings;
        FParams: TOciParams;
        FMacros: TOciMacros;
        FParamCheck, FMacroCheck: Boolean;
        FExpandedSQL: String;
        FSelfChanging: TOCIChangingKinds;
        FDefaultUpdateSQL: TOCIUpdateObject;
        FDataLink: TDetailDataLink;
{$IFDEF OCI_D5}
        FProviderParams: TParams;
{$ENDIF}
        FParamDataSet: TStringList;
        FSQLFromTable: String;
        FSQLOrderByPos: Integer;
        procedure ReadParamData(Reader: TReader);
        procedure ReadBool(Reader: TReader);
        class function ParseSQL(AParams: TOciParams; AMacrosUpd, AMacrosRead: TOciMacros;
            const AQuery: String; ACreateParams, ACreateMacros, ABindByName: Boolean;
            var AFrom: String; var AOrderByPos: Integer): String;
        procedure DoSQLChange(ASender: TObject);
        procedure SetSQL(AValue: TStrings);
        function GetMacroCount: Word;
        function GetParamCount: Word;
        function GetBindMode: TOCIParamBindMode;
        procedure SetBindMode(const Value: TOCIParamBindMode);
        function IsMS: Boolean;
        function IsPS: Boolean;
        procedure SetDataSource(Value: TDataSource);
        procedure SetParamsFromCursor;
        procedure RefreshParams;
        procedure AddParamDataSet(const AParamName: String; ADataSet: TOCIStatementDataSet);
        procedure RemoveParamDataSet(const AParamName: String; ADataSet: TOCIStatementDataSet);
        function GetParamDataSetCount: Integer;
        function GetParamDataSets(AIndex: Integer): TOCIStatementDataSet;
    protected
        procedure DefineProperties(Filer: TFiler); override;
        procedure UpdateRecordCount; override;
        procedure Loaded; override;
        procedure SetParams(AValue: TOciParams); virtual;
        procedure SetMacros(AValue: TOciMacros); virtual;
        function GetActualUpdateObject: TOCIUpdateObject; override;
        function GetDataSource: TDataSource; override;
        procedure InternalStatementBind(AForOpen: Boolean); override;
        procedure InternalStatementUnBind; override;
        procedure InternalCreateStatement; override;
        procedure InternalFreeStatement; override;
        function GetStatementText: String; override;
        procedure BindParamDataSet(ABindTo: Boolean); override;
        property DataLink: TDetailDataLink read FDataLink;
        property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
        property MacroCheck: Boolean read FMacroCheck write FMacroCheck default True;
        property SQL: TStrings read FSQL write SetSQL;
        property Text: string read FExpandedSQL;
        property Macros: TOCIMacros read FMacros write SetMacros stored IsMS;
        property MacroCount: Word read GetMacroCount;
        property DataSource: TDataSource read GetDataSource write SetDataSource;
{$IFDEF OCI_D5}
    protected
        function PSGetParams: TParams; override;
        procedure PSSetParams(AParams: TParams); override;
        procedure PSExecute; override;
        procedure PSSetCommandText(const CommandText: string); override;
{$ENDIF}        
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function ParamByName(const Value: string): TOCIParam;
        function FindParam(const Value: string): TOCIParam;
        function MacroByName(const Value: string): TOCIMacro;
        function FindMacro(const Value: string): TOCIMacro;
        procedure MacrosChanged;
        class procedure FillParams(AParams: TOCIParams; const ASQL: String);
        procedure GetDetailLinkFields(MasterFields, DetailFields: TList); {$IFDEF OCI_D4} override; {$ENDIF}
        function FindParamDataSet(AParam: TOCIParam): TOCIStatementDataSet;
        property ParamCount: Word read GetParamCount;
        property ParamDataSetCount: Integer read GetParamDataSetCount;
        property ParamDataSets[AIndex: Integer]: TOCIStatementDataSet read GetParamDataSets;
    published
        property Params: TOciParams read FParams write SetParams stored IsPS;
        property ParamBindMode: TOCIParamBindMode read GetBindMode write SetBindMode
            default pbByName;
        property Unidirectional;
        property UpdateMode;
        property DatabaseName;
        property TransactionManager;
        property OnExecError;
    end;

    TOCIQuery = class(TOCICustomQuery)
    protected
        function GetTableName: String; override;
    public
        function ExecSQL {$IFDEF OCI_D4} (ATimes: Integer = 0; AOffset: Integer = 0) {$ENDIF}: Boolean;
        function ExecSQLArray(ATimes, AOffset: Integer): Boolean;
        property Text;
        property RowsAffected;
        property StatementType;
        property MacroCount;
    published
        property ParamCheck;
        property MacroCheck;
        property Macros;
        property SQL;
        property RecordCountMode;
        property DataSource;
    end;

    TOCIPLSQLDescriber = class
    private
        // setup
        FOwner: TComponent;
        FDatabase: TOCICustomDatabase;
        FDataFormat: TOCIDataFormat;
        FOPackageName, FOProcedureName: String;
        FOverload: Integer;
        // run time
        FDescr: TOCIDescribe;
        FSPName: String;
        FObjType: ub1;
        FNumProcs: ub2;
        FProcIndex: ub2;
        FForProc: Boolean;
    public
        constructor CreateForProc(AOwner: TComponent; ADatabase: TOCICustomDatabase;
            ADataFormat: TOCIDataFormat; AOPackageName, AOProcedureName: String;
            AOverload: Integer);
        constructor CreateForPack(AOwner: TComponent; ADatabase: TOCICustomDatabase;
            ADataFormat: TOCIDataFormat; AOPackageName: String);
        destructor Destroy; override;
        procedure Describe;
        procedure LocateProc;
        procedure First(var AProcName: String; var AOverload: Integer);
        procedure Next(var AProcName: String; var AOverload: Integer);
        function EOL: Boolean;
        procedure BuildSQL(ASQL: TStrings; AParams: TOCIParams);
        procedure CleanUp;
        property Descr: TOCIDescribe read FDescr;
        property ObjType: ub1 read FObjType;
    end;

    TOCIStoredProc = class(TOCICustomQuery)
    private
        FStoredProcName: String;
        FOPackageName: String;
        FOProcedureName: String;
        FOverload: Word;
        procedure SetStoredProcName(const AValue: String);
        function SplitName(const AProcName: String; var AOPackageName,
            AOProcedureName: String): Boolean;
        procedure BuildSPQuery(const AOPackageName, AOProcedureName: String);
        procedure SetOPackName(const Value: String);
        procedure SetOProcName(const Value: String);
        procedure SetOverload(const Value: Word);
        procedure ProcNameChanged(AFullName: Boolean);
    protected
        procedure InternalCreateStatement; override;
        procedure InternalFreeStatement; override;
        procedure SetParams(AValue: TOciParams); override;
        procedure UpdateRecordCount; override;
{$IFDEF OCI_D5}
    protected
        procedure PSSetCommandText(const CommandText: string); override;
{$ENDIF}
    public
        constructor Create(AOwner: TComponent); override;
        procedure ExecProc;
        procedure CopyParams(Value: TOCIParams);
        function DescriptionsAvailable: Boolean;
    published
        property StoredProcName: String read FStoredProcName write SetStoredProcName
            stored False;
        property OPackageName: String read FOPackageName write SetOPackName;
        property OProcedureName: String read FOProcedureName write SetOProcName;
        property Overload: Word read FOverload write SetOverload default 0;
        property RecordCountMode;
    end;

    TOCIPLSQLType = class(TPersistent)
    public
        constructor Create; virtual;
    end;
    TOCIPLSQLTypeClass = class of TOCIPLSQLType;

    TOCIPLSQLRecord = class(TOCIPLSQLType)
    public
        constructor Create; override;
    end;

    TOCIPLSQLTable = class(TOCIPLSQLType)
    private
        FValue: Variant;
        FCount, FFirst, FLast: Integer;
        function GetItems(AIndex: Integer): Variant;
        procedure SetItems(AIndex: Integer; const Value: Variant);
    protected
        procedure AssignTo(ADest: TPersistent); override;
    public
        constructor Create; override;
        procedure Assign(ASource: TPersistent); override;
{$IFDEF OCI_D4}
        procedure Delete; overload;
        procedure Delete(AInd1: Integer); overload;
        procedure Delete(AInd1, AInd2: Integer); overload;
{$ELSE}
        procedure DeleteAll;
        procedure DeleteOne(AInd1: Integer);
        procedure Delete(AInd1, AInd2: Integer);
{$ENDIF}
        function Exists(AIndex: Integer): Boolean;
        function Next(AIndex: Integer): Integer;
        function Prior(AIndex: Integer): Integer;
        property Count: Integer read FCount;
        property First: Integer read FFirst;
        property Last: Integer read FLast;
        property Items[AIndex: Integer]: Variant read GetItems write SetItems; default;
    end;

    TOCICustomPackage = class(TComponent)
    private
        FQueries: TList;
        FDatabaseName: String;
        procedure SetDatabaseName(const AValue: String);
    protected
        procedure SetProcCount(ANum: Integer);
        function GetQuery(AIndex: Integer): TOCIQuery;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    published
        property DatabaseName: String read FDatabaseName write SetDatabaseName;
    end;

    TOCISequence = class(TComponent)
    private
        FQuery: TOCIQuery;
        FDatabaseName, FSequenceName: String;
        FCommited: Boolean;
        FAutoPost: Boolean;
        FValue: Double;
        function GetCurVal: Double;
        function GetNextVal: Double;
        procedure BuildQuery(ANextVal: Boolean);
    public
        destructor Destroy; override;
        constructor Create(AOwner: TComponent); override;
        procedure Post;
        procedure Refresh;
        property CurrVal: double read GetCurVal;
        property NextVal: double read GetNextVal;
    published
        property DatabaseName: String read FDatabaseName write FDatabaseName;
        property SequenceName: String read FSequenceName write FSequenceName;
        property AutoPost: Boolean read FAutoPost write FAutoPost
            default False;
    end;

    TOCINestedDataSet = class(TOCIStatementDataSet)
    private
        FParamDataSet: TOCICustomQuery;
        FParamName: String;
        FInParentScroll: Boolean;
        FCachedOCICursorBmk: POCIBookmark;
        FCachedActive: Boolean;
        procedure SetParamDataSet(const Value: TOCICustomQuery);
        procedure SetParamName(const Value: String);
        function GetParentDataSet: TOCIStatementDataSet;
        procedure ParentObjectUpdated(ALinkTo: Boolean);
{$IFDEF OCI_D4}
        function GetCacheCursors: Boolean;
{$ENDIF}
    protected
        procedure InternalCreateStatement; override;
        procedure InternalFreeStatement; override;
        procedure InternalStatementUnBind; override;
{$IFDEF OCI_D4}
        procedure SetDataSetField(const Value: TDataSetField); override;
{$ENDIF}
        procedure DestroyOCICursor; override;
        procedure UpdateRecordCount; override;
{$IFDEF OCI_D4}
        procedure DataEvent(Event: TDataEvent; Info: Integer); override;
        procedure RestoreActiveState;
        function PSGetParams: TParams; override;
        procedure PSSetParams(AParams: TParams); override;
        procedure PSExecute; override;
        procedure PSSetCommandText(const CommandText: string); override;
{$ENDIF}
    public
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        property ParentDataSet: TOCIStatementDataSet read GetParentDataSet;
        procedure CheckParentDataSet;
    published
        property ParamDataSet: TOCICustomQuery read FParamDataSet write SetParamDataSet;
        property ParamName: String read FParamName write SetParamName;
{$IFDEF OCI_D4}
        property DataSetField;
{$ENDIF}
        property RecordCountMode;
    end;

const
{$IFDEF OCI_D5}
    ot2dt: array [TOCIVarDataType] of TFieldType = (ftUnknown, ftSmallInt,
        ftInteger, ftFloat, ftBCD, ftString, ftString, ftFixedChar,
        ftVarBytes, ftDateTime, ftMemo, ftBlob, ftCursor, ftDataSet,
        ftOraClob, ftOraBlob, ftOraClob, ftOraBlob, ftString);
{$ELSE}
{$IFDEF OCI_D4}
    ot2dt: array [TOCIVarDataType] of TFieldType = (ftUnknown, ftSmallInt,
        ftInteger, ftFloat, ftBCD, ftString, ftString, ftFixedChar,
        ftVarBytes, ftDateTime, ftMemo, ftBlob, ftCursor, ftDataSet,
        ftMemo, ftBlob, ftMemo, ftBlob, ftString);
{$ELSE}
    ot2dt: array [TOCIVarDataType] of TFieldType = (ftUnknown, ftSmallInt,
        ftInteger, ftFloat, ftBCD, ftString, ftString, ftString,
        ftVarBytes, ftDateTime, ftMemo, ftBlob, ftCursor, ftUnknown,
        ftMemo, ftBlob, ftMemo, ftBlob, ftString);
{$ENDIF}
{$ENDIF}
    ot2var: array [TOCIVarDataType] of Integer = (varEmpty, varSmallInt,
        varInteger, varDouble, varCurrency, varString, varString, varString,
        varString, varDate, varString, varString, varEmpty, varEmpty, varEmpty,
        varEmpty, varEmpty, varEmpty, varString);
    dt2ot: array [TFieldType] of TOCIVarDataType = (
        otUnknown, otString, otSmallInt, otInteger, otInteger, otUnknown, otFloat,
        otBCD, otBCD, otDateTime, otDateTime, otDateTime, otRaw, otRaw,
        otInteger, otLongRaw, otLong, otUnknown, otUnknown, otUnknown, otUnknown,
        otUnknown, otCursor
{$IFDEF OCI_D4}
        , otChar, otUnknown, otInteger, otUnknown, otUnknown, otUnknown, otNestedDataSet
{$ENDIF}
{$IFDEF OCI_D5}
        , otBLOB, otCLOB, otUnknown, otUnknown, otUnknown, otUnknown
{$ENDIF}
{$IFDEF OCI_D6}
        , otUnknown, otUnknown
{$ENDIF}
{$IFDEF OCI_D10}
        , otString, otLong, otString, otString
{$ENDIF}
        );

    pt2vt: array[TParamType] of TOCIVarType = (
        odUnknown, odIn, odOut, odInOut, odOut);
    vt2pt: array[TOCIVarType] of TParamType = (
        ptUnknown, ptInput, ptOutput, ptInputOutput, ptOutput, ptUnknown);

var
    FOCIDatabaseNameExpandMode: TOCIDatabaseNameExpandMode;

    function Var2SQL(const AValue: Variant; const ADataFormat: TOCIDataFormat): String;
    function Var2SQLTyped(const AValue: Variant; AFieldType: TOCIMacroDataType;
        const ADataFormat: TOCIDataFormat): String;
    function VarType2FieldType(const AValue: Variant): TOCIMacroDataType;

implementation

Uses Forms, Dialogs, Consts, DBConsts, NCOciMsg, NCOciLoginDlg, NCOciErrorDlg,
     NCOciDM, NCOciUtil, NCOciBreakDlg, NCOciUpdateSQL
{$IFDEF OCI_D6}
     , Variants, RTLConsts
{$ENDIF}
     ;

const
    arrOnOff: array[Boolean] of String = ('Off', 'On');

var
    FDatabases: TOCIStringList;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIStringList

{$IFNDEF OCI_D6}
type
    __TStrings = class(TPersistent)
    private
        FUpdateCount: Integer;
    end;

function TOCIStringList.GetUpdateCount: Integer;
begin
    Result := __TStrings(Self).FUpdateCount;
end;
{$ENDIF}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIFetchParams

constructor TOCIFetchParams.Create(AOwner: TComponent);
begin
    inherited Create;
    FOwner := AOwner;
    RestoreDefaults;
end;

procedure TOCIFetchParams.Assign(ASource: TPersistent);
begin
    if ASource = Self then
        Exit;
    if ASource is TOCIFetchParams then begin
        if pvPieceBuffSize in TOCIFetchParams(ASource).FAssignedValues then
            PieceBuffSize := TOCIFetchParams(ASource).PieceBuffSize;
        if pvPieceBuffOwn in TOCIFetchParams(ASource).FAssignedValues then
            PieceBuffOwn := TOCIFetchParams(ASource).PieceBuffOwn;
        if pvInrecDataSize in TOCIFetchParams(ASource).FAssignedValues then
            InrecDataSize := TOCIFetchParams(ASource).InrecDataSize;
        if pvRowsetSize in TOCIFetchParams(ASource).FAssignedValues then
            RowsetSize := TOCIFetchParams(ASource).RowsetSize;
        if pvFetchExact in TOCIFetchParams(ASource).FAssignedValues then
            FetchExact := TOCIFetchParams(ASource).FetchExact;
    end
    else
        inherited Assign(ASource);
end;

procedure TOCIFetchParams.TuneVar(AValue: TOCIVariable);
begin
    AValue.InrecDataSize := InrecDataSize;
end;

procedure TOCIFetchParams.TuneStmt(AValue: TOCIStatement);
begin
    AValue.PieceBuffLen := PieceBuffSize;
    AValue.PieceBuffOwn := PieceBuffOwn;
//    RowsetSize
//    FetchExact
end;

function TOCIFetchParams.GetFetchAll: Boolean;
begin
    Result := FetchExact = -1;
end;

procedure TOCIFetchParams.SetFetchAll(const Value: Boolean);
begin
    if Value then
        FetchExact := -1
    else
        FetchExact := 0;
    if Value then
        if FOwner is TOCIDataSet then
            TOCIDataSet(FOwner).Unidirectional := False;
end;

function TOCIFetchParams.GetRowsetSize: sb4;
begin
    if IsValueOwn(pvRowsetSize) then
        Result := FRowsetSize
    else
        Result := ParentParams.FRowsetSize;
end;

function TOCIFetchParams.IsValueOwn(AValue: TOCIFetchParamsValue): Boolean;
begin
    Result := (FOwner = nil) or (FOwner is TOCICustomDatabase) or
       (AValue in FAssignedValues) or (TOCIDataSet(FOwner).PointedDatabase = nil);
end;

function TOCIFetchParams.GetParentParams: TOCIFetchParams;
begin
    Result := (FOwner as TOCIDataSet).PointedDatabase.DefaultFetchParams;
end;

function TOCIFetchParams.GetFetchExact: sb4;
begin
    if IsValueOwn(pvFetchExact) then
        Result := FFetchExact
    else
        Result := ParentParams.FetchExact;
end;

function TOCIFetchParams.GetInrecDataSize: sb4;
begin
    if IsValueOwn(pvInrecDataSize) then
        Result := FInrecDataSize
    else
        Result := ParentParams.InrecDataSize;
end;

function TOCIFetchParams.GetPieceBuffOwn: Boolean;
begin
    if IsValueOwn(pvPieceBuffOwn) then
        Result := FPieceBuffOwn
    else
        Result := ParentParams.PieceBuffOwn;
end;

function TOCIFetchParams.GetPieceBuffSize: sb4;
begin
    if IsValueOwn(pvPieceBuffSize) then
        Result := FPieceBuffSize
    else
        Result := ParentParams.PieceBuffSize;
end;

procedure TOCIFetchParams.RestoreDefaults;
begin
    FAssignedValues := [];
    FPieceBuffSize := IDefPieceBuffLen;
    FPieceBuffOwn := False;
    FInrecDataSize := IDefInrecDataSize;
    FRowsetSize := IDefRowSetSize;
    FFetchExact := 0;
end;

procedure TOCIFetchParams.SetFetchExact(const Value: sb4);
begin
    if Value < -1 then
        OCIDBErrorFmt(msgInvFetchPar, ['FetchExact'], nil);
    if (pvFetchExact in FAssignedValues) and (FFetchExact = Value) then
        Exit;
    FFetchExact := Value;
    Include(FAssignedValues, pvFetchExact);
end;

procedure TOCIFetchParams.SetInrecDataSize(const Value: sb4);
begin
    if Value < 0 then
        OCIDBErrorFmt(msgInvFetchPar, ['InrecDataSize'], nil);
    if (pvInrecDataSize in FAssignedValues) and (FInrecDataSize = Value) then
        Exit;
    FInrecDataSize := Value;
    Include(FAssignedValues, pvInrecDataSize);
end;

procedure TOCIFetchParams.SetPieceBuffOwn(const Value: Boolean);
begin
    if (pvPieceBuffOwn in FAssignedValues) and (FPieceBuffOwn = Value) then
        Exit;
    FPieceBuffOwn := Value;
    Include(FAssignedValues, pvPieceBuffOwn);
end;

procedure TOCIFetchParams.SetPieceBuffSize(const Value: sb4);
begin
    if Value < 1 then
        OCIDBErrorFmt(msgInvFetchPar, ['PieceBuffSize'], nil);
    if (pvPieceBuffSize in FAssignedValues) and (FPieceBuffSize = Value) then
        Exit;
    FPieceBuffSize := Value;
    Include(FAssignedValues, pvPieceBuffSize);
end;

procedure TOCIFetchParams.SetRowsetSize(const Value: sb4);
begin
    if Value < 1 then
        OCIDBErrorFmt(msgInvFetchPar, ['RowsetSize'], nil);
    if (pvRowsetSize in FAssignedValues) and (FRowsetSize = Value) then
        Exit;
    FRowsetSize := Value;
    Include(FAssignedValues, pvRowsetSize);
end;

function TOCIFetchParams.IsPBFS: Boolean;
begin
    Result := pvPieceBuffSize in FAssignedValues;
end;

function TOCIFetchParams.IsPBOS: Boolean;
begin
    Result := pvPieceBuffOwn in FAssignedValues;
end;

function TOCIFetchParams.IsRSS: Boolean;
begin
    Result := (pvRowsetSize in FAssignedValues) and not
        ((FOwner is TOCIDataSet) and TOCIDataSet(FOwner).FHaveBLOBs);
end;

function TOCIFetchParams.IsFES: Boolean;
begin
    Result := pvFetchExact in FAssignedValues;
end;

function TOCIFetchParams.IsIDSS: Boolean;
begin
    Result := pvInrecDataSize in FAssignedValues;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIDataFormat

function VarType2FieldType(const AValue: Variant): TOCIMacroDataType;
begin
    case VarType(AValue) and varTypeMask of
    varEmpty    : Result := mdUnknown;
    varNull     : Result := mdUnknown;
    varByte     : Result := mdInteger;
    varSmallint : Result := mdInteger;
    varInteger  : Result := mdInteger;
    varSingle   : Result := mdFloat;
    varDouble   : Result := mdFloat;
    varCurrency : Result := mdFloat;
    varDate     : Result := mdDateTime;
    varString   : Result := mdString;
    varOleStr   : Result := mdString;
    varBoolean  : Result := mdBoolean;
    else
    Result := mdUnknown;
    end;
end;

function Var2SQLTyped(const AValue: Variant; AFieldType: TOCIMacroDataType; const ADataFormat: TOCIDataFormat): String;
var
    Ds: char;
    V: Variant;
begin
    Result := '';
    if (VarType(AValue) and varTypeMask) in [varEmpty, varNull] then
        Result := 'NULL'
    else begin
        case AFieldType of
        mdInteger:
            Result := VarAsType(AValue, varInteger);
        mdFloat:
            begin
                V := VarAsType(AValue, varDouble);
                Ds := DecimalSeparator;
                DecimalSeparator := '.';
                try
                    Result := FloatToStr(V);
                finally
                    DecimalSeparator := Ds;
                end;
            end;
        mdDate:
            if ADataFormat <> nil then
                Result := 'to_date(''' + FormatDateTime(ADataFormat.DateFormat,
                    VarAsType(AValue, varDate)) + ''', ''' + ADataFormat.AssignDateFormat + ''')';
        mdDateTime:
            if ADataFormat <> nil then
                Result := 'to_date(''' + FormatDateTime(ADataFormat.DateTimeFormat,
                    VarAsType(AValue, varDate)) + ''', ''' + ADataFormat.AssignDateTimeFormat + ''')';
        mdUnknown, mdString:
            Result := '''' + VarAsType(AValue, varString) + '''';
        mdBoolean:
            if ADataFormat <> nil then begin
                V := ADataFormat.OBool2Value(VarAsType(AValue, varBoolean));
                Result := Var2SQL(V, ADataFormat);
            end;
        end;
    end;
end;

function Var2SQL(const AValue: Variant; const ADataFormat: TOCIDataFormat): String;
begin
    Result := Var2SQLTyped(AValue, VarType2FieldType(AValue), ADataFormat);
end;

constructor TOCIDataFormat.Create(AOwner: TComponent);
begin
    inherited Create;
    FOwner := AOwner;
    RestoreDefaults;
end;

procedure TOCIDataFormat.Assign(ASource: TPersistent);
begin
    if ASource = Self then
        Exit;
    if ASource is TOCIDataFormat then begin
        if fvOBoolType in TOCIDataFormat(ASource).FAssignedValues then
            OBoolType := TOCIDataFormat(ASource).OBoolType;
        if fvOBoolSize in TOCIDataFormat(ASource).FAssignedValues then
            OBoolSize := TOCIDataFormat(ASource).OBoolSize;
        if fvOBoolTrue in TOCIDataFormat(ASource).FAssignedValues then
            OBoolTrue := TOCIDataFormat(ASource).OBoolTrue;
        if fvOBoolFalse in TOCIDataFormat(ASource).FAssignedValues then
            OBoolFalse := TOCIDataFormat(ASource).OBoolFalse;
        if fvEInteger in TOCIDataFormat(ASource).FAssignedValues then
            EnableInteger := TOCIDataFormat(ASource).EnableInteger;
        if fvEBCD in TOCIDataFormat(ASource).FAssignedValues then
            EnableBCD := TOCIDataFormat(ASource).EnableBCD;
        if fvENumber in TOCIDataFormat(ASource).FAssignedValues then
            EnableNumber := TOCIDataFormat(ASource).EnableNumber;
        if fvELongString in TOCIDataFormat(ASource).FAssignedValues then
            EnableLongString := TOCIDataFormat(ASource).EnableLongString;
        if fvEFixedString in TOCIDataFormat(ASource).FAssignedValues then
            EnableFixedString := TOCIDataFormat(ASource).EnableFixedString;
        if fvERequired in TOCIDataFormat(ASource).FAssignedValues then
            EnableRequired := TOCIDataFormat(ASource).EnableRequired;
        if fvStrsTrim in TOCIDataFormat(ASource).FAssignedValues then
            StrsTrim := TOCIDataFormat(ASource).StrsTrim;
        if fvStrsEmpty2Null in TOCIDataFormat(ASource).FAssignedValues then
            StrsEmpty2Null := TOCIDataFormat(ASource).StrsEmpty2Null;
        if fvDateFormat in TOCIDataFormat(ASource).FAssignedValues then
            DateFormat := TOCIDataFormat(ASource).DateFormat;
        if fvDateTimeFormat in TOCIDataFormat(ASource).FAssignedValues then
            DateTimeFormat := TOCIDataFormat(ASource).DateTimeFormat;
        if fvAssignDateFormat in TOCIDataFormat(ASource).FAssignedValues then
            AssignDateFormat := TOCIDataFormat(ASource).AssignDateFormat;
        if fvAssignDateTimeFormat in TOCIDataFormat(ASource).FAssignedValues then
            AssignDateTimeFormat := TOCIDataFormat(ASource).AssignDateTimeFormat;
        if fvFixedMacroChar in TOCIDataFormat(ASource).FAssignedValues then
            FixedMacroChar := TOCIDataFormat(ASource).FixedMacroChar;
        if fvSQLMacroChar in TOCIDataFormat(ASource).FAssignedValues then
            SQLMacroChar := TOCIDataFormat(ASource).SQLMacroChar;
    end
    else
        inherited Assign(ASource);
end;

procedure TOCIDataFormat.RestoreDefaults;
begin
    FAssignedValues := [];
    FOBoolType := otSmallInt;
    FOBoolSize := SizeOf(Integer);
    FOBoolTrue := 1;
    FOBoolFalse := 0;
    FEnableInteger := False;
    FEnableBCD := False;
    FEnableNumber := False;
    FEnableLongString := False;
    FEnableFixedString := False;
    FEnableRequired := True;
    FStrsTrim := True;
    FStrsEmpty2Null := True;
    FDateFormat := 'dd.mm.yyyy';
    FDateTimeFormat := 'dd.mm.yyyy hh:nn:ss';
    FAssignDateFormat := 'dd.mm.yyyy';
    FAssignDateTimeFormat := 'dd.mm.yyyy hh24:mi:ss';
    FFixedMacroChar := '&';
    FSQLMacroChar := '%';
end;

function TOCIDataFormat.Value2OBool(AValue: Variant): Boolean;
begin
    try
        if VarIsNull(OBoolTrue) or VarIsEmpty(OBoolTrue) then
            Result := VarIsNull(AValue) or VarIsEmpty(AValue)
        else
            Result := not (VarIsNull(AValue) or VarIsEmpty(AValue)) and
                (AValue = FOBoolTrue);
    except
        Result := False;
    end;
end;

function TOCIDataFormat.OBool2Value(AValue: Boolean): Variant;
begin
    if AValue then
        Result := FOBoolTrue
    else
        Result := FOBoolFalse;
end;

function TOCIDataFormat.OBool2ValuePLSQL(const ALeft, ARight: String): String;
begin
    Result := 'if ' + ARight + ' is null then ' + ALeft + ' := null; ' +
              'elsif ' + ARight + ' then ' + ALeft + ' := ' + Var2SQL(OBoolTrue, Self) + '; ' +
              'else ' + ALeft + ' := ' + Var2SQL(OBoolFalse, Self) + '; end if;';
end;

function TOCIDataFormat.Value2OBoolPLSQL(const ALeft, ARight: String): String;
begin
    if VarIsNull(OBoolTrue) or VarIsEmpty(OBoolTrue) then
        Result := ALeft + ' := ' + ARight + ' is null;'
    else if VarIsNull(OBoolFalse) or VarIsEmpty(OBoolFalse) then
        Result := 'if ' + ARight + ' is null then ' + ALeft + ' := False; ' +
                  'else ' + ALeft + ' := (' + ARight + ' = ' + Var2SQL(OBoolTrue, Self) + '); end if;'
    else
        Result := 'if ' + ARight + ' is null then ' + ALeft + ' := null; ' +
                  'else ' + ALeft + ' := (' + ARight + ' = ' + Var2SQL(OBoolTrue, Self) + '); end if;';
end;

procedure TOCIDataFormat.TuneSelItem(AValue: TOCISelectItem);
begin
    AValue.EnableInteger := EnableInteger;
    AValue.EnableBCD := EnableBCD;
    AValue.EnableNumber := EnableNumber;
    AValue.EnableLongString := EnableLongString;
    AValue.EnableFixedString := EnableFixedString;
    AValue.EnableRequired := EnableRequired;
end;

procedure TOCIDataFormat.TuneVar(AValue: TOCIVariable);
begin
    AValue.StrsTrim := StrsTrim;
    AValue.StrsEmpty2Null := StrsEmpty2Null;
    AValue.EnableLongString := EnableLongString;
end;

function TOCIDataFormat.GetParentParams: TOCIDataFormat;
begin
    Result := (FOwner as TOCIDataSet).PointedDatabase.DefaultDataFormat;
end;

function TOCIDataFormat.IsValueOwn(AValue: TOCIDataFormatValue): Boolean;
begin
    Result := (FOwner = nil) or (FOwner is TOCICustomDatabase) or
       (AValue in FAssignedValues) or (TOCIDataSet(FOwner).PointedDatabase = nil);
end;

procedure TOCIDataFormat.InvalidateFieldDefs;
var
    i: Integer;
begin
    if (FOwner <> nil) and (FOwner is TOCIDataSet) then
        TOCIDataSet(FOwner).DataEvent(dePropertyChange, 0)
    else if (FOwner <> nil) and (FOwner is TOCICustomDatabase) then
        with TOCICustomDatabase(FOwner) do
            for i := 0 to DataSetCount - 1 do
                DataSets[i].DataEvent(dePropertyChange, 0);
end;

function TOCIDataFormat.GetEnableBCD: Boolean;
begin
    if IsValueOwn(fvEBCD) then
        Result := FEnableBCD
    else
        Result := ParentParams.EnableBCD;
end;

function TOCIDataFormat.GetEnableNumber: Boolean;
begin
    if IsValueOwn(fvENumber) then
        Result := FEnableNumber
    else
        Result := ParentParams.EnableNumber;
end;

function TOCIDataFormat.GetEnableFixedString: Boolean;
begin
    if IsValueOwn(fvEFixedString) then
        Result := FEnableFixedString
    else
        Result := ParentParams.EnableFixedString;
end;

function TOCIDataFormat.GetEnableInteger: Boolean;
begin
    if IsValueOwn(fvEInteger) then
        Result := FEnableInteger
    else
        Result := ParentParams.EnableInteger;
end;

function TOCIDataFormat.GetEnableLongString: Boolean;
begin
    if IsValueOwn(fvELongString) then
        Result := FEnableLongString
    else
        Result := ParentParams.EnableLongString;
end;

function TOCIDataFormat.GetOBoolFalse: Variant;
begin
    if IsValueOwn(fvOBoolFalse) then
        Result := FOBoolFalse
    else
        Result := ParentParams.OBoolFalse;
end;

function TOCIDataFormat.GetOBoolSize: Integer;
begin
    if IsValueOwn(fvOBoolSize) then
        Result := FOBoolSize
    else
        Result := ParentParams.OBoolSize;
end;

function TOCIDataFormat.GetOBoolTrue: Variant;
begin
    if IsValueOwn(fvOBoolTrue) then
        Result := FOBoolTrue
    else
        Result := ParentParams.OBoolTrue;
end;

function TOCIDataFormat.GetOBoolType: TOCIVarDataType;
begin
    if IsValueOwn(fvOBoolType) then
        Result := FOBoolType
    else
        Result := ParentParams.OBoolType;
end;

function TOCIDataFormat.GetEnableRequired: Boolean;
begin
    if IsValueOwn(fvERequired) then
        Result := FEnableRequired
    else
        Result := ParentParams.EnableRequired;
end;

procedure TOCIDataFormat.SetEnableBCD(const Value: Boolean);
begin
    if (fvEBCD in FAssignedValues) and (FEnableBCD = Value) then
        Exit;
    FEnableBCD := Value;
    Include(FAssignedValues, fvEBCD);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetEnableNumber(const Value: Boolean);
begin
    if (fvENumber in FAssignedValues) and (FEnableNumber = Value) then
        Exit;
    FEnableNumber := Value;
    Include(FAssignedValues, fvENumber);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetEnableFixedString(const Value: Boolean);
begin
    if (fvEFixedString in FAssignedValues) and (FEnableFixedString = Value) then
        Exit;
    FEnableFixedString := Value;
    Include(FAssignedValues, fvEFixedString);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetEnableInteger(const Value: Boolean);
begin
    if (fvEInteger in FAssignedValues) and (FEnableInteger = Value) then
        Exit;
    FEnableInteger := Value;
    Include(FAssignedValues, fvEInteger);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetEnableLongString(const Value: Boolean);
begin
    if (fvELongString in FAssignedValues) and (FEnableLongString = Value) then
        Exit;
    FEnableLongString := Value;
    Include(FAssignedValues, fvELongString);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetEnableRequired(AValue: Boolean);
begin
    if (fvERequired in FAssignedValues) and (FEnableRequired = AValue) then
        Exit;
    FEnableRequired := AValue;
    Include(FAssignedValues, fvERequired);
    InvalidateFieldDefs;
end;

procedure TOCIDataFormat.SetOBoolFalse(const Value: Variant);
begin
    try
        if (fvOBoolFalse in FAssignedValues) and (FOBoolFalse = Value) then
            Exit;
    except
    end;
    FOBoolFalse := Value;
    Include(FAssignedValues, fvOBoolFalse);
end;

procedure TOCIDataFormat.SetOBoolSize(const Value: Integer);
begin
    if (fvOBoolSize in FAssignedValues) and (FOBoolSize = Value) then
        Exit;
    FOBoolSize := Value;
    Include(FAssignedValues, fvOBoolSize);
end;

procedure TOCIDataFormat.SetOBoolTrue(const Value: Variant);
begin
    try
        if (fvOBoolTrue in FAssignedValues) and (FOBoolTrue = Value) then
            Exit;
    except
    end;
    FOBoolTrue := Value;
    Include(FAssignedValues, fvOBoolTrue);
end;

procedure TOCIDataFormat.SetOBoolType(const Value: TOCIVarDataType);
begin
    if (fvOBoolType in FAssignedValues) and (FOBoolType = Value) then
        Exit;
    FOBoolType := Value;
    Include(FAssignedValues, fvOBoolType);
end;

function TOCIDataFormat.IsEBCDS: Boolean;
begin
    Result := fvEBCD in FAssignedValues;
end;

function TOCIDataFormat.IsENS: Boolean;
begin
    Result := fvENumber in FAssignedValues;
end;

function TOCIDataFormat.IsEFSS: Boolean;
begin
    Result := fvEFixedString in FAssignedValues;
end;

function TOCIDataFormat.IsEIS: Boolean;
begin
    Result := fvEInteger in FAssignedValues;
end;

function TOCIDataFormat.IsELSS: Boolean;
begin
    Result := fvELongString in FAssignedValues;
end;

function TOCIDataFormat.IsOBSS: Boolean;
begin
    Result := fvOBoolSize in FAssignedValues;
end;

function TOCIDataFormat.IsOBTS: Boolean;
begin
    Result := fvOBoolType in FAssignedValues;
end;

function TOCIDataFormat.IsTS: Boolean;
begin
    Result := fvOBoolTrue in FAssignedValues;
end;

function TOCIDataFormat.IsFS: Boolean;
begin
    Result := fvOBoolFalse in FAssignedValues;
end;

function TOCIDataFormat.IsERS: Boolean;
begin
    Result := fvERequired in FAssignedValues;
end;

function TOCIDataFormat.GetStrsTrim: Boolean;
begin
    if IsValueOwn(fvStrsTrim) then
        Result := FStrsTrim
    else
        Result := ParentParams.StrsTrim;
end;

procedure TOCIDataFormat.SetStrsTrim(AValue: Boolean);
begin
    if (fvStrsTrim in FAssignedValues) and (FStrsTrim = AValue) then
        Exit;
    FStrsTrim := AValue;
    Include(FAssignedValues, fvStrsTrim);
end;

function TOCIDataFormat.IsSTS: Boolean;
begin
    Result := fvStrsTrim in FAssignedValues;
end;

function TOCIDataFormat.GetStrsEmpty2Null: Boolean;
begin
    if IsValueOwn(fvStrsEmpty2Null) then
        Result := FStrsEmpty2Null
    else
        Result := ParentParams.StrsEmpty2Null;
end;

procedure TOCIDataFormat.SetStrsEmpty2Null(AValue: Boolean);
begin
    if (fvStrsEmpty2Null in FAssignedValues) and (FStrsEmpty2Null = AValue) then
        Exit;
    FStrsEmpty2Null := AValue;
    Include(FAssignedValues, fvStrsEmpty2Null);
end;

function TOCIDataFormat.IsSENS: Boolean;
begin
    Result := fvStrsEmpty2Null in FAssignedValues;
end;

function TOCIDataFormat.GetAssignDateFormat: String;
begin
    if IsValueOwn(fvAssignDateFormat) then
        Result := FAssignDateFormat
    else
        Result := ParentParams.AssignDateFormat;
end;

procedure TOCIDataFormat.SetAssignDateFormat(const Value: String);
begin
    if (fvAssignDateFormat in FAssignedValues) and (FAssignDateFormat = Value) then
        Exit;
    FAssignDateFormat := Value;
    Include(FAssignedValues, fvAssignDateFormat);
end;

function TOCIDataFormat.IsADFS: Boolean;
begin
    Result := fvAssignDateFormat in FAssignedValues;
end;

function TOCIDataFormat.GetAssignDateTimeFormat: String;
begin
    if IsValueOwn(fvAssignDateTimeFormat) then
        Result := FAssignDateTimeFormat
    else
        Result := ParentParams.AssignDateTimeFormat;
end;

procedure TOCIDataFormat.SetAssignDateTimeFormat(const Value: String);
begin
    if (fvAssignDateTimeFormat in FAssignedValues) and (FAssignDateTimeFormat = Value) then
        Exit;
    FAssignDateTimeFormat := Value;
    Include(FAssignedValues, fvAssignDateTimeFormat);
end;

function TOCIDataFormat.IsADTFS: Boolean;
begin
    Result := fvAssignDateTimeFormat in FAssignedValues;
end;

function TOCIDataFormat.GetDateFormat: String;
begin
    if IsValueOwn(fvDateFormat) then
        Result := FDateFormat
    else
        Result := ParentParams.DateFormat;
end;

procedure TOCIDataFormat.SetDateFormat(const Value: String);
begin
    if (fvDateFormat in FAssignedValues) and (FDateFormat = Value) then
        Exit;
    FDateFormat := Value;
    Include(FAssignedValues, fvDateFormat);
end;

function TOCIDataFormat.IsDFS: Boolean;
begin
    Result := fvDateFormat in FAssignedValues;
end;

function TOCIDataFormat.GetDateTimeFormat: String;
begin
    if IsValueOwn(fvDateTimeFormat) then
        Result := FDateTimeFormat
    else
        Result := ParentParams.DateTimeFormat;
end;

procedure TOCIDataFormat.SetDateTimeFormat(const Value: String);
begin
    if (fvDateTimeFormat in FAssignedValues) and (FDateTimeFormat = Value) then
        Exit;
    FDateTimeFormat := Value;
    Include(FAssignedValues, fvDateTimeFormat);
end;

function TOCIDataFormat.IsDTFS: Boolean;
begin
    Result := fvDateTimeFormat in FAssignedValues;
end;

function TOCIDataFormat.GetFixedMacroChar: Char;
begin
    if IsValueOwn(fvFixedMacroChar) then
        Result := FFixedMacroChar
    else
        Result := ParentParams.FixedMacroChar;
end;

procedure TOCIDataFormat.SetFixedMacroChar(const Value: Char);
begin
    if (fvFixedMacroChar in FAssignedValues) and (FFixedMacroChar = Value) then
        Exit;
    FFixedMacroChar := Value;
    Include(FAssignedValues, fvFixedMacroChar);
end;

function TOCIDataFormat.IsFMCS: Boolean;
begin
    Result := fvFixedMacroChar in FAssignedValues;
end;

function TOCIDataFormat.GetSQLMacroChar: Char;
begin
    if IsValueOwn(fvSQLMacroChar) then
        Result := FSQLMacroChar
    else
        Result := ParentParams.SQLMacroChar;
end;

procedure TOCIDataFormat.SetSQLMacroChar(const Value: Char);
begin
    if (fvSQLMacroChar in FAssignedValues) and (FSQLMacroChar = Value) then
        Exit;
    FSQLMacroChar := Value;
    Include(FAssignedValues, fvSQLMacroChar);
end;

function TOCIDataFormat.IsSMCS: Boolean;
begin
    Result := fvSQLMacroChar in FAssignedValues;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCICustomDatabase

constructor TOCICustomDatabase.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    InitializeCriticalSection(FLock);
{$IFNDEF OCI_D5}
    FDataSets := TList.Create;
    FStreamedConnected := False;
{$ENDIF}
    LoginPrompt := True;
    FNonBlockingMode := False;
    FSilentMode := False;
    FAuthentMode := amDefault;
    FInitModes := [];
    FSQLBased := True;
    FDefaultFetchParams := TOCIFetchParams.Create(Self);
    FDefaultDataFormat := TOCIDataFormat.Create(Self);
    FLoginRetries := IDefLoginRetries;
    FLoginFields := [lfService, lfMode, lfProfile];
    FWaitCursor := crSQLWait;
    FShowWaitForm := True;
    FConnectStringUsages := [suStorePassword];
    FKeepConnection := True;
    FDefaultTransactionManager := TOCITransactionManager.Create(nil);
    with FDefaultTransactionManager do begin
        FDatabase := Self;
        FIsDefault := True;
        ConnectAction := tcNone;
        DisconnectAction := tdCommit;
        AutoCommit := True;
    end;
    if FDatabases.IndexOf(ExpandDatabaseName(SDefaultDBName, Self)) = -1 then
        DatabaseName := SDefaultDBName;
    FSeqCache := TOCIStringList.Create;
    FSeqCache.Sorted := True;
    FMaxCursors := -1;
    FServerVersion := -1;
    FUpdatesJournal := TOCIUpdatesJournal.Create;
end;

destructor TOCICustomDatabase.Destroy;
begin
    StartAccess;
    try
{$IFDEF OCI_D5}
        inherited Destroy;
{$ELSE}
        Close;
{$ENDIF}
        RemoveDatabaseName;
{$IFNDEF OCI_D5}
        FDataSets.Free;
        FDataSets := nil;
{$ENDIF}
        FDefaultFetchParams.Free;
        FDefaultFetchParams := nil;
        FDefaultDataFormat.Free;
        FDefaultDataFormat := nil;
        FDefaultTransactionManager.Free;
        FDefaultTransactionManager := nil;
        FSeqCache.Free;
        FSeqCache := nil;
        FUpdatesJournal.Free;
        FUpdatesJournal := nil;
        TNCOciBreakFrm.Cancel(nil);
{$IFNDEF OCI_D5}
        inherited Destroy;
{$ENDIF}
    finally
        EndAccess;
    end;
    DeleteCriticalSection(FLock);
end;

function TOCICustomDatabase.IsDBNS: Boolean;
begin
    Result := FDatabaseName <> SDefaultDBName;
end;

procedure TOCICustomDatabase.SetDefaultFetchParams(AValue: TOCIFetchParams);
begin
    FDefaultFetchParams.Assign(AValue);
end;

procedure TOCICustomDatabase.Loaded;
begin
{$IFNDEF OCI_D5}
    inherited Loaded;
{$ENDIF}
    try
        if not (csDesigning in ComponentState) and (suDesignOnly in FConnectStringUsages) then begin
            FServerName := '';
            FUserName := '';
            FPassword := '';
        end;
        FInLoaded := True;
        try
{$IFNDEF OCI_D5}
            if FStreamedConnected then
                Open;
{$ELSE}
            inherited Loaded;
{$ENDIF}
        finally
            FInLoaded := False;
        end;
    except
        if csDesigning in ComponentState then
            OCIDBHandleException(Self)
        else
            raise;
    end;
end;

procedure TOCICustomDatabase.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = SQLMonitor then
            SQLMonitor := nil;
        if AComponent = TransactionManager then
            TransactionManager := nil;
    end;
end;

function TOCICustomDatabase.GetOCIDataSet(Index: Integer): TOCIDataSet;
begin
{$IFDEF OCI_D5}
    Result := TOCIDataSet(inherited DataSets[Index]);
{$ELSE}
    Result := TOCIDataSet(FDataSets[Index]);
{$ENDIF}
end;

{$IFNDEF OCI_D5}
function TOCICustomDatabase.GetDataSetCount: Integer;
begin
    Result := FDataSets.Count;
end;
{$ENDIF}

procedure TOCICustomDatabase.StartAccess;
begin
    EnterCriticalSection(FLock);
end;

procedure TOCICustomDatabase.EndAccess;
begin
    LeaveCriticalSection(FLock);
end;

procedure TOCICustomDatabase.CloseDataSets;
begin
    StartAccess;
    try
        while DataSetCount <> 0 do
            DataSets[DataSetCount - 1].Disconnect;
    finally
        EndAccess;
    end;
end;

{$HINTS OFF}
type
    __TCustomConnection = class(TComponent)
    private
        FClients: TList;
        FDataSets: TList;
    end;
{$HINTS ON}

function SortOnUsage(Item1, Item2: Pointer): Integer;
begin
    if TOCIDataSet(Item1).Disconnectable and not TOCIDataSet(Item2).Disconnectable then
        Result := -1
    else if not TOCIDataSet(Item1).Disconnectable and TOCIDataSet(Item2).Disconnectable then
        Result := 1
    else if TOCIDataSet(Item1).UsageCount > TOCIDataSet(Item2).UsageCount then
        Result := -1
    else if TOCIDataSet(Item1).UsageCount < TOCIDataSet(Item2).UsageCount then
        Result := 1
    else
        Result := 0;
end;

procedure TOCICustomDatabase.ReleaseCursors(AExclude: TOCIDataSet);
var
    dsList: TList;
    i, n: Integer;
begin
    DbgOut(tfStmt, 'ReleaseCursors start, CRS = ' + IntToStr(DataSetCount));
{$IFDEF OCI_D5}
    dsList := __TCustomConnection(Self).FDataSets;
{$ELSE}
    dsList := FDataSets;
{$ENDIF}
    dsList.Sort(SortOnUsage);
    n := MulDiv(MaxCursors, IDSPctUsable, 100);
    i := dsList.Count - 1;
    while i > n do begin
        if (DataSets[i].Disconnectable) and (DataSets[i] <> AExclude) then
            DataSets[i].Disconnect;
        Dec(i);
    end;
    DbgOut(tfStmt, 'ReleaseCursors end, CRS = ' + IntToStr(DataSetCount));
end;

procedure TOCICustomDatabase.AttachDataSet(ADS: TOCIDataSet);
begin
    StartAccess;
    try
        if (MaxCursors <> -1) and ((DataSetCount + 1) > MaxCursors) then
            ReleaseCursors(ADS);
{$IFDEF OCI_D5}
        RegisterClient(ADS);
{$ELSE}
        FDataSets.Add(ADS);
{$ENDIF}
        if FDBUseCount = 0 then
            try
                Open;
            except
{$IFDEF OCI_D5}
                UnRegisterClient(ADS);
{$ELSE}
                FDataSets.Remove(ADS);
{$ENDIF}
                raise;
            end
        else
            Inc(FDBUseCount);
    finally
        EndAccess;
    end;
end;

procedure TOCICustomDatabase.DetachDataSet(ADS: TOCIDataSet);
begin
    StartAccess;
    try
{$IFDEF OCI_D5}
        UnRegisterClient(ADS);
{$ELSE}
        FDataSets.Remove(ADS);
{$ENDIF}
        if FDBUseCount = 1 then begin
            if not FKeepConnection then
                Close;
        end
        else
            Dec(FDBUseCount);
    finally
        EndAccess;
    end;
end;

function TOCICustomDatabase.GetSeq(const AName: String): TOCISequence;
var
    i: Integer;
    seq: TOCISequence;
begin
    if not FSeqCache.Find(AName, i) then begin
        seq := TOCISequence.Create(nil);
        seq.DatabaseName := DatabaseName;
        seq.SequenceName := AName;
        i := FSeqCache.AddObject(AName, seq);
    end;
    Result := TOCISequence(FSeqCache.Objects[i]);
end;

procedure TOCICustomDatabase.ClearSeqCache;
var
    i: Integer;
begin
    for i := 0 to FSeqCache.Count - 1 do
        TOCISequence(FSeqCache.Objects[i]).Free;
    FSeqCache.Clear;
end;

procedure TOCICustomDatabase.FreeHandles;
begin
    if FTransactionManager <> nil then
        FTransactionManager.FreeHandle;
    if FDefaultTransactionManager <> nil then
        FDefaultTransactionManager.FreeHandle;
    FSession.Free;
    FSession := nil;
    FServer.Free;
    FServer := nil;
    FService.Free;
    FService := nil;
    FEnv.Free;
    FEnv := nil;
    FServerVersion := -1;
end;

function TOCICustomDatabase.GetConnected: Boolean;
begin
    Result := FDBUseCount > 0;
end;

procedure TOCICustomDatabase.SetConnected(Value: Boolean);
begin
{$IFNDEF OCI_D5}
    if csReading in ComponentState then
        FStreamedConnected := Value
    else
        if Value then
            Open
        else
            Close;
{$ELSE}
    StartWait;
    StartAccess;
    try
        inherited SetConnected(Value);
    finally
        EndAccess;
        EndWait;
    end;
{$ENDIF}
end;

function TOCICustomDatabase.InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
    hErr, hTX: pOCIHandle): Boolean;
begin
    Result := False;
end;

procedure TOCICustomDatabase.ReleaseHandles;
begin
end;

procedure TOCICustomDatabase.{$IFNDEF OCI_D5}Close{$ELSE}DoDisconnect{$ENDIF};
begin
{$IFNDEF OCI_D5}
    StartWait;
    StartAccess;
    try
        if Connected then begin
            if not (csDestroying in ComponentState) and Assigned(FBeforeDisconnect) then
                FBeforeDisconnect(Self);
{$ENDIF}
            DbgOut(tfConnect, DatabaseName + ' disconnect');
            CloseDataSets;
            ClearSeqCache;
            ReleaseHandles;
            ActualTransactionManager.DisConnect;
            FreeHandles;
            FUpdatesJournal.Reset;
            if FDBUseCount <> 0 then
                Dec(FDBUseCount);
{$IFNDEF OCI_D5}
            if not (csDestroying in ComponentState) and Assigned(FAfterDisconnect) then
                FAfterDisconnect(Self);
        end;
    finally
        EndAccess;
        EndWait;
    end;
{$ENDIF}
end;

procedure TOCICustomDatabase.{$IFNDEF OCI_D5}Open{$ELSE}DoConnect{$ENDIF};
var
    done, logPr: Boolean;
    retry: Integer;
    hEnv, hSrvc, hSrv, hSes, hErr, hTX: pOCIHandle;
begin
{$IFNDEF OCI_D5}
    StartWait;
    StartAccess;
    try
        if not Connected then
        try
            if Assigned(FBeforeConnect) then
                FBeforeConnect(Self);
{$ELSE}
        try
{$ENDIF}
            if not Silent then
                TNCOciErrorFrm.HookExceptions;
            if SQLMonitor <> nil then begin
                SQLMonitor.ClientObjName := DatabaseName;
                SQLMonitor.Active := True;
            end;
            if InterceptHandles(hEnv, hSrvc, hSrv, hSes, hErr, hTX) then begin
                DbgOut(tfConnect, DatabaseName + ' handles intercepted');
                FEnv := TOCIEnvironment.CreateUsingHandle(hEnv, hErr, SQLMonitor, DoError, DoWarning);
                FService := TOCIService.CreateUsingHandle(FEnv, hSrvc);
                FService.NONBLOCKING_MODE := False;
                FService.OnYield := DoYield;
                FServer := TOCIServer.CreateUsingHandle(FService, hSrv);
                FSession := TOCISession.CreateUsingHandle(FService, hSes);
                DbgOut(tfConnect, DatabaseName + ' connected using external handles');
                ActualTransactionManager.ConnectUsingHandle(hTX);
            end
            else begin
                FEnv := TOCIEnvironment.Create(InitModes, SQLMonitor, DoError, DoWarning);
                FService := TOCIService.Create(FEnv);
                FService.OnYield := DoYield;
                FService.NONBLOCKING_MODE := False;
                FServer := TOCIServer.Create(FService);
                FSession := TOCISession.Create(FService);
                done := True;
                logPr := not Silent and LoginPrompt and not FInInteractiveOpen;
                retry := 0;
                repeat
                    try
                        if logPr then
                            if Assigned(OnLogin) then
                                OnLogin(Self, UserName, Password)
                            else if not TNCOciLoginFrm.Execute(Self, True) then
                                if FInLoaded then
                                    Exit
                                else
                                    Abort;
                        DbgOut(tfConnect, DatabaseName + ' connect');
                        FServer.Detach;
                        if ServerName = '<LOCAL>' then
                            FServer.Attach('')
                        else
                            FServer.Attach(ServerName);
                        if dmDistributed in InitModes then
                            FServer.INTERNAL_NAME := DatabaseName;
                        FSession.Start(UserName, Password, AuthentMode);
                        done := True;
                    except on E: EOCINativeError do
                         // invalid password/user name
                        if (E.Errors[0].ErrorCode = 1017) and not Silent and
                           (LoginPrompt or FInInteractiveOpen) then begin
                            OCIDBHandleException(Self);
                            Inc(retry);
                            if retry = LoginRetries then
                                OCIDBErrorFmt(msgOCIDBLoginRetries, [LoginRetries], Self);
                            done := False;
                            logPr := True;
                        end
                        // password expired
                        else if (E.Errors[0].ErrorCode = 28001) and not Silent and
                                (LoginPrompt or FInInteractiveOpen) then begin
                            OCIDBHandleException(Self);
                            FSession.Select;
                            if Assigned(FOnChangePassword) then
                                FOnChangePassword(Self)
                            else if not TNCOciLoginFrm.Execute(Self, False) then
                                Abort;
                            FSession.Start(UserName, Password, AuthentMode);
                            done := True;
                            logPr := False;
                        end
                        // will expired
                        else if (E.Errors[0].ErrorCode = 28002) then begin
                            if not Silent then
                                OCIDBHandleException(Self);
                        end
                        else
                            raise;
                    end;
                until done;
                ActualTransactionManager.Connect;
            end;
            FService.NONBLOCKING_MODE := NonBlockingMode;
            FUpdatesJournal.Reset;
            Inc(FDBUseCount);
        except
            FreeHandles;
            raise;
        end;
{$IFNDEF OCI_D5}
        if Assigned(FAfterConnect) then
            FAfterConnect(Self);
    finally
        EndAccess;
        EndWait;
    end;
{$ENDIF}
end;

function TOCICustomDatabase.InteractiveOpen: Boolean;
begin
    FInInteractiveOpen := True;
    try
        Result := TNCOciLoginFrm.Execute(Self, True);
        if Result then
            Open;
    finally
        FInInteractiveOpen := False;
    end;
end;

procedure TOCICustomDatabase.SetKeepConnection(Value: Boolean);
begin
    if FKeepConnection <> Value then begin
        FKeepConnection := Value;
        if not Value and (FDBUseCount = 0) then
            Close;
    end;
end;

procedure TOCICustomDatabase.CheckActive;
begin
    if not Connected then
        OCIDBError(msgOCIDBmbActive, Self);
end;

procedure TOCICustomDatabase.CheckInactive;
begin
    if Connected then
        if (csDesigning in ComponentState) or FInInteractiveOpen then
            Close
        else
            OCIDBError(msgOCIDBmbInactive, Self);
end;

procedure TOCICustomDatabase.Break;
begin
    if Connected then
    try
        FService.Break(False);
    finally
// Although help says, that after OCIBreak i must to call
// OCIReset, it give me a ORA-3113 on second execution of
// the same query. So, i comment this out ...
//        if NonBlockingMode then
//            FService.Reset;
    end;
end;

procedure TOCICustomDatabase.ChangePassword(const ANewPassword: String);
begin
    if FSession = nil then
        CheckActive;
    FSession.ChangePassword(UserName, Password, ANewPassword);
    FPassword := ANewPassword;
end;

function TOCICustomDatabase.GetConnectString: String;
begin
    if (Password = '') and (UserName = '') then
        Result := '/'
    else begin
        Result := UserName;
        if Password <> '' then
            Result := Result + '/' + Password;
    end;
    if ServerName <> '' then
        Result := Result + '@' + ServerName;
    if AuthentMode <> amDefault then begin
        Result := Result + ' AS ';
        if AuthentMode = amSysDba then
            Result := Result + 'SYSDBA'
        else if AuthentMode = amSysOper then
            Result := Result + 'SYSOPER';
    end;
end;

procedure TOCICustomDatabase.SetConnectString(const Value: String);
var
    i1, i2, i3: Integer;
    s, Rest: String;
begin
    if ConnectString <> Value then begin
        UserName := '';
        Password := '';
        ServerName := '';
        AuthentMode := amDefault;
        Rest := Value;
        i1 := Pos('/', Rest);
        i2 := Pos('@', Rest);
        i3 := Pos(' AS ', UpperCase(Rest));
        if i3 > 0 then
            Inc(i3);
        if (i3 <> 0) and
           ((i3 = 1) or (Rest[i3 - 1] = ' ')) and
           ((i3 + 2 > Length(Rest)) or (Rest[i3 + 2] = ' ')) then begin
            s := UpperCase(Trim(Copy(Rest, i3 + 2, Length(Rest))));
            if s = 'SYSDBA' then
                AuthentMode := amSysDba
            else if s = 'SYSOPER' then
                AuthentMode := amSysOper;
            Rest := Copy(Rest, 1, i3 - 1);
        end;
        if i2 <> 0 then begin
            ServerName := Trim(Copy(Rest, i2 + 1, Length(Rest)));
            Rest := Copy(Rest, 1, i2 - 1);
        end;
        if i1 <> 0 then begin
            Password := Trim(Copy(Rest, i1 + 1, Length(Rest)));
            Rest := Copy(Rest, 1, i1 - 1);
        end;
        UserName := Trim(Copy(Value, 1, Length(Rest)));
    end;
end;

procedure TOCICustomDatabase.SetAuthentMode(const Value: TOCIAuthentMode);
begin
    if FAuthentMode <> Value then begin
        CheckInactive;
        FAuthentMode := Value;
    end;
end;

class function TOCICustomDatabase.ExpandDatabaseName(AValue: String; AForComponent: TComponent): String;
var
    i: Integer;
begin
    Result := AValue;
    if (AForComponent = nil) or not (csDesigning in AForComponent.ComponentState) then begin
        if (FOCIDatabaseNameExpandMode <> deNone) and (Pos('%', Result) = 0) then
            if FOCIDatabaseNameExpandMode = deUseThread then
                Result := Result + '_%T'
            else if FOCIDatabaseNameExpandMode = deUseOwner then
                Result := Result + '_%O';
        while True do begin
            i := Pos('%', Result);
            if (i = 0) or ((i + 1) > Length(Result)) then
                System.Break;
            if Result[i + 1] in ['t', 'T'] then begin
                Delete(Result, i, 2);
                Insert(IntToStr(GetCurrentThreadId), Result, i);
            end
            else if Result[i + 1] in ['o', 'O'] then begin
                Delete(Result, i, 2);
                if AForComponent <> nil then begin
                    Insert(IntToStr(Integer(Pointer(AForComponent.Owner))), Result, i);
                end;
            end;
        end;
    end;
end;

procedure TOCICustomDatabase.SetDatabaseName(const Value: String);
var
    S: String;
begin
    StartAccess;
    try
        if FDatabaseName <> Value then begin
            CheckInactive;
            S := ExpandDatabaseName(Value, Self);
            FDatabases.AddObject(S, Self);
            RemoveDatabaseName;
            FDatabaseName := Value;
            FExpandedDBName := S;
            if FDefaultTransactionManager <> nil then
                FDefaultTransactionManager.DatabaseName := Value;
        end;
    finally
        EndAccess;
    end;
end;

procedure TOCICustomDatabase.RemoveDatabaseName;
var
    i: Integer;
begin
    if (FDatabases <> nil) and FDatabases.Find(FExpandedDBName, i) then
        FDatabases.Delete(i);
end;

procedure TOCICustomDatabase.SetInitModes(const Value: TOCIDatabaseModes);
begin
    if FInitModes <> Value then begin
        CheckInactive;
        FInitModes := Value;
    end;
end;

procedure TOCICustomDatabase.SetNonBlockingMode(const Value: Boolean);
begin
    if FNonBlockingMode <> Value then begin
        FNonBlockingMode := Value;
        if Connected then
            FService.NONBLOCKING_MODE := Value;
    end;
end;

procedure TOCICustomDatabase.SetPassword(const Value: String);
begin
    if FPassword <> Value then begin
        CheckInactive;
        FPassword := Value;
    end;
end;

procedure TOCICustomDatabase.SetServerName(const Value: String);
begin
    if FServerName <> Value then begin
        CheckInactive;
        FServerName := Value;
    end;
end;

procedure TOCICustomDatabase.SetUserName(const Value: String);
begin
    if FUserName <> Value then begin
        CheckInactive;
        FUserName := Value;
    end;
end;

class function TOCICustomDatabase.FindDatabase(const AName: String; AForComponent: TComponent): TOCICustomDatabase;
var
    i: Integer;
    s: String;
begin
    Result := nil;
    if AName = '' then
        s := SDefaultDBName
    else
        s := AName;
    if FDatabases.Find(ExpandDatabaseName(S, AForComponent), i) then
        Result := TOCICustomDatabase(FDatabases.Objects[i]);
end;

class function TOCICustomDatabase.DatabaseByName(const AName: String; AForComponent: TComponent): TOCICustomDatabase;
var
    s: String;
begin
    Result := FindDatabase(AName, AForComponent);
    if Result = nil then begin
        if AName = '' then
            s := SDefaultDBName
        else
            s := AName;
        OCIDBErrorFmt(msgOCIDBUnknown, [s], nil);
    end;
end;

procedure TOCICustomDatabase.ExecSQL(const ASQL: String);
begin
    CheckActive;
    with TOCIStatement.Create(FEnv) do
    try
        Prepare(ASQL);
        Execute(FService, 0, 0, False, AutoCommit, False);
    finally
        Free;
    end;
end;

function TOCICustomDatabase.GetServerVersion: String;
begin
    CheckActive;
    Result := FServer.ServerVersion;
end;

function TOCICustomDatabase.GetServerVersionNo: Integer;
var
    i, iFrom: Integer;
    s: String;
begin
    CheckActive;
    if FServerVersion = -1 then begin
        s := AnsiUpperCase(ServerVersion);
        i := Pos('RELEASE', s);
        FServerVersion := cvOracle80000;
        if i <> 0 then begin
            Inc(i, 7);
            while (s[i] = ' ') do
                Inc(i);
            iFrom := i;
            while s[i] in ['0'..'9', '.'] do
                Inc(i);
            FServerVersion := VerStr2Int(Copy(s, iFrom, i - iFrom));
        end;
    end;
    Result := FServerVersion;
end;

function TOCICustomDatabase.GetClientVersionNo: Integer;
begin
    CheckActive;
    Result := FOCIVersion;
end;

class procedure TOCICustomDatabase.GetServicesList(AList: TStrings);
var
    InComment, InStr: Boolean;
    pCh, pStParam: PChar;
    s, buff: String;
    BraceLevel: Integer;
    f: TFileStream;
begin
    InitOCI;
    AList.Clear;
    if FOCIPO8 then
        AList.Add('<LOCAL>');
    try
        f := TFileStream.Create(FOCITnsNames, fmOpenRead or fmShareDenyWrite);
        try
            SetLength(buff, f.Size);
            f.Read(PChar(Buff)^, f.Size);
        finally
            f.Free;
        end;
    except
        Exit;
    end;
    InComment := False;
    InStr := False;
    BraceLevel := 0;
    pCh := PChar(Buff) - 1;
    repeat
        Inc(pCh);
        case pCh^ of
        '#':
            begin
                if not InComment and not InStr then
                    InComment := True;
            end;
        '''':
            if not InComment then
                InStr := not InStr;
        '(':
            if not InComment and not InStr then
                Inc(BraceLevel);
        ')':
            if not InComment and not InStr then begin
                Dec(BraceLevel);
                if BraceLevel < 0 then
                    OCIDBErrorFmt(msgOCIDBTNManyClBraces, [s], nil);
            end;
        #13, #10:
            if InComment then
                InComment := False;
        'a'..'z', 'A'..'Z', '0'..'9':
            if not InComment and not InStr and (BraceLevel = 0) then begin
                pStParam := pCh;
                while pCh^ in ['a'..'z', 'A'..'Z', '0'..'9', '#', '$', '_', '.', '-'] do
                    Inc(pCh);
                SetString(s, pStParam, pCh - pStParam);
                AList.Add(s);
                Dec(pCh);
            end;
        end;
    until (pCh^ = #0);
end;

class procedure TOCICustomDatabase.GetObjectsList(const ADatabase: String; AList: TStrings;
    const AParentObject: String; AObjKind: TOCIObjKind; AIncludeSystem: Boolean);
var
    i: Integer;
    Q: TOCIQuery;
begin
    AList.Clear;
    case AObjKind of
    okService:
        GetServicesList(AList);
    okDatabase:
        begin
            AList.BeginUpdate;
            try
                AList.Clear;
                for i := 0 to FDatabases.Count - 1 do
                    AList.Add(TOCICustomDatabase(FDatabases.Objects[i]).DatabaseName);
            finally
                AList.EndUpdate;
            end;
        end;
    okSelectable:
        TOCIDM.GetSelectables(ADatabase, AList, AIncludeSystem);
    okPackage:
        TOCIDM.GetPackages(ADatabase, AList, AIncludeSystem);
    okProc:
        if AParentObject <> '' then
            TOCIDM.GetPackProcs(ADatabase, AParentObject, AList)
        else
            TOCIDM.GetProcs(ADatabase, AList, AIncludeSystem);
    okSeqs:
        TOCIDM.GetSeqs(ADatabase, AList, AIncludeSystem);
    okFields:
        begin
            Q := TOCIQuery.Create(nil);
            try
                Q.DatabaseName := ADatabase;
                Q.FetchParams.RowsetSize := 1;
                Q.SQL.Text := 'select * from ' + AParentObject + ' where 1 = 0';
                Q.Open;
                AList.Clear;
                for i := 0 to Q.FieldCount - 1 do
                    AList.Add(Q.Fields[i].FieldName);
                if AIncludeSystem then begin
                    AList.Add('ROWID');
                    AList.Add('ROWNUM');
                end;
            finally
                Q.Free;
            end;
        end;
    end;
end;

procedure TOCICustomDatabase.SetDefaultDataFormat(AValue: TOCIDataFormat);
begin
    FDefaultDataFormat.Assign(AValue);
end;

function TOCICustomDatabase.GetSilent: Boolean;
begin
    Result := SilentMode or (GetCurrentThreadID <> MainThreadID);
end;

procedure TOCICustomDatabase.AbortWait;
begin
    if FWaitCount > 0 then begin
        FWaitCount := 1;
        EndWait;
    end;
end;

procedure TOCICustomDatabase.EndWait;
begin
    if not Silent and (FWaitCount > 0) then begin
        Dec(FWaitCount);
        if FWaitCount = 0 then begin
            Screen.Cursor := crDefault;
            if ShowWaitForm then
                TNCOciBreakFrm.Cancel(Self);
        end;
    end;
end;

procedure TOCICustomDatabase.StartWait;
begin
    if not Silent and (WaitCursor <> crDefault) then begin
        Inc(FWaitCount);
        if FWaitCount = 1 then begin
            Screen.Cursor := WaitCursor;
            if ShowWaitForm then
                TNCOciBreakFrm.Execute(Self);
        end;
    end;
end;

procedure TOCICustomDatabase.DoYield(ASender: TObject);
begin
    Sleep(1);
    if not Silent and ShowWaitForm then
        TNCOciBreakFrm.ProcessMessages;
    if Assigned(OnYield) then
        OnYield(Self);
end;

function TOCICustomDatabase.GetActualTransactionManager: TOCITransactionManager;
begin
    if FTransactionManager = nil then
        Result := FDefaultTransactionManager
    else
        Result := FTransactionManager;
end;

procedure TOCICustomDatabase.SetTransactionManager(AValue: TOCITransactionManager);
begin
    if AValue = FDefaultTransactionManager then
        AValue := nil;
    if not ((AValue = nil) and (FTransactionManager = nil) or
            (AValue <> nil) and (ActualTransactionManager = AValue)) then begin
        if AValue <> nil then
            AValue.CheckDBNameMatch(DatabaseName);
        if TransactionManagersSerialize and ActualTransactionManager.InTransaction then
            OCIDBError(msgOCICannotChangeTM, Self);
        if Assigned(FBeforeTMConnect) then
            FBeforeTMConnect(Self);
        if Connected then
            ActualTransactionManager.Disconnect;
        FTransactionManager := nil;
        if ActualTransactionManager <> AValue then begin
            FTransactionManager := AValue;
            if FTransactionManager <> nil then
                FTransactionManager.FreeNotification(Self);
        end;
        if Connected then
            ActualTransactionManager.Connect;
        if Assigned(FAfterTMConnect) then
            FAfterTMConnect(Self);
    end;
end;

function TOCICustomDatabase.GetAutoCommit: Boolean;
begin
    Result := ActualTransactionManager.AutoCommit;
end;

procedure TOCICustomDatabase.SetAutoCommit(AValue: Boolean);
begin
    ActualTransactionManager.AutoCommit := AValue;
end;

function TOCICustomDatabase.GetInTransaction: Boolean;
begin
    Result := ActualTransactionManager.InTransaction;
end;

procedure TOCICustomDatabase.StartTransaction;
begin
    ActualTransactionManager.StartTransaction;
end;

procedure TOCICustomDatabase.Commit;
begin
    ActualTransactionManager.Commit;
end;

procedure TOCICustomDatabase.CommitAll;
begin
    ActualTransactionManager.CommitAll;
end;

procedure TOCICustomDatabase.Rollback;
begin
    ActualTransactionManager.Rollback;
end;

procedure TOCICustomDatabase.RollbackAll;
begin
    ActualTransactionManager.RollbackAll;
end;

function TOCICustomDatabase.GetHTransaction: TOCITransaction;
begin
    Result := ActualTransactionManager.HTransaction;
end;

function TOCICustomDatabase.GetTransIsolation: TOCITransactionMode;
begin
    Result := ActualTransactionManager.TransIsolation;
end;

procedure TOCICustomDatabase.SetTransIsolation(AValue: TOCITransactionMode);
begin
    ActualTransactionManager.TransIsolation := AValue;
end;

function TOCICustomDatabase.GetBeforeStartTransaction: TNotifyEvent;
begin
    Result := ActualTransactionManager.BeforeStartTransaction;
end;

procedure TOCICustomDatabase.SetBeforeStartTransaction(const Value: TNotifyEvent);
begin
    ActualTransactionManager.BeforeStartTransaction := Value;
end;

function TOCICustomDatabase.GetAfterStartTransaction: TNotifyEvent;
begin
    Result := ActualTransactionManager.AfterStartTransaction;
end;

procedure TOCICustomDatabase.SetAfterStartTransaction(const Value: TNotifyEvent);
begin
    ActualTransactionManager.AfterStartTransaction := Value;
end;

function TOCICustomDatabase.GetAfterCommit: TNotifyEvent;
begin
    Result := ActualTransactionManager.AfterCommit;
end;

procedure TOCICustomDatabase.SetAfterCommit(const Value: TNotifyEvent);
begin
    ActualTransactionManager.AfterCommit := Value;
end;

function TOCICustomDatabase.GetBeforeCommit: TNotifyEvent;
begin
    Result := ActualTransactionManager.BeforeCommit;
end;

procedure TOCICustomDatabase.SetBeforeCommit(const Value: TNotifyEvent);
begin
    ActualTransactionManager.BeforeCommit := Value;
end;

function TOCICustomDatabase.GetAfterRollback: TNotifyEvent;
begin
    Result := ActualTransactionManager.AfterRollback;
end;

procedure TOCICustomDatabase.SetAfterRollback(const Value: TNotifyEvent);
begin
    ActualTransactionManager.AfterRollback := Value;
end;

function TOCICustomDatabase.GetBeforeRollback: TNotifyEvent;
begin
    Result := ActualTransactionManager.BeforeRollback;
end;

procedure TOCICustomDatabase.SetBeforeRollback(const Value: TNotifyEvent);
begin
    ActualTransactionManager.BeforeRollback := Value;
end;

function TOCICustomDatabase.IsTXS: Boolean;
begin
    Result := ActualTransactionManager.IsDefault;
end;

procedure TOCICustomDatabase.SetSQLMonitor(const Value: TNCSQLMonitorClient);
begin
    if FSQLMonitor <> Value then begin
        FSQLMonitor := Value;
        if FSQLMonitor <> nil then
            FSQLMonitor.FreeNotification(Self);
    end;
end;

function TOCICustomDatabase.DbgActive: Boolean;
begin
    Result := (SQLMonitor <> nil) and SQLMonitor.Active;
end;

procedure TOCICustomDatabase.DbgOut(AType: TNCTraceFlag; const AStmt: String);
begin
    if (SQLMonitor <> nil) and SQLMonitor.Active then
        SQLMonitor.AddStatement(AType, AStmt);
end;

function TOCICustomDatabase.GetHError: TOCIError;
begin
    Result := HEnvironment.Error;
end;

function TOCICustomDatabase.GetWarning: EOCINativeError;
begin
    Result := HError.Warning;
end;

procedure TOCICustomDatabase.ClearWarning;
begin
    HError.ClearWarning;
end;

procedure TOCICustomDatabase.DoError(ASender: TObject; var E: Exception);
begin
    AbortWait;
    if (ASender <> nil) and (ASender is TOCIDataSet) then
        TOCIDataSet(ASender).DoError(ASender, E);
    if Assigned(FOnError) then
        FOnError(ASender, E);
end;

procedure TOCICustomDatabase.DoWarning(ASender: TObject; var E: Exception);
begin
    AbortWait;
    if (ASender <> nil) and (ASender is TOCIDataSet) then
        TOCIDataSet(ASender).DoWarning(ASender, E);
    if Assigned(FOnWarning) then
        FOnWarning(ASender, E);
end;

procedure TOCICustomDatabase.SetCachedUpdates(AValue: Boolean);
var
    i: Integer;
    curDS: TOCIDataSet;
begin
    if AValue <> FCachedUpdates then begin
        FCachedUpdates := AValue;
        try
            i := 0;
            while i < DataSetCount do begin
                curDS := DataSets[i];
                with curDS do
                    if Active then
                        UpdateCachedUpdatesProp(CachedUpdates, CachedUpdatesArea);
                if (i < DataSetCount) and (curDS = DataSets[i]) then
                    Inc(i);
            end;
        except
            if AValue then
                CachedUpdates := False;
            raise;
        end;
    end;
end;

function DSOA2Lst(ADataSets: array of TOCIDataSet): TList;
var
    i: Integer;
begin
    if ((High(ADataSets) - Low(ADataSets) + 1) = 1) and (ADataSets[0] = nil) then
        Result := nil
    else begin
        Result := TList.Create;
        for i := Low(ADataSets) to High(ADataSets) do
            Result.Add(ADataSets[i].FOCICursor);
    end;
end;

function TOCICustomDatabase.ProcessJournal(ADataSets: array of TOCIDataSet;
    AVersion: Integer; AOperation: TOCIUpdatesJournalOperation): Integer;
var
    i: Integer;
    DS: TOCIDataSet;
    lst: TList;
begin
    StartWait;
    Result := 0;
    try
        if AOperation = qoApply then
            StartTransaction;
        try
            if CachedUpdates then begin
                lst := DSOA2Lst(ADataSets);
                try
                    if AOperation in [qoApply, qoCancel, qoCommit] then
                        for i := Low(ADataSets) to High(ADataSets) do
                            with ADataSets[i] do begin
                                if AOperation = qoApply then
                                    CheckBrowseMode
                                else
                                    Cancel;
                                CheckCachedUpdates;
                                UpdateCursorPos;
                            end;
                    try
                        Result := FUpdatesJournal.ProcessJournal(lst, AVersion, AOperation);
                    finally
                        if AOperation in [qoApply, qoCancel, qoCommit] then
                            for i := Low(ADataSets) to High(ADataSets) do
                                ADataSets[i].Resync([]);
                    end;
                finally
                    if lst <> nil then
                        lst.Free;
                end;
            end
            else begin
                // for backward compatibility
                for i := Low(ADataSets) to High(ADataSets) do begin
                    DS := ADataSets[i];
                    case AOperation of
                    qoApply:  DS.ApplyUpdates;
                    qoCancel: DS.CancelUpdates;
                    qoCommit: DS.CommitUpdates;
                    end;
                end;
                if AOperation = qoApply then
                    for i := 0 to High(ADataSets) do
                        ADataSets[i].CommitUpdates;
            end;
            if AOperation = qoApply then
                Commit;
        except
            if AOperation = qoApply then
                Rollback;
            raise;
        end;
    finally
        EndWait;
    end;
end;

function TOCICustomDatabase.ApplyUpdates(ADataSets: array of TOCIDataSet;
    AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
begin
    Result := ProcessJournal(ADataSets, AVersion, qoApply);
end;

function TOCICustomDatabase.ApplyAllUpdates: Integer;
begin
    Result := ApplyUpdates([nil], -1);
end;

function TOCICustomDatabase.CommitUpdates(ADataSets: array of TOCIDataSet;
    AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
begin
    Result := ProcessJournal(ADataSets, AVersion, qoCommit);
end;

function TOCICustomDatabase.CommitAllUpdates: Integer;
begin
    Result := CommitUpdates([nil], -1);
end;

function TOCICustomDatabase.CancelUpdates(ADataSets: array of TOCIDataSet;
    AVersion: Integer {$IFDEF OCI_D4} = -1 {$ENDIF}): Integer;
begin
    Result := ProcessJournal(ADataSets, AVersion, qoCancel);
end;

function TOCICustomDatabase.CancelAllUpdates: Integer;
begin
    Result := CancelUpdates([nil], -1);
end;

function TOCICustomDatabase.GetUpdatesPending: Boolean;
begin
    Result := FUpdatesJournal.UpdatesPending;
end;

function TOCICustomDatabase.GetUpdatesCount: Integer;
begin
    Result := ProcessJournal([nil], -1, qoCount);
end;

function TOCICustomDatabase.GetUpdateVersion: Integer;
begin
    Result := FUpdatesJournal.UpdatesID;
end;

procedure TOCICustomDatabase.SetUpdateVersion(AValue: Integer);
begin
    FUpdatesJournal.UpdatesID := AValue;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

function TOCIImpHndlDatabase.InterceptHandles(var hEnv, hSrvc, hSrv, hSes,
    hErr, hTX: pOCIHandle): Boolean;
begin
    hEnv := FhImpEnv;
    hSrvc := FhImpSvc;
    hSrv := FhImpSrv;
    hSes := FhImpSes;
    hErr := FhImpErr;
    hTX := FhImpTX;
    Result := True;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

constructor TOCITransactionManager.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FXID := TOCIXid.Create;
    FXID.OnChanged := XIDChanged;
    FXID.OnChanging := XIDChanging;
    FSavepoints := TOCIStringList.Create;
    FAutoCommit := True;
    FTransIsolation := tmReadWrite;
    FInactiveTimeOut := MAXINT;
    FResumeTimeOut := 10;
    FConnectAction := tcSmart;
    FDisConnectAction := tdSmart;
    FCoupleMode := omDefault;
end;

destructor TOCITransactionManager.Destroy;
begin
    FXID.Free;
    FXID := nil;
    FSavepoints.Free;
    FSavepoints := nil;
    FreeHandle;
    inherited Destroy;
end;

procedure TOCITransactionManager.UpdateToLocal;
begin
    XID.Clear;
    if ConnectAction = tcResume then
        ConnectAction := tcStartTransaction;
    if DisconnectAction = tdSuspend then
        DisconnectAction := tdCommit;
end;

procedure TOCITransactionManager.UpdateToGlobal;
begin
    AutoCommit := False;
    RollbackSegment := '';
    if TransIsolation = tmDiscrete then
        TransIsolation := tmReadWrite;
end;

procedure TOCITransactionManager.SetTransIsolation(const Value: TOCITransactionMode);
begin
    if FTransIsolation <> Value then begin
        FTransIsolation := Value;
        if Value <> tmReadWrite then
            RollbackSegment := '';
        if Value = tmDiscrete then
            UpdateToLocal;
    end;
end;

procedure TOCITransactionManager.SetRollbackSegment(const Value: String);
begin
    if FRollbackSegment <> Value then begin
        FRollbackSegment := Value;
        if Value <> '' then begin
            TransIsolation := tmReadWrite;
            UpdateToLocal;
        end;
    end;
end;

function TOCITransactionManager.GetInTransaction: Boolean;
begin
    Result := (State = tsActive);
end;

function TOCITransactionManager.GetIsGlobal: Boolean;
begin
    Result := not XID.IsNull;
end;

procedure TOCITransactionManager.HandleNeeded(hTX: pOCIHandle);
begin
    if FDatabase = nil then
        FDatabase := TOCICustomDatabase.DatabaseByName(FDatabaseName, Self);
    if FDatabase.HService = nil then
        OCIDBError(msgOCIDBmbActive, Self);
    if not HandleAllocated then begin
        if hTX <> nil then
            FTransaction := TOCITransaction.CreateUsingHandle(FDatabase.FService, hTX)
        else begin
            FTransaction := TOCITransaction.Create(FDatabase.FService);
            XIDChanged(nil);
        end;
    end;
end;

procedure TOCITransactionManager.FreeHandle;
begin
    if HandleAllocated then begin
        FTransaction.Free;
        FTransaction := nil;
        if (FDatabase <> nil) and (FDatabase.FDefaultTransactionManager <> Self) then
            FDatabase := nil;
    end;
end;

function TOCITransactionManager.GetHandleAllocated: Boolean;
begin
    Result := FTransaction <> nil;
end;

procedure TOCITransactionManager.ConnectUsingHandle(hTX: pOCIHandle);
begin
    HandleNeeded(hTX);
    if HandleAllocated and not (csDestroying in Database.ComponentState) and
       (State <> tsActive) then begin
        Database.DbgOut(tfTransact, 'Connect');
        case ConnectAction of
        tcNone:
            FTransaction.Select;
        tcResume:
            Resume;
        tcStartTransaction:
            StartTransaction;
        tcSmart:
            begin
                if IsGlobal then begin
                    if State = tsSuspend then
                        Resume
                    else if State = tsInactive then
                        try
                            StartTransaction;
                        except on E: EOCINativeError do
                            // duplicate transaction identifier
                            if E.Errors[0].ErrorCode = 24757 then
                                Resume
                            else
                                raise;
                        end;
                end
                else if State = tsInactive then
                    StartTransaction;
            end;
        end;
    end;
end;

procedure TOCITransactionManager.Connect;
begin
    ConnectUsingHandle(nil);
end;

procedure TOCITransactionManager.Disconnect;
begin
    if HandleAllocated and (State = tsActive) then begin
        Database.DbgOut(tfTransact, 'Disconnect');
        case DisConnectAction of
        tdNone:
            ;
        tdSuspend:
            Suspend;
        tdCommit:
            CommitAll;
        tdRollback:
            RollbackAll;
        tdSmart:
            if State = tsActive then
                if IsGlobal then
                    Suspend
                else
                    CommitAll;
        end;
    end;
    FreeHandle;
end;

procedure TOCITransactionManager.SetState(AState: TOCITransactionState);
begin
    FState := AState;
    if Assigned(FOnStateChanged) then
        FOnStateChanged(Self);
end;

procedure TOCITransactionManager.StartTransaction;
var
    spName: String;
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if Assigned(FBeforeStartTransaction) then
            FBeforeStartTransaction(Self);
        Database.DbgOut(tfTransact, 'StartTransaction');
        FTransaction.Select;
        if InTransaction then begin
            spName := SDefPrefix + IntToStr(FSavepoints.Count);
            Database.ExecSQL('SAVEPOINT ' + spName);
            FSavepoints.Add(spName);
        end
        else begin
            if IsGlobal then
                FTransaction.StartGlobal(TransIsolation, InactiveTimeOut,
                    True, CoupleMode)
            else begin
                if RollbackSegment <> '' then
                    Database.ExecSQL(
                        'set transaction use rollback segment ' + RollbackSegment
                    )
                else if TransIsolation = tmDiscrete then
                    Database.ExecSQL(
                        'begin ' +
                        '   dbms_transaction.begin_discrete_transaction; ' +
                        'exception when dbms_transaction.DISCRETE_TRANSACTION_FAILED then ' +
                        '   dbms_transaction.read_write; ' +
                        'end;'
                    )
                else
                    FTransaction.StartLocal(TransIsolation);
            end;
            SetState(tsActive);
        end;
        if Assigned(FAfterStartTransaction) then
            FAfterStartTransaction(Self);
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.Suspend;
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
    try
        if Assigned(FBeforeSuspendTransaction) then
            FBeforeSuspendTransaction(Self);
        Database.DbgOut(tfTransact, 'Suspend');
        FSavepoints.Clear;
        FTransaction.Select;
        FTransaction.Detach;
        SetState(tsSuspend);
        if Assigned(FAfterSuspendTransaction) then
            FAfterSuspendTransaction(Self);
    except on E: EOCISystemError do
        // If we explicitly not StartTransaction, but implicitly and
        // XID is not null, when we will Suspend, FTransaction.Detach
        // raise OCI_INVALID_HANDLE. In other cases - let's Oracle will say !
        if (E.ErrorCode = OCI_INVALID_HANDLE) and (FOCIVersion < cvOracle81000) then
            OCIDBError(msgOCITransMBStarted, Self)
        else
            raise;
    end;
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.Resume;
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if Assigned(FBeforeResumeTransaction) then
            FBeforeResumeTransaction(Self);
        Database.DbgOut(tfTransact, 'Resume');
        FSavepoints.Clear;
        FTransaction.Select;
        FTransaction.StartGlobal(TransIsolation, ResumeTimeOut,
            False, CoupleMode);
        SetState(tsActive);
        if Assigned(FAfterResumeTransaction) then
            FAfterResumeTransaction(Self);
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.Prepare2PC;
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if Assigned(FBeforePrepareTransaction) then
            FBeforePrepareTransaction(Self);
        Database.DbgOut(tfTransact, 'Prepare');
        FTransaction.Select;
        FTransaction.Prepare2PC;
        SetState(tsPrepared);
        if Assigned(FAfterPrepareTransaction) then
            FAfterPrepareTransaction(Self);
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.InternalCommit(A2PC: Boolean; AAll: Boolean);
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if Assigned(FBeforeCommit) then
            FBeforeCommit(Self);
        Database.DbgOut(tfTransact, 'Commit');
        FTransaction.Select;
        if AAll then
            FSavepoints.Clear;
        if FSavepoints.Count > 0 then
            FSavepoints.Delete(FSavepoints.Count - 1)
        else begin
            try
                if A2PC then
                    FTransaction.Commit2PC
                else
                    FTransaction.Commit;
            except on E: EOCINativeError do
                case E.Errors[0].ErrorCode of
                1012: ; // not logged on
                1042: ; // internal error. hostdef extension doesn't exist
                3114: ; // not connected to Oracle
                else raise;
                end;
            end;
            SetState(tsInactive);
        end;
        if Assigned(FAfterCommit) then
            FAfterCommit(Self);
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.Commit;
begin
    InternalCommit(False, False);
end;

procedure TOCITransactionManager.Commit2PC;
begin
    InternalCommit(True, True);
end;

procedure TOCITransactionManager.CommitAll;
begin
    InternalCommit(False, True);
end;

procedure TOCITransactionManager.InternalRollback(AAll: Boolean);
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if not (csDestroying in ComponentState) and Assigned(FBeforeRollback) then
            FBeforeRollback(Self);
        Database.DbgOut(tfTransact, 'Rollback');
        FTransaction.Select;
        if AAll then
            FSavepoints.Clear;
        if FSavepoints.Count > 0 then begin
            Database.ExecSQL('ROLLBACK TO SAVEPOINT ' + FSavepoints[FSavepoints.Count - 1]);
            FSavepoints.Delete(FSavepoints.Count - 1);
        end
        else begin
            try
                FTransaction.Rollback;
            except on E: EOCINativeError do
                case E.Errors[0].ErrorCode of
                1012: ; // not logged on
                1042: ; // internal error. hostdef extension doesn't exist
                3114: ; // not connected to Oracle
                else raise;
                end;
            end;
            SetState(tsInactive);
        end;
        if not (csDestroying in ComponentState) and Assigned(FAfterRollback) then
            FAfterRollback(Self);
    finally
        Database.EndAccess;
    end;
end;

procedure TOCITransactionManager.Rollback;
begin
    InternalRollback(False);
end;

procedure TOCITransactionManager.RollbackAll;
begin
    InternalRollback(True);
end;

procedure TOCITransactionManager.Forget;
begin
    HandleNeeded(nil);
    Database.StartAccess;
    try
        if Assigned(FBeforeForget) then
            FBeforeForget(Self);
        Database.DbgOut(tfTransact, 'Forget');
        FSavepoints.Clear;
        FTransaction.Select;
        FTransaction.Forget;
        SetState(tsInActive);
        if Assigned(FAfterForget) then
            FAfterForget(Self);
    finally
        Database.EndAccess;
    end;
end;

function TOCITransactionManager.GetAutoCommit: Boolean;
begin
    if InTransaction and not (csDesigning in ComponentState) then
        Result := False
    else
        Result := FAutoCommit;
end;

procedure TOCITransactionManager.SetAutoCommit(const Value: Boolean);
begin
    if FAutoCommit <> Value then begin
        FAutoCommit := Value;
        if Value then
            UpdateToLocal;
        if HandleAllocated then
            Database.DbgOut(tfTransact, 'AutoCommit ' + arrOnOff[FAutoCommit]);
    end;
end;

procedure TOCITransactionManager.CheckInActive;
begin
    if HandleAllocated then
        OCIDBError(msgOCITransMBInAct, Self);
end;

procedure TOCITransactionManager.SetDatabaseName(const Value: String);
begin
    if csReading in ComponentState then
        FDatabaseName := Value
    else if FDatabaseName <> Value then begin
        CheckInActive;
        FDatabaseName := Value;
    end;
end;

procedure TOCITransactionManager.SetXID(const Value: TOCIXid);
begin
    XID.Assign(Value);
end;

procedure TOCITransactionManager.XIDChanging(ASender: TObject);
begin
    CheckInActive;
end;

procedure TOCITransactionManager.XIDChanged(ASender: TObject);
begin
    if IsGlobal then begin
        UpdateToGlobal;
        if HandleAllocated then
            XID.MarkTransaction(FTransaction);
    end
    else
        UpdateToLocal;
end;

procedure TOCITransactionManager.CheckDBNameMatch(const AName: String);
begin
    if not (((DatabaseName = '') or (DatabaseName = SDefaultDBName)) and
            ((AName = '') or (AName = SDefaultDBName)) or
            (DatabaseName = AName)) then
        OCIDBErrorFmt(msgOCIDBNameMismatch, [DatabaseName, AName], Self);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCILongStream

procedure AssignError(ASource, ADestination: TObject);
var
    SourceName: string;
begin
    if ASource <> nil then
        SourceName := ASource.ClassName
    else
        SourceName := 'nil';
    raise EConvertError.CreateFmt(SAssignError, [SourceName, ADestination.ClassName]);
end;

constructor TOCILongStream.Create(Field: TBLobField; Mode: TBlobStreamMode);
var
    buff: pUb1;
    sz: ub4;
begin
    inherited Create;
    FField := Field;
    FParam := nil;
    FMode := Mode;
    FDataSet := FField.DataSet as TOCIDataSet;
    if Mode in [bmWrite, bmReadWrite] then begin
        if FField.ReadOnly then
            OCIDBErrorFmt(msgOCIBlobReadOnly, [FField.DisplayName], FField);
        if not (FDataSet.State in [dsEdit, dsInsert]) then
            OCIDBError(msgDataSetNotEditing, FField);
    end;
    if FDataSet.GetActiveRecBuf(FRecBuf) then begin
        if not (FField.FieldKind in [fkData, fkInternalCalc]) or
           not (FDataSet.ODataType[FField.FieldNo] in [otROWID, otString, otChar, otRaw, otLong, otLongRaw]) then
            OCIDBErrorFmt(msgOCILongStreamInvalid, [FField.DisplayName], nil);
        if Mode in [bmRead, bmReadWrite] then begin
            if FDataSet.FOCICursor.GetDataPtr(FDataSet.GetEditBookmark(FRecBuf),
               FField.FieldNo, buff, sz) then begin
                Write(buff^, sz);
                Position := 0;
            end;
        end;
    end;
    FModified := False;
    FOpened := True;
end;

constructor TOCILongStream.CreateUsingParam(Param: TOciParam; AIndex: Integer; Mode: TBlobStreamMode);
var
    buff: pUb1;
    sz: ub4;
begin
    inherited Create;
    FField := nil;
    FParam := Param;
    FMode := Mode;
    FIndex := AIndex;
    FDataSet := FParam.DataSet as TOCIDataSet;
    FDataSet.CheckActive;
    if Mode in [bmWrite, bmReadWrite] then begin
        if not (FParam.OParamType in [odUnknown, odIn, odInOut]) then
            OCIDBErrorFmt(msgOCIBlobReadOnly, [FParam.DisplayName], nil);
    end;
    if not (FParam.ODataType in [otROWID, otString, otChar, otRaw, otLong, otLongRaw]) then
        OCIDBErrorFmt(msgOCILongStreamInvalid, [FParam.DisplayName], nil);
    if Mode in [bmRead, bmReadWrite] then begin
        if FParam.HVariable.GetDataPtr(AIndex, buff, sz) then begin
            Write(buff^, sz);
            Position := 0;
        end;
    end;
    FModified := False;
    FOpened := True;
end;

destructor TOCILongStream.Destroy;
begin
    if FOpened and FModified then
    try
        if FField <> nil then begin
            FDataSet.CheckActive;
            FDataSet.FOCICursor.SetData(FDataSet.GetEditBookmark(FRecBuf),
                FField.FieldNo, pUb1(Memory), Size);
            TBlobField(FField).Modified := True;
            FDataSet.DataEvent(deFieldChange, Longint(FField));
        end
        else
            FParam.HVariable.SetData(FIndex, pUb1(Memory), Size, dfDelphi);
    except
        OCIDBHandleException(Self);
    end;
    inherited Destroy;
end;

procedure TOCILongStream.CheckCanModify;
begin
    if FOpened and (FMode = bmRead) then
        if FField <> nil then
            OCIDBErrorFmt(msgOCIBlobReadOnly, [FField.DisplayName], nil)
        else
            OCIDBErrorFmt(msgOCIBlobReadOnly, [FParam.DisplayName], nil);
end;

procedure TOCILongStream.Clear;
begin
    CheckCanModify;
    inherited Clear;
    FModified := True;
end;

procedure TOCILongStream.SetSize(NewSize: Longint);
begin
    CheckCanModify;
    inherited SetSize(NewSize);
    FModified := True;
end;

function TOCILongStream.Write(const Buffer; Count: Longint): Longint;
begin
    CheckCanModify;
    Result := inherited Write(Buffer, Count);
    FModified := True;
end;

procedure TOCILongStream.Truncate;
begin
    SetSize(Position + 1);
end;

procedure TOCILongStream.Assign(ASource: TObject);
begin
    if ASource = Self then
        Exit;
    if ASource is TStream then
        CopyFrom(TStream(ASource), 0)
    else
        AssignError(ASource, Self);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCILOBStream

constructor TOCILOBStream.Create(Field: TBLobField; Mode: TBlobStreamMode;
    ALobLocatorClass: TOCILobLocatorClass);
var
    ptr: ppOCIHandle;
    sz: ub4;
    RecBuf: PChar;
begin
    inherited Create;
    FPosition := 0;
    FMode := Mode;
    FModified := False;
    FField := Field;
    FParam := nil;
    FDataSet := FField.DataSet as TOCIDataSet;
    FDataSet.CheckActive;
    if not FDataSet.GetActiveRecBuf(RecBuf) or (FDataSet.State = dsInsert) then
        Exit;
    if Mode in [bmWrite, bmReadWrite] then begin
        if FField.ReadOnly then
            OCIDBErrorFmt(msgOCIBlobReadOnly, [FField.DisplayName], nil);
        if not (FDataSet.State in [dsEdit, dsInsert]) then
            OCIDBError(msgDataSetNotEditing, FField);
    end;
    if not (FField.FieldKind in [fkData, fkInternalCalc]) or
       not (FDataSet.ODataType[FField.FieldNo] in otHBlobs) then
        OCIDBErrorFmt(msgOCILOBStreamInvalid, [FField.DisplayName], nil);
    FDataSet.FOCICursor.GetDataPtr(FDataSet.GetEditBookmark(RecBuf), FField.FieldNo, pUb1(ptr), sz);
    FOCILobLocator := ALobLocatorClass.CreateUseHandle(FDataSet.FDatabase.FService, ptr^);
    FOCILobLocator.OwningObj := FDataSet;
    if (Mode = bmWrite) and not IsNull then
        Truncate;
end;

constructor TOCILOBStream.CreateUsingParam(Param: TOciParam; AIndex: Integer;
    Mode: TBlobStreamMode; ALobLocatorClass: TOCILobLocatorClass);
var
    ptr: ppOCIHandle;
    sz: ub4;
begin
    inherited Create;
    FPosition := 0;
    FMode := Mode;
    FModified := False;
    FField := nil;
    FParam := Param;
    FIndex := AIndex;
    FDataSet := FParam.DataSet as TOCIDataSet;
    if not (FParam.ODataType in otHBlobs) then
        OCIDBErrorFmt(msgOCILOBStreamInvalid, [FParam.DisplayName], nil);
    FParam.HVariable.GetDataPtr(AIndex, pUb1(ptr), sz);
    FOCILobLocator := ALobLocatorClass.CreateUseHandle(FDataSet.FDatabase.FService, ptr^);
    FOCILobLocator.OwningObj := FDataSet;
    if (Mode = bmWrite) and not IsNull then
        Truncate;
end;

constructor TOCILOBStream.CreateAlone(ADatabase: TOCICustomDatabase;
    Mode: TBlobStreamMode; AODataType: TOCIVarDataType; ALobLocatorClass: TOCILobLocatorClass);
begin
    inherited Create;
    FPosition := 0;
    FMode := Mode;
    FModified := False;
    FField := nil;
    FParam := nil;
    FIndex := 0;
    FDataSet := nil;
    FOCILobLocator := ALobLocatorClass.Create(ADatabase.FService, nc2ociValueType[AODataType]);
    if (Mode = bmWrite) and not IsNull then
        Truncate;
end;

destructor TOCILOBStream.Destroy;
begin
    try
        if FModified then
            if FField <> nil then begin
                FDataSet.CheckActive;
                FDataSet.BlobModified(FField, IsNull);
            end
            else if FParam <> nil then
                FParam.BlobModified(FIndex, IsNull);
        if FOCILobLocator <> nil then begin
            if FOCILobLocator.IsInit then
              if FOCILobLocator.IsOpen then
                FOCILobLocator.Close;
            FOCILobLocator.Free;
            FOCILobLocator := nil;
        end;
    except
        OCIDBHandleException(Self);
    end;
    inherited Destroy;
end;

function TOCILOBStream.IsNull: Boolean;
begin
    Result := (FOCILobLocator = nil) or not FOCILobLocator.IsInit;
end;

procedure TOCILOBStream.CheckOpen;
begin
    if not IsNull and not FOCILobLocator.IsOpen then
        FOCILobLocator.Open(FMode = bmRead);
end;

procedure TOCILOBStream.CheckWrite;
begin
    if IsNull or (Self is TOCIFILEStream) or (FMode = bmRead) then
        OCIDBError(msgOCIBlobReadOnly, FField);
end;

function TOCILOBStream.Read(var Buffer; Count: Longint): Longint;
begin
    if IsNull or (Count < 0) then
        Result := 0
    else begin
        Result := Count;
        CheckOpen;
        FOCILobLocator.Read(@Buffer, Count, ub4(Result), FPosition + 1);
        Inc(FPosition, Result);
    end;
end;

function TOCILOBStream.Write(const Buffer; Count: Longint): Longint;
begin
    CheckOpen;
    CheckWrite;
    Result := 0;
end;

function TOCILOBStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
    CheckOpen;
    case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd:
        if IsNull then
            FPosition := 0
        else
            FPosition := FOCILobLocator.Length + Offset;
    end;
    Result := FPosition;
end;

procedure TOCILOBStream.Truncate;
begin
    CheckOpen;
    CheckWrite;
    SetSize(0);
end;

procedure TOCILOBStream.Assign(ASource: TObject);
begin
    if ASource = Self then
        Exit;
    if ASource is TOCILOBStream then begin
        if (FMode = bmRead) or (FOCILobLocator = nil) then
            OCIDBError(msgOCIBlobReadOnly, FField);
        FOCILobLocator.Assign(TOCILOBStream(ASource).FOCILobLocator);
        FPosition := 0;
        FModified := True;
        CheckOpen;
    end
    else if ASource is TStream then
        CopyFrom(TStream(ASource), 0)
    else
        AssignError(ASource, Self);
end;

procedure TOCILOBStream.SetSize(NewSize: Integer);
begin
    CheckOpen;
    CheckWrite;
end;

constructor TOCIFILEStream.Create(Field: TBLobField; Mode: TBlobStreamMode);
begin
    inherited Create(Field, Mode, TOCIExtLocator);
end;

constructor TOCIFILEStream.CreateUsingParam(Param: TOciParam; AIndex: Integer;
    Mode: TBlobStreamMode);
begin
    inherited CreateUsingParam(Param, AIndex, Mode, TOCIExtLocator);
end;

constructor TOCIFILEStream.CreateAlone(ADatabase: TOCICustomDatabase;
    Mode: TBlobStreamMode; AODataType: TOCIVarDataType);
begin
    inherited CreateAlone(ADatabase, Mode, AODataType, TOCIExtLocator);
end;

function TOCIFILEStream.GetFileName: String;
begin
    CheckOpen;
    if FOCILobLocator = nil then
        Result := ''
    else
        Result := TOCIExtLocator(FOCILobLocator).FileName;
end;

procedure TOCIFILEStream.SetFileName(const AValue: String);
begin
    CheckOpen;
    CheckWrite;
    TOCIExtLocator(FOCILobLocator).FileName := AValue;
    FModified := True;
end;

function TOCIFILEStream.GetDirectory: String;
begin
    CheckOpen;
    if FOCILobLocator = nil then
        Result := ''
    else
        Result := TOCIExtLocator(FOCILobLocator).Directory;
end;

procedure TOCIFILEStream.SetDirectory(const AValue: String);
begin
    CheckOpen;
    CheckWrite;
    TOCIExtLocator(FOCILobLocator).Directory := AValue;
    FModified := True;
end;

function TOCIFILEStream.GetFileExists: Boolean;
begin
    CheckOpen;
    if FOCILobLocator = nil then
        Result := False
    else
        Result := TOCIExtLocator(FOCILobLocator).FileExists;
end;

function TOCIFILEStream.GetLocator: TOCIExtLocator;
begin
    Result := TOCIExtLocator(FOCILobLocator);
end;

constructor TOCIILOBStream.Create(Field: TBLobField; Mode: TBlobStreamMode);
begin
    inherited Create(Field, Mode, TOCIIntLocator);
end;

constructor TOCIILOBStream.CreateUsingParam(Param: TOciParam; AIndex: Integer;
    Mode: TBlobStreamMode);
begin
    inherited CreateUsingParam(Param, AIndex, Mode, TOCIIntLocator);
end;

constructor TOCIILOBStream.CreateAlone(ADatabase: TOCICustomDatabase;
    Mode: TBlobStreamMode; AODataType: TOCIVarDataType);
begin
    inherited CreateAlone(ADatabase, Mode, AODataType, TOCIIntLocator);
end;

function TOCIILOBStream.Write(const Buffer; Count: Longint): Longint;
begin
    inherited Write(Buffer, Count);
    Result := Count;
    TOCIIntLocator(FOCILobLocator).Write(@Buffer, Count, ub4(Result), FPosition + 1);
    Inc(FPosition, Result);
    FModified := True;
end;

function TOCIILOBStream.GetLocator: TOCIIntLocator;
begin
    Result := TOCIIntLocator(FOCILobLocator);
end;

procedure TOCIILOBStream.SetSize(NewSize: Integer);
var
    C: ub1;
begin
    inherited SetSize(NewSize);
    if NewSize < Size then begin
        TOCIIntLocator(FOCILobLocator).Trim(NewSize);
        FModified := True;
    end
    else if NewSize > Size then begin
        Seek(NewSize - 1, soFromBeginning);
        C := 32;
        Write(C, 1);
        FModified := True;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIFieldDef & TOCIFieldDefs

procedure TOCIFieldDef.Assign(AObject: TPersistent);
begin
    if AObject = Self then
        Exit;
    if AObject is TOCIFieldDef then begin
        FName := TOCIFieldDef(AObject).Name;
        FIsNull := TOCIFieldDef(AObject).IsNull;
        FDataType := TOCIFieldDef(AObject).DataType;
        FDataSize := TOCIFieldDef(AObject).DataSize;
        FScale := TOCIFieldDef(AObject).Scale;
        FPrecision := TOCIFieldDef(AObject).Precision;
        FFieldNo := TOCIFieldDef(AObject).FieldNo;
    end
    else
        inherited Assign(AObject);
end;

constructor TOCIFieldDefs.Create;
begin
    inherited Create(TOCIFieldDef);
end;

function TOCIFieldDefs.FindByNo(AFieldNo: Integer): TOCIFieldDef;
var
    i: Integer;
begin
    Result := nil;
    for i := 0 to Count - 1 do
        if Items[i].FieldNo = AFieldNo then begin
            Result := Items[i];
            Break;
        end;
end;

function TOCIFieldDefs.FindDef(const AName: String): TOCIFieldDef;
var
    i: Integer;
begin
    Result := nil;
    for i := 0 to Count - 1 do
        if Items[i].Name = AName then begin
            Result := Items[i];
            Break;
        end;
end;

function TOCIFieldDefs.GetFieldDef(AIndex: Integer): TOCIFieldDef;
begin
    Result := inherited Items[AIndex] as TOCIFieldDef;
end;

procedure TOCIFieldDefs.SetFieldDef(AIndex: Integer; const Value: TOCIFieldDef);
begin
    (inherited Items[AIndex] as TOCIFieldDef).Assign(Value);
end;

function TOCIFieldDefs.Add: TOCIFieldDef;
begin
    Result := inherited Add as TOCIFieldDef;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIUpdateObject

destructor TOCIUpdateObject.Destroy;
begin
    if (DataSet <> nil) then begin
        if DataSet.UpdateObject = Self then
            DataSet.UpdateObject := nil;
        FDataSet := nil;
    end;
    inherited Destroy;
end;

procedure TOCIUpdateObject.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then
        if AComponent = DataSet then
            FDataSet := nil;
end;

procedure TOCIUpdateObject.CheckNoUpdatesPending;
begin
    if FDataSet <> nil then
        FDataSet.CheckNoUpdatesPending;
end;

procedure TOCIUpdateObject.SetUpdateMode(const AValue: TUpdateMode);
begin
    FUpdateMode := AValue;
end;

procedure TOCIUpdateObject.Apply(ASQLKind: TOCISQLKind; AOptions: TOCISQLKindOptions);
begin
    FApplyOptions := AOptions;
    if Supported[ASQLKind] then begin
        if (ASQLKind = skLock) and DataSet.LockMark or
           (ASQLKind = skUnLock) and not DataSet.LockMark then
            Exit;
        InternalApply(ASQLKind);
        if ASQLKind = skLock then
            DataSet.LockMark := True
        else if ASQLKind = skUnLock then
            DataSet.LockMark := False;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIDataSet

constructor TOCIDataSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFetchParams := TOCIFetchParams.Create(Self);
    FDataFormat := TOCIDataFormat.Create(Self);
    FFilters := TOCIFilters.Create(Self);
    FRecordCountMode := cmFetched;
    FRecordCount := -1;
    FOFieldDefs := TOCIFieldDefs.Create;
    FExpressions := TOCIStringList.Create;
    FExpressions.Sorted := True;
    FExpressions.Duplicates := dupError;
    FPostSeqList := TList.Create;
{$IFDEF OCI_D4}
    NestedDataSetClass := TOCINestedDataSet;
{$ENDIF}
    FAllowedOperations := [skLock .. skRefresh];
    FDatabase := TOCICustomDatabase.FindDatabase('', Self);
end;

destructor TOCIDataSet.Destroy;
begin
    Destroying;
    Disconnect;
    FFetchParams.Free;
    FFetchParams := nil;
    FDataFormat.Free;
    FDataFormat := nil;
    FFilters.Free;
    FFilters := nil;
    FOFieldDefs.Free;
    FOFieldDefs := nil;
    FExpressions.Free;
    FExpressions := nil;
    FPostSeqList.Free;
    FPostSeqList := nil;
    TransactionManager := nil;
    inherited Destroy;
end;

procedure TOCIDataSet.DoError(ASender: TObject; var E: Exception);
begin
    if Assigned(FOnError) then
        FOnError(ASender, E);
end;

procedure TOCIDataSet.DoWarning(ASender: TObject; var E: Exception);
begin
    if Assigned(FOnWarning) then
        FOnWarning(ASender, E);
end;

procedure TOCIDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if TransactionManager = AComponent then
            TransactionManager := nil;
        if UpdateObject = AComponent then
            UpdateObject := nil;
    end;
end;

function TOCIDataSet.IsDBNS: Boolean;
begin
    Result := FDatabaseName <> SDefaultDBName;
end;

procedure TOCIDataSet.SetFetchParams(AValue: TOCIFetchParams);
begin
    FFetchParams.Assign(AValue);
end;

procedure TOCIDataSet.SetDataFormat(const Value: TOCIDataFormat);
begin
    FDataFormat.Assign(Value);
end;

function TOCIDataSet.GetDescribed: Boolean;
begin
    Result := FOCICursor <> nil;
end;

procedure TOCIDataSet.OpenCursor(InfoQuery: Boolean);
begin
    IncDBUse;
    if not Active then
        if InfoQuery then
            DescribeOCICursor
        else
            CreateOCICursor;
    inherited OpenCursor(InfoQuery);
end;

procedure TOCIDataSet.InternalOpen;
begin
    UpdateCachedUpdatesProp(FCachedUpdates, FCachedUpdatesArea);
{$IFDEF OCI_D4}
    FieldDefs.Updated := False;
    FieldDefs.Update;
{$ELSE}
    InternalInitFieldDefs;
{$ENDIF}
    if DefaultFields then
        CreateFields;
    BindFields(True);
    OBindFields(True);
    InitBufferPointers;
    SetBufListSize(1);
    UpdateFiltering(False, True);
    InternalFirst;
    FOCICursor.OnUpdateRecord := UpdateRecordHandler;
    FOCICursor.OnUpdateError := UpdateErrorHandler;
    FOCICursor.OpenComplete;
//    if RecordCountMode = cmExactOnOpen then
//        GetRecordCount;
    PrepareExpressions;
    Inc(FUsageCount);
end;

procedure TOCIDataSet.DoAfterOpen;
begin
    if RecordCountMode = cmExactOnOpen then
        GetRecordCount;
    inherited DoAfterOpen;
end;

procedure TOCIDataSet.OBindFields(Binding: Boolean);
var
    i: Integer;
    ridFld: TField;
begin
    if Binding then begin
        FKeyFields := '';
        for i := 0 to FieldCount - 1 do
            with Fields[i] do begin
                if (Origin = '') and (FieldKind in [fkData, fkInternalCalc]) then
                    Origin := OName[FieldNo];
{$IFDEF OCI_D4}
                if not ((DataType in [ftBytes, ftVarBytes]) or IsBlob or
                        (Fields[i] is TObjectField) or not (pfInKey in ProviderFlags)) then
                    if FKeyFields <> '' then
                        FKeyFields := FKeyFields + ';' + FieldName
                    else
                        FKeyFields := FieldName;
{$ENDIF}
            end;
        if (FROWIDFieldNo <> -1) and (FKeyFields = '') then begin
            ridFld := FieldByNumber(FROWIDFieldNo);
            if ridFld <> nil then
                with ridFld do begin
{$IFDEF OCI_D4}
                    ProviderFlags := ProviderFlags + [pfInKey] - [pfInUpdate];
{$ENDIF}
                    ReadOnly := True;
                    FKeyFields := FieldName;
                end;
        end;
    end;
end;

procedure TOCIDataSet.CloseCursor;
begin
    inherited CloseCursor;
    DecDBUse;
    DestroyOCICursor;
end;

procedure TOCIDataSet.InternalClose;
begin
    ClearExpressions;
    UpdateFiltering(False, False);
    FRecordCount := -1;
    OBindFields(False);
    BindFields(False);
    if DefaultFields then
        DestroyFields;
end;

function TOCIDataSet.IsCursorOpen: Boolean;
begin
    Result := (FOCICursor <> nil) and FOCICursor.Active;
end;

procedure TOCIDataSet.InternalInitFieldDefs;
var
    i, j: Integer;
    FSize, FPrecision: Integer;
    FName: String;
begin
    FieldDefs.Clear;
    FROWIDFieldNo := -1;
    FHaveBLOBs := False;
    for i := 0 to OFieldDefs.Count - 1 do begin
        // Name
        FName := OFieldDefs[i].Name;
        j := 0;
        while FieldDefs.IndexOf(FName) >= 0 do begin
            Inc(j);
            FName := OFieldDefs[i].Name + '_' + IntToStr(j);
        end;
        // Type & Size
        FSize := 0;
        FPrecision := 0;
        case OFieldDefs[i].DataType of
            otBCD, otNumber:
                begin
                    FPrecision := OFieldDefs[i].Precision;
                    FSize := OFieldDefs[i].DataSize;
                end;
            otString, otChar, otRaw, otLong, otLongRaw:
                FSize := OFieldDefs[i].DataSize;
            otCLOB, otBLOB, otCFile, otBFile:
                begin
                    FSize := OFieldDefs[i].DataSize;
                    FHaveBLOBs := True;
                end;
            otROWID:
                begin
                    FSize := IRowIdLen;
                    if FROWIDFieldNo = -1 then
                        FROWIDFieldNo := OFieldDefs[i].FieldNo;
                end;
        end;
{$IFNDEF OCI_D35}
        TFieldDef.Create(FieldDefs, FName, ot2dt[OFieldDefs[i].DataType], FSize,
            not OFieldDefs[i].IsNull, OFieldDefs[i].FieldNo).Precision := FPrecision;
{$ELSE}
        with FieldDefs.AddFieldDef do begin
            FieldNo := OFieldDefs[i].FieldNo;
            Name := FName;
            DataType := ot2dt[OFieldDefs[i].DataType];
            Size := FSize;
            Precision := FPrecision;
{$IFDEF OCI_D35}
            Required := not OFieldDefs[i].IsNull;
{$ELSE}
            Attributes := [];
            if not OFieldDefs[i].IsNull then
                Attributes := Attributes + [faRequired];
            if OFieldDefs[i].DataType in [otCFile, otBFile] then
                Attributes := Attributes + [faReadOnly];
{$ENDIF}
{$IFDEF OCI_D5}
            if OFieldDefs[i].DataType in [otChar] then
                Attributes := Attributes + [faFixed];
{$ENDIF}
        end;
{$ENDIF}
    end;
end;

procedure TOCIDataSet.InternalHandleException;
begin
    OCIDBHandleException(Self);
end;

function TOCIDataSet.GetOItem(AFieldNo: Integer): TOCIFieldDef;
begin
    if AFieldNo > 0 then
        Result := OFieldDefs.FindByNo(AFieldNo)
    else
        Result := nil;
    if Result = nil then
        OCIDBErrorFmt(SFieldNotFound, ['FieldNo=' + IntToStr(AFieldNo)], Self);
end;

function TOCIDataSet.GetOName(AFieldNo: Integer): String;
begin
    if AFieldNo < 0 then
        Result := FieldByNumber(AFieldNo).FieldName
    else
        Result := OItem[AFieldNo].Name;
end;

function TOCIDataSet.GetODataType(AFieldNo: Integer): TOCIVarDataType;
begin
    if AFieldNo < 0 then
        Result := dt2ot[FieldByNumber(AFieldNo).DataType]
    else
        Result := OItem[AFieldNo].DataType;
end;

function TOCIDataSet.GetODataSize(AFieldNo: Integer): ub4;
begin
    if AFieldNo < 0 then
        Result := FieldByNumber(AFieldNo).Size
    else
        Result := OItem[AFieldNo].DataSize;
end;

function TOCIDataSet.GetTableName: String;
begin
    Result := '';
end;

// ------------------------------------------------------------
// Database

procedure TOCIDataSet.Disconnect;
begin
    Close;
end;

procedure TOCIDataSet.IncDBUse;
begin
    if FDBUseCount = 0 then begin
        FDatabase := TOCICustomDatabase.DatabaseByName(FDatabaseName, Self);
        FDatabase.AttachDataSet(Self);
    end;
    Inc(FDBUseCount);
end;

procedure TOCIDataSet.DecDBUse;
begin
    if FDBUseCount = 1 then begin
        FDatabase.DetachDataSet(Self);
        FDatabase := nil;
    end;
    Dec(FDBUseCount);
end;

procedure TOCIDataSet.SetDatabaseName(const Value: String);
begin
    if csReading in ComponentState then
        FDatabaseName := Value
    else if FDatabaseName <> Value then begin
        CheckInactive;
        if FDBUseCount <> 0 then
            OCIDBError(msgOCIDBmbInactive, Self);
        FDatabaseName := Value;
        DataEvent(dePropertyChange, 0);
        FDatabase := TOCICustomDatabase.FindDatabase(FDatabaseName, Self);
    end;
end;

procedure TOCIDataSet.CloseDatabase(Database: TOCICustomDatabase);
begin
    if Database <> nil then
        Database.Close;
end;

function TOCIDataSet.OpenDatabase: TOCICustomDatabase;
begin
    Result := TOCICustomDatabase.DatabaseByName(FDatabaseName, Self);
    Result.Open;
end;

function TOCIDataSet.GetPointedDatabase: TOCICustomDatabase;
begin
    if (FDatabase = nil) or
       not ((FDatabaseName = '') and (FDatabase.DatabaseName = SDefaultDBName) or
            (FDatabaseName = FDatabase.DatabaseName)) then
        FDatabase := TOCICustomDatabase.FindDatabase(FDatabaseName, Self);
    Result := FDatabase;
end;

// -------------------------------------------------------------
// Buffer related
{---------------------------------------------------------------}
{ Self Bookmark   | Ref Bookmark    | CalcFields | TRecInfo     }
{---------------------------------------------------------------}
{ cursor shadow = | actual record = | .......... | RNo, UpdStat,}
{ = Delphi buff   | = OCI buff      |            | BmkFlag      }
{---------------------------------------------------------------}

type
    TRecBoundInfo = (rbOff, rbEdited, rbInserted);
    PRecInfo = ^TRecInfo;
    TRecInfo = record
        RecordNumber: Longint;
        UpdateStatus: TUpdateStatus;
        BookmarkFlag: TBookmarkFlag;
        Bounded: TRecBoundInfo;
    end;

procedure TOCIDataSet.InitBufferPointers;
begin
    BookmarkSize := FOCICursor.BookmarkSize;
    FRecordSize := BookmarkSize * 2;
    FRecInfoOfs := FRecordSize + CalcFieldsSize;
    FRecBufSize := FRecInfoOfs + SizeOf(TRecInfo);
end;

function TOCIDataSet.GetRecordSize: Word;
begin
    Result := FRecordSize;
end;

function TOCIDataSet.AllocRecordBuffer: PChar;
begin
    Result := AllocMem(FRecBufSize);
    Move(PChar(FOCICursor.AllocateRecord)^, Result^, BookmarkSize);
end;

procedure TOCIDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
    FOCICursor.FreeRecord(POCIBookmark(Buffer));
    FreeMem(Buffer);
end;

procedure TOCIDataSet.InternalInitRecord(Buffer: PChar);
begin
    FOCICursor.InitRecord(POCIBookmark(Buffer));
    FillChar(Buffer[BookmarkSize], BookmarkSize, 0);
    FillChar(Buffer[RecordSize], FRecBufSize - RecordSize, 0);
end;

procedure TOCIDataSet.ClearCalcFields(Buffer: PChar);
begin
    FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

procedure TOCIDataSet.InitRecord(Buffer: PChar);
begin
    inherited InitRecord(Buffer);
    with PRecInfo(Buffer + FRecInfoOfs)^ do begin
        UpdateStatus := usInserted;
        BookMarkFlag := bfInserted;
        RecordNumber := -1;
        Bounded := rbInserted;
    end;
end;

function TOCIDataSet.OCIrs2ds(ARS: TOCIRecordStatus): TUpdateStatus;
const
    rs2ds: array [TOCIRecordStatus] of TUpdateStatus = (usUnmodified, usUnmodified,
        usUnmodified, usInserted, usModified, usDeleted, usUnmodified, usUnmodified,
        usUnmodified, usUnmodified, usUnmodified);
begin
    Result := rs2ds[ARS];
end;

function TOCIDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
    recProp: TOCICursorRecProp;
begin
    Database.StartWait;
    Result := grError;
    try
        case GetMode of
            gmNext: FOCICursor.Skip(1);
            gmPrior: FOCICursor.Skip(-1);
        end;
        if FOCICursor.EOF then
            Result := grEOF
        else if FOCICursor.BOF then
            Result := grBOF
        else begin
            FOCICursor.GetRecord(POCIBookmark(Buffer),
                POCIBookmark(Buffer + BookmarkSize), recProp);
            with PRecInfo(Buffer + FRecInfoOfs)^ do begin
                UpdateStatus := OCIrs2ds(recProp.FStatus);
                RecordNumber := recProp.FNumber;
                BookmarkFlag := bfCurrent;
                Bounded := rbOff;
            end;
            GetCalcFields(Buffer);
            Result := grOK;
        end;
        Database.EndWait;
    except
        Database.EndWait;
        if DoCheck then
            raise;
    end;
end;

function TOCIDataSet.GetCurrentRecord(Buffer: PChar): Boolean;
var
    recProp: TOCICursorRecProp;
begin
    if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then begin
        UpdateCursorPos;
        try
            FOCICursor.GetRecord(POCIBookmark(Buffer),
                POCIBookmark(Buffer + BookmarkSize), recProp);
            Result := True;
        except
            Result := False;
        end;
    end
    else
        Result := False;
end;

procedure TOCIDataSet.Resync(Mode: TResyncMode);
begin
    if Active then begin
        FOCICursor.Skip(0);
        inherited Resync(Mode);
    end;
end;

procedure TOCIDataSet.FetchAll;
begin
    CheckBrowseMode;
    Database.StartWait;
    try
        FOCICursor.FetchAll;
        CursorPosChanged;
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.OpenExact(ANRows: sb4);
begin
    CheckInActive;
    FetchParams.FetchExact := ANRows;
    Open;
end;

procedure TOCIDataSet.DoNavigationalCount;
var
    pBmk: POCIBookmark;
begin
    FRecordCount := 0;
    if Active and FOCICursor.IsBookmarkDefined then begin
        pBmk := FOCICursor.Bookmark;
        try
            FOCICursor.First(False);
            while not FOCICursor.EOF do begin
                Inc(FRecordCount);
                FOCICursor.Skip(1);
            end;
        finally
            FOCICursor.Bookmark := pBmk;
        end;
    end;
end;

function TOCIDataSet.GetRecordCount: Integer;
begin
    Result := -1;
    if Assigned(BeforeRecordCount) then
        BeforeRecordCount(Self);
    case RecordCountMode of
    cmFetched:
        begin
            CheckActive;
            Result := FOCICursor.RecordCount;
        end;
    cmExactOnOpen:
        begin
            if FRecordCount = -1 then
                if IsSequenced then
                    UpdateRecordCount
                else
                    DoNavigationalCount;
            Result := FRecordCount;
        end;
    cmExactAllways:
        begin
            if IsSequenced then
                UpdateRecordCount
            else
                DoNavigationalCount;
            Result := FRecordCount;
        end;
    end;
    if Assigned(AfterRecordCount) then
        AfterRecordCount(Self);
end;

procedure TOCIDataSet.UpdateRecordCount;
begin
    FRecordCount := -1;
end;

function TOCIDataSet.GetRecNo: Integer;
var
    BufPtr: PChar;
begin
    CheckActive;
    if State = dsFilter then
        Result := FOCICursor.RecNo
    else begin
        if State = dsCalcFields then
            BufPtr := CalcBuffer
        else
            BufPtr := ActiveBuffer;
        Result := PRecInfo(BufPtr + FRecInfoOfs).RecordNumber;
    end;
end;

procedure TOCIDataSet.SetRecNo(Value: Integer);
begin
    CheckBrowseMode;
    if Value <> RecNo then begin
        DoBeforeScroll;
        try
            FOCICursor.RecNo := Value;
            Resync([rmCenter]);
            DoAfterScroll;
        except
        end;
    end;
end;

function TOCIDataSet.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
    case State of
{$IFDEF OCI_D4}
    dsBlockRead,
{$ENDIF}
    dsBrowse:
        if IsEmpty then
            RecBuf := nil
        else
            RecBuf := ActiveBuffer;
    dsEdit, dsInsert:
        RecBuf := ActiveBuffer;
    dsSetKey:
        RecBuf := nil;
    dsCalcFields:
        RecBuf := CalcBuffer;
    dsFilter:
        RecBuf := FFilterBuffer;
    dsNewValue:
        if FOCICursor.InApplyUpdates then
            RecBuf := FOCICursor.GetNewRecord(nil)
        else
            RecBuf := FOCICursor.GetNewRecord(POCIBookmark(ActiveBuffer));
    dsOldValue:
        if FOCICursor.InApplyUpdates then
            RecBuf := FOCICursor.GetOldRecord(nil)
        else
            RecBuf := FOCICursor.GetOldRecord(POCIBookmark(ActiveBuffer));
    else
        RecBuf := nil;
    end;
    Result := RecBuf <> nil;
end;

function TOCIDataSet.GetEditBookmark(Buffer: PChar): POCIBookmark;
begin
    if Buffer = nil then
        Result := nil
    else if (State in [dsFilter, dsNewValue, dsOldValue, dsInactive]) then
        Result := POCIBookmark(Buffer)
    else with PRecInfo(Buffer + FRecInfoOfs)^ do
        if Bounded in [rbEdited, rbInserted] then
            Result := POCIBookmark(Buffer)
        else
            Result := POCIBookmark(Buffer + BookmarkSize);
end;

procedure TOCIDataSet.SetUnidirectional(const Value: Boolean);
begin
    if FUnidirectional <> Value then begin
        FUnidirectional := Value;
        if Value then
            FetchParams.FetchAll := False;
    end;
end;

// -------------------------------------------------------------
// Field related

{$IFDEF OCI_D4}
function TOCIDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
{$ELSE}
function TOCIDataSet.GetFieldDataByNo(FieldNo: Integer; Buffer: Pointer): Boolean;
{$ENDIF}
var
    RecBuf: PChar;
    sz: ub4;
begin
    try
        Result := GetActiveRecBuf(RecBuf);
        if Result then begin
            Result := FOCICursor.GetData(GetEditBookmark(RecBuf), FieldNo, Buffer, sz);
            if Result and (Buffer <> nil) and
               (FieldByNumber(FieldNo).DataType in [ftString, {$IFDEF OCI_D4} ftFixedChar, {$ENDIF} ftMemo]) then
                PChar(Buffer)[sz] := #0;
        end;
    except
{$IFDEF OCI_D4}
        if BlockReadSize > 0 then
            Result := False
        else
{$ENDIF}
            raise;
    end;
end;

function TOCIDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
    RecBuf: PChar;
begin
    if Field.FieldNo > 0 then
{$IFDEF OCI_D4}
        Result := GetFieldData(Field.FieldNo, Buffer)
{$ELSE}
        Result := GetFieldDataByNo(Field.FieldNo, Buffer)
{$ENDIF}
    else begin
        if State = dsFilter then begin
            RecBuf := TempBuffer;
            Result := True;
        end
        else
            Result := GetActiveRecBuf(RecBuf);
        if Result and (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields,
                                 {$IFDEF OCI_D4} dsBlockRead, {$ENDIF} dsFilter]) then begin
            Inc(RecBuf, FRecordSize + Field.Offset);
            Result := Boolean(RecBuf[0]);
            if Result and (Buffer <> nil) then
                Move(RecBuf[1], Buffer^, Field.DataSize);
        end;
    end;
end;

procedure TOCIDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
    RecBuf: PChar;
begin
    with Field do begin
        if State = dsFilter then
            RecBuf := TempBuffer
        else
            GetActiveRecBuf(RecBuf);
        if FieldNo > 0 then begin
            if (State = dsCalcFields) or not (State in dsWriteModes) then
                DatabaseError(SNotEditing {$IFDEF OCI_D4}, Self {$ENDIF});
            if ReadOnly and not (State in [dsSetKey, dsFilter, dsNewValue]) then
                DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
            if (State = dsSetKey) and (FieldNo < 0) then
                DatabaseErrorFmt(SNotIndexField, [DisplayName]);
            if (State in [dsEdit, dsInsert]) then begin
                FCurValidate := OnValidate;
                OnValidate := DoValidate;
                try
                    Validate(Buffer);
                finally
                    OnValidate := FCurValidate;
                end;
            end;
            if FieldKind <> fkInternalCalc then
                FOCICursor.SetData(GetEditBookmark(RecBuf), FieldNo, Buffer, $FFFFFFFF);
        end
        else begin
            if not (State in dsWriteModes) then 
                DatabaseError(SNotEditing {$IFDEF OCI_D4}, Self {$ENDIF});
            Inc(RecBuf, FRecordSize + Offset);
            Boolean(RecBuf[0]) := LongBool(Buffer);
            if Boolean(RecBuf[0]) then
                Move(Buffer^, RecBuf[1], DataSize);
        end;
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
            DataEvent(deFieldChange, Longint(Field));
    end;
end;

function TOCIDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
    Result := nil;
    case ODataType[Field.FieldNo] of
    otROWID, otString, otChar, otRaw, otLong, otLongRaw:
        Result := TOCILongStream.Create(Field as TBLobField, Mode);
    otCLOB, otBLOB:
        Result := TOCIILOBStream.Create(Field as TBLobField, Mode);
    otCFile, otBFile:
        Result := TOCIFILEStream.Create(Field as TBLobField, Mode);
    else
        OCIDBError(msgOCIBlobReadOnly, Self);
    end;
end;

procedure TOCIDataSet.CloseBlob(Field: TField);
begin
    {TODO
        Current LONG/BLOB realization does not need CloseBlob.
        May be later...
    TODO}
end;

procedure TOCIDataSet.BlobModified(AField: TField; AIsNull: Boolean);
var
    RecBuf: PChar;
begin
    GetActiveRecBuf(RecBuf);
    FOCICursor.SetIsNull(GetEditBookmark(RecBuf), AField.FieldNo, AIsNull);
    TBlobField(AField).Modified := True;
    DataEvent(deFieldChange, Longint(AField));
end;

{$IFNDEF OCI_D5}
function TOCIDataSet.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
    Curr := PCurrency(BCD)^;
    Result := True;
end;

function TOCIDataSet.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
    PCurrency(BCD)^ := Curr;
    Result := True;
end;
{$ENDIF}

procedure TOCIDataSet.ResetLOBs;
var
    prevState: TDataSetState;
    buff: PChar;
begin
    CheckActive;
    prevState := SetTempState(dsOldValue);
    try
        if GetActiveRecBuf(buff) then
            FOCICursor.ResetLobs(POCIBookmark(buff));
    finally
        RestoreState(prevState);
    end;
end;

{$IFDEF OCI_D5}
procedure TOCIDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
begin
    if Field.DataType = ftBCD then
        PCurrency(Dest)^ := PCurrency(Source)^
    else
        inherited DataConvert(Field, Source, Dest, ToNative);
end;
{$ENDIF}

function TOCIDataSet.GetFieldIsChanged(AIndex: Integer): Boolean;
var
    fld: TField;
begin
    fld := Fields[AIndex];
    Result := VarNearEqual(fld.NewValue, fld.OldValue)
end;

// -------------------------------------------------------------
// Navigation / Editing

procedure TOCIDataSet.CheckAllowedOperations(AOperation: TOCISQLKind);
var
    i, j: Integer;
    operNames, s: String;
begin
    if not (AOperation in AllowedOperations) then begin
        operNames := SOCIOperations;
        i := Integer(AOperation);
        j := 1;
        while (i >= 0) and (j >= Length(operNames)) do
            s := ExtractFieldName(operNames, j);
        OCIDBErrorFmt(msgOCICantExecOper, [s], Self);
    end;
end;

procedure TOCIDataSet.InternalFirst;
begin
    Database.StartWait;
    try
        FOCICursor.First(True);
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.InternalLast;
begin
    Database.StartWait;
    try
        FOCICursor.Last(True);
    finally
        Database.EndWait;
    end;
end;

function TOCIDataSet.IsSequenced: Boolean;
begin
    Result := not FFilters.AnyActive and Active and
        (FOCICursor.StatusFilter = [rsFetched, rsNewInserted, rsNewUpdated,
                                    rsNewInsApplyed, rsNewUpdApplyed]);
end;

function TOCIDataSet.GetCanModify: Boolean;
begin
    Result :=
        Active and
        (Assigned(FOnUpdateRecord) or (ActualUpdateObject <> nil)) and
        not ReadOnly;
end;

procedure TOCIDataSet.SetReadOnly(const Value: Boolean);
begin
    CheckInactive;
    FReadOnly := Value;
end;

procedure TOCIDataSet.InternalCancel;
begin
    Database.StartWait;
    try
        if State = dsEdit then
            UpdateProcessor(skUnLock, [soImmediate])
        else if State = dsInsert then
            UpdateProcessor(skUnLock, [soImmediate, soNoRecord]);
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
    CheckAllowedOperations(skInsert);
    Database.StartWait;
    try
        FOCICursor.InsertRecord(POCIBookmark(Buffer));
        UpdateProcessor(skUnLock, [soImmediate, soSave]);
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.DoBeforeInsert;
begin
    inherited DoBeforeInsert;
    CheckAllowedOperations(skInsert);
    Database.StartWait;
    try
        UpdateProcessor(skLock, [soImmediate, soNoRecord]);
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.InternalEdit;
begin
    CheckAllowedOperations(skUpdate);
    Database.StartWait;
    try
        FOCICursor.CopyRecord(POCIBookmark(ActiveBuffer + BookmarkSize),
            POCIBookmark(ActiveBuffer), False);
        PRecInfo(ActiveBuffer + FRecInfoOfs).Bounded := rbEdited;
        UpdateProcessor(skLock, [soImmediate]);
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.InternalPost;
begin
    Database.StartWait;
    try
        if State = dsEdit then begin
            FOCICursor.ModifyRecord(nil, POCIBookmark(ActiveBuffer));
            UpdateProcessor(skUnLock, [soImmediate, soSave]);
        end
        else begin
            FOCICursor.InsertRecord(POCIBookmark(ActiveBuffer));
            UpdateProcessor(skUnLock, [soImmediate, soSave]);
        end;
    finally
        Database.EndWait;
    end;
end;

procedure TOCIDataSet.InternalDelete;
begin
    CheckAllowedOperations(skDelete);
    Database.StartWait;
    try
        try
            UpdateProcessor(skLock, [soImmediate]);
            FOCICursor.DeleteRecord(nil);
            UpdateProcessor(skUnLock, [soImmediate, soSave, soNoRecord]);
        except on E: EOCINativeError do
            // not data found
            if E.Errors[0].ErrorCode <> 100 then
                raise;
        end;
    finally
        Database.EndWait;
    end;
end;

// -----------------------------------------------------------
// Bookmark

procedure TOCIDataSet.InternalGotoBookmark(Bookmark: TBookmark);
begin
    FOCICursor.Bookmark := Bookmark;
end;

procedure TOCIDataSet.InternalSetToRecord(Buffer: PChar);
begin
    InternalGotoBookmark((Buffer + BookmarkSize));
end;

function TOCIDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
    Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

procedure TOCIDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
    PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TOCIDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
    Move((Buffer + BookmarkSize)^, Data^, BookmarkSize);
end;

procedure TOCIDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
    Move(Data^, (Buffer + BookmarkSize)^, BookmarkSize);
end;

function TOCIDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
    RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
    Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
    if Result = 2 then
        Result := FOCICursor.CompareBookmarks(Bookmark1, Bookmark2);
end;

function TOCIDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
    Result := Active;
    if Result then begin
        CursorPosChanged;
        FOCICursor.BookmarkValid(Bookmark);
    end;
end;

// -----------------------------------------------------------
// Cached updates

procedure TOCIDataSet.CheckCachedUpdates;
begin
    CheckActive;
    if not CachedUpdates then
        OCIDBError(msgOCICachUpdMBAct, Self);
end;

procedure TOCIDataSet.CheckNoUpdatesPending;
begin
    if ([csLoading, csDestroying] * ComponentState = []) and
       (CachedUpdates and UpdatesPending or
        not CachedUpdates and (State in [dsInsert, dsEdit])) then
        OCIDBError(msgOCICantChProp, Self);
end;

procedure TOCIDataSet.SetCachedUpdates(AValue: Boolean);
begin
    if FCachedUpdates <> AValue then begin
        if not AValue then
            Cancel;
        if Active then
            UpdateCachedUpdatesProp(AValue, FCachedUpdatesArea);
        FCachedUpdates := AValue;
        if Active and not AValue then
            Resync([]);
    end;
end;

procedure TOCIDataSet.SetCachedUpdatesArea(AValue: TOCICachedUpdatesArea);
begin
    if FCachedUpdatesArea <> AValue then begin
        if Active then
            UpdateCachedUpdatesProp(FCachedUpdates, AValue);
        FCachedUpdatesArea := AValue;
    end;
end;

procedure TOCIDataSet.UpdateCachedUpdatesProp(ACachedUpdates: Boolean;
    ACachedUpdatesArea: TOCICachedUpdatesArea);
begin
    FOCICursor.CachedUpdates := ACachedUpdates;
    if (ACachedUpdatesArea in [caDatabase, caDefault]) and (Database <> nil) and
       Database.CachedUpdates then
        FOCICursor.UpdatesJournal := Database.FUpdatesJournal
    else
        FOCICursor.UpdatesJournal := nil;
end;

procedure TOCIDataSet.ApplyUpdates;
begin
    CheckBrowseMode;
    CheckCachedUpdates;
    UpdateCursorPos;
    try
        FOCICursor.ApplyUpdates(nil);
    finally
        Resync([]);
    end;
end;

procedure TOCIDataSet.CancelUpdates;
begin
    Cancel;
    CheckCachedUpdates;
    UpdateCursorPos;
    try
        FOCICursor.CancelUpdates(nil);
    finally
        Resync([]);
    end;
end;

procedure TOCIDataSet.CommitUpdates;
begin
    Cancel;
    CheckCachedUpdates;
    UpdateCursorPos;
    try
        FOCICursor.CommitUpdates(nil);
    finally
        Resync([]);
    end;
end;

function TOCIDataSet.GetUpdatesPending: Boolean;
begin
    Result := Active and FOCICursor.UpdatesPending;
end;

procedure TOCIDataSet.RevertRecord;
var
    RecBuf: PChar;
begin
    Cancel;
    CheckCachedUpdates;
    UpdateCursorPos;
    if GetActiveRecBuf(RecBuf) then
        try
            FOCICursor.CancelUpdates(POCIBookmark(RecBuf + BookmarkSize));
        finally
            Resync([]);
        end;
end;

function TOCIDataSet.GetUpdateRecordTypes: TOCIUpdateRecordTypes;
const
    ociRT2dRT: array[TOCIRecordStatus] of TOCIUpdateRecordType = (
        rtUnmodified, rtUnmodified, rtUnmodified, rtInserted, rtModified,
        rtDeleted, rtUnmodified, rtUnmodified, rtUnmodified, rtUnmodified,
        rtUnmodified
    );
var
    t: TOCIRecordStatus;
begin
    Result := [];
    if Active then begin
        CheckCachedUpdates;
        for t := Low(TOCIRecordStatus) to High(TOCIRecordStatus) do
            if t in FOCICursor.StatusFilter then
                Include(Result, ociRT2dRT[t]);
    end;
end;

procedure TOCIDataSet.SetUpdateRecordTypes(const Value: TOCIUpdateRecordTypes);
const
    dRT2ociRT: array[TOCIUpdateRecordType] of TOCIRecordStatuses = (
        [rsNewUpdated], [rsNewInserted], [rsNewDeleted],
        [rsFetched, rsNewInserted, rsNewInsApplyed, rsOld]
    );
var
    v: TOCIRecordStatuses;
    t: TOCIUpdateRecordType;
begin
    CheckCachedUpdates;
    CheckBrowseMode;
    UpdateCursorPos;
    v := [];
    for t := Low(TOCIUpdateRecordType) to High(TOCIUpdateRecordType) do
        if t in Value then
            v := v + dRT2ociRT[t];
    FOCICursor.StatusFilter := v;
    Resync([]);
end;

function TOCIDataSet.UpdateStatus: TUpdateStatus;
var
    RecBuf: PChar;
begin
    if CachedUpdates then begin
        if State = dsCalcFields then
            RecBuf := CalcBuffer
        else
            RecBuf := ActiveBuffer;
        Result := PRecInfo(RecBuf + FRecInfoOfs).UpdateStatus;
    end
    else
        Result := usUnModified;
end;

procedure TOCIDataSet.InternalUpdateProcessor(ACommand: TOCISQLKind;
    var AAction: TOCIUpdateAction; AOptions: TOCISQLKindOptions);
begin
    CheckAllowedOperations(ACommand);
    FLastCommand := ACommand;
    if Assigned(FOnUpdateRecord) then
        OnUpdateRecord(Self, ACommand, AAction)
    else if ActualUpdateObject <> nil then begin
        AAction := uaApplied;
        ActualUpdateObject.Apply(ACommand, AOptions);
    end;
end;

procedure TOCIDataSet.UpdateProcessor(ACommand: TOCISQLKind; AOptions: TOCISQLKindOptions);
var
    action: TOCIUpdateAction;
begin
    action := uaFail;
    repeat
        try
            InternalUpdateProcessor(ACommand, action, AOptions);
        except
            on E: Exception do begin
                UpdateErrorHandler(nil, E, action);
                if action = uaReRaise then
                    raise;
            end;
        end;
    until action <> uaRetry;
    if action = uaAbort then
        Abort
    else if action = uaFail then
        OCIDBError(msgOCIApplyFailed, Self);
end;

procedure TOCIDataSet.UpdateRecordHandler(Sender: TOCICursor;
    var AAction: TOCIUpdateAction);
begin
    case Sender.RecordStatus of
    rsNewInserted:
        begin
            InternalUpdateProcessor(skLock, AAction, [soNoRecord]);
            InternalUpdateProcessor(skInsert, AAction, []);
        end;
    rsNewUpdated:
        begin
            InternalUpdateProcessor(skLock, AAction, []);
            InternalUpdateProcessor(skUpdate, AAction, []);
        end;
    rsNewDeleted:
        begin
            InternalUpdateProcessor(skLock, AAction, []);
            InternalUpdateProcessor(skDelete, AAction, []);
        end;
    else
        AAction := uaApplied;
    end;
end;

procedure TOCIDataSet.SetUpdateObject(const Value: TOCIUpdateObject);
begin
    CheckNoUpdatesPending;
    if ActualUpdateObject <> Value then begin
        if ActualUpdateObject <> nil then
            ActualUpdateObject.DisconnectAll;
        if (Value <> nil) and (Value.DataSet <> nil) then
            Value.DataSet.UpdateObject := nil;
        if (FUpdateObject <> nil) and (FUpdateObject.DataSet = Self) then
            FUpdateObject.FDataSet := nil;
        FUpdateObject := nil;
        if ActualUpdateObject <> Value then begin
            FUpdateObject := Value;
            if FUpdateObject <> nil then begin
                FUpdateObject.FDataSet := Self;
                FUpdateObject.FreeNotification(Self);
                FreeNotification(FUpdateObject);
            end;
        end;
    end;
end;

procedure TOCIDataSet.UpdateErrorHandler(Sender: TOCICursor; E: Exception;
    var AAction: TOCIUpdateAction);
begin
    if (E is EDatabaseError) and Assigned(FOnUpdateError) then
        try
            FOnUpdateError(Self, EDatabaseError(E), FLastCommand, AAction);
        except
            InternalHandleException;
            AAction := uaFail;
        end
    else
        AAction := uaReRaise;
end;

procedure TOCIDataSet.SetOnUpdateError(const Value: TOCIUpdateErrorEvent);
begin
    CheckNoUpdatesPending;
    FOnUpdateError := Value;
end;

procedure TOCIDataSet.SetOnUpdateRecord(const Value: TOCIUpdateRecordEvent);
begin
    CheckNoUpdatesPending;
    FOnUpdateRecord := Value;
end;

function TOCIDataSet.RecordsEqual(ADataSet: TOCIDataSet; AThisValType,
    ADataSetValType: TDataSetState): Boolean;
var
    i: Integer;
    fld: TField;

    function GetFieldVal(AField: TField; AType: TDataSetState): Variant;
    begin
        if AType = dsOldValue then
            Result := AField.OldValue
        else if AType = dsNewValue then
            Result := AField.NewValue
        else
            Result := AField.Value;
    end;

begin
    Result := True;
    for i := 0 to FieldCount - 1 do
        if (Fields[i].FieldKind = fkData) and
           not (ODataType[Fields[i].FieldNo] in otAllBlobs) then begin
            fld := ADataSet.FindField(Fields[i].FieldName);
            if (fld <> nil) and
               not VarNearEqual(GetFieldVal(Fields[i], AThisValType),
                                GetFieldVal(fld, ADataSetValType)) then begin
                Result := False;
                Break;
            end;
        end;
end;

function TOCIDataSet.GetInApplyUpdates: Boolean;
begin
    Result := (FOCICursor <> nil) and FOCICursor.InApplyUpdates;
end;

function TOCIDataSet.GetUpdateMode: TUpdateMode;
begin
    if ActualUpdateObject = nil then
        Result := upWhereKeyOnly
    else
        Result := ActualUpdateObject.UpdateMode;
end;

procedure TOCIDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
    if ActualUpdateObject <> nil then
        ActualUpdateObject.UpdateMode := Value;
end;

function TOCIDataSet.IsUMs: Boolean;
begin
    Result := (FUpdateObject = nil);
end;

function TOCIDataSet.GetRecordLocked: Boolean;
begin
    CheckActive;
    if FOCICursor.InApplyUpdates then
        Result := FOCICursor.GetLocked(nil)
    else
        Result := FOCICursor.GetLocked(POCIBookmark(ActiveBuffer + BookmarkSize));
end;

procedure TOCIDataSet.SetRecordLocked(AValue: Boolean);
begin
    CheckActive;
    if FOCICursor.InApplyUpdates then
        FOCICursor.SetLocked(nil, AValue)
    else
        FOCICursor.SetLocked(POCIBookmark(ActiveBuffer + BookmarkSize), AValue);
end;

function TOCIDataSet.GetActualUpdateObject: TOCIUpdateObject;
begin
    Result := FUpdateObject;
end;

// -----------------------------------------------------------
// Filters, lookup, locate

procedure TOCIDataSet.SetFiltered(Value: Boolean);
begin
    inherited SetFiltered(Value);
    FFilters.DefaultFilter.Active := Value;
end;

procedure TOCIDataSet.SetFilterOptions(Value: TFilterOptions);
begin
    inherited SetFilterOptions(Value);
    FFilters.DefaultFilter.Options := Value;
end;

procedure TOCIDataSet.SetFilterText(const Value: string);
begin
    inherited SetFilterText(Value);
    FFilters.DefaultFilter.Text := Value;
end;

procedure TOCIDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
    inherited SetOnFilterRecord(Value);
    FFilters.DefaultFilter.OnFilterRecord := Value;
end;

procedure TOCIDataSet.SetFilters(AValue: TOCIFilters);
begin
    if AValue = FFilters then
        Exit;
    FFilters.Assign(AValue);
end;

procedure TOCIDataSet.ExecuteFilter(ASender: TOCICursor; var AAccept: Boolean);
var
    prevState: TDataSetState;
begin
    FFilterBuffer := ASender.Bookmark;
    prevState := SetTempState(dsFilter);
    try
        if FFilterUseCalcs then
            CalculateFields(TempBuffer);
        AAccept := FFilters.Execute;
    finally
        FFilterBuffer := nil;
        RestoreState(prevState);
    end;
end;

procedure TOCIDataSet.UpdateFiltering(AFilterChanged, ACursorActive: Boolean);
var
    i: Integer;
    existsFilter: Boolean;
begin
    if csReading in ComponentState then
        Exit;
    existsFilter := False;
    for i := 0 to FFilters.Count - 1 do begin
        if FFilters[i].Active and ACursorActive then begin
            FFilters[i].Prepare;
            existsFilter := True;
        end
        else if not ACursorActive then
            FFilters[i].UnPrepare;
    end;
    if existsFilter then
        FOCICursor.OnFilterRecord := ExecuteFilter
    else begin
        FFilterUseCalcs := False;
        if FOCICursor <> nil then
            FOCICursor.OnFilterRecord := nil;
    end;
    if not FInFindRecord and AFilterChanged and ACursorActive then
        First;
end;

procedure TOCIDataSet.RegisterFilterField(AField: TField);
begin
    if AField.FieldKind in [fkCalculated, fkLookup] then
        FFilterUseCalcs := FFilterUseCalcs or True;
end;

function TOCIDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
    prevFiltered: Boolean;
begin
    CheckBrowseMode;
    Database.StartWait;
    try
        DoBeforeScroll;
        SetFound(False);
        Result := False;
        UpdateCursorPos;
        CursorPosChanged;
        FInFindRecord := True;
        try
            prevFiltered := Filtered;
            try
                Filtered := True;
                if GoForward then begin
                    if Restart then
                        FOCICursor.First(False)
                    else
                        FOCICursor.Skip(1);
                end
                else begin
                    if Restart then
                        FOCICursor.Last(False)
                    else
                        FOCICursor.Skip(-1);
                end;
            finally
                Filtered := prevFiltered;
            end;
        finally
            FInFindRecord := False;
        end;
        if not FOCICursor.EOF and not FOCICursor.BOF then begin
            Resync([rmExact, rmCenter]);
            SetFound(True);
            Result := True;
            DoAfterScroll;
        end;
    finally
        Database.EndWait;
    end;
end;

function TOCIDataSet.LocateRecord(const AKeyFields: string;
  const AKeyValues: Variant; AOptions: TLocateOptions): Boolean;
begin
    CheckBrowseMode;
    Database.StartWait;
    try
        CursorPosChanged;
        FInFindRecord := True;
        try
            with FFilters.LookupFilter do
            try
                PrepareLookupFilter(AKeyFields, AKeyValues, AOptions);
                Active := True;
                FOCICursor.First(False);
                Result := not FOCICursor.EOF and not FOCICursor.BOF;
            finally
                Active := False;
            end;
        finally
            FInFindRecord := False;
        end;
    finally
        Database.EndWait;
    end;
end;

function TOCIDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
    Result := Null;
    if LocateRecord(KeyFields, KeyValues, []) then begin
        SetTempState(dsCalcFields);
        try
            GetRecord(TempBuffer, gmCurrent, True);
            CalculateFields(TempBuffer);
            Result := FieldValues[ResultFields];
        finally
            RestoreState(dsBrowse);
        end;
    end;
end;

function TOCIDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
    Options: TLocateOptions): Boolean;
begin
    DoBeforeScroll;
    Result := LocateRecord(KeyFields, KeyValues, Options);
    if Result then begin
        Resync([rmExact, rmCenter]);
        DoAfterScroll;
    end;
end;

function TOCIDataSet.IsFS: Boolean;
begin
    Result := Filters.Count > 0;
end;

procedure TOCIDataSet.RefreshRecord;
begin
    CheckBrowseMode;
    UpdateCursorPos;
    if CachedUpdates and (UpdateStatus <> usUnModified) then
        RevertRecord;
    try
        InternalRefreshRecord;
    finally
        DataEvent(deDataSetChange, 0);
    end;
end;

procedure TOCIDataSet.InternalRefreshRecord;
begin
    UpdateProcessor(skRefresh, []);
end;

procedure TOCIDataSet.InternalRefresh;
var
    savedKeyValues: Variant;
    locateFields: String;
    db: TOCICustomDatabase;
begin
    db := Database;
    db.StartWait;
    try
        locateFields := KeyFields;
        if locateFields <> '' then
            savedKeyValues := FieldValues[locateFields];
        DisableControls;
        try
            try
                Close;
                Open;
                if (locateFields = '') or
                   not LocateRecord(locateFields, savedKeyValues, []) then
                    FOCICursor.First(False);
            except
                on E: Exception do
                    if not (E is EAbort) then
                        raise;
            end;
        finally
            EnableControls;
        end;
    finally
        db.EndWait;
    end;
end;

// -----------------------------------------------------------
// Transactions

procedure TOCIDataSet.SetTransactionManager(const Value: TOCITransactionManager);
begin
    if FTransactionManager <> Value then begin
        Disconnect;
        FTransactionManager := Value;
        if FTransactionManager <> nil then
            FTransactionManager.FreeNotification(Self);
    end;
end;

procedure TOCIDataSet.AcquireTM;
begin
    if TransactionManager <> nil then begin
        TransactionManager.CheckDBNameMatch(DatabaseName);
        Database.TransactionManager := TransactionManager;
    end;
end;

procedure TOCIDataSet.ReleaseTM;
begin
end;

// -----------------------------------------------------------
// Constraints & defaults & calc fields

procedure TOCIDataSet.ClearExpressions;
var
    i: Integer;
begin
    for i := 0 to FExpressions.Count - 1 do
        FExpressions.Objects[i].Free;
    FExpressions.Clear;
    FExprKinds := [];
end;

procedure TOCIDataSet.PrepareExpressions;
var
    i: Integer;

    procedure CompileExpression(const AName, AExpression, AFieldName: String;
        AKind: TOCIExprKind; const AErrMsg: String);
    var
        opt: TOCIExpressionOptions;
    begin
        with TOCIExpressionParser.Create do
        try
        try
            opt := [poExtSyntax, poNaturalLang];
            if AKind = ekFieldDef then
                Include(opt, poDefaultExpr);
            FExpressions.AddObject(AName, BuildExpression(Self, AExpression,
                [foNoPartialCompare], opt, AFieldName));
            Include(FExprKinds, AKind);
        except on E: Exception do
            OCIDBErrorFmt(AErrMsg, [E.Message], Self);
        end;
        finally
            Free;
        end;
    end;

begin
    FExprKinds := [];
    for i := 0 to Constraints.Count - 1 do
        with Constraints[i] do begin
            if CustomConstraint <> '' then
                CompileExpression(#255'CC' + IntToStr(i), CustomConstraint,
                    '', ekRecCons, msgOCIRecConstCompFail);
            if ImportedConstraint <> '' then
                try
                    CompileExpression(#255'CI' + IntToStr(i), ImportedConstraint,
                        '', ekRecCons, msgOCIRecConstCompFail);
                except
                    // If we get exception during compile, then probably
                    // we does not have all fields included into dataset
                    // In this case - simply avoid compilation
                    if not FromDictionary then
                        raise;
                end;
        end;
    for i := 0 to FieldCount - 1 do
        with Fields[i] do begin
            if CustomConstraint <> '' then
                CompileExpression(FieldName + #255'CC', CustomConstraint,
                    FieldName, ekFieldCons, msgOCIFieldConstCompFail);
            if ImportedConstraint <> '' then
                CompileExpression(FieldName + #255'CI', ImportedConstraint,
                    FieldName, ekFieldCons, msgOCIFieldConstCompFail);
            if DefaultExpression <> '' then
                CompileExpression(FieldName + #255'D', DefaultExpression,
                    FieldName, ekFieldDef, msgOCIFieldDefCompFail);
        end;
end;

procedure TOCIDataSet.EvaluteDefaults(AFieldKinds: TFieldKinds);
var
    i, j: Integer;
    exp: TOCIExpression;
begin
    if ekFieldDef in FExprKinds then
        for i := 0 to FieldCount - 1 do
            with Fields[i] do
                if (FieldKind in AFieldKinds) and (DefaultExpression <> '') then begin
                    if not FExpressions.Find(FieldName + #255'D', j) then
                        OCIDBError(msgOCIExpNotFnd, Self);
                    exp := TOCIExpression(FExpressions.Objects[j]);
                    try
                        exp.Execute;
                    except on E: Exception do
                        OCIDBErrorFmt(msgOCIFieldDefFail, [E.Message], Self);
                    end;
                end;
end;

procedure TOCIDataSet.EvaluteChecks(AField: TField);

    procedure EvaluteSingle(const AExpName, AMessage, AErrMsg: String; AImported: Boolean);
    var
        exp: TOCIExpression;
        i: Integer;
    begin
        if not FExpressions.Find(AExpName, i) then
            if AImported then
                Exit
            else
                OCIDBError(msgOCIExpNotFnd, Self);
        exp := TOCIExpression(FExpressions.Objects[i]);
        if not exp.Execute then
            OCIDBErrorFmt(AErrMsg, [AMessage], Self);
    end;

    procedure EvaluteField(AField: TField);
    begin
        with AField do begin
            if CustomConstraint <> '' then
                EvaluteSingle(FieldName + #255'CC', ConstraintErrorMessage,
                    msgOCIFieldConstFail, False);
            if ImportedConstraint <> '' then
                EvaluteSingle(FieldName + #255'CI', ConstraintErrorMessage,
                    msgOCIFieldConstFail, True);
        end;
    end;

var
    i: Integer;
begin
    if ConstraintsDisabled then
        Exit;
    if AField = nil then begin
        if ekRecCons in FExprKinds then
            for i := 0 to Constraints.Count - 1 do
                with Constraints[i] do begin
                    if CustomConstraint <> '' then
                        EvaluteSingle(#255'CC' + IntToStr(i), ErrorMessage,
                            msgOCIRecConstFail, False);
                    if ImportedConstraint <> '' then
                        EvaluteSingle(#255'CI' + IntToStr(i), ErrorMessage,
                            msgOCIRecConstFail, True);
                end;
        if ekFieldCons in FExprKinds then
            for i := 0 to FieldCount - 1 do
                EvaluteField(Fields[i]);
    end
    else if ekFieldCons in FExprKinds then
        EvaluteField(AField);
end;

procedure TOCIDataSet.DoOnNewRecord;
begin
    EvaluteDefaults([fkData]);
    inherited DoOnNewRecord;
end;

procedure TOCIDataSet.DoValidate(ASender: TField);
begin
    EvaluteChecks(ASender);
    if Assigned(FCurValidate) then
        FCurValidate(ASender);
end;

procedure TOCIDataSet.DoOnCalcFields;
begin
    EvaluteDefaults([fkCalculated]);
    inherited DoOnCalcFields;
end;

procedure TOCIDataSet.DoBeforePost;
begin
    inherited DoBeforePost;
    EvaluteChecks(nil);
end;

procedure TOCIDataSet.DisableConstraints;
begin
    Inc(FConstDisableCount);
end;

procedure TOCIDataSet.EnableConstraints;
begin
    if FConstDisableCount > 0 then
        Dec(FConstDisableCount);
end;

function TOCIDataSet.ConstraintsDisabled: Boolean;
begin
    Result := FConstDisableCount > 0;
end;

function TOCIDataSet.IsCS: Boolean;
begin
    Result := Constraints.Count > 0;
end;

function TOCIDataSet.GetSeq(const AName: String; APostSeqOnPost: Boolean): TOCISequence;
begin
    CheckActive;
    Result := FDatabase.GetSeq(AName);
    if APostSeqOnPost then
        FPostSeqList.Add(Result);
end;

procedure TOCIDataSet.DoAfterPost;
var
    i: Integer;
begin
    for i := 0 to FPostSeqList.Count - 1 do
        TOCISequence(FPostSeqList[i]).Post;
    FPostSeqList.Clear;
    inherited DoAfterPost;
end;

procedure TOCIDataSet.LoadDefaults(const AOwner, ATableName: String);
var
    fld: TField;
begin
    with TOCIDM.GetTabDefaults(DatabaseName, AOwner, ATableName) do
    try
        while not EOF do begin
            fld := Self.FindField(Fields[0].AsString);
            if fld <> nil then
                fld.DefaultExpression := Fields[1].AsString;
            Next;
        end;
    finally
        Close;
    end;
end;

procedure TOCIDataSet.LoadConstraints(const AOwner, ATableName: String);
var
    i: Integer;
begin
    i := 0;
    while i < Constraints.Count do
        if Constraints[i].FromDictionary then
{$IFDEF OCI_D5}
            Constraints.Delete(i)
{$ELSE}
            Constraints[i].Free
{$ENDIF}
        else
            Inc(i);
    with TOCIDM.GetTabConstraints(DatabaseName, AOwner, ATableName) do
    try
        while not EOF do begin
            with Self.Constraints.Add do begin
                FromDictionary := True;
                ImportedConstraint := Fields[1].AsString;
                ErrorMessage := Fields[0].AsString;
            end;
            Next;
        end;
    finally
        Close;
    end;
end;

{$IFDEF OCI_D5}
// -----------------------------------------------------------
// MIDAS 3 support

function TOCIDataSet.PSInTransaction: Boolean;
var
    Database: TOCICustomDatabase;
begin
    Result := False;
    try
        Database := TOCICustomDatabase.DatabaseByName(DatabaseName, Self);
        Result := Database.InTransaction;
    except
    end;
end;

procedure TOCIDataSet.PSStartTransaction;
begin
    IncDBUse;
    try
        Database.StartTransaction;
    except
        DecDBUse;
        raise;
    end;
end;

procedure TOCIDataSet.PSEndTransaction(Commit: Boolean);
begin
    try
        if Commit then
            Database.Commit
        else
            Database.Rollback;
    finally
        DecDBUse;
    end;
end;

function TOCIDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: Pointer = nil): Integer;
var
    Q: TOCIQuery;
begin
    IncDBUse;
    try
        if Assigned(ResultSet) then begin
            TOCIDataSet(ResultSet^) := TOCIQuery.Create(nil);
            with TOCIQuery(ResultSet^) do begin
                DatabaseName := Self.DatabaseName;
                TransactionManager := Self.TransactionManager;
                SQL.Text := ASQL;
                Params.Assign(AParams);
                Open;
                Result := -1;
            end;
        end
        else begin
            Q := TOCIQuery.Create(nil);
            with Q do
            try
                DatabaseName := Self.DatabaseName;
                TransactionManager := Self.TransactionManager;
                SQL.Text := ASQL;
                Params.Assign(AParams);
                ExecSQL;
                Result := RowsAffected;
            finally
                Free;
            end;
        end;
    finally
        DecDBUse;
    end;
end;

function TOCIDataSet.PSGetQuoteChar: string;
begin
    Result := '"';
end;

function TOCIDataSet.PSGetTableName: string;
begin
    Result := TableName;
end;

function TOCIDataSet.PSIsSQLBased: Boolean;
begin
    Result := True;
end;

function TOCIDataSet.PSIsSQLSupported: Boolean;
begin
    Result := True;
end;

procedure TOCIDataSet.PSReset;
begin
    if Active then
        try
            DisableControls;
            Close;
            Open;
        finally
            EnableControls;
        end;
end;

function TOCIDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;

    procedure AssignParams(DataSet: TDataSet; Params: TOCIParams);
    var
        I: Integer;
        Old: Boolean;
        Param: TOCIParam;
        PName: string;
        Field: TField;
        Value: Variant;
    begin
        for I := 0 to Params.Count - 1 do begin
            Param := Params[I];
            PName := Param.Name;
            Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
            if Old then
                System.Delete(PName, 1, 4);
            Field := DataSet.FindField(PName);
            if not Assigned(Field) then
                Continue;
            if Old then
                Param.AssignFieldValue(Field, Field.OldValue)
            else begin
                Value := Field.NewValue;
                if VarIsEmpty(Value) then Value := Field.OldValue;
                Param.AssignFieldValue(Field, Value);
            end;
        end;
    end;

const
    uk2sk: array [TUpdateKind] of TOCISQLKind =
        (skUpdate, skInsert, skDelete);
var
    SQL: string;
    Params: TOCIParams;
    UpdateAction: TOCIUpdateAction;
begin
    Result := False;
    if Assigned(OnUpdateRecord) then begin
        UpdateAction := uaFail;
        if Assigned(FOnUpdateRecord) then begin
            FOnUpdateRecord(Delta, uk2sk[UpdateKind], UpdateAction);
            Result := UpdateAction = uaApplied;
        end;
    end;
    if not Result and Assigned(FUpdateObject) and (FUpdateObject is TOCIUpdateSQL) then begin
        SQL := TOCIUpdateSQL(FUpdateObject).SQL[uk2sk[UpdateKind]].Text;
        if SQL <> '' then begin
            Params := TOCIParams.Create;
            try
                TOCICustomQuery.FillParams(Params, SQL);
                AssignParams(Delta, Params);
                if PSExecuteStatement(SQL, TParams(Params)) = 0 then
                    OCIDBError(msgOCIRecordDeleted, Self);
                Result := True;
            finally
                Params.Free;
            end;
        end;
    end;
end;

function TOCIDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
    PrevErr: Integer;
begin
    if Prev <> nil then
        PrevErr := Prev.ErrorCode
    else
        PrevErr := 0;
    if E is EOCINativeError then
        with EOCINativeError(E) do
            Result := EUpdateError.Create(E.Message, '', Errors[0].ErrorCode, PrevErr, E)
    else
        Result := inherited PSGetUpdateException(E, Prev);
end;
{$ENDIF}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIStatementDataSet

constructor TOCIStatementDataSet.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FRowsAffected := -1;
    FLocal := False;
{$IFDEF OCI_D4}
    ObjectView := True;
{$ENDIF}
end;

procedure TOCIStatementDataSet.Loaded;
begin
    try
        if FStreamedPrepared and not Prepared then
            Prepare;
        inherited Loaded;
    except on E: Exception do
        if not (E is EAbort) then
            if csDesigning in ComponentState then
                InternalHandleException
            else
                raise;
    end;
end;

procedure TOCIStatementDataSet.Disconnect;
begin
    inherited Disconnect;
    UnPrepare;
end;

{$IFDEF OCI_D4}
procedure TOCIStatementDataSet.DataEvent(Event: TDataEvent; Info: Integer);
var
    i: Integer;
begin
    inherited DataEvent(Event, Info);
    if (Event in [deDataSetChange, deDataSetScroll]) and
       not IsEmpty and (State = dsBrowse) then
        for i := 0 to NestedDataSets.Count - 1 do
            TOCINestedDataSet(NestedDataSets[i]).RestoreActiveState;
end;
{$ENDIF}

procedure TOCIStatementDataSet.SetPrepared(Value: Boolean);
begin
    CheckInActive;
    if (csReading in ComponentState) then
        FStreamedPrepared := Value
    else if Prepared <> Value then begin
        if Value then begin
            if Assigned(FBeforePrepare) then
                FBeforePrepare(Self);
            IncDBUse;
            if Database.DbgActive then
                Database.DbgOut(tfQPrepare, arrOnOff[Value] + ' ' + StatementText);
            try
                CreateStatement;
                FPrepared := True;
            except
                DecDBUse;
                FreeStatement;
                raise;
            end;
            if Assigned(FAfterPrepare) then
                FAfterPrepare(Self);
        end
        else begin
            if Assigned(FBeforeUnPrepare) then
                FBeforeUnPrepare(Self);
            if Database.DbgActive then
                Database.DbgOut(tfQPrepare, arrOnOff[Value] + ' ' + StatementText);
            DecDBUse;
            FPrepared := False;
            FreeStatement;
            if Assigned(FAfterUnPrepare) then
                FAfterUnPrepare(Self);
        end;
    end;
end;

procedure TOCIStatementDataSet.CreateStatement;
var
    i: Integer;
    item: TOCISelectItem;
    pVar: TOCIVariable;
    fldDef: TOCIFieldDef;
begin
    Database.StartWait;
    try
        InternalCreateStatement;
        if FCachedOCICursor <> nil then begin
            FOCICursor := FCachedOCICursor;
            FCachedOCICursor := nil;
            if not FOCICursor.Active or (OFieldDefs.Count = 0) or (FOCICursor.DefVarCount = 0) then
                OCIDBError(msgOCIStmtCantOpen, Self);
        end
        else begin
            OFieldDefs.Clear;
            FHaveBLOBs := False;
            if FCrsStatement <> nil then begin
                FOCICursor := TOCIDeadCursor.Create;
                if (FCrsStatement = FStatement) and not (Self is TOCINestedDataSet) then
                    FCrsStatement.Describe(FDatabase.FService);
                for i := 1 to FCrsStatement.PARAM_COUNT do begin
                    item := TOCISelectItem.Create(FCrsStatement);
                    try
                        item.Position := i;
                        item.Bind;
                        FDataFormat.TuneSelItem(item);
                        fldDef := OFieldDefs.Add;
                        with fldDef do begin
                            FFieldNo := item.Position;
                            FName := item.Name;
                            FDataType := item.DataType;
                            FDataSize := item.DataSize;
                            FIsNull := item.IsNull;
                            FScale := item.SCALE;
                            FPrecision := item.PRECISION;
                        end;
                    finally
                        item.Free;
                    end;
                    pVar := TOCIVariable.Create(FCrsStatement);
                    FFetchParams.TuneVar(pVar);
                    FDataFormat.TuneVar(pVar);
                    with pVar do begin
                        Position := fldDef.FieldNo;
                        DumpLabel := fldDef.Name;
                        DataType := fldDef.DataType;
                        DataSize := fldDef.DataSize;
                        VarType := odDefine;
                        FHaveBLOBs := FHaveBLOBs or (fldDef.DataType in otHBlobs);
                    end;
                    FOCICursor.AddDefVar(pVar);
                end;
            end;
        end;
    finally
        Database.EndWait;
    end;
end;

procedure TOCIStatementDataSet.FreeStatement;
var
    n: Integer;
begin
    if FStatement  <> nil then begin
        try
            BindParamDataSet(False);
            InternalStatementUnBind;
            if FOCICursor <> nil then
            try
                n := FOCICursor.DefVarCount;
                while n > 0 do begin
                    FOCICursor.DefVar[1].Free;
                    FOCICursor.RemoveDefVar(1);
                    Dec(n);
                end;
                FOCICursor.Free;
            except
                OCIDBHandleException(Self);
            end;
            FOCICursor := nil;
            InternalFreeStatement;
        except
            OCIDBHandleException(Self);
        end;
        FCrsStatement := nil;
        FStatement := nil;
    end;
end;

procedure TOCIStatementDataSet.DescribeOCICursor;
begin
    Prepare;
    if FOCICursor = nil then
        OCIDBError(msgOCIStmtCantDescribe, Self);
end;

procedure TOCIStatementDataSet.CreateOCICursor;
begin
    Database.StartWait;
    try
        Prepare;
        if FStatement = nil then
            Abort;
        if FOCICursor = nil then
            OCIDBError(msgOCIStmtCantOpen, Self);
        FRowsAffected := -1;
        AcquireTM;
        try
            if Database.DbgActive then
                Database.DbgOut(tfQExecute, StatementText);
            BindParamDataSet(False);
            InternalStatementBind(True);
            if not FOCICursor.Active then
                TOCIDeadCursor(FOCICursor).Open(FCrsStatement, FDatabase.FService,
                    FFetchParams.RowsetSize, FFetchParams.FetchExact, FUnidirectional,
                    False);
            BindParamDataSet(True);
        except
            ReleaseTM;
            raise;
        end;
    finally
        Database.EndWait;
    end;
end;

procedure TOCIStatementDataSet.DestroyOCICursor;
begin
    if ActualUpdateObject <> nil then
        ActualUpdateObject.DisconnectAll;
    BindParamDataSet(False);
    if IsCursorOpen then
        FOCICursor.Close;
end;

function TOCIStatementDataSet.ExecArray(ATimes, AOffset: Integer): Boolean;
var
    execAction: TOCIExecAction;

    procedure UpdRA;
    begin
        if FRowsAffected = -1 then
            FRowsAffected := FStatement.ROW_COUNT
        else
            Inc(FRowsAffected, FStatement.ROW_COUNT);
    end;

begin
    CheckInactive;
    IncDBUse;
    Database.StartWait;
    try
        Prepare;
        if (FOCICursor <> nil) or (FStatement = nil) then
            OCIDBError(msgOCIStmtCantExec, Self);
        FRowsAffected := -1;
        AcquireTM;
        try
            if Database.DbgActive then
                Database.DbgOut(tfQExecute, StatementText);
            BindParamDataSet(False);
            InternalStatementBind(False);
            repeat
                execAction := eaExitSuccess;
                try
                    FStatement.Execute(FDatabase.FService, ATimes, AOffset, False,
                        FDatabase.AutoCommit, False);
                    UpdRA;
                except
                    on E: EOCINativeError do begin
                        UpdRA;
                        execAction := eaReRaise;
                        AOffset := AOffset + Integer(FStatement.ROW_COUNT);
                        if Assigned(OnExecError) then
                            OnExecError(Self, ATimes, AOffset, E, execAction);
                        case execAction of
                        eaApplied:
                            begin
                                Inc(AOffset);
                                Inc(FRowsAffected);
                            end;
                        eaSkip:
                            Inc(AOffset);
                        eaRetry:
                            ;
                        eaReRaise:
                            raise;
                        eaAbort:
                            Abort;
                        eaFail:
                            OCIDBError(msgStmtExecFailed, Self);
                        end;
                    end;
                end;
                if AOffset >= ATimes then
                    execAction := eaExitSuccess;
            until execAction in [eaExitSuccess, eaExitFailure];
            if execAction = eaExitSuccess then
                BindParamDataSet(True);
        finally
            ReleaseTM;
        end;
        Inc(FUsageCount);
    finally
        Database.EndWait;
        DecDBUse;
    end;
    Result := (execAction = eaExitSuccess);
end;

{$IFDEF OCI_D4}
function TOCIStatementDataSet.Exec(ATimes: Integer = 0; AOffset: Integer = 0): Boolean;
begin
    Result := ExecArray(ATimes, AOffset);
end;
{$ELSE}
function TOCIStatementDataSet.Exec: Boolean;
begin
    Result := ExecArray(0, 0);
end;
{$ENDIF}

procedure TOCIStatementDataSet.Prepare;
begin
    Prepared := True;
end;

procedure TOCIStatementDataSet.UnPrepare;
begin
    Prepared := False;
end;

function TOCIStatementDataSet.GetStatementType: TOCIStatementType;
const
    st: array[OCI_STMT_SELECT..OCI_STMT_DECLARE] of TOCIStatementType =
        (stSelect, stUpdate, stDelete, stInsert, stCreate,
         stDrop, stAlter, stBegin, stDeclare);
begin
    if Prepared then
        Result := st[FStatement.Stmt_Type]
    else
        Result := stUnknown;
end;

procedure TOCIStatementDataSet.InternalCreateStatement;
begin
end;

procedure TOCIStatementDataSet.InternalFreeStatement;
begin
end;

procedure TOCIStatementDataSet.InternalStatementBind(AForOpen: Boolean);
begin
end;

procedure TOCIStatementDataSet.InternalStatementUnBind;
begin
end;

function TOCIStatementDataSet.GetStatementText: String;
begin
    Result := '';
end;

procedure TOCIStatementDataSet.BindParamDataSet(ABindTo: Boolean);
begin
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIQueryDataLink

{$IFNDEF OCI_D4}
function TDetailDataLink.GetDetailDataSet: TDataSet;
begin
  Result := nil;
end;
{$ENDIF}

constructor TOCIQueryDataLink.Create(AQuery: TOCICustomQuery);
begin
    inherited Create;
    FQuery := AQuery;
end;

procedure TOCIQueryDataLink.ActiveChanged;
begin
    if FQuery.Active then
        FQuery.RefreshParams;
end;

function TOCIQueryDataLink.GetDetailDataSet: TDataSet;
begin
    Result := FQuery;
end;

procedure TOCIQueryDataLink.RecordChanged(Field: TField);
begin
    if (Field = nil) and FQuery.Active then
        FQuery.RefreshParams;
end;

procedure TOCIQueryDataLink.CheckBrowseMode;
begin
    if FQuery.Active then
        FQuery.CheckBrowseMode;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCICustomQuery

constructor TOCICustomQuery.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSQL := TOCIStringList.Create;
    TOCIStringList(FSQL).OnChange := DoSQLChange;
    FParams := TOCIParams.Create(Self);
    FMacros := TOCIMacros.Create(Self);
    FParamCheck := True;
    FMacroCheck := True;
    FDefaultUpdateSQL := TOCIUpdateSQL.Create(nil);
    FDefaultUpdateSQL.FDataSet := Self;
    FDataLink := TOCIQueryDataLink.Create(Self);
    FParamDataSet := TStringList.Create;
    FParamDataSet.Sorted := True;
    FSQLFromTable := '';
    FSQLOrderByPos := -1;
end;

destructor TOCICustomQuery.Destroy;
begin
    Destroying;
    Disconnect;
    FSQL.Free;
    FSQL := nil;
    FMacros.Free;
    FMacros := nil;
    FParams.Free;
    FParams := nil;
    FDefaultUpdateSQL.Free;
    FDefaultUpdateSQL := nil;
    FDataLink.Free;
    FDataLink := nil;
{$IFDEF OCI_D5}
    if FProviderParams <> nil then begin
        FProviderParams.Free;
        FProviderParams := nil;
    end;
{$ENDIF}        
    FParamDataSet.Free;
    FParamDataSet := nil;
    inherited Destroy;
end;

procedure TOCICustomQuery.DefineProperties(Filer: TFiler);
begin
    inherited DefineProperties(Filer);
    Filer.DefineProperty('ParamData', ReadParamData, nil, False);
    Filer.DefineProperty('RequestLive', ReadBool, nil, False);
end;

procedure TOCICustomQuery.ReadParamData(Reader: TReader);
begin
    Reader.ReadValue;
    Reader.ReadCollection(FParams);
end;

procedure TOCICustomQuery.ReadBool(Reader: TReader);
begin
    Reader.ReadBoolean;
end;

procedure TOCICustomQuery.Loaded;
begin
    try
        if (FSQL.Count > 0) and (FExpandedSQL = '') then
            DoSQLChange(nil);
        inherited Loaded;
    except
        if csDesigning in ComponentState then
            InternalHandleException
        else
            raise;
    end;
end;

function TOCICustomQuery.GetActualUpdateObject: TOCIUpdateObject;
begin
    if UpdateObject <> nil then
        Result := UpdateObject
    else
        Result := FDefaultUpdateSQL;
end;

procedure TOCICustomQuery.SetSQL(AValue: TStrings);
begin
    if AValue = FSQL then
        Exit;
    FSQL.Assign(AValue);
end;

class function TOCICustomQuery.ParseSQL(AParams: TOciParams; AMacrosUpd, AMacrosRead: TOciMacros;
    const AQuery: String; ACreateParams, ACreateMacros, ABindByName: Boolean; var AFrom: String;
    var AOrderByPos: Integer): String;
var
    InComment1, InComment2, InStr, InName: Boolean;
    pCh, pStCh, pStParam: PChar;
    s, subst: String;
    i, delta, expLen: Integer;
    IsParam: Boolean;
    m: TOCIMacro;
    FixedMacro, SQLMacro: Char;
    IsFixedMacro: Boolean;
    IsQuoted: Boolean;
    pFromEndCh, pFromStartCh: PChar;
    braceLevel: Integer;
    fromStartDelta, fromEndDelta: Integer;
    pOrderByStartCh: PChar;
    orderByDelta: Integer;
begin
    AFrom := '';
    AOrderByPos := -1;
    Result := AQuery;
    if Result = '' then
        Exit;
    pStCh := PChar(AQuery);
    pCh := pStCh - 1;
    InComment1 := False;
    InComment2 := False;
    InStr := False;
    InName := False;
    delta := 0;
    pFromEndCh := nil;
    pFromStartCh := nil;
    braceLevel := 0;
    fromStartDelta := 0;
    fromEndDelta := 0;
    pOrderByStartCh := nil;
    orderByDelta := 0;
    if (AParams <> nil) and (AParams.DataSet <> nil) and (AParams.DataSet is TOCIDataSet) then begin
        SQLMacro := TOCIDataSet(AParams.DataSet).DataFormat.SQLMacroChar;
        FixedMacro := TOCIDataSet(AParams.DataSet).DataFormat.FixedMacroChar;
    end
    else begin
        SQLMacro := '%';
        FixedMacro := '&';
    end;
    repeat
        Inc(pCh);
        case pCh^ of
        '(':
            if not InComment1 and not InComment2 and not InStr and not InName then
              Inc(braceLevel);
        ')':
            if not InComment1 and not InComment2 and not InStr and not InName then begin
              Dec(braceLevel);
              if (braceLevel = 0) and (pFromStartCh <> nil) and (pFromStartCh^ = '(') and
                 (pFromEndCh = nil) then begin
                pFromEndCh := pCh;
                fromEndDelta := delta;
              end;
            end;
        '/':
            begin
                Inc(pCh);
                if not InComment1 and not InComment2 and not InStr and not InName and
                   (pCh^ = '*') then
                    InComment1 := True
                else
                    Dec(pCh);
            end;
        '*':
            begin
                Inc(pCh);
                if not InComment2 and not InStr and not InName and
                   (pCh^ = '/') then
                    InComment1 := False
                else
                    Dec(pCh);
            end;
        '-':
            begin
                Inc(pCh);
                if not InComment1 and not InStr and not InName and
                   (pCh^ = '-') then
                    InComment2 := True
                else
                    Dec(pCh);
            end;
        '''':
            if not InComment1 and not InComment2 and not InName then
                InStr := not InStr;
        '"':
            if not InComment1 and not InComment2 and not InStr then begin
                InName := not InName;
                if not InName and (pFromStartCh <> nil) and (pFromStartCh^ = '"') and
                   (pFromEndCh = nil) then begin
                    pFromEndCh := pCh;
                    fromEndDelta := delta;
                end;
            end;
        #13, #10, ' ', #9, ',':
            begin
                if pCh^ in [#13, #10] then begin
                    if not InComment1 and InComment2 then
                        InComment2 := False;
                    if pCh^ = #13 then
                        Result[pCh - pStCh + 1 + delta] := ' ';
                end;
                if not InComment1 and not InComment2 and not InStr and not InName then
                    if (pFromStartCh <> nil) and (pFromEndCh = nil) and (braceLevel = 0) then begin
                        pFromEndCh := pCh - 1;
                        fromEndDelta := delta;
                    end;
            end;
        end;
        if not InComment1 and not InComment2 and not InStr and not InName then begin
            if ((pCh^ = 'o') or (pCh^ = 'O')) and (pOrderByStartCh = nil) and (braceLevel = 0) and
               (pCh > pStCh) and ((pCh - 1)^ in ['/', ')', ' ', '*', '"', '''', #13, #10, #7]) then begin
                if AnsiStrLIComp(pCh, PChar('order'), 5) = 0 then begin
                    Inc(pCh, 5);
                    if pCh^ in [' ', #13, #10, #7, #0] then begin
                        pOrderByStartCh := pCh - 5;
                        orderByDelta := delta;
                    end;
                end;
            end;
            if ((pCh^ = 'F') or (pCh^ = 'f')) and (pFromStartCh = nil) and (braceLevel = 0) and
               (pCh > pStCh) and ((pCh - 1)^ in ['/', ')', ' ', '*', '"', '''', #13, #10, #7]) then begin
                if AnsiStrLIComp(pCh, PChar('from'), 4) = 0 then begin
                    Inc(pCh, 4);
                    if pCh^ in [' ', '"', '(', #13, #10, #7, #0] then begin
                        while pCh^ in [' ', #13, #10, #7] do
                            Inc(pCh);
                        pFromStartCh := pCh;
                        fromStartDelta := delta;
                        Dec(pCh);
                    end;
                end;
            end;
            if (pCh^ = ':') or (pCh^ = SQLMacro) or (pCh^ = FixedMacro) then begin
                IsParam := pCh^ = ':';
                IsFixedMacro := pCh^ = FixedMacro;
                IsQuoted := False;
                Inc(pCh);
                if not IsParam or (pCh^ <> '=') then begin
                    pStParam := pCh;
                    if pCh^ = '"' then begin
                        IsQuoted := True;
                        repeat
                            Inc(pCh);
                        until (pCh^ in [#0, '"']);
                        if pCh^ = '"' then
                            Inc(pCh);
                        SetString(s, pStParam + 1, pCh - pStParam - 2);
                    end
                    else begin
                        while pCh^ in ['0'..'9', 'a'..'z', 'A'..'Z', '#', '$',
                                       '_', 'À'..'ß', 'à'..'ÿ'] do
                            Inc(pCh);
                        SetString(s, pStParam, pCh - pStParam);
                        s := AnsiUpperCase(s);
                    end;
                    if IsParam then begin
                        if ACreateParams and
                           (not ABindByName or (AParams.FindParam(s) = nil)) then
                            with TOciParam.Create(AParams) do begin
                                if (DataSet <> nil) and (csDesigning in DataSet.ComponentState) then
                                    OParamType := odIn;
                                Name := s;
                                IsCaseSensitive := IsQuoted;
                            end;
                    end
                    else begin
                        subst := '';
                        if AMacrosUpd <> nil then begin
                            m := AMacrosUpd.FindMacro(s);
                            if (m = nil) and ACreateMacros then begin
                                m := TOciMacro.Create(AMacrosUpd);
                                m.Name := s;
                                if IsFixedMacro then
                                    m.MacroType := mtString
                                else
                                    m.MacroType := mtSQL;
                            end;
                            if AMacrosUpd <> AMacrosRead then
                                m := AMacrosRead.FindMacro(s);
                            if m <> nil then
                                subst := m.SQL;
                        end;
                        i := pStParam - pStCh + delta;
                        expLen := Length(subst) - (pCh - pStParam + 1);
                        if expLen < 0 then
                            System.Delete(Result, i, -expLen)
                        else if expLen > 0 then begin
                            SetLength(s, expLen);
                            System.Insert(s, Result, i);
                        end;
                        if subst <> '' then
                            Move(PChar(subst)^, (PChar(Result) + i - 1)^, Length(subst));
                        Inc(delta, expLen);
                    end;
                    Dec(pCh);
                end;
            end;
        end;
    until (pCh^ = #0);
    if pFromStartCh <> nil then begin
        if pFromEndCh = nil then begin
            pFromEndCh := pCh - 1;
            fromEndDelta := delta;
        end;
        AFrom := Copy(Result, pFromStartCh - pStCh + 1 + fromStartDelta,
            pFromEndCh - pFromStartCh + 1 - fromStartDelta + fromEndDelta);
    end;
    if pOrderByStartCh <> nil then
        AOrderByPos := pOrderByStartCh - pStCh + 1 + orderByDelta;
end;

class procedure TOCICustomQuery.FillParams(AParams: TOCIParams; const ASQL: String);
var
    sFrom: String;
    iOrderBy: Integer;
begin
    ParseSQL(AParams, nil, nil, ASQL, True, False, True, sFrom, iOrderBy);
end;

procedure TOCICustomQuery.DoSQLChange(ASender: TObject);
var
    PList: TOciParams;
    MList: TOciMacros;
    L: Integer;
    S: String;
    pChSrc, pChDest: PChar;
begin
    if (csReading in ComponentState) or (TOCIStringList(SQL).UpdateCount > 0) then
        Exit;
    if FSelfChanging = [] then begin
        Disconnect;
        if ParamCheck or MacroCheck or (csDesigning in ComponentState) then begin
            PList := nil;
            if ParamCheck then
                PList := TOciParams.Create(Self);
            MList := nil;
            if MacroCheck then
                MList := TOciMacros.Create(Self);
            Include(FSelfChanging, ckLockParse);
            try
                FExpandedSQL := ParseSQL(PList, MList, FMacros, SQL.Text,
                    ParamCheck, MacroCheck, ParamBindMode = pbByName, FSQLFromTable,
                    FSQLOrderByPos);
                if ParamCheck then begin
                    PList.AssignValues(FParams);
                    FParams.Clear;
                    FParams.Assign(PList);
                end;
                if MacroCheck then begin
                    MList.AssignValues(FMacros);
                    FMacros.Clear;
                    FMacros.Assign(MList);
                end;
            finally
                Exclude(FSelfChanging, ckLockParse);
                if ParamCheck then
                    PList.Free;
                if MacroCheck then
                    MList.Free;
            end;
        end
        else begin
            S := SQL.Text;
            pChSrc := PChar(S);
            SetLength(FExpandedSQL, Length(S));
            pChDest := PChar(FExpandedSQL);
            L := 0;
            while pChSrc^ <> #0 do begin
                if pChSrc^ <> #13 then begin
                    pChDest^ := pChSrc^;
                    Inc(pChDest);
                    Inc(L);
                end;
                Inc(pChSrc);
            end;
            SetLength(FExpandedSQL, L);
        end;
        DataEvent(dePropertyChange, 0);
    end
    else if not (ckLockParse in FSelfChanging) then begin
        if (ckMacros in FSelfChanging) and not (ckSQL in FSelfChanging) then
            Disconnect;
        FExpandedSQL := ParseSQL(FParams, FMacros, FMacros, SQL.Text,
            False, False, ParamBindMode = pbByName, FSQLFromTable, FSQLOrderByPos);
    end;
end;

procedure TOCICustomQuery.MacrosChanged;
begin
    try
        Include(FSelfChanging, ckMacros);
        DoSQLChange(nil);
    finally
        Exclude(FSelfChanging, ckMacros);
    end;
end;

procedure TOCICustomQuery.InternalCreateStatement;
var
    i: Integer;
    stmtTp: ub2;
    crsParam: TOCIParam;
begin
    FStatement := TOCIStatement.Create(FDatabase.FEnv);
    FStatement.OwningObj := Self;
    FFetchParams.TuneStmt(FStatement);
    if (FSQL.Count > 0) and (FExpandedSQL = '') then
        DoSQLChange(nil);
    FStatement.Prepare(FExpandedSQL);
    InternalStatementBind(False);
    stmtTp := FStatement.STMT_TYPE;
    FCrsStatement := nil;
    if stmtTp in [OCI_STMT_BEGIN, OCI_STMT_DECLARE] then begin
        crsParam := nil;
        for i := 0 to FParams.Count - 1 do
            if (FParams[i].ODataType = otCursor) and (FindParamDataSet(FParams[i]) = nil) then begin
                crsParam := FParams[i];
                Break;
            end;
        if crsParam <> nil then begin
            FStatement.Execute(FDatabase.FService, 0, 0, False, FDatabase.AutoCommit, False);
            FCrsStatement := TOCIStatement.CreateUsingHandle(FDatabase.FEnv, crsParam.AsHandle);
            FCrsStatement.OwningObj := Self;
            FFetchParams.TuneStmt(FCrsStatement);
        end;
    end
    else if (stmtTp = OCI_STMT_SELECT) then
        FCrsStatement := FStatement;
end;

procedure TOCICustomQuery.InternalFreeStatement;
begin
    if (FCrsStatement <> nil) and (FCrsStatement <> FStatement) then
        FCrsStatement.Free;
    if FStatement <> nil then
        FStatement.Free;
end;

procedure TOCICustomQuery.InternalStatementBind(AForOpen: Boolean);
var
    i: Integer;
begin
    if FDataLink.DataSource <> nil then
        SetParamsFromCursor;
    FParams.Bind(FStatement, True);
    for i := 0 to FParams.Count - 1 do
        if FParams[i].ODataType = otCursor then
            FParams[i].HVariable.ResetBuffer(-1);
    if AForOpen then begin
        if (FStatement.STMT_TYPE in [OCI_STMT_BEGIN, OCI_STMT_DECLARE]) {and
           FCrsStatement.Canceled} then begin
            FStatement.Execute(FDatabase.FService, 0, 0, False, FDatabase.AutoCommit, False);
        end;
    end;
end;

procedure TOCICustomQuery.InternalStatementUnBind;
begin
    FParams.Bind(nil, False);
end;

function TOCICustomQuery.GetStatementText: String;
begin
    Result := FSQL.Text;
end;

procedure TOCICustomQuery.BindParamDataSet(ABindTo: Boolean);
var
    i: Integer;
begin
    for i := 0 to ParamDataSetCount - 1 do
        with ParamDataSets[i] do
            if not FInBindParamDataSet then begin
                FInBindParamDataSet := True;
                try
                    Active := ABindTo;
                finally
                    FInBindParamDataSet := False;
                end;
            end;
end;

function TOCICustomQuery.FindParam(const Value: string): TOCIParam;
begin
    Result := FParams.FindParam(Value);
end;

function TOCICustomQuery.GetParamCount: Word;
begin
    Result := FParams.Count;
end;

function TOCICustomQuery.ParamByName(const Value: string): TOCIParam;
begin
    Result := FParams.ParamByName(Value);
end;

procedure TOCICustomQuery.SetParams(AValue: TOciParams);
begin
    if AValue = FParams then
        Exit;
    FParams.Assign(AValue);
end;

function TOCICustomQuery.GetBindMode: TOCIParamBindMode;
begin
    Result := FParams.ParamBindMode;
end;

procedure TOCICustomQuery.SetBindMode(const Value: TOCIParamBindMode);
begin
    FParams.ParamBindMode := Value;
end;

function TOCICustomQuery.FindMacro(const Value: string): TOCIMacro;
begin
    Result := FMacros.FindMacro(Value);
end;

function TOCICustomQuery.MacroByName(const Value: string): TOCIMacro;
begin
    Result := FMacros.MacroByName(Value);
end;

function TOCICustomQuery.GetMacroCount: Word;
begin
    Result := FMacros.Count;
end;

procedure TOCICustomQuery.SetMacros(AValue: TOciMacros);
begin
    if AValue = FMacros then
        Exit;
    FMacros.Assign(AValue);
end;

function TOCICustomQuery.IsMS: Boolean;
begin
    Result := Macros.Count > 0;
end;

function TOCICustomQuery.IsPS: Boolean;
begin
    Result := Params.Count > 0;
end;

procedure TOCICustomQuery.UpdateRecordCount;
begin
    FRecordCount := 0;
    with TOCIQuery.Create(nil) do
    try
        DatabaseName := Self.DatabaseName;
        Unidirectional := True;
        SQL.BeginUpdate;
        try
            Params.Assign(Self.Params);
            Macros.Assign(Self.Macros);
            SQL.Add('select count(*) from (');
            if Self.FSQLOrderByPos <> -1 then
                SQL.Add(Copy(Self.SQL.Text, 1, Self.FSQLOrderByPos - 1))
            else
                SQL.AddStrings(Self.SQL);
            SQL.Add(')');
        finally
            SQL.EndUpdate;
        end;
        OpenExact(1);
        Self.FRecordCount := Fields[0].AsInteger;
    finally
        Free;
    end;
end;

procedure TOCICustomQuery.AddParamDataSet(const AParamName: String;
    ADataSet: TOCIStatementDataSet);
begin
    if FParamDataSet <> nil then
        if FParamDataSet.IndexOf(AParamName) = -1 then
            FParamDataSet.AddObject(AParamName, ADataSet);
end;

procedure TOCICustomQuery.RemoveParamDataSet(const AParamName: String;
    ADataSet: TOCIStatementDataSet);
var
    i: Integer;
begin
    if FParamDataSet <> nil then begin
        i := FParamDataSet.IndexOf(AParamName);
        if i <> -1 then
            FParamDataSet.Delete(i);
    end;
end;

function TOCICustomQuery.GetParamDataSetCount: Integer;
begin
    if FParamDataSet = nil then
        Result := 0
    else
        Result := FParamDataSet.Count;
end;

function TOCICustomQuery.GetParamDataSets(AIndex: Integer): TOCIStatementDataSet;
begin
    if FParamDataSet = nil then
        Result := nil
    else
        Result := TOCIStatementDataSet(FParamDataSet.Objects[AIndex]);
end;

function TOCICustomQuery.FindParamDataSet(AParam: TOCIParam): TOCIStatementDataSet;
var
    i: Integer;
begin
    if FParamDataSet = nil then begin
        Result := nil;
        Exit;
    end;
    if ParamBindMode = pbByName then
        i := FParamDataSet.IndexOf(AParam.Name)
    else
        i := FParamDataSet.IndexOf(IntToStr(AParam.Index));
    if i <> -1 then
        Result := GetParamDataSets(i)
    else
        Result := nil;
end;

// -----------------------------------------------------------
// Master-Detail support

procedure TOCICustomQuery.SetDataSource(Value: TDataSource);
begin
    if IsLinkedTo(Value) then
        DatabaseError(SCircularDataLink {$IFDEF OCI_D4}, Self {$ENDIF});
    FDataLink.DataSource := Value;
end;

function TOCICustomQuery.GetDataSource: TDataSource;
begin
    Result := FDataLink.DataSource;
end;

procedure TOCICustomQuery.SetParamsFromCursor;
var
    I: Integer;
    DataSet: TDataSet;
    fld: TField;
begin
    if FDataLink.DataSource <> nil then begin
        DataSet := FDataLink.DataSource.DataSet;
        if DataSet <> nil then begin
            for I := 0 to Params.Count - 1 do
                if not Params[i].Bound then begin
                    fld := DataSet.FindField(Params[i].Name);
                    if fld <> nil then begin
                        Params[i].OParamType := odIn;
                        Params[i].AssignFieldValue(fld, Unassigned, -1);
                        Params[i].Bound := False;
                    end;
                end;
        end;
    end;
end;

procedure TOCICustomQuery.RefreshParams;
var
    DataSet: TDataSet;
begin
    if FDataLink.DataSource <> nil then begin
        DataSet := FDataLink.DataSource.DataSet;
        if DataSet <> nil then
            if DataSet.Active and (DataSet.State <> dsSetKey) then begin
                DisableControls;
                try
                    Close;
                    Open;
                finally
                    EnableControls;
                end;
            end;
    end;
end;

procedure TOCICustomQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

    function AddFieldToList(const FieldName: string; DataSet: TDataSet;
        List: TList): Boolean;
    var
        Field: TField;
    begin
        Field := DataSet.FindField(FieldName);
        if (Field <> nil) then
            List.Add(Field);
        Result := Field <> nil;
    end;

var
    i: Integer;
begin
    MasterFields.Clear;
    DetailFields.Clear;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
        for i := 0 to Params.Count - 1 do
            if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
                AddFieldToList(Params[i].Name, Self, DetailFields);
end;

{$IFDEF OCI_D5}
// -----------------------------------------------------------
// MIDAS 3 support

function TOCICustomQuery.PSGetParams: TParams;
begin
    if FProviderParams = nil then
        FProviderParams := TParams.Create;
    FProviderParams.Assign(FParams);
    Result := FProviderParams;
end;

procedure TOCICustomQuery.PSSetParams(AParams: TParams);
begin
    if AParams.Count <> 0 then
        FParams.Assign(AParams);
    Close;
end;

procedure TOCICustomQuery.PSExecute;
begin
    Exec;
end;

procedure TOCICustomQuery.PSSetCommandText(const CommandText: string);
begin
    if CommandText <> '' then
        SQL.Text := CommandText;
end;
{$ENDIF}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIQuery

function TOCIQuery.ExecSQLArray(ATimes, AOffset: Integer): Boolean;
begin
    Result := ExecArray(ATimes, AOffset);
end;

{$IFDEF OCI_D4}
function TOCIQuery.ExecSQL(ATimes: Integer = 0; AOffset: Integer = 0): Boolean;
begin
    Result := ExecSQLArray(ATimes, AOffset);
end;
{$ELSE}
function TOCIQuery.ExecSQL: Boolean;
begin
    Result := ExecSQLArray(0, 0);
end;
{$ENDIF}

function TOCIQuery.GetTableName: String;
begin
    Result := FSQLFromTable;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIPLSQLDescriber

constructor TOCIPLSQLDescriber.CreateForProc(AOwner: TComponent;
    ADatabase: TOCICustomDatabase; ADataFormat: TOCIDataFormat;
    AOPackageName, AOProcedureName: String; AOverload: Integer);
begin
    inherited Create;
    FOwner := AOwner;
    FDatabase := ADatabase;
    FDataFormat := ADataFormat;
    FOPackageName := AOPackageName;
    FOProcedureName := AOProcedureName;
    FOverload := AOverload;
    FForProc := True;
end;

constructor TOCIPLSQLDescriber.CreateForPack(AOwner: TComponent;
    ADatabase: TOCICustomDatabase; ADataFormat: TOCIDataFormat;
    AOPackageName: String);
begin
    inherited Create;
    FOwner := AOwner;
    FDatabase := ADatabase;
    FDataFormat := ADataFormat;
    FOPackageName := AOPackageName;
    FOProcedureName := '';
    FOverload := 0;
    FForProc := False;
end;

destructor TOCIPLSQLDescriber.Destroy;
begin
    CleanUp;
    inherited Destroy;
end;

procedure TOCIPLSQLDescriber.Describe;
begin
    // build obj name
    FSPName := FOPackageName;
    if FOProcedureName <> '' then begin
        if FSPName <> '' then
            FSPName := FSPName + '.';
        FSPName := FSPName + FOProcedureName;
    end;
    FDatabase.DbgOut(tfQPrepare, 'Describe ' + FSPName);
    if FSPName = '' then
        OCIDBError(msgOCIPLSQLObjNameEmpty, FOwner);

    // Describe object and verify object type
    FDescr := TOCIDescribe.Create(FDatabase.FService);
    if FOPackageName <> '' then
        FDescr.DescribeName(FOPackageName)
    else
        FDescr.DescribeName(FOProcedureName);
    FObjType := FDescr.UB1[OCI_ATTR_PTYPE];

    if not (FObjType in [OCI_PTYPE_PKG, OCI_PTYPE_FUNC, OCI_PTYPE_PROC]) or
       (FObjType = OCI_PTYPE_PKG) and (FOProcedureName = '') or
       (FObjType = OCI_PTYPE_PKG) and (FOPackageName = '') then begin
        if FObjType = OCI_PTYPE_SYN then begin
            if FOPackageName <> '' then begin
                FOPackageName := NormOraName(FDescr.TEXT[OCI_ATTR_SCHEMA_NAME]) + '.' +
                    NormOraName(FDescr.TEXT[OCI_ATTR_NAME]);
                CleanUp;
                Describe;
            end
            else begin
                FOPackageName := '';
                FOProcedureName := NormOraName(FDescr.TEXT[OCI_ATTR_SCHEMA_NAME]) + '.' +
                    NormOraName(FDescr.TEXT[OCI_ATTR_NAME]);
                CleanUp;
                Describe;
            end;
        end
        else if FForProc then
            OCIDBErrorFmt(msgOCINotPLSQLObj, [FSPName], FOwner);
    end;
end;

procedure TOCIPLSQLDescriber.LocateProc;
begin
    // If object is package then locate procedure in it
    if FObjType = OCI_PTYPE_PKG then begin
        FNumProcs := FDescr.OpenList(OCI_ATTR_LIST_SUBPROGRAMS);
        FProcIndex := 0;
        while FProcIndex < FNumProcs do begin
            FDescr.GoToItem(FProcIndex);
            if (FDescr.Text[OCI_ATTR_NAME] = UCOraName(FOProcedureName)) and
               (FDescr.UB2[OCI_ATTR_OVERLOAD_ID] = FOverload) then
                Break;
            Inc(FProcIndex);
        end;
        if FProcIndex = FNumProcs then
            OCIDBErrorFmt(msgOCINotPackageProc, [FOProcedureName, FOPackageName], FOwner);
        FObjType := FDescr.UB1[OCI_ATTR_PTYPE];
    end
    else begin
        FNumProcs := 1;
        FProcIndex := 0;
    end;
end;

procedure TOCIPLSQLDescriber.First(var AProcName: String; var AOverload: Integer);
begin
    if FObjType = OCI_PTYPE_PKG then begin
        FNumProcs := FDescr.OpenList(OCI_ATTR_LIST_SUBPROGRAMS);
        FProcIndex := 0;
        if FProcIndex < FNumProcs then begin
            FDescr.GoToItem(FProcIndex);
            FObjType := FDescr.UB1[OCI_ATTR_PTYPE];
            AProcName := FDescr.Text[OCI_ATTR_NAME];
            AOverload := FDescr.UB2[OCI_ATTR_OVERLOAD_ID];
            if not FForProc then
                FSPName := FOPackageName + '.' + NormOraName(AProcName);
        end;
    end
    else begin
        FNumProcs := 1;
        FProcIndex := 0;
    end;
end;

function TOCIPLSQLDescriber.EOL: Boolean;
begin
    Result := FProcIndex >= FNumProcs;
end;

procedure TOCIPLSQLDescriber.Next(var AProcName: String; var AOverload: Integer);
begin
    if FProcIndex < FNumProcs then begin
        Inc(FProcIndex);
        if FProcIndex < FNumProcs then begin
            FDescr.GoToItem(FProcIndex);
            FObjType := FDescr.UB1[OCI_ATTR_PTYPE];
            AProcName := FDescr.Text[OCI_ATTR_NAME];
            AOverload := FDescr.UB2[OCI_ATTR_OVERLOAD_ID];
            if not FForProc then
                FSPName := FOPackageName + '.' + NormOraName(AProcName);
        end;
    end;
end;

procedure TOCIPLSQLDescriber.BuildSQL(ASQL: TStrings; AParams: TOCIParams);
const
    CRLF: String = #13#10;
var
    sBody, sDecl, sIn, sOut: String;
    i, j: Integer;
    numArgs: ub2;
    item: TOCISelectItem;

    function CreateParam(const APrefix, AVar: String; AItem: TOCISelectItem;
        AIsResult, AExclFromBody, AIsTable: Boolean): Boolean;
    var
        nm: String;
        vt: TOCIVarType;
        dt: TOCIVarDataType;
        sz: sb4;
        i, k: Integer;
        varTp: TOCIVarType;
        typeNm, varNm, memberName: String;
        rawTp: ub4;

        function TmpName(const APrefix: String): String;
        begin
            Result := APrefix + 'v' + IntToStr(j);
            Inc(j);
        end;

        function GetItemName(AItem: TOCISelectItem): String;
        begin
            Result := AItem.NAME;
            if (Result = '') and AIsResult and not AIsTable then
                Result := SDefParRESULT;
            Result := APrefix + Result;
        end;

        procedure BodyVar(const AName: String);
        begin
            if not AExclFromBody then
                if AIsResult then
                    sBody := AName + ' := ' + sBody
                else
                    sBody := sBody + AName;
        end;

        procedure AddParam;
        begin
            with TOciParam(AParams.Add) do begin
                OName := ':' + nm;
                OParamType := vt;
                ODataType := dt;
                if sz <> 0 then
                    ODataSize := sz;
                IsPLSQLTable := AIsTable;
{$IFDEF OCI_DEBUG}
                ShowMessage(OName + ' PT:' + IntToStr(Integer(OParamType)) +
                    ' DT:' + IntToStr(Integer(ODataType)) + ' DS:' + IntToStr(ODataSize) +
                    ' IsT:' + IntToStr(Integer(IsPLSQLTable)));
{$ENDIF}
            end;
        end;

        procedure CheckTypeSupported(ADataType: ub4);
        begin
            if AIsTable and (ADataType in [_SQLT_REC, _SQLT_BOL]) then
                OCIDBError(msgOCIBadTableType, FOwner);
        end;

    begin
        FDataFormat.TuneSelItem(AItem);
        nm := GetItemName(AItem);
        vt := AItem.VarType;
        dt := AItem.DataType;
        sz := AItem.DataSize;

        Result := True;
        rawTp := AItem.DATA_TYPE;
        CheckTypeSupported(rawTp);
        case rawTp of
        _SQLT_REC:
            begin
                nm := TmpName(APrefix);
                BodyVar(nm);
                typeNm := AItem.TYPE_NAME + '.' + AItem.SUB_NAME;
                if typeNm = '.' then
                    OCIDBError(msgOCIUnNamedRecParam, FOwner);
                sDecl := sDecl + nm + ' ' + typeNm + ';' + CRLF;
                varTp := AItem.VarType;
                varNm := GetItemName(AItem);
                k := FDescr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
                try
                    for i := 1 to k do begin
                        AItem := FDescr.SelectItem[i];
                        if AItem = nil then
                            Break;
                        memberName := nm + '.' + AItem.name;
                        try
                          if CreateParam(varNm + SDefParMembDelim, memberName, AItem, AIsResult, True, False) then begin
                              if varTp in [odIn, odInOut] then
                                  sIn := sIn + memberName + ' := :' + varNm + SDefParMembDelim + AItem.name + ';' + CRLF;
                              if varTp in [odOut, odInOut] then
                                  sOut := sOut + ':' + varNm + SDefParMembDelim + AItem.name + ' := ' + memberName + ';' + CRLF;
                          end;
                        finally
                          AItem.Free;
                        end;
                    end;
                finally
                    FDescr.CloseList;
                end;
                Result := False;
            end;
        _SQLT_TAB:
            begin
                FDescr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
                try
                    AItem := FDescr.SelectItem[1];
                    try
                        Result := CreateParam(nm, AVar, AItem, AIsResult, AExclFromBody, True);
                    finally
                        AItem.Free;
                    end;
                finally
                    FDescr.CloseList;
                end;
            end;
        _SQLT_BOL:
            begin
                if AVar = '' then begin
                    varNm := TmpName(APrefix);
                    sDecl := sDecl + varNm + ' boolean;' + CRLF;
                end
                else
                    varNm := AVar;
                if AItem.VarType in [odIn, odInOut] then
                    sIn := sIn + FDataFormat.Value2OBoolPLSQL(varNm, ':' + nm) + CRLF;
                if AItem.VarType in [odOut, odInOut] then
                    sOut := sOut + FDataFormat.OBool2ValuePLSQL(':' + nm, varNm) + CRLF;
                dt := FDataFormat.OBoolType;
                sz := FDataFormat.OBoolSize;
                BodyVar(varNm);
                AddParam;
                Result := False;
            end;
        else
            BodyVar(':' + nm);
            AddParam;
        end;
    end;

begin
    ASQL.BeginUpdate;
    try
        // Now start form a SQL and Params
        sDecl := '';
        sIn := '';
        sOut := '';
        sBody := '';
        j := 0;
        numArgs := FDescr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
        try
            // If this is function then form :Result parameter
            if FObjType = OCI_PTYPE_FUNC then begin
                item := FDescr.SelectItem[0];
                try
                    CreateParam('', '', item, True, False, False);
                finally
                    item.Free;
                end;
                Dec(numArgs);
            end;
            // Now for each parameter do ...
            sBody := sBody + FSPName;
            if numArgs > 0 then
                sBody := sBody + '(';
            for i := 1 to numArgs do begin
                if i > 1 then
                    sBody := sBody + ', ';
                item := FDescr.SelectItem[i];
                if item = nil then
                    Break;
                try
                    CreateParam('', '', item, False, False, False);
                finally
                    item.Free;
                end;
            end;
            if numArgs > 0 then
                sBody := sBody + ')';
            // Time to form anonymous PL/SQL block
            if sDecl <> '' then begin
                ASQL.Add('declare');
                ASQL.Add(sDecl);
            end;
            ASQL.Add('begin');
            if sIn <> '' then
                ASQL.Add(sIn);
            ASQL.Add(sBody + ';');
            if sOut <> '' then
                ASQL.Add(sOut);
            ASQL.Add('end;');
{$IFDEF OCI_DEBUG}
            ShowMessage(ASQL.Text);
{$ENDIF}
        finally
            FDescr.CloseList;
        end;
    finally
        ASQL.EndUpdate;
    end;
end;

procedure TOCIPLSQLDescriber.CleanUp;
begin
    if FDescr <> nil then begin
        if (FOPackageName <> '') and FDescr.IsListOpened then
            FDescr.CloseList;
        FDescr.Free;
        FDescr := nil;
    end;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIStoredProc

constructor TOCIStoredProc.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    ParamCheck := False;
    MacroCheck := False;
end;

function TOCIStoredProc.SplitName(const AProcName: String; var AOPackageName,
    AOProcedureName: String): Boolean;
var
    InName: Boolean;
    pCh, pStName: PChar;
    i: Integer;
    obj: array[0..2] of String;
begin
    InName := False;
    pStName := PChar(AProcName);
    pCh := pStName - 1;
    i := 0;
    repeat
        Inc(pCh);
        case pCh^ of
        '"':
            InName := not InName;
        '.':
            if not InName then begin
                if i > 2 then
                    OCIDBError(msgOCIPLSQLNameError, Self);
                SetString(Obj[i], pStName, pCh - pStName);
                Obj[i] := Trim(Obj[i]);
                Inc(i);
                pStName := pCh + 1;
            end;
        '@':
            if not InName then
                OCIDBError(msgOCIPLSQLNotRemote, Self);
        end;
    until (pCh^ = #0);
    if i > 2 then
        OCIDBError(msgOCIPLSQLNameError, Self);
    SetString(Obj[i], pStName, pCh - pStName);
    Obj[i] := Trim(Obj[i]);
    if i = 2 then begin
        FOPackageName := obj[0] + '.' + obj[1];
        FOProcedureName := obj[2];
        Result := False;
    end
    else if i = 1 then begin
        FOPackageName := obj[0];
        FOProcedureName := obj[1];
        Result := True;
    end
    else begin
        FOPackageName := '';
        FOProcedureName := obj[0];
        Result := False;
    end;
end;

procedure TOCIStoredProc.BuildSPQuery(const AOPackageName, AOProcedureName: String);
var
    descr: TOCIPLSQLDescriber;
begin
    descr := TOCIPLSQLDescriber.CreateForProc(Self, Database, DataFormat,
        FOPackageName, FOProcedureName, FOverload);
    try
        descr.Describe;
        descr.LocateProc;
        descr.BuildSQL(SQL, Params);
    finally
        descr.Free;
    end;
end;

procedure TOCIStoredProc.InternalCreateStatement;
var
    tmpParams: TOCIParams;
begin
    Include(FSelfChanging, ckSQL);
    try
        try
            tmpParams := TOCIParams.Create(nil);
            try
                tmpParams.Assign(Params);
                Params.Clear;
                BuildSPQuery(FOPackageName, FOProcedureName);
                Params.AssignValues(tmpParams);
                inherited InternalCreateStatement;
            finally
                tmpParams.Free;
            end;
        except
            SQL.Clear;
            Params.Clear;
            raise;
        end;
    finally
        Exclude(FSelfChanging, ckSQL);
    end;
end;

procedure TOCIStoredProc.InternalFreeStatement;
begin
    Include(FSelfChanging, ckSQL);
    try
        try
            inherited InternalFreeStatement;
        finally
            SQL.Clear;
        end;
    finally
        Exclude(FSelfChanging, ckSQL);
    end;
end;

procedure TOCIStoredProc.ProcNameChanged(AFullName: Boolean);
begin
    if not AFullName and (FOProcedureName <> '') then
        if FOPackageName <> '' then
            FStoredProcName := FOPackageName + '.' + FOProcedureName
        else
            FStoredProcName := FOProcedureName;
    if not (csReading in ComponentState) then begin
        SQL.Clear;
        if (csDesigning in ComponentState) and (FStoredProcName <> '') then
            Prepare;
    end;
end;

procedure TOCIStoredProc.ExecProc;
begin
    ExecArray(0, 0);
end;

procedure TOCIStoredProc.SetStoredProcName(const AValue: String);
var
    done, ambiguousName: Boolean;
begin
    if FStoredProcName <> AValue then
        if not (csReading in ComponentState) then begin
            CheckInactive;
            if AValue <> '' then
                ambiguousName := SplitName(AValue, FOPackageName, FOProcedureName)
            else begin
                ambiguousName := False;
                FOPackageName := '';
                FOProcedureName := '';
            end;
            FStoredProcName := AValue;
            done := True;
            repeat
                try
                    ProcNameChanged(True);
                    done := True;
                except on E: EOCINativeError do
                     // object does not exists && name may be standalone proc/func
                    if (E.Errors[0].ErrorCode = 4043) and ambiguousName then begin
                        done := False;
                        ambiguousName := False;
                        FOProcedureName := FOPackageName + '.' + FOProcedureName;
                        FOPackageName := '';
                    end
                    else
                        raise;
                end;
            until done;
        end
        else begin
            FStoredProcName := AValue;
            ProcNameChanged(True);
        end;
end;

procedure TOCIStoredProc.SetOPackName(const Value: String);
begin
    if FOPackageName <> Value then begin
        if not (csReading in ComponentState) then begin
            CheckInactive;
            FStoredProcName := '';
        end;
        FOPackageName := Value;
        FOProcedureName := '';
        ProcNameChanged(False);
    end;
end;

procedure TOCIStoredProc.SetOProcName(const Value: String);
begin
    if FOProcedureName <> Value then begin
        if not (csReading in ComponentState) then begin
            CheckInactive;
            FStoredProcName := '';
        end;
        FOProcedureName := Value;
        ProcNameChanged(False);
    end;
end;

procedure TOCIStoredProc.SetOverload(const Value: Word);
begin
    if FOverload <> Value then begin
        if not (csReading in ComponentState) then
            CheckInactive;
        FOverload := Value;
        ProcNameChanged(True);
    end;
end;

procedure TOCIStoredProc.SetParams(AValue: TOciParams);
begin
    if not Prepared and (FParams.Count = 0) then
        try
            Prepare;
            inherited SetParams(AValue);
        finally
            UnPrepare;
        end
    else
        inherited SetParams(AValue);
end;

function TOCIStoredProc.DescriptionsAvailable: Boolean;
var
    prevPrepare: Boolean;
begin
    prevPrepare := Prepared;
    try
        Prepared := True;
        Result := True;
    except
        Result := False;
    end;
    Prepared := prevPrepare;
end;

procedure TOCIStoredProc.CopyParams(Value: TOCIParams);
begin
    if not Prepared and (FParams.Count = 0) then
        try
            Prepare;
            Value.Assign(FParams);
        finally
            UnPrepare;
        end
    else
        Value.Assign(FParams);
end;

procedure TOCIStoredProc.UpdateRecordCount;
begin
    DoNavigationalCount;
end;

{$IFDEF OCI_D5}
// -----------------------------------------------------
// MIDAS 3 support

procedure TOCIStoredProc.PSSetCommandText(const CommandText: string);
begin
    if CommandText <> '' then
        StoredProcName := CommandText;
end;
{$ENDIF}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// PL/SQL generator base classes:
// - TOCIPLSQLType
// - TOCIPLSQLRecord
// - TOCIPLSQLTable
// - TOCICustomPackage

// TOCIPLSQLType

constructor TOCIPLSQLType.Create;
begin
    inherited Create;
end;

// TOCIPLSQLRecord

constructor TOCIPLSQLRecord.Create;
begin
    inherited Create;
end;

// TOCIPLSQLTable

constructor TOCIPLSQLTable.Create;
begin
    inherited Create;
end;

procedure TOCIPLSQLTable.Assign(ASource: TPersistent);
var
    par: TOCIParam;
    i: Integer;
begin
    if ASource is TOCIPLSQLTable then begin
        FValue := TOCIPLSQLTable(ASource).FValue;
        FCount := TOCIPLSQLTable(ASource).FCount;
        FFirst := TOCIPLSQLTable(ASource).FFirst;
        FLast := TOCIPLSQLTable(ASource).FLast;
    end
    else if ASource is TOCIParam then begin
{$IFDEF OCI_D4}
        Delete;
{$ELSE}
        DeleteAll;
{$ENDIF};
        par := TOCIParam(ASource);
        FValue := VarArrayCreate([1, par.ArrayLen - 1], varVariant);
        FCount := 0;
        FFirst := 0;
        FLast := 0;
        for i := 1 to par.ArrayLen - 1 do begin
            if par.IsNulls[i] then
                FValue[i] := Unassigned
            else begin
                Inc(FCount);
                if FFirst = 0 then
                    FFirst := i;
                FLast := i;
                FValue[i] := par.Values[i];
            end;
        end;
    end
    else
        inherited Assign(ASource);
end;

procedure TOCIPLSQLTable.AssignTo(ADest: TPersistent);
var
    par: TOCIParam;
    i: Integer;
begin
    if ADest is TOCIParam then begin
        par := TOCIParam(ADest);
        par.ArrayLen := Last + 1;
        par.Clear;
        for i := First to Last do
            if Exists(i) then
                par.Values[i] := FValue[i];
    end
    else
        inherited AssignTo(ADest);
end;

{$IFDEF OCI_D4}
procedure TOCIPLSQLTable.Delete;
{$ELSE}
procedure TOCIPLSQLTable.DeleteAll;
{$ENDIF}
begin
    if VarIsArray(FValue) then
        Delete(1, VarArrayHighBound(FValue, 1));
end;

procedure TOCIPLSQLTable.{$IFDEF OCI_D4} Delete {$ELSE} DeleteOne {$ENDIF}(AInd1: Integer);
begin
    if VarIsArray(FValue) then
        Delete(AInd1, AInd1);
end;

procedure TOCIPLSQLTable.Delete(AInd1, AInd2: Integer);
var
    i: Integer;
begin
    if VarIsArray(FValue) then begin
        for i := AInd1 to AInd2 do
            if Exists(i) then begin
                Dec(FCount);
                FValue[i] := Unassigned;
            end;
        if (FFirst >= AInd1) and (FFirst <= AInd2) or
           (FLast >= AInd1) and (FLast <= AInd2) then begin
            FFirst := 0;
            FLast := 0;
            for i := 1 to VarArrayHighBound(FValue, 1) do
                if Exists(i) then begin
                    if FFirst = 0 then
                        FFirst := i;
                    FLast := i;
                end;
        end;
        if FCount = 0 then
            FValue := Unassigned;
    end;
end;

function TOCIPLSQLTable.Exists(AIndex: Integer): Boolean;
begin
    Result := VarIsArray(FValue) and
        (AIndex >= 1) and (AIndex <= VarArrayHighBound(FValue, 1)) and
        not VarIsEmpty(FValue[AIndex]);
end;

function TOCIPLSQLTable.Next(AIndex: Integer): Integer;
var
    i: Integer;
begin
    Result := 0;
    for i := AIndex to VarArrayHighBound(FValue, 1) do
        if Exists(i) then begin
            Result := i;
            Break;
        end;
end;

function TOCIPLSQLTable.Prior(AIndex: Integer): Integer;
var
    i: Integer;
begin
    Result := 0;
    for i := AIndex downto 1 do
        if Exists(i) then begin
            Result := i;
            Break;
        end;
end;

function TOCIPLSQLTable.GetItems(AIndex: Integer): Variant;
begin
    if not Exists(AIndex) then
        raise EOCISystemError.Create(OCI_NO_DATA);
    Result := FValue[AIndex];
end;

procedure TOCIPLSQLTable.SetItems(AIndex: Integer; const Value: Variant);
const
    PGSize = 10;
begin
    if VarIsEmpty(Value) then begin
{$IFDEF OCI_D4}
        Delete(AIndex);
{$ELSE}
        DeleteOne(AIndex);
{$ENDIF};
        Exit;
    end;
    if not VarIsArray(FValue) then
        FValue := VarArrayCreate([1, (AIndex div PGSize + 1) * PGSize], varVariant)
    else if VarArrayHighBound(FValue, 1) < AIndex then
        VarArrayRedim(FValue, (AIndex div PGSize + 1) * PGSize);
    if not VarIsEmpty(FValue[AIndex]) then
        Dec(FCount);
    FValue[AIndex] := Value;
    Inc(FCount);
    if FFirst = 0 then
        FFirst := AIndex;
    if FLast < AIndex then
        FLast := AIndex;
end;

// TOCICustomPackage

constructor TOCICustomPackage.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FQueries := TList.Create;
end;

destructor TOCICustomPackage.Destroy;
var
    i: Integer;
begin
    for i := 0 to FQueries.Count - 1 do
        if FQueries[i] <> nil then
            TOCIQuery(FQueries[i]).Free;
    inherited Destroy;
end;

procedure TOCICustomPackage.SetDatabaseName(const AValue: String);
var
    i: Integer;
begin
    if FDatabaseName <> AValue then begin
        FDatabaseName := AValue;
        for i := 0 to FQueries.Count - 1 do
            if FQueries[i] <> nil then
                TOCIQuery(FQueries[i]).DatabaseName := AValue;
    end;
end;

function TOCICustomPackage.GetQuery(AIndex: Integer): TOCIQuery;
begin
    if FQueries[AIndex] = nil then begin
        FQueries[AIndex] := TOCIQuery.Create(nil);
        with TOCIQuery(FQueries[AIndex]) do begin
            DatabaseName := Self.FDatabaseName;
            ParamCheck := False;
            MacroCheck := False;
            Disconnectable := True;
        end;
    end;
    Result := TOCIQuery(FQueries[AIndex]);
end;

procedure TOCICustomPackage.SetProcCount(ANum: Integer);
begin
    FQueries.Count := ANum;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCISequence

constructor TOCISequence.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FQuery := TOCIQuery.Create(nil);
    FQuery.Disconnectable := True;
    FCommited := True;
    FAutoPost := False;
end;

destructor TOCISequence.Destroy;
begin
    FQuery.Free;
    FQuery := nil;
    inherited Destroy;
end;

procedure TOCISequence.BuildQuery(ANextVal: Boolean);
const
    arrOraOp: array[Boolean] of String = ('CURRVAL', 'NEXTVAL');
begin
    with FQuery do begin
        UnPrepare;
        DatabaseName := Self.DatabaseName;
        SQL.Clear;
        SQL.Add('select ' + SequenceName + '.' + arrOraOp[ANextVal] + ' from dual');
        Prepare;
    end;
end;

function TOCISequence.GetNextVal: Double;
begin
    if FCommited then begin
        if not FQuery.Prepared then
            BuildQuery(True);
        try
            FQuery.Open;
            FValue := FQuery.Fields[0].AsFloat;
            FCommited := False;
            if FAutoPost then
                Post;
        finally
            FQuery.Close;
        end;
    end;
    Result := FValue;
end;

function TOCISequence.GetCurVal: Double;
begin
    if FValue = 0 then
        Refresh;
    Result := FValue;
end;

procedure TOCISequence.Post;
begin
    FCommited := True;
end;

procedure TOCISequence.Refresh;
begin
    try
        BuildQuery(False);
        try
            FQuery.Open;
            FValue := FQuery.Fields[0].AsFloat;
        finally
            FCommited := True;
            FQuery.Close;
            FQuery.Unprepare;
        end;
    except
        GetNextVal;
    end;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCINestedDataSet

destructor TOCINestedDataSet.Destroy;
begin
    Destroying;
    Disconnect;
    ParentObjectUpdated(False);
    inherited Destroy;
end;

procedure TOCINestedDataSet.CheckParentDataSet;
begin
    if (FParamDataSet <> nil) and (FParamDataSet.FindParam(FParamName) = nil)
{$IFDEF OCI_D4}
       or (FParamDataSet = nil) and (DataSetField = nil)
{$ENDIF}
       then 
        OCIDBError(msgOCIParentDSRequired, Self);
end;

function TOCINestedDataSet.GetParentDataSet: TOCIStatementDataSet;
begin
    if FParamDataSet <> nil then
        Result := FParamDataSet
{$IFDEF OCI_D4}
    else if DataSetField <> nil then
        Result := DataSetField.DataSet as TOCIStatementDataSet
{$ENDIF}
    else
        Result := nil;
end;

procedure TOCINestedDataSet.Assign(Source: TPersistent);
begin
    if Source is TOCIParam then begin
        ParamDataSet := TOCIParam(Source).DataSet as TOCICustomQuery;
        if ParamDataSet.ParamBindMode = pbByName then
            ParamName := TOCIParam(Source).Name
        else
            ParamName := IntToStr(TOCIParam(Source).Index);
    end
{$IFDEF OCI_D4}
    else if Source is TDataSetField then
        DataSetField := TDataSetField(Source)
{$ENDIF}        
    else
        inherited Assign(Source);
end;

procedure TOCINestedDataSet.ParentObjectUpdated(ALinkTo: Boolean);
var
    pds: TOCIStatementDataSet;
begin
    if (FParamDataSet <> nil) and (FParamName <> '') then
        if ALinkTo then
            FParamDataSet.AddParamDataSet(FParamName, Self)
        else
            FParamDataSet.RemoveParamDataSet(FParamName, Self);
    pds := ParentDataSet;
    if pds <> nil then begin
        if pds.Active then
            pds.Disconnect;
        if ALinkTo then begin
            DatabaseName := pds.DatabaseName;
            TransactionManager := pds.TransactionManager;
        end;
    end;
end;

procedure TOCINestedDataSet.SetParamDataSet(const Value: TOCICustomQuery);
begin
    if FParamDataSet <> Value then begin
        if not (csLoading in ComponentState) then
            Disconnect;
{$IFDEF OCI_D4}
        DataSetField := nil;
{$ENDIF}
        ParentObjectUpdated(False);
        FParamDataSet := Value;
        ParentObjectUpdated(True);
    end;
end;

procedure TOCINestedDataSet.SetParamName(const Value: String);
begin
    if FParamName <> Value then begin
        if not (csLoading in ComponentState) then
            Disconnect;
{$IFDEF OCI_D4}
        DataSetField := nil;
{$ENDIF}
        ParentObjectUpdated(False);
        FParamName := Value;
        ParentObjectUpdated(True);
    end;
end;

{$IFDEF OCI_D4}
procedure TOCINestedDataSet.SetDataSetField(const Value: TDataSetField);
begin
    if DataSetField <> Value then begin
        if not (csLoading in ComponentState) then
            Disconnect;
        ParamDataSet := nil;
        ParamName := '';
        ParentObjectUpdated(False);
        ParentObjectUpdated(True);
        inherited SetDataSetField(Value);
    end;
end;

function TOCINestedDataSet.GetCacheCursors: Boolean;
begin
    Result := (DataSetField <> nil) and (ParentDataSet <> nil) and
              (ParentDataSet.FOCICursor <> nil) and not ParentDataSet.Unidirectional;
end;
{$ENDIF}

procedure TOCINestedDataSet.InternalCreateStatement;
{$IFDEF OCI_D4}
var
    ptr: ppOCIHandle;
    sz: ub4;
    data: TObject;
{$ENDIF}
begin
    CheckParentDataSet;
    if (FParamDataSet <> nil) then begin
        if not FInBindParamDataSet then begin
            FInBindParamDataSet := True;
            try
                ParentDataSet.Close;
                ParentDataSet.Exec;
            finally
                FInBindParamDataSet := False;
            end;
        end;
        FStatement := TOCIStatement.CreateUsingHandle(FDatabase.FEnv,
            FParamDataSet.ParamByName(FParamName).AsHandle);
        FStatement.OwningObj := Self;
    end
{$IFDEF OCI_D4}
    else if (DataSetField <> nil) then begin
        FCachedOCICursor := nil;
        FCachedOCICursorBmk := nil;
        FStatement := nil;
        if ParentDataSet.FOCICursor <> nil then begin
            if not ParentDataSet.Active and ParentDataSet.FOCICursor.Active then begin
                ParentDataSet.FOCICursor.First(False);
                if not ParentDataSet.FOCICursor.EOF then
                    FCachedOCICursorBmk := ParentDataSet.FOCICursor.Bookmark;
            end
            else if ParentDataSet.State in [dsBrowse, dsBlockRead] then begin
                ParentDataSet.UpdateCursorPos;
                if not (ParentDataSet.BOF and ParentDataSet.EOF) then
                    FCachedOCICursorBmk := ParentDataSet.FOCICursor.Bookmark;
            end;
        end;
        if FCachedOCICursorBmk <> nil then begin
            if GetCacheCursors and (ParentDataSet.State <> dsInsert) then begin
                data := ParentDataSet.FOCICursor.GetExtendedData(FCachedOCICursorBmk, DataSetField.FieldNo);
                if data <> nil then begin
                    FCachedOCICursor := data as TOCIDeadCursor;
                    FStatement := TOCIDeadCursor(FCachedOCICursor).HStatement;
                end;
            end;
            if (FCachedOCICursor = nil) and
               ParentDataSet.FOCICursor.GetDataPtr(FCachedOCICursorBmk, DataSetField.FieldNo, pUb1(ptr), sz) then begin
                FStatement := TOCIStatement.CreateUsingHandle(FDatabase.FEnv, ptr^);
                FStatement.OwningObj := Self;
            end;
        end;
    end
{$ENDIF}
    ;
    if FStatement <> nil then
        FFetchParams.TuneStmt(FStatement);
    FCrsStatement := FStatement;
end;

procedure TOCINestedDataSet.InternalStatementUnBind;
begin
    inherited InternalStatementUnBind;
{$IFDEF OCI_D4}
    CheckParentDataSet;
    if GetCacheCursors and (FCachedOCICursorBmk <> nil) then begin
        ParentDataSet.FOCICursor.SetExtendedData(FCachedOCICursorBmk, DataSetField.FieldNo, FOCICursor);
        FOCICursor := nil;
        FStatement := nil;
        FCrsStatement := nil;
        FCachedOCICursorBmk := nil;
        FCachedOCICursor := nil;
    end;
{$ENDIF}
end;

procedure TOCINestedDataSet.InternalFreeStatement;
begin
    if FStatement <> nil then
        FStatement.Free;
end;

procedure TOCINestedDataSet.DestroyOCICursor;
begin
{$IFDEF OCI_D4}
    if GetCacheCursors then begin
        if ActualUpdateObject <> nil then
            ActualUpdateObject.DisconnectAll;
        BindParamDataSet(False);
    end
    else
{$ENDIF}    
        inherited DestroyOCICursor;
    Unprepare;
end;

procedure TOCINestedDataSet.UpdateRecordCount;
begin
    DoNavigationalCount;
end;

{$IFDEF OCI_D4}
procedure TOCINestedDataSet.DataEvent(Event: TDataEvent; Info: Integer);
begin
    if Event = deUpdateState then begin
        if (State in [dsBrowse, dsInactive]) and not FInParentScroll then
            FCachedActive := Active;
    end
    else if Event = deParentScroll then begin
        FInParentScroll := True;
        try
            if (ParentDataSet.IsEmpty or (ParentDataSet.State <> dsBrowse)) then
                Close
            else begin
                DisableControls;
                try
                    Close;
                    Open;
                finally
                    EnableControls;
                end;
            end;
        finally
            FInParentScroll := False;
        end;
    end;
    try
        inherited DataEvent(Event, Info);
    except on E: EOCINativeError do
        if E.Errors[0].ErrorCode = 1002 then // out of fetch sequence
            Close
        else
            raise;
    end;
end;

procedure TOCINestedDataSet.RestoreActiveState;
begin
    if FCachedActive xor Active then
        Active := FCachedActive;
end;

procedure TOCINestedDataSet.PSExecute;
begin
    TOCIStoredProc(ParamDataSet).ExecProc;
end;

function TOCINestedDataSet.PSGetParams: TParams;
Var
    Types: TParamTypes;
    PTypes: TOCIVarDataTypes;
    j, i: Integer;
begin
    if not Assigned(ParamDataSet) then begin
        Result := nil;
        Exit;
    end;

    PTypes := [otSmallInt, otInteger, otFloat, otBCD, otNumber, otString,
        otChar, otDateTime, otLong, otROWID];
    Types := [ptInput, ptInputOutput, ptOutput, ptResult];


    if ParamDataSet.Prepared then
        ParamDataSet.Prepare;

    Result := TParams.Create;
    j := 0;
    for i := 0 to ParamDataSet.Params.Count-1 do
        with ParamDataSet.Params do
            if (Items[i].ParamType in Types) and (Items[i].ODataType in PTypes) then begin
                Result.CreateParam(Items[i].DataType,Items[i].Name,Items[i].ParamType);
                Result.Items[j].Value := Items[i].Value;
                Result.Items[j].Index := j;
                Inc(j);
            end;
end;

procedure TOCINestedDataSet.PSSetCommandText(const CommandText: string);
begin
    if (CommandText <> '') and (ParamDataSet is TOCIStoredProc) then begin
        Close;
        ParamDataSet.Disconnect;
        TOCIStoredProc(ParamDataSet).StoredProcName := CommandText;
    end;
end;

procedure TOCINestedDataSet.PSSetParams(AParams: TParams);
Var
    i: Integer;
begin
    if AParams.Count = 0 then exit;
    for i := 0 to AParams.Count-1 do
        ParamDataSet.ParamByName(AParams.Items[i].Name).Value := AParams.Items[i].Value;
end;
{$ENDIF}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

constructor TOCILOBStream.CreateTemporary(ADatabase: TOCICustomDatabase;
  Mode: TBlobStreamMode; AODataType: TOCIVarDataType;
  ALobLocatorClass: TOCILobLocatorClass; ADummy: Integer = 0);
begin
    inherited Create;
    FPosition := 0;
    FMode := Mode;
    FModified := False;
    FField := nil;
    FParam := nil;
    FIndex := 0;
    FDataSet := nil;
    FOCILobLocator := ALobLocatorClass.CreateTemporary(ADatabase.FService, nc2ociValueType[AODataType], true);
    if (Mode = bmWrite) and not IsNull then
        Truncate;
end;

function TOCILOBStream.IsTemporary: Boolean;
begin
  if FOCILobLocator = nil then
    Result := false
  else
    Result := FOCILobLocator.IsTemporary;
end;

constructor TOCIFILEStream.CreateTemporary(ADatabase: TOCICustomDatabase;
  Mode: TBlobStreamMode; AODataType: TOCIVarDataType; ADummy: Integer = 0);
begin
  inherited CreateTemporary(ADatabase, Mode, AODataType, TOCIExtLocator, ADummy);
end;

constructor TOCIILOBStream.CreateTemporary(ADatabase: TOCICustomDatabase;
  Mode: TBlobStreamMode; AODataType: TOCIVarDataType; ADummy: Integer = 0);
begin
  inherited CreateTemporary(ADatabase, Mode, AODataType, TOCIIntLocator, ADummy);
end;

initialization

    FDatabases := TOCIStringList.Create;
    FDatabases.Sorted := True;
    FDatabases.Duplicates := dupError;
    FOCIDatabaseNameExpandMode := deNone;

finalization

    FDatabases.Free;
    FDatabases := nil;

end.

