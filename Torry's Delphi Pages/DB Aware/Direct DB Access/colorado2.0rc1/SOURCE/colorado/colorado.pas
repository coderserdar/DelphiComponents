{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit colorado;

interface

{$I CDEFINES.INC}

uses FmtBCD, RTLConsts, Variants, Windows, Classes, SysUtils, db, cadodb, oledbadm;

type

  TConnectionOptions=set of (coCommitRetaining, coAbortRetaining,
                             coAsyncConnect, coCommitOnClose);
  TLoginPrompt=(lpAlways, lpComplete, lpCompleteRequired, lpNever, lpSimple);
  TCursorLocation=(clNone, clClient, clServer);
  TSourceType=(stTable, stTableDirect, stStoredProc, stText, stSchema,
               stPersistedRecordset);
  TCursorType=(ctUnspecified, ctForwardOnly, ctKeyset,
               ctDynamic, ctStatic);
  TLockType=(ltUnspecified, ltReadOnly, ltPessimistic,
             ltOptimistic, ltBatchOptimistic);

  TSupportsOptions=set of (spAddNew, spApproxPosition, spBookmark,
                           spDelete, spHoldRecords, spMovePrevious,
                           spResync, spUpdate, spUpdateBatch,
                           spIndex, spSeek, spFind, spNotify);
  TParamAttributes=set of (ptParamSigned, ptParamNullable, ptParamLong);
  TConnectMode=(cmUnknown, cmRead, cmWrite, cmReadWrite,
                cmShareDenyRead, cmShareDenyWrite, cmShareExclusive,
                cmShareDenyNone, cmRecursive);
  TIsolationLevel=(ilUnspecified, ilChaos, ilReadUncommitted,
                   ilBrowse, ilCursorStability, ilReadCommitted,
                   ilRepeatableRead, ilSerializable, ilIsolated);
  TMarshalOptions=(moMarshalAll, moMarshalModifiedOnly);
  TEventStatus=(esOK,  esErrorsOccurred,
                esCantDeny,  esCancel, esUnwantedEvent);
  TDataSourceType=(dsnUser, dsnSystem);

  TLocateOrigin=(loFromBeginningForward, loFromEndBackward,
                 loFromCurrentForward, loFromCurrentBackward);

  TRecordsetOptions=set of (roQuickReport, roRequery, roResync, roAsyncExecute,
                            roAsyncFetch, roAsyncFetchNonBlocking,
                            roWithoutTrailingBlanks, roPrepared, roHugeBlobs,
                            roWideString);

  TPersistFormat = (pfADTG, pfXML);

  TAffectedRecords = (arCurrent, arFilter, arAll);

const
  colorADO_Version: string[12] = '2.0.1';
  colorADO_LegalCopyright: string = 'Copyright (c) 2000-2003 Maciej Kujalowicz';
  ftFilterNone: string[1] = '';
  ftFilterPendingRecords: string[1] = '1';
  ftFilterAffectedRecords: string[1] = '2';
  ftFilterFetchedRecords: string[1] = '3';
  ftFilterConflictingRecords: string[1] = '4';

  CursorLocationEnum: array[TCursorLocation] of DWORD =(cadodb.adUseNone,
                      cadodb.adUseClient, cadodb.adUseServer);
  SourceTypeEnum: array[TSourceType] of DWORD =(cadodb.adCmdTable,
                  cadodb.adCmdTableDirect, cadodb.adCmdStoredProc,
                  cadodb.adCmdText, cadodb.adCmdUnknown, cadodb.adCmdFile);
  _CursorTypeEnum: array[TCursorType] of DWORD =(cadodb.adOpenUnspecified,
                  cadodb.adOpenForwardOnly, cadodb.adOpenKeyset,
                  cadodb.adOpenDynamic,
                  cadodb.adOpenStatic);
  _LockTypeEnum: array[TLockType] of DWORD =(cadodb.adLockUnspecified,
                cadodb.adLockReadOnly, cadodb.adLockPessimistic,
                cadodb.adLockOptimistic, cadodb.adLockBatchOptimistic);

  FilterTypeEnum: array[0..4] of Integer =(cadodb.adFilterNone,
                  cadodb.adFilterPendingRecords,
                  cadodb.adFilterAffectedRecords,
                  cadodb.adFilterFetchedRecords,
                  cadodb.adFilterConflictingRecords);
  ConnectModeEnum: array[TConnectMode] of DWORD =(cadodb.adModeUnknown,
                   cadodb.adModeRead, cadodb.adModeWrite,
                   cadodb.adModeReadWrite, cadodb.adModeShareDenyRead,
                   cadodb.adModeShareDenyWrite,
                   cadodb.adModeShareExclusive,
                   cadodb.adModeShareDenyNone, cadodb.adModeRecursive);
  IsolationLevelEnum: array[TIsolationLevel] of DWORD =(
                      cadodb.adXactUnspecified, cadodb.adXactChaos,
                      cadodb.adXactReadUncommitted, cadodb.adXactBrowse,
                      cadodb.adXactCursorStability,
                      cadodb.adXactReadCommitted,
                      cadodb.adXactRepeatableRead,
                      cadodb.adXactSerializable, cadodb.adXactIsolated);
  MarshalOptionsEnum: array[TMarshalOptions] of DWORD =(cadodb.adMarshalAll,
                      cadodb.adMarshalModifiedOnly);
  _EventStatusEnum: array[TEventStatus] of DWORD = (cadodb.adStatusOK,
                   cadodb.adStatusErrorsOccurred, cadodb.adStatusCantDeny,
                   cadodb.adStatusCancel, cadodb.adStatusUnwantedEvent);
  DataSourceTypeEnum: array[TDataSourceType] of DWORD=(adDSNUser, adDSNSystem);

  AffectedRecordsEnum: array[TAffectedRecords] of DWORD=(adAffectCurrent,
                       adAffectGroup, adAffectAll);

  const tmSuperior: Integer = -1;
  const tmEndless: Integer = 0;
  {$IFDEF VCL40}
  const ftVariant = ftParadoxOle;
  {$ENDIF}
type

  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecNo: LongInt;
    Bookmark: OleVariant;
    BookmarkFlag: TBookmarkFlag;
  end;

  TRecord = record
    Data: Variant;
    Modified: Boolean;
  end;

  TRecData = array [0..65534] of TRecord;

{:-- TParam (reintroduced)}

  TParam = class(DB.TParam)
  private
    { Private declarations }
    FPrecision: Integer;
    FScale: Integer;
    FSize: Integer;
    FAttributes: TParamAttributes;
    procedure WriteParamValue(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Precision: Integer read FPrecision write FPrecision default 0;
    property Scale: Integer read FScale write FScale default 0;
    property Size: Integer read FSize write FSize default 0;
    property Attributes: TParamAttributes
             read FAttributes
             write FAttributes
             default [ptParamSigned, ptParamNullable];
    property Value stored FALSE;
  end;

{:-- TParams (reintoduced)}

  TParams = class(DB.TParams)
  private
    { Private declarations }
    function GetItem(Index: Integer): TParam;
    procedure SetItem(Index: Integer; Value: TParam);
  public
    constructor Create(Owner: TPersistent); overload;
    constructor Create; overload;
    procedure AddParam(Value: TParam); virtual;
    procedure RemoveParam(Value: TParam); virtual;
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TParamType): TParam; virtual;
    function ParamByName(const Value: string): TParam; virtual;
    function FindParam(const Value: string): TParam; virtual;
    property Items[Index: Integer]: TParam read GetItem write SetItem; default;
  end;

{:-- TADOErrors}

  IADOConnection = cadodb.IADOConnection;
  IADORecordset = cadodb.IADORecordset;
  IADOCommand = cadodb.IADOCommand;

  TConnection = class;

  TADOError = packed record
    Number: Integer;
    Source: WideString;
    Description: WideString;
    HelpFile: WideString;
    HelpContext: Integer;
    SQLState: WideString;
    NativeError: Integer;
  end;

  TADOErrors = class(TObject)
  private
    { Private declarations }
    FOwner: TConnection;
    function Get_Item(Index: Integer): TADOError;
  public
    function Count: Integer;
    procedure Clear;
    property Items[Index: Integer]: TADOError read Get_Item; default;
    constructor Create(AOwner: TConnection);
  end;

{:-- TConnectionEvents}

  TInfoMessageEvent = procedure(Sender: TObject; const Error: TADOError;
                      var Status: TEventStatus) of object;

  TBeginTransCompleteEvent = procedure(Sender: TObject;
                             TransactionLevel: Integer;
                             const Error: TADOError;
                             var Status: TEventStatus) of object;
  TCommitTransCompleteEvent = procedure(Sender: TObject;
                              const Error: TADOError;
                              var Status: TEventStatus) of object;

  TRollbackTransCompleteEvent = procedure(Sender: TObject;
                                const Error: TADOError;
                                var Status: TEventStatus) of object;

  TBeforeExecuteEvent = procedure(Sender: TObject; var Source: WideString;
                        var CursorType: TCursorType;
                        var LockType: TLockType; var Options: TRecordsetOptions;
                        var Status: TEventStatus; const Command: IADOCommand;
                        const Recordset: IADORecordset) of object;

  TExecuteCompleteEvent = procedure(Sender: TObject; RecordsAffected: Integer;
                          const Error: TADOError;
                          var Status: TEventStatus; const Command: IADOCommand;
                          const Recordset: IADORecordset) of object;

  TBeforeConnectEvent = procedure(Sender: TObject;
                        var ConnectionString: WideString;
                        var UserID: WideString;
                        var Password: WideString;
                        var Options: TConnectionOptions;
                        var Status: TEventStatus) of object;

  TConnectCompleteEvent = procedure(Sender: TObject; const Error: TADOError;
                          var Status: TEventStatus) of object;

  TDisconnectEvent = procedure(Sender: TObject;
                     var Status: TEventStatus) of object;

  TConnectProgressEvent = procedure(Sender: TObject; Progress: Integer;
                          MaxProgress: Integer;
                          var Status: TEventStatus) of object;
                          
  TLowResourceEvent = procedure(Sender: TObject) of object;

  TConnectionEvents = class(TInterfacedObject, IADOConnectionEventsVt)
  private
    { Private declarations }
    FOwner: TConnection;
    FInternalEvents: Boolean;
  protected
    function InfoMessage(const pError: IADOError;
             var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function BeginTransComplete(TransactionLevel: Integer;
             const pError: IADOError; var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function CommitTransComplete(const pError: IADOError;
             var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function RollbackTransComplete(const pError: IADOError;
             var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function WillExecute(var Source: WideString;
             var CursorType: cadodb.CursorTypeEnum;
             var LockType: cadodb.LockTypeEnum; var Options: Integer;
             var adStatus: cadodb.EventStatusEnum; const pCommand: IADOCommand;
             const pRecordset: IADORecordset;
             const pConnection: IADOConnection): HResult; stdcall;
    function ExecuteComplete(RecordsAffected: Integer;
             const pError: IADOError; var adStatus: cadodb.EventStatusEnum;
             const pCommand: IADOCommand; const pRecordset: IADORecordset;
             const pConnection: IADOConnection): HResult; stdcall;
    function WillConnect(var ConnectionString: WideString;
             var UserID: WideString; var Password: WideString;
             var Options: Integer; var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function ConnectComplete(const pError: IADOError;
             var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
    function Disconnect(var adStatus: cadodb.EventStatusEnum;
             const pConnection: IADOConnection): HResult; stdcall;
  public
    constructor Create(AOwner: TConnection; InternalEvents: Boolean);
    destructor Destroy;override;
  end;

{:-- TConnection}

  TComponentsVersion = class(TPersistent)
  private
    { Private declarations }
    FADO: string;
    FComponents: string;
    FLegalCopyright: string;
    FMDAC: string;
    procedure SetValue(Index: Integer; Value: string);
  published
    property ADO: string index 0 read FADO write SetValue;
    property colorADO: string index 1 read FComponents write SetValue;
    property LegalCopyright: string index 2
             read FLegalCopyright
             write SetValue;
    property MDAC: string index 3 read FMDAC write SetValue;
  end;

  TConnection = class(TComponent)
  private
    { Private declarations }
    FClassVer: Integer;
    FStreamedCommitOnClose: Boolean;
    FActive: Boolean;
    FOptions: TConnectionOptions;
    FDefaultDatabase: WideString;
    FMode: TConnectMode;
    FIsolationLevel: TIsolationLevel;
    FConnectionString: WideString;
    FConnection : IADOConnection;
    FEventsConn: Integer;
    FInternalEventsConn: Integer;
    FErrors: TADOErrors;
    FRecordsets : TList;
    FLoginPrompt: TLoginPrompt;
    FTransactionLevel: Integer;
    FVersion: TComponentsVersion;
    FOnInfoMessage: TInfoMessageEvent;
    FOnBeginTransComplete: TBeginTransCompleteEvent;
    FOnCommitTransComplete: TCommitTransCompleteEvent;
    FOnRollbackTransComplete: TRollbackTransCompleteEvent;
    FBeforeExecute: TBeforeExecuteEvent;
    FOnExecuteComplete: TExecuteCompleteEvent;
    FOnConnectComplete: TConnectCompleteEvent;
    FBeforeConnect: TBeforeConnectEvent;
    FOnDisconnect: TDisconnectEvent;
    function GetParentConnection(Child: TComponent): TConnection;
    procedure SetParentConnection(Child: TComponent; Connection: TConnection);
    procedure SetChildActive(Child: TComponent; Active: Boolean);
    function Get_ConnectionString: WideString;
    procedure Set_ConnectionString(const pbstr: WideString);
    function Get_Active: Boolean;
    procedure Set_Active(plActive: Boolean);
    function Get_DefaultDatabase: WideString;
    procedure Set_DefaultDatabase(DefaultDatabase: WideString);
    function Get_Provider: WideString;
    procedure Set_Provider(Provider: WideString);
    function Get_Mode: TConnectMode;
    procedure Set_Mode(Mode: TConnectMode);
    function Get_IsolationLevel: TIsolationLevel;
    procedure Set_IsolationLevel(Level: TIsolationLevel);
    function Get_CommandTimeout: Integer;
    procedure Set_CommandTimeout(plTimeout: Integer);
    function Get_ConnectionTimeout: Integer;
    procedure Set_ConnectionTimeout(plTimeout: Integer);
    // compatibility with the previous version
    procedure ReadAttributes(Reader: TReader);
    procedure ReadClassVersion(Reader: TReader);
    procedure ReadCommitOnClose(Reader: TReader);
    procedure ReadCursorLocation(Reader: TReader);
    procedure SetOptions(const Value: TConnectionOptions);
    procedure WriteClassVersion(Writer: TWriter);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddReference(Component: TComponent); // internal call only
    procedure RemoveReference(Component: TComponent); // internal call only 
    procedure Close;
    procedure Open;
    function Connecting: Boolean;
    function Executing: Boolean;
    procedure CancelExecuting;
    function ShowErrors(ClearList: Boolean = TRUE): Boolean; {FALSE - no errors}
    function BeginTrans: Integer;
    procedure CommitTrans;
    procedure RollbackTrans;
    function Execute(CommandText: WideString; SourceType: TSourceType;
      AsyncExecute: Boolean = FALSE): Integer;
    procedure GetDataSourceNames(DataSourceType: TDataSourceType;
      List: TStrings);
    function GetConnectionString(DataSourceType: TDataSourceType;
      DataSourceName: WideString): WideString;
    procedure SetConnectionString(DataSourceType: TDataSourceType;
      DataSourceName: WideString; ConnectionString: WideString);
    property Errors: TADOErrors read FErrors;
    property ConnectionObject: IADOConnection read FConnection;
    property TransactionLevel: Integer read FTransactionLevel;
  published
    { Published declarations }
    property Options : TConnectionOptions
             read FOptions
             write SetOptions
             default [coCommitOnClose];
    property Provider: WideString read Get_Provider write Set_Provider;
    property ConnectionString : WideString
             read Get_ConnectionString
             write Set_ConnectionString;
    property Mode: TConnectMode read Get_Mode write Set_Mode;
    property IsolationLevel: TIsolationLevel
             read Get_IsolationLevel
             write Set_IsolationLevel;
    property CommandTimeout: Integer
             read Get_CommandTimeout
             write Set_CommandTimeout;
    property ConnectionTimeout: Integer
             read Get_ConnectionTimeout
             write Set_ConnectionTimeout;
    property LoginPrompt: TLoginPrompt read FLoginPrompt write FLoginPrompt
             default lpSimple;
    property Active : Boolean read Get_Active write Set_Active default FALSE;
    property DefaultDatabase: WideString
             read Get_DefaultDatabase
             write Set_DefaultDatabase;
    property Version: TComponentsVersion read FVersion stored FALSE;
    property OnInfoMessage: TInfoMessageEvent read FOnInfoMessage
             write FOnInfoMessage;
    property OnBeginTransComplete: TBeginTransCompleteEvent
             read FOnBeginTransComplete
             write FOnBeginTransComplete;
    property OnCommitTransComplete: TCommitTransCompleteEvent
             read FOnCommitTransComplete
             write FOnCommitTransComplete;
    property OnRollbackTransComplete: TRollbackTransCompleteEvent
             read FOnRollbackTransComplete
             write FOnRollbackTransComplete;
    property BeforeExecute: TBeforeExecuteEvent
             read FBeforeExecute
             write FBeforeExecute;
    property OnExecuteComplete: TExecuteCompleteEvent
             read FOnExecuteComplete
             write FOnExecuteComplete;
    property BeforeConnect: TBeforeConnectEvent
             read FBeforeConnect
             write FBeforeConnect;
    property OnConnectComplete: TConnectCompleteEvent
             read FOnConnectComplete
             write FOnConnectComplete;
    property OnDisconnect: TDisconnectEvent
             read FOnDisconnect
             write FOnDisconnect;
  end;


{:-- TRecordsetEvents -- currently not supported}

  TCustomRecordset = class;

  TRecordsetEvents = class(TInterfacedObject, IADORecordsetEventsVt)
  private
    { Private declarations }
    FOwner: TCustomRecordset;
  protected
    { IADORecordsetEventsVt}
    function WillChangeField(cFields: Integer; Fields: OleVariant;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function FieldChangeComplete(cFields: Integer; Fields: OleVariant;
             const pError: IADOError;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function WillChangeRecord(adReason: EventReasonEnum; cRecords: Integer;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function RecordChangeComplete(adReason: EventReasonEnum; cRecords: Integer;
             const pError: IADOError; var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function WillChangeRecordset(adReason: EventReasonEnum;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function RecordsetChangeComplete(adReason: EventReasonEnum;
             const pError: IADOError; var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function WillMove(adReason: EventReasonEnum; var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function MoveComplete(adReason: EventReasonEnum; const pError: IADOError;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function EndOfRecordset(var fMoreData: WordBool; var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function FetchProgress(Progress: Integer; MaxProgress: Integer;
             var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
    function FetchComplete(const pError: IADOError; var adStatus: Integer;
             const pRecordset: IADORecordset): HResult; stdcall;
  public
    constructor Create(AOwner: TCustomRecordset);
    destructor Destroy; override;
  end;


  TDBAsynchNotify = class(TInterfacedObject, IDBAsynchNotifyVt)
  private
    FOwner: TComponent;
  protected
    function OnLowResource(dwReserved: DWORD): HRESULT; stdcall;
    function OnProgress(const hChapter: ULONG; const eOperation: DWORD;
                        const ulProgress: ULONG; const ulProgressMax: ULONG;
                        const eAsynchPhase: DWORD;
                        const pwszStatusText: WideString): HRESULT; stdcall;
    function OnStop(hChapter: ULONG; eOperation: DWORD; hrStatus: HRESULT;
                    pwszStatusText: WideString): HRESULT; stdcall;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;


{:-- TCustomRecordset}

  TFetchProgress = procedure(Sender: TObject; Progress: Integer;
                   MaxProgress: Integer;
                   var Status: TEventStatus) of object;
  TFetchComplete = procedure(Sender: TObject; const Error: TADOError;
                   var Status: TEventStatus) of object;
  TRecordsetExecuteCompleteEvent = procedure(Sender: TObject;
                                   RecordsAffected: Integer;
                                   const Error: TADOError;
                                   const Status: TEventStatus) of object;


  TCDataSet = class(TDataSet)
  private
    { Private declarations }
    FRecInfoOfs: Integer;
    FRecordSize: Integer;
  protected
    { Protected declarations }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure ClearBuffers; override;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    {$IFDEF VCL40}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; override;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
      Decimals: Integer): Boolean; override;
    {$ENDIF}
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecordSize: Word; override;
    procedure InternalHandleException; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalInsert; override;
    procedure InternalOpen; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function InternalGetFieldData(Field: TField; RecBuffer: PChar;
      Buffer: Pointer; WithoutTrailingBlanks: Boolean): Boolean; virtual;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    {$IFDEF VCL50}
    procedure SetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean); override;
    {$ENDIF VCL50}
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; virtual;
    property RecInfoOfs: Integer read FRecInfoOfs;
  public
    { Public declarations }
    function CreateBlobStream(Field: TField;
      Mode: TBlobStreamMode): TStream; override;
    {$IFDEF VCL50}
    function GetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean): Boolean; override;
    {$ENDIF VCL50}
  end;

  TCustomRecordset = class(TCDataSet)
  private
    { Private declarations }
    FRecordset: IADORecordSet;
    FDSLocks: DWORD;
    FConnection: TConnection;
    FCommand: IADOCommand;
    FCommandTimeout: Integer;
    FEventsConn: Integer;
    FOldRecordset: IADORecordset;
    FLockType: TLockType;
    FCursorType: TCursorType;
    FCacheSize: Integer;
    FCursorLocation: TCursorLocation;
    FSQL: TStrings;
    FRequery: Boolean;
    FExecuteOnly: Boolean;
    FNoRecords: Boolean;
    FNextRecordset: Boolean;
    FWaitingForData: Boolean;
    FTableDirect: Boolean;
    FQRDataSource: TDataSource;
    FQRDataLink: TDataLink;
    FOptions: TRecordsetOptions;
    FFirstRecord: Boolean;
    FFilteredRecord: PChar;
    FOnFilterRecord: TFilterRecordEvent;
    FOldCurrentRecord: Integer;
    FLastCurrentRecord: Integer;
    FDataLink: TDataLink;
    FParams: TParams;
    FParamCheck: Boolean;
    FMarshalOptions: TMarshalOptions;
    FSourceType: TSourceType;
    FSchemaType: string;
    FTableName: string;
    FFileName: string;
    FStoredProcName: string;
    FMasterFields: string;
    FDetailFields: string;
    FOnFetchProgress: TFetchProgress;
    FOnFetchComplete: TFetchComplete;
    FOnExecuteComplete: TRecordsetExecuteCompleteEvent;
    FOnLowResource: TLowResourceEvent;
    procedure CheckLocked;
    procedure WriteChanges(Buffer: Pointer);
    function GetSource: string;
    procedure SetSource(Source: string);
    procedure SetTableDirect(Value: Boolean);
    procedure SetQuery(Value: TStrings);
    procedure SqlOnChange(Sender: TObject);
    procedure SetDS(Value: TDataSource);
    function GetDS: TDataSource;
    procedure GetFields(Fields: string; List: TStrings);
    procedure RefreshParams;
    procedure RetrieveParamsFromProvider;
    procedure SetConnection(Connection: TConnection);
    procedure SetSourceType(SourceType: TSourceType);
    procedure SetStoredProcName(StoredProcName: string);
    procedure SetTableName(TableName: string);
    procedure SetFileName(FileName: string);
    procedure SetSchemaType(SchemaType: string);
    function Get_CacheSize: Integer;
    procedure Set_CacheSize(plCacheSize: Integer);
    function Get_CursorLocation: TCursorLocation;
    procedure Set_CursorLocation(plCursorLoc: TCursorLocation);
    function Get_CursorType: TCursorType;
    procedure Set_CursorType(plCursorType: TCursorType);
    function Get_LockType: TLockType;
    procedure Set_LockType(plLockType: TLockType);
    function Get_MarshalOptions: TMarshalOptions;
    procedure Set_MarshalOptions(peMarshal: TMarshalOptions);
    function Get_MaxRecords: Integer;
    procedure Set_MaxRecords(plMaxRecords: Integer);
    function Get_Supports: TSupportsOptions;
    procedure Set_Supports(plSupports: TSupportsOptions);
    function Get_Sort: string;
    procedure Set_Sort(Criteria: string);
    procedure SetMasterFields(Fields: string);
    procedure SetDetailFields(Fields: string);
    procedure SetIndexName(IndexName: string);
    procedure SetOptions(Value: TRecordsetOptions);
    function GetIndexName: string;
    function GetParamCount: Word;
    function FilterAsInteger(Value: string): Integer;
             {-1: filter cannot be converted}
    function CheckCurrentRecord: Boolean;
    function GetCurrent_NextRecord(Current: Boolean;
      RecordBuffer: PChar): TGetResult;
    function GetPreviousRecord(RecordBuffer: PChar): TGetResult;
    function GetRecordset: IADORecordset;
    function GetRowPos: IUnknown;
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure SetActive(Value: Boolean); override;
    function GetRecNo: Longint; override;
    procedure SetRecNo(Value: Longint); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordBookmark(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult;
    function GetRecordCount: Integer; override;
    function GetCanModify: Boolean; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalPost; override;
    procedure DoAfterPost; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeScroll; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetCurrentRecord(Index: Integer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    property FilterOptions;
  public
    { Public declarations }
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function IsSequenced: Boolean; override;
    function SupportsBookmarks: Boolean;
    procedure ApplyUpdates(AffectedRecords: TAffectedRecords = arAll);
    procedure CancelUpdates(AffectedRecords: TAffectedRecords = arAll);
    procedure Execute;
    procedure ExecProc;
    procedure ExecSQL;
    procedure CancelExecuting;
    function Executing: Boolean;
    procedure Requery;
    function CloneRecordset(LockType: TLockType = ltUnspecified):
      TCustomRecordset;
    procedure SaveRecordset(FileName: WideString;
      PersistFormat: TPersistFormat = pfADTG);
    function NextRecordset: Boolean;
    function ParamByName(const Value: string): TParam;
    function CompareBookmarks(Bookmark1,
      Bookmark2: TBookmark): Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; overload; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions;
      Origin: TLocateOrigin): Boolean; reintroduce; overload;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function FindKey(const KeyValues: array of Variant): Boolean;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure ResyncRecord;
    property Connection: TConnection read FConnection write SetConnection;
    property SourceAsStringLines: TStrings read FSQL write SetQuery;
    property ParamCount: Word read GetParamCount;
    property IndexName: string read GetIndexName write SetIndexName;
    property RecordsetObject: IADORecordset read GetRecordset;
    property CommandObject: IADOCommand read FCommand;
    property ParamCheck: Boolean
             read FParamCheck
             write FParamCheck
             default True;
    property Params: TParams read FParams write FParams;
    property MasterFields: string read FMasterFields write SetMasterFields;
    property DetailFields: string read FDetailFields write SetDetailFields;
    property DataSource: TDataSource read GetDS write SetDS;
    property SourceType: TSourceType read FSourceType write SetSourceType;
    property Source: string read GetSource write SetSource stored FALSE;
  published
    { Published declarations }
    property Active default FALSE;
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
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property Filter;
    property Filtered;
    property OnFilterRecord;
    property CursorLocation: TCursorLocation
             read Get_CursorLocation
             write Set_CursorLocation;
    property CommandTimeout: Integer
             read FCommandTimeout
             write FCommandTimeout
             default -1;
    property CursorType: TCursorType read Get_CursorType write Set_CursorType;
    property LockType: TLockType read Get_LockType write Set_LockType;
    property MarshalOptions: TMarshalOptions
             read Get_MarshalOptions
             write Set_MarshalOptions;
    property MaxRecords: Integer read Get_MaxRecords write Set_MaxRecords;
    property CacheSize: Integer read Get_CacheSize write Set_CacheSize default 1;
    property Supports: TSupportsOptions
             read Get_Supports
             write Set_Supports
             stored FALSE;
    property Sort: string read Get_Sort write Set_Sort;
    property Options: TRecordsetOptions
             read FOptions
             write SetOptions
             default [];
    property OnExecuteComplete: TRecordsetExecuteCompleteEvent
             read FOnExecuteComplete
             write FOnExecuteComplete;
    property OnFetchProgress: TFetchProgress
             read FOnFetchProgress
             write FOnFetchProgress;
    property OnFetchComplete: TFetchComplete
             read FOnFetchComplete
             write FOnFetchComplete;
    property OnLowResource: TLowResourceEvent
             read FOnLowResource
             write FOnLowResource;
  end;

{:-- TRecordset}

  TRecordset = class(TCustomRecordset)
  private
    procedure ReadTableName(Reader: TReader);
    procedure WriteTableName(Writer: TWriter);
    procedure ReadFileName(Reader: TReader);
    procedure WriteFileName(Writer: TWriter);
    procedure ReadStoredProcName(Reader: TReader);
    procedure WriteStoredProcName(Writer: TWriter);
    procedure ReadSchemaType(Reader: TReader);
    procedure WriteSchemaType(Writer: TWriter);
    procedure ReadSQL(Reader: TReader);
    procedure WriteSQL(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property Connection;
    property SourceType;
    property ParamCheck;
    property Params;
    property Source;
    property MasterFields;
    property DetailFields;
    property DataSource;
    property IndexName;
  end;

{:-- TCTable}

  TCTable = class(TCustomRecordset)
  protected
    property Connection;
    property Source;
    property SourceType;
    property Params;
    property ParamCheck;
    property DataSource;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Database: TConnection read FConnection write SetConnection;
    property MasterFields;
    property DetailFields;
    property TableName: string read FTableName write SetTableName;
    property TableDirect: Boolean
             read FTableDirect
             write SetTableDirect
             default FALSE;
    property MasterSource: TDataSource read GetDS write SetDS;
    property IndexName;
  end;

{:-- TCQuery}

  TCQuery = class(TCustomRecordset)
  public
    constructor Create(AOwner: TComponent); override;
  protected
    property IndexName;
    property Source;
    property SourceType;
    property DetailFields;
    property MasterFields;
    property Connection;
  published
    property Database: TConnection read FConnection write SetConnection;
    property ParamCheck;
    property Params;
    property SQL: TStrings read FSQL write SetQuery;
    property DataSource;
  end;

{:-- TCStoredProc}

  TCStoredProc = class(TCustomRecordset)
  public
    constructor Create(AOwner: TComponent); override;
  protected
    property IndexName;
    property Source;
    property SourceType;
    property DetailFields;
    property MasterFields;
    property Connection;
  published
    property Database: TConnection read FConnection write SetConnection;
    property ParamCheck;
    property Params;
    property StoredProcName: string
             read FStoredProcName
             write SetStoredProcName;
    property DataSource;
  end;

{:-- TCSchema}

  TCSchema = class(TCustomRecordset)
  protected
    property Source;
    property SourceType;
    property Connection;
    property DataSource;
    property MasterFields;
    property DetailFields;
    property IndexName;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Database: TConnection read FConnection write SetConnection;
    property ParamCheck;
    property Params;
    property SchemaType: string read FSchemaType write SetSchemaType;
  end;

{:-- TCPersistedRecordset}

  TCPersistedRecordset = class(TCustomRecordset)
  protected
    property Source;
    property SourceType;
    property Connection;
    property ParamCheck;
    property Params;
    property DataSource;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MasterFields;
    property DetailFields;
    property FileName: string read FFileName write SetFileName;
    property MasterSource: TDataSource read GetDS write SetDS;
    property IndexName;
  end;

{:-- TCustomRecordsetDataLink}

  TCustomRecordsetDataLink = class(TDetailDataLink)
  private
    FRecordset: TCustomRecordset;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ARecordset: TCustomRecordset);
  end;

  TCBlobStream = class(TStream)
  private
    FDataSet: TCDataSet;
    FStream: IUnknown;
    FStreamIsNull: BOOL;
    FStreamSize: DWORD;
    FField: TBlobField;
    FMode: TBlobStreamMode;
    FPosition: Integer;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;
  end;

function DispGetPropValue(Disp: IDispatch; PropName: WideString;
  var PropValue: Variant): HRESULT;
function DispSetPropValue(Disp: IDispatch; PropName: WideString;
  const PropValue: Variant): HRESULT;
function DispInvokeMethod(Disp: IDispatch; MethodName: WideString): HRESULT;
function DispPropExists(Disp: IDispatch; PropName: WideString): BOOL;
function GetConnectionStringParam(ConnectionString: WideString;
  Param: string): string;
procedure SetConnectionStringParam(var ConnectionString: WideString;
  Param: string; Value: string);

implementation

uses
  TypInfo, ActiveX, Forms, ComObj, Consts, {$IFDEF VCL40}DBCommon,{$ENDIF}
  DBLogDlg, DbConsts, cschema, cconsts, cerrdlg, cmsdasc, cmdacver, cdlink,
  cfields, cblobdat;


function DispGetPropValue(Disp: IDispatch; PropName: WideString;
  var PropValue: Variant): HRESULT;
var ExcepInfo: TExcepInfo;
    Params: TDispParams;
    DispID: Integer;
    PPropName: PWideString;
begin
  PropValue := Null;
  PPropName := PWideString(PropName);
  Result := Disp.GetIDsOfNames(GUID_NULL,
    @PPropName, 1, LOCALE_SYSTEM_DEFAULT, @DispID);
  if Result <> 0 then Exit;
  ZeroMemory(@Params, SizeOf(DispParams));
  ZeroMemory(@ExcepInfo, SizeOf(ExcepInfo));
  Result := Disp.Invoke(DispID, GUID_NULL, LOCALE_SYSTEM_DEFAULT,
    DISPATCH_PROPERTYGET, Params, @PropValue,  @ExcepInfo, nil);
  if Result <> S_OK then PropValue := Null;
end;

function DispSetPropValue(Disp: IDispatch; PropName: WideString;
  const PropValue: Variant): HRESULT;
const DispIDArgs: Longint = DISPID_PROPERTYPUT;
var ExcepInfo: TExcepInfo;
    Params: TDispParams;
    DispID: Integer;
    PPropName: PWideString;
begin
  PPropName := PWideString(PropName);
  Result := Disp.GetIDsOfNames(GUID_NULL,
    @PPropName, 1, LOCALE_SYSTEM_DEFAULT, @DispID);
  if Result <> 0 then Exit;
  ZeroMemory(@Params, SizeOf(DispParams));
  Params.rgvarg := @PropValue;
  Params.rgdispidNamedArgs := @DispIDArgs;
  Params.cArgs := 1;
  Params.cNamedArgs := 1;
  ZeroMemory(@ExcepInfo, SizeOf(ExcepInfo));
  Result := Disp.Invoke(DispID, GUID_NULL, LOCALE_SYSTEM_DEFAULT,
                        DISPATCH_PROPERTYPUT, Params, nil,  @ExcepInfo, nil);
end;

function DispInvokeMethod(Disp: IDispatch; MethodName: WideString): HRESULT;
var ExcepInfo: TExcepInfo;
    Params: TDispParams;
    DispID: Integer;
    PMethodName: PWideString;
begin
  PMethodName := PWideString(MethodName);
  Result := Disp.GetIDsOfNames(GUID_NULL,
    @PMethodName, 1, LOCALE_SYSTEM_DEFAULT, @DispID);
  if Result <> 0 then Exit;
  ZeroMemory(@ExcepInfo, SizeOf(ExcepInfo));
  ZeroMemory(@Params, SizeOf(DispParams));
  Result := Disp.Invoke(DispID, GUID_NULL, LOCALE_SYSTEM_DEFAULT,
                        DISPATCH_METHOD, Params, nil,  @ExcepInfo, nil);
end;

function DispPropExists(Disp: IDispatch; PropName: WideString): BOOL;
var DispID: Integer;
    PPropName: PWideString;
    hRes: HRESULT;
begin
  PPropName := PWideString(PropName);
  hRes := Disp.GetIDsOfNames(GUID_NULL,
    @PPropName, 1, LOCALE_SYSTEM_DEFAULT, @DispID);
  if hRes <> 0
     then Result := FALSE
     else Result := TRUE;
end;

function IDBAsynchNotifyConnect(const Source: IUnknown; const IID: TIID;
  const Sink: IUnknown; var Connection: Longint): Boolean;
var pCPC : IConnectionPointContainer;
    pCP  : IConnectionPoint;
begin
  Result := FALSE;
  Connection := -1;
  if FAILED(Source.QueryInterface(IConnectionPointContainer, pCPC)) then Exit;
  if FAILED(pCPC.FindConnectionPoint(IID, pCP)) then
     begin
       pCPC := nil;
       Exit;
     end;
  if FAILED(pCP.Advise(Sink, Connection)) then
     begin
       pCP := nil;
       pCPC := nil;
       Exit;
     end;
  pCP := nil;
  pCPC := nil;
  Result := TRUE;
end;

{:-- TParam }

constructor TParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAttributes := [ptParamSigned, ptParamNullable];
end;

procedure TParam.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', nil, WriteParamValue, Bound);
end;

procedure TParam.WriteParamValue(Writer: TWriter);
var vInteger: Integer;
    vt: TValueType;
begin
  case VarType(Value) of
    varOleStr: Writer.WriteWideString(Value);
    varString: Writer.WriteString(Value);
    varByte: Writer.WriteInteger(Value);
    varSingle: Writer.WriteSingle(Value);
    varDouble: Writer.WriteFloat(Value);
    varCurrency: Writer.WriteCurrency(Value);
    varDate: Writer.WriteDate(Value);
    varBoolean: Writer.WriteBoolean(Value);
    varEmpty:
      begin
        vt := vaNil;
        Writer.Write(vt, SizeOf(TValueType));
      end;
    varInteger:
      begin
        vt := vaInt32;
        vInteger := Value;
        Writer.Write(vt, SizeOf(TValueType));
        Writer.Write(vInteger, SizeOf(LongInt));
      end;
    varSmallInt:
      begin
        vt := vaInt16;
        vInteger := Value;
        Writer.Write(vt, SizeOf(TValueType));
        Writer.Write(vInteger, SizeOf(SmallInt));
      end;
    else
      begin
        vt := vaNull;
        Writer.Write(vt, SizeOf(TValueType));
      end;
  end;
end;

{:-- TParams }

procedure TParams.AddParam(Value: TParam);
begin
  inherited AddParam(Value);
end;

constructor TParams.Create(Owner: TPersistent);
begin
  inherited Create(Owner);
  inherited Destroy;
  TCollection(Self).Create(TParam);
end;

constructor TParams.Create;
begin
  inherited Create;
  inherited Destroy;
  TCollection(Self).Create(TParam);
end;

function TParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TParam;
begin
  Result := TParam(inherited CreateParam(FldType, ParamName, ParamType));
end;

function TParams.FindParam(const Value: string): TParam;
begin
  Result := TParam(inherited FindParam(Value));
end;

function TParams.GetItem(Index: Integer): TParam;
begin
  Result := TParam(inherited Items[Index]);
end;

function TParams.ParamByName(const Value: string): TParam;
begin
  Result := TParam(inherited ParamByName(Value));
end;

procedure TParams.RemoveParam(Value: TParam);
begin
  inherited RemoveParam(Value);
end;

procedure TParams.SetItem(Index: Integer; Value: TParam);
begin
  inherited Items[Index] := Value;
end;

{:-- TADOErrors }

procedure TADOErrors.Clear;
begin
  FOwner.FConnection.Errors.Clear;
end;

function TADOErrors.Count: Integer;
begin
  Result := FOwner.FConnection.Errors.Count;
end;

constructor TADOErrors.Create(AOwner: TConnection);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TADOErrors.Get_Item(Index: Integer): TADOError;
begin
  if (Index < 0) or (Index >= FOwner.FConnection.Errors.Count) then
     raise EListError.CreateFmt(SListIndexError, [Index]);
  with Result do
       begin
         Number := FOwner.FConnection.Errors[Index].Number;
         Source := FOwner.FConnection.Errors[Index].Source;
         Description := FOwner.FConnection.Errors[Index].Description;
         HelpFile := FOwner.FConnection.Errors[Index].HelpFile;
         HelpContext := FOwner.FConnection.Errors[Index].HelpContext;
         SQLState := FOwner.FConnection.Errors[Index].SQLState;
         NativeError := FOwner.FConnection.Errors[Index].NativeError;
       end;
end;

function ADOStatusToES(adStatus: cadodb.EventStatusEnum): TEventStatus;
begin
  case adStatus of
  cadodb.adStatusOK: Result := esOK;
  cadodb.adStatusErrorsOccurred: Result := esErrorsOccurred;
  cadodb.adStatusCantDeny: Result := esCantDeny;
  cadodb.adStatusCancel: Result := esCancel;
  else
  Result := esUnwantedEvent;
  end;
end;

function ESToADOStatus(Status: TEventStatus): EventStatusEnum;
begin
  Result := _EventStatusEnum[Status];
end;

function pErrorToADOError(const pError: IADOError): TADOError;
begin
  ZeroMemory(@Result, SizeOf(TADOError));
  if pError = nil then Exit;
  with Result do
       begin
         Number := pError.Number;
         Source := pError.Source;
         Description := pError.Description;
         HelpFile := pError.HelpFile;
         HelpContext := pError.HelpContext;
         SQLState := pError.SQLState;
         NativeError := pError.NativeError;
       end;
end;

{:-- TConnectionEvents }

function TConnectionEvents.BeginTransComplete(TransactionLevel: Integer;
  const pError: IADOError; var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       if adStatus = adStatusOK then
          FOwner.FTransactionLevel := TransactionLevel;
       adStatus := adStatusOK;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             try
               if Assigned(FOwner.FOnBeginTransComplete)
                  then FOwner.FOnBeginTransComplete(FOwner, TransactionLevel,
                                 pErrorToADOError(pError), Status);
             except
               Application.HandleException(FOwner);
             end;
             adStatus := ESToADOStatus(Status);
           end;
end;

function TConnectionEvents.CommitTransComplete(const pError: IADOError;
  var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       if ((adStatus = adStatusOK) or (csDestroying in FOwner.ComponentState))
          and (FOwner.FConnection.Attributes and adXactCommitRetaining <>
               adXactCommitRetaining)
          then dec(FOwner.FTransactionLevel);
       if FOwner.FTransactionLevel < 0 then FOwner.FTransactionLevel := 0;
       adStatus := adStatusOK;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             try
               if Assigned(FOwner.FOnCommitTransComplete)
                  then FOwner.FOnCommitTransComplete(FOwner,
                                 pErrorToADOError(pError), Status);
             except
               Application.HandleException(FOwner);
             end;
             adStatus := ESToADOStatus(Status);
           end;
end;

function TConnectionEvents.ConnectComplete(const pError: IADOError;
  var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       if  (adStatus = adStatusOK)
       and (FOwner.FConnection.State and adStateOpen = adStateOpen)
       and (FOwner.FDefaultDatabase <> '')
       then
          try
            FOwner.FConnection.DefaultDatabase := FOwner.FDefaultDatabase;
          except end;
       adStatus := adStatusOK;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             if Assigned(FOwner.FOnConnectComplete) then
                try
                  FOwner.FOnConnectComplete(FOwner, pErrorToADOError(pError),
                                                    Status);
                except
                  Application.HandleException(FOwner);
                end;
             adStatus := ESToADOStatus(Status);
           end;
end;

constructor TConnectionEvents.Create(AOwner: TConnection;
  InternalEvents: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FInternalEvents := InternalEvents;
end;

destructor TConnectionEvents.Destroy;
begin
  inherited Destroy;
end;

function TConnectionEvents.Disconnect(var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  if not FInternalEvents then
     begin
       try
         FOwner.FConnection.Get_State;
       except
       end;
       Status := ADOStatusToES(adStatus);
       try
         if Assigned(FOwner.FOnDisconnect)
            then FOwner.FOnDisconnect(FOwner, Status);
       except
         Application.HandleException(FOwner);
       end;
       adStatus := ESToADOStatus(Status);
     end else adStatus := adStatusUnwantedEvent;
end;

function TConnectionEvents.ExecuteComplete(RecordsAffected: Integer;
  const pError: IADOError; var adStatus: EventStatusEnum;
  const pCommand: IADOCommand; const pRecordset: IADORecordset;
  const pConnection: IADOConnection): HResult;
var Status : TEventStatus;
    i      : Integer;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  try
    if pRecordset <> nil then pRecordset.Get_State;
  except
  end;
  try
    if pCommand <> nil then pCommand.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       if (pCommand <> nil) and (pCommand.State and adStateExecuting <> adStateExecuting) then
          begin
            for i := 0 to FOwner.FRecordsets.Count - 1 do
                if  (TComponent(FOwner.FRecordsets[i]) is TCustomRecordset)
                and (TCustomRecordset(FOwner.FRecordsets[i]).FCommand = pCommand) then
                   begin
                     if  (adStatus = adStatusOK)
                         and (TCustomRecordset(FOwner.FRecordsets[i]).FWaitingForData)
                         and (not(csDesigning in FOwner.ComponentState))
                         and (TCustomRecordset(FOwner.FRecordsets[i]).SourceType <> stSchema)
                         and (TCustomRecordset(FOwner.FRecordsets[i]).SourceType <> stTableDirect) then
                             begin
                               try
                                 TCustomRecordset(FOwner.FRecordsets[i]).FNextRecordset := TRUE;
                                 TCustomRecordset(FOwner.FRecordsets[i]).Close;
                                 TCustomRecordset(FOwner.FRecordsets[i]).Open;
                               except
                               end;
                               TCustomRecordset(FOwner.FRecordsets[i]).FNextRecordset := FALSE;
                             end;
                     try
                       if Assigned(TCustomRecordset(FOwner.FRecordsets[i]).FOnExecuteComplete)
                          then TCustomRecordset(FOwner.FRecordsets[i]).FOnExecuteComplete(Self, RecordsAffected, pErrorToADOError(pError), ADOStatusToES(adStatus));
                     except
                       Application.HandleException(FOwner.FRecordsets[i]);
                     end;
                   end;
          end;
       adStatus := adStatusOK;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             try
               if Assigned(FOwner.FOnExecuteComplete)
                  then FOwner.FOnExecuteComplete(FOwner, RecordsAffected, pErrorToADOError(pError),
                              Status, IADOCommand(pCommand), IADORecordset(pRecordset));
             except
               Application.HandleException(FOwner);
             end;
             adStatus := ESToADOStatus(Status);
           end;
end;

function TConnectionEvents.InfoMessage(const pError: IADOError;
  var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  if not FInternalEvents then
     begin
       try
         FOwner.FConnection.Get_State;
       except
       end;
       Status := ADOStatusToES(adStatus);
       try
         if Assigned(FOwner.FOnInfoMessage)
            then FOwner.FOnInfoMessage(FOwner, pErrorToADOError(pError), Status);
       except
         Application.HandleException(FOwner);
       end;
       adStatus := ESToADOStatus(Status);
     end else adStatus := adStatusUnwantedEvent;
end;

function TConnectionEvents.RollbackTransComplete(const pError: IADOError;
  var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       if ((adStatus = adStatusOK) or (csDestroying in FOwner.ComponentState))
          and (FOwner.FConnection.Attributes and adXactAbortRetaining <> adXactAbortRetaining)
          then dec(FOwner.FTransactionLevel);
       if FOwner.FTransactionLevel < 0 then FOwner.FTransactionLevel := 0;
       adStatus := adStatusOK;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             try
               if Assigned(FOwner.FOnRollbackTransComplete)
                  then FOwner.FOnRollbackTransComplete(FOwner, pErrorToADOError(pError), Status);
             except
               Application.HandleException(FOwner);
             end;
             adStatus := ESToADOStatus(Status);
           end;
end;

function TConnectionEvents.WillConnect(var ConnectionString, UserID,
  Password: WideString; var Options: Integer;
  var adStatus: EventStatusEnum;
  const pConnection: IADOConnection): HResult;
var Status      : TEventStatus;
    FDatabase,
    FUserID,
    FPassword   : string;
    FOptions    : TConnectionOptions;
    i           : Integer;
    v           : Variant;
begin
  Result := S_OK;
  try
    FOwner.FConnection.Get_State;
  except
  end;
  if FInternalEvents then
     begin
       adStatus := adStatusOK;
       if (FOwner.FLoginPrompt = lpSimple) and (not Assigned(FOwner.FBeforeConnect)) then
             begin
               FUserID := GetConnectionStringParam(ConnectionString, 'User ID');
               v := Null;
               if FUserID = ''
                  then FUserID := UserID;
               if FUserID = '' then
                  for i := 0 to FOwner.FConnection.Properties.Count - 1 do
                      if UpperCase(FOwner.FConnection.Properties [i].Name) = 'USER ID' then
                         begin
                           v := FOwner.FConnection.Properties [i].Value;
                           break;
                         end;
               if VarType(v) = varOleStr then FUserID := v;
               FDatabase := GetConnectionStringParam(ConnectionString, 'Data Source');
               if Length(FDatabase) > 25 then FDatabase := ExtractFileName(FDatabase);
               if not LoginDialog(FDatabase, FUserID, FPassword)
                  then adStatus := adStatusCancel
                  else begin
                         UserID := FUserID;
                         Password := FPassword;
                       end;
             end;
     end else
           begin
             Status := ADOStatusToES(adStatus);
             FOptions := [];
             if Options = adAsyncConnect then FOptions := FOptions + [coAsyncConnect];
             if Assigned(FOwner.FBeforeConnect) then
                begin
                  try
                    FOwner.FBeforeConnect(FOwner, ConnectionString, UserID,
                           Password, FOptions, Status);
                    adStatus := ESToADOStatus(Status);
                    if FOptions - [coAsyncConnect] <> [] then
                       DatabaseError(SInvalidOptions, FOwner);
                    if (coAsyncConnect in FOptions)
                       then Options := adAsyncConnect
                       else Options := Integer(adConnectUnspecified);
                  except
                    Application.HandleException(FOwner);
                    adStatus := adStatusCancel;
                  end;
                end;
           end;
end;

function TConnectionEvents.WillExecute(var Source: WideString;
  var CursorType: CursorTypeEnum; var LockType: LockTypeEnum;
  var Options: Integer; var adStatus: EventStatusEnum;
  const pCommand: IADOCommand; const pRecordset: IADORecordset;
  const pConnection: IADOConnection): HResult;
var Status      : TEventStatus;
    FOptions    : TRecordsetOptions;
    FLockType   : TLockType;
    FCursorType : TCursorType;
begin
  Result := S_OK;
  if not FInternalEvents then
     begin
       try
         FOwner.FConnection.Get_State;
       except
       end;
       try
         if pRecordset <> nil then pRecordset.Get_State;
       except
       end;
       try
         if pCommand <> nil then pCommand.Get_State;
       except
       end;
       Status := ADOStatusToES(adStatus);
       case LockType of
         cadodb.adLockReadOnly: FLockType := ltReadOnly;
         cadodb.adLockPessimistic: FLockType := ltPessimistic;
         cadodb.adLockOptimistic: FLockType := ltOptimistic;
         cadodb.adLockBatchOptimistic: FLockType := ltBatchOptimistic;
       else FLockType := ltUnspecified;
       end;
       case CursorType of
         cadodb.adOpenForwardOnly: FCursorType := ctForwardOnly;
         cadodb.adOpenKeyset: FCursorType := ctKeyset;
         cadodb.adOpenDynamic: FCursorType := ctDynamic;
         cadodb.adOpenStatic: FCursorType := ctStatic;
       else FCursorType := ctUnspecified;
       end;
       FOptions := [];
       if Options and adAsyncExecute = adAsyncExecute then FOptions := FOptions + [roAsyncExecute];
       if Options and adAsyncFetch = adAsyncFetch then FOptions := FOptions + [roAsyncFetch];
       if Options and adAsyncFetchNonBlocking = adAsyncFetchNonBlocking then FOptions := FOptions + [roAsyncFetchNonBlocking];
       try
         if Assigned(FOwner.FBeforeExecute)
            then FOwner.FBeforeExecute(FOwner, Source, FCursorType, FLockType,
                 FOptions, Status, IADOCommand(pCommand), IADORecordset(pRecordset));
         if FOptions - [roAsyncExecute, roAsyncFetch, roAsyncFetchNonBlocking] <> [] then
            DatabaseError(SInvalidOptions, FOwner);
         if not (roAsyncExecute in FOptions) then Options := Options and (not adAsyncExecute);
         if not (roAsyncFetch in FOptions) then Options := Options and (not adAsyncFetch);
         if not (roAsyncFetchNonBlocking in FOptions) then Options := Options and (not adAsyncFetchNonBlocking);
         adStatus := ESToADOStatus(Status);
       except
         Application.HandleException(FOwner);
         adStatus := adStatusCancel;
       end;
     end else adStatus := adStatusUnwantedEvent;
end;

{:-- TComponentsVersion }

procedure TComponentsVersion.SetValue(Index: Integer; Value: string);
begin
  // read-only property
end;

{:-- TConnection }

procedure TConnection.AddReference(Component: TComponent);
begin
  if Component <> nil then FRecordsets.Add(Component);
end;

function TConnection.BeginTrans: Integer;
begin
  Result := 0;
  if not Active then DatabaseError(SConnectionClosed, Self);
  try
    Result := FConnection.BeginTrans;
  except
    DatabaseError(SCannotBeginTrans, Self);
  end;
end;

procedure TConnection.CancelExecuting;
begin
  if (FConnection.State and adStateExecuting = adStateExecuting) then
     FConnection.Cancel;
end;

procedure TConnection.Close;
begin
  Active := FALSE;
end;

procedure TConnection.CommitTrans;
begin
  if not Active then DatabaseError(SConnectionClosed, Self);
  try
    FConnection.CommitTrans;
  except
    DatabaseError(SCannotCommit, Self);
  end;
end;

function TConnection.Connecting: Boolean;
begin
  Result := FConnection.State and adStateConnecting = adStateConnecting;
end;

constructor TConnection.Create(AOwner: TComponent);
var FMDACVersion : IVersion;
begin
  inherited Create(AOwner);
  FStreamedCommitOnClose := TRUE;
  FClassVer := 100;
  FActive := FALSE;
  FConnectionString := '';
  FLoginPrompt := lpSimple;
  FOptions := [coCommitOnClose];
  FTransactionLevel := 0;
  FConnection := CoConnection.Create;
  Set_DefaultDatabase('');
  Set_Mode(cmReadWrite);
  Set_IsolationLevel(ilCursorStability);
  FVersion := TComponentsVersion.Create;
  FVersion.FComponents := colorADO_Version;
  FVersion.FLegalCopyright := colorADO_LegalCopyright;
  FVersion.FADO := FConnection.Version;
  FMDACVersion := nil;
  try
    FMDACVersion := coVersion.Create;
    FVersion.FMDAC := FMDACVersion.String_;
  except
    FVersion.FMDAC := 'N/A';
  end;
  FMDACVersion := nil;
  FErrors := TADOErrors.Create(Self);
  InterfaceConnect(FConnection, IADOConnectionEvents, TConnectionEvents.Create(Self, FALSE), FEventsConn);
  InterfaceConnect(FConnection, IADOConnectionEvents, TConnectionEvents.Create(Self, TRUE), FInternalEventsConn);
  FRecordsets := TList.Create;
end;

procedure TConnection.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Attributes', ReadAttributes, nil , FALSE);
  Filer.DefineProperty('CursorLocation', ReadCursorLocation, nil , FALSE);
  Filer.DefineProperty('CommitOnClose', ReadCommitOnClose, nil , FALSE);
  Filer.DefineProperty('ClassVersion', ReadClassVersion, WriteClassVersion,
          TRUE);
end;

destructor TConnection.Destroy;
var FChild: TComponent;
begin
  Destroying;
  Set_Active(FALSE);
  while FRecordsets.Count > 0 do
   begin
     FChild := FRecordsets.Items[0];
     FRecordsets.Delete(0);
     if GetParentConnection(FChild) = Self then
        SetParentConnection(FChild, nil);
   end;
  InterfaceDisconnect(FConnection, IADOConnectionEvents, FEventsConn);
  InterfaceDisconnect(FConnection, IADOConnectionEvents, FInternalEventsConn);
  FConnection := nil;
  FRecordsets.Free;
  FVersion.Free;
  FErrors.Free;
  inherited Destroy;
end;

function TConnection.Execute(CommandText: WideString;
  SourceType: TSourceType; AsyncExecute: Boolean): Integer;
var Options: Integer;
    RecordsAffected: OleVariant;
    FRecordset: IADORecordset;
begin
  CancelExecuting;
  Options :=  adExecuteNoRecords;
  RecordsAffected := Null;
  if AsyncExecute then Options := Options or adAsyncExecute;
  if (SourceType <> stStoredProc) and (SourceType <> stText) then DatabaseError(SOnlyTextOrStoredProc, Self);
  FRecordset := FConnection.Execute(CommandText, RecordsAffected, Integer(SourceTypeEnum[SourceType]) or Options);
  if FRecordset <> nil then
     try
       if (FRecordset.State and adStateOpen = adStateOpen) then FRecordset.Close;
     except
     end;
  FRecordset := nil;
  if VarType(RecordsAffected) = varInteger
     then Result := RecordsAffected
     else Result := -1;
end;

function TConnection.Executing: Boolean;
begin
  Result := FConnection.State and adStateExecuting = adStateExecuting;
end;

function TConnection.GetConnectionString(DataSourceType: TDataSourceType;
  DataSourceName: WideString): WideString;
var FOLEDBAdmin : IOLEDBAdmin;
    ConnString : WideString;
    ErrorMsg    : string;
begin
  FOLEDBAdmin := coOLEDBAdmin.Create;
  try
    case DataSourceType of
      dsnUser: FOLEDBAdmin.Open(adDSNUser);
      dsnSystem: FOLEDBAdmin.Open(adDSNSystem);
    end;
    if FOLEDBAdmin.GetConnectionString(DataSourceName, ConnString) <> 0 then
       begin
         FmtStr(ErrorMsg, SDSNNotFound, [DataSourceName]);
         DatabaseError(ErrorMsg, Self);
       end;
    Result := ConnString;
  finally
    FOLEDBAdmin.Close;
  end;
end;

procedure TConnection.GetDataSourceNames(DataSourceType: TDataSourceType;
  List: TStrings);
var i           : Integer;
    FOLEDBAdmin : IOLEDBAdmin;
    FDSN        : WideString;
begin
  FOLEDBAdmin := nil;
  List.Clear;
  FOLEDBAdmin := coOLEDBAdmin.Create;
  try
    case DataSourceType of
      dsnUser: FOLEDBAdmin.Open(adDSNUser);
      dsnSystem: FOLEDBAdmin.Open(adDSNSystem);
    end;
    for i := 0 to FOLEDBAdmin.GetDSNCount - 1 do
        begin
          FOLEDBAdmin.GetDSN(i, FDSN);
          List.Add(FDSN);
        end;
  finally
    FOLEDBAdmin := nil;
  end;
end;

type GetConnectionMethod = function: TConnection;

function TConnection.GetParentConnection(Child: TComponent): TConnection;
var PropInfo: PPropInfo;
begin
  Result := nil;
  if not (Child is TComponent) then Exit;
  PropInfo := GetPropInfo(Child.ClassInfo, 'DATABASE');
  if PropInfo = nil then PropInfo := GetPropInfo(Child.ClassInfo, 'CONNECTION');
  if PropInfo = nil then Exit;
  if PropInfo^.GetProc = nil then Exit;
  Result := TConnection(pointer(GetOrdProp(Child, PropInfo)));
end;

function TConnection.Get_Active: Boolean;
begin
  if FConnection.State and adStateOpen <> adStateOpen then Result := FALSE else Result := TRUE;
end;

function TConnection.Get_CommandTimeout: Integer;
begin
  Result := FConnection.CommandTimeout;
end;

function TConnection.Get_ConnectionString: WideString;
begin
  Result := FConnectionString;
end;

function TConnection.Get_ConnectionTimeout: Integer;
begin
  Result := FConnection.ConnectionTimeout;
end;

function TConnection.Get_DefaultDatabase: WideString;
begin
  if (csWriting in ComponentState)
     or (not Get_Active)
     then
       begin
         Result := FDefaultDatabase;
         Exit;
       end;
  try
    Result := FConnection.DefaultDatabase;
  except
    Result := '';
  end;
end;

function TConnection.Get_IsolationLevel: TIsolationLevel;
begin
  if (csWriting in ComponentState)
     or (not Get_Active)
     then Result := FIsolationLevel
     else case FConnection.IsolationLevel of
            cadodb.adXactChaos: Result := ilChaos;
            cadodb.adXactBrowse: Result := ilBrowse;
            cadodb.adXactCursorStability: Result := ilCursorStability;
            cadodb.adXactRepeatableRead: Result := ilRepeatableRead;
            cadodb.adXactIsolated: Result := ilIsolated;
          else Result := ilUnspecified;
          end;
end;

function TConnection.Get_Mode: TConnectMode;
begin
  if (csWriting in ComponentState)
     or (not Get_Active)
     then Result := FMode
     else case FConnection.Mode of
            cadodb.adModeRead: Result := cmRead;
            cadodb.adModeWrite: Result := cmWrite;
            cadodb.adModeReadWrite: Result := cmReadWrite;
            cadodb.adModeShareDenyRead: Result := cmShareDenyRead;
            cadodb.adModeShareDenyWrite: Result := cmShareDenyWrite;
            cadodb.adModeShareExclusive: Result := cmShareExclusive;
            cadodb.adModeShareDenyNone: Result := cmShareDenyNone;
            cadodb.adModeRecursive: Result := cmRecursive;
          else Result := cmUnknown;
          end;
end;

function TConnection.Get_Provider: WideString;
begin
  Result := FConnection.Provider;
end;

procedure TConnection.Loaded;
begin
  inherited;
  if  (FClassVer < 150)
  and (FStreamedCommitOnClose)
  and not (coCommitOnClose in FOptions)
  then FOptions := FOptions + [coCommitOnClose]; 
  if FActive <> Active then
     try
       Set_Active(FActive);
     except
       if csDesigning in ComponentState
          then Application.HandleException(Self)
          else raise;
     end;
end;

procedure TConnection.Open;
begin
  Active := TRUE;
end;

procedure TConnection.ReadAttributes(Reader: TReader);
var FAttributes: string;
begin
  try
    Reader.ReadValue;
    FAttributes := Reader.ReadStr;
    while FAttributes <> '' do
          begin
            if UpperCase(FAttributes) = 'CACOMMITRETAINING' then FOptions := FOptions + [coCommitRetaining];
            if UpperCase(FAttributes) = 'CAABORTRETAINING' then FOptions := FOptions + [coAbortRetaining];
            if FAttributes <> '' then if Owner is TCustomForm then TCustomForm(Owner).Designer.Modified;
            FAttributes := Reader.ReadStr;
          end;
  except
    while Reader.ReadStr <> '' do;
  end;
end;

procedure TConnection.ReadClassVersion(Reader: TReader);
begin
  FClassVer := Reader.ReadInteger;
end;

procedure TConnection.ReadCommitOnClose(Reader: TReader);
begin
  if Reader.ReadBoolean
     then FOptions := FOptions + [coCommitOnClose]
     else
       begin
         FOptions := FOptions - [coCommitOnClose];
         FStreamedCommitOnClose := FALSE;
       end;
end;

procedure TConnection.ReadCursorLocation(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadStr;
end;

procedure TConnection.RemoveReference(Component: TComponent);
begin
  if FRecordsets.IndexOf(Component) <> -1 then
     FRecordsets.Delete(FRecordsets.IndexOf(Component));
end;

procedure TConnection.RollbackTrans;
begin
  if not Active then DatabaseError(SConnectionClosed, Self);
  try
    FConnection.RollbackTrans;
  except
    DatabaseError(SCannotRollback, Self);
  end;
end;

type SetActiveMethod = procedure(Value: Boolean);

procedure TConnection.SetChildActive(Child: TComponent; Active: Boolean);
var PropInfo: PPropInfo;
begin
  if not (Child is TComponent) then Exit;
  PropInfo := GetPropInfo(Child.ClassInfo, 'ACTIVE');
  if PropInfo = nil then Exit;
  SetOrdProp(Child, PropInfo, Integer(WordBool(Active)));
end;

procedure TConnection.SetConnectionString(DataSourceType: TDataSourceType;
  DataSourceName, ConnectionString: WideString);
var FOLEDBAdmin : IOLEDBAdmin;
    ErrorMsg    : string;
begin
  FOLEDBAdmin := coOLEDBAdmin.Create;
  try
    case DataSourceType of
      dsnUser: FOLEDBAdmin.Open(adDSNUser);
      dsnSystem: FOLEDBAdmin.Open(adDSNSystem);
    end;
    if FOLEDBAdmin.SetConnectionString(DataSourceName, ConnectionString) <> 0 then
       begin
         FmtStr(ErrorMsg, SCannotModifyDSN, [DataSourceName]);
         DatabaseError(ErrorMsg, Self);
       end;
  finally
    FOLEDBAdmin.Close;
  end;
end;

procedure TConnection.SetOptions(const Value: TConnectionOptions);
begin
  if Active then Close;
  FOptions := Value;
end;

procedure TConnection.SetParentConnection(Child: TComponent;
  Connection: TConnection);
var PropInfo: PPropInfo;
begin
  if not (Child is TComponent) then Exit;
  PropInfo := GetPropInfo(Child.ClassInfo, 'DATABASE');
  if PropInfo = nil then PropInfo := GetPropInfo(Child.ClassInfo, 'CONNECTION');
  if PropInfo = nil then Exit;
  SetOrdProp(Child, PropInfo, Integer(pointer(Connection)));
end;

procedure TConnection.Set_Active(plActive: Boolean);
var i                     : Integer;
    FOLEDBAdmin           : IOLEDBAdmin;
    FConnString           : WideString;
    WindowH               : Integer;
    StrResult             : string;
    FAsyncConnect         : Integer;
    FSystemDSN            : Boolean;
    FConnectionCS         : IADOConnectionConstruction;
    FDSO                  : IUnknown;
    FDBAsynchNotify       : TDBAsynchNotify;
begin
  if (csReading in ComponentState) then
     begin
       FActive := plActive;
       Exit;
     end;
  if (plActive) and (not Get_Active) then
     begin
       if (FConnection.State and adStateConnecting = adStateConnecting)
          or (FConnection.State and adStateExecuting = adStateExecuting)
          then FConnection.Cancel;
       if (FConnection.State and adStateOpen = adStateOpen)
          then FConnection.Close;
       if Pos('OLEDBDSN', AnsiUpperCase(FConnectionString)) > 0 then
          begin
            FOLEDBAdmin := coOLEDBAdmin.Create;
            try
              FSystemDSN := FALSE;
              if AnsiUpperCase(GetConnectionStringParam(FConnectionString,
                     'OLEDBDSNTYPE')) = 'USER' then
                 begin
                   FOLEDBAdmin.Open(adDSNUser);
                   if FOLEDBAdmin.GetConnectionString(GetConnectionStringParam(FConnectionString,
                                 'OLEDBDSN'), FConnString) <> 0 then FSystemDSN := TRUE;
                   FOLEDBAdmin.Close;
                 end else FSystemDSN := TRUE;
                 if FSystemDSN then
                    begin
                      FOLEDBAdmin.Open(adDSNSystem);
                      if FOLEDBAdmin.GetConnectionString(GetConnectionStringParam(FConnectionString,
                                         'OLEDBDSN'), FConnString) <> 0 then
                          begin
                            FmtStr(StrResult, SDSNNotFound, [GetConnectionStringParam(FConnectionString,
                                      'OLEDBDSN')]);
                            DatabaseError(StrResult, Self);
                          end;
                    end;
            finally
              FOLEDBAdmin.Close;
              FOLEDBAdmin := nil;
            end;
            FConnection.ConnectionString := FConnString;
          end else FConnection.ConnectionString := FConnectionString;
       if LoginPrompt <> lpSimple then
          begin
            for i := 0 to FConnection.Properties.Count - 1 do
                begin
                  if UpperCase(FConnection.Properties [i].Name) = 'PROMPT' then
                     case FLoginPrompt of
                       lpAlways: FConnection.Properties[i].Value := adPromptAlways;
                       lpComplete: FConnection.Properties[i].Value := adPromptComplete;
                       lpCompleteRequired: FConnection.Properties[i].Value := adPromptCompleteRequired;
                       lpNever: FConnection.Properties[i].Value := adPromptNever;
                     end;
                  if UpperCase(FConnection.Properties [i].Name) = 'WINDOW HANDLE' then
                     begin
                       WindowH := Application.DialogHandle;
                       if WindowH = 0 then WindowH := Application.Handle;
                       FConnection.Properties[i].Value := WindowH;
                     end;
                end;
          end;
       FConnection.Attributes := 0;
       if coCommitRetaining in FOptions
          then FConnection.Attributes := cadodb.adXactCommitRetaining;
       if coAbortRetaining in FOptions
          then FConnection.Attributes := FConnection.Attributes or cadodb.adXactAbortRetaining;
       if (coAsyncConnect in FOptions) and (not (csDesigning in ComponentState))
          then FAsyncConnect := adAsyncConnect
          else FAsyncConnect := Integer(adConnectUnspecified);
      FConnection.Open('', '', '', FAsyncConnect);
      if  (FConnection.QueryInterface(IID_IADOConnectionConstruction, FConnectionCS) = S_OK)
      and (FConnectionCS.Get_DSO(FDSO) = S_OK)
      and (FDSO <> nil) then
      begin
        FDBAsynchNotify := TDBAsynchNotify.Create(Self);
        if not IDBAsynchNotifyConnect(FDSO, IID_IDBAsynchNotifyVt,
                  FDBAsynchNotify, i) then
               FDBAsynchNotify.Destroy;
        FDSO := nil;
      end;
     end else
           if (not plActive) and (FConnection.State <> adStateClosed) then
              begin
                for i := 0 to FRecordsets.Count - 1 do
                    begin
                      try
                        if GetParentConnection(FRecordsets.Items[i]) = Self then
                           SetChildActive(FRecordsets.Items[i], FALSE);
                      except end;
                    end;
                if FTransactionLevel > 0 then
                   try
                     FConnection.Attributes := FConnection.Attributes and (not (cadodb.adXactCommitRetaining or cadodb.adXactAbortRetaining));
                     while FTransactionLevel > 0 do
                           if coCommitOnClose in FOptions
                              then FConnection.CommitTrans
                              else FConnection.RollbackTrans;
                   except
                   end;
                if (FConnection.State and adStateConnecting = adStateConnecting)
                   or (FConnection.State and adStateExecuting = adStateExecuting)
                   then FConnection.Cancel;
                if (FConnection.State and adStateOpen = adStateOpen)
                   then FConnection.Close;
              end;
end;

procedure TConnection.Set_CommandTimeout(plTimeout: Integer);
begin
  FConnection.CommandTimeout := plTimeout ;
end;

procedure TConnection.Set_ConnectionString(const pbstr: WideString);
begin
  if Active then Close;
  FConnectionString := pbstr;
  if GetConnectionStringParam(pbstr, 'Provider') <> ''
     then Provider := GetConnectionStringParam(pbstr, 'Provider');
end;

procedure TConnection.Set_ConnectionTimeout(plTimeout: Integer);
begin
  FConnection.ConnectionTimeout := plTimeout;
end;

procedure TConnection.Set_DefaultDatabase(DefaultDatabase: WideString);
begin
  if FConnection.State and adStateOpen = adStateOpen
     then FConnection.DefaultDatabase := DefaultDatabase;
  FDefaultDatabase := DefaultDatabase;
end;

procedure TConnection.Set_IsolationLevel(Level: TIsolationLevel);
begin
  FConnection.IsolationLevel := IsolationLevelEnum[Level];
  FIsolationLevel := Level;
end;

procedure TConnection.Set_Mode(Mode: TConnectMode);
begin
  FConnection.Mode := ConnectModeEnum[Mode];
  FMode := Mode;
end;

procedure TConnection.Set_Provider(Provider: WideString);
begin
  FConnection.Provider := Provider;
end;

function TConnection.ShowErrors(ClearList: Boolean): Boolean;
var FOLEDBProviderErrorsForm: TOLEDBProviderErrorsForm;
begin
  Result := FALSE;
  if FConnection.Errors.Count = 0 then Exit;
  FOLEDBProviderErrorsForm := TOLEDBProviderErrorsForm.Create(Self, FConnection.Errors);
  try
    FOLEDBProviderErrorsForm.ShowModal;
  finally
    FOLEDBProviderErrorsForm.Free;
  end;
  if ClearList then FConnection.Errors.Clear;
  Result := TRUE;
end;

procedure TConnection.WriteClassVersion(Writer: TWriter);
begin
  Writer.WriteInteger(150);
end;

{:-- TRecordsetEvents -- currently not supported}

constructor TRecordsetEvents.Create(AOwner: TCustomRecordset);
begin
  inherited Create;
  FRefCount := 0;
  FOwner := AOwner;
end;

destructor TRecordsetEvents.Destroy;
begin
  inherited;
end;

function TRecordsetEvents.EndOfRecordset(var fMoreData: WordBool;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.FetchComplete(const pError: IADOError;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusOK;
  Status := ADOStatusToES(adStatus);
  if Assigned(FOwner.FOnFetchComplete) then
     begin
       FOwner.FOnFetchComplete(FOwner, pErrorToADOError(pError), Status);
       adStatus := ESToADOStatus(Status);
     end else adStatus := cadodb.adStatusOK;
end;

function TRecordsetEvents.FetchProgress(Progress, MaxProgress: Integer;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
var Status: TEventStatus;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusOK;
  Status := ADOStatusToES(adStatus);
  if Assigned(FOwner.FOnFetchProgress) then
     begin
       FOwner.FOnFetchProgress(FOwner, Progress, MaxProgress, Status);
       adStatus := ESToADOStatus(Status);
     end else adStatus := cadodb.adStatusOK;
end;

function TRecordsetEvents.FieldChangeComplete(cFields: Integer;
  Fields: OleVariant; const pError: IADOError; var adStatus: Integer;
  const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.MoveComplete(adReason: EventReasonEnum;
  const pError: IADOError; var adStatus: Integer;
  const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.RecordChangeComplete(adReason: EventReasonEnum;
  cRecords: Integer; const pError: IADOError; var adStatus: Integer;
  const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.RecordsetChangeComplete(
  adReason: EventReasonEnum; const pError: IADOError;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.WillChangeField(cFields: Integer;
  Fields: OleVariant; var adStatus: Integer;
  const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.WillChangeRecord(adReason: EventReasonEnum;
  cRecords: Integer; var adStatus: Integer;
  const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.WillChangeRecordset(adReason: EventReasonEnum;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

function TRecordsetEvents.WillMove(adReason: EventReasonEnum;
  var adStatus: Integer; const pRecordset: IADORecordset): HResult;
begin
  Result := S_OK;
  adStatus := cadodb.adStatusUnwantedEvent;
end;

{:-- TDBAsynchNotify }

constructor TDBAsynchNotify.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TDBAsynchNotify.Destroy;
begin
  inherited Destroy;
end;

function TDBAsynchNotify.OnLowResource(dwReserved: DWORD): HRESULT;
begin
  Result := S_OK;
  if FOwner is TCustomRecordset then
     try
       if  (Assigned(TCustomRecordset(FOwner).FOnLowResource))
       then TCustomRecordset(FOwner).FOnLowResource(FOwner);
     except
       Application.HandleException(FOwner);
     end;
end;

function TDBAsynchNotify.OnProgress(const hChapter: ULONG;
  const eOperation: DWORD; const ulProgress, ulProgressMax: ULONG;
  const eAsynchPhase: DWORD; const pwszStatusText: WideString): HRESULT;
var Status: TEventStatus;
begin
  Status := esOK;
  if FOwner is TCustomRecordset then
     try
       if  (Assigned(TCustomRecordset(FOwner).FOnFetchProgress))
       and (eOperation = DBASYNCHOP_OPEN)
       and ((eAsynchPhase = DBASYNCHPHASE_POPULATION) or (eAsynchPhase = DBASYNCHPHASE_COMPLETE))
       then TCustomRecordset(FOwner).FOnFetchProgress(FOwner,
               ulProgress, ulProgressMax, Status);
     except
       Application.HandleException(FOwner);
     end;
  Result := S_OK;
end;

function TDBAsynchNotify.OnStop(hChapter: ULONG; eOperation: DWORD;
  hrStatus: HRESULT; pwszStatusText: WideString): HRESULT;
var Status: TEventStatus;
    Error: TAdoError;
begin
  if hrStatus = S_OK
     then Status := esOK
     else
       begin
         Status := esErrorsOccurred;
         Error.Description := pwszStatusText;
         Error.Number := hrStatus;
       end;
  if FOwner is TCustomRecordset then
     try
       if  (Assigned(TCustomRecordset(FOwner).FOnFetchComplete))
       and (eOperation = DBASYNCHOP_OPEN)
       then TCustomRecordset(FOwner).FOnFetchComplete(FOwner,
                     Error, Status);
     except
       Application.HandleException(FOwner);
     end;
  Result := S_OK;
end;

{:-- TCDataSet }

function TCDataSet.AllocRecordBuffer: PChar;
begin
  GetMem(Result, FRecordSize+SizeOf(TRecInfo));
  ZeroMemory(Result, FRecordSize+SizeOf(TRecInfo));
  InternalInitRecord(Result);
end;

{$IFDEF VCL40}
type FMTBCD = packed array [0..33] of Byte;
type UntypedFMTBCDToCurr = function(const BCD: FMTBCD; var Curr: Currency): Boolean;

function TCDataSet.BCDToCurr(BCD: Pointer;
  var Curr: Currency): Boolean;
var FBCD: FMTBCD;
begin
  CopyMemory(@FBCD, BCD, 34);
  Result := UntypedFMTBCDToCurr(@FMTBCDToCurr)(FBCD, Curr);
end;
{$ENDIF}

procedure TCDataSet.FreeRecordBuffer(var Buffer: PChar);
var i: integer;
begin
  VariantClear(PRecInfo(Buffer + FRecInfoOfs)^.Bookmark);
  for i := 0 to (FRecordSize div SizeOf(TRecord)) - 1 do
      variant(TRecData(pointer(Buffer)^)[i].Data) := Unassigned;
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TCDataSet.ClearBuffers;
var i: Integer;
begin
  inherited;
  for i := 0 to BufferCount - 1 do
      InternalInitRecord(Buffers [i]);
end;

procedure TCDataSet.ClearCalcFields(Buffer: PChar);
var i: Integer;
begin
  inherited;
  if CalcFieldsSize = 0 then Exit;
  for i := 0 to Fields.Count - 1 do
      if Fields [i].FieldKind = fkCalculated then
         TRecData(pointer(Buffer)^)[i].Data := Unassigned;
end;

{$IFDEF VCL40}
type UntypedCurrToFMTBCD = function(const Curr: Currency; var BCD: FMTBCD;
        Precision, Decimals: Integer): Boolean;

function TCDataSet.CurrToBCD(const Curr: Currency; BCD: Pointer;
  Precision, Decimals: Integer): Boolean;
var FBCD: FMTBCD;
begin
  Result := UntypedCurrToFMTBCD(@CurrToFMTBCD)(Curr, FBCD, 32, Decimals);
  if Result then CopyMemory(BCD, @FBCD, 34);
end;
{$ENDIF}

function TCDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TCBlobStream.Create(TBlobField(Field), Mode);
end;

function TCDataSet.GetFieldClass(
  FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftVariant
     then Result := TCVariantField
     else Result := inherited GetFieldClass(FieldType);
  {$IFDEF VCL40}
  if FieldType = ftWideString
     then Result := TWideStringField;
  {$ENDIF}
end;

{$IFDEF VCL50}
function TCDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  if Field.DataType = ftWideString
     then Result := GetFieldData(Field, Buffer)
     else Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$ENDIF VCL50}

procedure TCDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if VarType(Variant(PRecInfo(Buffer + FRecInfoOfs).Bookmark)) > 0 then
     POleVariant(Data)^ := PRecInfo(Buffer + FRecInfoOfs).Bookmark
     else
       VariantClear(POleVariant(Data)^);
end;

function TCDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

function TCDataSet.GetRecordSize: Word;
begin
  Result := SizeOf(TRecord) * Fields.Count;
end;

procedure TCDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TCDataSet.InternalInitRecord(Buffer: PChar);
var i: integer;
begin
  for i := 0 to (FRecordSize div SizeOf(TRecord)) - 1 do
      begin
        variant(TRecData(pointer(Buffer)^)[i].Data) := Unassigned;
        TRecData(pointer(Buffer)^)[i].Modified := FALSE;
      end;
  VariantClear(PRecInfo(Buffer + FRecInfoOfs)^.Bookmark);
end;

procedure TCDataSet.InternalInsert;
begin
  InternalInitRecord(ActiveBuffer);
end;

procedure TCDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PRecInfo(Buffer + FRecInfoOfs).Bookmark);
end;

procedure TCDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(OleVariant);
  if IsCursorOpen then
     begin
       FRecordSize := GetRecordSize;
       FRecInfoOfs := FRecordSize;
     end;
end;

procedure TCDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if (Buffer = nil) or (Data = nil) then Exit;
  PRecInfo(Buffer + FRecInfoOfs).Bookmark := POleVariant(Data)^;
end;

procedure TCDataSet.SetBookmarkFlag(Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

function TCDataSet.InternalGetFieldData(Field: TField; RecBuffer: PChar;
  Buffer: Pointer; WithoutTrailingBlanks: Boolean): Boolean;
var value         : Variant;
    p             : Pointer;
    n             : Integer;
    vDateTime     : TDateTime;
    vSmallInt     : SmallInt;
    vInteger      : Integer;
    vWord         : Word;
    vDouble       : Double;
    vCurrency     : Currency;
    {$IFDEF VCL50}
    vBcd          : TBcd;
    {$ENDIF}
begin
  value := TRecData(pointer(RecBuffer)^)[Field.Index].Data;
  if (VarIsEmpty(value)) then
     begin
      Result := FALSE;
      Exit;
     end else
         if (VarIsNull(value)) then
         begin
           Result := FALSE;
           Exit;
         end;
  p := nil;
  Result := TRUE;
  case VarType(value) of
  varOleStr   : p := pointer(TVarData(value).VOleStr);
  varString   : p := pointer(TVarData(value).VString);
  end;
  case Field.DataType of
  ftString,
  {$IFDEF VCL50}
  ftGUID,
  {$ENDIF}
  ftFixedChar:  if p <> nil then
                begin
                  case VarType(value) of
                    varOleStr: if Buffer <> nil then StrCopy(Buffer, PChar(OleStrToString(PWideChar(p))));
                    varString: if Buffer <> nil then StrCopy(Buffer, PChar(p));
                    else Result := FALSE;
                  end;
                  if Result and (Buffer <> nil) and WithoutTrailingBlanks then
                     begin
                       n := StrLen(Buffer);
                       while (n > 0) and (PChar(Buffer)[n-1] = #32) do
                            begin
                              PChar(Buffer)[n-1] := #0;
                              dec(n);
                            end;
                     end;
                end else if Buffer <> nil then StrCopy(Buffer, '');
  ftWideString: if (p <> nil) then
                begin
                  case VarType(value) of
                    varOleStr: if Buffer <> nil then WideString(Buffer^) := PWideChar(p);
                    varString: if Buffer <> nil then WideString(Buffer^) := string(p);
                    else Result := FALSE;
                  end;
                  if Result and (Buffer <> nil) and WithoutTrailingBlanks then
                     begin
                       n := SysStringLen(PWideChar(WideString(Buffer^)));
                       while (n > 0) and (PWideChar(WideString(Buffer^))[n-1] = #32) do
                            begin
                              PWideChar(WideString(Buffer^))[n-1] := #0;
                              dec(n);
                            end;
                     end;
                end else if Buffer <> nil then PWideChar(Buffer^) := nil;
  ftSmallInt :  begin
                 vSmallInt := value;
                 if Buffer <> nil then SmallInt(Buffer^) := vSmallInt;
                end;
  ftInteger,
  ftAutoInc  :  begin
                 vInteger := value;
                 if Buffer <> nil then Integer(Buffer^) := vInteger;
                end;
  ftWord,
  ftBoolean  :  begin
                 vWord := value;
                 if Buffer <> nil then Word(Buffer^) := vWord;
                end;
  ftFloat,
  ftCurrency,
  ftLargeInt :  begin
                 vDouble := value;
                 if Buffer <> nil then Double(Buffer^) := vDouble;
                end;
  ftBCD      :  begin
                 vCurrency := value;
                 {$IFDEF VCL50}
                 if Buffer <> nil then
                    if CurrToBCD(vCurrency, vBCD, 32, Field.Size)
                       then CopyMemory(Buffer, @vBcd, SizeOf(TBcd));
                 {$ELSE}
                 if Buffer <> nil then CurrToBCD(vCurrency, Buffer, 32, Field.Size);
                 {$ENDIF}
                end;
  ftDate     :  begin
                 vDateTime := value;
                 if Buffer <> nil then Integer(Buffer^) := DateTimeToTimeStamp(vDateTime).Date;
                end;
  ftTime     :  begin
                 vDateTime := value;
                 if Buffer <> nil then Integer(Buffer^) := DateTimeToTimeStamp(vDateTime).Time;
                end;
  ftDateTime :  begin
                 vDateTime := value;
                 if Buffer <> nil then TDateTimeAlias(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(vDateTime));
                end;
  ftBytes    :  if VarType(value) = varArray or VarByte then
                   begin
                     if Buffer <> nil then
                        begin
                          p := VarArrayLock(value);
                          try
                            CopyMemory(Buffer, p, Field.Size);
                          finally
                            VarArrayUnlock(value);
                          end;
                        end;
                   end else Result := FALSE;
  ftVarBytes  :  if VarType(value) = varArray or VarByte then
                   begin
                     if Buffer <> nil then
                        begin
                          vWord := VarArrayHighBound(value, 1) - VarArrayLowBound(value, 1) + 1;
                          p := VarArrayLock(value);
                          try
                            CopyMemory(Buffer, @vWord, 2);
                            CopyMemory(Pointer(Integer(Buffer) + 2), p, vWord);
                          finally
                            VarArrayUnlock(value);
                          end;
                        end;
                   end else Result := FALSE;
  ftMemo, ftBlob : if Buffer = nil then
                      begin
                        with CreateBlobStream(Field, bmRead) do
                             try
                               Result := Size > 0; 
                             finally
                               Free;
                             end;
                      end else Result := FALSE;
  else Result := FALSE;
  end;
  if (not Result) and (Field.DataType = ftVariant) then
     begin
       Result := TRUE;
       if Buffer <> nil then VariantCopy(OleVariant(Buffer^), value);
     end;
end;

{$IFDEF VCL50}
procedure TCDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
begin
  if Field.DataType = ftWideString
     then SetFieldData(Field, Buffer)
     else inherited SetFieldData(Field, Buffer, NativeFormat);
end;
{$ENDIF VCL50}

procedure TCDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var value         : Variant;
    p             : Pointer;
    vSmallInt     : SmallInt;
    vInteger      : Integer;
    vWord         : Word;
    vDouble       : Double;
    vCurrency     : Currency;
    vChar         : PChar;
    vTimeStamp    : TTimeStamp;
    RecBuffer     : Pointer;
begin
  value := Unassigned;
  if Buffer = nil then value := Null else
  case Field.DataType of
  ftString,
  {$IFDEF VCL50}
  ftGUID,
  {$ENDIF}
  ftFixedChar
             :  begin
                  vChar := PChar(Buffer);
                  value := StrPas(vChar);
                end;
  ftWideString
             :  begin
                  value := WideString(Buffer^);
                end;
  ftSmallInt :  begin
                 vSmallInt := SmallInt(Buffer^);
                 value := vSmallInt;
                end;
  ftInteger,
  ftAutoInc  :  begin
                 vInteger := Integer(Buffer^);
                 value := vInteger;
                end;
  ftWord,
  ftBoolean  :  begin
                 vWord := Word(Buffer^);
                 value := vWord;
                end;
  ftFloat,
  ftCurrency,
  ftLargeInt :  begin
                 vDouble := Double(Buffer^);
                 value := vDouble;
                end;
  ftBCD      :  begin
                 {$IFDEF VCL40}
                 if BCDToCurr(Buffer, vCurrency)
                    then value := vCurrency
                    else DatabaseError(SFieldValueError);
                 {$ELSE}
                 if BCDToCurr(TBcd(Buffer^), vCurrency)
                    then value := vCurrency
                    else DatabaseError(SFieldValueError);
                 {$ENDIF}
                end;
  ftDate     :  begin
                 vTimeStamp.Time := 0;
                 vTimeStamp.Date := Integer(Buffer^);
                 value := TimeStampToDateTime(vTimeStamp);
                end;
  ftTime     :  begin
                 vTimeStamp.Time := LongInt(Buffer^);
                 vTimeStamp.Date := DateDelta;
                 value := TimeStampToDateTime(vTimeStamp);
                end;
  ftDateTime :  begin
                 value := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
                end;
  ftBytes    :  begin
                  value := VarArrayCreate([0, Field.Size - 1], varByte);
                  p := VarArrayLock(value);
                  try
                    CopyMemory(p, Buffer, Field.Size);
                  finally
                    VarArrayUnlock(value);
                  end;
                end;
  ftVarBytes :  begin
                  vWord := Word(Buffer^);
                  value := VarArrayCreate([0, vWord - 1], varByte);
                  p := VarArrayLock(value);
                  try
                    CopyMemory(p, Buffer, vWord);
                  finally
                    VarArrayUnlock(value);
                  end;
                end;
  end;
  if (Field.DataType = ftVariant) and (Buffer <> nil) then
     value := OleVariant(Buffer^);
  if not ValidateFieldData(Field, Value) then
     raise EDatabaseError.CreateFmt(SFieldValueError, [Field.FieldName]);
  if State = dsCalcFields
     then RecBuffer := CalcBuffer
     else RecBuffer := ActiveBuffer;
  if Field.DataType = ftBoolean
     then
         begin
           if WordBool(TRecData(pointer(RecBuffer)^)[Field.Index].Data) <> WordBool(value) then
              begin
                TRecData(pointer(RecBuffer)^)[Field.Index].Modified :=
                not TRecData(pointer(RecBuffer)^)[Field.Index].Modified
              end;
         end
     else TRecData(pointer(RecBuffer)^)[Field.Index].Modified := TRUE;
  TRecData(pointer(RecBuffer)^)[Field.Index].Data := value;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
     DataEvent(deFieldChange, Longint(Field));
end;

function TCDataSet.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
begin
  Result := TRUE;
end;

{:-- TCustomRecordset }

procedure TCustomRecordset.ApplyUpdates(AffectedRecords: TAffectedRecords);
begin
  CheckLocked;
  if not FRecordset.Supports(adUpdateBatch) then DatabaseError(SNotItBatchMUpdateMode, Self);
  CheckBrowseMode;
  UpdateCursorPos;
  FRecordset.UpdateBatch(adAffectAll);
  Resync([]);
end;

procedure TCustomRecordset.CancelExecuting;
begin
  try
    InternalClose;
  except end;
end;

procedure TCustomRecordset.CancelUpdates(
  AffectedRecords: TAffectedRecords);
begin
  CheckLocked;
  if not FRecordset.Supports(adUpdateBatch) then
     DatabaseError(SNotItBatchMUpdateMode, Self);
  CheckBrowseMode;
  UpdateCursorPos;
  FRecordset.CancelBatch(AffectedRecordsEnum[AffectedRecords]);
  Resync([]);
end;

function TCustomRecordset.CheckCurrentRecord: Boolean;
var PrevState  :  TDataSetState;
    Buffer     :  PChar;
    i,n        :  Integer;
begin
  Result := TRUE;
  if (not Filtered) or (not Assigned(OnFilterRecord)) then Exit;
  Buffer := AllocRecordBuffer;
  try
    InternalInitRecord(Buffer);
    for i := 0 to Fields.Count - 1 do
      if Fields [i].FieldKind = fkData then
         begin
           n := Fields [i].FieldNo - 1;
           try
             if  (roHugeBlobs in FOptions)
             and (FRecordset.Fields[n].Attributes and adFldLong = adFldLong)
             then TRecData(pointer(Buffer)^)[i].Data := Null
             else TRecData(pointer(Buffer)^)[i].Data := FRecordset.Fields[n].Value;
           except TRecData(pointer(Buffer)^)[i].Data := Null; end;
         end;
    GetRecordBookmark(Buffer, gmCurrent, TRUE);
    PrevState :=  SetTempState(dsFilter);
    FFilteredRecord := Buffer;
    try
      FOnFilterRecord(Self, Result);
    finally
      RestoreState(PrevState);
    end;
  finally
    FreeRecordBuffer(Buffer);
  end;
end;

function TCustomRecordset.CloneRecordset(
  LockType: TLockType): TCustomRecordset;
var FNewRecordset  : IADORecordset;
begin
  CheckLocked;
  if not IsCursorOpen then DatabaseError(SDataSetClosed);
  if not SupportsBookmarks then DatabaseError(SNoBookmarks);
  if (LockType <> ltUnspecified) and (LockType <> ltReadOnly) then
     DatabaseError(SInvalidLockType);
  Result := TCustomRecordset.Create(Owner);
  FNewRecordset := nil;
  FNewRecordset := FRecordset.Clone(_LockTypeEnum[LockType]);
  if FNewRecordset = nil then Exit;
  Result.FRecordset := nil;
  Result.FRecordset := FNewRecordset;
  FNewRecordset := nil;
  try
    Result.FNextRecordset := TRUE;
    Result.Open;
  finally
    Result.FNextRecordset := FALSE;
  end;
end;

function TCustomRecordset.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
begin
  CheckLocked;
  if (Bookmark1 = nil) or (Bookmark2 = nil) then
     begin
       Result := 0;
       Exit;
     end;
  case FRecordset.CompareBookmarks(Variant(Pointer(Bookmark1)^), Variant(Pointer(Bookmark2)^)) of
    adCompareEqual: Result := 0;
    adCompareGreaterThan: Result := 1;
    adCompareLessThan: Result := -1;
    else begin
           Result := 0;
           DatabaseError(SCannotCompareBookmarks, Self);
         end;
  end;
end;

constructor TCustomRecordset.Create(Aowner: TComponent);
var FObject: TObject;
begin
  try
    inherited Create(AOwner);
  finally
    FCacheSize := 1;
    FDSLocks := 0;
    FRequery := FALSE;
    FExecuteOnly := FALSE;
    FNoRecords := FALSE;
    FNextRecordset := FALSE;
    FTableDirect := FALSE;
    FRecordset := CoRecordSet.Create;
    FEventsConn := -1;
    FOptions := [roResync, roWithoutTrailingBlanks];
    FQRDataLink := nil;
    FQRDataSource := nil;
    FOldRecordset := nil;
    Set_CursorLocation(clServer);
    Set_CursorType(ctKeyset);
    Set_LockType(ltOptimistic);
    Set_MarshalOptions(moMarshalAll);
    FCommand := nil;
    FCommandTimeout := tmSuperior;
    FSQL := TStringList.Create;
    FParams := TParams.Create(Self);
    TStringList(FSQL).OnChange := SqlOnChange;
    FDataLink := TCustomRecordsetDataLink.Create(Self);
    ParamCheck := TRUE;
    Filter := ftFilterNone;
    FConnection := nil;
    if AOwner = nil
       then FObject := nil
       else FObject := AOwner.FindComponent('Connection1');
    if (FObject <> nil) and (FObject is TConnection) then Connection := TConnection(FObject);
  end;
end;

destructor TCustomRecordset.Destroy;
begin
  Destroying;
  Close;
  try
    if  (FRecordset <> nil)
    and (FRecordset.State and adStateOpen = adStateOpen)
    then FRecordset.Close;
  except end;
  try
    if  (FCommand <> nil)
    and (FCommand.State and adStateExecuting = adStateExecuting)
    then FCommand.Cancel;
  except end;
  if FQRDataLink <> nil then FQRDataLink.Free;
  if FQRDataSource <> nil then FQRDataSource.Free;
  FCommand := nil;
  Connection := nil;
  FDataLink.Free;
  TStringList(FSQL).OnChange := nil;
  FSQL.Free;
  FParams.Free;
  //if (FRecordset <> nil) and (FEventsConn <> -1) then
  //   InterfaceDisconnect(FRecordset, IADORecordsetEvents, FEventsConn);
  FRecordset := nil;
  if not (csInheritable in ComponentStyle) then Exit;
  inherited Destroy;
end;

procedure TCustomRecordset.DoAfterPost;
begin
  inherited;
end;

procedure TCustomRecordset.DoBeforeInsert;
begin
  if not FRecordset.Supports(adAddNew) then
     DatabaseError(SCannotAddRecords, Self);
  UpdateCursorPos;
  FOldCurrentRecord := CurrentRecord;
  inherited;
end;

procedure TCustomRecordset.ExecProc;
begin
  Execute;
end;

procedure TCustomRecordset.ExecSQL;
begin
  Execute;
end;

procedure TCustomRecordset.Execute;
begin
  InternalClose;
  FExecuteOnly := TRUE;
  InternalOpen;
end;

function TCustomRecordset.Executing: Boolean;
begin
  Result := (FCommand <> nil)
            and
            (FCommand.State and adStateExecuting = adStateExecuting);
end;

function TCustomRecordset.FilterAsInteger(Value: string): Integer;
begin
  Result := -1;
  if Value = ftFilterNone then Result := 0;
  if Value = ftFilterPendingRecords then Result := 1;
  if Value = ftFilterAffectedRecords then Result := 2;
  if Value = ftFilterFetchedRecords then Result := 3;
  if Value = ftFilterConflictingRecords then Result := 4;
end;

function TCustomRecordset.FindKey(
  const KeyValues: array of Variant): Boolean;
begin
  CheckActive;
  CheckLocked;  
  CheckBrowseMode;
  if not SupportsBookmarks then DatabaseError(SNoBookmarks, Self);
  DoBeforeScroll;
  if FRecordset.Supports(adMovePrevious) then InternalFirst;
  FRecordset.Seek(OleVariant(VarArrayOf(KeyValues)), adSeekFirstEQ);
  if FRecordset.EOF then Result := FALSE else Result := TRUE;
  if Result then Result := CheckCurrentRecord;
  if not Result then
     begin
       UpdateCursorPos;
       Exit;
     end;
  CursorPosChanged;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;

function TCustomRecordset.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := FALSE;
  if Restart and GoForward then
     Result := Locate('', NULL, [], loFromBeginningForward);
  if Restart and not GoForward then
     Result := Locate('', NULL, [], loFromEndBackward);
  if not Restart and GoForward then
     Result := Locate('', NULL, [], loFromCurrentForward);
  if not Restart and not GoForward then
     Result := Locate('', NULL, [], loFromCurrentBackward);
end;

function TCustomRecordset.Get_CacheSize: Integer;
begin
  if Active
     then Result := FRecordset.Get_CacheSize
     else Result := FCacheSize;
end;

function TCustomRecordset.Get_CursorLocation: TCursorLocation;
begin
  if (csWriting in ComponentState)
     or (not IsCursorOpen)
     then Result := FCursorLocation
     else case FRecordset.CursorLocation of
            cadodb.adUseClient: Result := clClient;
            cadodb.adUseServer: Result := clServer;
          else Result := clNone;
          end;
end;

function TCustomRecordset.Get_CursorType: TCursorType;
begin
  if (csWriting in ComponentState)
     or (not IsCursorOpen)
     then Result := FCursorType
     else case FRecordset.CursorType of
            cadodb.adOpenForwardOnly: Result := ctForwardOnly;
            cadodb.adOpenKeyset: Result := ctKeyset;
            cadodb.adOpenDynamic: Result := ctDynamic;
            cadodb.adOpenStatic: Result := ctStatic;
          else Result := ctUnspecified;
          end;
end;

function TCustomRecordset.Get_LockType: TLockType;
begin
  if (csWriting in ComponentState)
     or (not IsCursorOpen)
     then Result := FLockType
     else case FRecordset.LockType of
            cadodb.adLockReadOnly: Result := ltReadOnly;
            cadodb.adLockPessimistic: Result := ltPessimistic;
            cadodb.adLockOptimistic: Result := ltOptimistic;
            cadodb.adLockBatchOptimistic: Result := ltBatchOptimistic;
          else Result := ltUnspecified;
          end;
end;

function TCustomRecordset.Get_MarshalOptions: TMarshalOptions;
begin
  if (IsCursorOpen) and (not (csWriting in ComponentState)) then
     begin
       if FRecordset.Get_MarshalOptions = cadodb.adMarshalModifiedOnly
          then Result := moMarshalModifiedOnly
          else Result := moMarshalAll;
     end else Result := FMarshalOptions;
end;

function TCustomRecordset.Get_MaxRecords: Integer;
begin
  Result := FRecordset.Get_MaxRecords;
end;

function TCustomRecordset.Get_Sort: string;
var FCriteria  : string;
    i          : Integer;
begin
  FCriteria := FRecordset.Get_Sort;
  i := Pos(',', FCriteria);
  while i > 0 do
        begin
          FCriteria [i] := ';';
          i := Pos(',', FCriteria);
        end;
  Result := FCriteria;
end;

function TCustomRecordset.Get_Supports: TSupportsOptions;
begin
  Result := [];
  if FRecordset.Supports(adAddNew) then Result := Result + [spAddNew];
  if FRecordset.Supports(adApproxPosition) then Result := Result + [spApproxPosition];
  if FRecordset.Supports(adBookmark) then Result := Result + [spBookmark];
  if FRecordset.Supports(adDelete) then Result := Result + [spDelete];
  if FRecordset.Supports(adHoldRecords) then Result := Result + [spHoldRecords];
  if FRecordset.Supports(adMovePrevious) then Result := Result + [spMovePrevious];
  if FRecordset.Supports(adResync) then Result := Result + [spResync];
  if FRecordset.Supports(adUpdate) then Result := Result + [spUpdate];
  if FRecordset.Supports(adUpdateBatch) then Result := Result + [spUpdateBatch];
  if FRecordset.Supports(adIndex) then Result := Result + [spIndex];
  if FRecordset.Supports(adSeek) then Result := Result + [spSeek];
  if FRecordset.Supports(adFind) then Result := Result + [spFind];
  if FRecordset.Supports(adNotify) then Result := Result + [spNotify];
end;

procedure TCustomRecordset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if not SupportsBookmarks then Exit;
  inherited GetBookmarkData(Buffer, Data);
end;

function TCustomRecordset.GetCanModify: Boolean;
begin
  if not IsCursorOpen then Result := FALSE else
     if (FRecordSet.Supports(adAddNew)) or
        (FRecordSet.Supports(adDelete)) or
        (FRecordSet.Supports(adUpdate)) then Result := TRUE else Result := FALSE;
end;

function TCustomRecordset.GetCurrent_NextRecord(Current: Boolean;
  RecordBuffer: PChar): TGetResult;
var PrevState  :  TDataSetState;
    Again      :  Boolean;
    Buffer     :  PChar;
    i,n        :  Integer;
begin
  repeat
    Buffer := RecordBuffer;
    Again := FALSE;
    Result := grEOF;
    if FRecordset.EOF then Exit;
    if not Current and not FRecordset.BOF and (FRecordset.EditMode <> adEditNone) then Exit;
    if not Current then FRecordset.MoveNext;
    if (FRecordset.EOF) then
       begin
         if (not FRecordSet.BOF) and (FRecordSet.Supports(adMovePrevious)) then
            FRecordSet.MovePrevious;
       end else Result := grOK;
    if (Result = grOK) and ((Buffer <> nil) or (Assigned(FOnFilterRecord) and Filtered)) then
       begin
         if Buffer = nil then Buffer := AllocRecordBuffer;
         InternalInitRecord(Buffer);
         for i := 0 to Fields.Count - 1 do
           if Fields [i].FieldKind = fkData then
              begin
                n := Fields [i].FieldNo - 1;
                try
                  if  (roHugeBlobs in FOptions)
                  and (FRecordset.Fields[n].Attributes and adFldLong = adFldLong)
                  then TRecData(pointer(Buffer)^)[i].Data := Null
                  else TRecData(pointer(Buffer)^)[i].Data := FRecordset.Fields[n].Value;
                except TRecData(pointer(Buffer)^)[i].Data := Null; end;
              end;
         GetRecordBookmark(Buffer, gmCurrent, TRUE);
         if Assigned(FOnFilterRecord) and Filtered then
            begin
              PrevState :=  SetTempState(dsFilter);
              FFilteredRecord := Buffer;
              try
                Again := TRUE;
                FOnFilterRecord(Self, Again);
              finally
                Again := not Again;
                if RecordBuffer = nil then FreeRecordBuffer(Buffer);
                RestoreState(PrevState);
              end;
            end;
       end;
    if Again and Current then
       begin
         Again := FALSE;
         Result := grEOF;
       end;
  until not Again;
  if (Result <> grOK) and (RecordBuffer <> nil) then InternalInitRecord(RecordBuffer);
end;

function TCustomRecordset.GetDS: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomRecordset.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
var RecBuffer     : PChar;
begin
  if (State = dsFilter) and (Assigned(FOnFilterRecord)) and Filtered
     then RecBuffer := FFilteredRecord
     else if State = dsCalcFields
             then RecBuffer := CalcBuffer
             else RecBuffer := ActiveBuffer;
  Result := InternalGetFieldData(Field, RecBuffer, Buffer,
    roWithoutTrailingBlanks in FOptions);
end;

procedure TCustomRecordset.GetFields(Fields: string; List: TStrings);
var p: Integer;
begin
  List.Clear;
  p := Pos(';', Fields);
  while p > 0 do
        begin
          if p > 1 then List.Add(Copy(Fields, 1, p-1));
          System.Delete(Fields, 1, p);
          p := Pos(';', Fields);
        end;
  if Length(Fields) > 0 then List.Add(Fields);
end;

function TCustomRecordset.GetIndexName: string;
begin
  Result := FRecordset.Index;
end;

function TCustomRecordset.GetParamCount: Word;
begin
  Result := FParams.Count;
end;

function TCustomRecordset.GetPreviousRecord(
  RecordBuffer: PChar): TGetResult;
var PrevState  :  TDataSetState;
    Again      :  Boolean;
    Buffer     :  PChar;
    i,n        :  Integer;
begin
  repeat
    Buffer := RecordBuffer;
    Again := FALSE;
    Result := grBOF;
    if FRecordset.BOF then Exit;
    if not FRecordset.Supports(adMovePrevious) then Exit;
    if not FRecordset.EOF and (FRecordset.EditMode <> adEditNone) then Exit;
    FRecordset.MovePrevious;
    if (FRecordSet.BOF) then
       begin
         if (not FRecordSet.EOF) then
            FRecordSet.MoveNext;
       end else Result := grOK;
    if (Result = grOK) and ((Buffer <> nil) or (Assigned(FOnFilterRecord) and Filtered)) then
       begin
         if Buffer = nil then Buffer := AllocRecordBuffer;
         InternalInitRecord(Buffer);
         for i := 0 to Fields.Count - 1 do
         if Fields [i].FieldKind = fkData then
            begin
              n := Fields [i].FieldNo - 1;
              try
                if  (roHugeBlobs in FOptions)
                and (FRecordset.Fields[n].Attributes and adFldLong = adFldLong)
                then TRecData(pointer(Buffer)^)[i].Data := Null
                else TRecData(pointer(Buffer)^)[i].Data := FRecordset.Fields[n].Value;
              except TRecData(pointer(Buffer)^)[i].Data := Null; end;
            end;
         GetRecordBookmark(Buffer, gmCurrent, TRUE);
         if Assigned(FOnFilterRecord) and Filtered then
            begin
              PrevState :=  SetTempState(dsFilter);
              FFilteredRecord := Buffer;
              try
                Again := TRUE;
                FOnFilterRecord(Self, Again);
              finally
                Again := not Again;
                if RecordBuffer = nil then FreeRecordBuffer(Buffer);
                RestoreState(PrevState);
              end;
            end;
       end;
  until not Again;
  if (Result <> grOK) and (RecordBuffer <> nil) then InternalInitRecord(RecordBuffer);
end;

function TCustomRecordset.GetRecNo: Longint;
var RecBuf: PChar;
begin
  Result := -1;
  if State = dsCalcFields
     then RecBuf := CalcBuffer
     else RecBuf := ActiveBuffer;
  if RecBuf <> nil then Result := PRecInfo(RecBuf + FRecInfoOfs).RecNo;
end;

function TCustomRecordset.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  CheckLocked;
  Result := grOK;
  InternalInitRecord(Buffer);
  case GetMode of
    gmNext:
        if  (not FRecordset.Supports(adMovePrevious))
        and (FFirstRecord) then
          begin
            FFirstRecord := FALSE;
            Result := GetCurrent_NextRecord(TRUE, Buffer);
          end
        else Result := GetCurrent_NextRecord(FALSE, Buffer);
    gmPrior: Result := GetPreviousRecord(Buffer);
    gmCurrent:
        begin
          if (FRecordSet.BOF) then Result := grBOF;
          if (FRecordSet.EOF) then Result := grEOF;
          if Result = grOK then Result := GetCurrent_NextRecord(TRUE, Buffer);
        end;
  end;
  if Result = grOK then GetCalcFields(Buffer);
end;

function TCustomRecordset.GetRecordBookmark(Buffer: PChar;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;
  PRecInfo(Buffer + FRecInfoOfs)^.Bookmark := Unassigned;
  if (FRecordSet.BOF) then Result := grBOF;
  if (FRecordSet.EOF) then Result := grEOF;
  if Result = grOK then
     with PRecInfo(Buffer + FRecInfoOfs)^ do
          begin
            BookmarkFlag := bfCurrent;
            if (SupportsBookmarks) then
               begin
                 VariantClear(Bookmark);
                 try
                   Bookmark := FRecordSet.Bookmark
                 except
                   VariantClear(Bookmark);
                 end;
               end
                else Bookmark := Unassigned;
            if  (FRecordset.Supports(adApproxPosition))
            and (DWORD(FRecordset.AbsolutePosition) < adPosEOF)
                then RecNo := FRecordset.AbsolutePosition
                else RecNo := -1;
          end;
end;

function TCustomRecordset.GetRecordCount: Integer;
begin
  Result := -1;
  if not IsCursorOpen then Exit;
  Result := FRecordSet.RecordCount;
end;

function TCustomRecordset.GetSource: string;
  function RemoveChars(Text: string): string;
  var n: integer;
  begin
    for n := 1 to Length(Text) do if Ord(Text[n]) < 32 then Text[n] := #32;
    Result := Text;
  end;
begin
  case SourceType of
  stTable,stTableDirect: Result := FTableName;
  stStoredProc: Result := FStoredProcName;
  stSchema: Result := FSchemaType;
  stText: Result := RemoveChars(FSQL.Text);
  stPersistedRecordset: Result := FFileName;
  end;
end;

procedure TCustomRecordset.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
  CheckLocked;
  if Append then if not FRecordset.EOF then FRecordset.MoveLast;
  InternalPost;
end;

procedure TCustomRecordset.InternalCancel;
begin
  if FRecordset.EOF or FRecordset.BOF then Exit;
  if (FRecordset.EditMode <> adEditNone)
     then
       begin
         CheckLocked;
         CursorPosChanged;
         FRecordset.CancelUpdate;
         UpdateCursorPos;
       end;
end;

procedure TCustomRecordset.InternalClose;
begin
  CheckLocked;
  if FNoRecords then Exit;
  if (not FNextRecordset) then
     begin
       if (SourceType = stSchema) or (FOldRecordset <> nil) then FRequery := FALSE;
       if FRequery then Exit;
       try
        if (FCommand <> nil) and (FCommand.State and adStateExecuting = adStateExecuting) then FCommand.Cancel;
       except end;
       try
         if (FRecordset <> nil) then
            begin
              if (FRecordset.State and adStateOpen = adStateOpen) then FRecordset.Close;
            end;
       except end;
       try
         if FOldRecordset <> nil then
         begin
           FRecordset := nil;
           FRecordset := FOldRecordset;
           FOldRecordset := nil;
         end;
       except end;
       if FQRDataLink <> nil then
       begin
         FQRDataLink.Free;
         FQRDataLink := nil;
       end;
       if FQRDataSource <> nil then
          begin
            FQRDataSource.Free;
            FQRDataSource := nil;
          end;
     end;
  BindFields(FALSE);
  if DefaultFields then DestroyFields;
end;

procedure TCustomRecordset.InternalDelete;
begin
  CheckLocked;
  try
    FRecordSet.Delete(adAffectCurrent);
  except
    InternalCancel;
    raise;
  end;
  try
    FRecordset.Update(EmptyParam, EmptyParam);
  except
    InternalCancel;
    raise;
  end;
  GetRecord(ActiveBuffer, gmNext, TRUE);
end;

procedure TCustomRecordset.InternalEdit;
begin
  if FDSLocks > 0 then Exit;
  UpdateCursorPos;
  GetRecord(ActiveBuffer, gmCurrent, TRUE);
end;

procedure TCustomRecordset.InternalFirst;
begin
  CheckLocked;
  if FRecordset.BOF then Exit;
  if not FRecordset.EOF and (FRecordset.EditMode <> adEditNone) then Exit;
  try
    FRecordset.MoveFirst;
  except
    Variant(FRecordset).Requery;
  end;
  if not FRecordSet.Supports(adMovePrevious) then
     begin
       FFirstRecord := TRUE;
       Exit;
     end;
  if not FRecordset.BOF then FRecordSet.MovePrevious;
end;

procedure TCustomRecordset.InternalGotoBookmark(Bookmark: Pointer);
begin
  CheckLocked;
  if VarType(Variant(Bookmark^)) = varEmpty then Exit;
  if (SupportsBookmarks) then
     begin
       try
         FRecordSet.Bookmark:=OleVariant(Bookmark^);
       except DatabaseError(SRecordNotFound, Self); end;
     end
end;

procedure TCustomRecordset.InternalInitFieldDefs;
var i,j       : integer;
    FType     : TFieldType;
    FSize     : Word;
    FRequired : Boolean;
    FReadOnly : Boolean;
    FFieldDef : TFieldDef;
    FTempOpen : Boolean;
begin
  FTempOpen := FALSE;
  if  (csDesigning in ComponentState)
  and (not (csReading in ComponentState))
  and (not IsCursorOpen) then
     try
       InternalOpen;
       FTempOpen := TRUE;
     except
       InternalClose;
     end;
  FieldDefs.Clear;
  for i := 0 to FRecordSet.Fields.Count - 1 do
      begin
        case FRecordSet.Fields [i].Type_ of
        adBinary, adVarBinary, adArray or adUnsignedTinyInt, adArray or adTinyInt:
                if FRecordSet.Fields [i].Attributes and adFldFixed = adFldFixed
                   then FType := ftBytes else FType := ftVarBytes;
        adBigInt: FType := ftLargeInt;
        adLongVarBinary: FType := ftBlob;
        adBoolean: FType := ftBoolean;
        adBSTR: FType := ftWideString;
        adWChar: FType := ftWideString;
        adVarWChar: FType := ftWideString;
        {$IFDEF VCL50}
        adGUID: FType := ftGUID;
        {$ELSE}
        adGUID: FType := ftString;
        {$ENDIF}
        adChar: FType := ftString;
        adVarChar: FType := ftString;
        adLongVarChar: FType := ftMemo;
        adLongVarWChar: FType := ftMemo;
        adCurrency: FType := ftBCD;
        adDate: FType := ftDateTime;
        adDBDate: FType := ftDate;
        adDBTime: FType := ftTime;
        adDBTimeStamp: FType := ftDateTime;
        adFileTime: FType := ftDateTime;
        adDecimal: FType := ftFloat;
        adVarNumeric:  FType := ftFloat;
        adDouble: FType := ftFloat;
        adEmpty: FType := ftUnknown;
        adError: FType := ftInteger;
        adIDispatch: FType := ftInteger;
        adInteger: FType := ftInteger;
        adIUnknown: FType := ftInteger;
        adNumeric: FType := ftFloat;
        adSingle: FType := ftFloat;
        adSmallInt: FType := ftSmallInt;
        adTinyInt: FType := ftSmallInt;
        adUnsignedBigInt: FType := ftLargeInt;
        adUnsignedInt: FType := ftInteger;
        adUnsignedSmallInt: FType := ftWord;
        adUnsignedTinyInt: FType := ftWord;
        adVariant: FType := ftVariant;
        else FType := ftUnknown;
        end;
        {$IFDEF VCL40}
        if (FType = ftWideString) and not (roWideString in FOptions) then
           FType := ftString;
        {$ENDIF}
        if FRecordSet.Fields [i].Attributes and adFldIsNullable = adFldIsNullable
           then FRequired := FALSE else FRequired := TRUE;
        if FRequired then
           for j := 0 to FRecordSet.Fields [i].Properties.Count -1 do
               if (UpperCase(FRecordSet.Fields [i].Properties [j].Name) = 'ISAUTOINCREMENT') then
                  begin
                    try
                      if VarAsType(FRecordSet.Fields [i].Properties [j].Value, varBoolean) = TRUE then FRequired := FALSE;
                    except end;
                  end;
        if (FRecordset.Fields[i].Attributes and adFldUnknownUpdatable <> adFldUnknownUpdatable)
           and
           (FRecordset.Fields[i].Attributes and adFldUpdatable <> adFldUpdatable)
           then FReadOnly := TRUE else FReadOnly := FALSE;
        if FType in [ftVarBytes, ftWideString, ftFixedChar, ftString, ftBytes]
           then FSize := FRecordSet.Fields [i].DefinedSize
           else if FType = ftBCD
                   then FSize := 4
                   else FSize := 0;
        if FRecordSet.Fields [i].Type_ = adGuid then FSize := 38;
        if (FRecordSet.Fields [i].Name = '')
           then FFieldDef := TFieldDef.Create(FieldDefs, 'Field' + IntToStr(i), FType, FSize, FRequired, i)
           else FFieldDef := TFieldDef.Create(FieldDefs, FRecordSet.Fields [i].Name, FType, FSize, FRequired, i);
        FFieldDef.Precision := FRecordSet.Fields [i].Precision;
        if FReadOnly then FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
     end;
  if FTempOpen then InternalClose;
end;

procedure TCustomRecordset.InternalLast;
begin
  CheckLocked;
  if FRecordSet.EOF then Exit;
  if not FRecordset.BOF and (FRecordset.EditMode <> adEditNone) then Exit;
  if not FRecordset.Supports(adMovePrevious) then
     DatabaseError(SCannotMoveLast, Self);
  FRecordSet.MoveLast;
  if not FRecordset.EOF then FRecordSet.MoveNext;
end;

procedure TCustomRecordset.InternalOpen;
var n               : Integer;
    FDataType       : Integer;
    FDirection      : Integer;
    FDataSet        : TDataSet;
    Requery         : Boolean;
    ExecuteOnly     : Boolean;
    AsyncExecute    : Integer;
    AsyncFetch      : Integer;
    FCValues        : variant;
    FResult         : variant;
    FSchemaIndex    : Integer;
    TempParams      : TParams;
    TempParam       : TParam;
    ParamAttrs      : Integer;
    FADOPrepared    : Variant;
    FRowset         : IUnknown;
    FRecordsetCS    : IADORecordsetConstruction;
    FDBAsynchNotify : TDBAsynchNotify;
    FParameter      : IADOParameter;
    DiffParamName   : Boolean;
begin
  AsyncFetch := 0;
  Requery := FRequery;
  FRequery := FALSE;
  ExecuteOnly := FExecuteOnly;
  FExecuteOnly := FALSE;
  FWaitingForData := FALSE;
  if roAsyncFetch in FOptions then AsyncFetch := adAsyncFetch;
  if roAsyncFetchNonBlocking in FOptions then AsyncFetch := AsyncFetch or adAsyncFetchNonBlocking;
  if not FNextRecordset then
     begin
       if (FMasterFields <> '') and (FDetailFields <> '') and (GetDS() <> nil)
          then RefreshParams();
       FFirstRecord := TRUE;
       FLastCurrentRecord := -1;
       if SourceType = stSchema then Requery := FALSE;
       if  (roAsyncExecute in Options)
       and (not(csDesigning in ComponentState))
          then AsyncExecute := adAsyncExecute
          else AsyncExecute := 0;
       if (SourceType = stTableDirect) or (SourceType = stSchema) then AsyncExecute := 0;
       if (SourceType <> stPersistedRecordset) then
          if FConnection = nil
             then DatabaseError(SNoConnection, Self)
             else if not FConnection.Active then FConnection.Open;
       if not Requery then
          begin
            if FCommand <> nil then
               begin
                 try
                   if (FCommand <> nil) and (FCommand.State and adStateExecuting = adStateExecuting) then FCommand.Cancel;
                 except
                 end;
                 if (not (roPrepared in FOptions)) then FCommand := nil;
               end;
            if (FRecordset <> nil) and (FRecordset.State and adStateOpen = adStateOpen)
               then FRecordset.Close;
            if (SourceType <> stTableDirect) and (SourceType <> stPersistedRecordset) then
               begin
                 if FCommand = nil then
                    begin
                      try
                        FCommand := CoCommand.Create;
                        FCommand.Set_ActiveConnection(FConnection.FConnection);
                        FCommand.CommandText := Params.ParseSQL(Source, FALSE);
                        FCommand.CommandType := SourceTypeEnum[SourceType];
                        FParameter := coParameter.Create;
                        FParameter.Name := 'Param1';
                        FParameter.Type_ := adInteger;
                        FCommand.Parameters.Append(FParameter);
                        FCommand.Parameters.Delete(0);
                        FParameter := nil;
                        if  (DispGetPropValue(FCommand, 'Prepared', FADOPrepared) = S_OK)
                        and (not VarIsNull(FADOPrepared))
                        then
                          begin
                            if (Boolean(FADOPrepared) <> (roPrepared in FOptions)) then
                               FCommand.Prepared := (roPrepared in FOptions);
                          end
                        else if (roPrepared in FOptions) then
                                DatabaseError(SPreparedNotSupported, Self);
                      except
                        FCommand := nil;
                        raise;
                      end;
                    end;
                 if FCommandTimeout = tmSuperior
                    then begin
                           if FCommand.CommandTimeout <> FConnection.FConnection.CommandTimeout then
                              FCommand.CommandTimeout := FConnection.FConnection.CommandTimeout;
                         end
                    else if FCommand.CommandTimeout <> FCommandTimeout then
                            FCommand.CommandTimeout := FCommandTimeout;
               end {:-- if (SourceType <> stTableDirect) and (SourceType <> stPersistedRecordset)}
               else
                 FCommand := nil;
          end;
       if ((SourceType = stText) or (SourceType = stStoredProc)) then
          begin
            if (FDataLink.DataSource <> nil) and (FParams.Count > 0) then
               begin
                 FDataSet := FDataLink.DataSource.DataSet;
                 if (FDataSet <> nil) and (FDataSet.Active) then
                    begin
                      FDataSet.FieldDefs.Update;
                      for n := 0 to FParams.Count - 1 do
                      with FParams[n] do
                          if not Bound then
                             begin
                               AssignField(FDataSet.FieldByName(Name));
                               Bound := False;
                             end;
                    end;
               end;
            if SourceType = stText then TempParams := TParams.Create else TempParams := FParams;
            try
              if SourceType = stText then TempParams.ParseSQL(Source, TRUE);
              while FCommand.Parameters.Count > TempParams.Count do
                    FCommand.Parameters.Delete(0);
              DiffParamName := FALSE;
              if (SourceType = stStoredProc) then
                 for n := 0 to FCommand.Parameters.Count - 1 do
                     if FCommand.Parameters[n].Name <> TempParams [n].Name then
                        DiffParamName := TRUE;
              if DiffParamName then
                 while FCommand.Parameters.Count > 0 do
                       FCommand.Parameters.Delete(0);
              while FCommand.Parameters.Count < TempParams.Count do
                    begin
                      FParameter := coParameter.Create;
                      if SourceType = stStoredProc
                         then FParameter.Name := TempParams [FCommand.Parameters.Count].Name 
                         else FParameter.Name := 'Param1';
                      FParameter.Type_ := adInteger;
                      FCommand.Parameters.Append(FParameter);
                      FParameter := nil;
                    end;
            for n := 0 to TempParams.Count - 1 do
                  begin
                    if SourceType = stText then TempParam := FParams.ParamByName(TempParams [n].Name)
                                           else TempParam := TempParams [n];
                    if TempParam = nil then DatabaseError('Param not found', Self);
                    case TempParam.ParamType of
                    ptInput: FDirection := adParamInput;
                    ptOutput: FDirection := adParamOutput;
                    ptInputOutput: FDirection := adParamInputOutput;
                    ptResult: FDirection := adParamReturnValue;
                    else FDirection := adParamInput;
                    end;
                    FDataType := adEmpty;
                    case TempParam.DataType of
                    ftUnknown: DatabaseError(Format(SBadParamDataType, [TempParam.Name]), Self);
                    ftString: FDataType := adVarChar;
                    ftSmallint:
                      if ptParamSigned in TempParam.FAttributes
                         then FDataType := adSmallInt
                         else FDataType := adUnsignedSmallInt;
                    ftInteger:
                      if ptParamSigned in TempParam.FAttributes
                         then FDataType := adInteger
                         else FDataType := adUnsignedInt;
                    ftWord: FDataType := adUnsignedSmallInt;
                    ftBoolean: FDataType := adBoolean;
                    ftFloat: FDataType := adDouble;
                    ftCurrency: FDataType := adCurrency;
                    ftBCD: FDataType := adCurrency;
                    ftDate: FDataType := adDBDate;
                    ftTime: FDataType := adDBTime;
                    ftDateTime: FDataType := adDBTimeStamp;
                    ftBytes: FDataType := adBinary;
                    ftVarBytes: FDataType := adVarBinary;
                    ftAutoInc: FDataType := adUnsignedInt;
                    ftBlob: FDataType := adLongVarBinary;
                    ftMemo: FDataType := adLongVarChar;
                    ftGraphic: FDataType := adLongVarBinary;
                    ftFmtMemo: FDataType := adLongVarChar;
                    ftTypedBinary: FDataType := adLongVarBinary;
                    ftFixedChar: FDataType := adChar;
                    ftWideString: FDataType := adVarWChar;
                    ftLargeint: FDataType := adBigInt;
                    ftVariant: FDataType := adVariant;
                    else FDataType := adEmpty;
                    end;
                    if  (TempParam.DataType in [ftString, ftBytes, ftVarBytes, ftBlob, ftMemo,
                         ftFixedChar, ftWideString])
                    and (TempParam.Size = 0)
                    then TempParam.Size := 255;
                    FCommand.Parameters[n].Direction := FDirection;
                    FCommand.Parameters[n].Type_ := FDataType;
                    FCommand.Parameters[n].Size := TempParam.Size;
                    FCommand.Parameters[n].NumericScale := TempParam.Scale;
                    FCommand.Parameters[n].Precision := TempParam.Precision;
                    ParamAttrs := 0;
                    if ptParamSigned in TempParam.FAttributes then
                       ParamAttrs := ParamAttrs or adParamSigned;
                    if ptParamNullable in TempParam.FAttributes then
                       ParamAttrs := ParamAttrs or adParamNullable;
                    if ptParamLong in TempParam.FAttributes then
                       ParamAttrs := ParamAttrs or adParamLong;
                    FCommand.Parameters[n].Attributes := ParamAttrs;
                    FCommand.Parameters[n].Value := TempParam.Value;
                    if VarIsEmpty(FCommand.Parameters[n].Value) then FCommand.Parameters[n].Value := Null;
                  end;
            finally
              if SourceType = stText then TempParams.Free;
            end;
          end;
       if ExecuteOnly then
          begin
            Variant(FCommand).Execute(,,FCommand.CommandType or adExecuteNoRecords or AsyncExecute)
          end
            else if Requery then Variant(FRecordSet).Requery
                    else
                      if SourceType = stSchema then
                         begin
                           FSchemaIndex := SchemaItemByName(FSchemaType);
                           if FSchemaIndex = -1 then DatabaseError(SInvalidSchemaType, Self);
                           if ParamCount > 0 then
                              begin
                                FCValues := VarArrayCreate([0, Params.Count - 1], varVariant);
                                for n := 0 to Params.Count - 1 do FCValues [n] := Params [n].Value;
                              end;
                           FOldRecordset := FRecordset;
                           FRecordset := nil;
                           try
                             if ParamCount > 0
                                then FResult := Variant(FConnection.FConnection).OpenSchema(SchemaItems [FSchemaIndex].Index, FCValues)
                                else FResult := Variant(FConnection.FConnection).OpenSchema(SchemaItems [FSchemaIndex].Index);
                             if VarType(FResult) <> varDispatch then DatabaseError(SInvalidSchemaResult, Self);
                           except
                             VarClear(FResult);
                             FRecordset := FOldRecordset;
                             FOldRecordset := nil;
                           raise;
                           end;
                           FRecordset := IADORecordset(IDispatch(FResult));
                           VarClear(FResult);
                         end
                           else
                             begin
                               case SourceType of
                                 stTableDirect : begin
                                                   if FEventsConn <> -1 then InterfaceDisconnect(FRecordset, IADORecordsetEvents, FEventsConn);
                                                   FEventsConn := -1;
                                                   Variant(FRecordSet).Open(Source,
                                                        FConnection.FConnection,
                                                        _CursorTypeEnum[CursorType],
                                                        _LockTypeEnum[LockType],
                                                        adCmdTableDirect or AsyncFetch);
                                                  end;
                                 stPersistedRecordset
                                               : begin
                                                   if FEventsConn <> -1 then InterfaceDisconnect(FRecordset, IADORecordsetEvents, FEventsConn);
                                                   FEventsConn := -1;
                                                   Variant(FRecordSet).Open(Source,,
                                                        _CursorTypeEnum[CursorType],
                                                        _LockTypeEnum[LockType],
                                                        adCmdFile or AsyncFetch);
                                                  end;
                                 else Variant(FRecordSet).Open(FCommand, ,_CursorTypeEnum[CursorType], _LockTypeEnum[LockType], AsyncExecute or AsyncFetch)
                               end;
                             end;
     end;
  if ((FCommand <> nil) and (FCommand.State and adStateExecuting = adStateExecuting))
     or (FRecordset.State and adStateExecuting = adStateExecuting) then
     begin
       FWaitingForData := TRUE;
       Exit;
     end;
  RetrieveParamsFromProvider;
  if not IsCursorOpen then Exit;
  if  (FRecordset.QueryInterface(IID_IADORecordsetConstruction, FRecordsetCS) = S_OK)
  and (FRecordsetCS.Get_Rowset(FRowset) = S_OK)
  and (FRowset <> nil) then
     begin
       FDBAsynchNotify := TDBAsynchNotify.Create(Self);
       if not IDBAsynchNotifyConnect(FRowset, IID_IDBAsynchNotifyVt,
                  FDBAsynchNotify, n) then
              FDBAsynchNotify.Destroy;
       FRowset := nil;
       FRecordsetCS := nil;
     end;
  if DWORD(FRecordset.MarshalOptions) <> MarshalOptionsEnum[FMarshalOptions]
     then FRecordset.MarshalOptions := MarshalOptionsEnum[FMarshalOptions];
  if FCacheSize <> FRecordset.Get_CacheSize then
     FRecordset.Set_CacheSize(FCacheSize);
  if ( FilterAsInteger(Filter) <> -1 )
       and (Filter <> ftFilterNone)
       and (Filtered)
       then FRecordset.Filter := FilterTypeEnum[FilterAsInteger(Filter)];
  if not FRecordset.BOF then
     begin
       if not FRecordSet.Supports(adMovePrevious)
          then FFirstRecord := TRUE
          else FRecordSet.MovePrevious;
     end;
  if Requery then Exit;
  FieldDefs.Updated := False;
  FieldDefs.Update;
  if (roQuickReport in FOptions) and (not FRecordset.Supports(adMovePrevious)) then
     begin
       if FQRDataLink <> nil then
          begin
            FQRDataLink.Free;
            FQRDataLink := nil;
          end;
       if FQRDataSource <> nil then
          begin
            FQRDataSource.Free;
            FQRDataSource := nil;
          end;
       FQRDataSource := TDataSource.Create(Self);
       FQRDataSource.DataSet := Self;
       FQRDataLink := TDataLink.Create;
       FQRDataLink.BufferCount := 2;
       FQRDataLink.DataSource := FQRDataSource;
     end;
  if DefaultFields then CreateFields;
  BindFields(True);
  inherited InternalOpen;
end;

procedure TCustomRecordset.InternalPost;
var FDataBefore: Boolean;
begin
  CheckLocked;
  if FRecordset.EOF then
     if (not FRecordset.BOF) and (FRecordset.Supports(adMovePrevious)) then
        begin
          FRecordset.MovePrevious;
        end;
  if FRecordset.BOF then
     begin
       if (not FRecordset.EOF) then FRecordset.MoveNext;
     end
       else
         if  (not SupportsBookmarks)
         and (FRecordset.Supports(adMovePrevious)) then
             begin
               FRecordset.MovePrevious;
               FRecordset.MoveNext;
             end;
  if (not FRecordset.BOF) and (not FRecordset.EOF)
     then FDataBefore := TRUE
     else FDataBefore := FALSE;
  if (State <> dsEdit) and (FRecordset.EOF or FRecordset.BOF or (FRecordset.EditMode = adEditNone)) then
     begin
       try
         FRecordset.AddNew(EmptyParam, EmptyParam);
       except
         FRecordset.CancelUpdate;
         raise;
       end;
     end;
  try
    WriteChanges(ActiveBuffer);
  except
    try
      FRecordset.CancelUpdate;
    except end;
    raise;
  end;
  try
    FRecordset.Update(EmptyParam, EmptyParam);
  except
    FRecordset.CancelUpdate;
    if FDataBefore then
       if SupportsBookmarks then
          begin
            CursorPosChanged;
            try
              UpdateCursorPos;
            except
            end;
          end else FRecordset.Move(0, EmptyParam);
    raise;
  end;
  if FRecordset.EOF then Exit;
  if FRecordset.Supports(adBookmark) then
     begin
       if GetRecordBookmark(ActiveBuffer, gmCurrent, TRUE) = grOK then
          begin
            if FRecordset.Supports(adMovePrevious) then FRecordset.MoveFirst;
            CursorPosChanged;
            try
             UpdateCursorPos;
            except
              Application.HandleException(Self);
            end;
          end;
     end else if (FRecordSet.Supports(adMovePrevious)) then
                 begin
                   if not FRecordset.EOF
                      then begin
                             FRecordset.MoveNext;
                             if  (not FRecordset.BOF) and (not FRecordset.EOF)
                                 then FRecordset.MovePrevious;
                           end
                      else if not FRecordset.BOF
                              then begin
                                     FRecordset.MovePrevious;
                                     if not FRecordset.EOF then FRecordset.MoveNext;
                                   end;
                 end;
end;

procedure TCustomRecordset.InternalRefresh;
begin
  CheckLocked;
  if not isCursorOpen then Exit;
  if FRequery or (roRequery in FOptions) then
        begin
          FRequery := TRUE;
          if (csDesigning in ComponentState) then
             begin
               FRequery := FALSE;
               Close;
               Open;
             end else
                 begin
                   FreeFieldBuffers;
                   ClearBuffers;
                   InternalClose;
                   InternalOpen;
                   FFirstRecord := FALSE;
                   if FRecordset.EOF then InternalInitRecord(ActiveBuffer);
                 end;
          Exit;
        end;
  if FRecordset.Supports(adResync) and (roResync in FOptions) then
     try
       FRecordset.Resync(adAffectAll, adResyncAllValues);
     except end;
end;

procedure TCustomRecordset.InternalSetToRecord(Buffer: PChar);
begin
  CheckLocked;
  if SupportsBookmarks then
     InternalGotoBookmark(@PRecInfo(Buffer + FRecInfoOfs).Bookmark);
end;

function TCustomRecordset.IsCursorOpen: Boolean;
begin
  try
    if (FRecordSet = nil)
    or (FRecordSet.State and adStateOpen <> adStateOpen)
    then Result := FALSE
    else Result := TRUE;
  except
    Result := FALSE;
  end;
end;

function TCustomRecordset.IsSequenced: Boolean;
begin
  Result := FALSE;
  //Always returns FALSE to avoid records counting by TDBGrid
end;

procedure TCustomRecordset.Loaded;
begin
  inherited;
end;

function TCustomRecordset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions;
  Origin: TLocateOrigin): Boolean;
var SearchString : string;
    List         : TStringList;
    Bookmark     : OleVariant;
    i            : Integer;
    Val          : string;
    EqualSign    : string;
    Pattern      : string;
    Offs         : Integer;
    Direction    : DWORD;
    Error        : Boolean;
    Found        : Boolean;
    Again        : Boolean;
begin
  CheckLocked;
  Result := FALSE;
  if not SupportsBookmarks then DatabaseError(SNoBookmarks, Self);
  CheckBrowseMode;
  DoBeforeScroll;
  if loPartialKey in Options then
     begin
       EqualSign := ' LIKE ';
       Pattern := '*';
     end else
           begin
             EqualSign := ' = ';
             Pattern := '';
           end;
  if Origin = loFromBeginningForward then
     begin
       if not FRecordset.BOF then FRecordset.MoveFirst;
       Offs := 0;
       Direction := adSearchForward;
     end;
  if Origin = loFromCurrentForward then
     begin
       UpdateCursorPos;
       Offs := 1;
       Direction := adSearchForward;
     end;
  if Origin = loFromCurrentBackward then
     begin
       UpdateCursorPos;
       Offs := 1;
       Direction := adSearchBackward;
     end;
  if Origin = loFromEndBackward then
     begin
       if not FRecordset.EOF then FRecordset.MoveLast;
       Offs := 0;
       Direction := adSearchBackward;
     end;
  repeat
    Again := FALSE;
    if KeyFields = '' then
       begin
         if FilterAsInteger(Filter) = -1 then
            begin
              SearchString := Filter;
              try
                Variant(FRecordset).Find(WideString(SearchString), Offs, Direction);
              except
                UpdateCursorPos;
                Exit;
              end;
            end else
                  begin
                    UpdateCursorPos;
                    Exit;
                  end;
       end else
           begin
             SearchString := '';
             List := TStringList.Create;
             GetFields(KeyFields, List);
             if (not VarIsArray(KeyValues)) then
                begin
                  if List.Count <> 1 then
                     begin
                       List.Free;
                       UpdateCursorPos;
                       Exit;
                     end;
                  if VarType(KeyValues) = varNull then
                     SearchString := '[' + List [0] + '] = Null'
                     else
                     SearchString := '[' + List [0] + ']' + EqualSign + #39 + Pattern + VarAsType(KeyValues, varString) + Pattern + #39;
                  List.Free;
                  try
                    Variant(FRecordset).Find(WideString(SearchString), Offs, Direction);
                  except
                    UpdateCursorPos;
                    Exit;
                  end;
                end
                else
                  begin
                    if (List.Count <> VarArrayHighBound(KeyValues, 1) - VarArrayLowBound(KeyValues, 1) + 1) then
                       begin
                         List.Free;
                         UpdateCursorPos;
                         Exit;
                       end;
                    try
                      if VarType(KeyValues [0]) = varNull then
                         SearchString := '[' + List [0] + '] = Null'
                         else begin
                                Val := VarAsType(KeyValues [0], varString);
                                SearchString := '[' + List [0] + ']' + EqualSign + #39 + Pattern + Val + Pattern + #39;
                                Val := '';
                              end;
                      Variant(FRecordset).Find(WideString(SearchString), Offs, Direction);
                      Found := FALSE;
                      if (FRecordset.BOF) or (FRecordset.EOF)
                         then Error := TRUE
                         else Error := FALSE;
                      while (not Found) and (not Error) do
                            begin
                              Bookmark := FRecordset.Bookmark;
                              for i := 0 to List.Count - 1 do
                                  begin
                                    if VarType(KeyValues [i]) = varNull then
                                       SearchString := '[' + List [i] + '] = Null'
                                       else begin
                                              Val := VarAsType(KeyValues [i], varString);
                                              SearchString := '[' + List [i] + ']' + EqualSign + #39 + Pattern + Val + Pattern + #39;
                                              Val := '';
                                            end;
                                    try
                                      Variant(FRecordset).Find(WideString(SearchString), 0, Direction);
                                    except
                                      Error := TRUE;
                                    end;
                                  end;
                              if (FRecordset.BOF) or (FRecordset.EOF)
                                 then Error := TRUE
                                 else if FRecordset.CompareBookmarks(FRecordset.Bookmark,
                                                    Bookmark) = adCompareEqual then Found := TRUE;
                            end;
                    except
                      List.Free;
                      UpdateCursorPos;
                      Exit;
                    end;
                  end;
           end;
    if (not FRecordset.BOF) and (not FRecordset.EOF) then
       if not CheckCurrentRecord then
          begin
            Offs := 1;
            Again := TRUE;
          end;
  until (not Again) or (FRecordset.BOF) or (FRecordset.EOF);
  if (FRecordset.EOF) or (FRecordset.BOF) then
     begin
       UpdateCursorPos;
     end else
           begin
             Result := TRUE;
             CursorPosChanged;
             Resync([rmExact, rmCenter]);
             DoAfterScroll;
           end;
end;

function TCustomRecordset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := Locate(KeyFields, KeyValues, Options, loFromBeginningForward);
end;

function TCustomRecordset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var FCurrentBookmark : TBookmark;
    List             : TStringList;
    i                : Integer;
begin
  Result := FALSE;
  if not SupportsBookmarks then DatabaseError(SNoBookmarks, Self);
  FCurrentBookmark := GetBookmark;
  List := TStringList.Create;
  try
    Result := Locate(KeyFields, KeyValues, [], loFromBeginningForward);
    if Result then
       begin
         GetFields(ResultFields, List);
         Result := VarArrayCreate([0, List.Count - 1], varVariant);
         for i := 0 to List.Count - 1 do
             Result := FieldByName(List [i]).AsVariant;
       end;
  finally
    List.Free;
    try
      GotoBookmark(FCurrentBookmark);
    finally
      FreeBookmark(FCurrentBookmark);
    end;
  end;
end;

function TCustomRecordset.NextRecordset: Boolean;
var FNewRecordset: IADORecordset;
begin
  CheckLocked;
  FNewRecordset := nil;
  Result := TRUE;
  if FRecordset = nil then DatabaseError(SInvalidDataSet, Self);
  FNextRecordset := TRUE;
  try
    Close;
    FNewRecordset := IADORecordset(IDispatch(Variant(FRecordset).NextRecordset));
    if FNewRecordset = nil then
       begin
         RetrieveParamsFromProvider;
         DatabaseError(SNoMoreRecordsets, Self);
       end else
             begin
               FRecordset := FNewRecordset;
             end;
    Open;
  except
    Result := FALSE;
  end;
  FNextRecordset := FALSE;
end;

procedure TCustomRecordset.OpenCursor(InfoQuery: Boolean);
begin
  inherited OpenCursor(InfoQuery);
  if (not InfoQuery) and (not IsCursorOpen) then
     begin
         FNoRecords := TRUE;
         CloseCursor;
     end;
end;

function TCustomRecordset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TCustomRecordset.RefreshParams;
var
  DataSet: TDataSet;
  MasterFields: TStringList;
  DetailFields: TStringList;
  i: Integer;
  FilterString: string;
  FRefresh: Boolean;
  FValue: string;
  FFilter: string;
begin
  CheckLocked;
  FilterString := '';
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          if (SourceType = stTable) or (SourceType = stTableDirect) or (SourceType = stPersistedRecordset) then
             begin
               MasterFields := TStringList.Create;
               DetailFields := TStringList.Create;
               GetFields(FMasterFields, MasterFields);
               GetFields(FDetailFields, DetailFields);
               if (MasterFields.Count > 0)
               and (DetailFields.Count = MasterFields.Count)
               then begin
                      for i := 0 to DetailFields.Count - 1 do
                          if DataSet.FindField(MasterFields [i]) <> nil then
                             begin
                               if DataSet.FindField(MasterFields [i]).IsNull
                                  then FValue := 'Null'
                                  else FValue := #39 + DataSet.FindField(MasterFields [i]).AsString + #39;
                               if FilterString <> '' then FilterString :=
                                                          FilterString +
                                                          ' AND ';
                               FilterString := FilterString + '[' +
                               DetailFields [i] + ']' +
                               ' = ' + FValue;
                             end;
                      if FilterString <> '' then FilterString := '('
                                                 + FilterString + ')';
                    end;
               MasterFields.Free;
               DetailFields.Free;
             end else
                 begin
                   if Active then
                      begin
                        FRequery := TRUE;
                        Refresh;
                      end;
                 end;
        end;
    end;
    if Filtered and (FilterAsInteger(Filter) = -1) then
       if FilterString = '' then FilterString := Filter
                            else FilterString := FilterString + ' AND ( '
                                 + Filter + ')';
    FRefresh := FALSE;
    FFilter := Filter;
    if (not Filtered) then FFilter := ftFilterNone;
    if (FilterAsInteger(Filter) = -1)
       or
        (
          (FilterAsInteger(Filter) <> -1)
          and (Filter = ftFilterNone)
        ) then
        begin
          FFilter := FilterString;
          if FFilter = '' then FFilter := ftFilterNone;
        end;
    if (
         ((VarType(FRecordset.Filter) = varInteger) or (VarType(FRecordset.Filter) = varByte))
         and (
               (FilterAsInteger(FFilter) = -1)
               or
               (FRecordset.Filter <> FilterTypeEnum[FilterAsInteger(FFilter)])
              )
        )
        or
       (
         ((VarType(FRecordset.Filter) <> varInteger) and (VarType(FRecordset.Filter) <> varByte))
         and (
               (FilterAsInteger(FFilter) <> -1)
               or
               (FRecordset.Filter <> FFilter)
              )
        ) then
       begin
         if (FilterAsInteger(FFilter) = -1)
            or (FFilter = ftFilterNone) then
            begin
              if FFilter = ftFilterNone
                 then FRecordset.Filter := FilterTypeEnum[FilterAsInteger(FFilter)]
                 else
                     FRecordset.Filter := WideString(FFilter);
              FRefresh := TRUE;
            end else if IsCursorOpen then
                        begin
                          FRecordset.Filter := FilterTypeEnum[FilterAsInteger(FFilter)];
                          FRefresh := TRUE;
                        end;
       end;
    if Assigned(FOnFilterRecord) and (Filtered) then FRefresh := TRUE;
    if FRefresh then
       if Active then
            begin
              CheckBrowseMode;
              First;
              if EOF then InternalInitRecord(ActiveBuffer);
              if FRequery then
                 Refresh
                 else begin
                        CheckBrowseMode;
                        UpdateCursorPos;
                        Resync([]);
                      end;
            end;
  finally
    EnableControls;
  end;
end;

procedure TCustomRecordset.Requery;
begin
  CheckLocked;
  FRequery := TRUE;
  Refresh;
end;

procedure TCustomRecordset.ResyncRecord;
begin
  CheckActive;
  CheckLocked;  
  CheckBrowseMode;
  UpdateCursorPos;
  if (FRecordSet.Supports(adResync)) then
     begin
       FRecordSet.Resync(adAffectCurrent, adResyncAllValues);
     end;
  GetRecord(ActiveBuffer, gmCurrent, TRUE);
  DataEvent(deDataSetChange, 0);
end;

procedure TCustomRecordset.RetrieveParamsFromProvider;
var n          : Integer;
begin
  if   ((SourceType = stText) or (SourceType = stStoredProc))
  and  (FCommand <> nil)
  and  (FCommand.State and adStateExecuting <> adStateExecuting)
  then
     for n := 0 to FCommand.Parameters.Count - 1 do
         if  (FCommand.Parameters [n].Direction <> adParamUnknown)
         and (FCommand.Parameters [n].Direction <> adParamInput)
         then ParamByName(FCommand.Parameters [n].Name).Value := FCommand.Parameters [n].Value;
end;

procedure TCustomRecordset.SaveRecordset(FileName: WideString;
  PersistFormat: TPersistFormat);
var FPersistFormat: PersistFormatEnum;
begin
  CheckActive;
  CheckLocked;
  CheckBrowseMode;
  if not IsCursorOpen then DatabaseError(SDataSetClosed);
  case PersistFormat of
    pfADTG: FPersistFormat := adPersistADTG;
    else FPersistFormat := adPersistXML;
  end;
  if FileName = ''
     then FRecordset.Save(EmptyParam, EmptyParam)
     else FRecordset.Save(FileName, FPersistFormat);
  First;
end;

procedure TCustomRecordset.Set_CacheSize(plCacheSize: Integer);
begin
  if plCacheSize < 1 then plCacheSize := 1; 
  FCacheSize := plCacheSize;
  if Active then FRecordset.Set_CacheSize(plCacheSize);
end;

procedure TCustomRecordset.Set_CursorLocation(
  plCursorLoc: TCursorLocation);
begin
  if Active then Active := FALSE;
  FRecordset.CursorLocation := CursorLocationEnum[plCursorLoc];
  FCursorLocation := plCursorLoc;
  if (plCursorLoc = clClient) and (csDesigning in ComponentState) and
     not (csReading in ComponentState) then Options := Options - [roResync];
end;

procedure TCustomRecordset.Set_CursorType(plCursorType: TCursorType);
begin
  if Active then Active := FALSE;
  FRecordset.CursorType := _CursorTypeEnum[plCursorType];
  FCursorType := plCursorType;
end;

procedure TCustomRecordset.Set_LockType(plLockType: TLockType);
begin
  if Active then Active := FALSE;
  FRecordset.LockType := _LockTypeEnum[plLockType];
  FLockType := plLockType;
  if (DWORD(FRecordset.LockType) <> _LockTypeEnum[ltReadOnly])
       and
     (roQuickReport in Options) then Options := Options - [roQuickReport];
end;

procedure TCustomRecordset.Set_MarshalOptions(peMarshal: TMarshalOptions);
begin
  if IsCursorOpen
     then FRecordset.Set_MarshalOptions(MarshalOptionsEnum[peMarshal]);
  FMarshalOptions := peMarshal;
end;

procedure TCustomRecordset.Set_MaxRecords(plMaxRecords: Integer);
begin
  if not (csReading in ComponentState) then Close;
  FRecordset.Set_MaxRecords(plMaxRecords);
end;

procedure TCustomRecordset.Set_Sort(Criteria: string);
var FCriteria  : string;
    i          : Integer;
begin
  CheckLocked;
  if Active then CheckBrowseMode;
  FCriteria := Criteria;
  i := Pos(';', FCriteria);
  while i > 0 do
        begin
          FCriteria [i] := ',';
          i := Pos(';', FCriteria);
        end;
  FRecordset.Set_Sort(FCriteria);
  if not Active then Exit;
  UpdateCursorPos;
  Resync([]);
end;

procedure TCustomRecordset.Set_Supports(plSupports: TSupportsOptions);
begin
  // read-only property
end;

procedure TCustomRecordset.SetActive(Value: Boolean);
begin
  if not (csReading in ComponentState)
     and (Active = Value)
     and (not Value)
     then CloseCursor
     else
       try
         inherited SetActive(Value);
       finally
         FNoRecords := FALSE;
       end;
end;

procedure TCustomRecordset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if not SupportsBookmarks then Exit;
  inherited SetBookmarkData(Buffer, Data);
end;

procedure TCustomRecordset.SetConnection(Connection: TConnection);
begin
  if not (csLoading in ComponentState) then SetActive(FALSE);
  FCommand := nil;
  if FConnection <> nil then FConnection.RemoveReference(Self);
  if Connection <> nil then Connection.AddReference(Self);
  FConnection := Connection;
end;

procedure TCustomRecordset.SetCurrentRecord(Index: Integer);
var FCurrentRecord : Integer;
    i              : Integer;
    FBuffer        : PChar;
    FBookmarkFlag  : TBookmarkFlag;
begin
  FCurrentRecord := CurrentRecord;
  FBuffer := Buffers[Index];
  FBookmarkFlag := GetBookmarkFlag(FBuffer);
  inherited SetCurrentRecord(Index);
  if (not SupportsBookmarks) and (Index <> FCurrentRecord)
     and (FBookmarkFlag <> bfBOF) and (FBookmarkFlag <> bfEOF)
     then
       begin
         CheckLocked;
         if FRecordset.EOF or FRecordset.BOF then FCurrentRecord := 0;
         if State = dsInsert then
               FCurrentRecord := FOldCurrentRecord;
         if FCurrentRecord = -1 then
            FCurrentRecord := FLastCurrentRecord;
         FLastCurrentRecord := Index;
         if FCurrentRecord = -1 then Exit;
         if FCurrentRecord = Index then Exit;
         if  (Index - FCurrentRecord > 0)
         and FRecordset.EOF then Exit;
         if  (Index - FCurrentRecord < 0)
         and FRecordset.BOF then Exit;
         if Index - FCurrentRecord > 0 then
            for i := 1 to Index - FCurrentRecord do GetCurrent_NextRecord(FALSE, nil);
         if Index - FCurrentRecord < 0 then
            for i := -1 downto Index - FCurrentRecord do GetPreviousRecord(nil);
         if (FRecordSet.EOF) then
            if (not FRecordSet.BOF) and (FRecordSet.Supports(adMovePrevious)) then
               FRecordSet.MovePrevious;
         if (FRecordSet.BOF) then
            if (not FRecordSet.EOF) then
               FRecordSet.MoveNext;
       end;
    FLastCurrentRecord := Index;
end;

procedure TCustomRecordset.SetDetailFields(Fields: string);
begin
  FDetailFields := Fields;
  if FMasterFields <> '' then RefreshParams;
end;

procedure TCustomRecordset.SetDS(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

procedure TCustomRecordset.SetFileName(FileName: string);
begin
  if not (csReading in ComponentState) then Close;
  FCommand := nil;
  FStoredProcName := '';
  FSchemaType := '';
  FTableName := '';
  TStringList(FSQL).OnChange := nil;
  FSQL.Clear;
  TStringList(FSQL).OnChange := SqlOnChange;
  FFileName := Trim(FileName);
  SourceType := stPersistedRecordset;
  FParams.Clear;
end;

procedure TCustomRecordset.SetFiltered(Value: Boolean);
var FFiltered: Boolean;
begin
  FFiltered := Filtered;
  inherited SetFiltered(Value);
  if FFiltered <> Value then RefreshParams;
end;

procedure TCustomRecordset.SetFilterText(const Value: string);
begin
  inherited SetFilterText(Value);
  if Filtered then RefreshParams;
end;

procedure TCustomRecordset.SetIndexName(IndexName: string);
begin
  CheckLocked;
  FRecordset.Index := IndexName;
end;

procedure TCustomRecordset.SetMasterFields(Fields: string);
begin
  FMasterFields := Fields;
  if FDetailFields <> '' then RefreshParams;
end;

procedure TCustomRecordset.SetOnFilterRecord(
  const Value: TFilterRecordEvent);
begin
  inherited SetOnFilterRecord(Value);
  FOnFilterRecord := Value;
  if Active then
     begin
       CheckBrowseMode;
       First;
     end;
end;

procedure TCustomRecordset.SetOptions(Value: TRecordsetOptions);
begin
  if Value <> FOptions then
     begin
       if not (csReading in ComponentState) then Close;
       FCommand := nil;
       if ((roQuickReport in Value) and (not (roQuickReport in FOptions)))
            or
          ((not (roQuickReport in Value)) and (roQuickReport in FOptions))
          then if Active then Close;
       if roQuickReport in Value then LockType := ltReadOnly;
       if (roRequery in Value) and (not (roRequery in FOptions)) then Value := Value - [roResync];
       if (roResync in Value) and (not (roResync in FOptions)) then Value := Value - [roRequery];
       if (roAsyncFetch in Value) and (not (roAsyncFetch in FOptions)) then Value := Value - [roAsyncFetchNonBlocking];
       if (roAsyncFetchNonBlocking in Value) and (not (roAsyncFetchNonBlocking in FOptions)) then Value := Value - [roAsyncFetch];
       FOptions := Value;
     end;
end;

procedure TCustomRecordset.SetQuery(Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
     begin
       try
         FSQL.BeginUpdate;
         FSQL.Assign(Value);
       finally
         FSQL.EndUpdate;
       end;
     end;
end;

procedure TCustomRecordset.SetRecNo(Value: Integer);
begin
  CheckActive;
  CheckLocked;
  CheckBrowseMode;
  DoBeforeScroll;
  FRecordset.AbsolutePosition := Value;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;

procedure TCustomRecordset.SetSchemaType(SchemaType: string);
var i          : Integer;
    Index      : Integer;
begin
  if not (csReading in ComponentState) then Close;
  FCommand := nil;
  FTableName := '';
  FFileName := '';
  TStringList(FSQL).OnChange := nil;
  FSQL.Clear;
  TStringList(FSQL).OnChange := SqlOnChange;
  FStoredProcName := '';
  FSchemaType := SchemaType;
  SourceType := stSchema;
  if (csReading in ComponentState)  then Exit;
  if not (csDesigning in ComponentState)  then Exit;
  if not ParamCheck then Exit;
  FParams.Clear;
  Index := SchemaItemByName(SchemaType);
  if Index = -1 then Exit;
  for i := 0 to SchemaItems [Index].ParamCount - 1 do
        begin
          Params.CreateParam(ftUnknown, SchemaItems [Index].Params [i], ptInput).Value := Unassigned;
        end;
end;

procedure TCustomRecordset.SetSource(Source: string);
begin
  case SourceType of
  stTable,stTableDirect: SetTableName(Source);
  stStoredProc: SetStoredProcName(Source);
  stSchema: SetSchemaType(Source);
  stText: if not (csDesigning in ComponentState) then
             begin
               FSQL.Text := Source;
             end;
  stPersistedRecordset: SetFileName(Source);
  end;
end;

procedure TCustomRecordset.SetSourceType(SourceType: TSourceType);
begin
  if not (csReading in ComponentState) then Close;
  FCommand := nil;
  if SourceType = FSourceType then Exit;
  if (Self is TCTable) and (SourceType <> stTable) and (SourceType <> stTableDirect) then DatabaseError(SCannotChangeType);
  if (Self is TCQuery) and (SourceType <> stText) then DatabaseError(SCannotChangeType);
  if (Self is TCSchema) and (SourceType <> stSchema) then DatabaseError(SCannotChangeType);
  if (Self is TCStoredProc) and (SourceType <> stStoredProc) then DatabaseError(SCannotChangeType);
  if (Self is TCPersistedRecordset)
      and
     (SourceType <> stPersistedRecordset)
     then DatabaseError(SCannotChangeType);
  FSourceType := SourceType;
  case SourceType of
  stTable,stTableDirect: begin
                           FStoredProcName := '';
                           TStringList(FSQL).OnChange := nil;
                           FSQL.Clear;
                           TStringList(FSQL).OnChange := SqlOnChange;
                           FSchemaType := '';
                           FFileName := '';
                         end;
  stStoredProc: begin
                  FTableName := '';
                  TStringList(FSQL).OnChange := nil;
                  FSQL.Clear;
                  TStringList(FSQL).OnChange := SqlOnChange;
                  FSchemaType := '';
                  FFileName := '';
                end;
  stSchema: begin
              FStoredProcName := '';
              TStringList(FSQL).OnChange := nil;
              FSQL.Clear;
              TStringList(FSQL).OnChange := SqlOnChange;
              FTableName := '';
              FFileName := '';
            end;
  stText: begin
            FStoredProcName := '';
            FTableName := '';
            FSchemaType := '';
            FFileName := '';
          end;
  stPersistedRecordset:
          begin
            FStoredProcName := '';
            FTableName := '';
            FSchemaType := '';
            TStringList(FSQL).OnChange := nil;
            FSQL.Clear;
            TStringList(FSQL).OnChange := SqlOnChange;
          end;
  end;
end;

procedure TCustomRecordset.SetStoredProcName(StoredProcName: string);
var FCommand   : IADOCommand;
    i          : Integer;
    FType      : TFieldType;
    FParamType : TParamType;
begin
  if not (csReading in ComponentState) then Close;
  Self.FCommand := nil;
  FTableName := '';
  FSchemaType := '';
  FFileName := '';
  TStringList(FSQL).OnChange := nil;
  FSQL.Clear;
  TStringList(FSQL).OnChange := SqlOnChange;
  FStoredProcName := StoredProcName;
  SourceType := stStoredProc;
  if (csReading in ComponentState)  then Exit;
  if not (csDesigning in ComponentState)  then Exit;
  if not ParamCheck then Exit;
  if FConnection = nil then Exit;
  if not FConnection.Active then Exit;
  FCommand := CoCommand.Create;
  try
    FCommand.Set_ActiveConnection(FConnection.FConnection);
    FCommand.CommandText := StoredProcName;
    FCommand.CommandType := cadodb.adCmdStoredProc;
    FCommand.Parameters.Refresh;
    FParams.Clear;
    for i := 0 to FCommand.Parameters.Count - 1 do
        begin
          case FCommand.Parameters [i].Type_ of
          adBinary, adVarBinary, adArray or adUnsignedTinyInt, adArray or adTinyInt:
                  if FRecordSet.Fields [i].Attributes and adFldFixed = adFldFixed
                     then FType := ftBytes else FType := ftVarBytes;
          adBigInt: FType := ftLargeInt;
          adLongVarBinary: FType := ftBlob;
          adBoolean: FType := ftBoolean;
          adBSTR: FType := ftWideString;
          adWChar: FType := ftWideString;
          adVarWChar: FType := ftWideString;
          {$IFDEF VCL50}
          adGUID: FType := ftGUID;
          {$ELSE}
          adGUID: FType := ftString;
          {$ENDIF}
          adChar: FType := ftString;
          adVarChar: FType := ftString;
          adLongVarChar: FType := ftMemo;
          adLongVarWChar: FType := ftMemo;
          adCurrency: FType := ftCurrency;
          adDate: FType := ftDateTime;
          adDBDate: FType := ftDate;
          adDBTime: FType := ftTime;
          adDBTimeStamp: FType := ftDateTime;
          adFileTime: FType := ftDateTime;
          adDecimal: FType := ftFloat;
          adVarNumeric: FType := ftFloat;
          adDouble: FType := ftFloat;
          adEmpty: FType := ftUnknown;
          adError: FType := ftInteger;
          adIDispatch: FType := ftInteger;
          adInteger: FType := ftInteger;
          adIUnknown: FType := ftInteger;
          adNumeric: FType := ftFloat;
          adSingle: FType := ftFloat;
          adSmallInt: FType := ftSmallInt;
          adTinyInt: FType := ftSmallInt;
          adUnsignedBigInt: FType := ftLargeInt;
          adUnsignedInt: FType := ftInteger;
          adUnsignedSmallInt: FType := ftWord;
          adUnsignedTinyInt: FType := ftWord;
          else FType := ftUnknown;
          end;
          case FCommand.Parameters [i].Direction of
          adParamUnknown: FParamType := ptUnknown;
          adParamInput: FParamType := ptInput;
          adParamOutput: FParamType := ptOutput;
          adParamInputOutput: FParamType := ptInputOutput;
          adParamReturnValue: FParamType := ptResult;
          else FParamType := ptUnknown;
          end;
          with Params.CreateParam(FType,
               FCommand.Parameters [i].Name, FParamType) do
               begin
                 Size := FCommand.Parameters [i].Size;
                 Scale := FCommand.Parameters [i].NumericScale;
                 Precision := FCommand.Parameters [i].Precision;
               end;
        end;
  finally
    FCommand := nil;
  end;
end;

procedure TCustomRecordset.SetTableDirect(Value: Boolean);
begin
  if (Value <> FTableDirect)
     and not (csReading in ComponentState) then Close;
  FCommand := nil;
  if Value then SourceType := stTableDirect else SourceType := stTable;
  FTableDirect := Value;
end;

procedure TCustomRecordset.SetTableName(TableName: string);
begin
  if not (csReading in ComponentState) then Close;
  FCommand := nil;
  FStoredProcName := '';
  FSchemaType := '';
  FFileName := '';
  TStringList(FSQL).OnChange := nil;
  FSQL.Clear;
  TStringList(FSQL).OnChange := SqlOnChange;
  FTableName := Trim(TableName);
  if (SourceType <> stTable) and (SourceType <> stTableDirect)
     then SourceType := stTable;
  FParams.Clear;
end;

procedure TCustomRecordset.SqlOnChange(Sender: TObject);
var
  TempParams: TParams;
  i, j: Integer;
begin
  if not (csReading in ComponentState) then Close;
  FCommand := nil;
  FTableName := '';
  FStoredProcName := '';
  FSchemaType := '';
  FFileName := '';
  SourceType := stText;
  if csReading in ComponentState then Exit;
  if ParamCheck then
  begin
    TempParams := TParams.Create(Self);
    try
      TempParams.ParseSQL(FSQL.Text, True);
      i := 0;
      while i <= TempParams.Count - 1 do
          begin
            j := i + 1;
            while j <= TempParams.Count - 1 do
              if AnsiUpperCase(TempParams [j].Name) = AnsiUpperCase(TempParams [i].Name)
                 then TempParams.RemoveParam(TempParams [j])
                 else inc(j);
            inc(i);
          end;
      TempParams.AssignValues(FParams);
      FParams.Clear;
      FParams.Assign(TempParams);
    finally
      TempParams.Free;
    end;
  end;
end;

function TCustomRecordset.SupportsBookmarks: Boolean;
begin
  Result := FRecordSet.Supports(adBookmark);
end;

procedure TCustomRecordset.WriteChanges(Buffer: Pointer);
var i, n: Integer;
    v: OleVariant;
begin
  for i := 0 to Fields.Count - 1 do
    if Fields [i].FieldKind = fkData then
      begin
      n := Fields [i].FieldNo - 1;
      if TRecData(pointer(Buffer)^)[i].Modified then
         if VarType(TRecData(pointer(Buffer)^)[i].Data) > 0 then
             begin
               VariantCopy(v, OleVariant(TRecData(pointer(Buffer)^)[i].Data));
               if (not VarIsEmpty(v)) and
               (
                 ((FRecordset.Fields[n].Attributes and adFldUnknownUpdatable) = adFldUnknownUpdatable)
                 or
                 (
                   ((FRecordset.Fields[n].Attributes and adFldUnknownUpdatable) <> adFldUnknownUpdatable)
                   and
                   ((FRecordset.Fields[n].Attributes and adFldUpdatable) = adFldUpdatable)
                 )
               ) and ((FRecordset.Fields[n].Attributes and adFldLong = 0) or not (roHugeBlobs in FOptions))
               then
                 begin
                   FRecordset.Fields [n].Value := v;
                 end;
             end;
      end;
end;

{:-- TRecordset }

procedure TRecordset.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TableName', ReadTableName, WriteTableName, FTableName <> '');
  Filer.DefineProperty('FileName', ReadFileName, WriteFileName, FFileName <> '');
  Filer.DefineProperty('StoredProcName', ReadStoredProcName, WriteStoredProcName, FStoredProcName <> '');
  Filer.DefineProperty('SchemaType', ReadSchemaType, WriteSchemaType, FSchemaType <> '');
  Filer.DefineProperty('SQL', ReadSQL, WriteSQL, FSQL.Count > 0);
end;

procedure TRecordset.ReadFileName(Reader: TReader);
begin
  FFileName := Reader.ReadString;
end;

procedure TRecordset.ReadSchemaType(Reader: TReader);
begin
  FSchemaType := Reader.ReadString;
end;

procedure TRecordset.ReadSQL(Reader: TReader);
begin
  FSQL.BeginUpdate;
  FSQL.Clear;
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do
          FSQL.Add(Reader.ReadString);
    Reader.ReadListEnd;
  finally
    FSQL.EndUpdate;
  end;
end;

procedure TRecordset.ReadStoredProcName(Reader: TReader);
begin
  SetStoredProcName(Reader.ReadString);
end;

procedure TRecordset.ReadTableName(Reader: TReader);
begin
  FTableName := Reader.ReadString;
end;

procedure TRecordset.WriteFileName(Writer: TWriter);
begin
  Writer.WriteString(FFileName);
end;

procedure TRecordset.WriteSchemaType(Writer: TWriter);
begin
  Writer.WriteString(FSchemaType);
end;

procedure TRecordset.WriteSQL(Writer: TWriter);
var i: integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FSQL.Count - 1 do Writer.WriteString(FSQL [i]);
  Writer.WriteListEnd;
end;

procedure TRecordset.WriteStoredProcName(Writer: TWriter);
begin
  Writer.WriteString(FStoredProcName);
end;

procedure TRecordset.WriteTableName(Writer: TWriter);
begin
  Writer.WriteString(FTableName);
end;

{:-- TCTable }

constructor TCTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceType := stTable;
end;

{:-- TCQuery }

constructor TCQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceType := stText;
end;

{:-- TCStoredProc }

constructor TCStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceType := stStoredProc;
end;

{:-- TCSchema }

constructor TCSchema.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceType := stSchema;
end;

{:-- TCPersistedRecordset }

constructor TCPersistedRecordset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SourceType := stPersistedRecordset;
end;

{:-- TCustomRecordsetDataLink }

procedure TCustomRecordsetDataLink.ActiveChanged;
begin
  if FRecordset.Active then FRecordset.RefreshParams;
end;

procedure TCustomRecordsetDataLink.CheckBrowseMode;
begin
  if FRecordset.Active then FRecordset.CheckBrowseMode;
end;

constructor TCustomRecordsetDataLink.Create(ARecordset: TCustomRecordset);
begin
  inherited Create;
  FRecordset := ARecordset;
end;

function TCustomRecordsetDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FRecordset;
end;

procedure TCustomRecordsetDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FRecordset.Active then FRecordset.RefreshParams;
end;

{:-- TCBlobStream }

constructor TCBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FStreamIsNull := FALSE;
  if Field.DataSet = nil then DatabaseError(SDataSetMissing, Field);
  if not (Field.DataSet is TCDataSet)
     then DatabaseError(SInvalidDataset, Field);
  if (Field.FieldNo <= 0) or (not Field.IsBlob)
     then DatabaseError(SInvalidFieldKind, Field);
  FStream := nil;
  if  (Field.DataSet is TCustomRecordset)
  and (Field.FieldKind = fkData) 
  and (roHugeBlobs in TCustomRecordset(Field.DataSet).FOptions) then
     begin
       if Mode = bmReadWrite then DatabaseError(SOLEBlobStreamModeError);
       if Field.DataSet.State <> dsInsert then Field.DataSet.UpdateCursorPos;
       if  (Mode = bmRead)
       and (TCustomRecordset(Field.DataSet).FRecordset.EOF or
            TCustomRecordset(Field.DataSet).FRecordset.BOF)
       then FStreamIsNull := TRUE
       else FStream := GetOLEDBStorageObject(TCustomRecordset(Field.DataSet).GetRowPos as IRowPosition,
              Field.FieldNo, FStreamIsNull, FStreamSize, Mode);
       if (FStream = nil) and ((not FStreamIsNull) or (Mode = bmWrite))
          then DatabaseError(SOLEBlobStreamError)
          else
            if FStream <> nil then
               Inc(TCustomRecordset(Field.DataSet).FDSLocks);
       if TCustomRecordset(Field.DataSet).FRecordset.Fields [Field.FieldNo - 1].Type_ = adLongVarWChar then
          FStreamSize := FStreamSize div 2;
     end;
  FDataSet := TCDataSet(Field.DataSet);
  FField := Field;
  FMode := Mode;
  FPosition := 0;
  if FMode = bmWrite then
     if FStream = nil then
        begin
          variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data) := Null;
          TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Modified := TRUE;
          FField.Modified := TRUE;
          if not (FDataSet.State in [dsCalcFields, dsFilter, dsNewValue]) then
             FDataSet.DataEvent(deFieldChange, Longint(FField));
        end
          else
            if not (FDataSet.State in [dsCalcFields, dsFilter, dsNewValue]) then
               FDataSet.DataEvent(deFieldChange, Longint(FField));
  inherited Create;
end;

destructor TCBlobStream.Destroy;
begin
  if FStream <> nil then Dec(TCustomRecordset(FDataSet).FDSLocks);
  FStream := nil;
  inherited Destroy;
end;

type BYTEARRAY = array of BYTE;

function TCBlobStream.Read(var Buffer; Count: Integer): Longint;
var tmpBuffer: pointer;
    FSequentialStream: ISequentialStream;
    dwSize: DWORD;
begin
  Result := 0;
  if (FStream <> nil) or (FStreamIsNull) then
     begin
       if FStreamIsNull then Exit;
       if FStream.QueryInterface(IID_ISequentialStream, FSequentialStream) <> S_OK then
          Exit;
       dwSize := 0;
       if TCustomRecordset(FDataSet).FRecordset.Fields [FField.FieldNo - 1].Type_ = adLongVarWChar then
          begin
            tmpBuffer := SysGetMem(Count * 2);
            FSequentialStream.RemoteRead(BYTEARRAY(tmpBuffer)[0], Count * 2, dwSize);
            dwSize := dwSize div 2;
            dwSize := WideCharToMultiByte(0, 0, PWideChar(tmpBuffer), dwSize, @Buffer, Count + 1, nil, nil);
            SysFreeMem(tmpBuffer);
          end else FSequentialStream.RemoteRead(BYTEARRAY(@Buffer)[0],
                      Count, dwSize);
       Result := dwSize;
       FSequentialStream := nil;
       if (Result < 0) then Result := 0;
       inc(FPosition, Result);
       Exit;
     end;
  if FMode = bmWrite then DatabaseError('Stream mode is write only', nil);
  if FPosition >= Size then Exit;
  Result := Size - FPosition;
  if Result > Count then Result := Count;
  case VarType(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)) of
  varString: begin
               tmpBuffer := TVarData(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)).VString;
               CopyMemory(@Buffer, @(PChar(string(tmpBuffer))[FPosition]), Result);
               inc(FPosition, Result);
             end;
  varOleStr: begin
               tmpBuffer := TVarData(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)).VOleStr;
               CopyMemory(@Buffer, @(PChar(WideCharToString(tmpBuffer))[FPosition]), Result);
               inc(FPosition, Result);
             end;
  varArray or VarByte:
             begin
               tmpBuffer := VarArrayLock(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data));
               CopyMemory(@Buffer, pointer(Integer(tmpBuffer)+FPosition), Result);
               VarArrayUnLock(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data));
               inc(FPosition, Result);
             end;
  else Result := 0;
  end;
end;

function TCBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
    function GetSize: Integer;
    begin
      if (FStream <> nil) then
      begin
        Result := FStreamSize;
        Exit;
      end;
      if (FStreamIsNull) then
      begin
        Result := 0;
        Exit;
      end;
      case VarType(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)) of
      varOleStr: Result := lstrlenW(TVarData(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data))
                           .VOleStr);
      varString: Result := Length(string((TVarData(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data))
                           .VString)));
      varArray or VarByte: Result := VarArrayHighBound(
                          variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data), 1) -
                          VarArrayLowBound(
                          variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data), 1) + 1;

      else Result := 0;
      end;
    end;
var OLEStream: IStream;
    OLESequentialStream: ISequentialStream;
    uliNewPosition: ULARGE_INTEGER;
    liOffset: LARGE_INTEGER;
begin
  if ((FStream = nil) and (FStreamIsNull))
  or ((Origin = soFromCurrent) and (Offset = 0) and (FPosition = 0))
  then
    begin
      Result := 0;
      FPosition := 0;
      Exit;
    end;
  liOffset.QuadPart := Offset;
  if FStream <> nil then
     begin
       if FStream.QueryInterface(IID_ISequentialStream, OLESequentialStream) <> S_OK then
          OLESequentialStream := nil;
       if FStream.QueryInterface(IID_IStream, OLEStream) <> S_OK then
          OLEStream := nil;
     end else
           begin
             OLEStream := nil;
             OLESequentialStream := nil;
           end;
  if (Origin = soFromEnd) and (Offset = 0) then
     begin
       if OLEStream <> nil
          then Result := OLEStream.RemoteSeek(liOffset, STREAM_SEEK_END,
                           uliNewPosition)
          else Result := GetSize;
       FPosition := Result;
       if (OLEStream = nil) and (OLESequentialStream <> nil) then
          FPosition := 0;
       Exit;
     end;
  if (Origin = soFromBeginning) and (Offset = 0) then
     begin
       Result := 0;
       if FPosition <> 0 then
          begin
            FPosition := 0;
            if (FStream <> nil) then
               begin
                 FStream := nil;
                 Dec(TCustomRecordset(FDataSet).FDSLocks);
                 if  (FMode = bmRead)
                 and (TCustomRecordset(FDataSet).FRecordset.EOF or
                      TCustomRecordset(FDataSet).FRecordset.BOF)
                 then FStreamIsNull := TRUE
                 else FStream := GetOLEDBStorageObject(TCustomRecordset(FDataSet).GetRowPos as IRowPosition,
                          FField.FieldNo, FStreamIsNull, FStreamSize, FMode);
                 if (FStream = nil) and ((not FStreamIsNull) or (FMode = bmWrite))
                    then DatabaseError(SOLEBlobStreamError)
                    else
                      if FStream <> nil then
                         Inc(TCustomRecordset(FDataSet).FDSLocks);
                      if TCustomRecordset(FDataSet).FRecordset.Fields [FField.FieldNo - 1].Type_ = adLongVarWChar then
                         FStreamSize := FStreamSize div 2;
               end;
          end;
       Exit;
     end;
  if (FStream <> nil) and (OLEStream = nil) then
     DatabaseError(SOLEBlobStreamSeekError);
  if (OLEStream <> nil) then
     begin
       case Origin of
         soFromBeginning : Result := OLEStream.RemoteSeek(liOffset,
                                       STREAM_SEEK_SET, uliNewPosition);
         soFromCurrent   : Result := OLEStream.RemoteSeek(liOffset,
                                       STREAM_SEEK_CUR, uliNewPosition);
         else              Result := OLEStream.RemoteSeek(liOffset,
                                       STREAM_SEEK_END, uliNewPosition);
       end;
       Exit;
     end;
  if GetSize <= 0 then
     begin
       FPosition := 0;
       Result := 0;
       Exit;
     end;
  if FPosition > GetSize then FPosition := GetSize;
  if FPosition < 0 then FPosition := 0;
  if (Origin = soFromCurrent) and (Offset = 0) then
     begin
       Result := FPosition;
       Exit;
     end;
  if (Origin = soFromEnd) then FPosition := GetSize - Offset;
  if (Origin = soFromBeginning) then FPosition := Offset;
  if (Origin = soFromCurrent) then FPosition := FPosition + Offset;
  if FPosition > GetSize  then FPosition := GetSize;
  if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

procedure TCBlobStream.SetSize(NewSize: Integer);
begin
  inherited SetSize(NewSize);
end;

function TCBlobStream.Write(const Buffer; Count: Integer): Longint;
var tmpBuffer: PChar;
    vStr: string;
    vPointer: pointer;
    HBound: Integer;
    NullChar: char;
    FSequentialStream: ISequentialStream;
    dwSize: DWORD;
begin
  if FStream <> nil then
     begin
       Result := 0;
       if FStream.QueryInterface(IID_ISequentialStream, FSequentialStream) <> S_OK then
          Exit;
       dwSize := 0;
       if TCustomRecordset(FDataSet).FRecordset.Fields [FField.FieldNo - 1].Type_ = adLongVarWChar then
          begin
            vPointer := SysGetMem((Count + 1) * 2);
            Count := MultiByteToWideChar(0, 0, @Buffer, Count, PWideChar(vPointer), Count + 1);
            FSequentialStream.RemoteWrite(BYTEARRAY(vPointer)[0], Count * 2, dwSize);
            dwSize := dwSize div 2;
            SysFreeMem(vPointer);
          end else FSequentialStream.RemoteWrite(BYTEARRAY(@Buffer)[0], Count, dwSize);
       Result := dwSize;
       FSequentialStream := nil;
       if (Result < 0) then Result := 0;
       inc(FPosition, Result);
       Exit;
     end;
  NullChar := #0;
  if FMode = bmRead then DatabaseError('Stream mode is read only', nil);
  if not((FDataSet.State = dsEdit) or (FDataSet.State = dsInsert))
     then DatabaseError(SNotEditing, FDataSet);
  if FPosition >= Size then FPosition := Size;
  Result := Size - FPosition;
  if Result > Count then Result := Count;
  if VarType(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)) < 2
     then begin
            if FField.DataType = ftMemo
               then variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data) := WideString('')
               else variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data) := VarArrayCreate([0, Count - 1], varByte);
          end;
  case VarType(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data)) of
  varString: begin
               vStr := VarToStr(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data));
               Result := Count;
               Delete(vStr, FPosition + 1, Result);
               tmpBuffer := SysGetMem(Result + 1);
               CopyMemory(tmpbuffer, pointer(@Buffer), Result);
               CopyMemory(pointer(DWORD(pointer(tmpbuffer)) + DWORD(Result)), @NullChar, 1);
               Insert(StrPas(tmpbuffer), vStr, FPosition + 1);
               SysFreeMem(tmpBuffer);
               variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data) := WideString(vStr);
               inc(FPosition, Result);
             end;
  varOleStr: begin
               vStr := variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data);
               Result := Count;
               Delete(vStr, FPosition + 1, Result);
               tmpBuffer := SysGetMem(Result + 1);
               CopyMemory(tmpbuffer, pointer(@Buffer), Result);
               CopyMemory(pointer(DWORD(pointer(tmpbuffer)) + DWORD(Result)), @NullChar, 1);
               Insert(StrPas(tmpbuffer), vStr, FPosition + 1);
               SysFreeMem(tmpBuffer);
               variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data) := WideString(vStr);
               inc(FPosition, Result);
             end;
  varArray or VarByte:
             begin
               if Result < Count then
                  begin
                    HBound := VarArrayHighBound(
                          variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data), 1);
                    Result := Count - Result;
                    VarArrayRedim(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data), HBound + Result);
                  end;
               Result := Count;
               vPointer := VarArrayLock(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data));
               CopyMemory(pointer(Integer(vPointer)+FPosition), @Buffer, Result);
               VarArrayUnLock(variant(TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Data));
               inc(FPosition, Result);
             end;
  else Result := 0;
  end;
  if Result > 0 then
     begin
       TRecData(pointer(FDataSet.ActiveBuffer)^)[FField.Index].Modified := TRUE;
       FField.Modified := TRUE;
       if not (FDataSet.State in [dsCalcFields, dsFilter, dsNewValue]) then
          FDataSet.DataEvent(deFieldChange, Longint(FField));
     end;
end;

function GetConnectionStringParam(ConnectionString: WideString; Param: string): string;
var p         : Integer;
    Len       : Integer;
    Found     : Boolean;
    WrongChar : Boolean;
begin
  Result := '';
  p := Pos(AnsiUpperCase(Param), AnsiUpperCase(ConnectionString));
  while p > 0 do
     begin
       Delete(ConnectionString, 1, p + Length(Param) - 1);
       p := 1;
       Found := FALSE;
       WrongChar := FALSE;
       Len := Length(ConnectionString);
       while (p <= Len) and (not Found) and (not WrongChar) do
             begin
               if ConnectionString[p] = '=' then
                  Found := TRUE
                  else if ConnectionString[p] <> ' ' then
                          WrongChar := TRUE;
               inc(p);
             end;
       if WrongChar then Found := FALSE;
       if Found then
          begin
            p := Pos(';', ConnectionString);
            if p > 0 then Delete(ConnectionString, p, Length(ConnectionString) - p + 1);
            for p := 1 to Length(ConnectionString) do
                if (ConnectionString[p] <> '=') and (Ord(ConnectionString[p]) > 31)
                   then Result := Result + ConnectionString[p];
            while (Length(Result) > 0) and (Result[1] = ' ') do Delete(Result, 1, 1);
            while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do Delete(Result, Length(Result), 1);
          end;
       p := Pos(AnsiUpperCase(Param), AnsiUpperCase(ConnectionString));
     end;
end;

procedure SetConnectionStringParam(var ConnectionString: WideString; Param: string;
  Value: string);
var p, ParamPos : Integer;
    Len         : Integer;
    Found       : Boolean;
    WrongChar   : Boolean;
begin
  p := Pos(AnsiUpperCase(Param), AnsiUpperCase(ConnectionString));
  while p > 0 do
     begin
       p := p + Length(Param);
       Found := FALSE;
       WrongChar := FALSE;
       Len := Length(ConnectionString);
       while (p <= Len) and (not Found) and (not WrongChar) do
             begin
               if ConnectionString[p] = '=' then
                  Found := TRUE
                  else if ConnectionString[p] <> ' ' then
                          WrongChar := TRUE;
               inc(p);
             end;
       if WrongChar then Found := FALSE;
       ParamPos := p;
       if Found then
          begin
            p := Pos(';', Copy(ConnectionString, ParamPos,
                   Length(ConnectionString) - ParamPos + 1));
            if p > 0
               then p := p + ParamPos - 1
               else p := Length(ConnectionString) + 1;
            Delete(ConnectionString, ParamPos, p - ParamPos);
            Insert(' ' + Value, ConnectionString, ParamPos);
          end;
       p := Pos(AnsiUpperCase(Param), AnsiUpperCase(Copy(ConnectionString,
                 ParamPos, Length(ConnectionString) - ParamPos + 1)));
       if p > 0 then p := p + ParamPos - 1;
     end;
end;

function TCustomRecordset.GetRowPos: IUnknown;
var FRecordsetCS: IADORecordsetConstruction;
    FObj: IUnknown;
begin
  Result := nil;
  if FRecordset.QueryInterface(IID_IADORecordsetConstruction,
       FRecordsetCS) <> S_OK then
     begin
       FRecordsetCS := nil;
       Exit;
     end;
  if FRecordsetCS.Get_RowPosition(FObj) <> S_OK
     then Result := nil
     else Result := FObj;
  FObj := nil;
  FRecordsetCS := nil;
end;

function TCustomRecordset.GetRecordset: IADORecordset;
begin
  Result := FRecordset;
end;

procedure TCustomRecordset.CheckLocked;
begin
  if FDSLocks > 0 then DatabaseError(SRecordsetLocked, Self);
end;

procedure TCustomRecordset.DoBeforeScroll;
begin
  CheckLocked;
  inherited;
end;

procedure TCustomRecordset.InternalInsert;
begin
  CheckLocked;
  inherited;
  if roHugeBlobs in FOptions then
     begin
       UpdateCursorPos;
       if FRecordset.EOF then
          if (not FRecordset.BOF) and (FRecordset.Supports(adMovePrevious)) then
             begin
               FRecordset.MovePrevious;
             end;
       if FRecordset.BOF then
          begin
            if (not FRecordset.EOF) then FRecordset.MoveNext;
          end;
       FRecordset.AddNew(EmptyParam, EmptyParam);
     end;
end;
end.
