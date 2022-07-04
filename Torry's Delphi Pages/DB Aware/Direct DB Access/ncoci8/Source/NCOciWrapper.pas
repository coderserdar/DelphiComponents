{*********************************************************}
{File:      NCOciWrapper.PAS                              }
{Revision:  0.04.00 / 4.11.2001                           }
{Comment:   NC OCI8 VCL: Oracle8 handles wrapping classes }
{Copyright: (c) 1999-2001, Dmitry Arefiev                 }
{Author:    Dmitry Arefiev, darefiev@da-soft.com          }
{*********************************************************}
{$I NCOciDef.inc}
{CE_Desc_Include(HelpText\NCOciWrapper.txt)}
{$O-} // If optimization is turned off, then pieced fetch
{$W-} // raise AV at call of OCIStmtExecute

unit NCOciWrapper;

interface

Uses NCOci, SysUtils, Classes, Windows, NCSQLMon;

type
    TOCIHandle = class;
    TOCIEnvironment = class;
    TOCIErrorMessage = class;
    EOCINativeError = class;
    TOCIError = class;
    TOCIService = class;
    TOCISession = class;
    TOCIServer = class;
    TOCIDescriptor = class;
    TOCIXid = class;
    TOCITransaction = class;
    TOCIStatement = class;
    TOCIVariable = class;
    TOCISelectItem = class;
    TOCILobLocator = class;
    TOCILobLocatorClass = class of TOCILobLocator;
    TOCIIntLocator = class;
    TOCIExtLocator = class;
    TOCIDescribe = class;
    TOCIDirectPath = class;
    TOCIDirectPathColArray = class;
    TOCIDirectPathStream = class;
    TOCIDirectPathColumnParam = class;

    TOCICustomWrappedComponent = class(TOCICustomComponent);
    TOCICustomWrappedDataSet = class(TOCICustomDataSet);

    PInteger = ^Integer;
    PDouble = ^Double;
{$IFNDEF OCI_D4}
    PDateTime = ^TDateTime;
{$ENDIF}

    TOCIErrorEvent = procedure (ASender: TObject; var AError: Exception) of object;
    TOCIWarningEvent = TOCIErrorEvent;

// --------------------------------------------------------------------------
// HANDLE's & DESCRIPTOR's
// --------------------------------------------------------------------------

    TOCIHandle = class
    private
        FHandle: pOCIHandle;
        FType: ub4;
        FOwner: TOCIHandle;
        FError: TOCIError;
        FSQLMonitor: TNCSQLMonitorClient;
        FOwningObj: TObject;
        function Init(AEnv: TOCIHandle; AType: ub4): pOCIHandle;
        function GetError: TOCIError;
        function GetSQLMonitor: TNCSQLMonitorClient;
        procedure Check(const AMsg: String; ACode: sb4);
    protected
        FOwnHandle: Boolean;
    public
        constructor Create;
        destructor Destroy; override;
        procedure SetStringAttr(AAtrType: Integer; const AValue: String);
        function GetStringAttr(AAtrType: Integer): String;
        procedure SetHandleAttr(AAtrType: Integer; AValue: pOCIHandle);
        function GetHandleAttr(AAtrType: Integer): pOCIHandle;
        procedure SetUB1Attr(AAtrType: Integer; AValue: ub1);
        function GetUB1Attr(AAtrType: Integer): ub1;
        procedure SetUB2Attr(AAtrType: Integer; AValue: ub2);
        function GetUB2Attr(AAtrType: Integer): ub2;
        procedure SetUB4Attr(AAtrType: Integer; AValue: ub4);
        function GetUB4Attr(AAtrType: Integer): ub4;
        procedure SetSB1Attr(AAtrType: Integer; AValue: sb1);
        function GetSB1Attr(AAtrType: Integer): sb1;
        procedure SetSB4Attr(AAtrType: Integer; AValue: Sb4);
        function GetSB4Attr(AAtrType: Integer): sb4;
        function DbgOutActive: Boolean;
        procedure DbgOut(AType: TNCTraceFlag; const AStmt: String);
        property Value: pOCIHandle read FHandle;
        property Handle: pOCIHandle read FHandle;
        property HType: ub4 read FType;
        property Error: TOCIError read GetError;
        property SQLMonitor: TNCSQLMonitorClient read GetSQLMonitor;
        property Owner: TOCIHandle read FOwner;
        property OwningObj: TObject read FOwningObj write FOwningObj;
    end;

    TOCIDatabaseMode = (dmThreaded, dmObject, dmEvents, dmShared, dmDistributed);
    TOCIDatabaseModes = set of TOCIDatabaseMode;
    TOCIEnvironment = class(TOCIHandle)
    private
        FError: TOCIError;
        FSQLMonitor: TNCSQLMonitorClient;
    public
        constructor Create(AModes: TOCIDatabaseModes;
                           ASQLMonitor: TNCSQLMonitorClient;
                           AOnError: TOCIErrorEvent;
                           AOnWarning: TOCIWarningEvent);
        constructor CreateUsingHandle(AHandle: pOCIHandle;
                           AErrHandle: pOCIHandle;
                           ASQLMonitor: TNCSQLMonitorClient;
                           AOnError: TOCIErrorEvent;
                           AOnWarning: TOCIWarningEvent);
        destructor Destroy; override;
        property CACHE_ARRAYFLUSH: sb4 index OCI_ATTR_CACHE_ARRAYFLUSH read GetSb4Attr write SetSb4Attr;
        property CACHE_MAX_SIZE: ub4 index OCI_ATTR_CACHE_MAX_SIZE read GetUb4Attr write SetUb4Attr;
        property CACHE_OPT_SIZE: ub4 index OCI_ATTR_CACHE_OPT_SIZE read GetUb4Attr write SetUb4Attr;
        property OBJECT_: sb4 index OCI_ATTR_OBJECT read GetSb4Attr;
        property PINOPTION: ub4 index OCI_ATTR_PINOPTION read GetUb4Attr write SetUb4Attr;
        property ALLOC_DURATION: ub2 index OCI_ATTR_ALLOC_DURATION read GetUb2Attr write SetUb2Attr;
        property PIN_DURATION: ub2 index OCI_ATTR_PIN_DURATION read GetUb2Attr write SetUb2Attr;
        property HEAPALLOC: ub4 index OCI_ATTR_HEAPALLOC read GetUb4Attr;
        property OBJECT_NEWNOTNULL: sb4 index OCI_ATTR_OBJECT_NEWNOTNULL read GetSb4Attr write SetSb4Attr;
        property OBJECT_DETECTCHANGE: sb4 index OCI_ATTR_OBJECT_DETECTCHANGE read GetSb4Attr write SetSb4Attr;
        property SHARED_HEAPALLOC: ub4 index OCI_ATTR_SHARED_HEAPALLOC read GetUb4Attr;
    end;

    TOCIErrorMessage = class
    private
        FErrorCode: sb4;
        FMessage: string;
        FLevel: ub4;
        FServerObject: String;
    public
        constructor Create(AOwner: EOCINativeError;
            ALevel: ub4; AErrorCode: sb4; AMessage: PChar);
        property ErrorCode: sb4 read FErrorCode;
        property Message: string read FMessage;
        property Level: ub4 read FLevel;
        property ServerObject: String read FServerObject; 
    end;

    EOCINativeError = class(EOCIErrorBase)
    private
        FErrors: TList;
        FParseErrorOffset: ub4;
        function GetError(Index: Integer): TOCIErrorMessage;
        function GetErrorCount: Integer;
    public
        constructor Create(const AMessage: String; AError: TOCIError);
        destructor Destroy; override;
        property ErrorCount: Integer read GetErrorCount;
        property Errors[Index: Integer]: TOCIErrorMessage read GetError;
        property ParseErrorOffset: ub4 read FParseErrorOffset;
    end;

    EOCISystemError = class(EOCIErrorBase)
    private
        FErrorCode: sb4;
    public
{$IFDEF OCI_D4}
    {$IFDEF OCI_BCB}
        constructor Create(AErrorCode: sb4; Dummy: Cardinal = 0); 
    {$ELSE}
        constructor Create(AErrorCode: sb4);
    {$ENDIF}
{$ELSE}
        constructor Create(AErrorCode: sb4);
{$ENDIF}
        property ErrorCode: sb4 read FErrorCode;
    end;

    TOCIError = class(TOCIHandle)
    private
        FWarning: EOCINativeError;
        FOnError: TOCIErrorEvent;
        FOnWarning: TOCIWarningEvent;
    public
        constructor Create(AEnv: TOCIEnvironment);
        constructor CreateUsingHandle(AEnv: TOCIEnvironment; AErrHandle: pOCIHandle);
        destructor Destroy; override;
        procedure Check(ASender: TObject; const AMsg: String; ACode: sword);
        procedure ClearWarning;
        property Warning: EOCINativeError read FWarning;
        property DML_ROW_OFFSET: ub4 index OCI_ATTR_DML_ROW_OFFSET read GetUb4Attr;
        property OnError: TOCIErrorEvent read FOnError write FOnError;
        property OnWarning: TOCIWarningEvent read FOnWarning write FOnWarning;
    end;

    TOCIService = class(TOCIHandle)
    private
        FOnYield: TNotifyEvent;
        FNonBlockinMode: Boolean;
        procedure DoYield;
        procedure SetNonblockingMode(const Value: Boolean);
    public
        constructor Create(AEnv: TOCIEnvironment);
        constructor CreateUsingHandle(AEnv: TOCIEnvironment; AHandle: pOCIHandle);
        procedure Break(ADoException: Boolean);
        procedure Reset(ADoException: Boolean);
        procedure TurnNONBLOCKING_MODE;
        procedure UpdateNonBlockingMode;
        property OnYield: TNotifyEvent read FOnYield write FOnYield;
        property ENV: pOCIHandle index OCI_ATTR_ENV read GetHandleAttr;
        property SERVER: pOCIHandle index OCI_ATTR_SERVER read GetHandleAttr write SetHandleAttr;
        property SESSION: pOCIHandle index OCI_ATTR_SESSION read GetHandleAttr write SetHandleAttr;
        property TRANS: pOCIHandle index OCI_ATTR_TRANS read GetHandleAttr write SetHandleAttr;
        property IN_V8_MODE: ub1 index OCI_ATTR_IN_V8_MODE read GetUb1Attr;
        property NONBLOCKING_MODE: Boolean read FNonBlockinMode write SetNonblockingMode;
    end;

    TOCIAuthentMode = (amDefault, amSysDba, amSysOper);
    TOCISession = class(TOCIHandle)
    private
        FStarted: Boolean;
    public
        constructor Create(AService: TOCIService);
        constructor CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
        destructor Destroy; override;
        procedure Start(const AUser, APassword: String; AAuthentMode: TOCIAuthentMode);
        procedure ChangePassword(const AUser, AOldPassword, ANewPassword: String);
        procedure Stop;
        procedure Select;
        property Started: Boolean read FStarted;
        property USERNAME: String index OCI_ATTR_USERNAME read GetStringAttr write SetStringAttr;
        property PASSWORD: String index OCI_ATTR_PASSWORD read GetStringAttr write SetStringAttr;
        property MIGSESSION: ub1 index OCI_ATTR_MIGSESSION read GetUb1Attr write SetUb1Attr;
    end;

    TOCIServer = class(TOCIHandle)
    private
        FAttached: Boolean;
        function GetServerVersion: String;
    public
        constructor Create(AService: TOCIService);
        constructor CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
        destructor Destroy; override;
        procedure Attach(const AServerName: String);
        procedure Detach;
        procedure Select;
        property ServerVersion: String read GetServerVersion;
        property Attached: Boolean read FAttached;
        property ENV: pOCIHandle index OCI_ATTR_ENV read GetHandleAttr;
        property EXTERNAL_NAME: String index OCI_ATTR_EXTERNAL_NAME read GetStringAttr write SetStringAttr;
        property INTERNAL_NAME: String index OCI_ATTR_INTERNAL_NAME read GetStringAttr write SetStringAttr;
        property IN_V8_MODE: ub1 index OCI_ATTR_IN_V8_MODE read GetUb1Attr;
        property FOCBK: Pointer index OCI_ATTR_FOCBK read GetHandleAttr write SetHandleAttr;
        property SERVER_GROUP: String index OCI_ATTR_SERVER_GROUP read GetStringAttr write SetStringAttr;
    end;

    TOCIXid = class(TPersistent)
    private
        FOnChanging, FOnChanged: TNotifyEvent;
        FName: String;
        procedure SetBQUAL(const AValue: String);
        function GetBQUAL: String;
        procedure SetGTRID(const AValue: String);
        function GetGTRID: String;
        function GetIsNull: Boolean;
        procedure SetName(const AValue: String);
    public
        FXID: TXID;
        constructor Create;
        procedure Assign(AValue: TPersistent); override;
        procedure Clear;
        procedure SetParts(const AGTRID, ABQUAL: String);
        procedure MarkTransaction(ATrans: TOCITransaction);
        property IsNull: Boolean read GetIsNull;
        property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
        property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    published
        property GTRID: String read GetGTRID write SetGTRID;
        property BQUAL: String read GetBQUAL write SetBQUAL;
        property Name: String read FName write SetName;
    end;

    TOCITransactionMode = (tmDefault, tmReadWrite, tmSerializable,
                           tmReadOnly, tmDiscrete);
    TOCICoupleMode = (omDefault, omTightly, omLoosely);
    TOCITransaction = class(TOCIHandle)
    public
        constructor Create(AService: TOCIService);
        constructor CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
        procedure Detach;
        procedure StartLocal(AMode: TOCITransactionMode);
        procedure StartGlobal(AMode: TOCITransactionMode; ATimeOut: ub4;
            ANew: Boolean; ACoupleMode: TOCICoupleMode);
        procedure Commit;
        procedure RollBack;
        procedure Select;
        procedure Prepare2PC;
        procedure Commit2PC;
        procedure Forget;
        procedure GetXID(var AValue: TXID);
        procedure SetXID(var AValue: TXID);
        property TRANS_NAME: String index OCI_ATTR_TRANS_NAME read GetStringAttr write SetStringAttr;
    end;

    TOCIPieceLoopData = record
        lastPieceVarHndl: pOCIHandle;
        lastPieceVar: TOCIVariable;
        lastPieceSize: ub4;
        lastInd: sb2;
        lastIter: ub4;
        lastPiece: ub1;
        lastRCode: ub2;
        lastInOut: ub1;
        lastBuff: pUb1;
    end;

    TOCIVarDataType = (otUnknown, otSmallInt, otInteger, otFloat, otBCD, otNumber,
                       otString, otChar, otRaw, otDateTime, otLong, otLongRaw,
                       otCursor, otNestedDataSet, otCLOB, otBLOB, otCFile, otBFile,
                       otROWID);
    TOCIVarDataTypes = set of TOCIVarDataType;
    TOCIVarType = (odUnknown, odIn, odOut, odInOut, odRet, odDefine);
    TOCIVarTypes = set of TOCIVarType;
    TOCIUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied, uaReRaise);

    TOCIStatement = class(TOCIHandle)
    private
        FEof: Boolean;
        FLastRowCount: ub4;
        FPieceBuffOwn: Boolean;
        FPieceBuffLen: ub4;
        FPieceBuff: pUb1;
        FPieceOutVars: TList;
        FVars: TList;
        FService: TOCIService;
        FRef: Boolean;
        FCanceled: Boolean;
        procedure Fetch(ANRows: ub4; AOrient: ub2);
        procedure SetPieceBuffOwn(AValue: Boolean);
        procedure SetPieceBuffLen(AValue: ub4);
        procedure AllocPieceBuff;
        procedure FreePieceBuff;
        function Hndl2PieceVar(varHndl: pOCIHandle): TOCIVariable;
        function GetPiecedFetch: Boolean;
        procedure InitPiecedFetch(var pld: TOCIPieceLoopData);
        procedure DoPiecedFetch(var APieceLoopData: TOCIPieceLoopData; ARes: sb4);
        procedure DumpOutVars(ARows: ub4);
        procedure DumpInVars(ATypes: TOCIVarTypes);
    protected
        procedure AddPieceVar(AVar: TOCIVariable);
        procedure RemovePieceVar(AVar: TOCIVariable);
        procedure AddVar(AVar: TOCIVariable);
        procedure RemoveVar(AVar: TOCIVariable);
    public
        constructor Create(AEnv: TOCIEnvironment);
        constructor CreateUsingHandle(AEnv: TOCIEnvironment; AHandle: pOCIHandle);
        destructor Destroy; override;
        procedure Prepare(const AStatement: String);
        procedure Describe(AService: TOCIService);
        procedure Execute(AService: TOCIService; ARows, AOffset: sb4;
            AExact, ACommit, AScrollable: Boolean);
        procedure CancelCursor;
        procedure Next(ANRows: ub4);
        property PieceBuffOwn: Boolean read FPieceBuffOwn write SetPieceBuffOwn;
        property PieceBuffLen: ub4 read FPieceBuffLen write SetPieceBuffLen;
        property Eof: Boolean read FEof;
        property LastRowCount: ub4 read FLastRowCount;
        property PiecedFetch: Boolean read GetPiecedFetch;
        property Canceled: Boolean read FCanceled;
        property Ref: Boolean read FRef write FRef;
        property NUM_DML_ERRORS: ub4 index OCI_ATTR_NUM_DML_ERRORS read GetUb4Attr;
        property ENV: pOCIHandle index OCI_ATTR_ENV read GetHandleAttr;
        property STMT_TYPE: ub2 index OCI_ATTR_STMT_TYPE read GetUB2Attr;
        property ROW_COUNT: ub4 index OCI_ATTR_ROW_COUNT read GetUB4Attr;
        property SQLFNCODE: ub2 index OCI_ATTR_SQLFNCODE read GetUB2Attr;
        property ROWID: pOCIRowid index OCI_ATTR_ROWID read GetHandleAttr;
        property PARAM_COUNT: ub4 index OCI_ATTR_PARAM_COUNT read GetUB4Attr;
        property PREFETCH_ROWS: ub4 index OCI_ATTR_PREFETCH_ROWS read GetUB4Attr write SetUB4Attr;
        property PREFETCH_MEMORY: ub4 index OCI_ATTR_PREFETCH_MEMORY read GetUB4Attr write SetUB4Attr;
        property PARSE_ERROR_OFFSET: ub4 index OCI_ATTR_PARSE_ERROR_OFFSET read GetUB4Attr;
    end;

    TBindData = record
        FInd: PSb2Array;
        FALen: PUb2Array;
        FRCode: PUb2Array;
        FOwned: PBoolArray;
        FBuffer: pUb1;
    end;
    PBindData = ^TBindData;

    TOraDate = record
        century, year, month, day,
        hour, minute, second: ub1;
    end;
    POraDate = ^TOraDate;

    TOraLong = record
        FSize: ub4;
        FData: pUb1;
    end;
    POraLong = ^TOraLong;

    TOCIBuffStatus = (bsInt, bsExtUnknown, bsExtInit);
    TOCIDataFormat = (dfOCI, dfDataSet, dfDelphi, dfDelphiOwned);

    TOCIVariable = class(TOCIHandle)
    private
        FPlaceHolder: String;
        FPosition: ub4;
        FBindData: PBindData;
        FValue_sz: ub4;       // value max length
        FValue_ty: ub2;       // value type
        FMaxArr_len: ub4;     // maximum number of elements
        FCurEle: ub4;         // actual number of elements
        FDataType: TOCIVarDataType;
        FVarType: TOCIVarType;
        FBuffStatus: TOCIBuffStatus;
        FInrecDataSize: ub4;
        FIsPLSQLTable: Boolean;
        FIsCaseSensitive: Boolean;
        FLongData: Boolean;
        FStrsTrim: Boolean;
        FStrsEmpty2Null: Boolean;
        FDumpLabel: String;
        FLastBindedBuffer: pUb1;
        FEnableLongString: Boolean;
        procedure SetArrayLen(const Value: ub4);
        procedure SetDataSize(const Value: ub4);
        procedure SetDataType(const Value: TOCIVarDataType);
        procedure SetVarType(const Value: TOCIVarType);
        function GetBuffer(AIndex: ub4): pUb1;
        procedure FreeBuffer;
        procedure CheckBufferAccess(AIndex: ub4);
        procedure SetExternBuffer(AValue: pUb1);
        function GetExternBuffer: pUb1;
        procedure FreeLong(pLong: POraLong);
        procedure InitLong(pLong: POraLong);
        procedure GetLong(pLong: POraLong; var AData: pUb1; var ASize: ub4);
        procedure SetLong(pLong: POraLong; AData: pUb1; ASize: ub4; AOwnBuffer: Boolean);
        procedure AppendLong(pLong: POraLong; AData: pUb1; ASize: ub4; AOwnBuffer: Boolean);
        procedure InitLongData;
        procedure SetInrecDataSize(const Value: ub4);
        procedure CheckOwner;
        function GetDumpLabel: String;
        function UpdateHBlobNullInd(AIndex: ub4): Boolean;
        function GetBindDataSize: ub4;
        procedure MapBuffer;
    protected
        property Buffer[AIndex: ub4]: pUb1 read GetBuffer;
    public
        constructor Create(AStmt: TOCIStatement);
        destructor Destroy; override;
        procedure Bind;
        // for TOciParam (parameters)
        procedure BindTo(AStmt: TOCIStatement);
        procedure BindOff;
        // for TOCIDeadCursor (external buffer management)
        procedure ClearBuffer(AIndex: sb4; AHandleOwned: Boolean);
        procedure InitBuffer(AIndex: sb4; AHandleOwned: Boolean);
        procedure ResetBuffer(AIndex: sb4);
        // Value related
        function GetDataPtr(AIndex: ub4; var ABuff: pUb1; var ASize: ub4): Boolean;
        function GetData(AIndex: ub4; ABuff: pUb1; var ASize: ub4; AFormat: TOCIDataFormat): Boolean;
        function DumpValue(AIndex: ub4): String;
        procedure SetData(AIndex: ub4; ABuff: pUb1; ASize: ub4; AFormat: TOCIDataFormat);
        procedure SetDataByRef(AIndex: ub4; ABuff: pUb1; ASize: ub4; AFormat: TOCIDataFormat);
        procedure AppendData(AIndex: ub4; ABuff: pUb1; ASize: ub4);
        procedure SetIsNull(AIndex: ub4; AIsNull: Boolean); // for LOB fields
        function GetIsOwned(AIndex: ub4): Boolean;
        // -----------
        property Name: String read FPlaceHolder write FPlaceHolder;
        property Position: ub4 read FPosition write FPosition;
        property DumpLabel: String read GetDumpLabel write FDumpLabel;
        property IsPLSQLTable: Boolean read FIsPLSQLTable write FIsPLSQLTable;
        property IsCaseSensitive: Boolean read FIsCaseSensitive write FIsCaseSensitive; 
        property DataType: TOCIVarDataType read FDataType write SetDataType;
        property DataSize: ub4 read FValue_sz write SetDataSize;
        property VarType: TOCIVarType read FVarType write SetVarType;
        property ArrayLen: ub4 read FMaxArr_len write SetArrayLen;
        property ArrayCount: ub4 read FCurEle;
        // -----------
        property ExternBuffer: pUb1 read GetExternBuffer write SetExternBuffer;
        property BindDataSize: ub4 read GetBindDataSize;
        property InrecDataSize: ub4 read FInrecDataSize write SetInrecDataSize;
        property LongData: Boolean read FLongData write FLongData;
        property StrsTrim: Boolean read FStrsTrim write FStrsTrim;
        property StrsEmpty2Null: Boolean read FStrsEmpty2Null write FStrsEmpty2Null;
        property EnableLongString: Boolean read FEnableLongString write FEnableLongString;
        // -----------
        property CHAR_COUNT: ub4 index OCI_ATTR_CHAR_COUNT read GetUb4Attr write SetUb4Attr;
        property CHARSET_ID: ub2 index OCI_ATTR_CHARSET_ID read GetUb2Attr write SetUb2Attr;
        property CHARSET_FORM: ub1 index OCI_ATTR_CHARSET_FORM read GetUb1Attr write SetUb1Attr;
        property PDPRC: ub2 index OCI_ATTR_PDPRC read GetUb2Attr write SetUb2Attr;
        property PDSCL: ub2 index OCI_ATTR_PDSCL read GetUb2Attr write SetUb2Attr;
        property ROWS_RETURNED: ub4 index OCI_ATTR_ROWS_RETURNED read GetUB4Attr;
        property MAXDATA_SIZE: sb4 index OCI_ATTR_MAXDATA_SIZE read GetSb4Attr write SetSb4Attr;
    end;

    TOCIDescriptor = class(TOCIHandle)
    private
        function Init(AEnv: TOCIHandle; AType: ub4): pOCIDescriptor;
    public
        destructor Destroy; override;
    end;

    TOCISelectItem = class(TOCIDescriptor)
    private
        FPosition: sb4;
        FEnableInteger: Boolean;
        FEnableBCD: Boolean;
        FEnableNumber: Boolean;
        FEnableLongString: Boolean;
        FEnableFixedString: Boolean;
        FEnableRequired: Boolean;
        function GetDataType: TOCIVarDataType;
        function GetDataSize: ub4;
        function GetVarType: TOCIVarType;
        function GetIsNull: Boolean;
    public
        constructor Create(AStmt: TOCIStatement);
        destructor Destroy; override;
        procedure Bind;
        property Position: sb4 read FPosition write FPosition;
        property DataType: TOCIVarDataType read GetDataType;
        property DataSize: ub4 read GetDataSize;
        property VarType: TOCIVarType read GetVarType;
        property IsNull: Boolean read GetIsNull;
        property EnableInteger: Boolean read FEnableInteger write FEnableInteger;
        property EnableBCD: Boolean read FEnableBCD write FEnableBCD;
        property EnableNumber: Boolean read FEnableNumber write FEnableNumber;
        property EnableLongString: Boolean read FEnableLongString write FEnableLongString;
        property EnableFixedString: Boolean read FEnableFixedString write FEnableFixedString;
        property EnableRequired: Boolean read FEnableRequired write FEnableRequired;
        property DATA_SIZE: ub4 index OCI_ATTR_DATA_SIZE read GetUb4Attr;
        property DATA_TYPE: ub4 index OCI_ATTR_DATA_TYPE read GetUb4Attr;
        property DISP_SIZE: ub4 index OCI_ATTR_DISP_SIZE read GetUb4Attr;
        property NAME: String index OCI_ATTR_NAME read GetStringAttr;
        property PRECISION: ub4 index OCI_ATTR_PRECISION read GetUb4Attr;
        property SCALE: ub4 index OCI_ATTR_SCALE read GetUb4Attr;
        property IS_NULL: ub4 index OCI_ATTR_IS_NULL read GetUb4Attr;
        property TYPE_NAME: String index OCI_ATTR_TYPE_NAME read GetStringAttr;
        property SUB_NAME: String index OCI_ATTR_SUB_NAME read GetStringAttr;
        property SCHEMA_NAME: String index OCI_ATTR_SCHEMA_NAME read GetStringAttr;
        property IOMODE: ub1 index OCI_ATTR_IOMODE read GetUB1Attr;
    end;

    TOCILobLocator = class(TOCIDescriptor)
    private
        FOwned: Boolean;
        function GetLength: sb4;
        function GetIsOpen: LongBool;
        function GetIsInit: LongBool;
        function GetIsTemporary: LongBool;
    public
        constructor Create(AService: TOCIService; AType: ub4); virtual;
        constructor CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle); virtual;
        constructor CreateTemporary(AService: TOCIService; AType: ub4; ACache: Boolean); virtual;
        destructor Destroy; override;
        procedure Assign(AFrom: TOCILobLocator);
        procedure Close;
        procedure Open(AReadOnly: Boolean);
        procedure Read(ABuff: pUb1; ABuffLen: ub4; var amount: ub4; offset: ub4);
        property Length: sb4 read GetLength;
        property IsOpen: LongBool read GetIsOpen;
        property IsInit: LongBool read GetIsInit;
        property IsTemporary: LongBool read GetIsTemporary;
    end;

    TOCIIntLocator = class(TOCILobLocator)
    private
        FBuffering: Boolean;
        procedure SetBuffering(const Value: Boolean);
    public
        constructor Create(AService: TOCIService; AType: ub4); override;
        constructor CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle); override;
        procedure Append(AFrom: TOCIIntLocator);
        procedure Copy(AFrom: TOCIIntLocator; amount, dst_offset, src_offset: ub4);
        procedure Erase(var amount: ub4; offset: ub4);
        procedure FlushBuffer;
        procedure LoadFromFile(AFrom: TOCIExtLocator; amount, dst_offset, src_offset: ub4);
        procedure Trim(ANewLen: ub4);
        procedure Write(ABuff: pUb1; ABuffLen: ub4; var amount: ub4; offset: ub4);
        property Buffering: Boolean read FBuffering write SetBuffering;
    end;

    TOCIExtLocator = class(TOCILobLocator)
    private
        function GetFileExists: Boolean;
        function GetDirectory: String;
        function GetFileName: String;
        procedure SetDirectory(const Value: String);
        procedure SetFileName(const Value: String);
        procedure GetFileDir(var ADir, AFileName: String);
        procedure SetFileDir(const ADir, AFileName: String);
    public
        constructor Create(AService: TOCIService; AType: ub4); override;
        constructor CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle); override;
        property FileExists: Boolean read GetFileExists;
        property FileName: String read GetFileName write SetFileName;
        property Directory: String read GetDirectory write SetDirectory;
    end;

    TOCIDescribe = class(TOCIHandle)
    private
        FCurr: TOCIHandle;
        FStack: TList;
        function GetPtr(AIndex: Integer): Pointer;
        function GetSB1(AIndex: Integer): sb1;
        function GetText(AIndex: Integer): String;
        function GetUB1(AIndex: Integer): ub1;
        function GetUB2(AIndex: Integer): ub2;
        function GetUB4(AIndex: Integer): ub4;
        function GetSelectItem(AIndex: Integer): TOCISelectItem;
        function GetIsListOpened: Boolean;
    public
        constructor Create(ASvc: TOCIService);
        destructor Destroy; override;
        procedure DescribeName(const AName: String);
        function OpenList(AAtrType: Integer): ub4;
        procedure CloseList;
        procedure GoToItem(AIndex: Integer);
        property SB1[AIndex: Integer]: sb1 read GetSB1;
        property UB1[AIndex: Integer]: ub1 read GetUB1;
        property UB2[AIndex: Integer]: ub2 read GetUB2;
        property UB4[AIndex: Integer]: ub4 read GetUB4;
        property TEXT[AIndex: Integer]: String read GetText;
        property PTR[AIndex: Integer]: Pointer read GetPtr;
        property SelectItem[AIndex: Integer]: TOCISelectItem read GetSelectItem;
        property IsListOpened: Boolean read GetIsListOpened; 
    end;

    TOCIDirectPath = class(TOCIHandle)
    private
        function GetColumns(AIndex: Integer): TOCIDirectPathColumnParam;
    public
        constructor Create(ASvc: TOCIService);
        procedure Abort;
        procedure Finish;
        procedure Prepare;
        procedure LoadStream(AStream: TOCIDirectPathStream);
        property BUF_SIZE: ub4 index OCI_ATTR_BUF_SIZE read GetUB4Attr write SetUB4Attr;
        property CHARSET_ID: ub2 index OCI_ATTR_CHARSET_ID read GetUB2Attr write SetUB2Attr;
        property DATEFORMAT: String index OCI_ATTR_DATEFORMAT read GetStringAttr write SetStringAttr;
        property DIRPATH_MODE: ub1 index OCI_ATTR_DIRPATH_MODE read GetUB1Attr write SetUB1Attr;
        property DIRPATH_NOLOG: ub1 index OCI_ATTR_DIRPATH_NOLOG read GetUB1Attr write SetUB1Attr;
        property DIRPATH_PARALLEL: ub1 index OCI_ATTR_DIRPATH_PARALLEL read GetUB1Attr write SetUB1Attr;
        property NUM_COLS: ub2 index OCI_ATTR_NUM_COLS read GetUB2Attr write SetUB2Attr;
        property LIST_COLUMNS: pOCIHandle index OCI_ATTR_LIST_COLUMNS read GetHandleAttr;
        property SCHEMA_NAME: String index OCI_ATTR_SCHEMA_NAME read GetStringAttr write SetStringAttr;
        property NAME: String index OCI_ATTR_NAME read GetStringAttr write SetStringAttr;
        property SUB_NAME: String index OCI_ATTR_SUB_NAME read GetStringAttr write SetStringAttr;
        property Columns[AIndex: Integer]: TOCIDirectPathColumnParam read GetColumns;
    end;

    TOCIDirecPathDataType = (dtUnknown, dtString, dtDateTime, dtInteger,
        dtUInteger, dtFloat, dtRaw);
    TOCIDirecPathDataTypes = set of TOCIDirecPathDataType;
    TOCIDirectPathColArray = class(TOCIHandle)
    public
        constructor Create(ADP: TOCIDirectPath);
        procedure EntryGet(ARowNum: ub4; AColIndex: ub2; var AData: pUb1; var ALen: ub4; var AFlag: ub1);
        procedure EntrySet(ARowNum: ub4; AColIndex: ub2; AData: pUb1; ALen: ub4; AFlag: ub1);
        procedure RowGet(ARowNum: ub4; var ADataArr: ppUb1; var ALenArr: pUb4; var AFlagArr: pUb1);
        procedure Reset;
        function ToStream(AStream: TOCIDirectPathStream; ARowCount, ARowIndex: ub4): sword;
        property NUM_COLS: ub2 index OCI_ATTR_NUM_COLS read GetUB2Attr write SetUB2Attr;
        property NUM_ROWS: ub2 index OCI_ATTR_NUM_ROWS read GetUB2Attr write SetUB2Attr;
        property COL_COUNT: ub2 index OCI_ATTR_COL_COUNT read GetUB2Attr write SetUB2Attr;
        property ROW_COUNT: ub2 index OCI_ATTR_ROW_COUNT read GetUB2Attr write SetUB2Attr;
    end;

    TOCIDirectPathStream = class(TOCIHandle)
    public
        constructor Create(ADP: TOCIDirectPath);
        procedure Reset;
        property BUF_ADDR: Pointer index OCI_ATTR_BUF_ADDR read GetHandleAttr write SetHandleAttr;
        property BUF_SIZE: ub4 index OCI_ATTR_BUF_SIZE read GetUB4Attr write SetUB4Attr;
        property ROW_COUNT: ub4 index OCI_ATTR_ROW_COUNT read GetUB4Attr write SetUB4Attr;
        property STREAM_OFFSET: ub4 index OCI_ATTR_STREAM_OFFSET read GetUB4Attr write SetUB4Attr;
    end;

    TOCIDirectPathColumnParam = class(TOCIDescriptor)
    private
        function GetDataType: TOCIDirecPathDataType;
        procedure SetDataType(AValue: TOCIDirecPathDataType);
    public
        constructor Create(ADP: TOCIDirectPath);
        property CHARSET_ID: ub2 index OCI_ATTR_CHARSET_ID read GetUB2Attr write SetUB2Attr;
        property DATA_SIZE: ub4 index OCI_ATTR_DATA_SIZE read GetUB4Attr write SetUB4Attr;
        property DATA_TYPE: ub2 index OCI_ATTR_DATA_TYPE read GetUB2Attr write SetUB2Attr;
        property DATEFORMAT: String index OCI_ATTR_DATEFORMAT read GetStringAttr write SetStringAttr;
        property NAME: String index OCI_ATTR_NAME read GetStringAttr write SetStringAttr;
        property PRECISION: ub1 index OCI_ATTR_PRECISION read GetUB1Attr write SetUB1Attr;
        property SCALE: sb1 index OCI_ATTR_SCALE read GetSB1Attr write SetSB1Attr;
        property DataType: TOCIDirecPathDataType read GetDataType write SetDataType;
    end;

const
    otHTypes: TOCIVarDataTypes = [otCursor, otNestedDataSet, otCLOB, otBLOB, otCFile, otBFile];
    otHBlobs: TOCIVarDataTypes = [otCLOB, otBLOB, otCFile, otBFile];
    otVBlobs: TOCIVarDataTypes = [otLong, otLongRaw];
    otAllBlobs: TOCIVarDataTypes = [otLong, otLongRaw, otCLOB, otBLOB, otCFile, otBFile];

    nc2ociValueType: array[TOCIVarDataType] of ub2 = (0, SQLT_INT, SQLT_INT,
        SQLT_FLT, SQLT_FLT, SQLT_CHR, SQLT_CHR, SQLT_AFC, SQLT_BIN, SQLT_DAT,
        SQLT_LNG, SQLT_LBI, SQLT_RSET, SQLT_RSET, SQLT_CLOB, SQLT_BLOB,
        SQLT_CFILEE, SQLT_BFILEE, SQLT_CHR);
    nc2ociValueSize: array[TOCIVarDataType] of sb4 = (0, SizeOf(Integer),
        SizeOf(Integer), SizeOf(Double), SizeOf(Double), IMaxNumPrecision+2,
        IDefStrSize, IDefStrSize, IDefStrSize, SizeOf(TOraDate), IDefLongSize,
        IDefLongSize, sizeof(pOCIStmt), sizeof(pOCIStmt), sizeof(pOCILobLocator),
        sizeof(pOCILobLocator), sizeof(pOCILobLocator), sizeof(pOCILobLocator),
        IRowIdLen);

    nc2ociDPDataType: array[TOCIDirecPathDataType] of ub2 = (0, SQLT_CHR, SQLT_DAT,
        SQLT_INT, SQLT_UIN, SQLT_FLT, SQLT_BIN);
    nc2ociDPValueSize: array[TOCIDirecPathDataType] of sb4 = (0, IDefStrSize, SizeOf(TOraDate),
        SizeOf(Integer), SizeOf(Cardinal), SizeOf(Double), IDefStrSize);

    procedure OraDate2Delphi(pIn: pUb1; var D: TDateTime);
    procedure Delphi2OraDate(pOut: pUb1; D: TDateTime);

const
    dataTypeName: array[TOCIVarDataType] of String =
      ('otUnknown', 'otSmallInt', 'otInteger', 'otFloat', 'otBCD', 'otNumber',
       'otString', 'otChar', 'otRaw', 'otDateTime', 'otLong', 'otLongRaw',
       'otCursor', 'otDataSet', 'otCLOB', 'otBLOB', 'otCFile', 'otBFile',
       'otROWID');
    varTypeName: array[TOCIVarType] of String =
      ('odUnknown', 'odIn', 'odOut', 'odInOut', 'odRet', 'odDefine');

implementation

Uses NCOciMsg, NCOciUtil;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

const
    hndlNames: array[1..64] of String =
   ('OCI_HTYPE_ENV',
    'OCI_HTYPE_ERROR',
    'OCI_HTYPE_SVCCTX',
    'OCI_HTYPE_STMT',
    'OCI_HTYPE_BIND',
    'OCI_HTYPE_DEFINE',
    'OCI_HTYPE_DESCRIBE',
    'OCI_HTYPE_SERVER',
    'OCI_HTYPE_SESSION',
    'OCI_HTYPE_TRANS',
    'OCI_HTYPE_COMPLEXOBJECT',
    'OCI_HTYPE_SECURITY',
    'OCI_HTYPE_SUBSCRIPTION',
    'OCI_HTYPE_DIRPATH_CTX',
    'OCI_HTYPE_DIRPATH_COLUMN_ARRAY',
    'OCI_HTYPE_DIRPATH_STREAM',
    'OCI_HTYPE_PROC',
    '','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','',
    'OCI_DTYPE_LOB',
    'OCI_DTYPE_SNAP',
    'OCI_DTYPE_RSET',
    'OCI_DTYPE_PARAM',
    'OCI_DTYPE_ROWID',
    'OCI_DTYPE_COMPLEXOBJECTCOMP',
    'OCI_DTYPE_FILE',
    'OCI_DTYPE_AQENQ_OPTIONS',
    'OCI_DTYPE_AQDEQ_OPTIONS',
    'OCI_DTYPE_AQMSG_PROPERTIES',
    'OCI_DTYPE_AQAGENT',
    'OCI_DTYPE_LOCATOR',
    'OCI_DTYPE_DATETIME',
    'OCI_DTYPE_INTERVAL',
    'OCI_DTYPE_AQNFY_DESCRIPTOR'
   );

constructor TOCIHandle.Create;
begin
    inherited Create;
    FHandle := nil;
    FType := 0;
    FOwner := nil;
end;

destructor TOCIHandle.Destroy;
begin
    if (FHandle <> nil) and FOwnHandle then
        Check('OCIHandleFree(' + hndlNames[FType] + ')',
            NCOci.OCIHandleFree(FHandle, FType));
    inherited Destroy;
end;

function TOCIHandle.Init(AEnv: TOCIHandle; AType: ub4): pOCIHandle;
begin
    Check('OCIHandleAlloc(' + hndlNames[AType] + ')',
        NCOci.OCIHandleAlloc(AEnv.FHandle, FHandle, AType, 0, nil));
    FType := AType;
    Result := @FHandle;
    FOwnHandle := True;
end;

procedure TOCIHandle.SetStringAttr(AAtrType: Integer; const AValue: String);
begin
    Check('OCIAttrSet Str', NCOci.OCIAttrSet(FHandle, FType, PChar(AValue),
        Length(AValue), AAtrType, Error.FHandle));
end;

function TOCIHandle.GetStringAttr(AAtrType: Integer): String;
var
    l: Integer;
    p: PChar;
begin
    Check('OCIAttrGet Str', NCOci.OCIAttrGet(FHandle, FType, @p, @l,
        AAtrType, Error.FHandle));
    SetString(Result, p, l);
end;

procedure TOCIHandle.SetHandleAttr(AAtrType: Integer; AValue: pOCIHandle);
begin
    Check('OCIAttrSet Hndl', NCOci.OCIAttrSet(FHandle, FType, AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetHandleAttr(AAtrType: Integer): pOCIHandle;
begin
    Result := nil;
    Check('OCIAttrGet Hndl', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

procedure TOCIHandle.SetUB1Attr(AAtrType: Integer; AValue: ub1);
begin
    Check('OCIAttrSet ub1', NCOci.OCIAttrSet(FHandle, FType, @AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetUB1Attr(AAtrType: Integer): ub1;
begin
    Result := 0;
    Check('OCIAttrGet ub1', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

procedure TOCIHandle.SetUB2Attr(AAtrType: Integer; AValue: ub2);
begin
    Check('OCIAttrSet ub2', NCOci.OCIAttrSet(FHandle, FType, @AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetUB2Attr(AAtrType: Integer): ub2;
begin
    Result := 0;
    Check('OCIAttrGet ub2', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

procedure TOCIHandle.SetUB4Attr(AAtrType: Integer; AValue: ub4);
begin
    Check('OCIAttrSet ub4', NCOci.OCIAttrSet(FHandle, FType, @AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetUB4Attr(AAtrType: Integer): ub4;
begin
    Result := 0;
    Check('OCIAttrGet ub4', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

procedure TOCIHandle.SetSB1Attr(AAtrType: Integer; AValue: Sb1);
begin
    Check('OCIAttrSet sb1', NCOci.OCIAttrSet(FHandle, FType, @AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetSB1Attr(AAtrType: Integer): sb1;
begin
    Result := 0;
    Check('OCIAttrGet sb1', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

procedure TOCIHandle.SetSB4Attr(AAtrType: Integer; AValue: Sb4);
begin
    Check('OCIAttrSet sb4', NCOci.OCIAttrSet(FHandle, FType, @AValue, 0,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetSB4Attr(AAtrType: Integer): sb4;
begin
    Result := 0;
    Check('OCIAttrGet sb4', NCOci.OCIAttrGet(FHandle, FType, @Result, nil,
        AAtrType, Error.FHandle));
end;

function TOCIHandle.GetError: TOCIError;
var
    h: TOCIHandle;
begin
    Result := FError;
    if Result = nil then begin
        h := Self;
        while (h <> nil) and not (h is TOCIEnvironment) do
            h := h.Owner;
        FError := TOCIEnvironment(h).FError;
        Result := FError;
    end;
end;

function TOCIHandle.GetSQLMonitor: TNCSQLMonitorClient;
var
    h: TOCIHandle;
begin
    if Integer(FSQLMonitor) = -1 then
        Result := nil
    else begin
        h := Self;
        while (h <> nil) and not (h is TOCIEnvironment) do
            h := h.Owner;
        with TOCIEnvironment(h) do
            if (h = nil) or (FSQLMonitor = nil) or not FSQLMonitor.Active then begin
                Self.FSQLMonitor := Pointer(-1);
                Result := nil;
            end
            else begin
                Self.FSQLMonitor := FSQLMonitor;
                Result := FSQLMonitor;
            end;
    end;
end;

procedure TOCIHandle.Check(const AMsg: String; ACode: sb4);
begin
    DbgOut(tfVendor, AMsg);
    if ACode <> OCI_SUCCESS then
        Error.Check(OwningObj, AMsg, ACode);
end;

function TOCIHandle.DbgOutActive: Boolean;
begin
    Result := (SQLMonitor <> nil) and SQLMonitor.Active;
end;

procedure TOCIHandle.DbgOut(AType: TNCTraceFlag; const AStmt: String);
begin
    if DbgOutActive then
        SQLMonitor.AddStatement(AType, 'OCI8 - ' + AStmt);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

{$IFDEF OCI_DEBUG}
function Malocf(ctxp: Pointer; size: Integer): Pointer; cdecl;
begin
    GetMem(Result, size);
end;

function Ralocf(ctxp: Pointer; memptr: Pointer; newsize: Integer): Pointer; cdecl;
begin
    Result := memptr;
    ReallocMem(Result, newsize);
end;

procedure Mfreef(ctxp: Pointer; memptr: Pointer); cdecl;
begin
    FreeMem(memptr);
end;
{$ENDIF}

constructor TOCIEnvironment.Create(AModes: TOCIDatabaseModes;
                                   ASQLMonitor: TNCSQLMonitorClient;
                                   AOnError: TOCIErrorEvent;
                                   AOnWarning: TOCIWarningEvent);
var
    mode: ub4;
    err1, err2: sword;
begin
    inherited Create;
    FType := OCI_HTYPE_ENV;
    FOwnHandle := True;
    mode := OCI_DEFAULT;
    if dmThreaded in AModes then
        mode := mode or OCI_THREADED;
    if dmObject in AModes then
        mode := mode or OCI_OBJECT;
    if (FOCIVersion >= cvOracle81000) and (dmEvents in AModes) then
        mode := mode or OCI_EVENTS;
    if (FOCIVersion >= cvOracle81000) and (dmShared in AModes) then
        mode := mode or OCI_SHARED;
    InitOCI;
    if FOCIVersion >= cvOracle81000 then begin
        err1 := NCOci.OCIEnvCreate(FHandle, mode, nil, nil, nil, nil, 0, nil);
        err2 := OCI_SUCCESS;
    end
    else begin
{$IFDEF OCI_DEBUG}
        err1 := NCOci.OCIInitialize(mode, nil, @malocf, @ralocf, @mfreef);
{$ELSE}
        err1 := NCOci.OCIInitialize(mode, nil, nil, nil, nil);
{$ENDIF}
        err2 := NCOci.OCIEnvInit(FHandle, OCI_DEFAULT, 0, nil);
    end;
    FSQLMonitor := ASQLMonitor;
    FError := TOCIError.Create(Self);
    FError.OnError := AOnError;
    FError.OnWarning := AOnWarning;
    if FOCIVersion >= cvOracle81000 then
        Check('OCIEnvCreate', err1)
    else begin
        Check('OCIInitialize', err1);
        Check('OCIEnvInit', err2);
    end;
end;

constructor TOCIEnvironment.CreateUsingHandle(AHandle: pOCIHandle;
    AErrHandle: pOCIHandle; ASQLMonitor: TNCSQLMonitorClient;
    AOnError: TOCIErrorEvent; AOnWarning: TOCIWarningEvent);
begin
    inherited Create;
    FType := OCI_HTYPE_ENV;
    FHandle := AHandle;
    FOwnHandle := False;
    InitOCI;
    FSQLMonitor := ASQLMonitor;
    FError := TOCIError.CreateUsingHandle(Self, AErrHandle);
    FError.OnError := AOnError;
    FError.OnWarning := AOnWarning;
end;

destructor TOCIEnvironment.Destroy;
begin
    FError.Free;
    FError := nil;
    inherited Destroy;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIErrorMessage.Create(AOwner: EOCINativeError;
    ALevel: ub4; AErrorCode: sb4; AMessage: PChar);
var
    i1, i2: Integer;
    s: String;    
begin
    inherited Create;
    AOwner.FErrors.Add(Self);
    FLevel := ALevel;
    FErrorCode := AErrorCode;
    FMessage := AMessage;
    i1 := Pos('#13', FMessage);
    i2 := Pos('#10', FMessage);
    if (i2 <> 0) and (i2 < i1) or (i1 = 0) then
        i1 := i2;
    if i1 = 0 then
        i1 := Length(FMessage) + 1;
    s := Copy(FMessage, 1, i1 - 1);
    i1 := Pos('(', s);
    i2 := Pos(')', s);
    FServerObject := Copy(s, i1 + 1, i2 - i1 - 1);
end;

constructor EOCINativeError.Create(const AMessage: String; AError: TOCIError);
var
    lvl: ub4;
    errCode: sb4;
    errBuf: array[0..511] of Char;
begin
    inherited Create(AMessage);
    FErrors := TList.Create;
    lvl := 1;
    AError.DbgOut(tfError, AMessage);
    while NCOci.OCIErrorGet(AError.FHandle, lvl, nil, errCode, errBuf, sizeof(errBuf),
                            OCI_HTYPE_ERROR) <> OCI_NO_DATA do begin
        TOCIErrorMessage.Create(Self, lvl, errCode, errBuf);
        AError.DbgOut(tfError, errBuf);
        if lvl = 1 then
            Message := Message + #13#10 + errBuf;
        Inc(lvl);
    end;
end;

destructor EOCINativeError.Destroy;
var
    i: Integer;
begin
    for i := 0 to FErrors.Count - 1 do
        TOCIErrorMessage(FErrors[i]).Free;
    FErrors.Free;
    inherited Destroy;
end;

function EOCINativeError.GetError(Index: Integer): TOCIErrorMessage;
begin
    Result := TOCIErrorMessage(FErrors[Index]);
end;

function EOCINativeError.GetErrorCount: Integer;
begin
    Result := FErrors.Count;
end;

{$IFDEF OCI_D4}
    {$IFDEF OCI_BCB}
constructor EOCISystemError.Create(AErrorCode: sb4; Dummy: Cardinal = 0);
    {$ELSE}
constructor EOCISystemError.Create(AErrorCode: sb4);
    {$ENDIF}
{$ELSE}
constructor EOCISystemError.Create(AErrorCode: sb4);
{$ENDIF}
var
    msg: String;
begin
    case AErrorCode of
    OCI_SUCCESS_WITH_INFO: msg := 'OCI_SUCCESS_WITH_INFO';
    OCI_NEED_DATA:         msg := 'OCI_NEED_DATA';
    OCI_NO_DATA:           msg := 'OCI_NO_DATA';
    OCI_INVALID_HANDLE:    msg := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:   msg := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:          msg := 'OCI_CONTINUE';
    end;
    FErrorCode := AErrorCode;
    inherited Create(msg);
end;

constructor TOCIError.Create(AEnv: TOCIEnvironment);
begin
    inherited Create;
    FOwner := AEnv;
    Init(AEnv, OCI_HTYPE_ERROR);
end;

constructor TOCIError.CreateUsingHandle(AEnv: TOCIEnvironment; AErrHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AEnv;
    FType := OCI_HTYPE_ERROR;
    FHandle := AErrHandle;
    FOwnHandle := False;
end;

destructor TOCIError.Destroy;
begin
    ClearWarning;
    inherited Destroy;
end;

procedure TOCIError.Check(ASender: TObject; const AMsg: String; ACode: sword);

    procedure ProceedWithErrorObj(AErr: Exception; AHndlr: TOCIErrorEvent; ARaise: Boolean);
    var
        sndr: TObject;
    begin
        if Assigned(AHndlr) then begin
            sndr := ASender;
            if sndr = nil then
                sndr := TObject(TMethod(AHndlr).Data);
            AHndlr(sndr, AErr);
        end;
        if (AErr <> nil) and ARaise then
            raise AErr;
    end;

begin
    case ACode of
    OCI_SUCCESS: ;
    OCI_NEED_DATA, OCI_NO_DATA, OCI_INVALID_HANDLE,
    OCI_STILL_EXECUTING, OCI_CONTINUE:
        ProceedWithErrorObj(EOCISystemError.Create(ACode), FOnError, True);
    OCI_SUCCESS_WITH_INFO:
        begin
            ClearWarning;
            FWarning := EOCINativeError.Create('OCI_SUCCESS_WITH_INFO', self);
            ProceedWithErrorObj(FWarning, FOnWarning, False);
        end;
    OCI_ERROR:
        ProceedWithErrorObj(EOCINativeError.Create('OCI_ERROR', self), FOnError, True);
    else ;
    end;
end;

procedure TOCIError.ClearWarning;
begin
    if FWarning <> nil then begin
        FWarning.Free;
        FWarning := nil;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIService.Create(AEnv: TOCIEnvironment);
begin
    inherited Create;
    FOwner := AEnv;
    Init(AEnv, OCI_HTYPE_SVCCTX);
    FNonBlockinMode := True;
end;

constructor TOCIService.CreateUsingHandle(AEnv: TOCIEnvironment; AHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AEnv;
    FType := OCI_HTYPE_SVCCTX;
    FHandle := AHandle;
    FNonBlockinMode := False;
    FOwnHandle := False;
end;

procedure TOCIService.Break(ADoException: Boolean);
var
    errCode: sb4;
begin
    errCode := NCOci.OCIBreak(FHandle, Error.FHandle);
    if (errCode <> OCI_SUCCESS) and ADoException then
        Check('OCIBreak', errCode)
    else
        Check('OCIBreak', OCI_SUCCESS);
end;

procedure TOCIService.Reset(ADoException: Boolean);
var
    errCode: sb4;
begin
    if FOCIVersion >= cvOracle81000 then begin
        errCode := NCOci.OCIReset(FHandle, Error.FHandle);
        if (errCode <> OCI_SUCCESS) and ADoException then
            Check('OCIReset', errCode)
        else
            Check('OCIReset', OCI_SUCCESS);
    end;
end;

procedure TOCIService.DoYield;
begin
    if Assigned(FOnYield) then
        FOnYield(Self);
end;

procedure TOCIService.TurnNONBLOCKING_MODE;
begin
    if FOCIVersion >= cvOracle80500 then
        Check('OCIAttrSet none', NCOci.OCIAttrSet(FHandle, FType, nil, 0,
            OCI_ATTR_NONBLOCKING_MODE, Error.FHandle));
end;

procedure TOCIService.SetNonblockingMode(const Value: Boolean);
begin
    if FNonBlockinMode <> Value then begin
        FNonBlockinMode := Value;
        TurnNONBLOCKING_MODE;
    end;
end;

procedure TOCIService.UpdateNonBlockingMode;
begin
    if FNonBlockinMode then begin
        TurnNONBLOCKING_MODE;
        TurnNONBLOCKING_MODE;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIServer.Create(AService: TOCIService);
begin
    inherited Create;
    FOwner := AService;
    Init(AService.FOwner, OCI_HTYPE_SERVER);
end;

constructor TOCIServer.CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AService;
    FType := OCI_HTYPE_SERVER;
    FHandle := AHandle;
    FOwnHandle := False;
end;

destructor TOCIServer.Destroy;
begin
    try
        Detach;
    except
    end;
    inherited Destroy;
end;

procedure TOCIServer.Attach(const AServerName: String);
begin
    Check('OCIServerAttach(' + AServerName + ')',
        NCOci.OCIServerAttach(FHandle, Error.FHandle,
        PChar(AServerName), Length(AServerName), OCI_DEFAULT));
    FAttached := True;
    Select;
end;

procedure TOCIServer.Detach;
begin
    if (FHandle <> nil) and FAttached then begin
        FAttached := False;
        Check('OCIServerDetach', NCOci.OCIServerDetach(FHandle, Error.FHandle, OCI_DEFAULT));
    end;
end;

procedure TOCIServer.Select;
begin
    TOCIService(FOwner).SERVER := FHandle;
end;

function TOCIServer.GetServerVersion: String;
const
    OCI_MAX_ATTR_LEN = 1024;
var
    buff: array[0 .. OCI_MAX_ATTR_LEN-1] of char;
begin
    OCIServerVersion(FHandle, Error.FHandle, @buff, OCI_MAX_ATTR_LEN, OCI_HTYPE_SERVER);
    Result := buff;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCISession.Create(AService: TOCIService);
begin
    inherited Create;
    FOwner := AService;
    Init(AService.FOwner, OCI_HTYPE_SESSION);
end;

constructor TOCISession.CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AService;
    FType := OCI_HTYPE_SESSION;
    FHandle := AHandle;
    FOwnHandle := False;
end;

destructor TOCISession.Destroy;
begin
    try
        Stop;
    except
    end;
    inherited Destroy;
end;

procedure TOCISession.Start(const AUser, APassword: String; AAuthentMode: TOCIAuthentMode);
var
    credt: ub4;
    mode: ub4;
begin
    if (AUser = '') and (APassword = '') then
        credt := OCI_CRED_EXT
    else begin
        USERNAME := AUser;
        PASSWORD := APassword;
        credt := OCI_CRED_RDBMS;
    end;
    mode := OCI_DEFAULT;
    if AAuthentMode = amSysDba then
        mode := OCI_SYSDBA
    else if AAuthentMode = amSysOper then
        mode := OCI_SYSOPER;
    Check('OCISessionBegin(' + AUser + ')', NCOci.OCISessionBegin(FOwner.FHandle,
        Error.FHandle, FHandle, credt, mode));
    FStarted := True;
    Select;
end;

procedure TOCISession.ChangePassword(const AUser, AOldPassword, ANewPassword: String);
begin
    Check('OCIPasswordChange', NCOci.OCIPasswordChange(FOwner.FHandle,
        Error.FHandle, PChar(AUser), Length(AUser), PChar(AOldPassword),
        Length(AOldPassword), PChar(ANewPassword), Length(ANewPassword),
        OCI_DEFAULT));
    FStarted := True;
end;

procedure TOCISession.Stop;
begin
    if (FHandle <> nil) and FStarted then begin
        FStarted := False;
        Check('OCISessionEnd', NCOci.OCISessionEnd(FOwner.FHandle, Error.FHandle,
            FHandle, OCI_DEFAULT));
    end;
end;

procedure TOCISession.Select;
begin
    TOCIService(FOwner).SESSION := FHandle;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

constructor TOCIXid.Create;
begin
    inherited Create;
    Clear;
end;

procedure TOCIXid.Assign(AValue: TPersistent);
begin
    if AValue is TOCIXid then begin
        GTRID := TOCIXid(AValue).GTRID;
        BQUAL := TOCIXid(AValue).BQUAL;
        Name := TOCIXid(AValue).Name;
    end
    else
        inherited Assign(AValue);
end;

procedure TOCIXid.Clear;
begin
    if IsNull then
        Exit;
    if Assigned(FOnChanging) then
        FOnChanging(Self);
    FXID.formatID := NULLXID_ID;
    FXID.gtrid_length := 0;
    FXID.bqual_length := 0;
    FillChar(FXID.data, XIDDATASIZE, 0);
    FName := '';
    if Assigned(FOnChanged) then
        FOnChanged(Self);
end;

function TOCIXid.GetGTRID: String;
begin
    SetString(Result, PChar(@FXID.data), FXID.gtrid_length);
end;

procedure TOCIXid.SetParts(const AGTRID, ABQUAL: String);
begin
    if Length(AGTRID) > MAXGTRIDSIZE then
        OCIDBErrorFmt(msgOCITooLongGTRID, [MAXGTRIDSIZE, Length(AGTRID)], nil);
    if Length(ABQUAL) > MAXBQUALSIZE then
        OCIDBErrorFmt(msgOCITooLongBQUAL, [MAXBQUALSIZE, Length(ABQUAL)], nil);
    if Assigned(FOnChanging) then
        FOnChanging(Self);
    FXID.gtrid_length := Length(AGTRID);
    FXID.bqual_length := Length(ABQUAL);
    if (FXID.gtrid_length > 0) and (FXID.bqual_length > 0) then
        FXID.formatID := IXIDFormatID;
    Move(PChar(AGTRID)^, FXID.data, FXID.gtrid_length);
    Move(PChar(ABQUAL)^, PChar(Integer(@FXID.data) + FXID.gtrid_length)^,
        FXID.bqual_length);
    if Assigned(FOnChanged) then
        FOnChanged(Self);
end;

procedure TOCIXid.SetGTRID(const AValue: String);
begin
    if GTRID <> AValue then
        SetParts(AValue, BQUAL);
end;

function TOCIXid.GetBQUAL: String;
begin
    SetString(Result, PChar(Integer(@FXID.data) + FXID.gtrid_length),
        FXID.bqual_length);
end;

procedure TOCIXid.SetBQUAL(const AValue: String);
begin
    if BQUAL <> AValue then
        SetParts(GTRID, AValue);
end;

function TOCIXid.GetIsNull: Boolean;
begin
    Result := (FXID.formatID = NULLXID_ID);
end;

procedure TOCIXid.SetName(const AValue: String);
begin
    if FName <> AValue then begin
        if Length(AValue) > MAXTXNAMELEN then
            OCIDBErrorFmt(msgOCITooLongTXName, [MAXTXNAMELEN, Length(AValue)], nil);
        if Assigned(FOnChanging) then
            FOnChanging(Self);
        FName := AValue;
        if Assigned(FOnChanged) then
            FOnChanged(Self);
    end;
end;

procedure TOCIXid.MarkTransaction(ATrans: TOCITransaction);
begin
    if FXID.formatID <> NULLXID_ID then begin
        ATrans.SetXID(FXID);
        if Name <> '' then
            ATrans.TRANS_NAME := Name;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

const
    nc2OCItm: array[TOCITransactionMode] of ub4 = (OCI_DEFAULT, OCI_TRANS_READWRITE,
        OCI_TRANS_SERIALIZABLE, OCI_TRANS_READONLY, OCI_TRANS_READWRITE);
    nc2OCItn: array[Boolean] of ub4 = (OCI_TRANS_RESUME, OCI_TRANS_NEW);
    nc2OCIcm: array[TOCICoupleMode] of ub4 = (OCI_DEFAULT, OCI_TRANS_LOOSE, OCI_TRANS_TIGHT);

constructor TOCITransaction.Create(AService: TOCIService);
begin
    inherited Create;
    FOwner := AService;
    Init(AService.FOwner, OCI_HTYPE_TRANS);
    Select;
end;

constructor TOCITransaction.CreateUsingHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AService;
    FType := OCI_HTYPE_TRANS;
    FHandle := AHandle;
    FOwnHandle := False;
end;

procedure TOCITransaction.StartLocal(AMode: TOCITransactionMode);
begin
    Check('OCITransStart', NCOci.OCITransStart(FOwner.FHandle, Error.FHandle,
        0, nc2OCItm[AMode]));
end;

procedure TOCITransaction.StartGlobal(AMode: TOCITransactionMode; ATimeOut: ub4;
    ANew: Boolean; ACoupleMode: TOCICoupleMode);
begin
    Check('OCITransStart', NCOci.OCITransStart(FOwner.FHandle, Error.FHandle,
        ATimeOut, nc2OCItn[ANew] or nc2OCIcm[ACoupleMode] or nc2OCItm[AMode]));
end;

procedure TOCITransaction.Detach;
begin
    Check('OCITransDetach', NCOci.OCITransDetach(FOwner.FHandle, Error.FHandle,
        OCI_DEFAULT));
end;

procedure TOCITransaction.Commit;
begin
    Check('OCITransCommit', NCOci.OCITransCommit(FOwner.FHandle, Error.FHandle,
        OCI_DEFAULT));
end;

procedure TOCITransaction.Commit2PC;
begin
    Check('OCITransCommit', NCOci.OCITransCommit(FOwner.FHandle, Error.FHandle,
        OCI_TRANS_TWOPHASE));
end;

procedure TOCITransaction.RollBack;
begin
    Check('OCITransRollback', NCOci.OCITransRollback(FOwner.FHandle, Error.FHandle,
        OCI_DEFAULT));
end;

procedure TOCITransaction.Prepare2PC;
var
    res: sb4;
begin
    res := NCOci.OCITransPrepare(FOwner.FHandle, Error.FHandle, OCI_DEFAULT);
    if res = OCI_SUCCESS_WITH_INFO then
        res := OCI_SUCCESS;
    Check('OCITransPrepare', res);
end;

procedure TOCITransaction.Forget;
begin
    Check('OCITransForget', NCOci.OCITransForget(FOwner.FHandle, Error.FHandle,
        OCI_DEFAULT));
end;

procedure TOCITransaction.SetXID(var AValue: TXID);
begin
    Check('OCIAttrSet XID', NCOci.OCIAttrSet(FHandle, FType, @AValue, SizeOf(TXID),
        OCI_ATTR_XID, Error.FHandle));
end;

procedure TOCITransaction.GetXID(var AValue: TXID);
begin
    Check('OCIAttrGet XID', NCOci.OCIAttrGet(FHandle, FType, @AValue, nil,
        OCI_ATTR_XID, Error.FHandle));
end;

procedure TOCITransaction.Select;
begin
    if TOCIService(FOwner).TRANS <> FHandle then
        TOCIService(FOwner).TRANS := FHandle;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIStatement.Create(AEnv: TOCIEnvironment);
begin
    inherited Create;
    FOwner := AEnv;
    Init(AEnv, OCI_HTYPE_STMT);
    FPieceBuffLen := 4000;
    FPieceOutVars := TList.Create;
    FVars := TList.Create;
    FRef := False;
    FCanceled := False;
end;

constructor TOCIStatement.CreateUsingHandle(AEnv: TOCIEnvironment; AHandle: pOCIHandle);
begin
    inherited Create;
    FOwner := AEnv;
    FType := OCI_HTYPE_STMT;
    FHandle := AHandle;
    FPieceBuffLen := 4000;
    FPieceOutVars := TList.Create;
    FVars := TList.Create;
    FRef := True;
    FCanceled := False;
end;

destructor TOCIStatement.Destroy;
begin
    FPieceOutVars.Free;
    FVars.Free;
    FreePieceBuff;
    if FRef then
        FHandle := nil;
    inherited Destroy;
end;

// -----------------------------------------
// Piece wise operation support

var
    FGlobalPieceBuffSize: ub4;
    FGlobalPieceBuff: pUb1;
    FGlobalPieceBuffUseCnt: ub4;

function GetGlobalPieceBuff(ASize: ub4): pUb1;
begin
    if FGlobalPieceBuffUseCnt = 0 then begin
        GetMem(FGlobalPieceBuff, ASize);
        FGlobalPieceBuffSize := ASize;
    end
    else if FGlobalPieceBuffSize < ASize then begin
        ReallocMem(FGlobalPieceBuff, ASize);
        FGlobalPieceBuffSize := ASize;
    end;
    Inc(FGlobalPieceBuffUseCnt);
    Result := FGlobalPieceBuff;
end;

procedure FreeGlobalPieceBuff;
begin
    Dec(FGlobalPieceBuffUseCnt);
    if FGlobalPieceBuffUseCnt = 0 then begin
        FreeMem(FGlobalPieceBuff, FGlobalPieceBuffSize);
        FGlobalPieceBuff := nil;
        FGlobalPieceBuffSize := 0;
    end;
end;

procedure TOCIStatement.AllocPieceBuff;
begin
    if FPieceBuff = nil then
        if FPieceBuffOwn then
            GetMem(FPieceBuff, FPieceBuffLen)
        else
            FPieceBuff := GetGlobalPieceBuff(FPieceBuffLen);
end;

procedure TOCIStatement.FreePieceBuff;
begin
    if FPieceBuff <> nil then
    try
        if FPieceBuffOwn then
            FreeMem(FPieceBuff, FPieceBuffLen)
        else
            FreeGlobalPieceBuff;
    except
        FPieceBuff := nil;
        raise;
    end;
end;

procedure TOCIStatement.SetPieceBuffOwn(AValue: Boolean);
begin
    if FPieceBuffOwn <> AValue then begin
        FreePieceBuff;
        FPieceBuffOwn := AValue;
    end;
end;

procedure TOCIStatement.SetPieceBuffLen(AValue: ub4);
begin
    if FPieceBuffLen <> AValue then begin
        FreePieceBuff;
        FPieceBuffLen := AValue;
    end;
end;

function TOCIStatement.Hndl2PieceVar(varHndl: pOCIHandle): TOCIVariable;
var
    i: Integer;
begin
    i := 0;
    while (i < FPieceOutVars.Count) and
          (TOCIVariable(FPieceOutVars[i]).FHandle <> varHndl) do
        Inc(i);
    if i = FPieceOutVars.Count then
        OCIDBError(msgOCIUndefPWVar, nil);
    Result := TOCIVariable(FPieceOutVars[i]);
    if not Result.LongData then
        OCIDBError(msgOCIBadTypePWVar, nil);
end;

procedure TOCIStatement.AddPieceVar(AVar: TOCIVariable);
begin
    if FPieceOutVars.IndexOf(AVar) = -1 then
        FPieceOutVars.Add(AVar);
end;

procedure TOCIStatement.RemovePieceVar(AVar: TOCIVariable);
begin
    FPieceOutVars.Remove(AVar);
end;

function TOCIStatement.GetPiecedFetch: Boolean;
begin
    Result := FPieceOutVars.Count > 0;
end;

procedure TOCIStatement.InitPiecedFetch(var pld: TOCIPieceLoopData);
begin
    if PiecedFetch then begin
        DbgOut(tfStmt, 'Init pieced fetch');
        FillChar(pld, SizeOf(TOCIPieceLoopData), 0);
    end;
end;

procedure TOCIStatement.DoPiecedFetch(var APieceLoopData: TOCIPieceLoopData; ARes: sb4);
var
    varHndl: pOCIHandle;
    varType: ub4;
    idx: ub4;
    varIter: ub4;
    pLastInd: Pointer;
    pLastRCode: pUb2;
begin
    if PiecedFetch then
    with APieceLoopData do begin
        DbgOut(tfStmt, 'Fetch piece');
        if (lastPieceVar <> nil) and (lastInOut <> OCI_PARAM_IN) then
            if lastPiece in [OCI_ONE_PIECE, OCI_FIRST_PIECE] then
                if lastInd = -1 then
                    lastPieceVar.SetData(lastIter, nil, 0, dfOCI)
                else
                    lastPieceVar.SetData(lastIter, FPieceBuff, lastPieceSize, dfOCI)
            else
                lastPieceVar.AppendData(lastIter, FPieceBuff, lastPieceSize);
        if ARes = OCI_NEED_DATA then begin
            Check('OCIStmtGetPieceInfo', NCOci.OCIStmtGetPieceInfo(FHandle,
                Error.FHandle, varHndl, varType, lastInOut, varIter, idx, lastPiece));
            if varHndl <> lastPieceVarHndl then begin
                lastPieceVar := Hndl2PieceVar(varHndl);
                lastPieceVarHndl := varHndl;
                lastRCode := 0;
            end;
            if varIter <> lastIter then begin
                lastRCode := 0;
                lastIter := varIter;
            end;
            pLastInd := @lastInd;
            pLastRCode := @lastRCode;
            if FOCIVersion >= cvOracle81600 then begin
                pLastInd := Pointer(ub4(pLastInd) - lastIter * SizeOf(sb2));
                pLastRCode := pUb2(ub4(pLastRCode) - lastIter * SizeOf(ub2));
            end;
            if lastInOut = OCI_PARAM_OUT then begin
                AllocPieceBuff;
                lastPieceSize := FPieceBuffLen;
                Check('OCIStmtSetPieceInfo', NCOci.OCIStmtSetPieceInfo(lastPieceVar.FHandle,
                    varType, Error.FHandle, FPieceBuff, lastPieceSize, lastPiece, pLastInd,
                    pLastRCode^));
            end
            else begin
                if lastPieceVar.GetDataPtr(lastIter, lastBuff, lastPieceSize) then
                    lastInd := 1
                else
                    lastInd := -1;
                Check('OCIStmtSetPieceInfo', NCOci.OCIStmtSetPieceInfo(lastPieceVar.FHandle,
                    varType, Error.FHandle, lastBuff, lastPieceSize, OCI_LAST_PIECE, pLastInd,
                    pLastRCode^));
            end;
        end
        else
            lastPieceVar := nil;
    end;
end;

// -----------------------------------------
// Dump variables

procedure TOCIStatement.DumpOutVars(ARows: ub4);
var
    i, j, n: Integer;
    ok: Boolean;
begin
    if DbgOutActive then begin
        ok := False;
        for i := 0 to FVars.Count - 1 do
            with TOCIVariable(FVars[i]) do
                if VarType = odDefine then begin
                    ok := True;
                    Break;
                end;
        if ok then
            for j := 0 to ARows - 1 do begin
                n := 1;
                DbgOut(tfDataOut, Format('Blocks row %d', [j]));
                for i := 0 to FVars.Count - 1 do
                    with TOCIVariable(FVars[i]) do
                        if VarType = odDefine then begin
                            DbgOut(tfDataOut, Format('Column: %d, Name: %s, Type: %s, Size: %d, Data: %s',
                                [n, DumpLabel, dataTypeName[DataType], DataSize, DumpValue(j)]));
                            Inc(n);
                        end;
            end;
    end;
end;

procedure TOCIStatement.DumpInVars(ATypes: TOCIVarTypes);
var
    i, j, n: Integer;
begin
    if DbgOutActive then begin
        n := 1;
        for i := 0 to FVars.Count - 1 do
            with TOCIVariable(FVars[i]) do
                if VarType in ATypes then begin
                    DbgOut(tfDataIn, Format('Param: %d, Name: %s, Mode: %s, Type: %s, Size: %d, Data(0): %s',
                        [n, DumpLabel, varTypeName[VarType], dataTypeName[DataType], DataSize, DumpValue(0)]));
                    for j := 1 to ArrayLen - 1 do
                        DbgOut(tfDataIn, Format('    Data(%d): %s', [j, DumpValue(j)]));
                    Inc(n);
                end;
    end;
end;

// -----------------------------------------
// Actual operation's

procedure TOCIStatement.AddVar(AVar: TOCIVariable);
begin
    if FVars.IndexOf(AVar) = -1 then
        FVars.Add(AVar);
end;

procedure TOCIStatement.RemoveVar(AVar: TOCIVariable);
begin
    FVars.Remove(AVar);
end;

procedure TOCIStatement.Prepare(const AStatement: String);
begin
    DbgOut(tfStmt, 'Prepare');
    Check('OCIStmtPrepare(' + AStatement + ')', NCOci.OCIStmtPrepare(FHandle,
        Error.FHandle, PChar(AStatement), Length(AStatement), OCI_NTV_SYNTAX,
        OCI_DEFAULT));
end;

procedure TOCIStatement.Describe(AService: TOCIService);
var
    res: sb4;
begin
    DbgOut(tfStmt, 'Describe');
    FService := AService;
    FService.UpdateNonBlockingMode;
    repeat
        res := NCOci.OCIStmtExecute(FService.FHandle, FHandle, Error.FHandle,
            0, 0, nil, nil, OCI_DESCRIBE_ONLY);
        case res of
            OCI_STILL_EXECUTING:
                FService.DoYield;
            OCI_NO_DATA, OCI_NEED_DATA, OCI_SUCCESS:
                Check('OCIStmtExecute', OCI_SUCCESS);
            else
                Check('OCIStmtExecute', res);
        end;
    until (res <> OCI_NEED_DATA) and (res <> OCI_STILL_EXECUTING);
end;

procedure TOCIStatement.Execute(AService: TOCIService; ARows, AOffset: sb4;
    AExact, ACommit, AScrollable: Boolean);
var
    mode: ub4;
    res: sb4;
    pieceLoopData: TOCIPieceLoopData;
begin
    mode := OCI_DEFAULT;
    if AExact then
        mode := mode or OCI_EXACT_FETCH;
    if ACommit then
        mode := mode or OCI_COMMIT_ON_SUCCESS;
    if AScrollable then
        mode := mode or OCI_SCROLLABLE_CURSOR;
    if ARows <= 0 then begin
        ARows := 1;
        AOffset := 0;
    end;
    if AOffset < 0 then
        AOffset := 0;
    if DbgOutActive then
        if ((ARows > 1) or (AOffset > 0)) then
            DbgOut(tfStmt, 'Execute [ARows = ' + IntToStr(ARows) + '], [AOffset = ' + IntToStr(AOffset) + ']')
        else
            DbgOut(tfStmt, 'Execute');
    FService := AService;
    FService.UpdateNonBlockingMode;
    DumpInVars([odIn, odInOut]);
    FLastRowCount := 0;
    FCanceled := False;
    InitPiecedFetch(pieceLoopData);
    Error.ClearWarning;
    try
        try
            repeat
                if not FRef then
                    res := NCOci.OCIStmtExecute(FService.FHandle, FHandle, Error.FHandle,
                        ARows, AOffset, nil, nil, mode)
                else
                    res := NCOci.OCIStmtFetch(FHandle, Error.FHandle, ARows,
                        OCI_FETCH_NEXT, OCI_DEFAULT);
                case res of
                    OCI_STILL_EXECUTING:
                        FService.DoYield;
                    OCI_NEED_DATA, OCI_NO_DATA, OCI_SUCCESS:
                        begin
                            if (res = OCI_NO_DATA) and AExact then
                                Check('OCIStmtExecute', res)
                            else
                                Check('OCIStmtExecute', OCI_SUCCESS);
                            DoPiecedFetch(pieceLoopData, res);
                        end;
                    else
                        Check('OCIStmtExecute', res);
                end;
            until (res <> OCI_NEED_DATA) and (res <> OCI_STILL_EXECUTING);
        except
            on E: EOCINativeError do begin
                if FOCIVersion >= cvOracle81000 then
                    E.FParseErrorOffset := PARSE_ERROR_OFFSET;
                raise;
            end;
        end;
    except
        if PiecedFetch then begin
            FService.Break(False);
            FService.Reset(False);
        end;
        raise;
    end;
    FLastRowCount := ROW_COUNT;
    FEof := (FLastRowCount <> ub4(ARows)) and (STMT_TYPE = OCI_STMT_SELECT);
    DumpInVars([odOut, odInOut, odRet]);
    DumpOutVars(FLastRowCount);
    if FEof then begin
        DbgOut(tfStmt, 'EOF');
        FCanceled := True;
    end;
end;

procedure TOCIStatement.Fetch(ANRows: ub4; AOrient: ub2);
var
    res: sb4;
    prevRowCount: ub4;
    pieceLoopData: TOCIPieceLoopData;
begin
    DbgOut(tfStmt, 'Fetch');
    prevRowCount := ROW_COUNT;
    InitPiecedFetch(pieceLoopData);
    Error.ClearWarning;
    try
        repeat
            res := NCOci.OCIStmtFetch(FHandle, Error.FHandle, ANRows, AOrient, OCI_DEFAULT);
            case res of
                OCI_STILL_EXECUTING:
                    FService.DoYield;
                OCI_NO_DATA, OCI_NEED_DATA, OCI_SUCCESS:
                    begin
                        Check('OCIStmtFetch', OCI_SUCCESS);
                        DoPiecedFetch(pieceLoopData, res);
                    end;
                else
                    Check('OCIStmtFetch', res);
            end;
        until (res <> OCI_NEED_DATA) and (res <> OCI_STILL_EXECUTING);
    except
        if PiecedFetch then begin
            FService.Break(False);
            FService.Reset(False);
        end;
        raise;
    end;
    FLastRowCount := ROW_COUNT - prevRowCount;
    FEof := FLastRowCount <> ANRows;
    DumpOutVars(FLastRowCount);
    if FEof then begin
        DbgOut(tfStmt, 'EOF');
        FCanceled := True;
    end;
end;

procedure TOCIStatement.Next(ANRows: ub4);
begin
    Fetch(ANRows, OCI_FETCH_NEXT);
end;

procedure TOCIStatement.CancelCursor;
begin
    DbgOut(tfStmt, 'Cancel');
    Check('OCIStmtFetch', NCOci.OCIStmtFetch(FHandle, Error.FHandle, 0,
        OCI_FETCH_NEXT, OCI_DEFAULT));
    FCanceled := True;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

type
    PDateTimeRec = ^TDateTimeRec;
    TDateTimeRec = record
        DateTime: TDateTime;
    end;

procedure OraDate2Delphi(pIn: pUb1; var D: TDateTime);
begin
    D := 0.0;
    with POraDate(pIn)^ do
        if Century <> 0 then
            D := EncodeDate((Century - 100) * 100 + (Year - 100), Month, Day) +
                 EncodeTime(Hour - 1, Minute - 1, Second - 1, 0);
end;

procedure Delphi2OraDate(pOut: pUb1; D: TDateTime);
var
    tmp, Y, M, D2, H, M2, S: Word;
begin
    with POraDate(pOut)^ do begin
        DecodeDate(D, Y, M, D2);
        Century := Y div 100 + 100;
        Year := Y - (Century - 100) * 100 + 100;
        Month := M;
        Day := D2;
        DecodeTime(D, H, M2, S, tmp);
        Hour := H + 1;
        Minute := M2 + 1;
        Second := S + 1;
    end;
end;

function Spaces(ALen: Integer): String;
begin
    SetString(Result, nil, ALen);
    FillChar(Result[1], ALen, ' ');
end;

// -------------------------------------------------
// create & destroy & prop's

constructor TOCIVariable.Create(AStmt: TOCIStatement);
begin
    inherited Create;
    FOwner := AStmt;
    ArrayLen := 1;
    InrecDataSize := IDefInrecDataSize;
end;

destructor TOCIVariable.Destroy;
begin
    FreeBuffer;
    FHandle := nil;
    inherited Destroy;
end;

procedure TOCIVariable.SetArrayLen(const Value: ub4);
begin
    if FMaxArr_len <> Value then begin
        FreeBuffer;
        FMaxArr_len := Value;
        FCurEle := Value;
    end;
end;

procedure TOCIVariable.SetDataSize(const Value: ub4);
begin
    if FValue_sz <> Value then begin
        FreeBuffer;
        FValue_sz := Value;
        InitLongData;
    end;
end;

procedure TOCIVariable.SetDataType(const Value: TOCIVarDataType);
begin
    if FDataType <> Value then begin
        FreeBuffer;
        FDataType := Value;
        FValue_ty := nc2ociValueType[Value];
        FValue_sz := nc2ociValueSize[Value];
        if (FValue_sz = IDefStrSize) and FEnableLongString then
            FValue_sz := IDefLStrSize;
        InitLongData;
    end;
end;

procedure TOCIVariable.SetVarType(const Value: TOCIVarType);
begin
    if FVarType <> Value then begin
        FreeBuffer;
        FVarType := Value;
        InitLongData;
    end;
end;

function TOCIVariable.GetDumpLabel: String;
begin
    if FDumpLabel = '' then
        if Name = '' then
            Result := IntToStr(Position)
        else
            Result := Name
    else
        Result := FDumpLabel;
end;

// -------------------------------------------------
// Bind record's array access

function TOCIVariable.GetBindDataSize: ub4;
var
    L: ub4;
begin
    if LongData then
        L := SizeOf(TOraLong)
    else
        L := FValue_sz;
    Result := Sizeof(TBindData) + (Sizeof(sb2) + Sizeof(ub2) +
        Sizeof(ub2) + Sizeof(Boolean) + L) * FMaxArr_len;
end;

function TOCIVariable.GetBuffer(AIndex: ub4): pUb1;
var
    L: ub4;
begin
    if LongData then
        L := SizeOf(TOraLong)
    else
        L := FValue_sz;
    Result := pUb1(ub4(FBindData^.FBuffer) + L * ub4(AIndex));
end;

procedure TOCIVariable.ClearBuffer(AIndex: sb4; AHandleOwned: Boolean);
var
    i: Integer;
    dType: ub4;

    procedure DoClear(i: Integer);
    begin
        if LongData then
            FreeLong(POraLong(Buffer[i]))
        else if (dType <> 0) and GetIsOwned(i) and AHandleOwned and
                (ppOCIHandle(Buffer[i])^ <> nil) then begin
            CheckOwner;
            if dType = OCI_DTYPE_RSET then
                Check('OCIHandleFree(' + hndlNames[OCI_HTYPE_STMT] + ')',
                    NCOci.OCIHandleFree(ppOCIHandle(Buffer[i])^, OCI_HTYPE_STMT))
            else
                Check('OCIDescriptorFree(' + hndlNames[dType] + ')',
                    NCOci.OCIDescriptorFree(ppOCIHandle(Buffer[i])^, dType));
            ppOCIHandle(Buffer[i])^ := nil;
        end;
    end;

begin
    dType := 0;
    case DataType of
    otCursor, otNestedDataSet:
        dType := OCI_DTYPE_RSET;
    otCLOB, otBLOB:
        dType := OCI_DTYPE_LOB;
    otCFile, otBFile:
        dType := OCI_DTYPE_FILE;
    end;
    if AIndex = -1 then
        for i := 0 to FMaxArr_len - 1 do
            DoClear(i)
    else
        DoClear(AIndex);
    if FBuffStatus = bsExtInit then
        FBuffStatus := bsExtUnknown;
    if FOwner <> nil then
        TOCIStatement(FOwner).RemovePieceVar(Self);
end;

procedure TOCIVariable.FreeBuffer;
begin
    if (FBindData <> nil) and (FBuffStatus in [bsInt, bsExtInit]) then begin
        ClearBuffer(-1, True);
        if FBuffStatus = bsInt then
            FreeMem(FBindData);
        FBindData := nil;
        FBuffStatus := bsInt;
        FLastBindedBuffer := nil;
    end;
end;

procedure TOCIVariable.InitBuffer(AIndex: sb4; AHandleOwned: Boolean);
var
    i: Integer;
    dType: ub4;

    procedure DoInit(i: ub4);
    begin
        CheckBufferAccess(i);
        with FBindData^ do begin
            FInd[i] := -1;
            FALen[i] := FValue_sz;
            FRCode[i] := OCI_SUCCESS;
            FOwned[i] := AHandleOwned;
        end;
        if LongData then
            InitLong(POraLong(Buffer[i]))
        else if (dType <> 0) and AHandleOwned then begin
            ppOCIHandle(Buffer[i])^ := nil;
            CheckOwner;
            if dType = OCI_DTYPE_RSET then begin
                Check('OCIHandleAlloc(' + hndlNames[OCI_HTYPE_STMT] + ')',
                    NCOci.OCIHandleAlloc(FOwner.FOwner.FHandle, ppOCIHandle(Buffer[i])^,
                    OCI_HTYPE_STMT, 0, nil));
                FBindData^.FInd[i] := 0;
            end
            else
                Check('OCIDescriptorAlloc(' + hndlNames[dType] + ')',
                    NCOci.OCIDescriptorAlloc(FOwner.FOwner.FHandle,
                    ppOCIHandle(Buffer[i])^, dType, 0, nil));
        end;
    end;

begin
    if FValue_ty <= 0 then
        OCIDBErrorFmt(msgOCIBadValueType, [FPlaceHolder, FValue_ty], nil);
    if not (DataType in [otString, otLong, otLongRaw, otNumber]) and (FValue_sz <= 0) then
        OCIDBError(msgOCIBadValueSize, nil);
    if FMaxArr_len <= 0 then
        OCIDBError(msgOCIBadArrayLen, nil);
    case DataType of
    otCursor, otNestedDataSet:
        dType := OCI_DTYPE_RSET;
    otCLOB, otBLOB:
        dType := OCI_DTYPE_LOB;
    otCFile, otBFile:
        dType := OCI_DTYPE_FILE;
    else
        dType := 0;
    end;
    if FBuffStatus = bsExtUnknown then
        FBuffStatus := bsExtInit;
    if AIndex = -1 then
        for i := 0 to FMaxArr_len - 1 do
            DoInit(i)
    else
        DoInit(AIndex);
end;

procedure TOCIVariable.ResetBuffer(AIndex: sb4);
begin
    ClearBuffer(AIndex, True);
    InitBuffer(AIndex, True);
end;

procedure TOCIVariable.CheckOwner;
begin
    if (FOwner = nil) then
        OCIDBError(msgOCIVarWithoutStmt, nil);
end;

procedure TOCIVariable.CheckBufferAccess(AIndex: ub4);
begin
    if AIndex >= FMaxArr_len then
        OCIDBError(msgOCIOutOfCount, nil);
    if (FBuffStatus <> bsExtInit) and (
           (FBindData = nil) and (FBuffStatus = bsInt) or
           (FBindData <> nil) and (FBuffStatus = bsExtUnknown)
       ) then begin
        if FBuffStatus = bsInt then begin
            GetMem(FBindData, BindDataSize);
            MapBuffer;
        end;
        InitBuffer(-1, True);
    end;
end;

procedure TOCIVariable.MapBuffer;
var
    p: pUb1;
begin
    p := pUb1(FBindData);
    with FBindData^ do begin
        Inc(p, Sizeof(TBindData));
        FInd := PSb2Array(p);
        Inc(p, Sizeof(sb2) * FMaxArr_len);
        FALen := PUb2Array(p);
        Inc(p, Sizeof(ub2) * FMaxArr_len);
        FRCode := PUb2Array(p);
        Inc(p, Sizeof(ub2) * FMaxArr_len);
        FOwned := PBoolArray(p);
        Inc(p, Sizeof(Boolean) * FMaxArr_len);
        FBuffer := pUb1(p);
    end;
end;

procedure TOCIVariable.SetExternBuffer(AValue: pUb1);
begin
    FBindData := PBindData(AValue);
    if FBindData <> nil then
        MapBuffer;
    FBuffStatus := bsExtInit;
end;

function TOCIVariable.GetExternBuffer: pUb1;
begin
    Result := pUb1(FBindData);
end;

function TOCIVariable.GetIsOwned(AIndex: ub4): Boolean;
begin
    CheckBufferAccess(AIndex);
    Result := FBindData^.FOwned[AIndex];
end;

// -------------------------------------------------
// actual bind

{$IFDEF OCI_USE_BINDDYNAMIC}
var
    null_ind: sb2;

function InCBFunc(ictxp: Pointer; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: Pointer; var alenp: Ub4; var piecep: Ub1; var indp: pSb2): sword; cdecl;
begin
    bufpp := nil;
    alenp := 0;
    indp := nil;
    null_ind := -1;
    indp := @null_ind;
    piecep := OCI_ONE_PIECE;
    Result := OCI_CONTINUE;
end;

function OutCBFunc(ictxp: Pointer; bindp: pOCIBind; iter: ub4; index: ub4;
    var bufpp: Pointer; var alenpp: pUb4; var piecep: ub1; var indpp: pSb2;
    var rcodepp: pUb2): sword; cdecl;
var
    V: TOCIVariable;
begin
    V := TOCIVariable(ictxp);
    if index = 0 then begin
        // check number of rows returned is valid
        // support ONLY for 1 row per 1 iteration
        if V.ROWS_RETURNED <> 1 then begin
            Result := OCI_ERROR;
            Exit;
        end;
    end;
    V.CheckBufferAccess(iter);
    with V.FBindData^ do begin
        bufpp := V.Buffer[iter];
        alenpp := @(FALen[iter]);
        indpp := @(FInd[iter]);
        rcodepp := @(FRCode[iter]);
    end;
    piecep := OCI_ONE_PIECE;
    Result := OCI_CONTINUE;
end;
{$ENDIF}

procedure TOCIVariable.Bind;
var
    v1: ub4;
    v2: pUb4;
    sz: sb4;
    tp: ub2;
    plcHldr: String;
begin
    CheckOwner;
    CheckBufferAccess(0);
    with FBindData^ do begin
        tp := FValue_ty;
        sz := FValue_sz;
        if DataType in otVBlobs then begin
            // this is special case - if value size <> 0 then
            // there was mapping VARCHAR2/RAW into otLong/otLongRaw
            if FValue_sz <> 0 then begin
                if DataType = otLong then
                    tp := nc2ociValueType[otString]
                else if DataType = otLongRaw then
                    tp := nc2ociValueType[otRaw];
            end
            else
                sz := $7FFFFFFF;
        end;
        if VarType = odDefine then begin
            FType := OCI_HTYPE_DEFINE;
            if LongData then begin
                Check('OCIDefineByPos(' + IntToStr(FPosition) + ')',
                    NCOci.OCIDefineByPos(FOwner.FHandle, FHandle, Error.FHandle,
                    FPosition, nil, sz, tp, nil, nil, nil, OCI_DYNAMIC_FETCH));
                TOCIStatement(FOwner).AddPieceVar(Self);
            end
            else begin
                Check('OCIDefineByPos(' + IntToStr(FPosition) + ')',
                    NCOci.OCIDefineByPos(FOwner.FHandle, FHandle, Error.FHandle,
                    FPosition, FBuffer, sz, tp, FInd, FALen, FRCode, OCI_DEFAULT));
            end;
        end
{$IFDEF OCI_USE_BINDDYNAMIC}
        // in 8.0.4 dynamic bind with BLOB's give access violation
        // when i try to use BLOB handle after query was executed
        else if VarType = odRet then begin
            if (FHandle = nil) or (FBuffer <> FLastBindedBuffer) then begin
                FLastBindedBuffer := FBuffer;
                FType := OCI_HTYPE_BIND;
                Check('OCIBindByName(' + FPlaceHolder + ')', NCOci.OCIBindByName(FOwner.FHandle,
                    FHandle, Error.FHandle, PChar(FPlaceHolder), Length(FPlaceHolder), nil,
                    sz, tp, nil, nil, nil, 0, nil, OCI_DATA_AT_EXEC));
                Check('OCIBindDynamic', NCOci.OCIBindDynamic(FHandle, Error.FHandle,
                    self, @InCBFunc, self, @OutCBFunc));
            end;
        end
{$ENDIF}
        else if VarType in [odRet, odUnknown, odIn, odOut, odInOut] then begin
            if (FHandle = nil) or (FBuffer <> FLastBindedBuffer) then begin
                FLastBindedBuffer := FBuffer;
                FType := OCI_HTYPE_BIND;
                if FIsPLSQLTable and
                   (TOCIStatement(FOwner).Stmt_Type in [OCI_STMT_BEGIN, OCI_STMT_DECLARE]) then begin
                    v1 := FMaxArr_len;
                    v2 := @FCurEle;
                end
                else begin
                    v1 := 0;
                    v2 := nil;
                end;
                if FPosition <> 0 then
                    if LongData then begin
                        Check('OCIBindByPos(' + IntToStr(FPosition) + ')',
                            NCOci.OCIBindByPos(FOwner.FHandle, FHandle, Error.FHandle,
                            FPosition, nil, sz, tp, nil, nil, nil, v1, v2, OCI_DATA_AT_EXEC));
                        TOCIStatement(FOwner).AddPieceVar(Self);
                    end
                    else
                        Check('OCIBindByPos(' + IntToStr(FPosition) + ')',
                            NCOci.OCIBindByPos(FOwner.FHandle, FHandle, Error.FHandle,
                            FPosition, FBuffer, sz, tp, FInd, FALen, FRCode, v1, v2, OCI_DEFAULT))
                else begin
                    if FIsCaseSensitive and (FOCIVersion >= cvOracle81600) then
                        plcHldr := ':"' + Copy(FPlaceHolder, 2, Length(FPlaceHolder)) + '"'
                    else
                        plcHldr := FPlaceHolder;
                    if LongData then begin
                        Check('OCIBindByName(' + plcHldr + ')',
                            NCOci.OCIBindByName(FOwner.FHandle, FHandle, Error.FHandle,
                            PChar(plcHldr), Length(plcHldr),
                            nil, sz, tp, nil, nil, nil, v1, v2, OCI_DATA_AT_EXEC));
                        TOCIStatement(FOwner).AddPieceVar(Self);
                    end
                    else
                        Check('OCIBindByName(' + plcHldr + ')',
                            NCOci.OCIBindByName(FOwner.FHandle, FHandle, Error.FHandle,
                            PChar(plcHldr), Length(plcHldr),
                            FBuffer, sz, tp, FInd, FALen, FRCode, v1, v2, OCI_DEFAULT));
                end;
            end;
        end
        else
            OCIDBError(msgOCIBadVarType, nil);
    end;
    TOCIStatement(FOwner).AddVar(Self);
end;

procedure TOCIVariable.BindTo(AStmt: TOCIStatement);
begin
    FOwner := AStmt;
    Bind;
end;

procedure TOCIVariable.BindOff;
begin
    if FOwner <> nil then begin
        ClearBuffer(-1, True);
        TOCIStatement(FOwner).RemovePieceVar(Self);
        TOCIStatement(FOwner).RemoveVar(Self);
        FOwner := nil;
        FHandle := nil;
        FError := nil;
    end;
end;

// -------------------------------------------------
// long & long raw value

procedure TOCIVariable.SetInrecDataSize(const Value: ub4);
begin
    if FInrecDataSize <> Value then begin
        FreeBuffer;
        FInrecDataSize := Value;
        InitLongData;
    end;
end;

procedure TOCIVariable.InitLongData;
begin
    FLongData := 
        ((FDataType in [otString, otChar, otRaw]) and (FValue_sz > FInrecDataSize) or
         (FDataType in otVBlobs) and ((FValue_sz = 0) or (FValue_sz > FInrecDataSize)));
end;

procedure TOCIVariable.FreeLong(pLong: POraLong);
begin
    if pLong^.FData <> nil then
        try
            FreeMem(pLong^.FData, pLong^.FSize);
        finally
            InitLong(pLong);
        end;
end;

procedure TOCIVariable.InitLong(pLong: POraLong);
begin
    pLong^.FData := nil;
    pLong^.FSize := 0;
end;

procedure TOCIVariable.GetLong(pLong: POraLong; var AData: pUb1; var ASize: ub4);
begin
    ASize := pLong^.FSize;
    AData := pLong^.FData;
end;

procedure TOCIVariable.SetLong(pLong: POraLong; AData: pUb1; ASize: ub4; AOwnBuffer: Boolean);
begin
    FreeLong(pLong);
    if AOwnBuffer then
        pLong^.FData := AData
    else begin
        GetMem(pLong^.FData, ASize);
        Move(AData^, pLong^.FData^, ASize);
    end;
    pLong^.FSize := ASize;
end;

procedure TOCIVariable.AppendLong(pLong: POraLong; AData: pUb1; ASize: ub4; AOwnBuffer: Boolean);
begin
    if pLong^.FData = nil then
        SetLong(pLong, AData, ASize, AOwnBuffer)
    else begin
        ReallocMem(pLong^.FData, pLong^.FSize + ASize);
        Move(AData^, pUb1(ub4(pLong^.FData) + pLong^.FSize)^, ASize);
        Inc(pLong^.FSize, ASize);
        if AOwnBuffer then
            FreeMem(AData);
    end;
end;

// -------------------------------------------------
// get/set value

function TOCIVariable.UpdateHBlobNullInd(AIndex: ub4): Boolean;
var
    init: LongBool;
begin
    CheckBufferAccess(AIndex);
    with FBindData^ do begin
        if DataType in otHBlobs then begin
            if ppOCIHandle(Buffer[AIndex])^ = nil then
                FInd[AIndex] := -1
            else begin
                Check('OCILobLocatorIsInit',
                    NCOci.OCILobLocatorIsInit(FOwner.FOwner.FHandle,
                    Error.FHandle, ppOCIHandle(Buffer[AIndex])^, init));
                if not init then
                    FInd[AIndex] := -1
                else
                    FInd[AIndex] := 0;
            end;
        end;
        Result := (FInd[AIndex] = 0);
    end;
end;

function TOCIVariable.GetDataPtr(AIndex: ub4; var ABuff: pUb1; var ASize: ub4): Boolean;
begin
    CheckBufferAccess(AIndex);
    with FBindData^ do
        if (FInd[AIndex] = -1) and not (DataType in otHTypes) then begin
            Result := False;
            ABuff := nil;
        end
        else begin
            Result := True;
            ABuff := Buffer[AIndex];
            case DataType of
                otSmallInt, otInteger:
                    ASize := SizeOf(Integer);
                otFloat:
                    ASize := SizeOf(Double);
                otBCD:
                    ASize := SizeOf(Double);
                otString, otChar, otRaw, otLong, otLongRaw, otNumber:
                    if LongData then
                        GetLong(POraLong(ABuff), ABuff, ASize)
                    else if DataType = otChar then
                        ASize := FValue_sz
                    else
                        ASize := FALen[AIndex];
                otROWID:
                    ASize := FALen[AIndex];
                otDateTime:
                    ASize := 7;
            else
                if DataType in otHTypes then begin
                    ASize := SizeOf(pOCIHandle);
                    Result := UpdateHBlobNullInd(AIndex);
                end
                else
                    OCIDBError(msgOCIUnsupValueType, nil);
            end;
        end;
end;

function TOCIVariable.GetData(AIndex: ub4; ABuff: pUb1; var ASize: ub4; AFormat: TOCIDataFormat): Boolean;
var
    p: pUb1;
    D: TDateTime;
    sz: Integer;
    S: String;
    pBuff: pUb1;
begin
    Result := GetDataPtr(AIndex, p, ASize);
    if (ABuff <> nil) and
       (Result or (DataType in otHTypes)) then
        case DataType of
            otSmallInt:
                PSmallInt(ABuff)^ := PInteger(p)^;
            otInteger:
                PInteger(ABuff)^ := PInteger(p)^;
            otFloat:
                PDouble(ABuff)^ := PDouble(p)^;
            otBCD:
                case AFormat of
                dfOCI:
                    PDouble(ABuff)^ := PDouble(p)^;
                dfDataSet, dfDelphi:
                    PCurrency(ABuff)^ := PDouble(p)^;
                end;
            otString, otChar, otRaw, otLong, otLongRaw, otROWID, otNumber:
                begin
                    sz := ASize;
                    pBuff := ABuff;
                    if (DataType = otRaw) and (AFormat = dfDataSet) then begin
                        PWord(ABuff)^ := sz;
                        pBuff := pUb1(PChar(pBuff) + 2);
                    end;
                    if ((DataType in [otString, otLong]) and FStrsTrim) or
                       (DataType = otChar) or (DataType = otNumber) then
                        while (sz > 0) and (PChar(p)[sz - 1] in [' ', #13, #10, #7]) do
                            Dec(sz);
                    if DataType <> otChar then
                        ASize := sz;
                    if (sz = 0) and FStrsEmpty2Null then
                        Result := False
                    else begin
                        Move(p^, PChar(pBuff)^, ASize);
                        if (DataType = otChar) and (ASize < FValue_sz) then begin
                            S := Spaces(FValue_sz - ASize);
                            Move(S[1], (PChar(pBuff) + ASize)^, FValue_sz - ASize);
                            ASize := FValue_sz;
                        end;
                    end;
                end;
            otDateTime:
                case AFormat of
                dfOCI:
                    POraDate(ABuff)^ := POraDate(p)^;
                dfDataSet:
                    begin
                        OraDate2Delphi(p, D);
                        PDateTimeRec(ABuff)^.DateTime := TimeStampToMSecs(DateTimeToTimeStamp(D));
                    end;
                dfDelphi:
                    OraDate2Delphi(p, PDateTime(ABuff)^);
                end;
        else
            if DataType in otHTypes then
                ppOCIHandle(ABuff)^ := ppOCIHandle(p)^
            else
                OCIDBError(msgOCIUnsupValueType, nil);
        end;
end;

function TOCIVariable.DumpValue(AIndex: ub4): String;
var
    p: pUb1;
    buff: array[0..63] of ub1;
    sz: ub4;

{$IFNDEF OCI_D4}
    procedure BinToHex(Buffer, Text: PChar; BufSize: Integer); assembler;
    asm
            PUSH    ESI
            PUSH    EDI
            MOV     ESI,EAX
            MOV     EDI,EDX
            MOV     EDX,0
            JMP     @@1
    @@0:    DB      '0123456789ABCDEF'
    @@1:    LODSB
            MOV     DL,AL
            AND     DL,0FH
            MOV     AH,@@0.Byte[EDX]
            MOV     DL,AL
            SHR     DL,4
            MOV     AL,@@0.Byte[EDX]
            STOSW
            DEC     ECX
            JNE     @@1
            POP     EDI
            POP     ESI
    end;
{$ENDIF}

begin
    if not GetDataPtr(AIndex, p, sz) then
        Result := 'NULL'
    else begin
        case DataType of
            otString, otChar, otLong, otROWID:
                if sz > 1024 then begin
                    SetString(Result, PChar(p), 1024);
                    Result := '(truncated at 1024) ''' + Result + ' ...''';
                end
                else begin
                    SetString(Result, PChar(p), sz);
                    Result := '''' + Result + '''';
                end;
            otNumber:
                SetString(Result, PChar(p), sz);
            otRaw, otLongRaw:
                if sz > 512 then begin
                    SetLength(Result, 1024);
                    BinToHex(PChar(p), PChar(Result), 512);
                    Result := '(truncated at 512) ' + Result + ' ...';
                end
                else begin
                    SetLength(Result, sz * 2);
                    BinToHex(PChar(p), PChar(Result), sz);
                end;
            else begin
                GetData(AIndex, @buff, sz, dfDelphi);
                case DataType of
                    otSmallInt:
                        Result := IntToStr(PSmallInt(@buff)^);
                    otInteger:
                        Result := IntToStr(PInteger(@buff)^);
                    otFloat:
                        Result := FloatToStr(PDouble(@buff)^);
                    otBCD:
                        Result := CurrToStr(PCurrency(@buff)^);
                    otDateTime:
                        Result := DateTimeToStr(PDateTime(@buff)^);
                else
                    if DataType in otHTypes then
                        Result := '<HANDLE>' + IntToHex(PInteger(@buff)^, 8)
                    else
                        OCIDBError(msgOCIUnsupValueType, nil);
                end;
            end;
        end;
    end;
end;

procedure TOCIVariable.SetIsNull(AIndex: ub4; AIsNull: Boolean);
begin
    CheckBufferAccess(AIndex);
    with FBindData^ do
        if AIsNull then
            FInd[AIndex] := -1
        else
            FInd[AIndex] := 0;
end;

procedure TOCIVariable.AppendData(AIndex: ub4; ABuff: pUb1; ASize: ub4);
begin
    if not LongData then
        OCIDBError(msgOCIUnsupValueType, nil);
    CheckBufferAccess(AIndex);
    AppendLong(POraLong(Buffer[AIndex]), ABuff, ASize, False);
    with FBindData^ do begin
        FInd[AIndex] := 0;
        FALen[AIndex] := SizeOf(TOraLong);
    end;
end;

procedure TOCIVariable.SetData(AIndex: ub4; ABuff: pUb1; ASize: ub4; AFormat: TOCIDataFormat);
var
    D: TDateTime;
    S: String;
    sz: sb4;
    pBuff: pUb1;
begin
    CheckBufferAccess(AIndex);
    with FBindData^ do begin
        if LongData then
            FreeLong(POraLong(Buffer[AIndex]));
        if ABuff = nil then
            FInd[AIndex] := -1
        else begin
            FInd[AIndex] := 0;
            case DataType of
                otSmallInt:
                    begin
                        PInteger(Buffer[AIndex])^ := PSmallInt(ABuff)^;
                        FALen[AIndex] := SizeOf(Integer);
                    end;
                otInteger:
                    begin
                        PInteger(Buffer[AIndex])^ := PInteger(ABuff)^;
                        FALen[AIndex] := SizeOf(Integer);
                    end;
                otFloat:
                    begin
                        PDouble(Buffer[AIndex])^ := PDouble(ABuff)^;
                        FALen[AIndex] := Sizeof(Double);
                    end;
                otBCD:
                    begin
                        case AFormat of
                        dfOCI:
                            PDouble(Buffer[AIndex])^ := PDouble(ABuff)^;
                        dfDataSet, dfDelphi:
                            PDouble(Buffer[AIndex])^ := PCurrency(ABuff)^;
                        end;
                        FALen[AIndex] := Sizeof(Double);
                    end;
                otString, otChar, otRaw, otLong, otLongRaw, otROWID, otNumber:
                    begin
                        pBuff := ABuff;
                        if ASize = $FFFFFFFF then
                            if (DataType = otRaw) and (AFormat = dfDataSet) then begin
                                ASize := PWord(ABuff)^;
                                pBuff := pUb1(PChar(pBuff) + 2);
                            end
                            else
                                ASize := StrLen(PChar(pBuff));
                        sz := ASize;
                        if ((DataType in [otString, otLong]) and FStrsTrim) or
                           (DataType = otChar) or (DataType = otNumber) then
                            while (sz > 0) and (PChar(pBuff)[sz - 1] in [' ', #13, #10, #7]) do
                                Dec(sz);
                        if DataType <> otChar then
                            ASize := sz;
                        if FStrsEmpty2Null and (sz = 0) then
                            FInd[AIndex] := -1
                        else begin
                            if (ASize > FValue_sz) and (
                                (DataType in [otString, otChar, otRaw, otROWID, otNumber]) or
                                (DataType in otVBlobs) and (FValue_sz <> 0)
                               ) then
                                OCIDBError(msgOCIDataToLarge, nil);
                            if LongData then begin
                                SetLong(POraLong(Buffer[AIndex]), pBuff, ASize, AFormat = dfDelphiOwned);
                                FALen[AIndex] := SizeOf(TOraLong);
                            end
                            else begin
                                Move(PChar(pBuff)^, Buffer[AIndex]^, ASize);
                                if (DataType = otChar) and (ASize < FValue_sz) then begin
                                    S := Spaces(FValue_sz - ASize);
                                    Move(S[1], (PChar(Buffer[AIndex]) + ASize)^, FValue_sz - ASize);
                                    FALen[AIndex] := FValue_sz;
                                end
                                else
                                    FALen[AIndex] := ASize;
                            end;
                        end;
                    end;
                otDateTime:
                    begin
                        case AFormat of
                        dfOCI:
                            POraDate(Buffer[AIndex])^ := POraDate(ABuff)^;
                        dfDataSet:
                            begin
                                D := TimeStampToDateTime(MSecsToTimeStamp(PDateTimeRec(ABuff)^.DateTime));
                                Delphi2OraDate(Buffer[AIndex], D);
                            end;
                        dfDelphi:
                            Delphi2OraDate(Buffer[AIndex], PDateTime(ABuff)^);
                        end;
                        FALen[AIndex] := 7;
                    end;
            else
                if DataType in otHTypes then begin
                    if ppOCIHandle(ABuff)^ <> nil then begin
                        ppOCIHandle(Buffer[AIndex])^ := ppOCIHandle(ABuff)^;
                        FALen[AIndex] := SizeOf(pOCIHandle);
                        UpdateHBlobNullInd(AIndex);
                    end;
                end
                else
                    OCIDBError(msgOCIUnsupValueType, nil);
            end;
        end;
    end;
end;

procedure TOCIVariable.SetDataByRef(AIndex: ub4; ABuff: pUb1; ASize: ub4;
    AFormat: TOCIDataFormat);
begin
    ClearBuffer(AIndex, True);
    SetData(AIndex, ABuff, ASize, AFormat);
    FBindData^.FOwned[AIndex] := False;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

function TOCIDescriptor.Init(AEnv: TOCIHandle; AType: ub4): pOCIDescriptor;
begin
    Check('OCIDescriptorAlloc(' + hndlNames[AType] + ')',
        NCOci.OCIDescriptorAlloc(AEnv.FHandle, FHandle, AType, 0, nil));
    FType := AType;
    Result := @FHandle;
end;

destructor TOCIDescriptor.Destroy;
begin
    if FHandle <> nil then
        Check('OCIDescriptorFree(' + hndlNames[FType] + ')',
            NCOci.OCIDescriptorFree(FHandle, FType));
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCISelectItem.Create(AStmt: TOCIStatement);
begin
    inherited Create;
    FOwner := AStmt;
    FType := OCI_DTYPE_PARAM;
    FEnableInteger := False;
    FEnableBCD := False;
    FEnableNumber := False;
    FEnableLongString := False;
    FEnableFixedString := False;
end;

destructor TOCISelectItem.Destroy;
begin
    FHandle := nil;
    inherited Destroy;
end;

procedure TOCISelectItem.Bind;
begin
    Check('OCIParamGet(' + IntToStr(FPosition) + ')',
        NCOci.OCIParamGet(FOwner.FHandle, OCI_HTYPE_STMT,
        Error.FHandle, FHandle, FPosition));
end;

function TOCISelectItem.GetDataType: TOCIVarDataType;
var
    ds, sc, prec: ub4;
begin
    case DATA_TYPE of
    SQLT_CHR, SQLT_VCS:
        begin
            ds := DATA_SIZE;
            if (FEnableLongString or (ds <= IMaxDlp2StrSize)) and
               (ds <= IMaxDlp4StrSize) then
                Result := otString
            else
                Result := otLong;
        end;
    SQLT_NUM:
        begin
            sc := SCALE;
            prec := PRECISION;
            if FEnableInteger and (sc = 0) and (prec <> 0) and (prec <= 5) then
                Result := otSmallInt
            else if FEnableInteger and (sc = 0) and (prec <> 0) and (prec <= 10) then
                Result := otInteger
            else if FEnableBCD and (sc <= 4) and (prec <> 0) and (prec <= 19) then
                Result := otBCD
            else if FEnableNumber and ((prec = 0) or (prec >= 15)) then
                Result := otNumber
            else
                Result := otFloat;
        end;
    SQLT_INT, _SQLT_PLI: Result := otInteger;
    SQLT_LNG: Result := otLong;
    SQLT_RID, SQLT_RDD: Result := otROWID;
    SQLT_DAT: Result := otDateTime;
    SQLT_BIN: Result := otRaw;
    SQLT_LBI: Result := otLongRaw;
    SQLT_AFC:
        if FEnableFixedString then
            Result := otChar
        else if FEnableLongString or (DATA_SIZE <= IMaxDlp2StrSize) then
            Result := otString
        else
            Result := otLong;
    SQLT_CLOB: Result := otCLOB;
    SQLT_BLOB: Result := otBLOB;
    SQLT_BFILEE: Result := otBFile;
    SQLT_CFILEE: Result := otCFile;
    SQLT_CUR: Result := otCursor;           // used in PL/SQL. maps to REF CURSOR
    SQLT_RSET: Result := otNestedDataSet;   // used in objects. maps to select ... cursor(select ...) ...
    else Result := otUnknown;
    end;
end;

function TOCISelectItem.GetDataSize: ub4;
var
    dt: TOCIVarDataType;
begin
    dt := DataType;
    if dt in [otString, otChar, otRaw, otLong, otLongRaw] then
        Result := DATA_SIZE
    else if dt = otNumber then
        try
          Result := DISP_SIZE
        except
          on E: EOCINativeError do
            begin
              if E.Errors[0].ErrorCode = 24328 then
                Result := nc2ociValueSize[otNumber]
              else
                raise;
            end;
        end
    else
        Result := nc2ociValueSize[dt];
end;

function TOCISelectItem.GetVarType: TOCIVarType;
begin
    Result := odUnknown;
    case IOMODE of
    OCI_TYPEPARAM_OUT: Result := odOut;
    OCI_TYPEPARAM_IN: Result := odIn;
    OCI_TYPEPARAM_INOUT: Result := odInOut;
    end;
end;

function TOCISelectItem.GetIsNull: Boolean;
begin
    Result := not FEnableRequired or (IS_NULL <> 0);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCILobLocator.Create(AService: TOCIService; AType: ub4);
begin
    inherited Create;
    FOwner := AService;
    Init(AService.FOwner, AType);
    FOwned := True;
end;

constructor TOCILobLocator.CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    FOwner := AService;
    FHandle := AHandle;
    FOwned := False;
end;

destructor TOCILobLocator.Destroy;
begin
    if not FOwned then
        FHandle := nil;
    if (FHandle <> nil) and IsTemporary then
      Check('OCILOBFreeTemporary', OCILOBFreeTemporary(FOwner.FHandle,
          Error.FHandle, FHandle));
    inherited Destroy;
end;

procedure TOCILobLocator.Assign(AFrom: TOCILobLocator);
begin
    Check('OCILobAssign', NCOci.OCILobAssign(FOwner.FOwner.FHandle, Error.FHandle,
        AFrom.FHandle, FHandle));
end;

procedure TOCILobLocator.Close;
begin
    if FOCIVersion >= cvOracle81000 then
        Check('OCILobClose', NCOci.OCILobClose(FOwner.FHandle, Error.FHandle, FHandle))
    else if FType = OCI_DTYPE_FILE then
        Check('OCILobFileClose', NCOci.OCILobClose(FOwner.FHandle, Error.FHandle, FHandle));
end;

function TOCILobLocator.GetLength: sb4;
begin
  Result := 0;
  if GetIsInit() then
    Check('OCILobGetLength', NCOci.OCILobGetLength(FOwner.FHandle, Error.FHandle,
        FHandle, ub4(Result)));
end;

function TOCILobLocator.GetIsOpen: LongBool;
begin
    if FOCIVersion >= cvOracle81000 then
        Check('OCILobIsOpen', NCOci.OCILobIsOpen(FOwner.FHandle, Error.FHandle,
            FHandle, Result))
    else if FType = OCI_DTYPE_FILE then
        Check('OCILobFileIsOpen', NCOci.OCILobIsOpen(FOwner.FHandle, Error.FHandle,
            FHandle, Result))
    else
        Result := True;
end;

function TOCILobLocator.GetIsInit: LongBool;
begin
    Check('OCILobLocatorIsInit', NCOci.OCILobLocatorIsInit(FOwner.FOwner.FHandle,
        Error.FHandle, FHandle, Result));
end;

procedure TOCILobLocator.Read(ABuff: pUb1; ABuffLen: ub4; var amount: ub4; offset: ub4);
begin
    Check('OCILobRead', NCOci.OCILobRead(FOwner.FHandle, Error.FHandle,
        FHandle, amount, offset, ABuff, ABuffLen, nil, nil, 0, SQLCS_IMPLICIT));
end;

procedure TOCILobLocator.Open(AReadOnly: Boolean);
const
    bool2mode: array[Boolean] of ub1 = (OCI_LOB_READWRITE, OCI_LOB_READONLY);
begin
    if FOCIVersion >= cvOracle81000 then
        Check('OCILobOpen', NCOci.OCILobOpen(FOwner.FHandle, Error.FHandle,
            FHandle, bool2mode[AReadOnly]))
    else if FType = OCI_DTYPE_FILE then
        Check('OCILobFileOpen', NCOci.OCILobOpen(FOwner.FHandle, Error.FHandle,
            FHandle, OCI_LOB_READONLY));
end;

constructor TOCIIntLocator.CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    inherited CreateUseHandle(AService, AHandle);
    FType := OCI_DTYPE_LOB;
end;

procedure TOCIIntLocator.Append(AFrom: TOCIIntLocator);
begin
    Check('OCILobAppend', NCOci.OCILobAppend(FOwner.FHandle, Error.FHandle,
        FHandle, AFrom.FHandle));
end;

procedure TOCIIntLocator.Copy(AFrom: TOCIIntLocator; amount, dst_offset, src_offset: ub4);
begin
    Check('OCILobCopy', NCOci.OCILobCopy(FOwner.FHandle, Error.FHandle, FHandle,
        AFrom.FHandle, amount, dst_offset, src_offset));
end;

procedure TOCIIntLocator.SetBuffering(const Value: Boolean);
begin
    if FBuffering <> Value then begin
        FBuffering := Value;
        if FBuffering then
            Check('OCILobEnableBuffering', NCOci.OCILobEnableBuffering(FOwner.FHandle,
                Error.FHandle, FHandle))
        else
            Check('OCILobDisableBuffering', NCOci.OCILobDisableBuffering(FOwner.FHandle,
                Error.FHandle, FHandle));
    end;
end;

procedure TOCIIntLocator.Erase(var amount: ub4; offset: ub4);
begin
    Check('OCILobErase', NCOci.OCILobErase(FOwner.FHandle, Error.FHandle,
        FHandle, amount, offset));
end;

procedure TOCIIntLocator.FlushBuffer;
begin
    Check('OCILobFlushBuffer', NCOci.OCILobFlushBuffer(FOwner.FHandle, Error.FHandle,
        FHandle, OCI_LOB_BUFFER_NOFREE));
end;

procedure TOCIIntLocator.LoadFromFile(AFrom: TOCIExtLocator; amount, dst_offset, src_offset: ub4);
begin
    Check('OCILobLoadFromFile', NCOci.OCILobLoadFromFile(FOwner.FHandle, Error.FHandle,
        FHandle, AFrom.FHandle, amount, dst_offset, src_offset));
end;

procedure TOCIIntLocator.Trim(ANewLen: ub4);
begin
    Check('OCILobTrim', NCOci.OCILobTrim(FOwner.FHandle, Error.FHandle,
        FHandle, ANewLen));
end;

procedure TOCIIntLocator.Write(ABuff: pUb1; ABuffLen: ub4; var amount: ub4; offset: ub4);
begin
    Check('OCILobWrite', NCOci.OCILobWrite(FOwner.FHandle, Error.FHandle,
        FHandle, amount, offset, ABuff, ABuffLen, OCI_ONE_PIECE, nil,
        nil, 0, SQLCS_IMPLICIT));
end;

constructor TOCIExtLocator.CreateUseHandle(AService: TOCIService; AHandle: pOCIHandle);
begin
    inherited CreateUseHandle(AService, AHandle);
    FType := OCI_DTYPE_FILE;
end;

function TOCIExtLocator.GetFileExists: Boolean;
begin
    Check('OCILobFileExists', NCOci.OCILobFileExists(FOwner.FHandle, Error.FHandle,
        FHandle, Result));
end;

procedure TOCIExtLocator.GetFileDir(var ADir, AFileName: String);
var
    dirBuff: array[0..29] of char;
    fileBuff: array[0..255] of char;
    dirLen, fileLen: ub2;
begin
    dirLen := 30;
    fileLen := 255;
    Check('OCILobFileGetName', NCOci.OCILobFileGetName(FOwner.FOwner.FHandle,
        Error.FHandle, FHandle, @dirBuff, dirLen, @fileBuff, fileLen));
    SetString(ADir, dirBuff, dirLen);
    SetString(AFileName, fileBuff, fileLen);
end;

procedure TOCIExtLocator.SetFileDir(const ADir, AFileName: String);
begin
    Check('OCILobFileSetName', NCOci.OCILobFileSetName(FOwner.FOwner.FHandle,
        Error.FHandle, FHandle, PChar(ADir), System.Length(ADir), PChar(AFileName),
        System.Length(AFileName)));
end;

function TOCIExtLocator.GetDirectory: String;
var
    AFileName: String;
begin
    GetFileDir(Result, AFileName);
end;

function TOCIExtLocator.GetFileName: String;
var
    ADir: String;
begin
    GetFileDir(ADir, Result);
end;

procedure TOCIExtLocator.SetDirectory(const Value: String);
begin
    SetFileDir(Value, FileName);
end;

procedure TOCIExtLocator.SetFileName(const Value: String);
begin
    SetFileDir(Directory, Value);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIDescribe.Create(ASvc: TOCIService);
begin
    inherited Create;
    FOwner := ASvc;
    Init(ASvc.FOwner, OCI_HTYPE_DESCRIBE);
    FStack := TList.Create;
    FCurr := TOCIHandle.Create;
    FCurr.FOwner := FOwner;
    FCurr.FType := OCI_DTYPE_PARAM;
end;

destructor TOCIDescribe.Destroy;
begin
    FStack.Free;
    FCurr.FHandle := nil;
    FCurr.Free;
    inherited Destroy;
end;

procedure TOCIDescribe.DescribeName(const AName: String);

    procedure InternalDescribeName(const AName: String; ACanAddPublic: Boolean);
    var
        res: sb4;
    begin
        repeat
            res := NCOci.OCIDescribeAny(FOwner.FHandle, Error.FHandle, PChar(AName),
                Length(AName), OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, FHandle);
            if res = OCI_STILL_EXECUTING then
                TOCIService(FOwner).DoYield
            else if (res = OCI_ERROR) and ACanAddPublic then begin
                InternalDescribeName('"PUBLIC".' + AName, False);
                res := OCI_SUCCESS;
            end
            else
                Check('OCIDescribeAny', res);
        until res <> OCI_STILL_EXECUTING;
        FCurr.FHandle := GetHandleAttr(OCI_ATTR_PARAM);
    end;
begin
    InternalDescribeName(AName, True);
end;

function TOCIDescribe.OpenList(AAtrType: Integer): ub4;
begin
    FStack.Add(FCurr.FHandle);
    FStack.Add(FCurr.GetHandleAttr(AAtrType));
    Result := 0;
    Check('OCIAttrGet ub4', NCOci.OCIAttrGet(FStack.Last, OCI_DTYPE_PARAM,
        @Result, nil, OCI_ATTR_NUM_PARAMS, Error.FHandle));
end;

procedure TOCIDescribe.CloseList;
begin
    FCurr.FHandle := FStack.Items[FStack.Count - 2];
    FStack.Delete(FStack.Count - 1);
    FStack.Delete(FStack.Count - 1);
end;

function TOCIDescribe.GetIsListOpened: Boolean;
begin
    Result := FStack.Count > 0;
end;

procedure TOCIDescribe.GoToItem(AIndex: Integer);
begin
    Check('OCIParamGet', NCOci.OCIParamGet(FStack.Last, OCI_DTYPE_PARAM,
        Error.FHandle, FCurr.FHandle, AIndex));
end;

function TOCIDescribe.GetSelectItem(AIndex: Integer): TOCISelectItem;
begin
    try
        GoToItem(AIndex);
    except on E: EOCINativeError do
        if E.Errors[0].ErrorCode = 24334 then begin
            Result := nil;
            Exit;
        end
        else
            raise;
    end;
    Result := TOCISelectItem.Create(TOCIStatement(FOwner));
    Result.FHandle := FCurr.FHandle;
end;

function TOCIDescribe.GetPtr(AIndex: Integer): Pointer;
begin
    Result := FCurr.GetHandleAttr(AIndex);
end;

function TOCIDescribe.GetSB1(AIndex: Integer): sb1;
begin
    Result := FCurr.GetSB1Attr(AIndex);
end;

function TOCIDescribe.GetText(AIndex: Integer): String;
begin
    Result := FCurr.GetStringAttr(AIndex);
end;

function TOCIDescribe.GetUB1(AIndex: Integer): ub1;
begin
    Result := FCurr.GetUB1Attr(AIndex);
end;

function TOCIDescribe.GetUB2(AIndex: Integer): ub2;
begin
    Result := FCurr.GetUB2Attr(AIndex);
end;

function TOCIDescribe.GetUB4(AIndex: Integer): ub4;
begin
    Result := FCurr.GetUB4Attr(AIndex);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIDirectPath.Create(ASvc: TOCIService);
begin
    inherited Create;
    FOwner := ASvc;
    Init(ASvc.FOwner, OCI_HTYPE_DIRPATH_CTX);
end;

procedure TOCIDirectPath.Abort;
begin
    Check('OCIDirPathAbort', NCOci.OCIDirPathAbort(Handle, Error.Handle));
end;

procedure TOCIDirectPath.Finish;
begin
    Check('OCIDirPathFinish', NCOci.OCIDirPathFinish(Handle, Error.Handle));
end;

procedure TOCIDirectPath.Prepare;
begin
    Check('OCIDirPathPrepare', NCOci.OCIDirPathPrepare(Handle,
        FOwner.Handle, Error.Handle));
end;

procedure TOCIDirectPath.LoadStream(AStream: TOCIDirectPathStream);
begin
    Check('OCIDirPathLoadStream', NCOci.OCIDirPathLoadStream(Handle,
        AStream.Handle, Error.Handle));
end;

function TOCIDirectPath.GetColumns(AIndex: Integer): TOCIDirectPathColumnParam;
begin
    Result := TOCIDirectPathColumnParam.Create(Self);
    Check('OCIParamGet', NCOci.OCIParamGet(LIST_COLUMNS, OCI_DTYPE_PARAM,
        Error.FHandle, Result.FHandle, AIndex));
end;

constructor TOCIDirectPathColArray.Create(ADP: TOCIDirectPath);
begin
    inherited Create;
    FOwner := ADP;
    Init(ADP, OCI_HTYPE_DIRPATH_COLUMN_ARRAY);
end;

procedure TOCIDirectPathColArray.EntryGet(ARowNum: ub4; AColIndex: ub2; var AData: pUb1;
    var ALen: ub4; var AFlag: ub1);
begin
    Check('OCIDirPathColArrayEntryGet', NCOci.OCIDirPathColArrayEntryGet(Handle,
        Error.Handle, ARowNum, AColIndex, AData, ALen, AFlag));
end;

procedure TOCIDirectPathColArray.EntrySet(ARowNum: ub4; AColIndex: ub2; AData: pUb1;
    ALen: ub4; AFlag: ub1);
begin
    Check('OCIDirPathColArrayEntrySet', NCOci.OCIDirPathColArrayEntrySet(Handle,
        Error.Handle, ARowNum, AColIndex, AData, ALen, AFlag));
end;

procedure TOCIDirectPathColArray.RowGet(ARowNum: ub4; var ADataArr: ppUb1;
    var ALenArr: pUb4; var AFlagArr: pUb1);
begin
    Check('OCIDirPathColArrayRowGet', NCOci.OCIDirPathColArrayRowGet(Handle,
        Error.Handle, ARowNum, ADataArr, ALenArr, AFlagArr));
end;

procedure TOCIDirectPathColArray.Reset;
begin
    Check('OCIDirPathColArrayReset', NCOci.OCIDirPathColArrayReset(Handle,
        Error.Handle));
end;

function TOCIDirectPathColArray.ToStream(AStream: TOCIDirectPathStream;
    ARowCount, ARowIndex: ub4): sword;
begin
    Result := NCOci.OCIDirPathColArrayToStream(Handle, FOwner.Handle,
        AStream.Handle, Error.Handle, ARowCount, ARowIndex);
    if (Result = OCI_SUCCESS) or (Result = OCI_CONTINUE) or (Result = OCI_NEED_DATA) then
        Check('OCIDirPathColArrayToStream', OCI_SUCCESS)
    else
        Check('OCIDirPathColArrayToStream', Result);
end;

constructor TOCIDirectPathStream.Create(ADP: TOCIDirectPath);
begin
    inherited Create;
    FOwner := ADP;
    Init(ADP, OCI_HTYPE_DIRPATH_STREAM);
end;

procedure TOCIDirectPathStream.Reset;
begin
    Check('OCIDirPathStreamReset', NCOci.OCIDirPathStreamReset(Handle,
        Error.Handle));
end;

constructor TOCIDirectPathColumnParam.Create(ADP: TOCIDirectPath);
begin
    inherited Create;
    FOwner := ADP;
    FType := OCI_DTYPE_PARAM;
end;

function TOCIDirectPathColumnParam.GetDataType: TOCIDirecPathDataType;
begin
    case DATA_TYPE of
    SQLT_CHR: Result := dtString;
    SQLT_DAT: Result := dtDateTime;
    SQLT_INT: Result := dtInteger;
    SQLT_UIN: Result := dtUInteger;
    SQLT_FLT: Result := dtFloat;
    SQLT_BIN: Result := dtRaw;
    else Result := dtUnknown;
    end;
end;

procedure TOCIDirectPathColumnParam.SetDataType(AValue: TOCIDirecPathDataType);
begin
    DATA_TYPE := nc2ociDPDataType[AValue];
    DATA_SIZE := nc2ociDPValueSize[AValue];
    if AValue in [dtString, dtDateTime, dtRaw, dtUnknown] then begin
        PRECISION := 0;
        SCALE := 0;
    end
    else if AValue in [dtInteger, dtUInteger] then
        SCALE := 0;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

constructor TOCIIntLocator.Create(AService: TOCIService; AType: ub4);
begin
  inherited Create(AService, OCI_DTYPE_LOB);
end;

constructor TOCIExtLocator.Create(AService: TOCIService; AType: ub4);
begin
  inherited Create(AService, OCI_DTYPE_FILE);
end;

constructor TOCILobLocator.CreateTemporary(AService: TOCIService; AType: ub4;
  ACache: Boolean);
var
    lobtype: ub1;
begin
  Create(AService, AType);

  if AType = SQLT_CLOB then
      lobtype := OCI_TEMP_CLOB
  else 
      lobtype := OCI_TEMP_BLOB;

  Check('OCILOBCreateTemporary', NCOci.OCILOBCreateTemporary(FOwner.FHandle,
      Error.FHandle, FHandle, OCI_DEFAULT, SQLCS_IMPLICIT, lobtype, ACache,
      OCI_DURATION_SESSION));
end;

function TOCILobLocator.GetIsTemporary: LongBool;
begin
  if FOCIVersion < cvOracle81000 then
    Result := false
  else
    Check('OCILOBIsTemporary', NCOci.OCILOBIsTemporary(FOwner.FOwner.FHandle,
        Error.FHandle, FHandle, Result));
end;

initialization

    FGlobalPieceBuffUseCnt := 0;

end.

