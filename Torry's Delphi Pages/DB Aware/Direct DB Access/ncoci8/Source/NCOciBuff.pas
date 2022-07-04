{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.05.01 / 01.10.2000                        }
{Comment:   NC OCI8 VCL: scrollable cursor implement.   }
{Copyright: (c) 1999-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciBuff;

interface

Uses Windows, Classes, Forms, SysUtils, NCOciWrapper, NCOci;

type
    TOCIUpdatesJournal = class;
    TOCICursor = class;
    TOCIDeadCursor = class;
    POCIBookmark = pointer;

    // ------------------------------------------------------------------------
    // Updates Journal
    // ------------------------------------------------------------------------

    TOCIUQItem = record
        FID: Integer;
        FCursor: TOCICursor;
        FBookmark: POCIBookmark;
    end;
    POCIUQItem = ^TOCIUQItem;
    TOCIUpdatesJournalOperation = (qoApply, qoCancel, qoCommit, qoCount, qoUndo);

    TOCIUpdatesJournal = class
    private
        FLastID: Integer;
        FJournal: TList;
        FInApplyUpdates: Boolean;
        FLock: TRTLCriticalSection;
        function GetUpdatesPending: Boolean;
        procedure StartAccess;
        procedure EndAccess;
        procedure SetUpdatesID(const Value: Integer);
    public
        constructor Create;
        destructor Destroy; override;
        procedure AddUpdate(ACursor: TOCICursor; ABookmark: POCIBookmark);
        procedure RemoveCursorUpdates(ACursor: TOCICursor);
        procedure RemoveUpdate(ACursor: TOCICursor; ABookmark: POCIBookmark);
        procedure Reset;
        function ProcessJournal(ACursors: TList; AToID: Integer;
            AOperation: TOCIUpdatesJournalOperation): Integer;
        function ApplyUpdates(ACursors: TList; AToID: Integer): Integer;
        function CancelUpdates(ACursors: TList; AToID: Integer): Integer;
        function CommitUpdates(ACursors: TList; AToID: Integer): Integer;
        function UpdatesCount(ACursors: TList; AToID: Integer): Integer;
        function UndoUpdates(ACursors: TList; AToID: Integer): Integer;
        property InApplyUpdates: Boolean read FInApplyUpdates;
        property UpdatesPending: Boolean read GetUpdatesPending;
        property UpdatesID: Integer read FLastID write SetUpdatesID;
    end;

    // ------------------------------------------------------------------------
    // Generic cursor
    // ------------------------------------------------------------------------

    TOCIRecordStatus = (rsEmpty, rsFetched, rsOld, rsNewInserted, rsNewUpdated,
        rsNewDeleted, rsNewInsApplyed, rsNewUpdApplyed, rsNewDelApplyed,
        rsDelphiBuff, rsUnUsed);
    POCIRecordStatus = ^TOCIRecordStatus;
    TOCIRecordStatuses = set of TOCIRecordStatus;

    TOCICursorFilter = procedure (ASender: TOCICursor; var AAccept: Boolean) of object;
    TOCIUpdateRecordEvent = procedure (Sender: TOCICursor; var AAction: TOCIUpdateAction) of object;
    TOCIUpdateErrorEvent = procedure (Sender: TOCICursor; E: Exception; var AAction: TOCIUpdateAction) of object;

    TOCICursorRecProp = record
        FStatus: TOCIRecordStatus;
        FNumber: ub4;
    end;
    POCICursorRecProp = ^TOCICursorRecProp;

    TOCICursor = class
    private
        function GetDefVar(APosition: sb4): TOCIVariable;
        function GetDefVarCount: sb4;
        procedure SetCachedUpdates(const Value: Boolean);
        procedure SetUpdatesJournal(AValue: TOCIUpdatesJournal);
    protected
        FStamp: array[0..3] of Char;
        FDefVars: TList;
        FEof, FBof: Boolean;
        FOnFilterRecord: TOCICursorFilter;
        FCursorCanceled: Boolean;
        FCachedUpdates: Boolean;
        FInApplyUpdates: Boolean;
        FUpdatesPending: Boolean;
        FStatusFilter: TOCIRecordStatuses;
        FOnUpdateRecord: TOCIUpdateRecordEvent;
        FOnUpdateError: TOCIUpdateErrorEvent;
        FUpdatesJournal: TOCIUpdatesJournal;
        procedure CheckActive;
        procedure CheckInactive;
        function GetRecNo: ub4; virtual; abstract;
        procedure SetRecNo(AValue: ub4); virtual; abstract;
        function GetActive: Boolean; virtual; abstract;
        function GetRecordCount: ub4; virtual; abstract;
        function GetRecordStatus: TOCIRecordStatus; virtual; abstract;
        function GetIsBookmarkDefined: Boolean; virtual; abstract;
        function GetBookmark: POCIBookmark; virtual; abstract;
        procedure GotoBookmark(ABmk: POCIBookmark); virtual; abstract;
        function GetBookmarkSize: ub4; virtual; abstract;
        procedure SetCurrentRecord(P: POCIBookmark; AResync: Boolean); virtual; abstract;
        procedure CheckBidir; virtual; abstract;
        procedure CheckNoBLOB;
    public
        constructor Create;
        destructor Destroy; override;
        procedure AddDefVar(AVar: TOCIVariable);
        procedure RemoveDefVar(APosition: sb4);

        function GetData(ABmk: POCIBookmark; APosition: sb4; ABuff: pUb1;
            var ASize: ub4): Boolean; virtual; abstract;
        function GetDataPtr(ABmk: POCIBookmark; APosition: sb4; var ABuff: pUb1;
            var ASize: ub4): Boolean; virtual; abstract;
        procedure SetData(ABmk: POCIBookmark; APosition: sb4; ABuff: pUb1;
            ASize: ub4); virtual; abstract;
        procedure SetIsNull(ABmk: POCIBookmark; APosition: sb4;
            AIsNull: Boolean); virtual; abstract; // for LOB fields
        function GetExtendedData(ABmk: POCIBookmark; APosition: sb4): TObject;
             virtual; abstract;
        procedure SetExtendedData(ABmk: POCIBookmark; APosition: sb4;
            AData: TObject); virtual; abstract;

        function AllocateRecord: POCIBookmark; virtual; abstract;
        procedure FreeRecord(P: POCIBookmark); virtual; abstract;
        procedure InitRecord(P: POCIBookmark); virtual; abstract;
        procedure GetRecord(P, PRef: POCIBookmark; var ARecProc: TOCICursorRecProp); virtual; abstract;
        procedure DeleteRecord(pToDel: POCIBookmark); virtual; abstract;
        procedure ModifyRecord(pToUpd, P: POCIBookmark); virtual; abstract;
        procedure InsertRecord(P: POCIBookmark); virtual; abstract;
        function CompareBookmarks(P1, P2: POCIBookmark): sb2; virtual; abstract;
        function BookmarkValid(P: POCIBookmark): Boolean; virtual; abstract;
        procedure CopyRecord(PCopyFrom, PCopyTo: POCIBookmark; ASyncHandles: Boolean); virtual; abstract;
        procedure ResetLobs(P: POCIBookmark); virtual; abstract;
        function GetLocked(P: POCIBookmark): Boolean; virtual; abstract;
        procedure SetLocked(P: POCIBookmark; AValue: Boolean); virtual; abstract;

        procedure OpenComplete; virtual; abstract;
        procedure Close; virtual;
        procedure Skip(ANum: sb4); virtual; abstract;
        procedure First(AtBOF: Boolean); virtual; abstract;
        procedure Last(AtEOF: Boolean); virtual; abstract;
        procedure FetchAll; virtual;

        function GetNewRecord(P: POCIBookmark): POCIBookmark; virtual; abstract;
        function GetOldRecord(P: POCIBookmark): POCIBookmark; virtual; abstract;
        procedure ApplyUpdates(P: POCIBookmark); virtual; abstract;
        procedure CancelUpdates(P: POCIBookmark); virtual; abstract;
        procedure CommitUpdates(P: POCIBookmark); virtual; abstract;
        procedure EndUpdate(P: POCIBookmark);

        property Eof: Boolean read FEof;
        property Bof: Boolean read FBof;
        property BookmarkSize: ub4 read GetBookmarkSize;
        property IsBookmarkDefined: Boolean read GetIsBookmarkDefined; 
        property Bookmark: POCIBookmark read GetBookmark write GotoBookmark;
        property RecNo: ub4 read GetRecNo write SetRecNo;
        property RecordCount: ub4 read GetRecordCount;
        property RecordStatus: TOCIRecordStatus read GetRecordStatus;
        property Active: Boolean read GetActive;
        property DefVarCount: sb4 read GetDefVarCount;
        property DefVar[AIndex: sb4]: TOCIVariable read GetDefVar;

        property OnFilterRecord: TOCICursorFilter read FOnFilterRecord write FOnFilterRecord;
        property CursorCanceled: Boolean read FCursorCanceled;
        property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates;
        property InApplyUpdates: Boolean read FInApplyUpdates;
        property UpdatesPending: Boolean read FUpdatesPending;
        property StatusFilter: TOCIRecordStatuses read FStatusFilter write FStatusFilter;
        property UpdatesJournal: TOCIUpdatesJournal read FUpdatesJournal write SetUpdatesJournal;
        property OnUpdateRecord: TOCIUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
        property OnUpdateError: TOCIUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    end;

    // ------------------------------------------------------------------------
    // Scrollable cursor with dead caching
    // ------------------------------------------------------------------------

    POCIDeadBookmark = ^TOCIDeadBookmark;
    PPOCIDeadBookmark = ^POCIDeadBookmark;
    TOCIDeadBookmark = record
        FPageIndex: sb4;
        FRecIndex: sb4;
    end;

    POCIDeadRecordHeader = ^TOCIDeadRecordHeader;
    PPOCIDeadRecordHeader = ^POCIDeadRecordHeader;
    TOCIDeadRecordHeader = record
        FSelfBmk: TOCIDeadBookmark;
        FStatus: TOCIRecordStatus;
        FRecordNumber: ub4;
        FRefRecord: POCIDeadRecordHeader;
        FLocked: Boolean;
    end;

    POCIDeadPage = pUb1;
    // ------------------------
    // TOCIDeadRecordHeader[0]
    // ...
    // TOCIDeadRecordHeader[FRecPerPage - 1]
    // ------------------------
    // DATA Var[1]
    // ...
    // DATA Var[FDefVars.Count]
    // ------------------------
    // Extended info for Var[i1]
    // ...
    // Extended info for Var[in]

    TUB4s = array[0..511] of ub4;
    PUB4s = ^TUB4s;

    TOCIDeadCursor = class(TOCICursor)
    private
        FStmt: TOCIStatement;
        FRecPerPage: sb4;
        FPageSize: ub4;
        FPages: TList;
        FPosition: TOCIDeadBookmark;
        FRowsInserted, FRowsDeleted: ub4;
        FVarOffsets: PUB4s;
        FFreeRecordsPage: sb4;
        FUnidirectional: Boolean;
        FOpenComplete: Boolean;
        FDisableFetch: Boolean;
        FActive: Boolean;
        procedure PageUpdateSize;
        procedure PageClear(APageInd: sb4);
        function PageNew: Integer;
        procedure PageFree(APageInd: sb4);
        function PageRecordHeader(APageInd, ARecInd: sb4): POCIDeadRecordHeader;
        procedure PageBind(APageInd: sb4);
        procedure PageFetched(APageInd: sb4);
        procedure FreePages;
        function PageRecordData(APageInd: sb4; APosition: sb4): pUb1;
        function IsPositionValid(APageInd, ARecInd: sb4; AExcludeEO: Boolean): Boolean;
        procedure CheckValidatePosition(APageInd, ARecInd: sb4; AExcludeEO: Boolean);
{$IFDEF OCI_DEBUG}
        function P2RecHead(P: Pointer): POCIDeadRecordHeader;
        procedure DumpRec(P, PExcl: POCIBookmark; const AName: String);
{$ENDIF}
        procedure ClearRecord(PRec: POCIDeadBookmark);
        function AllocateRecordInternal(AStatus: TOCIRecordStatus): POCIDeadRecordHeader;
        procedure CheckModifable;
        procedure UpdateFreeRecPage(APageInd: ub4);
    protected
        function GetRecNo: ub4; override;
        procedure SetRecNo(AValue: ub4); override;
        function GetActive: Boolean; override;
        function GetRecordCount: ub4; override;
        function GetIsBookmarkDefined: Boolean; override;
        function GetBookmark: POCIBookmark; override;
        procedure GotoBookmark(ABmk: POCIBookmark); override;
        function GetBookmarkSize: ub4; override;
        function GetRecordStatus: TOCIRecordStatus; override;
        function SetRecordStatus(APageInd, ARecInd: sb4; AStatus: TOCIRecordStatus): POCIDeadRecordHeader;
        procedure SetCurrentRecord(P: POCIBookmark; AResync: Boolean); override;
        procedure CheckBidir; override;
    public
        constructor Create;
        destructor Destroy; override;

        function GetData(ABmk: POCIBookmark; APosition: sb4; ABuff: pUb1;
            var ASize: ub4): Boolean; override;
        function GetDataPtr(ABmk: POCIBookmark; APosition: sb4; var ABuff: pUb1;
            var ASize: ub4): Boolean; override;
        procedure SetData(ABmk: POCIBookmark; APosition: sb4; ABuff: pUb1;
            ASize: ub4); override;
        procedure SetIsNull(ABmk: POCIBookmark; APosition: sb4;
            AIsNull: Boolean); override; // for LOB fields
        function GetExtendedData(ABmk: POCIBookmark; APosition: sb4): TObject;
            override;
        procedure SetExtendedData(ABmk: POCIBookmark; APosition: sb4;
            AData: TObject); override;

        function AllocateRecord: POCIBookmark; override;
        procedure FreeRecord(P: POCIBookmark); override;
        procedure InitRecord(P: POCIBookmark); override;
        procedure GetRecord(P, PRef: POCIBookmark; var ARecProc: TOCICursorRecProp); override;
        procedure DeleteRecord(pToDel: POCIBookmark); override;
        procedure ModifyRecord(pToUpd, P: POCIBookmark); override;
        procedure InsertRecord(P: POCIBookmark); override;
        function CompareBookmarks(P1, P2: POCIBookmark): sb2; override;
        function BookmarkValid(P: POCIBookmark): Boolean; override;
        procedure CopyRecord(PCopyFrom, PCopyTo: POCIBookmark; ASyncHandles: Boolean); override;
        procedure ResetLobs(P: POCIBookmark); override;
        function GetLocked(P: POCIBookmark): Boolean; override;
        procedure SetLocked(P: POCIBookmark; AValue: Boolean); override;

        procedure Open(AStmt: TOCIStatement; AService: TOCIService;
            AFetchSize: sb4; AExactRows: sb4; AUnidirectional, AToFirst: Boolean);
        procedure OpenComplete; override;
        procedure Describe(AStmt: TOCIStatement; AService: TOCIService);
        procedure Close; override;
        procedure Skip(ANum: sb4); override;
        procedure First(AtBOF: Boolean); override;
        procedure Last(AtEOF: Boolean); override;
        procedure FetchAll; override;

        function GetNewRecord(P: POCIBookmark): POCIBookmark; override;
        function GetOldRecord(P: POCIBookmark): POCIBookmark; override;
        procedure ApplyUpdates(P: POCIBookmark); override;
        procedure CancelUpdates(P: POCIBookmark); override;
        procedure CommitUpdates(P: POCIBookmark); override;

        property HStatement: TOCIStatement read FStmt;
    end;

implementation

Uses NCOciMsg {$IFDEF OCI_DEBUG}, NCSQLMon {$ENDIF};

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// TOCIUpdatesJournal - updates journal, that synchronize updates
// in multiple cursors

constructor TOCIUpdatesJournal.Create;
begin
    inherited Create;
    FJournal := TList.Create;
    FLastID := 0;
    InitializeCriticalSection(FLock);
end;

destructor TOCIUpdatesJournal.Destroy;
begin
    Reset;
    FJournal.Free;
    DeleteCriticalSection(FLock);
    inherited Destroy;
end;

procedure TOCIUpdatesJournal.StartAccess;
begin
    EnterCriticalSection(FLock);
end;

procedure TOCIUpdatesJournal.EndAccess;
begin
    LeaveCriticalSection(FLock);
end;

function TOCIUpdatesJournal.GetUpdatesPending: Boolean;
begin
    Result := (FJournal.Count > 0);
end;

function TOCIUpdatesJournal.ProcessJournal(ACursors: TList; AToID: Integer;
    AOperation: TOCIUpdatesJournalOperation): Integer;
var
    i, j: Integer;
    crsFnd: Boolean;
begin
    StartAccess;
    Result := 0;
    FInApplyUpdates := True;
    try
        if AOperation = qoUndo then
            i := FJournal.Count - 1
        else
            i := 0;
        while i < FJournal.Count do begin
            with POCIUQItem(FJournal[i])^ do begin
                if (AToID > 0) and
                   ((AOperation <> qoUndo) and (FID > AToID) or
                    (AOperation = qoUndo) and (FID < AToID)) then
                    Break;
                if ACursors = nil then
                    crsFnd := True
                else begin
                    crsFnd := False;
                    j := 0;
                    while (j < ACursors.Count) and not crsFnd do
                        if TOCICursor(ACursors[j]) = FCursor then
                            crsFnd := True
                        else
                            Inc(j);
                end;
                if crsFnd then begin
                    case AOperation of
                    qoApply:  FCursor.ApplyUpdates(FBookmark);
                    qoCancel: FCursor.CancelUpdates(FBookmark);
                    qoUndo:   FCursor.CancelUpdates(FBookmark);
                    qoCommit: FCursor.CommitUpdates(FBookmark);
                    end;
                    Inc(Result);
                end;
                if AOperation = qoUndo then
                    Dec(i)
                else if i < FJournal.Count then
                    if crsFnd and (AOperation in [qoCommit, qoCancel]) then
                        FJournal.Delete(i)
                    else
                        Inc(i);
            end;
        end;
    finally
        FInApplyUpdates := False;
        EndAccess;
    end;
end;

function TOCIUpdatesJournal.ApplyUpdates(ACursors: TList; AToID: Integer): Integer;
begin
    Result := ProcessJournal(ACursors, AToID, qoApply);
end;

function TOCIUpdatesJournal.CancelUpdates(ACursors: TList; AToID: Integer): Integer;
begin
    Result := ProcessJournal(ACursors, AToID, qoCancel);
end;

function TOCIUpdatesJournal.CommitUpdates(ACursors: TList; AToID: Integer): Integer;
begin
    Result := ProcessJournal(ACursors, AToID, qoCommit);
end;

function TOCIUpdatesJournal.UpdatesCount(ACursors: TList; AToID: Integer): Integer;
begin
    Result := ProcessJournal(ACursors, AToID, qoCount);
end;

function TOCIUpdatesJournal.UndoUpdates(ACursors: TList; AToID: Integer): Integer;
begin
    Result := ProcessJournal(ACursors, AToID, qoUndo);
end;

procedure TOCIUpdatesJournal.Reset;
var
    p: POCIUQItem;
    i: Integer;
begin
    StartAccess;
    try
        for i := 0 to FJournal.Count - 1 do begin
            p := POCIUQItem(FJournal[i]);
            Dispose(p);
        end;
        FJournal.Clear;
        FLastID := 0;
    finally
        EndAccess;
    end;
end;

procedure TOCIUpdatesJournal.AddUpdate(ACursor: TOCICursor;
  ABookmark: POCIBookmark);
var
    p: POCIUQItem;
begin
    StartAccess;
    try
        New(p);
        with p^ do begin
            FID := FLastID;
            Inc(FLastID);
            FCursor := ACursor;
            FBookmark := ABookmark;
        end;
        FJournal.Add(p);
    finally
        EndAccess;
    end;
end;

procedure TOCIUpdatesJournal.RemoveCursorUpdates(ACursor: TOCICursor);
var
    i: Integer;
    p: POCIUQItem;
begin
    if FInApplyUpdates then
        Exit;
    StartAccess;
    try
        i := 0;
        while i <= FJournal.Count - 1 do
            if POCIUQItem(FJournal[i])^.FCursor = ACursor then begin
                p := POCIUQItem(FJournal[i]);
                Dispose(p);
                FJournal.Delete(i);
            end
            else
                Inc(i);
    finally
        EndAccess;
    end;
end;

procedure TOCIUpdatesJournal.RemoveUpdate(ACursor: TOCICursor;
  ABookmark: POCIBookmark);
var
    i: Integer;
    p: POCIUQItem;
begin
    if FInApplyUpdates then
        Exit;
    StartAccess;
    try
        i := 0;
        while i <= FJournal.Count - 1 do
            with POCIUQItem(FJournal[i])^ do
                if (FCursor = ACursor) and
                   (FCursor.CompareBookmarks(FBookmark, aBookmark) = 0) then begin
                    p := POCIUQItem(FJournal[i]);
                    Dispose(p);
                    FJournal.Delete(i);
                end
                else
                    Inc(i);
    finally
        EndAccess;
    end;
end;

procedure TOCIUpdatesJournal.SetUpdatesID(const Value: Integer);
begin
    if FLastID > Value then begin
        UndoUpdates(nil, Value);
        if Value < 0 then
            FLastID := 0
        else
            FLastID := Value;
    end;
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// Generic cursor

constructor TOCICursor.Create;
begin
    inherited Create;
    FStamp[0] := 'C';
    FStamp[1] := 'R';
    FStamp[2] := 'T';
    FStamp[3] := 'D';
    FDefVars := TList.Create;
    FStatusFilter := [rsFetched, rsNewInserted, rsNewUpdated,
        rsNewInsApplyed, rsNewUpdApplyed];
end;

destructor TOCICursor.Destroy;
begin
    FDefVars.Free;
    FStamp[0] := 'D';
    FStamp[1] := 'S';
    FStamp[2] := 'T';
    FStamp[3] := 'R';
    inherited Destroy;
end;

procedure TOCICursor.AddDefVar(AVar: TOCIVariable);
begin
    CheckInactive;
    while ub4(FDefVars.Count) < AVar.Position do
        FDefVars.Add(nil);
    FDefVars[AVar.Position - 1] := AVar;
end;

procedure TOCICursor.CheckActive;
    procedure BuffMBOpen;
    begin
        OCIDBError(msgOCIBuffMBOpen, nil);
    end;
begin
    if not Active then
        BuffMBOpen;
end;

procedure TOCICursor.CheckInactive;
    procedure BuffMBClose;
    begin
        OCIDBError(msgOCIBuffMBClose, nil);
    end;
begin
    if Active then
        BuffMBClose;
end;

function TOCICursor.GetDefVarCount: sb4;
begin
    Result := FDefVars.Count;
end;

function TOCICursor.GetDefVar(APosition: sb4): TOCIVariable;
begin
    if (APosition > FDefVars.Count) or (APosition < 1) then
        OCIDBError(msgOCIInvalidVarIndex, nil);
    Result := TOCIVariable(FDefVars[APosition - 1]);
end;

procedure TOCICursor.RemoveDefVar(APosition: sb4);
begin
    FDefVars.Delete(APosition - 1);
end;

procedure TOCICursor.EndUpdate(P: POCIBookmark);
begin
    FUpdatesPending := True;
    if not CachedUpdates then begin
        ApplyUpdates(P);
        CommitUpdates(P);
    end
    else begin
        SetCurrentRecord(P, True);
        if FUpdatesJournal <> nil then
            FUpdatesJournal.AddUpdate(Self, P);
    end;
end;

procedure TOCICursor.FetchAll;
begin
    Last(False);
end;

procedure TOCICursor.SetCachedUpdates(const Value: Boolean);
begin
    if FCachedUpdates <> Value then begin
        if not Value then begin
            if UpdatesJournal <> nil then
                UpdatesJournal.RemoveCursorUpdates(Self);
            if UpdatesPending then
                CancelUpdates(nil);
        end
        else begin
            CheckBidir;
            CheckNoBLOB;
        end;
        FCachedUpdates := Value;
    end;
end;

procedure TOCICursor.CheckNoBLOB;
var
    i: Integer;
begin
    for i := 1 to DefVarCount do
        if DefVar[i].DataType in otHTypes then
            OCIDBError(msgOCIBadDT4CU, nil);
end;

procedure TOCICursor.SetUpdatesJournal(AValue: TOCIUpdatesJournal);
begin
    if UpdatesPending then
        CancelUpdates(nil);
    FUpdatesJournal := AValue;
end;

procedure TOCICursor.Close;
begin
    if UpdatesJournal <> nil then
        UpdatesJournal.RemoveCursorUpdates(Self);
end;

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// Scrollable cursor with dead caching

constructor TOCIDeadCursor.Create;
begin
    inherited Create;
    FPages := TList.Create;
    FFreeRecordsPage := -1;
end;

destructor TOCIDeadCursor.Destroy;
begin
    if Active then
        Close;
    FPages.Free;
    inherited Destroy;
end;

function TOCIDeadCursor.GetActive: Boolean;
begin
    // Result := FStmt <> nil;
    Result := FActive;
end;

procedure TOCIDeadCursor.UpdateFreeRecPage(APageInd: ub4);
begin
    if (FRecPerPage > 1) and
       ((FFreeRecordsPage = -1) or (ub4(FFreeRecordsPage) < APageInd)) then
        FFreeRecordsPage := APageInd;
end;

procedure TOCIDeadCursor.PageUpdateSize;
var
    i: Integer;
begin
    FPageSize := 0;
    GetMem(FVarOffsets, FDefVars.Count * SizeOf(Integer));
    for i := 0 to FDefVars.Count - 1 do
        with TOCIVariable(FDefVars[i]) do begin
            ArrayLen := FRecPerPage;
            FVarOffsets[i] := FPageSize;
            FPageSize := FPageSize + BindDataSize;
            if DataType in [otCursor, otNestedDataSet] then
                FPageSize := FPageSize + SizeOf(TObject) * ub4(FRecPerPage);
        end;
    FPageSize := FPageSize + ((SizeOf(TOCIDeadRecordHeader) + 3) and not 3) * ub4(FRecPerPage);
end;

function TOCIDeadCursor.SetRecordStatus(APageInd, ARecInd: sb4; AStatus: TOCIRecordStatus): POCIDeadRecordHeader;
const
    statWithHndl: TOCIRecordStatuses = [rsFetched, rsOld, rsNewInserted,
        rsNewInsApplyed, rsNewDeleted, rsNewDelApplyed];

    procedure ForEachVar(AInit: Boolean; AHandleOwned: Boolean);
    var
        i: Integer;
        bmk: TOCIDeadBookmark;
        data: TObject;
    begin
        for i := 0 to FDefVars.Count - 1 do
            with TOCIVariable(FDefVars[i]) do begin
                ArrayLen := FRecPerPage;
                ExternBuffer := PageRecordData(APageInd, i + 1);
                if DataType in [otCursor, otNestedDataSet] then begin
                    bmk.FPageIndex := APageInd;
                    bmk.FRecIndex := ARecInd;
                    if not AInit then begin
                        data := GetExtendedData(@bmk, i + 1);
                        if (data <> nil) and AHandleOwned then
                            data.Free;
                    end;
                    SetExtendedData(@bmk, i + 1, nil);
                end;
                if AInit then
                    InitBuffer(ARecInd, AHandleOwned)
                else
                    ClearBuffer(ARecInd, AHandleOwned);
            end;
    end;

begin
    Result := PageRecordHeader(APageInd, ARecInd);
    with Result^ do begin
        if ((FStatus in statWithHndl) and not (AStatus in statWithHndl)) or
           ((FStatus <> rsEmpty) and (AStatus = rsEmpty))  then
            ForEachVar(False, FStatus in statWithHndl)
        else if not (FStatus in statWithHndl) and (AStatus in statWithHndl) then
            ForEachVar(True, True);
        FStatus := AStatus;
    end;
end;

procedure TOCIDeadCursor.PageClear(APageInd: sb4);
var
    i: sb4;
begin
    for i := 0 to FRecPerPage - 1 do
        SetRecordStatus(APageInd, i, rsEmpty);
end;

function TOCIDeadCursor.PageNew: Integer;
begin
    Result := FPages.Add(AllocMem(FPageSize));
{$IFDEF OCI_DEBUG}
    PageClear(Result);
{$ENDIF}    
end;

procedure TOCIDeadCursor.PageFree(APageInd: sb4);
var
    p: POCIDeadRecordHeader;
begin
    try
        PageClear(APageInd);
    finally
        p := POCIDeadRecordHeader(FPages[APageInd]);
        FreeMem(p, FPageSize);
        FPages.Delete(APageInd);
    end;
end;

procedure TOCIDeadCursor.FreePages;
begin
    while FPages.Count > 0 do
        PageFree(0);
    FreeMem(FVarOffsets);
end;

function TOCIDeadCursor.PageRecordHeader(APageInd, ARecInd: sb4): POCIDeadRecordHeader;
begin
{$IFDEF OCI_DEBUG}
    CheckValidatePosition(APageInd, ARecInd, True);
{$ENDIF}
    Result := POCIDeadRecordHeader(sb4(FPages[APageInd]) +
        SizeOf(TOCIDeadRecordHeader) * ARecInd);
end;

function TOCIDeadCursor.PageRecordData(APageInd: sb4; APosition: sb4): pUb1;
begin
{$IFDEF OCI_DEBUG}
    CheckValidatePosition(APageInd, FRecPerPage - 1, True);
{$ENDIF}
    Result := pUb1(sb4(FPages[APageInd]) +
        SizeOf(TOCIDeadRecordHeader) * FRecPerPage +
        sb4(FVarOffsets[APosition - 1]));
end;

procedure TOCIDeadCursor.PageBind(APageInd: sb4);
var
    i: Integer;
begin
    for i := 0 to FRecPerPage - 1 do
        SetRecordStatus(APageInd, i, rsFetched);
    for i := 0 to FDefVars.Count - 1 do
        TOCIVariable(FDefVars[i]).Bind;
end;

procedure TOCIDeadCursor.PageFetched(APageInd: sb4);
var
    i: sb4;
    rNo: sb4;
    lastRowCnt: sb4;
begin
    if FStmt = nil then begin
        lastRowCnt := 0;
        FCursorCanceled := True;
    end
    else begin
        lastRowCnt := FStmt.LastRowCount;
        FCursorCanceled := FStmt.Eof;
    end;
    if lastRowCnt = 0 then
        if not FUnidirectional then begin
            PageFree(APageInd);
            Exit;
        end;
    rNo := sb4(RecordCount) - lastRowCnt + 1;
    for i := 0 to lastRowCnt - 1 do
        with PageRecordHeader(APageInd, i)^ do begin
            FSelfBmk.FPageIndex := APageInd;
            FSelfBmk.FRecIndex := i;
            FRecordNumber := rNo + i;
            FRefRecord := nil;
        end;
    for i := lastRowCnt to FRecPerPage - 1 do
        SetRecordStatus(APageInd, i, rsEmpty);
    if lastRowCnt <= FRecPerPage - 1 then
        UpdateFreeRecPage(APageInd);
end;

procedure TOCIDeadCursor.Open(AStmt: TOCIStatement; AService: TOCIService;
    AFetchSize: sb4; AExactRows: sb4; AUnidirectional, AToFirst: Boolean);
begin
    CheckInactive;
    if AFetchSize < 1 then
        AFetchSize := 1;
    if AExactRows > 0 then
        AFetchSize := AExactRows;
    with FPosition do begin
        FPageIndex := -1;
        FRecIndex := AFetchSize - 1;
    end;
    FRecPerPage := AFetchSize;
    FCursorCanceled := False;
    FEof := False;
    FBof := False;
    FRowsInserted := 0;
    FRowsDeleted := 0;
    FStmt := AStmt;
    FUnidirectional := AUnidirectional;
    FFreeRecordsPage := -1;
    FActive := True;
    try
        PageUpdateSize;
        PageNew;
        PageBind(0);
        if FStmt <> nil then
            FStmt.Execute(AService, AFetchSize, 0, AExactRows > 0, False, False);
    except
        FCursorCanceled := True;
        Close;
        raise;
    end;
    PageFetched(0);
    if not AUnidirectional and (AExactRows < 0) then
        FetchAll;
    if AToFirst then begin
        Skip(1);
        FBof := FEof;
    end;
end;

procedure TOCIDeadCursor.Describe(AStmt: TOCIStatement; AService: TOCIService);
begin
    FStmt := AStmt;
    if FStmt <> nil then
        FStmt.Describe(AService);
end;

procedure TOCIDeadCursor.Close;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    FreePages;
    try
        if not FCursorCanceled then begin
            FCursorCanceled := True;
            FStmt.CancelCursor;
        end;
    finally
        FStmt := nil;
        FActive := False;
        FOpenComplete := False;
    end;
    inherited Close;
end;

procedure TOCIDeadCursor.CheckBidir;
begin
    if FUnidirectional and FOpenComplete then
        OCIDBError(msgOCIMustBeBidir, nil);
end;

procedure TOCIDeadCursor.OpenComplete;
begin
    FOpenComplete := True;
end;

procedure TOCIDeadCursor.Skip(ANum: sb4);
var
    d: Integer;
    actualNo: ub4;
    i: Integer;
    ok, unidir: Boolean;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if ANum = 0 then begin
        Skip(1);
        Skip(-1);
        Exit;
    end;
    with FPosition do begin
        if ANum < 0 then begin
            actualNo := -ANum;
            d := -1;
            FBof := (FPageIndex = -1);
            FEof := False;
        end
        else begin
            actualNo := ANum;
            d := 1;
            FBof := False;
            FEof := (FPageIndex = FPages.Count) and FCursorCanceled;
        end;
        unidir := FUnidirectional and (FStatusFilter <> [rsEmpty]);
        while (actualNo > 0) and not FEof and not FBof do begin
            FRecIndex := FRecIndex + d;
            if FRecIndex = -1 then begin
                FPageIndex := FPageIndex - 1;
                FRecIndex := FRecPerPage - 1;
                if FPageIndex = -1 then
                    FBof := True;
            end
            else if FRecIndex = FRecPerPage then begin
                if unidir then begin
                    if (FPageIndex <> -1) then
                        if FCursorCanceled then
                            FEof := True
                        else begin
                            PageClear(0);
                            PageBind(0);
                            FStmt.Next(FRecPerPage);
                            PageFetched(0);
                            if FCursorCanceled then
                                FEof := FStmt.LastRowCount = 0;
                        end;
                    FPageIndex := 0;
                    FRecIndex := 0;
                end
                else begin
                    FPageIndex := FPageIndex + 1;
                    FRecIndex := 0;
                    if FPageIndex = FPages.Count then
                        if FCursorCanceled or (FStatusFilter = [rsEmpty]) then
                            FEof := True
                        else if FDisableFetch then
                            FEof := True
                        else begin
                            i := PageNew;
                            try
                                PageBind(i);
                                FStmt.Next(FRecPerPage);
                                PageFetched(i);
                            except
                                PageFree(i);
                                raise;
                            end;
                            if FCursorCanceled then
                                FEof := FStmt.LastRowCount = 0;
                        end;
                end;
            end;
            if not FEof and not FBof and
               (PageRecordHeader(FPageIndex, FRecIndex)^.FStatus in FStatusFilter) then
                if Assigned(FOnFilterRecord) then begin
                    ok := True;
                    FOnFilterRecord(Self, ok);
                    if ok then
                        Dec(actualNo);
                end
                else
                    Dec(actualNo);
        end;
    end;
end;

procedure TOCIDeadCursor.FetchAll;
var
    i: Integer;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    CheckBidir;
    i := -1;
    try
        while not FCursorCanceled do begin
            i := -1;
            i := PageNew;
            PageBind(i);
            FStmt.Next(FRecPerPage);
            PageFetched(i);
        end;
    except
        if i <> -1 then
            PageFree(i);
        raise;
    end;
end;

procedure TOCIDeadCursor.First(AtBOF: Boolean);
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    CheckBidir;
    with FPosition do begin
        FPageIndex := -1;
        FRecIndex := FRecPerPage - 1;
    end;
    if not AtBOF then
        Skip(1);
    FBof := FEof;
end;

procedure TOCIDeadCursor.Last(AtEOF: Boolean);
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    CheckBidir;
    FetchAll;
    with FPosition do begin
        FPageIndex := FPages.Count;
        FRecIndex := 0;
    end;
    if not AtEOF then
        Skip(-1);
    FEof := FBof;
end;

function TOCIDeadCursor.GetBookmarkSize: ub4;
begin
    Result := SizeOf(TOCIDeadBookmark);
end;

procedure TOCIDeadCursor.CheckValidatePosition(APageInd, ARecInd: sb4; AExcludeEO: Boolean);
    procedure InvBmk;
    begin
        OCIDBError(msgOCIInvalidBmk, nil);
    end;
begin
    if not IsPositionValid(APageInd, ARecInd, AExcludeEO) then
        InvBmk;
end;

function TOCIDeadCursor.IsPositionValid(APageInd, ARecInd: sb4; AExcludeEO: Boolean): Boolean;
begin
    Result := True;
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if AExcludeEO then begin
        if (APageInd < 0) or (APageInd > FPages.Count - 1) then
            Result := False;
    end
    else begin
        if (APageInd < -1) or (APageInd > FPages.Count) then
            Result := False;
    end;
    if Result and ((ARecInd < 0) or (ARecInd > FRecPerPage)) then
        Result := False;
end;

function TOCIDeadCursor.GetIsBookmarkDefined: Boolean;
begin
    Result := Active and
        IsPositionValid(FPosition.FPageIndex, FPosition.FRecIndex, True);
end;

function TOCIDeadCursor.GetBookmark: POCIBookmark;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    Result := @(PageRecordHeader(FPosition.FPageIndex, FPosition.FRecIndex)^.FSelfBmk);
end;

procedure TOCIDeadCursor.GotoBookmark(ABmk: POCIBookmark);
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    with POCIDeadBookmark(ABmk)^ do
        CheckValidatePosition(FPageIndex, FRecIndex, False);
    SetCurrentRecord(ABmk, True);
    if not FBof and not FEof then
        with FPosition, PageRecordHeader(FPageIndex, FRecIndex)^ do
            if (FRefRecord <> nil) and (FStatus = rsDelphiBuff) then
                FPosition := FRefRecord^.FSelfBmk;
end;

procedure TOCIDeadCursor.SetCurrentRecord(P: POCIBookmark; AResync: Boolean);
begin
    if P <> nil then begin
        FPosition := POCIDeadBookmark(P)^;
        FBof := FPosition.FPageIndex = -1;
        FEof := FPosition.FPageIndex = FPages.Count;
    end;
    if AResync then
        Skip(0);
end;

function TOCIDeadCursor.GetRecNo: ub4;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if FEof or FUnidirectional then
        Result := RecordCount
    else if FBof then
        Result := 0
    else
        with FPosition do
            Result := PageRecordHeader(FPageIndex, FRecIndex)^.FRecordNumber;
end;

procedure TOCIDeadCursor.SetRecNo(AValue: ub4);
var
    pgNo, rNo: sb4;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    CheckBidir;
    pgNo := (AValue - 1) div ub4(FRecPerPage);
    rNo := (AValue - 1) mod ub4(FRecPerPage);
    if FCursorCanceled and (pgNo >= FPages.Count) then
        OCIDBError(msgOCIInvalidRNO, nil);
    if pgNo < FPages.Count then begin
        FPosition.FPageIndex := pgNo;
        FPosition.FRecIndex := rNo;
    end
    else begin
        FPosition.FPageIndex := FPages.Count - 1;
        FPosition.FRecIndex := FRecPerPage - 1;
    end;
    FEof := False;
    FBof := False;
    while not Eof and (RecNo < AValue) do
        Skip(1);
end;

function TOCIDeadCursor.GetRecordCount: ub4;
begin
    if FStmt = nil then
        Result := 0
    else
        Result := FStmt.ROW_COUNT;
    Result := Result + FRowsInserted - FRowsDeleted;
end;

function TOCIDeadCursor.GetRecordStatus: TOCIRecordStatus;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if FEof or FBof then
        Result := rsEmpty
    else with FPosition do
        Result := PageRecordHeader(FPageIndex, FRecIndex)^.FStatus;
end;

function TOCIDeadCursor.GetData(ABmk: POCIBookmark; APosition: sb4;
    ABuff: pUb1; var ASize: ub4): Boolean;
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do begin
        ExternBuffer := PageRecordData(FPageIndex, APosition);
        Result := GetData(FRecIndex, ABuff, ASize, dfDataSet);
    end;
end;

function TOCIDeadCursor.GetDataPtr(ABmk: POCIBookmark; APosition: sb4;
    var ABuff: pUb1; var ASize: ub4): Boolean;
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do begin
        ExternBuffer := PageRecordData(FPageIndex, APosition);
        Result := GetDataPtr(FRecIndex, ABuff, ASize);
    end;
end;

procedure TOCIDeadCursor.SetData(ABmk: POCIBookmark; APosition: sb4;
    ABuff: pUb1; ASize: ub4);
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do begin
        ExternBuffer := PageRecordData(FPageIndex, APosition);
        SetData(FRecIndex, ABuff, ASize, dfDataSet);
    end;
end;

procedure TOCIDeadCursor.SetIsNull(ABmk: POCIBookmark; APosition: sb4; AIsNull: Boolean);
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do begin
        ExternBuffer := PageRecordData(FPageIndex, APosition);
        SetIsNull(FRecIndex, AIsNull);
    end;
end;

function TOCIDeadCursor.GetExtendedData(ABmk: POCIBookmark; APosition: sb4): TObject;
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do
        Result := TObject(pUb4(ub4(PageRecordData(FPageIndex, APosition)) +
            BindDataSize + ub4(FRecIndex) * SizeOf(TObject))^);
end;

procedure TOCIDeadCursor.SetExtendedData(ABmk: POCIBookmark; APosition: sb4;
    AData: TObject);
begin
    if ABmk = nil then
        ABmk := @FPosition;
    with DefVar[APosition], POCIDeadBookmark(ABmk)^ do
        pUb4(ub4(PageRecordData(FPageIndex, APosition)) +
            BindDataSize + ub4(FRecIndex) * SizeOf(TObject))^ := Ub4(AData);
end;

procedure TOCIDeadCursor.CopyRecord(PCopyFrom, PCopyTo: POCIBookmark; ASyncHandles: Boolean);
var
    i: Integer;
    p: pUb1;
    l: ub4;
    obj: TObject;

    procedure CopyVar(PCopyFrom, PCopyTo: POCIBookmark; ASyncHandles: Boolean);
    begin
        with DefVar[i] do begin
            with POCIDeadBookmark(PCopyFrom)^ do begin
                ExternBuffer := PageRecordData(FPageIndex, i);
                GetDataPtr(FRecIndex, p, l);
                if DataType in [otCursor, otNestedDataSet] then
                    obj := GetExtendedData(PCopyFrom, i);
            end;
            if ASyncHandles and (DataType in otHTypes) and (ppOCIHandle(p)^ = nil) then
                CopyVar(PCopyTo, PCopyFrom, False)
            else
                with POCIDeadBookmark(PCopyTo)^ do begin
                    ExternBuffer := PageRecordData(FPageIndex, i);
                    SetData(FRecIndex, p, l, dfOCI);
                    if DataType in [otCursor, otNestedDataSet] then
                        SetExtendedData(PCopyTo, i, obj);
                end;
        end;
    end;

begin
    if PCopyFrom = nil then
        PCopyFrom := @FPosition;
    if PCopyTo = nil then
        PCopyTo := @FPosition;
    for i := 1 to FDefVars.Count do
        CopyVar(PCopyFrom, PCopyTo, ASyncHandles);
end;

procedure TOCIDeadCursor.ClearRecord(PRec: POCIDeadBookmark);
var
    i: Integer;
begin
    for i := 1 to FDefVars.Count do
        SetData(PRec, i, nil, 0);
end;

procedure TOCIDeadCursor.ResetLobs(P: POCIBookmark);
var
    i: Integer;
begin
    if P = nil then
        P := @FPosition;
    for i := 0 to FDefVars.Count - 1 do
        with POCIDeadBookmark(P)^, TOCIVariable(FDefVars[i]) do
            if DataType in otHBlobs then begin
                ArrayLen := FRecPerPage;
                ExternBuffer := PageRecordData(FPageIndex, FRecIndex);
                ResetBuffer(FRecIndex);
            end;
end;

function TOCIDeadCursor.GetLocked(P: POCIBookmark): Boolean;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if P = nil then
        P := @FPosition;
    with POCIDeadBookmark(P)^ do
        Result := PageRecordHeader(FPageIndex, FRecIndex)^.FLocked;
end;

procedure TOCIDeadCursor.SetLocked(P: POCIBookmark; AValue: Boolean);
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    if P = nil then
        P := @FPosition;
    with POCIDeadBookmark(P)^ do
        PageRecordHeader(FPageIndex, FRecIndex)^.FLocked := AValue;
end;

// -----------------------------------------------------------------
// -----------------------------------------------------------------
// Delphi buffer management

{$IFDEF OCI_DEBUG}
procedure TOCIDeadCursor.DumpRec(P, PExcl: POCIBookmark; const AName: String);
const
    rs2str: array[TOCIRecordStatus] of String =
        ('rsEmpty', 'rsFetched', 'rsOld', 'rsNewInserted', 'rsNewUpdated',
         'rsNewDeleted', 'rsNewInsApplyed', 'rsNewUpdApplyed', 'rsNewDelApplyed',
         'rsDelphiBuff', 'rsUnUsed');
var
    s: String;
begin
    if (FStmt = nil) or not FStmt.DbgOutActive then
        Exit;
    with POCIDeadBookmark(P)^, PageRecordHeader(FPageIndex, FRecIndex)^ do begin
        if FRefRecord = nil then
            s := 'NIL'
        else
            s := '>>';
        s := Format('%s record {%d, %d}, stat:%s, #:%d, ref:%s', [AName, FPageIndex,
            FRecIndex, rs2str[FStatus], FRecordNumber, s]);
        FStmt.DbgOut(tfDataIn, s);
        if (FRefRecord <> nil) and (FRefRecord <> PExcl) then
            DumpRec(FRefRecord, P, '    ..');
    end;
end;

function TOCIDeadCursor.P2RecHead(P: POCIBookmark): POCIDeadRecordHeader;
begin
    with POCIDeadBookmark(P)^ do
        Result := PageRecordHeader(FPageIndex, FRecIndex);
    if Result^.FStatus <> rsDelphiBuff then
        OCIDBError(msgOCIModifyNotDelphiBuff, nil);
end;
{$ENDIF}

function TOCIDeadCursor.AllocateRecordInternal(AStatus: TOCIRecordStatus): POCIDeadRecordHeader;
var
    savePos: TOCIDeadBookmark;
    saveStFlt: TOCIRecordStatuses;
begin
{$IFDEF OCI_DEBUG}
    CheckActive;
{$ENDIF}
    savePos := FPosition;
    saveStFlt := FStatusFilter;
    StatusFilter := [rsEmpty];
    FDisableFetch := True;
    try
        // Free record page is unknown
        if FFreeRecordsPage = -1 then begin
            // If all records fetched then go to tail page
            // and try find free record
            if FCursorCanceled and (FPages.Count > 0) then begin
                FPosition.FPageIndex := FPages.Count - 1;
                FPosition.FRecIndex := 0;
                Skip(1);
            end
            // ... else allocate new empty page
            else
                FEof := True;
        end
        // ... else try find empty record from FFreeRecordsPage page
        else begin
            FPosition.FPageIndex := FFreeRecordsPage - 1;
            FPosition.FRecIndex := FRecPerPage - 1;
            Skip(1);
        end;
        // Not found, then allocate page
        if FEof then begin
            FPosition.FPageIndex := PageNew;
            FPosition.FRecIndex := 0;
        end;
        FFreeRecordsPage := FPosition.FPageIndex;
        Result := SetRecordStatus(FPosition.FPageIndex, FPosition.FRecIndex, AStatus);
        with Result^ do begin
            FSelfBmk := FPosition;
            FRecordNumber := FPosition.FPageIndex * FRecPerPage + FPosition.FRecIndex + 1;
            FRefRecord := nil;
        end;
    finally
        FPosition := savePos;
        FStatusFilter := saveStFlt;
        FDisableFetch := False;
    end;
end;

function TOCIDeadCursor.AllocateRecord: POCIBookmark;
begin
    Result := @(AllocateRecordInternal(rsDelphiBuff)^.FSelfBmk);
end;

procedure TOCIDeadCursor.FreeRecord(P: POCIBookmark);
begin
    with POCIDeadBookmark(p)^ do begin
        with SetRecordStatus(FPageIndex, FRecIndex, rsEmpty)^ do begin
            if FRefRecord <> nil then
                FRefRecord^.FRefRecord := nil;
            FRefRecord := nil;
        end;
        UpdateFreeRecPage(FPageIndex);
    end;
end;

procedure TOCIDeadCursor.InitRecord(P: POCIBookmark);
begin
{$IFDEF OCI_DEBUG}
    P2RecHead(P)^.FRefRecord := nil;
{$ELSE}
    with POCIDeadBookmark(P)^ do
        PageRecordHeader(FPageIndex, FRecIndex)^.FRefRecord := nil;
{$ENDIF}
    ClearRecord(P);
end;

procedure TOCIDeadCursor.GetRecord(P, PRef: POCIBookmark; var ARecProc: TOCICursorRecProp);
var
    pRH: POCIDeadRecordHeader;
begin
    with FPosition do
        pRH := PageRecordHeader(FPageIndex, FRecIndex);
{$IFDEF OCI_DEBUG}
    P2RecHead(P)^.FRefRecord := pRH;
{$ELSE}
    with POCIDeadBookmark(P)^ do
        PageRecordHeader(FPageIndex, FRecIndex)^.FRefRecord := pRH;
{$ENDIF}
    with ARecProc do begin
        POCIDeadBookmark(PRef)^ := pRH^.FSelfBmk;
        FStatus := pRH^.FStatus;
        if FUnidirectional then
            FNumber := RecordCount
        else
            FNumber := pRH^.FRecordNumber;
    end;
//    CopyRecord(@FPosition, P, False);
// Now record is not copied into local Delphi buffer,
// until we start edit it. It is optimization kind of
// navigational methods. See TOCIDataSet.GetEditBookmark
{$IFDEF OCI_DEBUG}
    DumpRec(P, nil, 'Delphi');
    DumpRec(PRef, nil, 'Referenced');
{$ENDIF}    
end;

procedure TOCIDeadCursor.DeleteRecord(pToDel: POCIBookmark);
var
    pRH: POCIDeadRecordHeader;
    UndoValue: TOCIRecordStatus;
    pUndoVar: POCIRecordStatus;

    procedure InitUndo;
    begin
        pUndoVar := nil;
    end;

    procedure SaveUndo(AStatusVar: POCIRecordStatus);
    begin
        pUndoVar := AStatusVar;
        if pUndoVar <> nil then
            UndoValue := pUndoVar^;
    end;

    procedure DoUndo;
    begin
        if pUndoVar <> nil then
            pUndoVar^ := UndoValue;
    end;

begin
    CheckModifable;
    if pToDel = nil then
        pToDel := @FPosition;
    with POCIDeadBookmark(pToDel)^ do
        pRH := PageRecordHeader(FPageIndex, FRecIndex);
    InitUndo;
    try
        with pRH^ do
            case FStatus of
            rsNewInserted:
                begin
                    Dec(FRowsInserted);
                    FreeRecord(@FSelfBmk);
                    Exit;
                end;
            rsNewDeleted, rsNewDelApplyed:
                Exit;
            rsFetched, rsNewUpdated, rsNewInsApplyed, rsNewUpdApplyed:
                begin
                    SaveUndo(@FStatus);
                    with FSelfBmk do
                        SetRecordStatus(FPageIndex, FRecIndex, rsNewDeleted);
                end;
            rsOld:
                begin
                    DeleteRecord(@FRefRecord^.FSelfBmk);
                    Exit;
                end;
            rsEmpty, rsDelphiBuff:
                OCIDBError(msgOCICantModifyBuff, nil);
            end;
        EndUpdate(@pRH^.FSelfBmk);
        Inc(FRowsDeleted);
    except
        DoUndo;
        raise;
    end;
end;

procedure TOCIDeadCursor.ModifyRecord(pToUpd, P: POCIBookmark);
var
    pCur: POCIDeadRecordHeader;
    pRH: POCIDeadRecordHeader;
    pBmk: POCIDeadBookmark;
    UndoValue: TOCIRecordStatus;
    pUndoVar: POCIRecordStatus;
    UndoRefValue: POCIDeadRecordHeader;
    pUndoRefVar: PPOCIDeadRecordHeader;
    UndoFreeRec: POCIDeadRecordHeader;
    CopyRec: Boolean;

    procedure InitUndo;
    begin
        pUndoVar := nil;
        pUndoRefVar := nil;
        UndoFreeRec := nil;
    end;

    procedure SaveUndo(AStatusVar: POCIRecordStatus;
        ARefRecord, AFreeRec: PPOCIDeadRecordHeader; ACopyRec: Boolean);
    begin
        pUndoVar := AStatusVar;
        if pUndoVar <> nil then
            UndoValue := pUndoVar^;
        pUndoRefVar := @ARefRecord;
        if pUndoRefVar <> nil then
            UndoRefValue := pUndoRefVar^;
        if AFreeRec <> nil then
            UndoFreeRec := AFreeRec^;
        CopyRec := ACopyRec;
    end;

    procedure DoUndo;
    begin
        if CopyRec then
            CopyRecord(@UndoFreeRec^.FSelfBmk, @UndoFreeRec^.FRefRecord^.FSelfBmk, False);
        if pUndoVar <> nil then
            pUndoVar^ := UndoValue;
        if pUndoRefVar <> nil then
            pUndoRefVar^ := UndoRefValue;
        if UndoFreeRec <> nil then
            FreeRecord(@UndoFreeRec^.FSelfBmk);
    end;

begin
    CheckModifable;
    pRH := nil;
    if pToUpd = nil then
        pToUpd := @FPosition;
    with POCIDeadBookmark(pToUpd)^ do
        pCur := PageRecordHeader(FPageIndex, FRecIndex);
    InitUndo;
    try
        with pCur^ do
            case FStatus of
            rsFetched, rsNewDeleted, rsNewDelApplyed, rsNewInsApplyed:
                begin
                    pRH := AllocateRecordInternal(rsOld);
                    pRH^.FRefRecord := pCur;
                    pRH^.FRecordNumber := pCur^.FRecordNumber;
                    SaveUndo(@FStatus, @FRefRecord, @pRH, True);
                    with FSelfBmk do
                        SetRecordStatus(FPageIndex, FRecIndex, rsNewUpdated);
                    FRefRecord := pRH;
                    CopyRecord(@FSelfBmk, @(pRH^.FSelfBmk), False);
                    pRH := pCur;
                end;
            rsOld:
                begin
                    ModifyRecord(@FRefRecord^.FSelfBmk, P);
                    Exit;
                end;
            rsNewUpdated, rsNewInserted:
                pRH := pCur;
            rsNewUpdApplyed:
                begin
                    SaveUndo(@FStatus, nil, nil, False);
                    with FSelfBmk do
                        pRH := SetRecordStatus(FPageIndex, FRecIndex, rsNewUpdated);
                    CopyRecord(@FSelfBmk, @FRefRecord^.FSelfBmk, False);
                end;
            rsEmpty, rsDelphiBuff:
                OCIDBError(msgOCICantModifyBuff, nil);
            end;
        pBmk := @(pRH^.FSelfBmk);
        CopyRecord(P, pBmk, False);
        EndUpdate(pBmk);
    except
        DoUndo;
        raise;
    end;
end;

procedure TOCIDeadCursor.InsertRecord(P: POCIBookmark);
var
    pRH: POCIDeadRecordHeader;
    pBmk: POCIDeadBookmark;
begin
    CheckModifable;
    pRH := AllocateRecordInternal(rsNewInserted);
    pBmk := @pRH^.FSelfBmk;
    try
        CopyRecord(P, pBmk, True);
        EndUpdate(pBmk);
        Inc(FRowsInserted);
    except
        FreeRecord(pBmk);
        raise;
    end;
end;

function TOCIDeadCursor.CompareBookmarks(P1, P2: POCIBookmark): sb2;
begin
    if (POCIDeadBookmark(P1)^.FPageIndex = POCIDeadBookmark(P2)^.FPageIndex) and
       (POCIDeadBookmark(P1)^.FRecIndex = POCIDeadBookmark(P2)^.FRecIndex) then
        Result := 0
    else if (POCIDeadBookmark(P1)^.FPageIndex < POCIDeadBookmark(P2)^.FPageIndex) or
            (POCIDeadBookmark(P1)^.FPageIndex = POCIDeadBookmark(P2)^.FPageIndex) and
                (POCIDeadBookmark(P1)^.FRecIndex < POCIDeadBookmark(P2)^.FRecIndex) then
        Result := -1
    else
        Result := 1;
end;

function TOCIDeadCursor.BookmarkValid(P: POCIBookmark): Boolean;
begin
    try
        with POCIDeadBookmark(P)^ do
            PageRecordHeader(FPageIndex, FRecIndex);
        Result := True;
    except
        Result := False;
    end;
end;

function TOCIDeadCursor.GetNewRecord(P: POCIBookmark): POCIBookmark;
begin
    if p = nil then
        p := @FPosition;
    Result := nil;
    with POCIDeadBookmark(p)^, PageRecordHeader(FPageIndex, FRecIndex)^ do begin
        case FStatus of
             rsFetched, rsNewInserted, rsNewUpdated, rsNewInsApplyed, rsNewUpdApplyed:
                Result := @FSelfBmk;
             rsOld, rsNewDeleted, rsNewDelApplyed:
                if FRefRecord <> nil then
                    Result := @FRefRecord^.FSelfBmk;
             rsDelphiBuff:
                if FRefRecord <> nil then
                    Result := GetNewRecord(@FRefRecord^.FSelfBmk);
        end;
    end;
end;

function TOCIDeadCursor.GetOldRecord(P: POCIBookmark): POCIBookmark;
begin
    if p = nil then
        p := @FPosition;
    Result := nil;
    with POCIDeadBookmark(p)^, PageRecordHeader(FPageIndex, FRecIndex)^ do begin
        case FStatus of
            rsNewUpdated, rsNewUpdApplyed:
                if FRefRecord <> nil then
                    Result := @FRefRecord^.FSelfBmk;
            rsFetched, rsOld, rsNewInserted, rsNewInsApplyed, rsNewDelApplyed:
                Result := @FSelfBmk;
            rsNewDeleted:
                if FRefRecord <> nil then
                    Result := @FRefRecord^.FSelfBmk
                else
                    Result := @FSelfBmk;
            rsDelphiBuff:
                if FRefRecord <> nil then
                    Result := GetOldRecord(@FRefRecord^.FSelfBmk);
        end;
    end;
end;

procedure TOCIDeadCursor.CheckModifable;
begin
    if InApplyUpdates then
        OCIDBError(msgOCIRecurseApply, nil);
    if not Assigned(OnUpdateRecord) then
        OCIDBError(msgOCINoApplyCallback, nil);
end;

procedure TOCIDeadCursor.ApplyUpdates(P: POCIBookmark);
var
    prevStatusFilter: TOCIRecordStatuses;
    updAction: TOCIUpdateAction;
    oldStatus, newStatus: TOCIRecordStatus;
    allApplyed: Boolean;
    lastPosBmk: TOCIDeadBookmark;
begin
    CheckModifable;
    prevStatusFilter := StatusFilter;
    StatusFilter := [rsNewInserted, rsNewUpdated, rsNewDeleted];
    FInApplyUpdates := True;
    FDisableFetch := True;
    allApplyed := True;
    lastPosBmk := FPosition;
    try
        if P = nil then
            First(False)
        else
            SetCurrentRecord(P, False);
        while not Eof do begin
            updAction := uaFail;
            repeat
                try
                    OnUpdateRecord(Self, updAction);
                except
                    on E: Exception do begin
                        if Assigned(OnUpdateError) then
                            OnUpdateError(Self, E, updAction);
                        if not Assigned(OnUpdateError) or (updAction = uaReRaise) then
                            raise;
                    end;
                end;
            until updAction <> uaRetry;
            case updAction of
            uaAbort:
                begin
                    CancelUpdates(P);
                    Abort;
                end;
            uaFail:
                begin
                    CancelUpdates(P);
                    OCIDBError(msgOCIApplyFailed, nil);
                end;
            uaApplied:
                begin
                    with FPosition do
                        oldStatus := PageRecordHeader(FPageIndex, FRecIndex)^.FStatus;
                    newStatus := rsEmpty;
                    case oldStatus of
                        rsNewInserted: newStatus := rsNewInsApplyed;
                        rsNewUpdated: newStatus := rsNewUpdApplyed;
                        rsNewDeleted: newStatus := rsNewDelApplyed;
                    end;
                    with FPosition do
                        SetRecordStatus(FPageIndex, FRecIndex, newStatus);
                end;
            uaSkip:
                allApplyed := False;
            end;
            if P = nil then
                Skip(1)
            else
                Break;
        end;
        FUpdatesPending := not allApplyed;
    finally
        StatusFilter := prevStatusFilter;
        FInApplyUpdates := False;
        FDisableFetch := False;
        SetCurrentRecord(@lastPosBmk, (P = nil) or (updAction <> uaReRaise));
    end;
end;

procedure TOCIDeadCursor.CancelUpdates(P: POCIBookmark);
var
    prevStatusFilter: TOCIRecordStatuses;
    pRH: POCIDeadRecordHeader;
    toP: POCIBookmark;

    procedure SetFetched(pRH: POCIDeadRecordHeader);
    begin
        with pRH^, FSelfBmk do begin
            SetRecordStatus(FPageIndex, FRecIndex, rsFetched);
            if FRefRecord <> nil then
                FreeRecord(@FRefRecord^.FSelfBmk);
            toP := @FSelfBmk;
        end;
    end;

begin
    prevStatusFilter := StatusFilter;
    // all records visible, because we must erase lock mark
    // for all records
    StatusFilter := [rsEmpty .. rsUnUsed];
    FDisableFetch := True;
    try
        if P = nil then
            First(False)
        else begin
            SetCurrentRecord(P, False);
            toP := nil;
        end;
        while not Eof do begin
            with FPosition do
                pRH := PageRecordHeader(FPageIndex, FRecIndex);
            with pRH^ do begin
                case FStatus of
                rsNewInserted:
                    begin
                        Dec(FRowsInserted);
                        FreeRecord(@FSelfBmk);
                    end;
                rsNewUpdated:
                    SetFetched(pRH^.FRefRecord);
                rsNewDeleted:
                    begin
                        Dec(FRowsDeleted);
                        if FRefRecord <> nil then
                            // it is the case, when records history was:
                            //                       [rsFetched]
                            //   [rsNewUpdated] <--> [rsOld]
                            //   [rsNewDeleted] <--> [rsOld]
                            SetFetched(pRH^.FRefRecord)
                        else
                            // in other cases they are:
                            //                       [rsFetched]
                            //                       [rsNewDeleted]
                            SetFetched(pRH);
                    end;
                end;
                if UpdatesJournal <> nil then
                    UpdatesJournal.RemoveUpdate(Self, @FSelfBmk);
                if CachedUpdates then
                    FLocked := False;
            end;
            if P = nil then
                Skip(1)
            else
                Break;
        end;
    finally
        FUpdatesPending := False;
        StatusFilter := prevStatusFilter;
        FDisableFetch := False;
        if P = nil then
            First(False)
        else if toP <> nil then
            SetCurrentRecord(toP, False);
    end;
end;

procedure TOCIDeadCursor.CommitUpdates(P: POCIBookmark);
var
    prevStatusFilter: TOCIRecordStatuses;
    lastPosBmk: TOCIDeadBookmark;
begin
    prevStatusFilter := StatusFilter;
    // all records visible, because we must erase lock mark
    // for all records
    StatusFilter := [rsEmpty .. rsUnUsed];
    FDisableFetch := True;
    try
        if P = nil then begin
            lastPosBmk := FPosition;
            First(False);
        end
        else
            SetCurrentRecord(P, False);
        while not Eof do begin
            with FPosition, PageRecordHeader(FPageIndex, FRecIndex)^ do begin
                case FStatus of
                rsNewInsApplyed:
                    SetRecordStatus(FPageIndex, FRecIndex, rsFetched);
                rsNewDelApplyed:
                    begin
                        if FRefRecord <> nil then
                            FreeRecord(@FRefRecord^.FSelfBmk);
                        FreeRecord(@FSelfBmk);
                    end;
                rsNewUpdApplyed:
                    begin
                        // i does not use there SetRecordStatus, because it
                        // clear record ...
                        FStatus := rsFetched;
                        if FRefRecord <> nil then begin
                            with FRefRecord^.FSelfBmk do
                                PageRecordHeader(FPageIndex, FRecIndex)^.FStatus := rsUnUsed;
                            FreeRecord(@FRefRecord^.FSelfBmk);
                        end;
                    end;
                end;
                if UpdatesJournal <> nil then
                    UpdatesJournal.RemoveUpdate(Self, @FSelfBmk);
                if CachedUpdates then
                    FLocked := False;
            end;
            if P = nil then
                Skip(1)
            else
                Break;
        end;
    finally
        FUpdatesPending := False;
        StatusFilter := prevStatusFilter;
        FDisableFetch := False;
        if P = nil then
            SetCurrentRecord(@lastPosBmk, not FInApplyUpdates);
    end;
end;

end.
