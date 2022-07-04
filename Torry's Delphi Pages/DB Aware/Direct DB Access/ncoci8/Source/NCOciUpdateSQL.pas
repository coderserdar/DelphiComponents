{*******************************************************}
{File:      NCOciUpdateSQL.PAS                          }
{Revision:  0.02.07 / 01.11.2000                        }
{Comment:   NC OCI8 VCL: editing capabilities           }
{Copyright: (c) 2000-2001, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciUpdateSQL;

interface

uses Windows, SysUtils, Graphics, Classes, Controls, Db, NCOci, NCOciWrapper,
     NCOciDB, NCOciParams;

type
    TOCIUpdateSQL = class;

    TOCILockMode = (lmPessimistic, lmOptimistic, lmNone);
    TOCILockPoint = (lpImmediate, lpDeferred);
    TOCIRefreshMode = (rmNone, rmReturning, rmSelect);

    TOCIUpdateSQLEvent = procedure (ASender: TOCIUpdateSQL; AKind: TOCISQLKind;
        var ADefault: Boolean) of object;

    TOCIUpdateSQL = class(TOCIUpdateObject)
    private
        FQueries: array[TOCISQLKind] of TOCIQuery;
        FSQLText: array[TOCISQLKind] of TStrings;
        FSQLParams: array[TOCISQLKind] of TOCIParams;
        FUserDefinedKinds: TOCISQLKinds;
        FSelfUpdating: Boolean;
        FTableName: String;
        FUpdateChangedFields: Boolean;
        FAutoBuildKinds: TOCISQLKinds;
        FLockMode: TOCILockMode;
        FLockPoint: TOCILockPoint;
        FOnSQLGenerate: TOCIUpdateSQLEvent;
        FOnApply: TOCIUpdateSQLEvent;
        FOnDisconnect: TOCIUpdateSQLEvent;
        FRefreshMode: TOCIRefreshMode;
        function GetQuery(ASQLKind: TOCISQLKind): TOCIQuery;
        function GetSQLIndex(Index: Integer): TStrings;
        procedure SetSQL(ASQLKind: TOCISQLKind; Value: TStrings);
        procedure SetSQLIndex(Index: Integer; Value: TStrings);
        function IsSQLStored(Index: Integer): Boolean;
        function GetSQLParams(ASQLKind: TOCISQLKind): TOciParams;
        procedure SetSQLParams(ASQLKind: TOCISQLKind; const Value: TOciParams);
        function GetActualTableName: String;
        function IsOraBlob(AField: TField): Boolean;
        function IsROWID(AField: TField): Boolean;
        procedure SetTableName(const Value: String);
        function Par2FieldName(var AName: String): Integer;
        procedure SetLockMode(const Value: TOCILockMode);
        procedure SetLockPoint(const Value: TOCILockPoint);
        function UseSmartRefresh: Boolean;
    protected
        function BuildFieldParam(AField: TField; ASQLKind: TOCISQLKind;
            AParType: TOCIVarType; ANewValue, AQuote: Boolean): String;
        function BuildFieldSQL(AField: TField): String;
        procedure BuildWherePhrase(ASQLKind: TOCISQLKind);
        procedure BuildReturningPhrase(ASQLKind: TOCISQLKind);
        function GetSQL(ASQLKind: TOCISQLKind): TStrings;
        procedure SQLChanged(Sender: TObject);
        function GenerateLock: Boolean; virtual;
        function GenerateUpdate: Boolean; virtual;
        function GenerateInsert: Boolean; virtual;
        function GenerateDelete: Boolean; virtual;
        function GenerateRefresh: Boolean; virtual;
        function FieldStorable(AField: TField): Boolean; virtual;
        function FieldUpdatable(AField: TField): Boolean; virtual;
        function FieldLocatable(AField: TField): Boolean; virtual;
        function FieldReqRefresh(AField: TField): Boolean; virtual;
        function FieldChanged(AField: TField): Boolean; virtual;
        function FieldInKey(AField: TField): Boolean; virtual;
        function GetSupported(ASQLKind: TOCISQLKind): Boolean; override;
        procedure SetUpdateMode(const Value: TUpdateMode); override;
        procedure InternalApply(ASQLKind: TOCISQLKind); override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DisconnectAll; override;
        procedure ExecSQL(ASQLKind: TOCISQLKind);
        procedure SetParams(ASQLKind: TOCISQLKind);
        procedure RefreshData(ASQLKind: TOCISQLKind);
        procedure Disconnect(ASQLKind: TOCISQLKind);
        property Query[ASQLKind: TOCISQLKind]: TOCIQuery read GetQuery;
        property SQL[ASQLKind: TOCISQLKind]: TStrings read GetSQL write SetSQL;
        property SQLParams[ASQLKind: TOCISQLKind]: TOciParams read GetSQLParams
            write SetSQLParams;
        property UserDefinedKinds: TOCISQLKinds read FUserDefinedKinds;
        property ActualTableName: String read GetActualTableName;
    published
        property UpdateChangedFields: Boolean read FUpdateChangedFields
            write FUpdateChangedFields default True;
        property AutoBuildKinds: TOCISQLKinds read FAutoBuildKinds write FAutoBuildKinds
            default [skLock, skUnLock, skUpdate, skInsert, skDelete, skRefresh];
        property LockMode: TOCILockMode read FLockMode write SetLockMode default lmOptimistic;
        property LockPoint: TOCILockPoint read FLockPoint write SetLockPoint default lpDeferred;
        property RefreshMode: TOCIRefreshMode read FRefreshMode write FRefreshMode default rmReturning;
        property TableName: String read FTableName write SetTableName;
        property SQLLock: TStrings index 0 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property SQLUnLock: TStrings index 1 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property SQLUpdate: TStrings index 2 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property SQLInsert: TStrings index 3 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property SQLDelete: TStrings index 4 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property SQLRefresh: TStrings index 5 read GetSQLIndex write SetSQLIndex stored IsSQLStored;
        property OnSQLGenerate: TOCIUpdateSQLEvent read FOnSQLGenerate write FOnSQLGenerate;
        property OnApply: TOCIUpdateSQLEvent read FOnApply write FOnApply;
        property OnDisconnect: TOCIUpdateSQLEvent read FOnDisconnect write FOnDisconnect;
        property UpdateMode default upWhereKeyOnly;
    end;

implementation

Uses NCOciUtil, NCOciMsg
{$IFDEF OCI_D6}
     , Variants
{$ENDIF}
     ;

const
    SSQLKinds: array[TOCISQLKind] of String =
        ('Lock', 'Unlock', 'Update', 'Insert', 'Delete', 'Refresh');

constructor TOCIUpdateSQL.Create(AOwner: TComponent);
var
    SQLKind: TOCISQLKind;
begin
    inherited Create(AOwner);
    FUpdateChangedFields := True;
    FAutoBuildKinds := [skLock, skUnLock, skUpdate, skInsert, skDelete, skRefresh];
    FLockMode := lmOptimistic;
    FLockPoint := lpDeferred;
    inherited SetUpdateMode(upWhereKeyOnly);
    FRefreshMode := rmReturning;
    for SQLKind := Low(TOCISQLKind) to High(TOCISQLKind) do begin
        FSQLText[SQLKind] := TStringList.Create;
        TStringList(FSQLText[SQLKind]).OnChange := SQLChanged;
        FSQLParams[SQLKind] := TOciParams.Create(nil);
    end;
end;

destructor TOCIUpdateSQL.Destroy;
var
    SQLKind: TOCISQLKind;
begin
    for SQLKind := Low(TOCISQLKind) to High(TOCISQLKind) do begin
        FSQLText[SQLKind].Free;
        FSQLParams[SQLKind].Free;
    end;
    inherited Destroy;
end;

function TOCIUpdateSQL.IsSQLStored(Index: Integer): Boolean;
begin
    Result := TOCISQLKind(Index) in FUserDefinedKinds;
end;

function TOCIUpdateSQL.GetQuery(ASQLKind: TOCISQLKind): TOCIQuery;
begin
    if (FQueries[ASQLKind] = nil) and (DataSet <> nil) then begin
        FQueries[ASQLKind] := TOCIQuery.Create(Self);
        with FQueries[ASQLKind] do begin
            Disconnectable := True;
            DatabaseName := DataSet.DatabaseName;
            TransactionManager := DataSet.TransactionManager;
            FetchParams := DataSet.FetchParams;
            with FetchParams do begin
                RowsetSize := 1;
                FetchAll := True;
            end;
            DataFormat := DataSet.DataFormat;
            SQL.BeginUpdate;
            try
                SQL.Assign(Self.SQL[ASQLKind]);
                if DataSet is TOCICustomQuery then
                    Macros.AssignValues(TOCIQuery(DataSet).Macros);
            finally
                SQL.EndUpdate;
            end;
            Params.AssignValues(Self.SQLParams[ASQLKind]);
            if DataSet is TOCICustomQuery then
                Params.AssignValues(TOCICustomQuery(DataSet).Params);
        end;
    end;
    Result := FQueries[ASQLKind];
    if (Result.SQL.Count > 0) and not Result.Prepared and
       not (ASQLKind in FUserDefinedKinds) then
        Result.Prepare;
end;

function TOCIUpdateSQL.GetSQL(ASQLKind: TOCISQLKind): TStrings;
var
    canGenerate, genDef, workExist: Boolean;
begin
    if ([csDesigning, csReading] * ComponentState = []) and
       (FSQLText[ASQLKind].Count = 0) and
       (ASQLKind in AutoBuildKinds) then begin
        FSelfUpdating := True;
        try
            FSQLText[ASQLKind].Clear;
            FSQLParams[ASQLKind].Clear;
            genDef := True;
            workExist := True;
            if Assigned(OnSQLGenerate) then
                OnSQLGenerate(Self, ASQLKind, genDef);
            if genDef then begin
                canGenerate := False;
                workExist := False;
                if DataSet is TOCICustomQuery then
                    with TOCICustomQuery(DataSet) do begin
                        if not Prepared then
                            Prepare;
                        canGenerate := (StatementType = stSelect);
                    end;
                if canGenerate then
                    case ASQLKind of
                        skLock: workExist := GenerateLock;
                        skUpdate: workExist := GenerateUpdate;
                        skInsert: workExist := GenerateInsert;
                        skDelete: workExist := GenerateDelete;
                        skRefresh: workExist := GenerateRefresh;
                    end;
            end;
            if (FSQLText[ASQLKind].Count = 0) and workExist then
                OCIDBErrorFmt(msgOCICantGenQuery, [SSQLKinds[ASQLKind]], nil);
        finally
            FSelfUpdating := False;
        end;
    end;
    Result := FSQLText[ASQLKind];
end;

procedure TOCIUpdateSQL.SetSQL(ASQLKind: TOCISQLKind; Value: TStrings);
begin
    FSQLText[ASQLKind].Assign(Value);
end;

function TOCIUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
    Result := GetSQL(TOCISQLKind(Index));
end;

procedure TOCIUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
    SetSQL(TOCISQLKind(Index), Value);
end;

function TOCIUpdateSQL.GetSQLParams(ASQLKind: TOCISQLKind): TOciParams;
begin
    Result := FSQLParams[ASQLKind];
end;

procedure TOCIUpdateSQL.SetSQLParams(ASQLKind: TOCISQLKind;
  const Value: TOciParams);
begin
    FSQLParams[ASQLKind].Assign(Value);
end;

procedure TOCIUpdateSQL.SQLChanged(Sender: TObject);
var
    SQLKind: TOCISQLKind;
begin
    for SQLKind := Low(TOCISQLKind) to High(TOCISQLKind) do
        if Sender = FSQLText[SQLKind] then begin
            if not FSelfUpdating then begin
                Include(FUserDefinedKinds, SQLKind);
                Disconnect(SQLKind);
            end;
            Break;
        end;
end;

function TOCIUpdateSQL.GetActualTableName: String;
begin
    if FTableName <> '' then
        Result := FTableName
    else if DataSet <> nil then
        Result := DataSet.TableName
    else
        Result := '';
end;

procedure TOCIUpdateSQL.SetTableName(const Value: String);
begin
    CheckNoUpdatesPending;
    if ActualTableName <> Value then begin
        FTableName := Value;
        DisconnectAll;
    end;
end;

procedure TOCIUpdateSQL.SetUpdateMode(const Value: TUpdateMode);
begin
    CheckNoUpdatesPending;
    if UpdateMode <> Value then begin
        inherited SetUpdateMode(Value);
        DisconnectAll;
    end;
end;

procedure TOCIUpdateSQL.SetLockMode(const Value: TOCILockMode);
begin
    CheckNoUpdatesPending;
    if LockMode <> Value then begin
        FLockMode := Value;
        Disconnect(skLock);
        Disconnect(skUnLock);
    end;
end;

procedure TOCIUpdateSQL.SetLockPoint(const Value: TOCILockPoint);
begin
    CheckNoUpdatesPending;
    if LockPoint <> Value then begin
        FLockPoint := Value;
        Disconnect(skLock);
        Disconnect(skUnLock);
    end;
end;

function TOCIUpdateSQL.GetSupported(ASQLKind: TOCISQLKind): Boolean;
begin
    if Assigned(OnSQLGenerate) then
        Result := (ASQLKind in (UserDefinedKinds + AutoBuildKinds))
    else if DataSet is TOCICustomQuery then
        with TOCICustomQuery(DataSet) do begin
            if not Prepared then
                Prepare;
            Result := (StatementType = stSelect) and
                (ASQLKind in (UserDefinedKinds + AutoBuildKinds));
        end
    else
        Result := (ASQLKind in UserDefinedKinds);
    Result := Result and not (
       (DataSet = nil) or
       (DataSet.Database = nil) or
       (ASQLKind = skUnLock) and
            ((LockMode = lmNone) or (LockMode <> lmNone) and DataSet.CachedUpdates) or
       (ASQLKind = skLock) and (soImmediate in ApplyOptions) and
            ((LockMode = lmNone) or (LockPoint = lpDeferred)) or
       (ASQLKind = skLock) and not (soImmediate in ApplyOptions) and
            ((LockMode = lmNone) or (LockPoint = lpImmediate) or
             (LockMode = lmOptimistic) and (LockPoint = lpDeferred))
    );
end;

function TOCIUpdateSQL.FieldChanged(AField: TField): Boolean;
begin
    Result := not VarNearEqual(AField.NewValue, AField.OldValue);
end;

function TOCIUpdateSQL.FieldStorable(AField: TField): Boolean;
begin
    Result := (AField.FieldKind = fkData) and (AField.Origin <> '');
end;

function TOCIUpdateSQL.FieldUpdatable(AField: TField): Boolean;
begin
    Result := FieldStorable(AField)
{$IFDEF OCI_D4}
        and (pfInUpdate in AField.ProviderFlags)
{$ENDIF}
    ;
end;

function TOCIUpdateSQL.FieldLocatable(AField: TField): Boolean;
begin
    Result := FieldStorable(AField) and
        not (DataSet.ODataType[AField.FieldNo] in (otVBlobs + otHTypes)) and
{$IFDEF OCI_D4}
        (AField.ProviderFlags * [pfInKey, pfInWhere] <> [])
{$ELSE}
        IsROWID(AField)
{$ENDIF}
    ;
end;

function TOCIUpdateSQL.FieldReqRefresh(AField: TField): Boolean;
begin
{$IFDEF OCI_D5}
    Result := ((AField.AutoGenerateValue = arAutoInc) or
               (AField.AutoGenerateValue = arDefault) and AField.IsNull) and
              UseSmartRefresh;
{$ELSE}
    Result := False;
{$ENDIF}
end;

function TOCIUpdateSQL.UseSmartRefresh: Boolean;
begin
    Result := (RefreshMode = rmReturning) and (DataSet.Database <> nil) and
              (DataSet.Database.ServerVersionNo >= cvOracle80000);
end;

function TOCIUpdateSQL.IsOraBlob(AField: TField): Boolean;
begin
    Result := (DataSet.ODataType[AField.FieldNo] in otHTypes);
end;

function TOCIUpdateSQL.IsROWID(AField: TField): Boolean;
begin
    Result := (DataSet.ODataType[AField.FieldNo] in
        [otROWID]);
end;

function TOCIUpdateSQL.FieldInKey(AField: TField): Boolean;
begin
{$IFDEF OCI_D4}
    Result := (pfInKey in AField.ProviderFlags);
{$ELSE}
    Result := IsROWID(AField);
{$ENDIF}
end;

function TOCIUpdateSQL.BuildFieldParam(AField: TField; ASQLKind: TOCISQLKind;
    AParType: TOCIVarType; ANewValue, AQuote: Boolean): String;
const
    valAge: array[Boolean] of String = ('OLD_', 'NEW_');
var
    par: TOCIParam;
    oParName, S: String;
begin
    oParName := valAge[ANewValue] + AField.FieldName;
    if Length(oParName) > 30 then begin
        S := IntToStr(AField.Index);
        oParName := Copy(oParName, 1, 30 - Length(S) - 2) + '_$' + S;
    end;
    if AQuote then
        Result := ':"' + oParName + '"'
    else
        Result := ':' + oParName;
    oParName := ':' + oParName;
    par := FSQLParams[ASQLKind].FindParam(oParName);
    if par <> nil then begin
        if par.OParamType <> AParType then
            par.OParamType := odInOut;
    end
    else
        FSQLParams[ASQLKind].CreateOParam(DataSet.ODataType[AField.FieldNo],
            DataSet.ODataSize[AField.FieldNo], oParName, AParType, False, AQuote);
end;

function TOCIUpdateSQL.BuildFieldSQL(AField: TField): String;
begin
    Result := '"' + AField.Origin + '"';
end;

procedure TOCIUpdateSQL.BuildWherePhrase(ASQLKind: TOCISQLKind);
var
    s: String;
    i: Integer;
begin
    s := '';
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldLocatable(DataSet.Fields[i]) and
           ((UpdateMode = upWhereAll) or
            (UpdateMode = upWhereChanged) and
                (FieldChanged(DataSet.Fields[i]) or FieldInKey(DataSet.Fields[i]))or
            (UpdateMode = upWhereKeyOnly) and FieldInKey(DataSet.Fields[i])) then begin
            if s <> '' then
                s := s + ' and ';
            if VarIsNull(DataSet.Fields[i].OldValue) then
                s := s + BuildFieldSQL(DataSet.Fields[i]) + ' is null'
            else
                s := s + BuildFieldSQL(DataSet.Fields[i]) + ' = ' +
                         BuildFieldParam(DataSet.Fields[i], ASQLKind, odIn, False,
                                         ASQLKind <> skRefresh); // see comment in GenerateRefresh
        end;
    if s = '' then
        OCIDBError(msgOCIKeyFieldsEmpty, Self);
    FSQLText[ASQLKind].Add('where ' + s);
end;

procedure TOCIUpdateSQL.BuildReturningPhrase(ASQLKind: TOCISQLKind);
var
    s1, s2: String;
    i: Integer;
begin
    s1 := '';
    s2 := '';
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldStorable(DataSet.Fields[i]) and
           (FieldReqRefresh(DataSet.Fields[i]) or
            (ASQLKind = skInsert) and (IsOraBlob(DataSet.Fields[i]) or IsROWID(DataSet.Fields[i]))) then begin
            if s1 <> '' then begin
                s1 := s1 + ', ';
                s2 := s2 + ', ';
            end;
            s1 := s1 + BuildFieldSQL(DataSet.Fields[i]);
            s2 := s2 + BuildFieldParam(DataSet.Fields[i], ASQLKind, odRet, True, True);
        end;
    if s1 <> '' then
        FSQLText[ASQLKind].Add('returning ' + s1 + ' into ' + s2);
end;

function TOCIUpdateSQL.GenerateLock: Boolean;
var
    i: Integer;
    s: String;
begin
    s := '';
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldLocatable(DataSet.Fields[i]) or
           (LockMode = lmPessimistic) and IsOraBlob(DataSet.Fields[i]) then begin
            if s <> '' then
                s := s + ', ';
            s := s + BuildFieldSQL(DataSet.Fields[i]);
        end;
    if s = '' then
        s := '*';
    FSQLText[skLock].Add('select ' + s + ' from ' + ActualTableName);
    BuildWherePhrase(skLock);
    if LockMode = lmPessimistic then
        FSQLText[skLock].Add('for update nowait');
    Result := True;
end;

function TOCIUpdateSQL.GenerateUpdate: Boolean;
var
    i: Integer;
    s: String;
    fldChng: Boolean;
begin
    s := '';
    Result := False;
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldUpdatable(DataSet.Fields[i]) and not IsOraBlob(DataSet.Fields[i]) then begin
            fldChng := not UpdateChangedFields or FieldChanged(DataSet.Fields[i]);
            Result := Result or fldChng;
            if fldChng then begin
                if s <> '' then
                    s := s + ', ';
                s := s + BuildFieldSQL(DataSet.Fields[i]) + ' = ' +
                         BuildFieldParam(DataSet.Fields[i], skUpdate, odIn, True, True);
            end;
        end;
    if s <> '' then begin
        FSQLText[skUpdate].Add('update ' + ActualTableName);
        FSQLText[skUpdate].Add('set ' + s);
        BuildWherePhrase(skUpdate);
        if UseSmartRefresh then
            BuildReturningPhrase(skUpdate);
    end;
end;

function TOCIUpdateSQL.GenerateInsert: Boolean;
var
    i: Integer;
    s1, s2, s3: String;
    fldChng: Boolean;
begin
    s1 := '';
    s2 := '';
    Result := False;
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldUpdatable(DataSet.Fields[i]) then begin
            fldChng := IsOraBlob(DataSet.Fields[i]) or not UpdateChangedFields or
                not DataSet.Fields[i].IsNull;
            Result := Result or fldChng;
            if fldChng then begin
                if (s3 = '') and not DataSet.Fields[i].Required then
                    s3 := BuildFieldSQL(DataSet.Fields[i]);
                if s1 <> '' then begin
                    s1 := s1 + ', ';
                    s2 := s2 + ', ';
                end;
                s1 := s1 + BuildFieldSQL(DataSet.Fields[i]);
                case DataSet.ODataType[DataSet.Fields[i].FieldNo] of
                    otBLOB, otBFILE: s2 := s2 + 'EMPTY_BLOB()';
                    otCLOB, otCFILE: s2 := s2 + 'EMPTY_CLOB()';
                    else s2 := s2 + BuildFieldParam(DataSet.Fields[i], skInsert, odIn, True, True);
                end;
            end;
        end;
    FSQLText[skInsert].Add('insert into ' + ActualTableName);
    if s1 = '' then begin
        s1 := s3;
        s2 := 'NULL';
    end;
    if s1 <> '' then begin
        FSQLText[skInsert].Add('(' + s1 + ')');
        FSQLText[skInsert].Add('values (' + s2 + ')');
        if UseSmartRefresh then
            BuildReturningPhrase(skInsert);
    end;
end;

function TOCIUpdateSQL.GenerateDelete: Boolean;
begin
    FSQLText[skDelete].Add('delete from ' + ActualTableName);
    BuildWherePhrase(skDelete);
    Result := True;
end;

function TOCIUpdateSQL.GenerateRefresh: Boolean;
var
    s1, s2: String;
    i: Integer;
begin
    s1 := '';
    s2 := '';
    for i := 0 to DataSet.FieldCount - 1 do
        if FieldStorable(DataSet.Fields[i]) and
           (not (soBlobsOnly in ApplyOptions) or IsOraBlob(DataSet.Fields[i])) then begin
            if s1 <> '' then begin
                s1 := s1 + ', ';
                s2 := s2 + ', ';
            end;
            s1 := s1 + BuildFieldSQL(DataSet.Fields[i]);
            // because it is PL/SQL block, i can't use quoted params, that is
            // like :"PARAM". Reason - bug in PL/SQL.
            s2 := s2 + BuildFieldParam(DataSet.Fields[i], skRefresh, odInOut, True, False);
        end;
    if s1 = '' then begin
        FSQLText[skRefresh].Add('select * from ' + ActualTableName);
        BuildWherePhrase(skRefresh);
    end
    else begin
        FSQLText[skRefresh].Add('begin');
        FSQLText[skRefresh].Add('select ' + s1 + ' into ' + s2);
        FSQLText[skRefresh].Add('from ' + ActualTableName);
        BuildWherePhrase(skRefresh);
        FSQLText[skRefresh].Add(';');
        FSQLText[skRefresh].Add('end;');
    end;
    Result := True;
end;

procedure TOCIUpdateSQL.ExecSQL(ASQLKind: TOCISQLKind);
begin
    if DataSet <> nil then
        with Query[ASQLKind] do
        try
            if SQL.Count = 0 then
                Exit;
            if StatementType = stSelect then begin
                Close;
                Open;
            end
            else
                ExecSQL;
            if (StatementType in [stUpdate, stDelete, stInsert]) and (RowsAffected <> 1) or
               (StatementType in [stSelect]) and (RecordCount <> 1) then
                OCIDBError(msgOCIRecordDeleted, Self);
            if (ASQLKind = skLock) and (StatementType = stSelect) and
               not DataSet.RecordsEqual(Query[ASQLKind], dsOldValue, dsCurValue) then begin
                Apply(skUnLock, []);
                OCIDBError(msgOCIRecordChanged, Self);
            end;
        except on E: EOCINativeError do
            // resource busy and acquire with NOWAIT specified
            if (E.Errors[0].ErrorCode = 54) then
                OCIDBError(msgOCIRecordLocked, Self)
            else
                raise;
        end;
end;

procedure TOCIUpdateSQL.DisconnectAll;
var
    SQLKind: TOCISQLKind;
begin
    if not (csDestroying in ComponentState) then
        for SQLKind := Low(TOCISQLKind) to High(TOCISQLKind) do
            Disconnect(SQLKind);
end;

procedure TOCIUpdateSQL.Disconnect(ASQLKind: TOCISQLKind);
var
    doDef: Boolean;
begin
    doDef := True;
    if Assigned(OnDisconnect) then
        OnDisconnect(Self, ASQLKind, doDef);
    if doDef then begin
        if not (ASQLKind in FUserDefinedKinds) then begin
            FSelfUpdating := True;
            try
                FSQLText[ASQLKind].Clear;
            finally
                FSelfUpdating := False;
            end;
        end;
        if FQueries[ASQLKind] <> nil then
            try
                FQueries[ASQLKind].Free;
            finally
                FQueries[ASQLKind] := nil;
            end;
    end;
end;

function TOCIUpdateSQL.Par2FieldName(var AName: String): Integer;
var
    i: Integer;
    fld: TField;
begin
    if AnsiCompareText(Copy(AName, 1, 4), 'OLD_') = 0 then begin
        Result := -1;
        System.Delete(AName, 1, 4);
    end
    else if AnsiCompareText(Copy(AName, 1, 4), 'NEW_') = 0 then begin
        Result := 1;
        System.Delete(AName, 1, 4);
    end
    else
        Result := 1;
    i := Pos('_$', AName);
    if i > 0 then
        try
            fld := DataSet.Fields[StrToInt(Copy(AName, i + 2, Length(AName)))];
            if Copy(fld.FieldName, 1, i - 1) = Copy(AName, 1, i - 1) then
                AName := fld.FieldName; 
        except
        end;
end;

procedure TOCIUpdateSQL.SetParams(ASQLKind: TOCISQLKind);
var
    I: Integer;
    Old: Boolean;
    Params: TOCIParams;
    Param: TOCIParam;
    parName: string;
    Field: TField;
    Value: Variant;
begin
    if DataSet = nil then
        Exit;
    Params := Query[ASQLKind].Params;
    for I := 0 to Params.Count - 1 do begin
        Param := Params[I];
        if Param.OParamType in [odUnknown, odIn, odInOut, odRet] then begin
            if Param.OParamType = odUnknown then
                Param.OParamType := odIn;
            parName := Param.Name;
            if AnsiCompareText(parName, SDefPrefix + 'SAVE') = 0 then
                Param.AsBoolean := soSave in ApplyOptions
            else if not (DataSet is TOCICustomQuery) or
                    (TOCICustomQuery(DataSet).FindParam(parName) = nil) then begin
                Old := Par2FieldName(parName) = -1;
                Field := DataSet.FindField(parName);
                if Field <> nil then
                    if (Param.OParamType in [odInOut, odRet]) and
                       (Param.ODataType in otHTypes) then
                        Param.AssignByRef(Field, -1)
                    else if Param.OParamType <> odRet then
                        if Old then
                            Param.AssignFieldValue(Field, Field.OldValue, -1)
                        else begin
                            Value := Field.NewValue;
                            if VarIsEmpty(Value) then
                                Value := Field.OldValue;
                            Param.AssignFieldValue(Field, Value, -1);
                        end;
            end;
        end;
    end;
end;

type
    __TOCIDataSet = class(TOCIDataSet);

procedure TOCIUpdateSQL.RefreshData(ASQLKind: TOCISQLKind);
var
    prevState: TDataSetState;
    i: Integer;
    new: Boolean;
    param: TOCIParam;
    parName: string;
    field: TField;
begin
    if DataSet <> nil then
        with Query[ASQLKind] do begin
            if SQL.Count = 0 then
                Exit;
            prevState := __TOCIDataSet(DataSet).SetTempState(dsNewValue);
            try
                if StatementType = stSelect then
                    for i := 0 to FieldCount - 1 do begin
                        field := DataSet.FindField(Fields[i].FieldName);
                        if (field <> nil) and not (ODataType[field.FieldNo] in otHTypes) then
                            field.Assign(Fields[i]);
                    end
                else if StatementType in [stDeclare, stBegin, stInsert, stDelete, stUpdate] then
                    for i := 0 to Params.Count - 1 do begin
                        param := Params[I];
                        parName := param.Name;
                        new := Par2FieldName(parName) = 1;
                        if (param.OParamType in [odOut, odInOut, odRet]) and
                           not (param.ODataType in otHTypes) and
                           new then begin
                            field := DataSet.FindField(parName);
                            if field <> nil then
                                field.Assign(param);
                        end;
                    end
                else
                    OCIDBError(msgOCICantUseSQL4Refresh, Self);
            finally
                __TOCIDataSet(DataSet).RestoreState(prevState);
            end;
        end;
end;

procedure TOCIUpdateSQL.InternalApply(ASQLKind: TOCISQLKind);
var
    appDef: Boolean;
begin
    appDef := True;
    if Assigned(OnApply) then
        OnApply(Self, ASQLKind, appDef);
    if not appDef then
        Exit;
    if (ASQLKind = skLock) and (LockMode = lmPessimistic) and
       (not DataSet.CachedUpdates or not DataSet.Database.InTransaction) then
        DataSet.Database.StartTransaction;
    if not (
        ((ASQLKind = skUnLock) and not (skUnLock in UserDefinedKinds)) or
        ((ASQLKind = skLock) and (soNoRecord in ApplyOptions) and
            not (skLock in UserDefinedKinds))
       ) then begin
        if UpdateChangedFields and (ASQLKind in [skUpdate, skInsert]) or
           (UpdateMode = upWhereChanged) and (ASQLKind = skUpdate) or
           (UpdateMode <> upWhereKeyOnly) and
                (ASQLKind in [skLock, skUpdate, skDelete, skRefresh]) then
            Disconnect(ASQLKind);
        if (ASQLKind = skRefresh) and DataSet.HaveBLOBs then
            DataSet.ResetLOBs;
        GetSQL(ASQLKind);
        GetQuery(ASQLKind);
        SetParams(ASQLKind);
        ExecSQL(ASQLKind);
        if (ASQLKind in [skUpdate, skInsert]) and ((RefreshMode = rmSelect) or
            (RefreshMode = rmReturning) and (DataSet.Database.ServerVersionNo < cvOracle80000)) then
            Apply(skRefresh, [])
        else if not (ASQLKind in [skDelete, skLock, skUnlock]) then
            RefreshData(ASQLKind);
    end;
    if (ASQLKind = skUnLock) and (LockMode = lmPessimistic) and
       not DataSet.CachedUpdates then begin
        if soSave in ApplyOptions then
            DataSet.Database.Commit
        else
            DataSet.Database.Rollback;
        // after LOB modification & end transaction
        // LOB descriptors become invalid
        if DataSet.HaveBLOBs and not (soNoRecord in ApplyOptions) then
            Apply(skRefresh, ApplyOptions + [soBlobsOnly]);
    end;
    // i do that here, because if i will do that in
    // DestroyOCICursor, AV is raised.
    if DataSet.HaveBLOBs then
        Disconnect(ASQLKind);
end;

end.
