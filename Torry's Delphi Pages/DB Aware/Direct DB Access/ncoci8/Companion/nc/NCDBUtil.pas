{*******************************************************}
{File:      NCDBUtil.PAS                                }
{Revision:  3.00 / 06.02.2000                           }
{Comment:   DB utilities                                }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCDBUtil;

interface

Uses Classes, DB
{$IFDEF OCI_D6}
    , Variants
{$ENDIF}
    ;

function DUAliases2SQL(AAliases: TStrings): String;
procedure DUSQL2Aliases(const AValue: String; AAliases: TStrings);
procedure DUContWhere(ASQL: TStrings; var wasWhere: Boolean; const S: String);
function DUFields2SQL(const AFields: String; const tblAlias: String;
    colAlias: Boolean; AAliases: TStrings): String;
function DUParamJoin2SQL(const AFields, ATblAlias: String; AAliases: TStrings;
    const AValue: Variant; ANoCase, APartialMatch: Boolean;
    const AFieldsPrefix: String; ASubstValues: Boolean): String;

function DUFieldName(const s: String; ind: Integer): String;
function DUFieldNo(const s, fldName: String): Integer;
function DUCountFields(const s: String): Integer;
function DUFieldIn(const s, fldName: String): Boolean;
function DUFieldsIn(const sWhere, sWhat: String; AAll: Boolean): Boolean;
function DUJoinFields(const s1, s2: String): String;
function DURemoveFields(const sFrom, sWhat: String): String;

{
procedure DUSQLExec(const ADb, ASQL: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []);
procedure DUProcExec(const ADb, AProc: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []);
function DUSQLOpen(const ADb, ASQL: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []): TOCIQuery;
function DUSQLEval(const ADb, AExpr: String;
    AOptions: TNCSQLOptions = []): Variant;
function DUProcEval(const ADb, AProc: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []): Variant;
function DUSQLLookup(const ADb, ATable, AKeyFields, AResultFields: String;
    AKeyValue: array of Variant; AOptions: TNCSQLOptions = []): Variant;
procedure DURefreshSQLCache;
}

implementation

Uses SysUtils, NCUtil;

function DUAliases2SQL(AAliases: TStrings): String;
var
    i: Integer;
    s: String;
begin
    Result := '';
    for i := 0 to AAliases.Count - 1 do begin
        if Result <> '' then
            Result := Result + ', ';
        s := AAliases.Names[i];
        Result := Result + AAliases.Values[s] + ' ' + s;
    end;
end;

procedure DUSQL2Aliases(const AValue: String; AAliases: TStrings);
var
    fromI, i, ParenthLvl: Integer;
    InString: Boolean;
    s, s1, s2: String;
begin
    InString := False;
    ParenthLvl := 0;
    fromI := 1;
    for i := 1 to Length(AValue) do
        if (AValue[i] in ['(', '[']) then
            Inc(ParenthLvl)
        else if (AValue[i] in [')', ']']) then
            Dec(ParenthLvl)
        else if (AValue[i] in ['''', '"']) then
            InString := not InString
        else if (ParenthLvl = 0) and not InString and (AValue[i] = ',') then begin
            s := Copy(AValue, fromI, i - fromI);
            fromI := 1;
            s1 := StrToken(s, [' '], fromI);
            s2 := StrToken(s, [' '], fromI);
            if s2 = '' then
                s2 := s1;
            AAliases.Add(s1 + '=' + s);
            fromI := i + 1;
        end;
end;

procedure DUContWhere(ASQL: TStrings; var wasWhere: Boolean; const S: String);
var
    S1: String;
begin
    S1 := S;
    if S1 <> '' then
        S1 := ' (' + S1 + ')';
    if not wasWhere then begin
        ASQL.Add('where' + S1);
        wasWhere := True;
    end
    else
        ASQL.Add('and' + S1);
end;

function DUFields2SQL(const AFields: String; const tblAlias: String;
    colAlias: Boolean; AAliases: TStrings): String;
var
    i: Integer;
    s, s1, s2: String;
begin
    Result := '';
    i := 1;
    while i <= Length(AFields) do begin
        s := ExtractFieldName(AFields, i);
        s1 := s;
        s2 := '';
        if AAliases <> nil then begin
            s2 := AAliases.Values[s1];
            if s2 <> '' then
                s1 := s2;
        end;
        if (s2 = '') and (tblAlias <> '') then
            if tblAlias <> ':' then
                s1 := tblAlias + '.' + s1
            else
                s1 := ':' + s1;
        if colAlias and (CompareText(s1, s) <> 0) then
            s1 := s1 + ' ' + s;
        if Result <> '' then
            Result := Result + ',';
        Result := Result + s1;
    end;
end;

function DUParamJoin2SQL(const AFields, ATblAlias: String; AAliases: TStrings;
    const AValue: Variant; ANoCase, APartialMatch: Boolean;
    const AFieldsPrefix: String; ASubstValues: Boolean): String;
var
    i, j, tp: Integer;
    s, s1: String;
begin
    Result := '';
    i := 1;
    j := 0;
    while i <= Length(AFields) do begin
        tp := VarExtType(AValue, j);
        if tp = varEmpty then
            Break;
        s := ExtractFieldName(AFields, i);
        s1 := DUFields2SQL(s, ATblAlias, False, AAliases);
        if tp = varNull then
            s1 := s1 + ' is null'
        else begin
            if ANoCase and ((tp = varOleStr) or (tp = varString)) then
                s1 := 'upper(' + s1 + ')';
            if APartialMatch then
                s1 := s1 + ' like '
            else
                s1 := s1 + ' = ';
            if not ASubstValues then
                s1 := s1 + ':' + AFieldsPrefix + s
            else begin
                s := Var2Text(VarExtValue(AValue, j, Null));
                if APartialMatch then
                    s := s + '%';
                if ANoCase and ((tp = varOleStr) or (tp = varString)) then
                    s := AnsiUpperCase(s);
                if (tp = varDate) or (tp = varOleStr) or (tp = varString) then
                    s1 := s1 + '''' + s + ''''
                else
                    s1 := s1 + s;
            end;
        end;
        if Result <> '' then
            Result := Result + ') and (';
        Result := Result + s1;
        Inc(j);
    end;
    Result := '(' + Result + ')';
end;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

function DUFieldName(const s: String; ind: Integer): String;
var
    iFrom, iTo, n: Integer;
begin
    Result := '';
    iFrom := 1;
    Dec(Ind);
    n := Length(s);
    while (iFrom <= n) and (Ind > 0) do begin
        if s[iFrom] = ';' then
            Dec(Ind);
        Inc(iFrom);
    end;
    if Ind = 0 then begin
        iTo := iFrom;
        while (iTo <= n) and (s[iTo] <> ';') do
            Inc(iTo);
        Result := Copy(s, iFrom, iTo - iFrom);
    end;
end;

function DUFieldNo(const s, fldName: String): Integer;
var
    n, i: Integer;
begin
    i := 1;
    Result := 0;
    n := Length(s);
    while (i <= n) and not (ExtractFieldName(s, i) = fldName) do
        Inc(Result);
    if i > n then
        Result := -1; 
end;

function DUCountFields(const s: String): Integer;
var
    n, i: integer;
begin
    if s = '' then
        Result := 0
    else begin
        Result := 1;
        n := Length(s);
        for i := 1 to n do
            if s[i] = ';' then
                inc(Result);
    end;
end;

function DUFieldIn(const s, fldName : string) : Boolean;
var
    i, fldNameLn, sLn: Integer;
    fldNameUp, sUp: String;
begin
    i := 0;
    fldNameUp := AnsiUpperCase(fldName);
    fldNameLn := Length(fldNameUp);
    sUp := AnsiUpperCase(s);
    sLn := Length(sUp);
    repeat
        i := iPos(fldNameUp, sUp, i + 1);
        Result := (i <> 0) and ((i = 1) or (sUp[i - 1] = ';')) and
            ((i + fldNameLn > sLn) or (sUp[i + fldNameLn] = ';'));
    until Result or (i = 0);
end;

function DUFieldsIn(const sWhere, sWhat: String; AAll: Boolean): Boolean;
var
    i: Integer;
    s: String;
    isIn: Boolean;
begin
    i := 1;
    Result := False;
    while i <= Length(sWhat) do begin
        s := ExtractFieldName(sWhat, i);
        isIn := DUFieldIn(sWhere, s);
        if AAll and not isIn then begin
            Result := False;
            Break;
        end;
        Result := Result or isIn;
    end;
end;

function DUJoinFields(const s1, s2: String): String;
var
    i: Integer;
    s: String;
begin
    Result := s1;
    i := 1;
    while i <= Length(s2) do begin
        s := ExtractFieldName(s2, i);
        if not DUFieldIn(s1, s) then begin
            if (Result <> '') and (Result[Length(Result)] <> ';') then
                Result := Result + ';';
            Result := Result + s;
        end;
    end;
end;

function DURemoveFields(const sFrom, sWhat: String): String;
var
    i: Integer;
    s: String;
begin
    Result := '';
    i := 1;
    while i <= Length(sFrom) do begin
        s := ExtractFieldName(sFrom, i);
        if not DUFieldIn(sWhat, s) then begin
            if Result <> '' then
                Result := Result + ';';
            Result := Result + s;
        end;
    end;
end;

{ ----------------------------------------------------------------------------- }
{ ----------------------------------------------------------------------------- }
{
type
    TNCCacheOptions = set of (coForFetchAll, coForFetchRow, coForExec, coSkipNull);
    TNCActivateMode = (amNotFound, amNull, amFound);

    TNCValuePara = record
        FKeyValue, FResultValue: Variant;
    end;
    PNCValuePara = ^TNCValuePara;

    TNCSQLCacheItem = class(TNCValuesCache)
    private
        FObject: TOCICustomQuery;
        FLocked: Integer;
        FUsed: Integer;
    public
        constructor Create(AObject: TOCICustomQuery);
        destructor Destroy; override;
    end;

    TDBDataSetClass = class of TDBDataSet;
    TNCSQLCache = class(TNCStringList)
    private
        FCacheSize: Integer;
        FSQLType: TOCICustomQueryClass;
    public
        constructor Create(ACacheSize: Integer; ASQLType: TOCICustomQueryClass);
        procedure GetSQLObject(const ADb, ASQL: String; ASOptions: TNCSQLOptions;
            ACOptions: TNCCacheOptions; const AParams: Variant; const AOutFields: String;
            var AOutValues: Variant; var AOutDataSet: TOCICustomQuery);
        procedure UnLockSQLObject(var AObject: TOCICustomQuery);
    end;

const
    DUSOCaches = [soCache1, soCache10, soCache100, soCache1000];

var
    DUSPCache: TNCSQLCache;
    DUQCache: TNCSQLCache;

// ------------------------------------------------

constructor TNCValuesCache.Create;
begin
    inherited Create;
    FValueList := TList.Create;
    FOverFlowPtr := 0;
end;

destructor TNCValuesCache.Destroy;
begin
    ClearValues;
    FValueList.Free;
    inherited Destroy;
end;

procedure TNCValuesCache.ClearValues;
var
    i: Integer;
begin
    if FValueList.Count = 0 then
        Exit;
    for i := 0 to FValueList.Count - 1 do
        dispose(PNCValuePara(FValueList[i]));
    FValueList.Clear;
    FOverFlowPtr := 0;
end;

function TNCValuesCache.LookupValue(const AKey: Variant; var AResult: Variant): Boolean;
var
    n, i: Integer;
begin
    n := FValueList.Count - 1;
    Result := False;
    AResult := Unassigned;
    for i := 0 to n do
        with PNCValuePara(FValueList[i])^ do
            if VarCmp(FKeyValue, AKey, False, False) then begin
                AResult := FResultValue;
                Result := True;
                Exit;
            end;
end;

procedure TNCValuesCache.AddValue(const AKey, AResult: Variant; AValuesSize: Integer);
var
    pPara: PNCValuePara;
begin
    if FValueList.Count >= AValuesSize then begin
        pPara := PNCValuePara(FValueList[FOverFlowPtr]);
        inc(FOverFlowPtr);
        if FOverFlowPtr >= FValueList.Count then
            FOverFlowPtr := 0;
    end
    else begin
        if FValueList.Capacity < AValuesSize then
            FValueList.Capacity := AValuesSize;
        new(pPara);
        FValueList.Add(pPara);
    end;
    pPara^.FKeyValue := AKey;
    pPara^.FResultValue := AResult;
end;

function TNCValuesCache.Count: Integer;
begin
    Result := FValueList.Count;
end;

// ------------------------------------------------

constructor TNCSQLCacheItem.Create(AObject: TOCICustomQuery);
begin
    inherited Create;
    FObject := AObject;
    FLocked := 0;
    FUsed := 0;
end;

destructor TNCSQLCacheItem.Destroy;
begin
    FObject.Free;
    inherited Destroy;
end;

// ------------------------------------------------

constructor TNCSQLCache.Create(ACacheSize: Integer; ASQLType: TOCICustomQueryClass);
begin
    inherited Create;
    ObjectsOwner := True;
    Sorted := True;
    FCacheSize := ACacheSize;
    FSQLType := ASQLType;
end;

procedure TNCSQLCache.GetSQLObject(const ADb, ASQL: String;
    ASOptions: TNCSQLOptions; ACOptions: TNCCacheOptions; const AParams: Variant;
    const AOutFields: String; var AOutValues: Variant; var AOutDataSet: TOCICustomQuery);
var
    nParams: Integer;
    vInParams: Variant;
    item: TNCSQLCacheItem;
    useCache: Boolean;

    // ------------------------------------
    // Find SQL object in cache
    // ------------------------------------
    function PrepareObject: TNCSQLCacheItem;
    var
        minVal, i, j: Integer;
        key: String;
        found: Boolean;
    begin
        key := ASQL + '@' + ADB + '@' + IntToStr(nParams);
        i := -1;
        // 1. Find object in cache
        // Must EQ: - database, sql, param set
        //          - cache parameters
        found := Find(key, i);
        if found then
            Result := TNCSQLCacheItem(Objects[i])
        else
        try
            if FCacheSize > Count then begin
                // ... 2) Else if not cache size limit, then alloc new Result
                Result := TNCSQLCacheItem.Create(FSQLType.Create(nil));
                i := AddObject(key, Result);
            end
            else begin
                // ... 3) Else find MRU Result, and reuse it
                minVal := MAXINT;
                for j := 0 to Count - 1 do begin
                    Result := TNCSQLCacheItem(Objects[j]);
                    if (Result.FUsed < minVal) and (Result.FLocked = 0) then begin
                        minVal := Result.FUsed;
                        i := j;
                    end;
                end;
                if i = -1 then
                    GenErrorResFmt(SCacheOverflow, [ASQL]);
                Result := TNCSQLCacheItem(Objects[i]);
                Result.ClearValues;
                Result.FObject.Disconnect;
            end;
            // 4) Set properties of new Result object
            with Result, Result.FObject do begin
                DatabaseName := ADb;
                ParamBindMode := pbByNumber;
                if FObject is TOCIQuery then
                    TOCIQuery(FObject).SQL.Add(ASQL)
                else if FObject is TOCIStoredProc then
                    TOCIStoredProc(FObject).StoredProcName := ASQL;
                Prepare;
            end;
        except
            if i <> -1 then
                Self.Delete(i);
            raise;
        end;
        // 5) Increment usage
        if soLock in ASOptions then
            Inc(Result.FLocked);
        Inc(Result.FUsed);
        AOutDataSet := Result.FObject;
    end;

    // ----------------------------------------------------
    // Find SQL object previos executions input params and
    // if found return output params/field values.
    // Also set object input params.
    // ----------------------------------------------------
    function SetObjectParams(AItem: TNCSQLCacheItem; var vInParams: Variant): TNCActivateMode;
    var
        i, j, ip: Integer;
        objParams: TOCIParams;
        allUnDef, isN: Boolean;
        tmp, v: Variant;

        function GetObjectId: String;
        begin
            with AItem do
                if FObject is TOCIQuery then
                    Result := TOCIQuery(FObject).Text
                else if FObject is TOCIStoredProc then
                    Result := TOCIStoredProc(FObject).StoredProcName;
        end;

    begin
        // 1) Get object TOCIParams
        objParams := AItem.FObject.Params;
        // 2) if we need lookup in cache then
        //    alloc and fill vInParams (varArray) of input param values
        ip := 0;
        j := -1;
        allUnDef := True;
        for i := 0 to nParams do begin
            // skip all varEmpty param values and
            // skip, if requested, varNull param values
            v := AParams[i];
            if not VarIsEmpty(v) then begin
                isN := VarIsNull(v);
                allUnDef := allUnDef and isN;
                if not ((coSkipNull in ACOptions) and isN) then begin
                    repeat
                        Inc(j);
                        if j >= objParams.Count then
                            raise EDUParamMismatch.CreateResFmt(SQParamMismatch, [GetObjectId]);
                    until objParams[j].ParamType <> ptResult;
                    if useCache then begin
                        if ip = 0 then
                            vInParams := v
                        else begin
                            if ip = 1 then begin
                                tmp := vInParams;
                                vInParams := VarArrayCreate([0, 32], varVariant);
                                vInParams[0] := tmp;
                            end;
                            vInParams[ip] := v;
                        end;
                        Inc(ip);
                    end;
                    objParams[j].Value := v;
                end;
            end;
        end;
        // 3) do lookup
        Result := amNotFound;
        if (soNoNull in ASOptions) and allUnDef and (j <> -1) then
            Result := amNull
        else if useCache then begin
            if ip > 1 then
                VarArrayRedim(vInParams, ip - 1);
            if AItem.LookupValue(vInParams, AOutValues) then
                Result := amFound;
        end;
    end;

    // ----------------------------------------------------
    // Activate object: Open, ExecSQL or ExecProc
    // ----------------------------------------------------
    procedure ActivateObject(AItem: TNCSQLCacheItem; const vInParams: Variant;
        AActivateMode: TNCActivateMode);
    var
        i, n: Integer;
    begin
        with AItem, AItem.FObject do
        try
            if AActivateMode = amFound then begin
                if coForFetchAll in ACOptions then begin
                    if Active then
                        First
                    else
                        Open;
                end;
            end
            else begin
                if coForFetchAll in ACOptions then begin
                    // If 'SELECT ...' and need full cursor
                    AOutValues := True;
                    Close;
                    Open;
                end
                else if coForFetchRow in ACOptions then begin
                    // if 'SELECT ...' and neew first row
                    if AActivateMode = amNull then begin
                        n := DUCountField(AOutFields);
                        AOutValues := VarArrayCreate([0, n - 1], varVariant);
                        for i := 0 to n - 1 do
                            AOutValues[i] := Null;
                    end
                    else begin
                        Open;
                        try
                            if (soNoDataFound in ASOptions) and DUIsEmpty(FObject) then
                                raise EDUNoDataFound.CreateResFmt(SNoDataFound, [ASQL]);
                            if (soToManyRows in ASOptions) then begin
                                Next;
                                if not EOF then
                                    raise EDUToManyRows.CreateResFmt(SToManyRows, [ASQL])
                                else
                                    Prior;
                            end;
                            AOutValues := FObject[AOutFields];
                        finally
                            Close;
                        end;
                    end;
                end
                else if coForExec in ACOptions then begin
                    // if 'INSERT/UPDATE/DELETE/EXEC ....'
                    if AActivateMode = amNull then
                        AOutValues := Null
                    else begin
                        AOutValues := True;
                        if FObject is TOCIQuery then begin
                            TOCIQuery(FObject).ExecSQL;
                            ClearValues;
                        end
                        else if FObject is TOCIStoredProc then
                            with TOCIStoredProc(FObject) do begin
                                ExecProc;
                                if (ParamCount > 0) and (Params[0].ParamType = ptResult) then
                                    AOutValues := Params[0].Value
                                else
                                    ClearValues;
                            end
                    end;
                end;
                if useCache then begin
                    n := 1;
                    if soCache1000 in ASOptions then
                        n := 1000
                    else if soCache100 in ASOptions then
                        n := 100
                    else if soCache10 in ASOptions then
                        n := 10;
                    AddValue(vInParams, AOutValues, n);
                end
                else
                    ClearValues;
            end;
        except
            Close;
            ClearValues;
            raise;
        end;
    end;

begin
    nParams := VarArrayHighBound(AParams, 1);
    useCache := DUSOCaches * ASOptions <> [];
    item := PrepareObject;
    ActivateObject(item, vInParams,
        SetObjectParams(item, vInParams));
end;

procedure TNCSQLCache.UnLockSQLObject(var AObject: TDBDataSet);
var
    i: Integer;
begin
    if AObject <> nil then begin
        for i := 0 to Count - 1 do
            if TNCSQLCacheItem(Objects[i]).FObject = AObject then begin
                Dec(TNCSQLCacheItem(Objects[i]).FLocked);
                Break;
            end;
        AObject := nil;
    end;
end;

// ------------------------------------------------

procedure DUSQLExec(const ADb, ASQL: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []);
var
    ov: Variant;
    ods: TDBDataSet;
begin
    DUQCache.GetSQLObject(ADb, ASQL, AOptions - [soNoDataFound, soToManyRows],
        [coForExec], VarArrayOf(AParams), '', ov, ods);
end;

procedure DUProcExec(const ADb, AProc: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []);
var
    ov: Variant;
    ods: TDBDataSet;
begin
    DUSPCache.GetSQLObject(ADb, AProc, AOptions - [soNoDataFound, soToManyRows],
        [coForExec], VarArrayOf(AParams), '', ov, ods);
end;

function DUSQLOpen(const ADb, ASQL: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []): TOCIQuery;
var
    ov: Variant;
    ds: TDBDataSet;
begin
    DUQCache.GetSQLObject(ADb, ASQL, AOptions - [soNoDataFound, soToManyRows],
        [coForFetchAll], VarArrayOf(AParams), '', ov, ds);
    Result := ds as TOCIQuery;
end;

function DUSQLEval(const ADb, AExpr: String; AOptions: TNCSQLOptions = []): Variant;
var
    ods: TDBDataSet;
begin
    DUQCache.GetSQLObject(ADb, 'select ' + AExpr + ' as Result from dual', AOptions,
        [coForFetchRow], VarArrayOf([Unassigned]), 'RESULT', Result, ods);
end;

function DUProcEval(const ADb, AProc: String; AParams: array of Variant;
    AOptions: TNCSQLOptions = []): Variant;
var
    ods: TDBDataSet;
begin
    DUSPCache.GetSQLObject(ADb, AProc, AOptions - [soNoDataFound, soToManyRows],
        [coForExec], VarArrayOf(AParams), '', Result, ods);
end;

function DUSQLLookup(const ADb, ATable, AKeyFields, AResultFields: String;
    AKeyValue: array of Variant; AOptions: TNCSQLOptions = [soNoNull]): Variant;
var
    i: Integer;
    ods: TDBDataSet;
begin
    if soNoNull in AOptions then begin
        i := 0;
        while (i <= High(AKeyValue)) and not VarIsDefined(AKeyValue[i]) do
            Inc(i);
        if i > High(AKeyValue) then begin
            Result := Null;
            Exit;
        end;
    end;
    DUQCache.GetSQLObject(ADb,
        'select ' + DUFields2SQL(AResultFields, '', False, nil) +
        ' from ' + ATable +
        ' where ' + DUParamJoin2SQL(AKeyFields, '', nil, VarArrayOf(AKeyValue),
                                    False, False, nil, '', False), AOptions,
        [coForFetchRow, coSkipNull], VarArrayOf(AKeyValue), AResultFields, Result, ods);
end;

procedure DURefreshSQLCache;
var
    i: Integer;
begin
    for i := 0 to DUQCache.Count - 1 do
        TNCSQLCacheItem(DUQCache.Objects[i]).ClearValues;
    for i := 0 to DUSPCache.Count - 1 do
        TNCSQLCacheItem(DUSPCache.Objects[i]).ClearValues;
end;
}
end.
