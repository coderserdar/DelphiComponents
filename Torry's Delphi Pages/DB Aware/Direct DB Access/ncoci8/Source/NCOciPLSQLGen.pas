{*********************************************************}
{File:      NCOciPLSQLGen.PAS                             }
{Revision:  0.01.00 / 30.01.2002                          }
{Comment:   NC OCI8 VCL: PL/SQL package wrapper generator }
{Copyright: (c) 1999-2002, Dmitry Arefiev                 }
{Author:    Dmitry Arefiev, darefiev@da-soft.com          }
{*********************************************************}
{$I NCOciDef.inc}

unit NCOciPLSQLGen;

interface

uses SysUtils, Classes, NCOci, NCOciWrapper, NCOciDB, NCOciParams;

type
    TOCIIntfCodePart = (cpHeader, cpPrivate, cpProtected, cpPublic);
    TOCIImplCodePart = (ipHeader, ipConstr, ipDestr, ipMethod);
    TOCITypeOptions = set of (toSize, toDecimal);

    TOCICodeGenerator = class
    private
        FIntfStreams: array[TOCIIntfCodePart] of TStream;
        FImplStreams: array[TOCIImplCodePart] of TStream;
        FMyClassName, FParentClassName, FOriginalObject: String;
        FIsComponent: Boolean;
    protected
        procedure WriteIntf(APart: TOCIIntfCodePart; const AStr: String);
        procedure WriteImplNewMethod(ASourceName: String);
        procedure WriteImpl(APart: TOCIImplCodePart; const AStr: String);
        procedure SaveIntf(AStream: TStream);
        procedure SaveImpl(AStream: TStream);
    public
        constructor Create;
        destructor Destroy; override;
        property MyClassName: String read FMyClassName write FMyClassName;
        property ParentClassName: String read FParentClassName write FParentClassName;
        property OriginalObject: String read FOriginalObject write FOriginalObject;
        property IsComponent: Boolean read FIsComponent write FIsComponent;
    end;

    TOCIPLSQLGenNameOptions = set of (noUpperCase, noLowerCase, noPretty,
        noTrimUnderscore, noTrimOthers);

    TOCIPLSQLGenName = class(TPersistent)
    private
        FOptions: TOCIPLSQLGenNameOptions;
        FFmt: String;
    public
        procedure Assign(ASource: TPersistent); override;
        function FormatName(const AStr: String): String;
        property Options: TOCIPLSQLGenNameOptions read FOptions write FOptions;
        property Fmt: String read FFmt write FFmt;
    end;

    EOCIGenUnSup = class(Exception)
    end;

    TOCIPLSQLNameKind = (pnField, pnRecType, pnTabType, pnPackage, pnParam,
        pnProc);

    TOCIPLSQLGenerator = class
    private
        // props
        FDatabase: TOCIDatabase;
        FPackName: String;
        FParentPackageClass: String;
        FParentRecClass: String;
        FParentTabClass: String;
        FUnitName: String;
        FNamingRules: array [TOCIPLSQLNameKind] of TOCIPLSQLGenName;
        FSkipUnsupportedProcs: Boolean;
        FProcsToGenerate: TStringList;
        // runtime
        FMetaInfo: TOCIPLSQLDescriber;
        FSql: TStringList;
        FParams: TOCIParams;
        FGenList: TStringList;
        FPackGen: TOCICodeGenerator;
        FCreatedObjects: TStringList;
        function PLSQLName2DelphiName(const AName: String; ANameKind: TOCIPLSQLNameKind): String;
        function Item2AsParamType(AItem: TOCISelectItem): String;
        function Item2DelphiType(AItem: TOCISelectItem): String;
        procedure ResetGens(AFree: Boolean);
        procedure GenComplexType(AItem: TOCISelectItem; AGen: TOCICodeGenerator);
        function GenParamUsage(AItem: TOCISelectItem; const AParamName: String;
            var AParamInd: Integer; ACreatedObject: TStrings; ASetValue: Boolean): String;
        function GetNamingRules(AIndex: TOCIPLSQLNameKind): TOCIPLSQLGenName;
        procedure SetNamingRules(AIndex: TOCIPLSQLNameKind; const Value: TOCIPLSQLGenName);
    public
        constructor Create;
        destructor Destroy; override;
        procedure Gen;
        procedure SaveToStream(AOutStream: TStream);
        // required props
        property PackName: String read FPackName write FPackName;
        property Database: TOCIDatabase read FDatabase write FDatabase;
        // options
        property ParentPackageClass: String read FParentPackageClass write FParentPackageClass;
        property ParentRecClass: String read FParentRecClass write FParentRecClass;
        property ParentTabClass: String read FParentTabClass write FParentTabClass;
        property UnitName: String read FUnitName write FUnitName;
        property NamingRules[AIndex: TOCIPLSQLNameKind]: TOCIPLSQLGenName read GetNamingRules
            write SetNamingRules;
        property SkipUnsupportedProcs: Boolean read FSkipUnsupportedProcs
            write FSkipUnsupportedProcs;
        property ProcsToGenerate: TStringList read FProcsToGenerate;
    end;

implementation

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

const
  CCommentBrd  = '{-----------------------------------------------------------------------------}';
  CCommentBody = '{                                                                             }';

procedure WriteStr(AStream: TStream; const AStr: String);
var
    S: String;
begin
    S := AStr + #13#10;
    AStream.Write(S[1], Length(S));
end;

procedure WriteComment(AStream: TStream; const AStr: String);
var
    S: String;
begin
    S := CCommentBody;
    if AStr <> '' then
        if Length(AStr) > Length(CCommentBody) - 6 then
            Move(AStr[1], S[3], Length(CCommentBody) - 6)
        else
            Move(AStr[1], S[3], Length(AStr));
    WriteStr(AStream, S);
end;

procedure WriteCommentBorder(AStream: TStream);
begin
    WriteStr(AStream, CCommentBrd);
end;

procedure StartNewMethod(AStream: TStream; ASourceName: String);
begin
    WriteStr(AStream, '');
    if ASourceName <> '' then
        WriteStr(AStream, '// Generated for: PL/SQL ' + ASourceName);
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCICodeGenerator

constructor TOCICodeGenerator.Create;
var
    intf: TOCIIntfCodePart;
    impl: TOCIImplCodePart;
begin
    inherited Create;
    for intf := Low(TOCIIntfCodePart) to High(TOCIIntfCodePart) do
        FIntfStreams[intf] := TMemoryStream.Create;
    for impl := Low(TOCIImplCodePart) to High(TOCIImplCodePart) do
        FImplStreams[impl] := TMemoryStream.Create;
end;

destructor TOCICodeGenerator.Destroy;
var
    intf: TOCIIntfCodePart;
    impl: TOCIImplCodePart;
begin
    for intf := Low(TOCIIntfCodePart) to High(TOCIIntfCodePart) do
        FIntfStreams[intf].Free;
    for impl := Low(TOCIImplCodePart) to High(TOCIImplCodePart) do
        FImplStreams[impl].Free;
    inherited Destroy;
end;

procedure TOCICodeGenerator.WriteImpl(APart: TOCIImplCodePart;
    const AStr: String);
begin
    WriteStr(FImplStreams[APart], AStr);
end;

procedure TOCICodeGenerator.WriteImplNewMethod(ASourceName: String);
begin
    StartNewMethod(FImplStreams[ipMethod], ASourceName);
end;

procedure TOCICodeGenerator.WriteIntf(APart: TOCIIntfCodePart;
    const AStr: String);
begin
  WriteStr(FIntfStreams[APart], AStr);
end;

procedure TOCICodeGenerator.SaveImpl(AStream: TStream);
var
    lNeedDelim: Boolean;
begin
    WriteStr(AStream, '// ' + MyClassName);
    WriteStr(AStream, '// Generated for: ' + OriginalObject);
    lNeedDelim := False;
    if FImplStreams[ipHeader].Size > 0 then begin
        if lNeedDelim then
            StartNewMethod(AStream, '');
        AStream.CopyFrom(FImplStreams[ipHeader], 0);
        lNeedDelim := True;
    end;
    if FImplStreams[ipConstr].Size > 0 then begin
        if lNeedDelim then
            StartNewMethod(AStream, '');
        if FIsComponent then begin
            WriteStr(AStream, 'constructor ' + MyClassName + '.Create(AOwner: TComponent);');
            WriteStr(AStream, 'begin');
            WriteStr(AStream, '  inherited Create(AOwner);');
        end
        else begin
            WriteStr(AStream, 'constructor ' + MyClassName + '.Create;');
            WriteStr(AStream, 'begin');
            WriteStr(AStream, '  inherited Create;');
        end;
        AStream.CopyFrom(FImplStreams[ipConstr], 0);
        WriteStr(AStream, 'end;');
        lNeedDelim := True;
    end;
    if FImplStreams[ipDestr].Size > 0 then begin
        if lNeedDelim then
            StartNewMethod(AStream, '');
        WriteStr(AStream, 'destructor ' + MyClassName + '.Destroy;');
        WriteStr(AStream, 'begin');
        AStream.CopyFrom(FImplStreams[ipDestr], 0);
        WriteStr(AStream, '  inherited Destroy;');
        WriteStr(AStream, 'end;');
    end;
    if FImplStreams[ipMethod].Size > 0 then begin
        AStream.CopyFrom(FImplStreams[ipMethod], 0);
        lNeedDelim := True;
    end;
    if not lNeedDelim then
        WriteStr(AStream, '// - no methods');
    WriteStr(AStream, '');
end;

procedure TOCICodeGenerator.SaveIntf(AStream: TStream);
begin
    WriteStr(AStream, '  // ' + MyClassName);
    WriteStr(AStream, '  // Generated for: ' + OriginalObject);
    if FIntfStreams[cpHeader].Size > 0 then begin
        AStream.CopyFrom(FIntfStreams[cpHeader], 0);
        WriteStr(AStream, '');
    end;
    WriteStr(AStream, '  ' + MyClassName + ' = class(' + ParentClassName + ')');
    if FIntfStreams[cpPrivate].Size > 0 then begin
        WriteStr(AStream, '  private');
        AStream.CopyFrom(FIntfStreams[cpPrivate], 0);
    end;
    if FIntfStreams[cpProtected].Size > 0 then begin
        WriteStr(AStream, '  protected');
        AStream.CopyFrom(FIntfStreams[cpProtected], 0);
    end;
    if (FIntfStreams[cpPublic].Size > 0) or
       (FImplStreams[ipConstr].Size > 0) or
       (FImplStreams[ipDestr].Size > 0) then begin
        WriteStr(AStream, '  public');
        if FImplStreams[ipConstr].Size > 0 then
            if FIsComponent then
                WriteStr(AStream, '    constructor Create(AOwner: TComponent); override;')
            else
                WriteStr(AStream, '    constructor Create; override;');
        if FImplStreams[ipDestr].Size > 0 then
            WriteStr(AStream, '    destructor Destroy; override;');
        if FIntfStreams[cpPublic].Size > 0 then
            AStream.CopyFrom(FIntfStreams[cpPublic], 0);
    end;
    WriteStr(AStream, '  end;');
    WriteStr(AStream, '');
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIPLSQLGenName

procedure TOCIPLSQLGenName.Assign(ASource: TPersistent);
begin
    if ASource is TOCIPLSQLGenName then begin
        FOptions := TOCIPLSQLGenName(ASource).FOptions;
        FFmt := TOCIPLSQLGenName(ASource).FFmt;
    end
    else
        inherited Assign(ASource);
end;

function TOCIPLSQLGenName.FormatName(const AStr: String): String;
var
    i: Integer;
    lNewWord: Boolean;
begin
    Result := AStr;
    i := 1;
    lNewWord := True;
    while i <= Length(Result) do begin
        while Result[i] = '_' do begin
            lNewWord := True;
            if noTrimUnderscore in FOptions then
                Delete(Result, i, 1)
            else
                Inc(i);
        end;
        while not (Result[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do begin
            lNewWord := True;
            if noTrimOthers in FOptions then
                Delete(Result, i, 1)
            else
                Inc(i);
        end;
        if noPretty in FOptions then
            if lNewWord then begin
                if (Result[i] >= 'a') and (Result[i] <= 'z') then
                    Result[i] := Char(Ord(Result[i]) - 32);
            end
            else begin
                if (Result[i] >= 'A') and (Result[i] <= 'Z') then
                    Result[i] := Char(Ord(Result[i]) + 32);
            end;
        Inc(i);
        lNewWord := False;
    end;
    if noUpperCase in FOptions then
        Result := UpperCase(Result)
    else if noLowerCase in FOptions then
        Result := LowerCase(Result);
    if Pos('%s', LowerCase(FFmt)) = 0 then
        Result := Format(FFmt + '%s', [Result])
    else
        Result := Format(FFmt, [Result]);
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIPLSQLGenerator

constructor TOCIPLSQLGenerator.Create;
var
    i: TOCIPLSQLNameKind;
begin
    inherited Create;
    FDatabase := nil;
    FPackName := '';
    FSql := TStringList.Create;
    FParams := TOCIParams.Create{$IFNDEF OCI_D4} (nil) {$ENDIF};
    FCreatedObjects := TStringList.Create;
    FGenList := TStringList.Create;
    ResetGens(False);
    FParentPackageClass := 'TOCICustomPackage';
    FParentRecClass := 'TOCIPLSQLRecord';
    FParentTabClass := 'TOCIPLSQLTable';
    FUnitName := '';
    FSkipUnsupportedProcs := True;
    for i := Low(TOCIPLSQLNameKind) to High(TOCIPLSQLNameKind) do
        FNamingRules[i] := TOCIPLSQLGenName.Create;
    with FNamingRules[pnRecType] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := 'TOCI%s';
    end;
    with FNamingRules[pnField] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := '%s';
    end;
    with FNamingRules[pnTabType] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := 'TOCI%s';
    end;
    with FNamingRules[pnPackage] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := 'TOCI%s';
    end;
    with FNamingRules[pnProc] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := '%s';
    end;
    with FNamingRules[pnParam] do begin
        Options := [noPretty, noTrimUnderscore, noTrimOthers];
        Fmt := 'A%s';
    end;
    FProcsToGenerate := TStringList.Create;
    FProcsToGenerate.Sorted := True;
    FProcsToGenerate.Duplicates := dupError;
end;

destructor TOCIPLSQLGenerator.Destroy;
var
    i: TOCIPLSQLNameKind;
begin
    FSql.Free;
    FParams.Free;
    ResetGens(True);
    FGenList.Free;
    FCreatedObjects.Free;
    for i := Low(TOCIPLSQLNameKind) to High(TOCIPLSQLNameKind) do
        FNamingRules[i].Free;
    FProcsToGenerate.Free;
    inherited Destroy;
end;

function TOCIPLSQLGenerator.GetNamingRules(AIndex: TOCIPLSQLNameKind): TOCIPLSQLGenName;
begin
    Result := FNamingRules[AIndex];
end;

procedure TOCIPLSQLGenerator.SetNamingRules(AIndex: TOCIPLSQLNameKind; const Value: TOCIPLSQLGenName);
begin
    FNamingRules[AIndex].Assign(Value);
end;

function TOCIPLSQLGenerator.PLSQLName2DelphiName(const AName: String; ANameKind: TOCIPLSQLNameKind): String;
begin
    Result := FNamingRules[ANameKind].FormatName(AName);
end;

function TOCIPLSQLGenerator.Item2AsParamType(AItem: TOCISelectItem): String;
begin
    case AItem.DATA_TYPE of
    _SQLT_BOL: Result := 'AsBoolean';
    else
        case AItem.DataType of
        otSmallInt: Result := 'AsSmallInt';
        otInteger:  Result := 'AsInteger';
        otFloat:    Result := 'AsFloat';
        otBCD:      Result := 'AsBCD';
        otNumber:   Result := 'AsString';
        otString:   Result := 'AsString';
        otChar:     Result := 'AsChar';
        otRaw:      Result := 'AsBlob';
        otDateTime: Result := 'AsDateTime';
        otLong:     Result := 'AsString';
        otLongRaw:  Result := 'AsBlob';
        otROWID:    Result := 'AsString';
        else
            // otUnknown, otCursor, otNestedDataSet, otCLOB, otBLOB, otCFile, otBFile
            if FSkipUnsupportedProcs then
                raise EOCIGenUnSup.Create('Oracle data type does not support AsXXXX property')
            else
                Result := 'As***';
        end;
    end;
end;

function TOCIPLSQLGenerator.Item2DelphiType(AItem: TOCISelectItem): String;
begin
    case AItem.DATA_TYPE of
    _SQLT_REC: Result := PLSQLName2DelphiName(AItem.SUB_NAME, pnRecType);
    _SQLT_TAB: Result := PLSQLName2DelphiName(AItem.SUB_NAME, pnTabType);
    _SQLT_BOL: Result := 'Boolean';
    else
        case AItem.DataType of
        otSmallInt: Result := 'SmallInt';
        otInteger:  Result := 'Longint';
        otFloat:    Result := 'Double';
        otBCD:      Result := 'Currency';
        otNumber:   Result := 'String';
        otString:   Result := 'String';
        otChar:     Result := 'String';
        otRaw:      Result := 'TBlobData';
        otDateTime: Result := 'TDateTime';
        otLong:     Result := 'String';
        otLongRaw:  Result := 'TBlobData';
        otROWID:    Result := 'String';
        otCLOB,
        otBLOB:     Result := 'TOCIILOBStream';
        otCFile,
        otBFile:    Result := 'TOCIFILEStream';
        else
            // otUnknown, otCursor, otNestedDataSet
            if FSkipUnsupportedProcs then
                raise EOCIGenUnSup.Create('Oracle data type is not mapped to Delphi data type')
            else
                Result := '***';
        end;
    end;
end;

procedure TOCIPLSQLGenerator.ResetGens(AFree: Boolean);
var
    i: Integer;
begin
    for i := 0 to FGenList.Count - 1 do
        TOCICodeGenerator(FGenList.Objects[i]).Free;
    FGenList.Clear;
    FPackGen := nil;
    if not AFree then begin
        FPackGen := TOCICodeGenerator.Create;
        FPackGen.IsComponent := True;
        FGenList.AddObject(FPackName, FPackGen);
    end;
end;

procedure TOCIPLSQLGenerator.GenComplexType(AItem: TOCISelectItem; AGen: TOCICodeGenerator);
var
    oGen: TOCICodeGenerator;
    i, n: Integer;
    subItem: TOCISelectItem;
    sFieldName: String;
begin
    case AItem.DATA_TYPE of
    _SQLT_REC:
        begin
            i := FGenList.IndexOf(AItem.SUB_NAME);
            if i = -1 then begin
                oGen := TOCICodeGenerator.Create;
                oGen.ParentClassName := FParentRecClass;
                oGen.MyClassName := Item2DelphiType(AItem);
                oGen.OriginalObject := 'PL/SQL Record "' + AItem.SUB_NAME + '"';
                FGenList.InsertObject(FGenList.Count - 1, AItem.SUB_NAME, oGen);
                oGen.WriteIntf(cpPublic, '    procedure Assign(ASource: TPersistent); override;');
                oGen.WriteImplNewMethod('');
                oGen.WriteImpl(ipMethod, 'procedure ' + oGen.MyClassName + '.Assign(ASource: TPersistent);');
                oGen.WriteImpl(ipMethod, 'begin');
                oGen.WriteImpl(ipMethod, '  if ASource is ' + oGen.MyClassName + ' then begin');
                n := FMetaInfo.Descr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
                try
                    for i := 1 to n do begin
                        subItem := FMetaInfo.Descr.SelectItem[i];
                        if subItem = nil then
                            Break;
                        try
                            GenComplexType(subItem, oGen);
                        finally
                            subItem.Free;
                        end;
                    end;
                finally
                    FMetaInfo.Descr.CloseList;
                end;
                oGen.WriteImpl(ipMethod, '  end');
                oGen.WriteImpl(ipMethod, '  else');
                oGen.WriteImpl(ipMethod, '    inherited Assign(ASource);');
                oGen.WriteImpl(ipMethod, 'end;');
            end
            else
                oGen := TOCICodeGenerator(FGenList.Objects[i]);
            if AGen <> nil then begin
                sFieldName := PLSQLName2DelphiName(AItem.name, pnField);
                AGen.WriteIntf(cpPrivate, '    F' + sFieldName + ': ' + oGen.MyClassName + ';');
                AGen.WriteIntf(cpPublic, '    property ' + sFieldName + ': ' + oGen.MyClassName +
                    ' read F' + sFieldName + ';');
                AGen.WriteImpl(ipMethod, '    F' + sFieldName + '.Assign(' + AGen.MyClassName +
                    '(ASource).F' + sFieldName + ');');
                AGen.WriteImpl(ipConstr, '  F' + sFieldName + ' := ' + oGen.MyClassName +
                    '.Create;');
                AGen.WriteImpl(ipDestr, '  F' + sFieldName + '.Free;');
                AGen.WriteImpl(ipDestr, '  F' + sFieldName + ' := nil;');
            end;
        end;
    _SQLT_TAB:
        begin
            i := FGenList.IndexOf(AItem.SUB_NAME);
            if i = -1 then begin
                oGen := TOCICodeGenerator.Create;
                oGen.ParentClassName := FParentTabClass;
                oGen.MyClassName := Item2DelphiType(AItem);
                oGen.OriginalObject := 'PL/SQL Table "' + AItem.SUB_NAME + '"';
                FGenList.InsertObject(FGenList.Count - 1, AItem.SUB_NAME, oGen);
            end
            else
                oGen := TOCICodeGenerator(FGenList.Objects[i]);
            if AGen <> nil then begin
                sFieldName := PLSQLName2DelphiName(AItem.name, pnField);
                AGen.WriteIntf(cpPrivate, '    F' + sFieldName + ': ' + oGen.MyClassName + ';');
                AGen.WriteIntf(cpPublic, '    property ' + sFieldName + ': ' + oGen.MyClassName +
                    ' read F' + sFieldName + ';');
                AGen.WriteImpl(ipMethod, '    F' + sFieldName + '.Assign(' + AGen.MyClassName +
                    '(ASource).F' + sFieldName + ');');
                AGen.WriteImpl(ipConstr, '  F' + sFieldName + ' := ' + oGen.MyClassName +
                    '.Create;');
                AGen.WriteImpl(ipDestr, '  F' + sFieldName + '.Free;');
                AGen.WriteImpl(ipDestr, '  F' + sFieldName + ' := nil;');
            end;
        end;
    else
        if AGen <> nil then begin
            sFieldName := PLSQLName2DelphiName(AItem.name, pnField);
            AGen.WriteIntf(cpPrivate, '    F' + sFieldName + ': ' + Item2DelphiType(AItem) + ';');
            AGen.WriteIntf(cpPublic, '    property ' + sFieldName + ': ' + Item2DelphiType(AItem) +
                ' read F' + sFieldName + ' write F' + sFieldName + ';');
            AGen.WriteImpl(ipMethod, '    F' + sFieldName + ' := ' + AGen.MyClassName +
                '(ASource).F' + sFieldName + ';');
        end;
    end;
end;

function TOCIPLSQLGenerator.GenParamUsage(AItem: TOCISelectItem; const AParamName: String;
    var AParamInd: Integer; ACreatedObject: TStrings; ASetValue: Boolean): String;
var
    i, n: Integer;
    subItem: TOCISelectItem;
    s: String;
begin
    Result := '';
    case AItem.DATA_TYPE of
    _SQLT_REC:
        begin
            GenComplexType(AItem, nil);
            if ACreatedObject <> nil then
                ACreatedObject.Add(AParamName + ' := ' + Item2DelphiType(AItem) + '.Create;');
            n := FMetaInfo.Descr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
            try
                for i := 1 to n do begin
                    subItem := FMetaInfo.Descr.SelectItem[i];
                    if subItem = nil then
                        Break;
                    try
                        if i > 1 then
                            Result := Result + #13#10;
                        Result := Result + GenParamUsage(subItem, AParamName + '.' +
                            PLSQLName2DelphiName(subItem.name, pnField), AParamInd,
                            nil, ASetValue);
                    finally
                        subItem.Free;
                    end;
                end;
            finally
                FMetaInfo.Descr.CloseList;
            end;
        end;
    _SQLT_TAB:
        begin
            GenComplexType(AItem, nil);
            if ACreatedObject <> nil then
                ACreatedObject.Add(AParamName + ' := ' + Item2DelphiType(AItem) + '.Create;');
            if ASetValue then
                Result := '    Params[' + IntToStr(AParamInd) + '].Assign(' + AParamName + ');'
            else
                Result := '    ' + AParamName + '.Assign(Params[' + IntToStr(AParamInd) + ']);';
            Inc(AParamInd);
        end;
    SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
            Result := '    Params[' + IntToStr(AParamInd) + '].AssignByRef(' +
                AParamName + ');';
            Inc(AParamInd);
            if ACreatedObject <> nil then begin
                s := '';
                case AItem.DATA_TYPE of
                SQLT_CLOB:
                    s := 'otCLOB';
                SQLT_BLOB:
                    s := 'otBLOB';
                SQLT_BFILEE:
                    s := 'otBFile';
                SQLT_CFILEE:
                    s := 'otCFile';
                end;
                if ASetValue then
                    ACreatedObject.Add(AParamName + ' := ' + Item2DelphiType(AItem) +
                        '.CreateAlone(Database, bmReadWrite, ' + s + ');')
                else
                    ACreatedObject.Add(AParamName + ' := ' + Item2DelphiType(AItem) +
                        '.CreateAlone(Database, bmRead, ' + s + ');');
            end;
        end;
    else
        begin
            if ASetValue then
                Result := '    Params[' + IntToStr(AParamInd) + '].' +
                    Item2AsParamType(AItem) + ' := ' + AParamName + ';'
            else
                Result := '    ' + AParamName + ' := ' + 'Params[' +
                    IntToStr(AParamInd) + '].' + Item2AsParamType(AItem) + ';';
            Inc(AParamInd);
        end;
    end;
end;

procedure TOCIPLSQLGenerator.Gen;
const
    SBools: array[Boolean] of String = ('False', 'True');
var
    oItem: TOCISelectItem;
    numArgs, iParamInd, iLastParamInd: Integer;
    iQueryInd: Integer;
    iOverld: Integer;
    j, i, prevJ: Integer;
    sPrcName, sProcType, sFuncResultType, sPars, sParName, sBodySetPars,
        sBodyGetPars, sBodyGetResult: String;
    sHeaderIntf, sHeaderImpl: String;
    dlpName, s: String;
begin
    ResetGens(False);
    FMetaInfo := TOCIPLSQLDescriber.CreateForPack(FDatabase, FDatabase,
        FDatabase.DefaultDataFormat, FPackName);
    FPackGen.ParentClassName := FParentPackageClass;
    FPackGen.MyClassName := PLSQLName2DelphiName(FPackName, pnPackage);
    FPackGen.OriginalObject := 'PL/SQL Package "' + FPackName + '"';
    try
        FMetaInfo.Describe;
        iQueryInd := 0;
        FMetaInfo.First(sPrcName, iOverld);
        while not FMetaInfo.EOL do begin
            sFuncResultType := '';
            sBodyGetResult := '';
            iParamInd := 0;
            sPars := '';
            sBodySetPars := '';
            sBodyGetPars := '';
            FCreatedObjects.Clear;

            if FProcsToGenerate.IndexOf(sPrcName + '[' + IntToStr(iOverld) + ']') <> -1 then
            try
                numArgs := FMetaInfo.Descr.OpenList(OCI_ATTR_LIST_ARGUMENTS);
                try
                    if FMetaInfo.ObjType = OCI_PTYPE_FUNC then begin
                        oItem := FMetaInfo.Descr.SelectItem[0];
                        if oItem <> nil then begin
                            try
                                sFuncResultType := Item2DelphiType(oItem);
                                if oItem.DATA_TYPE in [SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE] then
                                    sBodySetPars := GenParamUsage(oItem, 'Result', iParamInd, FCreatedObjects, True)
                                else
                                    sBodyGetResult := GenParamUsage(oItem, 'Result', iParamInd, FCreatedObjects, False);
                            finally
                                oItem.Free;
                            end;
                            Dec(numArgs);
                            iParamInd := 1;
                        end;
                    end;

                    if numArgs > 0 then
                        sPars := sPars + '(';
                    for i := 1 to numArgs do begin
                        oItem := FMetaInfo.Descr.SelectItem[i];
                        if oItem = nil then
                            Break;
                        try
                            if i > 1 then
                                sPars := sPars + '; ';
                            case oItem.VarType of
                            odIn:    sPars := sPars + 'const ';
                            odOut:   sPars := sPars + 'out ';
                            odInOut: sPars := sPars + 'var ';
                            end;
                            sParName := PLSQLName2DelphiName(oItem.name, pnParam);
                            sPars := sPars + sParName + ': ' + Item2DelphiType(oItem);
                            if oItem.DATA_TYPE in [SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE] then begin
                                if sBodySetPars <> '' then
                                    sBodySetPars := sBodySetPars + #13#10;
                                if oItem.VarType = odOut then
                                    sBodySetPars := sBodySetPars + GenParamUsage(oItem, sParName,
                                        iParamInd, FCreatedObjects, True)
                                else
                                    sBodySetPars := sBodySetPars + GenParamUsage(oItem, sParName,
                                        iParamInd, nil, True);
                            end
                            else begin
                                iLastParamInd := iParamInd;
                                if oItem.VarType in [odIn, odInOut] then begin
                                    if sBodySetPars <> '' then
                                        sBodySetPars := sBodySetPars + #13#10;
                                    sBodySetPars := sBodySetPars + GenParamUsage(oItem, sParName,
                                        iParamInd, nil, True);
                                end;
                                if oItem.VarType in [odOut, odInOut] then begin
                                    if sBodyGetPars <> '' then
                                        sBodyGetPars := sBodyGetPars + #13#10;
                                    iParamInd := iLastParamInd;
                                    if oItem.VarType = odOut then
                                        sBodyGetPars := sBodyGetPars + GenParamUsage(oItem, sParName,
                                            iParamInd, FCreatedObjects, False)
                                    else
                                        sBodyGetPars := sBodyGetPars + GenParamUsage(oItem, sParName,
                                            iParamInd, nil, False);
                                end;
                            end;
                        finally
                            oItem.Free;
                        end;
                    end;
                finally
                    FMetaInfo.Descr.CloseList;
                end;
                if numArgs > 0 then
                    sPars := sPars + ')';

                sHeaderIntf := '    ';
                sHeaderImpl := '';
                if FMetaInfo.ObjType = OCI_PTYPE_FUNC then
                    sProcType := 'function'
                else
                    sProcType := 'procedure';
                sHeaderIntf := sHeaderIntf + sProcType + ' ';
                sHeaderImpl := sHeaderImpl + sProcType + ' ';
                dlpName := PLSQLName2DelphiName(sPrcName, pnProc);
                sHeaderIntf := sHeaderIntf + dlpName;
                sHeaderImpl := sHeaderImpl + FPackGen.MyClassName + '.' + dlpName;
                sHeaderIntf := sHeaderIntf + sPars;
                sHeaderImpl := sHeaderImpl + sPars;
                if FMetaInfo.ObjType = OCI_PTYPE_FUNC then begin
                    sHeaderIntf := sHeaderIntf + ': ' + sFuncResultType;
                    sHeaderImpl := sHeaderImpl + ': ' + sFuncResultType;
                end;
                if iOverld > 0 then
                    sHeaderIntf := sHeaderIntf + '; overload;'
                else
                    sHeaderIntf := sHeaderIntf + ';';
                sHeaderImpl := sHeaderImpl + ';';

                try
                    FSql.Clear;
                    FParams.Clear;
                    FMetaInfo.BuildSQL(FSql, FParams);
                except on E: Exception do
                    if FSkipUnsupportedProcs then
                        raise EOCIGenUnSup.Create(E.Message)
                    else
                        FSql.Text := '***' + E.Message + '***';
                end;

                FPackGen.WriteIntf(cpPublic, sHeaderIntf);

                s := sProcType + ' "' + FPackName + '"."' + sPrcName + '"';
                if iOverld > 0 then
                    s := s + ' [' + IntToStr(iOverld) + ']';
                FPackGen.WriteImplNewMethod(s);
                
                FPackGen.WriteImpl(ipMethod, sHeaderImpl);
                FPackGen.WriteImpl(ipMethod, 'begin');
                FPackGen.WriteImpl(ipMethod, '  with GetQuery(' + IntToStr(iQueryInd) + ') do begin');
                FPackGen.WriteImpl(ipMethod, '    if SQL.Count = 0 then begin');
                FPackGen.WriteImpl(ipMethod, '      try');
                FPackGen.WriteImpl(ipMethod, '        SQL.BeginUpdate;');

                for i := 0 to FSql.Count - 1 do begin
                    s := FSql[i];
                    prevJ := 1;
                    for j := 1 to Length(s) do
                        if (s[j] = #13) and (s[j + 1] = #10) then begin
                            FPackGen.WriteImpl(ipMethod, '        SQL.Add(' +
                                QuotedStr(Copy(s, prevJ, j - prevJ)) + ');');
                            prevJ := j + 2;
                        end;
                    if prevJ <= Length(s) then
                        FPackGen.WriteImpl(ipMethod, '        SQL.Add(' +
                            QuotedStr(Copy(s, prevJ, Length(s) - prevJ + 1)) + ');');
                end;

                FPackGen.WriteImpl(ipMethod, '      finally');
                FPackGen.WriteImpl(ipMethod, '        SQL.EndUpdate;');
                FPackGen.WriteImpl(ipMethod, '      end;');

                FPackGen.WriteImpl(ipMethod, '      Params.Clear;');
                for i := 0 to FParams.Count - 1 do begin
                    FPackGen.WriteImpl(ipMethod, '      with Params.Add do begin');
                    if FParams[i].OName <> '' then
                        FPackGen.WriteImpl(ipMethod, '        OName := ' + QuotedStr(FParams[i].OName) + ';');
                    if FParams[i].OParamType <> odUnknown then
                        FPackGen.WriteImpl(ipMethod, '        OParamType := ' + varTypeName[FParams[i].OParamType] + ';');
                    if FParams[i].IsPLSQLTable then
                        FPackGen.WriteImpl(ipMethod, '        IsPLSQLTable := ' + SBools[FParams[i].IsPLSQLTable] + ';');
                    if FParams[i].ArrayLen <> 1 then
                        FPackGen.WriteImpl(ipMethod, '        ArrayLen := ' + IntToStr(FParams[i].ArrayLen) + ';');
                    if FParams[i].ODataType <> otUnknown then
                        FPackGen.WriteImpl(ipMethod, '        ODataType := ' + dataTypeName[FParams[i].ODataType] + ';');
                    if FParams[i].ODataSize <> 0 then
                        FPackGen.WriteImpl(ipMethod, '        ODataSize := ' + IntToStr(FParams[i].ODataSize) + ';');
                    FPackGen.WriteImpl(ipMethod, '      end;');
                end;
                FPackGen.WriteImpl(ipMethod, '    end;');

                for i := 0 to FCreatedObjects.Count - 1 do
                    FPackGen.WriteImpl(ipMethod, '    ' + FCreatedObjects[i]);

                if sBodySetPars <> '' then
                    FPackGen.WriteImpl(ipMethod, sBodySetPars);
                FPackGen.WriteImpl(ipMethod, '    ExecSQL;');
                if sBodyGetPars <> '' then
                    FPackGen.WriteImpl(ipMethod, sBodyGetPars);

                if FMetaInfo.ObjType = OCI_PTYPE_FUNC then
                    FPackGen.WriteImpl(ipMethod, sBodyGetResult);

                FPackGen.WriteImpl(ipMethod, '  end;');
                FPackGen.WriteImpl(ipMethod, 'end;');

            except
                on E: EOCIGenUnSup do begin
                    s := sProcType + ' "' + FPackName + '"."' + sPrcName + '"';
                    if iOverld > 0 then
                        s := s + ' [' + IntToStr(iOverld) + ']';
                    FPackGen.WriteImplNewMethod(s);
                    FPackGen.WriteImpl(ipMethod, '// *** ERROR: ' + E.Message);
                end;
            end;

            FMetaInfo.Next(sPrcName, iOverld);
            Inc(iQueryInd);
        end;

        FPackGen.WriteImpl(ipConstr, '  SetProcCount(' + IntToStr(iQueryInd) + ');');

    finally
        FMetaInfo.Free;
        FMetaInfo := nil;
    end;
end;

procedure TOCIPLSQLGenerator.SaveToStream(AOutStream: TStream);
var
    i: Integer;
begin
    WriteCommentBorder(AOutStream);
    WriteComment(AOutStream, 'This file is generated automatically by');
    WriteComment(AOutStream, 'NCOCI8 PL/SQL Wrapper Objects Generator, (c) 1999-2002, Dmitry Arefiev');
    WriteCommentBorder(AOutStream);
    WriteComment(AOutStream, 'Generated classes are:');
    for i := 0 to FGenList.Count - 1 do
        WriteComment(AOutStream, TOCICodeGenerator(FGenList.Objects[i]).MyClassName +
            ' -> ' + TOCICodeGenerator(FGenList.Objects[i]).OriginalObject);
    WriteComment(AOutStream, 'Warning: do not change this unit manually !');
    WriteComment(AOutStream, 'Otherwise your changes may be overwited in next time.');
    WriteCommentBorder(AOutStream);
    WriteStr(AOutStream, 'unit ' + FUnitName + ';');
    WriteStr(AOutStream, '');
    WriteStr(AOutStream, 'interface');
    WriteStr(AOutStream, '');
    WriteStr(AOutStream, 'uses Classes, SysUtils, DB, NCOci, NCOciWrapper, NCOciDB;');
    WriteStr(AOutStream, '');
    WriteStr(AOutStream, 'type');
    for i := 0 to FGenList.Count - 1 do
        TOCICodeGenerator(FGenList.Objects[i]).SaveIntf(AOutStream);
    WriteStr(AOutStream, 'implementation');
    WriteStr(AOutStream, '');
    for i := 0 to FGenList.Count - 1 do
        TOCICodeGenerator(FGenList.Objects[i]).SaveImpl(AOutStream);
    WriteStr(AOutStream, 'end.');
end;

end.
