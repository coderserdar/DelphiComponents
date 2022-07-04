{*******************************************************}
{File:      NCOciParams.PAS                             }
{Revision:  0.04.06 / 13.04.2005                        }
{Comment:   NC OCI8 VCL: TOCIParam, TOCIParams          }
{Copyright: (c) 1999-2005, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciParams;

interface

Uses Classes, DB, NCOci, NCOciWrapper;

type
    TOciParam = class;
    TOciParams = class;
    TOCIMacro = class;
    TOCIMacros = class;

{$IFNDEF OCI_D4}
    TBlobData = string;
    TParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
    TParamTypes = set of TParamType;
{$ENDIF}    

    TOciParam = class(TCollectionItem)
    private
        FBound: Boolean;
        FOCIVariable: TOCIVariable;

        function GetDataSet: TDataSet;
        function IsParamStored: Boolean;

        function GetDataType: TFieldType;
        function GetName: string;
        function GetOName: string;
        function GetParamType: TParamType;
        function GetArrayLen: Integer;
        function GetODataType: TOCIVarDataType;
        function GetOParamType: TOCIVarType;
        function GetDataSize: Integer;

        procedure SetName(const Value: string);
        procedure SetOName(const Value: string);
        procedure SetParamType(const Value: TParamType);
        procedure SetArrayLen(const Value: Integer);
        procedure SetIsNulls(AIndex: Integer; const Value: Boolean);
        procedure SetODataType(const Value: TOCIVarDataType);
        procedure SetOParamType(const Value: TOCIVarType);
        procedure SetDataSize(AValue: Integer);
        procedure SetDataType(Value: TFieldType);

        function GetAsBCD: Currency;
        function GetAsDateTime: TDateTime;
        function GetAsFloat: Double;
        function GetAsInteger: Longint;
        function GetAsMemo: string;
        function GetAsString: string;
        function GetIsNull: Boolean;
        function GetAsVariant: Variant;
        function GetAsBoolean: Boolean;
        function GetAsHandle: pOCIHandle;

        function GetAsBCDs(AIndex: Integer): Currency;
        function GetAsDateTimes(AIndex: Integer): TDateTime;
        function GetAsFloats(AIndex: Integer): Double;
        function GetAsIntegers(AIndex: Integer): Longint;
        function GetAsMemos(AIndex: Integer): string;
        function GetAsStrings(AIndex: Integer): string;
        function GetAsVariants(AIndex: Integer): Variant;
        function GetAsBooleans(AIndex: Integer): Boolean;
        function GetIsNulls(AIndex: Integer): Boolean;
        function GetAsHandles(AIndex: Integer): pOCIHandle;

        procedure SetAsBCD(const Value: Currency);
        procedure SetAsBlob(const Value: TBlobData);
        procedure SetAsDateTime(const Value: TDateTime);
        procedure SetAsFloat(const Value: Double);
        procedure SetAsInteger(Value: Longint);
        procedure SetAsMemo(const Value: string);
        procedure SetAsString(const Value: string);
        procedure SetAsSmallInt(Value: LongInt);
        procedure SetAsVariant(const Value: Variant);
        procedure SetText(const Value: string);
        procedure SetIsNull(const Value: Boolean);
        procedure SetAsBoolean(const Value: Boolean);

        procedure SetAsBCDs(AIndex: Integer; const Value: Currency);
        procedure SetAsBlobs(AIndex: Integer; const Value: TBlobData);
        procedure SetAsDateTimes(AIndex: Integer; const Value: TDateTime);
        procedure SetAsFloats(AIndex: Integer; const Value: Double);
        procedure SetAsIntegers(AIndex: Integer; Value: Longint);
        procedure SetAsMemos(AIndex: Integer; const Value: string);
        procedure SetAsStrings(AIndex: Integer; const Value: string);
        procedure SetAsChars(AIndex: Integer; const Value: string);
        procedure SetAsSmallInts(AIndex: Integer; Value: LongInt);
        procedure SetAsVariants(AIndex: Integer; const Value: Variant);
        procedure SetTexts(AIndex: Integer; const Value: string);
        procedure SetAsBooleans(AIndex: Integer; const Value: Boolean);

        procedure SetData(AIndex: Integer; AData: pUb1; ALen: sb4; AType: TOCIVarDataType);
        function GetIsPLSQLTable: Boolean;
        procedure SetIsPLSQLTable(const Value: Boolean);
        function GetIsCaseSensitive: Boolean;
        procedure SetIsCaseSensitive(const Value: Boolean);

        class function CanAssignBlobObjects(ADest: TObject; ASrc: TObject): Boolean;
        class procedure AssignBlobObjects(ADest: TObject; ADestIndex: Integer;
            ASrc: TObject; ASrcIndex: Integer);
        function GetAssignIncludeBlobs: Boolean;
        procedure InitUsingField(Field: TField);
        procedure InitUsingParam(Param: TOciParam);
        function GetOwned(AIndex: Integer): Boolean;

    protected
        procedure AssignParam(Param: TOciParam);
{$IFDEF OCI_D4}
        procedure AssignDelphiParam(Param: TParam);
{$ENDIF}        
        procedure AssignTo(Dest: TPersistent); override;
        function GetDisplayName: string; override;

    public
        constructor Create(Collection: TCollection); override;
        destructor Destroy; override;

        procedure Assign(Source: TPersistent); override;
        procedure AssignField(Field: TField; AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
        procedure AssignFieldValue(Field: TField; const Value: Variant; AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
        procedure AssignDataSet(DataSet: TDataSet);
        procedure AssignByRef(ASource: TObject; AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
        procedure LoadFromFile(const FileName: string; BlobType: TFieldType; AIndex: Integer {$IFDEF OCI_D4} = 0 {$ENDIF});
        procedure LoadFromStream(Stream: TStream; BlobType: TFieldType; AIndex: Integer {$IFDEF OCI_D4} = 0 {$ENDIF});
        function IsEqual(Value: TOciParam): Boolean;
        function CreateBlobStream(AIndex: Integer; Mode: TBlobStreamMode): TStream;
        procedure BlobModified(AIndex: Integer; AIsNull: Boolean);
        procedure Clear;

        property Bound: Boolean read FBound write FBound;
        property DataSet: TDataSet read GetDataSet;
        property HVariable: TOCIVariable read FOCIVariable;

        property AsBCD: Currency read GetAsBCD write SetAsBCD;
        property AsBlob: TBlobData read GetAsString write SetAsBlob;
        property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
        property AsFloat: Double read GetAsFloat write SetAsFloat;
        property AsInteger: LongInt read GetAsInteger write SetAsInteger;
        property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
        property AsMemo: string read GetAsMemo write SetAsMemo;
        property AsString: string read GetAsString write SetAsString;
        property AsChar: string read GetAsString write SetAsString;
        property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
        property AsHandle: pOCIHandle read GetAsHandle;
        property Text: string read GetAsString write SetText;

        property IsNulls[AIndex: Integer]: Boolean read GetIsNulls write SetIsNulls;
        property AsBCDs[AIndex: Integer]: Currency read GetAsBCDs write SetAsBCDs;
        property AsBlobs[AIndex: Integer]: TBlobData read GetAsStrings write SetAsBlobs;
        property AsDateTimes[AIndex: Integer]: TDateTime read GetAsDateTimes write SetAsDateTimes;
        property AsFloats[AIndex: Integer]: Double read GetAsFloats write SetAsFloats;
        property AsIntegers[AIndex: Integer]: LongInt read GetAsIntegers write SetAsIntegers;
        property AsSmallInts[AIndex: Integer]: LongInt read GetAsIntegers write SetAsSmallInts;
        property AsMemos[AIndex: Integer]: string read GetAsMemos write SetAsMemos;
        property AsStrings[AIndex: Integer]: string read GetAsStrings write SetAsStrings;
        property AsChars[AIndex: Integer]: string read GetAsStrings write SetAsChars;
        property AsBooleans[AIndex: Integer]: Boolean read GetAsBooleans write SetAsBooleans;
        property AsHandles[AIndex: Integer]: pOCIHandle read GetAsHandles;
        property Values[AIndex: Integer]: Variant read GetAsVariants write SetAsVariants;
        property Texts[AIndex: Integer]: string read GetAsStrings write SetTexts;
        property Owned[AIndex: Integer]: Boolean read GetOwned;

        property AssignIncludeBlobs: Boolean read GetAssignIncludeBlobs;

    published
        property ArrayLen: Integer read GetArrayLen write SetArrayLen
            default 1;
        property OName: String read GetOName write SetOName;
        property ODataType: TOCIVarDataType read GetODataType write SetODataType
            default otUnknown;
        property OParamType: TOCIVarType read GetOParamType write SetOParamType
            default odUnknown;
        property ODataSize: Integer read GetDataSize write SetDataSize
            default 0;
        property IsPLSQLTable: Boolean read GetIsPLSQLTable write SetIsPLSQLTable
            default False;
        property IsCaseSensitive: Boolean read GetIsCaseSensitive write SetIsCaseSensitive
            default False;
        property Value: Variant read GetAsVariant write SetAsVariant stored IsParamStored;
        property IsNull: Boolean read GetIsNull write SetIsNull stored False;
        // for compatibilitie with standard TParam
        property Name: string read GetName write SetName stored False;
        property DataType: TFieldType read GetDataType write SetDataType stored False;
        property ParamType: TParamType read GetParamType write SetParamType stored False;
    end;

    TOCIParamBindMode = (pbByName, pbByNumber);
    TOciParams = class(TCollection)
    private
        FOwner: TPersistent;
        FAssignIncludeBlobs: Boolean;
        FBindMode: TOCIParamBindMode;
        function GetParamValue(const ParamName: string): Variant;
        procedure ReadBinaryData(Stream: TStream);
        procedure SetParamValue(const ParamName: string;
          const Value: Variant);
        function GetItem(Index: Integer): TOciParam;
        procedure SetItem(Index: Integer; Value: TOciParam);
    protected
        procedure DefineProperties(Filer: TFiler); override;
        function GetDataSet: TDataSet;
        function GetOwner: TPersistent; override;
    public
        constructor Create(Owner: TPersistent); {$IFDEF OCI_D4} overload; {$ENDIF}
        procedure AssignValues(Value: TOciParams);
        { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
{$IFDEF OCI_D4}
        constructor Create; overload;
{$ENDIF}        
        function Add: TOciParam;
        procedure AddParam(Value: TOciParam);
        procedure RemoveParam(Value: TOciParam);
        function CreateParam(FldType: TFieldType; const ParamName: string;
            ParamType: TParamType): TOciParam;
        function CreateOParam(OFldType: TOCIVarDataType; ODataSize: Integer;
            const OParamName: string; OParamType: TOCIVarType; OIsPLSQLTable: Boolean;
            OIsCaseSensitive: Boolean): TOciParam;
        procedure GetParamList(List: TList; const ParamNames: string);
        function IsEqual(Value: TOciParams): Boolean;
        function ParamByName(const Value: string): TOciParam;
        function FindParam(const Value: string): TOciParam;
        procedure Bind(Astatement: TOCIStatement; AValue: Boolean);
        procedure SetValuesExt(const AParams: String; const AValue: Variant;
            const AParamsPrefix: String);
        property Items[Index: Integer]: TOciParam read GetItem write SetItem; default;
        property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
        property AssignIncludeBlobs: Boolean read FAssignIncludeBlobs
            write FAssignIncludeBlobs;
        property ParamBindMode: TOCIParamBindMode read FBindMode write FBindMode
            default pbByName;
        property DataSet: TDataSet read GetDataSet;
    end;

    TOCIMacroType = (mtString, mtSQL);
    TOCIMacroDataType = (mdUnknown, mdString, mdInteger, mdBoolean, mdFloat, mdDate, mdDateTime);
    TOCIMacro = class(TCollectionItem)
    private
        FName: String;
        FValue: Variant;
        FMacroType: TOCIMacroType;
        FDataType: TOCIMacroDataType;
        function GetDataSet: TDataSet;
        procedure SetValue(const AValue: Variant);
        function GetAsDateTime: TDateTime;
        function GetAsInteger: Integer;
        function GetAsString: String;
        function GetSQL: String;
        procedure SetAsDateTime(const AValue: TDateTime);
        procedure SetAsInteger(const AValue: Integer);
        procedure SetAsString(const AValue: String);
        function GetIsNull: Boolean;
        function GetAsFloat: Double;
        procedure SetAsFloat(const AValue: Double);
        function GetAsDate: TDateTime;
        procedure SetAsDate(const AValue: TDateTime);
        procedure SetDataType(const AValue: TOCIMacroDataType);
        procedure SetData(const AValue: Variant; AType: TOCIMacroDataType);
        procedure Changed;
    protected
        function GetDisplayName: string; override;
    public
        constructor Create(Collection: TCollection); override;
        procedure Clear;
        procedure Assign(AValue: TPersistent); override;
        function IsEqual(AValue: TOCIMacro): Boolean;
        property DataSet: TDataSet read GetDataSet;
        property AsString: String read GetAsString write SetAsString;
        property AsInteger: Integer read GetAsInteger write SetAsInteger;
        property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
        property AsDate: TDateTime read GetAsDate write SetAsDate;
        property AsFloat: Double read GetAsFloat write SetAsFloat;
        property IsNull: Boolean read GetIsNull;
        property SQL: String read GetSQL;
    published
        property Value: Variant read FValue write SetValue;
        property Name: String read FName write FName;
        property MacroType: TOCIMacroType read FMacroType write FMacroType
            default mtString;
        property DataType: TOCIMacroDataType read FDataType write SetDataType
            default mdUnknown;
    end;

    TOCIMacros = class(TCollection)
    private
        FOwner: TPersistent;
        function GetDataSet: TDataSet;
        function GetItem(AIndex: Integer): TOCIMacro;
        procedure SetItem(AIndex: Integer; AValue: TOCIMacro);
    protected
        function GetOwner: TPersistent; override;
        procedure AssignTo(ADest: TPersistent); override;
    public
        constructor Create(AOwner: TPersistent); {$IFDEF OCI_D4} overload; {$ENDIF}
        procedure Assign(AValue: TPersistent); override;
        procedure EndUpdate; {$IFDEF OCI_D4} override; {$ENDIF}
        procedure AssignValues(AValue: TOCIMacros);
        function IsEqual(Value: TOciMacros): Boolean;
        function MacroByName(const Value: string): TOciMacro;
        function FindMacro(const Value: string): TOciMacro;
        property DataSet: TDataSet read GetDataSet;
        property Items[AIndex: Integer]: TOCIMacro read GetItem write SetItem; default;
{$IFDEF OCI_D4}
        property UpdateCount;
{$ENDIF}        
    end;

implementation

Uses SysUtils, DBConsts, Graphics, NCOciDb, NCOciMsg, NCOciUtil
{$IFNDEF OCI_D4}
     , bdeconst
{$ENDIF}
{$IFDEF OCI_D6}
     , Variants
{$ENDIF}
     ;

type
    __TDataSet = class(TDataSet);

// -------------------------------------------------------------------
// -------------------------------------------------------------------
// TOciParam

constructor TOciParam.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FOCIVariable := TOCIVariable.Create(nil);
    FBound := False;
    if (DataSet <> nil) and (DataSet is TOCIDataSet) then
        with TOCIDataSet(DataSet) do begin
            FetchParams.TuneVar(FOCIVariable);
            DataFormat.TuneVar(FOCIVariable);
        end;
end;

destructor TOciParam.Destroy;
begin
    FOCIVariable.Free;
    FOCIVariable := nil;
    inherited Destroy;
end;

function TOciParam.IsEqual(Value: TOciParam): Boolean;
var
    i: Integer;
begin
    Result := (Name = Value.Name) and (DataType = Value.DataType) and
              (ParamType = Value.ParamType) and (Bound = Value.Bound) and
              (ArrayLen = Value.ArrayLen) and (IsPLSQLTable = Value.IsPLSQLTable) and
              (IsCaseSensitive = Value.IsCaseSensitive);
    if Result then
        for i := 0 to ArrayLen - 1 do begin
            try
                Result := (IsNulls[i] = Value.IsNulls[i]) and
                          (Values[i] = Value.Values[i]);
            except
                Result := False;
            end;
            if not Result then
                Break;
        end;
end;

function TOciParam.IsParamStored: Boolean;
begin
    Result := Bound and not IsNull;
end;

// Basic properties
function TOciParam.GetArrayLen: Integer;
begin
    Result := FOCIVariable.ArrayLen;
end;

procedure TOciParam.SetArrayLen(const Value: Integer);
begin
    FOCIVariable.ArrayLen := Value;
end;

procedure TOciParam.SetName(const Value: string);
begin
    OName := ':' + Value;
end;

function TOciParam.GetName: string;
begin
    Result := OName;
    Result := Copy(Result, 2, Length(Result));
end;

procedure TOciParam.SetOName(const Value: string);
begin
    FOCIVariable.Name := Value;
end;

function TOciParam.GetOName: string;
begin
    Result := FOCIVariable.Name;
end;

procedure TOciParam.SetParamType(const Value: TParamType);
begin
    FOCIVariable.VarType := pt2vt[Value];
end;

function TOciParam.GetParamType: TParamType;
var
    s: String;
begin
    Result := vt2pt[FOCIVariable.VarType];
    if (Result = ptOutput) then begin
        s := Name;
        if (SDefParRESULT = AnsiUpperCase(Copy(s, 1, SDefParRESULTLen))) and
           ((Length(s) = SDefParRESULTLen) or
            (Length(s) > SDefParRESULTLen) and (s[SDefParRESULTLen + 1] = SDefParMembDelim)) then
            Result := ptResult;
    end;
end;

procedure TOciParam.SetDataType(Value: TFieldType);
begin
    FOCIVariable.DataType := dt2ot[Value];
end;

function TOciParam.GetDataType: TFieldType;
begin
    Result := ot2dt[FOCIVariable.DataType];
end;

function TOciParam.GetODataType: TOCIVarDataType;
begin
    Result := FOCIVariable.DataType;
end;

procedure TOciParam.SetODataType(const Value: TOCIVarDataType);
begin
    FOCIVariable.DataType := Value;
end;

function TOciParam.GetOParamType: TOCIVarType;
begin
    Result := FOCIVariable.VarType;
end;

procedure TOciParam.SetOParamType(const Value: TOCIVarType);
begin
    FOCIVariable.VarType := Value;
end;

function TOciParam.GetDataSize: Integer;
begin
    Result := FOCIVariable.DataSize;
end;

procedure TOciParam.SetDataSize(AValue: Integer);
begin
    FOCIVariable.DataSize := AValue;
end;

function TOciParam.GetIsPLSQLTable: Boolean;
begin
    Result := FOCIVariable.IsPLSQLTable;
end;

procedure TOciParam.SetIsPLSQLTable(const Value: Boolean);
begin
    FOCIVariable.IsPLSQLTable := Value;
end;

function TOciParam.GetIsCaseSensitive: Boolean;
begin
    Result := FOCIVariable.IsCaseSensitive;
end;

procedure TOciParam.SetIsCaseSensitive(const Value: Boolean);
begin
    FOCIVariable.IsCaseSensitive := Value;
end;

procedure TOciParam.Assign(Source: TPersistent);

    procedure LoadFromBitmap(Bitmap: TBitmap);
    var
        MS: TMemoryStream;
    begin
        MS := TMemoryStream.Create;
        try
            Bitmap.SaveToStream(MS);
            LoadFromStream(MS, ftGraphic, 0);
        finally
            MS.Free;
        end;
    end;

    procedure LoadFromStrings(Source: TStrings);
    var
        i: Integer;
    begin
        if IsPLSQLTable then begin
            i := 0;
            if ArrayLen < Source.Count then
                ArrayLen := Source.Count;
            while (i < Source.Count) and (i < ArrayLen) do begin
                if Source[i] = '' then
                    IsNulls[i] := True
                else
                    AsStrings[i] := Source[i];
                Inc(i);
            end;
            while (i < ArrayLen) do begin
                IsNulls[i] := True;
                Inc(i);
            end;
        end
        else
            AsMemo := Source.Text;
    end;

begin
    if Source is TOciParam then
        AssignParam(TOciParam(Source))
    else if Source is TField then
        AssignField(TField(Source), -1)
    else if Source is TStrings then
        LoadFromStrings(TStrings(Source))
    else if Source is TBitmap then
        LoadFromBitmap(TBitmap(Source))
    else if (Source is TPicture) and (TPicture(Source).Graphic is TBitmap) then
        LoadFromBitmap(TBitmap(TPicture(Source).Graphic))
{$IFDEF OCI_D4}
    else if Source is TParam then
        AssignDelphiParam(TParam(Source))
{$ENDIF}        
    else if Source is TOCIStatementDataSet then
        AssignDataSet(TOCIStatementDataSet(Source))
    else
        inherited Assign(Source);
end;

class function TOciParam.CanAssignBlobObjects(ADest: TObject; ASrc: TObject): Boolean;

    function IsBlobObj(AObj: TObject): Boolean;
    begin
        Result :=
            (AObj is TField) and TField(AObj).IsBlob and
                (TField(AObj).DataSet <> nil) and (TField(AObj).DataSet is TOCIDataSet) or
            (AObj is TOciParam) and (TOciParam(AObj).ODataType in otAllBlobs);
    end;

begin
    Result := IsBlobObj(ADest) and IsBlobObj(ASrc);
end;

class procedure TOciParam.AssignBlobObjects(
    ADest: TObject; ADestIndex: Integer;
    ASrc: TObject; ASrcIndex: Integer);
var
    bsDest, bsSrc: TStream;
begin
    if ADest is TField then
        bsDest := TField(ADest).DataSet.CreateBlobStream(TField(ADest), bmWrite)
    else
        bsDest := TOciParam(ADest).CreateBlobStream(ADestIndex, bmWrite);
    try
        if ASrc is TField then
            bsSrc := TField(ASrc).DataSet.CreateBlobStream(TField(ASrc), bmRead)
        else
            bsSrc := TOciParam(ASrc).CreateBlobStream(ASrcIndex, bmRead);
        try
            if bsDest is TOCILongStream then
                TOCILongStream(bsDest).Assign(bsSrc)
            else if bsDest is TOCILOBStream then
                TOCILOBStream(bsDest).Assign(bsSrc);
        finally
            bsSrc.Free;
        end;
    finally
        bsDest.Free;
    end;
end;

function TOciParam.GetAssignIncludeBlobs: Boolean;
begin
    if not Assigned(Collection) then
        Result := False
    else
        Result := TOciParams(Collection).AssignIncludeBlobs;
end;

procedure TOciParam.AssignTo(Dest: TPersistent);

    procedure AssignStrings(Dest: TStrings);
    var
        i, j: Integer;
    begin
        if IsPLSQLTable then begin
            Dest.Clear;
            i := ArrayLen - 1;
            while (i >= 0) and IsNulls[i] do
                Dec(i);
            Dest.Capacity := i + 1;
            for j := 0 to i do
                Dest.Add(AsStrings[j]);
        end
        else
            Dest.Text := AsMemo;
    end;

    procedure AssignField(Dest: TField);
    begin
        if CanAssignBlobObjects(Dest, Self) then begin
            if AssignIncludeBlobs then
                AssignBlobObjects(Dest, 0, Self, 0);
        end
        else
            Dest.Value := Value;
    end;

{$IFDEF OCI_D4}
    procedure AssignDelphiParam(Dest: TParam);
    begin
        Dest.DataType := DataType;
        if IsNull then
            Dest.Clear
        else
            Dest.Value := Value;
        Dest.Bound := Bound;
        Dest.Name := Name;
        if Dest.ParamType = ptUnknown then
            Dest.ParamType := ParamType;
    end;
{$ENDIF}

begin
    if Dest is TField then
        AssignField(TField(Dest))
    else if Dest is TStrings then
        AssignStrings(TStrings(Dest))
{$IFDEF OCI_D4}
    else if Dest is TParam then
        AssignDelphiParam(TParam(Dest))
{$ENDIF}
    else
        inherited AssignTo(Dest);
end;

procedure TOciParam.InitUsingParam(Param: TOciParam);
begin
    ODataType := Param.ODataType;
    if OParamType = odUnknown then
        OParamType := Param.OParamType;
    OName := Param.OName;
    ODataSize := Param.ODataSize;
    ArrayLen := Param.ArrayLen;
    IsPLSQLTable := Param.IsPLSQLTable;
    IsCaseSensitive := Param.IsCaseSensitive;
    FBound := Param.Bound;
end;

procedure TOciParam.AssignParam(Param: TOciParam);
var
    buff: pUb1;
    sz: ub4;
    i: Integer;
    canAsBlobs: Boolean;
begin
    if Param <> nil then begin
        InitUsingParam(Param);
        if (OParamType in [odUnknown, odIn, odInOut]) and
           not (ODataType in [otUnknown, otCursor]) and
           ((ODataSize > 0) or (ODataType in otVBlobs)) and
           (ArrayLen > 0) then begin
            canAsBlobs := CanAssignBlobObjects(Self, Param);
            for i := 0 to ArrayLen - 1 do begin
                if canAsBlobs then begin
                    if AssignIncludeBlobs then
                        AssignBlobObjects(Self, i, Param, i);
                end
                else begin
                    Param.FOCIVariable.GetDataPtr(i, buff, sz);
                    FOCIVariable.SetData(i, buff, sz, dfOCI);
                end;
            end;
        end;
    end;
end;

{$IFDEF OCI_D4}
procedure TOciParam.AssignDelphiParam(Param: TParam);
begin
    if Param <> nil then begin
        DataType := Param.DataType;
        Name := Param.Name;
        if ParamType = ptUnknown then
            ParamType := Param.ParamType;
        if Param.IsNull then
            Clear
        else
            Value := Param.Value;
        Bound := Param.Bound;
    end;
end;
{$ENDIF}

procedure TOciParam.InitUsingField(Field: TField);
begin
    ArrayLen := 1;
{$IFDEF OCI_D4}
    if (Field.DataType = ftString) and TStringField(Field).FixedChar then begin
        DataType := ftFixedChar;
        ODataSize := Field.Size;
    end
    else
{$ENDIF}
    if (Field.DataType = ftMemo) and (Field.Size > 255) then begin
        DataType := ftString;
        ODataSize := Field.Size;
    end
    else if (Field.DataSet <> nil) and (Field.DataSet is TOCIDataSet) and
            (TOCIDataSet(Field.DataSet).Described) and (Field.FieldNo <> 0) then begin
        ODataType := TOCIDataSet(Field.DataSet).ODataType[Field.FieldNo];
        ODataSize := TOCIDataSet(Field.DataSet).ODataSize[Field.FieldNo];
    end
    else begin
        DataType := Field.DataType;
        if ODataType in [otROWID, otString, otChar, otRaw, otLong, otLongRaw] then
            ODataSize := Field.Size;
    end;
    Clear;
end;

procedure TOciParam.AssignFieldValue(Field: TField; const Value: Variant;
    AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
begin
    if Field <> nil then begin
        if AIndex = -1 then begin
            InitUsingField(Field);
            AIndex := 0;
        end;
        if VarIsEmpty(Value) then begin
            if CanAssignBlobObjects(Self, Field) then
                AssignBlobObjects(Self, AIndex, Field, 0)
            else
                Values[AIndex] := Field.Value;
        end
        else
            Values[AIndex] := Value;
        FBound := True;
    end;
end;

procedure TOciParam.AssignField(Field: TField;
    AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
begin
    if Field <> nil then begin
        AssignFieldValue(Field, Unassigned, AIndex);
        Name := Field.FieldName;
    end;
end;

procedure TOciParam.AssignByRef(ASource: TObject;
    AIndex: Integer {$IFDEF OCI_D4} = -1 {$ENDIF});
var
    fld: TField;
    ds: TOCIDataSet;
    fldDef: TOCIFieldDef;
    buff: pUb1;
    sz: ub4;
    recBuf: PChar;
    hndl: pOCIHandle;
begin
    if ASource is TField then begin
        fld := TField(ASource);
        if not (fld.DataSet is TOCIDataSet) then
            OCIDBError(msgOCICantAssignByRef, fld.DataSet);
        ds := TOCIDataSet(fld.DataSet);
        fldDef := ds.OFieldDefs.FindByNo(fld.FieldNo);
        if (fldDef = nil) or
           not (fldDef.DataType in [otCursor, otBLOB, otCLOB, otCFile, otBFile]) or
           not ds.GetActiveRecBuf(recBuf) then
            OCIDBError(msgOCICantAssignByRef, ds);
        if AIndex = -1 then begin
            InitUsingField(fld);
            AIndex := 0;
        end
        else if fldDef.DataType <> ODataType then
            OCIDBError(msgOCICantAssignByRef, ds);
        ds.HCursor.GetDataPtr(ds.GetEditBookmark(recBuf), fld.FieldNo, buff, sz);
        FOCIVariable.SetDataByRef(AIndex, buff, sz, dfOci);
    end
    else if ASource is TOCIParam then begin
        if not (TOCIParam(ASource).ODataType in [otCursor, otBLOB, otCLOB,
                                                 otCFile, otBFile]) then
            OCIDBError(msgOCICantAssignByRef, nil);
        if AIndex = -1 then begin
            InitUsingParam(TOCIParam(ASource));
            AIndex := 0;
        end
        else if TOCIParam(ASource).ODataType <> ODataType then
            OCIDBError(msgOCICantAssignByRef, nil);
        TOCIParam(ASource).FOCIVariable.GetDataPtr(AIndex, buff, sz);
        FOCIVariable.SetDataByRef(AIndex, buff, sz, dfOci);
    end
    else if ASource is TOCILOBStream then begin
        if not ((TOCIParam(ASource).ODataType in [otBLOB, otCLOB]) and
                (ASource is TOCIILOBStream) or
                (TOCIParam(ASource).ODataType in [otCFile, otBFile]) and
                (ASource is TOCIFILEStream)) then
            OCIDBError(msgOCICantAssignByRef, nil);
        hndl := TOCILOBStream(ASource).HLocator.Handle;
        FOCIVariable.SetDataByRef(AIndex, @hndl, sizeof(pOCILobLocator), dfOci);
    end
    else
        OCIDBError(msgOCICantAssignByRef, nil);
end;

procedure TOciParam.AssignDataSet(DataSet: TDataSet);
begin
    ODataType := otCursor;
    (DataSet as TOCIStatementDataSet).Assign(Self);
end;

procedure TOciParam.LoadFromFile(const FileName: string; BlobType: TFieldType;
    AIndex: Integer {$IFDEF OCI_D4} = 0 {$ENDIF});
var
    Stream: TStream;
begin
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
        LoadFromStream(Stream, BlobType, AIndex);
    finally
        Stream.Free;
    end;
end;

procedure TOciParam.LoadFromStream(Stream: TStream; BlobType: TFieldType;
    AIndex: Integer {$IFDEF OCI_D4} = 0 {$ENDIF});
var
    DataStr: string;
    len: Integer;
    pBuff: Pointer;
    ilobStr: TOCIILOBStream;
begin
    with Stream do begin
        DataType := BlobType;
        Position := 0;
        Len := Size;
        if FOCIVariable.DataType in [otCLOB, otBLOB] then begin
            FBound := True;
            ilobStr := TOCIILOBStream.CreateUsingParam(Self, AIndex, bmWrite);
            try
                ilobStr.CopyFrom(Stream, 0);
            finally
                ilobStr.Free;
            end;
        end
        else if FOCIVariable.LongData then begin
            FBound := True;
            GetMem(pBuff, len);
            try
                ReadBuffer(pBuff^, len);
                FOCIVariable.SetData(AIndex, pBuff, len, dfDelphiOwned);
            except
                FreeMem(pBuff);
            end;
        end
        else begin
            SetLength(DataStr, len);
            ReadBuffer(PChar(DataStr)^, len);
            Values[AIndex] := DataStr;
        end;
    end;
end;

procedure TOciParam.Clear;
var
    i: Integer;
begin
    for i := 0 to ArrayLen - 1 do
        FOCIVariable.SetData(i, nil, 0, dfDelphi);
end;

function TOciParam.GetDataSet: TDataSet;
begin
    if not Assigned(Collection) then
        Result := nil
    else
        Result := TOciParams(Collection).GetDataSet;
end;

function TOciParam.GetDisplayName: string;
begin
    if Name = '' then
        Result := inherited GetDisplayName
    else
        Result := Name;
end;

// Value related properties
function TOciParam.GetIsNull: Boolean;
begin
    Result := IsNulls[0];
end;

procedure TOciParam.SetIsNull(const Value: Boolean);
begin
    IsNulls[0] := Value;
end;

procedure TOciParam.SetIsNulls(AIndex: Integer; const Value: Boolean);
begin
    if Value then
        FOCIVariable.SetData(AIndex, nil, 0, dfDelphi);
end;

function TOciParam.GetIsNulls(AIndex: Integer): Boolean;
var
    buff: pUb1;
    sz: ub4;
begin
    Result := (ODataType = otUnknown) or
        (ODataType in otHTypes) and (DataSet <> nil) and (csDesigning in DataSet.ComponentState) or
        not FOCIVariable.GetDataPtr(AIndex, buff, sz);
end;

function TOciParam.GetOwned(AIndex: Integer): Boolean;
begin
    Result := FOCIVariable.GetIsOwned(AIndex);
end;

procedure TOciParam.SetAsFloats(AIndex: Integer; const Value: Double);
begin
    SetData(AIndex, @Value, SizeOf(Double), otFloat);
end;

procedure TOciParam.SetAsFloat(const Value: Double);
begin
    AsFloats[0] := Value;
end;

function TOciParam.GetAsFloats(AIndex: Integer): Double;
begin
    if IsNulls[AIndex] then
        Result := 0.0
    else
        Result := Values[AIndex];
end;

function TOciParam.GetAsFloat: Double;
begin
    Result := AsFloats[0];
end;

procedure TOciParam.SetAsBCDs(AIndex: Integer; const Value: Currency);
var
    V: Double;
begin
    V := Value;
    SetData(AIndex, @V, SizeOf(Double), otBCD);
end;

procedure TOciParam.SetAsBCD(const Value: Currency);
begin
    AsBCDs[0] := Value;
end;

function TOciParam.GetAsBCDs(AIndex: Integer): Currency;
begin
    if IsNulls[AIndex] then
        Result := 0
    else
        Result := Values[AIndex];
end;

function TOciParam.GetAsBCD: Currency;
begin
    Result := AsBCDs[0];
end;

procedure TOciParam.SetAsIntegers(AIndex: Integer; Value: Longint);
begin
    SetData(AIndex, @Value, SizeOf(Integer), otInteger);
end;

procedure TOciParam.SetAsInteger(Value: Longint);
begin
    AsIntegers[0] := Value;
end;

function TOciParam.GetAsIntegers(AIndex: Integer): Longint;
begin
    if IsNulls[AIndex] then
        Result := 0
    else
        Result := Values[AIndex];
end;

function TOciParam.GetAsInteger: Longint;
begin
    Result := AsIntegers[0];
end;

procedure TOciParam.SetAsSmallInts(AIndex: Integer; Value: LongInt);
begin
    SetData(AIndex, @Value, SizeOf(Integer), otSmallInt);
end;

procedure TOciParam.SetAsSmallInt(Value: LongInt);
begin
    AsSmallInts[0] := Value;
end;

procedure TOciParam.SetAsStrings(AIndex: Integer; const Value: string);
begin
    if ODataType in [otString, otChar] then
        SetData(AIndex, pUb1(PChar(Value)), Length(Value), ODataType)
    else
        SetData(AIndex, pUb1(PChar(Value)), Length(Value), otString);
end;

procedure TOciParam.SetAsString(const Value: string);
begin
    AsStrings[0] := Value;
end;

function TOciParam.GetAsStrings(AIndex: Integer): string;
begin
    if IsNulls[AIndex] then
        Result := ''
    else
        Result := Values[AIndex];
end;

function TOciParam.GetAsString: string;
begin
    Result := AsStrings[0];
end;

procedure TOciParam.SetAsChars(AIndex: Integer; const Value: string);
var
    S: String;
begin
    if ODataType in [otString, otChar] then
        SetData(AIndex, pUb1(PChar(S)), Length(S), ODataType)
    else
        SetData(AIndex, pUb1(PChar(Value)), Length(Value), otChar);
end;

procedure TOciParam.SetText(const Value: string);
begin
    Texts[0] := Value;
end;

procedure TOciParam.SetTexts(AIndex: Integer; const Value: string);
begin
    Values[AIndex] := Value;
end;

procedure TOciParam.SetAsDateTimes(AIndex: Integer; const Value: TDateTime);
begin
    SetData(AIndex, @Value, SizeOf(TDateTime), otDateTime);
end;

procedure TOciParam.SetAsDateTime(const Value: TDateTime);
begin
    AsDateTimes[0] := Value;
end;

function TOciParam.GetAsDateTimes(AIndex: Integer): TDateTime;
begin
    if IsNulls[AIndex] then
        Result := 0
    else
        Result := VarToDateTime(Values[AIndex]);
end;

function TOciParam.GetAsDateTime: TDateTime;
begin
    Result := AsDateTimes[0];
end;

procedure TOciParam.SetAsMemos(AIndex: Integer; const Value: string);
begin
    SetData(AIndex, pUb1(PChar(Value)), Length(Value), otLong);
end;

procedure TOciParam.SetAsMemo(const Value: string);
begin
    AsMemos[0] := Value;
end;

function TOciParam.GetAsMemos(AIndex: Integer): string;
begin
    if IsNulls[AIndex] then
        Result := ''
    else
        Result := Values[AIndex];
end;

function TOciParam.GetAsMemo: string;
begin
    Result := AsMemos[0];
end;

procedure TOciParam.SetAsBlobs(AIndex: Integer; const Value: TBlobData);
begin
    SetData(AIndex, pUb1(PChar(Value)), Length(Value), otLongRaw);
end;

procedure TOciParam.SetAsBlob(const Value: TBlobData);
begin
    AsBlobs[0] := Value;
end;

function TOciParam.GetAsBoolean: Boolean;
begin
    Result := AsBooleans[0];
end;

function TOciParam.GetAsBooleans(AIndex: Integer): Boolean;
begin
    if IsNulls[AIndex] or not Assigned(DataSet) then
        Result := False
    else
        Result := TOCIDataSet(DataSet).DataFormat.Value2OBool(Values[AIndex]);
end;

procedure TOciParam.SetAsBoolean(const Value: Boolean);
begin
    AsBooleans[0] := Value;
end;

procedure TOciParam.SetAsBooleans(AIndex: Integer; const Value: Boolean);
begin
    if Assigned(DataSet) then
        Values[AIndex] := TOCIDataSet(DataSet).DataFormat.OBool2Value(Value);
end;

function TOciParam.GetAsHandle: pOCIHandle;
begin
    Result := AsHandles[0];
end;

function TOciParam.GetAsHandles(AIndex: Integer): pOCIHandle;
var
    sz: ub4;
begin
    if not (ODataType in otHTypes) then
        OCIDBError(msgOCIParamIsNotHandle, nil);
    FOCIVariable.GetData(AIndex, @Result, sz, dfDelphi);
end;

function TOciParam.GetAsVariant: Variant;
begin
    Result := Values[0];
end;

function TOciParam.GetAsVariants(AIndex: Integer): Variant;
var
    pData: pUb1;
    sz: ub4;
    s: String;
    p: pUb1;
begin
    if IsNulls[AIndex] then
        Result := Null
    else begin
        sz := 0;
        pData := nil;
        Result := Unassigned;
        case ODataType of
            otSmallInt, otInteger:
                begin
                    TVarData(Result).Vtype := varInteger;
                    pData := @TVarData(Result).VInteger;
                end;
            otFloat, otBCD:
                begin
                    TVarData(Result).Vtype := varDouble;
                    pData := @TVarData(Result).VDouble;
                end;
            otDateTime:
                begin
                    TVarData(Result).Vtype := varDate;
                    pData := @TVarData(Result).VDate;
                end;
            otString, otChar, otLong, otROWID, otRaw, otLongRaw:
                begin
                    FOCIVariable.GetDataPtr(AIndex, p, sz);
                    SetLength(s, sz);
                    pData := pUb1(PChar(s));
                    Result := s;
                end;
            otNumber:
                begin
                    FOCIVariable.GetDataPtr(AIndex, p, sz);
                    SetLength(s, sz);
                    pData := pUb1(PChar(s));
                    Result := s;
                end;
            otCursor, otCLOB, otBLOB, otCFile, otBFile:
                OCIDBError(msgOCIComplexType, nil);
            otUnknown:;
        end;
        if pData <> nil then
            FOCIVariable.GetData(AIndex, pData, sz, dfDelphi);
    end;
end;

procedure TOciParam.SetAsVariant(const Value: Variant);
begin
    Values[0] := Value;
end;

procedure TOciParam.SetAsVariants(AIndex: Integer; const Value: Variant);
var
    V: Variant;
    sz: sb4;
    isE, isN: Boolean;
    ot: Integer;
    t: TOCIVarDataType;
    l: Integer;
    d: Double;
    s: String;
    c: Currency;
    pData: pUb1;
begin
    sz := 0;
    t := otUnknown;
    pData := nil;
    V := Value;
    isE := VarIsEmpty(V);
    isN := VarIsNull(V);
    if (ODataType <> otUnknown) and not isE and not isN then begin
        if ODataType in [otCursor, otCLOB, otBLOB, otCFile, otBFile] then
            OCIDBError(msgOCIComplexType, nil);
        ot := ot2var[ODataType];
        if ot = varEmpty then
            OCIDBError(msgOCIBadParamVal, nil);
        V := VarAsType(V, ot);
    end;
    case VarType(V) of
        varSmallint:
            begin
                t := otSmallInt; l := V; pData := @l;
            end;
        varByte:
            begin
                t := otSmallInt; l := V; pData := @l;
            end;
        varInteger:
            begin
                t := otInteger; pData := @TVarData(V).VInteger;
            end;
        varCurrency:
            begin
                t := otBCD; c := V; pData := @c;
            end;
        varSingle:
            begin
                t := otBCD; d := V; pData := @d;
            end;
        varDouble:
            begin
                t := otFloat; pData := @TVarData(V).VDouble;
            end;
        varDate:
            begin
                t := otDateTime; pData := @TVarData(V).VDate;
            end;
        varString, varOleStr:
            begin
                t := otString;
                s := V;
                pData := pUb1(PChar(s));
                sz := Length(s);
            end;
        varBoolean:
            begin
                if Assigned(DataSet) then
                    SetAsVariants(AIndex, (DataSet as TOCIDataSet).DataFormat.OBool2Value(V))
                else
                    OCIDBError(msgOCIBadParamVal, nil);
            end;
        varNull, varEmpty: ;
        else
            OCIDBError(msgOCIBadParamVal, nil);
    end;
    FBound := not isE;
    if isE or isN then begin
        if not (ODataType in [otUnknown, otCLOB, otBLOB, otCFile, otBFile, otCursor]) then
            FOCIVariable.SetData(AIndex, nil, 0, dfDelphi);
    end
    else begin
        if ODataType = otUnknown then
            ODataType := t;
        FOCIVariable.SetData(AIndex, pData, sz, dfDelphi);
    end;
end;

function TOciParam.CreateBlobStream(AIndex: Integer; Mode: TBlobStreamMode): TStream;
begin
    Result := nil;
    case ODataType of
    otROWID, otString, otChar, otRaw, otLong, otLongRaw:
        Result := TOCILongStream.CreateUsingParam(Self, AIndex, Mode);
    otCLOB, otBLOB:
        Result := TOCIILOBStream.CreateUsingParam(Self, AIndex, Mode);
    otCFile, otBFile:
        Result := TOCIFILEStream.CreateUsingParam(Self, AIndex, Mode);
    else
        OCIDBErrorFmt(msgOCIBlobReadOnly, ['Param "' + OName + '"'], nil);
    end;
end;

procedure TOciParam.BlobModified(AIndex: Integer; AIsNull: Boolean);
begin
    FOCIVariable.SetIsNull(AIndex, AIsNull);
end;

procedure TOciParam.SetData(AIndex: Integer; AData: pUb1; ALen: sb4; AType: TOCIVarDataType);
begin
    FBound := True;
    with FOCIVariable do begin
        DataType := AType;
        SetData(AIndex, AData, ALen, dfDelphi);
    end;
end;

// -------------------------------------------------------------------
// -------------------------------------------------------------------
// TOciParams

{$IFDEF OCI_D4}
constructor TOciParams.Create;
begin
    FOwner := nil;
    inherited Create(TOciParam);
    FBindMode := pbByName;
end;
{$ENDIF}

constructor TOciParams.Create(Owner: TPersistent);
begin
    FOwner := Owner;
    inherited Create(TOciParam);
    FBindMode := pbByName;
end;

function TOciParams.GetItem(Index: Integer): TOciParam;
begin
    Result := TOciParam(inherited Items[Index]);
end;

procedure TOciParams.SetItem(Index: Integer; Value: TOciParam);
begin
    inherited SetItem(Index, TCollectionItem(Value));
end;

function TOciParams.GetOwner: TPersistent;
begin
    Result := FOwner;
end;

function TOciParams.GetDataSet: TDataSet;
begin
    if (FOwner <> nil) and (FOwner is TDataSet) then
        Result := TDataSet(FOwner)
    else
        Result := nil;
end;

procedure TOciParams.AssignValues(Value: TOciParams);
var
    I: Integer;
    P: TOciParam;
begin
    for I := 0 to Value.Count - 1 do begin
        P := FindParam(Value[I].Name);
        if P <> nil then
            P.Assign(Value[I]);
    end;
end;

function TOciParams.Add: TOciParam;
begin
    Result := TOciParam(inherited Add);
end;

procedure TOciParams.AddParam(Value: TOciParam);
begin
    Value.Collection := Self;
end;

procedure TOciParams.RemoveParam(Value: TOciParam);
begin
    Value.Collection := nil;
end;

function TOciParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TOciParam;
begin
    Result := Add as TOciParam;
    Result.ParamType := ParamType;
    Result.Name := ParamName;
    Result.DataType := FldType;
end;

function TOciParams.CreateOParam(OFldType: TOCIVarDataType; ODataSize: Integer;
    const OParamName: string; OParamType: TOCIVarType; OIsPLSQLTable: Boolean;
    OIsCaseSensitive: Boolean): TOciParam;
begin
    Result := Add as TOciParam;
    Result.OParamType := OParamType;
    Result.ODataSize := ODataSize;
    Result.OName := OParamName;
    Result.ODataType := OFldType;
    Result.IsPLSQLTable := OIsPLSQLTable;
    Result.IsCaseSensitive := OIsCaseSensitive;
end;

function TOciParams.IsEqual(Value: TOciParams): Boolean;
var
    I: Integer;
begin
    Result := Count = Value.Count;
    if Result then
        for I := 0 to Count - 1 do begin
            Result := Items[I].IsEqual(Value.Items[I]);
            if not Result then
                Break;
        end
end;

function TOciParams.ParamByName(const Value: string): TOciParam;
begin
    Result := FindParam(Value);
    if Result = nil then
        DatabaseErrorFmt(SParameterNotFound, [Value] {$IFDEF OCI_D4}, GetDataSet {$ENDIF});
end;

function TOciParams.FindParam(const Value: string): TOciParam;
var
    I: Integer;
begin
    for I := 0 to Count - 1 do begin
        Result := Items[I];
        if AnsiCompareText(Result.Name, Value) = 0 then
            Exit;
    end;
    Result := nil;
end;

procedure TOciParams.DefineProperties(Filer: TFiler);
begin
    inherited DefineProperties(Filer);
    Filer.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

procedure TOciParams.ReadBinaryData(Stream: TStream);
var
    I, Temp, NumItems: Integer;
    Buffer: array[0..2047] of Char;
    TempStr: string;
    Version: Word;
    Bool: Boolean;
    PT: TParamType;
    DT: TFieldType;
begin
    Clear;
    with Stream do begin
        ReadBuffer(Version, SizeOf(Version));
        if Version > 2 then
            DatabaseError(SInvalidVersion);
        NumItems := 0;
        if Version = 2 then
            ReadBuffer(NumItems, SizeOf(NumItems))
        else
            ReadBuffer(NumItems, 2);
        for I := 0 to NumItems - 1 do
            with TOciParam(Add) do begin
                Temp := 0;
                if Version = 2 then
                    ReadBuffer(Temp, SizeOf(Temp))
                else
                    ReadBuffer(Temp, 1);
                SetLength(TempStr, Temp);
                ReadBuffer(PChar(TempStr)^, Temp);
                Name := TempStr;
                ReadBuffer(PT, SizeOf(PT));
                ReadBuffer(DT, SizeOf(DT));
                ParamType := PT;
                DataType := DT;
                if DataType <> ftUnknown then begin
                    Temp := 0;
                    if Version = 2 then
                        ReadBuffer(Temp, SizeOf(Temp))
                    else
                        ReadBuffer(Temp, 2);
                    ReadBuffer(Buffer, Temp);
                    FOCIVariable.SetData(0, @Buffer, Temp, dfDelphi);
                end;
                ReadBuffer(Bool, SizeOf(Bool));
                if Bool then
                    FOCIVariable.SetData(0, nil, 0, dfDelphi);
                ReadBuffer(FBound, SizeOf(FBound));
            end;
    end;
end;

function TOciParams.GetParamValue(const ParamName: string): Variant;
var
    I: Integer;
    Params: TList;
begin
    if Pos(';', ParamName) <> 0 then begin
        Params := TList.Create;
        try
            GetParamList(Params, ParamName);
            Result := VarArrayCreate([0, Params.Count - 1], varVariant);
            for I := 0 to Params.Count - 1 do
                Result[I] := TOciParam(Params[I]).Value;
        finally
            Params.Free;
        end;
    end
    else
        Result := ParamByName(ParamName).Value
end;

procedure TOciParams.SetParamValue(const ParamName: string;
  const Value: Variant);
var
    I: Integer;
    Params: TList;
begin
    if Pos(';', ParamName) <> 0 then begin
        Params := TList.Create;
        try
            GetParamList(Params, ParamName);
            for I := 0 to Params.Count - 1 do
                TOciParam(Params[I]).Value := Value[I];
        finally
            Params.Free;
        end;
    end
    else
        ParamByName(ParamName).Value := Value;
end;

procedure TOciParams.GetParamList(List: TList; const ParamNames: string);
var
    Pos: Integer;
begin
    Pos := 1;
    while Pos <= Length(ParamNames) do
        List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

procedure TOciParams.Bind(Astatement: TOCIStatement; AValue: Boolean);
var
    i: Integer;
begin
    for i := 0 to Count - 1 do
        with Items[i].HVariable do
            if AValue then begin
                if ParamBindMode = pbByNumber then
                    Position := i + 1
                else
                    Position := 0;
                BindTo(AStatement);
            end
            else
                BindOff;
end;

procedure TOciParams.SetValuesExt(const AParams: String; const AValue: Variant;
    const AParamsPrefix: String);
var
    i, j: Integer;
    p: TOCIParam;
begin
    i := 1;
    j := 0;
    while i <= Length(AParams) do begin
        p := FindParam(AParamsPrefix + ExtractFieldName(AParams, i));
        if p <> nil then
            if VarIsArray(AValue) then
                p.Value := AValue[j]
            else
                p.Value := AValue;
        Inc(j);
    end;
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIMacro

constructor TOCIMacro.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FName := '';
    FValue := Null;
    FMacroType := mtString;
    FDataType := mdUnknown;
end;

function TOCIMacro.GetSQL: String;
var
    df: TOCIDataFormat;
begin
    if MacroType = mtString then
        Result := AsString
    else begin
        if DataSet = nil then
            df := nil
        else
            df := (DataSet as TOCIDataSet).DataFormat;
        Result := Var2SQLTyped(FValue, DataType, df);
    end;
end;

function TOCIMacro.GetDisplayName: string;
begin
    if Name <> '' then
        Result := Name
    else
        Result := inherited GetDisplayName;
end;

function TOCIMacro.GetDataSet: TDataSet;
begin
    if not Assigned(Collection) then
        Result := nil
    else
        Result := TOciMacros(Collection).GetDataSet;
end;

function TOCIMacro.IsEqual(AValue: TOCIMacro): Boolean;
begin
    Result := (Name = AValue.Name) and (MacroType = AValue.MacroType) and
              (DataType = AValue.DataType);
    if Result then
        try
            Result := Value = AValue.Value;
        except
            Result := False;
        end;
end;

procedure TOCIMacro.Assign(AValue: TPersistent);
begin
    if AValue is TOciMacro then
        try
            if Collection <> nil then
                Collection.BeginUpdate;
            MacroType := TOciMacro(AValue).MacroType;
            DataType := TOciMacro(AValue).DataType;
            Name := TOciMacro(AValue).Name;
            Value := TOciMacro(AValue).Value;
        finally
            if Collection <> nil then
                Collection.EndUpdate;
        end
    else
        inherited Assign(AValue);
end;

procedure TOCIMacro.Changed;
begin
    if {$IFDEF OCI_D4} (TOCIMacros(Collection).UpdateCount = 0) and {$ENDIF}
       (DataSet <> nil) and (DataSet is TOCICustomQuery) then
        TOCICustomQuery(DataSet).MacrosChanged;
end;

procedure TOCIMacro.SetDataType(const AValue: TOCIMacroDataType);
begin
    if FDataType <> AValue then begin
        FDataType := AValue;
        Changed;
    end;
end;

procedure TOCIMacro.SetData(const AValue: Variant; AType: TOCIMacroDataType);
begin
    if Value <> AValue then begin
        FValue := AValue;
        if AType = mdUnknown then
            FDataType := VarType2FieldType(AValue)
        else
            FDataType := AType;
        Changed;
    end;
end;

function TOCIMacro.GetIsNull: Boolean;
begin
    Result := VarIsNull(FValue) or
              VarIsEmpty(FValue);
end;

procedure TOCIMacro.Clear;
begin
    SetData(Null, mdUnknown);
end;

procedure TOCIMacro.SetValue(const AValue: Variant);
begin
    SetData(AValue, mdUnknown);
end;

function TOCIMacro.GetAsString: String;
begin
    if IsNull then
        Result := ''
    else
        Result := FValue;
end;

procedure TOCIMacro.SetAsString(const AValue: String);
begin
    SetData(AValue, mdString);
end;

function TOCIMacro.GetAsInteger: Integer;
begin
    if IsNull then
        Result := 0
    else
        Result := FValue;
end;

procedure TOCIMacro.SetAsInteger(const AValue: Integer);
begin
    SetData(AValue, mdInteger);
end;

function TOCIMacro.GetAsDateTime: TDateTime;
begin
    if IsNull then
        Result := 0.0
    else
        Result := FValue;
end;

procedure TOCIMacro.SetAsDateTime(const AValue: TDateTime);
begin
    SetData(AValue, mdDateTime);
end;

function TOCIMacro.GetAsFloat: Double;
begin
    if IsNull then
        Result := 0.0
    else
        Result := FValue;
end;

procedure TOCIMacro.SetAsFloat(const AValue: Double);
begin
    SetData(AValue, mdFloat);
end;

function TOCIMacro.GetAsDate: TDateTime;
begin
    if IsNull then
        Result := 0.0
    else
        Result := FValue;
end;

procedure TOCIMacro.SetAsDate(const AValue: TDateTime);
var
    dt: TDateTime;
begin
    dt := Trunc(AValue);
    SetData(dt, mdDate);
end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// TOCIMacros

constructor TOCIMacros.Create(AOwner: TPersistent);
begin
    FOwner := AOwner;
    inherited Create(TOciMacro);
end;

procedure TOCIMacros.Assign(AValue: TPersistent);
var
    i, j: Integer;
    s: String;
begin
    if AValue is TStrings then begin
        BeginUpdate;
        try
            Clear;
            for i := 0 to TStrings(AValue).Count - 1 do
                with TOCIMacro(Add) do begin
                    s := TStrings(AValue)[i];
                    j := Pos('=', s);
                    Name := Copy(s, 1, j - 1);
                    Value := Copy(s, j + 1, Length(s));
                end;
        finally
            EndUpdate;
        end;
    end
    else
        inherited Assign(AValue);
end;

procedure TOCIMacros.AssignTo(ADest: TPersistent);
var
    i: Integer;
begin
    if ADest is TStrings then begin
        TStrings(ADest).Clear;
        for i := 0 to Count - 1 do
            TStrings(ADest).Add(Items[i].Name + '=' + Items[i].Value);
    end
    else
        inherited AssignTo(ADest);
end;

procedure TOCIMacros.AssignValues(AValue: TOCIMacros);
var
    I: Integer;
    M: TOciMacro;
begin
    BeginUpdate;
    try
        for I := 0 to AValue.Count - 1 do begin
            M := FindMacro(AValue[I].Name);
            if M <> nil then
                M.Assign(AValue[I]);
        end;
    finally
        EndUpdate;
    end;
end;

function TOCIMacros.IsEqual(Value: TOciMacros): Boolean;
var
    I: Integer;
begin
    Result := Count = Value.Count;
    if Result then
        for I := 0 to Count - 1 do begin
            Result := Items[I].IsEqual(Value.Items[I]);
            if not Result then
                Break;
        end
end;

function TOCIMacros.FindMacro(const Value: string): TOciMacro;
var
    I: Integer;
begin
    for I := 0 to Count - 1 do begin
        Result := Items[I];
        if AnsiCompareText(Result.Name, Value) = 0 then
            Exit;
    end;
    Result := nil;
end;

function TOCIMacros.MacroByName(const Value: string): TOciMacro;
begin
    Result := FindMacro(Value);
    if Result = nil then
        OCIDBErrorFmt(msgOCIMacroNotFound, [Value], nil);
end;

function TOCIMacros.GetDataSet: TDataSet;
begin
    if (FOwner <> nil) and (FOwner is TDataSet) then
        Result := TDataSet(FOwner)
    else
        Result := nil;
end;

function TOCIMacros.GetItem(AIndex: Integer): TOCIMacro;
begin
    Result := TOCIMacro(inherited Items[AIndex]);
end;

procedure TOCIMacros.SetItem(AIndex: Integer; AValue: TOCIMacro);
begin
    inherited Items[AIndex] := AValue;
end;

function TOCIMacros.GetOwner: TPersistent;
begin
    Result := FOwner;
end;

procedure TOCIMacros.EndUpdate;
begin
    inherited EndUpdate;
    if {$IFDEF OCI_D4} (UpdateCount = 0) and {$ENDIF}
       (DataSet <> nil) and (DataSet is TOCICustomQuery) then
        TOCICustomQuery(DataSet).MacrosChanged;
end;

end.
