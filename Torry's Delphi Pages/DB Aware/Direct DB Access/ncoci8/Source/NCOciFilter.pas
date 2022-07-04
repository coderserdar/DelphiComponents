{*******************************************************}
{File:      NCOciFilter.PAS                             }
{Revision:  0.04.06 / 19.10.2000                        }
{Comment:   NC OCI8 VCL: SQL expressions, filters       }
{Copyright: *** Most                                    }
{           (c) 1999-2001, Dmitry Arefiev               }
{           *** Portions:                               }
{           (c) 1999, Inprise Corporation               }
{           (c) 1999, InThink Corporation               }
{Author:    - Dmitry Arefiev, darefiev@da-soft.com      }
{           - Based on Borland Delphi DBCommon.pas      }
{           - Semi-natural language designed by InThink }
{             corporation                               }
{*******************************************************}
{$I NCOciDef.inc}
{$DEFINE OCI_DEBUG}
{*$DEFINE OCI_EXPRESSION_STANDALONE}

{TODO
1. TOCIExpressionNode.FDataType currently does not used in Execute,
   but we must for Oracle like type casting
2. Current implementation use VARIANT for run time,
   it is 1.3 times slower than BDE
3. Filter translation into server Oracle SQL
4. More error condition checking
TODO}

unit NCOciFilter;

interface

Uses Classes, DB;

type
    TOCIExpressionToken = (etEnd, etSymbol, etName, etLiteral,  etLParen,
        etRParen, etEQ, etNE, etGE, etLE, etGT, etLT, etCONCAT, etADD,
        etSUB, etMUL, etDIV, etComma, etLIKE, etISNULL, etISNOTNULL, etIN,
        etNOTIN, etANY, etSOME, etALL, etNOTLIKE, etBETWEEN, etNOTBETWEEN,
        etFIRST, etLAST, etNEXT, etPRIOR, etTHIS, etABS, etDAY, etMONTH,
        etQUARTER, etYEAR, etOF, etEMBEDDING, etNULL);

    TOCIExpressionOperator = (canNOTDEFINED, canASSIGN, canOR, canAND, canNOT,
        canEQ, canNE, canGE, canLE, canGT, canLT, canLIKE, canISBLANK, canNOTBLANK,
        canIN, canCONCAT, canADD, canSUB, canMUL, canDIV, canNOTIN, canANY,
        canALL, canNOTLIKE, canBETWEEN, canNOTBETWEEN);

    TOCIExpressionOption = (poExtSyntax, poAggregate, poDefaultExpr,
        poFieldNameGiven, poNaturalLang);
    TOCIExpressionOptions = set of TOCIExpressionOption;

    TOCIExpressionNodeKind = (enUnknown, enField, enConst, enOperator, enFunc);
    TOCIExpressionScopeKind = (skUnknown, skField, skAgg, skConst);

    POCIExpressionNode = ^TOCIExpressionNode;
    TOCIExpressionNode = record
        FNext: POCIExpressionNode;
        FLeft: POCIExpressionNode;
        FRight: POCIExpressionNode;
        FDataType: TFieldType;
        FScopeKind: TOCIExpressionScopeKind;
        FKind: TOCIExpressionNodeKind;
        FOperator: TOCIExpressionOperator;
        FData: Variant;
        FFuncInd: Integer;
        FField: TField;
        FArgs: TList;
        FPartial: Boolean;
    end;

    TOCIExpression = class
    private
        FRoot, FNodes: POCIExpressionNode;
        FDataSet: TDataSet;
        FOptions: TFilterOptions;
        FFieldName: String;
        FParserOptions: TOCIExpressionOptions;
        function NewNode(Kind: TOCIExpressionNodeKind; Operator: TOCIExpressionOperator;
            const Data: Variant; Left, Right: POCIExpressionNode; AFuncInd: Integer): POCIExpressionNode;
        procedure ClearNodes;
    public
        constructor Create(DataSet: TDataSet; Options: TFilterOptions;
            ParseOptions: TOCIExpressionOptions; const FieldName: string);
        destructor Destroy; override;
        function NewCompareNode(Field: TField; Operator: TOCIExpressionOperator;
            const Value: Variant; APartial: Boolean): POCIExpressionNode;
        procedure Dump(AStrs: TStrings);
        function Execute: Variant;
        property DataSet: TDataSet read FDataSet;
        property Options: TFilterOptions read FOptions;
    end;

    TOCIExprParserBmk = record
        FSourcePtr: PChar;
        FToken: TOCIExpressionToken;
        FTokenString: String;
    end;

    TOCIExpressionParser = class
    private
        FExpression: TOCIExpression;
        FText: String;
        FSourcePtr: PChar;
        FTokenPtr: PChar;
        FTokenString: String;
        FUCTokenString: String;
        FToken: TOCIExpressionToken;
        FNumericLit: Boolean;
        FTokenNumber: Double;
        FParserOptions: TOCIExpressionOptions;
        FDataSet: TDataSet;
        FInSQLLang: Boolean;
        procedure NextToken;
        function NextTokenIsLParen : Boolean;
        function ParseExpr: POCIExpressionNode;
        function ParseExpr2: POCIExpressionNode;
        function ParseExpr3: POCIExpressionNode;
        function ParseExpr4: POCIExpressionNode;
        function ParseExprNatural(AEnableList: Boolean): POCIExpressionNode;
        function ParseExpr5(AEnableList: Boolean): POCIExpressionNode;
        function ParseExpr6: POCIExpressionNode;
        function ParseExpr7: POCIExpressionNode;
        function TokenName: string;
        function TokenSymbolIs(const S: string): Boolean;
        function TokenSymbolIsFunc(const S: string): Integer;
        procedure GetFuncResultInfo(Node: POCIExpressionNode);
        procedure TypeCheckArithOp(Node: POCIExpressionNode);
        procedure GetScopeKind(Root, Left, Right : POCIExpressionNode);
        procedure SaveState(var ABmk: TOCIExprParserBmk);
        procedure RestoreState(var ABmk: TOCIExprParserBmk);
    public
        function BuildExpression(ADataSet: TDataSet; const Text: string;
            Options: TFilterOptions; ParserOptions: TOCIExpressionOptions;
            const FieldName: string): TOCIExpression;
    end;

{$IFNDEF OCI_EXPRESSION_STANDALONE}
    TOCIFilter = class(TCollectionItem)
    private
        FName: String;
        FText: String;
        FOptions: TFilterOptions;
        FActive: Boolean;
        FExpression: TOCIExpression;
        FOnFilterRecord: TFilterRecordEvent;
        FPrepared: Boolean;
        FBroken: Boolean;
        FKeyFields: String;
        FUseInApplyUpdates: Boolean;
        procedure SetActive(const Value: Boolean);
        function GetActive: Boolean;
        procedure SetOptions(const Value: TFilterOptions);
        procedure SetText(const Value: String);
        function GetDataSet: TDataSet;
        procedure SetFilterRecord(const Value: TFilterRecordEvent);
        procedure Changed(ACanCache: Boolean);
        function IsDefaultFilter: Boolean;
    protected
        function GetDisplayName: string; override;
        function Execute: Boolean;
    public
        destructor Destroy; override;
        procedure Assign(AValue: TPersistent); override;
        function IsEqual(AValue: TOCIFilter): Boolean;
        procedure Prepare;
        procedure PrepareLookupFilter(const AKeyFields: String;
            const AKeyValues: Variant; AOptions: TLocateOptions);
        procedure UnPrepare;
        property DataSet: TDataSet read GetDataSet;
    published
        property Name: String read FName write FName;
        property Text: String read FText write SetText;
        property Options: TFilterOptions read FOptions write SetOptions default [];
        property Active: Boolean read GetActive write SetActive default False;
        property UseInApplyUpdates: Boolean read FUseInApplyUpdates write
            FUseInApplyUpdates default False;
        property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord
            write SetFilterRecord;
    end;

    TOCIFilters = class(TCollection)
    private
        FOwner: TPersistent;
        function GetDataSet: TDataSet;
        function GetItem(AIndex: Integer): TOCIFilter;
        procedure SetItem(AIndex: Integer; AValue: TOCIFilter);
        function GetDefaultFilter: TOCIFilter;
        function GetLookupFilter: TOCIFilter;
        function GetAnyActive: Boolean;
    protected
        function GetOwner: TPersistent; override;
        procedure Update(Item: TCollectionItem); override;
    public
        constructor Create(AOwner: TPersistent); {$IFDEF OCI_D4} overload; {$ENDIF}
        function IsEqual(Value: TOciFilters): Boolean;
        function FilterByName(const Value: string): TOciFilter;
        function FindFilter(const Value: string): TOciFilter;
        function Execute: Boolean;
        property AnyActive: Boolean read GetAnyActive;
        property DataSet: TDataSet read GetDataSet;
        property DefaultFilter: TOCIFilter read GetDefaultFilter;
        property LookupFilter: TOCIFilter read GetLookupFilter;
        property Items[AIndex: Integer]: TOCIFilter read GetItem write SetItem; default;
{$IFDEF OCI_D4}
        property UpdateCount;
{$ENDIF}        
    end;
{$ENDIF}

    POCIFuncCall = function (const AArgs: Variant; AExpr: TOCIExpression): Variant;
    procedure OCIAddFunc(const AName: String; AScopeKind: TOCIExpressionScopeKind;
        AScopeKindArg: Integer; ADataType: TFieldType; ADataTypeArg: Integer;
        AArgMin: Integer; AArgMax: Integer; ACall: POCIFuncCall);

implementation

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

uses Forms, SysUtils, DBConsts, NCOciUtil, Math, Windows
     {$IFNDEF OCI_EXPRESSION_STANDALONE}, NCOci, NCOciMsg, NCOciDB {$ENDIF}
{$IFDEF OCI_D6}
     , Variants
{$ENDIF}
     ;

type
{$IFNDEF OCI_EXPRESSION_STANDALONE}
    __TOCIDataSet = class(TOCIDataSet);
{$ENDIF}    
    TOCIFuncDesc = class
    private
        FScopeKind: TOCIExpressionScopeKind;
        FScopeKindArg: Integer;
        FDataType: TFieldType;
        FDataTypeArg: Integer;
        FArgMin: Integer;
        FArgMax: Integer;
        FCall: POCIFuncCall;
    end;

const
    FStrTrue: String = STextTrue;
    FStrFalse: String = STextFalse;
    cWordSet: set of char = ['A'..'Z', 'a'..'z'];

var
    OCIFuncs: TStringList;

function IsNumeric(DataType: TFieldType): Boolean;
begin
    Result := DataType in [ftSmallInt, ftInteger, ftFloat, ftBCD];
end;

function IsTemporal(DataType: TFieldType): Boolean;
begin
    Result := DataType in [ftDateTime];
end;

function IsString(DataType: TFieldType): Boolean;
begin
    Result := DataType in [ftString {$IFDEF OCI_D4}, ftFixedChar {$ENDIF}];
end;

function IsBLOB(DataType: TFieldType): Boolean;
begin
    Result := DataType in [ftBlob, ftMemo
{$IFDEF OCI_D5}
    , ftOraBlob, ftOraClob
{$ENDIF}
    ];
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIFuncDesc

function StrIsNull(const V: Variant): Boolean;
var
    tp: Integer;
begin
    tp := (VarType(V) and varTypeMask);
    Result := (tp = varEmpty) or (tp = varNull) or
        ((tp = varString) or (tp = varOleStr)) and (V = '');
end;

function StrToVar(const S: String): Variant;
begin
    if S = '' then
        Result := Null
    else
        Result := S;
end;

function FunLike(const AStr, AMask: Variant; ANoCase: Boolean;
    AManyCharsMask, AOneCharMask, AEscapeChar: Char): Variant;
begin
    if VarIsNull(AStr) or VarIsNull(AMask) then
        Result := False
    else
        Result := StrLike(AStr, AMask, ANoCase, AManyCharsMask, AOneCharMask, AEscapeChar);
end;

function FunUpper(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := AnsiUpperCase(AArgs[0]);
end;

function FunLower(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := AnsiUpperCase(AArgs[0]);
end;

function FunSubstring(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    s: String;
    ind, cnt: Integer;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else begin
        s := AArgs[0];
        ind := AArgs[1];
        if ind < 0 then
            ind := Length(s) + ind + 1;
        if VarArrayHighBound(AArgs, 1) = 1 then
            cnt := Length(s)
        else if StrIsNull(AArgs[2]) or (AArgs[2] <= 0) then begin
            Result := Null;
            Exit;
        end
        else
            cnt := AArgs[2];
        Result := StrToVar(Copy(s, ind, cnt));
    end;
end;

type
    TOCITrimMode = set of (tmLeft, tmRight);

function InternalTrim(const AArgs: Variant; AExpr: TOCIExpression; AMode: TOCITrimMode): Variant;
var
    I, L: Integer;
    sWhere, sWhat: String;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        sWhere := AArgs[0];
        if VarArrayHighBound(AArgs, 1) = 1 then begin
            if StrIsNull(AArgs[1]) then begin
                Result := Null;
                Exit;
            end
            else
                sWhat := AArgs[1];
        end
        else
            sWhat := ' ';
        L := Length(sWhere);
        I := 1;
        if tmLeft in AMode then
            while (I <= L) and (StrScan(PChar(sWhat), sWhere[I]) <> nil) do
                Inc(I);
        if I > L then
            sWhere := ''
        else begin
            if tmRight in AMode then
                while (L >= I) and (StrScan(PChar(sWhat), sWhere[L]) <> nil) do
                    Dec(L);
            sWhere := Copy(sWhere, I, L - I + 1);
        end;
        Result := StrToVar(sWhere);
    end;
end;

function FunTrim(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := InternalTrim(AArgs, AExpr, [tmLeft, tmRight]);
end;

function FunTrimLeft(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := InternalTrim(AArgs, AExpr, [tmLeft]);
end;

function FunTrimRight(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := InternalTrim(AArgs, AExpr, [tmRight]);
end;

function FunYear(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    Y, M, D: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeDate(AArgs[0], Y, M, D);
        Result := Y;
    end;
end;

function FunMonth(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    Y, M, D: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeDate(AArgs[0], Y, M, D);
        Result := M;
    end;
end;

function FunDay(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    Y, M, D: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeDate(AArgs[0], Y, M, D);
        Result := D;
    end;
end;

function FunHour(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    H, M, S, MS: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeTime(AArgs[0], H, M, S, MS);
        Result := H;
    end;
end;

function FunMinute(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    H, M, S, MS: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeTime(AArgs[0], H, M, S, MS);
        Result := M;
    end;
end;

function FunSecond(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    H, M, S, MS: Word;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        DecodeTime(AArgs[0], H, M, S, MS);
        Result := S;
    end;
end;

function FunGetDate(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := Now;
end;

function FunDate(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Integer(Trunc(AArgs[0]));
end;

function FunTime(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    dt: TDateTime;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        dt := AArgs[0];
        Result := dt - Trunc(dt);
    end;
end;

function FunAbs(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Abs(AArgs[0]);
end;

function FunCeil(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Ceil(AArgs[0]);
end;

function FunCos(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Cos(AArgs[0]);
end;

function FunCosh(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Cosh(AArgs[0]);
end;

function FunExp(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Exp(AArgs[0]);
end;

function FunFloor(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Floor(AArgs[0]);
end;

function FunLn(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Ln(AArgs[0]);
end;

function FunLog(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else
        Result := LogN(AArgs[0], AArgs[1]);
end;

function FunMod(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else if AArgs[1] = 0 then
        Result := AArgs[0]
    else
        Result := AArgs[0] mod AArgs[1]
end;

function FunPower(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else
        Result := Power(AArgs[0], AArgs[1]);
end;

function FunRound(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
{TODO
    Support for
    1) for number
    2) for date
    3) Arg#2 - format mask
TODO}
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Round(AArgs[0]) + 0.0;
end;

function FunSign(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else if AArgs[0] > 0.0 then
        Result := 1
    else if AArgs[0] < 0.0 then
        Result := -1
    else
        Result := 0;
end;

function FunSin(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := sin(AArgs[0]);
end;

function FunSinh(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := sinh(AArgs[0]);
end;

function FunSqrt(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := sqrt(AArgs[0]);
end;

function FunTan(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := tan(AArgs[0]);
end;

function FunTanh(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := tanh(AArgs[0]);
end;

function FunTrunc(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
{TODO
    Support for
    1) for number
    2) for date
    3) Arg#2 - format mask
TODO}
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Trunc(AArgs[0]) + 0.0;
end;

function FunChr(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    s: String;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        s := Chr(Integer(AArgs[0]));
        Result := s;
    end;
end;

function FunConcat(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := AArgs[1]
    else if StrIsNull(AArgs[1]) then
        Result := AArgs[0]
    else
        Result := Concat(AArgs[0], AArgs[1]);
end;

function FunInitCap(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    i: Integer;
    s: String;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        s := AArgs[0];
        for i := 0 to Length(s) do
            if (s[i] in cWordSet) and ((i = 1) or not (s[i - 1] in cWordSet)) then
                s[i] := Chr(Integer(CharUpper(PChar(Ord(s[i])))));
    end;
end;

function InternalPad(const AArgs: Variant; AExpr: TOCIExpression; AFront: Boolean): Variant;
var
    n: Integer;
    s, ps: String;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) or
       (VarArrayHighBound(AArgs, 1) = 2) and StrIsNull(AArgs[2]) then
        Result := Null
    else begin
        s := AArgs[0];
        n := AArgs[1] - Length(s);
        if (VarArrayHighBound(AArgs, 1) = 1) then
            ps := ' '
        else
            ps := AArgs[2];
        while Length(ps) < n do
            ps := ps + ps;
        if Length(ps) > n then
            ps := Copy(ps, 1, n);
        if AFront then
            Result := StrToVar(ps + s)
        else
            Result := StrToVar(s + ps);
    end;
end;

function FunLPad(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := InternalPad(AArgs, AExpr, True);
end;

function FunRPad(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := InternalPad(AArgs, AExpr, False);
end;

function FunReplace(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    sWhere, sFrom, sTo: String;
    i: Integer;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else if StrIsNull(AArgs[1]) then
        Result := AArgs[0]
    else begin
        sWhere := AArgs[0];
        sFrom := AArgs[1];
        if VarArrayHighBound(AArgs, 1) = 2 then
            sTo := AArgs[2]
        else
            sTo := '';
        while True do begin
            i := Pos(sFrom, sWhere);
            if i = 0 then
                Break;
            Delete(sWhere, i, Length(sFrom));
            if sTo <> '' then
                Insert(sTo, sWhere, i);
        end;
        Result := StrToVar(sWhere);
    end;
end;

function FunSoundex(const AArgs: Variant; AExpr: TOCIExpression): Variant;
const
    SOUNDEX_LENGTH = 4;
var
    Source: String;
    code: String[SOUNDEX_LENGTH];
    i, j: Integer;
    c, res, prev: Char;

    function step2(C: Char): Char;
    begin
        case c of
        'B', 'F', 'P', 'V':
            Result := '1';
        'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z':
            Result := '2';
        'D', 'T':
            Result := '3';
        'L':
            Result := '4';
        'M', 'N':
            Result := '5';
        'R':
            Result := '6';
        'A', 'E', 'H', 'I', 'O', 'U', 'W', 'Y':
            Result := #0;
        else
            Result := '9';
        end;
    end;

begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        Source := Trim(AArgs[0]);
        if Source = '' then
            Result := Null
        else begin
            prev := #0;
            i := 1;
            j := 1;
            while (i <= Length(Source)) and (j <= SOUNDEX_LENGTH) do begin
                c := Chr(Integer(CharUpper(PChar(Ord(source[i])))));
                if c in cWordSet then begin
                    res := Step2(c);
                    if j = 1 then begin
                        code[j] := c;
                        Inc(j);
                    end
                    else if (res <> #0) and (res <> prev) then begin
                        code[j] := res;
                        Inc(j);
                    end;
                    prev := res;
                end;
                Inc(i);
            end;
            while j <= SOUNDEX_LENGTH do begin
                code[j] := '0';
                Inc(j);
            end;
            Result := code;
        end;
    end;
end;

function FunTranslate(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    sWhere, sFrom, sTo: String;
    i, j: Integer;
    pCh: PChar;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) or StrIsNull(AArgs[2]) then
        Result := Null
    else begin
        sWhere := AArgs[0];
        sFrom := AArgs[1];
        sTo := AArgs[2];
        i := 1;
        while i <= Length(sWhere) do begin
            pCh := StrScan(PChar(sFrom), sWhere[i]);
            if pCh <> nil then begin
                j := pCh - PChar(sFrom) + 1;
                if j > Length(sTo) then
                    Delete(sWhere, i, 1)
                else
                    sWhere[i] := sTo[j];
            end;
        end;
        Result := StrToVar(sWhere);
    end;
end;

function FunAscii(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    s: String;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        s := AArgs[0];
        Result := Ord(s[1]);
    end;
end;

function FunInstr(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    sWhere, sWhat: String;
    iCount, iFrom, hBnd: Integer;
    pStart, pCh: PChar;
    v: Variant;
    bReverse: Boolean;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else begin
        sWhere := AArgs[0];
        sWhat := AArgs[1];
        hBnd := VarArrayHighBound(AArgs, 1);
        iCount := 1;
        iFrom := 1;
        bReverse := False;
        if hBnd >= 2 then begin
            v := AArgs[2];
            if StrIsNull(v) then begin
                Result := Null;
                Exit;
            end;
            iFrom := v;
            bReverse := iFrom < 0;
            if bReverse then
                iFrom := Length(sWhere) + iFrom + 1;
            if hBnd >= 3 then begin
                v := AArgs[3];
                if StrIsNull(v) or (v <= 0) then begin
                    Result := Null;
                    Exit;
                end;
                iCount := v;
            end;
        end;
        pStart := PChar(sWhere) + iFrom - 1;
        pCh := nil;
        while iCount > 0 do begin
            if bReverse then begin
                pCh := StrRPos(pStart, PChar(sWhat));
                if pCh = nil then
                    Break;
                pStart := pCh - Length(sWhat);
            end
            else begin
                pCh := StrPos(pStart, PChar(sWhat));
                if pCh = nil then
                    Break;
                pStart := pCh + Length(sWhat);
            end;
            Dec(iCount);
        end;
        if pCh = nil then
            Result := 0
        else
            Result := pCh - PChar(sWhere) + 1;
    end;
end;

function FunLength(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else
        Result := Length(AArgs[0]);
end;

function FunAddMonths(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    AYear, AMonth, ADay: Word;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else begin
        DecodeDate(AArgs[0], AYear, AMonth, ADay);
        AMonth := (Integer(AYear) - 1) * 12 + (Integer(AMonth) + AArgs[1] - 1);
        AYear := AMonth div 12 + 1;
        AMonth := AMonth mod 12 + 1;
        Result := EncodeDate(AYear, AMonth, ADay);
    end;
end;

function FunLastDay(const AArgs: Variant; AExpr: TOCIExpression): Variant;
const
    DaysPerMonth: array[1..12] of Integer =
        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
var
    AYear, AMonth, ADay: Word;
    dt: TDateTime;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        dt := AArgs[0];
        DecodeDate(dt, AYear, AMonth, ADay);
        ADay := DaysPerMonth[AMonth];
        if (AMonth = 2) and
           (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0)) then
            Inc(ADay);
        Result := EncodeDate(AYear, AMonth, ADay) + (dt - Trunc(dt));
        TVarData(Result).VType := varDate;
    end;
end;

function FunFirstDay(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    AYear, AMonth, ADay: Word;
    dt: TDateTime;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else begin
        dt := AArgs[0];
        DecodeDate(AArgs[0], AYear, AMonth, ADay);
        Result := EncodeDate(AYear, AMonth, 1) + (dt - Int(dt));
        TVarData(Result).VType := varDate;
    end;
end;

function FunMonthsBW(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    AYear1, AMonth1, ADay1: Word;
    AYear2, AMonth2, ADay2: Word;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else begin
        DecodeDate(AArgs[0], AYear1, AMonth1, ADay1);
        DecodeDate(AArgs[1], AYear2, AMonth2, ADay2);
        Result := (Integer(AYear1 * 12 * 31 + AMonth1 * 31 + ADay1) -
                   Integer(AYear2 * 12 * 31 + AMonth2 * 31 + ADay2)) / 31;
    end;
end;

function FunNextDay(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    dt: TDateTime;
    nd: String;
    i: Integer;
begin
    if StrIsNull(AArgs[0]) or StrIsNull(AArgs[1]) then
        Result := Null
    else begin
        dt := AArgs[0];
        nd := AArgs[1];
        for i := 1 to 7 do begin
            dt := dt + 1;
            if AnsiStrLIComp(PChar(LongDayNames[DayOfWeek(dt)]),
                             PChar(nd), Length(nd)) = 0 then begin
                Result := dt;
                Exit;
            end;
        end;
        Result := Null;
        {TODO
            ORA-01846: not valid day of week
        TODo}
    end;
end;

function FunToChar(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    N: Integer;
    tp: Integer;
    E: Extended;
    V: Variant;
begin
{TODO
    support of Oracle's mask
TODO}
    N := VarArrayHighBound(AArgs, 1);
    if StrIsNull(AArgs[0]) or
       (N > 0) and StrIsNull(AArgs[1]) or
       (N > 1) and StrIsNull(AArgs[2]) then
        Result := Null
    else begin
        V := AArgs[0];
        if N > 0 then begin
            tp := (VarType(V) and varTypeMask);
            if tp = varDate then
                Result := FormatDateTime(AArgs[1], V)
            else if (tp = varSmallint) or (tp = varInteger) or (tp = varSingle) or
                    (tp = varDouble) or (tp = varCurrency) then begin
                E := V;
                Result := FormatFloat(AArgs[1], E);
            end
            else
                Result := VarAsType(V, varString);
        end
        else
            Result := VarAsType(V, varString);
    end;
end;

function FunToDate(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    N: Integer;
    prevSDF: String;
begin
{TODO
    support of Oracle's mask
TODO}
    N := VarArrayHighBound(AArgs, 1);
    if StrIsNull(AArgs[0]) or
       (N > 0) and StrIsNull(AArgs[1]) or
       (N > 1) and StrIsNull(AArgs[2]) then
        Result := Null
    else begin
        if N > 0 then begin
            prevSDF := ShortDateFormat;
            ShortDateFormat := AArgs[1];
        end;
        try
            Result := StrToDate(AArgs[0]);
        finally
            if N > 0 then
                ShortDateFormat := prevSDF;
        end;
    end;
end;

function FunToNumber(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    N: Integer;
begin
{TODO
    support of Oracle's mask
TODO}
    N := VarArrayHighBound(AArgs, 1);
    if StrIsNull(AArgs[0]) or
       (N > 0) and StrIsNull(AArgs[1]) or
       (N > 1) and StrIsNull(AArgs[2]) then
        Result := Null
    else
        Result := VarAsType(AArgs[0], varDouble);
end;

function FunDecode(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    n, i: Integer;
begin
    n := VarArrayHighBound(AArgs, 1);
    i := 1;
    Result := AArgs[0];
    while i <= n - 1 do begin
        if Result = AArgs[i] then begin
            Result := AArgs[i + 1];
            Break;
        end;
        Inc(i, 2);
    end;
    if i = n then
        Result := AArgs[i];
end;

function FunIIF(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if StrIsNull(AArgs[0]) then
        Result := Null
    else if AArgs[0] then
        Result := AArgs[1]
    else
        Result := AArgs[2]
end;

function FunNvl(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := AArgs[0];
    if StrIsNull(Result) then
        Result := AArgs[1];
end;

function FunGreatest(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    n, i: Integer;
begin
    n := VarArrayHighBound(AArgs, 1);
    Result := AArgs[0];
    for i := 0 to n do begin
        if StrIsNull(AArgs[i]) then begin
            Result := Null;
            Break;
        end;
        if Result < AArgs[i] then
            Result := AArgs[i];
    end;
end;

function FunLeast(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    n, i: Integer;
begin
    n := VarArrayHighBound(AArgs, 1);
    Result := AArgs[0];
    for i := 0 to n do begin
        if StrIsNull(AArgs[i]) then begin
            Result := Null;
            Break;
        end;
        if Result > AArgs[i] then
            Result := AArgs[i];
    end;
end;

function FunAggNotReady(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    raise Exception.Create('Aggregate functions jet not ready !');
end;

function FunRowNum(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    Result := AExpr.FDataSet.RecNo;
end;

{$IFNDEF OCI_EXPRESSION_STANDALONE}
function FunNextVal(const AArgs: Variant; AExpr: TOCIExpression): Variant;
var
    seq: TOCISequence;
    post: Boolean;
begin
    if AExpr.FDataSet is TOCIDataSet then begin
        post := False;
        if VarArrayHighBound(AArgs, 1) = 1 then
            if AArgs[1] then
                post := True;
        seq := __TOCIDataSet(AExpr.FDataSet).GetSeq(AArgs[0], not post);
        Result := seq.NextVal;
        if post then
            seq.Post;
    end
    else
        Result := null;
end;

function FunCurrVal(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    if AExpr.FDataSet is TOCIDataSet then
        Result := __TOCIDataSet(AExpr.FDataSet).GetSeq(AArgs[0], False).CurrVal
    else
        Result := null;
end;
{$ENDIF}

function FunReferences(const AArgs: Variant; AExpr: TOCIExpression): Variant;
begin
    
end;

procedure OCIAddFunc(const AName: String; AScopeKind: TOCIExpressionScopeKind;
    AScopeKindArg: Integer; ADataType: TFieldType; ADataTypeArg: Integer;
    AArgMin: Integer; AArgMax: Integer; ACall: POCIFuncCall);
var
    pFuncDesc: TOCIFuncDesc;
begin
    pFuncDesc := TOCIFuncDesc.Create;
    with pFuncDesc do begin
        FScopeKind := AScopeKind;
        FScopeKindArg := AScopeKindArg;
        FDataType := ADataType;
        FDataTypeArg := ADataTypeArg;
        FArgMin := AArgMin;
        FArgMax := AArgMax;
        FCall := ACall;
    end;
    OCIFuncs.AddObject(UpperCase(AName), pFuncDesc);
end;

function GetFuncDesc(AIndex: Integer): TOCIFuncDesc;
begin
    Result := TOCIFuncDesc(OCIFuncs.Objects[AIndex]);
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIExpression

constructor TOCIExpression.Create(DataSet: TDataSet; Options: TFilterOptions;
    ParseOptions: TOCIExpressionOptions; const FieldName: string);
begin
    inherited Create;
    FDataSet := DataSet;
    FOptions := Options;
    FFieldName := FieldName;
    FParserOptions := ParseOptions;
end;

destructor TOCIExpression.Destroy;
begin
    ClearNodes;
    inherited Destroy;
end;

function TOCIExpression.NewNode(Kind: TOCIExpressionNodeKind; Operator: TOCIExpressionOperator;
    const Data: Variant; Left, Right: POCIExpressionNode; AFuncInd: Integer): POCIExpressionNode;
begin
    New(Result);
    with Result^ do begin
        FNext := FNodes;
        FKind := Kind;
        FOperator := Operator;
        FData := Data;
        FLeft := Left;
        FRight := Right;
        FArgs := nil;
        FDataType := ftUnknown;
        FScopeKind := skUnknown;
        FFuncInd := AFuncInd;
        FField := nil;
        FPartial := False;
    end;
    FNodes := Result;
    if Kind = enField then begin
        if poFieldNameGiven in FParserOptions then
            Result^.FField := FDataSet.FieldByName(FFieldName);
        if Result^.FField = nil then
            Result^.FField := FDataSet.FieldByName(Data);
        if Result^.FField = nil then
            DatabaseErrorFmt(SFieldNotFound, [Data]);
{$IFNDEF OCI_EXPRESSION_STANDALONE}
        if FDataSet is TOCIDataSet then
            __TOCIDataSet(FDataSet).RegisterFilterField(Result^.FField);
{$ENDIF}            
        Result^.FDataType := Result^.FField.DataType;
    end;
end;

function TOCIExpression.NewCompareNode(Field: TField; Operator: TOCIExpressionOperator;
    const Value: Variant; APartial: Boolean): POCIExpressionNode;
var
    ConstExpr: POCIExpressionNode;
begin
    ConstExpr := NewNode(enConst, canNOTDEFINED, Value, nil, nil, -1);
    ConstExpr^.FDataType := Field.DataType;
    Result := NewNode(enOperator, Operator, Unassigned,
        NewNode(enField, canNOTDEFINED, Field.FieldName, nil, nil, -1),
        ConstExpr, -1);
    Result^.FPartial := APartial;
end;

procedure TOCIExpression.ClearNodes;
var
    Node: POCIExpressionNode;
begin
    while FNodes <> nil do begin
        Node := FNodes;
        FNodes := Node^.FNext;
        if Node^.FArgs <> nil then
            Node^.FArgs.Free;
        Dispose(Node);
    end;
end;

function TOCIExpression.Execute: Variant;

    function EvaluteNodeValueComplex(ANode: POCIExpressionNode): Variant;
    var
        l, r: Variant;
        i: Integer;
        pR: POCIExpressionNode;
        esc: String;
        pFunc: TOCIFuncDesc;

        function VarIsString(const V: Variant): Boolean;
        var
            tp: Integer;
        begin
            tp := VarType(l);
            Result := (tp = varString) or (tp = varOleStr);
        end;

        procedure UpperCaseLR;
        begin
            if VarIsString(l) then
                l := AnsiUpperCase(l);
            if VarIsString(r) then
                r := AnsiUpperCase(r);
        end;

        function PartialEQ(AForce: Boolean): Boolean;
        var
            sL, sR: String;
            ln, lnL, lnR: Integer;
            partial: Boolean;
        begin
            if VarIsString(l) and VarIsString(r) then begin
                sL := l;
                sR := r;
                lnL := Length(sL);
                lnR := Length(sR);
                if l <> '' then begin
                    partial := False;
                    if sL[lnL] = '*' then begin
                        partial := True;
                        Dec(lnL);
                    end;
                    if r <> '' then begin
                        if sR[lnR] = '*' then begin
                            partial := True;
                            Dec(lnR);
                        end;
                        if partial or AForce then begin
                            ln := lnR;
                            if ln > lnL then
                                ln := lnL;
                            if lnL < lnR then
                                Result := False
                            else if (foCaseInsensitive in FOptions) then
                                Result := AnsiStrLIComp(PChar(sL), PChar(sR), ln) = 0
                            else
                                Result := AnsiStrLComp(PChar(sL), PChar(sR), ln) = 0;
                            Exit;
                        end;
                    end;
                end;
                if (foCaseInsensitive in FOptions) then
                    Result := AnsiCompareText(sL, sR) = 0
                else
                    Result := sL = Sr;
            end
            else begin
                UpperCaseLR;
                Result := l = r;
            end;
        end;

        function EvaluteNodeValue(ANode: POCIExpressionNode): Variant;
        begin
            if ANode^.FKind = enField then
                Result := ANode^.FField.Value
            else if ANode^.FKind = enConst then
                Result := ANode^.FData
            else
                Result := EvaluteNodeValueComplex(ANode);
        end;

    begin
        case ANode^.FKind of
        enUnknown:;
        enField:
            Result := ANode^.FField.Value;
        enConst:
            Result := ANode^.FData;
        enOperator:
            case ANode^.FOperator of
            canNOTDEFINED:;
            canASSIGN:
                FDataSet.FieldByName(ANode^.FRight^.FData).Value :=
                    EvaluteNodeValue(ANode^.FLeft);
            canOR:
                Result := Boolean(EvaluteNodeValue(ANode^.FLeft)) or
                          Boolean(EvaluteNodeValue(ANode^.FRight));
            canAND:
                Result := Boolean(EvaluteNodeValue(ANode^.FLeft)) and
                          Boolean(EvaluteNodeValue(ANode^.FRight));
            canNOT:
                Result := not Boolean(EvaluteNodeValue(ANode^.FLeft));
            canEQ, canNE, canGE, canLE, canGT, canLT:
                begin
                    l := EvaluteNodeValue(ANode^.FLeft);
                    pR := ANode^.FRight;
                    if pR^.FOperator in [canANY, canALL] then begin
                        for i := 0 to pR.FArgs.Count - 1 do begin
                            r := EvaluteNodeValue(POCIExpressionNode(pR.FArgs[i]));
                            if (foCaseInsensitive in FOptions) and
                               not (foNoPartialCompare in FOptions) then
                                UpperCaseLR;
                            case ANode^.FOperator of
                                canEQ:
                                    if foNoPartialCompare in FOptions then
                                        Result := l = r
                                    else
                                        Result := PartialEQ(ANode^.FPartial);
                                canNE: Result := l <> r;
                                canGE: Result := l >= r;
                                canLE: Result := l <= r;
                                canGT: Result := l > r;
                                canLT: Result := l < r;
                            end;
                            if (pR^.FOperator = canANY) and Result or
                               (pR^.FOperator = canALL) and not Result then
                                Break;
                        end;
                    end
                    else begin
                        r := EvaluteNodeValue(ANode^.FRight);
                        if (foCaseInsensitive in FOptions) and
                           not (foNoPartialCompare in FOptions) then
                            UpperCaseLR;
                        case ANode^.FOperator of
                            canEQ:
                                if foNoPartialCompare in FOptions then
                                    Result := l = r
                                else
                                    Result := PartialEQ(ANode^.FPartial);
                            canNE: Result := l <> r;
                            canGE: Result := l >= r;
                            canLE: Result := l <= r;
                            canGT: Result := l > r;
                            canLT: Result := l < r;
                        end;
                    end;
                end;
            canLIKE, canNOTLIKE:
                begin
                    if ANode^.FArgs <> nil then
                        esc := EvaluteNodeValue(POCIExpressionNode(ANode^.FArgs.Items[0]))
                    else
                        esc := #9;
                    Result := FunLike(EvaluteNodeValue(ANode^.FLeft), EvaluteNodeValue(ANode^.FRight),
                                      foCaseInsensitive in FOptions, '%', '_', esc[1]);
                    if ANode^.FOperator = canNOTLIKE then
                        Result := not Result;
                end;
            canISBLANK, canNOTBLANK:
                begin
                    if ANode^.FLeft^.FKind = enField then
                        Result := ANode^.FLeft^.FField.IsNull
                    else
                        Result := StrIsNull(EvaluteNodeValue(ANode^.FLeft));
                    if ANode^.FOperator = canNOTBLANK then
                        Result := not Result;
                end;
            canIN, canNOTIN:
                begin
                    Result := False;
                    l := EvaluteNodeValue(ANode^.FLeft);
                    for i := 0 to ANode^.FArgs.Count - 1 do
                        if l = EvaluteNodeValue(POCIExpressionNode(ANode^.FArgs.Items[i])) then begin
                            Result := True;
                            Break;
                        end;
                    if ANode^.FOperator = canNOTIN then
                        Result := not Result;
                end;
            canCONCAT, canADD:
                Result := EvaluteNodeValue(ANode^.FLeft) +
                          EvaluteNodeValue(ANode^.FRight);
            canSUB:
                Result := EvaluteNodeValue(ANode^.FLeft) -
                          EvaluteNodeValue(ANode^.FRight);
            canMUL:
                Result := EvaluteNodeValue(ANode^.FLeft) *
                          EvaluteNodeValue(ANode^.FRight);
            canDIV:
                Result := EvaluteNodeValue(ANode^.FLeft) /
                          EvaluteNodeValue(ANode^.FRight);
            canBETWEEN, canNOTBETWEEN:
                begin
                    l := EvaluteNodeValue(ANode^.FLeft);
                    Result := (l >= EvaluteNodeValue(POCIExpressionNode(ANode^.FArgs.Items[0]))) and
                              (l <= EvaluteNodeValue(POCIExpressionNode(ANode^.FArgs.Items[1])));
                    if ANode^.FOperator = canNOTBETWEEN then
                        Result := not Result;
                end;
            end;
        enFunc:
            begin
                pFunc := GetFuncDesc(ANode^.FFuncInd);
                if (ANode^.FArgs = nil) or (ANode^.FArgs.Count = 0) then
                    r := null
                else begin
                    r := VarArrayCreate([0, ANode^.FArgs.Count - 1], varVariant);
                    for i := 0 to ANode^.FArgs.Count - 1 do
                        r[i] := EvaluteNodeValue(POCIExpressionNode(ANode^.FArgs.Items[i]));
                end;
                Result := pFunc.FCall(r, Self);
            end;
        end;
    end;
begin
    Result := EvaluteNodeValueComplex(FRoot);
end;

procedure TOCIExpression.Dump(AStrs: TStrings);
{$IFDEF OCI_DEBUG}
const
    enk2s: array[TOCIExpressionNodeKind] of String = (
        'enUnknown', 'enField', 'enConst', 'enOperator', 'enFunc');
    bool2s: array[Boolean] of String = (
        'False', 'True');
    esk2s: array[TOCIExpressionScopeKind] of String = (
        'skUnknown', 'skField', 'skAgg', 'skConst'
    );
    co2s: array[TOCIExpressionOperator] of String = (
        'canNOTDEFINED', 'canASSIGN', 'canOR', 'canAND', 'canNOT', 'canEQ', 'canNE',
        'canGE', 'canLE', 'canGT', 'canLT', 'canLIKE', 'canISBLANK', 'canNOTBLANK',
        'canIN', 'canCONCAT', 'canADD', 'canSUB', 'canMUL', 'canDIV', 'canNOTIN',
        'canANY', 'canALL', 'canNOTLIKE', 'canBETWEEN', 'canNOTBETWEEN'
    );
    ft2s: array[TFieldType] of String = (
        'ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord', 'ftBoolean',
        'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime', 'ftBytes',
        'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo',
        'ftParadoxOle', 'ftDBaseOle', 'ftTypedBinary', 'ftCursor'
{$IFDEF OCI_D4}
        , 'ftFixedChar', 'ftWideString', 'ftLargeint', 'ftADT', 'ftArray', 'ftReference', 'ftDataSet'
{$ENDIF}        
{$IFDEF OCI_D5}
        , 'ftOraBlob', 'ftOraClob', 'ftVariant', 'ftInterface', 'ftIDispatch', 'ftGuid'
{$ENDIF}
{$IFDEF OCI_D6}
        , 'ftTimeStamp', 'ftFMTBcd'
{$ENDIF}
{$IFDEF OCI_D10}
        , 'ftFixedWideChar', 'ftWideMemo', 'ftOraTimeStamp', 'ftOraInterval'
{$ENDIF}
        );

    function Var2Text(const AValue: Variant): String;
    begin
        if VarIsEmpty(AValue) then
            Result := 'Unassigned'
        else if VarIsNull(AValue) then
            Result := 'Null'
        else
            Result := AValue;
    end;

    procedure DumpNode(ANode: POCIExpressionNode; AIndent: Integer; AList: TStrings);
    var
        st: String;
        i: Integer;
    begin
        with ANode^ do begin
            st := '';
            for i := 1 to AIndent do
                st := st + ' ';
            AList.Add(st + 
                'Kind: ' + enk2s[FKind] + ' ' +
                'Oper: ' + co2s[Foperator] + ' ' +
                'Data: ' + Var2Text(FData) + ' ' +
                'DT: ' + ft2s[FDataType] + ' ' +
                'Scope: ' + esk2s[FScopeKind]
            );
            if (FArgs <> nil) and (FArgs.Count > 0) then begin
                AList.Add(st + 'ARGS:');
                for i := 0 to FArgs.Count - 1 do
                    DumpNode(POCIExpressionNode(FArgs[i]), AIndent + 2, AList);
            end;
            if FLeft <> nil then begin
                AList.Add(st + 'LEFT:');
                DumpNode(FLeft, AIndent + 2, AList);
            end;
            if FRight <> nil then begin
                AList.Add(st + 'RIGHT:');
                DumpNode(FRight, AIndent + 2, AList);
            end;
        end;
    end;

{$ENDIF}
begin
{$IFDEF OCI_DEBUG}
    AStrs.Clear;
    if FRoot <> nil then
        DumpNode(FRoot, 0, AStrs)
    else
        AStrs.Add('-= EMPTY =-');
{$ELSE}
    raise Exception.Create('Before use of TOCIExpression.Dump recompile NCOciFilter '#13#10 +
      'with uncommented line {$DEFINE OCI_DEBUG} in begin of unit');
{$ENDIF}
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIExpressionParser

function TOCIExpressionParser.BuildExpression(ADataSet: TDataSet; const Text: string;
    Options: TFilterOptions; ParserOptions: TOCIExpressionOptions; const FieldName: string): TOCIExpression;
var
    Root, DefField: POCIExpressionNode;
begin
    FExpression := TOCIExpression.Create(ADataSet, Options, ParserOptions, FieldName);
    FDataSet := ADataSet;
    FParserOptions := ParserOptions;
    FText := Text;
    FSourcePtr := PChar(Text);
    NextToken;
    Root := ParseExpr;
    if FToken <> etEnd then
        DatabaseError(SExprTermination);
    if (poAggregate in FParserOptions) and (Root^.FScopeKind <> skAgg) then
        DatabaseError(SExprNotAgg);
    if (not (poAggregate in FParserOptions)) and (Root^.FScopeKind = skAgg) then
        DatabaseError(SExprNoAggFilter);
    if (poDefaultExpr in ParserOptions) and (FieldName <> '') then begin
        DefField := FExpression.NewNode(enField, canNOTDEFINED, FieldName, nil, nil, -1);
        if (IsTemporal(DefField^.FDataType) and IsString(Root^.FDataType)) or
           ((DefField^.FDataType = ftBoolean) and IsString(Root^.FDataType)) then
            Root^.FDataType := DefField^.FDataType;
        if not ((Root^.FKind = enConst) and VarIsNull(Root^.FData)
           or (IsTemporal(DefField^.FDataType) and IsTemporal(Root^.FDataType))
           or (IsNumeric(DefField^.FDataType) and IsNumeric(Root^.FDataType))
           or (IsString(DefField^.FDataType) and IsString(Root^.FDataType))
           or (DefField^.FDataType = ftBoolean) and (Root^.FDataType = ftBoolean)) then
            DatabaseError(SExprTypeMis);
        Root := FExpression.NewNode(enOperator, canASSIGN, Unassigned, Root, DefField, -1);
    end;
    if not (poAggregate in FParserOptions) and not (poDefaultExpr in ParserOptions)
       and (Root^.FDataType <> ftBoolean) then
        DatabaseError(SExprIncorrect);
    FExpression.FRoot := Root;
    Result := FExpression;
end;

procedure TOCIExpressionParser.SaveState(var ABmk: TOCIExprParserBmk);
begin
    ABmk.FSourcePtr := FSourcePtr;
    ABmk.FToken := FToken;
    ABmk.FTokenString := FTokenString;
end;

procedure TOCIExpressionParser.RestoreState(var ABmk: TOCIExprParserBmk);
begin
    FSourcePtr := ABmk.FSourcePtr;
    FToken := ABmk.FToken;
    FTokenString := ABmk.FTokenString;
    FUCTokenString := UpperCase(ABmk.FTokenString);
end;

function TOCIExpressionParser.NextTokenIsLParen : Boolean;
var
    P : PChar;
begin
    P := FSourcePtr;
    while (P^ <> #0) and (P^ <= ' ') do
        Inc(P);
    Result := P^ = '(';
end;

procedure TOCIExpressionParser.NextToken;
type
    ASet = Set of Char;
var
    tmpPtr, P, TokenStart: PChar;
    L: Integer;
    StrBuf: array[0..255] of Char;
    PrevToken: TOCIExpressionToken;
    ch2: Char;
    sign: Double;
    prevDecSep: Char;

    procedure Skip(TheSet: ASet);
    begin
        while (P^ <> #0) and (P^ in TheSet) do
            Inc(P);
    end;

    procedure SkipWS;
    begin
        while (P^ <> #0) and (P^ <= ' ') do
            Inc(P);
    end;

    procedure SkpiWhileNot(TheSet: ASet);
    begin
        while (P^ <> #0) and not (P^ in TheSet) do
            Inc(P);
    end;

    procedure ExtractToken(TheSet: ASet);
    begin
        SkipWS;
        TokenStart := P;
        Skip(TheSet);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FUCTokenString := AnsiUpperCase(FTokenString);
    end;

    procedure ExtractWord;
    begin
        ExtractToken(['A'..'Z', 'a'..'z']);
    end;

    procedure DoNaturalComp;
    begin
        if (FUCTokenString = 'EQUAL') or (FUCTokenString = 'SAME') then
            FToken := etEQ
        else if (FUCTokenString = 'GREATER') or
                (FUCTokenString = 'UPPER') and (P^ <> '(') then
            FToken := etGT
        else if (FUCTokenString = 'LESS') or
                (FUCTokenString = 'LOWER') and (P^ <> '(') then
            FToken := etLT;
        if FToken in [etGT, etLT] then begin
            ExtractWord;
            if FUCTokenString <> 'THAN' then
                DatabaseError(SInvalidKeywordUse);
            tmpPtr := P;
            ExtractWord;
            if FUCTokenString = 'OR' then begin
                ExtractWord;
                if (FUCTokenString = 'EQUAL') or (FUCTokenString = 'SAME') then begin
                    if FToken = etGT then
                        FToken := etGE
                    else if FToken = etLT then
                        FToken := etLE;
                end
                else
                    DatabaseError(SInvalidKeywordUse);
            end
            else
                P := tmpPtr;
        end;
    end;

begin
    PrevToken := FToken;
    FTokenString := '';
    P := FSourcePtr;
    SkipWS;
    // /* comment */
    if (P^ <> #0) and (P^ = '/') and (P[1] <> #0) and (P[1] = '*') then begin
        Inc(P, 2);
        SkpiWhileNot(['*']);
        if (P^ = '*') and (P[1] <> #0) and (P[1] =  '/')  then
            Inc(P, 2)
        else
            DatabaseErrorFmt(SExprInvalidChar, [P^]);
    end
    // -- comment
    else if (P^ <> #0) and (P^ = '-') and (P[1] <> #0) and (P[1] = '-') then begin
        Inc(P, 2);
        SkpiWhileNot([#13, #10]);
    end;
    SkipWS;
    FTokenPtr := P;

    case P^ of
    'A'..'Z', 'a'..'z', 'À'..'ß', 'à'..'ÿ':
        begin
            ExtractToken(['A'..'Z', 'a'..'z', '0'..'9', '$', '#', '_', '.',
                          'À'..'ß', 'à'..'ÿ']);
            FToken := etSymbol;
            if FUCTokenString = 'NOT' then begin
                tmpPtr := P;
                ExtractWord;
                if FUCTokenString = 'IN' then
                    FToken := etNOTIN
                else if FUCTokenString = 'LIKE' then
                    FToken := etNOTLIKE
                else if FUCTokenString = 'BETWEEN' then
                    FToken := etNOTBETWEEN
                else
                    P := tmpPtr;
            end
            else if FUCTokenString = 'LIKE' then
                FToken := etLIKE
            else if FUCTokenString = 'IN' then
                FToken := etIN
            else if FUCTokenString = 'ANY' then
                FToken := etANY
            else if FUCTokenString = 'SOME' then
                FToken := etSOME
            else if FUCTokenString = 'ALL' then
                FToken := etALL
            else if FUCTokenString = 'BETWEEN' then
                FToken := etBETWEEN
            else if (poNaturalLang in FParserOptions) and (FUCTokenString = 'FIRST') then
                FToken := etFIRST
            else if (poNaturalLang in FParserOptions) and (FUCTokenString = 'LAST') then
                FToken := etLAST
            else if (poNaturalLang in FParserOptions) and
                    ((FUCTokenString = 'PRIOR') or (FUCTokenString = 'PREVIOS')) then
                FToken := etPRIOR
            else if (poNaturalLang in FParserOptions) and
                    ((FUCTokenString = 'NEXT') or (FUCTokenString = 'FOLLOWS')) then
                FToken := etNEXT
            else if (poNaturalLang in FParserOptions) and
                    ((FUCTokenString = 'THIS') or (FUCTokenString = 'CURRENT')) then
                FToken := etTHIS
            else if not FInSQLLang and (poNaturalLang in FParserOptions) and
                    (FUCTokenString = 'DAY') then
                FToken := etDAY
            else if not FInSQLLang and (poNaturalLang in FParserOptions) and
                    (FUCTokenString = 'MONTH') then
                FToken := etMONTH
            else if not FInSQLLang and (poNaturalLang in FParserOptions) and
                    (FUCTokenString = 'QUARTER') then
                FToken := etQUARTER
            else if not FInSQLLang and (poNaturalLang in FParserOptions) and
                    (FUCTokenString = 'YEAR') then
                FToken := etYEAR
            else if not FInSQLLang and (poNaturalLang in FParserOptions) and
                    (FUCTokenString = 'OF') then
                FToken := etOF
            else if FUCTokenString = 'IS' then begin
                ExtractWord;
                if FUCTokenString = 'NOT' then begin
                    ExtractWord;
                    if FUCTokenString = 'NULL' then
                        FToken := etISNOTNULL;
                end
                else if FUCTokenString = 'NULL' then
                    FToken := etISNULL
                else if (poNaturalLang in FParserOptions) then
                    DoNaturalComp;
                if FToken = etSYMBOL then
                    DatabaseError(SInvalidKeywordUse);
            end
            else if FUCTokenString = 'NULL' then
                FToken := etNULL
            else if (poNaturalLang in FParserOptions) then
                DoNaturalComp;
        end;
    '[', '"':
        begin
            if P^ = '[' then
                ch2 := ']'
            else
                ch2 := '"';
            Inc(P);
            TokenStart := P;
            P := AnsiStrScan(P, ch2);
            if P = nil then
                DatabaseError(SExprNameError);
            SetString(FTokenString, TokenStart, P - TokenStart);
            FToken := etName;
            Inc(P);
        end;
    '''':
        begin
            Inc(P);
            L := 0;
            while True do begin
                if P^ = #0 then
                    DatabaseError(SExprStringError);
                if P^ = '''' then begin
                    Inc(P);
                    if P^ <> '''' then
                        Break;
                end;
                if L < SizeOf(StrBuf) then begin
                    StrBuf[L] := P^;
                    Inc(L);
                end;
                Inc(P);
            end;
            SetString(FTokenString, StrBuf, L);
            FToken := etLiteral;
            FNumericLit := False;
        end;
    '-', '+', '0'..'9', '.':
        begin
            if (PrevToken <> etLiteral) and (PrevToken <> etName) and
               (PrevToken <> etSymbol) and (PrevToken <> etRParen) and
               (PrevToken <> etEMBEDDING) then begin
                if P^ = '-' then begin
                    sign := -1;
                    Inc(P);
                end
                else if P^ = '+' then begin
                    sign := 1;
                    Inc(P);
                end
                else
                    sign := 1;
                ExtractToken(['0'..'9', '.']);
                FToken := etLiteral;
                FNumericLit := True;
                prevDecSep := DecimalSeparator;
                DecimalSeparator := '.';
                try
                    FTokenNumber := StrToFloat(FTokenString) * sign;
                finally
                    DecimalSeparator := prevDecSep;
                end;
            end
            else if P^ = '+' then begin
                Inc(P);
                FToken := etADD;
            end
            else begin
                FToken := etSUB;
                Inc(P);
            end;
        end;
    '(':
        begin
            Inc(P);
            FToken := etLParen;
        end;
    ')':
        begin
            Inc(P);
            FToken := etRParen;
        end;
    '<':
        begin
            Inc(P);
            case P^ of
            '=':
                begin
                    Inc(P);
                    FToken := etLE;
                end;
            '>':
                begin
                    Inc(P);
                    FToken := etNE;
                end;
            else
                FToken := etLT;
            end;
        end;
    '^', '!':
        begin
            Inc(P);
            if P^ = '=' then begin
                Inc(P);
                FToken := etNE;
            end
            else
                DatabaseErrorFmt(SExprInvalidChar, [P^]);
        end;
    '=':
        begin
            Inc(P);
            FToken := etEQ;
        end;
    '>':
        begin
            Inc(P);
            if P^ = '=' then begin
                Inc(P);
                FToken := etGE;
            end
            else
                FToken := etGT;
        end;
    '|':
        begin
            Inc(P);
            if P^ = '|' then begin
                Inc(P);
                FToken := etCONCAT;
            end
            else
                DatabaseErrorFmt(SExprInvalidChar, [P^]);
        end;
    '*':
        begin
            Inc(P);
            FToken := etMUL;
        end;
    '/':
        begin
            Inc(P);
            FToken := etDIV;
      end;
    ',':
        begin
            Inc(P);
            FToken := etComma;
        end;
    '@':
        begin
            Inc(P);
            if P^ = '@' then begin
                Inc(P);
                FToken := etEMBEDDING;
                ExtractToken(['0'..'9']);
            end
            else
                DatabaseErrorFmt(SExprInvalidChar, [P^]);
        end;
    #0:
        FToken := etEnd;
    else
        DatabaseErrorFmt(SExprInvalidChar, [P^]);
    end;
    FSourcePtr := P;
end;

function TOCIExpressionParser.ParseExpr: POCIExpressionNode;
begin
    Result := ParseExpr2;
    while TokenSymbolIs('OR') do begin
        NextToken;
        Result := FExpression.NewNode(enOperator, canOR, Unassigned,
            Result, ParseExpr2, -1);
        GetScopeKind(Result, Result^.FLeft, Result^.FRight);
        Result^.FDataType := ftBoolean;
    end;
end;

function TOCIExpressionParser.ParseExpr2: POCIExpressionNode;
begin
    Result := ParseExpr3;
    while TokenSymbolIs('AND') do begin
        NextToken;
        Result := FExpression.NewNode(enOperator, canAND, Unassigned,
            Result, ParseExpr3, -1);
        GetScopeKind(Result, Result^.FLeft, Result^.FRight);
        Result^.FDataType := ftBoolean;
    end;
end;

function TOCIExpressionParser.ParseExpr3: POCIExpressionNode;
begin
    if TokenSymbolIs('NOT') then begin
        NextToken;
        Result := FExpression.NewNode(enOperator, canNOT, Unassigned,
            ParseExpr4, nil, -1);
        Result^.FDataType := ftBoolean;
    end
    else
        Result := ParseExpr4;
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
end;

function TOCIExpressionParser.ParseExpr4: POCIExpressionNode;
const
    Operators: array[etEQ..etLT] of TOCIExpressionOperator = (
        canEQ, canNE, canGE, canLE, canGT, canLT);
var
    Operator: TOCIExpressionOperator;
    Left, Right: POCIExpressionNode;
begin
    Result := ParseExprNatural(False);
    if (FToken in [etEQ..etLT, etLIKE, etNOTLIKE, etISNULL, etISNOTNULL,
                   etIN, etNOTIN, etBETWEEN, etNOTBETWEEN]) then begin
        case FToken of
        etEQ..etLT:
            Operator := Operators[FToken];
        etLIKE:
            Operator := canLIKE;
        etNOTLIKE:
            Operator := canNOTLIKE;
        etISNULL:
            Operator := canISBLANK;
        etISNOTNULL:
            Operator := canNOTBLANK;
        etIN:
            Operator := canIN;
        etNOTIN:
            Operator := canNOTIN;
        etBETWEEN:
            Operator := canBETWEEN;
        etNOTBETWEEN:
            Operator := canNOTBETWEEN;
        else
            Operator := canNOTDEFINED;
        end;
        NextToken;
        Left := Result;
        if Operator in [canIN, canNOTIN] then begin
            if FToken <> etLParen then
                DatabaseErrorFmt(SExprNoLParen, [TokenName]);
            NextToken;
            Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                Left, nil, -1);
            Result.FDataType := ftBoolean;
            if FToken <> etRParen then begin
                Result.FArgs := TList.Create;
                repeat
                    Right := ParseExpr;
                    if IsTemporal(Left.FDataType) then
                        Right.FDataType := Left.FDataType;
                    Result.FArgs.Add(Right);
                    if (FToken <> etComma) and (FToken <> etRParen) then
                        DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
                    if FToken = etComma then
                        NextToken;
                until (FToken = etRParen) or (FToken = etEnd);
                if FToken <> etRParen then
                    DatabaseErrorFmt(SExprNoRParen, [TokenName]);
                NextToken;
            end
            else
                DatabaseError(SExprEmptyInList);
        end
        else if Operator in [canBETWEEN, canNOTBETWEEN] then begin
            Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                Left, nil, -1);
            Result.FDataType := ftBoolean;
            Result.FArgs := TList.Create;
            Result.FArgs.Add(ParseExprNatural(False));
            if TokenSymbolIs('AND') then
                NextToken
            else
                DatabaseErrorFmt(SExprExpected, ['AND']);
            Result.FArgs.Add(ParseExprNatural(False));
        end
        else begin
            if Operator in [canEQ, canNE, canGE, canLE, canGT, canLT] then begin
                Right := ParseExprNatural(True);
                Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                    Left, Right, -1);
            end
            else if Operator in [canLike, canNotLike] then begin
                Right := ParseExprNatural(False);
                Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                    Left, Right, -1);
                if TokenSymbolIs('ESCAPE') then begin
                    NextToken;
                    Result.FArgs := TList.Create;
                    Result.FArgs.Add(ParseExprNatural(False));
                end;
            end
            else begin
                Right := nil;
                Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                    Left, Right, -1);
            end;
            if Right <> nil then begin
                if (Left^.FKind = enField) and (Right^.FKind = enConst) then
                    Right^.FDataType := Left^.FDataType
                else if (Right^.FKind = enField) and (Left^.FKind = enConst) then
                    Left^.FDataType := Right^.FDataType
            end;
            if IsBLOB(Left^.FDataType) and (Operator in [canLIKE, canNOTLIKE]) then begin
                if Right^.FKind = enConst then
                    Right^.FDataType := ftString;
            end
            else if not (Operator in [canISBLANK, canNOTBLANK])
                    and (IsBLOB(Left^.FDataType) or
                         (Right <> nil) and IsBLOB(Right^.FDataType)) then
                DatabaseError(SExprTypeMis);
            Result.FDataType := ftBoolean;
            if Right <> nil then begin
                if IsTemporal(Left.FDataType) and IsString(Right.FDataType) then
                    Right.FDataType := Left.FDataType
                else if IsTemporal(Right.FDataType) and IsString(Left.FDataType) then
                    Left.FDataType := Right.FDataType;
            end;
            GetScopeKind(Result, Left, Right);
        end;
    end;
end;

function NaturalDateExpr(D, M, Q, Y: String; DM, dmOf, MM, mmOf, QM, YM: TOCIExpressionToken): String;
begin
    if YM <> etEnd then
        Result := 'SYSDATE'
    else
        Result := '';

    case YM of
    etABS:   Result := 'ADD_MONTHS(SYSDATE, (' + Y + ' - YEAR(SYSDATE)) * 12)';
    etFIRST: Result := 'ADD_MONTHS(SYSDATE, - (12 * 2000))';
    etLAST:  Result := 'ADD_MONTHS(SYSDATE, 12 * 2000)';
    etPRIOR: Result := 'ADD_MONTHS(SYSDATE, -12)';
    etNEXT:  Result := 'ADD_MONTHS(SYSDATE, 12)';
    end;

    if (QM <> etEnd) and (Result = '') then
        Result := 'SYSDATE';
    case QM of
    etABS:   Result := 'ADD_MONTHS(' + Result + ', ' + Q + ' * 3 - MONTH(' + Result + '))';
    etFIRST: Result := 'ADD_MONTHS(' + Result + ', 1 - MONTH(' + Result + '))';
    etLAST:  Result := 'ADD_MONTHS(' + Result + ', 12 - MONTH(' + Result + '))';
    etPRIOR: Result := 'ADD_MONTHS(' + Result + ', -3)';
    etNEXT:  Result := 'ADD_MONTHS(' + Result + ', 3)';
    end;

    if (MM <> etEnd) and (Result = '') then
        Result := 'SYSDATE';
    case MM of
    etABS:
        case mmOf of
        etQuarter:
            Result := 'ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3) * 3 + 1 - MONTH(' + Result + ') + ' + M + ')';
        else
            Result := 'ADD_MONTHS(' + Result + ', ' + M + ' - MONTH(' + Result + '))';
        end;
    etFIRST:
        case mmOf of
        etQuarter:
            Result := 'ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3) * 3 + 1 - MONTH(' + Result + '))';
        else
            Result := 'ADD_MONTHS(' + Result + ', 1 - MONTH(' + Result + '))';
        end;
    etLAST:
        case mmOf of
        etQuarter:
            Result := 'ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3 + 1) * 3 - MONTH(' + Result + '))';
        else
            Result := 'ADD_MONTHS(' + Result + ', 12 - MONTH(' + Result + '))';
        end;
    etPRIOR: Result := 'ADD_MONTHS(' + Result + ', -1)';
    etNEXT:  Result := 'ADD_MONTHS(' + Result + ', 1)';
    end;
    
    if (DM <> etEnd) and (Result = '') then
        Result := 'SYSDATE';
    case DM of
    etABS:
        case dmOf of
        etMonth:
            Result := 'FIRST_DAY(' + Result + ') - 1 + ' + D;
        etQuarter:
            Result := 'FIRST_DAY(ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3) * 3 + 1 - MONTH(' + Result + '))) - 1 + ' + D;
        else
            Result := 'TO_DATE(''01' + DateSeparator + '01' + DateSeparator +
                ''' || TO_CHAR(YEAR(' + Result + ', )), ''DD/MM/YYYY'') - 1 + ' + D;
        end;
    etFIRST:
        case dmOf of
        etMonth:
            Result := 'FIRST_DAY(' + Result + ')';
        etQuarter:
            Result := 'FIRST_DAY(ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3) * 3 + 1 - MONTH(' + Result + ')))';
        else
            Result := 'TO_DATE(''01' + DateSeparator + '01' + DateSeparator +
                ''' || TO_CHAR(YEAR(' + Result + ')), ''DD/MM/YYYY'')';
        end;
    etLAST:
        case dmOf of
        etMonth:
            Result := 'LAST_DAY(' + Result + ')';
        etQuarter:
            Result := 'LAST_DAY(ADD_MONTHS(' + Result + ', TRUNC((MONTH(' + Result +
                ') - 1) / 3 + 1) * 3 - MONTH(' + Result + ')))';
        else
            Result := 'TO_DATE(''31' + DateSeparator + '12' + DateSeparator +
                ''' || TO_CHAR(YEAR(' + Result + ')), ''DD/MM/YYYY'')';
        end;
    etPRIOR: Result := Result + ' - 1';
    etNEXT:  Result := Result + ' + 1';
    end;
end;

function TOCIExpressionParser.ParseExprNatural(AEnableList: Boolean): POCIExpressionNode;
var
    d, m, q, y: String;
    dm, mm, qm, ym: TOCIExpressionToken;
    dmOf, mmOf: TOCIExpressionToken;
    s: String;
    Bmk: TOCIExprParserBmk;
begin
    Result := ParseExpr5(AEnableList);
    // List of values and after that natural lang - not supported !
    if (poNaturalLang in FParserOptions) and
       (FToken in [etFIRST, etLAST, etNEXT, etPRIOR, etTHIS,
                   etDAY, etMONTH, etQUARTER, etYEAR]) then begin
        d := '';
        m := '';
        q := '';
        y := '';
        dm := etEnd;
        mm := etEnd;
        qm := etEnd;
        ym := etEnd;
        dmOf := etEnd;
        mmOf := etEnd;
        while FToken in [etFIRST, etLAST, etNEXT, etPRIOR, etTHIS,
                         etDAY, etMONTH, etQUARTER, etYEAR] do begin
            if FToken in [etFIRST, etLAST, etNEXT, etPRIOR, etTHIS] then begin
                SaveState(Bmk);
                NextToken;
            end
            else begin
                Bmk.FToken := etABS;
                Bmk.FTokenString := '@@' + IntToStr(Integer(Pointer(Result)));
            end;
            case FToken of
            etDAY:
                begin
                    dm := Bmk.FToken;
                    if Bmk.FToken = etABS then
                        d := Bmk.FTokenString;
                end;
            etMONTH:
                begin
                    mm := Bmk.FToken;
                    if Bmk.FToken = etABS then
                        m := Bmk.FTokenString;
                end;
            etQUARTER:
                begin
                    qm := Bmk.FToken;
                    if Bmk.FToken = etABS then
                        q := Bmk.FTokenString;
                end;
            etYEAR:
                begin
                    ym := Bmk.FToken;
                    if Bmk.FToken = etABS then
                        y := Bmk.FTokenString;
                end;
            else
                DatabaseError(SExprTermination);
            end;
            if (dmOf = etEnd) and (FToken = etDAY) or
               (dmOf = etDAY) then
                dmOf := FToken;
            if (mmOf = etEnd) and (FToken = etMONTH) or
               (mmOf = etMONTH) then
                mmOf := FToken;
            NextToken;
            if FToken = etOF then
                NextToken;
            // Last succesfully parsed place in input stream
            SaveState(Bmk);
            if not (FToken in [etEND, etFIRST, etLAST, etNEXT, etPRIOR, etTHIS]) then
                try
                    Result := ParseExpr5(False);
                except
                end;
        end;
        // Back to good place, because
        // after that it is work of ParseExpr4 and higher
        RestoreState(Bmk);
        s := NaturalDateExpr(d, m, q, y, dm, dmOf, mm, mmOf, qm, ym);
        if s = '' then
            DatabaseError(SExprTermination);
        FInSQLLang := True;
        try
            FSourcePtr := PChar(s);
            NextToken;
            Result := ParseExpr5(False);
            if FToken <> etEnd then
                DatabaseError(SExprTermination);
        finally
            RestoreState(Bmk);
            FInSQLLang := False;
        end;
    end;
end;

function TOCIExpressionParser.ParseExpr5(AEnableList: Boolean): POCIExpressionNode;
const
    OperatorsList: array[etANY..etALL] of TOCIExpressionOperator = (
        canANY, canANY, canALL);
    Operators: array[etCONCAT..etSUB] of TOCIExpressionOperator = (
        canCONCAT, canADD, canSUB);
var
    Operator: TOCIExpressionOperator;
    Left, Right: POCIExpressionNode;
begin
    if FToken in [etANY, etSOME, etALL] then begin
        if not AEnableList then
            DatabaseErrorFmt(SExprExpected, [TokenName]);
        Operator := OperatorsList[FToken];
        NextToken;
        if FToken <> etLParen then
            DatabaseErrorFmt(SExprNoLParen, [TokenName]);
        NextToken;
        if FToken = etRParen then
            DatabaseError(SExprEmptyInList);
        Result := FExpression.NewNode(enOperator, Operator, Unassigned, nil, nil, -1);
        Result.FArgs := TList.Create;
        repeat
            Result.FArgs.Add(ParseExpr);
            if (FToken <> etComma) and (FToken <> etRParen) then
                DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
            if FToken = etComma then
                NextToken;
        until (FToken = etRParen) or (FToken = etEnd);
        if FToken <> etRParen then
            DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        NextToken;
        Result.FDataType := POCIExpressionNode(Result.FArgs.Items[0])^.FDataType;
        // ??? Result.FScopeKind := POCIExpressionNode(Result.FArgs.Items[0])^.FScopeKind;
    end
    else begin
        Result := ParseExpr6;
        while FToken in [etCONCAT, etADD, etSUB] do begin
            if not (poExtSyntax in FParserOptions) then
                DatabaseError(SExprNoArith);
            Operator := Operators[FToken];
            Left := Result;
            NextToken;
            Right := ParseExpr6;
            Result := FExpression.NewNode(enOperator, Operator, Unassigned,
                Left, Right, -1);
            TypeCheckArithOp(Result);
            GetScopeKind(Result, Left, Right);
        end;
    end;
end;

function TOCIExpressionParser.ParseExpr6: POCIExpressionNode;
const
    Operators: array[etMUL .. etDIV] of TOCIExpressionOperator = (
        canMUL, canDIV);
var
    Operator: TOCIExpressionOperator;
    Left, Right: POCIExpressionNode;
begin
    Result := ParseExpr7;
    while FToken in [etMUL, etDIV] do begin
        if not (poExtSyntax in FParserOptions) then
            DatabaseError(SExprNoArith);
        Operator := Operators[FToken];
        Left := Result;
        NextToken;
        Right := ParseExpr7;
        Result := FExpression.NewNode(enOperator, Operator, Unassigned,
            Left, Right, -1);
        TypeCheckArithOp(Result);
        GetScopeKind(Result, Left, Right);
    end;
end;

function TOCIExpressionParser.ParseExpr7: POCIExpressionNode;
var
    FuncName: string;
    FuncInd: Integer;
begin
    Result := nil;
    case FToken of
    etSymbol:
        begin
            FuncInd := TokenSymbolIsFunc(FTokenString);
            if (poExtSyntax in FParserOptions) and (FuncInd <> -1) then begin
                FuncName := FUCTokenString;
                if not NextTokenIsLParen then
                    Result := FExpression.NewNode(enFunc, canNOTDEFINED, FuncName,
                        nil, nil, FuncInd)
                else begin
                    NextToken;
                    if FToken <> etLParen then
                        DatabaseErrorFmt(SExprNoLParen, [TokenName]);
                    NextToken;
                    if (FuncName = 'COUNT') and (FToken = etMUL) then begin
                        FuncName := 'COUNT(*)';
                        NextToken;
                    end;
                    Result := FExpression.NewNode(enFunc, canNOTDEFINED, FuncName,
                        nil, nil, FuncInd);
                    if FToken <> etRParen then begin
                        Result.FArgs := TList.Create;
                        repeat
                            Result.FArgs.Add(ParseExpr);
                            if (FToken <> etComma) and (FToken <> etRParen) then
                                DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
                            if FToken = etComma then
                                NextToken;
                        until (FToken = etRParen) or (FToken = etEnd);
                    end;
                end;
                GetFuncResultInfo(Result);
            end
            else if TokenSymbolIs('NULL') then begin
                Result := FExpression.NewNode(enConst, canNOTDEFINED,
                    {$IFDEF OCI_D6} Variants {$ELSE} System {$ENDIF}.Null, nil, nil, -1);
                Result.FScopeKind := skConst;
            end
            else if TokenSymbolIs(FStrTrue) then begin
                Result := FExpression.NewNode(enConst, canNOTDEFINED, 1, nil,
                    nil, -1);
                Result.FScopeKind := skConst;
            end
            else if TokenSymbolIs(FStrFalse) then begin
                Result := FExpression.NewNode(enConst, canNOTDEFINED, 0, nil,
                    nil, -1);
                Result.FScopeKind := skConst;
            end
            else begin
                Result := FExpression.NewNode(enField, canNOTDEFINED,
                    FTokenString, nil, nil, -1);
                Result.FScopeKind := skField;
            end;
        end;
    etName:
        begin
            Result := FExpression.NewNode(enField, canNOTDEFINED, FTokenString,
                nil, nil, -1);
            Result.FScopeKind := skField;
        end;
    etNULL:
        begin
            Result := FExpression.NewNode(enConst, canNOTDEFINED, Null,
                nil, nil, -1);
            Result^.FDataType := ftUnknown;
            Result.FScopeKind := skConst;
        end;
    etLiteral:
        begin
            if FNumericLit then begin
                Result := FExpression.NewNode(enConst, canNOTDEFINED, FTokenNumber,
                    nil, nil, -1);
                Result^.FDataType := ftFloat;
            end
            else begin
                Result := FExpression.NewNode(enConst, canNOTDEFINED, FTokenString,
                    nil, nil, -1);
                Result^.FDataType := ftString;
            end;
            Result.FScopeKind := skConst;
        end;
    etLParen:
        begin
            NextToken;
            Result := ParseExpr;
            if FToken <> etRParen then
                DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        end;
    etEMBEDDING:
        Result := POCIExpressionNode(Pointer(StrToInt(FTokenString)));
    etFIRST, etLAST, etNEXT, etPRIOR, etTHIS, etABS, etDAY, etMONTH,
    etQUARTER, etYEAR, etOF:
        Exit;
    else
        DatabaseErrorFmt(SExprExpected, [TokenName]);
    end;
    NextToken;
end;

procedure TOCIExpressionParser.GetScopeKind(Root, Left, Right: POCIExpressionNode);
begin
    if (Left = nil) and (Right = nil) then
        Exit;
    if Right = nil then begin
        Root.FScopeKind := Left.FScopeKind;
        Exit;
    end;
    if ((Left^.FScopeKind = skField) and (Right^.FScopeKind = skAgg))
       or ((Left^.FScopeKind = skAgg) and (Right^.FScopeKind = skField)) then
        DatabaseError(SExprBadScope);
    if (Left^.FScopeKind = skConst) and (Right^.FScopeKind = skConst) then
        Root^.FScopeKind := skConst
    else if (Left^.FScopeKind = skAgg) or (Right^.FScopeKind = skAgg) then
        Root^.FScopeKind := skAgg
    else if (Left^.FScopeKind = skField) or (Right^.FScopeKind = skField) then
        Root^.FScopeKind := skField;
end;

procedure TOCIExpressionParser.GetFuncResultInfo(Node : POCIExpressionNode);
var
    i, n: Integer;
    pFuncDesc: TOCIFuncDesc;
begin
    i := TokenSymbolIsFunc(Node^.FData);
    if (Node^.FArgs = nil) then
        n := 0
    else
        n := Node^.FArgs.Count;
    pFuncDesc := GetFuncDesc(i);
    if (pFuncDesc.FArgMin > n) or (pFuncDesc.FArgMax < n) then
        DatabaseError(SExprTypeMis);
    if pFuncDesc.FDataType = ftUnknown then
        Node^.FDataType := POCIExpressionNode(Node^.FArgs.
            Items[pFuncDesc.FDataTypeArg])^.FDataType
    else
        Node^.FDataType := pFuncDesc.FDataType;
    if pFuncDesc.FScopeKind = skUnknown then
        Node^.FScopeKind := POCIExpressionNode(Node^.FArgs.
            Items[pFuncDesc.FScopeKindArg])^.FScopeKind
    else
        Node^.FScopeKind := pFuncDesc.FScopeKind;
end;

function TOCIExpressionParser.TokenName: string;
begin
    if FSourcePtr = FTokenPtr then
        Result := SExprNothing
    else begin
        SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
        Result := '''' + Result + '''';
    end;
end;

function TOCIExpressionParser.TokenSymbolIs(const S: string): Boolean;
begin
    Result := (FToken = etSymbol) and (FUCTokenString = S);
end;

function TOCIExpressionParser.TokenSymbolIsFunc(const S: string): Integer;
begin
    if not OCIFuncs.Find(UpperCase(S), Result) then
        Result := -1;
end;

procedure TOCIExpressionParser.TypeCheckArithOp(Node: POCIExpressionNode);
begin
    with Node^ do begin
        if IsNumeric(FLeft.FDataType) and IsNumeric(FRight.FDataType) then
            FDataType := ftFloat
        else if IsString(FLeft.FDataType) and IsString(FRight.FDataType) and
                ((FOperator = canADD) or (FOperator = canCONCAT)) then
            FDataType := ftString
        else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
                (FOperator = canADD) then
            FDataType := ftDateTime
        else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
                (FOperator = canSUB) then
            FDataType := FLeft.FDataType
        else if IsTemporal(FLeft.FDataType) and IsTemporal(FRight.FDataType) and
                (FOperator = canSUB) then
            FDataType := ftFloat
        else if IsString(FLeft.FDataType) and IsTemporal(FRight.FDataType) and
                (FOperator = canSUB) then begin
            FLeft.FDataType := FRight.FDataType;
            FDataType := ftFloat;
        end
        else if IsString(FLeft.FDataType) and IsNumeric(FRight.FDataType)and
                (FLeft.FKind = enConst) then
            FLeft.FDataType := ftDateTime
        else
            DatabaseError(SExprTypeMis);
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// TOCIFilter

{$IFNDEF OCI_EXPRESSION_STANDALONE}
destructor TOCIFilter.Destroy;
begin
    if IsDefaultFilter then begin
        DataSet.Filtered := False;
        DataSet.Filter := '';
        DataSet.FilterOptions := [];
    end;
    UnPrepare;
    inherited Destroy;
end;

function TOCIFilter.IsDefaultFilter: Boolean;
begin
    Result := (Name = SDefaultFltName) and (DataSet <> nil);
end;

function TOCIFilter.GetDisplayName: string;
begin
    if Name <> '' then
        Result := Name
    else
        Result := inherited GetDisplayName;
end;

procedure TOCIFilter.Assign(AValue: TPersistent);
begin
    if AValue is TOCIFilter then
        try
            if Collection <> nil then
                Collection.BeginUpdate;
            Active := False;
            Name := TOCIFilter(AValue).Name;
            Text := TOCIFilter(AValue).Text;
            Options := TOCIFilter(AValue).Options;
            Active := TOCIFilter(AValue).Active;
            OnFilterRecord := TOCIFilter(AValue).OnFilterRecord;
        finally
            if Collection <> nil then
                Collection.EndUpdate;
        end
    else
        inherited Assign(AValue);
end;

procedure TOCIFilter.Changed(ACanCache: Boolean);
begin
    if not (ACanCache and (DataSet <> nil) and DataSet.Active) then
        UnPrepare;
    FBroken := False;
    inherited Changed(False);
end;

procedure TOCIFilter.Prepare;
var
    parser: TOCIExpressionParser;
begin
    if not FPrepared and Active and (DataSet <> nil) and
       ((Text <> '') or Assigned(FOnFilterRecord)) then begin
        if Text <> '' then begin
            parser := TOCIExpressionParser.Create;
            try
                FExpression := parser.BuildExpression(DataSet, Text, Options, [poExtSyntax], '');
            finally
                parser.Free;
            end;
        end;
        FPrepared := True;
    end;
end;

procedure TOCIFilter.UnPrepare;
begin
    if FPrepared then begin
        FPrepared := False;
        FKeyFields := '';
        if FExpression <> nil then begin
            FExpression.Free;
            FExpression := nil;
        end;
    end;
end;

function TOCIFilter.Execute: Boolean;
begin
    if FBroken then
        Result := False
    else
        try
            Result := True;
            if FExpression <> nil then
                Result := FExpression.Execute;
            if Result and Assigned(FOnFilterRecord) then
                FOnFilterRecord(DataSet, Result);
        except
            OCIDBHandleException(Self);
            Result := False;
            FBroken := True;
        end;
end;

procedure TOCIFilter.SetActive(const Value: Boolean);
begin
    if (FActive <> Value) and ((DataSet <> nil) or not Value) then begin
        FActive := Value;
        if IsDefaultFilter then
            DataSet.Filtered := Value;
        Changed(True);
    end;
end;

function TOCIFilter.GetActive: Boolean;
begin
    Result := FActive and (
        (DataSet = nil) or
        (DataSet <> nil) and (
            (csDesigning in DataSet.ComponentState) or
            (DataSet is TOCIDataSet) and (
                UseInApplyUpdates or
                not TOCIDataSet(DataSet).InApplyUpdates
            )
        )
    );
end;

procedure TOCIFilter.SetOptions(const Value: TFilterOptions);
begin
    if FOptions <> Value then begin
        FOptions := Value;
        if IsDefaultFilter then
            DataSet.FilterOptions := Value;
        Changed(False);
    end;
end;

procedure TOCIFilter.SetText(const Value: String);
begin
    if FText <> Value then begin
        FText := Value;
        if IsDefaultFilter then
            DataSet.Filter := Value;
        Changed(False);
    end;
end;

procedure TOCIFilter.SetFilterRecord(const Value: TFilterRecordEvent);
begin
    if @FOnFilterRecord <> @Value then begin
        FOnFilterRecord := Value;
        if IsDefaultFilter then
            DataSet.OnFilterRecord := Value;
        Changed(False);
    end;
end;

function TOCIFilter.GetDataSet: TDataSet;
begin
    if not Assigned(Collection) then
        Result := nil
    else
        Result := TOciFilters(Collection).GetDataSet;
end;

function TOCIFilter.IsEqual(AValue: TOCIFilter): Boolean;
begin
    Result := (AValue <> nil) and
        (Text = AValue.Text) and
        (Name = AValue.Name) and
        (Options = AValue.Options) and
        (Active = AValue.Active) and
        (@OnFilterRecord = @AValue.OnFilterRecord);
end;

procedure TOCIFilter.PrepareLookupFilter(const AKeyFields: String;
    const AKeyValues: Variant; AOptions: TLocateOptions);
var
    i, n: Integer;
    Node: POCIExpressionNode;
    FilterOptions: TFilterOptions;
    Fields: TList;
begin
    FilterOptions := [];
    if loCaseInsensitive in AOptions then
        Include(FilterOptions, foCaseInsensitive);
    if not (loPartialKey in AOptions) then
        Include(FilterOptions, foNoPartialCompare);
    if FKeyFields = AKeyFields then begin
        FExpression.FOptions := FilterOptions;
        // One field only
        if FExpression.FRoot^.FOperator = canEQ then
            with FExpression.FRoot^ do begin
                FRight^.FData := AKeyValues;
                FPartial := (loPartialKey in AOptions);
            end
        else begin
            Node := FExpression.FRoot;
            i := 0;
            n := VarArrayHighBound(AKeyValues, 1);
            while (Node <> nil) and (Node^.FOperator in [canAND, canEQ]) do begin
                // AND.EQ.CONST
                if Node^.FOperator = canAND then
                    with Node^.FRight^ do begin
                        FRight^.FData := AKeyValues[n - i];
                        FPartial := (loPartialKey in AOptions);
                    end
                else if Node^.FOperator = canEQ then
                // EQ.CONST
                    with Node^ do begin
                        FRight^.FData := AKeyValues[n - i];
                        FPartial := (loPartialKey in AOptions);
                    end;
                Inc(i);
                // AND.[AND|EQ|FIELD]
                Node := Node^.FLeft;
            end;
        end
    end
    else begin
        UnPrepare;
        if DataSet = nil then
            Exit;
        Fields := TList.Create;
        try
            DataSet.GetFieldList(Fields, AKeyFields);
            FExpression := TOCIExpression.Create(DataSet, FilterOptions, [], '');
            if Fields.Count = 1 then
                FExpression.FRoot := FExpression.NewCompareNode(TField(Fields[0]),
                    canEQ, AKeyValues, (loPartialKey in AOptions))
            else
                for I := 0 to Fields.Count - 1 do begin
                    Node := FExpression.NewCompareNode(TField(Fields[I]), canEQ,
                        AKeyValues[I], (loPartialKey in AOptions));
                    if I = 0 then
                        FExpression.FRoot := Node
                    else
                        FExpression.FRoot := FExpression.NewNode(enOperator, canAND,
                            Unassigned, FExpression.FRoot, Node, -1);
                end;
            FPrepared := True;
            FKeyFields := AKeyFields;
        finally
            Fields.Free;
        end;
    end;
end;

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
{ TOCIFilters }

constructor TOCIFilters.Create(AOwner: TPersistent);
begin
    inherited Create(TOCIFilter);
    FOwner := AOwner;
end;

function TOCIFilters.GetOwner: TPersistent;
begin
    Result := FOwner;
end;

function TOCIFilters.GetDataSet: TDataSet;
begin
    if FOwner is TDataSet then
        Result := TDataSet(FOwner)
    else
        Result := nil;
end;

function TOCIFilters.IsEqual(Value: TOciFilters): Boolean;
var
    I: Integer;
begin
    Result := Count = Value.Count;
    if Result then
        for I := 0 to Count - 1 do begin
            Result := Items[I].IsEqual(Value.Items[I]);
            if not Result then
                Break;
        end;
end;

function TOCIFilters.GetItem(AIndex: Integer): TOCIFilter;
begin
    Result := TOCIFilter(inherited Items[AIndex]);
end;

procedure TOCIFilters.SetItem(AIndex: Integer; AValue: TOCIFilter);
begin
    inherited SetItem(AIndex, AValue);
end;

function TOCIFilters.FindFilter(const Value: string): TOciFilter;
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

function TOCIFilters.FilterByName(const Value: string): TOciFilter;
begin
    Result := FindFilter(Value);
    if Result = nil then
        OCIDBErrorFmt(msgOCIFilterNotFound, [Value], nil);
end;

function TOCIFilters.GetDefaultFilter: TOCIFilter;
begin
    Result := FindFilter(SDefaultFltName);
    if Result = nil then begin
        Result := TOCIFilter(Add);
        Result.Name := SDefaultFltName;
    end;
end;

function TOCIFilters.GetLookupFilter: TOCIFilter;
begin
    Result := FindFilter(SLookupFltName);
    if Result = nil then begin
        Result := TOCIFilter(Add);
        Result.Name := SLookupFltName;
    end;
end;

function TOCIFilters.Execute: Boolean;
var
    i: Integer;
begin
    Result := True;
    for i := 0 to Count - 1 do
        if Items[i].Active then begin
            Result := Items[i].Execute;
            if not Result then
                Break;
        end;
end;

procedure TOCIFilters.Update(Item: TCollectionItem);
begin
    if (DataSet <> nil) and (DataSet is TOCIDataSet) then
        with __TOCIDataSet(DataSet) do
            UpdateFiltering(True, Active);
end;

function TOCIFilters.GetAnyActive: Boolean;
var
    i: Integer;
begin
    Result := False;
    for i := 0 to Count - 1 do
        if Items[i].Active then begin
            Result := True;
            Exit;
        end;
end;
{$ENDIF}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
// Initialization && finalization

procedure FreeOCIFuncs;
var
    i: Integer;
begin
    for i := 0 to OCIFuncs.Count - 1 do
        OCIFuncs.Objects[i].Free;
    OCIFuncs.Free;
end;

procedure AllocOCIFuncs;
begin
    OCIFuncs := TStringList.Create;
    OCIFuncs.Capacity := 100;
    OCIFuncs.Sorted := True;
    OCIFuncs.Duplicates := dupError;

    OCIAddFunc('UPPER',     skUnknown,  0,  ftUnknown,   0,  1,  1,  FunUpper);
    OCIAddFunc('LOWER',     skUnknown,  0,  ftUnknown,   0,  1,  1,  FunLower);
    OCIAddFunc('SUBSTRING', skUnknown,  0,  ftUnknown,   0,  2,  3,  FunSubstring);
    OCIAddFunc('TRIM',      skUnknown,  0,  ftUnknown,   0,  1,  2,  FunTrim);
    OCIAddFunc('TRIMLEFT',  skUnknown,  0,  ftUnknown,   0,  1,  2,  FunTrimLeft);
    OCIAddFunc('TRIMRIGHT', skUnknown,  0,  ftUnknown,   0,  1,  2,  FunTrimRight);
    OCIAddFunc('YEAR',      skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunYear);
    OCIAddFunc('MONTH',     skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunMonth);
    OCIAddFunc('DAY',       skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunDay);
    OCIAddFunc('HOUR',      skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunHour);
    OCIAddFunc('MINUTE',    skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunMinute);
    OCIAddFunc('SECOND',    skUnknown,  0,  ftSmallInt, -1,  1,  1,  FunSecond);
    OCIAddFunc('GETDATE',   skConst,   -1,  ftDateTime, -1,  0,  0,  FunGetDate);
    OCIAddFunc('DATE',      skUnknown,  0,  ftDateTime, -1,  1,  1,  FunDate);
    OCIAddFunc('TIME',      skUnknown,  0,  ftDateTime, -1,  1,  1,  FunTime);
    OCIAddFunc('SUM',       skAgg,     -1,  ftFloat,    -1,  1,  1,  FunAggNotReady);
    OCIAddFunc('MIN',       skAgg,     -1,  ftUnknown,   0,  1,  1,  FunAggNotReady);
    OCIAddFunc('MAX',       skAgg,     -1,  ftUnknown,   0,  1,  1,  FunAggNotReady);
    OCIAddFunc('AVG',       skAgg,     -1,  ftFloat,    -1,  1,  1,  FunAggNotReady);
    OCIAddFunc('COUNT',     skAgg,     -1,  ftInteger,  -1,  0,  0,  FunAggNotReady);
    // ---------------------------------------------
    // Oracle's
    // Numeric
    OCIAddFunc('ABS',       skUnknown,  0,  ftFloat,    -1,  1,  1,  FunAbs);
    OCIAddFunc('CEIL',      skUnknown,  0,  ftInteger,  -1,  1,  1,  FunCeil);
    OCIAddFunc('COS',       skUnknown,  0,  ftFloat,    -1,  1,  1,  FunCos);
    OCIAddFunc('COSH',      skUnknown,  0,  ftFloat,    -1,  1,  1,  FunCosh);
    OCIAddFunc('EXP',       skUnknown,  0,  ftFloat,    -1,  1,  1,  FunExp);
    OCIAddFunc('FLOOR',     skUnknown,  0,  ftInteger,  -1,  1,  1,  FunFloor);
    OCIAddFunc('LN',        skUnknown,  0,  ftFloat,    -1,  1,  1,  FunLn);
    OCIAddFunc('LOG',       skUnknown,  0,  ftFloat,    -1,  2,  2,  FunLog);
    OCIAddFunc('MOD',       skUnknown,  0,  ftInteger,  -1,  2,  2,  FunMod);
    OCIAddFunc('POWER',     skUnknown,  0,  ftFloat,    -1,  2,  2,  FunPower);
    OCIAddFunc('ROUND',     skUnknown,  0,  ftFloat,    -1,  1,  2,  FunRound);
    OCIAddFunc('SIGN',      skUnknown,  0,  ftInteger,  -1,  1,  1,  FunSign);
    OCIAddFunc('SIN',       skUnknown,  0,  ftFloat,    -1,  1,  1,  FunSin);
    OCIAddFunc('SINH',      skUnknown,  0,  ftFloat,    -1,  1,  1,  FunSinh);
    OCIAddFunc('SQRT',      skUnknown,  0,  ftFloat,    -1,  1,  1,  FunSqrt);
    OCIAddFunc('TAN',       skUnknown,  0,  ftFloat,    -1,  1,  1,  FunTan);
    OCIAddFunc('TANH',      skUnknown,  0,  ftFloat,    -1,  1,  1,  FunTanh);
    OCIAddFunc('TRUNC',     skUnknown,  0,  ftFloat,    -1,  1,  2,  FunTrunc);
    OCIAddFunc('ROWNUM',    skField,   -1,  ftInteger,  -1,  0,  0,  FunRowNum);
    // Strings, result string
    OCIAddFunc('CHR',       skUnknown,  0,  ftString,   -1,  1,  1,  FunChr);
    OCIAddFunc('CONCAT',    skUnknown,  0,  ftUnknown,   0,  2,  2,  FunConcat);
    OCIAddFunc('INITCAP',   skUnknown,  0,  ftUnknown,   0,  1,  1,  FunInitCap);
    OCIAddFunc('LPAD',      skUnknown,  0,  ftUnknown,   0,  2,  3,  FunLPad);
    OCIAddFunc('LTRIM',     skUnknown,  0,  ftUnknown,   0,  1,  2,  FunTrimLeft);
    OCIAddFunc('REPLACE',   skUnknown,  0,  ftUnknown,   0,  2,  3,  FunReplace);
    OCIAddFunc('RPAD',      skUnknown,  0,  ftUnknown,   0,  2,  3,  FunRPad);
    OCIAddFunc('RTRIM',     skUnknown,  0,  ftUnknown,   0,  1,  2,  FunTrimRight);
    OCIAddFunc('SOUNDEX',   skUnknown,  0,  ftString,   -1,  1,  1,  FunSoundex);
    OCIAddFunc('SUBSTR',    skUnknown,  0,  ftUnknown,   0,  2,  3,  FunSubstring);
    OCIAddFunc('TRANSLATE', skUnknown,  0,  ftUnknown,   0,  3,  3,  FunTranslate);
    // Strings, result integer
    OCIAddFunc('ASCII',     skUnknown,  0,  ftInteger,  -1,  1,  1,  FunAscii);
    OCIAddFunc('INSTR',     skUnknown,  0,  ftInteger,  -1,  2,  4,  FunInstr);
    OCIAddFunc('LENGTH',    skUnknown,  0,  ftInteger,  -1,  1,  1,  FunLength);
    // Date
    OCIAddFunc('ADD_MONTHS',skUnknown,  0,  ftDateTime, -1,  2,  2,  FunAddMonths);
    OCIAddFunc('MONTHS_BETWEEN',
                            skUnknown,  0,  ftInteger,  -1,  2,  2,  FunMonthsBW);
    OCIAddFunc('LAST_DAY',  skUnknown,  0,  ftDateTime, -1,  1,  1,  FunLastDay);
    OCIAddFunc('FIRST_DAY', skUnknown,  0,  ftDateTime, -1,  1,  1,  FunFirstDay);
    OCIAddFunc('NEXT_DAY',  skUnknown,  0,  ftDateTime, -1,  2,  2,  FunNextDay);
    OCIAddFunc('SYSDATE',   skConst,   -1,  ftDateTime, -1,  0,  0,  FunGetDate);
    // Convert
    OCIAddFunc('TO_CHAR',   skUnknown,  0,  ftString,   -1,  1,  2,  FunToChar);
    OCIAddFunc('TO_DATE',   skUnknown,  0,  ftDateTime, -1,  1,  2,  FunToDate);
    OCIAddFunc('TO_NUMBER', skUnknown,  0,  ftFloat,    -1,  1,  2,  FunToNumber);
    // Others
    OCIAddFunc('DECODE',    skUnknown,  2,  ftUnknown,   2,  3,  MAXINT,  FunDecode);
    OCIAddFunc('IIF',       skUnknown,  1,  ftUnknown,   1,  3,  3,       FunIIF);
    OCIAddFunc('IF',        skUnknown,  1,  ftUnknown,   1,  3,  3,       FunIIF);
    OCIAddFunc('NVL',       skUnknown,  0,  ftUnknown,   0,  2,  2,       FunNvl);
    OCIAddFunc('GREATEST',  skUnknown,  0,  ftUnknown,   0,  1,  MAXINT,  FunGreatest);
    OCIAddFunc('LEAST',     skUnknown,  0,  ftUnknown,   0,  1,  MAXINT,  FunLeast);
{$IFNDEF OCI_EXPRESSION_STANDALONE}
    // Sequence
    OCIAddFunc('NEXTVAL',   skConst,    0,  ftFloat,    -1,  1,  2,  FunNextVal);
    OCIAddFunc('CURRVAL',   skConst,    0,  ftFloat,    -1,  1,  1,  FunCurrVal);
{$ENDIF}

    // Used in semi-natural language
    OCIAddFunc('TODAY',     skConst,   -1,  ftDateTime, -1,  0,  0,  FunGetDate);
end;

initialization

    AllocOCIFuncs;

finalization

    FreeOCIFuncs;

end.
