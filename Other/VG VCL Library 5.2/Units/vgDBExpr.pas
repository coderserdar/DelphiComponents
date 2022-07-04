{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         DataSet expression evaluation                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgDBExpr;

interface
{$IFDEF _D4_}
uses BDE, Classes, vgCanExp, DB, DBCommon;

type
{$IFDEF _D5_}
{ TAssignFilterExpr }
  TAssignFilterExpr = class
  private
    FDataSet: TDataSet;
    FFieldMap: TFieldMap;
    FOptions: TFilterOptions;
    FParserOptions: TParserOptions;
    FNodes: PExprNode;
    FExprBuffer: TExprData;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    FFieldName: string;
    FDependentFields: TBits;
    function FieldFromNode(Node: PExprNode): TField;
    function GetExprData(Pos, Size: Integer): PChar;
    function PutConstBCD(const Value: Variant; Decimals: Integer): Integer;
    function PutConstBool(const Value: Variant): Integer;
    function PutConstDate(const Value: Variant): Integer;
    function PutConstDateTime(const Value: Variant): Integer;
    function PutConstFloat(const Value: Variant): Integer;
    function PutConstInt(DataType: TFieldType; const Value: Variant): Integer;
    function PutConstNode(DataType: TFieldType; Data: PChar;
      Size: Integer): Integer;
    function PutConstStr(const Value: string): Integer;
    function PutConstTime(const Value: Variant): Integer;
    function PutData(Data: PChar; Size: Integer): Integer;
    function PutExprNode(Node: PExprNode; ParentOp: TCANOperator): Integer;
    function PutFieldNode(Field: TField; Node: PExprNode): Integer;
    function PutNode(NodeType: NodeClass; OpType: TCANOperator;
      OpCount: Integer): Integer;
    procedure SetNodeOp(Node, Index, Data: Integer);
    function PutConstant(Node: PExprNode): Integer;
    function GetFieldByName(Name: string) : TField;
  public
    constructor Create(DataSet: TDataSet; Options: TFilterOptions;
      ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits;
      FieldMap: TFieldMap);
    destructor Destroy; override;
    function NewCompareNode(Field: TField; Operator: TCANOperator;
      const Value: Variant): PExprNode;
    function NewNode(Kind: TExprNodeKind; Operator: TCANOperator;
      const Data: Variant; Left, Right: PExprNode): PExprNode;
    function GetFilterData(Root: PExprNode): TExprData;
    property DataSet: TDataSet write FDataSet;
  end;

{ TAssignExprParser }
  TAssignExprParser = class
  private
    FFilter: TAssignFilterExpr;
    FFieldMap: TFieldMap;
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FStrTrue: string;
    FStrFalse: string;
    FToken: TExprToken;
    FPrevToken: TExprToken;
    FFilterData: TExprData;
    FNumericLit: Boolean;
    FDataSize: Integer;
    FParserOptions: TParserOptions;
    FFieldName: string;
    FDataSet: TDataSet;
    FDependentFields: TBits;
    procedure NextToken;
    function NextTokenIsLParen : Boolean;
    function ParseExpr: PExprNode;
    function ParseExpr2: PExprNode;
    function ParseExpr3: PExprNode;
    function ParseExpr4: PExprNode;
    function ParseExpr5: PExprNode;
    function ParseExpr6: PExprNode;
    function ParseExpr7: PExprNode;
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
    function TokenSymbolIsFunc(const S: string) : Boolean;
    procedure GetFuncResultInfo(Node: PExprNode);
    procedure TypeCheckArithOp(Node: PExprNode);
    procedure GetScopeKind(Root, Left, Right : PExprNode);
  public
    constructor Create(DataSet: TDataSet; const Text: string;
      Options: TFilterOptions; ParserOptions: TParserOptions;
      const FieldName: string; DepFields: TBits; FieldMap: TFieldMap);
    destructor Destroy; override;
    procedure SetExprParams(const Text: string; Options: TFilterOptions;
      ParserOptions: TParserOptions; const FieldName: string);
    property FilterData: TExprData read FFilterData;
    property DataSize: Integer read FDataSize;
  end;

{$ELSE}

{ TAssignFilterExpr }
  TAssignFilterExpr = class
  private
    FDataSet: TDataSet;
    FOptions: TFilterOptions;
    FParserOptions: TParserOptions;
    FNodes: PExprNode;
    FExprBuffer: PCANExpr;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    FFieldName: string;
    FDependentFields: TBits;
    function FieldFromNode(Node: PExprNode): TField;
    function GetExprData(Pos, Size: Integer): PChar;
    function PutConstBCD(const Value: Variant; Decimals: Integer): Integer;
    function PutConstBool(const Value: Variant): Integer;
    function PutConstDate(const Value: Variant): Integer;
    function PutConstDateTime(const Value: Variant): Integer;
    function PutConstFloat(const Value: Variant): Integer;
    function PutConstInt(DataType: Integer; const Value: Variant): Integer;
    function PutConstNode(DataType: Integer; Data: PChar;
      Size: Integer): Integer;
    function PutConstStr(const Value: string): Integer;
    function PutConstTime(const Value: Variant): Integer;
    function PutData(Data: PChar; Size: Integer): Integer;
    function PutExprNode(Node: PExprNode; ParentOp: CanOp): Integer;
    function PutFieldNode(Field: TField; Node: PExprNode): Integer;
    function PutNode(NodeType: NodeClass; OpType: CanOp;
      OpCount: Integer): Integer;
    procedure SetNodeOp(Node, Index, Data: Integer);
    function PutConstant(Node: PExprNode): Integer;
    function GetFieldByName(Name: string) : TField;
  public
    constructor Create(DataSet: TDataSet; Options: TFilterOptions;
      ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits);
    destructor Destroy; override;
    function NewCompareNode(Field: TField; Operator: CanOp;
      const Value: Variant): PExprNode;
    function NewNode(Kind: TExprNodeKind; Operator: CanOp;
      const Data: Variant; Left, Right: PExprNode): PExprNode;
    function GetFilterData(Root: PExprNode): PCANExpr;
    property DataSet: TDataSet write FDataSet;
  end;

{ TAssignExprParser }
  TAssignExprParser = class
  private
    FFilter: TAssignFilterExpr;
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FStrTrue: string;
    FStrFalse: string;
    FToken: TExprToken;
    FPrevToken: TExprToken;
    FFilterData: PCANExpr;
    FNumericLit: Boolean;
    FDataSize: Integer;
    FParserOptions: TParserOptions;
    FFieldName: string;
    FDataSet: TDataSet;
    FDependentFields: TBits;
    procedure NextToken;
    function NextTokenIsLParen : Boolean;
    function ParseExpr: PExprNode;
    function ParseExpr2: PExprNode;
    function ParseExpr3: PExprNode;
    function ParseExpr4: PExprNode;
    function ParseExpr5: PExprNode;
    function ParseExpr6: PExprNode;
    function ParseExpr7: PExprNode;
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
    function TokenSymbolIsFunc(const S: string) : Boolean;
    procedure GetFuncResultInfo(Node: PExprNode);
    procedure TypeCheckArithOp(Node: PExprNode);
    procedure GetScopeKind(Root, Left, Right : PExprNode);
  public
    constructor Create(DataSet: TDataSet; const Text: string;
      Options: TFilterOptions; ParserOptions: TParserOptions;
      const FieldName: string; DepFields: TBits);
    destructor Destroy; override;
    procedure SetExprParams(const Text: string; Options: TFilterOptions;
      ParserOptions: TParserOptions; const FieldName: string);
    property FilterData: PCANExpr read FFilterData;
    property DataSize: Integer read FDataSize;
  end;
{$ENDIF}

{ TDBExprEvaluator }
  TDBExprEvaluator = class(TExprEvaluator)
  private
    FDataSet: TDataSet;
    FBmk: TBookmark;
    FExpression: string;
    procedure SetDataSet(Value: TDataSet);
    procedure SetExpression(Value: string);
    procedure UpdateData;
  protected
    { Abstract functions }
    function DoGetFieldValue(FieldNo: Word; const FieldName: string;
      var iFldType, iFldLen: Integer): Variant; override;
    procedure DoFirst; override;
    procedure DoNext; override;
    function DoGetEOF: Boolean; override;
    procedure DoBeginEvalute; override;
    procedure DoEndEvalute; override;
    procedure DoBeginAgg; override;
    procedure DoEndAgg; override;
  public
    procedure SetData(Value: PCanExpr); override;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Expression: string read FExpression write SetExpression;
  end;

{ TDBExpression }
  TDBExpression = class(TComponent)
  private
    FActive: Boolean;
    FDataLink: TDataLink;
    FEvaluator: TDBExprEvaluator;
    FInEvaluate: Boolean;
    FText: string;
    FOnChange: TNotifyEvent;
    procedure SetActive(Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetExpression: string;
    procedure SetExpression(Value: string);
    procedure SetText(Value: string);
    function GetValue: Variant;
  protected
    procedure RecordChanged;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: Variant read GetValue;
  published
    property Active: Boolean read FActive write SetActive default True;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Expression: string read GetExpression write SetExpression;
    property Text: string read FText write SetText stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
{$ENDIF}

implementation
{$IFDEF _D4_}
uses Windows, DBConsts, SysUtils, vgDB {$IFDEF _D4_}, vgDBUtl{$ENDIF};

{$IFDEF _D5_}
{ TAssignFilterExpr }
constructor TAssignFilterExpr.Create(DataSet: TDataSet; Options: TFilterOptions;
  ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits;
  FieldMap: TFieldMap);
begin
  FFieldMap := FieldMap;
  FDataSet := DataSet;
  FOptions := Options;
  FFieldName := FieldName;
  FParserOptions := ParseOptions;
  FDependentFields := DepFields;
end;

destructor TAssignFilterExpr.Destroy;
var
  Node: PExprNode;
begin
  SetLength(FExprBuffer, 0);
  while FNodes <> nil do
  begin
    Node := FNodes;
    FNodes := Node^.FNext;
    if (Node^.FKind = enFunc) and (Node^.FArgs <> nil) then
      Node^.FArgs.Free;
    Dispose(Node);
  end;
end;

function TAssignFilterExpr.FieldFromNode(Node: PExprNode): TField;
begin
  Result := GetFieldByName(Node^.FData);
  if not (Result.FieldKind in [fkData, fkInternalCalc]) then
    DatabaseErrorFmt(SExprBadField, [Result.FieldName]);
end;

function TAssignFilterExpr.GetExprData(Pos, Size: Integer): PChar;
begin
  SetLength(FExprBuffer, FExprBufSize + Size);
  Move(FExprBuffer[Pos], FExprBuffer[Pos + Size], FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
end;

function TAssignFilterExpr.GetFilterData(Root: PExprNode): TExprData;
begin
  FExprBufSize := CANExprSize;
  SetLength(FExprBuffer, FExprBufSize);
  PutExprNode(Root, coNOTDEFINED);
  PWord(@FExprBuffer[0])^ := CANEXPRVERSION;                { iVer }
  PWord(@FExprBuffer[2])^ := FExprBufSize;                  { iTotalSize }
  PWord(@FExprBuffer[4])^ := $FFFF;                         { iNodes }
  PWord(@FExprBuffer[6])^ := CANEXPRSIZE;                   { iNodeStart }
  PWord(@FExprBuffer[8])^ := FExprNodeSize + CANEXPRSIZE;   { iLiteralStart }
  Result := FExprBuffer;
end;

function TAssignFilterExpr.NewCompareNode(Field: TField; Operator: TCANOperator;
  const Value: Variant): PExprNode;
var
  ConstExpr: PExprNode;
begin
  ConstExpr := NewNode(enConst, coNOTDEFINED, Value, nil, nil);
  ConstExpr^.FDataType := Field.DataType;
  ConstExpr^.FDataSize := Field.Size;
  Result := NewNode(enOperator, Operator, Unassigned,
    NewNode(enField, coNOTDEFINED, Field.FieldName, nil, nil), ConstExpr);
end;

function TAssignFilterExpr.NewNode(Kind: TExprNodeKind; Operator: TCANOperator;
  const Data: Variant; Left, Right: PExprNode): PExprNode;
var
  Field : TField;
begin
  New(Result);
  with Result^ do
  begin
    FNext := FNodes;
    FKind := Kind;
    FPartial := False;
    FOperator := Operator;
    FData := Data;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
  if Kind = enField then
  begin
    Field := GetFieldByName(Data);
    if Field = nil then
      DatabaseErrorFmt(SFieldNotFound, [Data]);
    Result^.FDataType := Field.DataType;
    Result^.FDataSize := Field.Size;
  end;
end;

function TAssignFilterExpr.PutConstBCD(const Value: Variant;
  Decimals: Integer): Integer;
var
  C: Currency;
  BCD: TBcd;
begin
  if VarType(Value) = varString then
    C := StrToCurr(string(TVarData(Value).VString)) else
    C := Value;
  CurrToBCD(C, BCD, 32, Decimals);
  Result := PutConstNode(ftBCD, @BCD, 18);
end;

function TAssignFilterExpr.PutConstBool(const Value: Variant): Integer;
var
  B: WordBool;
begin
  B := Value;
  Result := PutConstNode(ftBoolean, @B, SizeOf(WordBool));
end;

function TAssignFilterExpr.PutConstDate(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToDate(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftDate, @TimeStamp.Date, 4);
end;

function TAssignFilterExpr.PutConstDateTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  DateData: Double;
begin
  if VarType(Value) = varString then
    DateTime := StrToDateTime(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  DateData := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
  Result := PutConstNode(ftDateTime, @DateData, 8);
end;

function TAssignFilterExpr.PutConstFloat(const Value: Variant): Integer;
var
  F: Double;
begin
  if VarType(Value) = varString then
    F := StrToFloat(string(TVarData(Value).VString)) else
    F := Value;
  Result := PutConstNode(ftFloat, @F, SizeOf(Double));
end;

function TAssignFilterExpr.PutConstInt(DataType: TFieldType;
  const Value: Variant): Integer;
var
  I, Size: Integer;
begin
  if VarType(Value) = varString then
    I := StrToInt(string(TVarData(Value).VString)) else
    I := Value;
  Size := 2;
  case DataType of
    ftSmallint:
      if (I < -32768) or (I > 32767) then DatabaseError(SExprRangeError);
    ftWord:
      if (I < 0) or (I > 65535) then DatabaseError(SExprRangeError);
  else
    Size := 4;
  end;
  Result := PutConstNode(DataType, @I, Size);
end;

function TAssignFilterExpr.PutConstNode(DataType: TFieldType; Data: PChar;
  Size: Integer): Integer;
begin
  Result := PutNode(nodeCONST, coCONST2, 3);
  SetNodeOp(Result, 0, FFieldMap[DataType]);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
end;

function TAssignFilterExpr.PutConstStr(const Value: string): Integer;
var
  Str: string;
  Buffer: array[0..255] of Char;
begin
  if Length(Value) >= SizeOf(Buffer) then
    Str := Copy(Value, 1, SizeOf(Buffer) - 1) else
    Str := Value;
  FDataSet.Translate(PChar(Str), Buffer, True);
  Result := PutConstNode(ftString, Buffer, Length(Str) + 1);
end;

function TAssignFilterExpr.PutConstTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToTime(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftTime, @TimeStamp.Time, 4);
end;

function TAssignFilterExpr.PutData(Data: PChar; Size: Integer): Integer;
begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
end;

function TAssignFilterExpr.PutConstant(Node: PExprNode): Integer;
begin
  Result := 0;
  case Node^.FDataType of
    ftSmallInt, ftInteger, ftWord, ftAutoInc:
      Result := PutConstInt(Node^.FDataType, Node^.FData);
    ftFloat, ftCurrency:
      Result := PutConstFloat(Node^.FData);
    ftString, ftWideString, ftFixedChar, ftGuid:
      Result := PutConstStr(Node^.FData);
    ftDate:
      Result := PutConstDate(Node^.FData);
    ftTime:
      Result := PutConstTime(Node^.FData);
    ftDateTime:
      Result := PutConstDateTime(Node^.FData);
    ftBoolean:
      Result := PutConstBool(Node^.FData);
    ftBCD:
      Result := PutConstBCD(Node^.FData, Node^.FDataSize);
    else
      DatabaseErrorFmt(SExprBadConst, [Node^.FData]);
  end;
end;

function TAssignFilterExpr.PutExprNode(Node: PExprNode; ParentOp: TCANOperator): Integer;
const
  ReverseOperator: array[coEQ..coLE] of TCANOperator = (coEQ, coNE, coLT,
    coGT, coLE, coGE);
  BoolFalse: WordBool = False;
var
  Field: TField;
  Left, Right, Temp : PExprNode;
  LeftPos, RightPos, ListElem, PrevListElem, I: Integer;
  Operator: TCANOperator;
  CaseInsensitive, PartialLength, L:  Integer;
  S: string;
begin
  Result := 0;
  case Node^.FKind of
    enField:
      begin
        Field := FieldFromNode(Node);
        if (ParentOp in [coOR, coNOT, coAND, coNOTDEFINED]) and
           (Field.DataType = ftBoolean) then
        begin
          Result := PutNode(nodeBINARY, coNE, 2);
          SetNodeOp(Result, 0, PutFieldNode(Field, Node));
          SetNodeOp(Result, 1, PutConstNode(ftBoolean, @BoolFalse, SizeOf(WordBool)));
        end
        else
          Result := PutFieldNode(Field, Node);
      end;
    enConst:
      Result := PutConstant(Node);
    enOperator:
      case Node^.FOperator of
        coIN:
          begin
            Result := PutNode(nodeBINARY, coIN, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
            ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
            SetNodeOp(Result, 1, ListElem);
            PrevListElem := ListElem;
            for I := 0 to Node^.FArgs.Count - 1 do 
            begin
              LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
              if I = 0 then 
                begin
                  SetNodeOp(PrevListElem, 0, LeftPos);
                  SetNodeOp(PrevListElem, 1, 0);
                end
              else
                begin
                  ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
                  SetNodeOp(ListElem, 0, LeftPos);
                  SetNodeOp(ListElem, 1, 0);
                  SetNodeOp(PrevListElem, 1, ListElem);
                  PrevListElem := ListElem;
                end;
              end;
          end;
        coNOT,
        coISBLANK,
        coNOTBLANK:
          begin
            Result := PutNode(nodeUNARY, Node^.FOperator, 1);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
          end;
        coEQ..coLE,
        coAND,coOR,
        coADD..coDIV,
        coLIKE,
        coASSIGN:
          begin
            Operator := Node^.FOperator;
            Left := Node^.FLeft;
            Right := Node^.FRight;
            if (Operator in [coEQ..coLE]) and (Right^.FKind = enField) and
               (Left^.FKind <> enField) then
            begin
              Temp := Left;
              Left := Right;
              Right := Temp;
              Operator := ReverseOperator[Operator];
            end;

            Result := 0;
            if (Left^.FKind = enField) and (Right^.FKind = enConst)
               and ((Node^.FOperator = coEQ)  or (Node^.FOperator = coNE)
               or (Node^.FOperator = coLIKE)) then
            begin
              if VarIsNull(Right^.FData) then
              begin
                case Node^.FOperator of
                  coEQ: Operator := coISBLANK;
                  coNE: Operator := coNOTBLANK;
                else
                  DatabaseError(SExprBadNullTest);
                end;
                Result := PutNode(nodeUNARY, Operator, 1);
                SetNodeOp(Result, 0, PutExprNode(Left,Node^.FOperator));
              end
              else if (Right^.FDataType in StringFieldTypes) then
              begin
                S := Right^.FData;
                L := Length(S);
                if L <> 0 then
                begin
                  CaseInsensitive := 0;
                  PartialLength := 0;
                  if foCaseInsensitive in FOptions then CaseInsensitive := 1;
                  if Node^.FPartial then PartialLength := L else
                    if not (foNoPartialCompare in FOptions) and (L > 1) and
                      (S[L] = '*') then
                    begin
                      Delete(S, L, 1);
                      PartialLength := L - 1;
                    end;
                  if (CaseInsensitive <> 0) or (PartialLength <> 0) then
                  begin
                    Result := PutNode(nodeCOMPARE, Operator, 4);
                    SetNodeOp(Result, 0, CaseInsensitive);
                    SetNodeOp(Result, 1, PartialLength);
                    SetNodeOp(Result, 2, PutExprNode(Left,Node^.FOperator));
                    SetNodeOp(Result, 3, PutConstStr(S));
                  end;
                end;
              end;
            end;

            if Result = 0 then
            begin
              if (Operator = coISBLANK) or (Operator = coNOTBLANK) then
              begin
                Result := PutNode(nodeUNARY, Operator, 1);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                SetNodeOp(Result, 0, LeftPos);
              end else
              begin
                Result := PutNode(nodeBINARY, Operator, 2);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                {!}
                if Operator <> coASSIGN then
                  RightPos := PutExprNode(Right,Node^.FOperator) else
                  RightPos := LeftPos;
                SetNodeOp(Result, 0, LeftPos);
                SetNodeOp(Result, 1, RightPos);
              end;
            end;
          end;
      end;
    enFunc:
      begin
        Result := PutNode(nodeFUNC, coFUNC2, 2);
        SetNodeOp(Result, 0,  PutData(PChar(string(Node^.FData)),
          Length(string(Node^.FData)) + 1));
        if Node^.FArgs <> nil then
        begin
          ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
          SetNodeOp(Result, 1, ListElem);
          PrevListElem := ListElem;
          for I := 0 to Node^.FArgs.Count - 1 do
          begin
            LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
            if I = 0 then
            begin
              SetNodeOp(PrevListElem, 0, LeftPos);
              SetNodeOp(PrevListElem, 1, 0);
            end
            else
            begin
              ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
              SetNodeOp(ListElem, 0, LeftPos);
              SetNodeOp(ListElem, 1, 0);
              SetNodeOp(PrevListElem, 1, ListElem);
              PrevListElem := ListElem;
            end;
          end;
        end else
          SetNodeOp(Result, 1, 0);
      end;
  end;
end;


function TAssignFilterExpr.PutFieldNode(Field: TField; Node: PExprNode): Integer;
var
  Buffer: array[0..255] of Char;
begin
  if poFieldNameGiven in FParserOptions then
    FDataSet.Translate(PChar(Field.FieldName), Buffer, True)
  else
    FDataSet.Translate(PChar(string(Node^.FData)), Buffer, True);
  Result := PutNode(nodeFIELD, coFIELD2, 2);
  SetNodeOp(Result, 0, Field.FieldNo);
  SetNodeOp(Result, 1, PutData(Buffer, StrLen(Buffer) + 1));
end;

function TAssignFilterExpr.PutNode(NodeType: NodeClass; OpType: TCANOperator;
  OpCount: Integer): Integer;
var
  Size: Integer;
  Data: PChar;
begin
  Size := CANHDRSIZE + OpCount * SizeOf(Word);
  Data := GetExprData(CANEXPRSIZE + FExprNodeSize, Size);
  PInteger(@Data[0])^ := Integer(NodeType); { CANHdr.nodeClass }
  PInteger(@Data[4])^ := Integer(OpType);   { CANHdr.coOp }
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
end;

procedure TAssignFilterExpr.SetNodeOp(Node, Index, Data: Integer);
begin
  PWordArray(PChar(FExprBuffer) + (CANEXPRSIZE + Node +
    CANHDRSIZE))^[Index] := Data;
end;

function TAssignFilterExpr.GetFieldByName(Name: string) : TField;
var
  I: Integer;
  F: TField;
  FieldInfo: TFieldInfo;
begin
  Result := nil;
  if poFieldNameGiven in FParserOptions then
    Result := FDataSet.FieldByName(FFieldName)
  else if poUseOrigNames in FParserOptions then
  begin
    for I := 0 to FDataset.FieldCount - 1 do
    begin
      F := FDataSet.Fields[I];
      if GetFieldInfo(F.Origin, FieldInfo) and
         (AnsiCompareStr(Name, FieldInfo.OriginalFieldName) = 0) then
      begin
        Result := F;
        Exit;
      end;
    end;
  end;
  if Result = nil then
    Result := FDataSet.FieldByName(Name);
  {
  if (Result <> nil) and (Result.FieldKind = fkCalculated) and (poAggregate in FParserOptions) then
    DatabaseErrorFmt(SExprNoAggOnCalcs, [Result.FieldName]);
  }
  if (poFieldDepend in FParserOptions) and (Result <> nil) and
     (FDependentFields <> nil) then
    FDependentFields[Result.FieldNo-1] := True;
end;

{ TAssignExprParser }
constructor TAssignExprParser.Create(DataSet: TDataSet; const Text: string;
  Options: TFilterOptions; ParserOptions: TParserOptions; const FieldName: string;
  DepFields: TBits; FieldMap: TFieldMap);
begin
  FFieldMap := FieldMap;
  FStrTrue := STextTrue;
  FStrFalse := STextFalse;
  FDataSet := DataSet;
  FDependentFields := DepFields;
  FFilter := TAssignFilterExpr.Create(DataSet, Options, ParserOptions, FieldName,
    DepFields, FieldMap);
  if Text <> '' then
    SetExprParams(Text, Options, ParserOptions, FieldName);
end;

destructor TAssignExprParser.Destroy;
begin
  FFilter.Free;
end;

procedure  TAssignExprParser.SetExprParams(const Text: string; Options: TFilterOptions;
  ParserOptions: TParserOptions; const FieldName: string);
var
  Root, DefField: PExprNode;
begin
  FParserOptions := ParserOptions;
  if FFilter <> nil then
    FFilter.Free;
  FFilter := TAssignFilterExpr.Create(FDataSet, Options, ParserOptions, FieldName,
    FDependentFields, FFieldMap);
  FText := Text;
  FSourcePtr := PChar(Text);
  FFieldName := FieldName;
  NextToken;
  Root := ParseExpr;
  if FToken <> etEnd then DatabaseError(SExprTermination);
  {
  if (poAggregate in FParserOptions) and (Root^.FScopeKind <> skAgg) then
     DatabaseError(SExprNotAgg);
  if (not (poAggregate in FParserOptions)) and (Root^.FScopeKind = skAgg) then
     DatabaseError(SExprNoAggFilter);
  }
  if poDefaultExpr in ParserOptions then
  begin
    {!}
    if FFieldName = '' then
    begin
      DefField := FFilter.NewNode(enConst, coNOTDEFINED, '', nil, nil);
      DefField.FKind := enField;
    end else begin
      DefField := FFilter.NewNode(enField, coNOTDEFINED, FFieldName, nil, nil);
      if (IsTemporal(DefField^.FDataType) and (Root^.FDataType in StringFieldTypes)) or
       ((DefField^.FDataType = ftBoolean ) and (Root^.FDataType in StringFieldTypes)) then
      Root^.FDataType := DefField^.FDataType;

      if not ((IsTemporal(DefField^.FDataType) and IsTemporal(Root^.FDataType))
         or (IsNumeric(DefField^.FDataType) and IsNumeric(Root^.FDataType))
         or ((DefField^.FDataType in StringFieldTypes) and (Root^.FDataType in StringFieldTypes))
         or ((DefField^.FDataType = ftBoolean) and (Root^.FDataType = ftBoolean))) then
        DatabaseError(SExprTypeMis);
    end;

    Root := FFilter.NewNode(enOperator, coASSIGN, Unassigned, Root, DefField);
  end;

  if not (poAggregate in FParserOptions) and not(poDefaultExpr in ParserOptions)
     and (Root^.FDataType <> ftBoolean ) then
     DatabaseError(SExprIncorrect);

  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
end;

function TAssignExprParser.NextTokenIsLParen : Boolean;
var
  P : PChar;
begin
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P^ = '(';
end;

procedure TAssignExprParser.NextToken;
type
  ASet = Set of Char;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

  procedure Skip(TheSet: ASet);
  begin
    while TRUE do
    begin
      if P^ in LeadBytes then
        Inc(P, 2)
      else if (P^ in TheSet) or IsKatakana(Byte(P^)) then
        Inc(P)
      else
        Exit;
    end;
  end;

begin
  FPrevToken := FToken;
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  if (P^ <> #0) and (P^ = '/') and (P[1] <> #0) and (P[1] = '*')then
  begin
    P := P + 2;
    while (P^ <> #0) and (P^ <> '*') do Inc(P);
    if (P^ = '*') and (P[1] <> #0) and (P[1] =  '/')  then
      P := P + 2
    else
      DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']'] do Inc(P);
        end
        else
          Skip(['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']']);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
        if CompareText(FTokenString, 'LIKE') = 0 then   { do not localize }
          FToken := etLIKE
        else if CompareText(FTokenString, 'IN') = 0 then   { do not localize }
          FToken := etIN
        else if CompareText(FTokenString, 'IS') = 0 then    { do not localize }
        begin
          while (P^ <> #0) and (P^ <= ' ') do Inc(P);
          TokenStart := P;
          Skip(['A'..'Z', 'a'..'z']);
          SetString(FTokenString, TokenStart, P - TokenStart);
          if CompareText(FTokenString, 'NOT')= 0 then  { do not localize }
          begin
            while (P^ <> #0) and (P^ <= ' ') do Inc(P);
            TokenStart := P;
            Skip(['A'..'Z', 'a'..'z']);
            SetString(FTokenString, TokenStart, P - TokenStart);
            if CompareText(FTokenString, 'NULL') = 0 then
              FToken := etISNOTNULL
            else
              DatabaseError(SInvalidKeywordUse);
          end
          else if CompareText (FTokenString, 'NULL') = 0  then  { do not localize }
          begin
            FToken := etISNULL;
          end
          else
            DatabaseError(SInvalidKeywordUse);
        end;
      end;
    '[':
      begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        if P = nil then DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    '''':
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then DatabaseError(SExprStringError);
          if P^ = '''' then
          begin
            Inc(P);
            if P^ <> '''' then Break;
          end;
          if L < SizeOf(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
        FNumericLit := False;
      end;
    '-', '0'..'9':
      begin
        if (FPrevToken <> etLiteral) and (FPrevToken <> etName) and
           (FPrevToken <> etSymbol)and (FPrevToken <> etRParen) then
          begin
            TokenStart := P;
            Inc(P);
            while (P^ in ['0'..'9', DecimalSeparator, 'e', 'E', '+', '-']) do
              Inc(P);
            if ((P-1)^ = ',') and (DecimalSeparator = ',') and (P^ = ' ') then
              Dec(P);
            SetString(FTokenString, TokenStart, P - TokenStart);
            FToken := etLiteral;
            FNumericLit := True;
          end
        else
         begin
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
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then
        begin
          Inc(P);
          FToken := etGE;
        end else
          FToken := etGT;
      end;
    '+':
      begin
        Inc(P);
        FToken := etADD;
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
    #0:
      FToken := etEnd;
  else
    DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  FSourcePtr := P;
end;

function TAssignExprParser.ParseExpr: PExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coOR, Unassigned,
      Result, ParseExpr2);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := ftBoolean;
  end;
end;

function TAssignExprParser.ParseExpr2: PExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coAND, Unassigned,
      Result, ParseExpr3);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := ftBoolean;
  end;
end;

function TAssignExprParser.ParseExpr3: PExprNode;
begin
  if TokenSymbolIs('NOT') then
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coNOT, Unassigned,
      ParseExpr4, nil);
    Result^.FDataType := ftBoolean;
  end else
    Result := ParseExpr4;
  GetScopeKind(Result, Result^.FLeft, Result^.FRight);
end;


function TAssignExprParser.ParseExpr4: PExprNode;
const
  Operators: array[etEQ..etLT] of TCANOperator = (
    coEQ, coNE, coGE, coLE, coGT, coLT);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr5;
  if (FToken in [etEQ..etLT]) or (FToken = etLIKE)
     or (FToken = etISNULL) or (FToken = etISNOTNULL)
     or (FToken = etIN) then
  begin
    case FToken of
      etEQ..etLT:
        Operator := Operators[FToken];
      etLIKE:
        Operator := coLIKE;
      etISNULL:
        Operator := coISBLANK;
      etISNOTNULL:
        Operator := coNOTBLANK;
      etIN:
        Operator := coIN;
      else
        Operator := coNOTDEFINED;
    end;
    NextToken;
    Left := Result;
    if Operator = coIN then
    begin
      if FToken <> etLParen then 
        DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
      NextToken;
      Result := FFilter.NewNode(enOperator, coIN, Unassigned,
                 Left, nil);
      Result.FDataType := ftBoolean;
      if FToken <> etRParen then
      begin
        Result.FArgs := TList.Create;
        repeat
          Right := ParseExpr;
          if IsTemporal(Left.FDataType) then
            Right.FDataType := Left.FDataType;
          Result.FArgs.Add(Right);
          if (FToken <> etComma) and (FToken <> etRParen) then
            DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
          if FToken = etComma then NextToken;
        until (FToken = etRParen) or (FToken = etEnd);
        if FToken <> etRParen then
          DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        NextToken;
      end else
        DatabaseError(SExprEmptyInList);
    end else
    begin
      if (Operator <> coISBLANK) and (Operator <> coNOTBLANK) then
        Right := ParseExpr5
      else
        Right := nil;
      Result := FFilter.NewNode(enOperator, Operator, Unassigned,
        Left, Right);
      if Right <> nil then
      begin
        if (Left^.FKind = enField) and (Right^.FKind = enConst) then
          begin
            Right^.FDataType := Left^.FDataType;
            Right^.FDataSize := Left^.FDataSize;
          end
        else if (Right^.FKind = enField) and (Left^.FKind = enConst) then
          begin
            Left^.FDataType := Right^.FDataType;
            Left^.FDataSize := Right^.FDataSize;
          end;
      end;
      if (Left^.FDataType in BlobFieldTypes) and (Operator = coLIKE) then
      begin
        if Right^.FKind = enConst then Right^.FDataType := ftString;
      end
      else if (Operator <> coISBLANK) and (Operator <> coNOTBLANK)
         and ((Left^.FDataType in (BlobFieldTypes + [ftBytes])) or
         ((Right <> nil) and (Right^.FDataType in (BlobFieldTypes + [ftBytes])))) then
        DatabaseError(SExprTypeMis);
      Result.FDataType := ftBoolean;
      if Right <> nil then
      begin
        if IsTemporal(Left.FDataType) and (Right.FDataType in StringFieldTypes) then
          Right.FDataType := Left.FDataType
        else if IsTemporal(Right.FDataType) and (Left.FDataType in StringFieldTypes) then
          Left.FDataType := Right.FDataType;
      end;
      GetScopeKind(Result, Left, Right);
    end;
  end;
end;

function TAssignExprParser.ParseExpr5: PExprNode;
const
  Operators: array[etADD..etDIV] of TCANOperator = (
    coADD, coSUB, coMUL, coDIV);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr6;
  while FToken in [etADD, etSUB] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr6;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;

function TAssignExprParser.ParseExpr6: PExprNode;
const
  Operators: array[etADD..etDIV] of TCANOperator = (
    coADD, coSUB, coMUL, coDIV);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr7;
  while FToken in [etMUL, etDIV] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr7;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;


function TAssignExprParser.ParseExpr7: PExprNode;
var
  FuncName: string;
begin
  case FToken of
    etSymbol:
      if (poExtSyntax in FParserOptions)
         and  NextTokenIsLParen and TokenSymbolIsFunc(FTokenString) then
        begin
          Funcname := FTokenString;
          NextToken;
          if FToken <> etLParen then 
            DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
          NextToken;
          if (CompareText(FuncName,'count') = 0) and (FToken = etMUL) then 
          begin
            FuncName := 'COUNT(*)';
            NextToken;
          end;
          Result := FFilter.NewNode(enFunc, coNOTDEFINED, FuncName,
                    nil, nil);
          if FToken <> etRParen then
          begin
            Result.FArgs := TList.Create;
            repeat
              Result.FArgs.Add(ParseExpr);
              if (FToken <> etComma) and (FToken <> etRParen) then
                DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]); 
              if FToken = etComma then NextToken;
            until (FToken = etRParen) or (FToken = etEnd);
          end else 
            Result.FArgs := nil;

          GetFuncResultInfo(Result);
        end
      else if TokenSymbolIs('NULL') then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, System.Null, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrTrue) then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 1, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrFalse) then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 0, nil, nil);
          Result.FScopeKind := skConst;
        end
      else
        begin
          Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, nil, nil);
          Result.FScopeKind := skField;
        end;
    etName:
      begin
        Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, nil, nil);
        Result.FScopeKind := skField;
      end;
    etLiteral:
      begin
        Result := FFilter.NewNode(enConst, coNOTDEFINED, FTokenString, nil, nil);
        if FNumericLit then Result^.FDataType := ftFloat else
           Result^.FDataType := ftString;
        Result.FScopeKind := skConst;
      end;
    etLParen:
      begin
        NextToken;
        Result := ParseExpr;
        if FToken <> etRParen then DatabaseErrorFmt(SExprNoRParen, [TokenName]);
      end;
  else
    DatabaseErrorFmt(SExprExpected, [TokenName]);
    Result := nil;
  end;
  NextToken;
end;

procedure  TAssignExprParser.GetScopeKind(Root, Left, Right : PExprNode);
begin
  if (Left = nil) and (Right = nil) then Exit;
  if Right = nil then
  begin
    Root.FScopeKind := Left.FScopeKind;
    Exit;
  end;
{  if ((Left^.FScopeKind = skField) and (Right^.FScopeKind = skAgg))
     or ((Left^.FScopeKind = skAgg) and (Right^.FScopeKind = skField)) then
    DatabaseError(SExprBadScope);
}
  if (Left^.FScopeKind = skConst) and (Right^.FScopeKind = skConst) then
    Root^.FScopeKind := skConst
  else if (Left^.FScopeKind = skAgg) or (Right^.FScopeKind = skAgg) then
    Root^.FScopeKind := skAgg
  else if (Left^.FScopeKind = skField) or (Right^.FScopeKind = skField) then
    Root^.FScopeKind := skField;
end;

procedure TAssignExprParser.GetFuncResultInfo(Node : PExprNode);
begin
  Node^.FDataType := ftString;
  if (CompareText(Node^.FData, 'COUNT(*)') <> 0 )
     and (CompareText(Node^.FData,'GETDATE') <> 0 )
     and ( (Node^.FArgs = nil ) or ( Node^.FArgs.Count = 0) ) then
      DatabaseError(SExprTypeMis);

  if (Node^.FArgs <> nil) and (Node^.FArgs.Count > 0) then
     Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  if (CompareText(Node^.FData , 'SUM') = 0) or
     (CompareText(Node^.FData , 'AVG') = 0) then
  begin
    Node^.FDataType := ftFloat;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'MIN') = 0) or
          (CompareText(Node^.FData , 'MAX') = 0) then
  begin
    Node^.FDataType := PExprNode(Node^.FArgs.Items[0])^.FDataType;
    Node^.FScopeKind := skAgg;
  end
  else if  (CompareText(Node^.FData , 'COUNT') = 0) or
           (CompareText(Node^.FData , 'COUNT(*)') = 0) then
  begin
    Node^.FDataType := ftInteger;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'YEAR') = 0) or
          (CompareText(Node^.FData , 'MONTH') = 0) or
          (CompareText(Node^.FData , 'DAY') = 0) or
          (CompareText(Node^.FData , 'HOUR') = 0) or
          (CompareText(Node^.FData , 'MINUTE') = 0) or
          (CompareText(Node^.FData , 'SECOND') = 0 ) then
  begin
    Node^.FDataType := ftInteger;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'GETDATE') = 0  then
  begin
    Node^.FDataType := ftDateTime;
    Node^.FScopeKind := skConst;
  end
  else if CompareText(Node^.FData , 'DATE') = 0  then
  begin
    Node^.FDataType := ftDate;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'TIME') = 0  then
  begin
    Node^.FDataType := ftTime;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end;
end;

function TAssignExprParser.TokenName: string;
begin
  if FSourcePtr = FTokenPtr then Result := SExprNothing else
  begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TAssignExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (CompareText(FTokenString, S) = 0);
end;


function TAssignExprParser.TokenSymbolIsFunc(const S: string) : Boolean;
begin
  Result := (CompareText(S, 'UPPER') = 0) or
            (CompareText(S, 'LOWER') = 0) or
            (CompareText(S, 'SUBSTRING') = 0) or
            (CompareText(S, 'TRIM') = 0) or
            (CompareText(S, 'TRIMLEFT') = 0) or
            (CompareText(S, 'TRIMRIGHT') = 0) or
            (CompareText(S, 'YEAR') = 0) or
            (CompareText(S, 'MONTH') = 0) or
            (CompareText(S, 'DAY') = 0) or
            (CompareText(S, 'HOUR') = 0) or
            (CompareText(S, 'MINUTE') = 0) or
            (CompareText(S, 'SECOND') = 0) or
            (CompareText(S, 'GETDATE') = 0) or
            (CompareText(S, 'DATE') = 0) or
            (CompareText(S, 'TIME') = 0) or
            (CompareText(S, 'SUM') = 0) or
            (CompareText(S, 'MIN') = 0) or
            (CompareText(S, 'MAX') = 0) or
            (CompareText(S, 'AVG') = 0) or
            (CompareText(S, 'COUNT') = 0);

end;

procedure  TAssignExprParser.TypeCheckArithOp(Node: PExprNode);
begin
  with Node^ do
  begin
    if IsNumeric(FLeft.FDataType) and IsNumeric(FRight.FDataType)  then
      FDataType := ftFloat
    else if (FLeft.FDataType in StringFieldTypes) and
       (FRight.FDataType in StringFieldTypes) and (FOperator = coADD) then
      FDataType := ftString
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = coADD) then
      FDataType := ftDateTime
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = coSUB) then
      FDataType := FLeft.FDataType
    else if IsTemporal(FLeft.FDataType) and IsTemporal(FRight.FDataType) and
       (FOperator = coSUB) then
      FDataType := ftFloat
    else if (FLeft.FDataType in StringFieldTypes) and IsTemporal(FRight.FDataType) and
       (FOperator = coSUB) then
    begin
      FLeft.FDataType := FRight.FDataType;
      FDataType := ftFloat;
    end
    else if ( FLeft.FDataType in StringFieldTypes) and  IsNumeric(FRight.FDataType )and
         (FLeft.FKind = enConst)  then
      FLeft.FDataType := ftDateTime
    else
      DatabaseError(SExprTypeMis);
  end;
end;

{$ELSE}

function IsNumeric(DataType: Integer): Boolean;
begin
  Result := vgCanExp.IsNumeric(DataType);
end;

function IsTemporal(DataType: Integer): Boolean;
begin
  Result := vgCanExp.IsTemporal(DataType);
end;

{ TAssignFilterExpr }
constructor TAssignFilterExpr.Create(DataSet: TDataSet; Options: TFilterOptions;
  ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits);
begin
  FDataSet := DataSet;
  FOptions := Options;
  FFieldName := FieldName;
  FParserOptions := ParseOptions;
  FDependentFields := DepFields;
end;

destructor TAssignFilterExpr.Destroy;
var
  Node: PExprNode;
begin
  FreeMem(FExprBuffer, FExprBufSize);
  while FNodes <> nil do
  begin
    Node := FNodes;
    FNodes := Node^.FNext;
    if (Node^.FKind = enFunc) and (Node^.FArgs <> nil) then
      Node^.FArgs.Free; 
    Dispose(Node);
  end;
end;

function TAssignFilterExpr.FieldFromNode(Node: PExprNode): TField;
begin
  Result := GetFieldByName(Node^.FData);
  if not (Result.FieldKind in [fkData, fkInternalCalc]) then
    DatabaseErrorFmt(SExprBadField, [Result.FieldName]);
end;

function TAssignFilterExpr.GetExprData(Pos, Size: Integer): PChar;
begin
  ReallocMem(FExprBuffer, FExprBufSize + Size);
  Move(PChar(FExprBuffer)[Pos], PChar(FExprBuffer)[Pos + Size],
    FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
end;

function TAssignFilterExpr.GetFilterData(Root: PExprNode): PCANExpr;
begin
  FExprBufSize := SizeOf(CANExpr);
  GetMem(FExprBuffer, FExprBufSize);
  PutExprNode(Root, canNOTDEFINED);
  with FExprBuffer^ do
  begin
    iVer := CANEXPRVERSION;
    iTotalSize := FExprBufSize;
    iNodes := $FFFF;
    iNodeStart := SizeOf(CANExpr);
    iLiteralStart := FExprNodeSize + SizeOf(CANExpr);
  end;
  Result := FExprBuffer;
end;

function TAssignFilterExpr.NewCompareNode(Field: TField; Operator: CanOp;
  const Value: Variant): PExprNode;
var 
  ConstExpr: PExprNode;
begin
  ConstExpr := NewNode(enConst, canNOTDEFINED, Value, nil, nil);
  ConstExpr^.FDataType := FldTypeMap[ Field.DataType ];
  ConstExpr^.FDataSize := Field.Size;
  Result := NewNode(enOperator, Operator, Unassigned,
    NewNode(enField, canNOTDEFINED, Field.FieldName, nil, nil), ConstExpr);
end;

function TAssignFilterExpr.NewNode(Kind: TExprNodeKind; Operator: CanOp;
  const Data: Variant; Left, Right: PExprNode): PExprNode;
var
  Field : TField;
begin
  New(Result);
  with Result^ do
  begin
    FNext := FNodes;
    FKind := Kind;
    FPartial := False;
    FOperator := Operator;
    FData := Data;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
  if Kind = enField then
  begin
    Field := GetFieldByName(Data);
    if Field = nil then
      DatabaseErrorFmt(SFieldNotFound, [Data]);
    Result^.FDataType := FldTypeMap[ Field.DataType ];
    Result^.FDataSize := Field.Size;
  end;
end;

function TAssignFilterExpr.PutConstBCD(const Value: Variant;
  Decimals: Integer): Integer;
var
  C: Currency;
  BCD: FMTBcd;
begin
  if VarType(Value) = varString then
    C := StrToCurr(string(TVarData(Value).VString)) else
    C := Value;
  CurrToFMTBCD(C, BCD, 32, Decimals);
  Result := PutConstNode(fldBCD, @BCD, 18);
end;

function TAssignFilterExpr.PutConstBool(const Value: Variant): Integer;
var
  B: WordBool;
begin
  B := Value;
  Result := PutConstNode(fldBOOL, @B, SizeOf(WordBool));
end;

function TAssignFilterExpr.PutConstDate(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToDate(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(fldDATE, @TimeStamp.Date, 4);
end;

function TAssignFilterExpr.PutConstDateTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  DateData: Double;
begin
  if VarType(Value) = varString then
    DateTime := StrToDateTime(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  DateData := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
  Result := PutConstNode(fldTIMESTAMP, @DateData, 8);
end;

function TAssignFilterExpr.PutConstFloat(const Value: Variant): Integer;
var
  F: Double;
begin
  if VarType(Value) = varString then
    F := StrToFloat(string(TVarData(Value).VString)) else
    F := Value;
  Result := PutConstNode(fldFLOAT, @F, SizeOf(Double));
end;

function TAssignFilterExpr.PutConstInt(DataType: Integer;
  const Value: Variant): Integer;
var
  I, Size: Integer;
begin
  if VarType(Value) = varString then
    I := StrToInt(string(TVarData(Value).VString)) else
    I := Value;
  Size := 2;
  case DataType of
    fldINT16:
      if (I < -32768) or (I > 32767) then DatabaseError(SExprRangeError);
    fldUINT16:
      if (I < 0) or (I > 65535) then DatabaseError(SExprRangeError);
  else
    Size := 4;
  end;
  Result := PutConstNode(DataType, @I, Size);
end;

function TAssignFilterExpr.PutConstNode(DataType: Integer; Data: PChar;
  Size: Integer): Integer;
begin
  Result := PutNode(nodeCONST, canCONST2, 3);
  SetNodeOp(Result, 0, DataType);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
end;

function TAssignFilterExpr.PutConstStr(const Value: string): Integer;
var
  Str: string;
  Buffer: array[0..255] of Char;
begin
  if Length(Value) >= SizeOf(Buffer) then
    Str := Copy(Value, 1, SizeOf(Buffer) - 1) else
    Str := Value;
  FDataSet.Translate(PChar(Str), Buffer, True);
  Result := PutConstNode(fldZSTRING, Buffer, Length(Str) + 1);
end;

function TAssignFilterExpr.PutConstTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToTime(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(fldTIME, @TimeStamp.Time, 4);
end;

function TAssignFilterExpr.PutData(Data: PChar; Size: Integer): Integer;
begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
end;

function TAssignFilterExpr.PutConstant(Node: PExprNode): Integer;
begin
  Result := 0;
  case Node^.FDataType of
    fldINT16, fldINT32, fldUINT16:
      Result := PutConstInt(Node^.FDataType, Node^.FData);
    fldFLOAT:
      Result := PutConstFloat(Node^.FData);
    fldZSTRING:
      Result := PutConstStr(Node^.FData);
    fldDATE:
      Result := PutConstDate(Node^.FData);
    fldTIME:
      Result := PutConstTime(Node^.FData);
    fldTIMESTAMP:
      Result := PutConstDateTime(Node^.FData);
    fldBOOL:
      Result := PutConstBool(Node^.FData);
    fldBCD:
      Result := PutConstBCD(Node^.FData, Node^.FDataSize);
    else
      DatabaseErrorFmt(SExprBadConst, [Node^.FData]); 
  end;
end;

function TAssignFilterExpr.PutExprNode(Node: PExprNode; ParentOp: CanOp): Integer;
const
  ReverseOperator: array[canEQ..canLE] of CanOp = (canEQ, canNE, canLT,
    canGT, canLE, canGE);
  BoolFalse: WordBool = False;
var
  Field: TField;
  Left, Right, Temp : PExprNode;
  LeftPos, RightPos, ListElem, PrevListElem, I: Integer;
  Operator: CanOp;
  CaseInsensitive, PartialLength, L:  Integer;
  S: string;
begin
  Result := 0;
  case Node^.FKind of
    enField:
      begin
        Field := FieldFromNode(Node);
        if (ParentOp in [canOR, canNOT, canAND, canNOTDEFINED]) and
           (Field.DataType = ftBoolean) then
        begin
          Result := PutNode(nodeBINARY, canNE, 2);
          SetNodeOp(Result, 0, PutFieldNode(Field, Node));
          SetNodeOp(Result, 1, PutConstNode(fldBOOL, @BoolFalse,
            SizeOf(WordBool)));
        end
        else
          Result := PutFieldNode(Field, Node);
      end;
    enConst:
      Result := PutConstant(Node);
    enOperator:
      case Node^.FOperator of
        canIN:
          begin
            Result := PutNode(nodeBINARY, canIN, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
            ListElem := PutNode(nodeLISTELEM, canLISTELEM2, 2);
            SetNodeOp(Result, 1, ListElem);
            PrevListElem := ListElem;
            for I := 0 to Node^.FArgs.Count - 1 do 
            begin
              LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
              if I = 0 then 
                begin
                  SetNodeOp(PrevListElem, 0, LeftPos);
                  SetNodeOp(PrevListElem, 1, 0);
                end
              else
                begin
                  ListElem := PutNode(nodeLISTELEM, canLISTELEM2, 2);
                  SetNodeOp(ListElem, 0, LeftPos);
                  SetNodeOp(ListElem, 1, 0);
                  SetNodeOp(PrevListElem, 1, ListElem);
                  PrevListElem := ListElem;
                end;
              end;
          end;
        canNOT,
        canISBLANK,
        canNOTBLANK:
          begin
            Result := PutNode(nodeUNARY, Node^.FOperator, 1);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
          end;
        canEQ..canLE,
        canAND,canOR,
        canADD..canDIV,
        canLIKE,
        canASSIGN:
          begin
            Operator := Node^.FOperator;
            Left := Node^.FLeft;
            Right := Node^.FRight;
            if (Operator in [CanEQ..canLE]) and (Right^.FKind = enField) and
               (Left^.FKind <> enField) then
            begin
              Temp := Left;
              Left := Right;
              Right := Temp;
              Operator := ReverseOperator[Operator];
            end;

            Result := 0;
            if (Left^.FKind = enField) and (Right^.FKind = enConst)
               and ((Node^.FOperator = canEQ)  or (Node^.FOperator = canNE)
               or (Node^.FOperator = canLIKE)) then
            begin
              if VarIsNull(Right^.FData) then
              begin
                case Node^.FOperator of
                  canEQ: Operator := canISBLANK;
                  canNE: Operator := canNOTBLANK;
                else
                  DatabaseError(SExprBadNullTest);
                end;
                Result := PutNode(nodeUNARY, Operator, 1);
                SetNodeOp(Result, 0, PutExprNode(Left,Node^.FOperator));
              end
              else if (Right^.FDataType = fldZSTRING) then
              begin
                S := Right^.FData;
                L := Length(S);
                if L <> 0 then
                begin
                  CaseInsensitive := 0;
                  PartialLength := 0;
                  if foCaseInsensitive in FOptions then CaseInsensitive := 1;
                  if Node^.FPartial then PartialLength := L else
                    if not (foNoPartialCompare in FOptions) and (L > 1) and
                      (S[L] = '*') then
                    begin
                      Delete(S, L, 1);
                      PartialLength := L - 1;
                    end;
                  if (CaseInsensitive <> 0) or (PartialLength <> 0) then
                  begin
                    Result := PutNode(nodeCOMPARE, Operator, 4);
                    SetNodeOp(Result, 0, CaseInsensitive);
                    SetNodeOp(Result, 1, PartialLength);
                    SetNodeOp(Result, 2, PutExprNode(Left,Node^.FOperator));
                    SetNodeOp(Result, 3, PutConstStr(S));
                  end;
                end;
              end;
            end;

            if Result = 0 then
            begin
              if (Operator = canISBLANK) or (Operator = canNOTBLANK) then
              begin
                Result := PutNode(nodeUNARY, Operator, 1);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                SetNodeOp(Result, 0, LeftPos);
              end else
              begin
                Result := PutNode(nodeBINARY, Operator, 2);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                if Operator <> canASSIGN then
                  RightPos := PutExprNode(Right,Node^.FOperator) else
                  RightPos := LeftPos;
                SetNodeOp(Result, 0, LeftPos);
                SetNodeOp(Result, 1, RightPos);
              end;
            end;
          end;
      end;
    enFunc:
      begin
        Result := PutNode(nodeFUNC, canFUNC2, 2);
        SetNodeOp(Result, 0,  PutData(PChar(string(Node^.FData)),
          Length(string(Node^.FData)) + 1));
        if Node^.FArgs <> nil then
        begin
          ListElem := PutNode(nodeLISTELEM, canLISTELEM2, 2);
          SetNodeOp(Result, 1, ListElem);
          PrevListElem := ListElem;
          for I := 0 to Node^.FArgs.Count - 1 do
          begin
            LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
            if I = 0 then
            begin
              SetNodeOp(PrevListElem, 0, LeftPos);
              SetNodeOp(PrevListElem, 1, 0);
            end
            else
            begin
              ListElem := PutNode(nodeLISTELEM, canLISTELEM2, 2);
              SetNodeOp(ListElem, 0, LeftPos);
              SetNodeOp(ListElem, 1, 0);
              SetNodeOp(PrevListElem, 1, ListElem);
              PrevListElem := ListElem;
            end;
          end;
        end else
          SetNodeOp(Result, 1, 0);
      end;
  end;
end;


function TAssignFilterExpr.PutFieldNode(Field: TField; Node: PExprNode): Integer;
var
  Buffer: array[0..255] of Char;
begin
  if poFieldNameGiven in FParserOptions then
    FDataSet.Translate(PChar(Field.FieldName), Buffer, True)
  else
    FDataSet.Translate(PChar(string(Node^.FData)), Buffer, True);
  Result := PutNode(nodeFIELD, canFIELD2, 2);
  SetNodeOp(Result, 0, Field.FieldNo);
  SetNodeOp(Result, 1, PutData(Buffer, StrLen(Buffer) + 1));
end;

function TAssignFilterExpr.PutNode(NodeType: NodeClass; OpType: CanOp;
  OpCount: Integer): Integer;
var
  Size: Integer;
begin
  Size := SizeOf(CANHdr) + OpCount * SizeOf(Word);
  with PCANHdr(GetExprData(SizeOf(CANExpr) + FExprNodeSize, Size))^ do
  begin
    nodeClass := NodeType;
    canOp := OpType;
  end;
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
end;

procedure TAssignFilterExpr.SetNodeOp(Node, Index, Data: Integer);
begin
  PWordArray(PChar(FExprBuffer) + (SizeOf(CANExpr) + Node +
    SizeOf(CANHdr)))^[Index] := Data;
end;

function TAssignFilterExpr.GetFieldByName(Name: string) : TField;
var
  I: Integer;
  F: TField;
  ADataSources: DataSources;
begin
  Result := nil;
  if poFieldNameGiven in FParserOptions then
    Result := FDataSet.FieldByName(FFieldName)
  else if poUseOrigNames in FParserOptions then
  begin
    for I := 0 to FDataset.FieldCount - 1 do
    begin
      F := FDataSet.Fields[I];
      StrCopy(ADataSources.szSourceFldName, PChar(F.FieldName));
      if GetFieldSource(FDataSet, ADataSources)
         and (AnsiStrComp(PChar(Name), ADataSources.szOrigFldName) = 0) then
      begin
        Result := F;
        Exit;
      end;
    end;
  end;
  if Result = nil then
    Result := FDataSet.FieldByName(Name);
  if (poFieldDepend in FParserOptions) and (Result <> nil) and
     (FDependentFields <> nil) then
    FDependentFields[Result.FieldNo-1] := True;
end;

{ TAssignExprParser }
constructor TAssignExprParser.Create(DataSet: TDataSet; const Text: string;
  Options: TFilterOptions; ParserOptions: TParserOptions; const FieldName: string;
  DepFields: TBits);
begin
  FStrTrue := STextTrue;
  FStrFalse := STextFalse;
  FDataSet := DataSet;
  FDependentFields := DepFields;
  FFilter := TAssignFilterExpr.Create(DataSet, Options, ParserOptions, FieldName, DepFields);
  if Text <> '' then
    SetExprParams(Text, Options, ParserOptions, FieldName);
end;

destructor TAssignExprParser.Destroy;
begin
  FFilter.Free;
end;

procedure  TAssignExprParser.SetExprParams(const Text: string; Options: TFilterOptions;
  ParserOptions: TParserOptions; const FieldName: string);
var
  Root, DefField: PExprNode;
begin
  FParserOptions := ParserOptions;
  if FFilter <> nil then
    FFilter.Free;
  FFilter := TAssignFilterExpr.Create(FDataSet, Options, ParserOptions, FieldName,
    FDependentFields);
  FText := Text;
  FSourcePtr := PChar(Text);
  FFieldName := FieldName;
  NextToken;
  Root := ParseExpr;
  if FToken <> etEnd then DatabaseError(SExprTermination);
//  if (poAggregate in FParserOptions) and (Root^.FScopeKind <> skAgg) then
//     DatabaseError(SExprNotAgg);
  if (not (poAggregate in FParserOptions)) and (Root^.FScopeKind = skAgg) then
     DatabaseError(SExprNoAggFilter);
  if poDefaultExpr in ParserOptions then
  begin
    if FFieldName = '' then
    begin
      DefField := FFilter.NewNode(enConst, canNOTDEFINED, '', nil, nil);
      DefField.FKind := enField;
    end else begin
      DefField := FFilter.NewNode(enField, canNOTDEFINED, FFieldName, nil, nil);
      if (IsTemporal(DefField^.FDataType) and (Root^.FDataType = fldZSTRING)) or
         ((DefField^.FDataType = fldBOOL ) and (Root^.FDataType = fldZSTRING)) then
        Root^.FDataType := DefField^.FDataType;


      if not ((IsTemporal(DefField^.FDataType) and IsTemporal(Root^.FDataType))
         or (IsNumeric(DefField^.FDataType) and IsNumeric(Root^.FDataType))
         or ((DefField^.FDataType = fldZSTRING) and (Root^.FDataType = fldZSTRING))
         or ((DefField^.FDataType = fldBOOL) and (Root^.FDataType = fldBOOL))) then
        DatabaseError(SExprTypeMis);
    end;
    Root := FFilter.NewNode(enOperator, canASSIGN, Unassigned, Root, DefField);
  end;

  if not (poAggregate in FParserOptions) and not(poDefaultExpr in ParserOptions)
     and (Root^.FDataType <> fldBOOL ) then
     DatabaseError(SExprIncorrect);
  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
end;

function TAssignExprParser.NextTokenIsLParen : Boolean;
var
  P : PChar;
begin
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P^ = '(';
end;

procedure TAssignExprParser.NextToken;
type
  ASet = Set of Char;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

  procedure Skip(TheSet: ASet);
  begin
    while TRUE do
    begin
      if P^ in LeadBytes then
        Inc(P, 2)
      else if (P^ in TheSet) or IsKatakana(Byte(P^)) then
        Inc(P)
      else
        Exit;
    end;
  end;

begin
  FPrevToken := FToken;
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  if (P^ <> #0) and (P^ = '/') and (P[1] <> #0) and (P[1] = '*')then
  begin
    P := P + 2;
    while (P^ <> #0) and (P^ <> '*') do Inc(P);
    if (P^ = '*') and (P[1] <> #0) and (P[1] =  '/')  then
      P := P + 2
    else
      DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']'] do Inc(P);
        end
        else
          Skip(['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']']);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
        if CompareText(FTokenString, 'LIKE') = 0 then   { do not localize }
          FToken := etLIKE
        else if CompareText(FTokenString, 'IN') = 0 then   { do not localize }
          FToken := etIN
        else if CompareText(FTokenString, 'IS') = 0 then    { do not localize }
        begin
          while (P^ <> #0) and (P^ <= ' ') do Inc(P);
          TokenStart := P;
          Skip(['A'..'Z', 'a'..'z']);
          SetString(FTokenString, TokenStart, P - TokenStart);
          if CompareText(FTokenString, 'NOT')= 0 then  { do not localize }
          begin
            while (P^ <> #0) and (P^ <= ' ') do Inc(P);
            TokenStart := P;
            Skip(['A'..'Z', 'a'..'z']);
            SetString(FTokenString, TokenStart, P - TokenStart);
            if CompareText(FTokenString, 'NULL') = 0 then
              FToken := etISNOTNULL
            else
              DatabaseError(SInvalidKeywordUse);
          end
          else if CompareText (FTokenString, 'NULL') = 0  then  { do not localize }
          begin
            FToken := etISNULL;
          end
          else
            DatabaseError(SInvalidKeywordUse);
        end;
      end;
    '[':
      begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        if P = nil then DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    '''':
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then DatabaseError(SExprStringError);
          if P^ = '''' then
          begin
            Inc(P);
            if P^ <> '''' then Break;
          end;
          if L < SizeOf(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
        FNumericLit := False;
      end;
    '-', '0'..'9':
      begin
        if (FPrevToken <> etLiteral) and (FPrevToken <> etName) and
           (FPrevToken <> etSymbol)and (FPrevToken <> etRParen) then
          begin
            TokenStart := P;
            Inc(P);
            while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do Inc(P);
            SetString(FTokenString, TokenStart, P - TokenStart);
            FToken := etLiteral;
            FNumericLit := True;
          end
        else
         begin
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
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then
        begin
          Inc(P);
          FToken := etGE;
        end else
          FToken := etGT;
      end;
    '+':
      begin
        Inc(P);
        FToken := etADD;
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
    #0:
      FToken := etEnd;
  else
    DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  FSourcePtr := P;
end;

function TAssignExprParser.ParseExpr: PExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canOR, Unassigned,
      Result, ParseExpr2);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := fldBOOL;
  end;
end;

function TAssignExprParser.ParseExpr2: PExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canAND, Unassigned,
      Result, ParseExpr3);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := fldBOOL;
  end;
end;

function TAssignExprParser.ParseExpr3: PExprNode;
begin
  if TokenSymbolIs('NOT') then
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canNOT, Unassigned,
      ParseExpr4, nil);
    Result^.FDataType := fldBOOL;
  end else
    Result := ParseExpr4;
  GetScopeKind(Result, Result^.FLeft, Result^.FRight);
end;


function TAssignExprParser.ParseExpr4: PExprNode;
const
  Operators: array[etEQ..etLT] of CanOp = (
    canEQ, canNE, canGE, canLE, canGT, canLT);
var
  Operator: CanOp;
  Left, Right: PExprNode;
begin
  Result := ParseExpr5;
  if (FToken in [etEQ..etLT]) or (FToken = etLIKE)
     or (FToken = etISNULL) or (FToken = etISNOTNULL)
     or (FToken = etIN) then
  begin
    case FToken of
      etEQ..etLT:
        Operator := Operators[FToken];
      etLIKE:
        Operator := canLIKE;
      etISNULL:
        Operator := canISBLANK;
      etISNOTNULL:
        Operator := canNOTBLANK;
      etIN:
        Operator := canIN;
      else
        Operator := canNOTDEFINED;
    end;
    NextToken;
    Left := Result;
    if Operator = canIN then
    begin
      if FToken <> etLParen then 
        DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
      NextToken;
      Result := FFilter.NewNode(enOperator, canIN, Unassigned,
                 Left, nil);
      Result.FDataType := fldBOOL;
      if FToken <> etRParen then
      begin
        Result.FArgs := TList.Create;
        repeat
          Right := ParseExpr;
          if IsTemporal(Left.FDataType) then
            Right.FDataType := Left.FDataType;
          Result.FArgs.Add(Right);
          if (FToken <> etComma) and (FToken <> etRParen) then
            DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
          if FToken = etComma then NextToken;
        until (FToken = etRParen) or (FToken = etEnd);
        if FToken <> etRParen then
          DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        NextToken;
      end else
        DatabaseError(SExprEmptyInList);
    end else
    begin
      if (Operator <> canISBLANK) and (Operator <> canNOTBLANK) then
        Right := ParseExpr5
      else
        Right := nil;
      Result := FFilter.NewNode(enOperator, Operator, Unassigned,
        Left, Right);
      if Right <> nil then
      begin
        if (Left^.FKind = enField) and (Right^.FKind = enConst) then
          begin
            Right^.FDataType := Left^.FDataType;
            Right^.FDataSize := Left^.FDataSize;
          end
        else if (Right^.FKind = enField) and (Left^.FKind = enConst) then
          begin
            Left^.FDataType := Right^.FDataType;
            Left^.FDataSize := Right^.FDataSize;
          end;
      end;
      if (Left^.FDataType = fldBLOB) and (Operator = canLIKE) then
      begin
        if Right^.FKind = enConst then Right^.FDataType := fldZSTRING;
      end 
      else if (Operator <> canISBLANK) and (Operator <> canNOTBLANK)
         and ((Left^.FDataType in [ fldBLOB, fldBYTES]) or 
         ((Right <> nil) and (Right^.FDataType in [ fldBLOB, fldBYTES]))) then
        DatabaseError(SExprTypeMis);
      Result.FDataType := fldBOOL;
      if Right <> nil then
      begin
        if IsTemporal(Left.FDataType) and (Right.FDataType = fldZSTRING) then
          Right.FDataType := Left.FDataType
        else if IsTemporal(Right.FDataType) and (Left.FDataType = fldZSTRING) then
          Left.FDataType := Right.FDataType;
      end;
      GetScopeKind(Result, Left, Right);
    end;
  end;
end;

function TAssignExprParser.ParseExpr5: PExprNode;
const
  Operators: array[etADD..etDIV] of CanOp = (
    canADD, canSUB, canMUL, canDIV);
var
  Operator: CanOp;
  Left, Right: PExprNode;
begin
  Result := ParseExpr6;
  while FToken in [etADD, etSUB] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr6;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;

function TAssignExprParser.ParseExpr6: PExprNode;
const
  Operators: array[etADD..etDIV] of CanOp = (
    canADD, canSUB, canMUL, canDIV);
var
  Operator: CanOp;
  Left, Right: PExprNode;
begin
  Result := ParseExpr7;
  while FToken in [etMUL, etDIV] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr7;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;


function TAssignExprParser.ParseExpr7: PExprNode;
var
  FuncName: string;
begin
  case FToken of
    etSymbol:
      if (poExtSyntax in FParserOptions)
         and  NextTokenIsLParen and TokenSymbolIsFunc(FTokenString) then
        begin
          Funcname := FTokenString;
          NextToken;
          if FToken <> etLParen then 
            DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
          NextToken;
          if (CompareText(FuncName,'count') = 0) and (FToken = etMUL) then 
          begin
            FuncName := 'COUNT(*)';
            NextToken;
          end;
          Result := FFilter.NewNode(enFunc, canNOTDEFINED, FuncName,
                    nil, nil);
          if FToken <> etRParen then
          begin
            Result.FArgs := TList.Create;
            repeat
              Result.FArgs.Add(ParseExpr);
              if (FToken <> etComma) and (FToken <> etRParen) then
                DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]); 
              if FToken = etComma then NextToken;
            until (FToken = etRParen) or (FToken = etEnd);
          end else 
            Result.FArgs := nil;

          GetFuncResultInfo(Result);
        end
      else if TokenSymbolIs('NULL') then
        begin
          Result := FFilter.NewNode(enConst, canNOTDEFINED, System.Null, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrTrue) then
        begin
          Result := FFilter.NewNode(enConst, canNOTDEFINED, 1, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrFalse) then
        begin
          Result := FFilter.NewNode(enConst, canNOTDEFINED, 0, nil, nil);
          Result.FScopeKind := skConst;
        end
      else
        begin
          Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
          Result.FScopeKind := skField;
        end;
    etName:
      begin
        Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
        Result.FScopeKind := skField;
      end;
    etLiteral:
      begin
        Result := FFilter.NewNode(enConst, canNOTDEFINED, FTokenString, nil, nil);
        if FNumericLit then Result^.FDataType := fldFLOAT else 
           Result^.FDataType := fldZSTRING;
        Result.FScopeKind := skConst;
      end;
    etLParen:
      begin
        NextToken;
        Result := ParseExpr;
        if FToken <> etRParen then DatabaseErrorFmt(SExprNoRParen, [TokenName]);
      end;
  else
    DatabaseErrorFmt(SExprExpected, [TokenName]);
    Result := nil;
  end;
  NextToken;
end;

procedure  TAssignExprParser.GetScopeKind(Root, Left, Right : PExprNode);
begin
  if (Left = nil) and (Right = nil) then Exit;
  if Right = nil then
  begin
    Root.FScopeKind := Left.FScopeKind;
    Exit;
  end;
//  if ((Left^.FScopeKind = skField) and (Right^.FScopeKind = skAgg))
//     or ((Left^.FScopeKind = skAgg) and (Right^.FScopeKind = skField)) then
//    DatabaseError(SExprBadScope);
  if (Left^.FScopeKind = skConst) and (Right^.FScopeKind = skConst) then
    Root^.FScopeKind := skConst
  else if (Left^.FScopeKind = skAgg) or (Right^.FScopeKind = skAgg) then
    Root^.FScopeKind := skAgg
  else if (Left^.FScopeKind = skField) or (Right^.FScopeKind = skField) then
    Root^.FScopeKind := skField;
end;

procedure TAssignExprParser.GetFuncResultInfo(Node : PExprNode);
begin
  Node^.FDataType := fldZSTRING;
  if (CompareText(Node^.FData, 'COUNT(*)') <> 0 )
     and (CompareText(Node^.FData,'GETDATE') <> 0 )
     and ( (Node^.FArgs = nil ) or ( Node^.FArgs.Count = 0) ) then
      DatabaseError(SExprTypeMis);

  if (Node^.FArgs <> nil) and (Node^.FArgs.Count > 0) then
     Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  if (CompareText(Node^.FData , 'SUM') = 0) or
     (CompareText(Node^.FData , 'AVG') = 0) then
  begin
    Node^.FDataType := fldFLOAT;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'MIN') = 0) or
          (CompareText(Node^.FData , 'MAX') = 0) then
  begin
    Node^.FDataType := PExprNode(Node^.FArgs.Items[0])^.FDataType;
    Node^.FScopeKind := skAgg;
  end
  else if  (CompareText(Node^.FData , 'COUNT') = 0) or
           (CompareText(Node^.FData , 'COUNT(*)') = 0) then
  begin
    Node^.FDataType := fldINT32;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'YEAR') = 0) or
          (CompareText(Node^.FData , 'MONTH') = 0) or
          (CompareText(Node^.FData , 'DAY') = 0) or
          (CompareText(Node^.FData , 'HOUR') = 0) or
          (CompareText(Node^.FData , 'MINUTE') = 0) or
          (CompareText(Node^.FData , 'SECOND') = 0 ) then
  begin
    Node^.FDataType := fldINT32;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'GETDATE') = 0  then
  begin
    Node^.FDataType := fldTIMESTAMP;
    Node^.FScopeKind := skConst;
  end
  else if CompareText(Node^.FData , 'DATE') = 0  then
  begin
    Node^.FDataType := fldDATE;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'TIME') = 0  then
  begin
    Node^.FDataType := fldTIME;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end;
end;

function TAssignExprParser.TokenName: string;
begin
  if FSourcePtr = FTokenPtr then Result := SExprNothing else
  begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TAssignExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (CompareText(FTokenString, S) = 0);
end;


function TAssignExprParser.TokenSymbolIsFunc(const S: string) : Boolean;
begin
  Result := (CompareText(S, 'UPPER') = 0) or
            (CompareText(S, 'LOWER') = 0) or
            (CompareText(S, 'SUBSTRING') = 0) or
            (CompareText(S, 'TRIM') = 0) or
            (CompareText(S, 'TRIMLEFT') = 0) or
            (CompareText(S, 'TRIMRIGHT') = 0) or
            (CompareText(S, 'YEAR') = 0) or
            (CompareText(S, 'MONTH') = 0) or
            (CompareText(S, 'DAY') = 0) or
            (CompareText(S, 'HOUR') = 0) or
            (CompareText(S, 'MINUTE') = 0) or
            (CompareText(S, 'SECOND') = 0) or
            (CompareText(S, 'GETDATE') = 0) or
            (CompareText(S, 'DATE') = 0) or
            (CompareText(S, 'TIME') = 0) or
            (CompareText(S, 'SUM') = 0) or
            (CompareText(S, 'MIN') = 0) or
            (CompareText(S, 'MAX') = 0) or
            (CompareText(S, 'AVG') = 0) or
            (CompareText(S, 'COUNT') = 0);

end;

procedure TAssignExprParser.TypeCheckArithOp(Node: PExprNode);
begin
  with Node^ do
  begin
    if IsNumeric(FLeft.FDataType) and IsNumeric(FRight.FDataType)  then
      FDataType := fldFLOAT
    else if (FLeft.FDataType = fldZSTRING) and
       (FRight.FDataType = fldZSTRING) and (FOperator = canADD) then
      FDataType := fldZSTRING
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = canADD) then
      FDataType := fldTIMESTAMP
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = canSUB) then
      FDataType := FLeft.FDataType
    else if IsTemporal(FLeft.FDataType) and IsTemporal(FRight.FDataType) and
       (FOperator = canSUB) then
      FDataType := fldFLOAT
    else if (FLeft.FDataType = fldZSTRING) and IsTemporal(FRight.FDataType) and
       (FOperator = canSUB) then
    begin
      FLeft.FDataType := FRight.FDataType;
      FDataType := fldFLOAT;
    end
    else if ( FLeft.FDataType = fldZSTRING) and  IsNumeric(FRight.FDataType )and
         (FLeft.FKind = enConst)  then
      FLeft.FDataType := fldTIMESTAMP
    else
      DatabaseError(SExprTypeMis);
  end;
end;
{$ENDIF}

{ TDBExprEvaluator }
procedure TDBExprEvaluator.DoFirst;
begin
  FDataSet.First;
end;

function TDBExprEvaluator.DoGetEOF: Boolean;
begin
  Result := FDataSet.EOF;
end;

procedure TDBExprEvaluator.DoNext;
begin
  FDataSet.Next;
end;

procedure TDBExprEvaluator.DoBeginEvalute;
begin
  FDataSet.DisableControls;
  FBmk := FDataSet.GetBookmark;
end;

procedure TDBExprEvaluator.DoEndEvalute;
begin
  FDataSet.FreeBookmark(FBmk);
  FDataSet.EnableControls;
end;

procedure TDBExprEvaluator.DoBeginAgg;
begin
end;

type
  TDataSetHack = class(TDataSet);

procedure TDBExprEvaluator.DoEndAgg;
begin
  try
    TDataSetHack(FDataSet).InternalGotoBookmark(FBmk);
    FDataSet.Resync([rmExact]);
  except
    on EDatabaseError do ;
  end;
end;

function TDBExprEvaluator.DoGetFieldValue(FieldNo: Word;
  const FieldName: string; var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

var
  Field: TField;
begin
  Field := FieldByNumber(FDataSet, FieldNo);
  if Assigned(Field) then
  begin
    case Field.DataType of
      ftString, ftBytes, ftVarBytes:
        SetResult(fldZSTRING, Field.Size);
      ftSmallint:
        SetResult(fldINT16, SizeOf(SmallInt));
      ftWord:
        SetResult(fldUINT16, SizeOf(Word));
      ftInteger, ftAutoInc:
        SetResult(fldINT32, SizeOf(Integer));
      ftBoolean:
        SetResult(fldBOOL, SizeOf(WordBool));
      ftFloat, ftCurrency, ftBCD:
        SetResult(fldFLOAT, SizeOf(Double));
      ftDate, ftTime, ftDateTime:
        SetResult(fldDATE, SizeOf(TDateTime));
      ftLargeint:
        SetResult(fldINT64, SizeOf(Int64));
    else
      SetResult(fldUNKNOWN, 0);
    end;
    Result := Field.Value;
  end else begin
    SetResult(fldUNKNOWN, 0);
    Result := Null;
  end;
end;

procedure TDBExprEvaluator.SetData(Value: PCanExpr);
begin
  FExpression := '';
  inherited SetData(Value);
end;

procedure TDBExprEvaluator.SetDataSet(Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    if FExpression <> '' then
      inherited SetData(nil);
    FDataSet := Value;
    if FExpression <> '' then
      UpdateData;
  end;
end;

procedure TDBExprEvaluator.SetExpression(Value: string);
begin
  if FExpression <> Value then
  begin
    inherited SetData(nil);
    FExpression := Value;
    if FExpression <> '' then
      UpdateData;
  end;
end;

procedure TDBExprEvaluator.UpdateData;
begin
  if Assigned(FDataSet) then
  begin
    with TAssignExprParser.Create(FDataSet, FExpression, [],
      [poDefaultExpr, poAggregate, poExtSyntax], '', nil {$IFDEF _D5_}, vgDB.FieldTypeMap{$ENDIF}) do
    try
      inherited SetData(Pointer(FilterData));
    finally
      Free;
    end;
  end;
end;

type
  TExpressionDataLink = class(TDataLink)
  private
    FExpression: TDBExpression;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  end;

procedure TExpressionDataLink.ActiveChanged;
begin
  RecordChanged(nil);
end;

procedure TExpressionDataLink.RecordChanged(Field: TField);
begin
  if Field = nil then
    FExpression.RecordChanged;
end;

{ TDBExpression }
constructor TDBExpression.Create(AOwner: TComponent);
begin
  inherited;
  FActive := True;
  FDataLink := TExpressionDataLink.Create;
  TExpressionDataLink(FDataLink).FExpression := Self;
  FEvaluator := TDBExprEvaluator.Create;
end;

destructor TDBExpression.Destroy;
begin
  FDataLink.Free;
  FEvaluator.Free;
  inherited;
end;

procedure TDBExpression.RecordChanged;
begin
  if not FInEvaluate then
  begin
    FInEvaluate := True;
    try
      if Active then
      try
        FText := VarToStr(GetValue);
      except
        FText := Exception(ExceptObject).Message;
      end else
        FText := '';
      Change;
    finally
      FInEvaluate := False;
    end;
  end;
end;

procedure TDBExpression.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDBExpression.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBExpression.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBExpression.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    RecordChanged;
  end;
end;

function TDBExpression.GetExpression: string;
begin
  Result := FEvaluator.Expression;
end;

procedure TDBExpression.SetExpression(Value: string);
begin
  if GetExpression <> Value then
  begin
    FEvaluator.Expression := Value;
    RecordChanged;
  end;
end;

procedure TDBExpression.SetText(Value: string);
begin
end;

function TDBExpression.GetValue: Variant;
var
  iFldType, iFldLen: Integer;
begin
  if Assigned(DataSource) and (DataSource.State = dsBrowse) and
    (Expression <> '') then
  begin
    FEvaluator.DataSet := DataSource.DataSet;
    Result := FEvaluator.Evalute(iFldType, iFldLen);
  end else
    Result := Null;
end;
{$ENDIF}

end.
