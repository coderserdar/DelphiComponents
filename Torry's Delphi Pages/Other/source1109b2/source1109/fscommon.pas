Unit fscommon;

{$T-,H+,X+,R-}
{$I fsdefine.inc}

Interface

Uses Classes,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  Db,
  windows;

Type
  WCHAR = WideChar;
  {$EXTERNALSYM WCHAR}
  PWChar = PWideChar;

  LPSTR = PAnsiChar;
  {$EXTERNALSYM LPSTR}
  PLPSTR = ^LPSTR;
  {$EXTERNALSYM PLPSTR}
  LPCSTR = PAnsiChar;
  {$EXTERNALSYM LPCSTR}
  LPCTSTR = PAnsiChar; { should be PWideChar if UNICODE }
  {$EXTERNALSYM LPCTSTR}
  LPTSTR = PAnsiChar; { should be PWideChar if UNICODE }
  {$EXTERNALSYM LPTSTR}
  LPWSTR = PWideChar;
  {$EXTERNALSYM LPWSTR}
  PLPWSTR = ^LPWSTR;
  {$EXTERNALSYM PLPWSTR}
  LPCWSTR = PWideChar;
  {$EXTERNALSYM LPCWSTR}

  DWORD = Longword;
  {$EXTERNALSYM DWORD}
  BOOL = LongBool;
  {$EXTERNALSYM BOOL}
  PBOOL = ^BOOL;
  {$EXTERNALSYM PBOOL}
  PByte = ^Byte;
  PINT = ^Integer;
  {$EXTERNALSYM PINT}
  PSingle = ^Single;
  PWORD = ^Word;
  {$EXTERNALSYM PWORD}
  PDWORD = ^DWORD;
  {$EXTERNALSYM PDWORD}
  LPDWORD = PDWORD;
  {$EXTERNALSYM LPDWORD}

  UCHAR = Byte;
  {$EXTERNALSYM UCHAR}
  PUCHAR = ^Byte;
  {$EXTERNALSYM PUCHAR}
  SHORT = Smallint;
  {$EXTERNALSYM SHORT}
  UINT = Longword;
  {$EXTERNALSYM UINT}
  PUINT = ^UINT;
  {$EXTERNALSYM PUINT}
  ULONG = Cardinal;
  {$EXTERNALSYM ULONG}
  PULONG = ^ULONG;
  {$EXTERNALSYM PULONG}
  PLongint = ^Longint;
  PInteger = ^Integer;
  PLongWord = ^Longword;
  PSmallInt = ^Smallint;
  PDouble = ^Double;
  PShortInt = ^Shortint;

  LCID = DWORD;
  {$EXTERNALSYM LCID}
  LANGID = Word;
  {$EXTERNALSYM LANGID}

  THandle = Longword;
  PHandle = ^THandle;

  TfsCANOperator = (
    coNOTDEFINED, {                                   }
    coISBLANK, { coUnary;  is operand blank.      }
    coNOTBLANK, { coUnary;  is operand not blank.  }
    coEQ, { coBinary, coCompare; equal.     }
    coNE, { coBinary; NOT equal.             }
    coGT, { coBinary; greater than.          }
    coLT, { coBinary; less than.             }
    coGE, { coBinary; greater or equal.      }
    coLE, { coBinary; less or equal.         }
    coNOT, { coUnary; NOT                     }
    coAND, { coBinary; AND                    }
    coOR, { coBinary; OR                     }
    coTUPLE2, { coUnary; Entire record is operand. }
    coFIELD2, { coUnary; operand is field        }
    coCONST2, { coUnary; operand is constant     }
    coMINUS, { coUnary;  minus. }
    coADD, { coBinary; addition. }
    coSUB, { coBinary; subtraction. }
    coMUL, { coBinary; multiplication. }
    coDIV, { coBinary; division. }
    coMOD, { coBinary; modulo division. }
    coREM, { coBinary; remainder of division. }
    coSUM, { coBinary, accumulate sum of. }
    coCOUNT, { coBinary, accumulate count of. }
    coMIN, { coBinary, find minimum of. }
    coMAX, { coBinary, find maximum of. }
    coAVG, { coBinary, find average of. }
    coCONT, { coBinary; provides a link between two }
    coUDF2, { coBinary; invokes a User defined fn }
    coCONTINUE2, { coUnary; Stops evaluating records }
    coLIKE, { coCompare, extended binary compare        }
    coIN, { coBinary field in list of values }
    coLIST2, { List of constant values of same type }
    coUPPER, { coUnary: upper case }
    coLOWER, { coUnary: lower case }
    coFUNC2, { coFunc: Function }
    coLISTELEM2, { coListElem: List Element }
    coASSIGN { coBinary: Field assignment }
    );

  fsNODEClass = ({ Node Class }
    nodeNULL, { Null node                   }
    nodeUNARY, { Node is a unary             }
    nodeBINARY, { Node is a binary            }
    nodeCOMPARE, { Node is a compare           }
    nodeFIELD, { Node is a field             }
    nodeCONST, { Node is a constant          }
    nodeTUPLE, { Node is a record }
    nodeCONTINUE, { Node is a continue node     }
    nodeUDF, { Node is a UDF node }
    nodeLIST, { Node is a LIST node }
    nodeFUNC, { Node is a Function node }
    nodeLISTELEM { Node is a List Element node }
    );

Const
  CANEXPRSIZE = 10; { SizeOf(CANExpr) }
  CANHDRSIZE = 8; { SizeOf(CANHdr) }
  CANEXPRVERSION = 2;

Type
  TfsExprData = Array Of Byte;
  TfsFieldMap = Array[TFieldType] Of Byte;

  { TfsFilterExpr }

Type

  TfsParserOption = (poExtSyntax, poAggregate, poDefaultExpr, poUseOrigNames,
    poFieldNameGiven, poFieldDepend);
  TfsParserOptions = Set Of TfsParserOption;

  TfsExprNodeKind = (enField, enConst, enOperator, enFunc);
  TfsExprScopeKind = (skField, skAgg, skConst);

  PExprNode = ^TfsExprNode;
  TfsExprNode = Record
    FNext: PExprNode;
    FKind: TfsExprNodeKind;
    FPartial: Boolean;
    FOperator: TfsCANOperator;
    FData: Variant;
    FLeft: PExprNode;
    FRight: PExprNode;
    FDataType: TFieldType;
    FDataSize: Integer;
    FArgs: TList;
    FScopeKind: TfsExprScopeKind;
  End ;

  TfsFilterExpr = Class
  Private
    FDataSet: TDataSet;
    FFieldMap: TfsFieldMap;
    FOptions: TFilterOptions;
    FParserOptions: TfsParserOptions;
    FNodes: PExprNode;
    FExprBuffer: TfsExprData;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    FFieldName: String;
    FDependentFields: TBits;
    Function FieldFromNode(Node: PExprNode): TField;
    Function GetExprData(Pos, Size: Integer): PChar;
    Function PutConstBCD(Const Value: Variant; Decimals: Integer): Integer;
    Function PutConstCurrency(Const Value: Variant): Integer;
    Function PutConstBool(Const Value: Variant): Integer;
    Function PutConstDate(Const Value: Variant): Integer;
    Function PutConstDateTime(Const Value: Variant): Integer;
    Function PutConstFloat(Const Value: Variant): Integer;
    Function PutConstInt(DataType: TFieldType; Const Value: Variant): Integer;
    Function PutConstNode(DataType: TFieldType; Data: PChar;
      Size: Integer): Integer;

    {$IFDEF DCC6OrLater}
      {$HINTS OFF}
    {$ENDIF}
    Function PutConstNode64(DataType : TFieldType ;
                            Data     : PChar      ;
                            Size     : Integer     ) : Int64 ;
    {$IFDEF DCC6OrLater}
      {$HINTS OFF}
    {$ENDIF}

    Function PutConstStr(Const Value: String): Integer;
    Function PutConstFsArrayStr(Const Value: String): Integer;
    Function PutConstTime(Const Value: Variant): Integer;
    Function PutData(Data: PChar; Size: Integer): Integer;
    Function PutExprNode(Node: PExprNode; ParentOp: TfsCANOperator): Integer;
    Function PutFieldNode(Field: TField; Node: PExprNode): Integer;
    Function PutNode(NodeType: fsNODEClass; OpType: TfsCANOperator;
      OpCount: Integer): Integer;
    Procedure SetNodeOp(Node, Index, Data: Integer);
    Function PutConstant(Node: PExprNode): Integer;
    Function GetFieldByName(Name: String): TField;
  Public
    Constructor Create(DataSet: TDataSet; Options: TFilterOptions;
      ParseOptions: TfsParserOptions; Const FieldName: String; DepFields: TBits;
      FieldMap: TfsFieldMap);
    Destructor Destroy; Override;
    Function NewCompareNode(Field: TField; Operator: TfsCANOperator;
      Const Value: Variant): PExprNode;
    Function NewNode(Kind: TfsExprNodeKind; Operator: TfsCANOperator;
      Const Data: Variant; Left, Right: PExprNode): PExprNode;
    Function GetFilterData(Root: PExprNode): TfsExprData;
    Property DataSet: TDataSet Write FDataSet;
  End ;

  { TfsExprParser }

  TfsExprToken = (etEnd, etSymbol, etName, etLiteral, etLParen, etRParen,
    etEQ, etNE, etGE, etLE, etGT, etLT, etADD, etSUB, etMUL, etDIV,
    etComma, etLIKE, etISNULL, etISNOTNULL, etIN);

  TfsExprParser = Class
  Private
    FFilter: TfsFilterExpr;
    FFieldMap: TfsFieldMap;
    FText: String;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: String;
    FStrTrue: String;
    FStrFalse: String;
    FToken: TfsExprToken;
    FPrevToken: TfsExprToken;
    FFilterData: TfsExprData;
    FNumericLit: Boolean;
    FDataSize: Integer;
    FParserOptions: TfsParserOptions;
    FFieldName: String;
    FDataSet: TDataSet;
    FDependentFields: TBits;
    Procedure NextToken;
    Function NextTokenIsLParen: Boolean;
    Function ParseExpr: PExprNode;
    Function ParseExpr2: PExprNode;
    Function ParseExpr3: PExprNode;
    Function ParseExpr4: PExprNode;
    Function ParseExpr5: PExprNode;
    Function ParseExpr6: PExprNode;
    Function ParseExpr7: PExprNode;
    Function TokenName: String;
    Function TokenSymbolIs(Const S: String): Boolean;
    Function TokenSymbolIsFunc(Const S: String): Boolean;
    Procedure GetFuncResultInfo(Node: PExprNode);
    Procedure TypeCheckArithOp(Node: PExprNode);
    Procedure GetScopeKind(Root, Left, Right: PExprNode);
  Public
    Constructor Create(DataSet: TDataSet; Const Text: String;
      Options: TFilterOptions; ParserOptions: TfsParserOptions;
      Const FieldName: String; DepFields: TBits; FieldMap: TfsFieldMap);
    Destructor Destroy; Override;
    Procedure SetExprParams(Const Text: String; Options: TFilterOptions;
      ParserOptions: TfsParserOptions; Const FieldName: String);
    Property FilterData: TfsExprData Read FFilterData;
    Property DataSize: Integer Read FDataSize;
  End ;

  { Field Origin parser }

Type
  TfsFieldInfo = Record
    DataBaseName: String;
    TableName: String;
    OriginalFieldName: String;
  End ;

Function fsGetFieldInfo(Const Origin: String; Var FieldInfo: TfsFieldInfo): Boolean;

Implementation

Uses fsllbase,
  SysUtils,
  dbconsts,
  Consts;

Function fsGetFieldInfo(Const Origin: String; Var FieldInfo: TfsFieldInfo): Boolean;
Var
  Current: PChar;
  Values: Array[0..4] Of String;
  I: Integer;

  Function GetPChar(Const S: String): PChar;
  Begin
    If S <> '' Then
      Result := PChar(Pointer(S))
    Else
      Result := '';
  End ;

  Procedure Split(Const S: String);
  Begin
    Current := PChar(Pointer(S));
  End ;

  Function NextItem: String;
  Var
    C: PChar;
    I: PChar;
    Terminator: Char;
    Ident: Array[0..1023] Of Char;
  Begin
    Result := '';
    C := Current;
    I := Ident;
    While C^ In ['.', ' ', #0] Do
      If C^ = #0 Then
        Exit
      Else
        Inc(C);
    Terminator := '.';
    If C^ = '"' Then
      Begin
        Terminator := '"';
        Inc(C);
      End ;
    While Not (C^ In [Terminator, #0]) Do
      Begin
        If C^ In LeadBytes Then
          Begin
            I^ := C^;
            Inc(C);
            Inc(I);
          End
        Else If C^ = '\' Then
          Begin
            Inc(C);
            If C^ In LeadBytes Then
              Begin
                I^ := C^;
                Inc(C);
                Inc(I);
              End ;
            If C^ = #0 Then
              Dec(C);
          End ;
        I^ := C^;
        Inc(C);
        Inc(I);
      End ;
    SetString(Result, Ident, I - Ident);
    If (Terminator = '"') And (C^ <> #0) Then
      Inc(C);
    Current := C;
  End ;

  Function PopValue: PChar;
  Begin
    If I >= 0 Then
      Begin
        Result := GetPChar(Values[I]);
        Dec(I);
      End
    Else
      Result := '';
  End ;

Begin
  Result := False;
  If (Origin = '') Then
    Exit;
  Split(Origin);
  I := -1;
  Repeat
    Inc(I);
    Values[I] := NextItem;
  Until (Values[I] = '') Or (I = High(Values));
  If I = High(Values) Then
    Exit;
  Dec(I);
  FieldInfo.OriginalFieldName := StrPas(PopValue);
  FieldInfo.TableName := StrPas(PopValue);
  FieldInfo.DataBaseName := StrPas(PopValue);
  Result := (FieldInfo.OriginalFieldName <> '') And (FieldInfo.TableName <> '');
End ;

Const
  StringFieldTypes = [ftString, ftFixedChar, ftWideString, ftBytes];
  BlobFieldTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle,
    ftTypedBinary];

Function IsNumeric(DataType: TFieldType): Boolean;
Begin
  Result := DataType In [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
    ftBCD, ftAutoInc, ftLargeInt];
End ;

Function IsTemporal(DataType: TFieldType): Boolean;
Begin
  Result := DataType In [ftDate, ftTime, ftDateTime];
End ;

{ TfsFilterExpr }

Constructor TfsFilterExpr.Create(DataSet: TDataSet; Options: TFilterOptions;
  ParseOptions: TfsParserOptions; Const FieldName: String; DepFields: TBits;
  FieldMap: TfsFieldMap);
Begin
  FFieldMap := FieldMap;
  FDataSet := DataSet;
  FOptions := Options;
  FFieldName := FieldName;
  FParserOptions := ParseOptions;
  FDependentFields := DepFields;
End ;

Destructor TfsFilterExpr.Destroy;
Var
  Node: PExprNode;
Begin
  SetLength(FExprBuffer, 0);
  While FNodes <> Nil Do
    Begin
      Node := FNodes;
      FNodes := Node^.FNext;
      If (Node^.FKind = enFunc) And (Node^.FArgs <> Nil) Then
        Node^.FArgs.Free;
      Dispose(Node);
    End ;
End ;

Function TfsFilterExpr.FieldFromNode(Node: PExprNode): TField;
Begin
  Result := GetFieldByName(Node^.FData);
  If Not (Result.FieldKind In [fkData, fkInternalCalc]) Then
    DatabaseErrorFmt(SExprBadField, [Result.FieldName]);
End ;

Function TfsFilterExpr.GetExprData(Pos, Size: Integer): PChar;
Begin
  SetLength(FExprBuffer, FExprBufSize + Size);
  Move(FExprBuffer[Pos], FExprBuffer[Pos + Size], FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
End ;

Function TfsFilterExpr.GetFilterData(Root: PExprNode): TfsExprData;
Begin
  FExprBufSize := CANExprSize;
  SetLength(FExprBuffer, FExprBufSize);
  PutExprNode(Root, coNOTDEFINED);
  PWord(@FExprBuffer[0])^ := CANEXPRVERSION; { iVer }
  PWord(@FExprBuffer[2])^ := FExprBufSize; { iTotalSize }
  PWord(@FExprBuffer[4])^ := $FFFF; { iNodes }
  PWord(@FExprBuffer[6])^ := CANEXPRSIZE; { iNodeStart }
  PWord(@FExprBuffer[8])^ := FExprNodeSize + CANEXPRSIZE; { iLiteralStart }
  Result := FExprBuffer;
End ;

Function TfsFilterExpr.NewCompareNode(Field: TField; Operator: TfsCANOperator;
  Const Value: Variant): PExprNode;
Var
  ConstExpr: PExprNode;
Begin
  ConstExpr := NewNode(enConst, coNOTDEFINED, Value, Nil, Nil);
  ConstExpr^.FDataType := Field.DataType;
  ConstExpr^.FDataSize := Field.Size;
  Result := NewNode(enOperator, Operator, Unassigned,
    NewNode(enField, coNOTDEFINED, Field.FieldName, Nil, Nil), ConstExpr);
End ;

Function TfsFilterExpr.NewNode(Kind: TfsExprNodeKind; Operator: TfsCANOperator;
  Const Data: Variant; Left, Right: PExprNode): PExprNode;
Var
  Field: TField;
Begin
  New(Result);
  With Result^ Do
    Begin
      FNext := FNodes;
      FKind := Kind;
      FPartial := False;
      FOperator := Operator;
      FData := Data;
      FLeft := Left;
      FRight := Right;
    End ;
  FNodes := Result;
  If Kind = enField Then
    Begin
      Field := GetFieldByName(Data);
      If Field = Nil Then
        DatabaseErrorFmt(SFieldNotFound, [Data]);
      Result^.FDataType := Field.DataType;
      Result^.FDataSize := Field.Size;
    End ;
End ;

Function TfsFilterExpr.PutConstBCD(Const Value    : Variant ;
                                         Decimals : Integer  ) : Integer ;
//  Var
//    C: Currency;
    //BCD: TBcd;
  Begin
    // note
    // bcd not implemented
    {If VarType(Value) = varString Then
      C := StrToCurr(String(TVarData(Value).VString))
    Else
      C := Value;
    CurrToBCD(C, BCD, 32, Decimals);
    Result := PutConstNode(ftBCD, @BCD, 18); }
    Result := 0 ;
  End ;

Function TfsFilterExpr.PutConstCurrency(Const Value: Variant): Integer;
Var
  C: Currency;
Begin
  If VarType(Value) = varString Then
    C := StrToCurr(String(TVarData(Value).VString))
  Else
    C := Value;
  Result := PutConstNode(ftCurrency, @C, 8);
End ;

Function TfsFilterExpr.PutConstBool(Const Value: Variant): Integer;
Var
  B: WordBool;
Begin
  B := Value;
  Result := PutConstNode(ftBoolean, @B, SizeOf(WordBool));
End ;

Function TfsFilterExpr.PutConstDate(Const Value: Variant): Integer;
Var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
Begin
  If VarType(Value) = varString Then
    DateTime := StrToDate(String(TVarData(Value).VString))
  Else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftDate, @TimeStamp.Date, 4);
End ;

Function TfsFilterExpr.PutConstDateTime(Const Value: Variant): Integer;
Var
  DateTime: TDateTime;
  DateData: Double;
Begin
  If VarType(Value) = varString Then
    DateTime := StrToDateTime(String(TVarData(Value).VString))
  Else
    DateTime := VarToDateTime(Value);
  DateData := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
  Result := PutConstNode(ftDateTime, @DateData, 8);
End ;

Function TfsFilterExpr.PutConstFloat(Const Value: Variant): Integer;
Var
  F: Extended;
Begin
  If VarType(Value) = varString Then
    F := StrToFloat(String(TVarData(Value).VString))
  Else
    F := Value;
  Result := PutConstNode(ftFloat, @F, 10);
End ;

Function TfsFilterExpr.PutConstInt(DataType: TFieldType;
  Const Value: Variant): Integer;
Var
  Size: Integer;
  I: Int64;
Begin
  If VarType(Value) = varString Then
    I := StrToInt64(String(TVarData(Value).VString))
  Else
    Begin
      {$IFDEF IsNoVariantInt64}
      I := Decimal(Value).lo64;
      {$ELSE}
      I := Value;
      {$ENDIF}
    End ;
  Size := 8;
  Case DataType Of
    ftSmallint:
      If (I < -32768) Or (I > 32767) Then
        DatabaseError(SExprRangeError);
    ftWord:
      If (I < 0) Or (I > 65535) Then
        DatabaseError(SExprRangeError);
  End ;
  Result := PutConstNode(DataType, @I, Size);
End ;

Function TfsFilterExpr.PutConstNode(DataType: TFieldType; Data: PChar;
  Size: Integer): Integer;
Begin
  Result := PutNode(nodeCONST, coCONST2, 3);
  SetNodeOp(Result, 0, FFieldMap[DataType]);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
End ;

Function TfsFilterExpr.PutConstNode64(DataType: TFieldType; Data: PChar;
  Size: Integer): Int64;
Begin
  Result := PutNode(nodeCONST, coCONST2, 3);
  SetNodeOp(Result, 0, FFieldMap[DataType]);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
End ;

Function TfsFilterExpr.PutConstStr(Const Value: String): Integer;
Var
  Str: String;
  Buffer: Array[0..255] Of Char;
Begin
  If Length(Value) >= SizeOf(Buffer) Then
    Str := Copy(Value, 1, SizeOf(Buffer) - 1)
  Else
    Str := Value;
  FDataSet.Translate(PChar(Str), Buffer, True);
  Result := PutConstNode(ftString, Buffer, Length(Str) + 1);
End ;

Function TfsFilterExpr.PutConstFsArrayStr(Const Value: String): Integer;
Var
  Str: String;
  Buffer: Array[0..255] Of Char;
Begin
  If Length(Value) >= SizeOf(Buffer) Then
    Str := Copy(Value, 1, SizeOf(Buffer) - 1)
  Else
    Str := Value;
  FDataSet.Translate(PChar(Str), Buffer, True);
  Result := PutConstNode(ftBytes, Buffer, Length(Str) + 1);
End ;

Function TfsFilterExpr.PutConstTime(Const Value: Variant): Integer;
Var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
Begin
  If VarType(Value) = varString Then
    DateTime := StrToTime(String(TVarData(Value).VString))
  Else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftTime, @TimeStamp.Time, 4);
End ;

Function TfsFilterExpr.PutData(Data: PChar; Size: Integer): Integer;
Begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
End ;

Function TfsFilterExpr.PutConstant(Node: PExprNode): Integer;
Begin
  Result := 0;
  Case Node^.FDataType Of
    ftSmallint, ftWord, ftAutoInc, ftLargeInt, ftInteger:
      Result := PutConstInt(Node^.FDataType, Node^.FData);
    ftFloat: Result := PutConstFloat(Node^.FData);
    ftString, ftWideString, ftFixedChar:
      Result := PutConstStr(Node^.FData);
    ftBytes:
      Result := PutConstFsArrayStr(Node^.FData);
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
    ftCurrency:
      Result := PutConstCurrency(Node^.FData);
    Else
      DatabaseErrorFmt(SExprBadConst, [Node^.FData]);
  End ;
End ;

Function TfsFilterExpr.PutExprNode(Node: PExprNode; ParentOp: TfsCANOperator): Integer;
Const
  ReverseOperator: Array[coEQ..coLE] Of TfsCANOperator = (coEQ, coNE, coLT,
    coGT, coLE, coGE);
  BoolFalse: WordBool = False;
Var
  Field: TField;
  Left, Right, Temp: PExprNode;
  LeftPos, RightPos, ListElem, PrevListElem, I: Integer;
  Operator: TfsCANOperator;
  CaseInsensitive, PartialLength, L: Integer;
  S: AnsiString;

Begin
  Result := 0;
  Case Node^.FKind Of
    enField:
      Begin
        Field := FieldFromNode(Node);
        If (ParentOp In [coOR, coNOT, coAND, coNOTDEFINED]) And
          (Field.DataType = ftBoolean) Then
          Begin
            Result := PutNode(nodeBINARY, coNE, 2);
            SetNodeOp(Result, 0, PutFieldNode(Field, Node));
            SetNodeOp(Result, 1, PutConstNode(ftBoolean, @BoolFalse, SizeOf(WordBool)));
          End
        Else
          Result := PutFieldNode(Field, Node);
      End ;
    enConst:
      Result := PutConstant(Node);
    enOperator:
      Case Node^.FOperator Of
        coIN:
          Begin
            Result := PutNode(nodeBINARY, coIN, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft, Node^.FOperator));
            ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
            SetNodeOp(Result, 1, ListElem);
            PrevListElem := ListElem;
            For I := 0 To Node^.FArgs.Count - 1 Do
              Begin
                LeftPos := PutExprNode(Node^.FArgs.Items[I], Node^.FOperator);
                If I = 0 Then
                  Begin
                    SetNodeOp(PrevListElem, 0, LeftPos);
                    SetNodeOp(PrevListElem, 1, 0);
                  End
                Else
                  Begin
                    ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
                    SetNodeOp(ListElem, 0, LeftPos);
                    SetNodeOp(ListElem, 1, 0);
                    SetNodeOp(PrevListElem, 1, ListElem);
                    PrevListElem := ListElem;
                  End ;
              End ;
          End ;
        coNOT,
          coISBLANK,
          coNOTBLANK:
          Begin
            Result := PutNode(nodeUNARY, Node^.FOperator, 1);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft, Node^.FOperator));
          End ;
        coEQ..coLE,
          coAND, coOR,
          coADD..coDIV,
          coLIKE,
          coASSIGN:
          Begin
            Operator := Node^.FOperator;
            Left := Node^.FLeft;
            Right := Node^.FRight;
            If (Operator In [coEQ..coLE]) And (Right^.FKind = enField) And
              (Left^.FKind <> enField) Then
              Begin
                Temp := Left;
                Left := Right;
                Right := Temp;
                Operator := ReverseOperator[Operator];
              End ;

            Result := 0;
            If (Left^.FKind = enField) And (Right^.FKind = enConst)
              And ((Node^.FOperator = coEQ) Or (Node^.FOperator = coNE)
              Or (Node^.FOperator = coLIKE)) Then
              Begin
                If VarIsNull(Right^.FData) Then
                  Begin
                    Case Node^.FOperator Of
                      coEQ: Operator := coISBLANK;
                      coNE: Operator := coNOTBLANK;
                      Else
                        DatabaseError(SExprBadNullTest);
                    End ;
                    Result := PutNode(nodeUNARY, Operator, 1);
                    SetNodeOp(Result, 0, PutExprNode(Left, Node^.FOperator));
                  End
                Else If (Right^.FDataType In StringFieldTypes) Then
                  Begin
                    S := Right^.FData;
                    L := Length(S);
                    If L <> 0 Then
                      Begin
                        CaseInsensitive := 0;
                        PartialLength := 0;
                        If foCaseInsensitive In FOptions Then
                          CaseInsensitive := 1;
                        If Node^.FPartial Then
                          PartialLength := L
                        Else If Not (foNoPartialCompare In FOptions) And (L > 1) And
                          (S[L] = '*') Then
                          Begin
                            Delete(S, L, 1);
                            PartialLength := L - 1;
                          End ;
                        If (CaseInsensitive <> 0) Or (PartialLength <> 0) Then
                          Begin
                            Result := PutNode(nodeCOMPARE, Operator, 4);
                            SetNodeOp(Result, 0, CaseInsensitive);
                            SetNodeOp(Result, 1, PartialLength);
                            SetNodeOp(Result, 2, PutExprNode(Left, Node^.FOperator));
                            SetNodeOp(Result, 3, PutConstStr(S));
                          End ;
                      End ;
                  End ;
              End ;

            If Result = 0 Then
              Begin
                If (Operator = coISBLANK) Or (Operator = coNOTBLANK) Then
                  Begin
                    Result := PutNode(nodeUNARY, Operator, 1);
                    LeftPos := PutExprNode(Left, Node^.FOperator);
                    SetNodeOp(Result, 0, LeftPos);
                  End
                Else
                  Begin
                    Result := PutNode(nodeBINARY, Operator, 2);
                    LeftPos := PutExprNode(Left, Node^.FOperator);
                    RightPos := PutExprNode(Right, Node^.FOperator);
                    SetNodeOp(Result, 0, LeftPos);
                    SetNodeOp(Result, 1, RightPos);
                  End ;
              End ;
          End ;
      End ;
    enFunc:
      Begin
        Result := PutNode(nodeFUNC, coFUNC2, 2);
        SetNodeOp(Result, 0, PutData(PChar(String(Node^.FData)),
          Length(String(Node^.FData)) + 1));
        If Node^.FArgs <> Nil Then
          Begin
            ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
            SetNodeOp(Result, 1, ListElem);
            PrevListElem := ListElem;
            For I := 0 To Node^.FArgs.Count - 1 Do
              Begin
                LeftPos := PutExprNode(Node^.FArgs.Items[I], Node^.FOperator);
                If I = 0 Then
                  Begin
                    SetNodeOp(PrevListElem, 0, LeftPos);
                    SetNodeOp(PrevListElem, 1, 0);
                  End
                Else
                  Begin
                    ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
                    SetNodeOp(ListElem, 0, LeftPos);
                    SetNodeOp(ListElem, 1, 0);
                    SetNodeOp(PrevListElem, 1, ListElem);
                    PrevListElem := ListElem;
                  End ;

              End ;
          End
        Else
          SetNodeOp(Result, 1, 0);
      End ;
  End ;
End ;

Function TfsFilterExpr.PutFieldNode(Field : TField    ;
                                    Node  : PExprNode  ) : Integer ;
  Var
    Buffer: Array[0..255] Of Char ;
    cStr : String ;

  Begin
    cStr := Field.FieldName ;
    If (poFieldNameGiven in FParserOptions) then
      FDataSet.Translate(PChar(cStr) , Buffer , True)
    Else
      FDataSet.Translate(PChar(String(Node^.FData)) , Buffer , True) ;

    Result := PutNode(nodeFIELD , coFIELD2 , 2) ;
    SetNodeOp(Result , 0 , Field.FieldNo) ;
    SetNodeOp(Result , 1 , PutData(Buffer , StrLen(Buffer) + 1)) ;
  End ;  

Function TfsFilterExpr.PutNode(NodeType: fsNODEClass; OpType: TfsCANOperator;
  OpCount: Integer): Integer;
Var
  Size: Integer;
  Data: PChar;
Begin
  Size := CANHDRSIZE + OpCount * SizeOf(Word);
  Data := GetExprData(CANEXPRSIZE + FExprNodeSize, Size);
  PInteger(@Data[0])^ := Integer(NodeType); { CANHdr.fsNODEClass }
  PInteger(@Data[4])^ := Integer(OpType); { CANHdr.coOp }
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
End ;

Procedure TfsFilterExpr.SetNodeOp(Node, Index, Data: Integer);
Begin
  PWordArray(PChar(FExprBuffer) + (CANEXPRSIZE + Node +
    CANHDRSIZE))^[Index] := Data;
End ;

Function TfsFilterExpr.GetFieldByName(Name: String): TField;
Var
  I: Integer;
  F: TField;
  FieldInfo: TfsFieldInfo;
Begin
  Result := Nil;
  If poFieldNameGiven In FParserOptions Then
    Result := FDataSet.FieldByName(FFieldName)
  Else If poUseOrigNames In FParserOptions Then
    Begin
      For I := 0 To FDataset.FieldCount - 1 Do
        Begin
          F := FDataSet.Fields[I];
          If fsGetFieldInfo(F.Origin, FieldInfo) And
            (AnsiCompareStr(Name, FieldInfo.OriginalFieldName) = 0) Then
            Begin
              Result := F;
              Exit;
            End ;
        End ;
    End ;
  If Result = Nil Then
    Result := FDataSet.FieldByName(Name);
  If (Result <> Nil) And (Result.FieldKind = fkCalculated) And (poAggregate In FParserOptions) Then
    DatabaseErrorFmt(SExprNoAggOnCalcs, [Result.FieldName]);
  If (poFieldDepend In FParserOptions) And (Result <> Nil) And
    (FDependentFields <> Nil) Then
    FDependentFields[Result.FieldNo - 1] := True;
End ;

Constructor TfsExprParser.Create(DataSet: TDataSet; Const Text: String;
  Options: TFilterOptions; ParserOptions: TfsParserOptions; Const FieldName: String;
  DepFields: TBits; FieldMap: TfsFieldMap);
Begin
  FFieldMap := FieldMap;
  FStrTrue := STextTrue;
  FStrFalse := STextFalse;
  FDataSet := DataSet;
  FDependentFields := DepFields;
  FFilter := TfsFilterExpr.Create(DataSet, Options, ParserOptions, FieldName,
    DepFields, FieldMap);
  If Text <> '' Then
    SetExprParams(Text, Options, ParserOptions, FieldName);
End ;

Destructor TfsExprParser.Destroy;
Begin
  FFilter.Free;
End ;

Procedure TfsExprParser.SetExprParams(Const Text: String; Options: TFilterOptions;
  ParserOptions: TfsParserOptions; Const FieldName: String);
Var
  Root, DefField: PExprNode;
Begin
  FParserOptions := ParserOptions;
  If FFilter <> Nil Then
    FFilter.Free;
  FFilter := TfsFilterExpr.Create(FDataSet, Options, ParserOptions, FieldName,
    FDependentFields, FFieldMap);
  FText := Text;
  FSourcePtr := PChar(Text);
  FFieldName := FieldName;
  NextToken;
  Root := ParseExpr;
  If FToken <> etEnd Then
    DatabaseError(SExprTermination);
  If (poAggregate In FParserOptions) And (Root^.FScopeKind <> skAgg) Then
    DatabaseError(SExprNotAgg);
  If (Not (poAggregate In FParserOptions)) And (Root^.FScopeKind = skAgg) Then
    DatabaseError(SExprNoAggFilter);
  If poDefaultExpr In ParserOptions Then
    Begin
      DefField := FFilter.NewNode(enField, coNOTDEFINED, FFieldName, Nil, Nil);
      If (IsTemporal(DefField^.FDataType) And (Root^.FDataType In StringFieldTypes)) Or
        ((DefField^.FDataType = ftBoolean) And (Root^.FDataType In StringFieldTypes)) Then
        Root^.FDataType := DefField^.FDataType;

      If Not ((IsTemporal(DefField^.FDataType) And IsTemporal(Root^.FDataType))
        Or (IsNumeric(DefField^.FDataType) And IsNumeric(Root^.FDataType))
        Or ((DefField^.FDataType In StringFieldTypes) And (Root^.FDataType In StringFieldTypes))
        Or ((DefField^.FDataType = ftBoolean) And (Root^.FDataType = ftBoolean))) Then
        DatabaseError(SExprTypeMis);
      Root := FFilter.NewNode(enOperator, coASSIGN, Unassigned, Root, DefField);
    End ;

  If Not (poAggregate In FParserOptions) And Not (poDefaultExpr In ParserOptions)
    And (Root^.FDataType <> ftBoolean) Then
    DatabaseError(SExprIncorrect);

  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
End ;

Function TfsExprParser.NextTokenIsLParen: Boolean;
Var
  P: PChar;
Begin
  P := FSourcePtr;
  While (P^ <> #0) And (P^ <= ' ') Do
    Inc(P);
  Result := P^ = '(';
End ;

Procedure TfsExprParser.NextToken;
Type
  ASet = Set Of Char;
Var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: Array[0..255] Of Char;

  Function IsKatakana(Const Chr: Byte): Boolean;
  Begin
    Result := False; // wik (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  End ;

  Procedure Skip(TheSet: ASet);
  Begin
    While True Do
      Begin
        If P^ In LeadBytes Then
          Inc(P, 2)
        Else If (P^ In TheSet) Or IsKatakana(Byte(P^)) Then
          Inc(P)
        Else
          Exit;
      End ;
  End ;

Begin
  FPrevToken := FToken;
  FTokenString := '';
  P := FSourcePtr;
  While (P^ <> #0) And (P^ <= ' ') Do
    Inc(P);
  If (P^ <> #0) And (P^ = '/') And (P[1] <> #0) And (P[1] = '*') Then
    Begin
      P := P + 2;
      While (P^ <> #0) And (P^ <> '*') Do
        Inc(P);
      If (P^ = '*') And (P[1] <> #0) And (P[1] = '/') Then
        P := P + 2
      Else
        DatabaseErrorFmt(SExprInvalidChar, [P^]);
    End ;
  While (P^ <> #0) And (P^ <= ' ') Do
    Inc(P);
  FTokenPtr := P;
  Case P^ Of
    'A'..'Z', 'a'..'z', '_', #$81..#$FE:
      Begin
        TokenStart := P;
        If Not SysLocale.FarEast Then
          Begin
            Inc(P);
            While P^ In ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']'] Do
              Inc(P);
          End
        Else
          Skip(['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']']);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
        If CompareText(FTokenString, 'LIKE') = 0 Then { do not localize }
          FToken := etLIKE
        Else If CompareText(FTokenString, 'IN') = 0 Then { do not localize }
          FToken := etIN
        Else If CompareText(FTokenString, 'IS') = 0 Then { do not localize }
          Begin
            While (P^ <> #0) And (P^ <= ' ') Do
              Inc(P);
            TokenStart := P;
            Skip(['A'..'Z', 'a'..'z']);
            SetString(FTokenString, TokenStart, P - TokenStart);
            If CompareText(FTokenString, 'NOT') = 0 Then { do not localize }
              Begin
                While (P^ <> #0) And (P^ <= ' ') Do
                  Inc(P);
                TokenStart := P;
                Skip(['A'..'Z', 'a'..'z']);
                SetString(FTokenString, TokenStart, P - TokenStart);
                If CompareText(FTokenString, 'NULL') = 0 Then
                  FToken := etISNOTNULL
                Else
                  DatabaseError(SInvalidKeywordUse);
              End
            Else If CompareText(FTokenString, 'NULL') = 0 Then { do not localize }
              Begin
                FToken := etISNULL;
              End
            Else
              DatabaseError(SInvalidKeywordUse);
          End ;
      End ;
    '[':
      Begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        If P = Nil Then
          DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      End ;
    '''':
      Begin
        Inc(P);
        L := 0;
        While True Do
          Begin
            If P^ = #0 Then
              DatabaseError(SExprStringError);
            If P^ = '''' Then
              Begin
                Inc(P);
                If P^ <> '''' Then
                  Break;
              End ;
            If L < SizeOf(StrBuf) Then
              Begin
                StrBuf[L] := P^;
                Inc(L);
              End ;
            Inc(P);
          End ;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
        FNumericLit := False;
      End ;
    '-', '0'..'9':
      Begin
        If (FPrevToken <> etLiteral) And (FPrevToken <> etName) And
          (FPrevToken <> etSymbol) And (FPrevToken <> etRParen) Then
          Begin
            TokenStart := P;
            Inc(P);
            While (P^ In ['0'..'9', DecimalSeparator, 'e', 'E', '+', '-']) Do
              Inc(P);
            If ((P - 1)^ = ',') And (DecimalSeparator = ',') And (P^ = ' ') Then
              Dec(P);
            SetString(FTokenString, TokenStart, P - TokenStart);
            FToken := etLiteral;
            FNumericLit := True;
          End
        Else
          Begin
            FToken := etSUB;
            Inc(P);
          End ;
      End ;
    '(':
      Begin
        Inc(P);
        FToken := etLParen;
      End ;
    ')':
      Begin
        Inc(P);
        FToken := etRParen;
      End ;
    '<':
      Begin
        Inc(P);
        Case P^ Of
          '=':
            Begin
              Inc(P);
              FToken := etLE;
            End ;
          '>':
            Begin
              Inc(P);
              FToken := etNE;
            End ;
          Else
            FToken := etLT;
        End ;
      End ;
    '=':
      Begin
        Inc(P);
        FToken := etEQ;
      End ;
    '>':
      Begin
        Inc(P);
        If P^ = '=' Then
          Begin
            Inc(P);
            FToken := etGE;
          End
        Else
          FToken := etGT;
      End ;
    '+':
      Begin
        Inc(P);
        FToken := etADD;
      End ;
    '*':
      Begin
        Inc(P);
        FToken := etMUL;
      End ;
    '/':
      Begin
        Inc(P);
        FToken := etDIV;
      End ;
    ',':
      Begin
        Inc(P);
        FToken := etComma;
      End ;
    #0:
      FToken := etEnd ;
    Else
      DatabaseErrorFmt(SExprInvalidChar, [P^]);
  End ;
  FSourcePtr := P;
End ;

Function TfsExprParser.ParseExpr: PExprNode;
Begin
  Result := ParseExpr2;
  While TokenSymbolIs('OR') Do
    Begin
      NextToken;
      Result := FFilter.NewNode(enOperator, coOR, Unassigned,
        Result, ParseExpr2);
      GetScopeKind(Result, Result^.FLeft, Result^.FRight);
      Result^.FDataType := ftBoolean;
    End ;
End ;

Function TfsExprParser.ParseExpr2: PExprNode;
Begin
  Result := ParseExpr3;
  While TokenSymbolIs('AND') Do
    Begin
      NextToken;
      Result := FFilter.NewNode(enOperator, coAND, Unassigned,
        Result, ParseExpr3);
      GetScopeKind(Result, Result^.FLeft, Result^.FRight);
      Result^.FDataType := ftBoolean;
    End ;
End ;

Function TfsExprParser.ParseExpr3: PExprNode;
Begin
  If TokenSymbolIs('NOT') Then
    Begin
      NextToken;
      Result := FFilter.NewNode(enOperator, coNOT, Unassigned,
        ParseExpr4, Nil);
      Result^.FDataType := ftBoolean;
    End
  Else
    Result := ParseExpr4;
  GetScopeKind(Result, Result^.FLeft, Result^.FRight);
End ;

Function TfsExprParser.ParseExpr4: PExprNode;
Const
  Operators: Array[etEQ..etLT] Of TfsCANOperator = (
    coEQ, coNE, coGE, coLE, coGT, coLT);
Var
  Operator: TfsCANOperator;
  Left, Right: PExprNode;
Begin
  Result := ParseExpr5;
  If (FToken In [etEQ..etLT]) Or (FToken = etLIKE)
    Or (FToken = etISNULL) Or (FToken = etISNOTNULL)
    Or (FToken = etIN) Then
    Begin
      Case FToken Of
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
        Else
          Operator := coNOTDEFINED;
      End ;
      NextToken;
      Left := Result;
      If Operator = coIN Then
        Begin
          If FToken <> etLParen Then
            DatabaseErrorFmt(SExprNoLParen, [TokenName]);
          NextToken;
          Result := FFilter.NewNode(enOperator, coIN, Unassigned,
            Left, Nil);
          Result.FDataType := ftBoolean;
          If FToken <> etRParen Then
            Begin
              Result.FArgs := TList.Create;
              Repeat
                Right := ParseExpr;
                If IsTemporal(Left.FDataType) Then
                  Right.FDataType := Left.FDataType;
                Result.FArgs.Add(Right);
                If (FToken <> etComma) And (FToken <> etRParen) Then
                  DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
                If FToken = etComma Then
                  NextToken;
              Until (FToken = etRParen) Or (FToken = etEnd);
              If FToken <> etRParen Then
                DatabaseErrorFmt(SExprNoRParen, [TokenName]);
              NextToken;
            End
          Else
            DatabaseError(SExprEmptyInList);
        End
      Else
        Begin
          If (Operator <> coISBLANK) And (Operator <> coNOTBLANK) Then
            Right := ParseExpr5
          Else
            Right := Nil;
          Result := FFilter.NewNode(enOperator, Operator, Unassigned,
            Left, Right);
          If Right <> Nil Then
            Begin
              If (Left^.FKind = enField) And (Right^.FKind = enConst) Then
                Begin
                  Right^.FDataType := Left^.FDataType;
                  Right^.FDataSize := Left^.FDataSize;
                End
              Else If (Right^.FKind = enField) And (Left^.FKind = enConst) Then
                Begin
                  Left^.FDataType := Right^.FDataType;
                  Left^.FDataSize := Right^.FDataSize;
                End ;
            End ;
          If (Left^.FDataType In BlobFieldTypes) And (Operator = coLIKE) Then
            Begin
              If Right^.FKind = enConst Then
                Right^.FDataType := ftString;
            End
          Else If (Operator <> coISBLANK) And (Operator <> coNOTBLANK)
            And ((Left^.FDataType In (BlobFieldTypes {+ [ftBytes]})) Or
            ((Right <> Nil) And (Right^.FDataType In (BlobFieldTypes {+ [ftBytes]})))) Then
            DatabaseError(SExprTypeMis);
          Result.FDataType := ftBoolean;
          If Right <> Nil Then
            Begin
              If IsTemporal(Left.FDataType) And (Right.FDataType In StringFieldTypes) Then
                Right.FDataType := Left.FDataType
              Else If IsTemporal(Right.FDataType) And (Left.FDataType In StringFieldTypes) Then
                Left.FDataType := Right.FDataType;
            End ;
          GetScopeKind(Result, Left, Right);
        End ;
    End ;
End ;

Function TfsExprParser.ParseExpr5: PExprNode;
Const
  Operators: Array[etADD..etDIV] Of TfsCANOperator = (
    coADD, coSUB, coMUL, coDIV);
Var
  Operator: TfsCANOperator;
  Left, Right: PExprNode;
Begin
  Result := ParseExpr6;
  While FToken In [etADD, etSUB] Do
    Begin
      If Not (poExtSyntax In FParserOptions) Then
        DatabaseError(SExprNoArith);
      Operator := Operators[FToken];
      Left := Result;
      NextToken;
      Right := ParseExpr6;
      Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
      TypeCheckArithOp(Result);
      GetScopeKind(Result, Left, Right);
    End ;
End ;

Function TfsExprParser.ParseExpr6: PExprNode;
Const
  Operators: Array[etADD..etDIV] Of TfsCANOperator = (
    coADD, coSUB, coMUL, coDIV);
Var
  Operator: TfsCANOperator;
  Left, Right: PExprNode;
Begin
  Result := ParseExpr7;
  While FToken In [etMUL, etDIV] Do
    Begin
      If Not (poExtSyntax In FParserOptions) Then
        DatabaseError(SExprNoArith);
      Operator := Operators[FToken];
      Left := Result;
      NextToken;
      Right := ParseExpr7;
      Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
      TypeCheckArithOp(Result);
      GetScopeKind(Result, Left, Right);
    End ;
End ;

Function TfsExprParser.ParseExpr7: PExprNode;
Var
  FuncName: String;
Begin
  Case FToken Of
    etSymbol:
      If (poExtSyntax In FParserOptions)
        And NextTokenIsLParen And TokenSymbolIsFunc(FTokenString) Then
        Begin
          Funcname := FTokenString;
          NextToken;
          If FToken <> etLParen Then
            DatabaseErrorFmt(SExprNoLParen, [TokenName]);
          NextToken;
          If (CompareText(FuncName, 'count') = 0) And (FToken = etMUL) Then
            Begin
              FuncName := 'COUNT(*)';
              NextToken;
            End ;
          Result := FFilter.NewNode(enFunc, coNOTDEFINED, FuncName,
            Nil, Nil);
          If FToken <> etRParen Then
            Begin
              Result.FArgs := TList.Create;
              Repeat
                Result.FArgs.Add(ParseExpr);
                If (FToken <> etComma) And (FToken <> etRParen) Then
                  DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
                If FToken = etComma Then
                  NextToken;
              Until (FToken = etRParen) Or (FToken = etEnd);
            End
          Else
            Result.FArgs := Nil;

          GetFuncResultInfo(Result);
        End
      Else If TokenSymbolIs('NULL') Then
        Begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, Null, Nil, Nil);
          Result.FScopeKind := skConst;
        End
      Else If TokenSymbolIs(FStrTrue) Then
        Begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 1, Nil, Nil);
          Result.FScopeKind := skConst;
        End
      Else If TokenSymbolIs(FStrFalse) Then
        Begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 0, Nil, Nil);
          Result.FScopeKind := skConst;
        End
      Else
        Begin
          Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, Nil, Nil);
          Result.FScopeKind := skField;
        End ;
    etName:
      Begin
        Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, Nil, Nil);
        Result.FScopeKind := skField;
      End ;
    etLiteral:
      Begin
        Result := FFilter.NewNode(enConst, coNOTDEFINED, FTokenString, Nil, Nil);
        If FNumericLit Then
          Result^.FDataType := ftFloat
        Else
          Result^.FDataType := ftString;
        Result.FScopeKind := skConst;
      End ;
    etLParen:
      Begin
        NextToken;
        Result := ParseExpr;
        If FToken <> etRParen Then
          DatabaseErrorFmt(SExprNoRParen, [TokenName]);
      End ;
    Else
      DatabaseErrorFmt(SExprExpected, [TokenName]);
      Result := Nil;
  End ;
  NextToken;
End ;

Procedure TfsExprParser.GetScopeKind(Root, Left, Right: PExprNode);
Begin
  If (Left = Nil) And (Right = Nil) Then
    Exit;
  If Right = Nil Then
    Begin
      Root.FScopeKind := Left.FScopeKind;
      Exit;
    End ;
  If ((Left^.FScopeKind = skField) And (Right^.FScopeKind = skAgg))
    Or ((Left^.FScopeKind = skAgg) And (Right^.FScopeKind = skField)) Then
    DatabaseError(SExprBadScope);
  If (Left^.FScopeKind = skConst) And (Right^.FScopeKind = skConst) Then
    Root^.FScopeKind := skConst
  Else If (Left^.FScopeKind = skAgg) Or (Right^.FScopeKind = skAgg) Then
    Root^.FScopeKind := skAgg
  Else If (Left^.FScopeKind = skField) Or (Right^.FScopeKind = skField) Then
    Root^.FScopeKind := skField;
End ;

Procedure TfsExprParser.GetFuncResultInfo(Node: PExprNode);
Begin
  Node^.FDataType := ftString;
  If (CompareText(Node^.FData, 'COUNT(*)') <> 0)
    And (CompareText(Node^.FData, 'GETDATE') <> 0)
    And ((Node^.FArgs = Nil) Or (Node^.FArgs.Count = 0)) Then
    DatabaseError(SExprTypeMis);

  If (Node^.FArgs <> Nil) And (Node^.FArgs.Count > 0) Then
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  If (CompareText(Node^.FData, 'SUM') = 0) Or
    (CompareText(Node^.FData, 'AVG') = 0) Then
    Begin
      Node^.FDataType := ftFloat;
      Node^.FScopeKind := skAgg;
    End
  Else If (CompareText(Node^.FData, 'MIN') = 0) Or
    (CompareText(Node^.FData, 'MAX') = 0) Then
    Begin
      Node^.FDataType := PExprNode(Node^.FArgs.Items[0])^.FDataType;
      Node^.FScopeKind := skAgg;
    End
  Else If (CompareText(Node^.FData, 'COUNT') = 0) Or
    (CompareText(Node^.FData, 'COUNT(*)') = 0) Then
    Begin
      Node^.FDataType := ftInteger;
      Node^.FScopeKind := skAgg;
    End
  Else If (CompareText(Node^.FData, 'YEAR') = 0) Or
    (CompareText(Node^.FData, 'MONTH') = 0) Or
    (CompareText(Node^.FData, 'DAY') = 0) Or
    (CompareText(Node^.FData, 'HOUR') = 0) Or
    (CompareText(Node^.FData, 'MINUTE') = 0) Or
    (CompareText(Node^.FData, 'SECOND') = 0) Then
    Begin
      Node^.FDataType := ftInteger;
      Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
    End
  Else If CompareText(Node^.FData, 'GETDATE') = 0 Then
    Begin
      Node^.FDataType := ftDateTime;
      Node^.FScopeKind := skConst;
    End
  Else If CompareText(Node^.FData, 'DATE') = 0 Then
    Begin
      Node^.FDataType := ftDate;
      Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
    End
  Else If CompareText(Node^.FData, 'TIME') = 0 Then
    Begin
      Node^.FDataType := ftTime;
      Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
    End ;
End ;

Function TfsExprParser.TokenName: String;
Begin
  If FSourcePtr = FTokenPtr Then
    Result := SExprNothing
  Else
    Begin
      SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
      Result := '''' + Result + '''';
    End ;
End ;

Function TfsExprParser.TokenSymbolIs(Const S: String): Boolean;
Begin
  Result := (FToken = etSymbol) And (CompareText(FTokenString, S) = 0);
End ;

Function TfsExprParser.TokenSymbolIsFunc(Const S: String): Boolean;
Begin
  Result := (CompareText(S, 'UPPER') = 0) Or
    (CompareText(S, 'LOWER') = 0) Or
    (CompareText(S, 'SUBSTRING') = 0) Or
    (CompareText(S, 'TRIM') = 0) Or
    (CompareText(S, 'TRIMLEFT') = 0) Or
    (CompareText(S, 'TRIMRIGHT') = 0) Or
    (CompareText(S, 'YEAR') = 0) Or
    (CompareText(S, 'MONTH') = 0) Or
    (CompareText(S, 'DAY') = 0) Or
    (CompareText(S, 'HOUR') = 0) Or
    (CompareText(S, 'MINUTE') = 0) Or
    (CompareText(S, 'SECOND') = 0) Or
    (CompareText(S, 'GETDATE') = 0) Or
    (CompareText(S, 'DATE') = 0) Or
    (CompareText(S, 'TIME') = 0) Or
    (CompareText(S, 'SUM') = 0) Or
    (CompareText(S, 'MIN') = 0) Or
    (CompareText(S, 'MAX') = 0) Or
    (CompareText(S, 'AVG') = 0) Or
    (CompareText(S, 'COUNT') = 0);

End ;

Procedure TfsExprParser.TypeCheckArithOp(Node: PExprNode);
Begin
  With Node^ Do
    Begin
      If IsNumeric(FLeft.FDataType) And IsNumeric(FRight.FDataType) Then
        FDataType := ftFloat
      Else If (FLeft.FDataType In StringFieldTypes) And
        (FRight.FDataType In StringFieldTypes) And (FOperator = coADD) Then
        FDataType := ftString
      Else If IsTemporal(FLeft.FDataType) And IsNumeric(FRight.FDataType) And
        (FOperator = coADD) Then
        FDataType := ftDateTime
      Else If IsTemporal(FLeft.FDataType) And IsNumeric(FRight.FDataType) And
        (FOperator = coSUB) Then
        FDataType := FLeft.FDataType
      Else If IsTemporal(FLeft.FDataType) And IsTemporal(FRight.FDataType) And
        (FOperator = coSUB) Then
        FDataType := ftFloat
      Else If (FLeft.FDataType In StringFieldTypes) And IsTemporal(FRight.FDataType) And
        (FOperator = coSUB) Then
        Begin
          FLeft.FDataType := FRight.FDataType;
          FDataType := ftFloat;
        End
      Else If (FLeft.FDataType In StringFieldTypes) And IsNumeric(FRight.FDataType) And
        (FLeft.FKind = enConst) Then
        FLeft.FDataType := ftDateTime
      Else
        DatabaseError(SExprTypeMis);
    End ;
End ;

End.

