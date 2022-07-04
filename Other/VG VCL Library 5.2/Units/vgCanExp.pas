{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         CANExpr expression evaluator                  }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgCanExp;

interface
uses BDE, SysUtils;

type
  TEvalFunction = (efUnknown, efSum, efMin, efMax, efAvg, efCount, efUpper,
    efLower, efSubString, efTrim, efTrimLeft, efTrimRight, efYear, efMonth,
    efDay, efHour, efMinute, efSecond, efGetDate, efDate, efTime);

  TExprEvaluator = class
  private
    FExpr: PCanExpr;
    FOpList: Variant;
    { Compare }
    function CompareOperandsEx(const Op1, Op2: Variant; CaseIns: Boolean;
      PartialLen: Word; CompareLike: Boolean): Integer;
    function CompareOperandsEx2(var Op1, Op2: Variant; var iFldType1, iFldLen1,
      iFldType2, iFldLen2: Integer): Integer;
    function CompareOperands(const Op1, Op2: Variant): Integer;
    { Existense }
    function Exists(const Op1, Op2: Variant): Boolean;
    { Operand extraction }
    function GetConstantValue(DataType, Size, Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFieldValue(FieldNo: Word; Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFuncValue(Func: TEvalFunction; Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFuncValueInternal(Func: TEvalFunction; Args: Variant; var iFldType, iFldLen: Integer): Variant;
    function GetOperandValue(Offset: Word; var iFldType, iFldLen: Integer): Variant;
  protected
    { Abstract functions }
    function DoGetFieldValue(FieldNo: Word; const FieldName: string;
      var iFldType, iFldLen: Integer): Variant; virtual;
    procedure DoFirst; virtual;
    procedure DoNext; virtual;
    function DoGetEOF: Boolean; virtual;
    procedure DoBeginAgg; virtual;
    procedure DoEndAgg; virtual;
    procedure DoBeginEvalute; virtual;
    procedure DoEndEvalute; virtual;
  public
    destructor Destroy; override;
    procedure SetData(Value: PCanExpr); virtual;
    function Evalute(var iFldType, iFldLen: Integer): Variant;
    property Data: PCanExpr read FExpr;
  end;

  ENotSupported = class (Exception);

function IsNumeric(DataType: Integer): Boolean;
function IsTemporal(DataType: Integer): Boolean;

procedure NotSupported;

const
  efAggregates  = [efSum, efMin, efMax, efAvg, efCount];

implementation
uses vgVCLRes, vgUtils, vgBCDUtl;

function IsNumeric(DataType: Integer): Boolean;
begin
  Result := DataType in [fldINT16, fldUINT16, fldINT32, fldFLOAT, fldBCD];
end;

function IsTemporal(DataType: Integer): Boolean;
begin
  Result := DataType in [fldDATE, fldTIME, fldTIMESTAMP];
end;

procedure NotSupported;
begin
  raise ENotSupported.Create(LoadStr(SNotSupported));
end;

const
  NumericLen    = 10;

function StrToEvalFunction(const Func: string): TEvalFunction;
begin
  if (CompareText(Func, 'SUM') = 0) then
    Result := efSum
  else if (CompareText(Func, 'MIN') = 0) then
    Result := efMin
  else if (CompareText(Func, 'MAX') = 0) then
    Result := efMax
  else if (CompareText(Func, 'AVG') = 0) then
    Result := efAvg
  else if (CompareText(Func, 'COUNT') = 0) then
    Result := efCount
  else if (CompareText(Func, 'COUNT(*)') = 0) then
    Result := efCount
  else if (CompareText(Func, 'UPPER') = 0) then
    Result := efUpper
  else if (CompareText(Func, 'LOWER') = 0) then
    Result := efLower
  else if (CompareText(Func, 'SUBSTRING') = 0) then
    Result := efSubString
  else if (CompareText(Func, 'TRIM') = 0) then
    Result := efTrim
  else if (CompareText(Func, 'TRIMLEFT') = 0) then
    Result := efTrimLeft
  else if (CompareText(Func, 'TRIMRIGHT') = 0) then
    Result := efTrimRight
  else if (CompareText(Func, 'YEAR') = 0) then
    Result := efYear
  else if (CompareText(Func, 'MONTH') = 0) then
    Result := efMonth
  else if (CompareText(Func, 'DAY') = 0) then
    Result := efDay
  else if (CompareText(Func, 'HOUR') = 0) then
    Result := efHour
  else if (CompareText(Func, 'MINUTE') = 0) then
    Result := efMinute
  else if (CompareText(Func, 'SECOND') = 0) then
    Result := efSecond
  else if (CompareText(Func, 'GETDATE') = 0) then
    Result := efGetDate
  else if (CompareText(Func, 'DATE') = 0) then
    Result := efDate
  else if (CompareText(Func, 'TIME') = 0) then
    Result := efTime
  else
    Result := efUnknown;
end;

procedure PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer;
  var iFldType, iFldLen: Integer);
begin
  if iFldType1 = fldINT32 then
  begin
    iFldType := iFldType1;
    iFldLen  := iFldLen1;
  end else begin
    iFldType := iFldType2;
    iFldLen  := iFldLen2;
  end
end;

procedure PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer;
  var iFldType, iFldLen: Integer);
begin
  if iFldType1 = fldFLOAT then
  begin
    iFldType := iFldType1;
    iFldLen  := iFldLen1;
  end else if iFldType2 = fldFLOAT then
  begin
    iFldType := iFldType2;
    iFldLen  := iFldLen2;
  end else
    PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2,  iFldType, iFldLen);
end;

procedure PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer;
  var iFldType, iFldLen: Integer);
begin
  if iFldType1 = fldZSTRING then
  begin
    iFldType := fldZSTRING;
    if iFldType2 = fldZSTRING then
      iFldLen  := Max(iFldLen1, iFldLen2)
    else
      iFldLen  := Max(iFldLen1, NumericLen);
  end else if iFldType1 = fldZSTRING then
  begin
    iFldType := fldZSTRING;
    if iFldType1 = fldZSTRING then
      iFldLen  := Max(iFldLen1, iFldLen2)
    else
      iFldLen  := Max(iFldLen2, NumericLen);
  end else
    PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2,  iFldType, iFldLen);
end;

procedure CheckNotBCD(var Operand: Variant; var iFldType, iFldLen: Integer);
var
  Curr: Currency;
begin
  if (iFldType = fldBCD) then
  begin
    if not VarIsNull(Operand) then
    begin
      FMTBCDToCurr(pFMTBCD(TVarData(Operand).VString)^, Curr);
      Operand := Curr;
    end;
    iFldType := fldFLOAT;
    iFldLen  := SizeOf(Double);
  end;
end;

procedure CheckBCD(var Operand: Variant; var iFldType, iFldLen: Integer);
var
  Curr: Currency;
  BCD: FMTBCD;
  S: string;
begin
  if (iFldType <> fldBCD) then
  begin
    if not VarIsNull(Operand) then
    begin
      Curr := Operand;
      CurrToFMTBCD(Curr, BCD, 32, 18);
      SetString(S, PChar(@BCD), SizeOf(FMTBCD));
      Operand := S;
    end;
    iFldType := fldBCD;
    iFldLen  := SizeOf(FMTBCD);
  end;
end;

{ TExprEvaluator }
destructor TExprEvaluator.Destroy;
begin
  FOpList := Unassigned;
  SetData(nil);
  inherited;
end;

procedure TExprEvaluator.SetData(Value: PCanExpr);
begin
  ReallocMem(FExpr, 0);

  if Assigned(Value) then
  begin
    GetMem(FExpr, Value^.iTotalSize);
    Move(Value^, FExpr^, Value^.iTotalSize);
  end;
end;

function TExprEvaluator.Evalute(var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

begin
  DoBeginEvalute;
  try
    if Assigned(FExpr) then
      Result := GetOperandValue(0, iFldType, iFldLen)
    else begin
      Result := Null;
      SetResult(fldUNKNOWN, 0);
    end;
  finally
    DoEndEvalute;
  end;
end;

function TExprEvaluator.CompareOperandsEx(const Op1, Op2: Variant;
  CaseIns: Boolean; PartialLen: Word; CompareLike: Boolean): Integer;
var
  OpNull1, OpNull2: Boolean;
begin
  try
    OpNull1 := VarIsNull(Op1);
    OpNull2 := VarIsNull(Op2);

    if OpNull1 or OpNull2 then
    begin
      Result := ord(OpNull1 and OpNull2 and not CaseIns and not CompareLike and (PartialLen = 0));
      Exit;
    end;

    if CompareLike then
    begin
      if CaseIns then
        Result := ord(Like(AnsiUpperCase(Op1), AnsiUpperCase(Op2))) else
        Result := ord(Like(Op1, Op2));
    end else begin
      if CaseIns then
        if PartialLen = 0 then
          Result := AnsiStrIComp(PChar(string(Op1)), PChar(string(Op2))) else
          Result := AnsiStrLIComp(PChar(string(Op1)), PChar(string(Op2)), PartialLen)
      else
        if PartialLen = 0 then
          Result := CompareOperands(Op1, Op2) else
          Result := AnsiStrLComp(PChar(string(Op1)), PChar(string(Op2)), PartialLen)
    end;
  except
    on EVariantError do Result := 1;
  end;
end;

function TExprEvaluator.CompareOperandsEx2(var Op1, Op2: Variant; var iFldType1, iFldLen1, iFldType2, iFldLen2: Integer): Integer;
begin
  try
    if (iFldType1 = fldBCD) or (iFldType2 = fldBCD) then
    begin
      CheckBCD(Op1, iFldType1, iFldLen1);
      CheckBCD(Op2, iFldType2, iFldLen2);
      Result := CompareFMTBCD(pFMTBCD(TVarData(Op1).VString)^, pFMTBCD(TVarData(Op2).VString)^);
    end else
      Result := CompareOperands(Op1, Op2);
  except
    on EVariantError do Result := 1;
  end;
end;

function TExprEvaluator.CompareOperands(const Op1, Op2: Variant): Integer;
begin
  try
    if Op1 > Op2 then Result :=  1 else
    if Op1 < Op2 then Result := -1 else Result := 0;
  except
    on EVariantError do Result := 1;
  end;
end;

function TExprEvaluator.Exists(const Op1, Op2: Variant): Boolean;
var
  I: Integer;
begin
  for I := 0 to VarArrayHighBound(Op2, 1) do
    if CompareOperands(Op1, Op2[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TExprEvaluator.GetOperandValue(Offset: Word; var iFldType, iFldLen: Integer): Variant;

var
  iFldType1, iFldType2, iFldLen1, iFldLen2: Integer;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

var
  I: Integer;
  S: string;
  CanHdr: pCanHdr;
  BCD: FMTBCD;
  P: pFMTBCD;
  Op1, Op2: Variant;
  Func: TEvalFunction;
begin
  CanHdr := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + Offset);

  case CanHdr^.nodeClass of
    nodeNULL:                           { Null node                  (*) }
      begin
        Result := Null;
        SetResult(fldUNKNOWN, 0);
      end;
    nodeUNARY:                          { Node is a unary            (*) }
      with pCanUnary(CanHdr)^ do
        case canOp of
          canISBLANK,                   { CANUnary;  is operand blank.     (*) }
          canNOTBLANK:                  { CANUnary;  is operand not blank. (*) }
            begin
              Result := VarIsNull(GetOperandValue(iOperand1, iFldType1, iFldLen1)) xor (canOp = canNOTBLANK);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canNOT:                       { CANUnary; NOT                    (*) }
            begin
              Result := not GetOperandValue(iOperand1, iFldType1, iFldLen1);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
        canMINUS:                     { CANUnary;  minus. }
            begin
              Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
              if iFldType1 = fldBCD then
              begin
                P := pFMTBCD(TVarData(Op1).VString);
                P.iSignSpecialPlaces := P.iSignSpecialPlaces xor $80;
                Result := Op1;
              end else
                Result := - GetOperandValue(iOperand1, iFldType1, iFldLen1);
              SetResult(iFldType1, iFldLen1);
            end;
          canUPPER:                     { CANUnary: upper case }
            begin
              Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
              if VarIsNull(Op1) then
                Result := Null else Result := AnsiUpperCase(Op1);
              SetResult(iFldType1, iFldLen1);
            end;
          canLOWER:                     { CANUnary: lower case }
            begin
              Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
              if VarIsNull(Op1) then
                Result := Null else Result := AnsiLowerCase(Op1);
              SetResult(iFldType1, iFldLen1);
            end;
        else
          NotSupported;
        end;
    nodeBINARY:                         { Node is a binary           (*) }
      with pCanBinary(CanHdr)^ do
      begin
        Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
        if canOp <> canASSIGN then
          Op2 := GetOperandValue(iOperand2, iFldType2, iFldLen2);
        case canOp of
          canEQ,                        { CANBinary, CANCompare; equal.    (*) }
          canNE:                        { CANBinary; NOT equal.            (*) }
            begin
              Result := ((VarIsNull(Op1) and VarIsNull(Op2)) or
                (CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) = 0)) xor (canOp = canNE);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canGT:                        { CANBinary; greater than.         (*) }
            begin
              if not (VarIsNull(Op1) or VarIsNull(Op2)) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) > 0 else
                Result := False;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canLT:                        { CANBinary; less than.            (*) }
            begin
              if not (VarIsNull(Op1) or VarIsNull(Op2)) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) < 0 else
                Result := False;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canGE:                        { CANBinary; greater or equal.     (*) }
            begin
              if not (VarIsNull(Op1) or VarIsNull(Op2)) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) >= 0 else
                Result := False;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canLE:                        { CANBinary; less or equal.        (*) }
            begin
              if not (VarIsNull(Op1) or VarIsNull(Op2)) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) <= 0 else
                Result := False;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canAND:                       { CANBinary; AND                   (*) }
            begin
              Result := Op1 and Op2;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canOR:                        { CANBinary; OR                    (*) }
            begin
              Result := Op1 or Op2;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canADD:                       { CANBinary; addition. }
            begin
              if (iFldType1 = fldBCD) or (iFldType2 = fldBCD) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                BCD := AddFMTBCD(pFMTBCD(TVarData(Op1).VString)^, pFMTBCD(TVarData(Op2).VString)^);
                SetString(S, PChar(@BCD), SizeOf(FMTBCD));
                Result := S;
                SetResult(fldBCD, SizeOf(FMTBCD));
              end else begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
                if (iFldType1 = fldZSTRING) or (iFldType2 = fldZSTRING) then
                  Result := Concat(Op1, Op2)
                else
                  Result := Op1 + Op2;
                if iFldType = fldZSTRING then
                  iFldLen := iFldLen1 + iFldLen2;
              end;
            end;
          canSUB:                       { CANBinary; subtraction. }
            begin
              if (iFldType1 = fldBCD) or (iFldType2 = fldBCD) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                BCD := SubFMTBCD(pFMTBCD(TVarData(Op1).VString)^, pFMTBCD(TVarData(Op2).VString)^);
                SetString(S, PChar(@BCD), SizeOf(FMTBCD));
                Result := S;
                SetResult(fldBCD, SizeOf(FMTBCD));
              end else begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                Result := Op1 - Op2;
                PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
              end;
            end;
          canMUL:                       { CANBinary; multiplication. }
            begin
              if (iFldType1 = fldBCD) or (iFldType2 = fldBCD) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                BCD := MulFMTBCD(pFMTBCD(TVarData(Op1).VString)^, pFMTBCD(TVarData(Op2).VString)^);
                SetString(S, PChar(@BCD), SizeOf(FMTBCD));
                Result := S;
                SetResult(fldBCD, SizeOf(FMTBCD));
              end else begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                Result := Op1 * Op2;
                PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
              end;
            end;
          canDIV:                       { CANBinary; division. }
            begin
              if (iFldType1 = fldBCD) or (iFldType2 = fldBCD) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                BCD := DivFMTBCD(pFMTBCD(TVarData(Op1).VString)^, pFMTBCD(TVarData(Op2).VString)^);
                SetString(S, PChar(@BCD), SizeOf(FMTBCD));
                Result := S;
                SetResult(fldBCD, SizeOf(FMTBCD));
              end else begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                if VarIsNull(Op2) or (CompareOperands(Op2, 0) = 0) then
                  Result := Null else Result := Op1 / Op2;
                SetResult(fldFLOAT, SizeOf(Double));
              end;
            end;
          canMOD:                       { CANBinary; modulo division. }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsNull(Op2) or (CompareOperands(Op2, 0) = 0) then
                Result := Null else Result := Op1 mod Op2;
              PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;
          canREM:                       { CANBinary; remainder of division. }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsNull(Op2) or (CompareOperands(Op2, 0) = 0) then
                Result := Null else Result := Op1 - Op1 div Op2 * Op2;
              PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;
          canMIN:                       { CANBinary, find minimum of. }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if (CompareOperands(Op1, Op2) < 0) then
                Result := Op1 else Result := Op2;
              PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;
          canMAX:                       { CANBinary, find maximum of. }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if (CompareOperands(Op1, Op2) > 0) then
                Result := Op1 else Result := Op2;
              PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;
          canAVG:                       { CANBinary, find average of. }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              Result := (Op1 + Op2) / 2;
              SetResult(fldFLOAT, SizeOf(Double));
            end;
          canLIKE:
            begin
              Result := CompareOperandsEx(Op1, Op2, False, 0, True);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canIN:                        { CANBinary field in list of values }
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              Result := Exists(Op1, Op2);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
          canASSIGN:
            begin
              Result := Op1;
              SetResult(iFldType1, iFldLen1);
            end;
          else
            NotSupported;
        end;
      end;
    nodeCOMPARE:                        { Node is a compare          (*) }
      with pCanCompare(CanHdr)^ do
        case canOp of
          canEQ,                        { CANBinary, CANCompare; equal.    (*) }
          canNE,                        { CANBinary; NOT equal.            (*) }
          canLIKE:
            begin
              Result := (CompareOperandsEx(iOperand1, iOperand2,
                bCaseInsensitive, iPartialLen, canOp = canLIKE) = 0) xor (canOp = canNE);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
        end;
    nodeFIELD:                          { Node is a field            (*) }
      with pCanField(CanHdr)^ do
        Result := GetFieldValue(iFieldNum, iNameOffset, iFldType, iFldLen);
    nodeCONST:                          { Node is a constant         (*) }
      with pCanConst(CanHdr)^ do
        Result := GetConstantValue(iType, iSize, iOffset, iFldType, iFldLen);
    nodeCONTINUE:                       { Node is a continue node    (*) }
      begin
        Result := True;
        SetResult(fldBOOL, SizeOf(WordBool));
      end;
    nodeLIST:                           { Node is a LIST node }
      begin
        with pCanList(CanHdr)^ do
          Result := GetOperandValue(iOffset, iFldType1, iFldLen1);
        SetResult(fldUNKNOWN, 0);
      end;
    nodeFUNC:                           { Node is a Function node }
      with pCANFunc(CanHdr)^ do
      begin
        Func := StrToEvalFunction(GetConstantValue(fldZSTRING, 0, iNameOffset, iFldType1, iFldLen1));
        Result := GetFuncValue(Func, iElemOffset, iFldType, iFldLen);
      end;
    nodeLISTELEM:                       { Node is a List Element node }
      begin
        if not VarIsArray(FOpList) then
          FOpList := VarArrayCreate([0, 9], varVariant);
        I := 0;
        repeat
          if I > VarArrayHighBound(FOpList, 1) then
             VarArrayRedim(FOpList, I + 10);

          with pCanListElem(CanHdr)^ do
          begin
            FOpList[I] := VarArrayOf([GetOperandValue(iOffset, iFldType1, iFldLen1), iFldType1, iFldLen1]);
            if iNextOffset = 0 then
            begin
              VarArrayRedim(FOpList, I);
              Result := FOpList;
              SetResult(fldUNKNOWN, 0);
              Exit;
            end;
            CanHdr := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + iNextOffset);
            Inc(I);
          end;
        until False;
      end;
    else
      NotSupported;
  end;
end;

function TExprEvaluator.GetConstantValue(DataType, Size, Offset: Word; var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

type
  PSmallint = ^Smallint;
  PInteger  = ^Integer;
  PWordBool = ^WordBool;
var
  CanHdr: pCanHdr;
  S: string;
begin
  CanHdr := pCanHdr(PChar(FExpr) + FExpr.iLiteralStart + Offset);

  case DataType of
    fldINT16, fldUINT16:
      begin
        Result := VarAsType(PSmallint(CanHdr)^, varSmallint);
        SetResult(DataType, SizeOf(SmallInt));
      end;
    fldINT32:
      begin
        Result := VarAsType(PInteger(CanHdr)^, varInteger);
        SetResult(DataType, SizeOf(Integer));
      end;
    fldFLOAT:
      begin
        Result := Double(CanHdr^);
        SetResult(DataType, SizeOf(Double));
      end;
    fldZSTRING:
      begin
        Result := StrPas(PChar(CanHdr));
        SetResult(DataType, Length(Result) + 1);
      end;
    fldDATE, fldTIME, fldTIMESTAMP:
      begin
        Result := VarAsType(TDateTime(CanHdr^), varDate);
        SetResult(DataType, SizeOf(TDateTime));
      end;
    fldBOOL:
      begin
        Result := VarAsType(PWordBool(CanHdr)^, varBoolean);
        SetResult(fldBOOL, SizeOf(WordBool));
      end;
    fldBCD:
      begin
        SetString(S, PChar(CanHdr), SizeOf(FMTBCD));
        Result := S;
        SetResult(fldBCD, SizeOf(FMTBCD));
      end;
    else
      NotSupported;
  end;
end;

function TExprEvaluator.GetFieldValue(FieldNo, Offset: Word; var iFldType, iFldLen: Integer): Variant;
var
  CanHdr: pCanHdr;
begin
  CanHdr := pCanHdr(PChar(FExpr) + FExpr.iLiteralStart + Offset);
  Result := DoGetFieldValue(FieldNo, StrPas(PChar(CanHdr)), iFldType, iFldLen);
end;

function TExprEvaluator.GetFuncValueInternal(Func: TEvalFunction; Args: Variant; var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

  procedure SetResultStr(Arg: Variant);
  var
    iFieldType: Integer;
  begin
    iFieldType := Arg[1];
    if iFieldType = fldZSTRING then
      SetResult(fldZSTRING, Arg[2]) else
      SetResult(fldZSTRING, NumericLen);
  end;

var
  Tmp: Variant;
  S: string;
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  Start, Count: Integer;
  TimeStamp: TDateTime;
begin
  case Func of
    efUpper, efLower:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          S := Tmp;
          if Func = efUpper then
            Result := AnsiUpperCase(S) else
            Result := AnsiLowerCase(S);
        end;
      end;
    efSubString:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          Start  := Args[1][0];
          Count  := Args[2][0];
          Result := Copy(S, Start, Count);
        end;
      end;
    efTrim, efTrimLeft, efTrimRight:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          S := Tmp;
          if Func = efTrim then
            Result := Trim(S)
          else if Func = efTrimLeft then
            Result := TrimLeft(S)
          else
            Result := TrimRight(S);
        end;
      end;
    efYear, efMonth, efDay:
      begin
        if VarIsNull(Args) then
          TimeStamp := Date else
          TimeStamp := Args[0][0];
        DecodeDate(TimeStamp, Year, Month, Day);
        SetResult(fldUINT16, SizeOf(Word));
        if Func = efYear then
          Result := Year
        else if Func = efMonth then
          Result := Month
        else
          Result := Day;
      end;
    efHour, efMinute, efSecond:
      begin
        if VarIsNull(Args) then
          TimeStamp := Time else
          TimeStamp := Args[0][0];
        DecodeTime(TimeStamp, Hour, Min, Sec, MSec);
        SetResult(fldUINT16, SizeOf(Word));
        if Func = efHour then
          Result := Hour
        else if Func = efMinute then
          Result := Min
        else
          Result := Sec;
      end;
    efGetDate:
      begin
        Result := Now;
        SetResult(fldTIMESTAMP, SizeOf(TDateTime));
      end;
    efDate:
      begin
        TimeStamp := Int(Args[0][0]);
        Result := TimeStamp;
        SetResult(fldDATE, SizeOf(TDateTime));
      end;
    efTime:
      begin
        TimeStamp := Frac(Args[0][0]);
        Result := TimeStamp;
        SetResult(fldTIME, SizeOf(TDateTime));
      end;
  end;
end;

function TExprEvaluator.GetFuncValue(Func: TEvalFunction; Offset: Word;
  var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

  function GetOperand: Variant;
  var
    iFldType1, iFldLen1: Integer;
  begin
    if Offset > 0 then
      Result := GetOperandValue(Offset, iFldType1, iFldLen1) else
      Result := VarArrayOf([Null, fldUNKNOWN, 0]);
  end;

var
  Sum: Double;
  Count, Cmp: Integer;
  Tmp, Tmp2: Variant;
  BCD: FMTBCD;
  S: string;
begin
  if Func in efAggregates then DoBeginAgg;
  try
    case Func of
      efUnknown:
        NotSupported;

      efSum:
        begin
          Sum    := 0;
          DoFirst;
          Tmp    := GetOperand[0];
          SetResult(Tmp[1], Tmp[2]);
          Tmp2 := Tmp[0];
          if iFldType = fldBCD then
          begin
            if VarIsNull(Tmp2) then
            begin
              SetZeroBcd(BCD);
              SetString(S, PChar(@BCD), SizeOf(FMTBCD));
              Result := S;
            end else
              Result := Tmp2
          end else begin
            if not VarIsNull(Tmp2) then
              Result := Tmp2 else
              Result := Sum;
          end;
          DoNext;
          while not DoGetEOF do
          begin
            Tmp := GetOperand[0][0];
            if not VarIsNull(Tmp) then
            begin
              if iFldType = fldBCD then
              begin
                BCD := AddFMTBCD(pFMTBCD(TVarData(Result).VString)^, pFMTBCD(TVarData(Tmp).VString)^);
                SetString(S, PChar(@BCD), SizeOf(FMTBCD));
                Result := S;
              end else
                Result := Result + Tmp;
            end;
            DoNext;
          end;
        end;

      efMin, efMax:
        begin
          DoFirst;
          Tmp    := GetOperand[0];
          Result := Tmp[0];
          SetResult(Tmp[1], Tmp[2]);
          DoNext;
          while not DoGetEOF do
          begin
            Tmp := GetOperand[0][0];
            Cmp := CompareOperandsEx2(Result, Tmp, iFldType, iFldLen, iFldType, iFldLen);
            if (Func = efMin) and (Cmp < 0) or (Func = efMax) and (Cmp > 0) then
              Result := Tmp;
            DoNext;
          end;
        end;

      efAvg:
        begin
          Sum   := 0;
          Count := 0;
          DoFirst;
          while not DoGetEOF do
          begin
            Tmp := GetOperand[0][0];
            CheckNotBCD(Tmp, iFldType, iFldLen);
            Sum := Sum + Tmp;
            Inc(Count);
            DoNext;
          end;
          if Count > 0 then
            Result := Sum / Count;
          SetResult(fldFLOAT, SizeOf(Double));
        end;

      efCount:
        begin
          Count := 0;
          DoFirst;
          while not DoGetEOF do
          begin
            Inc(Count);
            DoNext;
          end;
          Result := Count;
          SetResult(fldINT32, SizeOf(Integer));
        end;

      Succ(efCount)..High(TEvalFunction):
        Result := GetFuncValueInternal(Func, GetOperand, iFldType, iFldLen);
    end;
  finally
    if Func in efAggregates then DoEndAgg;
  end;
end;

function TExprEvaluator.DoGetFieldValue(FieldNo: Word; const FieldName: string;
  var iFldType, iFldLen: Integer): Variant;
begin
  Result   := Null;
  iFldType := fldUNKNOWN;
  iFldLen  := 0;
end;

procedure TExprEvaluator.DoFirst;
begin
end;

procedure TExprEvaluator.DoNext;
begin
end;

function TExprEvaluator.DoGetEOF: Boolean;
begin
  Result := True;
end;

procedure TExprEvaluator.DoBeginEvalute;
begin
end;

procedure TExprEvaluator.DoEndEvalute;
begin
end;

procedure TExprEvaluator.DoBeginAgg;
begin
end;

procedure TExprEvaluator.DoEndAgg;
begin
end;

end.

