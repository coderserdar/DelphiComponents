unit ZREval;

interface

{$I ZRDefine.inc}

uses
  SysUtils,                                         // Delphi RTL
  Classes,                                          // Delphi VCL
{$IFDEF D6Above}
  Variants,
{$ENDIF}
  ZRFormat;                                         // ZReport

type
  TZRElementCollection = class;                     // forward declaration

  { TZRElementLibrary }

  TArguments = TList;

  TZRElementLibrary = class(TObject)
  private
    fOwner      : TZRElementCollection;
    fName       : String;
    fDescription: String;
    fVendor     : String;
  protected
    function GetCount : Integer; virtual; abstract;
    function GetItem(Index: Integer): String; virtual; abstract;
    function Check(Index : Integer; Arguments: TArguments): Boolean; virtual;
    function Calculate(Index : Integer; Arguments: TArguments): Variant; virtual; abstract;
    property Owner : TZRElementCollection read fOwner;
  public
    constructor Create(aOwner: TZRElementCollection); virtual;
    function IndexOf(const Name: String): Integer; virtual;
    function  GetValue(Index : Integer; Arguments: TArguments): Variant;
    procedure SetValue(Index : Integer; Value: Variant); virtual; abstract;
    property Count : Integer read GetCount;
    property Items[Index: Integer]: String read GetItem; default;
    property Description: String read fDescription write fDescription;
    property Name       : String read fName        write fName;
    property Vendor     : String read fVendor      write fVendor;
  end;
  TZRElementLibraryClass = class of TZRElementLibrary;

  { TZRVariableLibrary }
  TZRVariableLibrary = class(TZRElementLibrary)
  protected
    function Check(Index : Integer; Arguments: TArguments): Boolean; override;
  end;

  { TZRElementCollection }
  TZRElementCollection = class(TStringList)
  protected
    function InternalFindEntry(const Name: String; var aLibrary: TZRElementLibrary; var aIndex: Integer): Boolean; virtual;
  public
    destructor Destroy; override;
    function RegisterLibrary(aClass: TZRElementLibraryClass; aName, aDescription, aVendor: String): TZRElementLibrary;
    function FindEntry(const Name: String; var aLibrary: TZRElementLibrary; var aIndex: Integer): Boolean;
  end;

  { TZRElement }
  TZRElement = class(TObject)
  protected
    function  GetValue: Variant; virtual;
    procedure SetValue(Value: Variant); virtual;
  public
    property Value: Variant read GetValue write SetValue;
  end;

  { TZRElementConstant }
  TZRElementConstant = class(TZRElement)
  private
    fValue : Variant;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(aValue : Variant);
  end;

  { TZRElementFunction }
  TZRElementFunction = class(TZRElement)
  protected
    fName     : String;
    fLibrary  : TZRElementLibrary;
    fIndex    : Integer;
    fArguments: TArguments;
  protected
    function  GetValue: Variant; override;
    procedure SetValue(Value: Variant); override;
    property Arguments: TList read fArguments;
  public
    destructor Destroy; override;
    constructor Create(aName: String; aLibrary: TZRElementLibrary; aIndex: Integer);
    property Name: String read fName write fName;
  end;

  { TZREvaluator }

  EZREvaluatorError = class(Exception);

  TZREvaluator = class(TComponent)
  private
    fElement    : TZRElement;
    fExpression : String;
    fErrorMsg   : String;
    fCollection : TZRElementCollection;
    function GetString: String;
  protected
    procedure SetExpression(const Value: String); virtual;
    function GetValue: Variant; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Parse: Boolean; virtual;
    property Collection: TZRElementCollection read fCollection write fCollection;
    property Expression: String read fExpression write SetExpression;
    property AsString: String read GetString;
    property Value : Variant read GetValue;
  end;

function Calculate(Formula: String; Values: array of const): Variant;

const
  szrVendorZYZ = 'ZYZ';

var
  ZRCollection : TZRElementCollection;

implementation

uses
  Math,                                             // Delphi RTL
  ZRConst;                                          // ZReport


function Calculate(Formula: String; Values: array of const): Variant;
var
  E: TZREvaluator;
begin
  Formula:= Format(Formula, Values);
  E:= TZREvaluator.Create(nil);
  try
    E.Expression := Formula;
    E.Parse;
    Result := E.Value;
  finally
    E.Free;
  end;
end;


{ TZROperator }
type
  TZROperator  = (zopLess, zopLessOrEqual, zopGreater,
                  zopGreaterOrEqual, zopEqual, zopUnequal,
                  zopLogicalOr, zopLogicalAnd, zopPlus, zopMinus,
                  zopMul, zopDiv, zopMod, zopPower,
                  zopBitwiseOr, zopBitwiseAnd, zopBitwiseXor,
                  zopShl, zopShr, zopAssign);

  TZRElementOperator = class(TZRElement)
  private
    fLeft,
    fRight   : TZRElement;
    fOperator: TZROperator;
    function GetValue: Variant; override;
  public
    constructor Create(aOperator: TZROperator; aLeft, aRight: TZRElement);
    destructor  Destroy; override;
    property Operator: TZROperator read fOperator;
  end;

constructor TZRElementOperator.Create(aOperator: TZROperator; aLeft, aRight: TZRElement);
begin
  inherited Create;
  fOperator:= aOperator;
  fLeft    := aLeft;
  fRight   := aRight;
end;

destructor TZRElementOperator.Destroy;
begin
  fRight.Free;
  fLeft .Free;
  inherited;
end;

function TZRElementOperator.GetValue: Variant;

  procedure Error(ID: Integer);
  begin
    raise Exception.CreateRes(ID);
  end;

var
  Left,
  Right    : Variant;
  LeftKind,
  RightKind: TZValueKind;

  function ConvertNull(Kind: TZValueKind): Variant;
  begin
    case Kind of
      zvkString   : Result := '';
      zvkInteger,
      zvkFloat,
      zvkCurrency : Result := 0;
      zvkBoolean  : Result := False;
      zvkDateTime : Result := 0;
      else          Result := Null;      //VarClear(Result);
    end;
  end;

  function DoDates: TDateTime;
  begin
    if LeftKind = RightKind then Error(szrIllegalDate);
    if not (LeftKind  in [zvkDateTime, zvkInteger, zvkFloat]) or
       not (RightKind in [zvkDateTime, zvkInteger, zvkFloat]) then
      Error(szrIllegalDate);
    case Operator of
      zopPlus  : Result:= TDateTime(Left) + TDateTime(Right);
      zopMinus : Result:= TDateTime(Left) - TDateTime(Right);
      else       Result:= Unassigned;
    end;
  end;

begin
  Result:= Unassigned;
  Left  := fLeft .Value;
  Right := fRight.Value;
  LeftKind  := ValueKind(Left );
  RightKind := ValueKind(Right);
  if (LeftKind = zvkError) or (RightKind = zvkError) then Exit;
  if (LeftKind = zvkNull) and (RightKind <> zvkNull) then Left  := ConvertNull(RightKind);
  if (RightKind = zvkNull) and (LeftKind <> zvkNull) then Right := ConvertNull(LeftKind);
  case Operator of
    zopLess          : Result:= Left < Right;
    zopLessOrEqual   : Result:= Left <= Right;
    zopGreater       : Result:= Left > Right;
    zopGreaterOrEqual: Result:= Left >= Right;
    zopEqual         : Result:= Left = Right;
    zopUnequal       : Result:= Left <> Right;
    zopLogicalOr     : if (LeftKind  = zvkBoolean) and
                          (RightKind = zvkBoolean) then
                         Result:= Boolean(Left) or Boolean(Right)
                       else
                         Error(szrIllegalLogics);
    zopLogicalAnd    : if (LeftKind  = zvkBoolean) and
                          (RightKind = zvkBoolean) then
                         Result:= Boolean(Left) and Boolean(Right)
                       else
                         Error(szrIllegalLogics);
    zopPlus          : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Result:= DoDates
                       else
                         Result:= Left + Right;
    zopMinus         : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Result:= DoDates
                       else
                         Result:= Left - Right;
    zopMul           : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Error(szrIllegalDate)
                       else
                         Result:= Left * Right;
    zopDiv           : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Error(szrIllegalDate)
                       else
                         Result:= Left / Right;
    zopMod           : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Error(szrIllegalDate)
                       else
                         Result:= Left mod Right;
    zopPower         : if (LeftKind = zvkDateTime) or
                          (RightKind = zvkDateTime) then
                         Error(szrIllegalDate)
                       else
                         Result:= Power(Left, Right);
    zopBitwiseOr     : if (LeftKind  = zvkInteger) and
                          (RightKind = zvkInteger) then
                         Result:= Integer(Left) or Integer(Right)
                       else
                         Error(szrIllegalBitwise);
    zopBitwiseAnd    : if (LeftKind  = zvkInteger) and
                          (RightKind = zvkInteger) then
                         Result:= Integer(Left) and Integer(Right)
                       else
                         Error(szrIllegalBitwise);
    zopBitwiseXor    : if (LeftKind  = zvkInteger) and
                          (RightKind = zvkInteger) then
                         Result:= Integer(Left) xor Integer(Right)
                       else
                         Error(szrIllegalBitwise);
    zopShl           : if (LeftKind  = zvkInteger) and
                          (RightKind = zvkInteger) then
                         Result:= Integer(Left) shl Integer(Right)
                       else
                         Error(szrIllegalBitwise);
    zopShr           : if (LeftKind  = zvkInteger) and
                          (RightKind = zvkInteger) then
                         Result:= Integer(Left) shr Integer(Right)
                       else
                         Error(szrIllegalBitwise);
    zopAssign        : begin
                         fLeft.Value:= Right;
                         Result := Right;
                       end;
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZREvaluator                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZREvaluator.Create(AOwner: TComponent);
begin
  inherited;
  Collection:= ZRCollection;
end;

destructor TZREvaluator.Destroy;
begin
  if Assigned(fElement) then fElement.Free;
  inherited;
end;

procedure TZREvaluator.SetExpression(const Value: String);
begin
  if fExpression <> Value then begin
    if Assigned(fElement) then begin
      fElement.Free;
      fElement:= nil;
    end;
    fExpression := Trim(Value);
    fErrorMsg   := Format(LoadStr(szrExpressionParse), [Expression]);
  end;
end;

function TZREvaluator.GetValue: Variant;
begin
  if not Assigned(fElement) then
    Result:= fErrorMsg
  else try
    Result:= fElement.Value;
  except
    on E: Exception do begin
      fErrorMsg:= Format(LoadStr(szrCalculationError), [Expression, E.Message]);
      Result   := fErrorMsg;
    end;
  end;
end;

function TZREvaluator.GetString: String;
var
  v: Variant;
begin
  v:= Value;
  case ValueKind(v) of
    zvkError  : Result:= Expression;
    zvkBoolean: if v then
                  Result:= LoadStr(szrBooleanTrue)
                else
                  Result:= LoadStr(szrBooleanFalse);
    else        Result:= v;
  end;
end;

function TZREvaluator.Parse: Boolean;

  procedure SetError(const ID: Integer; const Args: array of const);
  begin
    fErrorMsg:= Format(LoadStr(ID), Args);
  end;

  type
    TOperatorMap = record
      S: String[3];
      O: TZROperator;
    end;
  const
    OperatorMap: array[0..25] of TOperatorMap = (
      (S:'<<' ;   O:zopShl           ),
      (S:'SHL';   O:zopShl           ),
      (S:'>>' ;   O:zopShr           ),
      (S:'SHR';   O:zopShr           ),
      (S:'<>' ;   O:zopUnequal       ),
      (S:'!=' ;   O:zopUnequal       ),
      (S:'<=' ;   O:zopLessOrEqual   ),
      (S:'<'  ;   O:zopLess          ),
      (S:'>=' ;   O:zopGreaterOrEqual),
      (S:'>'  ;   O:zopGreater       ),
      (S:'==' ;   O:zopEqual         ),
      (S:'='  ;   O:zopEqual         ),
      (S:'+'  ;   O:zopPlus          ),
      (S:'-'  ;   O:zopMinus         ),
      (S:'OR' ;   O:zopLogicalOr     ),
      (S:'||' ;   O:zopLogicalOr     ),
      (S:'AND';   O:zopLogicalAnd    ),
      (S:'&&' ;   O:zopLogicalAnd    ),
      (S:'|'  ;   O:zopBitwiseOr     ),
      (S:'&'  ;   O:zopBitwiseAnd    ),
      (S:'XOR';   O:zopBitwiseXor    ),
      (S:'*'  ;   O:zopMul           ),
      (S:'/'  ;   O:zopDiv           ),
      (S:'%'  ;   O:zopMod           ),
      (S:'^'  ;   O:zopPower         ),
      (S:':=' ;   O:zopAssign        )
    );

  type
    TZRToken = Char;
  const
    ztEOF      = toEOF;
    ztSymbol   = toSymbol;
    ztString   = toString;
    ztInteger  = toInteger;
    ztFloat    = toFloat;
    ztOperator = Char(6);

  var
    LastPos    : Integer;
    CurPos     : Integer;
    Token      : String;

  function NextToken: TZRToken;
  var
    Sub : String;
    L, i: Integer;
    c   : Integer;

    procedure ScanInteger;
    begin
      if (i <= L) and (Sub[i] in ['+', '-']) then Inc(i);
      while (i <= L) and (Sub[i] in ['0'..'9']) do Inc(i);
    end;

    procedure ScanNumeric;

      procedure ScanFraction;
      begin
        if (i < L) and (Sub[i] = '.') then begin
          Result:= ztFloat;
          Inc(i);
          ScanInteger;
        end;
      end;

    begin
      Result:= ztInteger;
      i:= 1;
      ScanInteger;
      if (i <= L) then begin
        ScanFraction;
        if (i <= L) and (Sub[i] = 'E') then begin
          Result:= ztFloat;
          Inc(i);
          ScanInteger;
          ScanFraction;
        end;
      end;
    end;

  begin
    LastPos:= CurPos;
    Token  := '';

    while (CurPos <= length(Expression)) and (Expression[CurPos] = ' ') do Inc(CurPos);
    Sub := copy(Expression, CurPos, length(Expression));
    L   := length(Sub);

    Result:= ztEOF;
    if L = 0 then Exit;

    for i:= Low(OperatorMap) to High(OperatorMap) do
      if not (OperatorMap[i].S[1] in ['A'..'Z']) and
         (AnsiUpperCase(copy(Sub,1,length(OperatorMap[i].S))) = OperatorMap[i].S) then begin
        Result := ztOperator;
        Token  := OperatorMap[i].S;
        Inc(CurPos, length(OperatorMap[i].S));
        Exit;
      end;

    if Result = ztEOF then
      case Sub[1] of
        'A'..'Z',
        'a'..'z',
        'À'..'ß',
        'à'..'ÿ': begin
                    i:= 2;
                    while (i<=L) and (Sub[i] in ['A'..'Z','a'..'z','À'..'ß','à'..'ÿ','0'..'9','_','.']) do Inc(i);
                    Token := copy(Sub, 1, i-1);
                    Inc(CurPos, i-1);
                    Result:= ztSymbol;
                    for i:= Low(OperatorMap) to High(OperatorMap) do
                      if (OperatorMap[i].S[1] in ['A'..'Z']) and
                         (AnsiUpperCase(Token) = OperatorMap[i].S) then begin
                        Result := ztOperator;
                        Token  := OperatorMap[i].S;
                      end;
                  end;
        '0'..'9': begin
                    ScanNumeric;
                    Inc(CurPos, i-1);
                    Token := copy(Sub, 1, i-1);
                  end;
        '#','''': begin
                    Result:= ztString;
                    i:= 1;
                    while (i <= L) do begin
                      case Sub[i] of
                        '#' : begin
                                Inc(i);    c:= i;
                                ScanInteger;
                                c:= StrToIntDef(copy(Sub, c, i-c), -1);
                                if (c > 0) and (c < 255) then
                                  AppendStr(Token, Char(c))
                                else
                                  raise EZREvaluatorError.CreateResFmt(szrInvalidString, [Sub]);
                              end;
                        '''': begin
                                c:= System.Pos('''', copy(Sub,i+1,L));
                                if c > 0 then begin
                                  AppendStr(Token, copy(Sub, i+1, c-1));
                                  Inc(i, c+1);
                                end else
                                  raise EZREvaluatorError.CreateResFmt(szrInvalidString, [Sub]);
                              end;
                        else  break;
                      end;
                      Inc(CurPos, i-1);
                    end;
                  end;
      end;

    if Result = ztEOF then begin

      Result := Sub[1];
      Token  := Sub[1];
      Inc(CurPos);
    end;

  end;

  procedure ReturnToken;
  begin
    CurPos:= LastPos;
  end;

  type TZROperatorKind = (zokAssign, zokCompare, zokLogical, zokAdd, zokMultiply, zokPower, zokBitwise, zokShift);

  function ParseExpression(AcceptUnary: Boolean): TZRElement; forward;
  function ParseOperator(Kind: TZROperatorKind; AcceptUnary: Boolean): TZRElement; forward;

  function ParseTerm(AcceptUnary: Boolean): TZRElement;

    function FindEntry(Name: String; AcceptFunctions: Boolean): TZRElement;
    var
      aLibrary : TZRElementLibrary;
      aIndex   : Integer;
    begin
      Name := AnsiUpperCase(Name);
      if Collection.FindEntry(Name, aLibrary, aIndex) and (
         AcceptFunctions or (aLibrary is TZRVariableLibrary) ) then
        Result := TZRElementFunction.Create(Name, aLibrary, aIndex)
      else
        Result := nil;
    end;

    function ParseNot: TZRElement;
    begin
      Result := FindEntry('NOT', True);
      if Result = nil then
        SetError(szrUnknownEntry, [Token])
      else
        TZRElementFunction(Result).Arguments.Add(ParseTerm(True));
    end;

    function ParseEntry(AcceptFunctions: Boolean): TZRElement;

    var
      HasArg : Boolean;
      Arg    : TZRElement;
    begin
      Result:= FindEntry(Token, AcceptFunctions);
      if Result = nil then
        SetError(szrUnknownEntry, [Token])
      else
        if (Result is TZRElementFunction) then begin
          if (NextToken = '(') then begin
            repeat
              HasArg := not (NextToken in [')', ztEOF]);
              if HasArg then begin
                if (TZRElementFunction(Result).Arguments.Count = 0) and
                   (Token = ',') then begin
                  SetError(szrIllegalArguments, [TZRElementFunction(Result).Name]);
                  Result.Free;
                  Result:= nil;
                  break;
                end;
                if (Token <> ',') then ReturnToken;
                Arg := ParseExpression(True);
                if Arg <> nil then
                  TZRElementFunction(Result).Arguments.Add(Arg)
                else begin
                  Result.Free;
                  Result:= nil;
                  break;
                end;
              end;
            until not HasArg;

            if (Result <> nil) and (Token <> ')') then begin
              Result.Free;
              Result:= nil;
              SetError(szrMissingBracket, [')', Expression]);
            end;
          end else
            ReturnToken;
        end;
    end;

    function ParseDate: TZRElement;
    var
      S: String;
      T: TZRToken;
    begin
      Result:= nil;
      S:= '';
      repeat
        T:= NextToken;
        if not (T in [ztEOF, '}']) then AppendStr(S, Token);
      until T in [ztEOF, '}'];
      if T <> '}' then
        SetError(szrMissingBracket, ['}', Expression])
      else try
        Result:= TZRElementConstant.Create(StrToDate(S));
      except
        SetError(szrInvalidDate, [Expression])
      end;
    end;

  begin
    Result := nil;
    case NextToken of
      '('        : begin
                     Result:= ParseExpression(True);
                     if (Result <> nil) and (NextToken <> ')') then begin
                       Result.Free;
                       Result:= nil;
                       SetError(szrMissingBracket, [')', Expression]);
                     end;
                   end;
      '{'        : Result:= ParseDate;
      '['        : if NextToken <> ztSymbol then
                     SetError(szrIllegalBracket, ['[..]', Expression])
                   else begin
                     Result := ParseEntry(False);
                     if (Result <> nil) and (NextToken <> ']') then begin
                       Result.Free;
                       Result:= nil;
                       SetError(szrMissingBracket, [']', Expression]);
                     end
                   end;
      '!'        : Result:= ParseNot;
      ztSymbol   : if AnsiUpperCase(Token) = 'NOT' then
                     Result:= ParseNot
                   else
                     Result:= ParseEntry(True);
      ztString   : Result:= TZRElementConstant.Create(Token);
      ztInteger  : try
                     Result:= TZRElementConstant.Create(StrToInt(Token));
                   except
                     SetError(szrInvalidNumeric, [Token]);
                   end;
      ztFloat    : try
                     if DecimalSeparator <> '.' then
                       Token:= StringReplace(Token, '.', DecimalSeparator, [rfReplaceAll]);
                     Result:= TZRElementConstant.Create(StrToFloat(Token));
                   except
                     SetError(szrInvalidNumeric, [Token]);
                   end;
      ztOperator : if AcceptUnary then begin
                     if Token = '+' then
                       Result:= ParseOperator(zokAdd, False) else
                     if Token = '-' then begin
                       Result:= ParseOperator(zokAdd, False);
                       if Result <> nil then
                         Result:= TZRElementOperator.Create(zopMinus,
                                  TZRElementConstant.Create(0),
                                  Result);
                    end else
                       SetError(szrIllegalUnary, [Token, Expression]);
                   end else
                     SetError(szrIllegalUnary, [Token, Expression]);
      ztEOF      : Exit;
      else         SetError(szrExpressionError, [Expression]);
    end;
  end;

  const
    zokFirst = zokAssign;
    zokLast  = zokShift;
  const
    KindMap: array[TZROperator] of TZROperatorKind = (
      zokCompare,    // zopLess
      zokCompare,    // zopLessOrEqual
      zokCompare,    // zopGreater
      zokCompare,    // zopGreaterOrEqual
      zokCompare,    // zopEqual
      zokCompare,    // zopUnequal
      zokLogical,    // zopLogicalOr
      zokLogical,    // zopLogicalAnd
      zokAdd    ,    // zopPlus
      zokAdd    ,    // zopMinus
      zokMultiply,   // zopMul
      zokMultiply,   // zopDiv
      zokMultiply,   // zopMod
      zokPower,      // zopPower
      zokBitwise,    // zopBitwiseOr
      zokBitwise,    // zopBitwiseAnd
      zokBitwise,    // zopBitwiseXor
      zokShift,      // zopShl
      zokShift,      // zopShr
      zokAssign     // zopAssign
    );

  function ParseOperator(Kind: TZROperatorKind; AcceptUnary: Boolean): TZRElement;

  var
    Operator: TZROperator;

    function FindOperator: Boolean;
    var
      i: Integer;
    begin
      Operator:= zopEqual;
      Result  := (NextToken = ztOperator);
      if Result then begin
        for i:= Low(OperatorMap) to High(OperatorMap) do
          if (AnsiUpperCase(Token) = OperatorMap[i].S) and
             (KindMap[OperatorMap[i].O] = Kind) then begin
            Operator := OperatorMap[i].O;
            Result   := True;
            Exit;
          end;
      end;
      ReturnToken;
      Result:= False;
    end;

    function ParseRightOperand(Left: TZRElement): TZRElement;
    var
      Right: TZRElement;
    begin
      Result:= Left;
      if (Left <> nil) and FindOperator then begin
        if Kind = zokLast then
          Right:= ParseTerm(False)
        else
          Right:= ParseOperator(Succ(Kind), False);
        if Right <> nil then begin
          Result:= TZRElementOperator.Create(Operator, Left, Right);
          Result:= ParseRightOperand(Result);
        end else begin
          Result.Free;
          Result:= nil;
        end;
      end;
    end;

  begin
    if Kind = zokLast then
      Result:= ParseTerm(AcceptUnary)
    else
      Result:= ParseOperator(Succ(Kind), AcceptUnary);
    Result:= ParseRightOperand(Result);
  end;

  function ParseExpression(AcceptUnary: Boolean): TZRElement;
  begin
    Result:= ParseOperator(zokFirst, AcceptUnary);
  end;

begin
  LastPos   := 1;
  CurPos    := 1;
  fErrorMsg := '';
  fElement  := ParseExpression(True);
  if (fElement <> nil) and (CurPos <= length(Expression)) then begin
    fElement.Free;
    fElement:= nil;
    SetError(szrExpressionError, [Expression]);
  end;
  Result := fElement <> nil;
end;

{ TZRElement }

function TZRElement.GetValue: Variant;
begin
  Result:= Unassigned;
end;

procedure TZRElement.SetValue(Value: Variant);
begin
end;

{ TZRElementConstant }

constructor TZRElementConstant.Create(aValue: Variant);
begin
  inherited Create;
  fValue := aValue;
end;

function TZRElementConstant.GetValue: Variant;
begin
  Result := fValue;
end;

{ TZRElementFunction }

constructor TZRElementFunction.Create(aName: String; aLibrary: TZRElementLibrary; aIndex: Integer);
begin
  inherited Create;
  fArguments:= TList.Create;
  fName    := aName;
  fLibrary := aLibrary;
  fIndex   := aIndex;
end;

destructor TZRElementFunction.Destroy;
begin
  fArguments.Free;
  inherited;
end;

function TZRElementFunction.GetValue: Variant;
begin
  if (fLibrary <> nil) and (fIndex >= 0) and (fIndex < fLibrary.Count) then
    Result := fLibrary.GetValue(fIndex, fArguments)
  else
    Result := Unassigned;
end;

procedure TZRElementFunction.SetValue(Value: Variant);
begin
  if (fLibrary <> nil) and (fIndex >= 0) and (fIndex < fLibrary.Count) then
    fLibrary.SetValue(fIndex, Value);
end;

{ TZRElementLibrary }

constructor TZRElementLibrary.Create(aOwner: TZRElementCollection);
begin
  inherited Create;
  fOwner := aOwner;
end;

function TZRElementLibrary.IndexOf(const Name: String): Integer;
var
  i : Integer;
  S : String;
begin
  Result := -1;   i := 0;
  S := AnsiUpperCase(Name);
  while (i < Count) and (Result < 0) do begin
    if AnsiUpperCase(Items[i]) = S then Result := i;
    Inc(i);
  end;
end;

function TZRElementLibrary.GetValue(Index: Integer; Arguments: TArguments): Variant;
begin
  if Check(Index, Arguments) then
    try
      Result := Calculate(Index, Arguments)
    except
      Result := Unassigned;
    end
  else
    raise EZREvaluatorError.CreateResFmt(szrIllegalArguments, [Items[Index]]);
end;

function TZRElementLibrary.Check(Index: Integer; Arguments: TArguments): Boolean;
begin
  Result := True;
end;

{ TZRVariableLibrary }

function TZRVariableLibrary.Check(Index: Integer; Arguments: TArguments): Boolean;
begin
  Result := (Arguments.Count = 0);
end;

{ TZRElementCollection }

destructor TZRElementCollection.Destroy;
begin
  while Count > 0 do begin
    Objects[Count-1].Free;
    Delete(Count-1);
  end;
  inherited;
end;

function TZRElementCollection.RegisterLibrary(aClass: TZRElementLibraryClass; aName, aDescription, aVendor: String): TZRElementLibrary;
begin
  Result := aClass.Create(Self);
  Result.Name        := aName;
  Result.Description := aDescription;
  Result.Vendor      := aVendor;
  AddObject(Result.Name, Result);
end;

function TZRElementCollection.InternalFindEntry(const Name: String;
  var aLibrary: TZRElementLibrary; var aIndex: Integer): Boolean;
var
  i : Integer;
begin
  Result := False;  i := 0;
  while (i < Count) and not Result do begin
    aIndex := TZRElementLibrary(Objects[i]).IndexOf(Name);
    if aIndex >= 0 then begin
      aLibrary := TZRElementLibrary(Objects[i]);
      Result   := True;
    end;
    Inc(i);
  end;
  if not Result then begin
    aLibrary := nil;
    aIndex   := -1;
  end;
end;

function TZRElementCollection.FindEntry(const Name: String;
  var aLibrary: TZRElementLibrary; var aIndex: Integer): Boolean;
begin
  Result := InternalFindEntry(Name, aLibrary, aIndex);
  if not Result then begin
    if Self <> ZRCollection then
      Result := ZRCollection.FindEntry(Name, aLibrary, aIndex)
    else begin
      aLibrary := nil;
      aIndex   := -1;
    end;
  end;
end;

{$I ZRLibs.inc}

initialization
  ZRCollection := TZRElementCollection.Create;
  with ZRCollection do begin
    RegisterLibrary(TZRGeneralLibrary , 'General'    , 'General functions'     , szrVendorZYZ);
    RegisterLibrary(TZRMathLibrary    , 'Mathematics', 'Mathematical functions', szrVendorZYZ);
    RegisterLibrary(TZRStringLibrary  , 'String'     , 'String functions'      , szrVendorZYZ);
    RegisterLibrary(TZRDateTimeLibrary, 'Date/Time'  , 'Date - time functions' , szrVendorZYZ);
    RegisterLibrary(TZRConstantLibrary, 'Constants'  , 'Constants'             , szrVendorZYZ);
  end;

finalization
  ZRCollection.Free;
  ZRCollection := nil;
end.

