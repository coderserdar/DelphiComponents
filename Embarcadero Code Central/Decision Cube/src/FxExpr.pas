{=======================================================================}
{                                                                       }
{             Evgueni Koukline Visual Component Library                 }
{                 Decision Cube - VCL Edition                           }
{             Copyright (c) 2002-2003 Evgueni Koukline.                 }
{                     ALL RIGHTS RESERVED                               }
{                                                                       }
{   The entire contents of this file is protected by International      }
{   Copyright Laws. Unauthorized reproduction, reverse-engineering,     }
{   and distribution of all or any portion of the code contained in     }
{   this file is strictly prohibited and may result in severe civil     }
{   and criminal penalties and will be prosecuted to the maximum        }
{   extent possible under the law.                                      }
{                                                                       }
{  RESTRICTIONS                                                         }
{                                                                       }
{  THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES (DCU, OBJ,     }
{  DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE SECRETS   }
{  OF EVGUENI KOUKLINE. THE REGISTERED DEVELOPER IS LICENSED TO         }
{  DISTRIBUTE THE DECISION CUBE AS PART OF AN EXECUTABLE PROGRAM ONLY.  }
{                                                                       }
{  THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED FILES OR  }
{  ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE COPIED, TRANSFERRED, }
{  SOLD, DISTRIBUTED, OR OTHERWISE MADE AVAILABLE TO OTHER INDIVIDUALS  }
{  WITHOUT EXPRESS WRITTEN CONSENT AND PERMISSION FROM EVGUENI KOUKLINE.}
{                                                                       }
{     CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON         }
{     ADDITIONAL RESTRICTIONS.                                          }
{                                                                       }
{=======================================================================}

unit FxExpr;

interface

uses Classes, FxCommon, FxCache, Variants;

resourcestring
  SUnknownCharacter= 'Unknown character: %s';
  STooLongNumber='Too Long Number';
  SInvalidFloat='''%s'' - Invalid Float';
  SInvalidInt  ='''%s'' - Invalid Integer';
  SUnterminatedString='Unterminated String';
  SExpected='''%s'' expected but ''%s'' found';
  SIdentRedeclared='Identifier redeclared: ''%s''';
  SUndefiniteIdent='Undeclared identifier ''%s''';
  SSyntaxError='Syntaxis error';

const
  BUFSIZE=255;

type

  TMetaToken=(
    mtNone,mtMult,mtAdd,mtComp,mtSemi,
    mtIdent,mtNot,mtAnd,mtRBrack,mtEof,
    mtRPar,mtRange,mtPoint
  );

  TToken=(
    tkIDENT,tkInteg,tkReal,tkString,
    tkLPar,tkRPar,tkLBrack,tkRBrack,
    tkStar,tkSlash,tkDiv,tkMod,
    tkPlus,tkMinus,
    tkEQ,tkNE,tkLT,tkLE,tkGT,tkGE,
    tkAND,tkOR,tkNOT,
    tkRange,tkPoint,tkIn,
    tkColon,tkSemi,tkComma,
    tkEof
  );

const
  Meta:array [TToken] of TMetaToken=(
    mtIdent,mtNone,mtNone,mtNone,
    mtNone,mtRPar,mtNone,mtRBrack,
    mtMult,mtMult,mtMult,mtMult,
    mtAdd,mtAdd,
    mtComp,mtComp,mtComp,mtComp,mtComp,mtComp,
    mtAnd,mtAnd,mtNot,
    mtRange,mtPoint,mtNone,
    mtNone,mtSemi,mtNone,
    mtEof
  );

type
  TMetaSet=set of TMetaToken;

  TTokenVal=record
  case TToken of
    tkInteg :(i:Integer);
    tkReal  :(r:Extended);
    tkIdent,
    tkString:(s:PChar);
  end;

  TParser=class
  private
    FBuf:array [0..BUFSIZE] of Char;
    FDataStore:TCommonDataStore;
    FEscape:TMetaSet;
    FToken:TToken;
    FLine :Integer;
    FSource:string;
    FSrcName:string;
    FVal :TTokenVal;
    currp:PChar;
    procedure Identifier(ch:Char);
    procedure Number(ch:Char);
    procedure QString;
    procedure SetSource(const Value:string);
    function GetLog: TFxLog;
  protected
    procedure CheckNext(Value:TToken);
    procedure Error(const Value:string);
    procedure ErrorFmt(const Fmt:string; Values:array of const);
    procedure ErrorWith(ALine:Integer; const Value:string);
    procedure Skip(const Value:string);
    property Escape:TMetaSet read FEscape write FEscape;
    property Val:TTokenVal read FVal;
  public
    constructor Create(const ADataStore:TCommonDataStore);
    function Delimiter(Value:TToken):Boolean;
    procedure Next;
    property DataStore:TCommonDataStore read FDataStore;
    property Line:Integer read FLine;
    property Log:TFxLog read GetLog;
    property Source:string read FSource write SetSource;
    property SrcName:string read FSrcName write FSrcName;
    property Token:TToken read FToken;
  end;

  TUseList=class;
  TIdent=PChar;

  TSymbolTable=class(TSymbols)
  private
    FPoint :Integer;
    FLog   :TFxLog;
  protected
    procedure Insert(const Symbol:TSymbol);
    function Find(const ID:TIdent):TSymbol;
  public
    constructor Create(const ALog:TFxLog);
    procedure Declare(const ASym:TSymbol);
  end;

  TScope=class(TSymbolTable)
  private
    FParent:TScope;
    FUses:TUseList;
  public
    constructor Create(const ALog:TFxLog);
    function Lookup(const ID:TIdent):TSymbol;//overload;
    procedure AddUses(const Value:TScope);
  end;

  TSystemUnit=class(TSymbols)
  private
    FScope:TScope;
  public
    constructor Create(const ALog:TFxLog);
    destructor Destroy;override;
    property Scope:TScope read FScope;
  end;

  TUseList=class(TList)
  private
    function GetItems(const Index: Integer): TScope;
    procedure SetItems(const Index: Integer; const Value: TScope);
  public
    property Items[const Index:Integer]:TScope read GetItems write SetItems;default;
  end;

  TExprParser=class(TParser)
  private
    FScope:TScope;
    function Call(E:TExpr):TExpr;
    function Composition:TExpr;
    function Designator:TExpr;
    function Factor:TExpr;
    function Primary:TExpr;
    function Relation:TExpr;
    function Term:TExpr;
  public
    function Expr:TExpr;
    property Scope:TScope read FScope write FScope;
  end;

  TDimSymbol=class(TSymbol)
  private
    FDim:TFxAbstractMapItem;
  protected
    function GetActive:Boolean;override;
  public
    function Eval(const Off:Integer):Variant;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
    property Dim:TFxAbstractMapItem read FDim write FDim;
  end;

  TVarSymbol=class(TSymbol)
  protected
    FInit:TExpr;
    FVal :Variant;
  public
    destructor Destroy;override;
    function Eval(const Off:Integer):Variant;override;
    procedure Assign(const Value:Variant);override;
    procedure Initialize(const Off:Integer);
    property Val:Variant read FVal write FVal;
  end;

  TParamSymbol=class(TVarSymbol)
  end;

  TParamList=class(TSymbols)
  private
    function GetItems(Index:Integer):TParamSymbol;
  public
    property Items[Index:Integer]:TParamSymbol read GetItems;default;
  end;

  TProcVariant=class(TCustomVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo
      (var Dest:TVarData; const Source:TVarData; const AVarType:TVarType);override;
    procedure Clear(var V:TVarData);override;
    procedure Copy
      (var Dest:TVarData; const Source:TVarData; const Indirect:Boolean);override;
  end;

  TProcSymbol=class(TSymbol)
  private
    FParams:TParamList;
    FID:Integer;
  protected
    function GetActive:Boolean;override;
  public
    //Body  :TStmts;
    Value:Variant;
    constructor Create(const AName:PChar; const AID:Integer);
    destructor Destroy;override;
    function Eval(const Off:Integer):Variant;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
    procedure Assign(const Value:Variant);override;
    procedure Call(const Args:TExprs; const Off:Integer);
    property Params:TParamList read FParams;
  end;

  TProcVarData=packed record
    VType:TVarType;
    Reserver1,Reserver2:Word;
    ID   :Word;        // если ID<>0 это встроенная процедура
    VObj :Pointer;
    VProc:TSymbol;
  end;

function RegisterName(const AName:string):TIdent;
function VarIsProc(const AValue:Variant):Boolean;
function VarProcCreate(Obj,Proc:Pointer):Variant;
function VarProc:TVarType;

var
   SystemUnit:TSystemUnit;

implementation

uses SysUtils, FxMap, Math;

type

  PPair=^TPair;
  TPair=record
    Key  :TIdent;
    Value:TToken;
  end;

  TPairArray=array of TPair;

  TDictionary=class
  private
    head :Pointer;
    currp:PChar;
    count:Integer;
    FSize :Integer;
    FMask :Integer;
    // Статистика
    FN_Fault:Integer;
    FN_Try :Integer;
    FN_Fill:Integer;
    FTable:TPairArray;
    class function StringHash(const Str:PChar):Cardinal;
    function Alloc(const Str:PChar):PChar;
    function IndexOf(const Str:PChar):Cardinal;
    function GetItems(const Index:Integer):PPair;
    procedure Grow;
    procedure ReservedWords;
    procedure SetSize(const Value:Integer);
  public
    constructor Create(const ASize:Integer);
    destructor Destroy;override;
    function Add(const Word:PChar):Integer;
    function RegIdent(const Word:PChar):PChar;
    procedure KeyWord(const AWord:PChar; const Token:TToken);
    property N_Try:Integer read FN_Try;
    property N_Fault:Integer read FN_Fault;
    property N_Fill:Integer read FN_Fill;
    property Size:Integer read FSize write SetSize;
    property Items[const Index:Integer]:PPair read GetItems;default;
  end;

  TErrorSymbol=class(TSymbol)
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TUndefSymbol=class(TErrorSymbol)
  public
    constructor Create(const Parser:TParser);
  end;

  TExprList=class(TExprs)
  public
    constructor Create(Parser:TExprParser);
  end;

  TFooExpr=class(TExpr)
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  end;

  TErrorExpr=class(TFooExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TBaseCallExpr=class(TExpr)
  protected
    FParams:TExprList;
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    constructor Create(const Parser:TExprParser);
    destructor Destroy;override;
  end;

  TBuiltInCallExpr=class(TBaseCallExpr)
  private
    FID:Integer;
  public
    constructor Create(const Parser:TExprParser; const AID:Integer);
    function Eval(const Off:Integer):Variant;override;
  end;

  TExternCallExpr=class(TBaseCallExpr)
  private
    FID:Integer;
    FCube:TCommonDataStore;
  public
    constructor Create(const Parser:TExprParser; const AID:Integer);
    function Eval(const Off:Integer):Variant;override;
  end;

  TConstCallExpr=class(TBaseCallExpr)
  private
    FProc:TProcSymbol;
  public
    constructor Create(const Parser:TExprParser; const AProc:TProcSymbol);
    function Eval(const Off:Integer):Variant;override;
  end;

  TCallExpr=class(TBaseCallExpr)
  private
    FExpr:TExpr;
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    constructor Create(const Parser:TExprParser; const ACall:TExpr);
    destructor Destroy;override;
    function Eval(const Off:Integer):Variant;override;
  end;

  TNameExpr=class(TExpr)
  private
    FID:PChar;
    FSymbol:TSymbol;
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    constructor Create(const Parser:TExprParser);
    function Eval(const Off:Integer):Variant;override;
    property ID:PChar read FID;
  end;

  TIntegerExpr=class(TFooExpr)
  private
    FVal:Integer;
  public
    constructor Create(const Parser:TParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TRealExpr=class(TFooExpr)
  private
    FVal:Double;
  public
    constructor Create(const Parser:TParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TStringExpr=class(TFooExpr)
  private
    FVal:string;
  public
    constructor Create(const Parser:TParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TUnaryExpr=class(TExpr)
  private
    FExpr:TExpr;
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    constructor Create(const Parser:TParser);
    destructor Destroy;override;
    property Expr:TExpr read FExpr;
  end;

  TNopExpr=class(TUnaryExpr)
  public
    constructor Create(const Parser:TExprParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TNotExpr=class(TUnaryExpr)
  public
    constructor Create(const Parser: TExprParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TUnPlus=class(TUnaryExpr)
  public
    constructor Create(const Parser:TExprParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TUnMinus=class(TUnaryExpr)
  public
    constructor Create(const Parser:TExprParser);
    function Eval(const Off:Integer):Variant;override;
  end;

  TBiExpr=class(TExpr)
  private
    FLeft,FRight:TExpr;
  protected
    function GetActive:Boolean;override;
    function TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;override;
  public
    constructor Create(const Parser:TParser; const ALeft:TExpr);
    destructor Destroy;override;
    property Left:TExpr read FLeft;
    property Right:TExpr read FRight;
  end;

  TAndExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TOrExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TMultExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TDivExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TDivideExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TModExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TAddExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TSubExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
    function Eval(const Off:Integer):Variant;override;
  end;

  TCmpExpr=class(TBiExpr)
  public
    constructor Create(const Parser:TExprParser; const ALeft:TExpr);
  end;

  TEQExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TNEExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TLTExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TLEExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TGTExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TGEExpr=class(TCmpExpr)
  public
    function Eval(const Off:Integer):Variant;override;
  end;

  TCharSet=set of Char;

const
  AlphaNum:TCharSet=
    ['A'..'Z','a'..'z','0'..'9','_'];

  TokenText:array [TToken] of string=(
    'Identifier','Integer Constant','Real Constant','String',
    '(',')','[',']',
    '*','/','div','mod',
    '+','-',
    '=','<>','<','<=','>','>=',
    'and','or','not',
    '..','.','in',
    ':',';',',',
    'EOF'
  );

var
  Dict:TDictionary;

function RegisterName(const AName:string):TIdent;
begin
  Result:=Dict[Dict.Add(PChar(AName))].Key;
end;

{ TParser }

procedure TParser.CheckNext(Value: TToken);
begin
  if FToken=Value then
    Next
  else
    ErrorFmt(SExpected,[TokenText[Value],TokenText[Token]]);
end;

constructor TParser.Create(const ADataStore:TCommonDataStore);
begin
  inherited Create;
  FDataStore:=ADataStore;
  FEscape:=[mtEof];
end;

function TParser.Delimiter(Value: TToken): Boolean;
begin
  Error('Missing: "'+TokenText[Value]+'"');
  Result:=False;
end;

procedure TParser.Error(const Value: string);
begin
  Log.Error(Format('%s[%d]: %s',[FSrcName,FLine,Value]));
end;

procedure TParser.ErrorFmt(const Fmt: string; Values: array of const);
begin
  Error(Format(Fmt,Values));
end;

procedure TParser.ErrorWith(ALine: Integer; const Value: string);
begin
  Log.Error(Format('%s[%d]: %s',[FSrcName,ALine,Value]));
end;

function TParser.GetLog: TFxLog;
begin
  Result:=FDataStore.Log;
end;

procedure TParser.Identifier(ch: Char);
var
  I:Integer;
begin
  I:=0;
  repeat
    if I<BUFSIZE then FBuf[I]:=ch;
    Inc(I);
    ch:=currp^;Inc(currp);
  until not (ch in AlphaNum);
  Dec(currp);
  // идентификаторы различаются только по первым BUFSIZE сиволам
  if I>BUFSIZE then I:=BUFSIZE;
  FBuf[I]:=#0;
  I:=Dict.Add(FBuf);
  FToken:=dict[I].Value;
  FVal.s:=dict[I].Key;
end;

procedure TParser.Next;
var
  ch:Char;
label
  White;
begin
White:
  ch:=currp^;Inc(currp);
  case ch of
  'A'..'Z','a'..'z','_':
    Identifier(ch);
  '0'..'9':
    Number(ch);
  '''':
    QString;
  '(':
    FToken:=tkLPar;
  ')':
    FToken:=tkRPar;
  '[':
    FToken:=tkLBrack;
  ']':
    FToken:=tkRBrack;
  ';':
    FToken:=tkSemi;
  '.':
    if currp^<>'.' then
      FToken:=tkPoint
    else begin
      FToken:=tkRange;
      Inc(currp);
    end;
  ',':
    FToken:=tkComma;
  ':':
    FToken:=tkColon;
  '*':
    FToken:=tkStar;
  '/':
    FToken:=tkSlash;
  '+':
    FToken:=tkPlus;
  '-':
    FToken:=tkMinus;
  '>':if currp^<>'=' then
        FToken:=tkGT
      else begin
        FToken:=tkGE;
        Inc(currp);
      end;
  '<':if currp^ = '>' then begin
        FToken:=tkNE;
        Inc(currp);
      end else if currp^ = '=' then begin
        FToken:=tkLE;
        Inc(currp);
      end else begin
        FToken:=tkLT;
      end;
  '=':FToken:=tkEQ;
  ' ',#9,#10:
    goto White;
  #13:
    begin Inc(FLine);goto White end;
  #0:
    FToken:=tkEof;
  else
    ErrorFmt(SUnknownCharacter,[ch]);
  end;
end;

procedure TParser.Number(ch: Char);
var
  I:Integer;
begin
  FToken:=tkInteg;
  I:=0;
  repeat
    if I<BUFSIZE then FBuf[I]:=ch;
    Inc(I);
    ch:=currp^;Inc(currp);
  until not (ch in ['0'..'9']);
  if (ch='.') and (currp^<>'.') then begin
    FToken:=tkReal;
    ch:=DecimalSeparator;
    repeat
     if I<BUFSIZE then FBuf[I]:=ch;
     Inc(I);
     ch:=currp^;Inc(currp);
    until not (ch in ['0'..'9']);
  end;
  // Если выше в любой их проверки было невыполнено
  // I<BUFSIZE => следовательно после последующего инкремента
  // буде верно I>BUFSIZE
  if I>BUFSIZE then begin
    I:=BUFSIZE;
    Error(STooLongNumber);
  end;
  FBuf[I]:=#0;
  Dec(currp);
{ TODO : Необходимо выяснить влияние $R+ на работу Val }
  if Token=tkInteg then begin
    System.Val(FBuf,FVal.i,I);
    if I<>0 then ErrorFmt(SInvalidInt,[FBuf]);
  end else if not TextToFloat(FBuf,FVal.r,fvExtended)then
    ErrorFmt(SInvalidFloat, [FBuf]);
end;

procedure TParser.QString;
var
  I:Integer;
begin
  I:=0;
  while True do begin
    if currp^=#0 then begin
      Error(SUnterminatedString);
      Break;
    end else if currp^=''''then begin
      Inc(currp);
      if currp^<>'''' then Break;
    end;
    if I<BUFSIZE then FBuf[I]:=currp^;
    Inc(currp);Inc(I);
  end;
  FBuf[I]:=#0;
  FToken:=tkString;
  FVal.s:=FBuf;
end;

procedure TParser.SetSource(const Value:string);
begin
  FSource:=Value;
  FLine:=0;
  currp:=PChar(FSource);
  Next;
end;

procedure TParser.Skip(const Value: string);
begin
  Error(Value);
  while not(Meta[Token] in Escape) do Next;
end;

{ TExprParser }

function TExprParser.Call(E: TExpr): TExpr;
var
  Proc:TProcSymbol;
begin
  if (E is TNameExpr)and(TNameExpr(E).FSymbol is TProcSymbol)then
  begin
    Proc:=TProcSymbol(TNameExpr(E).FSymbol);
    if Proc.FID=0 then
      Result:=TConstCallExpr.Create(Self,Proc)
    else if Proc.FID<0 then
      Result:=TBuiltInCallExpr.Create(Self,Proc.FID)
    else
      Result:=TExternCallExpr.Create(Self,Proc.FID);
  end else
    Result:=TCallExpr.Create(Self,E);
end;

function TExprParser.Composition: TExpr;
begin
  if Token=tkPlus then
    Result:=TUnPlus.Create(Self)
  else if Token=tkMinus then
    Result:=TUnMinus.Create(Self)
  else
    Result:=Term;
  while True do begin
    case Token of
    tkPlus:  Result:=TAddExpr.Create(Self,Result);
    tkMinus: Result:=TSubExpr.Create(Self,Result);
    else
      Break;
    end;
  end;
end;

function TExprParser.Designator: TExpr;
begin
  Result:=TNameExpr.Create(Self);
  while True do begin
    case Token of
      //tkLBrack:
        //Result:=TElementExpr.Create(Parser,Result);
      tkLPar:
        Result:=Call(Result);
      //tkPoint:
        //Result:=TFieldExpr.Create(Parser,Result);
    else
      Break;
    end;
  end;
end;

function TExprParser.Expr: TExpr;
begin
  Result:=Relation;
  while True do begin
    case Token of
    tkAND:  Result:=TAndExpr.Create(Self,Result);
    tkOR:   Result:=TOrExpr.Create(Self,Result);
    else
      Break;
    end;
  end;
end;

function TExprParser.Factor: TExpr;
begin
  if Token=tkNOT then
    Result:=TNotExpr.Create(Self)
  else
    Result:=Primary;
end;

function TExprParser.Primary: TExpr;
var
  Escape:TMetaSet;
begin
  Escape:=Self.Escape;
  Self.Escape:=Self.Escape+[mtMult,mtAdd,mtAnd];
  case Token of
    tkIdent : Result:=Designator;
    tkInteg : Result:=TIntegerExpr.Create(Self);
    tkReal  : Result:=TRealExpr.Create(Self);
    tkString: Result:=TStringExpr.Create(Self);
    tkLPar  : Result:=TNopExpr.Create(Self);
//    tkLBrack: Result:=TAggrExpr.Create(Self);
  else
    Skip(SSyntaxError);
    Result:=TErrorExpr.Create;
  end;
  Self.Escape:=Escape;
end;

function TExprParser.Relation: TExpr;
begin
  Result:=Composition;
  case Token of
    tkEQ: Result:=TEQExpr.Create(Self,Result);
    tkNE: Result:=TNEExpr.Create(Self,Result);
    tkLT: Result:=TLTExpr.Create(Self,Result);
    tkLE: Result:=TLEExpr.Create(Self,Result);
    tkGE: Result:=TGEExpr.Create(Self,Result);
    tkGT: Result:=TGTExpr.Create(Self,Result);
  end;
end;

function TExprParser.Term: TExpr;
begin
  Result:=Factor;
  while True do begin
    case Token of
    tkStar:
      Result:=TMultExpr.Create(Self,Result);
    tkSlash:
      Result:=TDivideExpr.Create(Self,Result);
    tkDiv:
      Result:=TDivExpr.Create(Self,Result);
    tkMod:
      Result:=TModExpr.Create(Self,Result);
    else
      Break;
    end;
  end;
end;

{ TExprList }

constructor TExprList.Create(Parser: TExprParser);
begin
  inherited Create;
  repeat
    Add(Parser.Expr);
    while Parser.Token=tkComma do begin
      Parser.Next;
      Add(Parser.Expr);
    end;
    // token<>tkComma
  until (Meta[Parser.Token] in Parser.Escape) or Parser.Delimiter(tkComma);
end;

{ TErrorExpr }

function TErrorExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Unassigned;
end;

{ TCallExpr }

constructor TCallExpr.Create(const Parser:TExprParser; const ACall:TExpr);
begin
  inherited Create(Parser);
  FExpr:=ACall;
end;

destructor TCallExpr.Destroy;
begin
  FreeAndNil(FExpr);
  inherited;
end;

function TCallExpr.Eval(const Off: Integer): Variant;
var
  V:Variant;
  Proc:TProcSymbol;
begin
  V:=FExpr.Eval(Off);
  if not VarIsProc(V)then
    raise Exception.CreateFmt('Line %d: Invalid procedure call',[FLine]);
  Proc:=TProcVarData(V).VProc as TProcSymbol;
  Proc.Call(FParams,Off);
  Result:=Proc.Value;
  Proc.Value:=Unassigned;
end;

function TCallExpr.GetActive: Boolean;
begin
  Result:=FExpr.Active and inherited Active;
end;

function TCallExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:=FExpr.TryLoad(Range,Dims,Sums) and inherited TryLoad(Range,Dims,Sums);
end;

{ TNameExpr }

constructor TNameExpr.Create(const Parser: TExprParser);
begin
  inherited Create;
  FID  :=Parser.Val.s;
  FLine:=Parser.Line;
  FSymbol:=Parser.Scope.Lookup(Parser.Val.s);
  if FSymbol=nil then begin
    FSymbol:=TUndefSymbol.Create(Parser);
    Parser.Scope.Declare(FSymbol);
  end;
  Parser.Next;// Identifier
end;

function TNameExpr.Eval(const Off: Integer): Variant;
begin
  Result:=FSymbol.Eval(Off);
end;

function TNameExpr.GetActive: Boolean;
begin
  Result:=FSymbol.Active;
end;

function TNameExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:= FSymbol.TryLoad(Range,Dims,Sums);
end;

{ TIntegerExpr }

constructor TIntegerExpr.Create(const Parser: TParser);
begin
  inherited Create;
  FVal:=Parser.Val.i;
  FLine:=Parser.Line;
  Parser.Next;
end;

function TIntegerExpr.Eval(const Off: Integer): Variant;
begin
  Result:=FVal;
end;

{ TRealExpr }

constructor TRealExpr.Create(const Parser: TParser);
begin
  inherited Create;
  FVal:=Parser.Val.r;
  FLine:=Parser.Line;
  Parser.Next;
end;

function TRealExpr.Eval(const Off: Integer): Variant;
begin
  Result:=FVal;
end;

{ TStringExpr }

constructor TStringExpr.Create(const Parser: TParser);
begin
  inherited Create;
  FVal:=Parser.Val.s;
  FLine:=Parser.Line;
  Parser.Next;
end;

function TStringExpr.Eval(const Off: Integer): Variant;
begin
  Result:=FVal;
end;

{ TUnaryExpr }

constructor TUnaryExpr.Create(const Parser: TParser);
begin
  inherited Create;
  FLine:=Parser.Line;
  Parser.Next;
end;

destructor TUnaryExpr.Destroy;
begin
  FreeAndNil(FExpr);
  inherited;
end;

function TUnaryExpr.GetActive: Boolean;
begin
  Result:=Expr.Active;
end;

function TUnaryExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:= Expr.TryLoad(Range,Dims,Sums);
end;

{ TNopExpr }

constructor TNopExpr.Create(const Parser: TExprParser);
var
  Escape:TMetaSet;
begin
  inherited Create(Parser);
  Escape:=Parser.Escape;
  Include(Parser.FEscape,mtRPar);
  FExpr:=Parser.Expr;
  Parser.Escape:=Escape;
  PArser.CheckNext(tkRPar);
end;

function TNopExpr.Eval(const Off: Integer): Variant;
begin
  Result:=FExpr.Eval(Off);
end;

{ TNotExpr }

constructor TNotExpr.Create(const Parser: TExprParser);
begin
  inherited Create(Parser);
  FExpr:=Parser.Primary;
end;

function TNotExpr.Eval(const Off: Integer): Variant;
begin
  Result:= not Boolean(Expr.Eval(Off));
end;

{ TUnPlus }

constructor TUnPlus.Create(const Parser: TExprParser);
begin
  inherited Create(Parser);
  FExpr:=Parser.Term;
end;

function TUnPlus.Eval(const Off: Integer): Variant;
begin
  Result:=Expr.Eval(Off);
end;

{ TUnMinus }

constructor TUnMinus.Create(const Parser: TExprParser);
begin
  inherited Create(Parser);
  FExpr:=Parser.Term;
end;

function TUnMinus.Eval(const Off: Integer): Variant;
begin
  Result:= -Expr.Eval(Off);
end;

{ TBiExpr }

constructor TBiExpr.Create(const Parser:TParser; const ALeft:TExpr);
begin
  inherited Create;
  FLeft:=ALeft;
  FLine:=Parser.Line;
  Parser.Next;
end;

destructor TBiExpr.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRight);
  inherited;
end;

function TBiExpr.GetActive: Boolean;
begin
  Result:= Left.Active and Right.Active;
end;

function TBiExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer): Boolean;
begin
  Result:=Left.TryLoad(Range,Dims,Sums)and Right.TryLoad(Range,Dims,Sums);
end;

{ TAndExpr }

constructor TAndExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Relation;
end;

function TAndExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Boolean(Left.Eval(Off)) and Boolean(Right.Eval(Off));
end;

{ TOrExpr }

constructor TOrExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Relation;
end;

function TOrExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Boolean(Left.Eval(Off)) or Boolean(Right.Eval(Off));
end;

{ TMultExpr }

constructor TMultExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Factor;
end;

function TMultExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Left.Eval(Off)*Right.Eval(Off);
end;

{ TDivExpr }

constructor TDivExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Factor;
end;

function TDivExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Integer(Left.Eval(Off)) div Integer(Right.Eval(Off));
end;

{ TDivideExpr }

constructor TDivideExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Factor;
end;

function TDivideExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Left.Eval(Off) / Right.Eval(Off);
end;

{ TModExpr }

constructor TModExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Factor;
end;

function TModExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Integer(Left.Eval(Off)) mod Integer(Right.Eval(Off));
end;

{ TAddExpr }

constructor TAddExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Factor;
end;

function TAddExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Left.Eval(Off)+Right.Eval(Off);
end;

{ TSubExpr }

constructor TSubExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Term;
end;

function TSubExpr.Eval(const Off: Integer): Variant;
begin
  Result:=Left.Eval(Off)-Right.Eval(Off);
end;

{ TCmpExpr }

constructor TCmpExpr.Create(const Parser:TExprParser; const ALeft:TExpr);
begin
  inherited Create(Parser,ALeft);
  FRight:=Parser.Composition;
end;

{ TEQExpr }

function TEQExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)=Right.Eval(Off);
end;

{ TNEExpr }

function TNEExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)<>Right.Eval(Off);
end;

{ TLTExpr }

function TLTExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)<Right.Eval(Off);
end;

{ TLEExpr }

function TLEExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)<=Right.Eval(Off);
end;

{ TGTExpr }

function TGTExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)>Right.Eval(Off);
end;

{ TGEExpr }

function TGEExpr.Eval(const Off: Integer): Variant;
begin
  Result:= Left.Eval(Off)>=Right.Eval(Off);
end;

{ TFooExpr }

function TFooExpr.GetActive: Boolean;
begin
  Result:=True;
end;

function TFooExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer): Boolean;
begin
  Result:=True;
end;

{ TDimSymbol }

function TDimSymbol.Eval(const Off: Integer): Variant;
begin
  Result:=TSummary(TDimensionItem(FDim).DimLink).OffValue(Off);
end;

function TDimSymbol.GetActive: Boolean;
begin
  Result:=FDim.Active;
end;

function TDimSymbol.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:=FDim.TryLoad(Range,Dims,Sums);
end;

{ TDictionary }

const
  POOL_SIZE=512;

type
  PPool=^TPool;
  TPool=record
    next:PPool;
    pool:array [0..POOL_SIZE-1]of Char;
  end;

constructor TDictionary.Create(const ASize:Integer);
begin
  inherited Create;
  Size:=ASize;
  ReservedWords;
  Grow;
end;

destructor TDictionary.Destroy;
var
  buf:PPool;
begin
  while head<>nil do begin
    buf:=head;
    head:=buf.next;
    Dispose(buf);
  end;
  inherited;
end;

procedure TDictionary.Grow;
var
  T:TPairArray;
  I,H:Integer;
begin
  T:=FTable;
  FTable:=nil;
  Size:=Size*2;
  for I:=0 to High(T)do
    if T[I].Key<>nil then begin
      H:=IndexOf(T[I].Key);
      FTable[H]:=T[I];
      Inc(FN_Fill);
    end;
end;

function TDictionary.Add(const Word:PChar):Integer;
begin
  Result:=IndexOf(Word);
  with FTable[Result]do
  if Key=nil then begin
    Key  :=Alloc(Word);
    Value:=tkIdent;
    Inc(FN_Fill);
    if 4*FN_Fill > FSize*3 then begin
      Grow;
      Result:=IndexOf(Word);
    end;
  end;
end;

function TDictionary.IndexOf(const Str: PChar): Cardinal;
var
  T:Cardinal;
  P:PChar;
begin
  Result:=StringHash(Str) and FMask;
  T:=(Result*17) and FMask or 1;
  while(True)do begin
    Inc(FN_Try);
    P:=FTable[Result].Key;
    if (P=nil) or (StrComp(P,Str)=0)then Break;
    Inc(FN_Fault);
    Result:=(Result+T)and FMask;
  end;
end;

procedure TDictionary.KeyWord(const AWord:PChar; const Token:TToken);
begin
  with FTable[IndexOf(AWord)]do begin
    Assert(Key=nil,'Duplicate KeyWord');
    Key:=AWord;
    Value:=Token;
  end;
  Inc(FN_Fill);
end;

function TDictionary.RegIdent(const Word: PChar): PChar;
begin
  Result:=FTable[Add(Word)].Key;
end;

procedure TDictionary.ReservedWords;
begin
  KeyWord('and',tkAND);
  KeyWord('or' ,tkOR);
  KeyWord('not',tkNot);
  KeyWord('div',tkDIV);
  KeyWord('mod',tkMOD);
  KeyWord('in' ,tkIN );
end;

procedure TDictionary.SetSize(const Value: Integer);
begin
  FN_Fill:=0;
  FSize:=Value;
  FMask:=FSize-1;
  SetLength(FTable,FSize);
end;

class function TDictionary.StringHash(const Str: PChar): Cardinal;
var
  C:Cardinal;
  P:PChar;
begin
  P:=Str;
  Result:=0;
  while P^<>#0 do begin
    C:=Ord(P^);
    Inc(P);
    Result:=Result*67+(C-113);
  end;
  Inc(Result,P-Str);
end;

function TDictionary.Alloc(const Str: PChar): PChar;
var
  Len:Integer;
  Buf:PPool;
begin
  Len:=StrLen(Str)+1;
  if Len>Count then begin
    New(buf);
    Buf.next:=head;
    head:=Buf;
    count:=POOL_SIZE;
    currp:=@Buf.pool;
  end;
  Result:=StrCopy(currp,Str);
  Inc(currp,Len);
  Dec(Count,Len);
end;

function TDictionary.GetItems(const Index: Integer): PPair;
begin
  Result:=@FTable[Index];
end;

{ TSymbolTable }

constructor TSymbolTable.Create(const ALog: TFxLog);
begin
  inherited Create(False);
  FLog:=ALog;
end;

procedure TSymbolTable.Declare(const ASym: TSymbol);
var
  Sym:TSymbol;
begin
  Sym:=Find(ASym.ID);
  if Sym<>nil then
    FLog.ErrorFmt(SIdentRedeclared,[Sym.ID]);
  Insert(ASym);
end;

function TSymbolTable.Find(const ID: TIdent): TSymbol;
var
  i,j,k:Integer;
begin
  i:=0;
  j:=Count-1;
  while i<=j do begin
    k:=(i+j)shr 1;
    result:=Items[k];
    if ID=Result.ID then begin
      FPoint:=k;
      Exit;
    end else if ID<Result.ID then
      j:=k-1
    else
      i:=k+1;
  end;
  FPoint:=j+1;
  Result:=nil;
end;

procedure TSymbolTable.Insert(const Symbol: TSymbol);
begin
  inherited Insert(FPoint,Symbol);
end;

{ TUseList }

function TUseList.GetItems(const Index:Integer): TScope;
begin
  Result:= inherited Items[Index];
end;

procedure TUseList.SetItems(const Index:Integer; const Value: TScope);
begin
  inherited Items[Index]:=Value;
end;

{ TScope }

procedure TScope.AddUses(const Value:TScope);
begin
  if FUses=nil then FUses:=TUseList.Create;
  FUses.Add(Value);
end;

constructor TScope.Create(const ALog:TFxLog);
begin
  inherited Create(ALog);
end;

function TScope.Lookup(const ID:TIdent):TSymbol;
var
  I:Integer;
begin
  Assert(Self<>nil);
  Result:=Find(ID);
  if (Result=nil) and (FUses<>nil) then
    for I:=0 to FUses.Count-1 do begin
      Result:=FUses[I].Find(ID);
      if Result<>nil then Break;
    end;
  if (Result=nil) and (FParent<>nil) then
    Result:=FParent.Lookup(ID);
end;

{ TUndefSymbol }

constructor TUndefSymbol.Create(const Parser: TParser);
begin
  inherited Create(Parser.FVal.s,Parser.Line);
  Parser.ErrorFmt(SUndefiniteIdent,[string(ID)]);
end;

{ TErrorSymbol }

function TErrorSymbol.Eval(const Off:Integer): Variant;
begin
  Assert(False);
end;

function TErrorSymbol.GetActive: Boolean;
begin
  Result:=False;
end;

function TErrorSymbol.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:=False;
end;

{ TProcSymbol }

procedure TProcSymbol.Assign(const Value: Variant);
begin
  Assert(False);
end;

procedure TProcSymbol.Call(const Args:TExprs; const Off:Integer);
var
  I,ArgCount,ParamCount:Integer;
begin
  if Args<>nil then ArgCount:=Args.Count else ArgCount:=0;
  if Params<>nil then ParamCount:=Params.Count else ParamCount:=0;
  try
    for I:=0 to ParamCount-1 do begin
      if I<ArgCount then
        Params[I].FVal:=Args[I].Eval(Off)
      else if Params[I].FInit<>nil then
        Params[I].FVal:=Params[I].FInit.Eval(Off)
      else
        Params[I].FVal:=Unassigned;
    end;
    if ArgCount>ParamCount then
      raise Exception.CreateFmt ('Line %d: Кол-во аргументов превышает кол-во параметров',[Args[0].Line]);
    // Выполняем тело процедуры
    //if Body<>nil then
      //Body.Execute;
  finally
    // Финализация параметров
    for I:=0 to ParamCount-1 do
      Params[I].FVal:=Unassigned;
  end;
end;

constructor TProcSymbol.Create(const AName:PChar; const AID:Integer);
begin
  inherited Create(AName,0);
  FID:=AID;
end;

destructor TProcSymbol.Destroy;
begin
  FreeAndNil(FParams);
  //FreeAndNil(Body);
  inherited;
end;

function TProcSymbol.Eval(const Off:Integer): Variant;
begin
  Result:=VarProcCreate(nil,Self);
end;

function TProcSymbol.GetActive: Boolean;
begin
  Result:=True;
end;

function TProcSymbol.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
begin
  Result:=True;
end;

{ TParamList }

function TParamList.GetItems(Index: Integer): TParamSymbol;
begin
  Result:=TParamSymbol(inherited Items[Index]);
end;

{ TVarSymbol }

procedure TVarSymbol.Assign(const Value: Variant);
begin
  FVal:=Value;
end;

destructor TVarSymbol.Destroy;
begin
  FreeAndNil(FInit);
  inherited;
end;

function TVarSymbol.Eval(const Off: Integer): Variant;
begin
  Result:=FVal;
end;

procedure TVarSymbol.Initialize;
begin
  FVal:=Unassigned;
  if FInit<>nil then
    FVal:=FInit.Eval(Off);
end;

{ TProcVariant }

procedure TProcVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  RaiseCastError;
end;

procedure TProcVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if AVarType=varString then begin
    with TVarData(Dest)do begin
      VType:=varString;
      string(VString):='procedure "'+string(TProcVarData(Source).VProc.ID)+'"';
    end;
  end else
    RaiseCastError;
end;

procedure TProcVariant.Clear(var V: TVarData);
begin
  with TProcVarData(V)do begin
    VType:=varEmpty;
    VProc:=nil;
    VObj :=nil;
  end;
end;

procedure TProcVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  Assert(not Indirect);
  Dest:=Source;
end;

var
  ProcVariantType:TProcVariant;

function VarIsProc(const AValue:Variant):Boolean;
begin
  Result:= (TVarData(AValue).VType and varTypeMask) = VarProc;
end;

function VarProcCreate(Obj,Proc:Pointer):Variant;
begin
  VarClear(Result);
  with TProcVarData(Result)do begin
    VType:=ProcVariantType.VarType;
    VProc:=Proc;
    VObj :=Obj;
  end;
end;

function VarProc:TVarType;
begin
  Result:=ProcVariantType.VarType;
end;

{ TConstCallExpr }

constructor TConstCallExpr.Create(const Parser:TExprParser; const AProc:TProcSymbol);
begin
  inherited Create(Parser);
  FProc:=AProc;
end;

function TConstCallExpr.Eval(const Off: Integer): Variant;
begin
  FProc.Call(FParams,Off);
  Result:=FProc.Value;
  FProc.Value:=Unassigned;
end;

{ TBuiltInCallExpr }

constructor TBuiltInCallExpr.Create(const Parser:TExprParser; const AID:Integer);
const
  ArgCount:array [-13..-1]of Integer=
    (1,1,1,1,0,1,1,1,1,1,1,1,1);
var
  I:Integer;
begin
  inherited Create(Parser);
  FID:=AID;
  if FParams<>nil then I:=FParams.Count else I:=0;
  if I>ArgCount[FID] then
    Parser.Error('Too many actual parameters')
  else if I<ArgCount[FID] then
    Parser.Error('Not enough actual parameters');
end;

function TBuiltInCallExpr.Eval(const Off: Integer): Variant;
begin
  case FID of
   -1: Result:=Abs(FParams[0].Eval(Off));
   -2: Result:=Sqr(FParams[0].Eval(Off));
   -3: Result:=Sqrt(FParams[0].Eval(Off));
   -4: Result:=Ln(FParams[0].Eval(Off));
   -5: Result:=Exp(FParams[0].Eval(Off));
   -6: Result:=Int(FParams[0].Eval(Off));
   -7: Result:=Frac(FParams[0].Eval(Off));
   -8: Result:=Log10(FParams[0].Eval(Off));
   -9: Result:=Pi;
  -10: Result:=Round(FParams[0].Eval(Off));
  -11: Result:=Sin(FParams[0].Eval(Off));
  -12: Result:=Cos(FParams[0].Eval(Off));
  -13: Result:=Tan(FParams[0].Eval(Off));
  else
    Result:=Null;
  end;
end;

{ TBaseCallExpr }

constructor TBaseCallExpr.Create(const Parser: TExprParser);
var
  Escape:TMetaSet;
begin
  inherited Create;
  FLine:=Parser.Line;
  Parser.Next; //'('
  Escape:=Parser.Escape;
  Include(Parser.FEscape,mtRPar);
  if Parser.Token<>tkRPar then
    FParams:=TExprList.Create(Parser);
  Parser.CheckNext(tkRPar);
  Parser.Escape:=Escape;
end;

destructor TBaseCallExpr.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TBaseCallExpr.GetActive: Boolean;
var
  I:Integer;
begin
  Result:=True;
  if FParams<>nil then
    for I:=0 to FParams.Count-1 do
      Result:=Result and FParams[I].Active;
end;

function TBaseCallExpr.TryLoad(var Range:Cardinal; var Dims,Sums:Integer):Boolean;
var
  I:Integer;
begin
  Result:=True;
  if FParams<>nil then
    for I:=0 to FParams.Count-1 do
      Result:=Result and FParams[I].TryLoad(Range,Dims,Sums);
end;

{ TExternCallExpr }

constructor TExternCallExpr.Create(const Parser:TExprParser; const AID:Integer);
begin
  inherited Create(Parser);
  FID:=AID;
  FCube:=Parser.DataStore;
end;

function TExternCallExpr.Eval(const Off: Integer): Variant;
begin
  if Assigned(FCube.OnCall)then
    Result:=FCube.OnCall(FID-1,Off,FParams);
end;

{ TSystemUnit }

constructor TSystemUnit.Create(const ALog: TFxLog);
const
  FuncName:array [-13..-1] of string=(
    'Tan','Cos','Sin','Round','Pi','Log10','Frac','Int','Exp','Ln','Sqrt','Sqr','Abs'
  );
var
  I:Integer;
begin
  inherited Create(True);
  FScope:=TScope.Create(ALog);
  for I:=Low(FuncName)to High(FuncName)do
    Add(TProcSymbol.Create(Dict.RegIdent(PChar(FuncName[I])),I));
  for I:=0 to Count-1 do
    FScope.Declare(Items[I]);
end;

destructor TSystemUnit.Destroy;
begin
  FreeAndNil(FScope);
  inherited;
end;

initialization
  ProcVariantType:=TProcVariant.Create;
  Dict:=TDictionary.Create(64);
  SystemUnit:=TSystemUnit.Create(nil);
finalization
  SystemUnit.Free;
  Dict.Free;
  FreeAndNil(ProcVariantType);
end.
