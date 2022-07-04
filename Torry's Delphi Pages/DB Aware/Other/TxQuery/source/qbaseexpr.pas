{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   The contents of this file are subject to the Mozilla Public License    }
{   Version 1.1 (the "License"); you may not use this file except in       }
{   compliance with the License. You may obtain a copy of the License at   }
{   http://www.mozilla.org/MPL/                                            }
{                                                                          }
{   Software distributed under the License is distributed on an "AS IS"    }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the}
{   License for the specific language governing rights and limitations     }
{   under the License.                                                     }
{                                                                          }
{   The Original Code is Qbaseexpr.pas                                     }
{                                                                          }
{   The Initial Developer of the Original Code is Alfonso Moreno.          }
{   Portions created by Alfonso Moreno are Copyright (C) Alfonso Moreno.   }
{   All Rights Reserved.                                                   }
{                                                                          }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>          }
{                   Sherlyn CHEW (Malaysia)                                }
{              url: http://code.google.com/p/txquery/                      }
{                   http://groups.google.com/group/txquery                 }
{                                                                          }
{**************************************************************************}

Unit Qbaseexpr;

{$I XQ_flag.Inc}
Interface

Uses
  SysUtils, Classes;

Type

  TExprType = ( ttString, ttFloat, ttInteger, ttBoolean );

  TExpression = Class
  Private
    Function GetMaxLen: Integer;
  Protected
    Function GetMaxString: String; Virtual;
    Function GetAsString: String; Virtual;
    Function GetAsFloat: Double; Virtual;
    Function GetAsInteger: Integer; Virtual;
    Function GetAsBoolean: Boolean; Virtual;
    Function GetExprType: TExprType; Virtual; Abstract;
    Function GetIsNull: boolean; Virtual;
  Public
    Function CanReadAs( aExprType: TExprType ): Boolean;
    {means 'can be interpreted as'. Sort of}
    Property MaxString: String Read GetMaxString;
    Property AsString: String Read GetAsString;
    Property AsFloat: Double Read GetAsFloat;
    Property AsInteger: Integer Read GetAsInteger;
    Property AsBoolean: Boolean Read GetAsBoolean;
    Property ExprType: TExprType Read GetExprType;
    Property IsNull: boolean Read GetIsNull;
    Property MaxLen: Integer read GetMaxLen;
  End;

  TStringLiteral = Class( TExpression )
  Private
    FAsString: String;
  Protected
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( Const aAsString: String );
  End;

  TFloatLiteral = Class( TExpression )
  Private
    FAsFloat: Double;
  Protected
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( Const aAsFloat: Double );
  End;

  TIntegerLiteral = Class( TExpression )
  Private
    FAsInteger: Integer;
  Protected
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aAsInteger: Integer );
  End;

  TBooleanLiteral = Class( TExpression )
  Private
    FAsBoolean: Boolean;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aAsBoolean: Boolean );
  End;

  TParameterList = Class( TList )
  Private
    Function GetAsString( i: Integer ): String;
    Function GetAsFloat( i: Integer ): Double;
    Function GetAsInteger( i: Integer ): Integer;
    Function GetAsBoolean( i: Integer ): Boolean;
    Function GetExprType( i: Integer ): TExprType;
    Function GetParam( i: Integer ): TExpression;
  Public
    Destructor Destroy; Override;
    Property Param[i: Integer]: TExpression Read GetParam;
    Property ExprType[i: Integer]: TExprType Read GetExprType;
    Property AsString[i: Integer]: String Read GetAsString;
    Property AsFloat[i: Integer]: Double Read GetAsFloat;
    Property AsInteger[i: Integer]: Integer Read GetAsInteger;
    Property AsBoolean[i: Integer]: Boolean Read GetAsBoolean;
  End;

  TFunction = Class( TExpression )
  Private
    FParameterList: TParameterList;
    Function GetParam( n: Integer ): TExpression;
  Public
    Constructor Create( aParameterList: TParameterList );
    Destructor Destroy; Override;
    Function ParameterCount: Integer;
    Property Param[n: Integer]: TExpression Read GetParam;
  End;

  TTypeCast = Class( TFunction )
  Private
    Operator: TExprType;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aParameterList: TParameterList;
      aOperator: TExprType );
  End;

  TMF =
    ( mfTrunc, mfRound, mfAbs, mfArcTan, mfCos, mfExp, mfFrac, mfInt,
    mfLn, mfPi, mfSin, mfSqr, mfSqrt, mfPower );

  TMathExpression = Class( TFunction )
  Private
    Operator: TMF;
    Procedure CheckParameters;
  Protected
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aParameterList: TParameterList;
      aOperator: TMF );
  End;

  TSF = ( sfUpper, sfLower, sfCopy, sfPos, sfLength, sfLTrim, sfRTrim, sfTrim );

  TStringExpression = Class( TFunction )
  Private
    Operator: TSF;
    Procedure CheckParameters;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsInteger: Integer; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aParameterList: TParameterList; aOperator: TSF );
  End;

  TConditional = Class( TFunction )
  Private
    Procedure CheckParameters;
    Function Rex: TExpression;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  End;

Type
  TOperator = ( opNot,
    opExp,
    opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr,
    opPlus, opMinus, opOr, opXor,
    opEq, opNEQ, opLT, opGT, opLTE, opGTE );

  TOperators = Set Of TOperator;

  TUnaryOp = Class( TExpression )
  Private
    Operand: TExpression;
    OperandType: TExprType;
    Operator: TOperator;
  Protected
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aOperator: TOperator; aOperand: TExpression );
    Destructor Destroy; Override;
  End;

  TBinaryOp = Class( TExpression )
  Private
    Operand1, Operand2: TExpression;
    Operator: TOperator;
    OperandType: TExprType;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aOperator: TOperator; aOperand1, aOperand2: TExpression );
    Destructor Destroy; Override;
  End;

  TRelationalOp = Class( TExpression )
  Private
    Operand1, Operand2: TExpression;
    Operator: TOperator;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( aOperator: TOperator; aOperand1, aOperand2: TExpression );
    Destructor Destroy; Override;
  End;

  EExpression = Class( Exception );

  { additional functions }
  TASCIIExpr = Class( TFunction )
  Protected
    Function GetAsInteger: Integer; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( ParameterList: TParameterList );
  End;

  TLeftExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  End;

  TRightExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  End;

  { This function is used exclusively for the LIKE predicate in SQL }
  TLikePos = ( lpNone, lpLeft, lpMiddle, lpRight );
  TLikeCode = ( lcSingle, lcMultiple, lcOnlyUnderscores );

  TLikeItem = Class( TObject )
  Public
    LikeText: String; { text to find }
    LikePos: TLikePos; { text must go at left, middle, right or on a column }
    LikeCode: TLikeCode;
  End;

  TLikeList = Class( TObject )
  Private
    fItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TLikeItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TLikeItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TLikeItem Read GetItem; Default;
  End;

  TSQLLikeExpr = Class( Tfunction )
  Private
    LikeList: TLIKEList;
    FIsNotLike: Boolean;
    Function SQLPos( Var Start: Integer; Const Substr, Str: String ): Integer;
  Protected
    Function GetAsBoolean: Boolean; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( ParameterList: TParameterList; IsNotLike: Boolean );
    Destructor Destroy; Override;
    Procedure AddToList( Like: TLikeItem );
  End;

  { TSQLInPredicateExpr }
  { This function is used exclusively for the IN predicate in SQL SELECT
     something like this : SELECT * FROM customer WHERE CustNo IN (1,10,8) }
  TSQLInPredicateExpr = Class( Tfunction )
  Private
    FIsNotIn: Boolean;
  Protected
    Function GetAsBoolean: Boolean; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( ParameterList: TParameterList; IsNotIn: Boolean );
  End;

  TBetweenExpr = Class( Tfunction )
  Private
    FIsNotBetween: Boolean;
  Protected
    Function GetAsBoolean: Boolean; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( ParameterList: TParameterList; IsNotBetween: Boolean );
  End;

  TCaseWhenElseExpr = Class( TFunction )
  Private
    FElseExpr: TExpression;
    FThenParamList: TParameterList;
    Procedure CheckParameters;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( WhenParamList: TParameterList;
      ThenParamList: TParameterList; ElseExpr: TExpression );
    Destructor Destroy; Override;
  End;

  TDecodeExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( ParameterList: TParameterList );
  End;

  {Evaluate FormatDateTime('dd/mmm/yyyy', 32767)}
  TFormatDateTimeExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  End;

  {Evaluate FormatFloat('###,###,##0.00', 12345.567)}
  TFormatFloatExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  End;

  {Evaluate Format(Format,Args)}
  TFormatExpr = Class( TFunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprType: TExprType; Override;
  End;

  TDecodeKind = ( dkYear, dkMonth, dkDay, dkHour, dkMin, dkSec, dkMSec, dkWeek );

  { supports syntax: YEAR(expr), MONTH(expr), DAY(expr), HOUR(expr), MIN(expr), SEC(expr), MSEC(expr)}
  TDecodeDateTimeExpr = Class( TFunction )
  Private
    FDecodeKind: TDecodeKind;
  Protected
    Function GetAsInteger: Integer; Override;
    Function GetExprType: TExprType; Override;
  Public
    Constructor Create( ParameterList: TParameterList; DecodeKind: TDecodeKind );
  End;

  {  MINOF(arg1,arg2, ..., argn), MAXOF(ARG1,ARG2, ... ,argn)
      hint by: Fady Geagea
  }
  TMinMaxOfExpr = Class( Tfunction )
  Private
    FIsMin: Boolean;
  Protected
    Function GetAsFloat: Double; Override;
    function GetExprType: TExprtype; override;
  Public
    Constructor Create( ParameterList: TParameterList; IsMin: Boolean );
  End;

Const
  {to get a string representation of TExprType use NExprType[ExprType] }
  NExprType: Array[TExprType] Of String =
  ( 'String', 'Float', 'Integer', 'Boolean' );

  NBoolean: Array[Boolean] Of String = ( 'FALSE', 'TRUE' );

Implementation

Uses
  xqmiscel, QExprYacc;

Resourcestring
  SEXPR_WRONGWHENEXPR = 'Expression in Case must be boolean';

  SEXPR_WRONGTHENEXPR = 'Expressions in THEN section must be all of same type';

  SEXPR_UNKNOWNID = 'Unknown Identifier %s';

  SEXPR_OPERATORINCOMPAT = 'Operator %s incompatible with %s';

  SEXPR_CANNOTCASTTOSTRING = 'Cannot read %s as String';

  SEXPR_CANNOTCASTTOFLOAT = 'Cannot read %s as Float';

  SEXPR_CANNOTCASTTOINTEGER = 'Cannot read %s as Integer';

  SEXPR_CANNOTCASTTOBOOLEAN = 'Cannot read %s as boolean';

  SEXPR_WRONGUNARYOP = '%s is not simple unary operator';

  SEXPR_WRONGBINARYOP = '%s is not a simple binary operator';

  SEXPR_WRONGBOOLEANOP = 'cannot apply %s to boolean operands';

  SEXPR_WRONGRELATIONALOP = '%s is not relational operator';

  SEXPR_WRONGPARAMETER = 'Invalid parameter to %s';

  SEXPR_INVALIDPARAMETERTO = 'Invalid parameter to %s';

Const
  NOperator: Array[TOperator] Of String =
  ( 'opNot',
    'opExp',
    'opMult', 'opDivide', 'opDiv', 'opMod', 'opAnd', 'opShl', 'opShr',
    'opPlus', 'opMinus', 'opOr', 'opXor',
    'opEq', 'opNEQ', 'opLT', 'opGT', 'opLTE', 'opGTE' );

  RelationalOperators = [opEQ, opNEQ, opLT, opGT, opLTE, opGTE];

Function ResultType( Operator: TOperator; OperandType: TExprType ): TExprType;

  Procedure NotAppropriate;
  Begin
    Result := ttString;
    Raise EExpression.CreateFmt( SEXPR_OPERATORINCOMPAT,
      [NOperator[Operator], NExprType[OperandType]] )
  End;

Begin
  Case OperandType Of
    ttString:
      Case Operator Of
        opPlus: Result := ttString;
        opEq..opGTE: Result := ttBoolean;
      Else
        NotAppropriate;
      End;
    ttFloat:
      Case Operator Of
        opExp, opMult, opDivide, opPlus, opMinus: Result := ttFloat;
        opEq..opGTE: Result := ttBoolean;
      Else
        NotAppropriate;
      End;
    ttInteger:
      Case Operator Of
        opNot, opMult, opDiv, opMod, opAnd, opShl, opShr, opPlus, opMinus,
          opOr, opXor: Result := ttInteger;
        opExp, opDivide: Result := ttFloat;
        opEq..opGTE: Result := ttBoolean;
      Else
        NotAppropriate;
      End;
    ttBoolean:
      Case Operator Of
        opNot, opAnd, opOr, opXor, opEq, opNEQ: Result := ttBoolean;
      Else
        NotAppropriate;
      End;
  End
End;

Function CommonType( Op1Type, Op2Type: TExprType ): TExprType;
Begin
  If Op1Type < Op2Type Then
    Result := Op1Type
  Else
    Result := Op2Type
End;

Procedure Internal( Code: Integer );
Begin
  Raise EExpression.CreateFmt( 'Internal parser error. Code %d', [Code] )
End;

Function TExpression.GetIsNull: boolean;
Begin
  //Result := AsString = '';
  Result := False;
End;

Function TExpression.GetMaxLen: Integer;
Begin
  Result:= 0;
  If ExprType = ttString then
    Result:= Length( GetMaxString ) * SizeOf(Char);  { patched by ccy }
End;

Function TExpression.GetMaxString: String;
Begin
  Result:= AsString;  { the same string as default }
End;

Function TExpression.GetAsString: String;
Begin
  Case ExprType Of
    ttString: Raise EExpression.CreateFmt( SEXPR_CANNOTCASTTOSTRING,
        [NExprType[ExprType]] );
    ttFloat: Result := FloatToStr( AsFloat );
    ttInteger: Result := IntToStr( AsInteger );
    ttBoolean: Result := NBoolean[AsBoolean];
  End
End;

Function TExpression.GetAsFloat: Double;
Begin
  Result := 0;
  Case ExprType Of
    ttString:
      begin
        try
          Result := StrToFloat (AsString);
        except
          on EConvertError do
            Result := StrToDateTime (AsString);
        end;
      end;
    ttFloat:
      Raise EExpression.CreateFmt( SEXPR_CANNOTCASTTOFLOAT, [NExprType[ExprType]] );
    ttInteger, ttBoolean: Result := AsInteger;
  End
End;

Function TExpression.GetAsInteger: Integer;
Begin
  Result := 0;
  Case ExprType Of
    // Allow cast expression to string
    ttString : Result := StrToInt (AsString);
    //ttFloat : Result := FloatToStr (AsFloat);
    ttFloat, ttInteger: raise EExpression.CreateFmt(SEXPR_CANNOTCASTTOINTEGER,
                               [NExprType[ExprType]]);
    ttBoolean: Result := Integer( AsBoolean );
  End;
End;

Function TExpression.GetAsBoolean: Boolean;
Begin
  Raise EExpression.CreateFmt( SEXPR_CANNOTCASTTOBOOLEAN,
    [NExprType[ExprType]] )
End;

Function TExpression.CanReadAs( aExprType: TExprType ): Boolean;
Begin
  Result := Ord( ExprType ) >= Ord( aExprType )
End;

{ TStringLiteral }
Function TStringLiteral.GetAsString: String;
Begin
  Result := FAsString
End;

Function TStringLiteral.GetExprType: TExprType;
Begin
  Result := ttString
End;

Constructor TStringLiteral.Create( Const aAsString: String );
Begin
  Inherited Create;
  FAsString := aAsString
End;

Function TFloatLiteral.GetAsString: String;
Begin
  Result := FloatToStr( FAsFloat )
End;

Function TFloatLiteral.GetAsFloat: Double;
Begin
  Result := FAsFloat
End;

Function TFloatLiteral.GetExprType: TExprType;
Begin
  Result := ttFloat
End;

Constructor TFloatLiteral.Create( Const aAsFloat: Double );
Begin
  Inherited Create;
  FAsFloat := aAsFloat
End;

Function TIntegerLiteral.GetAsString: String;
Begin
  Result := FloatToStr( FAsInteger )
End;

Function TIntegerLiteral.GetAsFloat: Double;
Begin
  Result := FAsInteger
End;

Function TIntegerLiteral.GetAsInteger: Integer;
Begin
  Result := FAsInteger
End;

Function TIntegerLiteral.GetExprType: TExprType;
Begin
  Result := ttInteger
End;

Constructor TIntegerLiteral.Create( aAsInteger: Integer );
Begin
  Inherited Create;
  FAsInteger := aAsInteger
End;

function TBooleanLiteral.GetMaxString: String;
begin
  Result:=NBoolean[False];
end;

Function TBooleanLiteral.GetAsString: String;
Begin
  Result := NBoolean[FAsBoolean];
End;

Function TBooleanLiteral.GetAsFloat: Double;
Begin
  Result := GetAsInteger;
End;

Function TBooleanLiteral.GetAsInteger: Integer;
Begin
  Result := Integer( FAsBoolean );
End;

Function TBooleanLiteral.GetAsBoolean: Boolean;
Begin
  Result := FAsBoolean;
End;

Function TBooleanLiteral.GetExprType: TExprType;
Begin
  Result := ttBoolean
End;

Constructor TBooleanLiteral.Create( aAsBoolean: Boolean );
Begin
  Inherited Create;
  FAsBoolean := aAsBoolean
End;

Function TUnaryOp.GetAsFloat: Double;
Begin
  Case Operator Of
    opMinus: Result := -Operand.AsFloat;
    opPlus: Result := Operand.AsFloat;
  Else
    Result := Inherited GetAsFloat;
  End
End;

Function TUnaryOp.GetAsInteger: Integer;
Begin
  Result := 0;
  Case Operator Of
    opMinus: Result := -Operand.AsInteger;
    opPlus: Result := Operand.AsInteger;
    opNot:
      Case OperandType Of
        ttInteger: Result := Not Operand.AsInteger;
        ttBoolean: Result := Integer( AsBoolean );
      Else
        Internal( 6 );
      End;
  Else
    Result := Inherited GetAsInteger;
  End
End;

Function TUnaryOp.GetAsBoolean: Boolean;
Begin
  Case Operator Of
    opNot: Result := Not ( Operand.AsBoolean )
  Else
    Result := Inherited GetAsBoolean;
  End
End;

Function TUnaryOp.GetExprType: TExprType;
Begin
  Result := ResultType( Operator, OperandType )
End;

Constructor TUnaryOp.Create( aOperator: TOperator; aOperand: TExpression );
Begin
  Inherited Create;
  Operand := aOperand;
  Operator := aOperator;
  OperandType := Operand.ExprType;
  If Not ( Operator In [opNot, opPlus, opMinus] ) Then
    Raise EExpression.CreateFmt( SEXPR_WRONGUNARYOP,
      [NOperator[Operator]] )
End;

Destructor TUnaryOp.Destroy;
Begin
  Operand.Free;
  Inherited Destroy
End;

Function TBinaryOp.GetMaxString: String;
begin
  If ExprType = ttString then
  begin
    Case Operator Of
      opPlus: Result := Operand1.GetMaxString + Operand2.GetMaxString;
    Else
      Internal( 10 );
    End;
  end else
    Result:= GetAsString;
end;

Function TBinaryOp.GetAsString: String;
Begin
  Result := '';
  Case ExprType Of
    ttString:
      Case Operator Of
        opPlus: Result := Operand1.AsString + Operand2.AsString;
      Else
        Internal( 10 );
      End;
    ttFloat:
      Result := FloatToStr( AsFloat );
    ttInteger:
      Result := IntToStr( AsInteger );
    ttBoolean:
      Result := NBoolean[AsBoolean];
  End
End;

Function TBinaryOp.GetAsFloat: Double;
Begin
  Result := 0;
  Case ExprType Of
    ttFloat:
      Case Operator Of
        opExp: Result := Exp( Operand2.AsFloat * Ln( Operand1.AsFloat ) );
        opPlus: Result := Operand1.AsFloat + Operand2.AsFloat;
        opMinus: Result := Operand1.AsFloat - Operand2.AsFloat;
        opMult: Result := Operand1.AsFloat * Operand2.AsFloat;
        opDivide: Result := Operand1.AsFloat / Operand2.AsFloat;
      Else
        Internal( 11 );
      End;
    ttInteger:
      Result := AsInteger;
    ttBoolean:
      Result := Integer( AsBoolean );
  End
End;

Function TBinaryOp.GetAsInteger: Integer;
Begin
  Result := 0;
  Case ExprType Of
    ttInteger:
      Case Operator Of
        opPlus: Result := Operand1.AsInteger + Operand2.AsInteger;
        opMinus: Result := Operand1.AsInteger - Operand2.AsInteger;
        opMult: Result := Operand1.AsInteger * Operand2.AsInteger;
        opDiv: Result := Operand1.AsInteger Div Operand2.AsInteger;
        opMod: Result := Operand1.AsInteger Mod Operand2.AsInteger;
        opShl: Result := Operand1.AsInteger Shl Operand2.AsInteger;
        opShr: Result := Operand1.AsInteger Shr Operand2.AsInteger;
        opAnd: Result := Operand1.AsInteger And Operand2.AsInteger;
        opOr: Result := Operand1.AsInteger Or Operand2.AsInteger;
        opXor: Result := Operand1.AsInteger Xor Operand2.AsInteger;
      Else
        Internal( 12 );
      End;
    ttBoolean:
      Result := Integer( GetAsBoolean );
  End
End;

Function TBinaryOp.GetAsBoolean: Boolean;
Begin
  Result := false;
  Case Operator Of
    opAnd: Result := Operand1.AsBoolean And Operand2.AsBoolean;
    opOr: Result := Operand1.AsBoolean Or Operand2.AsBoolean;
    opXor: Result := Operand1.AsBoolean Xor Operand2.AsBoolean;
  Else
    Internal( 13 );
  End
End;

Function TBinaryOp.GetExprType: TExprType;
Begin
  GetExprType := ResultType( Operator, OperandType )
End;

Constructor TBinaryOp.Create( aOperator: TOperator; aOperand1, aOperand2: TExpression );
Begin
  Inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  OperandType := CommonType( Operand1.ExprType, Operand2.ExprType );
  If Not ( Operator In [opExp, opMult..opXor] ) Then
    Raise EExpression.CreateFmt( SEXPR_WRONGBINARYOP,
      [NOperator[Operator]] )
End;

Destructor TBinaryOp.Destroy;
Begin
  Operand1.Free;
  Operand2.Free;
  Inherited Destroy
End;

Function TRelationalOp.GetMaxString: String;
Begin
  Result := NBoolean[False];
End;

Function TRelationalOp.GetAsString: String;
Begin
  Result := NBoolean[AsBoolean]
End;

Function TRelationalOp.GetAsFloat: Double;
Begin
  Result := Integer( AsBoolean )
End;

Function TRelationalOp.GetAsInteger: Integer;
Begin
  Result := Integer( AsBoolean )
End;

Function TRelationalOp.GetAsBoolean: Boolean;
Begin
  Result := false;
  If ( Operand1.IsNull ) Or ( Operand2.IsNull ) Then
    Exit;
  Case CommonType( Operand1.ExprType, Operand2.ExprType ) Of
    ttBoolean:
      Case Operator Of
        opEQ: Result := Operand1.AsBoolean = Operand2.AsBoolean;
        opNEQ: Result := Operand1.AsBoolean <> Operand2.AsBoolean;
      Else
        Raise EExpression.CreateFmt( SEXPR_WRONGBOOLEANOP,
          [NOperator[Operator]] );
      End;

    ttInteger:
      Case Operator Of
        opLT: Result := Operand1.AsInteger < Operand2.AsInteger;
        opLTE: Result := Operand1.AsInteger <= Operand2.AsInteger;
        opGT: Result := Operand1.AsInteger > Operand2.AsInteger;
        opGTE: Result := Operand1.AsInteger >= Operand2.AsInteger;
        opEQ: Result := Operand1.AsInteger = Operand2.AsInteger;
        opNEQ: Result := Operand1.AsInteger <> Operand2.AsInteger;
      End;

    ttFloat:
      Case Operator Of
        opLT: Result := Operand1.AsFloat < Operand2.AsFloat;
        opLTE: Result := Operand1.AsFloat <= Operand2.AsFloat;
        opGT: Result := Operand1.AsFloat > Operand2.AsFloat;
        opGTE: Result := Operand1.AsFloat >= Operand2.AsFloat;
        opEQ: Result := Operand1.AsFloat = Operand2.AsFloat;
        opNEQ: Result := Operand1.AsFloat <> Operand2.AsFloat;
      End;

    ttString:
      Case Operator Of
        opLT: Result := Operand1.AsString < Operand2.AsString;
        opLTE: Result := Operand1.AsString <= Operand2.AsString;
        opGT: Result := Operand1.AsString > Operand2.AsString;
        opGTE: Result := Operand1.AsString >= Operand2.AsString;
        opEQ: Result := Operand1.AsString = Operand2.AsString;
        opNEQ: Result := Operand1.AsString <> Operand2.AsString;
      End;
  End
End;

Function TRelationalOp.GetExprType: TExprType;
Begin
  Result := ttBoolean
End;

Constructor TRelationalOp.Create( aOperator: TOperator; aOperand1, aOperand2: TExpression );
Begin
  Inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  If Not ( Operator In RelationalOperators ) Then
    Raise EExpression.CreateFmt( SEXPR_WRONGRELATIONALOP,
      [NOperator[Operator]] )
End;

Destructor TRelationalOp.Destroy;
Begin
  Operand1.Free;
  Operand2.Free;
  Inherited Destroy
End;

Function TParameterList.GetAsString( i: Integer ): String;
Begin
  Result := Param[i].AsString
End;

Function TParameterList.GetAsFloat( i: Integer ): Double;
Begin
  Result := Param[i].AsFloat
End;

Function TParameterList.GetAsInteger( i: Integer ): Integer;
Begin
  Result := Param[i].AsInteger
End;

Function TParameterList.GetAsBoolean( i: Integer ): Boolean;
Begin
  Result := Param[i].AsBoolean
End;

Function TParameterList.GetExprType( i: Integer ): TExprType;
Begin
  Result := Param[i].ExprType
End;

Function TParameterList.GetParam( i: Integer ): TExpression;
Begin
  Result := TExpression( Items[i] )
End;

Destructor TParameterList.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To ( Count - 1 ) Do
    TObject( Items[i] ).Free;
  Inherited Destroy
End;

{ TFunction }

Function TFunction.GetParam( n: Integer ): TExpression;
Begin
  Result := FParameterList.Param[n]
End;

Function TFunction.ParameterCount: Integer;
Begin
  If ( FParameterList <> Nil ) Then
    ParameterCount := FParameterList.Count
  Else
    ParameterCount := 0
End;

Constructor TFunction.Create( aParameterList: TParameterList );
Begin
  Inherited Create;
  FParameterList := aParameterList
End;

Destructor TFunction.Destroy;
Begin
  FParameterList.Free;
  Inherited Destroy
End;

Const
  NTypeCast: Array[TExprType] Of PChar =
  ( 'STRING', 'FLOAT', 'INTEGER', 'BOOLEAN' );
  NMF: Array[TMF] Of PChar =
  ( 'TRUNC', 'ROUND', 'ABS', 'ARCTAN', 'COS', 'EXP', 'FRAC', 'INT',
    'LN', 'PI', 'SIN', 'SQR', 'SQRT', 'POWER' );
  NSF: Array[TSF] Of PChar = ( 'UPPER', 'LOWER', 'COPY', 'POS', 'LENGTH', 'LTRIM', 'RTRIM', 'TRIM' );

Function TStringExpression.GetMaxString: String;
Begin
  CheckParameters;
  Case Operator Of
    sfUpper, sfLower, sfLTrim, sfRTrim, sfTrim: Result := Param[0].MaxString;
    sfCopy: Result := Copy( Param[0].MaxString, Param[1].AsInteger, Param[2].AsInteger );
  Else
    Result := Inherited GetAsString;
  End
End;

Function TStringExpression.GetAsString: String;
Begin
  CheckParameters;
  Case Operator Of
    sfUpper: Result := AnsiUpperCase( Param[0].AsString );
    sfLower:
      Result := AnsiLowerCase( Param[0].AsString );
    sfCopy: Result := Copy( Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger );
    sfLTrim: Result := TrimLeft( Param[0].AsString );
    sfRTrim: Result := TrimRight( Param[0].AsString );
    sfTrim: Result := Trim( Param[0].AsString );
  Else
    Result := Inherited GetAsString;
  End
End;

Function TStringExpression.GetAsInteger: Integer;
Begin
  CheckParameters;
  Case Operator Of
    sfPos: Result := AnsiPos( Param[0].AsString, Param[1].AsString );
    sfLength: Result := Length( Param[0].AsString );
  Else
    Result := Inherited GetAsInteger
  End
End;

Function TStringExpression.GetExprType: TExprType;
Begin
  Case Operator Of
    sfUpper, sfLower, sfCopy, sfLTrim, sfRTrim, sfTrim: Result := ttString;
  Else
    Result := ttInteger;
  End
End;

Procedure TStringExpression.CheckParameters;
Var
  OK: Boolean;
Begin
  OK := false;
  Case Operator Of
    sfUpper, sfLower, sfLength, sfLTrim, sfRTrim, sfTrim:
      OK := ( ParameterCount = 1 ) And
        ( Param[0].ExprType >= ttString );
    sfCopy:
      OK := ( ParameterCount = 3 ) And
        ( Param[0].ExprType >= ttString ) And
        ( Param[1].ExprType >= ttInteger ) And
        ( Param[2].ExprType >= ttInteger );
    sfPos:
      OK := ( ParameterCount = 2 ) And
        ( Param[0].ExprType >= ttString ) And
        ( Param[1].ExprType >= ttString );
  End;
  If Not OK Then
    Raise EExpression.CreateFmt( SEXPR_WRONGPARAMETER,
      [NSF[Operator]] )
End;

Constructor TStringExpression.Create( aParameterList: TParameterList;
  aOperator: TSF );
Begin
  Inherited Create( aParameterList );
  Operator := aOperator
End;

Function TMathExpression.GetAsFloat: Double;
Begin
  CheckParameters;
  Case Operator Of
    mfAbs: Result := Abs( Param[0].AsFloat );
    mfArcTan: Result := ArcTan( Param[0].AsFloat );
    mfCos: Result := Cos( Param[0].AsFloat );
    mfExp: Result := Exp( Param[0].AsFloat );
    mfFrac: Result := Frac( Param[0].AsFloat );
    mfInt: Result := Int( Param[0].AsFloat );
    mfLn: Result := Ln( Param[0].AsFloat );
    mfPi: Result := System.Pi;
    mfSin: Result := Sin( Param[0].AsFloat );
    mfSqr: Result := Sqr( Param[0].AsFloat );
    mfSqrt: Result := Sqrt( Param[0].AsFloat );
    mfPower: Result := Exp( Param[1].AsFloat * Ln( Param[0].AsFloat ) )
  Else
    Result := Inherited GetAsFloat;
  End
End;

Function TMathExpression.GetAsInteger: Integer;
Begin
  CheckParameters;
  Case Operator Of
    mfTrunc: Result := Trunc( Param[0].AsFloat );
    mfRound: Result := Round( Param[0].AsFloat );
    mfAbs: Result := Abs( Param[0].AsInteger );
  Else
    Result := Inherited GetAsInteger;
  End
End;

Procedure TMathExpression.CheckParameters;
Var
  OK: Boolean;
Begin
  OK := True;
  Case Operator Of
    mfTrunc, mfRound, mfArcTan, mfCos, mfExp, mfFrac, mfInt,
      mfLn, mfSin, mfSqr, mfSqrt, mfAbs:
      Begin
        OK := ( ParameterCount = 1 ) And
          ( Param[0].ExprType >= ttFloat );
      End;
    mfPower:
      Begin
        OK := ( ParameterCount = 2 ) And
          ( Param[0].ExprType >= ttFloat ) And
          ( Param[1].ExprType >= ttFloat );
      End;
  End;
  If Not OK Then
    Raise EExpression.CreateFmt( SEXPR_INVALIDPARAMETERTO, [NMF[Operator]] )
End;

Function TMathExpression.GetExprType: TExprType;
Begin
  Case Operator Of
    mfTrunc, mfRound: Result := ttInteger;
  Else
    Result := ttFloat;
  End
End;

Constructor TMathExpression.Create( aParameterList: TParameterList;
  aOperator: TMF );
Begin
  Inherited Create( aParameterList );
  Operator := aOperator
End;

function TTypeCast.GetMaxString: String;
begin
  Result := Param[0].GetMaxString
end;

Function TTypeCast.GetAsString: String;
Begin
  Result := Param[0].AsString
End;

Function TTypeCast.GetAsFloat: Double;
Begin
  If Param[0].ExprType = ttString Then
    Result := StrToFloat( Param[0].AsString )
  Else
    Result := Param[0].AsFloat
End;

Function TTypeCast.GetAsInteger: Integer;
Begin
  If Param[0].ExprType = ttString Then
    Result := StrToInt( Param[0].AsString )
  Else If Param[0].ExprType = ttFloat Then
    Result := Trunc( Param[0].AsFloat )
  Else
    Result := Param[0].AsInteger
End;

Function TTypeCast.GetAsBoolean: Boolean;
Begin
  Result := Param[0].AsBoolean
End;

Function TTypeCast.GetExprType: TExprType;
Begin
  Result := Operator
End;

Constructor TTypeCast.Create( aParameterList: TParameterList;
  aOperator: TExprType );
Begin
  Inherited Create( aParameterList );
  Operator := aOperator
End;

Function TConditional.Rex: TExpression;
Begin
  CheckParameters;
  If Param[0].AsBoolean Then
    Result := Param[1]
  Else
    Result := Param[2]
End;

Procedure TConditional.CheckParameters;
Begin
  If Not ( ( ParameterCount = 3 ) And
    ( Param[0].ExprType = ttBoolean ) ) Then
    Raise EExpression.Create( 'IF: Invalid parameters' )
End;

Function TConditional.GetMaxString: String;
Begin
  If Length( Param[1].AsString ) > Length( Param[2].AsString ) Then
    Result := Param[1].AsString
  Else
    Result := Param[2].AsString;
End;

Function TConditional.GetAsString: String;
Begin
  Result := Rex.AsString;
End;

Function TConditional.GetAsFloat: Double;
Begin
  Result := Rex.AsFloat
End;

Function TConditional.GetAsInteger: Integer;
Begin
  Result := Rex.AsInteger
End;

Function TConditional.GetAsBoolean: Boolean;
Begin
  Result := Rex.AsBoolean
End;

Function TConditional.GetExprType: TExprType;
Begin
  Result := Rex.ExprType
End;

{TASCIIExpr}

Constructor TASCIIExpr.Create( ParameterList: TParameterList );
Begin
  Inherited Create( ParameterList );
  If ( ParameterList.Count <> 1 ) Or ( ParameterList.ExprType[0] <> ttString ) Then
    Raise EExpression.Create( 'ASCII: Incorrect argument' );
End;

Function TASCIIExpr.GetAsInteger: Integer;
Begin
  If Length( Param[0].AsString ) = 0 Then
    Result := 0
  Else
    Result := Ord( Param[0].AsString[1] );
End;

Function TASCIIExpr.GetExprType: TExprType;
Begin
  result := ttInteger;
End;

{ TLeftExpr }

Function TLeftExpr.GetMaxString: String;
Begin
  Result := Copy( Param[0].GetMaxString, 1, Param[1].AsInteger );
End;

Function TLeftExpr.GetAsString: String;
Begin
  Result := Copy( Param[0].AsString, 1, Param[1].AsInteger );
End;

Function TLeftExpr.GetExprType: TExprType;
Begin
  Result := ttString;
End;

Function imax( a, b: integer ): integer;
Begin
  If a > b Then
    result := a
  Else
    result := b;
End;

Function imin( a, b: integer ): integer;
Begin
  If a < b Then
    result := a
  Else
    result := b;
End;

{ TRightExpr }

Function TRightExpr.GetMaxString: String;
Var
  p: Integer;
Begin
  p := IMax( 1, Length( Param[0].GetMaxString ) - Param[1].AsInteger + 1 );
  Result := Copy( Param[0].GetMaxString, p, Param[1].AsInteger );
End;

Function TRightExpr.GetAsString: String;
Var
  p: Integer;
Begin
  p := IMax( 1, Length( Param[0].AsString ) - Param[1].AsInteger + 1 );
  Result := Copy( Param[0].AsString, p, Param[1].AsInteger );
End;

Function TRightExpr.GetExprType: TExprType;
Begin
  Result := ttString;
End;

{ TLikeList implementaton}

Constructor TLikeList.Create;
Begin
  Inherited Create;
  fItems := TList.Create;
End;

Destructor TLikeList.Destroy;
Begin
  Clear;
  fItems.Free;
  Inherited Destroy;
End;

Function TLikeList.GetCount;
Begin
  Result := fItems.Count;
End;

Function TLikeList.GetItem( Index: Integer ): TLikeItem;
Begin
  Result := fItems[Index];
End;

Function TLikeList.Add: TLikeItem;
Begin
  Result := TLikeItem.Create;
  fItems.Add( Result );
End;

Procedure TLikeList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To fItems.Count - 1 Do
    TLikeItem( fItems[I] ).Free;
  fItems.Clear;
End;

Procedure TLikeList.Delete( Index: Integer );
Begin
  TLikeItem( fItems[Index] ).Free;
  fItems.Delete( Index );
End;

{ TSQLLikeExpr implementation }

Constructor TSQLLikeExpr.Create( ParameterList: TParameterList; IsNotLike: Boolean );
Var
  s, Work: String;
  p, n: Integer;
  Previous: Char;
  EscapeChar: Char;
  Accept: Boolean;
  Found, FoundPerc: Boolean;
Begin
  Inherited Create( ParameterList );
  LIKEList := TLikeList.Create;
  FIsNotLike := IsNotLike;
  If ( ParameterCount > 2 ) And ( Length( Param[2].AsString ) > 0 ) Then
    EscapeChar := Param[2].AsString[1]
  Else
    EscapeChar := #0;

  s := Param[1].AsString;
  If ( Length( s ) = 0 ) Or ( ( AnsiPos( '%', s ) = 0 ) And ( AnsiPos( '_', s ) = 0 ) ) Then
  Begin
    With LikeList.Add Do
    Begin
      LikeText := s;
      LikePos := lpNone;
    End;
  End
  Else
  Begin
    // verifica si solo son "_"
    Found:=false;
    FoundPerc:=false;
    for p:= 1 to Length(s) do
    begin
      if s[p]='%' then
      begin
        FoundPerc:=true;
        break;
      end;
      if s[p]='_' then
      begin
        Found:=true;
      end;
    end;
    if Found and not FoundPerc then
    begin
      With LikeList.Add Do
      Begin
        LikeText := s;
        LikePos := lpLeft;
        LikeCode := lcOnlyUnderscores;
      End;
      exit;
    end;
    work := '';
    p := 1;
    n := 0;
    Previous := #0;
    While p <= Length( s ) Do
    Begin
      Accept := ( ( s[p] = '%' ) And ( EscapeChar = #0 ) ) Or
        ( ( s[p] = '%' ) And ( Previous <> EscapeChar ) ) Or
        ( ( s[p] = '_' ) And ( Previous <> EscapeChar ) );
      If Accept Then
      Begin
        If ( Length( Work ) > 0 ) Then
        Begin
          If (n = 0) and (p = Length(s)) Then   { patched by ccy }
          Begin
            // text must start with Work
            With LikeList.Add Do
            Begin
              LikeText := Work;
              LikePos := lpLeft;
              If s[p] = '_' Then
                LikeCode := lcSingle
              Else
                LikeCode := lcMultiple
            End;
          End
          Else
          Begin
            // el texto debe tener en medio work
            (*With LikeList.Add Do
            Begin
              LikeText := Work;
              LikePos := lpMiddle;
              If s[p] = '_' Then
                LikeCode := lcSingle
              Else
                LikeCode := lcMultiple
            End;*)
            with LikeList.Add Do begin
              LikePos := lpMiddle;
              if s[p] = '_' then begin
                LikeCode := lcSingle;
                LikeText := s;
              end else begin
                LikeText := Work;
                LikeCode := lcMultiple;
              end;
            end;
          End;
        End;
        work := '';
        inc( n );
        if s[p] = '_' then Break; { patched by ccy }
      End
      Else
      Begin
        If ( EscapeChar = #0 ) Or Not ( s[p] = EscapeChar ) Then
          work := work + s[p];
      End;
      Previous := s[p];

      Inc( p );
    End;
    If Length( work ) > 0 Then
    Begin
      { texto deber terminar en Work }
      With LikeList.Add Do
      Begin
        LikePos := lpRight;
        LikeText := Work;
        If s[1] = '_' Then           { patched by ccy }
          LikeCode := lcSingle       { patched by ccy }
        else                         { patched by ccy }
          LikeCode := lcMultiple     { patched by ccy }
      End;
    End;
  End;
End;

Destructor TSQLLikeExpr.Destroy;
Begin
  LIKEList.Free;
  Inherited Destroy;
End;

Function TSQLLikeExpr.SQLPos( Var Start: Integer; Const Substr, Str: String ): Integer;
Var
  I, Pivot, NumValid, L1, L2: Integer;
  Accept: Boolean;
Begin
  Result := Low( Integer );
  L1 := Length( Str );
  L2 := Length( Substr );
  If ( L1 = 0 ) Or ( L2 = 0 ) Or ( L2 > L1 ) Then Exit;
  If ( Start = 1 ) And ( Pos( '_', Substr ) = 0 ) Then
  Begin
    Result := Pos( Substr, Str ); // speed up result
    If Result > 0 Then
      Inc( Start, Length( Substr ) );
  End Else
  Begin
    For I := Start To L1 Do
    Begin
      NumValid := 0;
      Pivot := 1;
      Accept := true;
      While Accept And ( I + Pivot - 1 <= L1 ) And ( Pivot <= L2 ) And
        ( ( Substr[Pivot] = '_' ) Or ( Str[I + Pivot - 1] = Substr[Pivot] ) ) Do
      Begin
        Inc( NumValid );
        Inc( Pivot );
      End;
      If NumValid = L2 Then
      Begin
        Inc( Start, Length( Substr ) );
        Result := I;
        Exit;
      End;
    End;
  End;
  If Result = 0 Then
    Result := Low( Integer );
End;

Procedure TSQLLikeExpr.AddToList( Like: TLikeItem );
Begin
  With LikeList.Add Do
  Begin
    LikePos := Like.LikePos;
    LikeCode := Like.LikeCode;
    LikeText := Like.LikeText;
  End;
End;

Function TSQLLikeExpr.GetAsBoolean: Boolean;
Var
  I, n, Start, p: Integer;
  Like: TLikeItem;
  s0, s1: String;
  Accept: Boolean;
Begin
  s0 := Param[0].AsString;
  Start := 1;
  if LIKEList.Count=1 then
  begin
    Like := LIKEList[0];
    if Like.LikeCode=lcOnlyUnderscores then
    begin
      s1 := Like.LikeText;
      Accept := ( SQLPos( Start, s1, s0 ) = 1 );
      If Accept And ( Length( s1 ) <> Length( s0 ) ) Then
        Accept := false;
      Result := Accept;
      If FIsNotLike Then
        Result := Not Result;
      exit;
    end;
  end;

  n := 0;
  Accept := False;
  For I := 0 To LIKEList.Count - 1 Do
  Begin
    Like := LIKEList[I];
    s1 := Like.LikeText;
    Case Like.LikePos Of
      lpNone: Accept := ( s0 = s1 );
      lpLeft:
        Begin
          Start := 1;
          If Like.LikeCode = lcSingle Then
            s1 := s1 + '_';
          Accept := ( SQLPos( Start, s1, s0 ) = 1 );
          If Accept And ( Like.LikeCode = lcSingle ) And
            ( Length( s1 ) <> Length( s0 ) ) Then
            Accept := false;
        End;
      lpMiddle:
        Accept := ( SQLPos( Start, s1, s0 ) > 0 );
      lpRight:
        Begin
          p := Length( s0 ) - Length( s1 ) + 1;
          If Start <= p Then
          Begin
            Start := p;
//            If Like.LikeCode = lcSingle Then   { patched by ccy }
//              s1 := '_' + s1;                  { patched by ccy }
            Accept := ( SQLPos( Start, s1, s0 ) = p );
            If Accept And ( Like.LikeCode = lcSingle ) And
              ( Length( s1 ) <> Length( s0 ) ) Then
              Accept := false;
          End
          Else
            Accept := False;
        End;
    End;
    If Accept Then
      Inc( n );
  End;
  Result := ( n = LIKEList.Count );
  If FIsNotLike Then
    Result := Not Result;
End;

function TSQLLikeExpr.GetExprType: TExprtype;
Begin
  Result := ttBoolean;
End;

{ TBetweenExpr }

Constructor TBetweenExpr.Create( ParameterList: TParameterList; IsNotBetween: Boolean );
Begin
  Inherited Create( ParameterList );
  FIsNotBetween := IsNotBetween;
End;

Function TBetweenExpr.GetAsBoolean: Boolean;
Var
  s: String;
  f: Double;
  i: Integer;
  b: Boolean;
Begin
  Result := False;
  { We'll compare expressions like
      CustNo BETWEEN 10 AND 30
  }
  Case Param[0].Exprtype Of
    ttString:
      Begin
        s := Param[0].AsString;
        result := ( s >= Param[1].AsString ) And ( s <= Param[2].AsString );
      End;
    ttFloat:
      Begin
        f := Param[0].AsFloat;
        result := ( f >= Param[1].AsFloat ) And ( f <= Param[2].AsFloat );
      End;
    ttInteger:
      Begin
        i := Param[0].AsInteger;
        result := ( i >= Param[1].AsInteger ) And ( i <= Param[2].AsInteger );
      End;
    ttBoolean:
      Begin
        b := Param[0].AsBoolean;
        result := ( b >= Param[1].AsBoolean ) And ( b <= Param[2].AsBoolean );
      End;
  End;
  If FIsNotBetween Then
    Result := Not Result;
End;

function TBetweenExpr.GetExprType: TExprtype;
Begin
  Result := ttBoolean;
End;

{ TSQLInPredicateExpr - class implementation}

Constructor TSQLInPredicateExpr.Create( ParameterList: TParameterList; IsNotIn: Boolean );
Begin
  Inherited Create( ParameterList );
  FIsNotIn := IsNotIn;
End;

Function TSQLInPredicateExpr.GetAsBoolean: Boolean;
Var
  t: Integer;
  s: String;
  f: Double;
  i: Integer;
  b: Boolean;
Begin
  Result := False;
  { We'll compare expressions like :
      COUNTRY IN ('USA','SPAIN','MEXICO','ENGLAND')
      CUSTID not IN (1,10,25)
      ISMARRIED IN (TRUE)
      Combination of parameters like:
      CUSTID IN ('USA', 2, 'MEXICO', 2.54)
      where CUSTID is integer, is invalid
  }
  Case Param[0].Exprtype Of
    ttString:
      Begin
        s := Param[0].AsString;
        For t := 1 To ParameterCount - 1 Do
          If s = Param[t].AsString Then
          Begin
            Result := True;
            Break;
          End;
      End;
    ttFloat:
      Begin
        f := Param[0].AsFloat;
        For t := 1 To ParameterCount - 1 Do
          If f = Param[t].AsFloat Then
          Begin
            Result := True;
            Break;
          End;
      End;
    ttInteger:
      Begin
        i := Param[0].AsInteger;
        For t := 1 To ParameterCount - 1 Do
          If i = Param[t].AsInteger Then
          Begin
            Result := True;
            Break;
          End;
      End;
    ttBoolean:
      Begin
        b := Param[0].AsBoolean;
        For t := 1 To ParameterCount - 1 Do
          If b = Param[t].AsBoolean Then
          Begin
            Result := True;
            Break;
          End;
      End;
  End;
  If FIsNotIn Then
    Result := Not Result;
End;

function TSQLInPredicateExpr.GetExprType: TExprtype;
Begin
  Result := ttBoolean;
End;

{ TCaseWhenElseExpr }

Constructor TCaseWhenElseExpr.Create( WhenParamList: TParameterList;
  ThenParamList: TParameterList; ElseExpr: TExpression );
Begin
  Inherited Create( WhenParamList );
  FThenParamList := ThenParamList;
  FElseExpr := ElseExpr;
End;

Procedure TCaseWhenElseExpr.CheckParameters;
Var
  I: Integer;
Begin
  { check that WHEN expression be of type boolean}
  For I := 0 To ParameterCount - 1 Do
  Begin
    If Param[I].ExprType <> ttBoolean Then
      Raise EExpression.Create( SEXPR_WRONGWHENEXPR );
  End;
  { check that all expression in THEN be of same type }
  For I := 1 To FThenParamList.Count - 1 Do
  Begin
    If Not FThenParamList.Param[I].CanReadAs( FThenParamList.Param[0].ExprType ) Then
      Raise EExpression.Create( SEXPR_WRONGTHENEXPR );
  End;
  If ( FElseExpr <> Nil ) And Not FElseExpr.CanReadAs( FThenParamList.Param[0].ExprType ) Then
    Raise EExpression.Create( SEXPR_WRONGTHENEXPR );
End;

Destructor TCaseWhenElseExpr.Destroy;
Begin
  FThenParamList.Free;
  If FElseExpr <> Nil Then
    FElseExpr.Free;
  Inherited Destroy;
End;

Function TCaseWhenElseExpr.GetAsBoolean: Boolean;
Var
  I: Integer;
Begin
  CheckParameters;
  Result := FALSE;
  For I := 0 To ParameterCount Do
    If Param[I].AsBoolean Then
    Begin
      Result := FThenParamList.AsBoolean[I];
      Exit;
    End;
  If FElseExpr <> Nil Then
    Result := FElseExpr.AsBoolean;
End;

Function TCaseWhenElseExpr.GetAsFloat: Double;
Var
  I: Integer;
Begin
  CheckParameters;
  Result := 0;
  For I := 0 To ParameterCount - 1 Do
    If Param[I].AsBoolean Then
    Begin
      Result := FThenParamList.AsFloat[I];
      Exit;
    End;
  If FElseExpr <> Nil Then
    Result := FElseExpr.AsFloat;
End;

Function TCaseWhenElseExpr.GetAsInteger: Integer;
Var
  I: Integer;
Begin
  CheckParameters;
  Result := 0;
  For I := 0 To ParameterCount Do
    If Param[I].AsBoolean Then
    Begin
      Result := FThenParamList.AsInteger[I];
      Exit;
    End;
  If FElseExpr <> Nil Then
    Result := FElseExpr.AsInteger;
End;

Function TCaseWhenElseExpr.GetMaxString: String;
var
  I: Integer;
Begin
  Result:= '';
  if not (GetExprType = ttString) then Exit;
  For I := 0 To ParameterCount - 1 Do
    if Length( FThenParamList.AsString[I] ) > Length( Result ) then
      Result:= FThenParamList.AsString[I];
  If ( FElseExpr <> Nil ) And ( Length( FElseExpr.AsString ) > Length( Result ) ) then
    Result := FElseExpr.AsString;
End;

Function TCaseWhenElseExpr.GetAsString: String;
Var
  I: Integer;
Begin
  CheckParameters;
  Result := '';
  For I := 0 To ParameterCount - 1 Do
    If Param[I].AsBoolean Then
    Begin
      Result := FThenParamList.AsString[I];
      Exit;
    End;
  If FElseExpr <> Nil Then
    Result := FElseExpr.AsString;
End;

function TCaseWhenElseExpr.GetExprType: TExprtype;
Begin
  { the expression type is the type of the first expression }
  Result := FThenParamList.ExprType[0];
End;

{TFormatDateTimeExpr - class implementation}
Function TFormatDateTimeExpr.GetMaxString: String;
Begin
  Result := FormatDateTime( Param[0].GetMaxString, Param[1].AsFloat );
End;

Function TFormatDateTimeExpr.GetAsString: String;
Begin
  Result := FormatDateTime( Param[0].AsString, Param[1].AsFloat );
End;

Function TFormatDateTimeExpr.GetExprType: TExprType;
Begin
  Result := ttString;
End;

{TFormatFloatExpr - class implementation
 FORMATFLOAT('###,###,##0.00', 32767)}

Function TFormatFloatExpr.GetMaxString: String;
Begin
  Result := FormatFloat( Param[0].GetMaxString, Param[1].AsFloat );
End;

Function TFormatFloatExpr.GetAsString: String;
Begin
  Result := FormatFloat( Param[0].AsString, Param[1].AsFloat );
End;

Function TFormatFloatExpr.GetExprType: TExprType;
Begin
  Result := ttString;
End;

{ TFormatExpr - class implementation
  Format('%d %s ...', 1234, 'ABC', ..., etc) }
Function TFormatExpr.GetMaxString: String;
Const
  MAXARGS = 20; {maximum number of arguments allowed (increase if needed)}
Var
  cnt, n: integer;
  ss: Array[0..MAXARGS] Of String;
  ea: Array[0..MAXARGS] Of Extended;
  Vars: Array[0..MAXARGS] Of TVarRec;
Begin
  n := imin( ParameterCount - 1, MAXARGS );
  { first parameter is the format string and the rest are the args}
  For cnt := 1 To n Do
  Begin
    Case Param[cnt].ExprType Of
      ttString:
        Begin
          Vars[cnt - 1].VType := vtString;
          ss[cnt - 1] := Param[cnt].AsString;
          Vars[cnt - 1].VString := @ss[cnt - 1];
        End;
      ttFloat:
        Begin
          Vars[cnt - 1].VType := vtExtended;
          ea[cnt - 1] := Param[cnt].AsFloat;
          Vars[cnt - 1].VExtended := @ea[cnt - 1];
        End;
      ttInteger:
        Begin
          Vars[cnt - 1].VType := vtInteger;
          Vars[cnt - 1].VInteger := Param[cnt].AsInteger;
        End;
      ttBoolean:
        Begin
          Vars[cnt - 1].VType := vtBoolean;
          Vars[cnt - 1].VBoolean := Param[cnt].AsBoolean;
        End;
    End;
  End;
  result := Format( Param[0].GetMaxString, Vars );
End;

Function TFormatExpr.GetAsString: String;
Const
  MAXARGS = 20; {maximum number of arguments allowed (increase if needed)}
Var
  cnt, n: integer;
  ss: Array[0..MAXARGS] Of String;
  ea: Array[0..MAXARGS] Of Extended;
  Vars: Array[0..MAXARGS] Of TVarRec;
  SFS: xqmiscel.TSaveFormatSettings;
Begin
  n := imin( ParameterCount - 1, MAXARGS );
  { first parameter is the format string and the rest are the args}
  For cnt := 1 To n Do
  Begin
    Case Param[cnt].ExprType Of
      ttString:
        Begin
          Vars[cnt - 1].VType := vtString;
          ss[cnt - 1] := Param[cnt].AsString;
          Vars[cnt - 1].VString := @ss[cnt - 1];
        End;
      ttFloat:
        Begin
          Vars[cnt - 1].VType := vtExtended;
          ea[cnt - 1] := Param[cnt].AsFloat;
          Vars[cnt - 1].VExtended := @ea[cnt - 1];
        End;
      ttInteger:
        Begin
          Vars[cnt - 1].VType := vtInteger;
          Vars[cnt - 1].VInteger := Param[cnt].AsInteger;
        End;
      ttBoolean:
        Begin
          Vars[cnt - 1].VType := vtBoolean;
          Vars[cnt - 1].VBoolean := Param[cnt].AsBoolean;
        End;
    End;
  End;
  SFS := SaveFormatSettings;
  try
    SysUtils.GetFormatSettings; {Reset to windows settings}
    result := Format( Param[0].AsString, Vars );
  finally
    RestoreFormatSettings(SFS);
  end;
End;

Function TFormatExpr.GetExprType: TExprType;
Begin
  result := ttString;
End;

// TDecodeDateTimeExpr implementation

Constructor TDecodeDateTimeExpr.Create( ParameterList: TParameterList;
  DecodeKind: TDecodeKind );
Begin
  Inherited Create( ParameterList );
  FDecodeKind := DecodeKind;
End;

Function TDecodeDateTimeExpr.GetExprType: TExprType;
Begin
  Result := ttInteger;
End;

Function TDecodeDateTimeExpr.GetAsInteger: Integer;
Var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
Begin

  Result := 0;
  Case FDecodeKind Of
    dkYear, dkMonth, dkDay: DecodeDate( Param[0].AsFloat, Year, Month, Day );
    dkHour, dkMin, dkSec, dkMSec:
      DecodeTime( Param[0].AsFloat, Hour, Min, Sec, MSec );
  End;
  Case FDecodeKind Of
    dkYear: Result := Year;
    dkMonth: Result := Month;
    dkDay: Result := Day;
    dkHour: Result := Hour;
    dkMin: Result := Min;
    dkSec: Result := Sec;
    dkMSec: Result := MSec;
  End;
End;

{ TDecodeExpr
  DECODE('abc', 'a', 1,
                'b', 2,
                'abc', 3,
                'd', 4,
                -1 )
 }

Constructor TDecodeExpr.Create( ParameterList: TParameterList );
Begin
  Inherited Create( ParameterList );
  { check for valid expressions }
  If ( ParameterList = Nil ) Or ( ( ParameterList.Count Mod 2 ) <> 0 ) Then
    Raise EExpression.Create( 'DECODE: Incorrect number of arguments' );
End;

Function TDecodeExpr.GetAsBoolean: Boolean;
Var
  I: Integer;
  Found: Boolean;
Begin
  Result := False;
  Found := false;
  I := 1;
  While I < ParameterCount - 1 Do
  Begin
    Case Param[0].ExprType Of
      ttString:
        If Param[0].AsString = Param[I].AsString Then
          Found := true;
      ttFloat:
        If Param[0].AsFloat = Param[I].AsFloat Then
          Found := true;
      ttInteger:
        If Param[0].AsInteger = Param[I].AsInteger Then
          Found := true;
      ttBoolean:
        If Param[0].AsBoolean = Param[I].AsBoolean Then
          Found := true;
    End;
    If found Then
    Begin
      Result := Param[I + 1].AsBoolean;
      break;
    End;
    Inc( I, 2 );
  End;
  If Not found Then
    Result := Param[ParameterCount - 1].AsBoolean;
End;

Function TDecodeExpr.GetAsFloat: Double;
Var
  I: Integer;
  Found: Boolean;
Begin
  Result := 0;
  Found := false;
  I := 1;
  While I < ParameterCount - 1 Do
  Begin
    Case Param[0].ExprType Of
      ttString:
        If Param[0].AsString = Param[I].AsString Then
          Found := true;
      ttFloat:
        If Param[0].AsFloat = Param[I].AsFloat Then
          Found := true;
      ttInteger:
        If Param[0].AsInteger = Param[I].AsInteger Then
          Found := true;
      ttBoolean:
        If Param[0].AsBoolean = Param[I].AsBoolean Then
          Found := true;
    End;
    If found Then
    Begin
      Result := Param[I + 1].AsFloat;
      break;
    End;
    Inc( I, 2 );
  End;
  If Not found Then
    Result := Param[ParameterCount - 1].AsFloat;
End;

Function TDecodeExpr.GetAsInteger: Integer;
Var
  I: Integer;
  Found: Boolean;
Begin
  Result := 0;
  Found := false;
  I := 1;
  While I < ParameterCount - 1 Do
  Begin
    Case Param[0].ExprType Of
      ttString:
        If Param[0].AsString = Param[I].AsString Then
          Found := true;
      ttFloat:
        If Param[0].AsFloat = Param[I].AsFloat Then
          Found := true;
      ttInteger:
        If Param[0].AsInteger = Param[I].AsInteger Then
          Found := true;
      ttBoolean:
        If Param[0].AsBoolean = Param[I].AsBoolean Then
          Found := true;
    End;
    If found Then
    Begin
      Result := Param[I + 1].AsInteger;
      break;
    End;
    Inc( I, 2 );
  End;
  If Not found Then
    Result := Param[ParameterCount - 1].AsInteger;
End;

Function TDecodeExpr.GetMaxString: String;
var
  I, L, MaxL: Integer;
Begin
  L := 2;
  MaxL := Length( Param[L].AsString );
  I := 2;
  While I <= ParameterCount - 1 Do
  Begin
    If Length( Param[I].AsString ) > MaxL Then
    Begin
      L := I;
      MaxL := Length( Param[I].AsString );
    End;
    Inc( I, 2 );
  End;
  Result := Param[L].AsString;
End;

Function TDecodeExpr.GetAsString: String;
Var
  I: Integer;
  Found: Boolean;
Begin
  Found := False;
  I := 1;
  While I < ParameterCount - 1 Do
  Begin
    Case Param[0].ExprType Of
      ttString:
        If Param[0].AsString = Param[I].AsString Then
          Found := true;
      ttFloat:
        If Param[0].AsFloat = Param[I].AsFloat Then
          Found := true;
      ttInteger:
        If Param[0].AsInteger = Param[I].AsInteger Then
          Found := true;
      ttBoolean:
        If Param[0].AsBoolean = Param[I].AsBoolean Then
          Found := true;
    End;
    If found Then
    Begin
      Result := Param[I + 1].AsString;
      break;
    End;
    Inc( I, 2 );
  End;
  If Not found Then
    Result := Param[ParameterCount - 1].AsString;
End;

function TDecodeExpr.GetExprType: TExprtype;
Begin
  Result := Param[2].ExprType;
End;

{ MINOF, MAXOF functions support}

Constructor TMinMaxOfExpr.Create( ParameterList: TParameterList; IsMin: Boolean );
Begin
  Inherited Create( ParameterList );
  { check for valid expressions }
  If ( ParameterList = Nil ) Or ( ParameterList.Count < 1 ) Then
    Raise EExpression.Create( 'MINOF, MAXOF: Incorrect number of arguments' );
  FIsMin := IsMin;
End;

Function TMinMaxOfExpr.GetAsFloat: Double;
Var
  i: Integer;
Begin
  Result := Param[0].AsFloat;
  For i := 1 To ParameterCount - 1 Do
  Begin
    If FIsMin Then
      Result := Min( Result, Param[i].AsFloat )
    Else
      Result := Max( Result, Param[i].AsFloat );
  End;
End;

function TMinMaxOfExpr.GetExprType: TExprtype;
Begin
  Result := ttFloat;
End;

End.
