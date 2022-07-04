/* Grammar for Expression Parser (c) 2001 Alfonso Moreno
   NOTES : DON'T FORGET TO MOVE THE GENERATED CONSTANTS TO THE PLACE INDICATED
*/

%{
{**********************************************}
{   Expression Parser                          }
{   Copyright (C) 2003 by Alfonso Moreno       }
{**********************************************}
unit QExprYacc;

{$I xq_flag.INC}
{$R Qexpryacc.res}
interface

uses
  SysUtils, Classes, Windows, Dialogs, QYaccLib, QBaseExpr, Db
(*$IFDEF LEVEL6*)
  , Variants
(*$ENDIF*)
  ;

const
  MAX_INDEXED_FIELDS = 10;

type

  TReferencedDatasetList = Class;

  TReferencedDataSetItem = Class
  Private
    FReferencedDataSets: TReferencedDataSetList;
    FDataSet: TDataSet;
    FCount: Integer;
  Public
    Constructor Create( RefDataSetList: TReferencedDataSetList );
    Property DataSet: TDataSet Read fDataSet Write fDataSet;
    Property Count: Integer Read fCount Write fCount;
  End;

  TReferencedDataSetList = Class
    FItems: TList;
    Function GetCount: Integer;
    Function GetItem( Index: Integer ): TReferencedDataSetItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add: TReferencedDataSetItem;
    Procedure Clear;
    Procedure Delete( Index: Integer );
    Function IndexOf( DataSet: TDataSet ): Integer;

    Property Count: Integer Read GetCount;
    Property Items[Index: Integer]: TReferencedDataSetItem Read GetItem; Default;
  End;

  TCheckData = Record
    Field: TField; // the field referenced
    RefCount: Integer; // No of references to a field
    FieldCount: Integer; // number of fields referenced in expression
    Fields: Array[1..MAX_INDEXED_FIELDS] Of TField; // the fields referenced (used for joining)
    HasMorefunctions: Boolean; // expression has more functions
  End;

  TExprParser = class(TCustomParser)
  private
    fDefaultDataSet: TDataSet;
    fAnalizer: TObject;
    fIdReferences: TStrings;
    fReferencedDataSets: TReferencedDataSetList;

    FExprList: TList;
    FTempParams: TParameterList;
    fGroupIdent: string;
    fIdentifier: string;
    fGroupIdentList: TStringList;
    fIdentifierList: TStringList;
    { is not this a simple expression ? like TABLE.FIELD
      this is used for detecting if giving the same data type to the
      result set as the original field }
    fIsComplex: Boolean;
    { used to obtain a pair of operators }
    Op1, Op2: TExpression;
    { a stacked list of params referencing to FExprList }
    fStackedParamCount: TList;
    { the number of parameters for the last function }
    fParamCount: Integer;
    { for the case..when..else..end }
    fWhenParamList: TParameterList;
    fThenParamList: TParameterList;
    fElseExpr: TExpression;
    { used in unknown identifiers }
    IDF: TExpression;
    Procedure IDFunc( Sender: TObject; Const Group, Identifier: String;
      ParameterList: TParameterList; Var Expression: TExpression );
    Function GetExplicitParam( const ParamName: string ): string;
    function AddExpression(Expression: TExpression): TExpression;
    function GetParamList: TParameterList;
    function ForceParamList(Count: Integer): TParameterList;
    procedure GetTwoOperators;
    procedure GetOneOperator;
    procedure AddParam;
    function GetString( const s: string ): string;
  public
    SubqueryExpr: TExpression; // used for subqueries only (special case)
    CheckData: TCheckData; // used when checking expression
    Expression: TExpression; // the real expression to evaluate

    constructor Create(SqlAnalizer: TObject; DataSet: TDataSet);
    destructor Destroy; override;
    function yyparse : integer; override;
    function GetExpression: TExpression;
    Procedure ParseExpression( Const ExprStr: String );
    Function CheckExpression( Const ExprStr: String ): Boolean;

    Property ReferencedDataSets: TReferencedDataSetList Read fReferencedDataSets Write fReferencedDataSets;
    Property IdReferences: TStrings Read FIdReferences;

    property IsComplex: Boolean read FIsComplex write FIsComplex;
  end;

//
// The generated constants must be placed here
// HERE !!!!
//

implementation

uses
  xquery, xqmiscel, Math, xqconsts, qlexlib, QExprLex, xqbase;

Constructor TReferencedDataSetItem.Create( RefDataSetList:
  TReferencedDataSetList );
Begin
  Inherited Create;
  fReferencedDataSets := RefDataSetList;
End;

Constructor TReferencedDataSetList.Create;
Begin
  Inherited Create;
  fItems := TList.Create;
End;

Destructor TReferencedDataSetList.Destroy;
Begin
  Clear;
  fItems.Free;
  Inherited Destroy;
End;

Function TReferencedDataSetList.GetCount: Integer;
Begin
  Result := fItems.Count;
End;

Function TReferencedDataSetList.GetItem( Index: Integer ): TReferencedDataSetItem;
Begin
  Result := fItems[Index];
End;

Function TReferencedDataSetList.Add: TReferencedDataSetItem;
Begin
  Result := TReferencedDataSetItem.Create( Self );
  fItems.Add( Result );
End;

Procedure TReferencedDataSetList.Clear;
Var
  I: Integer;
Begin
  For I := 0 To fItems.Count - 1 Do
    TReferencedDataSetItem( fItems[I] ).Free;
  fItems.Clear;
End;

Procedure TReferencedDataSetList.Delete( Index: Integer );
Begin
  TReferencedDataSetItem( fItems[Index] ).Free;
  fItems.Delete( Index );
End;

Function TReferencedDataSetList.IndexOf( DataSet: TDataSet ): Integer;
Var
  I: Integer;
Begin
  result := -1;
  For I := 0 To fItems.Count - 1 Do
    If TReferencedDataSetItem( fItems[I] ).Dataset = Dataset Then
    Begin
      result := I;
      Exit;
    End;
End;


Type

  TExplicitParamExpr = Class( TExpression )
  Private
    fAnalizer: TSqlAnalizer;
    fParam: TParam;
  Protected
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprtype; Override;
    Function GetIsNull: boolean; Override;
  Public
    Constructor Create( Analizer: TSqlAnalizer; Param: TParam );
  End;

  TFieldExpr = Class( Tfunction )
  Private
    FField: TField;
    FxQuery: TCustomxQuery;
    FParser: TExprParser;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprtype; Override;
    Function GetIsNull: boolean; Override;
  Public
    Constructor Create( ParameterList: TParameterList;
      F: TField; xQuery: TCustomxQuery; Parser: TExprParser );
    Property Field: TField Read FField;
  End;

  TResultSetFieldExpr = Class( TFunction )
  Private
    fxqField: TxqField;
  Protected
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprType: TExprtype; Override;
    Function GetIsNull: boolean; Override;
  Public
    Constructor Create( ParameterList: TParameterList; xqField: TxqField );
  End;

  TStrToDateExpr = Class( Tfunction )
  Protected
    Function GetAsFloat: Double; Override;
    Function GetExprtype: TExprtype; Override;
  End;

  TNowExpr = Class( Tfunction )
  Protected
    Function GetAsFloat: Double; Override;
    Function GetExprtype: TExprtype; Override;
  End;

  TSQLTrimExpr = Class( Tfunction )
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetExprtype: TExprtype; Override;
  End;

  TRoundDecExpr = Class( Tfunction )
  Private
    FIsRound: Boolean;
  Protected
    Function GetAsFloat: Double; Override;
    Function GetExprtype: TExprtype; Override;
  Public
    Constructor Create( ParameterList: TParameterList; IsRound: Boolean );
  End;

  TUDFExpr = Class( Tfunction )
  Private
    FxQuery: TCustomXQuery;
    FIdentifier: String;
    FResulttype: TExprtype;
    FParams: TParameterList;
    FMaxLen: Integer;
  Protected
    Function GetMaxString: String; Override;
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprtype: TExprtype; Override;
  Public
    Constructor Create( ParameterList: TParameterList; xQuery: TCustomXQuery;
      Const Identifier: String; Resulttype: TExprtype; MaxLen: Integer );
  End;

  TISNULLExpr = Class( Tfunction )
  Protected
    Function GetAsBoolean: Boolean; Override;
    Function GetExprtype: TExprtype; Override;
  End;

  TNULLValueExpr = Class( Tfunction )
  Protected
    Function GetAsBoolean: Boolean; Override;
    Function GetExprtype: TExprtype; Override;
  End;

  TFilterFieldExpr = Class( Tfunction )
  Private
    fField: TField;
  Protected
    Function GetAsString: String; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Integer; Override;
    Function GetAsBoolean: Boolean; Override;
    Function GetExprtype: TExprtype; Override;
  Public
    Constructor Create( ParameterList: TParameterList; F: TField );
  End;

Constructor TFieldExpr.Create( ParameterList: TParameterList;
  F: TField; xQuery: TCustomxQuery; Parser: TExprParser );
Begin
  Inherited Create( ParameterList );
  fField := F;
  fxQuery := xQuery;
  fParser := Parser;
End;

Function TFieldExpr.GetExprType: TExprtype;
Begin
  If Not(fField.Datatype In [ftMemo, ftFmtMemo]) And (fField.Datatype In ftNonTexttypes) Then
    Result := ttInteger
  Else
  Begin
    Case fField.Datatype Of
      ftString, ftMemo, ftFmtMemo(*$IFDEF LEVEL4*), ftFixedChar, ftWideString(*$ENDIF*)
(*$IFDEF LEVEL5*), ftGUID(*$ENDIF*):
        Result := ttString;
      ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftLargeInt:
        Result := ttFloat;
      ftAutoInc, ftSmallInt, ftInteger, ftWord :
        Result := ttInteger;
      ftBoolean:
        Result := ttBoolean;
    Else
      result := ttString;
    End;
  End;
End;

Function TFieldExpr.GetMaxString: String;
Begin
  If Not( fField.Datatype In [ftMemo, ftFmtMemo] ) And
        ( fField.DataType In ftNonTextTypes ) Then
  begin
  end else
  Begin
    If ( fField.DataType In [ftString, ftMemo, ftFmtMemo(*$IFDEF LEVEL4*), ftFixedChar,
      ftWideString(*$ENDIF*)
(*$IFDEF LEVEL5*), ftGUID(*$ENDIF*)] ) Then
    Begin
      if fField.DataType In [ftMemo, ftFmtMemo] then
      begin
        Result:= StringOfChar( 'X', 10 );
      end else
      begin
        Result:= StringOfChar( 'X', fField.Size );
      end;
    End;
  End;
End;

Function TFieldExpr.GetAsString: String;
Begin
  Result := '';
  If FxQuery.IsDataSetDisabled( fField.DataSet ) Then Exit;

  If Not( fField.Datatype In [ftMemo, ftFmtMemo] ) And
        ( fField.DataType In ftNonTextTypes ) Then
  begin
  end else
  Begin
    Result := fField.AsString;
  End;
End;

Function TFieldExpr.GetAsFloat: Double;
Begin
  Result := 0;
  If FxQuery.IsDataSetDisabled( fField.DataSet ) Then
    Exit;
  Result := fField.AsFloat;
End;

Function TFieldExpr.GetAsInteger: Integer;
Begin
  Result := 0;
  If FxQuery.IsDataSetDisabled( fField.DataSet ) Then
    Exit;
  Result := fField.AsInteger;
End;

Function TFieldExpr.GetAsBoolean: Boolean;
Begin
  Result := False;
  If FxQuery.IsDataSetDisabled( fField.DataSet ) Then
    Exit;
  Result := fField.AsBoolean;
End;

//unley

Function TFieldExpr.GetIsNull: boolean;
Begin
  If FxQuery.IsDataSetDisabled( fField.DataSet ) Then
  Begin
    Result := True;
    Exit;
  End;
  Result := fField.IsNull;
End;

//
Constructor TResultSetFieldExpr.Create( ParameterList: TParameterList; xqField: TxqField );
Begin
  Inherited Create( ParameterList );
  fxqField := xqField;
End;

Function TResultSetFieldExpr.GetAsString: String;
Begin
  Result := fxqField.AsString;
End;

Function TResultSetFieldExpr.GetAsFloat: Double;
Begin
  Result := fxqField.AsFloat;
End;

Function TResultSetFieldExpr.GetAsInteger: Integer;
Begin
  Result := fxqField.AsInteger;
End;

Function TResultSetFieldExpr.GetAsBoolean: Boolean;
Begin
  Result := fxqField.AsBoolean;
End;

Function TResultSetFieldExpr.GetExprType: TExprtype;
Begin
  Result := fxqField.DataType;
End;

Function TResultSetFieldExpr.GetIsNull: boolean;
Begin
  Result := fxqField.IsNull;
End;

//TNowExpr

function TNowExpr.GetAsFloat: Double;
begin
  Result := Now;
end;

function TNowExpr.GetExprtype: TExprtype;
begin
  Result:= ttFloat;
end;

//TStrToDate

Function TStrToDateExpr.GetAsFloat: Double;
Begin
  Try
    Result := StrToDate( Param[0].AsString );
  Except
    On E: Exception Do
    Begin
      MessageToUser( E.Message, mtError );
      Result := 0;
    End;
  End;
End;

Function TStrToDateExpr.GetExprtype: TExprtype;
Begin
  Result := ttFloat;
End;

//TSQLTrimExpr

Function SQLTrim( trimmed_char: char; Const S: String ): String;
Var
  I, L: Integer;
Begin
  L := Length( S );
  I := 1;
  While ( I <= L ) And ( S[I] = trimmed_char ) Do
    Inc( I );
  If I > L Then
    Result := ''
  Else
  Begin
    While S[L] = trimmed_char Do
      Dec( L );
    Result := Copy( S, I, L - I + 1 );
  End;
End;

Function SQLTrimLeft( trimmed_char: char; Const S: String ): String;
Var
  I, L: Integer;
Begin
  L := Length( S );
  I := 1;
  While ( I <= L ) And ( S[I] = trimmed_char ) Do
    Inc( I );
  Result := Copy( S, I, Maxint );
End;

Function SQLTrimRight( trimmed_char: char; Const S: String ): String;
Var
  I: Integer;
Begin
  I := Length( S );
  While ( I > 0 ) And ( S[I] = trimmed_char ) Do
    Dec( I );
  Result := Copy( S, 1, I );
End;

Function TSQLTrimExpr.GetAsString: String;
Begin
  Case Param[2].AsInteger Of
    0: // leading
      Result := SQLTrimLeft( Param[0].AsString[1], Param[1].MaxString );
    1: // trailing
      Result := SQLTrimRight( Param[0].AsString[1], Param[1].MaxString );
    2: // both
      Result := SQLTrim( Param[0].AsString[1], Param[1].MaxString );
  End;
End;

Function TSQLTrimExpr.GetMaxString: String;
Begin
  Case Param[2].AsInteger Of
    0: // leading
      Result := SQLTrimLeft( Param[0].AsString[1], Param[1].GetMaxString );
    1: // trailing
      Result := SQLTrimRight( Param[0].AsString[1], Param[1].GetMaxsString );
    2: // both
      Result := SQLTrim( Param[0].AsString[1], Param[1].GetMaxString );
  End;
End;

Function TSQLTrimExpr.GetExprtype: TExprtype;
Begin
  Result := ttString;
End;

//TRoundDecExpr

Constructor TRoundDecExpr.Create( ParameterList: TParameterList;
  IsRound: Boolean );
Begin
  Inherited Create( ParameterList );
  FIsRound := IsRound;
End;

Function TRoundDecExpr.GetAsFloat: Double;
Begin
  If Param[1].AsInteger = 0 Then
    Result := Param[0].AsFloat
  Else
    Result := Param[0].AsFloat * IntPower( 10, Param[1].AsInteger );
  If FIsRound Then
    Result := Result + 0.5;
  If Param[1].AsInteger > 0 Then
    Result := Int( Result ) / IntPower( 10, Param[1].AsInteger );
End;

Function TRoundDecExpr.GetExprtype: TExprtype;
Begin
  Result := ttFloat;
End;

//TISNULLExpr

Function TISNULLExpr.GetAsBoolean: Boolean;
Begin
  Result := Param[0].IsNull;
  If Param[1].AsBoolean = False Then
    Result := Not Result;
End;

Function TISNULLExpr.GetExprtype: TExprtype;
Begin
  Result := ttBoolean;
End;

//TNULLValueExpr

Function TNULLValueExpr.GetAsBoolean: Boolean;
Begin
  Result := True;
End;

Function TNULLValueExpr.GetExprtype: TExprtype;
Begin
  Result := ttBoolean;
End;

//functions defined in Exfunctions property

Constructor TUDFExpr.Create( ParameterList: TParameterList; xQuery:
  TCustomXQuery; Const Identifier: String; Resulttype: TExprtype;
  MaxLen: Integer );
Begin
  Inherited Create( ParameterList );
  FxQuery := xQuery;
  FIdentifier := Identifier;
  FParams := ParameterList;
  FResulttype := Resulttype;
  FMaxLen := MaxLen;
End;

Function TUDFExpr.GetExprtype: TExprtype;
Begin
  Result := FResulttype;
End;

Function TUDFExpr.GetMaxString: String;
Var
  MxL: Integer;
Begin
  Result:= '';
  if Not (FResulttype = ttString) then Exit;
  MxL:= FMaxLen;
  if MxL = 0 then MxL := 1;
  Result:= StringOfChar( 'x', MxL );
End;

Function TUDFExpr.GetAsString: String;
Var
  Value: variant;
Begin
  Value := Null;
  Result := '';
  FxQuery.OnUDFSolve( FxQuery, FIdentifier, FParams, Value );
  If vartype( Value ) <> varNull Then
    Result := Value; // varAstype(Value, varString);
End;

Function TUDFExpr.GetAsFloat: Double;
Var
  Value: variant;
Begin
  Value := Null;
  Result := 0;
  FxQuery.OnUDFSolve( FxQuery, FIdentifier, FParams, Value );
  If vartype( Value ) <> varNull Then
    Result := Value; // varAstype(Value, varDouble);
End;

Function TUDFExpr.GetAsInteger: Integer;
Var
  Value: variant;
Begin
  Value := Null;
  Result := 0;
  FxQuery.OnUDFSolve( FxQuery, FIdentifier, FParams, Value );
  If vartype( Value ) <> varNull Then
    Result := Value; // varAstype(Value, varInteger);
End;

Function TUDFExpr.GetAsBoolean: Boolean;
Var
  Value: variant;
Begin
  Value := Null;
  Result := False;
  FxQuery.OnUDFSolve( FxQuery, FIdentifier, FParams, Value );
  If vartype( Value ) <> varNull Then
    Result := Value; // varAstype(Value, varBoolean);
End;

// TFilterFieldExpr

Constructor TFilterFieldExpr.Create( ParameterList: TParameterList; F: TField );
Begin
  Inherited Create( ParameterList );
  fField := F;
End;

Function TFilterFieldExpr.GetExprtype: TExprtype;
Begin
  If fField.Datatype In ftNonTexttypes Then
    Result := ttInteger
  Else
    Case fField.Datatype Of
      ftString(*$IFDEF LEVEL4*), ftFixedChar, ftWideString(*$ENDIF*)(*$IFDEF LEVEL5*),
      ftGUID(*$ENDIF*): Result := ttString;
      ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime: Result := ttFloat;
      ftAutoInc, ftSmallInt, ftInteger, ftWord
(*$IFNDEF LEVEL3*), ftLargeInt(*$ENDIF*): Result := ttInteger;
      ftBoolean: Result := ttBoolean;
    Else
      result := ttString;
    End;
End;

Function TFilterFieldExpr.GetAsString: String;
Begin
  Result := '';
  If Not ( fField.Datatype In ftNonTexttypes ) Then
    Result := fField.AsString;
End;

Function TFilterFieldExpr.GetAsFloat: Double;
Begin
  Result := fField.AsFloat;
End;

Function TFilterFieldExpr.GetAsInteger: Integer;
Begin
  Result := fField.AsInteger;
End;

Function TFilterFieldExpr.GetAsBoolean: Boolean;
Begin
  Result := fField.AsBoolean;
End;

// TExplicitParamExpr
Constructor TExplicitParamExpr.Create( Analizer: TSqlAnalizer; Param: TParam );
Begin
  Inherited Create;
  fAnalizer:= Analizer;
  fParam:= Param;
  if fParam = Nil then
    Raise ExQueryError.Create( SParameterNotFound );
End;

Function TExplicitParamExpr.GetExprType: TExprtype;
var
  DataType: TFieldType;
  DataSet: TDataSet;
  fld: TField;
Begin
  Result:= ttString;
  if fAnalizer.xquery.DataSource <> Nil then
  begin
    DataSet := fanalizer.xquery.DataSource.DataSet;
    If DataSet = Nil Then exit;
    if DataSet.Active = False Then DataSet.Open;
    fld := DataSet.FindField(fParam.Name);
    if fld = nil then Exit;
    DataType:= fld.DataType
  end else
    DataType:= fParam.DataType;
  If Not(Datatype In [ftMemo, ftFmtMemo]) And (fParam.Datatype In ftNonTexttypes) Then
    Result := ttInteger
  Else
  Begin
    Case Datatype Of
      ftString, ftMemo, ftFmtMemo(*$IFDEF LEVEL4*), ftFixedChar, ftWideString(*$ENDIF*)
(*$IFDEF LEVEL5*), ftGUID(*$ENDIF*): Result := ttString;
      ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime: Result := ttFloat;
      ftAutoInc, ftSmallInt, ftInteger, ftWord
(*$IFNDEF LEVEL3*), ftLargeInt(*$ENDIF*): Result := ttInteger;
      ftBoolean: Result := ttBoolean;
    Else
      result := ttString;
    End;
  End;
End;

Function TExplicitParamExpr.GetAsString: String;
var
  DataSet: TDataSet;
  fld: TField;
Begin
  Result:= '';
  if fAnalizer.xquery.DataSource <> Nil then
  begin
    DataSet := fanalizer.xquery.DataSource.DataSet;
    If DataSet = Nil Then exit;
    if DataSet.Active = False Then DataSet.Open;
    fld := DataSet.FindField(fParam.Name);
    if fld = nil then Exit;
    Result := fld.AsString;
  end else
    Result := fParam.AsString;
End;

Function TExplicitParamExpr.GetAsFloat: Double;
var
  DataSet: TDataSet;
  fld: TField;
Begin
  Result:= 0;
  if fAnalizer.xquery.DataSource <> Nil then
  begin
    DataSet := fanalizer.xquery.DataSource.DataSet;
    If DataSet = Nil Then exit;
    if DataSet.Active = False Then DataSet.Open;
    fld := DataSet.FindField(fParam.Name);
    if fld = nil then Exit;
    Result := fld.AsFloat;
  end else
    Result := fParam.AsFloat;
End;

Function TExplicitParamExpr.GetAsInteger: Integer;
var
  DataSet: TDataSet;
  fld: TField;
Begin
  Result:= 0;
  if fAnalizer.xquery.DataSource <> Nil then
  begin
    DataSet := fanalizer.xquery.DataSource.DataSet;
    If DataSet = Nil Then exit;
    if DataSet.Active = False Then DataSet.Open;
    fld := DataSet.FindField(fParam.Name);
    if fld = nil then Exit;
    Result := fld.AsInteger;
  end else
    Result := fParam.AsInteger;
End;

Function TExplicitParamExpr.GetAsBoolean: Boolean;
var
  DataSet: TDataSet;
  fld: TField;
Begin
  Result:= false;
  if fAnalizer.xquery.DataSource <> Nil then
  begin
    DataSet := fanalizer.xquery.DataSource.DataSet;
    If DataSet = Nil Then exit;
    if DataSet.Active = False Then DataSet.Open;
    fld := DataSet.FindField(fParam.Name);
    if fld = nil then Exit;
    Result := fld.AsBoolean;
  end else
    Result := fParam.AsBoolean;
End;

//unley
Function TExplicitParamExpr.GetIsNull: boolean;
Begin
  Result := fParam.IsNull;
End;


//
constructor TExprParser.Create(SqlAnalizer: TObject; DataSet: TDataSet);
begin
  inherited Create;
  fDefaultDataSet := DataSet;
  fAnalizer := SqlAnalizer;
  fIdReferences := TStringList.Create;

  FExprList:= TList.Create;
  FStackedParamCount:= TList.Create;
  FGroupIdentList:= TStringList.create;
  FIdentifierList:= TStringList.create;
end;

destructor TExprParser.Destroy;
var
  I: Integer;
begin
  Expression.Free;
  fIdReferences.Free;

  for I:= 0 to FExprList.Count-1 do
    TObject(FExprList[I]).Free;
  FExprList.Free;
  if FWhenParamList<>nil then
    FWhenParamList.Free;
  if FThenParamList<>nil then
    FThenParamList.Free;
  if FElseExpr<>nil then
    FElseExpr.Free;
  FStackedParamCount.Free;
  FGroupIdentList.free;
  FIdentifierList.free;
  inherited Destroy;
end;

Procedure TExprParser.IDFunc( Sender: TObject; Const Group, Identifier: String;
  ParameterList: TParameterList; Var Expression: TExpression );
Var
  TmpDataSet: TDataSet;
  FieldName, TableName: String;
  NumError, Idx: Integer;
  F: TField;
  Accept: Boolean;
  Params: TParameterList;
  xqField: TxqField;
  Datatype: TExprType;
  MaxLen: Integer;
Begin
  Expression := Nil;
  NumError := 0;
  // The fields will be found in the result set
  If ( fAnalizer <> Nil ) And ( Length( Group ) > 0 ) And
     ( Length( Identifier ) > 0 ) And ( ParameterList = Nil ) Then
  Begin
    With TSqlAnalizer(fAnalizer) Do
    Begin
      If Not WhereContainsOnlyBasicFields And ( JoinList.Count > 0 ) Then
      Begin
        If ResultSet <> Nil Then
        Begin
          // received table.field notation
          FieldName := Group + '.' + Identifier;
          xqField := TSqlAnalizer(fAnalizer).ResultSet.FindField( FieldName );
          If xqField <> Nil Then
            Expression := TResultSetFieldExpr.Create( ParameterList, xqField );
        End;
      End;
    End;
    If Expression = Nil Then
    Begin
      // if was not found on the result set, search on table.field datasets
      TableName := Group;
      TmpDataSet := TSqlAnalizer(fAnalizer).FindDataSetByName( TableName );
      If Not Assigned( TmpDataSet ) Then
        Raise EExpression.Create( Format( SWrongDataSetname, [TableName] ) );
      FieldName := Identifier;
      If TmpDataSet = fDefaultDataSet Then
        Inc( CheckData.RefCount ); // number of primary dataset referenced
      (*else                                    // number of other datasets referenced in the expression
         Inc(CheckData.OtherRefCount);*)
      // create the list of datasets referenced in the expression
      If Assigned( fReferencedDataSets ) Then
      Begin
        Idx := fReferencedDataSets.IndexOf( TmpDataSet );
        If Idx >= 0 Then
          fReferencedDataSets[Idx].Count := fReferencedDataSets[Idx].Count + 1
        Else
          With fReferencedDataSets.Add Do
          Begin
            DataSet := TmpDataSet;
            Count := 1;
          End;
      End;
      // Create expression
      F := TmpDataSet.FindField( FieldName );
      If Assigned( F ) Then
      Begin
        Params := TParameterList.Create;
        Params.Add( TStringLiteral.Create( Group ) );
        Params.Add( TStringLiteral.Create( Identifier ) );
        Expression := TFieldExpr.Create( Params, F, TSqlAnalizer(fAnalizer).xQuery, Self );
        If CheckData.RefCount = 1 Then
          CheckData.Field := F;
        Inc( CheckData.FieldCount );
        If CheckData.FieldCount <= MAX_INDEXED_FIELDS Then
          CheckData.Fields[CheckData.FieldCount] := F; // used in multi-key joining
      End
      Else
        Raise EExpression.CreateFmt( SWrongFieldName, [FieldName] );
    End;
  End;
  If Expression = Nil Then
  Begin
    If Identifier = 'NOW' Then
    begin
      CheckData.HasMorefunctions := True;
      If Not Assigned( ParameterList ) Then
        Expression := TNowExpr.Create( ParameterList )
      else
        NumError := 1;
    End
    Else If Identifier = 'SQLTRIM' Then // SQL TRIM function
    Begin
      CheckData.HasMorefunctions := True;
      If Assigned( ParameterList ) And ( ParameterList.Count = 3 ) Then
      Begin
        If Length( ParameterList.AsString[0] ) <> 1 Then
          Raise EExpression.Create( SWrongLengthInTrim );
        Expression := TSQLTrimExpr.Create( ParameterList )
      End
      Else
        NumError := 1;
    End
    Else If ( Identifier = 'ROUNDDEC' ) Or ( Identifier = 'TRUNCDEC' ) Then
    Begin
      CheckData.HasMorefunctions := True;
      If Assigned( ParameterList ) And ( ParameterList.Count = 2 ) And
        ( ParameterList.Exprtype[0] In [ttFloat, ttInteger] ) And
        ( ParameterList.Exprtype[1] = ttInteger ) Then
      Begin
        Expression := TRoundDecExpr.Create( ParameterList,
          ( AnsiCompareText( Identifier, 'ROUNDDEC' ) = 0 ) )
      End
      Else
        NumError := 1;
    End
    Else If Identifier = 'ISNULL' Then // for use in sql select only
    Begin
      CheckData.HasMorefunctions := True;
      // not enough checking for now
      If Assigned( ParameterList ) And ( ParameterList.Count = 2 ) Then
        Expression := TISNULLExpr.Create( ParameterList )
      Else
        NumError := 1;
    End Else if (fAnalizer = Nil) and (fDefaultDataSet <> Nil) then
    Begin
      // used in filter property in TxQuery
      // now look in the fDataSet fields
      F := fDefaultDataSet.FindField( Identifier );
      If Assigned( F ) Then
      Begin
        If Not Assigned( ParameterList ) Then
          Expression := TFilterFieldExpr.Create( ParameterList, F )
        Else
          NumError := 1;
      End;
    End;
  End;
  { RFC 1.80 }
  if (fAnalizer <> Nil) And (Expression = Nil) And
    Assigned(TSqlAnalizer(fAnalizer).xQuery.OnUDFCheck) And
    Assigned(TSqlAnalizer(fAnalizer).xQuery.OnUDFSolve) then
  Begin
    With TSqlAnalizer(fAnalizer) Do
    Begin // is this an UDF function ?
      Accept := False;
      Datatype:= ttString;
      MaxLen:= 0;
      xQuery.OnUDFCheck( xQuery, Identifier, ParameterList, Datatype, MaxLen, Accept );
      If Accept Then
      Begin
        CheckData.HasMorefunctions := True;
        Expression := TUDFExpr.Create( ParameterList, xQuery, Identifier, Datatype, MaxLen );
      End
      Else
        NumError := 1; // wrong number of params
    End;
  End;

  If NumError = 1 Then
    Raise EExpression.CreateFmt( SWrongParameters, [Identifier] );
End;

Procedure TExprParser.ParseExpression( Const ExprStr: String );
Var
  lexer: TCustomLexer;
  outputStream: TMemoryStream;
  errorStream: TMemoryStream;
  stream: TMemoryStream;
  ErrLine, ErrCol: Integer;
  ErrMsg, Errtxt: String;
Begin
  If Length( ExprStr ) = 0 Then Exit;
  FillChar( CheckData, SizeOf( TCheckData ), 0 );
  If Expression <> Nil Then
    FreeObject( Expression );
  Try
    If Length( ExprStr ) > 0 Then
    Begin
      stream := TMemoryStream.create;
      stream.write( ExprStr[1], Length( ExprStr ) );
      stream.seek( 0, 0 );
      outputStream := TMemoryStream.create;
      errorStream := TMemoryStream.create;
      lexer := TExprLexer.Create;
      lexer.yyinput := Stream;
      lexer.yyoutput := outputStream;
      lexer.yyerrorfile := errorStream;
      // link to the identifier function
      yyLexer := lexer; // lexer and parser linked
      Try
        If yyparse = 1 Then
        Begin
          ErrLine := lexer.yylineno;
          ErrCol := lexer.yycolno - Lexer.yyTextLen - 1;
          ErrMsg := yyerrormsg;
          lexer.GetyyText (Errtxt);
          Raise EXQueryError.CreateFmt( SExprParserError, [ErrMsg, ErrLine, ErrCol, ErrTxt] );
        End;
        Expression := GetExpression;
        CheckData.HasMorefunctions := CheckData.HasMorefunctions Or Self.IsComplex;
      Finally
        stream.free;
        lexer.free;
        outputstream.free;
        errorstream.free;
      End;
    End;
  Except
    On E: Exception Do
    Begin
      Expression := Nil;
      //MessageToUser(E.Message, smsgerror,MB_ICONERROR);
      Raise;
    End;
  End;
End;

Function TExprParser.CheckExpression( Const ExprStr: String ): Boolean;
Var
  lexer: TCustomLexer;
  outputStream: TMemoryStream;
  errorStream: TMemoryStream;
  stream: TMemoryStream;
  ErrLine, ErrCol: Integer;
  ErrMsg, Errtxt: String;
Begin
  FillChar( CheckData, SizeOf( TCheckData ), 0 );
  Result := false;
  If Expression <> Nil Then
    FreeObject( Expression );
  Try
    If Length( ExprStr ) > 0 Then
    Begin
      stream := TMemoryStream.create;
      stream.write( ExprStr[1], Length( ExprStr ) );
      stream.seek( 0, 0 );
      outputStream := TMemoryStream.create;
      errorStream := TMemoryStream.create;
      lexer := TExprLexer.Create;
      lexer.yyinput := Stream;
      lexer.yyoutput := outputStream;
      lexer.yyerrorfile := errorStream;
      // link to the identifier function
      yyLexer := lexer; // lexer and parser linked
      Try
        If yyparse = 1 Then
        Begin
          ErrLine := lexer.yylineno;
          ErrCol := lexer.yycolno - Lexer.yyTextLen - 1;
          ErrMsg := Self.yyerrormsg;
          lexer.GetyyText (Errtxt);
          Raise EXQueryError.CreateFmt( SExprParserError, [ErrMsg, ErrLine, ErrCol, ErrTxt] );
        End;
        Expression := Self.GetExpression;
        CheckData.HasMorefunctions := CheckData.HasMorefunctions Or IsComplex;
        Result := CheckData.RefCount > 0;
      Finally
        stream.free;
        lexer.free;
        outputstream.free;
        errorstream.free;
      End;
    End;
  Except
    On E: Exception Do
    Begin
      Expression := Nil;
      //MessageToUser(E.Message, smsgerror,MB_ICONERROR);
      Raise;
    End;
  End;
End;

Function TExprParser.GetString( const s: string ): string;
begin
  Result:= Copy( s, 2, Length(s) - 2);
end;

// this function returns the final expression obtained
function TExprParser.GetExpression: TExpression;
begin
  Result:= nil;
  if FExprList.Count<>1 then Exit;
  Result:= TExpression(FExprList[0]);
  FExprList.Delete(0);
end;

function TExprParser.GetParamList: TParameterList;
var
  I: Integer;
  NumParams: Integer;
begin
  Result:= nil;
  if FStackedParamCount.Count=0 then
    NumParams:= 0
  else
  begin
    NumParams:= Longint(FStackedParamCount[FStackedParamCount.Count-1]);
    FStackedParamCount.Delete(FStackedParamCount.Count-1);
  end;
  if (FExprList.Count=0) or (NumParams=0) or (NumParams>FExprList.Count) then Exit;
  Result:= TParameterList.Create;
  for I:= 0 to NumParams - 1 do
    Result.Add(FExprList[FExprList.Count - NumParams + I]);
  while NumParams > 0 do
  begin
    FExprList.Delete(FExprList.Count-1);
    Dec(NumParams);
  end;
end;

function TExprParser.ForceParamList(Count: Integer): TParameterList;
var
  I: Integer;
  NumParams: Integer;
begin
  Result:= nil;
  NumParams:= Count;
  if (FExprList.Count=0) or (NumParams=0) or (NumParams>FExprList.Count) then Exit;
  Result:= TParameterList.Create;
  for I:= 0 to NumParams - 1 do
    Result.Add(FExprList[FExprList.Count - NumParams + I]);
  while NumParams > 0 do
  begin
    FExprList.Delete(FExprList.Count-1);
    Dec(NumParams);
  end;
end;

procedure TExprParser.GetTwoOperators;
begin
  Op1:= TExpression(FExprList[FExprList.Count-2]);
  Op2:= TExpression(FExprList[FExprList.Count-1]);
  FExprList.Delete(FExprList.Count-1);
  FExprList.Delete(FExprList.Count-1);
end;

procedure TExprParser.GetOneOperator;
begin
  Op1:= TExpression(FExprList[FExprList.Count-1]);
  FExprList.Delete(FExprList.Count-1);
end;

procedure TExprParser.AddParam;
begin
  FParamCount:= Longint(FStackedParamCount[FStackedParamCount.Count-1]);
  Inc(FParamCount);
  FStackedParamCount[FStackedParamCount.Count-1]:= Pointer(FParamCount);
end;

function TExprParser.AddExpression(Expression: TExpression): TExpression;
begin
  FExprList.Add(Expression);
  FIsComplex:= True;
  Result:= Expression;
end;

Function TExprParser.GetExplicitParam( const ParamName: string ): string;
var
  pf: TParamsAsFieldsItem;
  Param: TParam;
begin
  Result:= '';
  if fAnalizer = Nil then Exit;
  // First a search on the ParamsAsFields property
  if TSqlAnalizer(fAnalizer).XQuery.ParamsAsFields.Count > 0 then
  begin
    pf := TSqlAnalizer(fAnalizer).XQuery.ParamsAsFields.ParamByName( ParamName );
    if pf <> Nil then
    begin
      Result:= TSqlAnalizer(fAnalizer).QualifiedField( '\f"' + pf.Value + '"', True );
      Exit;
    end;
  end;
  // if not found then replace with default params
  Param:= TSqlAnalizer(fAnalizer).xQuery.ParamByName( ParamName );
  if Param = Nil then
    raise ExQueryError(Format(SParameterNotFound,[ParamName]));
  // return param value
  Result:= Param.AsString;
  FExprList.Add(TExplicitParamExpr.Create( TSqlAnalizer(fAnalizer), Param ));
end;

%}

%start BasicExpr

%token _IDENTIFIER
%token _UINTEGER
%token _SINTEGER
%token _NUMERIC
%token _STRING

%token _COMA
%token _LPAREN
%token _RPAREN
%token _PERIOD
%token _COLON

%left  RW_OR RW_XOR RW_AND
%left  _EQ _NEQ _GT _LT _GE _LE RW_BETWEEN RW_IN RW_LIKE
%left  _PLUS _SUB
%left  _DIV RW_DIV _MULT RW_MOD RW_SHL RW_SHR
%right UMINUS       /* Negation--unary minus */
%right _EXP         /* exponentiation */
%left  RW_NOT
%token _ILLEGAL


/* other reserved words and tokens */
%token  _COMMENT
        _BLANK
        _TAB
        _NEWLINE
        RW_TRUE
        RW_FALSE
        RW_STRING
        RW_FLOAT
        RW_INTEGER
        RW_BOOLEAN
        RW_IN
        RW_CASE
        RW_WHEN
        RW_THEN
        RW_ELSE
        RW_END
        RW_IF
        RW_CAST
        RW_AS
        RW_ESCAPE

%type <string>

%%

BasicExpr : ExprConstant
           | DefineIdentifier ParameterList
             { FGroupIdent:= FGroupIdentList[FGroupIdentList.Count-1];
               FGroupIdentList.Delete(FGroupIdentList.Count-1);
               FIdentifier:= FIdentifierList[FIdentifierList.Count-1];
               FIdentifierList.Delete(FIdentifierList.Count-1);
               FTempParams:= GetParamList;

               IDF:=nil;
               IDFunc(Self, FGroupIdent, FIdentifier, FTempParams, IDF);
               if IDF <> nil then
               begin
                 if Length(FGroupIdent)=0 then
                   AddExpression(IDF)
                 else
                 begin
                   FExprList.Add(IDF);
                   if FTempParams=nil then
                   begin
                     if FIdReferences.IndexOf( FGroupIdent + '.' + FIdentifier ) < 0 then
                       FIdReferences.Add(FGroupIdent + '.' + FIdentifier);
                   end;
                 end;
               end else
               begin
                 if CompareText(FIdentifier,'LTRIM')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfLTrim))
                 else if CompareText(FIdentifier,'NULL')=0 then
                   IDF:= AddExpression(TNullValueExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'RTRIM')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfRTrim))
                 else if CompareText(FIdentifier,'TRIM')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfTrim))
                 else if CompareText(FIdentifier,'TRUNC')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfTrunc))
                 else if CompareText(FIdentifier,'ROUND')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfRound))
                 else if CompareText(FIdentifier,'ABS')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfAbs))
                 else if CompareText(FIdentifier,'ARCTAN')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfArcTan))
                 else if CompareText(FIdentifier,'COS')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfCos))
                 else if CompareText(FIdentifier,'SIN')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfSin))
                 else if CompareText(FIdentifier,'EXP')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfExp))
                 else if CompareText(FIdentifier,'FRAC')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfFrac))
                 else if CompareText(FIdentifier,'INT')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfInt))
                 else if CompareText(FIdentifier,'LN')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfLn))
                 else if CompareText(FIdentifier,'PI')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfPi))
                 else if CompareText(FIdentifier,'SQR')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfSqr))
                 else if CompareText(FIdentifier,'SQRT')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfSqrt))
                 else if CompareText(FIdentifier,'POWER')=0 then
                   IDF:= AddExpression(TMathExpression.Create(FTempParams, mfPower))
                 else if CompareText(FIdentifier,'UPPER')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfUpper))
                 else if CompareText(FIdentifier,'LOWER')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfLower))
                 else if CompareText(FIdentifier,'COPY')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfCopy))
                 else if CompareText(FIdentifier,'SUBSTRING')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfCopy))
                 else if CompareText(FIdentifier,'POS')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfPos))
                 else if CompareText(FIdentifier,'CHARINDEX')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfPos))
                 else if CompareText(FIdentifier,'LENGTH')=0 then
                   IDF:= AddExpression(TStringExpression.Create(FTempParams, sfLength))
                 else if CompareText(FIdentifier,'LEFT')=0 then
                   IDF:= AddExpression(TLeftExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'RIGHT')=0 then
                   IDF:= AddExpression(TRightExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'YEAR')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkYear))
                 else if CompareText(FIdentifier,'MONTH')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkMonth))
                 else if CompareText(FIdentifier,'DAY')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkDay))
                 else if CompareText(FIdentifier,'HOUR')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkHour))
                 else if CompareText(FIdentifier,'MIN')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkMin))
                 else if CompareText(FIdentifier,'SEC')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkSec))
                 else if CompareText(FIdentifier,'MSEC')=0 then
                   IDF:= AddExpression(TDecodeDateTimeExpr.Create(FTempParams, dkMSec))
                 else if CompareText(FIdentifier,'FORMATDATETIME')=0 then
                   IDF:= AddExpression(TFormatDateTimeExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'FORMATFLOAT')=0 then
                   IDF:= AddExpression(TFormatFloatExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'FORMAT')=0 then
                   IDF:= AddExpression(TFormatExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'DECODE')=0 then
                   IDF:= AddExpression(TDecodeExpr.Create(FTempParams))
                 else if CompareText(FIdentifier,'MINOF')=0 then
                   IDF:= AddExpression(TMinMaxOfExpr.Create(FTempParams, True))
                 else if CompareText(FIdentifier,'MAXOF')=0 then
                   IDF:= AddExpression(TMinMaxOfExpr.Create(FTempParams, False))
                 else if CompareText(FIdentifier,'SQLLIKE')=0 then
                   IDF:= AddExpression(TSQLLikeExpr.Create(FTempParams, False))
                 else if CompareText(FIdentifier,'SQLNOTLIKE')=0 then
                   IDF:= AddExpression(TSQLLikeExpr.Create(FTempParams, True))
                 else if CompareText(FIdentifier,'ASCII')=0 then
                   IDF:= AddExpression(TASCIIExpr.Create(FTempParams));
               end;
               if IDF= nil then
               begin
                 FTempParams.Free;
                 yyerror(Format('Unknown Identifier %s', [$<string>1]));
                 yyabort;
                 Exit;
               end;
             }
           | RW_CAST _LPAREN BasicExpr RW_AS RW_STRING _RPAREN
             { AddExpression(TTypeCast.Create(ForceParamList(1), ttString)); }
           | RW_FLOAT ParameterList
             { AddExpression(TTypeCast.Create(GetParamList, ttFloat)); }
           | RW_CAST _LPAREN BasicExpr RW_AS RW_FLOAT _RPAREN
             { AddExpression(TTypeCast.Create(ForceParamList(1), ttFloat)); }
           | RW_INTEGER ParameterList
             { AddExpression(TTypeCast.Create(GetParamList, ttInteger)); }
           | RW_CAST _LPAREN BasicExpr RW_AS RW_INTEGER _RPAREN
             { AddExpression(TTypeCast.Create(ForceParamList(1), ttInteger)); }
           | RW_BOOLEAN ParameterList
             { AddExpression(TTypeCast.Create(GetParamList, ttBoolean)); }
           | RW_CAST _LPAREN BasicExpr RW_AS RW_BOOLEAN _RPAREN
             { AddExpression(TTypeCast.Create(ForceParamList(1), ttBoolean)); }
           | RW_IF ParameterList
             { AddExpression(TConditional.Create(GetParamList)); }
           | CASEClause
             { AddExpression(TCaseWhenElseExpr.Create(FWhenParamList, FThenParamList, FElseExpr));
               FWhenParamList:= nil;
               FThenParamList:= nil;
               FElseExpr:= nil;
             }
           | BasicExpr RW_BETWEEN ExprConstant RW_AND ExprConstant
             { AddExpression( TBetweenExpr.Create(ForceParamList(3), FALSE) ); }
           | BasicExpr RW_NOT RW_BETWEEN ExprConstant RW_AND ExprConstant
             { AddExpression( TBetweenExpr.Create(ForceParamList(3), TRUE) ); }
           | BasicExpr RW_IN ParameterList
             { AddExpression( TSQLInPredicateExpr.Create(ForceParamList(FParamCount + 1), FALSE) ); }
           | BasicExpr RW_NOT RW_IN ParameterList
             { AddExpression( TSQLInPredicateExpr.Create(ForceParamList(FParamCount + 1), TRUE) ); }
           | BasicExpr RW_LIKE ExprConstant ESCAPECharacter
             { AddExpression(TSQLLikeExpr.Create(ForceParamList(3), FALSE)); }
           | BasicExpr RW_NOT RW_LIKE ExprConstant ESCAPECharacter
             { AddExpression(TSQLLikeExpr.Create(ForceParamList(3), TRUE)); }
           | _SUB BasicExpr %prec UMINUS
             { GetOneOperator;
               AddExpression(TUnaryOp.Create(opMinus, Op1));
               FIsComplex:= True;}
           | _PLUS BasicExpr %prec UMINUS
             { GetOneOperator;
               AddExpression(TUnaryOp.Create(opPlus, Op1));
               FIsComplex:= True;}
           | RW_NOT BasicExpr
             { GetOneOperator;
               AddExpression(TUnaryOp.Create(opNot, Op1));
               FIsComplex:= True;}
           | BasicExpr _PLUS BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opPlus, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _SUB BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opMinus, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _MULT BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opMult, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _DIV BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opDivide, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_DIV BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opDiv, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _EXP BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opExp, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_MOD BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opMod, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_SHL BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opShl, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_SHR BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opShr, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _GE BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opGTE, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _LE BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opLTE, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _GT BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opGT, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _LT BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opLT, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _EQ BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opEQ, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr _NEQ BasicExpr
             { GetTwoOperators;
               AddExpression(TRelationalOp.Create(opNEQ, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_AND BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opAnd, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_OR BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opOr, Op1, Op2));
               FIsComplex:= True;}
           | BasicExpr RW_XOR BasicExpr
             { GetTwoOperators;
               AddExpression(TBinaryOp.Create(opXor, Op1, Op2));
               FIsComplex:= True;}
           | _LPAREN BasicExpr _RPAREN
             { FIsComplex:= True; }
           ;

ParameterList : /* empty */
                 { FStackedParamCount.Add(Pointer(0)); }
               | StartList _RPAREN
               | StartList ParamsList _RPAREN
               ;

StartList : _LPAREN
             { FStackedParamCount.Add(Pointer(0)); }
           ;

ParamsList : BasicExpr
             { AddParam; }
           | ParamsList _COMA BasicExpr
             { AddParam; }
           ;

/* CASE...WHEN...THEN...ELSE...END clause */
CASEClause : RW_CASE WHENList ELSEClause RW_END
            ;

WHENList : WHENClause
          | WHENList WHENClause
          ;

WHENClause : RW_WHEN BasicExpr RW_THEN BasicExpr
              { if FWhenParamList=nil then
                  FWhenParamList:= TParameterList.Create;
                if FThenParamList=nil then
                  FThenParamList:= TParameterList.Create;
                FWhenParamList.Add(FExprList[FExprList.Count-2]);
                FThenParamList.Add(FExprList[FExprList.Count-1]);
                FExprList.Delete(FExprList.Count-1);
                FExprList.Delete(FExprList.Count-1);
              }
            ;

ELSEClause : /* empty */
            | RW_ELSE BasicExpr
              { FElseExpr:= TExpression(FExprList[FExprList.Count-1]);
                FExprList.Delete(FExprList.Count-1);
              }
            ;

DefineIdentifier : _IDENTIFIER
                    { FGroupIdentList.Add('');
                      FIdentifierList.Add(UpperCase($<string>1));
                    }
                  | _IDENTIFIER _PERIOD _IDENTIFIER
                    { FGroupIdentList.Add(UpperCase($<string>1));
                      FIdentifierList.Add(UpperCase($<string>3));
                    }
                  ;

ExprConstant : _UINTEGER
                { Val($<string>1, IntVal, Code);
                  if Code=0 then
                    FExprList.Add(TIntegerLiteral.Create(StrToInt($<string>1)))
                  else
                    FExprList.Add(TFloatLiteral.Create(StrToFloat($<string>1)));
                }
              | _SINTEGER
                { Val($<string>1, IntVal, Code);
                  if Code=0 then
                    FExprList.Add(TIntegerLiteral.Create(StrToInt($<string>1)))
                  else
                    FExprList.Add(TFloatLiteral.Create(StrToFloat($<string>1)));
                }
              | _NUMERIC   { FExprList.Add(TFloatLiteral.Create(StrToFloat($<string>1))); }
              | _STRING    { FExprList.Add(TStringLiteral.Create( GetString( $<string>1 ) )); }
              | RW_TRUE    { FExprList.Add(TBooleanLiteral.Create(True)); }
              | RW_FALSE   { FExprList.Add(TBooleanLiteral.Create(False)); }
              | _COLON _IDENTIFIER
                {$<string>$ := GetExplicitParam( $<string>2 );}
              ;

ESCAPECharacter : /* empty */
                   { FExprList.Add(TStringLiteral.Create('')); }
                 | RW_ESCAPE _STRING
                   { FExprList.Add(TStringLiteral.Create(GetString( $<string>2 ))); }
                 ;

%%