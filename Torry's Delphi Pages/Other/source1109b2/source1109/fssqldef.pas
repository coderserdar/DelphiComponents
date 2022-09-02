{*********************************************************}
{* FSSQL: SQL Class Definitions                        *}
{* Programming: Krzysztof Winnicki                       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 * The next programing by Krzysztof Winnicki
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I fsdefine.inc}

{Enable the following to have index optimization analysis and usage
 information logged to a file (used for debugging)}
{$DEFINE LogIndexAnalysis}

{Enable the following to have transformation information
 logged to a file (used for debugging)}
{.$DEFINE LogTransformations}

{Enable the following to have writes counted}
{.$DEFINE CountWrites}

{Enable the following to have the root node made available through the global
 LastStatement variable below (used for debugging only)}
{.$DEFINE ExposeLastStatement}

Unit fssqldef;

Interface
Uses
  Windows,
  SysUtils,
  Classes,
  fsfunInterp,
  fsllbase,
  Db,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  fssqldb,
  fsserverclass,
  fshash;

{.$DEFINE LogIndexAnalysis}

{$IFDEF LogIndexAnalysis}
Const
  IALogFile = 'c:\fsidxlog.txt';
  {$ENDIF}

  {$IFDEF LogTransformations}
Const
  TRLogFile = 'c:\fstrlog.txt';
  {$ENDIF}

  {$IFDEF LogIndexAnalysis}
Var
  IALog: System.Text;
  {$ENDIF}

  {$IFDEF LogTransformations}
Var
  TRLog: System.Text;
  {$ENDIF}

Type
  TfsSqlAggQueryMode = (aqmIdle, aqmGrouping, aqmHaving);

  TfsSqlNode = Class;
  TfsSqlStatement = Class;
  TfsSqlEnumMethod = Procedure(Node: TfsSqlNode) Of Object;
  TfsSqlAggregate = Class;
  TfsSqlSELECT = Class;
  TfsSqlColumnListOwner = Class;
  TfsSqlRelOp = (roNone, roEQ, roLE, roL, roG, roGE, roNE);
  //('',   '=',  '<=', '<', '>', '>=', '<>');
  TfsSqlNode = Class(TFSSpecObject)
  Protected
    FParent: TfsSqlNode;
    FOwner: TfsSqlStatement;
    FOwnerStmt: TfsSqlColumnListOwner; {!!.11}
    FValue: String;
    FNull: boolean;
    Procedure WriteStr(Stream: TStream; Const S: String);
    Procedure WriteEOF(Stream: TStream);
    Procedure AddTableReference(Select: TfsSqlSELECT); Virtual;
    Procedure AddColumnDef(Target: TfsSqlColumnListOwner); Virtual;
    Procedure AddDistinctColumnDef(Target: TfsSqlColumnListOwner); Virtual;
    Procedure AddAggregate(Target: TList); Virtual;
    Procedure ClearBinding; Virtual;
    Function IsAncestor(Const Node: TfsSqlNode): Boolean;
    { Returns True if Node is an ancestor Of this node. }
    Procedure ResetConstant; Virtual;
    Procedure FlagAggregate(Select: TfsSqlSELECT); Virtual;
    Function GetType: TfsFieldType; Virtual;
    Function GetSize: Integer; Virtual;
    Function GetDecimals: Integer; Virtual;
    Function GetBlobLevel: TDataCompLevel; Virtual;
    Function GetRound: TRound; Virtual;
    Function GetOwner: TfsSqlStatement;
    Function GetOwnerSelect: TfsSqlSelect;
    Function GetOwnerStmt: TfsSqlColumnListOwner; {!!.11}
    Procedure SQLError(Const ErrorMsg: String);
    Procedure AssignError(Source: TfsSqlNode);
    Procedure TypeMismatch;
    Function BindField(Const TableName,
      FieldName: String): TfsSqlFieldProxy; Virtual;
    Function IsAggregate: Boolean; Virtual;
  Public

    Constructor Create(AParent: TfsSqlNode);
    Property Value: String Read FValue Write FValue;
    Property IsNull: boolean Read fNull Write fNull;
    Property Parent: TfsSqlNode Read FParent Write FParent;
    Property Owner: TfsSqlStatement Read GetOwner;
    Property OwnerSelect: TfsSqlSelect Read GetOwnerSelect;
    Property OwnerStmt: TfsSqlColumnListOwner Read GetOwnerStmt; {!!.11}
    Procedure EmitSQL(Stream: TStream); Virtual;
    Function SQLText: String;
    Function Equals(Other: TfsSqlNode): Boolean; Virtual; Abstract;
    Procedure Assign(Const Source: TfsSqlNode); Virtual; Abstract;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
      Virtual; Abstract;
  End;

  TfsSqlFieldRef = Class(TfsSqlNode)
  Protected
    FFieldName: String;
    FTableName: String;
    TypeKnown: Boolean;
    FType: TfsFieldType;
    FField: TfsSqlFieldProxy;
    FGroupField: TfsSqlFieldProxy;
    WasWildcard: Boolean;
    Procedure ClearBinding; Override;
    Function GetDecimals: Integer; Override;
    Function GetBlobLevel: TDataCompLevel; Override;
    Function GetSize: Integer; Override;
    Function GetTitle(Const Qualified: boolean): String; {!!.11}
    Function GetType: TfsFieldType; Override;
    Function GetRound: TRound; Override;
    Procedure CheckType;
    Procedure MatchType(ExpectedType: TfsFieldType);
    Function GetField: TfsSqlFieldProxy;
    Function GetGroupField: TfsSqlFieldProxy;
  Public
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Property TableName: String Read FTableName Write FTableName;
    Property FieldName: String Read FFieldName Write FFieldName;
    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
    Function GetValue(aArrayIndex: Integer): Variant;
    Property Field: TfsSqlFieldProxy Read GetField;
    Property GroupField: TfsSqlFieldProxy Read GetGroupField;
    Function DependsOn(Table: TFSSqlTableProxy): Boolean;
    Function QualName: String;
    Function IsNull: Boolean;
  End;

  TfsSqlSimpleExpression = Class;

  TfsAggCounter = Class(TFSSpecObject)
  Protected
    eFCount, eFMin, eFMax, eFSum: Extended;
    FDataType: TfsFieldType;
    Function GetMax: Variant;
    Function GetMin: Variant;
    Function GetSum: Variant;
    Function GetAvg: Variant;
    Function GetCount: Variant;
  Public
    Procedure Reset;
    Procedure Add(Const Value: Variant);
    Property Min: Variant Read GetMin;
    Property Max: Variant Read GetMax;
    Property Sum: Variant Read GetSum;
    Property Avg: Variant Read GetAvg;
    Property Count: Variant Read GetCount;
    Property DataType: TfsFieldType Read FDataType Write FDataType;
  End;

  TfsSqlAggFunction = (agCount, agMin, agMax, agSum, agAvg);

  TfsSqlAggregate = Class(TfsSqlNode)
  Protected
    FAgFunction: TfsSqlAggFunction;
    FSimpleExpression: TfsSqlSimpleExpression;
    FDistinct, FDistinctCase, fDistinctOnlyList, FGroupCase, FGroupUnionCase: Boolean;
    FCounter: TfsAggCounter;
    FSourceField: TfsSqlFieldProxy;
    Function GetTitle(Const Qualified: boolean): String; {!!.11}
    Procedure MatchType(ExpectedType: TfsFieldType);
    Function GetSize: Integer; Override;
    Function GetDecimals: Integer; Override;
    Function GetBlobLevel: TDataCompLevel; Override;
    Function GetType: TfsFieldType; Override;
    Procedure FlagAggregate(Select: TfsSqlSELECT); Override;
    Procedure AddAggregate(Target: TList); Override;
    Function Reduce: Boolean;
  Public
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Destructor Destroy; Override;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Property AgFunction: TfsSqlAggFunction Read FAgFunction Write FAgFunction;
    Property SimpleExpression: TfsSqlSimpleExpression
      Read FSimpleExpression Write FSimpleExpression;
    Property Distinct: Boolean Read FDistinct Write FDistinct;
    Property DistinctCase: Boolean Read FDistinctCase Write FDistinctCase;
    Property DistinctOnlyList: boolean Read fDistinctOnlyList Write fDistinctOnlyList;
    Property GroupCase: boolean Read FGroupCase Write FGroupCase;
    Property GroupUnionCase: boolean Read FGroupUnionCase Write FGroupUnionCase;

    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
    Function GetAggregateValue: Variant;
    Procedure CreateCounter(SourceField: TfsSqlFieldProxy);
    Procedure DeleteCounter;
    Procedure ResetCounters;
    Procedure Update;
    Function ValidType(AType: TfsFieldType): Boolean;
    Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  End;

  TfsSqlColumn = Class(TfsSqlNode)
  Protected
    FColumnName: String;
  Public
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Property ColumnName: String Read FColumnName Write FColumnName;
    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
  End;

  TfsSqlBaseColumn = Class(TfsSqlNode)
  Protected
    FFieldName: String;
    FTableName: String;
  Public
    Property TableName: String Read FTableName Write FTableName;
    Property FieldName: String Read FFieldName Write FFieldName;
  End;

  TfsSqlGroupColumn = Class(TfsSqlBaseColumn)
  Public
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
    Function QualColumnName: String; Virtual;
  End;

  TfsSqlOrderColumn = Class(TfsSqlBaseColumn)
  Public
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
    Function QualColumnName: String;
  End;

  TfsSqlSelection = Class;

  TfsSqlGroupColumnList = Class(TfsSqlNode)
  Protected
    ColumnList: TList;
    Procedure Clear;
    Function GetColumn(Index: Integer): TfsSqlGroupColumn;
    Procedure SetColumn(Index: Integer; Const Value: TfsSqlGroupColumn);
    Function GetColumnCount: Integer;
    Function Reduce: Boolean;
  Public
    UGroup: Boolean;
    Procedure Assign(Const Source: TfsSqlNode); Override;
    Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
    Constructor Create(AParent: TfsSqlNode);
    Destructor Destroy; Override;
    Function AddColumn(Column: TfsSqlGroupColumn): TfsSqlGroupColumn;
    Property ColumnCount: Integer Read GetColumnCount;
    Property Column[Index: Integer]: TfsSqlGroupColumn Read GetColumn Write SetColumn;
    Procedure EmitSQL(Stream: TStream); Override;
    Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function Contains(Const AColName: String;
    Se: TfsSqlSelection): Boolean;
End;

TfsSqlIsOp = (ioNull, ioTrue, ioFalse, ioUnknown);
TfsSqlIsTest = Class(TfsSqlNode)
Protected
  FUnaryNot: Boolean;
  FIsOp: TfsSqlIsOp;
  Procedure MatchType(ExpectedType: TfsFieldType);
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property UnaryNot: Boolean Read FUnaryNot Write FUnaryNot;
  Property IsOp: TfsSqlIsOp Read FIsOp Write FIsOp;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean(Const TestValue: Variant): Boolean;
  Function Evaluate(Expression: TfsSqlSimpleExpression): Boolean;
End;

TfsSqlBetweenClause = Class(TfsSqlNode)
Protected
  FSimpleHigh: TfsSqlSimpleExpression;
  FSimpleLow: TfsSqlSimpleExpression;
  FNegated: Boolean;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property Negated: Boolean Read FNegated Write FNegated;
  Property SimpleLow: TfsSqlSimpleExpression Read FSimpleLow Write FSimpleLow;
  Property SimpleHigh: TfsSqlSimpleExpression Read FSimpleHigh Write FSimpleHigh;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean(Const TestValue: Variant): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlLikePattern = Class(TFSSpecObject)
Protected
  LeadPattern,
    TrailPattern: String;
  LeadMask,
    TrailMask: String;
  FloatPatterns,
    FloatMasks: TStringList;
Public
  Constructor Create(SearchPattern: String; Const Escape: String);
  {S is the search pattern; Escape is an optional one-character escape
  character}
  {S contains the String to be searched for, And optionally one Or more
  occurrences Of
  '%' (match zero or more characters of any kind), And/Or
  '_'  (match exactly one character Of any kind)
If the Escape character is specified, it defines a character to prefix '%'
  or '_' With
  to indicate a literal '%' Or '_', respectively, in the search phrase S.}
  {the search must be Case sensitive ('a' <> 'A') }
  Destructor Destroy; Override;
  Function Find(Const TextToSearch: Variant; IgnoreCase: Boolean): Boolean; {!!.13}
  {examples:
  S = '%Berkeley%' - Find returns true if the String 'Berkeley' exists
    anywhere in TextToSearch
  S = 'S__' - Find returns true If TextToSearch is exactly thee characters
    long and starts With an upper-case 'S'
  S = '%c___' - Find returns True if length(TextToSearch) >= 4 And the
    last but three is 'c'
  S = '=_%' And Escape = '=' - Find returns True If TextToSearch begins
    With an underscore.
    }
End;

TfsSqlLikeClause = Class(TfsSqlNode)
Protected
  FSimpleExp: TfsSqlSimpleExpression;
  FEscapeExp: TfsSqlSimpleExpression;
  FNegated: Boolean;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Limited: Boolean;
  LikePattern: TfsSqlLikePattern;
  FBMCompat: Boolean; {!!.11}
  BMCompatChecked: Boolean; {!!.11}
  FBMTable: PBTable; {!!.11}
  FBMPhrase: String; {!!.11}
  FIgnoreCase: Boolean; {!!.13}
  Procedure CheckBMCompat; {!!.11}
  Function IsBMCompatible: Boolean; {!!.11}
  Function GetBmTable: PBTable; {!!.11}
  Function CanLimit: Boolean;
  Function CanReplaceWithCompare: Boolean;
  Procedure CheckIsConstant;
  Function GetLowLimit: String;
  Function GetHighLimit: String;
  Function IsConstant: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SimpleExp: TfsSqlSimpleExpression Read FSimpleExp Write FSimpleExp;
  Property EscapeExp: TfsSqlSimpleExpression Read FEscapeExp Write FEscapeExp;
  Property Negated: Boolean Read FNegated Write FNegated;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean(Const TestValue: Variant): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Property BmTable: PBTable Read GetBmTable; {!!.11}
  Property BmPhrase: String Read FBmPhrase; {!!.11}
  Property IgnoreCase: Boolean Read FIgnoreCase Write FIgnoreCase; {!!.13}
End;

TfsSqlSimpleExpressionList = Class;

TfsSqlInClause = Class(TfsSqlNode)
Protected
  FSimpleExp: TfsSqlSimpleExpressionList;
  FNegated: Boolean;
  FSubQuery: TfsSqlSELECT;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SimpleExpList: TfsSqlSimpleExpressionList
    Read FSimpleExp Write FSimpleExp;
  Property SubQuery: TfsSqlSELECT Read FSubQuery Write FSubQuery;
  Property Negated: Boolean Read FNegated Write FNegated;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean(Const TestValue: Variant): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlTableExp = Class;

TfsSqlMatchOption = (moUnspec, moPartial, moFull);
TfsSqlMatchClause = Class(TfsSqlNode)
Protected
  FSubQuery: TfsSqlSELECT;
  FOption: TfsSqlMatchOption;
  FUnique: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property Unique: Boolean Read FUnique Write FUnique;
  Property Option: TfsSqlMatchOption Read FOption Write FOption;
  Property SubQuery: TfsSqlSELECT Read FSubQuery Write FSubQuery;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean(Const TestValue: Variant): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlAllOrAnyClause = Class(TfsSqlNode)
Protected
  FSubQuery: TfsSqlSELECT;
  FAll: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function Compare(RelOp: TfsSqlRelOp; Const Val: Variant): Boolean;
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property All: Boolean Read FAll Write FAll;
  Property SubQuery: TfsSqlSELECT Read FSubQuery Write FSubQuery;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlExistsClause = Class(TfsSqlNode)
Protected
  FSubQuery: TfsSqlSELECT;
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SubQuery: TfsSqlSELECT Read FSubQuery Write FSubQuery;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlUniqueClause = Class(TfsSqlNode)
Protected
  FSubQuery: TfsSqlTableExp;
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SubQuery: TfsSqlTableExp Read FSubQuery Write FSubQuery;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlCondPrimary = Class(TfsSqlNode)
Protected
  FSimpleExp1: TfsSqlSimpleExpression;
  FRelOp: TfsSqlRelOp;
  FSimpleExp2: TfsSqlSimpleExpression;
  FBetweenClause: TfsSqlBetweenClause;
  FLikeClause: TfsSqlLikeClause;
  FInClause: TfsSqlInClause;
  FIsTest: TfsSqlIsTest;
  FAllOrAnyClause: TfsSqlAllOrAnyClause;
  FExistsClause: TfsSqlExistsClause;
  FUniqueClause: TfsSqlUniqueClause;
  FMatchClause: TfsSqlMatchClause;
  TypeChecked: Boolean;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Procedure CheckType;
  Function GetType: TfsFieldType; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetSize: Integer; Override;
  Function GetRound: TRound; Override;
  Function GetTitle(Const Qualified: boolean): String; {!!.11}
  Function JustSimpleExpression: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType); {!!.11}
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Property SimpleExp1: TfsSqlSimpleExpression
    Read FSimpleExp1 Write FSimpleExp1;
  Property RelOp: TfsSqlRelOp Read FRelOp Write FRelOp;
  Property SimpleExp2: TfsSqlSimpleExpression
    Read FSimpleExp2 Write FSimpleExp2;
  Property BetweenClause: TfsSqlBetweenClause
    Read FBetweenClause Write FBetweenClause;
  Property LikeClause: TfsSqlLikeClause Read FLikeClause Write FLikeClause;
  Property InClause: TfsSqlInClause Read FInClause Write FInClause;
  Property IsTest: TfsSqlIsTest Read FIsTest Write FIsTest;
  Property AllOrAnyClause: TfsSqlAllOrAnyClause
    Read FAllOrAnyClause Write FAllOrAnyClause;
  Property ExistsClause: TfsSqlExistsClause
    Read FExistsClause Write FExistsClause;
  Property UniqueClause: TfsSqlUniqueClause
    Read FUniqueClause Write FUniqueClause;
  Property MatchClause: TfsSqlMatchClause Read FMatchClause Write FMatchClause;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function GetValue(aArrayIndex: Integer): Variant;
  Procedure BindHaving;
  Function IsRelationTo(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy;
    Var Operator: TfsSqlRelOp;
    Var ArgExpression: TfsSqlSimpleExpression;
    Var SameCase: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlCondFactor = Class(TfsSqlNode)
Protected
  FUnaryNot: Boolean;
  FCondPrimary: TfsSqlCondPrimary;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  TmpKnown: Boolean;
  TmpValue: Boolean;
  EvalLevel: Integer;
  Procedure CheckIsConstant;
  Procedure Clear;
  Function IsConstant: Boolean;
  Function GetType: TfsFieldType; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetSize: Integer; Override;
  Function GetRound: TRound; Override;
  Function GetTitle(Const Qualified: boolean): String; {!!.11}
  Procedure MatchType(ExpectedType: TfsFieldType); {!!.11}
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Destructor Destroy; Override;
  Property UnaryNot: Boolean Read FUnaryNot Write FUnaryNot;
  Property CondPrimary: TfsSqlCondPrimary Read FCondPrimary Write FCondPrimary;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function GetValue(aArrayIndex: Integer): Variant;
  Procedure BindHaving;
  Function IsRelationTo(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy;
    Var Operator: TfsSqlRelOp;
    Var ArgExpression: TfsSqlSimpleExpression;
    Var SameCase: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Procedure MarkTrue;
  Procedure MarkUnknown;
End;

TfsSqlCondExp = Class;
TFSSpecObjectProc = Procedure Of Object;

TfsSqlKeyRelation = Record
  CondF: TfsSqlCondFactor;
  RelationB: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlCondFactor; {!!.11}
  NativeKeyIndex: Integer;
  RelationFieldCount,
    RelationKeyFieldCount: Integer;
  RelationOperators: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlRelOp;
  RelationOperatorB: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlRelOp; {!!.11}
  RelationKeyIsUnique: Boolean;
  RelationKeyIsCaseInsensitive: Boolean;
  RelationKeyIndexAsc: Boolean;
  ArgExpressionB: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlSimpleExpression; {!!.11}
  ArgExpressions: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlSimpleExpression;
  {$IFDEF LogIndexAnalysis}
  RelationFields: Array[0..pred(fscl_MaxIndexFlds)] Of TfsSqlFieldProxy;
  {$ENDIF}
  SameCases: Array[0..pred(fscl_MaxIndexFlds)] Of Boolean;
  SameCaseB: Array[0..pred(fscl_MaxIndexFlds)] Of Boolean; {!!.11}
  DepIndex: Integer;
End;

TFSSqlTableProxySubset = Class(TFSSpecObject)
Protected
  FTable: TFSSqlTableProxy;
  FOpposite: TFSSqlTableProxy;
  FOuter: Boolean;
Public
  Relations: Integer;
  KeyRelation: TfsSqlKeyRelation;
  Constructor Create(Table: TFSSqlTableProxy);
  Function EqualKeyDepth: Integer;
  Procedure Iterate(Iterator: TfsSqlTableIterator; Cookie: TffWord32);
  Property Table: TFSSqlTableProxy Read FTable;
  Procedure Assign(Const Source: TFSSqlTableProxySubset);
  Function UniqueValue: Boolean;
  Function ClosedSegment: Boolean;
  Function KeyDepth: Integer;
  Property Outer: Boolean Read FOuter Write FOuter;
  Property Opposite: TFSSqlTableProxy Read FOpposite Write FOpposite;
End;

TFSSqlTableProxySubsetList = Class;

TfsSqlCondTerm = Class(TfsSqlNode)
Protected
  CondFactorList: TList;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  OrderedSources: TFSSqlTableProxySubsetList;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function GetCondFactor(Index: Integer): TfsSqlCondFactor;
  Procedure SetCondFactor(Index: Integer; Const Value: TfsSqlCondFactor);
  Function GetCondFactorCount: Integer;
  Function GetSize: Integer; Override;
  Function GetTitle(Const Qualified: Boolean): String; {!!.11}
  Function GetType: TfsFieldType; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetRound: TRound; Override;
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
  Function AsBooleanLevel(Level: Integer): Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType); {!!.11}
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddCondFactor(Factor: TfsSqlCondFactor): TfsSqlCondFactor;
  Function InsertCondFactor(Index: Integer; Factor: TfsSqlCondFactor):
    TfsSqlCondFactor;
  Property CondFactorCount: Integer Read GetCondFactorCount;
  Property CondFactor[Index: Integer]: TfsSqlCondFactor
  Read GetCondFactor Write SetCondFactor;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function GetValue(aArrayIndex: Integer): Variant;
  Procedure BindHaving;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Procedure SetLevelDep(List: TFSSqlTableProxySubsetList);
End;

TFSSqlTableProxySubsetList = Class(TFSSpecObject)
Protected
  FList: TList;
  Level: Integer;
  FCondTerm: TfsSqlCondTerm;
  FCreateResultRecord: TFSSpecObjectProc;
  FRecordsRead: Longint;
  FOwner: TfsSqlStatement;
  WroteRow: Boolean;
  FOuterJoin: Boolean;
  FSkipInner: Boolean;
  V: Array[0..pred(fscl_MaxIndexFlds)] Of Variant;
  VB: Array[0..pred(fscl_MaxIndexFlds)] Of Variant; {!!.11}
  Procedure ReadSources;
  Function GetItem(Index: Integer): TFSSqlTableProxySubset;
  Function GetCount: Integer;
  Function ProcessLevel(Cookie1: TffWord32): Boolean;
  Procedure Clear;
  Function Insert(
    TableProxySubset: TFSSqlTableProxySubset): TFSSqlTableProxySubset;
Public
  Constructor Create(AOwner: TfsSqlStatement);
  Destructor Destroy; Override;
  Function Add(TableProxySubset: TFSSqlTableProxySubset): TFSSqlTableProxySubset;
  Procedure Delete(Index: Integer);
  Property Item[Index: Integer]: TFSSqlTableProxySubset Read GetItem;
  Property Count: Integer Read GetCount;
  Procedure Assign(Const Source: TFSSqlTableProxySubsetList);
  Function RelationUsed(Relation: TfsSqlCondFactor): Boolean;
  Function DependencyExists(Table: TFSSqlTableProxy): Boolean;
  Procedure Join(
    CondTerm: TfsSqlCondTerm;
    CreateResultRecord: TFSSpecObjectProc);
  Property RecordsRead: Longint Read FRecordsRead;
  Property Owner: TfsSqlStatement Read FOwner;
  Property OuterJoin: Boolean Read FOuterJoin Write FOuterJoin;
  Property SkipInner: Boolean Read FSkipInner Write FSkipInner;
End;

TfsSqlCondExp = Class(TfsSqlNode)
Protected
  CondTermList: TList;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function GetCondTerm(Index: Integer): TfsSqlCondTerm;
  Procedure SetCondTerm(Index: Integer; Const Value: TfsSqlCondTerm);
  Function GetCondTermCount: Integer;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetSize: Integer; Override;
  Procedure ResetConstant; Override;
  Function Reduce: Boolean;
  Function AsBooleanLevel(Level: Integer): Boolean;
  Procedure SetLevelDep(List: TFSSqlTableProxySubsetList);
  Function GetTitle(Const Qualified: Boolean): String; {!!.11}
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddCondTerm(Term: TfsSqlCondTerm): TfsSqlCondTerm;
  Property CondTermCount: Integer Read GetCondTermCount;
  Property CondTerm[Index: Integer]: TfsSqlCondTerm
  Read GetCondTerm Write SetCondTerm;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function AsBoolean: Boolean;
  Function GetValue(aArrayIndex: Integer): Variant;
  Procedure BindHaving;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlFloatLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  SingleValue: Single;
  DoubleValue: Double;
  ExtendedValue: Extended;
  CurrencyValue: Currency;
  StringValue: String;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlIntegerLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  Int32Value: Longint;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlInteger64Literal = Class(TfsSqlNode)
Protected
  FValue: String;
  Int64Value: Int64;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlStringLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  FType: TfsFieldType;
  Converted: Boolean;
  CharValue: Char;
  WideCharValue: WideChar;
  ShortStringValue: ShortString;
  NullStringValue: String;
  WideStringValue: WideString;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Constructor Create(AParent: TfsSqlNode);
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlIntervalDef = (iUnspec, iYear, iMonth, iDay, iHour, iMinute, iSecond);
TfsSqlIntervalLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  FStartDef: TfsSqlIntervalDef;
  FEndDef: TfsSqlIntervalDef;
  Y1, M1, D1, H1, S1: Integer;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
  Function AddIntervalTo(Target: TDateTime): TDateTime;
  Function SubtractIntervalFrom(Target: TDateTime): TDateTime;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Property StartDef: TfsSqlIntervalDef Read FStartDef Write FStartDef;
  Property EndDef: TfsSqlIntervalDef Read FEndDef Write FEndDef;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlTimestampLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  DateTimeValue: TDateTime;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlTimeLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  TimeValue: TDateTime;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlDateLiteral = Class(TfsSqlNode)
Protected
  FValue: String;
  DateValue: TDateTime;
  Converted: Boolean;
  Procedure ConvertToNative;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: String Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlBooleanLiteral = Class(TfsSqlNode)
Protected
  FValue: Boolean;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetType: TfsFieldType; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Value: Boolean Read FValue Write FValue;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue: Boolean;
End;

TfsSqlLiteral = Class(TfsSqlNode)
Protected
  FFloatLiteral: TfsSqlFloatLiteral;
  FIntegerLiteral: TfsSqlIntegerLiteral;
  FInteger64Literal: TfsSqlInteger64Literal;
  FStringLiteral: TfsSqlStringLiteral;
  FDateLiteral: TfsSqlDateLiteral;
  FTimeLiteral: TfsSqlTimeLiteral;
  FTimeStampLiteral: TfsSqlTimestampLiteral;
  FIntervalLiteral: TfsSqlIntervalLiteral;
  FBooleanLiteral: TfsSqlBooleanLiteral;
  Procedure Clear;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function AddIntervalTo(Target: TDateTime): TDateTime;
  Function SubtractIntervalFrom(Target: TDateTime): TDateTime;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Destructor Destroy; Override;
  Property BooleanLiteral: TfsSqlBooleanLiteral
    Read FBooleanLiteral Write FBooleanLiteral;
  Property FloatLiteral: TfsSqlFloatLiteral
    Read FFloatLiteral Write FFloatLiteral;
  Property IntegerLiteral: TfsSqlIntegerLiteral
    Read FIntegerLiteral Write FIntegerLiteral;
  Property Integer64Literal: TfsSqlInteger64Literal
    Read FInteger64Literal Write FInteger64Literal;
  Property StringLiteral: TfsSqlStringLiteral
    Read FStringLiteral Write FStringLiteral;
  Property DateLiteral: TfsSqlDateLiteral
    Read FDateLiteral Write FDateLiteral;
  Property TimeLiteral: TfsSqlTimeLiteral
    Read FTimeLiteral Write FTimeLiteral;
  Property TimeStampLiteral: TfsSqlTimestampLiteral
    Read FTimestampLiteral Write FTimestampLiteral;
  Property IntervalLiteral: TfsSqlIntervalLiteral
    Read FIntervalLiteral Write FIntervalLiteral;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlParam = Class(TfsSqlNode)
Protected
  FParmIndex: Integer;
  Function GetSize: Integer; Override;
  Function GetTitle(Const Qualified: boolean): String; {!!.11}
  Function GetType: TfsFieldType; Override;
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Property ParmIndex: Integer Read FParmIndex;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
End;

TfsSqlCoalesceExpression = Class(TfsSqlNode)
Protected
  ArgList: TList;
  Procedure Clear;
  Function GetArg(Index: Integer): TfsSqlSimpleExpression;
  Function GetArgCount: Integer;
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Property ArgCount: Integer Read GetArgCount;
  Property Arg[Index: Integer]: TfsSqlSimpleExpression Read GetArg;
  Function AddArg(Value: TfsSqlSimpleExpression): TfsSqlSimpleExpression;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlWhenClause = Class(TfsSqlNode)
Protected
  FWhenExp: TfsSqlCondExp;
  FThenExp: TfsSqlSimpleExpression;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property WhenExp: TfsSqlCondExp Read FWhenExp Write FWhenExp;
  Property ThenExp: TfsSqlSimpleExpression Read FThenExp Write FThenExp;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlWhenClauseList = Class(TfsSqlNode)
Protected
  WhenClauseList: TList;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function GetWhenClause(Index: Integer): TfsSqlWhenClause;
  Function GetWhenClauseCount: Integer;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Property WhenClauseCount: Integer Read GetWhenClauseCount;
  Property WhenClause[Index: Integer]: TfsSqlWhenClause Read GetWhenClause;
  Function AddWhenClause(Value: TfsSqlWhenClause): TfsSqlWhenClause;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlCaseExpression = Class(TfsSqlNode)
Protected
  FWhenClauseList: TfsSqlWhenClauseList;
  FElseExp: TfsSqlSimpleExpression;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  Procedure CheckIsConstant;
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
  Function IsConstant: Boolean;
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property WhenClauseList: TfsSqlWhenClauseList
    Read FWhenClauseList Write FWhenClauseList;
  Property ElseExp: TfsSqlSimpleExpression Read FElseExp Write FElseExp;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlScalarFunction = (sfCase, sfCharlen, sfCoalesce, sfCurrentDate, sfCurrentTime,
  sfCurrentTimestamp, sfCurrentUser, sfLower, sfUpper, sfPosition,
  sfWeekNo, sfSubstring, sfTrim, sfExtract, sfNullIf, sfCast,
  sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfPower, sfRand, sfRound,
  sfFlags, sfMod, sfDiv, sfODD, sfArray, sfISRECORDLOCKED,
  sfISUNDELETEDRECORD,
  sfISPROTECTDELETERECORD,
  sfISPROTECTUPDATERECORD,
  sfISMARKASBADRECORD);

TfsSqlLTB = (ltbBoth, ltbLeading, ltbTrailing);
TfsSqlScalarFunc = Class(TfsSqlNode)
Protected
  FSQLFunction: TfsSqlScalarFunction;
  FArg1: TfsSqlSimpleExpression;
  FArg2: TfsSqlSimpleExpression;
  FArg3: TfsSqlSimpleExpression;
  FArg4: TfsSqlSimpleExpression;
  FLTB: TfsSqlLTB;
  FXDef: TfsSqlIntervalDef;
  FCaseExp: TfsSqlCaseExpression;
  FCoalesceExp: TfsSqlCoalesceExpression;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  FType: TfsFieldType;
  FDecimals, FSize: Integer;
  fRoundType: TRound;
  fIsCastNull: Boolean;
  TypeKnown: Boolean;
  ConstantValue: Variant;
  fArrayIndex: Boolean;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function IsFieldFrom(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy): Boolean;
  Function GetTitle(Const Qualified: Boolean): String; {!!.11}
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Procedure CheckType;
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public

  Constructor Create(AParent: TfsSqlNode);
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SQLFunction: TfsSqlScalarFunction
    Read FSQLFunction Write FSQLFunction;
  Property Arg1: TfsSqlSimpleExpression Read FArg1 Write FArg1;
  Property Arg2: TfsSqlSimpleExpression Read FArg2 Write FArg2;
  Property Arg3: TfsSqlSimpleExpression Read FArg3 Write FArg3;
  Property Arg4: TfsSqlSimpleExpression Read FArg4 Write FArg4;
  Property LTB: TfsSqlLTB Read FLTB Write FLTB;
  Property RoundType: TRound Read fRoundType Write fRoundType;
  Property XDef: TfsSqlIntervalDef Read FXDef Write FXDef;
  Property CaseExp: TfsSqlCaseExpression Read FCaseExp Write FCaseExp;
  Property CoalesceExp: TfsSqlCoalesceExpression
    Read FCoalesceExp Write FCoalesceExp;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Property DataType: TfsFieldType Read getType Write FType;
  Property Decimals: Integer Read getDecimals Write FDecimals;
  Property Size: Integer Read getSize Write FSize;
  Property IsCastNull: Boolean Read fIsCastNull Write fIsCastNull;
  Property ArrayIndex: Boolean Read fArrayIndex Write fArrayIndex;
End;

TfsSqlMulOp = (moMul, moDiv);
TfsSqlFactor = Class(TfsSqlNode)
Protected
  TypeKnown: Boolean;
  FType: TfsFieldType;
  FMulOp: TfsSqlMulOp;
  FUnaryMinus: Boolean;
  FCondExp: TfsSqlCondExp;
  FFieldRef: TfsSqlFieldRef;
  FLiteral: TfsSqlLiteral;
  FParam: TfsSqlParam;
  FAggregate: TfsSqlAggregate;
  FSubQuery: TfsSqlSELECT;
  FScalarFunc: TfsSqlScalarFunc;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function GetDecimals: Integer; Override;
  Function GetSize: Integer; Override;
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Procedure CheckType;
  Function GetTitle(Const Qualified: boolean): String; {!!.11}
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function IsAggregate: Boolean; Override;
  Function AddIntervalTo(Target: TDateTime): TDateTime;
  Function Reduce: Boolean;
  Function SubtractIntervalFrom(Target: TDateTime): TDateTime;
  Procedure ResetConstant; Override;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Destructor Destroy; Override;
  Property MulOp: TfsSqlMulOp Read FMulOp Write FMulOp;
  Property UnaryMinus: Boolean Read FUnaryMinus Write FUnaryMinus;
  Property CondExp: TfsSqlCondExp Read FCondExp Write FCondExp;
  Property FieldRef: TfsSqlFieldRef Read FFieldRef Write FFieldRef;
  Property Literal: TfsSqlLiteral Read FLiteral Write FLiteral;
  Property Param: TfsSqlParam Read FParam Write FParam;
  Property Aggregate: TfsSqlAggregate Read FAggregate Write FAggregate;
  Property SubQuery: TfsSqlSELECT Read FSubQuery Write FSubQuery;
  Property ScalarFunc: TfsSqlScalarFunc Read FScalarFunc Write FScalarFunc;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function HasFieldRef: Boolean;
  Function IsField(Var FieldReferenced: TfsSqlFieldProxy): Boolean;
  Function IsFieldFrom(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy; Var SameCase: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function IsNull: Boolean;
  Function WasWildcard: Boolean; {!!.11}
End;

TfsSqlAddOp = (aoPlus, aoMinus, aoConcat);
TfsSqlTerm = Class(TfsSqlNode)
Protected
  TypeKnown: Boolean;
  FType: TfsFieldType;
  FAddOp: TfsSqlAddOp;
  FactorList: TList;
  FIsConstantChecked: Boolean;
  FIsConstant: Boolean;
  ConstantValue: Variant;
  Procedure Clear;
  Procedure CheckIsConstant;
  Function IsConstant: Boolean;
  Function GetFactor(Index: Integer): TfsSqlFactor;
  Procedure SetFactor(Index: Integer; Const Value: TfsSqlFactor);
  Function GetFactorCount: Integer;
  Function GetDecimals: Integer; Override;
  Function GetSize: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Procedure CheckType;
  Function GetTitle(Const Qualified: Boolean): String; {!!.11}
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function IsAggregate: Boolean; Override;
  //function GetAgg: TfsSqlAggregate; Override;
  Function AddIntervalTo(Target: TDateTime): TDateTime;
  Function SubtractIntervalFrom(Target: TDateTime): TDateTime;
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddFactor(Factor: TfsSqlFactor): TfsSqlFactor;
  Property FactorCount: Integer Read GetFactorCount;
  Property Factor[Index: Integer]: TfsSqlFactor Read GetFactor Write SetFactor;
  Property AddOp: TfsSqlAddOp Read FAddOp Write FAddOp;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function HasFieldRef: Boolean;
  Function IsField(Var FieldReferenced: TfsSqlFieldProxy): Boolean;
  Function IsFieldFrom(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy; Var SameCase: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function IsAggregateExpression: Boolean;
  Function IsNull: Boolean;
  Function WasWildcard: Boolean; {!!.11}
End;

TfsSqlSimpleExpression = Class(TfsSqlNode)
Protected
  TypeKnown: Boolean;
  FType: TfsFieldType;
  BoundHaving: Boolean;
  BoundHavingField: TfsSqlFieldProxy;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  ConstantValue: Variant;
  BindingHaving: Boolean;
  Procedure BindHaving;
  Procedure Clear;
  Function ConcatBLOBValues(Const Value1, Value2: Variant): Variant; {!!.13}
  Function GetTerm(Index: Integer): TfsSqlTerm;
  Procedure SetTerm(Index: Integer; Const Value: TfsSqlTerm);
  Function GetTermCount: Integer;
  Function GetSize: Integer; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Procedure CheckType;
  Function GetTitle(Const Qualified: Boolean): String; {!!.11}
  Procedure MatchType(ExpectedType: TfsFieldType);
  Function IsAggregate: Boolean; Override;
  Function IsConstant: Boolean;
  Function IsParameter: Boolean;
  Procedure CheckIsConstant;
  Function Reduce: Boolean;
  Procedure ResetConstant; Override;
Protected
  TermList: TList;
Public
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddTerm(Term: TfsSqlTerm): TfsSqlTerm;
  Property TermCount: Integer Read GetTermCount;
  Property Term[Index: Integer]: TfsSqlTerm Read GetTerm Write SetTerm;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function HasFieldRef: Boolean;
  Function IsField(Var FieldReferenced: TfsSqlFieldProxy): Boolean;
  Function IsFieldFrom(Table: TFSSqlTableProxy;
    Var FieldReferenced: TfsSqlFieldProxy; Var SameCase: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function IsAggregateExpression: Boolean;
  Function IsNull: Boolean;
  Function WasWildcard: Boolean; {!!.11}
End;

TfsSqlSimpleExpressionList = Class(TfsSqlNode)
Protected
  FExpressionList: TList;
  FIsConstant: Boolean;
  FIsConstantChecked: Boolean;
  Procedure CheckIsConstant;
  Procedure Clear;
  Function IsConstant: Boolean;
  Function GetExpression(Index: Integer): TfsSqlSimpleExpression;
  Function GetExpressionCount: Integer;
  Procedure SetExpression(Index: Integer;
    Const Value: TfsSqlSimpleExpression);
  Procedure MatchType(ExpectedType: TfsFieldType);
Function Contains(Const TestValue: Variant): Boolean;
Function Reduce: Boolean;
Procedure ResetConstant; Override;
Public
Procedure Assign(Const Source: TfsSqlNode); Override;
Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
Constructor Create(AParent: TfsSqlNode);
Destructor Destroy; Override;
Function AddExpression(Expression: TfsSqlSimpleExpression):
  TfsSqlSimpleExpression;
Property ExpressionCount: Integer Read GetExpressionCount;
Property Expression[Index: Integer]: TfsSqlSimpleExpression
Read GetExpression Write SetExpression;
Procedure EmitSQL(Stream: TStream); Override;
Function Equals(Other: TfsSqlNode): Boolean; Override;
Function DependsOn(Table: TFSSqlTableProxy): Boolean;
End;

TfsSqlSelection = Class(TfsSqlNode)
Protected
  FColumn: TfsSqlColumn;
  FSimpleExpression: TfsSqlSimpleExpression;
  AddedByWildcard: Boolean;
  Procedure AddColumnDef(Target: TfsSqlColumnListOwner); Override;
  Procedure AddDistinctColumnDef(Target: TfsSqlColumnListOwner); Override;
  Function GetIndex: Integer;
  Function Reduce: Boolean;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Property SimpleExpression: TfsSqlSimpleExpression
    Read FSimpleExpression Write FSimpleExpression;
  Property Column: TfsSqlColumn Read FColumn Write FColumn;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Property Index: Integer Read GetIndex;
  Function IsAggregateExpression: Boolean;
End;

TfsSqlSelectionList = Class(TfsSqlNode)
Protected
  FSelections: TList;
  Procedure Clear;
  Function GetSelection(Index: Integer): TfsSqlSelection;
  Procedure SetSelection(Index: Integer;
    Const Value: TfsSqlSelection);
  Function GetSelectionCount: Integer;
  Function Reduce: Boolean;
  //    Procedure ResetConstant; Override;
  Function GetNonWildSelection(Index: Integer): TfsSqlSelection;
  Property NonWildSelection[Index: Integer]: TfsSqlSelection
  Read GetNonWildSelection;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddSelection(NewSelection: TfsSqlSelection): TfsSqlSelection;
  Procedure InsertSelection(Index: Integer; NewSelection: TfsSqlSelection);
  Property SelectionCount: Integer Read GetSelectionCount;
  Property Selection[Index: Integer]: TfsSqlSelection
  Read GetSelection Write SetSelection;
  Function FindSelection(GroupCol: TfsSqlGroupColumn): TfsSqlSelection;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function NonWildSelectionCount: Integer;
End;

TfsSqlInsertColumnList = Class;

TfsSqlTableRef = Class(TfsSqlNode)
Protected
  FAlias: String;
  FTableName: String;
  fIndexName: String;
  FParamName: String;
  FTableExp: TfsSqlTableExp;
  FColumnList: TfsSqlInsertColumnList;
  FDatabaseName: String;
  FTable: TFSSqlTableProxy;
  Procedure AddTableReference(Select: TfsSqlSELECT); Override;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function GetResultTable: TFSSqlTableProxy;
  Function GetSQLName: String;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function BindTable(AOwner: TObject; Const TableName: String): TFSSqlTableProxy;
  Function Reduce: Boolean; {!!.11}
  Function TargetFieldFromSourceField(Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property TableName: String Read FTableName Write FTableName;
  Property IndexName: String Read fIndexName Write fIndexName;
  Property ParamName: String Read FParamName Write FParamName;
  Property DataBaseName: String Read FDatabaseName Write FDatabaseName;
  Property Alias: String Read FAlias Write FAlias;
  Property TableExp: TfsSqlTableExp Read FTableExp Write FTableExp;
  Property ColumnList: TfsSqlInsertColumnList
    Read FColumnList Write FColumnList;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Clear;
  Destructor Destroy; Override;
  Property SQLName: String Read GetSQLName;
  Function GetTable(AOwner: TObject; Const OpenMode: TffOpenMode; Const ExclContLock: Boolean): TFSSqlTableProxy;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
End;

TfsSqlTableRefList = Class(TfsSqlNode)
Protected
  FTableRefList: TList;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Procedure Clear;
  Function GetTableRef(Index: Integer): TfsSqlTableRef;
  Procedure SetTableRef(Index: Integer;
    Const Value: TfsSqlTableRef);
  Function GetTableRefCount: Integer;
  Function Reduce: Boolean;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function GetItem(Index: Integer): TfsSqlTableRef;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddTableRef(NewTableRef: TfsSqlTableRef): TfsSqlTableRef;
  Function GetNameForAlias(Const Alias: String): String;
  Function GetIndexName(Const AliasOrTableName: String): String;
  Property TableRefCount: Integer Read GetTableRefCount;
  Property TableRef[Index: Integer]: TfsSqlTableRef
  Read GetTableRef Write SetTableRef;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function GetFieldsFromTable(Const TableName: String; List: TList): TFSSqlTableProxy;
  Property Item[Index: Integer]: TfsSqlTableRef Read GetItem; Default;
End;

TfsSqlOrderItem = Class(TfsSqlNode)
Protected
  FColumn: TfsSqlOrderColumn;
  FIndex: String;
  FDescending, fNoCase, fNullTop: Boolean;
  FSize: Integer;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Property Column: TfsSqlOrderColumn Read FColumn Write FColumn;
  Property Index: String Read FIndex Write FIndex;
  Property Descending: Boolean Read FDescending Write FDescending;
  Property NoCase: Boolean Read FNoCase Write FNoCase;
  Property NullTop: Boolean Read FNullTop Write FNullTop;
  Property Size: Integer Read FSize Write FSize;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
End;

TfsSqlOrderList = Class(TfsSqlNode)
Protected
  FOrderItemList: TList;
  Procedure Clear;
  Function GetOrderItem(Index: Integer): TfsSqlOrderItem;
  Procedure SetOrderItem(Index: Integer;
    Const Value: TfsSqlOrderItem);
  Function GetOrderCount: Integer;
  Function Reduce: Boolean;
Public
  UOrder: Boolean;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddOrderItem(NewOrder: TfsSqlOrderItem): TfsSqlOrderItem;
  Property OrderCount: Integer Read GetOrderCount;
  Property OrderItem[Index: Integer]: TfsSqlOrderItem
  Read GetOrderItem Write SetOrderItem;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
End;

TfsSqlOuterJoinMode = (jmNone, jmLeft, jmRight, jmFull);
TfsSqlJoiner = Class(TFSSpecObject)
Protected
  FSources: TFSSqlTableProxySubsetList;
  FTargetTable: TFSSqlTableProxy;
  Level: Integer;
  FRecordsRead: Longint;
  {$IFDEF CountWrites}
  FRecordsWritten: Longint;
  {$ENDIF}
  FieldCopier: TfsFieldCopier;
  FSX, FT: TList;
  FCondExpWhere: TfsSqlCondExp;
  RecListL, RecListR,
    DupList: TfsNRecordHash;
  FirstCondTerm, LastCondTerm: Boolean;
  OptimizeCalled: Boolean;
  WasOptimized: Boolean;
  P: Procedure Of Object;
  FOwner: TfsSqlStatement;
  Procedure CreateResultRecord;
  Function ProcessLevel(Cookie1: TffWord32): Boolean;
  Procedure ReadSources;
  Function FindRelation(Term: TfsSqlCondTerm; CurFactor,
    CurFactor2: TfsSqlCondFactor; Table: TFSSqlTableProxy;
    TargetField: TfsSqlFieldProxy;
    Var Operator: TfsSqlRelOp;
    Var ArgExpression: TfsSqlSimpleExpression;
    Var SameCase: Boolean): TfsSqlCondFactor;
  Procedure Optimize(UseIndex: Boolean);
  Function WriteNull(Cookie: TffWord32): Boolean;
Public
  Constructor Create(AOwner: TfsSqlStatement; CondExp: TfsSqlCondExp);
  Destructor Destroy; Override;
  Procedure Execute(UseIndex: Boolean; LoopProc: TFSSpecObjectProc;
    OuterJoinMode: TfsSqlOuterJoinMode);
  Property Sources: TFSSqlTableProxySubsetList Read FSources;
  Procedure AddColumn(
    SourceExpression: TfsSqlSimpleExpression;
    SourceField: TfsSqlFieldProxy;
    Target: TfsSqlFieldProxy);
  Procedure ClearColumnList;
  Property RecordsRead: Longint Read FRecordsRead;
  {$IFDEF CountWrites}
  Property RecordsWritten: Longint Read FRecordsWritten;
  {$ENDIF}
  Property CondExpWhere: TfsSqlCondExp Read FCondExpWhere Write FCondExpWhere;
  Property Target: TFSSqlTableProxy Read FTargetTable Write FTargetTable;
  Property Owner: TfsSqlStatement Read FOwner;
End;

TfsSqlColumnListOwner = Class(TfsSqlNode)
Private
  FCommitBy: Longword;
  fIslimit, fLimitFirst: Boolean;
  fTopDirection: TTopDirection;
  fLimitCount, fLimitDivBy: Longint;
  fLimitPrct, fLimitDivAtOne, fLimitPrctDiv: Boolean;
  fLimitStart: Longint;
  fLimitPrctStart: Boolean;
  fLimitDistinct: Boolean;
Protected
  T: TFSSqlTableProxy; {!!.11}
  Columns, DistinctColumns: TStringList;
  DisplayColumns: TStringList;
Public
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Property CommitBy: Longword Read FCommitBy Write FCommitBy;
  Property TopDirection: TTopDirection Read FTopDirection Write FTopDirection;
  Property LimitCount: Longint Read fLimitCount Write fLimitCount;
  Property LimitDivBy: Longint Read fLimitDivBy Write fLimitDivBy;
  Property LimitPrct: Boolean Read fLimitPrct Write fLimitPrct;
  Property LimitPrctDiv: Boolean Read fLimitPrctDiv Write fLimitPrctDiv;
  Property LimitDivAtOne: Boolean Read fLimitDivAtOne Write fLimitDivAtOne;
  Property LimitPrctStart: Boolean Read fLimitPrctStart Write fLimitPrctStart;
  Property LimitStart: Longint Read fLimitStart Write fLimitStart;
  Property LimitDistinct: Boolean Read fLimitDistinct Write fLimitDistinct;
  Property LimitFirst: Boolean Read fLimitFirst Write fLimitFirst;
  Property Islimit: Boolean Read fIslimit Write fIslimit;
End;

TfsSqlJoinTableExp = Class;
TfsSqlNonJoinTableExp = Class;
TfsSqlNonJoinTableTerm = Class;

TfsSqlTableExp = Class(TfsSqlNode)
Protected
  FJoinTableExp: TfsSqlJoinTableExp;
  FNonJoinTableExp: TfsSqlNonJoinTableExp;
  FNestedTableExp: TfsSqlTableExp;
  FUnionTerm: TfsSqlNonJoinTableTerm;
  FUnion: TfsUnionType;
  fUnionCase: boolean;
  Procedure EnsureResultTable(NeedData: Boolean);
  Function CheckNoDups(Const CaseSensitive: Boolean): Boolean;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Constructor Create(AParent: TfsSqlNode);
  Function GetResultTable: TFSSqlTableProxy;
  Property JoinTableExp: TfsSqlJoinTableExp
    Read FJoinTableExp Write FJoinTableExp;
  Property NonJoinTableExp: TfsSqlNonJoinTableExp
    Read FNonJoinTableExp Write FNonJoinTableExp;
  Property NestedTableExp: TfsSqlTableExp
    Read FNestedTableExp Write FNestedTableExp;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Clear;
  Destructor Destroy; Override;
  Procedure Union2Table(
    aUnion: TfsUnionType;
    aUnionCase: boolean;
    aCursorID1: TffCursorID;
    Var aLiveResult: Boolean;
    Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Procedure EmitSQL(Stream: TStream); Override;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
  Function GetFieldsFromTable(Const TableName: String; List: TList):
    TFSSqlTableProxy;
  Property UnionTerm: TfsSqlNonJoinTableTerm Read FUnionTerm Write FUnionTerm;
  Property Union: TfsUnionType Read fUnion Write fUnion;
  Property UnionCase: boolean Read fUnionCase Write fUnionCase;
End;

TfsSqlUsingItem = Class(TfsSqlNode)
Protected
  FColumnName: String;
Public
  Property ColumnName: String Read FColumnName Write FColumnName;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure EmitSQL(Stream: TStream); Override;
End;

TfsSqlUsingList = Class(TfsSqlNode)
Protected
  FUsingItemList: TList;
  Procedure Clear;
  Function GetUsingItem(Index: Integer): TfsSqlUsingItem;
  Procedure SetUsingItem(Index: Integer;
    Const Value: TfsSqlUsingItem);
  Function GetUsingCount: Integer;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddItem(NewUsing: TfsSqlUsingItem): TfsSqlUsingItem;
  Property UsingCount: Integer Read GetUsingCount;
  Property UsingItem[Index: Integer]: TfsSqlUsingItem
  Read GetUsingItem Write SetUsingItem;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
End;

TfsSqlJoinType = (jtCross, jtInner, jtLeftOuter, jtRightOuter,
  jtFullOuter, jtUnion);
TfsSqlJoinTableExp = Class(TfsSqlNode)
Protected
  FTableRef1: TfsSqlTableRef;
  FTableRef2: TfsSqlTableRef;
  FCondExp: TfsSqlCondExp;
  FJoinType: TfsSqlJoinType;
  FNatural: Boolean;
  Bound: Boolean;
  TL, TR: TFSSqlTableProxy;
  Columns: TStringList;
  Joiner: TfsSqlJoiner;
  FUsingList: TfsSqlUsingList;
  UsingCondExp: TfsSqlCondExp;
  FResultTable: TFSSqlTableProxy;
  HaveData: Boolean;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Function GetResultTable: TFSSqlTableProxy;
  Function Execute2(NeedData: Boolean): TFSSqlTableProxy;
  Procedure Bind;
  Procedure ClearBindings(Node: TfsSqlNode);
  Procedure ClearColumns;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function DoJoin(NeedData: Boolean): TFSSqlTableProxy;
  Function BuildSimpleFieldExpr(AOwner: TfsSqlNode; Const ATableName,
    AFieldName: String;
    AField: TfsSqlFieldProxy): TfsSqlSimpleExpression;
  Procedure EnsureResultTable(NeedData: Boolean);
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Function BindField(Const TableName,
    FieldName: String): TfsSqlFieldProxy; Override;
  Property JoinType: TfsSqlJoinType Read FJoinType Write FJoinType;
  Property Natural: Boolean Read FNatural Write FNatural;
  Property TableRef1: TfsSqlTableRef Read FTableRef1 Write FTableRef1;
  Property TableRef2: TfsSqlTableRef Read FTableRef2 Write FTableRef2;
  Property CondExp: TfsSqlCondExp Read FCondExp Write FCondExp;
  Property UsingList: TfsSqlUsingList Read FUsingList Write FUsingList;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Clear;
  Destructor Destroy; Override;
  Procedure Execute(Var ALiveResult: Boolean;
    Var aCursorID: TffCursorID; Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Procedure EmitSQL(Stream: TStream); Override;
  Constructor Create(AParent: TfsSqlNode);
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
  Function GetFieldsFromTable(Const TableName: String; List: TList):
    TFSSqlTableProxy; {!!.11}
End;

TfsSqlValueList = Class;

TfsSqlNonJoinTablePrimary = Class(TfsSqlNode)
Protected
  FSelectSt: TfsSqlSELECT;
  FValueList: TfsSqlValueList;
  FNonJoinTableExp: TfsSqlNonJoinTableExp;
  FTableRef: TfsSqlTableRef;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Procedure EnsureResultTable(NeedData: Boolean);
  Function GetResultTable: TFSSqlTableProxy;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Destructor Destroy; Override;
  Property SelectSt: TfsSqlSELECT Read FSelectSt Write FSelectSt;
  Property ValueList: TfsSqlValueList Read FValueList Write FValueList;
  Property NonJoinTableExp: TfsSqlNonJoinTableExp
    Read FNonJoinTableExp Write FNonJoinTableExp;
  Property TableRef: TfsSqlTableRef Read FTableRef Write FTableRef;
  Procedure Clear;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Procedure EmitSQL(Stream: TStream); Override;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
End;

TfsSqlNonJoinTableTerm = Class(TfsSqlNode)
Protected
  FUnion: TfsUnionType;
  fUnionCase: boolean;
  FUnionTerm: TfsSqlNonJoinTableTerm;
  FNonJoinTablePrimary: TfsSqlNonJoinTablePrimary;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Procedure EnsureResultTable(NeedData: Boolean);
  Function GetResultTable: TFSSqlTableProxy;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Property NonJoinTablePrimary: TfsSqlNonJoinTablePrimary
    Read FNonJoinTablePrimary Write FNonJoinTablePrimary;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Clear;
  Destructor Destroy; Override;
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Procedure EmitSQL(Stream: TStream); Override;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
  Property Union: TfsUnionType Read FUnion Write FUnion;
  Property UnionCase: boolean Read fUnionCase Write fUnionCase;
  Property UnionTerm: TfsSqlNonJoinTableTerm Read FUnionTerm Write FUnionTerm;
End;

TfsSqlNonJoinTableExp = Class(TfsSqlNode)
Protected
  FNonJoinTableTerm: TfsSqlNonJoinTableTerm;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Function GetResultTable: TFSSqlTableProxy;
  Procedure EnsureResultTable(NeedData: Boolean);
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Function BindFieldDown(Const TableName,
    FieldName: String): TfsSqlFieldProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Public
  Property NonJoinTableTerm: TfsSqlNonJoinTableTerm
    Read FNonJoinTableTerm Write FNonJoinTableTerm;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure Clear;
  Destructor Destroy; Override;
  Procedure Execute(Var ALiveResult: Boolean;
    Var aCursorID: TffCursorID; Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Procedure EmitSQL(Stream: TStream); Override;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
  Function GetFieldsFromTable(Const TableName: String; List: TList):
    TFSSqlTableProxy; {!!.11}
End;

TfsSqlValueItem = Class(TfsSqlNode)
Protected
  FDefault: Boolean;
  FSimplex: TfsSqlSimpleExpression;
  Function GetType: TfsFieldType; Override;
  Function GetRound: TRound; Override;
  Function GetSize: Integer; Override;
  Function GetDecimals: Integer; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Property Default: Boolean Read FDefault Write FDefault;
  Property Simplex: TfsSqlSimpleExpression Read FSimplex Write FSimplex;
End;

TfsSqlValueList = Class(TfsSqlNode)
Protected
  FValueItemList: TList;
  FResultTable: TFSSqlTableProxy;
  Procedure Clear;
  Function GetValueItem(Index: Integer): TfsSqlValueItem;
  Procedure SetValueItem(Index: Integer;
    Const Value: TfsSqlValueItem);
  Function GetValueCount: Integer;
  Function GetResultTable: TFSSqlTableProxy;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddItem(NewValue: TfsSqlValueItem): TfsSqlValueItem;
  Property ValueCount: Integer Read GetValueCount;
  Property ValueItem[Index: Integer]: TfsSqlValueItem
  Read GetValueItem Write SetValueItem;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function Reduce: Boolean;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
End;

TfsSqlSELECT = Class(TfsSqlColumnListOwner)
Protected
  FIsSingleTable: Boolean;
  FDistinct, FDistinctCase, fDistinctOnlyList, FGroupCase, FGroupUnionCase: Boolean;
  FSelectionList: TfsSqlSelectionList;
  fDistinctList: TfsSqlSelectionList;
  FTableRefList: TfsSqlTableRefList;
  FGroupColumnList, FUnionGroupColumnList: TfsSqlGroupColumnList;
  FCondExpWhere: TfsSqlCondExp;
  FCondExpHaving: TfsSqlCondExp;
  FOrderList, FUnionOrderList: TfsSqlOrderList;
  FGrpTable: TFSSqlTableProxy;
  AggList: TList;
  FResultTable: TFSSqlTableProxy;
  TablesReferencedByOrder: TStringList;
  TableAliases: TStringList;
  HaveAggregates: Boolean;
  AggQueryMode: TfsSqlAggQueryMode;
  HavingTable: TFSSqlTableProxy;
  IsDependent: Boolean;
  Bound: Boolean;
  Joiner: TfsSqlJoiner;
  FInWhere: Boolean;
  WasStar: Boolean;
  HaveData: Boolean;
  TypeKnown: Boolean;
  FType: TfsFieldType;
  FDecimals: Integer;
  FSize: Integer;
  FRoundType: TRound;
  BindingDown: Boolean;

  fIsOnline: Boolean;
  fIsSystemTables: Boolean;

  fBlobLevelComp: TDataCompLevel;

  Procedure AddTableFields(Table: TFSSqlTableProxy;
    Const StartPoint: Integer;
    FieldRef: TfsSqlFieldRef);
  Procedure AddTableFieldsFromList(Table: TFSSqlTableProxy;
    Const StartPoint: Integer;
    FieldRef: TfsSqlFieldRef;
    List: TList); {!!.11}
  Procedure Bind;
  Function BindTable(AOwner: TObject;
    Const TableName: String): TFSSqlTableProxy;
  Procedure AddTableRefs(Node: TfsSqlNode);
  Procedure AddColumns(Node: TfsSqlNode);
  Procedure AddDistinctColumns(Node: TfsSqlNode);
  Procedure BuildSortList(Table: TFSSqlTableProxy;
    Var SortList: TfsSqlSortArray; aOrderList: TfsSqlOrderList);
  Procedure DoGroupCopy(aGroupColumnList: TfsSqlGroupColumnList; GroupColumnsIn: Integer; AggExpList,
    GroupColumnTargetField: TList; GroupCase: boolean);
  Procedure DoAggOrderBy;
  Procedure DoHaving;
  Procedure DoSortOnAll;
  Procedure DoRemoveDups(NeedData: Boolean);
  Procedure DoBuildGroupingTable(aGroupColumnList: TfsSqlGroupColumnList; GroupColumnsIn: Integer; FSF, FSX,
    GroupColumnTargetField: TList);
  Procedure DoOrderBy(aOrderList: TfsSqlOrderList; NeedData: Boolean; Table: TFSSqlTableProxy);
  Procedure DoTopDown(Var Table: TFSSqlTableProxy; Var L: Boolean);
  Procedure DoCheckAggregates;
  Function CheckAnyValue(RelOp: TfsSqlRelOp;
    Const Val: Variant): Boolean;
  Function CheckAllValues(RelOp: TfsSqlRelOp;
    Const Val: Variant): Boolean;
  Procedure Clear;
  Procedure ClearBindings(Node: TfsSqlNode);
  Procedure ResetIsConstant(Node: TfsSqlNode);
  Procedure FlagAggregates(Node: TfsSqlNode);
  Procedure EnumAggregates(Node: TfsSqlNode);
  Function BindField(Const TableName,
    FieldName: String): TfsSqlFieldProxy; Override;
  Function FindField(Const FieldName: String): TfsSqlFieldProxy;
  Procedure ExpandWildcards;
  Procedure MatchType(ExpectedType: TfsFieldType; AllowMultiple: Boolean);
  Function NormalQueryResult(NeedData: Boolean): TFSSqlTableProxy;
  Function CheckForValue(Value: Variant): Boolean;
  Function Match(Value: Variant; Unique: Boolean;
    MatchOption: TfsSqlMatchOption): Boolean;
  Function AggregateQueryResult(NeedData: Boolean): TFSSqlTableProxy;
  Function UnionGroupByQueryResult(NeedData: Boolean; aSource: TFSSqlTableProxy): TFSSqlTableProxy;
  Function CheckHaving: Boolean;
  Function Execute2(NeedData: Boolean): TFSSqlTableProxy;
  Procedure EnsureResultTable(NeedData: Boolean);
  Procedure ClearTableList;
  Function Reduce: Boolean;
  Function GetValue(aArrayIndex: Integer): Variant;
  Function CheckNonEmpty: Boolean;
  Function IsSubQuery: Boolean;
  Function GetType: TfsFieldType; Override;
  Function GetDecimals: Integer; Override;
  Function GetRound: TRound; Override;
  Function GetBlobLevel: TDataCompLevel; Override;
  Function GetSize: Integer; Override; {!!.13}
  Function GetResultTable: TFSSqlTableProxy;
  Function TargetFieldFromSourceField(
    Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
  Function TableWithCount(Const ColumnName: String): TFSSqlTableProxy; {!!.12}
Public
  Property InWhere: Boolean Read FInWhere Write FInWhere; //used only during parsing
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Property Distinct: Boolean Read FDistinct Write FDistinct;
  Property DistinctCase: boolean Read FDistinctCase Write FDistinctCase;
  Property DistinctOnlyList: boolean Read fDistinctOnlyList Write fDistinctOnlyList;
  Property GroupCase: boolean Read FGroupCase Write FGroupCase;
  Property GroupUnionCase: boolean Read FGroupUnionCase Write FGroupUnionCase;

  Property IsSystemTables: Boolean Read fIsSystemTables Write fIsSystemTables;
  Property IsOnline: Boolean Read fisOnline Write fIsOnline;

  Property SelectionList: TfsSqlSelectionList
    Read FSelectionList Write FSelectionList;
  Property DistinctList: TfsSqlSelectionList Read fDistinctList Write fDistinctList;
  Property TableRefList: TfsSqlTableRefList
    Read FTableRefList Write FTableRefList;
  Property CondExpWhere: TfsSqlCondExp Read FCondExpWhere Write FCondExpWhere;
  Property GroupColumnList: TfsSqlGroupColumnList
    Read FGroupColumnList Write FGroupColumnList;
  Property UnionGroupColumnList: TfsSqlGroupColumnList
    Read FUnionGroupColumnList Write FUnionGroupColumnList;
  Property CondExpHaving: TfsSqlCondExp
    Read FCondExpHaving Write FCondExpHaving;
  Property OrderList: TfsSqlOrderList Read FOrderList Write FOrderList;
  Property UnionOrderList: TfsSqlOrderList Read FUnionOrderList Write FUnionOrderList;
  Procedure EmitSQL(Stream: TStream); Override;
  Procedure Execute(
    Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
    Var RecordsRead: Integer);
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function DependsOn(Table: TFSSqlTableProxy): Boolean;
  Property ResultTable: TFSSqlTableProxy Read GetResultTable;
End;

TfsSqlInsertItem = Class(TfsSqlNode)
Protected
  FColumnName: String;
  Procedure AddColumnDef(Target: TfsSqlColumnListOwner); Override;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property ColumnName: String Read FColumnName Write FColumnName;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
End;

TfsSqlInsertColumnList = Class(TfsSqlNode)
Protected
  FInsertColumnItemList: TList;
  Procedure Clear;
  Function GetInsertColumnItem(Index: Integer): TfsSqlInsertItem;
  Procedure SetInsertColumnItem(Index: Integer;
    Const Value: TfsSqlInsertItem);
  Function GetInsertColumnCount: Integer;
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddItem(NewInsertColumn: TfsSqlInsertItem): TfsSqlInsertItem;
  Property InsertColumnCount: Integer Read GetInsertColumnCount;
  Property InsertColumnItem[Index: Integer]: TfsSqlInsertItem
  Read GetInsertColumnItem Write SetInsertColumnItem;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
End;

TfsSqlINSERT = Class(TfsSqlColumnListOwner)
Protected
  FTableName, FClientName: String;
  FInsertColumnList: TfsSqlInsertColumnList;
  FDefaultValues: Boolean;
  Bound: Boolean;
  FTableExp: TfsSqlTableExp;
  Procedure AddColumns(Node: TfsSqlNode);
  Procedure Bind;
  Procedure ClearBindings(Node: TfsSqlNode);
  Function Reduce: Boolean; {!!.11}
Public
  Destructor Destroy; Override;
  Property TableName: String Read FTableName Write FTableName;
  Property InsertColumnList: TfsSqlInsertColumnList
    Read FInsertColumnList Write FInsertColumnList;
  Property TableExp: TfsSqlTableExp Read FTableExp Write FTableExp;
  Property DefaultValues: Boolean Read FDefaultValues Write FDefaultValues;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure Clear;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function Execute(Var RowsAffected: Integer): TffResult; {!!.11}
End;

TfsSqlDELETE = Class(TfsSqlColumnListOwner) {!!.11}
Protected
  FTableRef: TfsSqlTableRef;
  FCondExpWhere: TfsSqlCondExp;
  Bound: Boolean;
  Joiner: TfsSqlJoiner;
  DeleteList: TList;
  Procedure Bind;
  Function BindField(Const TableName,
    FieldName: String): TfsSqlFieldProxy; Override;
  Procedure DeleteRecord;
  Function Reduce: Boolean; {!!.11}
Public
  Destructor Destroy; Override;
  Property TableRef: TfsSqlTableRef Read FTableRef Write FTableRef;
  Property CondExpWhere: TfsSqlCondExp
    Read FCondExpWhere Write FCondExpWhere;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure Clear;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function Execute(Var RowsAffected: Integer): TffResult; {!!.11}
End;

TfsSqlUpdateItem = Class(TfsSqlNode)
Protected
  FSimplex: TfsSqlSimpleExpression;
  FColumnName: String;
  FDefault, fNull: Boolean;
  F: TfsSqlFieldProxy;
  Procedure AddColumnDef(Target: TfsSqlColumnListOwner); Override;
  Function Reduce: Boolean; {!!.11}
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Destructor Destroy; Override;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Property ColumnName: String Read FColumnName Write FColumnName;
  Property Default: Boolean Read FDefault Write FDefault;
  Property Simplex: TfsSqlSimpleExpression Read FSimplex Write FSimplex;
  Procedure Update(aArrayIndex: Integer);
End;

TfsSqlUpdateList = Class(TfsSqlNode)
Protected
  FUpdateItemList: TList;
  Procedure Clear;
  Function GetUpdateItem(Index: Integer): TfsSqlUpdateItem;
  Function GetUpdateCount: Integer;
  Function Reduce: Boolean; {!!.11}
Public
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Constructor Create(AParent: TfsSqlNode);
  Destructor Destroy; Override;
  Function AddItem(NewValue: TfsSqlUpdateItem): TfsSqlUpdateItem;
  Property UpdateCount: Integer Read GetUpdateCount;
  Property UpdateItem[Index: Integer]: TfsSqlUpdateItem
  Read GetUpdateItem;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function Update(aArrayIndex: Integer): TffResult; {!!.11}
End;

TfsSqlUPDATE = Class(TfsSqlColumnListOwner)
Protected
  FTableRef: TfsSqlTableRef;
  FCondExpWhere: TfsSqlCondExp;
  FUpdateList: TfsSqlUpdateList;
  Bound: Boolean;
  FClientName: String; {!!.11}
  Joiner: TfsSqlJoiner;
  FRowsAffected: Integer;
  UpdateRecList: TList;
  Procedure AddColumns(Node: TfsSqlNode);
  Procedure Bind;
  Function BindField(Const TableName,
    FieldName: String): TfsSqlFieldProxy; Override;
  Procedure ClearBindings(Node: TfsSqlNode);
  Function Reduce: Boolean; {!!.11}
  Procedure UpdateRecord;
Public
  Destructor Destroy; Override;
  Property TableRef: TfsSqlTableRef Read FTableRef Write FTableRef;
  Property CondExpWhere: TfsSqlCondExp Read FCondExpWhere Write FCondExpWhere;
  Property UpdateList: TfsSqlUpdateList Read FUpdateList Write FUpdateList;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure Clear;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Procedure EmitSQL(Stream: TStream); Override;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Function Execute(Var RowsAffected: Integer): TffResult; {!!.11}
End;

TfsSqlStatement = Class(TfsSqlNode)
Protected
  FClientID: TffClientID;
  FClientName: TffNetName;
  FSessionID: TffSessionID;
  FInsert: TfsSqlINSERT;
  StartDate,
    StartDateTime,
    StartTime: TDateTime;
  ParmCount: Integer;
  ParmList: TFSVariantList;
  FUseIndex: Boolean;
  FUpdate: TfsSqlUPDATE;
  FDelete: TfsSqlDELETE;
  FReduce: Boolean;
  FDatabase: TfsSqlDatabaseProxy;
  RecordsRead: Integer;
  //FTableExp: TfsSqlTableExp;
  FTableExpList: TList;

Public
  Procedure AddTableExp(Value: TfsSqlTableExp);
  Function TableExp(Index: Integer): TfsSqlTableExp;
  Procedure ClearTableExp;
  Property UseIndex: Boolean Read FUseIndex Write FUseIndex;
  Property Reduce: Boolean Read FReduce Write FReduce;
  Procedure Assign(Const Source: TfsSqlNode); Override;
  Procedure EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean); Override;
  Property Insert: TfsSqlINSERT Read FInsert Write FInsert;
  Property Update: TfsSqlUPDATE Read FUpdate Write FUpdate;
  Property Delete: TfsSqlDELETE Read FDelete Write FDelete;
  Property TableExpList: TList Read FTableExpList Write FTableExpList;
  //Property TableExp: TfsSqlTableExp Read FTableExp Write FTableExp;
  Constructor Create;
  Destructor Destroy; Override;
  Procedure Bind(Const ClientID: TffClientID;
    Const SessionID: TffSessionID;
    Database: TfsSqlDatabaseProxy);
  Procedure EmitSQL(Stream: TStream); Override;
  {- write the SQL statement represented by this hierarchy}
  Function Execute(Var ALiveResult: Boolean;
    Var aCursorID: TffCursorID;
    Var RowsAffected,
    aRecordsRead: Integer): TffResult;
  Function Equals(Other: TfsSqlNode): Boolean; Override;
  Procedure SetParameter(Index: Integer; Value: Variant);
  Procedure ReduceStrength;
  Property Owner: TfsSqlStatement Read FOwner;
  Procedure Clear;
End;

TfsGroupColumnTargetInfo = Class(TFSSpecObject)
  { This Class helps correlate a selection field to a slot in the
  LastValues list that is created when grouping fields. There
  is not a one-to-one correspondence between the two lists
  because the Group By clause may reference fields not in the
  selection list. }
Public
  SelFldIndex,
    LastValueIndex: Longint;
End;

{$IFDEF ExposeLastStatement}
Var
  LastStatement: TfsSqlStatement; {debug hook}
  {$ENDIF}

Implementation

Uses
  fsllExcp,
  fssrbase,
  fssrbde,
  fssrlock,
  fsutil,
  Math; {!!.11}

{$I fsconst.inc}

Var
  TimeDelta: Double;
  OsChar: Array[0..255] Of AnsiChar;
  iC: Integer;

Const
  fsRelOpStr: Array[TfsSqlRelOp] Of String =
  ('', '=', '<=', '<', '>', '>=', '<>');
  fsDefStr: Array[TfsSqlIntervalDef] Of String = (
    'Unspec', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE', 'SECOND');
  fsCanOptimizeOnOperator: Array[TfsSqlRelOp] Of Boolean = (
    {roNone, roEQ, roLE, roL, roG, roGE, roNE}
    False, True, True, True, True, True, False);
  fsAgString: Array[TfsSqlAggFunction] Of String =
  ('COUNT', 'MIN', 'MAX', 'SUM', 'AVG');
  fsSqlInConvThreshold = 8; {maximum length Of expression list in
  an IN clause to convert to simple expressions}

Function fsPosCh(Const SearchCh: Char; Const SearchString: String): Integer;
{-same as POS but searches For a single Char}
Var
  Len: Integer;
Begin
  Len := length(SearchString);
  If Len <> 0 Then
    Begin
      Result := 1;
      Repeat
        If SearchString[Result] = SearchCh Then
          Exit;
        inc(Result);
      Until Result > Len;
    End;
  Result := 0;
End;

Function fsPosChI(Const SearchCh: Char; Const SearchString: String): Integer;
{-same as PosCh above, but ignores case}
Var
  Len: Integer;
  SearchChU: Char;
Begin
  Len := length(SearchString);
  If Len <> 0 Then
    Begin
      SearchChU := UpCase(SearchCh);
      Result := 1;
      Repeat
        If SearchString[Result] = SearchCh Then
          Exit;
        If UpCase(SearchString[Result]) = SearchChU Then
          Exit;
        inc(Result);
      Until Result > Len;
    End;
  Result := 0;
End;

Function fsPosI(Const SearchFor, SearchIn: String): Integer;
{-same as POS but ignores Case on both strings}
Var
  LenFor, LenIn, j: Integer;
  FirstCh: Char;
Begin
  LenFor := length(SearchFor);
  If LenFor = 0 Then
    Begin
      Result := 0;
      Exit;
    End;
  Result := fsPosChI(SearchFor[1], SearchIn);
  If (Result = 0) Or (LenFor = 1) Then
    Exit;
  LenIn := length(SearchIn);
  If LenIn <> 0 Then
    Begin
      dec(LenIn, LenFor);
      FirstCh := UpCase(SearchFor[1]);
      Repeat
        If UpCase(SearchIn[Result]) = FirstCh Then
          Begin
            J := 1;
            Repeat
              inc(J);
            Until (J > LenFor) Or (UpCase(SearchIn[Result + J - 1]) <> UpCase(SearchFor[J]));
            If J > LenFor Then
              Exit;
          End;
        inc(Result);
      Until Result > LenIn;
    End;
  Result := 0;
End;

{$IFNDEF DCC5OrLater}

Function CompareText(Const S1, S2: String): Integer; Assembler;
Asm
PUSH    ESI
PUSH    EDI
PUSH    EBX
MOV     ESI,EAX
MOV     EDI,EDX
Or      EAX,EAX
JE      @@0
MOV     EAX,[EAX-4]
@@0:    Or      EDX,EDX
JE      @@1
MOV     EDX,[EDX-4]
@@1:    MOV     ECX,EAX
CMP     ECX,EDX
JBE     @@2
MOV     ECX,EDX
@@2:    CMP     ECX,ECX
@@3:    REPE    CMPSB
JE      @@6
MOV     BL,BYTE PTR [ESI-1]
CMP     BL,'a'
JB      @@4
CMP     BL,'z'
JA      @@4
SUB     BL,20H
@@4:    MOV     BH,BYTE PTR [EDI-1]
CMP     BH,'a'
JB      @@5
CMP     BH,'z'
JA      @@5
SUB     BH,20H
@@5:    CMP     BL,BH
JE      @@3
MOVZX   EAX,BL
MOVZX   EDX,BH
@@6:    SUB     EAX,EDX
POP     EBX
POP     EDI
POP     ESI
End;

Function SameText(Const S1, S2: String): Boolean; Assembler;
Asm
CMP     EAX,EDX
JZ      @1
Or      EAX,EAX
JZ      @2
Or      EDX,EDX
JZ      @3
MOV     ECX,[EAX-4]
CMP     ECX,[EDX-4]
JNE     @3
CALL    CompareText
TEST    EAX,EAX
JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
End;
{$ENDIF}

Type
  TfsReadSourceEvent = Procedure(Sender: TObject;
    Var OkToCopy: boolean) Of Object;
  TfsEvaluateFieldEvent = Procedure(Sender: TObject;
    ColumnIndex: Integer; Var Res: variant) Of Object;

Function fsCreateLiteralStringExp(Parent: TfsSqlNode; Const S: String): TfsSqlSimpleExpression;
Var
  T: TfsSqlTerm;
  F: TfsSqlFactor;
  L: TfsSqlLiteral;
  SL: TfsSqlStringLiteral;
Begin
  Result := TfsSqlSimpleExpression.Create(Parent);
  T := TfsSqlTerm.Create(Result);
  F := TfsSqlFactor.Create(T);
  L := TfsSqlLiteral.Create(F);
  SL := TfsSqlStringLiteral.Create(L);
  SL.Value := '''' + S + '''';
  //Sl.NullStringValue:= S;
 // Sl.Converted:= true;
  L.StringLiteral := SL;
  F.Literal := L;
  T.AddFactor(F);
  Result.AddTerm(T);
End;

Constructor TfsSqlJoiner.Create(AOwner: TfsSqlStatement;
  CondExp: TfsSqlCondExp);
Begin
  Assert(AOwner <> Nil);
  Inherited Create;
  FOwner := AOwner;
  FCondExpWhere := CondExp;
  FSources := TFSSqlTableProxySubsetList.Create(AOwner);
  FieldCopier := TfsFieldCopier.Create;
  FSX := TList.Create;
  FT := TList.Create;
End;

Destructor TfsSqlJoiner.Destroy;
Begin
  FieldCopier.Free;
  FSX.Free;
  FT.Free;
  FSources.Free;
  Inherited Destroy;
End;

Procedure TfsSqlJoiner.AddColumn(
  SourceExpression: TfsSqlSimpleExpression;
  SourceField: TfsSqlFieldProxy;
  Target: TfsSqlFieldProxy);
Begin
  Assert((SourceExpression = Nil) Or (SourceField = Nil));
  If (SourceExpression = Nil) And (SourceField = Nil) Then {!!.13}
    FSX.Add(Pointer(1)) // flag - see CreateResultRecord               {!!.13}
  Else {!!.13}
    FSX.Add(SourceExpression);
  Target.IsTarget := True;
  If SourceField <> Nil Then
    Begin
      FieldCopier.Add(SourceField, Target);
      Target.SrcField := SourceField;
    End
  Else
    Target.SrcIndex := Pred(FSX.Count);
  FT.Add(Target);
End;

Procedure TfsSqlJoiner.ClearColumnList;
Begin
  FSX.Clear;
  FT.Clear;
  FieldCopier.Free;
  FieldCopier := TfsFieldCopier.Create;
End;

Function TfsSqlJoiner.ProcessLevel(Cookie1: TffWord32): Boolean;
Begin
  inc(FRecordsRead);
  inc(Owner.RecordsRead);
  { Time to check For timeout? }
  If FRecordsRead Mod 1000 = 0 Then
    FFCheckRemainingTime;
  If Level > 0 Then
    Begin
      If (CondExpWhere = Nil) Or CondExpWhere.AsBooleanLevel(Level) Then
        Begin
          dec(Level);
          ReadSources;
          inc(Level);
        End;
    End
  Else If (CondExpWhere = Nil) Or CondExpWhere.AsBoolean Then
    P;
  Result := True; {continue}
End;

Procedure TfsSqlJoiner.CreateResultRecord;
Var
  i: Integer;
  V: Variant;
Begin
  If (DupList <> Nil)
    And Not FirstCondTerm
    And DupList.Exists Then
    Exit;
  FTargetTable.Insert;
  For i := 0 To pred(FTargetTable.FieldCount) Do
    If FSX[i] <> Nil Then
      Begin
        //funkcje
        If Integer(FSX[i]) = 1 Then
          TfsSqlFieldProxy(Ft[i]).SetValue(1, -1)
        Else
          Begin {!!.13}
            V := TfsSqlSimpleExpression(FSX[i]).GetValue(-1);
            TfsSqlFieldProxy(Ft[i]).SetValue(V, -1);
          End; {!!.13}
      End;
  FieldCopier.Execute;
  FTargetTable.InsertPost;
  If (DupList <> Nil)
    And Not LastCondTerm Then
    DupList.Add;
  {$IFDEF CountWrites}
  inc(FRecordsWritten);
  {$ENDIF}
  If assigned(RecListL) Then
    If Not RecListL.Exists Then
      RecListL.Add;
  If assigned(RecListR) Then
    If Not RecListR.Exists Then
      RecListR.Add;
End;

Function TfsSqlJoiner.WriteNull(Cookie: TffWord32): Boolean;
Begin
  If Not TfsNRecordHash(Cookie).Exists Then
    CreateResultRecord;
  Result := True; {continue}
End;

Procedure TfsSqlJoiner.ReadSources;
Begin
  With Sources.Item[Level] Do
    Iterate(ProcessLevel, 0);
End;

Function TfsSqlJoiner.FindRelation(
  Term: TfsSqlCondTerm;
  CurFactor, CurFactor2: TfsSqlCondFactor;
  Table: TFSSqlTableProxy;
  TargetField: TfsSqlFieldProxy;
  Var Operator: TfsSqlRelOp;
  Var ArgExpression: TfsSqlSimpleExpression;
  Var SameCase: Boolean): TfsSqlCondFactor;
Var
  k, l: Integer;
  F: TfsSqlFieldProxy;
  DepFound: Boolean;
Begin
  With Term Do
    Begin
      For k := 0 To pred(CondFactorCount) Do
        If (CondFactor[k] <> CurFactor)
          And (CondFactor[k] <> CurFactor2)
          And Not OrderedSources.RelationUsed(CondFactor[k]) Then
          With CondFactor[k] Do
            If IsRelationTo(Table,
              F, Operator, ArgExpression, SameCase)
              And fsCanOptimizeOnOperator[Operator] Then
              Begin
                If F = TargetField Then
                  Begin
                    {check that it doesn't depend on something we haven't seen
                    at this point}
                    DepFound := False;

                    For l := 0 To pred(OrderedSources.Count) Do
                      If ArgExpression.DependsOn(OrderedSources.Item[l].Table) Then
                        Begin
                          DepFound := True;
                          break;
                        End;

                    If Not DepFound Then
                      Begin
                        Result := CondFactor[k];
                        Exit;
                      End;
                  End;
              End;
    End;
  Result := Nil;
End;

Procedure TfsSqlJoiner.Execute(UseIndex: Boolean; LoopProc: TFSSpecObjectProc;
  OuterJoinMode: TfsSqlOuterJoinMode);
Var
  i: Integer;
Begin
  FRecordsRead := 0;
  {$IFDEF CountWrites}
  FRecordsWritten := 0;
  {$ENDIF}

  If assigned(LoopProc) Then
    P := LoopProc
  Else
    P := CreateResultRecord;

  Case OuterJoinMode Of
    jmLeft, jmFull:
      Begin
        Sources.Item[0].Outer := True;
        Sources.Item[0].Opposite := Sources.Item[1].Table;
        Sources.OuterJoin := True;
      End;
    jmRight:
      Begin
        Sources.Item[1].Outer := True;
        Sources.Item[1].Opposite := Sources.Item[0].Table;
        Sources.OuterJoin := True;
      End;
  End;

  Optimize(UseIndex);

  If WasOptimized Then
    Begin
      Try
        If CondExpWhere.GetCondTermCount > 1 Then
          Begin
            DupList := TfsNRecordHash.Create;
            For i := 0 To pred(Sources.Count) Do
              Duplist.AddTable(Sources.Item[i].Table);
          End
        Else
          DupList := Nil;

        {process each term separately}
        FirstCondTerm := True;
        For i := 0 To pred(CondExpWhere.GetCondTermCount) Do
          Begin
            LastCondTerm := i = pred(CondExpWhere.GetCondTermCount);
            With CondExpWhere.CondTerm[i] Do
              Begin
                OrderedSources.OuterJoin := OuterJoinMode <> jmNone;
                OrderedSources.Join(CondExpWhere.CondTerm[i], P);
              End;
            FirstCondTerm := False;
          End;
      Finally
        If Assigned(DupList) Then
          Begin
            DupList.Free;
            DupList := Nil;
          End;
      End;

      If OuterJoinMode = jmFull Then
        Begin
          Sources.Item[0].Outer := False;
          Sources.Item[1].Outer := True;
          Sources.Item[1].Opposite := Sources.Item[0].Table;

          OptimizeCalled := False;
          Optimize(UseIndex);

          If WasOptimized Then
            Begin
              Try
                If CondExpWhere.GetCondTermCount > 1 Then
                  Begin
                    DupList := TfsNRecordHash.Create;
                    For i := 0 To pred(Sources.Count) Do
                      Duplist.AddTable(Sources.Item[i].Table);
                  End
                Else
                  DupList := Nil;

                {process each term separately}
                FirstCondTerm := True;
                For i := 0 To pred(CondExpWhere.GetCondTermCount) Do
                  Begin
                    LastCondTerm := i = pred(CondExpWhere.GetCondTermCount);
                    With CondExpWhere.CondTerm[i] Do
                      Begin
                        OrderedSources.OuterJoin := True;
                        OrderedSources.SkipInner := True;
                        OrderedSources.Join(CondExpWhere.CondTerm[i], P);
                      End;
                    FirstCondTerm := False;
                  End;

              Finally
                If Assigned(DupList) Then
                  Begin
                    DupList.Free;
                    DupList := Nil;
                  End;
              End;

            End
          Else
            Begin
              If CondExpWhere <> Nil Then
                CondExpWhere.SetLevelDep(Sources);
              Level := Sources.Count - 1;
              ReadSources;
            End;
          OptimizeCalled := False;

        End;

    End
  Else
    Begin
      Case OuterJoinMode Of
        jmLeft:
          Begin
            RecListL := TfsNRecordHash.Create;
            ReclistL.AddTable(Sources.Item[0].Table);
          End;
        jmRight:
          Begin
            RecListR := TfsNRecordHash.Create;
            ReclistR.AddTable(Sources.Item[1].Table);
          End;
        jmFull:
          Begin
            RecListL := TfsNRecordHash.Create;
            ReclistL.AddTable(Sources.Item[0].Table);
            RecListR := TfsNRecordHash.Create;
            ReclistR.AddTable(Sources.Item[1].Table);
          End;
      End;
      If CondExpWhere <> Nil Then
        CondExpWhere.SetLevelDep(Sources);
      Level := Sources.Count - 1;
      ReadSources;
      Case OuterJoinMode Of
        jmLeft:
          Begin
            Sources.Item[1].Table.NullRecord;
            Sources.Item[0].Table.Iterate(WriteNull, TffWord32(RecListL));
            RecListL.Free;
            RecListL := Nil;
          End;
        jmRight:
          Begin
            Sources.Item[0].Table.NullRecord;
            Sources.Item[1].Table.Iterate(WriteNull, TffWord32(RecListR));
            RecListR.Free;
            RecListR := Nil;
          End;
        jmFull:
          Begin
            Sources.Item[1].Table.NullRecord;
            Sources.Item[0].Table.Iterate(WriteNull, TffWord32(RecListL));
            Sources.Item[0].Table.NullRecord;
            Sources.Item[1].Table.Iterate(WriteNull, TffWord32(RecListR));
            RecListL.Free;
            RecListL := Nil;
            RecListR.Free;
            RecListR := Nil;
          End;
      End;
    End;
End;

Function CompareRelations(Const R1, R2: TFSSqlTableProxySubset): Boolean;
{ Returns True If R1 is 'better' than R2, e.g. it is likely to better
  limit the number Of rows we have to read to produce a result}
Var
  U1, U2: Boolean;
  I1, I2: Integer;
Begin
  If R2 = Nil Then
    Begin
      Result := True;
      Exit;
    End;
  {$IFDEF LogIndexAnalysis}
  writeln(IALog, ' Comparing relations');
  writeln(IALog, '   Rel1:');
  writeln(IALog, '     Table name:', R1.Table.Name, ' (', R1.Table.Alias, ')');
  writeln(IALog, '     Unique:', R1.UniqueValue);
  writeln(IALog, '     Closed segment:', R1.ClosedSegment);
  writeln(IALog, '     Equal key depth:', R1.EqualKeyDepth);
  writeln(IALog, '     Key depth:', R1.KeyDepth);
  writeln(IALog, '     Relation key is unique:', R1.KeyRelation.RelationKeyIsUnique);
  writeln(IALog, '     Relation key is Case insensitive:', R1.KeyRelation.RelationKeyIsCaseInsensitive);
  writeln(IALog, '     Record count:', R1.Table.GetRecordCount);
  writeln(IALog, '       Expression:', R1.KeyRelation.CondF.SqlText);
  writeln(IALog, '   Rel2:');
  writeln(IALog, '     Table name:', R2.Table.Name, ' (', R2.Table.Alias, ')');
  writeln(IALog, '     Unique:', R2.UniqueValue);
  writeln(IALog, '     Closed segment:', R2.ClosedSegment);
  writeln(IALog, '     Equal key depth:', R2.EqualKeyDepth);
  writeln(IALog, '     Key depth:', R2.KeyDepth);
  writeln(IALog, '     Relation key is unique:', R2.KeyRelation.RelationKeyIsUnique);
  writeln(IALog, '     Relation key is Case insensitive:', R2.KeyRelation.RelationKeyIsCaseInsensitive);
  writeln(IALog, '     Record count:', R2.Table.GetRecordCount);
  writeln(IALog, '       Expression:', R2.KeyRelation.CondF.SqlText);
  {$ENDIF}
  U1 := R1.UniqueValue;
  U2 := R2.UniqueValue;
  If U1 Then
    If Not U2 Then
      Begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' 1 is unique but 2 is not');
        {$ENDIF}
        Result := True;
        Exit;
      End
    Else
  Else If U2 Then
    Begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' 2 is unique but 1 is not');
      {$ENDIF}
      Result := False;
      Exit;
    End;
  U1 := R1.ClosedSegment;
  U2 := R2.ClosedSegment;
  If U1 Then
    If U2 Then
      If R1.EqualKeyDepth > R2.EqualKeyDepth Then
        Begin
          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' EqualKeyDepth(1) > EqualKeyDepth(2)');
          {$ENDIF}
          Result := True;
          Exit;
        End
      Else If R1.EqualKeyDepth < R2.EqualKeyDepth Then
        Begin
          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' EqualKeyDepth(1) < EqualKeyDepth(2)');
          {$ENDIF}
          Result := False;
          Exit;
        End
      Else If R1.KeyDepth > R2.KeyDepth Then
        Begin
          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' KeyDepth(1) > KeyDepth(2)');
          {$ENDIF}
          Result := True;
          Exit;
        End
      Else If R1.KeyDepth < R2.KeyDepth Then
        Begin
          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' KeyDepth(1) < KeyDepth(2)');
          {$ENDIF}
          Result := False;
          Exit;
        End
      Else
    Else
      Begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' Closed(1) And not Closed(2)');
        {$ENDIF}
        Result := True;
        Exit;
      End
  Else If U2 Then
    Begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not Closed(1) And Closed(2)');
      {$ENDIF}
      Result := False;
      Exit;
    End;
  U1 := R1.KeyRelation.RelationKeyIsUnique;
  U2 := R2.KeyRelation.RelationKeyIsUnique;
  If U1 Then
    If Not U2 Then
      Begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' RelationKeyIsUnique(1) And not RelationKeyIsUnique(2)');
        {$ENDIF}
        Result := True;
        Exit;
      End
    Else
  Else If U2 Then
    Begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not RelationKeyIsUnique(1) And RelationKeyIsUnique(2)');
      {$ENDIF}
      Result := False;
      Exit;
    End;
  U1 := R1.KeyRelation.RelationKeyIsCaseInsensitive;
  U2 := R2.KeyRelation.RelationKeyIsCaseInsensitive;
  If U1 Then
    If Not U2 Then
      Begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' RelationKeyIsCaseInsensitive(1) And not RelationKeyIsCaseInsensitive(2)');
        {$ENDIF}
        Result := True;
        Exit;
      End
    Else
  Else If U2 Then
    Begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not RelationKeyIsCaseInsensitive(1) And RelationKeyIsCaseInsensitive(2)');
      {$ENDIF}
      Result := False;
      Exit;
    End;
  I1 := R1.Table.GetRecordCount;
  I2 := R2.Table.GetRecordCount;
  {$IFDEF LogIndexAnalysis}
  If I1 > I2 Then
    writeln(IALog, ' RecordCount(1) > RecordCount(2)')
  Else
    writeln(IALog, ' RecordCount(1) < RecordCount(2)');
  {$ENDIF}
  If I1 > I2 Then
    Result := True
  Else
    Result := False;
End;

Function CompareKeyRelations(Const K1, K2: TfsSqlKeyRelation): Boolean;
{ Returns True If K1 is 'better' than K2, e.g. it is likely to better
  limit the number Of rows we have to read to produce a result}
Var
  U1, U2: Boolean;

  Function UniqueValue(Const K: TfsSqlKeyRelation): Boolean;
  Begin
    Result :=
      (K.RelationFieldCount = K.RelationKeyFieldCount)
      And (K.RelationOperators[K.RelationKeyFieldCount - 1] = roEQ);
  End;

  Function ClosedSegment(Const K: TfsSqlKeyRelation): Boolean;
  Begin
    Result :=
      (K.RelationOperators[K.RelationFieldCount - 1] = roEQ) Or
      (K.RelationOperatorB[K.RelationFieldCount - 1] <> roNone); {!!.11}
  End;

  Function KeyDepth(Const K: TfsSqlKeyRelation): Integer;
  Begin
    Result := K.RelationFieldCount;
  End;

  Function EqualKeyDepth(Const K: TfsSqlKeyRelation): Integer;
  Begin
    Result := 0;
    While (Result < K.RelationFieldCount)
      And (K.RelationOperators[Result] = roEQ) Do
      inc(Result);
  End;

Begin
  U1 := UniqueValue(K1);
  U2 := UniqueValue(K2);
  If U1 Then
    If Not U2 Then
      Begin
        Result := True;
        Exit;
      End
    Else If U2 Then
      Begin
        Result := False;
        Exit;
      End;
  U1 := ClosedSegment(K1);
  U2 := ClosedSegment(K2);
  If U1 Then
    If U2 Then
      If EqualKeyDepth(K1) > EqualKeyDepth(K2) Then
        Begin
          Result := True;
          Exit;
        End
      Else If EqualKeyDepth(K1) < EqualKeyDepth(K2) Then
        Begin
          Result := False;
          Exit;
        End
      Else If KeyDepth(K1) > KeyDepth(K2) Then
        Begin
          Result := True;
          Exit;
        End
      Else If KeyDepth(K1) < KeyDepth(K2) Then
        Begin
          Result := False;
          Exit;
        End
      Else
    Else
      Begin
        Result := True;
        Exit;
      End
  Else If U2 Then
    Begin
      Result := False;
      Exit;
    End;
  U1 := K1.RelationKeyIsUnique;
  U2 := K2.RelationKeyIsUnique;
  If U1 Then
    If Not U2 Then
      Begin
        Result := True;
        Exit;
      End
    Else If U2 Then
      Begin
        Result := False;
        Exit;
      End;
  U1 := K1.RelationKeyIsCaseInsensitive;
  U2 := K2.RelationKeyIsCaseInsensitive;
  If U1 Then
    If Not U2 Then
      Begin
        Result := True;
        Exit;
      End
    Else If U2 Then
      Begin
        Result := False;
        Exit;
      End;
  Result := False;
End;

{$IFDEF LogIndexAnalysis}

Procedure ShowComparison(Const K1, K2: TfsSqlKeyRelation);
Var
  U1, U2: Boolean;

  Function UniqueValue(Const K: TfsSqlKeyRelation): Boolean;
  Begin
    Result :=
      (K.RelationFieldCount = K.RelationKeyFieldCount)
      And (K.RelationOperators[K.RelationKeyFieldCount - 1] = roEQ);
  End;

  Function ClosedSegment(Const K: TfsSqlKeyRelation): Boolean;
  Begin
    Result := (K.RelationOperators[K.RelationFieldCount - 1] = roEQ)
      Or (K.RelationOperatorB[K.RelationFieldCount - 1] <> roNone); {!!.11}
  End;

  Function KeyDepth(Const K: TfsSqlKeyRelation): Integer;
  Begin
    Result := K.RelationFieldCount;
  End;

  Function EqualKeyDepth(Const K: TfsSqlKeyRelation): Integer;
  Begin
    Result := 0;
    While (Result < K.RelationFieldCount)
      And (K.RelationOperators[Result] = roEQ) Do
      inc(Result);
  End;

Begin
  U1 := UniqueValue(K1);
  U2 := UniqueValue(K2);
  If U1 Then
    If Not U2 Then
      Begin
        writeln(IALog, ' New is unique value');
        Exit;
      End
    Else If U2 Then
      Begin
        Raise Exception.Create('Internal error');
      End;
  U1 := ClosedSegment(K1);
  U2 := ClosedSegment(K2);
  If U1 Then
    If U2 Then
      If EqualKeyDepth(K1) > EqualKeyDepth(K2) Then
        Begin
          writeln(IALog, 'New has deeper equal key');
          Exit;
        End
      Else If KeyDepth(K1) > KeyDepth(K2) Then
        Begin
          writeln(IALog, 'New is deeper');
          Exit;
        End
      Else If KeyDepth(K1) < KeyDepth(K2) Then
        Begin
          Raise Exception.Create('Internal error');
        End
      Else
    Else
      Begin
        writeln(IALog, 'New is closed interval');
        Exit;
      End
  Else If U2 Then
    Begin
      Raise Exception.Create('Internal error');
    End;
  U1 := K1.RelationKeyIsUnique;
  U2 := K2.RelationKeyIsUnique;
  If U1 Then
    If Not U2 Then
      Begin
        writeln(IALog, 'New has unique key');
        Exit;
      End
    Else If U2 Then
      Begin
        Raise Exception.Create('Internal error');
      End;
  U1 := K1.RelationKeyIsCaseInsensitive;
  U2 := K2.RelationKeyIsCaseInsensitive;
  If U1 Then
    If Not U2 Then
      Begin
        writeln(IALog, 'New has Case insensitive key');
        Exit;
      End
    Else If U2 Then
      Begin
        Raise Exception.Create('Internal error');
      End;
  Raise Exception.Create('Internal error');
End;
{$ENDIF}

Procedure TfsSqlJoiner.Optimize;
Var
  IndexAsc: Boolean;
  RestSources: TFSSqlTableProxySubsetList;

  {$IFDEF LogIndexAnalysis}

  Procedure DumpOrderedList(OrderedSources: TFSSqlTableProxySubsetList; Const Title: String);
  Var
    j, y: Integer;
  Begin
    writeln(IALog, Title);
    For j := 0 To pred(OrderedSources.Count) Do
      Begin
        write(IALog, OrderedSources.Item[j].Table.Name, ' (', OrderedSources.Item[j].Table.Alias, ')');
        If OrderedSources.Item[j].KeyRelation.CondF <> Nil Then
          Begin
            write(IALog, ' relation fields: ', OrderedSources.Item[j].KeyRelation.RelationFieldCount);
            write(IALog, '(');
            For y := 0 To pred(OrderedSources.Item[j].KeyRelation.RelationFieldCount) Do
              Begin
                write(IALog, ' field:', OrderedSources.Item[j].KeyRelation.RelationFields[y].Name);
                write(IALog, ' argexp:', OrderedSources.Item[j].KeyRelation.ArgExpressions[y].SQLText);
                write(IALog, ' Operator:', fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperators[y]]);
                {!!.11 begin}
                If (OrderedSources.Item[j].KeyRelation.ArgExpressionB[y] <> Nil)
                  And (OrderedSources.Item[j].KeyRelation.RelationOperatorB[y] <> roNone)
                  And (OrderedSources.Item[j].KeyRelation.RelationB[y] <> Nil) Then
                  write(IALog, 'secondary expression:', OrderedSources.Item[j].KeyRelation.ArgExpressionB[y].SQLText,
                    ' operator:', fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB[y]]);
                {!!.11 end}
              End;
            write(IALog, ')');
            write(IALog, ' index:', OrderedSources.Item[j].KeyRelation.NativeKeyIndex {RelationKeyIndexNative});
            (* !!.11
          If (OrderedSources.Item[j].KeyRelation.ArgExpressionB <> nil)
            And (OrderedSources.Item[j].KeyRelation.RelationOperatorB <> roNone)
            And (OrderedSources.Item[j].KeyRelation.RelationB <> nil) then
            write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB.SQLText,
            ' operator:',fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB]);
            *)
            writeln(IALog);
          End
        Else
          writeln(IALog, ' no relation');
      End;
  End;

  {$ENDIF}

  Function FindRelations(CondTerm: TfsSqlCondTerm;
    MoreThanOne: Boolean): Boolean;
  Var
    l, j, k, y: Integer;
    Best, x: Integer;
    F, F2: TfsSqlFieldProxy;
    IndexRefs: Array[0..pred(fscl_MaxIndexes)] Of Integer;
    IgnoreCase: Boolean;
    IndexFields: Array[0..pred(fscl_MaxIndexFlds)] Of Integer;
    IndxFldCnt: Integer;
    Found: Boolean;
    CF: TfsSqlCondFactor;
    CurIgnoreCase: Boolean;
    DepFound: Integer;
    BestRelation: TFSSqlTableProxySubset;
    BestKeyRelation, CurKeyRelation: TfsSqlKeyRelation;
    HaveKeyRelation: Boolean;
    SameCase: Boolean;

    {$IFDEF LogIndexAnalysis}

    Procedure DumpBest;
    Var
      i: Integer;
    Begin
      With BestKeyRelation Do
        Begin
          writeln(IALog, '       condition:', CondF.SQLText);
          writeln(IALog, '       key:', NativeKeyIndex);
          writeln(IALog, '       Fields in key:', RelationKeyFieldCount);
          writeln(IALog, '       Fields:', RelationFieldCount);
          For i := 0 To pred(RelationFieldCount) Do
            Begin
              writeln(IALog, '        ', RelationFields[i].Name, ' ', fsRelOpStr[RelationOperators[i]], ' ',
                ArgExpressions[i].SQLText);
              {!!.11 begin}
              If RelationOperatorB[i] <> roNone Then
                writeln(IALog, '      Secondary relation:',
                  fsRelOpStr[RelationOperatorB[i]], ' ',
                  ArgExpressionB[i].SQLText);
              {!!.11 end}
            End;
          {!!.11 Begin
        If RelationOperatorB <> roNone then
          writeln(IALog, '      Secondary relation on last key field:',
          fsRelOpStr[RelationOperatorB], ' ',
          ArgExpressionB.SQLText);
          !!.11 end}
        End;
    End;

    {$ENDIF}
  Var
    z: Integer;
  Begin
    Result := False;
    {CurKeyRelation.ArgExpressionB := nil;}{!!.11}
    For z := 0 To pred(fscl_MaxIndexFlds) Do
      Begin {!!.11}
        CurKeyRelation.ArgExpressionB[z] := Nil; {!!.11}
        CurKeyRelation.RelationOperatorB[z] := roNone; {!!.11}
      End; {!!.11}

    With CondTerm Do
      Repeat

        //KeyState := ksNone;
        //Depth := 0;

        For j := 0 To pred(RestSources.Count) Do
          Begin
            RestSources.Item[j].Relations := 0;
            {$IFDEF LogIndexAnalysis}
            writeln(IALog, '  looking For relations on ',
              RestSources.Item[j].Table.Name, ' (', RestSources.Item[j].Table.Alias, ')');
            {$ENDIF}

            {we select among multiple keys as follows:}
            {if we find a unique key on the available field(s) we use that
            otherwise,
            we use the deepest key we can find, i.e. the key where the
            most segments can be satisfied.
            among keys With the same depth, we pick the ones With
            the tightest Or the most relations, e.g.
            = is better than >
            > And < is better than only >
            ties could be further settled based on the number Of
            key values in an index, but we don't currently do that}

            HaveKeyRelation := False;
            CurKeyRelation.RelationFieldCount := 0;

            For k := 0 To pred(CondFactorCount) Do
              Begin
                If Not OrderedSources.RelationUsed(CondFactor[k]) Then
                  With CondFactor[k] Do
                    Begin
                      If IsRelationTo(RestSources.Item[j].Table,
                        F, CurKeyRelation.RelationOperators[0],
                        CurKeyRelation.ArgExpressions[0], SameCase)
                        And fsCanOptimizeOnOperator[CurKeyRelation.
                        RelationOperators[0]] Then
                        Begin

                          If RestSources.Item[j].Outer
                            And CurKeyRelation.ArgExpressions[0].DependsOn(
                            RestSources.Item[j].Opposite) Then
                            Begin

                              {$IFDEF LogIndexAnalysis}
                              writeln(IALOG, '   ', CondFactor[k].SQLText, ' is a relation to ',
                                RestSources.Item[j].Table.Name, ' (', RestSources.Item[j].Table.Alias, '). Arg expression:',
                                CurKeyRelation.ArgExpressions[0].SQLText);
                              writeln(IALOG, '   but using would violate the outer join, so we can''t use it. Skipped.');
                              {$ENDIF}

                            End
                          Else
                            Begin

                              {$IFDEF LogIndexAnalysis}
                              writeln(IALOG, '   ', CondFactor[k].SQLText, ' is a relation to ',
                                RestSources.Item[j].Table.Name, ' (', RestSources.Item[j].Table.Alias, '). Arg expression:',
                                CurKeyRelation.ArgExpressions[0].SQLText);
                              {$ENDIF}

                              CurKeyRelation.CondF := CondFactor[k];
                              {CurKeyRelation.RelationB := nil;}{!!.11}
                              For z := 0 To pred(fscl_MaxIndexFlds) Do
                                Begin {!!.11}
                                  CurKeyRelation.ArgExpressionB[z] := Nil; {!!.11}
                                  CurKeyRelation.RelationOperatorB[z] := roNone; {!!.11}
                                End; {!!.11}

                              {Check that this relation does not depend on something
                            we can't determine at this level. For example, If we
                              have table1 at the deepest level, then table2 at the
                            next, we are looking For conditional expressions on
                              table2 that will limit the number Of rows we need to
                              read but we can't use conditions whose other side
                              refer to anything in table1.}

                              {$IFDEF LogIndexAnalysis}
                              writeln(IALog, '   Checking dependencies on deeper tables For :' +
                                CurKeyRelation.ArgExpressions[0].SQLText);
                              {$ENDIF}

                              DepFound := -1;

                              For l := pred(OrderedSources.Count) Downto 0 Do
                                If CurKeyRelation.ArgExpressions[0].DependsOn(
                                  OrderedSources.Item[l].Table) Then
                                  Begin
                                    DepFound := l;
                                    break;
                                  End;

                              {$IFDEF LogIndexAnalysis}
                              If DepFound <> -1 Then
                                writeln(IALog, '    Deeper dependency found:',
                                  CurKeyRelation.ArgExpressions[0].SQLText, ' : ',
                                  OrderedSources.Item[l].Table.Name, ' (', OrderedSources.Item[l].Table.Alias, ')')
                              Else
                                writeln(IALog, '    No deeper dependency found on ',
                                  CurKeyRelation.ArgExpressions[0].SQLText);
                              {$ENDIF}

                              {Part Of the expression opposite our field is from a table, which
                              has already been put in the list. We can still use this relation
                              by putting it below that other table *unless* something in the
                              existing list depends on us (the table we're looking at now)}

                              If (DepFound <> -1)
                                And OrderedSources.DependencyExists(RestSources.
                                Item[j].Table) Then
                                Begin
                                  {$IFDEF LogIndexAnalysis}
                                  writeln(IALog, '    Can''t use this - something Else depends on it');
                                  {$ENDIF}
                                  CurKeyRelation.CondF := Nil;
                                End
                              Else
                                Begin
                                  {$IFDEF LogIndexAnalysis}
                                  writeln(IALog, '    Relation found:', SQLText);
                                  writeln(IALog, '    field:', F.Name);
                                  writeln(IALog, '    same case:', SameCase); {!!.10}
                                  writeln(IALog, '    operator:', fsRelOpStr[CurKeyRelation.RelationOperators[0]]);
                                  writeln(IALog, '    arg expression:', CurKeyRelation.ArgExpressions[0].SQLTExt);
                                  writeln(IALog, '      looking For indexes on that field');
                                  {$ENDIF}

                                  x := RestSources.Item[j].Table.IndexesOnField(F,
                                    Not SameCase, IndexRefs);

                                  CurKeyRelation.RelationFieldCount := 1;

                                  {$IFDEF LogIndexAnalysis}
                                  CurKeyRelation.RelationFields[0] := F;
                                  {$ENDIF}

                                  If x <> 0 Then
                                    Begin

                                      Case CurKeyRelation.RelationOperators[0] Of
                                        roEQ:
                                          Begin
                                            For y := 0 To pred(x) Do
                                              Begin
                                                RestSources.Item[j].Table.GetIndexProperties
                                                  (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                                                  CurIgnoreCase, IndexAsc, IndxFldCnt,
                                                  IndexFields);

                                                CurKeyRelation.RelationFieldCount := 1;
                                                CurKeyRelation.RelationKeyFieldCount := IndxFldCnt;
                                                CurKeyRelation.RelationOperators[0] := roEQ;
                                                CurKeyRelation.RelationOperatorB[0] := roNone; {!!.11}
                                                CurKeyRelation.RelationKeyIsCaseInsensitive :=
                                                  CurIgnoreCase; {!!.11}
                                                CurKeyRelation.RelationKeyIndexAsc := IndexAsc;
                                                CurKeyRelation.NativeKeyIndex := IndexRefs[y];
                                                CurKeyRelation.DepIndex := DepFound;

                                                (* !!.11 actually, whether relation key is unique is irrelevant here
                                              If CurKeyRelation.RelationKeyIsUnique then
                                                Begin
                                                If IndxFldCnt = 1 then
                                                  Begin
                                                    IgnoreCase := CurIgnoreCase;
                                                  End Else
                                                  Begin
                                                    {Multi-segment key.
                                                  See If we have other relations that satisfy
                                                    the following fields in the key}
                                                    CurKeyRelation.RelationFieldCount := 1;
                                                  Repeat
                                                      F2 := RestSources.Item[j].Table.
                                                      Field(IndexFields[CurKeyRelation.
                                                      RelationFieldCount]);
                                                      CF := FindRelation(CondTerm, CondFactor[k],
                                                      nil, RestSources.Item[j].Table, F2,
                                                      CurKeyRelation.RelationOperators[
                                                      CurKeyRelation.RelationFieldCount],
                                                      CurKeyRelation.ArgExpressions[
                                                      CurKeyRelation.RelationFieldCount],
                                                      CurKeyRelation.SameCases[
                                                      CurKeyRelation.RelationFieldCount]);
                                                    If CF = nil then
                                                      Begin
                                                        {No further fields found.
                                                        We have a key, but not a unique one}
                                                        IgnoreCase := CurIgnoreCase;
                                                        break;
                                                      End Else
                                                      Begin
                                                        {we have a relation on this key segment}
                                                        {$IFDEF LogIndexAnalysis}
                                                        CurKeyRelation.RelationFields[
                                                        CurKeyRelation.RelationFieldCount] := F2;
                                                        {$ENDIF}

                                                      If CurKeyRelation.RelationOperators[
                                                        CurKeyRelation.RelationFieldCount] = roEQ then
                                                        Begin
                                                        {operator is = which means we can continue searching If
                                                          there are more fields in the key. Otherwise, we have a full
                                                          key}
                                                          IgnoreCase := CurIgnoreCase;
                                                        End Else
                                                        Begin
                                                          {Operator wasn't =, so we can't continue.
                                                          We can use this field, though, as the last one}
                                                          IgnoreCase := CurIgnoreCase;
                                                        {See If we have a secondary expression to Close the interval}
                                                          CF := FindRelation(CondTerm, CondFactor[k],
                                                          CF, RestSources.Item[j].Table, F2,
                                                          CurKeyRelation.RelationOperatorB,
                                                          CurKeyRelation.ArgExpressionB,
                                                          CurKeyRelation.SameCaseB);
                                                        If CF <> nil then
                                                          Begin
                                                            {we do - record data And update key state}

                                                            CurKeyRelation.RelationB := CF;
                                                            IgnoreCase := CurIgnoreCase;

                                                          End Else
                                                          Begin
                                                            CurKeyRelation.ArgExpressionB := nil;
                                                            CurKeyRelation.RelationOperatorB := roNone;
                                                          End;
                                                          inc(CurKeyRelation.RelationFieldCount);
                                                          break;
                                                        End;
                                                      End;
                                                      inc(CurKeyRelation.RelationFieldCount);
                                                    Until CurKeyRelation.RelationFieldCount >=
                                                    IndxFldCnt;
                                                  End;
                                                  end else Begin {not a unique key}
                                                  *)
                                                If IndxFldCnt = 1 Then
                                                  Begin
                                                    IgnoreCase := CurIgnoreCase;
                                                  End
                                                Else
                                                  Begin
                                                    {Multi-segment key.
                                                  See If we have other relations that satisfy
                                                    the following fields in the key}
                                                    CurKeyRelation.RelationFieldCount := 1;
                                                    Repeat
                                                      F2 := RestSources.Item[j].Table.
                                                        Field(IndexFields[
                                                        CurKeyRelation.RelationFieldCount]);
                                                      CF := FindRelation(CondTerm, CondFactor[k],
                                                        Nil, RestSources.Item[j].Table, F2,
                                                        CurKeyRelation.RelationOperators[
                                                        CurKeyRelation.RelationFieldCount],
                                                          CurKeyRelation.ArgExpressions[
                                                        CurKeyRelation.RelationFieldCount],
                                                          CurKeyRelation.SameCases[
                                                        CurKeyRelation.RelationFieldCount]);
                                                      If CF = Nil Then
                                                        Begin
                                                          {No further fields found, but
                                                          we have a key but not a full one}
                                                          IgnoreCase := CurIgnoreCase;
                                                          break;
                                                        End
                                                      Else
                                                        Begin
                                                          {we have a relation on this key segment}
                                                          {$IFDEF LogIndexAnalysis}
                                                          CurKeyRelation.RelationFields[CurKeyRelation.RelationFieldCount] := F2;
                                                          {$ENDIF}

                                                          If CurKeyRelation.RelationOperators[
                                                            CurKeyRelation.RelationFieldCount] = roEQ Then
                                                            Begin
                                                              {operator is = which means we can continue searching If
                                                                there are more fields in the key. Otherwise, we have a full
                                                                key}
                                                              IgnoreCase := CurIgnoreCase;
                                                              CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone;
                                                              {!!.11}
                                                            End
                                                          Else
                                                            Begin
                                                              {Operator wasn't =, so we can't continue.
                                                              We can use this field, though, as the last one}
                                                              IgnoreCase := CurIgnoreCase;
                                                              {see If we have other relations on this same segment}
                                                              CF := FindRelation(CondTerm, CondFactor[k],
                                                                CF, RestSources.Item[j].Table,
                                                                F2, CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount], {!!.11}
                                                                CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount], {!!.11}
                                                                CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount]); {!!.11}
                                                              If CF <> Nil Then
                                                                Begin
                                                                  {we do - record data And update key state}

                                                                  CurKeyRelation.RelationB[CurKeyRelation.RelationFieldCount] := CF; {!!.11}
                                                                  IgnoreCase := CurIgnoreCase;

                                                                End
                                                              Else
                                                                Begin
                                                                  CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount] := Nil;
                                                                  {!!.11}
                                                                  CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone;
                                                                  {!!.11}
                                                                End;
                                                              inc(CurKeyRelation.RelationFieldCount);
                                                              break;
                                                            End;
                                                        End;
                                                      inc(CurKeyRelation.RelationFieldCount);
                                                    Until CurKeyRelation.RelationFieldCount >=
                                                      IndxFldCnt;
                                                  End;
                                                {end;}{!!.11}
                                                If HaveKeyRelation Then
                                                  If CompareKeyRelations(CurKeyRelation, BestKeyRelation) Then
                                                    Begin
                                                      {$IFDEF LogIndexAnalysis}
                                                      writeln(IALog, '      New best key relation');
                                                      ShowComparison(CurKeyRelation, BestKeyrelation);
                                                      {$ENDIF}
                                                      BestKeyRelation := CurKeyRelation;
                                                      {$IFDEF LogIndexAnalysis}
                                                      DumpBest;
                                                      {$ENDIF}
                                                    End
                                                  Else
                                                Else
                                                  Begin
                                                    BestKeyRelation := CurKeyRelation;
                                                    {$IFDEF LogIndexAnalysis}
                                                    writeln(IALog, '      initial key relation');
                                                    DumpBest;
                                                    {$ENDIF}
                                                    HaveKeyRelation := True;
                                                  End;
                                              End;
                                          End;
                                        Else {~ Op <> roEQ}
                                          {non equal join operator}
                                          For y := 0 To pred(x) Do
                                            Begin
                                              RestSources.Item[j].Table.GetIndexProperties
                                                (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                                                IgnoreCase, IndexAsc, IndxFldCnt, IndexFields);

                                              CurKeyRelation.RelationFieldCount := 1;
                                              CurKeyRelation.RelationKeyFieldCount := IndxFldCnt;
                                              CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount - 1] := roNone; {!!.11}
                                              CurKeyRelation.RelationKeyIsCaseInsensitive :=
                                                CurIgnoreCase; {!!.11}
                                              CurKeyRelation.RelationKeyIndexAsc := IndexAsc;
                                              CurKeyRelation.NativeKeyIndex := IndexRefs[y];
                                              CurKeyRelation.DepIndex := DepFound;

                                              IgnoreCase := CurIgnoreCase;

                                              {see If we have other relations on this same segment}
                                              CF := FindRelation(CondTerm, CondFactor[k], Nil,
                                                RestSources.Item[j].Table, F, CurKeyRelation.
                                                RelationOperatorB[CurKeyRelation.RelationFieldCount - 1], {!!.11}
                                                CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount - 1], {!!.11}
                                                CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount - 1]); {!!.11}

                                              If CF <> Nil Then
                                                Begin
                                                  {we do - record data And update key state}

                                                  IgnoreCase := CurIgnoreCase;

                                                  CurKeyrelation.RelationB[CurKeyRelation.RelationFieldCount - 1] := CF; {!!.11}

                                                  {!!.11 begin}
                                                {check For more interval segments}
                                                  If (IndxFldCnt > 1)
                                                    And (CurKeyRelation.RelationOperators[0] In [roEQ, roGE, roLE])
                                                    And (CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount - 1] In [roEQ,
                                                    roGE,
                                                      roLE]) Then
                                                    Begin
                                                      {Multi-segment key.
                                                    See If we have other relations that satisfy
                                                      the following fields in the key}
                                                      Repeat
                                                        F2 := RestSources.Item[j].Table.
                                                          Field(IndexFields[
                                                          CurKeyRelation.RelationFieldCount]);
                                                        CF := FindRelation(CondTerm, CondFactor[k],
                                                          Nil, RestSources.Item[j].Table, F2,
                                                          CurKeyRelation.RelationOperators[
                                                          CurKeyRelation.RelationFieldCount],
                                                            CurKeyRelation.ArgExpressions[
                                                          CurKeyRelation.RelationFieldCount],
                                                            CurKeyRelation.SameCases[
                                                          CurKeyRelation.RelationFieldCount]);
                                                        If CF = Nil Then
                                                          Begin
                                                            {No further fields found, but
                                                            we have a key but not a full one}
                                                            IgnoreCase := CurIgnoreCase;
                                                            break;
                                                          End
                                                        Else If CurKeyRelation.RelationOperators[
                                                          CurKeyRelation.RelationFieldCount] In [roEQ, roGE, roLE] Then
                                                          Begin
                                                            {we have a relation on this key segment}
                                                            {$IFDEF LogIndexAnalysis}
                                                            CurKeyRelation.RelationFields[CurKeyRelation.RelationFieldCount] := F2;
                                                            {$ENDIF}

                                                            If CurKeyRelation.RelationOperators[
                                                              CurKeyRelation.RelationFieldCount] = roEQ Then
                                                              Begin
                                                                {operator is = which means we can continue searching If
                                                                  there are more fields in the key. Otherwise, we have a full
                                                                  key}
                                                                IgnoreCase := CurIgnoreCase;
                                                                CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone;
                                                                {!!.11}
                                                              End
                                                            Else
                                                              Begin
                                                                {Operator wasn't =}
                                                                IgnoreCase := CurIgnoreCase;
                                                                {see If we have other relations on this same segment}
                                                                CF := FindRelation(CondTerm, CondFactor[k],
                                                                  CF, RestSources.Item[j].Table,
                                                                  F2, CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount],
                                                                  {!!.11}
                                                                  CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount], {!!.11}
                                                                  CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount]); {!!.11}
                                                                If CF <> Nil Then
                                                                  Begin
                                                                    If Not (CurKeyRelation.RelationOperatorB[
                                                                      CurKeyRelation.RelationFieldCount] In [roEQ, roGE, roLE]) Then
                                                                      break;

                                                                    {we do - record data And update key state}

                                                                    CurKeyRelation.RelationB[CurKeyRelation.RelationFieldCount] := CF; {!!.11}
                                                                    IgnoreCase := CurIgnoreCase;

                                                                  End
                                                                Else
                                                                  Begin
                                                                    CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount] := Nil;
                                                                    {!!.11}
                                                                    CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] :=
                                                                      roNone;
                                                                    {!!.11}
                                                                    inc(CurKeyRelation.RelationFieldCount);
                                                                    break;
                                                                  End;
                                                              End;
                                                          End;
                                                        inc(CurKeyRelation.RelationFieldCount);
                                                      Until CurKeyRelation.RelationFieldCount >=
                                                        IndxFldCnt;
                                                    End;
                                                  {!!.11 end}
                                                End
                                              Else
                                                Begin
                                                  CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount - 1] := Nil; {!!.11}
                                                  CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount - 1] := roNone; {!!.11}
                                                End;

                                              If HaveKeyRelation Then
                                                If CompareKeyRelations(CurKeyRelation,
                                                  BestKeyRelation) Then
                                                  Begin
                                                    {$IFDEF LogIndexAnalysis}
                                                    ShowComparison(CurKeyRelation, BestKeyrelation);
                                                    {$ENDIF}
                                                    BestKeyRelation := CurKeyRelation;
                                                    {$IFDEF LogIndexAnalysis}
                                                    writeln(IALog, '      new best key relation');
                                                    DumpBest;
                                                    {$ENDIF}
                                                  End
                                                Else
                                              Else
                                                Begin
                                                  BestKeyRelation := CurKeyRelation;
                                                  {$IFDEF LogIndexAnalysis}
                                                  writeln(IALog, '      initial key relation');
                                                  DumpBest;
                                                  {$ENDIF}
                                                  HaveKeyRelation := True;
                                                End;

                                            End;
                                      End;

                                      {$IFDEF LogIndexAnalysis}
                                      writeln(IALog, '        ', x, ' found!');
                                      For y := 0 To pred(x) Do
                                        Begin
                                          RestSources.Item[j].Table.GetIndexProperties
                                            (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                                            IgnoreCase, IndexAsc, IndxFldCnt, IndexFields);
                                          writeln(IALog, '          key', y, ': ',
                                            '      Unique:', CurKeyRelation.RelationKeyIsUnique,
                                            '      IgnoreCase:', IgnoreCase,
                                            '      IndexAsc:', IndexAsc,
                                            '      Segments:', IndxFldCnt);
                                          If IndxFldCnt <> 0 Then
                                            Begin
                                              write(IALog, '           (');
                                              For z := 0 To pred(IndxFldCnt) Do
                                                Begin
                                                  write(IALog, RestSources.Item[j].Table.
                                                    Field(IndexFields[z]).Name, ' ');
                                                End;
                                              writeln(IALog, ')');
                                            End;
                                        End;
                                      {$ENDIF}

                                      inc(RestSources.Item[j].Relations);

                                    End
                                  Else
                                    {$IFDEF LogIndexAnalysis}
                                    writeln(IALog, '        none found');
                                  {$ENDIF}
                                End;
                            End;
                        End;
                    End;
              End;

            If HaveKeyRelation Then
              RestSources.Item[j].KeyRelation := BestKeyRelation;
          End;

        Found := False;
        Best := -1;

        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' Comparing relations');
        {$ENDIF}

        BestRelation := Nil;
        For j := 0 To pred(RestSources.Count) Do
          Begin
            If (Not MoreThanOne And (RestSources.Item[j].Relations = 1))
              Or (MoreThanOne And (RestSources.Item[j].Relations > 0)) Then
              Begin
                {$IFDEF LogIndexAnalysis}
                writeln(IALog, ' ', RestSources.Item[j].Table.Name, ' (',
                  RestSources.Item[j].Table.Alias, ') relations:',
                  RestSources.Item[j].Relations);
                {$ENDIF}
                If CompareRelations(RestSources.Item[j], BestRelation) Then
                  Begin
                    BestRelation := RestSources.Item[j];
                    Best := j;
                  End;

              End;
          End;

        If BestRelation <> Nil Then
          Begin

            {$IFDEF LogIndexAnalysis}
            writeln(IALog, ' Best:', BestRelation.Table.Name, ' (', BestRelation.Table.Alias, ')');
            {$ENDIF}
            If BestRelation.KeyRelation.DepIndex = -1 Then
              OrderedSources.Add(RestSources.Item[Best])
            Else
              OrderedSources.Insert(RestSources.Item[Best]);
            RestSources.Delete(Best);
            Found := True;
            {$IFDEF LogIndexAnalysis}
            DumpOrderedList(OrderedSources, ' Ordered list so far(inner to outer):');
            {$ENDIF}
            Result := True;
          End;

      Until Not Found;
  End;

Var
  i, j: Integer;
  {$IFDEF LogIndexAnalysis}
  y: Integer;
  {$ENDIF}
Begin
  If OptimizeCalled Then
    Exit;

  WasOptimized := False;

  If (CondExpWhere <> Nil) And UseIndex Then
    Begin

      {$IFDEF LogIndexAnalysis}
      AssignFile(IALog, IALogFile);
      {$I-}
      Append(IALog);
      If IOResult <> 0 Then
        Rewrite(IALog);
      writeln(IALog);
      writeln(IALog, 'Analyzing ' + CondExpWhere.Owner.SQLText);
      writeln(IALog, 'Analysis started at :', DateTimeToStr(Now));
      {$ENDIF}

      {look For relations that might be used For optimizing the join}

      {$IFDEF LogIndexAnalysis}
      writeln(IALog, 'Scanning For relations');
      {$ENDIF}

      For i := 0 To pred(CondExpWhere.GetCondTermCount) Do
        Begin

          {process each term separately}
          With CondExpWhere.CondTerm[i] Do
            Begin

              {$IFDEF LogIndexAnalysis}
              writeln(IALog, 'Term ', i, ' : ', SQLText);
              {$ENDIF}

              OrderedSources.Free;

              OrderedSources := TFSSqlTableProxySubsetList.Create(Owner);
              RestSources := TFSSqlTableProxySubsetList.Create(Owner);
              Try
                {We build an ordered list Of tables to process so that
                the inner-most table in the list is first.}

              {Specifically, we do this by looking For key relations
                which will limit the number Of rows we need to read from
                each table.}

                {RestSources are the tables at any time which have not
              yet been selected For processing.
                When RestSources.Count = 0, we're done.}

                RestSources.Assign(Sources);

                {First, find and process the relations With
                exactly one key resolution.}

                {$IFDEF LogIndexAnalysis}
                writeln(IALog, ' Looking for relations With exactly one resolution');
                {$ENDIF}

                If FindRelations(CondExpWhere.CondTerm[i], False) Then
                  WasOptimized := True;

                {$IFDEF LogIndexAnalysis}
                DumpOrderedList(OrderedSources, 'Final ordered list (inner to outer):');
                {$ENDIF}

                {Then, find and process the relations With
              more than one key resolution, If any.}

                {$IFDEF LogIndexAnalysis}
                writeln(IALog, ' Looking for relations With more than one resolution');
                {$ENDIF}

                If FindRelations(CondExpWhere.CondTerm[i], True) Then
                  WasOptimized := True;

                {Finally, add the sources With no key relations - If any}

                For j := pred(RestSources.Count) Downto 0 Do
                  Begin
                    RestSources.Item[j].KeyRelation.CondF := Nil;
                    OrderedSources.Add(RestSources.Item[j]);
                    RestSources.Delete(j);
                  End;

                Assert(RestSources.Count = 0);

                {done re-ordering}

                {$IFDEF LogIndexAnalysis}
                writeln(IALog, 'Ordered list (inner to outer):');
                For j := 0 To pred(OrderedSources.Count) Do
                  Begin
                    write(IALog, OrderedSources.Item[j].Table.Name, ' (', OrderedSources.Item[j].Table.Alias, ')');
                    If OrderedSources.Item[j].KeyRelation.CondF <> Nil Then
                      Begin
                        write(IALog, ' relation fields: ', OrderedSources.Item[j].KeyRelation.RelationFieldCount);
                        write(IALog, '(');
                        For y := 0 To pred(OrderedSources.Item[j].KeyRelation.RelationFieldCount) Do
                          Begin
                            write(IALog, ' field:', OrderedSources.Item[j].KeyRelation.RelationFields[y].Name);
                            write(IALog, ' argexp:', OrderedSources.Item[j].KeyRelation.ArgExpressions[y].SQLText);
                            write(IALog, ' Operator:', fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperators[y]]);
                            {!!.11 begin}
                            If (OrderedSources.Item[j].KeyRelation.ArgExpressionB[y] <> Nil)
                              And (OrderedSources.Item[j].KeyRelation.RelationOperatorB[y] <> roNone)
                              And (OrderedSources.Item[j].KeyRelation.RelationB[y] <> Nil) Then
                              write(IALog, 'secondary expression:', OrderedSources.Item[j].KeyRelation.ArgExpressionB[y].SQLText,
                                ' operator:', fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB[y]]);
                            {!!.11 end}
                          End;
                        write(IALog, ')');
                        write(IALog, ' index:', OrderedSources.Item[j].KeyRelation.NativeKeyIndex);
                        {!!.11 Begin
                      If (OrderedSources.Item[j].KeyRelation.ArgExpressionB <> nil)
                        And (OrderedSources.Item[j].KeyRelation.RelationOperatorB <> roNone)
                        And (OrderedSources.Item[j].KeyRelation.RelationB <> nil) then
                        write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB.SQLText,
                        ' operator:',fsRelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB]);
                        !!.11 end}
                        writeln(IALog);
                      End
                    Else
                      writeln(IALog, ' no relation');
                  End;
                {$ENDIF}
              Finally
                RestSources.Free;
              End;

            End;

        End;

      {$IFDEF LogIndexAnalysis}
      writeln(IALog);
      writeln(IALog, 'Analysis ended at :', DateTimeToStr(Now));
      CloseFile(IALog);
      {$ENDIF}

    End;

  OptimizeCalled := True;
End;

{===Utility routines=================================================}

Function BothNil(O1, O2: TfsSqlNode): Boolean;
Begin
  Result := (O1 = Nil) And (O2 = Nil);
End;
{--------}

Function BothNonNil(O1, O2: TfsSqlNode): Boolean;
Begin
  Result := (O1 <> Nil) And (O2 <> Nil);
End;
{====================================================================}

{===TfsSqlNode=======================================================}
{--------}

Procedure TfsSqlNode.AddAggregate(Target: TList);
Begin
End;
{--------}

Procedure TfsSqlNode.FlagAggregate;
Begin
End;
{--------}

Function TfsSqlNode.GetDecimals: Integer;
Begin
  Result := 0;
End;

Function TfsSqlNode.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
End;

Function TfsSqlNode.GetRound: TRound;
Begin
  Result := rNone;
End;
{--------}

Function TfsSqlNode.GetSize: Integer;
Begin
  Result := 0;
End;
{--------}

Function TfsSqlNode.GetType: TfsFieldType;
Begin
  Raise Exception.CreateFmt('Internal error:GetType not implemented For %s',
    [ClassName]);
End;

{--------}

Function TfsSqlNode.IsAggregate: Boolean;
Begin
  Raise Exception.CreateFmt('Internal error:IsAggregate not implemented For %s',
    [ClassName]);
End;
{--------}

Function TfsSqlNode.GetOwner: TfsSqlStatement;
Begin
  If (FOwner = Nil)
    And Not (Self Is TfsSqlStatement) Then
    Begin
      Assert(Parent <> Nil);
      FOwner := TfsSqlStatement(Parent);
      While FOwner.Parent <> Nil Do
        FOwner := TfsSqlStatement(FOwner.Parent);
      Assert(Owner Is TfsSqlStatement);
    End;
  Result := FOwner;
End;
{--------}
{Begin !!.11}

Function TfsSqlNode.GetOwnerStmt: TfsSqlColumnListOwner;
Begin
  If (FOwnerStmt = Nil) Then
    Begin
      FOwnerStmt := TfsSqlColumnListOwner(Self);
      While (FOwnerStmt <> Nil)
        And Not (TObject(FOwnerStmt) Is TfsSqlColumnListOwner) Do
        FOwnerStmt := TfsSqlColumnListOwner(FOwnerStmt.Parent);
      If Not (TObject(FOwnerStmt) Is TfsSqlColumnListOwner) Then
        FOwnerStmt := Nil;
    End;
  Result := FOwnerStmt;
End;
{--------}

Function TfsSqlNode.GetOwnerSelect: TfsSqlSelect;
Begin
  If (FOwnerStmt = Nil) Then
    Begin
      FOwnerStmt := TfsSqlSelect(Self);
      While (FOwnerStmt <> Nil)
        And Not (TObject(FOwnerStmt) Is TfsSqlSelect) Do
        FOwnerStmt := TfsSqlSelect(FOwnerStmt.Parent);
      If Not (TObject(FOwnerStmt) Is TfsSqlSelect) Then
        FOwnerStmt := Nil;
    End;
  Result := TfsSqlSelect(FOwnerStmt);
End;
{End !!.11}
{--------}

Procedure TfsSqlNode.TypeMismatch;
Begin
  SQLError('Type mismatch');
End;
{--------}

Procedure TfsSqlNode.WriteEOF(Stream: TStream);
Const
  NullChar: Char = #0;
Begin
  Stream.Write(NullChar, 1);
End;
{--------}

Procedure TfsSqlNode.WriteStr(Stream: TStream; Const S: String);
Begin
  If S <> '' Then
    Stream.Write(S[1], length(S));
End;
{--------}

Procedure TfsSqlNode.AddTableReference;
Begin
End;
{--------}

Procedure TfsSqlNode.AddColumnDef;
Begin
End;

Procedure TfsSqlNode.AddDistinctColumnDef;
Begin
End;
{--------}

Procedure TfsSqlNode.AssignError(Source: TfsSqlNode);
Begin
  Raise Exception.Create(Source.ClassName + ' not compatible With ' + ClassName);
End;
{--------}

Function TfsSqlNode.BindField(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  If Parent <> Nil Then
    Result := Parent.BindField(TableName, FieldName)
  Else
    Raise Exception.CreateFmt('No node could resolve the field %s.%s', {!!.11}
      [TableName, FieldName]); {!!.11}
End;
{--------}

Procedure TfsSqlNode.ClearBinding;
Begin
End;
{--------}

Function TfsSqlNode.IsAncestor(Const Node: TfsSqlNode): Boolean;
Var
  aParent: TfsSqlNode;
Begin
  aParent := FParent;
  Repeat
    Result := (aParent = Node);
    aParent := aParent.Parent;
  Until Result Or (aParent = Nil);
End;
{--------}

Procedure TfsSqlNode.ResetConstant;
Begin
End;
{--------}

Function TfsSqlNode.SQLText: String;
Var
  M: TMemoryStream;
Begin
  M := TMemoryStream.Create;
  Try
    EmitSQL(M);
    SetLength(Result, M.Size);
    M.Seek(0, 0);
    M.Read(Result[1], M.Size);
  Finally
    M.Free;
  End;
End;
{--------}

Procedure TfsSqlNode.SQLError(Const ErrorMsg: String);
Begin
  Raise Exception.CreateFmt('Error in statement: %s', [ErrorMsg]);
End;
{--------}

Constructor TfsSqlNode.Create(AParent: TfsSqlNode);
Begin
  Inherited Create;
  FParent := AParent;
  FNull := False;
End;
{--------}

Procedure TfsSqlNode.EmitSQL(Stream: TStream);
Begin
  Raise Exception.CreateFmt('Internal error:EmitSQL not implemented For %s',
    [ClassName]);
End;
{====================================================================}

{===TfsSqlSelectionList==============================================}

Function TfsSqlSelectionList.AddSelection(
  NewSelection: TfsSqlSelection): TfsSqlSelection;
Begin
  FSelections.Add(NewSelection);
  Result := NewSelection;
End;
{--------}

Procedure TfsSqlSelectionList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlSelectionList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlSelectionList(Source).SelectionCount) Do
        AddSelection(TfsSqlSelection.Create(Self)).Assign(
          TfsSqlSelectionList(Source).Selection[i]);
    End
  Else
    AssignError(Source);
End;

{--------}

Constructor TfsSqlSelectionList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FSelections := TList.Create;
End;
{--------}

Procedure TfsSqlSelectionList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(SelectionCount) Do
    Selection[i].Free;
  FSelections.Clear;
End;

{--------}

Destructor TfsSqlSelectionList.Destroy;
Begin
  Clear;
  FSelections.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlSelectionList.EmitSQL(Stream: TStream);
Var
  i: Integer;
  First: Boolean;
Begin
  If SelectionCount > 0 Then
    Begin
      First := True;
      For i := 0 To pred(SelectionCount) Do
        Begin
          If Not First Then
            WriteStr(Stream, ', ');
          If Not Selection[i].AddedByWildcard Then
            Begin
              Selection[i].EmitSQL(Stream);
              First := False;
            End;
        End;
    End;
End;
{--------}

Procedure TfsSqlSelectionList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  Assert(TObject(Self) Is TfsSqlSelectionList);
  EnumMethod(Self);
  For i := 0 To pred(SelectionCount) Do
    Selection[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlSelectionList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlSelectionList Then
    Begin
      If NonWildSelectionCount <> TfsSqlSelectionList(Other).NonWildSelectionCount Then
        Exit;
      For i := 0 To pred(NonWildSelectionCount) Do
        If Not NonWildSelection[i].Equals(TfsSqlSelectionList(Other).
          NonWildSelection[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlSelectionList.FindSelection(GroupCol:
  TfsSqlGroupColumn): TfsSqlSelection;
Var
  i: Integer;
  F: TfsSqlFieldProxy;
  Name: String;
Begin
  Name := GroupCol.QualColumnName;

  For i := 0 To pred(SelectionCount) Do
    If Assigned(Selection[i].SimpleExpression.Term[0].Factor[0].FieldRef) And
      (AnsiCompareText(Trim(Selection[i].SimpleExpression.Term[0].Factor[0].
      FieldRef.QualName), Name) = 0) Then
      Begin
        Result := Selection[i];
        Exit;
      End
    Else If AnsiCompareText(Trim(Selection[i].SQLText), Name) = 0 Then
      Begin
        Result := Selection[i];
        Exit;
      End
    Else If Selection[i].Column <> Nil Then
      If AnsiCompareText(Selection[i].Column.ColumnName, Name) = 0 Then
        Begin
          Result := Selection[i];
          Exit;
        End
      Else
    Else If Selection[i].SimpleExpression.IsField(F) Then
      If (AnsiCompareText(F.Name, Name) = 0) Or
        (AnsiCompareText(F.QualName, Name) = 0) Then
        Begin
          Result := Selection[i];
          Exit;
        End;
  Result := Nil;
End;
{--------}

Function TfsSqlSelectionList.GetNonWildSelection(
  Index: Integer): TfsSqlSelection;
Var
  i: Integer;
Begin
  For i := 0 To pred(SelectionCount) Do
    If Not Selection[i].AddedByWildcard Then
      Begin
        dec(Index);
        If Index < 0 Then
          Begin
            Result := Selection[i];
            Exit;
          End;
      End;
  Result := Nil;
End;
{--------}

Function TfsSqlSelectionList.GetSelection(
  Index: Integer): TfsSqlSelection;
Begin
  Result := TfsSqlSelection(FSelections[Index]);
  Assert(TObject(Result) Is TfsSqlSelection);
End;
{--------}

Function TfsSqlSelectionList.GetSelectionCount: Integer;
Begin
  Result := FSelections.Count;
End;
{--------}

Procedure TfsSqlSelectionList.InsertSelection(Index: Integer;
  NewSelection: TfsSqlSelection);
Begin
  FSelections.Insert(Index, NewSelection);
End;
{--------}

Function TfsSqlSelectionList.NonWildSelectionCount: Integer;
Var
  i: Integer;
Begin
  Result := 0;
  For i := 0 To pred(SelectionCount) Do
    If Not Selection[i].AddedByWildcard Then
      inc(Result);
End;

Function TfsSqlSelectionList.Reduce: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  For i := 0 To pred(SelectionCount) Do
    Result := Result Or Selection[i].Reduce;
End;

Procedure TfsSqlSelectionList.SetSelection(Index: Integer;
  Const Value: TfsSqlSelection);
Begin
  FSelections[Index] := Value;
End;
{====================================================================}

{===TfsSqlSimpleExpression===========================================}

Function TfsSqlSimpleExpression.AddTerm(Term: TfsSqlTerm): TfsSqlTerm;
Begin
  TermList.Add(Term);
  Result := Term;
End;
{--------}

Procedure TfsSqlSimpleExpression.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlSimpleExpression Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlSimpleExpression(Source).TermCount) Do
        Begin
          AddTerm(TfsSqlTerm.Create(Self)).Assign(
            TfsSqlSimpleExpression(Source).Term[i]);
        End;
    End
  Else
    AssignError(Source);
End;
{--------}

Constructor TfsSqlSimpleExpression.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  TermList := TList.Create;
End;
{--------}

Procedure TfsSqlSimpleExpression.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    Term[i].Free;
  TermList.Clear;
  Inherited;
End;
{--------}
{Begin !!.13}

Function TfsSqlSimpleExpression.ConcatBLOBValues(Const Value1, Value2: Variant): Variant;
Var
  VPtr1, VPtr2: PAnsiChar;
  VStr1, VStr2: String;
  VLen1, VLen2: Integer;
  VPtrResult: PAnsiChar;
Begin
  Try
    If VarType(Value1) And VarTypeMask = varByte Then
      Begin
        VPtr1 := VarArrayLock(Value1);
        VStr1 := '';
        VLen1 := VarArrayHighBound(Value1, 1);
      End
    Else
      Begin
        VStr1 := VarToStr(Value1);
        VPtr1 := PAnsiChar(VStr1);
        VLen1 := Length(VStr1);
      End;

    If VarType(Value2) And VarTypeMask = varByte Then
      Begin
        VPtr2 := VarArrayLock(Value2);
        VStr2 := '';
        VLen2 := VarArrayHighBound(Value2, 1);
      End
    Else
      Begin
        VStr2 := VarToStr(Value2);
        VPtr2 := PAnsiChar(VStr2);
        VLen2 := Length(VStr2);
      End;

    { Assumption: The result may always be returned as a BLOB value. }
    Result := VarArrayCreate([1, VLen1 + VLen2], varByte);
    VPtrResult := VarArrayLock(Result);
    Try
      Move(VPtr1^, VPtrResult^, VLen1);
      inc(VPtrResult, VLen1);
      Move(VPtr2^, VPtrResult^, VLen2);
    Finally
      VarArrayUnlock(Result);
    End;

  Finally
    If VStr1 = '' Then
      VarArrayUnlock(Value1);
    If VStr2 = '' Then
      VarArrayUnlock(Value2);
  End;
End;
{End !!.13}
{--------}

Function TfsSqlSimpleExpression.DependsOn(
  Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    If Term[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Destructor TfsSqlSimpleExpression.Destroy;
Begin
  Clear;
  TermList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlSimpleExpression.EmitSQL(Stream: TStream);
Const
  AddOpStr: Array[TfsSqlAddOp] Of String = (' + ', ' - ', ' || ');
Var
  i: Integer;
Begin
  Term[0].EmitSQL(Stream);
  For i := 1 To pred(TermCount) Do
    Begin
      WriteStr(Stream, AddOpStr[Term[i].AddOp]);
      Term[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlSimpleExpression.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(TermCount) Do
    Term[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlSimpleExpression.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlSimpleExpression Then
    Begin
      If TermCount <> TfsSqlSimpleExpression(Other).TermCount Then
        Exit;
      For i := 0 To pred(TermCount) Do
        If Not Term[i].Equals(TfsSqlSimpleExpression(Other).Term[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

// obs³uga int64

Function TfsSqlSimpleExpression.GetValue(aArrayIndex: Integer): Variant;
Var
  i: Integer;
  Op: Variant;
  Type1, Type2: TfsFieldType; {!!.13}
  E, E1: Extended;
Begin
  If assigned(OwnerSelect) And
    (OwnerSelect.AggQueryMode = aqmHaving) And Not IsConstant
    And Not IsParameter Then
    Begin
      Assert(BoundHaving);
      Result := BoundHavingField.GetValue(aArrayIndex);
      Exit;
    End;
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  Result := Term[0].GetValue(aArrayIndex);
  If VarIsNull(Result) Then
    Exit;
  For i := 1 To pred(TermCount) Do
    Begin
      Op := Term[i].GetValue(aArrayIndex);
      If VarIsNull(Op) Then
        Begin
          Result := Null;
          Exit;
        End;
      Type1 := Term[0].GetType;
      Type2 := Term[i].GetType;
      Case Term[i].AddOp Of
        aoPlus:
          If (Type1 In [fstDate, fstTime, fstDateTime]) And
            (Type2 = fstInterval) Then
            Result := Term[i].AddIntervalTo(Result)
          Else If (Type1 In [fstBLOB..fstBLOBGraphic]) Or
            (Type2 In [fstBLOB..fstBLOBGraphic]) Then
            Result := ConcatBLOBValues(Result, Op)
          Else
            Begin
              {$IFDEF IsNoVariantInt64}
              Case TVarData(Result).Vtype Of
                vt_e80:
                  Begin
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + decimal(Op).lo64;
                        End;
                      Else
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + Op;
                        End;
                    End;
                  End;
                vt_decimal:
                  Begin
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + decimal(Op).lo64;
                        End;
                      Else
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + Op;
                        End;
                    End;
                  End;
                Else
                  Begin
                    // op jest ró¿ne od Extended i int64
                    // ale Result mo¿e by int64 lub Extended
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := Result;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E + decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Op).lo64;
                          E1 := Result;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E1 + E;
                        End;
                      Else
                        Result := Result + Op;
                    End;
                  End;
              End;
              {$ELSE}
              Result := Result + Op;
              {$ENDIF}
            End;
        aoMinus:
          If (Type1 In [fstDate, fstTime, fstDateTime]) And
            (Type2 = fstInterval) Then
            Result := Term[i].SubtractIntervalFrom(Result)
          Else
            Begin
              {$IFDEF IsNoVariantInt64}
              Case TVarData(Result).Vtype Of
                vt_e80:
                  Begin
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - decimal(Op).lo64;
                        End;
                      Else
                        Begin
                          E := decimal(Result).ext80;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - Op;
                        End;
                    End;
                  End;
                vt_decimal:
                  Begin
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - decimal(Op).lo64;
                        End;
                      Else
                        Begin
                          E := decimal(Result).lo64;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - Op;
                        End;
                    End;
                  End;
                Else
                  Begin
                    // op jest ró¿ne od Extended i int64
                    // ale Result mo¿e by int64 lub Extended
                    Case TVarData(Op).Vtype Of
                      vt_e80:
                        Begin
                          E := Result;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E - decimal(Op).ext80;
                        End;
                      vt_decimal:
                        Begin
                          E := decimal(Op).lo64;
                          E1 := Result;
                          TVarData(Result).Vtype := VT_E80;
                          decimal(Result).ext80 := E1 - E;
                        End;
                      Else
                        Result := Result - Op;
                    End;
                  End;
              End;
              {$ELSE}
              Result := Result - Op;
              {$ENDIF}
            End;
        aoConcat:
          If (Type1 In [fstBLOB..fstBLOBGraphic]) Or
            (Type2 In [fstBLOB..fstBLOBGraphic]) Then
            Result := ConcatBLOBValues(Result, Op)
          Else
            Result := Result + Op;
      End;
    End;
End;
{--------}

Function TfsSqlSimpleExpression.HasFieldRef: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    If Term[i].HasFieldRef Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlSimpleExpression.IsAggregateExpression: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    If Term[i].IsAggregateExpression Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlSimpleExpression.IsField(Var FieldReferenced:
  TfsSqlFieldProxy): Boolean;
Begin
  Result := (TermCount = 1) And Term[0].IsField(FieldReferenced);
End;
{--------}

Function TfsSqlSimpleExpression.IsFieldFrom(
  Table: TFSSqlTableProxy; Var FieldReferenced: TfsSqlFieldProxy;
  Var SameCase: Boolean): Boolean;
Begin
  Result := (TermCount = 1) And Term[0].IsFieldFrom(Table,
    FieldReferenced, SameCase);
End;
{--------}

Function TfsSqlSimpleExpression.IsNull: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    If Term[i].IsNull Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlSimpleExpression.GetTerm(
  Index: Integer): TfsSqlTerm;
Begin
  Result := TfsSqlTerm(TermList[Index]);
End;
{--------}

Function TfsSqlSimpleExpression.GetTermCount: Integer;
Begin
  Result := TermList.Count;
End;
{--------}

Function TfsSqlSimpleExpression.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If TermCount = 1 Then
    Result := Term[0].GetTitle(Qualified) {!!.11}
  Else
    Result := 'EXP';
End;
{--------}

Function TfsSqlSimpleExpression.IsParameter: Boolean;
Begin
  Result := (TermCount = 1)
    And (Term[0].FactorCount = 1)
    And (Term[0].Factor[0].Param <> Nil);
End;
{--------}

Procedure TfsSqlSimpleExpression.BindHaving;
Var
  i: Integer;
Begin
  BindingHaving := True;
  Try
    If IsConstant
      Or IsParameter Then
      Exit;
  Finally
    BindingHaving := False;
  End;
  For i := 0 To pred(OwnerSelect.SelectionList.SelectionCount) Do
    If OwnerSelect.SelectionList.Selection[i].SimpleExpression.Equals(
      Self) Then
      Begin
        BoundHavingField := OwnerSelect.HavingTable.Field(i);
        BoundHaving := True;
        Exit;
      End;
  SQLError('Expression in HAVING clause doesn''t match any columns');
End;
{--------}

Function PropagateType(Type1, Type2: TfsFieldType): TfsFieldType;

  Function IsInt(Type1: TfsFieldType): Boolean;
  Begin
    Result := Type1 In [fstUInt8, fstUInt16, fstUInt32,
      fstInt8, fstInt16, fstInt32, fstAutoInc32, fstInt64, fstAutoinc64, fstRecVersion];
  End;

  Function IsSigned(Type1: TfsFieldType): Boolean;
  Begin
    Result := Type1 In [fstInt8, fstInt16, fstInt32, fstAutoInc32, fstAutoinc64, fstInt64, fstRecVersion];
  End;

  Function IsDate(Type1: TfsFieldType): Boolean;
  Begin
    Result := Type1 In [fstDate, fstTime, fstDateTime];
  End;

Begin
  If Type1 = Type2 Then
    Result := Type1
  Else
    Begin
      If IsInt(Type1) And IsInt(Type2) Then
        Begin
          If (Type1 In [fstInt64, fstAutoInc64, fstRecVersion]) Or (Type2 In [fstInt64, fstAutoInc64, fstRecVersion]) Then
            Result := fstInt64
          Else If (Type1 = fstUInt32) And (Type2 = fstUInt32) Then
            Result := fstUInt32
          Else
            Result := fstInt32;
        End
      Else
        Begin
          If IsDate(Type1) And IsDate(Type2) Then
            Begin
              If (Type1 = fstDateTime) Or (Type2 = fstDateTime) Then
                Result := fstDateTime
              Else If (Type1 = fstDate) Or (Type2 = fstDate) Then
                Result := fstDate
              Else
                Result := fstTime;
            End
          Else
            Begin
              If IsDate(Type1) Then
                Begin
                  If (Type1 = fstDateTime) Then
                    Result := fstDateTime
                  Else If (Type1 = fstDate) Then
                    Result := fstDate
                  Else
                    Result := fstTime;
                End
              Else
                Result := fstExtended;
            End;
        End;
    End;
End;
{--------}

Procedure TfsSqlSimpleExpression.CheckType;
Var
  i: Integer;
  Type2: TfsFieldType;
Begin
  FType := Term[0].GetType;
  If TermCount > 1 Then
    Begin
      Case Term[1].AddOp Of
        aoPlus:
          Case FType Of
            fstUInt8,
              fstUInt16,
              fstUInt32,
              fstInt8,
              fstInt16,
              fstInt32,
              fstAutoInc32,
              fstAutoInc64,
              fstSingle,
              fstDouble,
              fstExtended,
              fstInt64,
              fstRecVersion,
              fstCurrency,
              fstBcd,
              fstDate,
              fstTime,
              fstDateTime,
              fstSingleChar,
              fstSingleWideChar,
              fstShortString,
              fstVarNullString,
              fstNullString,
              fstVarWideString,
              fstWideString:
              ;
            Else
              SQLError('Operator/operand mismatch');
          End;
        aoMinus:
          Case FType Of
            fstUInt8,
              fstUInt16,
              fstUInt32,
              fstInt8,
              fstInt16,
              fstInt32,
              fstAutoInc32,
              fstAutoInc64,
              fstSingle,
              fstDouble,
              fstExtended,
              fstInt64,
              fstRecVersion,
              fstCurrency, fstBcd,
              fstDate,
              fstTime,
              fstDateTime: ;
            Else
              SQLError('Operator/operand mismatch');
          End;
        aoConcat:
          Case FType Of
            fstSingleChar,
              fstSingleWideChar,
              fstShortString,
              fstVarNullString,
              fstNullString,
              fstVarWideString,
              fstWideString:
              ;
            Else
              SQLError('Operator/operand mismatch');
          End;
      End;
      For i := 1 To pred(TermCount) Do
        Begin
          Type2 := Term[i].GetType;
          Case Term[i].AddOp Of
            aoPlus:
              Case Type2 Of
                fstUInt8,
                  fstUInt16,
                  fstUInt32,
                  fstInt8,
                  fstInt16,
                  fstInt32,
                  fstAutoInc32,
                  fstAutoInc64,
                  fstSingle,
                  fstDouble,
                  fstExtended,
                  fstInt64,
                  fstRecVersion,
                  fstCurrency,
                  fstBcd,
                  fstSingleChar,
                  fstSingleWideChar,
                  fstShortString,
                  fstVarNullString,
                  fstNullString,
                  fstVarWideString,
                  fstWideString,
                  fstDate,
                  fstTime,
                  fstDateTime,
                  fstInterval:
                Else
                  SQLError('Operator/operand mismatch');
              End;
            aoMinus:
              Case Type2 Of
                fstUInt8,
                  fstUInt16,
                  fstUInt32,
                  fstInt8,
                  fstInt16,
                  fstInt32,
                  fstAutoInc32,
                  fstAutoInc64,
                  fstSingle,
                  fstDouble,
                  fstExtended,
                  fstInt64,
                  fstRecVersion,
                  fstCurrency,
                  fstBcd,
                  fstDate,
                  fstTime,
                  fstDateTime,
                  fstInterval:
                  ;
                Else
                  SQLError('Operator/operand mismatch');
              End;
            aoConcat:
              Case Type2 Of
                fstSingleChar,
                  fstSingleWideChar,
                  fstShortString,
                  fstVarNullString,
                  fstNullString,
                  fstVarWideString,
                  fstWideString:
                  ;
                Else
                  SQLError('Operator/operand mismatch');
              End;
          End;
          Case Type2 Of
            fstUInt8, fstUInt16, fstUInt32, fstInt8, fstInt16, fstInt32,
              fstAutoInc32, fstAutoInc64, fstSingle, fstDouble, fstExtended,
              fstInt64, fstRecVersion, fstCurrency, fstDate, fstTime, fstDateTime, fstBcd:
              FType := PropagateType(FType, Type2);
          End;
        End;
    End;
  TypeKnown := True;
End;
{--------}

Function TfsSqlSimpleExpression.GetDecimals: Integer;
Var
  i, j: Integer;
Begin
  Result := 0;
  If Term[0] <> Nil Then
    Result := Term[0].GetDecimals;
  For i := 1 To pred(TermCount) Do
    Begin
      j := 0;
      If Term[i] <> Nil Then
        j := Term[i].GetDecimals;
      If j > Result Then
        Result := j;
    End;
End;

Function TfsSqlSimpleExpression.GetRound: TRound;
Var
  i: Integer;
  j: TRound;
Begin
  Result := RNone;
  If Term[0] <> Nil Then
    Result := Term[0].GetRound;
  For i := 1 To pred(TermCount) Do
    Begin
      j := Term[i].GetRound;
      If j <> Result Then
        Result := j;
    End;
End;

Function TfsSqlSimpleExpression.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If Term[0] <> Nil Then
    Result := Term[0].GetBlobLevel;
End;
{--------}

Function TfsSqlSimpleExpression.GetSize: Integer;
Var
  i: Integer;
Begin
  Result := Term[0].GetSize;
  {operator here can only be aoConcat
(because GetSize is only called For text fields)}
  For i := 1 To pred(TermCount) Do
    inc(Result, Term[i].GetSize);
End;

{--------}

Function TfsSqlSimpleExpression.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    CheckType;
  Result := FType
End;
{--------}

Function TfsSqlSimpleExpression.IsAggregate: Boolean;
Begin
  Result := (TermCount = 1) And Term[0].IsAggregate;
End;
{--------}

Procedure TfsSqlSimpleExpression.CheckIsConstant;
Var
  i: Integer;
  Save: TfsSqlAggQueryMode;
  v: Variant;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(TermCount) Do
    If Not Term[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  If Not BindingHaving Then
    Begin
      Save := aqmIdle;
      If assigned(OwnerSelect) Then
        Begin
          Save := OwnerSelect.AggQueryMode;
          OwnerSelect.AggQueryMode := aqmIdle;
        End;
      v := getvalue(-1);
      {$IFDEF IsNoVariantInt64}
      If (TVarData(V).VType = VT_DECIMAL) Then
        Begin
          TVarData(ConstantValue).VType := VT_DECIMAL;
          Decimal(ConstantValue).lo64 := Decimal(V).lo64;
        End
      Else If (TVarData(V).VType = VT_E80) Then
        Begin
          TVarData(ConstantValue).VType := VT_E80;
          Decimal(ConstantValue).ext80 := Decimal(V).ext80;
        End
      Else
        ConstantValue := v;
      {$ELSE}
      ConstantValue := v;
      {$ENDIF}
      If assigned(OwnerSelect) Then
        OwnerSelect.AggQueryMode := Save;
    End;
  FIsConstant := True;
End;
{--------}

Function TfsSqlSimpleExpression.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Procedure TfsSqlSimpleExpression.MatchType(ExpectedType: TfsFieldType);
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    Term[i].MatchType(ExpectedType);
End;
{--------}

Function TfsSqlSimpleExpression.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(TermCount) Do
    If Term[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlSimpleExpression.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{--------}

Procedure TfsSqlSimpleExpression.SetTerm(Index: Integer;
  Const Value: TfsSqlTerm);
Begin
  TermList[Index] := Value;
End;
{Begin !!.11}
{--------}

Function TfsSqlSimpleExpression.WasWildcard: Boolean;
Begin
  If TermCount = 1 Then
    Result := Term[0].WasWildcard
  Else
    Result := False;
End;
{End !!.11}
{====================================================================}

{===TfsSqlTerm=======================================================}

Function TfsSqlTerm.AddFactor(Factor: TfsSqlFactor): TfsSqlFactor;
Begin
  FactorList.Add(Factor);
  Result := Factor;
End;
{--------}

Function TfsSqlTerm.AddIntervalTo(Target: TDateTime): TDateTime;
Begin
  Result := Factor[0].AddIntervalTo(Target);
End;
{--------}

Function TfsSqlTerm.SubtractIntervalFrom(Target: TDateTime): TDateTime;
Begin
  Result := Factor[0].SubtractIntervalFrom(Target);
End;
{--------}

Procedure TfsSqlTerm.CheckIsConstant;
Var
  i: Integer;
  v: Variant;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(FactorCount) Do
    If Not Factor[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  v := getvalue(-1);
  {$IFDEF IsNoVariantInt64}
  If (TVarData(V).VType = VT_DECIMAL) Then
    Begin
      TVarData(ConstantValue).VType := VT_DECIMAL;
      Decimal(ConstantValue).lo64 := Decimal(V).lo64;
    End
  Else If (TVarData(V).VType = VT_E80) Then
    Begin
      TVarData(ConstantValue).VType := VT_E80;
      Decimal(ConstantValue).ext80 := Decimal(V).ext80;
    End
  Else
    ConstantValue := v;
  {$ELSE}
  ConstantValue := v;
  {$ENDIF}
  FIsConstant := True;
End;
{--------}

Procedure TfsSqlTerm.CheckType;
Var
  i: Integer;
  Type2: TfsFieldType;
Begin
  FType := Factor[0].GetType;
  If FactorCount > 1 Then
    Begin
      Case Factor[1].MulOp Of
        moMul, moDiv:
          Case FType Of
            fstUInt8,
              fstUInt16,
              fstUInt32,
              fstInt8,
              fstInt16,
              fstInt32,
              fstAutoInc32,
              fstAutoInc64,
              fstSingle,
              fstDouble,
              fstExtended,
              fstInt64,
              fstRecVersion,
              fstCurrency,
              fstBcd:
              ;
            Else
              SQLError('Operator/operand mismatch');
          End;
      End;
      For i := 1 To pred(FactorCount) Do
        Begin
          Case Factor[i].MulOp Of
            moMul, moDiv:
              Begin
                Type2 := Factor[i].GetType;
                Case Type2 Of
                  fstUInt8,
                    fstUInt16,
                    fstUInt32,
                    fstInt8,
                    fstInt16,
                    fstInt32,
                    fstAutoInc32,
                    fstAutoInc64,
                    fstSingle,
                    fstDouble,
                    fstExtended,
                    fstInt64,
                    fstRecVersion,
                    fstCurrency, fstBcd:
                    ;
                  Else
                    SQLError('Operator/operand mismatch');
                End;
                Case Type2 Of
                  fstUInt8, fstUInt16, fstUInt32, fstInt8, fstInt16, fstInt32,
                    fstAutoInc32, fstAutoInc64, fstRecVersion, fstSingle, fstDouble, fstExtended, fstInt64,
                    fstCurrency, fstBcd:
                    FType := PropagateType(FType, Type2);
                End;
              End;
          End;
        End;
    End;
  TypeKnown := True;
End;
{--------}

Procedure TfsSqlTerm.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlTerm Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlTerm(Source).FactorCount) Do
        Begin
          AddFactor(TfsSqlFactor.Create(Self)).Assign(
            TfsSqlTerm(Source).Factor[i]);
        End;
      AddOp := TfsSqlTerm(Source).AddOp;
    End
  Else
    AssignError(Source);
End;
{--------}

Constructor TfsSqlTerm.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FactorList := TList.Create;
End;
{--------}

Procedure TfsSqlTerm.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    Factor[i].Free;
  FactorList.Clear;
End;
{--------}

Function TfsSqlTerm.DependsOn(Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    If Factor[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Destructor TfsSqlTerm.Destroy;
Begin
  Clear;
  FactorList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlTerm.EmitSQL(Stream: TStream);
Const
  MulOpStr: Array[TfsSqlMulOp] Of String = (' * ', ' / ');
Var
  i: Integer;
Begin
  Factor[0].EmitSQL(Stream);
  For i := 1 To pred(FactorCount) Do
    Begin
      WriteStr(Stream, MulOpStr[Factor[i].MulOp]);
      Factor[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlTerm.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(FactorCount) Do
    Factor[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlTerm.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If (Other Is TfsSqlTerm)
    And (AddOp = TfsSqlTerm(Other).AddOp) Then
    Begin
      If FactorCount <> TfsSqlTerm(Other).FactorCount Then
        Exit;
      For i := 0 To pred(FactorCount) Do
        If Not Factor[i].Equals(TfsSqlTerm(Other).Factor[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlTerm.GetFactor(Index: Integer): TfsSqlFactor;
Begin
  Result := TfsSqlFactor(FactorList[Index]);
End;
{--------}

Function TfsSqlTerm.GetFactorCount: Integer;
Begin
  Result := FactorList.Count;
End;
{--------}

Function TfsSqlTerm.GetDecimals: Integer;
Var
  i, j: Integer;
Begin
  Result := 0;
  If Factor[0] <> Nil Then
    Result := Factor[0].GetDecimals;
  For i := 1 To pred(FactorCount) Do
    Begin
      j := 0;
      If Factor[i] <> Nil Then
        j := Factor[i].GetDecimals;
      If j > Result Then
        Result := j;
    End;
End;

Function TfsSqlTerm.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If Factor[0] <> Nil Then
    Result := Factor[0].GetBlobLevel;
End;
{--------}

Function TfsSqlTerm.GetSize: Integer;
Begin
  Result := Factor[0].GetSize;
End;

Function TfsSqlTerm.GetRound: TRound;
Begin
  Result := RNone;
  If Factor[0] <> Nil Then
    Result := Factor[0].GetRound;
End;
{--------}

Function TfsSqlTerm.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If FactorCount = 1 Then
    Result := Factor[0].GetTitle(Qualified) {!!.11}
  Else
    Result := 'EXP';
End;
{--------}

Function TfsSqlTerm.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    CheckType;
  Result := FType
End;

{--------}

// obs³uga int64

Function TfsSqlTerm.GetValue(aArrayIndex: Integer): Variant;
Var
  i: Integer;
  Op: Variant;
  E, E1: Extended;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  Result := Factor[0].GetValue(aArrayIndex);
  If VarIsNull(Result) Then
    Exit;
  For i := 1 To pred(FactorCount) Do
    Begin
      Op := Factor[i].GetValue(aArrayIndex);
      If VarIsNull(Op) Then
        Begin
          Result := Null;
          Exit;
        End;
      Case Factor[i {1}].MulOp Of {!!.11}
        moMul:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Result).Vtype Of
              vt_e80:
                Begin
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * decimal(Op).lo64;
                      End;
                    Else
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * Op;
                      End;
                  End;
                End;
              vt_decimal:
                Begin
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * decimal(Op).lo64;
                      End;
                    Else
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * Op;
                      End;
                  End;
                End;
              Else
                Begin
                  // op jest ró¿ne od Extended i int64
                  // ale Result mo¿e by int64 lub Extended
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := Result;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E * decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Op).lo64;
                        E1 := Result;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E1 * E;
                      End;
                    Else
                      Result := Result * Op;
                  End;
                End;
            End;
            {$ELSE}
            Result := Result * Op;
            {$ENDIF}
          End;
        moDiv:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Result).Vtype Of
              vt_e80:
                Begin
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / decimal(Op).lo64;
                      End;
                    Else
                      Begin
                        E := decimal(Result).ext80;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / Op;
                      End;
                  End;
                End;
              vt_decimal:
                Begin
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / decimal(Op).lo64;
                      End;
                    Else
                      Begin
                        E := decimal(Result).lo64;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / Op;
                      End;
                  End;
                End;
              Else
                Begin
                  // op jest ró¿ne od Extended i int64
                  // ale Result mo¿e by int64 lub Extended
                  Case TVarData(Op).Vtype Of
                    vt_e80:
                      Begin
                        E := Result;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E / decimal(Op).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Op).lo64;
                        E1 := Result;
                        TVarData(Result).Vtype := VT_E80;
                        decimal(Result).ext80 := E1 / E;
                      End;
                    Else
                      Result := Result / Op;
                  End;
                End;
            End;
            {$ELSE}
            Result := Result / Op;
            {$ENDIF}
          End;
      End;
    End;
End;
{--------}

Function TfsSqlTerm.IsAggregate: Boolean;
Begin
  Result := (FactorCount = 1) And Factor[0].IsAggregate;
End;
{--------}

Function TfsSqlTerm.HasFieldRef: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    If Factor[i].HasFieldRef Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlTerm.IsAggregateExpression: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    If Factor[i].IsAggregate Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlTerm.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Function TfsSqlTerm.IsField(Var FieldReferenced: TfsSqlFieldProxy): Boolean;
Begin
  Result := (FactorCount = 1) And Factor[0].IsField(FieldReferenced);
End;
{--------}

Function TfsSqlTerm.IsFieldFrom(Table: TFSSqlTableProxy;
  Var FieldReferenced: TfsSqlFieldProxy; Var SameCase: Boolean): Boolean;
Begin
  Result := (FactorCount = 1) And Factor[0].IsFieldFrom(Table, FieldReferenced,
    SameCase);
End;
{--------}

Function TfsSqlTerm.IsNull: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    If Factor[i].IsNull Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlTerm.MatchType(ExpectedType: TfsFieldType);
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    Factor[i].MatchType(ExpectedType);
End;
{--------}

Function TfsSqlTerm.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(FactorCount) Do
    If Factor[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlTerm.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{--------}

Procedure TfsSqlTerm.SetFactor(Index: Integer;
  Const Value: TfsSqlFactor);
Begin
  FactorList[Index] := Value;
End;
{Begin !!.11}
{--------}

Function TfsSqlTerm.WasWildcard: Boolean;
Begin
  If FactorCount = 1 Then
    Result := Factor[0].WasWildcard
  Else
    Result := False;
End;
{End !!.11}
{====================================================================}

{===TfsSqlCondExp====================================================}

Function TfsSqlCondExp.AddCondTerm(Term: TfsSqlCondTerm): TfsSqlCondTerm;
Begin
  CondTermList.Add(Term);
  Result := Term;
End;
{--------}

Function TfsSqlCondExp.AsBooleanLevel(Level: Integer): Boolean;
Var
  i: Integer;
Begin
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  For i := 0 To pred(CondTermCount) Do
    If CondTerm[i].AsBooleanLevel(Level) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlCondExp.AsBoolean: Boolean;
Var
  i: Integer;
Begin
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  For i := 0 To pred(CondTermCount) Do
    If CondTerm[i].AsBoolean Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlCondExp.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlCondExp Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlCondExp(Source).CondTermCount) Do
        AddCondTerm(TfsSqlCondTerm.Create(Self)).Assign(
          TfsSqlCondExp(Source).CondTerm[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlCondExp.BindHaving;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondTermCount) Do
    CondTerm[i].BindHaving;
End;

Procedure TfsSqlCondExp.CheckIsConstant;
Var
  i: Integer;
  v: Variant;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(CondTermCount) Do
    If Not CondTerm[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  v := getvalue(-1);
  {$IFDEF IsNoVariantInt64}
  If (TVarData(V).VType = VT_DECIMAL) Then
    Begin
      TVarData(ConstantValue).VType := VT_DECIMAL;
      Decimal(ConstantValue).lo64 := Decimal(V).lo64;
    End
  Else If (TVarData(V).VType = VT_E80) Then
    Begin
      TVarData(ConstantValue).VType := VT_E80;
      Decimal(ConstantValue).ext80 := Decimal(V).ext80;
    End
  Else
    ConstantValue := v;
  {$ELSE}
  ConstantValue := v;
  {$ENDIF}
  FIsConstant := True;
End;

Constructor TfsSqlCondExp.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  CondTermList := TList.Create;
End;
{--------}

Procedure TfsSqlCondExp.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondTermCount) Do
    CondTerm[i].Free;
  CondTermList.Clear;
End;
{--------}

Function TfsSqlCondExp.DependsOn(Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondTermCount) Do
    If CondTerm[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Destructor TfsSqlCondExp.Destroy;
Begin
  Clear;
  CondTermList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlCondExp.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  CondTerm[0].EmitSQL(Stream);
  For i := 1 To pred(CondTermCount) Do
    Begin
      WriteStr(Stream, ' OR');
      CondTerm[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlCondExp.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(CondTermCount) Do
    CondTerm[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlCondExp.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlCondExp Then
    Begin
      If CondTermCount <> TfsSqlCondExp(Other).CondTermCount Then
        Exit;
      For i := 0 To pred(CondTermCount) Do
        If Not CondTerm[i].Equals(TfsSqlCondExp(Other).CondTerm[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlCondExp.GetCondTerm(
  Index: Integer): TfsSqlCondTerm;
Begin
  Result := TfsSqlCondTerm(CondTermList[Index]);
End;
{--------}

Function TfsSqlCondExp.GetCondTermCount: Integer;
Begin
  Result := CondTermList.Count;
End;
{--------}

Function TfsSqlCondExp.GetRound: TRound;
Begin
  Result := RNone;
  If CondTermCount > 1 Then
    TypeMismatch;
  If CondTerm[0] <> Nil Then
    Result := CondTerm[0].GetRound;
End;

Function TfsSqlCondExp.GetDecimals: Integer;
Begin
  Result := 0;
  If CondTermCount > 1 Then
    TypeMismatch;
  If CondTerm[0] <> Nil Then
    Result := CondTerm[0].GetDecimals;
End;

Function TfsSqlCondExp.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If CondTermCount > 1 Then
    TypeMismatch;
  If CondTerm[0] <> Nil Then
    Result := CondTerm[0].GetBlobLevel;
End;
{--------}
{!!.10 new}

Function TfsSqlCondExp.GetSize: Integer;
Begin
  If CondTermCount > 1 Then
    Result := 1
  Else
    Result := CondTerm[0].GetSize;
End;
{--------}

Function TfsSqlCondExp.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If CondTermCount > 1 Then
    Result := 'COND'
  Else
    Result := CondTerm[0].GetTitle(Qualified); {!!.11}
End;

{--------}

Function TfsSqlCondExp.GetType: TfsFieldType;
Var
  i: Integer;
Begin
  If CondTermCount > 1 Then
    Begin
      {force Type conversion at lower level If necessary}
      For i := 0 To pred(CondTermCount) Do
        CondTerm[i].GetType;
      Result := fstBoolean
    End
  Else
    Result := CondTerm[0].GetType;
End;
{--------}

Function TfsSqlCondExp.GetValue(aArrayIndex: Integer): Variant;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  If CondTermCount > 1 Then
    Result := AsBoolean
  Else
    Result := CondTerm[0].GetValue(aArrayIndex);
End;
{--------}

Function TfsSqlCondExp.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Procedure TfsSqlCondExp.MatchType(ExpectedType: TfsFieldType);
Begin
  If CondTermCount = 1 Then {!!.11}
    CondTerm[0].MatchType(ExpectedType) {!!.11}
  Else {!!.11}  If GetType <> ExpectedType Then
    TypeMismatch;
End;
{--------}

Function TfsSqlCondExp.Reduce: Boolean;
Var
  i, j: Integer;
  InFactIX,
    InTermIX: Integer;
  NewTerm, LiftTerm: TfsSqlCondTerm;
  NewFactor: TfsSqlCondFactor;
  NewPrimary: TfsSqlCondPrimary;
  LiftInClause: TfsSqlInClause;
  LiftInExp: TfsSqlSimpleExpression;
  LiftExp: TfsSqlCondExp;
Begin
  Result := False;
  LiftInClause := Nil;
  LiftInExp := Nil;
  LiftExp := Nil;
  InTermIX := -1; //just to make the compiler happy
  InFactIX := -1; //just to make the compiler happy
  For i := 0 To pred(CondTermCount) Do
    Begin
      {look For conditional terms nested inside redundant parens}
        {eliminate parens when found}
      LiftTerm := Nil;
      LiftExp := Nil;
      With CondTerm[i] Do
        Begin
          If CondFactorCount = 1 Then
            Begin
              With CondFactor[0] Do
                If Not UnaryNot Then
                  If (CondPrimary.RelOp = roNone) Then
                    If CondPrimary.SimpleExp1 <> Nil Then
                      If CondPrimary.JustSimpleExpression Then
                        With CondPrimary.SimpleExp1 Do
                          If TermCount = 1 Then
                            Begin
                              With Term[0] Do
                                If FactorCount = 1 Then
                                  With Factor[0] Do
                                    If CondExp <> Nil Then
                                      With CondExp Do
                                        If CondTermCount = 1 Then
                                          Begin
                                            LiftTerm := TfsSqlCondTerm.Create(Self);
                                            LiftTerm.Assign(CondTerm[0]);
                                          End;
                            End;
            End;
          If LiftTerm <> Nil Then
            Begin
              Clear;
              Assign(LiftTerm);
              LiftTerm.Free;
              Result := True;
              {Get out. We may have more to do here, but Global Logic will
              call us again, And there may be other transformations that can
              be applied first.}
              break;
            End;
          If Reduce Then
            Begin
              {term itself was reduced}
              Result := True;
              break;
            End;
          If Not Result Then
            Begin
              {look For IN expressions to be converted to simple comparisons}
              For j := 0 To pred(CondFactorCount) Do
                With CondFactor[j] Do
                  If Not UnaryNot Then {can't handle negated expressions}
                    If CondPrimary.RelOp = roNone Then
                      If (CondPrimary.InClause <> Nil)
                        And Not (CondPrimary.InClause.Negated)
                        And (CondPrimary.InClause.SubQuery = Nil)
                        And (CondPrimary.InClause.SimpleExpList.ExpressionCount <=
                        fsSqlInConvThreshold) Then
                        Begin
                          {Here's one. Make a copy of it And get up to the
                          root level since we'll be doing surgery on this
                          very node hierarchy we're current looking at}
                          LiftInClause := TfsSqlInClause.Create(Self);
                          LiftInClause.Assign(CondPrimary.InClause);
                          LiftInExp := TfsSqlSimpleExpression.Create(Self);
                          LiftInExp.Assign(CondPrimary.SimpleExp1);
                          InTermIX := i; // just a reference back to here
                          If CondFactorCount > 1 Then
                            {we have other factors that need to be copied -
                            make note Of where the IN is - we should copy
                            everything BUT}
                            InFactIX := j
                              {we're the only factor, make a note Of that by
setting the InFactIX flag to -1 indicating no
other factors should be copied}
                          Else
                            InFactIX := -1;
                          break;
                        End;
            End;
          If Not Result Then
            Begin
              {look For nested conditional expressions to be lifted out, like
                (A OR B) And C to be converted to A And C OR B And C}
              For j := 0 To pred(CondFactorCount) Do
                With CondFactor[j] Do
                  If Not UnaryNot Then
                    If (CondPrimary.RelOp = roNone) Then
                      If CondPrimary.SimpleExp1 <> Nil Then
                        If CondPrimary.JustSimpleExpression Then
                          With CondPrimary.SimpleExp1 Do
                            If TermCount = 1 Then
                              Begin
                                With Term[0] Do
                                  If FactorCount = 1 Then
                                    With Factor[0] Do
                                      If CondExp <> Nil Then
                                        Begin
                                          LiftExp := TfsSqlCondExp.Create(Self);
                                          LiftExp.Assign(CondExp);
                                          InTermIX := i; // A reference back to here
                                          InFactIX := j; // A reference back to here
                                        End;
                              End;
            End;
          If LiftInClause <> Nil Then
            break;
          If LiftExp <> Nil Then
            break;
        End;
    End;
  If LiftExp <> Nil Then
    Begin
      {create a top-level conditional term For each nested term,
        then copy each conditional factor Except the one we're converting
        to each new term:}
      For i := 0 To pred(LiftExp.CondTermCount) Do
        Begin
          NewTerm := TfsSqlCondTerm.Create(Self);
          NewTerm.Assign(LiftExp.CondTerm[i]);
          For j := 0 To pred(CondTerm[InTermIX].CondFactorCount) Do
            If j <> InFactIX Then
              Begin
                NewFactor := TfsSqlCondFactor.Create(NewTerm);
                NewFactor.Assign(CondTerm[InTermIX].CondFactor[j]);
                NewTerm.AddCondFactor(NewFactor);
              End;
          AddCondTerm(NewTerm);
        End;
      LiftInClause.Free;
      LiftInExp.Free;
      LiftExp.Free;
      CondTerm[InTermIX].Free;
      CondTermList.Delete(InTermIX);
      Result := True;
      Exit;
    End;
  If (LiftInClause <> Nil)
    And (InFactIX = -1) Then
    Begin {only do this optimization If no other factors} {!!.11}
      {Okay - that was the easy bit, finding the IN clause.
    We now need to build conditional terms For each Of the
      alternatives - each With a simple comparison corresponding
      to each entry in the IN clause list.}
      For i := 0 To pred(LiftInClause.SimpleExpList.ExpressionCount) Do
        Begin
          NewTerm := TfsSqlCondTerm.Create(Self);
          NewFactor := TfsSqlCondFactor.Create(NewTerm);
          NewPrimary := TfsSqlCondPrimary.Create(NewFactor);
          NewPrimary.SimpleExp1 := TfsSqlSimpleExpression.Create(NewPrimary);
          NewPrimary.SimpleExp1.Assign(LiftInExp);
          NewPrimary.SimpleExp2 := TfsSqlSimpleExpression.Create(NewPrimary);
          NewPrimary.SimpleExp2.Assign(LiftInClause.SimpleExpList.Expression[i]);
          NewPrimary.RelOp := roEQ;
          NewFactor.CondPrimary := NewPrimary;
          NewTerm.AddCondFactor(NewFactor);
          {If we didn't have any other conditional factors
          combined With the IN clause - IOW, we didn't have something like
          Exp IN [blahblah] And something else,
          then we're actually done. All we need to do is add each term, then
          finish off by deleting the original term which held the IN clause.

        On the other hand, If we did have other factors, they all need to
          be copied to the new term:}
          If InFactIX <> -1 Then
            Begin
              With CondTerm[InTermIX] Do
                For j := 0 To pred(CondFactorCount) Do
                  If j <> InFactIX Then
                    Begin
                      NewFactor := TfsSqlCondFactor.Create(NewTerm);
                      NewFactor.Assign(CondFactor[j]);
                      NewTerm.AddCOndFactor(NewFactor);
                    End;
            End;

          AddCondTerm(NewTerm);
        End;
      {LiftInClause.Free;}{!!.12}
      {LiftInExp.Free;}{!!.12}
      //get rid of the original term With the IN clause
      CondTerm[InTermIX].Free;
      CondTermList.Delete(InTermIX);
      Result := True;
    End;
  LiftInClause.Free; {!!.12}
  LiftInExp.Free; {!!.12}
  {!!.11 begin}
  If Not Result Then
    For i := 0 To pred(CondTermCount) Do
      If CondTerm[i].Reduce Then
        Begin
          Result := True;
          break;
        End;
  {!!.11 end}
End;

Procedure TfsSqlCondExp.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

Procedure TfsSqlCondExp.SetCondTerm(Index: Integer;
  Const Value: TfsSqlCondTerm);
Begin
  CondTermList[Index] := Value;
End;

Procedure TfsSqlCondExp.SetLevelDep(List: TFSSqlTableProxySubsetList);
Var
  i: Integer;
Begin
  For i := 0 To pred(CondTermCount) Do
    CondTerm[i].SetLevelDep(List);
End;

{====================================================================}

{===TfsSqlCondTerm===================================================}

Function TfsSqlCondTerm.AddCondFactor(Factor: TfsSqlCondFactor): TfsSqlCondFactor;
Begin
  CondFactorList.Add(Factor);
  Result := Factor;
End;
{--------}

Function TfsSqlCondTerm.InsertCondFactor(Index: Integer;
  Factor: TfsSqlCondFactor): TfsSqlCondFactor;
Begin
  CondFactorList.Insert(Index, Factor);
  Result := Factor;
End;
{--------}

Procedure TfsSqlCondTerm.SetLevelDep(List: TFSSqlTableProxySubsetList);
Var
  F, Level: Integer;
Begin
  For F := 0 To pred(CondFactorCount) Do
    With CondFactor[F] Do
      Begin
        EvalLevel := List.Count;
        For Level := pred(List.Count) Downto 0 Do
          If DependsOn(List.Item[Level].Table) Then
            EvalLevel := Level;
      End;
End;

Function TfsSqlCondTerm.AsBoolean: Boolean;
Var
  i: Integer;
Begin
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  For i := 0 To pred(CondFactorCount) Do
    If Not CondFactor[i].AsBoolean Then
      Begin
        Result := False;
        Exit;
      End;
  Result := True;
End;
{--------}

Function TfsSqlCondTerm.AsBooleanLevel(Level: Integer): Boolean;
Var
  i: Integer;
Begin
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  For i := 0 To pred(CondFactorCount) Do
    If (CondFactor[i].EvalLevel >= Level)
      And Not CondFactor[i].AsBoolean Then
      Begin
        Result := False;
        Exit;
      End;
  Result := True;
End;
{--------}

Procedure TfsSqlCondTerm.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlCondTerm Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlCondTerm(Source).CondFactorCount) Do
        Begin
          AddCondFactor(TfsSqlCondFactor.Create(Self)).Assign(
            TfsSqlCondTerm(Source).CondFactor[i]);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlCondTerm.BindHaving;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondFactorCount) Do
    CondFactor[i].BindHaving;
End;

Procedure TfsSqlCondTerm.CheckIsConstant;
Var
  i: Integer;
  v: Variant;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(CondFactorCount) Do
    If Not CondFactor[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  v := getvalue(-1);
  {$IFDEF IsNoVariantInt64}
  If (TVarData(V).VType = VT_DECIMAL) Then
    Begin
      TVarData(ConstantValue).VType := VT_DECIMAL;
      Decimal(ConstantValue).lo64 := Decimal(V).lo64;
    End
  Else If (TVarData(V).VType = VT_E80) Then
    Begin
      TVarData(ConstantValue).VType := VT_E80;
      Decimal(ConstantValue).ext80 := Decimal(V).ext80;
    End
  Else
    ConstantValue := v;
  {$ELSE}
  ConstantValue := v;
  {$ENDIF}
  FIsConstant := True;
End;

Constructor TfsSqlCondTerm.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  CondFactorList := TList.Create;
End;
{--------}

Procedure TfsSqlCondTerm.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondFactorCount) Do
    CondFactor[i].Free;
  CondFactorList.Clear;
End;
{--------}

Function TfsSqlCondTerm.DependsOn(Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(CondFactorCount) Do
    If CondFactor[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Destructor TfsSqlCondTerm.Destroy;
Begin
  Clear;
  CondFactorList.Free;
  OrderedSources.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlCondTerm.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  CondFactor[0].EmitSQL(Stream);
  For i := 1 To pred(CondFactorCount) Do
    Begin
      WriteStr(Stream, ' AND');
      CondFactor[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlCondTerm.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(CondFactorCount) Do
    CondFactor[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlCondTerm.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlCondTerm Then
    Begin
      If CondFactorCount <> TfsSqlCondTerm(Other).CondFactorCount Then
        Exit;
      For i := 0 To pred(CondFactorCount) Do
        If Not CondFactor[i].Equals(TfsSqlCondTerm(Other).CondFactor[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlCondTerm.GetCondFactor(
  Index: Integer): TfsSqlCondFactor;
Begin
  Result := TfsSqlCondFactor(CondFactorList[Index]);
End;
{--------}

Function TfsSqlCondTerm.GetCondFactorCount: Integer;
Begin
  Result := CondFactorList.Count;
End;
{--------}

Function TfsSqlCondTerm.GetRound: TRound;
Begin
  Result := RNone;
  If CondFactorCount > 1 Then
    TypeMismatch;
  If CondFactor[0] <> Nil Then
    Result := CondFactor[0].GetRound;
End;

Function TfsSqlCondTerm.GetDecimals: Integer;
Begin
  Result := 0;
  If CondFactorCount > 1 Then
    TypeMismatch;
  If CondFactor[0] <> Nil Then
    Result := CondFactor[0].GetDecimals;
End;

Function TfsSqlCondTerm.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If CondFactorCount > 1 Then
    TypeMismatch;
  If CondFactor[0] <> Nil Then
    Result := CondFactor[0].GetBlobLevel;
End;
{--------}
{!!.10 new}

Function TfsSqlCondTerm.GetSize: Integer;
Begin
  If CondFactorCount > 1 Then
    Result := 1
  Else
    Result := CondFactor[0].GetSize;
End;
{--------}

Function TfsSqlCondTerm.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If CondFactorCount > 1 Then
    Result := 'COND'
  Else
    Result := CondFactor[0].GetTitle(Qualified); {!!.11}
End;

{--------}

Function TfsSqlCondTerm.GetType: TfsFieldType;
Var
  i: Integer;
Begin
  If CondFactorCount > 1 Then
    Begin
      {force Type conversion at lower level If necessary}
      For i := 0 To pred(CondFactorCount) Do
        CondFactor[i].GetType;
      Result := fstBoolean
    End
  Else
    Result := CondFactor[0].GetType;
End;
{--------}

Function TfsSqlCondTerm.GetValue(aArrayIndex: Integer): Variant;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  If CondFactorCount > 1 Then
    Result := AsBoolean
  Else
    Result := CondFactor[0].GetValue(aArrayIndex);
End;
{--------}

Function TfsSqlCondTerm.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

{!!.11 new}

Procedure TfsSqlCondTerm.MatchType(ExpectedType: TfsFieldType);
Var
  i: Integer;
  T: TfsFieldType;
Begin
  If CondFactorCount > 1 Then
    Begin
      If ExpectedType <> fstBoolean Then
        TypeMismatch;
      {force necessary Type conversion at lower level}
      T := CondFactor[0].GetType;
      For i := 1 To CondFactorCount - 1 Do
        CondFactor[i].MatchType(T);
    End
  Else
    CondFactor[0].MatchType(ExpectedType);
End;

Function TfsSqlCondTerm.Reduce: Boolean;
Var
  i, j: Integer;
  LiftFactor: TfsSqlCondFactor;
  LiftTerm: TfsSqlCondTerm;
  B: Boolean;
  S: String;
Begin
  {Look For conditional factors nested inside redundant parens}
    { - eliminate parens when found}
  {Look for BETWEEN expressions And convert them to two comparisons}
  Result := False;
  For i := 0 To pred(CondFactorCount) Do
    Begin
      //LiftFactor := nil;
      LiftTerm := Nil;
      With CondFactor[i] Do
        Begin
          If (CondPrimary.RelOp = roNone) Then
            If CondPrimary.BetweenClause <> Nil Then
              Begin
                If Not CondPrimary.BetweenClause.Negated Xor UnaryNot Then
                  Begin
                    {create a new CondPrimary to hold the >= comparison}
                    LiftFactor := TfsSqlCondFactor.Create(Self);
                    LiftFactor.CondPrimary := TfsSqlCondPrimary.Create(LiftFactor);
                    LiftFactor.CondPrimary.RelOp := roGE;
                    LiftFactor.CondPrimary.SimpleExp1 :=
                      TfsSqlSimpleExpression.Create(LiftFactor.CondPrimary);
                    LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
                    LiftFactor.CondPrimary.SimpleExp2 :=
                      TfsSqlSimpleExpression.Create(LiftFactor.CondPrimary);
                    LiftFactor.CondPrimary.SimpleExp2.Assign(
                      CondPrimary.BetweenClause.SimpleLow);
                    InsertCondFactor(i, LiftFactor);
                    {convert current CondPrimary to a >= comparison}
                    CondPrimary.RelOp := roLE;
                    CondPrimary.SimpleExp2 := TfsSqlSimpleExpression.Create(CondPrimary);
                    CondPrimary.SimpleExp2.Assign(CondPrimary.BetweenClause.SimpleHigh);
                    CondPrimary.BetweenClause.Free;
                    CondPrimary.BetweenClause := Nil;
                    Result := True;
                    UnaryNot := False;
                    break;
                  End;
              End
            Else If CondPrimary.LikeClause <> Nil Then
              Begin
                If Not CondPrimary.LikeClause.Negated Xor UnaryNot Then
                  Begin
                    If CondPrimary.LikeClause.CanLimit Then
                      Begin
                        {create a new CondPrimary to hold the >= comparison}
                        LiftFactor := TfsSqlCondFactor.Create(Self);
                        LiftFactor.CondPrimary := TfsSqlCondPrimary.Create(LiftFactor);
                        LiftFactor.CondPrimary.RelOp := roGE;
                        LiftFactor.CondPrimary.SimpleExp1 := TfsSqlSimpleExpression.Create(LiftFactor.CondPrimary);
                        LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
                        S := CondPrimary.LikeClause.GetLowLimit;
                        LiftFactor.CondPrimary.SimpleExp2 := fsCreateLiteralStringExp(LiftFactor, S);
                        InsertCondFactor(i, LiftFactor);
                        {create a new CondPrimary to hold the <= comparison}
                        LiftFactor := TfsSqlCondFactor.Create(Self);
                        LiftFactor.CondPrimary := TfsSqlCondPrimary.Create(LiftFactor);
                        LiftFactor.CondPrimary.RelOp := roL;
                        LiftFactor.CondPrimary.SimpleExp1 := TfsSqlSimpleExpression.Create(LiftFactor.CondPrimary);
                        LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
                        S := CondPrimary.LikeClause.GetHighLimit;
                        LiftFactor.CondPrimary.SimpleExp2 := fsCreateLiteralStringExp(LiftFactor, S);
                        InsertCondFactor(i, LiftFactor);
                        If CondPrimary.LikeClause.CanReplaceWithCompare Then
                          Begin
                            {we no longer need the LIKE clause}
                            CondFactor[i + 2].Free;
                            CondFactorList.Delete(i + 2); // adjust For the two we just inserted
                          End
                        Else
                          CondPrimary.LikeClause.Limited := True;
                        Result := True;
                        break;
                      End;
                  End;
              End
            Else If CondPrimary.InClause <> Nil Then
            Else If CondPrimary.IsTest <> Nil Then
            Else If CondPrimary.ExistsClause <> Nil Then
            Else If CondPrimary.UniqueClause <> Nil Then
            Else If CondPrimary.MatchClause <> Nil Then
            Else If CondPrimary.SimpleExp1 <> Nil Then
              With CondPrimary.SimpleExp1 Do
                If TermCount = 1 Then
                  Begin
                    With Term[0] Do
                      If FactorCount = 1 Then
                        With Factor[0] Do
                          If CondExp <> Nil Then
                            With CondExp Do
                              If CondTermCount = 1 Then
                                LiftTerm := CondTerm[0];
                  End;
          If LiftTerm <> Nil Then
            Begin
              //first lift all but the very first conditional factor to this level
              For j := 1 To pred(LiftTerm.CondFactorCount) Do
                Self.AddCondFactor(TfsSqlCondFactor.Create(Self)).
                  Assign(LiftTerm.CondFactor[j]);
              //then copy the contents of the first conditional factOr
              //  (possibly the only one) into this one
              B := UnaryNot; // save UnaryNot setting
              LiftFactor := TfsSqlCondFactor.Create(Self);
              LiftFactor.Assign(LiftTerm.CondFactor[0]);
              Clear;
              Assign(LiftFactor);
              LiftFactor.Free;
              UnaryNot := UnaryNot Xor B;
              Result := True;
              {Get out. We may have more to do here, but Global Logic will
              call us again, And there may be other transformations that can
              be applied first.}
              break;
            End;
          If Reduce Then
            Begin
              {factor itself was reduced}
              Result := True;
              break;
            End;
        End;
    End;
  {!!.11 begin}
  If Not Result Then
    For i := 0 To pred(CondFactorCount) Do
      If CondFactor[i].Reduce Then
        Begin
          Result := True;
          break;
        End;
  {!!.11 end}
End;

Procedure TfsSqlCondTerm.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

Procedure TfsSqlCondTerm.SetCondFactor(Index: Integer;
  Const Value: TfsSqlCondFactor);
Begin
  CondFactorList[Index] := Value;
End;
{====================================================================}

{===TfsSqlGroupColumnList=================================================}

Function TfsSqlGroupColumnList.AddColumn(Column: TfsSqlGroupColumn):
  TfsSqlGroupColumn;
Begin
  ColumnList.Add(Column);
  Result := Column;
End;
{--------}

Procedure TfsSqlGroupColumnList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlGroupColumnList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlGroupColumnList(Source).ColumnCount) Do
        AddColumn(TfsSqlGroupColumn.Create(Self)).Assign(
          TfsSqlGroupColumnList(Source).Column[i]);
    End
  Else
    AssignError(Source);
End;
{--------}

Function TfsSqlGroupColumnList.Contains(Const AColName: String;
  Se: TfsSqlSelection): Boolean;
{Rewritten !!.06}
Var
  i: Integer;
  aGrpColText,
    aSelText: String;
Begin
  If Assigned(Se.SimpleExpression.Term[0].Factor[0].FieldRef) Then
    aSelText := Trim(Se.SimpleExpression.Term[0].Factor[0].FieldRef.QualName)
  Else
    aSelText := Trim(Se.SQLText);

  For i := 0 To pred(ColumnCount) Do
    Begin
      aGrpColText := Trim(Column[i].QualColumnName);
      Result := (AnsiCompareText(aColName, aGrpColText) = 0) Or
        (AnsiCompareText(aSelText, aGrpColText) = 0);
      If Result Then
        Exit;
    End; { For }
  Result := False;
End;
{--------}

Constructor TfsSqlGroupColumnList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  ColumnList := TList.Create;
  UGroup := False;
End;
{--------}

Procedure TfsSqlGroupColumnList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(ColumnCount) Do
    Column[i].Free;
  ColumnList.Clear;
End;
{--------}

Destructor TfsSqlGroupColumnList.Destroy;
Begin
  Clear;
  ColumnList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlGroupColumnList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  Column[0].EmitSQL(Stream);
  For i := 1 To pred(ColumnCount) Do
    Begin
      WriteStr(Stream, ', ');
      Column[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlGroupColumnList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(ColumnCount) Do
    Column[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlGroupColumnList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlGroupColumnList Then
    Begin
      If ColumnCount <> TfsSqlGroupColumnList(Other).ColumnCount Then
        Exit;
      For i := 0 To pred(ColumnCount) Do
        If Not Column[i].Equals(TfsSqlGroupColumnList(Other).Column[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlGroupColumnList.GetColumn(Index: Integer): TfsSqlGroupColumn;
Begin
  Result := TfsSqlGroupColumn(ColumnList[Index]);
End;
{--------}

Function TfsSqlGroupColumnList.GetColumnCount: Integer;
Begin
  Result := ColumnList.Count;
End;
{--------}

Function TfsSqlGroupColumnList.Reduce: Boolean;
Begin
  Result := False;
End;

Procedure TfsSqlGroupColumnList.SetColumn(Index: Integer;
  Const Value: TfsSqlGroupColumn);
Begin
  ColumnList[Index] := Value;
End;
{====================================================================}

{===TfsSqlTableRefList===============================================}

Function TfsSqlTableRefList.AddTableRef(
  NewTableRef: TfsSqlTableRef): TfsSqlTableRef;
Begin
  FTableRefList.Add(NewTableRef);
  Result := NewTableRef;
End;
{--------}

Procedure TfsSqlTableRefList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlTableRefList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlTableRefList(Source).TableRefCount) Do
        AddTableRef(TfsSqlTableRef.Create(Self)).Assign(
          TfsSqlTableRefList(Source).TableRef[i]);
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlTableRefList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FTableRefList := TList.Create;
End;
{--------}

Function TfsSqlTableRefList.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Var
  i: Integer;
Begin
  Result := Nil;
  For i := 0 To pred(TableRefCount) Do
    Begin
      Result := TableRef[i].BindFieldDown(TableName, FieldName);
      If Result <> Nil Then
        Exit;
    End;
End;

Function TfsSqlTableRefList.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Var
  i: Integer;
Begin
  Result := Nil;
  For i := 0 To pred(TableRefCount) Do
    Begin
      Result := TableRef[i].BindTable(AOwner, TableName);
      If Result <> Nil Then
        Exit;
    End;
End;

Procedure TfsSqlTableRefList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(TableRefCount) Do
    TableRef[i].Free;
  FTableRefList.Clear;
  Inherited;
End;
{--------}

Destructor TfsSqlTableRefList.Destroy;
Begin
  Clear;
  FTableRefList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlTableRefList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  If TableRefCount > 0 Then
    Begin
      TableRef[0].EmitSQL(Stream);
      For i := 1 To pred(TableRefCount) Do
        Begin
          WriteStr(Stream, ' ,');
          TableRef[i].EmitSQL(Stream);
        End;
    End;
End;
{--------}

Procedure TfsSqlTableRefList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(TableRefCount) Do
    TableRef[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlTableRefList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlTableRefList Then
    Begin
      If TableRefCount <> TfsSqlTableRefList(Other).TableRefCount Then
        Exit;
      For i := 0 To pred(TableRefCount) Do
        If Not TableRef[i].Equals(TfsSqlTableRefList(Other).TableRef[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}
{!!.11 new}

Function TfsSqlTableRefList.GetFieldsFromTable(Const TableName: String;
  List: TList): TFSSqlTableProxy;
{-returns fields from table that are ultimately coming from the table
specified in the TableName argument. NIL if not found.}
Var
  i, j: Integer;
Begin
  Result := Nil; {!!.11}
  For i := 0 To TableRefCount - 1 Do
    If SameText(TableRef[i].Alias, TableName)
      Or SameText(TableRef[i].TableName, TableName) Then
      Begin
        Result := TableRef[i].ResultTable;
        For j := 0 To Result.FieldCount - 1 Do
          List.Add(Result.Field(j));
        Exit;
      End;
  {still here, which means that If there's a match, it's in a nested table}
  For i := 0 To TableRefCount - 1 Do
    Begin
      If TableRef[i].TableExp <> Nil Then {!!.11}
        Result := TableRef[i].TableExp.GetFieldsFromTable(TableName, List);
      If Result <> Nil Then
        Exit;
    End;
End;
{--------}

Function TfsSqlTableRefList.GetNameForAlias(Const Alias: String): String;
Var
  Inx: Integer;
Begin
  Result := '';
  For Inx := 0 To Pred(FTableRefList.Count) Do
    Begin
      If TfsSqlTableRef(FTableRefList[Inx]).Alias = Alias Then
        Begin
          Result := TfsSqlTableRef(FTableRefList[Inx]).TableName;
          Break;
        End;
    End;
End;
{--------}

Function TfsSqlTableRefList.GetIndexName(Const AliasOrTableName: String): String;
Var
  Inx: Integer;
Begin
  Result := '';
  For Inx := 0 To Pred(FTableRefList.Count) Do
    Begin
      If TfsSqlTableRef(FTableRefList[Inx]).TableName = AliasOrTableName Then
        Begin
          Result := TfsSqlTableRef(FTableRefList[Inx]).IndexName;
          Break;
        End;
    End;
  For Inx := 0 To Pred(FTableRefList.Count) Do
    Begin
      If TfsSqlTableRef(FTableRefList[Inx]).Alias = AliasOrTableName Then
        Begin
          Result := TfsSqlTableRef(FTableRefList[Inx]).IndexName;
          Break;
        End;
    End;
End;

Function TfsSqlTableRefList.GetTableRef(
  Index: Integer): TfsSqlTableRef;
Begin
  Result := TfsSqlTableRef(FTableRefList[Index]);
End;

Function TfsSqlTableRefList.GetItem(Index: Integer): TfsSqlTableRef;
Begin
  Result := TfsSqlTableRef(FTableRefList[Index]);
End;
{--------}

Function TfsSqlTableRefList.GetTableRefCount: Integer;
Begin
  Result := FTableRefList.Count;
End;
{--------}
{!!.11 rewritten}

Function TfsSqlTableRefList.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To TableRefCount - 1 Do
    If TableRef[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

Procedure TfsSqlTableRefList.SetTableRef(Index: Integer;
  Const Value: TfsSqlTableRef);
Begin
  FTableRefList[Index] := Value;
End;
{====================================================================}

{===TfsSqlStatement==================================================}

Procedure TfsSqlStatement.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
  TableExp: TfsSqlTableExp;
Begin
  If Source Is TfsSqlStatement Then
    Begin
      Clear;
      If TfsSqlStatement(Source).Insert <> Nil Then
        Begin
          Insert := TfsSqlINSERT.Create(Self);
          Insert.Assign(TfsSqlStatement(Source).Insert);
          Insert.FClientName := TfsSqlStatement(Source).Insert.FClientName;
          Insert.CommitBy := TfsSqlStatement(Source).Insert.CommitBy;
          Insert.LimitCount := TfsSqlStatement(Source).Insert.LimitCount;
          Insert.LimitDivBy := TfsSqlStatement(Source).Insert.LimitDivBy;
          Insert.LimitPrct := TfsSqlStatement(Source).Insert.LimitPrct;
          Insert.LimitPrctDiv := TfsSqlStatement(Source).Insert.LimitPrctDiv;
          Insert.LimitDivAtOne := TfsSqlStatement(Source).Insert.LimitDivAtOne;
          Insert.LimitPrctStart := TfsSqlStatement(Source).Insert.LimitPrctStart;
          Insert.LimitStart := TfsSqlStatement(Source).Insert.LimitStart;
          Insert.LimitDistinct := TfsSqlStatement(Source).Insert.LimitDistinct;
          Insert.LimitFirst := TfsSqlStatement(Source).Insert.LimitFirst;
          Insert.Islimit := TfsSqlStatement(Source).Insert.Islimit;
        End;
      If TfsSqlStatement(Source).Update <> Nil Then
        Begin
          Update := TfsSqlUPDATE.Create(Self);
          Update.Assign(TfsSqlStatement(Source).Update);
          Update.FClientName := TfsSqlStatement(Source).Update.FClientName;
          Update.CommitBy := TfsSqlStatement(Source).Update.CommitBy;
          Update.LimitCount := TfsSqlStatement(Source).Update.LimitCount;
          Update.LimitDivBy := TfsSqlStatement(Source).Update.LimitDivBy;
          Update.LimitPrct := TfsSqlStatement(Source).Update.LimitPrct;
          Update.LimitPrctDiv := TfsSqlStatement(Source).Update.LimitPrctDiv;
          Update.LimitDivAtOne := TfsSqlStatement(Source).Update.LimitDivAtOne;
          Update.LimitPrctStart := TfsSqlStatement(Source).Update.LimitPrctStart;
          Update.LimitStart := TfsSqlStatement(Source).Update.LimitStart;
          Update.LimitDistinct := TfsSqlStatement(Source).Update.LimitDistinct;
          Update.LimitFirst := TfsSqlStatement(Source).Update.LimitFirst;
          Update.Islimit := TfsSqlStatement(Source).Update.Islimit;
        End;
      If TfsSqlStatement(Source).Delete <> Nil Then
        Begin
          Delete := TfsSqlDELETE.Create(Self);
          Delete.Assign(TfsSqlStatement(Source).Delete);
          Delete.CommitBy := TfsSqlStatement(Source).Delete.CommitBy;
          Delete.LimitCount := TfsSqlStatement(Source).Delete.LimitCount;
          Delete.LimitDivBy := TfsSqlStatement(Source).Delete.LimitDivBy;
          Delete.LimitPrct := TfsSqlStatement(Source).Delete.LimitPrct;
          Delete.LimitPrctDiv := TfsSqlStatement(Source).Delete.LimitPrctDiv;
          Delete.LimitDivAtOne := TfsSqlStatement(Source).Delete.LimitDivAtOne;
          Delete.LimitPrctStart := TfsSqlStatement(Source).Delete.LimitPrctStart;
          Delete.LimitStart := TfsSqlStatement(Source).Delete.LimitStart;
          Delete.LimitDistinct := TfsSqlStatement(Source).Delete.LimitDistinct;
          Delete.LimitFirst := TfsSqlStatement(Source).Delete.LimitFirst;
          Delete.Islimit := TfsSqlStatement(Source).Delete.Islimit;
        End;
      If TfsSqlStatement(Source).TableExpList.Count > 0 Then
        Begin
          TableExpList.Clear;
          For i := 0 To TfsSqlStatement(Source).TableExpList.Count - 1 Do
            Begin
              TableExp := TfsSqlTableExp.Create(Self);
              TableExp.Assign(TfsSqlStatement(Source).TableExp(i));
              AddTableExp(TableExp);
            End;
        End;
      Reduce := TfsSqlStatement(Source).Reduce;
      UseIndex := TfsSqlStatement(Source).UseIndex;
    End
  Else
    AssignError(Source);
End;
{Begin !!.11}
{--------}

Procedure TfsSqlStatement.Bind(Const ClientID: TffClientID;
  Const SessionID: TffSessionID;
  Database: TfsSqlDatabaseProxy);
Var
  i: Integer;
Begin
  FClientID := ClientID;
  FSessionID := SessionID;
  FDatabase := Database;
  FClientName := '';
  If Database.Engine.ClientList.ClientCount > 0 Then
    Begin
      For i := 0 To Database.Engine.ClientList.ClientCount - 1 Do
        Begin
          If Database.Engine.ClientList.Client[ftFromIndex, i].ClientId = ClientId Then
            Begin
              FClientName := Database.Engine.ClientList.Client[ftFromIndex, i].ClientName;
              break;
            End;
        End;
    End;

  If assigned(Insert) Then
    Begin
      Insert.Bind;
      Insert.FClientName := FClientName;
    End
  Else If assigned(Update) Then
    Begin
      Update.Bind;
      Update.FClientName := FClientName;
    End
  Else If assigned(Delete) Then
    Delete.Bind;
End;
{--------}
{End !!.11}

Procedure TfsSqlStatement.Clear;
Var
  i: Integer;
  t: TfsSqlTableExp;
Begin
  While fTableExpList.Count > 0 Do
    Begin
      t := TfsSqlTableExp(fTableExpList[0]);
      t.free;
      t := Nil;
      fTableExpList.Delete(0);
    End;
  Insert.Free;
  Insert := Nil;
  Update.Free;
  Update := Nil;
  Delete.Free;
  Delete := Nil;
  fTableExpList.free;
  fTableExpList := Nil;
  //TableExp.Free;
  //TableExp := Nil;
End;

Procedure TfsSqlStatement.AddTableExp(Value: TfsSqlTableExp);
Begin
  If Value <> Nil Then
    fTableExpList.Add(Value);
End;

Function TfsSqlStatement.TableExp(Index: Integer): TfsSqlTableExp;
Begin
  Result := Nil;
  If (fTableExpList.Count > 0) And (Index >= 0)
    And (Index <= fTableExpList.Count - 1) Then
    Result := TfsSqlTableExp(fTableExpList[Index]);
End;

Procedure TfsSqlStatement.ClearTableExp;
Var
  i: Integer;
Begin
  While fTableExpList.Count > 0 Do
    fTableExpList.Delete(0);
End;
{--------}

Constructor TfsSqlStatement.Create;
Begin
  Inherited Create(Nil);
  fTableExpList := TList.Create;
  {$IFDEF ExposeLastStatement}
  LastStatement := Self; {debug hook}
  {$ENDIF}
End;
{--------}

Destructor TfsSqlStatement.Destroy;
Begin
  ParmList.Free;
  Clear;
  Inherited;
  {$IFDEF ExposeLastStatement}
  LastStatement := Nil; {debug hook}
  {$ENDIF}
End;
{--------}

Procedure TfsSqlStatement.EmitSQL(Stream: TStream);
Begin
  If Not UseIndex Then
    WriteStr(Stream, 'NOINDEX ');
  If Not Reduce Then
    WriteStr(Stream, 'NOREDUCE ');
  If assigned(Insert) Then
    Insert.EmitSQL(Stream);
  If assigned(Update) Then
    Update.EmitSQL(Stream);
  If assigned(Delete) Then
    Delete.EmitSQL(Stream);
  If assigned(TableExp(0)) Then
    TableExp(0).EmitSQL(Stream);
  WriteStr(Stream, ';');
  WriteEOF(Stream);
End;
{--------}

Procedure TfsSqlStatement.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(Insert) Then
    Insert.EnumNodes(EnumMethod, Deep);
  If assigned(Update) Then
    Update.EnumNodes(EnumMethod, Deep);
  If assigned(Delete) Then
    Delete.EnumNodes(EnumMethod, Deep);
  If assigned(TableExp(0)) Then
    TableExp(0).EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlStatement.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlStatement)
    And ((BothNil(Insert, TfsSqlStatement(Other).Insert)
    Or (BothNonNil(Insert, TfsSqlStatement(Other).Insert)
    And Insert.Equals(TfsSqlStatement(Other).Insert))))
    And ((BothNil(Update, TfsSqlStatement(Other).Update)
    Or (BothNonNil(Update, TfsSqlStatement(Other).Update)
    And Update.Equals(TfsSqlStatement(Other).Update))))
    And ((BothNil(Delete, TfsSqlStatement(Other).Delete)
    Or (BothNonNil(Delete, TfsSqlStatement(Other).Delete)
    And Delete.Equals(TfsSqlStatement(Other).Delete))))
    And ((BothNil(TableExp(0), TfsSqlStatement(Other).TableExp(0))
    Or (BothNonNil(TableExp(0), TfsSqlStatement(Other).TableExp(0))
    And TableExp(0).Equals(TfsSqlStatement(Other).TableExp(0)))));
End;
{--------}

Function TfsSqlStatement.Execute(Var ALiveResult: Boolean;
  Var aCursorID: TffCursorID;
  Var RowsAffected,
  aRecordsRead: Integer): TffResult;
Var
  i: Integer;
  tmpCursorID1, tmpCursorID2: TffCursorID;
  tmpRecordsRead: Longint;
Begin
  Result := DBIERR_NONE; {!!.11}
  StartDate := Date;
  StartTime := Time;
  StartDateTime := Now;
  aCursorID := 0;
  RecordsRead := 0;
  If TableExpList.Count > 0 Then
    Begin
      // for unionlist
      tmpRecordsRead := 0;
      tmpCursorID1 := 0;
      i := 0;
      TableExp(i).Execute(aLiveResult, aCursorID, RecordsRead);
      tmpCursorID1 := aCursorID;
      aRecordsRead := RecordsRead;
      inc(i);
      While i <= TableExpList.Count - 1 Do
        Begin
          TableExp(i).Execute(aLiveResult, tmpCursorID2, RecordsRead);
          TableExp(i).Union2Table(TableExp(i - 1).Union, TableExp(i - 1).UnionCase, tmpCursorID1, aLiveResult, aCursorID, RecordsRead);
          aRecordsRead := RecordsRead;
          tmpCursorID1 := aCursorID;
          inc(i);
        End;
    End
      {Begin !!.11}
  Else If assigned(Insert) Then
    Result := Insert.Execute(RowsAffected)
  Else If assigned(Update) Then
    Result := Update.Execute(RowsAffected)
  Else If assigned(Delete) Then
    Result := Delete.Execute(RowsAffected)
  Else
    Raise Exception.Create('Statement is empty');
  {End !!.11}
  aRecordsRead := RecordsRead;
End;
{-------}

Procedure TfsSqlStatement.ReduceStrength;
Begin
  {$IFDEF LogTransformations}
  AssignFile(TRLog, TRLogFile);
  {$I-}
  Append(TRLog);
  If IOResult <> 0 Then
    Rewrite(TRLog);
  writeln(TRLog);
  writeln(TRLog, 'Transforming ' + SQLText);
  writeln(TRLog, 'started at :', DateTimeToStr(Now));
  {$ENDIF}

  If TableExpList.Count > 0 Then
    Begin
      While TableExp(0).Reduce Do
        Begin
          {$IFDEF LogTransformations}
          writeln(TRLog, 'new form:' + SQLText);
          {$ENDIF}
        End;
    End
  Else If assigned(Insert) Then
    Begin
      While Insert.Reduce Do
        Begin
          {$IFDEF LogTransformations}
          writeln(TRLog, 'new form:' + SQLText);
          {$ENDIF}
        End;
    End
  Else If assigned(Update) Then
    Begin
      While Update.Reduce Do
        Begin
          {$IFDEF LogTransformations}
          writeln(TRLog, 'new form:' + SQLText);
          {$ENDIF}
        End;
    End
  Else If assigned(Delete) Then
    Begin
      While Delete.Reduce Do
        Begin
          {$IFDEF LogTransformations}
          writeln(TRLog, 'new form:' + SQLText);
          {$ENDIF}
        End;
    End;
  {!!.11 end}

  {$IFDEF LogTransformations}
  writeln(TRLog);
  writeln(TRLog, 'ended at :', DateTimeToStr(Now));
  CloseFile(TRLog);
  {$ENDIF}
End;

Procedure TfsSqlStatement.SetParameter(Index: Integer; Value: Variant);
Begin
  If ParmCount = 0 Then
    Raise Exception.Create('Error: Attempt to Set parameter on non-parameterized query');
  If ParmList = Nil Then
    ParmList := TFSVariantList.Create(ParmCount);
  ParmList.SetValue(Index, Value);
End;

{====================================================================}

{===TfsSqlSelect=====================================================}
{--------}

Procedure TfsSqlSELECT.AddTableRefs(Node: TfsSqlNode);
Begin
  Node.AddTableReference(Self);
End;
{--------}

Procedure TfsSqlSELECT.AddColumns(Node: TfsSqlNode);
Begin
  Node.AddColumnDef(Self);
End;

Procedure TfsSqlSELECT.AddDistinctColumns(Node: TfsSqlNode);
Begin
  Node.AddDistinctColumnDef(Self);
End;
{--------}

Procedure TfsSqlSELECT.ClearBindings(Node: TfsSqlNode);
Begin
  Node.ClearBinding;
End;
{--------}

Function TfsSqlSELECT.Reduce: Boolean;
Begin
  If SelectionList <> Nil Then
    Result := SelectionList.Reduce
  Else
    Result := False;
  Result := Result Or TableRefList.Reduce;
  If CondExpWhere <> Nil Then
    Result := Result Or CondExpWhere.Reduce;
  If GroupColumnList <> Nil Then
    Result := Result Or GroupColumnList.Reduce;
  If UnionGroupColumnList <> Nil Then
    Result := Result Or UnionGroupColumnList.Reduce;
  If CondExpHaving <> Nil Then
    Result := Result Or CondExpHaving.Reduce;
  If OrderList <> Nil Then
    Result := Result Or OrderList.Reduce;
  If UnionOrderList <> Nil Then
    Result := Result Or UnionOrderList.Reduce;
End;
{--------}

Procedure TfsSqlSELECT.ResetIsConstant(Node: TfsSqlNode);
Begin
  Node.ResetConstant;
End;
{--------}

Procedure TfsSqlSELECT.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, 'SELECT');
  If Distinct Then
    Begin
      WriteStr(Stream, ' DISTINCT');
      If Not Self.DistinctCase Then
        WriteStr(Stream, ' CASE FALSE');
    End
  Else
    WriteStr(Stream, ' ALL');
  If (SelectionList = Nil) Or WasStar Then
    WriteStr(Stream, ' *')
  Else
    SelectionList.EmitSQL(Stream);
  WriteStr(Stream, ' FROM');
  TableRefList.EmitSQL(Stream);
  If CondExpWhere <> Nil Then
    Begin
      WriteStr(Stream, ' WHERE');
      CondExpWhere.EmitSQL(Stream);
    End;
  If GroupColumnList <> Nil Then
    Begin
      WriteStr(Stream, ' GROUP BY');
      GroupColumnList.EmitSQL(Stream);
    End;
  If UnionGroupColumnList <> Nil Then
    Begin
      WriteStr(Stream, ' GROUPUNION BY');
      UnionGroupColumnList.EmitSQL(Stream);
    End;
  If CondExpHaving <> Nil Then
    Begin
      WriteStr(Stream, ' HAVING');
      CondExpHaving.EmitSQL(Stream);
    End;
  If OrderList <> Nil Then
    OrderList.EmitSQL(Stream);
  If UnionOrderList <> Nil Then
    UnionOrderList.EmitSQL(Stream);
End;
{--------}

Procedure TfsSqlSELECT.AddTableFields(Table: TFSSqlTableProxy;
  Const StartPoint: Integer;
  FieldRef: TfsSqlFieldRef);
Var
  Factor: TfsSqlFactor;
  j: Integer;
  Selection: TfsSqlSelection;
  StartVal: Integer;
  Term: TfsSqlTerm;
Begin
  Assert(Table <> Nil);
  Assert(Table Is TFSSqlTableProxy);
  If Table.FieldCount > 0 Then
    Begin
      StartVal := Pred(Table.FieldCount);
      If FieldRef <> Nil Then
        Begin
          FieldRef.WasWildcard := True;
          FieldRef.FieldName := Table.Field(StartVal).Name;
          If Table.Alias <> '' Then
            FieldRef.TableName := Table.Alias
          Else
            FieldRef.TableName := Table.Name;
          dec(StartVal);
        End;
      For j := StartVal Downto 0 Do
        Begin
          Selection := TfsSqlSelection.Create(SelectionList);
          Selection.SimpleExpression :=
            TfsSqlSimpleExpression.Create(Selection);
          Term := TfsSqlTerm.Create(Selection.SimpleExpression);
          Factor := TfsSqlFactor.Create(Term);
          Factor.FieldRef := TfsSqlFieldRef.Create(Factor);
          If Table.Alias <> '' Then
            Factor.FieldRef.TableName := Table.Alias
          Else
            Factor.FieldRef.TableName := Table.Name;
          Factor.FieldRef.FieldName := Table.Field(j).Name;
          Term.AddFactor(Factor);
          Selection.AddedByWildcard := True;
          Selection.SimpleExpression.AddTerm(Term);
          SelectionList.InsertSelection(StartPoint, Selection);
        End;
    End;
End;
{--------}

Procedure TfsSqlSELECT.AddTableFieldsFromList(Table: TFSSqlTableProxy;
  Const StartPoint: Integer;
  FieldRef: TfsSqlFieldRef;
  List: TList);
Var
  Factor: TfsSqlFactor;
  j: Integer;
  Selection: TfsSqlSelection;
  StartVal: Integer;
  Term: TfsSqlTerm;
Begin
  Assert(Table <> Nil);
  Assert(Table Is TFSSqlTableProxy);
  If Table.FieldCount > 0 Then
    Begin
      StartVal := Pred(List.Count);
      If FieldRef <> Nil Then
        Begin
          FieldRef.WasWildcard := True;
          FieldRef.FieldName := TfsSqlFieldProxy(List[StartVal]).Name;
          dec(StartVal);
        End;
      For j := StartVal Downto 0 Do
        Begin
          Selection := TfsSqlSelection.Create(SelectionList);
          Selection.SimpleExpression :=
            TfsSqlSimpleExpression.Create(Selection);
          Term := TfsSqlTerm.Create(Selection.SimpleExpression);
          Factor := TfsSqlFactor.Create(Term);
          Factor.FieldRef := TfsSqlFieldRef.Create(Factor);
          If TfsSqlFieldProxy(List[j]).OwnerTable.Alias <> '' Then
            Factor.FieldRef.TableName :=
              TfsSqlFieldProxy(List[j]).OwnerTable.Alias
          Else
            Factor.FieldRef.TableName :=
              TfsSqlFieldProxy(List[j]).OwnerTable.Name;
          Factor.FieldRef.FieldName := TfsSqlFieldProxy(List[j]).Name;
          Term.AddFactor(Factor);
          Selection.AddedByWildcard := True;
          Selection.SimpleExpression.AddTerm(Term);
          SelectionList.InsertSelection(StartPoint, Selection);
        End;
    End;
End;
{--------}

Procedure TfsSqlSELECT.ExpandWildcards;
Var
  i, j, ix: Integer;
  T: TFSSqlTableProxy;
  Simp: TfsSqlSimpleExpression;
  FR: TfsSqlFieldRef;
  List: TList; {!!.11}
Begin
  FIsSingleTable := True;

  Assert(SelectionList <> Nil);
  If SelectionList.SelectionCount <> 1 Then
    FIsSingleTable := False;

  {If SelectionList = Nil Then
    Begin
      // If the selectionlist is empty then only a wildcard was specified.
      //  Note that With the fix Of issue 481, this is dead code.
      WasStar := True;
      SelectionList := TfsSqlSelectionList.Create( Self );
      Assert( Assigned( TablesReferencedByOrder ) );
      For i := Pred( TablesReferencedByOrder.Count ) Downto 0 Do
        Begin
          T := TFSSqlTableProxy( TablesReferencedByOrder.Objects[i] );
          AddTableFields( T, 0, Nil );
        End;
    End
  Else
    Begin }
  For i := pred(SelectionList.SelectionCount) Downto 0 Do
    Begin
      Simp := SelectionList.Selection[i].SimpleExpression;
      If Simp <> Nil Then
        Begin
          FR := Simp.Term[0].Factor[0].FieldRef;
          If FR <> Nil Then
            Begin
              If FR.FieldName = '' Then
                Begin
                  Assert(Assigned(TablesReferencedByOrder));
                  { If no table name specified then add fields from all tables
                    referenced in the FROM clause. }
                  If FR.TableName = '' Then
                    Begin
                      Assert(Assigned(TablesReferencedByOrder));
                      For j := pred(TablesReferencedByOrder.Count) Downto 0 Do
                        Begin
                          T := TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]);
                          If j = 0 Then
                            AddTableFields(T, i, FR)
                          Else
                            Begin
                              AddTableFields(T, i, Nil);
                              FIsSingleTable := False;
                            End;
                        End;
                    End
                  Else
                    Begin
                      { Otherwise the wildcard was qualified With a tablename. }
                      ix := TablesReferencedByOrder.IndexOf(FR.TableName);
                      If ix = -1 Then
                        Begin
                          Assert(Assigned(TableAliases));
                          With TableAliases Do
                            Begin
                              ix := IndexOf(FR.TableName);
                              If ix <> -1 Then
                                ix := Integer(Objects[ix])
                              Else
                                Begin
                                  {!!.11 begin}
                                  {might be part Of a nested table expression}
                                  List := TList.Create;
                                  Try
                                    T := TableRefList.GetFieldsFromTable(FR.TableName, List);
                                    If T <> Nil Then
                                      Begin
                                        AddTableFieldsFromList(T, i, FR, List);
                                        ix := -1;
                                      End
                                    Else
                                      {!!.11 end}
                                      SQLError('Unknown table: ' + FR.TableName);
                                  Finally {!!.11}
                                    List.Free; {!!.11}
                                  End; {!!.11}
                                End;
                            End;
                        End;
                      If ix <> -1 Then
                        Begin {!!.11}
                          T := TFSSqlTableProxy(TablesReferencedByOrder.Objects[ix]);
                          AddTableFields(T, i, FR);
                        End; {!!.11}
                      //If TablesReferencedByOrder.Count <> 1 Then
                       // FIsSingleTable := False;
                    End;
                End
              Else
                FIsSingleTable := False;
            End
          Else
            FIsSingleTable := False;
        End
      Else
        FIsSingleTable := False;
    End;

End;
{--------}

Procedure TfsSqlSELECT.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  If Deep Then
    Begin
      EnumMethod(Self);
      If SelectionList <> Nil Then
        SelectionList.EnumNodes(EnumMethod, Deep);
      TableRefList.EnumNodes(EnumMethod, Deep);
      If CondExpWhere <> Nil Then
        CondExpWhere.EnumNodes(EnumMethod, Deep);
      If GroupColumnList <> Nil Then
        GroupColumnList.EnumNodes(EnumMethod, Deep);
      If UnionGroupColumnList <> Nil Then
        UnionGroupColumnList.EnumNodes(EnumMethod, Deep);
      If CondExpHaving <> Nil Then
        CondExpHaving.EnumNodes(EnumMethod, Deep);
      If OrderList <> Nil Then
        OrderList.EnumNodes(EnumMethod, Deep);
      If UnionOrderList <> Nil Then
        UnionOrderList.EnumNodes(EnumMethod, Deep);
    End;
End;
{--------}

Procedure TfsSqlSELECT.ClearTableList;
Var
  i: Integer;
Begin
  If TablesReferencedByOrder <> Nil Then
    Begin
      For i := 0 To pred(TablesReferencedByOrder.Count) Do
        If assigned(TablesReferencedByOrder.Objects[i]) Then
          If TFSSqlTableProxy(TablesReferencedByOrder.Objects[i]).Owner = Self Then
            Begin
              TFSSqlTableProxy(TablesReferencedByOrder.Objects[i]).Owner := Nil;
              TObject(TablesReferencedByOrder.Objects[i]).Free;
            End;
      TablesReferencedByOrder.Clear;
    End;
  If TableAliases <> Nil Then
    TableAliases.Clear;
  Bound := False;
End;
{--------}

Procedure TfsSqlSELECT.Bind;
Var
  i, j: Integer;
  T: TFSSqlTableProxy;
  Alias: String; {!!.11}
Begin
  If CondExpWhere <> Nil Then
    CondExpWhere.EnumNodes(ClearBindings, False);
  If CondExpHaving <> Nil Then
    CondExpHaving.EnumNodes(ClearBindings, False);
  ClearTableList;
  TableRefList.EnumNodes(AddTableRefs, False);
  Assert(Assigned(TablesReferencedByOrder));
  For i := 0 To pred(TablesReferencedByOrder.Count) Do
    Begin
      Assert(TablesReferencedByOrder[i] <> '');
      If pos('$$UNNAMED', TablesReferencedByOrder[i]) <> 0 Then
        Assert(TablesReferencedByOrder.Objects[i] <> Nil)
      Else
        Begin
          j := TableAliases.IndexOfObject(TObject(i));
          If j = -1 Then
            Alias := ''
          Else
            Alias := TableAliases[j];
          assert(Owner.FDatabase <> Nil);
          assert(TObject(Owner.FDatabase) Is TfsSqlDatabaseProxy);
          T := Owner.FDatabase.TableByName(Self, TablesReferencedByOrder[i],
            omReadOnly, False, Alias, TableRefList.GetIndexName(TablesReferencedByOrder[i]));
          If T = Nil Then
            SQLError('Unable to open table: ' + TablesReferencedByOrder[i] +
              '. Ensure the table exists And is not in use by ' +
              'another process.');
          TablesReferencedByOrder.Objects[i] := T;
        End;
    End;
  ExpandWildcards;

  If CondExpWhere <> Nil Then
    CondExpWhere.MatchType(fstBoolean);

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  SelectionList.EnumNodes(AddColumns, False);
  Assert(Assigned(DistinctColumns));
  DistinctColumns.Clear;
  If Assigned(DistinctList) Then
    DistinctList.EnumNodes(AddDistinctColumns, False);

  {figure out If we're using aggregates}
  {if we are, we need to prepare For those}
  HaveAggregates := False;

  SelectionList.EnumNodes(FlagAggregates, False);
  If Assigned(DistinctList) Then
    DistinctList.EnumNodes(FlagAggregates, False);

  If Distinct Then
    Begin
      {ensure that all fields have a Type we can compare}
      Assert(Assigned(Columns));
      Assert(Assigned(DistinctColumns));
      If Assigned(DistinctList) Then
        Begin
          If DistinctList.SelectionCount > 0 Then
            Begin
              For i := 0 To pred(DistinctColumns.Count) Do
                Begin
                  Case TfsSqlNode(DistinctColumns.Objects[i]).GetType Of
                    fstBoolean..fstDateTime: ;
                    fstBcd, fstRecVersion, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: ;
                    fstShortString..fstWideString: ;
                    Else
                      SQLError('Field ' + DistinctColumns[i] + ' has a type, which is incompatible With DISTINCT');
                  End;
                End;
            End;
        End
      Else
        Begin
          For i := 0 To pred(Columns.Count) Do
            Begin
              Case TfsSqlNode(Columns.Objects[i]).GetType Of
                fstBoolean..fstDateTime: ;
                fstBcd, fstRecVersion, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: ;
                fstShortString..fstWideString: ;
                Else
                  SQLError('Field ' + Columns[i] + ' has a type, which is incompatible With DISTINCT');
              End;
            End;
        End;
    End;
  Bound := True;
End;
{--------}

Function TfsSqlSELECT.BindField(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Var
  T: TFSSqlTableProxy;
  j: Integer;
Begin
  Result := Nil;
  If TableName <> '' Then
    Begin
      Assert(Assigned(TablesReferencedByOrder));
      j := TablesReferencedByOrder.IndexOf(TableName);
      If (j = -1)
        {can't refer to aliased table With its actual name}{!!.12}
      Or (TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]).Alias <> '') {!!.12} Then
        Begin
          //may be an alias
          Assert(Assigned(TableAliases));
          With TableAliases Do
            Begin
              j := IndexOf(TableName);
              If j <> -1 Then
                Begin
                  j := Integer(Objects[j]);
                  T := TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]);
                  If T = Nil Then {!!.11}
                    SQLError('Invalid field reference:' + TableName + '.' + FieldName); {!!.11}
                End
              Else
                Begin
                  //may be a field from an exclosed expression
                  If BindingDown Then {!!.11}
                    Result := Nil {!!.11}
                  Else
                    Try {!!.11}
                      BindingDown := True; {!!.11}
                      Result := TableRefList.BindFieldDown(TableName, FieldName); {!!.11}
                    Finally {!!.11}
                      BindingDown := False; {!!.11}
                    End; {!!.11}
                  If Result = Nil Then
                    If IsSubQuery Then
                      Begin
                        {may be field at outer level}
                        Result := Parent.BindField(TableName, FieldName);
                        IsDependent := True;
                        Exit;
                      End;
                  {Else
                  Result := TableRefList.BindFieldDown(TableName, FieldName);}{!!.11}
                  If Result = Nil Then
                    SQLError('Unknown field:' + TableName + '.' + FieldName);
                  Exit;
                End;
            End;
        End
      Else
        Begin
          T := TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]);
          Assert(T <> Nil, 'Table not resolved:'
            + TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]).Name); {!!.11}
        End;
      Assert(T <> Nil);
      Result := T.FieldByName(FieldName);
      If Result = Nil Then
        SQLError('Unknown field:' + TableName + '.' + FieldName);
    End
  Else
    Begin
      Assert(Assigned(TablesReferencedByOrder));
      For j := 0 To pred(TablesReferencedByOrder.Count) Do
        Begin
          T := TFSSqlTableProxy(TablesReferencedByOrder.Objects[j]);
          Assert(T <> Nil);
          Assert(T Is TFSSqlTableProxy);
          If T.FieldByName(FieldName) <> Nil Then
            Begin
              Result := T.FieldByName(FieldName);
              Exit;
            End;
        End;
      { No binding found yet. See if this is an alias For a field in the
        result table. }
      If Joiner <> Nil Then
        For j := 0 To Pred(Joiner.FT.Count) Do
          Begin
            If AnsiCompareText(TfsSqlFieldProxy(Joiner.FT[j]).Name, FieldName) = 0 Then
              Begin
                Result := Joiner.FT[j];
                Exit;
              End;
          End;
      SQLError('Unknown field:' + FieldName);
    End;
End;

Function TfsSqlSELECT.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  Result := TableRefList.BindTable(AOwner, TableName);
End;

{--------}

Function TfsSqlSELECT.FindField(Const FieldName: String): TfsSqlFieldProxy;
Var
  P: Integer;
Begin
  P := fsPosCh('.', FieldName);
  If P = 0 Then
    Result := BindField('', FieldName)
  Else
    Result := BindField(copy(FieldName, 1, P - 1), copy(FieldName, P + 1, MaxInt));
End;
{--------}

Procedure TfsSqlSELECT.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlSELECT Then
    Begin
      Clear;
      Distinct := TfsSqlSELECT(Source).Distinct;
      DistinctCase := TfsSqlSELECT(Source).DistinctCase;
      GroupCase := TfsSqlSELECT(Source).GroupCase;
      GroupUnionCase := TfsSqlSELECT(Source).GroupUnionCase;
      DistinctOnlyList := TfsSqlSELECT(Source).DistinctOnlyList;
      If TfsSqlSELECT(Source).SelectionList <> Nil Then
        Begin
          SelectionList := TfsSqlSelectionList.Create(Self);
          SelectionList.Assign(TfsSqlSELECT(Source).SelectionList);
        End;
      If TfsSqlSELECT(Source).DistinctList <> Nil Then
        Begin
          DistinctList := TfsSqlSelectionList.Create(Self);
          DistinctList.Assign(TfsSqlSELECT(Source).DistinctList);
        End;
      TableRefList := TfsSqlTableRefList.Create(Self);
      TableRefList.Assign(TfsSqlSELECT(Source).TableRefList);
      If TfsSqlSELECT(Source).CondExpWhere <> Nil Then
        Begin
          CondExpWhere := TfsSqlCondExp.Create(Self);
          CondExpWhere.Assign(TfsSqlSELECT(Source).CondExpWhere);
        End;
      If TfsSqlSELECT(Source).GroupColumnList <> Nil Then
        Begin
          GroupColumnList := TfsSqlGroupColumnList.Create(Self);
          GroupColumnList.Assign(TfsSqlSELECT(Source).GroupColumnList);
        End;
      If TfsSqlSELECT(Source).UnionGroupColumnList <> Nil Then
        Begin
          UnionGroupColumnList := TfsSqlGroupColumnList.Create(Self);
          UnionGroupColumnList.Assign(TfsSqlSELECT(Source).UnionGroupColumnList);
        End;
      If TfsSqlSELECT(Source).CondExpHaving <> Nil Then
        Begin
          CondExpHaving := TfsSqlCondExp.Create(Self);
          CondExpHaving.Assign(TfsSqlSELECT(Source).CondExpHaving);
        End;
      If TfsSqlSELECT(Source).OrderList <> Nil Then
        Begin
          OrderList := TfsSqlOrderList.Create(Self);
          OrderList.Assign(TfsSqlSELECT(Source).OrderList);
        End;
      If TfsSqlSELECT(Source).UnionOrderList <> Nil Then
        Begin
          UnionOrderList := TfsSqlOrderList.Create(Self);
          UnionOrderList.Assign(TfsSqlSELECT(Source).UnionOrderList);
        End;
    End
  Else
    AssignError(Source);
End;
{--------}

Constructor TfsSqlSELECT.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  TablesReferencedByOrder := TStringList.Create;
  TableAliases := TStringList.Create;
  TableAliases.Sorted := True;
  TableAliases.Duplicates := dupError;
  AggQueryMode := aqmIdle;
  fIsSystemTables := False;
  fIsOnline := False;
  UnionOrderList := Nil;
  UnionGroupColumnList := Nil;
End;
{--------}

Procedure TfsSqlSELECT.Clear;
Begin
  ClearTableList;

  FSelectionList.Free;
  FSelectionList := Nil;

  FTableRefList.Free;
  FTableRefList := Nil;

  FCondExpWhere.Free;
  FCondExpWhere := Nil;

  FGroupColumnList.Free;
  FGroupColumnList := Nil;

  If Assigned(FUnionGroupColumnList) Then
    FUnionGroupColumnList.Free;
  FUnionGroupColumnList := Nil;

  FCondExpHaving.Free;
  FCondExpHaving := Nil;

  FOrderList.Free;
  FOrderList := Nil;
  If Assigned(FUnionOrderList) Then
    FUnionOrderList.Free;
  FUnionOrderList := Nil;
End;
{--------}

Function TfsSqlSELECT.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If Not Bound Then
    Bind;
  Result :=
    ((CondExpWhere <> Nil) And CondExpWhere.DependsOn(Table))
    Or ((CondExpHaving <> Nil) And CondExpHaving.DependsOn(Table));

End;
{--------}

Destructor TfsSqlSELECT.Destroy;
Begin
  If FResultTable <> Nil Then
    Begin
      FResultTable.Owner := Nil;
      FResultTable.Free;
    End;
  Clear;
  TableAliases.Free;
  TablesReferencedByOrder.Free;
  Joiner.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlSELECT.FlagAggregates(Node: TfsSqlNode);
Begin
  Node.FlagAggregate(Self);
End;
{--------}

Procedure TfsSqlSELECT.EnumAggregates(Node: TfsSqlNode);
Begin
  Node.AddAggregate(AggList);
End;
{--------}

Function TfsSqlSELECT.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Var
  i: Integer;
Begin
  For i := 0 To pred(Columns.Count) Do
    If Columns.Objects[i] = F Then
      Begin
        Result := ResultTable.Field(i);
        Exit;
      End;
  Result := Nil;
End;

Procedure TfsSqlSELECT.EnsureResultTable(NeedData: Boolean);
Begin
  Assert(TObject(Self) Is TfsSqlSELECT);
  If IsDependent Or (NeedData And Not HaveData) Then
    Begin
      If FResultTable <> Nil Then
        Begin
          Assert(TObject(FResultTable) Is TFSSqlTableProxy);
          Assert(FResultTable.Owner = Self);
          FResultTable.Owner := Nil;
          FResultTable.Free;
          FResultTable := Nil;
        End;
    End;
  If FResultTable = Nil Then
    Begin
      FResultTable := Execute2(NeedData);
      HaveData := NeedData;
    End;
End;

Function TfsSqlSELECT.CheckForValue(Value: Variant): Boolean;
Begin
  ////
  EnsureResultTable(True);
  If VarIsNull(Value) Then
    Result := False
  Else
    Begin
      ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
      Result := ResultTable.First;
    End;
End;

////
{$IFDEF IsNoVariantInt64}

Function TfsSqlSELECT.CheckAllValues(RelOp: TfsSqlRelOp;
  Const Val: Variant): Boolean;
Var
  TestVal: Variant;
  EVal, ETestVal: Extended;
  IVal, ITestVal: Int64;
Begin
  EnsureResultTable(True);
  Result := False;
  If VarIsNull(Val) Then
    Exit;
  If ResultTable.First Then
    Begin
      Repeat
        // value field
        TestVal := ResultTable.Field(0).GetValue(-1);
        If VarIsNull(TestVal) Then
          Exit;
        Case RelOp Of
          roEQ:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If ETestVal <> EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If ETestVal <> IVal Then Exit;
                        End;
                      Else
                        If ETestVal <> Val Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If ITestVal <> EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If ITestVal <> IVal Then Exit;
                        End;
                      Else
                        If ETestVal <> Val Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If TestVal <> EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If TestVal <> EVal Then Exit;
                        End;
                      Else
                        If TestVal <> Val Then Exit;
                    End;
                  End;
              End;
            End;
          roLE:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal > ETestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal > ETestVal Then Exit;
                        End;
                      Else
                        If Val > ETestVal Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal > ITestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal > ITestVal Then Exit;
                        End;
                      Else
                        If Val > ETestVal Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal > TestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If EVal > TestVal Then Exit;
                        End;
                      Else
                        If EVal > TestVal Then Exit;
                    End;
                  End;
              End;
            End;
          roL:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal >= ETestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal >= ETestVal Then Exit;
                        End;
                      Else
                        If Val >= ETestVal Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal >= ITestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal >= ITestVal Then Exit;
                        End;
                      Else
                        If Val >= ETestVal Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal >= TestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If EVal >= TestVal Then Exit;
                        End;
                      Else
                        If EVal >= TestVal Then Exit;
                    End;
                  End;
              End;
            End;
          roG:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal <= ETestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal <= ETestVal Then Exit;
                        End;
                      Else
                        If Val <= ETestVal Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal <= ITestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal <= ITestVal Then Exit;
                        End;
                      Else
                        If Val <= ETestVal Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal <= TestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If EVal <= TestVal Then Exit;
                        End;
                      Else
                        If EVal <= TestVal Then Exit;
                    End;
                  End;
              End;
            End;
          roGE:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal < ETestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal < ETestVal Then Exit;
                        End;
                      Else
                        If Val < ETestVal Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal < ITestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If IVal < ITestVal Then Exit;
                        End;
                      Else
                        If Val < ETestVal Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If EVal < TestVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If EVal < TestVal Then Exit;
                        End;
                      Else
                        If EVal < TestVal Then Exit;
                    End;
                  End;
              End;
            End;
          roNE:
            Begin
              Case TVarData(TestVal).Vtype Of
                vt_e80:
                  Begin
                    ETestVal := decimal(TestVal).ext80;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If ETestVal = EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If ETestVal = IVal Then Exit;
                        End;
                      Else
                        If ETestVal = Val Then Exit;
                    End;
                  End;
                vt_decimal:
                  Begin
                    ITestVal := decimal(TestVal).lo64;
                    // Not compactible int64
                    ETestVal := decimal(TestVal).lo64;
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If ITestVal = EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          IVal := decimal(Val).lo64;
                          If ITestVal = IVal Then Exit;
                        End;
                      Else
                        If ETestVal = Val Then Exit;
                    End;
                  End;
                Else
                  Begin
                    // TestVal jest ró¿ne od Extended i int64
                    // ale Val mo¿e by int64 lub Extended
                    Case TVarData(Val).Vtype Of
                      vt_e80:
                        Begin
                          EVal := decimal(Val).ext80;
                          If TestVal = EVal Then Exit;
                        End;
                      vt_decimal:
                        Begin
                          //IVal := decimal( Val ).lo64;
                          EVal := decimal(Val).lo64;
                          If TestVal = EVal Then Exit;
                        End;
                      Else
                        If TestVal = Val Then Exit;
                    End;
                  End;
              End;
            End;
        End;
      Until Not ResultTable.Next;
      Result := True;
    End;
End;

// obs³uga int64

Function TfsSqlSELECT.CheckAnyValue(RelOp: TfsSqlRelOp;
  Const Val: Variant): Boolean;
Var
  TestVal: Variant;
  EVal, ETestVal: Extended;
  IVal, ITestVal: Int64;
Begin
  EnsureResultTable(True);
  Result := True;
  If ResultTable.First Then
    Repeat
      TestVal := ResultTable.Field(0).GetValue(-1);
      Case RelOp Of
        roEQ:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If ETestVal = EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If ETestVal = IVal Then Exit;
                      End;
                    Else
                      If ETestVal = Val Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If ITestVal = EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If ITestVal = IVal Then Exit;
                      End;
                    Else
                      If ETestVal = Val Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If TestVal = EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If TestVal = EVal Then Exit;
                      End;
                    Else
                      If TestVal = Val Then Exit;
                  End;
                End;
            End;
          End;
        roLE:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal <= ETestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal <= ETestVal Then Exit;
                      End;
                    Else
                      If Val <= ETestVal Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal <= ITestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal <= ITestVal Then Exit;
                      End;
                    Else
                      If Val <= ETestVal Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal <= TestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If EVal <= TestVal Then Exit;
                      End;
                    Else
                      If EVal <= TestVal Then Exit;
                  End;
                End;
            End;
          End;
        roL:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal < ETestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal < ETestVal Then Exit;
                      End;
                    Else
                      If Val < ETestVal Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal < ITestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal < ITestVal Then Exit;
                      End;
                    Else
                      If Val < ETestVal Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal < TestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If EVal < TestVal Then Exit;
                      End;
                    Else
                      If EVal < TestVal Then Exit;
                  End;
                End;
            End;
          End;
        roG:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal > ETestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal > ETestVal Then Exit;
                      End;
                    Else
                      If Val > ETestVal Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal > ITestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal > ITestVal Then Exit;
                      End;
                    Else
                      If Val > ETestVal Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal > TestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If EVal > TestVal Then Exit;
                      End;
                    Else
                      If EVal > TestVal Then Exit;
                  End;
                End;
            End;
          End;
        roGE:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal >= ETestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal >= ETestVal Then Exit;
                      End;
                    Else
                      If Val >= ETestVal Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal >= ITestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If IVal >= ITestVal Then Exit;
                      End;
                    Else
                      If Val >= ETestVal Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If EVal >= TestVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If EVal >= TestVal Then Exit;
                      End;
                    Else
                      If EVal >= TestVal Then Exit;
                  End;
                End;
            End;
          End;
        roNE:
          Begin
            Case TVarData(TestVal).Vtype Of
              vt_e80:
                Begin
                  ETestVal := decimal(TestVal).ext80;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If ETestVal <> EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If ETestVal <> IVal Then Exit;
                      End;
                    Else
                      If ETestVal <> Val Then Exit;
                  End;
                End;
              vt_decimal:
                Begin
                  ITestVal := decimal(TestVal).lo64;
                  // Not compactible int64
                  ETestVal := decimal(TestVal).lo64;
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If ITestVal <> EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        IVal := decimal(Val).lo64;
                        If ITestVal <> IVal Then Exit;
                      End;
                    Else
                      If ETestVal <> Val Then Exit;
                  End;
                End;
              Else
                Begin
                  // TestVal jest ró¿ne od Extended i int64
                  // ale Val mo¿e by int64 lub Extended
                  Case TVarData(Val).Vtype Of
                    vt_e80:
                      Begin
                        EVal := decimal(Val).ext80;
                        If TestVal <> EVal Then Exit;
                      End;
                    vt_decimal:
                      Begin
                        //IVal := decimal( Val ).lo64;
                        EVal := decimal(Val).lo64;
                        If TestVal <> EVal Then Exit;
                      End;
                    Else
                      If TestVal <> Val Then Exit;
                  End;
                End;
            End;
          End;
      End;
    Until Not ResultTable.Next;
  Result := False;
End;
{$ELSE}

Function TfsSqlSELECT.CheckAllValues(RelOp: TfsSqlRelOp;
  Const Val: Variant): Boolean;
Var
  TestVal: Variant;
Begin
  EnsureResultTable(True);
  Result := False;
  If VarIsNull(Val) Then Exit;
  If ResultTable.First Then
    Begin
      Repeat
        TestVal := ResultTable.Field(0).GetValue(-1);
        If VarIsNull(TestVal) Then Exit;
        Case RelOp Of
          roEQ:
            If TestVal <> Val Then
              Exit;
          roLE:
            If Val > TestVal Then
              Exit;
          roL:
            If Val >= TestVal Then
              Exit;
          roG:
            If Val <= TestVal Then
              Exit;
          roGE:
            If Val < TestVal Then
              Exit;
          roNE:
            If TestVal = Val Then
              Exit;
        End;
      Until Not ResultTable.Next;
      Result := True;
    End;
End;

Function TfsSqlSELECT.CheckAnyValue(RelOp: TfsSqlRelOp;
  Const Val: Variant): Boolean;
Begin
  EnsureResultTable(True);
  Result := True;
  If ResultTable.First Then
    Repeat
      Case RelOp Of
        roEQ:
          If ResultTable.Field(0).GetValue(-1) = Val Then
            Exit;
        roLE:
          If Val <= ResultTable.Field(0).GetValue(-1) Then
            Exit;
        roL:
          If Val < ResultTable.Field(0).GetValue(-1) Then
            Exit;
        roG:
          If Val > ResultTable.Field(0).GetValue(-1) Then
            Exit;
        roGE:
          If Val >= ResultTable.Field(0).GetValue(-1) Then
            Exit;
        roNE:
          If ResultTable.Field(0).GetValue(-1) <> Val Then
            Exit;
      End;
    Until Not ResultTable.Next;
  Result := False;
End;
{$ENDIF}

Function TfsSqlSELECT.CheckNonEmpty: Boolean;
Begin
  EnsureResultTable(True);
  Result := FResultTable.First;
End;

Function TfsSqlSELECT.GetDecimals: Integer;
Begin
  If Not TypeKnown Then
    Begin
      EnsureResultTable(False);
      FDecimals := FResultTable.Field(0).GetDecimals;
      FType := FResultTable.Field(0).GetType;
      FSize := FResultTable.Field(0).GetSize; {!!.13}
      fBlobLevelComp := FResultTable.Field(0).GetBlobLevel; {!!.13}
      FRoundType := FResultTable.Field(0).GetRound;
      TypeKnown := True;
    End;
  Result := FDecimals;
End;

Function TfsSqlSELECT.GetBlobLevel: TDataCompLevel;
Begin
  If Not TypeKnown Then
    Begin
      EnsureResultTable(False);
      FDecimals := FResultTable.Field(0).GetDecimals;
      FType := FResultTable.Field(0).GetType;
      FSize := FResultTable.Field(0).GetSize; {!!.13}
      fBlobLevelComp := FResultTable.Field(0).GetBlobLevel; {!!.13}
      FRoundType := FResultTable.Field(0).GetRound;
      TypeKnown := True;
    End;
  Result := fBlobLevelComp;
End;

{!!.13 new}

Function TfsSqlSELECT.GetSize: Integer;
Begin
  If Not TypeKnown Then
    Begin
      EnsureResultTable(False);
      FDecimals := FResultTable.Field(0).GetDecimals;
      FType := FResultTable.Field(0).GetType;
      FSize := FResultTable.Field(0).GetSize;
      fBlobLevelComp := FResultTable.Field(0).GetBlobLevel; {!!.13}
      FRoundType := FResultTable.Field(0).GetRound;
      TypeKnown := True;
    End;
  Result := FSize;
End;

Function TfsSqlSELECT.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    Begin
      EnsureResultTable(False);
      FDecimals := FResultTable.Field(0).GetDecimals;
      FType := FResultTable.Field(0).GetType;
      FSize := FResultTable.Field(0).GetSize; {!!.13}
      fBlobLevelComp := FResultTable.Field(0).GetBlobLevel; {!!.13}
      FRoundType := FResultTable.Field(0).GetRound;
      TypeKnown := True;
    End;
  Result := FType;
End;

Function TfsSqlSELECT.GetRound: TRound;
Begin
  If Not TypeKnown Then
    Begin
      EnsureResultTable(False);
      FDecimals := FResultTable.Field(0).GetDecimals;
      FType := FResultTable.Field(0).GetType;
      FSize := FResultTable.Field(0).GetSize; {!!.13}
      fBlobLevelComp := FResultTable.Field(0).GetBlobLevel; {!!.13}
      FRoundType := FResultTable.Field(0).GetRound;
      TypeKnown := True;
    End;
  Result := FRoundType;
End;

Function TfsSqlSELECT.GetValue(aArrayIndex: Integer): Variant;
Begin
  EnsureResultTable(True);
  If ResultTable.First Then
    Result := ResultTable.Field(0).GetValue(aArrayIndex)
  Else
    Result := Null;
End;

Procedure TfsSqlSELECT.BuildSortList(Table: TFSSqlTableProxy; Var SortList: TfsSqlSortArray; aOrderList: TfsSqlOrderList);
{-logic extracted from DoOrderBy}
Var
  i, z, k: Integer;
  IX: Integer;
  s: String;
  FR: TfsSqlFieldRef;
  AliasName: String;
Begin
  For i := 0 To pred(aOrderList.OrderCount) Do
    Begin
      If aOrderList.OrderItem[i].Column <> Nil Then
        Begin
          s := aOrderList.OrderItem[i].Column.QualColumnName;
          Assert(Assigned(Columns));
          z := Columns.IndexOf(S);
          If z = -1 Then
            Begin
              z := fsPosCh('.', S);
              If z = 0 Then
                Begin
                  S := '.' + S;
                  // may be unqualified field but qualified columns
                  z := -1;
                  For k := 0 To pred(Columns.Count) Do
                    If fsPosI(S, Columns[k]) <> 0 Then
                      Begin
                        z := k;
                        break;
                      End;
                  If z = -1 Then
                    Begin
                      SQLError('Unknown column specified in ORDER BY clause: ' +
                        Copy(S, 2, Length(S) - 1));
                    End;
                End
              Else
                Begin
                  // Try to find qualified column
                  z := -1;
                  {S := Uppercase(S);}{!!.10}
                  Assert(Assigned(Columns));
                  For k := 0 To pred(Columns.Count) Do
                    Begin
                      FR := (Columns.Objects[k] As TfsSqlSimpleExpression).Term[0].Factor[0].FieldRef;
                      If Assigned(FR) And
                        SameText(S, Trim(FR.SQLText)) Then
                        Begin
                          z := k;
                          break;
                        End;
                    End;
                  If z = -1 Then
                    Begin
                      //Table might be aliased. Replace alias With corresponding name.
                      z := fsPosCh('.', S);
                      AliasName := UpperCase(Copy(s, 1, z - 1));

                      Assert(Assigned(TableAliases));
                      IX := TableAliases.IndexOf(AliasName);
                      If IX <> -1 Then
                        Begin
                          IX := Integer(TableAliases.Objects[IX]);
                          Assert(Assigned(TablesReferencedByOrder));
                          S := TablesReferencedByOrder[IX] + '.' +
                            UpperCase(Copy(S, Z + 1, MaxInt));

                          //Repeat search For field
                          z := -1;
                          Assert(Assigned(Columns));
                          For k := 0 To Pred(Columns.Count) Do
                            Begin
                              FR := (Columns.Objects[K] As TfsSqlSimpleExpression).Term[0].Factor[0].FieldRef;
                              If Assigned(FR) And
                                SameText(S, Trim(FR.SQLText)) Then
                                Begin
                                  z := k;
                                  break;
                                End;
                            End;
                        End
                      Else
                        z := -1;
                    End;

                  If z = -1 Then
                    Begin
                      // may be qualified field but unqualified columns
                      z := fsPosCh('.', S);
                      S := copy(S, z + 1, MaxInt);
                      z := -1;
                      Assert(Assigned(Columns));
                      For k := 0 To pred(Columns.Count) Do
                        If fsPosI(S, Columns[k]) <> 0 Then
                          Begin
                            z := k;
                            break;
                          End;
                      If z = -1 Then
                        SQLError('Unknown column specified in ORDER BY clause:' + S);
                    End;
                End;
            End;

          Assert(Assigned(Columns));
          SortList[i] := Table.FieldByName(Columns[z]).Index + 1;
        End
      Else
        Begin
          z := StrToInt(aOrderList.OrderItem[i].Index);
          SortList[i] := Table.FieldByName(Columns[z - 1]).Index + 1;
        End;
      If aOrderList.OrderItem[i].Descending Then
        SortList[i] := -SortList[i];
    End;
End;

Procedure TfsSqlSELECT.DoOrderBy(aOrderList: TfsSqlOrderList; NeedData: Boolean; Table: TFSSqlTableProxy);
Var
  SortList: TfsSqlSortArray;
  SortNoCase: TfsSqlSortArray;
  SortSize: TfsSqlSortArray;
  SortNull: TfsSqlSortArray;
  Status: TffResult;
  i: Integer;
Begin
  If (aOrderList <> Nil) And NeedData Then
    Begin
      BuildSortList(Table, SortList, aOrderList);
      For i := 0 To aOrderList.OrderCount - 1 Do
        Begin
          SortNoCase[i] := Byte(aOrderList.OrderItem[i].NoCase);
          SortSize[i] := aOrderList.OrderItem[i].Size;
          SortNull[i] := Byte(aOrderList.OrderItem[i].NullTop);
        End;
      Status := Table.OrderSort(aOrderList.OrderCount, SortList, SortNoCase, SortSize, SortNull); {!!.13}
      If Status <> DBIERR_NONE Then
        Raise EfsException.CreateNoData(fsStrResServer, Status);
    End;
End;

Procedure TfsSqlSELECT.DoTopDown(Var Table: TFSSqlTableProxy; Var L: Boolean);
Var
  T2: TFSSqlTableProxy;
  RCount, LimitCountT, LimitCountC, LimitCountD: Int64;
  rct: Boolean;
Begin
  rct := False;
  If LimitCount < -1 Then LimitCount := -2; // no limit
  //(-2 internal no limit, -1 all but copy for other function, 0 empty, > 1 count limit)
  L := False;
  If TopDirection <> tdNone Then
    If (LimitCount >= -1) Then
      Begin
        RCount := 0;
        If LimitCount > -1 Then
          Begin
            If LimitPrct Then
              If (LimitCount > 0) And (LimitCount < 100) Then
                Begin
                  RCount := Table.GetRecordCount;
                  rct := True;
                  If LimitCount > 0 Then
                    LimitCount := LimitCount * RCount Div 100;
                End
              Else If (LimitCount >= 100) Then
                LimitCount := -1;
          End;

        If LimitStart <= 0 Then LimitStart := 0;
        If LimitPrctStart Then
          If (LimitStart > 0) And (LimitStart < 100) Then
            Begin
              If Not rct Then
                Begin
                  RCount := Table.GetRecordCount;
                  rct := True;
                End;
              LimitStart := LimitStart * RCount Div 100;
              If LimitStart <= 0 Then LimitStart := 0;
            End
          Else If (LimitStart >= 100) Then
            LimitStart := 0;

        If Self.LimitDivBy <= 0 Then Self.LimitDivBy := 0;
        If LimitPrctDiv Then
          If (LimitDivBy > 1) And (LimitDivBy < 100) Then
            Begin
              If Not rct Then
                Begin
                  RCount := Table.GetRecordCount;
                  rct := True;
                End;
              LimitDivBy := LimitDivBy * RCount Div 100;
              If LimitDivBy <= 0 Then LimitDivBy := 0;
            End
          Else If (LimitDivBy >= 100) Then
            LimitDivBy := 0;

        LimitCountT := LimitCount;
        LimitCountD := LimitCount;
        LimitCountC := LimitCount;

        Case TopDirection Of
          tdTopCenterDown:
            Begin
              If LimitCount = -1 Then
                TopDirection := tdTop
              Else
                Begin
                  If Not rct Then
                    RCount := Table.GetRecordCount;
                  If ((LimitCount * 3) >= RCount) Or ((LimitCount * 2) >= RCount)
                    Or (LimitCount >= RCount) Then
                    Begin
                      TopDirection := tdTop;
                      LimitCountT := -1;
                    End;
                End;
            End;
          tdCenter:
            Begin
              If LimitCount = -1 Then
                TopDirection := tdTop
              Else
                Begin
                  If Not rct Then
                    RCount := Table.GetRecordCount;
                  If (LimitCountC >= RCount) Then
                    Begin
                      TopDirection := tdTop;
                      LimitCountT := -1;
                    End;
                End;
            End;
          tdTopDown:
            Begin
              If LimitCount = -1 Then
                TopDirection := tdTop
              Else
                Begin
                  If Not rct Then
                    RCount := Table.GetRecordCount;
                  If ((LimitCount * 2) >= RCount) Or (LimitCount >= RCount) Then
                    Begin
                      TopDirection := tdTop;
                      LimitCountT := -1;
                    End;
                End;
            End;
          tdTop: ;
          tdDown:
            Begin
              If Not rct Then
                RCount := Table.GetRecordCount;
              If (LimitCountD >= RCount) Then
                Begin
                  TopDirection := tdTop;
                  LimitCountT := -1;
                End;
            End;
        End;

        T2 := Table.CopyLimit(Self, RCount, LimitCountT, LimitCountC, LimitCountD, LimitStart,
          TopDirection, LimitDivBy, LimitDivAtOne);
        Table.Owner := Nil;
        Table.Free;
        Table := T2;
        L := True;

        If LimitDistinct And (Table <> Nil) Then
          Begin
            T2 := Table.CopyUnique(Self, True);
            Table.Owner := Nil;
            Table.Free;
            Table := T2;
          End;
      End;
End;

Function TfsSqlSELECT.NormalQueryResult(NeedData: Boolean): TFSSqlTableProxy;
Var
  N: TfsSqlNode;
  T2: TFSSqlTableProxy;
  F: TfsSqlFieldProxy;
  FieldDefList: TfsSqlFieldDefList;
  Td, IsProc: Boolean;

  Procedure sDistinct;
  Var
    j, k: Integer;
    Field: TFSSqlFieldProxy;
    fn, fn1: String;
  Begin
    If Distinct And NeedData Then
      Begin
        Result.ClearDistinct;
        Result.DistinctFieldName.Clear;
        If Self.fDistinctOnlyList Then
          For k := 0 To Self.DistinctList.SelectionCount - 1 Do
            Begin
              If Self.DistinctList.Selection[k].SimpleExpression.GetTerm(0).HasFieldRef Then
                Begin
                  fn1 := Self.DistinctList.Selection[k].SimpleExpression.GetTerm(0).GetFactor(0).FieldRef.Field.Name;
                  Result.DistinctFieldName.Add(fn1);
                End
              Else
                Begin
                  // self.DistinctList.Selection.
                End;
            End;
        T2 := Result.CopyUnique(Self, DistinctCase);
        Result.ClearDistinct;
        Result.DistinctFieldName.Clear;
        Result.Owner := Nil;
        Result.Free;
        Result := T2;
      End;
  End;

  Procedure sCopyData;
  Begin
    T2 := Result.CopyLimit(Self, 0, -1, 0, 0, 1, tdtop, 0, True);
    Result.Owner := Nil;
    Result.Free;
    Result := T2;
  End;

  Procedure NormalT;
  Var
    j: Integer;
  Begin
    {build a normal answer table}
    FieldDefList := TfsSqlFieldDefList.Create;
    Try
      Assert(Assigned(Columns));
      For j := 0 To pred(Columns.Count) Do
        Begin
          N := TfsSqlNode(Columns.Objects[j]);
          FieldDefList.AddField(Columns[j], '', N.GetType, N.GetSize, N.GetDecimals,
            N.GetBlobLevel, N.getRound);
        End;
      Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self,
        FieldDefList);
    Finally
      FieldDefList.Free;
    End;
  End;

  Procedure NormalT2;
  Var
    i: Integer;
  Begin
    If Joiner = Nil Then
      Begin
        Joiner := TfsSqlJoiner.Create(Owner, CondExpWhere);
        Assert(Assigned(TablesReferencedByOrder));
        For i := 0 To pred(TablesReferencedByOrder.Count) Do
          Joiner.Sources.Add(
            TFSSqlTableProxySubset.Create(
            TFSSqlTableProxy(TablesReferencedByOrder.Objects[i])));
      End;

    Joiner.ClearColumnList;

    Assert(Assigned(Columns));
    For i := 0 To pred(Columns.Count) Do
      Begin
        If TfsSqlSimpleExpression(Columns.Objects[i]).IsField(F) Then
          Begin
            Joiner.AddColumn(
              Nil,
              F,
              Result.Field(i));
          End
        Else
          Begin
            Joiner.AddColumn(
              TfsSqlSimpleExpression(Columns.Objects[i]),
              Nil,
              Result.Field(i));
          End;
      End;

    If NeedData Then
      Begin
        Joiner.Target := Result;
        Owner.FDatabase.StartTransaction([Nil]);
        Try
          Joiner.Execute(Owner.UseIndex, Nil, jmNone);
        Except
          Owner.FDatabase.AbortTransaction;
          Raise;
        End;
        Owner.FDatabase.Commit;
      End;

    For i := 0 To Result.FieldCount - 1 Do
      Result.Field(i).IsTarget := False;

    {At this point we have a table With all records that meet the
    WHERE criteria.}

    {if DISTINCT was specifed, we now need to remove any duplicates}
    If Distinct And NeedData Then
      sDistinct;
    If (Parent Is TfsSqlInClause) Or (Parent Is TfsSqlMatchClause) Then
      Begin
        {need an index to allow the IN And MATCH clauses to be evaluated}
        T2 := Result.CopySortedOnAllFields(Self);
        Result.Owner := Nil;
        Result.Free;
        Result := T2;
      End
    Else
      Begin
        If NeedData Then
          Begin
            If LimitFirst Then
              Begin
                DoTopDown(Result, td);
                DoOrderBy(OrderList, NeedData, Result);
              End
            Else
              Begin
                DoOrderBy(OrderList, NeedData, Result);
                DoTopDown(Result, td);
              End;
          End;
      End;
  End;

  Procedure ProcessOnline;
  Begin
    Result := Owner.FDatabase.TableByName(Self, TablesReferencedByOrder[0],
      omReadWrite, False, TablesReferencedByOrder[0], TableRefList.GetIndexName(TablesReferencedByOrder[0]));
    If Result = Nil Then
      SQLError('Unable to open table: ' + TablesReferencedByOrder[0] +
        '. Ensure the table exists And is not in use by ' +
        'another process or Index is wrong');
    Result.OnlineCursor := True;
    IsProc := Result.IsProcedure;

    If LimitFirst Then
      Begin
        td := False;
        If TopDirection <> tdNone Then
          DoTopDown(Result, td);
        Result.OnlineCursor := Not td;
        If Not Td And (OrderList <> Nil) Then
          Begin
            sCopyData;
            Result.OnlineCursor := False;
          End;
        If (OrderList <> Nil) Then
          Begin
            DoOrderBy(OrderList, NeedData, Result);
            Result.OnlineCursor := False;
          End;
      End
    Else
      Begin
        td := False;
        If (OrderList <> Nil) Then
          Begin
            sCopyData;
            DoOrderBy(OrderList, NeedData, Result);
            Result.OnlineCursor := False;
          End;
        If TopDirection <> tdNone Then
          DoTopDown(Result, td);
        If Result.OnlineCursor Then
          Result.OnlineCursor := Not td;
      End;
    If IsProc Then
      Result.OnlineCursor := False;
  End;
Begin
  //not Self.IsSystemTables
  If {IsOnline And }  FIsSingleTable And
  Assigned(TablesReferencedByOrder) And (TablesReferencedByOrder.Count = 1) And
    (TableRefList[0].TableExp = Nil) And
    (Not Assigned(GroupColumnList) Or (GroupColumnList.ColumnCount = 0)) And
    Not Assigned(CondExpHaving) And (Not assigned(CondExpWhere) Or (Parent Is TfsSqlInClause)
    Or (Parent Is TfsSqlMatchClause) Or Distinct) Then
    Begin
      Try
        If NeedData Then
          Begin
            If Not IsOnline Then
              Begin
                If (TopDirection = tdNone) And (OrderList = Nil) Then
                  Begin
                    NormalT;
                    NormalT2;
                    Result.OnlineCursor := False;
                  End
                Else
                  ProcessOnline;
              End
            Else
              ProcessOnline;
          End;
      Except
        If Assigned(Result) Then
          Begin
            Result.Owner := Nil;
            Result.Free;
          End;
        Raise;
      End;
    End
  Else
    Begin
      // normal table
      NormalT;
      Try
        NormalT2;
      Except
        If Assigned(Result) Then
          Begin
            Result.Owner := Nil;
            Result.Free;
          End;
        Raise;
      End;
    End;
End;

Function TfsSqlSELECT.CheckHaving: Boolean;
Begin
  Result := CondExpHaving.AsBoolean;
End;

Procedure TfsSqlSELECT.DoAggOrderBy;
{-utility method For AggregateQueryResult}
Var
  i, j, z, k, IX: Integer;
  S: String;
  FR: TfsSqlFieldRef;
  AliasName: String;
  SortList: TfsSqlSortArray;
  Status: TffResult;
Begin
  //do ORDER BY
  If OrderList <> Nil Then
    Begin

      j := pred(OrderList.OrderCount);
      For i := 0 To j Do
        Begin
          If OrderList.OrderItem[i].Column <> Nil Then
            Begin
              s := OrderList.OrderItem[i].Column.QualColumnName;
              z := Columns.IndexOf(S);
              If z = -1 Then
                Begin
                  z := fsPosCh('.', S);
                  If z = 0 Then
                    Begin
                      S := '.' + S;
                      // may be unqualified field but qualified columns
                      z := -1;
                      For k := 0 To pred(Columns.Count) Do
                        If fsPosI(S, Columns[k]) <> 0 Then
                          Begin
                            z := k;
                            break;
                          End;
                      If z = -1 Then
                        Begin
                          SQLError('Unknown column specified in ORDER BY clause: ' +
                            Copy(S, 2, Length(S) - 1));
                        End;
                    End
                  Else
                    Begin
                      // This is a qualified column. Try to find qualified column
                      z := -1;
                      For k := 0 To pred(Columns.Count) Do
                        Begin
                          FR := (Columns.Objects[k] As TfsSqlSimpleExpression).
                            Term[0].Factor[0].FieldRef;
                          If Assigned(FR) And (fsPosI(S, FR.SQLText) <> 0) Then
                            Begin
                              z := k;
                              break;
                            End;
                        End;
                      If z = -1 Then
                        Begin
                          //Table might be aliased. Replace alias With corresponding table name
                          z := fsPosCh('.', S);
                          AliasName := UpperCase(Copy(s, 1, z - 1));

                          Assert(Assigned(TableAliases));
                          IX := TableAliases.IndexOf(AliasName);
                          If IX <> -1 Then
                            Begin
                              IX := Integer(TableAliases.Objects[IX]);
                              Assert(Assigned(TablesReferencedByOrder));
                              S := TablesReferencedByOrder[IX] + '.' +
                                UpperCase(Copy(S, Z + 1, MaxInt));

                              //Repeat search For field
                              z := -1;
                              For k := 0 To Pred(Columns.Count) Do
                                Begin
                                  FR := (Columns.Objects[K] As TfsSqlSimpleExpression).Term[0].Factor[0].FieldRef;
                                  If Assigned(FR) And (fsPosI(S, FR.SQLText) <> 0) Then
                                    Begin
                                      z := k;
                                      break;
                                    End;
                                End;
                            End
                          Else
                            z := -1;
                        End;

                      If z = -1 Then
                        Begin
                          // may be qualified field but unqualified columns
                          Z := fsPosCh('.', S);
                          S := copy(S, z + 1, MaxInt);
                          Z := -1;
                          For k := 0 To pred(Columns.Count) Do
                            If fsPosI(S, Columns[k]) <> 0 Then
                              Begin
                                z := k;
                                break;
                              End;
                          If z = -1 Then
                            SQLError('Unknown column specified in ORDER BY clause:' + S);
                        End;
                    End;
                End;

              SortList[i] := FGrpTable.Field(z).Index + 1;
            End
          Else
            Begin
              z := StrToInt(OrderList.OrderItem[i].Index);
              SortList[i] := FGrpTable.Field(z - 1).Index + 1;
            End;
          If OrderList.OrderItem[i].Descending Then
            SortList[i] := -SortList[i];
        End;

      Status := FGrpTable.Sort(j + 1, SortList, False); {!!.13}
      If Status <> DBIERR_NONE Then
        Raise EfsException.CreateNoData(fsStrResServer, Status);
    End;
End;

Procedure TfsSqlSELECT.DoGroupCopy(aGroupColumnList: TfsSqlGroupColumnList; GroupColumnsIn: Integer; AggExpList,
  GroupColumnTargetField: TList; GroupCase: boolean);
Var
  GroupColumnsOut: Integer;
  FieldDefList: TfsSqlFieldDefList;
  i: Integer;
  N: TfsSqlNode;
  Se: TfsSqlSelection;
  T2: TFSSqlTableProxy;
  L, P: Variant;
  i64: Int64;
  e: Extended;
  EVal: Comp;

  Procedure CopyGrouped(Const Source, Target: TFSSqlTableProxy;
    GroupColumnsIn, GroupColumnsOut, NonGroupColumns: Integer;
    Const GroupColumnTargetField,
    AggExpList: TList);

  Var
    i: Integer;
    IsFirst, HaveGroup, NewGroup: Boolean;
    LastValues: TFSVariantList;

    Procedure WriteGroup;
    Var
      TgtInfo: TfsGroupColumnTargetInfo;
      i: Integer;
    Begin
      Target.Insert;
      For i := 0 To pred(GroupColumnsOut) Do
        Begin
          TgtInfo := TfsGroupColumnTargetInfo(GroupColumnTargetField[i]);
          If TgtInfo <> Nil Then
            Target.Field(TgtInfo.SelFldIndex).SetValue
              (LastValues.GetValue(TgtInfo.LastValueIndex), -1);
        End;
      For i := 0 To pred(NonGroupColumns) Do
        Target.Field(GroupColumnsOut + i).SetValue(
          TfsSqlSimpleExpression(AggExpList[i]).GetValue(-1), -1);
      For i := 0 To pred(AggList.Count) Do
        TfsSqlAggregate(AggList[i]).ResetCounters;
      Target.InsertPost;
    End;

  Begin

    Owner.FDatabase.StartTransaction([Nil]);
    Try
      IsFirst := True;
      HaveGroup := False;
      LastValues := TFSVariantList.Create(GroupColumnsIn);
      {we know that the source table has grouping columns first}
      For i := 0 To pred(AggList.Count) Do
        TfsSqlAggregate(AggList[i]).CreateCounter(Source.Field(i + GroupColumnsIn));
      Source.First;
      While Not Source.EOF Do
        Begin
          If IsFirst Then
            Begin
              IsFirst := False;
              NewGroup := True;
            End
          Else
            Begin
              NewGroup := False;
              For i := 0 To pred(GroupColumnsIn) Do
                Begin
                  Source.Field(i).Value(-1, L);
                  LastValues.Value(i, P);

                  {$IFDEF IsNoVariantInt64}
                  Case TVarData(L).Vtype Of
                    vt_e80:
                      Begin
                        Case TVarData(P).Vtype Of
                          VT_E80: If Decimal(L).ext80 <> Decimal(P).ext80 Then
                              Begin
                                NewGroup := True;
                                break;
                              End;
                          vt_decimal: If Decimal(L).ext80 <> Decimal(P).lo64 Then
                              Begin
                                NewGroup := True;
                                break;
                              End;
                          Else If Decimal(L).ext80 <> P Then
                            Begin
                              NewGroup := True;
                              break;
                            End;
                        End;
                      End;
                    vt_decimal:
                      Begin
                        Case TVarData(P).Vtype Of
                          VT_E80: If Decimal(L).lo64 <> Decimal(P).ext80 Then
                              Begin
                                NewGroup := True;
                                break;
                              End;
                          vt_decimal: If Decimal(L).lo64 <> Decimal(P).lo64 Then
                              Begin
                                NewGroup := True;
                                break;
                              End;
                          Else
                            Begin
                              Eval := Decimal(L).lo64;
                              If Eval <> P Then
                                Begin
                                  NewGroup := True;
                                  break;
                                End;
                            End;
                        End;
                      End;
                    Else
                      Begin
                        Case TVarData(P).Vtype Of
                          VT_E80: If L <> Decimal(P).ext80 Then
                              Begin
                                NewGroup := True;
                                break;
                              End;
                          vt_decimal:
                            Begin
                              Eval := Decimal(P).lo64;
                              If L <> Eval Then
                                Begin
                                  NewGroup := True;
                                  break;
                                End;
                            End;
                          Else
                            Begin
                              If Not GroupCase Then
                                If (Source.Field(i).GetType In [fstShortString..fstWideString,
                                  fstSingleChar, fstSingleWideChar]) Then
                                  Begin
                                    If Not varisnull(L) Then
                                      L := AnsiLowerCase(L);
                                    If Not varisnull(P) Then
                                      P := AnsiLowerCase(P);
                                  End;
                              If L <> P Then
                                Begin
                                  NewGroup := True;
                                  break;
                                End;
                            End;
                        End;
                      End;
                  End;
                  {$ELSE}
                  If Not GroupCase Then
                    If (Source.Field(i).GetType In [fstShortString..fstWideString,
                      fstSingleChar, fstSingleWideChar]) Then
                      Begin
                        If Not varisnull(L) Then
                          L := AnsiLowerCase(L);
                        If Not varisnull(P) Then
                          P := AnsiLowerCase(P);
                      End;
                  If L <> P Then
                    Begin
                      NewGroup := True;
                      break;
                    End;
                  {$ENDIF}
                End;
            End;
          If NewGroup Then
            Begin
              If HaveGroup Then
                Begin
                  Source.Prior;
                  WriteGroup;
                  Source.Next;
                End;
              For i := 0 To pred(GroupColumnsIn) Do
                Begin
                  Source.Field(i).Value(-1, L);
                  LastValues.SetValue(i, L);
                End;
              HaveGroup := True;
            End;

          For i := 0 To pred(AggList.Count) Do
            TfsSqlAggregate(AggList[i]).Update;
          Source.Next;
        End;
      {If we happen to have an empty set And If we don't have grouping
      columns, an 'empty' Record should be added to hold the
    count value of zero as well as null For any aggregates}
      If HaveGroup Or (GroupColumnsIn = 0) Then
        WriteGroup;
      For i := 0 To pred(AggList.Count) Do
        With TfsSqlAggregate(AggList[i]) Do
          DeleteCounter;
      Owner.FDatabase.Commit;
    Finally
      LastValues.Free;
    End;
  End;

Begin
  {build a normal answer table}

  GroupColumnsOut := 0;
  {build field definition For answer table}
  FieldDefList := TfsSqlFieldDefList.Create;
  Try
    Assert(Assigned(Columns));
    For i := 0 To pred(Columns.Count) Do
      Begin
        N := TfsSqlNode(Columns.Objects[i]);
        If i < GroupColumnsIn Then {!!.11}
          FieldDefList.AddField(Columns[i], '',
            N.GetType, N.GetSize, N.GetDecimals, N.GetBlobLevel, N.GetRound)
        Else {!!.11}
          {Begin !!.12}
          { Aggregate fields that reference date, time, & currency fields
        should be of the same Type in the result Set. Other field
          types should be changed to fstDouble in order to avoid clipping
          Of the value. }
          Case N.GetType Of
            fstExtended, fstBcd, fstCurrency..fstDateTime:
              FieldDefList.AddField(Columns[i], '',
                N.GetType, N.GetSize, N.GetDecimals, blnone, N.GetRound);
            fstSingleChar, {..8-bit character}
            fstSingleWideChar, {..16-bit character (UNICODE)}
            fstShortString, {..length Byte string}
            fstVarNullString, {..null-terminated string}
            fstNullString, {..null-terminated string}
            fstVarWideString,
              fstWideString: {..null-terminated string Of wide chars}
              // fstVarString, // Not realised
              // fstVarWideString:
              FieldDefList.AddField(Columns[i], '',
                N.GetType, N.GetSize, 0, blNone, N.GetRound);
            fstUInt8, {..byte (8-bit unsigned integer)}
            fstUInt16, {..16-bit unsigned Integer (aka word)}
            fstUInt32, {..32-bit unsigned integer}
            fstInt8, {..8-bit signed integer}
            fstInt16, {..16-bit signed integer}
            fstInt32, {..32-bit signed integer}
            fstInt64: {..64 int64 Type (8 bytes signed integer)}
              FieldDefList.AddField(Columns[i], '',
                N.GetType, N.GetSize, 0, blnone, rNone);
            fstAutoInc32, {..32-bit unsigned Integer; auto incrementing}
            fstAutoInc64,
              fstRecVersion: {..64-bit signed Integer; auto incrementing}
              FieldDefList.AddField(Columns[i], '',
                N.GetType, N.GetSize, N.GetDecimals, blnone, rNone);
            Else
              FieldDefList.AddField(Columns[i], '',
                fstExtended, 18, N.GetDecimals, blnone, N.GetRound);
          End;
        {End !!.12}
        Se := SelectionList.Selection[i];
        If (aGroupColumnList <> Nil) And
          aGroupColumnList.Contains(Columns[i], Se) Then
          inc(GroupColumnsOut)
        Else
          AggExpList.Add(N);
      End;

    T2 := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  Finally
    FieldDefList.Free;
  End;

  AggQueryMode := aqmGrouping;
  Try
    CopyGrouped(
      FGrpTable,
      T2,
      GroupColumnsIn,
      GroupColumnsOut,
      AggExpList.Count,
      GroupColumnTargetField,
      AggExpList);
  Finally
    AggQueryMode := aqmIdle;
  End;

  FGrpTable.Owner := Nil;
  FGrpTable.Free;
  FGrpTable := T2;
End;

Procedure TfsSqlSELECT.DoHaving;
Var
  T2: TFSSqlTableProxy;
Begin
  If CondExpHaving <> Nil Then
    Begin
      AggQueryMode := aqmHaving;
      Try
        HavingTable := FGrpTable;
        CondExpHaving.BindHaving;
        CondExpHaving.EnumNodes(ResetIsConstant, False);
        T2 := FGrpTable.CopyValidated(Self, CheckHaving);
        FGrpTable.Owner := Nil;
        FGrpTable.Free;
        FGrpTable := T2;
      Finally
        AggQueryMode := aqmIdle;
      End;
    End;
End;

Procedure TfsSqlSELECT.DoSortOnAll;
Var
  T2: TFSSqlTableProxy;
Begin
  T2 := FGrpTable.CopySortedOnAllFields(Self);
  FGrpTable.Owner := Nil; {!!.11}
  FGrpTable.Free;
  FGrpTable := T2;
End;

Procedure TfsSqlSELECT.DoRemoveDups(NeedData: Boolean);
Var
  i: Integer;
  LDistinct: Boolean;
  T2: TFSSqlTableProxy;
Begin
  If Not Distinct Then
    Begin
      LDistinct := False;
      For i := 0 To pred(AggList.Count) Do
        If TfsSqlAggregate(AggList[i]).Distinct Then
          Begin
            LDistinct := True;
            break;
          End;
    End
  Else
    LDistinct := True;

  If LDistinct And NeedData Then
    Begin
      T2 := FGrpTable.CopyUnique(Self, Self.DistinctCase); {!!.13}
      FGrpTable.Owner := Nil;
      FGrpTable.Free;
      FGrpTable := T2;
    End;
End;

Procedure TfsSqlSELECT.DoBuildGroupingTable(aGroupColumnList: TfsSqlGroupColumnList; GroupColumnsIn: Integer; FSF, FSX,
  GroupColumnTargetField: TList);
Var
  FieldDefList: TfsSqlFieldDefList;
  i: Integer;
  Co: TfsSqlGroupColumn;
  Se: TfsSqlSelection;
  F: TfsSqlFieldProxy;
  GrpTgtInfo: TfsGroupColumnTargetInfo;
  Ag: TfsSqlAggregate;
  FldType: TfsFieldType;
Begin
  FieldDefList := TfsSqlFieldDefList.Create;
  Try
    {build field definition For grouping table}
    For i := 0 To pred(GroupColumnsIn) Do
      Begin
        Co := aGroupColumnList.Column[i];
        Se := SelectionList.FindSelection(Co);
        If Se <> Nil Then
          Begin
            If Se.SimpleExpression.IsField(F) Then
              Begin
                FSF.Add(F);
                FSX.Add(Nil);
              End
            Else
              Begin
                FSF.Add(Nil);
                FSX.Add(Se.SimpleExpression);
              End;
            GrpTgtInfo := TfsGroupColumnTargetInfo.Create;
            GrpTgtInfo.SelFldIndex := Se.Index;
            GrpTgtInfo.LastValueIndex := i;
            GroupColumnTargetField.Add(GrpTgtInfo);
            FieldDefList.AddField(
              Co.QualColumnName, '', //?
              Se.SimpleExpression.GetType,
              Se.SimpleExpression.GetSize,
              Se.SimpleExpression.GetDecimals, Se.SimpleExpression.GetBlobLevel, Se.SimpleExpression.GetRound);

          End
        Else
          Begin
            {grouping field is not in selection list}
            {must be plain field in source table}
            F := FindField(Co.QualColumnName);
            FSF.Add(F);
            FSX.Add(Nil);
            FieldDefList.AddField(
              Co.QualColumnName, '', //?
              F.GetType,
              F.GetSize,
              F.GetDecimals,
              F.GetBlobLevel,
              f.GetRound);
          End;
      End;

    SelectionList.EnumNodes(EnumAggregates, False);

    For i := 0 To pred(AggList.Count) Do
      Begin
        Ag := TfsSqlAggregate(AggList[i]);
        If Ag.SimpleExpression <> Nil Then
          Begin
            FldType := Ag.SimpleExpression.GetType;
            If Not Ag.ValidType(FldType) Then
              Raise Exception.CreateFmt('The %s aggregate Function requires a numeric field.',
                [fsAgString[Ag.AgFunction]]);
            {AVG() needs float field even for Integer expressions}
            If Ag.AgFunction = agAvg Then
              FieldDefList.AddField(
                Ag.GetTitle(True) + '$' + IntToStr(i), {!!.11}
                '', //?
                FldType,
                Ag.SimpleExpression.GetSize,
                Ag.SimpleExpression.GetDecimals,
                Ag.SimpleExpression.GetBlobLevel,
                Ag.SimpleExpression.GetRound)
            Else
              FieldDefList.AddField(
                Ag.GetTitle(True) + '$' + IntToStr(i), {!!.11}
                '', //?
                FldType,
                Ag.SimpleExpression.GetSize,
                Ag.SimpleExpression.GetDecimals,
                Ag.SimpleExpression.GetBlobLevel,
                Ag.SimpleExpression.GetRound)
          End
        Else // COUNT(* )
          FieldDefList.AddField(
            Ag.GetTitle(True) + '$' + IntToStr(i), {!!.11}
            '', //
            fstExtended,
            18,
            0,
            blNone,
            rNone);
      End;

    FGrpTable := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self,
      FieldDefList);
  Finally
    FieldDefList.Free;
  End;
End;

Procedure TfsSqlSELECT.DoCheckAggregates;
Var
  i: Integer;
  Se: TfsSqlSelection;
  F: TfsSqlFieldProxy;
  LDistinct: Boolean;
Begin
  LDistinct := False;
  { LDistinct is being used to check For situation where a non-aggregate
    column is listed after an aggregate column. }
  For i := 0 To pred(SelectionList.SelectionCount) Do
    Begin
      se := SelectionList.Selection[i];
      If se.IsAggregateExpression Then
        LDistinct := True
      Else If LDistinct Then
        SQLError('Non-aggregate column "' + Trim(se.SQLText) +
          '" must appear before aggregate columns in the selection list.')
      Else If se.SimpleExpression.IsField(F) And
        ((GroupColumnList = Nil) Or
        (Not GroupColumnList.Contains(Columns[i], se))) Then
        SQLError('Non-aggregate column "' + Trim(se.SQLText) +
          '" must appear in GROUP BY');
    End;
End;

{!!.11 new}

Function TfsSqlSELECT.TableWithCount(Const ColumnName: String): TFSSqlTableProxy; {!!.12}
Var
  FieldDefList: TfsSqlFieldDefList;
Begin
  FieldDefList := TfsSqlFieldDefList.Create;
  Try
    FieldDefList.AddField(ColumnName, '', fstInt32, 18, 0, blNone, rNone);
    Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  Finally
    FieldDefList.Free;
  End;
  Owner.FDatabase.StartTransaction([Nil]);
  Try
    Result.Insert;
    Result.Field(0).SetValue(TFSSqlTableProxy(TablesReferencedByOrder.Objects[0]).GetRecordCount, -1);
    Result.InsertPost;
    Owner.FDatabase.Commit;
  Except
    Owner.FDatabase.AbortTransaction;
    Raise;
  End;
End;

Function TfsSqlSELECT.AggregateQueryResult(NeedData: Boolean): TFSSqlTableProxy;
Var
  i: Integer;
  T2: TFSSqlTableProxy;
  GroupColumnsIn: Integer;
  SortList: TfsSqlSortArray;
  GroupColumnTargetField,
    AggExpList,
    FSX: TList;
  FSF: TList;
  j: Integer;
  Status: TffResult;
  ColumnName: String; {!!.12}
  td: Boolean;
Begin
  If (GroupColumnList = Nil)
    //And (UnionGroupColumnList = Nil)
  And (CondExpWhere = Nil)
    And (TablesReferencedByOrder.Count = 1)
    And (CondExpHaving = Nil)
    And (SelectionList.SelectionCount = 1)
    And (SelectionList.Selection[0].SimpleExpression <> Nil)
    And (SelectionList.Selection[0].SimpleExpression.TermCount = 1)
    And (SelectionList.Selection[0].SimpleExpression.Term[0].FactorCount = 1)
    And (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate <> Nil)
    And (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate.AgFunction = agCount)
    And (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate.SimpleExpression = Nil) Then
    Begin
      {special case, plain "COUNT(*)" - use Record count reported by low-level code}
      If SelectionList.Selection[0].Column <> Nil Then {!!.12}
        ColumnName := SelectionList.Selection[0].Column.ColumnName {!!.12}
      Else {!!.12}
        ColumnName := 'COUNT(*)'; {!!.12}
      Result := TableWithCount(ColumnName); {!!.12}
      Exit;
    End;

  FGrpTable := Nil;
  T2 := Nil;

  {Columns contain the columns that will be in the result table.
  However, we may still group on other fields from the selection result -
in particular If this is a sub-query}

{field list For grouping table creation}

  FSX := Nil;
  FSF := Nil;
  GroupColumnTargetField := Nil;
  AggExpList := Nil;

  Try
    {field lists For joiner - one For expressions, another For fields}
    FSX := TList.Create;
    FSF := TList.Create;

    {where the groups should appear in the final result}
    GroupColumnTargetField := TList.Create;
    AggExpList := TList.Create;

    If GroupColumnList = Nil Then
      GroupColumnsIn := 0
    Else
      GroupColumnsIn := GroupColumnList.ColumnCount;

    {make sure all non-grouped columns are aggregate expressions}

    DoCheckAggregates;

    AggList := TList.Create;
    Try
      DoBuildGroupingTable(GroupColumnList, GroupColumnsIn, FSF, FSX,
        GroupColumnTargetField);

      Try
        If Joiner = Nil Then
          Begin

            Joiner := TfsSqlJoiner.Create(Owner, CondExpWhere);

            Assert(Assigned(TablesReferencedByOrder));
            For i := 0 To pred(TablesReferencedByOrder.Count) Do
              Joiner.Sources.Add(
                TFSSqlTableProxySubset.Create(
                TFSSqlTableProxy(TablesReferencedByOrder.Objects[i])));
          End;

        Joiner.ClearColumnList;

        If GroupColumnList <> Nil Then
          Begin
            For i := 0 To pred(GroupColumnsIn) Do
              Begin
                Joiner.AddColumn(
                  FSX[i],
                  FSF[i],
                  FGrpTable.Field(i));
              End;
          End;

        For i := 0 To pred(AggList.Count) Do
          Begin
            Joiner.AddColumn(
              TfsSqlAggregate(AggList[i]).SimpleExpression,
              Nil,
              FGrpTable.Field(i + GroupColumnsIn));
          End;

        If NeedData Then
          Begin
            Joiner.Target := FGrpTable;
            Owner.FDatabase.StartTransaction([Nil]);
            Try
              Joiner.Execute(Owner.UseIndex, Nil, jmNone);
              Owner.FDatabase.Commit;
            Except
              Owner.FDatabase.AbortTransaction;
              Raise;
            End;
          End;

        {turn off special aggregation flags so that the table result
        may be queried}
        For i := 0 To FGrpTable.FieldCount - 1 Do
          FGrpTable.Field(i).IsTarget := False;

        {At this point we have a table With all records that meet the
        WHERE criteria.}

        {if DISTINCT was specifed, we now need to remove any duplicates}

        DoRemoveDups(NeedData);

        If GroupColumnList <> Nil Then
          Begin
            { we need to group FGrpTable }
            { First, sort the data on groups }
            For i := 0 To pred(GroupColumnsIn) Do
              SortList[i] := FGrpTable.Field(i).Index + 1;

            Status := FGrpTable.Sort(GroupColumnsIn, SortList, Self.GroupCase); {!!.13}
            If Status <> DBIERR_NONE Then
              Raise EfsException.CreateNoData(fsStrResServer, Status);

          End;

        {we now have the data sorted on the grouping fields}
        {we then copy to another table With a slightly different
        layout to hold aggregate counters rather than data values
      For the non-grouped columns}

        DoGroupCopy(GroupColumnList, GroupColumnsIn, AggExpList,
          GroupColumnTargetField, Self.GroupCase);

        DoHaving;

        If (Parent Is TfsSqlInClause) Or (Parent Is TfsSqlMatchClause) Then
          Begin
            {need an index to allow the IN And MATCH clauses to be evaluated}
            DoSortOnAll;
          End
        Else
          Begin
            If LimitFirst Then
              Begin
                If NeedData Then
                  DoTopDown(FGrpTable, td);
                DoAggOrderBy;
              End
            Else
              Begin
                DoAggOrderBy;
                If NeedData Then
                  DoTopDown(FGrpTable, td);
              End;
          End;
      Except
        If FGrpTable <> T2 Then
          T2.Free;
        FGrpTable.Owner := Nil;
        FGrpTable.Free;
        Raise;
      End;

    Finally
      AggList.Free;
    End;
    For j := 0 To Pred(GroupColumnTargetField.Count) Do
      TfsGroupColumnTargetInfo(GroupColumnTargetField[j]).Free;

  Finally
    GroupColumnTargetField.Free;
    FSF.Free;
    FSX.Free;
    AggExpList.Free;
  End;
  Result := FGrpTable;
End;

Function TfsSqlSELECT.UnionGroupByQueryResult(NeedData: Boolean; aSource: TFSSqlTableProxy): TFSSqlTableProxy;
Var
  i: Integer;
  T2: TFSSqlTableProxy;
  GroupColumnsIn: Integer;
  SortList: TfsSqlSortArray;
  GroupColumnTargetField,
    AggExpList,
    FSX: TList;
  FSF: TList;
  j: Integer;
  Status: TffResult;
Begin
  FGrpTable := Nil;
  T2 := Nil;

  FSX := Nil;
  FSF := Nil;
  GroupColumnTargetField := Nil;
  AggExpList := Nil;

  Try
    FSX := TList.Create;
    FSF := TList.Create;

    GroupColumnTargetField := TList.Create;
    AggExpList := TList.Create;

    If UnionGroupColumnList = Nil Then
      GroupColumnsIn := 0
    Else
      GroupColumnsIn := UnionGroupColumnList.ColumnCount;

    //DoCheckAggregates;

    AggList := TList.Create;
    Try
      DoBuildGroupingTable(UnionGroupColumnList, GroupColumnsIn, FSF, FSX,
        GroupColumnTargetField);

      Try
        If Joiner = Nil Then
          Begin

            Joiner := TfsSqlJoiner.Create(Owner, CondExpWhere);

            Assert(Assigned(aSource));
            // For i := 0 To pred(TablesReferencedByOrder.Count) Do
            Joiner.Sources.Add(
              TFSSqlTableProxySubset.Create(
              aSource));
          End;

        Joiner.ClearColumnList;

        If UnionGroupColumnList <> Nil Then
          Begin
            For i := 0 To pred(GroupColumnsIn) Do
              Begin
                Joiner.AddColumn(
                  FSX[i],
                  FSF[i],
                  FGrpTable.Field(i));
              End;
          End;

        For i := 0 To pred(AggList.Count) Do
          Begin
            Joiner.AddColumn(
              TfsSqlAggregate(AggList[i]).SimpleExpression,
              Nil,
              FGrpTable.Field(i + GroupColumnsIn));
          End;

        If NeedData Then
          Begin
            Joiner.Target := FGrpTable;
            Owner.FDatabase.StartTransaction([Nil]);
            Try
              Joiner.Execute(Owner.UseIndex, Nil, jmNone);
              Owner.FDatabase.Commit;
            Except
              Owner.FDatabase.AbortTransaction;
              Raise;
            End;
          End;

        For i := 0 To FGrpTable.FieldCount - 1 Do
          FGrpTable.Field(i).IsTarget := False;

        If UnionGroupColumnList <> Nil Then
          Begin
            For i := 0 To pred(GroupColumnsIn) Do
              SortList[i] := FGrpTable.Field(i).Index + 1;

            Status := FGrpTable.Sort(GroupColumnsIn, SortList, True);
            If Status <> DBIERR_NONE Then
              Raise EfsException.CreateNoData(fsStrResServer, Status);

          End;

        DoGroupCopy(UnionGroupColumnList, GroupColumnsIn, AggExpList,
          GroupColumnTargetField, True);
      Except
        If FGrpTable <> T2 Then
          T2.Free;
        FGrpTable.Owner := Nil;
        FGrpTable.Free;
        Raise;
      End;

    Finally
      AggList.Free;
    End;
    Try
      For j := 0 To Pred(GroupColumnTargetField.Count) Do
        TfsGroupColumnTargetInfo(GroupColumnTargetField[j]).Free;
    Except
    End;

  Finally
    GroupColumnTargetField.Free;
    FSF.Free;
    FSX.Free;
    AggExpList.Free;
  End;
  Result := FGrpTable;
End;

{--------}

Function TfsSqlSELECT.Execute2(NeedData: Boolean): TFSSqlTableProxy;
Begin
  {check that all referenced tables And fields exist}
  If Not Bound Then
    Bind;
  If HaveAggregates Or (GroupColumnList <> Nil) Then
    Begin
      Result := AggregateQueryResult(NeedData);
    End
  Else
    Begin
      Result := NormalQueryResult(NeedData);
    End;
  If Result <> Nil Then
    Begin
      Result.Select := Self;
      Result.Group := Self;
    End;
End;
{--------}

Procedure TfsSqlSELECT.Execute(Var ALiveResult: Boolean;
  Var aCursorID: TffCursorID; Var RecordsRead: Integer);
Var
  T: TFSSqlTableProxy;
Begin
  Assert(Owner <> Nil);
  T := Execute2(True);
  aCursorID := T.CursorID;
  aLiveResult := t.OnlineCursor;
  T.LeaveCursorOpen := True;
  If T.Owner = Self Then
    Begin
      T.Owner := Nil;
      T.Free;
    End;
End;
{--------}

Function TfsSqlSELECT.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlSELECT)
    And (Distinct = TfsSqlSELECT(Other).Distinct)
    And (BothNil(SelectionList, TfsSqlSELECT(Other).SelectionList)
    Or (BothNonNil(SelectionList, TfsSqlSELECT(Other).SelectionList)
    And SelectionList.Equals(TfsSqlSELECT(Other).SelectionList))
    Or (((SelectionList = Nil) And TfsSqlSELECT(Other).WasStar)
    Or (WasStar And (TfsSqlSELECT(Other).SelectionList = Nil)))
    )
    And TableRefList.Equals(TfsSqlSELECT(Other).TableRefList)
    And (BothNil(CondExpWhere, TfsSqlSELECT(Other).CondExpWhere)
    Or (BothNonNil(CondExpWhere, TfsSqlSELECT(Other).CondExpWhere)
    And CondExpWhere.Equals(TfsSqlSELECT(Other).CondExpWhere))
    )
    And (BothNil(GroupColumnList, TfsSqlSELECT(Other).GroupColumnList)
    Or (BothNonNil(GroupColumnList, TfsSqlSELECT(Other).GroupColumnList)
    And GroupColumnList.Equals(TfsSqlSELECT(Other).GroupColumnList))
    )
    And (BothNil(CondExpHaving, TfsSqlSELECT(Other).CondExpHaving)
    Or (BothNonNil(CondExpHaving, TfsSqlSELECT(Other).CondExpHaving)
    And CondExpHaving.Equals(TfsSqlSELECT(Other).CondExpHaving))
    )
    And
    (BothNil(OrderList, TfsSqlSELECT(Other).OrderList)
    Or (BothNonNil(OrderList, TfsSqlSELECT(Other).OrderList)
    And OrderList.Equals(TfsSqlSELECT(Other).OrderList))
    )
    And
    (BothNil(UnionOrderList, TfsSqlSELECT(Other).UnionOrderList)
    Or (BothNonNil(UnionOrderList, TfsSqlSELECT(Other).UnionOrderList)
    And UnionOrderList.Equals(TfsSqlSELECT(Other).UnionOrderList)));
End;
{--------}

Function TfsSqlSELECT.GetResultTable: TFSSqlTableProxy;
Begin
  EnsureResultTable(True);
  Result := FResultTable;
End;

Function TfsSqlSELECT.IsSubQuery: Boolean;
Var
  P: TfsSqlNode;
Begin
  P := Parent;
  While P <> Nil Do
    Begin
      If (P Is TfsSqlSELECT)
        Or (P Is TfsSqlUPDATE)
        Or (P Is TfsSqlDELETE)
        Or (P Is TfsSqlINSERT) Then
        Begin
          Result := True;
          Exit;
        End;
      P := P.Parent;
    End;
  Result := False;
End;
{--------}

Function TfsSqlSELECT.Match(Value: Variant; Unique: Boolean;
  MatchOption: TfsSqlMatchOption): Boolean;

  Function RangeIsOne(Const Table: TFSSqlTableProxy): Boolean;
  Begin
    Result := Table.First And Not Table.Next;
  End;

Begin
  EnsureResultTable(True);
  If Not Unique Then
    Case MatchOption Of
      moUnspec:
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := ResultTable.First;
          End;
      moPartial:
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := ResultTable.First;
          End;
      Else //moFull :
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := ResultTable.First;
          End;
    End
  Else
    Case MatchOption Of
      moUnspec:
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := RangeIsOne(ResultTable);
          End;
      moPartial:
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := RangeIsOne(ResultTable);
          End;
      Else //moFull :
        If VarIsNull(Value) Then
          Result := True
        Else
          Begin
            ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
            Result := RangeIsOne(ResultTable);
          End;
    End;
End;
{--------}

Procedure TfsSqlSELECT.MatchType(ExpectedType: TfsFieldType; AllowMultiple: Boolean);
Begin
  //this will only be called when the current SELECT statement
  //functions as a sub-query
  If Not AllowMultiple And (SelectionList.SelectionCount <> 1) Then
    SQLError('Sub-query was expected to have exactly one column');
  EnsureResultTable(False);
End;
{====================================================================}

{===TfsSqlFieldRef===================================================}

Procedure TfsSqlFieldRef.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlFieldRef Then
    Begin
      TableName := TfsSqlFieldRef(Source).TableName;
      FieldName := TfsSqlFieldRef(Source).FieldName;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlFieldRef.CheckType;
{ Rewritten !!.06}
Var
  Found: Boolean;
  Inx: Integer;
  Select: TfsSqlSelect;
  Selection: TfsSqlSelection;
Begin
  Found := False;
  { The field reference may be an alias Or a direct reference to a field. }
  If (TableName = '') Then
    Begin
      { See If it is an alias. }
      Select := OwnerSelect;
      If Select <> Nil Then
        Begin
          For Inx := 0 To Pred(Select.SelectionList.SelectionCount) Do
            Begin
              Selection := Select.SelectionList.Selection[Inx];
              If (Not IsAncestor(Selection)) And
                (Selection.Column <> Nil) And
                (AnsiCompareText(Selection.Column.ColumnName, FieldName) = 0) Then
                Begin
                  FType := Selection.SimpleExpression.GetType;
                  Found := True;
                  Break;
                End;
            End;
        End
      Else
        Begin
        End;
    End;

  { If this isn't an alias then see If it is a direct reference. }
  If Not Found Then
    Begin
      Assert(Field <> Nil);
      FType := Field.GetType;
    End;
  TypeKnown := True;
End;
{--------}

Procedure TfsSqlFieldRef.ClearBinding;
Begin
  FField := Nil;
End;
{--------}

Function TfsSqlFieldRef.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  {!!.12 begin}
  If Field.IsTarget Then
    Begin
      Assert(OwnerSelect <> Nil);
      If Field.SrcIndex > -1 Then
        Result := TfsSqlSimpleExpression(OwnerSelect.Joiner.FSX[
          Field.SrcIndex]).DependsOn(Table)
      Else
        Result := Field.SrcField.OwnerTable = Table;
    End
  Else
    {!!.12 end}
    Result := Field.OwnerTable = Table;
End;
{--------}

Procedure TfsSqlFieldRef.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  If WasWildcard Then
    Begin
      WriteStr(Stream, TableName);
      WriteStr(Stream, '.*');
    End
  Else
    WriteStr(Stream, GetTitle(True)); {!!.11}
End;
{--------}

Procedure TfsSqlFieldRef.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlFieldRef.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlFieldRef)
    And (AnsiCompareText(TableName, TfsSqlFieldRef(Other).TableName) = 0)
    And
    ((AnsiCompareText(FieldName, TfsSqlFieldRef(Other).FieldName) = 0)
    Or (WasWildcard And (TfsSqlFieldRef(Other).FieldName = '')
    Or (((FieldName = '') And TfsSqlFieldRef(Other).WasWildcard))));
End;
{--------}

Function TfsSqlFieldRef.GetDecimals: Integer;
Begin
  Result := 0;
  If Field <> Nil Then
    Result := Field.GetDecimals;
End;

Function TfsSqlFieldRef.GetRound: TRound;
Begin
  Result := RNone;
  If Field <> Nil Then
    Result := Field.GetRound;
End;

Function TfsSqlFieldRef.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If Field <> Nil Then
    Result := field.getbloblevel;
End;
{--------}

Function TfsSqlFieldRef.GetField: TfsSqlFieldProxy;
Begin
  If FField = Nil Then
    FField := Parent.BindField(TableName, FieldName);
  Result := FField;
End;
{--------}

Function TfsSqlFieldRef.GetGroupField: TfsSqlFieldProxy;
Begin
  If OwnerSelect = Nil Then
    SQLError('Field references may not occur in this context');
  If FGroupField = Nil Then
    Begin
      FGroupField := OwnerSelect.FGrpTable.FieldByName(QualName);
      If FGroupField = Nil Then
        Begin
          FGroupField := OwnerSelect.FGrpTable.FieldByName(FieldName);
          If FGroupField = Nil Then
            SQLError('Unknown field:' + FieldName);
        End;
    End;
  Result := FGroupField;
End;
{--------}

Function TfsSqlFieldRef.GetSize: Integer;
Begin
  Result := Field.GetSize;
End;
{--------}

Function TfsSqlFieldRef.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If Qualified And (TableName <> '') Then {!!.11}
    If FieldName <> '' Then
      Result := TableName + '.' + FieldName
    Else
      Result := TableName + '.*'
  Else
    Result := FieldName;
End;

{--------}

Function TfsSqlFieldRef.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    CheckType;
  Result := FType;
End;
{--------}

Function TfsSqlFieldRef.GetValue(aArrayIndex: Integer): Variant;
Begin
  If (OwnerSelect <> Nil) And
    (OwnerSelect.AggQueryMode = aqmGrouping) Then
    Begin
      Result := GroupField.GetValue(aArrayIndex);
    End
  Else If Field.IsTarget Then
    Begin
      Assert(OwnerSelect <> Nil);
      If Field.SrcIndex > -1 Then
        Result := TfsSqlSimpleExpression(OwnerSelect.Joiner.FSX[Field.SrcIndex]).GetValue(aArrayIndex)
      Else
        Result := Field.SrcField.GetValue(aArrayIndex);
    End
  Else
    Result := Field.GetValue(aArrayIndex);
End;
{--------}

Function TfsSqlFieldRef.IsNull: Boolean;
Begin
  If (OwnerSelect <> Nil) And
    (OwnerSelect.AggQueryMode = aqmGrouping) Then
    Result := VarIsNull(GroupField.GetValue(-1))
  Else If Field.IsTarget Then
    Begin
      Assert(OwnerSelect <> Nil);
      If Field.SrcIndex > -1 Then
        Result := TfsSqlSimpleExpression(OwnerSelect.Joiner.
          FSX[Field.SrcIndex]).IsNull
      Else
        Result := Field.SrcField.IsNull;
    End
  Else
    Result := Field.IsNull;
End;
{--------}

Procedure TfsSqlFieldRef.MatchType(ExpectedType: TfsFieldType);
Begin
  If GetType <> ExpectedType Then
    Case GetType Of
      fstUInt8..fstCurrency, fstRecVersion, fstBcd, fstDate..fstDateTime:
        Case ExpectedType Of
          fstUInt8..fstCurrency, fstRecVersion, fstBcd, fstDate..fstDateTime: ;
          fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: ; // for array index
          Else
            TypeMismatch;
        End;
      fstSingleChar,
        fstSingleWideChar,
        fstShortString..fstWideString,
        fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
        Case ExpectedType Of
          fstSingleChar, fstSingleWideChar, fstShortString..fstWideString, fstBLOBMemo,
            fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble: ;
          fstUInt8..fstCurrency, fstRecVersion, fstBcd, fstDate..fstDateTime: ;
          Else
            TypeMismatch;
        End;
      fstBLOB..fstBLOBGraphic:
        Case ExpectedType Of
          fstSingleChar, fstSingleWideChar,
            fstShortString..fstWideString,
            fstBLOB..fstBLOBGraphic: ;
          Else
            TypeMismatch;
        End;
      Else
        TypeMismatch;
    End;
End;
{--------}

Function TfsSqlFieldRef.QualName: String;
Var
  Name: String;
Begin
  Result := FFieldName;
  { If no tablename specified then obtain table name Of source table. }
  If FTableName = '' Then
    Begin
      If assigned(FField) Then
        Result := FField.OwnerTable.Name + '.' + FFieldName
      Else
        Result := FFieldName;
    End
  Else
    Begin
      If OwnerSelect = Nil Then
        SQLError('Field references may not occur in this context');
      { Has a table name. Is it really an alias? }
      Name := OwnerSelect.TableRefList.GetNameForAlias(FTableName);
      If Name <> '' Then
        Result := Name + '.' + FFieldName
      Else
        Result := TableName + '.' + FFieldName;
    End;
End;
{====================================================================}

{===TfsSqlAggregate==================================================}
{--------}

Procedure TfsSqlAggregate.AddAggregate(Target: TList);
Begin
  Target.Add(Self);
End;
{--------}

Procedure TfsSqlAggregate.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlAggregate Then
    Begin
      AgFunction := TfsSqlAggregate(Source).AgFunction;
      SimpleExpression.Free;
      SimpleExpression := Nil;
      If assigned(TfsSqlAggregate(Source).SimpleExpression) Then
        Begin
          SimpleExpression := TfsSqlSimpleExpression.Create(Self);
          SimpleExpression.Assign(TfsSqlAggregate(Source).SimpleExpression);
        End;
      Distinct := TfsSqlAggregate(Source).Distinct;
      DistinctCase := TfsSqlAggregate(Source).DistinctCase;
      GroupCase := TfsSqlAggregate(Source).GroupCase;
      GroupUnionCase := TfsSqlAggregate(Source).GroupUnionCase;
    End
  Else
    AssignError(Source);
End;
{--------}

Procedure TfsSqlAggregate.CreateCounter(SourceField: TfsSqlFieldProxy);
Begin
  FCounter := TfsAggCounter.Create;
  FSourceField := SourceField;
End;
{--------}

Procedure TfsSqlAggregate.DeleteCounter;
Begin
  FCounter.Free;
  FCounter := Nil;
  FSourceField := Nil;
End;
{--------}

Function TfsSqlAggregate.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SimpleExpression.DependsOn(Table);
End;
{--------}

Destructor TfsSqlAggregate.Destroy;
Begin
  SimpleExpression.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlAggregate.ResetCounters;
Begin
  FCounter.Reset;
End;
{--------}

Procedure TfsSqlAggregate.Update;
Var
  V: Variant;
Begin
  V := FSourceField.GetValue(-1);
  Case AgFunction Of
    agCount:
      If (FSourceField = Nil) Or Not VarIsNull(V) Then {!!.13}
        Begin
          FCounter.DataType := Self.FSourceField.GetType;
          FCounter.Add(1);
        End;
    Else
      If Not VarIsNull(V) Then
        Begin
          FCounter.DataType := Self.FSourceField.GetType;
          FCounter.Add(V);
        End;
  End;
End;
{--------}

Procedure TfsSqlAggregate.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  WriteStr(Stream, fsAgString[AgFunction]);
  WriteStr(Stream, '(');
  If SimpleExpression <> Nil Then
    Begin
      If Distinct Then
        Begin
          WriteStr(Stream, ' DISTINCT');
          If Not DistinctCase Then
            WriteStr(Stream, ' DISTINCT CASE FALSE');
        End
      Else
        WriteStr(Stream, ' ALL');
      SimpleExpression.EmitSQL(Stream);
    End
  Else
    WriteStr(Stream, '*');
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlAggregate.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If SimpleExpression <> Nil Then
    SimpleExpression.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlAggregate.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlAggregate)
    And (AgFunction = TfsSqlAggregate(Other).AgFunction)
    And (Distinct = TfsSqlAggregate(Other).Distinct)
    And (
    BothNil(SimpleExpression, TfsSqlAggregate(Other).SimpleExpression)
    Or (
    BothNonNil(SimpleExpression, TfsSqlAggregate(Other).SimpleExpression)
    And SimpleExpression.Equals(TfsSqlAggregate(Other).SimpleExpression)
    )
    );
End;
{--------}

{ TfsAggCounter }

Const
  NumericVarTypes: Set Of Byte =
  [varSmallint, varInteger, varSingle,
    {$IFDEF DCC6OrLater}
  varShortInt,
    varWord,
    varLongWord,
    varInt64,
    {$ENDIF}
  varDouble, varCurrency,
    {$IFDEF IsNoVariantInt64}
  14,
    15,
    {$ENDIF}
  varByte];

Function TfsAggCounter.GetCount: Variant;
Begin
  Result := eFCount;
End;

Function TfsAggCounter.GetAvg: Variant;
Begin
  If eFCount <> 0 Then
    Begin
      {$IFDEF IsNoVariantInt64}
      Case DataType Of
        fstUInt8..fstCurrency, fstRecVersion:
          Begin
            TVarData(Result).vtype := VT_E80;
            decimal(Result).ext80 := eFSum / eFCount;
          End;
        Else
          Result := eFSum / eFCount;
      End;
      {$ELSE}
      Result := eFSum / eFCount;
      {$ENDIF}
    End
  Else
    Result := Null;
End;

Function TfsAggCounter.GetMax: Variant;
Begin
  If eFCount <> 0 Then
    Begin
      {$IFDEF IsNoVariantInt64}
      Case DataType Of
        fstUInt8..fstCurrency, fstRecVersion:
          Begin
            TVarData(Result).vtype := VT_E80;
            decimal(Result).ext80 := eFMax;
          End;
        Else
          Result := eFMax;
      End;
      {$ELSE}
      Result := eFMax;
      {$ENDIF}
    End
  Else
    Result := Null;
End;

Function TfsAggCounter.GetMin: Variant;
Begin
  If eFCount <> 0 Then
    Begin
      {$IFDEF IsNoVariantInt64}
      Case DataType Of
        fstUInt8..fstCurrency, fstRecVersion:
          Begin
            TVarData(Result).vtype := VT_E80;
            decimal(Result).ext80 := eFMin;
          End;
        Else
          Result := eFMin;
      End;
      {$ELSE}
      Result := eFMin;
      {$ENDIF}
    End
  Else
    Result := Null;
End;

Function TfsAggCounter.GetSum: Variant;
Begin
  If eFCount <> 0 Then
    Begin
      {$IFDEF IsNoVariantInt64}
      Case DataType Of
        fstUInt8..fstCurrency, fstRecVersion:
          Begin
            TVarData(Result).vtype := VT_E80;
            decimal(Result).ext80 := eFSum;
          End;
        Else
          Result := eFSum;
      End;
      {$ELSE}
      Result := eFSum;
      {$ENDIF}
    End
  Else
    Result := Null;
End;

Procedure TfsAggCounter.Reset;
Begin
  eFCount := 0;
End;

Procedure TfsAggCounter.Add(Const Value: Variant);
Var
  e: Extended;
  i: Int64;
Begin
  If eFCount = 0 Then
    Begin
      If varisnull(Value) Then
        Begin
          eFMin := 0;
          eFMax := 0;
          eFSum := 0;
          Exit;
        End;
      Case DataType Of
        {$IFDEF IsNoVariantInt64}
        fstSingle..fstCurrency:
          Begin
            eFMin := decimal(Value).ext80;
            eFMax := decimal(Value).ext80;
            eFSum := decimal(Value).ext80;
          End;
        fstInt64, fstAutoInc64, fstRecVersion, fstUInt32:
          Begin
            eFMin := Decimal(Value).lo64;
            eFMax := decimal(Value).lo64;
            eFSum := decimal(Value).lo64;
          End;
        {$ENDIF}
        fstDate..fstDateTime, fstUInt8, fstUInt16, fstInt8..fstInt32:
          Begin
            eFMin := Value;
            eFMax := Value;
            eFSum := Value;
          End;
        Else
          Begin
            eFMin := Value;
            eFMax := Value;
            If (VarType(Value) And VarTypeMask) In NumericVarTypes Then
              eFSum := Value;
          End;
      End;
    End
  Else
    Begin
      Case DataType Of
        {$IFDEF IsNoVariantInt64}
        fstInt64, fstAutoInc64, fstRecVersion, fstUInt32:
          Begin
            If varisnull(Value) Then
              i := 0
            Else
              i := decimal(Value).lo64;
            eFSum := eFSum + i;
            If i < eFMin Then
              eFMin := i;
            If i > eFMax Then
              eFMax := i;
          End;
        fstSingle..fstCurrency:
          Begin
            If varisnull(Value) Then
              e := 0
            Else
              e := decimal(Value).ext80;
            eFSum := eFSum + e;
            If e < eFMin Then
              eFMin := e;
            If e > eFMax Then
              eFMax := e;
          End;
        {$ENDIF}
        fstDateTime, fstDate, fstTime, fstUInt8, fstUInt16, fstInt8..fstInt32:
          Begin
            If varisnull(Value) Then
              e := 0
            Else
              e := Value;
            eFSum := eFSum + e;
            If e < eFMin Then
              eFMin := e;
            If e > eFMax Then
              eFMax := e;
          End;
        Else
          Begin
            If varisnull(Value) Then
              e := 0
            Else
              e := Value;
            If e < eFMin Then
              eFMin := e;
            If e > eFMax Then
              eFMax := e;
            If (VarType(Value) And VarTypeMask) In NumericVarTypes Then
              eFSum := eFSum + e;
          End;
      End;
    End;
  eFCount := eFCount + 1;
End;

Function TfsSqlAggregate.GetAggregateValue: Variant;
Begin
  If FCounter = Nil Then
    Result := 0
  Else
    Begin
      Case AgFunction Of
        agCount:
          Result := FCounter.Count;
        agMin:
          Result := FCounter.Min;
        agMax:
          Result := FCounter.Max;
        agSum:
          Result := FCounter.Sum;
        Else //agAvg :
          Result := FCounter.Avg;
      End;
    End;
End; {--------}

Procedure TfsSqlAggregate.FlagAggregate(Select: TfsSqlSELECT);
Begin
  Select.HaveAggregates := True;
End;
{--------}

Function TfsSqlAggregate.GetDecimals: Integer;
Begin
  If SimpleExpression <> Nil Then
    Result := SimpleExpression.GetDecimals
  Else
    Result := 0;
End;

Function TfsSqlAggregate.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If SimpleExpression <> Nil Then
    Result := SimpleExpression.getbloblevel;
End;
{--------}

Function TfsSqlAggregate.GetSize: Integer;
Begin
  If SimpleExpression <> Nil Then
    Result := SimpleExpression.GetSize
  Else
    Result := 0;
End;
{--------}

Function TfsSqlAggregate.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  Result := fsAgString[AgFunction] + '(';
  If Distinct Then
    Result := Result + 'DISTINCT ';
  If SimpleExpression = Nil Then
    Result := Result + '*'
  Else
    Result := Result + SimpleExpression.GetTitle(Qualified); {!!.11}
  Result := Result + ')';
End;

{--------}

Function TfsSqlAggregate.GetType: TfsFieldType;
Begin
  If SimpleExpression = Nil Then
    Result := fstExtended
  Else
    Case SimpleExpression.GetType Of
      fstDateTime:
        Case AgFunction Of
          agCount:
            Result := fstExtended;
          Else
            Result := fstDateTime;
        End;
      fstDate:
        Case AgFunction Of
          agCount:
            Result := fstExtended;
          Else
            Result := fstDate;
        End;
      fstTime:
        Case AgFunction Of
          agCount:
            Result := fstExtended;
          Else
            Result := fstTime;
        End;
      Else
        Result := fstExtended;
    End;
End;
{--------}

Procedure TfsSqlAggregate.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstUInt8..fstDateTime, fstRecVersion {fstBinaryDecimals}:
      ;
    Else
      TypeMismatch;
  End;
End;
{--------}

Function TfsSqlAggregate.Reduce: Boolean;
Begin
  If SimpleExpression <> Nil Then
    Result := SimpleExpression.Reduce
  Else
    Result := False;
End;
{--------}

Function TfsSqlAggregate.ValidType(AType: TfsFieldType): Boolean;
Begin
  Case agFunction Of
    agSum, agAvg:
      Result := (aType In [fstUInt8..fstDateTime, fstRecVersion {fstBinaryDecimals}]);
    Else
      Result := True;
  End;
End;
{====================================================================}

{===TfsSqlColumn=====================================================}

Procedure TfsSqlColumn.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlColumn Then
    Begin
      ColumnName := TfsSqlColumn(Source).ColumnName;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlColumn.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  WriteStr(Stream, ColumnName);
End;
{--------}

Procedure TfsSqlColumn.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlColumn.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlColumn)
    And (AnsiCompareText(ColumnName, TfsSqlColumn(Other).ColumnName) = 0);
End;
{====================================================================}

{===TfsSqlIsTest=====================================================}

Function TfsSqlIsTest.AsBoolean(Const TestValue: Variant): Boolean;
Begin
  Case IsOp Of
    ioNull:
      Result := VarIsNull(TestValue) Xor UnaryNot;
    ioTrue:
      If UnaryNot Then
        Result := Not TestValue
      Else
        Result := TestValue;
    ioFalse:
      If UnaryNot Then
        Result := TestValue
      Else
        Result := Not TestValue;
    Else
      //ioUnknown :
      Result := VarIsNull(TestValue) Xor UnaryNot;
  End;
End;
{--------}

Procedure TfsSqlIsTest.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlIsTest Then
    Begin
      UnaryNot := TfsSqlIsTest(Source).UnaryNot;
      IsOp := TfsSqlIsTest(Source).IsOp;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlIsTest.EmitSQL(Stream: TStream);
Const
  IsOpStr: Array[TfsSqlIsOp] Of String =
  ('NULL', 'TRUE', 'FALSE', 'UNKNOWN');
Begin
  WriteStr(Stream, ' IS');
  If UnaryNot Then
    WriteStr(Stream, ' NOT');
  WriteStr(Stream, ' ');
  WriteStr(Stream, IsOpStr[IsOp]);
End;
{--------}

Procedure TfsSqlIsTest.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlIsTest.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlIsTest)
    And (UnaryNot = TfsSqlIsTest(Other).UnaryNot)
    And (IsOp = TfsSqlIsTest(Other).IsOp);
End;
{--------}

Function TfsSqlIsTest.Evaluate(
  Expression: TfsSqlSimpleExpression): Boolean;
{- allow check against NULL For non-variant compatible fields}
Begin
  Case IsOp Of
    ioNull, ioUnknown:
      Result := Expression.IsNull Xor UnaryNot;
    Else
      Result := AsBoolean(Expression.GetValue(-1));
  End;
End;

Procedure TfsSqlIsTest.MatchType(ExpectedType: TfsFieldType);
Begin
End;

{====================================================================}

{===TfsSqlBetweenClause==============================================}

Function TfsSqlBetweenClause.AsBoolean(Const TestValue: Variant): Boolean;
Var
  Sl, Sh: Variant;
  Sle, She, Tve: Extended;
Begin
  If VarIsNull(TestValue) Then
    Result := False
  Else
    Begin
      Sl := SimpleLow.GetValue(-1);
      Sh := SimpleHigh.GetValue(-1);
      {$IFDEF IsNoVariantInt64}
      Case TVarData(Sl).Vtype Of
        vt_e80:
          Begin
            Case TVarData(TestValue).Vtype Of
              vt_e80:
                Begin
                  Result := ((decimal(TestValue).ext80 >= decimal(Sl).ext80) And (
                    decimal(TestValue).ext80 <= decimal(Sh).ext80)) Xor Negated;
                End;
              vt_decimal:
                Begin
                  Result := ((decimal(TestValue).lo64 >= decimal(Sl).ext80) And
                    (decimal(TestValue).lo64 <= decimal(Sh).ext80)) Xor Negated;
                End;
              Else
                Result := ((TestValue >= decimal(Sl).ext80) And
                  (TestValue <= decimal(Sh).ext80)) Xor Negated;
            End;
          End;
        vt_decimal:
          Begin
            Case TVarData(TestValue).Vtype Of
              vt_e80:
                Begin
                  Result := ((decimal(TestValue).ext80 >= decimal(Sl).lo64) And (
                    decimal(TestValue).ext80 <= decimal(Sh).lo64)) Xor Negated;
                End;
              vt_decimal:
                Begin
                  Result := ((decimal(TestValue).lo64 >= decimal(Sl).lo64) And
                    (decimal(TestValue).lo64 <= decimal(Sh).lo64)) Xor Negated;
                End;
              Else
                Begin
                  Sle := decimal(Sl).lo64;
                  She := decimal(Sh).lo64;
                  Result := ((TestValue >= Sle) And (TestValue <= She)) Xor Negated;
                End;
            End;
          End;
        Else
          Begin
            // Sl inne
            Case TVarData(TestValue).Vtype Of
              vt_e80:
                Begin
                  Result := ((decimal(TestValue).ext80 >= Sl) And (
                    decimal(TestValue).ext80 <= Sh)) Xor Negated;
                End;
              vt_decimal:
                Begin
                  Tve := decimal(TestValue).lo64;
                  Result := ((Tve >= Sl) And (Tve <= Sh)) Xor Negated;
                End;
              Else
                Begin
                  Result := ((TestValue >= Sl) And (TestValue <= Sh)) Xor Negated;
                End;
            End;
          End;
      End;
      {$ELSE}
      Result := ((TestValue >= Sl) And (TestValue <= Sh)) Xor Negated;
      {$ENDIF}
    End;
End;

Procedure TfsSqlBetweenClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlBetweenClause Then
    Begin
      Negated := TfsSqlBetweenClause(Source).Negated;
      SimpleLow.Free;
      SimpleLow := TfsSqlSimpleExpression.Create(Self);
      SimpleLow.Assign(TfsSqlBetweenClause(Source).SimpleLow);
      SimpleHigh.Free;
      SimpleHigh := TfsSqlSimpleExpression.Create(Self);
      SimpleHigh.Assign(TfsSqlBetweenClause(Source).SimpleHigh);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlBetweenClause.CheckIsConstant;
Begin
  FIsConstantChecked := True;
  FIsConstant := SimpleLow.IsConstant And SimpleHigh.IsConstant;
End;
{--------}

Function TfsSqlBetweenClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SimpleLow.DependsOn(Table) Or SimpleHigh.DependsOn(Table);
End;

Destructor TfsSqlBetweenClause.Destroy;
Begin
  SimpleLow.Free;
  SimpleHigh.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlBetweenClause.EmitSQL(Stream: TStream);
Begin
  If Negated Then
    WriteStr(Stream, ' NOT');
  WriteStr(Stream, ' BETWEEN ');
  SimpleLow.EmitSQL(Stream);
  WriteStr(Stream, ' And ');
  SimpleHigh.EmitSQL(Stream);
End;
{--------}

Procedure TfsSqlBetweenClause.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SimpleLow.EnumNodes(EnumMethod, Deep);
  SimpleHigh.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlBetweenClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlBetweenClause)
    And (Negated = TfsSqlBetweenClause(Other).Negated)
    And (SimpleLow.Equals(TfsSqlBetweenClause(Other).SimpleLow))
    And (SimpleHigh.Equals(TfsSqlBetweenClause(Other).SimpleHigh));
End;
{--------}

Function TfsSqlBetweenClause.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Procedure TfsSqlBetweenClause.MatchType(ExpectedType: TfsFieldType);
Begin
  SimpleLow.MatchType(ExpectedType);
  SimpleHigh.MatchType(ExpectedType);
End;
{--------}

Function TfsSqlBetweenClause.Reduce: Boolean;
Begin
  Result := SimpleLow.Reduce Or SimpleHigh.Reduce;
End;

Procedure TfsSqlBetweenClause.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{====================================================================}

{ TfsSqlLikePattern }

Constructor TfsSqlLikePattern.Create(SearchPattern: String; Const Escape: String);
Var
  i: Integer;
  Mask: String;
  Esc: Char;
Begin
  FloatPatterns := TStringList.Create;
  FloatMasks := TStringList.Create;

  {
  Search pattern is made up Of
  0 Or 1 lead pattern
  0-N floating patterns, And
  0 Or 1 trail pattern.
  Patterns are separated by '%'.
If search pattern starts With '%', it does not have a lead pattern.
If search pattern ends With '%', it does not have a trail pattern.

  Place holders, '_', are not considered here but in Find.

  }

{build a separate mask String For place holders so that we can use
the same logic for escaped And non-escaped search patterns}

  Mask := SearchPattern;
  If Escape <> '' Then
    Begin
      i := length(SearchPattern);
      Esc := Escape[1];
      While i >= 2 Do
        Begin
          If SearchPattern[i - 1] = Esc Then
            Begin
              Mask[i] := ' '; // blank out the mask character
              //remove the escape
              Delete(Mask, i - 1, 1);
              Delete(SearchPattern, i - 1, 1);
            End;
          dec(i);
        End;
    End;

  If (SearchPattern = '') Then
    Exit;

  If Mask[1] <> '%' Then
    Begin
      {we have a lead pattern}
      i := fsPosCh('%', Mask);
      If i = 0 Then
        Begin
          {entire search pattern is a lead pattern}
          LeadPattern := SearchPattern;
          LeadMask := Mask;
          Exit;
        End;

      LeadPattern := copy(SearchPattern, 1, i - 1);
      LeadMask := copy(Mask, 1, i - 1);

      Delete(SearchPattern, 1, i - 1);
      Delete(Mask, 1, i - 1);
    End;

  If (SearchPattern = '') Then
    Exit;

  i := length(Mask);

  If Mask[i] <> '%' Then
    Begin
      {we have a trail pattern}
      While (i > 0) And (Mask[i] <> '%') Do
        dec(i);
      If i = 0 Then
        Begin
          {entire remaining pattern is a trail pattern}
          TrailPattern := SearchPattern;
          TrailMask := Mask;
          Exit;
        End;

      TrailPattern := copy(SearchPattern, i + 1, MaxInt);
      TrailMask := copy(Mask, i + 1, MaxInt);

      Delete(SearchPattern, i + 1, MaxInt);
      Delete(Mask, i + 1, MaxInt);
    End;

  {we now have one Or more floating patterns separated by '%'}

  If Mask = '' Then
    Exit;

  If Mask[1] <> '%' Then
    Exit;

  Delete(Mask, 1, 1);
  Delete(SearchPattern, 1, 1);

  Repeat

    i := fsPosCh('%', Mask);

    If i = 0 Then
      Begin
        {entire remaining search pattern is one pattern}
        FloatPatterns.Add(SearchPattern);
        FloatMasks.Add(Mask);
        Exit;
      End;

    FloatPatterns.Add(copy(SearchPattern, 1, i - 1));
    FloatMasks.Add(copy(Mask, 1, i - 1));

    Delete(SearchPattern, 1, i);
    Delete(Mask, 1, i);

  Until SearchPattern = '';

End;

Destructor TfsSqlLikePattern.Destroy;
Begin
  FloatPatterns.Free;
  FloatMasks.Free;
  Inherited;
End;

Function CharsDiffer(IgnoreCase: Boolean; C1, C2: Char): Boolean;
Begin
  If IgnoreCase Then
    Result := CharUpper(Pointer(C1)) <> CharUpper(Pointer(C2))
  Else
    Result := C1 <> C2;
End;

Function Match(Const Pattern, Mask: String;
  PatternLength: Integer;
  Const PTextToSearch: PAnsiChar;
  Const TextLen: Integer;
  StartIndex: Integer;
  IgnoreCase: Boolean
  ): Boolean;
{ Look For an exact match Of the pattern at StartIndex, disregarding
locations With '_' in the mask.
Note: StartIndex is base zero. }
Var
  i: Integer;
Begin
  Result := True;
  If TextLen < PatternLength Then
    Result := False
  Else
    For i := 1 To PatternLength Do
      If (Mask[i] <> '_') And
        CharsDiffer(IgnoreCase, PTextToSearch[StartIndex + i - 1], Pattern[i]) Then
        Begin
          Result := False;
          Break;
        End; { If }
End;

Function Scan(Const Pattern, Mask: String;
  PatternLength: Integer;
  Const PTextToSearch: PAnsiChar;
  Const TextLen: Integer;
  StartIndex: Integer;
  IgnoreCase: Boolean {!!.13}
  ): Integer;
{Modified !!.13}
{ Scan For a match Of the pattern starting at StartIndex, disregarding
locations With '_' in the mask. Return -1 If not found, otherwise
return the position immediately following the matched phrase. }
Var
  L, i: Integer;
  Found: Boolean;
Begin
  L := TextLen - StartIndex;
  Repeat
    If L < PatternLength Then
      Begin
        Result := -1;
        Exit;
      End;
    Found := True;
    For i := 1 To PatternLength Do
      If (i - 1 > L) Or (Mask[i] <> '_') And
        {(PTextToSearch[i + StartIndex - 1] <> Pattern[i]) then begin}{!!.13}
      CharsDiffer(IgnoreCase, PTextToSearch[i + StartIndex - 1], Pattern[i]) Then
        Begin {!!.13}
          Found := False;
          Break;
        End;
    If Found Then
      Begin
        Result := StartIndex + PatternLength;
        Exit;
      End;
    inc(StartIndex);
    dec(L);
  Until False;
End;

Function TfsSqlLikePattern.Find(Const TextToSearch: Variant;
  IgnoreCase: Boolean): Boolean;
{Search the TextToSearch. Return true If the search pattern was found}
Var
  TextLen,
    LeadLen,
    TrailLen,
    i,
    l,
    StartPos,
    EndPos: Integer;
  VStr, P: String;
  VPtr: PAnsiChar;
Begin
  Result := False;
  Try
    If TVarData(TextToSearch).VType And VarTypeMask = varByte Then
      Begin
        TextLen := VarArrayHighBound(TextToSearch, 1);
        If TextLen = 0 Then
          Exit;
        VStr := '';
        VPtr := VarArrayLock(TextToSearch);
      End
    Else
      Begin
        If (TextToSearch = '') Or (TextToSearch = null) Then
          TextLen := 0
        Else
          TextLen := Length(TextToSearch);
        If TextLen = 0 Then
          Exit;
        VStr := VarToStr(TextToSearch);
        VPtr := PAnsiChar(VStr);
      End;

    LeadLen := Length(LeadPattern);
    TrailLen := Length(TrailPattern);
    If LeadLen > 0 Then
      Begin
        { If there is a lead pattern then see If there is a match. }
        If Not Match(LeadPattern, LeadMask, LeadLen, VPtr, TextLen, 0,
          IgnoreCase) Then
          Begin {!!.13}
            { No match so exit. }
            Result := False;
            Exit;
          End;
        { There was a match so Set the starting position For the next match. }
        StartPos := LeadLen;
      End
    Else
      { No lead pattern. Next match starts at beginning of String. }
      StartPos := 0;

    If TrailLen > 0 Then
      Begin
        { There is a trail pattern. Does it overlap With the lead pattern?  }
        i := TextLen - TrailLen;
        If i < StartPos Then
          Begin
            { Yes it overlaps. A match is not possible so exit. }
            Result := False;
            Exit;
          End;
        If Not Match(TrailPattern, TrailMask, TrailLen, VPtr, TextLen, i,
          IgnoreCase) Then
          Begin {!!.13}
            Result := False;
            Exit;
          End;
        EndPos := i - 1;
      End
    Else
      EndPos := TextLen - 1;

    If FloatPatterns.Count = 0 Then
      If TextLen <> LeadLen + TrailLen Then
        Begin
          Result := False;
          Exit;
        End;

    For i := 0 To pred(FloatPatterns.Count) Do
      Begin
        P := FloatPatterns[i];
        l := Length(P);
        { If the length Of the float pattern is greater than the number Of
          characters left in the String then a match is not possible. }
        If l > EndPos - StartPos + 1 Then
          Begin
            Result := False;
            Exit;
          End;
        StartPos := Scan(P, FloatMasks[i], l, VPtr, TextLen, StartPos, IgnoreCase); {!!.13}
        If StartPos = -1 Then
          Begin
            Result := False;
            Exit;
          End;
      End;
    Result := True;
  Finally
    If VStr = '' Then
      VarArrayUnlock(TextToSearch);
  End;
End;
{===TfsSqlLikeClause=================================================}

Function TfsSqlLikeClause.AsBoolean(Const TestValue: Variant): Boolean;
Var
  V1, V2, V3: Variant;
  s1: String;
  b: Boolean;
Begin
  If VarIsNull(TestValue) Or (TestValue = '') Then
    Begin
      Result := Negated;
      Exit;
    End;
  {$IFDEF IsNoVariantInt64}
  Case TVarData(TestValue).Vtype Of
    vt_e80:
      Begin
        s1 := fsFloatToStr(Decimal(TestValue).ext80);
        v1 := s1;
      End;
    vt_decimal:
      Begin
        s1 := IntToStr(Decimal(TestValue).lo64);
        v1 := s1;
      End;
    Else
      V1 := TestValue;
  End;
  {$ELSE}
  V1 := TestValue;
  {$ENDIF}

  If LikePattern = Nil Then
    Begin
      V2 := SimpleExp.GetValue(-1);
      {$IFDEF IsNoVariantInt64}
      If Not VarIsNull(V2) Then
        Begin
          Case TVarData(V2).Vtype Of
            vt_e80:
              Begin
                s1 := fsFloatToStr(Decimal(V2).ext80);
                v2 := s1;
              End;
            vt_decimal:
              Begin
                s1 := IntToStr(Decimal(V2).lo64);
                v2 := s1;
              End;
          End;
        End;
      {$ENDIF}

      If EscapeExp <> Nil Then
        Begin
          V3 := EscapeExp.GetValue(-1);
          {$IFDEF IsNoVariantInt64}
          If Not VarIsNull(V3) Then
            Begin
              Case TVarData(V3).Vtype Of
                vt_e80:
                  Begin
                    s1 := fsFloatToStr(Decimal(V3).ext80);
                    v3 := s1;
                  End;
                vt_decimal:
                  Begin
                    s1 := IntToStr(Decimal(V3).lo64);
                    v3 := s1;
                  End;
              End;
            End;
          {$ENDIF}

          LikePattern := TfsSqlLikePattern.Create(v2, v3);
        End
      Else
        LikePattern := TfsSqlLikePattern.Create(v2, '');
    End;
  b := LikePattern.Find(V1, IgnoreCase);
  Result := b Xor Negated;
  If Not IsConstant Then
    Begin
      LikePattern.Free;
      LikePattern := Nil;
    End;
End;
{--------}

Procedure TfsSqlLikeClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlLikeClause Then
    Begin
      If SimpleExp = Nil Then
        SimpleExp := TfsSqlSimpleExpression.Create(Self);
      SimpleExp.Assign(TfsSqlLikeClause(Source).SimpleExp);
      If (EscapeExp = Nil) And (TfsSqlLikeClause(Source).EscapeExp <> Nil) Then
        Begin
          EscapeExp := TfsSqlSimpleExpression.Create(Self);
          EscapeExp.Assign(TfsSqlLikeClause(Source).EscapeExp);
        End;
      Negated := TfsSqlLikeClause(Source).Negated;
    End
  Else
    AssignError(Source);
End;

Function TfsSqlLikeClause.CanLimit: Boolean;
Var
  S: String;
Begin
  Result := False;
  If Not Limited
    And Not IgnoreCase
    And SimpleExp.IsConstant
    And ((EscapeExp = Nil)) Then
    Begin
      S := SimpleExp.GetValue(-1);
      If Not (S[1] In ['%', '_']) Then
        Result := (GetHighLimit <> '');
    End;
End;

Function TfsSqlLikeClause.CanReplaceWithCompare: Boolean;
Var
  S: String;
Begin
  Result := False;
  If Not Limited
    And Not IgnoreCase
    And SimpleExp.IsConstant
    And ((EscapeExp = Nil)) Then
    Begin {!!.11}
      S := SimpleExp.GetValue(-1);
      Result := (fsPosCh('_', S) = 0)
        And (length(S) > 1)
        And (fsPosCh('%', S) = length(S));
    End;
End;

Procedure TfsSqlLikeClause.CheckIsConstant;
Begin
  FIsConstantChecked := True;
  FIsConstant := SimpleExp.IsConstant And ((EscapeExp = Nil) Or EscapeExp.IsConstant);
End;
{--------}

Function TfsSqlLikeClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SimpleExp.DependsOn(Table);
End;

Destructor TfsSqlLikeClause.Destroy;
Begin
  SimpleExp.Free;
  EscapeExp.Free;
  LikePattern.Free;
  If FBmTable <> Nil Then {!!.11}
    Dispose(FBmTable); {!!.11}
  Inherited;
End;
{--------}

Procedure TfsSqlLikeClause.EmitSQL(Stream: TStream);
Begin
  If Negated Then
    WriteStr(Stream, ' NOT');
  WriteStr(Stream, ' LIKE ');
  SimpleExp.EmitSQL(Stream);
  If EscapeExp <> Nil Then
    Begin
      WriteStr(Stream, ' ESCAPE');
      EscapeExp.EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlLikeClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SimpleExp.EnumNodes(EnumMethod, Deep);
  If EscapeExp <> Nil Then
    EscapeExp.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlLikeClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlLikeClause)
    And (Negated = TfsSqlLikeClause(Other).Negated)
    And (SimpleExp.Equals(TfsSqlLikeClause(Other).SimpleExp))
    And (BothNil(EscapeExp, TfsSqlLikeClause(Other).EscapeExp)
    Or (BothNonNil(EscapeExp, TfsSqlLikeClause(Other).EscapeExp)
    And EscapeExp.Equals(TfsSqlLikeClause(Other).EscapeExp)));
End;
{--------}

{!!.11 new}

Function TfsSqlLikeClause.GetBmTable: PBTable;
Var
  S: String;
Begin
  If FBmTable = Nil Then
    Begin
      Assert(IsBMCompatible);
      If IgnoreCase Then {!!.13}
        S := AnsiUpperCase(SimpleExp.GetValue(-1)) {!!.13}
      Else {!!.13}
        S := SimpleExp.GetValue(-1);
      New(FBmTable);
      FBMPhrase := copy(S, 2, length(S) - 2);
      fsBMMakeTableS(FBmPhrase, FBmTable^);
    End;
  Result := FBmTable;
End;

Function TfsSqlLikeClause.GetHighLimit: String;
Var
  i, j: Integer;
Begin
  Result := GetLowLimit;
  i := length(Result);

  If Result[i] In [' '..'~'] Then
    Begin
      If Result[1] In ['z', 'Z'] Then
        Begin
          j := fscl_MaxKeyLength - i;
          For i := 1 To j Do
            Result := Result + OsChar[255]; // max compareindex
        End
      Else
        Begin
          If Result[i] In ['z', 'Z'] Then
            Result := Result + 'z'
          Else
            inc(Result[i]);
        End;
    End
  Else
    Result := '';
End;

Function TfsSqlLikeClause.GetLowLimit: String;
Var
  P: Integer;
Begin
  Result := SimpleExp.GetValue(-1);
  P := 1;
  While (P <= length(Result))
    And Not (Result[P] In ['%', '_']) Do
    inc(P);
  dec(P);
  If P < length(Result) Then
    Result := copy(Result, 1, P);
End;

{!!.11 new}

Procedure TfsSqlLikeClause.CheckBMCompat;
Var
  S: String;
  Len,
    Inx: Integer;
Begin
  FBMCompat := False;
  If SimpleExp.IsConstant And (EscapeExp = Nil) Then
    Begin
      S := SimpleExp.GetValue(-1);
      Len := Length(S);
      FBMCompat := (Len >= 3) And
        (S[1] = '%') And
        (S[Len] = '%');
      { Verify there is not another wildcard character in the middle Of the
      String. }
      For Inx := 2 To Pred(Len) Do
        If S[Inx] = '%' Then
          Begin
            FBMCompat := False;
            Break;
          End;
    End;
  BMCompatChecked := True;
End;

{!!.11 new}

Function TfsSqlLikeClause.IsBMCompatible: Boolean;
Begin
  If Not BMCompatChecked Then
    CheckBMCompat;
  Result := FBMCompat;
End;

Function TfsSqlLikeClause.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Procedure TfsSqlLikeClause.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstSingleChar, fstSingleWideChar,
      fstShortString..fstWideString,
      fstUInt8..fstDateTime, fstRecVersion:
      SimpleExp.MatchType(ExpectedType);
    fstBLOB..fstBLOBGraphic: {!!.11}
      SimpleExp.MatchType(fstNullString); {!!.11}
    Else
      SQLError(Format('The LIKE operator may not be applied to %s fields', {!!.11}
        [FieldDataTypes[ExpectedType]])); {!!.11}
  End;
End;
{--------}

Function TfsSqlLikeClause.Reduce: Boolean;
Begin
  Result := SimpleExp.Reduce Or ((EscapeExp <> Nil) And EscapeExp.Reduce);
End;

Procedure TfsSqlLikeClause.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{====================================================================}

{===TfsSqlInClause===================================================}

Function TfsSqlInClause.AsBoolean(Const TestValue: Variant): Boolean;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.CheckForValue(TestValue)
  Else
    Result := SimpleExpList.Contains(TestValue);
  Result := Result Xor Negated;
End;
{--------}

Procedure TfsSqlInClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlInClause Then
    Begin
      SimpleExpList.Free;
      SimpleExpList := Nil; {!!.12}
      SubQuery.Free;
      SubQuery := Nil; {!!.12}
      If TfsSqlInClause(Source).SubQuery <> Nil Then
        Begin
          SubQuery := TfsSqlSELECT.Create(Self);
          SubQuery.Assign(TfsSqlInClause(Source).SubQuery);
        End
      Else
        Begin
          SimpleExpList := TfsSqlSimpleExpressionList.Create(Self);
          SimpleExpList.Assign(TfsSqlInClause(Source).SimpleExpList);
        End;
      Negated := TfsSqlInClause(Source).Negated;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlInClause.CheckIsConstant;
Begin
  FIsConstantChecked := True;
  If SubQuery <> Nil Then
    FIsConstant := False
  Else
    FIsConstant := SimpleExpList.IsConstant;
End;
{--------}

Function TfsSqlInClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.DependsOn(Table)
  Else
    Result := SimpleExpList.DependsOn(Table);
End;

Destructor TfsSqlInClause.Destroy;
Begin
  SubQuery.Free;
  SimpleExpList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlInClause.EmitSQL(Stream: TStream);
Begin
  If Negated Then
    WriteStr(Stream, ' NOT');
  WriteStr(Stream, ' IN (');
  If SubQuery <> Nil Then
    SubQuery.EmitSQL(Stream)
  Else
    SimpleExpList.EmitSQL(Stream);
  WriteStr(Stream, ') ');
End;
{--------}

Procedure TfsSqlInClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If SubQuery <> Nil Then
    SubQuery.EnumNodes(EnumMethod, Deep)
  Else
    SimpleExpList.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlInClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlInClause)
    And (Negated = TfsSqlInClause(Other).Negated);
  If Result Then
    If SubQuery <> Nil Then
      If TfsSqlInClause(Other).SubQuery = Nil Then
        Result := False
      Else
        Result := SubQuery.Equals(TfsSqlInClause(Other).SubQuery)
    Else If TfsSqlInClause(Other).SimpleExpList = Nil Then
      Result := False
    Else
      Result := SimpleExpList.Equals(TfsSqlInClause(Other).SimpleExpList);
End;
{--------}

Function TfsSqlInClause.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Procedure TfsSqlInClause.MatchType(ExpectedType: TfsFieldType);
Begin
  If SubQuery <> Nil Then
    SubQuery.MatchType(ExpectedType, True)
  Else
    SimpleExpList.MatchType(ExpectedType);
End;
{--------}

Function TfsSqlInClause.Reduce: Boolean;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.Reduce
  Else
    Result := SimpleExpList.Reduce;
End;

Procedure TfsSqlInClause.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{====================================================================}
{$IFDEF IsNoVariantInt64}

Function SimpleCompare(RelOp: TfsSqlRelOp; Const Val1, Val2: Variant): Boolean;
Const
  ValIsBLOBArray: Array[boolean, boolean] Of Byte =
  ((1, { false, false }
    2), { false, True  }
    (3, { true,  False }
    4) { true,  true  }
    );
Var
  VStr: String;
  VPtr1, VPtr2: PAnsiChar;
  Inx, VPtr1Len, VPtr2Len: Integer;
  VPtr1Locked, VPtr2Locked: Boolean;
  ValIsBLOBCase: Byte;
  E: Extended;
Begin
  If VarIsNull(Val1) Or
    VarIsNull(Val2) Then
    Begin
      Result := False;
      Exit;
    End;
  Assert(RelOp <> roNone);

  ValIsBLOBCase := ValIsBLOBArray[VarIsArray(Val1) And
    (TVarData(Val1).VType And VarTypeMask = varByte),
    VarIsArray(Val2) And
    (TVarData(Val2).VType And VarTypeMask = varByte)];
  If ValIsBLOBCase = 1 Then
    Case RelOp Of
      roEQ:
        If (VarType(Val1) And VarTypeMask = VarDate)
          And (VarType(Val2) And VarTypeMask = VarDate) Then
          Result := abs(Double(Val1) - Double(Val2)) < TimeDelta
        Else
          Begin
            Case TVarData(Val1).Vtype Of
              vt_e80:
                Begin
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := decimal(Val1).ext80 = decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        Result := decimal(Val1).ext80 = decimal(Val2).lo64;
                      End;
                    Else
                      Result := decimal(Val1).ext80 = Val2;
                  End;
                End;
              vt_decimal:
                Begin
                  E := decimal(Val1).lo64;
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := decimal(Val1).lo64 = decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        Result := decimal(Val1).lo64 = decimal(Val2).lo64;
                      End;
                    Else
                      Result := E = Val2;
                  End;
                End;
              Else
                Begin
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := Val1 = decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Val2).lo64;
                        Result := Val1 = E;
                      End;
                    Else
                      Result := Val1 = Val2;
                  End;
                End;
            End;
          End;
      roLE:
        Begin
          Case TVarData(Val1).Vtype Of
            vt_e80:
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).ext80 <= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).ext80 <= decimal(Val2).lo64;
                    End;
                  Else
                    Result := decimal(Val1).ext80 <= Val2;
                End;
              End;
            vt_decimal:
              Begin
                E := decimal(Val1).lo64;
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).lo64 <= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).lo64 <= decimal(Val2).lo64;
                    End;
                  Else
                    Result := E <= Val2;
                End;
              End;
            Else
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := Val1 <= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      E := decimal(Val2).lo64;
                      Result := Val1 <= E;
                    End;
                  Else
                    Result := Val1 <= Val2;
                End;
              End;
          End;
        End;
      roL:
        Begin
          Case TVarData(Val1).Vtype Of
            vt_e80:
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).ext80 < decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).ext80 < decimal(Val2).lo64;
                    End;
                  Else
                    Result := decimal(Val1).ext80 < Val2;
                End;
              End;
            vt_decimal:
              Begin
                E := decimal(Val1).lo64;
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).lo64 < decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).lo64 < decimal(Val2).lo64;
                    End;
                  Else
                    Result := E < Val2;
                End;
              End;
            Else
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := Val1 < decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      E := decimal(Val2).lo64;
                      Result := Val1 < E;
                    End;
                  Else
                    Result := Val1 < Val2;
                End;
              End;
          End;
        End;
      roG:
        Begin
          Case TVarData(Val1).Vtype Of
            vt_e80:
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).ext80 > decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).ext80 > decimal(Val2).lo64;
                    End;
                  Else
                    Result := decimal(Val1).ext80 > Val2;
                End;
              End;
            vt_decimal:
              Begin
                E := decimal(Val1).lo64;
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).lo64 > decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).lo64 > decimal(Val2).lo64;
                    End;
                  Else
                    Result := E > Val2;
                End;
              End;
            Else
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := Val1 > decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      E := decimal(Val2).lo64;
                      Result := Val1 > E;
                    End;
                  Else
                    Result := Val1 > Val2;
                End;
              End;
          End;
        End;
      roGE:
        Begin
          Case TVarData(Val1).Vtype Of
            vt_e80:
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).ext80 >= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).ext80 >= decimal(Val2).lo64;
                    End;
                  Else
                    Result := decimal(Val1).ext80 >= Val2;
                End;
              End;
            vt_decimal:
              Begin
                E := decimal(Val1).lo64;
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := decimal(Val1).lo64 >= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      Result := decimal(Val1).lo64 >= decimal(Val2).lo64;
                    End;
                  Else
                    Result := E >= Val2;
                End;
              End;
            Else
              Begin
                Case TVarData(Val2).Vtype Of
                  vt_e80:
                    Begin
                      Result := Val1 >= decimal(Val2).ext80;
                    End;
                  vt_decimal:
                    Begin
                      E := decimal(Val2).lo64;
                      Result := Val1 >= E;
                    End;
                  Else
                    Result := Val1 >= Val2;
                End;
              End;
          End;
        End;
      Else //roNE :
        If (VarType(Val1) And VarTypeMask = VarDate)
          And (VarType(Val2) And VarTypeMask = VarDate) Then
          Result := abs(Double(Val1) - Double(Val2)) >= TimeDelta
        Else
          Begin
            Case TVarData(Val1).Vtype Of
              vt_e80:
                Begin
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := decimal(Val1).ext80 <> decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        Result := decimal(Val1).ext80 <> decimal(Val2).lo64;
                      End;
                    Else
                      Result := decimal(Val1).ext80 <> Val2;
                  End;
                End;
              vt_decimal:
                Begin
                  E := decimal(Val1).lo64;
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := decimal(Val1).lo64 <> decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        Result := decimal(Val1).lo64 <> decimal(Val2).lo64;
                      End;
                    Else
                      Result := E <> Val2;
                  End;
                End;
              Else
                Begin
                  Case TVarData(Val2).Vtype Of
                    vt_e80:
                      Begin
                        Result := Val1 <> decimal(Val2).ext80;
                      End;
                    vt_decimal:
                      Begin
                        E := decimal(Val2).lo64;
                        Result := Val1 <> E;
                      End;
                    Else
                      Result := Val1 <> Val2;
                  End;
                End;
            End;
          End;
    End { Case }
  Else
    Begin
      { One of the parameters is a BLOB. It must be converted to a String.
      This code is kind Of flaky in that it is a duplicate Of the preceding
      section. However, this approach should give us optimal performance for
      cases where neither parameter is a BLOB. }
      VPtr1 := Nil;
      VPtr2 := Nil;
      VPtr1Locked := False;
      VPtr2Locked := False;
      Try
        Case ValIsBLOBCase Of
          2:
            Begin
              VStr := VarToStr(Val1);
              VPtr1 := PAnsiChar(VStr);
              VPtr1Len := Length(VStr);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            End;
          3:
            Begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VStr := VarToStr(Val2);
              VPtr2 := PAnsiChar(VStr);
              VPtr2Len := Length(VStr);
            End;
          4:
            Begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            End;
          Else
            Begin
              VPtr1Len := 0;
              VPtr2Len := 0;
            End;
        End; { Case }
        Inx := Windows.CompareStringA(LOCALE_USER_DEFAULT, 0,
          VPtr1, VPtr1Len, VPtr2, VPtr2Len) - 2;
        Case RelOp Of
          roEQ: Result := (Inx = 0);
          roLE: Result := (Inx <= 0);
          roL: Result := (Inx < 0);
          roG: Result := (Inx > 0);
          roGE: Result := (Inx >= 0);
          Else
            { roNE }
            Result := (Inx <> 0);
        End; { Case }
      Finally
        If VPtr1Locked Then
          VarArrayUnlock(Val1);
        If VPtr2Locked Then
          VarArrayUnlock(Val2);
      End;
    End; { If..else }
End;
{$ELSE}

Function SimpleCompare(RelOp: TfsSqlRelOp; Const Val1, Val2: Variant): Boolean;
Const
  ValIsBLOBArray: Array[boolean, boolean] Of Byte =
  ((1, { false, false }
    2), { false, true  }
    (3, { true,  false }
    4) { true,  true  }
    );
Var
  VStr: String;
  VPtr1, VPtr2: PAnsiChar;
  Inx, VPtr1Len, VPtr2Len: Integer;
  VPtr1Locked, VPtr2Locked: Boolean;
  ValIsBLOBCase: Byte;
Begin
  If VarIsNull(Val1) Or
    VarIsNull(Val2) Then
    Begin
      Result := False;
      Exit;
    End;
  Assert(RelOp <> roNone);

  ValIsBLOBCase := ValIsBLOBArray[VarIsArray(Val1) And
    (TVarData(Val1).VType And VarTypeMask = varByte),
    VarIsArray(Val2) And
    (TVarData(Val2).VType And VarTypeMask = varByte)];
  If ValIsBLOBCase = 1 Then
    Case RelOp Of
      roEQ:
        If (VarType(Val1) And VarTypeMask = VarDate)
          And (VarType(Val2) And VarTypeMask = VarDate) Then
          Result := abs(Double(Val1) - Double(Val2)) < TimeDelta
        Else
          Result := Val1 = Val2;
      roLE:
        Result := Val1 <= Val2;
      roL:
        Result := Val1 < Val2;
      roG:
        Result := Val1 > Val2;
      roGE:
        Result := Val1 >= Val2;
      Else //roNE :
        If (VarType(Val1) And VarTypeMask = VarDate)
          And (VarType(Val2) And VarTypeMask = VarDate) Then
          Result := abs(Double(Val1) - Double(Val2)) >= TimeDelta
        Else
          Result := Val1 <> Val2;
    End { case }
  Else
    Begin
      { One of the parameters is a BLOB. It must be converted to a string.
        This code is kind of flaky in that it is a duplicate of the preceding
        section. However, this approach should give us optimal performance for
        cases where neither parameter is a BLOB. }
      VPtr1 := Nil;
      VPtr2 := Nil;
      VPtr1Locked := False;
      VPtr2Locked := False;
      Try
        Case ValIsBLOBCase Of
          2:
            Begin
              VStr := VarToStr(Val1);
              VPtr1 := PAnsiChar(VStr);
              VPtr1Len := Length(VStr);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            End;
          3:
            Begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VStr := VarToStr(Val2);
              VPtr2 := PAnsiChar(VStr);
              VPtr2Len := Length(VStr);
            End;
          4:
            Begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            End;
          Else
            Begin
              VPtr1Len := 0;
              VPtr2Len := 0;
            End;
        End; { case }
        Inx := Windows.CompareStringA(LOCALE_USER_DEFAULT, 0,
          VPtr1, VPtr1Len, VPtr2, VPtr2Len) - 2;
        Case RelOp Of
          roEQ: Result := (Inx = 0);
          roLE: Result := (Inx <= 0);
          roL: Result := (Inx < 0);
          roG: Result := (Inx > 0);
          roGE: Result := (Inx >= 0);
          Else
            { roNE }
            Result := (Inx <> 0);
        End; { case }
      Finally
        If VPtr1Locked Then
          VarArrayUnlock(Val1);
        If VPtr2Locked Then
          VarArrayUnlock(Val2);
      End;
    End; { if..else }
End;

{$ENDIF}

{===TfsSqlCondPrimary================================================}

Function TfsSqlCondPrimary.AsBoolean: Boolean;
Var
  F: TfsSqlFieldProxy; {!!.11}
  BMTable: PBTable; {!!.13}
Begin
  Result := False;
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  If Not TypeChecked Then
    CheckType;

  If RelOp = roNone Then
    If BetweenClause <> Nil Then
      Result := BetweenClause.AsBoolean(SimpleExp1.GetValue(-1))
    Else If LikeClause <> Nil Then
      If SimpleExp1.IsField(F) And LikeClause.IsBMCompatible Then
        Begin {!!.11} {!!.13}
          {Need to call BMTable before method call - otherwise BMPhrase doesn't get initialized in time}
          BMTable := LikeClause.BMTable;
          Result := F.BMMatch(BMTable^, LikeClause.BMPhrase, LikeClause.IgnoreCase) {!!.11} {!!.13}
        End
      Else {!!.11} {!!.13}
        Result := LikeClause.AsBoolean(SimpleExp1.GetValue(-1))
    Else If InClause <> Nil Then
      Result := InClause.AsBoolean(SimpleExp1.GetValue(-1))
    Else If IsTest <> Nil Then
      Result := IsTest.Evaluate(SimpleExp1)
    Else If ExistsClause <> Nil Then
      Result := ExistsClause.AsBoolean
    Else If UniqueClause <> Nil Then
      Result := UniqueClause.AsBoolean
    Else If MatchClause <> Nil Then
      Result := MatchClause.AsBoolean(SimpleExp1.GetValue(-1))
    Else
      Result := SimpleExp1.GetValue(-1)
  Else If SimpleExp2 <> Nil Then
    Result := SimpleCompare(RelOp, SimpleExp1.GetValue(-1), SimpleExp2.GetValue(-1))
  Else If AllOrAnyClause <> Nil Then
    Result := AllOrAnyClause.Compare(RelOp, SimpleExp1.GetValue(-1))
  Else
    SQLError('Simple expression or ANY/ALL clause expected');
End;
{--------}

Procedure TfsSqlCondPrimary.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlCondPrimary Then
    Begin

      Clear;

      If assigned(TfsSqlCondPrimary(Source).SimpleExp1) Then
        Begin
          SimpleExp1 := TfsSqlSimpleExpression.Create(Self);
          SimpleExp1.Assign(TfsSqlCondPrimary(Source).SimpleExp1);
        End;

      RelOp := TfsSqlCondPrimary(Source).RelOp;

      If assigned(TfsSqlCondPrimary(Source).SimpleExp2) Then
        Begin
          SimpleExp2 := TfsSqlSimpleExpression.Create(Self);
          SimpleExp2.Assign(TfsSqlCondPrimary(Source).SimpleExp2);
        End;

      If assigned(TfsSqlCondPrimary(Source).BetweenClause) Then
        Begin
          BetweenClause := TfsSqlBetweenClause.Create(Self);
          BetweenClause.Assign(TfsSqlCondPrimary(Source).BetweenClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).LikeClause) Then
        Begin
          LikeClause := TfsSqlLikeClause.Create(Self);
          LikeClause.Assign(TfsSqlCondPrimary(Source).LikeClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).InClause) Then
        Begin
          InClause := TfsSqlInClause.Create(Self);
          InClause.Assign(TfsSqlCondPrimary(Source).InClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).IsTest) Then
        Begin
          IsTest := TfsSqlIsTest.Create(Self);
          IsTest.Assign(TfsSqlCondPrimary(Source).IsTest);
        End;

      If assigned(TfsSqlCondPrimary(Source).AllOrAnyClause) Then
        Begin
          AllOrAnyClause := TfsSqlAllOrAnyClause.Create(Self);
          AllOrAnyClause.Assign(TfsSqlCondPrimary(Source).AllOrAnyClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).ExistsClause) Then
        Begin
          ExistsClause := TfsSqlExistsClause.Create(Self);
          ExistsClause.Assign(TfsSqlCondPrimary(Source).ExistsClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).UniqueClause) Then
        Begin
          UniqueClause := TfsSqlUniqueClause.Create(Self);
          UniqueClause.Assign(TfsSqlCondPrimary(Source).UniqueClause);
        End;

      If assigned(TfsSqlCondPrimary(Source).MatchClause) Then
        Begin
          MatchClause := TfsSqlMatchClause.Create(Self);
          MatchClause.Assign(TfsSqlCondPrimary(Source).MatchClause);
        End;

    End
  Else
    AssignError(Source);
End;
{--------}

Procedure TfsSqlCondPrimary.BindHaving;
Begin
  If SimpleExp1 <> Nil Then
    SimpleExp1.BindHaving;
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        SQLError('BETWEEN not supported in a HAVING clause')
      Else If LikeClause <> Nil Then
        SQLError('LIKE not supported in a HAVING clause')
      Else If InClause <> Nil Then
        SQLError('IN not supported in a HAVING clause')
      Else If IsTest <> Nil Then
        If ExistsClause <> Nil Then
          SQLError('EXISTS not supported in a HAVING clause')
        Else If UniqueClause <> Nil Then
          SQLError('UNIQUE not supported in a HAVING clause')
        Else If MatchClause <> Nil Then
          SQLError('MATCH not supported in a HAVING clause');
    Else
      If AllOrAnyClause <> Nil Then
        //SQLError('ANY or ALL conditions not supported in a HAVING clause')
      Else
        Begin
          Assert(SimpleExp2 <> Nil);
          SimpleExp2.BindHaving;
        End;
  End;
End;

Procedure TfsSqlCondPrimary.CheckIsConstant;
Var
  v: Variant;
Begin
  FIsConstantChecked := True;
  FIsConstant := False;
  If SimpleExp1 <> Nil Then
    If Not SimpleExp1.IsConstant Then
      Exit;
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        If Not BetweenClause.IsConstant Then
          Exit
        Else
      Else If LikeClause <> Nil Then
        If Not LikeClause.IsConstant Then
          Exit
        Else
      Else If InClause <> Nil Then
        If Not InClause.IsConstant Then
          Exit
        Else
      Else If IsTest <> Nil Then
        // constant by definition
      Else If ExistsClause <> Nil Then
        Exit
      Else If UniqueClause <> Nil Then
        Exit
      Else If MatchClause <> Nil Then
        Exit;
    Else
      If AllOrAnyClause <> Nil Then
        Exit
      Else
        Begin
          Assert(SimpleExp2 <> Nil);
          If Not SimpleExp2.IsConstant Then
            Exit;
        End;
  End;
  v := getvalue(-1);
  {$IFDEF IsNoVariantInt64}
  If (TVarData(V).VType = VT_DECIMAL) Then
    Begin
      TVarData(ConstantValue).VType := VT_DECIMAL;
      Decimal(ConstantValue).lo64 := Decimal(V).lo64;
    End
  Else If (TVarData(V).VType = VT_E80) Then
    Begin
      TVarData(ConstantValue).VType := VT_E80;
      Decimal(ConstantValue).ext80 := Decimal(V).ext80;
    End
  Else
    ConstantValue := v;
  {$ELSE}
  ConstantValue := v;
  {$ENDIF}
  FIsConstant := True;
End;

Procedure TfsSqlCondPrimary.CheckType;
Var
  T1: TfsFieldType;
Begin
  If SimpleExp1 <> Nil Then
    T1 := SimpleExp1.GetType
  Else
    T1 := fstBLOB; {anything that doesn't match a valid SQL type}
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        BetweenClause.MatchType(T1)
      Else If LikeClause <> Nil Then
        LikeClause.MatchType(T1)
      Else If InClause <> Nil Then
        InClause.MatchType(T1)
      Else If IsTest <> Nil Then
        IsTest.MatchType(T1)
      Else If ExistsClause <> Nil Then
        //T1 := ExistsClause.GetType
      Else If UniqueClause <> Nil Then
        //T1 := UniqueClause.GetType
      Else If MatchClause <> Nil Then
        MatchClause.MatchType(T1);
    //Else
  //  If T1 <> fstBoolean then
    //    TypeMismatch;
    Else
      If AllOrAnyClause <> Nil Then
        AllOrAnyClause.MatchType(T1)
      Else
        Begin
          Assert(SimpleExp2 <> Nil);
          SimpleExp2.MatchType(T1);
        End;
  End;
  TypeChecked := True;
End;
{--------}

Procedure TfsSqlCondPrimary.Clear;
Begin
  SimpleExp1.Free;
  SimpleExp1 := Nil;
  BetweenClause.Free;
  BetweenClause := Nil;
  LikeClause.Free;
  LikeClause := Nil;
  InClause.Free;
  InClause := Nil;
  IsTest.Free;
  IsTest := Nil;
  ExistsClause.Free;
  ExistsClause := Nil;
  UniqueClause.Free;
  UniqueClause := Nil;
  MatchClause.Free;
  MatchClause := Nil;
  AllOrAnyClause.Free;
  AllOrAnyClause := Nil;
  SimpleExp2.Free;
  SimpleExp2 := Nil;
  Inherited;
End;
{--------}

Function TfsSqlCondPrimary.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := False;
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or BetweenClause.DependsOn(Table)
      Else If LikeClause <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or LikeClause.DependsOn(Table)
      Else If InClause <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or InClause.DependsOn(Table)
      Else If IsTest <> Nil Then
        Result := SimpleExp1.DependsOn(Table)
      Else If ExistsClause <> Nil Then
        Result := ExistsClause.DependsOn(Table)
      Else If UniqueClause <> Nil Then
        Result := UniqueClause.DependsOn(Table)
      Else If MatchClause <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or MatchClause.DependsOn(Table)
      Else
        Result := SimpleExp1.DependsOn(Table);
    Else //roEQ, roLE, roL, roG, roGE, roNE :
      If SimpleExp2 <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or SimpleExp2.DependsOn(Table)
      Else If AllOrAnyClause <> Nil Then
        Result := SimpleExp1.DependsOn(Table) Or AllOrAnyClause.DependsOn(Table)
      Else
        SQLError('Simple expression or ANY/ALL clause expected');
  End;
  If AllOrAnyClause <> Nil Then
    Result := Result Or AllOrAnyClause.DependsOn(Table);
End;

Destructor TfsSqlCondPrimary.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlCondPrimary.EmitSQL(Stream: TStream);
Begin
  If SimpleExp1 <> Nil Then
    SimpleExp1.EmitSQL(Stream);
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        BetweenClause.EmitSQL(Stream)
      Else If LikeClause <> Nil Then
        LikeClause.EmitSQL(Stream)
      Else If InClause <> Nil Then
        InClause.EmitSQL(Stream)
      Else If IsTest <> Nil Then
        IsTest.EmitSQL(Stream)
      Else If ExistsClause <> Nil Then
        ExistsClause.EmitSQL(Stream)
      Else If UniqueClause <> Nil Then
        UniqueClause.EmitSQL(Stream)
      Else If MatchClause <> Nil Then
        MatchClause.EmitSQL(Stream);
    Else
      WriteStr(Stream, ' ');
      WriteStr(Stream, fsRelOpStr[RelOp]);
      WriteStr(Stream, ' ');
      If AllOrAnyClause <> Nil Then
        AllOrAnyClause.EmitSQL(Stream)
      Else
        SimpleExp2.EmitSQL(Stream);
  End;
End;
{--------}

Procedure TfsSqlCondPrimary.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If SimpleExp1 <> Nil Then
    SimpleExp1.EnumNodes(EnumMethod, Deep);
  Case RelOp Of
    roNone:
      If BetweenClause <> Nil Then
        BetweenClause.EnumNodes(EnumMethod, Deep)
      Else If LikeClause <> Nil Then
        LikeClause.EnumNodes(EnumMethod, Deep)
      Else If InClause <> Nil Then
        InClause.EnumNodes(EnumMethod, Deep)
      Else If IsTest <> Nil Then
        IsTest.EnumNodes(EnumMethod, Deep)
      Else If MatchClause <> Nil Then
        MatchClause.EnumNodes(EnumMethod, Deep)
      Else If ExistsClause <> Nil Then
        ExistsClause.EnumNodes(EnumMethod, Deep)
      Else If UniqueClause <> Nil Then
        UniqueClause.EnumNodes(EnumMethod, Deep);
    Else
      If SimpleExp2 <> Nil Then
        SimpleExp2.EnumNodes(EnumMethod, Deep)
      Else If AllOrAnyClause <> Nil Then
        AllOrAnyClause.EnumNodes(EnumMethod, Deep);
  End;
End;
{--------}

Function TfsSqlCondPrimary.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlCondPrimary)
    And (RelOp = TfsSqlCondPrimary(Other).RelOp)
    And (
    BothNil(SimpleExp1, TfsSqlCondPrimary(Other).SimpleExp1)
    Or (
    BothNonNil(SimpleExp1, TfsSqlCondPrimary(Other).SimpleExp1)
    And SimpleExp1.Equals(TfsSqlCondPrimary(Other).SimpleExp1)
    )
    )
    And (
    BothNil(SimpleExp2, TfsSqlCondPrimary(Other).SimpleExp2)
    Or (
    BothNonNil(SimpleExp2, TfsSqlCondPrimary(Other).SimpleExp2)
    And SimpleExp2.Equals(TfsSqlCondPrimary(Other).SimpleExp2)
    )
    )
    And (
    BothNil(BetweenClause, TfsSqlCondPrimary(Other).BetweenClause)
    Or (
    BothNonNil(BetweenClause, TfsSqlCondPrimary(Other).BetweenClause)
    And BetweenClause.Equals(TfsSqlCondPrimary(Other).BetweenClause)
    )
    )
    And (
    BothNil(LikeClause, TfsSqlCondPrimary(Other).LikeClause)
    Or (
    BothNonNil(LikeClause, TfsSqlCondPrimary(Other).LikeClause)
    And LikeClause.Equals(TfsSqlCondPrimary(Other).LikeClause)
    )
    )
    And (
    BothNil(InClause, TfsSqlCondPrimary(Other).InClause)
    Or (
    BothNonNil(InClause, TfsSqlCondPrimary(Other).InClause)
    And InClause.Equals(TfsSqlCondPrimary(Other).InClause)
    )
    )
    And (
    BothNil(IsTest, TfsSqlCondPrimary(Other).IsTest)
    Or (
    BothNonNil(IsTest, TfsSqlCondPrimary(Other).IsTest)
    And IsTest.Equals(TfsSqlCondPrimary(Other).IsTest)
    )
    )
    And (
    BothNil(AllOrAnyClause, TfsSqlCondPrimary(Other).AllOrAnyClause)
    Or (
    BothNonNil(AllOrAnyClause, TfsSqlCondPrimary(Other).AllOrAnyClause)
    And AllOrAnyClause.Equals(TfsSqlCondPrimary(Other).AllOrAnyClause)
    )
    )
    And (
    BothNil(ExistsClause, TfsSqlCondPrimary(Other).ExistsClause)
    Or (
    BothNonNil(ExistsClause, TfsSqlCondPrimary(Other).ExistsClause)
    And ExistsClause.Equals(TfsSqlCondPrimary(Other).ExistsClause)
    )
    )
    And (
    BothNil(MatchClause, TfsSqlCondPrimary(Other).MatchClause)
    Or (
    BothNonNil(MatchClause, TfsSqlCondPrimary(Other).MatchClause)
    And MatchClause.Equals(TfsSqlCondPrimary(Other).MatchClause)
    )
    )
    And (
    BothNil(UniqueClause, TfsSqlCondPrimary(Other).UniqueClause)
    Or (
    BothNonNil(UniqueClause, TfsSqlCondPrimary(Other).UniqueClause)
    And UniqueClause.Equals(TfsSqlCondPrimary(Other).UniqueClause)
    )
    );
End;
{--------}

Function TfsSqlCondPrimary.GetDecimals: Integer;
Begin
  If SimpleExp1 <> Nil Then
    Result := SimpleExp1.GetDecimals
  Else
    Result := 0;
End;

Function TfsSqlCondPrimary.GetRound: TRound;
Begin
  If SimpleExp1 <> Nil Then
    Result := SimpleExp1.GetRound
  Else
    Result := rNone;
End;

Function TfsSqlCondPrimary.GetBlobLevel: TDataCompLevel;
Begin
  If SimpleExp1 <> Nil Then
    Result := SimpleExp1.GetBlobLevel
  Else
    Result := blNone;
End;
{--------}

Function TfsSqlCondPrimary.GetSize: Integer;
Begin
  Case RelOp Of
    roNone:
      Result := SimpleExp1.GetSize
    Else
      Result := 1;
  End;
End;
{--------}

Function TfsSqlCondPrimary.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  Case GetType Of
    fstBoolean:
      Result := 'COND'
    Else
      Result := SimpleExp1.GetTitle(Qualified); {!!.11}
  End;
End;

{--------}

Function TfsSqlCondPrimary.GetType: TfsFieldType;
Begin
  If SimpleExp1 <> Nil Then
    Result := SimpleExp1.GetType
  Else
    Result := fstBoolean; {should never happen}
  Case RelOp Of
    roNone:
      If (BetweenClause <> Nil)
        Or (LikeClause <> Nil)
        Or (InClause <> Nil)
        Or (IsTest <> Nil)
        Or (MatchClause <> Nil) Then
        Result := fstBoolean;
    Else
      If SimpleExp2 <> Nil Then
        SimpleExp2.MatchType(Result);
      Result := fstBoolean;
  End;
End;
{--------}

Function TfsSqlCondPrimary.GetValue(aArrayIndex: Integer): Variant;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  Case GetType Of
    fstBoolean:
      Result := AsBoolean
    Else
      Result := SimpleExp1.GetValue(aArrayIndex);
  End;
End;
{--------}

Function TfsSqlCondPrimary.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Function TfsSqlCondPrimary.IsRelationTo(Table: TFSSqlTableProxy;
  Var FieldReferenced: TfsSqlFieldProxy;
  Var Operator: TfsSqlRelOp;
  Var ArgExpression: TfsSqlSimpleExpression;
  Var SameCase: Boolean): Boolean; {!!.10}
Begin
  ArgExpression := Nil;
  Case RelOp Of
    roEQ, roLE, roL, roG, roGE, roNE:
      Begin
        If SimpleExp2 <> Nil Then
          If SimpleExp1.IsFieldFrom(Table, FieldReferenced, SameCase) Then
            Begin
              Result := True;
              ArgExpression := SimpleExp2;
            End
          Else If SimpleExp2.IsFieldFrom(Table, FieldReferenced, SameCase) Then
            Begin
              Result := True;
              ArgExpression := SimpleExp1;
            End
          Else
            Result := False
        Else {typically ANY Or ALL relation}
          Result := False;
      End;
    Else
      Result := False;
  End;
  If AllOrAnyClause <> Nil Then
    Result := False;
  Operator := RelOp;
End;
{--------}

Function TfsSqlCondPrimary.JustSimpleExpression: Boolean;
Begin
  Result := (RelOp = roNone)
    And (BetweenClause = Nil)
    And (LikeClause = Nil)
    And (InClause = Nil)
    And (IsTest = Nil)
    And (ExistsClause = Nil)
    And (UniqueClause = Nil)
    And (MatchClause = Nil);
End;

{!!.11 new}

Procedure TfsSqlCondPrimary.MatchType(ExpectedType: TfsFieldType);
Begin
  Case RelOp Of
    roNone:
      If (BetweenClause <> Nil)
        Or (LikeClause <> Nil)
        Or (InClause <> Nil)
        Or (IsTest <> Nil)
        Or (ExistsClause <> Nil) {!!.11}
      Or (MatchClause <> Nil) Then
        If ExpectedType <> fstBoolean Then
          TypeMismatch
        Else
      Else
        SimpleExp1.MatchType(ExpectedType);
    Else
      If SimpleExp2 <> Nil Then
        Begin
          SimpleExp2.MatchType(SimpleExp1.GetType);
          If ExpectedType <> fstBoolean Then
            TypeMismatch;
        End;
  End;
End;

Function TfsSqlCondPrimary.Reduce: Boolean;
Begin
  Result := True;
  If (SimpleExp1 <> Nil) And SimpleExp1.Reduce Then
    Exit;
  If (SimpleExp2 <> Nil) And SimpleExp2.Reduce Then
    Exit;
  If (BetweenClause <> Nil) And BetweenClause.Reduce Then
    Exit;
  If (LikeClause <> Nil) And LikeClause.Reduce Then
    Exit;
  If (InClause <> Nil) And InClause.Reduce Then
    Exit;
  If (ExistsClause <> Nil) And ExistsClause.Reduce Then
    Exit;
  If (UniqueClause <> Nil) And UniqueClause.Reduce Then
    Exit;
  If (MatchClause <> Nil) And MatchClause.Reduce Then
    Exit;
  If (AllOrAnyClause <> Nil) And AllOrAnyClause.Reduce Then
    Exit;
  Result := False;
End;

Procedure TfsSqlCondPrimary.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

{====================================================================}

{===TfsSqlCondFactor=================================================}

Function TfsSqlCondFactor.AsBoolean: Boolean;
Begin
  If TmpKnown Then
    Begin
      Result := TmpValue;
      Exit;
    End;
  If IsConstant Then
    Begin
      Result := ConstantValue;
      Exit;
    End;
  Result := CondPrimary.AsBoolean;
  If UnaryNot Then
    Result := Not Result;
End;
{--------}

Procedure TfsSqlCondFactor.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlCondFactor Then
    Begin
      If CondPrimary = Nil Then
        CondPrimary := TfsSqlCondPrimary.Create(Self);
      CondPrimary.Assign(TfsSqlCondFactor(Source).CondPrimary);
      UnaryNot := TfsSqlCondFactor(Source).UnaryNot;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlCondFactor.BindHaving;
Begin
  CondPrimary.BindHaving;
End;

Procedure TfsSqlCondFactor.CheckIsConstant;
Var
  v: Variant;
Begin
  FIsConstantChecked := True;
  If CondPrimary.IsConstant Then
    Begin
      v := getvalue(-1);
      {$IFDEF IsNoVariantInt64}
      If (TVarData(V).VType = VT_DECIMAL) Then
        Begin
          TVarData(ConstantValue).VType := VT_DECIMAL;
          Decimal(ConstantValue).lo64 := Decimal(V).lo64;
        End
      Else If (TVarData(V).VType = VT_E80) Then
        Begin
          TVarData(ConstantValue).VType := VT_E80;
          Decimal(ConstantValue).ext80 := Decimal(V).ext80;
        End
      Else
        ConstantValue := v;
      {$ELSE}
      ConstantValue := v;
      {$ENDIF}
      FIsConstant := True;
    End;
End;

Procedure TfsSqlCondFactor.Clear;
Begin
  If CondPrimary <> Nil Then
    CondPrimary.Clear;
End;

Function TfsSqlCondFactor.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := CondPrimary.DependsOn(Table);
End;

Destructor TfsSqlCondFactor.Destroy;
Begin
  CondPrimary.Free;
  Inherited;
End;

Procedure TfsSqlCondFactor.EmitSQL(Stream: TStream);
Begin
  If UnaryNot Then
    WriteStr(Stream, ' NOT');
  CondPrimary.EmitSQL(Stream);
End;
{--------}

Procedure TfsSqlCondFactor.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  CondPrimary.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlCondFactor.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlCondFactor)
    And (UnaryNot = TfsSqlCondFactor(Other).UnaryNot)
    And (CondPrimary.Equals(TfsSqlCondFactor(Other).CondPrimary));
End;
{--------}

Function TfsSqlCondFactor.GetDecimals: Integer;
Begin
  Result := 0;
  If CondPrimary <> Nil Then
    Result := CondPrimary.GetDecimals;
End;

Function TfsSqlCondFactor.GetRound: TRound;
Begin
  Result := RNone;
  If CondPrimary <> Nil Then
    Result := CondPrimary.GetRound;
End;

Function TfsSqlCondFactor.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
  If CondPrimary <> Nil Then
    Result := CondPrimary.GetBlobLevel;
End;
{--------}
{!!.10}

Function TfsSqlCondFactor.GetSize: Integer;
Begin
  If UnaryNot Then
    Result := 1
  Else
    Result := CondPrimary.GetSize;
End;
{--------}

Function TfsSqlCondFactor.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  Result := CondPrimary.GetTitle(Qualified); {!!.11}
End;

{--------}

Function TfsSqlCondFactor.GetType: TfsFieldType;
Begin
  If UnaryNot Then
    Result := fstBoolean
  Else
    Result := CondPrimary.GetType;
End;
{--------}

Function TfsSqlCondFactor.GetValue(aArrayIndex: Integer): Variant;
Begin
  If TmpKnown Then
    Begin
      Result := TmpValue;
      Exit;
    End;
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  If UnaryNot Then
    Result := AsBoolean
  Else
    Result := CondPrimary.GetValue(aArrayIndex);
End;
{--------}

Function TfsSqlCondFactor.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Function TfsSqlCondFactor.IsRelationTo(Table: TFSSqlTableProxy;
  Var FieldReferenced: TfsSqlFieldProxy;
  Var Operator: TfsSqlRelOp;
  Var ArgExpression: TfsSqlSimpleExpression;
  Var SameCase: Boolean): Boolean;
Begin
  ArgExpression := Nil;
  Result := CondPrimary.IsRelationTo(Table, FieldReferenced,
    Operator, ArgExpression, SameCase)
    And Not ArgExpression.DependsOn(Table);
  If Result And UnaryNot Then
    Case Operator Of
      roNone: ;
      roEQ: Operator := roNE;
      roLE: Operator := roG;
      roL: Operator := roGE;
      roG: Operator := roLE;
      roGE: Operator := roL;
      roNE: Operator := roEQ;
    End;
End;
{--------}

Procedure TfsSqlCondFactor.MarkTrue;
Begin
  TmpKnown := True;
  TmpValue := True;
End;
{--------}

Procedure TfsSqlCondFactor.MarkUnknown;
Begin
  TmpKnown := False;
End;
{--------}
{!!.11 - new}

Procedure TfsSqlCondFactor.MatchType(ExpectedType: TfsFieldType);
Begin
  If UnaryNot Then
    If ExpectedType <> fstBoolean Then
      TypeMismatch
    Else
  Else
    CondPrimary.MatchType(ExpectedType);
End;
{--------}

Function TfsSqlCondFactor.Reduce: Boolean;
Var
  LiftPrimary: TfsSqlCondPrimary;
  NewExp: TfsSqlSimpleExpression;
  NewTerm: TfsSqlTerm;
  NewFactor: TfsSqlFactor;
  NewCondExp: TfsSqlCondExp;
  NewCondTerm: TfsSqlCondTerm;
  NewCondFactor: TfsSqlCondFactor;
  NewCondPrimary: TfsSqlCondPrimary;
Begin
  {look For a conditional primary nested inside redundant parens}
    {eliminate parens when found}
  Result := False;
  LiftPrimary := Nil;
  If (CondPrimary.RelOp = roNone) Then
    With CondPrimary Do
      Begin
        //if SimpleExp1 <> nil then Begin
        If JustSimpleExpression Then
          Begin
            With SimpleExp1 Do
              If TermCount = 1 Then
                With Term[0] Do
                  If FactorCount = 1 Then
                    With Factor[0] Do
                      If CondExp <> Nil Then
                        With CondExp Do
                          If CondTermCount = 1 Then
                            With CondTerm[0] Do
                              If CondFactorCount = 1 Then
                                With CondFactor[0] Do
                                  Begin
                                    LiftPrimary := TfsSqlCondPrimary.Create(Self);
                                    LiftPrimary.Assign(CondPrimary);
                                  End;
            If LiftPrimary <> Nil Then
              Begin
                Clear;
                Assign(LiftPrimary);
                LiftPrimary.Free;
                Result := True;
              End
            Else If Reduce Then
              Begin
                {expression itself was reduced}
                Result := True;
              End;
          End;
        If Not Result Then
          Result := Reduce;
      End;
  If Not Result Then
    Begin {otherwise we'll be called again}
      {see If this a negated simple expression which can be reversed}
      If UnaryNot And (CondPrimary.RelOp <> roNone) Then
        Begin
          {it is, reverse condition And remove NOT}
          Case CondPrimary.RelOp Of
            roEQ: CondPrimary.RelOp := roNE;
            roLE: CondPrimary.RelOp := roG;
            roL: CondPrimary.RelOp := roGE;
            roG: CondPrimary.RelOp := roLE;
            roGE: CondPrimary.RelOp := roL;
            roNE: CondPrimary.RelOp := roEQ;
          End;
          UnaryNot := False;
          Result := True;
        End;
    End;
  If Not Result Then {otherwise we'll be called again}
    If (CondPrimary.RelOp = roNE) { "<>" operator }
    {can't optimize ALL/ANY clauses this way}
    And (CondPrimary.AllOrAnyClause = Nil) Then {!!.11}
      If CondPrimary.SimpleExp1.HasFieldRef
        Or CondPrimary.SimpleExp2.HasFieldRef Then
        Begin
          {convert expressions Of the form
          Simple Exp1 <> Simple Exp2
          where at least one expression contains a field reference
          to
          (Simple Exp1 < Simple Exp2 Or Simple Exp1 > Simple Exp2)
        to allow For index optimization later on}
          NewExp := TfsSqlSimpleExpression.Create(CondPrimary);
          NewTerm := TfsSqlTerm.Create(NewExp);
          NewFactor := TfsSqlFactor.Create(NewTerm);
          NewCondExp := TfsSqlCondExp.Create(NewFactor);

          NewCondTerm := TfsSqlCondTerm.Create(NewCondExp);
          NewCondFactor := TfsSqlCondFactor.Create(NewCondTerm);
          NewCondPrimary := TfsSqlCondPrimary.Create(NewCondFactor);
          NewCondPrimary.Assign(CondPrimary);
          NewCondPrimary.RelOp := roL;
          NewCondFactor.CondPrimary := NewCondPrimary;
          NewCondTerm.AddCondFactor(NewCondFactor);
          NewCondExp.AddCondTerm(NewCondTerm);

          NewCondTerm := TfsSqlCondTerm.Create(NewCondExp);
          NewCondFactor := TfsSqlCondFactor.Create(NewCondTerm);
          NewCondPrimary := TfsSqlCondPrimary.Create(NewCondFactor);
          NewCondPrimary.Assign(CondPrimary);
          NewCondPrimary.RelOp := roG;
          NewCondFactor.CondPrimary := NewCondPrimary;
          NewCondTerm.AddCondFactor(NewCondFactor);
          NewCondExp.AddCondTerm(NewCondTerm);

          NewFactor.CondExp := NewCondExp;
          NewTerm.AddFactor(NewFactor);
          NewExp.AddTerm(NewTerm);

          CondPrimary.SimpleExp2.Free;
          CondPrimary.SimpleExp2 := Nil;
          CondPrimary.RelOp := roNone;
          CondPrimary.SimpleExp1.Assign(NewExp);
          NewExp.Free;
          Result := True;
        End;
  If Not Result Then {!!.11}
    Result := CondPrimary.Reduce; {!!.11}
End;
{--------}

Procedure TfsSqlCondFactor.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{====================================================================}

{===TfsSqlFloatLiteral===============================================}

Procedure TfsSqlFloatLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlFloatLiteral Then
    Value := TfsSqlFloatLiteral(Source).Value
  Else
    AssignError(Source);
End;

Procedure TfsSqlFloatLiteral.ConvertToNative;
Var
  Code: Integer;
Begin
  Case GetType Of
    fstSingle:
      Begin
        //Val(Value, SingleValue, Code);
        fsStringToExtended(Value, ExtendedValue, Code);
      End;
    fstDouble:
      Begin
        //Val(Value, DoubleValue, Code);
        fsStringToExtended(Value, ExtendedValue, Code);
      End;
    fstExtended:
      Begin
        fsStringToExtended(Value, ExtendedValue, Code);
      End;
    fstCurrency, fstBcd:
      Begin
        fsStringToCurrency(Value, CurrencyValue, Code);
      End;

  End;
  Converted := Code = 0;
End;

Procedure TfsSqlFloatLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlFloatLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlFloatLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlFloatLiteral)
    And (AnsiCompareText(Value, TfsSqlFloatLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlFloatLiteral.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
End;

Function TfsSqlFloatLiteral.GetRound: TRound;
Begin
  Result := rNone;
End;

Function TfsSqlFloatLiteral.GetDecimals: Integer;
Begin
  Result := 0;
End;
{--------}

Function TfsSqlFloatLiteral.GetType: TfsFieldType;
Begin
  Result := fstExtended;
End;
{--------}

Function TfsSqlFloatLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  Case GetType Of
    fstSingle:
      Begin
        {$IFDEF IsNoVariantInt64}
        TVarData(Result).VType := VT_E80;
        Decimal(Result).Ext80 := ExtendedValue;
        {$ELSE}
        Result := ExtendedValue;
        {$ENDIF}
      End;
    fstDouble:
      Begin
        {$IFDEF IsNoVariantInt64}
        TVarData(Result).VType := VT_E80;
        Decimal(Result).Ext80 := ExtendedValue;
        {$ELSE}
        Result := ExtendedValue;
        {$ENDIF}
      End;
    fstExtended:
      Begin
        {$IFDEF IsNoVariantInt64}
        TVarData(Result).VType := VT_E80;
        Decimal(Result).Ext80 := ExtendedValue;
        {$ELSE}
        Result := ExtendedValue;
        {$ENDIF}
      End;
    fstCurrency, fstBcd:
      Begin
        TVarData(Result).VType := VarCurrency;
        TVarData(Result).VCurrency := CurrencyValue;
      End;
  End;
End;
{--------}

Procedure TfsSqlFloatLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstUInt8..fstAutoInc64, fstRecVersion:
      ;
    fstSingle..fstCurrency, fstBcd:
      ;
    Else
      TypeMismatch;
  End;
End;

{====================================================================}

{===TfsSqlIntegerLiteral=============================================}

Procedure TfsSqlIntegerLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlIntegerLiteral Then
    Begin
      Value := TfsSqlIntegerLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlIntegerLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlIntegerLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlIntegerLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlIntegerLiteral)
    And (AnsiCompareText(Value, TfsSqlIntegerLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlIntegerLiteral.GetType: TfsFieldType;
Begin
  Result := fstInt32;
End;

Procedure TfsSqlIntegerLiteral.ConvertToNative;
Begin
  Int32Value := StrToInt(Value);
  Converted := True;
End;

Function TfsSqlIntegerLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  Result := Int32Value;
End;
{--------}

Procedure TfsSqlIntegerLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstUInt8..fstCurrency, fstRecVersion, fstBcd:
      ;
    fstShortString..fstWideString:
      ;
    Else
      TypeMismatch;
  End;
End;

{====================================================================}

{===TfsSqlInteger64Literal=============================================}

Procedure TfsSqlInteger64Literal.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlInteger64Literal Then
    Begin
      Value := TfsSqlInteger64Literal(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlInteger64Literal.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlInteger64Literal.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlInteger64Literal.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlIntegerLiteral)
    And (AnsiCompareText(Value, TfsSqlInteger64Literal(Other).Value) = 0);
End;
{--------}

Function TfsSqlInteger64Literal.GetType: TfsFieldType;
Begin
  Result := fstInt64;
End;

Procedure TfsSqlInteger64Literal.ConvertToNative;
Begin
  Int64Value := StrToInt64(Value);
  Converted := True;
End;

Function TfsSqlInteger64Literal.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  {$IFDEF IsNoVariantInt64}
  TVarData(Result).VType := VT_DECIMAL;
  Decimal(Result).lo64 := Int64Value;
  {$ELSE}
  Result := Int64Value;
  {$ENDIF}
End;
{--------}

Procedure TfsSqlInteger64Literal.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstUInt8..fstCurrency, fstRecVersion, fstBcd:
      ;
    fstShortString..fstWideString:
      ;
    Else
      TypeMismatch;
  End;
End;

{====================================================================}

{===TfsSqlStringLiteral==============================================}

Procedure TfsSqlStringLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlStringLiteral Then
    Begin
      Value := TfsSqlStringLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlStringLiteral.ConvertToNative;
Var
  S: String;
  P: Integer;
Begin
  S := copy(Value, 2, length(Value) - 2); //strip quotes
  {convert internal Double-quotes to single quotes}
  P := pos('''''', S);
  While P <> 0 Do
    Begin
      Delete(S, P, 1);
      P := pos('''''', S);
    End;
  Assert(GetType In [fstSingleChar, fstSingleWideChar,
    fstShortString..fstWideString, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble]);
  Case GetType Of
    fstSingleChar:
      CharValue := S[1];
    fstSingleWideChar:
      WideCharValue := WideChar(S[1]);
    fstShortString:
      ShortStringValue := S;
    fstNullString, fstVarNullString:
      NullStringValue := PChar(S);
    fstWideString, fstVarWideString {, fstUnicode}:
      WideStringValue := S;
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      NullStringValue := PChar(S);
  End;
  Converted := True;
End;
{--------}

Constructor TfsSqlStringLiteral.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FType := fstNullString;
End;
{--------}

Procedure TfsSqlStringLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlStringLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlStringLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlStringLiteral)
    And (AnsiCompareText(Value, TfsSqlStringLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlStringLiteral.GetSize: Integer;
Begin
  If Not Converted Then
    ConvertToNative;
  Assert(GetType In [fstSingleChar..fstWideString, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble]);
  Case GetType Of
    fstSingleChar:
      Result := 1;
    fstSingleWideChar:
      Result := 2;
    fstShortString:
      Result := length(ShortStringValue);
    fstNullString, fstVarNullString:
      Result := length(NullStringValue);
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      Result := length(NullStringValue);
    Else //fstWideString,fstVarWideString :
      Result := length(WideStringValue);
  End;
End;
{--------}

Function TfsSqlStringLiteral.GetType: TfsFieldType;
Begin
  Result := FType;
End;
{--------}

Function TfsSqlStringLiteral.GetValue(aArrayIndex: Integer): Variant;
Var
  s: String;
Begin
  If Not Converted Then
    ConvertToNative;
  Assert(GetType In [fstSingleChar..fstWideString, fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble]);
  Case GetType Of
    fstSingleChar:
      Result := CharValue;
    fstSingleWideChar:
      Result := WideCharValue;
    fstShortString:
      Result := ShortStringValue;
    fstNullString, fstVarNullString:
      Result := NullStringValue;
    fstWideString, fstVarWideString:
      Result := WideStringValue;
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      Result := NullStringValue;
  End;
End;
{--------}

Procedure TfsSqlStringLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstSingleChar,
      fstSingleWideChar,
      fstShortString..fstWideString,
      fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble,
      fstUInt8..fstdatetime, fstRecVersion:
      Begin
        FType := ExpectedType;
        Converted := False;
      End;
    {Begin !!.11}
    fstBLOB..fstBLOBGraphic:
      Begin
        FType := fstNullString;
        Converted := False;
      End;
    {End !!.11}
    Else
      TypeMismatch;
  End;
End;

{====================================================================}

{===TfsSqlLiteral====================================================}

Procedure TfsSqlLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlLiteral Then
    Begin
      Clear;

      If assigned(TfsSqlLiteral(Source).FloatLiteral) Then
        Begin
          FloatLiteral := TfsSqlFloatLiteral.Create(Self);
          FloatLiteral.Assign(TfsSqlLiteral(Source).FloatLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).IntegerLiteral) Then
        Begin
          IntegerLiteral := TfsSqlIntegerLiteral.Create(Self);
          IntegerLiteral.Assign(TfsSqlLiteral(Source).IntegerLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).Integer64Literal) Then
        Begin
          Integer64Literal := TfsSqlInteger64Literal.Create(Self);
          Integer64Literal.Assign(TfsSqlLiteral(Source).Integer64Literal);
        End;

      If assigned(TfsSqlLiteral(Source).StringLiteral) Then
        Begin
          StringLiteral := TfsSqlStringLiteral.Create(Self);
          StringLiteral.Assign(TfsSqlLiteral(Source).StringLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).DateLiteral) Then
        Begin
          DateLiteral := TfsSqlDateLiteral.Create(Self);
          DateLiteral.Assign(TfsSqlLiteral(Source).DateLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).TimeLiteral) Then
        Begin
          TimeLiteral := TfsSqlTimeLiteral.Create(Self);
          TimeLiteral.Assign(TfsSqlLiteral(Source).TimeLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).TimeStampLiteral) Then
        Begin
          TimeStampLiteral := TfsSqlTimeStampLiteral.Create(Self);
          TimeStampLiteral.Assign(TfsSqlLiteral(Source).TimeStampLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).IntervalLiteral) Then
        Begin
          IntervalLiteral := TfsSqlIntervalLiteral.Create(Self);
          IntervalLiteral.Assign(TfsSqlLiteral(Source).IntervalLiteral);
        End;

      If assigned(TfsSqlLiteral(Source).BooleanLiteral) Then
        Begin
          BooleanLiteral := TfsSqlBooleanLiteral.Create(Self);
          BooleanLiteral.Assign(TfsSqlLiteral(Source).BooleanLiteral);
        End;

    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlLiteral.Clear;
Begin
  FloatLiteral.Free;
  IntegerLiteral.Free;
  Integer64Literal.Free;
  StringLiteral.Free;
  DateLiteral.Free;
  TimeLiteral.Free;
  TimeStampLiteral.Free;
  IntervalLiteral.Free;
  BooleanLiteral.Free;
  FloatLiteral := Nil;
  IntegerLiteral := Nil;
  Integer64Literal := Nil;
  StringLiteral := Nil;
  DateLiteral := Nil;
  TimeLiteral := Nil;
  TimeStampLiteral := Nil;
  IntervalLiteral := Nil;
  BooleanLiteral := Nil;
End;

Destructor TfsSqlLiteral.Destroy;
Begin
  Clear;
  Inherited;
End;
{--------}

Procedure TfsSqlLiteral.EmitSQL(Stream: TStream);
Begin
  If FloatLiteral <> Nil Then
    FloatLiteral.EmitSQL(Stream)
  Else If IntegerLiteral <> Nil Then
    IntegerLiteral.EmitSQL(Stream)
  Else If Integer64Literal <> Nil Then
    Integer64Literal.EmitSQL(Stream)
  Else If StringLiteral <> Nil Then
    StringLiteral.EmitSQL(Stream)
  Else If DateLiteral <> Nil Then
    DateLiteral.EmitSQL(Stream)
  Else If TimeLiteral <> Nil Then
    TimeLiteral.EmitSQL(Stream)
  Else If TimestampLiteral <> Nil Then
    TimestampLiteral.EmitSQL(Stream)
  Else If IntervalLiteral <> Nil Then
    IntervalLiteral.EmitSQL(Stream)
  Else If BooleanLiteral <> Nil Then
    BooleanLiteral.EmitSQL(Stream)
  Else
    Assert(False);
End;
{--------}

Function TfsSqlLiteral.AddIntervalTo(Target: TDateTime): TDateTime;
Begin
  If IntervalLiteral <> Nil Then
    Result := IntervalLiteral.AddIntervalTo(Target)
  Else
    Begin
      SQLError('Internal error: Type Mismatch');
      Result := Null;
    End;
End;
{--------}

Function TfsSqlLiteral.SubtractIntervalFrom(Target: TDateTime): TDateTime;
Begin
  If IntervalLiteral <> Nil Then
    Result := IntervalLiteral.SubtractIntervalFrom(Target)
  Else
    Begin
      SQLError('Internal error: Type Mismatch');
      Result := Null;
    End;
End;
{--------}

Procedure TfsSqlLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If FloatLiteral <> Nil Then
    FloatLiteral.EnumNodes(EnumMethod, Deep)
  Else If IntegerLiteral <> Nil Then
    IntegerLiteral.EnumNodes(EnumMethod, Deep)
  Else If Integer64Literal <> Nil Then
    Integer64Literal.EnumNodes(EnumMethod, Deep)
  Else If StringLiteral <> Nil Then
    StringLiteral.EnumNodes(EnumMethod, Deep)
  Else If DateLiteral <> Nil Then
    DateLiteral.EnumNodes(EnumMethod, Deep)
  Else If TimeLiteral <> Nil Then
    TimeLiteral.EnumNodes(EnumMethod, Deep)
  Else If TimestampLiteral <> Nil Then
    TimestampLiteral.EnumNodes(EnumMethod, Deep)
  Else If IntervalLiteral <> Nil Then
    IntervalLiteral.EnumNodes(EnumMethod, Deep)
  Else If BooleanLiteral <> Nil Then
    BooleanLiteral.EnumNodes(EnumMethod, Deep)
  Else
    Assert(False);
End;
{--------}

Function TfsSqlLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If FloatLiteral <> Nil Then
    Result := FloatLiteral.GetValue(aArrayIndex)
  Else If IntegerLiteral <> Nil Then
    Result := IntegerLiteral.GetValue(aArrayIndex)
  Else If Integer64Literal <> Nil Then
    Result := Integer64Literal.GetValue(aArrayIndex)
  Else If StringLiteral <> Nil Then
    Result := StringLiteral.GetValue(aArrayIndex)
  Else If DateLiteral <> Nil Then
    Result := DateLiteral.GetValue(aArrayIndex)
  Else If TimeLiteral <> Nil Then
    Result := TimeLiteral.GetValue(aArrayIndex)
  Else If TimestampLiteral <> Nil Then
    Result := TimestampLiteral.GetValue(aArrayIndex)
  Else If IntervalLiteral <> Nil Then
    Result := IntervalLiteral.GetValue(aArrayIndex)
  Else If BooleanLiteral <> Nil Then
    Result := BooleanLiteral.GetValue
  Else
    Assert(False);
End;
{--------}

Function TfsSqlLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlLiteral)
    And
    (BothNil(FloatLiteral, TfsSqlLiteral(Other).FloatLiteral)
    Or (BothNonNil(FloatLiteral, TfsSqlLiteral(Other).FloatLiteral)
    And FloatLiteral.Equals(TfsSqlLiteral(Other).FloatLiteral)
    )
    )
    And
    (BothNil(IntegerLiteral, TfsSqlLiteral(Other).IntegerLiteral)
    Or (BothNonNil(IntegerLiteral, TfsSqlLiteral(Other).IntegerLiteral)
    And IntegerLiteral.Equals(TfsSqlLiteral(Other).IntegerLiteral)
    )
    )
    And
    (BothNil(Integer64Literal, TfsSqlLiteral(Other).Integer64Literal)
    Or (BothNonNil(Integer64Literal, TfsSqlLiteral(Other).Integer64Literal)
    And Integer64Literal.Equals(TfsSqlLiteral(Other).Integer64Literal)
    )
    )
    And
    (BothNil(StringLiteral, TfsSqlLiteral(Other).StringLiteral)
    Or (BothNonNil(StringLiteral, TfsSqlLiteral(Other).StringLiteral)
    And StringLiteral.Equals(TfsSqlLiteral(Other).StringLiteral)
    )
    )
    And
    (BothNil(DateLiteral, TfsSqlLiteral(Other).DateLiteral)
    Or (BothNonNil(DateLiteral, TfsSqlLiteral(Other).DateLiteral)
    And DateLiteral.Equals(TfsSqlLiteral(Other).DateLiteral)
    )
    )
    And
    (BothNil(TimeLiteral, TfsSqlLiteral(Other).TimeLiteral)
    Or (BothNonNil(TimeLiteral, TfsSqlLiteral(Other).TimeLiteral)
    And TimeLiteral.Equals(TfsSqlLiteral(Other).TimeLiteral)
    )
    )
    And
    (BothNil(TimestampLiteral, TfsSqlLiteral(Other).TimestampLiteral)
    Or (BothNonNil(TimestampLiteral, TfsSqlLiteral(Other).TimestampLiteral)
    And TimestampLiteral.Equals(TfsSqlLiteral(Other).TimestampLiteral)
    )
    )
    And
    (BothNil(IntervalLiteral, TfsSqlLiteral(Other).IntervalLiteral)
    Or (BothNonNil(IntervalLiteral, TfsSqlLiteral(Other).IntervalLiteral)
    And IntervalLiteral.Equals(TfsSqlLiteral(Other).IntervalLiteral)
    )
    )
    And
    (BothNil(BooleanLiteral, TfsSqlLiteral(Other).BooleanLiteral)
    Or (BothNonNil(BooleanLiteral, TfsSqlLiteral(Other).BooleanLiteral)
    And BooleanLiteral.Equals(TfsSqlLiteral(Other).BooleanLiteral)
    )
    );
End;
{--------}

Function TfsSqlLiteral.GetDecimals: Integer;
Begin
  If FloatLiteral <> Nil Then
    Result := FloatLiteral.GetDecimals
  Else
    Result := 0;
End;

Function TfsSqlLiteral.GetBlobLevel: TDataCompLevel;
Begin
  If StringLiteral <> Nil Then
    Result := StringLiteral.Getbloblevel
  Else
    Result := blNone;
End;
{--------}

Function TfsSqlLiteral.GetSize: Integer;
Begin
  Result := 0;
  If FloatLiteral <> Nil Then
    Result := FloatLiteral.GetSize
  Else If IntegerLiteral <> Nil Then
    Result := IntegerLiteral.GetSize
  Else If Integer64Literal <> Nil Then
    Result := Integer64Literal.GetSize
  Else If StringLiteral <> Nil Then
    Result := StringLiteral.GetSize
  Else If DateLiteral <> Nil Then
    Result := DateLiteral.GetSize
  Else If TimeLiteral <> Nil Then
    Result := TimeLiteral.GetSize
  Else If TimestampLiteral <> Nil Then
    Result := TimestampLiteral.GetSize
  Else If IntervalLiteral <> Nil Then
    Result := IntervalLiteral.GetSize
  Else If BooleanLiteral <> Nil Then
    Result := BooleanLiteral.GetSize
  Else
    Assert(False);
End;
{--------}

Function TfsSqlLiteral.GetType: TfsFieldType;
Begin
  Result := fstInterval; {dummy to suppress compiler warning}
  If FloatLiteral <> Nil Then
    Result := FloatLiteral.GetType
  Else If IntegerLiteral <> Nil Then
    Result := IntegerLiteral.GetType
  Else If Integer64Literal <> Nil Then
    Result := Integer64Literal.GetType
  Else If StringLiteral <> Nil Then
    Result := StringLiteral.GetType
  Else If DateLiteral <> Nil Then
    Result := DateLiteral.GetType
  Else If TimeLiteral <> Nil Then
    Result := TimeLiteral.GetType
  Else If TimestampLiteral <> Nil Then
    Result := TimestampLiteral.GetType
  Else If IntervalLiteral <> Nil Then
    Result := IntervalLiteral.GetType
  Else If BooleanLiteral <> Nil Then
    Result := BooleanLiteral.GetType
  Else
    Assert(False);
End;
{--------}

Function IsValidDate(Const S: ShortString): Boolean;
Begin
  If (length(S) <> 12)
    Or (S[6] <> '-')
    Or (S[9] <> '-') Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 2, 4)),
        StrToInt(copy(S, 7, 2)),
        StrToInt(copy(S, 10, 2)));
      Result := True;
    Except
      Result := False;
    End;
End;

Function IsValidTime(Const S: ShortString): Boolean;
Begin
  If (length(S) <> 10)
    Or (S[4] <> ':')
    Or (S[7] <> ':') Then
    Result := False
  Else
    Try
      EncodeTime(
        StrToInt(copy(S, 2, 2)),
        StrToInt(copy(S, 5, 2)),
        StrToInt(copy(S, 8, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End;
End;

Function IsValidTimestamp(Const S: ShortString): Boolean;
Begin
  If (length(S) < 21)
    Or (S[6] <> '-')
    Or (S[9] <> '-')
    Or (S[12] <> ' ')
    Or (S[15] <> ':')
    Or (S[18] <> ':') Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 2, 4)),
        StrToInt(copy(S, 7, 2)),
        StrToInt(copy(S, 10, 2)));
      EncodeTime(
        StrToInt(copy(S, 13, 2)),
        StrToInt(copy(S, 16, 2)),
        StrToInt(copy(S, 19, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End;
End;

Procedure TfsSqlLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  If FloatLiteral <> Nil Then
    FloatLiteral.MatchType(ExpectedType)
  Else If IntegerLiteral <> Nil Then
    IntegerLiteral.MatchType(ExpectedType)
  Else If Integer64Literal <> Nil Then
    Integer64Literal.MatchType(ExpectedType)
  Else If StringLiteral <> Nil Then
    Case ExpectedType Of
      fstDate, fstTime, fstDateTime:
        Begin
          {String literal, but caller was expecting a Date-type.}
        {See if the String literal represents a valid date.}
          {If it does, convert.}
          If IsValidTimestamp(StringLiteral.Value) Then
            Begin
              TimeStampLiteral := TfsSqlTimestampLiteral.Create(Self);
              TimeStampLiteral.Value := StringLiteral.Value;
              StringLiteral.Free;
              StringLiteral := Nil;
            End
          Else If IsValidDate(StringLiteral.Value) Then
            Begin
              DateLiteral := TfsSqlDateLiteral.Create(Self);
              DateLiteral.Value := StringLiteral.Value;
              StringLiteral.Free;
              StringLiteral := Nil;
            End
          Else If IsValidTime(StringLiteral.Value) Then
            Begin
              TimeLiteral := TfsSqlTimeLiteral.Create(Self);
              TimeLiteral.Value := StringLiteral.Value;
              StringLiteral.Free;
              StringLiteral := Nil;
            End
          Else
            TypeMismatch;
        End;
      Else
        StringLiteral.MatchType(ExpectedType);
    End
  Else If DateLiteral <> Nil Then
    DateLiteral.MatchType(ExpectedType)
  Else If TimeLiteral <> Nil Then
    TimeLiteral.MatchType(ExpectedType)
  Else If TimestampLiteral <> Nil Then
    TimestampLiteral.MatchType(ExpectedType)
  Else If IntervalLiteral <> Nil Then
    IntervalLiteral.MatchType(ExpectedType)
  Else If BooleanLiteral <> Nil Then
    BooleanLiteral.MatchType(ExpectedType)
  Else
    Assert(False);
End;
{====================================================================}

{===TfsSqlParam======================================================}

Procedure TfsSqlParam.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlParam Then
    Begin
      FParmIndex := TfsSqlParam(Source).FParmIndex;
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlParam.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FParmIndex := Owner.ParmCount;
  inc(Owner.ParmCount);
End;
{--------}

Function TfsSqlParam.GetDecimals: Integer;
Begin
  Result := 0;
End;
{--------}

Function TfsSqlParam.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
End;

Function TfsSqlParam.GetSize: Integer;
Begin
  Case GetType Of
    fstWideString: Result := length(GetValue(-1));
    fstShortString: Result := length(GetValue(-1));
    fstBLOB: Result := VarArrayHighBound(GetValue(-1), 1); {!!.13}
    Else
      Result := 0;
  End;
End;
{--------}

Function TfsSqlParam.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  Result := '?';
End;
{--------}

Function TfsSqlParam.GetType: TfsFieldType;
Var
  V: Variant;
Begin
  Result := fstInterval; {dummy to suppress compiler warning}
  V := Owner.ParmList.GetValue(ParmIndex);
  Case VarType(V) And VarTypeMask Of
    varSmallint: Result := fstInt32;
    varInteger: Result := fstInt32;
    varSingle: Result := fstSingle;
    varDouble: Result := fstDouble;
    varCurrency: Result := fstCurrency; //fstBinaryDecimals;
    varDate: Result := fstDateTime;
    varOleStr: Result := fstWideString;
    varBoolean: Result := fstBoolean;
    varString: Result := fstShortString;
    varByte: Result := fstBLOB;
    {$IFDEF IsNoVariantInt64}
    15: Result := fstExtended;
    14: Result := fstInt64;
    {$ELSE}
    varInt64: Result := fstInt64;
    {$ENDIF}

    Else
      SQLError('Unsupported parameter type:' + IntToHex(VarType(V), 0));
  End;
End;
{--------}

Procedure TfsSqlParam.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ?');
End;
{--------}

Procedure TfsSqlParam.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlParam.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlParam);
End;
{--------}

Function TfsSqlParam.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Owner.ParmList = Nil Then
    Raise Exception.Create('No parameter values specified For query. ' +
      'Verify the parameters listed in the ' +
      'TfsQuery.Params Property Matches the ' +
      'parameters specified in the TfsQuery.SQL ' +
      'property.');
  Result := Owner.ParmList.GetValue(ParmIndex);
End;
{--------}

Procedure TfsSqlParam.MatchType(ExpectedType: TfsFieldType);
Begin
End;
{====================================================================}

{===TfsSqlFactor=====================================================}

Function TfsSqlFactor.AddIntervalTo(Target: TDateTime): TDateTime;
Begin
  If Literal <> Nil Then
    Result := Literal.AddIntervalTo(Target)
  Else
    Begin
      SQLError('Not implemented');
      Result := Null;
    End;
End;
{--------}

Function TfsSqlFactor.SubtractIntervalFrom(Target: TDateTime): TDateTime;
Begin
  If Literal <> Nil Then
    Result := Literal.SubtractIntervalFrom(Target)
  Else
    Begin
      SQLError('Not implemented');
      Result := Null;
    End;
End;
{--------}

Procedure TfsSqlFactor.CheckIsConstant;
Var
  v: variant;
Begin
  FIsConstantChecked := True;
  If SubQuery <> Nil Then
    FIsConstant := False
  Else If CondExp <> Nil Then
    FIsConstant := CondExp.IsConstant
  Else If FieldRef <> Nil Then
    FIsConstant := False
  Else If Literal <> Nil Then
    FIsConstant := {True} Literal.IntervalLiteral = Nil
    {can't store interval values, so we can't handle those
  as constant values even If they are in fact constant}
  Else If Param <> Nil Then
    FIsConstant := False
  Else If Aggregate <> Nil Then
    FIsConstant := False
  Else If ScalarFunc <> Nil Then
    FIsConstant := ScalarFunc.IsConstant
  Else
    Assert(False);
  If FIsConstant Then
    Begin
      FIsConstant := False;
      v := getvalue(-1);
      {$IFDEF IsNoVariantInt64}
      If (TVarData(V).VType = VT_DECIMAL) Then
        Begin
          TVarData(ConstantValue).VType := VT_DECIMAL;
          Decimal(ConstantValue).lo64 := Decimal(V).lo64;
        End
      Else If (TVarData(V).VType = VT_E80) Then
        Begin
          TVarData(ConstantValue).VType := VT_E80;
          Decimal(ConstantValue).ext80 := Decimal(V).ext80;
        End
      Else
        ConstantValue := v;
      {$ELSE}
      ConstantValue := v;
      {$ENDIF}
      FIsConstant := True;
    End;
End;
{--------}

Procedure TfsSqlFactor.CheckType;
Begin
  If SubQuery <> Nil Then
    FType := SubQuery.GetType
  Else If CondExp <> Nil Then
    FType := CondExp.GetType
  Else If FieldRef <> Nil Then
    FType := FieldRef.GetType
  Else If Literal <> Nil Then
    FType := Literal.GetType
  Else If Param <> Nil Then
    FType := Param.GetType
  Else If Aggregate <> Nil Then
    FType := Aggregate.GetType
  Else If ScalarFunc <> Nil Then
    FType := ScalarFunc.GetType
  Else
    Assert(False);
  If UnaryMinus Then
    Case FType Of
      fstUInt8,
        fstUInt16,
        fstUInt32,
        fstInt8,
        fstInt16,
        fstInt32,
        fstAutoInc32,
        fstAutoInc64,
        fstSingle,
        fstDouble,
        fstExtended,
        fstInt64,
        fstRecVersion,
        fstCurrency, fstBcd:
        ;
      Else
        SQLError('Operator/operand mismatch');
    End;
  TypeKnown := True;
End;
{--------}

Procedure TfsSqlFactor.Clear;
Begin
  SubQuery.Free;
  CondExp.Free;
  FieldRef.Free;
  Literal.Free;
  Param.Free;
  Aggregate.Free;
  ScalarFunc.Free;
  SubQuery := Nil;
  CondExp := Nil;
  FieldRef := Nil;
  Literal := Nil;
  Param := Nil;
  Aggregate := Nil;
  ScalarFunc := Nil;
End;
{--------}

Function TfsSqlFactor.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.DependsOn(Table)
  Else If CondExp <> Nil Then
    Result := CondExp.DependsOn(Table)
  Else If FieldRef <> Nil Then
    Result := FieldRef.DependsOn(Table)
  Else If Literal <> Nil Then
    Result := False
  Else If Param <> Nil Then
    Result := False
  Else If Aggregate <> Nil Then
    Result := Aggregate.DependsOn(Table)
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.DependsOn(Table)
  Else
    Begin
      Assert(False);
      Result := False;
    End;
End;
{--------}

Destructor TfsSqlFactor.Destroy;
Begin
  Clear;
  Inherited;
End;
{--------}

Procedure TfsSqlFactor.EmitSQL(Stream: TStream);
Begin
  If UnaryMinus Then
    WriteStr(Stream, ' - ');
  If SubQuery <> Nil Then
    Begin
      WriteStr(Stream, ' (');
      SubQuery.EmitSQL(Stream);
      WriteStr(Stream, ')');
    End
  Else If CondExp <> Nil Then
    Begin
      WriteStr(Stream, ' (');
      CondExp.EmitSQL(Stream);
      WRiteStr(Stream, ')');
    End
  Else If FieldRef <> Nil Then
    FieldRef.EmitSQL(Stream)
  Else If Literal <> Nil Then
    Literal.EmitSQL(Stream)
  Else If Param <> Nil Then
    Param.EmitSQL(Stream)
  Else If Aggregate <> Nil Then
    Aggregate.EmitSQL(Stream)
  Else If ScalarFunc <> Nil Then
    ScalarFunc.EmitSQL(Stream)
  Else
    Assert(False);
End;
{--------}

Procedure TfsSqlFactor.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If SubQuery <> Nil Then
    SubQuery.EnumNodes(EnumMethod, Deep)
  Else If CondExp <> Nil Then
    CondExp.EnumNodes(EnumMethod, Deep)
  Else If FieldRef <> Nil Then
    FieldRef.EnumNodes(EnumMethod, Deep)
  Else If Literal <> Nil Then
    Literal.EnumNodes(EnumMethod, Deep)
  Else If Param <> Nil Then
    Param.EnumNodes(EnumMethod, Deep)
  Else If ScalarFunc <> Nil Then
    ScalarFunc.EnumNodes(EnumMethod, Deep)
  Else If Aggregate <> Nil Then
    Aggregate.EnumNodes(EnumMethod, Deep)
  Else
    Assert(False);
End;
{--------}

Function TfsSqlFactor.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlFactor)
    And (MulOp = TfsSqlFactor(Other).MulOp)
    And (UnaryMinus = TfsSqlFactor(Other).UnaryMinus)
    And
    (BothNil(CondExp, TfsSqlFactor(Other).CondExp)
    Or (
    BothNonNil(CondExp, TfsSqlFactor(Other).CondExp)
    And CondExp.Equals(TfsSqlFactor(Other).CondExp)
    )
    )
    And
    (BothNil(FieldRef, TfsSqlFactor(Other).FieldRef)
    Or (
    BothNonNil(FieldRef, TfsSqlFactor(Other).FieldRef)
    And FieldRef.Equals(TfsSqlFactor(Other).FieldRef)
    )
    )
    And
    (BothNil(Literal, TfsSqlFactor(Other).Literal)
    Or (
    BothNonNil(Literal, TfsSqlFactor(Other).Literal)
    And Literal.Equals(TfsSqlFactor(Other).Literal)
    )
    )
    And
    (BothNil(Param, TfsSqlFactor(Other).Param)
    Or (
    BothNonNil(Param, TfsSqlFactor(Other).Param)
    And Param.Equals(TfsSqlFactor(Other).Param)
    )
    )
    And
    (BothNil(Aggregate, TfsSqlFactor(Other).Aggregate)
    Or (
    BothNonNil(Aggregate, TfsSqlFactor(Other).Aggregate)
    And Aggregate.Equals(TfsSqlFactor(Other).Aggregate)
    )
    )
    And
    (BothNil(SubQuery, TfsSqlFactor(Other).SubQuery)
    Or (
    BothNonNil(SubQuery, TfsSqlFactor(Other).SubQuery)
    And SubQuery.Equals(TfsSqlFactor(Other).SubQuery)
    )
    )
    And
    (BothNil(ScalarFunc, TfsSqlFactor(Other).ScalarFunc)
    Or (
    BothNonNil(ScalarFunc, TfsSqlFactor(Other).ScalarFunc)
    And ScalarFunc.Equals(TfsSqlFactor(Other).ScalarFunc)
    )
    );
End;
{--------}

Function TfsSqlFactor.GetRound: TRound;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.GetRound
  Else If CondExp <> Nil Then
    Result := CondExp.GetRound
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetRound
  Else If Literal <> Nil Then
    Result := Literal.GetRound
  Else If Param <> Nil Then
    Result := Param.GetRound
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetRound
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetRound
  Else
    Begin
      Assert(False);
      Result := rnone;
    End;
End;

Function TfsSqlFactor.GetDecimals: Integer;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.GetDecimals
  Else If CondExp <> Nil Then
    Result := CondExp.GetDecimals
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetDecimals
  Else If Literal <> Nil Then
    Result := Literal.GetDecimals
  Else If Param <> Nil Then
    Result := Param.GetDecimals
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetDecimals
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetDecimals
  Else
    Begin
      Assert(False);
      Result := 0;
    End;
End;

Function TfsSqlFactor.GetBlobLevel: TDataCompLevel;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.GetBlobLevel
  Else If CondExp <> Nil Then
    Result := CondExp.GetBlobLevel
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetBlobLevel
  Else If Literal <> Nil Then
    Result := Literal.GetBlobLevel
  Else If Param <> Nil Then
    Result := Param.GetBlobLevel
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetBlobLevel
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetBlobLevel
  Else
    Begin
      Assert(False);
      Result := blNone;
    End;
End;
{--------}

Function TfsSqlFactor.GetSize: Integer;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.GetSize
  Else If CondExp <> Nil Then
    Result := CondExp.GetSize
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetSize
  Else If Literal <> Nil Then
    Result := Literal.GetSize
  Else If Param <> Nil Then
    Result := Param.GetSize
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetSize
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetSize
  Else
    Begin
      Assert(False);
      Result := 0;
    End;
End;
{--------}

Function TfsSqlFactor.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  If SubQuery <> Nil Then
    Result := 'SUB'
  Else If CondExp <> Nil Then
    Result := CondExp.GetTitle(Qualified) {!!.11}
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetTitle(Qualified) {!!.11}
  Else If Literal <> Nil Then
    Result := 'LIT'
  Else If Param <> Nil Then
    Result := Param.GetTitle(Qualified) {!!.11}
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetTitle(Qualified) {!!.11}
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetTitle(Qualified) {!!.11}
  Else
    Assert(False);
End;
{--------}

Function TfsSqlFactor.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    CheckType;
  Result := FType
End;
{--------}

Procedure TfsSqlFactor.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlFactor Then
    Begin
      Clear;

      MulOp := TfsSqlFactor(Source).MulOp;

      UnaryMinus := TfsSqlFactor(Source).UnaryMinus;

      If assigned(TfsSqlFactor(Source).CondExp) Then
        Begin
          CondExp := TfsSqlCondExp.Create(Self);
          CondExp.Assign(TfsSqlFactor(Source).CondExp);
        End;

      If assigned(TfsSqlFactor(Source).FieldRef) Then
        Begin
          FieldRef := TfsSqlFieldRef.Create(Self);
          FieldRef.Assign(TfsSqlFactor(Source).FieldRef);
        End;

      If assigned(TfsSqlFactor(Source).Literal) Then
        Begin
          Literal := TfsSqlLiteral.Create(Self);
          Literal.Assign(TfsSqlFactor(Source).Literal);
        End;

      If assigned(TfsSqlFactor(Source).Param) Then
        Begin
          Param := TfsSqlParam.Create(Self);
          Param.Assign(TfsSqlFactor(Source).Param);
        End;

      If assigned(TfsSqlFactor(Source).Aggregate) Then
        Begin
          Aggregate := TfsSqlAggregate.Create(Self);
          Aggregate.Assign(TfsSqlFactor(Source).Aggregate);
        End;

      If assigned(TfsSqlFactor(Source).SubQuery) Then
        Begin
          SubQuery := TfsSqlSELECT.Create(Self);
          SubQuery.Assign(TfsSqlFactor(Source).SubQuery);
        End;

      If assigned(TfsSqlFactor(Source).ScalarFunc) Then
        Begin
          ScalarFunc := TfsSqlScalarFunc.Create(Self);
          ScalarFunc.Assign(TfsSqlFactor(Source).ScalarFunc);
        End;

    End
  Else
    AssignError(Source);
End;
{--------}

Function TfsSqlFactor.GetValue(aArrayIndex: Integer): Variant;
Var
  E: Extended;
  I: Int64;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;

  If SubQuery <> Nil Then
    Result := SubQuery.GetValue(aArrayIndex)
  Else If CondExp <> Nil Then
    Result := CondExp.GetValue(aArrayIndex)
  Else If FieldRef <> Nil Then
    Result := FieldRef.GetValue(aArrayIndex)
  Else If Literal <> Nil Then
    Result := Literal.GetValue(aArrayIndex)
  Else If Param <> Nil Then
    Result := Param.GetValue(aArrayIndex)
  Else If Aggregate <> Nil Then
    Result := Aggregate.GetAggregateValue
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.GetValue(aArrayIndex)
  Else
    Assert(False);
  If UnaryMinus Then
    If Not VarIsNull(Result) Then
      Begin
        {$IFDEF IsNoVariantInt64}
        If (TVarData(Result).VType = VT_DECIMAL) Then
          Begin
            I := Decimal(Result).lo64;
            TVarData(Result).VType := VT_DECIMAL;
            Decimal(Result).lo64 := -I;
          End
        Else If (TVarData(Result).VType = VT_E80) Then
          Begin
            E := Decimal(Result).ext80;
            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := -E;
          End
        Else
          Result := -Result;
        {$ELSE}
        Result := -Result;
        {$ENDIF}
      End;
End;
{--------}

Function TfsSqlFactor.HasFieldRef: Boolean;
Begin
  Result := (FieldRef <> Nil);
End;
{--------}

Function TfsSqlFactor.IsField(Var FieldReferenced: TfsSqlFieldProxy): Boolean;
Begin
  Result := (FieldRef <> Nil) And Not UnaryMinus;
  If Result Then
    FieldReferenced := FieldRef.Field;
End;
{--------}

Function TfsSqlFactor.IsFieldFrom(Table: TFSSqlTableProxy;
  Var FieldReferenced: TfsSqlFieldProxy; Var SameCase: Boolean): Boolean;
Begin
  Result := (FieldRef <> Nil) And
    (FieldRef.Field <> Nil) And
    (FieldRef.Field.OwnerTable = Table);
  If Result Then
    Begin
      FieldReferenced := FieldRef.Field;
      SameCase := True;
    End
  Else If ScalarFunc <> Nil Then
    Begin
      Result := ScalarFunc.IsFieldFrom(Table, FieldReferenced);
      SameCase := False;
    End;
End;
{--------}

Function TfsSqlFactor.IsNull: Boolean;
Begin
  If FieldRef <> Nil Then
    Result := FieldRef.IsNull
  Else
    Result := VarIsNull(GetValue(-1));
End;
{--------}

Function TfsSqlFactor.IsAggregate: Boolean;
Begin
  Result := Aggregate <> Nil;
End;
{--------}

Function TfsSqlFactor.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Procedure TfsSqlFactor.MatchType(ExpectedType: TfsFieldType);
Begin
  If SubQuery <> Nil Then
    SubQuery.MatchType(ExpectedType, True)
  Else If CondExp <> Nil Then
    CondExp.MatchType(ExpectedType)
  Else If FieldRef <> Nil Then
    FieldRef.MatchType(ExpectedType)
  Else If Literal <> Nil Then
    Literal.MatchType(ExpectedType)
  Else If Param <> Nil Then
    Param.MatchType(ExpectedType)
  Else If Aggregate <> Nil Then
    Aggregate.MatchType(ExpectedType)
  Else If ScalarFunc <> Nil Then
    ScalarFunc.MatchType(ExpectedType)
  Else
    Assert(False);
End;
{--------}

Function TfsSqlFactor.Reduce: Boolean;
Var
  LiftFactor: TfsSqlFactor;
Begin
  If SubQuery <> Nil Then
    Result := SubQuery.Reduce
  Else If CondExp <> Nil Then
    Begin
      {!!.11 begin}
      {if conditional expression is nothing but a parenthesized factor,
      lift it to this level}
      LiftFactor := Nil;
      If CondExp.CondTermCount = 1 Then
        With CondExp.CondTerm[0] Do
          If CondFactorCount = 1 Then
            With CondFactor[0] Do
              If Not UnaryNot Then
                With CondPrimary Do
                  If (RelOp = roNone) And (SimpleExp2 = Nil) Then
                    With SimpleExp1 Do
                      If TermCount = 1 Then
                        With Term[0] Do
                          If FactorCount = 1 Then
                            Begin
                              LiftFactor := TfsSqlFactor.Create(Parent);
                              LiftFactor.Assign(Factor[0]);
                              LiftFactor.MulOp := MulOp; {!!.13}
                            End;
      If LiftFactor <> Nil Then
        Begin
          CondExp.Free;
          CondExp := Nil;
          Assign(LiftFactor);
          LiftFactor.Free;
          Result := True;
        End
      Else
        {!!.11 end}
        Result := CondExp.Reduce
    End
  Else If FieldRef <> Nil Then
    Result := False
  Else If Literal <> Nil Then
    Result := False
  Else If Param <> Nil Then
    Result := False
  Else If Aggregate <> Nil Then
    Result := Aggregate.Reduce
  Else If ScalarFunc <> Nil Then
    Result := ScalarFunc.Reduce
  Else
    Result := False;
End;
{--------}

Procedure TfsSqlFactor.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;
{Begin !!.11}
{--------}

Function TfsSqlFactor.WasWildcard: Boolean;
Begin
  If FieldRef <> Nil Then
    Result := FieldRef.WasWildcard
  Else
    Result := False;
End;
{End !!.11}
{====================================================================}

{===TfsSqlSelection==================================================}

Procedure TfsSqlSelection.AddColumnDef(Target: TfsSqlColumnListOwner);
{Rewritten !!.11}
Var
  S, SQual: String;
  F: TfsSqlNode;
  i: Integer;
Begin
  If Column <> Nil Then
    S := Column.ColumnName
  Else
    S := '';
  F := SimpleExpression;
  If S = '' Then
    S := SimpleExpression.GetTitle(False);

  If Target.Columns.IndexOf(S) <> -1 Then
    Begin
      { See if we can use the qualified column name. This is done For the sake
        of backwards compatibility With existing SQL statements in FF clients. }
      SQual := SimpleExpression.GetTitle(True);
      If Target.Columns.IndexOf(SQual) = -1 Then
        Target.Columns.AddObject(SQual, F)
      Else
        Begin
          i := 1;
          Repeat
            inc(i);
          Until Target.Columns.IndexOf(S + '_' + IntToStr(i)) = -1;
          Target.Columns.AddObject(S + '_' + IntToStr(i), F);
        End;
    End
  Else
    Target.Columns.AddObject(S, F);
End;

Procedure TfsSqlSelection.AddDistinctColumnDef(Target: TfsSqlColumnListOwner);
{Rewritten !!.11}
Var
  S, SQual: String;
  F: TfsSqlNode;
  i: Integer;
Begin
  If Column <> Nil Then
    S := Column.ColumnName
  Else
    S := '';
  F := SimpleExpression;
  If S = '' Then
    S := SimpleExpression.GetTitle(False);

  If Target.DistinctColumns.IndexOf(S) <> -1 Then
    Begin
      { See if we can use the qualified column name. This is done For the sake
        of backwards compatibility With existing SQL statements in FF clients. }
      SQual := SimpleExpression.GetTitle(True);
      If Target.DistinctColumns.IndexOf(SQual) = -1 Then
        Target.DistinctColumns.AddObject(SQual, F)
      Else
        Begin
          i := 1;
          Repeat
            inc(i);
          Until Target.DistinctColumns.IndexOf(S + '_' + IntToStr(i)) = -1;
          Target.DistinctColumns.AddObject(S + '_' + IntToStr(i), F);
        End;
    End
  Else
    Target.DistinctColumns.AddObject(S, F);
End;
{--------}

Procedure TfsSqlSelection.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlSelection Then
    Begin
      SimpleExpression.Free;
      SimpleExpression := TfsSqlSimpleExpression.Create(Self);
      SimpleExpression.Assign(TfsSqlSelection(Source).SimpleExpression);
      Column.Free;
      Column := Nil;
      If assigned(TfsSqlSelection(Source).Column) Then
        Begin
          Column := TfsSqlColumn.Create(Self);
          Column.Assign(TfsSqlSelection(Source).Column);
        End;
    End
  Else
    AssignError(Source);
End;

Destructor TfsSqlSelection.Destroy;
Begin
  SimpleExpression.Free;
  Column.Free;
  Inherited;
End;

Procedure TfsSqlSelection.EmitSQL(Stream: TStream);
Begin
  SimpleExpression.EmitSQL(Stream);
  If Column <> Nil Then
    Begin
      WriteStr(Stream, ' AS');
      Column.EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlSelection.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SimpleExpression.EnumNodes(EnumMethod, Deep);
  If Column <> Nil Then
    Column.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlSelection.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlSelection)
    And (
    BothNil(SimpleExpression, TfsSqlSelection(Other).SimpleExpression)
    Or (BothNonNil(SimpleExpression, TfsSqlSelection(Other).SimpleExpression)
    And SimpleExpression.Equals(TfsSqlSelection(Other).SimpleExpression)
    )
    )
    And (
    BothNil(Column, TfsSqlSelection(Other).Column)
    Or (BothNonNil(Column, TfsSqlSelection(Other).Column)
    And Column.Equals(TfsSqlSelection(Other).Column)
    )
    );
End;
{--------}

Function TfsSqlSelection.GetIndex: Integer;
Begin
  Result := TfsSqlSelectionList(Parent).FSelections.IndexOf(Self);
End;

{--------}

Function TfsSqlSelection.IsAggregateExpression: Boolean;
Begin
  Result := SimpleExpression.IsAggregateExpression;
End;

Function TfsSqlSelection.Reduce: Boolean;
Begin
  Result := SimpleExpression.Reduce;
End;

{====================================================================}

{===TfsSqlTableRef===================================================}

Procedure TfsSqlTableRef.AddTableReference(Select: TfsSqlSELECT);
Var
  IX, I: Integer;
Begin
  IX := -1;
  Assert(Assigned(Select.TablesReferencedByOrder));
  If TableName <> '' Then
    Begin
      If DataBaseName <> '' Then
        If Not SameText(DataBaseName, Owner.FDatabase.Alias) Then
          SQLError(format('The referenced database name %s does not ' +
            'match the current database, %s.',
            [DataBaseName, Owner.FDatabase.Alias]));
      IX := Select.TablesReferencedByOrder.Add(TableName)
    End
  Else
    Begin
      Assert(Assigned(TableExp));
      TableExp.EnsureResultTable(True);
      If Select.TablesReferencedByOrder.IndexOf('$$UNNAMED') = -1 Then
        IX := Select.TablesReferencedByOrder.AddObject('$$UNNAMED',
          TableExp.ResultTable)
      Else
        Begin
          I := 2;
          Repeat
            If Select.TablesReferencedByOrder.IndexOf('$$UNNAMED_' + IntToStr(I)) =
              -1 Then
              Begin
                IX := Select.TablesReferencedByOrder.AddObject('$$UNNAMED_' +
                  IntToStr(I), TableExp.ResultTable);
                break;
              End;
            inc(I);
          Until False;
        End;
    End;
  If Alias <> '' Then
    Begin
      Assert(Assigned(Select.TableAliases));
      If Select.TableAliases.IndexOf(Alias) <> -1 Then
        SQLError('Duplicate alias definition:' + Alias);
      Select.TableAliases.AddObject(Alias, TObject(IX));
    End;
End;
{--------}

Procedure TfsSqlTableRef.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlTableRef Then
    Begin
      Clear;
      TableName := TfsSqlTableRef(Source).TableName;
      Alias := TfsSqlTableRef(Source).Alias;
      If TfsSqlTableRef(Source).TableExp <> Nil Then
        Begin
          TableExp := TfsSqlTableExp.Create(Self);
          TableExp.Assign(TfsSqlTableRef(Source).TableExp);
        End;
      If TfsSqlTableRef(Source).ColumnList <> Nil Then
        Begin
          ColumnList := TfsSqlInsertColumnList.Create(Self);
          ColumnList.Assign(TfsSqlTableRef(Source).ColumnList);
        End;
    End
  Else
    AssignError(Source);
End;

Function TfsSqlTableRef.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
{- not used For binding directly from SELECT - only fOr
  binding to contained sub-expressions}
Begin
  If TableExp <> Nil Then
    Result := TableExp.BindFieldDown(TableName, FieldName)
  Else If SameText(TableName, Self.TableName)
    And (Alias = '') {can't bind to table name If alias present} {!!.12}
  Or SameText(TableName, Alias) Then
    Result := ResultTable.FieldByName(FieldName)
  Else
    Result := Nil;
End;

Function TfsSqlTableRef.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  If SameText(TableName, Alias) Or SameText(TableName, Self.TableName) Then
    Result := GetTable(AOwner, omReadOnly, False)
  Else If TableExp <> Nil Then
    Result := TableExp.BindTable(AOwner, TableName)
  Else
    Result := Nil;
End;

Procedure TfsSqlTableRef.Clear;
Begin
  TableName := '';
  Alias := '';
  TableExp.Free;
  TableExp := Nil;
  ColumnList.Free;
  ColumnList := Nil;
End;

Function TfsSqlTableRef.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If TableExp <> Nil Then
    Result := TableExp.DependsOn(Table)
  Else
    Result := False;
End;

Destructor TfsSqlTableRef.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlTableRef.EmitSQL(Stream: TStream);
Begin
  If TableName <> '' Then
    Begin
      WriteStr(Stream, ' ');
      WriteStr(Stream, TableName);
      If Alias <> '' Then
        Begin
          WriteStr(Stream, ' AS ');
          WriteStr(Stream, Alias);
        End;
    End
  Else If TableExp <> Nil Then
    Begin
      WriteStr(Stream, ' (');
      TableExp.EmitSQL(Stream);
      WriteStr(Stream, ')');
      If Alias <> '' Then
        Begin
          WriteStr(Stream, ' AS ');
          WriteStr(Stream, Alias);
        End;
      If ColumnList <> Nil Then
        Begin
          WriteStr(Stream, ' (');
          ColumnList.EmitSQL(Stream);
          WriteStr(Stream, ')');
        End;
    End;
End;
{--------}

Procedure TfsSqlTableRef.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If Deep And assigned(TableExp) Then
    TableExp.EnumNodes(EnumMethod, Deep);
  If assigned(ColumnList) Then
    ColumnList.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlTableRef.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlTableRef)
    And (AnsiCompareText(TableName, TfsSqlTableRef(Other).TableName) = 0)
    And (AnsiCompareText(Alias, TfsSqlTableRef(Other).Alias) = 0)
    And (BothNil(TableExp, TfsSqlTableRef(Other).TableExp)
    Or (BothNonNil(TableExp, TfsSqlTableRef(Other).TableExp)
    And TableExp.Equals(TfsSqlTableRef(Other).TableExp)
    ))
    And (BothNil(ColumnList, TfsSqlTableRef(Other).ColumnList)
    Or (BothNonNil(ColumnList, TfsSqlTableRef(Other).ColumnList)
    And ColumnList.Equals(TfsSqlTableRef(Other).ColumnList)
    ));
End;

Procedure TfsSqlTableRef.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Var
  T: TFSSqlTableProxy;
Begin
  Assert(Owner <> Nil);
  T := GetTable(Self, omReadOnly, False);
  aCursorID := T.CursorID;
  T.LeaveCursorOpen := True;
  If T.Owner = Self Then
    Begin
      T.Owner := Nil;
      T.Free;
    End;
End;

Function TfsSqlTableRef.GetResultTable: TFSSqlTableProxy;
Begin
  Result := GetTable(Self, omReadOnly, False);
End;

Function TfsSqlTableRef.GetSQLName: String;
Begin
  If Alias <> '' Then
    Result := Alias
  Else If TableName <> '' Then
    Result := TableName
  Else
    Result := 'UNNAMED';
End;

Function TfsSqlTableRef.GetTable(AOwner: TObject;
  Const OpenMode: TffOpenMode; Const ExclContLock: Boolean): TFSSqlTableProxy;
Begin
  If DataBaseName <> '' Then
    If Not SameText(DataBaseName, Owner.FDatabase.Alias) Then
      SQLError(format('The referenced database name %s does not ' +
        'match the current database, %s.',
        [DataBaseName, Owner.FDatabase.Alias]));
  If TableName <> '' Then
    Begin
      If FTable = Nil Then
        Begin
          FTable := Owner.FDatabase.TableByName(AOwner, TableName,
            OpenMode, ExclContLock, Alias, ''); {!!.11}
          If FTable = Nil Then
            SQLError('Unable to open table: ' + TableName +
              '. Ensure the table exists And is not in use by ' +
              'another process.');
          FTable.SetIndex(-1);
        End;
      Result := FTable;
    End
  Else
    Result := TableExp.ResultTable;
End;

Function TfsSqlTableRef.Reduce: Boolean;
Begin
  If TableExp <> Nil Then
    If TableExp.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

Function TfsSqlTableRef.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Begin
  If TableExp <> Nil Then
    Result := TableExp.TargetFieldFromSourceField(F)
  Else
    Result := Nil; {!!.13}
End;

{====================================================================}

{===TfsSqlSimpleExpressionList=======================================}

Function TfsSqlSimpleExpressionList.AddExpression(
  Expression: TfsSqlSimpleExpression): TfsSqlSimpleExpression;
Begin
  FExpressionList.Add(Expression);
  Result := Expression;
End;
{--------}

Procedure TfsSqlSimpleExpressionList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlSimpleExpressionList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlSimpleExpressionList(Source).ExpressionCount) Do
        AddExpression(TfsSqlSimpleExpression.Create(Self)).Assign(
          TfsSqlSimpleExpressionList(Source).Expression[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlSimpleExpressionList.CheckIsConstant;
Var
  i: Integer;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(ExpressionCount) Do
    If Not Expression[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  FIsConstant := True;
End;

Function TfsSqlSimpleExpressionList.Contains(Const TestValue: Variant): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(ExpressionCount) Do
    If Expression[i].GetValue(-1) = TestValue Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Constructor TfsSqlSimpleExpressionList.Create(
  AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FExpressionList := TList.Create;
End;
{--------}

Procedure TfsSqlSimpleExpressionList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(ExpressionCount) Do
    Expression[i].Free;
  FExpressionList.Clear;
End;
{--------}

Function TfsSqlSimpleExpressionList.DependsOn(
  Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(ExpressionCount) Do
    If Expression[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Destructor TfsSqlSimpleExpressionList.Destroy;
Begin
  Clear;
  FExpressionList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlSimpleExpressionList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  Expression[0].EmitSQL(Stream);
  For i := 1 To pred(ExpressionCount) Do
    Begin
      WriteStr(Stream, ', ');
      Expression[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlSimpleExpressionList.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(ExpressionCount) Do
    Expression[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlSimpleExpressionList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlSimpleExpressionList Then
    Begin
      If ExpressionCount <> TfsSqlSimpleExpressionList(Other).ExpressionCount Then
        Exit;
      For i := 0 To pred(ExpressionCount) Do
        If Not Expression[i].Equals(TfsSqlSimpleExpressionList(Other).Expression[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlSimpleExpressionList.GetExpression(
  Index: Integer): TfsSqlSimpleExpression;
Begin
  Result := TfsSqlSimpleExpression(FExpressionList[Index]);
End;
{--------}

Function TfsSqlSimpleExpressionList.GetExpressionCount: Integer;
Begin
  Result := FExpressionList.Count;
End;
{--------}

Function TfsSqlSimpleExpressionList.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Procedure TfsSqlSimpleExpressionList.MatchType(ExpectedType: TfsFieldType);
Var
  i: Integer;
Begin
  For i := 0 To pred(ExpressionCount) Do
    Expression[i].MatchType(ExpectedType);
End;
{--------}

Function TfsSqlSimpleExpressionList.Reduce: Boolean;
Var
  I: Integer;
Begin
  For i := 0 To pred(ExpressionCount) Do
    If Expression[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlSimpleExpressionList.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

Procedure TfsSqlSimpleExpressionList.SetExpression(Index: Integer;
  Const Value: TfsSqlSimpleExpression);
Begin
  FExpressionList[Index] := Value;
End;
{====================================================================}

{===TfsSqlOrderColumn================================================}

Procedure TfsSqlOrderColumn.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlOrderColumn Then
    Begin
      TableName := TfsSqlOrderColumn(Source).TableName;
      FieldName := TfsSqlOrderColumn(Source).FieldName;
    End
  Else
    AssignError(Source);
End;
{--------}

Procedure TfsSqlOrderColumn.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  If TableName <> '' Then
    Begin
      WriteStr(Stream, TableName);
      WriteStr(Stream, '.');
    End;
  WriteStr(Stream, FieldName);
End;
{--------}

Procedure TfsSqlOrderColumn.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlOrderColumn.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result := Other Is TfsSqlOrderColumn
    And (AnsiCompareText(TableName, TfsSqlOrderColumn(Other).TableName) = 0)
    And (AnsiCompareText(FieldName, TfsSqlOrderColumn(Other).FieldName) = 0);
End;
{--------}

Function TfsSqlOrderColumn.QualColumnName: String;
Begin
  If TableName <> '' Then
    Result := TableName + '.' + FieldName
  Else
    Result := FieldName;
End;
{====================================================================}

{===TfsSqlGroupColumn================================================}

Procedure TfsSqlGroupColumn.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlGroupColumn Then
    Begin
      TableName := TfsSqlGroupColumn(Source).TableName;
      FieldName := TfsSqlGroupColumn(Source).FieldName;
    End
  Else
    AssignError(Source);
End;
{--------}

Procedure TfsSqlGroupColumn.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  If TableName <> '' Then
    Begin
      WriteStr(Stream, TableName);
      WriteStr(Stream, '.');
    End;
  WriteStr(Stream, FieldName);
End;
{--------}

Procedure TfsSqlGroupColumn.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlGroupColumn.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result := Other Is TfsSqlGroupColumn
    And (AnsiCompareText(TableName, TfsSqlGroupColumn(Other).TableName) = 0)
    And (AnsiCompareText(FieldName, TfsSqlGroupColumn(Other).FieldName) = 0);
End;
{--------}

Function TfsSqlGroupColumn.QualColumnName: String;
Var
  F: TfsSqlFieldProxy;
  Name: String;
Begin
  If OwnerSelect = Nil Then
    SQLError('Field references may not occur in this context');
  If TableName <> '' Then
    Begin
      Name := OwnerSelect.TableRefList.GetNameForAlias(FTableName);
      If Name <> '' Then
        Result := Name + '.' + FFieldName
      Else
        Result := TableName + '.' + FFieldName;
    End
  Else
    Begin
      { If this is an alias For a field in the selection list then return
        the name. }
      If OwnerSelect.Columns.IndexOf(FieldName) > -1 Then
        Result := FieldName
      Else
        Begin
          { Find the proxy For this field. }
          F := OwnerSelect.FindField(FFieldName);
          If F = Nil Then
            Result := FFieldName
          Else
            Result := F.OwnerTable.Name + '.' + FFieldName;
        End;
    End;
End;
{====================================================================}

{===TfsSqlOrderItem==================================================}

Procedure TfsSqlOrderItem.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlOrderItem Then
    Begin
      If TfsSqlOrderItem(Source).Column <> Nil Then
        Begin
          If Column = Nil Then
            Column := TfsSqlOrderColumn.Create(Self);
          Column.Assign(TfsSqlOrderItem(Source).Column);
        End;
      Index := TfsSqlOrderItem(Source).Index;
      Descending := TfsSqlOrderItem(Source).Descending;
      Size := TfsSqlOrderItem(Source).Size;
      NoCase := TfsSqlOrderItem(Source).NoCase;
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlOrderItem.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
End;

Destructor TfsSqlOrderItem.Destroy;
Begin
  Column.Free;
  Inherited;
End;

Procedure TfsSqlOrderItem.EmitSQL(Stream: TStream);
Begin
  If Column <> Nil Then
    Column.EmitSQL(Stream)
  Else
    Begin
      WriteStr(Stream, ' ');
      WriteStr(Stream, Index);
    End;
  If Descending Then
    WriteStr(Stream, ' DESC')
  Else
    Writestr(Stream, ' ASC');

  If NoCase Then
    Writestr(Stream, ' NOT CASE');
End;
{--------}

Procedure TfsSqlOrderItem.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If Column <> Nil Then
    Column.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlOrderItem.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlOrderItem)
    And (Descending = TfsSqlOrderItem(Other).Descending)
    And (Index = TfsSqlOrderItem(Other).Index)
    And (BothNil(Column, TfsSqlOrderItem(Other).Column)
    Or (BothNonNil(Column, TfsSqlOrderItem(Other).Column)
    And Column.Equals(TfsSqlOrderItem(Other).Column)
    ));
End;
{--------}
{====================================================================}

{===TfsSqlOrderList==================================================}

Function TfsSqlOrderList.AddOrderItem(NewOrder: TfsSqlOrderItem): TfsSqlOrderItem;
Begin
  FOrderItemList.Add(NewOrder);
  Result := NewOrder;
End;
{--------}

Procedure TfsSqlOrderList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlOrderList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlOrderList(Source).OrderCount) Do
        AddOrderItem(TfsSqlOrderItem.Create(Self)).Assign(
          TfsSqlOrderList(Source).OrderItem[i]);
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlOrderList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FOrderItemList := TList.Create;
  UOrder := False;
End;
{--------}

Procedure TfsSqlOrderList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(FOrderItemList.Count) Do
    OrderItem[i].Free;
  FOrderItemList.Clear;
End;
{--------}

Destructor TfsSqlOrderList.Destroy;
Begin
  Clear;
  FOrderItemList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlOrderList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  If UOrder Then
    WriteStr(Stream, ' ORDERUNION BY')
  Else
    WriteStr(Stream, ' ORDER BY');
  OrderItem[0].EmitSQL(Stream);
  For i := 1 To pred(OrderCount) Do
    Begin
      WriteStr(Stream, ', ');
      OrderItem[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlOrderList.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(OrderCount) Do
    OrderItem[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlOrderList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlOrderList Then
    Begin
      If OrderCount <> TfsSqlOrderList(Other).OrderCount Then
        Exit;
      For i := 0 To pred(OrderCount) Do
        If Not OrderItem[i].Equals(TfsSqlOrderList(Other).OrderItem[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlOrderList.GetOrderCount: Integer;
Begin
  Result := FOrderItemList.Count;
End;
{--------}

Function TfsSqlOrderList.GetOrderItem(
  Index: Integer): TfsSqlOrderItem;
Begin
  Result := TfsSqlOrderItem(FOrderItemList[Index]);
End;
{--------}

Function TfsSqlOrderList.Reduce: Boolean;
Begin
  Result := False;
End;

Procedure TfsSqlOrderList.SetOrderItem(Index: Integer;
  Const Value: TfsSqlOrderItem);
Begin
  FOrderItemList[Index] := Value;
End;
{====================================================================}

{===TfsSqlAllOrAnyClause=============================================}

Procedure TfsSqlAllOrAnyClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlAllOrAnyClause Then
    Begin
      All := TfsSqlAllOrAnyClause(Source).All;
      SubQuery.Free;
      SubQuery := TfsSqlSELECT.Create(Self);
      SubQuery.Assign(TfsSqlAllOrAnyClause(Source).SubQuery);
    End
  Else
    AssignError(Source);
End;

Function TfsSqlAllOrAnyClause.Compare(RelOp: TfsSqlRelOp;
  Const Val: Variant): Boolean;
Begin
  If All Then
    Result := SubQuery.CheckAllValues(RelOp, Val)
  Else
    Result := SubQuery.CheckAnyValue(RelOp, Val);
End;

Function TfsSqlAllOrAnyClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SubQuery.DependsOn(Table);
End;

Destructor TfsSqlAllOrAnyClause.Destroy;
Begin
  SubQuery.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlAllOrAnyClause.EmitSQL(Stream: TStream);
Begin
  If All Then
    WriteStr(Stream, ' ALL ')
  Else
    WriteStr(Stream, ' ANY ');
  WriteStr(Stream, '(');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlAllOrAnyClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlAllOrAnyClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlAllOrAnyClause)
    And (All = TfsSqlAllOrAnyClause(Other).All)
    And (SubQuery.Equals(TfsSqlAllOrAnyClause(Other).SubQuery));
End;
{--------}

Procedure TfsSqlAllOrAnyClause.MatchType(ExpectedType: TfsFieldType);
Begin
  SubQuery.MatchType(ExpectedType, True);
End;

Function TfsSqlAllOrAnyClause.Reduce: Boolean;
Begin
  Result := SubQuery.Reduce;
End;

{====================================================================}

{===TfsSqlExistsClause===============================================}

Function TfsSqlExistsClause.AsBoolean: Boolean;
Begin
  Result := SubQuery.CheckNonEmpty;
End;
{--------}

Procedure TfsSqlExistsClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlExistsClause Then
    Begin
      SubQuery.Free;
      SubQuery := TfsSqlSELECT.Create(Self);
      SubQuery.Assign(TfsSqlExistsClause(Source).SubQuery);
    End
  Else
    AssignError(Source);
End;

Function TfsSqlExistsClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SubQuery.DependsOn(Table);
End;

Destructor TfsSqlExistsClause.Destroy;
Begin
  SubQuery.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlExistsClause.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' EXISTS (');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlExistsClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlExistsClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlExistsClause)
    And (SubQuery.Equals(TfsSqlExistsClause(Other).SubQuery));
End;
{--------}

Function TfsSqlExistsClause.Reduce: Boolean;
Begin
  Result := SubQuery.Reduce;
End;

{====================================================================}

{===TfsSqlUniqueClause===============================================}

Function TfsSqlUniqueClause.AsBoolean: Boolean;
Begin
  Result := SubQuery.CheckNoDups(True);
End;
{--------}

Procedure TfsSqlUniqueClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlUniqueClause Then
    Begin
      SubQuery.Free;
      SubQuery := TfsSqlTableExp.Create(Self);
      SubQuery.Assign(TfsSqlUniqueClause(Source).SubQuery);
    End
  Else
    AssignError(Source);
End;

Function TfsSqlUniqueClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SubQuery.DependsOn(Table);
End;

Destructor TfsSqlUniqueClause.Destroy;
Begin
  SubQuery.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlUniqueClause.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' UNIQUE (');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlUniqueClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlUniqueClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlUniqueClause)
    And (SubQuery.Equals(TfsSqlUniqueClause(Other).SubQuery));
End;
{--------}

Function TfsSqlUniqueClause.Reduce: Boolean;
Begin
  Result := SubQuery.Reduce;
End;

{====================================================================}

Function OffsetTime(Const DateTime: TDateTime; DeltaH, DeltaM, DeltaS: Integer): TDateTime;
Var
  Mi, H, S, MSec: Word;
  Hs, Mis, Ss: Integer;
  DeltaD: Integer;
Begin
  DecodeTime(DateTime, H, Mi, S, MSec);
  Hs := H;
  Mis := Mi;
  Ss := S;
  Ss := Ss + (DeltaS Mod 60);
  Mis := Mis + (DeltaS Div 60);
  If Ss < 0 Then
    Begin
      dec(Mis);
      inc(Ss, 60);
    End
  Else If Ss >= 60 Then
    Begin
      inc(Mis);
      dec(Ss, 60);
    End;
  Mis := Mis + (DeltaM Mod 60);
  Hs := Hs + (DeltaM Div 60);
  If Mis < 0 Then
    Begin
      dec(Hs);
      inc(Mis, 60);
    End
  Else If Mis >= 60 Then
    Begin
      inc(Hs);
      dec(Mis, 60);
    End;
  Hs := Hs + (DeltaH Mod 24);
  DeltaD := (DeltaH Div 24);
  If Hs < 0 Then
    Begin
      dec(DeltaD);
      inc(Hs, 24);
    End
  Else If Hs >= 24 Then
    Begin
      inc(DeltaD);
      dec(Hs, 24);
    End;
  Result := Round(DateTime) + EncodeTime(Hs, Mis, Ss, MSec) + DeltaD;
End;

{===TfsSqlIntervalLiteral============================================}

Function TfsSqlIntervalLiteral.AddIntervalTo(Target: TDateTime): TDateTime;
Begin
  If Not Converted Then
    ConvertToNative;
  Case StartDef Of
    iYear:
      Case EndDef Of
        iUnspec:
          Result := IncMonth(Target, Y1 * 12);
        Else //iMonth :
          Result := IncMonth(Target, Y1 * 12 + M1);
      End;
    iMonth:
      Result := IncMonth(Target, M1);
    iDay:
      Case EndDef Of
        iUnspec:
          Result := Target + D1;
        iHour:
          Result := OffsetTime(Target, H1, 0, 0) + D1;
        iMinute:
          Result := OffsetTime(Target, H1, M1, 0) + D1;
        Else //iSecond :
          Result := OffsetTime(Target, H1, M1, S1) + D1;
      End;
    iHour:
      Case EndDef Of
        iUnspec:
          Result := OffsetTime(Target, H1, 0, 0);
        iMinute:
          Result := OffsetTime(Target, H1, M1, 0);
        Else //iSecond :
          Result := OffsetTime(Target, H1, M1, S1);
      End;
    iMinute:
      Case EndDef Of
        iUnspec:
          Result := OffsetTime(Target, 0, M1, 0);
        Else //iSecond :
          Result := OffsetTime(Target, 0, M1, S1);
      End;
    Else //iSecond :
      Result := OffsetTime(Target, 0, 0, S1);
  End;
End;
{--------}

Function TfsSqlIntervalLiteral.SubtractIntervalFrom(Target: TDateTime): TDateTime;
Begin
  If Not Converted Then
    ConvertToNative;
  Case StartDef Of
    iYear:
      Case EndDef Of
        iUnspec:
          Result := IncMonth(Target, -Y1 * 12);
        Else //iMonth :
          Result := IncMonth(Target, -(Y1 * 12 + M1));
      End;
    iMonth:
      Result := IncMonth(Target, -M1);
    iDay:
      Case EndDef Of
        iUnspec:
          Result := Target - D1;
        iHour:
          Result := OffsetTime(Target, -H1, 0, 0) - D1;
        iMinute:
          Result := OffsetTime(Target, -H1, -M1, 0) - D1;
        Else //iSecond :
          Result := OffsetTime(Target, -H1, -M1, -S1) - D1;
      End;
    iHour:
      Case EndDef Of
        iUnspec:
          Result := OffsetTime(Target, -H1, 0, 0);
        iMinute:
          Result := OffsetTime(Target, -H1, -M1, 0);
        Else //iSecond :
          Result := OffsetTime(Target, -H1, -M1, -S1);
      End;
    iMinute:
      Case EndDef Of
        iUnspec:
          Result := OffsetTime(Target, 0, -M1, 0);
        Else //iSecond :
          Result := OffsetTime(Target, 0, -M1, -S1);
      End;
    Else //iSecond :
      Result := OffsetTime(Target, 0, 0, -S1);
  End;
End;
{--------}

Procedure TfsSqlIntervalLiteral.ConvertToNative;
Var
  S: String;
  P: Integer;
Begin
  S := Value;
  Case StartDef Of
    iUnspec:
      SQLError('Internal error in date/time interval literal');
    iYear:
      Case EndDef Of
        iUnspec:
          Y1 := StrToInt(copy(S, 2, length(S) - 2));
        iYear:
          SQLError('Syntax error in year-month interval literal');
        iMonth:
          Begin
            P := fsPosCh('-', S);
            If P = 0 Then
              SQLError('Syntax error in year-month interval literal: "-" expected');
            Y1 := StrToInt(copy(S, 2, P - 2));
            M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        Else
          SQLError('Syntax error in year-month interval literal');
      End;
    iMonth:
      Case EndDef Of
        iUnspec:
          M1 := StrToInt(copy(S, 2, length(S) - 2));
        Else
          SQLError('Syntax error in year-month interval literal');
      End;
    iDay:
      Case EndDef Of
        iUnspec:
          D1 := StrToInt(copy(S, 2, length(S) - 2));
        iHour:
          Begin
            P := fsPosCh(' ', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: " " expected');
            D1 := StrToInt(copy(S, 2, P - 2));
            H1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        iMinute:
          Begin
            P := fsPosCh(' ', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: " " expected');
            D1 := StrToInt(copy(S, 2, P - 2));
            Delete(S, 2, P - 2);
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            H1 := StrToInt(copy(S, 2, P - 2));
            M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        iSecond:
          Begin
            P := fsPosCh(' ', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: " " expected');
            D1 := StrToInt(copy(S, 2, P - 2));
            Delete(S, 2, P - 1);
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            H1 := StrToInt(copy(S, 2, P - 2));
            Delete(S, 2, P - 1);
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            M1 := StrToInt(copy(S, 2, P - 2));
            S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        Else
          SQLError('Syntax error in date-time interval literal');
      End;
    iHour:
      Case EndDef Of
        iUnspec:
          H1 := StrToInt(copy(S, 2, length(S) - 2));
        iMinute:
          Begin
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            H1 := StrToInt(copy(S, 2, P - 2));
            M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        iSecond:
          Begin
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            H1 := StrToInt(copy(S, 2, P - 2));
            Delete(S, 2, P - 1);
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            M1 := StrToInt(copy(S, 2, P - 2));
            S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        Else
          SQLError('Syntax error in date-time interval literal');
      End;
    iMinute:
      Case EndDef Of
        iUnspec:
          M1 := StrToInt(copy(S, 2, length(S) - 2));
        iSecond:
          Begin
            ;
            P := fsPosCh(':', S);
            If P = 0 Then
              SQLError('Syntax error in date-time interval literal: ":" expected');
            M1 := StrToInt(copy(S, 2, P - 2));
            S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
          End;
        Else
          SQLError('Syntax error in date-time interval literal');
      End;
    iSecond:
      Case EndDef Of
        iUnspec:
          S1 := StrToInt(copy(S, 2, length(S) - 2));
        Else
          SQLError('Syntax error in date-time interval literal');
      End;
    Else
      SQLError('Syntax error in date-time interval literal');
  End;
  Converted := True;
End;
{--------}

Procedure TfsSqlIntervalLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlIntervalLiteral Then
    Begin
      Value := TfsSqlIntervalLiteral(Source).Value;
      StartDef := TfsSqlIntervalLiteral(Source).StartDef;
      EndDef := TfsSqlIntervalLiteral(Source).EndDef;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlIntervalLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' INTERVAL ');
  WriteStr(Stream, Value);
  WriteStr(Stream, ' ');
  WriteStr(Stream, fsDefStr[StartDef]);
  If EndDef <> iUnspec Then
    Begin
      WriteStr(Stream, ' TO ');
      WriteStr(Stream, fsDefStr[EndDef]);
    End;
End;
{--------}

Procedure TfsSqlIntervalLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlIntervalLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlIntervalLiteral)
    And (AnsiCompareText(Value, TfsSqlIntervalLiteral(Other).Value) = 0)
    And (StartDef = TfsSqlIntervalLiteral(Other).StartDef)
    And (EndDef = TfsSqlIntervalLiteral(Other).EndDef);
End;
{--------}

Function TfsSqlIntervalLiteral.GetType: TfsFieldType;
Begin
  Result := fstInterval;
End;
{--------}

Function TfsSqlIntervalLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  Result := '';
  {This value returned to allow tests For NULL to pass}
End;
{--------}

Procedure TfsSqlIntervalLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstDate,
      fstDateTime:
      ;
    Else
      TypeMismatch;
  End;
  If Not Converted Then
    ConvertToNative;
End;
{====================================================================}

{===TfsSqlTimestampLiteral===========================================}

Procedure TfsSqlTimestampLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlTimeStampLiteral Then
    Begin
      Value := TfsSqlTimeStampLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlTimeStampLiteral.ConvertToNative;
Begin
  If (length(Value) < 21)
    Or Not (Value[6] In ['-', '.', '/'])
    Or (Value[9] <> Value[6])
    Or (Value[12] <> ' ')
    Or (Value[15] <> ':')
    Or (Value[18] <> ':') Then
    SQLError('Syntax error in time stamp literal');
  DateTimeValue :=
    EncodeDate(
    StrToInt(copy(Value, 2, 4)),
    StrToInt(copy(Value, 7, 2)),
    StrToInt(copy(Value, 10, 2)))
    +
    EncodeTime(
    StrToInt(copy(Value, 13, 2)),
    StrToInt(copy(Value, 16, 2)),
    StrToInt(copy(Value, 19, 2)),
    0);
  Converted := True;
End;

Procedure TfsSqlTimestampLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' TIMESTAMP ');
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlTimestampLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlTimestampLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlTimestampLiteral)
    And (AnsiCompareText(Value, TfsSqlTimestampLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlTimestampLiteral.GetType: TfsFieldType;
Begin
  Result := fstDateTime;
End;

Function TfsSqlTimestampLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  Result := DateTimeValue;
End;
{--------}

Procedure TfsSqlTimestampLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstTime,
      fstDateTime:
      ;
    Else
      TypeMismatch;
  End;
  If Not Converted Then
    ConvertToNative;
End;
{====================================================================}

{===TfsSqlTimeLiteral================================================}

Procedure TfsSqlTimeLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlTimeLiteral Then
    Begin
      Value := TfsSqlTimeLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlTimeLiteral.ConvertToNative;
Begin
  If (length(Value) <> 10)
    Or (Value[4] <> ':')
    Or (Value[7] <> ':') Then
    SQLError('Syntax error in time literal');
  TimeValue := EncodeTime(
    StrToInt(copy(Value, 2, 2)),
    StrToInt(copy(Value, 5, 2)),
    StrToInt(copy(Value, 8, 2)),
    0);
  Converted := True;
End;
{--------}

Procedure TfsSqlTimeLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' TIME ');
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlTimeLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlTimeLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlTimeLiteral)
    And (AnsiCompareText(Value, TfsSqlTimeLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlTimeLiteral.GetType: TfsFieldType;
Begin
  Result := fstTime;
End;

Function TfsSqlTimeLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  Result := TimeValue;
End;
{--------}

Procedure TfsSqlTimeLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstTime,
      fstDateTime:
      ;
    Else
      TypeMismatch;
  End;
  If Not Converted Then
    ConvertToNative;
End;
{====================================================================}

{===TfsSqlDateLiteral================================================}
{--------}

Procedure TfsSqlDateLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlDateLiteral Then
    Begin
      Value := TfsSqlDateLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlDateLiteral.ConvertToNative;
Begin
  // '2005-12-31' - 12 chr
  // '31-12-2005'
  If (length(Value) <> 12)
    Or Not (Value[6] In ['-', '.', '/'])
    Or (Value[9] <> Value[6]) Then
    SQLError('Syntax error in date literal');
  Try
    DateValue := EncodeDate(
      StrToInt(copy(Value, 2, 4)),
      StrToInt(copy(Value, 7, 2)),
      StrToInt(copy(Value, 10, 2)));
    Converted := True;
  Except
    // not yet
    Try
      DateValue := EncodeDate(
        StrToInt(copy(Value, 8, 4)),
        StrToInt(copy(Value, 5, 2)),
        StrToInt(copy(Value, 2, 2)));
      Converted := True;
    Except
      Converted := False;
      SQLError('Syntax error in date literal');
    End;
  End;
End;
{--------}

Procedure TfsSqlDateLiteral.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' DATE ');
  WriteStr(Stream, Value);
End;
{--------}

Procedure TfsSqlDateLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlDateLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlDateLiteral)
    And (AnsiCompareText(Value, TfsSqlDateLiteral(Other).Value) = 0);
End;
{--------}

Function TfsSqlDateLiteral.GetType: TfsFieldType;
Begin
  Result := fstDate;
End;

Function TfsSqlDateLiteral.GetValue(aArrayIndex: Integer): Variant;
Begin
  If Not Converted Then
    ConvertToNative;
  Result := DateValue;
End;
{--------}

Procedure TfsSqlDateLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstDate,
      fstDateTime:
      ;
    Else
      TypeMismatch;
  End;
  If Not Converted Then
    ConvertToNative;
End;
{===TfsSqlBooleanLiteral================================================}
{--------}

Procedure TfsSqlBooleanLiteral.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlBooleanLiteral Then
    Begin
      Value := TfsSqlBooleanLiteral(Source).Value;
    End
  Else
    AssignError(Source);
End;

{--------}

Procedure TfsSqlBooleanLiteral.EmitSQL(Stream: TStream);
Begin
  If Value Then
    WriteStr(Stream, ' TRUE')
  Else
    WriteStr(Stream, ' FALSE');
End;
{--------}

Procedure TfsSqlBooleanLiteral.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;
{--------}

Function TfsSqlBooleanLiteral.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlBooleanLiteral)
    And (Value = TfsSqlBooleanLiteral(Other).Value);
End;
{--------}

Function TfsSqlBooleanLiteral.GetType: TfsFieldType;
Begin
  Result := fstBoolean;
End;

Function TfsSqlBooleanLiteral.GetValue: Boolean;
Begin
  Result := Value;
End;
{--------}

Procedure TfsSqlBooleanLiteral.MatchType(ExpectedType: TfsFieldType);
Begin
  Case ExpectedType Of
    fstBoolean: ;
    Else
      TypeMismatch;
  End;
End;
{====================================================================}

Const
  FuncStr: Array[TfsSqlScalarFunction] Of String = (
    'CASE', 'CHARACTER_LENGTH', 'COALESCE', 'CURRENT_DATE', 'CURRENT_TIME', 'CURRENT_TIMESTAMP',
    'CURRENT_USER', 'LOWER', 'UPPER', 'POSITION', 'WEEKNO', 'SUBSTRING',
    'TRIM', 'EXTRACT', 'NULLIF', 'CAST',
    'ABS', 'CEIL', 'FLOOR', 'EXP', 'LOG', 'POWER', 'RAND', 'ROUND', 'FLAGS', 'MOD',
    'DIV', 'ODD', 'ARRAY', 'ISRECORDLOCKED', 'ISUNDELETEDRECORD', 'ISPROTECTDELETERECORD',
    'ISPROTECTUPDATERECORD', 'ISMARKASBADRECORD');
  LeadStr: Array[TfsSqlLTB] Of String = ('BOTH', 'LEADING', 'TRAILING');
  {===TfsSqlScalarFunc=================================================}

Constructor TfsSqlScalarFunc.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  Arg1 := Nil;
  Arg2 := Nil;
  Arg3 := Nil;
  Arg4 := Nil;
  FDecimals := 0;
  FSize := 0;
  fRoundType := rNone;
  fIsCastNull := False;
  fArrayIndex := False;
End;

Procedure TfsSqlScalarFunc.CheckIsConstant;
Var
  V: variant;
Begin
  FIsConstantChecked := True;
  Case SQLFunction Of
    sfCase:
      FIsConstant := CaseExp.IsConstant;
    sfCharlen:
      FIsConstant := Arg1.IsConstant;
    sfFlags:
      FIsConstant := Arg2.IsConstant And Arg1.IsConstant;
    sfCoalesce, sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD:
      FIsConstant := False;
    sfCurrentDate:
      FIsConstant := True;
    sfCurrentTime:
      FIsConstant := True;
    sfCurrentTimestamp:
      FIsConstant := True;
    sfCurrentUser:
      FIsConstant := True;
    sfLower, sfWeekNo:
      FIsConstant := Arg1.IsConstant;
    sfUpper:
      FIsConstant := Arg1.IsConstant;
    sfPosition:
      FIsConstant := Arg2.IsConstant And Arg1.IsConstant;
    sfSubstring:
      FIsConstant :=
        Arg1.IsConstant And Arg2.IsConstant And
        ((Arg3 = Nil) Or (Arg3.IsConstant));
    sfTrim:
      FIsConstant :=
        ((Arg1 = Nil) Or (Arg1.IsConstant))
        And ((Arg2 = Nil) Or (Arg2.IsConstant));
    sfExtract:
      FIsConstant := Arg1.IsConstant;
    sfNullIf:
      FIsConstant := Arg2.IsConstant And Arg1.IsConstant;
    {!!.11 begin}
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfODD:
      FIsConstant := Arg1.IsConstant;
    sfCast:
      Begin
        If Arg1 <> Nil Then
          FIsConstant := Arg1.IsConstant;
      End;
    sfRound:
      Begin
        FIsConstant := ((Arg1 = Nil) Or (Arg1.IsConstant))
          And ((Arg2 = Nil) Or (Arg2.IsConstant))
          And ((Arg3 = Nil) Or (Arg3.IsConstant));
      End;
    sfArray:
      Begin
        FIsConstant := (Arg2.IsConstant And Arg1.IsConstant)
          And ((Arg3 = Nil) Or (Arg3.IsConstant));
      End;
    sfRand:
      FIsConstant := False;
    sfPower, sfMod, sfDiv:
      FIsConstant := Arg2.IsConstant And Arg1.IsConstant;
    {!!.11 end}
    Else
      Assert(False);
  End;
  If FIsConstant Then
    Begin
      FIsConstant := False;
      v := getvalue(-1);
      {$IFDEF IsNoVariantInt64}
      If (TVarData(V).VType = VT_DECIMAL) Then
        Begin
          TVarData(ConstantValue).VType := VT_DECIMAL;
          Decimal(ConstantValue).lo64 := Decimal(V).lo64;
        End
      Else If (TVarData(V).VType = VT_E80) Then
        Begin
          TVarData(ConstantValue).VType := VT_E80;
          Decimal(ConstantValue).ext80 := Decimal(V).ext80;
        End
      Else
        ConstantValue := v;
      {$ELSE}
      ConstantValue := v;
      {$ENDIF}
      FIsConstant := True;
    End;
End;

Procedure TfsSqlScalarFunc.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlScalarFunc Then
    Begin
      Clear;
      SQLFunction := TfsSqlScalarFunc(Source).SQLFunction;
      If assigned(TfsSqlScalarFunc(Source).Arg1) Then
        Begin
          Arg1 := TfsSqlSimpleExpression.Create(Self);
          Arg1.Assign(TfsSqlScalarFunc(Source).Arg1);
        End;
      If assigned(TfsSqlScalarFunc(Source).Arg2) Then
        Begin
          Arg2 := TfsSqlSimpleExpression.Create(Self);
          Arg2.Assign(TfsSqlScalarFunc(Source).Arg2);
        End;
      If assigned(TfsSqlScalarFunc(Source).Arg3) Then
        Begin
          Arg3 := TfsSqlSimpleExpression.Create(Self);
          Arg3.Assign(TfsSqlScalarFunc(Source).Arg3);
        End;
      If assigned(TfsSqlScalarFunc(Source).Arg4) Then
        Begin
          Arg4 := TfsSqlSimpleExpression.Create(Self);
          Arg4.Assign(TfsSqlScalarFunc(Source).Arg4);
        End;

      LTB := TfsSqlScalarFunc(Source).LTB;
      XDef := TfsSqlScalarFunc(Source).XDef;
      Self.FType := TfsSqlScalarFunc(Source).FType;
      Self.FSize := TfsSqlScalarFunc(Source).FSize;
      Self.FDecimals := TfsSqlScalarFunc(Source).FDecimals;
      Self.fRoundType := TfsSqlScalarFunc(Source).fRoundType;
      Self.fIsCastNull := TfsSqlScalarFunc(Source).fIsCastNull;
      If assigned(TfsSqlScalarFunc(Source).CaseExp) Then
        Begin
          CaseExp := TfsSqlCaseExpression.Create(Self);
          CaseExp.Assign(TfsSqlScalarFunc(Source).CaseExp);
        End;
      If assigned(TfsSqlScalarFunc(Source).CoalesceExp) Then
        Begin
          CoalesceExp := TfsSqlCoalesceExpression.Create(Self);
          CoalesceExp.Assign(TfsSqlScalarFunc(Source).CoalesceExp);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlScalarFunc.Clear;
Begin
  CaseExp.Free;
  CoalesceExp.Free;
  Arg1.Free;
  Arg2.Free;
  Arg3.Free;
  Arg4.Free;
  CaseExp := Nil;
  CoalesceExp := Nil;
  Arg1 := Nil;
  Arg2 := Nil;
  Arg3 := Nil;
  Arg4 := Nil;
End;

Function TfsSqlScalarFunc.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Case SQLFunction Of
    sfCase:
      Result := CaseExp.DependsOn(Table);
    sfCharlen,
      sfLower,
      sfUpper,
      sfExtract:
      Result := Arg1.DependsOn(Table);
    sfCoalesce:
      Result := CoalesceExp.DependsOn(Table);
    sfCurrentDate,
      sfCurrentTime,
      sfCurrentTimestamp,
      sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD,
      sfCurrentUser:
      Result := False;
    sfPosition, sfFlags:
      Result := Arg2.DependsOn(Table) Or Arg1.DependsOn(Table);
    sfSubstring:
      Begin
        Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table);
        If Not Result And (Arg3 <> Nil) Then
          Result := Arg3.DependsOn(Table);
      End;
    sfTrim:
      Begin
        If Arg2 = Nil Then
          Result := Arg1.DependsOn(Table)
        Else
          Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table)
      End;
    sfNullIf:
      Begin
        Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table);
      End;
    {!!.11 begin}
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfWeekNo, sfODD:
      Result := Arg1.DependsOn(Table);
    sfCast: Result := ((Arg1 <> Nil) And Arg1.DependsOn(Table)) Or Self.IsCastNull;
    sfArray:
      Begin
        If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 <> Nil) Then
          Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table) Or Arg3.DependsOn(Table)
        Else If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 = Nil) Then
          Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table);
      End;
    sfRound:
      Begin
        If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 <> Nil) Then
          Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table) Or Arg3.DependsOn(Table)
        Else If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 = Nil) Then
          Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table)
        Else If (Arg1 <> Nil) And (Arg2 = Nil) And (Arg3 = Nil) Then
          Result := Arg1.DependsOn(Table);
      End;
    sfRand:
      Result := False;
    sfPower, sfMod, sfDiv:
      Result := Arg1.DependsOn(Table) Or Arg2.DependsOn(Table);
    {!!.11 end}
    Else
      Assert(False);
      Result := False;
  End;
End;

Destructor TfsSqlScalarFunc.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlScalarFunc.EmitSQL(Stream: TStream);

  Function rToString(ARound: TRound): String;
  Var
    s: String;
  Begin
    s := '';
    Case ARound Of
      rMathematical: S := ' MATHEMATICAL';
      rMatAfter1: S := ' MATAFTER1';
      rMatAfter2: S := ' MATAFTER2';
      rMatAfter3: S := ' MATAFTER3';
      rMatAfter4: S := ' MATAFTER4';
      rMatAfter5: S := ' MATAFTER5';
      rMatAfter6: S := ' MATAFTER6';
      rMatAfter7: S := ' MATAFTER7';
      rMatAfter8: S := ' MATAFTER8';
      rMatAfter9: S := ' MATAFTER9';
    End;
    Result := s;
  End;

  Function DataTypeToString(ADT: TfsFieldType; ADecimals, ASize: Integer; ARound: TRound): String;
  Var
    s: String;
  Begin
    s := '';
    Case ARound Of
      rMathematical: S := ' MATHEMATICAL';
      rMatAfter1: S := ' MATAFTER1';
      rMatAfter2: S := ' MATAFTER2';
      rMatAfter3: S := ' MATAFTER3';
      rMatAfter4: S := ' MATAFTER4';
      rMatAfter5: S := ' MATAFTER5';
      rMatAfter6: S := ' MATAFTER6';
      rMatAfter7: S := ' MATAFTER7';
      rMatAfter8: S := ' MATAFTER8';
      rMatAfter9: S := ' MATAFTER9';
    End;
    Result := '';
    Case ADT Of
      fstInterval: Result := Result + ' INTERVAL';
      fstBoolean: Result := Result + ' BOOLEAN';
      fstSingleChar: Result := Result + ' CHAR';
      fstSingleWideChar: Result := Result + ' WIDECHAR';
      fstUInt8: Result := Result + ' BYTE';
      fstUInt16: Result := Result + ' WORD16';
      fstUInt32: Result := Result + ' WORD32';
      fstInt8: Result := Result + ' INT8';
      fstInt16: Result := Result + ' INT16';
      fstInt32: Result := Result + ' INT32';
      fstInt64: Result := Result + ' INT64';
      fstRecVersion: Result := Result + ' RECVERSION';
      fstAutoInc32: Result := Result + ' AUTOINC32';
      fstAutoInc64: Result := Result + ' AUTOINC64';
      fstSingle: Result := Result + ' SINGLE' + S;
      fstDouble: Result := Result + ' DOUBLE' + S;
      fstExtended: Result := Result + ' EXTENDED' + S;
      fstCurrency: Result := Result + ' CURRENCY' + S;
      fstBcd: Result := Result + ' BCD' + S;
      fstDate: Result := Result + ' DATE';
      fstTime: Result := Result + ' TIME';
      fstDateTime: Result := Result + ' DATETIME';
      fstBlob: Result := Result + ' BLOB';
      fstBLOBMemo: Result := Result + ' MEMO';
      fstBLOBGraphic: Result := Result + ' GRAPHIC';
      fstArrayUInt8: Result := Result + ' BYTEARRAY';
      fstArrayInt32: Result := Result + ' INTARRAY';
      fstArrayUInt16: Result := Result + ' WORDARRAY';
      fstArrayDouble: Result := Result + ' BYTEARRAY';
      fstShortString: Result := Result + ' SHORTSTRING';
      fstNullString: Result := Result + ' NULLSTRING';
      fstVarNullString: Result := Result + ' VARNULLSTRING';
      fstWideString: Result := Result + ' WIDESTRING';
      fstVarWideString: Result := Result + ' VARWIDESTRING';
    End;
    Case ADT Of
      fstSingle, fstDouble, fstExtended, fstCurrency, fstBcd:
        If ADecimals <> 0 Then
          Result := Result + '(' + IntToStr(ADecimals) + ')';
      fstAutoInc32, fstAutoInc64:
        Begin
          If ASize <> 0 Then
            Result := Result + '(' + IntToStr(ASize) + ')';
          If ADecimals <> 0 Then
            Result := Result + '(' + IntToStr(ADecimals) + ')';
        End;
      fstShortString, fstNullString, fstVarNullString, fstWideString, fstVarWideString:
        Result := Result + '(' + IntToStr(ASize) + ')';
    End;
  End;

Begin
  WriteStr(Stream, ' ');
  Case SQLFunction Of
    sfCase:
      CaseExp.EmitSQL(Stream);
    sfCoalesce:
      CoalesceExp.EmitSQL(Stream);
    sfCurrentDate,
      sfCurrentTime,
      sfCurrentTimestamp,
      sfCurrentUser,
      sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD,
      sfRand:
      WriteStr(Stream, FuncStr[SQLFunction]);
    Else
      WriteStr(Stream, FuncStr[SQLFunction]);
      WriteStr(Stream, '(');
      Case SQLFunction Of
        sfCharlen,
          sfLower,
          sfUpper,
          sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfWeekNo, sfODD:
          Begin
            Arg1.EmitSQL(Stream);
          End;
        sfNullIf,
          sfPosition,
          sfFlags,
          sfPower, sfMod, sfDiv: {!!.11}
          Begin
            Arg1.EmitSQL(Stream);
            WriteStr(Stream, ' , ');
            Arg2.EmitSQL(Stream);
          End;
        sfCast:
          Begin
            If Arg1 <> Nil Then
              Arg1.EmitSQL(Stream)
            Else If IsCastNull Then
              WriteStr(Stream, 'NULL AS ' + DataTypeToString(DataType, Decimals, Size, RoundType) + ' )')
            Else
              WriteStr(Stream, ' AS ' + DataTypeToString(DataType, Decimals, Size, RoundType) + ' )');
          End;
        sfRound: {!!.11}
          Begin
            Arg1.EmitSQL(Stream);
            WriteStr(Stream, IntToStr(fDecimals) + ', ' + rToString(fRoundType) + ' )')
          End;
        sfArray:
          Begin
            If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 <> Nil) Then
              Begin
                Arg1.EmitSQL(Stream);
                WriteStr(Stream, ' , ');
                Arg2.EmitSQL(Stream);
                WriteStr(Stream, ' , ');
                Arg3.EmitSQL(Stream);
              End
            Else If (Arg1 <> Nil) And (Arg2 <> Nil) And (Arg3 = Nil) Then
              Begin
                Arg1.EmitSQL(Stream);
                WriteStr(Stream, ' , ');
                Arg2.EmitSQL(Stream);
              End;
          End;
        sfSubstring:
          Begin
            Arg1.EmitSQL(Stream);
            WriteStr(Stream, ' FROM ');
            Arg2.EmitSQL(Stream);
            If Arg3 <> Nil Then
              Begin
                WriteStr(Stream, ' For ');
                Arg3.EmitSQL(Stream);
              End;
          End;
        sfTrim:
          Begin
            WriteStr(Stream, LeadStr[LTB]);
            WriteStr(Stream, ' ');
            If Arg1 <> Nil Then
              Arg1.EmitSQL(Stream);
            If Arg2 <> Nil Then
              Begin
                WriteStr(Stream, ' FROM ');
                Arg2.EmitSQL(Stream);
              End;
          End;
        sfExtract:
          Begin
            WriteStr(Stream, fsDefStr[XDef]);
            WriteStr(Stream, ' FROM ');
            Arg1.EmitSQL(Stream);
          End;
      End;
      WriteStr(Stream, ')');
  End;
End;
{--------}

Procedure TfsSqlScalarFunc.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  Case SQLFunction Of
    sfCase:
      CaseExp.EnumNodes(EnumMethod, Deep);
    sfCoalesce:
      CoalesceExp.EnumNodes(EnumMethod, Deep);
    sfCurrentDate,
      sfCurrentTime,
      sfCurrentTimestamp,
      sfCurrentUser,
      sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD,
      sfRand:
      ;
    Else
      Case SQLFunction Of
        sfCharlen,
          sfLower,
          sfUpper,
          sfExtract,
          sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfWeekNo, sfODD:
          Arg1.EnumNodes(EnumMethod, Deep);
        sfNullIf,
          sfPosition,
          sfFlags,
          sfPower, sfMod, sfDiv:
          Begin
            Arg1.EnumNodes(EnumMethod, Deep);
            Arg2.EnumNodes(EnumMethod, Deep);
          End;
        sfCast:
          Begin
            If Arg1 <> Nil Then
              Arg1.EnumNodes(EnumMethod, Deep);
            If Arg2 <> Nil Then
              Arg2.EnumNodes(EnumMethod, Deep);
            If Arg3 <> Nil Then
              Arg3.EnumNodes(EnumMethod, Deep);
            If Arg4 <> Nil Then
              Arg4.EnumNodes(EnumMethod, Deep);
          End;
        sfArray:
          Begin
            If Arg1 <> Nil Then
              Arg1.EnumNodes(EnumMethod, Deep);
            If Arg2 <> Nil Then
              Arg2.EnumNodes(EnumMethod, Deep);
            If Arg3 <> Nil Then
              Arg3.EnumNodes(EnumMethod, Deep);
          End;
        sfRound: {!!.11}
          Begin
            Arg1.EnumNodes(EnumMethod, Deep);
            If Arg2 <> Nil Then
              Begin
                Arg2.EnumNodes(EnumMethod, Deep);
                If Arg3 <> Nil Then
                  Arg3.EnumNodes(EnumMethod, Deep);
              End;
          End;
        sfSubstring:
          Begin
            Arg1.EnumNodes(EnumMethod, Deep);
            Arg2.EnumNodes(EnumMethod, Deep);
            If Arg3 <> Nil Then
              Arg3.EnumNodes(EnumMethod, Deep);
          End;
        sfTrim:
          Begin
            If Arg1 <> Nil Then
              Arg1.EnumNodes(EnumMethod, Deep);
            If Arg2 <> Nil Then
              Arg2.EnumNodes(EnumMethod, Deep);
          End;
      End;
  End;
End;
{--------}

Function TfsSqlScalarFunc.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result := False;
  If Other Is TfsSqlScalarFunc Then
    Begin
      If SQLFunction <> TfsSqlScalarFunc(Other).SQLFunction Then
        Exit;
      Case SQLFunction Of
        sfCase:
          If Not CaseExp.Equals(TfsSqlScalarFunc(Other).CaseExp) Then
            Exit;
        sfCoalesce:
          If Not CoalesceExp.Equals(TfsSqlScalarFunc(Other).CoalesceExp) Then
            Exit;
        sfCharlen,
          sfLower,
          sfUpper,
          sfExtract,
          sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfWeekNo, sfODD:
          If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
            Exit;
        sfNullIf,
          sfPosition,
          sfFlags,
          sfPower, sfMod, sfDiv: {!!.11}
          Begin
            If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
              Exit;
            If Not Arg2.Equals(TfsSqlScalarFunc(Other).Arg2) Then
              Exit;
          End;
        sfCast:
          Begin
            If Arg1 <> Nil Then
              If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
                Exit;
          End;
        sfRound:
          Begin
            If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
              Exit;
          End;
        sfArray:
          Begin
            If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
              Exit;
            If Not Arg2.Equals(TfsSqlScalarFunc(Other).Arg2) Then
              Exit;
            If Arg3 <> Nil Then
              If Not Arg3.Equals(TfsSqlScalarFunc(Other).Arg3) Then
                Exit;
          End;
        sfSubstring:
          Begin
            If Not Arg1.Equals(TfsSqlScalarFunc(Other).Arg1) Then
              Exit;
            If Not Arg2.Equals(TfsSqlScalarFunc(Other).Arg2) Then
              Exit;
            If Not (
              BothNil(Arg3, TfsSqlScalarFunc(Other).Arg3)
              Or (BothNonNil(Arg3, TfsSqlScalarFunc(Other).Arg3)
              And Arg3.Equals(TfsSqlScalarFunc(Other).Arg3))) Then
              Exit;
          End;
        sfTrim:
          Begin
            If Not (
              BothNil(Arg1, TfsSqlScalarFunc(Other).Arg1)
              Or (BothNonNil(Arg1, TfsSqlScalarFunc(Other).Arg1)
              And Arg1.Equals(TfsSqlScalarFunc(Other).Arg1))) Then
              Exit;
            If Not (
              BothNil(Arg2, TfsSqlScalarFunc(Other).Arg2)
              Or (BothNonNil(Arg2, TfsSqlScalarFunc(Other).Arg2)
              And Arg2.Equals(TfsSqlScalarFunc(Other).Arg2))) Then
              Exit;
          End;
      End;
      Result := True;
    End;
End;
{--------}

Function TfsSqlScalarFunc.GetDecimals: Integer;
Begin
  Result := FDecimals
End;

Function TfsSqlScalarFunc.GetRound: TRound;
Begin
  Result := fRoundType;
End;

Function TfsSqlScalarFunc.GetBlobLevel: TDataCompLevel;
Begin
  Result := blNone;
End;
{--------}

Function TfsSqlScalarFunc.GetSize: Integer;
Var
  S: String;
Begin
  {should only be called on text functions}
  Case SQLFunction Of
    sfCase:
      Result := CaseExp.GetSize;
    sfLower,
      sfUpper,
      sfSubstring, sfWeekNo:
      Result := Arg1.GetSize;
    sfTrim:
      If Arg2 = Nil Then
        Result := Arg1.GetSize
      Else
        Result := Arg2.GetSize;
    sfCoalesce:
      Result := CoalesceExp.GetSize;
    sfCurrentUser:
      Begin
        S := GetValue(-1);
        Result := length(S);
      End;
    sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD: Result := 2;
    sfNullIf:
      Result := Arg1.GetSize;
    sfCast,
      sfRound: Result := FSize
    Else If Arg1 <> Nil Then
      Result := Arg1.GetSize;
  End;
End;
{--------}

Function TfsSqlScalarFunc.GetTitle(Const Qualified: Boolean): String; {!!.11}
Begin
  Result := FuncStr[SQLFunction];
End;
{--------}

Procedure TfsSqlScalarFunc.CheckType;
Begin
  Case SQLFunction Of
    sfCase:
      FType := CaseExp.GetType;
    sfCharlen:
      Begin
        Arg1.MatchType(fstShortString);
        FType := fstInt32;
      End;
    sfCoalesce:
      FType := CoalesceExp.GetType;
    sfCurrentDate:
      FType := fstDate;
    sfCurrentTime:
      FType := fstTime;
    sfCurrentTimestamp:
      FType := fstDateTime;
    sfCurrentUser:
      FType := fstShortString;
    sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD: FType := fstBoolean;
    sfLower:
      Begin
        Arg1.MatchType(fstShortString);
        FType := fstShortString;
      End;
    sfUpper:
      Begin
        Arg1.MatchType(fstShortString);
        FType := fstShortString;
      End;

    sfFlags, sfODD:
      Begin
        FType := fstBoolean;
      End;
    sfPosition:
      Begin
        Arg1.MatchType(fstShortString);
        Arg2.MatchType(fstShortString);
        FType := fstInt32;
      End;
    sfSubstring:
      Begin
        Arg1.MatchType(fstShortString);
        Arg2.MatchType(fstInt32);
        If Arg3 <> Nil Then
          Arg3.MatchType(fstInt32);
        FType := fstShortString;
      End;
    sfTrim:
      Begin
        If Arg1 <> Nil Then
          Arg1.MatchType(fstShortString);
        If Arg2 <> Nil Then
          Arg2.MatchType(fstShortString);
        FType := fstShortString;
      End;
    sfExtract:
      Begin
        Arg1.MatchType(fstDateTime);
        FType := fstInt32;
      End;
    sfNullIf:
      FType := Arg1.GetType;
    sfAbs, sfExp, sfLog, sfRand, sfPower:
      FType := fstExtended;
    sfMod:
      Begin
        FType := fstInt32;
      End;
    sfDiv:
      Begin
        FType := fstInt64;
      End;
    sfCast:
      Begin
        TypeKnown := True;
      End;
    sfround:
      Begin
        Arg1.MatchType(fstExtended);
        FType := fstExtended;
      End;
    sfWeekNo:
      Begin
        Case Arg1.GetType Of
          fstDateTime: Arg1.MatchType(fstDateTime);
          fstDate: Arg1.MatchType(fstDate);
          fstTime: Arg1.MatchType(fstTime);
          fstShortString,
            fstNullString,
            fstVarNullString,
            fstWideString,
            fstVarWideString: Arg1.MatchType(fstShortString);
        End;
        fType := fstint16;
      End;
    sfArray:
      Begin
        Case Arg1.GetType Of
          fstArrayUInt8:
            Begin
              Arg1.MatchType(fstUInt8);
              fType := fstUInt8;
            End;
          fstArrayUInt16:
            Begin
              Arg1.MatchType(fstUInt16);
              fType := fstUInt16;
            End;
          fstArrayInt32:
            Begin
              Arg1.MatchType(fstInt32);
              fType := fstInt32;
            End;
          fstArrayDouble:
            Begin
              Arg1.MatchType(fstDouble);
              fType := fstDouble;
            End;
        End;
      End;
    sfCeil, sfFloor:
      Case Arg1.GetType Of
        fstDate..fstDateTime:
          FType := Arg1.GetType;
        Else
          FType := fstExtended;
      End;
    Else
      Assert(False);
  End;
  TypeKnown := True;
End;
{--------}

Function TfsSqlScalarFunc.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    CheckType;
  Result := FType;
End;
{Begin !!.13}
{--------}

Function ConvertBLOBToString(Const Value: Variant): String;
{ Converts a BLOB value to a String value.
Assumption: Value is always a Var array of Byte }
Var
  ResultLen: Longint;
  VPtr: PAnsiChar;
Begin
  ResultLen := VarArrayHighBound(Value, 1);
  SetLength(Result, ResultLen);
  VPtr := VarArrayLock(Value);
  Try
    Move(VPtr^, Result[1], ResultLen);
  Finally
    VarArrayUnlock(Value);
  End;
End;
{End !!.13}
{--------}

Function TfsSqlScalarFunc.GetValue(aArrayIndex: Integer): Variant;
Const
  Ch64: Array[0..63] Of Int64 = (0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192,
    16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216,
    33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 4294967296,
    8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 274877906944, 549755813888,
    1099511627776, 2199023255552, 4398046511104, 8796093022208, 17592186044416, 35184372088832,
    70368744177664, 140737488355328, 281474976710656, 562949953421312, 1125899906842624,
    2251799813685248, 4503599627370496, 9007199254740992, 18014398509481984, 36028797018963968,
    72057594037927936, 144115188075855872, 288230376151711744, 576460752303423488, 1152921504606846976,
    2305843009213693952, 4611686018427387904);

Var
  S: String;
  WS, WS2: widestring; {!!.11}
  I1, I2, code, i: Integer;
  Y, M, D: Word;
  Hour, Min, Sec, MSec: Word;
  Ch: Char;
  DT: TDateTime;
  V, V2, V3: Variant; {!!.11}
  E, E2: Extended;
  r64, l64, I64: Int64;
  StV: TstringList;
  V4: Variant;
  r: TRound;
  By: Byte;
  Wo: Word;
  Db: Extended;
  Dbl: Double;
  Sh: Shortint;
  Sm: Smallint;
  Si: Single;
  Cu: Currency;
  co: Comp;
  bo: Boolean;
  tm: TTimeStamp;
  t: TFSSqlTableProxy;

  Function ExtractValue(Const Value: String; Var Pos: Integer): String;
  Var
    I: Integer;
  Begin
    I := Pos;
    While (I <= Length(Value)) And (Value[I] <> ';') Do
      Inc(I);
    Result := Trim(Copy(Value, Pos, I - Pos));
    If (I <= Length(Value)) And (Value[I] = ';') Then
      Inc(I);
    Pos := I;
  End;

  Procedure AddValues(Const Value: String);
  Var
    Pos: Integer;
    s: String;
  Begin
    Pos := 1;
    While Pos <= Length(Value) Do
      Begin
        s := ExtractFieldName(Value, Pos);
        If s <> '' Then
          StV.add(s);
      End;
  End;

  Function WeekNo(ADate: TDateTime): Integer;
  Var
    AYear, AMonth, ADay: Word;
    //TheYear : Word;      // week, to which the week belongs
    AWeekDay: Word; // Day of week For 1. Jan
    ANumDays: Integer; // Days since 1. Jan
    AFirstDayOfYear: TDateTime; // Date of 1. Jn
  Begin
    Try
      DecodeDate(ADate, AYear, AMonth, ADay);
      //TheYear := AYear;
      AFirstDayOfYear := EncodeDate(AYear, 1, 1);
      AWeekDay := SysUtils.DayOfWeek(AFirstDayOfYear);
      ANumDays := Trunc(Int(ADate) - AFirstDayOfYear) + (7 - SysUtils.DayOfWeek(ADate - 1)) +
        (7 * Ord(AWeekDay In [2..5]));
      Result := ANumDays Div 7;
      If Result = 0 Then
        Begin
          If (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 1, 1)) > 5) Or
            (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 12, 31)) < 5) Then
            Result := 52
          Else
            Result := 53;
          //TheYear := AYear - 1;
        End
      Else If Result = 53 Then
        If (AWeekDay > 5) Or (SysUtils.DayOfWeek(EncodeDate(AYear, 12, 31)) < 5) Then
          Begin
            Result := 1;
            //TheYear := AYear + 1;
          End;
    Except
      Result := 0;
    End;
  End;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  Case SQLFunction Of
    sfCase:
      Result := CaseExp.GetValue(aArrayIndex);
    sfWeekNo:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        DT := V;
        If VarIsNull(V) Then
          Result := V
        Else
          Result := WeekNo(Dt);
      End;
    sfCharlen:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else If (VarType(V) And VarTypeMask = varByte) Then
          Result := VarArrayHighBound(V, 1)
        Else
          Result := length(V);
      End;
    sfFlags:
      Begin
        V := Arg1.GetValue(aArrayIndex); // expession, field
        V2 := Arg2.GetValue(aArrayIndex); // value
        If VarIsNull(V) Or VarIsNull(V2) Then
          Result := False
        Else
          Begin
            Result := False;
            {$IFDEF IsNoVariantInt64}
            If TVarData(V).VType = VT_DECIMAL Then
              l64 := Decimal(V).lo64
            Else
              Begin
                co := v;
                l64 := Round(co);
              End;
            {$ELSE}
            l64 := v;
            {$ENDIF}
            StV := TStringList.Create;
            Try
              AddValues(V2);
              For i := 0 To StV.Count - 1 Do
                Begin
                  r64 := strtoint64(StV[i]);
                  Result := (l64 And r64) > 0;
                  If Result Then
                    Begin
                      Result := False;
                      For code := 0 To 63 Do
                        Begin
                          If r64 = ch64[code] Then
                            Result := True;
                        End;
                    End;
                  If Result Then
                    break;
                End;
            Finally
              StV.free;
            End;
          End;
      End;
    sfODD:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := False
        Else
          Begin
            Result := False;
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              Begin
                i64 := Decimal(V).lo64;
              End
            Else If (TVarData(V).VType = VT_E80) Then
              Begin
                i64 := Round(Decimal(V).ext80);
              End
            Else
              Begin
                co := V;
                i64 := Round(co);
              End;
            {$ELSE}
            i64 := v;
            {$ENDIF}
            If i64 <> 0 Then
              Begin
                l64 := i64 Div 2;
                l64 := l64 * 2;
                Result := l64 = i64;
              End
            Else
              Result := True;
          End;
      End;

    sfCoalesce:
      Result := CoalesceExp.GetValue(aArrayIndex);
    sfCurrentDate:
      Result := Owner.StartDate;
    sfCurrentTime:
      Result := Owner.StartTime;
    sfCurrentTimestamp:
      Result := Owner.StartDateTime;
    sfCurrentUser:
      Begin
        If Owner.FClientName = '' Then
          Result := '' //IntToStr(Owner.FClientID)
        Else
          Result := Owner.FClientName;
      End;
    sfISUNDELETEDRECORD:
      Begin
        Result := null;
        If Self.OwnerSelect <> Nil Then
          Begin
            T := TFSSqlTableProxy(Self.OwnerSelect.TablesReferencedByOrder.Objects[0]);
            If T <> Nil Then
              Result := t.ISUNDELETEDRECORD;
          End
        Else If Self.Owner.Update <> Nil Then
          Begin
            T := Self.Owner.Update.t;
            If T <> Nil Then
              Result := t.ISUNDELETEDRECORD;
          End
        Else If Self.Owner.Delete <> Nil Then
          Begin
            T := Self.Owner.Delete.t;
            If T <> Nil Then
              Result := t.ISUNDELETEDRECORD;
          End;
      End;
    sfISPROTECTDELETERECORD:
      Begin
        Result := null;
        If Self.OwnerSelect <> Nil Then
          Begin
            T := TFSSqlTableProxy(Self.OwnerSelect.TablesReferencedByOrder.Objects[0]);
            If T <> Nil Then
              Result := t.ISPROTECTDELETERECORD;
          End
        Else If Self.Owner.Update <> Nil Then
          Begin
            T := Self.Owner.Update.t;
            If T <> Nil Then
              Result := t.ISPROTECTDELETERECORD;
          End
        Else If Self.Owner.Delete <> Nil Then
          Begin
            T := Self.Owner.Delete.t;
            If T <> Nil Then
              Result := t.ISPROTECTDELETERECORD;
          End;
      End;
    sfISPROTECTUPDATERECORD:
      Begin
        Result := null;
        If Self.OwnerSelect <> Nil Then
          Begin
            T := TFSSqlTableProxy(Self.OwnerSelect.TablesReferencedByOrder.Objects[0]);
            If T <> Nil Then
              Result := t.ISPROTECTUPDATERECORD;
          End
        Else If Self.Owner.Update <> Nil Then
          Begin
            T := Self.Owner.Update.t;
            If T <> Nil Then
              Result := t.ISPROTECTUPDATERECORD;
          End
        Else If Self.Owner.Delete <> Nil Then
          Begin
            T := Self.Owner.Delete.t;
            If T <> Nil Then
              Result := t.ISPROTECTUPDATERECORD;
          End;
      End;
    sfISMARKASBADRECORD:
      Begin
        Result := null;
        If Self.OwnerSelect <> Nil Then
          Begin
            T := TFSSqlTableProxy(Self.OwnerSelect.TablesReferencedByOrder.Objects[0]);
            If T <> Nil Then
              Result := t.ISMARKASBADRECORD;
          End
        Else If Self.Owner.Update <> Nil Then
          Begin
            T := Self.Owner.Update.t;
            If T <> Nil Then
              Result := t.ISMARKASBADRECORD;
          End
        Else If Self.Owner.Delete <> Nil Then
          Begin
            T := Self.Owner.Delete.t;
            If T <> Nil Then
              Result := t.ISMARKASBADRECORD;
          End;
      End;
    sfISRECORDLOCKED:
      Begin
        Result := null;
        If Self.OwnerSelect <> Nil Then
          Begin
            T := TFSSqlTableProxy(Self.OwnerSelect.TablesReferencedByOrder.Objects[0]);
            If T <> Nil Then
              Result := t.IsRecordLocked;
          End
        Else If Self.Owner.Update <> Nil Then
          Begin
            T := Self.Owner.Update.t;
            If T <> Nil Then
              Result := t.IsRecordLocked;
          End
        Else If Self.Owner.Delete <> Nil Then
          Begin
            T := Self.Owner.Delete.t;
            If T <> Nil Then
              Result := t.IsRecordLocked;
          End;
      End;
    sfLower:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else If (VarType(V) And VarTypeMask = varByte) Then
          Result := AnsiLowerCase(ConvertBLOBToString(V))
        Else
          Result := AnsiLowerCase(V);
      End;
    sfUpper:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else If (VarType(V) And VarTypeMask = varByte) Then
          Result := AnsiUpperCase(ConvertBLOBToString(V))
        Else
          Result := AnsiUpperCase(V);
      End;
    sfPosition:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        V2 := Arg2.GetValue(aArrayIndex);
        If VarIsNull(V) Or VarIsNull(V2) Then
          Result := 0
        Else
          Begin
            WS := V;
            If WS = '' Then
              Result := 1
            Else
              Begin
                If (VarType(V2) And VarTypeMask = varByte) Then
                  WS2 := ConvertBLOBToString(V2)
                Else
                  WS2 := V2;
                Result := Pos(WS, WS2);
              End; { If }
          End; { If }
      End;

    sfSubstring:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            If (VarType(V) And VarTypeMask = varByte) Then
              S := ConvertBLOBToString(V)
            Else
              S := V;
            I1 := Arg2.GetValue(aArrayIndex);
            If Arg3 = Nil Then
              Result := copy(S, I1, length(S))
            Else
              Begin
                I2 := Arg3.GetValue(aArrayIndex);
                Result := copy(S, I1, I2);
              End;
          End;
      End;
    sfTrim:
      Begin
        If Arg2 = Nil Then
          Begin
            V := Arg1.GetValue(aArrayIndex);
            If VarIsNull(V) Then
              Begin
                Result := V;
                Exit;
              End;
            If (VarType(V) And VarTypeMask = varByte) Then
              S := ConvertBLOBToString(V)
            Else
              S := V;
            Ch := ' ';
          End
        Else If Arg1 = Nil Then
          Begin
            V := Arg2.GetValue(aArrayIndex);
            If VarIsNull(V) Then
              Begin
                Result := V;
                Exit;
              End;
            If (VarType(V) And VarTypeMask = varByte) Then
              S := ConvertBLOBToString(V)
            Else
              S := V;
            Ch := ' ';
          End
        Else
          Begin
            V := Arg1.GetValue(aArrayIndex);
            If VarIsNull(V) Then
              Begin
                Result := V;
                Exit;
              End;
            If (VarType(V) And VarTypeMask = varByte) Then
              S := ConvertBLOBToString(V)
            Else
              S := V;
            Ch := S[1];
            V := Arg2.GetValue(aArrayIndex);
            If VarIsNull(V) Then
              S := ''
            Else If (VarType(V) And VarTypeMask = varByte) Then
              S := ConvertBLOBToString(V)
            Else
              S := V;
          End;
        Case LTB Of
          ltbBoth:
            Begin
              While (length(S) > 0) And (S[1] = Ch) Do
                Delete(S, 1, 1);
              While (length(S) > 0) And (S[length(S)] = Ch) Do
                Delete(S, length(S), 1);
            End;
          ltbLeading:
            While (length(S) > 0) And (S[1] = Ch) Do
              Delete(S, 1, 1);
          ltbTrailing:
            While (length(S) > 0) And (S[length(S)] = Ch) Do
              Delete(S, length(S), 1);
        End;
        Result := S;
      End;
    sfExtract:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Begin
            Result := V;
            Exit;
          End;
        DT := V;
        Case XDef Of
          iYear:
            Begin
              DecodeDate(DT, Y, M, D);
              Result := Y;
            End;
          iMonth:
            Begin
              DecodeDate(DT, Y, M, D);
              Result := M;
            End;
          iDay:
            Begin
              DecodeDate(DT, Y, M, D);
              Result := D;
            End;
          iHour:
            Begin
              DecodeTime(DT, Hour, Min, Sec, MSec);
              Result := Hour;
            End;
          iMinute:
            Begin
              DecodeTime(DT, Hour, Min, Sec, MSec);
              Result := Min;
            End;
          Else
            //iSecond:
            Begin
              DecodeTime(DT, Hour, Min, Sec, MSec);
              Result := Sec;
            End;
        End;
      End;
    sfNullIf:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If V = Arg2.GetValue(aArrayIndex) Then
          Result := Null
        Else
          Result := V;
      End;
    sfAbs:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;

            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := Abs(E);
            {$ELSE}
            Result := Abs(V);
            {$ENDIF}
          End;
      End;
    sfCeil:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;

            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := Ceil(E);
            {$ELSE}
            Result := Ceil(V);
            {$ENDIF}
          End;
      End;
    sfFloor:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;

            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := Floor(E);
            {$ELSE}
            Result := Floor(V);
            {$ENDIF}
          End;
      End;
    sfExp:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;

            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := Exp(E);
            {$ELSE}
            Result := Exp(V);
            {$ENDIF}
          End;
      End;
    sfLog:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            {$IFDEF IsNoVariantInt64}
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;

            TVarData(Result).VType := VT_E80;
            Decimal(Result).ext80 := Ln(E);
            {$ELSE}
            Result := Ln(V);
            {$ENDIF}
          End;
      End;
    sfRand:
      Result := Random;
    sfDiv:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            V2 := Arg2.GetValue(aArrayIndex);
            If VarIsNull(V2) Then
              Result := V2
            Else
              Begin
                {$IFDEF IsNoVariantInt64}
                If (TVarData(V).VType = VT_DECIMAL) Then
                  Begin
                    i64 := Decimal(V).lo64;
                  End
                Else If (TVarData(V).VType = VT_E80) Then
                  Begin
                    i64 := Trunc(Decimal(V).ext80);
                  End
                Else
                  Begin
                    co := V;
                    i64 := Trunc(co);
                  End;

                If (TVarData(V2).VType = VT_DECIMAL) Then
                  Begin
                    l64 := Decimal(V2).lo64;
                  End
                Else If (TVarData(V2).VType = VT_E80) Then
                  Begin
                    l64 := Trunc(Decimal(V2).ext80);
                  End
                Else
                  Begin
                    co := V2;
                    l64 := Trunc(co);
                  End;

                TVarData(Result).VType := VT_DECIMAL;
                Decimal(Result).lo64 := i64 Div l64;
                {$ELSE}
                Result := V Div V2;
                {$ENDIF}
              End;
          End;
      End;
    sfMod:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            V2 := Arg2.GetValue(aArrayIndex);
            If VarIsNull(V2) Then
              Result := V2
            Else
              Begin
                {$IFDEF IsNoVariantInt64}
                If (TVarData(V).VType = VT_DECIMAL) Then
                  Begin
                    i64 := Decimal(V).lo64;
                  End
                Else If (TVarData(V).VType = VT_E80) Then
                  Begin
                    i64 := Trunc(Decimal(V).ext80);
                  End
                Else
                  Begin
                    co := V;
                    i64 := Trunc(co);
                  End;

                If (TVarData(V2).VType = VT_DECIMAL) Then
                  Begin
                    l64 := Decimal(V2).lo64;
                  End
                Else If (TVarData(V2).VType = VT_E80) Then
                  Begin
                    l64 := Trunc(Decimal(V2).ext80);
                  End
                Else
                  Begin
                    co := V2;
                    l64 := Trunc(co);
                  End;

                TVarData(Result).VType := VT_DECIMAL;
                Decimal(Result).lo64 := i64 Mod l64;
                {$ELSE}
                Result := V Mod V2;
                {$ENDIF}
              End;
          End;
      End;

    sfPower:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := V
        Else
          Begin
            V2 := Arg2.GetValue(aArrayIndex);
            If VarIsNull(V2) Then
              Result := V2
            Else
              Begin
                {$IFDEF IsNoVariantInt64}
                If (TVarData(V).VType = VT_DECIMAL) Then
                  E := Decimal(V).lo64
                Else If (TVarData(V).VType = VT_E80) Then
                  E := Decimal(V).ext80
                Else
                  E := V;

                If (TVarData(V2).VType = VT_DECIMAL) Then
                  E2 := Decimal(V2).lo64
                Else If (TVarData(V2).VType = VT_E80) Then
                  E2 := Decimal(V2).ext80
                Else
                  E2 := V;

                TVarData(Result).VType := VT_E80;
                Decimal(Result).ext80 := Power(E, E2);
                {$ELSE}
                Result := Power(V, V2);
                {$ENDIF}
              End;
          End;
      End;
    sfCast:
      Begin
        If IsCastNull Then
          Begin
            Result := null;
            Exit;
          End;
        If Arg1 = Nil Then
          Begin
            Result := null;
            Exit;
          End;
        // left value
        V := Arg1.GetValue(aArrayIndex);
        // result is right value
        //fType is right type value
        If VarIsNull(V) Then
          Begin
            Result := V;
            Exit;
          End;
        // right
        Case fType Of
          fstBoolean:
            Begin
              // left
              Case VarType(V) Of
                varBoolean:
                  Result := V;
                varInteger,
                  varByte,
                  {$IFDEF DCC6OrLater}
                varShortInt,
                  varWord,
                  varLongWord,
                  varInt64,
                  {$ENDIF}
                varSingle,
                  varDouble,
                  varCurrency,
                  {$IFDEF IsNoVariantInt64}
                14, 15,
                  {$ENDIF}
                varSmallint:
                  Result := V <> 0;
                Else
                  SqlError('Invalid cast');
              End;
            End;
          fstSingleChar,
            fstSingleWideChar,
            fstShortString..
            fstWideString:
            Begin
              Case VarType(V) Of
                varSmallint,
                  {$IFDEF IsNoVariantInt64}
                14,
                  {$ENDIF}
                varInteger,
                  {$IFDEF DCC6OrLater}
                varShortInt,
                  varWord,
                  varLongWord,
                  varInt64,
                  {$ENDIF}
                varByte:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If (TVarData(V).VType = VT_DECIMAL) Then
                      i64 := Decimal(V).lo64
                    Else
                      i64 := trunc(V);
                    {$ELSE}
                    i64 := V;
                    {$ENDIF}
                    S := IntToStr(i64);
                    Result := copy(S, 1, fSize);
                  End;
                varSingle,
                  varDouble,
                  {$IFDEF IsNoVariantInt64}
                15,
                  {$ENDIF}
                varCurrency:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    {$ELSE}
                    Db := V;
                    {$ENDIF}
                    S := fsFloatToStr(Db);
                    Result := copy(S, 1, fSize);
                  End;
                varDate:
                  Begin
                    dt := v;
                    tm := DateTimeToTimeStamp(DT);
                    If (tm.Date = 0) Or (tm.Date = 693594.0) Then
                      S := FormatDateTime('hh:nn:ss', dt)
                    Else
                      S := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
                    Result := copy(S, 1, fSize);
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    Result := copy(S, 1, fSize);
                  End;
                varBoolean:
                  Begin
                    If V Then
                      S := 'TRUE'
                    Else
                      S := 'FALSE';
                    Result := copy(S, 1, fSize);
                  End;
                Else
                  SqlError('Invalid cast');
              End;
            End;
          fstUInt8,
            fstUInt16,
            fstUInt32,
            fstInt8,
            fstInt16,
            fstInt32,
            fstInt64,
            fstAutoInc64,
            fstRecVersion,
            fstAutoInc32:
            Begin
              i64 := 0;
              Case VarType(V) Of
                varSmallint,
                  varInteger,
                  {$IFDEF DCC6OrLater}
                varShortInt,
                  varWord,
                  varLongWord,
                  {$IFDEF IsNoVariantInt64}
                14,
                  {$ENDIF}
                varInt64,
                  {$ENDIF}
                varByte:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If (TVarData(V).VType = VT_DECIMAL) Then
                      i64 := Decimal(V).lo64
                    Else
                      i64 := trunc(V);
                    {$ELSE}
                    i64 := V;
                    {$ENDIF}
                  End;
                varSingle,
                  varDouble,
                  {$IFDEF IsNoVariantInt64}
                15,
                  {$ENDIF}
                varCurrency:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    {$ELSE}
                    Db := V;
                    {$ENDIF}
                    i64 := Round(Db);
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    i64 := StrToInt(S);
                  End;
                varBoolean:
                  Begin
                    If V Then
                      i64 := 1
                    Else
                      i64 := 0;
                  End;
                Else
                  SqlError('Invalid cast');
              End;
              Case fType Of
                fstUInt8:
                  Begin
                    By := i64;
                    Result := By;
                  End;
                fstUInt16:
                  Begin
                    Wo := i64;
                    Result := Wo;
                  End;
                fstUInt32:
                  Begin
                    Db := i64;
                    Result := Db;
                  End;
                fstInt8:
                  Begin
                    Sh := i64;
                    Result := Sh;
                  End;
                fstInt16:
                  Begin
                    Sm := i64;
                    Result := Sm;
                  End;
                fstInt32,
                  fstAutoInc32:
                  Begin
                    I := i64;
                    Result := I;
                  End;
                fstInt64, fstAutoInc64, fstRecVersion:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    TVarData(Result).VType := VT_DECIMAL;
                    Decimal(Result).lo64 := i64;
                    {$ELSE}
                    Result := i64;
                    {$ENDIF}
                  End;
              End;
            End;
          fstSingle,
            fstDouble,
            fstExtended,
            fstCurrency:
            Begin
              Db := 0;
              Case VarType(V) Of
                varSmallint,
                  varInteger,
                  {$IFDEF DCC6OrLater}
                varShortInt,
                  varWord,
                  {$IFDEF IsNoVariantInt64}
                14,
                  {$ENDIF}
                varLongWord,
                  varInt64,
                  {$ENDIF}
                varByte:
                  Begin
                    Db := V;
                  End;
                varSingle,
                  varDouble,
                  {$IFDEF IsNoVariantInt64}
                15,
                  {$ENDIF}
                varCurrency:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    {$ELSE}
                    Db := V;
                    {$ENDIF}
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    Db := StrToFloat(S);
                  End;
                varBoolean:
                  Begin
                    If V Then
                      Db := 1
                    Else
                      Db := 0;
                  End;
                varDate:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If TVarData(v).VType = VT_DECIMAL Then
                      Db := Decimal(v).lo64
                    Else If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    dbl := Db;
                    Db := TDateTime(dbl);
                    {$ELSE}
                    Db := Double(TDateTime(V));
                    {$ENDIF}
                  End;
                Else
                  SqlError('Invalid cast');
              End;
              Case fType Of
                fstSingle:
                  Begin
                    db := RoundExtended(db, Decimals, RoundType);
                    Si := Db;
                    Result := Si;
                  End;
                fstDouble:
                  Begin
                    db := RoundExtended(db, Decimals, RoundType);
                    Result := Db;
                  End;
                fstCurrency:
                  Begin
                    db := RoundExtended(db, Decimals, RoundType);
                    Cu := Db;
                    Result := Cu;
                  End;
                fstExtended:
                  Begin
                    db := RoundExtended(db, Decimals, RoundType);
                    {$IFDEF IsNoVariantInt64}
                    TVarData(Result).VType := VT_E80;
                    Decimal(Result).ext80 := Db;
                    {$ELSE}
                    Result := Db;
                    {$ENDIF}
                  End;
              End;
            End;
          fstDate:
            Begin
              Case VarType(V) Of
                varDouble:
                  Begin
                    Dbl := V;
                    Dbl := int(Dbl);
                    Dt := TDateTime(Dbl);
                    Result := Dt;
                  End;
                {$IFDEF IsNoVariantInt64}
                14:
                  Begin
                    Db := Decimal(v).lo64;
                    Dbl := int(Db);
                    Dt := TDateTime(Dbl);
                    Result := DT;
                  End;
                15:
                  Begin
                    Db := Decimal(v).ext80;
                    Dbl := int(Db);
                    Dt := TDateTime(Dbl);
                    Result := DT;
                  End;
                {$ENDIF}
                varDate:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If TVarData(v).VType = VT_DECIMAL Then
                      Db := Decimal(v).lo64
                    Else If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    dbl := Db;
                    Result := int(dbl);
                    {$ELSE}
                    Result := trunc(V);
                    {$ENDIF}
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    If fsIsValidTimestamp(S) Then
                      Begin
                        Dt := VrStrToTimestamp(S);
                        Result := Dt;
                      End
                    Else If fsIsValidDate(S) Then
                      Begin
                        Dt := VrStrToDate(S);
                        Result := Dt;
                      End
                    Else If fsIsValidtime(S) Then
                      Begin
                        Dt := VrStrTotime(S);
                        Result := Dt;
                      End
                    Else
                      SqlError('Invalid cast');
                  End;
                Else
                  SqlError('Invalid cast');
              End;
            End;
          fstTime:
            Begin
              Case VarType(V) Of
                {$IFDEF IsNoVariantInt64}
                14:
                  Begin
                    dB := Decimal(v).lo64;
                    Dbl := DB;
                    Dbl := frac(dbl);
                    Result := TDateTime(Dbl);
                  End;
                15:
                  Begin
                    dB := Decimal(v).ext80;
                    Dbl := DB;
                    Dbl := frac(dbl);
                    Result := TDateTime(Dbl);
                  End;
                {$ENDIF}
                varDouble:
                  Begin
                    Dbl := V;
                    Dbl := frac(dbl);
                    Result := TDateTime(Dbl);
                  End;
                varDate:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If TVarData(v).VType = VT_DECIMAL Then
                      Db := Decimal(v).lo64
                    Else If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    Dbl := frac(db);
                    Result := TDateTime(Dbl);
                    {$ELSE}
                    Dbl := frac(v);
                    Result := TDateTime(Dbl);
                    {$ENDIF}
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    If fsIsValidTimestamp(S) Then
                      Begin
                        Dt := VrStrToTimestamp(S);
                        Result := Dt;
                      End
                    Else If fsIsValidDate(S) Then
                      Begin
                        Dt := VrStrToDate(S);
                        Result := Dt;
                      End
                    Else If fsIsValidtime(S) Then
                      Begin
                        Dt := VrStrTotime(S);
                        Result := Dt;
                      End
                    Else
                      SqlError('Invalid cast');
                  End;
                Else
                  SqlError('Invalid cast');
              End;
            End;
          fstDateTime:
            Begin
              Case VarType(V) Of
                {$IFDEF IsNoVariantInt64}
                14:
                  Begin
                    dB := Decimal(v).lo64;
                    Dbl := DB;
                    Result := TDateTime(Dbl);
                  End;
                15:
                  Begin
                    dB := Decimal(v).ext80;
                    Dbl := DB;
                    Result := TDateTime(Dbl);
                  End;
                {$ENDIF}
                varDouble:
                  Begin
                    Dbl := V;
                    Result := TDateTime(Dbl);
                  End;
                varDate:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    If TVarData(v).VType = VT_DECIMAL Then
                      Db := Decimal(v).lo64
                    Else If (TVarData(V).VType = VT_E80) Then
                      Db := Decimal(V).ext80
                    Else
                      Db := V;
                    dbl := Db;
                    Result := TDateTime(Dbl);
                    {$ELSE}
                    Result := V;
                    {$ENDIF}
                  End;
                varOleStr,
                  varString:
                  Begin
                    S := V;
                    If fsIsValidTimestamp(S) Then
                      Begin
                        Dt := VrStrToTimestamp(S);
                        Result := Dt;
                      End
                    Else If fsIsValidDate(S) Then
                      Begin
                        Dt := VrStrToDate(S);
                        Result := Dt;
                      End
                    Else If fsIsValidtime(S) Then
                      Begin
                        Dt := VrStrTotime(S);
                        Result := Dt;
                      End
                    Else
                      SqlError('Invalid cast');
                  End;
                Else
                  SqlError('Invalid cast');
              End;
            End;
          Else
            SqlError('Invalid cast');
        End;

      End;
    sfArray:
      Begin
        If (Arg1 = Nil) Or (Arg2 = Nil) Then
          Begin
            Result := null;
            SqlError('Invalid Array');
          End
        Else
          Begin
            V := Arg2.GetValue(-1);
            If v < 0 Then
              SqlError('Invalid Array Index: ' + v)
            Else
              Result := Arg1.GetValue(v);
          End;
      End;
    sfRound:
      Begin
        V := Arg1.GetValue(aArrayIndex);
        If VarIsNull(V) Then
          Result := Null
        Else
          Begin
            If FSize <= 0 Then
              FSize := 20;
            If fRoundType = rNone Then
              fRoundType := rMathematical;

            {$IFDEF IsNoVariantInt64}
            TVarData(Result).VType := VT_E80;
            If (TVarData(V).VType = VT_DECIMAL) Then
              E := Decimal(V).lo64
            Else If (TVarData(V).VType = VT_E80) Then
              E := Decimal(V).ext80
            Else
              E := V;
            Decimal(Result).ext80 := RoundExtended(E, fDecimals, fRoundType);
            {$ELSE}
            E := V;
            Result := RoundExtended(E, fDecimals, fRoundType);
            {$ENDIF}
          End;
      End;
    Else
      Assert(False);
  End;
End;

Function TfsSqlScalarFunc.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Function TfsSqlScalarFunc.IsFieldFrom(Table: TFSSqlTableProxy;
  Var FieldReferenced: TfsSqlFieldProxy): Boolean;
Var
  SameCase: Boolean;
Begin
  If SQLFunction In [sfUpper, sfLower] Then
    Result := Arg1.IsFieldFrom(Table, FieldReferenced, SameCase)
  Else
    Result := False;
End;

Procedure TfsSqlScalarFunc.MatchType(ExpectedType: TfsFieldType);
Begin
  Case SQLFunction Of
    sfCast:
      Begin
        If ExpectedType In [fstSingleChar, fstSingleWideChar, fstShortString, fstVarNullString, fstNullString, fstWideString, fstVarWideString,
          fstBlob..ffcLastBLOBType] Then
          If Not (GetType In [fstSingleChar, fstSingleWideChar, fstShortString, fstVarNullString, fstNullString, fstWideString, fstVarWideString,
            fstBlob..ffcLastBLOBType]) Then
            TypeMismatch
          Else
        Else If ExpectedType In [fstDate, fstTime, fstDateTime] Then
          If Not (GetType In [fstDate, fstTime, fstDateTime]) Then
            TypeMismatch
          Else
        Else If ExpectedType In [fstUInt8, fstUInt16, fstUInt32, fstInt8, fstInt16, fstInt32,
          fstInt64, fstAutoInc32, fstAutoInc64, fstRecVersion, fstSingle, fstDouble, fstExtended, fstCurrency, fstBcd] Then
          If Not (GetType In [fstUInt8, fstUInt16, fstUInt32, fstInt8, fstInt16, fstInt32,
            fstInt64, fstAutoInc32, fstAutoInc64, fstRecVersion, fstSingle, fstDouble, fstExtended, fstCurrency, fstBcd]) Then
            TypeMismatch
          Else
        Else If GetType <> ExpectedType Then
          TypeMismatch;
      End
    Else
      Begin
        Case ExpectedType Of
          fstSingleChar,
            fstSingleWideChar,
            fstShortString,
            fstNullString,
            fstVarNullString,
            fstWideString,
            fstVarWideString,
            fstBLOB..fstBLOBGraphic:
            Case GetType Of
              fstSingleChar,
                fstSingleWideChar,
                fstShortString,
                fstNullString,
                fstVarNullString,
                fstWideString,
                fstVarWideString,
                fstBLOB..fstBLOBGraphic:
                ;
              Else
                TypeMismatch;
            End;
          fstUInt8..fstCurrency, fstBcd, fstRecVersion, fstDate..fstDateTime:
            Begin
              Case GetType Of
                fstUInt8..fstCurrency, fstBcd, fstRecVersion: ;
                fstDate..fstDateTime: ;
                Else
                  TypeMismatch;
              End;
            End;
          Else
            If GetType <> ExpectedType Then
              TypeMismatch;
        End;
      End;
  End;
End;
{--------}

Function TfsSqlScalarFunc.Reduce: Boolean;
Begin
  Case SQLFunction Of
    sfCase:
      Result := CaseExp.Reduce;
    sfCharlen:
      Result := Arg1.Reduce;
    sfCoalesce:
      Result := CoalesceExp.Reduce;
    sfCurrentDate:
      Result := False;
    sfCurrentTime:
      Result := False;
    sfCurrentTimestamp:
      Result := False;
    sfCurrentUser, sfISRECORDLOCKED,
      sfISUNDELETEDRECORD,
      sfISPROTECTDELETERECORD,
      sfISPROTECTUPDATERECORD,
      sfISMARKASBADRECORD:
      Result := False;
    sfLower:
      Result := Arg1.Reduce;
    sfUpper, sfWeekNo:
      Result := Arg1.Reduce;
    sfPosition, sfFlags:
      Begin
        Result := Arg1.Reduce;
        If Not Result And (Arg2 <> Nil) Then
          Result := Arg2.Reduce;
      End;
    sfSubstring:
      Begin
        Result := Arg1.Reduce Or Arg2.Reduce;
        If Not Result And (Arg3 <> Nil) Then
          Result := Arg3.Reduce;
      End;
    sfArray:
      Begin
        Result := Arg1.Reduce Or Arg2.Reduce;
        If (Arg3 <> Nil) Then
          Result := Result And Arg3.Reduce;
      End;
    sfTrim:
      Begin
        If Arg2 = Nil Then
          Begin
            Result := Arg1.Reduce
          End
        Else If Arg1 = Nil Then
          Begin
            Result := Arg2.Reduce;
          End
        Else
          Begin
            Result := Arg1.Reduce Or Arg2.Reduce;
          End;
      End;
    sfExtract:
      Begin
        Result := Arg1.Reduce;
      End;
    sfNullIf:
      Begin
        Result := Arg1.Reduce Or Arg2.Reduce;
      End;
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfODD:
      Result := Arg1.Reduce;
    sfRand:
      Result := False;
    sfPower, sfMod, sfDiv:
      Result := Arg1.Reduce Or Arg2.Reduce;
    sfCast:
      Begin
        Result := Not Self.IsCastNull;
        If Self.IsCastNull Then Exit;
        If Arg2 <> Nil Then
          Begin
            If Arg3 <> Nil Then
              Result := Arg1.Reduce Or Arg2.Reduce Or Arg3.Reduce
            Else
              Result := Arg1.Reduce Or Arg2.Reduce;
          End
        Else
          Result := Arg1.Reduce;
      End;
    sfRound:
      Begin
        If Arg2 <> Nil Then
          Begin
            If Arg3 <> Nil Then
              Result := Arg1.Reduce Or Arg2.Reduce Or Arg3.Reduce
            Else
              Result := Arg1.Reduce Or Arg2.Reduce;
          End
        Else
          Result := Arg1.Reduce;
      End;
    Else
      Result := False;
  End;
End;
{--------}

Procedure TfsSqlScalarFunc.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

{====================================================================}

{===TfsSqlWhenClauseList=============================================}

Function TfsSqlWhenClauseList.AddWhenClause(Value: TfsSqlWhenClause): TfsSqlWhenClause;
Begin
  WhenClauseList.Add(Value);
  Result := Value;
End;
{--------}

Procedure TfsSqlWhenClauseList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlWhenClauseList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlWhenClauseList(Source).WhenClauseCount) Do
        AddWhenClause(TfsSqlWhenClause.Create(Self)).Assign(
          TfsSqlWhenClauseList(Source).WhenClause[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlWhenClauseList.CheckIsConstant;
Var
  i: Integer;
Begin
  FIsConstantChecked := True;
  For i := 0 To pred(WhenClauseCount) Do
    If Not WhenClause[i].IsConstant Then
      Begin
        FIsConstant := False;
        Exit;
      End;
  FIsConstant := True;
End;

Constructor TfsSqlWhenClauseList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  WhenClauseList := TList.Create;
End;
{--------}

Function TfsSqlWhenClauseList.DependsOn(Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(WhenClauseCount) Do
    If WhenClause[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Procedure TfsSqlWhenClauseList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(WhenClauseCount) Do
    WhenClause[i].Free;
  WhenClauseList.Clear;
End;
{--------}

Destructor TfsSqlWhenClauseList.Destroy;
Begin
  Clear;
  WhenClauseList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlWhenClauseList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  For i := 0 To pred(WhenClauseCount) Do
    WhenClause[i].EmitSQL(Stream);
End;
{--------}

Procedure TfsSqlWhenClauseList.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(WhenClauseCount) Do
    WhenClause[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlWhenClauseList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlWhenClauseList Then
    Begin
      If WhenClauseCount <> TfsSqlWhenClauseList(Other).WhenClauseCount Then
        Exit;
      For i := 0 To pred(WhenClauseCount) Do
        If Not WhenClause[i].Equals(TfsSqlWhenClauseList(Other).WhenClause[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlWhenClauseList.GetWhenClause(
  Index: Integer): TfsSqlWhenClause;
Begin
  Result := TfsSqlWhenClause(WhenClauseList[Index]);
End;
{--------}

Function TfsSqlWhenClauseList.GetWhenClauseCount: Integer;
Begin
  Result := WhenClauseList.Count;
End;
{--------}

Function TfsSqlWhenClauseList.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Procedure TfsSqlWhenClauseList.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

{====================================================================}

{===TfsSqlWhenClause=================================================}

Procedure TfsSqlWhenClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlWhenClause Then
    Begin
      If WhenExp = Nil Then
        WhenExp := TfsSqlCondExp.Create(Self);
      WhenExp.Assign(TfsSqlWhenClause(Source).WhenExp);
      ThenExp.Free;
      ThenExp := Nil;
      If assigned(TfsSqlWhenClause(Source).ThenExp) Then
        Begin
          ThenExp := TfsSqlSimpleExpression.Create(Self);
          ThenExp.Assign(TfsSqlWhenClause(Source).ThenExp);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlWhenClause.CheckIsConstant;
Begin
  FIsConstantChecked := True;
  FIsConstant := WhenExp.IsConstant And
    (Not assigned(ThenExp) Or
    ThenExp.IsConstant);
End;
{--------}

Function TfsSqlWhenClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := WhenExp.DependsOn(Table) Or
    ((ThenExp <> Nil) And ThenExp.DependsOn(Table));
End;

Destructor TfsSqlWhenClause.Destroy;
Begin
  WhenExp.Free;
  ThenExp.Free;
  Inherited;
End;

Procedure TfsSqlWhenClause.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' WHEN ');
  WhenExp.EmitSQL(Stream);
  WriteStr(Stream, ' THEN ');
  If ThenExp <> Nil Then
    ThenExp.EmitSQL(Stream)
  Else
    WriteStr(Stream, ' NULL');
End;
{--------}

Procedure TfsSqlWhenClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  WhenExp.EnumNodes(EnumMethod, Deep);
  If assigned(ThenExp) Then
    ThenExp.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlWhenClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlWhenClause)
    And (WhenExp.Equals(TfsSqlWhenClause(Other).WhenExp))
    And
    BothNil(ThenExp, TfsSqlWhenClause(Other).ThenExp)
    Or (BothNonNil(ThenExp, TfsSqlWhenClause(Other).ThenExp)
    And (ThenExp.Equals(TfsSqlWhenClause(Other).ThenExp)));
End;
{--------}

Function TfsSqlWhenClause.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;

Procedure TfsSqlWhenClause.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

{====================================================================}

{===TfsSqlCaseExpression=============================================}

Procedure TfsSqlCaseExpression.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlCaseExpression Then
    Begin
      If WhenClauseList = Nil Then
        WhenClauseList := TfsSqlWhenClauseList.Create(Self);
      WhenClauseList.Assign(TfsSqlCaseExpression(Source).WhenClauseList);
      ElseExp.Free;
      ElseExp := Nil;
      If Assigned(TfsSqlCaseExpression(Source).ElseExp) Then
        Begin
          ElseExp := TfsSqlSimpleExpression.Create(Self);
          ElseExp.Assign(TfsSqlCaseExpression(Source).ElseExp);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlCaseExpression.CheckIsConstant;
Var
  v: Variant;
Begin
  FIsConstantChecked := True;
  FIsConstant :=
    WhenClauseList.IsConstant And ((ElseExp = Nil) Or ElseExp.IsConstant);
  If FIsConstant Then
    Begin
      FIsConstant := False;
      v := getvalue(-1);
      {$IFDEF IsNoVariantInt64}
      If (TVarData(V).VType = VT_DECIMAL) Then
        Begin
          TVarData(ConstantValue).VType := VT_DECIMAL;
          Decimal(ConstantValue).lo64 := Decimal(V).lo64;
        End
      Else If (TVarData(V).VType = VT_E80) Then
        Begin
          TVarData(ConstantValue).VType := VT_E80;
          Decimal(ConstantValue).ext80 := Decimal(V).ext80;
        End
      Else
        ConstantValue := v;
      {$ELSE}
      ConstantValue := v;
      {$ENDIF}
      FIsConstant := True;
    End;
End;

Function TfsSqlCaseExpression.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := WhenClauseList.DependsOn(Table) Or
    (ElseExp <> Nil) And ElseExp.DependsOn(Table);
End;

Destructor TfsSqlCaseExpression.Destroy;
Begin
  WhenClauseList.Free;
  ElseExp.Free;
  Inherited;
End;

Procedure TfsSqlCaseExpression.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' CASE');
  WhenClauseList.EmitSQL(Stream);
  WriteStr(Stream, ' Else ');
  If ElseExp <> Nil Then
    ElseExp.EmitSQL(Stream)
  Else
    WriteStr(Stream, 'NULL');
  WriteStr(Stream, ' END');
End;
{--------}

Procedure TfsSqlCaseExpression.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  WhenClauseList.EnumNodes(EnumMethod, Deep);
  If ElseExp <> Nil Then
    ElseExp.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlCaseExpression.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlCaseExpression)
    And WhenClauseList.Equals(TfsSqlCaseExpression(Other).WhenClauseList)
    And (BothNil(ElseExp, TfsSqlCaseExpression(Other).ElseExp)
    Or (BothNonNil(ElseExp, TfsSqlCaseExpression(Other).ElseExp)
    And
    ElseExp.Equals(TfsSqlCaseExpression(Other).ElseExp)
    )
    );
End;
{--------}

Function TfsSqlCaseExpression.GetSize: Integer;
Var
  i: Integer;
Begin
  Result := 0;
  For i := 0 To pred(WhenClauseList.WhenClauseCount) Do
    If WhenClauseList.WhenClause[i].ThenExp <> Nil Then
      Result := FFMaxI(Result, WhenClauseList.WhenClause[i].ThenExp.GetSize);
  If ElseExp <> Nil Then
    Result := FFMaxI(Result, ElseExp.GetSize);
End;

Function TfsSqlCaseExpression.GetType: TfsFieldType;
Begin
  If WhenClauseList.WhenClause[0].ThenExp <> Nil Then
    Result := WhenClauseList.WhenClause[0].ThenExp.GetType
  Else
    Result := fstShortString; {actually, NULL}
End;

Function TfsSqlCaseExpression.GetValue(aArrayIndex: Integer): Variant;
Var
  i: Integer;
Begin
  If IsConstant Then
    Begin
      {$IFDEF IsNoVariantInt64}
      If TVarData(ConstantValue).VType = VT_E80 Then
        Begin
          TVarData(Result).VType := VT_E80;
          Decimal(Result).ext80 := Decimal(ConstantValue).ext80;
        End
      Else If (TVarData(ConstantValue).VType = VT_DECIMAL) Then
        Begin
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := Decimal(ConstantValue).lo64;
        End
      Else
        Result := ConstantValue;
      {$ELSE}
      Result := ConstantValue;
      {$ENDIF}
      Exit;
    End;
  For i := 0 To pred(WhenClauseList.WhenClauseCount) Do
    If WhenClauseList.WhenClause[i].WhenExp.AsBoolean Then
      Begin
        If WhenClauseList.WhenClause[i].ThenExp <> Nil Then
          Result := WhenClauseList.WhenClause[i].ThenExp.GetValue((aArrayIndex))
        Else
          Result := Null;
        Exit;
      End;
  If ElseExp <> Nil Then
    Result := ElseExp.GetValue((aArrayIndex))
  Else
    Result := Null;
End;
{--------}

Function TfsSqlCaseExpression.IsConstant: Boolean;
Begin
  If Not FIsConstantChecked Then
    CheckIsConstant;
  Result := FIsConstant;
End;
{--------}

Function TfsSqlCaseExpression.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(WhenClauseList.WhenClauseCount) Do
    If WhenClauseList.WhenClause[i].WhenExp.Reduce Then
      Begin
        Result := True;
        Exit;
      End
    Else If WhenClauseList.WhenClause[i].ThenExp <> Nil Then
      If WhenClauseList.WhenClause[i].ThenExp.Reduce Then
        Begin
          Result := True;
          Exit;
        End;
  If ElseExp <> Nil Then
    Result := ElseExp.Reduce
  Else
    Result := False;
End;

Procedure TfsSqlCaseExpression.ResetConstant;
Begin
  FIsConstantChecked := False;
  FIsConstant := False;
End;

{====================================================================}

{===TfsSqlMatchClause================================================}

Function TfsSqlMatchClause.AsBoolean(Const TestValue: Variant): Boolean;
Begin
  Result := SubQuery.Match(TestValue, Unique, Option)
End;
{--------}

Procedure TfsSqlMatchClause.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlMatchClause Then
    Begin
      Unique := TfsSqlMatchClause(Source).Unique;
      Option := TfsSqlMatchClause(Source).Option;
      SubQuery.Free;
      SubQuery := TfsSqlSELECT.Create(Self);
      SubQuery.Assign(TfsSqlMatchClause(Source).SubQuery);
    End
  Else
    AssignError(Source);
End;

Function TfsSqlMatchClause.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Result := SubQuery.DependsOn(Table);
End;

Destructor TfsSqlMatchClause.Destroy;
Begin
  SubQuery.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlMatchClause.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' MATCH');
  If Unique Then
    WriteStr(Stream, ' UNIQUE');
  Case Option Of
    moPartial:
      WriteStr(Stream, ' PARTIAL');
    moFull:
      WriteStr(Stream, ' FULL');
  End;
  WriteStr(Stream, '(');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlMatchClause.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlMatchClause.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlMatchClause)
    And (Unique = TfsSqlMatchClause(Other).Unique)
    And (Option = TfsSqlMatchClause(Other).Option)
    And (SubQuery.Equals(TfsSqlMatchClause(Other).SubQuery));
End;
{--------}

Procedure TfsSqlMatchClause.MatchType(ExpectedType: TfsFieldType);
Begin
  SubQuery.MatchType(ExpectedType, False);
End;

Function TfsSqlMatchClause.Reduce: Boolean;
Begin
  Result := SubQuery.Reduce;
End;

{====================================================================}
{ TfsSqlCoalesceExpression }

Function TfsSqlCoalesceExpression.AddArg(Value: TfsSqlSimpleExpression): TfsSqlSimpleExpression;
Begin
  ArgList.Add(Value);
  Result := Value;
End;
{--------}

Procedure TfsSqlCoalesceExpression.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlCoalesceExpression Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlCoalesceExpression(Source).ArgCount) Do
        AddArg(TfsSqlSimpleExpression.Create(Self)).Assign(
          TfsSqlCoalesceExpression(Source).Arg[i]);
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlCoalesceExpression.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  ArgList := TList.Create;
End;
{--------}

Procedure TfsSqlCoalesceExpression.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(ArgCount) Do
    Arg[i].Free;
  ArgList.Clear;
End;
{--------}

Destructor TfsSqlCoalesceExpression.Destroy;
Begin
  Clear;
  ArgList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlCoalesceExpression.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  WriteStr(Stream, ' COALESCE(');
  Arg[0].EmitSQL(Stream);
  For i := 1 To pred(ArgCount) Do
    Begin
      WriteStr(Stream, ' ,');
      Arg[i].EmitSQL(Stream);
    End;
  WriteStr(Stream, ')');
End;
{--------}

Procedure TfsSqlCoalesceExpression.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(ArgCount) Do
    Arg[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlCoalesceExpression.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlCoalesceExpression Then
    If ArgCount = TfsSqlCoalesceExpression(Other).ArgCount Then
      Begin
        For i := 0 To pred(ArgCount) Do
          If Not Arg[i].Equals(TfsSqlCoalesceExpression(Other).Arg[i]) Then
            Exit;
        Result := True;
      End;
End;
{--------}

Function TfsSqlCoalesceExpression.GetArg(
  Index: Integer): TfsSqlSimpleExpression;
Begin
  Result := TfsSqlSimpleExpression(ArgList[Index]);
End;
{--------}

Function TfsSqlCoalesceExpression.GetArgCount: Integer;
Begin
  Result := ArgList.Count;
End;
{--------}

Function TfsSqlCoalesceExpression.GetValue(aArrayIndex: Integer): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  For i := 0 To pred(ArgCount) Do
    Begin
      Result := Arg[i].GetValue(aArrayIndex);
      If Not VarIsNull(Result) Then
        Exit;
    End;
End;
{--------}

Function TfsSqlCoalesceExpression.DependsOn(
  Table: TFSSqlTableProxy): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(ArgCount) Do
    If Arg[i].DependsOn(Table) Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{--------}

Function TfsSqlCoalesceExpression.GetType: TfsFieldType;
Begin
  Result := Arg[0].GetType;
End;
{--------}

Function TfsSqlCoalesceExpression.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(ArgCount) Do
    If Arg[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;
{====================================================================}

Function TfsSqlCoalesceExpression.GetSize: Integer;
Var
  i: Integer;
Begin
  Result := 0;
  For i := 0 To pred(ArgCount) Do
    Result := FFMaxI(Result, Arg[i].GetSize);
End;

{ TFSSqlTableProxySubset }

Procedure TFSSqlTableProxySubset.Assign(
  Const Source: TFSSqlTableProxySubset);
Begin
  FTable := Source.Table;
  KeyRelation := Source.KeyRelation;
  Outer := Source.Outer;
  Opposite := Source.Opposite;
End;

Constructor TFSSqlTableProxySubset.Create;
Begin
  FTable := Table;
End;

Procedure TFSSqlTableProxySubset.Iterate(Iterator: TfsSqlTableIterator;
  Cookie: TffWord32);
Begin
  FTable.Iterate(Iterator, Cookie);
End;

Function TFSSqlTableProxySubset.UniqueValue: Boolean;
Begin
  Result :=
    (KeyRelation.RelationFieldCount = KeyRelation.RelationKeyFieldCount)
    And (KeyRelation.RelationOperators[KeyRelation.RelationKeyFieldCount - 1] = roEQ);
End;

Function TFSSqlTableProxySubset.ClosedSegment: Boolean;
Begin
  Result := KeyRelation.RelationOperatorB[KeyRelation.RelationKeyFieldCount - 1] <> roNone; {!!.11}
End;

Function TFSSqlTableProxySubset.KeyDepth: Integer;
Begin
  Result := KeyRelation.RelationFieldCount;
End;

Function TFSSqlTableProxySubset.EqualKeyDepth: Integer;
Begin
  Result := 0;
  While (Result < KeyRelation.RelationFieldCount)
    And (KeyRelation.RelationOperators[Result] = roEQ) Do
    inc(Result);
End;

{ TFSSqlTableProxySubsetList }

Function TFSSqlTableProxySubsetList.Add(
  TableProxySubset: TFSSqlTableProxySubset): TFSSqlTableProxySubset;
Begin
  FList.Add(TableProxySubset);
  Result := TableProxySubset;
End;

{!!.10 new}

Function TFSSqlTableProxySubsetList.Insert(
  TableProxySubset: TFSSqlTableProxySubset): TFSSqlTableProxySubset;
Begin
  FList.Insert(0, TableProxySubset);
  Result := TableProxySubset;
End;

Procedure TFSSqlTableProxySubsetList.Assign(
  Const Source: TFSSqlTableProxySubsetList);
Var
  i: Integer;
Begin
  Clear;
  For i := 0 To pred(Source.Count) Do
    Add(TFSSqlTableProxySubset.Create(Source.Item[i].Table)).Assign(Source.Item[i]);
  OuterJoin := Source.OuterJoin;
End;

Constructor TFSSqlTableProxySubsetList.Create;
Begin
  Assert(AOwner <> Nil);
  FOwner := AOwner;
  FList := TList.Create;
End;

Procedure TFSSqlTableProxySubsetList.Delete(Index: Integer);
Begin
  FList.Delete(Index);
End;

Procedure TFSSqlTableProxySubsetList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(FList.Count) Do
    Item[i].Free;
  FList.Clear;
End;

Destructor TFSSqlTableProxySubsetList.Destroy;
Begin
  Clear;
  FList.Free;
  Inherited;
End;

Function TFSSqlTableProxySubsetList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Function TFSSqlTableProxySubsetList.GetItem(
  Index: Integer): TFSSqlTableProxySubset;
Begin
  Result := TFSSqlTableProxySubset(FList[Index]);
End;

Function TFSSqlTableProxySubsetList.RelationUsed(
  Relation: TfsSqlCondFactor): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(Count) Do
    If Item[i].KeyRelation.CondF = Relation Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

Function TFSSqlTableProxySubsetList.DependencyExists(
  Table: TFSSqlTableProxy): Boolean;
Var
  i, j: Integer;
Begin
  For i := 0 To pred(Count) Do
    For j := 0 To Item[i].KeyRelation.RelationFieldCount - 1 Do
      Begin
        If Item[i].KeyRelation.ArgExpressions[j].DependsOn(Table) Then
          Begin
            Result := True;
            Exit;
          End;
        If (Item[i].KeyRelation.ArgExpressionB[j] <> Nil) {!!.11}
        And Item[i].KeyRelation.ArgExpressionB[j].DependsOn(Table) Then
          Begin {!!.11}
            Result := True;
            Exit;
          End;
      End;
  Result := False;
End;

Function TFSSqlTableProxySubsetList.ProcessLevel(Cookie1: TffWord32): Boolean;
Begin
  inc(FRecordsRead);
  inc(Owner.RecordsRead);
  { Time to check For timeout? }
  If FRecordsRead Mod 1000 = 0 Then
    FFCheckRemainingTime;
  Result := True; {continue}
  If Level = 0 Then
    Begin
      If FCondTerm.AsBoolean Then
        If Not SkipInner Then
          FCreateResultRecord;
      If SkipInner Then
        {SkipInner means we're writing NULL records For outer join
        records With no match, so we just need to know If there
          were any here; we don't need to see the rest, so stop reading:}
        Result := False;
      WroteRow := True;
    End
  Else
    Begin
      If FCondTerm.AsBooleanLevel(Level) Then
        Begin
          dec(Level);
          ReadSources;
          inc(Level);
        End;
    End;
End;

Procedure TFSSqlTableProxySubsetList.ReadSources;
Var
  i: Integer;
  NullLimit,
    BUsed: Boolean;
  KeyHasIntervals: Boolean; {!!.11}
  Vt: Variant;
Begin
  With Item[Level] Do
    Begin
      NullLimit := False;
      If KeyRelation.CondF <> Nil Then
        Begin
          Table.SetIndex(KeyRelation.NativeKeyIndex - 1);
          For i := 0 To KeyRelation.RelationFieldCount - 1 Do
            Begin
              Assert(KeyRelation.ArgExpressions[i] Is TfsSqlSimpleExpression);
              Vt := TfsSqlSimpleExpression(KeyRelation.ArgExpressions[i]).GetValue(-1);
              {$IFDEF IsNoVariantInt64}
              If (TVarData(Vt).VType = VT_DECIMAL) Then
                Begin
                  TVarData(V[i]).VType := VT_DECIMAL;
                  Decimal(V[i]).lo64 := Decimal(Vt).lo64;
                  TVarData(VB[i]).VType := VT_DECIMAL;
                  Decimal(VB[i]).lo64 := Decimal(Vt).lo64;
                End
              Else If (TVarData(Vt).VType = VT_E80) Then
                Begin
                  TVarData(V[i]).VType := VT_E80;
                  Decimal(V[i]).ext80 := Decimal(Vt).ext80;
                  TVarData(VB[i]).VType := VT_E80;
                  Decimal(VB[i]).ext80 := Decimal(Vt).ext80;
                End
              Else
                Begin
                  V[i] := Vt;
                  VB[i] := V[i];
                End;
              {$ELSE}
              V[i] := Vt;
              VB[i] := V[i];
              {$ENDIF}
              If VarIsNull(V[i]) Then
                NullLimit := True;
            End;

          KeyHasIntervals := False;
          For i := 0 To KeyRelation.RelationFieldCount - 2 Do
            If KeyRelation.RelationOperators[i] <> roEQ Then
              Begin
                KeyHasIntervals := True;
                break;
              End;

          {can't preevaluate open intervals on key alone because Of possible null values}
          For i := 0 To KeyRelation.RelationFieldCount - 1 Do
            Case KeyRelation.RelationOperators[i] Of
              roL, roG:
                Begin
                  KeyHasIntervals := True;
                  break;
                End;
            End;

          If Not KeyHasIntervals And
            Not KeyRelation.RelationKeyIsCaseInsensitive Then
            KeyRelation.CondF.MarkTrue;

          For i := 0 To KeyRelation.RelationFieldCount - 1 Do {!!.11}
            If KeyRelation.RelationOperatorB[i] <> roNone Then
              Begin {!!.11}
                Assert(KeyRelation.ArgExpressionB[i] Is TfsSqlSimpleExpression);
                Vt := TfsSqlSimpleExpression(KeyRelation.ArgExpressionB[i]).GetValue(-1);
                {$IFDEF IsNoVariantInt64}
                If (TVarData(Vt).VType = VT_DECIMAL) Then
                  Begin
                    TVarData(VB[i]).VType := VT_DECIMAL;
                    Decimal(VB[i]).lo64 := Decimal(Vt).lo64;
                  End
                Else If (TVarData(Vt).VType = VT_E80) Then
                  Begin
                    TVarData(VB[i]).VType := VT_E80;
                    Decimal(VB[i]).ext80 := Decimal(Vt).ext80;
                  End
                Else
                  Begin
                    VB[i] := Vt;
                  End;
                {$ELSE}
                VB[i] := Vt;
                {$ENDIF}
                If VarIsNull(VB[i]) Then
                  NullLimit := True;
              End;
          BUsed := False;
          If Not NullLimit Then
            Case KeyRelation.RelationOperators[KeyRelation.RelationFieldCount - 1] Of
              roEQ:
                Table.SetRange(V, VB, KeyRelation.RelationFieldCount, {!!.11}
                  KeyRelation.RelationFieldCount, True, True,
                  KeyRelation.RelationKeyIndexAsc);
              roLE:
                Case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] Of {!!.11}
                  roG:
                    Begin
                      Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, False, True,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  roGE:
                    Begin
                      Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, True, True,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  Else
                    Table.SetRange(V, V, KeyRelation.RelationFieldCount - 1,
                      KeyRelation.RelationFieldCount, True, True,
                      KeyRelation.RelationKeyIndexAsc);
                End;
              roL:
                Case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] Of {!!.11}
                  roG:
                    Begin
                      Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, False, False,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  roGE:
                    Begin //like
                      Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, True, False, /////
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  Else
                    Table.SetRange(V, V, KeyRelation.RelationFieldCount - 1,
                      KeyRelation.RelationFieldCount, True, False,
                      KeyRelation.RelationKeyIndexAsc);
                End;
              roG:
                Case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] Of {!!.11}
                  roLE:
                    Begin
                      Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, False, True,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  roL:
                    Begin // > and <
                      Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, False, False,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  Else
                    Table.SetRange(V, V, KeyRelation.RelationFieldCount,
                      KeyRelation.RelationFieldCount - 1, False, True,
                      KeyRelation.RelationKeyIndexAsc);
                End;
              roGE:
                Case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] Of {!!.11}
                  roLE:
                    Begin
                      Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, True, True,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  roL:
                    Begin
                      Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                        KeyRelation.RelationFieldCount, True, False,
                        KeyRelation.RelationKeyIndexAsc);
                      BUsed := True;
                    End;
                  Else
                    Table.SetRange(V, V, KeyRelation.RelationFieldCount,
                      KeyRelation.RelationFieldCount - 1, True, True,
                      KeyRelation.RelationKeyIndexAsc);
                End;
              Else
                Assert(False);
            End;
          If Not KeyHasIntervals And {!!.11}
          Not KeyRelation.RelationKeyIsCaseInsensitive And BUsed Then
            KeyRelation.RelationB[KeyRelation.RelationFieldCount - 1].MarkTrue; {!!.11}
        End
      Else
        Table.SetIndex(-1);
      {if not NullLimit then begin}{!!.11}
      WroteRow := False;
      If Not NullLimit Then {!!.11}
        Iterate(ProcessLevel, 0);
      If OuterJoin And Not WroteRow And (Level = 0) Then
        Begin
          Item[0].Table.NullRecord;
          FCreateResultRecord;
        End;
      {end;}{!!.11}
      If KeyRelation.CondF <> Nil Then
        Begin
          KeyRelation.CondF.MarkUnknown;
          If KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] <> roNone Then {!!.11}
            KeyRelation.RelationB[KeyRelation.RelationFieldCount - 1].MarkUnknown; {!!.11}
        End;
    End;
End;

Procedure TFSSqlTableProxySubsetList.Join;
Begin
  FCondTerm := CondTerm;
  CondTerm.SetLevelDep(Self);
  FCreateResultRecord := CreateResultRecord;
  Level := Count - 1;
  ReadSources;
End;

{ TfsSqlINSERT }

Procedure TfsSqlINSERT.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlINSERT Then
    Begin
      Clear;
      DefaultValues := TfsSqlINSERT(Source).DefaultValues;
      IsNull := TfsSqlINSERT(Source).IsNull;
      TableName := TfsSqlINSERT(Source).TableName;
      If TfsSqlINSERT(Source).InsertColumnList <> Nil Then
        Begin
          InsertColumnList := TfsSqlInsertColumnList.Create(Self);
          InsertColumnList.Assign(TfsSqlINSERT(Source).InsertColumnList);
        End;

      If TfsSqlINSERT(Source).TableExp <> Nil Then
        Begin
          TableExp := TfsSqlTableExp.Create(Self);
          TableExp.Assign(TfsSqlINSERT(Source).TableExp);
        End;

    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlINSERT.AddColumns(Node: TfsSqlNode);
Begin
  Node.AddColumnDef(Self);
End;
{--------}

Procedure TfsSqlINSERT.Bind;
Var
  i: Integer;
  F: TfsSqlFieldProxy;
Begin
  If InsertColumnList <> Nil Then
    InsertColumnList.EnumNodes(ClearBindings, False);
  T := Owner.FDatabase.TableByName(Self, TableName, omReadOnly, False, '', '');
  If T = Nil Then
    SQLError('Unable to open table: ' + TableName +
      '. Ensure the table exists And is not in use by ' +
      'another process.');

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  If InsertColumnList <> Nil Then
    InsertColumnList.EnumNodes(AddColumns, False);
  If Columns.Count = 0 Then
    Begin
      For i := 0 To T.FieldCount - 1 Do
        Begin
          F := T.Field(i);
          If Not F.CanUpdate Then
            SQLError('Changing fields of this Type is not currently supported ' +
              'through SQL:' + Columns[i]);
          Columns.AddObject(T.Field(i).Name, F);
        End;
    End
  Else
    Begin
      For i := 0 To Columns.Count - 1 Do
        Begin
          F := T.FieldByName(Columns[i]);
          If F = Nil Then
            SQLError('Unknown field For table ' + TableName + 'in INSERT statement:' +
              Columns[i]);

          If Not F.CanUpdate Then
            SQLError('Changing fields of this Type is not currently supported through SQL:' +
              Columns[i]);

          Columns.Objects[i] := F;
        End;
    End;
  Bound := True;
End;
{--------}

Procedure TfsSqlINSERT.Clear;
Begin
  TableName := '';
  InsertColumnList.Free;
  InsertColumnList := Nil;
  TableExp.Free;
  TableExp := Nil;
End;
{--------}

Procedure TfsSqlINSERT.ClearBindings(Node: TfsSqlNode);
Begin
  Node.ClearBinding;
End;
{--------}

Destructor TfsSqlINSERT.Destroy;
Begin
  Clear;
  If T <> Nil Then
    If T.Owner = Self Then
      Begin
        T.Owner := Nil;
        T.Free;
      End;
  Inherited;
End;
{--------}

Procedure TfsSqlINSERT.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, 'INSERT INTO ');
  WriteStr(Stream, TableName);
  WriteStr(Stream, ' ');
  If DefaultValues Then
    WriteStr(Stream, 'DEFAULT VALUES ')
  Else
    Begin
      If assigned(InsertColumnList) Then
        Begin
          WriteStr(Stream, '(');
          InsertColumnList.EmitSQL(Stream);
          WriteStr(Stream, ') ');
        End;
      If assigned(TableExp) Then
        TableExp.EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlINSERT.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(InsertColumnList) Then
    InsertColumnList.EnumNodes(EnumMethod, Deep);
  If assigned(TableExp) Then
    TableExp.EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlINSERT.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlINSERT)
    And (DefaultValues = TfsSqlINSERT(Other).DefaultValues)
    And (IsNull = TfsSqlINSERT(Other).IsNull)
    And (TableName = TfsSqlINSERT(Other).TableName)
    And (BothNil(InsertColumnList, TfsSqlINSERT(Other).InsertColumnList)
    Or (BothNonNil(InsertColumnList, TfsSqlINSERT(Other).InsertColumnList)
    And InsertColumnList.Equals(TfsSqlINSERT(Other).InsertColumnList))
    )
    And (BothNil(TableExp, TfsSqlINSERT(Other).TableExp)
    Or (BothNonNil(TableExp, TfsSqlINSERT(Other).TableExp)
    And TableExp.Equals(TfsSqlINSERT(Other).TableExp))
    );
End;
{Begin !!.13}
{--------}

Function CanInsert(Const SrcType, TgtType: TfsFieldType): Boolean;
Begin
  { According to our past rules, which are very lax, most every Type is
    compatible With all other types. New rules:
    - BLOBs may not be inserted into non-BLOB fields
    - strings may be inserted into BLOBs
    - strings cannot be inserted into numerics Or date time }
  If SrcType <> TgtType Then
    Case TgtType Of
      { Numerics & datetime values may be inserted into numerics. }
      fstUInt8..fstCurrency, fstBcd:
        Case SrcType Of
          fstUInt8..fstCurrency, fstRecVersion, fstBcd, fstDate..fstDateTime:
            Result := True;
          Else
            Result := False;
        End;
      fstDate..fstDateTime:
        { Numerics, datetime, and String values may be inserted into datetime
      columns. If a date is to be inserted via a string, the string must
        be preceded via the DATE keyword. }
        Case SrcType Of
          fstUInt8..fstCurrency, fstRecVersion, fstBcd,
            fstDate..fstDateTime:
            Result := True;
          Else
            Result := False;
        End; { Case }
      fstSingleChar,
        fstSingleWideChar,
        fstShortString..fstWideString:
        { Everything except BLOBs may be inserted into a String. }
        Case SrcType Of
          fstBLOB..fstBLOBGraphic:
            Result := False;
          Else
            Result := True;
        End; { Case }
      fstBLOB..fstBLOBGraphic:
        { Strings & other BLOBs may be inserted into BLOBs. }
        Case SrcType Of
          fstSingleChar, fstSingleWideChar,
            fstShortString..fstWideString,
            fstBLOB..fstBLOBGraphic:
            Result := True;
          Else
            Result := False;
        End; { Case }
      fstArrayDouble, fstArrayUInt8, fstArrayInt32, fstArrayUInt16:
        If SrcType = fstBLOB Then
          Result := False
        Else
          Result := True;
      Else
        Result := False;
    End { Case }
  Else
    Result := True;
End;
{End !!.13}
{--------}

Function TfsSqlINSERT.Execute(Var RowsAffected: Integer): TffResult;
{Revised !!.13}
Var
  i: Integer;
  ST: TFSSqlTableProxy;
  CPos: Longword;
Begin
  CPos := 0;
  Result := Owner.FDatabase.StartTransaction([T]);
  If Result = DBIERR_NONE Then
    Try
      RowsAffected := 0;
      If Not Bound Then
        Bind;
      { Make sure the target table can be modified. }
      Result := T.EnsureWritable;
      If Result <> DBIERR_NONE Then
        Begin
          Owner.FDatabase.AbortTransaction;
          Exit;
        End;

      { If inserting default values only then do so. }
      If DefaultValues Then
        Begin
          T.Insert;
          T.SetDefaults;
          Result := T.InsertPost;
          If Result = DBIERR_NONE Then
            Begin
              Owner.FDatabase.Commit;
              RowsAffected := 1;
            End
          Else
            Owner.FDatabase.AbortTransaction;
        End
      Else If TableExp <> Nil Then
        Begin
          { Values are coming from a valuelist Or subquery. }
          ST := TableExp.ResultTable;
          { Validate the number of source And target columns. }
          If ST.FieldCount <> Columns.Count Then
            SQLError('The number Of columns in the source clause must match ' +
              'the number Of columns in the INSERT statement.');

          { Do the field types match? }
          For i := 0 To Pred(ST.FieldCount) Do
            If Not ST.Field(i).isnull And Not CanInsert(ST.Field(i).GetType,
              TfsSqlFieldProxy(Columns.Objects[i]).GetType) Then
              SQLError(Format('The Type For source column %d (column name ' +
                '"%s") is incompatible with the Type For ' +
                'target column %d (column name "%s")',
                [i, ST.Field(i).Name, i, Columns[i]]));

          { Roll through the source table, inserting its rows into the result
          table. }
          ST.First;
          While Not ST.EOF Do
            Begin
              T.Insert;
              T.SetDefaults;
              For i := 0 To FFMinI(Pred(ST.FieldCount), Pred(Columns.Count)) Do
                TfsSqlFieldProxy(Columns.Objects[i]).SetValue(ST.Field(i).GetValue(-1), -1);
              Result := T.InsertPostNoDefaults;
              If CommitBy > 0 Then
                Begin
                  Inc(Cpos);
                  If Cpos = CommitBy Then
                    Begin
                      Cpos := 0;
                      Result := Owner.FDatabase.Commit;
                      If Result <> DBIERR_NONE Then
                        Begin
                          Result := Owner.FDatabase.AbortTransaction;
                          System.break;
                        End
                      Else
                        Begin
                          Result := Owner.FDatabase.StartTransaction([T]);
                          Result := T.EnsureWritable;
                          If Result <> DBIERR_NONE Then
                            System.break;
                        End;
                    End;
                End;
              If Result = DBIERR_NONE Then
                inc(RowsAffected)
              Else
                break;
              ST.Next;
            End;
          If Result = DBIERR_NONE Then
            Begin
              If Owner.FDatabase.InTransaction Then
                Owner.FDatabase.Commit;
            End
          Else
            Begin
              Owner.FDatabase.AbortTransaction;
              RowsAffected := 0;
              Assert(False, 'Unexpected INSERT scenario - Abort Transaction');
            End;
        End
      Else
        Assert(False, 'Unexpected INSERT scenario');
    Except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      Assert(False, 'Unexpected INSERT scenario - Abort Transaction');
    End
  Else If Result = DBIERR_LOCKED Then
    FSRaiseException(EfsException, fsStrResServer, fserrLockRejected,
      [ffcLockExclusive, '', T.Name])
  Else
    FSRaiseException(EfsException, fsStrResServer, Result, [T.Name]);
End;
{--------}
{!!.11 new}

Function TfsSqlINSERT.Reduce: Boolean;
Begin
  If TableExp <> Nil Then
    If TableExp.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

{ TfsSqlInsertItem }

Procedure TfsSqlInsertItem.AddColumnDef(Target: TfsSqlColumnListOwner);
Begin
  Target.Columns.Add(ColumnName);
End;

Procedure TfsSqlInsertItem.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlInsertItem Then
    Begin
      ColumnName := TfsSqlInsertItem(Source).ColumnName;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlInsertItem.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ColumnName);
End;

Procedure TfsSqlInsertItem.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;

Function TfsSqlInsertItem.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlInsertItem)
    And (ColumnName = TfsSqlInsertItem(Other).ColumnName);
End;

{ TfsSqlInsertColumnList }

Function TfsSqlInsertColumnList.AddItem(
  NewInsertColumn: TfsSqlInsertItem): TfsSqlInsertItem;
Begin
  FInsertColumnItemList.Add(NewInsertColumn);
  Result := NewInsertColumn;
End;

Procedure TfsSqlInsertColumnList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlInsertColumnList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlInsertColumnList(Source).InsertColumnCount) Do
        AddItem(TfsSqlInsertItem.Create(Self)).Assign(
          TfsSqlInsertColumnList(Source).InsertColumnItem[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlInsertColumnList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(InsertColumnCount) Do
    InsertColumnItem[i].Free;
  FInsertColumnItemList.Clear;
End;

Constructor TfsSqlInsertColumnList.Create(AParent: TfsSqlNode);
Begin
  Inherited;
  FInsertColumnItemList := TList.Create;
End;

Destructor TfsSqlInsertColumnList.Destroy;
Begin
  Clear;
  FInsertColumnItemList.Free;
  Inherited;
End;

Procedure TfsSqlInsertColumnList.EmitSQL(Stream: TStream);
Var
  i: Integer;
  First: Boolean;
Begin
  First := True;
  For i := 0 To pred(InsertColumnCount) Do
    Begin
      If First Then
        First := False
      Else
        WriteStr(Stream, ', ');
      InsertColumnItem[i].EmitSQL(Stream);
    End;
End;

Procedure TfsSqlInsertColumnList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(InsertColumnCount) Do
    InsertColumnItem[i].EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlInsertColumnList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlInsertColumnList Then
    Begin
      If InsertColumnCount <> TfsSqlInsertColumnList(Other).InsertColumnCount Then
        Exit;
      For i := 0 To pred(InsertColumnCount) Do
        If Not InsertColumnItem[i].Equals(TfsSqlInsertColumnList(Other).InsertColumnItem[i]) Then
          Exit;
      Result := True;
    End;
End;

Function TfsSqlInsertColumnList.GetInsertColumnCount: Integer;
Begin
  Result := FInsertColumnItemList.Count;
End;

Function TfsSqlInsertColumnList.GetInsertColumnItem(
  Index: Integer): TfsSqlInsertItem;
Begin
  Result := TfsSqlInsertItem(FInsertColumnItemList[Index]);
End;

Procedure TfsSqlInsertColumnList.SetInsertColumnItem(Index: Integer;
  Const Value: TfsSqlInsertItem);
Begin
  FInsertColumnItemList[Index] := Value;
End;

{ TfsSqlValueItem }

Procedure TfsSqlValueItem.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlValueItem Then
    Begin
      Simplex.Free;
      {Simplex := nil;}{unnecessary}
      Default := TfsSqlUpdateItem(Source).Default;
      IsNull := TfsSqlUpdateItem(Source).IsNull;
      Simplex := TfsSqlSimpleExpression.Create(Self);
      Simplex.Assign(TfsSqlValueItem(Source).Simplex);
    End
  Else
    AssignError(Source);
End;

Destructor TfsSqlValueItem.Destroy;
Begin
  Simplex.Free;
  Inherited;
End;

Procedure TfsSqlValueItem.EmitSQL(Stream: TStream);
Begin
  If Default Then
    WriteStr(Stream, 'DEFAULT ')
  Else If Simplex = Nil Then
    WriteStr(Stream, 'NULL ')
  Else
    Simplex.EmitSQL(Stream);
End;

Procedure TfsSqlValueItem.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(Simplex) Then
    Simplex.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlValueItem.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlValueItem)
    And (Default = TfsSqlValueItem(Other).Default)
    And (IsNull = TfsSqlValueItem(Other).IsNull)
    And (BothNil(Simplex, TfsSqlValueItem(Other).Simplex)
    Or (BothNonNil(Simplex, TfsSqlValueItem(Other).Simplex)
    And Simplex.Equals(TfsSqlValueItem(Other).Simplex)));
End;

Function TfsSqlValueItem.GetDecimals: Integer;
Begin
  If assigned(Simplex) Then
    Result := Simplex.GetDecimals
  Else
    Result := 0;
End;

Function TfsSqlValueItem.GetBlobLevel: TDataCompLevel;
Begin
  If assigned(Simplex) Then
    Result := Simplex.GetBlobLevel
  Else
    Result := blNone;
End;

Function TfsSqlValueItem.GetSize: Integer;
Begin
  If assigned(Simplex) Then
    Result := Simplex.GetSize
  Else
    Result := 1;
End;

Function TfsSqlValueItem.GetRound: TRound;
Begin
  If assigned(Simplex) Then
    Result := Simplex.GetRound
  Else
    Result := rNone;
End;

Function TfsSqlValueItem.GetType: TfsFieldType;
Begin
  If assigned(Simplex) Then
    Result := Simplex.GetType
  Else
    Result := fstBoolean;
End;

{ TfsSqlValueList }

Function TfsSqlValueList.AddItem(
  NewValue: TfsSqlValueItem): TfsSqlValueItem;
Begin
  FValueItemList.Add(NewValue);
  Result := NewValue;
End;

Procedure TfsSqlValueList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlValueList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlValueList(Source).ValueCount) Do
        AddItem(TfsSqlValueItem.Create(Self)).Assign(
          TfsSqlValueList(Source).ValueItem[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlValueList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(ValueCount) Do
    ValueItem[i].Free;
  FValueItemList.Clear;
End;

Constructor TfsSqlValueList.Create(AParent: TfsSqlNode);
Begin
  Inherited;
  FValueItemList := TList.Create;
End;

Destructor TfsSqlValueList.Destroy;
Begin
  Clear;
  FValueItemList.Free;
  If FResultTable <> Nil Then
    Begin
      If FResultTable.Owner = Self Then
        Begin
          FResultTable.Owner := Nil;
          FResultTable.Free;
        End;
    End;
  Inherited;
End;

Procedure TfsSqlValueList.EmitSQL(Stream: TStream);
Var
  i: Integer;
  First: Boolean;
Begin
  First := True;
  For i := 0 To pred(ValueCount) Do
    Begin
      If First Then
        First := False
      Else
        WriteStr(Stream, ', ');
      ValueItem[i].EmitSQL(Stream);
    End;
End;

Procedure TfsSqlValueList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(ValueCount) Do
    ValueItem[i].EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlValueList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlValueList Then
    Begin
      If ValueCount <> TfsSqlValueList(Other).ValueCount Then
        Exit;
      For i := 0 To pred(ValueCount) Do
        If Not ValueItem[i].Equals(TfsSqlValueList(Other).ValueItem[i]) Then
          Exit;
      Result := True;
    End;
End;

Procedure TfsSqlValueList.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Begin
  Raise Exception.Create('Not yet implemented');
End;

Function TfsSqlValueList.GetResultTable: TFSSqlTableProxy;
Var
  FieldDefList: TfsSqlFieldDefList;
  i: Integer;
  FldName: String; {!!.11}
  Field: TfsSqlFieldProxy; {!!.11}
Begin
  {Begin !!.13}
  If FResultTable <> Nil Then
    For i := 0 To pred(ValueCount) Do
      If (ValueItem[i].Simplex <> Nil) And
        Not ValueItem[i].Simplex.IsConstant Then
        Begin
          FResultTable.Owner := Nil;
          FResultTable.Free;
          FResultTable := Nil;
          break;
        End; { If }
  {End !!.13}
  If FResultTable = Nil Then
    Begin
      FieldDefList := TfsSqlFieldDefList.Create;
      Try
        {Begin !!.11}
        For i := 0 To pred(ValueCount) Do
          Begin
            FldName := 'Value_' + IntToStr(i + 1);
            Field := OwnerStmt.T.Field(i);
            If ValueItem[i].Default Or ValueItem[i].IsNull Then //?
              FieldDefList.AddField(FldName, '', Field.GetType, Field.GetSize,
                Field.GetDecimals, field.GetBlobLevel, Field.GetRound)
            Else //?
              FieldDefList.AddField(FldName, '', ValueItem[i].GetType,
                ValueItem[i].GetSize, ValueItem[i].GetDecimals, ValueItem[i].GetBlobLevel, ValueItem[i].GetRound);
          End; { For }
        {End !!.11}
        FResultTable := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList); {!!.10}
      Finally
        FieldDefList.Free;
      End;
      Owner.FDatabase.StartTransaction([Nil]);
      Try
        FResultTable.Insert;
        For i := 0 To pred(ValueCount) Do
          If ValueItem[i].Simplex <> Nil Then
            FResultTable.Field(i).SetValue(ValueItem[i].Simplex.GetValue(-1), -1)
          Else If ValueItem[i].Default Then
            FResultTable.Field(i).SetDefault
          Else If ValueItem[i].IsNull Then
            FResultTable.Field(i).SetFieldToNull
          Else
            FResultTable.Field(i).SetFieldToNull;
        FResultTable.InsertPost;
      Except
        Owner.FDatabase.AbortTransaction;
        FResultTable.Owner := Nil;
        FResultTable.Free;
        FResultTable := Nil;
        Raise;
      End;
      Owner.FDatabase.Commit;
    End;
  Result := FResultTable;
End;

Function TfsSqlValueList.GetValueCount: Integer;
Begin
  Result := FValueItemList.Count;
End;

Function TfsSqlValueList.GetValueItem(Index: Integer): TfsSqlValueItem;
Begin
  Result := TfsSqlValueItem(FValueItemList[Index]);
End;

Function TfsSqlValueList.Reduce: Boolean;
Begin
  Result := False;
End;

Procedure TfsSqlValueList.SetValueItem(Index: Integer;
  Const Value: TfsSqlValueItem);
Begin
  FValueItemList[Index] := Value;
End;

{ TfsSqlDELETE }

Procedure TfsSqlDELETE.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlDELETE Then
    Begin
      Clear;

      If TfsSqlDELETE(Source).TableRef <> Nil Then
        Begin
          TableRef := TfsSqlTableRef.Create(Self);
          TableRef.Assign(TfsSqlDELETE(Source).TableRef);
        End;

      If TfsSqlDELETE(Source).CondExpWhere <> Nil Then
        Begin
          CondExpWhere := TfsSqlCondExp.Create(Self);
          CondExpWhere.Assign(TfsSqlDELETE(Source).CondExpWhere);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlDELETE.Bind;
Begin
  Assert(TableRef <> Nil);
  T := TableRef.GetTable(Self, omReadWrite, False); {!!.11}
  If T = Nil Then
    SQLError('Unable to open table: ' + TableRef.SQLName + //TableName +
      '. Ensure the table exists And is not in use by ' +
      'another process.');

  If CondExpWhere <> Nil Then
    CondExpWhere.MatchType(fstBoolean);
  Bound := True;
End;

Function TfsSqlDELETE.BindField(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  Result := Nil;
  Assert(T <> Nil);
  Assert(T Is TFSSqlTableProxy);
  If T.FieldByName(FieldName) <> Nil Then
    Begin
      Result := T.FieldByName(FieldName);
      Exit;
    End;
  SQLError('Unknown field:' + FieldName);
End;

Procedure TfsSqlDELETE.Clear;
Begin
  TableRef.Free;
  TableRef := Nil;
  CondExpWhere.Free;
  CondExpWhere := Nil;
End;

Procedure TfsSqlDELETE.DeleteRecord;
Var
  Pos: TffInt64;
Begin
  Pos := T.GetCurrentRecordID;
  DeleteList.Add(Pointer(Pos.iLow));
  DeleteList.Add(Pointer(Pos.iHigh));
End;

Destructor TfsSqlDELETE.Destroy;
Begin
  If T <> Nil Then
    If T.Owner = Self Then
      Begin
        T.Owner := Nil;
        T.Free;
      End;
  Clear;
  Joiner.Free;
  Inherited;
End;

Procedure TfsSqlDELETE.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, 'DELETE FROM ');
  TableRef.EmitSQL(Stream);
  WriteStr(Stream, ' ');
  If assigned(CondExpWhere) Then
    Begin
      WriteStr(Stream, 'WHERE ');
      CondExpWhere.EmitSQL(Stream);
    End;
End;

Procedure TfsSqlDELETE.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(TableRef) Then
    TableRef.EnumNodes(EnumMethod, Deep);
  If assigned(CondExpWhere) Then
    CondExpWhere.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlDELETE.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlDELETE)
    And (BothNil(TableRef, TfsSqlDELETE(Other).TableRef)
    Or (BothNonNil(TableRef, TfsSqlDELETE(Other).TableRef)
    And TableRef.Equals(TfsSqlDELETE(Other).TableRef)))
    And (BothNil(CondExpWhere, TfsSqlDELETE(Other).CondExpWhere)
    Or (BothNonNil(CondExpWhere, TfsSqlDELETE(Other).CondExpWhere)
    And CondExpWhere.Equals(TfsSqlDELETE(Other).CondExpWhere)));
End;

Function TfsSqlDELETE.Execute(Var RowsAffected: Integer): TffResult; {!!.11}
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  i: Integer;
  Cpos: Longword;
  Pos: TffInt64;
  NotOkTr: Boolean;

  Function DeleteRecords: TffResult;
  Var
    aRecord: PffByteArray;
  Begin
    FFGetMem(aRecord, TfsSrBaseCursor(T.CursorID).Table.Dictionary.RecordLength);
    Try
      If TopDirection In [tdDown] Then
        TfsSrBaseCursor(T.CursorID).SetToEnd
      Else
        TfsSrBaseCursor(T.CursorID).SetToBegin;
      Result := 0;
      While (Result = DBIERR_NONE) Do
        Begin
          If TfsSrBaseCursor(T.CursorID).CursorInfo.Pos = cpOnRecord Then
            Begin
              Result := TfsSrBaseCursor(T.CursorID).GetRecord(aRecord, ffsltExclusive, tluDatabase, aflag, arefNr, False);
              If Result = DBIERR_NOCURRREC Then
                If TopDirection In [tdDown] Then
                  Result := TfsSrBaseCursor(T.CursorID).GetPriorRecord(aRecord, ffsltExclusive, aflag, arefNr)
                Else
                  Result := TfsSrBaseCursor(T.CursorID).GetNextRecord(aRecord, ffsltExclusive, aflag, arefNr);
            End
          Else If TopDirection In [tdDown] Then
            Result := TfsSrBaseCursor(T.CursorID).GetPriorRecord(aRecord, ffsltExclusive, aflag, arefNr)
          Else
            Result := TfsSrBaseCursor(T.CursorID).GetNextRecord(aRecord, ffsltExclusive, aflag, arefnr);

          If Result = DBIERR_NONE Then
            Begin
              Result := TfsSrBaseCursor(T.CursorID).DeleteRecord(aRecord);
              If Result = DBIERR_NOTSUFFFIELDRIGHTS Then
                Begin
                  If TopDirection In [tdDown] Then
                    Result := TfsSrBaseCursor(T.CursorID).GetPriorRecord(aRecord, ffsltExclusive, aflag, arefnr)
                  Else
                    Result := TfsSrBaseCursor(T.CursorID).GetNextRecord(aRecord, ffsltExclusive, aflag, arefnr);
                End;
            End;

          If Result = DBIERR_NONE Then
            Begin
              If CommitBy > 0 Then
                Begin
                  Inc(Cpos);
                  If Cpos = CommitBy Then
                    Begin
                      Cpos := 0;
                      Result := Owner.FDatabase.Commit;
                      If Result <> DBIERR_NONE Then
                        Begin
                          Result := Owner.FDatabase.AbortTransaction;
                          NotOkTr := True;
                          System.break;
                        End
                      Else
                        Begin
                          Result := Owner.FDatabase.StartTransaction([T]);
                          Result := T.EnsureWritable;
                          If Result <> DBIERR_NONE Then
                            System.break;
                        End;
                    End;
                End;
              inc(RowsAffected);
              If LimitCount > 0 Then
                Begin
                  If RowsAffected = LimitCount Then
                    System.break;
                End;
            End;
        End;
      If Result = DBIERR_EOF Then
        Result := 0;
    Finally
      FFFreeMem(aRecord, TfsSrBaseCursor(T.CursorID).Table.Dictionary.RecordLength);
    End;
  End;

  Function ExecDel: tffResult;
  Begin
    If TopDirection In [tdDown] Then
      Begin
        Pos.iHigh := TffWord32(DeleteList[i]);
        dec(i);
      End
    Else
      Begin
        Pos.iLow := TffWord32(DeleteList[i]);
        inc(i);
      End;
    If TopDirection In [tdDown] Then
      Assert(i > 0)
    Else
      Assert(i < DeleteList.Count);

    If TopDirection In [tdDown] Then
      Begin
        Pos.iLow := TffWord32(DeleteList[i]);
        dec(i);
      End
    Else
      Begin
        Pos.iHigh := TffWord32(DeleteList[i]);
        inc(i);
      End;
    Result := T.GetRecordByID(Pos, ffsltExclusive);
    Case Result Of
      DBIERR_NONE:
        Result := T.Delete;
      DBIERR_RECDELETED: ;
    End;
    If CommitBy > 0 Then
      Begin
        Inc(Cpos);
        If Cpos = CommitBy Then
          Begin
            Cpos := 0;
            Result := Owner.FDatabase.Commit;
            If Result <> DBIERR_NONE Then
              Begin
                Result := Owner.FDatabase.AbortTransaction;
                NotOkTr := True;
              End
            Else
              Begin
                Result := Owner.FDatabase.StartTransaction([T]);
                If Result = DBIERR_NONE Then
                  Result := T.EnsureWritable;
              End;
          End;
      End;
  End;

Begin
  Result := Owner.FDatabase.StartTransaction([T]);
  NotOkTr := False;

  If Result = DBIERR_NONE Then
    Try
      If Not Bound Then
        Bind;
      {Begin !!.11}
      Result := T.EnsureWritable;
      If Result <> DBIERR_NONE Then
        Begin
          Owner.FDatabase.AbortTransaction;
          Exit;
        End;
      {End !!.11}
      RowsAffected := 0;
      Cpos := 0;
      If Self.CondExpWhere = Nil Then
        Begin
          If TableRef.IndexName <> '' Then
            Begin
              i := t.FindIndex(TableRef.IndexName);
              If i >= 0 Then
                T.SetIndex(i - 1)
              Else
                sqlerror('Error: IndexName is wrong [' + TableRef.IndexName + ']');
            End
          Else
            T.SetIndex(-1); {switch to raw Record id index}
          Result := DeleteRecords;
        End
      Else
        Begin
          If Joiner = Nil Then
            Begin
              Joiner := TfsSqlJoiner.Create(Owner, CondExpWhere);
              Joiner.Sources.Add(TFSSqlTableProxySubset.Create(T));
            End;

          Joiner.ClearColumnList;

          Joiner.Target := Nil;
          DeleteList := TList.Create;
          Try
            Joiner.Execute(Owner.UseIndex, DeleteRecord, jmNone);
            If TableRef.IndexName <> '' Then
              Begin
                i := t.FindIndex(TableRef.IndexName);
                If i >= 0 Then
                  T.SetIndex(i - 1)
                Else
                  sqlerror('Error: IndexName is wrong [' + TableRef.IndexName + ']');
              End
            Else
              T.SetIndex(-1); {switch to raw Record id index}

            If TopDirection In [tdDown] Then
              Begin
                i := DeleteList.Count - 1;
                While (Result = DBIERR_NONE) And
                  (i > 0) Do
                  Begin
                    Result := execdel;
                    If Result = DBIERR_NONE Then
                      Begin
                        inc(RowsAffected);
                        If LimitCount > 0 Then
                          Begin
                            If RowsAffected = LimitCount Then
                              System.break;
                          End;
                      End;
                  End;
              End
            Else
              Begin
                i := 0;
                While (Result = DBIERR_NONE) And
                  (i < DeleteList.Count) Do
                  Begin
                    Result := execdel;
                    If Result = DBIERR_NONE Then
                      Begin
                        inc(RowsAffected);
                        If LimitCount > 0 Then
                          Begin
                            If RowsAffected = LimitCount Then
                              System.break;
                          End;
                      End;
                  End;
              End;
          Finally
            DeleteList.Free;
          End;
        End;
      If Result = DBIERR_NONE Then
        Begin
          If Owner.FDatabase.InTransaction Then
            Owner.FDatabase.Commit;
        End
      Else
        Begin
          Owner.FDatabase.AbortTransaction;
          RowsAffected := 0;
          Assert(False, 'Unexpected DELETE scenario - Abort Transaction');
        End;
    Except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      Raise;
    End
  Else If Result = DBIERR_LOCKED Then
    FSRaiseException(EfsException, fsStrResServer, fserrLockRejected,
      [ffcLockExclusive, '', T.Name])
  Else
    FSRaiseException(EfsException, fsStrResServer, Result, [T.Name]);
End;
{--------}

{!!.11 new}

Function TfsSqlDELETE.Reduce: Boolean;
Begin
  If TableRef <> Nil Then
    If TableRef.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  If CondExpWhere <> Nil Then
    If CondExpWhere.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

{ TfsSqlUPDATE }

Procedure TfsSqlUPDATE.AddColumns(Node: TfsSqlNode);
Begin
  Node.AddColumnDef(Self);
End;

Procedure TfsSqlUPDATE.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlUPDATE Then
    Begin
      Clear;
      If TfsSqlUPDATE(Source).TableRef <> Nil Then
        Begin
          TableRef := TfsSqlTableRef.Create(Self);
          TableRef.Assign(TfsSqlUPDATE(Source).TableRef);
        End;
      If TfsSqlUPDATE(Source).UpdateList <> Nil Then
        Begin
          UpdateList := TfsSqlUpdateList.Create(Self);
          UpdateList.Assign(TfsSqlUPDATE(Source).UpdateList);
        End;
      If TfsSqlUPDATE(Source).CondExpWhere <> Nil Then
        Begin
          CondExpWhere := TfsSqlCondExp.Create(Self);
          CondExpWhere.Assign(TfsSqlUPDATE(Source).CondExpWhere);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlUPDATE.Bind;
Var
  i: Integer;
  F: TfsSqlFieldProxy;
Begin
  Assert(UpdateList <> Nil);
  UpdateList.EnumNodes(ClearBindings, False);
  T := TableRef.GetTable(Self, omReadWrite, False); {!!.11}
  If T = Nil Then
    SQLError('Unable to open table: ' + TableRef.SQLName + //TableName +
      '. Ensure the table exists And is not in use by ' +
      'another process.');

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  UpdateList.EnumNodes(AddColumns, False);
  Assert(Columns.Count > 0);
  For i := 0 To Columns.Count - 1 Do
    Begin
      F := T.FieldByName(Columns[i]);
      If F = Nil Then
        SQLError('Unknown field For table ' + TableRef.SQLName + 'in UPDATE statement:' +
          Columns[i]);

      If Not F.CanUpdate Then
        SQLError('Changing fields of this Type is not currently supported through SQL:' +
          Columns[i]);

      TfsSqlUpdateItem(Columns.Objects[i]).F := F;
      With TfsSqlUpdateItem(Columns.Objects[i]) Do
        If Simplex <> Nil Then
          Simplex.MatchType(F.GetType);

    End;
  If CondExpWhere <> Nil Then
    CondExpWhere.MatchType(fstBoolean);
  Bound := True;
End;

Function TfsSqlUPDATE.BindField(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  Result := Nil;
  Assert(T <> Nil);
  Assert(T Is TFSSqlTableProxy);
  If T.FieldByName(FieldName) <> Nil Then
    Begin
      Result := T.FieldByName(FieldName);
      Exit;
    End;
  SQLError('Unknown field:' + FieldName);
End;

Procedure TfsSqlUPDATE.Clear;
Begin
  TableRef.Free;
  TableRef := Nil;
  UpdateList.Free;
  UpdateList := Nil;
  CondExpWhere.Free;
  CondExpWhere := Nil;
End;

Procedure TfsSqlUPDATE.ClearBindings(Node: TfsSqlNode);
Begin
  Node.ClearBinding;
End;

Destructor TfsSqlUPDATE.Destroy;
Begin
  If T <> Nil Then
    If T.Owner = Self Then
      Begin
        T.Owner := Nil;
        T.Free;
      End;
  Clear;
  Joiner.Free;
  Inherited;
End;

Procedure TfsSqlUPDATE.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, 'UPDATE ');
  TableRef.EmitSQL(Stream);
  WriteStr(Stream, ' Set ');
  If assigned(UpdateList) Then
    UpdateList.EmitSQL(Stream);
  If assigned(CondExpWhere) Then
    CondExpWhere.EmitSQL(Stream);
End;

Procedure TfsSqlUPDATE.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(TableRef) Then
    TableRef.EnumNodes(EnumMethod, Deep);
  If assigned(UpdateList) Then
    UpdateList.EnumNodes(EnumMethod, Deep);
  If assigned(CondExpWhere) Then
    CondExpWhere.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlUPDATE.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlUPDATE)
    And (BothNil(TableRef, TfsSqlUPDATE(Other).TableRef)
    Or (BothNonNil(TableRef, TfsSqlUPDATE(Other).TableRef)
    And UpdateList.Equals(TfsSqlUPDATE(Other).UpdateList)))
    And (BothNil(UpdateList, TfsSqlUPDATE(Other).UpdateList)
    Or (BothNonNil(UpdateList, TfsSqlUPDATE(Other).UpdateList)
    And UpdateList.Equals(TfsSqlUPDATE(Other).UpdateList)))
    And (BothNil(CondExpWhere, TfsSqlUPDATE(Other).CondExpWhere)
    Or (BothNonNil(CondExpWhere, TfsSqlUPDATE(Other).CondExpWhere)
    And CondExpWhere.Equals(TfsSqlUPDATE(Other).CondExpWhere)));
End;

Function TfsSqlUPDATE.Execute(Var RowsAffected: Integer): TffResult; {!!.11}
Var
  i: Integer;
  Cpos: Longword;
  Pos: TffInt64;
Begin
  Result := Owner.FDatabase.StartTransaction([T]);
  If Result = DBIERR_NONE Then
    Try
      If Not Bound Then
        Bind;

      Result := T.EnsureWritable;
      If Result <> DBIERR_NONE Then
        Begin
          Owner.FDatabase.AbortTransaction;
          Exit;
        End;

      FRowsAffected := 0;
      If Joiner = Nil Then
        Begin
          Joiner := TfsSqlJoiner.Create(Owner, CondExpWhere);
          Joiner.Sources.Add(
            TFSSqlTableProxySubset.Create(
            TFSSqlTableProxy(T)));
        End;

      Joiner.ClearColumnList;

      Joiner.Target := Nil;
      UpdateRecList := TList.Create;
      Try
        Joiner.Execute(Owner.UseIndex, UpdateRecord, jmNone);
        T.SetIndex(-1); {switch to raw Record id index} {!!.11}
        i := 0;
        CPos := 0;
        While (Result = DBIERR_NONE) And
          (i < UpdateRecList.Count) Do
          Begin
            Pos.iLow := TffWord32(UpdateRecList[i]);
            inc(i);
            Assert(i < UpdateRecList.Count);
            Pos.iHigh := TffWord32(UpdateRecList[i]);
            inc(i);
            Result := T.GetRecordByID(Pos, ffsltExclusive);
            Case Result Of
              DBIERR_NONE:
                Result := UpdateList.Update(-1);
              DBIERR_RECDELETED: ;
              Else
                break;
            End;

            If CommitBy > 0 Then
              Begin
                Inc(Cpos);
                If Cpos = CommitBy Then
                  Begin
                    Cpos := 0;
                    Result := Owner.FDatabase.Commit;
                    If Result <> DBIERR_NONE Then
                      Begin
                        Result := Owner.FDatabase.AbortTransaction;
                        System.break;
                      End
                    Else
                      Begin
                        Result := Owner.FDatabase.StartTransaction([T]);
                        If Result = DBIERR_NONE Then
                          Result := T.EnsureWritable;
                        If Result <> DBIERR_NONE Then
                          System.break;
                      End;
                  End;
              End;
            If Result = DBIERR_NONE Then {!!.11}
              inc(FRowsAffected);
          End;
      Finally
        UpdateRecList.Free;
      End;
      {Begin !!.11}
      If Result = DBIERR_NONE Then
        Begin
          If Owner.FDatabase.InTransaction Then
            Owner.FDatabase.Commit;
          RowsAffected := FRowsAffected;
        End
      Else
        Begin
          Owner.FDatabase.AbortTransaction;
          RowsAffected := 0;
          Assert(False, 'Unexpected UPDATE scenario - Abort Transaction');
        End;
      {End !!.11}
    Except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      Raise;
    End
  Else If Result = DBIERR_LOCKED Then
    FSRaiseException(EfsException, fsStrResServer, fserrLockRejected,
      [ffcLockExclusive, '', T.Name])
  Else
    FSRaiseException(EfsException, fsStrResServer, Result, [T.Name]);
End;
{--------}

{!!.11 new}

Function TfsSqlUPDATE.Reduce: Boolean;
Begin
  If TableRef <> Nil Then
    If TableRef.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  If CondExpWhere <> Nil Then
    If CondExpWhere.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  If UpdateList <> Nil Then
    If UpdateList.Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

Procedure TfsSqlUPDATE.UpdateRecord;
Var
  Pos: TffInt64;
Begin
  Pos := T.GetCurrentRecordID;
  UpdateRecList.Add(Pointer(Pos.iLow));
  UpdateRecList.Add(Pointer(Pos.iHigh));
End;

{ TfsSqlUpdateItem }

Procedure TfsSqlUpdateItem.AddColumnDef(Target: TfsSqlColumnListOwner);
Begin
  Target.Columns.AddObject(ColumnName, Self);
End;

Procedure TfsSqlUpdateItem.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlUpdateItem Then
    Begin
      Simplex.Free;
      Simplex := Nil;
      ColumnName := TfsSqlUpdateItem(Source).ColumnName;
      Default := TfsSqlUpdateItem(Source).Default;
      IsNull := TfsSqlUpdateItem(Source).IsNull;
      If TfsSqlUpdateItem(Source).Simplex <> Nil Then
        Begin
          Simplex := TfsSqlSimpleExpression.Create(Self);
          Simplex.Assign(TfsSqlUpdateItem(Source).Simplex);
        End;
    End
  Else
    AssignError(Source);
End;

Destructor TfsSqlUpdateItem.Destroy;
Begin
  Simplex.Free;
  Inherited;
End;

Procedure TfsSqlUpdateItem.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ColumnName);
  WriteStr(Stream, ' = ');
  If Default Then
    WriteStr(Stream, 'DEFAULT ')
  Else If Simplex = Nil Then
    WriteStr(Stream, 'NULL ')
  Else
    Simplex.EmitSQL(Stream);
End;

Procedure TfsSqlUpdateItem.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If Simplex <> Nil Then
    Simplex.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlUpdateItem.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    (Other Is TfsSqlUpdateItem)
    And (ColumnName = TfsSqlUpdateItem(Other).ColumnName)
    And (Default = TfsSqlUpdateItem(Other).Default)
    And (IsNull = TfsSqlUpdateItem(Other).IsNull)
    And (BothNil(Simplex, TfsSqlUpdateItem(Other).Simplex)
    Or (BothNonNil(Simplex, TfsSqlUpdateItem(Other).Simplex)
    And Simplex.Equals(TfsSqlUpdateItem(Other).Simplex)));
End;

Function TfsSqlUpdateItem.Reduce: Boolean;
Begin
  Result := (Simplex <> Nil) And Simplex.Reduce;
End;

Procedure TfsSqlUpdateItem.Update(aArrayIndex: Integer);
Begin
  Assert(F <> Nil);
  If Simplex <> Nil Then
    Begin
      F.SetValue(Simplex.GetValue(aArrayIndex), aArrayIndex);
    End
  Else If Not Default Then
    F.SetFieldToNull;
End;

{ TfsSqlUpdateList }

Function TfsSqlUpdateList.AddItem(
  NewValue: TfsSqlUpdateItem): TfsSqlUpdateItem;
Begin
  FUpdateItemList.Add(NewValue);
  Result := NewValue;
End;

Procedure TfsSqlUpdateList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlValueList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlValueList(Source).ValueCount) Do
        AddItem(TfsSqlUpdateItem.Create(Self)).Assign(
          TfsSqlValueList(Source).ValueItem[i]);
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlUpdateList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(UpdateCount) Do
    UpdateItem[i].Free;
  FUpdateItemList.Clear;
End;

Constructor TfsSqlUpdateList.Create(AParent: TfsSqlNode);
Begin
  Inherited;
  FUpdateItemList := TList.Create;
End;

Destructor TfsSqlUpdateList.Destroy;
Begin
  Clear;
  FUpdateItemList.Free;
  Inherited;
End;

Procedure TfsSqlUpdateList.EmitSQL(Stream: TStream);
Var
  i: Integer;
  First: Boolean;
Begin
  First := True;
  For i := 0 To pred(UpdateCount) Do
    Begin
      If First Then
        First := False
      Else
        WriteStr(Stream, ', ');
      UpdateItem[i].EmitSQL(Stream);
    End;
End;

Procedure TfsSqlUpdateList.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(UpdateCount) Do
    UpdateItem[i].EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlUpdateList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlValueList Then
    Begin
      If UpdateCount <> TfsSqlUpdateList(Other).UpdateCount Then
        Exit;
      For i := 0 To pred(UpdateCount) Do
        If Not UpdateItem[i].Equals(TfsSqlUpdateList(Other).UpdateItem[i]) Then
          Exit;
      Result := True;
    End;
End;

Function TfsSqlUpdateList.GetUpdateCount: Integer;
Begin
  Result := FUpdateItemList.Count;
End;

Function TfsSqlUpdateList.GetUpdateItem(Index: Integer): TfsSqlUpdateItem;
Begin
  Result := TfsSqlUpdateItem(FUpdateItemList[Index]);
End;

{!!.11 new}

Function TfsSqlUpdateList.Reduce: Boolean;
Var
  i: Integer;
Begin
  For i := 0 To UpdateCount - 1 Do
    If UpdateItem[i].Reduce Then
      Begin
        Result := True;
        Exit;
      End;
  Result := False;
End;

Function TfsSqlUpdateList.Update(aArrayIndex: Integer): TffResult; {!!.11}
Var
  i: Integer;
Begin
  Assert(Parent <> Nil);
  Assert(TObject(Parent) Is TfsSqlUpdate);
  For i := 0 To UpdateCount - 1 Do
    Begin
      If UpdateItem[i].Default Then
        TfsSqlUpdate(Parent).T.SetDefault(UpdateItem[i].ColumnName)
      Else If UpdateItem[i].IsNull Then
        TfsSqlUpdate(Parent).T.SetNull(UpdateItem[i].ColumnName);
      UpdateItem[i].Update(aArrayIndex);
    End;
  Result := TfsSqlUpdate(Parent).T.Update; {!!.11}
End;

{ TfsSqlColumnListOwner }

Constructor TfsSqlColumnListOwner.Create(AParent: TfsSqlNode);
Begin
  Inherited;
  Columns := TStringList.Create;
  DistinctColumns := TStringList.Create;
  DisplayColumns := TStringList.Create;
  FCommitBy := 0;
  fLimitCount := -2;
  fLimitPrct := False;
  fLimitPrctStart := False;
  fLimitStart := 1;
  fTopDirection := tdNone;
  fLimitDivBy := 0;
  fLimitDivAtOne := False;
  fLimitPrctDiv := False;
  fLimitDistinct := False;
  fIslimit := False;
  fLimitFirst := False;
End;

Destructor TfsSqlColumnListOwner.Destroy;
Begin
  Columns.Free;
  DistinctColumns.free;
  DisplayColumns.free;
  Inherited;
End;

{ TfsSqlNonJoinTablePrimary }

Procedure TfsSqlNonJoinTablePrimary.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlNonJoinTablePrimary Then
    Begin
      Clear;
      If TfsSqlNonJoinTablePrimary(Source).SelectSt <> Nil Then
        Begin
          SelectSt := TfsSqlSELECT.Create(Self);
          SelectSt.Assign(TfsSqlNonJoinTablePrimary(Source).SelectSt);
        End;
      If TfsSqlNonJoinTablePrimary(Source).ValueList <> Nil Then
        Begin
          ValueList := TfsSqlValueList.Create(Self);
          ValueList.Assign(TfsSqlNonJoinTablePrimary(Source).ValueList);
        End;
      If TfsSqlNonJoinTablePrimary(Source).NonJoinTableExp <> Nil Then
        Begin
          NonJoinTableExp := TfsSqlNonJoinTableExp.Create(Self);
          NonJoinTableExp.Assign(TfsSqlNonJoinTablePrimary(Source).NonJoinTableExp);
        End;
      If TfsSqlNonJoinTablePrimary(Source).TableRef <> Nil Then
        Begin
          TableRef := TfsSqlTableRef.Create(Self);
          TableRef.Assign(TfsSqlNonJoinTablePrimary(Source).TableRef);
        End;
    End
  Else
    AssignError(Source);
End;

Function TfsSqlNonJoinTablePrimary.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  If SelectSt <> Nil Then
    Result := SelectSt.BindField(TableName, FieldName)
  Else If NonJoinTableExp <> Nil Then
    Result := NonJoinTableExp.BindFieldDown(TableName, FieldName)
  Else If TableRef <> Nil Then
    Result := TableRef.BindFieldDown(TableName, FieldName)
  Else
    Result := Nil;
End;

Function TfsSqlNonJoinTablePrimary.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  If SelectSt <> Nil Then
    Result := SelectSt.BindTable(AOwner, TableName)
  Else If NonJoinTableExp <> Nil Then
    Result := NonJoinTableExp.BindTable(AOwner, TableName)
  Else If TableRef <> Nil Then
    Result := TableRef.BindTable(AOwner, TableName)
  Else
    Result := Nil;
End;

Procedure TfsSqlNonJoinTablePrimary.Clear;
Begin
  SelectSt.Free;
  SelectSt := Nil;
  ValueList.Free;
  ValueList := Nil;
  NonJoinTableExp.Free;
  NonJoinTableExp := Nil;
  TableRef.Free;
  TableRef := Nil;
End;

Function TfsSqlNonJoinTablePrimary.DependsOn(
  Table: TFSSqlTableProxy): Boolean;
Begin
  If SelectSt <> Nil Then
    Result := SelectSt.DependsOn(Table)
  Else If NonJoinTableExp <> Nil Then
    Result := NonJoinTableExp.DependsOn(Table)
  Else If TableRef <> Nil Then
    Result := TableRef.DependsOn(Table)
  Else
    Result := False;
End;

Destructor TfsSqlNonJoinTablePrimary.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlNonJoinTablePrimary.EmitSQL(Stream: TStream);
Begin
  If SelectSt <> Nil Then
    SelectSt.EmitSQL(Stream);
  If ValueList <> Nil Then
    ValueList.EmitSQL(Stream);
  If NonJoinTableExp <> Nil Then
    Begin
      WriteStr(Stream, ' (');
      NonJoinTableExp.EmitSQL(Stream);
      WriteStr(Stream, ')');
    End;
  If TableRef <> Nil Then
    Begin
      WriteStr(Stream, ' TABLE ');
      TableRef.EmitSQL(Stream);
    End;
End;

Procedure TfsSqlNonJoinTablePrimary.EnsureResultTable(NeedData: Boolean);
Begin
  If SelectSt <> Nil Then
    SelectSt.EnsureResultTable(NeedData);
  If NonJoinTableExp <> Nil Then
    NonJoinTableExp.EnsureResultTable(NeedData);
End;

Procedure TfsSqlNonJoinTablePrimary.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If SelectSt <> Nil Then
    SelectSt.EnumNodes(EnumMethod, Deep);
  If ValueList <> Nil Then
    ValueList.EnumNodes(EnumMethod, Deep);
  If NonJoinTableExp <> Nil Then
    NonJoinTableExp.EnumNodes(EnumMethod, Deep);
  If TableRef <> Nil Then
    TableRef.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlNonJoinTablePrimary.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    Other Is TfsSqlNonJoinTablePrimary
    And ((BothNil(SelectSt, TfsSqlNonJoinTablePrimary(Other).SelectSt)
    Or (BothNonNil(SelectSt, TfsSqlNonJoinTablePrimary(Other).SelectSt)
    And SelectSt.Equals(TfsSqlNonJoinTablePrimary(Other).SelectSt))))
    And ((BothNil(ValueList, TfsSqlNonJoinTablePrimary(Other).ValueList)
    Or (BothNonNil(ValueList, TfsSqlNonJoinTablePrimary(Other).ValueList)
    And ValueList.Equals(TfsSqlNonJoinTablePrimary(Other).ValueList))))
    And ((BothNil(NonJoinTableExp, TfsSqlNonJoinTablePrimary(Other).NonJoinTableExp)
    Or (BothNonNil(NonJoinTableExp, TfsSqlNonJoinTablePrimary(Other).NonJoinTableExp)
    And NonJoinTableExp.Equals(TfsSqlNonJoinTablePrimary(Other).NonJoinTableExp))))
    And ((BothNil(TableRef, TfsSqlNonJoinTablePrimary(Other).TableRef)
    Or (BothNonNil(TableRef, TfsSqlNonJoinTablePrimary(Other).TableRef)
    And TableRef.Equals(TfsSqlNonJoinTablePrimary(Other).TableRef))));
End;

Procedure TfsSqlNonJoinTablePrimary.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Begin
  If assigned(SelectSt) Then
    SelectSt.Execute(aLiveResult, aCursorID, RecordsRead)
  Else If assigned(ValueList) Then
    ValueList.Execute(aLiveResult, aCursorID, RecordsRead)
  Else If assigned(NonJoinTableExp) Then
    NonJoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead)
  Else If assigned(TableRef) Then
    TableRef.Execute(aLiveResult, aCursorID, RecordsRead)
  Else
    Assert(False);
End;

Function TfsSqlNonJoinTablePrimary.GetResultTable: TFSSqlTableProxy;
Begin
  Result := Nil;
  If assigned(SelectSt) Then
    Result := SelectSt.ResultTable
  Else If assigned(ValueList) Then
    Result := ValueList.ResultTable
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.ResultTable
  Else If assigned(TableRef) Then
    Result := TableRef.ResultTable
  Else
    Assert(False);
End;

Function TfsSqlNonJoinTablePrimary.Reduce: Boolean;
Begin
  Result := False;
  If assigned(SelectSt) Then
    Result := SelectSt.Reduce
  Else If assigned(ValueList) Then
    Result := ValueList.Reduce
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.Reduce
  Else If assigned(TableRef) Then
    Result := False //TableRef.Reduce
  Else
    Assert(False);
End;

Function TfsSqlNonJoinTablePrimary.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Begin
  Result := Nil;
  If assigned(SelectSt) Then
    Result := SelectSt.TargetFieldFromSourceField(F)
  Else If assigned(ValueList) Then
    Result := Nil
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.TargetFieldFromSourceField(F)
  Else If assigned(TableRef) Then
    Result := TableRef.TargetFieldFromSourceField(F)
  Else
    Assert(False);
End;

{ TfsSqlTableExp }

Procedure TfsSqlTableExp.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlTableExp Then
    Begin
      Clear;
      If TfsSqlTableExp(Source).NestedTableExp <> Nil Then
        Begin
          NestedTableExp := TfsSqlTableExp.Create(Self);
          NestedTableExp.Assign(TfsSqlTableExp(Source).NestedTableExp);
        End;
      If TfsSqlTableExp(Source).JoinTableExp <> Nil Then
        Begin
          JoinTableExp := TfsSqlJoinTableExp.Create(Self);
          JoinTableExp.Assign(TfsSqlTableExp(Source).JoinTableExp);
        End;
      If TfsSqlTableExp(Source).NonJoinTableExp <> Nil Then
        Begin
          NonJoinTableExp := TfsSqlNonJoinTableExp.Create(Self);
          NonJoinTableExp.Assign(TfsSqlTableExp(Source).NonJoinTableExp);
        End;
      If TfsSqlTableExp(Source).UnionTerm <> Nil Then
        Begin
          UnionTerm := TfsSqlNonJoinTableTerm.Create(Self);
          UnionTerm.Union := TfsSqlTableExp(Source).UnionTerm.Union;
          UnionTerm.UnionCase := TfsSqlTableExp(Source).UnionTerm.UnionCase;
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlTableExp.Clear;
Begin
  UnionTerm.free;
  UnionTerm := Nil;
  NestedTableExp.Free;
  NestedTableExp := Nil;
  JoinTableExp.Free;
  JoinTableExp := Nil;
  NonJoinTableExp.Free;
  NonJoinTableExp := Nil;
End;

Constructor TfsSqlTableExp.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
End;

Destructor TfsSqlTableExp.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlTableExp.EmitSQL(Stream: TStream);
Begin
  If assigned(NestedTableExp) Then
    NestedTableExp.EmitSQL(Stream);
  If assigned(JoinTableExp) Then
    JoinTableExp.EmitSQL(Stream);
  If assigned(NonJoinTableExp) Then
    NonJoinTableExp.EmitSQL(Stream);
End;

Function TfsSqlTableExp.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  If assigned(UnionTerm) Then
    Result := UnionTerm.BindFieldDown(TableName, FieldName)
  Else If assigned(NestedTableExp) Then
    Result := NestedTableExp.BindFieldDown(TableName, FieldName)
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.BindFieldDown(TableName, FieldName)
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.BindFieldDown(TableName, FieldName)
  Else
    Result := Nil;
End;

Function TfsSqlTableExp.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  If assigned(UnionTerm) Then
    Result := UnionTerm.BindTable(AOwner, TableName)
  Else If assigned(NestedTableExp) Then
    Result := NestedTableExp.BindTable(AOwner, TableName)
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.BindTable(AOwner, TableName)
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.BindTable(AOwner, TableName)
  Else
    Result := Nil;
End;

Function TfsSqlTableExp.CheckNoDups(Const CaseSensitive: Boolean): Boolean;
Begin
  EnsureResultTable(True);
  Result := Not ResultTable.HasDuplicates(CaseSensitive);
End;

Function TfsSqlTableExp.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If assigned(NestedTableExp) Then
    Result := NestedTableExp.DependsOn(Table)
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.DependsOn(Table)
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.DependsOn(Table)
  Else
    Result := False;
  If Not Result And assigned(UnionTerm) Then
    Result := UnionTerm.DependsOn(Table);
End;

Procedure TfsSqlTableExp.EnsureResultTable(NeedData: Boolean);
Begin
  If assigned(NestedTableExp) Then
    NestedTableExp.EnsureResultTable(NeedData);
  If assigned(JoinTableExp) Then
    JoinTableExp.EnsureResultTable(NeedData);
  If assigned(NonJoinTableExp) Then
    NonJoinTableExp.EnsureResultTable(NeedData);
  If assigned(UnionTerm) Then
    UnionTerm.EnsureResultTable(NeedData);
End;

Procedure TfsSqlTableExp.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(NestedTableExp) Then
    NestedTableExp.EnumNodes(EnumMethod, Deep);
  If assigned(JoinTableExp) Then
    JoinTableExp.EnumNodes(EnumMethod, Deep);
  If assigned(NonJoinTableExp) Then
    NonJoinTableExp.EnumNodes(EnumMethod, Deep);
  If assigned(UnionTerm) Then
    UnionTerm.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlTableExp.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    Other Is TfsSqlTableExp
    And ((BothNil(NestedTableExp, TfsSqlTableExp(Other).NestedTableExp)
    Or (BothNonNil(NestedTableExp, TfsSqlTableExp(Other).NestedTableExp)
    And NestedTableExp.Equals(TfsSqlTableExp(Other).NestedTableExp))))
    And ((BothNil(JoinTableExp, TfsSqlTableExp(Other).JoinTableExp)
    Or (BothNonNil(JoinTableExp, TfsSqlTableExp(Other).JoinTableExp)
    And JoinTableExp.Equals(TfsSqlTableExp(Other).JoinTableExp))))
    And ((BothNil(NonJoinTableExp, TfsSqlTableExp(Other).NonJoinTableExp)
    Or (BothNonNil(NonJoinTableExp, TfsSqlTableExp(Other).NonJoinTableExp)
    And NonJoinTableExp.Equals(TfsSqlTableExp(Other).NonJoinTableExp))))
    And ((BothNil(UnionTerm, TfsSqlTableExp(Other).UnionTerm)
    Or (BothNonNil(UnionTerm, TfsSqlTableExp(Other).UnionTerm)
    And UnionTerm.Equals(TfsSqlTableExp(Other).UnionTerm))));
End;

Procedure TfsSqlTableExp.Union2Table(
  aUnion: TfsUnionType;
  aUnionCase: boolean;
  aCursorID1: TffCursorID;
  Var aLiveResult: Boolean;
  Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Var
  T, T2, TU: TfsSqlTableProxy;
  FC: TfsFieldCopier;
  i: Integer;
  ResultT: TfsSqlTableProxy;
  FieldDefList,
    FieldDefListU: TfsSqlFieldDefList;
Begin
  T := Owner.FDatabase.TableByCursorID(Self, aCursorID1);
  Tu := Nil;
  If assigned(NestedTableExp) Then
    Tu := NestedTableExp.GetResultTable
  Else If assigned(JoinTableExp) Then
    Tu := JoinTableExp.GetResultTable
  Else If assigned(NonJoinTableExp) Then
    Tu := NonJoinTableExp.GetResultTable
  Else
    Assert(False);
  Try

    FieldDefList := Nil;
    FieldDefListU := Nil;
    Try
      FieldDefList := T.ExtractFieldDef;
      FieldDefListU := TU.ExtractFieldDef;
      // prevent for incompctible fields
      If FieldDefList.Count > FieldDefListU.Count Then
        SQLError('Error: Master table field count > Detail table field counr: ' +
          IntToStr(FieldDefList.Count) + ' : ' + IntToStr(FieldDefListU.Count));
      If Not FieldDefList.CompareTypes(FieldDefListU) Then
        SQLError('Error: Field types is not compactibles!');

      ResultT := Owner.fDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
    Finally
      FieldDefList.Free;
      FieldDefListU.Free;
    End;
    Try
      Try
        FC := Nil;
        Try
          FC := TfsFieldCopier.Create;

          For i := 0 To ResultT.FieldCount - 1 Do
            FC.Add(T.Field(i), ResultT.Field(i));
          Owner.FDatabase.StartTransaction([Nil]);
          Try
            If T.First Then
              Repeat
                ResultT.Insert;
                FC.Execute;
                ResultT.InsertPost;
              Until Not T.Next;
            Owner.FDatabase.Commit;
          Except
            Owner.FDatabase.AbortTransaction;
            Raise;
          End;
        Finally
          FC.Free;
        End;
      Finally
        T.LeaveCursorOpen := False;
        If T.Owner = Self Then
          Begin
            T.Owner := Nil;
            T.Free;
          End;
      End;

      FC := Nil;
      Try
        FC := TfsFieldCopier.Create;
        For i := 0 To ResultT.FieldCount - 1 Do
          FC.Add(TU.Field(i), ResultT.Field(i));
        Owner.FDatabase.StartTransaction([Nil]);
        Try
          If TU.First Then
            Repeat
              ResultT.Insert;
              FC.Execute;
              ResultT.InsertPost;
            Until Not TU.Next;
          Owner.FDatabase.Commit;
        Except
          Owner.FDatabase.AbortTransaction;
          Raise;
        End;
      Finally
        FC.Free;
      End;

      ResultT.First;
      If aUnion = utNormal Then
        Begin
          ResultT.First;
          T2 := ResultT.CopyUnique(Self, aUnionCase);
          ResultT.Owner := Nil;
          ResultT.Free;
          ResultT := T2;
        End;

      // groupunion
      If Tu.Group <> Nil Then
        If (TfsSqlSELECT(Tu.Group).UnionGroupColumnList <> Nil) Then
          TfsSqlSELECT(Tu.Group).UnionGroupByQueryResult(True, TU);

      If tu.select <> Nil Then
        If (TfsSqlSELECT(Tu.Select).UnionOrderList <> Nil) Then
          TfsSqlSELECT(Tu.Select).DoOrderBy(TfsSqlSELECT(Tu.Select).UnionOrderList, True, ResultT);

      aCursorID := ResultT.CursorID;
      aLiveResult := False;
      ResultT.LeaveCursorOpen := True;
    Finally
      ResultT.Owner := Nil;
      ResultT.Free;
    End;

  Finally
    If TU.Owner = Self Then
      Begin
        TU.Owner := Nil;
        TU.Free;
      End;
  End;
End;

Procedure TfsSqlTableExp.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Var
  T, T2, TU: TfsSqlTableProxy;
  FC: TfsFieldCopier;
  i: Integer;
  ResultT: TfsSqlTableProxy;
  FieldDefList,
    FieldDefListU: TfsSqlFieldDefList;
Begin
  If Assigned(UnionTerm) Then
    Begin

      T := Nil;
      If assigned(NestedTableExp) Then
        T := NestedTableExp.GetResultTable
      Else If assigned(JoinTableExp) Then
        T := JoinTableExp.GetResultTable
      Else If assigned(NonJoinTableExp) Then
        T := NonJoinTableExp.GetResultTable
      Else
        Assert(False);

      TU := UnionTerm.GetResultTable;

      Try

        FieldDefList := Nil;
        FieldDefListU := Nil;
        Try
          FieldDefList := T.ExtractFieldDef;
          FieldDefListU := TU.ExtractFieldDef;
          // prevent for incompctible fields
          If FieldDefList.Count > FieldDefListU.Count Then
            SQLError('Error: Master table field count > Detail table field counr: ' +
              IntToStr(FieldDefList.Count) + ' : ' + IntToStr(FieldDefListU.Count));
          If Not FieldDefList.CompareTypes(FieldDefListU) Then
            SQLError('Error: Field types is not compactibles!');

          ResultT := Owner.fDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
        Finally
          FieldDefList.Free;
          FieldDefListU.Free;
        End;
        Try
          Try
            FC := Nil;
            Try
              FC := TfsFieldCopier.Create;

              For i := 0 To ResultT.FieldCount - 1 Do
                FC.Add(T.Field(i), ResultT.Field(i));
              Owner.FDatabase.StartTransaction([Nil]);
              Try
                If T.First Then
                  Repeat
                    ResultT.Insert;
                    FC.Execute;
                    ResultT.InsertPost;
                  Until Not T.Next;
                Owner.FDatabase.Commit;
              Except
                Owner.FDatabase.AbortTransaction;
                Raise;
              End;
            Finally
              FC.Free;
            End;
          Finally
            If T.Owner = Self Then
              Begin
                T.Owner := Nil;
                T.Free;
              End;
          End;

          FC := Nil;
          Try
            FC := TfsFieldCopier.Create;
            For i := 0 To ResultT.FieldCount - 1 Do
              FC.Add(TU.Field(i), ResultT.Field(i));
            Owner.FDatabase.StartTransaction([Nil]);
            Try
              If TU.First Then
                Repeat
                  ResultT.Insert;
                  FC.Execute;
                  ResultT.InsertPost;
                Until Not TU.Next;
              Owner.FDatabase.Commit;
            Except
              Owner.FDatabase.AbortTransaction;
              Raise;
            End;
          Finally
            FC.Free;
          End;

          If UnionTerm.Union = utNormal Then
            Begin
              ResultT.first;
              T2 := ResultT.CopyUnique(Self, UnionTerm.UnionCase);
              ResultT.Owner := Nil;
              ResultT.Free;
              ResultT := T2;
            End;

          // groupunion
          If Tu.Group <> Nil Then
            If (TfsSqlSELECT(Tu.Group).UnionGroupColumnList <> Nil) Then
              TfsSqlSELECT(Tu.Group).UnionGroupByQueryResult(True, TU);

          If Tu.Select <> Nil Then
            If (TfsSqlSELECT(Tu.Select).UnionOrderList <> Nil) Then
              TfsSqlSELECT(Tu.Select).DoOrderBy(TfsSqlSELECT(Tu.Select).UnionOrderList, True, ResultT);

          aCursorID := ResultT.CursorID;
          aLiveResult := False;
          ResultT.LeaveCursorOpen := True;
        Finally
          ResultT.Owner := Nil;
          ResultT.Free;
        End;

      Finally
        If TU.Owner = Self Then
          Begin
            TU.Owner := Nil;
            TU.Free;
          End;
      End;

    End
  Else
    Begin
      If assigned(NestedTableExp) Then
        NestedTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
      If assigned(JoinTableExp) Then
        JoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
      If assigned(NonJoinTableExp) Then
        NonJoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
    End;
End;

{!!.11 new}

Function TfsSqlTableExp.GetFieldsFromTable(Const TableName: String; List: TList):
  TFSSqlTableProxy;
{-returns fields from table that are ultimately coming from the table
specified in the TableName argument. NIL if not found.}
Begin
  Result := Nil;
  If assigned(NestedTableExp) Then
    Result := NestedTableExp.GetFieldsFromTable(TableName, List)
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.GetFieldsFromTable(TableName, List)
  Else If assigned(NonJoinTableExp) Then
    Result := NonJoinTableExp.GetFieldsFromTable(TableName, List)
  Else
    Assert(False);
End;

Function TfsSqlTableExp.GetResultTable: TFSSqlTableProxy;
Var
  T2: TfsSqlTableProxy;
  FC: TfsFieldCopier;
  i: Integer;
  Select, Group: TFSSpecObject;
Begin
  If assigned(UnionTerm) Then
    Begin
      Result := Nil;
      If assigned(NestedTableExp) Then
        Result := NestedTableExp.GetResultTable
      Else If assigned(JoinTableExp) Then
        Result := JoinTableExp.GetResultTable
      Else If assigned(NonJoinTableExp) Then
        Result := NonJoinTableExp.GetResultTable
      Else
        Assert(False);
      Select := Nil;
      Group := Nil;
      T2 := UnionTerm.GetResultTable;
      If T2.Select <> Nil Then
        Select := T2.Select;
      If T2.Group <> Nil Then
        Group := T2.Group;
      FC := Nil;
      Try
        FC := TfsFieldCopier.Create;
        For i := 0 To Result.FieldCount - 1 Do
          FC.Add(T2.Field(i), Result.Field(i));
        Owner.FDatabase.StartTransaction([Nil]);
        Try
          If T2.First Then
            Repeat
              Result.Insert;
              FC.Execute;
              Result.InsertPost;
            Until Not T2.Next;
          Owner.FDatabase.Commit;
        Except
          Owner.FDatabase.AbortTransaction;
          Raise;
        End;
      Finally
        FC.Free;
        If T2.Owner = Self Then
          Begin
            T2.Owner := Nil;
            T2.Free;
          End;
      End;

      If UnionTerm.Union = utNormal Then
        Begin
          T2 := Result.CopyUnique(Self, UnionTerm.UnionCase);
          If Result.Owner = Self Then
            Begin
              Result.Owner := Nil;
              Result.Free;
            End;
          Result := T2;
        End;
      // groupunion
      If Group <> Nil Then
        If (TfsSqlSELECT(Group).UnionGroupColumnList <> Nil) Then
          TfsSqlSELECT(Group).UnionGroupByQueryResult(True, Result);

      If Select <> Nil Then
        If (TfsSqlSELECT(Select).UnionOrderList <> Nil) Then
          TfsSqlSELECT(Select).DoOrderBy(TfsSqlSELECT(Select).UnionOrderList, True, Result);
    End
  Else
    Begin
      Result := Nil;
      If assigned(NestedTableExp) Then
        Result := NestedTableExp.ResultTable
      Else If assigned(JoinTableExp) Then
        Result := JoinTableExp.ResultTable
      Else If assigned(NonJoinTableExp) Then
        Result := NonJoinTableExp.ResultTable
      Else
        Assert(False);
    End;
End;

Function TfsSqlTableExp.Reduce: Boolean;
Begin
  If assigned(NestedTableExp) Then
    Result := NestedTableExp.Reduce
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.Reduce
  Else
    Result := False;
  If assigned(NonJoinTableExp) Then
    Result := Result Or NonJoinTableExp.Reduce;
End;

Function TfsSqlTableExp.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Begin
  Result := Nil;
  If assigned(NestedTableExp) Then
    Result := NestedTableExp.TargetFieldFromSourceField(F)
  Else If assigned(JoinTableExp) Then
    Result := JoinTableExp.TargetFieldFromSourceField(F)
  Else If assigned(NonJoinTableExp) Then
    NonJoinTableExp.TargetFieldFromSourceField(F)
  Else
    Assert(False);
End;

{ TfsSqlJoinTableExp }

Function TfsSqlJoinTableExp.BuildSimpleFieldExpr(AOwner: TfsSqlNode;
  Const ATableName, AFieldName: String; AField: TfsSqlFieldProxy
  ): TfsSqlSimpleExpression;
Var
  Term: TfsSqlTerm;
  Fact: TfsSqlFactor;
  FieldRef: TfsSqlFieldRef;
Begin
  Result := TfsSqlSimpleExpression.Create(AOwner);
  Term := TfsSqlTerm.Create(Result);
  Fact := TfsSqlFactor.Create(Term);
  FieldRef := TfsSqlFieldRef.Create(Fact);
  FieldRef.TableName := ATableName;
  FieldRef.FieldName := AFieldName;
  FieldRef.FField := AField;
  Fact.FieldRef := FieldRef;
  Term.AddFactor(Fact);
  Result.AddTerm(Term);
End;

Procedure TfsSqlJoinTableExp.ClearColumns;
Var
  i: Integer;
Begin
  If Columns = Nil Then
    Exit;
  For i := 0 To Columns.Count - 1 Do
    If TObject(Columns.Objects[i]) Is TfsSqlSimpleExpression Then
      TObject(Columns.Objects[i]).Free;
  Columns.Clear;
End;

Procedure TfsSqlJoinTableExp.Bind;
Var
  i, j: Integer;
  FL, FR: TfsSqlFieldProxy;
  lCondTerm: TfsSqlCondTerm;
  lCondFact: TfsSqlCondFactor;
  lCondPrim: TfsSqlCondPrimary;
  lSimp1, lSimp2, cSimp, cSimp1, cSimp2: TfsSqlSimpleExpression;
  cTerm: TfsSqlTerm;
  cFact: TfsSqlFactor;
  cScalar: TfsSqlScalarFunc;
  cCoalesce: TfsSqlCoalesceExpression;
  S: String;
  OS: TfsSqlSELECT;
  CF, NewCF: TfsSqlCondFactor;
  CP: TfsSqlCondPrimary;
Const
  UorN: Array[Boolean] Of String = ('UNION', 'NATURAL');
Begin
  //If JoinType = jtUnion Then
  //  SQLError('UNION JOIN is not currently supported by FSSQL');
  If Natural And (JoinType = jtUnion) Then
    SQLError('NATURAL And UNION cannot both be specified on a JOIN');
  If Natural { Or ( JoinType = jtUnion )} Then
    Begin
      If CondExp <> Nil Then
        SQLError(UorN[Natural] + ' joins do not accept an ON clause');
      If UsingList <> Nil Then
        sQLError(UorN[Natural] + ' joins do not accept a USING clause');
    End;
  If Not Natural And Not (JoinType In [jtCross, jtUnion]) Then
    Begin
      If (CondExp = Nil) And (UsingList = Nil) Then
        SQLError('The join must have either an ON or a USING clause');
    End;
  If CondExp <> Nil Then
    CondExp.EnumNodes(ClearBindings, False);
  Assert(assigned(TableRef1));
  TL := TableRef1.BindTable(Self, TableRef1.TableName);
  Assert(assigned(TL));
  Assert(assigned(TableRef2));
  TR := TableRef2.BindTable(Self, TableRef2.TableName);
  Assert(assigned(TR));

  {build column list}
  Assert(Assigned(Columns));
  ClearColumns;

  If Natural Then
    Begin
      UsingCondExp := TfsSqlCondExp.Create(Self);
      lCondTerm := TfsSqlCondTerm.Create(UsingCondExp);
      For i := 0 To TL.FieldCount - 1 Do
        Begin
          FL := TL.Field(i);
          FR := TR.FieldByName(FL.Name);
          If FR <> Nil Then
            Begin
              {common field}
              lCondFact := TfsSqlCondFactor.Create(lCondTerm);
              lCondPrim := TfsSqlCondPrimary.Create(lCondFact);
              lSimp1 := BuildSimpleFieldExpr(lCondPrim, TableRef1.SQLName,
                FL.Name, FL);
              lSimp2 := BuildSimpleFieldExpr(lCondPrim, TableRef2.SQLName,
                FR.Name, FR);
              Case JoinType Of
                jtRightOuter:
                  Columns.AddObject(FL.Name, FR);
                jtFullOuter:
                  Begin
                    cSimp := TfsSqlSimpleExpression.Create(Self);
                    cTerm := TfsSqlTerm.Create(cSimp);
                    cFact := TfsSqlFactor.Create(cTerm);
                    cScalar := TfsSqlScalarFunc.Create(cFact);
                    cScalar.SQLFunction := sfCoalesce;
                    cCoalesce := TfsSqlCoalesceExpression.Create(cScalar);
                    cSimp1 := BuildSimpleFieldExpr(cCoalesce, TableRef1.SQLName,
                      FL.Name, FL);
                    cSimp2 := BuildSimpleFieldExpr(cCoalesce, TableRef2.SQLName,
                      FR.Name, FR);
                    cCoalesce.AddArg(cSimp1);
                    cCoalesce.AddArg(cSimp2);
                    cScalar.CoalesceExp := cCoalesce;
                    cFact.ScalarFunc := cScalar;
                    cTerm.AddFactor(cFact);
                    cSimp.AddTerm(cTerm);
                    Columns.AddObject(FL.Name, cSimp);
                  End;
                Else
                  Columns.AddObject(FL.Name, FL);
              End;
              lCondPrim.SimpleExp1 := lSimp1;
              lCondPrim.SimpleExp2 := lSimp2;
              lCondPrim.RelOp := roEQ;
              lCondFact.CondPrimary := lCondPrim;
              lCondTerm.AddCondFactor(lCondFact);
            End;
        End;
      If lCondTerm.CondFactorCount = 0 Then
        Begin
          lCondTerm.Free;
          UsingCondExp.Free;
          UsingCondExp := Nil;
        End
      Else
        Begin
          UsingCondExp.AddCondTerm(lCondTerm);
          UsingCondExp.MatchType(fstBoolean);
        End;
      For i := 0 To TL.FieldCount - 1 Do
        Begin
          FL := TL.Field(i);
          If Columns.IndexOf(FL.Name) = -1 Then
            Columns.AddObject(FL.Name, FL);
        End;
      For i := 0 To TR.FieldCount - 1 Do
        Begin
          FR := TR.Field(i);
          If Columns.IndexOf(FR.Name) = -1 Then
            Columns.AddObject(FR.Name, FR);
        End;
    End
  Else If UsingList <> Nil Then
    Begin
      UsingCondExp := TfsSqlCondExp.Create(Self);
      lCondTerm := TfsSqlCondTerm.Create(UsingCondExp);
      For i := 0 To UsingList.UsingCount - 1 Do
        Begin
          lCondFact := TfsSqlCondFactor.Create(lCondTerm);
          lCondPrim := TfsSqlCondPrimary.Create(lCondFact);
          FL := TL.FieldByName(UsingList.UsingItem[i].ColumnName);
          If FL = Nil Then
            SQLError(format('Field %s does not exist in table %s.',
              [UsingList.UsingItem[i].ColumnName, TableRef1.SQLName]));
          FR := TR.FieldByName(UsingList.UsingItem[i].ColumnName);
          If FR = Nil Then
            SQLError(format('Field %s does not exist in table %s.',
              [UsingList.UsingItem[i].ColumnName, TableRef2.SQLName]));
          lSimp1 := BuildSimpleFieldExpr(lCondPrim, TableRef1.SQLName,
            FL.Name, FL);
          lSimp2 := BuildSimpleFieldExpr(lCondPrim, TableRef2.SQLName,
            FR.Name, FR);
          Case JoinType Of
            jtRightOuter:
              Columns.AddObject(FL.Name, FR);
            jtFullOuter:
              Begin
                cSimp := TfsSqlSimpleExpression.Create(Self);
                cTerm := TfsSqlTerm.Create(cSimp);
                cFact := TfsSqlFactor.Create(cTerm);
                cScalar := TfsSqlScalarFunc.Create(cFact);
                cScalar.SQLFunction := sfCoalesce;
                cCoalesce := TfsSqlCoalesceExpression.Create(cScalar);
                cSimp1 := BuildSimpleFieldExpr(cCoalesce, TableRef1.SQLName,
                  FL.Name, FL);
                cSimp2 := BuildSimpleFieldExpr(cCoalesce, TableRef2.SQLName,
                  FR.Name, FR);
                cCoalesce.AddArg(cSimp1);
                cCoalesce.AddArg(cSimp2);
                cScalar.CoalesceExp := cCoalesce;
                cFact.ScalarFunc := cScalar;
                cTerm.AddFactor(cFact);
                cSimp.AddTerm(cTerm);
                Columns.AddObject(FL.Name, cSimp);
              End;
            Else
              Columns.AddObject(FL.Name, FL);
          End;
          lCondPrim.SimpleExp1 := lSimp1;
          lCondPrim.SimpleExp2 := lSimp2;
          lCondPrim.RelOp := roEQ;
          lCondFact.CondPrimary := lCondPrim;
          lCondTerm.AddCondFactor(lCondFact);
        End;
      UsingCondExp.AddCondTerm(lCondTerm);
      UsingCondExp.MatchType(fstBoolean);
      For i := 0 To TL.FieldCount - 1 Do
        Begin
          FL := TL.Field(i);
          If Columns.IndexOf(FL.Name) = -1 Then
            Columns.AddObject(FL.Name, FL);
        End;
      For i := 0 To TR.FieldCount - 1 Do
        Begin
          FL := TR.Field(i);
          j := Columns.IndexOf(FL.Name);
          If j = -1 Then
            Columns.AddObject(FL.Name, FL)
          Else If j >= UsingList.UsingCount Then
            Columns.AddObject(TR.Name + '.' + FL.Name, FL);
        End;
    End
  Else
    Begin
      For i := 0 To TL.FieldCount - 1 Do
        Columns.AddObject(TL.Field(i).Name, TL.Field(i));
      For i := 0 To TR.FieldCount - 1 Do
        If Columns.IndexOf(TR.Field(i).Name) = -1 Then
          Columns.AddObject(TR.Field(i).Name, TR.Field(i))
        Else
          Begin
            S := TR.Name + '.' + TR.Field(i).Name;
            If Columns.IndexOf(S) = -1 Then
              Columns.AddObject(S, TR.Field(i))
            Else
              Begin
                j := 2;
                While Columns.IndexOf(S + '_' + IntToStr(j)) <> -1 Do
                  inc(j);
                Columns.AddObject(S + '_' + IntToStr(j), TR.Field(i));
              End;
          End;
    End;

  If (CondExp <> Nil) Then
    Begin
      If (CondExp.CondTermCount = 1) Then
        Begin
          OS := OwnerSelect;
          If (OS <> Nil)
            And (OS.CondExpWhere <> Nil)
            And (OS.CondExpWhere.CondTermCount = 1) Then
            Begin
              For i := 0 To OS.CondExpWhere.CondTerm[0].CondFactorCount - 1 Do
                Begin
                  CF := OS.CondExpWhere.CondTerm[0].CondFactor[i];
                  If Not CF.IsConstant
                    And Not CF.UnaryNot Then
                    Begin
                      CP := CF.CondPrimary;
                      If CP.RelOp In [roEQ, roLE, roL, roG, roGE, roNE] Then
                        Begin
                          If CP.SimpleExp2.IsConstant Or CP.SimpleExp2.IsParameter Then
                            Begin
                              If Cp.SimpleExp1.TermCount = 1 Then
                                If Cp.SimpleExp1.Term[0].FactorCount = 1 Then
                                  If Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> Nil Then
                                    If (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName <> '')
                                      And (
                                      (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName = TableRef1.TableName)
                                      Or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName = TableRef1.Alias)) Then
                                      Begin
                                        NewCF := TfsSqlCondFactor.Create(CondExp.CondTerm[0]);
                                        NewCF.Assign(CF);
                                        CondExp.CondTerm[0].AddCondFactor(NewCF);
                                      End
                                    Else If Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> Nil Then
                                      If (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName <> '')
                                        And (
                                        ((Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName = TableRef2.TableName)
                                        Or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName = TableRef2.Alias))) Then
                                        Begin
                                          NewCF := TfsSqlCondFactor.Create(CondExp.CondTerm[0]);
                                          NewCF.Assign(CF);
                                          CondExp.CondTerm[0].AddCondFactor(NewCF);
                                        End;
                            End;
                        End;
                    End;
                End;

            End;
        End;
      CondExp.MatchType(fstBoolean);
    End;

  Bound := True;
End;

Function TfsSqlJoinTableExp.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  Result := TableRef1.BindTable(AOwner, TableName);
  If Result = Nil Then
    Result := TableRef2.BindTable(AOwner, TableName);
End;

Function TfsSqlJoinTableExp.BindField(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Var
  T: TFSSqlTableProxy;
Begin
  Result := Nil;
  If TableName <> '' Then
    Begin
      T := TableRef1.BindTable(Self, TableName);
      If T <> Nil Then
        If T <> TL Then
          Begin
            Result := TableRef1.TargetFieldFromSourceField(T.FieldByName(FieldName));
            Exit;
          End;
      If T = Nil Then
        Begin
          T := TableRef2.BindTable(Self, TableName);
          If T <> Nil Then {!!.11}
            If T <> TR Then
              Begin
                Result := TableRef2.TargetFieldFromSourceField(T.FieldByName(FieldName));
                Exit;
              End;
        End;
      If T = Nil Then
        SQLError('Unknown field:' + TableName + '.' + FieldName);

      Assert(T <> Nil);
      Result := T.FieldByName(FieldName);
      If Result = Nil Then
        SQLError('Unknown field:' + TableName + '.' + FieldName);
    End
  Else
    Begin
      If TL.FieldByName(FieldName) <> Nil Then
        Begin
          Result := TL.FieldByName(FieldName);
          Exit;
        End;
      If TR.FieldByName(FieldName) <> Nil Then
        Begin
          Result := TR.FieldByName(FieldName);
          Exit;
        End;
      SQLError('Unknown field:' + FieldName);
    End;
End;

Function TfsSqlJoinTableExp.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Var
  i: Integer;
Begin
  Result := Nil;
  If TableName <> '' Then
    Begin
      Result := TableRef1.BindFieldDown(TableName, FieldName);
      If Result = Nil Then
        Result := TableRef2.BindFieldDown(TableName, FieldName);
      If Result = Nil Then
        Exit;

      EnsureResultTable(False {True});

      For i := 0 To pred(Columns.Count) Do
        If Columns.Objects[i] = Result Then
          Begin
            Result := FResultTable.Field(i);
            Exit;
          End;

      Result := Nil;
    End
  Else
    Begin
      If TL.FieldByName(FieldName) <> Nil Then
        Begin
          Result := TL.FieldByName(FieldName);
          Exit;
        End;
      If TR.FieldByName(FieldName) <> Nil Then
        Begin
          Result := TR.FieldByName(FieldName);
          Exit;
        End;
      SQLError('Unknown field:' + FieldName);
    End;
End;

Procedure TfsSqlJoinTableExp.ClearBindings(Node: TfsSqlNode);
Begin
  Node.ClearBinding;
End;

Function TfsSqlJoinTableExp.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  If Not Bound Then
    Bind;
  Result :=
    ((UsingCondExp <> Nil) And UsingCondExp.DependsOn(Table))
    Or ((CondExp <> Nil) And CondExp.DependsOn(Table));
End;

Function TfsSqlJoinTableExp.DoJoin(NeedData: Boolean): TFSSqlTableProxy;
Var
  i: Integer;
  T2: TFSSqlTableProxy;
  F: TfsSqlFieldProxy;
  N: TfsSqlNode;
  FieldDefList: TfsSqlFieldDefList;
  OuterJoinMode: TfsSqlOuterJoinMode;
Begin

  {build a normal answer table}

{build field definition For answer table}
  FieldDefList := TfsSqlFieldDefList.Create;
  Try
    Assert(Assigned(Columns));
    For i := 0 To pred(Columns.Count) Do
      Begin
        If Columns.Objects[i] Is TfsSqlFieldProxy Then
          Begin
            F := TfsSqlFieldProxy(Columns.Objects[i]);
            FieldDefList.AddField(Columns[i], '', F.GetType, F.GetSize, F.GetDecimals, f.GetBlobLevel, f.GetRound);
          End
        Else
          Begin
            N := TfsSqlNode(Columns.Objects[i]);
            FieldDefList.AddField(Columns[i], '', N.GetType, N.GetSize, N.GetDecimals, n.GetBlobLevel, n.GetRound);
          End;
      End;

    Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  Finally
    FieldDefList.Free;
  End;

  Try

    If Joiner = Nil Then
      Begin

        If UsingCondExp <> Nil Then
          Joiner := TfsSqlJoiner.Create(Owner, UsingCondExp)
        Else
          Joiner := TfsSqlJoiner.Create(Owner, CondExp);

        Joiner.Sources.Add(
          TFSSqlTableProxySubset.Create(TL));
        Joiner.Sources.Add(
          TFSSqlTableProxySubset.Create(TR));

      End;

    Joiner.ClearColumnList;

    Assert(Assigned(Columns));
    For i := 0 To pred(Columns.Count) Do
      If Columns.Objects[i] Is TfsSqlFieldProxy Then
        Joiner.AddColumn(
          Nil,
          TfsSqlFieldProxy(Columns.Objects[i]),
          Result.Field(i))
      Else
        Joiner.AddColumn(
          TfsSqlSimpleExpression(Columns.Objects[i]),
          Nil,
          Result.Field(i));

    If NeedData Then
      Begin
        Joiner.Target := Result;
        Owner.FDatabase.StartTransaction([Nil]);
        Try
          Case JoinType Of
            jtLeftOuter:
              OuterJoinMode := jmLeft;
            jtRightOuter:
              OuterJoinMode := jmRight;
            jtFullOuter:
              OuterJoinMode := jmFull;
            Else
              OuterJoinMode := jmNone;
          End;

          Joiner.Execute(Owner.UseIndex, Nil, OuterJoinMode);
        Except
          Owner.FDatabase.AbortTransaction;
          Raise;
        End;
        Owner.FDatabase.Commit;
      End;

    For i := 0 To Result.FieldCount - 1 Do
      Result.Field(i).IsTarget := False;

    If (Parent Is TfsSqlInClause) Or (Parent Is TfsSqlMatchClause) Then
      Begin
        {need an index to allow the IN And MATCH clauses to be evaluated}

        T2 := Result.CopySortedOnAllFields(Self);

        Result.Owner := Nil;
        Result.Free;
        Result := T2;
      End;
  Except
    Result.Owner := Nil;
    Result.Free;
    Raise;
  End;
End;

Procedure TfsSqlJoinTableExp.EnsureResultTable(NeedData: Boolean);
Begin
  If (NeedData And Not HaveData) Then
    Begin
      FResultTable.Free;
      FResultTable := Nil;
    End;
  If FResultTable = Nil Then
    Begin
      FResultTable := Execute2(NeedData);
      HaveData := NeedData;
    End;
End;

Function TfsSqlJoinTableExp.Execute2(NeedData: Boolean): TFSSqlTableProxy;
Begin
  {check that all referenced tables And fields exist}
  If Not Bound Then
    Bind;

  {create the result}
  Result := DoJoin(NeedData);
End;

Function TfsSqlJoinTableExp.GetResultTable: TFSSqlTableProxy;
Begin
  EnsureResultTable(True);
  Result := FResultTable;
End;

Procedure TfsSqlJoinTableExp.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlJoinTableExp Then
    Begin
      Clear;
      JoinType := TfsSqlJoinTableExp(Source).JoinType;
      Natural := TfsSqlJoinTableExp(Source).Natural;
      If TfsSqlJoinTableExp(Source).TableRef1 <> Nil Then
        Begin
          TableRef1 := TfsSqlTableRef.Create(Self);
          TableRef1.Assign(TfsSqlJoinTableExp(Source).TableRef1);
        End;
      If TfsSqlJoinTableExp(Source).TableRef2 <> Nil Then
        Begin
          TableRef2 := TfsSqlTableRef.Create(Self);
          TableRef2.Assign(TfsSqlJoinTableExp(Source).TableRef2);
        End;
      If TfsSqlJoinTableExp(Source).CondExp <> Nil Then
        Begin
          CondExp := TfsSqlCondExp.Create(Self);
          CondExp.Assign(TfsSqlJoinTableExp(Source).CondExp);
        End;
      If TfsSqlJoinTableExp(Source).UsingList <> Nil Then
        Begin
          UsingList := TfsSqlUsingList.Create(Self);
          UsingList.Assign(TfsSqlJoinTableExp(Source).UsingList);
        End;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlJoinTableExp.Clear;
Begin
  ClearColumns;
  UsingCondExp.Free;
  UsingCondExp := Nil;
  TableRef1.Free;
  TableRef1 := Nil;
  TableRef2.Free;
  TableRef2 := Nil;
  CondExp.Free;
  CondExp := Nil;
  UsingList.Free;
  UsingList := Nil;
End;

Destructor TfsSqlJoinTableExp.Destroy;
Begin
  ClearColumns;
  Columns.Free;
  Columns := Nil;
  {only free the tables If they belongs to us}
    {if they are sub-expressions they will be
    destroyed by the owning expression object}
  If (TL <> Nil) And (TL.Owner = Self) Then
    Begin
      TL.Owner := Nil;
      TL.Free;
    End;
  If (TR <> Nil) And (TR.Owner = Self) Then
    Begin
      TR.Owner := Nil;
      TR.Free;
    End;
  Clear;
  Joiner.Free;
  If FResultTable <> Nil Then
    If FResultTable.Owner = Self Then
      Begin
        FResultTable.Owner := Nil;
        FResultTable.Free;
        FResultTable := Nil;
      End;
  UsingCondExp.Free;
  Inherited;
End;

Procedure TfsSqlJoinTableExp.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  TableRef1.EmitSQL(Stream);
  If JoinType = jtCross Then
    WriteStr(Stream, ' CROSS JOIN ')
  Else
    Begin
      If Natural Then
        WriteStr(Stream, ' NATURAL');
      Case JoinType Of
        jtInner:
          WriteStr(Stream, ' INNER');
        jtLeftOuter:
          WriteStr(Stream, ' LEFT OUTER');
        jtRightOuter:
          WriteStr(Stream, ' RIGHT OUTER');
        jtFullOuter:
          WriteStr(Stream, ' FULL OUTER');
        jtUnion:
          WriteStr(Stream, ' UNION');
      End;
      WriteStr(Stream, ' JOIN');
    End;
  TableRef2.EmitSQL(Stream);
  If CondExp <> Nil Then
    Begin
      WriteStr(Stream, ' ON');
      CondExp.EmitSQL(Stream);
    End;
  If UsingList <> Nil Then
    Begin
      WriteStr(Stream, ' USING (');
      UsingList.EmitSQL(Stream);
      WriteStr(Stream, ')');
    End;
End;

Procedure TfsSqlJoinTableExp.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(TableRef1) Then
    TableRef1.EnumNodes(EnumMethod, Deep);
  If assigned(TableRef2) Then
    TableRef2.EnumNodes(EnumMethod, Deep);
  If assigned(CondExp) Then
    CondExp.EnumNodes(EnumMethod, Deep);
  If assigned(UsingList) Then
    UsingList.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlJoinTableExp.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    Other Is TfsSqlJoinTableExp
    And (JoinType = TfsSqlJoinTableExp(Other).JoinType)
    And (Natural = TfsSqlJoinTableExp(Other).Natural)
    And ((BothNil(TableRef1, TfsSqlJoinTableExp(Other).TableRef1)
    Or (BothNonNil(TableRef1, TfsSqlJoinTableExp(Other).TableRef1)
    And TableRef1.Equals(TfsSqlJoinTableExp(Other).TableRef1))))
    And ((BothNil(TableRef2, TfsSqlJoinTableExp(Other).TableRef2)
    Or (BothNonNil(TableRef2, TfsSqlJoinTableExp(Other).TableRef2)
    And TableRef2.Equals(TfsSqlJoinTableExp(Other).TableRef2))))
    And ((BothNil(CondExp, TfsSqlJoinTableExp(Other).CondExp)
    Or (BothNonNil(CondExp, TfsSqlJoinTableExp(Other).CondExp)
    And CondExp.Equals(TfsSqlJoinTableExp(Other).CondExp))))
    And ((BothNil(UsingList, TfsSqlJoinTableExp(Other).UsingList)
    Or (BothNonNil(UsingList, TfsSqlJoinTableExp(Other).UsingList)
    And UsingList.Equals(TfsSqlJoinTableExp(Other).UsingList))));
End;

Procedure TfsSqlJoinTableExp.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Var
  T: TFSSqlTableProxy;
Begin
  Assert(Owner <> Nil);
  aLiveResult := False;
  T := Execute2(True);
  aCursorID := T.CursorID;
  T.LeaveCursorOpen := True;
  If T.Owner = Self Then
    Begin
      T.Owner := Nil;
      T.Free;
    End;
End;

Function TfsSqlJoinTableExp.GetFieldsFromTable(Const TableName: String; List: TList): TFSSqlTableProxy;
Var
  i: Integer;
Begin
  Result := Nil;
  If SameText(TableRef1.Alias, TableName)
    Or SameText(TableRef1.TableName, TableName) Then
    Begin
      Result := ResultTable;
      For i := 0 To pred(Columns.Count) Do
        If Columns.Objects[i] Is TfsSqlFieldProxy Then
          If TfsSqlFieldProxy(Columns.Objects[i]).OwnerTable = TableRef1.FTable Then
            List.Add(Columns.Objects[i]);
      Exit;
    End;
  If SameText(TableRef2.Alias, TableName)
    Or SameText(TableRef2.TableName, TableName) Then
    Begin
      Result := ResultTable;
      For i := 0 To pred(Columns.Count) Do
        If Columns.Objects[i] Is TfsSqlFieldProxy Then
          If TfsSqlFieldProxy(Columns.Objects[i]).OwnerTable = TableRef2.FTable Then
            List.Add(Columns.Objects[i]);
      Exit;
    End;
End;

Function TfsSqlJoinTableExp.Reduce: Boolean;
Begin
  If assigned(CondExp) Then
    Result := CondExp.Reduce
  Else
    Result := False;
  {!!.11 begin}
  If Not Result Then
    If TableRef1.Reduce Then
      Result := True
    Else If TableRef2.Reduce Then
      Result := True;
  {!!.11 end}
End;

Function TfsSqlJoinTableExp.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Var
  i: Integer;
Begin
  For i := 0 To pred(Columns.Count) Do
    If Columns.Objects[i] = F Then
      Begin
        Result := ResultTable.Field(i);
        Exit;
      End;
  {!!.11 begin}
  {We don't have the sought after source field represented in
  our answer table directly, but it might be represented
  indirectly as a field in a nested table expression}
  Result := TableRef1.TargetFieldFromSourceField(F);
  If Result <> Nil Then
    Begin
      For i := 0 To pred(Columns.Count) Do
        If Columns.Objects[i] = Result Then
          Begin
            Result := ResultTable.Field(i);
            Exit;
          End;
    End;
  Result := TableRef2.TargetFieldFromSourceField(F);
  If Result <> Nil Then
    Begin
      For i := 0 To pred(Columns.Count) Do
        If Columns.Objects[i] = Result Then
          Begin
            Result := ResultTable.Field(i);
            Exit;
          End;
    End;
  {!!.11 end}
  Result := Nil;
End;

{ TfsSqlNonJoinTableTerm }

Procedure TfsSqlNonJoinTableTerm.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlNonJoinTableTerm Then
    Begin
      Clear;
      If TfsSqlNonJoinTableTerm(Source).NonJoinTablePrimary <> Nil Then
        Begin
          NonJoinTablePrimary := TfsSqlNonJoinTablePrimary.Create(Self);
          NonJoinTablePrimary.Assign(TfsSqlNonJoinTableTerm(Source).NonJoinTablePrimary);
        End;
    End
  Else
    AssignError(Source);
End;

Function TfsSqlNonJoinTableTerm.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  Result := NonJoinTablePrimary.BindFieldDown(TableName, FieldName);
End;

Function TfsSqlNonJoinTableTerm.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  Result := NonJoinTablePrimary.BindTable(AOwner, TableName);
End;

Procedure TfsSqlNonJoinTableTerm.Clear;
Begin
  NonJoinTablePrimary.Free;
  NonJoinTablePrimary := Nil;
End;

Function TfsSqlNonJoinTableTerm.DependsOn(
  Table: TFSSqlTableProxy): Boolean;
Begin
  Assert(NonJoinTablePrimary <> Nil);
  Result := NonJoinTablePrimary.DependsOn(Table);
End;

Destructor TfsSqlNonJoinTableTerm.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlNonJoinTableTerm.EmitSQL(Stream: TStream);
Begin
  If assigned(NonJoinTablePrimary) Then
    NonJoinTablePrimary.EmitSQL(Stream);
End;

Procedure TfsSqlNonJoinTableTerm.EnsureResultTable(NeedData: Boolean);
Begin
  assert(assigned(NonJoinTablePrimary));
  NonJoinTablePrimary.EnsureResultTable(NeedData);
End;

Procedure TfsSqlNonJoinTableTerm.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(NonJoinTablePrimary) Then
    NonJoinTablePrimary.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlNonJoinTableTerm.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    Other Is TfsSqlNonJoinTableTerm
    And ((BothNil(NonJoinTablePrimary, TfsSqlNonJoinTableTerm(Other).NonJoinTablePrimary)
    Or (BothNonNil(NonJoinTablePrimary, TfsSqlNonJoinTableTerm(Other).NonJoinTablePrimary)
    And NonJoinTablePrimary.Equals(TfsSqlNonJoinTableTerm(Other).NonJoinTablePrimary))))
End;

Procedure TfsSqlNonJoinTableTerm.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Begin
  Assert(NonJoinTablePrimary <> Nil);
  NonJoinTablePrimary.Execute(aLiveResult, aCursorID, RecordsRead);
End;

Function TfsSqlNonJoinTableTerm.GetResultTable: TFSSqlTableProxy;
Begin
  Assert(NonJoinTablePrimary <> Nil);
  Result := NonJoinTablePrimary.ResultTable;
End;

Function TfsSqlNonJoinTableTerm.Reduce: Boolean;
Begin
  Assert(NonJoinTablePrimary <> Nil);
  Result := NonJoinTablePrimary.Reduce;
End;

Function TfsSqlNonJoinTableTerm.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Begin
  Result := NonJoinTablePrimary.TargetFieldFromSourceField(F);
End;

{ TfsSqlNonJoinTableExp }

Procedure TfsSqlNonJoinTableExp.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlNonJoinTableExp Then
    Begin
      Clear;
      If TfsSqlNonJoinTableExp(Source).NonJoinTableTerm <> Nil Then
        Begin
          NonJoinTableTerm := TfsSqlNonJoinTableTerm.Create(Self);
          NonJoinTableTerm.Assign(TfsSqlNonJoinTableExp(Source).NonJoinTableTerm);
        End;
    End
  Else
    AssignError(Source);
End;

Function TfsSqlNonJoinTableExp.BindFieldDown(Const TableName,
  FieldName: String): TfsSqlFieldProxy;
Begin
  Result := NonJoinTableTerm.BindFieldDown(TableName, FieldName);
End;

Function TfsSqlNonJoinTableExp.BindTable(AOwner: TObject;
  Const TableName: String): TFSSqlTableProxy;
Begin
  Result := NonJoinTableTerm.BindTable(AOwner, TableName);
End;

Procedure TfsSqlNonJoinTableExp.Clear;
Begin
  NonJoinTableTerm.Free;
  NonJoinTableTerm := Nil;
End;

Function TfsSqlNonJoinTableExp.DependsOn(Table: TFSSqlTableProxy): Boolean;
Begin
  Assert(NonJoinTableTerm <> Nil);
  Result := NonJoinTableTerm.DependsOn(Table);
End;

Destructor TfsSqlNonJoinTableExp.Destroy;
Begin
  Clear;
  Inherited;
End;

Procedure TfsSqlNonJoinTableExp.EmitSQL(Stream: TStream);
Begin
  If assigned(NonJoinTableTerm) Then
    NonJoinTableTerm.EmitSQL(Stream);
End;

Procedure TfsSqlNonJoinTableExp.EnsureResultTable(NeedData: Boolean);
Begin
  Assert(Assigned(NonJoinTableTerm));
  NonJoinTableTerm.EnsureResultTable(NeedData);
End;

Procedure TfsSqlNonJoinTableExp.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
  If assigned(NonJoinTableTerm) Then
    NonJoinTableTerm.EnumNodes(EnumMethod, Deep);
End;

Function TfsSqlNonJoinTableExp.Equals(Other: TfsSqlNode): Boolean;
Begin
  Result :=
    Other Is TfsSqlNonJoinTableExp
    And ((BothNil(NonJoinTableTerm, TfsSqlNonJoinTableExp(Other).NonJoinTableTerm)
    Or (BothNonNil(NonJoinTableTerm, TfsSqlNonJoinTableExp(Other).NonJoinTableTerm)
    And NonJoinTableTerm.Equals(TfsSqlNonJoinTableExp(Other).NonJoinTableTerm))))
End;

Procedure TfsSqlNonJoinTableExp.Execute(
  Var aLiveResult: Boolean; Var aCursorID: TffCursorID;
  Var RecordsRead: Integer);
Begin
  Assert(NonJoinTableTerm <> Nil);
  NonJoinTableTerm.Execute(aLiveResult, aCursorID, RecordsRead);
End;

{!!.11 new}

Function TfsSqlNonJoinTableExp.GetFieldsFromTable(Const TableName: String; List: TList): TFSSqlTableProxy; {!!.11}
Begin
  Result := Nil;
End;

Function TfsSqlNonJoinTableExp.GetResultTable: TFSSqlTableProxy;
Begin
  Assert(NonJoinTableTerm <> Nil);
  Result := NonJoinTableTerm.ResultTable;
End;

Function TfsSqlNonJoinTableExp.Reduce: Boolean;
Begin
  Assert(NonJoinTableTerm <> Nil);
  Result := NonJoinTableTerm.Reduce;
End;

Constructor TfsSqlJoinTableExp.Create;
Begin
  Inherited;
  Columns := TStringList.Create;
End;

Function TfsSqlNonJoinTableExp.TargetFieldFromSourceField(
  Const F: TfsSqlFieldProxy): TfsSqlFieldProxy;
Begin
  Result := NonJoinTableTerm.TargetFieldFromSourceField(F);
End;

{ TfsSqlUsingItem }

Procedure TfsSqlUsingItem.Assign(Const Source: TfsSqlNode);
Begin
  If Source Is TfsSqlUsingItem Then
    Begin
      ColumnName := TfsSqlUsingItem(Source).ColumnName;
    End
  Else
    AssignError(Source);
End;

Procedure TfsSqlUsingItem.EmitSQL(Stream: TStream);
Begin
  WriteStr(Stream, ' ');
  WriteStr(Stream, ColumnName);
End;

Procedure TfsSqlUsingItem.EnumNodes(EnumMethod: TfsSqlEnumMethod;
  Const Deep: Boolean);
Begin
  EnumMethod(Self);
End;

Function TfsSqlUsingItem.Equals(Other: TfsSqlNode): Boolean;
Begin
  If Other Is TfsSqlUsingItem Then
    Result := ColumnName = TfsSqlUsingItem(Other).ColumnName
  Else
    Result := False;
End;

{===TfsSqlUsingList==================================================}

Function TfsSqlUsingList.AddItem(NewUsing: TfsSqlUsingItem): TfsSqlUsingItem;
Begin
  FUsingItemList.Add(NewUsing);
  Result := NewUsing;
End;
{--------}

Procedure TfsSqlUsingList.Assign(Const Source: TfsSqlNode);
Var
  i: Integer;
Begin
  If Source Is TfsSqlUsingList Then
    Begin
      Clear;
      For i := 0 To pred(TfsSqlUsingList(Source).UsingCount) Do
        AddItem(TfsSqlUsingItem.Create(Self)).Assign(
          TfsSqlUsingList(Source).UsingItem[i]);
    End
  Else
    AssignError(Source);
End;

Constructor TfsSqlUsingList.Create(AParent: TfsSqlNode);
Begin
  Inherited Create(AParent);
  FUsingItemList := TList.Create;
End;
{--------}

Procedure TfsSqlUsingList.Clear;
Var
  i: Integer;
Begin
  For i := 0 To pred(FUsingItemList.Count) Do
    UsingItem[i].Free;
  FUsingItemList.Clear;
End;
{--------}

Destructor TfsSqlUsingList.Destroy;
Begin
  Clear;
  FUsingItemList.Free;
  Inherited;
End;
{--------}

Procedure TfsSqlUsingList.EmitSQL(Stream: TStream);
Var
  i: Integer;
Begin
  UsingItem[0].EmitSQL(Stream);
  For i := 1 To pred(UsingCount) Do
    Begin
      WriteStr(Stream, ', ');
      UsingItem[i].EmitSQL(Stream);
    End;
End;
{--------}

Procedure TfsSqlUsingList.EnumNodes(EnumMethod: TfsSqlEnumMethod; Const Deep: Boolean);
Var
  i: Integer;
Begin
  EnumMethod(Self);
  For i := 0 To pred(UsingCount) Do
    UsingItem[i].EnumNodes(EnumMethod, Deep);
End;
{--------}

Function TfsSqlUsingList.Equals(Other: TfsSqlNode): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Other Is TfsSqlUsingList Then
    Begin
      If UsingCount <> TfsSqlUsingList(Other).UsingCount Then
        Exit;
      For i := 0 To pred(UsingCount) Do
        If Not UsingItem[i].Equals(TfsSqlUsingList(Other).UsingItem[i]) Then
          Exit;
      Result := True;
    End;
End;
{--------}

Function TfsSqlUsingList.GetUsingCount: Integer;
Begin
  Result := FUsingItemList.Count;
End;
{--------}

Function TfsSqlUsingList.GetUsingItem(
  Index: Integer): TfsSqlUsingItem;
Begin
  Result := TfsSqlUsingItem(FUsingItemList[Index]);
End;
{--------}

Procedure TfsSqlUsingList.SetUsingItem(Index: Integer;
  Const Value: TfsSqlUsingItem);
Begin
  FUsingItemList[Index] := Value;
End;
{====================================================================}

Procedure Sort(Locale: Integer; Var iArray: Array Of AnsiChar);

  Function AnsiCompareStrX(Const S1, S2: String): Integer;
  Begin
    Result := CompareStringA(Locale, NORM_IGNORECASE, PChar(S1), Length(S1),
      PChar(S2), Length(S2)) - 2;
  End;

  Procedure QuickSort(iLow, iHigh: Integer);
  Var
    iLo, iHi: Integer;
    x, Temp: AnsiChar;
  Begin
    iLo := iLow;
    iHi := iHigh;
    X := iArray[(iLow + iHigh) Div 2];
    Repeat
      While AnsiCompareStrX(iArray[iLo], X) < 0 Do
        Inc(iLo);
      While AnsiCompareStrX(iArray[iHi], X) > 0 Do
        Dec(iHi);
      If (iLo <= iHi) Then
        Begin
          Temp := iArray[iLo];
          iArray[iLo] := iArray[iHi];
          iArray[iHi] := Temp;
          Inc(iLo);
          Dec(iHi);
        End;
    Until iLo > iHi;
    If (iHi > iLow) Then QuickSort(iLow, iHi);
    If (iLo < iHigh) Then QuickSort(iLo, iHigh);
  End;

Begin
  QuickSort(Low(iArray), High(iArray));
End;

Initialization
  {calculate TimeDelta as one second}{!!.01}
  TimeDelta := EncodeTime(0, 0, 2, 0) - EncodeTime(0, 0, 1, 0);

  // temporary result for like - at V2 new engine
  For iC := 0 To 255 Do
    OsChar[iC] := AnsiLowerCase(chr(iC))[1];
  Sort(LOCALE_USER_DEFAULT, OsChar);
End.

