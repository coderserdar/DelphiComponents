{*********************************************************}
{* FlashFiler: SQL Class Definitions                     *}
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

{2.11 - extensive changes throughout}

{$I ffdefine.inc}

{Enable the following to have index optimization analysis and usage
 information logged to a file (used for debugging)}
{.$DEFINE LogIndexAnalysis}

{Enable the following to have transformation information
 logged to a file (used for debugging)}
{$DEFINE LogTransformations}

{Enable the following to have writes counted}
{.$DEFINE CountWrites}

{Enable the following to have the root node made available through the global
 LastStatement variable below (used for debugging only)}
{.$DEFINE ExposeLastStatement}

unit ffsqldef;

interface
uses
  Windows,
  SysUtils,
  Classes,
  DB,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  ffllbase,
  ffsqldb,
  ffhash;

const
  fftInterval = fftReserved20;
{$IFDEF LogIndexAnalysis}
  IALogFile = 'c:\ffialog.txt';
{$ENDIF}
{$IFDEF LogTransformations}
  TRLogFile = 'c:\fftrlog.txt';
{$ENDIF}

{$IFDEF LogIndexAnalysis}
var
  IALog : System.Text;
{$ENDIF}
{$IFDEF LogTransformations}
var
  TRLog : System.Text;
{$ENDIF}

type
  TffSqlAggQueryMode = (aqmIdle, aqmGrouping, aqmHaving);

  TffSqlNode = class;
  TffSqlStatement = class;
  TffSqlEnumMethod = procedure(Node: TffSqlNode) of object;
  TffSqlAggregate = class;
  TffSqlSELECT = class;
  TffSqlColumnListOwner = class;
  TffSqlRelOp = (roNone, roEQ, roLE, roL, roG, roGE, roNE);
  TffSqlNode = class(TFFObject)
  protected
    FParent : TffSqlNode;
    FOwner : TffSqlStatement;
    FOwnerStmt: TffSqlColumnListOwner;                                 {!!.11}
    procedure WriteStr(Stream: TStream; const S: string);
    procedure WriteEOF(Stream: TStream);
    procedure AddTableReference(Select: TffSqlSELECT); virtual;
    procedure AddColumnDef(Target: TffSqlColumnListOwner); virtual;
    procedure AddAggregate(Target: TList); virtual;
    procedure ClearBinding; virtual;
    function IsAncestor(const Node : TffSqlNode) : Boolean;
      { Returns True if Node is an ancestor of this node. }
    procedure ResetConstant; virtual;
    procedure FlagAggregate(Select: TffSqlSELECT); virtual;
    function GetType: TffFieldType; virtual;
    function GetSize: Integer; virtual;
    function GetDecimals: Integer; virtual;
    function GetOwner: TffSqlStatement;
    function GetOwnerSelect : TffSqlSelect;
    function GetOwnerStmt: TFFSqlColumnListOwner;                      {!!.11}
    procedure SQLError(const ErrorMsg: string);
    procedure AssignError(Source: TffSqlNode);
    procedure TypeMismatch;
    function BindField(const TableName,
      FieldName: string): TFFSqlFieldProxy; virtual;
    function IsAggregate: Boolean; virtual;
  public
    constructor Create(AParent: TffSqlNode);
    property Parent : TffSqlNode read FParent write FParent;
    property Owner :   TffSqlStatement read GetOwner;
    property OwnerSelect : TffSqlSelect read GetOwnerSelect;
    property OwnerStmt: TFFSqlColumnListOwner read GetOwnerStmt;       {!!.11}
    procedure EmitSQL(Stream : TStream); virtual;
    function SQLText: string;
    function Equals(Other: TffSqlNode): Boolean; virtual; abstract;
    procedure Assign(const Source: TffSqlNode); virtual; abstract;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
      virtual; abstract;
  end;

  TffSqlFieldRef = class(TffSqlNode)
  protected
    FFieldName: string;
    FTableName: string;
    TypeKnown : Boolean;
    FType : TffFieldType;
    FField : TFFSqlFieldProxy;
    FGroupField : TffSqlFieldProxy;
    WasWildcard: Boolean;
    procedure ClearBinding; override;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    function GetType: TffFieldType; override;
    procedure CheckType;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetField: TFFSqlFieldProxy;
    function GetGroupField : TffSqlFieldProxy;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    property TableName : string read FTableName write FTableName;
    property FieldName : string read FFieldName write FFieldName;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue : Variant;
    property Field: TFFSqlFieldProxy read GetField;
    property GroupField : TffSqlFieldProxy read GetGroupField;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function QualName : string;
    function IsNull: Boolean;
  end;

  TffSqlSimpleExpression = class;

  TAggCounter = class(TffObject)
  protected
    FMin, FMax : variant;
    FSum, FCount : double;
    function GetMax: Variant;
    function GetMin: Variant;
    function GetSum: Variant;
    function GetAvg: Variant;
  public
    procedure Reset;
    procedure Add(const Value: Variant);
    property Min: Variant read GetMin;
    property Max: Variant read GetMax;
    property Count: double read FCount;
    property Sum: Variant read GetSum;
    property Avg: Variant read GetAvg;
  end;

  TffSQLAggFunction = (agCount, agMin, agMax, agSum, agAvg);

  TffSqlAggregate = class(TffSqlNode)
  protected
    FAgFunction: TffSQLAggFunction;
    FSimpleExpression : TffSqlSimpleExpression;
    FDistinct : Boolean;
    FCounter : TAggCounter;
    FSourceField: TFFSqlFieldProxy;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);
    function GetSize: Integer; override;
    function GetDecimals: Integer; override;
    function GetType: TffFieldType; override;
    procedure FlagAggregate(Select: TffSqlSELECT); override;
    procedure AddAggregate(Target: TList); override;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    destructor Destroy; override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property AgFunction : TffSQLAggFunction read FAgFunction write FAgFunction;
    property SimpleExpression : TffSqlSimpleExpression
      read FSimpleExpression write FSimpleExpression;                
    property Distinct: Boolean read FDistinct write FDistinct;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetAggregateValue: Variant;
    procedure CreateCounter(SourceField: TFFSqlFieldProxy);
    procedure DeleteCounter;
    procedure ResetCounters;
    procedure Update;
    function ValidType(aType : TffFieldType) : Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlColumn = class(TffSqlNode)
  protected
    FColumnName: string;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property ColumnName: string read FColumnName write FColumnName;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlBaseColumn = class(TffSqlNode)
  protected
    FFieldName: string;
    FTableName: string;
  public
    property TableName: string read FTableName write FTableName;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TffSqlGroupColumn = class(TffSqlBaseColumn)
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function QualColumnName: string; virtual;
  end;

  TffSqlOrderColumn = class(TffSqlBaseColumn)
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function QualColumnName: string;
  end;

  TffSqlSelection = class;

  TffSqlGroupColumnList = class(TffSqlNode)
  protected
    ColumnList : TList;
    procedure Clear;
    function GetColumn(Index: Integer): TffSqlGroupColumn;
    procedure SetColumn(Index: Integer; const Value: TffSqlGroupColumn);
    function GetColumnCount: Integer;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddColumn(Column: TffSqlGroupColumn): TffSqlGroupColumn;
    property ColumnCount : Integer read GetColumnCount;
    property Column[Index: Integer] : TffSqlGroupColumn read GetColumn write SetColumn;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function Contains(const aColName : string;
                            Se: TffSqlSelection): Boolean;
  end;

  TffSqlIsOp = (ioNull, ioTrue, ioFalse, ioUnknown);
  TffSqlIsTest = class(TffSqlNode)
  protected
    FUnaryNot : Boolean;
    FIsOp : TffSqlIsOp;
    procedure MatchType(ExpectedType: TffFieldType);
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property UnaryNot: Boolean read FUnaryNot write FUnaryNot;
    property IsOp : TffSqlIsOp read FIsOp write FIsOp;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean(const TestValue: Variant): Boolean;
    function Evaluate(Expression: TffSqlSimpleExpression): Boolean;
  end;

  TffSqlBetweenClause = class(TffSqlNode)
  protected
    FSimpleHigh: TffSqlSimpleExpression;
    FSimpleLow: TffSqlSimpleExpression;
    FNegated : Boolean;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property Negated : Boolean read FNegated write FNegated;
    property SimpleLow : TffSqlSimpleExpression read FSimpleLow write FSimpleLow;
    property SimpleHigh : TffSqlSimpleExpression read FSimpleHigh write FSimpleHigh;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean(const TestValue: Variant): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlLikePattern = class(TffObject)
  protected
    LeadPattern,
    TrailPattern : string;
    LeadMask,
    TrailMask: string;
    FloatPatterns,
    FloatMasks: TStringList;
  public
    constructor Create(SearchPattern: string; const Escape: string);
    {S is the search pattern; Escape is an optional one-character escape
    character}
    {S contains the string to be searched for, and optionally one or more
    occurrences of
      '%' (match zero or more characters of any kind), and/or
      '_'  (match exactly one character of any kind)
      If the Escape character is specified, it defines a character to prefix '%'
    or '_' with
      to indicate a literal '%' or '_', respectively, in the search phrase S.}
     {the search must be case sensitive ('a' <> 'A') }
    destructor Destroy; override;
    function Find(const TextToSearch: Variant; IgnoreCase: Boolean): Boolean;  {!!.13}
     {examples:
        S = '%Berkeley%' - Find returns true if the string 'Berkeley' exists
    anywhere in TextToSearch
        S = 'S__' - Find returns true if TextToSearch is exactly thee characters
    long and starts with an upper-case 'S'
        S = '%c___' - Find returns True if length(TextToSearch) >= 4 and the
    last but three is 'c'
        S = '=_%' and Escape = '=' - Find returns True if TextToSearch begins
    with an underscore.
      }
  end;

  TffSqlLikeClause = class(TffSqlNode)
  protected
    FSimpleExp: TffSqlSimpleExpression;
    FEscapeExp: TffSqlSimpleExpression;
    FNegated : Boolean;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    Limited: Boolean;
    LikePattern: TffSqlLikePattern;
    FBMCompat : Boolean;                                               {!!.11}
    BMCompatChecked : Boolean;                                         {!!.11}
    FBMTable: PBTable;                                                 {!!.11}
    FBMPhrase: string;                                                 {!!.11}
    FIgnoreCase: Boolean;                                              {!!.13}
    procedure CheckBMCompat;                                           {!!.11}
    function IsBMCompatible: Boolean;                                  {!!.11}
    function GetBmTable: PBTable;                                      {!!.11}
    function CanLimit: Boolean;
    function CanReplaceWithCompare: Boolean;
    procedure CheckIsConstant;
    function GetLowLimit: string;
    function GetHighLimit: string;
    function IsConstant: Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SimpleExp : TffSqlSimpleExpression read FSimpleExp write FSimpleExp;
    property EscapeExp: TffSqlSimpleExpression read FEscapeExp write FEscapeExp;
    property Negated : Boolean read FNegated write FNegated;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean(const TestValue: Variant): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    property BmTable: PBTable read GetBmTable;                         {!!.11}
    property BmPhrase: string read FBmPhrase;                          {!!.11}
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;   {!!.13}
  end;

  TffSqlSimpleExpressionList = class;

  TffSqlInClause = class(TffSqlNode)
  protected
    FSimpleExp: TffSqlSimpleExpressionList;
    FNegated : Boolean;
    FSubQuery : TffSqlSELECT;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SimpleExpList : TffSqlSimpleExpressionList
      read FSimpleExp write FSimpleExp;
    property SubQuery : TffSqlSELECT read FSubQuery write FSubQuery;
    property Negated : Boolean read FNegated write FNegated;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean(const TestValue: Variant): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlTableExp = class;

  TffSqlMatchOption = (moUnspec, moPartial, moFull);
  TffSqlMatchClause = class(TffSqlNode)
  protected
    FSubQuery : TffSqlSELECT;
    FOption: TffSqlMatchOption;
    FUnique : Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property Unique: Boolean read FUnique write FUnique;
    property Option: TffSqlMatchOption read FOption write FOption;
    property SubQuery : TffSqlSELECT read FSubQuery write FSubQuery;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean(const TestValue: Variant): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlAllOrAnyClause = class(TffSqlNode)
  protected
    FSubQuery : TffSqlSELECT;
    FAll : Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function Compare(RelOp: TffSqlRelOp; const Val: Variant): Boolean;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property All: Boolean read FAll write FAll;
    property SubQuery : TffSqlSELECT read FSubQuery write FSubQuery;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlExistsClause = class(TffSqlNode)
  protected
    FSubQuery : TffSqlSELECT;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SubQuery : TffSqlSELECT read FSubQuery write FSubQuery;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlUniqueClause = class(TffSqlNode)
  protected
    FSubQuery: TffSqlTableExp;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SubQuery : TffSqlTableExp read FSubQuery write FSubQuery;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlCondPrimary = class(TffSqlNode)
  protected
    FSimpleExp1: TffSqlSimpleExpression;
    FRelOp: TffSqlRelOp;
    FSimpleExp2: TffSqlSimpleExpression;
    FBetweenClause : TffSqlBetweenClause;
    FLikeClause : TffSqlLikeClause;
    FInClause : TffSqlInClause;
    FIsTest : TffSqlIsTest;
    FAllOrAnyClause : TffSqlAllOrAnyClause;
    FExistsClause : TFfSqlExistsClause;
    FUniqueClause : TFfSqlUniqueClause;
    FMatchClause : TffSqlMatchClause;
    TypeChecked : Boolean;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    procedure CheckType;
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    function JustSimpleExpression: Boolean;
    procedure MatchType(ExpectedType: TffFieldType);                   {!!.11}
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    procedure Assign(const Source: TffSqlNode); override;
    property SimpleExp1 : TffSqlSimpleExpression
      read FSimpleExp1 write FSimpleExp1;
    property RelOp : TffSqlRelOp read FRelOp write FRelOp;
    property SimpleExp2 : TffSqlSimpleExpression
      read FSimpleExp2 write FSimpleExp2;
    property BetweenClause : TffSqlBetweenClause
      read FBetweenClause write FBetweenClause;
    property LikeClause : TffSqlLikeClause read FLikeClause write FLikeClause;
    property InClause : TffSqlInClause read FInClause write FInClause;
    property IsTest : TffSqlIsTest read FIsTest write FIsTest;
    property AllOrAnyClause : TffSqlAllOrAnyClause
      read FAllOrAnyClause write FAllOrAnyClause;
    property ExistsClause : TffSqlExistsClause
      read FExistsClause write FExistsClause;
    property UniqueClause : TffSqlUniqueClause
      read FUniqueClause write FUniqueClause;
    property MatchClause : TffSqlMatchClause read FMatchClause write FMatchClause;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function GetValue: Variant;
    procedure BindHaving;
    function IsRelationTo(Table : TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy;
      var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlCondFactor = class(TffSqlNode)
  protected
    FUnaryNot: Boolean;
    FCondPrimary: TffSqlCondPrimary;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    TmpKnown: Boolean;
    TmpValue: Boolean;
    EvalLevel: Integer;
    procedure CheckIsConstant;
    procedure Clear;
    function IsConstant: Boolean;
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);                   {!!.11}
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    destructor Destroy; override;
    property UnaryNot : Boolean read FUnaryNot write FUnaryNot;
    property CondPrimary : TffSqlCondPrimary read FCondPrimary write FCondPrimary;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function GetValue: Variant;
    procedure BindHaving;
    function IsRelationTo(Table : TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy;
      var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    procedure MarkTrue;
    procedure MarkUnknown;
  end;

  TffSqlCondExp = class;
  TFFObjectProc = procedure of object;

  TFFSqlKeyRelation = record
    CondF : TFFSqlCondFactor;
    RelationB: array[0..pred(ffcl_MaxIndexFlds)] of TffSqlCondFactor; {!!.11}
    NativeKeyIndex: Integer;
    RelationFieldCount,
    RelationKeyFieldCount: Integer;
    RelationOperators : array[0..pred(ffcl_MaxIndexFlds)] of TffSqlRelOp;
    RelationOperatorB : array[0..pred(ffcl_MaxIndexFlds)] of TffSqlRelOp; {!!.11}
    RelationKeyIsUnique: Boolean;
    RelationKeyIsCaseInsensitive: Boolean;
    RelationKeyIndexAsc : Boolean;
    ArgExpressionB : array[0..pred(ffcl_MaxIndexFlds)] of TffSqlSimpleExpression; {!!.11}
    ArgExpressions : array[0..pred(ffcl_MaxIndexFlds)] of TffSqlSimpleExpression;
    {$IFDEF LogIndexAnalysis}
    RelationFields : array[0..pred(ffcl_MaxIndexFlds)] of TFFSqlFieldProxy;
    {$ENDIF}
    SameCases : array[0..pred(ffcl_MaxIndexFlds)] of Boolean;
    SameCaseB: array[0..pred(ffcl_MaxIndexFlds)] of Boolean; {!!.11}
    DepIndex: Integer;
  end;

  TFFSqlTableProxySubset = class(TffObject)
  protected
    FTable : TFFSqlTableProxy;
    FOpposite: TFFSqlTableProxy;
    FOuter: Boolean;
  public
    Relations: Integer;
    KeyRelation: TffSqlKeyRelation;
    constructor Create(Table: TFFSqlTableProxy);
    function EqualKeyDepth: Integer;
    procedure Iterate(Iterator: TFFSqlTableIterator; Cookie: TffWord32);
    property Table : TFFSqlTableProxy read FTable;
    procedure Assign(const Source: TFFSqlTableProxySubset);
    function UniqueValue: Boolean;
    function ClosedSegment: Boolean;
    function KeyDepth: Integer;
    property Outer: Boolean read FOuter write FOuter;
    property Opposite: TFFSqlTableProxy read FOpposite write FOpposite;
  end;

  TFFSqlTableProxySubsetList = class;

  TffSqlCondTerm = class(TffSqlNode)
  protected
    CondFactorList : TList;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    OrderedSources : TFFSqlTableProxySubsetList;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function GetCondFactor(Index: Integer): TffSqlCondFactor;
    procedure SetCondFactor(Index: Integer; const Value: TffSqlCondFactor);
    function GetCondFactorCount: Integer;
    function GetSize: Integer; override;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function Reduce: Boolean;
    procedure ResetConstant; override;
    function AsBooleanLevel(Level: Integer): Boolean;
    procedure MatchType(ExpectedType: TffFieldType);                   {!!.11}
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddCondFactor(Factor : TffSqlCondFactor): TffSqlCondFactor;
    function InsertCondFactor(Index: Integer; Factor : TffSqlCondFactor):
      TffSqlCondFactor;
    property CondFactorCount : Integer read GetCondFactorCount;
    property CondFactor[Index: Integer] : TffSqlCondFactor
      read GetCondFactor write SetCondFactor;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function GetValue: Variant;
    procedure BindHaving;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    procedure SetLevelDep(List: TFFSqlTableProxySubsetList);
  end;

  TFFSqlTableProxySubsetList = class(TffObject)
  protected
    FList : TList;
    Level : Integer;
    FCondTerm : TffSqlCondTerm;
    FCreateResultRecord : TFFObjectProc;
    FRecordsRead : Longint;
    FOwner: TffSqlStatement;
    WroteRow: Boolean;
    FOuterJoin: Boolean;
    FSkipInner: Boolean;
    V : array[0..pred(ffcl_MaxIndexFlds)] of Variant;
    VB : array[0..pred(ffcl_MaxIndexFlds)] of Variant; {!!.11}
    procedure ReadSources;
    function GetItem(Index: Integer): TFFSqlTableProxySubset;
    function GetCount: Integer;
    function ProcessLevel(Cookie1: TffWord32): Boolean;
    procedure Clear;
    function Insert(
      TableProxySubset: TFFSqlTableProxySubset): TFFSqlTableProxySubset;
  public
    constructor Create(AOwner: TffSqlStatement);
    destructor Destroy; override;
    function Add(TableProxySubset: TFFSqlTableProxySubset): TFFSqlTableProxySubset;
    procedure Delete(Index: Integer);
    property Item[Index: Integer]: TFFSqlTableProxySubset read GetItem;
    property Count: Integer read GetCount;
    procedure Assign(const Source: TFFSqlTableProxySubsetList);
    function RelationUsed(Relation: TffSqlCondFactor): Boolean;
    function DependencyExists(Table : TFFSqlTableProxy): Boolean;
    procedure Join(
      CondTerm: TffSqlCondTerm;
      CreateResultRecord: TFFObjectProc);
    property RecordsRead : Longint read FRecordsRead;
    property Owner: TffSqlStatement read FOwner;
    property OuterJoin: Boolean read FOuterJoin write FOuterJoin;
    property SkipInner: Boolean read FSkipInner write FSkipInner;
  end;

  TffSqlCondExp = class(TffSqlNode)
  protected
    CondTermList : TList;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function GetCondTerm(Index: Integer): TffSqlCondTerm;
    procedure SetCondTerm(Index: Integer; const Value: TffSqlCondTerm);
    function GetCondTermCount: Integer;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    procedure ResetConstant; override;
    function Reduce: Boolean;
    function AsBooleanLevel(Level: Integer): Boolean;
    procedure SetLevelDep(List: TFFSqlTableProxySubsetList);
    function GetTitle(const Qualified : boolean): string;              {!!.11}
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddCondTerm(Term : TffSqlCondTerm): TffSqlCondTerm;
    property CondTermCount : Integer read GetCondTermCount;
    property CondTerm[Index: Integer] : TffSqlCondTerm
      read GetCondTerm write SetCondTerm;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function AsBoolean: Boolean;
    function GetValue: Variant;
    procedure BindHaving;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffComp = array[0..7] of Byte;

  TffSqlFloatLiteral = class(TffSqlNode)
  protected
    FValue : string;
    SingleValue : single;
    DoubleValue : double;
    ExtendedValue : extended;
    CompValue : TffComp;
    CurrencyValue : currency;
    Converted: Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlIntegerLiteral = class(TffSqlNode)
  protected
    FValue : string;
    Int32Value: Integer;
    Converted: Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlStringLiteral = class(TffSqlNode)
  protected
    FValue : string;
    FType : TffFieldType;
    Converted : Boolean;
    CharValue : Char;
    WideCharValue : WideChar;
    ShortStringValue : ShortString;
    ShortAnsiStringValue : ShortString;
    NullStringValue : string;
    NullAnsiStrValue : string;
    WideStringValue : WideString;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    constructor Create(AParent: TffSqlNode);
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlIntervalDef = (iUnspec, iYear, iMonth, iDay, iHour, iMinute, iSecond);
  TffSqlIntervalLiteral = class(TffSqlNode)
  protected
    FValue : string;
    FStartDef : TffSqlIntervalDef;
    FEndDef : TffSqlIntervalDef;
    Y1, M1, D1, H1, S1 : Integer;
    Converted: Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
    function AddIntervalTo(Target: TDateTime): TDateTime;
    function SubtractIntervalFrom(Target: TDateTime): TDateTime;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    property StartDef : TffSqlIntervalDef read FStartDef write FStartDef;
    property EndDef : TffSqlIntervalDef read FEndDef write FEndDef;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlTimestampLiteral = class(TffSqlNode)
  protected
    FValue : string;
    DateTimeValue: TDateTime;
    Converted: Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlTimeLiteral = class(TffSqlNode)
  protected
    FValue : string;
    TimeValue : TDateTime;
    Converted : Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlDateLiteral = class(TffSqlNode)
  protected
    FValue : string;
    DateValue : TDateTime;
    Converted : Boolean;
    procedure ConvertToNative;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : string read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlBooleanLiteral = class(TffSqlNode)
  protected
    FValue : Boolean;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetType: TffFieldType; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Value : Boolean read FValue write FValue;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Boolean;
  end;

  TffSqlLiteral = class(TffSqlNode)
  protected
    FFloatLiteral: TffSqlFloatLiteral;
    FIntegerLiteral: TffSqlIntegerLiteral;
    FStringLiteral: TffSqlStringLiteral;
    FDateLiteral : TffSqlDateLiteral;
    FTimeLiteral : TffSqlTimeLiteral;
    FTimeStampLiteral : TffSqlTimestampLiteral;
    FIntervalLiteral : TffSqlIntervalLiteral;
    FBooleanLiteral: TffSqlBooleanLiteral;
    procedure Clear;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function AddIntervalTo(Target: TDateTime): TDateTime;
    function SubtractIntervalFrom(Target: TDateTime): TDateTime;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    destructor Destroy; override;
    property BooleanLiteral : TffSqlBooleanLiteral
      read FBooleanLiteral write FBooleanLiteral;
    property FloatLiteral : TffSqlFloatLiteral
      read FFloatLiteral write FFloatLiteral;
    property IntegerLiteral : TffSqlIntegerLiteral
      read FIntegerLiteral write FIntegerLiteral;
    property StringLiteral : TffSqlStringLiteral
      read FStringLiteral write FStringLiteral;
    property DateLiteral : TffSqlDateLiteral
      read FDateLiteral write FDateLiteral;
    property TimeLiteral : TffSqlTimeLiteral
      read FTimeLiteral write FTimeLiteral;
    property TimeStampLiteral : TffSqlTimestampLiteral
      read FTimestampLiteral write FTimestampLiteral;
    property IntervalLiteral : TffSqlIntervalLiteral
      read FIntervalLiteral write FIntervalLiteral;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue : Variant;
  end;

  TffSqlParam = class(TffSqlNode)
  protected
    FParmIndex: Integer;
    function GetSize: Integer; override;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    function GetType: TffFieldType; override;
    procedure MatchType(ExpectedType: TffFieldType);
    function GetDecimals: Integer; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    property ParmIndex: Integer read FParmIndex;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
  end;

  TffSqlCoalesceExpression = class(TffSqlNode)
  protected
    ArgList : TList;
    procedure Clear;
    function GetArg(Index: Integer): TffSqlSimpleExpression;
    function GetArgCount: Integer;
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    property ArgCount : Integer read GetArgCount;
    property Arg[Index: Integer]: TffSqlSimpleExpression read GetArg;
    function AddArg(Value: TffSqlSimpleExpression): TffSqlSimpleExpression;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlWhenClause = class(TffSqlNode)
  protected
    FWhenExp : TffSqlCondExp;
    FThenExp : TffSqlSimpleExpression;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property WhenExp : TffSqlCondExp read FWhenExp write FWhenExp;
    property ThenExp : TffSqlSimpleExpression read FThenExp write FThenExp;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlWhenClauseList = class(TffSqlNode)
  protected
    WhenClauseList : TList;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function GetWhenClause(Index: Integer): TffSqlWhenClause;
    function GetWhenClauseCount: Integer;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    property WhenClauseCount : Integer read GetWhenClauseCount;
    property WhenClause[Index: Integer]: TffSqlWhenClause read GetWhenClause;
    function AddWhenClause(Value: TffSqlWhenClause): TffSqlWhenClause;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlCaseExpression = class(TffSqlNode)
  protected
    FWhenClauseList : TffSqlWhenClauseList;
    FElseExp : TffSqlSimpleExpression;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    procedure CheckIsConstant;
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    function IsConstant: Boolean;
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property WhenClauseList : TffSqlWhenClauseList
      read FWhenClauseList write FWhenClauseList;
    property ElseExp : TffSqlSimpleExpression read FElseExp write FElseExp;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlScalarFunction = (sfCase, sfCharlen, sfCoalesce, sfCurrentDate, sfCurrentTime,
    sfCurrentTimestamp, sfCurrentUser, sfLower, sfUpper, sfPosition,
    sfSessionUser, sfSubstring, sfSystemUser, sfTrim, sfExtract, sfNullIf,
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfPower, sfRand, sfRound);   {!!.11}
  TffSqlLTB = (ltbBoth, ltbLeading, ltbTrailing);
  TffSqlScalarFunc = class(TffSqlNode)
  protected
    FSQLFunction : TffSqlScalarFunction;
    FArg1 : TffSqlSimpleExpression;
    FArg2 : TffSqlSimpleExpression;
    FArg3 : TffSqlSimpleExpression;
    FLTB : TffSqlLTB;
    FXDef : TffSqlIntervalDef;
    FCaseExp : TffSqlCaseExpression;
    FCoalesceExp : TffSqlCoalesceExpression;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    FType : TffFieldType;
    TypeKnown : Boolean;
    ConstantValue: Variant;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function IsFieldFrom(Table: TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy): Boolean;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    procedure CheckType;
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SQLFunction : TffSqlScalarFunction
      read FSQLFunction write FSQLFunction;
    property Arg1 : TffSqlSimpleExpression read FArg1 write FArg1;
    property Arg2 : TffSqlSimpleExpression read FArg2 write FArg2;
    property Arg3 : TffSqlSimpleExpression read FArg3 write FArg3;
    property LTB : TffSqlLTB read FLTB write FLTB;
    property XDef : TffSqlIntervalDef read FXDef write FXDef;
    property CaseExp : TffSqlCaseExpression read FCaseExp write FCaseExp;
    property CoalesceExp : TFFSqlCoalesceExpression
      read FCoalesceExp write FCoalesceExp;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlMulOp = (moMul, moDiv);
  TffSqlFactor = class(TffSqlNode)
  protected
    TypeKnown : Boolean;
    FType : TffFieldType;
    FMulOp: TffSqlMulOp;
    FUnaryMinus : Boolean;
    FCondExp: TffSqlCondExp;
    FFieldRef: TffSqlFieldRef;
    FLiteral: TffSqlLiteral;
    FParam: TffSqlParam;
    FAggregate : TffSqlAggregate;
    FSubQuery : TffSqlSELECT;
    FScalarFunc : TffSqlScalarFunc;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    procedure CheckType;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);
    function IsAggregate: Boolean; override;
    function AddIntervalTo(Target: TDateTime): TDateTime;
    function Reduce: Boolean;
    function SubtractIntervalFrom(Target: TDateTime): TDateTime;
    procedure ResetConstant; override;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    destructor Destroy; override;
    property MulOp :TffSqlMulOp read FMulOp write FMulOp;
    property UnaryMinus : Boolean read FUnaryMinus write FUnaryMinus;
    property CondExp : TffSqlCondExp read FCondExp write FCondExp;
    property FieldRef : TffSqlFieldRef read FFieldRef write FFieldRef;
    property Literal : TffSqlLiteral read FLiteral write FLiteral;
    property Param : TffSqlParam read FParam write FParam;
    property Aggregate : TffSqlAggregate read FAggregate write FAggregate;
    property SubQuery : TffSqlSELECT read FSubQuery write FSubQuery;
    property ScalarFunc : TffSqlScalarFunc read FScalarFunc write FScalarFunc;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function HasFieldRef: Boolean;
    function IsField(var FieldReferenced: TFFSqlFieldProxy): Boolean;
    function IsFieldFrom(Table: TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy; var SameCase: Boolean): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function IsNull: Boolean;
    function WasWildcard : Boolean;                                    {!!.11}
  end;

  TffSqlAddOp = (aoPlus, aoMinus, aoConcat);
  TffSqlTerm = class(TffSqlNode)
  protected
    TypeKnown : Boolean;
    FType : TffFieldType;
    FAddOp: TffSqlAddOp;
    FactorList : TList;
    FIsConstantChecked: Boolean;
    FIsConstant: Boolean;
    ConstantValue: Variant;
    procedure Clear;
    procedure CheckIsConstant;
    function IsConstant: Boolean;
    function GetFactor(Index: Integer): TffSqlFactor;
    procedure SetFactor(Index: Integer; const Value: TffSqlFactor);
    function GetFactorCount: Integer;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;
    function GetType: TffFieldType; override;
    procedure CheckType;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);
    function IsAggregate: Boolean; override;
    //function GetAgg: TffSqlAggregate; override;
    function AddIntervalTo(Target: TDateTime): TDateTime;
    function SubtractIntervalFrom(Target: TDateTime): TDateTime;
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddFactor(Factor: TffSqlFactor): TffSqlFactor;
    property FactorCount : Integer read GetFactorCount;
    property Factor[Index: Integer] : TffSqlFactor read GetFactor write SetFactor;
    property AddOp :TffSqlAddOp read FAddOp write FAddOp;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function HasFieldRef: Boolean;
    function IsField(var FieldReferenced: TFFSqlFieldProxy): Boolean;
    function IsFieldFrom(Table: TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy; var SameCase: Boolean): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function IsAggregateExpression: Boolean;
    function IsNull: Boolean;
    function WasWildcard : Boolean;                                    {!!.11}
  end;

  TffSqlSimpleExpression = class(TffSqlNode)
  protected
    TypeKnown : Boolean;
    FType : TffFieldType;
    BoundHaving : Boolean;
    BoundHavingField : TFFSqlFieldProxy;
    FIsConstant : Boolean;
    FIsConstantChecked: Boolean;
    ConstantValue: Variant;
    BindingHaving: Boolean;
    procedure BindHaving;
    procedure Clear;
    function ConcatBLOBValues(const Value1, Value2 : Variant) : Variant; {!!.13}
    function GetTerm(Index: Integer): TffSqlTerm;
    procedure SetTerm(Index: Integer; const Value: TffSqlTerm);
    function GetTermCount: Integer;
    function GetSize: Integer; override;
    function GetDecimals: Integer; override;
    function GetType: TffFieldType; override;
    procedure CheckType;
    function GetTitle(const Qualified : boolean): string;              {!!.11}
    procedure MatchType(ExpectedType: TffFieldType);
    function IsAggregate: Boolean; override;
    function IsConstant: Boolean;
    function IsParameter: Boolean;
    procedure CheckIsConstant;
    function Reduce: Boolean;
    procedure ResetConstant; override;
  protected
    TermList : TList;
  public
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Assign(const Source: TffSqlNode); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddTerm(Term : TffSqlTerm): TffSqlTerm;
    property TermCount : Integer read GetTermCount;
    property Term[Index: Integer] : TffSqlTerm read GetTerm write SetTerm;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetValue: Variant;
    function HasFieldRef: Boolean;
    function IsField(var FieldReferenced: TFFSqlFieldProxy): Boolean;
    function IsFieldFrom(Table: TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy; var SameCase: Boolean): Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function IsAggregateExpression: Boolean;
    function IsNull: Boolean;
    function WasWildcard : Boolean;                                    {!!.11}
  end;

  TffSqlSimpleExpressionList = class(TffSqlNode)
  protected
    FExpressionList : TList;
    FIsConstant: Boolean;
    FIsConstantChecked: Boolean;
    procedure CheckIsConstant;
    procedure Clear;
    function IsConstant: Boolean;
    function GetExpression(Index: Integer): TffSqlSimpleExpression;
    function GetExpressionCount: Integer;
    procedure SetExpression(Index: Integer;
      const Value: TffSqlSimpleExpression);
    procedure MatchType(ExpectedType: TffFieldType);
    function Contains(const TestValue: Variant): Boolean;
    function Reduce: Boolean;
    procedure ResetConstant; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddExpression(Expression: TffSqlSimpleExpression):
      TffSqlSimpleExpression;
    property ExpressionCount : Integer read GetExpressionCount;
    property Expression[Index: Integer] : TffSqlSimpleExpression
      read GetExpression write SetExpression;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
  end;

  TffSqlSelection = class(TffSqlNode)
  protected
    FColumn: TffSqlColumn;
    FSimpleExpression: TffSqlSimpleExpression;
    AddedByWildcard: Boolean;
    procedure AddColumnDef(Target: TffSqlColumnListOwner); override;
    function GetIndex: Integer;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    property SimpleExpression : TffSqlSimpleExpression
      read FSimpleExpression write FSimpleExpression;
    property Column : TffSqlColumn read FColumn write FColumn;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    property Index: Integer read GetIndex;
    function IsAggregateExpression: Boolean;
  end;

  TffSqlSelectionList = class(TffSqlNode)
  protected
    FSelections : TList;
    procedure Clear;
    function GetSelection(Index: Integer): TffSqlSelection;
    procedure SetSelection(Index: Integer;
      const Value: TffSqlSelection);
    function GetSelectionCount: Integer;
    function Reduce: Boolean;
//    procedure ResetConstant; override;
    function GetNonWildSelection(Index: Integer): TffSqlSelection;
    property NonWildSelection[Index: Integer]: TffSqlSelection
      read GetNonWildSelection;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddSelection(NewSelection: TffSqlSelection): TffSqlSelection;
    procedure InsertSelection(Index: Integer; NewSelection: TffSqlSelection);
    property SelectionCount : Integer read GetSelectionCount;
    property Selection[Index: Integer]: TffSqlSelection
      read GetSelection write SetSelection;
    function FindSelection(GroupCol : TffSqlGroupColumn): TffSqlSelection;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function NonWildSelectionCount: Integer;
  end;

  TffSqlInsertColumnList = class;

  TffSqlTableRef = class(TffSqlNode)
  protected
    FAlias : string;
    FTableName : string;
    FTableExp: TffSqlTableExp;
    FColumnList: TFFSqlInsertColumnList;
    FDatabaseName: string;
    FTable: TffSqlTableProxy;
    procedure AddTableReference(Select: TffSqlSELECT); override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function GetResultTable: TFFSqlTableProxy;
    function GetSQLName: string;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function BindTable(AOwner: TObject; const TableName: string): TFFSqlTableProxy;
    function Reduce: Boolean;                                          {!!.11}
    function TargetFieldFromSourceField(const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property TableName : string read FTableName write FTableName;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property Alias : string read FAlias write FAlias;
    property TableExp:  TffSqlTableExp read FTableExp write FTableExp;
    property ColumnList  :  TFFSqlInsertColumnList
      read FColumnList write FColumnList;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Clear;
    destructor Destroy; override;
    property SQLName: string read GetSQLName;
    function GetTable(AOwner: TObject; const ExclContLock : Boolean): TffSqlTableProxy;
    property ResultTable: TFFSqlTableProxy read GetResultTable;
  end;

  TffSqlTableRefList = class(TffSqlNode)
  protected
    FTableRefList : TList;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    procedure Clear;
    function GetTableRef(Index: Integer): TffSqlTableRef;
    procedure SetTableRef(Index: Integer;
      const Value: TffSqlTableRef);
    function GetTableRefCount: Integer;
    function Reduce: Boolean;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddTableRef(NewTableRef: TffSqlTableRef): TffSqlTableRef;
    function GetNameForAlias(const Alias : string) : string;
    property TableRefCount : Integer read GetTableRefCount;
    property TableRef[Index: Integer]: TffSqlTableRef
      read GetTableRef write SetTableRef;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function GetFieldsFromTable(const TableName: string; List: TList): TffSqlTableProxy;
  end;

  TffSqlOrderItem = class(TffSqlNode)
  protected
    FColumn: TFFSqlOrderColumn;
    FIndex: string;
    FDescending: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    property Column: TFFSqlOrderColumn read FColumn write FColumn;
    property Index: string read FIndex write FIndex;
    property Descending: Boolean read FDescending write FDescending;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlOrderList = class(TffSqlNode)
  protected
    FOrderItemList : TList;
    procedure Clear;
    function GetOrderItem(Index: Integer): TffSqlOrderItem;
    procedure SetOrderItem(Index: Integer;
      const Value: TffSqlOrderItem);
    function GetOrderCount: Integer;
    function Reduce: Boolean;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddOrderItem(NewOrder: TffSqlOrderItem): TffSqlOrderItem;
    property OrderCount : Integer read GetOrderCount;
    property OrderItem[Index: Integer]: TffSqlOrderItem
      read GetOrderItem write SetOrderItem;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlOuterJoinMode = (jmNone, jmLeft, jmRight, jmFull);
  TffSqlJoiner = class(TffObject)
  protected
    FSources            : TFFSqlTableProxySubsetList;
    FTargetTable        : TFFSqlTableProxy;
    Level               : integer;
    FRecordsRead        : Longint;
    {$IFDEF CountWrites}
    FRecordsWritten     : Longint;
    {$ENDIF}
    FieldCopier         : TffFieldCopier;
    FSX, FT : TList;
    FCondExpWhere: TffSqlCondExp;
    RecListL, RecListR,
    DupList : TffNRecordHash;
    FirstCondTerm, LastCondTerm : Boolean;
    OptimizeCalled: Boolean;
    WasOptimized: Boolean;
    P: procedure of object;
    FOwner: TffSqlStatement;
    procedure CreateResultRecord;
    function ProcessLevel(Cookie1: TffWord32): Boolean;
    procedure ReadSources;
    function FindRelation(Term: TffSqlCondTerm; CurFactor,
          CurFactor2: TffSqlCondFactor; Table : TFFSqlTableProxy;
          TargetField : TFFSqlFieldProxy;
      var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): TffSqlCondFactor;
    procedure Optimize(UseIndex: Boolean);
    function WriteNull(Cookie: TffWord32): Boolean;
  public
    constructor Create(AOwner: TffSqlStatement; CondExp: TffSqlCondExp);
    destructor Destroy; override;
    procedure Execute(UseIndex: Boolean; LoopProc: TFFObjectProc;
      OuterJoinMode: TffSqlOuterJoinMode);
    property Sources : TFFSqlTableProxySubsetList read FSources;
    procedure AddColumn(
     SourceExpression: TffSqlSimpleExpression;
     SourceField : TffSqlFieldProxy;
      Target: TFFSqlFieldProxy);
    procedure ClearColumnList;
    property RecordsRead : Longint read FRecordsRead;
    {$IFDEF CountWrites}
    property RecordsWritten: Longint read FRecordsWritten;
    {$ENDIF}
    property CondExpWhere : TffSqlCondExp read FCondExpWhere write FCondExpWhere;
    property Target : TFFSqlTableProxy read FTargetTable write FTargetTable;
    property Owner: TffSqlStatement read FOwner;
  end;

  TffSqlColumnListOwner = class(TffSqlNode)
  protected
    T : TffSqlTableProxy;                                              {!!.11}
    Columns : TStringList;
  public
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
  end;

  TffSqlJoinTableExp = class;
  TffSqlNonJoinTableExp = class;

  TffSqlTableExp = class(TffSqlNode)
  protected
    FJoinTableExp: TffSqlJoinTableExp;
    FNonJoinTableExp: TffSqlNonJoinTableExp;
    FNestedTableExp: TffSqlTableExp;
    procedure EnsureResultTable(NeedData: Boolean);
    function CheckNoDups: Boolean;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    function GetResultTable: TFFSqlTableProxy;
    property JoinTableExp: TffSqlJoinTableExp
      read FJoinTableExp write FJoinTableExp;
    property NonJoinTableExp: TffSqlNonJoinTableExp
      read FNonJoinTableExp write FNonJoinTableExp;
    property NestedTableExp:  TffSqlTableExp
      read FNestedTableExp write FNestedTableExp;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Clear;
    destructor Destroy; override;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function Reduce: Boolean;
    procedure EmitSQL(Stream : TStream); override;
    property ResultTable: TFFSqlTableProxy read GetResultTable;
    function GetFieldsFromTable(const TableName: string; List: TList):
      TffSqlTableProxy;                     {!!.11}
  end;

  TFFSqlUsingItem = class(TffSqlNode)
  protected
    FColumnName: string;
  public
    property ColumnName: string read FColumnName write FColumnName;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
  end;

  TffSqlUsingList = class(TffSqlNode)
  protected
    FUsingItemList : TList;
    procedure Clear;
    function GetUsingItem(Index: Integer): TffSqlUsingItem;
    procedure SetUsingItem(Index: Integer;
      const Value: TffSqlUsingItem);
    function GetUsingCount: Integer;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddItem(NewUsing: TffSqlUsingItem): TffSqlUsingItem;
    property UsingCount : Integer read GetUsingCount;
    property UsingItem[Index: Integer]: TffSqlUsingItem
      read GetUsingItem write SetUsingItem;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlJoinType = (jtCross, jtInner, jtLeftOuter, jtRightOuter,
    jtFullOuter, jtUnion);
  TffSqlJoinTableExp = class(TffSqlNode)
  protected
    FTableRef1: TffSqlTableRef;
    FTableRef2: TffSqlTableRef;
    FCondExp: TFFSqlCondExp;
    FJoinType: TffSqlJoinType;
    FNatural: Boolean;
    Bound: Boolean;
    TL, TR : TffSqlTableProxy;
    Columns: TStringList;
    Joiner : TffSqlJoiner;
    FUsingList: TFFSqlUsingList;
    UsingCondExp: TFFSqlCondExp;
    FResultTable : TFFSqlTableProxy;
    HaveData: Boolean;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    function GetResultTable: TffSqlTableProxy;
    function Execute2(NeedData: Boolean): TffSqlTableProxy;
    procedure Bind;
    procedure ClearBindings(Node: TffSqlNode);
    procedure ClearColumns;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function DoJoin(NeedData: Boolean): TffSqlTableProxy;
    function BuildSimpleFieldExpr(AOwner: TffSqlNode; const ATableName,
      AFieldName: string;
      AField: TffSqlFieldProxy): TffSqlSimpleExpression;
    procedure EnsureResultTable(NeedData: Boolean);
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    function BindField(const TableName,
      FieldName: string): TFFSqlFieldProxy; override;
    property JoinType: TffSqlJoinType read FJoinType write FJoinType;
    property Natural: Boolean read FNatural write FNatural;
    property TableRef1: TffSqlTableRef read FTableRef1 write FTableRef1;
    property TableRef2: TffSqlTableRef read FTableRef2 write FTableRef2;
    property CondExp: TFFSqlCondExp read FCondExp write FCondExp;
    property UsingList  :  TFFSqlUsingList read FUsingList write FUsingList;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Clear;
    destructor Destroy; override;
    procedure Execute(var aLiveResult: Boolean;
      var aCursorID: TffCursorID; var RecordsRead: Integer);
    function Reduce: Boolean;
    procedure EmitSQL(Stream : TStream); override;
    constructor Create(AParent: TffSqlNode);
    property ResultTable: TffSqlTableProxy read GetResultTable;
    function GetFieldsFromTable(const TableName: string; List: TList):
      TffSqlTableProxy;                     {!!.11}
  end;

  TffSqlValueList = class;

  TffSqlNonJoinTablePrimary = class(TffSqlNode)
  protected
    FSelectSt: TFFSqlSELECT;
    FValueList: TffSqlValueList;
    FNonJoinTableExp:  TffSqlNonJoinTableExp;
    FTableRef:  TffSqlTableRef;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    procedure EnsureResultTable(NeedData: Boolean);
    function GetResultTable: TffSqlTableProxy;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    destructor Destroy; override;
    property SelectSt:  TFFSqlSELECT read FSelectSt write FSelectSt;
    property ValueList:  TffSqlValueList read FValueList write FValueList;
    property NonJoinTableExp: TffSqlNonJoinTableExp
      read FNonJoinTableExp write FNonJoinTableExp;
    property TableRef: TffSqlTableRef read FTableRef write FTableRef;
    procedure Clear;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function Reduce: Boolean;
    procedure EmitSQL(Stream : TStream); override;
    property ResultTable: TffSqlTableProxy read GetResultTable;
  end;

  TffSqlNonJoinTableTerm = class(TffSqlNode)
  protected
    FNonJoinTablePrimary: TffSqlNonJoinTablePrimary;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    procedure EnsureResultTable(NeedData: Boolean);
    function GetResultTable: TffSqlTableProxy;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    property NonJoinTablePrimary:  TffSqlNonJoinTablePrimary
      read FNonJoinTablePrimary write FNonJoinTablePrimary;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Clear;
    destructor Destroy; override;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function Reduce: Boolean;
    procedure EmitSQL(Stream : TStream); override;
    property ResultTable: TffSqlTableProxy read GetResultTable;
  end;

  TffSqlNonJoinTableExp = class(TffSqlNode)
  protected
    FNonJoinTableTerm: TffSqlNonJoinTableTerm;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    function GetResultTable: TffSqlTableProxy;
    procedure EnsureResultTable(NeedData: Boolean);
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    function BindFieldDown(const TableName,
      FieldName: string): TFFSqlFieldProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
  public
    property NonJoinTableTerm:  TffSqlNonJoinTableTerm
      read FNonJoinTableTerm write FNonJoinTableTerm;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure Clear;
    destructor Destroy; override;
    procedure Execute(var aLiveResult: Boolean;
      var aCursorID: TffCursorID; var RecordsRead: Integer);
    function Reduce: Boolean;
    procedure EmitSQL(Stream : TStream); override;
    property ResultTable: TffSqlTableProxy read GetResultTable;
    function GetFieldsFromTable(const TableName: string; List: TList):
      TffSqlTableProxy;                     {!!.11}
  end;

  TffSqlValueItem = class(TffSqlNode)
  protected
    FDefault : Boolean;
    FSimplex: TffSqlSimpleExpression;
    function GetType: TffFieldType; override;
    function GetSize: Integer; override;
    function GetDecimals: Integer; override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    property Default : Boolean read FDefault write FDefault;
    property Simplex: TffSqlSimpleExpression read FSimplex write FSimplex;
  end;

  TffSqlValueList = class(TffSqlNode)
  protected
    FValueItemList : TList;
    FResultTable: TFFSqlTableProxy;
    procedure Clear;
    function GetValueItem(Index: Integer): TffSqlValueItem;
    procedure SetValueItem(Index: Integer;
      const Value: TffSqlValueItem);
    function GetValueCount: Integer;
    function GetResultTable: TFFSqlTableProxy;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddItem(NewValue: TffSqlValueItem): TffSqlValueItem;
    property ValueCount : Integer read GetValueCount;
    property ValueItem[Index: Integer]: TffSqlValueItem
      read GetValueItem write SetValueItem;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function Reduce: Boolean;
    property ResultTable: TFFSqlTableProxy read GetResultTable;
  end;

  TffSqlSELECT = class(TffSqlColumnListOwner)
  protected
    FDistinct : Boolean;
    FSelectionList : TffSqlSelectionList;
    FTableRefList : TffSqlTableRefList;
    FGroupColumnList : TffSqlGroupColumnList;
    FCondExpWhere: TffSqlCondExp;
    FCondExpHaving: TffSqlCondExp;
    FOrderList : TffSqlOrderList;
    FGrpTable : TffSqlTableProxy;
    AggList : TList;
    FResultTable : TFFSqlTableProxy;
    TablesReferencedByOrder : TStringList;
    TableAliases : TStringList;
    HaveAggregates : Boolean;
    AggQueryMode : TffSqlAggQueryMode;
    HavingTable: TffSqlTableProxy;
    IsDependent: Boolean;
    Bound: Boolean;
    Joiner : TffSqlJoiner;
    FInWhere: Boolean;
    WasStar: Boolean;
    HaveData: Boolean;
    RequestLive: Boolean;
    TypeKnown: Boolean;
    FType: TffFieldType;
    FDecimals: Integer;
    FSize: Integer;                                                    {!!.13}
    BindingDown: Boolean;                                              {!!.11}
    procedure AddTableFields(Table : TffSqlTableProxy;
                       const StartPoint : Integer;
                             FieldRef : TffSqlFieldRef);
    procedure AddTableFieldsFromList(Table : TffSqlTableProxy;
                                const StartPoint : Integer;
                                      FieldRef : TffSqlFieldRef;
                                      List: TList);                    {!!.11}
    procedure Bind;
    function BindTable(AOwner: TObject;
      const TableName: string): TFFSqlTableProxy;
    procedure AddTableRefs(Node: TffSqlNode);
    procedure AddColumns(Node: TffSqlNode);
    procedure BuildSortList(Table: TffSqlTableProxy;
      var SortList: TffSqlSortArray);                                  {!!.11}
    procedure DoGroupCopy(GroupColumnsIn: Integer; AggExpList,
      GroupColumnTargetField: TList);
    procedure DoAggOrderBy;
    procedure DoHaving;
    procedure DoSortOnAll;
    procedure DoRemoveDups(NeedData: Boolean);
    procedure DoBuildGroupingTable(GroupColumnsIn : Integer; FSF, FSX,
      GroupColumnTargetField: TList);
    procedure DoOrderBy(NeedData: Boolean; Table: TffSqlTableProxy);
    procedure DoCheckAggregates;
    function CheckAnyValue(RelOp: TffSqlRelOp;
      const Val: Variant): Boolean;
    function CheckAllValues(RelOp: TffSqlRelOp;
       const Val: Variant): Boolean;
    {procedure CheckTableList;} {!!.12 debug code}
    procedure Clear;
    procedure ClearBindings(Node: TffSqlNode);
    procedure ResetIsConstant(Node: TffSqlNode);
    procedure FlagAggregates(Node: TffSqlNode);
    procedure EnumAggregates(Node: TffSqlNode);
    function BindField(const TableName,
      FieldName: string): TFFSqlFieldProxy; override;
    function FindField(const FieldName: string): TFFSqlFieldProxy;
    procedure ExpandWildcards;
    procedure MatchType(ExpectedType: TffFieldType; AllowMultiple: Boolean);
    function NormalQueryResult(NeedData: Boolean): TffSqlTableProxy;
    function CheckForValue(Value: Variant):Boolean;
    function Match(Value: Variant; Unique: Boolean;
      MatchOption: TffSqlMatchOption): Boolean;
    function AggregateQueryResult(NeedData: Boolean): TffSqlTableProxy;
    function CheckHaving: Boolean;
    function Execute2(NeedData: Boolean): TffSqlTableProxy;
    procedure EnsureResultTable(NeedData: Boolean);
    procedure ClearTableList;
    function Reduce: Boolean;
    function GetValue: Variant;
    function CheckNonEmpty: Boolean;
    function IsSubQuery: Boolean;
    function GetType: TffFieldType; override;
    function GetDecimals: Integer; override;
    function GetSize: Integer; override;                               {!!.13}
    function GetResultTable: TFFSqlTableProxy;
    function TargetFieldFromSourceField(
      const F: TffSqlFieldProxy): TffSqlFieldProxy;
    function TableWithCount(const ColumnName: string): TffSqlTableProxy; {!!.12}
  public
    property InWhere: Boolean read FInWhere write FInWhere; //used only during parsing
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    property Distinct: Boolean read FDistinct write FDistinct;
    property SelectionList : TffSqlSelectionList
      read FSelectionList write FSelectionList;
    property TableRefList : TffSqlTableRefList
      read FTableRefList write FTableRefList;
    property CondExpWhere : TffSqlCondExp read FCondExpWhere write FCondExpWhere;
    property GroupColumnList : TffSqlGroupColumnList
      read FGroupColumnList write FGroupColumnList;
    property CondExpHaving : TffSqlCondExp
      read FCondExpHaving write FCondExpHaving;
    property OrderList : TffSqlOrderList read FOrderList write FOrderList;
    procedure EmitSQL(Stream : TStream); override;
    procedure Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
    function Equals(Other: TffSqlNode): Boolean; override;
    function DependsOn(Table: TFFSqlTableProxy): Boolean;
    property ResultTable : TFFSqlTableProxy read GetResultTable;
  end;

  TffSqlInsertItem = class(TffSqlNode)
  protected
    FColumnName: string;
    procedure AddColumnDef(Target: TffSqlColumnListOwner); override;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property ColumnName: string read FColumnName write FColumnName;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlInsertColumnList = class(TffSqlNode)
  protected
    FInsertColumnItemList : TList;
    procedure Clear;
    function GetInsertColumnItem(Index: Integer): TffSqlInsertItem;
    procedure SetInsertColumnItem(Index: Integer;
      const Value: TffSqlInsertItem);
    function GetInsertColumnCount: Integer;
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddItem(NewInsertColumn: TffSqlInsertItem): TffSqlInsertItem;
    property InsertColumnCount : Integer read GetInsertColumnCount;
    property InsertColumnItem[Index: Integer]: TffSqlInsertItem
      read GetInsertColumnItem write SetInsertColumnItem;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
  end;

  TffSqlINSERT = class(TffSqlColumnListOwner)
  protected
    FTableName: string;
    FInsertColumnList: TFFSqlInsertColumnList;
    FDefaultValues: Boolean;
    Bound: Boolean;
//    T : TffSqlTableProxy;                                            {!!.11}
    FTableExp: TffSqlTableExp;
    procedure AddColumns(Node: TffSqlNode);
    procedure Bind;
    procedure ClearBindings(Node: TffSqlNode);
    function Reduce: Boolean;                                          {!!.11}
  public
    destructor Destroy; override;
    property TableName : string read FTableName write FTableName;
    property InsertColumnList: TFFSqlInsertColumnList
      read FInsertColumnList write FInsertColumnList;
    property TableExp: TffSqlTableExp read FTableExp write FTableExp;
    property DefaultValues: Boolean read FDefaultValues write FDefaultValues;
    procedure Assign(const Source: TffSqlNode); override;
    procedure Clear;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function Execute(var RowsAffected: Integer) : TffResult;           {!!.11}
  end;

  TffSqlDELETE = class(TffSqlColumnListOwner)                          {!!.11}
  protected
    FTableRef: TffSqlTableRef;
    FCondExpWhere: TffSqlCondExp;
    Bound: Boolean;
//    T : TffSqlTableProxy;                                            {!!.11}
    Joiner : TffSqlJoiner;
    DeleteList: TList;
    procedure Bind;
    function BindField(const TableName,
      FieldName: string): TFFSqlFieldProxy; override;
    procedure DeleteRecord;
    function Reduce: Boolean;                                          {!!.11}
  public
    destructor Destroy; override;
    property TableRef:  TffSqlTableRef read FTableRef write FTableRef;
    property CondExpWhere : TffSqlCondExp
      read FCondExpWhere write FCondExpWhere;
    procedure Assign(const Source: TffSqlNode); override;
    procedure Clear;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function Execute(var RowsAffected: Integer) : TffResult;           {!!.11}
  end;

  TffSqlUpdateItem = class(TffSqlNode)
  protected
    FSimplex: TffSqlSimpleExpression;
    FColumnName: string;
    FDefault: Boolean;
    F: TffSqlFieldProxy;
    procedure AddColumnDef(Target: TffSqlColumnListOwner); override;
    function Reduce: Boolean;                                          {!!.11}
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    destructor Destroy; override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    property ColumnName: string read FColumnName write FColumnName;
    property Default: Boolean read FDefault write FDefault;
    property Simplex: TffSqlSimpleExpression read FSimplex write FSimplex;
    procedure Update;
  end;

  TffSqlUpdateList = class(TffSqlNode)
  protected
    FUpdateItemList : TList;
    procedure Clear;
    function GetUpdateItem(Index: Integer): TffSqlUpdateItem;
    function GetUpdateCount: Integer;
    function Reduce: Boolean;                                          {!!.11}
  public
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    constructor Create(AParent: TffSqlNode);
    destructor Destroy; override;
    function AddItem(NewValue: TffSqlUpdateItem): TffSqlUpdateItem;
    property UpdateCount : Integer read GetUpdateCount;
    property UpdateItem[Index: Integer]: TffSqlUpdateItem
      read GetUpdateItem;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function Update : TffResult;                                       {!!.11}
  end;

  TffSqlUPDATE = class(TffSqlColumnListOwner)
  protected
    FTableRef: TffSqlTableRef;
    FCondExpWhere: TffSqlCondExp;
    FUpdateList: TFFSqlUpdateList;
    Bound: Boolean;
//    T : TffSqlTableProxy;                                            {!!.11}
    Joiner : TffSqlJoiner;
    FRowsAffected: Integer;
    UpdateRecList: TList;
    procedure AddColumns(Node: TffSqlNode);
    procedure Bind;
    function BindField(const TableName,
      FieldName: string): TFFSqlFieldProxy; override;
    procedure ClearBindings(Node: TffSqlNode);
    function Reduce: Boolean;                                          {!!.11}
    procedure UpdateRecord;
  public
    destructor Destroy; override;
    property TableRef:  TffSqlTableRef read FTableRef write FTableRef;
    property CondExpWhere : TffSqlCondExp read FCondExpWhere write FCondExpWhere;
    property UpdateList: TFFSqlUpdateList read FUpdateList write FUpdateList;
    procedure Assign(const Source: TffSqlNode); override;
    procedure Clear;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    procedure EmitSQL(Stream : TStream); override;
    function Equals(Other: TffSqlNode): Boolean; override;
    function Execute(var RowsAffected: Integer) : TffResult;           {!!.11}
  end;

  TffSqlStatement = class(TffSqlNode)
  protected
    FClientID: TffClientID;
    FSessionID: TffSessionID;
    FInsert: TffSqlINSERT;
    StartDate,
    StartDateTime,
    StartTime : TDateTime;
    ParmCount : Integer;
    ParmList : TFFVariantList;
    FUseIndex: Boolean;
    FUpdate: TffSqlUPDATE;
    FDelete: TffSqlDELETE;
    FReduce: Boolean;
    FDatabase : TffSqlDatabaseProxy;
    RecordsRead: Integer;
    FTableExp: TffSqlTableExp;
  public
    property UseIndex: Boolean read FUseIndex write FUseIndex;
    property Reduce: Boolean read FReduce write FReduce;
    procedure Assign(const Source: TffSqlNode); override;
    procedure EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean); override;
    property Insert: TffSqlINSERT read FInsert write FInsert;
    property Update: TffSqlUPDATE read FUpdate write FUpdate;
    property Delete: TffSqlDELETE read FDelete write FDelete;
    property TableExp: TffSqlTableExp read FTableExp write FTableExp;
    constructor Create;
    destructor Destroy; override;
{Begin !!.11}
    procedure Bind(const ClientID: TffClientID;
                   const SessionID: TffSessionID;
                         Database : TffSqlDatabaseProxy);
{End !!.11}
    procedure EmitSQL(Stream : TStream); override;
     {- write the SQL statement represented by this hierarchy}
{Begin !!.11}
    function Execute(var aLiveResult: Boolean;
                     var aCursorID: TffCursorID;
                     var RowsAffected,
                         aRecordsRead: integer) : TffResult;
{End !!.11}
    function Equals(Other: TffSqlNode): Boolean; override;
    procedure SetParameter(Index: Integer; Value: Variant);
    procedure ReduceStrength;
    property Owner: TffSqlStatement read FOwner;
    procedure Clear;
  end;

  TffGroupColumnTargetInfo = class(TffObject)
    { This class helps correlate a selection field to a slot in the
      LastValues list that is created when grouping fields. There
      is not a one-to-one correspondence between the two lists
      because the Group By clause may reference fields not in the
      selection list. }
  public
    SelFldIndex,
    LastValueIndex : Longint;
  end;

{$IFDEF ExposeLastStatement}
var
  LastStatement : TffSqlStatement; {debug hook}
{$ENDIF}

implementation

uses
  ffllExcp,
  ffsrbase,
  ffsrbde,
  ffsrlock,
  Math;                                                                {!!.11}

{$I ffconst.inc}

var
  TimeDelta : double;

const
  RelOpStr : array[TffSqlRelOp] of string =
    ('', '=', '<=', '<', '>', '>=', '<>');
  DefStr : array[TffSqlIntervalDef] of string = (
  'Unspec', 'YEAR', 'MONTH', 'DAY', 'HOUR', 'MINUTE', 'SECOND');
  CanOptimizeOnOperator: array[TffSqlRelOp] of Boolean = (
    {roNone, roEQ, roLE, roL, roG, roGE, roNE}
     FALSE,  TRUE, TRUE, TRUE, TRUE, TRUE, FALSE);
  AgString : array[TffSqlAggFunction] of string =
    ('COUNT','MIN','MAX','SUM','AVG');
  ffSqlInConvThreshold = 8; {maximum length of expression list in
    an IN clause to convert to simple expressions}

function PosCh(const SearchCh: Char; const SearchString: string): Integer;
{-same as POS but searches for a single Char}
var
  Len: Integer;
begin
  Len := length(SearchString);
  if Len <> 0 then begin
    Result := 1;
    repeat
      if SearchString[Result] = SearchCh then
        exit;
      inc(Result);
    until Result > Len;
  end;
  Result := 0;
end;

function PosChI(const SearchCh: Char; const SearchString: string): Integer;
{-same as PosCh above, but ignores case}
var
  Len: Integer;
  SearchChU: Char;
begin
  Len := length(SearchString);
  if Len <> 0 then begin
    SearchChU := UpCase(SearchCh);
    Result := 1;
    repeat
      if SearchString[Result] = SearchCh then
        exit;
      if UpCase(SearchString[Result]) = SearchChU then
        exit;
      inc(Result);
    until Result > Len;
  end;
  Result := 0;
end;

function PosI(const SearchFor, SearchIn: string): Integer;
{-same as POS but ignores case on both strings}
var
  LenFor, LenIn, j: Integer;
  FirstCh: Char;
begin
  LenFor := length(SearchFor);
  if LenFor = 0 then begin
    Result := 0;
    exit;
  end;
  Result := PosChI(SearchFor[1], SearchIn);
  if (Result = 0) or (LenFor = 1) then
    exit;
  LenIn := length(SearchIn);
  if LenIn <> 0 then begin
    dec(LenIn, LenFor);
    FirstCh := UpCase(SearchFor[1]);
    repeat
      if UpCase(SearchIn[Result]) = FirstCh then begin
        J := 1;
        repeat
          inc(J);
        until (J > LenFor) or (UpCase(SearchIn[Result + J - 1]) <> UpCase(SearchFor[J]));
        if J > LenFor then
          exit;
      end;
      inc(Result);
    until Result > LenIn;
  end;
  Result := 0;
end;

{$IFNDEF DCC5OrLater}
function CompareText(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@0
        MOV     EAX,[EAX-4]
@@0:    OR      EDX,EDX
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
end;

function SameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
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
end;
{$ENDIF}

type
  TReadSourceEvent = procedure(Sender: TObject;
    var OkToCopy: boolean) of object;
  TEvaluateFieldEvent = procedure(Sender: TObject;
    ColumnIndex : Integer; var Res : variant) of object;

function CreateLiteralStringExp(Parent: TffSqlNode; const S: string): TffSqlSimpleExpression;
var
  T : TffSqlTerm;
  F : TffSqlFactor;
  L : TffSqlLiteral;
  SL : TffSqlStringLiteral;
begin
  Result := TffSqlSimpleExpression.Create(Parent);
  T := TffSqlTerm.Create(Result);
  F := TffSqlFactor.Create(T);
  L := TffSqlLiteral.Create(F);
  SL := TffSqlStringLiteral.Create(L);
  SL.Value := '''' + S + '''';
  L.StringLiteral := SL;
  F.Literal := L;
  T.AddFactor(F);
  Result.AddTerm(T);
end;

constructor TffSqlJoiner.Create(AOwner: TffSqlStatement;
  CondExp: TffSqlCondExp);
begin
  Assert(AOwner <> nil);
  inherited Create;
  FOwner := AOwner;
  FCondExpWhere := CondExp;
  FSources := TFFSqlTableProxySubsetList.Create(AOwner);
  FieldCopier := TffFieldCopier.Create;
  FSX := TList.Create;
  FT := TList.Create;
end;

destructor TffSqlJoiner.Destroy;
begin
  FieldCopier.Free;
  FSX.Free;
  FT.Free;
  FSources.Free;
  inherited Destroy;
end;

procedure TffSqlJoiner.AddColumn(
  SourceExpression: TffSqlSimpleExpression;
  SourceField : TffSqlFieldProxy;
  Target: TFFSqlFieldProxy);
begin
  Assert((SourceExpression = nil) or (SourceField = nil));
  if (SourceExpression = nil) and (SourceField = nil) then             {!!.13}
    FSX.Add(Pointer(1)) // flag - see CreateResultRecord               {!!.13}
  else                                                                 {!!.13}
    FSX.Add(SourceExpression);
  Target.IsTarget := True;
  if SourceField <> nil then begin
    FieldCopier.Add(SourceField, Target);
    Target.SrcField := SourceField;
  end
  else
    Target.SrcIndex := Pred(FSX.Count);
  FT.Add(Target);
end;

procedure TffSqlJoiner.ClearColumnList;
begin
  FSX.Clear;
  FT.Clear;
  FieldCopier.Free;
  FieldCopier := TffFieldCopier.Create;
end;

function TffSqlJoiner.ProcessLevel(Cookie1: TffWord32): Boolean;
begin
  inc(FRecordsRead);
  inc(Owner.RecordsRead);
  { Time to check for timeout? }
  if FRecordsRead mod 1000 = 0 then
    FFCheckRemainingTime;
  if Level > 0 then begin
    if (CondExpWhere = nil) or CondExpWhere.AsBooleanLevel(Level) then begin
      dec(Level);
      ReadSources;
      inc(Level);
    end;
  end else
    if (CondExpWhere = nil) or CondExpWhere.AsBoolean then
      P;
  Result := True; {continue}
end;

procedure TffSqlJoiner.CreateResultRecord;
var
  i : Integer;
  V : Variant;
begin
  if (DupList <> nil)
  and not FirstCondTerm
  and DupList.Exists then exit;
  FTargetTable.Insert;
  for i := 0 to pred(FTargetTable.FieldCount) do
    if FSX[i] <> nil then begin
      if Integer(FSX[i]) = 1 then                                      {!!.13}
        TFFSqlFieldProxy(Ft[i]).SetValue(1)                            {!!.13}
      else begin                                                       {!!.13}
        V := TFFSqlSimpleExpression(FSX[i]).GetValue;
        TFFSqlFieldProxy(Ft[i]).SetValue(V);
      end;                                                             {!!.13}
    end;
  FieldCopier.Execute;
  FTargetTable.Post;
  if (DupList <> nil)
  and not LastCondTerm then
    DupList.Add;
  {$IFDEF CountWrites}
  inc(FRecordsWritten);
  {$ENDIF}
  if assigned(RecListL) then
    if not RecListL.Exists then
      RecListL.Add;
  if assigned(RecListR) then
    if not RecListR.Exists then
      RecListR.Add;
end;

function TffSqlJoiner.WriteNull(Cookie: TffWord32): Boolean;
begin
  if not TffNRecordHash(Cookie).Exists then
    CreateResultRecord;
  Result := True; {continue}
end;

procedure TffSqlJoiner.ReadSources;
begin
  with Sources.Item[Level] do
    Iterate(ProcessLevel, 0);
end;

function TffSqlJoiner.FindRelation(
      Term: TffSqlCondTerm;
      CurFactor, CurFactor2: TffSqlCondFactor;
      Table : TFFSqlTableProxy;
      TargetField : TFFSqlFieldProxy;
      var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): TffSqlCondFactor;
var
  k, l : Integer;
  F : TFFSqlFieldProxy;
  DepFound : Boolean;
begin
  with Term do begin
    for k := 0 to pred(CondFactorCount) do
      if (CondFactor[k] <> CurFactor)
      and (CondFactor[k] <> CurFactor2)
      and not OrderedSources.RelationUsed(CondFactor[k]) then
        with CondFactor[k] do
          if IsRelationTo(Table,
            F, Operator, ArgExpression, SameCase)
                and CanOptimizeOnOperator[Operator] then begin
              if F = TargetField then begin
                {check that it doesn't depend on something we haven't seen
                 at this point}
                DepFound := False;

                for l := 0 to pred(OrderedSources.Count) do
                  if ArgExpression.DependsOn(OrderedSources.Item[l].Table) then begin
                    DepFound := True;
                    break;
                  end;

                if not DepFound then begin
                  Result := CondFactor[k];
                  exit;
                end;
              end;
            end;
  end;
  Result := nil;
end;

procedure TffSqlJoiner.Execute(UseIndex: Boolean; LoopProc: TFFObjectProc;
      OuterJoinMode: TffSqlOuterJoinMode);
var
  i : Integer;
begin
  FRecordsRead := 0;
  {$IFDEF CountWrites}
  FRecordsWritten := 0;
  {$ENDIF}

  if assigned(LoopProc) then
    P := LoopProc
  else
    P := CreateResultRecord;

  case OuterJoinMode of
  jmLeft, jmFull :
    begin
      Sources.Item[0].Outer := True;
      Sources.Item[0].Opposite := Sources.Item[1].Table;
      Sources.OuterJoin := True;
    end;
  jmRight :
    begin
      Sources.Item[1].Outer := True;
      Sources.Item[1].Opposite := Sources.Item[0].Table;
      Sources.OuterJoin := True;
    end;
  end;

  Optimize(UseIndex);

  if WasOptimized then begin

    if CondExpWhere.GetCondTermCount > 1 then begin
      DupList := TffNRecordHash.Create;
      for i := 0 to pred(Sources.Count) do
        Duplist.AddTable(Sources.Item[i].Table);
    end else
      DupList := nil;

    {process each term separately}
    FirstCondTerm := True;
    for i := 0 to pred(CondExpWhere.GetCondTermCount) do begin
      LastCondTerm := i = pred(CondExpWhere.GetCondTermCount);
      with CondExpWhere.CondTerm[i] do begin
        OrderedSources.OuterJoin := OuterJoinMode <> jmNone;
        OrderedSources.Join(CondExpWhere.CondTerm[i], P);
      end;
      FirstCondTerm := False;
    end;

    DupList.Free;
    DupList := nil;

    if OuterJoinMode = jmFull then begin
      Sources.Item[0].Outer := False;
      Sources.Item[1].Outer := True;
      Sources.Item[1].Opposite := Sources.Item[0].Table;

      OptimizeCalled := False;
      Optimize(UseIndex);

      if WasOptimized then begin

        if CondExpWhere.GetCondTermCount > 1 then begin
          DupList := TffNRecordHash.Create;
          for i := 0 to pred(Sources.Count) do
            Duplist.AddTable(Sources.Item[i].Table);
        end else
          DupList := nil;

        {process each term separately}
        FirstCondTerm := True;
        for i := 0 to pred(CondExpWhere.GetCondTermCount) do begin
          LastCondTerm := i = pred(CondExpWhere.GetCondTermCount);
          with CondExpWhere.CondTerm[i] do begin
            OrderedSources.OuterJoin := True;
            OrderedSources.SkipInner := True;
            OrderedSources.Join(CondExpWhere.CondTerm[i], P);
          end;
          FirstCondTerm := False;
        end;

        DupList.Free;
        DupList := nil;

      end else begin
        if CondExpWhere <> nil then
          CondExpWhere.SetLevelDep(Sources);
        Level := Sources.Count - 1;
        ReadSources;
      end;
      OptimizeCalled := False;

    end;

  end else begin
    case OuterJoinMode of
    jmLeft :
      begin
        RecListL := TffNRecordHash.Create;
        ReclistL.AddTable(Sources.Item[0].Table);
      end;
    jmRight :
      begin
        RecListR := TffNRecordHash.Create;
        ReclistR.AddTable(Sources.Item[1].Table);
      end;
    jmFull :
      begin
        RecListL := TffNRecordHash.Create;
        ReclistL.AddTable(Sources.Item[0].Table);
        RecListR := TffNRecordHash.Create;
        ReclistR.AddTable(Sources.Item[1].Table);
      end;
    end;
    if CondExpWhere <> nil then
      CondExpWhere.SetLevelDep(Sources);
    Level := Sources.Count - 1;
    ReadSources;
    case OuterJoinMode of
    jmLeft :
      begin
        Sources.Item[1].Table.NullRecord;
        Sources.Item[0].Table.Iterate(WriteNull, TffWord32(RecListL));
        RecListL.Free;
        RecListL := nil;
      end;
    jmRight :
      begin
        Sources.Item[0].Table.NullRecord;
        Sources.Item[1].Table.Iterate(WriteNull, TffWord32(RecListR));
        RecListR.Free;
        RecListR := nil;
      end;
    jmFull :
      begin
        Sources.Item[1].Table.NullRecord;
        Sources.Item[0].Table.Iterate(WriteNull, TffWord32(RecListL));
        Sources.Item[0].Table.NullRecord;
        Sources.Item[1].Table.Iterate(WriteNull, TffWord32(RecListR));
        RecListL.Free;
        RecListL := nil;
        RecListR.Free;
        RecListR := nil;
      end;
    end;
  end;
end;

function CompareRelations(const R1, R2: TFFSqlTableProxySubset): Boolean;
{ Returns True if R1 is 'better' than R2, e.g. it is likely to better
  limit the number of rows we have to read to produce a result}
var
  U1, U2: Boolean;
  I1, I2: Integer;
begin
  if R2 = nil then begin
    Result := True;
    exit;
  end;
  {$IFDEF LogIndexAnalysis}
  writeln(IALog, ' Comparing relations');
  writeln(IALog, '   Rel1:');
  writeln(IALog, '     Table name:',R1.Table.Name, ' (', R1.Table.Alias,')');
  writeln(IALog, '     Unique:',R1.UniqueValue);
  writeln(IALog, '     Closed segment:',R1.ClosedSegment);
  writeln(IALog, '     Equal key depth:',R1.EqualKeyDepth);
  writeln(IALog, '     Key depth:',R1.KeyDepth);
  writeln(IALog, '     Relation key is unique:',R1.KeyRelation.RelationKeyIsUnique);
  writeln(IALog, '     Relation key is case insensitive:',R1.KeyRelation.RelationKeyIsCaseInsensitive);
  writeln(IALog, '     Record count:',R1.Table.GetRecordCount);
  writeln(IALog, '       Expression:',R1.KeyRelation.CondF.SqlText);
  writeln(IALog, '   Rel2:');
  writeln(IALog, '     Table name:',R2.Table.Name, ' (', R2.Table.Alias,')');
  writeln(IALog, '     Unique:',R2.UniqueValue);
  writeln(IALog, '     Closed segment:',R2.ClosedSegment);
  writeln(IALog, '     Equal key depth:',R2.EqualKeyDepth);
  writeln(IALog, '     Key depth:',R2.KeyDepth);
  writeln(IALog, '     Relation key is unique:',R2.KeyRelation.RelationKeyIsUnique);
  writeln(IALog, '     Relation key is case insensitive:',R2.KeyRelation.RelationKeyIsCaseInsensitive);
  writeln(IALog, '     Record count:',R2.Table.GetRecordCount);
  writeln(IALog, '       Expression:',R2.KeyRelation.CondF.SqlText);
  {$ENDIF}
  U1 := R1.UniqueValue;
  U2 := R2.UniqueValue;
  if U1 then
    if not U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' 1 is unique but 2 is not');
      {$ENDIF}
      Result := True;
      exit;
    end else
  else
    if U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' 2 is unique but 1 is not');
      {$ENDIF}
      Result := False;
      exit;
    end;
  U1 := R1.ClosedSegment;
  U2 := R2.ClosedSegment;
  if U1 then
    if U2 then
      if R1.EqualKeyDepth > R2.EqualKeyDepth then begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' EqualKeyDepth(1) > EqualKeyDepth(2)');
        {$ENDIF}
        Result := True;
        exit;
      end else
      if R1.EqualKeyDepth < R2.EqualKeyDepth then begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' EqualKeyDepth(1) < EqualKeyDepth(2)');
        {$ENDIF}
        Result := False;
        exit;
      end else
      if R1.KeyDepth > R2.KeyDepth then begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' KeyDepth(1) > KeyDepth(2)');
        {$ENDIF}
        Result := True;
        exit;
      end else
      if R1.KeyDepth < R2.KeyDepth then begin
        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' KeyDepth(1) < KeyDepth(2)');
        {$ENDIF}
        Result := False;
        exit;
      end else
    else begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' Closed(1) and not Closed(2)');
      {$ENDIF}
      Result := True;
      exit;
    end
  else
    if U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not Closed(1) and Closed(2)');
      {$ENDIF}
      Result := False;
      exit;
    end;
  U1 := R1.KeyRelation.RelationKeyIsUnique;
  U2 := R2.KeyRelation.RelationKeyIsUnique;
  if U1 then
    if not U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' RelationKeyIsUnique(1) and not RelationKeyIsUnique(2)');
      {$ENDIF}
      Result := True;
      exit;
    end else
  else
    if U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not RelationKeyIsUnique(1) and RelationKeyIsUnique(2)');
      {$ENDIF}
      Result := False;
      exit;
    end;
  U1 := R1.KeyRelation.RelationKeyIsCaseInsensitive;
  U2 := R2.KeyRelation.RelationKeyIsCaseInsensitive;
  if U1 then
    if not U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' RelationKeyIsCaseInsensitive(1) and not RelationKeyIsCaseInsensitive(2)');
      {$ENDIF}
      Result := True;
      exit;
    end else
  else
    if U2 then begin
      {$IFDEF LogIndexAnalysis}
      writeln(IALog, ' not RelationKeyIsCaseInsensitive(1) and RelationKeyIsCaseInsensitive(2)');
      {$ENDIF}
      Result := False;
      exit;
    end;
  I1 := R1.Table.GetRecordCount;
  I2 := R2.Table.GetRecordCount;
  {$IFDEF LogIndexAnalysis}
  if I1 > I2 then
    writeln(IALog, ' RecordCount(1) > RecordCount(2)')
  else
    writeln(IALog, ' RecordCount(1) < RecordCount(2)');
  {$ENDIF}
  if I1 > I2 then
    Result := True
  else
    Result := False;
end;

function CompareKeyRelations(const K1, K2: TFFSqlKeyRelation): Boolean;
{ Returns True if K1 is 'better' than K2, e.g. it is likely to better
  limit the number of rows we have to read to produce a result}
var
  U1, U2: Boolean;

  function UniqueValue(const K: TFFSqlKeyRelation): Boolean;
  begin
    Result :=
     (K.RelationFieldCount = K.RelationKeyFieldCount)
     and (K.RelationOperators[K.RelationKeyFieldCount - 1] = roEQ);
  end;

  function ClosedSegment(const K: TFFSqlKeyRelation): Boolean;
  begin
    Result :=
      (K.RelationOperators[K.RelationFieldCount - 1] = roEQ) or
      (K.RelationOperatorB[K.RelationFieldCount - 1] <> roNone); {!!.11}
  end;

  function KeyDepth(const K: TFFSqlKeyRelation): Integer;
  begin
    Result := K.RelationFieldCount;
  end;

  function EqualKeyDepth(const K: TFFSqlKeyRelation): Integer;
  begin
    Result := 0;
    while (Result < K.RelationFieldCount)
      and (K.RelationOperators[Result] = roEQ) do
        inc(Result);
  end;

begin
  U1 := UniqueValue(K1);
  U2 := UniqueValue(K2);
  if U1 then
    if not U2 then begin
      Result := True;
      exit;
    end
  else
    if U2 then begin
      Result := False;
      exit;
    end;
  U1 := ClosedSegment(K1);
  U2 := ClosedSegment(K2);
  if U1 then
    if U2 then
      if EqualKeyDepth(K1) > EqualKeyDepth(K2) then begin
        Result := True;
        exit;
      end else
      if EqualKeyDepth(K1) < EqualKeyDepth(K2) then begin
        Result := False;
        exit;
      end else
      if KeyDepth(K1) > KeyDepth(K2) then begin
        Result := True;
        exit;
      end else
      if KeyDepth(K1) < KeyDepth(K2) then begin
        Result := False;
        exit;
      end else
    else begin
      Result := True;
      exit;
    end
  else
    if U2 then begin
      Result := False;
      exit;
    end;
  U1 := K1.RelationKeyIsUnique;
  U2 := K2.RelationKeyIsUnique;
  if U1 then
    if not U2 then begin
      Result := True;
      exit;
    end
  else
    if U2 then begin
      Result := False;
      exit;
    end;
  U1 := K1.RelationKeyIsCaseInsensitive;
  U2 := K2.RelationKeyIsCaseInsensitive;
  if U1 then
    if not U2 then begin
      Result := True;
      exit;
    end
  else
    if U2 then begin
      Result := False;
      exit;
    end;
  Result := False;
end;

{$IFDEF LogIndexAnalysis}
procedure ShowComparison(const K1, K2: TFFSqlKeyRelation);
var
  U1, U2: Boolean;

  function UniqueValue(const K: TFFSqlKeyRelation): Boolean;
  begin
    Result :=
     (K.RelationFieldCount = K.RelationKeyFieldCount)
     and (K.RelationOperators[K.RelationKeyFieldCount - 1] = roEQ);
  end;

  function ClosedSegment(const K: TFFSqlKeyRelation): Boolean;
  begin
    Result := (K.RelationOperators[K.RelationFieldCount - 1] = roEQ)
      or (K.RelationOperatorB[K.RelationFieldCount - 1] <> roNone); {!!.11}
  end;

  function KeyDepth(const K: TFFSqlKeyRelation): Integer;
  begin
    Result := K.RelationFieldCount;
  end;

  function EqualKeyDepth(const K: TFFSqlKeyRelation): Integer;
  begin
    Result := 0;
    while (Result < K.RelationFieldCount)
      and (K.RelationOperators[Result] = roEQ) do
        inc(Result);
  end;

begin
  U1 := UniqueValue(K1);
  U2 := UniqueValue(K2);
  if U1 then
    if not U2 then begin
      writeln(IALog,' New is unique value');
      exit;
    end
  else
    if U2 then begin
      raise Exception.Create('Internal error');
    end;
  U1 := ClosedSegment(K1);
  U2 := ClosedSegment(K2);
  if U1 then
    if U2 then
      if EqualKeyDepth(K1) > EqualKeyDepth(K2) then begin
        writeln(IALog,'New has deeper equal key');
        exit;
      end else
      if KeyDepth(K1) > KeyDepth(K2) then begin
        writeln(IALog,'New is deeper');
        exit;
      end else
      if KeyDepth(K1) < KeyDepth(K2) then begin
        raise Exception.Create('Internal error');
      end else
    else begin
      writeln(IALog, 'New is closed interval');
      exit;
    end
  else
    if U2 then begin
      raise Exception.Create('Internal error');
    end;
  U1 := K1.RelationKeyIsUnique;
  U2 := K2.RelationKeyIsUnique;
  if U1 then
    if not U2 then begin
      writeln(IALog, 'New has unique key');
      exit;
    end
  else
    if U2 then begin
      raise Exception.Create('Internal error');
    end;
  U1 := K1.RelationKeyIsCaseInsensitive;
  U2 := K2.RelationKeyIsCaseInsensitive;
  if U1 then
    if not U2 then begin
      writeln(IALog, 'New has case insensitive key');
      exit;
    end
  else
    if U2 then begin
      raise Exception.Create('Internal error');
    end;
  raise Exception.Create('Internal error');
end;
{$ENDIF}

procedure TffSqlJoiner.Optimize;
var
  IndexAsc : Boolean;
  RestSources : TFFSqlTableProxySubsetList;

  {$IFDEF LogIndexAnalysis}

  procedure DumpOrderedList(OrderedSources : TFFSqlTableProxySubsetList; const Title: string);
  var
    j, y: integer;
  begin
    writeln(IALog, Title);
    for j := 0 to pred(OrderedSources.Count) do begin
      write(IALog, OrderedSources.Item[j].Table.Name, ' (', OrderedSources.Item[j].Table.Alias, ')');
      if OrderedSources.Item[j].KeyRelation.CondF <> nil then begin
        write(IALog, ' relation fields: ',OrderedSources.Item[j].KeyRelation.RelationFieldCount);
        write(IALog, '(');
        for y := 0 to pred(OrderedSources.Item[j].KeyRelation.RelationFieldCount) do begin
          write(IALog, ' field:', OrderedSources.Item[j].KeyRelation.RelationFields[y].Name);
          write(IALog, ' argexp:',OrderedSources.Item[j].KeyRelation.ArgExpressions[y].SQLText);
          write(IALog, ' Operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperators[y]]);
          {!!.11 begin}
          if (OrderedSources.Item[j].KeyRelation.ArgExpressionB[y] <> nil)
          and (OrderedSources.Item[j].KeyRelation.RelationOperatorB[y] <> roNone)
          and (OrderedSources.Item[j].KeyRelation.RelationB[y] <> nil) then
              write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB[y].SQLText,
              ' operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB[y]]);
          {!!.11 end}
        end;
        write(IALog, ')');
        write(IALog, ' index:',OrderedSources.Item[j].KeyRelation.NativeKeyIndex{RelationKeyIndexNative});
        (* !!.11
        if (OrderedSources.Item[j].KeyRelation.ArgExpressionB <> nil)
        and (OrderedSources.Item[j].KeyRelation.RelationOperatorB <> roNone)
        and (OrderedSources.Item[j].KeyRelation.RelationB <> nil) then
            write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB.SQLText,
            ' operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB]);
        *)
        writeln(IALog);
      end else
        writeln(IALog, ' no relation');
    end;
  end;

  {$ENDIF}

  function FindRelations(CondTerm: TffSqlCondTerm;
    MoreThanOne: Boolean): Boolean;
  var
    l, j, k, y : Integer;
    Best, x : Integer;
    F, F2 : TFFSqlFieldProxy;
    IndexRefs : array[0..pred(ffcl_MaxIndexes)] of Integer;
    IgnoreCase: Boolean;
    IndexFields : array[0..pred(ffcl_MaxIndexFlds)] of Integer;
    IndxFldCnt : Integer;
    Found: Boolean;
    CF : TFFSqlCondFactor;
    CurIgnoreCase : Boolean;
    DepFound: Integer;
    BestRelation: TFFSqlTableProxySubset;
    BestKeyRelation, CurKeyRelation: TFFSqlKeyRelation;
    HaveKeyRelation: Boolean;
    SameCase: Boolean;

    {$IFDEF LogIndexAnalysis}
    procedure DumpBest;
    var
      i : Integer;
    begin
      with BestKeyRelation do begin
        writeln(IALog,'       condition:',CondF.SQLText);
        writeln(IALog,'       key:',NativeKeyIndex);
        writeln(IALog,'       Fields in key:',RelationKeyFieldCount);
        writeln(IALog,'       Fields:',RelationFieldCount);
        for i := 0 to pred(RelationFieldCount) do begin
          writeln(IALog, '        ',RelationFields[i].Name,' ',RelOpStr[RelationOperators[i]], ' ',
            ArgExpressions[i].SQLText);
          {!!.11 begin}
          if RelationOperatorB[i] <> roNone then
            writeln(IALog, '      Secondary relation:',
              RelOpStr[RelationOperatorB[i]], ' ',
                ArgExpressionB[i].SQLText);
          {!!.11 end}
        end;
        {!!.11 begin
        if RelationOperatorB <> roNone then
          writeln(IALog, '      Secondary relation on last key field:',
            RelOpStr[RelationOperatorB], ' ',
              ArgExpressionB.SQLText);
        !!.11 end}
      end;
    end;


  {$ENDIF}
  var
    z: Integer;
  begin
    Result := False;
    {CurKeyRelation.ArgExpressionB := nil;}                            {!!.11}
    for z := 0 to pred(ffcl_MaxIndexFlds) do begin                     {!!.11}
      CurKeyRelation.ArgExpressionB[z] := nil;                         {!!.11}
      CurKeyRelation.RelationOperatorB[z] := roNone;                   {!!.11}
    end;                                                               {!!.11}

    with CondTerm do
      repeat

        //KeyState := ksNone;
        //Depth := 0;

        for j := 0 to pred(RestSources.Count) do begin
          RestSources.Item[j].Relations := 0;
          {$IFDEF LogIndexAnalysis}
          writeln(IALog, '  looking for relations on ',
            RestSources.Item[j].Table.Name, ' (', RestSources.Item[j].Table.Alias,')');
          {$ENDIF}

          {we select among multiple keys as follows:}
          {if we find a unique key on the available field(s) we use that
           otherwise,
             we use the deepest key we can find, i.e. the key where the
             most segments can be satisfied.
             among keys with the same depth, we pick the ones with
               the tightest or the most relations, e.g.
                 = is better than >
                 > and < is better than only >
                 ties could be further settled based on the number of
                   key values in an index, but we don't currently do that}

          HaveKeyRelation := False;
          CurKeyRelation.RelationFieldCount := 0;

          for k := 0 to pred(CondFactorCount) do begin
            if not OrderedSources.RelationUsed(CondFactor[k]) then
              with CondFactor[k] do begin
                if IsRelationTo(RestSources.Item[j].Table,
                  F, CurKeyRelation.RelationOperators[0],
                    CurKeyRelation.ArgExpressions[0], SameCase)
                and CanOptimizeOnOperator[CurKeyRelation.
                  RelationOperators[0]] then begin

                  if RestSources.Item[j].Outer
                  and CurKeyRelation.ArgExpressions[0].DependsOn(
                    RestSources.Item[j].Opposite) then begin

                    {$IFDEF LogIndexAnalysis}
                    writeln(IALOG,'   ',CondFactor[k].SQLText,' is a relation to ',
                      RestSources.Item[j].Table.Name,' (',RestSources.Item[j].Table.Alias,'). Arg expression:', CurKeyRelation.ArgExpressions[0].SQLText);
                    writeln(IALOG,'   but using would violate the outer join, so we can''t use it. Skipped.');
                    {$ENDIF}

                  end else begin

                    {$IFDEF LogIndexAnalysis}
                    writeln(IALOG,'   ',CondFactor[k].SQLText,' is a relation to ',
                      RestSources.Item[j].Table.Name,' (',RestSources.Item[j].Table.Alias,'). Arg expression:',
                        CurKeyRelation.ArgExpressions[0].SQLText);
                    {$ENDIF}

                    CurKeyRelation.CondF := CondFactor[k];
                    {CurKeyRelation.RelationB := nil;}                 {!!.11}
                    for z := 0 to pred(ffcl_MaxIndexFlds) do begin     {!!.11}
                      CurKeyRelation.ArgExpressionB[z] := nil;         {!!.11}
                      CurKeyRelation.RelationOperatorB[z] := roNone;   {!!.11}
                    end;                                               {!!.11}

                    {Check that this relation does not depend on something
                     we can't determine at this level. For example, if we
                     have table1 at the deepest level, then table2 at the
                     next, we are looking for conditional expressions on
                     table2 that will limit the number of rows we need to
                     read but we can't use conditions whose other side
                     refer to anything in table1.}

                    {$IFDEF LogIndexAnalysis}
                    writeln(IALog, '   Checking dependencies on deeper tables for :' +
                      CurKeyRelation.ArgExpressions[0].SQLText);
                    {$ENDIF}

                    DepFound := -1;

                    for l := pred(OrderedSources.Count) downto 0 do
                      if CurKeyRelation.ArgExpressions[0].DependsOn(
                        OrderedSources.Item[l].Table) then begin
                        DepFound := l;
                        break;
                      end;

                    {$IFDEF LogIndexAnalysis}
                    if DepFound <> -1 then
                      writeln(IALog, '    Deeper dependency found:',
                        CurKeyRelation.ArgExpressions[0].SQLText,' : ',
                        OrderedSources.Item[l].Table.Name,' (',OrderedSources.Item[l].Table.Alias,')')
                    else
                      writeln(IALog, '    No deeper dependency found on ',
                        CurKeyRelation.ArgExpressions[0].SQLText);
                    {$ENDIF}

                    {Part of the expression opposite our field is from a table, which
                     has already been put in the list. We can still use this relation
                     by putting it below that other table *unless* something in the
                     existing list depends on us (the table we're looking at now)}

                    if (DepFound <> -1)
                    and OrderedSources.DependencyExists(RestSources.
                      Item[j].Table) then begin
                      {$IFDEF LogIndexAnalysis}
                        writeln(IALog, '    Can''t use this - something else depends on it');
                      {$ENDIF}
                      CurKeyRelation.CondF := nil;
                    end else begin
                      {$IFDEF LogIndexAnalysis}
                      writeln(IALog, '    Relation found:', SQLText);
                      writeln(IALog, '    field:',F.Name);
                      writeln(IALog, '    same case:', SameCase); {!!.10}
                      writeln(IALog, '    operator:', RelOpStr[CurKeyRelation.RelationOperators[0]]);
                      writeln(IALog, '    arg expression:', CurKeyRelation.ArgExpressions[0].SQLTExt);
                      writeln(IALog, '      looking for indexes on that field');
                      {$ENDIF}

                      x := RestSources.Item[j].Table.IndexesOnField(F,
                        not SameCase, IndexRefs);

                      CurKeyRelation.RelationFieldCount := 1;

                      {$IFDEF LogIndexAnalysis}
                      CurKeyRelation.RelationFields[0] := F;
                      {$ENDIF}

                      if x <> 0 then begin

                        case CurKeyRelation.RelationOperators[0] of
                        roEQ :
                          begin
                            for y := 0 to pred(x) do begin
                              RestSources.Item[j].Table.GetIndexProperties
                                (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                                 CurIgnoreCase, IndexAsc, IndxFldCnt,
                                 IndexFields);

                              CurKeyRelation.RelationFieldCount := 1;
                              CurKeyRelation.RelationKeyFieldCount := IndxFldCnt;
                              CurKeyRelation.RelationOperators[0] := roEQ;
                              CurKeyRelation.RelationOperatorB[0] := roNone; {!!.11}
                              CurKeyRelation.RelationKeyIsCaseInsensitive :=
                                CurIgnoreCase;                               {!!.11}
                              CurKeyRelation.RelationKeyIndexAsc := IndexAsc;
                              CurKeyRelation.NativeKeyIndex := IndexRefs[y];
                              CurKeyRelation.DepIndex := DepFound;

                              (* !!.11 actually, whether relation key is unique is irrelevant here
                              if CurKeyRelation.RelationKeyIsUnique then begin
                                if IndxFldCnt = 1 then begin
                                  IgnoreCase := CurIgnoreCase;
                                end else begin
                                  {Multi-segment key.
                                   See if we have other relations that satisfy
                                     the following fields in the key}
                                  CurKeyRelation.RelationFieldCount := 1;
                                  repeat
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
                                    if CF = nil then begin
                                      {No further fields found.
                                       We have a key, but not a unique one}
                                      IgnoreCase := CurIgnoreCase;
                                      break;
                                    end else begin
                                      {we have a relation on this key segment}
                                      {$IFDEF LogIndexAnalysis}
                                      CurKeyRelation.RelationFields[
                                        CurKeyRelation.RelationFieldCount] := F2;
                                      {$ENDIF}

                                      if CurKeyRelation.RelationOperators[
                                        CurKeyRelation.RelationFieldCount] = roEQ then begin
                                        {operator is = which means we can continue searching if
                                         there are more fields in the key. Otherwise, we have a full
                                         key}
                                        IgnoreCase := CurIgnoreCase;
                                      end else begin
                                        {Operator wasn't =, so we can't continue.
                                         We can use this field, though, as the last one}
                                        IgnoreCase := CurIgnoreCase;
                                        {See if we have a secondary expression to close the interval}
                                        CF := FindRelation(CondTerm, CondFactor[k],
                                          CF, RestSources.Item[j].Table, F2,
                                          CurKeyRelation.RelationOperatorB,
                                          CurKeyRelation.ArgExpressionB,
                                          CurKeyRelation.SameCaseB);
                                        if CF <> nil then begin
                                          {we do - record data and update key state}

                                          CurKeyRelation.RelationB := CF;
                                          IgnoreCase := CurIgnoreCase;

                                        end else begin
                                          CurKeyRelation.ArgExpressionB := nil;
                                          CurKeyRelation.RelationOperatorB := roNone;
                                        end;
                                        inc(CurKeyRelation.RelationFieldCount);
                                        break;
                                      end;
                                    end;
                                    inc(CurKeyRelation.RelationFieldCount);
                                  until CurKeyRelation.RelationFieldCount >=
                                    IndxFldCnt;
                                end;
                              end else begin {not a unique key}
                              *)
                                if IndxFldCnt = 1 then begin
                                  IgnoreCase := CurIgnoreCase;
                                end else begin
                                  {Multi-segment key.
                                   See if we have other relations that satisfy
                                     the following fields in the key}
                                  CurKeyRelation.RelationFieldCount := 1;
                                  repeat
                                    F2 := RestSources.Item[j].Table.
                                      Field(IndexFields[
                                        CurKeyRelation.RelationFieldCount]);
                                    CF := FindRelation(CondTerm, CondFactor[k],
                                      nil, RestSources.Item[j].Table, F2,
                                      CurKeyRelation.RelationOperators[
                                        CurKeyRelation.RelationFieldCount],
                                      CurKeyRelation.ArgExpressions[
                                        CurKeyRelation.RelationFieldCount],
                                      CurKeyRelation.SameCases[
                                        CurKeyRelation.RelationFieldCount]);
                                    if CF = nil then begin
                                      {No further fields found, but
                                       we have a key but not a full one}
                                      IgnoreCase := CurIgnoreCase;
                                      break;
                                    end else begin
                                      {we have a relation on this key segment}
                                      {$IFDEF LogIndexAnalysis}
                                      CurKeyRelation.RelationFields[CurKeyRelation.RelationFieldCount] := F2;
                                      {$ENDIF}

                                      if CurKeyRelation.RelationOperators[
                                        CurKeyRelation.RelationFieldCount] = roEQ then begin
                                        {operator is = which means we can continue searching if
                                         there are more fields in the key. Otherwise, we have a full
                                         key}
                                        IgnoreCase := CurIgnoreCase;
                                        CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone; {!!.11}
                                      end else begin
                                        {Operator wasn't =, so we can't continue.
                                         We can use this field, though, as the last one}
                                        IgnoreCase := CurIgnoreCase;
                                        {see if we have other relations on this same segment}
                                        CF := FindRelation(CondTerm, CondFactor[k],
                                          CF, RestSources.Item[j].Table,
                                          F2, CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount], {!!.11}
                                          CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount], {!!.11}
                                          CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount]); {!!.11}
                                        if CF <> nil then begin
                                          {we do - record data and update key state}

                                          CurKeyRelation.RelationB[CurKeyRelation.RelationFieldCount] := CF; {!!.11}
                                          IgnoreCase := CurIgnoreCase;

                                        end else begin
                                          CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount] := nil; {!!.11}
                                          CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone; {!!.11}
                                        end;
                                        inc(CurKeyRelation.RelationFieldCount);
                                        break;
                                      end;
                                    end;
                                    inc(CurKeyRelation.RelationFieldCount);
                                  until CurKeyRelation.RelationFieldCount >=
                                    IndxFldCnt;
                                end;
                              {end;}                                   {!!.11}
                              if HaveKeyRelation then
                                if CompareKeyRelations(CurKeyRelation, BestKeyRelation) then begin
                                  {$IFDEF LogIndexAnalysis}
                                  writeln(IALog,'      New best key relation');
                                  ShowComparison(CurKeyRelation, BestKeyrelation);
                                  {$ENDIF}
                                  BestKeyRelation := CurKeyRelation;
                                  {$IFDEF LogIndexAnalysis}
                                  DumpBest;
                                  {$ENDIF}
                                end else
                              else begin
                                BestKeyRelation := CurKeyRelation;
                                {$IFDEF LogIndexAnalysis}
                                writeln(IALog,'      initial key relation');
                                DumpBest;
                                {$ENDIF}
                                HaveKeyRelation := True;
                              end;
                            end;
                          end;
                        else {~ Op <> roEQ}
                          {non equal join operator}
                          for y := 0 to pred(x) do begin
                            RestSources.Item[j].Table.GetIndexProperties
                              (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                              IgnoreCase, IndexAsc, IndxFldCnt, IndexFields);

                            CurKeyRelation.RelationFieldCount := 1;
                            CurKeyRelation.RelationKeyFieldCount := IndxFldCnt;
                            CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount-1] := roNone; {!!.11}
                            CurKeyRelation.RelationKeyIsCaseInsensitive :=
                              CurIgnoreCase;                       {!!.11}
                            CurKeyRelation.RelationKeyIndexAsc := IndexAsc;
                            CurKeyRelation.NativeKeyIndex := IndexRefs[y];
                            CurKeyRelation.DepIndex := DepFound;

                            IgnoreCase := CurIgnoreCase;

                            {see if we have other relations on this same segment}
                            CF := FindRelation(CondTerm, CondFactor[k], nil,
                              RestSources.Item[j].Table, F, CurKeyRelation.
                                RelationOperatorB[CurKeyRelation.RelationFieldCount-1], {!!.11}
                              CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount-1], {!!.11}
                              CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount-1]); {!!.11}

                            if CF <> nil then begin
                              {we do - record data and update key state}

                              IgnoreCase := CurIgnoreCase;

                              CurKeyrelation.RelationB[CurKeyRelation.RelationFieldCount-1] := CF; {!!.11}

                              {!!.11 begin}
                              {check for more interval segments}
                              if (IndxFldCnt > 1)
                              and (CurKeyRelation.RelationOperators[0] in [roEQ, roGE, roLE])
                              and (CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount-1] in [roEQ, roGE, roLE]) then begin
                                {Multi-segment key.
                                 See if we have other relations that satisfy
                                   the following fields in the key}
                                repeat
                                  F2 := RestSources.Item[j].Table.
                                    Field(IndexFields[
                                      CurKeyRelation.RelationFieldCount]);
                                  CF := FindRelation(CondTerm, CondFactor[k],
                                    nil, RestSources.Item[j].Table, F2,
                                    CurKeyRelation.RelationOperators[
                                      CurKeyRelation.RelationFieldCount],
                                    CurKeyRelation.ArgExpressions[
                                      CurKeyRelation.RelationFieldCount],
                                    CurKeyRelation.SameCases[
                                      CurKeyRelation.RelationFieldCount]);
                                  if CF = nil then begin
                                    {No further fields found, but
                                     we have a key but not a full one}
                                    IgnoreCase := CurIgnoreCase;
                                    break;
                                  end else
                                  if CurKeyRelation.RelationOperators[
                                      CurKeyRelation.RelationFieldCount] in [roEQ, roGE, roLE] then
                                  begin
                                    {we have a relation on this key segment}
                                    {$IFDEF LogIndexAnalysis}
                                    CurKeyRelation.RelationFields[CurKeyRelation.RelationFieldCount] := F2;
                                    {$ENDIF}

                                    if CurKeyRelation.RelationOperators[
                                      CurKeyRelation.RelationFieldCount] = roEQ then begin
                                      {operator is = which means we can continue searching if
                                       there are more fields in the key. Otherwise, we have a full
                                       key}
                                      IgnoreCase := CurIgnoreCase;
                                      CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone; {!!.11}
                                    end else begin
                                      {Operator wasn't =}
                                      IgnoreCase := CurIgnoreCase;
                                      {see if we have other relations on this same segment}
                                      CF := FindRelation(CondTerm, CondFactor[k],
                                        CF, RestSources.Item[j].Table,
                                        F2, CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount], {!!.11}
                                        CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount], {!!.11}
                                        CurKeyRelation.SameCaseB[CurKeyRelation.RelationFieldCount]); {!!.11}
                                      if CF <> nil then begin
                                        if not (CurKeyRelation.RelationOperatorB[
                                          CurKeyRelation.RelationFieldCount] in [roEQ, roGE, roLE]) then
                                            break;
                                            
                                        {we do - record data and update key state}

                                        CurKeyRelation.RelationB[CurKeyRelation.RelationFieldCount] := CF; {!!.11}
                                        IgnoreCase := CurIgnoreCase;

                                      end else begin
                                        CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount] := nil; {!!.11}
                                        CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount] := roNone; {!!.11}
                                        inc(CurKeyRelation.RelationFieldCount);
                                        break;
                                      end;
                                    end;
                                  end;
                                  inc(CurKeyRelation.RelationFieldCount);
                                until CurKeyRelation.RelationFieldCount >=
                                  IndxFldCnt;
                              end;
                              {!!.11 end}
                            end else begin
                              CurKeyRelation.ArgExpressionB[CurKeyRelation.RelationFieldCount-1] := nil; {!!.11}
                              CurKeyRelation.RelationOperatorB[CurKeyRelation.RelationFieldCount-1] := roNone; {!!.11}
                            end;

                            if HaveKeyRelation then
                              if CompareKeyRelations(CurKeyRelation,
                                BestKeyRelation) then begin
                                {$IFDEF LogIndexAnalysis}
                                ShowComparison(CurKeyRelation, BestKeyrelation);
                                {$ENDIF}
                                BestKeyRelation := CurKeyRelation;
                                {$IFDEF LogIndexAnalysis}
                                writeln(IALog,'      new best key relation');
                                DumpBest;
                                {$ENDIF}
                              end else
                            else begin
                              BestKeyRelation := CurKeyRelation;
                              {$IFDEF LogIndexAnalysis}
                              writeln(IALog,'      initial key relation');
                              DumpBest;
                              {$ENDIF}
                              HaveKeyRelation := True;
                            end;

                          end;
                        end;

                        {$IFDEF LogIndexAnalysis}
                        writeln(IALog, '        ', x, ' found!');
                        for y := 0 to pred(x) do begin
                          RestSources.Item[j].Table.GetIndexProperties
                            (IndexRefs[y], CurKeyRelation.RelationKeyIsUnique,
                             IgnoreCase, IndexAsc, IndxFldCnt, IndexFields);
                          writeln(IALog, '          key', y, ': ',
                                  '      Unique:', CurKeyRelation.RelationKeyIsUnique,
                                  '      IgnoreCase:', IgnoreCase,
                                  '      IndexAsc:', IndexAsc,
                                  '      Segments:',IndxFldCnt);
                          if IndxFldCnt <> 0 then begin
                            write(IALog, '           (');
                            for z := 0 to pred(IndxFldCnt) do begin
                              write(IALog, RestSources.Item[j].Table.
                                Field(IndexFields[z]).Name,' ');
                            end;
                            writeln(IALog, ')');
                          end;
                        end;
                        {$ENDIF}

                        inc(RestSources.Item[j].Relations);

                      end else
                        {$IFDEF LogIndexAnalysis}
                        writeln(IALog, '        none found');
                        {$ENDIF}
                    end;
                  end;
                end;
              end;
          end;

          if HaveKeyRelation then
            RestSources.Item[j].KeyRelation := BestKeyRelation;
        end;

        Found := False;
        Best := -1;

        {$IFDEF LogIndexAnalysis}
        writeln(IALog, ' Comparing relations');
        {$ENDIF}

        BestRelation := nil;
        for j := 0 to pred(RestSources.Count) do begin
          if (not MoreThanOne and (RestSources.Item[j].Relations = 1))
            or (MoreThanOne and (RestSources.Item[j].Relations > 0)) then begin
            {$IFDEF LogIndexAnalysis}
            writeln(IALog, ' ', RestSources.Item[j].Table.Name,' (',
              RestSources.Item[j].Table.Alias,') relations:',
                RestSources.Item[j].Relations);
            {$ENDIF}
            if CompareRelations(RestSources.Item[j], BestRelation) then begin
              BestRelation := RestSources.Item[j];
              Best := j;
            end;

          end;
        end;

        if BestRelation <> nil then begin

          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' Best:', BestRelation.Table.Name,' (',BestRelation.Table.Alias,')');
          {$ENDIF}
          if BestRelation.KeyRelation.DepIndex = -1 then
            OrderedSources.Add(RestSources.Item[Best])
          else
            OrderedSources.Insert(RestSources.Item[Best]);
          RestSources.Delete(Best);
          Found := True;
          {$IFDEF LogIndexAnalysis}
          DumpOrderedList(OrderedSources, ' Ordered list so far(inner to outer):');
          {$ENDIF}
          Result := True;
        end;

      until not Found;
  end;

var
  i, j : Integer;
  {$IFDEF LogIndexAnalysis}
  y : Integer;
  {$ENDIF}
begin
  if OptimizeCalled then exit;

  WasOptimized := False;

  if (CondExpWhere <> nil) and UseIndex then begin

    {$IFDEF LogIndexAnalysis}
    AssignFile(IALog, IALogFile);
    {$I-}
    Append(IALog);
    if IOResult <> 0 then
      Rewrite(IALog);
    writeln(IALog);
    writeln(IALog, 'Analyzing ' + CondExpWhere.Owner.SQLText);
    writeln(IALog, 'Analysis started at :',DateTimeToStr(Now));
    {$ENDIF}

    {look for relations that might be used for optimizing the join}

    {$IFDEF LogIndexAnalysis}
    writeln(IALog, 'Scanning for relations');
    {$ENDIF}

    for i := 0 to pred(CondExpWhere.GetCondTermCount) do begin

      {process each term separately}
      with CondExpWhere.CondTerm[i] do begin

        {$IFDEF LogIndexAnalysis}
        writeln(IALog, 'Term ', i, ' : ',SQLText);
        {$ENDIF}

        OrderedSources.Free;

        OrderedSources := TFFSqlTableProxySubsetList.Create(Owner);
        RestSources := TFFSqlTableProxySubsetList.Create(Owner);
        try
          {We build an ordered list of tables to process so that
           the inner-most table in the list is first.}

          {Specifically, we do this by looking for key relations
           which will limit the number of rows we need to read from
           each table.}

          {RestSources are the tables at any time which have not
           yet been selected for processing.
           When RestSources.Count = 0, we're done.}

          RestSources.Assign(Sources);

          {First, find and process the relations with
            exactly one key resolution.}

          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' Looking for relations with exactly one resolution');
          {$ENDIF}

          if FindRelations(CondExpWhere.CondTerm[i], False) then
            WasOptimized := True;

          {$IFDEF LogIndexAnalysis}
          DumpOrderedList(OrderedSources, 'Final ordered list (inner to outer):');
          {$ENDIF}

          {Then, find and process the relations with
            more than one key resolution, if any.}

          {$IFDEF LogIndexAnalysis}
          writeln(IALog, ' Looking for relations with more than one resolution');
          {$ENDIF}

          if FindRelations(CondExpWhere.CondTerm[i], True) then
            WasOptimized := True;

          {Finally, add the sources with no key relations - if any}

          for j := pred(RestSources.Count) downto 0 do begin
            RestSources.Item[j].KeyRelation.CondF := nil;
            OrderedSources.Add(RestSources.Item[j]);
            RestSources.Delete(j);
          end;

          Assert(RestSources.Count = 0);

          {done re-ordering}

          {$IFDEF LogIndexAnalysis}
          writeln(IALog, 'Ordered list (inner to outer):');
          for j := 0 to pred(OrderedSources.Count) do begin
            write(IALog, OrderedSources.Item[j].Table.Name,' (',OrderedSources.Item[j].Table.Alias,')');
            if OrderedSources.Item[j].KeyRelation.CondF <> nil then begin
              write(IALog, ' relation fields: ',OrderedSources.Item[j].KeyRelation.RelationFieldCount);
              write(IALog, '(');
              for y := 0 to pred(OrderedSources.Item[j].KeyRelation.RelationFieldCount) do begin
                write(IALog, ' field:', OrderedSources.Item[j].KeyRelation.RelationFields[y].Name);
                write(IALog, ' argexp:',OrderedSources.Item[j].KeyRelation.ArgExpressions[y].SQLText);
                write(IALog, ' Operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperators[y]]);
                {!!.11 begin}
                if (OrderedSources.Item[j].KeyRelation.ArgExpressionB[y] <> nil)
                and (OrderedSources.Item[j].KeyRelation.RelationOperatorB[y] <> roNone)
                and (OrderedSources.Item[j].KeyRelation.RelationB[y] <> nil) then
                    write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB[y].SQLText,
                    ' operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB[y]]);
                {!!.11 end}
              end;
              write(IALog, ')');
              write(IALog, ' index:',OrderedSources.Item[j].KeyRelation.NativeKeyIndex);
              {!!.11 begin
              if (OrderedSources.Item[j].KeyRelation.ArgExpressionB <> nil)
              and (OrderedSources.Item[j].KeyRelation.RelationOperatorB <> roNone)
              and (OrderedSources.Item[j].KeyRelation.RelationB <> nil) then
                  write(IALog, 'secondary expression:',OrderedSources.Item[j].KeyRelation.ArgExpressionB.SQLText,
                  ' operator:',RelOpStr[OrderedSources.Item[j].KeyRelation.RelationOperatorB]);
              !!.11 end}
              writeln(IALog);
            end else
              writeln(IALog, ' no relation');
          end;
          {$ENDIF}
        finally
          RestSources.Free;
        end;

      end;

    end;

    {$IFDEF LogIndexAnalysis}
    writeln(IALog);
    writeln(IALog, 'Analysis ended at :',DateTimeToStr(Now));
    CloseFile(IALog);
    {$ENDIF}

  end;

  OptimizeCalled := True;
end;

{===Utility routines=================================================}
function BothNil(O1, O2: TffSqlNode): Boolean;
begin
  Result := (O1 = nil) and (O2 = nil);
end;
{--------}
function BothNonNil(O1, O2: TffSqlNode): Boolean;
begin
  Result := (O1 <> nil) and (O2 <> nil);
end;
{====================================================================}

{===TffSqlNode=======================================================}
{--------}
procedure TffSqlNode.AddAggregate(Target: TList);
begin
end;
{--------}
procedure TffSqlNode.FlagAggregate;
begin
end;
{--------}
function TffSqlNode.GetDecimals: Integer;
begin
  raise Exception.CreateFmt('Internal error:GetDecimals not implemented for %s',
    [ClassName]);
end;
{--------}
function TffSqlNode.GetSize: Integer;
begin
  Result := 0;
end;
{--------}
function TffSqlNode.GetType: TffFieldType;
begin
  raise Exception.CreateFmt('Internal error:GetType not implemented for %s',
    [ClassName]);
end;
{--------}
function TffSqlNode.IsAggregate: Boolean;
begin
  raise Exception.CreateFmt('Internal error:IsAggregate not implemented for %s',
    [ClassName]);
end;
{--------}
function TffSqlNode.GetOwner: TffSqlStatement;
begin
  if (FOwner = nil)
  and not (Self is TffSqlStatement) then begin
    Assert(Parent <> nil);
    FOwner := TffSqlStatement(Parent);
    while FOwner.Parent <> nil do
      FOwner := TffSqlStatement(FOwner.Parent);
    Assert(Owner is TffSqlStatement);
  end;
  Result := FOwner;
end;
{--------}
{Begin !!.11}
function TffSqlNode.GetOwnerStmt: TFFSqlColumnListOwner;
begin
  if (FOwnerStmt = nil) then begin
    FOwnerStmt := TFFSqlColumnListOwner(Self);
    while (FOwnerStmt <> nil)
    and not (TObject(FOwnerStmt) is TFFSqlColumnListOwner) do
      FOwnerStmt := TFFSqlColumnListOwner(FOwnerStmt.Parent);
    if not (TObject(FOwnerStmt) is TFFSqlColumnListOwner) then
      FOwnerStmt := nil;
  end;
  Result := FOwnerStmt;
end;
{--------}
function TffSqlNode.GetOwnerSelect: TFFSqlSelect;
begin
  if (FOwnerStmt = nil) then begin
    FOwnerStmt := TFFSqlSelect(Self);
    while (FOwnerStmt <> nil)
    and not (TObject(FOwnerStmt) is TFFSqlSelect) do
      FOwnerStmt := TFFSqlSelect(FOwnerStmt.Parent);
    if not (TObject(FOwnerStmt) is TFFSqlSelect) then
      FOwnerStmt := nil;
  end;
  Result := TffSqlSelect(FOwnerStmt);
end;
{End !!.11}
{--------}
procedure TffSqlNode.TypeMismatch;
begin
  SQLError('Type mismatch');
end;
{--------}
procedure TffSqlNode.WriteEOF(Stream: TStream);
const
  NullChar : Char = #0;
begin
  Stream.Write(NullChar, 1);
end;
{--------}
procedure TffSqlNode.WriteStr(Stream: TStream; const S: string);
begin
  if S <> '' then
    Stream.Write(S[1], length(S));
end;
{--------}
procedure TffSqlNode.AddTableReference;
begin
end;
{--------}
procedure TffSqlNode.AddColumnDef;
begin
end;
{--------}
procedure TffSqlNode.AssignError(Source: TffSqlNode);
begin
  raise Exception.Create(Source.ClassName + ' not compatible with ' + ClassName);
end;
{--------}
function TffSqlNode.BindField(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  if Parent <> nil then
    Result := Parent.BindField(TableName, FieldName)
  else
    raise Exception.CreateFmt('No node could resolve the field %s.%s', {!!.11}
      [TableName, FieldName]);                                         {!!.11}
end;
{--------}
procedure TffSqlNode.ClearBinding;
begin
end;
{--------}
function TffSqlNode.IsAncestor(const Node : TffSqlNode) : Boolean;
var
  aParent : TffSqlNode;
begin
  aParent := FParent;
  repeat
    Result := (aParent = Node);
    aParent := aParent.Parent;
  until Result or (aParent = nil);
end;
{--------}
procedure TffSqlNode.ResetConstant;
begin
end;
{--------}
function TffSqlNode.SQLText: string;
var
  M : TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    EmitSQL(M);
    SetLength(Result, M.Size);
    M.Seek(0, 0);
    M.Read(Result[1], M.Size);
  finally
    M.Free;
  end;
end;
{--------}
procedure TffSqlNode.SQLError(const ErrorMsg: string);
begin
  raise Exception.CreateFmt('Error in statement: %s', [ErrorMsg]);
end;
{--------}
constructor TffSqlNode.Create(AParent: TffSqlNode);
begin
  inherited Create;
  FParent := AParent;
end;
{--------}
procedure TffSqlNode.EmitSQL(Stream: TStream);
begin
  raise Exception.CreateFmt('Internal error:EmitSQL not implemented for %s',
    [ClassName]);
end;
{====================================================================}

{===TffSqlSelectionList==============================================}
function TffSqlSelectionList.AddSelection(
  NewSelection: TffSqlSelection): TffSqlSelection;
begin
  FSelections.Add(NewSelection);
  Result := NewSelection;
end;
{--------}
procedure TffSqlSelectionList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlSelectionList then begin
    Clear;
    for i := 0 to pred(TffSqlSelectionList(Source).SelectionCount) do
      AddSelection(TffSqlSelection.Create(Self)).Assign(
        TffSqlSelectionList(Source).Selection[i]);
  end else
    AssignError(Source);
end;

{--------}
constructor TffSqlSelectionList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FSelections := TList.Create;
end;
{--------}
procedure TffSqlSelectionList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(SelectionCount) do
    Selection[i].Free;
  FSelections.Clear;
end;

{--------}
destructor TffSqlSelectionList.Destroy;
begin
  Clear;
  FSelections.Free;
  inherited;
end;
{--------}
procedure TffSqlSelectionList.EmitSQL(Stream: TStream);
var
  i : Integer;
  First: Boolean;
begin
  if SelectionCount > 0 then begin
    First := True;
    for i := 0 to pred(SelectionCount) do begin
      if not First then
        WriteStr(Stream, ', ');
      if not Selection[i].AddedByWildcard then begin
        Selection[i].EmitSQL(Stream);
        First := False;
      end;
    end;
  end;
end;
{--------}
procedure TffSqlSelectionList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i : Integer;
begin
  Assert(TObject(Self) is TffSqlSelectionList);
  EnumMethod(Self);
  for i := 0 to pred(SelectionCount) do
    Selection[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlSelectionList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlSelectionList then begin
    if NonWildSelectionCount <> TffSqlSelectionList(Other).NonWildSelectionCount then
      exit;
    for i := 0 to pred(NonWildSelectionCount) do
      if not NonWildSelection[i].Equals(TffSqlSelectionList(Other).
        NonWildSelection[i]) then
          exit;
    Result := True;
  end;
end;
{--------}
function TffSqlSelectionList.FindSelection(GroupCol :
  TffSqlGroupColumn) : TffSqlSelection;
var
  i : Integer;
  F : TffSqlFieldProxy;
  Name : string;
begin
  Name := GroupCol.QualColumnName;

  for i := 0 to pred(SelectionCount) do
    if Assigned(Selection[i].SimpleExpression.Term[0].Factor[0].FieldRef) and
       (AnsiCompareText(Trim(Selection[i].SimpleExpression.Term[0].Factor[0].
         FieldRef.QualName), Name) = 0) then begin
      Result := Selection[i];
      exit;
    end else
    if AnsiCompareText(Trim(Selection[i].SQLText), Name) = 0 then begin
      Result := Selection[i];
      exit;
    end else
    if Selection[i].Column <> nil then
      if AnsiCompareText(Selection[i].Column.ColumnName, Name) = 0 then begin
        Result := Selection[i];
        exit;
      end else
    else
    if Selection[i].SimpleExpression.IsField(F) then
      if (AnsiCompareText(F.Name, Name) = 0) or
         (AnsiCompareText(F.QualName, Name) = 0) then begin
        Result := Selection[i];
        exit;
      end;
  Result := nil;
end;
{--------}
function TffSqlSelectionList.GetNonWildSelection(
  Index: Integer): TffSqlSelection;
var
  i: Integer;
begin
  for i := 0 to pred(SelectionCount) do
    if not Selection[i].AddedByWildcard then begin
      dec(Index);
      if Index < 0 then begin
        Result := Selection[i];
        exit;
      end;
    end;
  Result := nil;
end;
{--------}
function TffSqlSelectionList.GetSelection(
  Index: Integer): TffSqlSelection;
begin
  Result := TffSqlSelection(FSelections[Index]);
  Assert(TObject(Result) is TffSqlSelection);
end;
{--------}
function TffSqlSelectionList.GetSelectionCount: Integer;
begin
  Result := FSelections.Count;
end;
{--------}
procedure TffSqlSelectionList.InsertSelection(Index: Integer;
  NewSelection: TffSqlSelection);
begin
  FSelections.Insert(Index, NewSelection);
end;
{--------}
function TffSqlSelectionList.NonWildSelectionCount: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to pred(SelectionCount) do
    if not Selection[i].AddedByWildcard then
      inc(Result);
end;

function TffSqlSelectionList.Reduce: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to pred(SelectionCount) do
    Result := Result or Selection[i].Reduce;
end;

procedure TffSqlSelectionList.SetSelection(Index: Integer;
  const Value: TffSqlSelection);
begin
  FSelections[Index] := Value;
end;
{====================================================================}

{===TffSqlSimpleExpression===========================================}
function TffSqlSimpleExpression.AddTerm(Term: TffSqlTerm): TffSqlTerm;
begin
  TermList.Add(Term);
  Result := Term;
end;
{--------}
procedure TffSqlSimpleExpression.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlSimpleExpression then begin
    Clear;
    for i := 0 to pred(TffSqlSimpleExpression(Source).TermCount) do begin
      AddTerm(TffSqlTerm.Create(Self)).Assign(
        TffSqlSimpleExpression(Source).Term[i]);
    end;
  end else
    AssignError(Source);
end;
{--------}
constructor TffSqlSimpleExpression.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  TermList := TList.Create;
end;
{--------}
procedure TffSqlSimpleExpression.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    Term[i].Free;
  TermList.Clear;
  inherited;
end;
{--------}
{Begin !!.13}
function TffSqlSimpleExpression.ConcatBLOBValues(const Value1, Value2 : Variant) : Variant;
var
  VPtr1, VPtr2 : PAnsiChar;
  VStr1, VStr2 : string;
  VLen1, VLen2 : Integer;
  VPtrResult : PAnsiChar;
begin
  try
    if VarType(Value1) and VarTypeMask = varByte then begin
      VPtr1 := VarArrayLock(Value1);
      VStr1 := '';
      VLen1 := VarArrayHighBound(Value1, 1);
    end
    else begin
      VStr1 := VarToStr(Value1);
      VPtr1 := PAnsiChar(VStr1);
      VLen1 := Length(VStr1);
    end;

    if VarType(Value2) and VarTypeMask = varByte then begin
      VPtr2 := VarArrayLock(Value2);
      VStr2 := '';
      VLen2 := VarArrayHighBound(Value2, 1);
    end
    else begin
      VStr2 := VarToStr(Value2);
      VPtr2 := PAnsiChar(VStr2);
      VLen2 := Length(VStr2);
    end;

    { Assumption: The result may always be returned as a BLOB value. }
    Result := VarArrayCreate([1, VLen1 + VLen2], varByte);
    VPtrResult := VarArrayLock(Result);
    try
      Move(VPtr1^, VPtrResult^, VLen1);
      inc(VPtrResult, VLen1);
      Move(VPtr2^, VPtrResult^, VLen2);
    finally
      VarArrayUnlock(Result);
    end;

  finally
    if VStr1 = '' then
      VarArrayUnlock(Value1);
    if VStr2 = '' then
      VarArrayUnlock(Value2);
  end;
end;
{End !!.13}
{--------}
function TffSqlSimpleExpression.DependsOn(
  Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    if Term[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
destructor TffSqlSimpleExpression.Destroy;
begin
  Clear;
  TermList.Free;
  inherited;
end;
{--------}
procedure TffSqlSimpleExpression.EmitSQL(Stream: TStream);
const
  AddOpStr : array[TffSqlAddOp] of string = (' + ', ' - ', ' || ');
var
  i : Integer;
begin
  Term[0].EmitSQL(Stream);
  for i := 1 to pred(TermCount) do begin
    WriteStr(Stream, AddOpStr[Term[i].AddOp]);
    Term[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlSimpleExpression.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(TermCount) do
    Term[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlSimpleExpression.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlSimpleExpression then begin
    if TermCount <> TffSqlSimpleExpression(Other).TermCount then
      exit;
    for i := 0 to pred(TermCount) do
      if not Term[i].Equals(TffSqlSimpleExpression(Other).Term[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlSimpleExpression.GetValue: Variant;
var
  i : Integer;
  Op: Variant;
  Type1, Type2 : TffFieldType;                                         {!!.13}
begin
  if assigned(OwnerSelect) and
  (OwnerSelect.AggQueryMode = aqmHaving) and not IsConstant
  and not IsParameter then begin
    Assert(BoundHaving);
    Result := BoundHavingField.GetValue;
    exit;
  end;
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  Result := Term[0].GetValue;
  if VarIsNull(Result) then exit;
  for i := 1 to pred(TermCount) do begin
    Op := Term[i].GetValue;
    if VarIsNull(Op) then begin
      Result := Null;
      exit;
    end;
    Type1 := Term[0].GetType;
    Type2 := Term[i].GetType;
    case Term[i].AddOp of
    aoPlus :
      if (Type1 in [fftStDate, fftStTime, fftDateTime]) and
         (Type2 = fftInterval) then
        Result := Term[i].AddIntervalTo(Result)
      else if (Type1 in [fftBLOB..fftBLOBTypedBin]) or
              (Type2 in [fftBLOB..fftBLOBTypedBin]) then
        Result := ConcatBLOBValues(Result, Op)
      else
        Result := Result + Op;
    aoMinus :
      if (Type1 in [fftStDate, fftStTime, fftDateTime]) and
         (Type2 = fftInterval) then
        Result := Term[i].SubtractIntervalFrom(Result)
      else
        Result := Result - Op;
    aoConcat :
      if (Type1 in [fftBLOB..fftBLOBTypedBin]) or
         (Type2 in [fftBLOB..fftBLOBTypedBin]) then
        Result := ConcatBLOBValues(Result, Op)
      else
        Result := Result + Op;
    end;
  end;
end;
{--------}
function TffSqlSimpleExpression.HasFieldRef: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    if Term[i].HasFieldRef then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlSimpleExpression.IsAggregateExpression: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    if Term[i].IsAggregateExpression then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlSimpleExpression.IsField(var FieldReferenced:
  TFFSqlFieldProxy): Boolean;
begin
  Result := (TermCount = 1) and Term[0].IsField(FieldReferenced);
end;
{--------}
function TffSqlSimpleExpression.IsFieldFrom(
  Table: TFFSqlTableProxy; var FieldReferenced: TFFSqlFieldProxy;
      var SameCase: Boolean): Boolean;
begin
  Result := (TermCount = 1) and Term[0].IsFieldFrom(Table,
    FieldReferenced, SameCase);
end;
{--------}
function TffSqlSimpleExpression.IsNull: Boolean;
var
  i: Integer;
begin
  for i := 0 to pred(TermCount) do
    if Term[i].IsNull then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlSimpleExpression.GetTerm(
  Index: Integer): TffSqlTerm;
begin
  Result := TffSqlTerm(TermList[Index]);
end;
{--------}
function TffSqlSimpleExpression.GetTermCount: Integer;
begin
  Result := TermList.Count;
end;
{--------}
function TffSqlSimpleExpression.GetTitle(const Qualified : Boolean): string; {!!.11}
begin
  if TermCount = 1 then
    Result := Term[0].GetTitle(Qualified)                            {!!.11}
  else
    Result := 'EXP';
end;
{--------}
function TffSqlSimpleExpression.IsParameter: Boolean;
begin
  Result := (TermCount = 1)
    and (Term[0].FactorCount = 1)
    and (Term[0].Factor[0].Param <> nil);
end;
{--------}
procedure TffSqlSimpleExpression.BindHaving;
var
  i: Integer;
begin
  BindingHaving := True;
  try
    if IsConstant
    or IsParameter then
      exit;
  finally
    BindingHaving := False;
  end;
  for i := 0 to pred(OwnerSelect.SelectionList.SelectionCount) do
    if OwnerSelect.SelectionList.Selection[i].SimpleExpression.Equals(
      Self) then begin
        BoundHavingField := OwnerSelect.HavingTable.Field(i);
        BoundHaving := True;
        exit;
      end;
    (* test code
    {attempt to bind to aliased expression}
    else
    if OwnerSelect.SelectionList.Selection[i].Column <> nil then begin
      if SameText(OwnerSelect.SelectionList.Selection[i].Column.ColumnName,
        trim(Self.SQLText)) then begin
          BoundHavingField := OwnerSelect.HavingTable.Field(i);
          BoundHaving := True;
          exit;
        end;
    end;
    *)
  SQLError('Expression in HAVING clause doesn''t match any columns');
end;
{--------}
function PropagateType(Type1, Type2: TffFieldType): TffFieldType;

  function IsInt(Type1: TffFieldType): Boolean;
  begin
    Result := Type1 in [fftByte, fftWord16, fftWord32,
      fftInt8, fftInt16, fftInt32, fftAutoInc];
  end;

  function IsSigned(Type1: TffFieldType): Boolean;
  begin
    Result := Type1 in [fftInt8, fftInt16, fftInt32];
  end;

begin
  if Type1 = Type2 then
    Result := Type1
  else
    if IsInt(Type1) then
      if IsInt(Type2) then
        if IsSigned(Type1) then
          if IsSigned(Type2) then
            Result := fftInt32
          else
            Result := fftSingle
        else
          if IsSigned(Type2) then
            Result := fftSingle
          else
            Result := fftWord32
      else
        Result := Type2
    else
      if IsInt(Type2) then
        Result := Type1
      else
        Result := fftExtended;
end;
{--------}
procedure TffSqlSimpleExpression.CheckType;
var
  i : Integer;
  Type2: TffFieldType;
begin
  FType := Term[0].GetType;
  if TermCount > 1 then begin
    case Term[1].AddOp of
    aoPlus :
      case FType of
      fftByte,
      fftWord16,
      fftWord32,
      fftInt8,
      fftInt16,
      fftInt32,
      fftAutoInc,
      fftSingle,
      fftDouble,
      fftExtended,
      fftComp,
      fftCurrency,
      fftStDate,
      fftStTime,
      fftDateTime,
      fftChar,
      fftWideChar,
      fftShortString,
      fftShortAnsiStr,
      fftNullString,
      fftNullAnsiStr,
      fftWideString :
        ;
      else
        SQLError('Operator/operand mismatch');
      end;
    aoMinus :
      case FType of
      fftByte,
      fftWord16,
      fftWord32,
      fftInt8,
      fftInt16,
      fftInt32,
      fftAutoInc,
      fftSingle,
      fftDouble,
      fftExtended,
      fftComp,
      fftCurrency:
        ;
      fftStDate,
      fftStTime,
      fftDateTime :
        case Term[1].GetType of
          fftStDate, fftStTime, fftDateTime :
            FType := fftDouble;
        end;  { case }
      else
        SQLError('Operator/operand mismatch');
      end;
    aoConcat :
      case FType of
      fftChar,
      fftWideChar,
      fftShortString,
      fftShortAnsiStr,
      fftNullString,
      fftNullAnsiStr,
      fftWideString :
        ;
      else
        SQLError('Operator/operand mismatch');
      end;
    end;
    for i := 1 to pred(TermCount) do begin
      Type2 := Term[i].GetType;
      case Term[i].AddOp of
      aoPlus :
        case Type2 of
        fftByte,
        fftWord16,
        fftWord32,
        fftInt8,
        fftInt16,
        fftInt32,
        fftAutoInc,
        fftSingle,
        fftDouble,
        fftExtended,
        fftComp,
        fftCurrency,
        fftChar,
        fftWideChar,
        fftShortString,
        fftShortAnsiStr,
        fftNullString,
        fftNullAnsiStr,
        fftWideString,
        fftStDate,
        fftStTime,
        fftDateTime,
        fftInterval:
        else
          SQLError('Operator/operand mismatch');
        end;
      aoMinus :
        case Type2 of
        fftByte,
        fftWord16,
        fftWord32,
        fftInt8,
        fftInt16,
        fftInt32,
        fftAutoInc,
        fftSingle,
        fftDouble,
        fftExtended,
        fftComp,
        fftCurrency,
        fftStDate,
        fftStTime,
        fftDateTime,
        fftInterval:
         ;
        else
          SQLError('Operator/operand mismatch');
        end;
      aoConcat :
        case Type2 of
        fftChar,
        fftWideChar,
        fftShortString,
        fftShortAnsiStr,
        fftNullString,
        fftNullAnsiStr,
        fftWideString :
          ;
        else
          SQLError('Operator/operand mismatch');
        end;
      end;
      case Type2 of
      fftByte, fftWord16, fftWord32, fftInt8, fftInt16, fftInt32,
      fftAutoInc, fftSingle, fftDouble, fftExtended, fftComp, fftCurrency :
        FType := PropagateType(FType, Type2);
      end;
    end;
  end;
  TypeKnown := True;
end;
{--------}
function TffSqlSimpleExpression.GetDecimals: Integer;
var
  i, j : Integer;
begin
  Result := Term[0].GetDecimals;
  for i := 1 to pred(TermCount) do begin
    j := Term[i].GetDecimals;
    if j > Result then
      Result := j;
  end;
end;
{--------}
function TffSqlSimpleExpression.GetSize: Integer;
var
  i : Integer;
begin
  Result := Term[0].GetSize;
  {operator here can only be aoConcat
    (because GetSize is only called for text fields)}
  for i := 1 to pred(TermCount) do
    inc(Result, Term[i].GetSize);
end;
{--------}
function TffSqlSimpleExpression.GetType: TffFieldType;
begin
  if not TypeKnown then
    CheckType;
  Result := FType
end;
{--------}
function TffSqlSimpleExpression.IsAggregate: Boolean;
begin
  Result := (TermCount = 1) and Term[0].IsAggregate;
end;
{--------}
procedure TffSqlSimpleExpression.CheckIsConstant;
var
  i : Integer;
  Save : TffSqlAggQueryMode;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(TermCount) do
    if not Term[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  if not BindingHaving then begin
    Save := aqmIdle;
    if assigned(OwnerSelect) then begin
      Save := OwnerSelect.AggQueryMode;
      OwnerSelect.AggQueryMode := aqmIdle;
    end;
    ConstantValue := GetValue;
    if assigned(OwnerSelect) then
      OwnerSelect.AggQueryMode := Save;                                
  end;
  FIsConstant := True;
end;
{--------}
function TffSqlSimpleExpression.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
procedure TffSqlSimpleExpression.MatchType(ExpectedType: TffFieldType);
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    Term[i].MatchType(ExpectedType);
end;
{--------}
function TffSqlSimpleExpression.Reduce: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(TermCount) do
    if Term[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlSimpleExpression.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{--------}
procedure TffSqlSimpleExpression.SetTerm(Index: Integer;
  const Value: TffSqlTerm);
begin
  TermList[Index] := Value;
end;
{Begin !!.11}
{--------}
function TffSqlSimpleExpression.WasWildcard : Boolean;
begin
  if TermCount = 1 then
    Result := Term[0].WasWildcard
  else
    Result := False;
end;
{End !!.11}
{====================================================================}

{===TffSqlTerm=======================================================}
function TffSqlTerm.AddFactor(Factor: TffSqlFactor): TffSqlFactor;
begin
  FactorList.Add(Factor);
  Result := Factor;
end;
{--------}
function TffSqlTerm.AddIntervalTo(Target: TDateTime): TDateTime;
begin
  Result := Factor[0].AddIntervalTo(Target);
end;
{--------}
function TffSqlTerm.SubtractIntervalFrom(Target: TDateTime): TDateTime;
begin
  Result := Factor[0].SubtractIntervalFrom(Target);
end;
{--------}
procedure TffSqlTerm.CheckIsConstant;
var
  i : Integer;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(FactorCount) do
    if not Factor[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  ConstantValue := GetValue;
  FIsConstant := True;
end;
{--------}
procedure TffSqlTerm.CheckType;
var
  i : Integer;
  Type2: TffFieldType;
begin
  FType := Factor[0].GetType;
  if FactorCount > 1 then begin
    case Factor[1].MulOp of
    moMul, moDiv :
      case FType of
      fftByte,
      fftWord16,
      fftWord32,
      fftInt8,
      fftInt16,
      fftInt32,
      fftAutoInc,
      fftSingle,
      fftDouble,
      fftExtended,
      fftComp,
      fftCurrency :
       ;
      else
        SQLError('Operator/operand mismatch');
      end;
    end;
    for i := 1 to pred(FactorCount) do begin
      case Factor[i].MulOp of
      moMul, moDiv :
        begin
          Type2 := Factor[i].GetType;
          case Type2 of
          fftByte,
          fftWord16,
          fftWord32,
          fftInt8,
          fftInt16,
          fftInt32,
          fftAutoInc,
          fftSingle,
          fftDouble,
          fftExtended,
          fftComp,
          fftCurrency :
           ;
          else
            SQLError('Operator/operand mismatch');
          end;
          case Type2 of
          fftByte, fftWord16, fftWord32, fftInt8, fftInt16, fftInt32,
          fftAutoInc, fftSingle, fftDouble, fftExtended, fftComp,
          fftCurrency :
            FType := PropagateType(FType, Type2);
          end;
        end;
      end;
    end;
  end;
  TypeKnown := True;
end;
{--------}
procedure TffSqlTerm.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlTerm then begin
    Clear;
    for i := 0 to pred(TffSqlTerm(Source).FactorCount) do begin
      AddFactor(TffSqlFactor.Create(Self)).Assign(
        TffSqlTerm(Source).Factor[i]);
    end;
    AddOp := TffSqlTerm(Source).AddOp;
  end else
    AssignError(Source);
end;
{--------}
constructor TffSqlTerm.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FactorList := TList.Create;
end;
{--------}
procedure TffSqlTerm.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    Factor[i].Free;
  FactorList.Clear;
end;
{--------}
function TffSqlTerm.DependsOn(Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    if Factor[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
destructor TffSqlTerm.Destroy;
begin
  Clear;
  FactorList.Free;
  inherited;
end;
{--------}
procedure TffSqlTerm.EmitSQL(Stream: TStream);
const
  MulOpStr : array[TffSqlMulOp] of string = (' * ', ' / ');
var
  i : Integer;
begin
  Factor[0].EmitSQL(Stream);
  for i := 1 to pred(FactorCount) do begin
    WriteStr(Stream, MulOpStr[Factor[i].MulOp]);
    Factor[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlTerm.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(FactorCount) do
    Factor[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlTerm.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if (Other is TffSqlTerm)
  and (AddOp = TffSqlTerm(Other).AddOp) then begin
    if FactorCount <> TffSqlTerm(Other).FactorCount then
      exit;
    for i := 0 to pred(FactorCount) do
      if not Factor[i].Equals(TffSqlTerm(Other).Factor[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlTerm.GetFactor(Index: Integer): TffSqlFactor;
begin
  Result := TffSqlFactor(FactorList[Index]);
end;
{--------}
function TffSqlTerm.GetFactorCount: Integer;
begin
  Result := FactorList.Count;
end;
{--------}
function TffSqlTerm.GetDecimals: Integer;
var
  i, j : Integer;
begin
  Result := Factor[0].GetDecimals;
  for i := 1 to pred(FactorCount) do begin
    j := Factor[i].GetDecimals;
    if j > Result then
      Result := j;
  end;
end;
{--------}
function TffSqlTerm.GetSize: Integer;
begin
  Result := Factor[0].GetSize;
end;
{--------}
function TffSqlTerm.GetTitle(const Qualified : Boolean): string;       {!!.11}
begin
  if FactorCount = 1 then
    Result := Factor[0].GetTitle(Qualified)                            {!!.11}
  else
    Result := 'EXP';
end;
{--------}
function TffSqlTerm.GetType: TffFieldType;
begin
  if not TypeKnown then
    CheckType;
  Result := FType
end;
{--------}
function TffSqlTerm.GetValue: Variant;
var
  i : Integer;
  Op: Variant;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  Result := Factor[0].GetValue;
  if VarIsNull(Result) then exit;
  for i := 1 to pred(FactorCount) do begin
    Op := Factor[i].GetValue;
    if VarIsNull(Op) then begin
      Result := Null;
      exit;
    end;
    case Factor[i{1}].MulOp of                                         {!!.11}
    moMul :
      Result := Result * Op;
    moDiv :
      Result := Result / Op;
    end;
  end;
end;
{--------}
function TffSqlTerm.IsAggregate: Boolean;
begin
  Result := (FactorCount = 1) and Factor[0].IsAggregate;
end;
{--------}
function TffSqlTerm.HasFieldRef: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    if Factor[i].HasFieldRef then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlTerm.IsAggregateExpression: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    if Factor[i].IsAggregate then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlTerm.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
function TffSqlTerm.IsField(var FieldReferenced: TFFSqlFieldProxy): Boolean;
begin
  Result := (FactorCount = 1) and Factor[0].IsField(FieldReferenced);
end;
{--------}
function TffSqlTerm.IsFieldFrom(Table: TFFSqlTableProxy;
  var FieldReferenced: TFFSqlFieldProxy; var SameCase: Boolean): Boolean;
begin
  Result := (FactorCount = 1) and Factor[0].IsFieldFrom(Table, FieldReferenced,
    SameCase);
end;
{--------}
function TffSqlTerm.IsNull: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    if Factor[i].IsNull then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlTerm.MatchType(ExpectedType: TffFieldType);
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    Factor[i].MatchType(ExpectedType);
end;
{--------}
function TffSqlTerm.Reduce: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(FactorCount) do
    if Factor[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlTerm.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{--------}
procedure TffSqlTerm.SetFactor(Index: Integer;
  const Value: TffSqlFactor);
begin
  FactorList[Index] := Value;
end;
{Begin !!.11}
{--------}
function TffSqlTerm.WasWildcard : Boolean;
begin
  if FactorCount = 1 then
    Result := Factor[0].WasWildcard
  else
    Result := False;
end;
{End !!.11}
{====================================================================}

{===TffSqlCondExp====================================================}
function TffSqlCondExp.AddCondTerm(Term: TffSqlCondTerm): TffSqlCondTerm;
begin
  CondTermList.Add(Term);
  Result := Term;
end;
{--------}
function TffSqlCondExp.AsBooleanLevel(Level: Integer): Boolean;
var
  i : Integer;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  for i := 0 to pred(CondTermCount) do
    if CondTerm[i].AsBooleanLevel(Level) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlCondExp.AsBoolean: Boolean;
var
  i : Integer;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  for i := 0 to pred(CondTermCount) do
    if CondTerm[i].AsBoolean then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlCondExp.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlCondExp then begin
    Clear;
    for i := 0 to pred(TffSqlCondExp(Source).CondTermCount) do
      AddCondTerm(TffSqlCondTerm.Create(Self)).Assign(
        TffSqlCondExp(Source).CondTerm[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlCondExp.BindHaving;
var
  i : Integer;
begin
  for i := 0 to pred(CondTermCount) do
    CondTerm[i].BindHaving;
end;

procedure TffSqlCondExp.CheckIsConstant;
var
  i : Integer;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(CondTermCount) do
    if not CondTerm[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  ConstantValue := GetValue;
  FIsConstant := True;
end;

constructor TffSqlCondExp.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  CondTermList := TList.Create;
end;
{--------}
procedure TffSqlCondExp.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(CondTermCount) do
    CondTerm[i].Free;
  CondTermList.Clear;
end;
{--------}
function TffSqlCondExp.DependsOn(Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(CondTermCount) do
    if CondTerm[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
destructor TffSqlCondExp.Destroy;
begin
  Clear;
  CondTermList.Free;
  inherited;
end;
{--------}
procedure TffSqlCondExp.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  CondTerm[0].EmitSQL(Stream);
  for i := 1 to pred(CondTermCount) do begin
    WriteStr(Stream, ' OR');
    CondTerm[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlCondExp.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(CondTermCount) do
    CondTerm[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlCondExp.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlCondExp then begin
    if CondTermCount <> TffSqlCondExp(Other).CondTermCount then
      exit;
    for i := 0 to pred(CondTermCount) do
      if not CondTerm[i].Equals(TffSqlCondExp(Other).CondTerm[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlCondExp.GetCondTerm(
  Index: Integer): TffSqlCondTerm;
begin
  Result := TffSqlCondTerm(CondTermList[Index]);
end;
{--------}
function TffSqlCondExp.GetCondTermCount: Integer;
begin
  Result := CondTermList.Count;
end;
{--------}
function TffSqlCondExp.GetDecimals: Integer;
begin
  if CondTermCount > 1 then
    TypeMismatch;
  Result := CondTerm[0].GetDecimals;
end;
{--------}
{!!.10 new}
function TffSqlCondExp.GetSize: Integer;
begin
  if CondTermCount > 1 then
    Result := 1
  else
    Result := CondTerm[0].GetSize;
end;
{--------}
function TffSqlCondExp.GetTitle(const Qualified : Boolean): string;    {!!.11}
begin
  if CondTermCount > 1 then
    Result := 'COND'
  else
    Result := CondTerm[0].GetTitle(Qualified);                         {!!.11}
end;
{--------}
function TffSqlCondExp.GetType: TffFieldType;
var
  i: Integer;
begin
  if CondTermCount > 1 then begin
    {force type conversion at lower level if necessary}
    for i := 0 to pred(CondTermCount) do
      CondTerm[i].GetType;
    Result := fftBoolean
  end else
    Result := CondTerm[0].GetType;
end;
{--------}
function TffSqlCondExp.GetValue: Variant;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  if CondTermCount > 1 then
    Result := AsBoolean
  else
    Result := CondTerm[0].GetValue;
end;
{--------}
function TffSqlCondExp.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

procedure TffSqlCondExp.MatchType(ExpectedType: TffFieldType);
begin
  if CondTermCount = 1 then                                            {!!.11}
    CondTerm[0].MatchType(ExpectedType)                                {!!.11}
  else                                                                 {!!.11}
  if GetType <> ExpectedType then
    TypeMismatch;
end;
{--------}
function TffSqlCondExp.Reduce: Boolean;
var
  i,j : Integer;
  InFactIX,
  InTermIX: Integer;
  NewTerm, LiftTerm : TffSqlCondTerm;
  NewFactor: TffSqlCondFactor;
  NewPrimary: TffSqlCondPrimary;
  LiftInClause: TffSqlInClause;
  LiftInExp: TffSqlSimpleExpression;
  LiftExp : TffSqlCondExp;
begin
  Result := False;
  LiftInClause := nil;
  LiftInExp := nil;
  LiftExp := nil;
  InTermIX := -1; //just to make the compiler happy
  InFactIX := -1; //just to make the compiler happy
  for i := 0 to pred(CondTermCount) do begin
    {look for conditional terms nested inside redundant parens}
    {eliminate parens when found}
    LiftTerm := nil;
    LiftExp := nil;
    with CondTerm[i] do begin
      if CondFactorCount = 1 then begin
        with CondFactor[0] do
          if not UnaryNot then
            if (CondPrimary.RelOp = roNone) then
              if CondPrimary.SimpleExp1 <> nil then
                if CondPrimary.JustSimpleExpression then
                  with CondPrimary.SimpleExp1 do
                    if TermCount  = 1 then begin
                      with Term[0] do
                        if FactorCount = 1 then
                          with Factor[0] do
                            if CondExp <> nil then
                              with CondExp do
                                if CondTermCount = 1 then begin
                                  LiftTerm := TffSqlCondTerm.Create(Self);
                                  LiftTerm.Assign(CondTerm[0]);
                                end;
                    end;
      end;
      if LiftTerm <> nil then begin
        Clear;
        Assign(LiftTerm);
        LiftTerm.Free;
        Result := True;
        {Get out. We may have more to do here, but Global Logic will
         call us again, and there may be other transformations that can
         be applied first.}
        break;
      end;
      if Reduce then begin
        {term itself was reduced}
        Result := True;
        break;
      end;
      if not Result then begin
        {look for IN expressions to be converted to simple comparisons}
        for j := 0 to pred(CondFactorCount) do
          with CondFactor[j] do
            if not UnaryNot then {can't handle negated expressions}
              if CondPrimary.RelOp = roNone then
                if (CondPrimary.InClause <> nil)
                and not (CondPrimary.InClause.Negated)
                and (CondPrimary.InClause.SubQuery = nil)
                and (CondPrimary.InClause.SimpleExpList.ExpressionCount <=
                  ffSqlInConvThreshold) then begin
                  {Here's one. Make a copy of it and get up to the
                   root level since we'll be doing surgery on this
                   very node hierarchy we're current looking at}
                  LiftInClause := TffSqlInClause.Create(Self);
                  LiftInClause.Assign(CondPrimary.InClause);
                  LiftInExp := TffSqlSimpleExpression.Create(Self);
                  LiftInExp.Assign(CondPrimary.SimpleExp1);
                  InTermIX := i; // just a reference back to here
                  if CondFactorCount > 1 then
                    {we have other factors that need to be copied -
                     make note of where the IN is - we should copy
                     everything BUT}
                    InFactIX := j
                    {we're the only factor, make a note of that by
                     setting the InFactIX flag to -1 indicating no
                     other factors should be copied}
                  else
                    InFactIX := -1;
                  break;
                end;
      end;
      if not Result then begin
        {look for nested conditional expressions to be lifted out, like
          (A OR B) AND C to be converted to A AND C OR B AND C}
        for j := 0 to pred(CondFactorCount) do
          with CondFactor[j] do
            if not UnaryNot then
              if (CondPrimary.RelOp = roNone) then
                if CondPrimary.SimpleExp1 <> nil then
                  if CondPrimary.JustSimpleExpression then
                    with CondPrimary.SimpleExp1 do
                      if TermCount  = 1 then begin
                        with Term[0] do
                          if FactorCount = 1 then
                            with Factor[0] do
                              if CondExp <> nil then begin
                                LiftExp := TffSqlCondExp.Create(Self);
                                LiftExp.Assign(CondExp);
                                InTermIX := i; // A reference back to here
                                InFactIX := j; // A reference back to here
                              end;
                      end;
      end;
      if LiftInClause <> nil then
        break;
      if LiftExp <> nil then
        break;
    end;
  end;
  if LiftExp <> nil then begin
    {create a top-level conditional term for each nested term,
     then copy each conditional factor except the one we're converting
     to each new term:}
    for i := 0 to pred(LiftExp.CondTermCount) do begin
      NewTerm := TffSqlCondTerm.Create(Self);
      NewTerm.Assign(LiftExp.CondTerm[i]);
      for j := 0 to pred(CondTerm[InTermIX].CondFactorCount) do
        if j <> InFactIX then begin
          NewFactor := TffSqlCondFactor.Create(NewTerm);
          NewFactor.Assign(CondTerm[InTermIX].CondFactor[j]);
          NewTerm.AddCondFactor(NewFactor);
        end;
      AddCondTerm(NewTerm);
    end;
    LiftInClause.Free;
    LiftInExp.Free;
    LiftExp.Free;
    CondTerm[InTermIX].Free;
    CondTermList.Delete(InTermIX);
    Result := True;
    exit;
  end;
  if (LiftInClause <> nil)
  and (InFactIX = -1) then begin {only do this optimization if no other factors} {!!.11}
    {Okay - that was the easy bit, finding the IN clause.
     We now need to build conditional terms for each of the
     alternatives - each with a simple comparison corresponding
     to each entry in the IN clause list.}
    for i := 0 to pred(LiftInClause.SimpleExpList.ExpressionCount) do begin
      NewTerm := TffSqlCondTerm.Create(Self);
      NewFactor := TffSqlCondFactor.Create(NewTerm);
      NewPrimary := TffSqlCondPrimary.Create(NewFactor);
      NewPrimary.SimpleExp1 := TffSqlSimpleExpression.Create(NewPrimary);
      NewPrimary.SimpleExp1.Assign(LiftInExp);
      NewPrimary.SimpleExp2 := TffSqlSimpleExpression.Create(NewPrimary);
      NewPrimary.SimpleExp2.Assign(LiftInClause.SimpleExpList.Expression[i]);
      NewPrimary.RelOp := roEQ;
      NewFactor.CondPrimary := NewPrimary;
      NewTerm.AddCondFactor(NewFactor);
      {If we didn't have any other conditional factors
       combined with the IN clause - IOW, we didn't have something like
           Exp IN [blahblah] AND something else,
       then we're actually done. All we need to do is add each term, then
       finish off by deleting the original term which held the IN clause.

       On the other hand, if we did have other factors, they all need to
       be copied to the new term:}
      if InFactIX <> -1 then begin
        with CondTerm[InTermIX] do
          for j := 0 to pred(CondFactorCount) do
            if j <> InFactIX then begin
              NewFactor := TffSqlCondFactor.Create(NewTerm);
              NewFactor.Assign(CondFactor[j]);
              NewTerm.AddCOndFactor(NewFactor);
            end;
      end;

      AddCondTerm(NewTerm);
    end;
    {LiftInClause.Free;}                                               {!!.12}
    {LiftInExp.Free;}                                                  {!!.12}
    //get rid of the original term with the IN clause
    CondTerm[InTermIX].Free;
    CondTermList.Delete(InTermIX);
    Result := True;
  end;
  LiftInClause.Free;                                                   {!!.12}
  LiftInExp.Free;                                                      {!!.12}
  {!!.11 begin}
  if not Result then
    for i := 0 to pred(CondTermCount) do
      if CondTerm[i].Reduce then begin
        Result := True;
        break;
      end;
  {!!.11 end}
end;

procedure TffSqlCondExp.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

procedure TffSqlCondExp.SetCondTerm(Index: Integer;
  const Value: TffSqlCondTerm);
begin
  CondTermList[Index] := Value;
end;

procedure TffSqlCondExp.SetLevelDep(List: TFFSqlTableProxySubsetList);
var
  i : Integer;
begin
  for i := 0 to pred(CondTermCount) do
    CondTerm[i].SetLevelDep(List);
end;

{====================================================================}


{===TffSqlCondTerm===================================================}
function TffSqlCondTerm.AddCondFactor(Factor: TffSqlCondFactor): TffSqlCondFactor;
begin
  CondFactorList.Add(Factor);
  Result := Factor;
end;
{--------}
function TffSqlCondTerm.InsertCondFactor(Index: Integer;
  Factor : TffSqlCondFactor): TffSqlCondFactor;
begin
  CondFactorList.Insert(Index, Factor);
  Result := Factor;
end;
{--------}
procedure TffSqlCondTerm.SetLevelDep(List: TFFSqlTableProxySubsetList);
var
  F, Level : Integer;
begin
  for F := 0 to pred(CondFactorCount) do
    with CondFactor[F] do begin
      EvalLevel := List.Count;
      for Level := pred(List.Count) downto 0 do
        if DependsOn(List.Item[Level].Table) then
          EvalLevel := Level;
    end;
end;

function TffSqlCondTerm.AsBoolean: Boolean;
var
  i : Integer;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  for i := 0 to pred(CondFactorCount) do
    if not CondFactor[i].AsBoolean then begin
      Result := False;
      exit;
    end;
  Result := True;
end;
{--------}
function TffSqlCondTerm.AsBooleanLevel(Level: Integer): Boolean;
var
  i : Integer;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  for i := 0 to pred(CondFactorCount) do
    if (CondFactor[i].EvalLevel >= Level)
    and not CondFactor[i].AsBoolean then begin
      Result := False;
      exit;
    end;
  Result := True;
end;
{--------}
procedure TffSqlCondTerm.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlCondTerm then begin
    Clear;
    for i := 0 to pred(TffSqlCondTerm(Source).CondFactorCount) do begin
      AddCondFactor(TffSqlCondFactor.Create(Self)).Assign(
        TffSqlCondTerm(Source).CondFactor[i]);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlCondTerm.BindHaving;
var
  i : Integer;
begin
  for i := 0 to pred(CondFactorCount) do
    CondFactor[i].BindHaving;
end;

procedure TffSqlCondTerm.CheckIsConstant;
var
  i : Integer;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(CondFactorCount) do
    if not CondFactor[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  ConstantValue := GetValue;
  FIsConstant := True;
end;

constructor TffSqlCondTerm.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  CondFactorList := TList.Create;
end;
{--------}
procedure TffSqlCondTerm.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(CondFactorCount) do
    CondFactor[i].Free;
  CondFactorList.Clear;
end;
{--------}
function TffSqlCondTerm.DependsOn(Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(CondFactorCount) do
    if CondFactor[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
destructor TffSqlCondTerm.Destroy;
begin
  Clear;
  CondFactorList.Free;
  OrderedSources.Free;
  inherited;
end;
{--------}
procedure TffSqlCondTerm.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  CondFactor[0].EmitSQL(Stream);
  for i := 1 to pred(CondFactorCount) do begin
    WriteStr(Stream,' AND');
    CondFactor[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlCondTerm.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(CondFactorCount) do
    CondFactor[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlCondTerm.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlCondTerm then begin
    if CondFactorCount <> TffSqlCondTerm(Other).CondFactorCount then
      exit;
    for i := 0 to pred(CondFactorCount) do
      if not CondFactor[i].Equals(TffSqlCondTerm(Other).CondFactor[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlCondTerm.GetCondFactor(
  Index: Integer): TffSqlCondFactor;
begin
  Result := TffSqlCondFactor(CondFactorList[Index]);
end;
{--------}
function TffSqlCondTerm.GetCondFactorCount: Integer;
begin
  Result := CondFactorList.Count;
end;
{--------}
function TffSqlCondTerm.GetDecimals: Integer;
begin
  if CondFactorCount > 1 then
    TypeMismatch;
  Result := CondFactor[0].GetDecimals;
end;
{--------}
{!!.10 new}
function TffSqlCondTerm.GetSize: Integer;
begin
  if CondFactorCount > 1 then
    Result := 1
  else
    Result := CondFactor[0].GetSize;
end;
{--------}
function TffSqlCondTerm.GetTitle(const Qualified : Boolean): string;   {!!.11}
begin
  if CondFactorCount > 1 then
    Result := 'COND'
  else
    Result := CondFactor[0].GetTitle(Qualified);                       {!!.11}
end;
{--------}
function TffSqlCondTerm.GetType: TffFieldType;
var
  i: Integer;
begin
  if CondFactorCount > 1 then begin
    {force type conversion at lower level if necessary}
    for i := 0 to pred(CondFactorCount) do
      CondFactor[i].GetType;
    Result := fftBoolean
  end else
    Result := CondFactor[0].GetType;
end;
{--------}
function TffSqlCondTerm.GetValue: Variant;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  if CondFactorCount > 1 then
    Result := AsBoolean
  else
    Result := CondFactor[0].GetValue;
end;
{--------}
function TffSqlCondTerm.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

{!!.11 new}
procedure TffSqlCondTerm.MatchType(ExpectedType: TffFieldType);
var
  i: Integer;
  T: TffFieldType;
begin
  if CondFactorCount > 1 then begin
    if ExpectedType <> fftBoolean then
      TypeMismatch;
    {force necessary type conversion at lower level}
    T := CondFactor[0].GetType;
    for i := 1 to CondFactorCount - 1 do
      CondFactor[i].MatchType(T);
  end else
    CondFactor[0].MatchType(ExpectedType);
end;

function TffSqlCondTerm.Reduce: Boolean;
var
  i, j : Integer;
  LiftFactor : TffSqlCondFactor;
  LiftTerm: TffSqlCondTerm;
  B : Boolean;
begin
  {Look for conditional factors nested inside redundant parens}
  { - eliminate parens when found}
  {Look for BETWEEN expressions and convert them to two comparisons}
  Result := False;
  for i := 0 to pred(CondFactorCount) do begin
    //LiftFactor := nil;
    LiftTerm := nil;
    with CondFactor[i] do begin
      if (CondPrimary.RelOp = roNone) then
        if CondPrimary.BetweenClause <> nil then begin
          if not CondPrimary.BetweenClause.Negated xor UnaryNot then begin
            {create a new CondPrimary to hold the >= comparison}
            LiftFactor := TffSqlCondFactor.Create(Self);
            LiftFactor.CondPrimary := TffSqlCondPrimary.Create(LiftFactor);
            LiftFactor.CondPrimary.RelOp := roGE;
            LiftFactor.CondPrimary.SimpleExp1 :=
              TffSqlSimpleExpression.Create(LiftFactor.CondPrimary);
            LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
            LiftFactor.CondPrimary.SimpleExp2 :=
              TffSqlSimpleExpression.Create(LiftFactor.CondPrimary);
            LiftFactor.CondPrimary.SimpleExp2.Assign(
              CondPrimary.BetweenClause.SimpleLow);
            InsertCondFactor(i, LiftFactor);
            {convert current CondPrimary to a >= comparison}
            CondPrimary.RelOp := roLE;
            CondPrimary.SimpleExp2 := TffSqlSimpleExpression.Create(CondPrimary);
            CondPrimary.SimpleExp2.Assign(CondPrimary.BetweenClause.SimpleHigh);
            CondPrimary.BetweenClause.Free;
            CondPrimary.BetweenClause := nil;
            Result := True;
            UnaryNot := False;
            break;
          end;
        end else
        if CondPrimary.LikeClause <> nil then begin
          if not CondPrimary.LikeClause.Negated xor UnaryNot then begin
            if CondPrimary.LikeClause.CanLimit then begin
              {create a new CondPrimary to hold the >= comparison}
              LiftFactor := TffSqlCondFactor.Create(Self);
              LiftFactor.CondPrimary := TffSqlCondPrimary.Create(LiftFactor);
              LiftFactor.CondPrimary.RelOp := roGE;
              LiftFactor.CondPrimary.SimpleExp1 := TffSqlSimpleExpression.Create(LiftFactor.CondPrimary);
              LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
              LiftFactor.CondPrimary.SimpleExp2 := CreateLiteralStringExp(LiftFactor, CondPrimary.LikeClause.GetLowLimit);
              InsertCondFactor(i, LiftFactor);
              {create a new CondPrimary to hold the <= comparison}
              LiftFactor := TffSqlCondFactor.Create(Self);
              LiftFactor.CondPrimary := TffSqlCondPrimary.Create(LiftFactor);
              LiftFactor.CondPrimary.RelOp := roL;
              LiftFactor.CondPrimary.SimpleExp1 := TffSqlSimpleExpression.Create(LiftFactor.CondPrimary);
              LiftFactor.CondPrimary.SimpleExp1.Assign(CondPrimary.SimpleExp1);
              LiftFactor.CondPrimary.SimpleExp2 := CreateLiteralStringExp(LiftFactor, CondPrimary.LikeClause.GetHighLimit);
              InsertCondFactor(i, LiftFactor);
              if CondPrimary.LikeClause.CanReplaceWithCompare then begin
                {we no longer need the LIKE clause}
                CondFactor[i + 2].Free;
                CondFactorList.Delete(i + 2); // adjust for the two we just inserted
              end else
                CondPrimary.LikeClause.Limited := True;
              Result := True;
              break;
            end;
          end;
        end else
        if CondPrimary.InClause <> nil then
        else
        if CondPrimary.IsTest <> nil then
        else
        if CondPrimary.ExistsClause <> nil then
        else
        if CondPrimary.UniqueClause <> nil then
        else
        if CondPrimary.MatchClause <> nil then
        else
          if CondPrimary.SimpleExp1 <> nil then
            with CondPrimary.SimpleExp1 do
              if TermCount  = 1 then begin
                with Term[0] do
                  if FactorCount = 1 then
                    with Factor[0] do
                      if CondExp <> nil then
                        with CondExp do
                          if CondTermCount = 1 then
                            LiftTerm := CondTerm[0];
              end;
      if LiftTerm <> nil then begin
        //first lift all but the very first conditional factor to this level
        for j := 1 to pred(LiftTerm.CondFactorCount) do
          Self.AddCondFactor(TffSqlCondFactor.Create(Self)).
            Assign(LiftTerm.CondFactor[j]);
        //then copy the contents of the first conditional factor
        //  (possibly the only one) into this one
        B := UnaryNot; // save UnaryNot setting
        LiftFactor := TffSqlCondFactor.Create(Self);
        LiftFactor.Assign(LiftTerm.CondFactor[0]);
        Clear;
        Assign(LiftFactor);
        LiftFactor.Free;
        UnaryNot := UnaryNot xor B;
        Result := True;
        {Get out. We may have more to do here, but Global Logic will
         call us again, and there may be other transformations that can
         be applied first.}
        break;
      end;
      if Reduce then begin
        {factor itself was reduced}
        Result := True;
        break;
      end;
    end;
  end;
  {!!.11 begin}
  if not Result then
    for i := 0 to pred(CondFactorCount) do
      if CondFactor[i].Reduce then begin
        Result := True;
        break;
      end;
  {!!.11 end}
end;

procedure TffSqlCondTerm.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

procedure TffSqlCondTerm.SetCondFactor(Index: Integer;
  const Value: TffSqlCondFactor);
begin
  CondFactorList[Index] := Value;
end;
{====================================================================}

{===TffSqlGroupColumnList=================================================}
function TffSqlGroupColumnList.AddColumn(Column: TffSqlGroupColumn):
  TffSqlGroupColumn;
begin
  ColumnList.Add(Column);
  Result := Column;
end;
{--------}
procedure TffSqlGroupColumnList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlGroupColumnList then begin
    Clear;
    for i := 0 to pred(TffSqlGroupColumnList(Source).ColumnCount) do
      AddColumn(TffSqlGroupColumn.Create(Self)).Assign(
        TffSqlGroupColumnList(Source).Column[i]);
  end else
    AssignError(Source);
end;
{--------}
function TffSqlGroupColumnList.Contains(const aColName : string;
                                        Se: TffSqlSelection): Boolean;
{Rewritten !!.06}
var
  i : Integer;
  aGrpColText,
  aSelText : string;
begin
  if Assigned(Se.SimpleExpression.Term[0].Factor[0].FieldRef) then
    aSelText := Trim(Se.SimpleExpression.Term[0].Factor[0].FieldRef.QualName)
  else
    aSelText := Trim(Se.SQLText);

  for i := 0 to pred(ColumnCount) do begin
    aGrpColText := Trim(Column[i].QualColumnName);
    Result := (AnsiCompareText(aColName, aGrpColText) = 0) or
              (AnsiCompareText(aSelText, aGrpColText) = 0);
    if Result then
      Exit;
  end;  { for }
  Result := False;
end;
{--------}
constructor TffSqlGroupColumnList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  ColumnList := TList.Create;
end;
{--------}
procedure TffSqlGroupColumnList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(ColumnCount) do
    Column[i].Free;
  ColumnList.Clear;
end;
{--------}
destructor TffSqlGroupColumnList.Destroy;
begin
  Clear;
  ColumnList.Free;
  inherited;
end;
{--------}
procedure TffSqlGroupColumnList.EmitSQL(Stream: TStream);
var
  i: Integer;
begin
  Column[0].EmitSQL(Stream);
  for i := 1 to pred(ColumnCount) do begin
    WriteStr(Stream,', ');
    Column[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlGroupColumnList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(ColumnCount) do
    Column[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlGroupColumnList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlGroupColumnList then begin
    if ColumnCount <> TffSqlGroupColumnList(Other).ColumnCount then
      exit;
    for i := 0 to pred(ColumnCount) do
      if not Column[i].Equals(TffSqlGroupColumnList(Other).Column[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlGroupColumnList.GetColumn(Index: Integer): TffSqlGroupColumn;
begin
  Result := TffSqlGroupColumn(ColumnList[Index]);
end;
{--------}
function TffSqlGroupColumnList.GetColumnCount: Integer;
begin
  Result := ColumnList.Count;
end;
{--------}
function TffSqlGroupColumnList.Reduce: Boolean;
begin
  Result := False;
end;

procedure TffSqlGroupColumnList.SetColumn(Index: Integer;
  const Value: TffSqlGroupColumn);
begin
  ColumnList[Index] := VAlue;
end;
{====================================================================}

{===TffSqlTableRefList===============================================}
function TffSqlTableRefList.AddTableRef(
  NewTableRef: TffSqlTableRef): TffSqlTableRef;
begin
  FTableRefList.Add(NewTableRef);
  Result := NewTableRef;
end;
{--------}
procedure TffSqlTableRefList.Assign(const Source: TffSqlNode);
var
  i: Integer;
begin
  if Source is TffSqlTableRefList then begin
    Clear;
    for i := 0 to pred(TffSqlTableRefList(Source).TableRefCount) do
      AddTableRef(TffSqlTableRef.Create(Self)).Assign(
        TffSqlTableRefList(Source).TableRef[i]);
  end else
    AssignError(Source);
end;

constructor TffSqlTableRefList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FTableRefList := TList.Create;
end;
{--------}
function TffSqlTableRefList.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to pred(TableRefCount) do begin
    Result := TableRef[i].BindFieldDown(TableName, FieldName);
    if Result <> nil then
      exit;
  end;
end;

function TffSqlTableRefList.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to pred(TableRefCount) do begin
    Result := TableRef[i].BindTable(AOwner, TableName);
    if Result <> nil then
      exit;
  end;
end;

procedure TffSqlTableRefList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(TableRefCount) do
    TableRef[i].Free;
  FTableRefList.Clear;
  inherited;
end;
{--------}
destructor TffSqlTableRefList.Destroy;
begin
  Clear;
  FTableRefList.Free;
  inherited;
end;
{--------}
procedure TffSqlTableRefList.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  if TableRefCount > 0 then begin
    TableRef[0].EmitSQL(Stream);
    for i := 1 to pred(TableRefCount) do begin
      WriteStr(Stream,' ,');
      TableRef[i].EmitSQL(Stream);
    end;
  end;
end;
{--------}
procedure TffSqlTableRefList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(TableRefCount) do
    TableRef[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlTableRefList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlTableRefList then begin
    if TableRefCount <> TffSqlTableRefList(Other).TableRefCount then
      exit;
    for i := 0 to pred(TableRefCount) do
      if not TableRef[i].Equals(TffSqlTableRefList(Other).TableRef[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
{!!.11 new}
function TffSqlTableRefList.GetFieldsFromTable(const TableName: string;
  List: TList): TffSqlTableProxy;
{-returns fields from table that are ultimately coming from the table
  specified in the TableName argument. NIL if not found.}
var
  i, j: Integer;
begin
  Result := nil;                                                       {!!.11}
  for i := 0 to TableRefCount - 1 do
    if SameText(TableRef[i].Alias, TableName)
    or SameText(TableRef[i].TableName, TableName) then begin
      Result := TableRef[i].ResultTable;
      for j := 0 to Result.FieldCount - 1 do
        List.Add(Result.Field(j));
      exit;
    end;
  {still here, which means that if there's a match, it's in a nested table}
  for i := 0 to TableRefCount - 1 do begin
    if TableRef[i].TableExp <> nil then                                {!!.11}
      Result := TableRef[i].TableExp.GetFieldsFromTable(TableName, List);
    if Result <> nil then
      exit;
  end;
//  Result := nil;                                                     {Deleted !!.11}
end;
{--------}
function TffSqlTableRefList.GetNameForAlias(const Alias : string) : string;
var
  Inx : Integer;
begin
  Result := '';
  for Inx := 0 to Pred(FTableRefList.Count) do begin
    if TffSqlTableRef(FTableRefList[Inx]).Alias = Alias then begin
      Result := TffSqlTableRef(FTableRefList[Inx]).TableName;
      Break;
    end;
  end;
end;
{--------}
function TffSqlTableRefList.GetTableRef(
  Index: Integer): TffSqlTableRef;
begin
  Result := TffSqlTableRef(FTableRefList[Index]);
end;
{--------}
function TffSqlTableRefList.GetTableRefCount: Integer;
begin
  Result := FTableRefList.Count;
end;
{--------}
{!!.11 rewritten}
function TffSqlTableRefList.Reduce: Boolean;
var
  i: Integer;
begin
  for i := 0 to TableRefCount - 1 do
    if TableRef[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TffSqlTableRefList.SetTableRef(Index: Integer;
  const Value: TffSqlTableRef);
begin
  FTableRefList[Index] := Value;
end;
{====================================================================}

{===TffSqlStatement==================================================}
procedure TffSqlStatement.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlStatement then begin
    Clear;
    if TffSqlStatement(Source).Insert <> nil then begin
      Insert := TffSqlINSERT.Create(Self);
      Insert.Assign(TffSqlStatement(Source).Insert);
    end;
    if TffSqlStatement(Source).Update <> nil then begin
      Update := TffSqlUPDATE.Create(Self);
      Update.Assign(TffSqlStatement(Source).Update);
    end;
    if TffSqlStatement(Source).Delete <> nil then begin
      Delete := TffSqlDELETE.Create(Self);
      Delete.Assign(TffSqlStatement(Source).Delete);
    end;
    if TffSqlStatement(Source).TableExp <> nil then begin
      TableExp := TffSqlTableExp.Create(Self);
      TableExp.Assign(TffSqlStatement(Source).TableExp);
    end;
    Reduce := TffSqlStatement(Source).Reduce;
    UseIndex := TffSqlStatement(Source).UseIndex;
  end else
    AssignError(Source);
end;
{Begin !!.11}
{--------}
procedure TffSqlStatement.Bind(const ClientID: TffClientID;
                               const SessionID: TffSessionID;
                                     Database : TffSqlDatabaseProxy);
begin
  FClientID := ClientID;
  FSessionID := SessionID;
  FDatabase := Database;
  if assigned(Insert) then
    Insert.Bind
  else if assigned(Update) then
    Update.Bind
  else if assigned(Delete) then
    Delete.Bind;
end;
{--------}
{End !!.11}
procedure TffSqlStatement.Clear;
begin
  Insert.Free;
  Insert := nil;
  Update.Free;
  Update := nil;
  Delete.Free;
  Delete := nil;
  TableExp.Free;
  TableExp := nil;
end;
{--------}
constructor TffSqlStatement.Create;
begin
  inherited Create(nil);
  {$IFDEF ExposeLastStatement}
  LastStatement := Self; {debug hook}
  {$ENDIF}
end;
{--------}
destructor TffSqlStatement.Destroy;
begin
  ParmList.Free;
  Clear;
  inherited;
  {$IFDEF ExposeLastStatement}
  LastStatement := nil; {debug hook}
  {$ENDIF}
end;
{--------}
procedure TffSqlStatement.EmitSQL(Stream: TStream);
begin
  if not UseIndex then
    WriteStr(Stream,'NOINDEX ');
  if not Reduce then
    WriteStr(Stream,'NOREDUCE ');
  if assigned(Insert) then
    Insert.EmitSQL(Stream);
  if assigned(Update) then
    Update.EmitSQL(Stream);
  if assigned(Delete) then
    Delete.EmitSQL(Stream);
  if assigned(TableExp) then
    TableExp.EmitSQL(Stream);
  WriteStr(Stream,';');
  WriteEOF(Stream);
end;
{--------}
procedure TffSqlStatement.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(Insert) then
    Insert.EnumNodes(EnumMethod, Deep);
  if assigned(Update) then
    Update.EnumNodes(EnumMethod, Deep);
  if assigned(Delete) then
    Delete.EnumNodes(EnumMethod, Deep);
  if assigned(TableExp) then
    TableExp.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlStatement.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlStatement)
    and ((BothNil(Insert, TffSqlStatement(Other).Insert)
    or (BothNonNil(Insert, TffSqlStatement(Other).Insert)
      and Insert.Equals(TffSqlStatement(Other).Insert))))
    and ((BothNil(Update, TffSqlStatement(Other).Update)
    or (BothNonNil(Update, TffSqlStatement(Other).Update)
      and Update.Equals(TffSqlStatement(Other).Update))))
    and ((BothNil(Delete, TffSqlStatement(Other).Delete)
    or (BothNonNil(Delete, TffSqlStatement(Other).Delete)
      and Delete.Equals(TffSqlStatement(Other).Delete))))
    and ((BothNil(TableExp, TffSqlStatement(Other).TableExp)
    or (BothNonNil(TableExp, TffSqlStatement(Other).TableExp)
      and TableExp.Equals(TffSqlStatement(Other).TableExp))));
end;
{--------}
{Begin !!.11}
function TffSqlStatement.Execute(var aLiveResult: Boolean;
                                 var aCursorID: TffCursorID;
                                 var RowsAffected,
                                     aRecordsRead: integer) : TffResult;
{End !!.11}
begin
  Result := DBIERR_NONE;                                               {!!.11}
  StartDate := Date;
  StartTime := Time;
  StartDateTime := Now;
  aCursorID := 0;
  RecordsRead := 0;
  if assigned(TableExp) then
    TableExp.Execute(aLiveResult, aCursorID, RecordsRead)
{Begin !!.11}
  else if assigned(Insert) then
    Result := Insert.Execute(RowsAffected)
  else if assigned(Update) then
    Result := Update.Execute(RowsAffected)
  else if assigned(Delete) then
    Result := Delete.Execute(RowsAffected)
  else
    raise Exception.Create('Statement is empty');
{End !!.11}
  aRecordsRead := RecordsRead;
end;
{-------}
procedure TffSqlStatement.ReduceStrength;
begin
  {$IFDEF LogTransformations}
  AssignFile(TRLog, TRLogFile);
  {$I-}
  Append(TRLog);
  if IOResult <> 0 then
    Rewrite(TRLog);
  writeln(TRLog);
  writeln(TRLog, 'Transforming ' + SQLText);
  writeln(TRLog, 'started at :',DateTimeToStr(Now));
  {$ENDIF}

  if assigned(TableExp) then begin
    while TableExp.Reduce do begin
      {$IFDEF LogTransformations}
      writeln(TRLog, 'new form:' + SQLText);
      {$ENDIF}
    end;
  end else
  {!!.11 begin}
  if assigned(Insert) then begin
    while Insert.Reduce do begin
      {$IFDEF LogTransformations}
      writeln(TRLog, 'new form:' + SQLText);
      {$ENDIF}
    end;
  end
  else
  if assigned(Update) then begin
    while Update.Reduce do begin
      {$IFDEF LogTransformations}
      writeln(TRLog, 'new form:' + SQLText);
      {$ENDIF}
    end;
  end else
  if assigned(Delete) then begin
    while Delete.Reduce do begin
      {$IFDEF LogTransformations}
      writeln(TRLog, 'new form:' + SQLText);
      {$ENDIF}
    end;
  end;
  {!!.11 end}

  {$IFDEF LogTransformations}
  writeln(TRLog);
  writeln(TRLog, 'ended at :',DateTimeToStr(Now));
  CloseFile(TRLog);
  {$ENDIF}
end;

procedure TffSqlStatement.SetParameter(Index: Integer; Value: Variant);
begin
  if ParmCount = 0 then
    raise Exception.Create('Error: Attempt to set parameter on non-parameterized query');
  if ParmList = nil then
    ParmList := TFFVariantList.Create(ParmCount);
  ParmList.SetValue(Index, Value);
end;

{====================================================================}

{===TffSqlSelect=====================================================}
{--------}
procedure TffSqlSELECT.AddTableRefs(Node: TffSqlNode);
begin
  Node.AddTableReference(Self);
end;
{--------}
procedure TffSqlSELECT.AddColumns(Node: TffSqlNode);
begin
  Node.AddColumnDef(Self);
end;
{--------}
procedure TffSqlSELECT.ClearBindings(Node: TffSqlNode);
begin
  Node.ClearBinding;
end;
{--------}
function TffSqlSELECT.Reduce: Boolean;
begin
  if SelectionList <> nil then
    Result := SelectionList.Reduce
  else
    Result := False;
  Result := Result or TableRefList.Reduce;
  if CondExpWhere <> nil then
    Result := Result or CondExpWhere.Reduce;
  if GroupColumnList <> nil then
    Result := Result or GroupColumnList.Reduce;
  if CondExpHaving <> nil then
    Result := Result or CondExpHaving.Reduce;
  if OrderList <> nil then
    Result := Result or OrderList.Reduce;
end;
{--------}
procedure TffSqlSELECT.ResetIsConstant(Node: TffSqlNode);
begin
  Node.ResetConstant;
end;
{--------}
procedure TffSqlSELECT.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, 'SELECT');
  if Distinct then
    WriteStr(Stream, ' DISTINCT')
  else
    WriteStr(Stream, ' ALL');
  if (SelectionList = nil) or WasStar then
    WriteStr(Stream, ' *')
  else
    SelectionList.EmitSQL(Stream);
  WriteStr(Stream, ' FROM');
  TableRefList.EmitSQL(Stream);
  if CondExpWhere <> nil then begin
    WriteStr(Stream,' WHERE');
    CondExpWhere.EmitSQL(Stream);
  end;
  if GroupColumnList <> nil then begin
    WriteStr(Stream,' GROUP BY');
    GroupColumnList.EmitSQL(Stream);
  end;
  if CondExpHaving <> nil then begin
    WriteStr(Stream,' HAVING');
    CondExpHaving.EmitSQL(Stream);
  end;
  if OrderList <> nil then
    OrderList.EmitSQL(Stream);
end;
{--------}
procedure TffSqlSELECT.AddTableFields(Table : TffSqlTableProxy;
                                const StartPoint : Integer;
                                      FieldRef : TffSqlFieldRef);
var
  Factor : TFFSqlFactor;
  j : Integer;
  Selection : TFFSqlSelection;
  StartVal : Integer;
  Term : TFFSqlTerm;
begin
  Assert(Table <> nil);
  Assert(Table is TffSqlTableProxy);
  if Table.FieldCount > 0 then begin
    StartVal := Pred(Table.FieldCount);
    { If passed a field reference then replace its field name with the
      first field of the table. }
    if FieldRef <> nil then begin
      FieldRef.WasWildcard := True;
      FieldRef.FieldName := Table.Field(StartVal).Name;
      dec(StartVal);
    end;
    for j := StartVal downto 0 do begin
      Selection := TffSqlSelection.Create(SelectionList);
      Selection.SimpleExpression :=
        TffSqlSimpleExpression.Create(Selection);
      Term := TFFSqlTerm.Create(Selection.SimpleExpression);
      Factor := TFFSqlFactor.Create(Term);
      Factor.FieldRef := TffSqlFieldRef.Create(Factor);
      if Table.Alias <> '' then                                        {!!.12}
        Factor.FieldRef.TableName := Table.Alias                       {!!.12}
      else                                                             {!!.12}
        Factor.FieldRef.TableName := Table.Name;
      Factor.FieldRef.FieldName := Table.Field(j).Name;
      Term.AddFactor(Factor);
      Selection.AddedByWildcard := True;
      Selection.SimpleExpression.AddTerm(Term);
      SelectionList.InsertSelection(StartPoint, Selection);
    end;
  end;
end;
{--------}
procedure TffSqlSELECT.AddTableFieldsFromList(Table : TffSqlTableProxy;
                                const StartPoint : Integer;
                                      FieldRef : TffSqlFieldRef;
                                      List: TList);
var
  Factor : TFFSqlFactor;
  j : Integer;
  Selection : TFFSqlSelection;
  StartVal : Integer;
  Term : TFFSqlTerm;
begin
  Assert(Table <> nil);
  Assert(Table is TffSqlTableProxy);
  if Table.FieldCount > 0 then begin
    StartVal := Pred(List.Count);
    { If passed a field reference then replace its field name with the
      first field of the table. }
    if FieldRef <> nil then begin
      FieldRef.WasWildcard := True;
      FieldRef.FieldName := TffSqlFieldProxy(List[StartVal]).Name;
      dec(StartVal);
    end;
    for j := StartVal downto 0 do begin
      Selection := TffSqlSelection.Create(SelectionList);
      Selection.SimpleExpression :=
        TffSqlSimpleExpression.Create(Selection);
      Term := TFFSqlTerm.Create(Selection.SimpleExpression);
      Factor := TFFSqlFactor.Create(Term);
      Factor.FieldRef := TffSqlFieldRef.Create(Factor);
      Factor.FieldRef.TableName := Table.Name;
      Factor.FieldRef.FieldName := TffSqlFieldProxy(List[j]).Name;
      Term.AddFactor(Factor);
      Selection.AddedByWildcard := True;
      Selection.SimpleExpression.AddTerm(Term);
      SelectionList.InsertSelection(StartPoint, Selection);
    end;
  end;
end;
{--------}
procedure TffSqlSELECT.ExpandWildcards;
var
  i, j, ix : Integer;
  T : TffSqlTableProxy;
  Simp : TFFSqlSimpleExpression;
  FR : TffSqlFieldRef;
  List: TList;                                                         {!!.11}
begin
  if SelectionList = nil then begin
    { If the selectionlist is empty then only a wildcard was specified.
      Note that with the fix of issue 481, this is dead code. }
    WasStar := True;
    SelectionList := TffSqlSelectionList.Create(Self);
    Assert(Assigned(TablesReferencedByOrder));
    for i := Pred(TablesReferencedByOrder.Count) downto 0 do begin
      T := TffSqlTableProxy(TablesReferencedByOrder.Objects[i]);
      AddTableFields(T, 0, nil);                               
    end;
  end else begin
    for i := pred(SelectionList.SelectionCount) downto 0 do begin
      Simp := SelectionList.Selection[i].SimpleExpression;
      if Simp <> nil then begin
        FR := Simp.Term[0].Factor[0].FieldRef;
        if FR <> nil then begin
          if FR.FieldName = '' then begin
            Assert(Assigned(TablesReferencedByOrder));
            { If no table name specified then add fields from all tables
              referenced in the FROM clause. }
            if FR.TableName = '' then begin
              Assert(Assigned(TablesReferencedByOrder));
              for j := pred(TablesReferencedByOrder.Count) downto 0 do begin
                T := TffSqlTableProxy(TablesReferencedByOrder.Objects[j]);
                if j = 0 then
                  AddTableFields(T, i, FR)
                else
                  AddTableFields(T, i, nil);
              end;
            end
            else begin
              { Otherwise the wildcard was qualified with a tablename. }
              ix := TablesReferencedByOrder.IndexOf(FR.TableName);
              if ix = -1 then begin
                Assert(Assigned(TableAliases));
                with TableAliases do begin
                  ix := IndexOf(FR.TableName);
                  if ix <> -1 then
                    ix := Integer(Objects[ix])
                  else begin
                    {!!.11 begin}
                    {might be part of a nested table expression}
                    List := TList.Create;
                    try
                      T := TableRefList.GetFieldsFromTable(FR.TableName, List);
                      if T <> nil then begin
                        AddTableFieldsFromList(T, i, FR, List);
                        ix := -1;
                      end else
                      {!!.11 end}
                        SQLError('Unknown table: ' + FR.TableName);
                    finally                                            {!!.11}
                      List.Free;                                       {!!.11}
                    end;                                               {!!.11}
                  end;
                end;
              end;
              if ix <> -1 then begin                                   {!!.11}
                T := TffSqlTableProxy(TablesReferencedByOrder.Objects[ix]);
                AddTableFields(T, i, FR);
              end;                                                     {!!.11}
            end;
          end;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TffSqlSELECT.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  if Deep then begin
    EnumMethod(Self);
    if SelectionList <> nil then
      SelectionList.EnumNodes(EnumMethod, Deep);
    TableRefList.EnumNodes(EnumMethod, Deep);
    if CondExpWhere <> nil then
      CondExpWhere.EnumNodes(EnumMethod, Deep);
    if GroupColumnList <> nil then
      GroupColumnList.EnumNodes(EnumMethod, Deep);
    if CondExpHaving <> nil then
      CondExpHaving.EnumNodes(EnumMethod, Deep);
    if OrderList <> nil then
      OrderList.EnumNodes(EnumMethod, Deep);
  end;
end;
{--------}
{!!.12 debug code
procedure TffSqlSELECT.CheckTableList;
var
  i : Integer;
begin
  if TablesReferencedByOrder <> nil then begin
    for i := 0 to pred(TablesReferencedByOrder.Count) do
      if pos('$$UNNAMED', TablesReferencedByOrder[i]) = 0 then
        if assigned(TablesReferencedByOrder.Objects[i]) then
          if not (TObject(TablesReferencedByOrder.Objects[i]) is TffSqlTableProxy) then
            raise Exception.Create('Table list broken');
  end;
end;
}
procedure TffSqlSELECT.ClearTableList;
var
  i : Integer;
begin
  {CheckTableList;} {!!.12 debug code}
  if TablesReferencedByOrder <> nil then begin
    for i := 0 to pred(TablesReferencedByOrder.Count) do
        if assigned(TablesReferencedByOrder.Objects[i]) then
          if TffSqlTableProxy(TablesReferencedByOrder.Objects[i]).Owner = Self then begin {!!.10}
            TffSqlTableProxy(TablesReferencedByOrder.Objects[i]).Owner := nil;            {!!.10}
            TObject(TablesReferencedByOrder.Objects[i]).Free;
          end;                                                                            {!!.10}
    TablesReferencedByOrder.Clear;
  end;
  if TableAliases <> nil then
    TableAliases.Clear;
  Bound := False;
end;
{--------}
procedure TffSqlSELECT.Bind;
var
  i, j : Integer;
  T : TffSqlTableProxy;
  Alias: string;                                                       {!!.11}
begin
  if CondExpWhere <> nil then
    CondExpWhere.EnumNodes(ClearBindings, False);
  if CondExpHaving <> nil then
    CondExpHaving.EnumNodes(ClearBindings, False);
  ClearTableList;
  TableRefList.EnumNodes(AddTableRefs, False);
  Assert(Assigned(TablesReferencedByOrder));
  for i := 0 to pred(TablesReferencedByOrder.Count) do begin
    Assert(TablesReferencedByOrder[i] <> '');
    if pos('$$UNNAMED', TablesReferencedByOrder[i]) <> 0 then
      Assert(TablesReferencedByOrder.Objects[i] <> nil)
    else begin
      j := TableAliases.IndexOfObject(TObject(i));
      if j = -1 then
        Alias := ''
      else
        Alias := TableAliases[j];
      T := Owner.FDatabase.TableByName(Self, TablesReferencedByOrder[i],
        False, Alias); {!!.11}
      if T = nil then
        SQLError('Unable to open table: ' + TablesReferencedByOrder[i] +
                 '. Ensure the table exists and is not in use by ' +
                 'another process.');
      TablesReferencedByOrder.Objects[i] := T;
    end;
  end;
  ExpandWildcards;

  if CondExpWhere <> nil then
    CondExpWhere.MatchType(fftBoolean);

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  SelectionList.EnumNodes(AddColumns, False);

  {figure out if we're using aggregates}
  {if we are, we need to prepare for those}
  HaveAggregates := False;

  SelectionList.EnumNodes(FlagAggregates, False);

  {!!.11 begin}
  if Distinct then begin
    {ensure that all fields have a type we can compare}
    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do begin
      case TffSqlNode(Columns.Objects[i]).GetType of
      fftBoolean..fftDateTime : ;
      fftShortString..{fftShortAnsiStr}fftWideString : ;               {!!.12}
      else
        SQLError('Field ' + Columns[i] + ' has a type, which is incompatible with DISTINCT');
      end;
    end;
  end;
  {!!.11 end}
  Bound := True;
end;
{--------}
function TffSqlSELECT.BindField(const TableName,
  FieldName: string): TFFSqlFieldProxy;
var
  T: TFFSqlTableProxy;
  j : Integer;
begin
  Result := nil;
  if TableName <> '' then begin
    Assert(Assigned(TablesReferencedByOrder));
    j := TablesReferencedByOrder.IndexOf(TableName);
    if (j = -1)
    {can't refer to aliased table with its actual name}                   {!!.12}
    or (TffSqlTableProxy(TablesReferencedByOrder.Objects[j]).Alias <> '') {!!.12}
     then begin
      //may be an alias
      Assert(Assigned(TableAliases));
      with TableAliases do begin
        j := IndexOf(TableName);
        if j <> -1 then begin
          j := Integer(Objects[j]);
          T := TffSqlTableProxy(TablesReferencedByOrder.Objects[j]);
          if T = nil then                                                       {!!.11}
            SQLError('Invalid field reference:' + TableName + '.' + FieldName); {!!.11}
        end else begin
          //may be a field from an exclosed expression
          if BindingDown then                                          {!!.11}
            Result := nil                                              {!!.11}
          else
            try                                                        {!!.11}
              BindingDown := True;                                     {!!.11}
              Result := TableRefList.BindFieldDown(TableName, FieldName);           {!!.11}
            finally                                                    {!!.11}
              BindingDown := False;                                    {!!.11}
            end;                                                       {!!.11}
          if Result = nil then
            if IsSubQuery then begin
              {may be field at outer level}
              Result := Parent.BindField(TableName, FieldName);
              IsDependent := True;
              exit;
            end;
          {else
            Result := TableRefList.BindFieldDown(TableName, FieldName);} {!!.11}
          if Result = nil then
            SQLError('Unknown field:' + TableName + '.' + FieldName);
          exit;
        end;
      end;
    end else begin
      T := TffSqlTableProxy(TablesReferencedByOrder.Objects[j]);
      Assert(T <> nil, 'Table not resolved:'
        + TffSqlTableProxy(TablesReferencedByOrder.Objects[j]).Name);  {!!.11}
    end;
    Assert(T <> nil);
    Result := T.FieldByName(FieldName);
    if Result = nil then
      SQLError('Unknown field:' + TableName + '.' + FieldName);
  end else begin
    Assert(Assigned(TablesReferencedByOrder));
    for j := 0 to pred(TablesReferencedByOrder.Count) do begin
      T := TffSqlTableProxy(TablesReferencedByOrder.Objects[j]);
      Assert(T <> nil);
      Assert(T is TffSqlTableProxy);
      if T.FieldByName(FieldName) <> nil then begin
        Result := T.FieldByName(FieldName);
        Exit;
      end;
    end;
    { No binding found yet. See if this is an alias for a field in the
      result table. }
    if Joiner <> nil then
      for j := 0 to Pred(Joiner.FT.Count) do begin
        if AnsiCompareText(TFFSqlFieldProxy(Joiner.FT[j]).Name, FieldName) = 0 then begin
          Result := Joiner.FT[j];
          Exit;
        end;
      end;
    SQLError('Unknown field:' + FieldName);
  end;
end;

function TffSqlSELECT.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  Result := TableRefList.BindTable(AOwner, TableName);
end;

{--------}
function TffSqlSELECT.FindField(const FieldName: string): TFFSqlFieldProxy;
var
  P : Integer;
begin
  P := PosCh('.', FieldName);
  if P = 0 then
    Result := BindField('', FieldName)
  else
    Result := BindField(copy(FieldName, 1, P - 1), copy(FieldName, P + 1, MaxInt));
end;
{--------}
procedure TffSqlSELECT.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlSELECT then begin
    Clear;
    Distinct := TffSqlSELECT(Source).Distinct;
    if TffSqlSELECT(Source).SelectionList <> nil then begin
      SelectionList := TffSqlSelectionList.Create(Self);
      SelectionList.Assign(TffSqlSELECT(Source).SelectionList);
    end;
    TableRefList := TffSqlTableRefList.Create(Self);
    TableRefList.Assign(TffSqlSELECT(Source).TableRefList);
    if TffSqlSELECT(Source).CondExpWhere <> nil then begin
      CondExpWhere := TffSqlCondExp.Create(Self);
      CondExpWhere.Assign(TffSqlSELECT(Source).CondExpWhere);
    end;
    if TffSqlSELECT(Source).GroupColumnList <> nil then begin
      GroupColumnList := TffSqlGroupColumnList.Create(Self);
      GroupColumnList.Assign(TffSqlSELECT(Source).GroupColumnList);
    end;
    if TffSqlSELECT(Source).CondExpHaving <> nil then begin
      CondExpHaving := TffSqlCondExp.Create(Self);
      CondExpHaving.Assign(TffSqlSELECT(Source).CondExpHaving);
    end;
    if TffSqlSELECT(Source).OrderList <> nil then begin
      OrderList := TffSqlOrderList.Create(Self);
      OrderList.Assign(TffSqlSELECT(Source).OrderList);
    end;
  end else
    AssignError(Source);
end;
{--------}
constructor TffSqlSELECT.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  TablesReferencedByOrder := TStringList.Create;
  TableAliases := TStringList.Create;
  TableAliases.Sorted := True;
  TableAliases.Duplicates := dupError;
  AggQueryMode := aqmIdle;                                           
end;
{--------}
procedure TffSqlSELECT.Clear;
begin
  ClearTableList;

  FSelectionList.Free;
  FSelectionList:= nil;

  FTableRefList.Free;
  FTableRefList:= nil;

  FCondExpWhere.Free;
  FCondExpWhere:= nil;

  FGroupColumnList.Free;
  FGroupColumnList:= nil;

  FCondExpHaving.Free;
  FCondExpHaving:= nil;

  FOrderList.Free;
  FOrderList:= nil;

end;
{--------}
function TffSqlSELECT.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if not Bound then
    Bind;
  Result :=
    ((CondExpWhere <> nil) and CondExpWhere.DependsOn(Table))
  or ((CondExpHaving <> nil) and CondExpHaving.DependsOn(Table));

end;
{--------}
destructor TffSqlSELECT.Destroy;
begin
  if FResultTable <> nil then begin
    FResultTable.Owner := nil;
    FResultTable.Free;
  end;
  Clear;
  TableAliases.Free;
  TablesReferencedByOrder.Free;
  Joiner.Free;
  inherited;
end;
{--------}
procedure TffSqlSELECT.FlagAggregates(Node: TffSqlNode);
begin
  Node.FlagAggregate(Self);
end;
{--------}
procedure TffSqlSELECT.EnumAggregates(Node: TffSqlNode);
begin
  Node.AddAggregate(AggList);
end;
{--------}
function TffSqlSELECT.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
var
  i: Integer;
begin
  for i := 0 to pred(Columns.Count) do
    if Columns.Objects[i] = F then begin
      Result := ResultTable.Field(i);
      exit;
    end;
  Result := nil;
end;

{ TAggCounter }

function TAggCounter.GetAvg: Variant;
begin
  if FCount <> 0 then
    Result := FSum / FCount
  else
    Result := Null;
end;

function TAggCounter.GetMax: Variant;
begin
  if FCount <> 0 then
    Result := FMax
  else
    Result := Null;
end;

function TAggCounter.GetMin: Variant;
begin
  if FCount <> 0 then
    Result := FMin
  else
    Result := Null;
end;

function TAggCounter.GetSum: Variant;
begin
  if FCount <> 0 then
    Result := FSum
  else
    Result := Null;
end;

procedure TAggCounter.Reset;
begin
  FCount := 0;
end;

const
  NumericVarTypes : set of Byte =
    [varSmallint, varInteger, varSingle,
      {$IFDEF DCC6OrLater}
      varShortInt,
      {$ENDIF}
      varDouble, varCurrency, varByte];

procedure TAggCounter.Add(const Value: Variant);
begin
  if FCount = 0 then begin
    FMin := Value;
    FMax := Value;
    if (VarType(Value) and VarTypeMask) in NumericVarTypes then
      FSum := Value;
  end else begin
    if Value < FMin then
      FMin := Value;
    if Value > FMax then
      FMax := Value;
    if (VarType(Value) and VarTypeMask) in NumericVarTypes then
      FSum := FSum + Value;
  end;
  FCount := FCount + 1;
end;

procedure TffSqlSELECT.EnsureResultTable(NeedData: Boolean);
begin
  Assert(TObject(Self) is TffSqlSELECT);
  if IsDependent or (NeedData and not HaveData) then begin
    if FResultTable <> nil then begin
      Assert(TObject(FResultTable) is TffSqlTableProxy);
      Assert(FResultTable.Owner = Self);
      FResultTable.Owner := nil;
      FResultTable.Free;
      FResultTable := nil;
    end;
  end;
  if FResultTable = nil then begin
    FResultTable := Execute2(NeedData);
    HaveData := NeedData;
  end;
end;

function TffSqlSELECT.CheckForValue(Value: Variant): Boolean;
begin
  EnsureResultTable(True);
  if VarIsNull(Value) then
    Result := False
  else begin
    ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
    Result := ResultTable.First;
  end;
end;

function TffSqlSELECT.CheckAllValues(RelOp: TffSqlRelOp;
  const Val: Variant): Boolean;
var
  TestVal: Variant;
begin
  EnsureResultTable(True);
  Result := False;
  if VarIsNull(Val) then exit;
  if ResultTable.First then begin
    repeat
      TestVal := ResultTable.Field(0).GetValue;
      if VarIsNull(TestVal) then exit;
      case RelOp of
      roEQ :
        if TestVal <> Val then
          exit;
      roLE :
        if Val > TestVal then
          exit;
      roL :
        if Val >= TestVal then
          exit;
      roG :
        if Val <= TestVal then
          exit;
      roGE :
        if Val < TestVal then
          exit;
      roNE :
        if TestVal = Val then
          exit;
      end;
    until not ResultTable.Next;
    Result := True;
  end;
end;

function TffSqlSELECT.CheckAnyValue(RelOp: TffSqlRelOp;
  const Val: Variant): Boolean;
begin
  EnsureResultTable(True);
  Result := True;
  if ResultTable.First then
    repeat
      case RelOp of
      roEQ :
        if ResultTable.Field(0).GetValue = Val then
          exit;
      roLE :
        if Val <= ResultTable.Field(0).GetValue then
          exit;
      roL :
        if Val < ResultTable.Field(0).GetValue then
          exit;
      roG :
        if Val > ResultTable.Field(0).GetValue then
          exit;
      roGE :
        if Val >= ResultTable.Field(0).GetValue then
          exit;
      roNE :
        if ResultTable.Field(0).GetValue <> Val then
          exit;
      end;
    until not ResultTable.Next;
  Result := False;
end;

function TffSqlSELECT.CheckNonEmpty: Boolean;
begin
  EnsureResultTable(True);
  Result := FResultTable.First;
end;

function TffSqlSELECT.GetDecimals: Integer;
begin
  if not TypeKnown then begin
    EnsureResultTable(False);
    FDecimals := FResultTable.Field(0).GetDecimals;
    FType := FResultTable.Field(0).GetType;
    FSize := FResultTable.Field(0).GetSize;                             {!!.13}
    TypeKnown := True;
  end;
  Result := FDecimals;
end;

{!!.13 new}
function TffSqlSELECT.GetSize: Integer;
begin
  if not TypeKnown then begin
    EnsureResultTable(False);
    FDecimals := FResultTable.Field(0).GetDecimals;
    FType := FResultTable.Field(0).GetType;
    FSize := FResultTable.Field(0).GetSize;
    TypeKnown := True;
  end;
  Result := FSize;
end;

function TffSqlSELECT.GetType: TffFieldType;
begin
  if not TypeKnown then begin
    EnsureResultTable(False);
    FDecimals := FResultTable.Field(0).GetDecimals;
    FType := FResultTable.Field(0).GetType;
    FSize := FResultTable.Field(0).GetSize;                             {!!.13}
    TypeKnown := True;
  end;
  Result := FType;
end;

function TffSqlSELECT.GetValue: Variant;
begin
  EnsureResultTable(True);
  if ResultTable.First then
    Result := ResultTable.Field(0).GetValue
  else
    Result := Null;
end;

procedure TffSqlSELECT.BuildSortList(Table: TffSqlTableProxy; var SortList: TffSqlSortArray);
{-logic extracted from DoOrderBy}
var
  i, z, k: Integer;
  IX : Integer;
  s: string;
  FR : TffSqlFieldRef;
  AliasName: string;
begin
  for i := 0 to pred(OrderList.OrderCount) do begin
    if OrderList.OrderItem[i].Column <> nil then begin
      s := OrderList.OrderItem[i].Column.QualColumnName;
      Assert(Assigned(Columns));
      z := Columns.IndexOf(S);
      if z = -1 then begin
        z := PosCh('.', S);
        if z = 0 then begin
          S := '.' + S;
          // may be unqualified field but qualified columns
          z := -1;
          for k := 0 to pred(Columns.Count) do
            if posI(S, Columns[k]) <> 0 then begin
              z := k;
              break;
            end;
          if z = -1 then begin
            SQLError('Unknown column specified in ORDER BY clause: ' +
                     Copy(S, 2, Length(S) - 1));
          end;
        end else begin
          // Try to find qualified column
          z := -1;
          {S := Uppercase(S);} {!!.10}
          Assert(Assigned(Columns));
          for k := 0 to pred(Columns.Count) do begin
            FR := (Columns.Objects[k] as TffSQLSimpleExpression).Term[0].Factor[0].FieldRef;
            if Assigned(FR) and
              SameText(S, Trim(FR.SQLText)) then begin
              z := k;
              break;
            end;
          end;
          if z = -1 then begin
            //Table might be aliased. Replace alias with corresponding name.
            z := PosCh('.', S);
            AliasName := UpperCase(Copy(s, 1, z-1));

            Assert(Assigned(TableAliases));
            IX := TableAliases.IndexOf(AliasName);
            if IX <> -1 then begin
              IX := Integer(TableAliases.Objects[IX]);
              Assert(Assigned(TablesReferencedByOrder));
              S := TablesReferencedByOrder[IX] + '.' +
                     UpperCase(Copy(S, Z+1, MaxInt));

              //Repeat search for field
              z := -1;
              Assert(Assigned(Columns));
              for k := 0 to Pred(Columns.Count) do begin
                FR := (Columns.Objects[K] as TffSQLSimpleExpression).Term[0].Factor[0].FieldRef;
                if Assigned(FR) and
                SameText(S, Trim(FR.SQLText))
                then begin
                  z := k;
                  break;
                end;
              end;
            end else
              z := -1;
          end;

          if z = -1 then begin
            // may be qualified field but unqualified columns
            z := PosCh('.', S);
            S := copy(S, z + 1, MaxInt);
            z := -1;
            Assert(Assigned(Columns));
            for k := 0 to pred(Columns.Count) do
              if posI(S, Columns[k]) <> 0 then begin
                z := k;
                break;
              end;
            if z = -1 then
              SQLError('Unknown column specified in ORDER BY clause:'+S);
          end;
        end;
      end;

      Assert(Assigned(Columns));
      SortList[i] := Table.FieldByName(Columns[z]).Index + 1;
    end else begin
      z := StrToInt(OrderList.OrderItem[i].Index);
      SortList[i] := Table.FieldByName(Columns[z - 1]).Index + 1;
    end;
    if OrderList.OrderItem[i].Descending then
      SortList[i] := -SortList[i];
  end;
end;

procedure TffSqlSELECT.DoOrderBy;
var
  SortList: TffSqlSortArray;
  Status : TffResult;
begin
  if (OrderList <> nil) and NeedData then begin

    BuildSortList(Table, SortList);                                    {!!.11}

    Status := Table.Sort(OrderList.OrderCount, SortList, False);       {!!.13}
    if Status <> DBIERR_NONE then
      raise EffException.CreateNoData(ffStrResServer, Status);
  end;
end;

function TffSqlSELECT.NormalQueryResult(NeedData: Boolean): TffSqlTableProxy;
var
  i : Integer;
  N : TffSqlNode;
  T2 : TffSqlTableProxy;
  F : TffSqlFieldProxy;
  FieldDefList: TffSqlFieldDefList;
begin

  {build a normal answer table}

  {build field definition for answer table}
  FieldDefList := TffSqlFieldDefList.Create;
  try
    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do begin
      N := TffSqlNode(Columns.Objects[i]);
      FieldDefList.AddField(Columns[i], N.GetType, N.GetSize, N.GetDecimals);
    end;

    Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self,
      FieldDefList);
  finally
    FieldDefList.Free;
  end;

  try

    if Joiner = nil then begin
      Joiner := TffSqlJoiner.Create(Owner, CondExpWhere);

      Assert(Assigned(TablesReferencedByOrder));
      for i := 0 to pred(TablesReferencedByOrder.Count) do
        Joiner.Sources.Add(
          TFFSqlTableProxySubset.Create(
            TFFSqlTableProxy(TablesReferencedByOrder.Objects[i])));

    end;

    Joiner.ClearColumnList;

    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do begin
      if TffSqlSimpleExpression(Columns.Objects[i]).IsField(F) then begin
        Joiner.AddColumn(
          nil,
          F,
          Result.Field(i));
      end else begin
        Joiner.AddColumn(
          TffSqlSimpleExpression(Columns.Objects[i]),
          nil,
          Result.Field(i));
      end;
    end;

    if NeedData then begin
      Joiner.Target := Result;
      Owner.FDatabase.StartTransaction([nil]);
      try
        Joiner.Execute(Owner.UseIndex, nil, jmNone);
      except
        Owner.FDatabase.AbortTransaction;
        raise;
      end;
      Owner.FDatabase.Commit;
    end;

    for i := 0 to Result.FieldCount - 1 do
      Result.Field(i).IsTarget := False;

    {At this point we have a table with all records that meet the
     WHERE criteria.}

    {if DISTINCT was specifed, we now need to remove any duplicates}

    if Distinct and NeedData then begin
      T2 := Result.CopyUnique(Self, True);                             {!!.13}
      Result.Owner := nil;
      Result.Free;
      Result := T2;
    end;

    if (Parent is TffSqlInClause) or (Parent is TffSqlMatchClause) then begin
      {need an index to allow the IN and MATCH clauses to be evaluated}

      T2 := Result.CopySortedOnAllFields(Self);

      Result.Owner := nil;
      Result.Free;
      Result := T2;
    end else begin
      //do ORDER BY

      DoOrderBy(NeedData, Result);

    end;
  except
    Result.Owner := nil;
    Result.Free;
    raise;
  end;
end;

function TffSqlSELECT.CheckHaving: Boolean;
begin
  Result := CondExpHaving.AsBoolean;
end;

procedure TffSqlSELECT.DoAggOrderBy;
{-utility method for AggregateQueryResult}
var
  i, j, z, k, IX: Integer;
  S: string;
  FR : TffSQLFieldRef;
  AliasName : string;
  SortList: TffSqlSortArray;
  Status : TffResult;
begin
  //do ORDER BY
  if OrderList <> nil then begin

    j := pred(OrderList.OrderCount);
    for i := 0 to j do begin
      if OrderList.OrderItem[i].Column <> nil then begin
        s := OrderList.OrderItem[i].Column.QualColumnName;
        z := Columns.IndexOf(S);
        if z = -1 then begin
          z := PosCh('.', S);
          if z = 0 then begin
            S := '.' + S;
            // may be unqualified field but qualified columns
            z := -1;
            for k := 0 to pred(Columns.Count) do
              if posI(S, Columns[k]) <> 0 then begin
                z := k;
                break;
              end;
            if z = -1 then begin
              SQLError('Unknown column specified in ORDER BY clause: ' +
                       Copy(S, 2, Length(S) - 1));
            end;
          end else begin
            // This is a qualified column. Try to find qualified column
            z := -1;
            for k := 0 to pred(Columns.Count) do begin
              FR := (Columns.Objects[k] as TffSQLSimpleExpression).
                Term[0].Factor[0].FieldRef;
              if Assigned(FR) and (posI(S, FR.SQLText) <> 0) then begin
                z := k;
                break;
              end;
            end;
            if z = -1 then begin
              //Table might be aliased. Replace alias with corresponding table name
              z := PosCh('.', S);
              AliasName := UpperCase(Copy(s, 1, z-1));

              Assert(Assigned(TableAliases));
              IX := TableAliases.IndexOf(AliasName);
              if IX <> -1 then begin
                IX := Integer(TableAliases.Objects[IX]);
                Assert(Assigned(TablesReferencedByOrder));
                S := TablesReferencedByOrder[IX] + '.' +
                       UpperCase(Copy(S, Z+1, MaxInt));

                //Repeat search for field
                z := -1;
                for k := 0 to Pred(Columns.Count) do begin
                  FR := (Columns.Objects[K] as TffSQLSimpleExpression).Term[0].Factor[0].FieldRef;
                  if Assigned(FR) and (posI(S, FR.SQLText) <> 0) then begin
                    z := k;
                    break;
                  end;
                end;
              end else
                z := -1;
            end;

            if z = -1 then begin
              // may be qualified field but unqualified columns
              Z := PosCh('.', S);
              S := copy(S, z + 1, MaxInt);
              Z := -1;
              for k := 0 to pred(Columns.Count) do
                if posI(S, Columns[k]) <> 0 then begin
                  z := k;
                  break;
                end;
              if z = -1 then
                SQLError('Unknown column specified in ORDER BY clause:'+S);
            end;
          end;
        end;

        SortList[i] := FGrpTable.Field(z).Index + 1;
      end else begin
        z := StrToInt(OrderList.OrderItem[i].Index);
        SortList[i] := FGrpTable.Field(z - 1).Index + 1;
      end;
      if OrderList.OrderItem[i].Descending then
        SortList[i] := -SortList[i];
    end;

    Status := FGrpTable.Sort(j + 1, SortList, False);                  {!!.13}
    if Status <> DBIERR_NONE then
      raise EffException.CreateNoData(ffStrResServer, Status);
  end;
end;

procedure TffSqlSELECT.DoGroupCopy;
var
  GroupColumnsOut : Integer;
  FieldDefList: TffSqlFieldDefList;
  i: Integer;
  N : TffSqlNode;
  Se : TffSqlSelection;
  T2 : TffSqlTableProxy;

  procedure CopyGrouped(const Source, Target: TFFSqlTableProxy;
    GroupColumnsIn, GroupColumnsOut, NonGroupColumns: Integer;
    const GroupColumnTargetField,
          AggExpList: TList);

  var
    i : Integer;
    IsFirst, HaveGroup, NewGroup : Boolean;
    LastValues : TffVariantList;

    procedure WriteGroup;
    var
      TgtInfo : TffGroupColumnTargetInfo;
      i : Integer;
    begin
      Target.Insert;
      for i := 0 to pred(GroupColumnsOut) do begin
        TgtInfo := TffGroupColumnTargetInfo(GroupColumnTargetField[i]);
        if TgtInfo <> nil then
          Target.Field(TgtInfo.SelFldIndex).SetValue
            (LastValues.GetValue(TgtInfo.LastValueIndex));
      end;
      for i := 0 to pred(NonGroupColumns) do
        Target.Field(GroupColumnsOut + i).SetValue(
          TffSqlSimpleExpression(AggExpList[i]).GetValue);
      for i := 0 to pred(AggList.Count) do
        TffSqlAggregate(AggList[i]).ResetCounters;
      Target.Post;
    end;


  begin

    Owner.FDatabase.StartTransaction([nil]);
    try
      IsFirst := True;
      HaveGroup := False;
      LastValues := TffVariantList.Create(GroupColumnsIn);
      {we know that the source table has grouping columns first}
      for i := 0 to pred(AggList.Count) do
        TffSqlAggregate(AggList[i]).CreateCounter(Source.Field(i + GroupColumnsIn));
      Source.First;
      while not Source.EOF do begin
        if IsFirst then begin
          IsFirst := False;
          NewGroup := True;
        end else begin
          NewGroup := False;
          for i := 0 to pred(GroupColumnsIn) do
            if Source.Field(i).GetValue <> LastValues.GetValue(i) then begin
              NewGroup := True;
              break;
            end;
        end;
        if NewGroup then begin
          if HaveGroup then begin
            Source.Prior;
            WriteGroup;
            Source.Next;
          end;
          for i := 0 to pred(GroupColumnsIn) do
            LastValues.SetValue(i, Source.Field(i).GetValue);
          HaveGroup := True;
        end;

        for i := 0 to pred(AggList.Count) do
          TffSqlAggregate(AggList[i]).Update;
        Source.Next;
      end;
      {If we happen to have an empty set AND if we don't have grouping
       columns, an 'empty' record should be added to hold the
        count value of zero as well as null for any aggregates}
      if HaveGroup or (GroupColumnsIn = 0) then
        WriteGroup;
      for i := 0 to pred(AggList.Count) do
        with TffSqlAggregate(AggList[i]) do
          DeleteCounter;
      Owner.FDatabase.Commit;
    finally
      LastValues.Free;
    end;
  end;

begin
  {build a normal answer table}

  GroupColumnsOut := 0;
  {build field definition for answer table}
  FieldDefList := TffSqlFieldDefList.Create;
  try
    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do begin
      N := TffSqlNode(Columns.Objects[i]);
      if i < GroupColumnsIn then                                       {!!.11}
        FieldDefList.AddField(Columns[i],
          N.GetType, N.GetSize, N.GetDecimals)
      else                                                             {!!.11}
{Begin !!.12}
        { Aggregate fields that reference date, time, & currency fields
          should be of the same type in the result set. Other field
          types should be changed to fftDouble in order to avoid clipping
          of the value. }
        case N.GetType of
        fftCurrency..fftDateTime:
          FieldDefList.AddField(Columns[i],
            N.GetType, N.GetSize, N.GetDecimals);
        else
          FieldDefList.AddField(Columns[i],
            fftDouble, 8, N.GetDecimals);
        end;
{End !!.12}
      Se := SelectionList.Selection[i];
      if (GroupColumnList <> nil) and
         GroupColumnList.Contains(Columns[i], Se) then
        inc(GroupColumnsOut)
      else
        AggExpList.Add(N);
    end;

    T2 := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  finally
    FieldDefList.Free;
  end;

  AggQueryMode := aqmGrouping;
  try
    CopyGrouped(
      FGrpTable,
      T2,
      GroupColumnsIn,
      GroupColumnsOut,
      AggExpList.Count,
      GroupColumnTargetField,
      AggExpList);
  finally
    AggQueryMode := aqmIdle;
  end;

  FGrpTable.Owner := nil;
  FGrpTable.Free;
  FGrpTable := T2;
end;

procedure TffSqlSELECT.DoHaving;
var
  T2 : TffSqlTableProxy;
begin
  if CondExpHaving <> nil then begin
    AggQueryMode := aqmHaving;
    try
      HavingTable := FGrpTable;
      CondExpHaving.BindHaving;
      CondExpHaving.EnumNodes(ResetIsConstant, False);
      T2 := FGrpTable.CopyValidated(Self, CheckHaving);
      FGrpTable.Owner := nil;
      FGrpTable.Free;
      FGrpTable := T2;
    finally
      AggQueryMode := aqmIdle;
    end;
  end;
end;

procedure TffSqlSELECT.DoSortOnAll;
var
  T2 : TffSqlTableProxy;
begin
  T2 := FGrpTable.CopySortedOnAllFields(Self);
  FGrpTable.Owner := nil;                                              {!!.11}
  FGrpTable.Free;
  FGrpTable := T2;
end;

procedure TffSqlSELECT.DoRemoveDups(NeedData: Boolean);
var
  i: Integer;
  LDistinct: Boolean;
  T2 : TffSqlTableProxy;
begin
  if not Distinct then begin
    LDistinct := False;
    for i := 0 to pred(AggList.Count) do
      if TffSqlAggregate(AggList[i]).Distinct then begin
        LDistinct := True;
        break;
      end;
  end else
    LDistinct := True;

  if LDistinct and NeedData then begin
    T2 := FGrpTable.CopyUnique(Self, True);                            {!!.13}
    FGrpTable.Owner := nil;
    FGrpTable.Free;
    FGrpTable := T2;
  end;
end;

procedure TffSqlSELECT.DoBuildGroupingTable;
var
  FieldDefList: TffSqlFieldDefList;
  i: Integer;
  Co : TffSqlGroupColumn;
  Se : TffSqlSelection;
  F : TffSqlFieldProxy;
  GrpTgtInfo : TffGroupColumnTargetInfo;
  Ag : TffSqlAggregate;
  FldType : TffFieldType;
begin
  FieldDefList := TffSqlFieldDefList.Create;
  try
    {build field definition for grouping table}
    for i := 0 to pred(GroupColumnsIn) do begin
      Co := GroupColumnList.Column[i];
      Se := SelectionList.FindSelection(Co);
      if Se <> nil then begin
        if Se.SimpleExpression.IsField(F) then begin
          FSF.Add(F);
          FSX.Add(nil);
        end else begin
          FSF.Add(nil);
          FSX.Add(Se.SimpleExpression);
        end;
        GrpTgtInfo := TffGroupColumnTargetInfo.Create;
        GrpTgtInfo.SelFldIndex := Se.Index;
        GrpTgtInfo.LastValueIndex := i;
        GroupColumnTargetField.Add(GrpTgtInfo);
        FieldDefList.AddField(
          Co.QualColumnName,
          Se.SimpleExpression.GetType,
          Se.SimpleExpression.GetSize,
          Se.SimpleExpression.GetDecimals);

      end else begin
        {grouping field is not in selection list}
        {must be plain field in source table}
        F := FindField(Co.QualColumnName);
        FSF.Add(F);
        FSX.Add(nil);
        FieldDefList.AddField(
          Co.QualColumnName,
          F.GetType,
          F.GetSize,
          F.GetDecimals);
      end;
    end;

    SelectionList.EnumNodes(EnumAggregates, False);

    for i := 0 to pred(AggList.Count) do begin
      Ag := TffSqlAggregate(AggList[i]);
      if Ag.SimpleExpression <> nil then begin
        FldType := Ag.SimpleExpression.GetType;
        if not Ag.ValidType(FldType) then
          raise Exception.CreateFmt('The %s aggregate function requires a numeric field.',
                                    [AgString[Ag.AgFunction]]);
        {AVG() needs float field even for integer expressions}
        if Ag.AgFunction = agAvg then
          FieldDefList.AddField(
            Ag.GetTitle(True) + '$' + IntToStr(i),                     {!!.11}
            fftDouble,
            0,
            2)
        else
          FieldDefList.AddField(
            Ag.GetTitle(True) + '$' + IntToStr(i),                     {!!.11} 
            FldType,
            Ag.SimpleExpression.GetSize,
            Ag.SimpleExpression.GetDecimals)
      end
      else // COUNT(* )
        FieldDefList.AddField(
          Ag.GetTitle(True) + '$' + IntToStr(i),                       {!!.11}
          fftDouble,
          0,
          0);

    end;

    FGrpTable := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self,
      FieldDefList);
  finally
    FieldDefList.Free;
  end;
end;

procedure TffSqlSELECT.DoCheckAggregates;
var
  i: Integer;
  Se : TffSqlSelection;
  F : TffSqlFieldProxy;
  LDistinct: Boolean;
begin
  LDistinct := False;
    { LDistinct is being used to check for situation where a non-aggregate
      column is listed after an aggregate column. }
  for i := 0 to pred(SelectionList.SelectionCount) do begin
    se := SelectionList.Selection[i];
    if se.IsAggregateExpression then
      LDistinct := True
    else if LDistinct then
      SQLError('Non-aggregate column "' + Trim(se.SQLText) +
               '" must appear before aggregate columns in the selection list.')
    else if se.SimpleExpression.IsField(F) and
            ((GroupColumnList = nil) or
             (not GroupColumnList.Contains(Columns[i], se))) then
      SQLError('Non-aggregate column "' + trim(se.SQLText) +
      '" must appear in GROUP BY');
  end;
end;

{!!.11 new}
function TffSqlSELECT.TableWithCount(const ColumnName: string): TffSqlTableProxy; {!!.12}
var
  FieldDefList: TffSqlFieldDefList;
begin
  FieldDefList := TffSqlFieldDefList.Create;
  try
    FieldDefList.AddField(ColumnName, fftDouble, 8, 0);                {!!.12}
    Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  finally
    FieldDefList.Free;
  end;
  Owner.FDatabase.StartTransaction([nil]);
  try
    Result.Insert;
    Result.Field(0).SetValue(TFFSqlTableProxy(TablesReferencedByOrder.Objects[0]).GetRecordCount);
    Result.Post;
    Owner.FDatabase.Commit;
  except
    Owner.FDatabase.AbortTransaction;
    raise;
  end;
end;

function TffSqlSELECT.AggregateQueryResult(NeedData: Boolean): TffSqlTableProxy;
var
  i : Integer;
  T2 : TffSqlTableProxy;
  GroupColumnsIn : Integer;
  SortList: TffSqlSortArray;
  GroupColumnTargetField,
  AggExpList,
  FSX : TList;
  FSF : TList;
  j : Integer;
  Status : TffResult;
  ColumnName: string;                                                  {!!.12}
begin
  {!!.11 begin}
  if (GroupColumnList = nil)
  and (CondExpWhere = nil)                                             {!!.12}
  and (TablesReferencedByOrder.Count = 1)
  and (CondExpHaving = nil)
  and (SelectionList.SelectionCount = 1)
  and (SelectionList.Selection[0].SimpleExpression <> nil)
  and (SelectionList.Selection[0].SimpleExpression.TermCount = 1)
  and (SelectionList.Selection[0].SimpleExpression.Term[0].FactorCount = 1)
  and (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate <> nil)
  and (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate.AgFunction = agCount)
  and (SelectionList.Selection[0].SimpleExpression.Term[0].Factor[0].Aggregate.SimpleExpression = nil) then begin
    {special case, plain "COUNT(*)" - use record count reported by low-level code}
    if SelectionList.Selection[0].Column <> nil then                   {!!.12}
      ColumnName := SelectionList.Selection[0].Column.ColumnName       {!!.12}
    else                                                               {!!.12}
      ColumnName := 'COUNT(*)';                                        {!!.12}
    Result := TableWithCount(ColumnName);                              {!!.12}
    exit;
  end;
  {!!.11 end}

  FGrpTable := nil;
  T2 := nil;

  {Columns contain the columns that will be in the result table.
   However, we may still group on other fields from the selection result -
   in particular if this is a sub-query}

  {field list for grouping table creation}

  FSX := nil;
  FSF := nil;
  GroupColumnTargetField := nil;
  AggExpList := nil;

  try
    {field lists for joiner - one for expressions, another for fields}
    FSX := TList.Create;
    FSF := TList.Create;

    {where the groups should appear in the final result}
    GroupColumnTargetField := TList.Create;
    AggExpList := TList.Create;

    if GroupColumnList = nil then
      GroupColumnsIn := 0
    else
      GroupColumnsIn := GroupColumnList.ColumnCount;

    {make sure all non-grouped columns are aggregate expressions}

    DoCheckAggregates;

    AggList := TList.Create;
    try
      DoBuildGroupingTable(GroupColumnsIn, FSF, FSX,
        GroupColumnTargetField);

      try
        if Joiner = nil then begin

          Joiner := TffSqlJoiner.Create(Owner, CondExpWhere);

          Assert(Assigned(TablesReferencedByOrder));
          for i := 0 to pred(TablesReferencedByOrder.Count) do
            Joiner.Sources.Add(
              TFFSqlTableProxySubset.Create(
                TFFSqlTableProxy(TablesReferencedByOrder.Objects[i])));
        end;

        Joiner.ClearColumnList;

        if GroupColumnList <> nil then begin
          for i := 0 to pred(GroupColumnsIn) do begin
            Joiner.AddColumn(
              FSX[i],
              FSF[i],
              FGrpTable.Field(i));
          end;
        end;

        for i := 0 to pred(AggList.Count) do begin
          Joiner.AddColumn(
            TffSqlAggregate(AggList[i]).SimpleExpression,
            nil,
            FGrpTable.Field(i + GroupColumnsIn));
        end;

        if NeedData then begin
          Joiner.Target := FGrpTable;
          Owner.FDatabase.StartTransaction([nil]);
          try
            Joiner.Execute(Owner.UseIndex, nil, jmNone);
            Owner.FDatabase.Commit;
          except
            Owner.FDatabase.AbortTransaction;
            raise;
          end;
        end;

        {turn off special aggregation flags so that the table result
         may be queried}
        for i := 0 to FGrpTable.FieldCount - 1 do
          FGrpTable.Field(i).IsTarget := False;

        {At this point we have a table with all records that meet the
         WHERE criteria.}

        {if DISTINCT was specifed, we now need to remove any duplicates}

        DoRemoveDups(NeedData);

        if GroupColumnList <> nil then begin
          { we need to group FGrpTable }
          { First, sort the data on groups }
          for i := 0 to pred(GroupColumnsIn) do
            SortList[i] := FGrpTable.Field(i).Index + 1;

          Status := FGrpTable.Sort(GroupColumnsIn, SortList, True);    {!!.13}
          if Status <> DBIERR_NONE then
            raise EffException.CreateNoData(ffStrResServer, Status);

        end;

        {we now have the data sorted on the grouping fields}
        {we then copy to another table with a slightly different
         layout to hold aggregate counters rather than data values
         for the non-grouped columns}

        DoGroupCopy(GroupColumnsIn, AggExpList,
          GroupColumnTargetField);

        DoHaving;

        if (Parent is TffSqlInClause) or (Parent is TffSqlMatchClause) then begin
          {need an index to allow the IN and MATCH clauses to be evaluated}

          DoSortOnAll;
        end else
          DoAggOrderBy;
      except
        if FGrpTable <> T2 then
          T2.Free;
        FGrpTable.Owner := nil;
        FGrpTable.Free;
        raise;
      end;

    finally
      AggList.Free;
    end;
    for j := 0 to Pred(GroupColumnTargetField.Count) do
      TffGroupColumnTargetInfo(GroupColumnTargetField[j]).Free;

  finally
    GroupColumnTargetField.Free;
    FSF.Free;
    FSX.Free;
    AggExpList.Free;
  end;
  Result := FGrpTable;
end;
{--------}
function TffSqlSELECT.Execute2(NeedData: Boolean): TffSqlTableProxy;
begin
  {check that all referenced tables and fields exist}
  if not Bound then
    Bind;

  if HaveAggregates or (GroupColumnList <> nil) then begin
    Result := AggregateQueryResult(NeedData);
    RequestLive := False;
  end else begin
    Result := NormalQueryResult(NeedData);
    RequestLive := False; {!!! for now}
  end;
end;
{--------}
procedure TffSqlSELECT.Execute(var aLiveResult: Boolean;
  var aCursorID: TffCursorID; var RecordsRead: Integer);
var
  T : TffSqlTableProxy;
begin
  Assert(Owner <> nil);
  RequestLive := aLiveResult;
  T := Execute2(True);
  aCursorID := T.CursorID;
  aLiveResult := RequestLive;
  T.LeaveCursorOpen := True;
  if T.Owner = Self then begin
    T.Owner := nil;
    T.Free;
  end;
end;
{--------}
function TffSqlSELECT.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlSELECT)
    and (Distinct = TffSqlSELECT(Other).Distinct)
    and (BothNil(SelectionList, TffSqlSELECT(Other).SelectionList)
       or (BothNonNil(SelectionList, TffSqlSELECT(Other).SelectionList)
         and SelectionList.Equals(TffSqlSELECT(Other).SelectionList))
       or ( ((SelectionList = nil) and TffSqlSELECT(Other).WasStar)
            or (WasStar and (TffSqlSELECT(Other).SelectionList = nil)) )
       )
    and TableRefList.Equals(TffSqlSELECT(Other).TableRefList)
    and (BothNil(CondExpWhere, TffSqlSELECT(Other).CondExpWhere)
       or (BothNonNil(CondExpWhere, TffSqlSELECT(Other).CondExpWhere)
         and CondExpWhere.Equals(TffSqlSELECT(Other).CondExpWhere))
       )
    and (BothNil(GroupColumnList, TffSqlSELECT(Other).GroupColumnList)
       or (BothNonNil(GroupColumnList, TffSqlSELECT(Other).GroupColumnList)
         and GroupColumnList.Equals(TffSqlSELECT(Other).GroupColumnList))
       )
    and (BothNil(CondExpHaving, TffSqlSELECT(Other).CondExpHaving)
       or (BothNonNil(CondExpHaving, TffSqlSELECT(Other).CondExpHaving)
         and CondExpHaving.Equals(TffSqlSELECT(Other).CondExpHaving))
       )
    and
    (BothNil(OrderList, TffSqlSELECT(Other).OrderList)
       or (BothNonNil(OrderList, TffSqlSELECT(Other).OrderList)
         and OrderList.Equals(TffSqlSELECT(Other).OrderList)));
end;
{--------}
function TffSqlSELECT.GetResultTable: TFFSqlTableProxy;
begin
  EnsureResultTable(True);
  Result := FResultTable;
end;

function TffSqlSELECT.IsSubQuery: Boolean;
var
  P: TffSqlNode;
begin
  P := Parent;
  while P <> nil do begin
    if (P is TffSqlSELECT)
    or (P is TffSqlUPDATE)
    or (P is TffSqlDELETE)
    or (P is TffSqlINSERT) then begin
      Result := True;
      exit;
    end;
    P := P.Parent;
  end;
  Result := False;
end;
{--------}
function TffSqlSELECT.Match(Value: Variant; Unique: Boolean;
  MatchOption: TffSqlMatchOption): Boolean;

  function RangeIsOne(const Table: TffSqlTableProxy): Boolean;
  begin
    Result := Table.First and not Table.Next;
  end;

begin
  EnsureResultTable(True);
  if not Unique then
    case MatchOption of
    moUnspec :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := ResultTable.First;
      end;
    moPartial :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := ResultTable.First;
      end;
    else//moFull :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := ResultTable.First;
      end;
    end
  else
    case MatchOption of
    moUnspec :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := RangeIsOne(ResultTable);
      end;
    moPartial :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := RangeIsOne(ResultTable);
      end;
    else//moFull :
      if VarIsNull(Value) then
        Result := True
      else begin
        ResultTable.SetRange([Value], [Value], 1, 1, True, True, True);
        Result := RangeIsOne(ResultTable);
      end;
    end;
end;
{--------}
procedure TffSqlSELECT.MatchType(ExpectedType: TffFieldType; AllowMultiple: Boolean);
begin
  //this will only be called when the current SELECT statement
  //functions as a sub-query
  if not AllowMultiple and (SelectionList.SelectionCount <> 1) then
    SQLError('Sub-query was expected to have exactly one column');
  EnsureResultTable(False);
end;
{====================================================================}

{===TffSqlFieldRef===================================================}
procedure TffSqlFieldRef.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlFieldRef then begin
    TableName :=  TffSqlFieldRef(Source).TableName;
    FieldName := TffSqlFieldRef(Source).FieldName;
  end else
    AssignError(Source);
end;

procedure TffSqlFieldRef.CheckType;
{ Rewritten !!.06}
var
  Found : Boolean;
  Inx : Integer;
  Select : TffSQLSelect;
  Selection : TffSQLSelection;
begin
  Found := False;
  { The field reference may be an alias or a direct reference to a field. }
  if (TableName = '') then begin
    { See if it is an alias. }
    Select := OwnerSelect;
    if Select <> nil then begin
      for Inx := 0 to Pred(Select.SelectionList.SelectionCount) do begin
        Selection := Select.SelectionList.Selection[Inx];
        if (not IsAncestor(Selection)) and
           (Selection.Column <> nil) and
           (AnsiCompareText(Selection.Column.ColumnName, FieldName) = 0) then begin
          FType := Selection.SimpleExpression.GetType;
          Found := True;
          Break;
        end;
      end;
    end else begin
    end;
  end;

  { If this isn't an alias then see if it is a direct reference. }
  if not Found then begin
    Assert(Field <> nil);
    FType := Field.GetType;
  end;
  TypeKnown := True;
end;
{--------}
procedure TffSqlFieldRef.ClearBinding;
begin
  FField := nil;
end;
{--------}
function TffSqlFieldRef.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  {!!.12 begin}
  if Field.IsTarget then begin
    Assert(OwnerSelect <> nil);
    if Field.SrcIndex > -1 then
      Result := TffSQLSimpleExpression(OwnerSelect.Joiner.FSX[
        Field.SrcIndex]).DependsOn(Table)
    else
      Result := Field.SrcField.OwnerTable = Table;
  end else
  {!!.12 end}
    Result := Field.OwnerTable = Table;
end;
{--------}
procedure TffSqlFieldRef.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' ');
  if WasWildcard then begin
    WriteStr(Stream, TableName);
    WriteStr(Stream, '.*');
  end else
    WriteStr(Stream, GetTitle(True));                                  {!!.11}
end;
{--------}
procedure TffSqlFieldRef.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlFieldRef.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlFieldRef)
    and (AnsiCompareText(TableName, TffSqlFieldRef(Other).TableName) = 0)
    and
      ( (AnsiCompareText(FieldName, TffSqlFieldRef(Other).FieldName) = 0)
        or (WasWildcard and (TffSqlFieldRef(Other).FieldName = '')
        or (((FieldName = '') and TffSqlFieldRef(Other).WasWildcard))));
end;
{--------}
function TffSqlFieldRef.GetDecimals: Integer;
begin
  Result := Field.GetDecimals;
end;
{--------}
function TffSqlFieldRef.GetField: TFFSqlFieldProxy;
begin
  if FField = nil then
    FField := Parent.BindField(TableName, FieldName);
  Result := FField;
end;
{--------}
function TffSqlFieldRef.GetGroupField: TFFSqlFieldProxy;
begin
  if OwnerSelect = nil then
    SQLError('Field references may not occur in this context');
  if FGroupField = nil then begin
    FGroupField := OwnerSelect.FGrpTable.FieldByName(QualName);
    if FGroupField = nil then begin
      FGroupField := OwnerSelect.FGrpTable.FieldByName(FieldName);
      if FGroupField = nil then
        SQLError('Unknown field:' + FieldName);
    end;
  end;
  Result := FGroupField;
end;
{--------}
function TffSqlFieldRef.GetSize: Integer;
begin
  Result := Field.GetSize;
end;
{--------}
function TffSqlFieldRef.GetTitle(const Qualified : Boolean): string;   {!!.11}
begin
  if Qualified and (TableName <> '') then                              {!!.11}
    if FieldName <> '' then
      Result := TableName + '.' + FieldName
    else
      Result := TableName + '.*'
  else
    Result := FieldName;
end;
{--------}
function TffSqlFieldRef.GetType: TffFieldType;
begin
  if not TypeKnown then
    CheckType;
  Result := FType;
end;
{--------}
function TffSqlFieldRef.GetValue: Variant;
begin
  if (OwnerSelect <> nil) and
  (OwnerSelect.AggQueryMode = aqmGrouping) then
    Result := GroupField.GetValue
  else if Field.IsTarget then begin
    Assert(OwnerSelect <> nil);
    if Field.SrcIndex > -1 then
      Result := TffSQLSimpleExpression(OwnerSelect.Joiner.FSX[
        Field.SrcIndex]).GetValue
    else
      Result := Field.SrcField.GetValue;
  end else
    Result := Field.GetValue;
end;
{--------}
function TffSqlFieldRef.IsNull: Boolean;
begin
  if (OwnerSelect <> nil) and
  (OwnerSelect.AggQueryMode = aqmGrouping) then
    Result := VarIsNull(GroupField.GetValue)
  else if Field.IsTarget then begin
    Assert(OwnerSelect <> nil);
    if Field.SrcIndex > -1 then
      Result := TffSQLSimpleExpression(OwnerSelect.Joiner.
        FSX[Field.SrcIndex]).IsNull
    else
      Result := Field.SrcField.IsNull;
  end else
    Result := Field.IsNull;
end;
{--------}
procedure TffSqlFieldRef.MatchType(ExpectedType: TffFieldType);
begin
  if GetType <> ExpectedType then
    case GetType of
      fftByte..fftCurrency :
        case ExpectedType of
          fftByte..fftCurrency :
            { OK };
        else
          TypeMismatch;
        end;
      fftStDate,
      fftStTime,
      fftDateTime :
        case ExpectedType of
          fftStDate..fftDateTime :
            { OK };
        else
          TypeMismatch;
        end;  { case }
      fftChar,
      fftWideChar,
      fftShortString..fftWideString :
        case ExpectedType of
          fftChar, fftWideChar, fftShortString..fftWideString :
            { OK };
        else
          TypeMismatch;
        end;  { case }
{Begin !!.13}
      fftBLOB..fftBLOBTypedBin :
        case ExpectedType of
          fftChar, fftWideChar,
          fftShortString..fftWideString,
          fftBLOB..fftBLOBTypedBin :
            { OK };
        else
          TypeMismatch;
        end;  { case }
{End !!.13}
    else
      TypeMismatch;
    end;  { case }
end;
{--------}
function TffSQLFieldRef.QualName : string;
var
  Name : string;
begin
  Result := FFieldName;
  { If no tablename specified then obtain table name of source table. }
  if FTableName = '' then begin
    if assigned(FField) then
      Result := FField.OwnerTable.Name + '.' + FFieldName
    else
      Result := FFieldName;
  end
  else begin
    if OwnerSelect = nil then
      SQLError('Field references may not occur in this context');
    { Has a table name. Is it really an alias? }
    Name := OwnerSelect.TableRefList.GetNameForAlias(FTableName);
    if Name <> '' then
      Result := Name + '.' + FFieldName
    else
      Result := TableName + '.' + FFieldName;
  end;
end;
{====================================================================}

{===TffSqlAggregate==================================================}
{--------}
procedure TffSqlAggregate.AddAggregate(Target: TList);
begin
  Target.Add(Self);
end;
{--------}
procedure TffSqlAggregate.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlAggregate then begin
    AgFunction := TffSqlAggregate(Source).AgFunction;
    SimpleExpression.Free;
    SimpleExpression := nil;
    if assigned(TffSqlAggregate(Source).SimpleExpression) then begin
      SimpleExpression := TffSqlSimpleExpression.Create(Self);
      SimpleExpression.Assign(TffSqlAggregate(Source).SimpleExpression);
    end;
    Distinct := TffSqlAggregate(Source).Distinct;
  end else
    AssignError(Source);
end;
{--------}
procedure TffSqlAggregate.CreateCounter(SourceField: TFFSqlFieldProxy);
begin
  FCounter := TAggCounter.Create;
  FSourceField := SourceField;
end;
{--------}
procedure TffSqlAggregate.DeleteCounter;
begin
  FCounter.Free;
  FCounter := nil;
  FSourceField := nil;
end;
{--------}
function TffSqlAggregate.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SimpleExpression.DependsOn(Table);
end;
{--------}
destructor TffSqlAggregate.Destroy;
begin
  SimpleExpression.Free;
  inherited;
end;
{--------}
procedure TffSqlAggregate.ResetCounters;
begin
  FCounter.Reset;
end;
{--------}
procedure TffSqlAggregate.Update;
begin
  case AgFunction of
  agCount :
    if (FSourceField = nil) or not VarIsNull(FSourceField.GetValue) then {!!.13}
      FCounter.Add(1);
  else
    if not VarIsNull(FSourceField.GetValue) then
      FCounter.Add(FSourceField.GetValue);
  end;
end;
{--------}
procedure TffSqlAggregate.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' ');
  WriteStr(Stream, AgString[AgFunction]);
  WriteStr(Stream,'(');
  if SimpleExpression <> nil then begin
    if Distinct then
      WriteStr(Stream,' DISTINCT')
    else
      WriteStr(Stream,' ALL');
    SimpleExpression.EmitSQL(Stream);
  end else
    WriteStr(Stream, '*');
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlAggregate.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if SimpleExpression <> nil then
    SimpleExpression.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlAggregate.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlAggregate)
    and (AgFunction = TffSqlAggregate(Other).AgFunction)
    and (Distinct = TffSqlAggregate(Other).Distinct)
    and (
      BothNil(SimpleExpression, TffSqlAggregate(Other).SimpleExpression)
      or (
        BothNonNil(SimpleExpression, TffSqlAggregate(Other).SimpleExpression)
        and SimpleExpression.Equals(TffSqlAggregate(Other).SimpleExpression)
      )
    );
end;
{--------}
function TffSqlAggregate.GetAggregateValue: Variant;
begin
  if FCounter = nil then
    Result := 0
  else begin
    case AgFunction of
    agCount :
      Result := FCounter.Count;
    agMin :
      Result := FCounter.Min;
    agMax :
      Result := FCounter.Max;
    agSum :
      Result := FCounter.Sum;
    else //agAvg :
      Result := FCounter.Avg;
    end;
  end;
end;
{--------}
procedure TffSqlAggregate.FlagAggregate(Select: TffSqlSELECT);
begin
  Select.HaveAggregates := True;
end;
{--------}
function TffSqlAggregate.GetDecimals: Integer;
begin
  case AgFunction of
  agCount :
    Result := 0;
  else
    Result := 2;
  end;
end;
{--------}
function TffSqlAggregate.GetSize: Integer;
begin
  if SimpleExpression <> nil then
    Result := SimpleExpression.GetSize
  else
    Result := 0;
end;
{--------}
function TffSqlAggregate.GetTitle(const Qualified : Boolean): string;  {!!.11}
begin
  Result := AgString[AgFunction] + '(';
  if Distinct then
    Result := Result + 'DISTINCT ';
  if SimpleExpression = nil then
    Result := Result + '*'
  else
    Result := Result + SimpleExpression.GetTitle(Qualified);           {!!.11}
  Result := Result + ')';
end;
{--------}
function TffSqlAggregate.GetType: TffFieldType;
begin
  if SimpleExpression = nil then
    Result := fftDouble
  else
    case SimpleExpression.GetType of
    fftExtended :
      Result := fftExtended;
    fftCurrency :
      case AgFunction of
      agCount :
        Result := fftDouble;
      else
        Result := fftCurrency;
      end;
    else
      case AgFunction of
      agCount,
      agAvg:
        Result := fftDouble;
      else
        Result := SimpleExpression.GetType;
      end;
    end;
end;
{--------}
procedure TffSqlAggregate.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftByte..fftCurrency :
    ;
  else
    TypeMismatch;
  end;
end;
{--------}
function TffSqlAggregate.Reduce: Boolean;
begin
  if SimpleExpression <> nil then
    Result := SimpleExpression.Reduce
  else
    Result := False;
end;
{--------}
function TffSqlAggregate.ValidType(aType : TffFieldType) : Boolean;
begin
  case agFunction of
    agSum, agAvg :
      Result := (aType in [fftByte..fftCurrency]);
  else
    Result := True;
  end;
end;
{====================================================================}

{===TffSqlColumn=====================================================}
procedure TffSqlColumn.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlColumn then begin
    ColumnName := TffSqlColumn(Source).ColumnName;
  end else
    AssignError(Source);
end;

procedure TffSqlColumn.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' ');
  WriteStr(Stream, ColumnName);
end;
{--------}
procedure TffSqlColumn.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlColumn.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlColumn)
    and (AnsiCompareText(ColumnName, TffSqlColumn(Other).ColumnName) = 0);
end;
{====================================================================}

{===TffSqlIsTest=====================================================}
function TffSqlIsTest.AsBoolean(const TestValue: Variant): Boolean;
begin
  case IsOp of
  ioNull :
    Result := VarIsNull(TestValue) xor UnaryNot;
  ioTrue :
    if UnaryNot then
      Result := not TestValue
    else
      Result := TestValue;
  ioFalse :
    if UnaryNot then
      Result := TestValue
    else
      Result := not TestValue;
  else
  //ioUnknown :
    Result := VarIsNull(TestValue) xor UnaryNot;
  end;
end;
{--------}
procedure TffSqlIsTest.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlIsTest then begin
    UnaryNot := TffSqlIsTest(Source).UnaryNot;
    IsOp := TffSqlIsTest(Source).IsOp;
  end else
    AssignError(Source);
end;

procedure TffSqlIsTest.EmitSQL(Stream: TStream);
const
  IsOpStr : array[TffSqlIsOp] of string =
    ('NULL', 'TRUE', 'FALSE', 'UNKNOWN');
begin
  WriteStr(Stream,' IS');
  if UnaryNot then
    WriteStr(Stream,' NOT');
  WriteStr(Stream,' ');
  WriteStr(Stream, IsOpStr[IsOp]);
end;
{--------}
procedure TffSqlIsTest.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlIsTest.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlIsTest)
    and (UnaryNot = TffSqlIsTest(Other).UnaryNot)
    and (IsOp = TffSqlIsTest(Other).IsOp);
end;
{--------}
function TffSqlIsTest.Evaluate(
  Expression: TffSqlSimpleExpression): Boolean;
{- allow check against NULL for non-variant compatible fields}
begin
  case IsOp of
  ioNull, ioUnknown :
    Result := Expression.IsNull xor UnaryNot;
  else
    Result := AsBoolean(Expression.GetValue);
  end;
end;

procedure TffSqlIsTest.MatchType(ExpectedType: TffFieldType);
begin
end;

{====================================================================}

{===TffSqlBetweenClause==============================================}
function TffSqlBetweenClause.AsBoolean(const TestValue: Variant): Boolean;
begin
  if VarIsNull(TestValue) then
    Result := False
  else
    Result :=
      (
        (TestValue >= SimpleLow.GetValue)
        and
        (TestValue <= SimpleHigh.GetValue)
      ) xor Negated;
end;
{--------}
procedure TffSqlBetweenClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlBetweenClause then begin
    Negated := TffSqlBetweenClause(Source).Negated;
    SimpleLow.Free;
    SimpleLow := TffSqlSimpleExpression.Create(Self);
    SimpleLow.Assign(TffSqlBetweenClause(Source).SimpleLow);
    SimpleHigh.Free;
    SimpleHigh := TffSqlSimpleExpression.Create(Self);
    SimpleHigh.Assign(TffSqlBetweenClause(Source).SimpleHigh);
  end else
    AssignError(Source);
end;

procedure TffSqlBetweenClause.CheckIsConstant;
begin
  FIsConstantChecked := True;
  FIsConstant :=
    SimpleLow.IsConstant and SimpleHigh.IsConstant;
end;
{--------}
function TffSqlBetweenClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SimpleLow.DependsOn(Table) or SimpleHigh.DependsOn(Table);
end;

destructor TffSqlBetweenClause.Destroy;
begin
  SimpleLow.Free;
  SimpleHigh.Free;
  inherited;
end;
{--------}
procedure TffSqlBetweenClause.EmitSQL(Stream: TStream);
begin
  if Negated then
    WriteStr(Stream,' NOT');
  WriteStr(Stream, ' BETWEEN ');
  SimpleLow.EmitSQL(Stream);
  WriteStr(Stream,' AND ');
  SimpleHigh.EmitSQL(Stream);
end;
{--------}
procedure TffSqlBetweenClause.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  SimpleLow.EnumNodes(EnumMethod, Deep);
  SimpleHigh.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlBetweenClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlBetweenClause)
    and (Negated = TffSqlBetweenClause(Other).Negated)
    and (SimpleLow.Equals(TffSqlBetweenClause(Other).SimpleLow))
    and (SimpleHigh.Equals(TffSqlBetweenClause(Other).SimpleHigh));
end;
{--------}
function TffSqlBetweenClause.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
procedure TffSqlBetweenClause.MatchType(ExpectedType: TffFieldType);
begin
  SimpleLow.MatchType(ExpectedType);
  SimpleHigh.MatchType(ExpectedType);
end;
{--------}
function TffSqlBetweenClause.Reduce: Boolean;
begin
  Result := SimpleLow.Reduce or SimpleHigh.Reduce;
end;

procedure TffSqlBetweenClause.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{====================================================================}

{ TffSqlLikePattern }

constructor TffsqlLikePattern.Create(SearchPattern: string; const Escape: string);
var
  i: Integer;
  Mask : string;
  Esc: Char;
begin
  FloatPatterns := TStringList.Create;
  FloatMasks := TStringList.Create;

  {
    Search pattern is made up of
      0 or 1 lead pattern
      0-N floating patterns, and
      0 or 1 trail pattern.
    Patterns are separated by '%'.
    If search pattern starts with '%', it does not have a lead pattern.
    If search pattern ends with '%', it does not have a trail pattern.

    Place holders, '_', are not considered here but in Find.

  }

  {build a separate mask string for place holders so that we can use
   the same logic for escaped and non-escaped search patterns}

  Mask := SearchPattern;
  if Escape <> '' then begin
    i := length(SearchPattern);
    Esc := Escape[1];
    while i >= 2 do begin
      if SearchPattern[i - 1] = Esc then begin
        Mask[i] := ' '; // blank out the mask character
        //remove the escape
        Delete(Mask, i - 1, 1);
        Delete(SearchPattern, i - 1, 1);
      end;
      dec(i);
    end;
  end;

  if (SearchPattern = '') then
    exit;

  if Mask[1] <> '%' then begin
    {we have a lead pattern}
    i := PosCh('%', Mask);
    if i = 0 then begin
      {entire search pattern is a lead pattern}
      LeadPattern := SearchPattern;
      LeadMask := Mask;
      exit;
    end;

    LeadPattern := copy(SearchPattern, 1, i - 1);
    LeadMask := copy(Mask, 1, i - 1);

    Delete(SearchPattern, 1, i - 1);
    Delete(Mask, 1, i - 1);
  end;

  if (SearchPattern = '') then
    exit;

  i := length(Mask);

  if Mask[i] <> '%' then begin
    {we have a trail pattern}
    while (i > 0) and (Mask[i] <> '%') do
      dec(i);
    if i = 0 then begin
      {entire remaining pattern is a trail pattern}
      TrailPattern := SearchPattern;
      TrailMask := Mask;
      exit;
    end;

    TrailPattern := copy(SearchPattern, i + 1, MaxInt);
    TrailMask := copy(Mask, i + 1, MaxInt);

    Delete(SearchPattern, i + 1, MaxInt);
    Delete(Mask, i + 1, MaxInt);
  end;

  {we now have one or more floating patterns separated by '%'}

  if Mask = '' then
    exit;

  if Mask[1] <> '%' then
    exit;

  Delete(Mask, 1, 1);
  Delete(SearchPattern, 1, 1);

  repeat

    i := PosCh('%', Mask);

    if i = 0 then begin
      {entire remaining search pattern is one pattern}
      FloatPatterns.Add(SearchPattern);
      FloatMasks.Add(Mask);
      exit;
    end;

    FloatPatterns.Add(copy(SearchPattern, 1, i - 1));
    FloatMasks.Add(copy(Mask, 1, i - 1));

    Delete(SearchPattern, 1, i);
    Delete(Mask, 1, i);

  until SearchPattern = '';

end;

destructor TffSqlLikePattern.Destroy;
begin
  FloatPatterns.Free;
  FloatMasks.Free;
  inherited;
end;

{!!.13 new}
function CharsDiffer(IgnoreCase: Boolean; C1, C2: Char): Boolean;
begin
  if IgnoreCase then
    Result := CharUpper(Pointer(C1)) <> CharUpper(Pointer(C2))
  else
    Result := C1 <> C2;
end;

function Match(const Pattern, Mask : string;
                     PatternLength : Integer;
               const PTextToSearch : PAnsiChar;
               const TextLen       : Integer;
                     StartIndex    : Integer;
                     IgnoreCase    : Boolean                           {!!.13}
                     ): Boolean;
{Modified !!.13}
{ Look for an exact match of the pattern at StartIndex, disregarding
  locations with '_' in the mask.
  Note: StartIndex is base zero. }
var
  i : Integer;
begin
  Result := True;
  if TextLen < PatternLength then
    Result := False
  else
  for i := 1 to PatternLength do
    if (Mask[i] <> '_') and
       {(PTextToSearch[StartIndex + i - 1] <> Pattern[i]) then begin} {!!.13}
       CharsDiffer(IgnoreCase, PTextToSearch[StartIndex + i - 1], Pattern[i]) then begin {!!.13}
        Result := False;
        Break;
    end;  { if }
end;

function Scan(const Pattern, Mask : string;
                    PatternLength : Integer;
              const PTextToSearch : PAnsiChar;
              const TextLen       : Integer;
                    StartIndex: Integer;
                    IgnoreCase: Boolean                                {!!.13}
                    ) : Integer;
{Modified !!.13}
{ Scan for a match of the pattern starting at StartIndex, disregarding
  locations with '_' in the mask. Return -1 if not found, otherwise
  return the position immediately following the matched phrase. }
var
  L, i : Integer;
  Found : Boolean;
begin
  L := TextLen - StartIndex;
  repeat
    if L < PatternLength then begin
      Result := -1;
      Exit;
    end;
    Found := True;
    for i := 1 to PatternLength do
      if (i - 1 > L) or (Mask[i] <> '_') and
         {(PTextToSearch[i + StartIndex - 1] <> Pattern[i]) then begin} {!!.13}
         CharsDiffer(IgnoreCase, PTextToSearch[i + StartIndex - 1], Pattern[i]) then begin {!!.13}
        Found := False;
        Break;
      end;
    if Found then begin
      Result := StartIndex + PatternLength;
      Exit;
    end;
    inc(StartIndex);
    dec(L);
  until False;
end;

function TffSqlLikePattern.Find(const TextToSearch: Variant;
         IgnoreCase: Boolean                                           {!!.13}
         ): Boolean;
{Rewritten !!.13}
{Search the TextToSearch. Return true if the search pattern was found}
var
  TextLen,
  LeadLen,
  TrailLen,
  i,
  l,
  StartPos,
  EndPos: Integer;
  VStr, P : string;
  VPtr : PAnsiChar;
begin
  Result := False;
  try
    if TVarData(TextToSearch).VType and VarTypeMask = varByte then begin
      TextLen := VarArrayHighBound(TextToSearch, 1);
      if TextLen = 0 then
        Exit;
      VStr := '';
      VPtr := VarArrayLock(TextToSearch);
    end
    else begin
      TextLen := Length(TextToSearch);
      if TextLen = 0 then
        Exit;
      VStr := VarToStr(TextToSearch);
      VPtr := PAnsiChar(VStr);
    end;

    LeadLen := Length(LeadPattern);
    TrailLen := Length(TrailPattern);
    if LeadLen > 0 then begin
      { If there is a lead pattern then see if there is a match. }
      if not Match(LeadPattern, LeadMask, LeadLen, VPtr, TextLen, 0,
             IgnoreCase) then begin                                    {!!.13}
        { No match so exit. }
        Result := False;
        Exit;
      end;
      { There was a match so set the starting position for the next match. }
      StartPos := LeadLen;
    end else
      { No lead pattern. Next match starts at beginning of string. }
      StartPos := 0;

    if TrailLen > 0 then begin
      { There is a trail pattern. Does it overlap with the lead pattern?  }
      i := TextLen - TrailLen;
      if i < StartPos then begin
        { Yes it overlaps. A match is not possible so exit. }
        Result := False;
        Exit;
      end;
      if not Match(TrailPattern, TrailMask, TrailLen, VPtr, TextLen, i,
             IgnoreCase) then begin                                    {!!.13}
        Result := False;
        Exit;
      end;
      EndPos := i - 1;
    end else
      EndPos := TextLen - 1;

    if FloatPatterns.Count = 0 then
      if TextLen <> LeadLen + TrailLen then begin
        Result := False;
        Exit;
      end;

    for i := 0 to pred(FloatPatterns.Count) do begin
      P := FloatPatterns[i];
      l := Length(P);
      { If the length of the float pattern is greater than the number of
        characters left in the string then a match is not possible. }
      if l > EndPos - StartPos + 1 then begin
        Result := False;
        Exit;
      end;
      StartPos := Scan(P, FloatMasks[i], l, VPtr, TextLen, StartPos, IgnoreCase); {!!.13}
      if StartPos = -1 then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  finally
    if VStr = '' then
      VarArrayUnlock(TextToSearch);
  end;
end;
{===TffSqlLikeClause=================================================}
function TffSqlLikeClause.AsBoolean(const TestValue: Variant): Boolean;
begin
  if VarIsNull(TestValue) then begin
    Result := Negated;
    exit;
  end;
  if LikePattern = nil then
    if EscapeExp <> nil then
      LikePattern := TffSqlLikePattern.Create(SimpleExp.GetValue, EscapeExp.GetValue)
    else
      LikePattern := TffSqlLikePattern.Create(SimpleExp.GetValue, '');
  Result := LikePattern.Find(TestValue, IgnoreCase) xor Negated;       {!!.13}
  if not IsConstant then begin
    LikePattern.Free;
    LikePattern := nil;
  end;
end;
{--------}
procedure TffSqlLikeClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlLikeClause then begin
    if SimpleExp = nil then
      SimpleExp := TffSqlSimpleExpression.Create(Self);
    SimpleExp.Assign(TffSqlLikeClause(Source).SimpleExp);
    if (EscapeExp = nil) and (TffSqlLikeClause(Source).EscapeExp <> nil) then begin
      EscapeExp := TffSqlSimpleExpression.Create(Self);
      EscapeExp.Assign(TffSqlLikeClause(Source).EscapeExp);
    end;
    Negated := TffSqlLikeClause(Source).Negated;
  end else
    AssignError(Source);
end;

function TffSqlLikeClause.CanLimit: Boolean;
var
  S: string;
begin
  Result := False;
  if not Limited
    and not IgnoreCase                                                 {!!.13}
    and SimpleExp.IsConstant
    and ((EscapeExp = nil) {or EscapeExp.IsConstant}) then begin       {!!.11}
      S := SimpleExp.GetValue;
      if not (S[1] in ['%', '_']) then
        Result := (GetHighLimit <> '');
    end;
end;

function TffSqlLikeClause.CanReplaceWithCompare: Boolean;
var
  S: string;
begin
  Result := False;
  if not Limited
    and not IgnoreCase                                                 {!!.13}
    and SimpleExp.IsConstant
    and ((EscapeExp = nil) {or EscapeExp.IsConstant}) then begin       {!!.11}
      S := SimpleExp.GetValue;
      Result := (PosCh('_', S) = 0)
        and (length(S) > 1)
        and (PosCh('%', S) = length(S));
    end;
end;

procedure TffSqlLikeClause.CheckIsConstant;
begin
  FIsConstantChecked := True;
  FIsConstant := SimpleExp.IsConstant and ((EscapeExp = nil) or EscapeExp.IsConstant);
end;
{--------}
function TffSqlLikeClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SimpleExp.DependsOn(Table);
end;

destructor TffSqlLikeClause.Destroy;
begin
  SimpleExp.Free;
  EscapeExp.Free;
  LikePattern.Free;
  if FBmTable <> nil then                                              {!!.11}
    Dispose(FBmTable);                                                 {!!.11}
  inherited;
end;
{--------}
procedure TffSqlLikeClause.EmitSQL(Stream: TStream);
begin
  if Negated then
    WriteStr(Stream,' NOT');
  WriteStr(Stream, ' LIKE ');
  SimpleExp.EmitSQL(Stream);
  if EscapeExp <> nil then begin
    WriteStr(Stream,' ESCAPE');
    EscapeExp.EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlLikeClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SimpleExp.EnumNodes(EnumMethod, Deep);
  if EscapeExp <> nil then
    EscapeExp.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlLikeClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlLikeClause)
    and (Negated = TffSqlLikeClause(Other).Negated)
    and (SimpleExp.Equals(TffSqlLikeClause(Other).SimpleExp))
    and (BothNil(EscapeExp, TffSqlLikeClause(Other).EscapeExp)
      or (BothNonNil(EscapeExp, TffSqlLikeClause(Other).EscapeExp)
        and EscapeExp.Equals(TffSqlLikeClause(Other).EscapeExp)));
end;
{--------}

{!!.11 new}
function TffSqlLikeClause.GetBmTable: PBTable;
var
  S: string;
begin
  if FBmTable = nil then begin
    Assert(IsBMCompatible);
    if IgnoreCase then                                                 {!!.13}
      S := AnsiUpperCase(SimpleExp.GetValue)                           {!!.13}
    else                                                               {!!.13}
      S := SimpleExp.GetValue;
    New(FBmTable);
    FBMPhrase := copy(S, 2, length(S) - 2);
    BMMakeTableS(FBmPhrase, FBmTable^);
  end;
  Result := FBmTable;
end;

function TffSqlLikeClause.GetHighLimit: string;
var
  i: Integer;
begin
  Result := GetLowLimit;
  i := length(Result);
  if Result[i] in [' '..'~'] then
    inc(Result[i])
  else
    Result := '';
end;

function TffSqlLikeClause.GetLowLimit: string;
var
  P : Integer;
begin
  Result := SimpleExp.GetValue;
  P := 1;
  while (P <= length(Result))
    and not (Result[P] in ['%', '_']) do
      inc(P);
  dec(P);
  if P < length(Result) then
    Result := copy(Result, 1 , P);
end;

{!!.11 new}
procedure TffSqlLikeClause.CheckBMCompat;
var
  S: string;
  Len,
  Inx : Integer;
begin
  FBMCompat := False;
  if SimpleExp.IsConstant and (EscapeExp = nil) then begin
    S := SimpleExp.GetValue;
    Len := Length(S);
    FBMCompat := (Len >= 3) and
                 (S[1] = '%') and
                 (S[Len] = '%');
    { Verify there is not another wildcard character in the middle of the
      string. }
    for Inx := 2 to Pred(Len) do
      if S[Inx] = '%' then begin
        FBMCompat := False;
        Break;
      end;
  end;
  BMCompatChecked := True;
end;

{!!.11 new}
function TffSqlLikeClause.IsBMCompatible: Boolean;
begin
  if not BMCompatChecked then
    CheckBMCompat;
  Result := FBMCompat;
end;

function TffSqlLikeClause.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
procedure TffSqlLikeClause.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftChar, fftWideChar,
  fftShortString..fftWideString :
    SimpleExp.MatchType(ExpectedType);
  fftBLOB..fftBLOBTypedBin :                                           {!!.11}
    SimpleExp.MatchType(fftNullAnsiStr);                               {!!.11}
  else
    SQLError(Format('The LIKE operator may not be applied to %s fields', {!!.11}
                    [FieldDataTypes[ExpectedType]]));                  {!!.11}
  end;
end;
{--------}
function TffSqlLikeClause.Reduce: Boolean;
begin
  Result := SimpleExp.Reduce or ((EscapeExp <> nil) and EscapeExp.Reduce);
end;

procedure TffSqlLikeClause.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{====================================================================}

{===TffSqlInClause===================================================}
function TffSqlInClause.AsBoolean(const TestValue: Variant): Boolean;
begin
  if SubQuery <> nil then
    Result := SubQuery.CheckForValue(TestValue)
  else
    Result := SimpleExpList.Contains(TestValue);
  Result := Result xor Negated;
end;
{--------}
procedure TffSqlInClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlInClause then begin
    SimpleExpList.Free;
    SimpleExpList := nil;                                              {!!.12}
    SubQuery.Free;
    SubQuery := nil;                                                   {!!.12}
    if TffSqlInClause(Source).SubQuery <> nil then begin
      SubQuery := TffSqlSELECT.Create(Self);
      SubQuery.Assign(TffSqlInClause(Source).SubQuery);
    end else begin
      SimpleExpList := TffSqlSimpleExpressionList.Create(Self);
      SimpleExpList.Assign(TffSqlInClause(Source).SimpleExpList);
    end;
    Negated := TffSqlInClause(Source).Negated;
  end else
    AssignError(Source);
end;

procedure TffSqlInClause.CheckIsConstant;
begin
  FIsConstantChecked := True;
  if SubQuery <> nil then
    FIsConstant := False
  else
    FIsConstant := SimpleExpList.IsConstant;
end;
{--------}
function TffSqlInClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if SubQuery <> nil then
    Result := SubQuery.DependsOn(Table)
  else
    Result := SimpleExpList.DependsOn(Table);
end;

destructor TffSqlInClause.Destroy;
begin
  SubQuery.Free;
  SimpleExpList.Free;
  inherited;
end;
{--------}
procedure TffSqlInClause.EmitSQL(Stream: TStream);
begin
  if Negated then
    WriteStr(Stream,' NOT');
  WriteStr(Stream, ' IN (');
  if SubQuery <> nil then
    SubQuery.EmitSQL(Stream)
  else
    SimpleExpList.EmitSQL(Stream);
  WriteStr(Stream, ') ');
end;
{--------}
procedure TffSqlInClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if SubQuery <> nil then
    SubQuery.EnumNodes(EnumMethod, Deep)
  else
    SimpleExpList.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlInClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlInClause)
    and (Negated = TffSqlInClause(Other).Negated);
  if Result then
    if SubQuery <> nil then
      if TffSqlInClause(Other).SubQuery = nil then
        Result := False
      else
        Result := SubQuery.Equals(TffSqlInClause(Other).SubQuery)
    else
      if TffSqlInClause(Other).SimpleExpList = nil then
        Result := False
      else
        Result := SimpleExpList.Equals(TffSqlInClause(Other).SimpleExpList);
end;
{--------}
function TffSqlInClause.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

procedure TffSqlInClause.MatchType(ExpectedType: TffFieldType);
begin
  if SubQuery <> nil then
    SubQuery.MatchType(ExpectedType, True)
  else
    SimpleExpList.MatchType(ExpectedType);
end;
{--------}
function TffSqlInClause.Reduce: Boolean;
begin
  if SubQuery <> nil then
    Result := SubQuery.Reduce
  else
    Result := SimpleExpList.Reduce;
end;

procedure TffSqlInClause.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{====================================================================}

function SimpleCompare(RelOp: TffSqlRelOp; const Val1, Val2: Variant): Boolean;
const
  ValIsBLOBArray : array[boolean, boolean] of Byte =
    ( (1,  { false, false }
       2), { false, true  }
      (3,  { true,  false }
       4)  { true,  true  }
    );
var
  VStr : string;
  VPtr1, VPtr2 : PAnsiChar;
  Inx, VPtr1Len, VPtr2Len : Integer;
  VPtr1Locked, VPtr2Locked : Boolean;
  ValIsBLOBCase : Byte;
begin
  if VarIsNull(Val1) or
     VarIsNull(Val2) then begin
    Result := False;
    Exit;
  end;
  Assert(RelOp <> roNone);

  ValIsBLOBCase := ValIsBLOBArray[VarIsArray(Val1) and
                                  (TVarData(Val1).VType and VarTypeMask = varByte),
                                  VarIsArray(Val2) and
                                  (TVarData(Val2).VType and VarTypeMask  = varByte)];
  if ValIsBLOBCase = 1 then
    case RelOp of
      roEQ :
        if (VarType(Val1) and VarTypeMask = VarDate)
        and (VarType(Val2) and VarTypeMask = VarDate) then
          Result := abs(double(Val1) - double(Val2)) < TimeDelta
        else
          Result := Val1 = Val2;
      roLE :
        Result := Val1 <= Val2;
      roL :
        Result := Val1 < Val2;
      roG :
        Result := Val1 > Val2;
      roGE :
        Result := Val1 >= Val2;
      else//roNE :
        if (VarType(Val1) and VarTypeMask = VarDate)
        and (VarType(Val2) and VarTypeMask = VarDate) then
          Result := abs(double(Val1) - double(Val2)) >= TimeDelta
        else
          Result := Val1 <> Val2;
    end  { case }
  else begin
    { One of the parameters is a BLOB. It must be converted to a string.
      This code is kind of flaky in that it is a duplicate of the preceding
      section. However, this approach should give us optimal performance for
      cases where neither parameter is a BLOB. }
    VPtr1 := nil;
    VPtr2 := nil;
    VPtr1Locked := False;
    VPtr2Locked := False;
    try
      case ValIsBLOBCase of
        2 : begin
              VStr := VarToStr(Val1);
              VPtr1 := PAnsiChar(VStr);
              VPtr1Len := Length(VStr);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            end;
        3 : begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VStr := VarToStr(Val2);
              VPtr2 := PAnsiChar(VStr);
              VPtr2Len := Length(VStr);
            end;
        4 : begin
              VPtr1 := VarArrayLock(Val1);
              VPtr1Locked := True;
              VPtr1Len := VarArrayHighBound(Val1, 1);
              VPtr2 := VarArrayLock(Val2);
              VPtr2Locked := True;
              VPtr2Len := VarArrayHighBound(Val2, 1);
            end;
        else begin
          VPtr1Len := 0;
          VPtr2Len := 0;
        end;
      end;  { case }
      Inx := Windows.CompareStringA(LOCALE_USER_DEFAULT, 0,
                                    VPtr1, VPtr1Len, VPtr2, VPtr2Len) - 2;
      case RelOp of
        roEQ : Result := (Inx = 0);
        roLE : Result := (Inx <= 0);
        roL  : Result := (Inx < 0);
        roG  : Result := (Inx > 0);
        roGE : Result := (Inx >= 0);
        else
          { roNE }
          Result := (Inx <> 0);
      end;  { case }
    finally
      if VPtr1Locked then
        VarArrayUnlock(Val1);
      if VPtr2Locked then
        VarArrayUnlock(Val2);
    end;
  end;  { if..else }
end;

{===TffSqlCondPrimary================================================}
function TffSqlCondPrimary.AsBoolean: Boolean;
var
  F: TffSqlFieldProxy;                                                 {!!.11}
  BMTable: PBTable;                                                    {!!.13}
begin
  Result := False;
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  if not TypeChecked then
    CheckType;

  if RelOp = roNone then
    if BetweenClause <> nil then
      Result := BetweenClause.AsBoolean(SimpleExp1.GetValue)
    else
    if LikeClause <> nil then
      if SimpleExp1.IsField(F) and LikeClause.IsBMCompatible then begin     {!!.11}{!!.13}
        {Need to call BMTable before method call - otherwise BMPhrase doesn't get initialized in time}
        BMTable := LikeClause.BMTable;
        Result := F.BMMatch(BMTable^, LikeClause.BMPhrase, LikeClause.IgnoreCase)  {!!.11}{!!.13}
      end else                                                             {!!.11}{!!.13}
        Result := LikeClause.AsBoolean(SimpleExp1.GetValue)
    else
    if InClause <> nil then
      Result := InClause.AsBoolean(SimpleExp1.GetValue)
    else
    if IsTest <> nil then
      Result := IsTest.Evaluate(SimpleExp1)
    else
    if ExistsClause <> nil then
      Result := ExistsClause.AsBoolean
    else
    if UniqueClause <> nil then
      Result := UniqueClause.AsBoolean
    else
    if MatchClause <> nil then
      Result := MatchClause.AsBoolean(SimpleExp1.GetValue)
    else
      Result := SimpleExp1.GetValue
  else
    if SimpleExp2 <> nil then
      Result := SimpleCompare(RelOp, SimpleExp1.GetValue, SimpleExp2.GetValue)
    else
    if AllOrAnyClause <> nil then
      Result := AllOrAnyClause.Compare(RelOp, SimpleExp1.GetValue)
    else
      SQLError('Simple expression or ANY/ALL clause expected');
end;
{--------}
procedure TffSqlCondPrimary.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlCondPrimary then begin

    Clear;

    if assigned(TffSqlCondPrimary(Source).SimpleExp1) then begin
      SimpleExp1 := TffSqlSimpleExpression.Create(Self);
      SimpleExp1.Assign(TffSqlCondPrimary(Source).SimpleExp1);
    end;

    RelOp := TffSqlCondPrimary(Source).RelOp;

    if assigned(TffSqlCondPrimary(Source).SimpleExp2) then begin
      SimpleExp2 := TffSqlSimpleExpression.Create(Self);
      SimpleExp2.Assign(TffSqlCondPrimary(Source).SimpleExp2);
    end;

    if assigned(TffSqlCondPrimary(Source).BetweenClause) then begin
      BetweenClause := TffSqlBetweenClause.Create(Self);
      BetweenClause.Assign(TffSqlCondPrimary(Source).BetweenClause);
    end;

    if assigned(TffSqlCondPrimary(Source).LikeClause) then begin
      LikeClause := TffSqlLikeClause.Create(Self);
      LikeClause.Assign(TffSqlCondPrimary(Source).LikeClause);
    end;

    if assigned(TffSqlCondPrimary(Source).InClause) then begin
      InClause := TffSqlInClause.Create(Self);
      InClause.Assign(TffSqlCondPrimary(Source).InClause);
    end;

    if assigned(TffSqlCondPrimary(Source).IsTest) then begin
      IsTest := TffSqlIsTest.Create(Self);
      IsTest.Assign(TffSqlCondPrimary(Source).IsTest);
    end;

    if assigned(TffSqlCondPrimary(Source).AllOrAnyClause) then begin
      AllOrAnyClause := TffSqlAllOrAnyClause.Create(Self);
      AllOrAnyClause.Assign(TffSqlCondPrimary(Source).AllOrAnyClause);
    end;

    if assigned(TffSqlCondPrimary(Source).ExistsClause) then begin
      ExistsClause := TffSqlExistsClause.Create(Self);
      ExistsClause.Assign(TffSqlCondPrimary(Source).ExistsClause);
    end;

    if assigned(TffSqlCondPrimary(Source).UniqueClause) then begin
      UniqueClause := TffSqlUniqueClause.Create(Self);
      UniqueClause.Assign(TffSqlCondPrimary(Source).UniqueClause);
    end;

    if assigned(TffSqlCondPrimary(Source).MatchClause) then begin
      MatchClause := TffSqlMatchClause.Create(Self);
      MatchClause.Assign(TffSqlCondPrimary(Source).MatchClause);
    end;

  end else
    AssignError(Source);
end;
{--------}
procedure TffSqlCondPrimary.BindHaving;
begin
  if SimpleExp1 <> nil then
    SimpleExp1.BindHaving;
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      SQLError('BETWEEN not supported in a HAVING clause')
    else
    if LikeClause <> nil then
      SQLError('LIKE not supported in a HAVING clause')
    else
    if InClause <> nil then
      SQLError('IN not supported in a HAVING clause')
    else
    {if IsTest <> nil then
      SQLError('IS not supported in a HAVING clause')
    else}                                                              {!!.11}
    if ExistsClause <> nil then
      SQLError('EXISTS not supported in a HAVING clause')
    else
    if UniqueClause <> nil then
      SQLError('UNIQUE not supported in a HAVING clause')
    else
    if MatchClause <> nil then
      SQLError('MATCH not supported in a HAVING clause');
  else
    if AllOrAnyClause <> nil then
      //SQLError('ANY or ALL conditions not supported in a HAVING clause')
    else begin
      Assert(SimpleExp2 <> nil);
      SimpleExp2.BindHaving;
    end;
  end;
end;

procedure TffSqlCondPrimary.CheckIsConstant;
begin
  FIsConstantChecked := True;
  FIsConstant := False;
  if SimpleExp1 <> nil then
    if not SimpleExp1.IsConstant then
        exit;
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      if not BetweenClause.IsConstant then
        exit
      else
    else
    if LikeClause <> nil then
      if not LikeClause.IsConstant then
        exit
      else
    else
    if InClause <> nil then
      if not InClause.IsConstant then
        exit
      else
    else
    if IsTest <> nil then
      // constant by definition
    else
    if ExistsClause <> nil then
      exit
    else
    if UniqueClause <> nil then
      exit
    else
    if MatchClause <> nil then
      exit;
  else
    if AllOrAnyClause <> nil then
      exit
    else begin
      Assert(SimpleExp2 <> nil);
      if not SimpleExp2.IsConstant then
        exit;
    end;
  end;
  ConstantValue := GetValue;
  FIsConstant := True;
end;

procedure TffSqlCondPrimary.CheckType;
var
  T1 : TffFieldType;
begin
  if SimpleExp1 <> nil then
    T1 := SimpleExp1.GetType
  else
    T1 := fftBLOB; {anything that doesn't match a valid SQL type}
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      BetweenClause.MatchType(T1)
    else
    if LikeClause <> nil then
      LikeClause.MatchType(T1)
    else
    if InClause <> nil then
      InClause.MatchType(T1)
    else
    if IsTest <> nil then
      IsTest.MatchType(T1)
    else
    if ExistsClause <> nil then
      //T1 := ExistsClause.GetType
    else
    if UniqueClause <> nil then
      //T1 := UniqueClause.GetType
    else
    if MatchClause <> nil then
      MatchClause.MatchType(T1);
    //else
    //  if T1 <> fftBoolean then
    //    TypeMismatch;
  else
    if AllOrAnyClause <> nil then
      AllOrAnyClause.MatchType(T1)
    else begin
      Assert(SimpleExp2 <> nil);
      SimpleExp2.MatchType(T1);
    end;
  end;
  TypeChecked := True;
end;
{--------}
procedure TffSqlCondPrimary.Clear;
begin
  SimpleExp1.Free;
  SimpleExp1 := nil;
  BetweenClause.Free;
  BetweenClause := nil;
  LikeClause.Free;
  LikeClause := nil;
  InClause.Free;
  InClause := nil;
  IsTest.Free;
  IsTest := nil;
  ExistsClause.Free;
  ExistsClause := nil;
  UniqueClause.Free;
  UniqueClause := nil;
  MatchClause.Free;
  MatchClause := nil;
  AllOrAnyClause.Free;
  AllOrAnyClause := nil;
  SimpleExp2.Free;
  SimpleExp2 := nil;
  inherited;
end;
{--------}
function TffSqlCondPrimary.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := False;
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      Result := SimpleExp1.DependsOn(Table) or BetweenClause.DependsOn(Table)
    else
    if LikeClause <> nil then
      Result := SimpleExp1.DependsOn(Table) or LikeClause.DependsOn(Table)
    else
    if InClause <> nil then
      Result := SimpleExp1.DependsOn(Table) or InClause.DependsOn(Table)
    else
    if IsTest <> nil then
      Result := SimpleExp1.DependsOn(Table)
    else
    if ExistsClause <> nil then
      Result := ExistsClause.DependsOn(Table)
    else
    if UniqueClause <> nil then
      Result := UniqueClause.DependsOn(Table)
    else
    if MatchClause <> nil then
      Result := SimpleExp1.DependsOn(Table) or MatchClause.DependsOn(Table)
    else
      Result := SimpleExp1.DependsOn(Table);
  else //roEQ, roLE, roL, roG, roGE, roNE :
    if SimpleExp2 <> nil then
      Result := SimpleExp1.DependsOn(Table) or SimpleExp2.DependsOn(Table)
    else
    if AllOrAnyClause <> nil then
      Result := SimpleExp1.DependsOn(Table) or AllOrAnyClause.DependsOn(Table)
    else
      SQLError('Simple expression or ANY/ALL clause expected');
  end;
  if AllOrAnyClause <> nil then
    Result := Result or AllOrAnyClause.DependsOn(Table);
end;

destructor TffSqlCondPrimary.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlCondPrimary.EmitSQL(Stream: TStream);
begin
  if SimpleExp1 <> nil then
    SimpleExp1.EmitSQL(Stream);
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      BetweenClause.EmitSQL(Stream)
    else
    if LikeClause <> nil then
      LikeClause.EmitSQL(Stream)
    else
    if InClause <> nil then
      InClause.EmitSQL(Stream)
    else
    if IsTest <> nil then
      IsTest.EmitSQL(Stream)
    else
    if ExistsClause <> nil then
      ExistsClause.EmitSQL(Stream)
    else
    if UniqueClause <> nil then
      UniqueClause.EmitSQL(Stream)
    else
    if MatchClause <> nil then
      MatchClause.EmitSQL(Stream);
  else
    WriteStr(Stream,' ');
    WriteStr(Stream, RelOpStr[RelOp]);
    WriteStr(Stream,' ');
    if AllOrAnyClause <> nil then
      AllOrAnyClause.EmitSQL(Stream)
    else
      SimpleExp2.EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlCondPrimary.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if SimpleExp1 <> nil then
    SimpleExp1.EnumNodes(EnumMethod, Deep);
  case RelOp of
  roNone :
    if BetweenClause <> nil then
      BetweenClause.EnumNodes(EnumMethod, Deep)
    else
    if LikeClause <> nil then
      LikeClause.EnumNodes(EnumMethod, Deep)
    else
    if InClause <> nil then
      InClause.EnumNodes(EnumMethod, Deep)
    else
    if IsTest <> nil then
      IsTest.EnumNodes(EnumMethod, Deep)
    else
    if MatchClause <> nil then
      MatchClause.EnumNodes(EnumMethod, Deep)
    else
    if ExistsClause <> nil then
      ExistsClause.EnumNodes(EnumMethod, Deep)
    else
    if UniqueClause <> nil then
      UniqueClause.EnumNodes(EnumMethod, Deep);
  else
    if SimpleExp2 <> nil then
      SimpleExp2.EnumNodes(EnumMethod, Deep)
    else
      if AllOrAnyClause <> nil then
        AllOrAnyClause.EnumNodes(EnumMethod, Deep);
  end;
end;
{--------}
function TffSqlCondPrimary.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlCondPrimary)
    and (RelOp = TffSqlCondPrimary(Other).RelOp)
    and (
      BothNil(SimpleExp1, TffSqlCondPrimary(Other).SimpleExp1)
      or (
        BothNonNil(SimpleExp1, TffSqlCondPrimary(Other).SimpleExp1)
        and SimpleExp1.Equals(TffSqlCondPrimary(Other).SimpleExp1)
       )
      )
    and (
      BothNil(SimpleExp2, TffSqlCondPrimary(Other).SimpleExp2)
      or (
        BothNonNil(SimpleExp2, TffSqlCondPrimary(Other).SimpleExp2)
        and SimpleExp2.Equals(TffSqlCondPrimary(Other).SimpleExp2)
       )
      )
    and (
      BothNil(BetweenClause, TffSqlCondPrimary(Other).BetweenClause)
      or (
        BothNonNil(BetweenClause, TffSqlCondPrimary(Other).BetweenClause)
        and BetweenClause.Equals(TffSqlCondPrimary(Other).BetweenClause)
       )
      )
    and (
      BothNil(LikeClause, TffSqlCondPrimary(Other).LikeClause)
      or (
        BothNonNil(LikeClause, TffSqlCondPrimary(Other).LikeClause)
        and LikeClause.Equals(TffSqlCondPrimary(Other).LikeClause)
       )
      )
    and (
      BothNil(InClause, TffSqlCondPrimary(Other).InClause)
      or (
        BothNonNil(InClause, TffSqlCondPrimary(Other).InClause)
        and InClause.Equals(TffSqlCondPrimary(Other).InClause)
       )
      )
    and (
      BothNil(IsTest, TffSqlCondPrimary(Other).IsTest)
      or (
        BothNonNil(IsTest, TffSqlCondPrimary(Other).IsTest)
        and IsTest.Equals(TffSqlCondPrimary(Other).IsTest)
       )
      )
    and (
      BothNil(AllOrAnyClause, TffSqlCondPrimary(Other).AllOrAnyClause)
      or (
        BothNonNil(AllOrAnyClause, TffSqlCondPrimary(Other).AllOrAnyClause)
        and AllOrAnyClause.Equals(TffSqlCondPrimary(Other).AllOrAnyClause)
       )
      )
    and (
      BothNil(ExistsClause, TffSqlCondPrimary(Other).ExistsClause)
      or (
        BothNonNil(ExistsClause, TffSqlCondPrimary(Other).ExistsClause)
        and ExistsClause.Equals(TffSqlCondPrimary(Other).ExistsClause)
       )
      )
    and (
      BothNil(MatchClause, TffSqlCondPrimary(Other).MatchClause)
      or (
        BothNonNil(MatchClause, TffSqlCondPrimary(Other).MatchClause)
        and MatchClause.Equals(TffSqlCondPrimary(Other).MatchClause)
       )
      )
    and (
      BothNil(UniqueClause, TffSqlCondPrimary(Other).UniqueClause)
      or (
        BothNonNil(UniqueClause, TffSqlCondPrimary(Other).UniqueClause)
        and UniqueClause.Equals(TffSqlCondPrimary(Other).UniqueClause)
       )
      );
end;
{--------}
function TffSqlCondPrimary.GetDecimals: Integer;
begin
  if SimpleExp1 <> nil then
    Result := SimpleExp1.GetDecimals
  else
    Result := 0;
end;
{--------}
function TffSqlCondPrimary.GetSize: Integer;
begin
  case RelOp of
  roNone :
    Result := SimpleExp1.GetSize
  else
    Result := 1;
  end;
end;
{--------}
function TffSqlCondPrimary.GetTitle(const Qualified : Boolean): string; {!!.11}
begin
  case GetType of
  fftBoolean:
    Result := 'COND'
  else
    Result := SimpleExp1.GetTitle(Qualified);                        {!!.11}
  end;
end;
{--------}
function TffSqlCondPrimary.GetType: TffFieldType;
begin
  if SimpleExp1 <> nil then
    Result := SimpleExp1.GetType
  else
    Result := fftBoolean; {should never happen}
  case RelOp of
  roNone :
    if (BetweenClause <> nil)
    or (LikeClause <> nil)
    or (InClause <> nil)
    or (IsTest <> nil)
    or (MatchClause <> nil) then
      Result := fftBoolean;
  else
    if SimpleExp2 <> nil then
      SimpleExp2.MatchType(Result);
    Result := fftBoolean;
  end;
end;
{--------}
function TffSqlCondPrimary.GetValue: Variant;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  case GetType of
  fftBoolean:
    Result := AsBoolean
  else
    Result := SimpleExp1.GetValue;
  end;
end;
{--------}
function TffSqlCondPrimary.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
function TffSqlCondPrimary.IsRelationTo(Table: TFFSqlTableProxy;
      var FieldReferenced: TFFSqlFieldProxy;
      var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): Boolean; {!!.10}
begin
  ArgExpression := nil;
  case RelOp of
  roEQ, roLE, roL, roG, roGE, roNE :
    begin
      if SimpleExp2 <> nil then
        if SimpleExp1.IsFieldFrom(Table, FieldReferenced, SameCase) then begin
          Result := True;
          ArgExpression := SimpleExp2;
        end else
        if SimpleExp2.IsFieldFrom(Table, FieldReferenced, SameCase) then begin
          Result := True;
          ArgExpression := SimpleExp1;
        end else
          Result := False
      else {typically ANY or ALL relation}
        Result := False;
    end;
  else
    Result := False;
  end;
  if AllOrAnyClause <> nil then
    Result := False;
  Operator := RelOp;
end;
{--------}
function TffSqlCondPrimary.JustSimpleExpression: Boolean;
begin
  Result := (RelOp = roNone)
   and (BetweenClause = nil)
   and (LikeClause = nil)
   and (InClause = nil)
   and (IsTest = nil)
   and (ExistsClause = nil)
   and (UniqueClause = nil)
   and (MatchClause = nil);
end;

{!!.11 new}
procedure TffSqlCondPrimary.MatchType(ExpectedType: TffFieldType);
begin
  case RelOp of
  roNone :
    if (BetweenClause <> nil)
    or (LikeClause <> nil)
    or (InClause <> nil)
    or (IsTest <> nil)
    or (ExistsClause <> nil)                                           {!!.11}
    or (MatchClause <> nil) then
      if ExpectedType <> fftBoolean then
        TypeMismatch
      else
    else
      SimpleExp1.MatchType(ExpectedType);
  else
    if SimpleExp2 <> nil then begin
      SimpleExp2.MatchType(SimpleExp1.GetType);
      if ExpectedType <> fftBoolean then
        TypeMismatch;
    end;
  end;
end;

function TffSqlCondPrimary.Reduce: Boolean;
begin
  Result := True;
  if (SimpleExp1 <> nil) and SimpleExp1.Reduce then
    exit;
  if (SimpleExp2 <> nil) and SimpleExp2.Reduce then
    exit;
  if (BetweenClause <> nil) and BetweenClause.Reduce then
    exit;
  if (LikeClause <> nil) and LikeClause.Reduce then
    exit;
  if (InClause <> nil) and InClause.Reduce then
    exit;
  if (ExistsClause <> nil) and ExistsClause.Reduce then
    exit;
  if (UniqueClause <> nil) and UniqueClause.Reduce then
    exit;
  if (MatchClause <> nil) and MatchClause.Reduce then
    exit;
  if (AllOrAnyClause <> nil) and AllOrAnyClause.Reduce then
    exit;
  Result := False;
end;

procedure TffSqlCondPrimary.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

{====================================================================}

{===TffSqlCondFactor=================================================}
function TffSqlCondFactor.AsBoolean: Boolean;
begin
  if TmpKnown then begin
    Result := TmpValue;
    exit;
  end;
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  Result := CondPrimary.AsBoolean;
  if UnaryNot then
    Result := not Result;
end;
{--------}
procedure TffSqlCondFactor.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlCondFactor then begin
    if CondPrimary = nil then
      CondPrimary := TffSqlCondPrimary.Create(Self);
    CondPrimary.Assign(TffSqlCondFactor(Source).CondPrimary);
    UnaryNot := TffSqlCondFactor(Source).UnaryNot;
  end else
    AssignError(Source);
end;

procedure TffSqlCondFactor.BindHaving;
begin
  CondPrimary.BindHaving;
end;

procedure TffSqlCondFactor.CheckIsConstant;
begin
  FIsConstantChecked := True;
  if CondPrimary.IsConstant then begin
    ConstantValue := GetValue;
    FIsConstant := True;
  end;
end;

procedure TffSqlCondFactor.Clear;
begin
  if CondPrimary <> nil then
    CondPrimary.Clear;
end;

function TffSqlCondFactor.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := CondPrimary.DependsOn(Table);
end;

destructor TffSqlCondFactor.Destroy;
begin
  CondPrimary.Free;
  inherited;
end;

procedure TffSqlCondFactor.EmitSQL(Stream: TStream);
begin
  if UnaryNot then
    WriteStr(Stream,' NOT');
  CondPrimary.EmitSQL(Stream);
end;
{--------}
procedure TffSqlCondFactor.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  CondPrimary.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlCondFactor.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlCondFactor)
    and (UnaryNot = TffSqlCondFactor(Other).UnaryNot)
    and (CondPrimary.Equals(TffSqlCondFactor(Other).CondPrimary));
end;
{--------}
function TffSqlCondFactor.GetDecimals: Integer;
begin
  Result := CondPrimary.GetDecimals;
end;
{--------}
{!!.10}
function TffSqlCondFactor.GetSize: Integer;
begin
  if UnaryNot then
    Result := 1
  else
    Result := CondPrimary.GetSize;
end;
{--------}
function TffSqlCondFactor.GetTitle(const Qualified : Boolean): string; {!!.11}
begin
  Result := CondPrimary.GetTitle(Qualified);                           {!!.11}
end;
{--------}
function TffSqlCondFactor.GetType: TffFieldType;
begin
  if UnaryNot then
    Result := fftBoolean
  else
    Result := CondPrimary.GetType;
end;
{--------}
function TffSqlCondFactor.GetValue: Variant;
begin
  if TmpKnown then begin
    Result := TmpValue;
    exit;
  end;
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  if UnaryNot then
    Result := AsBoolean
  else
    Result := CondPrimary.GetValue;
end;
{--------}
function TffSqlCondFactor.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
function TffSqlCondFactor.IsRelationTo(Table: TFFSqlTableProxy;
  var FieldReferenced: TFFSqlFieldProxy;
  var Operator: TffSqlRelOp;
      var ArgExpression: TffSqlSimpleExpression;
      var SameCase: Boolean): Boolean;
begin
  ArgExpression := nil;
  Result := CondPrimary.IsRelationTo(Table, FieldReferenced,
    Operator, ArgExpression, SameCase)
    and not ArgExpression.DependsOn(Table);
  if Result and UnaryNot then
    case Operator of
    roNone : ;
    roEQ : Operator := roNE;
    roLE : Operator := roG;
    roL : Operator := roGE;
    roG : Operator := roLE;
    roGE : Operator := roL;
    roNE : Operator := roEQ;
    end;
end;
{--------}
procedure TffSqlCondFactor.MarkTrue;
begin
  TmpKnown := True;
  TmpValue := True;
end;
{--------}
procedure TffSqlCondFactor.MarkUnknown;
begin
  TmpKnown := False;
end;
{--------}
{!!.11 - new}
procedure TffSqlCondFactor.MatchType(ExpectedType: TffFieldType);
begin
  if UnaryNot then
    if ExpectedType <> fftBoolean then
      TypeMismatch
    else
  else
    CondPrimary.MatchType(ExpectedType);
end;
{--------}
function TffSqlCondFactor.Reduce: Boolean;
var
  LiftPrimary : TffSqlCondPrimary;
  NewExp: TffSqlSimpleExpression;
  NewTerm: TffSqlTerm;
  NewFactor: TffSqlFactor;
  NewCondExp : TffSqlCondExp;
  NewCondTerm: TffSqlCondTerm;
  NewCondFactor : TffSqlCondFactor;
  NewCondPrimary : TffSqlCondPrimary;
begin
  {look for a conditional primary nested inside redundant parens}
  {eliminate parens when found}
  Result := False;
  LiftPrimary := nil;
  if (CondPrimary.RelOp = roNone) then
    with CondPrimary do begin
      //if SimpleExp1 <> nil then begin
      if JustSimpleExpression then begin
        with SimpleExp1 do
          if TermCount  = 1 then
            with Term[0] do
              if FactorCount = 1 then
                with Factor[0] do
                  if CondExp <> nil then
                    with CondExp do
                      if CondTermCount = 1 then
                        with CondTerm[0] do
                          if CondFactorCount = 1 then
                            with CondFactor[0] do begin
                             LiftPrimary := TffSqlCondPrimary.Create(Self);
                             LiftPrimary.Assign(CondPrimary);
                            end;
        if LiftPrimary <> nil then begin
          Clear;
          Assign(LiftPrimary);
          LiftPrimary.Free;
          Result := True;
        end else
        if Reduce then begin
          {expression itself was reduced}
          Result := True;
        end;
      end;
      if not Result then
        Result := Reduce;
    end;
  if not Result then begin {otherwise we'll be called again}
    {see if this a negated simple expression which can be reversed}
    if UnaryNot and (CondPrimary.RelOp <> roNone) then begin
      {it is, reverse condition and remove NOT}
      case CondPrimary.RelOp of
      roEQ : CondPrimary.RelOp := roNE;
      roLE : CondPrimary.RelOp := roG;
      roL : CondPrimary.RelOp := roGE;
      roG : CondPrimary.RelOp := roLE;
      roGE: CondPrimary.RelOp := roL;
      roNE : CondPrimary.RelOp := roEQ;
      end;
      UnaryNot := False;
      Result := True;
    end;
  end;
  if not Result then {otherwise we'll be called again}
    if (CondPrimary.RelOp = roNE) { "<>" operator }
    {can't optimize ALL/ANY clauses this way}
    and (CondPrimary.AllOrAnyClause = nil) then                        {!!.11}
      if CondPrimary.SimpleExp1.HasFieldRef
      or CondPrimary.SimpleExp2.HasFieldRef then begin
        {convert expressions of the form
          Simple Exp1 <> Simple Exp2
         where at least one expression contains a field reference
         to
          (Simple Exp1 < Simple Exp2 OR Simple Exp1 > Simple Exp2)
         to allow for index optimization later on}
        NewExp := TffSqlSimpleExpression.Create(CondPrimary);
        NewTerm := TffSqlTerm.Create(NewExp);
        NewFactor := TffSqlFactor.Create(NewTerm);
        NewCondExp := TffSqlCondExp.Create(NewFactor);

        NewCondTerm := TffSqlCondTerm.Create(NewCondExp);
        NewCondFactor := TffSqlCondFactor.Create(NewCondTerm);
        NewCondPrimary := TffSqlCondPrimary.Create(NewCondFactor);
        NewCondPrimary.Assign(CondPrimary);
        NewCondPrimary.RelOp := roL;
        NewCondFactor.CondPrimary := NewCondPrimary;
        NewCondTerm.AddCondFactor(NewCondFactor);
        NewCondExp.AddCondTerm(NewCondTerm);

        NewCondTerm := TffSqlCondTerm.Create(NewCondExp);
        NewCondFactor := TffSqlCondFactor.Create(NewCondTerm);
        NewCondPrimary := TffSqlCondPrimary.Create(NewCondFactor);
        NewCondPrimary.Assign(CondPrimary);
        NewCondPrimary.RelOp := roG;
        NewCondFactor.CondPrimary := NewCondPrimary;
        NewCondTerm.AddCondFactor(NewCondFactor);
        NewCondExp.AddCondTerm(NewCondTerm);

        NewFactor.CondExp := NewCondExp;
        NewTerm.AddFactor(NewFactor);
        NewExp.AddTerm(NewTerm);

        CondPrimary.SimpleExp2.Free;
        CondPrimary.SimpleExp2 := nil;
        CondPrimary.RelOp := roNone;
        CondPrimary.SimpleExp1.Assign(NewExp);
        NewExp.Free;
        Result := True;
      end;
  if not Result then                                                   {!!.11}
    Result := CondPrimary.Reduce;                                      {!!.11}
end;
{--------}
procedure TffSqlCondFactor.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{====================================================================}

{===TffSqlFloatLiteral===============================================}
procedure TffSqlFloatLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlFloatLiteral then begin
    Value := TffSqlFloatLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlFloatLiteral.ConvertToNative;
var
  Code : Integer;
begin
  case GetType of
  fftSingle :
    Val(Value, SingleValue, Code);
  fftDouble :
    Val(Value, DoubleValue, Code);
  fftExtended :
    Val(Value, ExtendedValue, Code);
  fftComp :
    Val(Value, Comp(CompValue), Code);
  fftCurrency :
    begin
      FFValCurr(Value, CurrencyValue, Code);
    end;
  end;
  Converted := Code = 0;
end;

procedure TffSqlFloatLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlFloatLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlFloatLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlFloatLiteral)
    and (AnsiCompareText(Value, TffSqlFloatLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlFloatLiteral.GetDecimals: Integer;
begin
  Result := 2;
end;
{--------}
function TffSqlFloatLiteral.GetType: TffFieldType;
begin
  Result := fftDouble;
end;
{--------}
function TffSqlFloatLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  case GetType of
  fftSingle :
    Result := SingleValue;
  fftDouble :
    Result := DoubleValue;
  fftExtended :
    Result := ExtendedValue;
  fftComp :
    Result := Comp(CompValue);
  fftCurrency :
    Result := CurrencyValue;
  end;
end;
{--------}
procedure TffSqlFloatLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftByte..fftAutoInc :
    ;
  fftSingle..fftCurrency :
    ;
  else
    TypeMismatch;
  end;
end;

{====================================================================}

{===TffSqlIntegerLiteral=============================================}
procedure TffSqlIntegerLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlIntegerLiteral then begin
    Value := TffSqlIntegerLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlIntegerLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlIntegerLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlIntegerLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlIntegerLiteral)
    and (AnsiCompareText(Value, TffSqlFloatLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlIntegerLiteral.GetType: TffFieldType;
begin
  Result := fftInt32;
end;

procedure TffSqlIntegerLiteral.ConvertToNative;
begin
  Int32Value := StrToInt(Value);
  Converted := True;
end;

function TffSqlIntegerLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  Result := Int32Value;
end;
{--------}
procedure TffSqlIntegerLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftByte..fftCurrency :
    ;
  fftShortString..fftWideString :
    ;
  else
    TypeMismatch;
  end;
end;

{====================================================================}

{===TffSqlStringLiteral==============================================}
procedure TffSqlStringLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlStringLiteral then begin
    Value := TffSqlStringLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlStringLiteral.ConvertToNative;
var
  S : string;
  P : Integer;
begin
  S := copy(Value, 2, length(Value) - 2); //strip quotes
  {convert internal double-quotes to single quotes}
  P := pos('''''', S);
  while P <> 0 do begin
    Delete(S, P, 1);
    P := pos('''''', S);
  end;
  Assert(GetType in [fftChar, fftWideChar,
                     fftShortString..fftWideString]);
  case GetType of
  fftChar :
    CharValue := S[1];
  fftWideChar :
    WideCharValue := WideChar(S[1]);
  fftShortString :
    ShortStringValue := S;
  fftShortAnsiStr :
    ShortAnsiStringValue := S;
  fftNullString :
    NullStringValue := PChar(S);
  fftNullAnsiStr :
    NullAnsiStrValue := PChar(S);
  fftWideString :
    WideStringValue := S;
  end;
  Converted := True;
end;
{--------}
constructor TffSqlStringLiteral.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FType := fftNullAnsiStr;                                             {!!.11}
end;
{--------}
procedure TffSqlStringLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlStringLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlStringLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlStringLiteral)
    and (AnsiCompareText(Value, TffSqlStringLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlStringLiteral.GetSize: Integer;
begin
  if not Converted then
    ConvertToNative;
  Assert(GetType in [fftChar..fftWideString]);
  case GetType of
  fftChar :
    Result := 1;
  fftWideChar :
    Result := 2;
  fftShortString :
    Result := length(ShortStringValue);
  fftShortAnsiStr :
    Result := length(ShortAnsiStringValue);
  fftNullString :
    Result := length(NullStringValue{^});
  fftNullAnsiStr :
    Result := length(NullAnsiStrValue);
  else //fftWideString :
    Result := length(WideStringValue);
  end;
end;
{--------}
function TffSqlStringLiteral.GetType: TffFieldType;
begin
  Result := FType;
end;
{--------}
function TffSqlStringLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  Assert(GetType in [fftChar..fftWideString]);
  case GetType of
  fftChar :
    Result := CharValue;
  fftWideChar :
    Result := WideCharValue;
  fftShortString :
    Result := ShortStringValue;
  fftShortAnsiStr :
    Result := ShortAnsiStringValue;
  fftNullString :
    Result := NullStringValue{^};
  fftNullAnsiStr :
    Result := NullAnsiStrValue;
  fftWideString :
    Result := WideStringValue;
  end;
end;
{--------}
procedure TffSqlStringLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftChar,
  fftWideChar,
  fftShortString..fftWideString :
    begin
      FType := ExpectedType;
      Converted := False;
    end;
{Begin !!.11}
  fftBLOB..fftBLOBTypedBin :
    begin
      FType := fftNullAnsiStr;
      Converted := False;
    end;
{End !!.11}
  else
    TypeMismatch;
  end;
end;

{====================================================================}

{===TffSqlLiteral====================================================}
procedure TffSqlLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlLiteral then begin
    Clear;

    if assigned(TffSqlLiteral(Source).FloatLiteral) then begin
      FloatLiteral := TffSqlFloatLiteral.Create(Self);
      FloatLiteral.Assign(TffSqlLiteral(Source).FloatLiteral);
    end;

    if assigned(TffSqlLiteral(Source).IntegerLiteral) then begin
      IntegerLiteral := TffSqlIntegerLiteral.Create(Self);
      IntegerLiteral.Assign(TffSqlLiteral(Source).IntegerLiteral);
    end;

    if assigned(TffSqlLiteral(Source).StringLiteral) then begin
      StringLiteral := TffSqlStringLiteral.Create(Self);
      StringLiteral.Assign(TffSqlLiteral(Source).StringLiteral);
    end;

    if assigned(TffSqlLiteral(Source).DateLiteral) then begin
      DateLiteral := TffSqlDateLiteral.Create(Self);
      DateLiteral.Assign(TffSqlLiteral(Source).DateLiteral);
    end;

    if assigned(TffSqlLiteral(Source).TimeLiteral) then begin
      TimeLiteral := TffSqlTimeLiteral.Create(Self);
      TimeLiteral.Assign(TffSqlLiteral(Source).TimeLiteral);
    end;

    if assigned(TffSqlLiteral(Source).TimeStampLiteral) then begin
      TimeStampLiteral := TffSqlTimeStampLiteral.Create(Self);
      TimeStampLiteral.Assign(TffSqlLiteral(Source).TimeStampLiteral);
    end;

    if assigned(TffSqlLiteral(Source).IntervalLiteral) then begin
      IntervalLiteral := TffSqlIntervalLiteral.Create(Self);
      IntervalLiteral.Assign(TffSqlLiteral(Source).IntervalLiteral);
    end;

    if assigned(TffSqlLiteral(Source).BooleanLiteral) then begin
      BooleanLiteral := TffSqlBooleanLiteral.Create(Self);
      BooleanLiteral.Assign(TffSqlLiteral(Source).BooleanLiteral);
    end;

  end else
    AssignError(Source);
end;

procedure TffSqlLiteral.Clear;
begin
  FloatLiteral.Free;
  IntegerLiteral.Free;
  StringLiteral.Free;
  DateLiteral.Free;
  TimeLiteral.Free;
  TimeStampLiteral.Free;
  IntervalLiteral.Free;
  BooleanLiteral.Free;
  FloatLiteral:= nil;
  IntegerLiteral:= nil;
  StringLiteral:= nil;
  DateLiteral:= nil;
  TimeLiteral:= nil;
  TimeStampLiteral:= nil;
  IntervalLiteral:= nil;
  BooleanLiteral := nil;
end;

destructor TffSqlLiteral.Destroy;
begin
  Clear;
  inherited;
end;
{--------}
procedure TffSqlLiteral.EmitSQL(Stream: TStream);
begin
  if FloatLiteral <> nil then
    FloatLiteral.EmitSQL(Stream)
  else
  if IntegerLiteral <> nil then
    IntegerLiteral.EmitSQL(Stream)
  else
  if StringLiteral <> nil then
    StringLiteral.EmitSQL(Stream)
  else
  if DateLiteral <> nil then
    DateLiteral.EmitSQL(Stream)
  else
  if TimeLiteral <> nil then
    TimeLiteral.EmitSQL(Stream)
  else
  if TimestampLiteral <> nil then
    TimestampLiteral.EmitSQL(Stream)
  else
  if IntervalLiteral <> nil then
    IntervalLiteral.EmitSQL(Stream)
  else
  if BooleanLiteral <> nil then
    BooleanLiteral.EmitSQL(Stream)
  else
    Assert(False);
end;
{--------}
function TffSqlLiteral.AddIntervalTo(Target: TDateTime): TDateTime;
begin
  if IntervalLiteral <> nil then
    Result := IntervalLiteral.AddIntervalTo(Target)
  else begin
    SQLError('Internal error: Type Mismatch');
    Result := Null;
  end;
end;
{--------}
function TffSqlLiteral.SubtractIntervalFrom(Target: TDateTime): TDateTime;
begin
  if IntervalLiteral <> nil then
    Result := IntervalLiteral.SubtractIntervalFrom(Target)
  else begin
    SQLError('Internal error: Type Mismatch');
    Result := Null;
  end;
end;
{--------}
procedure TffSqlLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if FloatLiteral <> nil then
    FloatLiteral.EnumNodes(EnumMethod, Deep)
  else
  if IntegerLiteral <> nil then
    IntegerLiteral.EnumNodes(EnumMethod, Deep)
  else
  if StringLiteral <> nil then
    StringLiteral.EnumNodes(EnumMethod, Deep)
  else
  if DateLiteral <> nil then
    DateLiteral.EnumNodes(EnumMethod, Deep)
  else
  if TimeLiteral <> nil then
    TimeLiteral.EnumNodes(EnumMethod, Deep)
  else
  if TimestampLiteral <> nil then
    TimestampLiteral.EnumNodes(EnumMethod, Deep)
  else
  if IntervalLiteral <> nil then
    IntervalLiteral.EnumNodes(EnumMethod, Deep)
  else
  if BooleanLiteral <> nil then
    BooleanLiteral.EnumNodes(EnumMethod, Deep)
  else
    Assert(False);
end;
{--------}
function TffSqlLiteral.GetValue: Variant;
begin
  if FloatLiteral <> nil then
    Result := FloatLiteral.GetValue
  else
  if IntegerLiteral <> nil then
    Result := IntegerLiteral.GetValue
  else
  if StringLiteral <> nil then
    Result := StringLiteral.GetValue
  else
  if DateLiteral <> nil then
    Result := DateLiteral.GetValue
  else
  if TimeLiteral <> nil then
    Result := TimeLiteral.GetValue
  else
  if TimestampLiteral <> nil then
    Result := TimestampLiteral.GetValue
  else
  if IntervalLiteral <> nil then
    Result := IntervalLiteral.GetValue
  else
  if BooleanLiteral <> nil then
    Result := BooleanLiteral.GetValue
  else
    Assert(False);
end;
{--------}
function TffSqlLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlLiteral)
    and
      (BothNil(FloatLiteral, TffSqlLiteral(Other).FloatLiteral)
       or (BothNonNil(FloatLiteral, TffSqlLiteral(Other).FloatLiteral)
         and FloatLiteral.Equals(TffSqlLiteral(Other).FloatLiteral)
         )
      )
    and
      (BothNil(IntegerLiteral, TffSqlLiteral(Other).IntegerLiteral)
       or (BothNonNil(IntegerLiteral, TffSqlLiteral(Other).IntegerLiteral)
         and IntegerLiteral.Equals(TffSqlLiteral(Other).IntegerLiteral)
         )
      )
    and
      (BothNil(StringLiteral, TffSqlLiteral(Other).StringLiteral)
       or (BothNonNil(StringLiteral, TffSqlLiteral(Other).StringLiteral)
         and StringLiteral.Equals(TffSqlLiteral(Other).StringLiteral)
         )
       )
    and
      (BothNil(DateLiteral, TffSqlLiteral(Other).DateLiteral)
       or (BothNonNil(DateLiteral, TffSqlLiteral(Other).DateLiteral)
         and DateLiteral.Equals(TffSqlLiteral(Other).DateLiteral)
         )
       )
    and
      (BothNil(TimeLiteral, TffSqlLiteral(Other).TimeLiteral)
       or (BothNonNil(TimeLiteral, TffSqlLiteral(Other).TimeLiteral)
         and TimeLiteral.Equals(TffSqlLiteral(Other).TimeLiteral)
         )
       )
    and
      (BothNil(TimestampLiteral, TffSqlLiteral(Other).TimestampLiteral)
       or (BothNonNil(TimestampLiteral, TffSqlLiteral(Other).TimestampLiteral)
         and TimestampLiteral.Equals(TffSqlLiteral(Other).TimestampLiteral)
         )
      )
    and
      (BothNil(IntervalLiteral, TffSqlLiteral(Other).IntervalLiteral)
       or (BothNonNil(IntervalLiteral, TffSqlLiteral(Other).IntervalLiteral)
         and IntervalLiteral.Equals(TffSqlLiteral(Other).IntervalLiteral)
         )
      )
    and
      (BothNil(BooleanLiteral, TffSqlLiteral(Other).BooleanLiteral)
       or (BothNonNil(BooleanLiteral, TffSqlLiteral(Other).BooleanLiteral)
         and BooleanLiteral.Equals(TffSqlLiteral(Other).BooleanLiteral)
         )
      );
end;
{--------}
function TffSqlLiteral.GetDecimals: Integer;
begin
  if FloatLiteral <> nil then
    Result := FloatLiteral.GetDecimals
  else
    Result := 0;
end;
{--------}
function TffSqlLiteral.GetSize: Integer;
begin
  Result := 0;
  if FloatLiteral <> nil then
    Result := FloatLiteral.GetSize
  else
  if IntegerLiteral <> nil then
    Result := IntegerLiteral.GetSize
  else
  if StringLiteral <> nil then
    Result := StringLiteral.GetSize
  else
  if DateLiteral <> nil then
    Result := DateLiteral.GetSize
  else
  if TimeLiteral <> nil then
    Result := TimeLiteral.GetSize
  else
  if TimestampLiteral <> nil then
    Result := TimestampLiteral.GetSize
  else
  if IntervalLiteral <> nil then
    Result := IntervalLiteral.GetSize
  else
  if BooleanLiteral <> nil then
    Result := BooleanLiteral.GetSize
  else
    Assert(False);
end;
{--------}
function TffSqlLiteral.GetType: TffFieldType;
begin
  Result := fftInterval; {dummy to suppress compiler warning}
  if FloatLiteral <> nil then
    Result := FloatLiteral.GetType
  else
  if IntegerLiteral <> nil then
    Result := IntegerLiteral.GetType
  else
  if StringLiteral <> nil then
    Result := StringLiteral.GetType
  else
  if DateLiteral <> nil then
    Result := DateLiteral.GetType
  else
  if TimeLiteral <> nil then
    Result := TimeLiteral.GetType
  else
  if TimestampLiteral <> nil then
    Result := TimestampLiteral.GetType
  else
  if IntervalLiteral <> nil then
    Result := IntervalLiteral.GetType
  else
  if BooleanLiteral <> nil then
    Result := BooleanLiteral.GetType
  else
    Assert(False);
end;
{--------}

function IsValidDate(const S: ShortString): Boolean;
begin
  if (length(S) <> 12)
  or (S[6] <> '-')
  or (S[9] <> '-') then
    Result := False
  else
    try
      EncodeDate(
        StrToInt(copy(S, 2, 4)),
        StrToInt(copy(S, 7, 2)),
        StrToInt(copy(S, 10, 2)));
      Result := True;
    except
      Result := False;
    end;
end;

function IsValidTime(const S: ShortString): Boolean;
begin
  if (length(S) <> 10)
  or (S[4] <> ':')
  or (S[7] <> ':') then
    Result := False
  else
    try
      EncodeTime(
          StrToInt(copy(S, 2, 2)),
          StrToInt(copy(S, 5, 2)),
          StrToInt(copy(S, 8, 2)),
          0);
      Result := True;
    except
      Result := False;
    end;
end;

function IsValidTimestamp(const S: ShortString): Boolean;
begin
  if (length(S) < 21)
  or (S[6] <> '-')
  or (S[9] <> '-')
  or (S[12] <> ' ')
  or (S[15] <> ':')
  or (S[18] <> ':') then
    Result := False
  else
    try
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
    except
      Result := False;
    end;
end;

procedure TffSqlLiteral.MatchType(ExpectedType: TffFieldType);
begin
  if FloatLiteral <> nil then
    FloatLiteral.MatchType(ExpectedType)
  else
  if IntegerLiteral <> nil then
    IntegerLiteral.MatchType(ExpectedType)
  else
  if StringLiteral <> nil then
    case ExpectedType of
    fftStDate, fftStTime, fftDateTime:
      begin
        {String literal, but caller was expecting a Date-type.}
         {See if the string literal represents a valid date.}
         {If it does, convert.}
        if IsValidDate(StringLiteral.Value) then begin
          DateLiteral := TffSqlDateLiteral.Create(Self);
          DateLiteral.Value := StringLiteral.Value;
          StringLiteral.Free;
          StringLiteral := nil;
        end else
         {See if the string literal represents a valid time.}
         {If it does, convert.}
        if IsValidTime(StringLiteral.Value) then begin
          TimeLiteral := TffSqlTimeLiteral.Create(Self);
          TimeLiteral.Value := StringLiteral.Value;
          StringLiteral.Free;
          StringLiteral := nil;
        end else
         {See if the string literal represents a valid time stamp}
         {If it does, convert.}
        if IsValidTimestamp(StringLiteral.Value) then begin
          TimeStampLiteral := TffSqlTimestampLiteral.Create(Self);
          TimeStampLiteral.Value := StringLiteral.Value;
          StringLiteral.Free;
          StringLiteral := nil;
        end else
          TypeMismatch;
      end;
    else
      StringLiteral.MatchType(ExpectedType);
    end
  else
  if DateLiteral <> nil then
    DateLiteral.MatchType(ExpectedType)
  else
  if TimeLiteral <> nil then
    TimeLiteral.MatchType(ExpectedType)
  else
  if TimestampLiteral <> nil then
    TimestampLiteral.MatchType(ExpectedType)
  else
  if IntervalLiteral <> nil then
    IntervalLiteral.MatchType(ExpectedType)
  else
  if BooleanLiteral <> nil then
    BooleanLiteral.MatchType(ExpectedType)
  else
    Assert(False);
end;
{====================================================================}

{===TffSqlParam======================================================}
procedure TffSqlParam.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlParam then begin
    FParmIndex := TffSqlParam(Source).FParmIndex;
  end else
    AssignError(Source);
end;

constructor TffSqlParam.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FParmIndex := Owner.ParmCount;
  inc(Owner.ParmCount);
end;
{--------}
function TffSqlParam.GetDecimals: Integer;
begin
  Result := 0;
end;
{--------}
function TffSqlParam.GetSize: Integer;
begin
  case GetType of
  fftWideString : Result := length(GetValue);
  fftShortAnsiStr : Result := length(GetValue);
  fftBLOB : Result := VarArrayHighBound(GetValue, 1);                  {!!.13}
  else
    Result := 0;
  end;
end;
{--------}
function TffSqlParam.GetTitle(const Qualified : Boolean): string;      {!!.11}
begin
  Result := '?';
end;
{--------}
function TffSqlParam.GetType: TffFieldType;
var
  V : Variant;
begin
  Result := fftInterval; {dummy to suppress compiler warning}
  V := Owner.ParmList.GetValue(ParmIndex);
  case VarType(V) and VarTypeMask of
  varSmallint  : Result := fftInt32;
  varInteger   : Result := fftInt32;
  varSingle    : Result := fftSingle;
  varDouble    : Result := fftDouble;
  varCurrency  : Result := fftCurrency;
  varDate      : Result := fftDateTime;
  varOleStr    : Result := fftWideString;
  varBoolean   : Result := fftBoolean;
  varString    : Result := fftShortAnsiStr;
  varByte      : Result := fftBLOB;                                    {!!.13}
  else
    SQLError('Unsupported parameter type:'+IntToHex(VarType(V),0));
  end;
end;
{--------}
procedure TffSqlParam.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' ?');
end;
{--------}
procedure TffSqlParam.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlParam.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlParam) {
    and (AnsiCompareText(Name, TffSqlParam(Other).Name) = 0)};
end;
{--------}
function TffSqlParam.GetValue: Variant;
begin
  if Owner.ParmList = nil then
    raise Exception.Create('No parameter values specified for query. ' +
                           'Verify the parameters listed in the ' +
                           'TffQuery.Params property matches the ' +
                           'parameters specified in the TffQuery.SQL ' +
                           'property.');
  Result := Owner.ParmList.GetValue(ParmIndex);
end;
{--------}
procedure TffSqlParam.MatchType(ExpectedType: TffFieldType);
begin
end;
{====================================================================}

{===TffSqlFactor=====================================================}
function TffSqlFactor.AddIntervalTo(Target: TDateTime): TDateTime;
begin
  if Literal <> nil then
    Result := Literal.AddIntervalTo(Target)
  else begin
    SQLError('Not implemented');
    Result := Null;
  end;
end;
{--------}
function TffSqlFactor.SubtractIntervalFrom(Target: TDateTime): TDateTime;
begin
  if Literal <> nil then
    Result := Literal.SubtractIntervalFrom(Target)
  else begin
    SQLError('Not implemented');
    Result := Null;
  end;
end;
{--------}
procedure TffSqlFactor.CheckIsConstant;
begin
  FIsConstantChecked := True;
  if SubQuery <> nil then
    FIsConstant := False
  else
  if CondExp <> nil then
    FIsConstant := CondExp.IsConstant
  else
  if FieldRef <> nil then
    FIsConstant := False
  else
  if Literal <> nil then
    FIsConstant := {True} Literal.IntervalLiteral = nil
      {can't store interval values, so we can't handle those
       as constant values even if they are in fact constant}
  else
  if Param <> nil then
    FIsConstant := False
  else
  if Aggregate <> nil then
    FIsConstant := False
  else
  if ScalarFunc <> nil then
    FIsConstant := ScalarFunc.IsConstant
  else
    Assert(False);
  if FIsConstant then begin
    FIsConstant := False;
    ConstantValue := GetValue;
    FIsConstant := True;
  end;
end;
{--------}
procedure TffSqlFactor.CheckType;
begin
  if SubQuery <> nil then
    FType := SubQuery.GetType
  else
  if CondExp <> nil then
    FType:= CondExp.GetType
  else
  if FieldRef <> nil then
    FType := FieldRef.GetType
  else
  if Literal <> nil then
    FType := Literal.GetType
  else
  if Param <> nil then
    FType := Param.GetType
  else
  if Aggregate <> nil then
    FType := Aggregate.GetType
  else
  if ScalarFunc <> nil then
    FType := ScalarFunc.GetType
  else
    Assert(False);
  if UnaryMinus then
    case FType of
    fftByte,
    fftWord16,
    fftWord32,
    fftInt8,
    fftInt16,
    fftInt32,
    fftAutoInc,
    fftSingle,
    fftDouble,
    fftExtended,
    fftComp,
    fftCurrency :
     ;
    else
      SQLError('Operator/operand mismatch');
    end;
  TypeKnown := True;
end;
{--------}
procedure TffSqlFactor.Clear;
begin
  SubQuery.Free;
  CondExp.Free;
  FieldRef.Free;
  Literal.Free;
  Param.Free;
  Aggregate.Free;
  ScalarFunc.Free;
  SubQuery:= nil;
  CondExp:= nil;
  FieldRef:= nil;
  Literal:= nil;
  Param:= nil;
  Aggregate:= nil;
  ScalarFunc:= nil;
end;
{--------}
function TffSqlFactor.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if SubQuery <> nil then
    Result := SubQuery.DependsOn(Table)
  else
  if CondExp <> nil then
    Result := CondExp.DependsOn(Table)
  else
  if FieldRef <> nil then
    Result := FieldRef.DependsOn(Table)
  else
  if Literal <> nil then
    Result := False
  else
  if Param <> nil then
    Result := False
  else
  if Aggregate <> nil then
    Result := Aggregate.DependsOn(Table)
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.DependsOn(Table)
  else begin
    Assert(False);
    Result := False;
  end;
end;
{--------}
destructor TffSqlFactor.Destroy;
begin
  Clear;
  inherited;
end;
{--------}
procedure TffSqlFactor.EmitSQL(Stream: TStream);
begin
  if UnaryMinus then
    WriteStr(Stream,' - ');
  if SubQuery <> nil then begin
    WriteStr(Stream,' (');
    SubQuery.EmitSQL(Stream);
    WriteStr(Stream,')');
  end else
  if CondExp <> nil then begin
    WriteStr(Stream,' (');
    CondExp.EmitSQL(Stream);
    WRiteStr(Stream,')');
  end else
  if FieldRef <> nil then
    FieldRef.EmitSQL(Stream)
  else
  if Literal <> nil then
    Literal.EmitSQL(Stream)
  else
  if Param <> nil then
    Param.EmitSQL(Stream)
  else
  if Aggregate <> nil then
    Aggregate.EmitSQL(Stream)
  else
  if ScalarFunc <> nil then
    ScalarFunc.EmitSQL(Stream)
  else
    Assert(False);
end;
{--------}
procedure TffSqlFactor.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if SubQuery <> nil then
    SubQuery.EnumNodes(EnumMethod, Deep)
  else
  if CondExp <> nil then
    CondExp.EnumNodes(EnumMethod, Deep)
  else
  if FieldRef <> nil then
    FieldRef.EnumNodes(EnumMethod, Deep)
  else
  if Literal <> nil then
    Literal.EnumNodes(EnumMethod, Deep)
  else
  if Param <> nil then
    Param.EnumNodes(EnumMethod, Deep)
  else
  if ScalarFunc <> nil then
    ScalarFunc.EnumNodes(EnumMethod, Deep)
  else
  if Aggregate <> nil then
    Aggregate.EnumNodes(EnumMethod, Deep)
  else
    Assert(False);
end;
{--------}
function TffSqlFactor.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlFactor)
    and (MulOp = TffSqlFactor(Other).MulOp)
    and (UnaryMinus = TffSqlFactor(Other).UnaryMinus)
    and
      (BothNil(CondExp, TffSqlFactor(Other).CondExp)
       or (
         BothNonNil(CondExp, TffSqlFactor(Other).CondExp)
         and CondExp.Equals(TffSqlFactor(Other).CondExp)
        )
      )
    and
      (BothNil(FieldRef, TffSqlFactor(Other).FieldRef)
       or (
         BothNonNil(FieldRef, TffSqlFactor(Other).FieldRef)
         and FieldRef.Equals(TffSqlFactor(Other).FieldRef)
        )
      )
    and
      (BothNil(Literal, TffSqlFactor(Other).Literal)
       or (
         BothNonNil(Literal, TffSqlFactor(Other).Literal)
         and Literal.Equals(TffSqlFactor(Other).Literal)
        )
      )
    and
      (BothNil(Param, TffSqlFactor(Other).Param)
       or (
         BothNonNil(Param, TffSqlFactor(Other).Param)
         and Param.Equals(TffSqlFactor(Other).Param)
        )
      )
    and
      (BothNil(Aggregate, TffSqlFactor(Other).Aggregate)
       or (
         BothNonNil(Aggregate, TffSqlFactor(Other).Aggregate)
         and Aggregate.Equals(TffSqlFactor(Other).Aggregate)
        )
      )
    and
      (BothNil(SubQuery, TffSqlFactor(Other).SubQuery)
       or (
         BothNonNil(SubQuery, TffSqlFactor(Other).SubQuery)
         and SubQuery.Equals(TffSqlFactor(Other).SubQuery)
        )
      )
    and
      (BothNil(ScalarFunc, TffSqlFactor(Other).ScalarFunc)
       or (
         BothNonNil(ScalarFunc, TffSqlFactor(Other).ScalarFunc)
         and ScalarFunc.Equals(TffSqlFactor(Other).ScalarFunc)
        )
      );
end;
{--------}
function TffSqlFactor.GetDecimals: Integer;
begin
  if SubQuery <> nil then
    Result := SubQuery.GetDecimals
  else
  if CondExp <> nil then
    Result := CondExp.GetDecimals
  else
  if FieldRef <> nil then
    Result := FieldRef.GetDecimals
  else
  if Literal <> nil then
    Result := Literal.GetDecimals
  else
  if Param <> nil then
    Result := Param.GetDecimals
  else
  if Aggregate <> nil then
    Result := Aggregate.GetDecimals
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.GetDecimals
  else begin
    Assert(False);
    Result := 0;
  end;
end;
{--------}
function TffSqlFactor.GetSize: Integer;
begin
  if SubQuery <> nil then
    Result := SubQuery.GetSize
  else
  if CondExp <> nil then
    Result := CondExp.GetSize
  else
  if FieldRef <> nil then
    Result := FieldRef.GetSize
  else
  if Literal <> nil then
    Result := Literal.GetSize
  else
  if Param <> nil then
    Result := Param.GetSize
  else
  if Aggregate <> nil then
    Result := Aggregate.GetSize
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.GetSize
  else begin
    Assert(False);
    Result := 0;
  end;
end;
{--------}
function TffSqlFactor.GetTitle(const Qualified : Boolean): string;     {!!.11}
begin
  if SubQuery <> nil then
    Result := 'SUB'
  else
  if CondExp <> nil then
    Result := CondExp.GetTitle(Qualified)                              {!!.11}
  else
  if FieldRef <> nil then
    Result := FieldRef.GetTitle(Qualified)                             {!!.11}
  else
  if Literal <> nil then
    Result := 'LIT'
  else
  if Param <> nil then
    Result := Param.GetTitle(Qualified)                                {!!.11}
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.GetTitle(Qualified)                           {!!.11}
  else
  if Aggregate <> nil then
    Result := Aggregate.GetTitle(Qualified)                            {!!.11}
  else
    Assert(False);
end;
{--------}
function TffSqlFactor.GetType: TffFieldType;
begin
  if not TypeKnown then
    CheckType;
  Result := FType
end;
{--------}
procedure TffSqlFactor.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlFactor then begin
    Clear;

    MulOp := TffSqlFactor(Source).MulOp;

    UnaryMinus := TffSqlFactor(Source).UnaryMinus;

    if assigned(TffSqlFactor(Source).CondExp) then begin
      CondExp := TffSqlCondExp.Create(Self);
      CondExp.Assign(TffSqlFactor(Source).CondExp);
    end;

    if assigned(TffSqlFactor(Source).FieldRef) then begin
      FieldRef := TffSqlFieldRef.Create(Self);
      FieldRef.Assign(TffSqlFactor(Source).FieldRef);
    end;

    if assigned(TffSqlFactor(Source).Literal) then begin
      Literal := TffSqlLiteral.Create(Self);
      Literal.Assign(TffSqlFactor(Source).Literal);
    end;

    if assigned(TffSqlFactor(Source).Param) then begin
      Param := TffSqlParam.Create(Self);
      Param.Assign(TffSqlFactor(Source).Param);
    end;

    if assigned(TffSqlFactor(Source).Aggregate) then begin
      Aggregate := TffSqlAggregate.Create(Self);
      Aggregate.Assign(TffSqlFactor(Source).Aggregate);
    end;

    if assigned(TffSqlFactor(Source).SubQuery) then begin
      SubQuery := TffSqlSELECT.Create(Self);
      SubQuery.Assign(TffSqlFactor(Source).SubQuery);
    end;

    if assigned(TffSqlFactor(Source).ScalarFunc) then begin
      ScalarFunc := TffSqlScalarFunc.Create(Self);
      ScalarFunc.Assign(TffSqlFactor(Source).ScalarFunc);
    end;

  end else
    AssignError(Source);
end;
{--------}
function TffSqlFactor.GetValue: Variant;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  if SubQuery <> nil then
    Result := SubQuery.GetValue
  else
  if CondExp <> nil then
    Result := CondExp.GetValue
  else
  if FieldRef <> nil then
    Result := FieldRef.GetValue
  else
  if Literal <> nil then
    Result := Literal.GetValue
  else
  if Param <> nil then
    Result := Param.GetValue
  else
  if Aggregate <> nil then
    Result := Aggregate.GetAggregateValue
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.GetValue
  else
    Assert(False);
  if UnaryMinus then
    if not VarIsNull(Result) then
      Result := - Result;
end;
{--------}
function TffSqlFactor.HasFieldRef: Boolean;
begin
  Result := (FieldRef <> nil);
end;
{--------}
function TffSqlFactor.IsField(var FieldReferenced: TFFSqlFieldProxy): Boolean;
begin
  Result := (FieldRef <> nil) and not UnaryMinus;
  if Result then
    FieldReferenced := FieldRef.Field;
end;
{--------}
function TffSqlFactor.IsFieldFrom(Table: TFFSqlTableProxy;
  var FieldReferenced: TFFSqlFieldProxy; var SameCase: Boolean): Boolean;
begin
  Result := (FieldRef <> nil) and
            (FieldRef.Field <> nil) and
            (FieldRef.Field.OwnerTable = Table);
  if Result then begin
    FieldReferenced := FieldRef.Field;
    SameCase := True; 
  end else
    if ScalarFunc <> nil then begin
      Result := ScalarFunc.IsFieldFrom(Table, FieldReferenced);
      SameCase := False;
    end;
end;
{--------}
function TffSqlFactor.IsNull: Boolean;
begin
  if FieldRef <> nil then
    Result := FieldRef.IsNull
  else
    Result := VarIsNull(GetValue);
end;
{--------}
function TffSqlFactor.IsAggregate: Boolean;
begin
  Result := Aggregate <> nil;
end;
{--------}
function TffSqlFactor.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
procedure TffSqlFactor.MatchType(ExpectedType: TffFieldType);
begin
  if SubQuery <> nil then
    SubQuery.MatchType(ExpectedType, True)
  else
  if CondExp <> nil then
    CondExp.MatchType(ExpectedType)
  else
  if FieldRef <> nil then
    FieldRef.MatchType(ExpectedType)
  else
  if Literal <> nil then
    Literal.MatchType(ExpectedType)
  else
  if Param <> nil then
    Param.MatchType(ExpectedType)
  else
  if Aggregate <> nil then
    Aggregate.MatchType(ExpectedType)
  else
  if ScalarFunc <> nil then
    ScalarFunc.MatchType(ExpectedType)
  else
    Assert(False);
end;
{--------}
function TffSqlFactor.Reduce: Boolean;
var
  LiftFactor: TffSqlFactor;
begin
  if SubQuery <> nil then
    Result := SubQuery.Reduce
  else
  if CondExp <> nil then begin
    {!!.11 begin}
    {if conditional expression is nothing but a parenthesized factor,
     lift it to this level}
    LiftFactor := nil;
    if CondExp.CondTermCount = 1 then
      with CondExp.CondTerm[0] do
        if CondFactorCount = 1 then
          with CondFactor[0] do
            if not UnaryNot then
              with CondPrimary do
                if (RelOp = roNone) and (SimpleExp2 = nil) then
                  with SimpleExp1 do
                    if TermCount = 1 then
                      with Term[0] do
                        if FactorCount = 1 then begin
                          LiftFactor := TffSqlFactor.Create(Parent);
                          LiftFactor.Assign(Factor[0]);
                          LiftFactor.MulOp := MulOp;                   {!!.13}
                        end;
    if LiftFactor <> nil then begin
      CondExp.Free;
      CondExp := nil;
      Assign(LiftFactor);
      LiftFactor.Free;
      Result := True;
    end else
    {!!.11 end}
      Result := CondExp.Reduce
  end else
  if FieldRef <> nil then
    Result := False
  else
  if Literal <> nil then
    Result := False
  else
  if Param <> nil then
    Result := False
  else
  if Aggregate <> nil then
    Result := Aggregate.Reduce
  else
  if ScalarFunc <> nil then
    Result := ScalarFunc.Reduce
  else
    Result := False;
end;
{--------}
procedure TffSqlFactor.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;
{Begin !!.11}
{--------}
function TffSqlFactor.WasWildcard : Boolean;
begin
  if FieldRef <> nil then
    Result := FieldRef.WasWildcard
  else
    Result := False;
end;
{End !!.11}
{====================================================================}

{===TffSqlSelection==================================================}
procedure TffSqlSelection.AddColumnDef(Target: TffSqlColumnListOwner);
{Rewritten !!.11}
var
  S, SQual : string;
  F : TffSqlNode;
  i : Integer;
begin
  if Column <> nil then
    S := Column.ColumnName
  else
    S := '';
  F := SimpleExpression;
  if S = '' then
    S := SimpleExpression.GetTitle(False);

  if Target.Columns.IndexOf(S) <> -1 then begin
    { See if we can use the qualified column name. This is done for the sake
      of backwards compatibility with existing SQL statements in FF clients. }
    SQual := SimpleExpression.GetTitle(True);
    if Target.Columns.IndexOf(SQual) = -1 then
      Target.Columns.AddObject(SQual, F)
    else begin
      i := 1;
      repeat
        inc(i);
      until Target.Columns.IndexOf(S + '_' + IntToStr(i)) = -1;
      Target.Columns.AddObject(S + '_' + IntToStr(i), F);
    end;
  end else
    Target.Columns.AddObject(S, F);
end;
{--------}
procedure TffSqlSelection.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlSelection then begin
    SimpleExpression.Free;
    SimpleExpression := TffSqlSimpleExpression.Create(Self);
    SimpleExpression.Assign(TffSqlSelection(Source).SimpleExpression);
    Column.Free;
    Column := nil;
    if assigned(TffSqlSelection(Source).Column) then begin
      Column := TffSqlColumn.Create(Self);
      Column.Assign(TffSqlSelection(Source).Column);
    end;
  end else
    AssignError(Source);
end;

destructor TffSqlSelection.Destroy;
begin
  SimpleExpression.Free;
  Column.Free;
  inherited;
end;

procedure TffSqlSelection.EmitSQL(Stream: TStream);
begin
  SimpleExpression.EmitSQL(Stream);
  if Column <> nil then begin
    WriteStr(Stream,' AS');
    Column.EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlSelection.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SimpleExpression.EnumNodes(EnumMethod, Deep);
  if Column <> nil then
    Column.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlSelection.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlSelection)
    and (
      BothNil(SimpleExpression, TffSqlSelection(Other).SimpleExpression)
      or (BothNonNil(SimpleExpression, TffSqlSelection(Other).SimpleExpression)
        and SimpleExpression.Equals(TffSqlSelection(Other).SimpleExpression)
        )
      )
    and (
      BothNil(Column, TffSqlSelection(Other).Column)
      or (BothNonNil(Column, TffSqlSelection(Other).Column)
        and Column.Equals(TffSqlSelection(Other).Column)
        )
      );
end;
{--------}
function TffSqlSelection.GetIndex: Integer;
begin
  Result := TffSqlSelectionList(Parent).FSelections.IndexOf(Self);
end;

{--------}
function TffSqlSelection.IsAggregateExpression: Boolean;
begin
  Result := SimpleExpression.IsAggregateExpression;
end;

function TffSqlSelection.Reduce: Boolean;
begin
  Result := SimpleExpression.Reduce;
end;

{====================================================================}

{===TffSqlTableRef===================================================}
procedure TffSqlTableRef.AddTableReference(Select: TffSqlSELECT);
var
  IX, I : Integer;
begin
  IX := -1;
  Assert(Assigned(Select.TablesReferencedByOrder));
  if TableName <> '' then begin
    if DatabaseName <> '' then
      if not SameText(DatabaseName, Owner.FDatabase.Alias) then
        SQLError(format('The referenced database name %s does not '+
          'match the current database, %s.',
          [DatabaseName, Owner.FDatabase.Alias]));
    IX := Select.TablesReferencedByOrder.Add(TableName)
  end else begin
    Assert(Assigned(TableExp));
    TableExp.EnsureResultTable(True);
    if Select.TablesReferencedByOrder.IndexOf('$$UNNAMED') = -1 then
      IX := Select.TablesReferencedByOrder.AddObject('$$UNNAMED',
        TableExp.ResultTable)
    else begin
      I := 2;
      repeat
        if Select.TablesReferencedByOrder.IndexOf('$$UNNAMED_' + IntToStr(I)) =
          -1 then begin
          IX := Select.TablesReferencedByOrder.AddObject('$$UNNAMED_' +
            IntToStr(I), TableExp.ResultTable);
          break;
        end;
        inc(I);
      until False;
    end;
  end;
  if Alias <> '' then begin
    Assert(Assigned(Select.TableAliases));
    if Select.TableAliases.IndexOf(Alias) <> -1 then
      SQLError('Duplicate alias definition:' + Alias);
    Select.TableAliases.AddObject(Alias, TObject(IX));
  end;
end;
{--------}
procedure TffSqlTableRef.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlTableRef then begin
    Clear;
    TableName := TffSqlTableRef(Source).TableName;
    Alias := TffSqlTableRef(Source).Alias;
    if TffSqlTableRef(Source).TableExp <> nil then begin
      TableExp := TffSqlTableExp.Create(Self);
      TableExp.Assign(TffSqlTableRef(Source).TableExp);
    end;
    if TffSqlTableRef(Source).ColumnList <> nil then begin
      ColumnList := TFFSqlInsertColumnList.Create(Self);
      ColumnList.Assign(TffSqlTableRef(Source).ColumnList);
    end;
  end else
    AssignError(Source);
end;

function TffSqlTableRef.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
{- not used for binding directly from SELECT - only for
   binding to contained sub-expressions}
begin
  if TableExp <> nil then
    Result := TableExp.BindFieldDown(TableName, FieldName)
  else
  if SameText(TableName, Self.TableName)
    and (Alias = '') {can't bind to table name if alias present}       {!!.12}
  or SameText(TableName, Alias) then
    Result := ResultTable.FieldByName(FieldName)
  else
    Result := nil;
end;

function TffSqlTableRef.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  if SameText(TableName, Alias) or SameText(TableName, Self.TableName) then
    Result := GetTable(AOwner, False)
  else
    if TableExp <> nil then
      Result := TableExp.BindTable(AOwner, TableName)
    else
      Result := nil;
end;

procedure TffSqlTableRef.Clear;
begin
  TableName := '';
  Alias := '';
  TableExp.Free;
  TableExp := nil;
  ColumnList.Free;
  ColumnList := nil;
end;

function TffSqlTableRef.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if TableExp <> nil then
    Result := TableExp.DependsOn(Table)
  else
    Result := False;
end;

destructor TffSqlTableRef.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlTableRef.EmitSQL(Stream: TStream);
begin
  if TableName <> '' then begin
    WriteStr(Stream, ' ');
    WriteStr(Stream, TableName);
    if Alias <> '' then begin
      WriteStr(Stream,' AS ');
      WriteStr(Stream, Alias);
    end;
  end else
  if TableExp <> nil then begin
    WriteStr(Stream, ' (');
    TableExp.EmitSQL(Stream);
    WriteStr(Stream,')');
    if Alias <> '' then begin
      WriteStr(Stream,' AS ');
      WriteStr(Stream, Alias);
    end;
    if ColumnList <> nil then begin
      WriteStr(Stream, ' (');
      ColumnList.EmitSQL(Stream);
      WriteStr(Stream, ')');
    end;
  end;
end;
{--------}
procedure TffSqlTableRef.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if Deep and assigned(TableExp) then
    TableExp.EnumNodes(EnumMethod, Deep);
  if assigned(ColumnList) then
    ColumnList.EnumNodes(EnumMethod, Deep);
end;
{--------}

function TffSqlTableRef.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlTableRef)
    and (AnsiCompareText(TableName, TffSqlTableRef(Other).TableName) = 0)
    and (AnsiCompareText(Alias, TffSqlTableRef(Other).Alias) = 0)
    and (BothNil(TableExp, TffSqlTableRef(Other).TableExp)
      or (BothNonNil(TableExp, TffSqlTableRef(Other).TableExp)
        and TableExp.Equals(TffSqlTableRef(Other).TableExp)
      ))
    and (BothNil(ColumnList, TffSqlTableRef(Other).ColumnList)
      or (BothNonNil(ColumnList, TffSqlTableRef(Other).ColumnList)
        and ColumnList.Equals(TffSqlTableRef(Other).ColumnList)
      ));
end;

procedure TffSqlTableRef.Execute(
      var aLiveResult: Boolean; var aCursorID: TffCursorID;
      var RecordsRead: Integer);
var
  T : TffSqlTableProxy;
begin
  Assert(Owner <> nil);
  T := GetTable(Self, False);
  aCursorID := T.CursorID;
  T.LeaveCursorOpen := True;
  if T.Owner = Self then begin
    T.Owner := nil;
    T.Free;
  end;
end;

function TffSqlTableRef.GetResultTable: TFFSqlTableProxy;
begin
  Result := GetTable(Self, False);
end;

function TffSqlTableRef.GetSQLName: string;
begin
  if Alias <> '' then
    Result := Alias
  else
  if TableName <> '' then
    Result := TableName
  else
    Result := 'UNNAMED';
end;

function TffSqlTableRef.GetTable(AOwner: TObject;
                           const ExclContLock : Boolean): TffSqlTableProxy;
begin
  if DatabaseName <> '' then
    if not SameText(DatabaseName, Owner.FDatabase.Alias) then
      SQLError(format('The referenced database name %s does not '+
        'match the current database, %s.',
        [DatabaseName, Owner.FDatabase.Alias]));
  if TableName <> '' then begin
    if FTable = nil then begin
      FTable := Owner.FDatabase.TableByName(AOwner, TableName,
        ExclContLock, Alias);                                          {!!.11}
      if FTable = nil then
        SQLError('Unable to open table: ' + TableName +
               '. Ensure the table exists and is not in use by ' +
               'another process.');
      FTable.SetIndex(-1);
    end;
    Result := FTable;
  end else
    Result := TableExp.ResultTable;
end;

{!!.11 new}
function TffSqlTableRef.Reduce: Boolean;
begin
  if TableExp <> nil then
    if TableExp.Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

function TffSqlTableRef.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
begin
  if TableExp <> nil then
    Result := TableExp.TargetFieldFromSourceField(F)
  else
    Result := nil;                                                     {!!.13}
end;

{====================================================================}

{===TffSqlSimpleExpressionList=======================================}
function TffSqlSimpleExpressionList.AddExpression(
  Expression: TffSqlSimpleExpression): TffSqlSimpleExpression;
begin
  FExpressionList.Add(Expression);
  Result := Expression;
end;
{--------}
procedure TffSqlSimpleExpressionList.Assign(const Source: TffSqlNode);
var
  i: Integer;
begin
  if Source is TffSqlSimpleExpressionList then begin
    Clear;
    for i := 0 to pred(TffSqlSimpleExpressionList(Source).ExpressionCount) do
      AddExpression(TffSqlSimpleExpression.Create(Self)).Assign(
        TffSqlSimpleExpressionList(Source).Expression[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlSimpleExpressionList.CheckIsConstant;
var
  i : Integer;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(ExpressionCount) do
    if not Expression[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  FIsConstant := True;
end;

function TffSqlSimpleExpressionList.Contains(const TestValue: Variant): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(ExpressionCount) do
    if Expression[i].GetValue = TestValue then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
constructor TffSqlSimpleExpressionList.Create(
  AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FExpressionList := TList.Create;
end;
{--------}
procedure TffSqlSimpleExpressionList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(ExpressionCount) do
    Expression[i].Free;
  FExpressionList.Clear;
end;
{--------}
function TffSqlSimpleExpressionList.DependsOn(
  Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(ExpressionCount) do
    if Expression[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
destructor TffSqlSimpleExpressionList.Destroy;
begin
  Clear;
  FExpressionList.Free;
  inherited;
end;
{--------}
procedure TffSqlSimpleExpressionList.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  Expression[0].EmitSQL(Stream);
  for i := 1 to pred(ExpressionCount) do begin
    WriteStr(Stream,', ');
    Expression[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlSimpleExpressionList.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(ExpressionCount) do
    Expression[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlSimpleExpressionList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlSimpleExpressionList then begin
    if ExpressionCount <> TffSqlSimpleExpressionList(Other).ExpressionCount then
      exit;
    for i := 0 to pred(ExpressionCount) do
      if not Expression[i].Equals(TffSqlSimpleExpressionList(Other).Expression[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlSimpleExpressionList.GetExpression(
  Index: Integer): TffSqlSimpleExpression;
begin
  Result := TffSqlSimpleExpression(FExpressionList[Index]);
end;
{--------}
function TffSqlSimpleExpressionList.GetExpressionCount: Integer;
begin
  Result := FExpressionList.Count;
end;
{--------}
function TffSqlSimpleExpressionList.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

procedure TffSqlSimpleExpressionList.MatchType(ExpectedType: TffFieldType);
var
  i : Integer;
begin
  for i := 0 to pred(ExpressionCount) do
    Expression[i].MatchType(ExpectedType);
end;
{--------}
function TffSqlSimpleExpressionList.Reduce: Boolean;
var
  I : integer;
begin
  for i := 0 to pred(ExpressionCount) do
    if Expression[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlSimpleExpressionList.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

procedure TffSqlSimpleExpressionList.SetExpression(Index: Integer;
  const Value: TffSqlSimpleExpression);
begin
  FExpressionList[Index] := Value;
end;
{====================================================================}

{===TffSqlOrderColumn================================================}
procedure TffSqlOrderColumn.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlOrderColumn then begin
    TableName := TffSqlOrderColumn(Source).TableName;
    FieldName := TffSqlOrderColumn(Source).FieldName;
  end else
    AssignError(Source);
end;
{--------}
procedure TffSqlOrderColumn.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ' ');
  if TableName <> '' then begin
    WriteStr(Stream, TableName);
    WriteStr(Stream, '.');
  end;
  WriteStr(Stream, FieldName);
end;
{--------}
procedure TffSqlOrderColumn.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlOrderColumn.Equals(Other: TffSqlNode): Boolean;
begin
  Result := Other is TffSqlOrderColumn
   and (AnsiCompareText(TableName, TffSqlOrderColumn(Other).TableName) = 0)
   and (AnsiCompareText(FieldName, TffSqlOrderColumn(Other).FieldName) = 0);
end;
{--------}
function TffSqlOrderColumn.QualColumnName : string;
begin
  if TableName <> '' then
    Result := TableName + '.' + FieldName
  else
    Result := FieldName;
end;
{====================================================================}

{===TffSqlGroupColumn================================================}
procedure TffSqlGroupColumn.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlGroupColumn then begin
    TableName := TffSqlGroupColumn(Source).TableName;
    FieldName := TffSqlGroupColumn(Source).FieldName;
  end else
    AssignError(Source);
end;
{--------}
procedure TffSqlGroupColumn.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ' ');
  if TableName <> '' then begin
    WriteStr(Stream, TableName);
    WriteStr(Stream, '.');
  end;
  WriteStr(Stream, FieldName);
end;
{--------}
procedure TffSqlGroupColumn.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlGroupColumn.Equals(Other: TffSqlNode): Boolean;
begin
  Result := Other is TffSqlGroupColumn
   and (AnsiCompareText(TableName, TffSqlGroupColumn(Other).TableName) = 0)
   and (AnsiCompareText(FieldName, TffSqlGroupColumn(Other).FieldName) = 0);
end;
{--------}
function TffSqlGroupColumn.QualColumnName: string;
var
  F : TffSqlFieldProxy;
  Name : string;
begin
  if OwnerSelect = nil then
    SQLError('Field references may not occur in this context');
  if TableName <> '' then begin
    Name := OwnerSelect.TableRefList.GetNameForAlias(FTableName);
    if Name <> '' then
      Result := Name + '.' + FFieldName
    else
      Result := TableName + '.' + FFieldName;
  end
  else begin
    { If this is an alias for a field in the selection list then return
      the name. }
    if OwnerSelect.Columns.IndexOf(FieldName) > -1 then
      Result := FieldName
    else begin
      { Find the proxy for this field. }
      F := OwnerSelect.FindField(FFieldName);
      if F = nil then
        Result := FFieldName
      else
        Result := F.OwnerTable.Name + '.' + FFieldName;
    end;
  end;
end;
{====================================================================}

{===TffSqlOrderItem==================================================}
procedure TffSqlOrderItem.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlOrderItem then begin
    if TffSqlOrderItem(Source).Column <> nil then begin
      if Column = nil then
        Column := TffSqlOrderColumn.Create(Self);
      Column.Assign(TffSqlOrderItem(Source).Column);
    end;
    Index := TffSqlOrderItem(Source).Index;
    Descending := TffSqlOrderItem(Source).Descending;
  end else
    AssignError(Source);
end;

constructor TffSqlOrderItem.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
end;

destructor TffSqlOrderItem.Destroy;
begin
  Column.Free;
  inherited;
end;

procedure TffSqlOrderItem.EmitSQL(Stream: TStream);
begin
  if Column <> nil then
    Column.EmitSQL(Stream)
  else begin
    WriteStr(Stream, ' ');
    WriteStr(Stream, Index);
  end;
  if Descending then
    WriteStr(Stream,' DESC')
  else
    Writestr(Stream,' ASC');
end;
{--------}
procedure TffSqlOrderItem.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  if Column <> nil then
    Column.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlOrderItem.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlOrderItem)
    and (Descending = TffSqlOrderItem(Other).Descending)
    and (Index = TffSqlOrderItem(Other).Index)
    and (BothNil(Column, TffSqlOrderItem(Other).Column)
      or (BothNonNil(Column, TffSqlOrderItem(Other).Column)
        and Column.Equals(TffSqlOrderItem(Other).Column)
      ));
end;
{--------}
{====================================================================}

{===TffSqlOrderList==================================================}
function TffSqlOrderList.AddOrderItem(NewOrder: TffSqlOrderItem): TffSqlOrderItem;
begin
  FOrderItemList.Add(NewOrder);
  Result := NewOrder;
end;
{--------}
procedure TffSqlOrderList.Assign(const Source: TffSqlNode);
var
  i: Integer;
begin
  if Source is TffSqlOrderList then begin
    Clear;
    for i := 0 to pred(TffSqlOrderList(Source).OrderCount) do
      AddOrderItem(TffSqlOrderItem.Create(Self)).Assign(
        TffSqlOrderList(Source).OrderItem[i]);
  end else
    AssignError(Source);
end;

constructor TffSqlOrderList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FOrderItemList := TList.Create;
end;
{--------}
procedure TffSqlOrderList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(FOrderItemList.Count) do
    OrderItem[i].Free;
  FOrderItemList.Clear;
end;
{--------}
destructor TffSqlOrderList.Destroy;
begin
  Clear;
  FOrderItemList.Free;
  inherited;
end;
{--------}
procedure TffSqlOrderList.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  WriteStr(Stream,' ORDER BY');
  OrderItem[0].EmitSQL(Stream);
  for i := 1 to pred(OrderCount) do begin
    WriteStr(Stream,', ');
    OrderItem[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlOrderList.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(OrderCount) do
    OrderItem[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlOrderList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlOrderList then begin
    if OrderCount <> TffSqlOrderList(Other).OrderCount then
      exit;
    for i := 0 to pred(OrderCount) do
      if not OrderItem[i].Equals(TffSqlOrderList(Other).OrderItem[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlOrderList.GetOrderCount: Integer;
begin
  Result := FOrderItemList.Count;
end;
{--------}
function TffSqlOrderList.GetOrderItem(
  Index: Integer): TffSqlOrderItem;
begin
  Result := TffSqlOrderItem(FOrderItemList[Index]);
end;
{--------}
function TffSqlOrderList.Reduce: Boolean;
begin
  Result := False;
end;

procedure TffSqlOrderList.SetOrderItem(Index: Integer;
  const Value: TffSqlOrderItem);
begin
  FOrderItemList[Index] := Value;
end;
{====================================================================}

{===TffSqlAllOrAnyClause=============================================}
procedure TffSqlAllOrAnyClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlAllOrAnyClause then begin
    All := TffSqlAllOrAnyClause(Source).All;
    SubQuery.Free;
    SubQuery := TffSqlSELECT.Create(Self);
    SubQuery.Assign(TffSqlAllOrAnyClause(Source).SubQuery);
  end else
    AssignError(Source);
end;

function TffSqlAllOrAnyClause.Compare(RelOp: TffSqlRelOp;
  const Val: Variant): Boolean;
begin
  if All then
    Result := SubQuery.CheckAllValues(RelOp, Val)
  else
    Result := SubQuery.CheckAnyValue(RelOp, Val);
end;

function TffSqlAllOrAnyClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SubQuery.DependsOn(Table);
end;

destructor TffSqlAllOrAnyClause.Destroy;
begin
  SubQuery.Free;
  inherited;
end;
{--------}
procedure TffSqlAllOrAnyClause.EmitSQL(Stream: TStream);
begin
  if All then
    WriteStr(Stream,' ALL ')
  else
    WriteStr(Stream,' ANY ');
  WriteStr(Stream,'(');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlAllOrAnyClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlAllOrAnyClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlAllOrAnyClause)
    and (All = TffSqlAllOrAnyClause(Other).All)
    and (SubQuery.Equals(TffSqlAllOrAnyClause(Other).SubQuery));
end;
{--------}
procedure TffSqlAllOrAnyClause.MatchType(ExpectedType: TffFieldType);
begin
  SubQuery.MatchType(ExpectedType, True);
end;

function TffSqlAllOrAnyClause.Reduce: Boolean;
begin
  Result := SubQuery.Reduce;
end;

{====================================================================}

{===TffSqlExistsClause===============================================}
function TffSqlExistsClause.AsBoolean: Boolean;
begin
  Result := SubQuery.CheckNonEmpty;
end;
{--------}
procedure TffSqlExistsClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlExistsClause then begin
    SubQuery.Free;
    SubQuery := TffSqlSELECT.Create(Self);
    SubQuery.Assign(TffSqlExistsClause(Source).SubQuery);
  end else
    AssignError(Source);
end;

function TffSqlExistsClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SubQuery.DependsOn(Table);
end;

destructor TffSqlExistsClause.Destroy;
begin
  SubQuery.Free;
  inherited;
end;
{--------}
procedure TffSqlExistsClause.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' EXISTS (');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlExistsClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlExistsClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlExistsClause)
    and (SubQuery.Equals(TffSqlExistsClause(Other).SubQuery));
end;
{--------}
function TffSqlExistsClause.Reduce: Boolean;
begin
  Result := SubQuery.Reduce;
end;

{====================================================================}

{===TffSqlUniqueClause===============================================}
function TffSqlUniqueClause.AsBoolean: Boolean;
begin
  Result := SubQuery.CheckNoDups;
end;
{--------}
procedure TffSqlUniqueClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlUniqueClause then begin
    SubQuery.Free;
    SubQuery := TffSqlTableExp.Create(Self);
    SubQuery.Assign(TffSqlUniqueClause(Source).SubQuery);
  end else
    AssignError(Source);
end;

function TffSqlUniqueClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SubQuery.DependsOn(Table);
end;

destructor TffSqlUniqueClause.Destroy;
begin
  SubQuery.Free;
  inherited;
end;
{--------}
procedure TffSqlUniqueClause.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' UNIQUE (');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlUniqueClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlUniqueClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlUniqueClause)
    and (SubQuery.Equals(TffSqlUniqueClause(Other).SubQuery));
end;
{--------}
function TffSqlUniqueClause.Reduce: Boolean;
begin
  Result := SubQuery.Reduce;
end;

{====================================================================}

function OffsetTime(const DateTime: TDateTime; DeltaH, DeltaM, DeltaS: Integer): TDateTime;
var
  Mi, H, S, MSec : Word;
  Hs, Mis, Ss : Integer;
  DeltaD : Integer;
begin
  DecodeTime(DateTime, H, Mi, S, MSec);
  Hs := H;
  Mis := Mi;
  Ss := S;
  Ss := Ss + (DeltaS mod 60);
  Mis := Mis + (DeltaS div 60);
  if Ss < 0 then begin
    dec(Mis);
    inc(Ss, 60);
  end else
  if Ss >= 60 then begin
    inc(Mis);
    dec(Ss, 60);
  end;
  Mis := Mis + (DeltaM mod 60);
  Hs := Hs + (DeltaM div 60);
  if Mis < 0 then begin
    dec(Hs);
    inc(Mis, 60);
  end else
  if Mis >= 60 then begin
    inc(Hs);
    dec(Mis, 60);
  end;
  Hs := Hs + (DeltaH mod 24);
  DeltaD := (DeltaH div 24);
  if Hs < 0 then begin
    dec(DeltaD);
    inc(Hs, 24);
  end else
  if Hs >= 24 then begin
    inc(DeltaD);
    dec(Hs, 24);
  end;
  Result := Round(DateTime) + EncodeTime(Hs, Mis, Ss, MSec) + DeltaD;
end;

{===TffSqlIntervalLiteral============================================}
function TffSqlIntervalLiteral.AddIntervalTo(Target: TDateTime): TDateTime;
begin
  if not Converted then
    ConvertToNative;
  case StartDef of
  iYear :
    case EndDef of
    iUnspec :
      Result := IncMonth(Target, Y1 * 12);
    else //iMonth :
      Result := IncMonth(Target, Y1 * 12 + M1);
    end;
  iMonth :
    Result := IncMonth(Target, M1);
  iDay :
    case EndDef of
    iUnspec :
      Result := Target + D1;
    iHour :
      Result := OffsetTime(Target, H1, 0, 0) + D1;
    iMinute :
      Result := OffsetTime(Target, H1, M1, 0) + D1;
    else//iSecond :
      Result := OffsetTime(Target, H1, M1, S1) + D1;
    end;
  iHour :
    case EndDef of
    iUnspec :
      Result := OffsetTime(Target, H1, 0, 0);
    iMinute :
      Result := OffsetTime(Target, H1, M1, 0);
    else//iSecond :
      Result := OffsetTime(Target, H1, M1, S1);
    end;
  iMinute :
    case EndDef of
    iUnspec :
      Result := OffsetTime(Target, 0, M1, 0);
    else//iSecond :
      Result := OffsetTime(Target, 0, M1, S1);
    end;
  else //iSecond :
    Result := OffsetTime(Target, 0, 0, S1);
  end;
end;
{--------}
function TffSqlIntervalLiteral.SubtractIntervalFrom(Target: TDateTime): TDateTime;
begin
  if not Converted then
    ConvertToNative;
  case StartDef of
  iYear :
    case EndDef of
    iUnspec :
      Result := IncMonth(Target, -Y1 * 12);
    else//iMonth :
      Result := IncMonth(Target, -(Y1 * 12 + M1));
    end;
  iMonth :
    Result := IncMonth(Target, -M1);
  iDay :
    case EndDef of
    iUnspec :
      Result := Target - D1;
    iHour :
      Result := OffsetTime(Target, -H1, 0, 0) - D1;
    iMinute :
      Result := OffsetTime(Target, -H1, -M1, 0) - D1;
    else//iSecond :
      Result := OffsetTime(Target, -H1, -M1, -S1) - D1;
    end;
  iHour :
    case EndDef of
    iUnspec :
      Result := OffsetTime(Target, -H1, 0, 0);
    iMinute :
      Result := OffsetTime(Target, -H1, -M1, 0);
    else//iSecond :
      Result := OffsetTime(Target, -H1, -M1, -S1);
    end;
  iMinute :
    case EndDef of
    iUnspec :
      Result := OffsetTime(Target, 0, -M1, 0);
    else//iSecond :
      Result := OffsetTime(Target, 0, -M1, -S1);
    end;
  else//iSecond :
    Result := OffsetTime(Target, 0, 0, -S1);
  end;
end;
{--------}
procedure TffSqlIntervalLiteral.ConvertToNative;
var
  S : string;
  P : Integer;
begin
  S := Value;
  case StartDef of
  iUnspec :
    SQLError('Internal error in date/time interval literal');
  iYear :
    case EndDef of
    iUnspec :
      Y1 := StrToInt(copy(S, 2, length(S) - 2));
    iYear :
      SQLError('Syntax error in year-month interval literal');
    iMonth :
      begin
        P := PosCh('-', S);
        if P = 0 then
          SQLError('Syntax error in year-month interval literal: "-" expected');
        Y1 := StrToInt(copy(S, 2, P - 2));
        M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    else
      SQLError('Syntax error in year-month interval literal');
    end;
  iMonth :
    case EndDef of
    iUnspec :
      M1 := StrToInt(copy(S, 2, length(S) - 2));
    else
      SQLError('Syntax error in year-month interval literal');
    end;
  iDay :
    case EndDef of
    iUnspec :
      D1 := StrToInt(copy(S, 2, length(S) - 2));
    iHour :
      begin
        P := PosCh(' ', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: " " expected');
        D1 := StrToInt(copy(S, 2, P - 2));
        H1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    iMinute :
      begin
        P := PosCh(' ', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: " " expected');
        D1 := StrToInt(copy(S, 2, P - 2));
        Delete(S, 2, P - 2);
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        H1 := StrToInt(copy(S, 2, P - 2));
        M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    iSecond :
      begin
        P := PosCh(' ', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: " " expected');
        D1 := StrToInt(copy(S, 2, P - 2));
        Delete(S, 2, P - 1);
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        H1 := StrToInt(copy(S, 2, P - 2));
        Delete(S, 2, P - 1);
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        M1 := StrToInt(copy(S, 2, P - 2));
        S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    else
      SQLError('Syntax error in date-time interval literal');
    end;
  iHour :
    case EndDef of
    iUnspec :
      H1 := StrToInt(copy(S, 2, length(S) - 2));
    iMinute :
      begin
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        H1 := StrToInt(copy(S, 2, P - 2));
        M1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    iSecond :
      begin
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        H1 := StrToInt(copy(S, 2, P - 2));
        Delete(S, 2, P - 1);
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        M1 := StrToInt(copy(S, 2, P - 2));
        S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    else
      SQLError('Syntax error in date-time interval literal');
    end;
  iMinute :
    case EndDef of
    iUnspec :
      M1 := StrToInt(copy(S, 2, length(S) - 2));
    iSecond :
      begin;
        P := PosCh(':', S);
        if P = 0 then
          SQLError('Syntax error in date-time interval literal: ":" expected');
        M1 := StrToInt(copy(S, 2, P - 2));
        S1 := StrToInt(copy(S, P + 1, length(S) - P - 1));
      end;
    else
      SQLError('Syntax error in date-time interval literal');
    end;
  iSecond :
    case EndDef of
    iUnspec :
      S1 := StrToInt(copy(S, 2, length(S) - 2));
    else
      SQLError('Syntax error in date-time interval literal');
    end;
  else
    SQLError('Syntax error in date-time interval literal');
  end;
  Converted := True;
end;
{--------}
procedure TffSqlIntervalLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlIntervalLiteral then begin
    Value := TffSqlIntervalLiteral(Source).Value;
    StartDef := TffSqlIntervalLiteral(Source).StartDef;
    EndDef := TffSqlIntervalLiteral(Source).EndDef;
  end else
    AssignError(Source);
end;

procedure TffSqlIntervalLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' INTERVAL ');
  WriteStr(Stream, Value);
  WriteStr(Stream,' ');
  WriteStr(Stream, DefStr[StartDef]);
  if EndDef <> iUnspec then begin
    WriteStr(Stream,' TO ');
    WriteStr(Stream, DefStr[EndDef]);
  end;
end;
{--------}
procedure TffSqlIntervalLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlIntervalLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
   (Other is TffSqlIntervalLiteral)
   and (AnsiCompareText(Value, TffSqlIntervalLiteral(Other).Value) = 0)
   and (StartDef = TffSqlIntervalLiteral(Other).StartDef)
   and (EndDef = TffSqlIntervalLiteral(Other).EndDef);
end;
{--------}
function TffSqlIntervalLiteral.GetType: TffFieldType;
begin
  Result := fftInterval;
end;
{--------}
function TffSqlIntervalLiteral.GetValue: Variant;
begin
  Result := '';
  {This value returned to allow tests for NULL to pass}
end;
{--------}
procedure TffSqlIntervalLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftStDate,
  fftDateTime :
    ;
  else
    TypeMismatch;
  end;
  if not Converted then
    ConvertToNative;
end;
{====================================================================}

{===TffSqlTimestampLiteral===========================================}
procedure TffSqlTimestampLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlTimeStampLiteral then begin
    Value := TffSqlTimeStampLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlTimeStampLiteral.ConvertToNative;
begin
  if (length(Value) < 21)
  or not (Value[6] in ['-', '.', '/'])
  or (Value[9] <> Value[6])
  or (Value[12] <> ' ')
  or (Value[15] <> ':')
  or (Value[18] <> ':') then
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
end;

procedure TffSqlTimestampLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' TIMESTAMP ');
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlTimestampLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlTimestampLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlTimestampLiteral)
    and (AnsiCompareText(Value, TffSqlTimestampLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlTimestampLiteral.GetType: TffFieldType;
begin
  Result := fftDateTime;
end;

function TffSqlTimestampLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  Result := DateTimeValue;
end;
{--------}
procedure TffSqlTimestampLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftStTime,
  fftDateTime :
    ;
  else
    TypeMismatch;
  end;
  if not Converted then
    ConvertToNative;
end;
{====================================================================}

{===TffSqlTimeLiteral================================================}
procedure TffSqlTimeLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlTimeLiteral then begin
    Value := TffSqlTimeLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlTimeLiteral.ConvertToNative;
begin
  if (length(Value) <> 10)
  or (Value[4] <> ':')
  or (Value[7] <> ':') then
    SQLError('Syntax error in time literal');
  TimeValue := EncodeTime(
    StrToInt(copy(Value, 2, 2)),
    StrToInt(copy(Value, 5, 2)),
    StrToInt(copy(Value, 8, 2)),
    0);
  Converted := True;
end;
{--------}
procedure TffSqlTimeLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' TIME ');
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlTimeLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlTimeLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlTimeLiteral)
    and (AnsiCompareText(Value, TffSqlTimeLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlTimeLiteral.GetType: TffFieldType;
begin
  Result := fftStTime;
end;

function TffSqlTimeLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  Result := TimeValue;
end;
{--------}
procedure TffSqlTimeLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftStTime,
  fftDateTime :
    ;
  else
    TypeMismatch;
  end;
  if not Converted then
    ConvertToNative;
end;
{====================================================================}

{===TffSqlDateLiteral================================================}
{--------}
procedure TffSqlDateLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlDateLiteral then begin
    Value := TffSqlDateLiteral(Source).Value;
  end else
    AssignError(Source);
end;

procedure TffSqlDateLiteral.ConvertToNative;
begin
  if (length(Value) <> 12)
  or not (Value[6] in ['-', '.', '/'])
  or (Value[9] <> Value[6]) then
    SQLError('Syntax error in date literal');
  DateValue := EncodeDate(
    StrToInt(copy(Value, 2, 4)),
    StrToInt(copy(Value, 7, 2)),
    StrToInt(copy(Value, 10, 2)));
  Converted := True;
end;
{--------}
procedure TffSqlDateLiteral.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' DATE ');
  WriteStr(Stream, Value);
end;
{--------}
procedure TffSqlDateLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlDateLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlDateLiteral)
    and (AnsiCompareText(Value, TffSqlDateLiteral(Other).Value) = 0);
end;
{--------}
function TffSqlDateLiteral.GetType: TffFieldType;
begin
  Result := fftStDate;
end;

function TffSqlDateLiteral.GetValue: Variant;
begin
  if not Converted then
    ConvertToNative;
  Result := DateValue;
end;
{--------}
procedure TffSqlDateLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftStDate,
  fftDateTime :
    ;
  else
    TypeMismatch;
  end;
  if not Converted then
    ConvertToNative;
end;
{===TffSqlBooleanLiteral================================================}
{--------}
procedure TffSqlBooleanLiteral.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlBooleanLiteral then begin
    Value := TffSqlBooleanLiteral(Source).Value;
  end else
    AssignError(Source);
end;

{--------}
procedure TffSqlBooleanLiteral.EmitSQL(Stream: TStream);
begin
  if Value then
    WriteStr(Stream, ' TRUE')
  else
    WriteStr(Stream, ' FALSE');
end;
{--------}
procedure TffSqlBooleanLiteral.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
end;
{--------}
function TffSqlBooleanLiteral.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlBooleanLiteral)
    and (Value = TffSqlBooleanLiteral(Other).Value);
end;
{--------}
function TffSqlBooleanLiteral.GetType: TffFieldType;
begin
  Result := fftBoolean;
end;

function TffSqlBooleanLiteral.GetValue: Boolean;
begin
  Result := Value;
end;
{--------}
procedure TffSqlBooleanLiteral.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  fftBoolean : ;
  else
    TypeMismatch;
  end;
end;
{====================================================================}

const
  FuncStr : array[TffSqlScalarFunction] of string = (
  'CASE', 'CHARACTER_LENGTH','COALESCE', 'CURRENT_DATE','CURRENT_TIME','CURRENT_TIMESTAMP',
  'CURRENT_USER','LOWER','UPPER','POSITION','SESSION_USER','SUBSTRING',
  'SYSTEM_USER','TRIM','EXTRACT', 'NULLIF',
  'ABS', 'CEIL', 'FLOOR', 'EXP', 'LOG', 'POWER', 'RAND', 'ROUND');     {!!.11}
  LeadStr : array[TffSqlLTB] of string = ('BOTH', 'LEADING', 'TRAILING');
{===TffSqlScalarFunc=================================================}
procedure TffSqlScalarFunc.CheckIsConstant;
begin
  FIsConstantChecked := True;
  case SQLFunction of
  sfCase :
    FIsConstant := CaseExp.IsConstant;
  sfCharlen :
    FIsConstant := Arg1.IsConstant;
  sfCoalesce :
    FIsConstant := False;
  sfCurrentDate :
    FIsConstant := True;
  sfCurrentTime :
    FIsConstant := True;
  sfCurrentTimestamp :
    FIsConstant := True;
  sfCurrentUser :
    FIsConstant := True;
  sfLower :
    FIsConstant := Arg1.IsConstant;
  sfUpper :
    FIsConstant := Arg1.IsConstant;
  sfPosition :
    FIsConstant := Arg2.IsConstant and Arg1.IsConstant;
  sfSessionUser :
    FIsConstant := True;
  sfSubstring :
    FIsConstant :=
      Arg1.IsConstant and Arg2.IsConstant and
       ((Arg3 = nil) or (Arg3.IsConstant));
  sfSystemUser :
    FIsConstant := True;
  sfTrim :
    FIsConstant :=
     ((Arg1 = nil) or (Arg1.IsConstant))
      and ((Arg2 = nil) or (Arg2.IsConstant));
  sfExtract :
    FIsConstant := Arg1.IsConstant;
  sfNullIf :
    FIsConstant := Arg2.IsConstant and Arg1.IsConstant;
  {!!.11 begin}
  sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :
    FIsConstant := Arg1.IsConstant;
  sfRand :
    FIsConstant := False;
  sfPower :
    FIsConstant := Arg2.IsConstant and Arg1.IsConstant;
  {!!.11 end}
  else
    Assert(False);
  end;
  if FIsConstant then begin
    FIsConstant := False;
    ConstantValue := GetValue;
    FIsConstant := True;
  end;
end;

procedure TffSqlScalarFunc.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlScalarFunc then begin
    Clear;
    SQLFunction := TffSqlScalarFunc(Source).SQLFunction;
    if assigned(TffSqlScalarFunc(Source).Arg1) then begin
      Arg1 := TffSqlSimpleExpression.Create(Self);
      Arg1.Assign(TffSqlScalarFunc(Source).Arg1);
    end;
    if assigned(TffSqlScalarFunc(Source).Arg2) then begin
      Arg2 := TffSqlSimpleExpression.Create(Self);
      Arg2.Assign(TffSqlScalarFunc(Source).Arg2);
    end;
    if assigned(TffSqlScalarFunc(Source).Arg3) then begin
      Arg3 := TffSqlSimpleExpression.Create(Self);
      Arg3.Assign(TffSqlScalarFunc(Source).Arg3);
    end;
    LTB := TffSqlScalarFunc(Source).LTB;
    XDef := TffSqlScalarFunc(Source).XDef;
    if assigned(TffSqlScalarFunc(Source).CaseExp) then begin
      CaseExp := TffSqlCaseExpression.Create(Self);
      CaseExp.Assign(TffSqlScalarFunc(Source).CaseExp);
    end;
    if assigned(TffSqlScalarFunc(Source).CoalesceExp) then begin
      CoalesceExp := TffSqlCoalesceExpression.Create(Self);
      CoalesceExp.Assign(TffSqlScalarFunc(Source).CoalesceExp);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlScalarFunc.Clear;
begin
  CaseExp.Free;
  CoalesceExp.Free;
  Arg1.Free;
  Arg2.Free;
  Arg3.Free;
  CaseExp:= nil;
  CoalesceExp:= nil;
  Arg1:= nil;
  Arg2:= nil;
  Arg3:= nil;
end;

function TffSqlScalarFunc.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  case SQLFunction of
  sfCase :
    Result := CaseExp.DependsOn(Table);
  sfCharlen,
  sfLower,
  sfUpper,
  sfExtract :
    Result := Arg1.DependsOn(Table);
  sfCoalesce :
    Result := CoalesceExp.DependsOn(Table);
  sfSystemUser,
  sfCurrentDate,
  sfCurrentTime,
  sfCurrentTimestamp,
  sfCurrentUser,
  sfSessionUser :
    Result := False;
  sfPosition :
    Result := Arg2.DependsOn(Table) or Arg1.DependsOn(Table);
  sfSubstring :
    begin
      Result := Arg1.DependsOn(Table) or Arg2.DependsOn(Table);
      if not Result and (Arg3 <> nil) then
        Result := Arg3.DependsOn(Table);
    end;
  sfTrim :
    begin
      if Arg2 = nil then
        Result := Arg1.DependsOn(Table)
      else
        Result := Arg1.DependsOn(Table) or Arg2.DependsOn(Table)
    end;
  sfNullIf :
    begin
      Result := Arg1.DependsOn(Table) or Arg2.DependsOn(Table);
    end;
  {!!.11 begin}
  sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :
    Result := Arg1.DependsOn(Table) ;
  sfRand :
    Result := False;
  sfPower :
    Result := Arg1.DependsOn(Table) or Arg2.DependsOn(Table);
  {!!.11 end}
  else
    Assert(False);
    Result := False;
  end;
end;

destructor TffSqlScalarFunc.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlScalarFunc.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ' ');
  case SQLFunction of
  sfCase :
    CaseExp.EmitSQL(Stream);
  sfCoalesce :
    CoalesceExp.EmitSQL(Stream);
  sfCurrentDate,
  sfCurrentTime,
  sfCurrentTimestamp,
  sfCurrentUser,
  sfSessionUser,
  sfSystemUser,
  sfRand :                                                             {!!.11}
    WriteStr(Stream, FuncStr[SQLFunction]);
  else
    WriteStr(Stream, FuncStr[SQLFunction]);
    WriteStr(Stream,'(');
    case SQLFunction of
    sfCharlen,
    sfLower,
    sfUpper,
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :                    {!!.11}
      begin
        Arg1.EmitSQL(Stream);
      end;
    sfNullIf,
    sfPosition,
    sfPower :                                                          {!!.11}
      begin
        Arg1.EmitSQL(Stream);
        WriteStr(Stream,' , ');
        Arg2.EmitSQL(Stream);
      end;
    sfSubstring :
      begin
        Arg1.EmitSQL(Stream);
        WriteStr(Stream,' FROM ');
        Arg2.EmitSQL(Stream);
        if Arg3 <> nil then begin
          WriteStr(Stream,' FOR ');
          Arg3.EmitSQL(Stream);
        end;
      end;
    sfTrim :
      begin
        WriteStr(Stream, LeadStr[LTB]);
        WriteStr(Stream, ' ');
        if Arg1 <> nil then
          Arg1.EmitSQL(Stream);
        if Arg2 <> nil then begin
          WriteStr(Stream,' FROM ');
          Arg2.EmitSQL(Stream);
        end;
      end;
    sfExtract :
      begin
        WriteStr(Stream, DefStr[XDef]);
        WriteStr(Stream,' FROM ');
        Arg1.EmitSQL(Stream);
      end;
    end;
    WriteStr(Stream,')');
  end;
end;
{--------}
procedure TffSqlScalarFunc.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  case SQLFunction of
  sfCase :
    CaseExp.EnumNodes(EnumMethod, Deep);
  sfCoalesce :
    CoalesceExp.EnumNodes(EnumMethod, Deep);
  sfCurrentDate,
  sfCurrentTime,
  sfCurrentTimestamp,
  sfCurrentUser,
  sfSessionUser,
  sfSystemUser,
  sfRand :                                                             {!!.11}
    ;
  else
    case SQLFunction of
    sfCharlen,
    sfLower,
    sfUpper,
    sfExtract,
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :                    {!!.11}
      Arg1.EnumNodes(EnumMethod, Deep);
    sfNullIf,
    sfPosition,
    sfPower :                                                          {!!.11}
      begin
        Arg1.EnumNodes(EnumMethod, Deep);
        Arg2.EnumNodes(EnumMethod, Deep);
      end;
    sfSubstring :
      begin
        Arg1.EnumNodes(EnumMethod, Deep);
        Arg2.EnumNodes(EnumMethod, Deep);
        if Arg3 <> nil then
          Arg3.EnumNodes(EnumMethod, Deep);
      end;
    sfTrim :
      begin
        if Arg1 <> nil then
          Arg1.EnumNodes(EnumMethod, Deep);
        if Arg2 <> nil then
          Arg2.EnumNodes(EnumMethod, Deep);
      end;
    end;
  end;
end;
{--------}
function TffSqlScalarFunc.Equals(Other: TffSqlNode): Boolean;
begin
  Result := False;
  if Other is TffSqlScalarFunc then begin
    if SQLFunction <> TffSqlScalarFunc(Other).SQLFunction then
      exit;
    case SQLFunction of
    sfCase :
      if not CaseExp.Equals(TffSqlScalarFunc(Other).CaseExp) then
        exit;
    sfCoalesce :
      if not CoalesceExp.Equals(TffSqlScalarFunc(Other).CoalesceExp) then
        exit;
    sfCharlen,
    sfLower,
    sfUpper,
    sfExtract,
    sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :                    {!!.11}
      if not Arg1.Equals(TffSqlScalarFunc(Other).Arg1) then
        exit;
    sfNullIf,
    sfPosition,
    sfPower :                                                          {!!.11}
      begin
        if not Arg1.Equals(TffSqlScalarFunc(Other).Arg1) then
          exit;
        if not Arg2.Equals(TffSqlScalarFunc(Other).Arg2) then
          exit;
      end;
    sfSubstring :
      begin
        if not Arg1.Equals(TffSqlScalarFunc(Other).Arg1) then
          exit;
        if not Arg2.Equals(TffSqlScalarFunc(Other).Arg2) then
          exit;
        if not (
          BothNil(Arg3, TffSqlScalarFunc(Other).Arg3)
          or (BothNonNil(Arg3, TffSqlScalarFunc(Other).Arg3)
            and Arg3.Equals(TffSqlScalarFunc(Other).Arg3))) then
            exit;
      end;
    sfTrim :
      begin
        if not (
          BothNil(Arg1, TffSqlScalarFunc(Other).Arg1)
          or (BothNonNil(Arg1, TffSqlScalarFunc(Other).Arg1)
            and Arg1.Equals(TffSqlScalarFunc(Other).Arg1))) then
            exit;
        if not (
          BothNil(Arg2, TffSqlScalarFunc(Other).Arg2)
          or (BothNonNil(Arg2, TffSqlScalarFunc(Other).Arg2)
            and Arg2.Equals(TffSqlScalarFunc(Other).Arg2))) then
            exit;
      end;
    end;
    Result := True;
  end;
end;
{--------}
function TffSqlScalarFunc.GetDecimals: Integer;
begin
  Result := 0;
end;
{--------}
function TffSqlScalarFunc.GetSize: Integer;
var
  S : string;
begin
  {should only be called on text functions}
  case SQLFunction of
  sfCase :
    Result := CaseExp.GetSize;
  sfLower,
  sfUpper,
  sfSubstring :
    Result := Arg1.GetSize;
  sfTrim :
    if Arg2 = nil then
      Result := Arg1.GetSize
    else
      Result := Arg2.GetSize;
  sfCoalesce :
    Result := CoalesceExp.GetSize;
  sfCurrentUser,
  sfSystemUser,
  sfSessionUser :
    begin
      S := GetValue;
      Result := length(S);
    end;
  sfNullIf :
    Result := Arg1.GetSize;
  else
    Result := 0;
  end;
end;
{--------}
function TffSqlScalarFunc.GetTitle(const Qualified : Boolean): string; {!!.11}
begin
  Result := FuncStr[SQLFunction];
end;
{--------}
procedure TffSqlScalarFunc.CheckType;
begin
  case SQLFunction of
  sfCase :
    FType := CaseExp.GetType;
  sfCharlen :
    begin
      Arg1.MatchType(fftShortString);
      FType := fftInt32;
    end;
  sfCoalesce :
    FType := CoalesceExp.GetType;
  sfCurrentDate :
    FType := fftStDate;
  sfCurrentTime :
    FType := fftStTime;
  sfCurrentTimestamp :
    FType := fftDateTime;
  sfCurrentUser :
    FType := fftShortAnsiStr;
  sfLower :
    begin
      Arg1.MatchType(fftShortString);
      FType := fftShortAnsiStr;
    end;
  sfUpper :
    begin
      Arg1.MatchType(fftShortString);
      FType := fftShortAnsiStr;
    end;
  sfPosition :
    begin
      Arg1.MatchType(fftShortString);
      Arg2.MatchType(fftShortString);
      FType := fftInt32;
    end;
  sfSessionUser :
    FType := fftShortAnsiStr;
  sfSubstring :
    begin
      Arg1.MatchType(fftShortString);
      Arg2.MatchType(fftInt32);
      if Arg3 <> nil then
        Arg3.MatchType(fftInt32);
      FType := fftShortAnsiStr;
    end;
  sfSystemUser :
    FType := fftShortAnsiStr;
  sfTrim :
    begin
      if Arg1 <> nil then
        Arg1.MatchType(fftShortString);
      if Arg2 <> nil then
        Arg2.MatchType(fftShortString);
      FType := fftShortAnsiStr;
    end;
  sfExtract :
    begin
      Arg1.MatchType(fftDateTime);
      FType := fftInt32;
    end;
  sfNullIf :
    FType := Arg1.GetType;
  {!!.11 begin}
  sfAbs, {sfCeil, sfFloor, }sfExp, sfLog, sfRound, sfRand, sfPower :   {!!.12}
    FType := fftDouble;
  {!!.11 end}
  sfCeil, sfFloor:                                                     {!!.12}
    case Arg1.GetType of                                               {!!.12}
    fftStDate..fftDateTime :                                           {!!.12}
      FType := Arg1.GetType;                                           {!!.12}
    else                                                               {!!.12}
      FType := fftDouble;                                              {!!.12}
    end;                                                               {!!.12}
  else
    Assert(False);
  end;
  TypeKnown := True;
end;
{--------}
function TffSqlScalarFunc.GetType: TffFieldType;
begin
  if not TypeKnown then
    CheckType;
  Result := FType;
end;
{Begin !!.13}
{--------}
function ConvertBLOBToString(const Value : Variant) : string;
  { Converts a BLOB value to a string value.
    Assumption: Value is always a var array of byte }
var
  ResultLen : Longint;
  VPtr : PAnsiChar;
begin
  ResultLen :=  VarArrayHighBound(Value, 1);
  SetLength(Result, ResultLen);
  VPtr := VarArrayLock(Value);
  try
    Move(VPtr^, Result[1], ResultLen);
  finally
    VarArrayUnlock(Value);
  end;
end;
{End !!.13}
{--------}
function TffSqlScalarFunc.GetValue: Variant;
{Revised !!.13 - Scalar functions updated to recognize BLOB fields as
  arrays of bytes instead of as strings. }
var
  S : string;
  WS, WS2 : widestring;                                                {!!.11}
  I1, I2 : Integer;
  Y, M, D : Word;
  Hour, Min, Sec, MSec : Word;
  Ch : Char;
  DT : TDateTime;
  V, V2 : Variant;                                                     {!!.11}
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  case SQLFunction of
  sfCase :
    Result := CaseExp.GetValue;
  sfCharlen :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else if (VarType(V) and VarTypeMask = varByte) then
        Result := VarArrayHighBound(V, 1)
      else
        Result := length(V);
    end;
  sfCoalesce :
    Result := CoalesceExp.GetValue;
  sfCurrentDate :
    Result := Owner.StartDate;
  sfCurrentTime :
    Result := Owner.StartTime;
  sfCurrentTimestamp :
    Result := Owner.StartDateTime;
  sfCurrentUser :
    Result := IntToStr(Owner.FClientID);
  sfLower :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else if (VarType(V) and VarTypeMask = varByte) then
        Result := AnsiLowerCase(ConvertBLOBToString(V))
      else
        Result := AnsiLowerCase(V);
    end;
  sfUpper :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else if (VarType(V) and VarTypeMask = varByte) then
        Result := AnsiUpperCase(ConvertBLOBToString(V))
      else
        Result := AnsiUpperCase(V);
    end;
  sfPosition :
    begin
      V := Arg1.GetValue;
      V2 := Arg2.GetValue;
      if VarIsNull(V) or VarIsNull(V2) then
        Result := 0
      else begin
        WS := V;
        if WS = '' then
          Result := 1
        else begin
          if (VarType(V2) and VarTypeMask = varByte) then
            WS2 := ConvertBLOBToString(V2)
          else
            WS2 := V2;
          Result := Pos(WS, WS2);
        end;  { if }
      end;  { if }
    end;
  sfSessionUser :
    Result := IntToStr(Owner.FSessionID);
  sfSubstring :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else begin
        if (VarType(V) and VarTypeMask = varByte) then
          S := ConvertBLOBToString(V)
        else
          S := V;
        I1 := Arg2.GetValue;
        if Arg3 = nil then
          Result := copy(S, I1, length(S))
        else begin
          I2 := Arg3.GetValue;
          Result := copy(S, I1, I2);
        end;
      end;
    end;
  sfSystemUser :
    SQLError('SYSTEM_USER is not supported at this time');
  sfTrim :
    begin
      if Arg2 = nil then begin
        V := Arg1.GetValue;
        if VarIsNull(V) then begin
          Result := V;
          Exit;
        end;
        if (VarType(V) and VarTypeMask = varByte) then
          S := ConvertBLOBToString(V)
        else
          S := V;
        Ch := ' ';
      end else
      if Arg1 = nil then begin
        V := Arg2.GetValue;
        if VarIsNull(V) then begin
          Result := V;
          Exit;
        end;
        if (VarType(V) and VarTypeMask = varByte) then
          S := ConvertBLOBToString(V)
        else
          S := V;
        Ch := ' ';
      end else begin
        V := Arg1.GetValue;
        if VarIsNull(V) then begin
          Result := V;
          Exit;
        end;
        if (VarType(V) and VarTypeMask = varByte) then
          S := ConvertBLOBToString(V)
        else
          S := V;
        Ch := S[1];
        V := Arg2.GetValue;
        if VarIsNull(V) then
          S := ''
        else if (VarType(V) and VarTypeMask = varByte) then
          S := ConvertBLOBToString(V)
        else
          S := V;
      end;
      case LTB of
      ltbBoth :
        begin
          while (length(S) > 0) and (S[1] = Ch) do
            Delete(S, 1, 1);
          while (length(S) > 0) and (S[length(S)] = Ch) do
            Delete(S, length(S), 1);
        end;
      ltbLeading :
        while (length(S) > 0) and (S[1] = Ch) do
          Delete(S, 1, 1);
      ltbTrailing :
        while (length(S) > 0) and (S[length(S)] = Ch) do
          Delete(S, length(S), 1);
      end;
      Result := S;
    end;
  sfExtract :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then begin
        Result := V;
        exit;
      end;
      DT := V;
      case XDef of
      iYear :
        begin
          DecodeDate(DT, Y, M, D);
          Result := Y;
        end;
      iMonth :
        begin
          DecodeDate(DT, Y, M, D);
          Result := M;
        end;
      iDay :
        begin
          DecodeDate(DT, Y, M, D);
          Result := D;
        end;
      iHour :
        begin
          DecodeTime(DT, Hour, Min, Sec, MSec);
          Result := Hour;
        end;
      iMinute:
        begin
          DecodeTime(DT, Hour, Min, Sec, MSec);
          Result := Min;
        end;
      else
      //iSecond:
        begin
          DecodeTime(DT, Hour, Min, Sec, MSec);
          Result := Sec;
        end;
      end;
    end;
  sfNullIf :
    begin
      V := Arg1.GetValue;
      if V = Arg2.GetValue then
        Result := Null
      else
        Result := V;
    end;
  sfAbs :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := abs(V);
    end;
  sfCeil :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := Ceil(V);
    end;
  sfFloor :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := Floor(V);
    end;
  sfExp :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := Exp(V);
    end;
  sfLog :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := Ln(V);
    end;
  sfRound :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else
        Result := 1.0 * Round(V);
    end;
  sfRand :
    Result := Random;
  sfPower :
    begin
      V := Arg1.GetValue;
      if VarIsNull(V) then
        Result := V
      else begin
        V2 := Arg2.GetValue;
        if VarIsNull(V2) then
          Result := V2
        else
          Result := Power(V, V2);
      end;
    end;
  else
    Assert(False);
  end;
end;

function TffSqlScalarFunc.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

function TffSqlScalarFunc.IsFieldFrom(Table: TFFSqlTableProxy;
  var FieldReferenced: TFFSqlFieldProxy): Boolean;
var
  SameCase: Boolean;
begin
  if SQLFunction in [sfUpper, sfLower] then
    Result := Arg1.IsFieldFrom(Table, FieldReferenced, SameCase)
  else
    Result := False;
end;

procedure TffSqlScalarFunc.MatchType(ExpectedType: TffFieldType);
begin
  case ExpectedType of
  {!!.11 begin}
  fftChar,
  fftWideChar,
  fftShortString,
  fftShortAnsiStr,
  fftNullString,
  fftNullAnsiStr,
  fftWideString,
  fftBLOB..fftBLOBTypedBin :
    case GetType of
    fftChar,
    fftWideChar,
    fftShortString,
    fftShortAnsiStr,
    fftNullString,
    fftNullAnsiStr,
    fftWideString,
    fftBLOB..fftBLOBTypedBin :
      ; {ok}
    else
      TypeMismatch;
    end;
  {!!.11 end}
  fftStDate,
  fftStTime,
  fftDateTime:
    case GetType of
    fftStDate,
    fftStTime,
    fftDateTime:
      ; {ok}
    else
      TypeMismatch;
    end;
  else
    if GetType <> ExpectedType then
      TypeMismatch;
  end;
end;
{--------}
function TffSqlScalarFunc.Reduce: Boolean;
begin
  case SQLFunction of
  sfCase :
    Result := CaseExp.Reduce;
  sfCharlen :
    Result := Arg1.Reduce;
  sfCoalesce :
    Result := CoalesceExp.Reduce;
  sfCurrentDate :
    Result := False;
  sfCurrentTime :
    Result := False;
  sfCurrentTimestamp :
    Result := False;
  sfCurrentUser :
    Result := False;
  sfLower :
    Result := Arg1.Reduce;
  sfUpper :
    Result := Arg1.Reduce;
  sfPosition :
    begin
      Result := Arg1.Reduce;
      if not Result and (Arg2 <> nil) then
        Result := Arg2.Reduce;
    end;
  sfSessionUser :
    Result := False;
  sfSubstring :
    begin
      Result := Arg1.Reduce or Arg2.Reduce;
      if not Result and (Arg3 <> nil) then
        Result := Arg3.Reduce;
    end;
  sfSystemUser :
    Result := False;
  sfTrim :
    begin
      if Arg2 = nil then begin
        Result := Arg1.Reduce
      end else
      if Arg1 = nil then begin
        Result := Arg2.Reduce;
      end else begin
        Result := Arg1.Reduce or Arg2.Reduce;
      end;
    end;
  sfExtract :
    begin
      Result := Arg1.Reduce;
    end;
  sfNullIf :
    begin
      Result := Arg1.Reduce or Arg2.Reduce;
    end;
  {!!.11 begin}
  sfAbs, sfCeil, sfFloor, sfExp, sfLog, sfRound :
    Result := Arg1.Reduce;
  sfRand :
    Result := False;
  sfPower :
    Result := Arg1.Reduce or Arg2.Reduce;
  {!!.11 end}
  else
    Result := False;
  end;
end;
{--------}
procedure TffSqlScalarFunc.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

{====================================================================}

{===TffSqlWhenClauseList=============================================}
function TffSqlWhenClauseList.AddWhenClause(Value: TffSqlWhenClause): TffSqlWhenClause;
begin
  WhenClauseList.Add(Value);
  Result := Value;
end;
{--------}
procedure TffSqlWhenClauseList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlWhenClauseList then begin
    Clear;
    for i := 0 to pred(TffSqlWhenClauseList(Source).WhenClauseCount) do
      AddWhenClause(TffSqlWhenClause.Create(Self)).Assign(
        TffSqlWhenClauseList(Source).WhenClause[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlWhenClauseList.CheckIsConstant;
var
  i : Integer;
begin
  FIsConstantChecked := True;
  for i := 0 to pred(WhenClauseCount) do
    if not WhenClause[i].IsConstant then begin
      FIsConstant := False;
      exit;
    end;
  FIsConstant := True;
end;

constructor TffSqlWhenClauseList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  WhenClauseList := TList.Create;
end;
{--------}
function TffSqlWhenClauseList.DependsOn(Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(WhenClauseCount) do
    if WhenClause[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
procedure TffSqlWhenClauseList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(WhenClauseCount) do
    WhenClause[i].Free;
  WhenClauseList.Clear;
end;
{--------}
destructor TffSqlWhenClauseList.Destroy;
begin
  Clear;
  WhenClauseList.Free;
  inherited;
end;
{--------}
procedure TffSqlWhenClauseList.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  for i := 0 to pred(WhenClauseCount) do
    WhenClause[i].EmitSQL(Stream);
end;
{--------}
procedure TffSqlWhenClauseList.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(WhenClauseCount) do
    WhenClause[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlWhenClauseList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlWhenClauseList then begin
    if WhenClauseCount <> TffSqlWhenClauseList(Other).WhenClauseCount then
      exit;
    for i := 0 to pred(WhenClauseCount) do
      if not WhenClause[i].Equals(TffSqlWhenClauseList(Other).WhenClause[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlWhenClauseList.GetWhenClause(
  Index: Integer): TffSqlWhenClause;
begin
  Result := TffSqlWhenClause(WhenClauseList[Index]);
end;
{--------}
function TffSqlWhenClauseList.GetWhenClauseCount: Integer;
begin
  Result := WhenClauseList.Count;
end;
{--------}
function TffSqlWhenClauseList.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

procedure TffSqlWhenClauseList.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

{====================================================================}

{===TffSqlWhenClause=================================================}
procedure TffSqlWhenClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlWhenClause then begin
    if WhenExp = nil then
      WhenExp := TffSqlCondExp.Create(Self);
    WhenExp.Assign(TffSqlWhenClause(Source).WhenExp);
    ThenExp.Free;
    ThenExp := nil;
    if assigned(TffSqlWhenClause(Source).ThenExp) then begin
      ThenExp := TffSqlSimpleExpression.Create(Self);
      ThenExp.Assign(TffSqlWhenClause(Source).ThenExp);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlWhenClause.CheckIsConstant;
begin
  FIsConstantChecked := True;
  FIsConstant := WhenExp.IsConstant and
    (not assigned(ThenExp) or
    ThenExp.IsConstant);
end;
{--------}
function TffSqlWhenClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := WhenExp.DependsOn(Table) or
    ((ThenExp <> nil) and ThenExp.DependsOn(Table));
end;

destructor TffSqlWhenClause.Destroy;
begin
  WhenExp.Free;
  ThenExp.Free;
  inherited;
end;

procedure TffSqlWhenClause.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' WHEN ');
  WhenExp.EmitSQL(Stream);
  WriteStr(Stream,' THEN ');
  if ThenExp <> nil then
    ThenExp.EmitSQL(Stream)
  else
    WriteStr(Stream,' NULL');
end;
{--------}
procedure TffSqlWhenClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  WhenExp.EnumNodes(EnumMethod, Deep);
  if assigned(ThenExp) then
    ThenExp.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlWhenClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlWhenClause)
    and (WhenExp.Equals(TffSqlWhenClause(Other).WhenExp))
    and
      BothNil(ThenExp, TffSqlWhenClause(Other).ThenExp)
      or (BothNonNil(ThenExp, TffSqlWhenClause(Other).ThenExp)
        and (ThenExp.Equals(TffSqlWhenClause(Other).ThenExp)));
end;
{--------}
function TffSqlWhenClause.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;

procedure TffSqlWhenClause.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;


{====================================================================}

{===TffSqlCaseExpression=============================================}
procedure TffSqlCaseExpression.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlCaseExpression then begin
    if WhenClauseList = nil then
      WhenClauseList := TffSqlWhenClauseList.Create(Self);
    WhenClauseList.Assign(TffSqlCaseExpression(Source).WhenClauseList);
    ElseExp.Free;
    ElseExp := nil;
    if Assigned(TffSqlCaseExpression(Source).ElseExp) then begin
      ElseExp := TffSqlSimpleExpression.Create(Self);
      ElseExp.Assign(TffSqlCaseExpression(Source).ElseExp);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlCaseExpression.CheckIsConstant;
begin
  FIsConstantChecked := True;
  FIsConstant :=
    WhenClauseList.IsConstant and ((ElseExp = nil) or ElseExp.IsConstant);
  if FIsConstant then begin
    FIsConstant := False;
    ConstantValue := GetValue;
    FIsConstant := True;
  end;
end;

function TffSqlCaseExpression.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := WhenClauseList.DependsOn(Table) or
    (ElseExp <> nil) and ElseExp.DependsOn(Table);
end;

destructor TffSqlCaseExpression.Destroy;
begin
  WhenClauseList.Free;
  ElseExp.Free;
  inherited;
end;

procedure TffSqlCaseExpression.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' CASE');
  WhenClauseList.EmitSQL(Stream);
  WriteStr(Stream,' ELSE ');
  if ElseExp <> nil then
    ElseExp.EmitSQL(Stream)
  else
    WriteStr(Stream, 'NULL');
  WriteStr(Stream,' END');
end;
{--------}
procedure TffSqlCaseExpression.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  WhenClauseList.EnumNodes(EnumMethod, Deep);
  if ElseExp <> nil then
    ElseExp.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlCaseExpression.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlCaseExpression)
    and WhenClauseList.Equals(TffSqlCaseExpression(Other).WhenClauseList)
    and (BothNil(ElseExp, TffSqlCaseExpression(Other).ElseExp)
      or (BothNonNil(ElseExp, TffSqlCaseExpression(Other).ElseExp)
           and
           ElseExp.Equals(TffSqlCaseExpression(Other).ElseExp)
         )
      );
end;
{--------}
function TffSqlCaseExpression.GetSize: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to pred(WhenClauseList.WhenClauseCount) do
    if WhenClauseList.WhenClause[i].ThenExp <> nil then
      Result := FFMaxI(Result, WhenClauseList.WhenClause[i].ThenExp.GetSize);
  if ElseExp <> nil then
    Result := FFMaxI(Result, ElseExp.GetSize);
end;

function TffSqlCaseExpression.GetType: TffFieldType;
begin
  if WhenClauseList.WhenClause[0].ThenExp <> nil then
    Result := WhenClauseList.WhenClause[0].ThenExp.GetType
  else
    Result := fftShortString; {actually, NULL}
end;

function TffSqlCaseExpression.GetValue: Variant;
var
  i : Integer;
begin
  if IsConstant then begin
    Result := ConstantValue;
    exit;
  end;
  for i := 0 to pred(WhenClauseList.WhenClauseCount) do
    if WhenClauseList.WhenClause[i].WhenExp.AsBoolean then begin
      if WhenClauseList.WhenClause[i].ThenExp <> nil then
        Result := WhenClauseList.WhenClause[i].ThenExp.GetValue
      else
        Result := Null;
      exit;
    end;
  if ElseExp <> nil then
    Result := ElseExp.GetValue
  else
    Result := Null;
end;
{--------}
function TffSqlCaseExpression.IsConstant: Boolean;
begin
  if not FIsConstantChecked then
    CheckIsConstant;
  Result := FIsConstant;
end;
{--------}
function TffSqlCaseExpression.Reduce: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(WhenClauseList.WhenClauseCount) do
    if WhenClauseList.WhenClause[i].WhenExp.Reduce then begin
      Result := True;
      exit;
    end else
    if WhenClauseList.WhenClause[i].ThenExp <> nil then
      if WhenClauseList.WhenClause[i].ThenExp.Reduce then begin
        Result := True;
        exit;
      end;
  if ElseExp <> nil then
    Result := ElseExp.Reduce
  else
    Result := False;
end;

procedure TffSqlCaseExpression.ResetConstant;
begin
  FIsConstantChecked := False;
  FIsConstant := False;
end;

{====================================================================}

{===TffSqlMatchClause================================================}
function TffSqlMatchClause.AsBoolean(const TestValue: Variant): Boolean;
begin
  Result := SubQuery.Match(TestValue, Unique, Option)
end;
{--------}
procedure TffSqlMatchClause.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlMatchClause then begin
    Unique := TffSqlMatchClause(Source).Unique;
    Option := TffSqlMatchClause(Source).Option;
    SubQuery.Free;
    SubQuery := TffSqlSELECT.Create(Self);
    SubQuery.Assign(TffSqlMatchClause(Source).SubQuery);
  end else
    AssignError(Source);
end;

function TffSqlMatchClause.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Result := SubQuery.DependsOn(Table);
end;

destructor TffSqlMatchClause.Destroy;
begin
  SubQuery.Free;
  inherited;
end;
{--------}
procedure TffSqlMatchClause.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ' MATCH');
  if Unique then
    WriteStr(Stream,' UNIQUE');
  case Option of
  moPartial :
    WriteStr(Stream,' PARTIAL');
  moFull :
    WriteStr(Stream,' FULL');
  end;
  WriteStr(Stream,'(');
  SubQuery.EmitSQL(Stream);
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlMatchClause.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
begin
  EnumMethod(Self);
  SubQuery.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlMatchClause.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlMatchClause)
    and (Unique = TffSqlMatchClause(Other).Unique)
    and (Option = TffSqlMatchClause(Other).Option)
    and (SubQuery.Equals(TffSqlMatchClause(Other).SubQuery));
end;
{--------}
procedure TffSqlMatchClause.MatchType(ExpectedType: TffFieldType);
begin
  SubQuery.MatchType(ExpectedType, False);
end;

function TffSqlMatchClause.Reduce: Boolean;
begin
  Result := SubQuery.Reduce;
end;

{====================================================================}
{ TffSqlCoalesceExpression }
function TffSqlCoalesceExpression.AddArg(Value: TffSqlSimpleExpression): TffSqlSimpleExpression;
begin
  ArgList.Add(Value);
  Result := Value;
end;
{--------}
procedure TffSqlCoalesceExpression.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlCoalesceExpression then begin
    Clear;
    for i := 0 to pred(TffSqlCoalesceExpression(Source).ArgCount) do
      AddArg(TffSqlSimpleExpression.Create(Self)).Assign(
        TffSqlCoalesceExpression(Source).Arg[i]);
  end else
    AssignError(Source);
end;

constructor TffSqlCoalesceExpression.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  ArgList := TList.Create;
end;
{--------}
procedure TffSqlCoalesceExpression.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(ArgCount) do
    Arg[i].Free;
  ArgList.Clear;
end;
{--------}
destructor TffSqlCoalesceExpression.Destroy;
begin
  Clear;
  ArgList.Free;
  inherited;
end;
{--------}
procedure TffSqlCoalesceExpression.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  WriteStr(Stream,' COALESCE(');
  Arg[0].EmitSQL(Stream);
  for i := 1 to pred(ArgCount) do begin
    WriteStr(Stream,' ,');
    Arg[i].EmitSQL(Stream);
  end;
  WriteStr(Stream,')');
end;
{--------}
procedure TffSqlCoalesceExpression.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(ArgCount) do
    Arg[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlCoalesceExpression.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlCoalesceExpression then
    if ArgCount = TffSqlCoalesceExpression(Other).ArgCount then begin
      for i := 0 to pred(ArgCount) do
        if not Arg[i].Equals(TffSqlCoalesceExpression(Other).Arg[i]) then
          exit;
      Result := True;
    end;
end;
{--------}
function TffSqlCoalesceExpression.GetArg(
  Index: Integer): TffSqlSimpleExpression;
begin
  Result := TffSqlSimpleExpression(ArgList[Index]);
end;
{--------}
function TffSqlCoalesceExpression.GetArgCount: Integer;
begin
  Result := ArgList.Count;
end;
{--------}
function TffSqlCoalesceExpression.GetValue: Variant;
var
  i : Integer;
begin
  Result := Null;
  for i := 0 to pred(ArgCount) do begin
    Result := Arg[i].GetValue;
    if Result <> Null then
      exit;
  end;
end;
{--------}
function TffSqlCoalesceExpression.DependsOn(
  Table: TFFSqlTableProxy): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(ArgCount) do
    if Arg[i].DependsOn(Table) then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{--------}
function TffSqlCoalesceExpression.GetType: TffFieldType;
begin
  Result := Arg[0].GetType;
end;
{--------}
function TffSqlCoalesceExpression.Reduce: Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(ArgCount) do
    if Arg[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;
{====================================================================}

function TffSqlCoalesceExpression.GetSize: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to pred(ArgCount) do
    Result := FFMaxI(Result, Arg[i].GetSize);
end;

{ TFFSqlTableProxySubset }

procedure TFFSqlTableProxySubset.Assign(
  const Source: TFFSqlTableProxySubset);
begin
  FTable := Source.Table;
  KeyRelation := Source.KeyRelation;
  Outer := Source.Outer;
  Opposite := Source.Opposite;
end;

constructor TFFSqlTableProxySubset.Create;
begin
  FTable := Table;
end;

procedure TFFSqlTableProxySubset.Iterate(Iterator: TFFSqlTableIterator;
  Cookie: TffWord32);
begin
  FTable.Iterate(Iterator, Cookie);
end;

function TFFSqlTableProxySubset.UniqueValue: Boolean;
begin
  Result :=
   (KeyRelation.RelationFieldCount = KeyRelation.RelationKeyFieldCount)
   and (KeyRelation.RelationOperators[KeyRelation.RelationKeyFieldCount - 1] = roEQ);
end;

function TFFSqlTableProxySubset.ClosedSegment: Boolean;
begin
  Result := KeyRelation.RelationOperatorB[KeyRelation.RelationKeyFieldCount - 1] <> roNone; {!!.11}
end;

function TFFSqlTableProxySubset.KeyDepth: Integer;
begin
  Result := KeyRelation.RelationFieldCount;
end;

function TFFSqlTableProxySubset.EqualKeyDepth: Integer;
begin
  Result := 0;
  while (Result < KeyRelation.RelationFieldCount)
    and (KeyRelation.RelationOperators[Result] = roEQ) do
      inc(Result);
end;

{ TFFSqlTableProxySubsetList }

function TFFSqlTableProxySubsetList.Add(
  TableProxySubset: TFFSqlTableProxySubset): TFFSqlTableProxySubset;
begin
  FList.Add(TableProxySubset);
  Result := TableProxySubset;
end;

{!!.10 new}
function TFFSqlTableProxySubsetList.Insert(
  TableProxySubset: TFFSqlTableProxySubset): TFFSqlTableProxySubset;
begin
  FList.Insert(0, TableProxySubset);
  Result := TableProxySubset;
end;

procedure TFFSqlTableProxySubsetList.Assign(
  const Source: TFFSqlTableProxySubsetList);
var
  i : Integer;
begin
  Clear;
  for i := 0 to pred(Source.Count) do
    Add(TFFSqlTableProxySubset.Create(Source.Item[i].Table)).Assign(Source.Item[i]);
  OuterJoin := Source.OuterJoin;
end;

constructor TFFSqlTableProxySubsetList.Create;
begin
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FList := TList.Create;
end;

procedure TFFSqlTableProxySubsetList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TFFSqlTableProxySubsetList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(FList.Count) do
    Item[i].Free;
  FList.Clear;
end;

destructor TFFSqlTableProxySubsetList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TFFSqlTableProxySubsetList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFFSqlTableProxySubsetList.GetItem(
  Index: Integer): TFFSqlTableProxySubset;
begin
  Result := TFFSqlTableProxySubset(FList[Index]);
end;

function TFFSqlTableProxySubsetList.RelationUsed(
  Relation: TffSqlCondFactor): Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(Count) do
    if Item[i].KeyRelation.CondF = Relation then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

function TFFSqlTableProxySubsetList.DependencyExists(
  Table : TFFSqlTableProxy): Boolean;
var
  i, j : Integer;
begin
  for i := 0 to pred(Count) do
    for j := 0 to Item[i].KeyRelation.RelationFieldCount - 1 do begin
      if Item[i].KeyRelation.ArgExpressions[j].DependsOn(Table) then begin
        Result := True;
        exit;
      end;
      if (Item[i].KeyRelation.ArgExpressionB[j] <> nil)                {!!.11}
      and Item[i].KeyRelation.ArgExpressionB[j].DependsOn(Table) then begin {!!.11}
        Result := True;
        exit;
      end;
    end;
  Result := False;
end;

function TFFSqlTableProxySubsetList.ProcessLevel(Cookie1: TffWord32): Boolean;
begin
  inc(FRecordsRead);
  inc(Owner.RecordsRead);
  { Time to check for timeout? }
  if FRecordsRead mod 1000 = 0 then
    FFCheckRemainingTime;
  Result := True; {continue}
  if Level = 0 then begin
    if FCondTerm.AsBoolean then
      if not SkipInner then
        FCreateResultRecord;
    if SkipInner then
      {SkipInner means we're writing NULL records for outer join
       records with no match, so we just need to know if there
       were any here; we don't need to see the rest, so stop reading:}
      Result := False;
    WroteRow := True;
  end else begin
    if FCondTerm.AsBooleanLevel(Level) then begin
      dec(Level);
      ReadSources;
      inc(Level);
    end;
  end;
end;

procedure TFFSqlTableProxySubsetList.ReadSources;
var
  {V : array[0..pred(ffcl_MaxIndexFlds)] of Variant;
  VB : array[0..pred(ffcl_MaxIndexFlds)] of Variant;} {!!.11}
  i : Integer;
  NullLimit,
  BUsed : Boolean;
  KeyHasIntervals: Boolean;                                            {!!.11}
begin
  with Item[Level] do begin
    NullLimit := False;
    if KeyRelation.CondF <> nil then begin
      Table.SetIndex(KeyRelation.NativeKeyIndex - 1);
      for i := 0 to KeyRelation.RelationFieldCount - 1 do begin
        Assert(KeyRelation.ArgExpressions[i] is TffSqlSimpleExpression);
        V[i] := TffSqlSimpleExpression(KeyRelation.ArgExpressions[i]).GetValue;
        if VarIsNull(V[i]) then
          NullLimit := True;
        VB[i] := V[i];
      end;

      {!!.11 begin}
      KeyHasIntervals := False;
      for i := 0 to KeyRelation.RelationFieldCount - 2 do
        if KeyRelation.RelationOperators[i] <> roEQ then begin
          KeyHasIntervals := True;
          break;
        end;
      {!!.11 end}
      {!!.13}
      {can't preevaluate open intervals on key alone because of possible null values}
      for i := 0 to KeyRelation.RelationFieldCount - 1 do
        case KeyRelation.RelationOperators[i] of
        roL, roG : begin
          KeyHasIntervals := True;
          break;
        end;
      end;
      {!!.13}

      if not KeyHasIntervals and                                       {!!.11}
      not KeyRelation.RelationKeyIsCaseInsensitive then
        KeyRelation.CondF.MarkTrue;

      for i := 0 to KeyRelation.RelationFieldCount - 1 do              {!!.11}
        if KeyRelation.RelationOperatorB[i] <> roNone then begin       {!!.11}
          Assert(KeyRelation.ArgExpressionB[i] is TffSqlSimpleExpression); {!!.11}
          VB[i{KeyRelation.RelationFieldCount - 1}] :=                 {!!.11}
            TffSqlSimpleExpression(KeyRelation.ArgExpressionB[i]).GetValue; {!!.11}
          if VarIsNull(VB[i{KeyRelation.RelationFieldCount - 1}]) then {!!.11}
            NullLimit := True;
        end;
      BUsed := False;
      if not NullLimit then
        case KeyRelation.RelationOperators[KeyRelation.RelationFieldCount - 1] of
        roEQ :
          Table.SetRange(V, VB, KeyRelation.RelationFieldCount,        {!!.11}
                         KeyRelation.RelationFieldCount, True, True,
                         KeyRelation.RelationKeyIndexAsc);
        roLE :
          case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] of {!!.11}
          roG :
            begin
              Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, False, True,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          roGE :
            begin
              Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, True, True,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          else
            Table.SetRange(V, V, KeyRelation.RelationFieldCount - 1,
                           KeyRelation.RelationFieldCount, True, True,
                           KeyRelation.RelationKeyIndexAsc);
          end;
        roL :
          case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] of {!!.11}
          roG :
            begin
              Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, False, False,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          roGE :
            begin
              Table.SetRange(VB, V, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, True, False,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          else
            Table.SetRange(V, V, KeyRelation.RelationFieldCount - 1,
                           KeyRelation.RelationFieldCount, True, False,
                           KeyRelation.RelationKeyIndexAsc);
          end;
        roG :
          case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] of {!!.11}
          roLE :
            begin
              Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, False, True,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          roL :
            begin
              Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, False, False,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          else
            Table.SetRange(V, V, KeyRelation.RelationFieldCount,
                           KeyRelation.RelationFieldCount - 1, False, True,
                           KeyRelation.RelationKeyIndexAsc);
          end;
        roGE :
          case KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] of {!!.11}
          roLE :
            begin
              Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, True, True,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          roL :
            begin
              Table.SetRange(V, VB, KeyRelation.RelationFieldCount,
                             KeyRelation.RelationFieldCount, True, False,
                             KeyRelation.RelationKeyIndexAsc);
              BUsed := True;
            end;
          else
            Table.SetRange(V, V, KeyRelation.RelationFieldCount,
                           KeyRelation.RelationFieldCount - 1, True, True,
                           KeyRelation.RelationKeyIndexAsc);
          end;
        else
          Assert(False);
        end;
      if not KeyHasIntervals and                                       {!!.11}
      not KeyRelation.RelationKeyIsCaseInsensitive and BUsed then
        KeyRelation.RelationB[KeyRelation.RelationFieldCount - 1].MarkTrue; {!!.11}
    end else
      Table.SetIndex(-1);
    {if not NullLimit then begin}                                      {!!.11}
      WroteRow := False;
    if not NullLimit then                                              {!!.11}
      Iterate(ProcessLevel, 0);
    if OuterJoin and not WroteRow and (Level = 0) then begin
      Item[0].Table.NullRecord;
      FCreateResultRecord;
    end;
    {end;}                                                             {!!.11}
    if KeyRelation.CondF <> nil then begin
      KeyRelation.CondF.MarkUnknown;
      if KeyRelation.RelationOperatorB[KeyRelation.RelationFieldCount - 1] <> roNone then {!!.11}
        KeyRelation.RelationB[KeyRelation.RelationFieldCount - 1].MarkUnknown; {!!.11}
    end;
  end;
end;

procedure TFFSqlTableProxySubsetList.Join;
begin
  FCondTerm := CondTerm;
  CondTerm.SetLevelDep(Self);
  FCreateResultRecord := CreateResultRecord;
  Level := Count - 1;
  ReadSources;
end;

{ TffSqlINSERT }

procedure TffSqlINSERT.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlINSERT then begin
    Clear;
    DefaultValues := TffSqlINSERT(Source).DefaultValues;
    TableName := TffSqlINSERT(Source).TableName;
    if TffSqlINSERT(Source).InsertColumnList <> nil then begin
      InsertColumnList := TffSqlInsertColumnList.Create(Self);
      InsertColumnList.Assign(TffSqlINSERT(Source).InsertColumnList);
    end;

    if TffSqlINSERT(Source).TableExp <> nil then begin
      TableExp := TffSqlTableExp.Create(Self);
      TableExp.Assign(TffSqlINSERT(Source).TableExp);
    end;

  end else
    AssignError(Source);
end;

procedure TffSqlINSERT.AddColumns(Node: TffSqlNode);
begin
  Node.AddColumnDef(Self);
end;
{--------}
procedure TffSqlINSERT.Bind;
var
  i: Integer;
  F: TFFSqlFieldProxy;
begin
  if InsertColumnList <> nil then
    InsertColumnList.EnumNodes(ClearBindings, False);
  T := Owner.FDatabase.TableByName(Self, TableName, False, '');        {!!.11}
  if T = nil then
    SQLError('Unable to open table: ' + TableName +
             '. Ensure the table exists and is not in use by ' +
             'another process.');

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  if InsertColumnList <> nil then
    InsertColumnList.EnumNodes(AddColumns, False);
  if Columns.Count = 0 then begin
    for i := 0 to T.FieldCount - 1 do begin
      F := T.Field(i);
      if not F.CanUpdate then
        SQLError('Changing fields of this type is not currently supported ' +
                 'through SQL:' + Columns[i]);
      Columns.AddObject(T.Field(i).Name, F);
    end;
  end else begin
    for i := 0 to Columns.Count - 1 do begin
      F := T.FieldByName(Columns[i]);
      if F = nil then
        SQLError('Unknown field for table ' + TableName + 'in INSERT statement:' +
          Columns[i]);

      if not F.CanUpdate then
        SQLError('Changing fields of this type is not currently supported through SQL:' +
          Columns[i]);

      Columns.Objects[i] := F;
    end;
  end;
  Bound := True;
end;
{--------}
procedure TffSqlINSERT.Clear;
begin
  TableName := '';
  InsertColumnList.Free;
  InsertColumnList := nil;
  TableExp.Free;
  TableExp := nil;
end;
{--------}
procedure TffSqlINSERT.ClearBindings(Node: TffSqlNode);
begin
  Node.ClearBinding;
end;
{--------}
destructor TffSqlINSERT.Destroy;
begin
  Clear;
  if T <> nil then
    if T.Owner = Self then begin
    T.Owner := nil;
    T.Free;
  end;
  inherited;
end;
{--------}
procedure TffSqlINSERT.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, 'INSERT INTO ');
  WriteStr(Stream, TableName);
  WriteStr(Stream,' ');
  if DefaultValues then
    WriteStr(Stream,'DEFAULT VALUES ')
  else begin
    if assigned(InsertColumnList) then begin
      WriteStr(Stream,'(');
      InsertColumnList.EmitSQL(Stream);
      WriteStr(Stream,') ');
    end;
    if assigned(TableExp) then
      TableExp.EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlINSERT.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(InsertColumnList) then
    InsertColumnList.EnumNodes(EnumMethod,Deep);
  if assigned(TableExp) then
    TableExp.EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlINSERT.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlINSERT)
    and (DefaultValues = TffSqlINSERT(Other).DefaultValues)
    and (TableName = TffSqlINSERT(Other).TableName)
    and (BothNil(InsertColumnList, TffSqlINSERT(Other).InsertColumnList)
       or (BothNonNil(InsertColumnList, TffSqlINSERT(Other).InsertColumnList)
         and InsertColumnList.Equals(TffSqlINSERT(Other).InsertColumnList))
       )
    and (BothNil(TableExp, TffSqlINSERT(Other).TableExp)
       or (BothNonNil(TableExp, TffSqlINSERT(Other).TableExp)
         and TableExp.Equals(TffSqlINSERT(Other).TableExp))
       );
end;
{Begin !!.13}
{--------}
function CanInsert(const SrcType, TgtType : TffFieldType) : Boolean;
begin
  { According to our past rules, which are very lax, most every type is
    compatible with all other types. New rules:
     - BLOBs may not be inserted into non-BLOB fields
     - strings may be inserted into BLOBs
     - strings cannot be inserted into numerics or date time }
  if SrcType <> TgtType then
    case TgtType of
      { Numerics & datetime values may be inserted into numerics. }
      fftByte..fftCurrency :
        case SrcType of
          fftByte..fftCurrency, fftStDate..fftDateTime :
            Result := True;
        else
          Result := False;
        end;
      fftStDate..fftDateTime :
        { Numerics, datetime, and string values may be inserted into datetime
          columns. If a date is to be inserted via a string, the string must
          be preceded via the DATE keyword. }
        case SrcType of
          fftByte..fftCurrency,
          fftStDate..fftDateTime :
            Result := True;
        else
          Result := False;
        end;  { case }
      fftChar,
      fftWideChar,
      fftShortString..fftWideString :
        { Everything except BLOBs may be inserted into a string. }
        case SrcType of
          fftBLOB..fftBLOBTypedBIN :
            Result := False;
        else
          Result := True;
        end;  { case }
      fftBLOB..fftBLOBTypedBIN :
        { Strings & other BLOBs may be inserted into BLOBs. }
        case SrcType of
          fftChar, fftWideChar,
          fftShortString..fftWideString,
          fftBLOB..fftBLOBTypedBIN :
            Result := True;
        else
          Result := False;
        end;  { case }
    else
      Result := False;
    end  { case }
  else
    Result := True;
end;
{End !!.13}
{--------}
function TffSqlINSERT.Execute(var RowsAffected: Integer) : TffResult;
{Revised !!.13}
var
  i : Integer;
  ST : TffSQLTableProxy;
begin
  Result := Owner.FDatabase.StartTransaction([T]);
  if Result = DBIERR_NONE then
    try
      RowsAffected := 0;
      if not Bound then
        Bind;
      { Make sure the target table can be modified. }
      Result := T.EnsureWritable;
      if Result <> DBIERR_NONE then begin
        Owner.FDatabase.AbortTransaction;
        Exit;
      end;

      { If inserting default values only then do so. }
      if DefaultValues then begin
        T.Insert;
        T.SetDefaults;
        Result := T.Post;
        if Result = DBIERR_NONE then begin
          Owner.FDatabase.Commit;
          RowsAffected := 1;
        end
        else
          Owner.FDatabase.AbortTransaction;
      end
      else if TableExp <> nil then begin
        { Values are coming from a valuelist or subquery. }
        ST := TableExp.ResultTable;
        { Validate the number of source and target columns. }
        if ST.FieldCount <> Columns.Count then
          SQLError('The number of columns in the source clause must match ' +
                   'the number of columns in the INSERT statement.');

        { Do the field types match? }
        for i := 0 to Pred(ST.FieldCount) do
          if not CanInsert(ST.Field(i).GetType,
                           TffSqlFieldProxy(Columns.Objects[i]).GetType) then
            SQLError(Format('The type for source column %d (column name ' +
                            '"%s") is incompatible with the type for ' +
                            'target column %d (column name "%s")',
                            [i, ST.Field(i).Name, i, Columns[i]]));

        { Roll through the source table, inserting its rows into the result
          table. }
        ST.First;
        while not ST.EOF do begin
          T.Insert;
          T.SetDefaults;
          for i := 0 to FFMinI(Pred(ST.FieldCount), Pred(Columns.Count)) do
            TFFSqlFieldProxy(Columns.Objects[i]).SetValue(ST.Field(i).GetValue);
          Result := T.PostNoDefaults;
          if Result = DBIERR_NONE then
            inc(RowsAffected)
          else
            break;
          ST.Next;
        end;
        if Result = DBIERR_NONE Then
          Owner.FDatabase.Commit
        else begin
          Owner.FDatabase.AbortTransaction;
          RowsAffected := 0;
        end;
      end else
        Assert(False, 'Unexpected INSERT scenario');
    except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      raise;
    end
  else if Result = DBIERR_LOCKED then
    FFRaiseException(EffException, ffStrResServer, fferrLockRejected,
                     [ffcLockExclusive, '', T.Name])
  else
    FFRaiseException(EffException, ffStrResServer, Result, [T.Name]);
end;
{--------}
{!!.11 new}
function TffSqlINSERT.Reduce: Boolean;
begin
  if TableExp <> nil then
    if TableExp.Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

{ TffSqlInsertItem }

procedure TffSqlInsertItem.AddColumnDef(Target: TffSqlColumnListOwner);
begin
  Target.Columns.Add(ColumnName);
end;

procedure TffSqlInsertItem.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlInsertItem then begin
    ColumnName := TffSqlInsertItem(Source).ColumnName;
  end else
    AssignError(Source);
end;

procedure TffSqlInsertItem.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ColumnName);
end;

procedure TffSqlInsertItem.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
end;

function TffSqlInsertItem.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlInsertItem)
    and (ColumnName = TffSqlInsertItem(Other).ColumnName);
end;

{ TffSqlInsertColumnList }

function TffSqlInsertColumnList.AddItem(
  NewInsertColumn: TffSqlInsertItem): TffSqlInsertItem;
begin
  FInsertColumnItemList.Add(NewInsertColumn);
  Result := NewInsertColumn;
end;

procedure TffSqlInsertColumnList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlInsertColumnList then begin
    Clear;
    for i := 0 to pred(TffSqlInsertColumnList(Source).InsertColumnCount) do
      AddItem(TffSqlInsertItem.Create(Self)).Assign(
        TffSqlInsertColumnList(Source).InsertColumnItem[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlInsertColumnList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(InsertColumnCount) do
    InsertColumnItem[i].Free;
  FInsertColumnItemList.Clear;
end;

constructor TffSqlInsertColumnList.Create(AParent: TffSqlNode);
begin
  inherited;
  FInsertColumnItemList := TList.Create;
end;

destructor TffSqlInsertColumnList.Destroy;
begin
  Clear;
  FInsertColumnItemList.Free;
  inherited;
end;

procedure TffSqlInsertColumnList.EmitSQL(Stream: TStream);
var
  i : Integer;
  First: Boolean;
begin
  First := True;
  for i := 0 to pred(InsertColumnCount) do begin
    if First then
      First := False
    else
      WriteStr(Stream, ', ');
    InsertColumnItem[i].EmitSQL(Stream);
  end;
end;

procedure TffSqlInsertColumnList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(InsertColumnCount) do
    InsertColumnItem[i].EnumNodes(EnumMethod, Deep);
end;

function TffSqlInsertColumnList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlInsertColumnList then begin
    if InsertColumnCount <> TffSqlInsertColumnList(Other).InsertColumnCount then
      exit;
    for i := 0 to pred(InsertColumnCount) do
      if not InsertColumnItem[i].Equals(TffSqlInsertColumnList(Other).InsertColumnItem[i]) then
        exit;
    Result := True;
  end;
end;

function TffSqlInsertColumnList.GetInsertColumnCount: Integer;
begin
  Result := FInsertColumnItemList.Count;
end;

function TffSqlInsertColumnList.GetInsertColumnItem(
  Index: Integer): TffSqlInsertItem;
begin
  Result := TffSqlInsertItem(FInsertColumnItemList[Index]);
end;

procedure TffSqlInsertColumnList.SetInsertColumnItem(Index: Integer;
  const Value: TffSqlInsertItem);
begin
  FInsertColumnItemList[Index] := Value;
end;

{ TffSqlValueItem }

procedure TffSqlValueItem.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlValueItem then begin
    Simplex.Free;
    {Simplex := nil;} {unnecessary}
    Default := TffSqlUpdateItem(Source).Default;
    Simplex := TffSqlSimpleExpression.Create(Self);
    Simplex.Assign(TffSqlValueItem(Source).Simplex);
  end else
    AssignError(Source);
end;

destructor TffSqlValueItem.Destroy;
begin
  Simplex.Free;
  inherited;
end;

procedure TffSqlValueItem.EmitSQL(Stream: TStream);
begin
  if Default then
    WriteStr(Stream, 'DEFAULT ')
  else if Simplex = nil then
    WriteStr(Stream, 'NULL ')
  else
    Simplex.EmitSQL(Stream);
end;

procedure TffSqlValueItem.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(Simplex) then
    Simplex.EnumNodes(EnumMethod, Deep);
end;

function TffSqlValueItem.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlValueItem)
    and (Default = TffSqlValueItem(Other).Default)
    and (BothNil(Simplex, TffSqlValueItem(Other).Simplex)
       or (BothNonNil(Simplex, TffSqlValueItem(Other).Simplex)
         and Simplex.Equals(TffSqlValueItem(Other).Simplex)));
end;

function TffSqlValueItem.GetDecimals: Integer;
begin
  if assigned(Simplex) then
    Result := Simplex.GetDecimals
  else
    Result := 0;
end;

function TffSqlValueItem.GetSize: Integer;
begin
  if assigned(Simplex) then
    Result := Simplex.GetSize
  else
    Result := 1;
end;

function TffSqlValueItem.GetType: TffFieldType;
begin
  if assigned(Simplex) then
    Result := Simplex.GetType
  else
    Result := fftBoolean;
end;

{ TffSqlValueList }

function TffSqlValueList.AddItem(
  NewValue: TffSqlValueItem): TffSqlValueItem;
begin
  FValueItemList.Add(NewValue);
  Result := NewValue;
end;

procedure TffSqlValueList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlValueList then begin
    Clear;
    for i := 0 to pred(TffSqlValueList(Source).ValueCount) do
      AddItem(TffSqlValueItem.Create(Self)).Assign(
        TffSqlValueList(Source).ValueItem[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlValueList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(ValueCount) do
    ValueItem[i].Free;
  FValueItemList.Clear;
end;

constructor TffSqlValueList.Create(AParent: TffSqlNode);
begin
  inherited;
  FValueItemList := TList.Create;
end;

destructor TffSqlValueList.Destroy;
begin
  Clear;
  FValueItemList.Free;
  if FResultTable <> nil then begin
    if FResultTable.Owner = Self then begin
      FResultTable.Owner := nil;
      FResultTable.Free;
    end;
  end;
  inherited;
end;

procedure TffSqlValueList.EmitSQL(Stream: TStream);
var
  i : Integer;
  First: Boolean;
begin
  First := True;
  for i := 0 to pred(ValueCount) do begin
    if First then
      First := False
    else
      WriteStr(Stream, ', ');
    ValueItem[i].EmitSQL(Stream);
  end;
end;

procedure TffSqlValueList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i: Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(ValueCount) do
    ValueItem[i].EnumNodes(EnumMethod, Deep);
end;

function TffSqlValueList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlValueList then begin
    if ValueCount <> TffSqlValueList(Other).ValueCount then
      exit;
    for i := 0 to pred(ValueCount) do
      if not ValueItem[i].Equals(TffSqlValueList(Other).ValueItem[i]) then
        exit;
    Result := True;
  end;
end;

procedure TffSqlValueList.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
begin
  raise Exception.Create('Not yet implemented');
end;

function TffSqlValueList.GetResultTable: TFFSqlTableProxy;
var
  FieldDefList : TffSqlFieldDefList;
  i: Integer;
  FldName : string;                                                    {!!.11}
  Field : TffSqlFieldProxy;                                            {!!.11}
begin
{Begin !!.13}
  if FResultTable <> nil then
    for i := 0 to pred(ValueCount) do
      if (ValueItem[i].Simplex <> nil) and
         not ValueItem[i].Simplex.IsConstant then begin
        FResultTable.Owner := nil;
        FResultTable.Free;
        FResultTable := nil;
        break;
      end;  { if }
{End !!.13}
  if FResultTable = nil then begin
    FieldDefList := TffSqlFieldDefList.Create;
    try
{Begin !!.11}
      for i := 0 to pred(ValueCount) do begin
        FldName := 'Value_'+IntToStr(i+1);
        Field := OwnerStmt.T.Field(i);
        if ValueItem[i].Default then
          FieldDefList.AddField(FldName, Field.GetType, Field.GetSize,
                                Field.GetDecimals)
        else
          FieldDefList.AddField(FldName, ValueItem[i].GetType,
                                ValueItem[i].GetSize, ValueItem[i].GetDecimals);
      end;  { for }
{End !!.11}
      FResultTable := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList); {!!.10}
    finally
      FieldDefList.Free;
    end;
    Owner.FDatabase.StartTransaction([nil]);
    try
      FResultTable.Insert;
      for i := 0 to pred(ValueCount) do
        if ValueItem[i].Simplex <> nil then
          FResultTable.Field(i).SetValue(ValueItem[i].Simplex.GetValue)
{Begin !!.11}
        else if ValueItem[i].Default then
          FResultTable.Field(i).SetDefault
{End !!.11}
        else
          FResultTable.Field(i).SetFieldToNull;
      FResultTable.Post;
    except
      Owner.FDatabase.AbortTransaction;
      FResultTable.Owner := nil;
      FResultTable.Free;
      FResultTable := nil;
      raise;
    end;
    Owner.FDatabase.Commit;
  end;
  Result := FResultTable;
end;

function TffSqlValueList.GetValueCount: Integer;
begin
  Result := FValueItemList.Count;
end;

function TffSqlValueList.GetValueItem(Index: Integer): TffSqlValueItem;
begin
  Result := TffSqlValueItem(FValueItemList[Index]);
end;

function TffSqlValueList.Reduce: Boolean;
begin
  Result := False;
end;

procedure TffSqlValueList.SetValueItem(Index: Integer;
  const Value: TffSqlValueItem);
begin
  FValueItemList[Index] := Value;
end;

{ TffSqlDELETE }

procedure TffSqlDELETE.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlDELETE then begin
    Clear;

    if TffSqlDELETE(Source).TableRef <> nil then begin
      TableRef := TffSqlTableRef.Create(Self);
      TableRef.Assign(TffSqlDELETE(Source).TableRef);
    end;

    if TffSqlDELETE(Source).CondExpWhere <> nil then begin
      CondExpWhere := TffSqlCondExp.Create(Self);
      CondExpWhere.Assign(TffSqlDELETE(Source).CondExpWhere);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlDELETE.Bind;
begin
  Assert(TableRef <> nil);
  T := TableRef.GetTable(Self, False);                                 {!!.11}
  if T = nil then
    SQLError('Unable to open table: ' + TableRef.SQLName + //TableName +
             '. Ensure the table exists and is not in use by ' +
             'another process.');

  if CondExpWhere <> nil then
    CondExpWhere.MatchType(fftBoolean);
  Bound := True;
end;

function TffSqlDELETE.BindField(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  Result := nil;
  Assert(T <> nil);
  Assert(T is TffSqlTableProxy);
  if T.FieldByName(FieldName) <> nil then begin
    Result := T.FieldByName(FieldName);
    Exit;
  end;
  SQLError('Unknown field:' + FieldName);
end;

procedure TffSqlDELETE.Clear;
begin
  TableRef.Free;
  TableRef := nil;
  CondExpWhere.Free;
  CondExpWhere := nil;
end;

procedure TffSqlDELETE.DeleteRecord;
var
  Pos: TffInt64;
begin
  Pos := T.GetCurrentRecordID;
  DeleteList.Add(Pointer(Pos.iLow));
  DeleteList.Add(Pointer(Pos.iHigh));
end;

destructor TffSqlDELETE.Destroy;
begin
  if T <> nil then
    if T.Owner = Self then begin
    T.Owner := nil;
    T.Free;
  end;
  Clear;
  Joiner.Free;
  inherited;
end;

procedure TffSqlDELETE.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,'DELETE FROM ');
  TableRef.EmitSQL(Stream);
  WriteStr(Stream,' ');
  if assigned(CondExpWhere) then begin
    WriteStr(Stream,'WHERE ');
    CondExpWhere.EmitSQL(Stream);
  end;
end;

procedure TffSqlDELETE.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(TableRef) then
    TableRef.EnumNodes(EnumMethod, Deep);
  if assigned(CondExpWhere) then
    CondExpWhere.EnumNodes(EnumMethod, Deep);
end;

function TffSqlDELETE.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlDELETE)
    and (BothNil(TableRef, TffSqlDELETE(Other).TableRef)
       or (BothNonNil(TableRef, TffSqlDELETE(Other).TableRef)
         and TableRef.Equals(TffSqlDELETE(Other).TableRef)))
    and (BothNil(CondExpWhere, TffSqlDELETE(Other).CondExpWhere)
       or (BothNonNil(CondExpWhere, TffSqlDELETE(Other).CondExpWhere)
         and CondExpWhere.Equals(TffSqlDELETE(Other).CondExpWhere)));
end;

function TffSqlDELETE.Execute(var RowsAffected: Integer) : TffResult;  {!!.11}
var
  i: Integer;
  Pos: TffInt64;
begin
  Result := Owner.FDatabase.StartTransaction([T]);
  if Result = DBIERR_NONE then
    try
      if not Bound then
        Bind;
{Begin !!.11}
      Result := T.EnsureWritable;
      if Result <> DBIERR_NONE then begin
        Owner.FDatabase.AbortTransaction;
        Exit;
      end;
{End !!.11}
      RowsAffected := 0;
      if Joiner = nil then begin
        Joiner := TffSqlJoiner.Create(Owner, CondExpWhere);
        Joiner.Sources.Add(TFFSqlTableProxySubset.Create(T));
      end;

      Joiner.ClearColumnList;

      Joiner.Target := nil;
      DeleteList := TList.Create;
      try
        Joiner.Execute(Owner.UseIndex, DeleteRecord, jmNone);
        T.SetIndex(-1); {switch to raw record id index}                {!!.11}
        i := 0;
        while (Result = DBIERR_NONE) and                               {!!.11}
              (i < DeleteList.Count) do begin                          {!!.11}
          Pos.iLow := TffWord32(DeleteList[i]);
          inc(i);
          Assert(i < DeleteList.Count);
          Pos.iHigh := TffWord32(DeleteList[i]);
          inc(i);
          T.GetRecordByID(Pos, ffsltExclusive);                        {!!.11}
          Result := T.Delete;                                          {!!.11}
          if Result = DBIERR_NONE then                                 {!!.11}
            inc(RowsAffected);                                         {!!.11}
        end;
//        RowsAffected := DeleteList.Count div 2;                      {Deleted !!.11}
      finally
        DeleteList.Free;
      end;
{Begin !!.11}
      if Result = DBIERR_NONE then
        Owner.FDatabase.Commit
      else
        Owner.FDatabase.AbortTransaction;
{End !!.11}
    except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      raise;
    end
  else if Result = DBIERR_LOCKED then
    FFRaiseException(EffException, ffStrResServer, fferrLockRejected,
                     [ffcLockExclusive, '', T.Name])
  else
    FFRaiseException(EffException, ffStrResServer, Result, [T.Name]);
end;
{--------}

{!!.11 new}
function TffSqlDELETE.Reduce: Boolean;
begin
  if TableRef <> nil then
    if TableRef.Reduce then begin
      Result := True;
      exit;
    end;
  if CondExpWhere <> nil then
    if CondExpWhere.Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

{ TffSqlUPDATE }

procedure TffSqlUPDATE.AddColumns(Node: TffSqlNode);
begin
  Node.AddColumnDef(Self);
end;

procedure TffSqlUPDATE.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlUPDATE then begin
    Clear;
    if TffSqlUPDATE(Source).TableRef <> nil then begin
      TableRef := TffSqlTableRef.Create(Self);
      TableRef.Assign(TffSqlUPDATE(Source).TableRef);
    end;
    if TffSqlUPDATE(Source).UpdateList <> nil then begin
      UpdateList := TffSqlUpdateList.Create(Self);
      UpdateList.Assign(TffSqlUPDATE(Source).UpdateList);
    end;
    if TffSqlUPDATE(Source).CondExpWhere <> nil then begin
      CondExpWhere := TffSqlCondExp.Create(Self);
      CondExpWhere.Assign(TffSqlUPDATE(Source).CondExpWhere);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlUPDATE.Bind;
var
  i: Integer;
  F: TFFSqlFieldProxy;
begin
  Assert(UpdateList <> nil);
  UpdateList.EnumNodes(ClearBindings, False);
  T := TableRef.GetTable(Self, False);                                 {!!.11}
  if T = nil then
    SQLError('Unable to open table: ' + TableRef.SQLName + //TableName +
             '. Ensure the table exists and is not in use by ' +
             'another process.');

  {build column list}
  Assert(Assigned(Columns));
  Columns.Clear;
  UpdateList.EnumNodes(AddColumns, False);
  Assert(Columns.Count > 0);
  for i := 0 to Columns.Count - 1 do begin
    F := T.FieldByName(Columns[i]);
    if F = nil then
      SQLError('Unknown field for table ' + TableRef.SQLName + 'in UPDATE statement:' +
        Columns[i]);

    if not F.CanUpdate then
      SQLError('Changing fields of this type is not currently supported through SQL:' +
        Columns[i]);

    TffSqlUpdateItem(Columns.Objects[i]).F := F;
    with TffSqlUpdateItem(Columns.Objects[i]) do
      if Simplex <> nil then
        Simplex.MatchType(F.GetType);

  end;
  if CondExpWhere <> nil then
    CondExpWhere.MatchType(fftBoolean);
  Bound := True;
end;

function TffSqlUPDATE.BindField(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  Result := nil;
  Assert(T <> nil);
  Assert(T is TffSqlTableProxy);
  if T.FieldByName(FieldName) <> nil then begin
    Result := T.FieldByName(FieldName);
    Exit;
  end;
  SQLError('Unknown field:' + FieldName);
end;

procedure TffSqlUPDATE.Clear;
begin
  TableRef.Free;
  TableRef := nil;
  UpdateList.Free;
  UpdateList := nil;
  CondExpWhere.Free;
  CondExpWhere := nil;
end;

procedure TffSqlUPDATE.ClearBindings(Node: TffSqlNode);
begin
  Node.ClearBinding;
end;

destructor TffSqlUPDATE.Destroy;
begin
  if T <> nil then
    if T.Owner = Self then begin
    T.Owner := nil;
    T.Free;
  end;
  Clear;
  Joiner.Free;
  inherited;
end;

procedure TffSqlUPDATE.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, 'UPDATE ');
  TableRef.EmitSQL(Stream);
  WriteStr(Stream,' SET ');
  if assigned(UpdateList) then
    UpdateList.EmitSQL(Stream);
  if assigned(CondExpWhere) then
    CondExpWhere.EmitSQL(Stream);
end;

procedure TffSqlUPDATE.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(TableRef) then
    TableRef.EnumNodes(EnumMethod, Deep);
  if assigned(UpdateList) then
    UpdateList.EnumNodes(EnumMethod, Deep);
  if assigned(CondExpWhere) then
    CondExpWhere.EnumNodes(EnumMethod, Deep);
end;

function TffSqlUPDATE.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlUPDATE)
    and (BothNil(TableRef, TffSqlUPDATE(Other).TableRef)
       or (BothNonNil(TableRef, TffSqlUPDATE(Other).TableRef)
         and UpdateList.Equals(TffSqlUPDATE(Other).UpdateList)))
    and (BothNil(UpdateList, TffSqlUPDATE(Other).UpdateList)
       or (BothNonNil(UpdateList, TffSqlUPDATE(Other).UpdateList)
         and UpdateList.Equals(TffSqlUPDATE(Other).UpdateList)))
    and (BothNil(CondExpWhere, TffSqlUPDATE(Other).CondExpWhere)
       or (BothNonNil(CondExpWhere, TffSqlUPDATE(Other).CondExpWhere)
         and CondExpWhere.Equals(TffSqlUPDATE(Other).CondExpWhere)));
end;

function TffSqlUPDATE.Execute(var RowsAffected: Integer) : TffResult;  {!!.11}
var
  i: Integer;
  Pos: TffInt64;
begin
  Result := Owner.FDatabase.StartTransaction([T]);
  if Result = DBIERR_NONE then
    try
      if not Bound then
        Bind;
{Begin !!.11}
      Result := T.EnsureWritable;
      if Result <> DBIERR_NONE then begin
        Owner.FDatabase.AbortTransaction;
        Exit;
      end;
{End !!.11}
      FRowsAffected := 0;
      if Joiner = nil then begin
        Joiner := TffSqlJoiner.Create(Owner, CondExpWhere);
        Joiner.Sources.Add(
            TFFSqlTableProxySubset.Create(
              TFFSqlTableProxy(T)));
      end;

      Joiner.ClearColumnList;

      Joiner.Target := nil;
      UpdateRecList := TList.Create;
      try
        Joiner.Execute(Owner.UseIndex, UpdateRecord, jmNone);
        T.SetIndex(-1); {switch to raw record id index}                {!!.11}
        i := 0;
        while (Result = DBIERR_NONE) and                               {!!.11}
              (i < UpdateRecList.Count) do begin                       {!!.11}
          Pos.iLow := TffWord32(UpdateRecList[i]);
          inc(i);
          Assert(i < UpdateRecList.Count);
          Pos.iHigh := TffWord32(UpdateRecList[i]);
          inc(i);
          T.GetRecordByID(Pos, ffsltExclusive);                        {!!.11}
          Result := UpdateList.Update;                                 {!!.11}
          if Result = DBIERR_NONE then                                 {!!.11}
            inc(FRowsAffected);
        end;
      finally
        UpdateRecList.Free;
      end;
{Begin !!.11}
      if Result = DBIERR_NONE then begin
        Owner.FDatabase.Commit;
        RowsAffected := FRowsAffected;
      end
      else
        Owner.FDatabase.AbortTransaction;
{End !!.11}
    except
      Owner.FDatabase.AbortTransaction;
      RowsAffected := 0;
      raise;
    end
  else if Result = DBIERR_LOCKED then
    FFRaiseException(EffException, ffStrResServer, fferrLockRejected,
                     [ffcLockExclusive, '', T.Name])
  else
    FFRaiseException(EffException, ffStrResServer, Result, [T.Name]);
end;
{--------}

{!!.11 new}
function TffSqlUPDATE.Reduce: Boolean;
begin
  if TableRef <> nil then
    if TableRef.Reduce then begin
      Result := True;
      exit;
    end;
  if CondExpWhere <> nil then
    if CondExpWhere.Reduce then begin
      Result := True;
      exit;
    end;
  if UpdateList <> nil then
    if UpdateList.Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TffSqlUPDATE.UpdateRecord;
var
  Pos: TffInt64;
begin
  Pos := T.GetCurrentRecordID;
  UpdateRecList.Add(Pointer(Pos.iLow));
  UpdateRecList.Add(Pointer(Pos.iHigh));
end;

{ TffSqlUpdateItem }

procedure TffSqlUpdateItem.AddColumnDef(Target: TffSqlColumnListOwner);
begin
  Target.Columns.AddObject(ColumnName, Self);
end;

procedure TffSqlUpdateItem.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlUpdateItem then begin
    Simplex.Free;
    Simplex := nil;
    ColumnName := TffSqlUpdateItem(Source).ColumnName;
    Default := TffSqlUpdateItem(Source).Default;
    if TffSqlUpdateItem(Source).Simplex <> nil then begin
      Simplex := TffSqlSimpleExpression.Create(Self);
      Simplex.Assign(TffSqlUpdateItem(Source).Simplex);
    end;
  end else
    AssignError(Source);
end;

destructor TffSqlUpdateItem.Destroy;
begin
  Simplex.Free;
  inherited;
end;

procedure TffSqlUpdateItem.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ColumnName);
  WriteStr(Stream,' = ');
  if Default then
    WriteStr(Stream, 'DEFAULT ')
  else
  if Simplex = nil then
    WriteStr(Stream, 'NULL ')
  else
    Simplex.EmitSQL(Stream);
end;

procedure TffSqlUpdateItem.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if Simplex <> nil then
    Simplex.EnumNodes(EnumMethod, Deep);
end;

function TffSqlUpdateItem.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    (Other is TffSqlUpdateItem)
    and (ColumnName = TffSqlUpdateItem(Other).ColumnName)
    and (Default = TffSqlUpdateItem(Other).Default)
    and (BothNil(Simplex, TffSqlUpdateItem(Other).Simplex)
       or (BothNonNil(Simplex, TffSqlUpdateItem(Other).Simplex)
         and Simplex.Equals(TffSqlUpdateItem(Other).Simplex)));
end;

function TffSqlUpdateItem.Reduce: Boolean;
begin
  Result := (Simplex <> nil) and Simplex.Reduce;
end;

procedure TffSqlUpdateItem.Update;
begin
  Assert(F <> nil);
  if Simplex <> nil then
    F.SetValue(Simplex.GetValue)
  else
    F.SetFieldToNull;
end;

{ TffSqlUpdateList }

function TffSqlUpdateList.AddItem(
  NewValue: TffSqlUpdateItem): TffSqlUpdateItem;
begin
  FUpdateItemList.Add(NewValue);
  Result := NewValue;
end;

procedure TffSqlUpdateList.Assign(const Source: TffSqlNode);
var
  i : Integer;
begin
  if Source is TffSqlValueList then begin
    Clear;
    for i := 0 to pred(TffSqlValueList(Source).ValueCount) do
      AddItem(TffSqlUpdateItem.Create(Self)).Assign(
        TffSqlValueList(Source).ValueItem[i]);
  end else
    AssignError(Source);
end;

procedure TffSqlUpdateList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(UpdateCount) do
    UpdateItem[i].Free;
  FUpdateItemList.Clear;
end;

constructor TffSqlUpdateList.Create(AParent: TffSqlNode);
begin
  inherited;
  FUpdateItemList := TList.Create;
end;

destructor TffSqlUpdateList.Destroy;
begin
  Clear;
  FUpdateItemList.Free;
  inherited;
end;

procedure TffSqlUpdateList.EmitSQL(Stream: TStream);
var
  i : Integer;
  First: Boolean;
begin
  First := True;
  for i := 0 to pred(UpdateCount) do begin
    if First then
      First := False
    else
      WriteStr(Stream, ', ');
    UpdateItem[i].EmitSQL(Stream);
  end;
end;

procedure TffSqlUpdateList.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
var
  i: Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(UpdateCount) do
    UpdateItem[i].EnumNodes(EnumMethod, Deep);
end;

function TffSqlUpdateList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlValueList then begin
    if UpdateCount <> TffSqlUpdateList(Other).UpdateCount then
      exit;
    for i := 0 to pred(UpdateCount) do
      if not UpdateItem[i].Equals(TffSqlUpdateList(Other).UpdateItem[i]) then
        exit;
    Result := True;
  end;
end;

function TffSqlUpdateList.GetUpdateCount: Integer;
begin
  Result := FUpdateItemList.Count;
end;

function TffSqlUpdateList.GetUpdateItem(Index: Integer): TffSqlUpdateItem;
begin
  Result := TffSqlUpdateItem(FUpdateItemList[Index]);
end;

{!!.11 new}
function TffSqlUpdateList.Reduce: Boolean;
var
  i: Integer;
begin
  for i := 0 to UpdateCount - 1 do
    if UpdateItem[i].Reduce then begin
      Result := True;
      exit;
    end;
  Result := False;
end;

function TffSqlUpdateList.Update : TffResult;                          {!!.11}
var
  i: Integer;
begin
  for i := 0 to UpdateCount - 1 do
    UpdateItem[i].Update;
  Assert(Parent <> nil);
  Assert(TObject(Parent) is TffSqlUpdate);
  Result := TffSqlUpdate(Parent).T.Update;                             {!!.11}
end;

{ TffSqlColumnListOwner }

constructor TffSqlColumnListOwner.Create(AParent: TffSqlNode);
begin
  inherited;
  Columns := TStringList.Create;
end;

destructor TffSqlColumnListOwner.Destroy;
begin
  Columns.Free;
  inherited;
end;

{ TffSqlNonJoinTablePrimary }

procedure TffSqlNonJoinTablePrimary.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlNonJoinTablePrimary then begin
    Clear;
    if TffSqlNonJoinTablePrimary(Source).SelectSt <> nil then begin
      SelectSt := TFFSqlSELECT.Create(Self);
      SelectSt.Assign(TffSqlNonJoinTablePrimary(Source).SelectSt);
    end;
    if TffSqlNonJoinTablePrimary(Source).ValueList <> nil then begin
      ValueList := TffSqlValueList.Create(Self);
      ValueList.Assign(TffSqlNonJoinTablePrimary(Source).ValueList);
    end;
    if TffSqlNonJoinTablePrimary(Source).NonJoinTableExp <> nil then begin
      NonJoinTableExp := TffSqlNonJoinTableExp.Create(Self);
      NonJoinTableExp.Assign(TffSqlNonJoinTablePrimary(Source).NonJoinTableExp);
    end;
    if TffSqlNonJoinTablePrimary(Source).TableRef <> nil then begin
      TableRef := TffSqlTableRef.Create(Self);
      TableRef.Assign(TffSqlNonJoinTablePrimary(Source).TableRef);
    end;
  end else
    AssignError(Source);
end;

function TffSqlNonJoinTablePrimary.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  if SelectSt <> nil then
    Result := SelectSt.BindField(TableName, FieldName)
  else
  if NonJoinTableExp <> nil then
    Result := NonJoinTableExp.BindFieldDown(TableName, FieldName)
  else
  if TableRef <> nil then
    Result := TableRef.BindFieldDown(TableName, FieldName)
  else
    Result := nil;
end;

function TffSqlNonJoinTablePrimary.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  if SelectSt <> nil then
    Result := SelectSt.BindTable(AOwner, TableName)
  else
  if NonJoinTableExp <> nil then
    Result := NonJoinTableExp.BindTable(AOwner, TableName)
  else
  if TableRef <> nil then
    Result := TableRef.BindTable(AOwner, TableName)
  else
    Result := nil;
end;

procedure TffSqlNonJoinTablePrimary.Clear;
begin
  SelectSt.Free;
  SelectSt := nil;
  ValueList.Free;
  ValueList := nil;
  NonJoinTableExp.Free;
  NonJoinTableExp := nil;
  TableRef.Free;
  TableRef := nil;
end;

function TffSqlNonJoinTablePrimary.DependsOn(
  Table: TFFSqlTableProxy): Boolean;
begin
  if SelectSt <> nil then
    Result := SelectSt.DependsOn(Table)
  else
  if NonJoinTableExp <> nil then
    Result := NonJoinTableExp.DependsOn(Table)
  else
  if TableRef <> nil then
    Result := TableRef.DependsOn(Table)
  else
    Result := False;
end;

destructor TffSqlNonJoinTablePrimary.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlNonJoinTablePrimary.EmitSQL(Stream: TStream);
begin
  if SelectSt <> nil then
    SelectSt.EmitSQL(Stream);
  if ValueList <> nil then
    ValueList.EmitSQL(Stream);
  if NonJoinTableExp <> nil then begin
    WriteStr(Stream,' (');
    NonJoinTableExp.EmitSQL(Stream);
    WriteStr(Stream,')');
  end;
  if TableRef <> nil then begin
    WriteStr(Stream,' TABLE ');
    TableRef.EmitSQL(Stream);
  end;
end;

procedure TffSqlNonJoinTablePrimary.EnsureResultTable(NeedData: Boolean);
begin
  if SelectSt <> nil then
    SelectSt.EnsureResultTable(NeedData);
  if NonJoinTableExp <> nil then
    NonJoinTableExp.EnsureResultTable(NeedData);
end;

procedure TffSqlNonJoinTablePrimary.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if SelectSt <> nil then
    SelectSt.EnumNodes(EnumMethod, Deep);
  if ValueList <> nil then
    ValueList.EnumNodes(EnumMethod, Deep);
  if NonJoinTableExp <> nil then
    NonJoinTableExp.EnumNodes(EnumMethod, Deep);
  if TableRef <> nil then
    TableRef.EnumNodes(EnumMethod, Deep);
end;

function TffSqlNonJoinTablePrimary.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    Other is TffSqlNonJoinTablePrimary
    and ((BothNil(SelectSt, TffSqlNonJoinTablePrimary(Other).SelectSt)
    or (BothNonNil(SelectSt, TffSqlNonJoinTablePrimary(Other).SelectSt)
      and SelectSt.Equals(TffSqlNonJoinTablePrimary(Other).SelectSt))))
    and ((BothNil(ValueList, TffSqlNonJoinTablePrimary(Other).ValueList)
    or (BothNonNil(ValueList, TffSqlNonJoinTablePrimary(Other).ValueList)
      and ValueList.Equals(TffSqlNonJoinTablePrimary(Other).ValueList))))
    and ((BothNil(NonJoinTableExp, TffSqlNonJoinTablePrimary(Other).NonJoinTableExp)
    or (BothNonNil(NonJoinTableExp, TffSqlNonJoinTablePrimary(Other).NonJoinTableExp)
      and NonJoinTableExp.Equals(TffSqlNonJoinTablePrimary(Other).NonJoinTableExp))))
    and ((BothNil(TableRef, TffSqlNonJoinTablePrimary(Other).TableRef)
    or (BothNonNil(TableRef, TffSqlNonJoinTablePrimary(Other).TableRef)
      and TableRef.Equals(TffSqlNonJoinTablePrimary(Other).TableRef))));
end;

procedure TffSqlNonJoinTablePrimary.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
begin
  if assigned(SelectSt) then
    SelectSt.Execute(aLiveResult, aCursorID, RecordsRead)
  else
  if assigned(ValueList) then
    ValueList.Execute(aLiveResult, aCursorID, RecordsRead)
  else
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead)
  else
  if assigned(TableRef) then
    TableRef.Execute(aLiveResult, aCursorID, RecordsRead)
  else
    Assert(False);
end;

function TffSqlNonJoinTablePrimary.GetResultTable: TffSqlTableProxy;
begin
  Result := nil;
  if assigned(SelectSt) then
    Result := SelectSt.ResultTable
  else
  if assigned(ValueList) then
    Result := ValueList.ResultTable
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.ResultTable
  else
  if assigned(TableRef) then
    Result := TableRef.ResultTable
  else
    Assert(False);
end;

function TffSqlNonJoinTablePrimary.Reduce: Boolean;
begin
  Result := False;
  if assigned(SelectSt) then
    Result := SelectSt.Reduce
  else
  if assigned(ValueList) then
    Result := ValueList.Reduce
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.Reduce
  else
  if assigned(TableRef) then
    Result := False //TableRef.Reduce
  else
    Assert(False);
end;

function TffSqlNonJoinTablePrimary.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
begin
  Result := nil;
  if assigned(SelectSt) then
    Result := SelectSt.TargetFieldFromSourceField(F)
  else
  if assigned(ValueList) then
    Result := nil
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.TargetFieldFromSourceField(F)
  else
  if assigned(TableRef) then
    Result := TableRef.TargetFieldFromSourceField(F)
  else
    Assert(False);
end;

{ TffSqlTableExp }

procedure TffSqlTableExp.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlTableExp then begin
    Clear;
    if TffSqlTableExp(Source).NestedTableExp <> nil then begin
      NestedTableExp := TffSqlTableExp.Create(Self);
      NestedTableExp.Assign(TffSqlTableExp(Source).NestedTableExp);
    end;
    if TffSqlTableExp(Source).JoinTableExp <> nil then begin
      JoinTableExp := TffSqlJoinTableExp.Create(Self);
      JoinTableExp.Assign(TffSqlTableExp(Source).JoinTableExp);
    end;
    if TffSqlTableExp(Source).NonJoinTableExp <> nil then begin
      NonJoinTableExp := TffSqlNonJoinTableExp.Create(Self);
      NonJoinTableExp.Assign(TffSqlTableExp(Source).NonJoinTableExp);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlTableExp.Clear;
begin
  NestedTableExp.Free;
  NestedTableExp := nil;
  JoinTableExp.Free;
  JoinTableExp := nil;
  NonJoinTableExp.Free;
  NonJoinTableExp := nil;
end;

destructor TffSqlTableExp.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlTableExp.EmitSQL(Stream: TStream);
begin
  if assigned(NestedTableExp) then
    NestedTableExp.EmitSQL(Stream);
  if assigned(JoinTableExp) then
    JoinTableExp.EmitSQL(Stream);
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.EmitSQL(Stream);
end;

function TffSqlTableExp.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  if assigned(NestedTableExp) then
    Result := NestedTableExp.BindFieldDown(TableName, FieldName)
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.BindFieldDown(TableName, FieldName)
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.BindFieldDown(TableName, FieldName)
  else
    Result := nil;
end;

function TffSqlTableExp.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  if assigned(NestedTableExp) then
    Result := NestedTableExp.BindTable(AOwner, TableName)
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.BindTable(AOwner, TableName)
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.BindTable(AOwner, TableName)
  else
    Result := nil;
end;

function TffSqlTableExp.CheckNoDups: Boolean;
begin
  EnsureResultTable(True);
  Result := not ResultTable.HasDuplicates(True);                       {!!.13}
end;

function TffSqlTableExp.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if assigned(NestedTableExp) then
    Result := NestedTableExp.DependsOn(Table)
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.DependsOn(Table)
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.DependsOn(Table)
  else
    Result := False;
end;

procedure TffSqlTableExp.EnsureResultTable(NeedData: Boolean);
begin
  if assigned(NestedTableExp) then
    NestedTableExp.EnsureResultTable(NeedData);
  if assigned(JoinTableExp) then
    JoinTableExp.EnsureResultTable(NeedData);
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.EnsureResultTable(NeedData);
end;

procedure TffSqlTableExp.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(NestedTableExp) then
    NestedTableExp.EnumNodes(EnumMethod, Deep);
  if assigned(JoinTableExp) then
    JoinTableExp.EnumNodes(EnumMethod, Deep);
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.EnumNodes(EnumMethod, Deep);
end;

function TffSqlTableExp.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    Other is TffSqlTableExp
    and ((BothNil(NestedTableExp, TffSqlTableExp(Other).NestedTableExp)
    or (BothNonNil(NestedTableExp, TffSqlTableExp(Other).NestedTableExp)
      and NestedTableExp.Equals(TffSqlTableExp(Other).NestedTableExp))))
    and ((BothNil(JoinTableExp, TffSqlTableExp(Other).JoinTableExp)
    or (BothNonNil(JoinTableExp, TffSqlTableExp(Other).JoinTableExp)
      and JoinTableExp.Equals(TffSqlTableExp(Other).JoinTableExp))))
    and ((BothNil(NonJoinTableExp, TffSqlTableExp(Other).NonJoinTableExp)
    or (BothNonNil(NonJoinTableExp, TffSqlTableExp(Other).NonJoinTableExp)
      and NonJoinTableExp.Equals(TffSqlTableExp(Other).NonJoinTableExp))));
end;

procedure TffSqlTableExp.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
begin
  if assigned(NestedTableExp) then
    NestedTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
  if assigned(JoinTableExp) then
    JoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.Execute(aLiveResult, aCursorID, RecordsRead);
end;

{!!.11 new}
function TffSqlTableExp.GetFieldsFromTable(const TableName: string; List: TList):
  TffSqlTableProxy;
{-returns fields from table that are ultimately coming from the table
  specified in the TableName argument. NIL if not found.}
begin
  Result := nil;
  if assigned(NestedTableExp) then
    Result := NestedTableExp.GetFieldsFromTable(TableName, List)
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.GetFieldsFromTable(TableName, List)
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.GetFieldsFromTable(TableName, List)
  else
    Assert(False);
end;

function TffSqlTableExp.GetResultTable: TFFSqlTableProxy;
begin
  Result := nil;
  if assigned(NestedTableExp) then
    Result := NestedTableExp.ResultTable
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.ResultTable
  else
  if assigned(NonJoinTableExp) then
    Result := NonJoinTableExp.ResultTable
  else
    Assert(False);
end;

function TffSqlTableExp.Reduce: Boolean;
begin
  if assigned(NestedTableExp) then
    Result := NestedTableExp.Reduce
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.Reduce
  else
    Result := False;
  if assigned(NonJoinTableExp) then
    Result := Result or NonJoinTableExp.Reduce;
end;

function TffSqlTableExp.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
begin
  Result := nil;
  if assigned(NestedTableExp) then
    Result := NestedTableExp.TargetFieldFromSourceField(F)
  else
  if assigned(JoinTableExp) then
    Result := JoinTableExp.TargetFieldFromSourceField(F)
  else
  if assigned(NonJoinTableExp) then
    NonJoinTableExp.TargetFieldFromSourceField(F)
  else
    Assert(False);
end;

{ TffSqlJoinTableExp }

function TffSqlJoinTableExp.BuildSimpleFieldExpr(AOwner: TffSqlNode;
  const ATableName, AFieldName: string; AField: TffSqlFieldProxy
    ): TffSqlSimpleExpression;
var
  Term: TffSqlTerm;
  Fact: TffSqlFactor;
  FieldRef: TffSqlFieldRef;
begin
  Result := TffSqlSimpleExpression.Create(AOwner);
  Term := TffSqlTerm.Create(Result);
  Fact := TffSqlFactor.Create(Term);
  FieldRef := TffSqlFieldRef.Create(Fact);
  FieldRef.TableName := ATableName;
  FieldRef.FieldName := AFieldName;
  FieldRef.FField := AField;
  Fact.FieldRef := FieldRef;
  Term.AddFactor(Fact);
  Result.AddTerm(Term);
end;

procedure TffSqlJoinTableExp.ClearColumns;
var
  i: Integer;
begin
  if Columns = nil then exit;
  for i := 0 to Columns.Count - 1 do
    if TObject(Columns.Objects[i]) is TffSqlSimpleExpression then
      TObject(Columns.Objects[i]).Free;
  Columns.Clear;
end;

procedure TffSqlJoinTableExp.Bind;
var
  i, j : Integer;
  FL, FR: TffSqlFieldProxy;
  lCondTerm: TffSqlCondTerm;
  lCondFact: TffSqlCondFactor;
  lCondPrim: TffSqlCondPrimary;
  lSimp1, lSimp2, cSimp, cSimp1, cSimp2: TffSqlSimpleExpression;
  cTerm: TffSqlTerm;
  cFact: TffSqlFactor;
  cScalar : TffSqlScalarFunc;
  cCoalesce : TffSqlCoalesceExpression;
  S: string;                                                           {!!.11}
  OS: TffSqlSELECT;
  CF, NewCF: TffSqlCondFactor;
  CP: TffSqlCondPrimary;
const
  UorN: array[Boolean] of string = ('UNION', 'NATURAL');
begin
  if JoinType = jtUnion then
    SQLError('UNION JOIN is not currently supported by FlashFiler SQL');
  if Natural and (JoinType = jtUnion) then
    SQLError('NATURAL and UNION cannot both be specified on a JOIN');
  if Natural or (JoinType = jtUnion) then begin
    if CondExp <> nil then
      SQLError(UorN[Natural] + ' joins do not accept an ON clause');
    if UsingList <> nil then
      sQLError(UorN[Natural] + ' joins do not accept a USING clause');
  end;
  if not Natural and not (JoinType in [jtCross,jtUnion]) then begin
    if (CondExp = nil) and (UsingList = nil) then
      SQLError('The join must have either an ON or a USING clause');
  end;
  if CondExp <> nil then
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

  if Natural then begin
    UsingCondExp := TffSqlCondExp.Create(Self);
    lCondTerm := TffSqlCondTerm.Create(UsingCondExp);
    for i := 0 to TL.FieldCount - 1 do begin
      FL := TL.Field(i);
      FR := TR.FieldByName(FL.Name);
      if FR <> nil then begin
        {common field}
        lCondFact := TffSqlCondFactor.Create(lCondTerm);
        lCondPrim := TffSqlCondPrimary.Create(lCondFact);
        lSimp1 := BuildSimpleFieldExpr(lCondPrim, TableRef1.SQLName,
          FL.Name, FL);
        lSimp2 := BuildSimpleFieldExpr(lCondPrim, TableRef2.SQLName,
          FR.Name, FR);
        case JoinType of
        jtRightOuter :
          Columns.AddObject(FL.Name, FR);
        jtFullOuter :
          begin
            cSimp := TffSqlSimpleExpression.Create(Self);
            cTerm := TffSqlTerm.Create(cSimp);
            cFact := TffSqlFactor.Create(cTerm);
            cScalar := TffSqlScalarFunc.Create(cFact);
            cScalar.SQLFunction := sfCoalesce;
            cCoalesce := TffSqlCoalesceExpression.Create(cScalar);
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
          end;
        else
          Columns.AddObject(FL.Name, FL);
        end;
        lCondPrim.SimpleExp1 := lSimp1;
        lCondPrim.SimpleExp2 := lSimp2;
        lCondPrim.RelOp := roEQ;
        lCondFact.CondPrimary := lCondPrim;
        lCondTerm.AddCondFactor(lCondFact);
      end;
    end;
    if lCondTerm.CondFactorCount = 0 then begin
      lCondTerm.Free;
      UsingCondExp.Free;
      UsingCondExp := nil;
    end else begin
      UsingCondExp.AddCondTerm(lCondTerm);
      UsingCondExp.MatchType(fftBoolean);
    end;
    for i := 0 to TL.FieldCount - 1 do begin
      FL := TL.Field(i);
      if Columns.IndexOf(FL.Name) = -1 then
        Columns.AddObject(FL.Name, FL);
    end;
    for i := 0 to TR.FieldCount - 1 do begin
      FR := TR.Field(i);
      if Columns.IndexOf(FR.Name) = -1 then
        Columns.AddObject(FR.Name, FR);
    end;
  end else
  if UsingList <> nil then begin
    UsingCondExp := TffSqlCondExp.Create(Self);
    lCondTerm := TffSqlCondTerm.Create(UsingCondExp);
    for i := 0 to UsingList.UsingCount - 1 do begin
      lCondFact := TffSqlCondFactor.Create(lCondTerm);
      lCondPrim := TffSqlCondPrimary.Create(lCondFact);
      FL := TL.FieldByName(UsingList.UsingItem[i].ColumnName);
      if FL = nil then
        SQLError(format('Field %s does not exist in table %s.',
          [UsingList.UsingItem[i].ColumnName, TableRef1.SQLName]));
      FR := TR.FieldByName(UsingList.UsingItem[i].ColumnName);
      if FR = nil then
        SQLError(format('Field %s does not exist in table %s.',
          [UsingList.UsingItem[i].ColumnName, TableRef2.SQLName]));
      lSimp1 := BuildSimpleFieldExpr(lCondPrim, TableRef1.SQLName,
        FL.Name, FL);
      lSimp2 := BuildSimpleFieldExpr(lCondPrim, TableRef2.SQLName,
        FR.Name, FR);
      case JoinType of
      jtRightOuter :
        Columns.AddObject(FL.Name, FR);
      jtFullOuter :
        begin
          cSimp := TffSqlSimpleExpression.Create(Self);
          cTerm := TffSqlTerm.Create(cSimp);
          cFact := TffSqlFactor.Create(cTerm);
          cScalar := TffSqlScalarFunc.Create(cFact);
          cScalar.SQLFunction := sfCoalesce;
          cCoalesce := TffSqlCoalesceExpression.Create(cScalar);
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
        end;
      else
        Columns.AddObject(FL.Name, FL);
      end;
      lCondPrim.SimpleExp1 := lSimp1;
      lCondPrim.SimpleExp2 := lSimp2;
      lCondPrim.RelOp := roEQ;
      lCondFact.CondPrimary := lCondPrim;
      lCondTerm.AddCondFactor(lCondFact);
    end;
    UsingCondExp.AddCondTerm(lCondTerm);
    (*
    {!!.11 begin}
    {if this join is enclosed in a SELECT with a WHERE clause,
     and if the WHERE clause consists only of a single conditional term,
     and if any of the conditional factors limit either side of the join,
     then copy those conditional factors into the join condition}
    //writeln(SqlText);
    //writeln(' ',CondExp.SqlText);
    OS := OwnerSelect;
    if (OS <> nil)
    and (OS.CondExpWhere <> nil)
    and (OS.CondExpWhere.CondTermCount = 1) then begin
      for i := 0 to OS.CondExpWhere.CondTerm[0].CondFactorCount - 1 do begin
        CF := OS.CondExpWhere.CondTerm[0].CondFactor[i];
        //writeln('  ',CF.SqlText);
        if not CF.IsConstant
        and not CF.UnaryNot then begin
          CP := CF.CondPrimary;
          if CP.RelOp in [roEQ, roLE, roL, roG, roGE] then begin
            if CP.SimpleExp2.IsConstant or CP.SimpleExp2.IsParameter then begin
              if Cp.SimpleExp1.TermCount = 1 then
                if Cp.SimpleExp1.Term[0].FactorCount = 1 then
                  if Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> nil then
                    if (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                      = TableRef1.TableName)
                    or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                      = TableRef1.Alias) then begin
                      //writeln('    found left constraint:', CP.SqlText);
                      NewCF := TffSqlCondFactor.Create(lCondTerm);
                      NewCF.Assign(CF);
                      lCondTerm.AddCondFactor(NewCF);
                      //writeln('      ',CondExp.SqlText);
                    end
                  else
                  if Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> nil then
                    if (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                      = TableRef2.TableName)
                    or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                      = TableRef2.Alias) then begin
                      //writeln('    found right constraint', CP.SqlText);
                      NewCF := TffSqlCondFactor.Create(lCondTerm);
                      NewCF.Assign(CF);
                      lCondTerm.AddCondFactor(NewCF);
                      //writeln('      ',CondExp.SqlText);
                    end;
            end;
          end;
        end;
      end;

    end;
    {!!.11 end}
    *)
    UsingCondExp.MatchType(fftBoolean);
    for i := 0 to TL.FieldCount - 1 do begin
      FL := TL.Field(i);
      if Columns.IndexOf(FL.Name) = -1 then
        Columns.AddObject(FL.Name, FL);
    end;
    for i := 0 to TR.FieldCount - 1 do begin
      FL := TR.Field(i);
      j := Columns.IndexOf(FL.Name);
      if j = -1 then
        Columns.AddObject(FL.Name, FL)
      else
      if j >= UsingList.UsingCount then
        Columns.AddObject(TR.Name + '.' + FL.Name, FL);
    end;
  end else begin
    for i := 0 to TL.FieldCount - 1 do
      Columns.AddObject(TL.Field(i).Name, TL.Field(i));
    for i := 0 to TR.FieldCount - 1 do
      if Columns.IndexOf(TR.Field(i).Name) = -1 then
        Columns.AddObject(TR.Field(i).Name, TR.Field(i))
      {!!.11 begin}
      else begin
        S := TR.Name + '.' + TR.Field(i).Name;
        if Columns.IndexOf(S) = -1 then
          Columns.AddObject(S, TR.Field(i))
        else begin
          j := 2;
          while Columns.IndexOf(S + '_' + IntToStr(j)) <> -1 do
            inc(j);
          Columns.AddObject(S+ '_' + IntToStr(j), TR.Field(i));
        end;
      end;
      {!!.11 end}
  end;

  if (CondExp <> nil) then begin
    {!!.11 begin}
    if (CondExp.CondTermCount = 1) then begin
      {if this join is enclosed in a SELECT with a WHERE clause,
       and if the WHERE clause consists only of a single conditional term,
       and if any of the conditional factors limit either side of the join,
       then copy those conditional factors into the join condition}
      //writeln(SqlText);
      //writeln(' ',CondExp.SqlText);
      OS := OwnerSelect;
      if (OS <> nil)
      and (OS.CondExpWhere <> nil)
      and (OS.CondExpWhere.CondTermCount = 1) then begin
        for i := 0 to OS.CondExpWhere.CondTerm[0].CondFactorCount - 1 do begin
          CF := OS.CondExpWhere.CondTerm[0].CondFactor[i];
          //writeln('  ',CF.SqlText);
          if not CF.IsConstant
          and not CF.UnaryNot then begin
            CP := CF.CondPrimary;
            if CP.RelOp in [roEQ, roLE, roL, roG, roGE, roNE] then begin
              if CP.SimpleExp2.IsConstant or CP.SimpleExp2.IsParameter then begin
                if Cp.SimpleExp1.TermCount = 1 then
                  if Cp.SimpleExp1.Term[0].FactorCount = 1 then
                    if Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> nil then
                      if (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName <> '') {!!.13}
                      and (                                                         {!!.13}
                      (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                        = TableRef1.TableName)
                      or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                        = TableRef1.Alias)) then begin                              {!!.13}
                        //writeln('    found left constraint:', CP.SqlText);
                        NewCF := TffSqlCondFactor.Create(CondExp.CondTerm[0]);
                        NewCF.Assign(CF);
                        CondExp.CondTerm[0].AddCondFactor(NewCF);
                        //writeln('      ',CondExp.SqlText);
                      end
                    else
                    if Cp.SimpleExp1.Term[0].Factor[0].FieldRef <> nil then
                      if (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName <> '') {!!.13}
                      and (                                                         {!!.13}
                       ((Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                        = TableRef2.TableName)
                      or (Cp.SimpleExp1.Term[0].Factor[0].FieldRef.TableName
                        = TableRef2.Alias))) then begin                             {!!.13}
                        //writeln('    found right constraint', CP.SqlText);
                        NewCF := TffSqlCondFactor.Create(CondExp.CondTerm[0]);
                        NewCF.Assign(CF);
                        CondExp.CondTerm[0].AddCondFactor(NewCF);
                        //writeln('      ',CondExp.SqlText);
                      end;
              end;
            end;
          end;
        end;

      end;
    end;
    {!!.11 end}
    CondExp.MatchType(fftBoolean);
  end;

  Bound := True;
end;

function TffSqlJoinTableExp.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  Result := TableRef1.BindTable(AOwner, TableName);
  if Result = nil then
    Result := TableRef2.BindTable(AOwner, TableName);
end;

function TffSqlJoinTableExp.BindField(const TableName,
  FieldName: string): TFFSqlFieldProxy;
var
  T: TFFSqlTableProxy;
begin
  Result := nil;
  if TableName <> '' then begin
    T := TableRef1.BindTable(Self, TableName);
    if T <> nil then
      if T <> TL then begin
        Result := TableRef1.TargetFieldFromSourceField(T.FieldByName(FieldName));
        exit;
      end;
    if T = nil then begin
      T := TableRef2.BindTable(Self, TableName);
      if T <> nil then                                                 {!!.11}
        if T <> TR then begin
          Result := TableRef2.TargetFieldFromSourceField(T.FieldByName(FieldName));
          exit;
        end;
    end;
    if T = nil then
      SQLError('Unknown field:' + TableName + '.' + FieldName);

    Assert(T <> nil);
    Result := T.FieldByName(FieldName);
    if Result = nil then
      SQLError('Unknown field:' + TableName + '.' + FieldName);
  end else begin
    if TL.FieldByName(FieldName) <> nil then begin
      Result := TL.FieldByName(FieldName);
      Exit;
    end;
    if TR.FieldByName(FieldName) <> nil then begin
      Result := TR.FieldByName(FieldName);
      Exit;
    end;
    SQLError('Unknown field:' + FieldName);
  end;
end;

function TffSqlJoinTableExp.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
var
  i: Integer;
begin
  Result := nil;
  if TableName <> '' then begin
    Result := TableRef1.BindFieldDown(TableName, FieldName);
    if Result = nil then
      Result := TableRef2.BindFieldDown(TableName, FieldName);
    if Result = nil then
      exit;

    EnsureResultTable(False{True});

    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] = Result then begin
        Result := FResultTable.Field(i);
        exit;
      end;

    Result := nil;
  end else begin
    if TL.FieldByName(FieldName) <> nil then begin
      Result := TL.FieldByName(FieldName);
      Exit;
    end;
    if TR.FieldByName(FieldName) <> nil then begin
      Result := TR.FieldByName(FieldName);
      Exit;
    end;
    SQLError('Unknown field:' + FieldName);
  end;
end;

procedure TffSqlJoinTableExp.ClearBindings(Node: TffSqlNode);
begin
  Node.ClearBinding;
end;

function TffSqlJoinTableExp.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  if not Bound then
    Bind;
  Result :=
    ((UsingCondExp <> nil) and UsingCondExp.DependsOn(Table))
    or ((CondExp <> nil) and CondExp.DependsOn(Table));
end;

function TffSqlJoinTableExp.DoJoin(NeedData: Boolean): TffSqlTableProxy;
var
  i : Integer;
  T2 : TffSqlTableProxy;
  F : TffSqlFieldProxy;
  N : TffSqlNode;
  FieldDefList: TffSqlFieldDefList;
  OuterJoinMode: TffSqlOuterJoinMode;
begin

  {build a normal answer table}

  {build field definition for answer table}
  FieldDefList := TffSqlFieldDefList.Create;
  try
    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do begin
      if Columns.Objects[i] is TffSqlFieldProxy then begin
        F := TffSqlFieldProxy(Columns.Objects[i]);
        FieldDefList.AddField(Columns[i], F.GetType, F.GetSize, F.GetDecimals);
      end else begin
        N := TffSqlNode(Columns.Objects[i]);
        FieldDefList.AddField(Columns[i], N.GetType, N.GetSize, N.GetDecimals);
      end;
    end;

    Result := Owner.FDatabase.CreateTemporaryTableWithoutIndex(Self, FieldDefList);
  finally
    FieldDefList.Free;
  end;

  try

    if Joiner = nil then begin

      if UsingCondExp <> nil then
        Joiner := TffSqlJoiner.Create(Owner, UsingCondExp)
      else
        Joiner := TffSqlJoiner.Create(Owner, CondExp);

      Joiner.Sources.Add(
        TFFSqlTableProxySubset.Create(TL));
      Joiner.Sources.Add(
        TFFSqlTableProxySubset.Create(TR));

    end;

    Joiner.ClearColumnList;

    Assert(Assigned(Columns));
    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] is TffSqlFieldProxy then
        Joiner.AddColumn(
            nil,
            TffSqlFieldProxy(Columns.Objects[i]),
            Result.Field(i))
      else
        Joiner.AddColumn(
            TffSqlSimpleExpression(Columns.Objects[i]),
            nil,
            Result.Field(i));

    if NeedData then begin
      Joiner.Target := Result;
      Owner.FDatabase.StartTransaction([nil]);
      try
        case JoinType of
        jtLeftOuter :
          OuterJoinMode := jmLeft;
        jtRightOuter :
          OuterJoinMode := jmRight;
        jtFullOuter :
          OuterJoinMode := jmFull;
        else
          OuterJoinMode := jmNone;
        end;

        Joiner.Execute(Owner.UseIndex, nil, OuterJoinMode);
      except
        Owner.FDatabase.AbortTransaction;
        raise;
      end;
      Owner.FDatabase.Commit;
    end;

    for i := 0 to Result.FieldCount - 1 do
      Result.Field(i).IsTarget := False;

    if (Parent is TffSqlInClause) or (Parent is TffSqlMatchClause) then begin
      {need an index to allow the IN and MATCH clauses to be evaluated}

      T2 := Result.CopySortedOnAllFields(Self);

      Result.Owner := nil;
      Result.Free;
      Result := T2;
    end;
  except
    Result.Owner := nil;
    Result.Free;
    raise;
  end;
end;

procedure TffSqlJoinTableExp.EnsureResultTable(NeedData: Boolean);
begin
  if (NeedData and not HaveData) then begin
    FResultTable.Free;
    FResultTable := nil;
  end;
  if FResultTable = nil then begin
    FResultTable := Execute2(NeedData);
    HaveData := NeedData;
  end;
end;

function TffSqlJoinTableExp.Execute2(NeedData: Boolean): TffSqlTableProxy;
begin
  {check that all referenced tables and fields exist}
  if not Bound then
    Bind;

  {create the result}
  Result := DoJoin(NeedData);
end;

function TffSqlJoinTableExp.GetResultTable: TffSqlTableProxy;
begin
  EnsureResultTable(True);
  Result := FResultTable;
end;

procedure TffSqlJoinTableExp.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlJoinTableExp then begin
    Clear;
    JoinType := TffSqlJoinTableExp(Source).JoinType;
    Natural := TffSqlJoinTableExp(Source).Natural;
    if TffSqlJoinTableExp(Source).TableRef1 <> nil then begin
      TableRef1 := TffSqlTableRef.Create(Self);
      TableRef1.Assign(TffSqlJoinTableExp(Source).TableRef1);
    end;
    if TffSqlJoinTableExp(Source).TableRef2 <> nil then begin
      TableRef2 := TffSqlTableRef.Create(Self);
      TableRef2.Assign(TffSqlJoinTableExp(Source).TableRef2);
    end;
    if TffSqlJoinTableExp(Source).CondExp <> nil then begin
      CondExp := TFFSqlCondExp.Create(Self);
      CondExp.Assign(TffSqlJoinTableExp(Source).CondExp);
    end;
    if TffSqlJoinTableExp(Source).UsingList <> nil then begin
      UsingList := TFFSqlUsingList.Create(Self);
      UsingList.Assign(TffSqlJoinTableExp(Source).UsingList);
    end;
  end else
    AssignError(Source);
end;

procedure TffSqlJoinTableExp.Clear;
begin
  ClearColumns;
  UsingCondExp.Free;
  UsingCondExp := nil;
  TableRef1.Free;
  TableRef1 := nil;
  TableRef2.Free;
  TableRef2 := nil;
  CondExp.Free;
  CondExp := nil;
  UsingList.Free;
  UsingList := nil;
end;

destructor TffSqlJoinTableExp.Destroy;
begin
  ClearColumns;
  Columns.Free;
  Columns := nil;
  {only free the tables if they belongs to us}
  {if they are sub-expressions they will be
   destroyed by the owning expression object}
  if (TL <> nil) and (TL.Owner = Self) then begin
    TL.Owner := nil;
    TL.Free;
  end;
  if (TR <> nil) and (TR.Owner = Self) then begin
    TR.Owner := nil;
    TR.Free;
  end;
  Clear;
  Joiner.Free;
  if FResultTable <> nil then
    if FResultTable.Owner = Self then begin
      FResultTable.Owner := nil;
      FResultTable.Free;
      FResultTable := nil;
    end;
  UsingCondExp.Free;
  inherited;
end;

procedure TffSqlJoinTableExp.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream,' ');
  TableRef1.EmitSQL(Stream);
  if JoinType = jtCross then
    WriteStr(Stream,' CROSS JOIN ')
  else begin
    if Natural then
      WriteStr(Stream,' NATURAL');
    case JoinType of
    jtInner :
      WriteStr(Stream,' INNER');
    jtLeftOuter :
      WriteStr(Stream,' LEFT OUTER');
    jtRightOuter :
      WriteStr(Stream,' RIGHT OUTER');
    jtFullOuter :
      WriteStr(Stream,' FULL OUTER');
    jtUnion :
      WriteStr(Stream,' UNION');
    end;
    WriteStr(Stream,' JOIN');
  end;
  TableRef2.EmitSQL(Stream);
  if CondExp <> nil then begin
    WriteStr(Stream,' ON');
    CondExp.EmitSQL(Stream);
  end;
  if UsingList <> nil then begin
    WriteStr(Stream,' USING (');
    UsingList.EmitSQL(Stream);
    WriteStr(Stream,')');
  end;
end;

procedure TffSqlJoinTableExp.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(TableRef1) then
    TableRef1.EnumNodes(EnumMethod, Deep);
  if assigned(TableRef2) then
    TableRef2.EnumNodes(EnumMethod, Deep);
  if assigned(CondExp) then
    CondExp.EnumNodes(EnumMethod, Deep);
  if assigned(UsingList) then
    UsingList.EnumNodes(EnumMethod, Deep);
end;

function TffSqlJoinTableExp.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    Other is TffSqlJoinTableExp
    and (JoinType = TffSqlJoinTableExp(Other).JoinType)
    and (Natural = TffSqlJoinTableExp(Other).Natural)
    and ((BothNil(TableRef1, TffSqlJoinTableExp(Other).TableRef1)
    or (BothNonNil(TableRef1, TffSqlJoinTableExp(Other).TableRef1)
      and TableRef1.Equals(TffSqlJoinTableExp(Other).TableRef1))))
    and ((BothNil(TableRef2, TffSqlJoinTableExp(Other).TableRef2)
    or (BothNonNil(TableRef2, TffSqlJoinTableExp(Other).TableRef2)
      and TableRef2.Equals(TffSqlJoinTableExp(Other).TableRef2))))
    and ((BothNil(CondExp, TffSqlJoinTableExp(Other).CondExp)
    or (BothNonNil(CondExp, TffSqlJoinTableExp(Other).CondExp)
      and CondExp.Equals(TffSqlJoinTableExp(Other).CondExp))))
    and ((BothNil(UsingList, TffSqlJoinTableExp(Other).UsingList)
    or (BothNonNil(UsingList, TffSqlJoinTableExp(Other).UsingList)
      and UsingList.Equals(TffSqlJoinTableExp(Other).UsingList))));
end;

procedure TffSqlJoinTableExp.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
var
  T : TffSqlTableProxy;
begin
  Assert(Owner <> nil);
  aLiveResult := False;
  T := Execute2(True);
  aCursorID := T.CursorID;
  T.LeaveCursorOpen := True;
  if T.Owner = self then begin
    T.Owner := nil;
    T.Free;
  end;
end;

function TffSqlJoinTableExp.GetFieldsFromTable(const TableName: string; List: TList): TffSqlTableProxy;
var
  i: Integer;
begin
  Result := nil;
  if SameText(TableRef1.Alias, TableName)
  or SameText(TableRef1.TableName, TableName) then begin
    Result := ResultTable;
    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] is TffSqlFieldProxy then
        if TffSqlFieldProxy(Columns.Objects[i]).OwnerTable = TableRef1.FTable then
          List.Add(Columns.Objects[i]);
    exit;
  end;
  if SameText(TableRef2.Alias, TableName)
  or SameText(TableRef2.TableName, TableName) then begin
    Result := ResultTable;
    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] is TffSqlFieldProxy then
        if TffSqlFieldProxy(Columns.Objects[i]).OwnerTable = TableRef2.FTable then
          List.Add(Columns.Objects[i]);
    exit;
  end;
end;

function TffSqlJoinTableExp.Reduce: Boolean;
begin
  if assigned(CondExp) then
    Result := CondExp.Reduce
  else
    Result := False;
  {!!.11 begin}
  if not Result then
    if TableRef1.Reduce then
      Result := True
    else
    if TableRef2.Reduce then
      Result := True;
  {!!.11 end}
end;

function TffSqlJoinTableExp.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
var
  i: Integer;
begin
  for i := 0 to pred(Columns.Count) do
    if Columns.Objects[i] = F then begin
      Result := ResultTable.Field(i);
      exit;
    end;
  {!!.11 begin}
  {We don't have the sought after source field represented in
   our answer table directly, but it might be represented
   indirectly as a field in a nested table expression}
  Result := TableRef1.TargetFieldFromSourceField(F);
  if Result <> nil then begin
    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] = Result then begin
        Result := ResultTable.Field(i);
        exit;
      end;
  end;
  Result := TableRef2.TargetFieldFromSourceField(F);
  if Result <> nil then begin
    for i := 0 to pred(Columns.Count) do
      if Columns.Objects[i] = Result then begin
        Result := ResultTable.Field(i);
        exit;
      end;
  end;
  {!!.11 end}
  Result := nil;
end;

{ TffSqlNonJoinTableTerm }

procedure TffSqlNonJoinTableTerm.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlNonJoinTableTerm then begin
    Clear;
    if TffSqlNonJoinTableTerm(Source).NonJoinTablePrimary <> nil then begin
      NonJoinTablePrimary := TffSqlNonJoinTablePrimary.Create(Self);
      NonJoinTablePrimary.Assign(TffSqlNonJoinTableTerm(Source).NonJoinTablePrimary);
    end;
  end else
    AssignError(Source);
end;

function TffSqlNonJoinTableTerm.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  Result := NonJoinTablePrimary.BindFieldDown(TableName, FieldName);
end;

function TffSqlNonJoinTableTerm.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  Result := NonJoinTablePrimary.BindTable(AOwner, TableName);
end;

procedure TffSqlNonJoinTableTerm.Clear;
begin
  NonJoinTablePrimary.Free;
  NonJoinTablePrimary := nil;
end;

function TffSqlNonJoinTableTerm.DependsOn(
  Table: TFFSqlTableProxy): Boolean;
begin
  Assert(NonJoinTablePrimary <> nil);
  Result := NonJoinTablePrimary.DependsOn(Table);
end;

destructor TffSqlNonJoinTableTerm.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlNonJoinTableTerm.EmitSQL(Stream: TStream);
begin
  if assigned(NonJoinTablePrimary) then
    NonJoinTablePrimary.EmitSQL(Stream);
end;

procedure TffSqlNonJoinTableTerm.EnsureResultTable(NeedData: Boolean);
begin
  assert(assigned(NonJoinTablePrimary));
  NonJoinTablePrimary.EnsureResultTable(NeedData);
end;

procedure TffSqlNonJoinTableTerm.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(NonJoinTablePrimary) then
    NonJoinTablePrimary.EnumNodes(EnumMethod, Deep);
end;

function TffSqlNonJoinTableTerm.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    Other is TffSqlNonJoinTableTerm
    and ((BothNil(NonJoinTablePrimary, TffSqlNonJoinTableTerm(Other).NonJoinTablePrimary)
    or (BothNonNil(NonJoinTablePrimary, TffSqlNonJoinTableTerm(Other).NonJoinTablePrimary)
      and NonJoinTablePrimary.Equals(TffSqlNonJoinTableTerm(Other).NonJoinTablePrimary))))
end;

procedure TffSqlNonJoinTableTerm.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
begin
  Assert(NonJoinTablePrimary <> nil);
  NonJoinTablePrimary.Execute(aLiveResult, aCursorID, RecordsRead);
end;

function TffSqlNonJoinTableTerm.GetResultTable: TffSqlTableProxy;
begin
  Assert(NonJoinTablePrimary <> nil);
  Result := NonJoinTablePrimary.ResultTable;
end;

function TffSqlNonJoinTableTerm.Reduce: Boolean;
begin
  Assert(NonJoinTablePrimary <> nil);
  Result := NonJoinTablePrimary.Reduce;
end;

function TffSqlNonJoinTableTerm.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
begin
  Result := NonJoinTablePrimary.TargetFieldFromSourceField(F);
end;

{ TffSqlNonJoinTableExp }

procedure TffSqlNonJoinTableExp.Assign(const Source: TffSqlNode);
begin
  if Source is TffSqlNonJoinTableExp then begin
    Clear;
    if TffSqlNonJoinTableExp(Source).NonJoinTableTerm <> nil then begin
      NonJoinTableTerm := TffSqlNonJoinTableTerm.Create(Self);
      NonJoinTableTerm.Assign(TffSqlNonJoinTableExp(Source).NonJoinTableTerm);
    end;
  end else
    AssignError(Source);
end;

function TffSqlNonJoinTableExp.BindFieldDown(const TableName,
  FieldName: string): TFFSqlFieldProxy;
begin
  Result := NonJoinTableTerm.BindFieldDown(TableName, FieldName);
end;

function TffSqlNonJoinTableExp.BindTable(AOwner: TObject;
  const TableName: string): TFFSqlTableProxy;
begin
  Result := NonJoinTableTerm.BindTable(AOwner, TableName);
end;

procedure TffSqlNonJoinTableExp.Clear;
begin
  NonJoinTableTerm.Free;
  NonJoinTableTerm := nil;
end;

function TffSqlNonJoinTableExp.DependsOn(Table: TFFSqlTableProxy): Boolean;
begin
  Assert(NonJoinTableTerm <> nil);
  Result := NonJoinTableTerm.DependsOn(Table);
end;

destructor TffSqlNonJoinTableExp.Destroy;
begin
  Clear;
  inherited;
end;

procedure TffSqlNonJoinTableExp.EmitSQL(Stream: TStream);
begin
  if assigned(NonJoinTableTerm) then
    NonJoinTableTerm.EmitSQL(Stream);
end;

procedure TffSqlNonJoinTableExp.EnsureResultTable(NeedData: Boolean);
begin
  Assert(Assigned(NonJoinTableTerm));
  NonJoinTableTerm.EnsureResultTable(NeedData);
end;

procedure TffSqlNonJoinTableExp.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
  if assigned(NonJoinTableTerm) then
    NonJoinTableTerm.EnumNodes(EnumMethod, Deep);
end;

function TffSqlNonJoinTableExp.Equals(Other: TffSqlNode): Boolean;
begin
  Result :=
    Other is TffSqlNonJoinTableExp
    and ((BothNil(NonJoinTableTerm, TffSqlNonJoinTableExp(Other).NonJoinTableTerm)
    or (BothNonNil(NonJoinTableTerm, TffSqlNonJoinTableExp(Other).NonJoinTableTerm)
      and NonJoinTableTerm.Equals(TffSqlNonJoinTableExp(Other).NonJoinTableTerm))))
end;

procedure TffSqlNonJoinTableExp.Execute(
  var aLiveResult: Boolean; var aCursorID: TffCursorID;
  var RecordsRead: Integer);
begin
  Assert(NonJoinTableTerm <> nil);
  NonJoinTableTerm.Execute(aLiveResult, aCursorID, RecordsRead);
end;

{!!.11 new}
function TffSqlNonJoinTableExp.GetFieldsFromTable(const TableName: string; List: TList): TffSqlTableProxy;                     {!!.11}
begin
  Result := nil;
end;

function TffSqlNonJoinTableExp.GetResultTable: TffSqlTableProxy;
begin
  Assert(NonJoinTableTerm <> nil);
  Result := NonJoinTableTerm.ResultTable;
end;

function TffSqlNonJoinTableExp.Reduce: Boolean;
begin
  Assert(NonJoinTableTerm <> nil);
  Result := NonJoinTableTerm.Reduce;
end;

constructor TffSqlJoinTableExp.Create;
begin
  inherited;
  Columns := TStringList.Create;
end;

function TffSqlNonJoinTableExp.TargetFieldFromSourceField(
  const F: TffSqlFieldProxy): TffSqlFieldProxy;
begin
  Result := NonJoinTableTerm.TargetFieldFromSourceField(F);
end;

{ TFFSqlUsingItem }

procedure TFFSqlUsingItem.Assign(const Source: TffSqlNode);
begin
  if Source is TFFSqlUsingItem then begin
    ColumnName := TFFSqlUsingItem(Source).ColumnName;
  end else
    AssignError(Source);
end;

procedure TFFSqlUsingItem.EmitSQL(Stream: TStream);
begin
  WriteStr(Stream, ' ');
  WriteStr(Stream, ColumnName);
end;

procedure TFFSqlUsingItem.EnumNodes(EnumMethod: TffSqlEnumMethod;
  const Deep: Boolean);
begin
  EnumMethod(Self);
end;

function TFFSqlUsingItem.Equals(Other: TffSqlNode): Boolean;
begin
  if Other is TFFSqlUsingItem then
    Result := ColumnName = TFFSqlUsingItem(Other).ColumnName
  else
    Result := False;
end;

{===TffSqlUsingList==================================================}
function TffSqlUsingList.AddItem(NewUsing: TffSqlUsingItem): TffSqlUsingItem;
begin
  FUsingItemList.Add(NewUsing);
  Result := NewUsing;
end;
{--------}
procedure TffSqlUsingList.Assign(const Source: TffSqlNode);
var
  i: Integer;
begin
  if Source is TffSqlUsingList then begin
    Clear;
    for i := 0 to pred(TffSqlUsingList(Source).UsingCount) do
      AddItem(TffSqlUsingItem.Create(Self)).Assign(
        TffSqlUsingList(Source).UsingItem[i]);
  end else
    AssignError(Source);
end;

constructor TffSqlUsingList.Create(AParent: TffSqlNode);
begin
  inherited Create(AParent);
  FUsingItemList := TList.Create;
end;
{--------}
procedure TffSqlUsingList.Clear;
var
  i : Integer;
begin
  for i := 0 to pred(FUsingItemList.Count) do
    UsingItem[i].Free;
  FUsingItemList.Clear;
end;
{--------}
destructor TffSqlUsingList.Destroy;
begin
  Clear;
  FUsingItemList.Free;
  inherited;
end;
{--------}
procedure TffSqlUsingList.EmitSQL(Stream: TStream);
var
  i : Integer;
begin
  UsingItem[0].EmitSQL(Stream);
  for i := 1 to pred(UsingCount) do begin
    WriteStr(Stream,', ');
    UsingItem[i].EmitSQL(Stream);
  end;
end;
{--------}
procedure TffSqlUsingList.EnumNodes(EnumMethod: TffSqlEnumMethod; const Deep: Boolean);
var
  i : Integer;
begin
  EnumMethod(Self);
  for i := 0 to pred(UsingCount) do
    UsingItem[i].EnumNodes(EnumMethod, Deep);
end;
{--------}
function TffSqlUsingList.Equals(Other: TffSqlNode): Boolean;
var
  i : Integer;
begin
  Result := False;
  if Other is TffSqlUsingList then begin
    if UsingCount <> TffSqlUsingList(Other).UsingCount then
      exit;
    for i := 0 to pred(UsingCount) do
      if not UsingItem[i].Equals(TffSqlUsingList(Other).UsingItem[i]) then
        exit;
    Result := True;
  end;
end;
{--------}
function TffSqlUsingList.GetUsingCount: Integer;
begin
  Result := FUsingItemList.Count;
end;
{--------}
function TffSqlUsingList.GetUsingItem(
  Index: Integer): TffSqlUsingItem;
begin
  Result := TffSqlUsingItem(FUsingItemList[Index]);
end;
{--------}
procedure TffSqlUsingList.SetUsingItem(Index: Integer;
  const Value: TffSqlUsingItem);
begin
  FUsingItemList[Index] := Value;
end;
{====================================================================}

initialization
  {calculate TimeDelta as one second}                                {!!.01}
  TimeDelta := EncodeTime(0, 0, 2, 0) - EncodeTime(0, 0, 1, 0);      {!!.01}
end.


