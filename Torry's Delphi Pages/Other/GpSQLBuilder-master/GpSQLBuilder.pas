///<summary>SQL query builder.</summary>
///<author>Primoz Gabrijelcic</author>
///<remarks><para>
///Copyright (c) 2015, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without
///modification, are permitted provided that the following conditions are met:
///
///* Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///
///* Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///
///* Neither the name of GpSQLBuilder nor the names of its
///  contributors may be used to endorse or promote products derived from
///  this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
///AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
///IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
///FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
///DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
///SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
///CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
///OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
///OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2010-11-24
///   Last modification : 2015-07-12
///   Version           : 3.05
///</para><para>
///   History:
///     3.05: 2015-07-12
///       - [leledumbo] Added .Insert and .Into methods.
///       - Sorted TGpSQLBuilder implementation.
///       - &Set(column, value: string) overload automatically adds quotes around the
///         `value` parameter when converted to string. If you don't want that to
///         happen, you can use the other overload: &Set(column, [value]).
///     3.04a: 2015-06-30
///       - Fixed a bug when Where.&Or was called before Where.&And.
///     3.04: 2015-06-18
///       - Following methods now accept `expression: IGpSQLBuilderExpression` parameter:
///         IGpSQLBuilderCase.When, IGpSQLBuilder.&Case, IGpSQLBuilder.Having,
///         IGpSQLBuilder.Where.
///       - TGpSQLBuilderExpression.&Or can be used on an empty expression (it is
///         silently converted to &And). This simplifies writing `for` loops which
///         add conditions with &Or.
///       - Added helper class SQL.
///     3.03: 2015-06-17
///       - Added .Update, .&Set, and .Delete methods.
///     3.02: 2015-05-05
///       - IGpSQLColums was renamed to IGpSQLNames.
///       - IGpSQLBuilder.From adds a new name on each call to accomodate multiple tables
///         in the From part.
///     3.01: 2015-04-30
///       - Added .Distinct method.
///     3.0: 2015-04-29
///       - Internal redesign: SQL is generated as an abstract syntax tree and only
///         converted to text when AsString is called. This allows implementing the
///         'pretty print' function and makes the code less ugly.
///       - Added other Join types.
///       - Case expression can be used in the OrderBy section.
///     2.02a: 2015-04-20
///       - Corrected SQL generation for LeftJoin().&As() construct.
///     2.02: 2015-04-05
///       - Reimplemented old .Subquery mechanism as .Expression: IGpSQLBuilderExpression.
///       - Added &And and &Or overloads accepting IGpSQLBuilderExpression.
///     2.01: 2015-04-05
///       - Added integer-accepting overloads for IGpSQLBuilderCase.&Then and .&Else.
///     2.0: 2015-04-04
///       - Removed AndE and OrE aliases.
///       - Removed Column overload which accepted 'alias' parameter.
///       - Removed 'dbAlias' parameter from From and LeftJoin methods.
///       - Removed 'subquery' concept as it was not really useful.
///       - Renamed AllColumns to All.
///       - Renamed AsAlias to &As.
///       - IGpSQLBuilderCase.&Then and .&Else values are not automatically quoted.
///     1.08: 2015-04-03
///        - &And and &Or aliases for AndE and OrE.
///     1.07: 2015-04-02
///       - IGpSQLBuilder
///         - Added new overloads for Select, Where, OrderBy, GroupBy, and Having.
///         - Added property ActiveSection.
///         - Added methods &On and &Case.
///       - Added case-implementing interface IGpSQLBuilderCase.
///     1.06: 2015-03-16
///       - Exposed Sections[].
///       - Added parameter dbAlias to the Column method.
///       - Added overloaded Column acception array of const.
///       - Added parameter dbAlias to the LeftJoin method.
///       - Fixed IGpSQLBuilderSection.Clear.
///     1.05: 2015-03-13
///       - Added parameter dbAlias to the From method.
///     1.04: 2013-03-06
///       - Added function AllColumns.
///     1.03: 2013-03-04
///       - Supports multiple left joins.
///     1.02: 2012-01-10
///       - Supports multiple 'from' databases.
///     1.01: 2010-12-02
///       - Added 'OR' expression builder.
///     1.0b: 2010-11-30
///       - Fixed memory leak in TGpSQLBuilder.Destroy.
///     1.0a: 2010-11-25
///       - AsAlias did not insert 'AS' token.
///       - Clear and ClearAll did not return result.
///     1.0: 2010-11-24
///       - Released.
///</para></remarks>

unit GpSQLBuilder;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  GpSQLBuilder.AST;

type
  IGpSQLBuilder = interface;

  IGpSQLBuilderExpression = interface ['{CC7ED7A2-3B39-4341-9CBB-EE1C7851BBA9}']
    function  GetAsString: string;
    function  GetExpression: IGpSQLExpression;
  //
    function  &And(const expression: array of const): IGpSQLBuilderExpression; overload;
    function  &And(const expression: string): IGpSQLBuilderExpression; overload;
    function  &And(const expression: IGpSQLExpression): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: array of const): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: string): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: IGpSQLExpression): IGpSQLBuilderExpression; overload;
    property AsString: string read GetAsString;
    property Expression: IGpSQLExpression read GetExpression;
  end; { IGpSQLBuilderExpression }

  IGpSQLBuilderCase = interface ['{1E379718-0959-455A-80AA-63BDA7C92F8C}']
    function  GetAsString: string;
    function  GetCase: IGpSQLCase;
  //
    function  &And(const expression: array of const): IGpSQLBuilderCase; overload;
    function  &And(const expression: string): IGpSQLBuilderCase; overload;
    function  &And(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function  &Else(const value: string): IGpSQLBuilderCase; overload;
    function  &Else(const value: int64): IGpSQLBuilderCase; overload;
    function  &End: IGpSQLBuilderCase;
    function  &Or(const expression: array of const): IGpSQLBuilderCase; overload;
    function  &Or(const expression: string): IGpSQLBuilderCase; overload;
    function  &Or(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function  &Then(const value: string): IGpSQLBuilderCase; overload;
    function  &Then(const value: int64): IGpSQLBuilderCase; overload;
    function  When(const condition: string): IGpSQLBuilderCase; overload;
    function  When(const condition: array of const): IGpSQLBuilderCase; overload;
    function  When(const condition: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    property &Case: IGpSQLCase read GetCase;
    property AsString: string read GetAsString;
  end; { IGpSQLBuilderCase }

  IGpSQLBuilder = interface ['{43EA3E34-A8DB-4257-A19F-030F404646E7}']
    function &And(const expression: string): IGpSQLBuilder; overload;
  //
    function &And(const expression: array of const): IGpSQLBuilder; overload;
    function &And(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function &As(const alias: string): IGpSQLBuilder;
    function &Case(const expression: string = ''): IGpSQLBuilderCase; overload;
    function &Case(const expression: array of const): IGpSQLBuilderCase; overload;
    function &Case(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function &On(const expression: string): IGpSQLBuilder; overload;
    function &On(const expression: array of const): IGpSQLBuilder; overload;
    function &Or(const expression: string): IGpSQLBuilder; overload;
    function &Or(const expression: array of const): IGpSQLBuilder; overload;
    function &Or(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function All: IGpSQLBuilder;
  //
    function Clear: IGpSQLBuilder;
    function ClearAll: IGpSQLBuilder;
    function Column(const colName: string): IGpSQLBuilder; overload;
    function Column(const dbName, colName: string): IGpSQLBuilder; overload;
    function Column(const colName: array of const): IGpSQLBuilder; overload;
    function Column(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function Delete: IGpSQLBuilder;
    function Desc: IGpSQLBuilder;
    function Distinct: IGpSQLBuilder;
    function Expression(const term: string = ''): IGpSQLBuilderExpression; overload;
    function Expression(const term: array of const): IGpSQLBuilderExpression; overload;
    function First(num: integer): IGpSQLBuilder;
    function From(const dbName: string): IGpSQLBuilder;
    function FullJoin(const dbName: string): IGpSQLBuilder;
    function GetAsString: string;
    function GetAST: IGpSQLAST;
    function GroupBy(const colName: string = ''): IGpSQLBuilder;
    function Having(const expression: string = ''): IGpSQLBuilder; overload;
    function Having(const expression: array of const): IGpSQLBuilder; overload;
    function Having(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function InnerJoin(const dbName: string): IGpSQLBuilder;
    function Insert: IGpSQLBuilder;
    function Into(const tableName: string): IGpSQLBuilder;
    function IsEmpty: boolean;
    function LeftJoin(const dbName: string): IGpSQLBuilder;
    function OrderBy(const colName: string = ''): IGpSQLBuilder; overload;
    function OrderBy(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function RightJoin(const dbName: string): IGpSQLBuilder;
    function Select(const colName: string = ''): IGpSQLBuilder; overload;
    function Select(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function &Set(const colName, colValue: string): IGpSQLBuilder; overload;
    function &Set(const colName: string; const colValue: array of const): IGpSQLBuilder; overload;
    function Skip(num: integer): IGpSQLBuilder;
    function Update(const tableName: string): IGpSQLBuilder;
    function Where(const expression: string = ''): IGpSQLBuilder; overload;
    function Where(const expression: array of const): IGpSQLBuilder; overload;
    function Where(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    property AsString: string read GetAsString;
    property AST: IGpSQLAST read GetAST;
  end; { IGpSQLBuilder }

  SQL = class
    class function Count(const s: string): string; overload;
    class function Count(const s: IGpSQLBuilder): string; overload;
    class function Count(const s: IGpSQLBuilderExpression): string; overload;
    class function Exists(const s: string): string; overload;
    class function Exists(const s: IGpSQLBuilder): string; overload;
    class function Exists(const s: IGpSQLBuilderExpression): string; overload;
    class function Lower(const s: string): string; overload;
    class function Lower(const s: IGpSQLBuilder): string; overload;
    class function Lower(const s: IGpSQLBuilderExpression): string; overload;
    class function Min(const s: string): string; overload;
    class function Min(const s: IGpSQLBuilder): string; overload;
    class function Min(const s: IGpSQLBuilderExpression): string; overload;
    class function Max(const s: string): string; overload;
    class function Max(const s: IGpSQLBuilder): string; overload;
    class function Max(const s: IGpSQLBuilderExpression): string; overload;
    class function Upper(const s: string): string; overload;
    class function Upper(const s: IGpSQLBuilder): string; overload;
    class function Upper(const s: IGpSQLBuilderExpression): string; overload;
    class function Q(const s: string): string; overload;
    class function Q(const s: IGpSQLBuilder): string; overload;
    class function Q(const s: IGpSQLBuilderExpression): string; overload;
  end; { SQL }

function CreateGpSQLBuilder: IGpSQLBuilder;

implementation

uses
  {$IFNDEF FPC}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  GpSQLBuilder.Serialize;

type
  TGpSQLBuilderExpression = class(TInterfacedObject, IGpSQLBuilderExpression)
  strict private
    FExpression: IGpSQLExpression;
    FLastAnd   : IGpSQLExpression;
  strict protected
    function  FindRightmostAnd(const expression: IGpSQLExpression): IGpSQLExpression;
    function  GetAsString: string;
    function  GetExpression: IGpSQLExpression;
  public
    constructor Create(const expression: string = ''); overload;
    constructor Create(const expression: IGpSQLExpression); overload;
    function  &And(const expression: array of const): IGpSQLBuilderExpression; overload;
    function  &And(const expression: string): IGpSQLBuilderExpression; overload;
    function  &And(const expression: IGpSQLExpression): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: array of const): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: string): IGpSQLBuilderExpression; overload;
    function  &Or(const expression: IGpSQLExpression): IGpSQLBuilderExpression; overload;
    property AsString: string read GetAsString;
    property Expression: IGpSQLExpression read GetExpression;
  end; { TGpSQLBuilderExpression }

  TGpSQLBuilderCase = class(TInterfacedObject, IGpSQLBuilderCase)
  strict private
    FCase    : IGpSQLCase;
    FLastExpr: IGpSQLBuilderExpression;
  strict protected
    function  GetAsString: string;
    function GetCase: IGpSQLCase;
  public
    constructor Create(const expression: string);
    function  &And(const expression: array of const): IGpSQLBuilderCase; overload;
    function  &And(const expression: string): IGpSQLBuilderCase; overload;
    function  &And(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function  &Else(const value: string): IGpSQLBuilderCase; overload;
    function  &Else(const value: int64): IGpSQLBuilderCase; overload;
    function  &End: IGpSQLBuilderCase;
    function  Expression: IGpSQLBuilderExpression;
    function  &Or(const expression: array of const): IGpSQLBuilderCase; overload;
    function  &Or(const expression: string): IGpSQLBuilderCase; overload;
    function  &Or(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function  &Then(const value: string): IGpSQLBuilderCase; overload;
    function  &Then(const value: int64): IGpSQLBuilderCase; overload;
    function  When(const condition: string): IGpSQLBuilderCase; overload;
    function  When(const condition: array of const): IGpSQLBuilderCase; overload;
    function  When(const condition: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    property &Case: IGpSQLCase read GetCase;
    property AsString: string read GetAsString;
  end; { TGpSQLBuilderCase }

  TGpSQLBuilder = class(TInterfacedObject, IGpSQLBuilder)
  strict private
  type
    TGpSQLSection = (secSelect, secDelete, secInsert, secUpdate, secJoin, secWhere, secGroupBy, secHaving, secOrderBy);
    TGpSQLSections = set of TGpSQLSection;
  var
    FActiveSection: TGpSQLSection;
    FActiveExpr   : IGpSQLBuilderExpression;
    FActiveValues : IGpSQLNameValuePairs;
    FAST          : IGpSQLAST;
    FASTColumns   : IGpSQLNames;
    FASTSection   : IGpSQLSection;
    FASTName      : IGpSQLName;
    FTableNames   : IGpSQLNames;
  strict protected
    function  AutoQuote(const s: string): string;
    procedure AssertHaveName;
    procedure AssertSection(sections: TGpSQLSections);
    function  CreateJoin(joinType: TGpSQLJoinType; const dbName: string): IGpSQLBuilder;
    function  GetAsString: string;
    function  GetAST: IGpSQLAST;
    function  InternalSet(const colName, colValue: string): IGpSQLBuilder;
    procedure SelectSection(section: TGpSQLSection);
  public
    constructor Create;
    function  &And(const expression: array of const): IGpSQLBuilder; overload;
    function  &And(const expression: string): IGpSQLBuilder; overload;
    function  &And(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function  All: IGpSQLBuilder;
    function  &As(const alias: string): IGpSQLBuilder;
    function  &Case(const expression: string = ''): IGpSQLBuilderCase; overload;
    function  &Case(const expression: array of const): IGpSQLBuilderCase; overload;
    function  &Case(const expression: IGpSQLBuilderExpression): IGpSQLBuilderCase; overload;
    function  Clear: IGpSQLBuilder;
    function  ClearAll: IGpSQLBuilder;
    function  Column(const colName: string): IGpSQLBuilder; overload;
    function  Column(const dbName, colName: string): IGpSQLBuilder; overload;
    function  Column(const colName: array of const): IGpSQLBuilder; overload;
    function  Column(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function  Delete: IGpSQLBuilder;
    function  Desc: IGpSQLBuilder;
    function  Distinct: IGpSQLBuilder;
    function  Expression(const term: string = ''): IGpSQLBuilderExpression; overload;
    function  Expression(const term: array of const): IGpSQLBuilderExpression; overload;
    function  First(num: integer): IGpSQLBuilder;
    function  From(const dbName: string): IGpSQLBuilder;
    function  FullJoin(const dbName: string): IGpSQLBuilder;
    function  GroupBy(const colName: string = ''): IGpSQLBuilder;
    function  Having(const expression: string = ''): IGpSQLBuilder; overload;
    function  Having(const expression: array of const): IGpSQLBuilder; overload;
    function  Having(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function  Insert: IGpSQLBuilder;
    function  Into(const tableName: string): IGpSQLBuilder;
    function  InnerJoin(const dbName: string): IGpSQLBuilder;
    function  IsEmpty: boolean;
    function  LeftJoin(const dbName: string): IGpSQLBuilder;
    function  &On(const expression: string): IGpSQLBuilder; overload;
    function  &On(const expression: array of const): IGpSQLBuilder; overload;
    function  &Or(const expression: array of const): IGpSQLBuilder; overload;
    function  &Or(const expression: string): IGpSQLBuilder; overload;
    function  &Or(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    function  OrderBy(const colName: string = ''): IGpSQLBuilder; overload;
    function  OrderBy(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function  RightJoin(const dbName: string): IGpSQLBuilder;
    function  Select(const colName: string = ''): IGpSQLBuilder; overload;
    function  Select(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder; overload;
    function  &Set(const colName, colValue: string): IGpSQLBuilder; overload;
    function  &Set(const colName: string; const colValue: array of const): IGpSQLBuilder; overload;
    function  Skip(num: integer): IGpSQLBuilder;
    function  Update(const tableName: string): IGpSQLBuilder;
    function  Where(const expression: string = ''): IGpSQLBuilder; overload;
    function  Where(const expression: array of const): IGpSQLBuilder; overload;
    function  Where(const expression: IGpSQLBuilderExpression): IGpSQLBuilder; overload;
    property AsString: string read GetAsString;
    property AST: IGpSQLAST read GetAST;
  end; { TGpSQLBuilder }

{ exports }

function CreateGpSQLBuilder: IGpSQLBuilder;
begin
  Result := TGpSQLBuilder.Create;
end; { CreateGpSQLBuilder }

{ globals }

function VarRecToString(const vr: TVarRec): string;
const
  BoolChars: array [boolean] of string = ('F', 'T');
{$ifndef fpc}
type
  PtrUInt = Integer;
{$endif}
begin
  case vr.VType of
    vtInteger:    Result := IntToStr(vr.VInteger);
    vtBoolean:    Result := BoolChars[vr.VBoolean];
    vtChar:       Result := char(vr.VChar);
    vtExtended:   Result := FloatToStr(vr.VExtended^);
    vtString:     Result := string(vr.VString^);
    vtPointer:    Result := IntToHex(PtrUInt(vr.VPointer),8);
    vtPChar:      Result := string(vr.VPChar^);
    vtObject:     Result := vr.VObject.ClassName;
    vtClass:      Result := vr.VClass.ClassName;
    vtWideChar:   Result := string(vr.VWideChar);
    vtPWideChar:  Result := string(vr.VPWideChar^);
    vtAnsiString: Result := string(vr.VAnsiString);
    vtCurrency:   Result := CurrToStr(vr.VCurrency^);
    vtVariant:    Result := string(vr.VVariant^);
    vtWideString: Result := string(WideString(vr.VWideString));
    vtInt64:      Result := IntToStr(vr.VInt64^);
    {$IFDEF Unicode}
    vtUnicodeString: Result := string(vr.VUnicodeString);
    {$ENDIF}
    else raise Exception.Create('VarRecToString: Unsupported parameter type');
  end;
end; { VarRecToString }

function SqlParamsToStr(const params: array of const): string;
var
  iParam: integer;
  lastCh: char;
  sParam: string;
begin
  Result := '';
  for iParam := Low(params) to High(params) do begin
    sParam := VarRecToString(params[iparam]);
    if Result = '' then
      lastCh := ' '
    else
      lastCh := Result[Length(Result)];
    if (lastCh <> '.') and (lastCh <> '(') and (lastCh <> ' ') and (lastCh <> ':') and
       (sParam <> ',') and (sParam <> '.') and (sParam <> ')')
    then
      Result := Result + ' ';
    Result := Result + sParam;
  end;
end; { SqlParamsToStr }

{ TGpSQLBuilderCase }

constructor TGpSQLBuilderCase.Create(const expression: string);
begin
  inherited Create;
  FCase := CreateSQLCase;
  if expression <> '' then
    FCase.CaseExpression.Term := expression;
end; { TGpSQLBuilderCase.Create }

function TGpSQLBuilderCase.&And(const expression: array of const): IGpSQLBuilderCase;
begin
  FLastExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilderCase.&And(const expression: string): IGpSQLBuilderCase;
begin
  FLastExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilderCase.&And(const expression: IGpSQLBuilderExpression):
  IGpSQLBuilderCase;
begin
  FLastExpr.&And(expression.Expression);
  Result := Self;
end; { TGpSQLBuilderCase.&And }

function TGpSQLBuilderCase.&Else(const value: string): IGpSQLBuilderCase;
begin
  FLastExpr := TGpSQLBuilderExpression.Create(value);
  FCase.ElseExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.&Else }

function TGpSQLBuilderCase.&Else(const value: int64): IGpSQLBuilderCase;
begin
  Result := &Else(IntToStr(value));
end; { TGpSQLBuilderCase.&Else }

function TGpSQLBuilderCase.&End: IGpSQLBuilderCase;
begin
  Result := Self;
end; { TGpSQLBuilderCase.&End }

function TGpSQLBuilderCase.GetAsString: string;
begin
  Result := CreateSQLSerializer(FCase).AsString;
end; { TGpSQLBuilderCase.GetAsString }

function TGpSQLBuilderCase.&Or(const expression: array of const): IGpSQLBuilderCase;
begin
  FLastExpr.&Or(expression);
  Result := Self;
end; {  TGpSQLBuilder.&Or}

function TGpSQLBuilderCase.&Or(const expression: string): IGpSQLBuilderCase;
begin
  FLastExpr.&Or(expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilderCase.&Or(const expression: IGpSQLBuilderExpression):
  IGpSQLBuilderCase;
begin
  FLastExpr.&Or(expression.Expression);
  Result := Self;
end; { TGpSQLBuilderCase.&Or }

function TGpSQLBuilderCase.&Then(const value: string): IGpSQLBuilderCase;
begin
  Assert(FCase.WhenList.Count > 0, 'TGpSQLBuilderCase.&Then: Missing When');
  FLastExpr := TGpSQLBuilderExpression.Create(value);
  FCase.WhenList[FCase.WhenList.Count-1].ThenExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.&Then }

function TGpSQLBuilderCase.&Then(const value: int64): IGpSQLBuilderCase;
begin
  Result := &Then(IntToStr(value));
end; { TGpSQLBuilderCase.&Then }

function TGpSQLBuilderCase.Expression: IGpSQLBuilderExpression;
begin
  Result := TGpSQLBuilderExpression.Create;
end; { TGpSQLBuilderCase.Expression }

function TGpSQLBuilderCase.GetCase: IGpSQLCase;
begin
  Result := FCase;
end; { TGpSQLBuilderCase.GetCase }

function TGpSQLBuilderCase.When(const condition: array of const): IGpSQLBuilderCase;
begin
  Result := When(SqlParamsToStr(condition));
end; { TGpSQLBuilderCase.When }

function TGpSQLBuilderCase.When(const condition: string): IGpSQLBuilderCase;
begin
  Result := When(TGpSQLBuilderExpression.Create(condition));
end; { TGpSQLBuilderCase.When }

function TGpSQLBuilderCase.When(const condition: IGpSQLBuilderExpression):
  IGpSQLBuilderCase;
var
  wt: IGpSQLCaseWhenThen;
begin
  FLastExpr := condition;
  wt := FCase.WhenList.Add;
  wt.WhenExpression := FLastExpr.Expression;
  Result := Self;
end; { TGpSQLBuilderCase.When }

{ TGpSQLBuilderExpression }

constructor TGpSQLBuilderExpression.Create(const expression: string);
begin
  inherited Create;
  FExpression := CreateSQLExpression;
  if expression <> '' then
    &And(expression);
end; { TGpSQLBuilderExpression.Create }

constructor TGpSQLBuilderExpression.Create(const expression: IGpSQLExpression);
begin
  inherited Create;
  FExpression := expression;
  FLastAnd := FindRightmostAnd(expression);
end; { TGpSQLBuilderExpression.Create }

function TGpSQLBuilderExpression.&And(const expression: string): IGpSQLBuilderExpression;
var
  node: IGpSQLExpression;
begin
  node := CreateSQLExpression;
  node.Term := expression;
  Result := &And(node);
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.&And(
  const expression: array of const): IGpSQLBuilderExpression;
begin
  Result := &And(SqlParamsToStr(expression));
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.&And(const expression: IGpSQLExpression):
  IGpSQLBuilderExpression;
var
  node: IGpSQLExpression;
  root: IGpSQLExpression;
begin
  root := FExpression;
  if root.IsEmpty then begin
    root.Assign(expression);
    FLastAnd := root;
  end
  else begin
    node := CreateSQLExpression;
    node.Assign(root);
    root.Left := node;
    root.Operation := opAnd;
    root.Right := expression;
    FLastAnd := root.Right;
  end;
  Result := Self;
end; { TGpSQLBuilderExpression.&And }

function TGpSQLBuilderExpression.FindRightmostAnd(const expression: IGpSQLExpression):
  IGpSQLExpression;
begin
  if expression.Operation = opNone then
    Result := expression
  else if expression.Operation = opOr then
    Result := expression
  else
    Result := FindRightmostAnd(expression.Right);
end; { TGpSQLBuilderExpression.FindRightmostAnd }

function TGpSQLBuilderExpression.&Or(const expression: string): IGpSQLBuilderExpression;
var
  node: IGpSQLExpression;
begin
  node := CreateSQLExpression;
  node.Term := expression;
  Result := &Or(node);
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.&Or(const expression: array of const): IGpSQLBuilderExpression;
begin
  Result := &Or(SqlParamsToStr(expression));
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.&Or(const expression: IGpSQLExpression): IGpSQLBuilderExpression;
var
  node: IGpSQLExpression;
begin
  if (not assigned(FLastAnd)) or FLastAnd.IsEmpty then
    Result := &And(expression)
  else begin
    node := CreateSQLExpression;
    node.Assign(FLastAnd);
    FLastAnd.Left := node;
    FLastAnd.Operation := opOr;
    FLastAnd.Right := expression;
  end;
  Result := Self;
end; { TGpSQLBuilderExpression.&Or }

function TGpSQLBuilderExpression.GetAsString: string;
begin
  Result := CreateSQLSerializer(Expression).AsString;
end; { TGpSQLBuilderExpression.GetAsString }

function TGpSQLBuilderExpression.GetExpression: IGpSQLExpression;
begin
  Result := FExpression;
end; { TGpSQLBuilderExpression.GetExpression }

{ TGpSQLBuilder }

constructor TGpSQLBuilder.Create;
begin
  inherited;
  FAST := CreateSQLAST;
end; { TGpSQLBuilder.Create }

function TGpSQLBuilder.All: IGpSQLBuilder;
begin
  Result := Column('*');
end; { TGpSQLBuilder.All }

function TGpSQLBuilder.&And(const expression: array of const): IGpSQLBuilder;
begin
  Result := &And(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&And(const expression: string): IGpSQLBuilder;
begin
  FActiveExpr.&And(expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&And(const expression: IGpSQLBuilderExpression): IGpSQLBuilder;
begin
  FActiveExpr.&And(expression.Expression);
  Result := Self;
end; { TGpSQLBuilder.&And }

function TGpSQLBuilder.&As(const alias: string): IGpSQLBuilder;
begin
  AssertSection([secSelect, secDelete, secJoin]);
  AssertHaveName;

  FASTName.Alias := alias;
  Result := Self;
end; { TGpSQLBuilder.&As }

procedure TGpSQLBuilder.AssertHaveName;
begin
  if not assigned(FASTName) then
    raise Exception.Create('TGpSQLBuilder: Curernt name is not set');
end; { TGpSQLBuilder.AssertHaveName }

procedure TGpSQLBuilder.AssertSection(sections: TGpSQLSections);
begin
  if not (FActiveSection in sections) then
    raise Exception.Create('TGpSQLBuilder: Not supported in this section');
end; { TGpSQLBuilder.AssertSection }

function TGpSQLBuilder.AutoQuote(const s: string): string;
begin
  if (s <> '') and (s[1] = '''') and (s[Length(s)] = '''') then
    Result := s
  else
    Result := '''' + s + '''';
end; { TGpSQLBuilder.AutoQuote }

function TGpSQLBuilder.&Case(const expression: string = ''): IGpSQLBuilderCase;
begin
  Result := TGpSQLBuilderCase.Create(expression);
end; { TGpSQLBuilder.&Case }

function TGpSQLBuilder.&Case(const expression: array of const): IGpSQLBuilderCase;
begin
  Result := &Case(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&Case }

function TGpSQLBuilder.&Case(const expression: IGpSQLBuilderExpression):
  IGpSQLBuilderCase;
begin
  Result := TGpSQLBuilderCase.Create('');
  Result.&And(expression);
end; { TGpSQLBuilder }

function TGpSQLBuilder.Clear: IGpSQLBuilder;
begin
  FASTSection.Clear;
  Result := Self;
end; { TGpSQLBuilder.Clear }

function TGpSQLBuilder.ClearAll: IGpSQLBuilder;
begin
  FAST.Clear;
  Result := Self;
end; { TGpSQLBuilder.ClearAll }

function TGpSQLBuilder.Column(const colName: string): IGpSQLBuilder;
begin
  if assigned(FASTColumns) then begin
    FASTName := FASTColumns.Add;
    FASTName.Name := colName;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.',
      [FASTSection.Name]);
  Result := Self;
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const dbName, colName: string): IGpSQLBuilder;
begin
  Result := Column(dbName + '.' + colName);
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const colName: array of const): IGpSQLBuilder;
begin
  Result := Column(SqlParamsToStr(colName));
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.Column(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder;
begin
  if assigned(FASTColumns) then begin
    FASTName := FASTColumns.Add;
    FASTName.&Case := caseExpr.&Case;
  end
  else
    raise Exception.CreateFmt('Current section [%s] does not support COLUMN.',
      [FASTSection.Name]);
  Result := Self;
end; { TGpSQLBuilder.Column }

function TGpSQLBuilder.CreateJoin(joinType: TGpSQLJoinType; const dbName: string):
  IGpSQLBuilder;
var
  join: IGpSQLJoin;
begin
  FActiveSection := secJoin;
  join := FAST.Joins.Add;
  join.JoinType := joinType;
  FASTName := join.JoinedTable;
  FASTName.Name := dbName;
  FASTSection := join;
  FASTColumns := nil;
  FActiveExpr := TGpSQLBuilderExpression.Create(join.Condition);
  Result := Self;
end; { TGpSQLBuilder.CreateJoin }

function TGpSQLBuilder.Delete: IGpSQLBuilder;
begin
  SelectSection(secDelete);
  Result := Self;
end; { TGpSQLBuilder.Delete }

function TGpSQLBuilder.Desc: IGpSQLBuilder;
begin
  AssertSection([secOrderBy]);
  Assert(FASTColumns.Count > 0, 'TGpSQLBuilder.Desc: No columns set up yet');
  (FASTColumns[FASTColumns.Count - 1] as IGpSQLOrderByColumn).Direction := dirDescending;
  Result := Self;
end; { TGpSQLBuilder.Desc }

function TGpSQLBuilder.Distinct: IGpSQLBuilder;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqDistinct;
  Result := Self;
end; { TGpSQLBuilder.Distinct }

function TGpSQLBuilder.Expression(const term: string): IGpSQLBuilderExpression;
begin
  Result := TGpSQLBuilderExpression.Create(term);
end; { TGpSQLBuilder.Expression }

function TGpSQLBuilder.Expression(const term: array of const): IGpSQLBuilderExpression;
begin
  Result := Expression(SqlParamsToStr(term));
end; { TGpSQLBuilder.Expression }

function TGpSQLBuilder.First(num: integer): IGpSQLBuilder;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqFirst;
  qual.Value := num;
  Result := Self;
end; { TGpSQLBuilder.First }

function TGpSQLBuilder.From(const dbName: string): IGpSQLBuilder;
begin
  AssertSection([secSelect, secDelete]);
  FASTName := FTableNames.Add;
  FASTName.Name := dbName;
  Result := Self;
end; { TGpSQLBuilder.From }

function TGpSQLBuilder.FullJoin(const dbName: string): IGpSQLBuilder;
begin
  Result := CreateJoin(jtFull, dbName);
end; { TGpSQLBuilder.FullJoin }

function TGpSQLBuilder.GetAsString: string;
begin
  Result := CreateSQLSerializer(AST).AsString;
end; { TGpSQLBuilder.GetAsString }

function TGpSQLBuilder.GetAST: IGpSQLAST;
begin
  Result := FAST;
end; { TGpSQLBuilder.GetAST }

function TGpSQLBuilder.GroupBy(const colName: string): IGpSQLBuilder;
begin
  SelectSection(secGroupBy);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.GroupBy }

function TGpSQLBuilder.Having(const expression: string): IGpSQLBuilder;
begin
  SelectSection(secHaving);
  if expression = '' then
    Result := Self
  else
    Result := &And(expression);
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.Having(const expression: array of const): IGpSQLBuilder;
begin
  Result := Having(SqlParamsToStr(expression));
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.Having(const expression: IGpSQLBuilderExpression): IGpSQLBuilder;
begin
  SelectSection(secHaving);
  Result := &And(expression);
end; { TGpSQLBuilder.Having }

function TGpSQLBuilder.InnerJoin(const dbName: string): IGpSQLBuilder;
begin
  Result := CreateJoin(jtInner, dbName);
end; { TGpSQLBuilder.InnerJoin }

function TGpSQLBuilder.Insert: IGpSQLBuilder;
begin
  SelectSection(secInsert);
  Result := Self;
end; { TGpSQLBuilder.Insert }

function TGpSQLBuilder.InternalSet(const colName, colValue: string): IGpSQLBuilder;
var
  pair: IGpSQLNameValue;
begin
  AssertSection([secInsert, secUpdate]);
  pair := FActiveValues.Add;
  pair.Name := colName;
  pair.Value := colValue;
  Result := Self;
end; { TGpSQLBuilder.InternalSet }

function TGpSQLBuilder.Into(const tableName: string): IGpSQLBuilder;
begin
  AssertSection([secInsert]);
  (FASTSection as IGpSQLInsert).TableName := tableName;
  Result := Self;
end; { TGpSQLBuilder.Into }

function TGpSQLBuilder.IsEmpty: boolean;
begin
  Result := FASTSection.IsEmpty;
end; { TGpSQLBuilder.IsEmpty }

function TGpSQLBuilder.LeftJoin(const dbName: string): IGpSQLBuilder;
begin
  Result := CreateJoin(jtLeft, dbName);
end; { TGpSQLBuilder.LeftJoin }

function TGpSQLBuilder.&On(const expression: string): IGpSQLBuilder;
begin
  Result := &And(expression);
end; { TGpSQLBuilder.&On }

function TGpSQLBuilder.&On(const expression: array of const): IGpSQLBuilder;
begin
  Result := &On(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&On }

function TGpSQLBuilder.OrderBy(const colName: string): IGpSQLBuilder;
begin
  SelectSection(secOrderBy);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.OrderBy }

function TGpSQLBuilder.OrderBy(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder;
begin
  SelectSection(secOrderBy);
  Result := Column(caseExpr);
end; { TGpSQLBuilder.OrderBy }

function TGpSQLBuilder.RightJoin(const dbName: string): IGpSQLBuilder;
begin
  Result := CreateJoin(jtRight, dbName);
end; { TGpSQLBuilder.RightJoin }

function TGpSQLBuilder.&Or(const expression: array of const): IGpSQLBuilder;
begin
  Result := &Or(SqlParamsToStr(expression));
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.&Or(const expression: string): IGpSQLBuilder;
begin
  FActiveExpr.&Or(expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.&Or(const expression: IGpSQLBuilderExpression): IGpSQLBuilder;
begin
  FActiveExpr.&Or(expression.Expression);
  Result := Self;
end; { TGpSQLBuilder.&Or }

function TGpSQLBuilder.Select(const colName: string): IGpSQLBuilder;
begin
  SelectSection(secSelect);
  if colName = '' then
    Result := Self
  else
    Result := Column(colName);
end; { TGpSQLBuilder.Select }

function TGpSQLBuilder.Select(const caseExpr: IGpSQLBuilderCase): IGpSQLBuilder;
begin
  SelectSection(secSelect);
  Result := Column(caseExpr);
end; { TGpSQLBuilder.Select }

procedure TGpSQLBuilder.SelectSection(section: TGpSQLSection);
begin
  case section of
    secSelect:
      begin
        FASTSection   := FAST.Select;
        FASTColumns   := FAST.Select.Columns;
        FActiveExpr   := nil;
        FTableNames   := FAST.Select.TableNames;
        FActiveValues := nil;
      end;
    secDelete:
      begin
        FASTSection   := FAST.Delete;
        FASTColumns   := nil;
        FActiveExpr   := nil;
        FTableNames   := FAST.Delete.TableNames;
        FActiveValues := nil;
      end;
    secInsert:
      begin
        FASTSection   := FAST.Insert;
        FASTColumns   := nil;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := FAST.Insert.Values;
      end;
    secUpdate:
      begin
        FASTSection   := FAST.Update;
        FASTColumns   := nil;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
        FActiveValues := FAST.Update.Values;
      end;
    secWhere:
      begin
        FASTSection   := FAST.Where;
        FASTColumns   := nil;
        FActiveExpr   := TGpSQLBuilderExpression.Create(FAST.Where.Expression);
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secGroupBy:
      begin
        FASTSection   := FAST.GroupBy;
        FASTColumns   := FAST.GroupBy.Columns;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secHaving:
      begin
        FASTSection   := FAST.Having;
        FASTColumns   := nil;
        FActiveExpr   := TGpSQLBuilderExpression.Create(FAST.Having.Expression);
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    secOrderBy:
      begin
        FASTSection   := FAST.OrderBy;
        FASTColumns   := FAST.OrderBy.Columns;
        FActiveExpr   := nil;
        FTableNames   := nil;
        FActiveValues := nil;
      end;
    else
      raise Exception.Create('TGpSQLBuilder.SelectSection: Unknown section');
  end;
  FActiveSection := section;
end; { TGpSQLBuilder.SelectSection }

function TGpSQLBuilder.&Set(const colName, colValue: string): IGpSQLBuilder;
begin
  Result := InternalSet(colName, AutoQuote(colValue));
end; { TGpSQLBuilder }

function TGpSQLBuilder.&Set(const colName: string; const colValue: array of const):
  IGpSQLBuilder;
begin
  Result := InternalSet(colName, SqlParamsToStr(colValue));
end; { TGpSQLBuilder }

function TGpSQLBuilder.Skip(num: integer): IGpSQLBuilder;
var
  qual: IGpSQLSelectQualifier;
begin
  AssertSection([secSelect]);
  qual := (FASTSection as IGpSQLSelect).Qualifiers.Add;
  qual.Qualifier := sqSkip;
  qual.Value := num;
  Result := Self;
end; { TGpSQLBuilder.Skip }

function TGpSQLBuilder.Update(const tableName: string): IGpSQLBuilder;
begin
  SelectSection(secUpdate);
  (FASTSection as IGpSQLUpdate).TableName := tableName;
  Result := Self;
end; { TGpSQLBuilder.Update }

function TGpSQLBuilder.Where(const expression: string): IGpSQLBuilder;
begin
  SelectSection(secWhere);
  if expression = '' then
    Result := Self
  else
    Result := &And(expression);
end; { TGpSQLBuilder.Where }

function TGpSQLBuilder.Where(const expression: array of const): IGpSQLBuilder;
begin
  Result := Where(SqlParamsToStr(expression));
end; { TGpSQLBuilder.Where }

function TGpSQLBuilder.Where(const expression: IGpSQLBuilderExpression): IGpSQLBuilder;
begin
  SelectSection(secWhere);
  Result := &And(expression);
end; { TGpSQLBuilder.Where }

{ SQL }

class function SQL.Count(const s: string): string;
begin
  Result := 'Count(' + s + ')';
end; { SQL.Count }

class function SQL.Count(const s: IGpSQLBuilderExpression): string;
begin
  Result := Count(s.AsString);
end; { SQL.Count }

class function SQL.Count(const s: IGpSQLBuilder): string;
begin
  Result := Count(s.AsString);
end; { SQL.Count }

class function SQL.Exists(const s: string): string;
begin
  Result := 'exists (' + s + ')';
end; { SQL.Exists }

class function SQL.Exists(const s: IGpSQLBuilder): string;
begin
  Result := Exists(s.AsString);
end; { SQL.Exists }

class function SQL.Exists(const s: IGpSQLBuilderExpression): string;
begin
  Result := Exists(s.AsString);
end; { SQL.Exists }

class function SQL.Lower(const s: string): string;
begin
  Result := 'Lower(' + s + ')';
end; { SQL.Lower }

class function SQL.Lower(const s: IGpSQLBuilder): string;
begin
  Result := Lower(s.AsString);
end; { SQL.Lower }

class function SQL.Lower(const s: IGpSQLBuilderExpression): string;
begin
  Result := Lower(s.AsString);
end; { SQL.Lower }

class function SQL.Max(const s: string): string;
begin
  Result := 'Max(' + s + ')';
end; { SQL.Max }

class function SQL.Max(const s: IGpSQLBuilder): string;
begin
  Result := Max(s.AsString);
end; { SQL.Max }

class function SQL.Max(const s: IGpSQLBuilderExpression): string;
begin
  Result := Max(s.AsString);
end; { SQL.Max }

class function SQL.Min(const s: string): string;
begin
  Result := 'Min(' + s + ')';
end; { SQL.Min }

class function SQL.Min(const s: IGpSQLBuilderExpression): string;
begin
  Result := Min(s.AsString);
end; { SQL.Min }

class function SQL.Min(const s: IGpSQLBuilder): string;
begin
  Result := Min(s.AsString);
end; { SQL.Min }

class function SQL.Upper(const s: string): string;
begin
  Result := 'Upper(' + s + ')';
end; { SQL.Upper }

class function SQL.Upper(const s: IGpSQLBuilder): string;
begin
  Result := Upper(s.AsString);
end; { SQL.Upper }

class function SQL.Upper(const s: IGpSQLBuilderExpression): string;
begin
  Result := Upper(s.AsString);
end; { SQL.Upper }

class function SQL.Q(const s: string): string;
begin
  Result := '''' + s + '''';
end; { SQL.Q }

class function SQL.Q(const s: IGpSQLBuilder): string;
begin
  Result := Q(s.AsString);
end; { SQL.Q }

class function SQL.Q(const s: IGpSQLBuilderExpression): string;
begin
  Result := Q(s.AsString);
end; { SQL.Q }

end.
