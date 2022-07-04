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
{   The Original Code is xqYacc.pas                                        }
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

unit xqYacc;

{$I XQ_FLAG.INC}
{$R xqyacc.res}
interface

uses
   SysUtils, Classes, Windows, Dialogs, QLexLib, QYaccLib, xqBase, xquery,
   DB, xqmiscel, CnvStrUtils, xqJoins
{$IFDEF LEVEL3}
   , DBTables
{$ENDIF}
   ;


type
  TxqParser = class( TCustomParser )
  private
    FAnalizer        : TSqlAnalizer;
    FCurrAnalizer    : TSqlAnalizer;    { only a pointer. never created }
    FForLength       : String;
    FEscapeChar      : String;
    FTrimPosition    : Integer;
    FExtractField    : Integer;
    FCastType        : Integer;
    FCastLen         : Integer;
    FIsDistinctAggr  : Boolean;
    FAsAlias         : String;
    FNumInserts      : Integer;   { used in INSERTO INTO...}
    FAggregateList   : TAggregateList;
    FColSubqueryList : TList;     { the list of subqueries for the current select }
    FIsFullPath      : Boolean;
    FTempJoinOnItem  : TJoinOnItem;
    FJoinInWhereTables: TStringList;
    FJoinInWhereFields: TStringList;
    FTableName       : string;
    FFieldName       : string;
    Fwhenlist        : string;
    FIsNotInList     : Boolean;
    FInPredicateList : TStringList;
    { For CREATE TABLE }
    FNumTables, FFieldType, FScale, FPrecision, FSize, FBlobType: Integer;
    FtempTopNInSelect: Integer;
    FtempTopNInGroupBy: Integer;

    procedure SetFieldParams(AFieldType, AScale, APrecision, ASize, ABlobType: Integer);
    procedure SetTableName(const TableName: String);
    procedure SetAlterTableName(const TableName: String);
    procedure AddCreateField(const FieldName: String);
    procedure AddAlterField(const FieldName: String; DropField: Boolean);
    procedure AddPrimaryKeyField(const FieldName: String);
    function CurrentCreateTable: TCreateTableItem;
    function CurrentAlterTable: TCreateTableItem;
    function GetCurrentAnalizer: TSqlAnalizer;
    function CurrentInsertItem : TInsertItem;
    function AddSubqueryInSelect : String;
    procedure CreateNewSubquery;
    function GetRootAnalizer: TSQLAnalizer;
    function GetString(const s: string): string;
    function CreateInListExpression( const Expr : String ): String;
    Procedure SetJoinTestTbls( Const Test1, Test2: string);
  public

    constructor Create(Analizer: TSqlAnalizer);
    destructor Destroy; override;

    function yyparse : integer; override;
    procedure yyerror(const msg : string);

    { specials }
    procedure AddColumn(const AColText : String; IsTransformColumn: Boolean);
    function AddAggregate( pAggregate: TAggregateKind;
                           const pAggregateStr: String ): String;
    procedure AddGroupBy(const ColName: String);
    procedure AddOrderBy(const ColName: String; Descending: Boolean);
    procedure AddTable(const TableName, Alias: String; IsFullPath: Boolean);
    procedure AddJoin();
    procedure AddJoinCandidate(const LeftExpr, RightExpr: String);
    procedure AddHavingColumn( const ColText : String );
    procedure AddUpdateColumn(const ColumnName, ColumnExpr: String);
    procedure AddWhereOptimize(const AFieldName, ARangeStart,
       ARangeEnd : String; ARelOperator : TRelationalOperator);

    property Analizer: TSqlAnalizer read fAnalizer write fAnalizer;
    property AsAlias: String read fAsAlias write fAsAlias;
    property CurrentAnalizer: TSqlAnalizer read GetCurrentAnalizer;
    property TableName: string read fTableName;
    property FieldName: string read fFieldName;
  end;

const _IDENTIFIER = 257;
const _UINTEGER = 258;
const _NUMERIC = 259;
const _STRING = 260;
const _COMA = 261;
const _LPAREN = 262;
const _RPAREN = 263;
const _LSQUARE = 264;
const _RSQUARE = 265;
const _PERIOD = 266;
const _SEMICOLON = 267;
const _COLON = 268;
const RW_OR = 269;
const RW_AND = 270;
const _EQ = 271;
const _NEQ = 272;
const _GT = 273;
const _LT = 274;
const _GE = 275;
const _LE = 276;
const RW_BETWEEN = 277;
const RW_IN = 278;
const RW_LIKE = 279;
const _PLUS = 280;
const _SUB = 281;
const _DIV = 282;
const _MULT = 283;
const RW_MOD = 284;
const RW_IDIV = 285;
const RW_SHL = 286;
const RW_SHR = 287;
const UMINUS = 288;
const _EXP = 289;
const RW_NOT = 290;
const _ILLEGAL = 291;
const _COMMENT = 292;
const _BLANK = 293;
const _TAB = 294;
const _NEWLINE = 295;
const RW_TRUE = 296;
const RW_FALSE = 297;
const RW_SELECT = 298;
const RW_DISTINCT = 299;
const RW_FROM = 300;
const RW_WHERE = 301;
const RW_ORDER = 302;
const RW_BY = 303;
const RW_ASC = 304;
const RW_DESC = 305;
const RW_AS = 306;
const RW_INNER = 307;
const RW_OUTER = 308;
const RW_FULL = 309;
const RW_JOIN = 310;
const RW_ON = 311;
const RW_GROUP = 312;
const RW_HAVING = 313;
const RW_ANY = 314;
const RW_ALL = 315;
const RW_SUM = 316;
const RW_AVG = 317;
const RW_COUNT = 318;
const RW_MIN = 319;
const RW_MAX = 320;
const RW_STDEV = 321;
const RW_LEFT = 322;
const RW_RIGHT = 323;
const RW_LEADING = 324;
const RW_TRAILING = 325;
const RW_BOTH = 326;
const RW_TRIM = 327;
const RW_EXTRACT = 328;
const RW_YEAR = 329;
const RW_MONTH = 330;
const RW_DAY = 331;
const RW_HOUR = 332;
const RW_MINUTE = 333;
const RW_SECOND = 334;
const RW_FOR = 335;
const RW_SUBSTRING = 336;
const RW_DELETE = 337;
const RW_UPDATE = 338;
const RW_INSERT = 339;
const RW_INTO = 340;
const RW_VALUES = 341;
const RW_SET = 342;
const RW_CAST = 343;
const RW_CHAR = 344;
const RW_INTEGER = 345;
const RW_BOOLEAN = 346;
const RW_DATE = 347;
const RW_TIME = 348;
const RW_DATETIME = 349;
const RW_FLOAT = 350;
const RW_ESCAPE = 351;
const RW_CREATE = 352;
const RW_TABLE = 353;
const RW_SMALLINT = 354;
const RW_MONEY = 355;
const RW_AUTOINC = 356;
const RW_PRIMARY = 357;
const RW_KEY = 358;
const RW_BLOB = 359;
const RW_INDEX = 360;
const RW_UNIQUE = 361;
const RW_DROP = 362;
const RW_TRANSFORM = 363;
const RW_PIVOT = 364;
const RW_UNION = 365;
const RW_WITH = 366;
const RW_IS = 367;
const RW_NULL = 368;
const RW_ALTER = 369;
const RW_COLUMN = 370;
const RW_ADD = 371;
const RW_APPEND = 372;
const RW_CASE = 373;
const RW_WHEN = 374;
const RW_THEN = 375;
const RW_ELSE = 376;
const RW_END = 377;
const RW_PACK = 378;
const RW_ZAP = 379;
const RW_REINDEX = 380;
const RW_RANGE = 381;
const RW_USING = 382;
const RW_FIELDS = 383;
const RW_TO = 384;
const RW_TOP = 385;

type YYSType = record
               yystring : string
               end(*YYSType*);

// global definitions:

var yylval : YYSType;

implementation

uses
   xqLex, xqConsts;

(*----------------------------------------------------------------------------*)
procedure TxqParser.yyerror(const msg : string);
begin
   yyerrorMsg := msg;
   { yyerrorMsg := IntToStr(yyLexer.yylineno) +  ': ' + msg + ' at or before '+ yyLexer.yytext + '. ' +
   Format('Line %d, Column %d',[yyLexer.yylineno,yyLexer.yycolno]);
   if Analizer.xQuery.ShowErrorMessage then
      ShowMessage( yyerrorMsg ); }
end;

constructor TxqParser.Create(Analizer: TSqlAnalizer);
begin
   inherited Create;
   fAnalizer:= Analizer;
   fInPredicateList := TStringList.Create;
   fTempJoinOnItem:= TJoinOnItem.Create(nil);
   fJoinInWhereTables:= TStringList.create;
   fJoinInWhereFields:= TStringList.create;
end;

destructor TxqParser.Destroy;
begin
   if Assigned(fAggregateList) then
      fAggregateList.Free;
   if Assigned(fTempJoinOnItem) then
     FreeObject(fTempJoinOnItem);
   fJoinInWhereTables.Free;
   fJoinInWhereFields.Free;
   fInPredicateList.Free;
   inherited Destroy;
end;

Function TxqParser.GetString( const s: string ): string;
begin
  Result:= Copy( s, 2, Length(s) - 2);
end;

procedure TxqParser.CreateNewSubquery;
var
  TmpAnalizer : TSqlAnalizer;
begin
  TmpAnalizer := GetCurrentAnalizer;
  fCurrAnalizer := TSqlAnalizer.Create( TmpAnalizer, fAnalizer.xQuery );
  TmpAnalizer.SubqueryList.Add( fCurrAnalizer );
end;

function TxqParser.GetRootAnalizer: TSQLAnalizer;
begin
  Result := GetCurrentAnalizer;
  while Result.ParentAnalizer <> nil do Result := Result.ParentAnalizer;
end;

function TxqParser.GetCurrentAnalizer: TSqlAnalizer;
begin
   if fCurrAnalizer= nil then fCurrAnalizer := Self.fAnalizer;
   Result := fCurrAnalizer;
end;

procedure TxqParser.AddColumn(const AColText : String; IsTransformColumn: Boolean);
var
  Column : TColumnItem;
  TmpAnalizer: TSqlAnalizer;

  function StripFields(const s: String): String;
  var
     p, i, j: Integer;
     Found: Boolean;
  begin
     Result:= s;
     p:= Pos('\f"', Result);
     while p > 0 do
     begin
       j:= p + 3;
       i:= j;
       Found:= True;
       while j <= Length(Result) do
       begin
         if Result[j] = '"' then
         begin
            Found:= True;
            Break;
         end;
         Inc(j);
       end;
       if Not Found then Exit;    { fatal error }
       if j <= Length(Result) then
         Result:= Copy(Result, 1, p - 1) + Copy(Result, i, j - i) +
           Copy(Result, j + 1, Length(Result));

       p:= Pos('\f"', Result);
     end;
  end;

begin
  if IsTransformColumn then
     Column := CurrentAnalizer.TransformColumnList.Add
  else
     Column := CurrentAnalizer.ColumnList.Add;
  with Column do
  begin
     ColumnExpr:= AColText;
     { this mean that aggregate columns are embedded in ColumnExpr}
     if Assigned(Self.fAggregateList) then
     begin
       Column.AggregateList.Free;    { free previous aggregate list}
       Column.AggregateList := Self.fAggregateList; { assign the current list}
       Self.fAggregateList:= nil;    { define as nil the current }
     end;
     if Assigned(Self.fColSubqueryList) then
     begin
       TmpAnalizer := GetRootAnalizer;
       if TmpAnalizer = CurrentAnalizer then begin
          Column.SubqueryList.Free;
          Column.SubqueryList := Self.fColSubqueryList;
          Self.fColSubqueryList:= nil;
          if Length(Self.fAsAlias)= 0 then
             Self.fAsAlias := 'Subquery';
       end;
     end;
     CastType  := Self.fCastType;
     CastLen   := IMax(1, Self.fCastLen);      // only used for strings
     if Length(self.fAsAlias) > 0 then
     begin
       AsAlias:= self.fAsAlias;
       IsAsExplicit:= True;
     end else
     begin
       IsAsExplicit := False;
       AsAlias := StripFields(AColText);
       if AggregateList.Count > 0  then
         case AggregateList[0].Aggregate of
            akSUM: AsAlias := SAggrSUM + StripFields(AggregateList[0].AggregateStr);
            akAVG: AsAlias := SAggrAVG + StripFields(AggregateList[0].AggregateStr);
            akSTDEV: AsAlias := SAggrSTDEV + StripFields(AggregateList[0].AggregateStr);
            akMIN: AsAlias := SAggrMIN + StripFields(AggregateList[0].AggregateStr);
            akMAX: AsAlias := SAggrMAX + StripFields(AggregateList[0].AggregateStr);
            akCOUNT: AsAlias := SAggrCOUNT;
         end;
     end;
  end;

  fAsAlias  := '';
  fCastType := 0;
  fCastLen  := 0;
end;

{ This function will return an aggregate encoded with something like :
  (Aggregate 1) }
function TxqParser.AddAggregate( pAggregate: TAggregateKind;
  const pAggregateStr: String ) : String;
begin
  if fAggregateList = nil then
     fAggregateList := TAggregateList.Create;
  with fAggregateList.Add do
  begin
     AggregateStr := pAggregateStr;
     Aggregate    := pAggregate;
     IsDistinctAg := Self.fIsDistinctAggr;
  end;
  Result := Format('{Aggregate %d}', [fAggregateList.Count - 1]);

  Self.fIsDistinctAggr := False;
end;

{ This function will return a Subquery encoded with something like :
  (Subquery 1) }
function TxqParser.AddSubqueryInSelect : String;
var
  MainAnalizer, Analizer: TSqlAnalizer;
begin
  if fColSubqueryList = nil then
     fColSubqueryList := TList.Create;
  { Now add the Subquery.
    Important: No nested subqueries are allowed in this release. Example
    SELECT custno, (SELECT custno FROM customer WHERE
      custno >= ALL(SELECT custno FROM customer WHERE custno BETWEEN 1000 AND 2000)) FROM customer; )
    But this is allowed:
     SELECT custno, (SELECT SUM(amountpaid) FROM customer WHERE custno BETWEEN 1000 AND 2000) /
        (SELECT Count(*) FROM customer WHERE custno BETWEEN 2000 AND 3000) FROM customer;
  }
  { do to the following trick, nested subqueries in the SELECT columns are not allowed }
  MainAnalizer := GetRootAnalizer;
  Analizer := TSqlAnalizer( MainAnalizer.SubqueryList[0] );
  MainAnalizer.SubqueryList.Clear;
  fColSubqueryList.Add( Analizer );
  Result := Format( '{Subquery %d}',[fColSubqueryList.Count - 1] );
end;

procedure TxqParser.AddGroupBy(const ColName: String);
var
  GroupBy: TOrderByItem;
  Index, Code: integer;
begin
  Val(ColName, Index, Code);
  GroupBy := CurrentAnalizer.GroupByList.Add;
  if Code = 0 then
  begin
     GroupBy.Alias := '';
     GroupBy.ColIndex := Index - 1;
  end else
  begin
     GroupBy.Alias := ColName;
     GroupBy.ColIndex:= -1;   { means: not defined by number }
  end;
end;

procedure TxqParser.AddOrderBy(const ColName: String; Descending: Boolean);
var
  OrderBy     : TOrderByItem;
  Index, Code : integer;
begin
  Val(ColName, Index, Code);
  OrderBy:= CurrentAnalizer.OrderByList.Add;
  if Code = 0 then
  begin
     OrderBy.Alias := '';
     OrderBy.ColIndex := Index - 1;
  end else
  begin
     OrderBy.Alias := ColName;
     { means: not defined by number and must be solved in checkintegrity }
     OrderBy.ColIndex:= -1;
  end;
  OrderBy.Desc:= Descending;
end;

Procedure TxqParser.AddTable(const TableName, Alias: String; IsFullPath: Boolean);
var
   Table: TTableItem;
   s: String;
begin
   Table:= CurrentAnalizer.TableList.Add;
   if IsFullPath then
   begin
      s := Copy(TableName,2,Length(TableName)-2);
      Table.TableName := s;
   end else
      Table.TableName := TableName;
   Table.IsFullPath := IsFullPath;
   if Length(Alias) > 0 then
      Table.Alias := Alias
   else
   begin
      if IsFullPath then
         Table.Alias := ChangeFileExt(ExtractFileName(s),'')
      else
         Table.Alias := TableName;
   end;
end;

Procedure TxqParser.SetJoinTestTbls( Const Test1, Test2: string );
Begin
  If Length( fTempJoinOnItem.LeftRefTest ) > 0 Then Exit;
  fTempJoinOnItem.LeftRefTest:= Test1;
  fTempJoinOnItem.RightRefTest:= Test2;
End;

Procedure TxqParser.AddJoin();
Begin
   With CurrentAnalizer.JoinList.Add Do
     Assign( FTempJoinOnItem );
   { initializes the item used }
   With fTempJoinOnItem Do
   Begin
     JoinAction:= jkLeftInnerJoin;
     JoinExpression:= '';
     FTempJoinOnItem.LeftRefTest:= '';
     FTempJoinOnItem.RightRefTest:= '';
     // gis product
     FTempJoinOnItem.GraphicJoin := False;
   End;
End;

procedure TxqParser.AddJoinCandidate(const LeftExpr, RightExpr: String);
begin
  CurrentAnalizer.LJoinCandidateList.Add( LeftExpr );
  CurrentAnalizer.RJoinCandidateList.Add( RightExpr );
end;

procedure TxqParser.AddHavingColumn( const ColText : String );
var
   Column: TColumnItem;
begin
   Column := CurrentAnalizer.ColumnList.Add;
   with Column do
   begin
      ColumnExpr := ColText;
      if Assigned(Self.fAggregateList) then
      begin
        AggregateList.Free;    { free previous aggregate list}
        AggregateList := Self.fAggregateList; { assign the current list}
        Self.fAggregateList:= nil;    { define as nil the current }
      end;
      { mark also as a temporary column that will be deleted after result set is
        generated }
      IsTemporaryCol := True;
   end;
end;

procedure TxqParser.AddUpdateColumn(const ColumnName, ColumnExpr: String);
var
   UpdateItem: TUpdateItem;
begin
   UpdateItem := CurrentAnalizer.UpdateColumnList.Add;
   with UpdateItem do
   begin
      ColName := ColumnName;
      ColExpr := ColumnExpr;
   end;

end;

procedure TxqParser.AddWhereOptimize( const AFieldName, ARangeStart,
   ARangeEnd: String; ARelOperator: TRelationalOperator );
var
   WhereOptimize : TWhereOptimizeItem;
   N             : Integer;

   function DeleteStringDelim(const s: String): String;
   begin
      Result := s;
      if Length(Result) > 1 then
      begin
         if CharInSet(Result[1], xqbase.SQuote) and CharInSet(Result[Length(Result)], xqbase.SQuote) then
         Result:= Copy(Result, 2, Length(Result) - 2);
      end;
   end;

begin
   if Not (yyLexer as TxqLexer).IsWhereActive then Exit;
   N := CurrentAnalizer.WhereOptimizeList.Count - 1;
   if ((ARelOperator in [ropLE, ropLT]) or (ARelOperator in [ropGE, ropGT]) ) and (N > 0) then
   begin

      { I will check if the previous command was something like (CustNo >= 1000)
        and if so, and this is something like (CustNo <= 3000) then
       I will add to the previous optimize and will generate a ropBETWEEN range   }

      WhereOptimize:= CurrentAnalizer.WhereOptimizeList[N - 1];
      if ( ((ARelOperator in [ropLE, ropLT]) and (WhereOptimize.RelOperator in [ropGE, ropGT])) or
         ((ARelOperator in [ropGE, ropGT]) and (WhereOptimize.RelOperator in [ropLE, ropLT])) ) and
         (AnsiCompareText(WhereOptimize.FieldNames, AFieldName) = 0) then
      begin
         WhereOptimize.RangeEnd    := DeleteStringDelim( ARangeEnd );
         WhereOptimize.RelOperator := ropBETWEEN;
         Exit;
      end;
   end;

   WhereOptimize := CurrentAnalizer.WhereOptimizeList.Add;
   with WhereOptimize do
   begin
      FieldNames  := AFieldName;
      RangeStart  := DeleteStringDelim(ARangeStart);
      RangeEnd    := DeleteStringDelim(ARangeEnd);
      RelOperator := ARelOperator;
      CanOptimize := False;
   end;

end;

{ ALTER TABLE }
function TxqParser.CurrentAlterTable: TCreateTableItem;
begin
  if FNumTables > CurrentAnalizer.AlterTableList.Count - 1 then
     Result := CurrentAnalizer.AlterTableList.Add
  else
     Result:= CurrentAnalizer.AlterTableList[ FNumTables ];
end;

{ CREATE TABLE }
function TxqParser.CurrentCreateTable: TCreateTableItem;
begin
  if FNumTables > CurrentAnalizer.CreateTableList.Count - 1 then
     Result := CurrentAnalizer.CreateTableList.Add
  else
     Result:= CurrentAnalizer.CreateTableList[ FNumTables ];
end;

procedure TxqParser.SetTableName(const TableName: String);
begin
   CurrentCreateTable.TableName := TableName;
end;

procedure TxqParser.SetAlterTableName(const TableName: String);
begin
   CurrentAlterTable.TableName := TableName;
end;

procedure TxqParser.SetFieldParams(AFieldType, AScale, APrecision, ASize, ABlobType: Integer);
begin
  FFieldType := AFieldType;
  FScale     := AScale;
  FPrecision := APrecision;
  FSize      := ASize;
  FBlobType  := ABlobType;
end;

procedure TxqParser.AddAlterField(const FieldName: String; DropField: Boolean);
var
  I: Integer;
begin
  { check if field exists }
  with CurrentAlterTable do
     for I := 0 to FieldCount - 1 do
       if (AnsiCompareText(Fields[I].FieldName, FieldName) = 0)
         and (Fields[I].MustDrop=DropField) then
       begin
          yyerror(SDuplicateFieldName);
          yyabort;
          Exit;
       end;
  if (FFieldType = RW_BLOB) and not (FBlobType in [1..5]) then
  begin
    yyerror(SBlobFieldWrongType);
    yyabort;
  end;
  CurrentAlterTable.Fields.AddField(FieldName, FFieldType, FScale, FPrecision,
    FSize, FBlobType, DropField);
end;

procedure TxqParser.AddCreateField(const FieldName: String);
var
  I: Integer;
begin
  { check if field exists }
  with CurrentCreateTable do
     for I := 0 to FieldCount - 1 do
   if AnsiCompareText(Fields[I].FieldName, FieldName) = 0 then
   begin
      yyerror(SDuplicateFieldName);
      yyabort;
      Exit;
   end;
  if (FFieldType = RW_BLOB) and not (FBlobType in [1..5]) then
  begin
    yyerror(SBlobFieldWrongType);
    yyabort;
  end;
  CurrentCreateTable.Fields.AddField(FieldName,FFieldType, FScale, FPrecision,
    FSize, FBlobType, False);
end;

procedure TxqParser.AddPrimaryKeyField(const FieldName: String);
var
  I : Integer;
  Found : Boolean;
begin
  { check that the field exists in the list of field names }
  Found := False;
  with CurrentCreateTable do
     for I := 0 to FieldCount - 1 do
  if AnsiCompareText(Fields[I].FieldName, FieldName) = 0 then
  begin
     Found:= True;
     Break;
  end;
  if Not Found then
  begin
     yyerror(SFieldNameNotFound);
     yyabort;
  end;
  CurrentCreateTable.PrimaryKey.Add( FieldName );
end;

function TxqParser.CurrentInsertItem: TInsertItem;
begin
   if fNumInserts >= CurrentAnalizer.InsertList.Count then
      Result := CurrentAnalizer.InsertList.Add
   else
      Result := CurrentAnalizer.InsertList[fNumInserts];
end;

function TxqParser.CreateInListExpression( const Expr : String ) : String;
var
   I : Integer;
begin
   { This subroutine helps to change the syntax:
     custno IN (1000, 2000, 3000)
     to this :
     (custno = 1000) OR (custno = 2000) OR (custno = 3000) }
   Result := '';
   for i := 0 to fInPredicateList.Count - 1 do
   begin
      Result := Result + Format('(%s = %s)', [Expr, fInPredicateList[i]]);
      if i < fInPredicateList.Count - 1 then
        Result := Result + ' OR ';
   end;
   if fIsNotInList then
      Result := 'NOT ( ' + Result + ' )'
   else
      Result := '( ' + Result + ' )';
   fInPredicateList.Clear;
end;


// function yylex : Integer; forward;  // addition 1

function TXQParser.yyparse : Integer; // addition 2

var yystate, yysp, yyn : SmallInt;
    yys : array [1..yymaxdepth] of SmallInt;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         fAnalizer.Statement := ssSelect;
       end;
   2 : begin
         fAnalizer.Statement := ssSelect;
       end;
   3 : begin
         fAnalizer.Statement := ssUnion;
       end;
   4 : begin
         fAnalizer.Statement := ssUpdate;
       end;
   5 : begin
         fAnalizer.Statement := ssDelete;
       end;
   6 : begin
         fAnalizer.Statement := ssInsert;
       end;
   7 : begin
         fAnalizer.Statement := ssCreateTable;
       end;
   8 : begin
         fAnalizer.Statement := ssAlterTable;
       end;
   9 : begin
         fAnalizer.Statement := ssCreateIndex;
       end;
  10 : begin
         fAnalizer.Statement := ssDropTable;
       end;
  11 : begin
         fAnalizer.Statement := ssDropIndex;
       end;
  12 : begin
         fAnalizer.Statement := ssPacktable;
       end;
  13 : begin
         fAnalizer.Statement := ssZapTable;
       end;
  14 : begin
         fAnalizer.Statement := ssReindexTable;
       end;
  15 : begin
         yyval := yyv[yysp-7];
       end;
  16 : begin
         yyval := yyv[yysp-3];
       end;
  17 : begin
       end;
  18 : begin
         CurrentAnalizer.IntoTable:= yyv[yysp-2].yystring;
       end;
  19 : begin
         CurrentAnalizer.DoSelectAll := True;
         CurrentAnalizer.AddFieldIfNot('*');
       end;
  20 : begin
         yyval := yyv[yysp-1];
       end;
  21 : begin
         yyval := yyv[yysp-2];
       end;
  22 : begin
         CurrentAnalizer.IsDistinct:= True;
       end;
  23 : begin
       end;
  24 : begin
         CurrentAnalizer.TopNInSelect:= StrToInt( yyv[yysp-0].yystring ) ;
       end;
  25 : begin
       end;
  26 : begin
         CurrentAnalizer.TopNInGroupBy:= StrToInt( yyv[yysp-0].yystring ) ;
       end;
  27 : begin
         yyval := yyv[yysp-0];
       end;
  28 : begin
         yyval := yyv[yysp-2];
       end;
  29 : begin
         addColumn( yyv[yysp-1].yystring, false );
       end;
  30 : begin
         CurrentAnalizer.TableAllFields.Add( yyv[yysp-2].yystring );
         CurrentAnalizer.AddFieldIfNot( yyv[yysp-2].yystring + '.*' );

       end;
  31 : begin
         AddColumn( yyv[yysp-4].yystring, False );
       end;
  32 : begin
       end;
  33 : begin
         fAsAlias:= yyv[yysp-0].yystring;
       end;
  34 : begin
       end;
  35 : begin
         yyval := yyv[yysp-0];
       end;
  36 : begin
         yyval := yyv[yysp-0];
       end;
  37 : begin
         yyval := yyv[yysp-0];
       end;
  38 : begin
         yyval := yyv[yysp-0];
       end;
  39 : begin
         yyval := yyv[yysp-0];
       end;
  40 : begin
         yyval := yyv[yysp-0];
       end;
  41 : begin
         yyval := yyv[yysp-0];
       end;
  42 : begin
         yyval := yyv[yysp-0];
       end;
  43 : begin
         yyval.yystring := AddSubqueryInSelect;
       end;
  44 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' + '  + yyv[yysp-0].yystring;
       end;
  45 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' - '  + yyv[yysp-0].yystring;
       end;
  46 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' * '  + yyv[yysp-0].yystring;
       end;
  47 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' / '  + yyv[yysp-0].yystring;
       end;
  48 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' ^ '  + yyv[yysp-0].yystring;
       end;
  49 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' MOD '  + yyv[yysp-0].yystring;
       end;
  50 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' DIV '  + yyv[yysp-0].yystring;
       end;
  51 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' SHL '  + yyv[yysp-0].yystring;
       end;
  52 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' SHR '  + yyv[yysp-0].yystring;
       end;
  53 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' > '  + yyv[yysp-0].yystring;
       end;
  54 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' < '  + yyv[yysp-0].yystring;
       end;
  55 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' <> ' + yyv[yysp-0].yystring;
       end;
  56 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' >= ' + yyv[yysp-0].yystring;
       end;
  57 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' <= ' + yyv[yysp-0].yystring;
       end;
  58 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' = '  + yyv[yysp-0].yystring;
       end;
  59 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' AND ' + yyv[yysp-0].yystring;
       end;
  60 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' AND ' + yyv[yysp-0].yystring;
       end;
  61 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
  62 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
  63 : begin
         yyval.yystring := yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
  64 : begin
         yyval.yystring := AddAggregate(akSUM, yyv[yysp-1].yystring);
       end;
  65 : begin
         yyval.yystring := AddAggregate(akMIN, yyv[yysp-1].yystring);
       end;
  66 : begin
         yyval.yystring := AddAggregate(akMAX, yyv[yysp-1].yystring);
       end;
  67 : begin
         yyval.yystring := AddAggregate(akAVG, yyv[yysp-1].yystring);
       end;
  68 : begin
         yyval.yystring := AddAggregate(akSTDEV, yyv[yysp-1].yystring);
       end;
  69 : begin
         yyval.yystring := AddAggregate(akCOUNT, '0');
       end;
  70 : begin
         yyval.yystring := AddAggregate(akCOUNT, yyv[yysp-1].yystring);
       end;
  71 : begin
       end;
  72 : begin
         Self.fIsDistinctAggr:= True;
       end;
  73 : begin
         yyval.yystring := yyv[yysp-2].yystring + '()';
       end;
  74 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
  75 : begin
         yyval := yyv[yysp-0];
       end;
  76 : begin
         yyval := yyv[yysp-0];
       end;
  77 : begin
         yyval := yyv[yysp-0];
       end;
  78 : begin
         yyval.yystring:= yyv[yysp-2].yystring + ', ' + yyv[yysp-0].yystring;
       end;
  79 : begin

         yyval.yystring := Format('SQLTRIM(%s, %s, %d)',[yyv[yysp-3].yystring, yyv[yysp-1].yystring, fTrimPosition]);

       end;
  80 : begin
         fTrimPosition := 0;
       end;
  81 : begin
         fTrimPosition := 1;
       end;
  82 : begin
         fTrimPosition := 2;
       end;
  83 : begin
         case fExtractField of
         0: yyval.yystring := Format('YEAR(%s)',  [yyv[yysp-1].yystring]);
         1: yyval.yystring := Format('MONTH(%s)', [yyv[yysp-1].yystring]);
         2: yyval.yystring := Format('DAY(%s)',   [yyv[yysp-1].yystring]);
         3: yyval.yystring := Format('HOUR(%s)',  [yyv[yysp-1].yystring]);
         4: yyval.yystring := Format('MIN(%s)',   [yyv[yysp-1].yystring]);
         5: yyval.yystring := Format('SEC(%s)',   [yyv[yysp-1].yystring]);
         end;

       end;
  84 : begin
         fExtractField:= 0;
       end;
  85 : begin
         fExtractField:= 1;
       end;
  86 : begin
         fExtractField:= 2;
       end;
  87 : begin
         fExtractField:= 3;
       end;
  88 : begin
         fExtractField:= 4;
       end;
  89 : begin
         fExtractField:= 5;
       end;
  90 : begin
         if Length(fForLength) > 0 then
         yyval.yystring := Format('COPY(%s,%s,%s)',[yyv[yysp-4].yystring,yyv[yysp-2].yystring,fForLength])
         else
         yyval.yystring := Format('COPY(%s,%s,LENGTH(%s))',[yyv[yysp-4].yystring,yyv[yysp-2].yystring,yyv[yysp-4].yystring]);
         fForLength := '';
       end;
  91 : begin
       end;
  92 : begin
         fForLength:= yyv[yysp-0].yystring;
       end;
  93 : begin
         fCastType := RW_CHAR;
         fCastLen  := StrToInt( yyv[yysp-1].yystring );
       end;
  94 : begin
         fCastType := RW_INTEGER;
       end;
  95 : begin
         fCastType := RW_BOOLEAN;
       end;
  96 : begin
         fCastType := RW_DATE;
       end;
  97 : begin
         fCastType := RW_TIME;
       end;
  98 : begin
         fCastType := RW_DATETIME;
       end;
  99 : begin
         fCastType := RW_FLOAT;
       end;
 100 : begin
         fCastType := RW_MONEY;
       end;
 101 : begin
         yyval.yystring := yyv[yysp-3].yystring + ' ' + fwhenlist + ' ' + yyv[yysp-1].yystring + ' ' + yyv[yysp-0].yystring;
       end;
 102 : begin
         fwhenlist := '';
       end;
 103 : begin
         yyval := yyv[yysp-0];
       end;
 104 : begin
         yyval := yyv[yysp-1];
       end;
 105 : begin
         fwhenlist := fwhenlist + ' ' + yyv[yysp-3].yystring + ' ' + yyv[yysp-2].yystring + ' ' + yyv[yysp-1].yystring + ' ' + yyv[yysp-0].yystring;
         yyval.yystring := yyv[yysp-3].yystring + ' ' + yyv[yysp-2].yystring + ' ' + yyv[yysp-1].yystring + ' ' + yyv[yysp-0].yystring;

       end;
 106 : begin
       end;
 107 : begin
         yyval.yystring := yyv[yysp-1].yystring + ' ' +yyv[yysp-0].yystring;
       end;
 108 : begin
         yyval := yyv[yysp-0];
       end;
 109 : begin
         yyval := yyv[yysp-0];
       end;
 110 : begin
         yyval := yyv[yysp-0];
       end;
 111 : begin
         yyval := yyv[yysp-0];
       end;
 112 : begin
         yyval := yyv[yysp-0];
       end;
 113 : begin
         yyval.yystring := Format('\f"%s"',[yyv[yysp-0].yystring]);
         CurrentAnalizer.AddFieldIfNot( yyv[yysp-0].yystring );

       end;
 114 : begin
         yyval := yyv[yysp-0];
       end;
 115 : begin
         yyval.yystring := Format('\f"%s.%s"',[yyv[yysp-2].yystring, yyv[yysp-0].yystring]);
         CurrentAnalizer.AddFieldIfNot( yyv[yysp-2].yystring + '.' + yyv[yysp-0].yystring);

       end;
 116 : begin
         yyval.yystring := CurrentAnalizer.ReplaceParams( yyv[yysp-1].yystring + yyv[yysp-0].yystring );
       end;
 117 : begin
         yyval := yyv[yysp-1];
       end;
 118 : begin
         yyval := yyv[yysp-2];
       end;
 119 : begin
         yyval := yyv[yysp-0];
       end;
 120 : begin
         yyval := yyv[yysp-2];
       end;
 121 : begin
         AddTable(yyv[yysp-0].yystring, '', False);
       end;
 122 : begin
         AddTable( yyv[yysp-1].yystring, yyv[yysp-0].yystring, False);
       end;
 123 : begin
         AddTable( yyv[yysp-0].yystring, '', true );
       end;
 124 : begin
         AddTable(yyv[yysp-1].yystring, yyv[yysp-0].yystring, True);
       end;
 125 : begin
         With CurrentAnalizer.TableList.Add Do
         Begin
         NumSubquery:= CurrentAnalizer.SubqueryList.Count-1;
         TableName:= yyv[yysp-0].yystring;
         Alias:= yyv[yysp-0].yystring;
         End;

       end;
 126 : begin
         yyval := yyv[yysp-0];
       end;
 127 : begin
         yyval := yyv[yysp-1];
       end;
 128 : begin
         fTempJoinOnItem.JoinExpression:= yyv[yysp-0].yystring;
         AddJoin;

       end;
 129 : begin
         fTempJoinOnItem.JoinAction := jkLeftInnerJoin;
       end;
 130 : begin
         fTempJoinOnItem.JoinAction := jkLeftInnerJoin;
       end;
 131 : begin
         fTempJoinOnItem.JoinAction := jkLeftInnerJoin;
       end;
 132 : begin
         fTempJoinOnItem.JoinAction := jkLeftInnerJoin;
       end;
 133 : begin
         fTempJoinOnItem.JoinAction := jkLeftOuterJoin;
       end;
 134 : begin
         fTempJoinOnItem.JoinAction := jkLeftOuterJoin;
       end;
 135 : begin
         fTempJoinOnItem.JoinAction := jkRightInnerJoin;
       end;
 136 : begin
         fTempJoinOnItem.JoinAction := jkRightInnerJoin;
       end;
 137 : begin
         fTempJoinOnItem.JoinAction := jkRightOuterJoin;
       end;
 138 : begin
         fTempJoinOnItem.JoinAction := jkFullOuterJoin;
       end;
 139 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 140 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 141 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 142 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 143 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 144 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
         SetJoinTestTbls( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
       end;
 145 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring ;
       end;
 146 : begin
         yyval.yystring:= yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring ;
       end;
 147 : begin
         yyval.yystring:= yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring ;
       end;
 148 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' + ' + yyv[yysp-0].yystring;
       end;
 149 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' - ' + yyv[yysp-0].yystring;
       end;
 150 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' * ' + yyv[yysp-0].yystring;
       end;
 151 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' / ' + yyv[yysp-0].yystring;
       end;
 152 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 153 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 154 : begin
         if fEscapeChar = '' then fEscapeChar := #39#39;
         yyval.yystring := Format('SQLNOTLIKE(%s, %s, %s)',[yyv[yysp-4].yystring, yyv[yysp-1].yystring, fEscapeChar]);
         fEscapeChar:= '';

       end;
 155 : begin
         if fEscapeChar = '' then fEscapeChar := #39#39;
         yyval.yystring := Format('SQLLIKE(%s, %s, %s)',[yyv[yysp-3].yystring, yyv[yysp-1].yystring, fEscapeChar]);
         fEscapeChar:= '';

       end;
 156 : begin
         yyval.yystring := CreateInListExpression( yyv[yysp-4].yystring );
       end;
 157 : begin
         yyval.yystring := Format('(%s >= %s) AND (%s <= %s)',
         [yyv[yysp-4].yystring, yyv[yysp-2].yystring, yyv[yysp-4].yystring, yyv[yysp-0].yystring]);
       end;
 158 : begin
         yyval.yystring := yyv[yysp-2].yystring + '()';
       end;
 159 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 160 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 161 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 162 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 163 : begin
         yyval.yystring:= '[dummy].' + yyv[yysp-0].yystring;
       end;
 164 : begin
         yyval.yystring:= yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 165 : begin
         yyval.yystring:= '[dummy].' + yyv[yysp-0].yystring;
       end;
 166 : begin
         yyval := yyv[yysp-0];
       end;
 167 : begin
         yyval := yyv[yysp-0];
       end;
 168 : begin
         yyval := yyv[yysp-0];
       end;
 169 : begin
         yyval := yyv[yysp-0];
       end;
 170 : begin
         yyval := yyv[yysp-0];
       end;
 171 : begin
         yyval := yyv[yysp-0];
       end;
 172 : begin
         yyval.yystring := yyv[yysp-2].yystring + '()';
       end;
 173 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 174 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 175 : begin
         yyval.yystring := yyv[yysp-3].yystring + yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 176 : begin
         yyval.yystring := Format('(%s >= %s) AND (%s <= %s)',
         [yyv[yysp-4].yystring, yyv[yysp-2].yystring, yyv[yysp-4].yystring, yyv[yysp-0].yystring]);
       end;
 177 : begin
         yyval.yystring := Format('(%s >= %s) AND (%s <= %s)',
         [yyv[yysp-4].yystring, yyv[yysp-2].yystring, yyv[yysp-4].yystring, yyv[yysp-0].yystring]);
         AddWhereOptimize( yyv[yysp-4].yystring, yyv[yysp-2].yystring, yyv[yysp-0].yystring, ropBETWEEN );
       end;
 178 : begin
         yyval.yystring := CreateInListExpression( yyv[yysp-4].yystring );
       end;
 179 : begin
         if Pos('NOT',UpperCase(yyv[yysp-1].yystring)) = 0 then
         yyval.yystring := yyv[yysp-2].yystring + Format(' = (Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1])
         else
         yyval.yystring := yyv[yysp-2].yystring + Format(' <> (Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1]);

       end;
 180 : begin
         if fEscapeChar = '' then fEscapeChar := #39#39;
         yyval.yystring := Format('SQLLIKE(%s, %s, %s)',[yyv[yysp-3].yystring, yyv[yysp-1].yystring, fEscapeChar]);
         fEscapeChar:= '';

       end;
 181 : begin
         if fEscapeChar = '' then fEscapeChar := #39#39;
         yyval.yystring := Format('SQLNOTLIKE(%s, %s, %s)',[yyv[yysp-4].yystring, yyv[yysp-1].yystring, fEscapeChar]);
         fEscapeChar:= '';

       end;
 182 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 183 : begin
         yyval.yystring := yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 184 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' + ' + yyv[yysp-0].yystring;
       end;
 185 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' - ' + yyv[yysp-0].yystring;
       end;
 186 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' * ' + yyv[yysp-0].yystring;
       end;
 187 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' / ' + yyv[yysp-0].yystring;
       end;
 188 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' ^ ' + yyv[yysp-0].yystring;
       end;
 189 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' MOD ' + yyv[yysp-0].yystring;
       end;
 190 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' DIV ' + yyv[yysp-0].yystring;
       end;
 191 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' SHL ' + yyv[yysp-0].yystring;
       end;
 192 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' SHR ' + yyv[yysp-0].yystring;
       end;
 193 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddJoinCandidate( yyv[yysp-2].yystring, yyv[yysp-0].yystring );
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropBETWEEN);
       end;
 194 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropGE);
       end;
 195 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropLE);
       end;
 196 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropGT);
       end;
 197 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropLT);
       end;
 198 : begin
         yyval.yystring := yyv[yysp-2].yystring + #32 + yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring;
         AddWhereOptimize( yyv[yysp-2].yystring, yyv[yysp-0].yystring, yyv[yysp-0].yystring, ropNEQ);
       end;
 199 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' AND ' + yyv[yysp-0].yystring;
       end;
 200 : begin
         yyval.yystring := yyv[yysp-2].yystring + ' OR ' + yyv[yysp-0].yystring;
       end;
 201 : begin
         yyval.yystring := ' NOT ' + yyv[yysp-0].yystring;
       end;
 202 : begin
         yyval.yystring := yyv[yysp-2].yystring + yyv[yysp-1].yystring + yyv[yysp-0].yystring;
       end;
 203 : begin
         yyval.yystring := Format('(Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1]);
       end;
 204 : begin
         yyval.yystring := Format('ISNULL(%s,TRUE)', [yyv[yysp-2].yystring]);
       end;
 205 : begin
         yyval.yystring := Format('ISNULL(%s,FALSE)', [yyv[yysp-3].yystring]);
       end;
 206 : begin
         fIsNotInList := False;
       end;
 207 : begin
         yyval.yystring := yyv[yysp-1].yystring + #32 + yyv[yysp-0].yystring ; fIsNotInList := True;
       end;
 208 : begin
         fInPredicateList.Add( yyv[yysp-0].yystring );
       end;
 209 : begin
         fInPredicateList.Add( yyv[yysp-0].yystring );
       end;
 210 : begin
         yyval := yyv[yysp-0];
       end;
 211 : begin
         yyval := yyv[yysp-0];
       end;
 212 : begin
         yyval := yyv[yysp-0];
       end;
 213 : begin
         if CurrentAnalizer.xQuery.WithDummies then
         yyval.yystring := 'DummyBoolean(True)'
         else
         yyval.yystring := 'True' ;

       end;
 214 : begin
         if CurrentAnalizer.xQuery.WithDummies then
         yyval.yystring := 'DummyBoolean(False)'
         else
         yyval.yystring := 'False';

       end;
 215 : begin
         yyval := yyv[yysp-0];
       end;
 216 : begin
         yyval := yyv[yysp-0];
       end;
 217 : begin
         yyval.yystring:= yyv[yysp-2].yystring + ', ' + yyv[yysp-0].yystring;
       end;
 218 : begin
       end;
 219 : begin
         CurrentAnalizer.WhereStr := yyv[yysp-0].yystring;
       end;
 220 : begin
       end;
 221 : begin
         CurrentAnalizer.UserDefinedRange.UsingIndex:= GetString(yyv[yysp-0].yystring);
       end;
 222 : begin
         CurrentAnalizer.UserDefinedRange.ForFields.Add(GetString(yyv[yysp-0].yystring));
       end;
 223 : begin
         CurrentAnalizer.UserDefinedRange.ForFields.Add(GetString(yyv[yysp-0].yystring));
       end;
 224 : begin
         CurrentAnalizer.UserDefinedRange.StartValues.Add( yyv[yysp-0].yystring );
       end;
 225 : begin
         CurrentAnalizer.UserDefinedRange.StartValues.Add( yyv[yysp-0].yystring );
       end;
 226 : begin
         CurrentAnalizer.UserDefinedRange.EndValues.Add( yyv[yysp-0].yystring );
       end;
 227 : begin
         CurrentAnalizer.UserDefinedRange.EndValues.Add( yyv[yysp-0].yystring );
       end;
 228 : begin
         yyval := yyv[yysp-0];
       end;
 229 : begin
         yyval := yyv[yysp-0];
       end;
 230 : begin
         yyval := yyv[yysp-0];
       end;
 231 : begin
         yyval := yyv[yysp-0];
       end;
 232 : begin
         CurrentAnalizer.SubqueryKindList.Add( Pointer(skAny) );
       end;
 233 : begin
         CurrentAnalizer.SubqueryKindList.Add( Pointer(skAny) );
       end;
 234 : begin
         CurrentAnalizer.SubqueryKindList.Add( Pointer(skAll) );
       end;
 235 : begin
         yyval := yyv[yysp-2];
       end;
 236 : begin
         TSqlAnalizer( CurrentAnalizer.SubqueryList[0] ).Statement:= ssUnion;
       end;
 237 : begin
         yyval := yyv[yysp-5];
       end;
 238 : begin
         CurrentAnalizer.DoSelectAll := True;
       end;
 239 : begin
         yyval := yyv[yysp-1];
       end;
 240 : begin
         yyval := yyv[yysp-0];
       end;
 241 : begin
         CurrentAnalizer.IsDistinct := True;
       end;
 242 : begin
         CreateNewSubquery;
         CurrentAnalizer.TopNInSelect:= FtempTopNInSelect;
         CurrentAnalizer.TopNInGroupBy:= FtempTopNInGroupBy;

       end;
 243 : begin
         FtempTopNInSelect:= 0;
       end;
 244 : begin
         FtempTopNInSelect:= StrToInt( yyv[yysp-0].yystring ) ;
       end;
 245 : begin
         FtempTopNInGroupBy:= 0;
       end;
 246 : begin
         FtempTopNInGroupBy:= StrToInt( yyv[yysp-0].yystring ) ;
       end;
 247 : begin
         Self.fCurrAnalizer := Self.fCurrAnalizer.ParentAnalizer;
       end;
 248 : begin
         fEscapeChar := '';
       end;
 249 : begin
         fEscapeChar := GetString( yyv[yysp-0].yystring );
       end;
 250 : begin
       end;
 251 : begin
         yyval := yyv[yysp-3];
       end;
 252 : begin
         AddGroupBy( yyv[yysp-0].yystring );
       end;
 253 : begin
         AddGroupBy( yyv[yysp-0].yystring );
       end;
 254 : begin
         yyval.yystring := Format('\f"%s"', [yyv[yysp-0].yystring]);
       end;
 255 : begin
         yyval := yyv[yysp-0];
       end;
 256 : begin
         yyval.yystring := yyv[yysp-0].yystring;
       end;
 257 : begin
       end;
 258 : begin
         AddHavingColumn( yyv[yysp-0].yystring );
         CurrentAnalizer.HavingCol := CurrentAnalizer.ColumnList.Count-1;
       end;
 259 : begin
       end;
 260 : begin
         yyval := yyv[yysp-2];
       end;
 261 : begin
         yyval := yyv[yysp-0];
       end;
 262 : begin
         yyval := yyv[yysp-2];
       end;
 263 : begin
         AddOrderBy( yyv[yysp-0].yystring, False );
       end;
 264 : begin
         AddOrderBy( yyv[yysp-1].yystring, False );
       end;
 265 : begin
         AddOrderBy( yyv[yysp-1].yystring, True );
       end;
 266 : begin
       end;
 267 : begin
         yyval := yyv[yysp-0];
       end;
 268 : begin
         CurrentAnalizer.UpdateColumnList.SyntaxUsed:= 0;
       end;
 269 : begin
         // NOTA: hay un error en esta sintaxis.
         CurrentAnalizer.UpdateColumnList.SyntaxUsed:= 1;
       end;
 270 : begin
         yyval := yyv[yysp-0];
       end;
 271 : begin
         yyval := yyv[yysp-2];
       end;
 272 : begin
         AddUpdateColumn(yyv[yysp-2].yystring, yyv[yysp-0].yystring);
       end;
 273 : begin
         AddUpdateColumn(yyv[yysp-2].yystring,'');
       end;
 274 : begin
         yyval := yyv[yysp-4];
       end;
 275 : begin
         yyval := yyv[yysp-1];
       end;
 276 : begin
         Inc(fNumInserts);
       end;
 277 : begin
         Inc(fNumInserts);
       end;
 278 : begin
         if fIsFullPath then
         CurrentInsertItem.TableName := Copy(yyv[yysp-3].yystring,2,Length(yyv[yysp-3].yystring)-2)
         else
         CurrentInsertItem.TableName := yyv[yysp-3].yystring;
         CurrentInsertItem.IsFullPath  := self.fIsFullPath ;

       end;
 279 : begin
       end;
 280 : begin
         yyval := yyv[yysp-0];
       end;
 281 : begin
         fIsFullPath := false;
       end;
 282 : begin
         fIsFullPath := true;
       end;
 283 : begin
         yyval := yyv[yysp-2];
       end;
 284 : begin
         yyval := yyv[yysp-2];
       end;
 285 : begin
         CurrentInsertItem.FieldNames.Add(yyv[yysp-0].yystring);
       end;
 286 : begin
         CurrentInsertItem.FieldNames.Add(yyv[yysp-0].yystring);
       end;
 287 : begin
         yyval := yyv[yysp-2];
       end;
 288 : begin
         yyval := yyv[yysp-0];
       end;
 289 : begin
         CurrentInsertItem.ExprList.Add( yyv[yysp-0].yystring );
       end;
 290 : begin
         CurrentInsertItem.ExprList.Add( yyv[yysp-0].yystring );
       end;
 291 : begin
         yyval := yyv[yysp-0];
       end;
 292 : begin
         yyval := yyv[yysp-0];
       end;
 293 : begin
         yyval := yyv[yysp-1];
       end;
 294 : begin
         yyval := yyv[yysp-0];
       end;
 295 : begin
         yyval := yyv[yysp-1];
       end;
 296 : begin
         SetAlterTableName( GetString( yyv[yysp-1].yystring ) );
       end;
 297 : begin
         yyval := yyv[yysp-0];
       end;
 298 : begin
         yyval := yyv[yysp-2];
       end;
 299 : begin
         AddAlterField(yyv[yysp-1].yystring,false);
       end;
 300 : begin
         AddAlterField(yyv[yysp-0].yystring,true);
       end;
 301 : begin
       end;
 302 : begin
         yyval := yyv[yysp-0];
       end;
 303 : begin
         yyval := yyv[yysp-1];
       end;
 304 : begin
         Inc(FNumTables);
       end;
 305 : begin
         Inc(FNumTables);
       end;
 306 : begin
         SetTableName( GetString( yyv[yysp-4].yystring ) );
       end;
 307 : begin
         SetFieldParams(RW_CHAR,0,0,StrToInt(yyv[yysp-1].yystring),0);
       end;
 308 : begin
         SetFieldParams(RW_INTEGER,0,0,0,0);
       end;
 309 : begin
         SetFieldParams(RW_SMALLINT,0,0,0,0);
       end;
 310 : begin
         SetFieldParams(RW_BOOLEAN,0,0,0,0);
       end;
 311 : begin
         SetFieldParams(RW_DATE,0,0,0,0);
       end;
 312 : begin
         SetFieldParams(RW_TIME,0,0,0,0);
       end;
 313 : begin
         SetFieldParams(RW_DATETIME,0,0,0,0);
       end;
 314 : begin
         SetFieldParams(RW_MONEY,0,0,0,0);
       end;
 315 : begin
         SetFieldParams(RW_FLOAT,0,0,0,0);
       end;
 316 : begin
         SetFieldParams(RW_FLOAT,StrToInt(yyv[yysp-1].yystring),0,0,0);
       end;
 317 : begin
         SetFieldParams(RW_FLOAT,StrToInt(yyv[yysp-3].yystring),StrToInt(yyv[yysp-1].yystring),0,0);
       end;
 318 : begin
         SetFieldParams(RW_AUTOINC,0,0,0,0);
       end;
 319 : begin
         SetFieldParams(RW_BLOB,0,0,0,StrToInt(yyv[yysp-1].yystring));
       end;
 320 : begin
         AddCreateField(yyv[yysp-1].yystring);
       end;
 321 : begin
         AddCreateField(yyv[yysp-1].yystring);
       end;
 322 : begin
       end;
 323 : begin
         yyval := yyv[yysp-4];
       end;
 324 : begin
         AddPrimaryKeyField( yyv[yysp-0].yystring );
       end;
 325 : begin
         AddPrimaryKeyField( yyv[yysp-0].yystring );
       end;
 326 : begin
         CurrentAnalizer.IndexName  := yyv[yysp-6].yystring;
         CurrentAnalizer.IndexTable := GetString( yyv[yysp-4].yystring );
       end;
 327 : begin
       end;
 328 : begin
         CurrentAnalizer.IndexUnique := True;
       end;
 329 : begin
       end;
 330 : begin
         yyval := yyv[yysp-0];
       end;
 331 : begin
         CurrentAnalizer.IndexDescending := True;
       end;
 332 : begin
         CurrentAnalizer.IndexColumnList.Add(yyv[yysp-0].yystring);
       end;
 333 : begin
         CurrentAnalizer.IndexColumnList.Add(yyv[yysp-0].yystring);
       end;
 334 : begin
         CurrentAnalizer.IndexTable:= GetString( yyv[yysp-1].yystring );
       end;
 335 : begin
         CurrentAnalizer.IndexTable:= GetString( yyv[yysp-2].yystring );
         CurrentAnalizer.IndexName := yyv[yysp-1].yystring;
       end;
 336 : begin
         CurrentAnalizer.PivotStr := yyv[yysp-2].yystring;
       end;
 337 : begin
         AddColumn( yyv[yysp-0].yystring, True );
       end;
 338 : begin
         AddColumn( yyv[yysp-3].yystring, True );
       end;
 339 : begin
         yyval := yyv[yysp-0];
       end;
 340 : begin
         yyval := yyv[yysp-2];
       end;
 341 : begin
       end;
 342 : begin
         yyval := yyv[yysp-3];
       end;
 343 : begin
         CurrentAnalizer.SubqueryInPivotPredicate:= true ;
       end;
 344 : begin
         CurrentAnalizer.PivotInList.Add( RemoveStrDelim( yyv[yysp-0].yystring));
       end;
 345 : begin
         CurrentAnalizer.PivotInList.Add( RemoveStrDelim( yyv[yysp-0].yystring));
       end;
 346 : begin
         yyval := yyv[yysp-5];
       end;
 347 : begin
         CurrentAnalizer.DoSelectAll := True;
       end;
 348 : begin
         yyval := yyv[yysp-1];
       end;
 349 : begin
         yyval := yyv[yysp-0];
       end;
 350 : begin
         CurrentAnalizer.IsDistinct := True;
       end;
 351 : begin
         CurrentAnalizer.UnionAnalizer :=
         TSqlAnalizer.Create( CurrentAnalizer.ParentAnalizer, fAnalizer.xQuery );
         Self.fCurrAnalizer := CurrentAnalizer.UnionAnalizer;
       end;
 352 : begin
         yyval := yyv[yysp-2];
       end;
 353 : begin
         yyval := yyv[yysp-2];
       end;
 354 : begin
         yyval := yyv[yysp-2];
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : SmallInt;
              end;
     YYRRec = record
                len, sym : SmallInt;
              end;

const

yynacts   = 4700;
yyngotos  = 1169;
yynstates = 691;
yynrules  = 354;

var

yya : array [1..yynacts    ] of YYARec;
yyg : array [1..yyngotos   ] of YYARec;
yyd : array [0..yynstates-1] of SmallInt;
yyal: array [0..yynstates-1] of SmallInt;
yyah: array [0..yynstates-1] of SmallInt;
yygl: array [0..yynstates-1] of SmallInt;
yygh: array [0..yynstates-1] of SmallInt;
yyr : array [1..yynrules   ] of YYRRec;

procedure LoadResArrays;

  procedure ResLoad(const resname: string; ResourceBuffer: Pointer);
  var
    ResourceSize: Integer;
    ResourcePtr: PChar;
    BinResource: THandle;
    ResInstance: Longint;
    H: THandle;
    Buf: array[0..255] of Char;
  begin
    H := System.FindResourceHInstance(HInstance);
    StrPLCopy(Buf, resname, SizeOf(Buf)-1);
    ResInstance := FindResource(H, Buf, RT_RCDATA);
    if ResInstance = 0 then begin
      H := HInstance;
      {try to find in main binary}
      ResInstance := FindResource(H, Buf, RT_RCDATA);
    end;
    ResourceSize := SizeofResource(H,ResInstance);
    BinResource := LoadResource(H,ResInstance);
    ResourcePtr := LockResource(BinResource);
    Move(ResourcePtr^, ResourceBuffer^, ResourceSize);
    UnlockResource(BinResource);
    FreeResource(BinResource);

  end;
begin

  ResLoad('xqyacc_YYA', @yya[1]);
  ResLoad('xqyacc_YYG', @yyg[1]);

  ResLoad('xqyacc_YYD', @yyd[0]);

  ResLoad('xqyacc_YYAL', @yyal[0]);

  ResLoad('xqyacc_YYAH', @yyah[0]);

  ResLoad('xqyacc_YYGL', @yygl[0]);

  ResLoad('xqyacc_YYGH', @yygh[0]);

  ResLoad('xqyacc_YYR', @yyr[1]);


end;


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : SmallInt) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : SmallInt) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* load arrays from resource *)
  LoadResArrays;

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      repeat
         yychar := yyLexer.yylex; if yychar<0 then yychar := 0;
         // ignore comments and blanks [ \n\t]
         if not( (yychar=_COMMENT) or (yychar=_BLANK) or
                 (yychar=_TAB) or (yychar=_NEWLINE) ) then break;
      until false;
      if yychar= _ILLEGAL then goto error;
    end;

  (*
  if yydebug then
    writeln( yyLexer.yyOutput, 'state '+intToStr( yystate)+ ', char '+
                               intToStr( yychar) + ' at line n°'+
                               intToStr(yyLexer.yylineno) + ', col n°' +
                               intToStr( yyLexer.yycolno));
  *)

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          (*
          if yydebug then
            if yysp>1 then
              writeln( yyLexer.yyOutput, 'error recovery pops state ' +
                       intToStr(yys[yysp])+', uncovers '+ intToStr(yys[yysp-1]))
            else
              writeln( yyLexer.yyOutput, 'error recovery fails ... abort');
          *)
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      (*
      if yydebug then
        writeln( yyLexer.yyOutput, 'error recovery discards char '+
                 intToStr( yychar));
      *)
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  //if yydebug then writeln( yyLexer.yyOutput, 'reduce '+ intToStr( -yyn));

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);

end.