/* Grammar for TxQuery dataset (Delphi 3,4,5,6), (c) 2002 Alfonso Moreno
   NOTES :
   DON'T FORGET TO MOVE THE GENERATED CONSTANTS TO THE PLACE INDICATED
*/

%{

{*****************************************************************************}
{         Parser for TxQuery component                                        }
{         Copyright (c) 2003 by Alfonso Moreno                                }
{*****************************************************************************}

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

//
// The generated constants must be placed here
// HERE !!!!
//

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
         if (Result[1] in xqbase.SQuote) and (Result[Length(Result)] in xqbase.SQuote) then
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

%}

%start sql

%token _IDENTIFIER
%token _UINTEGER
%token _NUMERIC
%token _STRING

%token _COMA
%token _LPAREN
%token _RPAREN
%token _LSQUARE
%token _RSQUARE
%token _PERIOD
%token _SEMICOLON
%token _COLON

%left  RW_OR
%left  RW_AND
%left  _EQ _NEQ _GT _LT _GE _LE RW_BETWEEN RW_IN RW_LIKE
%left  _PLUS _SUB
%left  _DIV _MULT RW_MOD RW_IDIV RW_SHL RW_SHR
%right UMINUS       /* Negation--unary minus */
%right _EXP       /* exponentiation */
%left  RW_NOT
%token _ILLEGAL


/* other reserved words and tokens */
%token  _COMMENT
        _BLANK
        _TAB
        _NEWLINE
        RW_TRUE
        RW_FALSE
        RW_SELECT
        RW_DISTINCT
        RW_FROM
        RW_WHERE
        RW_ORDER
        RW_BY
        RW_ASC
        RW_DESC
        RW_AS
        RW_INNER
        RW_OUTER
        RW_FULL
        RW_JOIN
        RW_ON
        RW_GROUP
        RW_HAVING
        RW_ANY
        RW_ALL
        RW_SUM
        RW_AVG
        RW_COUNT
        RW_MIN
        RW_MAX
        RW_STDEV
        RW_LEFT
        RW_RIGHT
        RW_LEADING
        RW_TRAILING
        RW_BOTH
        RW_TRIM
        RW_EXTRACT
        RW_YEAR
        RW_MONTH
        RW_DAY
        RW_HOUR
        RW_MINUTE
        RW_SECOND
        RW_FOR
        RW_SUBSTRING
        RW_DELETE
        RW_UPDATE
        RW_INSERT
        RW_INTO
        RW_VALUES
        RW_SET
        RW_CAST
        RW_CHAR
        RW_INTEGER
        RW_BOOLEAN
        RW_DATE
        RW_TIME
        RW_DATETIME
        RW_FLOAT
        RW_ESCAPE
        RW_CREATE
        RW_TABLE
        RW_SMALLINT
        RW_MONEY
        RW_AUTOINC
        RW_PRIMARY
        RW_KEY
        RW_BLOB
        RW_INDEX
        RW_UNIQUE
        RW_DROP
        RW_TRANSFORM
        RW_PIVOT
        RW_UNION
        RW_WITH
        RW_IS
        RW_NULL
        RW_ALTER
        RW_COLUMN
        RW_ADD
        RW_APPEND
        RW_CASE
        RW_WHEN
        RW_THEN
        RW_ELSE
        RW_END
        RW_PACK
        RW_ZAP
        RW_REINDEX
        RW_RANGE
        RW_USING
        RW_FIELDS
        RW_TO
        RW_RANGE
        RW_TO
        RW_TOP

%type <string>

%%

sql : SelectStatement        { fAnalizer.Statement := ssSelect;      }
    | TransformStatement     { fAnalizer.Statement := ssSelect;      }
    | UnionStatement         { fAnalizer.Statement := ssUnion;       }
    | UpdateStatement        { fAnalizer.Statement := ssUpdate;      }
    | DeleteStatement        { fAnalizer.Statement := ssDelete;      }
    | InsertStatement        { fAnalizer.Statement := ssInsert;      }
    | CreateTableStatement   { fAnalizer.Statement := ssCreateTable; }
    | AlterTableStatement    { fAnalizer.Statement := ssAlterTable; }
    | CreateIndexStatement   { fAnalizer.Statement := ssCreateIndex; }
    | DropTableStatement     { fAnalizer.Statement := ssDropTable;   }
    | DropIndexStatement     { fAnalizer.Statement := ssDropIndex;   }
    | PackTableStatement     { fAnalizer.Statement := ssPacktable;    }
    | ZapTableStatement      { fAnalizer.Statement := ssZapTable;     }
    | ReindexTableStatement  { fAnalizer.Statement := ssReindexTable; }
    ;

/*  SELECT statement */
SelectStatement : SelectClause FromClause WhereClause UserDefRangeClause
                   GroupByClause OrderByClause ForCopyAppend EndStatement
                 ;

UnionStatement : SelectStatement RW_UNION SecondSelectStatement EndStatement
                ;

ForCopyAppend : /* empty */
                | RW_INTO _IDENTIFIER RW_FOR RW_APPEND
                  { CurrentAnalizer.IntoTable:= $<string>2; }
                ;

/* SELECT clause */
SelectClause : SelectCase _MULT
                { CurrentAnalizer.DoSelectAll := True;
                  CurrentAnalizer.AddFieldIfNot('*');}
              | SelectCase ExprFieldList
              ;

SelectCase : TOPNGroupBy RW_SELECT TOPNSelect
            | TOPNGroupBy RW_SELECT TOPNSelect RW_DISTINCT
              {CurrentAnalizer.IsDistinct:= True;}
            ;

TOPNSelect : /* empty */
            | RW_TOP _UINTEGER
              { CurrentAnalizer.TopNInSelect:= StrToInt( $<string>2 ) ; }
            ;

TOPNGroupBy : /* empty */
             | RW_TOP _UINTEGER
               { CurrentAnalizer.TopNInGroupBy:= StrToInt( $<string>2 ) ; }
             ;

ExprFieldList : ExprField
              | ExprFieldList _COMA ExprField
              ;

ExprField : SelectExpr AsIdentifier
             { addColumn( $<string>1, false ); }
           | _IDENTIFIER _PERIOD _MULT
             { CurrentAnalizer.TableAllFields.Add( $<string>1 );
               CurrentAnalizer.AddFieldIfNot( $<string>1 + '.*' );
             }
           | RW_CAST _LPAREN SelectExpr RW_AS DataType _RPAREN AsIdentifier
             { AddColumn( $<string>3, False ); }
           ;

AsIdentifier : /* empty */
              | AsOptional _IDENTIFIER
                { fAsAlias:= $<string>2;}
              ;

AsOptional : /* empty */
            | RW_AS
            ;

SelectExpr  : DefineField
             | DefineConstant
             | SelectFunc
             | TrimFunction
             | ExtractFunction
             | SubstringFunction
             | CaseFunction
             | Subquery                        { $<string>$ := AddSubqueryInSelect; }

             | SelectExpr _PLUS SelectExpr   { $<string>$ := $<string>1 + ' + '  + $<string>3; }
             | SelectExpr _SUB SelectExpr    { $<string>$ := $<string>1 + ' - '  + $<string>3; }
             | SelectExpr _MULT SelectExpr   { $<string>$ := $<string>1 + ' * '  + $<string>3; }
             | SelectExpr _DIV SelectExpr    { $<string>$ := $<string>1 + ' / '  + $<string>3; }
             | SelectExpr _EXP SelectExpr    { $<string>$ := $<string>1 + ' ^ '  + $<string>3; }
             | SelectExpr RW_MOD SelectExpr  { $<string>$ := $<string>1 + ' MOD '  + $<string>3; }
             | SelectExpr RW_IDIV SelectExpr { $<string>$ := $<string>1 + ' DIV '  + $<string>3; }
             | SelectExpr RW_SHL SelectExpr  { $<string>$ := $<string>1 + ' SHL '  + $<string>3; }
             | SelectExpr RW_SHR SelectExpr  { $<string>$ := $<string>1 + ' SHR '  + $<string>3; }

             | SelectExpr _GT SelectExpr   { $<string>$ := $<string>1 + ' > '  + $<string>3; }
             | SelectExpr _LT SelectExpr   { $<string>$ := $<string>1 + ' < '  + $<string>3; }
             | SelectExpr _NEQ SelectExpr  { $<string>$ := $<string>1 + ' <> ' + $<string>3; }
             | SelectExpr _GE SelectExpr   { $<string>$ := $<string>1 + ' >= ' + $<string>3; }
             | SelectExpr _LE SelectExpr   { $<string>$ := $<string>1 + ' <= ' + $<string>3; }
             | SelectExpr _EQ SelectExpr   { $<string>$ := $<string>1 + ' = '  + $<string>3; }

             | SelectExpr RW_AND SelectExpr { $<string>$ := $<string>1 + ' AND ' + $<string>3; }
             | SelectExpr RW_OR  SelectExpr { $<string>$ := $<string>1 + ' AND ' + $<string>3; }

             | _SUB SelectExpr %prec UMINUS  { $<string>$ := $<string>1 + $<string>2;}
             | _PLUS SelectExpr %prec UMINUS { $<string>$ := $<string>1 + $<string>2;}

             | _LPAREN SelectExpr _RPAREN    { $<string>$ := $<string>1 + $<string>2 + $<string>3;}

               /* Aggregate functions follows */
             | RW_SUM _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akSUM, $<string>4); }
             | RW_MIN _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akMIN, $<string>4); }
             | RW_MAX _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akMAX, $<string>4); }
             | RW_AVG _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akAVG, $<string>4); }
             | RW_STDEV _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akSTDEV, $<string>4); }
             | RW_COUNT _LPAREN _MULT _RPAREN
               { $<string>$ := AddAggregate(akCOUNT, '0'); }
             | RW_COUNT _LPAREN DistinctAggr SelectExpr _RPAREN
               { $<string>$ := AddAggregate(akCOUNT, $<string>4); }
             ;

DistinctAggr : /* empty */
              | RW_DISTINCT  { Self.fIsDistinctAggr:= True; }
              ;

SelectFunc : _IDENTIFIER _LPAREN _RPAREN
              { $<string>$ := $<string>1 + '()';}
            | _IDENTIFIER _LPAREN SelectListParam _RPAREN
              { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4;}
            ;

SelectListParam : _MULT
                  | ListParamExpr
                  ;

ListParamExpr : SelectExpr
                | ListParamExpr _COMA SelectExpr
                  { $<string>$:= $<string>1 + ', ' + $<string>3; }
                ;

TrimFunction : RW_TRIM _LPAREN TrimPosition _STRING RW_FROM WhereExpr _RPAREN
                {
                  $<string>$ := Format('SQLTRIM(%s, %s, %d)',[$<string>4, $<string>6, fTrimPosition]);
                }
              ;

TrimPosition : RW_LEADING   {fTrimPosition := 0;}
              | RW_TRAILING  {fTrimPosition := 1;}
              | RW_BOTH      {fTrimPosition := 2;}
              ;

ExtractFunction : RW_EXTRACT _LPAREN ExtractField RW_FROM WhereExpr _RPAREN
       { case fExtractField of
           0: $<string>$ := Format('YEAR(%s)',  [$<string>5]);
           1: $<string>$ := Format('MONTH(%s)', [$<string>5]);
           2: $<string>$ := Format('DAY(%s)',   [$<string>5]);
           3: $<string>$ := Format('HOUR(%s)',  [$<string>5]);
           4: $<string>$ := Format('MIN(%s)',   [$<string>5]);
           5: $<string>$ := Format('SEC(%s)',   [$<string>5]);
         end;
       }
     ;

ExtractField : RW_YEAR   { fExtractField:= 0; }
              | RW_MONTH  { fExtractField:= 1; }
              | RW_DAY    { fExtractField:= 2; }
              | RW_HOUR   { fExtractField:= 3; }
              | RW_MINUTE { fExtractField:= 4; }
              | RW_SECOND { fExtractField:= 5; }
              ;

SubstringFunction : RW_SUBSTRING _LPAREN WhereExpr RW_FROM _UINTEGER ForLength _RPAREN
                     { if Length(fForLength) > 0 then
                          $<string>$ := Format('COPY(%s,%s,%s)',[$<string>3,$<string>5,fForLength])
                       else
                          $<string>$ := Format('COPY(%s,%s,LENGTH(%s))',[$<string>3,$<string>5,$<string>3]);
                       fForLength := '';}
       ;

ForLength : /* empty */
           | RW_FOR _UINTEGER {fForLength:= $<string>2;}
           ;

DataType : RW_CHAR _LPAREN _UINTEGER _RPAREN
            { fCastType := RW_CHAR;
              fCastLen  := StrToInt( $<string>3 ); }
          | RW_INTEGER                          { fCastType := RW_INTEGER; }
          | RW_BOOLEAN                          { fCastType := RW_BOOLEAN; }
          | RW_DATE                             { fCastType := RW_DATE; }
          | RW_TIME                             { fCastType := RW_TIME; }
          | RW_DATETIME                         { fCastType := RW_DATETIME; }
          | RW_FLOAT                            { fCastType := RW_FLOAT; }
          | RW_MONEY                            { fCastType := RW_MONEY; }
          ;

/* CASE...WHEN...THEN...ELSE...END clause */

CaseFunction : StartCase WhenList ElseClause RW_END
                { $<string>$ := $<string>1 + ' ' + fwhenlist + ' ' + $<string>3 + ' ' + $<string>4; }
              ;

StartCase : RW_CASE
            { fwhenlist := ''; }
           ;

WhenList : WhenClause
          | WhenList WhenClause
          ;

WhenClause : RW_WHEN SelectExpr RW_THEN WhenConstant
              { fwhenlist := fwhenlist + ' ' + $<string>1 + ' ' + $<string>2 + ' ' + $<string>3 + ' ' + $<string>4;
                $<string>$ := $<string>1 + ' ' + $<string>2 + ' ' + $<string>3 + ' ' + $<string>4;
              }
            ;

ElseClause : /* empty */
            | RW_ELSE WhenConstant  { $<string>$ := $<string>1 + ' ' +$<string>2; }
            ;

WhenConstant : _UINTEGER
              | _NUMERIC
              | _STRING
              | RW_TRUE
              | RW_FALSE
              ;

DefineField : _IDENTIFIER
               { $<string>$ := Format('\f"%s"',[$<string>1]);
                 CurrentAnalizer.AddFieldIfNot( $<string>1 );
               }
             | QualifiedField
             ;

QualifiedField : _IDENTIFIER _PERIOD _IDENTIFIER
                  { $<string>$ := Format('\f"%s.%s"',[$<string>1, $<string>3]);
                    CurrentAnalizer.AddFieldIfNot( $<string>1 + '.' + $<string>3);
                  }
                ;

DefineParam : _COLON _IDENTIFIER
               { $<string>$ := CurrentAnalizer.ReplaceParams( $<string>1 + $<string>2 ); }
             ;

/* FROM clause */
FromClause : RW_FROM ListTables
           | RW_FROM TableIdentifier ListJoin
           ;

ListTables : TableIdentifier
           | ListTables _COMA TableIdentifier
           ;

TableIdentifier : _IDENTIFIER
                   { AddTable($<string>1, '', False);}
                 | _IDENTIFIER _IDENTIFIER
                   { AddTable( $<string>1, $<string>2, False); }
                 | _STRING
                   { AddTable( $<string>1, '', true );}
                 | _STRING _IDENTIFIER
                   { AddTable($<string>1, $<string>2, True); }
                 | Subquery _IDENTIFIER
                   { With CurrentAnalizer.TableList.Add Do
                     Begin
                       NumSubquery:= CurrentAnalizer.SubqueryList.Count-1;
                       TableName:= $<string>2;
                       Alias:= $<string>2;
                     End;
                   }
                 ;


/* Explicit JOINing */
ListJoin : ListJoinTable
         | ListJoin ListJoinTable
         ;

ListJoinTable : JoinAction TableIdentifier RW_ON JoinExpression
                  { fTempJoinOnItem.JoinExpression:= $<string>4;
                    AddJoin;
                  }
              ;

JoinAction : RW_JOIN                     { fTempJoinOnItem.JoinAction := jkLeftInnerJoin;      }
           | RW_INNER RW_JOIN            { fTempJoinOnItem.JoinAction := jkLeftInnerJoin;      }
           | RW_LEFT RW_INNER RW_JOIN    { fTempJoinOnItem.JoinAction := jkLeftInnerJoin;  }
           | RW_LEFT RW_JOIN             { fTempJoinOnItem.JoinAction := jkLeftInnerJoin;  }
           | RW_OUTER RW_JOIN            { fTempJoinOnItem.JoinAction := jkLeftOuterJoin;  }
           | RW_LEFT RW_OUTER RW_JOIN    { fTempJoinOnItem.JoinAction := jkLeftOuterJoin;  }
           | RW_RIGHT RW_INNER RW_JOIN   { fTempJoinOnItem.JoinAction := jkRightInnerJoin; }
           | RW_RIGHT RW_JOIN            { fTempJoinOnItem.JoinAction := jkRightInnerJoin; }
           | RW_RIGHT RW_OUTER RW_JOIN   { fTempJoinOnItem.JoinAction := jkRightOuterJoin; }
           | RW_FULL RW_OUTER RW_JOIN    { fTempJoinOnItem.JoinAction := jkFullOuterJoin;  }
           ;

JoinExpression : LeftJoinField _EQ RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | LeftJoinField _NEQ RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | LeftJoinField _GE RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | LeftJoinField _LE RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | LeftJoinField _GT RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | LeftJoinField _LT RightJoinField
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ;
                   SetJoinTestTbls( $<string>1, $<string>3 ); }
               | _LPAREN JoinExpression _RPAREN
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3 ; }
               | JoinExpression RW_AND JoinExpression
                 { $<string>$:= $<string>1 + #32 + $<string>2 + #32 + $<string>3 ; }
               | JoinExpression RW_OR JoinExpression
                 { $<string>$:= $<string>1 + #32 + $<string>2 + #32 + $<string>3 ; }
               | JoinExpression _PLUS JoinExpression
                 {$<string>$ := $<string>1 + ' + ' + $<string>3;}
               | JoinExpression _SUB JoinExpressionr
                 {$<string>$ := $<string>1 + ' - ' + $<string>3;}
               | JoinExpression _MULT JoinExpression
                 {$<string>$ := $<string>1 + ' * ' + $<string>3;}
               | JoinExpression _DIV JoinExpression
                 {$<string>$ := $<string>1 + ' / ' + $<string>3;}
               | _SUB JoinExpression %prec UMINUS
                 {$<string>$ := $<string>1 + $<string>2;}
               | _PLUS JoinExpression %prec UMINUS
                 {$<string>$ := $<string>1 + $<string>2;}
               | JoinExpression RW_NOT RW_LIKE _STRING EscapeCharacter
                 { if fEscapeChar = '' then fEscapeChar := #39#39;
                   $<string>$ := Format('SQLNOTLIKE(%s, %s, %s)',[$<string>1, $<string>4, fEscapeChar]);
                   fEscapeChar:= '';
                 }
               | JoinExpression RW_LIKE _STRING EscapeCharacter
                 { if fEscapeChar = '' then fEscapeChar := #39#39;
                   $<string>$ := Format('SQLLIKE(%s, %s, %s)',[$<string>1, $<string>3, fEscapeChar]);
                   fEscapeChar:= '';
                 }
               | JoinExpression InOperator _LPAREN InPredicate _RPAREN
                 { $<string>$ := CreateInListExpression( $<string>1 ); }
               | JoinExpression RW_BETWEEN DefineConstant RW_AND DefineConstant
                 { $<string>$ := Format('(%s >= %s) AND (%s <= %s)',
                   [$<string>1, $<string>3, $<string>1, $<string>5]); }
               | _IDENTIFIER _LPAREN _RPAREN
                 { $<string>$ := $<string>1 + '()'; }
               | _IDENTIFIER _LPAREN WhereListParam _RPAREN
                 { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
               | RW_LEFT _LPAREN WhereListParam _RPAREN
                 { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
               | RW_RIGHT _LPAREN WhereListParam _RPAREN
                 { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
               ;

LeftJoinField : _IDENTIFIER _PERIOD _IDENTIFIER
                { $<string>$:= $<string>1 + $<string>2 + $<string>3; }
              | _IDENTIFIER
                { $<string>$:= '[dummy].' + $<string>1; }
              ;

RightJoinField : _IDENTIFIER _PERIOD _IDENTIFIER
                 { $<string>$:= $<string>1 + $<string>2 + $<string>3; }
               | _IDENTIFIER
                 { $<string>$:= '[dummy].' + $<string>1; }
               ;

/* WHERE clause */
WhereExpr : DefineField
          | DefineConstant
          | TrimFunction
          | ExtractFunction
          | SubstringFunction
          | IsNull
          | _IDENTIFIER _LPAREN _RPAREN
            { $<string>$ := $<string>1 + '()'; }
          | _IDENTIFIER _LPAREN WhereListParam _RPAREN
            { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
          | RW_LEFT _LPAREN WhereListParam _RPAREN
            { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
          | RW_RIGHT _LPAREN WhereListParam _RPAREN
            { $<string>$ := $<string>1 + $<string>2 + $<string>3 + $<string>4; }
          | WhereExpr RW_BETWEEN DefineField RW_AND DefineField
            { $<string>$ := Format('(%s >= %s) AND (%s <= %s)',
              [$<string>1, $<string>3, $<string>1, $<string>5]); }
          | WhereExpr RW_BETWEEN DefineConstant RW_AND DefineConstant
            { $<string>$ := Format('(%s >= %s) AND (%s <= %s)',
              [$<string>1, $<string>3, $<string>1, $<string>5]);
              AddWhereOptimize( $<string>1, $<string>3, $<string>5, ropBETWEEN ); }
          | WhereExpr InOperator _LPAREN InPredicate _RPAREN
            { $<string>$ := CreateInListExpression( $<string>1 ); }
          | WhereExpr InOperator AnyAllSubquery
            { if Pos('NOT',UpperCase($<string>2)) = 0 then
                $<string>$ := $<string>1 + Format(' = (Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1])
             else
                $<string>$ := $<string>1 + Format(' <> (Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1]);
            }
          | WhereExpr RW_LIKE _STRING EscapeCharacter
            { if fEscapeChar = '' then fEscapeChar := #39#39;
              $<string>$ := Format('SQLLIKE(%s, %s, %s)',[$<string>1, $<string>3, fEscapeChar]);
              fEscapeChar:= '';
            }
          | WhereExpr RW_NOT RW_LIKE _STRING EscapeCharacter
            { if fEscapeChar = '' then fEscapeChar := #39#39;
              $<string>$ := Format('SQLNOTLIKE(%s, %s, %s)',[$<string>1, $<string>4, fEscapeChar]);
              fEscapeChar:= '';
            }

          | _SUB WhereExpr %prec UMINUS     {$<string>$ := $<string>1 + $<string>2;}
          | _PLUS WhereExpr %prec UMINUS    {$<string>$ := $<string>1 + $<string>2;}

          | WhereExpr _PLUS WhereExpr    {$<string>$ := $<string>1 + ' + ' + $<string>3;}
          | WhereExpr _SUB WhereExpr     {$<string>$ := $<string>1 + ' - ' + $<string>3;}
          | WhereExpr _MULT WhereExpr    {$<string>$ := $<string>1 + ' * ' + $<string>3;}
          | WhereExpr _DIV WhereExpr     {$<string>$ := $<string>1 + ' / ' + $<string>3;}
          | WhereExpr _EXP WhereExpr     {$<string>$ := $<string>1 + ' ^ ' + $<string>3;}
          | WhereExpr RW_MOD WhereExpr   {$<string>$ := $<string>1 + ' MOD ' + $<string>3;}
          | WhereExpr RW_IDIV WhereExpr  {$<string>$ := $<string>1 + ' DIV ' + $<string>3;}
          | WhereExpr RW_SHL WhereExpr   {$<string>$ := $<string>1 + ' SHL ' + $<string>3;}
          | WhereExpr RW_SHR WhereExpr   {$<string>$ := $<string>1 + ' SHR ' + $<string>3;}

          | WhereExpr _EQ WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddJoinCandidate( $<string>1, $<string>3 );
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropBETWEEN);  }
          | WhereExpr _GE WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropGE); }
          | WhereExpr _LE WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropLE); }
          | WhereExpr _GT WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropGT); }
          | WhereExpr _LT WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropLT); }
          | WhereExpr _NEQ WhereExpr
            { $<string>$ := $<string>1 + #32 + $<string>2 + #32 + $<string>3;
              AddWhereOptimize( $<string>1, $<string>3, $<string>3, ropNEQ);   }
          | WhereExpr RW_AND WhereExpr
            { $<string>$ := $<string>1 + ' AND ' + $<string>3;  }
          | WhereExpr RW_OR WhereExpr
            { $<string>$ := $<string>1 + ' OR ' + $<string>3;   }
          | RW_NOT WhereExpr            { $<string>$ := ' NOT ' + $<string>2; }
          | _LPAREN WhereExpr _RPAREN   { $<string>$ := $<string>1 + $<string>2 + $<string>3; }
          | AnyAllSubquery              { $<string>$ := Format('(Subquery %d)', [CurrentAnalizer.SubqueryList.Count-1]); }
          ;

IsNull : DefineField RW_IS RW_NULL         { $<string>$ := Format('ISNULL(%s,TRUE)', [$<string>1]); }
       | DefineField RW_IS RW_NOT RW_NULL  { $<string>$ := Format('ISNULL(%s,FALSE)', [$<string>1]); }
       ;

InOperator : RW_IN
             { fIsNotInList := False; }
           | RW_NOT RW_IN
             { $<string>$ := $<string>1 + #32 + $<string>2 ; fIsNotInList := True; }
           ;

InPredicate : DefineConstant
              {fInPredicateList.Add( $<string>1 ); }
            | InPredicate _COMA DefineConstant
              { fInPredicateList.Add( $<string>3 );}
            ;

DefineConstant : _UINTEGER
               | _NUMERIC
               | _STRING
               | RW_TRUE
                 { if CurrentAnalizer.xQuery.WithDummies then
                     $<string>$ := 'DummyBoolean(True)'
                   else
                     $<string>$ := 'True' ;
                 }
               | RW_FALSE
                 { if CurrentAnalizer.xQuery.WithDummies then
                     $<string>$ := 'DummyBoolean(False)'
                   else
                     $<string>$ := 'False';
                 }
               | DefineParam
               ;

WhereListParam : WhereExpr
                 | WhereListParam _COMA WhereExpr
                   {$<string>$:= $<string>1 + ', ' + $<string>3;}
                 ;

WhereClause : /* empty */
            | RW_WHERE WhereExpr
              { CurrentAnalizer.WhereStr := $<string>2; }
            ;

/*  a user defined range */
UserDefRangeClause : /* empty */
                     | RW_SET RW_RANGE RW_FOR RW_FIELDS RangeFieldList RW_FROM FromRangeValues RW_TO ToRangeValues RW_USING RW_INDEX _STRING
                     { CurrentAnalizer.UserDefinedRange.UsingIndex:= GetString($<string>12); }
                     ;

RangeFieldList : _STRING
                 { CurrentAnalizer.UserDefinedRange.ForFields.Add(GetString($<string>1)); }
               | RangeFieldList _COMA _STRING
                 { CurrentAnalizer.UserDefinedRange.ForFields.Add(GetString($<string>3)); }
               ;

FromRangeValues : RangeConstant
                    { CurrentAnalizer.UserDefinedRange.StartValues.Add( $<string>1 );}
                  | FromRangeValues _COMA RangeConstant
                    { CurrentAnalizer.UserDefinedRange.StartValues.Add( $<string>3 );}
                  ;

ToRangeValues : RangeConstant
                  { CurrentAnalizer.UserDefinedRange.EndValues.Add( $<string>1 );}
                | ToRangeValues _COMA RangeConstant
                  { CurrentAnalizer.UserDefinedRange.EndValues.Add( $<string>3 );}
                ;

RangeConstant : _STRING
               | _UINTEGER
               | _NUMERIC
               | DefineParam
               ;

AnyAllSubquery : Subquery         {CurrentAnalizer.SubqueryKindList.Add( Pointer(skAny) );}
                | RW_ANY Subquery  {CurrentAnalizer.SubqueryKindList.Add( Pointer(skAny) );}
                | RW_ALL Subquery  {CurrentAnalizer.SubqueryKindList.Add( Pointer(skAll) );}
                ;

Subquery : _LPAREN FirstSubquery EndSubquery
         | _LPAREN FirstSubquery RW_UNION SecondSelectStatement EndSubquery
           { TSqlAnalizer( CurrentAnalizer.SubqueryList[0] ).Statement:= ssUnion; }
         ;

FirstSubquery : SubqSelectClause FromClause WhereClause
                 UserDefRangeClause GroupByClause OrderByClause
               ;

/* start a new Subquery */
SubqSelectClause : SubqSelectCase _MULT
                     { CurrentAnalizer.DoSelectAll := True; }
                   | SubqSelectCase ExprFieldList
                   ;

SubqSelectCase : StartSubquery
                 | StartSubquery RW_DISTINCT
                   { CurrentAnalizer.IsDistinct := True; }
                 ;

StartSubquery : SubqTOPNSelect RW_SELECT SubqTOPNGroupBy
                 { CreateNewSubquery;
                   CurrentAnalizer.TopNInSelect:= FtempTopNInSelect;
                   CurrentAnalizer.TopNInGroupBy:= FtempTopNInGroupBy;
                 }
               ;

SubqTOPNSelect : /* empty */
                   { FtempTopNInSelect:= 0; }
                 | RW_TOP _UINTEGER
                   { FtempTopNInSelect:= StrToInt( $<string>2 ) ; }
                 ;

SubqTOPNGroupBy : /* empty */
                   { FtempTopNInGroupBy:= 0; }
                  | RW_TOP _UINTEGER
                    { FtempTopNInGroupBy:= StrToInt( $<string>2 ) ; }
                  ;

EndSubquery : _RPAREN   { Self.fCurrAnalizer := Self.fCurrAnalizer.ParentAnalizer; }
             ;

EscapeCharacter : /* empty */        {fEscapeChar := '';}
                 | RW_ESCAPE _STRING  {fEscapeChar := GetString( $<string>2 );}
                 ;

/* GROUP BY clause */

GroupByClause : /* empty */
               | RW_GROUP RW_BY ListFieldsGroup HavingPredicate
               ;


ListFieldsGroup : DefineFieldIndex                         {AddGroupBy( $<string>1 );}
                  | ListFieldsGroup _COMA DefineFieldIndex {AddGroupBy( $<string>3 );}
                  ;

DefineFieldIndex : _IDENTIFIER      { $<string>$ := Format('\f"%s"', [$<string>1]); }
                   | QualifiedField
                   | _UINTEGER        { $<string>$ := $<string>1; }
                   ;

HavingPredicate : /* empty */
                 | RW_HAVING SelectExpr
                   { AddHavingColumn( $<string>2 );
                     CurrentAnalizer.HavingCol := CurrentAnalizer.ColumnList.Count-1; }
                 ;

/* ORDER BY clause */

OrderByClause : /* empty */
               | RW_ORDER RW_BY ListFieldsOrder
               ;

ListFieldsOrder : DefineFieldsOrder
                  | ListFieldsOrder _COMA DefineFieldsOrder
                  ;

DefineFieldsOrder : DefineFieldIndex           {AddOrderBy( $<string>1, False );}
                    | DefineFieldIndex RW_ASC    {AddOrderBy( $<string>1, False );}
                    | DefineFieldIndex RW_DESC   {AddOrderBy( $<string>1, True );}
                    ;

/* end of select */
EndStatement : /* empty */
              | _SEMICOLON
              ;

/* UPDATE statement */
UpdateStatement : RW_UPDATE TableIdentifier RW_SET ListUpdateColumns
                     WhereClause EndStatement
                   { CurrentAnalizer.UpdateColumnList.SyntaxUsed:= 0; }
                 | RW_UPDATE TableIdentifier RW_SET ListUpdateColumns
                     _EQ Subquery EndStatement
                   { // NOTA: hay un error en esta sintaxis.
                     CurrentAnalizer.UpdateColumnList.SyntaxUsed:= 1; }
                 ;

ListUpdateColumns : update_column
                  | ListUpdateColumns _COMA update_column
                  ;

update_column : _IDENTIFIER _EQ WhereExpr {AddUpdateColumn($<string>1, $<string>3);}
              | _IDENTIFIER _EQ RW_NULL {AddUpdateColumn($<string>1,'');}
              ;

/* DELETE statement */
DeleteStatement : RW_DELETE RW_FROM TableIdentifier WhereClause EndStatement
                 ;

/* INSERT statement */
InsertStatement : InsertStatementList EndStatement
                 ;

InsertStatementList : InsertOneRecord                        { Inc(fNumInserts); }
                      | InsertStatementList InsertOneRecord   { Inc(fNumInserts); }
                      ;

InsertOneRecord : RW_INSERT RW_INTO InsertIdentifier InsertColumnsList
                    OptionalValues InsertValuesList
                      { if fIsFullPath then
                          CurrentInsertItem.TableName := Copy($<string>3,2,Length($<string>3)-2)
                        else
                          CurrentInsertItem.TableName := $<string>3;
                        CurrentInsertItem.IsFullPath  := self.fIsFullPath ;
                      }
                  ;

OptionalValues : /* empty */
               | RW_VALUES
               ;

InsertIdentifier : _IDENTIFIER     { fIsFullPath := false; }
                 | _STRING
                   { fIsFullPath := true; }
                 ;


InsertColumnsList : _LPAREN ColumnsList _RPAREN
                  | _LPAREN _MULT _RPAREN
                   ;

/* define the fields to update */
ColumnsList : _IDENTIFIER                       {CurrentInsertItem.FieldNames.Add($<string>1);}
             | ColumnsList _COMA _IDENTIFIER    {CurrentInsertItem.FieldNames.Add($<string>3);}
             ;

InsertValuesList : _LPAREN ValuesList _RPAREN
                 | Subquery
                 ;

/* define the values to store in fields */
ValuesList : InsertValue                    {CurrentInsertItem.ExprList.Add( $<string>1 );}
            | ValuesList _COMA InsertValue  {CurrentInsertItem.ExprList.Add( $<string>3 );}
            ;

InsertValue : RW_NULL
             | WhereExpr
             ;


/* ALTER TABLE STATEMENT */

AlterTableStatement : AlterTableList EndStatement

AlterTableList : AlterOneTable
                | AlterTableList AlterOneTable
                ;

AlterOneTable : RW_ALTER RW_TABLE _STRING AlterList
                 {SetAlterTableName( GetString( $<string>3 ) ); }
               ;

AlterList : AlterOneField
           | AlterList _COMA AlterOneField
           ;

AlterOneField : RW_ADD AlterColumn _IDENTIFIER FieldType {AddAlterField($<string>3,false);}
               | RW_DROP AlterColumn _IDENTIFIER           {AddAlterField($<string>3,true);}
               ;

AlterColumn : /* empty */
             | RW_COLUMN
             ;


/* CREATE TABLE STATEMENT */
CreateTableStatement : CreateTableList EndStatement
                      ;

CreateTableList : CreateOneTable                   {Inc(FNumTables);}
                 | CreateTableList CreateOneTable  {Inc(FNumTables);}
                 ;

CreateOneTable : RW_CREATE RW_TABLE _STRING _LPAREN CreateList PrimaryKey _RPAREN
                  {SetTableName( GetString( $<string>3 ) );}
                ;

/* the type of fields that can be created */
FieldType : RW_CHAR _LPAREN _UINTEGER _RPAREN   {SetFieldParams(RW_CHAR,0,0,StrToInt($<string>3),0);}
           | RW_INTEGER                         {SetFieldParams(RW_INTEGER,0,0,0,0);}
           | RW_SMALLINT                        {SetFieldParams(RW_SMALLINT,0,0,0,0);}
           | RW_BOOLEAN                         {SetFieldParams(RW_BOOLEAN,0,0,0,0);}
           | RW_DATE                            {SetFieldParams(RW_DATE,0,0,0,0);}
           | RW_TIME                            {SetFieldParams(RW_TIME,0,0,0,0);}
           | RW_DATETIME                        {SetFieldParams(RW_DATETIME,0,0,0,0);}
           | RW_MONEY                           {SetFieldParams(RW_MONEY,0,0,0,0);}
           | RW_FLOAT                           {SetFieldParams(RW_FLOAT,0,0,0,0);}
           | RW_FLOAT _LPAREN _UINTEGER _RPAREN {SetFieldParams(RW_FLOAT,StrToInt($<string>3),0,0,0);}
           | RW_FLOAT _LPAREN _UINTEGER _COMA _UINTEGER _RPAREN {SetFieldParams(RW_FLOAT,StrToInt($<string>3),StrToInt($<string>5),0,0);}
           | RW_AUTOINC                         {SetFieldParams(RW_AUTOINC,0,0,0,0);}
           | RW_BLOB _LPAREN _UINTEGER _RPAREN  {SetFieldParams(RW_BLOB,0,0,0,StrToInt($<string>3));}
           ;

CreateList : _IDENTIFIER FieldType                     {AddCreateField($<string>1);}
            | CreateList _COMA _IDENTIFIER FieldType   {AddCreateField($<string>3);}
            ;

PrimaryKey : /* empty */
            | RW_PRIMARY RW_KEY _LPAREN CreateFieldList _RPAREN
            ;

CreateFieldList : _IDENTIFIER                           { AddPrimaryKeyField( $<string>1 ); }
                  | CreateFieldList _COMA _IDENTIFIER   { AddPrimaryKeyField( $<string>3 ); }
                  ;

/* CREATE INDEX statement */
CreateIndexStatement : RW_CREATE IndexUnique IndexOrder RW_INDEX _IDENTIFIER RW_ON _STRING _LPAREN IndexFieldList _RPAREN EndStatement
                        { CurrentAnalizer.IndexName  := $<string>5;
                          CurrentAnalizer.IndexTable := GetString( $<string>7 ); }
                      ;

IndexUnique : /* empty */
             | RW_UNIQUE  { CurrentAnalizer.IndexUnique := True; }
             ;

IndexOrder : /* empty */
            | RW_ASC
            | RW_DESC      { CurrentAnalizer.IndexDescending := True; }
            ;

IndexFieldList : _IDENTIFIER   {CurrentAnalizer.IndexColumnList.Add($<string>1);}
                 | IndexFieldList _COMA _IDENTIFIER  {CurrentAnalizer.IndexColumnList.Add($<string>3);}
                 ;

/* DROP TABLE statement */
DropTableStatement : RW_DROP RW_TABLE _STRING EndStatement
                      {CurrentAnalizer.IndexTable:= GetString( $<string>3 );}
                    ;

/* DROP INDEX statement */
DropIndexStatement : RW_DROP RW_INDEX _STRING _IDENTIFIER EndStatement
                     { CurrentAnalizer.IndexTable:= GetString( $<string>3 );
                       CurrentAnalizer.IndexName := $<string>4; }
                    ;

/* TRANSFORM...PIVOT statement */
TransformStatement : RW_TRANSFORM TransfAggregateList SelectStatement RW_PIVOT SelectExpr PivotInPredicate EndStatement
                      { CurrentAnalizer.PivotStr := $<string>5; }
                    ;

/* this is needed in order to reduce this column first */
TransfAggregate : SelectExpr  { AddColumn( $<string>1, True ); }
                 | RW_CAST _LPAREN SelectExpr RW_AS DataType _RPAREN { AddColumn( $<string>3, True ); }
                 ;

TransfAggregateList : TransfAggregate
                      | TransfAggregateList _COMA TransfAggregate
                      ;

PivotInPredicate : /* empty */
                   | RW_IN _LPAREN PivotInList _RPAREN
                   | RW_IN Subquery    { CurrentAnalizer.SubqueryInPivotPredicate:= true ;}
                   ;

PivotInList : DefineConstant
                {CurrentAnalizer.PivotInList.Add( RemoveStrDelim( $<string>1));}
              | PivotInList _COMA DefineConstant
                {CurrentAnalizer.PivotInList.Add( RemoveStrDelim( $<string>3));}
               ;

/* this second select clause is for the UNION statement */

SecondSelectStatement : SecondSelectClause FromClause WhereClause
                          UserDefRangeClause GroupByClause OrderByClause
                        ;

/* start a new select statement */
SecondSelectClause : SecondSelectCase _MULT              {CurrentAnalizer.DoSelectAll := True;}
                     | SecondSelectCase ExprFieldList
                     ;

SecondSelectCase : SecondSelect
                   | SecondSelect RW_DISTINCT
                     { CurrentAnalizer.IsDistinct := True; }
                   ;

/* create the second select */
SecondSelect : RW_SELECT
                { CurrentAnalizer.UnionAnalizer :=
                    TSqlAnalizer.Create( CurrentAnalizer.ParentAnalizer, fAnalizer.xQuery );
                  Self.fCurrAnalizer := CurrentAnalizer.UnionAnalizer; }
              ;

PackTableStatement : RW_PACK RW_TABLE ListTables
                    ;

ZapTableStatement : RW_ZAP RW_TABLE ListTables
                   ;

ReindexTableStatement : RW_REINDEX RW_TABLE ListTables

%%