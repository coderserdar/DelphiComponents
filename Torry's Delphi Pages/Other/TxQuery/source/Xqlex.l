
%{

{**********************************************}
{   Lexical analizer for TxQuery component     }
{   Copyright (c) 2003 by Alfonso Moreno       }
{**********************************************}

unit XQLex;

{$I XQ_FLAG.INC}
interface

uses
  SysUtils, Classes, QLexLib, xqYacc;

type
  TxqLexer = Class( TCustomLexer)
  private
    FIsWhereActive : Boolean;
    FDateFormat    : String;
    FIgnoreBadDates: Boolean;
  public

    // utility functions
    function IsKeyword(const id : String; var token : integer) : boolean;
    // Lexer main functions
    function yylex : Integer; override;
    procedure yyaction( yyruleno : integer);
    procedure commenteof;

    property IsWhereActive: Boolean read FIsWhereActive write FIsWhereActive;
    property DateFormat: String read FDateFormat write FDateFormat;
    property IgnoreBadDates: Boolean read FIgnoreBadDates write FIgnoreBadDates;
  end;

//===============================================
// reserved words definition
//===============================================
  type
    TRWord = record
       rword: string[10];
       token: smallint;
    end;

  const
    rwords : array [1..101] of TRword = (
    (rword:'TOP';          token: RW_TOP),
    (rword:'SELECT';       token: RW_SELECT),
    (rword:'DISTINCT';     token: RW_DISTINCT),
    (rword:'TRUE';         token: RW_TRUE),
    (rword:'FALSE';        token: RW_FALSE),
    (rword:'AND';          token: RW_AND),
    (rword:'OR';           token: RW_OR),
    (rword:'NOT';          token: RW_NOT),
    (rword:'FROM';         token: RW_FROM),
    (rword:'WHERE';        token: RW_WHERE),
    (rword:'ORDER';        token: RW_ORDER),
    (rword:'BY';           token: RW_BY),
    (rword:'ASC';          token: RW_ASC),
    (rword:'DESC';         token: RW_DESC),
    (rword:'AS';           token: RW_AS),
    (rword:'INNER';        token: RW_INNER),
    (rword:'OUTER';        token: RW_OUTER),
    (rword:'FULL';         token: RW_FULL),
    (rword:'JOIN';         token: RW_JOIN),
    (rword:'ON';           token: RW_ON),
    (rword:'GROUP';        token: RW_GROUP),
    (rword:'ANY';          token: RW_ANY),
    (rword:'ALL';          token: RW_ALL),
    (rword:'SUM';          token: RW_SUM),
    (rword:'AVG';          token: RW_AVG),
    (rword:'COUNT';        token: RW_COUNT),
    (rword:'MIN';          token: RW_MIN),
    (rword:'MAX';          token: RW_MAX),
    (rword:'STDEV';        token: RW_STDEV),
    (rword:'BETWEEN';      token: RW_BETWEEN),
    (rword:'IN';           token: RW_IN),
    (rword:'LIKE';         token: RW_LIKE),
    (rword:'LEFT';         token: RW_LEFT),
    (rword:'RIGHT';        token: RW_RIGHT),
    (rword:'HAVING';       token: RW_HAVING),
    (rword:'LEADING';      token: RW_LEADING),
    (rword:'TRAILING';     token: RW_TRAILING),
    (rword:'BOTH';         token: RW_BOTH),
    (rword:'TRIM';         token: RW_TRIM),
    (rword:'EXTRACT';      token: RW_EXTRACT),
    (rword:'YEAR';         token: RW_YEAR),
    (rword:'MONTH';        token: RW_MONTH),
    (rword:'DAY';          token: RW_DAY),
    (rword:'HOUR';         token: RW_HOUR),
    (rword:'MINUTE';       token: RW_MINUTE),
    (rword:'SECOND';       token: RW_SECOND),
    (rword:'SUBSTRING';    token: RW_SUBSTRING),
    (rword:'FOR';          token: RW_FOR),
    (rword:'DELETE';       token: RW_DELETE),
    (rword:'UPDATE';       token: RW_UPDATE),
    (rword:'INSERT';       token: RW_INSERT),
    (rword:'INTO';         token: RW_INTO),
    (rword:'VALUES';       token: RW_VALUES),
    (rword:'SET';          token: RW_SET),
    (rword:'CAST';         token: RW_CAST),
    (rword:'CHAR';         token: RW_CHAR),
    (rword:'INTEGER';      token: RW_INTEGER),
    (rword:'BOOLEAN';      token: RW_BOOLEAN),
    (rword:'DATE';         token: RW_DATE),
    (rword:'DATETIME';     token: RW_DATETIME),
    (rword:'TIME';         token: RW_TIME),
    (rword:'FLOAT';        token: RW_FLOAT),
    (rword:'NUMERIC';      token: RW_FLOAT),
    (rword:'ESCAPE';       token: RW_ESCAPE),
    (rword:'CREATE';       token: RW_CREATE),
    (rword:'TABLE';        token: RW_TABLE),
    (rword:'SMALLINT';     token: RW_SMALLINT),
    (rword:'MONEY';        token: RW_MONEY),
    (rword:'AUTOINC';      token: RW_AUTOINC),
    (rword:'PRIMARY';      token: RW_PRIMARY),
    (rword:'KEY';          token: RW_KEY),
    (rword:'BLOB';         token: RW_BLOB),
    (rword:'INDEX';        token: RW_INDEX),
    (rword:'UNIQUE';       token: RW_UNIQUE),
    (rword:'DROP';         token: RW_DROP),
    (rword:'TRANSFORM';    token: RW_TRANSFORM),
    (rword:'PIVOT';        token: RW_PIVOT),
    (rword:'UNION';        token: RW_UNION),
    (rword:'WITH';         token: RW_WITH),
    (rword:'IS';           token: RW_IS),
    (rword:'NULL';         token: RW_NULL),
    (rword:'MOD';          token: RW_MOD),
    (rword:'DIV';          token: RW_IDIV),
    (rword:'SHL';          token: RW_SHL),
    (rword:'SHR';          token: RW_SHR),
    (rword:'ALTER';        token: RW_ALTER),
    (rword:'COLUMN';       token: RW_COLUMN),
    (rword:'ADD';          token: RW_ADD),
    (rword:'APPEND';       token: RW_APPEND),
    (rword:'CASE';         token: RW_CASE),
    (rword:'WHEN';         token: RW_WHEN),
    (rword:'THEN';         token: RW_THEN),
    (rword:'ELSE';         token: RW_ELSE),
    (rword:'END';          token: RW_END),
    (rword:'PACK';         token: RW_PACK),
    (rword:'ZAP';          token: RW_ZAP),
    (rword:'REINDEX';      token: RW_REINDEX),
    (rword:'FIELDS';       token: RW_FIELDS),
    (rword:'USING';        token: RW_USING),
    (rword:'RANGE';        token: RW_RANGE),
    (rword:'TO';           token: RW_TO)

    );

implementation

uses xqConsts, xquery;

function TxqLexer.IsKeyword(const id : string; var token : integer) : boolean;
(* returns corresponding token number in token *)

var
  k : integer;
begin
  Result:= false;
  for k:= Low(rwords) to High(rwords) do
     if AnsiCompareText(id, rwords[k].rword)=0 then
     begin
        Result:= True;
        token := rwords[k].token;
        Exit;
     end;
end;

procedure TxqLexer.commenteof;
begin
  writeln(yyErrorfile, 'unexpected EOF inside comment at line '
                       +intToStr( yylineno));
end;

%}

DIGIT      [0-9]
LOWER      [a-z]
UPPER      [A-Z]
EXTENDED   [\200-\377]
LETTER     ({UPPER}|{LOWER}|{EXTENDED})

UINT       {DIGIT}+
SINT       [+-]{DIGIT}+
HEXA       [$]({DIGIT}|[A-Fa-f])+
ENL        ({UINT}"."{UINT})|({UINT}".")|("."{UINT})
ANL        {ENL}[Ee]{SINT}

%%

   var
      c: char;
      token, code, value: Integer;
      SaveDate: String;

      Function ReturnDate( const ADate: String ): string;
      begin
        SaveDate := ShortDateFormat;
        if Length( Self.FDateFormat ) = 0 then
           Self.FDateFormat := ShortDateFormat;//SDefaultDateFormat;
        ShortDateFormat := Self.FDateFormat;
        try
           Result := FloatToStr( StrToDate( ADate ) );
           if FIsWhereActive then
              Result := 'DummyDate(' + Result + ')';
           ShortDateFormat := SaveDate;
           returni( _NUMERIC );
        except
           if not FIgnoreBadDates then
           begin
              FIgnoreBadDates:= False;
              raise;
           end;
        end;
      end;

("_"|{LETTER})("_"|{LETTER}|{DIGIT})*
  begin
    If AnsiCompareText( yylval.yystring, 'NOW' ) = 0 Then
    Begin
      SaveDate := ShortDateFormat;
      if Length( Self.FDateFormat ) = 0 then
         Self.FDateFormat := ShortDateFormat;//SDefaultDateFormat;
      ShortDateFormat := Self.FDateFormat;
      try
         yylval.yystring := FloatToStr( Now );
         if FIsWhereActive then
            yylval.yystring := 'DummyDate(' + yylval.yystring + ')';
         ShortDateFormat := SaveDate;
         returni( _NUMERIC );
      except
         if not FIgnoreBadDates then
         begin
            FIgnoreBadDates:= False;
            raise;
         end;
      end;
      Exit;
    End;
    if IsKeyword(yylval.yystring, token) then
    begin
      if token = RW_WHERE then
        FIsWhereActive:= True
      else if FIsWhereActive and ( (token = RW_GROUP) or
         (token =RW_ORDER) or (token = RW_SELECT) or (token =RW_PIVOT) ) then
        FIsWhereActive := False;

        returni(token);
    end
    else
      returni(_IDENTIFIER);
  end;
"/*"[^\*]*"*/" returni( _COMMENT );
"[*"[^\*]*"*]" returni( _COMMENT );
"--"[^\-]*"--" returni( _COMMENT );
\[[^\[\]]*\]
  begin
    // extended identifier
    //yylval.yystring := yylval.yystring;
    returni( _IDENTIFIER );
  end;

({ENL})|({ANL})      returni( _NUMERIC );

{UINT}               returni( _UINTEGER );

{HEXA}
  begin
    Val(yylval.yystring,value,code);
    if code=0 then
    begin
      yylval.yystring:= IntToStr(value);
      returni(_NUMERIC);
    end else
      returni(_ILLEGAL);
  end;

\'[^\']*\'
  begin
    c := get_char;
    unget_char(c);
    if c = #39 then
      yymore
    else
      returni( _STRING );
  end;
\"[^\"]*\"
  begin
    c := get_char;
    unget_char(c);
    if c = #34 then
      yymore
    else
      returni( _STRING );
  end;
\#[^\#]*\#
  begin
    // previously this "#"{DIGIT}{1,2}"/"{DIGIT}{1,2}"/"({DIGIT}{2}|{DIGIT}{4})"#"
    if yytextlen >= 10 then
    begin
      yylval.yystring := ReturnDate( Copy( yylval.yystring, 2, yyTextLen - 2) );
    end;
  end;
","  returni( _COMA );
"("  returni( _LPAREN );
")"  returni( _RPAREN );
"["  returni( _LSQUARE );
"]"  returni( _RSQUARE );
">"  returni( _GT );
"<"  returni( _LT );
"."  returni( _PERIOD );
":"  returni( _COLON );
";"  returni( _SEMICOLON );
"="  returni( _EQ );
"*"  returni( _MULT );
"+"  returni( _PLUS );
"-"  returni( _SUB );
"^"  returni( _EXP );
"/"  returni( _DIV );
"<>" returni( _NEQ );
">=" returni( _GE );
"<=" returni( _LE );
"||"
  begin
    yylval.yystring := '+';
    returni( _PLUS );
  end;
[ ]  returni( _BLANK );
[\n] returni( _NEWLINE );
[\t] returni( _TAB );
.    returni( _ILLEGAL );