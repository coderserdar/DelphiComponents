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
{   The Original Code is XQLex.pas                               }
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
       rword: string;
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

uses xqConsts, xquery, CnvStrUtils;

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





procedure TXQLexer.yyaction ( yyruleno : Integer );
  (* local definitions: *)

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

begin
  GetyyText (yylval.yystring);
  (* actions: *)
  case yyruleno of
  1:

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
  2:
               returni( _COMMENT );
  3:
               returni( _COMMENT );
  4:
               returni( _COMMENT );
  5:

  begin
    // extended identifier
    //yylval.yystring := yylval.yystring;
    returni( _IDENTIFIER );
  end;

  6:
                     returni( _NUMERIC );

  7:
                     returni( _UINTEGER );

  8:

  begin
    Val(yylval.yystring,value,code);
    if code=0 then
    begin
      yylval.yystring:= IntToStr(value);
      returni(_NUMERIC);
    end else
      returni(_ILLEGAL);
  end;

  9:

  begin
    c := get_char;
    unget_char(c);
    if c = #39 then
      yymore
    else
      returni( _STRING );
  end;
  10:

  begin
    c := get_char;
    unget_char(c);
    if c = #34 then
      yymore
    else
      returni( _STRING );
  end;
  11:

  begin
    // previously this "#"{DIGIT}{1,2}"/"{DIGIT}{1,2}"/"({DIGIT}{2}|{DIGIT}{4})"#"
    if yytextlen >= 10 then
    begin
      yylval.yystring := ReturnDate( Copy( yylval.yystring, 2, yyTextLen - 2) );
    end;
  end;
  12:
     returni( _COMA );
  13:
     returni( _LPAREN );
  14:
     returni( _RPAREN );
  15:
     returni( _LSQUARE );
  16:
     returni( _RSQUARE );
  17:
     returni( _GT );
  18:
     returni( _LT );
  19:
     returni( _PERIOD );
  20:
     returni( _COLON );
  21:
     returni( _SEMICOLON );
  22:
     returni( _EQ );
  23:
     returni( _MULT );
  24:
     returni( _PLUS );
  25:
     returni( _SUB );
  26:
     returni( _EXP );
  27:
     returni( _DIV );
  28:
     returni( _NEQ );
  29:
     returni( _GE );
  30:
     returni( _LE );
  31:

  begin
    yylval.yystring := '+';
    returni( _PLUS );
  end;
  32:
     returni( _BLANK );
  33:
     returni( _NEWLINE );
  34:
     returni( _TAB );
  35:
     returni( _ILLEGAL );
  end;
end(*yyaction*);

function TXQLexer.yylex : Integer;
(* DFA table: *)

type YYTRec = record
                cc : set of AnsiChar; { patched by ccy }
                s  : SmallInt;
              end;

const

yynmarks   = 66;
yynmatches = 66;
yyntrans   = 108;
yynstates  = 61;

yyk : array [1..yynmarks] of SmallInt = (
  { 0: }
  { 1: }
  { 2: }
  1,
  35,
  { 3: }
  27,
  35,
  { 4: }
  15,
  35,
  { 5: }
  25,
  35,
  { 6: }
  7,
  35,
  { 7: }
  19,
  35,
  { 8: }
  35,
  { 9: }
  35,
  { 10: }
  35,
  { 11: }
  35,
  { 12: }
  12,
  35,
  { 13: }
  13,
  35,
  { 14: }
  14,
  35,
  { 15: }
  16,
  35,
  { 16: }
  17,
  35,
  { 17: }
  18,
  35,
  { 18: }
  20,
  35,
  { 19: }
  21,
  35,
  { 20: }
  22,
  35,
  { 21: }
  23,
  35,
  { 22: }
  24,
  35,
  { 23: }
  26,
  35,
  { 24: }
  35,
  { 25: }
  32,
  35,
  { 26: }
  33,
  { 27: }
  34,
  35,
  { 28: }
  35,
  { 29: }
  1,
  { 30: }
  { 31: }
  { 32: }
  { 33: }
  5,
  { 34: }
  { 35: }
  7,
  { 36: }
  6,
  { 37: }
  6,
  { 38: }
  8,
  { 39: }
  { 40: }
  9,
  { 41: }
  { 42: }
  10,
  { 43: }
  { 44: }
  11,
  { 45: }
  29,
  { 46: }
  28,
  { 47: }
  30,
  { 48: }
  31,
  { 49: }
  { 50: }
  5,
  { 51: }
  { 52: }
  { 53: }
  2,
  { 54: }
  { 55: }
  3,
  5,
  { 56: }
  4,
  { 57: }
  { 58: }
  3
  { 59: }
  { 60: }
);

yym : array [1..yynmatches] of SmallInt = (
{ 0: }
{ 1: }
{ 2: }
  1,
  35,
{ 3: }
  27,
  35,
{ 4: }
  15,
  35,
{ 5: }
  25,
  35,
{ 6: }
  7,
  35,
{ 7: }
  19,
  35,
{ 8: }
  35,
{ 9: }
  35,
{ 10: }
  35,
{ 11: }
  35,
{ 12: }
  12,
  35,
{ 13: }
  13,
  35,
{ 14: }
  14,
  35,
{ 15: }
  16,
  35,
{ 16: }
  17,
  35,
{ 17: }
  18,
  35,
{ 18: }
  20,
  35,
{ 19: }
  21,
  35,
{ 20: }
  22,
  35,
{ 21: }
  23,
  35,
{ 22: }
  24,
  35,
{ 23: }
  26,
  35,
{ 24: }
  35,
{ 25: }
  32,
  35,
{ 26: }
  33,
{ 27: }
  34,
  35,
{ 28: }
  35,
{ 29: }
  1,
{ 30: }
{ 31: }
{ 32: }
{ 33: }
  5,
{ 34: }
{ 35: }
  7,
{ 36: }
  6,
{ 37: }
  6,
{ 38: }
  8,
{ 39: }
{ 40: }
  9,
{ 41: }
{ 42: }
  10,
{ 43: }
{ 44: }
  11,
{ 45: }
  29,
{ 46: }
  28,
{ 47: }
  30,
{ 48: }
  31,
{ 49: }
{ 50: }
  5,
{ 51: }
{ 52: }
{ 53: }
  2,
{ 54: }
{ 55: }
  3,
  5,
{ 56: }
  4,
{ 57: }
{ 58: }
  3
{ 59: }
{ 60: }
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11..#31,'!','%','&','?','@','\','`',
            '{','}'..#127 ]; s: 28),
  ( cc: [ #9 ]; s: 27),
  ( cc: [ #10 ]; s: 26),
  ( cc: [ ' ' ]; s: 25),
  ( cc: [ '"' ]; s: 10),
  ( cc: [ '#' ]; s: 11),
  ( cc: [ '$' ]; s: 8),
  ( cc: [ '''' ]; s: 9),
  ( cc: [ '(' ]; s: 13),
  ( cc: [ ')' ]; s: 14),
  ( cc: [ '*' ]; s: 21),
  ( cc: [ '+' ]; s: 22),
  ( cc: [ ',' ]; s: 12),
  ( cc: [ '-' ]; s: 5),
  ( cc: [ '.' ]; s: 7),
  ( cc: [ '/' ]; s: 3),
  ( cc: [ '0'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 18),
  ( cc: [ ';' ]; s: 19),
  ( cc: [ '<' ]; s: 17),
  ( cc: [ '=' ]; s: 20),
  ( cc: [ '>' ]; s: 16),
  ( cc: [ 'A'..'Z','_','a'..'z',#128..#255 ]; s: 2),
  ( cc: [ '[' ]; s: 4),
  ( cc: [ ']' ]; s: 15),
  ( cc: [ '^' ]; s: 23),
  ( cc: [ '|' ]; s: 24),
{ 1: }
  ( cc: [ #1..#8,#11..#31,'!','%','&','?','@','\','`',
            '{','}'..#127 ]; s: 28),
  ( cc: [ #9 ]; s: 27),
  ( cc: [ #10 ]; s: 26),
  ( cc: [ ' ' ]; s: 25),
  ( cc: [ '"' ]; s: 10),
  ( cc: [ '#' ]; s: 11),
  ( cc: [ '$' ]; s: 8),
  ( cc: [ '''' ]; s: 9),
  ( cc: [ '(' ]; s: 13),
  ( cc: [ ')' ]; s: 14),
  ( cc: [ '*' ]; s: 21),
  ( cc: [ '+' ]; s: 22),
  ( cc: [ ',' ]; s: 12),
  ( cc: [ '-' ]; s: 5),
  ( cc: [ '.' ]; s: 7),
  ( cc: [ '/' ]; s: 3),
  ( cc: [ '0'..'9' ]; s: 6),
  ( cc: [ ':' ]; s: 18),
  ( cc: [ ';' ]; s: 19),
  ( cc: [ '<' ]; s: 17),
  ( cc: [ '=' ]; s: 20),
  ( cc: [ '>' ]; s: 16),
  ( cc: [ 'A'..'Z','_','a'..'z',#128..#255 ]; s: 2),
  ( cc: [ '[' ]; s: 4),
  ( cc: [ ']' ]; s: 15),
  ( cc: [ '^' ]; s: 23),
  ( cc: [ '|' ]; s: 24),
{ 2: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z',#128..#255 ]; s: 29),
{ 3: }
  ( cc: [ '*' ]; s: 30),
{ 4: }
  ( cc: [ #1..')','+'..'Z','\','^'..#255 ]; s: 32),
  ( cc: [ '*' ]; s: 31),
  ( cc: [ ']' ]; s: 33),
{ 5: }
  ( cc: [ '-' ]; s: 34),
{ 6: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'9' ]; s: 35),
{ 7: }
  ( cc: [ '0'..'9' ]; s: 37),
{ 8: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 38),
{ 9: }
  ( cc: [ #1..'&','('..#255 ]; s: 39),
  ( cc: [ '''' ]; s: 40),
{ 10: }
  ( cc: [ #1..'!','#'..#255 ]; s: 41),
  ( cc: [ '"' ]; s: 42),
{ 11: }
  ( cc: [ #1..'"','$'..#255 ]; s: 43),
  ( cc: [ '#' ]; s: 44),
{ 12: }
{ 13: }
{ 14: }
{ 15: }
{ 16: }
  ( cc: [ '=' ]; s: 45),
{ 17: }
  ( cc: [ '=' ]; s: 47),
  ( cc: [ '>' ]; s: 46),
{ 18: }
{ 19: }
{ 20: }
{ 21: }
{ 22: }
{ 23: }
{ 24: }
  ( cc: [ '|' ]; s: 48),
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z',#128..#255 ]; s: 29),
{ 30: }
  ( cc: [ #1..')','+'..#255 ]; s: 30),
  ( cc: [ '*' ]; s: 49),
{ 31: }
  ( cc: [ #1..')','+'..'Z','\','^'..#255 ]; s: 31),
  ( cc: [ '*' ]; s: 60),
  ( cc: [ '[' ]; s: 59),
  ( cc: [ ']' ]; s: 50),
{ 32: }
  ( cc: [ #1..'Z','\','^'..#255 ]; s: 32),
  ( cc: [ ']' ]; s: 33),
{ 33: }
{ 34: }
  ( cc: [ #1..',','.'..#255 ]; s: 34),
  ( cc: [ '-' ]; s: 51),
{ 35: }
  ( cc: [ '.' ]; s: 36),
  ( cc: [ '0'..'9' ]; s: 35),
{ 36: }
  ( cc: [ '0'..'9' ]; s: 36),
{ 37: }
  ( cc: [ '0'..'9' ]; s: 37),
  ( cc: [ 'E','e' ]; s: 52),
{ 38: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 38),
{ 39: }
  ( cc: [ #1..'&','('..#255 ]; s: 39),
  ( cc: [ '''' ]; s: 40),
{ 40: }
{ 41: }
  ( cc: [ #1..'!','#'..#255 ]; s: 41),
  ( cc: [ '"' ]; s: 42),
{ 42: }
{ 43: }
  ( cc: [ #1..'"','$'..#255 ]; s: 43),
  ( cc: [ '#' ]; s: 44),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
  ( cc: [ '/' ]; s: 53),
{ 50: }
  ( cc: [ #1..')','+'..#255 ]; s: 59),
  ( cc: [ '*' ]; s: 54),
{ 51: }
  ( cc: [ '-' ]; s: 56),
{ 52: }
  ( cc: [ '+','-' ]; s: 57),
{ 53: }
{ 54: }
  ( cc: [ ']' ]; s: 58),
{ 55: }
{ 56: }
{ 57: }
  ( cc: [ '0'..'9' ]; s: 36),
{ 58: }
{ 59: }
  ( cc: [ #1..')','+'..#255 ]; s: 59),
  ( cc: [ '*' ]; s: 54),
{ 60: }
  ( cc: [ #1..'Z','\','^'..#255 ]; s: 32),
  ( cc: [ ']' ]; s: 55)
);

yykl : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 14,
{ 10: } 15,
{ 11: } 16,
{ 12: } 17,
{ 13: } 19,
{ 14: } 21,
{ 15: } 23,
{ 16: } 25,
{ 17: } 27,
{ 18: } 29,
{ 19: } 31,
{ 20: } 33,
{ 21: } 35,
{ 22: } 37,
{ 23: } 39,
{ 24: } 41,
{ 25: } 42,
{ 26: } 44,
{ 27: } 45,
{ 28: } 47,
{ 29: } 48,
{ 30: } 49,
{ 31: } 49,
{ 32: } 49,
{ 33: } 49,
{ 34: } 50,
{ 35: } 50,
{ 36: } 51,
{ 37: } 52,
{ 38: } 53,
{ 39: } 54,
{ 40: } 54,
{ 41: } 55,
{ 42: } 55,
{ 43: } 56,
{ 44: } 56,
{ 45: } 57,
{ 46: } 58,
{ 47: } 59,
{ 48: } 60,
{ 49: } 61,
{ 50: } 61,
{ 51: } 62,
{ 52: } 62,
{ 53: } 62,
{ 54: } 63,
{ 55: } 63,
{ 56: } 65,
{ 57: } 66,
{ 58: } 66,
{ 59: } 67,
{ 60: } 67
);

yykh : array [0..yynstates-1] of SmallInt = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 12,
{ 8: } 13,
{ 9: } 14,
{ 10: } 15,
{ 11: } 16,
{ 12: } 18,
{ 13: } 20,
{ 14: } 22,
{ 15: } 24,
{ 16: } 26,
{ 17: } 28,
{ 18: } 30,
{ 19: } 32,
{ 20: } 34,
{ 21: } 36,
{ 22: } 38,
{ 23: } 40,
{ 24: } 41,
{ 25: } 43,
{ 26: } 44,
{ 27: } 46,
{ 28: } 47,
{ 29: } 48,
{ 30: } 48,
{ 31: } 48,
{ 32: } 48,
{ 33: } 49,
{ 34: } 49,
{ 35: } 50,
{ 36: } 51,
{ 37: } 52,
{ 38: } 53,
{ 39: } 53,
{ 40: } 54,
{ 41: } 54,
{ 42: } 55,
{ 43: } 55,
{ 44: } 56,
{ 45: } 57,
{ 46: } 58,
{ 47: } 59,
{ 48: } 60,
{ 49: } 60,
{ 50: } 61,
{ 51: } 61,
{ 52: } 61,
{ 53: } 62,
{ 54: } 62,
{ 55: } 64,
{ 56: } 65,
{ 57: } 65,
{ 58: } 66,
{ 59: } 66,
{ 60: } 66
);

yyml : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 11,
{ 8: } 13,
{ 9: } 14,
{ 10: } 15,
{ 11: } 16,
{ 12: } 17,
{ 13: } 19,
{ 14: } 21,
{ 15: } 23,
{ 16: } 25,
{ 17: } 27,
{ 18: } 29,
{ 19: } 31,
{ 20: } 33,
{ 21: } 35,
{ 22: } 37,
{ 23: } 39,
{ 24: } 41,
{ 25: } 42,
{ 26: } 44,
{ 27: } 45,
{ 28: } 47,
{ 29: } 48,
{ 30: } 49,
{ 31: } 49,
{ 32: } 49,
{ 33: } 49,
{ 34: } 50,
{ 35: } 50,
{ 36: } 51,
{ 37: } 52,
{ 38: } 53,
{ 39: } 54,
{ 40: } 54,
{ 41: } 55,
{ 42: } 55,
{ 43: } 56,
{ 44: } 56,
{ 45: } 57,
{ 46: } 58,
{ 47: } 59,
{ 48: } 60,
{ 49: } 61,
{ 50: } 61,
{ 51: } 62,
{ 52: } 62,
{ 53: } 62,
{ 54: } 63,
{ 55: } 63,
{ 56: } 65,
{ 57: } 66,
{ 58: } 66,
{ 59: } 67,
{ 60: } 67
);

yymh : array [0..yynstates-1] of SmallInt = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 4,
{ 4: } 6,
{ 5: } 8,
{ 6: } 10,
{ 7: } 12,
{ 8: } 13,
{ 9: } 14,
{ 10: } 15,
{ 11: } 16,
{ 12: } 18,
{ 13: } 20,
{ 14: } 22,
{ 15: } 24,
{ 16: } 26,
{ 17: } 28,
{ 18: } 30,
{ 19: } 32,
{ 20: } 34,
{ 21: } 36,
{ 22: } 38,
{ 23: } 40,
{ 24: } 41,
{ 25: } 43,
{ 26: } 44,
{ 27: } 46,
{ 28: } 47,
{ 29: } 48,
{ 30: } 48,
{ 31: } 48,
{ 32: } 48,
{ 33: } 49,
{ 34: } 49,
{ 35: } 50,
{ 36: } 51,
{ 37: } 52,
{ 38: } 53,
{ 39: } 53,
{ 40: } 54,
{ 41: } 54,
{ 42: } 55,
{ 43: } 55,
{ 44: } 56,
{ 45: } 57,
{ 46: } 58,
{ 47: } 59,
{ 48: } 60,
{ 49: } 60,
{ 50: } 61,
{ 51: } 61,
{ 52: } 61,
{ 53: } 62,
{ 54: } 62,
{ 55: } 64,
{ 56: } 65,
{ 57: } 65,
{ 58: } 66,
{ 59: } 66,
{ 60: } 66
);

yytl : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 28,
{ 2: } 55,
{ 3: } 56,
{ 4: } 57,
{ 5: } 60,
{ 6: } 61,
{ 7: } 63,
{ 8: } 64,
{ 9: } 65,
{ 10: } 67,
{ 11: } 69,
{ 12: } 71,
{ 13: } 71,
{ 14: } 71,
{ 15: } 71,
{ 16: } 71,
{ 17: } 72,
{ 18: } 74,
{ 19: } 74,
{ 20: } 74,
{ 21: } 74,
{ 22: } 74,
{ 23: } 74,
{ 24: } 74,
{ 25: } 75,
{ 26: } 75,
{ 27: } 75,
{ 28: } 75,
{ 29: } 75,
{ 30: } 76,
{ 31: } 78,
{ 32: } 82,
{ 33: } 84,
{ 34: } 84,
{ 35: } 86,
{ 36: } 88,
{ 37: } 89,
{ 38: } 91,
{ 39: } 92,
{ 40: } 94,
{ 41: } 94,
{ 42: } 96,
{ 43: } 96,
{ 44: } 98,
{ 45: } 98,
{ 46: } 98,
{ 47: } 98,
{ 48: } 98,
{ 49: } 98,
{ 50: } 99,
{ 51: } 101,
{ 52: } 102,
{ 53: } 103,
{ 54: } 103,
{ 55: } 104,
{ 56: } 104,
{ 57: } 104,
{ 58: } 105,
{ 59: } 105,
{ 60: } 107
);

yyth : array [0..yynstates-1] of SmallInt = (
{ 0: } 27,
{ 1: } 54,
{ 2: } 55,
{ 3: } 56,
{ 4: } 59,
{ 5: } 60,
{ 6: } 62,
{ 7: } 63,
{ 8: } 64,
{ 9: } 66,
{ 10: } 68,
{ 11: } 70,
{ 12: } 70,
{ 13: } 70,
{ 14: } 70,
{ 15: } 70,
{ 16: } 71,
{ 17: } 73,
{ 18: } 73,
{ 19: } 73,
{ 20: } 73,
{ 21: } 73,
{ 22: } 73,
{ 23: } 73,
{ 24: } 74,
{ 25: } 74,
{ 26: } 74,
{ 27: } 74,
{ 28: } 74,
{ 29: } 75,
{ 30: } 77,
{ 31: } 81,
{ 32: } 83,
{ 33: } 83,
{ 34: } 85,
{ 35: } 87,
{ 36: } 88,
{ 37: } 90,
{ 38: } 91,
{ 39: } 93,
{ 40: } 93,
{ 41: } 95,
{ 42: } 95,
{ 43: } 97,
{ 44: } 97,
{ 45: } 97,
{ 46: } 97,
{ 47: } 97,
{ 48: } 97,
{ 49: } 98,
{ 50: } 100,
{ 51: } 101,
{ 52: } 102,
{ 53: } 102,
{ 54: } 103,
{ 55: } 103,
{ 56: } 103,
{ 57: } 104,
{ 58: } 104,
{ 59: } 106,
{ 60: } 108
);


var yyn : Integer;

label start, scan, action;

begin

start:

  (* initialize: *)

  yynew;

scan:

  (* mark positions and matches: *)

  for yyn := yykl[yystate] to     yykh[yystate] do yymark(yyk[yyn]);
  for yyn := yymh[yystate] downto yyml[yystate] do yymatch(yym[yyn]);

  if yytl[yystate]>yyth[yystate] then goto action; (* dead state *)

  (* get next character: *)

  yyscan;

  (* determine action: *)

  yyn := yytl[yystate];
  while (yyn<=yyth[yystate]) and not CharInSet(yyactchar, yyt[yyn].cc) do inc(yyn);
  if yyn>yyth[yystate] then goto action;
    (* no transition on yyactchar in this state *)

  (* switch to new state: *)

  yystate := yyt[yyn].s;

  goto scan;

action:

  (* execute action: *)

  if yyfind(yyrule) then
    begin
      yyaction(yyrule);
      if yyreject then goto action;
    end
  else if not yydefault and yywrap then
    begin
      yyclear;
      returni(0);
    end;

  if not yydone then goto start;

  yylex := yyretval;

end(*yylex*);

end.