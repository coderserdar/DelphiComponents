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
{   The Original Code is QExprLex.pas                                      }
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

unit QExprLex;

{$I XQ_FLAG.INC}
interface

uses
  SysUtils, QLexLib, QExprYacc;

type
    TExprLexer = Class(TCustomLexer)
    public
      // utility functions
      function IsKeyword(const id : String; var token : integer) : boolean;
      // Lexer main functions
      function yylex : Integer; override;
      procedure yyaction( yyruleno : integer);
      procedure commenteof;
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
    rwords : array [1..26] of TRword = (
    (rword: 'AND';            token: RW_AND),
    (rword: 'OR';             token: RW_OR),
    (rword: 'XOR';            token: RW_XOR),
    (rword: 'NOT';            token: RW_NOT),
    (rword: 'MOD';            token: RW_MOD),
    (rword: 'TRUE';           token: RW_TRUE),
    (rword: 'FALSE';          token: RW_FALSE),
    (rword: 'LIKE';           token: RW_LIKE),
    (rword: 'STRING';         token: RW_STRING),
    (rword: 'FLOAT';          token: RW_FLOAT),
    (rword: 'INTEGER';        token: RW_INTEGER),
    (rword: 'BOOLEAN';        token: RW_BOOLEAN),
    (rword: 'SHL';            token: RW_SHL),
    (rword: 'SHR';            token: RW_SHR),
    (rword: 'IN';             token: RW_IN),
    (rword: 'BETWEEN';        token: RW_BETWEEN),
    (rword: 'DIV';            token: RW_DIV),
    (rword: 'CASE';           token: RW_CASE),
    (rword: 'WHEN';           token: RW_WHEN),
    (rword: 'THEN';           token: RW_THEN),
    (rword: 'ELSE';           token: RW_ELSE),
    (rword: 'IF';             token: RW_IF),
    (rword: 'CAST';           token: RW_CAST),
    (rword: 'ESCAPE';         token: RW_ESCAPE),
    (rword: 'AS';             token: RW_AS),
    (rword: 'END';            token: RW_END)
    );

implementation

uses CnvStrUtils;

resourcestring
  SDefaultDateFormat = 'm/d/yyyy';

function TExprLexer.IsKeyword(const id : string; var token : integer) : boolean;
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

procedure TExprLexer.commenteof;
begin
  writeln(yyErrorfile, 'unexpected EOF inside comment at line ' +intToStr( yylineno));
end;





procedure TExprLexer.yyaction ( yyruleno : Integer );
  (* local definitions: *)

   var
      c: char;
      token, code, value: Integer;
      SaveDate: String;

begin
  GetyyText (yylval.yystring);
  (* actions: *)
  case yyruleno of
  1:

  if IsKeyword(yylval.yystring, token) then
    returni(token)
  else
    returni(_IDENTIFIER);
  2:

  begin
    // extended identifier for using in fields with same name as reserved word
    yylval.yystring := Copy(yylval.yystring, 2, yyTextLen - 2);
    returni( _IDENTIFIER );
  end;

  3:
                     returni( _NUMERIC );

  4:
                     returni( _UINTEGER );

  5:
                     returni( _SINTEGER );

  6:

  begin
    Val(yylval.yystring, value, code);
    if code=0 then
    begin
      yylval.yystring:= IntToStr(value);
      returni(_NUMERIC);
    end else
      returni(_ILLEGAL);
  end;

  7:

  begin
    c := get_char;
    unget_char(c);
    if c = #39 then
      yymore
    else
      returni( _STRING );
  end;
  8:

  begin
    c := get_char;
    unget_char(c);
    if c = #34 then
      yymore
    else
      returni( _STRING );
  end;
  9:

  if Length( yylval.yystring ) >= 10 then
  begin
    { section to handle dates in the format m/d/yyyy }
    SaveDate := ShortDateFormat;
    ShortDateFormat := SDefaultDateFormat;
    yylval.yystring := FloatToStr(StrToDate(Copy(yylval.yystring, 2, yyTextLen - 2)));
    ShortDateFormat := SaveDate;
    returni(_NUMERIC);
  end;
  10:
     returni( _COMA );
  11:
     returni( _LPAREN );
  12:
     returni( _RPAREN );
  13:
     returni( _GT );
  14:
     returni( _LT );
  15:
     returni( _EQ );
  16:
     returni( _NEQ );
  17:
     returni( _GE );
  18:
     returni( _LE );
  19:
     returni( _PERIOD );
  20:
     returni( _COLON );
  21:
     returni( _MULT );
  22:
     returni( _PLUS );
  23:
     returni( _SUB );
  24:
     returni( _EXP );
  25:
     returni( _DIV );
  26:
               returni( _COMMENT );
  27:
     returni( _BLANK );
  28:
     returni( _NEWLINE );
  29:
     returni( _TAB );
  30:
     returni( _ILLEGAL );
  end;
end(*yyaction*);

function TExprLexer.yylex : Integer;
(* DFA table: *)

type YYTRec = record
                cc : set of AnsiChar; { patched by ccy }
                s  : SmallInt;
              end;

const

yynmarks   = 55;
yynmatches = 55;
yyntrans   = 89;
yynstates  = 48;

yyk : array [1..yynmarks] of SmallInt = (
  { 0: }
  { 1: }
  { 2: }
  1,
  30,
  { 3: }
  30,
  { 4: }
  23,
  30,
  { 5: }
  4,
  30,
  { 6: }
  19,
  30,
  { 7: }
  30,
  { 8: }
  30,
  { 9: }
  30,
  { 10: }
  30,
  { 11: }
  10,
  30,
  { 12: }
  11,
  30,
  { 13: }
  12,
  30,
  { 14: }
  13,
  30,
  { 15: }
  14,
  30,
  { 16: }
  15,
  30,
  { 17: }
  20,
  30,
  { 18: }
  21,
  30,
  { 19: }
  22,
  30,
  { 20: }
  24,
  30,
  { 21: }
  25,
  30,
  { 22: }
  27,
  30,
  { 23: }
  28,
  { 24: }
  29,
  30,
  { 25: }
  30,
  { 26: }
  1,
  { 27: }
  { 28: }
  2,
  { 29: }
  5,
  { 30: }
  4,
  { 31: }
  3,
  { 32: }
  3,
  { 33: }
  6,
  { 34: }
  { 35: }
  7,
  { 36: }
  { 37: }
  8,
  { 38: }
  { 39: }
  9,
  { 40: }
  17,
  { 41: }
  16,
  { 42: }
  18,
  { 43: }
  { 44: }
  { 45: }
  { 46: }
  { 47: }
  26
);

yym : array [1..yynmatches] of SmallInt = (
{ 0: }
{ 1: }
{ 2: }
  1,
  30,
{ 3: }
  30,
{ 4: }
  23,
  30,
{ 5: }
  4,
  30,
{ 6: }
  19,
  30,
{ 7: }
  30,
{ 8: }
  30,
{ 9: }
  30,
{ 10: }
  30,
{ 11: }
  10,
  30,
{ 12: }
  11,
  30,
{ 13: }
  12,
  30,
{ 14: }
  13,
  30,
{ 15: }
  14,
  30,
{ 16: }
  15,
  30,
{ 17: }
  20,
  30,
{ 18: }
  21,
  30,
{ 19: }
  22,
  30,
{ 20: }
  24,
  30,
{ 21: }
  25,
  30,
{ 22: }
  27,
  30,
{ 23: }
  28,
{ 24: }
  29,
  30,
{ 25: }
  30,
{ 26: }
  1,
{ 27: }
{ 28: }
  2,
{ 29: }
  5,
{ 30: }
  4,
{ 31: }
  3,
{ 32: }
  3,
{ 33: }
  6,
{ 34: }
{ 35: }
  7,
{ 36: }
{ 37: }
  8,
{ 38: }
{ 39: }
  9,
{ 40: }
  17,
{ 41: }
  16,
{ 42: }
  18,
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
  26
);

yyt : array [1..yyntrans] of YYTrec = (
{ 0: }
  ( cc: [ #1..#8,#11..#31,'!','%','&',';','?','@','\',']',
            '`','{'..#127 ]; s: 25),
  ( cc: [ #9 ]; s: 24),
  ( cc: [ #10 ]; s: 23),
  ( cc: [ ' ' ]; s: 22),
  ( cc: [ '"' ]; s: 9),
  ( cc: [ '#' ]; s: 10),
  ( cc: [ '$' ]; s: 7),
  ( cc: [ '''' ]; s: 8),
  ( cc: [ '(' ]; s: 12),
  ( cc: [ ')' ]; s: 13),
  ( cc: [ '*' ]; s: 18),
  ( cc: [ '+' ]; s: 19),
  ( cc: [ ',' ]; s: 11),
  ( cc: [ '-' ]; s: 4),
  ( cc: [ '.' ]; s: 6),
  ( cc: [ '/' ]; s: 21),
  ( cc: [ '0'..'9' ]; s: 5),
  ( cc: [ ':' ]; s: 17),
  ( cc: [ '<' ]; s: 15),
  ( cc: [ '=' ]; s: 16),
  ( cc: [ '>' ]; s: 14),
  ( cc: [ 'A'..'Z','_','a'..'z',#128..#255 ]; s: 2),
  ( cc: [ '[' ]; s: 3),
  ( cc: [ '^' ]; s: 20),
{ 1: }
  ( cc: [ #1..#8,#11..#31,'!','%','&',';','?','@','\',']',
            '`','{'..#127 ]; s: 25),
  ( cc: [ #9 ]; s: 24),
  ( cc: [ #10 ]; s: 23),
  ( cc: [ ' ' ]; s: 22),
  ( cc: [ '"' ]; s: 9),
  ( cc: [ '#' ]; s: 10),
  ( cc: [ '$' ]; s: 7),
  ( cc: [ '''' ]; s: 8),
  ( cc: [ '(' ]; s: 12),
  ( cc: [ ')' ]; s: 13),
  ( cc: [ '*' ]; s: 18),
  ( cc: [ '+' ]; s: 19),
  ( cc: [ ',' ]; s: 11),
  ( cc: [ '-' ]; s: 4),
  ( cc: [ '.' ]; s: 6),
  ( cc: [ '/' ]; s: 21),
  ( cc: [ '0'..'9' ]; s: 5),
  ( cc: [ ':' ]; s: 17),
  ( cc: [ '<' ]; s: 15),
  ( cc: [ '=' ]; s: 16),
  ( cc: [ '>' ]; s: 14),
  ( cc: [ 'A'..'Z','_','a'..'z',#128..#255 ]; s: 2),
  ( cc: [ '[' ]; s: 3),
  ( cc: [ '^' ]; s: 20),
{ 2: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z',#128..#255 ]; s: 26),
{ 3: }
  ( cc: [ #1..'Z','\','^'..#255 ]; s: 27),
  ( cc: [ ']' ]; s: 28),
{ 4: }
  ( cc: [ '0'..'9' ]; s: 29),
{ 5: }
  ( cc: [ '.' ]; s: 31),
  ( cc: [ '0'..'9' ]; s: 30),
{ 6: }
  ( cc: [ '0'..'9' ]; s: 32),
{ 7: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 33),
{ 8: }
  ( cc: [ #1..'&','('..#255 ]; s: 34),
  ( cc: [ '''' ]; s: 35),
{ 9: }
  ( cc: [ #1..'!','#'..#255 ]; s: 36),
  ( cc: [ '"' ]; s: 37),
{ 10: }
  ( cc: [ #1..'"','$'..#255 ]; s: 38),
  ( cc: [ '#' ]; s: 39),
{ 11: }
{ 12: }
{ 13: }
{ 14: }
  ( cc: [ '=' ]; s: 40),
{ 15: }
  ( cc: [ '=' ]; s: 42),
  ( cc: [ '>' ]; s: 41),
{ 16: }
{ 17: }
{ 18: }
{ 19: }
  ( cc: [ '0'..'9' ]; s: 29),
{ 20: }
{ 21: }
  ( cc: [ '*' ]; s: 43),
{ 22: }
{ 23: }
{ 24: }
{ 25: }
{ 26: }
  ( cc: [ '0'..'9','A'..'Z','_','a'..'z',#128..#255 ]; s: 26),
{ 27: }
  ( cc: [ #1..'Z','\','^'..#255 ]; s: 27),
  ( cc: [ ']' ]; s: 28),
{ 28: }
{ 29: }
  ( cc: [ '.' ]; s: 44),
  ( cc: [ '0'..'9' ]; s: 29),
{ 30: }
  ( cc: [ '.' ]; s: 31),
  ( cc: [ '0'..'9' ]; s: 30),
{ 31: }
  ( cc: [ '0'..'9' ]; s: 31),
{ 32: }
  ( cc: [ '0'..'9' ]; s: 32),
  ( cc: [ 'E','e' ]; s: 45),
{ 33: }
  ( cc: [ '0'..'9','A'..'F','a'..'f' ]; s: 33),
{ 34: }
  ( cc: [ #1..'&','('..#255 ]; s: 34),
  ( cc: [ '''' ]; s: 35),
{ 35: }
{ 36: }
  ( cc: [ #1..'!','#'..#255 ]; s: 36),
  ( cc: [ '"' ]; s: 37),
{ 37: }
{ 38: }
  ( cc: [ #1..'"','$'..#255 ]; s: 38),
  ( cc: [ '#' ]; s: 39),
{ 39: }
{ 40: }
{ 41: }
{ 42: }
{ 43: }
  ( cc: [ #1..')','+'..#255 ]; s: 43),
  ( cc: [ '*' ]; s: 46),
{ 44: }
  ( cc: [ '0'..'9' ]; s: 31),
{ 45: }
  ( cc: [ '+','-' ]; s: 44),
{ 46: }
  ( cc: [ '/' ]; s: 47)
{ 47: }
);

yykl : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 4,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 11,
{ 9: } 12,
{ 10: } 13,
{ 11: } 14,
{ 12: } 16,
{ 13: } 18,
{ 14: } 20,
{ 15: } 22,
{ 16: } 24,
{ 17: } 26,
{ 18: } 28,
{ 19: } 30,
{ 20: } 32,
{ 21: } 34,
{ 22: } 36,
{ 23: } 38,
{ 24: } 39,
{ 25: } 41,
{ 26: } 42,
{ 27: } 43,
{ 28: } 43,
{ 29: } 44,
{ 30: } 45,
{ 31: } 46,
{ 32: } 47,
{ 33: } 48,
{ 34: } 49,
{ 35: } 49,
{ 36: } 50,
{ 37: } 50,
{ 38: } 51,
{ 39: } 51,
{ 40: } 52,
{ 41: } 53,
{ 42: } 54,
{ 43: } 55,
{ 44: } 55,
{ 45: } 55,
{ 46: } 55,
{ 47: } 55
);

yykh : array [0..yynstates-1] of SmallInt = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 10,
{ 8: } 11,
{ 9: } 12,
{ 10: } 13,
{ 11: } 15,
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
{ 23: } 38,
{ 24: } 40,
{ 25: } 41,
{ 26: } 42,
{ 27: } 42,
{ 28: } 43,
{ 29: } 44,
{ 30: } 45,
{ 31: } 46,
{ 32: } 47,
{ 33: } 48,
{ 34: } 48,
{ 35: } 49,
{ 36: } 49,
{ 37: } 50,
{ 38: } 50,
{ 39: } 51,
{ 40: } 52,
{ 41: } 53,
{ 42: } 54,
{ 43: } 54,
{ 44: } 54,
{ 45: } 54,
{ 46: } 54,
{ 47: } 55
);

yyml : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 1,
{ 2: } 1,
{ 3: } 3,
{ 4: } 4,
{ 5: } 6,
{ 6: } 8,
{ 7: } 10,
{ 8: } 11,
{ 9: } 12,
{ 10: } 13,
{ 11: } 14,
{ 12: } 16,
{ 13: } 18,
{ 14: } 20,
{ 15: } 22,
{ 16: } 24,
{ 17: } 26,
{ 18: } 28,
{ 19: } 30,
{ 20: } 32,
{ 21: } 34,
{ 22: } 36,
{ 23: } 38,
{ 24: } 39,
{ 25: } 41,
{ 26: } 42,
{ 27: } 43,
{ 28: } 43,
{ 29: } 44,
{ 30: } 45,
{ 31: } 46,
{ 32: } 47,
{ 33: } 48,
{ 34: } 49,
{ 35: } 49,
{ 36: } 50,
{ 37: } 50,
{ 38: } 51,
{ 39: } 51,
{ 40: } 52,
{ 41: } 53,
{ 42: } 54,
{ 43: } 55,
{ 44: } 55,
{ 45: } 55,
{ 46: } 55,
{ 47: } 55
);

yymh : array [0..yynstates-1] of SmallInt = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 2,
{ 3: } 3,
{ 4: } 5,
{ 5: } 7,
{ 6: } 9,
{ 7: } 10,
{ 8: } 11,
{ 9: } 12,
{ 10: } 13,
{ 11: } 15,
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
{ 23: } 38,
{ 24: } 40,
{ 25: } 41,
{ 26: } 42,
{ 27: } 42,
{ 28: } 43,
{ 29: } 44,
{ 30: } 45,
{ 31: } 46,
{ 32: } 47,
{ 33: } 48,
{ 34: } 48,
{ 35: } 49,
{ 36: } 49,
{ 37: } 50,
{ 38: } 50,
{ 39: } 51,
{ 40: } 52,
{ 41: } 53,
{ 42: } 54,
{ 43: } 54,
{ 44: } 54,
{ 45: } 54,
{ 46: } 54,
{ 47: } 55
);

yytl : array [0..yynstates-1] of SmallInt = (
{ 0: } 1,
{ 1: } 25,
{ 2: } 49,
{ 3: } 50,
{ 4: } 52,
{ 5: } 53,
{ 6: } 55,
{ 7: } 56,
{ 8: } 57,
{ 9: } 59,
{ 10: } 61,
{ 11: } 63,
{ 12: } 63,
{ 13: } 63,
{ 14: } 63,
{ 15: } 64,
{ 16: } 66,
{ 17: } 66,
{ 18: } 66,
{ 19: } 66,
{ 20: } 67,
{ 21: } 67,
{ 22: } 68,
{ 23: } 68,
{ 24: } 68,
{ 25: } 68,
{ 26: } 68,
{ 27: } 69,
{ 28: } 71,
{ 29: } 71,
{ 30: } 73,
{ 31: } 75,
{ 32: } 76,
{ 33: } 78,
{ 34: } 79,
{ 35: } 81,
{ 36: } 81,
{ 37: } 83,
{ 38: } 83,
{ 39: } 85,
{ 40: } 85,
{ 41: } 85,
{ 42: } 85,
{ 43: } 85,
{ 44: } 87,
{ 45: } 88,
{ 46: } 89,
{ 47: } 90
);

yyth : array [0..yynstates-1] of SmallInt = (
{ 0: } 24,
{ 1: } 48,
{ 2: } 49,
{ 3: } 51,
{ 4: } 52,
{ 5: } 54,
{ 6: } 55,
{ 7: } 56,
{ 8: } 58,
{ 9: } 60,
{ 10: } 62,
{ 11: } 62,
{ 12: } 62,
{ 13: } 62,
{ 14: } 63,
{ 15: } 65,
{ 16: } 65,
{ 17: } 65,
{ 18: } 65,
{ 19: } 66,
{ 20: } 66,
{ 21: } 67,
{ 22: } 67,
{ 23: } 67,
{ 24: } 67,
{ 25: } 67,
{ 26: } 68,
{ 27: } 70,
{ 28: } 70,
{ 29: } 72,
{ 30: } 74,
{ 31: } 75,
{ 32: } 77,
{ 33: } 78,
{ 34: } 80,
{ 35: } 80,
{ 36: } 82,
{ 37: } 82,
{ 38: } 84,
{ 39: } 84,
{ 40: } 84,
{ 41: } 84,
{ 42: } 84,
{ 43: } 86,
{ 44: } 87,
{ 45: } 88,
{ 46: } 89,
{ 47: } 89
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