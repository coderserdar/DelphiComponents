{**************************************************************************}
{   TxQuery DataSet                                                        }
{                                                                          }
{   Copyright (C) <1999-2003> of                                           }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                            }
{   email: luisarvayo@yahoo.com                                            }
{     url: http://www.ezsoft.com                                           }
{          http://www.sigmap.com/txquery.htm                               }
{                                                                          }
{   Open Source patch review (2009) with permission from Alfonso Moreno by }
{   Chee-Yang CHAU and Sherlyn CHEW (Klang, Selangor, Malaysia)            }
{   email: cychau@gmail.com                                                }
{   url: http://code.google.com/p/txquery/                                 }
{        http://groups.google.com/group/txquery                            }
{                                                                          }
{   This program is free software: you can redistribute it and/or modify   }
{   it under the terms of the GNU General Public License as published by   }
{   the Free Software Foundation, either version 3 of the License, or      }
{   (at your option) any later version.                                    }
{                                                                          }
{   This program is distributed in the hope that it will be useful,        }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of         }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
{   GNU General Public License for more details.                           }
{                                                                          }
{   You should have received a copy of the GNU General Public License      }
{   along with this program.  If not, see <http://www.gnu.org/licenses/>.  }
{                                                                          }
{**************************************************************************}

Unit QLEXLIB;

{$I XQ_FLAG.INC}
Interface

Uses Classes;

Const
  nl = #10; (* newline character *)
  max_chars = maxint div SizeOf(Char); { patched by ccy }
  intial_bufsize = 16384;

Type

  PCharArray = ^TCharArray;
  TCharArray = array [1..max_chars] of Char;

  TCustomLexer = Class
  private
    function GetBuf(Index: Integer): Char;
    procedure SetBuf(Index: Integer; Value: Char);
  Public
    //yyinput, yyoutput : Text;      (* input and output file            *)
    //yyerrorfile       : Text;      (* standard error file              *)
    yyinput, yyoutput: TStream; (* input and output file            *)
    yyerrorfile: TStream; (* standard error file              *)
    yyline: String; (* current input line               *)
    yylineno, yycolno: Integer; (* current input position           *)
    //yytext: String; (* matched text                     *)
    yyTextBuf         : PCharArray;
    yyTextLen         : Integer;
    yyTextBufSize     : Integer;
    (*   (should be considered r/o)     *)
{yyleng            : Byte         (* length of matched text *)
absolute yytext;                incompatible with Delphi 2.0       }

(* I/O routines:

The following routines get_char, unget_char and put_char are used to
implement access to the input and output files. Since \n (newline) for
Lex means line end, the I/O routines have to translate MS-DOS line ends
(carriage-return/line-feed) into newline characters and vice versa. Input
is buffered to allow rescanning text (via unput_char).

The input buffer holds the text of the line to be scanned. When the input
buffer empties, a new line is obtained from the input stream. Characters
can be returned to the input buffer by calls to unget_char. At end-of-
file a null character is returned.

The input routines also keep track of the input position and set the
yyline, yylineno, yycolno variables accordingly.

Since the rest of the Lex library only depends on these three routines
(there are no direct references to the yyinput and yyoutput files or
to the input buffer), you can easily replace get_char, unget_char and
put_char by another suitable set of routines, e.g. if you want to read
from/write to memory, etc. *)

    Function get_char: Char;
    (* obtain one character from the input file (null character at end-of-
       file) *)

    Procedure unget_char( c: Char );
    (* return one character to the input file to be reread in subsequent
       calls to get_char *)

    Procedure put_char( c: Char );
    (* write one character to the output file *)

  (* Utility routines: *)

    Procedure echo;
    (* echoes the current match to the output stream *)

    Procedure yymore;
    (* append the next match to the current one *)

    Procedure yyless( n: Integer );
    (* truncate yytext to size n and return the remaining characters to the
       input stream *)

    Procedure reject;
    (* reject the current match and execute the next one *)

    (* reject does not actually cause the input to be rescanned; instead,
       internal state information is used to find the next match. Hence
       you should not try to modify the input stream or the yytext variable
       when rejecting a match. *)

    Procedure returni( n: Integer );
    Procedure returnc( c: Char );
    (* sets the return value of yylex *)

    Procedure start( state: Integer );
    (* puts the lexical analyzer in the given start state; state=0 denotes
       the default start state, other values are user-defined *)

  (* yywrap:

     The yywrap function is called by yylex at end-of-file (unless you have
     specified a rule matching end-of-file). You may redefine this routine
     in your Lex program to do application-dependent processing at end of
     file. In particular, yywrap may arrange for more input and return false
     in which case the yylex routine resumes lexical analysis. *)

    Function yywrap: Boolean;
    (* The default yywrap routine supplied here closes input and output
       files and returns true (causing yylex to terminate). *)

  (* The following are the internal data structures and routines used by the
     lexical analyzer routine yylex; they should not be used directly. *)

    Function yylex: Integer; Virtual; Abstract;
    (* this function must be overriden by the Lexer descendent in order
       to provide the lexing service *)
    constructor Create;
    destructor Destroy; override;
    procedure CheckBuffer(Index : integer);
    procedure CheckyyTextBuf(Size : integer);
    procedure GetyyText(var s : string);
    property Buf[Index: Integer]: Char read GetBuf write SetBuf;

  Protected
    yystate: Integer; (* current state of lexical analyzer *)
    yyactchar: Char; (* current character *)
    yylastchar: Char; (* last matched character (#0 if none) *)
    yyrule: Integer; (* matched rule *)
    yyreject: Boolean; (* current match rejected? *)
    yydone: Boolean; (* yylex return value set? *)
    yyretval: Integer; (* yylex return value *)
    bufptr: Integer;
    //buf: Array[1..max_chars] Of Char;
    bufSize : Integer;
    FBuf : PCharArray;

    Procedure yynew;
    (* starts next match; initializes state information of the lexical
       analyzer *)

    Procedure yyscan;
    (* gets next character from the input stream and updates yytext and
       yyactchar accordingly *)

    Procedure yymark( n: Integer );
    (* marks position for rule no. n *)

    Procedure yymatch( n: Integer );
    (* declares a match for rule number n *)

    Function yyfind( Var n: Integer ): Boolean;
    (* finds the last match and the corresponding marked position and
       adjusts the matched string accordingly; returns:
       - true if a rule has been matched, false otherwise
       - n: the number of the matched rule *)

    Function yydefault: Boolean;
    (* executes the default action (copy character); returns true unless
       at end-of-file *)

    Procedure yyclear;
    (* reinitializes state information after lexical analysis has been
       finished *)

    Procedure fatal( msg: String );
    (* writes a fatal error message and halts program *)

  End; (* TCustomLexeer *)

Function eof( aStream: Tstream ): boolean;
Procedure readln( aStream: TStream; Var aLine: String );
Procedure writeln( aStream: TStream; aline: String );
Procedure write( aStream: TStream; aLine: String );

Implementation

uses
  math, SysUtils, CnvStrUtils;

(* utility procedures *)

Function eof( aStream: Tstream ): boolean;
Begin
  result := aStream.position >= aStream.size;
End;

Procedure readln( aStream: TStream; Var aLine: String );
Var
  //aBuffer: String;
  CRBuf : string;
  trouve: boolean;
  unCar: Char;
  Buf : PChar;
  BufSize : Integer;
  i : Integer;

  procedure CheckBuffer;
  begin
    repeat
      if i >= BufSize then
      begin
        BufSize := max (BufSize * 2, 256);
        ReallocMem (Buf, BufSize * SizeOf(Char)); { patched by ccy }
      end;
    until i < BufSize;
  end;

Begin
  // ??????
  //aBuffer := '';
  BufSize := 256;
  i := 0;
  GetMem (Buf, BufSize * SizeOf(Char)); { patched by ccy }
  try
    trouve := false;
    Repeat
      aStream.read( unCar, 1 * SizeOf(Char) ); { patched by ccy }
      If aStream.Position >= aStream.Size Then
      Begin
        trouve := true;
        if not CharInSet(unCar, [#10,#13]) then
        begin
          //aLine := aBuffer+unCar
          Inc (i);
          CheckBuffer;
          Move (uncar, Buf [i - 1], 1 * SizeOf(Char)); { patched by ccy }
          SetLength (aLine, i);
          Move (Buf^, aLine [1], i * SizeOf(Char)); { patched by ccy }
        end else
        begin
          if i > 0 then
          begin
            SetLength (aLine, i);
            Move (Buf^, aLine [1], i * SizeOf(Char)); { patched by ccy }
          end else
            aLine:='';
        end;
      End
      Else
        Case unCar Of
          #10:
            Begin
              //aLine := aBuffer;
              if i>0 then
              begin
                SetLength (aLine, i);
                Move (Buf^, aLine [1], i * SizeOf(Char)); { patched by ccy }
                trouve := true;
              end else
                aLine:= '';
            End;
          #13:
            Begin
              aStream.read( unCar, 1 * SizeOf(Char)); { patched by ccy }
              If unCar = #10 Then
              Begin
                //aLine := aBuffer;
                if i > 0 then
                begin
                  SetLength (aLine, i);
                  Move (Buf^, aLine [1], i * SizeOf(Char)); { patched by ccy }
                  trouve := true;
                end else
                  aLine:='';
              End
              Else
              Begin
                Inc (i, 2);
                CheckBuffer;
                CRBuf := #13 + unCar;
                Move (CRBuf [1], Buf [i - 2], 2 * SizeOf(Char)); { patched by ccy }
                //aBuffer := aBuffer + #13 + unCar;
              End;
            End;
        Else
          //aBuffer := aBuffer + unCar;
        begin
          Inc (i);
          CheckBuffer;
          Move (unCar, Buf [i - 1], 1 * SizeOf(Char)); { patched by ccy }
          //aBuffer := aBuffer+unCar;
        end;

        End;
    Until trouve;
  finally
    FreeMem (Buf, BufSize * SizeOf(Char));  { patched by ccy }
  end;
End;

Procedure writeln( aStream: TStream; aline: String );
Const
  FINLIGNE: Array[1..2] Of char = ( #13, #10 );
Begin
  // ??????
  write( aStream, aLine );
  aStream.write( FINLIGNE, 2 );
End;

Procedure write( aStream: TStream; aLine: String );
Const
  WRITEBUFSIZE = 8192;
Var
  aBuffer: Array[1..WRITEBUFSIZE] Of char;
  j, nbuf: integer;
  k, nreste: integer;
Begin
  nbuf := length( aLine ) Div WRITEBUFSIZE;
  nreste := length( aLine ) - ( nbuf * WRITEBUFSIZE );
  For j := 0 To nbuf - 1 Do
  Begin
    For k := 1 To WRITEBUFSIZE Do
      aBuffer[k] := aLine[j * WRITEBUFSIZE + k];
    aStream.write( aBuffer, WRITEBUFSIZE );
  End;
  For k := 1 To nreste Do
    aBuffer[k] := aLine[nbuf * WRITEBUFSIZE + k];
  aStream.write( aBuffer, nreste );
End;

Procedure TCustomLexer.fatal( msg: String );
(* writes a fatal error message and halts program *)
Begin
  writeln( yyerrorfile, 'LexLib: ' + msg );
  halt( 1 );
End;

(* I/O routines: *)

Function TCustomLexer.get_char: Char;
Var
  i: Integer;
Begin
  If ( bufptr = 0 ) And Not eof( yyinput ) Then
  Begin
    readln( yyinput, yyline );
    inc( yylineno );
    yycolno := 1;
    buf[1] := nl;
    For i := 1 To length( yyline ) Do
    begin
      buf[i + 1] := yyline[length( yyline ) - i + 1];
    end;
    inc( bufptr, length( yyline ) + 1 );
  End;
  If bufptr > 0 Then
  Begin
    get_char := buf[bufptr];
    dec( bufptr );
    inc( yycolno );
  End
  Else
    get_char := #0;
End;

Procedure TCustomLexer.unget_char( c: Char );
Begin
  If bufptr = max_chars Then
    fatal( 'input buffer overflow' );
  inc( bufptr );
  dec( yycolno );
  buf[bufptr] := c;
End;

Procedure TCustomLexer.put_char( c: Char );
Begin
  If c = #0 Then
    { ignore }
  Else If c = nl Then
    writeln( yyoutput, '' )
  Else
    write( yyoutput, c )
End;

(* Variables:

   Some state information is maintained to keep track with calls to yymore,
   yyless, reject, start and yymatch/yymark, and to initialize state
   information used by the lexical analyzer.
   - yystext: contains the initial contents of the yytext variable; this
     will be the empty string, unless yymore is called which sets yystext
     to the current yytext
   - yysstate: start state of lexical analyzer (set to 0 during
     initialization, and modified in calls to the start routine)
   - yylstate: line state information (1 if at beginning of line, 0
     otherwise)
   - yystack: stack containing matched rules; yymatches contains the number of
     matches
   - yypos: for each rule the last marked position (yymark); zeroed when rule
     has already been considered
   - yysleng: copy of the original yyleng used to restore state information
     when reject is used *)

Const

  max_matches = 1024;
  max_rules = 256;

Var

  yystext: String;
  yysstate, yylstate: Integer;
  yymatches: Integer;
  yystack: Array[1..max_matches] Of Integer;
  yypos: Array[1..max_rules] Of Integer;
  yysleng: Byte;

(* Utilities: *)

Procedure TCustomLexer.echo;
Var
  i: Integer;
Begin
  for i := 1 to yyTextLen do
    put_char(yyTextBuf^ [i])
End;

Procedure TCustomLexer.yymore;
Begin
  //yystext := yytext;
  if yyTextBuf <> nil then
  begin
    if yyTextLen > 0 then
    begin
      SetLength (yystext, yyTextLen);
      Move (yyTextBuf^, yystext [1], yyTextLen);
    end else
      yystext:='';
  end
  else yystext := '';
End;

Procedure TCustomLexer.yyless( n: Integer );
Var
  i: Integer;
Begin
  for i := yytextlen downto n+1 do
    unget_char(yytextbuf^ [i]);
  CheckyyTextBuf (n);
  yyTextLen := n;
End;

Procedure TCustomLexer.reject;
Var
  i: Integer;
Begin
  yyreject := true;
  for i := yyTextLen + 1 to yysleng do
  begin
    Inc (yyTextLen);
    yyTextBuf^ [yyTextLen] := get_char;
    //yytext := yytext + get_char;
  end;
  dec( yymatches );
End;

Procedure TCustomLexer.returni( n: Integer );
Begin
  yyretval := n;
  yydone := true;
End;

Procedure TCustomLexer.returnc( c: Char );
Begin
  yyretval := ord( c );
  yydone := true;
End;

Procedure TCustomLexer.start( state: Integer );
Begin
  yysstate := state;
End;

(* yywrap: *)

Function TCustomLexer.yywrap: Boolean;
Begin
  // ????? close(yyinput); close(yyoutput);
  yywrap := true;
End;

(* Internal routines: *)

Procedure TCustomLexer.yynew;
Begin
  If yylastchar <> #0 Then
    If yylastchar = nl Then
      yylstate := 1
    Else
      yylstate := 0;
  yystate := yysstate + yylstate;
  //yytext := yystext;
  CheckyyTextBuf (Length (yystext));
  yyTextLen := Length (yystext);
  if yyTextLen > 0 then
    Move (yystext [1], yytextbuf^, yyTextLen);

  yystext := '';
  yymatches := 0;
  yydone := false;
End;

Procedure TCustomLexer.yyscan;
Begin
  //if Length(yytext)=255 then fatal('yytext overflow');
  yyactchar := get_char;
  //yytext := yytext + yyactchar;
  CheckyyTextBuf (yyTextLen + 1);
  Inc (yyTextLen);
  yyTextBuf^ [yyTextLen] := yyactchar;
End;

Procedure TCustomLexer.yymark( n: Integer );
Begin
  If n > max_rules Then
    fatal( 'too many rules' );
  //yypos[n] := Length( yytext );
  yypos [n] := yyTextLen;
End;

Procedure TCustomLexer.yymatch( n: Integer );
Begin
  inc( yymatches );
  If yymatches > max_matches Then
    fatal( 'match stack overflow' );
  yystack[yymatches] := n;
End;

Function TCustomLexer.yyfind( Var n: Integer ): Boolean;
Begin
  yyreject := false;
  While ( yymatches > 0 ) And ( yypos[yystack[yymatches]] = 0 ) Do
    dec( yymatches );
  If yymatches > 0 Then
  Begin
    yysleng := yyTextLen;
    n := yystack[yymatches];
    yyless( yypos[n] );
    yypos[n] := 0;
    if yyTextLen >0 then
      yylastchar := yytextbuf^ [yytextlen]
    Else
      yylastchar := #0;
    yyfind := true;
  End
  Else
  Begin
    yyless( 0 );
    yylastchar := #0;
    yyfind := false;
  End
End;

Function TCustomLexer.yydefault: Boolean;
Begin
  yyreject := false;
  yyactchar := get_char;
  If yyactchar <> #0 Then
  Begin
    put_char( yyactchar );
    yydefault := true;
  End
  Else
  Begin
    yylstate := 1;
    yydefault := false;
  End;
  yylastchar := yyactchar;
End;

Procedure TCustomLexer.yyclear;
Begin
  bufptr := 0;
  yysstate := 0;
  yylstate := 1;
  yylastchar := #0;
  yyTextLen := 0;
  yystext := '';
End;

constructor TCustomLexer.Create;
begin
  inherited Create;
  CheckyyTextBuf (intial_bufsize);
  CheckBuffer (intial_bufsize);
end;

destructor TCustomLexer.Destroy;
begin
  FreeMem (FBuf);
  FreeMem (yyTextBuf);
  inherited;
end;

procedure TCustomLexer.CheckBuffer(Index : integer);
begin
  repeat
    if Index > BufSize then
    begin
      bufSize := max (bufSize * 2, intial_bufsize);
      ReallocMem (FBuf, bufSize);
    end;
  until Index <= bufSize;
end;

function TCustomLexer.GetBuf(Index: Integer): Char;
begin
  CheckBuffer (Index);
  Result := FBuf^ [Index];
end;

procedure TCustomLexer.SetBuf(Index: Integer; Value: Char);
begin
  CheckBuffer (Index);
  FBuf^ [Index] := Value;
end;

procedure TCustomLexer.CheckyyTextBuf(Size : integer);
begin
  repeat
    if Size > yyTextBufSize then
    begin
      yyTextBufSize := max (yyTextBufSize * 2, intial_bufsize);
      ReallocMem (yyTextBuf, yyTextBufSize);
    end;
  until Size <= yyTextBufSize;
end;

procedure TCustomLexer.GetyyText(var s : string);
begin
  if yyTextLen > 0 then
  begin
    SetLength (s, yyTextLen);
    Move (yytextbuf^, s[1], yyTextLen * SizeOf(Char)); { patched by ccy }
  end else
    s:= '';
end;

End.
