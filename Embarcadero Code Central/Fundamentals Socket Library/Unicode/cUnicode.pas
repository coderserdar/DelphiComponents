{$INCLUDE ..\cDefines.inc}
unit cUnicode;

{                                                                              }
{                        Unicode string functions v3.04                        }
{                                                                              }
{      This unit is copyright © 2000-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cUnicode.pas                     }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Unicode functions for using WideStrings.                                   }
{                                                                              }
{ Revision history:                                                            }
{   19/04/2002  0.01  Initial version                                          }
{   26/04/2002  0.02  Added WidePos, WideReplace and Append functions.         }
{   28/10/2002  3.03  Refactored for Fundamentals 3.                           }
{   18/12/2002  3.04  Removed dependancy on cUnicodeChar unit.                 }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUtils;

const
  UnitName      = 'cUnicode';
  UnitVersion   = '3.03';
  UnitDesc      = 'Unicode strings';



{                                                                              }
{ Unicode errors                                                               }
{                                                                              }
type
  EUnicode = class(Exception);



{                                                                              }
{ WideString functions                                                         }
{                                                                              }
type
  WideCharMatchFunction = function (const Ch: WideChar): Boolean;

function  WideMatchChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WideMatchCharsRev(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WideMatchAllChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WideMatchAnsiCharNoCase(const M: Char; const C: WideChar): Boolean;
function  WideMatchAnsiStr(const M: String; const P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;
function  WideMatch(const M: WideString; const P: PWideChar): Boolean;

function  WideEqualAnsiStr(const M: String; const S: WideString;
          const CaseSensitive: Boolean = True): Boolean;

function  WidePosChar(const F: WideChar; const P: PWideChar): Integer; overload;
function  WidePosAnsiChar(const F: Char; const P: PWideChar): Integer;
function  WidePosAnsiCharSet(const F: CharSet; const P: PWideChar): Integer;
function  WidePosAnsiStr(const F: String; const P: PWideChar;
          const CaseSensitive: Boolean = True): Integer;

function  WideSkipChar(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Boolean;
function  WideSkipChars(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Integer;
function  WideSkipAnsiChar(const Ch: Char; var P: PWideChar): Boolean;
function  WideSkipAnsiStr(const M: String; var P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;

function  WideExtractBeforeChar(const Ch: WideChar; var P: PWideChar;
          var S: WideString): Boolean;
function  WideExtractBeforeAnsiChar(const Ch: Char; var P: PWideChar;
          var S: WideString): Boolean;
function  WideExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: Char;
          var P: PWideChar; var S: WideString): Boolean;
function  WideExtractAnsiCharQuoted(const Delimiter: Char;
          var P: PWideChar; var S: WideString): Boolean;

function  WideDup(const Ch: WideChar; const Count: Integer): WideString;

procedure WideTrimInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);
procedure WideTrimLeftInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);
procedure WideTrimRightInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);

function  WideTrim(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;
function  WideTrimLeft(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;
function  WideTrimRight(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;

function  WideCountChar(const CharMatchFunc: WideCharMatchFunction;
          const S: WideString): Integer; overload;
function  WideCountChar(const Ch: WideChar; const S: WideString): Integer; overload;

function  WidePosChar(const F: WideChar; const S: WideString;
          const StartIndex: Integer = 1): Integer; overload;
function  WidePos(const F: WideString; const S: WideString;
          const StartIndex: Integer = 1): Integer; overload;

procedure WideReplaceChar(const Find: WideChar; const Replace: WideString;
          var S: WideString);

procedure WideSetLengthAndZero(var S: WideString; const NewLength: Integer);

{$IFDEF DELPHI5}
function  WideUpperCase(const S: WideString): WideString;
function  WideLowerCase(const S: WideString): WideString;
{$ENDIF}



{                                                                              }
{ Dynamic Array functions                                                      }
{                                                                              }
type
  WideStringArray = Array of WideString;

function  Append(var V : WideStringArray;
          const R: WideString): Integer; overload;
function  AppendWideStringArray(var V : WideStringArray;
          const R: WideStringArray): Integer;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Fundamentals }
  {$IFDEF DELPHI5}
  cUnicodeChar,
  {$ENDIF}
  cUnicodeCodecs;



{                                                                              }
{ Local declarations                                                           }
{                                                                              }
const
  WideNULL = WideChar(#0);



{                                                                              }
{ Match                                                                        }
{                                                                              }
function WideMatchChars(const CharMatchFunc: WideCharMatchFunction;
    const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  C := Q^;
  if (L < 0) and (C = WideNULL) then
    exit;
  Repeat
    if not CharMatchFunc(C) then
      exit;
    Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  Until (L = 0) or ((L < 0) and (C = WideNULL));
end;

function WideMatchCharsRev(const CharMatchFunc: WideCharMatchFunction;
    const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  Inc(Q, L - 1);
  C := Q^;
  if (L < 0) and (C = WideNULL) then
    exit;
  Repeat
    if not CharMatchFunc(C) then
      exit;
    Inc(Result);
    Dec(Q);
    if L < 0 then
      C := Q^ else
      begin
        Dec(L);
        if L > 0 then
          C := Q^;
      end;
  Until (L = 0) or ((L < 0) and (C = WideNULL));
end;

function WideMatchAllChars(const CharMatchFunc: WideCharMatchFunction; const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  C := Q^;
  if (L < 0) and (C = WideNULL) then
    exit;
  Repeat
    if CharMatchFunc(C) then
      Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  Until (L = 0) or ((L < 0) and (C = WideNULL));
end;

function WideMatchAnsiCharNoCase(const M: Char; const C: WideChar): Boolean;
const ASCIICaseOffset = Ord('a') - Ord('A');
var D, N : Char;
begin
  if Ord(C) > $7F then
    begin
      Result := False;
      exit;
    end;
  D := Char(Ord(C));
  if D in ['A'..'Z'] then
    D := Char(Ord(D) + ASCIICaseOffset);
  N := M;
  if N in ['A'..'Z'] then
    N := Char(Ord(N) + ASCIICaseOffset);
  Result := D = N;
end;

function WideMatchAnsiStr(const M: String; const P: PWideChar;
    const CaseSensitive: Boolean): Boolean;
var I, L : Integer;
    Q : PWideChar;
    R : PChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  R := Pointer(M);
  Q := P;
  if CaseSensitive then
    begin
      For I := 1 to L do
        if Ord(R^) <> Ord(Q^) then
          begin
            Result := False;
            exit;
          end else
          begin
            Inc(R);
            Inc(Q);
          end;
    end else
    begin
      For I := 1 to L do
        if not WideMatchAnsiCharNoCase(R^, Q^) then
          begin
            Result := False;
            exit;
          end else
          begin
            Inc(R);
            Inc(Q);
          end;
    end;
  Result := True;
end;

function WideMatch(const M: WideString; const P: PWideChar): Boolean;
var I, L : Integer;
    Q, R : PWideChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  R := Pointer(M);
  Q := P;
  For I := 1 to L do
    if R^ <> Q^ then
      begin
        Result := False;
        exit;
      end else
      begin
        Inc(R);
        Inc(Q);
      end;
  Result := True;
end;

function WideEqualAnsiStr(const M: String; const S: WideString;
    const CaseSensitive: Boolean): Boolean;
var L : Integer;
begin
  L := Length(M);
  Result := L = Length(S);
  if not Result or (L = 0) then
    exit;
  Result := WideMatchAnsiStr(M, Pointer(S), CaseSensitive);
end;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function WidePosAnsiChar(const F: Char; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Q^ <> #0 do
    if Ord(Q^) = Ord(F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WidePosAnsiCharSet(const F: CharSet; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Q^ <> #0 do
    if (Ord(Q^) < $80) and (Char(Ord(Q^)) in F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WidePosChar(const F: WideChar; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Q^ <> #0 do
    if Q^ = F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WidePosAnsiStr(const F: String; const P: PWideChar; const CaseSensitive: Boolean): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Q^ <> #0 do
    if WideMatchAnsiStr(F, Q, CaseSensitive) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;



{                                                                              }
{ Skip                                                                         }
{                                                                              }
function WideSkipChar(const CharMatchFunc: WideCharMatchFunction; var P: PWideChar): Boolean;
var C : WideChar;
begin
  Assert(Assigned(CharMatchFunc), 'Assigned(CharMatchFunc)');
  C := P^;
  if C = #0 then
    begin
      Result := False;
      exit;
    end;
  Result := CharMatchFunc(C);
  if Result then
    Inc(P);
end;

function WideSkipChars(const CharMatchFunc: WideCharMatchFunction; var P: PWideChar): Integer;
var C : WideChar;
begin
  Assert(Assigned(CharMatchFunc), 'Assigned(CharMatchFunc)');
  Result := 0;
  if not Assigned(P) then
    exit;
  C := P^;
  While C <> #0 do
    if not CharMatchFunc(C) then
      exit else
      Inc(P);
end;

function WideSkipAnsiChar(const Ch: Char; var P: PWideChar): Boolean;
begin
  Result := Ord(P^) = Ord(Ch);
  if Result then
    Inc(P);
end;

function WideSkipAnsiStr(const M: String; var P: PWideChar; const CaseSensitive: Boolean): Boolean;
begin
  Result := WideMatchAnsiStr(M, P, CaseSensitive);
  if Result then
    Inc(P, Length(M));
end;



{                                                                              }
{ Extract                                                                      }
{                                                                              }
function WideExtractBeforeChar(const Ch: WideChar; var P: PWideChar; var S: WideString): Boolean;
var I : Integer;
begin
  I := WidePosChar(Ch, P);
  Result := I >= 0;
  if I <= 0 then
    begin
      S := '';
      exit;
    end;
  Inc(P, I);
  SetLength(S, I);
  Move(P^, Pointer(S)^, I * Sizeof(WideChar));
end;

function WideExtractBeforeAnsiChar(const Ch: Char; var P: PWideChar; var S: WideString): Boolean;
var I : Integer;
begin
  I := WidePosAnsiChar(Ch, P);
  Result := I >= 0;
  if I <= 0 then
    begin
      S := '';
      exit;
    end;
  Inc(P, I);
  SetLength(S, I);
  Move(P^, Pointer(S)^, I * Sizeof(WideChar));
end;

function WideExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: Char;
         var P: PWideChar; var S: WideString): Boolean;
var Q : PWideChar;
begin
  Q := P;
  Result := Assigned(Q) and (Ord(Q^) < $80) and (Ord(Q^) = Ord(LeftDelimiter));
  if not Result then
    begin
      S := '';
      exit;
    end;
  Inc(Q);
  Result := WideExtractBeforeAnsiChar(RightDelimiter, Q, S);
  if not Result then
    exit;
  Inc(Q);
  P := Q;
end;

function WideExtractAnsiCharQuoted(const Delimiter: Char; var P: PWideChar; var S: WideString): Boolean;
begin
  Result := WideExtractAnsiCharDelimited(Delimiter, Delimiter, P, S);
end;



{                                                                              }
{ Dup                                                                          }
{                                                                              }
function WideDup(const Ch: WideChar; const Count: Integer): WideString;
var I : Integer;
    P : PWideChar;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  P := Pointer(Result);
  For I := 1 to Count do
    begin
      P^ := Ch;
      Inc(P);
    end;
end;



{                                                                              }
{ Trim                                                                         }
{                                                                              }
procedure WideTrimLeftInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWideWhiteSpace;
  I := 0;
  P := Pointer(S);
  While F(P^) do
    begin
      Inc(I);
      Inc(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if I = 0 then
    exit;
  S := Copy(S, I + 1, L);
end;

procedure WideTrimRightInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWideWhiteSpace;
  I := 0;
  P := Pointer(S);
  Inc(P, L - 1);
  While F(P^) do
    begin
      Inc(I);
      Dec(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if I = 0 then
    exit;
  SetLength(S, L);
end;

procedure WideTrimInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, J, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWideWhiteSpace;
  I := 0;
  P := Pointer(S);
  Inc(P, L - 1);
  While F(P^) or IsWideControl(P^) do
    begin
      Inc(I);
      Dec(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  J := 0;
  P := Pointer(S);
  While F(P^) or IsWideControl(P^) do
    begin
      Inc(J);
      Inc(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if (I = 0) and (J = 0) then
    exit;
  S := Copy(S, J + 1, L);
end;

function WideTrimLeft(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimLeftInPlace(Result, MatchFunc);
end;

function WideTrimRight(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimRightInPlace(Result, MatchFunc);
end;

function WideTrim(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimInPlace(Result, MatchFunc);
end;



{                                                                              }
{ Count                                                                        }
{                                                                              }
function WideCountChar(const CharMatchFunc: WideCharMatchFunction; const S: WideString): Integer;
begin
  Result := WideMatchAllChars(CharMatchFunc, Pointer(S), Length(S));
end;

function WideCountChar(const Ch: WideChar; const S: WideString): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := 0;
  Q := PWideChar(S);
  if not Assigned(Q) then
    exit;
  For I := 1 to Length(S) do
    begin
      if Q^ = Ch then
        Inc(Result);
      Inc(Q);
    end;
end;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function WidePosChar(const F: WideChar; const S: WideString; const StartIndex: Integer): Integer;
var P : PWideChar;
    I, L : Integer;
begin
  L := Length(S);
  if (StartIndex > L) or (StartIndex < 1) then
    begin
      Result := 0;
      exit;
    end;
  P := Pointer(S);
  Inc(P, StartIndex - 1);
  For I := StartIndex to L do
    if P^ = F then
      begin
        Result := I;
        exit;
      end else
      Inc(P);
  Result := 0;
end;

function WidePos(const F: WideString; const S: WideString; const StartIndex: Integer): Integer;
var P : PWideChar;
    I, L : Integer;
begin
  L := Length(S);
  if (StartIndex > L) or (StartIndex < 1) then
    begin
      Result := 0;
      exit;
    end;
  P := Pointer(S);
  Inc(P, StartIndex - 1);
  For I := StartIndex to L do
    if WideMatch(F, P) then
      begin
        Result := I;
        exit;
      end else
      Inc(P);
  Result := 0;
end;



{                                                                              }
{ Replace                                                                      }
{                                                                              }
procedure WideReplaceChar(const Find: WideChar; const Replace: WideString; var S: WideString);
var C, L, M, I, R : Integer;
    P, Q : PWideChar;
    T : WideString;
begin
  C := WideCountChar(Find, S);
  if C = 0 then
    exit;
  R := Length(Replace);
  M := Length(S);
  L := M + (R - 1) * C;
  if L = 0 then
    begin
      S := '';
      exit;
    end;
  SetLength(T, L);
  P := Pointer(S);
  Q := Pointer(T);
  For I := 1 to M do
    if P^ = Find then
      begin
        if R > 0 then
          begin
            Move(Pointer(Replace)^, Q^, Sizeof(WideChar) * R);
            Inc(Q, R);
          end;
        Inc(P);
      end else
      begin
        Q^ := P^;
        Inc(P);
        Inc(Q);
      end;
  S := T;
end;

procedure WideSetLengthAndZero(var S: WideString; const NewLength: Integer);
var L : Integer;
    P : PWideChar;
begin
  L := Length(S);
  if L = NewLength then
    exit;
  SetLength(S, NewLength);
  if L > NewLength then
    exit;
  P := Pointer(S);
  Inc(P, L);
  FillChar(P^, (NewLength - L) * Sizeof(WideChar), #0);
end;

{$IFDEF DELPHI5}
function WideUpperCase(const S: WideString): WideString;
var I : Integer;
begin
  Result := '';
  For I := 1 to Length(S) do
    Result := Result + WideUpCaseFolding(S[I]);
end;

function WideLowerCase(const S: WideString): WideString;
var I : Integer;
begin
  Result := '';
  For I := 1 to Length(S) do
    Result := Result + WideLowCaseFolding(S[I]);
end;
{$ENDIF}



{                                                                              }
{ Dynamic Array functions                                                      }
{                                                                              }
function Append(var V : WideStringArray; const R: WideString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function AppendWideStringArray(var V : WideStringArray; const R: WideStringArray): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      For I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var S: WideString;
begin
  Assert(WideMatchAnsiCharNoCase('A', 'a'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('z', 'Z'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('1', '1'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('A', 'B'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('0', 'A'), 'WideMatchAnsiCharNoCase');

  Assert(WideMatchAnsiStr('Unicode', 'uNicode', False), 'WideMatchAnsiStr');
  Assert(not WideMatchAnsiStr('Unicode', 'uNicode', True), 'WideMatchAnsiStr');
  Assert(WideMatchAnsiStr('Unicode', 'Unicode', True), 'WideMatchAnsiStr');

  Assert(WideTrimLeft(' X ') = 'X ', 'WideTrimLeft');
  Assert(WideTrimRight(' X ') = ' X', 'WideTrimRight');
  Assert(WideTrim(' X ') = 'X', 'WideTrim');

  Assert(WideDup('X', 0) = '', 'WideDup');
  Assert(WideDup('X', 1) = 'X', 'WideDup');
  Assert(WideDup('A', 4) = 'AAAA', 'WideDup');

  S := 'AXAYAA';
  WideReplaceChar('A', '', S);
  Assert(S = 'XY', 'WideReplaceChar');
  S := 'AXAYAA';
  WideReplaceChar('A', 'B', S);
  Assert(S = 'BXBYBB', 'WideReplaceChar');
  S := 'AXAYAA';
  WideReplaceChar('A', 'CC', S);
  Assert(S = 'CCXCCYCCCC', 'WideReplaceChar');
  S := 'AXAXAA';
  WideReplaceChar('X', 'IJK', S);
  Assert(S = 'AIJKAIJKAA', 'WideReplaceChar');
end;



end.

