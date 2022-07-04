{                                                                              }
{                        Random number functions v3.09                         }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cRandom.pas                     }
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
{ Revision history:                                                            }
{   1999/11/07  0.01  Add RandomSeed.                                          }
{   1999/12/01  0.02  Add RandomUniform.                                       }
{   1999/12/03  0.03  Add RandomNormal.                                        }
{   2000/01/23  1.04  Add RandomPseudoWord.                                    }
{   2000/07/13  1.05  Fix bug reported by Andrew Driazgov.                     }
{   2000/08/22  1.06  Add RandomHex.                                           }
{   2000/09/20  1.07  Improve RandomSeed.                                      }
{   2002/06/01  3.08  Create cRandom unit.                                     }
{   2003/08/09  3.09  Replace random number generator.                         }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cRandom;

interface



{                                                                              }
{ RandomSeed                                                                   }
{   Returns a random seed value based on various system states.                }
{                                                                              }
function  RandomSeed: LongWord;



{                                                                              }
{ Uniform random number generator                                              }
{   Returns a random number from a uniform density distribution (ie all number }
{   have an equal probability of being 'chosen')                               }
{   RandomFloat returns an random floating point value between 0 and 1.        }
{   RandomPseudoWord returns a random word-like string.                        }
{                                                                              }
function  RandomUniform: LongWord; overload;
function  RandomUniform(const N: Integer): Integer; overload;
function  RandomBoolean: Boolean;
function  RandomByte: Byte;
function  RandomInt64: Int64; overload;
function  RandomInt64(const N: Int64): Int64; overload;
function  RandomHex(const Digits: Integer = 8): String;
function  RandomFloat: Extended;
function  RandomAlphaStr(const Length: Integer): String;
function  RandomPseudoWord(const Length: Integer): String;

// Alternative random number generators
function  mwcRandomLongWord: LongWord;
function  urnRandomLongWord: LongWord;
function  moaRandomFloat: Extended;
function  mwcRandomFloat: Extended;



{                                                                              }
{ Normal distribution random number generator                                  }
{   RandomNormalF returns a random number that has a Normal(0,1) distribution  }
{   (Gaussian distribution)                                                    }
{                                                                              }
function  RandomNormalF: Extended;



{                                                                              }
{ GUIDs                                                                        }
{                                                                              }
type
  TGUID128 = Array[0..3] of LongWord;

function  GenerateGUID32: LongWord;
function  GenerateGUID64: Int64;
function  GenerateGUID128: TGUID128;

function  GUID128ToHex(const GUID: TGUID128): String;



implementation

uses
  { Delphi }
  Windows,
  SysUtils,
  Math;



{                                                                              }
{ Linear Congruential Random Number Generators                                 }
{   The general form of a linear congruential generator is:                    }
{   SEED = (A * SEED + C) mod M                                                }
{                                                                              }
function lcRandom1(const Seed: LongWord): LongWord;
begin
  Result := LongWord(29943829 * Int64(Seed) - 1);
end;

function lcRandom2(const Seed: LongWord): LongWord;
begin
  Result := LongWord(69069 * Int64(Seed) + 1);
end;

function lcRandom3(const Seed: LongWord): LongWord;
begin
  Result := LongWord(1103515245 * Int64(Seed) + 12345);
end;

function lcRandom4(const Seed: LongWord): LongWord;
begin
  Result := LongWord(214013 * Int64(Seed) + 2531011);
end;

function lcRandom5(const Seed: LongWord): LongWord;
begin
  Result := LongWord(134775813 * Int64(Seed) + 1);
end;



{                                                                              }
{ RandomSeed                                                                   }
{                                                                              }
var
  StartupSeed   : Int64 = 0;
  FixedSeedInit : Boolean = False;
  FixedSeed     : LongWord = 0;
  VariableSeed  : LongWord = 0;

procedure InitFixedSeed;
var L : LongWord;
    B : Array[0..258] of Byte;

  function ApplyBuffer(const S: LongWord): LongWord;
  var I : Integer;
  begin
    Result := S;
    if L > 0 then
      For I := 0 to StrLen(PChar(@B)) - 1 do
        Result := Result xor (LongWord(B[I]) shl ((I mod 7) * 4));
  end;

var S : LongWord;
    P : Int64;
    Q : Pointer;
    T : LongWord;
begin
  S := $A5F04182;
  { Pointer values }
  Q := @FixedSeed;
  S := LongWord(Int64(S) + LongWord(Q));
  Q := @B;
  S := LongWord(Int64(S) + LongWord(Q));
  { Startup Seed }
  S := S xor LongWord(StartupSeed) xor LongWord(StartupSeed shr 32);
  {$IFDEF OS_WIN32}
  { CPU Frequency }
  if QueryPerformanceFrequency(P) then
    S := S xor LongWord(P) xor LongWord(P shr 32);
  { OS User Name }
  L := 256;
  if GetUserName(@B, L) then
    S := ApplyBuffer(S);
  { OS Computer Name }
  L := 256;
  if GetComputerName(@B, L) then
    S := ApplyBuffer(S);
  { OS Timing }
  T := GetTickCount;
  While GetTickCount = T do
    begin
      Sleep(0);
      S := lcRandom4(S);
      if QueryPerformanceCounter(P) then
        S := LongWord(Int64(S) + LongWord(P) + LongWord(P shr 32));
    end;
  {$ENDIF}
  { Randomize bits }
  S := lcRandom2(lcRandom1(S));
  { Save fixed seed }
  FixedSeed := S;
  FixedSeedInit := True;
end;

function RandomSeed: LongWord;
var P            : Int64;
    Ye, Mo, Da   : Word;
    H, Mi, S, S1 : Word;
begin
  {$IFDEF CPU_INTEL386}
  { CPU Registers }
  asm
    lahf
    add eax, ebx
    adc eax, ecx
    adc eax, edx
    adc eax, esi
    adc eax, edi
    mov Result, eax
  end;
  {$ELSE}
  Result := 0;
  {$ENDIF}
  { Fixed Seed }
  if not FixedSeedInit then
    InitFixedSeed;
  Result := Result xor FixedSeed;
  { System Date }
  DecodeDate(Date, Ye, Mo, Da);
  Result := Result xor Ye xor (Mo shl 16) xor (Da shl 24);
  { System Time }
  DecodeTime(Time, H, Mi, S, S1);
  Result := Result xor H xor (Mi shl 8) xor (S1 shl 16) xor (S shl 24);
  {$IFDEF OS_WIN32}
  { OS Counter }
  Result := Result xor GetTickCount;
  { OS Handles }
  Result := Result xor GetCurrentProcessID
                   xor GetCurrentThreadID;
  { CPU Counter }
  if QueryPerformanceCounter(P) then
    Result := LongWord(Int64(Result) + LongWord(P) + LongWord(P shr 32));
  {$ENDIF}
  { Variable Seed }
  Result := LongWord(Int64(Result) + VariableSeed);
  VariableSeed := lcRandom5(lcRandom4(Result));
  { Randomize bits }
  Result := lcRandom3(lcRandom1(Result));
end;



{                                                                              }
{ Mother-of-All pseudo random number generator                                 }
{   This is a multiply-with-carry or recursion-with-carry generator.           }
{   It has a cycle length of 3E+47.                                            }
{   It was invented by George Marsaglia.                                       }
{                                                                              }
var
  moaSeeded : Boolean = False;
  moaX      : Array[0..3] of LongWord;
  moaC      : LongWord;

procedure moaInitSeed(const Seed: LongWord);
var I : Integer;
    S : LongWord;
begin
  S := Seed;
  For I := 0 to 3 do
    begin
      S := lcRandom1(S);
      moaX[I] := S;
    end;
  moaC := lcRandom1(S);
  moaSeeded := True;
end;

function moaRandomLongWord: LongWord;
var S  : Int64;
    Xn : LongWord;
begin
  if not moaSeeded then
    moaInitSeed(RandomSeed);
  S := 2111111111 * Int64(moaX[0]) +
             1492 * Int64(moaX[1]) +
             1776 * Int64(moaX[2]) +
             5115 * Int64(moaX[3]) +
                    Int64(moaC);
  moaC := LongWord(S shr 32);
  Xn := LongWord(S);
  moaX[0] := moaX[1];
  moaX[1] := moaX[2];
  moaX[2] := moaX[3];
  moaX[3] := Xn;
  Result := Xn;
end;

function moaRandomFloat: Extended;
begin
  Result := moaRandomLongWord / High(LongWord);
end;



{                                                                              }
{ Multiply-With-Carry pseudo random number generator mentioned by George       }
{ Marsaglia in his paper on the Mother-of-All generator:                       }
{   " Here is an interesting simple MWC generator with period > 2^92, for      }
{   32-bit arithmetic:                                                         }
{   x[n]=1111111464*(x[n-1]+x[n-2]) + carry mod 2^32.                          }
{   Suppose you have functions, say top() and bot(), that give the top and     }
{   bottom halves of a 64-bit result.  Then, with initial 32-bit x, y and      }
{   carry c,  simple statements such as                                        }
{          y=bot(1111111464*(x+y)+c)                                           }
{          x=y                                                                 }
{          c=top(y)                                                            }
{   will, repeated, give over 2^92 random 32-bit y's. "                        }
{                                                                              }
var
  mwcSeeded : Boolean = False;
  mwcX      : LongWord;
  mwcY      : LongWord;
  mwcC      : LongWord;

procedure mwcInitSeed(const Seed: LongWord);
begin
  mwcX := lcRandom2(Seed);
  mwcY := lcRandom2(mwcX);
  mwcC := lcRandom2(mwcY);
  mwcSeeded := True;
end;

function mwcRandomLongWord: LongWord;
var S : Int64;
begin
  if not mwcSeeded then
    mwcInitSeed(RandomSeed);
  S := 1111111464 * (Int64(mwcX) + mwcY) + mwcC;
  Result := LongWord(S);
  mwcX := mwcY;
  mwcY := Result;
  mwcC := LongWord(S shr 32);
end;

function mwcRandomFloat: Extended;
begin
  Result := mwcRandomLongWord / High(LongWord);
end;



{                                                                              }
{ Universal random number generator proposed by Marsaglia, Zaman, and Tsang.   }
{ FSU-SCRI-87-50                                                               }
{   It has a period of 2^144 = 2E+43.                                          }
{   Only 24 bits are guarantueed to be completely random.                      }
{   This generator passes all known statistical tests on randomness.           }
{   The algorithm is a combination of a Fibonacci sequence and an arithmetic   }
{   sequence.                                                                  }
{                                                                              }
var
  urnSeeded : Boolean = False;
  urnU      : Array[1..97] of Double;
  urnC      : Double;
  urnCD     : Double;
  urnCM     : Double;
  urnI      : Integer;
  urnJ      : Integer;

procedure urnInit(const IJ, KL: Integer);
var I, J, K, L : Integer;
    F, G, M    : Integer;
    S, T       : Double;
begin
  Assert((IJ >= 0) and (IJ <= 31328) and (KL >= 0) and (KL <= 30081));
  I := (IJ div 177) mod 177 + 2;
  J := IJ mod 177 + 2;
  K := (KL div 169) mod 178 + 1;
  L := KL mod 169;
  for F := 1 to 97 do
    begin
      S := 0.0;
      T := 0.5;
      for G := 1 to 24 do
        begin
	  M := (((I * J) mod 179) * K) mod 179;
	  I := J;
	  J := K;
	  K := M;
	  L := (53 * L + 1) mod 169;
	  if ((L * M) mod 64 >= 32) then
            S := S + T;
	  T := T * 0.5;
        end;
      urnU[F] := S;
    end;
  urnC  := 362436.0 / 16777216.0;
  urnCD := 7654321.0 / 16777216.0;
  urnCM := 16777213.0 / 16777216.0;
  urnI  := 97;
  urnJ  := 33;
  urnSeeded := True;
end;

procedure urnInitSeed(const Seed: LongWord);
begin
  urnInit((Seed and $FFFF) mod 30000, (Seed shr 16) mod 30000);
end;

function urnRandomFloat: Double;
var R : Double;
begin
  if not urnSeeded then
    urnInitSeed(RandomSeed);
  R := urnU[urnI] - urnU[urnJ];
  if R < 0.0 then
    R := R + 1.0;
  urnU[urnI] := R;
  Dec(urnI);
  if urnI = 0 then
    urnI := 97;
  Dec(urnJ);
  if urnJ = 0 then
    urnJ := 97;
  urnC := urnC - urnCD;
  if urnC < 0.0 then
    urnC := urnC + urnCM;
  R := R - urnC;
  if R < 0.0 then
    R := R + 1.0;
  Result := R;
end;

function urnRandomLongWord: LongWord;
begin
  Result := LongWord(Trunc(urnRandomFloat * 4294967295.0));
end;



{                                                                              }
{ Uniform Random                                                               }
{                                                                              }
function RandomUniform: LongWord;
begin
  Result := moaRandomLongWord;
end;

function RandomUniform(const N: Integer): Integer;
begin
  if N <= 0 then
    Result := 0
  else
    Result := Integer(RandomUniform mod LongWord(N));
end;

function RandomBoolean: Boolean;
begin
  Result := RandomUniform and 1 = 1;
end;

function RandomByte: Byte;
begin
  Result := Byte(RandomUniform and $FF);
end;

function RandomFloat: Extended;
begin
  Result := urnRandomFloat;
end;

function RandomInt64: Int64;
begin
  Int64Rec(Result).Lo := RandomUniform;
  Int64Rec(Result).Hi := RandomUniform;
end;

function RandomInt64(const N: Int64): Int64;
begin
  if N <= 0 then
    Result := 0
  else
    begin
      Result := RandomInt64;
      if Result < 0 then
        Result := -Result;
      Result := Result mod N;
    end;
end;

function RandomHex(const Digits: Integer): String;
var I : Integer;
begin
  Result := '';
  Repeat
    I := Digits - Length(Result);
    if I > 0 then
      Result := Result + IntToHex(RandomUniform, 8);
  Until I <= 0;
  SetLength(Result, Digits);
end;

function RandomAlphaStr(const Length: Integer): String;
var I : Integer;
begin
  if Length <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  For I := 1 to Length do
    Result[I] := Char(Ord('A') + RandomUniform(26));
end;

function RandomPseudoWord(const Length: Integer): String;
const Vowels = 'AEIOUY';
      Consonants = 'BCDFGHJKLMNPQRSTVWXZ';
var I, A, P, T : Integer;
begin
  if Length <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  P := -1;
  A := RandomUniform(2);
  For I := 1 to Length do
    begin
      Case A of
        0 : Result[I] := Vowels[RandomUniform(6) + 1];
        1 : Result[I] := Consonants[RandomUniform(20) + 1];
      end;
      T := A;
      if A = P then
        A := A xor 1
      else
        A := RandomUniform(2);
      P := T;
    end;
end;



{                                                                              }
{ Normal Random                                                                }
{                                                                              }
var
  HasRandomNormal : Boolean = False;
  ARandomNormal   : Extended;

function RandomNormalF: Extended;
var fac, r, v1, v2: Extended;
begin
  if not HasRandomNormal then
    begin
      Repeat
        v1 := 2.0 * RandomFloat - 1.0;
        v2 := 2.0 * RandomFloat - 1.0;
        r := Sqr(v1) + Sqr(v2);
      Until r < 1.0;
      fac := Sqrt(-2.0 * ln(r) / r);
      ARandomNormal := v1 * fac;
      Result := v2 * fac;
      HasRandomNormal := True;
    end else
    begin
      Result := ARandomNormal;
      HasRandomNormal := False;
    end;
end;



{                                                                              }
{ GUID                                                                         }
{                                                                              }
var
  GUIDInit : Boolean = False;
  GUIDBase : TGUID128 = (0, 0, 0, 0);

procedure InitGUID;
var I : Integer;
begin
  GUIDBase[0] := RandomSeed;
  For I := 1 to 3 do
    GUIDBase[I] := RandomUniform;
  GUIDInit := True;
end;

function GenerateGUID32: LongWord;
begin
  if not GUIDInit then
    InitGUID;
  Result := GUIDBase[3];
  GUIDBase[3] := LongWord(GUIDBase[3] + 1);
end;

function GenerateGUID64: Int64;
begin
  if not GUIDInit then
    InitGUID;
  Int64Rec(Result).Hi := GUIDBase[2];
  Int64Rec(Result).Lo := GUIDBase[3];
  GUIDBase[3] := LongWord(GUIDBase[3] + 1);
end;

function GenerateGUID128: TGUID128;
begin
  if not GUIDInit then
    InitGUID;
  Result := GUIDBase;
  GUIDBase[3] := LongWord(GUIDBase[3] + 1);
  if GUIDBase[3] = 0 then
    GUIDBase[2] := LongWord(GUIDBase[2] + 1);
  GUIDBase[1] := RandomUniform;
end;

function GUID128ToHex(const GUID: TGUID128): String;
begin
  Result := IntToHex(GUIDBase[0], 8) +
            IntToHex(GUIDBase[1], 8) +
            IntToHex(GUIDBase[2], 8) +
            IntToHex(GUIDBase[3], 8);
end;



initialization
  {$IFDEF OS_WIN32}
  QueryPerformanceCounter(StartupSeed);
  StartupSeed := StartupSeed xor GetTickCount;
  {$ENDIF}
end.

