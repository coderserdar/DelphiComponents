{$INCLUDE ..\cDefines.inc}
unit cRandom;

{                                                                              }
{                        Random number functions v3.08                         }
{                                                                              }
{      This unit is copyright © 1999-2002 by David Butler (david@e.co.za)      }
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
{   1999/11/07  0.01  Added RandomSeed.                                        }
{   1999/12/01  0.02  Added RandomUniform.                                     }
{   1999/12/03  0.03  Added RandomNormal.                                      }
{   2000/01/23  1.04  Added RandomPseudoWord.                                  }
{   2000/07/13  1.05  Fixed bug in RandomUniform reported by Andrew Driazgov   }
{                     <andrey@asp.tstu.ru>                                     }
{   2000/08/22  1.06  Added RandomHex.                                         }
{   2000/09/20  1.07  Added more random states to RandomSeed.                  }
{   2002/06/01  3.08  Created cRandom unit from cSysUtils and cMaths.          }
{                                                                              }

interface



{                                                                              }
{ RandomSeed                                                                   }
{   Returns a random seed value, based on the Windows counter, the CPU counter }
{   and the current date/time and other 'random' states.                       }
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
function  RandomPseudoWord(const Length: Integer): String;



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
  SysUtils;



{                                                                              }
{ RandomSeed                                                                   }
{                                                                              }
function RandomSeed: LongWord;
var I            : Int64;
    Ye, Mo, Da   : Word;
    H, Mi, S, S1 : Word;
begin
  Result := $A5F04182;

  // Date
  DecodeDate(Date, Ye, Mo, Da);
  Result := Result xor Ye xor (Mo shl 16) xor (Da shl 24);

  // Time
  DecodeTime(Time, H, Mi, S, S1);
  Result := Result xor H xor (Mi shl 8) xor (S1 shl 16) xor (S shl 24);

  {$IFDEF OS_WIN32}
  // OS Counter (Ticks since OS start-up)
  Result := Result xor GetTickCount;

  // CPU Counter (Ticks since CPU start-up)
  if QueryPerformanceCounter(I) then
    Result := Result xor LongWord(I) xor LongWord(I shr 32);

  // CPU Frequency (Specific to CPU model)
  if QueryPerformanceFrequency(I) then
    Result := Result xor LongWord(I) xor LongWord(I shr 32);

  // Process
  Result := Result xor GetCurrentProcessID xor GetCurrentThreadID;
  {$ENDIF}
end;



{                                                                              }
{ Uniform Random                                                               }
{                                                                              }

{ Random number generator from ACM Transactions on Modeling and Computer       }
{ Simulation 8(1) 3-30, 1990.  Supposedly it has a period of -1 + 2^19937.     }
{ The original was in C; this translation returns the same values as the       }
{ original.  It is called the Mersenne Twister.                                }
{ The following code was written by Toby Ewing <ewing@iastate.edu>, slightly   }
{ modified by Frank Heckenbach <frank@pascal.gnu.de>.                          }
{ It was inspired by C code, released under the GNU Library General Public     }
{ License, written by Makoto Matsumoto <matumoto@math.keio.ac.jp> and          }
{ Takuji Nishimura, considering the suggestions by Topher Cooper and           }
{ Marc Rieffel in July-Aug 97.                                                 }
const
  N = 624; // Period parameters
  M = 397;

var
  mti : Integer;
  mt  : Array[0..N - 1] of LongWord; // the array for the state vector
  RandomUniformInitialized : Boolean = False;

{ Set initial seeds to mt [N] using the generator Line 25 of Table 1 in        }
{ [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp 102].      }
Procedure RandomUniformInit(const Seed: LongWord);
var I : Integer;
begin
  mt[0] := Seed;
  For I := 1 to N - 1 do
    mt[I] := LongWord(Int64(69069) * mt[I - 1]);
  mti := N;
  RandomUniformInitialized := True
end;

function RandomUniform: LongWord;
const
  Matrix_A = $9908B0DF; // constant vector a
  T_Mask_B = $9D2C5680; // Tempering parameters
  T_Mask_C = $EFC60000;
  Up_Mask  = $80000000; // most significant w-r bits
  Low_Mask = $7FFFFFFF; // least significant r bits
  mag01    : Array[0..1] of LongWord = (0, Matrix_A);

var
  y  : LongWord;
  kk : Integer;

begin
  if not RandomUniformInitialized then
    RandomUniformInit(RandomSeed);
  if mti >= N then { generate N words at one time }
    begin
      For kk := 0 to N - M - 1 do
        begin
          y := (mt[kk] and Up_Mask) or (mt[kk + 1] and Low_Mask);
          mt[kk] := mt[kk + M] xor (y shr 1) xor mag01[y and 1]
        end;
      For kk := N - M to N - 2 do
        begin
          y := (mt[kk] and Up_Mask) or (mt[kk + 1] and Low_Mask);
          mt[kk] := mt[kk + M - N] xor (y shr 1) xor mag01[y and 1]
        end;
      y := (mt[N - 1] and Up_Mask) or (mt[0] and Low_Mask);
      mt[N - 1] := mt[M - 1] xor (y shr 1) xor mag01[y and 1];
      mti := 0
    end;
  y := mt[mti];
  Inc(mti);
  y := y xor (y shr 11);
  y := y xor ((y shl 7) and T_Mask_B);
  y := y xor ((y shl 15) and T_Mask_C);
  y := y xor (y shr 18);
  Result := y;
end;

function RandomUniform(const N: Integer): Integer;
begin
  if N = 0 then
    Result := 0 else
    Result := Integer(Int64(RandomUniform) mod N);
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
  Result := RandomUniform / High(LongWord);
end;

function RandomInt64: Int64;
begin
  Int64Rec(Result).Lo := RandomUniform;
  Int64Rec(Result).Hi := RandomUniform;
end;

function RandomInt64(const N: Int64): Int64;
begin
  if N = 0 then
    Result := 0 else
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

function RandomPseudoWord(const Length: Integer): String;
const Vowels = 'AEIOUY';
      Consonants = 'BCDFGHJKLMNPQRSTVWXZ';
var I : Integer;
begin
  Assert(Length >= 0, 'RandomPseudoWord: Invalid Length parameter');
  SetLength(Result, Length);
  For I := 1 to Length do
    Case RandomUniform(2) of
      0 : Result[I] := Vowels[RandomUniform(6) + 1];
      1 : Result[I] := Consonants[RandomUniform(20) + 1];
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



end.

