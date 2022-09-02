{*********************************************************}
{* FlashFiler: Random number encryption for tables       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}
{$Q-}
{$R-}

unit fftbcryp;

interface

uses
  SysUtils,
  ffllbase;

procedure FFCodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
procedure FFCodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
procedure FFDecodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
procedure FFDecodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);

implementation

{Note: we use 4 linear-congruential random number generators. All are
       of the form R(N+1) := ((R(N) * ia) + ic) mod im. All have been
       chosen so that the intermediate multiplication does not exceed
       2^31, ie, so that it fits in a longint without overflow. These
       4 generators populate a shuffle table, and random numbers are
       extracted out of this to supply bytes for an XOR table. It is
       this table that is used to encrypt/decrypt a block.

       Random number references: Numerical Recipes in Pascal.

       The seeds for the 4 random number generators are built from
       hashes of people's names; just for fun.
}

const
  ffc_CodeBlockLength = 4096; {DO NOT CHANGE: FFCodeBlock is optimised for this value}
  ffc_RandTableCount = 55;

const
  im1 = 243000; ia1 = 4561; ic1 = 51349;
  im2 = 134456; ia2 = 8121; ic2 = 28411;
  im3 = 714025; ia3 = 1366; ic3 =150889;
  im4 = 214326; ia4 = 3613; ic4 = 45289;

const
  ffc_Seed1 = 226797638 mod im1;   {Elf hash of "gandalf"}
  ffc_Seed2 = 127453534 mod im2;   {Elf hash of "sauron"}
  ffc_Seed3 = 214225708 mod im3;   {Elf hash of "rivendell"}
  ffc_Seed4 =  99118931 mod im4;   {Elf hash of "frodo baggins"}

type
  PffCodeBlock = ^TffCodeBlock;
  TffCodeBlock = array [0..pred(ffc_CodeBlockLength)] of byte;
  PffRandomTable = ^TffRandomTable;
  TffRandomTable = array [0..pred(ffc_RandTableCount)] of TffWord32;

var
  CB : PffCodeBlock;
  CBServer : PffCodeBlock;
  RT : PffRandomTable;
  RTInx : integer;
  RTInxStep : integer;


{===FFCodeBlock======================================================}
procedure CodeBlockPrim;
register;
asm
  {eax => aBlock}
  {edx = aBlockLen}
  {ecx = aRandomizer value}
  {esi => code block}
  push ebx
  add esi, ffc_CodeBlockLength
  and edx, $FFFFFFF0
  add eax, edx
  neg edx
  mov ebx, ecx
  shr ebx, 8
  and ebx, $FF0
  jz @@ResetCode
  neg ebx
  jmp @@NextLong
@@ResetCode:
  mov ebx, ffc_CodeBlockLength
  neg ebx
@@NextLong:
  mov ecx, [eax+edx]
  xor ecx, [esi+ebx]
  mov [eax+edx], ecx
  add edx, 4
  jz @@Exit

  mov ecx, [eax+edx]
  xor ecx, [esi+ebx+4]
  mov [eax+edx], ecx
  add edx, 4
  jz @@Exit

  mov ecx, [eax+edx]
  xor ecx, [esi+ebx+8]
  mov [eax+edx], ecx
  add edx, 4
  jz @@Exit

  mov ecx, [eax+edx]
  xor ecx, [esi+ebx+12]
  mov [eax+edx], ecx
  add edx, 4
  jz @@Exit

  add ebx, 16
  jnz @@NextLong
  jmp @@ResetCode
@@Exit:
  pop ebx
end;
{--------}
procedure FFCodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
register;
asm
  push esi
  mov esi, CB
  call CodeBlockPrim
  pop esi
end;
{--------}
procedure FFCodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
register;
asm
  push esi
  mov esi, CBServer
  call CodeBlockPrim
  pop esi
end;
{--------}
procedure FFDecodeBlock(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
register;
asm
  push esi
  mov esi, CB
  call CodeBlockPrim
  pop esi
end;
{--------}
procedure FFDecodeBlockServer(aBlock : PffByteArray; aBlockLen : TffWord32; aRandomizer : TffWord32);
register;
asm
  push esi
  mov esi, CBServer
  call CodeBlockPrim
  pop esi
end;
{====================================================================}


{===Helper routines==================================================}
function GetRandomByte : byte;
var
  LI : LongInt;
begin
  inc(RTinx);
  if (RTinx = ffc_RandTableCount) then
    RTInx := 0;
  inc(RTinxStep);
  if (RTinxStep = ffc_RandTableCount) then
    RTinxStep := 0;
  LI := RT^[RTInx] + RT^[RTInxStep];
  RT^[RTInx] := TffWord32(LI);
  {use the third byte}
  Result := (LI shr 16) and $FF;
end;
{--------}
procedure CalcRandTable;
type
  LongAsBytes = array [0..3] of byte;
var
  L : LongAsBytes;
  LI : longint absolute L;
  S1 : longint;
  S2 : longint;
  S3 : longint;
  S4 : longint;
  i  : integer;
begin
  {we use 4 linear-congruential random number generators, each
   generates a byte of each longint of the table}
  S1 := ffc_Seed1;
  S2 := ffc_Seed2;
  S3 := ffc_Seed3;
  S4 := ffc_Seed4;
  for i := 0 to pred(ffc_RandTableCount) do begin
    S1 := ((S1 * ia1) + ic1) mod im1;
    L[0] := (S1 * 256) div im1;
    S2 := ((S2 * ia2) + ic2) mod im2;
    L[1] := (S2 * 256) div im2;
    S3 := ((S3 * ia3) + ic3) mod im3;
    L[2] := (S3 * 256) div im3;
    S4 := ((S4 * ia4) + ic4) mod im4;
    L[3] := (S4 * 256) div im4;
    RT^[i] := LI;
  end;
  RTInx := 0;
  RTInxStep := 31;
  {rev the engine a bit}
  for i := 0 to pred(ffc_RandTableCount) do begin
    GetRandomByte;
  end;
end;
{--------}
procedure CalcRandomCodeBuffers;
var
  i : integer;
begin
  for i := 0 to pred(ffc_CodeBlockLength) do
    CB^[i] := GetRandomByte;
  for i := 0 to pred(ffc_CodeBlockLength) do
    CBServer^[i] := GetRandomByte;
end;
{====================================================================}


{===Initialization and finalization==================================}
procedure FinalizeUnit; 
begin
  if (CB <> nil) then begin
    FillChar(CB^, sizeof(CB^), 0);
    Dispose(CB);
  end;
  if (CBServer <> nil) then begin
    FillChar(CBServer^, sizeof(CBServer^), 0);
    Dispose(CBServer);
  end;
  if (RT <> nil) then begin
    FillChar(RT^, sizeof(RT^), 0);
    Dispose(RT);
  end;
end;
{--------}
procedure InitializeUnit;
begin
  New(CB);
  New(CBServer);
  New(RT);
  {first calculate the table of random longints}
  CalcRandTable;
  {next set each byte of the random code buffers}
  CalcRandomCodeBuffers;
end;
{====================================================================}


initialization
  InitializeUnit;

finalization
  FinalizeUnit;
end.

