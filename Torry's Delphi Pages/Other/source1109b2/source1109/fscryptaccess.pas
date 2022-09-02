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

{$I fsdefine.inc}
{$Q-}
{$R-}

Unit fscryptaccess;

Interface

Uses
  SysUtils,
  fsllbase;

Procedure FFCodeBlock(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
Procedure FFCodeBlockServer(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
Procedure FFDecodeBlock(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
Procedure FFDecodeBlockServer(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);

Implementation

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

Const
  ffc_CodeBlockLength = 4096; {DO NOT CHANGE: FFCodeBlock is optimised for this value}
  ffc_RandTableCount = 55;

Const
  im1 = 243000;
  ia1 = 4561;
  ic1 = 51349;
  im2 = 134456;
  ia2 = 8121;
  ic2 = 28411;
  im3 = 714025;
  ia3 = 1366;
  ic3 = 150889;
  im4 = 214326;
  ia4 = 3613;
  ic4 = 45289;

Const
  ffc_Seed1 = 226797638 Mod im1; {Elf hash of "gandalf"}
  ffc_Seed2 = 127453534 Mod im2; {Elf hash of "sauron"}
  ffc_Seed3 = 214225708 Mod im3; {Elf hash of "rivendell"}
  ffc_Seed4 = 99118931 Mod im4; {Elf hash of "frodo baggins"}

Type
  PffCodeBlock = ^TffCodeBlock;
  TffCodeBlock = Array[0..pred(ffc_CodeBlockLength)] Of Byte;
  PffRandomTable = ^TffRandomTable;
  TffRandomTable = Array[0..pred(ffc_RandTableCount)] Of TffWord32;

Var
  CB: PffCodeBlock;
  CBServer: PffCodeBlock;
  RT: PffRandomTable;
  RTInx: Integer;
  RTInxStep: Integer;

  {===FFCodeBlock======================================================}

Procedure CodeBlockPrim;
  Register;
Asm
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
End;
{--------}

Procedure FFCodeBlock(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
  Register;
Asm
  push esi
  mov esi, CB
  call CodeBlockPrim
  pop esi
End;
{--------}

Procedure FFCodeBlockServer(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
  Register;
Asm
  push esi
  mov esi, CBServer
  call CodeBlockPrim
  pop esi
End;
{--------}

Procedure FFDecodeBlock(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
  Register;
Asm
  push esi
  mov esi, CB
  call CodeBlockPrim
  pop esi
End;
{--------}

Procedure FFDecodeBlockServer(aBlock: PffByteArray; aBlockLen: TffWord32; aRandomizer: TffWord32);
  Register;
Asm
  push esi
  mov esi, CBServer
  call CodeBlockPrim
  pop esi
End;
{====================================================================}

{===Helper routines==================================================}

Function GetRandomByte: Byte;
Var
  LI: Longint;
Begin
  inc(RTinx);
  If (RTinx = ffc_RandTableCount) Then
    RTInx := 0;
  inc(RTinxStep);
  If (RTinxStep = ffc_RandTableCount) Then
    RTinxStep := 0;
  LI := RT^[RTInx] + RT^[RTInxStep];
  RT^[RTInx] := TffWord32(LI);
  {use the third byte}
  Result := (LI Shr 16) And $FF;
End;
{--------}

Procedure CalcRandTable;
Type
  LongAsBytes = Array[0..3] Of Byte;
Var
  L: LongAsBytes;
  LI: Longint Absolute L;
  S1: Longint;
  S2: Longint;
  S3: Longint;
  S4: Longint;
  i: Integer;
Begin
  {we use 4 linear-congruential random number generators, each
   generates a byte of each longint of the table}
  S1 := ffc_Seed1;
  S2 := ffc_Seed2;
  S3 := ffc_Seed3;
  S4 := ffc_Seed4;
  For i := 0 To pred(ffc_RandTableCount) Do
    Begin
      S1 := ((S1 * ia1) + ic1) Mod im1;
      L[0] := (S1 * 256) Div im1;
      S2 := ((S2 * ia2) + ic2) Mod im2;
      L[1] := (S2 * 256) Div im2;
      S3 := ((S3 * ia3) + ic3) Mod im3;
      L[2] := (S3 * 256) Div im3;
      S4 := ((S4 * ia4) + ic4) Mod im4;
      L[3] := (S4 * 256) Div im4;
      RT^[i] := LI;
    End;
  RTInx := 0;
  RTInxStep := 31;
  {rev the engine a bit}
  For i := 0 To pred(ffc_RandTableCount) Do
    Begin
      GetRandomByte;
    End;
End;
{--------}

Procedure CalcRandomCodeBuffers;
Var
  i: Integer;
Begin
  For i := 0 To pred(ffc_CodeBlockLength) Do
    CB^[i] := GetRandomByte;
  For i := 0 To pred(ffc_CodeBlockLength) Do
    CBServer^[i] := GetRandomByte;
End;
{====================================================================}

{===Initialization and finalization==================================}

Procedure FinalizeUnit;
Begin
  If (CB <> Nil) Then
    Begin
      FillChar(CB^, sizeof(CB^), 0);
      Dispose(CB);
    End;
  If (CBServer <> Nil) Then
    Begin
      FillChar(CBServer^, sizeof(CBServer^), 0);
      Dispose(CBServer);
    End;
  If (RT <> Nil) Then
    Begin
      FillChar(RT^, sizeof(RT^), 0);
      Dispose(RT);
    End;
End;
{--------}

Procedure InitializeUnit;
Begin
  New(CB);
  New(CBServer);
  New(RT);
  {first calculate the table of random longints}
  CalcRandTable;
  {next set each byte of the random code buffers}
  CalcRandomCodeBuffers;
End;
{====================================================================}

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;
End.

