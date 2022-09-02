{********************************************************************}
{* NUMKEY32.PAS - numeric to key conversion                         *}
{********************************************************************}

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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{Notes: 1. Real type support has been discarded
        2. The CStyle key routines not implemented yet
        3. IntToKey and KeyToInt are now called Int16ToKey and KeyToInt16
        4. LongToKey and KeyToLong are now called Int32ToKey and KeyToInt32
        5. There is no support yet for 32-bit unsigned integers}

{$IFNDEF Win32}
  !! Error - this unit can only be compiled for 32-bit
{$ENDIF}

unit NumKey32;

{$A-} {No alignment}
{$H-} {No long string support--all short string routines}
{$V-} {No var string checking}

interface

type
  String1  = String[1];
  String2  = String[2];
  String4  = String[4];
  String5  = String[5];
  String6  = String[6];
  String7  = String[7];
  String8  = String[8];
  String9  = String[9];
  String10 = String[10];

{---routines for numeric conversions---}
function ShortToKey(S : ShortInt) : String1;
  {-Convert a shortint to a string}

function KeyToShort(const S : String1) : ShortInt;
  {-Convert a string to a shortint}

function ByteToKey(B : Byte) : String1;
  {-Convert a byte to a string}

function KeyToByte(const S : String1) : Byte;
  {-Convert a string to a byte}

function Int16ToKey(I : SmallInt) : String2;
  {-Convert an integer to a string}

function KeyToInt16(const S : String2) : SmallInt;
  {-Convert a string to an integer}

function WordToKey(W : Word) : String2;
  {-Convert a word to a string}

function KeyToWord(const S : String2) : Word;
  {-Convert a string to a word}

function Int32ToKey(L : LongInt) : String4;
  {-Convert a longint to a string}

function KeyToInt32(const S : String4) : LongInt;
  {-Convert a string to a longint}

function BcdToKey(var B) : String10;                                   {!!.53}
  {-Convert a BCD real to a string}

procedure KeyToBcd(S : String10; var B);                               {!!.53}
  {-Convert a string to a BCD real}

{Note:
 Although the following two routines work with Extendeds, you can also use
 them with singles, doubles, and comps. You may use any of the following
 string types to hold the results of ExtToKey without losing any precision:

    Single    : String5 (min) - String10 (max)
    Double    : String9 (min) - String10 (max)
    Extended  : String10 (min/max)
    Comp      : String10 (min/max)

 Slightly shorter strings (one less than the recommended minimum) may be used
 for singles, doubles, and extendeds if you are willing to sacrifice some
 precision, however. We strongly recommend that you always use a String10 for
 comps.
}
function ExtToKey(E : Extended) : String10;
  {-Convert an extended to a string}

function KeyToExt(S : String10) : Extended;
  {-Convert a string to an extended}


{---routines for packing/unpacking keys---}
function Pack4BitKey(Src : string; Len : Byte) : string;
  {-Pack the Source string into sequences of 4 bits (max length = Len).
    The following characters are mapped to 1-15, respectively, all others
    to 0: '(', ')', '+', '-', '.', '0'..'9'.}

function Pack5BitKeyUC(Src : string; Len : Byte) : string;
  {-Pack the Source string into sequences of 5 bits (max length = Len).
    Characters from 'a' to 'z' converted to upper case, then characters from
    'A' to 'Z' are mapped to 1-26, all others to 0.}

function Pack6BitKeyUC(Src : string; Len : Byte) : string;
  {-Pack the Source string into sequences of 6 bits (max length = Len).
    Characters from 'a' to 'z' converted to upper case, then characters from
    '!' to '_' are mapped to 1-63, all others to 0.}

function Pack6BitKey(Src : string; Len : Byte) : string;
  {-Pack the Source string into sequences of 6 bits (max length = Len).
    Characters from '0'-'9' mapped to 1-10, 'A'-'Z' to 11-36, 'a'-'z' to
    37-62, all others to 0.}

function Unpack4BitKey(Src : string) : string;
  {-Unpack a key created by Pack4BitKey}

function Unpack5BitKeyUC(Src : string) : string;
  {-Unpack a key created by Pack5BitKeyUC}

function Unpack6BitKeyUC(Src : string) : string;
  {-Unpack a key created by Pack6BitKeyUC}

function Unpack6BitKey(Src : string) : string;
  {-Unpack a key created by Pack6BitKey}


{---miscellaneous routines---}
function DescendingKey(S : string; MaxLen : Byte) : string;
  {-Invert values in S to allow descending sorts, pad to MaxLen with #$FF}


implementation

type
  nkBCD = array[1..10] of Byte;

const
  Pack4Table : array [0..17] of byte =
               {(  )  *  +  ,  -  .  /  0  1  2   3  4   5   6   7   8   9}
               (1, 2, 0, 3, 0, 4, 5, 0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
  Unpack4Table : array [0..15] of char =
                 (' ', '(', ')', '+', '-', '.', '0', '1',
                  '2', '3', '4', '5', '6', '7', '8', '9');


{===Helper routines==================================================}
procedure ReverseBytes(var V; Size : Word); assembler;
  {-Reverse the ordering of bytes from V[1] to V[Size]. Size must be >= 2.}
  asm
    push ebx
    movzx edx, dx
    mov ecx, eax
    add ecx, edx
    dec ecx
    shr edx, 1
  @@Again:
    mov bl, [eax]
    xchg bl, [ecx]
    mov [eax], bl
    inc eax
    dec ecx
    dec edx
    jnz @@Again
    pop ebx
  end;
{--------}
procedure ToggleBits(var V; Size : Word); assembler;
  {-Toggle the bits from V[1] to V[Size]}
  asm
    movzx edx, dx
  @@Again:
    not byte ptr [eax]
    inc eax
    dec edx
    jnz @@Again
  end;
{--------}
function SwapWord(L : LongInt) : LongInt; assembler;
  {-Swap low- and high-order words of L}
  asm
    rol eax, 16
  end;
{--------}
procedure ZeroPad(var S : ShortString; Len : Word);
  {-Pad S to length Len with 0's}
  var
    SLen  : Byte absolute S;
  begin
    if SLen < Len then
      begin
        FillChar(S[Succ(SLen)], Len-SLen, 0);
        SLen := Len;
      end;
  end;
{--------}
function Pack4Prim(C : byte) : byte; assembler;
  asm
    cmp al, '('                  {less than '('?}
    jb @@ZeroIt                  {yes, return zero}
    cmp al, '9'                  {greater than '9'?}
    ja @@ZeroIt                  {yes, return zero}
    push ebx                     {save ebx}
    movzx ebx, al                {convert character in al}
    add ebx, OFFSET Pack4Table
    mov al, [ebx - '(']
    pop ebx                      {restore ebx}
    jmp @@Exit                   {exit}
  @@ZeroIt:
    xor al, al                   {return zero: is out of range}
  @@Exit:
  end;
{--------}
function Pack5UCPrim(C : byte) : byte; assembler;
  asm
    cmp al, 'z'       {greater than 'z'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, 'a'       {less than 'a'?}
    jb @@CheckAtoZ    {yes, go check on 'A'-'Z'}
    sub al, 96        {convert to 1..26}
    jmp @@Exit        {exit}
  @@CheckAtoZ:
    cmp al, 'Z'       {greater than 'Z'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, 'A'       {less than 'A'?}
    jb @@ZeroIt       {yes, return zero}
    sub al, 64        {convert to 1..26}
    jmp @@Exit        {exit}
  @@ZeroIt:
    xor al, al        {return zero: is out of range}
  @@Exit:
  end;
{--------}
function Pack6Prim(C : byte) : byte; assembler;
  asm
    cmp al, 'z'       {greater than 'z'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, 'a'       {less than 'a'?}
    jb @@CheckAtoZ    {yes, go check on 'A'-'Z'}
    sub al, 60        {convert to 37..62}
    jmp @@Exit        {exit}
  @@CheckAtoZ:
    cmp al, 'Z'       {greater than 'Z'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, 'A'       {less than 'A'?}
    jb @@Check0to9    {yes, go check on '0'-'9'}
    sub al, 54        {convert to 11..36}
    jmp @@Exit        {exit}
  @@Check0to9:
    cmp al, '9'       {greater than '9'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, '0'       {less than '0'?}
    jb @@ZeroIt       {yes, return zero}
    sub al, 47        {convert to 1..10}
    jmp @@Exit        {exit}
  @@ZeroIt:
    xor al, al        {return zero: is out of range}
  @@Exit:
  end;
{--------}
function Pack6UCPrim(C : byte) : byte; assembler;
  asm
    cmp al, '_'       {greater than '_'?}
    ja @@CheckAtoZ    {yes, go check on 'a'-'z'}
    cmp al, '!'       {less than '!'?}
    jb @@ZeroIt       {yes, return zero}
    sub al, 32        {convert to 1..63}
    jmp @@Exit        {exit}
  @@CheckAtoZ:
    cmp al, 'z'       {greater than 'z'?}
    ja @@ZeroIt       {yes, return zero}
    cmp al, 'a'       {less than 'a'?}
    jb @@ZeroIt       {yes, return zero}
    sub al, 64        {convert to 33..58}
    jmp @@Exit        {exit}
  @@ZeroIt:
    xor al, al        {return zero: is out of range}
  @@Exit:
  end;
{--------}
function Unpack4Prim(C : byte) : byte; assembler;
  asm
    push ebx                     {save ebx}
    movzx ebx, al                {convert character in al}
    add ebx, OFFSET Unpack4Table
    mov al, [ebx]
    pop ebx                      {restore ebx}
  end;
{--------}
function Unpack5UCPrim(C : byte) : byte; assembler;
  asm
    or al, al         {zero?}
    je @@Space        {yes, return space}
    add al, 32        {map to 'A'-'Z'}
  @@Space:
    add al, 32
  end;
{--------}
function Unpack6UCPrim(C : byte) : byte; assembler;
  asm
    add al, 32        {map to ' '-'Z'}
  end;
{--------}
function Unpack6Prim(C : byte) : byte; assembler;
  asm
    or al, al         {zero?}
    jz @@Space        {yes, return space}
    sub al, 10        {check for 1-10}
    jnle @@CheckAToZ  {no, go check for A-Z code}
    add al, 57        {convert to '0'..'9'}
    jmp @@Exit        {exit}
  @@Space:
    mov al, 32        {return space}
    jmp @@Exit
  @@CheckAToZ:
    sub al, 26        {convert 1..52 to 'A'..'Z', 'a'..'z'}
    jnle @@ItsaToz
    sub al, 6
  @@ItsaToz:
    add al, 96
  @@Exit:
  end;
{--------}
procedure GenericPack; assembler;
  asm
    {eax = Src; dl = Len; dh = BitsPerChar; ecx = result; ebx = pack function}
    {Note: references to [esp+4] are to the conversion routine}
    {                 to [esp] are the address of the final char in result}
    push edi              {save esi, edi}
    push esi
    push ebx              {save pack function on stack}
    mov esi, eax          {esi => Src}
    mov edi, ecx          {edi => result}

    xor eax, eax          {set eax to zero}
    mov al, dl            {al is length of result}
    mov [edi], al         {store in result}
    inc edi               {point to next dest char}
    add eax, edi          {calc final address of result on stack}
    push eax

    mov dl, 8             {dl = bytes per char delta}
    sub dl, dh

    mov bl, [esi]         {bl = # chars in Src}
    inc esi
    xor bh, bh            {bh = # bits in ah}

  @@Main:
    cmp edi, [esp]        {is result string full?}
    jae @@Exit            {yes, exit}
    or bl, bl             {any chars left in Src}
    jz @@Finish           {no, finish off the result string}
    mov al, [esi]         {get next char from Src}
    inc esi
    dec bl                {decrement number of chars left in Src}
    call dword ptr [esp+4]{convert it}
    mov cl, dl            {shift bottom of al to top of al}
    shl al, cl

    mov ch, dh            {ch = bits per char}
  @@BitLoop:
    cmp bh, 8             {8 bits set in ah yet?}
    jb @@NextBit          {no, go add next one}
    mov [edi], ah         {store ah in result string}
    inc edi
    xor bh, bh            {set bits in ah to zero}
  @@NextBit:
    rol al, 1             {get next bit from top of al into CF}
    rcl ah, 1             {push CF into bottom of ah}
    inc bh                {increment number of bits in ah}
    dec ch                {decrement bits to go}
    jnz @@BitLoop         {continue if more bits}
    jmp @@Main            {go get next character}

  @@Finish:               {time to finish up}
    or bh, bh             {any bits left in ah?}
    jz @@ZeroPad          {no, go pad result string with zeros}
    mov cl, 8             {calc number of bits to stuff ah with}
    sub cl, bh
    shl ah, cl            {..and do it}
    mov [edi], ah         {store ah in result string}
    inc edi

  @@ZeroPad:              {time to pad result string with zero bytes}
    mov ecx, [esp]        {calculate the number of characters}
    sub ecx, edi
    xor eax, eax
    cld
    rep stosb             {pad with #0s}

  @@Exit:
    add esp, 8            {toss local variables}
    pop esi               {restore edi, esi and ebx}
    pop edi
    pop ebx
  end;
{--------}
procedure GenericUnpack; assembler;
  asm
    {eax = Src; edx = result; cl = BitsPerChar; ebx = unpack function}
    push edi              {save esi, edi}
    push esi
    push edx              {save address of result[0]}
    mov esi, eax          {esi => Src}
    mov edi, edx          {edi => result}
    mov dh, cl            {dh = bits per character}
    mov ecx, ebx          {ecx = conversion routine}

    mov bl, [esi]         {bl = length of Src}
    inc esi
    xor bh, bh            {bh = number of bits in ah}

  @@Main:
    or bl, bl             {any characters left?}
    jz @@Finish           {no, go finish off}

    mov dl, dh            {cl = bits per character}
    xor al, al            {set new character to zero}

  @@BitLoop:
    or bh, bh             {any bits left in ah?}
    jnz @@Next            {yes, go rotate next bit}
    mov ah, [esi]         {get next character from Src}
    inc esi
    dec bl                {decrement number of chars left in Src}
    mov bh, 8             {another 8 bits to go through}
  @@Next:
    rol ah, 1             {get highest bit in ah into CF} 
    rcl al, 1             {rotate CF into bottom of al}
    dec bh                {decrement bits left in ah}
    dec dl                {decrement bits left to go for this char}
    jnz @@BitLoop

    call ecx              {convert al}
    inc edi               {put converted char into result string}
    mov [edi], al
    jmp @@Main            {go back for next conversion}

  @@Finish:
    cmp bh, dh            {is there a complete char in ah?}    {!!.55}
    jb @@Exit             {no, go write length byte}
    mov esi, ecx          {esi = conversion routine}           {!!.55}
    mov cl, 8             {calc shift value}
    sub cl, dh                                                 {!!.55}
    mov al, ah            {set unconverted char in al}
    shr al, cl
    call esi              {convert al}                         {!!.55}
    inc edi               {put converted char into result string}
    mov [edi], al

  @@Exit:
    mov eax, edi          {calc length}
    pop edi
    sub eax, edi
    mov [edi], al         {store length}

    pop esi               {restore edi, esi and ebx}
    pop edi
    pop ebx
  end;
{====================================================================}


{===Packed keys generation===========================================}
function Pack4BitKey(Src : string; Len : Byte) : string; assembler;
  asm
    push ebx
    mov dh, $04
    lea ebx, Pack4Prim
    jmp GenericPack
  end;
{--------}
function Pack5BitKeyUC(Src : string; Len : Byte) : string; assembler;
  asm
    push ebx
    mov dh, $05
    lea ebx, Pack5UCPrim
    jmp GenericPack
  end;
{--------}
function Pack6BitKeyUC(Src : string; Len : Byte) : string; assembler;
  asm
    push ebx
    mov dh, $06
    lea ebx, Pack6UCPrim
    jmp GenericPack
  end;
{--------}
function Pack6BitKey(Src : string; Len : Byte) : string; assembler;
  asm
    push ebx
    mov dh, $06
    lea ebx, Pack6Prim
    jmp GenericPack
  end;
{====================================================================}


{===Packed keys decompiling==========================================}
function Unpack4BitKey(Src : string) : string; assembler;
  asm
    push ebx
    mov cl, $04
    lea ebx, Unpack4Prim
    jmp GenericUnpack
  end;
{--------}
function Unpack5BitKeyUC(Src : string) : string; assembler;
  asm
    push ebx
    mov cl, $05
    lea ebx, Unpack5UCPrim
    jmp GenericUnpack
  end;
{--------}
function Unpack6BitKeyUC(Src : string) : string; assembler;
  asm
    push ebx
    mov cl, $06
    lea ebx, Unpack6UCPrim
    jmp GenericUnpack
  end;
{--------}
function Unpack6BitKey(Src : string) : string; assembler;
  asm
    push ebx
    mov cl, $06
    lea ebx, Unpack6Prim
    jmp GenericUnpack
  end;
{====================================================================}



{===Numeric to string conversions====================================}
function ShortToKey(S : ShortInt) : String1;
  {-Convert a shortint to a string}
  begin
    Result[0] := #1;
    Result[1] := Char(byte(S) xor $80);                        {!!.55}
  end;
{--------}
function KeyToShort(const S : String1) : ShortInt;
  {-Convert a string to a shortint}
  begin
    Result := ShortInt(byte(S[1]) xor $80);                    {!!.55}
  end;
{--------}
function ByteToKey(B : Byte) : String1;
  {-Convert a byte to a string}
  begin
    Result[0] := #1;
    Result[1] := Char(B);
  end;
{--------}
function KeyToByte(const S : String1) : Byte;
  {-Convert a string to a byte}
  begin
    Result := Byte(S[1]);
  end;
{--------}
function Int16ToKey(I : SmallInt) : String2;
 {-Convert a 16-bit integer to a string}
  var
    LResult : record
      Len : Byte;
      RI  : Word;
    end absolute Result;
  begin
    LResult.Len := 2;
    LResult.RI := Swap(Word(I) xor $8000);                     {!!.55}
  end;
{--------}
function KeyToInt16(const S : String2) : SmallInt;
  {-Convert a string to an integer}                  {rewritten !!.56}
asm
  mov ax, [eax+1]
  xchg ah, al
  xor ax, $8000
end;
{--------}
function WordToKey(W : Word) : String2;
  {-Convert a word to a string}
  var
    LResult : record
      Len : Byte;
      RW  : Word;
    end absolute Result;
  begin
    LResult.Len := 2;
    LResult.RW := Swap(W);
  end;
{--------}
function KeyToWord(const S : String2) : Word;
  {-Convert a string to a word}
  var
    Temp : record
      Len : Byte;
      W   : Word;
    end absolute S;
  begin
    Result := Swap(Temp.W);
  end;
{--------}
function Int32ToKey(L : LongInt) : String4;
  {-Convert a 32-bit integer to a string}
  var
    LRec : record
      L1 : Word;
      L2 : Word;
    end absolute L;
    LResult : record
      Len : Byte;
      W1, W2 : Word;
    end absolute Result;
  begin
    L := L xor longint($80000000);                             {!!.55}
    LResult.Len := 4;
    LResult.W1 := Swap(LRec.L2);
    LResult.W2 := Swap(LRec.L1);
  end;
{--------}
function KeyToInt32(const S : String4) : LongInt;
  {-Convert a string to a 32-bit integer}
  var
    Temp : record case Byte of
      0 : (Len : Byte; W1, W2 : Word);
      1 : (X : Byte; L : LongInt);
    end absolute S;
    LResult : record
      case byte of
        0 : (W1, W2 : word);
        1 : (L : longint);
    end;
  begin
    LResult.W1 := Swap(Temp.W2);
    LResult.W2 := Swap(Temp.W1);
    Result := LResult.L xor longint($80000000);                {!!.55}
  end;
{--------}
function BcdToKey(var B) : String10;
  {-Convert a BCD real to a string}
  var
    LResult : record
      case byte of
        0 : (Len : Byte; BT : nkBCD);
        1 : (XXX, Exp, LSB : Byte);
    end absolute Result;
    BP : nkBCD absolute B;
  begin
    {regularize 0 values}
    if BP[1] and $7F = 0 then
      FillChar(BP, SizeOf(BP), 0);

    LResult.BT := BP;

    {put the mantissa into MSB->LSB order}
    ReverseBytes(LResult.LSB, 9);

    {flip the sign bit}
    LResult.Exp := LResult.Exp xor $80;

    if LResult.Exp and $80 = 0 then begin
      ToggleBits(LResult.BT, 10);
      LResult.Exp := LResult.Exp and $7F;
    end;

    LResult.Len := 10;
  end;
{--------}
procedure KeyToBcd(S : String10; var B);
  {-Convert a string to a BCD real}
  var
    Temp : record case Byte of
      0 : (Len : Byte; BT : nkBCD);
      1 : (XXX, Exp, MSB : Byte);
    end absolute S;
    BR : nkBcd absolute B;
  begin
    {pad to proper length just in case}
    ZeroPad(S, 10);

    {flip the sign bit}
    Temp.Exp := Temp.Exp xor $80;

    if Temp.Exp and $80 <> 0 then begin
      ToggleBits(Temp.BT, 10);
      Temp.Exp := Temp.Exp or $80;
    end;

    {put the mantissa back into LSB->MSB order}
    ReverseBytes(Temp.MSB, 9);

    BR := Temp.BT;
  end;
{--------}
function ExtToKey(E : Extended) : String10;
  {-Convert an extended to a string}
  var
    LResult : record
      case Byte of
        0 : (Len : Byte; EE : Extended);
        1 : (XXX, Exp : Byte);
        2 : (Str : String10);
    end absolute Result;
  begin
    LResult.EE := E;

    {move the exponent to the front and put mantissa in MSB->LSB order}
    ReverseBytes(LResult.EE, 10);

    {flip the sign bit}
    LResult.Exp := LResult.Exp xor $80;

    if LResult.Exp and $80 = 0 then begin
      ToggleBits(LResult.EE, 10);
      LResult.Exp := LResult.Exp and $7F;
    end;

    LResult.Len := 10;
  end;
{--------}
function KeyToExt(S : String10) : Extended;
  {-Convert a string to an extended}
  var
    Temp : record case Byte of
      0 : (Len : Byte; EE : Extended);
      1 : (XXX, Exp : Byte);
      2 : (Str : String10);
    end absolute S;
  begin
    {pad to proper length just in case}
    ZeroPad(S, 10);

    {flip the sign bit}
    Temp.Exp := Temp.Exp xor $80;

    if Temp.Exp and $80 <> 0 then begin
      ToggleBits(Temp.EE, 10);
      Temp.Exp := Temp.Exp or $80;
    end;

    {move the exponent back to the end and put mantissa in LSB->MSB order}
    ReverseBytes(Temp.EE, 10);

    Result := Temp.EE;
  end;
{====================================================================}



{===Miscellaneous routines===========================================}
function DescendingKey(S : string; MaxLen : Byte) : string; assembler;
  asm
    {eax = S, dl = MaxLen, ecx = result string}
    push esi           {Save esi and edi}
    push edi
    mov esi, eax       {esi => S}
    mov edi, ecx       {edi => result string}
    movzx edx, dl      {edx = MaxLen}
    xor eax, eax       {eax = length of string}
    mov al, [esi]
    mov ecx, eax       {ecx = length of string}
    cmp ecx, edx       {length of result string = max(ecx, edx)}
    ja @@SetLength
    mov eax, edx
  @@SetLength:
    mov [edi], al
    sub edx, ecx       {calc amount of padding}
    or ecx, ecx        {is length of S zero}
    jz @@Pad           {yes, jump to padding routine}
  @@Again:
    inc esi            {point to next source character}
    inc edi            {point to next destination character}
    mov al, [esi]      {get next character}
    not al             {convert}
    mov [edi], al      {put next character}
    dec ecx            {any more characters in source string?}
    jnz @@Again        {yes, go do next}
  @@Pad:
    cmp edx, 0         {any padding to do?}
    jle @@Exit         {no, go tidy up}
    inc edi            {point at next destination character}
    mov al, $FF        {replicate required number of $FFs to pad}
    mov ecx, edx
    cld
    rep stosb
  @@Exit:
    pop edi            {restore esi and edi}
    pop esi
  end;


end.
