{********************************************************************}
{* NUMKEYS.PAS - numeric to key conversion                          *}
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

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF CanAllowOverlays}
  {$F+,O+,A-}
{$ENDIF}
{$IFDEF UseSymFlex}
  {$N+}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

Unit
  Numkeys;

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

{---------------- routines for numeric conversions ----------------}

function ShortToKey(S : ShortInt) : String1;
  {-Convert a shortint to a string}

function KeyToShort(S : String1) : ShortInt;
  {-Convert a string to a shortint}

function ByteToKey(B : Byte) : String1; {!!.41}
  {-Convert a byte to a string}

function KeyToByte(S : String1) : Byte; {!!.41}
  {-Convert a string to a byte}

function IntToKey(I : Integer) : String2;
  {-Convert an integer to a string}

function KeyToInt(S : String2) : Integer;
  {-Convert a string to an integer}

function WordToKey(W : Word) : String2;
  {-Convert a word to a string}

function KeyToWord(S : String2) : Word;
  {-Convert a string to a word}

function LongToKey(L : LongInt) : String4;
  {-Convert a longint to a string}

function KeyToLong(S : String4) : LongInt;
  {-Convert a string to a longint}

function RealToKey(R : Real) : String6;
  {-Convert a real to a string}

function KeyToReal(S : String6) : Real;
  {-Convert a string to a real}

function BcdToKey(var B) : String10;
  {-Convert a BCD real to a string}

procedure KeyToBcd(S : String10; var B);
  {-Convert a string to a BCD real}

{$IFOPT N+}

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

{$ENDIF}

{---------------- routines for packing/unpacking keys ----------------}

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

{---------------- miscellaneous routines ----------------}

function DescendingKey(S : string; MaxLen : Byte) : string;
  {-Invert values in S to allow descending sorts, pad to MaxLen with #$FF}

function CStyleNumKey(S : String) : String; {!!.41}
  {-Convert Pascal-style numkey S into a C-style numkey, compatible with ASCIIZ}

function PascalStyleNumKey(S : String) : String; {!!.41}
  {-Convert C-style numkey S into a Pascal-style numkey}

function CStyleDescendingKey(S : string; MaxLen : byte) : string; {!!.41}
  {-Convert S to a descending key, using C-style algorithm}

  {=========================================================================}

implementation

var
  ProcPtr : Word;            {!!.22}

  {$L NUMKEYS.OBJ}

  function Pack4BitKey(Src : string; Len : Byte) : string;
    external;
  function Pack5BitKeyUC(Src : string; Len : Byte) : string;
    external;
  function Pack6BitKeyUC(Src : string; Len : Byte) : string;
    external;
  function Pack6BitKey(Src : string; Len : Byte) : string;
    external;

  function Unpack4BitKey(Src : string) : string;
    external;
  function Unpack5BitKeyUC(Src : string) : string;
    external;
  function Unpack6BitKeyUC(Src : string) : string;
    external;
  function Unpack6BitKey(Src : string) : string;
    external;

  function DescendingKey(S : string; MaxLen : Byte) : string;
    external;

  procedure ReverseBytes(var V; Size : Word);
    {-Reverse the ordering of bytes from V[1] to V[Size]. Size must be >= 2.}
  inline(
    $8C/$DB/                 {mov bx,ds    ;save DS}
    $59/                     {pop cx       ;CX = Size}
    $5E/                     {pop si}
    $1F/                     {pop ds       ;DS:SI => V[1]}
    $89/$F7/                 {mov di,si    ;DS:DI = V[Size]}
    $01/$CF/                 {add di,cx}
    $4F/                     {dec di}
    $D1/$E9/                 {shr cx,1}
    {                         again:}
    $8A/$04/                 {mov al,[si]  ;switch DS:DI}
    $86/$05/                 {xchg al,[di] ; and DS:DI}
    $88/$04/                 {mov [si],al}
    $46/                     {inc si       ;adjust indexes}
    $4F/                     {dec di}
    $E2/$F6/                 {loop again}
    $8E/$DB);                {mov ds,bx    ;restore DS}

  procedure ToggleBits(var V; Size : Word);        {!!.04}
    {-Toggle the bits from V[1] to V[Size]}        {!!.04}
  inline(
    $59/                     {pop cx  ;cx = Size}
    $5F/                     {pop di}
    $07/                     {pop es  ;es:di -> V}
                             {again:}
    $26/$F6/$15/             {not byte ptr es:[di] ;flip bits}
    $47/                     {inc di}
    $E2/$FA);                {loop again}


  function SwapWord(L : LongInt) : LongInt;
    {-Swap low- and high-order words of L}
  inline(
    $5A/                     {pop dx ;pop low word into DX}
    $58);                    {pop ax ;pop high word into AX}

  function FirstRotation(W : Word) : Word;
    {-Move sign bit from high bit of MSB of mantissa (AH) into high bit of
      exponent (AL), rotating low bit of exponent into its place.}
  inline(
    $58/                     {pop ax      ;AX = W}
    $80/$F4/$80/             {xor ah,$80  ;flip sign bit}
    $D0/$C4/                 {rol ah,1    ;rotate sign bit into CF}
    $D0/$D8/                 {rcr al,1    ;rotate sign bit into AL,}
    {                                     ;low bit of AL into CF}
    $D0/$DC);                {rcr ah,1    ;rotate low bit of AL into}
    {                                     ;high bit of AH}

  function SecondRotation(W : Word) : Word;
    {-Undo the work of the first rotation}
  inline(
    $58/                     {pop ax      ;AX = W}
    $34/$80/                 {xor al,$80  ;flip sign bit}
    $D0/$C4/                 {rol ah,1    ;rotate high bit of AH into CF}
    $D0/$D0/                 {rcl al,1    ;rotate it back into low bit of AL}
    {                                     ;sign bit out of AL into CF}
    $D0/$DC);                {rcr ah,1    ;rotate sign bit back into}
    {                                     ;high bit of AH}

  procedure ZeroPad(var S : String; Len : Word);
    {-Pad S to length Len with 0's}
  var
    SLen  : Byte absolute S;
  begin
    if SLen < Len then begin
      FillChar(S[Succ(SLen)], Len-SLen, 0);
      SLen := Len;
    end;
  end;

  function ShortToKey(S : ShortInt) : String1;
    {-Convert a shortint to a string}
  begin
    ShortToKey[0] := #1;
    ShortToKey[1] := Char(S xor $80);
  end;

  function KeyToShort(S : String1) : ShortInt;
    {-Convert a string to a shortint}
  begin
    KeyToShort := ShortInt(S[1]) xor $80;
  end;

  function ByteToKey(B : Byte) : String1; {!!.41}
    {-Convert a byte to a string}
  begin
    ByteToKey[0] := #1;
    ByteToKey[1] := Char(B);
  end;

  function KeyToByte(S : String1) : Byte; {!!.41}
    {-Convert a string to a byte}
  begin
    KeyToByte := Byte(S[1]);
  end;

  function IntToKey(I : Integer) : String2;
    {-Convert an integer to a string}
  const
    LResult :                                                   {!!.51}
      record case Byte of
        0 : (Len : Byte; RI : Word);
        1 : (Str : String2);
      end = (Str : '  ');
  begin
    LResult.RI := Swap(I xor $8000);                            {!!.51}
    IntToKey := LResult.Str;                                    {!!.51}
  end;

  function KeyToInt(S : String2) : Integer;
    {-Convert a string to an integer}
  var
    Temp :
      record
        Len : Byte;
        I   : Integer;
      end absolute S;
  begin
    KeyToInt := Swap(Temp.I) xor $8000;
  end;

  function WordToKey(W : Word) : String2;
    {-Convert a word to a string}
  const
    LResult :                                                   {!!.51}
      record case Byte of
        0 : (Len : Byte; RW : Word);
        1 : (Str : String2);
      end = (Str : '  ');
  begin
    LResult.RW := Swap(W);                                      {!!.51}
    WordToKey := LResult.Str;                                   {!!.51}
  end;

  function KeyToWord(S : String2) : Word;
    {-Convert a string to a word}
  var
    Temp :
      record
        Len : Byte;
        W   : Word;
      end absolute S;
  begin
    KeyToWord := Swap(Temp.W);
  end;

  function LongToKey(L : LongInt) : String4;
    {-Convert a longint to a string}
  const
    LResult :                                                   {!!.51}
      record case Byte of
        0 : (Len : Byte; W1, W2 : Word);
        1 : (Str : String4);
      end = (Str : '    ');
  var
    LRec :
      record
        L1 : Word;
        L2 : Word;
      end absolute L;
  begin
    L := L xor $80000000;
    LResult.W1 := Swap(LRec.L2);                                {!!.51}
    LResult.W2 := Swap(LRec.L1);                                {!!.51}
    LongToKey := LResult.Str;                                   {!!.51}
  end;

  function KeyToLong(S : String4) : LongInt;
    {-Convert a string to a longint}
  var
    Temp :
      record case Byte of
        0 : (Len : Byte; W1, W2 : Word);
        1 : (X : Byte; L : LongInt);
      end absolute S;
  begin
    Temp.W1 := Swap(Temp.W1);
    Temp.W2 := Swap(Temp.W2);
    KeyToLong := SwapWord(Temp.L) xor $80000000;
  end;

  function RealToKey(R : Real) : String6;
    {-Convert a real to a string}
  const
    Temp :
      record case Byte of
        0 : (Len : Byte; RR : Real);
        1 : (Str : String6);
        2 : (XXX, Exp, LSB : Byte);
        3 : (YYY : Byte; MantExp : Word);
      end = (Str : '      ');
  begin
    Temp.RR := R;

    {put the mantissa into MSB->LSB order}
    ReverseBytes(Temp.LSB, 5);

    {move the sign bit from the MSB of the mantissa into the high bit
     of the exponent, rotating the low bit of the exponent into its place}
    Temp.MantExp := FirstRotation(Temp.MantExp);

    if Temp.Exp and $80 = 0 then begin  {!!.04}
      ToggleBits(Temp.RR, 6);           {!!.04}
      Temp.Exp := Temp.Exp and $7F;     {!!.04}
    end;                                {!!.04}

    RealToKey := Temp.Str;
  end;

  function KeyToReal(S : String6) : Real;
    {-Convert a string to a real}
  var
    Temp :
      record case Byte of
        0 : (Len : Byte; RR : Real);
        1 : (Str : String6);
        2 : (XXX, Exp, MSB : Byte);
        3 : (YYY : Byte; MantExp : Word);
      end absolute S;
  begin
    {pad to proper length just in case}
    ZeroPad(S, 6);

    if Temp.Exp and $80 = 0 then begin {!!.04}
      ToggleBits(Temp.RR, 6);          {!!.04}
      Temp.Exp := Temp.Exp and $7F;    {!!.04}
    end;                               {!!.04}

    {put the sign bit back where it belongs}
    Temp.MantExp := SecondRotation(Temp.MantExp);

    {put the mantissa back into LSB->MSB order}
    ReverseBytes(Temp.MSB, 5);

    KeyToReal := Temp.RR;
  end;

type
  nkBCD = array[1..10] of Byte;

  function BcdToKey(var B) : String10;
    {-Convert a BCD real to a string}
  const
    Temp :
      record case Byte of
        0 : (Len : Byte; BT : nkBCD);
        1 : (Str : String10);
        2 : (XXX, Exp, LSB : Byte);
      end = (Str : '          ');
  var
    BP : nkBCD absolute B;
  begin
    {regularize 0 values}
    if BP[1] and $7F = 0 then
      FillChar(BP, SizeOf(BP), 0);

    Temp.BT := BP;

    {put the mantissa into MSB->LSB order}
    ReverseBytes(Temp.LSB, 9);

    {flip the sign bit}
    Temp.Exp := Temp.Exp xor $80;

    if Temp.Exp and $80 = 0 then begin
      ToggleBits(Temp.BT, 10);
      Temp.Exp := Temp.Exp and $7F;
    end;

    BcdToKey := Temp.Str;
  end;

  procedure KeyToBcd(S : String10; var B);
    {-Convert a string to a BCD real}
  var
    Temp :
      record case Byte of
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

{$IFOPT N+}

  function ExtToKey(E : Extended) : String10;
    {-Convert an extended to a string}
  const
    Temp :
      record case Byte of
        0 : (Len : Byte; EE : Extended);
        1 : (XXX, Exp : Byte);
        2 : (Str : String10);
      end = (Str : '          ');
  begin
    Temp.EE := E;

    {move the exponent to the front and put mantissa in MSB->LSB order}
    ReverseBytes(Temp.EE, 10);

    {flip the sign bit}
    Temp.Exp := Temp.Exp xor $80;

    if Temp.Exp and $80 = 0 then begin  {!!.04}
      ToggleBits(Temp.EE, 10);          {!!.04}
      Temp.Exp := Temp.Exp and $7F;     {!!.04}
    end;                                {!!.04}

    ExtToKey := Temp.Str;
  end;

  function KeyToExt(S : String10) : Extended;
    {-Convert a string to an extended}
  var
    Temp :
      record case Byte of
        0 : (Len : Byte; EE : Extended);
        1 : (XXX, Exp : Byte);
        2 : (Str : String10);
      end absolute S;
  begin
    {pad to proper length just in case}
    ZeroPad(S, 10);

    {flip the sign bit}
    Temp.Exp := Temp.Exp xor $80;

    if Temp.Exp and $80 <> 0 then begin {!!.04}
      ToggleBits(Temp.EE, 10);          {!!.04}
      Temp.Exp := Temp.Exp or $80;      {!!.04}
    end;                                {!!.04}

    {move the exponent back to the end and put mantissa in LSB->MSB order}
    ReverseBytes(Temp.EE, 10);

    KeyToExt := Temp.EE;
  end;

{$ENDIF}

function CStyleNumKey(S : String) : String;
  {-Convert Pascal-style numkey S into a C-style numkey, compatible with ASCIIZ}
const
  MaxInpStrLen = 223;
type
  PWord = ^Word;
var
  WPtr : PWord;
  I, ResLen, ShiftCount, OrigLen : Word;
  ResStr : String;
begin
  OrigLen := Length(S);
  if (OrigLen = 0) or (OrigLen > MaxInpStrLen) then begin
    CStyleNumKey := '';
    Exit;
  end;

  ResLen := OrigLen+Succ(Pred(OrigLen) DIV 7);
  ResStr[0] := Chr(ResLen);

  S[0] := #0;
  if OrigLen = 4 then
    ShiftCount := 4
  else
    ShiftCount := 1;
  WPtr := @S;

  for I := 1 To ResLen do begin
    ResStr[I] := Chr(((Swap(WPtr^) shr ShiftCount) and $FF) or $80);
    if ShiftCount < 7 then begin
      Inc(ShiftCount);
      Inc(PWord(@WPtr)^);
    end else
      ShiftCount := 0;
  end;

  if OrigLen = 4 then
    ResStr[1] := Chr((Byte(ResStr [1]) shl 4) or $01);

  CStyleNumKey := ResStr;
end;

function PascalStyleNumKey(S : String) : String;
  {-Convert C-style numkey S into a Pascal-style numkey}
Type
  PByte = ^Byte;
  PWord = ^Word;
Var
  B1Ptr, B2Ptr : PByte;
  I, ShiftLCount, ShiftRCount, ResLen, OrigLen : Word;
begin
  OrigLen := Length(S);
  if OrigLen = 0 then begin
    PascalStyleNumKey := '';
    Exit;
  end;

  ResLen := OrigLen-Succ(Pred(OrigLen) DIV 8);
  S[0] := Chr(ResLen);

  B1Ptr := @S[1];
  B2Ptr := @S[2];
  if OrigLen = 5 then begin
    B1Ptr^ := B1Ptr^ shr 4;
    ShiftLCount := 4;
    ShiftRCount := 3;
  end else begin
    ShiftLCount := 1;
    ShiftRCount := 6;
  end;

  for I := 1 To ResLen do begin
    B1Ptr^ := (B1Ptr^ shl ShiftLCount) or ((B2Ptr^ and $7F) shr ShiftRCount);
    if ShiftLCount < 7 then begin
      Inc(ShiftLCount);
      Dec(ShiftRCount);
      Inc(PWord(@B1Ptr)^);
      Inc(PWord(@B2Ptr)^);
    end else begin
      ShiftLCount := 1;
      ShiftRCount := 6;
      Inc(PWord(@B1Ptr)^, 2);
      Inc(PWord(@B2Ptr)^, 2);
    end;
  end;

  PascalStyleNumKey := S;
end;

function CStyleDescendingKey(S : string; MaxLen : byte) : string;
  {-Convert S to a descending key, using C-style algorithm}
var
  I     : Word;
  ToAdd : Integer;
begin
  ToAdd := MaxLen-Length(S);
  if ToAdd < 0 then begin
    CStyleDescendingKey := '';
    exit;
  end;
  for I := 1 to Length(S) do
    S[I] := Char(-Byte(S[I]));
  FillChar(S[succ(Length(S))], ToAdd, $FF);
  S[0] := Chr(MaxLen);
  CStyleDescendingKey := S;
end;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
