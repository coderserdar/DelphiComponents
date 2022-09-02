{*********************************************************}
{* FlashFiler: Date/time support routines                *}
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

Unit fslldate;

Interface

Uses
  fsllbase,
  fsstdate,
  Windows,
  SysUtils;

Const
  {the following characters are meaningful in date Picture masks}
  pmMonth = 'M'; {formatting character for a date string picture mask. }
  pmDay = 'D'; {formatting character for a date string picture mask. }
  pmYear = 'Y'; {formatting character for a date string picture mask}
  pmDateSlash = '/'; {formatting character for a date string picture mask}

  pmHour = 'h'; {formatting character for a time string picture mask}
  pmMinute = 'm'; {formatting character for a time string picture mask}
  pmSecond = 's'; {formatting character for a time string picture mask}
  {'hh:mm:ss tt' -\> '12:00:00 pm', 'hh:mmt' -\> '12:00p'}
  pmAmPm = 't'; {formatting character for a time string picture mask.
  This generates 'AM' or 'PM'}
  pmTimeColon = ':'; {formatting character for a time string picture mask}

  MaxDateLen = 40; { maximum length of date picture mask }

Function DateStringToDMY(Const Picture, S: String; Var Day, Month, Year: Integer;
  Epoch: Integer): Boolean;
{-extract day, month, and year from S, returning true if string is valid}

Function DatePCharToDMY(Picture, S: PAnsiChar; Var Day, Month, Year: Integer;
  Epoch: Integer): Boolean;
{-extract day, month, and year from S, returning true if string is valid}

Function TimeStringToHMS(Const Picture, S: String;
  Var Hour, Minute, Second: Integer): Boolean;
{-extract Hours, Minutes, Seconds from St, returning true if string is valid}

Function TimePCharToHMS(Picture, S: PAnsiChar;
  Var Hour, Minute, Second: Integer): Boolean;
{-extract Hours, Minutes, Seconds from St, returning true if string is valid}

Implementation

Var
  w1159: Array[0..5] Of AnsiChar;
  w2359: Array[0..5] Of AnsiChar;

  {===== Internal Routines =====}

Function StrChPos(P: PAnsiChar; C: AnsiChar;
  Var Pos: Cardinal): Boolean; Register;
{-Sets Pos to position of character C within string P returns True if found}
Asm
  push   esi               {save since we'll be changing}
  push   edi
  push   ebx
  mov    esi, ecx          {save Pos}

  cld                      {forward string ops}
  mov    edi, eax          {copy P to EDI}
  or     ecx, -1
  xor    eax, eax          {zero}
  mov    ebx, edi          {save EDI to EBX}
  repne  scasb             {search for NULL terminator}
  not    ecx
  dec    ecx               {ecx has len of string}

  test   ecx, ecx
  jz     @@NotFound        {if len of P = 0 then done}

  mov    edi, ebx          {reset EDI to beginning of string}
  mov    al, dl            {copy C to AL}
  repne  scasb             {find C in string}
  jne    @@NotFound

  mov    ecx, edi          {calculate position of C}
  sub    ecx, ebx
  dec    ecx               {ecx holds found position}

  mov    [esi], ecx        {store location}
  mov    eax, 1            {return true}
  jmp    @@ExitCode

@@NotFound:
  xor    eax, eax

@@ExitCode:

  pop    ebx               {restore registers}
  pop    edi
  pop    esi
End;

Function UpCaseChar(C: AnsiChar): AnsiChar; Register;
Asm
  and   eax, 0FFh
  push  eax
  call  CharUpper
End;

Procedure ExtractFromPicture(Picture, S: PAnsiChar;
  Ch: AnsiChar; Var I: Integer;
  Blank, Default: Integer);
{-extract the value of the subfield specified by Ch from S and return in
  I. I will be set to -1 in case of an error, Blank if the subfield exists
  in Picture but is empty, Default if the subfield doesn't exist in
  Picture.}
Var
  PTmp: Array[0..20] Of AnsiChar;
  J, K: Cardinal;
  Code: Integer;
  Found,
    UpFound: Boolean;
Begin
  {find the start of the subfield}
  I := Default;
  Found := StrChPos(Picture, Ch, J);
  Ch := UpCaseChar(Ch);
  UpFound := StrChPos(Picture, Ch, K);

  If Not Found Or (UpFound And (K < J)) Then
    Begin
      J := K;
      Found := UpFound;
    End;
  If Not Found Or (StrLen(S) <> StrLen(Picture)) Then
    Exit;

  {extract the substring}
  PTmp[0] := #0;
  K := 0;
  While (UpCaseChar(Picture[J]) = Ch) And (J < StrLen(Picture)) Do
    Begin
      If S[J] <> ' ' Then
        Begin
          PTmp[k] := S[J];
          Inc(K);
          PTmp[k] := #0;
        End;
      Inc(J);
    End;

  If StrLen(PTmp) = 0 Then
    I := Blank
  Else
    Begin
      {convert to a value}
      Val(PTmp, I, Code);
      If Code <> 0 Then
        I := -1;
    End;
End;

{===== Exported routines =====}

Function DateStringToDMY(Const Picture, S: String; Var Day, Month, Year: Integer;
  Epoch: Integer): Boolean;
{-extract day, month, and year from S, returning true if string is valid}
Var
  Buf1: Array[0..255] Of AnsiChar;
  Buf2: Array[0..255] Of AnsiChar;
Begin
  StrPCopy(Buf1, Picture);
  StrPCopy(Buf2, S);
  Result := DatePCharToDMY(Buf1, Buf2, Day, Month, Year, Epoch);
End;

Function DatePCharToDMY(Picture, S: PAnsiChar; Var Day, Month, Year: Integer;
  Epoch: Integer): Boolean;
{-extract day, month, and year from S, returning true if string is valid}
Begin
  Result := False;
  If StrLen(Picture) <> StrLen(S) Then
    Exit;

  ExtractFromPicture(Picture, S, pmMonth, Month, -1, DefaultMonth);
  ExtractFromPicture(Picture, S, pmDay, Day, -1, 1);
  ExtractFromPicture(Picture, S, pmYear, Year, -1, DefaultYear);
  Result := ValidDate(Day, Month, Year, Epoch);
End;

Function TimeStringToHMS(Const Picture, S: String;
  Var Hour, Minute, Second: Integer): Boolean;
{-extract Hours, Minutes, Seconds from St, returning true if string is valid}
Var
  Buf1: Array[0..255] Of AnsiChar;
  Buf2: Array[0..255] Of AnsiChar;
Begin
  StrPCopy(Buf1, Picture);
  StrPCopy(Buf2, S);
  Result := TimePCharToHMS(Buf1, Buf2, Hour, Minute, Second);
End;

Function TimePCharToHMS(Picture, S: PAnsiChar;
  Var Hour, Minute, Second: Integer): Boolean;
{-extract Hours, Minutes, Seconds from St, returning true if string is valid}
Var
  I, J: Cardinal;
  Tmp,
    t1159,
    t2359: Array[0..20] Of AnsiChar;
Begin
  Result := False;
  If StrLen(Picture) <> StrLen(S) Then
    Exit;

  {extract hours, minutes, seconds from St}
  ExtractFromPicture(Picture, S, pmHour, Hour, -1, 0);
  ExtractFromPicture(Picture, S, pmMinute, Minute, -1, 0);
  ExtractFromPicture(Picture, S, pmSecond, Second, -1, 0);
  If (Hour = -1) Or (Minute = -1) Or (Second = -1) Then
    Begin
      Result := False;
      Exit;
    End;

  {check for TimeOnly}
  If StrChPos(Picture, pmAmPm, I) And (w1159[0] <> #0)
    And (w2359[0] <> #0) Then
    Begin
      Tmp[0] := #0;
      J := 0;
      While Picture[I] = pmAmPm Do
        Begin
          Tmp[J] := S[I];
          Inc(J);
          Inc(I);
        End;
      Tmp[J] := #0;
      FFStrTrimR(Tmp);

      StrCopy(t1159, w1159);
      t1159[J] := #0;
      StrCopy(t2359, w2359);
      t2359[J] := #0;

      If (Tmp[0] = #0) Then
        Hour := -1
      Else If StrIComp(Tmp, t2359) = 0 Then
        Begin
          If (Hour < 12) Then
            Inc(Hour, 12)
          Else If (Hour = 0) Or (Hour > 12) Then
            {force BadTime}
            Hour := -1;
        End
      Else If StrIComp(Tmp, t1159) = 0 Then
        Begin
          If Hour = 12 Then
            Hour := 0
          Else If (Hour = 0) Or (Hour > 12) Then
            {force BadTime}
            Hour := -1;
        End
      Else
        {force BadTime}
        Hour := -1;
    End;

  Result := ValidTime(Hour, Minute, Second);
End;

Initialization
  GetProfileString('intl', 's1159', 'AM', w1159, SizeOf(w1159));
  GetProfileString('intl', 's2359', 'PM', w2359, SizeOf(w2359));
End.

