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

{$I ffdefine.inc}

unit fflldate;

interface

uses
  ffllbase,
  ffstdate,
  Windows,
  SysUtils;

const
  {the following characters are meaningful in date Picture masks}
  pmMonth  = 'M';  {formatting character for a date string picture mask. }
  pmDay    = 'D';  {formatting character for a date string picture mask. }
  pmYear   = 'Y';  {formatting character for a date string picture mask}
  pmDateSlash  = '/';  {formatting character for a date string picture mask}

  pmHour     = 'h';   {formatting character for a time string picture mask}
  pmMinute   = 'm';   {formatting character for a time string picture mask}
  pmSecond   = 's';   {formatting character for a time string picture mask}
  {'hh:mm:ss tt' -\> '12:00:00 pm', 'hh:mmt' -\> '12:00p'}
  pmAmPm      = 't';  {formatting character for a time string picture mask.
                      This generates 'AM' or 'PM'}
  pmTimeColon = ':';  {formatting character for a time string picture mask}

  MaxDateLen = 40;  { maximum length of date picture mask }


function DateStringToDMY(const Picture, S : string; var Day, Month, Year : Integer;
  Epoch : Integer) : Boolean;
  {-extract day, month, and year from S, returning true if string is valid}

function DatePCharToDMY(Picture, S : PAnsiChar; var Day, Month, Year : Integer;
  Epoch : Integer) : Boolean;
  {-extract day, month, and year from S, returning true if string is valid}

function TimeStringToHMS(const Picture, S : string;
                           var Hour, Minute, Second : Integer) : Boolean;
  {-extract Hours, Minutes, Seconds from St, returning true if string is valid}

function TimePCharToHMS(Picture, S : PAnsiChar;
                    var Hour, Minute, Second : Integer) : Boolean;
  {-extract Hours, Minutes, Seconds from St, returning true if string is valid}

implementation

var
  w1159            : array[0..5] of AnsiChar;
  w2359            : array[0..5] of AnsiChar;


{===== Internal Routines =====}

function StrChPos(P : PAnsiChar; C : AnsiChar;
                  var Pos : Cardinal): Boolean; register;
  {-Sets Pos to position of character C within string P returns True if found}
asm
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
end;


function UpCaseChar(C : AnsiChar) : AnsiChar; register;
asm
  and   eax, 0FFh
  push  eax
  call  CharUpper
end;

procedure ExtractFromPicture(Picture, S : PAnsiChar;
                             Ch : AnsiChar; var I : Integer;
                             Blank, Default : Integer);
  {-extract the value of the subfield specified by Ch from S and return in
    I. I will be set to -1 in case of an error, Blank if the subfield exists
    in Picture but is empty, Default if the subfield doesn't exist in
    Picture.}
var
  PTmp    : Array[0..20] of AnsiChar;
  J, K    : Cardinal;
  Code    : Integer;
  Found,
  UpFound : Boolean;
begin
  {find the start of the subfield}
  I := Default;
  Found := StrChPos(Picture, Ch, J);
  Ch := UpCaseChar(Ch);
  UpFound := StrChPos(Picture, Ch, K);

  if not Found or (UpFound and (K < J)) then begin
    J := K;
    Found := UpFound;
  end;
  if not Found or (StrLen(S) <> StrLen(Picture)) then
    Exit;

  {extract the substring}
  PTmp[0] := #0;
  K := 0;
  while (UpCaseChar(Picture[J]) = Ch) and (J < StrLen(Picture)) do begin
    if S[J] <> ' ' then begin
      PTmp[k] := S[J];
      Inc(K);
      PTmp[k] := #0;
    end;
    Inc(J);
  end;

  if StrLen(PTmp) = 0 then
    I := Blank
  else begin
    {convert to a value}
    Val(PTmp, I, Code);
    if Code <> 0 then
      I := -1;
  end;
end;

{===== Exported routines =====}


function DateStringToDMY(const Picture, S : string; var Day, Month, Year : Integer;
  Epoch : Integer) : Boolean;
  {-extract day, month, and year from S, returning true if string is valid}
var
  Buf1 : array[0..255] of AnsiChar;
  Buf2 : array[0..255] of AnsiChar;
begin
  StrPCopy(Buf1, Picture);
  StrPCopy(Buf2, S);
  Result := DatePCharToDMY(Buf1, Buf2, Day, Month, Year, Epoch);
end;

function DatePCharToDMY(Picture, S : PAnsiChar; var Day, Month, Year : Integer;
  Epoch : Integer) : Boolean;
  {-extract day, month, and year from S, returning true if string is valid}
begin
  Result := False;
  if StrLen(Picture) <> StrLen(S) then
    Exit;

  ExtractFromPicture(Picture, S, pmMonth, Month, -1, DefaultMonth);
  ExtractFromPicture(Picture, S, pmDay, Day, -1, 1);
  ExtractFromPicture(Picture, S, pmYear, Year, -1, DefaultYear);
  Result := ValidDate(Day, Month, Year, Epoch);
end;

function TimeStringToHMS(const Picture, S : string;
                           var Hour, Minute, Second : Integer) : Boolean;
  {-extract Hours, Minutes, Seconds from St, returning true if string is valid}
var
  Buf1 : array[0..255] of AnsiChar;
  Buf2 : array[0..255] of AnsiChar;
begin
  StrPCopy(Buf1, Picture);
  StrPCopy(Buf2, S);
  Result := TimePCharToHMS(Buf1, Buf2, Hour, Minute, Second);
end;

function TimePCharToHMS(Picture, S : PAnsiChar;
                    var Hour, Minute, Second : Integer) : Boolean;
  {-extract Hours, Minutes, Seconds from St, returning true if string is valid}
var
  I, J  : Cardinal;
  Tmp,
  t1159,
  t2359 : array[0..20] of AnsiChar;
begin
  Result := False;
  if StrLen(Picture) <> StrLen(S) then
    Exit;

  {extract hours, minutes, seconds from St}
  ExtractFromPicture(Picture, S, pmHour, Hour, -1, 0);
  ExtractFromPicture(Picture, S, pmMinute,  Minute, -1, 0);
  ExtractFromPicture(Picture, S, pmSecond,  Second, -1, 0);
  if (Hour = -1) or (Minute = -1) or (Second = -1) then begin
    Result := False;
    Exit;
  end;

  {check for TimeOnly}
  if StrChPos(Picture, pmAmPm, I) and (w1159[0] <> #0)
    and (w2359[0] <> #0) then begin
    Tmp[0] := #0;
    J := 0;
    while Picture[I] = pmAmPm do begin
      Tmp[J] := S[I];
      Inc(J);
      Inc(I);
    end;
    Tmp[J] := #0;
    FFStrTrimR(Tmp);

    StrCopy(t1159, w1159);
    t1159[J] := #0;
    StrCopy(t2359, w2359);
    t2359[J] := #0;

    if (Tmp[0] = #0) then
      Hour := -1
    else if StrIComp(Tmp, t2359) = 0 then begin
      if (Hour < 12) then
        Inc(Hour, 12)
      else if (Hour = 0) or (Hour > 12) then
        {force BadTime}
        Hour := -1;
    end else if StrIComp(Tmp, t1159) = 0 then begin
      if Hour = 12 then
        Hour := 0
      else if (Hour = 0) or (Hour > 12) then
        {force BadTime}
        Hour := -1;
    end else
      {force BadTime}
      Hour := -1;
  end;

  Result := ValidTime(Hour, Minute, Second);
end;

initialization
  GetProfileString('intl', 's1159', 'AM', w1159, SizeOf(w1159));
  GetProfileString('intl', 's2359', 'PM', w2359, SizeOf(w2359));
end.



