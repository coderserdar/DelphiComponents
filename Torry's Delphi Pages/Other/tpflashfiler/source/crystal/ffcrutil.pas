{*********************************************************}
{* Low-Level functions for general use.                  *)
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

{$I ffcrdefn.inc}

unit ffcrutil;

interface

uses
  ffllbase,
  ffcrltyp,
  ffcrtype;

function PadStr(const S : TffShStr; const Width : Word): TffShStr;
procedure TrimStrR(P : PChar);
function CrDateToDateTime(BDate : TcrDate) : TDateTime;
procedure CrDateToYearMonthDay(BDate : TcrDate;
                         var Year  : TcrInt16s;
                         var Month : TcrInt16u;
                         var Day   : TcrInt16u);
function YearMonthDayToCrDate(const Year, Month, Day: SmallInt): TcrDate;
function BoolToStr(const Bool: TcrBoolean): TffShStr;
function MyStrPas(S: PChar): TffShStr;
function DumpNBytes(Data: Pointer; N: Integer): TffShStr;

implementation

uses
  FFStDate,
  SysUtils;

function BoolToStr(const Bool: TcrBoolean): TffShStr;
begin
  if Bool then
    Result := 'True'
  else
    Result := 'False';
end;
{--------}
function MyStrPas(S: PChar): TffShStr;
begin
  if not Assigned(S) then Result := 'nil'
  else Result := '"' + StrPas(S) + '"';
end;
{--------}
function DumpNBytes(Data: Pointer; N: Integer): TffShStr;
var
  I: Integer;
  DataBytes: PByteArray absolute Data;
begin
  Result := '';
  if Assigned(Data) then
    for I := 0 to N - 1 do
      Result := Result + IntToHex(Ord(DataBytes^[I]),2) + ' '
  else
    Result := 'nil';
end;
{--------}
function PadStr(const S : TffShStr; const Width : Word): TffShStr;
var
  I : Integer;
begin
  if Length(S) >= Width then
    Result := Copy(S, 1, Width)
  else begin
    Result := S;
    for I := Succ(Length(Result)) to Width do
      Result := Result + ' ';
  end;
end;
{--------}
procedure TrimStrR(P : PChar);
  {-Trim trailing blanks from P}
var
  I : Integer;
begin
  I := StrLen(P);
  if I = 0 then
    Exit;

  {delete trailing spaces}
  Dec(I);
  while (I >= 0) and (P[I] = ' ') do begin
    P[I] := #0;
    Dec(I);
  end;
end;
{--------}
{ Conversion from gregorian to julian date representation.
  If specificed date is invalid, dateToDate returns (-1),
  otherwise return Julian date representation.

  Julian date = 0 for date 4713/01/01 B.C. }

function CrDateToDateTime(BDate: TcrDate): TDateTime;
var
  Day, Month, Year: Integer;
begin
  StDateToDMY(AstJulianDateToStDate(BDate, False), Day, Month, Year);
  Result := EncodeDate(Year, Month, Day);
end;
{--------}
procedure CrDateToYearMonthDay(BDate : TcrDate;
                           var Year  : TcrInt16s;
                           var Month : TcrInt16u;
                           var Day   : TcrInt16u);
begin
  { see date2ymd.cpp }
end;
{--------}
function YearMonthDayToCrDate(const Year, Month, Day: SmallInt): TcrDate;
begin
  { Use SysTools routines to convert date to Julian date.  DMYToStDate
    performs date validation as well. }
  Result := Trunc(AstJulianDate(DMYToStDate(Day, Month, Year, 1950)));
  if Result = BadDate then Result := -1;
end;
{====================================================================}
end.

