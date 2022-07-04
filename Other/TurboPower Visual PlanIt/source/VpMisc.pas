{*********************************************************}
{*                   VPMISC.PAS 1.03                     *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit VpMisc;
  {-Miscellaneous functions and procedures}

interface

uses
  Windows, Buttons, Classes, Controls, ExtCtrls, Forms, Graphics, Messages,
  SysUtils, Consts, VpBase, VpData, VpConst;

type
  TDayList = array[1..12] of Word;


  TVpDayType = (dtSunday, dtMonday, dtTuesday, dtWednesday, dtThursday,
                 dtFriday, dtSaturday);

  TVpDateFormat   = (dfShort, dfLong);

  TVpDayNameWidth = Integer;

const
  MonthDays: array [Boolean] of TDayList =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

function DaysInMonth(Year, Month : Integer) : Integer;
  {-return the number of days in the specified month of a given year}
function DefaultEpoch : Integer;
  {-return the current century}
function GetLeftButton : Byte;
procedure GetRGB(Clr : TColor; var IR, IG, IB : Byte);
function IsLeapYear(Year : Integer) : Boolean;
function GetStartOfWeek(Date: TDateTime; StartOn: TVpDayType): TDateTime;

procedure StripString(var Str: string);
  { strips non-alphanumeric characters from the beginning and end of the string}
function AssembleName(Contact: TVpContact): string;
  { returns an assembled name string }
procedure ParseName(Contact: TVpContact; const Value: string);
  { parses the name into it's elements and updates the contact }
procedure ParseCSZ(Str: string; var City, State, Zip: string);
  { parses the string and returns the city, state and zip parameters }
function LoadBaseBitmap(lpBitmapName : PAnsiChar) : HBITMAP;
  {-load and return the handle to bitmap resource}
function LoadBaseCursor(lpCursorName : PAnsiChar) : HCURSOR;
  {-load and return the handle to cursor resource}
function MinI(X, Y : Integer) : Integer;
  {-return the minimum of two integers}
function MaxI(X, Y : Integer) : Integer;
  {-return the maximum of two integers}
function HeightOf(const R : TRect) : Integer;
  {- return the height of the TRect}
function WidthOf(const R : TRect) : Integer;
  {- return the width of the TRect}
function GetDisplayString(Canvas : TCanvas; const S : string;
  MinChars, MaxWidth : Integer) : string;
  {-given a string, a minimum number of chars to display, and a max width, }
  { find the string that can be displayed in that width - add ellipsis to  }
  { the end if necessary and possible                                      }
procedure DrawBevelRect(const Canvas: TCanvas; R: TRect;
  Shadow, Highlight: TColor);
  {-draws a bevel in the specified TRect, using the specified colors }
function PointInRect(Point: TPoint; Rect: TRect): Boolean;
  {-determines if the specified point resides inside the specified TRect }

function GetAlarmAdvanceTime(Advance: Integer;
  AdvanceType: TVpAlarmAdvType): TDateTime;

{$IFNDEF Delphi6}

function MonthOfTheYear (TheDate : TDateTime) : Word;

procedure IncAMonth (var Year, Month, Day : Word; NumMonths : Integer);

function IncMonth(const TheDate : TDateTime;
  NumberOfMonths : Integer) : TDateTime;

function IncYear (TheDate : TDateTime; NumYears : Integer) : TDateTime;
{$ENDIF}

function GetJulianDate(Date: TDateTime): Word;

function HourToLine (const Value       : TVpHours;
                     const Granularity : TVpGranularity) : Integer;

function GetStartLine (StartTime: TDateTime;
  Granularity: TVpGranularity): Integer;

function GetEndLine (EndTime: TDateTime;
  Granularity: TVpGranularity): Integer;

function TimeInRange(Time, StartTime, EndTime: TDateTime;
  Inclusive: Boolean): Boolean;

function LineToStartTime(Line: Integer; Granularity: TVpGranularity): TDateTime;

function GetLineDuration(Granularity: TVpGranularity): Double;

implementation

uses
  VpException, VpSR;

procedure StripString(var Str: string);
begin
  if Length (Str) < 1 then
    Exit;
  while not (Str[1] in ['A'..'Z', 'a'..'z', '0'..'9']) do
    delete(Str, 1, 1);
  while not (Str[Length(Str)] in ['A'..'Z', 'a'..'z', '0'..'9']) do
    delete(Str, Length(Str), 1);
end;
{=====}

function AssembleName(Contact: TVpContact): string;
begin
  result := Contact.LastName;
  if Assigned (Contact.Owner) then begin
    if Contact.Owner.ContactSort = csFirstLast then begin
      if Contact.FirstName <> '' then
        result := Contact.FirstName + ' ' + Result;
    end else begin
      if Contact.FirstName <> '' then
        result := result + ', ' + Contact.FirstName;
    end;
  end else begin
    if Contact.FirstName <> '' then
      result := result + ', ' + Contact.FirstName;
  end;
end;
{=====}

procedure ParseName(Contact: TVpContact; const Value: string);
var
  name, ln, fn: string;
begin
  name := Value;

  { strip spaces from the beginning and end of the name string }
  StripString(name);

  { parse string }
  if pos(',', name) > 0 then begin
    { lastname, firstname }
    ln := copy(name, 1, pos(',', name) -1);
    fn := copy(name, pos(',', name), length(name));
  end else begin
    { firstname lastname }
    ln := copy(name, LastDelimiter(' ', name), length(name));
    fn := copy(name, 1, LastDelimiter(' ', name) - 1);
  end;

  { strip fn and ln strings }
  StripString(fn);
  StripString(ln);

  { assign the strings to the proper contact fields }
  Contact.LastName := ln;
  Contact.FirstName := fn;
end;
{=====}

procedure ParseCSZ(Str: string; var City, State, Zip: string);
var
  num: integer;
begin
  StripString(Str);

  if Pos(',', Str) > 0 then begin
    City := copy (Str, 1, pos(',', str) - 1);
    delete(str, 1, pos(',', str));
  end;

  num := LastDelimiter(' ', Str);

  if (num > 0)
  and (num < Length(Str))
  and (Str[num + 1] in ['0'..'9']) then begin
    Zip := copy(Str, num, length(Str));
    Delete(Str, num, length(str));
  end;

  State := Str;

  StripString(City);
  StripString(State);
  StripString(Zip);
end;
{=====}

function LoadBaseBitmap(lpBitmapName : PAnsiChar) : HBITMAP;
begin
  Result := LoadBitmap(FindClassHInstance(TVpCustomControl), lpBitmapName);
end;
{=====}

function LoadBaseCursor(lpCursorName : PAnsiChar) : HCURSOR;
begin
  Result := LoadCursor(FindClassHInstance(TVpCustomControl), lpCursorName);
end;
{=====}

function MinI(X, Y : Integer) : Integer;
asm
  cmp  eax, edx
  jle  @@Exit
  mov  eax, edx
@@Exit:
end;
{=====}

function MaxI(X, Y : Integer) : Integer;
asm
  cmp  eax, edx
  jge  @@Exit
  mov  eax, edx
@@Exit:
end;
{=====}

function WidthOf(const R : TRect) : Integer;
begin
  Result := R.Right - R.Left;
end;
{=====}

function HeightOf(const R : TRect) : Integer;
begin
  Result := R.Bottom - R.Top;
end;
{=====}

function GetDisplayString(Canvas : TCanvas; const S : string;
  MinChars, MaxWidth : Integer) : string;
var
  iDots, EllipsisWidth, Extent, Len, Width : Integer;
  ShowEllipsis : Boolean;
begin
  {be sure that the Canvas Font is set before entering this routine}
  EllipsisWidth := Canvas.TextWidth('...');
  Len := Length(S);
  Result := S;
  Extent := Canvas.TextWidth(Result);
  ShowEllipsis := False;
  Width := MaxWidth;
  while (Extent > Width) do begin
    ShowEllipsis := True;
    Width := MaxWidth - EllipsisWidth;
    if Len > MinChars then begin
      Delete(Result, Len, 1);
      dec(Len);
    end else
      break;
    Extent := Canvas.TextWidth(Result);
  end;
  if ShowEllipsis then begin
    Result := Result + '...';
    inc(Len, 3);
    Extent := Canvas.TextWidth(Result);
    iDots := 3;
    while (iDots > 0) and (Extent > MaxWidth) do begin
      Delete(Result, Len, 1);
      Dec(Len);
      Extent := Canvas.TextWidth(Result);
      Dec(iDots);
    end;
  end;
end;
{=====}

procedure DrawBevelRect(const Canvas: TCanvas; R: TRect;
  Shadow, Highlight: TColor);
begin
  with Canvas do
  begin
    Pen.Color := Shadow;
    PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
      Point(R.Right, R.Top)]);
    Pen.Color := Highlight;
    PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
      Point(R.Left, R.Bottom)]);
  end;
end;
{=====}

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  result := (Point.X >= Rect.Left) and (Point.X <= Rect.Right)
        and (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom);
end;
{=====}

function DaysInMonth(Year, Month : Integer) : Integer;
begin
  if (Year < 100) then
    raise EVpDateException.Create(RSInvalidYear + ' "' + IntToStr(Year) + '"');
  case Month of
    1, 3, 5, 7, 8, 10, 12 : Result := 31;
    4, 6, 9, 11           : Result := 30;
    2                     : Result := 28+Ord(IsLeapYear(Year));
  else
    Result := 0;
  end;
end;
{=====}

function DefaultEpoch : Integer;
var
  ThisYear   : Word;
  ThisMonth  : Word;
  ThisDay    : Word;
begin
  DecodeDate(SysUtils.Date, ThisYear, ThisMonth, ThisDay);
  Result := (ThisYear div 100) * 100;
end;
{=====}

function GetLeftButton : Byte;
const
  RLButton : array[Boolean] of Word = (VK_LBUTTON, VK_RBUTTON);
begin
  Result := RLButton[GetSystemMetrics(SM_SWAPBUTTON) <> 0];
end;
{=====}

procedure GetRGB(Clr : TColor; var IR, IG, IB : Byte);
begin
  IR := GetRValue(Clr);
  IG := GetGValue(Clr);
  IB := GetBValue(Clr);
end;
{=====}

function IsLeapYear(Year : Integer) : Boolean;
begin
  Result := (Year mod 4 = 0) and (Year mod 4000 <> 0) and
    ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;
{=====}

function GetStartOfWeek(Date: TDateTime; StartOn: TVpDayType): TDateTime;
begin
  result := Date;
  case StartOn of
    dtSunday:    result := Date - (DayOfWeek(Date) - 1);
    dtMonday:    result := Date - (DayOfWeek(Date) - 2);
    dtTuesday:   result := Date - (DayOfWeek(Date) - 3);
    dtWednesday: result := Date - (DayOfWeek(Date) - 4);
    dtThursday:  result := Date - (DayOfWeek(Date) - 5);
    dtFriday:    result := Date - (DayOfWeek(Date) - 6);
    dtSaturday:  result := Date - (DayOfWeek(Date) - 7);
  end;
end;
{=====}



{$IFNDEF Delphi6}
{=====}

function MonthOfTheYear (TheDate : TDateTime) : Word;
var
  Year, Day: Word;
begin
  DecodeDate (TheDate, Year, Result, Day);
end;
{=====}

procedure IncAMonth (var Year, Month, Day : Word; NumMonths : Integer);
type
  PMonthDayTable = ^TMonthDayTable;
  TMonthDayTable = array[1..12] of Word;

const
  MonthDays: array [Boolean] of TMonthDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
var
  DayTable: PDayTable;
  Sign: Integer;

begin
  if NumMonths >= 0 then
    Sign := 1
  else
    Sign := -1;
  Year := Year + (NumMonths div 12);
  NumMonths := NumMonths mod 12;
  Inc (Month, NumMonths);
  if Word (Month-1) > 11 then
  begin
    Inc (Year, Sign);
    Inc (Month, -12 * Sign);
  end;
  DayTable := @MonthDays[IsLeapYear (Year)];
  if Day > DayTable^[Month] then
    Day := DayTable^[Month];
end;
{=====}

function IncMonth(const TheDate : TDateTime; NumberOfMonths : Integer) : TDateTime;
var
  Year, Month, Day : Word;
begin
  DecodeDate (TheDate, Year, Month, Day);
  IncAMonth (Year, Month, Day, NumberOfMonths);
  Result := EncodeDate (Year, Month, Day);
end;
{=====}

function IncYear (TheDate : TDateTime; NumYears : Integer) : TDateTime;
begin
  Result := IncMonth (TheDate, NumYears * 12);
end;
{=====}
{$ENDIF}

function GetJulianDate(Date: TDateTime): Word;
var
  y, m, d, I: word;
  Julian: Word;
begin
  Julian := 0;
  DecodeDate(Date, y, m, d);

  { Inc Julian by the number of days in each of the elapsed months }
  for I := 1 to M do
    Inc(Julian, DaysInMonth(Y, I));

  { add in the elapsed days from this month }
  Julian := Julian + D;

  { return the value }
  result := Julian;
end;
{=====}

function HourToLine (const Value       : TVpHours;
                     const Granularity : TVpGranularity) : Integer;
begin
  case Granularity of
    gr60Min : Result := Ord (Value);
    gr30Min : Result := Ord (Value) * 2;
    gr20Min : Result := Ord (Value) * 3;
    gr15Min : Result := Ord (Value) * 4;
    gr10Min : Result := Ord (Value) * 6;
    gr06Min : Result := Ord (Value) * 10;
    gr05Min : Result := Ord (Value) * 12;
    else
      Result := Ord (Value) * 2; { Default to 30 minutes }
  end;
end;
{=====}

function GetStartLine (StartTime: TDateTime;
  Granularity: TVpGranularity): Integer;
var
  LineDuration : Double; { the percentage of a day covered by each line }
  Time         : Double;
begin
  { remove the date part, and add one minute to the time }
  Time := StartTime - trunc(StartTime) + (1 / MinutesInDay);

  case Granularity of
    gr60Min : LineDuration := 60 / MinutesInDay;
    gr30Min : LineDuration := 30 / MinutesInDay;
    gr20Min : LineDuration := 20 / MinutesInDay;
    gr15Min : LineDuration := 15 / MinutesInDay;
    gr10Min : LineDuration := 10 / MinutesInDay;
    gr06Min : LineDuration :=  6 / MinutesInDay;
    gr05Min : LineDuration :=  5 / MinutesInDay;
  else
    LineDuration := 30 / MinutesInDay;
  end;

  result := trunc(Time / LineDuration);
end;
{=====}

function GetEndLine (EndTime: TDateTime;
  Granularity: TVpGranularity): Integer;
var
  LineDuration : Double; { the percentage of a day covered by each line }
  Time         : Double;
begin
  { remove the date part, and subtract one minute from the time }
  Time := EndTime - trunc(EndTime) - (1 / MinutesInDay);

  case Granularity of
    gr60Min : LineDuration := 60 / MinutesInDay;
    gr30Min : LineDuration := 30 / MinutesInDay;
    gr20Min : LineDuration := 20 / MinutesInDay;
    gr15Min : LineDuration := 15 / MinutesInDay;
    gr10Min : LineDuration := 10 / MinutesInDay;
    gr06Min : LineDuration :=  6 / MinutesInDay;
    gr05Min : LineDuration :=  5 / MinutesInDay;
  else
    LineDuration := 30 / MinutesInDay;
  end;

  result := trunc(Time / LineDuration);
end;
{=====}

function GetAlarmAdvanceTime(Advance: Integer;
  AdvanceType: TVpAlarmAdvType): TDateTime;
begin
  result := 0.0;
  case AdvanceType of
    atMinutes : result := Advance / MinutesInDay;
    atHours   : result := (Advance * 60) / MinutesInDay;
    atDays    : result := Advance;
  end;
end;
{=====}

function TimeInRange(Time, StartTime, EndTime: TDateTime;
  Inclusive: Boolean): Boolean;
begin
  if Inclusive then
    result := (Time >= StartTime) and (Time <= EndTime)
  else
    result := (Time > StartTime) and (Time < EndTime);
end;
{=====}

function LineToStartTime(Line: Integer; Granularity: TVpGranularity): TDateTime;
begin
  case Granularity of
    gr60Min : result := (Line * 24) / MinutesInDay;
    gr30Min : result := (Line * 30) / MinutesInDay;
    gr20Min : result := (Line * 20) / MinutesInDay;
    gr15Min : result := (Line * 15) / MinutesInDay;
    gr10Min : result := (Line * 10) / MinutesInDay;
    gr06Min : result := (Line *  6) / MinutesInDay;
    gr05Min : result := (Line *  5) / MinutesInDay;
  else
    result := (Line * 30) / MinutesInDay;
  end;
  {chop off the date portion}
  result := result - trunc(Result);
end;
{=====}

function GetLineDuration(Granularity: TVpGranularity): Double;
begin
  case Granularity of
    gr60Min : result := 24 / MinutesInDay;
    gr30Min : result := 30 / MinutesInDay;
    gr20Min : result := 20 / MinutesInDay;
    gr15Min : result := 15 / MinutesInDay;
    gr10Min : result := 10 / MinutesInDay;
    gr06Min : result :=  6 / MinutesInDay;
    gr05Min : result :=  5 / MinutesInDay;
  else
    result := 30 / MinutesInDay;
  end;
  { chop off the date portion }
  result := result - trunc(result);
end;
{=====}

end.
