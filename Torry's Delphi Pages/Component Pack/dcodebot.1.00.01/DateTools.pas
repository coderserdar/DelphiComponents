
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit DateTools;

interface

{$I STD.INC}

uses
  Windows, SysUtils;

function DateDiff(const First, Last: TDateTime): Integer;
function CurrentYear(const Value: TDateTime): Integer;
function CurrentMonth(const Value: TDateTime): Integer;
function PrevMonth(const Value: TDateTime): TDateTime;
function NextMonth(const Value: TDateTime): TDateTime;
function MiddleOfMonth(const Value: TDateTime): TDateTime;
function FirstOfMonth(const Value: TDateTime): TDateTime;
function LastOfMonth(const Value: TDateTime): TDateTime;
function DateToMonths(const Value: TDateTime): Integer;
function MonthsToDate(const Value: Integer): TDateTime;
function DateToShortStr(const Value: TDateTime): string;
function DateToLongStr(const Value: TDateTime): string;
function StartOfDay(const Value: TDateTime): TDateTime;
function EndOfDay(const Value: TDateTime): TDateTime;

implementation

const
  OneDay = 1;
  OneHour = OneDay / 24;
  OneMinute = OneHour / 60;
  OneSecond = OneMinute / 60;

function DateDiff(const First, Last: TDateTime): Integer;
begin
  Result := Trunc(Last - First);
  if Result < 0 then
    Result := 0;
end;

function CurrentYear(const Value: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := Year;
end;

function CurrentMonth(const Value: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := Month;
end;

function PrevMonth(const Value: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Day := 1;
  Dec(Month);
  if Month < 1 then
  begin
    Month := 12;
    Dec(Year);
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function NextMonth(const Value: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Day := 1;
  Inc(Month);
  if Month > 12 then
  begin
    Month := 1;
    Inc(Year);
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function MiddleOfMonth(const Value: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Day := 15;
  Result := EncodeDate(Year, Month, Day);
end;

function FirstOfMonth(const Value: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Day := 1;
  Result := EncodeDate(Year, Month, Day);
end;

function LastOfMonth(const Value: TDateTime): TDateTime;
begin
  Result := NextMonth(Value) - OneSecond;
end;

function DateToMonths(const Value: TDateTime): Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := Year * 12 + Month;
end;

function MonthsToDate(const Value: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  Year := (Value-1) div 12;
  Month := Value mod 12;
  if Month = 0 then
    Month := 12;
  Day := 1;
  Result := EncodeDate(Year, Month, Day);
end;

function DateToShortStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime('mm/dd/yyyy', Value);
end;

function DateToLongStr(const Value: TDateTime): string;
begin
  Result := UpperCase(FormatDateTime('mm/dd/yyyy hh:mm am/pm', Value));
end;

function StartOfDay(const Value: TDateTime): TDateTime;
begin
  Result := Trunc(Value);
end;

function EndOfDay(const Value: TDateTime): TDateTime;
begin
  Result := Trunc(Value + OneDay) - OneSecond;
end;

end.
