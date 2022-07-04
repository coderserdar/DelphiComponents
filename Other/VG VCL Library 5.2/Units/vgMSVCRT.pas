{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Some MSVCRT declarations and utility routines }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgMSVCRT;

interface
uses Windows;

{ --- MSVCRT }
type
  Ptm = ^Ttm;
  Ttm = record
    tm_sec : integer;   // Seconds
    tm_min : integer;   // Minutes
    tm_hour : integer;  // Hour (0--23)
    tm_mday : integer;  // Day of month (1--31)
    tm_mon : integer;   // Month (0--11)
    tm_year : integer;  // Year (calendar year minus 1900)
    tm_wday : integer;  // Weekday (0--6) Sunday = 0)
    tm_yday : integer;  // Day of year (0--365)
    tm_isdst : integer; // 0 if daylight savings time is not in effect)
  end;

{ --- MSVCRT strings }
function _malloc(Size: Integer): Pointer; cdecl; external 'MSVCRT.DLL' name 'malloc';
{ Allocates memory block of size Size }

function _realloc(P: Pointer; Size: Integer): Pointer; cdecl; external 'MSVCRT.DLL' name 'realloc';
{ Reallocates memory block allocated with _malloc to size Size and returns new pointer }

procedure _free(P: pointer); cdecl; external 'MSVCRT.DLL' name 'free';
{ Frees memory allocated with _malloc }

{ --- MSVCRT time }
function _localtime(Time: PInteger): Ptm; cdecl; external 'MSVCRT.DLL' name 'localtime';
{ Converts Time longint into Ttm structure }

function _mktime(tm: Ptm): Integer; cdecl; external 'MSVCRT.DLL' name 'mktime';

function _IntToDateTime(Time: Integer): TDateTime;
{ Converts Time longint into TDateTime }

function _DateTimeToInt(Time: TDateTime): Integer;
{ Converts Time into longint }

implementation
uses SysUtils;

function _IntToDateTime(Time: Integer): TDateTime;
var
  tm: Ptm;
begin
  tm := _localtime(@Time);
  with tm^ do
    Result :=
      EncodeDate(tm_year + 1900, tm_mon + 1, tm_mday) +
      EncodeTime(tm_hour, tm_min, tm_sec, 0);
end;

function _DateTimeToInt(Time: TDateTime): Integer;
var
  tm: Ttm;
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(Time, Year, Month, Day);
  DecodeTime(Time, Hour, Min, Sec, MSec);
  ZeroMemory(@tm, SizeOf(tm));
  with tm do
  begin
    tm_sec := Sec;
    tm_min := Min;
    tm_hour := Hour;
    tm_mday := Day;
    tm_mon := Month - 1;
    tm_year := Year - 1900;
  end;
  Result := _mktime(@tm);
end;

end.
