{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Albert Drent
Description:  ASGRout parser routines
Creation:     Januari 1998
Version:      1.2.B
EMail:        a.drent@aducom.com (www.aducom.com)
Support:      support@aducom.com (www.aducom.com)
Legal issues: Copyright (C) 2003 by Aducom Software

              Aducom Software
              Eckhartstr 61
              9746 BN  Groningen
              Netherlands

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. If you make changes which improves the component you must
                 mail these to aducom as the moderator of the components
                 complete with documentation for the benefits of the community.

              4. You are not allowed to create commercial available components
                 using this software. If you use this source in any way to create
                 your own components, your source should be free of charge,
                 available to anyone. It's a far better idea to distribute your
                 changes through Aducom Software.

              5. This notice may not be removed or altered from any source
                 distribution.

              6. You must register this software by entering the support forum.
                 I like to keep track about where the components are used, so
                 sending a picture postcard to the author would be appreciated.
                 Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Modifications
              26/5/2004 Function YYYYMMDDParser by JPierce, necessary for
              locale independent datehandling in SQLite components.
              1/9/2005 Changes to the StrToFloatX routine, now depending on
              decimalseparator.

*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

unit ASGRout3;

interface

uses SysUtils;

const
  vtcIdentifier = 1;
  vtcNumber = 2;
  vtcAssignment = 3;
  vtcQString = 4;
  vtcDString = 5;
  vtcRelOp = 6;
  vtcFloat = 7;
  vtcDelimiter = 8;
  vtcEof = 9;

procedure FindErrorPos(InString: string; ErrPos: integer;
  var TheLine, TheCol: integer);
function GetWord(var InString: string; var StartPos: integer;
  var VarType: integer): string;
function GetWordByDelim(var InString: string; var StartPos: integer;
  var Delim: string): string;
function PeekWord(var InString: string; StartPos: integer;
  var VarType: integer): string;
function Recover(var InString: string; var StartPos: integer): boolean;
function StrToIntX(StrIn: string): integer;
function StrToFloatX(StrIn : string) : extended;
function StrToDateX(TheDate: string): TDateTime;
function StrToDateTimeX(const S: string): TDateTime;
function StrToTimeX(TheTime : string): TDateTime;
function YYYYMMDDParser(Str: PChar): TDateTime;
function FloatParser(Str: string): string;// jordi march
function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;

implementation

function WStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) do begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
  end;
  Dest^ := #$00;
end;

function FloatParser(Str: string): string;// jordi march
var
  Point: Byte;
begin
  if  DecimalSeparator <> '.'  then  begin
    Point := Pos ('.', Str);
    if  Point <> 0
    then  Str[Point] := DecimalSeparator;
  end;
  Result := Str;
end;

 //==============================================================================
 // Convert dates to a correct datetime notation. Try several notations,
 // starting with the system defaults                           
 //==============================================================================

function StrToDateTimeX(const S: string): TDateTime;
begin
  if S = '' then
     StrToDateTimeX := 0
  else begin
     try
        StrToDateTimeX := StrToDateTime(S);
     except
        StrToDateTimeX := StrToDateX(s);
     end;
  end;
end;

function StrToDateX(TheDate: string): TDateTime;
var
  DateFormat: string;
  DateSep:    char;

  function StrToDateDefX(TheDate : string) : TDateTime;
  begin
   {$IFDEF ASQLITE_D6PLUS}
    StrToDateX      := StrToDateDef(TheDate, -1.5); //!!!Jure: This will not yield an exception, but will still detect if the conversion could not be done
   {$ELSE}
    try
       StrToDateX      := StrToDate(TheDate);
    except
       StrToDateX      := -1.5;
    end;
   {$ENDIF}
  end;

begin
  DateFormat := ShortDateFormat; // save current settings
  DateSep    := DateSeparator;
  try
    StrToDateX      := StrToDateDefX(TheDate);
    if result = -1.5 then begin
      DateSeparator   := '-';
      ShortDateFormat := 'dd-mm-yyyy';
      StrToDateX      := StrToDateDefX(TheDate);
      if result = -1.5 then begin
        ShortDateFormat := 'yyyy-mm-dd';
        try
          StrToDateX := StrToDate(TheDate)
        except
          StrToDateX := StrToDateX('01-01-1900');
          raise;
        end;
      end;
    end;
  finally
    ShortDateFormat := DateFormat;
    DateSeparator   := DateSep;
  end;
end;

function StrToTimeX(TheTime : string): TDateTime;
var p : integer;
begin
     p := pos(' ', TheTime);
     if p > 0 then Delete(TheTime, 1, p);
     p := pos('.', TheTime);
     if p > 0 then Delete(TheTime, p,99);
     try
        StrToTimeX := StrToTime(TheTime);
     except
        StrToTimeX := 0;
     end;
end;

// Routine submitted by jpierce, modified to accept more types
// It requires that the date be in strict yyyy-mm-dd [hh:nn:[ss[:mmm]]]

function YYYYMMDDParser(Str: PChar): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Result := 0;

  try
    if Length(Str) >= 10 then // 10 = Length of YYYY-MM-DD
    begin
      Year := StrToInt(Copy(Str, 1, 4));
      Month := StrToInt(Copy(Str, 6, 2));
      Day := StrToInt(Copy(Str, 9, 2));

      Result := EncodeDate(Year, Month, Day);
    end;

    if Length(Str) > 10 then // it has a time
    begin
      Hour := StrToInt(Copy(Str, 12, 2));
      Min := StrToInt(Copy(Str, 15, 2));
      Sec := 0;
      MSec := 0;
      if Length(Str) > 16 then Sec := StrToInt(Copy(Str, 18, 2));
      if Length(Str) > 19 then Msec := StrToInt(Copy(Str, 21, 3));
      Result := Result + EncodeTime(Hour, Min, Sec, MSec);
    end;
  except
    Result := 0;
  end;
end;

function StrToIntX(StrIn: string): integer;
var
E: Integer;
begin
 Val(StrIn, Result, E);
 if E <> 0 then Result := 0;
end;

function StrToFloatX(StrIn : string) : extended;
begin
  if not TextToFloat(PChar(StrIn), Result, fvExtended) then
  Result := 0;
end;

procedure FindErrorPos(InString: string; ErrPos: integer;
  var TheLine, TheCol: integer);
var
  i: integer;
begin
  TheLine := 1;
  TheCol := 1;
  i := 1;
  while i < ErrPos do
  begin
    if InString[i] in [ #10, #13] then
    begin
      Inc(TheLine);
      TheCol := 1;
      Inc(i);
      Inc(i);
    end
    else
    begin
      Inc(TheCol);
      Inc(i);
    end;
  end;
end;

function Recover(var InString: string;
  var StartPos: integer): boolean;
begin
  if (StartPos > Length(InString)) then
  begin
    Recover := false;
    exit;
  end;

  while (Startpos < Length(InString)) and
    ( not (InString[StartPos] in [ #10, #13])) do
    Inc(StartPos);
  Recover := true;
end;

function PeekWord(var InString: string; StartPos: integer;
  var VarType: integer): string;
begin
  PeekWord := GetWord(InString, StartPos, VarType);
end;

function GetWordByDelim(var InString: string;
  var StartPos: integer;
  var Delim: string): string;
var
  Ret: string;
begin
  Ret := '';
  while (StartPos <= Length(InString)) and (InString[StartPos] = ' ') do
    Inc(StartPos);
  while (StartPos <= Length(InString)) and (Pos(InString[StartPos], Delim) = 0) do
  begin
    Ret := Ret + InString[StartPos];
    Inc(StartPos);
  end;
  GetWordByDelim := Trim(Ret);
end;

function GetWord(var InString: string; var StartPos: integer;
  var VarType: integer): string;
var
  TheChar: char;
  Rv:      string;
begin
  if (StartPos > Length(InString)) then
  begin
    GetWord := '';
    VarType := vtcEof;
    exit;
  end;

  while (StartPos <= Length(InString)) and (InString[StartPos] <= #32) do
    Inc(StartPos);

  TheChar := InString[StartPos];
  Rv      := '';

  if TheChar in ['a'..'z', 'A'..'Z'] then
    VarType := vtcIdentifier
  else if TheChar in ['0'..'9', '-'] then
    VarType := vtcNumber
  else if TheChar = ':' then
    VarType := vtcAssignment
  else if TheChar = '"' then
    VarType := vtcDString
  else if TheChar = '''' then
    VarType := vtcQString
  else if TheChar in ['>', '=', '<'] then
    VarType := vtcRelOp
  else
  begin
    Inc(StartPos);
    if TheChar = '!' then
    begin
      Recover(InString, StartPos);
      Rv      := GetWord(InString, StartPos, VarType);
      GetWord := Rv;
    end
    else
    begin
      GetWord := TheChar;
    end;
    exit;
  end;

  case VarType of
    vtcIdentifier:
    begin
      while InString[StartPos] in ['a'..'z', 'A'..'Z', '_','0'..'9'] do
      begin
        Rv := Rv + InString[StartPos];
        Inc(StartPos);
      end;
    end;
    vtcNumber:
    begin
      while InString[StartPos] in ['-', '0'..'9', '.'] do
      begin
        if InString[StartPos] = '.' then
          VarType := vtcFloat;
        Rv := Rv + InString[StartPos];
        Inc(StartPos);
      end;
      if VarType = vtcFloat then
        Rv := FloatToStr(StrToFloat(Rv))
      else
        Rv := IntToStr(StrToInt(Rv));
    end;
    vtcAssignment:
    begin
      Rv := InString[StartPos];
      Inc(StartPos);
      if InString[StartPos] = '=' then
      begin
        Inc(StartPos);
        Rv := ':=';
      end
      else
      begin
        VarType := vtcDelimiter;
        Rv      := ':';
      end;
    end;
    vtcQString:
    begin
      Inc(StartPos);
      while InString[StartPos] <> '''' do
      begin
        Rv := Rv + InString[StartPos];
        Inc(StartPos);
      end;
      Inc(StartPos);
    end;
    vtcDString:
    begin
      Inc(StartPos);
      while InString[StartPos] <> '"' do
      begin
        Rv := Rv + InString[StartPos];
        Inc(StartPos);
      end;
      Inc(StartPos);
    end;
    vtcRelOp:
    begin
      Rv := InString[StartPos];
      if Rv = '<' then
      begin
        if InString[StartPos + 1] in ['=', '>'] then
        begin
          Rv := Rv + InString[StartPos + 1];
          StartPos := StartPos + 2;
        end
        else
        begin
          Inc(StartPos);
        end;
      end
      else if Rv = '>' then
      begin
        if InString[StartPos + 1] in ['=', '<'] then
        begin
          Rv := Rv + InString[StartPos + 1];
          StartPos := StartPos + 2;
        end
        else
        begin
          Inc(StartPos);
        end;
      end
      else
      begin
        Inc(StartPos);
      end;
    end;
  end;
  GetWord := Rv;
end;

{$IFDEF SQLite_Static} 
Var 
  TZInfo  :_TIME_ZONE_INFORMATION; 
  TZRes   :Integer; 

initialization 
  PInteger(@__timezone)^:=0; 
  PInteger(@__daylight)^:=0; 
  TZRes:=GetTimezoneInformation(TZInfo); 
  if TZRes>=0 Then 
    PInteger(@__timezone)^:=TZInfo.Bias*60; 
  if TZRes=TIME_ZONE_ID_DAYLIGHT Then 
    PInteger(@__daylight)^:=1; 
{$ENDIF} 

end.
