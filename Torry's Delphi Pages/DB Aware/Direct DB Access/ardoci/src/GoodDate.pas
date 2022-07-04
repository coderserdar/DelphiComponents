unit GoodDate;

interface

// Unix-like datetime(seconds from 1 Jan 1970) *
function GetLocalUDateTime:int64;
function GetLocalUDate:integer;
function GetUDateFromUDateTime(UDateTime:int64):integer;
function GetUTimeFromUDateTime(UDateTime:int64):integer;
procedure UnMakeUDate(UDate:integer;var Year, Month, Day: Word);
procedure UnMakeUTime(UTime:integer;var Hour, Min, Sec: Word);
function MakeUDate(Year, Month, Day: Word):integer;
function MakeUTime(Hour, Min, Sec: Word):integer;
function UDateToSDate(UDate:integer):string;
function UTimeToSTime(UTime:integer):string;
function UDateToDateTime(UDate:integer):TDateTime;
function UTimeToDateTime(UTime:integer):TDateTime;
function DateTimeToUDate(DateTime:TDateTime):integer;
function DateTimeToUTime(DateTime:TDateTime):integer;

function MakeGoodDate(Year, Month, Day: Word):integer;
function MakeGoodTime(Hour, Min, Sec, MSec: Word):integer;
function MakeGoodDateTime(Year, Month, Day, Hour, Min, Sec, MSec: Word):int64;
procedure UnMakeGoodDate(ADate:integer;var Year, Month, Day: Word);
procedure UnMakeGoodTime(ATime:integer;var Hour, Min, Sec, MSec: Word);
procedure UnMakeGoodDateTime(ADateTime:int64;var Year, Month, Day: Word;var Hour, Min, Sec, MSec: Word);

function DateTimeToGoodDate(DDate:TDateTime):integer;
function DateTimeToGoodTime(DDate:TDateTime):integer;
function DateTimeToGoodDateTime(DDate:TDateTime):int64;

function GoodDateToDateTime(ADate:integer):TDateTime;
function GoodTimeToDateTime(ADate:integer):TDateTime;
function GoodDateTimeToDateTime(ADate:int64):TDateTime;


implementation

uses SysUtils, Windows;

function MakeGoodDate(Year, Month, Day: Word):integer;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := 0;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
   begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Result := I * 365 + I div 4 - I div 100 + I div 400 + Day;
   end;
end;

function MakeGoodTime(Hour, Min, Sec, MSec: Word):integer;
begin
  Result := 0;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
    Result := Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec;
end;

function MakeGoodDateTime(Year, Month, Day, Hour, Min, Sec, MSec: Word):int64;
begin
 Result:=MakeGoodDate(Year, Month, Day);
 Result:=Result*MSecsPerDay;
 Result:=Result+MakeGoodTime(Hour, Min, Sec, MSec);
end;

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

procedure UnMakeGoodDate(ADate:integer;var Year, Month, Day: Word);
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T:integer;
  DayTable: PDayTable;
begin
  T:=ADate;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
  end else
  begin
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    DayTable := @MonthDays[IsLeapYear(Y)];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

procedure UnMakeGoodTime(ATime:integer;var Hour, Min, Sec, MSec: Word);
var
  MinCount, MSecCount: Word;
begin
  DivMod(ATime, 60000, MinCount, MSecCount);
  DivMod(MinCount, 60, Hour, Min);
  DivMod(MSecCount, 1000, Sec, MSec);
end;

procedure UnMakeGoodDateTime(ADateTime:int64;var Year, Month, Day: Word;var Hour, Min, Sec, MSec: Word);
begin
 UnMakeGoodDate(ADateTime div MSecsPerDay,Year,Month,Day);
 UnMakeGoodTime(ADateTime mod MSecsPerDay,Hour,Min,Sec,MSec);
end;

function DateTimeToGoodDate(DDate:TDateTime):integer;
begin
 Result:=trunc(DDate)+DateDelta;
end;

function DateTimeToGoodTime(DDate:TDateTime):integer;
begin
 Result:=trunc(MSecsPerDay*frac(DDate));
end;

function DateTimeToGoodDateTime(DDate:TDateTime):int64;
begin
 Result:=(trunc(DDate)+DateDelta);
 Result:=Result*MSecsPerDay+trunc(frac(DDate)*MSecsPerDay);
end;

function GoodDateToDateTime(ADate:integer):TDateTime;
begin
 Result:=ADate-DateDelta;
end;

function GoodTimeToDateTime(ADate:integer):TDateTime;
begin
 Result:=(ADate*1.0)/MSecsPerDay;
end;

function GoodDateTimeToDateTime(ADate:int64):TDateTime;
begin
 Result:=((ADate div MSecsPerDay)-DateDelta)+((ADate mod MSecsPerDay)*1.0)/MSecsPerDay;
end;

function GetLocalUDateTime:int64;
var
  SystemTime:TSystemTime;
  DayTable:PDayTable;
begin
 Result:=0;
 GetLocalTime(SystemTime);

 with SystemTime do begin
  DayTable := @MonthDays[IsLeapYear(wYear)];
  if (wHour >= 24) or (wMinute >= 60) or (wSecond >= 60) or (wMilliSeconds >= 1000) or
     (wYear < 1) or (wYear > 9999) or (wMonth < 1) or (wMonth > 12) or
     (wDay < 1) and (wDay > DayTable^[wMonth]) then exit;

  Result:=MakeUDate(wYear,wMonth,wDay)*24*60*60+MakeUTime(wHour,wMinute,wSecond);
 end;
end;

function GetLocalUDate:integer;
var
  SystemTime:TSystemTime;
  DayTable: PDayTable;
begin
 Result:=0;
 GetLocalTime(SystemTime);

 with SystemTime do begin
  DayTable := @MonthDays[IsLeapYear(wYear)];
  if (wYear < 1) or (wYear > 9999) or (wMonth < 1) or (wMonth > 12) or
     (wDay < 1) and (wDay > DayTable^[wMonth]) then exit;

  Result:=MakeUDate(wYear,wMonth,wDay);
 end;
end;

function GetUDateFromUDateTime(UDateTime:int64):integer;
begin
 Result:=UDateTime div (24*60*60);
end;

function GetUTimeFromUDateTime(UDateTime:int64):integer;
begin
 Result:=UDateTime mod (24*60*60);
end;

procedure UnMakeUDate(UDate:integer;var Year, Month, Day: Word);
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T:integer;
  DayTable: PDayTable;
begin
  T:=UDate;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
  end else
  begin
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    DayTable := @MonthDays[IsLeapYear(Y)];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y+1970;
    Month := M;
    Day := D + 1;
  end;
end;

procedure UnMakeUTime(UTime:integer;var Hour, Min, Sec: Word);
begin
 Hour:=UTime div (60*60);
 Min:=(UTime mod (60*60)) div 60;
 Sec:=UTime mod 60;
end;

function UDateToSDate(UDate:integer):string;
var Year, Month, Day: Word;
begin
 UnMakeUDate(UDate,Year,Month,Day);
 Format('%2d.%2d.%4d',[Day,Month,Year]);
end;

function UTimeToSTime(UTime:integer):string;
var Hour, Min, Sec: Word;
begin
 UnMakeUTime(UTime,Hour,Min,Sec);
 Format('%2d:%2d:%2d',[Hour,Min,Sec]);
end;

function MakeUDate(Year, Month, Day: Word):integer;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := 0;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
   begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1970;
    Result := I * 365 + I div 4 - I div 100 + I div 400 + Day;
   end;
end;

function MakeUTime(Hour, Min, Sec: Word):integer;
begin
 Result := 0;
 if (Hour < 24) and (Min < 60) and (Sec < 60) then
   Result := Hour * 60*60 + Min * 60 + Sec;
end;

function UDateToDateTime(UDate:integer):TDateTime;
var Y,M,D:word;
begin
 if UDate=0 then Result:=0
  else begin
   UnMakeUDate(UDate,Y,M,D);
   Result:=EncodeDate(Y,M,D);
  end;
end;

function UTimeToDateTime(UTime:integer):TDateTime;
var H,M,S:word;
begin
 if UTime=0 then Result:=0
  else begin
   UnMakeUTime(UTime,H,M,S);
   Result:=EncodeTime(H,M,S,0);
  end;
end;

function DateTimeToUDate(DateTime:TDateTime):integer;
var Y,M,D:word;
begin
 DecodeDate(DateTime,Y,M,D);
 Result:=MakeUDate(Y,M,D);
end;

function DateTimeToUTime(DateTime:TDateTime):integer;
var H,M,S,MS:word;
begin
 DecodeTime(DateTime,H,M,S,MS);
 Result:=MakeUTime(H,M,S);
end;

end.
