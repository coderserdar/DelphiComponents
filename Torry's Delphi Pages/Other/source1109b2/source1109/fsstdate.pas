{*********************************************************}
{* FSSQL     : Date and time calculations                *}
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

Unit fsstdate;
{-Date and time manipulation - from SysTools}

Interface

Uses
  Windows,
  SysUtils;

Type
  TStDate = Longint;
  {In STDATE, dates are stored in long integer format as the number of days
  since January 1, 1600}

  TDateArray = Array[0..(MaxLongInt Div SizeOf(TStDate)) - 1] Of TStDate;
  {Type for StDate open array}

  TStDayType = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
  {An enumerated type used when representing a day of the week}

  TStBondDateType = (bdtActual, bdt30E360, bdt30360, bdt30360psa);
  {An enumerated type used for calculating bond date differences}

  TStTime = Longint;
  {STDATE handles time in a manner similar to dates, representing a given
  time of day as the number of seconds since midnight}

  TStDateTimeRec =
    Record
    {This record type simply combines the two basic date types defined by
     STDATE, Date and Time}
    D: TStDate;
    T: TStTime;
  End;

Const
  MinYear = 1600; {Minimum valid year for a date variable}
  MaxYear = 3999; {Maximum valid year for a date variable}
  Mindate = $00000000; {Minimum valid date for a date variable - 01/01/1600}
  Maxdate = $000D6025; {Maximum valid date for a date variable - 12/31/3999}
  Date1900 = $0001AC05; {This constant contains the Julian date for 01/01/1900}
  Date1980 = $00021E28; {This constant contains the Julian date for 01/01/1980}
  Date2000 = $00023AB1; {This constant contains the Julian date for 01/01/2000}
  {This value is used to represent an invalid date, such as 12/32/1992}
  BadDate = Longint($FFFFFFFF);

  DeltaJD = $00232DA8; {Days between 1/1/-4173 and 1/1/1600}

  MinTime = 0; {Minimum valid time for a time variable - 00:00:00 am}
  MaxTime = 86399; {Maximum valid time for a time variable - 23:59:59 pm}
  {This value is used to represent an invalid time of day, such as 12:61:00}
  BadTime = Longint($FFFFFFFF);

  SecondsInDay = 86400; {Number of seconds in a day}
  SecondsInHour = 3600; {Number of seconds in an hour}
  SecondsInMinute = 60; {Number of seconds in a minute}
  HoursInDay = 24; {Number of hours in a day}
  MinutesInHour = 60; {Number of minutes in an hour}
  MinutesInDay = 1440; {Number of minutes in a day}

Var
  DefaultYear: Integer; {default year--used by DateStringToDMY}
  DefaultMonth: Shortint; {default month}

  {-------julian date routines---------------}

Function CurrentDate: TStDate;
{-returns today's date as a Julian date}

Function ValidDate(Day, Month, Year, Epoch: Integer): Boolean;
{-Verify that day, month, year is a valid date}

Function DMYtoStDate(Day, Month, Year, Epoch: Integer): TStDate;
{-Convert from day, month, year to a Julian date}

Procedure StDateToDMY(Julian: TStDate; Var Day, Month, Year: Integer);
{-Convert from a Julian date to day, month, year}

Function IncDate(Julian: TStDate; Days, Months, Years: Integer): TStDate;
{-Add (or subtract) the number of days, months, and years to a date}

Function IncDateTrunc(Julian: TStDate; Months, Years: Integer): TStDate;
{-Add (or subtract) the specified number of months and years to a date}

Procedure DateDiff(Date1, Date2: TStDate;
  Var Days, Months, Years: Integer);
{-Return the difference in days, months, and years between two valid Julian
  dates}

Function BondDateDiff(Date1, Date2: TStDate; DayBasis: TStBondDateType): TStDate;
{-Return the difference in days between two valid Julian
dates using a specific financial basis}

Function WeekOfYear(Julian: TStDate): Byte;
{-Returns the week number of the year given the Julian Date}

Function AstJulianDate(Julian: TStDate): Double;
{-Returns the Astronomical Julian Date from a TStDate}

Function AstJulianDatetoStDate(AstJulian: Double; Truncate: Boolean): TStDate;
{-Returns a TStDate from an Astronomical Julian Date.
Truncate TRUE   Converts to appropriate 0 hours then truncates
         FALSE  Converts to appropriate 0 hours, then rounds to
                nearest;}

Function AstJulianDatePrim(Year, Month, Date: Integer; UT: TStTime): Double;
{-Returns an Astronomical Julian Date for any year, even those outside
  MinYear..MaxYear}

Function DayOfWeek(Julian: TStDate): TStDayType;
{-Return the day of the week for a Julian date}

Function DayOfWeekDMY(Day, Month, Year, Epoch: Integer): TStDayType;
{-Return the day of the week for the day, month, year}

Function IsLeapYear(Year: Integer): Boolean;
{-Return True if Year is a leap year}

Function DaysInMonth(Month: Integer; Year, Epoch: Integer): Integer;
{-Return the number of days in the specified month of a given year}

Function ResolveEpoch(Year, Epoch: Integer): Integer;
{-Convert 2 digit year to 4 digit year according to Epoch}

{-------time routines---------------}

Function ValidTime(Hours, Minutes, Seconds: Integer): Boolean;
{-Return True if Hours:Minutes:Seconds is a valid time}

Procedure StTimeToHMS(T: TStTime;
  Var Hours, Minutes, Seconds: Byte);
{-Convert a time variable to hours, minutes, seconds}

Function HMStoStTime(Hours, Minutes, Seconds: Byte): TStTime;
{-Convert hours, minutes, seconds to a time variable}

Function CurrentTime: TStTime;
{-Return the current time in seconds since midnight}

Procedure TimeDiff(Time1, Time2: TStTime;
  Var Hours, Minutes, Seconds: Byte);
{-Return the difference in hours, minutes, and seconds between two times}

Function IncTime(T: TStTime; Hours, Minutes, Seconds: Byte): TStTime;
{-Add the specified hours, minutes, and seconds to a given time of day}

Function DecTime(T: TStTime; Hours, Minutes, Seconds: Byte): TStTime;
{-Subtract the specified hours, minutes, and seconds from a given time of day}

Function RoundToNearestHour(T: TStTime; Truncate: Boolean): TStTime;
{-Given a time, round it to the nearest hour, or truncate minutes and
seconds}

Function RoundToNearestMinute(Const T: TStTime; Truncate: Boolean): TStTime;
{-Given a time, round it to the nearest minute, or truncate seconds}

{-------- routines for DateTimeRec records ---------}

Procedure DateTimeDiff(DT1: TStDateTimeRec; Var DT2: TStDateTimeRec;
  Var Days: Longint; Var Secs: Longint);
{-Return the difference in days and seconds between two points in time}

Procedure IncDateTime(DT1: TStDateTimeRec; Var DT2: TStDateTimeRec;
  Days: Integer; Secs: Longint);
{-Increment (or decrement) a date and time by the specified number of days
and seconds}

Function DateTimeToStDate(DT: TDateTime): TStDate;
{-Convert Delphi TDateTime to TStDate}

Function DateTimeToStTime(DT: TDateTime): TStTime;
{-Convert Delphi TDateTime to TStTime}

Function StDateToDateTime(D: TStDate): TDateTime;
{-Convert TStDate to TDateTime}

Function StTimeToDateTime(T: TStTime): TDateTime;
{-Convert TStTime to TDateTime}

Function Convert2ByteDate(TwoByteDate: Word): TStDate;
{-Convert an Object Professional two byte date into a SysTools date}

Function Convert4ByteDate(FourByteDate: TStDate): Word;
{-Convert a SysTools date into an Object Professional two byte date}

Function FFStDateToString(Const ADate: TStDate): String;
Function FFStringToStDate(Const ADateStr: String): TStDate;
Function FFStTimeToString(Const ATime: TStTime): String;
Function FFStringToStTime(Const ATimeStr: String): TStTime;
Procedure fsSetMillisecond(Var D: TDateTime; Const Milliseconds: Word);

Implementation

Const
  First2Months = 59; {1600 was a leap year}
  FirstDayOfWeek = Saturday; {01/01/1600 was a Saturday}
  DateLen = 40; {maximum length of Picture strings}
  MaxMonthName = 15;
  MaxDayName = 15;
  StDateConv = 109571;

Type
  {  DateString = string[DateLen];}
  SString = String[255];

Procedure DecodeDateTime(Const DateTime: TDateTime; Var Year, Month, Day, Hour, Minute, Second, Millisecond: Word);
Begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, Millisecond);
End;

Function EncodeDateTime(Const Year, Month, Day, Hour, Minute, Second, Millisecond: Word): TDateTime;
Begin
  Result := EncodeDate(Year, Month, Day) +
    EncodeTime(Hour, Minute, Second, Millisecond);
End;

Procedure fsSetMillisecond(Var D: TDateTime; Const Milliseconds: Word);
Var
  Ye, Mo, Da, Ho, Mi, Se, Ms: Word;
Begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Se, Milliseconds);
End;

Function IsLeapYear(Year: Integer): Boolean;
{-Return True if Year is a leap year}
Begin
  Result := (Year Mod 4 = 0) And (Year Mod 4000 <> 0) And
    ((Year Mod 100 <> 0) Or (Year Mod 400 = 0));
End;

Function IsLastDayofMonth(Day, Month, Year: Integer): Boolean;
{-Return True if date is the last day in month}
Var
  Epoch: Integer;
Begin
  Epoch := (Year Div 100) * 100;
  If ValidDate(Day + 1, Month, Year, Epoch) Then
    Result := False
  Else
    Result := True;
End;

Function IsLastDayofFeb(Date: TStDate): Boolean;
{-Return True if date is the last day in February}
Var
  Day, Month, Year: Integer;
Begin
  StDateToDMY(Date, Day, Month, Year);
  If (Month = 2) And IsLastDayOfMonth(Day, Month, Year) Then
    Result := True
  Else
    Result := False;
End;

Procedure ExchangeLongInts(Var I, J: Longint);
  Register;
Asm
  mov  ecx, [eax]
  push ecx
  mov  ecx, [edx]
  mov  [eax], ecx
  pop  ecx
  mov  [edx], ecx
End;

Procedure ExchangeStructs(Var I, J; Size: Cardinal);
  Register;
Asm
  push edi
  push ebx
  push ecx
  shr  ecx, 2
  jz   @@LessThanFour

@@AgainDWords:
  mov  ebx, [eax]
  mov  edi, [edx]
  mov  [edx], ebx
  mov  [eax], edi
  add  eax, 4
  add  edx, 4
  dec  ecx
  jnz  @@AgainDWords

@@LessThanFour:
  pop  ecx
  and  ecx, $3
  jz   @@Done
  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh
  inc  eax
  inc  edx
  dec  ecx
  jz   @@Done

  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh
  inc  eax
  inc  edx
  dec  ecx
  jz   @@Done

  mov  bl, [eax]
  mov  bh, [edx]
  mov  [edx], bl
  mov  [eax], bh

@@Done:
  pop  ebx
  pop  edi
End;

Function ResolveEpoch(Year, Epoch: Integer): Integer;
{-Convert 2-digit year to 4-digit year according to Epoch}
Var
  EpochYear,
    EpochCent: Integer;
Begin
  If Word(Year) < 100 Then
    Begin
      EpochYear := Epoch Mod 100;
      EpochCent := (Epoch Div 100) * 100;
      If (Year < EpochYear) Then
        Inc(Year, EpochCent + 100)
      Else
        Inc(Year, EpochCent);
    End;
  Result := Year;
End;

Function CurrentDate: TStDate;
{-Returns today's date as a julian}
Var
  Year, Month, Date: Word;
Begin
  DecodeDate(Now, Year, Month, Date);
  Result := DMYToStDate(Date, Month, Year, 0);
End;

Function DaysInMonth(Month: Integer; Year, Epoch: Integer): Integer;
{-Return the number of days in the specified month of a given year}
Begin
  Year := ResolveEpoch(Year, Epoch);

  If (Year < MinYear) Or (Year > MaxYear) Then
    Begin
      Result := 0;
      Exit;
    End;

  Case Month Of
    1, 3, 5, 7, 8, 10, 12:
      Result := 31;
    4, 6, 9, 11:
      Result := 30;
    2:
      Result := 28 + Ord(IsLeapYear(Year));
    Else
      Result := 0;
  End;
End;

Function ValidDate(Day, Month, Year, Epoch: Integer): Boolean;
{-Verify that day, month, year is a valid date}
Begin
  Year := ResolveEpoch(Year, Epoch);

  If (Day < 1) Or (Year < MinYear) Or (Year > MaxYear) Then
    Result := False
  Else
    Case Month Of
      1..12:
        Result := Day <= DaysInMonth(Month, Year, Epoch);
      Else
        Result := False;
    End
End;

Function DMYtoStDate(Day, Month, Year, Epoch: Integer): TStDate;
{-Convert from day, month, year to a julian date}
Begin
  Year := ResolveEpoch(Year, Epoch);

  If Not ValidDate(Day, Month, Year, Epoch) Then
    Result := BadDate
  Else If (Year = MinYear) And (Month < 3) Then
    If Month = 1 Then
      Result := Pred(Day)
    Else
      Result := Day + 30
  Else
    Begin
      If Month > 2 Then
        Dec(Month, 3)
      Else
        Begin
          Inc(Month, 9);
          Dec(Year);
        End;
      Dec(Year, MinYear);
      Result :=
        ((Longint(Year Div 100) * 146097) Div 4) +
        ((Longint(Year Mod 100) * 1461) Div 4) +
        (((153 * Month) + 2) Div 5) + Day + First2Months;
    End;
End;

Function WeekOfYear(Julian: TStDate): Byte;
{-Returns the week number of the year given the Julian Date}
Var
  Day, Month, Year: Integer;
  FirstJulian: TStDate;
Begin
  If (Julian < MinDate) Or (Julian > MaxDate) Then
    Begin
      Result := 0;
      Exit;
    End;

  Julian := Julian + 3 - ((6 + Ord(DayOfWeek(Julian))) Mod 7);
  StDateToDMY(Julian, Day, Month, Year);
  FirstJulian := DMYToStDate(1, 1, Year, 0);
  Result := 1 + (Julian - FirstJulian) Div 7;
End;

Function AstJulianDate(Julian: TStDate): Double;
{-Returns the Astronomical Julian Date from a TStDate}
Begin
  {Subtract 0.5d since Astronomical JD starts at noon
   while TStDate (with implied .0) starts at midnight}
  Result := Julian - 0.5 + DeltaJD;
End;

Function AstJulianDatePrim(Year, Month, Date: Integer; UT: TStTime): Double;
Var
  A, B: Integer;
  LY,
    GC: Boolean;

Begin
  Result := -MaxLongInt;
  If (Not (Month In [1..12])) Or (Date < 1) Then
    Exit
  Else If (Month In [1, 3, 5, 7, 8, 10, 12]) And (Date > 31) Then
    Exit
  Else If (Month In [4, 6, 9, 11]) And (Date > 30) Then
    Exit
  Else If (Month = 2) Then
    Begin
      LY := IsLeapYear(Year);
      If ((LY) And (Date > 29)) Or (Not (LY) And (Date > 28)) Then
        Exit;
    End
  Else If ((UT < 0) Or (UT >= SecondsInDay)) Then
    Exit;

  If (Month <= 2) Then
    Begin
      Year := Year - 1;
      Month := Month + 12;
    End;
  A := abs(Year Div 100);

  If (Year > 1582) Then
    GC := True
  Else If (Year = 1582) Then
    Begin
      If (Month > 10) Then
        GC := True
      Else If (Month < 10) Then
        GC := False
      Else
        Begin
          If (Date >= 15) Then
            GC := True
          Else
            GC := False;
        End;
    End
  Else
    GC := False;
  If (GC) Then
    B := 2 - A + abs(A Div 4)
  Else
    B := 0;

  Result := Trunc(365.25 * (Year + 4716))
    + Trunc(30.6001 * (Month + 1))
    + Date + B - 1524.5
    + UT / SecondsInDay;
End;

Function AstJulianDatetoStDate(AstJulian: Double; Truncate: Boolean): TStDate;
{-Returns a TStDate from an Astronomical Julian Date.
  Truncate TRUE   Converts to appropriate 0 hours then truncates
           FALSE  Converts to appropriate 0 hours, then rounds to
                  nearest;}
Begin
  {Convert to TStDate, adding 0.5d for implied .0d of TStDate}
  AstJulian := AstJulian + 0.5 - DeltaJD;
  If (AstJulian < MinDate) Or (AstJulian > MaxDate) Then
    Begin
      Result := BadDate;
      Exit;
    End;

  If Truncate Then
    Result := Trunc(AstJulian)
  Else
    Result := Trunc(AstJulian + 0.5);
End;

Procedure StDateToDMY(Julian: TStDate; Var Day, Month, Year: Integer);
{-Convert from a julian date to month, day, year}
Var
  I, J: Longint;
Begin
  If Julian = BadDate Then
    Begin
      Day := 0;
      Month := 0;
      Year := 0;
    End
  Else If Julian <= First2Months Then
    Begin
      Year := MinYear;
      If Julian <= 30 Then
        Begin
          Month := 1;
          Day := Succ(Julian);
        End
      Else
        Begin
          Month := 2;
          Day := Julian - 30;
        End;
    End
  Else
    Begin
      I := (4 * Longint(Julian - First2Months)) - 1;

      J := (4 * ((I Mod 146097) Div 4)) + 3;
      Year := (100 * (I Div 146097)) + (J Div 1461);
      I := (5 * (((J Mod 1461) + 4) Div 4)) - 3;
      Day := ((I Mod 153) + 5) Div 5;

      Month := I Div 153;
      If Month < 10 Then
        Inc(Month, 3)
      Else
        Begin
          Dec(Month, 9);
          Inc(Year);
        End;
      Inc(Year, MinYear);
    End;
End;

Function IncDate(Julian: TStDate; Days, Months, Years: Integer): TStDate;
{-Add (or subtract) the number of months, days, and years to a date.
  Months and years are added before days. No overflow/underflow
  checks are made}
Var
  Day, Month, Year, Day28Delta: Integer;
Begin
  StDateToDMY(Julian, Day, Month, Year);
  Day28Delta := Day - 28;
  If Day28Delta < 0 Then
    Day28Delta := 0
  Else
    Day := 28;

  Inc(Year, Years);
  Inc(Year, Months Div 12);
  Inc(Month, Months Mod 12);
  If Month < 1 Then
    Begin
      Inc(Month, 12);
      Dec(Year);
    End
  Else If Month > 12 Then
    Begin
      Dec(Month, 12);
      Inc(Year);
    End;

  Julian := DMYtoStDate(Day, Month, Year, 0);
  If Julian <> BadDate Then
    Begin
      Inc(Julian, Days);
      Inc(Julian, Day28Delta);
    End;
  Result := Julian;
End;

Function IncDateTrunc(Julian: TStDate; Months, Years: Integer): TStDate;
{-Add (or subtract) the specified number of months and years to a date}
Var
  Day, Month, Year: Integer;
  MaxDay, Day28Delta: Integer;
Begin
  StDateToDMY(Julian, Day, Month, Year);
  Day28Delta := Day - 28;
  If Day28Delta < 0 Then
    Day28Delta := 0
  Else
    Day := 28;

  Inc(Year, Years);
  Inc(Year, Months Div 12);
  Inc(Month, Months Mod 12);
  If Month < 1 Then
    Begin
      Inc(Month, 12);
      Dec(Year);
    End
  Else If Month > 12 Then
    Begin
      Dec(Month, 12);
      Inc(Year);
    End;

  Julian := DMYtoStDate(Day, Month, Year, 0);
  If Julian <> BadDate Then
    Begin
      MaxDay := DaysInMonth(Month, Year, 0);
      If Day + Day28Delta > MaxDay Then
        Inc(Julian, MaxDay - Day)
      Else
        Inc(Julian, Day28Delta);
    End;
  Result := Julian;
End;

Procedure DateDiff(Date1, Date2: TStDate; Var Days, Months, Years: Integer);
{-Return the difference in days,months,years between two valid julian dates}
Var
  Day1, Day2, Month1, Month2, Year1, Year2: Integer;
Begin
  {we want Date2 > Date1}
  If Date1 > Date2 Then
    ExchangeLongInts(Date1, Date2);

  {convert dates to day,month,year}
  StDateToDMY(Date1, Day1, Month1, Year1);
  StDateToDMY(Date2, Day2, Month2, Year2);

  {days first}
  If Day2 < Day1 Then
    Begin
      Dec(Month2);
      If Month2 = 0 Then
        Begin
          Month2 := 12;
          Dec(Year2);
        End;
      Inc(Day2, DaysInMonth(Month2, Year2, 0));
    End;
  Days := Day2 - Day1;

  {now months and years}
  If Month2 < Month1 Then
    Begin
      Inc(Month2, 12);
      Dec(Year2);
    End;
  Months := Month2 - Month1;
  Years := Year2 - Year1;
End;

Function BondDateDiff(Date1, Date2: TStDate; DayBasis: TStBondDateType): TStDate;
{-Return the difference in days between two valid Julian
  dates using one a specific accrual method}
Var
  Day1,
    Month1,
    Year1,
    Day2,
    Month2,
    Year2: Integer;
  IY: Longint;
Begin
  {we want Date2 > Date1}
  If Date1 > Date2 Then
    ExchangeLongInts(Date1, Date2);

  If (DayBasis = bdtActual) Then
    Result := Date2 - Date1
  Else
    Begin
      StDateToDMY(Date1, Day1, Month1, Year1);
      StDateToDMY(Date2, Day2, Month2, Year2);

      If ((DayBasis = bdt30360PSA) And IsLastDayofFeb(Date1)) Or (Day1 = 31) Then
        Day1 := 30;
      If (DayBasis = bdt30E360) Then
        Begin
          If (Day2 = 31) Then
            Day2 := 30
        End
      Else If (Day2 = 31) And (Day1 >= 30) Then
        Day2 := 30;

      IY := 360 * (Year2 - Year1);
      Result := IY + 30 * (Month2 - Month1) + (Day2 - Day1);
    End;
End;

Function DayOfWeek(Julian: TStDate): TStDayType;
{-Return the day of the week for the date. Returns TStDayType(7) if Julian =
  BadDate.}
Var
  B: Byte;
Begin
  If Julian = BadDate Then
    Begin
      B := 7;
      Result := TStDayType(B);
    End
  Else
    Result := TStDayType((Julian + Ord(FirstDayOfWeek)) Mod 7);
End;

Function DayOfWeekDMY(Day, Month, Year, Epoch: Integer): TStDayType;
{-Return the day of the week for the day, month, year}
Begin
  Result := DayOfWeek(DMYtoStDate(Day, Month, Year, Epoch));
End;

Procedure StTimeToHMS(T: TStTime; Var Hours, Minutes, Seconds: Byte);
{-Convert a Time variable to Hours, Minutes, Seconds}
Begin
  If T = BadTime Then
    Begin
      Hours := 0;
      Minutes := 0;
      Seconds := 0;
    End
  Else
    Begin
      Hours := T Div SecondsInHour;
      Dec(T, Longint(Hours) * SecondsInHour);
      Minutes := T Div SecondsInMinute;
      Dec(T, Longint(Minutes) * SecondsInMinute);
      Seconds := T;
    End;
End;

Function HMStoStTime(Hours, Minutes, Seconds: Byte): TStTime;
{-Convert Hours, Minutes, Seconds to a Time variable}
Var
  T: TStTime;
Begin
  Hours := Hours Mod HoursInDay;
  T := (Longint(Hours) * SecondsInHour) + (Longint(Minutes) * SecondsInMinute) + Seconds;
  Result := T Mod SecondsInDay;
End;

Function ValidTime(Hours, Minutes, Seconds: Integer): Boolean;
{-Return true if Hours:Minutes:Seconds is a valid time}
Begin
  If (Hours < 0) Or (Hours > 23) Or
    (Minutes < 0) Or (Minutes >= 60) Or
    (Seconds < 0) Or (Seconds >= 60) Then
    Result := False
  Else
    Result := True;
End;

Function CurrentTime: TStTime;
{-Returns current time in seconds since midnight}
Begin
  Result := Trunc(SysUtils.Time * SecondsInDay);
End;

Procedure TimeDiff(Time1, Time2: TStTime; Var Hours, Minutes, Seconds: Byte);
{-Return the difference in hours,minutes,seconds between two times}
Begin
  StTimeToHMS(Abs(Time1 - Time2), Hours, Minutes, Seconds);
End;

Function IncTime(T: TStTime; Hours, Minutes, Seconds: Byte): TStTime;
{-Add the specified hours,minutes,seconds to T and return the result}
Begin
  Inc(T, HMStoStTime(Hours, Minutes, Seconds));
  Result := T Mod SecondsInDay;
End;

Function DecTime(T: TStTime; Hours, Minutes, Seconds: Byte): TStTime;
{-Subtract the specified hours,minutes,seconds from T and return the result}
Begin
  Hours := Hours Mod HoursInDay;
  Dec(T, HMStoStTime(Hours, Minutes, Seconds));
  If T < 0 Then
    Result := T + SecondsInDay
  Else
    Result := T;
End;

Function RoundToNearestHour(T: TStTime; Truncate: Boolean): TStTime;
{-Round T to the nearest hour, or Truncate minutes and seconds from T}
Var
  Hours, Minutes, Seconds: Byte;
Begin
  StTimeToHMS(T, Hours, Minutes, Seconds);
  Seconds := 0;
  If Not Truncate Then
    If Minutes >= (MinutesInHour Div 2) Then
      Inc(Hours);
  Minutes := 0;
  Result := HMStoStTime(Hours, Minutes, Seconds);
End;

Function RoundToNearestMinute(Const T: TStTime; Truncate: Boolean): TStTime;
{-Round T to the nearest minute, or Truncate seconds from T}
Var
  Hours, Minutes, Seconds: Byte;
Begin
  StTimeToHMS(T, Hours, Minutes, Seconds);
  If Not Truncate Then
    If Seconds >= (SecondsInMinute Div 2) Then
      Inc(Minutes);
  Seconds := 0;
  Result := HMStoStTime(Hours, Minutes, Seconds);
End;

Procedure DateTimeDiff(DT1: TStDateTimeRec; Var DT2: TStDateTimeRec;
  Var Days: Longint; Var Secs: Longint);
{-Return the difference in days and seconds between two points in time}
Var
  tDT1, tDT2: TStDateTimeRec;
Begin
  tDT1 := DT1;
  tDT2 := DT2;
  {swap if tDT1 later than tDT2}
  If (tDT1.D > tDT2.D) Or ((tDT1.D = tDT2.D) And (tDT1.T > tDT2.T)) Then
    ExchangeStructs(tDT1, tDT2, sizeof(TStDateTimeRec));

  {the difference in days is easy}
  Days := tDT2.D - tDT1.D;

  {difference in seconds}
  If tDT2.T < tDT1.T Then
    Begin
      {subtract one day, add 24 hours}
      Dec(Days);
      Inc(tDT2.T, SecondsInDay);
    End;
  Secs := tDT2.T - tDT1.T;
End;

Function DateTimeToStDate(DT: TDateTime): TStDate;
{-Convert Delphi TDateTime to TStDate}
Var
  Day, Month, Year: Word;
Begin
  DecodeDate(DT, Year, Month, Day);
  Result := DMYToStDate(Day, Month, Year, 0);
End;

Function DateTimeToStTime(DT: TDateTime): TStTime;
{-Convert Delphi TDateTime to TStTime}
Var
  Hour, Min, Sec, MSec: Word;
Begin
  DecodeTime(DT, Hour, Min, Sec, MSec);
  Result := HMSToStTime(Hour, Min, Sec);
End;

Function StDateToDateTime(D: TStDate): TDateTime;
{-Convert TStDate to TDateTime}
Var
  Day, Month, Year: Integer;
Begin
  Result := 0;
  If D <> BadDate Then
    Begin
      StDateToDMY(D, Day, Month, Year);
      Result := EncodeDate(Year, Month, Day);
    End;
End;

Function StTimeToDateTime(T: TStTime): TDateTime;
{-Convert TStTime to TDateTime}
Var
  Hour, Min, Sec: Byte;
Begin
  Result := 0;
  If T <> BadTime Then
    Begin
      StTimeToHMS(T, Hour, Min, Sec);
      Result := EncodeTime(Hour, Min, Sec, 0);
    End;
End;

Procedure IncDateTime(DT1: TStDateTimeRec; Var DT2: TStDateTimeRec; Days: Integer; Secs: Longint);
{-Increment (or decrement) DT1 by the specified number of days and seconds
  and put the result in DT2}
Begin
  DT2 := DT1;

  {date first}
  Inc(DT2.D, Longint(Days));

  If Secs < 0 Then
    Begin
      {change the sign}
      Secs := -Secs;

      {adjust the date}
      Dec(DT2.D, Secs Div SecondsInDay);
      Secs := Secs Mod SecondsInDay;

      If Secs > DT2.T Then
        Begin
          {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
          Dec(DT2.D);
          Inc(DT2.T, SecondsInDay);
        End;

      {now subtract the seconds}
      Dec(DT2.T, Secs);
    End
  Else
    Begin
      {increment the seconds}
      Inc(DT2.T, Secs);

      {adjust date if necessary}
      Inc(DT2.D, DT2.T Div SecondsInDay);

      {force time to 0..SecondsInDay-1 range}
      DT2.T := DT2.T Mod SecondsInDay;
    End;
End;

Function Convert2ByteDate(TwoByteDate: Word): TStDate;
Begin
  Result := Longint(TwoByteDate) + Date1900;
End;

Function Convert4ByteDate(FourByteDate: TStDate): Word;
Begin
  Result := Word(FourByteDate - Date1900);
End;

Procedure SetDefaultYear;
{-Initialize DefaultYear and DefaultMonth}
Var
  Month, Day, Year: Word;
  T: TDateTime;
Begin
  T := Now;
  DecodeDate(T, Year, Month, Day);
  DefaultYear := Year;
  DefaultMonth := Month;
End;

Function FFStDateToString(Const ADate: TStDate): String;
Begin
  Result := FormatDateTime(ShortDateFormat, ADate - StDateConv);
End;

Function FFStringToStDate(Const ADateStr: String): TStDate;
Begin
  Result := Trunc(StrToDateTime(ADateStr)) + StDateConv;
End;

Function FFStTimeToString(Const ATime: TStTime): String;
Begin
  Result := FormatDateTime(ShortTimeFormat, StTimeToDateTime(ATime));
End;

Function FFStringToStTime(Const ATimeStr: String): TStTime;
Begin
  Result := DateTimeToStTime(StrToDateTime(ATimeStr));
End;

Initialization
  {initialize DefaultYear and DefaultMonth}
  SetDefaultYear;
End.

