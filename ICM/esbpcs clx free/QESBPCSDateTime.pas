{: Contains the Date & Time Routines used by ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

     Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCSDateTime;

{$I esbpcs.inc}

interface

uses
     {$IFDEF MSWINDOWS}
     Windows,
     {$ENDIF}
     {$IFDEF LINUX}
     Types,
     {$ENDIF}
     QESBPCSGlobals;

{--- Current Date ---}
{: A Speed Optimised Routine to get the Current Date. Time Portion is Zero.
 Better to use <See Routine=ESBToday> as that is the newer name of this routine.
 @cat DTMath
}
function OptDate: TDateTime;

{: A Speed Optimised Routine to get the Current Date. Time Portion is Zero.
 @cat DTMath
}
function ESBToday: TDateTime;

{: A Speed Optimised Routine to get the Yesterday's Date. Time Portion is Zero.
 @cat DTMath
}
function ESBYesterday: TDateTime;

{: A Speed Optimised Routine to get the Tomorrow's Date. Time Portion is Zero.
 @cat DTMath
}
function ESBTomorrow: TDateTime;

{--- Conversions ---}

{: A Speed Optimised DecodeDate developed by Ken Otto that is many times faster
 than the once included in SysUtils. If you need Words rather than Integers
 use the slightly slower OptDecodeDateW.
 @cat DTConv
}
procedure OptDecodeDateI (const DT: TDateTime; out Year, Month, Day: Integer);

{: A Speed Optimised DecodeDate developed by Ken Otto that is many times faster
 than the once included in SysUtils. If you want even faster results and
 are happy to use Integers than use the slightly slower OptDecodeDateI.
 @cat DTConv
}
procedure OptDecodeDateW (const DT: TDateTime; out Year, Month, Day: Word);

{: A Speed Optimised EncodeDate developed by Ken Otto that is many times faster
 than the once included in SysUtils, and includes Exception Handling. If you
 need Words rather than Integers use the slightly slower OptEncodeDateW.
 @cat DTConv
}
function OptEncodeDateI (Year, Month, Day: Integer): TDateTime;

{: A Speed Optimised EncodeDate developed by Ken Otto that is many times faster
 than the once included in SysUtils, and includes Exception Handling. If you
 want even faster results and are happy to use Integers than use the
 slightly slower OptEncodeDateI.
 @cat DTConv
}
function OptEncodeDateW (Year, Month, Day: Word): TDateTime;

{: A Speed Optimised Routine for getting the Year portion of a Date based on
 Routine by Ken Otto that is many times faster than using DecodeDate in
 SysUtils.
 @cat DTConv
}
function OptDate2Year (const DT: TDateTime): Word;

{: A Speed Optimised Routine for getting the Month portion of a Date based on
 Routine by Ken Otto that is many times faster than using DecodeDate in
 SysUtils.
 @cat DTConv
}
function OptDate2Month (const DT: TDateTime): Word;

{: A Speed Optimised Routine for getting the Day portion of a Date based on
 Routine by Ken Otto that is many times faster than using DecodeDate in
 SysUtils.
 @cat DTConv
}
function OptDate2Day (const DT: TDateTime): Word;

{: An Enhanced EncodeDate that includes Exception Handling.
 Use <See Routine=OptEncodeDateI> or <See Routine=OptEncodeDateW> - this
 routine has been kept for backward compatibility.
 @cat DTConv
}
function ESBEncodeDate (const Year, Month, Day: Word): TDateTime;

{: An Enhanced DecodeDate that includes Exception Handling.
 Use <See Routine=OptDecodeDateI> or <See Routine=OptDecodeDateW> - this
 routine has been kept for backward compatibility.
 @cat DTConv
}
procedure ESBDecodeDate (const Date: TDateTime; var Year, Month, Day: Word);

{: An Enhanced EncodeTime that includes Exception Handling.
 @cat DTConv
}
function ESBEncodeTime (const Hour, Min, Sec, MSec: Word): TDateTime;

{: An Enhanced EncodeTime that includes Exception Handling.
 @cat DTConv
}
procedure ESBDecodeTime (const Time: TDateTime; var Hour, Min, Sec, MSec: Word);

{: Returns Date as a String using ShortDateFormat from Regional
 Settings. If <See Var=ESBBlankWhenZero> is true and the Date is Zero,
 then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if a
  Date	Conversion error occurs.
 @param DT Date/Time to Convert.
 @cat DTConv
}
function Date2Str (const DT: TDateTime): string;

{: Returns Date as a String in the format YYYYMMDD.
 If <See Var=ESBBlankWhenZero> is true and the Date is Zero,
 then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if a
  Date	Conversion error occurs.
 @param DT Date/Time to Convert.
 @cat DTConv
}
function Date2DigitStr (const DT: TDateTime): string;

{: Returns Date as a String using Format. If <See Var=ESBBlankWhenZero> is
 true and the Date is Zero, then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if
  a Date Conversion error occurs.
 @param DT Date/Time to Convert.
 @param Format the Date Format to use, eg 'DD/MM/YYYY'.
 @cat DTConv
}
function Date2FormatStr (const DT: TDateTime; const Format: string): string;

{: Returns the Day of the Month number from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Day of the Month.
 @cat DTConv
}
function Date2Day (const DT: TDateTime): Word;

{: Returns the Month number from a given date/time, 1 = Jan, etc.
 @param DT Date/Time to Convert.
 @returns the Month where 1 = Jan, through 12 = Dec.
 @cat DTConv
 @cat MonthMath
}
function Date2Month (const DT: TDateTime): Word;

{: Returns the Year from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Year component, including Century.
 @cat DTConv
 @cat YearMath
}
function Date2Year (const DT: TDateTime): Word;

{: Returns the Time Portion as a string HH:MM with time separator
 from the Regional Settings. If <See Var=ESBBlankWhenZero> is true and
 DT is Zero, then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if
  a Time Conversion error occurs.
 @param DT Date/Time to Convert.
 @cat DTConv
}
function Time2Str (const DT: TDateTime): string;

{: Returns Time as a String using Format. If <See Var=ESBBlankWhenZero> is
 true and DT is Zero, then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if
  a Date Conversion error occurs.
 @param DT Date/Time to Convert.
 @param Format the Time Format to use, eg 'hh:mm'.
 @cat DTConv
}
function Time2FormatStr (const DT: TDateTime; const Format: string): string;

{: Returns Date/Time as a String using Format. If <See Var=ESBBlankWhenZero>
 is true and DT is Zero, then an Empty String will be returned.<p>
 If an error occurs an Empty String is Returned.
 If <See Var=ESBRaiseDateError> is true then an Exception is raised if
  a Date Conversion error occurs.
 @param DT Date/Time to Convert.
 @param Format the Date/Time Format to use, eg 'DD/MM/YYYY hh:mm'.
 @cat DTConv
}
function DateTime2FormatStr (const DT: TDateTime; const Format: string): string;

{: Returns the Hour from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Hour component.
 @cat DTConv
}
function Time2Hr (const DT: TDateTime): Word;

{: Returns the Minute from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Minute component.
 @cat DTConv
}
function Time2Min (const DT: TDateTime): Word;

{: Returns the Second from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Second component.
 @cat DTConv
}
function Time2Sec (const DT: TDateTime): Word;

{: Returns the Millisecond from a given date/time.
 @param DT Date/Time to Convert.
 @returns the Millisecond component.
 @cat DTConv
}
function Time2MSec (const DT: TDateTime): Word;

{: Returns the current Year - from Today's Date.
 This Integer routine is faster than <See Routine=ThisYear>.
 @cat DTConv
 @cat YearMath
}
function OptThisYear: Integer;

{: Returns the current Month - from Today's Date.
 This Integer routine is faster than <See Routine=ThisMonth>.
 @cat DTConv
 @cat MonthMath
}
function OptThisMonth: Integer;

{: Returns the current Day - from Today's Date.
 This Integer routine is faster than <See Routine=ThisDay>.
 @cat DTConv
}
function OptThisDay: Integer;

{: Returns the current Year - from Today's Date.
 For a faster Integer routine see <See Routine=OptThisYear>.
 @cat DTConv
 @cat YearMath
}
function ThisYear: Word;

{: Returns the current Month - from Today's Date.
 For a faster Integer routine see <See Routine=OptThisMonth>.
 @cat DTConv
 @cat MonthMath
}
function ThisMonth: Word;

{: Returns the current Day - from Today's Date.
 For a faster Integer routine see <See Routine=OptThisDay>.
 @cat DTConv
}
function ThisDay: Word;

{: Returns the current Hour - from the current Time.
 @cat DTConv
}
function ThisHr: Word;

{: Returns the current Minute - from the current Time.
 @cat DTConv
}
function ThisMin: Word;

{: Returns the current Second - from the current Time.
 @cat DTConv
}
function ThisSec: Word;

{: Returns the current Millisecond - from the current Time.
 @cat DTConv
}
function ThisMSec: Word;

{: Converts a string containing a Time into a DateTime.
 The following are all exceptable separators for entry: [':', '.']
 though the current TimeSeparator will be used for display
 Times can be entered without Separators but Leading Zeroes must then be used
     and the format is assumed to be either HHMM or HHMMSS.
 @param TimeStr The String to convert.
 @cat DTConv
}
function Str2Time (const TimeStr: string): TDateTime;

{: Converts a string containing a Date into a DateTime. If the Item
 has no month and/or year then the current month and year will be assumed. <p>
 The following are all exceptable separators for entry: [' ', ',', '.', '/', '-', '\']
 though the current DateSeparator will be used for display.<p>
 Dates can be entered without Separators but Leading Zeroes must then be used.
 Date parsing is highly dependant upon the current ShortDateFormat.

 <See Var=ESB2DigitYr> contols the different ways in which 2 Digit Years
 are handled in	Str2Date.<p>
 edyNone - Nothing is done, left to Delphi to handle. <p>
 edyCutOff - the  ESB2DigitCutOff is used to decide which century
  the date lies in. If 1900 + Yr less than <See Var=ESB2DigitCutOff>
  then it is assumed that 2000 + Yr is wanted, otherwise 1900 + Yr
  is used.<p>
 edyHistoric  - asssumes that the yr is this year or earlier.

 @param DateStr The String to convert.
 @param Year If Year and StartMonth are entered then if the Month is at least
  StartMonth, then this Year is implied. If it is less then the StartMonth
  then Year + 1 is implied. Only has meaning if the Year is omitted in the String.
 @param StartMonth If Year and StartMonth are entered then if the Month is at least
  StartMonth, then this Year is implied. If it is less then the StartMonth
  then Year + 1 is implied. Only has meaning if the Year is omitted in the String.
 @cat DTConv
}
function Str2Date (const DateStr: string): TDateTime; overload;
function Str2Date (const DateStr: string; const Year, StartMonth: Integer): TDateTime; overload;

{: Converts a string containing a Date into a DateTime. If the Item
 has no month and/or year then the current month and year will be assumed. <p>
 If a 2 Digit Year is used then it is assumted that it is this year or earlier.
 @param DateStr The String to convert.
 @cat DTConv
}
function Str2HistoricDate (const DateStr: string): TDateTime;

{: Converts a string containing a Date into a DateTime. If the Item
 has no month and/or year then the current month and year will be assumed. <p>
 If a 2 Digit Year is used then CutOff is used to decide which century
 the date lies in. If 1900 + Yr less than CutOff then it is assumed that
 2000 + Yr is wanted, otherwise 1900 + Yr is used.
 @param DateStr The String to convert.
 @cat DTConv
}
function Str2CutOffDate (const DateStr: string; const CutOff: Word): TDateTime;

{: Converts a string containing a Date into a DateTime. Uses supplied Format
 instead of ShortDateFormat for the String. If the Item	has no month
 and/or year then the current month and year will be assumed. <p>

 <See Var=ESB2DigitYr> contols the different ways in which 2 Digit Years
 are handled in Str2Date. <p>
 edyNone - Nothing is done, left to Delphi to handle.<p>
 edyCutOff - the  ESB2DigitCutOff is used to decide which century
  the date lies in. If 1900 + Yr less than <See Var=ESB2DigitCutOff>
  then it is assumed that 2000 + Yr is wanted, otherwise 1900 + Yr
  is used.<p>
 edyHistoric  - asssumes that the yr is this year or earlier.
 @param DateStr The String to convert.
 @param Format that the Date is assumed to be in, eg 'DD/MM/YY'.
 @cat DTConv
}
function StrFormat2Date (const DateStr: string; const Format: string): TDateTime;

{: Converts a string containing a Date into a DateTime. Assumes formatting
 in the form of 'YYYYMMDD'. If String has length less then 8 or is an
 invalid Date then Exception is raised.
 @param DateStr The String to convert.
 @cat DTConv
}
function DigitStr2Date (const DateStr: string): TDateTime;

{: Returns the Long Month Description for the supplied Date. Relies
 on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Long Month Name of the Month component.
 @cat DTConv
 @cat MonthMath
}
function Date2LongMonth (const DT: TDateTime): string;

{: Returns the Short Month Description for the supplied Date. Relies
 on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Short Month Name of the Month component.
 @cat DTConv
 @cat MonthMath
}
function Date2ShortMonth (const DT: TDateTime): string;

{: Returns the Short Month Description for the supplied Date followed by
 a space then the last two digits of the year. Relies on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Short Month Name of the Month component.
 @cat DTConv
 @cat MonthMath
}
function Date2ShortMonthYY (const DT: TDateTime): string;

{: Returns the Short Month Description for the supplied Date followed by
 a space then the year. Relies on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Short Month Name of the Month component.
 @cat DTConv
 @cat MonthMath
}
function Date2ShortMonthYYYY (const DT: TDateTime): string;

{: Returns the Long Day of Week Description for the supplied Date. Relies
 on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Long Day of Week Name for the given Date.
 @cat DTConv
}
function Date2LongDOW (const DT: TDateTime): string;

{: Returns the Short Day of Week Description for the supplied Date. Relies
 on Regional Settings.
 @param DT Date/Time to convert.
 @returns the Short Day of Week Name for the given Date.
 @cat DTConv
}
function Date2ShortDOW (const DT: TDateTime): string;

{: Given a Month Name this routines searches through the Short and then
 Long Month Names supplied in the Registry to do a Left Match, and
 then return the Month Number. So for English Names, 'Ma' would return
 3 for 'March' .
 @param MonthName Name of the Month to search for.
 @returns the Month Number, 1 through 12 - 0 implies not found.
 @cat DTConv
 @cat MonthMath
}
function MonthName2Month (const MonthName: string): Word;

{: Given a Day Name this routines searches through the Short and then
 Long Day Names supplied in the Registry to do a Left Match, and
 then return the Day Number. So for English Names, 'T' would return
 3 for 'Tuesday'.
 @param DayName Name of the Day of Week to search for.
 @returns the DOW Number, 1 through 7 - 0 implies not found. 1 is Sunday.
 @cat DTConv
}
function DayName2DOW (const DayName: string): Byte;

{: Returns the current Day of the week from Today's Date.
 @returns the DOW Number, 1 through 7 where 1 is Sunday.
 @cat DTConv
}
function ThisDOW: Byte;

{: Identifies which of the three common formats for Date Order
 that a given Date Format is in.

 edoDMY - Day/Month/Year<p>
 edoMDY - Month/Day/Year<p>
 edoYD - Year/Month/Day

 @param DateFormat that the Date is assumed to be in, eg 'DD/MM/YY'.
 @cat DTConv
}
function GetESBDateOrder (const DateFormat: string): TESBDateOrder;

{: Returns the (Year * 100 + Month) number for a given date/time, 3 Mar 2000
 would give 200003
 @returns the Month where 1 = Jan, through 12 = Dec.
 @cat DTConv
 @cat MonthMath
}
function Date2YearMonth (const DT: TDateTime): LongWord;

{: Returns the number of Hours the specified number of Days represents.
 @param Value Number of Days
 @returns the Number of Hours
 @cat DTConv
}
function Days2Hrs (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Hours represents.
 @param Value Number of Hours
 @returns the Number of Days
 @cat DTConv
}
function Hrs2Days (const Value: Extended): Extended;

{: Returns the number of Hours the specified number of Mintues represents.
 @param Value Number of Minutes
 @returns the Number of Hours
 @cat DTConv
}
function Mins2Hrs (const Value: Extended): Extended;

{: Returns the number of Minutes the specified number of Hours represents.
 @param Value Number of Hours
 @returns the Number of Minutes
 @cat DTConv
}
function Hrs2Mins (const Value: Extended): Extended;

{: Returns the number of Minutes the specified number of Seconds represents.
 @param Value Number of Minutes
 @returns the Number of Seconds
 @cat DTConv
}
function Mins2Secs (const Value: Extended): Extended;

{: Returns the number of Seconds the specified number of Mintues represents.
 @param Value Number of Seconds
 @returns the Number of Minutes
 @cat DTConv
}
function Secs2Mins (const Value: Extended): Extended;

{: Returns the number of Seconds the specified number of Days represents.
 @param Value Number of Days
 @returns the Number of Seconds
 @cat DTConv
}
function Days2Secs (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Seconds represents.
 @param Value Number of Seconds
 @returns the Number of Days
 @cat DTConv
}
function Secs2Days (const Value: Extended): Extended;

{: Returns the number of Hours the specified number of Seconds represents.
 @param Value Number of Seconds
 @returns the Number of Hours
 @cat DTConv
}
function Secs2Hrs (const Value: Extended): Extended;

{: Returns the number of Seconds the specified number of Hours represents.
 @param Value Number of Hours
 @returns the Number of Seconds
 @cat DTConv
}
function Hrs2Secs (const Value: Extended): Extended;

{: Returns the number of Minutes the specified number of Days represents.
 @param Value Number of Days
 @returns the Number of Minutes
 @cat DTConv
}
function Days2Mins (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Minutes represents.
 @param Value Number of Minutes
 @returns the Number of Days
 @cat DTConv
}
function Mins2Days (const Value: Extended): Extended;

{: Returns the number of Weeks the specified number of Days represents.
 @param Value Number of Days
 @returns the Number of Weeks
 @cat DTConv
}
function Days2Weeks (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Weeks represents.
 @param Value Number of Weeks
 @returns the Number of Days
 @cat DTConv
}
function Weeks2Days (const Value: Extended): Extended;

{: Returns the number of Fortnights the specified number of Days represents.
 @param Value Number of Days
 @returns the Number of Fortnights
 @cat DTConv
}
function Days2Fortnights (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Fortnights represents.
 @param Value Number of Fortnights
 @returns the Number of Days
 @cat DTConv
}
function Fortnights2Days (const Value: Extended): Extended;

{: Returns the number of Months the specified number of Days represents. Months
 are based on Synodic Months - see <See Const=DaysPerSynodicMonth>.
 @param Value Number of Days
 @returns the Number of Months
 @cat DTConv
}
function Days2Months (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Months represents.  Months
 are based on Synodic Months - see <See Const=DaysPerSynodicMonth>.
 @param Value Number of Months (Synodic)
 @returns the Number of Days
 @cat DTConv
}
function Months2Days (const Value: Extended): Extended;

{: Returns the number of Years the specified number of Days represents. Years
 are based on Tropical Years - see <See Const=DaysPerTropicalYear>.
 @param Value Number of Days
 @returns the Number of Years (Tropical)
 @cat DTConv
}
function Days2Years (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Years represents.  Months
 are based on Tropical years - see <See Const=DaysPerTropicalYear>.
 @param Value Number of Years (Tropical)
 @returns the Number of Days
 @cat DTConv
}
function Years2Days (const Value: Extended): Extended;

{: Returns the number of Years the specified number of Days represents. Years
 are based on Gregorian Years - see <See Const=DaysPerGregorianYear>.
 @param Value Number of Days
 @returns the Number of Years (Gregorian)
 @cat DTConv
}
function Days2YearsGregorian (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Years represents.  Months
 are based on Gregorian years - see <See Const=DaysPerGregorianYear>.
 @param Value Number of Years (Gregorian)
 @returns the Number of Days
 @cat DTConv
}
function YearsGregorian2Days (const Value: Extended): Extended;

{: Returns the number of Years the specified number of Days represents. Years
 are based on Julian Years - see <See Const=DaysPerJulianYear>.
 @param Value Number of Days
 @returns the Number of Years (Julian)
 @cat DTConv
}
function Days2YearsJulian (const Value: Extended): Extended;

{: Returns the number of Days the specified number of Years represents.  Months
 are based on Julian years - see <See Const=DaysPerJulianYear>.
 @param Value Number of Years (Julian)
 @returns the Number of Days
 @cat DTConv
}
function YearsJulian2Days (const Value: Extended): Extended;

{--- Year Based Routines ---}

{: Is given Year a Leap Year. Thanks to Dr John Stockton
 for suggesting a faster methodology.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
 @cat DTComparison
}
function IsLeapYear (const Year: Word): Boolean; overload;
function IsLeapYear (const Year: Integer): Boolean; overload;
function IsLeapYear (const DT: TDateTime): Boolean; overload;

{: Returns the GoldenNumber for a given Year. Values are 1 -> 19.
 The relationship between the Moon's Phases and the Year, repeats
 itself every 19 years.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetGoldenNumber (const Year: Word): Integer; overload;
function GetGoldenNumber (const Year: Integer): Integer; overload;
function GetGoldenNumber (const DT: TDateTime): Integer; overload;

{: Return the Epact, which is a measure of the age of the moon (ie the number
 of days that have passed since an "official" new moon) on a particular date.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetEpact (const Year: Word): Integer; overload;
function GetEpact (const Year: Integer): Integer; overload;
function GetEpact (const DT: TDateTime): Integer; overload;

{:Returns the Date of Easter Sunday for given Year - based on current Calendar.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetEasterSunday (const Year: Word): TDateTime; overload;
function GetEasterSunday (const Year: Integer): TDateTime; overload;
function GetEasterSunday (const DT: TDateTime): TDateTime; overload;

{:Returns the Date of Good Friday for given Year - based on current Calendar.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetGoodFriday (const Year: Word): TDateTime; overload;
function GetGoodFriday (const Year: Integer): TDateTime; overload;
function GetGoodFriday (const DT: TDateTime): TDateTime; overload;

{: Returns Christmas Day, for a given Year.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetChristmasDay (const Year: Word): TDateTime; overload;
function GetChristmasDay (const Year: Integer): TDateTime; overload;
function GetChristmasDay (const DT: TDateTime): TDateTime; overload;

{: Returns First Day of the Year, for a given Year.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetFirstDayOfYear (const Year: Word): TDateTime; overload;
function GetFirstDayOfYear (const Year: Integer): TDateTime; overload;
function GetFirstDayOfYear (const DT: TDateTime): TDateTime; overload;

{: Returns Last Day of the Year, for a given Year.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
}
function GetLastDayOfYear (const Year: Word): TDateTime; overload;
function GetLastDayOfYear (const Year: Integer): TDateTime; overload;
function GetLastDayOfYear (const DT: TDateTime): TDateTime; overload;

{: Returns First Sunday of the Year, for a given Year. Used in
 Week No Routines.
 @param Year the Year to be processed - should be 4 digit, eg 1999.
 @param DT the Date to be processed.
 @cat DTMath
 @cat YearMath
 @cat WeekMath
}
function GetFirstSundayOfYear (const Year: Word): TDateTime; overload;
function GetFirstSundayOfYear (const Year: Integer): TDateTime; overload;
function GetFirstSundayOfYear (const DT: TDateTime): TDateTime; overload;

{--- Date Arithmetic ---}

{: Returns the number of whole minutes apart the two times are (date portion ignored)
 and DT1 is assumed to be before DT2.
 @param DT1 First Time, Date portion ignores - assumed to be the earlier time.
 @param DT2 Second Time, Date portion ignores - assumed to be the later time.
 @cat DTMath
}
function MinutesApart (const DT1, DT2: TDateTime): Word;

{: Returns the decimal number of Days apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInDays (const DT1, DT2: TDateTime): Extended;

{: Returns the decimal number of Weeks apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInWeeks (const DT1, DT2: TDateTime): Extended;

{: Returns the decimal number of fortnights apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInFortnights (const DT1, DT2: TDateTime): Extended;

{: Returns the decimal number of hours apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInHrs (const DT1, DT2: TDateTime): Extended;

{: Returns the decimal number of Minutes apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInMins (const DT1, DT2: TDateTime): Extended;

{: Returns the decimal number of Seconds apart the two date/times are. If 0 then
 they are the same Date/Time, if negative then DT1 occurs after DT2 DT2.
 @param DT1 First Date/Time to process.
 @param DT2 Second DateTime to process.
 @cat DTMath
}
function TimeApartInSecs (const DT1, DT2: TDateTime): Extended;

{: Converts a time in MilliSeconds to a string of the form 'H:MM:SS.mmm'.
 @param MS Value in Milliseconds.
 @cat DTMath
}
function MS2TimeStr (const MS: Int64): string;

{: Returns the current date/time as a string in the Format of: YYYYMMDD-HHMMSSmmm.
 @cat DTMath
}
function GetDateTimeStamp: string;

{: Adjusts the date so that it has the Year specified. Makes 29 Feb of any
 year that is not a Leap year 1 Mar.
 @param D Date/Time to process.
 @param Year Year to make the date in, eg 1999.
 @cat DTMath
 @cat YearMath
}
function AdjustDateYear (const D: TDateTime; const Year: Word): TDateTime; overload;
function AdjustDateYear (const D: TDateTime; const Year: Integer): TDateTime; overload;

{: Adds a Floating Point amount of Seconds to a Given Date/Time.
 @param DT Date/Time to process.
 @param Secs Number of Seconds to Add - can be negative.
 @cat DTMath
}
function AddSecs (const DT: TDateTime; const Secs: Extended): TDateTime;

{: Adds a Floating Point amount of Minutes to a Given Date/Time.
 @param DT Date/Time to process.
 @param Mins Number of Minutes to Add - can be negative.
 @cat DTMath
}
function AddMins (const DT: TDateTime; const Mins: Extended): TDateTime;

{: Adds a Floating Point amount of Hours to a Given Date/Time.
 @param DT Date/Time to process.
 @param Hrss Number of Hours to Add - can be negative.
 @cat DTMath
}
function AddHrs (const DT: TDateTime; const Hrs: Extended): TDateTime;

{: Adds a Floating Point amount of Days to a Given Date/Time.
 Though this is the same as normal Addition it is added for completion.
 @param DT Date/Time to process.
 @param Dayss Number of Days to Add - can be negative.
 @cat DTMath
}
function AddDays (const DT: TDateTime; const Days: Extended): TDateTime;

{: Adds a Floating Point amount of Weeks to a Given Date/Time.
 @param DT Date/Time to process.
 @param Weeks Number of Weeks to Add - can be negative.
 @cat DTMath
 @cat WeekMath
}
function AddWeeks (const DT: TDateTime; const Weeks: Extended): TDateTime;

{: Adds a Floating Point amount of Fortnights to a Given Date/Time.
 @param DT Date/Time to process.
 @param Fortnights Number of Fortnights to Add - can be negative.
 @cat DTMath
}
function AddFortnights (const DT: TDateTime; const FNights: Extended): TDateTime;

{: Adds a Floating Point amount of Months to a Given Date/Time.
 Fractional portion of Month is assumed to be related to 30 day months.
 Time portion preserved.<p>

 If adding Months results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Months Added. The fractional part always
 is added to the resultant Date/Time.

 @param DT Date/Time to process.
 @param Months Number of Months to Add - can be negative.
 @cat DTMath
 @cat MonthMath
}
function AddMonths (const DT: TDateTime; const Months: Extended): TDateTime;

{: Adds a Specified Number of Calendar Months to a Given Date/Time.
 Time portion preserved.<p>

 If adding Months results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Months Added. The fractional part always
 is added to the resultant Date/Time.

 @param DT Date/Time to process.
 @param Months Number of Months to Add - can be negative.
 @cat DTMath
 @cat MonthMath
}
function AddCalendarMonths (const DT: TDateTime; const Months: Integer): TDateTime;

{: Adds a Floating Point amount of Quarters to a Given Date/Time.
 Fractional portion of Quarter is assumed to be related to 30 day month,
 as the AddMonths routine is used.
 Time portion preserved. <p>

 If adding Quarters results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Quarter Added. The fractional part always
 is added to the resultant Date/Time.

 @param DT Date/Time to process.
 @param Qtrs Number of Quarters to Add - can be negative.
 @cat DTMath
}
function AddQuarters (const DT: TDateTime; const Qtrs: Extended): TDateTime;

{: Adds a Floating Point amount of Semesters to a Given Date/Time.
 Fractional portion of Semester is assumed to be related to 30 day month,
 as the AddMonths routine is used.
 Time portion preserved.<p>

 If adding Semesters results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Semeters Added. The fractional part always
 is added to the resultant Date/Time.

 @param DT Date/Time to process.
 @param Sems Number of Semesters to Add - can be negative.
 @cat DTMath
}
function AddSemesters (const DT: TDateTime; const Sems: Extended): TDateTime;

{: Adds a Floating Point amount of Years to a Given Date/Time.
 Fractional portion of Year is assumed to be related to 365.25 day years.
 Time portion preserved.<p>

 If Adding Years results in landing on Feb 29 in a non-leap year, then
 this will be converted to Feb 28.This only applies to the
 integral component of the Years Added. The fractional part always
 is added to the resultant Date/Time.

 @param DT Date/Time to process.
 @param Yrs Number of Years to Add - can be negative.
 @cat DTMath
 @cat YearMath
}
function AddYrs (const DT: TDateTime; const Yrs: Extended): TDateTime;

{: Subtracts a Floating Point amount of Seconds from a Given Date/Time.
 @param DT Date/Time to process.
 @param Secs Number of Seconds to Subtract - can be negative.
 @cat DTMath
}
function SubtractSecs (const DT: TDateTime; const Secs: Extended): TDateTime;

{: Subtracts a Floating Point amount of Minutes from a Given Date/Time.
 @param DT Date/Time to process.
 @param Mins Number of Minutes to Subtract - can be negative.
 @cat DTMath
}
function SubtractMins (const DT: TDateTime; const Mins: Extended): TDateTime;

{: Subtracts a Floating Point amount of Hours from a Given Date/Time.
 @param DT Date/Time to process.
 @param Hrs Number of Hours to Subtract - can be negative.
 @cat DTMath
}
function SubtractHrs (const DT: TDateTime; const Hrs: Extended): TDateTime;

{: Subtracts a Floating Point amount of Days from a Given Date/Time.
 @param DT Date/Time to process.
 @param Days Number of Days to Subtract - can be negative.
 @cat DTMath
}
function SubtractDays (const DT: TDateTime; const Days: Extended): TDateTime;

{: Subtracts a Floating Point amount of Weeks from a Given Date/Time.
 @param DT Date/Time to process.
 @param Weeks Number of Weeks to Subtract - can be negative.
 @cat DTMath
 @cat WeekMath
}
function SubtractWeeks (const DT: TDateTime; const Weeks: Extended): TDateTime;

{: Subtracts a Floating Point amount of Fortnights from a Given Date/Time.
 @param DT Date/Time to process.
 @param Fortnights Number of Fortnights to Subtract - can be negative.
 @cat DTMath
}
function SubtractFortnights (const DT: TDateTime; const FNights: Extended): TDateTime;

{: Subtracts a Floating Point amount of Months from a Given Date/Time.
 Fractional portion of Month is assumed to be related to 30 day months.
 Time portion preserved. <p>

 If Subtracting Months results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Months Subtracted. The fractional part always
 is Subtracted from the resultant Date/Time.

 @param DT Date/Time to process.
 @param Months Number of Months to Subtract - can be negative.
 @cat DTMath
 @cat MonthMath
}
function SubtractMonths (const DT: TDateTime; const Months: Extended): TDateTime;

{: Subtracts a Floating Point amount of Quarters from a Given Date/Time.
 Fractional portion of Quarter is assumed to be related to 30 day month,
 as the AddMonths routine is used. Time portion preserved.<p>

 If Subtracting Quarters results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Quarter Subtracted. The fractional part always
 is Subtracted from the resultant Date/Time.

 @param DT Date/Time to process.
 @param Qtrs Number of Quarters to Subtract - can be negative.
 @cat DTMath
}
function SubtractQuarters (const DT: TDateTime; const Qtrs: Extended): TDateTime;

{: Subtracts a Floating Point amount of Semesters from a Given Date/Time.
 Fractional portion of Semester is assumed to be related to 30 day month,
 as the AddMonths routine is used. Time portion preserved.<p>

 If Subtracting Semesters results in landing on a nonsense date like 31 Apr
 then the last day in the month is used. This only applies to the
 integral component of the Semeters Subtracted. The fractional part always
 is Subtracted from the resultant Date/Time.

 @param DT Date/Time to process.
 @param Sems Number of Semesters to Subtract - can be negative.
 @cat DTMath
}
function SubtractSemesters (const DT: TDateTime; const Sems: Extended): TDateTime;

{: Subtracts a Floating Point amount of Years from a Given Date/Time.
 Fractional portion of Year is assumed to be related to 365.25 day years.
 Time portion preserved.<p>

 If Subtracting Years results in landing on Feb 29 in a non-leap year, then
 this will be converted to Feb 28.This only applies to the
 integral component of the Years Subtracted. The fractional part always
 is Subtracted from the resultant Date/Time.

 @param DT Date/Time to process.
 @param Yrs Number of Years to Subtract - can be negative.
 @cat DTMath
 @cat YearMath
}
function SubtractYrs (const DT: TDateTime; const Yrs: Extended): TDateTime;

{: Returns Last Day of the Month, for a given Date/Time - Time portion preserved.
 Alternatively for a given Month Year.
 @param DT Date/Time to process.
 @param Month Month in given year, 1 = Jan, 12 = Dec.
 @param Year 4-digit Year, such as 1999.
 @cat DTMath
 @cat MonthMath
}
function GetLastDayOfMonth (const DT: TDateTime): TDateTime; overload;
function GetLastDayOfMonth (const Month, Year: Word): TDateTime; overload;
function GetLastDayOfMonth (const Month, Year: Integer): TDateTime; overload;

{: Returns First Day of the Month, for a given Date/Time - Time portion preserved.
 Alternatively for a given Month Year.
 @param DT Date/Time to process.
 @param Month Month in given year, 1 = Jan, 12 = Dec.
 @param Year 4-digit Year, such as 1999.
 @cat DTMath
 @cat MonthMath
}
function GetFirstDayOfMonth (const DT: TDateTime): TDateTime; overload;
function GetFirstDayOfMonth (const Month, Year: Word): TDateTime; overload;
function GetFirstDayOfMonth (const Month, Year: Integer): TDateTime; overload;

{: Returns the Given Occurrence (Day of Month) of a Day of Week in a given
 Month/Year. Thus can be used to find the first Wednesday, Last Monday,
 etc. DOMType can be one of the following:<p>
  domFirst - First occurrence in a Month.<p>
  domSecond - Second occurrence in a Month.<p>
  domThird - Third occurrence in a Month.<p>
  domFourth - Fourth occurrence in a Month.<p>
  domLast - Last occurrence in a Month.

 @param DOMType the desired Day of Month Type.
 @param DOW the Day of Week, 1 = Sunday, 7 = Saturady.
 @param Month the month of the year, 1 = Jan, 12 = Dec.
 @param Year 4-digit year such as 1999.
 @returns the Date uniquely defined by the above.
 @cat DTMath
 @cat MonthMath
}
function DayOfMonth2Date (const DOMType: TESBDOMType; const DOW: Byte;
     const Month, Year: Word): TDateTime; overload;
function DayOfMonth2Date (const DOMType: TESBDOMType;
     const DOW, Month, Year: Integer): TDateTime; overload;

{: Returns the Start of the week containing given Date/Time, assumes that
 the Start of the Week is Sunday - Time portion preserved.
 @param DT Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function StartOfWeek (const DT: TDateTime): TDateTime;

{: Returns the End of the week containing given Date/Time, assumes that
 the End of the Week is Saturday - Time portion preserved.
 @param DT Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function EndOfWeek (const DT: TDateTime): TDateTime;

{: Returns true if both DateTimes refer to the same Calendar Month,
 can have different years.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat MonthMath
 @cat DTComparison
}
function DatesInSameMonth (const DT1, DT2: TDateTime): Boolean;

{: Returns true if both DateTimes refer to the same Year.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat YearMath
 @cat DTComparison
}
function DatesInSameYear (const DT1, DT2: TDateTime): Boolean;

{: Returns true if both DateTimes refer to the exact same Month,
 cannot have different years.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat MonthMath
 @cat DTComparison
}
function DatesInSameMonthYear (const DT1, DT2: TDateTime): Boolean;

{: Returns the Number of Days between DT2 and DT1. If result is 0 then
 they are the same Date, if result is negative then DT2 occurs before
 DT1. Today and yesterday are 1 day apart.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
}
function DaysApart (const DT1, DT2: TDateTime): LongInt;

{: Returns the Exact Number of Weeks between DT2 and DT1. If result is 0 then
 they are in the same Day, if result is negative then DT2 occurs before
 DT1.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function ExactWeeksApart (const DT1, DT2: TDateTime): Extended;

{: Returns the Number of Weeks between DT2 and DT1. If result is 0 then
 they are in within a Week of each other, if result is negative then DT2
 occurs before DT1.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function WeeksApart (const DT1, DT2: TDateTime): LongInt;

{: Returns the Number of Calendar Weeks between DT2 and DT1. A Calendar Week is
 taken as starting with Sunday. If result is 0 then they are in within the
 same Calendar Week, if result is negative then DT2 occurs in a Calendar
 Week before DT1.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function CalendarWeeksApart (const DT1, DT2: TDateTime): LongInt;

{: Returns the Number of Calendar Months between DT2 and DT1. If result is 0
 then they are in within the same Calendar Month, if result is negative
 then DT2	occurs in a Calendar Month before DT1.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function CalendarMonthsApart (const DT1, DT2: TDateTime): LongInt;

{: Returns the number of days in the Month represented by the given Date.
 Alternatively for a given Month Year.
 @param DT Date/Time to process.
 @param Month Month in given year, 1 = Jan, 12 = Dec.
 @param Year 4-digit Year, such as 1999.
 @cat DTMath
 @cat MonthMath
}
function DaysInMonth (const DT: TDateTime): Byte; overload;
function DaysInMonth (const Month, Year: Word): Byte; overload;
function DaysInMonth (const Month, Year: Integer): Byte; overload;

{: Returns the number of days in the current Month.
 @cat DTMath
 @cat MonthMath
}
function DaysInThisMonth: Byte;

{: Returns the Number of days left in the Month represented by the given Date.
 @param DT Date/Time to process.
 @cat DTMath
 @cat MonthMath
}
function DaysLeftInMonth (const DT: TDateTime): Byte; overload;

{: Returns the number of days left in the current Month.
 @param DT Date/Time to process.
 @cat DTMath
 @cat MonthMath
}
function DaysLeftInThisMonth: Byte;

{: Returns the number of days in the Year represented by the given Date.
 @param DT Date/Time to process.
 @cat DTMath
 @cat YearMath
}
function DaysInYear (const DT: TDateTime): Integer; overload;
function DaysInYear (const Year: Word): Integer; overload;
function DaysInYear (const Year: Integer): Integer; overload;

{: Returns the Day Number in the Year represented by the given Date.
 For a faster Integer Routine see <See Routine=OptDayOfYear>.
 @param DT Date/Time to process.
 @cat DTMath
 @cat YearMath
}
function DayOfYear (const DT: TDateTime): Word;

{: Returns the Day Number in the Year represented by the given Date.
 @param DT Date/Time to process.
 @cat DTMath
 @cat YearMath
}
function OptDayOfYear (const DT: TDateTime): Integer;

{: Returns the Day Number in this Year represented by today.
 For a faster Integer Routine see <See Routine=OptThisDayOfYear>.
 @cat DTMath
 @cat YearMath
}
function ThisDayOfYear: Word;

{: Returns the Day Number in this Year represented by today.
 @cat DTMath
 @cat YearMath
}
function OptThisDayOfYear: Integer;

{: Returns the number of days left in the Year represented by the given Date.
 For a faster Integer Routine see <See Routine=OptDaysLeftInYear>.
 @param DT Date/Time to process.
 @cat DTMath
 @cat YearMath
}
function DaysLeftInYear (const DT: TDateTime): Word;

{: Returns the number of days left in the Year represented by the given Date.
 @param DT Date/Time to process.
 @cat DTMath
 @cat YearMath
}
function OptDaysLeftInYear (const DT: TDateTime): Integer;

{: Returns the number of days left in this Year.
 For a faster Integer Routine see <See Routine=OptDaysLeftInThisYear>.
 @cat DTMath
 @cat YearMath
}
function DaysLeftInThisYear: Word;

{: Returns the number of days left in this Year.
 @cat DTMath
 @cat YearMath
}
function OptDaysLeftInThisYear: Integer;

{: Returns 1 if Date in Jan through Jun or 2 if Date in Jul through Dec.
 @param DT Date/Time to process.
 @cat DTMath
}
function WhichSemester (const DT: TDateTime): Byte;

{: Returns 1 if Date in Jan through Mar, 2 if Date in Apr through Jun,
 3 if Date in Jul through Sep, 4 if Date in Oct through Dec.
 @param DT Date/Time to process.
 @cat DTMath
}
function WhichQuarter (const DT: TDateTime): Byte;

{: Returns First Day of the Quarter, for a given Date/Time - Time portion,
 preserved or Returns First Day of the Quarter, for a given Quarter and Year.
 @param DT Date/Time to process.
 @param Qtr Quarter No, 1 through 4.
 @param Year 4-digit Year such as 1999.
 @cat DTMath
}
function GetFirstDayOfQuarter (const DT: TDateTime): TDateTime; overload;
function GetFirstDayofQuarter (const Qtr: Byte; const Year: Word): TDateTime; overload;
function GetFirstDayofQuarter (const Qtr, Year: Integer): TDateTime; overload;

{: Returns Last Day of the Quarter, for a given Date/Time - Time portion
 preserved or Returns Last Day of the Quarter, for a given Quarter and Year.
 @param DT Date/Time to process.
 @param Qtr Quarter No, 1 through 4.
 @param Year 4-digit Year such as 1999.
 @cat DTMath
}
function GetLastDayOfQuarter (const DT: TDateTime): TDateTime; overload;
function GetLastDayofQuarter (const Qtr: Byte; const Year: Word): TDateTime; overload;
function GetLastDayofQuarter (const Qtr, Year: Integer): TDateTime; overload;

{: Returns the Age (in years) of a "person" given their Date of Birth (DOB)
 and the Date of Reference (DT). If DT occurs before DB then -1 is
 returned.
 @param DOB Date of Birth.
 @param DT Date in question.
 @returns Age in Integral Years at the Date in question.
 @cat DTMath
 @cat YearMath
}
function AgeAtDate (const DOB, DT: TDateTime): Integer;

{: Returns the current Age (in years) of a "person" given their Date of Birth
 (DOB) using the System Date. If DOB occurs after the System Date then -1
 is returned.
 @param DOB Date of Birth.
 @returns Age in Integral Years at the current Date.
 @cat DTMath
}
function AgeNow (const DOB: TDateTime): Integer;

{: Returns the Age (in months) of a "person" given their Date of Birth (DOB)
 and the Date of Reference (DT). If DT occurs before DB then -1 is returned.
 Routine donated by David Gobbett.
 @param DOB Date of Birth.
 @param DT Date in question.
 @returns Age in Integral Months at the Date in question.
 @cat DTMath
 @cat MonthMath
 @cat YearMath
}
function AgeAtDateInMonths (const DOB, DT: TDateTime): Integer;

{: Returns the current Age (in months) of a "person" given their Date of Birth
 (DOB) using the System Date. If DOB occurs after the System Date then -1
 is returned.
 Routine donated by David Gobbett.
 @param DOB Date of Birth.
 @returns Age in Integral Months at the current Date.
 @cat DTMath
 @cat MonthMath
}
function AgeNowInMonths (const DOB: TDateTime): Integer;

{: Returns the Age (in weeks) of a "person" given their Date of Birth (DOB)
 and the Date of Reference (DT). If DT occurs before DB then -1 is returned.
 Routine donated by David Gobbett.
 @param DOB Date of Birth.
 @param DT Date in question.
 @returns Age in Integral Weeks at the Date in question.
 @cat DTMath
 @cat WeekMath
}
function AgeAtDateInWeeks (const DOB, DT: TDateTime): Integer;

{: Returns the current Age (in weeks) of a "person" given their Date of Birth (DOB)
 using the System Date. If DOB occurs after the System Date then -1 is returned.
 Routine donated by David Gobbett.
 @param DOB Date of Birth.
 @returns Age in Integral Weeks at the current Date.
 @cat DTMath
 @cat WeekMath
}
function AgeNowInWeeks (const DOB: TDateTime): Integer;

{--- Week No Routines ---}

{: Converts Dates into a Week No in the Current Year. Weeks are assumed to
 start with Sunday. The week that Jan 1 occurs is the 1st week of the year,
 the Sunday AFTER Jan 1 would be the start of the 2nd week of the year.
 Note that this does mean that there can be 54 weeks in a year!
 @param DT Date/Time to be processed.
 @cat DTMath
 @cat WeekMath
}
function Date2WeekNo (const DT: TDateTime): Integer;

{: Returns true if the two Dates are in the same WeekNo. Will return false
 if Dates from Different Years.
 @param DT1 First Date/Time to be processed.
 @param DT2 Second Date/Time to be processed.
 @cat DTMath
 @cat WeekMath
 @cat DTComparison
}
function DatesInSameWeekNo (const DT1, DT2: TDateTime): Boolean;

{:Returns true No of Weeks, based on WeekNo, that the two dates are
 apart. A Negative result implies DT2 occurs before D1. Will return -999
 if Dates from Different Years.
 @param DT1 First Date/Time to be processed.
 @param DT2 Second Date/Time to be processed.
 @cat DTMath
 @cat WeekMath
}
function WeekNosApart (const DT1, DT2: TDateTime): Integer;

{: Returns the WeekNo of the current Date (System Date).
 @cat DTMath
 @cat WeekMath
}
function ThisWeekNo: Integer;

{: Returns the Start of the Week for the given WeekNo in the given year,
 assuming that the Start of the Week is Sunday.
 @param WeekNo Week Number in given year.
 @param Year 4 digit year such as 1999.
 @cat DTMath
 @cat WeekMath
}
function StartOfWeekNo (const WeekNo, Year: Word): TDateTime; overload;
function StartOfWeekNo (const WeekNo, Year: Integer): TDateTime; overload;

{: Returns the End of the Week for the given WeekNo in the given year,
 assuming that the End of the Week is Saturday.
 @param WeekNo Week Number in given year.
 @param Year 4 digit year such as 1999.
 @cat DTMath
 @cat WeekMath
}
function EndOfWeekNo (const WeekNo, Year: Word): TDateTime; overload;
function EndOfWeekNo (const WeekNo, Year: Integer): TDateTime; overload;

{: Returns the Date for a given Day of Week, a given WeekNo, and the given Year,
 assuming that the Start of the Week is Sunday, and that DOW uses 1 for Sunday.
 @param DOW Day of Week, 1 = Sunday, 7 = Saturday.
 @param WeekNo Week Number in given year.
 @param Year 4 digit year such as 1999.
 @cat DTMath
 @cat WeekMath
}
function DWY2Date (const DOW, WeekNo, Year: Word): TDateTime; overload;
function DWY2Date (const DOW, WeekNo, Year: Integer): TDateTime; overload;

{ --- ISO-8601 Compliant Week Routines --- }

{: Returns Day Of Week According to ISO-8601 which has Monday as 1 and
 Sunday as 7.
 @param DT Date/Time to be processed.
 @cat DTMath
 @cat WeekMath
}
function ISODayOfWeek (const DT: TDateTime): Byte;

{: Returns the current Day of the week from Today's Date, according to ISO-8601
 which has Monday as 1 and Sunday as 7.
 @returns the DOW Number, 1 through 7 where 1 is Monday.
 @cat DTConv
}
function ThisISODOW: Byte;

{: Converts an ISO-8601 Day of Week into a Delphi Day Of Week.
 @param ISODOW Day of Week where Monday is 1 through Sunday is 7.
 @returns Day of Week where Sunday is 1 through Saturday is 7.
 @cat DTMath
 @cat WeekMath
}
function ISODOW2DOW (const ISODOW: Byte): Byte;

{: Converts a Delphi Day of Week into an ISO-8601 Day Of Week.
 @param DOW Day of Week where Sunday is 1 through Saturday is 7.
 @returns Day of Week where Monday is 1 through Sunday is 7.
 @cat DTMath
 @cat WeekMath
}
function DOW2ISODOW (const DOW: Byte): Byte;

{: Most years have 52 weeks, but years that start on a Thursday and leap
 years that start on a Wednesday (or Thursday) have 53 weeks. Based on
 code supplied by Niklas Astram.
 @param Year 4-digit year such as 1999.
 @param DT Date to take year from.
 @cat DTMath
 @cat WeekMath
 @cat YearMath
}
function ISOWeeksInYear (const Year: Word): Integer; overload;
function ISOWeeksInYear (const Year: Integer): Integer; overload;
function ISOWeeksInYear (const DT: TDateTime): Integer; overload;

{: Returns First Monday of the Year, for a given Year. Used in
 ISO-8601 Week No Routines.
 @param Year 4-digit year such as 1999.
 @param DT Date to take year from.
 @cat DTMath
 @cat WeekMath
 @cat YearMath
}
function GetFirstMondayOfYear (const Year: Word): TDateTime; overload;
function GetFirstMondayOfYear (const Year: Integer): TDateTime; overload;
function GetFirstMondayOfYear (const DT: TDateTime): TDateTime; overload;

{: Returns the Start of the week containing given Date/Time, assumes that
 the Start of the Week is Monday according to ISO-8601
 - Time portion preserved.
 @param DT Date to process.
 @cat DTMath
 @cat WeekMath
}
function StartOfISOWeek (const DT: TDateTime): TDateTime;

{: Returns the End of the week containing given Date/Time, assumes that
 the End of the Week is Sunday according to ISO-8601
 - Time portion preserved.
 @param DT Date to process.
 @cat DTMath
 @cat WeekMath
}
function EndOfISOWeek (const DT: TDateTime): TDateTime;

{: Converts Dates into a Week No and Year according to ISO-8601. Weeks are
 assumed to start with Monday. The week that Jan 4 occurs in is the 1st week of
 the year. Note that this does mean that there can be 53 weeks in a year!
 @param DT Date to process.
 @param WeekNo Returns the ISO-8601 Week Number in specified Year
 @param Year Returns 4 digit year to which the Week Number applies, not
  necessarily the same year as DT.
 @cat DTMath
 @cat WeekMath
}
procedure Date2ISOWeekNo (const DT: TDateTime; var WeekNo: Integer;
     var Year: Word); overload;
procedure Date2ISOWeekNo (const DT: TDateTime; var WeekNo, Year: Integer); overload;

{: Returns Date as a Basic Format for ISO Calendar Week/Day: YYYYWwwD where
 YYYY is year, 'W' is literal, ww is weekno and D is the ISO Day of Week -
 Monday is First Day.
 @param DT Date to take year from.
 @cat WeekMath
 @cat DTConv
}
function Date2ISOWeekStr (const DT: TDateTime): string;

{: Returns Date as a Enhanced Format for ISO Calendar Week/Day: YYYY-Www-D where
 YYYY is year, 'W' is literal, ww is weekno and D is the ISO Day of Week -
 Monday is First Day.
 @param DT Date to process.
 @cat WeekMath
 @cat DTConv
}
function Date2ISOWeekEnhStr (const DT: TDateTime): string;

{: Returns Date as a Basic Format for ISO Calendar Week: YYYYWww where
 YYYY is year, 'W' is literal, ww is weekno.
 @param DT Date to process.
 @cat WeekMath
 @cat DTConv
}
function Date2ISOWeekOnlyStr (const DT: TDateTime): string;

{: Returns Date as a Enhanced Format for ISO Calendar Week: YYYY-Www where
 YYYY is year, 'W' is literal, ww is weekno.
 @param DT Date to process.
 @cat WeekMath
 @cat DTConv
}
function Date2ISOWeekOnlyEnhStr (const DT: TDateTime): string;

{: Returns Date as a Basic Format for ISO Dates: YYYYMMDD where
 YYYY is year, MM is the Month, DD is Day of Month. Zero Padded.
 @param DT Date to process.
 @cat DTConv
}
function Date2ISOStr (const DT: TDateTime): string;

{: Returns Date as a Basic Format for ISO Dates: YYYY-MM-DD where
 YYYY is year, MM is the Month, DD is Day of Month. Zero Padded.
 @param DT Date to process.
 @cat DTConv
}
function Date2ISOEnhStr (const DT: TDateTime): string;

{: Returns Date as a Basic Format for ISO Dates: YYYYMMDD where
 YYYY is year, MM is the Month, DD is Day of Month. Zero Padded.
 @param DT Date to process.
 @cat DTConv
}
function Date2ISOInt (const DT: TDateTime): LongWord;

{Returns true if the two Dates are in the same ISO-8601 WeekNo.
 @param DT1 First Date to process.
 @param DT2 Second Date to process.
 @cat DTMath
 @cat WeekMath
 @cat DTComparison
}
function DatesInSameISOWeekNo (const DT1, DT2: TDateTime): Boolean;

{Returns true No of Weeks, based on ISOWeekNo, that the two dates are
 apart. A Negative result implies DT2 occurs before D1.
 @param DT1 First Date to process.
 @param DT2 Second Date to process.
 @cat DTMath
 @cat WeekMath
}
function ISOWeekNosApart (DT1, DT2: TDateTime): Int64;

{: Returns the ISO-8601 WeekNo of the current Date (System Date).
 @param WeekNo Returns the ISO-8601 Week Number in specified Year
 @param Year Returns 4 digit year to which the Week Number applies.
 @cat DTMath
 @cat WeekMath
}
procedure ThisISOWeekNo (var WeekNo: Integer; var Year: Word); overload;
procedure ThisISOWeekNo (var WeekNo, Year: Integer); overload;

{: Returns the Start of the Week for the given ISO-8601 WeekNo in the given
 year. Note that the Start of the Week is Monday in ISO-8601.
 @param WeekNo Returns the ISO-8601 Week Number in specified Year
 @param Year Returns 4 digit year to which the Week Number applies.
 @cat DTMath
 @cat WeekMath
}
function StartOfISOWeekNo (const WeekNo: Integer; const Year: Word): TDateTime; overload;
function StartOfISOWeekNo (const WeekNo, Year: Integer): TDateTime; overload;

{: Returns the End of the Week for the given ISO-8601 WeekNo in the given
 year. Note that the End of the Week is Sunday in ISO-8601.
 @param WeekNo Returns the ISO-8601 Week Number in specified Year.
 @param Year Returns 4 digit year to which the Week Number applies.
 @cat DTMath
 @cat WeekMath
}
function EndOfISOWeekNo (const WeekNo: Integer; const Year: Word): TDateTime; overload;
function EndOfISOWeekNo (const WeekNo, Year: Integer): TDateTime; overload;

{: Returns the Date for a given Day of Week, a given WeekNo, and the given Year,
 as defined in ISO-8601. Note that the Start of the Week is Monday, and
 that DOW uses 1 for Monday.
 @param Year 4 digit year to which the Week Number applies.
 @param WeekNo the ISO-8601 Week Number in specified Year.
 @param DOW Day of Week, 1 for Monday through 7 for Sunday.
 @cat DTMath
 @cat WeekMath
}
function ISOYWD2Date (const Year: Word; const WeekNo, DOW: Integer): TDateTime; overload;
function ISOYWD2Date (const Year, WeekNo, DOW: Integer): TDateTime; overload;

{: Returns the Number of ISO Weeks between DT2 and DT1. An ISO Week is
 taken as starting with Monday. If result is 0 then they are in within the
 same ISO Week, if result is negative then DT2 occurs in an ISO Week before
 DT1.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTMath
 @cat WeekMath
}
function ISOWeeksApart (const DT1, DT2: TDateTime): LongInt;

{--- Boolean Identification ---}

{: Returns True if they are both the same Date ignoring the Time portion.
 @param DT1 First Date to process.
 @param DT2 Second Date to process.
 @cat DTComparison
}
function SameDate (const DT1, DT2: TDateTime): Boolean;

{: Returns True if they are both the same Time ignoring the Date portion.
 Times are considered the same if they are less then 1 millisecond apart.
 @param DT1 First Time to process.
 @param DT2 Second Time to process.
 @cat DTComparison
}
function SameTime (const DT1, DT2: TDateTime): Boolean;

{: Returns True if they are both the same Date/Time. Date/Times are considered
 the same if they are less then 1 millisecond apart.
 @param DT1 First Date/Time to process.
 @param DT2 Second Date/Time to process.
 @cat DTComparison
}
function SameDateTime (const DT1, DT2: TDateTime): Boolean;

{: Returns True if the given Date's Month is January.
 @param DT Date to process.
 @cat DTComparison
}
function IsJanuary (const DT: TDateTime): Boolean;

{: Returns True if today's Month is January.
 @cat DTComparison
}
function IsJanuaryNow: Boolean;

{: Returns True if the given Date's Month is February.
 @param DT Date to process.
 @cat DTComparison
}
function IsFebruary (const DT: TDateTime): Boolean;

{: Returns True if today's Month is February.
 @cat DTComparison
}
function IsFebruaryNow: Boolean;

{: Returns True if the given Date's Month is March.
 @param DT Date to process.
 @cat DTComparison
}
function IsMarch (const DT: TDateTime): Boolean;

{: Returns True if today's Month is March.
 @cat DTComparison
}
function IsMarchNow: Boolean;

{: Returns True if the given Date's Month is April.
 @param DT Date to process.
 @cat DTComparison
}
function IsApril (const DT: TDateTime): Boolean;

{: Returns True if today's Month is April.
 @cat DTComparison
}
function IsAprilNow: Boolean;

{: Returns True if the given Date's Month is May.
 @param DT Date to process.
 @cat DTComparison
}
function IsMay (const DT: TDateTime): Boolean;

{: Returns True if today's Month is May.
 @cat DTComparison
}
function IsMayNow: Boolean;

{: Returns True if the given Date's Month is June.
 @param DT Date to process.
 @cat DTComparison
}
function IsJune (const DT: TDateTime): Boolean;

{: Returns True if today's Month is June.
 @cat DTComparison
}
function IsJuneNow: Boolean;

{: Returns True if the given Date's Month is July.
 @param DT Date to process.
 @cat DTComparison
}
function IsJuly (const DT: TDateTime): Boolean;

{: Returns True if today's Month is July.
 @cat DTComparison
}
function IsJulyNow: Boolean;

{: Returns True if the given Date's Month is August.
 @param DT Date to process.
 @cat DTComparison
}
function IsAugust (const DT: TDateTime): Boolean;

{: Returns True if today's Month is August.
 @cat DTComparison
}
function IsAugustNow: Boolean;

{: Returns True if the given Date's Month is September.
 @param DT Date to process.
 @cat DTComparison
}
function IsSeptember (const DT: TDateTime): Boolean;

{: Returns True if today's Month is September.
 @cat DTComparison
}
function IsSeptemberNow: Boolean;

{: Returns True if the given Date's Month is October.
 @param DT Date to process.
 @cat DTComparison
}
function IsOctober (const DT: TDateTime): Boolean;

{: Returns True if today's Month is October.
 @cat DTComparison
}
function IsOctoberNow: Boolean;

{: Returns True if the given Date's Month is November.
 @param DT Date to process.
 @cat DTComparison
}
function IsNovember (const DT: TDateTime): Boolean;

{: Returns True if today's Month is November.
 @cat DTComparison
}
function IsNovemberNow: Boolean;

{: Returns True if the given Date's Month is December.
 @param DT Date to process.
 @cat DTComparison
}
function IsDecember (const DT: TDateTime): Boolean;

{: Returns True if todays Month is December.
 @cat DTComparison
}
function IsDecemberNow: Boolean;

{: Returns True if the Time portion is a AM value.
 @param DT Time to process.
 @cat DTComparison
}
function IsAM (const DT: TDateTime): Boolean;

{: Returns True if the currnet Time is a AM value.
 @cat DTComparison
}
function IsAMNow: Boolean;

{: Returns True if the Time portion is a PM value.
 @param DT Time to process.
 @cat DTComparison
}
function IsPM (const DT: TDateTime): Boolean;

{: Returns True if the current Time is a PM value.
 @cat DTComparison
}
function IsPMNow: Boolean;

{: Returns True if the Time portion represents Noon, 12:00pm.
 @param DT Time to process.
 @cat DTComparison
}
function IsNoon (const DT: TDateTime): Boolean;

{: Returns True if the current Time represents Noon, 12:00pm.
 @cat DTComparison
}
function IsNoonNow: Boolean;

{: Returns True if the Time portion represents Midnight, 12:00am.
 @param DT Time to process.
 @cat DTComparison
}
function IsMidnight (const DT: TDateTime): Boolean;

{: Returns True if the current Time represents Midnight, 12:00am.
 @cat DTComparison
}
function IsMidnightNow: Boolean;

{: Returns True if the Date represents a Sunday.
 @param DT Time to process.
 @cat DTComparison
}
function IsSunday (const DT: TDateTime): Boolean;

{: Returns True if today is a Sunday.
 @cat DTComparison
}
function IsSundayNow: Boolean;

{: Returns True if the Date represents a Monday.
 @param DT Date to process.
 @cat DTComparison
}
function IsMonday (const DT: TDateTime): Boolean;

{: Returns True if today is a Monday.
 @cat DTComparison
}
function IsMondayNow: Boolean;

{: Returns True if the Date represents a Tuesday.
 @param DT Date to process.
 @cat DTComparison
}
function IsTuesday (const DT: TDateTime): Boolean;

{: Returns True if today is a Tuesday.
 @cat DTComparison
}
function IsTuesdayNow: Boolean;

{: Returns True if the Date represents a Wednesday.
 @param DT Date to process.
 @cat DTComparison
}
function IsWednesday (const DT: TDateTime): Boolean;

{: Returns True if today is a Wednesday.
 @cat DTComparison
}
function IsWednesdayNow: Boolean;

{: Returns True if the Date represents a Thursday.
 @param DT Date to process.
 @cat DTComparison
}
function IsThursday (const DT: TDateTime): Boolean;

{: Returns True if today is a Thursday.
 @cat DTComparison
}
function IsThursdayNow: Boolean;

{: Returns True if the Date represents a Friday.
 @param DT Date to process.
 @cat DTComparison
}
function IsFriday (const DT: TDateTime): Boolean;

{: Returns True if today is a Friday.
 @cat DTComparison
}
function IsFridayNow: Boolean;

{: Returns True if the Date represents a Saturday.
 @param DT Date to process.
 @cat DTComparison
}
function IsSaturday (const DT: TDateTime): Boolean;

{: Returns True if today is a Saturday.
 @cat DTComparison
}
function IsSaturdayNow: Boolean;

{: Returns True if the Date represents Saturday or Sunday.
 @param DT Date to process.
 @cat DTComparison
}
function IsWeekend (const DT: TDateTime): Boolean;

{: Returns True if today is Saturday or Sunday.
 @cat DTComparison
}
function IsWeekendNow: Boolean;

{: Returns True if the Date represents Monday through Friday.
 @param DT Date to process.
 @cat DTComparison
}
function IsWeekday (const DT: TDateTime): Boolean;

{: Returns True if today is Monday through Friday.
 @cat DTComparison
}
function IsWeekdayNow: Boolean;

{: Returns True if Month can be found in the Short Months from the
 Regional Settings.
 @param Month String containing Month Name
 @cat DTComparison
 @cat MonthMath
}
function IsValidShortMonth (const Month: string): Boolean;

{: Returns True if Month can be found in the Long Months from the
 Regional Settings.
 @param Month String containing Month Name
 @cat DTComparison
 @cat MonthMath
}
function IsValidLongMonth (const Month: string): Boolean;

{: Returns True if DOW can be found in the Short Day Names from the
 Regional Settings.
 @param DOW String containing Day of Week Name
 @cat DTComparison
}
function IsValidShortDOW (const DOW: string): Boolean;

{: Returns True if DOW can be found in the Long Day Names from the
 Regional Settings.
 @param DOW String containing Day of Week Name
 @cat DTComparison
}
function IsValidLongDOW (const DOW: string): Boolean;

{: Returns true if the date is the first day of a Month.
 @param DT Date to process.
 @cat DTComparison
 @cat MonthMath
}
function IsFirstDayOfMonth (const DT: TDateTime): Boolean;

{: Returns true if today is the first day of a Month.
 @cat DTComparison
 @cat MonthMath
}
function IsFirstDayOfMonthNow: Boolean;

{: Returns true if the date is the last day of a Month.
 @param DT Date to process.
 @cat DTComparison
 @cat MonthMath
}
function IsLastDayOfMonth (const DT: TDateTime): Boolean;

{: Returns true if today is the last day of a Month.
 @cat DTComparison
 @cat MonthMath
}
function IsLastDayOfMonthNow: Boolean;

{: Returns true if the date is the first day of a Year.
 @param DT Date to process.
 @cat DTComparison
 @cat YearMath
}
function IsFirstDayOfYear (const DT: TDateTime): Boolean;

{: Returns true if today is the first day of a Year.
 @cat DTComparison
 @cat YearMath
}
function IsFirstDayOfYearNow: Boolean;

{: Returns true if the date is the last day of a Year.
 @param DT Date to process.
 @cat DTComparison
 @cat YearMath
}
function IsLastDayOfYear (const DT: TDateTime): Boolean;

{: Returns true if today is the last day of a Year.
 @cat DTComparison
 @cat YearMath
}
function IsLastDayOfYearNow: Boolean;

{--- Holiday/Working Day Routines ---}

{: Returns True if the specified Date is a Working Day.
 It is not a Working Day if the Day of Week lies in <See Var=NonWorkingDays>
 and if <See Var=ESBUseChristianHolidays> is
 set to True and the Date is Easter Sunday, Good Friday, Christmas Day.
 Easter Saturday & Easter Monday can also be made non-working by
 setting <See Var=ESBUseEasterSaturday> & <See Var=ESBUseEasterMonday>
 to true.
 @param DT Date to process.
 @cat DTComparison
 @cat DTMath
}
function IsWorkingDay (const DT: TDateTime): Boolean;

{: Returns the number of the specified Day of Week in the Given Month/Year.
 Exception results for invalid DOW or invalid Month.
 @param DOW Day of the Week, Sunday = 1 through Saturday = 7
 @param Month Desired Month in the range 1 through 12
 @param Year Desired Year
 @cat DTMath
}
function DOWsInMonth (const DOW: Byte; const Month, Year: Word): Integer;

{: Returns the number of the WeekEnd Days (Sat & Sun) in the Given Month/Year.
 @param Month Desired Month in the range 1 through 12
 @param Year Desired Year
 @cat DTMath
}
function WeekendDaysInMonth (const Month, Year: Word): Integer;

{: Returns the number of the Week Days (Mon through Fri) in the Given Month/Year.
 Exception results for invalid Month.
 @param Month Desired Month in the range 1 through 12
 @param Year Desired Year
 @cat DTMath
}
function WeekDaysInMonth (const Month, Year: Word): Integer;

{: Returns the number of the specified Day of Week in the Given Date Range (inclusive).
 If EndDT occurs before StartDT then they are swapped.
 Thanks to Tom Grieve for his assistance with this routine.
 Exception results for invalid DOW or invalid Month.
 @param DOW Day of the Week, Sunday = 1 through Saturday = 7. Overloaded version also allows for TESBDayOfWeek type
 @param StartDT The beginning Date
 @param EndDT The end Date
 @cat DTMath
}
function DOWsInRange (const DOW: Byte; const StartDT, EndDT: TDateTime): Integer; overload;
function DOWsInRange (const DOW: TESBDayofWeek; const StartDT, EndDT: TDateTime): Integer; overload;

{--- Conversions for use with Databases ---}

{: Returns Date as a string suitable for MS Access Date comparisons,
 ie #MM/DD/YYYY#.
 @param DT Date to process.
 @cat DTConv
}
function Date2AccessStr (const DT: TDateTime): string;

(*: Returns Date as a string suitable for ANSI Standard comparisons,
 ie  { d 'YYYY-MM-DD' }
 @param DT Date to process.
 @cat DTConv
*)
function Date2ANSISQLStr (const DT: TDateTime): string;

{--- TESBMonth and TESBDayOfWeek Conversions ---}

{ Converts a Day of week in the form of 1 = Sunday through 7 = Saturday
 into a TESBDayOfWeek.
 @cat DTConv
}
function DOW2ESBDayOfWeek (const DOW: Byte): TESBDayOfWeek;

{ Converts a TESBDayOfWeek into a Day of week in the form of 1 = Sunday through 7 = Saturday.
 @cat DTConv
}
function ESBDayOfWeek2DOW (const DOW: TESBDayOfWeek): Integer;

{ Converts an ISO-8601 Compliant Day of week in the form of 1 = Monday through 7 = Sunday
 into a TESBDayOfWeek.
 @cat DTConv
}
function ISODOW2ESBDayOfWeek (const DOW: Byte): TESBDayOfWeek;

{ Converts a TESBDayOfWeek into an ISO-8601 Compliant Day of week in the form of
 1 = Monday through 7 = Sunday.
 @cat DTConv
}
function ESBDayOfWeek2ISODOW (const DOW: TESBDayOfWeek): Integer;

{ Converts a Day of week in the form of 1 = January through 12 = December
 into a TESBMonth.
 @cat DTConv
}
function Month2ESBMonth (const Month: Word): TESBMonth; overload;
function Month2ESBMonth (const Month: Integer): TESBMonth; overload;

{ Converts a TESBMonth into a Month in the form of 1 = January through 12 = December.
 @cat DTConv
}
function ESBMonth2Month (const Month: TESBMonth): Integer;

{--- Astrological Routines ---}

{: Returns the Star Sign (Astrology) for the Given Date. Values obtained from
 ESBStarSigns.
 Thanks to Tom Grieve for this routine.
 @cat DTConv
}
function Date2StarSign (const DT: TDateTime): string;

{--- Sidereal Days ---}

{: Converts "normal" 24-hour Days into Sidereal Days. A Sidereal Day can be thought
of as the time taken for the stars to return to their same positions.
See <See Const=HrsPerSiderealDay>.
 @param Value Number of 24-hour Days
 @returns the Number of Sidereal Days
 @cat DTConv
}
function Days2SiderealDays (const Days: Extended): Extended;

{: Converts Sidereal Days into "normal" 24-hour Days. A Sidereal Day can be thought
of as the time taken for the stars to return to their same positions.
See <See Const=HrsPerSiderealDay>.
 @param Value Number of Sidereal Days
 @returns the Number of 24-hour Days
 @cat DTConv
}
function SiderealDays2Days (const Days: Extended): Extended;
implementation

uses
     {$IFDEF LINUX}
     Libc,
     {$ENDIF}
     SysUtils,
     QESBPCS_RS_Globals,
     QESBPCSConvert, QESBPCSSystem;

function OptDate: TDateTime;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := OptEncodeDateW (ST.wYear, ST.wMonth, ST.wDay);
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := OptEncodeDateI (UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday);
end;
{$ENDIF}

function ESBToday: TDateTime;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := OptEncodeDateW (ST.wYear, ST.wMonth, ST.wDay);
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := OptEncodeDateI (UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday);
end;
{$ENDIF}

function ESBYesterday: TDateTime;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := OptEncodeDateW (ST.wYear, ST.wMonth, ST.wDay) - 1;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := OptEncodeDateI (UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) - 1;
end;
{$ENDIF}

function ESBTomorrow: TDateTime;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := OptEncodeDateW (ST.wYear, ST.wMonth, ST.wDay) + 1;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := OptEncodeDateI (UT.tm_year + 1900, UT.tm_mon + 1, UT.tm_mday) + 1;
end;
{$ENDIF}

{--- Conversions ---}

procedure OptDecodeDateI (const DT: TDateTime; out Year, Month, Day: Integer);
var
     J: Integer;
begin
     J := pred ((Trunc (DT) + 693900) shl 2);
     Year := J div 146097;
     Day := (J - 146097 * Year) shr 2;
     J := (Day shl 2 + 3) div 1461;
     Day := (Day shl 2 + 7 - 1461 * J) shr 2;
     Month := (5 * Day - 3) div 153;
     Day := (5 * Day + 2 - 153 * Month) div 5;
     Year := 100 * Year + J;
     if Month < 10 then
          Inc (Month, 3)
     else
     begin
          Dec (Month, 9);
          Inc (Year);
     end;
end;

procedure OptDecodeDateW (const DT: TDateTime; out Year, Month, Day: Word);
var
     J: Integer;
begin
     J := pred ((Trunc (DT) + 693900) shl 2);
     Year := J div 146097;
     Day := (J - 146097 * Year) shr 2;
     J := (Day shl 2 + 3) div 1461;
     Day := (Day shl 2 + 7 - 1461 * J) shr 2;
     Month := (5 * Day - 3) div 153;
     Day := (5 * Day + 2 - 153 * Month) div 5;
     Year := 100 * Year + J;
     if Month < 10 then
          Inc (Month, 3)
     else
     begin
          Dec (Month, 9);
          Inc (Year);
     end;
end;

function OptEncodeDateI (Year, Month, Day: Integer): TDateTime;
var
     DayTable: PDayTable;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     DayTable := @MonthDays [IsLeapYear (Year)];
     if (Day <= DayTable^ [Month]) and (Year >= 1) and (Year < 10000) and
          (Month < 13) and (Day > 0) then
     begin
          if Month > 2 then
               Dec (Month, 3)
          else if (Month > 0) then
          begin
               Inc (Month, 9);
               Dec (Year);
          end
          else // Month <= 0
               raise EConvertError.Create (rsInvalidDate);

          Result := (146097 * (Year div 100)) shr 2 +
               (1461 * (Year - 100 * (Year div 100))) shr 2 +
               (153 * Month + 2) div 5 + Day - 693900;
     end
     else
          raise EConvertError.Create (rsInvalidDate + IntToStr (Year) + '-'
               + IntToStr (Month) + '-' + IntToStr (Day));
end;

function OptEncodeDateW (Year, Month, Day: Word): TDateTime;
var
     DayTable: PDayTable;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     DayTable := @MonthDays [IsLeapYear (Year)];
     if (Day <= DayTable^ [Month]) and (Year >= 1) and (Year < 10000) and
          (Month < 13) and (Day > 0) then
     begin
          if Month > 2 then
               Dec (Month, 3)
          else if (Month > 0) then
          begin
               Inc (Month, 9);
               Dec (Year);
          end
          else // Month <= 0
               raise EConvertError.Create (rsInvalidDate);

          Result := (146097 * (Year div 100)) shr 2 +
               (1461 * (Year - 100 * (Year div 100))) shr 2 +
               (153 * Month + 2) div 5 + Day - 693900;
     end
     else
          raise EConvertError.Create (rsInvalidDate);
end;

function OptDate2Year (const DT: TDateTime): Word;
var
     J: Integer;
     Day, Month, Year: Integer;
begin
     J := pred ((Trunc (DT) + 693900) shl 2);
     Year := J div 146097;
     Day := (J - 146097 * Year) shr 2;
     J := (Day shl 2 + 3) div 1461;
     Day := (Day shl 2 + 7 - 1461 * J) shr 2;
     Month := (5 * Day - 3) div 153;
     Year := 100 * Year + J;
     if Month >= 10 then
          Inc (Year);
     Result := Year
end;

function OptDate2Month (const DT: TDateTime): Word;
var
     J: Integer;
     Day, Month, Year: Integer;
begin
     J := pred ((Trunc (DT) + 693900) shl 2);
     Year := J div 146097;
     Day := (J - 146097 * Year) shr 2;
     J := (Day shl 2 + 3) div 1461;
     Day := (Day shl 2 + 7 - 1461 * J) shr 2;
     Month := (5 * Day - 3) div 153;
     if Month < 10 then
          Inc (Month, 3)
     else
          Dec (Month, 9);
     Result := Month
end;

function OptDate2Day (const DT: TDateTime): Word;
var
     J: Integer;
     Day, Month, Year: Integer;
begin
     J := pred ((Trunc (DT) + 693900) shl 2);
     Year := J div 146097;
     Day := (J - 146097 * Year) shr 2;
     J := (Day shl 2 + 3) div 1461;
     Day := (Day shl 2 + 7 - 1461 * J) shr 2;
     Month := (5 * Day - 3) div 153;
     Day := (5 * Day + 2 - 153 * Month) div 5;
     Result := Day
end;

function ESBEncodeDate (const Year, Month, Day: Word): TDateTime;
begin
     try
          Result := EncodeDate (Year, Month, Day)
     except
          raise EConvertError.Create (rsInvalidDate);
     end;
end;

procedure ESBDecodeDate (const Date: TDateTime; var Year, Month, Day: Word);
begin
     try
          DecodeDate (Date, Year, Month, Day)
     except
          raise EConvertError.Create (rsInvalidDateTime);
     end;
end;

function ESBEncodeTime (const Hour, Min, Sec, MSec: Word): TDateTime;
begin
     try
          Result := EncodeTime (Hour, Min, Sec, MSec)
     except
          raise EConvertError.Create (rsInvalidTime);
     end;
end;

procedure ESBDecodeTime (const Time: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
     try
          DecodeTime (Time, Hour, Min, Sec, MSec)
     except
          raise EConvertError.Create (rsInvalidDateTime);
     end;
end;

function Date2Str (const DT: TDateTime): string;
begin
     try
          if ESBBlankWhenZero and (abs (DT) < OneDTMilliSecond) then
               Result := ''
          else
               Result := DateToStr (DT);
     except
          Result := '';
          if ESBRaiseDateError then
               raise;
     end;
end;

function Date2DigitStr (const DT: TDateTime): string;
begin
     Result := Date2FormatStr (DT, 'YYYYMMDD');
end;

function Date2FormatStr (const DT: TDateTime; const Format: string): string;
begin
     try
          if ESBBlankWhenZero and (abs (DT) < OneDTMilliSecond) then
               Result := ''
          else
               DateTimeToString (Result, Format, DT);
     except
          Result := '';
          if ESBRaiseDateError then
               raise;
     end;
end;

function Time2Str (const DT: TDateTime): string;
var
     Hrs, Mins, Secs, MSecs: Word;
     Hold: Boolean;
begin
     try
          if ESBBlankWhenZero and (abs (DT) < OneDTMilliSecond) then
               Result := ''
          else
          begin
               ESBDecodeTime (DT, Hrs, Mins, Secs, MSecs);
               Hold := ESBBlankWhenZero;
               ESBBlankWhenZero := False;
               try
                    Result := Int2EStr (Hrs) + TimeSeparator
                         + Int2ZStr (Mins, 2);
               finally
                    ESBBlankWhenZero := Hold;
               end;
          end;
     except
          Result := '';
          if ESBRaiseDateError then
               raise;
     end;
end;

function Time2FormatStr (const DT: TDateTime; const Format: string): string;
begin
     try
          if ESBBlankWhenZero and (abs (DT) < 0.000001) then
               Result := ''
          else
               DateTimeToString (Result, Format, DT);
     except
          Result := '';
          if ESBRaiseDateError then
               raise;
     end;
end;

function DateTime2FormatStr (const DT: TDateTime; const Format: string): string;
begin
     try
          if ESBBlankWhenZero and (abs (DT) < 0.000001) then
               Result := ''
          else
               DateTimeToString (Result, Format, DT);
     except
          Result := '';
          if ESBRaiseDateError then
               raise;
     end;
end;

function Date2Year (const DT: TDateTime): Word;
var
     D, M: Word;
begin
     OptDecodeDateW (DT, Result, M, D);
end;

function Date2Month (const DT: TDateTime): Word;
var
     D, Y: Word;
begin
     OptDecodeDateW (DT, Y, Result, D);
end;

function Date2YearMonth (const DT: TDateTime): LongWord;
var
     D, M, Y: Word;
begin
     OptDecodeDateW (DT, Y, M, D);
     Result := Y * 100 + M;
end;

function Date2Day (const DT: TDateTime): Word;
var
     M, Y: Word;
begin
     OptDecodeDateW (DT, Y, M, Result);
end;

function Time2Hr (const DT: TDateTime): Word;
var
     Min, Sec, MSec: Word;
begin
     ESBDecodeTime (DT, Result, Min, Sec, MSec);
end;

function Time2Min (const DT: TDateTime): Word;
var
     Hr, Sec, MSec: Word;
begin
     ESBDecodeTime (DT, Hr, Result, Sec, MSec);
end;

function Time2Sec (const DT: TDateTime): Word;
var
     Hr, Min, MSec: Word;
begin
     ESBDecodeTime (DT, Hr, Min, Result, MSec);
end;

function Time2MSec (const DT: TDateTime): Word;
var
     Hr, Min, Sec: Word;
begin
     ESBDecodeTime (DT, Hr, Min, Sec, Result);
end;

function OptThisYear: Integer;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wYear;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_year + 1900;
end;
{$ENDIF}

function OptThisMonth: Integer;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wMonth;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_mon + 1;
end;
{$ENDIF}

function OptThisDay: Integer;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wDay;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_mday;
end;
{$ENDIF}

function ThisYear: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wYear;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_year + 1900;
end;
{$ENDIF}

function ThisMonth: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wMonth;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_mon + 1;
end;
{$ENDIF}

function ThisDay: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wDay;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     UT: TUnixTime;
begin
     __time (@T);
     localtime_r (@T, UT);
     Result := UT.tm_mday;
end;
{$ENDIF}

function ThisDOW: Byte;
begin
     Result := DayOfWeek (Date);
end;

function ThisHr: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wHour
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     TV: TTimeVal;
     UT: TUnixTime;
begin
     gettimeofday (TV, nil);
     T := TV.tv_sec;
     localtime_r (@T, UT);
     Result := UT.tm_hour;
end;
{$ENDIF}

function ThisMin: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wMinute;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     TV: TTimeVal;
     UT: TUnixTime;
begin
     gettimeofday (TV, nil);
     T := TV.tv_sec;
     localtime_r (@T, UT);
     Result := UT.tm_min;
end;
{$ENDIF}

function ThisSec: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wSecond;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     TV: TTimeVal;
     UT: TUnixTime;
begin
     gettimeofday (TV, nil);
     T := TV.tv_sec;
     localtime_r (@T, UT);
     Result := UT.tm_sec;
end;
{$ENDIF}

function ThisMSec: Word;
{$IFDEF MSWINDOWS}
var
     ST: TSystemTime;
begin
     Windows.GetLocalTime (ST);
     Result := ST.wMilliseconds;
end;
{$ENDIF}
{$IFDEF LINUX}
var
     T: TTime_T;
     TV: TTimeVal;
     UT: TUnixTime;
begin
     gettimeofday (TV, nil);
     T := TV.tv_sec;
     localtime_r (@T, UT);
     Result := TV.tv_usec div 1000;
end;
{$ENDIF}

function Str2Time (const TimeStr: string): TDateTime;
var
     S: string;
begin
     S := Trim (TimeStr);
     if S = '' then
     begin
          Result := 0.0;
          Exit;
     end;

     if IsDigitStr (S) then
     begin
          case Length (S) of
               4: S := LeftStr (S, 2) + TimeSeparator + RightStr (S, 2);
               6: S := LeftStr (S, 2) + TimeSeparator + Copy (S, 3, 2)
                    + TimeSeparator + Copy (S, 5, 2);
          end;
     end;

     try
          // Allow '.' and ':' as valid alternatives for TimeSeparator
          S := ReplaceChStr (S, '.', TimeSeparator);
          S := ReplaceChStr (S, ':', TimeSeparator);
          // S := ReplaceChStr ( S, ' ', TimeSeparator);

          // Remove trailing Separator if any
          if S [Length (S)] = TimeSeparator then
          begin
               S := LeftStr (S, Length (S) - 1);
               if S = '' then
               begin
                    Result := 0.0;
                    Exit;
               end;
          end;

          //Frac ensures the Date Component is 0
          Result := Frac (StrToTime (S));
     except
          Result := 0.0;
          if ESBRaiseDateError then
               raise;
     end;
end;

function GetESBDateOrder (const DateFormat: string): TESBDateOrder;
var
     I, Len: LongWord;
     Ch: Char;
begin
     Result := edoDMY;
     Len := Length (DateFormat);
     for I := 1 to Len do
     begin
          Ch := Upcase (DateFormat [I]);
          if Ch in ['D', 'E', 'M', 'Y'] then
          begin
               case Ch of
                    'D': Result := edoDMY;
                    'E': Result := edoYMD;
                    'M': Result := edoMDY;
                    'Y': Result := edoYMD;
               else
                    Result := edoUnknown;
               end;
               Break;
          end;
     end;
end;

{---}

function Str2Date (const DateStr: string): TDateTime;
var
     P1, P2, I: Integer;
     Yr: Word;
     DateOrder: TESBDateOrder;
     Hold: Boolean;
     S: string;
     Found: Boolean;
begin
     S := UpperCase (Trim (DateStr));
     if S = '' then
     begin
          Result := 0.0;
          Exit;
     end;

     if S [1] = '+' then
     begin
          Result := ESBToday + Str2Float (RightAfterStr (S, 1));
          Exit;
     end
     else if S [1] = '-' then
     begin
          Result := ESBToday - Str2Float (RightAfterStr (S, 1));
          Exit;
     end;

     DateOrder := GetESBDateOrder (ShortDateFormat);

     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          if IsDigitStr (S) then
          begin
               case Length (S) of
                    4: S := LeftStr (S, 2) + DateSeparator + RightStr (S, 2);
                    6: S := LeftStr (S, 2) + DateSeparator + Copy (S, 3, 2)
                         + DateSeparator + Copy (S, 5, 2);
                    8:
                         begin
                              if DateOrder = edoYMD then
                                   S := LeftStr (S, 4) + DateSeparator + Copy (S, 5, 2)
                                        + DateSeparator + Copy (S, 7, 2)
                              else
                                   S := LeftStr (S, 2) + DateSeparator + Copy (S, 3, 2)
                                        + DateSeparator + Copy (S, 5, 4);
                         end;
               end;
          end
          else
          begin
               Found := False;
               for I := 1 to 12 do
               begin
                    P1 := Pos (UpperCase (LongMonthNames [I]), S);
                    if P1 > 0 then
                    begin
                         S := LeftStr (S, P1 - 1) + Int2EStr (I) +
                              RightAfterStr (S, P1 + Length (LongMonthNames [I]) - 1);
                         Found := True;
                         Break;
                    end;
               end;

               if not Found then
               begin
                    for I := 1 to 12 do
                    begin
                         P1 := Pos (UpperCase (ShortMonthNames [I]), S);
                         if P1 > 0 then
                         begin
                              S := LeftStr (S, P1 - 1) + Int2EStr (I) +
                                   RightAfterStr (S, P1 + Length (ShortMonthNames [I]) - 1);
                              Break;
                         end;
                    end;
               end;
          end;

          try
               // Allow '-' and '/' as valid alternatives for DateSeparator
               S := ReplaceChStr (S, '-', DateSeparator);
               S := ReplaceChStr (S, '/', DateSeparator);
               S := ReplaceChStr (S, '\', DateSeparator);
               S := ReplaceChStr (S, ' ', DateSeparator);
               S := ReplaceChStr (S, '.', DateSeparator);
               S := ReplaceChStr (S, ',', DateSeparator);

               // Remove trailing Separator if any
               if S [Length (S)] = DateSeparator then
               begin
                    S := LeftStr (S, Length (S) - 1);
                    if S = '' then
                    begin
                         Result := 0.0;
                         Exit;
                    end;
               end;

               // Remove Duplicate Separators
               repeat
                    P1 := Pos (DateSeparator + DateSeparator, S);
                    if P1 <> 0 then
                         Delete (S, P1, 1);
               until P1 = 0;

               P1 := ESBPosCh (DateSeparator, S);
               if P1 > 0 then // If at least one Date Separator
               begin
                    P2 := ESBPosCh (DateSeparator, Copy (S, P1 + 1, Length (S) - P1));
                    if P2 > 0 then // If 2 Date Separators
                    begin
                         // Get Components
                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        Yr := Str2Word (Copy (S, P1 + P2 + 1, Length (S) - (P1 + P2)));
                                   end;
                         else
                              begin
                                   Yr := Str2Word (LeftStr (S, P1 - 1));
                              end;
                         end;

                         if Yr < 100 then // If 2 Digit
                         begin
                              case ESB2DigitYr of
                                   // edyNone - Nothing has to be done
                                   edyCutOff: // Process using ESB2DigitCutOff
                                        begin
                                             if 1900 + Yr < ESB2DigitCutOff then
                                                  Yr := 2000 + Yr
                                             else
                                                  Yr := 1900 + Yr
                                        end;
                                   edyHistoric: // Take Yr as this year or earlier
                                        begin
                                             if 2000 + Yr <= ThisYear then
                                                  Yr := 2000 + Yr
                                             else
                                                  Yr := 1900 + Yr;
                                        end;
                              end;
                         end;
                         // Rebuild String
                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        S := LeftStr (S, P1 + P2) + Int2EStr (Yr);
                                   end;
                              edoYMD:
                                   begin
                                        S := Int2EStr (Yr) + RightAfterStr (S, P1 - 1);
                                   end;
                         end;
                    end
                    else
                    begin
                         // Assume This Year is implied
                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        S := S + DateSeparator + Int2EStr (ThisYear)
                                   end;
                              edoYMD:
                                   begin
                                        S := Int2EStr (ThisYear) + DateSeparator + S;
                                   end;
                         end;
                    end;
               end
               else
               begin
                    // Assume This Month and Year are implied
                    case DateOrder of
                         edoDMY:
                              begin
                                   S := S + DateSeparator + Int2EStr (ThisMonth)
                                        + DateSeparator + Int2EStr (ThisYear);
                              end;
                         edoMDY:
                              begin
                                   S := Int2EStr (ThisMonth) + DateSeparator + S
                                        + DateSeparator + Int2EStr (ThisYear);
                              end;
                         edoYMD:
                              begin
                                   S := Int2EStr (ThisYear) + DateSeparator +
                                        Int2EStr (ThisMonth) + DateSeparator + S;
                              end;
                    end;
               end;

               //Int ensures the fractional Component is 0
               Result := Int (StrToDate (S));
          except
               Result := 0.0;
               if ESBRaiseDateError then
                    raise EConvertError.Create (rsInvalidDate + ' - ' + DateStr);
          end;
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Str2Date (const DateStr: string; const Year, StartMonth: Integer): TDateTime;
var
     P1, P2, I: Integer;
     Yr, Mnth: Integer;
     DateOrder: TESBDateOrder;
     Hold: Boolean;
     S: string;
     Found: Boolean;
begin
     if (StartMonth < 1) or (StartMonth > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     S := UpperCase (Trim (DateStr));
     if S = '' then
     begin
          Result := 0.0;
          Exit;
     end;

     if S [1] = '+' then
     begin
          Result := ESBToday + Str2Float (RightAfterStr (S, 1));
          Exit;
     end
     else if S [1] = '-' then
     begin
          Result := ESBToday - Str2Float (RightAfterStr (S, 1));
          Exit;
     end;

     DateOrder := GetESBDateOrder (ShortDateFormat);

     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          if IsDigitStr (S) then
          begin
               case Length (S) of
                    4: S := LeftStr (S, 2) + DateSeparator + RightStr (S, 2);
                    6: S := LeftStr (S, 2) + DateSeparator + Copy (S, 3, 2)
                         + DateSeparator + Copy (S, 5, 2);
                    8:
                         begin
                              if DateOrder = edoYMD then
                                   S := LeftStr (S, 4) + DateSeparator + Copy (S, 5, 2)
                                        + DateSeparator + Copy (S, 7, 2)
                              else
                                   S := LeftStr (S, 2) + DateSeparator + Copy (S, 3, 2)
                                        + DateSeparator + Copy (S, 5, 4);
                         end;
               end;
          end
          else
          begin
               Found := False;
               for I := 1 to 12 do
               begin
                    P1 := Pos (UpperCase (LongMonthNames [I]), S);
                    if P1 > 0 then
                    begin
                         S := LeftStr (S, P1 - 1) + Int2EStr (I) +
                              RightAfterStr (S, P1 + Length (LongMonthNames [I]) - 1);
                         Found := True;
                         Break;
                    end;
               end;

               if not Found then
               begin
                    for I := 1 to 12 do
                    begin
                         P1 := Pos (UpperCase (ShortMonthNames [I]), S);
                         if P1 > 0 then
                         begin
                              S := LeftStr (S, P1 - 1) + Int2EStr (I) +
                                   RightAfterStr (S, P1 + Length (ShortMonthNames [I]) - 1);
                              Break;
                         end;
                    end;
               end;
          end;

          try
               // Allow '-' and '/' as valid alternatives for DateSeparator
               S := ReplaceChStr (S, '-', DateSeparator);
               S := ReplaceChStr (S, '/', DateSeparator);
               S := ReplaceChStr (S, '\', DateSeparator);
               S := ReplaceChStr (S, ' ', DateSeparator);
               S := ReplaceChStr (S, '.', DateSeparator);
               S := ReplaceChStr (S, ',', DateSeparator);

               // Remove trailing Separator if any
               if S [Length (S)] = DateSeparator then
               begin
                    S := LeftStr (S, Length (S) - 1);
                    if S = '' then
                    begin
                         Result := 0.0;
                         Exit;
                    end;
               end;

               // Remove Duplicate Separators
               repeat
                    P1 := Pos (DateSeparator + DateSeparator, S);
                    if P1 <> 0 then
                         Delete (S, P1, 1);
               until P1 = 0;

               P1 := ESBPosCh (DateSeparator, S);
               if P1 > 0 then // If at least one Date Separator
               begin
                    P2 := ESBPosCh (DateSeparator, Copy (S, P1 + 1, Length (S) - P1));
                    if P2 > 0 then // If 2 Date Separators
                    begin
                         // Get Components
                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        Yr := Str2Word (Copy (S, P1 + P2 + 1, Length (S) - (P1 + P2)));
                                   end;
                         else
                              begin
                                   Yr := Str2Word (LeftStr (S, P1 - 1));
                              end;
                         end;

                         if Yr < 100 then // If 2 Digit
                         begin
                              case ESB2DigitYr of
                                   // edyNone - Nothing has to be done
                                   edyCutOff: // Process using ESB2DigitCutOff
                                        begin
                                             if 1900 + Yr < ESB2DigitCutOff then
                                                  Yr := 2000 + Yr
                                             else
                                                  Yr := 1900 + Yr
                                        end;
                                   edyHistoric: // Take Yr as this year or earlier
                                        begin
                                             if 2000 + Yr <= Year + 1 then
                                                  Yr := 2000 + Yr
                                             else
                                                  Yr := 1900 + Yr;
                                        end;
                              end;
                         end;
                         // Rebuild String
                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        S := LeftStr (S, P1 + P2) + Int2EStr (Yr);
                                   end;
                              edoYMD:
                                   begin
                                        S := Int2EStr (Yr) + RightAfterStr (S, P1 - 1);
                                   end;
                         end;
                    end
                    else
                    begin
                         // Assume This Year is implied
                         case DateOrder of
                              edoDMY:
                                   begin
                                        Mnth := Str2Int (RightAfterChStr (S, DateSeparator));
                                   end;
                         else
                              Mnth := Str2Int (LeftTillChStr (S, DateSeparator));
                         end;
                         if Mnth < StartMonth then
                              Yr := Year + 1
                         else
                              Yr := Year;

                         case DateOrder of
                              edoDMY, edoMDY:
                                   begin
                                        S := S + DateSeparator + Int2EStr (Yr)
                                   end;
                              edoYMD:
                                   begin
                                        S := Int2EStr (Yr) + DateSeparator + S;
                                   end;
                         end;
                    end;
               end
               else
               begin
                    Mnth := ThisMonth;
                    if Mnth < StartMonth then
                         Yr := Year + 1
                    else
                         Yr := Year;

                    // Assume This Month and Year are implied
                    case DateOrder of
                         edoDMY:
                              begin
                                   S := S + DateSeparator + Int2EStr (Mnth)
                                        + DateSeparator + Int2EStr (Yr);
                              end;
                         edoMDY:
                              begin
                                   S := Int2EStr (Mnth) + DateSeparator + S
                                        + DateSeparator + Int2EStr (Yr);
                              end;
                         edoYMD:
                              begin
                                   S := Int2EStr (Yr) + DateSeparator +
                                        Int2EStr (Mnth) + DateSeparator + S;
                              end;
                    end;
               end;

               //Int ensures the fractional Component is 0
               Result := Int (StrToDate (S));
          except
               Result := 0.0;
               if ESBRaiseDateError then
                    raise EConvertError.Create (rsInvalidDate + ' - ' + DateStr);
          end;
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Str2HistoricDate (const DateStr: string): TDateTime;
var
     Hold: TESB2DigitYr;
begin
     Hold := ESB2DigitYr;
     ESB2DigitYr := edyHistoric;
     try
          Result := Str2Date (DateStr);
     finally
          ESB2DigitYr := Hold;
     end;
end;

function Str2CutoffDate (const DateStr: string; const CutOff: Word): TDateTime;
var
     Hold1: TESB2DigitYr;
     Hold2: Word;
begin
     Hold1 := ESB2DigitYr;
     Hold2 := ESB2DigitCutOff;
     ESB2DigitYr := edyCutOff;
     ESB2DigitCutOff := CutOff;
     try
          Result := Str2Date (DateStr);
     finally
          ESB2DigitYr := Hold1;
          ESB2DigitCutOff := Hold2;
     end;
end;

function DigitStr2Date (const DateStr: string): TDateTime;
var
     Day, Month, Year: Integer;
     S: string;
begin
     if Length (DateStr) < 8 then
          raise EConvertError.Create (rsInvalidDate);

     S := Trim (DateStr);
     Year := Str2Int (LeftStr (S, 4));
     Month := Str2Int (Copy (S, 5, 2));
     Day := Str2Int (Copy (S, 7, 2));
     Result := OptEncodeDateI (Year, Month, Day);
end;

function StrFormat2Date (const DateStr: string; const Format: string): TDateTime;
var
     Hold: string;
begin
     Hold := ShortDateFormat;
     ShortDateFormat := Format;
     try
          Result := Str2Date (DateStr);
     finally
          ShortDateFormat := Hold;
     end;
end;

function Date2LongMonth (const DT: TDateTime): string;
begin
     Result := LongMonthNames [Date2Month (DT)];
end;

function Date2ShortMonth (const DT: TDateTime): string;
begin
     Result := ShortMonthNames [Date2Month (DT)];
end;

function Date2ShortMonthYY (const DT: TDateTime): string;
var
     Year: Integer;
begin
     Year := OptDate2Year (DT);
     Result := ShortMonthNames [Date2Month (DT)] + ' ' + Int2ZStr (Year mod 100, 2);
end;

function Date2ShortMonthYYYY (const DT: TDateTime): string;
var
     Year: Integer;
begin
     Year := OptDate2Year (DT);
     Result := ShortMonthNames [Date2Month (DT)] + ' ' + Int2ZStr (Year, 4);
end;

function Date2LongDOW (const DT: TDateTime): string;
begin
     Result := LongDayNames [DayOfWeek (DT)];
end;

function Date2ShortDOW (const DT: TDateTime): string;
begin
     Result := ShortDayNames [DayOfWeek (DT)];
end;

function MonthName2Month (const MonthName: string): Word;
var
     I: Integer;
     Len: Integer;
     MN: string;
begin
     Result := 0;
     if MonthName = '' then
          Exit;
     Len := Length (MonthName);
     MN := AnsiUpperCase (MonthName);
     for I := 1 to 12 do
     begin
          if AnsiUpperCase (LeftStr (ShortMonthNames [I], Len)) = MN then
          begin
               Result := I;
               Exit;
          end;
     end;
     for I := 1 to 12 do
     begin
          if AnsiUpperCase (LeftStr (LongMonthNames [I], Len)) = MN then
          begin
               Result := I;
               Exit;
          end;
     end;
end;

function DayName2DOW (const DayName: string): Byte;
var
     I: Integer;
     Len: Integer;
     DN: string;
begin
     Result := 0;
     if DayName = '' then
          Exit;
     Len := Length (DayName);
     DN := AnsiUpperCase (DayName);
     for I := 1 to 7 do
     begin
          if AnsiUpperCase (LeftStr (ShortDayNames [I], Len)) = DN then
          begin
               Result := I;
               Exit;
          end;
     end;
     for I := 1 to 7 do
     begin
          if AnsiUpperCase (LeftStr (LongDayNames [I], Len)) = DN then
          begin
               Result := I;
               Exit;
          end;
     end;
end;

{--- Year Based Routines ---}

function IsLeapYear (const Year: Word): Boolean;
begin
     Result := ((Year and 3) = 0) and ((Year mod 100 > 0) or (Year mod 400 = 0))
end;

function IsLeapYear (const Year: Integer): Boolean;
begin
     Result := ((Year and 3) = 0) and ((Year mod 100 > 0) or (Year mod 400 = 0))
end;

function IsLeapYear (const DT: TDateTime): Boolean;
begin
     Result := IsLeapYear (Date2Year (DT));
end;

function GetGoldenNumber (const Year: Word): Integer;
begin
     Result := Year mod 19 + 1;
end;

function GetGoldenNumber (const Year: Integer): Integer;
begin
     Result := Year mod 19 + 1;
end;

function GetGoldenNumber (const DT: TDateTime): Integer;
begin
     Result := GetGoldenNumber (Date2Year (DT));
end;

function GetEpact (const Year: Word): Integer;
var
     Century: Integer;
begin
     Century := Year div 100 + 1;
     Result := ((11 * (GetGoldenNumber (Year) - 1)) mod 30
          + (8 * Century + 5) div 25) - (3 * Century) div 4 + 8;
     while Result < 1 do
          Result := Result + 30;
     while Result > 30 do
          Result := Result - 30;
end;

function GetEpact (const Year: Integer): Integer;
var
     Century: Integer;
begin
     Century := Year div 100 + 1;
     Result := ((11 * (GetGoldenNumber (Year) - 1)) mod 30
          + (8 * Century + 5) div 25) - (3 * Century) div 4 + 8;
     while Result < 1 do
          Result := Result + 30;
     while Result > 30 do
          Result := Result - 30;
end;

function GetEpact (const DT: TDateTime): Integer;
begin
     Result := GetEpact (Date2Year (DT));
end;

function GetEasterSunday (const Year: Word): TDateTime;
var
     C, I, J, H, G, L: Integer;
     D, M: Word;
begin
     G := GetGoldenNumber (Year) - 1;
     C := Year div 100;
     H := (C - C div 4 - (8 * C + 13) div 25 + 19 * G + 15) mod 30;
     I := H - (H div 28) * (1 - (H div 28) * (29 div (H + 1)) * ((21 - G) div 11));
     J := (Year + Year div 4 + I + 2 - C + C div 4) mod 7;

     L := I - J;
     M := 3 + (L + 40) div 44;
     D := L + 28 - 31 * (M div 4);
     Result := OptEncodeDateW (Year, M, D);
end;

function GetEasterSunday (const Year: Integer): TDateTime;
var
     C, I, J, H, G, L: Integer;
     D, M: Integer;
begin
     G := GetGoldenNumber (Year) - 1;
     C := Year div 100;
     H := (C - C div 4 - (8 * C + 13) div 25 + 19 * G + 15) mod 30;
     I := H - (H div 28) * (1 - (H div 28) * (29 div (H + 1)) * ((21 - G) div 11));
     J := (Year + Year div 4 + I + 2 - C + C div 4) mod 7;

     L := I - J;
     M := 3 + (L + 40) div 44;
     D := L + 28 - 31 * (M div 4);
     Result := OptEncodeDateI (Year, M, D);
end;

function GetEasterSunday (const DT: TDateTime): TDateTime;
begin
     Result := GetEasterSunday (Date2Year (DT));
end;

function GetGoodFriday (const Year: Word): TDateTime;
begin
     Result := GetEasterSunday (Year) - 2;
end;

function GetGoodFriday (const Year: Integer): TDateTime;
begin
     Result := GetEasterSunday (Year) - 2;
end;

function GetGoodFriday (const DT: TDateTime): TDateTime;
begin
     Result := GetGoodFriday (OptDate2Year (DT));
end;

function GetChristmasDay (const Year: Word): TDateTime;
begin
     Result := OptEncodeDateW (Year, 12, 25);
end;

function GetChristmasDay (const Year: Integer): TDateTime;
begin
     Result := OptEncodeDateI (Year, 12, 25);
end;

function GetChristmasDay (const DT: TDateTime): TDateTime;
begin
     Result := GetChristmasDay (OptDate2Year (DT));
end;

function GetFirstDayOfYear (const Year: Word): TDateTime;
begin
     Result := OptEncodeDateW (Year, 1, 1);
end;

function GetFirstDayOfYear (const Year: Integer): TDateTime;
begin
     Result := OptEncodeDateI (Year, 1, 1);
end;

function GetFirstDayOfYear (const DT: TDateTime): TDateTime;
begin
     Result := GetFirstDayOfYear (OptDate2Year (DT));
end;

function GetLastDayOfYear (const Year: Word): TDateTime;
begin
     Result := OptEncodeDateW (Year, 12, 31);
end;

function GetLastDayOfYear (const Year: Integer): TDateTime;
begin
     Result := OptEncodeDateI (Year, 12, 31);
end;

function GetLastDayOfYear (const DT: TDateTime): TDateTime;
begin
     Result := GetLastDayOfYear (OptDate2Year (DT));
end;

function GetFirstSundayOfYear (const Year: Word): TDateTime;
var
     StartYear: TDateTime;
begin
     StartYear := GetFirstDayOfYear (Year);
     if DayOfWeek (StartYear) = 1 then
          Result := StartYear
     else
          Result := StartOfWeek (StartYear) + 7;
end;

function GetFirstSundayOfYear (const Year: Integer): TDateTime;
var
     StartYear: TDateTime;
begin
     StartYear := GetFirstDayOfYear (Year);
     if DayOfWeek (StartYear) = 1 then
          Result := StartYear
     else
          Result := StartOfWeek (StartYear) + 7;
end;

function GetFirstSundayOfYear (const DT: TDateTime): TDateTime;
begin
     Result := GetFirstDayOfYear (OptDate2Year (DT));
end;

{--- Date Arithmetic ---}

function StartOfWeek (const DT: TDateTime): TDateTime;
begin
     Result := DT - DayOfWeek (DT) + 1;
end;

function EndOfWeek (const DT: TDateTime): TDateTime;
begin
     Result := DT - DayOfWeek (DT) + 7;
end;

function MinutesApart (const DT1, DT2: TDateTime): Word;
var
     Hr1, Min1, Sec1, MSec1: Word;
     Hr2, Min2, Sec2, MSec2: Word;
begin
     ESBDecodeTime (DT1, Hr1, Min1, Sec1, MSec1);
     ESBDecodeTime (DT2, Hr2, Min2, Sec2, MSec2);
     if Min2 < Min1 then
     begin
          Min2 := Min2 + 60;
          if Hr2 > 0 then
               Dec (Hr2)
          else
               Hr2 := 23;
     end;
     if Hr1 > Hr2 then
          Hr2 := Hr2 + 24;
     Result := (Hr2 - Hr1) * 60 + (Min2 - Min1);
end;

function TimeApartInFortnights (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1) / 14;
end;

function TimeApartInWeeks (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1) / 7;
end;

function TimeApartInDays (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1);
end;

function TimeApartInHrs (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1) * 24;
end;

function TimeApartInMins (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1) * 24 * 60;
end;

function TimeApartInSecs (const DT1, DT2: TDateTime): Extended;
begin
     if SameDateTime (DT1, DT2) then
          Result := 0
     else
          Result := (DT2 - DT1) * 24 * 60 * 60;
end;

function MS2TimeStr (const MS: Int64): string;
var
     L: Int64;
     Hold: Boolean;
begin
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          L := MS;
          Result := '.' + Int2ZStr (L mod 1000, 3);
          L := L div 1000;
          Result := ':' + Int2ZStr (L mod 60, 2) + Result;
          L := L div 60;
          Result := ':' + Int2ZStr (L mod 60, 2) + Result;
          L := L div 60;
          Result := Int2EStr (L) + Result;
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function AdjustDateYear (const D: TDateTime; const Year: Word): TDateTime;
var
     Day, Month, OldYear: Word;
begin
     OptDecodeDateW (D, OldYear, Month, Day);
     if Year = OldYear then
     begin
          Result := Int (D);
          Exit;
     end;
     if not IsLeapYear (Year) and (Month = 2) and (Day = 29) then
     begin
          Month := 3;
          Day := 1;
     end;
     Result := OptEncodeDateW (Year, Month, Day);
end;

function AdjustDateYear (const D: TDateTime; const Year: Integer): TDateTime;
var
     Day, Month, OldYear: Integer;
begin
     OptDecodeDateI (D, OldYear, Month, Day);
     if Year = OldYear then
     begin
          Result := Int (D);
          Exit;
     end;
     if not IsLeapYear (Year) and (Month = 2) and (Day = 29) then
     begin
          Month := 3;
          Day := 1;
     end;
     Result := OptEncodeDateI (Year, Month, Day);
end;

function GetDateTimeStamp: string;
var
     DT: TDateTime;
     Year, Month, Day: Integer;
     Hr, Min, Sec, MSec: Word;
     Hold: Boolean;
begin
     DT := Now;
     OptDecodeDateI (DT, Year, Month, Day);
     ESBDecodeTime (DT, Hr, Min, Sec, MSec);

     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Year, 4) + Int2ZStr (Month, 2) +
               Int2ZStr (Day, 2) + '-' + Int2ZStr (Hr, 2) +
               Int2ZStr (Min, 2) + Int2ZStr (Sec, 2) + Int2ZStr (MSec, 3);
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function AddSecs (const DT: TDateTime; const Secs: Extended): TDateTime;
begin
     Result := DT + Secs * OneDTSecond
end;

function AddMins (const DT: TDateTime; const Mins: Extended): TDateTime;
begin
     Result := DT + Mins * OneDTMinute
end;

function AddHrs (const DT: TDateTime; const Hrs: Extended): TDateTime;
begin
     Result := DT + Hrs * OneDTHour
end;

function AddWeeks (const DT: TDateTime; const Weeks: Extended): TDateTime;
begin
     Result := DT + Weeks * 7;
end;

function AddFortnights (const DT: TDateTime; const FNights: Extended): TDateTime;
begin
     Result := AddWeeks (DT, FNights * 2);
end;

function AddMonths (const DT: TDateTime; const Months: Extended): TDateTime;
var
     Day, Month, Year: Integer;
     IMonth: Integer;
begin
     OptDecodeDateI (DT, Year, Month, Day);
     IMonth := Month + Trunc (Months);

     if IMonth > 12 then
     begin
          Year := Year + (IMonth - 1) div 12;
          IMonth := IMonth mod 12;
          if IMonth = 0 then
               IMonth := 12;
     end
     else if IMonth < 1 then
     begin
          Year := Year + (IMonth div 12) - 1; // sub years;
          IMonth := 12 - abs (IMonth) mod 12;
     end;
     Month := IMonth;

     // Ensure Day of Month is valid
     if Month = 2 then
     begin
          if IsLeapYear (Year) and (Day > 29) then
               Day := 29
          else if not IsLeapYear (Year) and (Day > 28) then
               Day := 28;
     end
     else if (Month in [9, 4, 6, 11]) and (Day = 31) then
          Day := 30;

     Result := OptEncodeDateI (Year, Month, Day) + Frac (Months) * 30 +
          Frac (DT);
end;

function AddCalendarMonths (const DT: TDateTime; const Months: Integer): TDateTime;
var
     Day, Month, Year: Integer;
     IMonth: Integer;
begin
     OptDecodeDateI (DT, Year, Month, Day);
     IMonth := Month + Months;

     if IMonth > 12 then
     begin
          Year := Year + (IMonth - 1) div 12;
          IMonth := IMonth mod 12;
          if IMonth = 0 then
               IMonth := 12;
     end
     else if IMonth < 1 then
     begin
          Year := Year + (IMonth div 12) - 1; // sub years;
          IMonth := 12 - abs (IMonth) mod 12;
     end;
     Month := IMonth;

     // Ensure Day of Month is valid
     if Month = 2 then
     begin
          if IsLeapYear (Year) and (Day > 29) then
               Day := 29
          else if not IsLeapYear (Year) and (Day > 28) then
               Day := 28;
     end
     else if (Month in [9, 4, 6, 11]) and (Day = 31) then
          Day := 30;

     Result := OptEncodeDateI (Year, Month, Day) + Frac (DT);
end;

function AddQuarters (const DT: TDateTime; const Qtrs: Extended): TDateTime;
begin
     Result := AddMonths (DT, Qtrs * 3);
end;

function AddSemesters (const DT: TDateTime; const Sems: Extended): TDateTime;
begin
     Result := AddMonths (DT, Sems * 6);
end;

function AddYrs (const DT: TDateTime; const Yrs: Extended): TDateTime;
var
     Day, Month, Year: Integer;
begin
     OptDecodeDateI (DT, Year, Month, Day);
     Year := Year + Trunc (Yrs);
     if not IsLeapYear (Year) and (Month = 2) and (Day = 29) then
          Day := 28;
     Result := OptEncodeDateI (Year, Month, Day) + Frac (Yrs) * 365.25
          + Frac (DT);
end;

function AddDays (const DT: TDateTime; const Days: Extended): TDateTime;
begin
     Result := DT + Days;
end;

function SubtractSecs (const DT: TDateTime; const Secs: Extended): TDateTime;
begin
     Result := AddSecs (DT, -1 * Secs);
end;

function SubtractMins (const DT: TDateTime; const Mins: Extended): TDateTime;
begin
     Result := AddMins (DT, -1 * Mins);
end;

function SubtractHrs (const DT: TDateTime; const Hrs: Extended): TDateTime;
begin
     Result := AddHrs (DT, -1 * Hrs);
end;

function SubtractWeeks (const DT: TDateTime; const Weeks: Extended): TDateTime;
begin
     Result := AddWeeks (DT, -1 * Weeks);
end;

function SubtractFortnights (const DT: TDateTime; const FNights: Extended): TDateTime;
begin
     Result := AddWeeks (DT, FNights * -2);
end;

function SubtractMonths (const DT: TDateTime; const Months: Extended): TDateTime;
begin
     Result := AddMonths (DT, -1 * Months);
end;

function SubtractQuarters (const DT: TDateTime; const Qtrs: Extended): TDateTime;
begin
     Result := AddMonths (DT, Qtrs * -3);
end;

function SubtractSemesters (const DT: TDateTime; const Sems: Extended): TDateTime;
begin
     Result := AddMonths (DT, Sems * -6);
end;

function SubtractYrs (const DT: TDateTime; const Yrs: Extended): TDateTime;
begin
     Result := AddYrs (DT, -1 * Yrs);
end;

function SubtractDays (const DT: TDateTime; const Days: Extended): TDateTime;
begin
     Result := DT - Days;
end;

function AgeAtDate (const DOB, DT: TDateTime): Integer;
var
     D1, M1, Y1, D2, M2, Y2: Integer;
begin
     if DT < DOB then
          Result := -1
     else
     begin
          OptDecodeDateI (DOB, Y1, M1, D1);
          OptDecodeDateI (DT, Y2, M2, D2);
          Result := Y2 - Y1;
          if (M2 < M1) or ((M2 = M1) and (D2 < D1)) then
               Dec (Result);
     end;
end;

function AgeNow (const DOB: TDateTime): Integer;
begin
     Result := AgeAtDate (DOB, Date);
end;

function GetLastDayOfMonth (const DT: TDateTime): TDateTime;
var
     D, M, Y: Integer;
begin
     OptDecodeDateI (DT, Y, M, D);
     case M of
          2:
               begin
                    if IsLeapYear (Y) then
                         D := 29
                    else
                         D := 28;
               end;
          4, 6, 9, 11: D := 30
     else
          D := 31;
     end;
     Result := OptEncodeDateI (Y, M, D) + Frac (DT);
end;

function GetLastDayOfMonth (const Month, Year: Word): TDateTime;
var
     D: Word;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     case Month of
          2:
               begin
                    if IsLeapYear (Year) then
                         D := 29
                    else
                         D := 28;
               end;
          4, 6, 9, 11: D := 30
     else
          D := 31;
     end;
     Result := OptEncodeDateW (Year, Month, D);
end;

function GetLastDayOfMonth (const Month, Year: Integer): TDateTime;
var
     D: Integer;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     case Month of
          2:
               begin
                    if IsLeapYear (Year) then
                         D := 29
                    else
                         D := 28;
               end;
          4, 6, 9, 11: D := 30
     else
          D := 31;
     end;
     Result := OptEncodeDateI (Year, Month, D);
end;

function GetFirstDayofMonth (const DT: TDateTime): TDateTime;
var
     D, M, Y: Integer;
begin
     OptDecodeDateI (DT, Y, M, D);
     Result := OptEncodeDateI (Y, M, 1) + Frac (DT);
end;

function GetFirstDayofMonth (const Month, Year: Word): TDateTime;
begin
     Result := OptEncodeDateW (Year, Month, 1);
end;

function GetFirstDayofMonth (const Month, Year: Integer): TDateTime;
begin
     Result := OptEncodeDateI (Year, Month, 1);
end;

function DatesInSameMonth (const DT1, DT2: TDateTime): Boolean;
begin
     Result := Date2Month (DT1) = Date2Month (DT2);
end;

function DatesInSameYear (const DT1, DT2: TDateTime): Boolean;
begin
     Result := Date2Year (DT1) = Date2Year (DT2);
end;

function DatesInSameMonthYear (const DT1, DT2: TDateTime): Boolean;
begin
     Result := DatesInSameMonth (DT1, DT2) and DatesInSameYear (DT1, DT2);
end;

function DaysApart (const DT1, DT2: TDateTime): LongInt;
begin
     Result := Trunc (DT2) - Trunc (DT1);
end;

function DateIsLeapYear (const DT: TDateTime): Boolean;
begin
     Result := IsLeapYear (Date2Year (DT));
end;

function ExactWeeksApart (const DT1, DT2: TDateTime): Extended;
begin
     Result := DaysApart (DT1, DT2) / 7;
end;

function WeeksApart (const DT1, DT2: TDateTime): Integer;
begin
     Result := DaysApart (DT1, DT2) div 7;
end;

function CalendarWeeksApart (const DT1, DT2: TDateTime): Integer;
begin
     Result := WeeksApart (StartOfWeek (DT1), StartOfWeek (DT2));
end;

function CalendarMonthsApart (const DT1, DT2: TDateTime): Integer;
var
     D1, M1, Y1, D2, M2, Y2: Integer;
begin
     OptDecodeDateI (DT1, Y1, M1, D1);
     OptDecodeDateI (DT2, Y2, M2, D2);
     if Y1 = Y2 then
          Result := M2 - M1
     else if Y2 > Y1 then
          Result := (12 - M1) + (Y2 - (Y1 + 1)) * 12 + M2
     else
          Result := - ((12 - M2) + (Y1 - (Y2 + 1)) * 12 + M1)
end;

function DaysInMonth (const DT: TDateTime): Byte;
begin
     case Date2Month (DT) of
          2: if DateIsLeapYear (DT) then
                    Result := 29
               else
                    Result := 28;
          4, 6, 9, 11: Result := 30;
     else
          Result := 31;
     end;
end;

function DaysInMonth (const Month, Year: Word): Byte;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     case Month of
          2: if IsLeapYear (Year) then
                    Result := 29
               else
                    Result := 28;
          4, 6, 9, 11: Result := 30;
     else
          Result := 31;
     end;
end;

function DaysInMonth (const Month, Year: Integer): Byte;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     case Month of
          2: if IsLeapYear (Year) then
                    Result := 29
               else
                    Result := 28;
          4, 6, 9, 11: Result := 30;
     else
          Result := 31;
     end;
end;

function DaysInThisMonth: Byte;
begin
     Result := DaysInMonth (ESBToday);
end;

function DaysLeftInMonth (const DT: TDateTime): Byte;
begin
     Result := DaysInMonth (DT) - Date2Day (DT);
end;

function DaysLeftInThisMonth: Byte;
begin
     Result := DaysLeftInMonth (ESBToday);
end;

function DaysInYear (const DT: TDateTime): Integer;
begin
     if DateIsLeapYear (DT) then
          Result := 366
     else
          Result := 365;
end;

function DaysInYear (const Year: Word): Integer;
begin
     if IsLeapYear (Year) then
          Result := 366
     else
          Result := 365;
end;

function DaysInYear (const Year: Integer): Integer;
begin
     if IsLeapYear (Year) then
          Result := 366
     else
          Result := 365;
end;

function DayOfYear (const DT: TDateTime): Word;
begin
     Result := Trunc (DT) - Trunc (OptEncodeDateW (Date2Year (DT), 1, 1)) + 1;
end;

function OptDayOfYear (const DT: TDateTime): Integer;
begin
     Result := Trunc (DT) - Trunc (OptEncodeDateI (OptDate2Year (DT), 1, 1)) + 1;
end;

function DaysLeftInYear (const DT: TDateTime): Word;
begin
     Result := DaysInYear (DT) - DayOfYear (DT);
end;

function ThisDayOfYear: Word;
begin
     Result := DayOfYear (ESBToday);
end;

function DaysLeftInThisYear: Word;
begin
     Result := DaysLeftInYear (ESBToday);
end;

function OptDaysLeftInYear (const DT: TDateTime): Integer;
begin
     Result := DaysInYear (DT) - OptDayOfYear (DT);
end;

function OptThisDayOfYear: Integer;
begin
     Result := OptDayOfYear (ESBToday);
end;

function OptDaysLeftInThisYear: Integer;
begin
     Result := OptDaysLeftInYear (ESBToday);
end;

function WhichSemester (const DT: TDateTime): Byte;
begin
     Result := (Date2Month (DT) - 1) div 6 + 1;
end;

function WhichQuarter (const DT: TDateTime): Byte;
begin
     Result := (Date2Month (DT) - 1) div 3 + 1;
end;

function GetFirstDayofQuarter (const DT: TDateTime): TDateTime;
var
     D, M, Y: Integer;
begin
     OptDecodeDateI (DT, Y, M, D);
     case M of
          1..3: M := 1;
          4..6: M := 4;
          7..9: M := 7;
          10..12: M := 10;
     end;
     Result := OptEncodeDateI (Y, M, 1) + Frac (DT);
end;

function GetLastDayofQuarter (const DT: TDateTime): TDateTime;
var
     D, M, Y: Integer;
begin
     OptDecodeDateI (DT, Y, M, D);
     case M of
          1..3:
               begin
                    M := 3;
                    D := 31;
               end;
          4..6:
               begin
                    M := 6;
                    D := 30;
               end;
          7..9:
               begin
                    M := 9;
                    D := 30;
               end;
          10..12:
               begin
                    M := 12;
                    D := 31;
               end;
     end;
     Result := OptEncodeDateI (Y, M, D) + Frac (DT);
end;

function GetFirstDayofQuarter (const Qtr: Byte; const Year: Word): TDateTime;
begin
     Result := OptEncodeDateW (Year, (Qtr - 1) * 3 + 1, 1);
end;

function GetFirstDayofQuarter (const Qtr, Year: Integer): TDateTime;
begin
     Result := OptEncodeDateI (Year, (Qtr - 1) * 3 + 1, 1);
end;

function GetLastDayofQuarter (const Qtr: Byte; const Year: Word): TDateTime;
var
     D, M: Word;
begin
     case Qtr of
          1:
               begin
                    M := 3;
                    D := 31;
               end;
          2:
               begin
                    M := 6;
                    D := 30;
               end;
          3:
               begin
                    M := 9;
                    D := 30;
               end;
          4:
               begin
                    M := 12;
                    D := 31;
               end;
     else
          begin
               M := 0;
               D := 0;
          end;
     end;
     Result := OptEncodeDateW (Year, M, D);
end;

function GetLastDayofQuarter (const Qtr, Year: Integer): TDateTime;
var
     D, M: Integer;
begin
     case Qtr of
          1:
               begin
                    M := 3;
                    D := 31;
               end;
          2:
               begin
                    M := 6;
                    D := 30;
               end;
          3:
               begin
                    M := 9;
                    D := 30;
               end;
          4:
               begin
                    M := 12;
                    D := 31;
               end;
     else
          begin
               M := 0;
               D := 0;
          end;
     end;
     Result := OptEncodeDateI (Year, M, D);
end;

function AgeAtDateInMonths (const DOB, DT: TDateTime): Integer;
var
     D1, D2: Integer;
     M1, M2: Integer;
     Y1, Y2: Integer;
begin
     if DT < DOB then
          Result := -1
     else
     begin
          OptDecodeDateI (DOB, Y1, M1, D1);
          OptDecodeDateI (DT, Y2, M2, D2);
          if Y1 = Y2 then // Same Year
               Result := M2 - M1
          else // Different Years
          begin
               // 12 months per year age
               Result := 12 * AgeAtDate (DOB, DT);
               if M1 > M2 then
                    Result := Result + (12 - M1) + M2
               else if M1 < M2 then
                    Result := Result + M2 - M1
               else if D1 > D2 then // Same Month
                    Result := Result + 12;
          end;
          if D1 > D2 then // we have counted one month too many
               Dec (Result);
     end;
end;

function AgeAtDateInWeeks (const DOB, DT: TDateTime): Integer;
begin
     if DT < DOB then
          Result := -1
     else
     begin
          Result := Trunc (DT - DOB) div 7;
     end; {else}
end;

function AgeNowInMonths (const DOB: TDateTime): Integer;
begin
     Result := AgeAtDateInMonths (DOB, Date);
end;

function AgeNowInWeeks (const DOB: TDateTime): Integer;
begin
     Result := AgeAtDateInWeeks (DOB, Date);
end;

{--- Week No Routines ---}

function Date2WeekNo (const DT: TDateTime): Integer;
var
     Year: Integer;
     FirstSunday, StartYear: TDateTime;
     WeekOfs: Byte;
begin
     Year := OptDate2Year (DT);
     StartYear := GetFirstDayOfYear (Year);
     if DayOfWeek (StartYear) = 0 then
     begin
          FirstSunday := StartYear;
          WeekOfs := 1;
     end
     else
     begin
          FirstSunday := StartOfWeek (StartYear) + 7;
          WeekOfs := 2;
          if DT < FirstSunday then
          begin
               Result := 1;
               Exit;
          end;
     end;
     Result := DaysApart (FirstSunday, StartofWeek (DT)) div 7 + WeekOfs;
end;

function DatesInSameWeekNo (const DT1, DT2: TDateTime): Boolean;
begin
     if Date2Year (DT1) <> Date2Year (DT2) then
          Result := False
     else
          Result := Date2WeekNo (DT1) = Date2WeekNo (DT2);
end;

function WeekNosApart (const DT1, DT2: TDateTime): Integer;
begin
     if Date2Year (DT1) <> Date2Year (DT2) then
          Result := -999
     else
          Result := Date2WeekNo (DT2) - Date2WeekNo (DT1);
end;

function ThisWeekNo: Integer;
begin
     Result := Date2WeekNo (ESBToday);
end;

function StartOfWeekNo (const WeekNo, Year: Word): TDateTime;
var
     FirstSunday: TDateTime;
begin
     FirstSunday := GetFirstSundayOfYear (Year);
     if Date2Day (FirstSunday) = 1 then
          Result := AddWeeks (FirstSunday, WeekNo - 1)
     else
          Result := AddWeeks (FirstSunday, WeekNo - 2)
end;

function StartOfWeekNo (const WeekNo, Year: Integer): TDateTime;
var
     FirstSunday: TDateTime;
begin
     FirstSunday := GetFirstSundayOfYear (Year);
     if OptDate2Day (FirstSunday) = 1 then
          Result := AddWeeks (FirstSunday, WeekNo - 1)
     else
          Result := AddWeeks (FirstSunday, WeekNo - 2)
end;

function EndOfWeekNo (const WeekNo, Year: Word): TDateTime;
begin
     Result := StartOfWeekNo (WeekNo, Year) + 6;
end;

function EndOfWeekNo (const WeekNo, Year: Integer): TDateTime;
begin
     Result := StartOfWeekNo (WeekNo, Year) + 6;
end;

function DWY2Date (const DOW, WeekNo, Year: Word): TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := StartOfWeekNo (WeekNo, Year) + DOW - 1;
end;

function DWY2Date (const DOW, WeekNo, Year: Integer): TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := StartOfWeekNo (WeekNo, Year) + DOW - 1;
end;

{ --- ISO-8601 Compliant Week Routines --- }

function ISODayOfWeek (const DT: TDateTime): Byte;
begin
     Result := DOW2ISODOW (DayOfWeek (DT));
end;

function ThisISODOW: Byte;
begin
     Result := ISODayOfWeek (ESBToday);
end;

function ISODOW2DOW (const ISODOW: Byte): Byte;
begin
     if (ISODOW < 1) or (ISODOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := ISODow + 1;
     if Result > 7 then
          Result := 1;
end;

function DOW2ISODOW (const DOW: Byte): Byte;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := Dow - 1;
     if Result = 0 then
          Result := 7;
end;

function StartOfISOWeek (const DT: TDateTime): TDateTime;
begin
     Result := DT - ISODayOfWeek (DT) + 1;
end;

function EndOfISOWeek (const DT: TDateTime): TDateTime;
begin
     Result := DT - ISODayOfWeek (DT) + 7;
end;

{: Most years have 52 weeks, but years that start on a Thursday and leap
years that start on a Wednesday (or Thursday) have 53 weeks. Based on
code supplied by Niklas Astram }

function ISOWeeksInYear (const Year: Word): Integer;
var
     DOW: Integer;
begin
     DOW := ISODayOfWeek (GetFirstDayOfYear (Year));
     if (DOW = 4) or ((DOW = 3) and IsLeapYear (Year)) then
          Result := 53
     else
          Result := 52;
end;

function ISOWeeksInYear (const Year: Integer): Integer;
var
     DOW: Integer;
begin
     DOW := ISODayOfWeek (GetFirstDayOfYear (Year));
     if (DOW = 4) or ((DOW = 3) and IsLeapYear (Year)) then
          Result := 53
     else
          Result := 52;
end;

function ISOWeeksInYear (const DT: TDateTime): Integer; overload;
begin
     Result := ISOWeeksInYear (Date2Year (DT));
end;

procedure Date2ISOWeekNo (const DT: TDateTime; var WeekNo: Integer;
     var Year: Word);
var
     FirstMonday, StartYear: TDateTime;
     WeekOfs: Byte;
begin
     Year := Date2Year (DT);
     StartYear := GetFirstDayOfYear (Year) + 3; // Jan 4th
     if ISODayOfWeek (StartYear) <= 4 then
     begin
          FirstMonday := StartOfISOWeek (StartYear);
          WeekOfs := 1;
          if DT < FirstMonday then
          begin
               Dec (Year);
               WeekNo := ISOWeeksInYear (Year);
               Exit;
          end;
     end
     else
     begin
          FirstMonday := StartOfISOWeek (StartYear) + 7;
          WeekOfs := 2;
          if DT < FirstMonday then
          begin
               WeekNo := 1;
               Exit;
          end;
     end;
     WeekNo := DaysApart (FirstMonday, StartofISOWeek (DT)) div 7 + WeekOfs;
     if WeekNo > ISOWeeksInYear (Year) then
     begin
          WeekNo := 1;
          Inc (Year);
     end;
end;

procedure Date2ISOWeekNo (const DT: TDateTime; var WeekNo, Year: Integer);
var
     FirstMonday, StartYear: TDateTime;
     WeekOfs: Integer;
begin
     Year := OptDate2Year (DT);
     StartYear := GetFirstDayOfYear (Year) + 3; // Jan 4th
     if ISODayOfWeek (StartYear) <= 4 then
     begin
          FirstMonday := StartOfISOWeek (StartYear);
          WeekOfs := 1;
          if DT < FirstMonday then
          begin
               Dec (Year);
               WeekNo := ISOWeeksInYear (Year);
               Exit;
          end;
     end
     else
     begin
          FirstMonday := StartOfISOWeek (StartYear) + 7;
          WeekOfs := 2;
          if DT < FirstMonday then
          begin
               WeekNo := 1;
               Exit;
          end;
     end;
     WeekNo := DaysApart (FirstMonday, StartofISOWeek (DT)) div 7 + WeekOfs;
     if WeekNo > ISOWeeksInYear (Year) then
     begin
          WeekNo := 1;
          Inc (Year);
     end;
end;

function Date2ISOWeekStr (const DT: TDateTime): string;
var
     WeekNo: Integer;
     Year: Integer;
     Hold: Boolean;
begin
     Date2ISOWeekNo (DT, WeekNo, Year);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Year, 4) + 'W' + Int2ZStr (WeekNo, 2)
               + Int2EStr (ISODayOfWeek (DT));
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Date2ISOWeekEnhStr (const DT: TDateTime): string;
var
     WeekNo: Integer;
     Year: Integer;
     Hold: Boolean;
begin
     Date2ISOWeekNo (DT, WeekNo, Year);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Year, 4) + '-W' + Int2ZStr (WeekNo, 2) + '-'
               + Int2EStr (ISODayOfWeek (DT));
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Date2ISOWeekOnlyStr (const DT: TDateTime): string;
var
     WeekNo: Integer;
     Year: Integer;
     Hold: Boolean;
begin
     Date2ISOWeekNo (DT, WeekNo, Year);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Year, 4) + 'W' + Int2ZStr (WeekNo, 2);
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Date2ISOWeekOnlyEnhStr (const DT: TDateTime): string;
var
     WeekNo: Integer;
     Year: Integer;
     Hold: Boolean;
begin
     Date2ISOWeekNo (DT, WeekNo, Year);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Year, 4) + '-W' + Int2ZStr (WeekNo, 2);
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Date2ISOStr (const DT: TDateTime): string;
var
     D, M, Y: Integer;
     Hold: Boolean;
begin
     OptDecodeDateI (DT, Y, M, D);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Y, 4) + Int2ZStr (M, 2)
               + Int2ZStr (D, 2);
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function Date2ISOInt (const DT: TDateTime): LongWord;
var
     D, M, Y: Word;
begin
     OptDecodeDateW (DT, Y, M, D);
     Result := Y * 10000 + M * 100 + D;
end;

function Date2ISOEnhStr (const DT: TDateTime): string;
var
     D, M, Y: Integer;
     Hold: Boolean;
begin
     OptDecodeDateI (DT, Y, M, D);
     Hold := ESBBlankWhenZero;
     ESBBlankWhenZero := False;
     try
          Result := Int2ZStr (Y, 4) + '-' + Int2ZStr (M, 2) + '-'
               + Int2ZStr (D, 2);
     finally
          ESBBlankWhenZero := Hold;
     end;
end;

function DatesInSameISOWeekNo (const DT1, DT2: TDateTime): Boolean;
var
     W1, W2: Integer;
     Y1, Y2: Word;
begin
     Date2ISOWeekNo (DT1, W1, Y1);
     Date2ISOWeekNo (DT2, W2, Y2);
     Result := (W1 = W2) and (Y1 = Y2);
end;

function ISOWeekNosApart (DT1, DT2: TDateTime): Int64;
var
     W1, W2: Integer;
     I, Y1, Y2: Word;
     Negative: Boolean;
     DTHold: TDateTime;
begin
     Negative := Int (DT2) < Int (DT1);
     if Negative then
     begin
          DTHold := DT1;
          DT1 := DT2;
          DT2 := DTHold;
     end;

     // Now DT1 <= DT2

     Date2ISOWeekNo (DT1, W1, Y1);
     Date2ISOWeekNo (DT2, W2, Y2);
     if Y1 = Y2 then
          Result := W2 - W1
     else
     begin
          Result := ISOWeeksInYear (Y1) - W1 + W2;
          for I := Y1 + 1 to Y2 - 1 do
               Result := Result + ISOWeeksInYear (I)
     end;

     if Negative then
          Result := -1 * Result;
end;

procedure ThisISOWeekNo (var WeekNo: Integer; var Year: Word);
begin
     Date2ISOWeekNo (ESBToday, WeekNo, Year);
end;

procedure ThisISOWeekNo (var WeekNo, Year: Integer);
begin
     Date2ISOWeekNo (ESBToday, WeekNo, Year);
end;

function GetFirstMondayOfYear (const Year: Word): TDateTime;
var
     StartYear: TDateTime;
begin
     StartYear := GetFirstDayOfYear (Year);
     if ISODayOfWeek (StartYear) = 1 then
          Result := StartYear
     else
          Result := StartOfISOWeek (StartYear) + 7;
end;

function GetFirstMondayOfYear (const Year: Integer): TDateTime;
var
     StartYear: TDateTime;
begin
     StartYear := GetFirstDayOfYear (Year);
     if ISODayOfWeek (StartYear) = 1 then
          Result := StartYear
     else
          Result := StartOfISOWeek (StartYear) + 7;
end;

function GetFirstMondayOfYear (const DT: TDateTime): TDateTime;
begin
     Result := GetFirstMondayOfYear (Date2Year (DT));
end;

function StartOfISOWeekNo (const WeekNo: Integer; const Year: Word): TDateTime;
var
     FirstMonday: TDateTime;
begin
     FirstMonday := GetFirstMondayOfYear (Year);
     if Date2Day (FirstMonday) < 5 then
          Result := AddWeeks (FirstMonday, WeekNo - 1)
     else
          Result := AddWeeks (FirstMonday, WeekNo - 2)
end;

function StartOfISOWeekNo (const WeekNo, Year: Integer): TDateTime;
var
     FirstMonday: TDateTime;
begin
     FirstMonday := GetFirstMondayOfYear (Year);
     if Date2Day (FirstMonday) < 5 then
          Result := AddWeeks (FirstMonday, WeekNo - 1)
     else
          Result := AddWeeks (FirstMonday, WeekNo - 2)
end;

function EndOfISOWeekNo (const WeekNo: Integer; const Year: Word): TDateTime;
begin
     Result := StartOfISOWeekNo (WeekNo, Year) + 6;
end;

function EndOfISOWeekNo (const WeekNo, Year: Integer): TDateTime;
begin
     Result := StartOfISOWeekNo (WeekNo, Year) + 6;
end;

function ISOYWD2Date (const Year: Word; const WeekNo, DOW: Integer): TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := StartOfISOWeekNo (WeekNo, Year) + DOW - 1;
end;

function ISOYWD2Date (const Year, WeekNo, DOW: Integer): TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := StartOfISOWeekNo (WeekNo, Year) + DOW - 1;
end;

function ISOWeeksApart (const DT1, DT2: TDateTime): Integer;
begin
     Result := WeeksApart (StartOfISOWeek (DT1), StartOfISOWeek (DT2));
end;

{--- Boolean Identification ---}

function SameDate (const DT1, DT2: TDateTime): Boolean;
begin
     Result := Int (DT1) = Int (DT2);
end;

function SameTime (const DT1, DT2: TDateTime): Boolean;
begin
     Result := abs (Frac (DT1) - Frac (DT2)) < OneDTMillisecond;
end;

function SameDateTime (const DT1, DT2: TDateTime): Boolean;
begin
     Result := abs (DT1 - DT2) < OneDTMillisecond;
end;

function IsJanuary (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 1;
end;

function IsJanuaryNow: Boolean;
begin
     Result := Date2Month (Date) = 1;
end;

function IsFebruary (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 2;
end;

function IsFebruaryNow: Boolean;
begin
     Result := Date2Month (Date) = 2;
end;

function IsMarch (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 3;
end;

function IsMarchNow: Boolean;
begin
     Result := Date2Month (Date) = 3;
end;

function IsApril (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 4;
end;

function IsAprilNow: Boolean;
begin
     Result := Date2Month (Date) = 4;
end;

function IsMay (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 5;
end;

function IsMayNow: Boolean;
begin
     Result := Date2Month (Date) = 5;
end;

function IsJune (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 6;
end;

function IsJuneNow: Boolean;
begin
     Result := Date2Month (Date) = 6;
end;

function IsJuly (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 7;
end;

function IsJulyNow: Boolean;
begin
     Result := Date2Month (Date) = 7;
end;

function IsAugust (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 8;
end;

function IsAugustNow: Boolean;
begin
     Result := Date2Month (Date) = 8;
end;

function IsSeptember (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 9;
end;

function IsSeptemberNow: Boolean;
begin
     Result := Date2Month (Date) = 9;
end;

function IsOctober (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 10;
end;

function IsOctoberNow: Boolean;
begin
     Result := Date2Month (Date) = 10;
end;

function IsNovember (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 11;
end;

function IsNovemberNow: Boolean;
begin
     Result := Date2Month (Date) = 11;
end;

function IsDecember (const DT: TDateTime): Boolean;
begin
     Result := Date2Month (DT) = 12;
end;

function IsDecemberNow: Boolean;
begin
     Result := Date2Month (Date) = 12;
end;

function IsAM (const DT: TDateTime): Boolean;
begin
     Result := Frac (DT) < 0.5
end;

function IsAMNow: Boolean;
begin
     Result := Frac (Time) < 0.5
end;

function IsPM (const DT: TDateTime): Boolean;
begin
     Result := not IsAM (DT);
end;

function IsPMNow: Boolean;
begin
     Result := not IsAMNow;
end;

function IsNoon (const DT: TDateTime): Boolean;
begin
     Result := Frac (DT) = 0.5;
end;

function IsNoonNow: Boolean;
begin
     Result := Frac (Time) = 0.5;
end;

function IsMidnight (const DT: TDateTime): Boolean;
begin
     Result := Frac (DT) = 0.0;
end;

function IsMidnightNow: Boolean;
begin
     Result := Frac (Time) = 0.0;
end;

function IsSunday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 1;
end;

function IsSundayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 1;
end;

function IsMonday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 2;
end;

function IsMondayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 2;
end;

function IsTuesday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 3;
end;

function IsTuesdayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 3;
end;

function IsWednesday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 4;
end;

function IsWednesdayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 4;
end;

function IsThursday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 5;
end;

function IsThursdayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 5;
end;

function IsFriday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 6;
end;

function IsFridayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 6;
end;

function IsSaturday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) = 7;
end;

function IsSaturdayNow: Boolean;
begin
     Result := DayOfWeek (Date) = 7;
end;

function IsWeekend (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) in [1, 7];
end;

function IsWeekendNow: Boolean;
begin
     Result := DayOfWeek (Date) in [1, 7];
end;

function IsWeekday (const DT: TDateTime): Boolean;
begin
     Result := DayOfWeek (DT) in [2..6];
end;

function IsWeekdayNow: Boolean;
begin
     Result := DayOfWeek (Date) in [2..6];
end;

function IsValidShortMonth (const Month: string): Boolean;
var
     S: string;
     I: Integer;
begin
     Result := False;
     S := AnsiUpperCase (Month);
     for I := 1 to 12 do
          if S = AnsiUpperCase (ShortMonthNames [I]) then
          begin
               Result := True;
               Break
          end;
end;

function IsValidLongMonth (const Month: string): Boolean;
var
     S: string;
     I: Integer;
begin
     Result := False;
     S := AnsiUpperCase (Month);
     for I := 1 to 12 do
          if S = AnsiUpperCase (LongMonthNames [I]) then
          begin
               Result := True;
               Break
          end;
end;

function IsValidShortDOW (const DOW: string): Boolean;
var
     S: string;
     I: Integer;
begin
     Result := False;
     S := AnsiUpperCase (DOW);
     for I := 1 to 12 do
          if S = AnsiUpperCase (ShortDayNames [I]) then
          begin
               Result := True;
               Break
          end;
end;

function IsValidLongDOW (const DOW: string): Boolean;
var
     S: string;
     I: Integer;
begin
     Result := False;
     S := AnsiUpperCase (DOW);
     for I := 1 to 12 do
          if S = AnsiUpperCase (LongDayNames [I]) then
          begin
               Result := True;
               Break
          end;
end;

function IsFirstDayOfMonth (const DT: TDateTime): Boolean;
begin
     Result := Date2Day (DT) = 1;
end;

function IsFirstDayOfMonthNow: Boolean;
begin
     Result := IsFirstDayOfMonth (SysUtils.Date);
end;

function IsLastDayOfMonth (const DT: TDateTime): Boolean;
begin
     Result := Int (DT) = Int (GetLastDayOfMonth (DT));
end;

function IsLastDayOfMonthNow: Boolean;
begin
     Result := IsLastDayOfMonth (SysUtils.Date);
end;

function IsFirstDayOfYear (const DT: TDateTime): Boolean;
begin
     Result := Int (DT) = Int (GetFirstDayOfYear (DT));
end;

function IsFirstDayOfYearNow: Boolean;
begin
     Result := IsFirstDayOfYear (SysUtils.Date);
end;

function IsLastDayOfYear (const DT: TDateTime): Boolean;
begin
     Result := Int (DT) = Int (GetLastDayOfYear (DT));
end;

function IsLastDayOfYearNow: Boolean;
begin
     Result := IsLastDayOfYear (SysUtils.Date);
end;

function DayOfMonth2Date (const DOMType: TESBDOMType; const DOW: Byte;
     const Month, Year: Word): TDateTime;
var
     Ofs: Integer;
     DT: TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     if DOMType < domLast then
     begin
          DT := GetFirstDayOfMonth (Month, Year);
          Ofs := DOW - DayOfWeek (DT);
          if Ofs < 0 then
               Ofs := Ofs + 7;
          Result := DT + Ofs + 7 * Integer (DOMType);
     end
     else
     begin
          DT := GetLastDayOfMonth (Month, Year);
          Ofs := DayofWeek (DT) - DOW;
          if Ofs < 0 then
               Ofs := Ofs + 7;
          Result := DT - Ofs;
     end;
end;

function DayOfMonth2Date (const DOMType: TESBDOMType;
     const DOW, Month, Year: Integer): TDateTime;
var
     Ofs: Integer;
     DT: TDateTime;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     if DOMType < domLast then
     begin
          DT := GetFirstDayOfMonth (Month, Year);
          Ofs := DOW - DayOfWeek (DT);
          if Ofs < 0 then
               Ofs := Ofs + 7;
          Result := DT + Ofs + 7 * Integer (DOMType);
     end
     else
     begin
          DT := GetLastDayOfMonth (Month, Year);
          Ofs := DayofWeek (DT) - DOW;
          if Ofs < 0 then
               Ofs := Ofs + 7;
          Result := DT - Ofs;
     end;
end;

{--- Holiday/Work Day processing ---}

function IsWorkingDay (const DT: TDateTime): Boolean;
var
     DOW: Byte;
     Easter: TDateTime;
begin
     Result := True;
     DOW := DayOfWeek (DT);
     if DOW in NonWorkingDays then
          Result := False
     else
     begin
          if ESBUseChristianHolidays then
          begin
               Easter := GetEasterSunday (DT);
               if SameDate (Easter, DT) or SameDate (Easter - 2, DT) then
                    Result := False
               else if ESBUseEasterSaturday and SameDate (Easter - 1, DT) then
                    Result := False
               else if ESBUseEasterMonday and SameDate (Easter + 1, DT) then
                    Result := False
               else if SameDate (GetChristmasDay (DT), DT) then
                    Result := False
          end;
     end;
end;

function Date2AccessStr (const DT: TDateTime): string;
var
     Day, Month, Year: Integer;
begin
     try
          OptDecodeDateI (DT, Year, Month, Day);
          Result := '#' + Int2ZStr (Month, 2) + '/' + Int2ZStr (Day, 2)
               + '/' + Int2ZStr (Year, 4) + '#';
     except
          Result := '';
     end;
end;

function Date2ANSISQLStr (const DT: TDateTime): string;
var
     Day, Month, Year: Integer;
begin
     try
          OptDecodeDateI (DT, Year, Month, Day);
          Result := '{ d ''' + Int2ZStr (Year, 4) + '-' + Int2ZStr (Month, 2)
               + '-' + Int2ZStr (Day, 2) + ''' }';
     except
          Result := '';
     end;
end;

function Days2Hrs (const Value: Extended): Extended;
begin
     Result := Value * HrsPerDay;
end;

function Hrs2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerHr;
end;

function Mins2Hrs (const Value: Extended): Extended;
begin
     Result := Value * HrsPerMin;
end;

function Hrs2Mins (const Value: Extended): Extended;
begin
     Result := Value * MinsPerHr;
end;

function Mins2Secs (const Value: Extended): Extended;
begin
     Result := Value * SecsPerMin;
end;

function Secs2Mins (const Value: Extended): Extended;
begin
     Result := Value * MinsPerSec;
end;

function Hrs2Secs (const Value: Extended): Extended;
begin
     Result := Value * SecsPerHr;
end;

function Secs2Hrs (const Value: Extended): Extended;
begin
     Result := Value * HrsPerSec;
end;

function Days2Secs (const Value: Extended): Extended;
begin
     Result := Value * SecsPerDay;
end;

function Secs2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerSec;
end;

function Days2Mins (const Value: Extended): Extended;
begin
     Result := Value * MinsPerDay;
end;

function Mins2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerMin;
end;

function Days2Weeks (const Value: Extended): Extended;
begin
     Result := Value * WeeksPerDay;
end;

function Weeks2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerWeek;
end;

function Days2Fortnights (const Value: Extended): Extended;
begin
     Result := Value * FortnightsPerDay;
end;

function Fortnights2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerFortnight;
end;

function Days2Months (const Value: Extended): Extended;
begin
     Result := Value / DaysPerSynodicMonth;
end;

function Months2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerSynodicMonth;
end;

function Days2Years (const Value: Extended): Extended;
begin
     Result := Value / DaysPerTropicalYear;
end;

function Years2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerTropicalYear;
end;

function Days2YearsGregorian (const Value: Extended): Extended;
begin
     Result := Value / DaysPerGregorianYear;
end;

function YearsGregorian2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerGregorianYear;
end;

function Days2YearsJulian (const Value: Extended): Extended;
begin
     Result := Value / DaysPerJulianYear;
end;

function YearsJulian2Days (const Value: Extended): Extended;
begin
     Result := Value * DaysPerJulianYear;
end;

function DOWsInMonth (const DOW: Byte; const Month, Year: Word): Integer;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     Result := DOWsInRange (DOW, GetFirstDayOfMonth (Month, Year), GetLastDayOfMonth (Month, Year));
end;

function DOWsInRange (const DOW: Byte; const StartDT, EndDT: TDateTime): Integer;
var
     DT1, DT2: TDateTime;
     StartDOW, Days, Weeks, Extras: Integer;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     DT1 := StartDT;
     DT2 := EndDT;
     if DT2 < DT1 then
          SwapXY (DT1, DT2);
     Days := Trunc (DT2) - Trunc (DT1) + 1;
     Weeks := Days div 7;
     Extras := Days mod 7;
     StartDOW := DayOfWeek (DT1);

     if (Integer (DOW) - StartDOW + 7) mod 7 < Extras then
          Result := Weeks + 1
     else
          Result := Weeks;
end;

function DOWsInRange (const DOW: TESBDayofWeek; const StartDT, EndDT: TDateTime): Integer;
begin
     Result := DOWsInRange (Integer (DOW) + 1, StartDT, EndDT);
end;

function WeekEndDaysInMonth (const Month, Year: Word): Integer;
begin
     Result := DOWsInMonth (1, Month, Year) + DOWsInMonth (7, Month, Year)
end;

function WeekDaysInMonth (const Month, Year: Word): Integer;
begin
     Result := DaysInMonth (Month, Year) - (DOWsInMonth (1, Month, Year)
          + DOWsInMonth (7, Month, Year))
end;

function DOW2ESBDayOfWeek (const DOW: Byte): TESBDayOfWeek;
begin
     if (DOW < 1) or (DOW > 7) then
          raise EConvertError.Create (rsInvalidDOW);

     Result := TESBDayOfWeek (DOW - 1);
end;

function ESBDayOfWeek2DOW (const DOW: TESBDayOfWeek): Integer;
begin
     Result := Integer (DOW) + 1;
end;

function ISODOW2ESBDayOfWeek (const DOW: Byte): TESBDayOfWeek;
begin
     Result := DOW2ESBDayOfWeek (ISODOW2DOW (DOW));
end;

function ESBDayOfWeek2ISODOW (const DOW: TESBDayOfWeek): Integer;
begin
     Result := DOW2ISODOW (ESBDayOfWeek2DOW (DOW));
end;

function Month2ESBMonth (const Month: Word): TESBMonth;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     Result := TESBMonth (Month - 1);
end;

function Month2ESBMonth (const Month: Integer): TESBMonth;
begin
     if (Month < 1) or (Month > 12) then
          raise EConvertError.Create (rsInvalidMonth);

     Result := TESBMonth (Month - 1);
end;

function ESBMonth2Month (const Month: TESBMonth): Integer;
begin
     Result := Integer (Month) + 1;
end;

function Date2StarSign (const DT: TDateTime): string;
var
     Year, Month, Day: Integer;
     Ofs: Integer;
begin
     OptDecodeDateI (DT, Year, Month, Day);
     case Month * 100 + Day of
          0101..0119: Ofs := 12; // Capricorn
          0120..0218: Ofs := 1; // Aquarius
          0219..0320: Ofs := 2; // Pisces
          0321..0419: Ofs := 3; // Aries
          0420..0520: Ofs := 4; // Taurus
          0521..0621: Ofs := 5; // Gemini
          0622..0722: Ofs := 6; // Cancer
          0723..0822: Ofs := 7; // Leo
          0823..0922: Ofs := 8; // Virgo
          0923..1022: Ofs := 9; // Libra
          1023..1121: Ofs := 10; // Scorpio
          1122..1221: Ofs := 11; // Sagittarius
          1222..1231: Ofs := 12; // Capricorn
     else
          Ofs := 0;
     end;

     if Ofs in [1..12] then
          Result := ESBStarSigns [Ofs]
     else
          Result := '';
end;

function Days2SiderealDays (const Days: Extended): Extended;
begin
     Result := Days * 24 / HrsPerSiderealDay;
end;

function SiderealDays2Days (const Days: Extended): Extended;
begin
     Result := Days * HrsPerSiderealDay / 24;
end;

end.
