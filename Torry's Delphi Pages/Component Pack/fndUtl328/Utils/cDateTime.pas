{$INCLUDE ..\cDefines.inc}
unit cDateTime;

{                                                                              }
{                         DateTime functions v3.09                             }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cDateTime.pas                    }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Notes:                                                                       }
{   A good source of information on calendars is the FAQ ABOUT CALENDARS,      }
{   available at http://www.tondering.dk/claus/calendar.html                   }
{                                                                              }
{   Note the following (and more) is available in SysUtils:                    }
{     Function IsLeapYear (Year : Word) : Boolean                              }
{       (1 = Sunday .. 7 = Saturday)                                           }
{     Function EncodeDate (Year, Month, Day : Word) : TDateTime;               }
{     Procedure DecodeDate (D : DateTime; var Year, Month, Day : Word);        }
{     var ShortDayNames, LongDayNames, ShortMonthNames, LongMonthNames : Array }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   1999/11/10  0.01  Initial version from scratch. Add functions. DayOfYear.  }
{   1999/11/21  0.02  EasterSunday function. Diff functions. ISOInteger.       }
{   2000/03/04  1.03  Moved RFC functions to cInternetStandards.               }
{   2000/03/05  1.04  Added Time Zone functions from cInternetStandards.       }
{   2000/05/03  1.05  Added ISO Week functions, courtesy of Martin Boonstra    }
{                     <m.boonstra@imn.nl>                                      }
{   2000/08/16  1.06  Fixed bug in GMTBias reported by Gerhard Steinwedel      }
{                     <steinwedel@gmx.de>                                      }
{   2001/12/22  2.07  Added RFC DateTime functions from cInternetStandards.    }
{   2002/01/10  3.08  Fixed bug with negative values in AddMonths as           }
{                     reported by Michael Valentiner <MichaelVB@gmx.de>        }
{   2004/02/22  3.09  Fixed bug in RFCDateTimeToGMTDateTime.                   }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils;

const
  UnitName      = 'cDateTime';
  UnitVersion   = '3.09';
  UnitDesc      = 'Date/Time functions';
  UnitCopyright = 'Copyright (c) 1999-2004 David J Butler';



{                                                                              }
{ Exception                                                                    }
{                                                                              }
type
  EDateTime = class(Exception);



{                                                                              }
{ Decoding                                                                     }
{                                                                              }
{$IFNDEF DELPHI6_UP}
procedure DecodeDateTime(const DateTime: TDateTime;
          var Year, Month, Day, Hour, Minute, Second, Millisecond: Word);
{$ENDIF}
function  Century(const D: TDateTime): Word;
function  Year(const D: TDateTime): Word;
function  Month(const D: TDateTime): Word;
function  Day(const D: TDateTime): Word;
function  Hour(const D: TDateTime): Word;
function  Minute(const D: TDateTime): Word;
function  Second(const D: TDateTime): Word;
function  Millisecond(const D: TDateTime): Word;

const
  OneDay         = 1.0;
  OneWeek        = OneDay * 7;
  OneHour        = OneDay / 24.0;
  OneMinute      = OneHour / 60.0;
  OneSecond      = OneMinute / 60.0;
  OneMillisecond = OneSecond / 1000.0;



{                                                                              }
{ Encoding                                                                     }
{                                                                              }
{$IFNDEF DELPHI6_UP}
function  EncodeDateTime(const Year, Month, Day, Hour, Minute, Second, Millisecond: Word): TDateTime;
{$ENDIF}
procedure SetYear(var D: TDateTime; const Year: Word);
procedure SetMonth(var D: TDateTime; const Month: Word);
procedure SetDay(var D: TDateTime; const Day: Word);
procedure SetHour(var D: TDateTime; const Hour: Word);
procedure SetMinute(var D: TDateTime; const Minute: Word);
procedure SetSecond(var D: TDateTime; const Second: Word);
procedure SetMillisecond(var D: TDateTime; const Milliseconds: Word);



{                                                                              }
{ Comparison                                                                   }
{                                                                              }
function  IsEqual(const D1, D2: TDateTime): Boolean; overload;
function  IsEqual(const D1: TDateTime; const Ye, Mo, Da: Word): Boolean; overload;
function  IsEqual(const D1: TDateTime; const Ho, Mi, Se, ms: Word): Boolean; overload;
function  IsAM(const D: TDateTime): Boolean;
function  IsPM(const D: TDateTime): Boolean;
function  IsMidnight(const D: TDateTime): Boolean;
function  IsNoon(const D: TDateTime): Boolean;
function  IsSunday(const D: TDateTime): Boolean;
function  IsMonday(const D: TDateTime): Boolean;
function  IsTuesday(const D: TDateTime): Boolean;
function  IsWedneday(const D: TDateTime): Boolean;
function  IsThursday(const D: TDateTime): Boolean;
function  IsFriday(const D: TDateTime): Boolean;
function  IsSaturday(const D: TDateTime): Boolean;
function  IsWeekend(const D: TDateTime): Boolean;



{                                                                              }
{ Relative date/times                                                          }
{                                                                              }
function  Noon(const D: TDateTime): TDateTime;
function  Midnight(const D: TDateTime): TDateTime;
function  FirstDayOfMonth(const D: TDateTime): TDateTime;
function  LastDayOfMonth(const D: TDateTime): TDateTime;
function  NextWorkday(const D: TDateTime): TDateTime;
function  PreviousWorkday(const D: TDateTime): TDateTime;
function  FirstDayOfYear(const D: TDateTime): TDateTime;
function  LastDayOfYear(const D: TDateTime): TDateTime;
function  EasterSunday(const Year: Word): TDateTime;
function  GoodFriday(const Year: Word): TDateTime;

function  AddMilliseconds(const D: TDateTime; const N: Int64): TDateTime;
function  AddSeconds(const D: TDateTime; const N: Int64): TDateTime;
function  AddMinutes(const D: TDateTime; const N: Integer): TDateTime;
function  AddHours(const D: TDateTime; const N: Integer): TDateTime;
function  AddDays(const D: TDateTime; const N: Integer): TDateTime;
function  AddWeeks(const D: TDateTime; const N: Integer): TDateTime;
function  AddMonths(const D: TDateTime; const N: Integer): TDateTime;
function  AddYears(const D: TDateTime; const N: Integer): TDateTime;



{                                                                              }
{ Counting                                                                     }
{                                                                              }
{   DayOfYear and WeekNumber start at 1.                                       }
{   WeekNumber is not the ISO week number but the week number where week one   }
{     starts at Jan 1.                                                         }
{   For reference: ISO standard 8601:1988 - (European Standard EN 28601).      }
{     "It states that a week is identified by its number in a given year.      }
{      A week begins with a Monday (day 1) and ends with a Sunday (day 7).     }
{      The first week of a year is the one which includes the first Thursday   }
{      (day 4), or equivalently the one which includes January 4.              }
{      In other words, the first week of a new year is the week that has the   }
{      majority of its days in the new year."                                  }
{   ISOFirstWeekOfYear returns the start date (Monday) of the first ISO week   }
{     of a year (may be in the previous year).                                 }
{   ISOWeekNumber returns the ISO Week number and the year to which the week   }
{     number applies.                                                          }
{                                                                              }
function  DayOfYear(const Ye, Mo, Da: Word): Integer; overload;
function  DayOfYear(const D: TDateTime): Integer; overload;
function  DaysInMonth(const Ye, Mo: Word): Integer; overload;
function  DaysInMonth(const D: TDateTime): Integer; overload;
function  DaysInYear(const Ye: Word): Integer;
function  DaysInYearDate(const D: TDateTime): Integer;
function  WeekNumber(const D: TDateTime): Integer;
function  ISOFirstWeekOfYear(const Ye: Word): TDateTime;
procedure ISOWeekNumber(const D: TDateTime; var WeekNumber, WeekYear: Word);
function  DateTimeAsISO8601String(const D: TDateTime): String;
function  ISO8601StringAsDateTime(const D: String): TDateTime;



{                                                                              }
{ Difference                                                                   }
{   Returns difference between two dates (D2 - D1).                            }
{                                                                              }
function  DiffMilliseconds(const D1, D2: TDateTime): Int64;
function  DiffSeconds(const D1, D2: TDateTime): Integer;
function  DiffMinutes(const D1, D2: TDateTime): Integer;
function  DiffHours(const D1, D2: TDateTime): Integer;
function  DiffDays(const D1, D2: TDateTime): Integer;
function  DiffWeeks(const D1, D2: TDateTime): Integer;
function  DiffMonths(const D1, D2: TDateTime): Integer;
function  DiffYears(const D1, D2: TDateTime): Integer;



{                                                                              }
{ Time Zone                                                                    }
{   Uses systems regional settings to convert between local and GMT time.      }
{                                                                              }
function  GMTTimeToLocalTime(const D: TDateTime): TDateTime;
function  LocalTimeToGMTTime(const D: TDateTime): TDateTime;
function  NowAsGMTTime: TDateTime;



{                                                                              }
{ Conversions                                                                  }
{                                                                              }
{   ANSI Integer is an integer in the format YYYYDDD (where DDD = day number)  }
{   ISO-8601 Integer date is an integer in the format YYYYMMDD.                }
{   TropicalYear is the time for one orbit of the earth around the sun.        }
{   TwoDigitYearToYear returns the full year number given a two digit year.    }
{   SynodicMonth is the time between two full moons.                           }
{                                                                              }
function  DateTimeToANSI(const D: TDateTime): Integer;
function  ANSIToDateTime(const Julian: Integer): TDateTime;
function  DateTimeToISOInteger(const D: TDateTime): Integer;
function  DateTimeToISO(const D: TDateTime): String;
function  ISOIntegerToDateTime(const ISOInteger: Integer): TDateTime;
function  TwoDigitRadix2000YearToYear(const Y: Integer): Integer;
function  DateTimeAsElapsedTime(const D: TDateTime;
          const IncludeMilliseconds: Boolean = False): String;



{                                                                              }
{ RFC DateTimes                                                                }
{                                                                              }
{   RFC1123 DateTime is the preferred representation on the Internet for all   }
{   DateTime values.                                                           }
{   Use DateTimeToRFCDateTime to convert local time to RFC1123 DateTime.       }
{   Use RFCDateTimeToDateTime to convert RFC DateTime formats to local time.   }
{   Returns 0.0 if not a recognised RFC DateTime.                              }
{   See RFC822, RFC850, RFC1123, RFC1036, RFC1945.                             }
{                                                                              }
{ From RFC 822 (Standard for the format of ARPA INTERNET Text Messages):       }
{    "time        =  hour zone                      ; ANSI and Military        }
{     hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT] ; 00:00:00 - 23:59:59      }
{     zone        =  "UT"  / "GMT"                  ; Universal Time           }
{                                                   ; North American : UT      }
{                 /  "EST" / "EDT"                  ;  Eastern:  - 5/ - 4      }
{                 /  "CST" / "CDT"                  ;  Central:  - 6/ - 5      }
{                 /  "MST" / "MDT"                  ;  Mountain: - 7/ - 6      }
{                 /  "PST" / "PDT"                  ;  Pacific:  - 8/ - 7      }
{                 /  1ALPHA                         ; Military: Z = UT;        }
{                                                   ;  A:-1; (J not used)      }
{                                                   ;  M:-12; N:+1; Y:+12      }
{                 / ( ("+" / "-") 4DIGIT )          ; Local differential       }
{                                                   ;  hours+min. (HHMM)       }
{     date-time   =  [ day "," ] date time          ; dd mm yy                 }
{                                                   ;  hh:mm:ss zzz            }
{     day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"                          }
{                 /  "Fri"  / "Sat" /  "Sun"                                   }
{     date        =  1*2DIGIT month 2DIGIT        ; day month year             }
{                                                 ;  e.g. 20 Jun 82            }
{     month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"                        }
{                 /  "May"  /  "Jun" /  "Jul"  /  "Aug"                        }
{                 /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"                    "   }
{                                                                              }
{ Note that even though RFC 822 states hour=2DIGIT":"2DIGIT, none of the       }
{   examples given in the appendix include the ":",                            }
{   for example: "26 Aug 76 1429 EDT"                                          }
{                                                                              }
{                                                                              }
{ From RFC 1036 (Standard for Interchange of USENET Messages):                 }
{                                                                              }
{   "Its format must be acceptable both in RFC-822 and to the getdate(3)       }
{    routine that is provided with the Usenet software.   ...                  }
{    One format that is acceptable to both is:                                 }
{                                                                              }
{                      Wdy, DD Mon YY HH:MM:SS TIMEZONE                        }
{                                                                              }
{    Note in particular that ctime(3) format:                                  }
{                                                                              }
{                          Wdy Mon DD HH:MM:SS YYYY                            }
{                                                                              }
{    is not acceptable because it is not a valid RFC-822 date.  However,       }
{    since older software still generates this format, news                    }
{    implementations are encouraged to accept this format and translate        }
{    it into an acceptable format.                                         "   }
{                                                                              }
{   "Here is an example of a message in the old format (before the             }
{    existence of this standard). It is recommended that                       }
{    implementations also accept messages in this format to ease upward        }
{    conversion.                                                               }
{                                                                              }
{               Posted: Fri Nov 19 16:14:55 1982                           "   }
{                                                                              }
{                                                                              }
{ From RFC 1945 (Hypertext Transfer Protocol -- HTTP/1.0)                      }
{                                                                              }
{  "HTTP/1.0 applications have historically allowed three different            }
{   formats for the representation of date/time stamps:                        }
{                                                                              }
{       Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123        }
{       Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036      }
{       Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() format           }
{                                                                              }
{   The first format is preferred as an Internet standard and represents       }
{   a fixed-length subset of that defined by RFC 1123 [6] (an update to        }
{   RFC 822 [7]). The second format is in common use, but is based on the      }
{   obsolete RFC 850 [10] date format and lacks a four-digit year.             }
{   HTTP/1.0 clients and servers that parse the date value should accept       }
{   all three formats, though they must never generate the third               }
{   (asctime) format.                                                          }
{                                                                              }
{      Note: Recipients of date values are encouraged to be robust in          }
{      accepting date values that may have been generated by non-HTTP          }
{      applications, as is sometimes the case when retrieving or posting       }
{      messages via proxies/gateways to SMTP or NNTP.                       "  }
{                                                                              }
{  "All HTTP/1.0 date/time stamps must be represented in Universal Time        }
{   (UT), also known as Greenwich Mean Time (GMT), without exception.          }
{                                                                              }
{       HTTP-date      = rfc1123-date | rfc850-date | asctime-date             }
{                                                                              }
{       rfc1123-date   = wkday "," SP date1 SP time SP "GMT"                   }
{       rfc850-date    = weekday "," SP date2 SP time SP "GMT"                 }
{       asctime-date   = wkday SP date3 SP time SP 4DIGIT                      }
{                                                                              }
{       date1          = 2DIGIT SP month SP 4DIGIT                             }
{                        ; day month year (e.g., 02 Jun 1982)                  }
{       date2          = 2DIGIT "-" month "-" 2DIGIT                           }
{                        ; day-month-year (e.g., 02-Jun-82)                    }
{       date3          = month SP ( 2DIGIT | ( SP 1DIGIT ))                    }
{                        ; month day (e.g., Jun  2)                            }
{                                                                              }
{       time           = 2DIGIT ":" 2DIGIT ":" 2DIGIT                          }
{                        ; 00:00:00 - 23:59:59                                 }
{                                                                              }
{       wkday          = "Mon" | "Tue" | "Wed"                                 }
{                      | "Thu" | "Fri" | "Sat" | "Sun"                         }
{                                                                              }
{       weekday        = "Monday" | "Tuesday" | "Wednesday"                    }
{                      | "Thursday" | "Friday" | "Saturday" | "Sunday"         }
{                                                                              }
{       month          = "Jan" | "Feb" | "Mar" | "Apr"                         }
{                      | "May" | "Jun" | "Jul" | "Aug"                         }
{                      | "Sep" | "Oct" | "Nov" | "Dec"                      "  }
{                                                                              }
function  RFC850DayOfWeek(const S: String): Integer;
function  RFC1123DayOfWeek(const S: String): Integer;
function  RFCMonth(const S: String): Word;

function  GMTTimeToRFC1123Time(const D: TDateTime;
          const IncludeSeconds: Boolean = False): String;
function  GMTDateTimeToRFC1123DateTime(const D: TDateTime;
          const IncludeDayOfWeek: Boolean = True): String;
function  DateTimeToRFCDateTime(const D: TDateTime): String;
function  NowAsRFCDateTime: String;

function  RFCDateTimeToGMTDateTime(const S: String): TDateTime;
function  RFCDateTimeToDateTime(const S: String): TDateTime;

function  RFCTimeZoneToGMTBias(const Zone: String): Integer;



{                                                                              }
{ High-precision timing                                                        }
{                                                                              }
{   StartTimer returns an encoded time (running timer).                        }
{   StopTimer returns an encoded elapsed time (stopped timer).                 }
{   ResumeTimer returns an encoded time (running timer), given an encoded      }
{     elapsed time (stopped timer).                                            }
{   StoppedTimer returns an encoded elapsed time of zero, ie a stopped timer   }
{     with no time elapsed.                                                    }
{   MillisecondsElapsed returns the time elapsed, given a running or a stopped }
{     Timer.                                                                   }
{   Times are encoded in CPU clock cycles.                                     }
{   CPU clock frequency returns the number of CPU clock cycles per second.     }
{                                                                              }
type
  THPTimer = Int64;

function  StartTimer: THPTimer;
procedure StopTimer(var Timer: THPTimer);
procedure ResumeTimer(var StoppedTimer: THPTimer);
function  StoppedTimer: THPTimer;
function  ElapsedTimer(const Milliseconds: Integer): THPTimer;
function  MillisecondsElapsed(const Timer: THPTimer;
          const TimerRunning: Boolean = True): Integer;
function  MicrosecondsElapsed(const Timer: THPTimer;
          const TimerRunning: Boolean = True): Integer;
function  CPUClockFrequency: Int64;
procedure DelayMicroSeconds(const MicroSeconds: Integer);



const
  TropicalYear = 365.24219 * OneDay;  // 365 days, 5 hr, 48 min, 46 sec
  SynodicMonth = 29.53059 * OneDay;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function  TimePeriodStr(const D: TDateTime): String;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation



uses
  { Delphi }
  Windows,
  {$IFDEF DELPHI6_UP}
  DateUtils,
  {$ENDIF}

  { Fundamentals }
  cUtils,
  cStrings;



{                                                                              }
{ Decoding                                                                     }
{                                                                              }
function Century(const D: TDateTime): Word;
begin
  Result := Year(D) div 100;
end;

function Year(const D: TDateTime): Word;
var Mo, Da : Word;
begin
  DecodeDate(D, Result, Mo, Da);
end;

function Month(const D: TDateTime): Word;
var Ye, Da : Word;
begin
  DecodeDate(D, Ye, Result, Da);
end;

function Day(const D: TDateTime): Word;
var Ye, Mo : Word;
begin
  DecodeDate(D, Ye, Mo, Result);
end;

function Hour(const D: TDateTime): Word;
var Mi, Se, MS : Word;
begin
  DecodeTime(D, Result, Mi, Se, MS);
end;

function Minute(const D: TDateTime): Word;
var Ho, Se, MS : Word;
begin
  DecodeTime(D, Ho, Result, Se, MS);
end;

function Second(const D: TDateTime): Word;
var Ho, Mi, MS : Word;
begin
  DecodeTime(D, Ho, Mi, Result, MS);
end;

function Millisecond(const D: TDateTime): Word;
var Ho, Mi, Se : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Result);
end;

{$IFNDEF DELPHI6_UP}
procedure DecodeDateTime(const DateTime: TDateTime; var Year, Month, Day, Hour, Minute, Second, Millisecond : Word);
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, Millisecond);
end;

function EncodeDateTime(const Year, Month, Day, Hour, Minute, Second, Millisecond: Word): TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) +
            EncodeTime(Hour, Minute, Second, Millisecond);
end;
{$ENDIF}




{                                                                              }
{ Encoding                                                                     }
{                                                                              }
procedure SetYear(var D: TDateTime; const Year: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Year, Mo, Da, Ho, Mi, Se, Ms);
end;

procedure SetMonth(var D: TDateTime; const Month: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Month, Da, Ho, Mi, Se, Ms);
end;

procedure SetDay(var D: TDateTime; const Day: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Day, Ho, Mi, Se, Ms);
end;

procedure SetHour(var D: TDateTime; const Hour: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Hour, Mi, Se, Ms);
end;

procedure SetMinute(var D: TDateTime; const Minute: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Minute, Se, Ms);
end;

procedure SetSecond(var D: TDateTime; const Second: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Second, Ms);
end;

procedure SetMillisecond(var D: TDateTime; const Milliseconds: Word);
var Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
begin
  DecodeDateTime(D, Ye, Mo, Da, Ho, Mi, Se, Ms);
  D := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Se, Milliseconds);
end;



{                                                                              }
{ Comparison                                                                   }
{                                                                              }
function IsEqual(const D1, D2: TDateTime): Boolean;
begin
  Result := Abs(D1 - D2) < OneMillisecond;
end;

function IsEqual(const D1: TDateTime; const Ye, Mo, Da: Word): Boolean;
var Ye1, Mo1, Da1 : Word;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  Result := (Da = Da1) and (Mo = Mo1) and (Ye = Ye1);
end;

function IsEqual(const D1: TDateTime; const Ho, Mi, Se, ms: Word): Boolean;
var Ho1, Mi1, Se1, ms1 : Word;
begin
  DecodeTime(D1, Ho1, Mi1, Se1, ms1);
  Result := (ms = ms1) and (Se = Se1) and (Mi = Mi1) and (Ho = Ho1);
end;

function IsAM(const D: TDateTime): Boolean;
begin
  Result := Frac(D) < 0.5;
end;

function IsPM(const D: TDateTime): Boolean;
begin
  Result := Frac(D) >= 0.5;
end;

function IsNoon(const D: TDateTime): Boolean;
begin
  Result := Abs(Frac(D) - 0.5) < OneMillisecond;
end;

function IsMidnight(const D: TDateTime): Boolean;
var T : Extended;
begin
  T := Frac(D);
  Result := (T < OneMillisecond) or (T > 1.0 - OneMillisecond);
end;

function IsSunday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 1;
end;

function IsMonday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 2;
end;

function IsTuesday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 3;
end;

function IsWedneday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 4;
end;

function IsThursday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 5;
end;

function IsFriday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 6;
end;

function IsSaturday(const D: TDateTime): Boolean;
begin
  Result := DayOfWeek(D) = 7;
end;

function IsWeekend(const D: TDateTime): Boolean;
begin
  Result := Byte(DayOfWeek(D)) in [1, 7];
end;



{                                                                              }
{ Relative calculations                                                        }
{                                                                              }
function Noon(const D: TDateTime): TDateTime;
begin
  Result := Int(D) + 0.5 * OneDay;
end;

function Midnight(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, Da);
end;

function NextWorkday(const D: TDateTime): TDateTime;
begin
  Case DayOfWeek(D) of
    1..5 : Result := Trunc(D) + OneDay;       // 1..5 Sun..Thu
    6    : Result := Trunc(D) + 3 * OneDay;   // 6    Fri
  else
    Result := Trunc(D) + 2 * OneDay;          // 7    Sat
  end;
end;

function PreviousWorkday(const D: TDateTime): TDateTime;
begin
  Case DayOfWeek(D) of
    1 : Result := Trunc(D) - 2 * OneDay;  // 1    Sun
    2 : Result := Trunc(D) - 3 * OneDay;  // 2    Mon
  else
    Result := Trunc(D) - OneDay;          // 3..7 Tue-Sat
  end;
end;

function LastDayOfMonth(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, Word(DaysInMonth(Ye, Mo)));
end;

function FirstDayOfMonth(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, Mo, 1);
end;

function LastDayOfYear(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, 12, 31);
end;

function FirstDayOfYear(const D: TDateTime): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := EncodeDate(Ye, 1, 1);
end;

{ This algorithm comes from http://www.tondering.dk/claus/calendar.html:       }
{ " This algorithm is based in part on the algorithm of Oudin (1940) as        }
{   quoted in "Explanatory Supplement to the Astronomical Almanac",            }
{   P. Kenneth Seidelmann, editor.                                             }
{   People who want to dig into the workings of this algorithm, may be         }
{   interested to know that                                                    }
{     G is the Golden Number-1                                                 }
{     H is 23-Epact (modulo 30)                                                }
{     I is the number of days from 21 March to the Paschal full moon           }
{     J is the weekday for the Paschal full moon (0=Sunday, 1=Monday,etc.)     }
{     L is the number of days from 21 March to the Sunday on or before         }
{       the Paschal full moon (a number between -6 and 28) "                   }
function EasterSunday(const Year: Word): TDateTime;
var C, I, J, H, G, L : Integer;
    D, M : Word;
begin
  G := Year mod 19;
  C := Year div 100;
  H := (C - C div 4 - (8 * C + 13) div 25 + 19 * G + 15) mod 30;
  I := H - (H div 28) * (1 - (H div 28) * (29 div (H + 1)) * ((21 - G) div 11));
  J := (Year + Year div 4 + I + 2 - C + C div 4) mod 7;
  L := I - J;
  M := 3 + (L + 40) div 44;
  D := L + 28 - 31 * (M div 4);
  Result := EncodeDate(Year, M, D);
end;

function GoodFriday(const Year: Word): TDateTime;
begin
  Result := EasterSunday(Year) - 2 * OneDay;
end;

function AddMilliseconds(const D: TDateTime; const N: Int64): TDateTime;
begin
  Result := D + OneMillisecond * N;
end;

function AddSeconds(const D: TDateTime; const N: Int64): TDateTime;
begin
  Result := D + OneSecond * N;
end;

function AddMinutes(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + OneMinute * N;
end;

function AddHours(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + OneHour * N;
end;

function AddDays(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + N;
end;

function AddWeeks(const D: TDateTime; const N: Integer): TDateTime;
begin
  Result := D + N * 7 * OneDay;
end;

function AddMonths(const D: TDateTime; const N: Integer): TDateTime;
var Ye, Mo, Da : Word;
    IMo : Integer;
begin
  DecodeDate(D, Ye, Mo, Da);
  Inc(Ye, N div 12);
  IMo := Mo;
  Inc(IMo, N mod 12);
  if IMo > 12 then
    begin
      Dec(IMo, 12);
      Inc(Ye);
    end else
    if IMo < 1 then
      begin
        Inc(IMo, 12);
        Dec(Ye);
      end;
  Mo := Word(IMo);
  Da := Word(MinI(Da, DaysInMonth(Ye, Mo)));
  Result := EncodeDate(Ye, Mo, Da) + Frac(D);
end;

function AddYears(const D: TDateTime; const N: Integer): TDateTime;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Inc(Ye, N);
  Da := Word(MinI(Da, DaysInMonth(Ye, Mo)));
  Result := EncodeDate(Ye, Mo, Da);
end;




{                                                                              }
{ Counting                                                                     }
{                                                                              }
const
  DaysInNonLeapMonth : Array[1..12] of Integer = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  CumDaysInNonLeapMonth : Array[1..12] of Integer = (
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

function DayOfYear(const Ye, Mo, Da: Word): Integer; overload;
begin
  Result := CumDaysInNonLeapMonth[Mo] + Da;
  if (Mo > 2) and IsLeapYear(Ye) then
    Inc(Result);
end;

function DayOfYear(const D: TDateTime): Integer; overload;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DayOfYear(Ye, Mo, Da);
end;

function DaysInMonth(const Ye, Mo: Word): Integer;
begin
  Result := DaysInNonLeapMonth[Mo];
  if (Mo = 2) and IsLeapYear(Ye) then
    Inc(Result);
end;

function DaysInMonth(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DaysInMonth(Ye, Mo);
end;

function DaysInYear(const Ye: Word): Integer;
begin
  if IsLeapYear(Ye) then
    Result := 366
  else
    Result := 365;
end;

function DaysInYearDate(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := DaysInYear(Ye);
end;

function WeekNumber(const D: TDateTime): Integer;
begin
  Result := (DiffDays(FirstDayOfYear(D), D) div 7) + 1;
end;

{ ISO Week functions courtesy of Martin Boonstra (m.boonstra@imn.nl)           }
function ISOFirstWeekOfYear(const Ye: Word): TDateTime;
const WeekStartOffset: Array[1..7] of Integer = (1, 0, -1, -2, -3, 3, 2);
            // Weekday  Start of ISO week 1 is
            //  1 Su          02-01-Year
            //  2 Mo          01-01-Year
            //  3 Tu          31-12-(Year-1)
            //  4 We          30-12-(Year-1)
            //  5 Th          29-12-(Year-1)
            //  6 Fr          04-01-Year
            //  7 Sa          03-01-Year
begin
  // Adjust with an offset from 01-01-Ye
  Result := EncodeDate(Ye, 1, 1);
  Result := AddDays(Result, WeekStartOffset[DayOfWeek(Result)]);
end;

procedure ISOWeekNumber(const D: TDateTime; var WeekNumber, WeekYear : Word);
var Ye : Word;
    ISOFirstWeekOfPrevYear,
    ISOFirstWeekOfCurrYear,
    ISOFirstWeekOfNextYear : TDateTime;
begin
  { 3 cases:                                                       }
  {   1: D < ISOFirstWeekOfCurrYear                                }
  {       D lies in week 52/53 of previous year                    }
  {   2: ISOFirstWeekOfCurrYear <= D < ISOFirstWeekOfNextYear      }
  {       D lies in week N (1..52/53) of this year                 }
  {   3: D >= ISOFirstWeekOfNextYear                               }
  {       D lies in week 1 of next year                            }
  Ye := Year(D);
  ISOFirstWeekOfCurrYear := ISOFirstWeekOfYear(Ye);
  if D >= ISOFirstWeekOfCurrYear then
    begin
      ISOFirstWeekOfNextYear := ISOFirstWeekOfYear(Ye + 1);
      if (D < ISOFirstWeekOfNextYear) then
        begin // case 2
          WeekNumber := DiffDays(ISOFirstWeekOfCurrYear, D) div 7 + 1;
          WeekYear := Ye;
        end else
        begin // case 3
          WeekNumber := 1;
          WeekYear := Ye + 1;
        end;
    end else
    begin // case 1
      ISOFirstWeekOfPrevYear := ISOFirstWeekOfYear(Ye - 1);
      WeekNumber := DiffDays(ISOFirstWeekOfPrevYear, D) div 7 + 1;
      WeekYear := Ye - 1;
    end;
end;

function DateTimeAsISO8601String(const D: TDateTime): String;
begin
  Result := FormatDateTime('yyyymmdd', D) + 'T' + FormatDateTime('hh:nn:ss', D);
end;

function ISO8601StringAsDateTime(const D: String): TDateTime;
var Date, Time : String;
    Ye, Mo, Da : Word;
begin
  StrSplitAt(UpperCase(D), 'T', Date, Time);
  Ye := Word(StrToInt(CopyLeft(Date, 4)));
  Mo := Word(StrToInt(CopyRange(Date, 5, 6)));
  Da := Word(StrToInt(CopyRange(Date, 7, 8)));
  Result := EncodeDate(Ye, Mo, Da) + StrToTime(Time);
end;



{                                                                              }
{ Difference                                                                   }
{                                                                              }
function DiffMilliseconds(const D1, D2: TDateTime): Int64;
begin
  Result := Integer(Trunc((D2 - D1) / OneMillisecond));
end;

function DiffSeconds(const D1, D2: TDateTime): Integer;
begin
  Result := Integer(Trunc((D2 - D1) / OneSecond));
end;

function DiffMinutes(const D1, D2: TDateTime): Integer;
begin
  Result := Integer(Trunc((D2 - D1) / OneMinute));
end;

function DiffHours(const D1, D2: TDateTime): Integer;
begin
  Result := Integer(Trunc((D2 - D1) / OneHour));
end;

function DiffDays(const D1, D2: TDateTime): Integer;
begin
  Result := Integer(Trunc(D2 - D1));
end;

function DiffWeeks(const D1, D2: TDateTime): Integer;
begin
  Result := Trunc(D2 - D1) div 7;
end;

function DiffMonths(const D1, D2: TDateTime): Integer;
var Ye1, Mo1, Da1 : Word;
    Ye2, Mo2, Da2 : Word;
    ModMonth1,
    ModMonth2     : TDateTime;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  DecodeDate(D2, Ye2, Mo2, Da2);
  Result := (Ye2 - Ye1) * 12 + (Mo2 - Mo1);
  ModMonth1 := Da1 + Frac(D1);
  ModMonth2 := Da2 + Frac(D2);
  if (D2 > D1) and (ModMonth2 < ModMonth1) then
    Dec(Result);
  if (D2 < D1) and (ModMonth2 > ModMonth1) then
    Inc(Result);
end;

function DiffYears(const D1, D2: TDateTime): Integer;
var Ye1, Mo1, Da1 : Word;
    Ye2, Mo2, Da2 : Word;
    ModYear1,
    ModYear2      : TDateTime;
begin
  DecodeDate(D1, Ye1, Mo1, Da1);
  DecodeDate(D2, Ye2, Mo2, Da2);
  Result := Ye2 - Ye1;
  ModYear1 := Mo1 * 31 + Da1 + Frac(Da1);
  ModYear2 := Mo2 * 31 + Da2 + Frac(Da2);
  if (D2 > D1) and (ModYear2 < ModYear1) then
    Dec(Result);
  if (D2 < D1) and (ModYear2 > ModYear1) then
    Inc(Result);
end;



{                                                                              }
{ Conversions                                                                  }
{                                                                              }
function DateTimeToANSI(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := Ye * 1000 + DayOfYear(Ye, Mo, Da);
end;

function ANSIToDateTime(const Julian: Integer): TDateTime;
const MaxJulian = $FFFF * 1000 + 366;
var DDD     : Integer;
    C, J    : Integer;
    M, Y, I : Word;
begin
  DDD := Julian mod 1000;
  if (DDD = 0) or (DDD > 366) or (Julian > MaxJulian) then
    raise EDateTime.Create('Invalid ANSI date format');

  Y := Julian div 1000;
  M := 0;
  C := 0;
  For I := 1 to 12 do
    begin
      J := DaysInNonLeapMonth[I];
      if (I = 2) and IsLeapYear(Y) then
        Inc(J);
      Inc(C, J);
      if C >= DDD then
        begin
          M := I;
          break;
        end;
    end;
  if M = 0 then // DDD > end of year
    raise EDateTime.Create('Invalid ANSI date format');

  Result := EncodeDate(Y, M, DDD - C + J);
end;

function DateTimeToISOInteger(const D: TDateTime): Integer;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := Ye * 10000 + Mo * 100 + Da;
end;

function DateTimeToISO(const D: TDateTime): String;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  Result := IntToStr(Ye) + '-' +
            PadLeft(IntToStr(Mo), '0', 2) + '-' +
            PadLeft(IntToStr(Da), '0', 2);
end;

function ISOIntegerToDateTime(const ISOInteger: Integer): TDateTime;
var Ye, Mo, Da : Word;
begin
  Ye := ISOInteger div 10000;
  Mo := (ISOInteger mod 10000) div 100;
  if (Mo < 1) or (Mo > 12) then
    raise EDateTime.Create('Invalid ISO Integer date format');
  Da := ISOInteger mod 100;
  if (Da < 1) or (Da > DaysInMonth(Ye, Mo)) then
    raise EDateTime.Create('Invalid ISO Integer date format');
  Result := EncodeDate(Ye, Mo, Da);
end;

function TwoDigitRadix2000YearToYear(const Y: Integer): Integer;
begin
  if Y < 50 then
    Result := 2000 + Y else
    Result := 1900 + Y;
end;

function DateTimeAsElapsedTime(const D: TDateTime;
    const IncludeMilliseconds: Boolean): String;
var I : Int64;
begin
  I := Trunc(D);
  if I > 0 then
    Result := IntToStr(I) + '.'
  else
    Result := '';
  Result := Result + IntToStr(Hour(D)) + ':' +
            PadLeft(IntToStr(Minute(D)), '0', 2) + ':' +
            PadLeft(IntToStr(Second(D)), '0', 2);
  if IncludeMilliseconds then
    Result := Result + '.' + PadLeft(IntToStr(Millisecond(D)), '0', 3);
end;



{                                                                              }
{ Time Zone                                                                    }
{                                                                              }

{ Returns the GMT bias (in minutes) from the operating system's regional       }
{ settings.                                                                    }
function GMTBias : Integer;
var TZI : TTimeZoneInformation;
begin
  if GetTimeZoneInformation(TZI) = TIME_ZONE_ID_DAYLIGHT then
    Result := TZI.DaylightBias else
    Result := 0;
  Result := Result + TZI.Bias;
end;

{ Converts GMT Time to Local Time                                              }
function GMTTimeToLocalTime(const D: TDateTime): TDateTime;
begin
  Result := D - GMTBias / (24.0 * 60.0);
end;

{ Converts Local Time to GMT Time                                              }
function LocalTimeToGMTTime(const D: TDateTime): TDateTime;
begin
  Result := D + GMTBias / (24.0 * 60.0);
end;

function NowAsGMTTime: TDateTime;
begin
  Result := LocalTimeToGMTTime(Now);
end;



{                                                                              }
{ RFC DateTime                                                                 }
{                                                                              }
const
  RFC850DayNames : Array[1..7] of String = (
      'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
  RFC1123DayNames : Array[1..7] of String = (
      'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  RFCMonthNames : Array[1..12] of String = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

function RFC850DayOfWeek(const S: String): Integer;
var I : Integer;
begin
  For I := 1 to 7 do
    if StrEqualNoCase(RFC850DayNames[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFC1123DayOfWeek(const S: String): Integer;
var I : Integer;
begin
  For I := 1 to 7 do
    if StrEqualNoCase(RFC1123DayNames[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function RFCMonth(const S: String): Word;
var I : Word;
begin
  For I := 1 to 12 do
    if StrEqualNoCase(RFCMonthNames[I], S) then
      begin
        Result := I;
        exit;
      end;
  Result := 0;
end;

function GMTTimeToRFC1123Time(const D: TDateTime; const IncludeSeconds: Boolean): String;
var Ho, Mi, Se, Ms : Word;
begin
  DecodeTime(D, Ho, Mi, Se, Ms);
  Result := PadLeft(IntToStr(Ho), '0', 2) + ':' +
            PadLeft(IntToStr(Mi), '0', 2);
  if IncludeSeconds then
    Result := Result + ':' + PadLeft(IntToStr(Se), '0', 2);
  Result := Result + ' GMT';
end;

function GMTDateTimeToRFC1123DateTime(const D: TDateTime; const IncludeDayOfWeek: Boolean): String;
var Ye, Mo, Da : Word;
begin
  DecodeDate(D, Ye, Mo, Da);
  if IncludeDayOfWeek then
    Result := RFC1123DayNames[DayOfWeek(D)] + ', '
  else
    Result := '';
  Result := Result +
            PadLeft(IntToStr(Da), '0', 2) + ' ' +
            RFCMonthNames[Mo] + ' ' +
            IntToStr(Ye) + ' ' +
            GMTTimeToRFC1123Time(D, True);
end;

function DateTimeToRFCDateTime(const D: TDateTime): String;
begin
  Result := GMTDateTimeToRFC1123DateTime(LocalTimeToGMTTime(D), True);
end;

function RFCTimeZoneToGMTBias(const Zone: String): Integer;
type
  TZoneBias = record
     Zone : String;
     Bias : Integer;
   end;

const
  SPACE = csWhiteSpace;
  TimeZones = 35;
  ZoneBias : Array[1..TimeZones] of TZoneBias =
      ((Zone:'GMT'; Bias:0),       (Zone:'UT';  Bias:0),
       (Zone:'EST'; Bias:-5*60),   (Zone:'EDT'; Bias:-4*60),
       (Zone:'CST'; Bias:-6*60),   (Zone:'CDT'; Bias:-5*60),
       (Zone:'MST'; Bias:-7*60),   (Zone:'MDT'; Bias:-6*60),
       (Zone:'PST'; Bias:-8*60),   (Zone:'PDT'; Bias:-7*60),
       (Zone:'Z';   Bias:0),       (Zone:'A';   Bias:-1*60),
       (Zone:'B';   Bias:-2*60),   (Zone:'C';   Bias:-3*60),
       (Zone:'D';   Bias:-4*60),   (Zone:'E';   Bias:-5*60),
       (Zone:'F';   Bias:-6*60),   (Zone:'G';   Bias:-7*60),
       (Zone:'H';   Bias:-8*60),   (Zone:'I';   Bias:-9*60),
       (Zone:'K';   Bias:-10*60),  (Zone:'L';   Bias:-11*60),
       (Zone:'M';   Bias:-12*60),  (Zone:'N';   Bias:1*60),
       (Zone:'O';   Bias:2*60),    (Zone:'P';   Bias:3*60),
       (Zone:'Q';   Bias:4*60),    (Zone:'R';   Bias:3*60),
       (Zone:'S';   Bias:6*60),    (Zone:'T';   Bias:3*60),
       (Zone:'U';   Bias:8*60),    (Zone:'V';   Bias:3*60),
       (Zone:'W';   Bias:10*60),   (Zone:'X';   Bias:3*60),
       (Zone:'Y';   Bias:12*60));

var
  S : String;
  I : Integer;

begin
  if Zone[1] in ['+', '-'] then // +hhmm format
    begin
      S := Trim(Zone, SPACE);
      Result := MaxI(-23, MinI(23, StrToIntDef(Copy(S, 2, 2), 0))) * 60;
      S := CopyFrom(S, 4);
      if S <> '' then
        Result := Result + MinI(59, MaxI(0, StrToIntDef(S, 0)));
      if Zone[1] = '-' then
        Result := -Result;
    end else
    begin // named format
      S := Trim(Zone, SPACE);
      For I := 1 to TimeZones do
        if StrEqualNoCase(ZoneBias[I].Zone, S) then
          begin
            Result := ZoneBias[I].Bias;
            exit;
          end;
      Result := 0;
    end;
end;

procedure RFCTimeToGMTTime(const S: String; var Hours, Minutes, Seconds: Integer);
const
  SPACE = csWhiteSpace;

var
  I : Integer;
  T : String;
  Bias, HH, MM, SS : Integer;
  U : StringArray;

begin
  U := nil;
  Hours := 0;
  Minutes := 0;
  Seconds := 0;
  T := Trim(S, SPACE);
  if T = '' then
    exit;

  // Get Zone bias
  I := PosCharRev(SPACE, T);
  if I > 0 then
    begin
      Bias := RFCTimeZoneToGMTBias(CopyFrom(T, I + 1));
      T := Trim(CopyLeft(T, I - 1), SPACE);
    end
  else
    Bias := 0;

  // Get time
  U := StrSplit(T, ':');
  if (Length(U) = 1) and (Length(U[0]) = 4) then
    begin // old hhmm format
      HH := StrToIntDef(Copy(U[0], 1, 2), 0);
      MM := StrToIntDef(Copy(U[0], 3, 2), 0);
      SS := 0;
    end else
  if (Length(U) >= 2) or (Length(U) <= 3) then // hh:mm[:ss] format (RFC1123)
    begin
      HH := StrToIntDef(Trim(U[0], SPACE), 0);
      MM := StrToIntDef(Trim(U[1], SPACE), 0);
      if Length(U) = 3 then
        SS := StrToIntDef(Trim(U[2], SPACE), 0) else
        SS := 0;
    end
  else
    exit;

  Hours := MaxI(0, MinI(23, HH));
  Minutes := MaxI(0, MinI(59, MM));
  Seconds := MaxI(0, MinI(59, SS));
  Inc(Hours, Bias div 60);
  Inc(Minutes, Bias mod 60);
end;

function EncodeBiasedDateTime(const Year, Month, Day, Hour, Minute, Second: Integer): TDateTime;
var Ho, Mi : Integer;
begin
  Result := EncodeDate(Word(Year), Word(Month), Word(Day));
  Ho := Hour;
  Mi := Minute;
  if Mi < 0 then
    begin
      Inc(Mi, 60);
      Dec(Ho);
    end;
  if Ho < 0 then
    begin
      Inc(Ho, 24);
      Result := AddDays(Result, -1);
    end;
  Result := Result + EncodeTime(Word(Ho), Word(Mi), Word(Second), 0);
end;

function RFCDateTimeToGMTDateTime(const S: String): TDateTime;
const
  SPACE = csWhiteSpace;

var
  T, U : String;
  I : Integer;
  D, M, Y, DOW, Ho, Mi, Se : Integer;
  V, W : StringArray;

begin
  Result := 0.0;

  W := nil;
  T := Trim(S, SPACE);

  // Extract Day of week
  I := PosChar(SPACE + [','], T);
  if I > 0 then
    begin
      U := CopyLeft(T, I - 1);
      DOW := RFC850DayOfWeek(U);
      if DOW = -1 then
        DOW := RFC1123DayOfWeek(U);
      if DOW <> -1 then
        T := Trim(CopyFrom(S, I + 1), SPACE);
    end;

  V := StrSplitChar(T, SPACE);
  if Length(V) < 3 then
    exit;

  if Pos('-', V[0]) > 0 then // RFC850 date, eg "Sunday, 06-Nov-94 08:49:37 GMT"
    begin
      W := StrSplitChar(V[0], '-');
      if Length(W) <> 3 then
        exit;
      M := RFCMonth(W[1]);
      if M = 0 then
        exit;
      D := StrToIntDef(W[0], 0);
      Y := StrToIntDef(W[2], 0);
      if Y < 100 then
        Y := TwoDigitRadix2000YearToYear(Y);
      RFCTimeToGMTTime(V[1] + ' ' + V[2], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
      exit;
    end;

  M := RFCMonth(V[1]);
  if M >= 1 then // RFC822 date, eg Sun, 06 Nov 1994 08:49:37 GMT
    begin
      D := StrToIntDef(V[0], 0);
      Y := StrToIntDef(V[2], 0);
      Ho := 0;
      Mi := 0;
      Se := 0;
      if Length(V) = 4 then
        RFCTimeToGMTTime(V[3], Ho, Mi, Se) else
      if Length(V) >= 5 then
        RFCTimeToGMTTime(V[3] + ' ' + V[4], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
      exit;
    end;

  M := RFCMonth(V[0]);
  if M >= 1 then // ANSI C asctime() format, eg "Sun Nov  6 08:49:37 1994"
    begin
      D := StrToIntDef(V[1], 0);
      Y := StrToIntDef(V[3], 0);
      RFCTimeToGMTTime(V[2], Ho, Mi, Se);
      Result := EncodeBiasedDateTime(Y, M, D, Ho, Mi, Se);
    end;
end;

function RFCDateTimeToDateTime(const S: String): TDateTime;
begin
  Result := GMTTimeToLocalTime(RFCDateTimeToGMTDateTime(S));
end;

function NowAsRFCDateTime : String;
begin
  Result := DateTimeToRFCDateTime(Now);
end;



{                                                                              }
{ High-precision timing                                                        }
{                                                                              }
var
  HighPrecisionTimerInit   : Boolean = False;
  HighPrecisionMilliFactor : Int64;  // millisecond factor
  HighPrecisionMicroFactor : Int64;  // microsecond factor

function CPUClockFrequency : Int64;
begin
  if not QueryPerformanceFrequency(Result) then
    raise EDateTime.Create('High resolution timer not available');
end;

procedure InitHighPrecisionTimer;
begin
  HighPrecisionMilliFactor := CPUClockFrequency;
  HighPrecisionMilliFactor := HighPrecisionMilliFactor div 1000;
  HighPrecisionMicroFactor := CPUClockFrequency;
  HighPrecisionMicroFactor := HighPrecisionMicroFactor div 1000000;
  HighPrecisionTimerInit := True;
end;

function StartTimer : Int64;
begin
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  QueryPerformanceCounter(Result);
end;

function MillisecondsElapsed(const Timer: Int64; const TimerRunning: Boolean = True): Integer;
var I : Int64;
begin
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  if not TimerRunning then
    Result := Timer div HighPrecisionMilliFactor else
    begin
      QueryPerformanceCounter(I);
      {$IFDEF DELPHI5}
      {$Q-}
      Result := (I - Timer) div HighPrecisionMilliFactor;
      {$ELSE}
      Result := Int64(I - Timer) div HighPrecisionMilliFactor;
      {$ENDIF}
    end;
end;

function MicrosecondsElapsed(const Timer: Int64; const TimerRunning: Boolean = True): Integer;
var I : Int64;
begin
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  if not TimerRunning then
    Result := Timer div HighPrecisionMicroFactor else
    begin
      QueryPerformanceCounter(I);
      {$IFDEF DELPHI5}
      {$Q-}
      Result := (I - Timer) div HighPrecisionMicroFactor;
      {$ELSE}
      Result := Int64(I - Timer) div HighPrecisionMicroFactor;
      {$ENDIF}
    end;
end;

procedure StopTimer(var Timer : Int64);
var I : Int64;
begin
  QueryPerformanceCounter(I);
  {$IFDEF DELPHI5}
  {$Q-}
  Timer := I - Timer;
  {$ELSE}
  Timer := Int64(I - Timer);
  {$ENDIF}
end;

procedure ResumeTimer(var StoppedTimer : Int64);
begin
  StoppedTimer := Int64(StartTimer - StoppedTimer);
end;

function StoppedTimer : Int64;
begin
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  Result := 0;
end;

function ElapsedTimer(const Milliseconds: Integer): THPTimer;
var I : Int64;
begin
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  QueryPerformanceCounter(I);
  {$IFDEF DELPHI5}
  {$Q-}
  Result := I - (Milliseconds * HighPrecisionMilliFactor);
  {$ELSE}
  Result := Int64(I - (Milliseconds * HighPrecisionMilliFactor));
  {$ENDIF}
end;

procedure DelayMicroSeconds(const MicroSeconds: Integer);
var I, J, F : Int64;
begin
  if MicroSeconds <= 0 then
    exit;
  if not HighPrecisionTimerInit then
    InitHighPrecisionTimer;
  if not QueryPerformanceCounter(I) then
    exit;
  {$IFDEF DELPHI5}
  {$Q-}
  F := MicroSeconds * HighPrecisionMicroFactor;
  Repeat
    QueryPerformanceCounter(J);
    J := J - I;
  Until J >= F;
  {$ELSE}
  F := Int64(MicroSeconds * HighPrecisionMicroFactor);
  Repeat
    QueryPerformanceCounter(J);
  Until Int64(J - I) >= F;
  {$ENDIF}
end;



{                                                                              }
{ Natural language                                                             }
{                                                                              }
function TimePeriodStr(const D: TDateTime): String;
var E : TDateTime;
    I : Integer;
begin
  E := Abs(D);
  if E < OneMillisecond then
    Result := '' else
  if E >= OneWeek then
    begin
      I := Trunc(D / OneWeek);
      if I = 1 then
        Result := 'a week' else
        Result := IntToStr(I) + ' weeks';
    end else
  if E >= OneDay then
    begin
      I := Trunc(D / OneDay);
      if I = 1 then
        Result := 'a day' else
        Result := IntToStr(I) + ' days';
    end else
  if E >= OneHour then
    begin
      I := Trunc(D / OneHour);
      if I = 1 then
        Result := 'an hour' else
        Result := IntToStr(I) + ' hours';
    end else
  if E >= OneMinute then
    begin
      I := Trunc(D / OneMinute);
      if I = 1 then
        Result := 'a minute' else
        Result := IntToStr(I) + ' minutes';
    end
  else
    begin
      I := Trunc(D / OneSecond);
      if I = 1 then
        Result := 'a second' else
        Result := IntToStr(I) + ' seconds';
    end;
end;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var A, B : TDateTime;
    Ye, Mo, Da, Ho, Mi, Se, Ms : Word;
    Ye2, Mo2, Da2, Ho2, Mi2, Se2, Ms2 : Word;
    S : String;
begin
  Ho := 7;
  Mi := 10;
  Da := 8;
  Ms := 3;
  For Ye := 1999 to 2001 do
    For Mo := 1 to 12 do
      For Se := 0 to 59 do
        begin
          A := EncodeDateTime(Ye, Mo, Da, Ho, Mi, Se, Ms);
          DecodeDateTime(A, Ye2, Mo2, Da2, Ho2, Mi2, Se2, Ms2);
          Assert(Ye = Ye2, 'DecodeDate');
          Assert(Mo = Mo2, 'DecodeDate');
          Assert(Da = Da2, 'DecodeDate');
          Assert(Ho = Ho2, 'DecodeDate');
          Assert(Mi = Mi2, 'DecodeDate');
          Assert(Se = Se2, 'DecodeDate');
          Assert(Ms = Ms2, 'DecodeDate');
          Assert(Year(A) = Ye, 'Year');
          Assert(Month(A) = Mo, 'Month');
          Assert(Day(A) = Da, 'Day');
          Assert(Hour(A) = Ho, 'Hour');
          Assert(Minute(A) = Mi, 'Minute');
          Assert(Second(A) = Se, 'Second');
          Assert(Millisecond(A) = Ms, 'Millisecond');
        end;
  A := EncodeDateTime(2002, 05, 31, 07, 04, 01, 02);
  Assert(IsEqual(A, 2002, 05, 31), 'IsEqual');
  Assert(IsEqual(A, 07, 04, 01, 02), 'IsEqual');
  Assert(IsFriday(A), 'IsFriday');
  Assert(not IsMonday(A), 'IsMonday');
  A := AddWeeks(A, 2);
  Assert(IsEqual(A, 2002, 06, 14), 'AddWeeks');
  A := AddHours(A, 2);
  Assert(IsEqual(A, 09, 04, 01, 02), 'AddHours');
  A := EncodeDateTime(2004, 03, 01, 0, 0, 0, 0);
  Assert(DayOfYear(A) = 61, 'DayOfYear');
  Assert(DaysInMonth(2004, 02) = 29, 'DaysInMonth');
  Assert(DaysInMonth(2005, 02) = 28, 'DaysInMonth');
  Assert(DaysInMonth(2001, 01) = 31, 'DaysInMonth');
  Assert(DaysInYear(2000) = 366, 'DaysInYear');
  Assert(DaysInYear(2004) = 366, 'DaysInYear');
  Assert(DaysInYear(2006) = 365, 'DaysInYear');
  A := EncodeDateTime(2001, 09, 02, 12, 11, 10, 0);
  S := GMTTimeToRFC1123Time(A, True);
  Assert(S = '12:11:10 GMT');
  {$IFNDEF FREEPASCAL} // Weird bug with FreePascal
  S := GMTDateTimeToRFC1123DateTime(A, True);
  Assert(S = 'Sun, 02 Sep 2001 12:11:10 GMT', 'GMTDateTimeToRFC1123DateTime');
  For Ye := 1999 to 2004 do
    For Mo := 1 to 2 do
      For Da := 1 to 2 do
        For Ho := 0 to 23 do
          begin
            A := EncodeDateTime(Ye, Mo, Da, Ho, 11, 10, 0);
            S := GMTDateTimeToRFC1123DateTime(A, True);
            B := RFCDateTimeToGMTDateTime(S);
            Assert(IsEqual(A, B), 'RFCDateTimeToGMTDateTime');
          end;
  {$ENDIF}
end;



end.

