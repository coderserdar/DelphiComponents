{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_parser_date;

interface
{$i psc_defines.inc}
uses
  classes,
  sysutils,
  winapi.windows,

  myla_system,
  myla_interfaces,
  myla_parser,

  psc_wrapper,
  psc_procs,
  psc_const;

{------------------------------------------------------------------------------}

type
  TPSCDTFormatPartStyle=(
    DTFPS_NOZERO,
    DTFPS_ZERO,
    DTFPS_SHORT,
    DTFPS_LONG,
    DTFPS_TWODIGITS,
    DTFPS_FOURDIGITS,
    DTFPS_AMPM,
    DTFPS_AP,
    DTFPS_AMPM_LOCALE
  );

  TPSCDTFormatPartKind=(
    DTFPK_YEAR_IN_ERA,
    DTFPK_ERA,
    DTFPK_DAY,
    DTFPK_MONTH,
    DTFPK_YEAR,
    DTFPK_HOUR12,
    DTFPK_HOUR24,
    DTFPK_MINUTE,
    DTFPK_SECOND,
    DTFPK_MILLISECOND,
    DTFPK_AMPM,
    DTFPK_ASIS,
    DTFPK_DATE_SEPARATOR,
    DTFPK_TIME_SEPARATOR
  );

  TPSCDTFormatPartKinds = set of TPSCDTFormatPartKind;

  TPSCDTFormatPart=packed record
    Kind:TPSCDTFormatPartKind;
    Style:TPSCDTFormatPartStyle;
    DataAsStr:String;
  end;

  TPSCDTFormatParts=packed record
    PartCount:Integer;
    Parts:Array of TPSCDTFormatPart;
  end;
  
  TPSCAMPM = Array[boolean] Of String;

  IPSCDateTimeFormat=interface(IPSCInterface)
    ['{DE997039-3F0D-4720-B58D-0B2C622B8137}']
    function GetSysLocale:TSysLocale;
    function GetTimeAMString:String;
    function GetTimePMString:String;
    function GetDateSeparator:String;
    function GetTimeSeparator:String;
    function GetLongDateFormat:String;
    function GetShortDateFormat:String;
    function GetLongTimeFormat:String;
    function GetShortTimeFormat:String;
    function GetShortDayName(const AIndex:Integer):String;
    function GetLongDayName(const AIndex:Integer):String;
    function GetLongMonthName(const AIndex:Integer):String;
    function GetShortMonthName(const AIndex:Integer):String;
    function GetDateAltFormatted(const ADate:TDateTime;const AFormat: string): string;
  end;

  TPSCDateFormatKind=(dfkShort,dfkLong);

  TPSCDateTimeFormat = Class(TPSCIntfPersistent)
  private
    FSavedState: TPSCDateTimeFormatRec;
    FThisFormat: IPSCDateTimeFormat;
    FState: TPSCDateTimeFormatRec;
    FFormat: IPSCDateTimeFormat;
    FDateKind: TPSCDateFormatKind;
    FTimeKind: TPSCDateFormatKind;

    procedure SetDateKind(V:TPSCDateFormatKind);
    procedure SetTimeKind(V:TPSCDateFormatKind);
    Procedure SetLongDateFormat(Const V: String);
    Procedure SetShortDateFormat(Const V: String);
    Procedure SetLongTimeFormat(Const V: String);
    Procedure SetShortTimeFormat(Const V: String);
    Procedure SetDateSeparator(Const V: String);
    Procedure SetTimeSeparator(Const V: String);
    Procedure SetTimeAMString(Const V: String);
    Procedure SetTimePMString(Const V: String);

    Function GetShortDateFormat: String;
    Function GetLongDateFormat: String;
    function GetDateSeparator:String;
    function GetTimeSeparator:String;
    Function GetLongTimeFormat: String;
    Function GetShortTimeFormat: String;
    Function GetTimeAMString: String;
    Function GetTimePMString: String;

    Function IsLongDateFormatStored:Boolean;
    Function IsShortDateFormatStored:Boolean;
    Function IsLongTimeFormatStored:Boolean;
    Function IsShortTimeFormatStored:Boolean;
    Function IsDateSeparatorStored:Boolean;
    Function IsTimeSeparatorStored:Boolean;
    Function IsTimeAMStringStored:Boolean;
    Function IsTimePMStringStored:Boolean;

    Function GetDefaultShortDateFormat: String;
    Function GetDefaultLongDateFormat: String;
    function GetDefaultDateSeparator:String;
    function GetDefaultTimeSeparator:String;
    Function GetDefaultLongTimeFormat: String;
    Function GetDefaultShortTimeFormat: String;
    Function GetDefaultTimeAMString: String;
    Function GetDefaultTimePMString: String;

    function GetTimeFormat:String;
    function GetDateFormat:String;
    function GetDateTimeFormat:String;

    function GetSysLocale:TSysLocale;
    function GetShortDayName(const AIndex:Integer):String;
    function GetLongDayName(const AIndex:Integer):String;
    function GetLongMonthName(const AIndex:Integer):String;
    function GetShortMonthName(const AIndex:Integer):String;
    function GetDateAltFormatted(const ADate:TDateTime;const AFormat: string): string;
    function AsRecord:TPSCDateTimeFormatRec;
  protected
    property BaseFormat:IPSCDateTimeFormat Read FFormat Write FFormat;
  public
    Procedure SetSystemFormat;
    Procedure RestoreSystemFormat;

    Function GetFormat(AKind:TPSCDateTimeKind):String;
    Function ToStringEx(Const V: TDateTime;AKind:TPSCDateTimeKind): String; overload;
    Function ToStringEx(Const V: TDateTime;const AFormat:String): String; overload;
    Function FromString(Const S: String;AKind:TPSCDateTimeKind;
      const ADefault:TDateTime): TDateTime;

    constructor Create(AOwner:TPersistent;const AFormat:IPSCDateTimeFormat);
    destructor Destroy;override;

    property ThisFormat:IPSCDateTimeFormat Read FThisFormat;
  published
    Property LongDateFormat: String read GetLongDateFormat write
      SetLongDateFormat Stored IsLongDateFormatStored;
    Property ShortDateFormat: String read GetShortDateFormat write
      SetShortDateFormat Stored IsShortDateFormatStored;
    Property LongTimeFormat: String read GetLongTimeFormat write
      SetLongTimeFormat Stored IsLongTimeFormatStored;
    Property ShortTimeFormat: String read GetShortTimeFormat write
      SetShortTimeFormat Stored IsShortTimeFormatStored;
    Property DateSeparator: String read GetDateSeparator write SetDateSeparator
      Stored IsDateSeparatorStored;
    Property TimeSeparator: String read GetTimeSeparator write SetTimeSeparator
      Stored IsTimeSeparatorStored;
    Property AMSymbol: String read GetTimeAMString write SetTimeAMString
      stored IsTimeAMStringStored;
    Property PMSymbol: String read GetTimePMString write SetTimePMString
      stored IsTimePMStringStored;
    property DateKind:TPSCDateFormatKind Read FDateKind Write SetDateKind Default dfkShort;
    property TimeKind:TPSCDateFormatKind Read FTimeKind Write SetTimeKind Default dfkShort;
  End;

const
  CPSCTIME_AMPM:TPSCAMPM = ('AM', 'PM');
  CPSCTIME_AP:TPSCAMPM = ('a', 'p');

{------------------------------------------------------------------------------}

function PSCWindowsDateTimeFormat(ALocale : Cardinal):IPSCDateTimeFormat;
function PSCSysUtilsDateTimeFormat:IPSCDateTimeFormat;
function PSCParseDateTimeFormatToParts(const AFormatString:String;
 const AFormat : IPSCDateTimeFormat):TPSCDTFormatParts;
function PSCFormatDateTimeUseParts(const ADate:TDateTime;
 const AFormatParts : TPSCDTFormatParts; const AFormat:IPSCDateTimeFormat) : string;
function PSCInsertLeadingChars(const S: string; C: Char; AMaxLen : integer) : string;
function PSCDecodeAMPM(const AMPM : string; const ADefault: TPSCAMPM) : TPSCAMPM;

function PSCGetDefaultDateTimeFormat:IPSCDateTimeFormat;
procedure PSCSetDefaultDateTimeFormat(const AFormat:IPSCDateTimeFormat);

{------------------------------------------------------------------------------}

implementation

{------------------------------------------------------------------------------}

type
  TPSCFormatDTPartParams = packed record
    AFormatPart:TPSCDTFormatPart;
    ADate:TDateTime;
    ADateAsArray:TPSCDateTimePartsArray;
    AFormat:IPSCDateTimeFormat;
  end;

  TPSCDayNames = Array[1..7] of String;
  TPSCMonthNames = Array[1..12] of String;
  TPSCFormatDTPart = function(const AParams:TPSCFormatDTPartParams) : string;

  TPSCWindowsDateTimeFormat = class(TInterfacedObject, IPSCDateTimeFormat)
  private
    FCurrentLocale : Cardinal;
  public
    function GetSysLocale:TSysLocale;
    function GetTimeAMString:String;
    function GetTimePMString:String;
    function GetDateSeparator:String;
    function GetTimeSeparator:String;
    function GetLongDateFormat:String;
    function GetShortDateFormat:String;
    function GetLongTimeFormat:String;
    function GetShortTimeFormat:String;
    function GetShortDayName(const AIndex:Integer):String;
    function GetLongDayName(const AIndex:Integer):String;
    function GetLongMonthName(const AIndex:Integer):String;
    function GetShortMonthName(const AIndex:Integer):String;
    function GetDateAltFormatted(const ADate:TDateTime;const AFormat: string): string;
    Constructor Create(ALocale : Cardinal);
  end;

function PSCFormat_YearInEraNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_YearInEraZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_EraShort(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_EraLong(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_DayNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_DayZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_DayShort(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_DayLong(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MonthNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MonthZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MonthShort(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MonthLong(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_YearTwoDigits(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_YearFourDigits(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_Hour12NoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_Hour12Zero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_Hour24NoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_Hour24Zero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MinuteNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MinuteZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_SecondNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_SecondZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MSecondNoZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_MSecondZero(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_AMPM(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_AP(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_AMPM_Locale(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_ASIS(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_DateSeparator(const AParams: TPSCFormatDTPartParams) : string; forward;
function PSCFormat_TimeSeparator(const AParams: TPSCFormatDTPartParams) : string; forward;

{------------------------------------------------------------------------------}

const
  PSCDTPartsProcs : array[TPSCDTFormatPartKind, TPSCDTFormatPartStyle] of TPSCFormatDTPart =
  (
    (
      PSCFormat_YearInEraNoZero,
      PSCFormat_YearInEraZero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      nil,
      nil,
      PSCFormat_EraShort,
      PSCFormat_EraLong,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_DayNoZero,
      PSCFormat_DayZero,
      PSCFormat_DayShort,
      PSCFormat_DayLong,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_MonthNoZero,
      PSCFormat_MonthZero,
      PSCFormat_MonthShort,
      PSCFormat_MonthLong,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      nil,
      nil,
      nil,
      nil,
      PSCFormat_YearTwoDigits,
      PSCFormat_YearFourDigits,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_Hour12NoZero,
      PSCFormat_Hour12Zero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_Hour24NoZero,
      PSCFormat_Hour24Zero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_MinuteNoZero,
      PSCFormat_MinuteZero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_SecondNoZero,
      PSCFormat_SecondZero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      PSCFormat_MSecondNoZero,
      PSCFormat_MSecondZero,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      nil
    ),
    (
      nil,
      nil,
      nil,
      nil,
      nil,
      nil,
      PSCFormat_AMPM,
      PSCFormat_AP,
      PSCFormat_AMPM_Locale
    ),
    (
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS,
      PSCFormat_ASIS
    ),
    (
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator,
      PSCFormat_DateSeparator
    ),
    (
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator,
      PSCFormat_TimeSeparator
    )
  );

{------------------------------------------------------------------------------}

function PSCFormatDateTimeUsePart(const AParams: TPSCFormatDTPartParams) : string;
begin
  with AParams.AFormatPart do
  begin
    if Assigned(PSCDTPartsProcs[Kind, Style]) then
      result := PSCDTPartsProcs[Kind, Style](AParams)
    else
      result := '';
  end;
end;

{------------------------------------------------------------------------------}

const
  psdt_normal             = 0;
  psdt_comment            = 40;

  tok_dt_d                = 1;
  tok_dt_dd               = 2;
  tok_dt_ddd              = 3;
  tok_dt_dddd             = 4;
  tok_dt_e                = 5;
  tok_dt_ee               = 6;
  tok_dt_g                = 7;
  tok_dt_gg               = 8;
  tok_dt_m                = 9;
  tok_dt_mm               = 10;
  tok_dt_mmm              = 11;
  tok_dt_mmmm             = 12;
  tok_dt_yy               = 13;
  tok_dt_yyyy             = 14;
  tok_dt_h                = 15;
  tok_dt_hh               = 16;
  tok_dt_n                = 17;
  tok_dt_nn               = 18;
  tok_dt_s                = 19;
  tok_dt_ss               = 20;
  tok_dt_z                = 21;
  tok_dt_zzz              = 22;
  tok_dt_ampm             = 23;
  tok_dt_ap               = 24;
  tok_dt_ampm_locale      = 25;
  tok_dt_datesep          = 26;
  tok_dt_timesep          = 27;
  tok_dt_asis             = 28;
  tok_dt_skip             = 29;

  tok_dt_t                = 30;
  tok_dt_tt               = 31;
  tok_dt_c                = 32;
  tok_dt_ddddd            = 33;
  tok_dt_dddddd           = 34;

  CPSCDTTokenToKind:Array[1..29] of TPSCDTFormatPart=(
    (Kind:DTFPK_DAY;         Style:DTFPS_NOZERO     ),//tok_dt_d
    (Kind:DTFPK_DAY;         Style:DTFPS_ZERO       ),//tok_dt_dd
    (Kind:DTFPK_DAY;         Style:DTFPS_SHORT      ),//tok_dt_ddd
    (Kind:DTFPK_DAY;         Style:DTFPS_LONG       ),//tok_dt_dddd
    (Kind:DTFPK_YEAR_IN_ERA; Style:DTFPS_NOZERO     ),//tok_dt_e
    (Kind:DTFPK_YEAR_IN_ERA; Style:DTFPS_ZERO       ),//tok_dt_ee
    (Kind:DTFPK_ERA;         Style:DTFPS_SHORT      ),//tok_dt_g
    (Kind:DTFPK_ERA;         Style:DTFPS_LONG       ),//tok_dt_gg
    (Kind:DTFPK_MONTH;       Style:DTFPS_NOZERO     ),//tok_dt_m
    (Kind:DTFPK_MONTH;       Style:DTFPS_ZERO       ),//tok_dt_mm
    (Kind:DTFPK_MONTH;       Style:DTFPS_SHORT      ),//tok_dt_mmm
    (Kind:DTFPK_MONTH;       Style:DTFPS_LONG       ),//tok_dt_mmmm
    (Kind:DTFPK_YEAR;        Style:DTFPS_TWODIGITS  ),//tok_dt_yy
    (Kind:DTFPK_YEAR;        Style:DTFPS_FOURDIGITS ),//tok_dt_yyyy
    (Kind:DTFPK_HOUR24;        Style:DTFPS_NOZERO     ),//tok_dt_h
    (Kind:DTFPK_HOUR24;        Style:DTFPS_ZERO       ),//tok_dt_hh
    (Kind:DTFPK_MINUTE;      Style:DTFPS_NOZERO     ),//tok_dt_n
    (Kind:DTFPK_MINUTE;      Style:DTFPS_ZERO       ),//tok_dt_nn
    (Kind:DTFPK_SECOND;      Style:DTFPS_NOZERO     ),//tok_dt_s
    (Kind:DTFPK_SECOND;      Style:DTFPS_ZERO       ),//tok_dt_ss
    (Kind:DTFPK_MILLISECOND; Style:DTFPS_NOZERO     ),//tok_dt_z
    (Kind:DTFPK_MILLISECOND; Style:DTFPS_ZERO       ),//tok_dt_zzz
//    (Kind:DTFPK_AMPM;        Style:DTFPS_AMPM_LOCALE),//tok_dt_t
//    (Kind:DTFPK_AMPM;        Style:DTFPS_AMPM_LOCALE),//tok_dt_tt
    (Kind:DTFPK_AMPM;        Style:DTFPS_AMPM       ),//tok_dt_ampm
    (Kind:DTFPK_AMPM;        Style:DTFPS_AP         ),//tok_dt_ap
    (Kind:DTFPK_AMPM;        Style:DTFPS_AMPM_LOCALE),//tok_dt_ampm_locale
    (Kind:DTFPK_DATE_SEPARATOR                      ),//tok_dt_datesep
    (Kind:DTFPK_TIME_SEPARATOR                      ),//tok_dt_timesep
    (Kind:DTFPK_ASIS                                ),//tok_dt_asis
    (Kind:DTFPK_ASIS                                )//tok_dt_skip
  );

type
  TPSCDTFormatParser=class(TPSCParser)
  private
    FKinds : TPSCDTFormatPartKinds;
    FFormat:IPSCDateTimeFormat;
    procedure InitFormatSyntax;
    procedure AddPart(AToken:Integer);
  protected
    Function Parse_c: Integer;
    Function Parse_d: Integer;
    Function Parse_e: Integer;
    Function Parse_g: Integer;
    Function Parse_m: Integer;
    Function Parse_y: Integer;
    Function Parse_h: Integer;
    Function Parse_n: Integer;
    Function Parse_s: Integer;
    Function Parse_z: Integer;
    Function Parse_t: Integer;
    Function Parse_a: Integer;
    Function Parse_date_sep: Integer;
    Function Parse_time_sep: Integer;
    Function Parse_asis : integer;
    Function Parse_comment : integer;
    Function CountCharOccurrence(ACharSet:TPSCCharSet;
      const ACToken : Array of Integer):Integer;
  public
    Parts:TPSCDTFormatParts;
    procedure ParseFormat;
    constructor Create(const ASource:IPSCParserStream);
    property Format:IPSCDateTimeFormat read FFormat write FFormat;
  end;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_date_sep: Integer;
begin
  Next;
  Result:=tok_dt_datesep;
end;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_time_sep: Integer;
begin
  Next;
  Result:=tok_dt_timesep;
end;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_d: Integer;
Const
  MyTokens : Array[1..6] Of integer = (tok_dt_d, tok_dt_dd, tok_dt_ddd, tok_dt_dddd,
   tok_dt_ddddd, tok_dt_dddddd);
Begin
  result := CountCharOccurrence(['d', 'D'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_e: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_e, tok_dt_ee);
Begin
  result := CountCharOccurrence(['e', 'E'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_g: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_g, tok_dt_gg);
Begin
  result := CountCharOccurrence(['g', 'G'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_m: Integer;
Const
  MyTokens : Array[1..4] Of Integer = (tok_dt_m, tok_dt_mm, tok_dt_mmm, tok_dt_mmmm);
Begin
  result := CountCharOccurrence(['m', 'M'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_y: Integer;
Const
  MyTokens : Array[1..4] Of Integer = (tok_dt_asis, tok_dt_yy, tok_dt_asis, tok_dt_yyyy);
Begin
  result := CountCharOccurrence(['y', 'Y'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_h: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_h, tok_dt_hh);
Begin
  result := CountCharOccurrence(['h', 'H'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_n: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_n, tok_dt_nn);
Begin
  result := CountCharOccurrence(['n', 'N'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_s: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_s, tok_dt_ss);
Begin
  result := CountCharOccurrence(['s', 'S'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_z: Integer;
Const
  MyTokens : Array[1..3] Of Integer = (tok_dt_z, tok_dt_asis, tok_dt_zzz);
Begin
  result := CountCharOccurrence(['g', 'G'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_t: Integer;
Const
  MyTokens : Array[1..2] Of Integer = (tok_dt_t, tok_dt_tt);
Begin
  result := CountCharOccurrence(['t', 'T'], MyTokens);
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_a: Integer;
var
  S:String;
Begin
  // While GetChar in ['a','A','m','M','p','P','/'] Do
  While CharInSet(GetChar, ['a','A','m','M','p','P','/']) Do
    Next;

  S:=TokenString;

  If PSCCompareText(S, 'a/p') = 0 Then
    Result := tok_dt_ap
  Else
  If PSCCompareText(S, 'ampm') = 0 Then
    Result := tok_dt_ampm_locale
  Else
  If PSCCompareText(S, 'am/pm') = 0 Then
    Result := tok_dt_ampm
  Else
    Result := tok_dt_asis;
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_c: Integer;
begin
  Next;
  Result:=tok_dt_c;
end;

{------------------------------------------------------------------------------}

procedure TPSCDTFormatParser.InitFormatSyntax;
begin
  ClearParserProcs;
  RegisterProc([psdt_Normal], [#1..#255],Parse_asis);
  RegisterProc([psdt_Comment], [#1..#255],Parse_asis);
  RegisterProc([psdt_Normal], [''''],Parse_comment);
  RegisterProc([psdt_Comment], [''''],Parse_comment);
  RegisterProc([psdt_Normal], ['c','C'],Parse_c);
  RegisterProc([psdt_Normal], ['d','D'],Parse_d);
  RegisterProc([psdt_Normal], ['e','E'],Parse_e);
  RegisterProc([psdt_Normal], ['g','G'],Parse_g);
  RegisterProc([psdt_Normal], ['m','M'],Parse_m);
  RegisterProc([psdt_Normal], ['y','Y'],Parse_y);
  RegisterProc([psdt_Normal], ['h','H'],Parse_h);
  RegisterProc([psdt_Normal], ['n','N'],Parse_n);
  RegisterProc([psdt_Normal], ['s','S'],Parse_s);
  RegisterProc([psdt_Normal], ['z','Z'],Parse_z);
  RegisterProc([psdt_Normal], ['t','T'],Parse_t);
  RegisterProc([psdt_Normal], ['a','A'],Parse_a);
  RegisterProc([psdt_Normal], ['/'],Parse_date_sep);
  RegisterProc([psdt_Normal], [':'],Parse_time_sep);
end;

{------------------------------------------------------------------------------}

constructor TPSCDTFormatParser.Create(const ASource:IPSCParserStream);
begin
  inherited;
  InitFormatSyntax;
end;

{------------------------------------------------------------------------------}

procedure TPSCDTFormatParser.AddPart(AToken:Integer);
var
  MyLength : integer;
  OldPartCount : integer;
  MyMultiDateTimeParts : TPSCDTFormatParts;

  procedure AddMultiParts(AToken : integer);
  var
    i : integer;
    S : string;
  begin
    With Parts, FFormat do
    begin
      case AToken of
        tok_dt_t:
          S := GetShortTimeFormat;
        tok_dt_tt:
          S := GetLongTimeFormat;
        tok_dt_c:
          S := GetShortDateFormat + ' ' + GetLongTimeFormat;
        tok_dt_ddddd:
          S := GetShortDateFormat;
        tok_dt_dddddd:
          S := GetLongDateFormat;
      end;
      MyMultiDateTimeParts := PSCParseDateTimeFormatToParts(S, Format);
      OldPartCount := PartCount;
      SetLength(Parts, PartCount + MyMultiDateTimeParts.PartCount);
      if (PartCount + MyMultiDateTimeParts.PartCount) > OldPartCount then
      begin
        for i := OldPartCount to (PartCount + MyMultiDateTimeParts.PartCount - 1) do
        begin
          Parts[i] := MyMultiDateTimeParts.Parts[i - OldPartCount];
          Inc(PartCount);
        end;
        for i := 0 to MyMultiDateTimeParts.PartCount - 1 do
          FKinds := FKinds + [MyMultiDateTimeParts.Parts[i].Kind];
      end;
    end;
  end;

begin
  With Parts do
  begin
    if AToken in [tok_dt_t, tok_dt_tt, tok_dt_c, tok_dt_ddddd, tok_dt_dddddd] then
    begin
      AddMultiParts(AToken);    
      exit;
    end;
    MyLength := Length(Parts);
    If MyLength<=PartCount then
      SetLength(Parts,PSCGetArrayGrowDelta(PartCount) + MyLength);

    If ((PartCount>0) and (Parts[PartCount-1].Kind in [DTFPK_HOUR12, DTFPK_HOUR24])) or
    ((PartCount>1) and (Parts[PartCount-2].Kind in [DTFPK_HOUR12, DTFPK_HOUR24])) then
    begin
      If AToken=tok_dt_m then
        AToken:=tok_dt_n
      else
      If AToken=tok_dt_mm then
        AToken:=tok_dt_nn;
    end;
    Parts[PartCount]:= CPSCDTTokenToKind[AToken];
    FKinds := FKinds + [Parts[PartCount].Kind];
    If (Parts[PartCount].Kind in [DTFPK_AMPM,DTFPK_ASIS]) and
     (AToken <> tok_dt_skip) then
      Parts[PartCount].DataAsStr:=TokenString;

    inc(PartCount);
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCDTFormatParser.ParseFormat;
var
  i : integer;
begin
  While NextToken <> tok_Eof Do
    AddPart(Token);
  with Parts do
  begin
    if DTFPK_AMPM in FKinds then
      for i := 0 to PartCount - 1 do
      begin
        if Parts[i].Kind = DTFPK_HOUR24 then
          Parts[i].Kind := DTFPK_HOUR12;
      end;
  end;
end;

{------------------------------------------------------------------------------}

function PSCParseDateTimeFormatToParts(const AFormatString:String;
 const AFormat : IPSCDateTimeFormat):TPSCDTFormatParts;
var
  MyParser:TPSCDTFormatParser;
begin
  MyParser:=TPSCDTFormatParser.Create(PSCCreateParserStream(AFormatString));
  MyParser.Format := AFormat;
  try
    MyParser.ParseFormat;
    Result:=MyParser.Parts;
  finally
    MyParser.Free;
  end;
end;

{------------------------------------------------------------------------------}

function PSCFormatDateTimeUseParts(const ADate:TDateTime;
 const AFormatParts : TPSCDTFormatParts; const AFormat:IPSCDateTimeFormat) : string;
var
  i : integer;
  MyParams : TPSCFormatDTPartParams;
  MyDateAsArray : TPSCDateTimePartsArray;
begin
  result := '';
  PSCDecodeDate(ADate, MyDateAsArray[dtpYear], MyDateAsArray[dtpMonth],
   MyDateAsArray[dtpDay]);
  PSCDecodeTime(ADate, MyDateAsArray[dtpHour], MyDateAsArray[dtpMinute],
   MyDateAsArray[dtpSecond], MyDateAsArray[dtpMSec]);
  MyParams.ADate := ADate;
  MyParams.ADateASArray := MyDateAsArray;
  MyParams.AFormat := AFormat;
  for i := 0 to AFormatParts.PartCount - 1 do
    with AFormatParts.Parts[i] do
    begin
      MyParams.AFormatPart := AFormatParts.Parts[i];
      result := result + PSCFormatDateTimeUsePart(MyParams);
    end;
end;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_asis: integer;
Const
  MyValidCharSet : TPSCCharSet = ['c', 'C', 'd', 'D', 'e', 'E', 'g', 'G', 'm', 'M', 'y', 'Y',
   'h', 'H', 'n', 'N', 's', 'S', 'z', 'Z', 't', 'T', 'a', 'A', '''', '/', ':'];
Begin
  If ParserState = psdt_Normal Then
    //While (GetChar <> #0) And Not (GetChar In MyValidCharSet) Do
    While (GetChar <> #0) And Not CharInSet(GetChar, MyValidCharSet) Do
      Next;
  If ParserState = psdt_Comment Then
    While (GetChar <> #0) And (GetChar <> '''') Do
      Next;
  Result := tok_dt_asis;
End;

{------------------------------------------------------------------------------}

function PSCGetTimeFormatEx(ALocale:Cardinal;const AMinSecStr:String):String;
var
  MyTimePrefix : String;
  MyHourFormat : String;
  MyTimePostfix : String;
begin
  MyTimePrefix := '';
  MyTimePostfix := '';
  if PSCStrToIntDef(PSCGetLocaleStr(ALocale, LOCALE_ITLZERO, '0'), 0) = 0 then
    MyHourFormat := 'h' else
    MyHourFormat := 'hh';
  if PSCStrToIntDef(PSCGetLocaleStr(ALocale, LOCALE_ITIME, '0'), 0) = 0 then
    if PSCStrToIntDef(PSCGetLocaleStr(ALocale, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
      MyTimePostfix := ' AMPM'
    else
      MyTimePrefix := 'AMPM ';

  Result := MyTimePrefix + MyHourFormat + AMinSecStr + MyTimePostfix;
end;

{------------------------------------------------------------------------------}

function PSCInsertLeadingChars(const S: string; C: Char; AMaxLen : integer) : string;
begin
  Result:=S;
  While Length(Result)<AMaxLen do
    Result:=C+Result;
end;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.CountCharOccurrence(ACharSet:TPSCCharSet;
  const ACToken : Array of Integer): Integer;
Var
  MyCount : integer;
Begin
  MyCount := -1;
  // While (GetChar in ACharSet) Do
  While CharInSet(GetChar, ACharSet) Do
  Begin
    Next;
    inc(MyCount);
  End;
  IF (MyCount <= High(ACToken)) and (MyCount >= Low(ACToken)) then
    Result := ACToken[MyCount]
  Else
    Result:= tok_dt_asis;
End;

{------------------------------------------------------------------------------}

Function TPSCDTFormatParser.Parse_comment: integer;
Begin
  Next;
  Result := tok_dt_skip;
  Case ParserState Of
    psdt_Normal:
      ParserState := psdt_Comment;
    psdt_Comment:
      ParserState := psdt_Normal;
  End;
End;

{------------------------------------------------------------------------------}

function PSCFormat_YearInEraNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  with AParams, AFormat do
    result := GetDateAltFormatted(ADate,'yy');
  while Pos('0',result)=1  do
    Delete(Result, 1, 1);
end;

{------------------------------------------------------------------------------}

function PSCFormat_YearInEraZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  with AParams, AFormat do
    result := GetDateAltFormatted(ADate,'yy');
  Result := PSCInsertLeadingChars(Result,'0',2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_EraShort(const AParams: TPSCFormatDTPartParams) : string;
var
  MySysLocale : TSysLocale;
begin
  with AParams, AFormat do
  begin
    MySysLocale := GetSysLocale;
    result := GetDateAltFormatted(ADate, 'gg');
    case MySysLocale.PriLangID of
      LANG_JAPANESE:
        //Result := Copy(Result, 1, CharToBytelen(Result, 1));
        Result := Copy(Result, 1, CharToElementLen(Result, 1));
      LANG_CHINESE:
        if (MySysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
         //and (ByteToCharLen(Result, Length(Result)) = 4) then
         and (ElementToCharLen(Result, Length(Result)) = 4) then
            //Result := Copy(Result, 1, CharToBytelen(Result, 2));
            Result := Copy(Result, 1, CharToElementLen(Result, 2));
    end;
  end;
end;

{------------------------------------------------------------------------------}

function PSCFormat_EraLong(const AParams: TPSCFormatDTPartParams) : string;
begin
  with AParams, AFormat do
    result := GetDateAltFormatted(ADate,'gg');
end;

{------------------------------------------------------------------------------}

function PSCFormat_DayNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpDay]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_DayZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpDay]), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_DayShort(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetShortDayName(PSCDayOfWeek(AParams.ADate));
end;

{------------------------------------------------------------------------------}

function PSCFormat_DayLong(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetLongDayName(PSCDayOfWeek(AParams.ADate));
end;

{------------------------------------------------------------------------------}

function PSCFormat_MonthNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpMonth]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MonthZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpMonth]), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MonthShort(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetShortMonthName(AParams.ADateAsArray[dtpMonth]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MonthLong(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetLongMonthName(AParams.ADateAsArray[dtpMonth]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_YearTwoDigits(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpYear] mod 100), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_YearFourDigits(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpYear]), '0', 4);
end;

{------------------------------------------------------------------------------}

function PSCFormat_Hour12NoZero(const AParams: TPSCFormatDTPartParams) : string;
var
  My12Hour : word;
begin
  if AParams.ADateAsArray[dtpHour] > 12 then
    My12Hour := AParams.ADateAsArray[dtpHour] - 12
  else
    My12Hour := AParams.ADateAsArray[dtpHour];
  result := PSCIntToStr(My12Hour);
end;

{------------------------------------------------------------------------------}

function PSCFormat_Hour12Zero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCFormat_Hour12NoZero(AParams), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_Hour24NoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpHour]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_Hour24Zero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpHour]), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MinuteNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpMinute]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MinuteZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpMinute]), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_SecondNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpSecond]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_SecondZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpSecond]), '0', 2);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MSecondNoZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCIntToStr(AParams.ADateAsArray[dtpMSec]);
end;

{------------------------------------------------------------------------------}

function PSCFormat_MSecondZero(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := PSCInsertLeadingChars(PSCIntToStr(AParams.ADateAsArray[dtpMSec]), '0', 3);
end;

{------------------------------------------------------------------------------}

function PSCFormat_AMPM(const AParams: TPSCFormatDTPartParams) : string;
var
  MyAMPM : TPSCAMPM;
begin
  MyAMPM := PSCDecodeAMPM(AParams.AFormatPart.DataAsStr, CPSCTIME_AMPM);
  result := MyAMPM[AParams.ADateAsArray[dtpHour] >= 12];
end;

{------------------------------------------------------------------------------}

function PSCFormat_AP(const AParams: TPSCFormatDTPartParams) : string;
var
  MyAP : TPSCAMPM;
begin
  MyAP := PSCDecodeAMPM(AParams.AFormatPart.DataAsStr, CPSCTIME_AP);
  result := MyAP[AParams.ADateAsArray[dtpHour] >= 12];
end;

{------------------------------------------------------------------------------}

function PSCFormat_AMPM_Locale(const AParams: TPSCFormatDTPartParams) : string;
begin
  if AParams.ADateAsArray[dtpHour] >= 12 then
    result := AParams.AFormat.GetTimePMString
  else
    result := AParams.AFormat.GetTimeAMString;
end;

{------------------------------------------------------------------------------}

function PSCFormat_ASIS(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormatPart.DataAsStr;
end;

{------------------------------------------------------------------------------}

function PSCFormat_DateSeparator(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetDateSeparator;
end;

{------------------------------------------------------------------------------}

function PSCFormat_TimeSeparator(const AParams: TPSCFormatDTPartParams) : string;
begin
  result := AParams.AFormat.GetTimeSeparator;
end;

{------------------------------------------------------------------------------}

constructor TPSCWindowsDateTimeFormat.Create(ALocale : Cardinal);
begin
  inherited Create;
  FCurrentLocale := ALocale;
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetDateAltFormatted(const ADate:TDateTime;const AFormat: string): string;
begin
  Result:=PSCGetDateFormatted(FCurrentLocale,DATE_USE_ALT_CALENDAR,
    ADate,AFormat);
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetDateSeparator: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SDATE, '/');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetLongDateFormat: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SLONGDATE, 'mmmm d, yyyy');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetLongDayName(const AIndex: Integer): String;
const
  MyDefLongDayNames : array[1..7] of string = ('Sunday', 'Monday', 'Tuesday',
  'Wednesday', 'Thursday', 'Friday', 'Saturday');
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SDAYNAME1 + (AIndex + 5) mod 7,
   MyDefLongDayNames[AIndex]);
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetLongMonthName(const AIndex: Integer): String;
const
  MyDefLongMonthNames : array[1..12] of string = ('January', 'February', 'March',
   'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SMONTHNAME1 + AIndex - 1,
   MyDefLongMonthNames[AIndex]);
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetLongTimeFormat: String;
begin
  result:=PSCGetTimeFormatEx(FCurrentLocale,':mm:ss');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetShortDateFormat: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SSHORTDATE, 'm/d/yy');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetShortDayName(const AIndex: Integer): String;
const
  MyDefShortDayNames : array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed',
   'Thu', 'Fri', 'Sat');
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SABBREVDAYNAME1 + (AIndex + 5) mod 7,
   MyDefShortDayNames[AIndex]);
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetShortMonthName(const AIndex: Integer): String;
const
  MyDefShortMonthNames : array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr',
   'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_SABBREVMONTHNAME1 + AIndex - 1,
   MyDefShortMonthNames[AIndex]);
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetShortTimeFormat: String;
begin
  result:=PSCGetTimeFormatEx(FCurrentLocale,':mm');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetSysLocale: TSysLocale;
begin
  result := SysLocale;
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetTimeAMString: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_S1159, 'am');
end;

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetTimePMString: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_S2359, 'pm');
end;                                           

{------------------------------------------------------------------------------}

function TPSCWindowsDateTimeFormat.GetTimeSeparator: String;
begin
  result := PSCGetLocaleStr(FCurrentLocale, LOCALE_STIME, ':');
end;

{------------------------------------------------------------------------------}

function PSCWindowsDateTimeFormat(ALocale : Cardinal):IPSCDateTimeFormat;
begin
  result := TPSCWindowsDateTimeFormat.Create(ALocale);
end;

{------------------------------------------------------------------------------}

function PSCDecodeAMPM(const AMPM : string; const ADefault : TPSCAMPM) : TPSCAMPM;
var
  P : integer;
begin
  P := Pos('/', AMPM);
  if P > 0 then
  begin
    result[false] := Copy(AMPM, 1, P - 1);
    result[true] := Copy(AMPM, P + 1, MaxInt);
  end
  else
  begin
    result[false] := ADefault[false];
    result[true] := ADefault[true];
  end;
end;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetLongDateFormat(Const V: String);
Begin
  If FState.LongDateFormat <> V Then
    Begin
      FState.LongDateFormat := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetShortDateFormat(Const V: String);
Begin
  If FState.ShortDateFormat <> V Then
    Begin
      FState.ShortDateFormat := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetLongTimeFormat(Const V: String);
Begin
  If FState.LongTimeFormat <> V Then
    Begin
      FState.LongTimeFormat := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetShortTimeFormat(Const V: String);
Begin
  If FState.ShortTimeFormat <> V Then
    Begin
      FState.ShortTimeFormat := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetLongTimeFormat: String;
Begin
  Result := FState.LongTimeFormat;
  If Result = '' Then
    Result := GetDefaultLongTimeFormat;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetLongDateFormat: String;
Begin
  Result := FState.LongDateFormat;
  If Result = '' Then
    Result := GetDefaultLongDateFormat;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetShortTimeFormat: String;
Begin
  Result := FState.ShortTimeFormat;
  If Result = '' Then
    Result := GetDefaultShortTimeFormat;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetTimeAMString: String;
Begin
  Result := FState.TimeAMString;
  If Result = '' Then
    Result := GetDefaultTimeAMString;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetTimePMString: String;
Begin
  Result := FState.TimePMString;
  If Result = '' Then
    Result := GetDefaultTimePMString;
End;

{------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetShortDateFormat: String;
Begin
  Result := FState.ShortDateFormat;
  If Result = '' Then
    Result := GetDefaultShortDateFormat;
End;

{------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDateSeparator:String;
begin
  If FState.DateSeparator <> '' Then
    Result:=FState.DateSeparator
  else
    Result:=GetDefaultDateSeparator;
end;

{------------------------------------------------------------------}

function TPSCDateTimeFormat.GetTimeSeparator:String;
begin
  If FState.TimeSeparator <> '' Then
    Result:=FState.TimeSeparator
  else
    Result:=GetDefaultTimeSeparator;
end;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetDateSeparator(Const V: String);
Begin
  If FState.DateSeparator <> V Then
    Begin
      FState.DateSeparator := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetTimeSeparator(Const V: String);
Begin
  If FState.TimeSeparator <> V Then
    Begin
      FState.TimeSeparator := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetTimeAMString(Const V: String);
Begin
  If FState.TimeAMString <> V Then
    Begin
      FState.TimeAMString := V;
      Changed;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetTimePMString(Const V: String);
Begin
  If FState.TimePMString <> V Then
    Begin
      FState.TimePMString := V;
      Changed;
    End;
End;

{--------------------------------------}

procedure TPSCDateTimeFormat.SetDateKind(V:TPSCDateFormatKind);
begin
  If FDateKind<>V then
  begin
    FDateKind:=V;
    Changed;
  end;
end;

{--------------------------------------}

procedure TPSCDateTimeFormat.SetTimeKind(V:TPSCDateFormatKind);
begin
  If FTimeKind<>V then
  begin
    FTimeKind:=V;
    Changed;
  end;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsLongDateFormatStored:Boolean;
begin
  Result:=LongDateFormat<>GetDefaultLongDateFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsShortDateFormatStored:Boolean;
begin
  Result:=ShortDateFormat<>GetDefaultShortDateFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsLongTimeFormatStored:Boolean;
begin
  Result:=LongTimeFormat<>GetDefaultLongTimeFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsShortTimeFormatStored:Boolean;
begin
  Result:=ShortTimeFormat<>GetDefaultShortTimeFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsDateSeparatorStored:Boolean;
begin
  Result:=DateSeparator<>GetDefaultDateSeparator;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsTimeSeparatorStored:Boolean;
begin
  Result:=TimeSeparator<>GetDefaultTimeSeparator;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsTimeAMStringStored:Boolean;
begin
  Result:=AMSymbol<>GetDefaultTimeAMString;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.IsTimePMStringStored:Boolean;
begin
  Result:=PMSymbol<>GetDefaultTimePMString;
end;

{------------------------------------------------------------------------------}

type
  TPSCDateTimeFormatToInterface=class(TInterfacedObject,IPSCDateTimeFormat)
  private
    FFormat:TPSCDateTimeFormat;
    function GetSysLocale:TSysLocale;
    function GetTimeAMString:String;
    function GetTimePMString:String;
    function GetDateSeparator:String;
    function GetTimeSeparator:String;
    function GetLongDateFormat:String;
    function GetShortDateFormat:String;
    function GetLongTimeFormat:String;
    function GetShortTimeFormat:String;
    function GetShortDayName(const AIndex:Integer):String;
    function GetLongDayName(const AIndex:Integer):String;
    function GetLongMonthName(const AIndex:Integer):String;
    function GetShortMonthName(const AIndex:Integer):String;
    function GetDateAltFormatted(const ADate:TDateTime;const AFormat:string):string;
  public
    constructor Create(AFormat:TPSCDateTimeFormat);
  end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetSysLocale:TSysLocale;
begin
  Result:=FFormat.GetSysLocale;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetTimeAMString:String;
begin
  Result:=FFormat.GetTimeAMString;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetTimePMString:String;
begin
  Result:=FFormat.GetTimePMString;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetDateSeparator:String;
begin
  Result:=FFormat.GetDateSeparator;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetTimeSeparator:String;
begin
  Result:=FFormat.GetTimeSeparator;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetLongDateFormat:String;
begin
  Result:=FFormat.GetLongDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetShortDateFormat:String;
begin
  Result:=FFormat.GetShortDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetLongTimeFormat:String;
begin
  Result:=FFormat.GetLongTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetShortTimeFormat:String;
begin
  Result:=FFormat.GetShortTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetShortDayName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetShortDayName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetLongDayName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetLongDayName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetLongMonthName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetLongMonthName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetShortMonthName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetShortMonthName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormatToInterface.GetDateAltFormatted(const ADate:TDateTime;
  const AFormat:string):string;
begin
  Result:=FFormat.GetDateAltFormatted(ADate,AFormat);
end;

{------------------------------------------------------------------------------}

constructor TPSCDateTimeFormatToInterface.Create(AFormat:TPSCDateTimeFormat);
begin
  inherited Create;
  FFormat:=AFormat;
end;

{------------------------------------------------------------------------------}

constructor TPSCDateTimeFormat.Create(AOwner:TPersistent;
  const AFormat:IPSCDateTimeFormat);
begin
  inherited Create(AOwner);
  FFormat:=AFormat;
  FThisFormat:=TPSCDateTimeFormatToInterface.Create(Self);
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultShortDateFormat: String;
begin
  Result:=FFormat.GetShortDateFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultLongDateFormat: String;
begin
  Result:=FFormat.GetLongDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDefaultDateSeparator:String;
begin
  Result:=FFormat.GetDateSeparator;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDefaultTimeSeparator:String;
begin
  Result:=FFormat.GetTimeSeparator;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultLongTimeFormat: String;
begin
  Result:=FFormat.GetLongTimeFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultShortTimeFormat: String;
begin
  Result:=FFormat.GetShortTimeFormat;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultTimeAMString: String;
begin
  Result:=FFormat.GetTimeAMString;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.GetDefaultTimePMString: String;
begin
  Result:=FFormat.GetTimePMString;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetTimeFormat:String;
begin
  If TimeKind=dfkShort then
    Result:=GetShortTimeFormat
  else
    Result:=GetLongTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDateFormat:String;
begin
  If DateKind=dfkShort then
    Result:=GetShortDateFormat
  else
    Result:=GetLongDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDateTimeFormat:String;
begin
  Result:=GetDateFormat + ' ' + GetTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetFormat(AKind:TPSCDateTimeKind):String;
begin
  case AKind of
    cpkDate:
      Result:=GetDateFormat;
    cpkTime:
      Result:=GetTimeFormat;
  else
      Result:=GetDateTimeFormat;
  end;
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.ToStringEx(Const V: TDateTime;AKind:TPSCDateTimeKind): String;
var
  MyFormat:String;
  MyDateTime:TDateTime;
begin
  case AKind of
   cpkDate:
     MyDateTime:=PSCDateOf(V);
   cpkTime:
     MyDateTime:=PSCTimeOf(V);
   else
     begin
       MyDateTime:=V;
       If PSCDateOf(MyDateTime)=0 then
         AKind:=cpkTime
       else
       If PSCTimeOf(MyDateTime)=0 then
         AKind:=cpkDate;
     end;
  end;

  If (MyDateTime=0) then
  begin
    Result:=SysUtils.DateToStr(0);
    exit;
  end;

  MyFormat:=GetFormat(AKind);

  Result:=ToStringEx(MyDateTime,MyFormat);
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.ToStringEx(Const V: TDateTime;
  const AFormat:String): String;
var
  MyParts:TPSCDTFormatParts;
begin
  MyParts:=PSCParseDateTimeFormatToParts(AFormat,ThisFormat);
  Result:=PSCFormatDateTimeUseParts(V,MyParts,ThisFormat);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.AsRecord:TPSCDateTimeFormatRec;
begin
  With Result do
  begin
    ShortDateFormat := GetShortDateFormat;
    LongDateFormat  := GetLongDateFormat;

    ShortTimeFormat := GetShortTimeFormat;
    LongTimeFormat  := GetLongTimeFormat;

    DateSeparator   := GetDateSeparator;
    TimeSeparator   := GetTimeSeparator;
    TimeAMString    := GetTimeAMString;
    TimePMString    := GetTimePMString;
  end;
end;

{------------------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.SetSystemFormat;
begin
  FSavedState:=PSCSysDTFormatToRec;
  PSCRecToSysDTFormat(AsRecord);
end;

{------------------------------------------------------------------------------}

Procedure TPSCDateTimeFormat.RestoreSystemFormat;
begin
  PSCRecToSysDTFormat(FSavedState);
end;

{------------------------------------------------------------------------------}

Function TPSCDateTimeFormat.FromString(Const S: String;
  AKind:TPSCDateTimeKind;const ADefault:TDateTime): TDateTime;
begin
  SetSystemFormat;
  try
    case AKind of
     cpkDate:
       Result := PSCDateOf(PSCStrToDateEx(S,ADefault));
     cpkTime:
       Result := PSCTimeOf(PSCStrToTimeEx(S,ADefault));
     else
       Result := PSCStrToDateTimeEx(S,ADefault);
    end;
  finally
    RestoreSystemFormat;
  end;
end;

{------------------------------------------------------------------------------}

type
  TPSCSysUtilsDateTimeFormat = class(TInterfacedObject, IPSCDateTimeFormat)
  private
  public
    function GetSysLocale:TSysLocale;
    function GetTimeAMString:String;
    function GetTimePMString:String;
    function GetDateSeparator:String;
    function GetTimeSeparator:String;
    function GetLongDateFormat:String;
    function GetShortDateFormat:String;
    function GetLongTimeFormat:String;
    function GetShortTimeFormat:String;
    function GetShortDayName(const AIndex:Integer):String;
    function GetLongDayName(const AIndex:Integer):String;
    function GetLongMonthName(const AIndex:Integer):String;
    function GetShortMonthName(const AIndex:Integer):String;
    function GetDateAltFormatted(const ADate:TDateTime;const AFormat:string):string;
  end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetSysLocale:TSysLocale;
begin
  Result:=SysUtils.SysLocale;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetTimeAMString:String;
begin
  Result:=FormatSettings.TimeAMString;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetTimePMString:String;
begin
  Result:=FormatSettings.TimePMString;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetDateSeparator:String;
begin
  Result:=FormatSettings.DateSeparator;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetTimeSeparator:String;
begin
  Result:=FormatSettings.TimeSeparator;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetLongDateFormat:String;
begin
  Result:=FormatSettings.LongDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetShortDateFormat:String;
begin
  Result:=FormatSettings.ShortDateFormat;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetLongTimeFormat:String;
begin
  Result:=FormatSettings.LongTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetShortTimeFormat:String;
begin
  Result:=FormatSettings.ShortTimeFormat;
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetShortDayName(const AIndex:Integer):String;
begin
  Result:=FormatSettings.ShortDayNames[AIndex];
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetLongDayName(const AIndex:Integer):String;
begin
  Result:=FormatSettings.LongDayNames[AIndex];
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetLongMonthName(const AIndex:Integer):String;
begin
  Result:=FormatSettings.ShortMonthNames[AIndex];
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetShortMonthName(const AIndex:Integer):String;
begin
  Result:=FormatSettings.ShortMonthNames[AIndex];
end;

{------------------------------------------------------------------------------}

function TPSCSysUtilsDateTimeFormat.GetDateAltFormatted(const ADate:TDateTime;
 const AFormat:string):string;
begin
  Result:=FormatDateTime(AFormat,ADate);
end;

{------------------------------------------------------------------------------}

function PSCSysUtilsDateTimeFormat:IPSCDateTimeFormat;
begin
  Result:=TPSCSysUtilsDateTimeFormat.Create;
end;

{------------------------------------------------------------------------------}

var
  MyDefaultDateTimeFormat:IPSCDateTimeFormat;

function PSCGetDefaultDateTimeFormat:IPSCDateTimeFormat;
begin
  If MyDefaultDateTimeFormat=nil then
    PSCSetDefaultDateTimeFormat(PSCSysUtilsDateTimeFormat);
  Result:=MyDefaultDateTimeFormat;
end;

{------------------------------------------------------------------------------}

procedure PSCSetDefaultDateTimeFormat(const AFormat:IPSCDateTimeFormat);
begin
  MyDefaultDateTimeFormat:=AFormat;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetSysLocale:TSysLocale;
begin
  Result:=FFormat.GetSysLocale;
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetShortDayName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetShortDayName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetLongDayName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetLongDayName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetLongMonthName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetLongMonthName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetShortMonthName(const AIndex:Integer):String;
begin
  Result:=FFormat.GetShortMonthName(AIndex);
end;

{------------------------------------------------------------------------------}

function TPSCDateTimeFormat.GetDateAltFormatted(const ADate:TDateTime;
  const AFormat:string):string;
begin                    
  Result:=FFormat.GetDateAltFormatted(ADate,AFormat);
end;

{------------------------------------------------------------------------------}

destructor TPSCDateTimeFormat.Destroy;
begin
  inherited;
end;

{------------------------------------------------------------------------------}

end.
