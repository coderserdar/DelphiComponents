{*******************************************************}
{File:      NCDTUtil.PAS                                }
{Revision:  1.08 / 06.02.2000                           }
{Comment:   Date utilities                              }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCDTUtil;

interface

type
    TNCDay = (dyMonday, dyTuesday, dyWednesday, dyThursday, dyFriday, dySaturday, dySunday);
    TNCDays = set of TNCDay;
    TNCDateOrder = (doMDY, doDMY, doYMD);

function DTBow(ADate: TDateTime): TDateTime;
function DTBom(ADate: TDateTime): TDateTime;
function DTBoq(ADate: TDateTime): TDateTime;
function DTBoy(ADate: TDateTime): TDateTime;
function DTEow(ADate: TDateTime): TDateTime;
function DTEom(ADate: TDateTime): TDateTime;
function DTEoq(ADate: TDateTime): TDateTime;
function DTEoy(ADate: TDateTime): TDateTime;
function DTDay(ADate: TDateTime): Integer;
function DTDayOfWeek(ADate: TDateTime): TNCDay;
function DTWeak(ADate: TDateTime): Integer;
function DTMonth(ADate: TDateTime): Integer;
function DTQuarter(ADate: TDateTime): Integer;
function DTYear(ADate: TDateTime): Integer;
function DTDaysMonth(AYear, AMonth: Integer): Integer;
function DTDaysQuarter(AYear, AQuart: Integer): Integer;
function DTDaysYear(AYear: Integer): Integer;
function DTIsLeap(AYear: Integer): Boolean;
function DTMoveDate(ADate: TDateTime; ADYear, ADMonth, ADDay: Integer): TDateTime;
procedure DTMoveDateDecoded(var AYear, AMonth, ADay: Word; ADYear, ADMonth, ADDay: Integer);
function DTCMonth(nNum: integer): string;
function DTCDayOfWeek(nDay: TNCDay): string;
function DTShortDateEditMask: String;
function DTIsDateMask(const AMask: String): Boolean;
function DTParseDateMask(const AMask: String; var ARealMask: String): Boolean;
function DTStrToDate(const ADate: String; ASep: Char; const AFormat: String): TDateTime;
function DTVar2DateVar(const ADate: Variant): Variant;
function DTOrder(const DateFormat: string): TNCDateOrder;

type
    TNCDateInterval = record
        dFrom, dTo: TDateTime;
    end;

function DInt(d1, d2: TDateTime): TNCDateInterval;
function DIntOverlayed(i1, i2: TNCDateInterval): Boolean;
function DIntUnion(i1, i2: TNCDateInterval): TNCDateInterval;
function DIntIntersect(i1, i2: TNCDateInterval): TNCDateInterval;
function DIntDateIn(i1: TNCDateInterval; d: TDateTime): Boolean;
function DIntDaysLen(i1: TNCDateInterval): Integer;
procedure DIntLength(i1: TNCDateInterval; var AYear, AMonth, ADay: Integer);
function DIntEmpty(i1: TNCDateInterval): Boolean;
function DIntMove(i1: TNCDateInterval; ADYear, ADMonth, ADDay: Integer): TNCDateInterval;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

implementation

Uses SysUtils, Windows, NCUtil
{$IFDEF OCI_D6}
    , Variants
{$ENDIF}
    ;

const
    DaysPerMonth: array[1..12] of Integer =
        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

function DTDayOfWeek(ADate: TDateTime): TNCDay;
begin
    Result := TNCDay((SysUtils.DayOfWeek(ADate) + 5) mod 7);
end;

function DTMoveDate(ADate: TDateTime; ADYear, ADMonth, ADDay: Integer): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    DTMoveDateDecoded(AYear, AMonth, ADay, ADYear, ADMonth, ADDay);
    Result := EncodeDate(AYear, AMonth, ADay);
end;

procedure DTMoveDateDecoded(var AYear, AMonth, ADay: Word; ADYear, ADMonth, ADDay: Integer);
begin
    Inc(ADMonth, (ADYear + (AYear - 1)) * 12 + (AMonth - 1));
    ADYear := ADMonth div 12 + 1;
    ADMonth := ADMonth mod 12 + 1;
    Inc(ADDay, ADay);
    while (ADDay > DTDaysMonth(ADYear, ADMonth)) or (ADDay <= 0) do
        if ADDay <= 0 then begin
            Dec(ADMonth);
            if ADMonth < 1 then begin
                ADMonth := 12;
                Dec(ADYear);
            end;
            Inc(ADDay, DTDaysMonth(ADYear, ADMonth));
        end
        else begin
            Dec(ADDay, DTDaysMonth(ADYear, ADMonth));
            Inc(ADMonth);
            if ADMonth > 12 then begin
                ADMonth := 1;
                Inc(ADYear);
            end;
        end;
    AYear := ADYear; AMonth := ADMonth; ADay := ADDay;
end;

function DTIsLeap(AYear: Integer): Boolean;
begin
    Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DTDaysMonth(AYear, AMonth: Integer): Integer;
begin
    Result := DaysPerMonth[AMonth];
    if (AMonth = 2) and DTIsLeap(AYear) then
        Inc(Result);
end;

function DTDaysQuarter(AYear, AQuart: Integer): Integer;
var
    m: Integer;
begin
    m := (AQuart - 1) * 3 + 1;
    Result := DaysPerMonth[m] + DaysPerMonth[m + 1] + DaysPerMonth[m + 2];
    if (AQuart = 1) and DTIsLeap(AYear) then
        Inc(Result);
end;

function DTDaysYear(AYear: Integer): Integer;
begin
    Result := 365;
    if DTIsLeap(AYear) then
        Result := 366;
end;

function DTDay(ADate: TDateTime): Integer;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := ADay;
end;

function DTWeak(ADate: TDateTime): Integer;
begin
    Result := (Round(ADate - DTBoy(ADate)) - 1) div 7;
end;

function DTMonth(ADate: TDateTime): Integer;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := AMonth;
end;

function DTQuarter(ADate: TDateTime): Integer;
begin
    Result := (DTMonth(ADate) - 1) div 3 + 1;
end;

function DTYear(ADate: TDateTime): Integer;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := AYear;
end;

function DTBow(ADate: TDateTime): TDateTime;
begin
    Result := ADate - Integer(DTDayOfWeek(ADate)) + 1;
end;

function DTBom(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := EncodeDate(AYear, AMonth, 1);
end;

function DTBoq(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := EncodeDate(AYear, (DTQuarter(ADate) - 1) * 3 + 1, 1);
end;

function DTBoy(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := EncodeDate(AYear, 1, 1);
end;

function DTEow(ADate: TDateTime): TDateTime;
begin
    Result := DTBow(ADate) + 6;
end;

function DTEom(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := EncodeDate(AYear, AMonth, DTDaysMonth(AYear, AMonth));
end;

function DTEoq(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    AMonth := (DTQuarter(ADate) - 1) * 3;
    Result := EncodeDate(AYear, AMonth, DTDaysMonth(AYear, AMonth));
end;

function DTEoy(ADate: TDateTime): TDateTime;
var
    AYear, AMonth, ADay: Word;
begin
    DecodeDate(ADate, AYear, AMonth, ADay);
    Result := EncodeDate(AYear, 12, DTDaysMonth(AYear, 12));
end;

function DTCMonth(nNum: integer): string;
begin
    if (nNum >= 1) and (nNum <= 12) then
        Result := LongMonthNames[nNum]
    else
        Result := '';
end;

function DTCDayOfWeek(nDay: TNCDay): string;
begin
    if (Integer(nDay) >= 1) and (Integer(nDay) <= 7) then
        Result := LongDayNames[Integer(nDay)]
    else
        Result := '';
end;

function ConvDTMask(AMask: String): String;
begin
    Result := '!' + StrTran(StrTran(StrTran(AMask, 'd', '9'), 'm', '9'),
        'y', '9') + ';1;0';
end;

function DTShortDateEditMask: String;
begin
    Result := ConvDTMask(ShortDateFormat);
end;

function DTIsDateMask(const AMask: String): Boolean;
begin
    Result := (Length(AMask) > 0) and (UpperCase(Copy(Trim(AMask), 1, 2)) = '$D');
end;

function DTParseDateMask(const AMask: String; var ARealMask: String): Boolean;
var
    s: String;
begin
    Result := True;
    if DTIsDateMask(AMask) then begin
        s := Copy(AMask, 3, Length(AMask));
        if s = '' then
            ARealMask := DTShortDateEditMask
        else
            ARealMask := ConvDTMask(s);
    end
    else
        Result := False;
end;

function DTStrToDate(const ADate: String; ASep: Char; const AFormat: String): TDateTime;
var
    prevShortDateFormat: String;
    prevDateSeparator: Char;
begin
    prevDateSeparator := DateSeparator;
    prevShortDateFormat := ShortDateFormat;
    DateSeparator := ASep;
    ShortDateFormat := AFormat;
    try
        Result := StrToDate(ADate);
    finally
        DateSeparator := prevDateSeparator;
        ShortDateFormat := prevShortDateFormat;
    end;
end;

function DTVar2DateVar(const ADate: Variant): Variant;
var
    tp: Integer;
    s: String;
begin
    tp := VarType(ADate) and varTypeMask;
    if (tp = varString) or (tp = varOleStr) then begin
        s := StrSqueeze(ADate, [' ', DateSeparator], True);
        if Empty(s) then begin
            Result := Null;
            Exit;
        end;
    end;
    Result := ADate;
end;

{$WARNINGS OFF}
function DTOrder(const DateFormat: string): TNCDateOrder;
var
  I: Integer;
begin
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
  Result := doMDY;
end;
{$WARNINGS ON}

// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

function DInt(d1, d2: TDateTime): TNCDateInterval;
begin
    with Result do
        if (d1 < d2) or (d2 = 0.0) then begin
            dFrom := d1;
            dTo := d2;
        end
        else begin
            dFrom := d2;
            dTo := d1;
        end
end;

function DIntOverlayed(i1, i2: TNCDateInterval): Boolean;
begin
    Result := ((i2.dTo = 0.0) or (i1.dFrom < i2.dTo)) and
              ((i1.dTo = 0.0) or (i2.dFrom < i1.dTo));
end;

function DIntUnion(i1, i2: TNCDateInterval): TNCDateInterval;
begin
    Result.dFrom := i1.dFrom;
    if Result.dFrom > i2.dFrom then
        Result.dFrom := i2.dFrom;
    Result.dTo := i1.dTo;
    if (Result.dTo <> 0.0) and ((i2.dTo = 0.0) or (Result.dTo < i2.dTo)) then
        Result.dTo := i2.dTo;
end;

function DIntIntersect(i1, i2: TNCDateInterval): TNCDateInterval;
begin
    Result.dFrom := i1.dFrom;
    if Result.dFrom < i2.dFrom then
        Result.dFrom := i2.dFrom;
    Result.dTo := i1.dTo;
    if (i2.dTo <> 0.0) and ((Result.dTo = 0.0) or (Result.dTo > i2.dTo)) then
        Result.dTo := i2.dTo;
end;

function DIntDateIn(i1: TNCDateInterval; d: TDateTime): Boolean;
begin
    Result := (i1.dFrom <= d) and ((i1.dTo = 0.0) or (i1.dTo > d));
end;

function DIntDaysLen(i1: TNCDateInterval): Integer;
begin
    if i1.dTo = 0.0 then
        Result := 1000000
    else
        Result := Round(i1.dTo - i1.dFrom + 1);
end;

procedure DIntLength(i1: TNCDateInterval; var AYear, AMonth, ADay: Integer);
begin
    DecodeDate(DIntDaysLen(i1), PWord(@AYear)^, PWord(@AMonth)^, PWord(@ADay)^);
end;

function DIntEmpty(i1: TNCDateInterval): Boolean;
begin
    Result := (i1.dTo <> 0.0) and (i1.dTo <= i1.dFrom);
end;

function DIntMove(i1: TNCDateInterval; ADYear, ADMonth, ADDay: Integer): TNCDateInterval;
begin
    Result.dFrom := DTMoveDate(i1.dFrom, ADYear, ADMonth, ADDay);
    if i1.dTo <> 0.0 then
        Result.dTo := DTMoveDate(i1.dTo, ADYear, ADMonth, ADDay)
    else
        Result.dTo := 0.0;
end;

end.
