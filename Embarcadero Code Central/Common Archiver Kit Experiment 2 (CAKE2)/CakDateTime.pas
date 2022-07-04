unit CakDateTime;

interface

uses
  Sysutils;

function DecodeTimeStr(Input: string): string;

implementation

function DecodeTimeStr(Input: string): string;
var
  i:    Integer;
  hh, mm, ss, ms, yy, nn, dd: Word;
  Date: TDateTime;
const
  day: array[1..7] of string =
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
  month: array[1..12] of string =
    ('Janurary', 'Feburary', 'March', 'April', 'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December');
  function ret_dddd: string;
   begin
    Result := day[DayOfWeek(Now)];
   end;
  function ret_yyyy: string;
   begin
    Result := IntToStr(yy);
   end;
  function ret_mmmm: string;
   begin
    Result := month[mm];
   end;
  function ret_ddd: string;
   begin
    Result := Copy(day[DayOfWeek(Now)], 0, 3);
   end;
  function ret_mmm: string;
   begin
    Result := Copy(month[DayOfWeek(Now)], 0, 3);
   end;
  function ret_hh2: string;
   begin
    Result := IntToStr(hh);
    if hh < 10 then Result := '0' + Result;
   end;
  function ret_hh: string;
   begin
    if hh > 12 then
      Result := IntToStr(hh - 12)
    else
      Result := IntToStr(hh);

    if Length(Result) = 1 then Result := '0' + Result;
   end;
  function ret_dd: string;
   begin
    Result := IntToStr(dd);
    if dd < 10 then Result := '0' + Result;
   end;
  function ret_yy: string;
   begin
    Result := Copy(IntToStr(yy), 3, 2);
   end;
  function ret_mm: string;
   begin
    Result := IntToStr(mm);
    if mm < 10 then Result := '0' + Result;
   end;
  function ret_nn: string;
   begin
    Result := IntToStr(nn);
    if nn < 10 then Result := '0' + Result;
   end;
  function ret_ss: string;
   begin
    Result := IntToStr(ss);
    if ss < 10 then Result := '0' + Result;
   end;
  function ret_h: string;
   begin
    Result := IntToStr(hh);
   end;
  function ret_d: string;
   begin
    Result := IntToStr(dd);
   end;
  function ret_m: string;
   begin
    Result := IntToStr(mm);
   end;
  function ret_n: string;
   begin
    Result := IntToStr(nn);
   end;
  function ret_s: string;
   begin
    Result := IntToStr(ss);
   end;
  function ret_p: string;
   begin
    if hh > 12 then
      Result := 'PM'
    else
      Result := 'AM';
   end;
  function replace(Source: string; Fromm, Too: string): string;
  var
    i: Integer;
   begin
    i := Pos(Fromm, ' ' + Source);
    if i <> 0 then
     begin
      Dec(i);
      if i = 0 then
        Result := Too + Copy(Source, Length(Fromm), Length(Source) - Length(Fromm) + 1);
      Result := Copy(Source, 0, i - 1) + Too + Copy(Source,
        i + Length(Fromm), Length(Source) - i - Length(Fromm) + 1);
     end
    else
      Result := Source;
   end;
begin
  Date   := Now;
  Result := '';
  if Pos('%', ' ' + Input) = 0 then
    for i := 1 to Length(Input) do
      case Input[i] of
        'y': Result := Result + '%YYYY%';
        'n': Result := Result + '%M%';
        'd': Result := Result + '%D%';
        'h': Result := Result + '%H%';
        'm': Result := Result + '%M%';
        's': Result := Result + '%S%';
        else
          Result := Result + Input[i];
       end
  else
    Result := Input;
  DecodeTime(Date, hh, nn, ss, ms);
  DecodeDate(Date, yy, mm, dd);

  Result := replace(Result, '%YYYY%', ret_yyyy);
  Result := replace(Result, '%MMMM%', ret_mmmm);
  Result := replace(Result, '%DDDD%', ret_dddd);

  Result := replace(Result, '%HHH%', ret_hh2);
  Result := replace(Result, '%MMM%', ret_mmm);
  Result := replace(Result, '%DDD%', ret_ddd);

  Result := replace(Result, '%YY%', ret_yy);
  Result := replace(Result, '%MM%', ret_mm);
  Result := replace(Result, '%DD%', ret_dd);
  Result := replace(Result, '%HH%', ret_hh);
  Result := replace(Result, '%NN%', ret_nn);
  Result := replace(Result, '%SS%', ret_ss);

  Result := replace(Result, '%M%', ret_m);
  Result := replace(Result, '%D%', ret_d);
  Result := replace(Result, '%H%', ret_h);
  Result := replace(Result, '%N%', ret_n);
  Result := replace(Result, '%P%', ret_p);
  Result := replace(Result, '%S%', ret_s);
end;

end.
