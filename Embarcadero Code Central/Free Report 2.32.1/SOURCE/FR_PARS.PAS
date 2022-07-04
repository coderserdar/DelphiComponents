
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Expression parser            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Pars;

interface

{$I FR.inc}

type
  TGetPValueEvent = procedure(const s: String; var v: Variant) of object;
  TFunctionEvent = procedure(const Name: String; p1, p2, p3: Variant;
                             var Val: String) of object;

  TfrParser = class
  private
    FOnGetValue: TGetPValueEvent;
    FOnFunction: TFunctionEvent;
    function GetIdentify(const s: String; var i: Integer): String;
    function GetString(const s: String; var i: Integer):String;
    procedure Get3Parameters(const s: String; var i: Integer;
      var s1, s2, s3: String);
  public
    function Str2OPZ(s: String): String;
    function CalcOPZ(const s: String): Variant;
    function Calc(const s: String): Variant;
    property OnGetValue: TGetPValueEvent read FOnGetValue write FOnGetValue;
    property OnFunction: TFunctionEvent read FOnFunction write FOnFunction;
  end;

function GetBrackedVariable(s: String; var i, j: Integer): String;

implementation

uses SysUtils;

const
  ttGe = #1; ttLe = #2;
  ttNe = #3; ttOr = #4; ttAnd = #5;
  ttInt = #6;  ttFrac = #7;
  ttUnMinus = #9; ttUnPlus = #10; ttStr = #11;
  ttNot = #12; ttMod = #13; ttRound = #14;


function GetBrackedVariable(s: String; var i, j: Integer): String;
var
  c: Integer;
  fl1, fl2: Boolean;
begin
  j := i; fl1 := True; fl2 := True; c := 0;
  Result := '';
  if s = '' then Exit;
  Dec(j);
  repeat
    Inc(j);
    if fl1 and fl2 then
      if s[j] = '[' then
      begin
        if c = 0 then i := j;
        Inc(c);
      end
      else if s[j] = ']' then Dec(c);
    if fl1 then
      if s[j] = '"' then fl2 := not fl2;
    if fl2 then
      if s[j] = '''' then fl1 := not fl1;
  until (c = 0) or (j >= Length(s));
  Result := Copy(s, i + 1, j - i - 1);
end;

function TfrParser.CalcOPZ(const s: String): Variant;
var
  i, j, k, i1, st: Integer;
  s1: String;
  nm: Array[1..8] of Variant;
  Format: (fNone, fDate, fTime);
begin
  st := 1;
  i := 1;
  Format := fNone;
  while i <= Length(s) do
  begin
    j := i;
    case s[i] of
      '+', ttOr:
        nm[st - 2] := nm[st - 2] + nm[st - 1];
      '-':
        nm[st - 2] := nm[st - 2] - nm[st - 1];
      '*', ttAnd:
        nm[st - 2] := nm[st - 2] * nm[st - 1];
      '/':
        if nm[st - 1] <> 0 then
          nm[st - 2] := nm[st - 2] / nm[st - 1] else
          nm[st - 2] := 0;
      '>':
        if nm[st - 2] > nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      '<':
        if nm[st - 2] < nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      '=':
        if nm[st - 2] = nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttNe:
        if nm[st - 2] <> nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttGe:
        if nm[st - 2] >= nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttLe:
        if nm[st - 2] <= nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttInt:
        nm[st - 1] := Int(nm[st - 1]);
      ttFrac:
        nm[st - 1] := Frac(nm[st - 1]);
      ttRound:
        nm[st - 1] := Integer(Round(nm[st - 1]));
      ttUnMinus:
        nm[st - 1] := -nm[st - 1];
      ttUnPlus:;
      ttStr:
        begin
          s1 := nm[st - 1];
          nm[st - 1] := s1;
        end;
      ttNot:
        if nm[st - 1] = 0 then nm[st - 1] := 1 else nm[st - 1] := 0;
      ttMod:
        nm[st - 2] := nm[st - 2] mod nm[st - 1];
      '%':
        begin
          if s[i + 1] in ['d', 'D'] then Format := fDate;
          if s[i + 1] in ['t', 'T'] then Format := fTime;
          Inc(i);
        end;
      ' ': ;
      '[':
        begin
          k := i;
          s1 := GetBrackedVariable(s, k, i);
          if Assigned(FOnGetValue) then
            FOnGetValue(s1, nm[st]);
          Inc(st);
        end
      else
        begin
          if s[i] = '''' then
          begin
            s1 := GetString(s, i);
            while Pos('''' + '''', s1) <> 0 do
              Delete(s1,Pos('''' + '''', s1), 1);
            s1 := Copy(s1, 2, Length(s1) - 2);
            if Format = fNone then
              nm[st] := s1
            else if Format = fDate then
              nm[st] := StrToDate(s1)
            else if Format = fTime then
              nm[st] := StrToTime(s1);
            Format := fNone;
            k := i;
          end
          else
          begin
            k := i;
            s1 := GetIdentify(s, k);
            if s1[1] in ['0'..'9', '.', ','] then
            begin
              for i1 := 1 to Length(s1) do
                if s1[i1] in ['.', ','] then s1[i1] := DecimalSeparator;
              nm[st] := StrToFloat(s1);
            end
            else
              if Assigned(FOnGetValue) then
                FOnGetValue(AnsiUpperCase(s1), nm[st]);
          end;
          i := k;
          Inc(st);
        end;
    end;
    if s[j] in ['+', '-', '*', '/', '>', '<', '=', ttGe, ttLe, ttNe,
      ttOr, ttAnd, ttMod] then
      Dec(st);
    Inc(i);
  end;
  Result := nm[1];
end;

function TfrParser.GetIdentify(const s: String; var i: Integer): String;
var
  k, n: Integer;
begin
  n := 0;
  while (i <= Length(s)) and (s[i] = ' ') do
    Inc(i);
  k := i; Dec(i);
  repeat
    Inc(i);
    while (i <= Length(s)) and
      not (s[i] in [' ', '+', '-', '*', '/', '>', '<', '=', '(', ')']) do
    begin
      if s[i] = '"' then Inc(n);
      Inc(i);
    end;
  until n mod 2 = 0;
  Result := Copy(s, k, i - k);
end;

function TfrParser.GetString(const s: String; var i: Integer): String;
var
  k: Integer;
  f: Boolean;
begin
  k := i; Inc(i);
  repeat
    while (i <= Length(s)) and (s[i] <> '''') do
      Inc(i);
    f := True;
    if (i < Length(s)) and (s[i + 1] = '''') then
    begin
      f := False;
      Inc(i, 2);
    end;
  until f;
  Result := Copy(s, k, i - k + 1);
  Inc(i);
end;

procedure TfrParser.Get3Parameters(const s: String; var i: Integer;
  var s1, s2, s3: String);
var
  c, d, oi, ci: Integer;
begin
  s1 := ''; s2 := ''; s3 := '';
  c := 1; d := 1; oi := i + 1; ci := 1;
  repeat
    Inc(i);
    if s[i] = '(' then Inc(c)
    else if s[i] = ')' then Dec(c);
    if s[i] = '''' then
      if d = 1 then Inc(d) else d := 1;
    if (s[i] = ',') and (c = 1) and (d = 1) then
    begin
      if ci = 1 then
        s1 := Copy(s, oi, i - oi) else
        s2 := Copy(s, oi, i - oi);
      oi := i + 1; Inc(ci);
    end;
  until (c = 0) or (i >= Length(s));
  case ci of
    1: s1 := Copy(s, oi, i - oi);
    2: s2 := Copy(s, oi, i - oi);
    3: s3 := Copy(s, oi, i - oi);
  end;
  Inc(i);
end;

function TfrParser.Str2OPZ(s: String): String;
label 1;
var
  i, i1, j, p, ci, cn: Integer;
  stack: String;
  res, s1, s2, s3, s4, sr: String;
  vr: Boolean;
  c: Char;
  function Priority(c: Char): Integer;
  begin
    case c of
      '(': Priority := 5;
      ')': Priority := 4;
      '=', '>', '<', ttGe, ttLe, ttNe: Priority := 3;
      '+', '-', ttUnMinus, ttUnPlus: Priority := 2;
      '*', '/', ttOr, ttAnd, ttNot, ttMod: Priority := 1;
      ttInt, ttFrac, ttRound, ttStr: Priority := 0;
      else Priority := 0;
    end;
  end;
  procedure ProcessQuotes(var s: String);
  var
    i: Integer;
  begin
    if (Length(s) = 0) or (s[1] <> '''') then Exit;
    i := 2;
    if Length(s) > 2 then
      while i <= Length(s) do
      begin
        if (s[i] = '''') and (i < Length(s)) then
        begin
          Insert('''', s, i);
          Inc(i);
        end;
        Inc(i);
      end;
  end;
begin
  res := '';
  stack := '';
  i := 1; vr := False;
  while i <= Length(s) do
  begin
    case s[i] of
      '(':
        begin
          stack := '(' + stack;
          vr := False;
        end;
      ')':
        begin
          p := Pos('(', stack);
          res := res + Copy(stack, 1, p - 1);
          stack := Copy(stack, p + 1, Length(stack) - p);
        end;
      '+', '-', '*', '/', '>', '<', '=':
        begin
          if (s[i] = '<') and (s[i + 1] = '>') then
          begin
            Inc(i);
            s[i] := ttNe;
          end else
          if (s[i] = '>') and (s[i + 1] = '=') then
          begin
            Inc(i);
            s[i] := ttGe;
          end else
          if (s[i] = '<') and (s[i + 1] = '=') then
          begin
            Inc(i);
            s[i] := ttLe;
          end;

1:        if not vr then
          begin
            if s[i] = '-' then s[i] := ttUnMinus;
            if s[i] = '+' then s[i] := ttUnPlus;
          end;
          vr := False;
          if stack = '' then stack := s[i] + stack
          else
            if Priority(s[i]) < Priority(stack[1]) then
              stack := s[i] + stack
            else
            begin
              repeat
                res := res + stack[1];
                stack := Copy(stack, 2, Length(stack) - 1);
              until (stack = '') or (Priority(stack[1]) > Priority(s[i]));
              stack := s[i] + stack;
            end;
        end;
      ';': break;
      ' ': ;
      else
      begin
        vr := True;
        s2 := '';
        i1 := i;
        if s[i] = '%' then
        begin
          s2 := '%' + s[i + 1];
          Inc(i, 2);
        end;
        if s[i] = '''' then
          s2 := s2 + GetString(s, i)
        else if s[i] = '[' then
        begin
          s2 := s2 + '[' + GetBrackedVariable(s, i, j) + ']';
          i := j + 1;
        end
        else
          s2 := s2 + GetIdentify(s, i);
        c := s[i];
        if (Length(s2) > 0) and (s2[1] in ['0'..'9', '.', ',']) then
          res := res + s2 + ' '
        else
        begin
          s1 := AnsiUpperCase(s2);
          if s1 = 'INT' then
          begin
            s[i - 1] := ttInt;
            Dec(i);
            goto 1;
          end
          else if s1 = 'FRAC' then
          begin
            s[i - 1] := ttFrac;
            Dec(i);
            goto 1;
          end
          else if s1 = 'ROUND' then
          begin
            s[i - 1] := ttRound;
            Dec(i);
            goto 1;
          end
          else if s1 = 'OR' then
          begin
            s[i - 1] := ttOr;
            Dec(i);
            goto 1;
          end
          else if s1 = 'AND' then
          begin
            s[i - 1] := ttAnd;
            Dec(i);
            goto 1;
          end
          else if s1 = 'NOT' then
          begin
            s[i - 1] := ttNot;
            Dec(i);
            goto 1;
          end
          else if s1 = 'STR' then
          begin
            s[i - 1] := ttStr;
            Dec(i);
            goto 1;
          end
          else if s1 = 'MOD' then
          begin
            s[i - 1] := ttMod;
            Dec(i);
            goto 1;
          end
          else if s1 = 'COPY' then
          begin
            Get3Parameters(s, i, s2, s3, s4);
            ci := StrToInt(CalcOPZ(Str2OPZ(s3)));
            cn := StrToInt(CalcOPZ(Str2OPZ(s4)));
            s1 := '''' + Copy(CalcOPZ(Str2OPZ(s2)), ci, cn) + '''';
            ProcessQuotes(s1);
            Delete(s, i1, i - i1);
            Insert('(' + s1 + ')', s, i1);
            i := i1;
          end
          else if s1 = 'IF' then
          begin
            Get3Parameters(s, i, s2, s3, s4);
            if Int(StrToFloat(CalcOPZ(Str2OPZ(s2)))) > 0 then
              s1 := s3 else
              s1 := s4;
            ProcessQuotes(s1);
            Delete(s, i1, i - i1);
            Insert('(' + s1 + ')', s, i1);
            i := i1;
          end
          else if c = '(' then // other function
          begin
            Get3Parameters(s, i, s2, s3, s4);
            sr := '';
            if Assigned(FOnFunction) then
              FOnFunction(s1, s2, s3, s4, sr);
            ProcessQuotes(sr);
            Delete(s, i1, i - i1);
            Insert('(' + sr + ')', s, i1);
            i := i1;
          end
          else res := res + s2 + ' ';
        end;
        Dec(i);
      end;
    end;
    Inc(i);
  end;
  if stack <> '' then res := res + stack;
  Result := res;
end;

function TfrParser.Calc(const s: String): Variant;
begin
  Result := CalcOPZ(Str2OPZ(s));
end;

end.

