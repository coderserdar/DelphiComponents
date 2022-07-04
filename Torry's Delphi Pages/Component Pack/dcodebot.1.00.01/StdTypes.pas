
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit StdTypes;

interface

{$I STD.INC}

uses
  Windows, DB;

{ General array datatypes }

type
  TIntegerArray = array of Integer;
  PIntegerArray = ^TIntegerArray;

  TStringArray = array of string;
  PStringArray = ^TStringArray;

{ Aliases }

  TVariantTypes = TIntegerArray;

{ Array datatype construction routines }

function IntegerArray(const Items: array of Integer): TIntegerArray;
function StringArray(const Items: array of string): TStringArray;

{ The Contains functions checks if the Container holds a copy of Item }

function Contains(const Container: TIntegerArray; Item: Integer): Boolean; overload;
function Contains(const Container, Item: TIntegerArray): Boolean; overload;
function Contains(const Container: TStringArray; Item: string): Boolean; overload;
function Contains(const Container, Item: TStringArray): Boolean; overload;

{ The Combine functions merges multiple arrays into a single array }

procedure Combine(var Dest: TIntegerArray; const Values: TIntegerArray); overload;
procedure Combine(var Dest: TIntegerArray; const Values: array of Integer); overload;
{ function Combine(const Values: array of TStringArray): TStringArray; overload; }

{ General range datatypes }

type
  TIntegerRange = record
    Lo: Integer;
    Hi: Integer;
  end;

  TDateRange = record
    Lo: TDateTime;
    Hi: TDateTime;
  end;

{ Range datatype construction routines }

function IntegerRange(const Lo, Hi: Integer): TIntegerRange;
function DateRange(const Lo, Hi: TDateTime): TDateRange;

{ Range datatype testing routing }

function IsRangeEmpty(Range: TIntegerRange): Boolean; overload;
function IsRangeEmpty(Range: TDateRange): Boolean; overload;

{ TParamItem datatype }

type
  TParamItem = record
    Name: string;
    Value: Variant;
    DataType: TFieldType;
  end;

{ TParamItem construction routines }

function StrParam(const Name: string; const Value: string): TParamItem;
function IntParam(const Name: string; Value: Integer): TParamItem;
function DateTimeParam(const Name: string; const Value: TDateTime): TParamItem;
function DateParam(const Name: string; const Value: TDateTime): TParamItem;
function FloatParam(const Name: string; const Value: Double): TParamItem;

{ Default string formats }

const
  DefaultCurrencyFormat: string = '%.2m';
  DefaultDateFormat: string = 'm/dd/yyyy';
  DefaultTimeFormat: string = 'h:mm:ss';
  DefaultDateTimeFormat: string = 'm/dd/yyyy h:mm:ssAM/PM';

implementation

function IntegerArray(const Items: array of Integer): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, High(Items) - Low(Items) + 1);
  for I := 0 to Length(Result) - 1 do
    Result[I] := Items[Low(Items) + I];
end;

function StringArray(const Items: array of string): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, High(Items) - Low(Items) + 1);
  for I := 0 to Length(Result) - 1 do
    Result[I] := Items[Low(Items) + I];
end;

function Contains(const Container: TIntegerArray; Item: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Container) to High(Container) do
    if Container[I] = Item then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container, Item: TIntegerArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Item) to High(Item) do
    if Contains(Container, Item[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container: TStringArray; Item: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Container) to High(Container) do
    if Container[I] = Item then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container, Item: TStringArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Item) to High(Item) do
    if Contains(Container, Item[I]) then
    begin
      Result := True;
      Break;
    end;
end;

procedure Combine(var Dest: TIntegerArray; const Values: TIntegerArray);
var
  Len: Integer;
  I: Integer;
begin
  Len := Length(Dest);
  I := Len;
  Inc(Len, Length(Values));
  if I = Len then Exit;
  SetLength(Dest, Len);
  Move(PInteger(@Values[0])^, PInteger(@Dest[I])^, Length(Values) *
    SizeOf(Integer));
end;

procedure Combine(var Dest: TIntegerArray; const Values: array of Integer);
var
  Len: Integer;
  I, J: Integer;
begin
  Len := Length(Dest);
  I := High(Values) - Low(Values) + 1;
  if I = 0 then Exit;
  SetLength(Dest, Len + I);
  J := 0;
  for I := Len to Length(Dest) - 1 do
  begin
    Dest[I] := Values[Low(Values) + J];
    Inc(J);
  end;
end;

{function Combine(const Values: array of TIntegerArray): TIntegerArray;
var
  Source: PInteger;
  Dest: PInteger;
  Len: Integer;
  I: Integer;
begin
  Result := nil;
  Len := 0;
  for I := Low(Values) to High(Values) do
    Inc(Len, Length(Values[I]));
  SetLength(Result, Len);
  Dest := @Result[0];
  for I := Low(Values) to High(Values) do
    if Length(Values[I]) > 0 then
    begin
      Source := @Values[I][0];
      Move(Source^, Dest^, Length(Values[I]) * SizeOf(Integer));
      Inc(Dest, Length(Values[I]));
    end;
end;

function Combine(const Values: array of TStringArray): TStringArray;
var
  Len: Integer;
  I, J, K: Integer;
begin
  Result := nil;
  Len := 0;
  for I := Low(Values) to High(Values) do
    Inc(Len, Length(Values[I]));
  SetLength(Result, Len);
  K := 0;
  for I := Low(Values) to High(Values) do
    for J := Low(Values[I]) to High(Values[I]) do
    begin
      Result[K] := Values[I][K];
      Inc(K);
    end;
end;}

function IntegerRange(const Lo, Hi: Integer): TIntegerRange;
begin
  Result.Lo := Lo;
  Result.Hi := Hi;
end;

function DateRange(const Lo, Hi: TDateTime): TDateRange;
begin
  Result.Lo := Lo;
  Result.Hi := Hi;
end;

function IsRangeEmpty(Range: TIntegerRange): Boolean;
begin
  with Range do
    Result := (Lo = 0) and (Hi = 0);
end;

function IsRangeEmpty(Range: TDateRange): Boolean;
begin
  with Range do
    Result := (Lo = 0) and (Hi = 0);
end;

function StrParam(const Name: string; const Value: string): TParamItem;
begin
  Result.Name := Name;
  if Value = '' then
    Result.Value := NULL
  else
    Result.Value := Value;
  Result.DataType := ftString;
end;

function IntParam(const Name: string; Value: Integer): TParamItem;
begin
  Result.Name := Name;
  Result.Value := Value;
  Result.DataType := ftInteger;
end;

function DateTimeParam(const Name: string; const Value: TDateTime): TParamItem;
begin
  Result.Name := Name;
  if Value = 0 then
    Result.Value := NULL
  else
    Result.Value := Value;
  Result.DataType := ftDateTime;
end;

function DateParam(const Name: string; const Value: TDateTime): TParamItem;
var
  Date: TDateTime;
begin
  Date := Trunc(Value);
  Result.Name := Name;
  if Value = 0 then
    Result.Value := NULL
  else
    Result.Value := Date;
  Result.DataType := ftDate;
end;

function FloatParam(const Name: string; const Value: Double): TParamItem;
begin
  Result.Name := Name;
  Result.Value := Value;
  Result.DataType := ftFloat;
end;

end.
