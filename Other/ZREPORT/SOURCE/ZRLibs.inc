{ TZRMathLibrary }
type
  TZRMathLibrary = class(TZRElementLibrary)
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Check(Index : Integer; Arguments: TList): Boolean; override;
    function Calculate(Index : Integer; Arguments: TList): Variant; override;
  end;

function TZRMathLibrary.GetCount: Integer;
begin
  Result := 11;
end;

function TZRMathLibrary.GetItem(Index: Integer): String;
const
  Names: array[0..10] of String[8] = (
    'Int', 'Frac', 'SQRT', 'Abs',
    'Sin', 'Cos', 'Tan', 'Atn',
    'Exp', 'Ln', 'Round' );
begin
  Result := Names[Index];
end;

function TZRMathLibrary.Check(Index: Integer; Arguments: TList): Boolean;
begin
  case Index of
    10:  Result := (Arguments.Count in [1, 2]) and
                   (ValueKind(TZRElement(Arguments[0]).Value) in [zvkInteger, zvkFloat, zvkCurrency]) and
                   ((Arguments.Count = 1) or (ValueKind(TZRElement(Arguments[1]).Value) = zvkInteger));
    else Result := (Arguments.Count = 1) and
                   (ValueKind(TZRElement(Arguments[0]).Value) in [zvkInteger, zvkFloat]);
  end;
end;

function TZRMathLibrary.Calculate(Index: Integer; Arguments: TList): Variant;
var
  X : Variant;
var
  Digits: Integer;
  S     : String;
  E     : Integer;
  P, V  : Extended;
begin
  X := TZRElement(Arguments[0]).Value;
  case Index of
     0: Result := Int(X);
     1: Result := Frac(X);
     2: Result := SQRT(X);
     3: Result := Abs(X);
     4: Result := Sin(X);
     5: Result := Cos(X);
     6: Result := Tan(X);
     7: Result := ArcTan(X);
     8: Result := Exp(X);
     9: Result := Ln(X);
    10: begin
          if Arguments.Count = 2 then
            Digits:= TZRElement(Arguments[1]).Value
          else
            Digits:= 0;
          P:= Power(10, Digits);
          Str(TZRElement(Arguments[0]).Value * P: 0: 0, S);
          Val(S, V, E);
          Result:= V / P;
        end;
  end;
end;

{ TZRStringLibrary }
type
  TZRStringLibrary = class(TZRElementLibrary)
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Check(Index : Integer; Arguments: TList): Boolean; override;
    function Calculate(Index : Integer; Arguments: TList): Variant; override;
  end;

function TZRStringLibrary.GetCount: Integer;
begin
  Result := 11;
end;

function TZRStringLibrary.GetItem(Index: Integer): String;
const
  Names : array[0..10] of String[12] = (
    'Trim', 'Len' , 'Upper', 'Lower', 'Proper',
    'Copy', 'Delete', 'Pos', 'Str', 'FormatFloat', 'FormatCurr');
begin
  Result := Names[Index];
end;

function TZRStringLibrary.Check(Index: Integer; Arguments: TList): Boolean;
begin
  case Index of
    5,6: Result := (Arguments.Count in [2,3]) and
                   (ValueKind(TZRElement(Arguments[0]).Value) = zvkString ) and
                   (ValueKind(TZRElement(Arguments[1]).Value) = zvkInteger) and
                   ((Arguments.Count < 3) or (ValueKind(TZRElement(Arguments[2]).Value) = zvkInteger));
    7  : Result := (Arguments.Count in [2{,3}]) and
                   (ValueKind(TZRElement(Arguments[0]).Value) = zvkString) and
                   (ValueKind(TZRElement(Arguments[1]).Value) = zvkString);
    8: Result := (Arguments.Count = 1);
    9,
   10: Result := (Arguments.Count = 2) and
                 (ValueKind(TZRElement(Arguments[0]).Value) = zvkString) and
                 (ValueKind(TZRElement(Arguments[1]).Value) in [zvkInteger, zvkFloat]);
    else Result := (Arguments.Count = 1) and
                   (ValueKind(TZRElement(Arguments[0]).Value) = zvkString);
  end;
end;

function TZRStringLibrary.Calculate(Index: Integer; Arguments: TList): Variant;
var
  S    : String;
  Start,
  Count: Integer;
begin
  S := TZRElement(Arguments[0]).Value;
  case Index of
    0: Result := trim(S);
    1: Result := length(S);
    2: S := AnsiUpperCase(S);
    3: S := AnsiLowerCase(S);
    4: Result := AnsiUpperCase(copy(S, 1, 1)) +
                 AnsiLowerCase(copy(S, 2, length(S)-1));
    5,
    6: begin
         Start:= TZRElement(Arguments[1]).Value;
         if Arguments.Count = 3 then
           Count:= TZRElement(Arguments[2]).Value
         else
           Count:= length(S);
         case Index of
           5: Result:= copy(S, Start, Count);
           6: begin
                System.Delete(S, Start, Count);
                Result:= S;
              end;
         end;
       end;
    7: Result := System.Pos(S, TZRElement(Arguments[1]).Value);
    8: case ValueKind(TZRElement(Arguments[0]).Value) of
         zvkNull    : Result := '';
         zvkBoolean : if TZRElement(Arguments[0]).Value then
                        Result:= LoadStr(szrBooleanTrue)
                      else
                        Result:= LoadStr(szrBooleanFalse);
         else         Result:= String(TZRElement(Arguments[0]).Value);
       end;
    9: Result:= FormatFloat(TZRElement(Arguments[0]).Value,
                     Double(TZRElement(Arguments[1]).Value));
   10: Result:= FormatCurr (TZRElement(Arguments[0]).Value,
                     Double(TZRElement(Arguments[1]).Value));
  end;
end;

{ TZRConstantLibrary }
type
  TZRConstantLibrary = class(TZRElementLibrary)
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Check(Index : Integer; Arguments: TList): Boolean; override;
    function Calculate(Index : Integer; Arguments: TList): Variant; override;
  end;

function TZRConstantLibrary.GetCount: Integer;
begin
  Result := 4;
end;

function TZRConstantLibrary.GetItem(Index: Integer): String;
const
  Names: array[0..3] of String[8] = (
    'True', 'False', 'Date', 'Pi');
begin
  Result := Names[Index];
end;

function TZRConstantLibrary.Check(Index: Integer; Arguments: TList): Boolean;
begin
  Result := (Arguments.Count = 0);
end;

function TZRConstantLibrary.Calculate(Index: Integer; Arguments: TList): Variant;
begin
  case Index of
    0: Result := True;
    1: Result := False;
    2: Result := Date;
    3: Result := Pi;
  end;
end;

{ TZRDateTimeLibrary }
type
  TZRDateTimeLibrary = class(TZRElementLibrary)
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Check(Index : Integer; Arguments: TList): Boolean; override;
    function Calculate(Index : Integer; Arguments: TList): Variant; override;
  end;

function TZRDateTimeLibrary.GetCount: Integer;
begin
  Result := 5;
end;

function TZRDateTimeLibrary.GetItem(Index: Integer): String;
const
  Names: array[0..4] of String[8] = (
    'Year', 'Month', 'Day', 'DMY', 'YMD');
begin
  Result := Names[Index];
end;

function TZRDateTimeLibrary.Check(Index: Integer; Arguments: TList): Boolean;
begin
  case Index of
    0,1,2: Result := (Arguments.Count = 1) and
                     (ValueKind(TZRElement(Arguments[0]).Value) = zvkDateTime);
    3, 4 : Result := (Arguments.Count = 3) and
                     (ValueKind(TZRElement(Arguments[0]).Value) = zvkInteger) and
                     (ValueKind(TZRElement(Arguments[1]).Value) = zvkInteger) and
                     (ValueKind(TZRElement(Arguments[2]).Value) = zvkInteger);
    else   Result := False;
  end;
end;

function TZRDateTimeLibrary.Calculate(Index: Integer; Arguments: TList): Variant;
var
  D,M,Y: Word;
begin
  case Index of
    0,1,2: begin
             DecodeDate(TZRElement(Arguments[0]).Value, Y, M, D);
             case Index of
               0: Result:= Y;
               1: Result:= M;
               2: Result:= D;
             end;
           end;
    3: Result := EncodeDate(
                   TZRElement(Arguments[2]).Value,
                   TZRElement(Arguments[1]).Value,
                   TZRElement(Arguments[0]).Value );
    4: Result := EncodeDate(
                   TZRElement(Arguments[0]).Value,
                   TZRElement(Arguments[1]).Value,
                   TZRElement(Arguments[2]).Value );
  end;
end;

{ TZRGeneralLibrary }
type
  TZRGeneralLibrary = class(TZRElementLibrary)
  protected
    function GetCount : Integer; override;
    function GetItem(Index: Integer): String; override;
    function Check(Index : Integer; Arguments: TList): Boolean; override;
    function Calculate(Index : Integer; Arguments: TList): Variant; override;
  end;

function TZRGeneralLibrary.GetCount: Integer;
begin
  Result := 3;
end;

function TZRGeneralLibrary.GetItem(Index: Integer): String;
const
  Names: array[0..2] of String[8] = (
    'not', 'iif', 'isnull');
begin
  Result := Names[Index];
end;

function TZRGeneralLibrary.Check(Index: Integer; Arguments: TList): Boolean;
begin
  case Index of
    0: Result := (Arguments.Count = 1) and
                 (ValueKind(TZRElement(Arguments[0]).Value) in [zvkInteger, zvkBoolean]);
    1: Result := (Arguments.Count = 3) and
                 (ValueKind(TZRElement(Arguments[0]).Value) = zvkBoolean);
    2: Result := (Arguments.Count > 0);
    else Result := False;
  end;
end;

function TZRGeneralLibrary.Calculate(Index: Integer; Arguments: TList): Variant;
var
  i: Integer;
begin
  case Index of
    0: Result := not TZRElement(Arguments[0]).Value;
    1: if TZRElement(Arguments[0]).Value then
         Result := TZRElement(Arguments[1]).Value
       else
         Result := TZRElement(Arguments[2]).Value;
    2: begin
         i := 0;
         while VarIsEmpty(Result) and (i < Arguments.Count) do begin
           if ValueKind(TZRElement(Arguments[i]).Value) in [zvkError,zvkNull] then
             Inc(i)
           else
             Result := TZRElement(Arguments[i]).Value;
         end;
       end;
  end;
end;


