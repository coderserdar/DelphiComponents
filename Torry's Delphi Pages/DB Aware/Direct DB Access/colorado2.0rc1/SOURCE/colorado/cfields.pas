{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit cfields;

{$I CDEFINES.INC}

interface
uses Variants, OleCtrls, Classes, SysUtils, sysconst, dbconsts, db;

{$IFDEF VCL40}
const ftVariant = ftParadoxOle;
type TCVariantField = class(TField)
{$ELSE}
type TCVariantField = class(TVariantField)
{$ENDIF}
     protected
       class procedure CheckTypeSize(Value: Integer); override;
       function GetAsDateTime: TDateTime; override;
       function GetAsInteger: Integer; override;
       function GetAsFloat: Double; override;
       function GetAsString: string; override;
       function GetAsBoolean: Boolean; override;
       function GetAsVariant: Variant; override;
       {$IFDEF VCL40}
       function GetDataSize: Word; override;
       {$ELSE}
       function GetDataSize: Integer; override;
       {$ENDIF}
       function GetAsCurrency: Currency; override;
       procedure GetText(var Text: string; DisplayText: Boolean); override;
       procedure SetAsDateTime(Value: TDateTime); override;
       procedure SetAsInteger(Value: Integer); override;
       procedure SetAsString(const Value: string); override;
       procedure SetAsBoolean(Value: Boolean); override;
       procedure SetAsVariant(const Value: Variant); override;
       procedure SetAsCurrency(Value: Currency); override;
       procedure SetAsFloat(Value: Double); override;
       procedure SetText(const Value: string); override;
     public
       constructor Create(AOwner: TComponent); override;
     end;

{$IFDEF VCL40}
type TWideStringField = class(TStringField)
     protected
       class procedure CheckTypeSize(Value: Integer); override;
       function GetAsString: string; override;
       function GetAsVariant: Variant; override;
       function GetAsWideString: WideString; virtual;
       function GetDataSize: Word; override;
       procedure SetAsString(const Value: string); override;
       procedure SetAsWideString(const Value: WideString); virtual;
       procedure SetVarValue(const Value: Variant); override;
     public
       constructor Create(AOwner: TComponent); override;
       property Value: WideString read GetAsWideString write SetAsWideString;
     end;
{$ENDIF}

implementation

{:-- TCVariantField }

class procedure TCVariantField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then DatabaseError(SInvalidFieldSize);
end;

constructor TCVariantField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftVariant);
end;

function TCVariantField.GetAsBoolean: Boolean;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := FALSE;
       Exit;
     end;
  Result := Boolean(v);
  v := Null;
end;

function TCVariantField.GetAsCurrency: Currency;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := 0;
       Exit;
     end;
  Result := Currency(v);
  v := Null;
end;

function TCVariantField.GetAsDateTime: TDateTime;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := 0;
       Exit;
     end;
  Result := TDateTime(v);
  v := Null;
end;

function TCVariantField.GetAsFloat: Double;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := 0;
       Exit;
     end;
  Result := Double(v);
  v := Null;
end;

function TCVariantField.GetAsInteger: Integer;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := 0;
       Exit;
     end;
  Result := Integer(v);
  v := Null;
end;

function TCVariantField.GetAsString: string;
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Result := '';
       Exit;
     end;
  Result := VarToStr(v);
  v := Null;
end;

function TCVariantField.GetAsVariant: Variant;
begin
  GetData(@Result);
end;

{$IFDEF VCL40}
function TCVariantField.GetDataSize: Word;
{$ELSE}
function TCVariantField.GetDataSize: Integer;
{$ENDIF}
begin
  Result := SizeOf(Variant);
end;

procedure TCVariantField.GetText(var Text: string; DisplayText: Boolean);
var v: Variant;
begin
  if not GetData(@v) then
     begin
       Text := '';
       Exit;
     end;
  case VarType(v) of
    varString, varOleStr: Text := '''' + VarToStr(v) + '''';
    varBoolean: if v then Text := STextTrue else Text := STextFalse;
    varNull: Text := '';
    varCurrency: Text := FloatToStrF(v, ffCurrency, 19, CurrencyDecimals);
    else Text := VarToStr(v);
  end;
  v := Null;
end;

procedure TCVariantField.SetAsBoolean(Value: Boolean);
var v: Variant;
begin
  v := Value;
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsCurrency(Value: Currency);
var v: Variant;
begin
  v := Value;
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsDateTime(Value: TDateTime);
var v: Variant;
begin
  v := Value;
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsFloat(Value: Double);
var v: Variant;
begin
  v := Value;
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsInteger(Value: Integer);
var v: Variant;
begin
  v := Value;
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsString(const Value: string);
var v: Variant;
begin
  v := WideString(Value);
  SetData(@v);
  v := Null;
end;

procedure TCVariantField.SetAsVariant(const Value: Variant);
begin
  SetData(@value);
end;

procedure TCVariantField.SetText(const Value: string);
var v_ext: Extended;
    v_curr: Currency;
    v_int, i: Integer;
    v_str: string;
    v: Variant;
begin
  if  (Length(Value) > 1)
  and (Value [1] = '''')
  and (Value [Length(Value)] = '''')
  then begin
         SetAsString(Copy(Value, 2, Length(Value) - 2));
         Exit;
       end;
  v_str := Trim(Value);
  if (UpperCase(v_str) = 'NULL') or (Length(v_str) = 0) then
     begin
       v := Null;
       SetData(@v);
       Exit;
     end;
  i := Pos(AnsiUpperCase(CurrencyString), AnsiUpperCase(v_str));
  if i > 0 then
     begin
       Delete(v_str, i, Length(CurrencyString));
       if TextToFloat(PChar(v_str), v_curr, fvCurrency) then
       begin
         SetAsCurrency(v_curr);
         Exit;
       end;
     end;
  if TextToFloat(PChar(v_str), v_ext, fvExtended) then
     begin
       SetAsFloat(v_ext);
       Exit;
     end;
  if UpperCase(v_str) = Copy(UpperCase(STextFalse), 1, Length(v_str)) then
     begin
       v := FALSE;
       SetData(@v);
       v := Null;
       Exit;
     end;
  if UpperCase(v_str) = Copy(UpperCase(STextTrue), 1, Length(v_str)) then
     begin
       v := TRUE;
       SetData(@v);
       v := Null;
       Exit;
     end;
  Val(Value, v_int, i);
  if i = 0 then
     begin
       SetAsInteger(v_int);
       Exit;
     end;
  try
    SetAsDateTime(StrToDateTime(v_str));
    Exit;
  except
    DatabaseError(SInvalidVarCast);
  end;
  DatabaseError(SInvalidVarCast);
end;

{$IFDEF VCL40}
{:-- TWideStringField }

class procedure TWideStringField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then DatabaseError(SInvalidFieldSize);
end;

constructor TWideStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWideString);
end;

function TWideStringField.GetAsString: string;
begin
  Result := GetAsWideString;
end;

function TWideStringField.GetAsVariant: Variant;
var Res: WideString;
begin
  if not GetData(@Res)
     then Result := Null
     else Result := Res;
end;

function TWideStringField.GetAsWideString: WideString;
begin
  Result := '';
  GetData(@Result);
end;

function TWideStringField.GetDataSize: Word;
begin
  Result := SizeOf(WideString);
end;

procedure TWideStringField.SetAsString(const Value: string);
begin
  SetAsWideString(Value);
end;

procedure TWideStringField.SetAsWideString(const Value: WideString);
var FValue: WideString;
begin
  if Length(Value) > Size then
     begin
       FValue := Copy(Value, 1, Size);
       SetData(@FValue);
     end else
           SetData(@Value);
end;

procedure TWideStringField.SetVarValue(const Value: Variant);
begin
  SetAsWideString(Value);
end;
{$ENDIF}

initialization
  {$IFDEF VCL40}
  RegisterClass(TWideStringField);
  {$ENDIF}
  RegisterClass(TCVariantField);
end.
