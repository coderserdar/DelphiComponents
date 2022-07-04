{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TParams utility routines                      }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgDBPrms;

interface
uses vgSystem, vgDB, DB{$IFNDEF _D4_}, DBTables{$ENDIF};

{ --- TParams }
procedure SetParams(Params: TParams; const ParamName: string; const Values: Variant);
{ Assigns values of parameters to Values }

procedure AssignParams(Params: TParams; DataSet: TDataSet);
{ Assigns values of parameters to values of fields in DataSet}

procedure UpdateBlobParams(Params: TParams; const ParamNames: string);
{ Changes DataType property to ftBlob }

procedure ClearParam(Param: TParam; const Nvl: Variant);
{ If Nvl = Null then clears Param else set Param to Nvl }

procedure ClearParams(Params: TParams; const Nvl: Variant);
{ ClearParam for each parameter in Params. Nvl must be a Variant array }

function FindParam(Params: TParams; const ParamName: string): TParam;
{ Returns Param object if exists else nil }

function ParamRequired(Params: TParams; const ParamName: string): TParam;
{ Returns Param object if exists else creates new }

procedure SetUpdateParams(Params: TParams; DataSet: TDataSet);
{ Setup Params like TUpdateSQL requires }

procedure CreateParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
{ Parses Value and creates list of params }

procedure SetInParams(Params: TParams; Value: Variant);

function GetOutParams(Params: TParams): Variant;

function ParamsToVariant(Params: TParams; Macro, Compatible: Boolean): Variant;
{ Creates variant array from Params }

procedure VariantToParams(const VarParams: Variant; Params: TParams; Compatible: Boolean);
{ Creates Params from Value }

{ --- TDBParams }
procedure SetDBParams(Params: TDBParams; const ParamName: string; const Values: Variant);
{ Assigns values of parameters to Values }

procedure AssignDBParams(Params: TDBParams; DataSet: TDataSet);
{ Assigns values of parameters to values of fields in DataSet}

procedure UpdateBlobDBParams(Params: TDBParams; const ParamNames: string);
{ Changes DataType property to ftBlob }

procedure ClearDBParam(Param: TDBParam; const Nvl: Variant);
{ If Nvl = Null then clears Param else set Param to Nvl }

procedure ClearDBParams(Params: TDBParams; const Nvl: Variant);
{ ClearParam for each parameter in Params. Nvl must be a Variant array }

function FindDBParam(Params: TDBParams; const ParamName: string): TDBParam;
{ Returns Param object if exists else nil }

function DBParamRequired(Params: TDBParams; const ParamName: string): TDBParam;
{ Returns Param object if exists else creates new }

procedure SetUpdateDBParams(Params: TDBParams; DataSet: TDataSet);
{ Setup Params like TUpdateSQL requires }

procedure CreateDBParams(List: TDBParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; const Delims: TCharSet);
{ Parses Value and creates list of params }

function DBParamsToVariant(Params: TDBParams; Macro, Compatible: Boolean): Variant;
{ Creates variant array from Params }
{ if Compatible = True then result array is compatible with default form of IProvider.SetParams }

procedure VariantToDBParams(const VarParams: Variant; Params: TDBParams; Compatible: Boolean);
{ Creates Params from Value }

procedure ParamsToDBParams(Params: TParams; DBParams: TDBParams);
procedure DBParamsToParams(DBParams: TDBParams; Params: TParams);

implementation
uses SysUtils, Classes, vgDBUtl;

procedure SetParams(Params: TParams; const ParamName: string; const Values: Variant);
var
  I: Integer;
begin
  if (ParamName <> '') then
    Params.ParamValues[ParamName] := Values
  else if (Params.Count = 1) and not VarIsEmpty(Values) then
    Params[0].Value := Values
  else if (Params.Count > 1) and VarIsArray(Values) then
    for I := 0 to Params.Count - 1 do
      Params[I].Value := Values[I]
end;

procedure AssignParams(Params: TParams; DataSet: TDataSet);
var
  I: Integer;
  Param: TParam;
  Field: TField;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];
    Field := DataSet.FindField(Param.Name);
    if Assigned(Field) then
    begin
      Param.AssignFieldValue(Field, Field.Value);
      if Param.DataType = ftUnknown then
      begin
        Param.Clear;
        Param.DataType := Field.DataType;
      end;
    end;
  end;
end;

procedure UpdateBlobParams(Params: TParams; const ParamNames: string);
{$IFDEF _D3_}
var
  Pos: Integer;
  Param: TParam;
{$ENDIF}
begin
{$IFDEF _D3_}
  Pos := 1;
  while Pos <= Length(ParamNames) do
  begin
    Param := Params.ParamByName(ExtractFieldName(ParamNames, Pos));
    if Param.DataType <> ftBlob then Param.AsBlob := Param.AsString;
  end;
{$ENDIF}
end;

procedure ClearParam(Param: TParam; const Nvl: Variant);
begin
  with Param do if VarIsNull(Nvl) then Clear else Value := Nvl;
end;

procedure ClearParams(Params: TParams; const Nvl: Variant);
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do ClearParam(Params[I], Nvl[I]);
end;

function FindParam(Params: TParams; const ParamName: string): TParam;
{$IFDEF _D4_}
begin
  Result := Params.FindParam(ParamName);
end;
{$ELSE}
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Params.Count - 1 do
  begin
    if AnsiCompareText(Params[I].Name, ParamName) = 0 then
    begin
      Result := Params[I];
      Break;
    end;
  end;
end;
{$ENDIF}

function ParamRequired(Params: TParams; const ParamName: string): TParam;
begin
  Result := FindParam(Params, ParamName);
  if not Assigned(Result) then
    Result := Params.CreateParam(ftUnknown, ParamName, ptUnknown);
end;

procedure SetUpdateParams(Params: TParams; DataSet: TDataSet);
var
  I: Integer;
  Old: Boolean;
  Param: TParam;
  PName: string;
  Field: TField;
  Value: Variant;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];
    PName := Param.Name;
    Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
    if Old then System.Delete(PName, 1, 4);
    Field := DataSet.FindField(PName);
    if not Assigned(Field) then Continue;
    if Old then
      Param.AssignFieldValue(Field, Field.OldValue)
    else begin
      Value := Field.NewValue;
      if VarIsEmpty(Value) then
      begin
        Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end else
        Param.AssignFieldValue(Field, Value);
    end;
  end;
end;

procedure CreateParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end else
        Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
        if FindParam(List, Name) = nil then
        begin
          if Macro then
            List.CreateParam(ftString, Name, ptInput).AsString := TrueSQL else
            List.CreateParam(ftUnknown, Name, ptUnknown);
        end;
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

procedure SetInParams(Params: TParams; Value: Variant);
var
  I: Integer;
  Param: TParam;
  Tmp: TParams;
begin
  Tmp := TParams.Create;
  try
    VariantToParams(Value, Tmp, False);
    for I := 0 to Tmp.Count - 1 do
    begin
      Param := Tmp[I];
      Params.ParamByName(Param.Name).Assign(Param);
    end;
  finally
    Tmp.Free;
  end;
end;

function GetOutParams(Params: TParams): Variant;
var
  I: Integer;
  Param, NewParam: TParam;
  Tmp: TParams;
begin
  Tmp := TParams.Create;
  try
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      if Param.ParamType in [ptOutput, ptInputOutput, ptResult] then
      begin
        NewParam := TParam.Create(Tmp, ptUnknown);
        try
          NewParam.Assign(Param);
        except
          NewParam.Free;
          raise;
        end;
      end;
    end;
    Result := ParamsToVariant(Tmp, False, False);
  finally
    Tmp.Free;
  end;
end;

function ParamsToVariant(Params: TParams; Macro, Compatible: Boolean): Variant;
var
  I: Integer;
  Tmp: Variant;
begin
  if (Params.Count = 0) then
    Result := Null
  else begin
    Result := VarArrayCreate([0, Params.Count - 1], varVariant);
    for I := 0 to Params.Count - 1 do
      with Params[I] do
      begin
        if Compatible then
          Tmp := VarArrayOf([Name, Unassigned, Unassigned, Unassigned]) else
          Tmp := VarArrayOf([Name, Unassigned, Unassigned, Unassigned, Macro]);
        Tmp[3 - 2 * ord(Compatible)] := Value;
        Tmp[1 + ord(Compatible)] := Ord(DataType);
        Tmp[2 + ord(Compatible)] := Ord(ParamType);
        Result[I] := Tmp;
      end;
  end;
end;

procedure VariantToParams(const VarParams: Variant; Params: TParams; Compatible: Boolean);
var
  I: Integer;
  Param: TParam;
begin
  Params.Clear;

  if not VarIsNull(VarParams) then
    for I := 0 to VarArrayHighBound(VarParams, 1) do
    begin
      Param := Params.CreateParam(
        TFieldType(VarParams[I][1 + ord(Compatible)]),
        VarParams[I][0],
        TParamType(VarParams[I][2 + ord(Compatible)]));

      Param.DataType := TFieldType(VarParams[I][1 + ord(Compatible)]);
      if VarIsNull(VarParams[I][3 - 2 * ord(Compatible)]) or VarIsEmpty(VarParams[I][3 - 2 * ord(Compatible)]) then
        Param.Clear else Param.Value := VarParams[I][3 - 2 * ord(Compatible)];
    end;
end;

procedure SetDBParams(Params: TDBParams; const ParamName: string; const Values: Variant);
var
  I: Integer;
begin
  if (ParamName <> '') then
    Params.ParamValues[ParamName] := Values
  else if Params.Count = 1 then
    Params[0].Value := Values
  else if (Params.Count > 1) and VarIsArray(Values) then
    for I := 0 to Params.Count - 1 do
      Params[I].Value := Values[I]
end;

procedure AssignDBParams(Params: TDBParams; DataSet: TDataSet);
var
  I: Integer;
  Param: TDBParam;
  Field: TField;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];
    Field := DataSet.FindField(Param.Name);
    if Assigned(Field) then
    begin
      Param.AssignFieldValue(Field, Field.Value);
      if Param.DataType = ftUnknown then
      begin
        Param.Clear;
        Param.DataType := Field.DataType;
      end;
    end;
  end;
end;

procedure UpdateBlobDBParams(Params: TDBParams; const ParamNames: string);
{$IFDEF _D3_}
var
  Pos: Integer;
  Param: TDBParam;
{$ENDIF}
begin
{$IFDEF _D3_}
  Pos := 1;
  while Pos <= Length(ParamNames) do
  begin
    Param := Params.ParamByName(ExtractFieldName(ParamNames, Pos));
    if Param.DataType <> ftBlob then Param.AsBlob := Param.AsString;
  end;
{$ENDIF}
end;

procedure ClearDBParam(Param: TDBParam; const Nvl: Variant);
begin
  with Param do if VarIsNull(Nvl) then Clear else Value := Nvl;
end;

procedure ClearDBParams(Params: TDBParams; const Nvl: Variant);
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do ClearDBParam(Params[I], Nvl[I]);
end;

function FindDBParam(Params: TDBParams; const ParamName: string): TDBParam;
begin
  Result := Params.FindParam(ParamName);
end;

function DBParamRequired(Params: TDBParams; const ParamName: string): TDBParam;
begin
  Result := FindDBParam(Params, ParamName);
  if not Assigned(Result) then
    Result := Params.CreateParam(ftUnknown, ParamName, vgDB.ptUnknown);
end;

procedure SetUpdateDBParams(Params: TDBParams; DataSet: TDataSet);
var
  I: Integer;
  Old: Boolean;
  Param: TDBParam;
  PName: string;
  Field: TField;
  Value: Variant;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];
    PName := Param.Name;
    Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
    if Old then System.Delete(PName, 1, 4);
    Field := DataSet.FindField(PName);
    if not Assigned(Field) then Continue;
    if Old then
      Param.AssignFieldValue(Field, Field.OldValue)
    else begin
      Value := Field.NewValue;
      if VarIsEmpty(Value) then
      begin
        Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end else
        Param.AssignFieldValue(Field, Value);
    end;
  end;
end;

procedure CreateDBParams(List: TDBParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; const Delims: TCharSet);
var
  DefValue: Variant;
begin
  if Macro then
    DefValue := TrueSQL else
    DefValue := Null;
  List.ParseSQLEx(Value, SpecialChar, True, DefValue)
end;

function DBParamsToVariant(Params: TDBParams; Macro, Compatible: Boolean): Variant;
var
  I: Integer;
  Tmp: Variant;
begin
  if (Params.Count = 0) then
    Result := Null
  else begin
    Result := VarArrayCreate([0, Params.Count - 1], varVariant);
    for I := 0 to Params.Count - 1 do
      with Params[I] do
      begin
        if Compatible then
          Tmp := VarArrayOf([Name, Unassigned, Unassigned, Unassigned]) else
          Tmp := VarArrayOf([Name, Unassigned, Unassigned, Unassigned, Macro]);
        Tmp[3 - 2 * ord(Compatible)] := Value;
        Tmp[1 + ord(Compatible)] := Ord(DataType);
        Tmp[2 + ord(Compatible)] := Ord(ParamType);
        Result[I] := Tmp;
      end;
  end;
end;

procedure VariantToDBParams(const VarParams: Variant; Params: TDBParams; Compatible: Boolean);
var
  I: Integer;
  Param: TDBParam;
  SaveChange: TNotifyEvent;
begin
  SaveChange := Params.OnChange;
  try
    Params.OnChange := nil;
    Params.Clear;

    if not VarIsNull(VarParams) then
      for I := 0 to VarArrayHighBound(VarParams, 1) do
      begin
        Param := Params.CreateParam(
          TFieldType(VarParams[I][1 + ord(Compatible)]),
          VarParams[I][0],
          TDBParamType(VarParams[I][2 + ord(Compatible)]));

        Param.DataType := TFieldType(VarParams[I][1 + ord(Compatible)]);
        if VarIsNull(VarParams[I][3 - 2 * ord(Compatible)]) or VarIsEmpty(VarParams[I][3 - 2 * ord(Compatible)]) then
          Param.Clear else Param.Value := VarParams[I][3 - 2 * ord(Compatible)];
      end;
  finally
    Params.OnChange := SaveChange;
  end;
end;

procedure ParamsToDBParams(Params: TParams; DBParams: TDBParams);
var
  VarParams: Variant;
begin
  VarParams := ParamsToVariant(Params, False, True);
  VariantToDBParams(VarParams, DBParams, True)
end;

procedure DBParamsToParams(DBParams: TDBParams; Params: TParams);
var
  VarParams: Variant;
begin
  VarParams := DBParamsToVariant(DBParams, False, False);
  VariantToParams(VarParams, Params, False)
end;

end.
