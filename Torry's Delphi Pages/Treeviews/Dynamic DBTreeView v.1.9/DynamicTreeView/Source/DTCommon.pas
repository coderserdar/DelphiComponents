unit DTCommon;

interface

{$I DTDBTree.Inc}

uses Types;

resourcestring
{$IFDEF RUSSIAN}
  sDuplicateListID = 'Duplicate list Key %s';
  sCanNotFindParent = 'Не могу найти родителя (%s=%s). Нарушена структура таблицы.';
  sMoveNodeQuestion = 'Переместить элемент %s в элемент %s?';
  sCanNotDeleteNode = 'Выбранный элемент %s содержит подэлементы. Удаление запрещено.';
  sDeleteSelectedNode = 'Удалить выбранный элемент?';
  sSearchEnded = 'Достигнут конец дерева.';
  sSearchEndedNotFound = 'Строка %s не найдена.';
  sFind = 'Найти';
  sFindNext = 'Искать далее';
  sSearchIsCanceled = 'Поиск отменен.';
  sDatasetIsNotCustomClientDataSet = 'DataSource Dataset is not TCustomClientDataSet';
  sDatasetIsNotClientDataSet = 'DataSource Dataset is not TClientDataSet';
  sDatasetIsNotTable = 'DataSource Dataset is not TTable';
  sDatasetIsNotCustomADODataSet = 'DataSource Dataset is not TCustomADODataSet';
{$ELSE}
  sDuplicateListID = 'Duplicate list Key %s';
  sCanNotFindParent = 'Can not find parent (%s=%s). Wrong database.';
  sMoveNodeQuestion = 'Move node %s into node %s?';
  sCanNotDeleteNode = 'Selected node %s has children. Can not delete.';
  sDeleteSelectedNode = 'Delete selected node?';
  sSearchEnded = 'End of tree reached.';
  sSearchEndedNotFound = 'Search string %s not found.';
  sFind = 'Find';
  sFindNext = 'Find next';
  sSearchIsCanceled = 'Search is canceled.';
  sDatasetIsNotCustomClientDataSet = 'DataSource Dataset is not TCustomClientDataSet';
  sDatasetIsNotClientDataSet = 'DataSource Dataset is not TClientDataSet';
  sDatasetIsNotTable = 'DataSource Dataset is not TTable';
  sDatasetIsNotCustomADODataSet = 'DataSource Dataset is not TCustomADODataSet';
{$ENDIF}
  sDefaultKey = '1';

function Question(Str: String): Boolean;
procedure Information(Str: String);
function StringAsArray(const AString: String; ADivider: String = ';'): TStringDynArray;
function ArrayAsString(StrArray: TStringDynArray; ADivider: String = ';'): String;
function ExtractString(const AString, ASeparator: string; var Pos: Integer): string;
function VarArrayToKey(AArray: Variant): String;
function VariantIsEmpty(AArray: Variant): Boolean;
function KeyToVarArray(Key: String): Variant;
function SameKeys(Key1, Key2: String): Boolean;
function VarEquals(const V1, V2: Variant): Boolean;
//function SetKeyToDefParent(Key: String): String;
//function DefaultParentKey(Key: String): Boolean;

implementation

uses Forms, Windows, Variants;

resourcestring
  sQuestion    = 'Question';
  sWarning     = 'Information';

function Question(Str: String): Boolean;
var
  Return: Integer;
begin
  Result := False;
  Return := Application.MessageBox(PChar(Str), PChar(sQuestion), MB_YESNO + MB_ICONQUESTION);
  if Return = IDYES then
    Result := True;
end;

procedure Information(Str: String);
begin
  Application.MessageBox(PChar(Str), PChar(sWarning), MB_OK + MB_ICONINFORMATION);
end;

function ExtractString(const AString, ASeparator: string; var Pos: Integer): string;
var
  i: Integer;
begin
  i := Pos;
  while (i <= Length(AString)) and (AString[i] <> ASeparator) do Inc(i);
    Result := (Copy(AString, Pos, i - Pos));
  if (i <= Length(AString)) and (AString[i] = ASeparator) then Inc(i);
    Pos := i;
end;

function DivideString(const AString, ASeparator: String): TStringDynArray;
var
  i, Pos: Integer;
begin
  SetLength(Result, 0);
  Pos := 1;
  i := 1;
  while Pos <= Length(AString) do
  begin
    SetLength(Result, i);
    Result[i - 1] := ExtractString(AString, ASeparator, Pos);
    Inc(i);
  end;
end;

function StringAsArray(const AString: String; ADivider: String = ';'): TStringDynArray;
begin
  Result := DivideString(AString, ADivider);
end;

function VariantIsEmpty(AArray: Variant): Boolean;
var
  i: Integer;
begin
  if VarArrayDimCount(AArray) = 0 then
    Result := VarIsEmpty(AArray) or (AArray = NULL)
  else
  begin
    Result := True;
    for i := VarArrayLowBound(AArray, 1) to VarArrayHighBound(AArray, 1) do
      if AArray[i] <> NULL then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function VarArrayToKey(AArray: Variant): String;
var
  i: Integer;
  Key: String;
begin
  Result := '';
  if VarArrayDimCount(AArray) = 0 then
    Result := VarToStr(AArray)
  else
    for i := VarArrayLowBound(AArray, 1) to VarArrayHighBound(AArray, 1) do
    begin
      Key := VarToStr(AArray[i]);
      if Key = '' then
        Key := sDefaultKey;
      Result := Result + Key;
      if i < VarArrayDimCount(AArray) then
        Result := Result + ';';
    end;
end;

function KeyToVarArray(Key: String): Variant;
var
  i: Integer;
  StrArray: TStringDynArray;
begin
{
  if Pos(';', Key) = 1 then
    Key := '0' + Key;
}    
  StrArray := StringAsArray(Key);
  if Length(StrArray) = 1 then
    Result := Key
  else
  begin
    Result := VarArrayCreate([0, Length(StrArray) - 1], varVariant);
    for i := Low(StrArray) to High(StrArray) do
      Result[i] := StrArray[i];
  end;
end;

function SameKeys(Key1, Key2: String): Boolean;
var
  i: Integer;
  StrArray1, StrArray2: TStringDynArray;
begin
  StrArray1 := StringAsArray(Key1);
  StrArray2 := StringAsArray(Key2);
  Result := True;
  if Length(StrArray1) <> Length(StrArray2) then
    Result := False
  else
    for i := Low(StrArray1) to High(StrArray2) do
      if (StrArray1[i] <> '') and (StrArray2[i] <> '') then
        if StrArray1[i] <> StrArray2[i] then
        begin
          Result := False;
          Break;
        end;
end;

function CheckKey(Key: String): String;
var
  i: Integer;
  StrArray: TStringDynArray;
begin
  Result := '';
  StrArray := StringAsArray(Key);
  for i := Low(StrArray) to High(StrArray) do
  begin
    Result := Result + StrArray[i];
    if (i < High(StrArray)) and (Result <> '') then
      Result := Result + ';';
  end;
end;

function ArrayAsString(StrArray: TStringDynArray; ADivider: String = ';'): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(StrArray) to High(StrArray) do
  begin
    Result := Result + StrArray[i];
    if (i < High(StrArray)) and (Result <> '') then
      Result := Result + ';';
  end;
end;

{
function SetKeyToDefParent(Key: String): String;
var
  i: Integer;
  StrArray: TStringArray;
begin
  StrArray := StringAsArray(Key);
  Result := sDefault + ';';
  for i := Low(StrArray) + 1 to High(StrArray) do
  begin
    Result := Result + StrArray[i];
    if (i < High(StrArray)) and (Result <> '') then
      Result := Result + ';';
  end;
end;
}

{
function DefaultParentKey(Key: String): Boolean;
var
  StrArray: TStringArray;
begin
  StrArray := StringAsArray(Key);
  Result := StrArray[Low(StrArray)] = sDefault;
end;
}

function VarEquals(const V1, V2: Variant): Boolean;
var
  i: Integer;
begin
  Result := not (VarIsArray(V1) xor VarIsArray(V2));
  if not Result then Exit;
  Result := False;
  try
    if VarIsArray(V1) and VarIsArray(V2) then
    begin
      if (VarArrayDimCount(V1) = VarArrayDimCount(V2)) and
      (VarArrayLowBound(V1, 1) = VarArrayLowBound(V2, 1)) and
      (VarArrayHighBound(V1, 1) = VarArrayHighBound(V2, 1))
      then
      for i := VarArrayLowBound(V1, 1) to VarArrayHighBound(V1, 1) do
      begin
        Result := V1[i] = V2[i];
        if not Result then Exit;
      end
    end
    else
      Result := V1 = V2;
  except
  end;
end;

end.
