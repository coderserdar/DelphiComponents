{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: CommonLib.pas,v 1.59 2003/04/16 13:20:55 mvj Exp $}

{----------------------------------------------------------------------------
  CommonLib          Library functions used within plib

  Company:           Polycon Ab
  Authors:           LAA
----------------------------------------------------------------------------}

unit CommonLib;

interface
{$i common.inc}

uses
  Classes,
  DataType, DataElements, Criteria, Quilt;

type
  TGarbageOwner = class
  private
    fGarbageCan : TList;
  protected
    property GarbageCan : TList read fGarbageCan write fGarbageCan;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOwnership(lstObjects : TList);
    procedure AddOneOwnership(AnObject : TObject);
  end;

  TFileStringList = class(TStringList)
  protected
    procedure SetTextStr(const Value: string); override;
  end;


{/** Copies from FromList to ToList */}
procedure CopyListContent(FromList, ToList : TList);
{/** Empties a list with objects*/}
procedure EmptyListWithObjects(AList:TList);
procedure EmptyStringListWithObjects(AList:TStrings);
procedure EmptyListWithPointers(AList : TList);
procedure EmptyStringListWithPointers(AList : TStrings);
{/** Frees a list with objects*/}
procedure FreeListWithObjects(AList:TList);
procedure FreeStringListWithObjects(AList:TStrings);
procedure FreeListWithPointers(AList : TList);
procedure FreeStringListWithPointers(AList : TStrings);

function FindString( Strings : TStrings; var str: string; var idx : Integer;
  CaseSensitive : Boolean ) : Boolean;
{$ifndef D4_OR_HIGHER}
function Max(a,b : Integer) : Integer;
function Min(a,b : Integer) : Integer;
{$endif D4_OR_HIGHER}

function CreateListCriteria(DataRows : TStrings; TableKeysOnly : Boolean) : TCriteria;
procedure StorageValuesToCriteria(Storage : TAbstractRowStorage; SearchCriteria, ResultCriteria : TCriteria; AffectedFields : TDataFieldSet);

procedure SetRowDefaultsFromCrit(ARow : TDataRow; ACrit: TCriteria);
procedure SetRowDefaultsFromQuiltPatch(ARow : TDataRow; AQuiltPatch: TQuiltPatch);

{/** How many digits does it take to show a MaxValue? */}
function NumberOfDigits(MaxValue : integer) : integer;

{/** A version of normal IntToStr that pads the result if it's too short, normally used
     to pad with eihter space (to make right-centered strings) or zeroes so that the lexical
     and logical (on the integer values) ordering is the same.
     Example PaddedInToStr(1,'0',3) = '001'.
     @author LAA
*/}
function PaddedIntToStr(AValue:integer; PadChar:char; MinLength:integer) : string;
function MergePathAndFileName(ADir, AFile : String) : String;
function IsNumeric(const AStr : string) : Boolean;

function FieldByName(SearchInTable : TDataTable; FieldName: String) : TDataField;

type

  TAbstractFieldList = class
  protected
    function GetCount : Integer; virtual; abstract;
    function GetField(Index : Integer) : TDataField; virtual; abstract;
  public
    function ContainsField(Field : TDataField) : Boolean; virtual; abstract;
    procedure Remove(AField : TDataField); virtual; abstract;
    procedure Clear; virtual; abstract;
    property Field[Index : Integer] : TDataField read GetField; default;
    property Count : Integer read GetCount;
  end;

  TFieldList = class(TAbstractFieldList)
  private
    FFields : TList;
    FDuplicates : TDuplicates;

    procedure SetField(Index : Integer; Field : TDataField);
  protected
    function GetCount : Integer; override;
    function GetField(Index : Integer) : TDataField; override;
  public
    constructor Create;
    constructor CreateFromFieldArray(Fields : array of TDataField);
    destructor Destroy; override;

    procedure Add(Field : TDataField);
//    procedure AddFromFieldArray(Fields : array of TDataField);
    procedure Insert(Index : Integer; Field : TDataField);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Delete(Index : Integer);
    procedure Remove(Field : TDataField); override;
    procedure Clear; override;

    function ContainsField(Field : TDataField) : Boolean; override;
    function IndexOf(AField : TDataField) : Integer;

    procedure Assign(FieldList : TFieldList);
    procedure CopyFrom(FieldList : TFieldList);
    procedure AddFrom(FieldList : TAbstractFieldList);
    procedure CopyFromSet(Fields : TDataFieldSet);
    procedure AddFromSet(Fields : TDataFieldSet);
    procedure AddFromArray(Fields : array of TDataField);
    procedure FieldsToList(Table : TDataTable);
    procedure KeysToList(Table : TDataTable; AddFictiveFields:Boolean=True);

    property Count : Integer read GetCount;
    property Field[Index : Integer] : TDataField read GetField write SetField; default;
    property Duplicates : TDuplicates read FDuplicates write FDuplicates;
  end;

  TDefaultValueFieldList = class(TAbstractFieldList)
  protected
    FFieldsAndValues : TStringList;

    function GetFieldString(AField : TDataField) : String;
    function GetValue(AField : TDataField) : TValue;
    function GetString(Index : Integer) : String;
  protected
    function GetCount : Integer; override;
    function GetField(Index : Integer) : TDataField; override;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsField(AField : TDataField) : Boolean; override;
    procedure Add(AField : TDataField; Value : String);
    procedure AddFromArray( Fields : array of TDataField; Values : array of String);
    procedure Remove(AField : TDataField); override;
    procedure Clear; override;
    procedure CopyFrom(ValueList : TDefaultValueFieldList);

    property FieldValue[AField : TDataField] : TValue read GetValue;
    property Strings[Index : Integer] : String read GetString;
  end;

  TDoubleList = class
  private
    FListId : TList;
    FListObj : TList;

    function GetId(Index: Integer): TObject;
    function GetObj(Index: Integer): TObject;
  protected
    function GetCount : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add( AId, AObj : TObject ) : Integer;
    procedure Delete(Index : Integer);
    function Remove(AId : TObject) : Integer;
    procedure Clear;

    function IndexOf(AId: TObject): Integer;
    function IndexOfObj(AObj: TObject): Integer;

    property Id[Index : Integer] : TObject read GetId;
    property Obj[Index : Integer] : TObject read GetObj; 
    property Count : Integer read GetCount;
  end;

  {/** A running number generatro for any inmemory table that wish to generate
       an incremental sequence of running numbers.
       Developers note: at the moment it always increments the number even when
       not necessary because of other keys. */}
  TInmemoryIncrementGenerator = class(TDefaultRunningNumberGenerator)
  public
    function GetNextTempRunningNumber(Table : TDataTable; const Values : TValueList) : TValue; override;
  end;

  function InmemoryNumberGenerator : TInmemoryIncrementGenerator;
  function SafeStrToInt(const AStr : String; DefaultValue : Integer) : Integer;


implementation

uses
  Math,

//  DataEditorConstants,

  DataTypes, SysUtils;

function SafeStrToInt(const AStr : String; DefaultValue : Integer) : Integer;
begin
{  if AStr = '' then
    Result := DefaultValue
  else
  try
    Result := StrToInt(AStr);
  except
    Result := DefaultValue;
  end; }
  // Exists in SysUtils
  Result := StrToIntDef(AStr, DefaultValue);
end;

procedure StorageValuesToCriteria(Storage : TAbstractRowStorage; SearchCriteria, ResultCriteria : TCriteria; AffectedFields : TDataFieldSet);
  procedure AddField(AList : TList; ACriteria : TCriteria; AIndex : Integer);
  var
    FField : TDataField;
  begin
    FField := Storage.DataTable.Field[AIndex];
    ACriteria[FField].AcceptNone;
    AList.Add(FField);
  end;

var
  i, j : Integer;
  FRows : TStrings;
  FFields : TList;
  FField : TDataField;
  FCriteriaField : TCriteriaField;
begin
  FFields := TList.Create;
  FRows := TStringList.Create;

  if AffectedFields = nil then
    for i := 0 to Storage.DataTable.FieldCount - 1 do
      AddField(FFields, ResultCriteria, i)
  else
    for i := 0 to Storage.DataTable.FieldCount - 1 do
      if AffectedFields.IncludeField(Storage.DataTable, i) then
        AddField(FFields, ResultCriteria, i);

  Storage.GetRows(FRows, SearchCriteria, gaReference);

  for i := 0 to FFields.Count - 1 do
  begin
    FField := FFields.Items[i];
    FCriteriaField := ResultCriteria[FField];
    for j := 0 to FRows.Count - 1 do
      FCriteriaField.AddValue(TDataRow(FRows.Objects[j])[FField]);
  end;

  FFields.Free;
  FRows.Free;
end;

function MergePathAndFileName(ADir, AFile : String) : String;
begin
  if (Length(ADir) > 0) and (ADir[Length(ADir)] <> '\') then
    ADir := ADir + '\';

  Result := ADir + AFile;
end;

function CreateListCriteria(DataRows : TStrings; TableKeysOnly : Boolean) : TCriteria;
var
  iRow, iField : Integer;
  LoopMax : Integer;
  Row : TDataRow;
begin
  Result := TCriteria.Create;

  for iRow := 0 to DataRows.Count - 1 do
  begin
    Row := TDataRow(DataRows.Objects[iRow]);

    if TableKeysOnly then
      LoopMax := Row.DataTable.KeyCount - 1
    else
      LoopMax := Row.DataTable.FieldCount - 1;

    for iField := 0 to LoopMax do
      Result[Row.DataTable.Field[iField]].AddValue(Row.ValueByIndex[iField]);
  end;
end;

procedure CopyListContent(FromList, ToList : TList);
var
  idx : Integer;
begin

  if FromList = nil then
    raise Exception.Create('CopyListContent: Invalid parameter: FromList = nil')
  else if ToList = nil then
    raise Exception.Create('CopyListContent: Invalid parameter: ToList = nil');

  for idx := 0 to FromList.Count -1 do
    ToList.Add(FromList[idx]);
end;

procedure EmptyListWithObjects(AList:TList);
var iCnt : integer;
begin
  for iCnt := AList.Count-1 downto 0 do
  // try
    TObject( AList[iCnt] ).Free;
 { except
    ShowMessage(IntToStr(iCnt));
  end;   }
  AList.Clear;
end;

procedure EmptyStringListWithObjects(AList:TStrings);
var iCnt : integer;
begin
  for iCnt := AList.Count-1 downto 0 do
    TObject( AList.Objects[iCnt] ).Free;
  AList.Clear;
end;

procedure FreeStringListWithObjects(AList:TStrings);
begin
  EmptyStringListWithObjects(AList);
  AList.Free;
end;

procedure EmptyListWithPointers(AList : TList);
var iCnt : integer;
begin
  for iCnt := 0 to AList.Count - 1 do
    Dispose( AList[iCnt] );
  AList.Clear;
end;

procedure FreeListWithPointers(AList : TList);
begin
  EmptyListWithPointers(AList);
  AList.Free;
end;

procedure EmptyStringListWithPointers(AList : TStrings);
var iCnt : integer;
begin
  for iCnt := 0 to AList.Count - 1 do
    Dispose( Pointer(AList.Objects[iCnt]) );
  AList.Clear;
end;

procedure FreeStringListWithPointers(AList : TStrings);
begin
  EmptyStringListWithPointers(AList);
  AList.Free;
end;

function FindString( Strings : TStrings; var str: string; var idx : Integer;
  CaseSensitive : Boolean ) : Boolean;
var
  i : Integer;
  upStr, strItem, upStrItem : String;
begin
  i := Strings.IndexOf( str );
  if (i >= 0) then
  begin
    Result := True;
    idx := i;
//    str := Strings[i];
  end
  else
  begin
    Result := False;
    if CaseSensitive then
      upStr := str
    else
      upStr := AnsiUpperCase(str);

    for i := 0 to Strings.Count -1 do
    begin
      strItem := Strings[i];
      if CaseSensitive then
        upStrItem := strItem 
      else
        upStrItem := AnsiUpperCase( strItem );

      if Pos( upStr, upStrItem ) = 1 then
      begin
        Result := True;
        idx := i;
        Delete( strItem, 1, Length(str) );
        str := str + strItem;
        Exit;
      end;
    end;
  end;
end;

function NumberOfDigits(MaxValue : integer) : integer;
begin
  if MaxValue = 0 then
    // 0 or 1 -- which is correct? depends on definition... this is a literal definition 
    Result := 1
  else
    Result := Trunc( Log10(Abs(MaxValue)) + 1 );
end;

function PaddedIntToStr(AValue:integer; PadChar:char; MinLength:integer) : string;
var
  ResultLength : integer;
begin
  result := IntToStr( AValue );
  ResultLength := Length( Result );
  if ResultLength < MinLength then
    result := StringOfChar(PadChar, MinLength-ResultLength) + result;
end;

function IsNumeric(const AStr : string) : Boolean;
var
  iChar : integer;
begin
  Result := AStr<>'';
  for iChar := 1 to Length(AStr) do
    if (AStr[iChar] < '0') or (AStr[iChar] > '9') then
    begin
      Result := False;
      Break;
    end;
end;

function FieldByName(SearchInTable : TDataTable; FieldName: String) : TDataField;
var
  i : Integer;
begin
  if SearchInTable = nil then
    Result := TDataField.FieldByName(FIeldName)
  else
  begin
    Result := nil;

    for i := 0 to SearchInTable.FieldCount - 1 do
      if Copy(SearchInTable.Field[i].FieldName, 1, Length(FieldName)) = FieldName then
      begin
        if (FieldName = '') and (SearchInTable.Field[i].FieldName <> '') then
          Continue;

        Result := SearchInTable.Field[i];
        if Result.FieldName = FieldName then // exact match
          Break;
      end;
  end;
end;


{/** Free the list as long as all objects with it. Note that this function frees
     the objects in reverse order! */}
procedure FreeListWithObjects( AList : TList );
begin
  if AList<>nil then
  begin
    EmptyListWithObjects(AList);
    AList.Free;
  end;
end;

{ TFieldList }

procedure TFieldList.Add(Field: TDataField);
begin
  Insert(Count, Field);
end;

procedure TFieldList.AddFrom(FieldList: TAbstractFieldList);
var
  iField : Integer;
begin
  for iField := 0 to FieldList.Count -1 do
    FFields.Add(FieldList[iField]);
end;

procedure TFieldList.AddFromSet(Fields: TDataFieldSet);
var
  Iterator : TDataFieldSetIterator;
begin
  Iterator := TDataFieldSetIterator.Create(Fields);

  while not Iterator.EOF do
  begin
    FFields.Add(Iterator.Field);
    Iterator.Next;
  end;
  Iterator.Free;

(*  for iField := 0 to Fields.Count -1 do
    FFields.Add(Fields.Field[iField]); *)
end;

procedure TFieldList.AddFromArray(Fields: array of TDataField);
var
  iField : Integer;
begin
  for iField := Low(Fields) to High(Fields) do
    FFields.Add(Fields[iField]);
end;

procedure TFieldList.FieldsToList(Table : TDataTable);
begin
  Table.FieldsToList(FFields);
end;

procedure TFieldList.KeysToList(Table: TDataTable; AddFictiveFields:Boolean);
begin
  Table.KeysToList(FFields,AddFictiveFields);
end;

procedure TFieldList.Clear;
begin
  FFields.Clear;
end;

function TFieldList.ContainsField(Field: TDataField): Boolean;
begin
  Result := FFields.IndexOf(Field) >= 0;
end;

procedure TFieldList.Assign(FieldList : TFieldList);
begin
  CopyFrom(FieldList);
end;

procedure TFieldList.CopyFrom(FieldList: TFieldList);
begin
  Clear;
  AddFrom(FieldList);
end;

procedure TFieldList.CopyFromSet(Fields: TDataFieldSet);
begin
  Clear;
  AddFromSet(Fields);
end;

constructor TFieldList.Create;
begin
  Inherited Create;
  FFields := TList.Create;
  FDuplicates := dupAccept;
end;

constructor TFieldList.CreateFromFieldArray(Fields: array of TDataField);
begin
  Create;
  AddFromArray(Fields);
end;

(*procedure TFieldList.AddFromFieldArray(Fields : array of TDataField);
var
  iField : Integer;
begin
  for iField := Low(Fields) to High(Fields) do
    Add(Fields[iField]);
end;
*)
procedure TFieldList.Delete(Index: Integer);
begin
  FFields.Delete(Index);
end;

destructor TFieldList.Destroy;
begin
  inherited Destroy;
  FFields.Free;
end;

function TFieldList.GetCount: Integer;
begin
  Result := FFields.Count;
end;

function TFieldList.GetField(Index: Integer): TDataField;
begin
  Result := TDataField(FFields[Index]);
end;

procedure TFieldList.SetField(Index : Integer; Field : TDataField);
begin
  if (Duplicates = dupAccept) or
     not ContainsField(Field) then
    FFields[Index]:= Field
  else if Duplicates = dupError then
    raise Exception.Create('FieldList doesn''t allow duplicates!');
end;

function TFieldList.IndexOf(AField: TDataField): Integer;
begin
  Result := FFields.IndexOf(AField);
end;

procedure TFieldList.Insert(Index: Integer; Field: TDataField);
begin
  if (Duplicates = dupAccept) or
     not ContainsField(Field) then
    FFields.Insert(Index, Field)
  else if Duplicates = dupError then
    raise Exception.Create('FieldList doesn''t allow duplicates!');
end;

procedure TFieldList.Move(CurIndex, NewIndex: Integer);
begin
  FFields.Move(CurIndex, NewIndex);
end;

procedure TFieldList.Remove(Field: TDataField);
begin
  FFields.Remove(Field);
end;

{ TDefaultValueFieldList }

constructor TDefaultValueFieldList.Create;
begin
  Inherited Create;

  FFieldsAndValues := TStringList.Create;
end;

destructor TDefaultValueFieldList.Destroy;
begin
  inherited;

  FFieldsAndValues.Free;
end;

procedure TDefaultValueFieldList.Add(AField: TDataField;
  Value: String);
var
  idx : Integer;
begin
  idx := FFieldsAndValues.IndexOfObject(AField);
  if idx >= 0 then
    FFieldsAndValues[idx] := Value
  else
    FFieldsAndValues.AddObject(Value, AField);
end;

procedure TDefaultValueFieldList.AddFromArray( Fields : array of TDataField; Values : array of String );
var
  OneValue : Boolean;

  function GetIdxValue( idx : Integer ) : String;
  begin
    if OneValue then
      Result := Values[0]
    else
      Result := Values[idx];
  end;

var
  i : Integer;
begin
  OneValue := High(Values) = Low(Values);
  Assert( ( High(Fields) - Low(Fields) = High(Values) - Low(Values) ) or OneValue );

  for i := Low(Fields) to High(Fields) do
    Add( Fields[i], GetIdxValue(i) ); 
end;

function TDefaultValueFieldList.GetValue(
  AField: TDataField): TValue;
begin
  Result := ValueFromString(GetFieldString(AField));
end;

function TDefaultValueFieldList.GetField(Index: Integer): TDataField;
begin
  Result := TDataField(FFieldsAndValues.Objects[Index]);
end;

function TDefaultValueFieldList.GetString(Index : Integer) : String;
begin
  Result := FFieldsAndValues[Index];
end;

function TDefaultValueFieldList.GetCount: Integer;
begin
  Result := FFieldsAndValues.Count;
end;

function TDefaultValueFieldList.GetFieldString(
  AField: TDataField): String;
var
  idx : Integer;
begin
  idx := FFieldsAndValues.IndexOfObject(AField);
  if idx = -1 then
    raise Exception.Create('Field ' + AField.FieldName + ' doesn''t have any default ' +
          'value!');
  Result := Strings[idx];
end;

function TDefaultValueFieldList.ContainsField(
  AField: TDataField): Boolean;
begin
  Result := (FFieldsAndValues.IndexOfObject(AField) >= 0);
end;

procedure TDefaultValueFieldList.Remove(AField: TDataField);
begin
  FFieldsAndValues.Delete(FFieldsAndValues.IndexOfObject(AField));
end;

procedure TDefaultValueFieldList.Clear;
begin
  FFieldsAndValues.Clear;
end;

procedure TDefaultValueFieldList.CopyFrom(ValueList : TDefaultValueFieldList);
var
  i : Integer;
begin
  if Assigned(ValueList) then
    for i := 0 to ValueList.Count - 1 do
      Add( ValueList.Field[i], ValueList.Strings[i] );
end;

procedure SetRowDefaultsFromCrit(ARow : TDataRow; ACrit: TCriteria);
var
  CritIterator : TCriteriaFieldIterator;
  AField : TDataField;
  ACritField : TCriteriaField;
begin
  CritIterator := TCriteriaFieldIterator.Create(ACrit);
  while not CritIterator.EOF do
  begin
    ACritField := CritIterator.CriteriaField;
    AField := CritIterator.DataField;
    if ACritField.HasExactlyOneValue and
       ARow.DataTable.TableHasField( AField ) then
      ARow[AField] := ACritField.OnlyValue;
    CritIterator.Next;
  end;
  CritIterator.Free;
end;

procedure SetRowDefaultsFromQuiltPatch(ARow : TDataRow; AQuiltPatch: TQuiltPatch);
var
  AField : TDataField;
  AVal : TValue;
begin
  with AQuiltPatch.CreateIterator do
  begin
    while not Eof do
    begin
      AField := QuiltField.DataField;
      if QuiltField.AcceptsExactlyOneValue(AField, AVal) and
         ARow.DataTable.TableHasField(AField) then
        ARow[AField] := AVal;
      Next;
    end;

    Free;
  end;
end;

{ TFileStringList }

procedure TFileStringList.SetTextStr(const Value: string);
var
  Val2 : String;
  i : Integer;
begin
  Val2 := Value;
  for i := 1 to Length(Val2) do
    if Val2[i] = Char(0) then
      Val2[i] := ' ';

  inherited SetTextStr(Val2);
end;

{ TGarbageOwner }

constructor TGarbageOwner.Create;
begin
  inherited Create;
  fGarbageCan := TList.Create;
end;

procedure TGarbageOwner.AddOneOwnership(AnObject: TObject);
begin
  fGarbageCan.Add( AnObject );
end;

procedure TGarbageOwner.AddOwnership(lstObjects: TList);
var
  iObject : integer;
begin
  for iObject:= 0 to lstObjects.Count-1 do
    fGarbageCan.Add( lstObjects[iObject] );
end;

destructor TGarbageOwner.Destroy;
begin
  FreeListWithObjects( fGarbageCan );
  inherited Destroy;
end;

{ TInmemoryIncrementGenerator }

var
  AnInmemoryNumberGenerator : TInmemoryIncrementGenerator = nil;

function TInmemoryIncrementGenerator.GetNextTempRunningNumber(Table: TDataTable; const Values: TValueList): TValue;
begin
  if Values.Count>0 then
    Result := ValueFromInteger( AsInteger(Values.Values[Values.Count-1]) +1 )
  else
    Result := ValueFromInteger( 1 );
end;

function InmemoryNumberGenerator : TInmemoryIncrementGenerator;
begin
  if AnInmemoryNumberGenerator=nil then
    AnInmemoryNumberGenerator := TInmemoryIncrementGenerator.Create;
  result := AnInmemoryNumberGenerator;
end;

{$ifndef D4_OR_HIGHER}
function Max(a,b : Integer) : Integer;
begin
  result := MaxIntValue([a,b]);
end;

function Min(a,b : Integer) : Integer;
begin
  result := MinIntValue([a,b]);
end;
{$endif D4_OR_HIGHER}

{ TDoubleList }

function TDoubleList.Add(AId, AObj: TObject): Integer;
begin
  Result := FListId.Add( AId );
  FListObj.Add( AObj );
end;

procedure TDoubleList.Clear;
begin
  FListId.Clear;
  FListObj.Clear;
end;

constructor TDoubleList.Create;
begin
  inherited Create;

  FListId := TList.Create;
  FListObj := TList.Create;
end;

procedure TDoubleList.Delete(Index: Integer);
begin
  FListId.Delete(Index);
  FListObj.Delete(Index);
end;

destructor TDoubleList.Destroy;
begin
  FListId.Free;
  FListObj.Free;

  inherited;
end;

function TDoubleList.GetCount: Integer;
begin
  Result := FListId.Count;
end;

function TDoubleList.GetId(Index: Integer): TObject;
begin
  Result := FListId[Index];
end;

function TDoubleList.GetObj(Index: Integer): TObject;
begin
  Result := FListObj[Index];
end;

function TDoubleList.IndexOf(AId: TObject): Integer;
begin
  Result := FListId.IndexOf(AId);
end;

function TDoubleList.IndexOfObj(AObj: TObject): Integer;
begin
  Result := FListObj.IndexOf(AObj);
end;

function TDoubleList.Remove(AId: TObject) : Integer;
begin
  Result := FListId.Remove(AId);
  FListObj.Delete( Result );
end;

initialization

finalization
  AnInmemoryNumberGenerator.Free;

end.

