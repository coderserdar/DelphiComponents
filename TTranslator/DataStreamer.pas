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

unit DataStreamer;

interface

uses
  Classes, DataElements, DataType;

type
  TCustomStreamer = class
  private
    FStream : TMemoryStream;
    FKillStream : Boolean;

    function GetStream: TMemoryStream;
    procedure SetStream(const Value: TMemoryStream);
  protected
    procedure Error(Msg : String);

    procedure WriteInt(Int : Integer);
    procedure WriteString(Str : String);
    procedure WriteLengthAndStr(Str : String);
    function ReadInt : Integer;
    function ReadString(Length : Integer) : String;
    function ReadLengthAndStr : String;

    property Stream : TMemoryStream read GetStream write SetStream;

    constructor Create;
  public
    destructor Destroy; override;
  end;

  TTableInfoStreamer = class(TCustomStreamer)
  protected
    procedure LoadTableInfoFromStream(ATableInfo : TStrings);
    procedure SaveTableInfoToStream(ATable : TDataTable);
  end;

  TCustomRowStreamer = class(TCustomStreamer)
  private
    FStreamTableInfo : TStrings;
    FTable : TDataTable;
    procedure SetStreamTableInfo(const Value: TStrings);
  protected
    constructor Create(ATable : TDataTable; AStreamTableInfo : TStrings = nil);

    property StreamTableInfo : TStrings read FStreamTableInfo write SetStreamTableInfo;
    property Table : TDataTable read FTable;
  end;

  TDataRowStreamer = class(TCustomRowStreamer)
  protected
    procedure FetchFieldValue(ARow : TDataRow; FieldIdx : Integer; DataType : TDataType);
    procedure StoreFieldValue(ARow : TDataRow; FieldIdx : Integer);

    procedure GetSpecialValue(Index : Integer; out Value : String);
    procedure SetSpecialValue(Index : Integer; Value : String);

    procedure LoadRowFromStream(ARow : TDataRow);
    procedure SaveRowToStream(ARow : TDataRow);
  end;

  TRowListStreamer = class(TCustomRowStreamer)
  protected
    procedure LoadListFromStream(ATable : TDataTable; ARowList : TStrings);
    procedure SaveListToStream(ARowList : TStrings);
  end;

  TRowStorageStreamer = class(TRowListStreamer)
  protected
    procedure LoadStorageFromStream(AStorage : TAbstractRowStorage);
    procedure SaveStorageToStream(AStorage : TAbstractRowStorage);
  end;

  TPrsFileHandler = class(TRowStorageStreamer)
  public
    procedure LoadFromFile(AStorage : TAbstractRowStorage; AFileName: String);
    procedure SaveToFile(AStorage : TAbstractRowStorage; AFileName: String);
  end;

implementation

uses
  SysUtils, DataTypes;

const
  MSG_CORRUPT_OR_WRONG ='The data is corrupt or has the wrong format!';
  MSG_NO_DATATYPE = 'No data type defined!';
  MSG_INFO_MISSING_OR_INCORRECT = 'Information about the stored data size is missing or incorrect!';

type
  TDataRowLink = class(TDataRow);
  TRowStorageLink = class(TAbstractRowStorage);

{ TCustomStreamer }

constructor TCustomStreamer.Create;
begin
  inherited Create;
  FStream := nil;
  FKillStream := False;
end;

destructor TCustomStreamer.Destroy;
begin
  inherited;
  if FKillStream then
    FStream.Free;
end;

procedure TCustomStreamer.Error(Msg : String);
begin
  raise Exception.Create(Msg);
end;

function TCustomStreamer.GetStream: TMemoryStream;
begin
  if FStream = nil then
  begin
    FKillStream := True;
    FStream := TMemoryStream.Create;
  end;

  Result := FStream;
end;

function TCustomStreamer.ReadInt: Integer;
begin
  if Stream.Read(Result, SizeOf(Integer)) <> SizeOf(Integer) then
    Error(Self.ClassName + '.ReadInt: ' + MSG_CORRUPT_OR_WRONG);
end;

function TCustomStreamer.ReadLengthAndStr: String;
begin
  Result := ReadString(ReadInt);
end;

function TCustomStreamer.ReadString(Length: Integer): String;
begin
  if Length > 0 then
  begin
    Result := StringOfChar(' ', Length);

    if Stream.Read(Result[1], Length) <> Length then
      Error(Self.ClassName + '.ReadString: ' + MSG_CORRUPT_OR_WRONG);
  end
  else
    Result := '';
end;

procedure TCustomStreamer.SetStream(const Value: TMemoryStream);
begin
  Assert(FStream = nil);
  FStream := Value;
end;

procedure TCustomStreamer.WriteInt(Int: Integer);
begin
  Stream.Write(Int, SizeOf(Integer));
end;

procedure TCustomStreamer.WriteLengthAndStr(Str: String);
begin
  WriteInt(Length(Str));
  WriteString(Str);
end;

procedure TCustomStreamer.WriteString(Str: String);
var
  Size : Integer;
begin
  Size := Length(Str);
  if Size > 0 then
    Stream.Write(Str[1], Size);
end;

{ TTableInfoStreamer }

procedure TTableInfoStreamer.LoadTableInfoFromStream(ATableInfo : TStrings);
var
  i, FieldCount : Integer;
  strFieldName, strClassName : String;
  DataType : TDataType;
  Props : TStrings;
begin
  Props := TStringList.Create;

  FieldCount := ReadInt;

  for i := 0 to FieldCount -1 do
  begin
    strFieldName := ReadLengthAndStr;
    strClassName := ReadLengthAndStr;

    // Read properties
    Props.Clear;
    Props.CommaText := ReadLengthAndStr;
    DataType := DataTypeFactory.DataTypeByName(strClassName, Props);
    ATableInfo.AddObject(strFieldName, DataType);
  end;

  Props.Free;
end;

procedure TTableInfoStreamer.SaveTableInfoToStream(ATable : TDataTable);
var
  i : Integer;
  AField : TDataField;
  AProps : TStrings;
begin
  AProps := TStringList.Create;

  WriteInt(ATable.FieldCount);

  for i := 0 to ATable.FieldCount -1 do
  begin
    AField := ATable.Field[i];
    WriteLengthAndStr(AField.FieldName);

    // Write properties
    AProps.Clear;
    DataTypeFactory.PropertiesForType(AField.DataType, AProps);
    WriteLengthAndStr(AField.DataType.ClassName);
    WriteLengthAndStr(AProps.CommaText);
  end;

  AProps.Free;
end;

{ TCustomRowStreamer }

constructor TCustomRowStreamer.Create(ATable : TDataTable; AStreamTableInfo : TStrings = nil);
begin
  inherited Create;

 FTable := ATable;
 FStreamTableInfo := AStreamTableInfo;
end;

procedure TCustomRowStreamer.SetStreamTableInfo(const Value: TStrings);
begin
  FStreamTableInfo := Value;
end;

{ TDataRowStreamer }

procedure TDataRowStreamer.LoadRowFromStream(ARow : TDataRow);
var
  FieldCount_Table, iField, iField_Table, delCnt : Integer;
  FieldCount_Stream : Integer;
  ADataType : TDataType;
  AField : TDataField;
begin
  FieldCount_Table := Table.FieldCount;
  if StreamTableInfo <> nil then
    FieldCount_Stream := StreamTableInfo.Count
  else
    FieldCount_Stream := FieldCount_Table;

  delCnt := 0;

  for iField := 0 to FieldCount_Stream -1 do
  begin
    if FieldCount_Table = FieldCount_Stream then
    begin
      if StreamTableInfo <> nil then
        ADataType := StreamTableInfo.Objects[iField] as TDataType
      else
        ADataType := Table.Field[iField].DataType;
        
      FetchFieldValue(ARow, iField, ADataType);
    end
    else
    begin
      // StreamTableInfo exists
      AField := TDataField.FieldByName(StreamTableInfo[iField]);
      iField_Table := Table.IndexOfField(AField);
      if (iField_Table = -1) then
      begin
        if FieldCount_Table > StreamTableInfo.Count then
          Error(Self.ClassName + '.LoadFromStream: ' + MSG_CORRUPT_OR_WRONG)
        else
        begin
          Inc(delCnt);
          if delCnt > (StreamTableInfo.Count - FieldCount_Table) then
            Error(Self.ClassName + '.LoadFromStream: ' + MSG_CORRUPT_OR_WRONG)
        end;
      end;

      FetchFieldValue(ARow, iField_Table, StreamTableInfo.Objects[iField] as TDataType);
    end;
  end;
end;

procedure TDataRowStreamer.SaveRowToStream(ARow: TDataRow);
var
  iField : Integer;
begin
  for iField := 0 to Table.FieldCount -1 do
    StoreFieldValue(ARow, iField);
end;

procedure TDataRowStreamer.FetchFieldValue(ARow : TDataRow; FieldIdx: Integer; DataType : TDataType);
var
  Value : TValue;
  Pos, StoredSize, DataSize: Integer;
begin
  Assert(Stream.Position >= 0);

  StoredSize := -1;
  DataSize := -1;
  
  if DataType = nil then
    Error(Self.ClassName + '.FetchFieldValue: ' + MSG_NO_DATATYPE)
  else
  begin
    DataSize := DataType.DataSize;
    if DataType.DynamicSize then
      Stream.Read(StoredSize, SizeOf(Integer))
    else
      StoredSize := DataSize;
  end;

  if StoredSize < 0 then
    Error(Self.ClassName + '.FetchFieldValue: ' + MSG_INFO_MISSING_OR_INCORRECT);

  Pos := Stream.Position;

  if FieldIdx < 0 then
    Stream.Position := Pos + StoredSize
  else
  begin
    // Update Position *before* the actual write, so that the GetSpecialValue sees the correct position,
    // since Position isn't updated automatically when we write directly to the memory
    Stream.Position := Pos + DataSize;

    try
      try
        if DataType is TUndefinedType then
          Value := Table.Field[FieldIdx].DataType.DefaultValue
        else if DataType.DynamicSize then
          Value := DataType.FetchValue( Pointer(Longint(Stream.Memory) + Pos), GetSpecialValue, FieldIdx)
        else
          Value := DataType.FetchValue( Pointer(Longint(Stream.Memory) + Pos), nil, FieldIdx);
      except
        Value := Table.Field[FieldIdx].DataType.DefaultValue;
      end;
      TDataRowLink(ARow).StoreValue(FieldIdx, Value);
    except
    end;

    if DataType.DynamicSize then
      Stream.Position := Pos + StoredSize;
  end;
end;

procedure TDataRowStreamer.StoreFieldValue(ARow : TDataRow; FieldIdx: Integer);
var
  Value : TValue;
  IntSize, DataSize : Integer;
  IntPos, DataPos, Pos : Integer;
  DataType : TDataType;
begin
  DataType := Table.Field[FieldIdx].DataType;
  Assert(DataType <> nil);

  DataSize := DataType.DataSize;
  Assert(DataSize >= 0);

  // Register the position to store size info on, if the data type is dynamic
  IntPos := Stream.Position;
  Assert(IntPos >= 0);

  IntSize := SizeOf(Integer);

  // Make space for size informaton when the data type is dynamic
  if DataType.DynamicSize then
    Stream.Position := IntPos + IntSize;

  // Register where the data starts, in order to calculate the amount of data saved
  DataPos := Stream.Position;

  // Update Size and Position *before* the actual write, to increase the size of the stream,
  // if it is too small, and so that the GetSpecialValue sees the correct position,
  // since Position isn't updated automatically when we write directly to the memory
  Pos := Stream.Position + DataSize;
  if Pos > Stream.Size then
    Stream.Size := Pos;
  Stream.Position := Pos;

  Value := TDataRowLink(ARow).FetchValue(FieldIdx);
  if DataType.DynamicSize then
    DataType.StoreValue(Pointer(Longint(Stream.Memory) + DataPos), Value, SetSpecialValue, FieldIdx)
  else
    DataType.StoreValue(Pointer(Longint(Stream.Memory) + DataPos), Value, nil, FieldIdx);

  // Store the size of a dynamic data type after it's data is saved
  if DataType.DynamicSize then
  begin
    Pos := Stream.Position;
    DataSize := Pos - DataPos;
    if DataSize < 0 then
      raise Exception.Create(Self.ClassName + '.StoreFieldValue: Data can''t have negative length!');

    Stream.Position := IntPos;
    WriteInt(DataSize);
    Stream.Position := Pos;
  end;
end;

procedure TDataRowStreamer.GetSpecialValue(Index: Integer;
  out Value: String);
begin
  Value := ReadLengthAndStr;
end;

procedure TDataRowStreamer.SetSpecialValue(Index: Integer; Value: String);
begin
  WriteLengthAndStr(Value);
end;

{ TRowListStreamer }

procedure TRowListStreamer.LoadListFromStream(ATable : TDataTable; ARowList: TStrings);
var
  RowCount, iRow : Integer;
  ARow : TDataRow;
  RowStreamer : TDataRowStreamer;
begin
  RowStreamer := TDataRowStreamer.Create(Table, StreamTableInfo);
  try
    RowStreamer.Stream := Stream;
    RowCount := ReadInt;

    if RowCount > 0 then
    begin
      for iRow := 0 to RowCount - 1 do
      begin
        ARow := TDataRow.Create(ATable);
        try
          RowStreamer.LoadRowFromStream(ARow);
        except
          ARow.Free;
          Continue;
        end;
        ARowList.AddObject('', ARow);
      end;
    end;
  finally
    RowStreamer.Free;
  end;
end;

procedure TRowListStreamer.SaveListToStream(ARowList: TStrings);
var
  iRow : Integer;
  ARow : TDataRow;
  ATable : TDataTable;
  RowStreamer : TDataRowStreamer;
begin
  WriteInt(ARowList.Count);

  if ARowList.Count > 0 then
  begin
    if Table = nil then
      ATable := (ARowList.Objects[0] as TDataRow).DataTable
    else
      ATable := Table;

    RowStreamer := TDataRowStreamer.Create(ATable, StreamTableInfo);
    try
      RowStreamer.Stream := Stream;

      Stream.Size := Stream.Size + ATable.DataLength * ARowList.Count;

      for iRow := 0 to ARowList.Count - 1 do
      begin
        ARow := ARowList.Objects[iRow] as TDataRow;
        Assert(ARow.DataTable = ATable);
        RowStreamer.SaveRowToStream(ARow);
      end;
    finally
      RowStreamer.Free;
    end;
  end;
end;

{ TRowStorageStreamer }

procedure TRowStorageStreamer.LoadStorageFromStream(
  AStorage: TAbstractRowStorage);
var
  iRow : Integer;
  ARow : TDataRow;
  Rows : TStrings;
begin
  FTable := AStorage.DataTable;

  StreamTableInfo := TStringList.Create;
  with TTableInfoStreamer.Create do
  begin
    Stream := Self.Stream;
    LoadTableInfoFromStream(StreamTableInfo);
    Free;
  end;

  Rows := TStringList.Create;
  LoadListFromStream(AStorage.DataTable, Rows);

  for iRow := 0 to Rows.Count -1 do
  begin
    ARow := Rows.Objects[iRow] as TDataRow;
    TRowStorageLink(AStorage).ForcePutRow(ARow, paDontOverwriteKeys);
    TDataRowLink(ARow).FStatus := rsNew; // rsUnchanged;
  end;

  Rows.Free;
  StreamTableInfo.Free;
  StreamTableInfo := nil;

  FTable := nil;
end;

procedure TRowStorageStreamer.SaveStorageToStream(AStorage: TAbstractRowStorage);
var
  List : TStrings;
begin
  FTable := AStorage.DataTable;

  with TTableInfoStreamer.Create do
  begin
    Stream := Self.Stream;
    SaveTableInfoToStream(Table);
    Free;
  end;

  List := TStringList.Create;
  AStorage.GetRows(List, nil, gaReference);
  SaveListToStream(List);
  List.Free;

  FTable := nil;
end;

{ TPrsFileHandler }

procedure TPrsFileHandler.LoadFromFile(AStorage : TAbstractRowStorage; AFileName: String);
begin
  Stream.LoadFromFile(AFileName);
  LoadStorageFromStream(AStorage);
end;

procedure TPrsFileHandler.SaveToFile(AStorage : TAbstractRowStorage; AFileName: String);
begin
  SaveStorageToStream(AStorage);
  Stream.SaveToFile(AFileName);
end;

end.

