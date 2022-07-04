unit SQLMetaData;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$N+,P+,S-,R-}

interface

uses
  Windows, Classes, Forms, Dialogs, SysUtils, DBXpress, SQLExpr,
  Provider, DBClient, Variants;

type
  TFieldMetaData = record
    ColumnName: string;
    ColumnPosition: LongInt;
    ColumnDataType: LongInt;
    ColumnTypeName: string;
    ColumnSubtype: LongInt;
    ColumnLength: LongInt;
    ColumnPrecision: LongInt;
    ColumnScale: LongInt;
    ColumnNullable: LongInt;  // 1=Not Nullable, 0=Nullable
  end;

  TMetaDataSortOrder = (soName, soPosition);
  TMetaDataObjectType = (otTable, otSysTable, otView, otSynonym, otColumn,
     otIndex, otMetaData);

  TSQLMetaData = class(TSQLConnection)
  private
    FAbout: String;
    FActiveSQLObject: String;
    FActiveSQLObjectType: TMetaDataObjectType;
    FSubActiveSQLObject: String;
    FSQLDataSet: TSQLDataSet;
    FDataSetProvider: TDataSetProvider;
    FClientDataSet: TClientDataSet;
    FMetaDataFilter: String;
    FMetaDataFieldCount: Integer;
    FResultStrings: TStrings;
    FTableMetaData: Array[0..1023] of TFieldMetaData;
    FWorkStrings: TStrings;
    procedure SetTableMetaData(const ATableName: String);
    procedure SetActiveSQLObject(AObjectType: TMetaDataObjectType);
  protected

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;

    procedure GetTableNames(AList: TStrings); overload;
    procedure GetSysTableNames(AList: TStrings);
    procedure GetViewNames(AList: TStrings);
    procedure GetSynonymNames(AList: TStrings);
    procedure GetFieldNames(const ATableName: string; AList: TStrings;
       ASortOrder: TMetaDataSortOrder = soPosition); overload;
    function GetFieldMetaData(const ATableName, AColumnName: string): TFieldMetaData; overload;
    function GetFieldMetaData(const ATableName: string; AFieldIndex: Integer): TFieldMetaData; overload;
    procedure GetPrimaryKeyFieldNames(const ATableName: string; AList: TStrings);
    function GetPrimaryKeyFields(const ATableName: string): string;
    procedure GetIndexFieldNames(const ATableName, AIndexName: string; AList: TStrings);
    function GetIndexFields(const ATableName, AIndexName: string): string;
    function GetInsertStatement(const ATableName: string; ASQL: TStrings): Integer;
    function GetUpdateStatement(const ATableName: string; ASQL: TStrings): Integer;
    function GetSelectStatement(const ATableName: string; ASQL: TStrings): Integer;
  published
    property About: String read FAbout;
    property MetaDataFilter: String read FMetaDataFilter write FMetaDataFilter;
  end;

implementation

{ TSQLMetaData }

constructor TSQLMetaData.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := 'Ver 0.8.0.7';
  FMetaDataFieldCount := -1;
  Self.TableScope := [tsTable];
  FClientDataSet := TClientDataSet.Create(Self);
  FClientDataSet.Name := 'smdClientDataSet';
  FDataSetProvider := TDataSetProvider.Create(Self);
  FDataSetProvider.Name := 'smdDataSetProvider';
  FSQLDataSet := TSQLDataSet.Create(Self);
  FSQLDataSet.Name := 'smdSQLDataSet';
  FSQLDataSet.SQLConnection := Self;
  FDataSetProvider.DataSet := FSQLDataSet;
  FClientDataSet.ProviderName := FDataSetProvider.Name;
  FResultStrings := TStringList.Create;
  FWorkStrings := TStringList.Create;
end;

destructor TSQLMetaData.Destroy;
begin
  FClientDataSet.Close;
  FSQLDataSet.Close;
  FClientDataSet.Free;
  FDataSetProvider.Free;
  FSQLDataSet.Free;
  FResultStrings.Free;
  FWorkStrings.Free;
  Self.Close;
  inherited;
end;

procedure TSQLMetaData.Refresh;
begin
  //  Have an index of the procedure last run.  Close the
  //  connection and rerun the procedure / function to get a fresh
  //  copy of the metadata.  -- Proposed usage
end;

procedure TSQLMetaData.SetActiveSQLObject(AObjectType: TMetaDataObjectType);
begin
  if AObjectType <> FActiveSQLObjectType then begin
    if AObjectType <> otMetaData then FMetaDataFieldCount := -1;
    FActiveSQLObjectType := AObjectType;
  end;
end;

procedure TSQLMetaData.GetTableNames(AList: TStrings);
begin
  FClientDataSet.Close;
  FSQLDataSet.Close;
  Self.TableScope := [tsTable];
  FSQLDataSet.SetSchemaInfo(stTables, '', '');
  FClientDataSet.IndexFieldNames := 'TABLE_NAME';
  FClientDataSet.Open;

  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    AList.Add(FClientDataSet.FieldByName('TABLE_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otTable);
  FActiveSQLObject := '';
  FSubActiveSQLObject := '';
end;

procedure TSQLMetaData.GetSysTableNames(AList: TStrings);
begin
  FClientDataSet.Close;
  FSQLDataSet.Close;
  Self.TableScope := [tsTable];
  FSQLDataSet.SetSchemaInfo(stSysTables, '', '');
  FClientDataSet.IndexFieldNames := 'TABLE_NAME';
  FClientDataSet.Open;

  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    AList.Add(FClientDataSet.FieldByName('TABLE_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otSysTable);
  FActiveSQLObject := '';
  FSubActiveSQLObject := '';
end;

procedure TSQLMetaData.GetViewNames(AList: TStrings);
begin
  FClientDataSet.Close;
  FSQLDataSet.Close;
  Self.TableScope := [tsView];
  FSQLDataSet.SetSchemaInfo(stTables, '', '');
  FClientDataSet.IndexFieldNames := 'TABLE_NAME';
  FClientDataSet.Open;

  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    AList.Add(FClientDataSet.FieldByName('TABLE_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otView);
  FActiveSQLObject := '';
  FSubActiveSQLObject := '';
end;

procedure TSQLMetaData.GetSynonymNames(AList: TStrings);
begin
  FClientDataSet.Close;
  FSQLDataSet.Close;
  Self.TableScope := [tsSynonym];
  FSQLDataSet.SetSchemaInfo(stTables, '', '');
  FClientDataSet.IndexFieldNames := 'TABLE_NAME';
  FClientDataSet.Open;

  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    AList.Add(FClientDataSet.FieldByName('TABLE_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otSynonym);
  FActiveSQLObject := '';
  FSubActiveSQLObject := '';
end;

procedure TSQLMetaData.GetFieldNames(const ATableName: string; AList: TStrings;
       ASortOrder: TMetaDataSortOrder = soPosition);
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otColumn) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stColumns, ATableName, '');
    if ASortOrder = soName then
      FClientDataSet.IndexFieldNames := 'COLUMN_NAME'
    else
      FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    AList.Add(FClientDataSet.FieldByName('COLUMN_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otColumn);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := '';
end;

function TSQLMetaData.GetFieldMetaData(const ATableName, AColumnName: string): TFieldMetaData;
begin
  Result.ColumnName := '';
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otColumn) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stColumns, ATableName, '');
    FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    if AColumnName = FClientDataSet.FieldByName('COLUMN_NAME').AsString then begin
      Result.ColumnName := FClientDataSet.FieldByName('COLUMN_NAME').AsString;
      Result.ColumnPosition := FClientDataSet.FieldByName('COLUMN_POSITION').AsInteger;
      Result.ColumnDatatype := FClientDataSet.FieldByName('COLUMN_DATATYPE').AsInteger;
      Result.ColumnTypeName := FClientDataSet.FieldByName('COLUMN_TYPENAME').AsString;
      Result.ColumnSubtype := FClientDataSet.FieldByName('COLUMN_SUBTYPE').AsInteger;
      Result.ColumnLength := FClientDataSet.FieldByName('COLUMN_LENGTH').AsInteger;
      Result.ColumnPrecision := FClientDataSet.FieldByName('COLUMN_PRECISION').AsInteger;
      Result.ColumnScale := FClientDataSet.FieldByName('COLUMN_SCALE').AsInteger;
      Result.ColumnNullable := FClientDataSet.FieldByName('COLUMN_NULLABLE').AsInteger;  // 1=Not Nullable, 0=Nullable  }
      Break;
    end;
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otColumn);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := AColumnName;
end;

procedure TSQLMetaData.SetTableMetaData(const ATableName: String);
var
  FieldCount: Integer;
begin
  FMetaDataFieldCount := -1;
  FClientDataSet.Close;
  FSQLDataSet.Close;
  FSQLDataSet.SetSchemaInfo(stColumns, ATableName, '');
  FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
  FClientDataSet.Open;
  FieldCount := 0;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    if FieldCount >= 1023 then begin
      FMetaDataFieldCount := -1;
      Raise Exception.Create('Error:  Meta Data Array can only ' +
         'accomadate 1024 fields per table.');
    end;
    FTableMetaData[FieldCount].ColumnName :=
       FClientDataSet.FieldByName('COLUMN_NAME').AsString;
    FTableMetaData[FieldCount].ColumnPosition :=
       FClientDataSet.FieldByName('COLUMN_POSITION').AsInteger;
    FTableMetaData[FieldCount].ColumnDatatype :=
       FClientDataSet.FieldByName('COLUMN_DATATYPE').AsInteger;
    FTableMetaData[FieldCount].ColumnTypeName :=
       FClientDataSet.FieldByName('COLUMN_TYPENAME').AsString;
    FTableMetaData[FieldCount].ColumnSubtype :=
       FClientDataSet.FieldByName('COLUMN_SUBTYPE').AsInteger;
    FTableMetaData[FieldCount].ColumnLength :=
       FClientDataSet.FieldByName('COLUMN_LENGTH').AsInteger;
    FTableMetaData[FieldCount].ColumnPrecision :=
       FClientDataSet.FieldByName('COLUMN_PRECISION').AsInteger;
    FTableMetaData[FieldCount].ColumnScale :=
       FClientDataSet.FieldByName('COLUMN_SCALE').AsInteger;
    FTableMetaData[FieldCount].ColumnNullable :=
       FClientDataSet.FieldByName('COLUMN_NULLABLE').AsInteger;  // 1=Not Nullable, 0=Nullable  }
    Inc(FieldCount);
    FClientDataSet.Next;
  end;

  FMetaDataFieldCount := FieldCount;
  SetActiveSQLObject(otMetaData);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := '';
end;

function TSQLMetaData.GetFieldMetaData(const ATableName: string; AFieldIndex: Integer): TFieldMetaData;
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otMetaData) then begin
    SetTableMetaData(ATableName);
  end;

  Result.ColumnName := FTableMetaData[AFieldIndex].ColumnName;
  Result.ColumnPosition := FTableMetaData[AFieldIndex].ColumnPosition;
  Result.ColumnDatatype := FTableMetaData[AFieldIndex].ColumnDatatype;
  Result.ColumnTypeName := FTableMetaData[AFieldIndex].ColumnTypeName;
  Result.ColumnSubtype := FTableMetaData[AFieldIndex].ColumnSubtype;
  Result.ColumnLength := FTableMetaData[AFieldIndex].ColumnLength;
  Result.ColumnPrecision := FTableMetaData[AFieldIndex].ColumnPrecision;
  Result.ColumnScale := FTableMetaData[AFieldIndex].ColumnScale;
  Result.ColumnNullable := FTableMetaData[AFieldIndex].ColumnNullable;
end;

procedure TSQLMetaData.GetPrimaryKeyFieldNames(const ATableName: string; AList: TStrings);
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otIndex) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stIndexes, ATableName, '');
    FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  AList.Clear;
  FClientDataSet.First;

  while not(FClientDataSet.Eof) do begin
    if FClientDataSet.FieldByName('INDEX_TYPE').AsInteger = 6 then
       AList.Add(FClientDataSet.FieldByName('COLUMN_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otIndex);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := '';
end;

function TSQLMetaData.GetPrimaryKeyFields(const ATableName: string): string;
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otIndex) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stIndexes, ATableName, '');
    FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  FClientDataSet.First;


  while not(FClientDataSet.Eof) do begin
    if FClientDataSet.FieldByName('INDEX_TYPE').AsInteger = 6 then begin
      if Result > #32 then
        Result := Result + ';' + FClientDataSet.FieldByName('COLUMN_NAME').AsString
      else
        Result := FClientDataSet.FieldByName('COLUMN_NAME').AsString;
    end;
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otIndex);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := '';
end;

procedure TSQLMetaData.GetIndexFieldNames(const ATableName, AIndexName: string; AList: TStrings);
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otIndex) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stIndexes, ATableName, '');
    FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  AList.Clear;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    if FClientDataSet.FieldByName('INDEX_NAME').AsString = AIndexName then
       AList.Add(FClientDataSet.FieldByName('COLUMN_NAME').AsString);
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otIndex);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := AIndexName;
end;

function TSQLMetaData.GetIndexFields(const ATableName, AIndexName: string): string;
begin
  if (ATableName <> FActiveSQLObject) or (FActiveSQLObjectType <> otIndex) then begin
    FClientDataSet.Close;
    FSQLDataSet.Close;
    FSQLDataSet.SetSchemaInfo(stIndexes, ATableName, '');
    FClientDataSet.IndexFieldNames := 'COLUMN_POSITION';
    FClientDataSet.Open;
  end;
  FClientDataSet.First;
  while not(FClientDataSet.Eof) do begin
    if FClientDataSet.FieldByName('INDEX_NAME').AsString = AIndexName then begin
      if Result > #32 then
        Result := Result + ';' + FClientDataSet.FieldByName('COLUMN_NAME').AsString
      else
        Result := FClientDataSet.FieldByName('COLUMN_NAME').AsString;
    end;
    FClientDataSet.Next;
  end;

  SetActiveSQLObject(otIndex);
  FActiveSQLObject := ATableName;
  FSubActiveSQLObject := AIndexName;
end;

function TSQLMetaData.GetInsertStatement(const ATableName: string; ASQL: TStrings): Integer;
var
  FieldCount: Integer;
begin
  SetTableMetaData(ATableName);
  ASQL.Clear;
  ASQL.Add('INSERT INTO ');
  ASQL.Add(ATableName);
  ASQL.Add(' ( ');
  for FieldCount := 0 to FMetaDataFieldCount - 1 do begin
    if FieldCount = FMetaDataFieldCount - 1 then
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName)
    else
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName + ' ,');
  end;
  ASQL.Add(' ) VALUES ( ');
  for FieldCount := 0 to FMetaDataFieldCount - 1 do begin
    if FieldCount = FMetaDataFieldCount - 1 then
      ASQL.Add('value_for_' + GetFieldMetaData(ATableName, FieldCount).ColumnName + ' )')
    else
      ASQL.Add('value_for_' + GetFieldMetaData(ATableName, FieldCount).ColumnName + ' ,');
  end;
  Result := 3 + FMetaDataFieldCount;  // First value_for_ Line
end;

function TSQLMetaData.GetUpdateStatement(const ATableName: string; ASQL: TStrings): Integer;
var
  FieldCount: Integer;
begin
  SetTableMetaData(ATableName);
  ASQL.Clear;
  ASQL.Add('UPDATE ');
  ASQL.Add(ATableName);
  ASQL.Add(' SET ');
  for FieldCount := 0 to FMetaDataFieldCount - 1 do begin
    if FieldCount = FMetaDataFieldCount - 1 then
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName +
         ' = ' + GetFieldMetaData(ATableName, FieldCount).ColumnTypeName)
    else
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName +
      ' = ' + GetFieldMetaData(ATableName, FieldCount).ColumnTypeName + ' ,');
  end;
  Result := 3;  // First FIELD = VALUE line
end;

function TSQLMetaData.GetSelectStatement(const ATableName: string; ASQL: TStrings): Integer;
var
  FieldCount: Integer;
begin
  SetTableMetaData(ATableName);
  ASQL.Clear;
  ASQL.Add('SELECT ');
  for FieldCount := 0 to FMetaDataFieldCount - 1 do begin
    if FieldCount = FMetaDataFieldCount - 1 then
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName)
    else
      ASQL.Add(GetFieldMetaData(ATableName, FieldCount).ColumnName + ' ,');
  end;
  ASQL.Add('FROM ');
  ASQL.Add(ATableName);
  Result := ASQL.Count - 1;  // Table name line
end;

end.
