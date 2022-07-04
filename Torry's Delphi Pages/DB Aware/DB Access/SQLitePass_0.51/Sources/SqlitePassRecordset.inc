{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2010

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 01.01.2010

  --------------------------------------------------------------------------- }

{ TSqlitePassRecordset}

constructor TSqlitePassRecordset.Create(Dataset: TSqlitePassDataset);
begin
  Create(Dataset.Database);
  FDataset := Dataset;
end;

constructor TSqlitePassRecordset.Create(Database: TSqlitePassDatabase);
begin
  inherited Create;
  FDataset := nil;
  FDatabase := Database;
  FActive := False;
  FDirectPost := True;

  { Minimum Capacity is set to 100 records
  using a higher value when dealing with very large tables could improve speed }
  FRecordMemoryBlocCapacity := 100;

  {FRecordsMemoryBloc will be allocated in the GetRecords procedure,
   once we know the record size }
  FRecordsMemoryBlocs := TList.Create;
  FRecycledRecords := TSqlitePassList.Create;

  FAnsiStrings := TSqlitePassAnsiStringList.Create;
  FAnsiStrings.Capacity := FRecordsCapacity;
  FRecycledAnsiStrings := TSqlitePassIntegerList.Create;

  FWideStrings := TSqlitePassWideStringList.Create;
  FWideStrings.Capacity := FRecordsCapacity;
  FRecycledWideStrings := TSqlitePassIntegerList.Create;

  FMemoryStreams := TList.Create;
  FRecycledMemoryStreams := TSqlitePassIntegerList.Create;

  FActiveRecordOldValues := nil;
  ClearMemory;
end;

destructor TSqlitePassRecordset.Destroy;
begin
  Close;
  FMemoryStreams.Free;
  FRecycledMemoryStreams.Free;
  FAnsiStrings.Free;
  FRecycledAnsiStrings.Free;
  FWideStrings.Free;
  FRecycledWideStrings.Free;
  FRecycledRecords.Free;
  FRecordsMemoryBlocs.Free;
  FDataset := nil;
  FDatabase := nil;
  inherited Destroy;
end;

{ --- Opening / Closing --- }

Procedure TSqlitePassRecordset.Open(Const Sql: String; Const TablesCount: Word = 0);
begin
Close;
Try
 FActive := True;
 FRowidTablesCount := TablesCount;
 GetRecords(Sql);
Except
 On EAbort do Close; { Safe call to free memory if needed }
    else begin
         Close; { Safe call to free memory if needed }
         FDatabase.DatabaseError.RaiseExceptionFmt(Msg3013, [CRLF, Sql], -1, FDatabase);
         end;
end;
end;

procedure TSqlitePassRecordset.Close;
begin
if FActive then
   try
   ClearMemory;
   finally
   FActive := False;
   end;
end;

procedure TSqlitePassRecordset.ClearMemory;
var
i: Integer;

begin
 { Reset Boolean Flags }
 FRecordsChanged := False;

 SetLength(FDataReader, 0);
 Finalize(FDataReader);
 FreeMem(FActiveRecordOldValues);
 FActiveRecordOldValues := nil;

 { Move to the first record }
 FCurrentRecordIndex := -1;
 FOldActiveRecordIndex := -1;

 { Clear Records Pointers }
 SetFRecordsCapacity(0);
 FRecordsCount := 0;
 FRecycledRecords.Clear;

 { Clear FRecordsMemoryBlocs }
 if FRecordsMemoryBlocs.Count > 0 then
 begin
 For i := 0 to Pred(FRecordsMemoryBlocs.Count)
     do FreeMem(FRecordsMemoryBlocs[i]);
 FRecordsMemoryBlocs.Clear;
 end;
 FRecordsUsedInMemoryBloc := FRecordMemoryBlocCapacity;
 
 { Clear Strings }
 FAnsiStrings.Clear;
 FRecycledAnsiStrings.Clear;

 { Clear WideStrings }
 FWideStrings.Clear;
 FRecycledWideStrings.Clear;

 { Clear Blobs and clear Blobs List pointer }
 For i := 0 to Pred(FMemoryStreams.Count)
     do TMemoryStream(FMemoryStreams[i]).Free;
 FMemoryStreams.Clear;
 FRecycledMemoryStreams.Clear;

 {  }
 SetLength(FInternalFieldsValueOffset, 0);
 Finalize(FInternalFieldsValueOffset);
end;

{ Returns the active record index : the record that is currently used by dataset user }
function TSqlitePassRecordset.GetActiveRecordIndex: Integer;
begin
 if Assigned(FDataset)
    then Result := FDataset.GetActiveRecord
    else Result := FCurrentRecordIndex;
end;

{ Result : the memory size needed to store a given Datatype in the internal record buffer }
function TSqlitePassRecordset.GetFieldSize(Const DataType: TFieldType): Integer;
begin
  case DataType of
       ftInteger, ftSmallInt, ftWord, ftDate, ftTime,
       ftString, ftFixedChar, ftWideString: Result := SizeOf(Integer);
       {$IFDEF HasftFixedWideChar}
       ftFixedWideChar: Result := SizeOf(Integer);
       {$ENDIF HasftFixedWideChar}
       ftDateTime, ftFloat, ftCurrency: Result := SizeOf(Double);
       ftBCD: Result := SizeOf(Currency);
       ftBoolean: Result := SizeOf(WordBool);
       ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary, ftBytes, ftVarBytes
       {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF}: Result := SizeOf(Integer);
       ftAutoInc, ftLargeint: Result := SizeOf(Int64);
       else Result := SizeOf(PRecBuffer);
       end;
  { Add the field status flag to field size }
  Inc(Result, SizeOf(Byte));
end;


{ Calculates the size needed to store fields values, including calculated and lookup fields
  and others information in a record
  Returns the number of fields including calculated and lookup }
function TSqlitePassRecordset.SetFRecordSize: Integer;
var
i, j: Integer;
begin
  FCalcFieldsCount := 0;
  SetLength(FInternalFieldsValueOffset, 0);
  Finalize(FInternalFieldsValueOffset);
  
  { Offsets from record start }
  FBookMarkDataOffset := SizeOf(TBookMarkFlag);
  FRecordStateOffset  := FBookMarkDataOffset + SizeOf(Cardinal);
  FRowIdsOffset       := FRecordStateOffset + SizeOf(TSqlitePassRecordState);
  FFieldsValuesOffset := FRowIdsOffset
                        + (FRowidTablesCount * SizeOf(Int64))
                        + SizeOf(Byte); // Skip Field Status Flag
  FRecordSize         := FFieldsValuesOffset;

  if Assigned(FDataset)
     then begin
          { Set the Fields offset to the maximum length. We will downsize it later }
          SetLength(FInternalFieldsValueOffset, FDataset.FieldDefs.Count + FDataset.Fields.Count);

          { fkData Fields }
          for i := 0 to Pred(FDataset.FieldDefs.Count)
              do begin
                 FInternalFieldsValueOffset[i] := FRecordSize;
                 Inc(FRecordSize, GetFieldSize(FDataset.FieldDefs[i].DataType));
                 end;
          { Other fields Types (Calculated or lookup) }
          j := FDataset.FieldDefs.Count;
          for i := 0 to Pred(FDataset.Fields.Count)
              do if FDataset.Fields[i].FieldKind <> fkData then
                 begin
                 FInternalFieldsValueOffset[j] := FRecordSize;
                 Inc(FRecordSize, GetFieldSize(FDataset.Fields[i].DataType));
                 Inc(j);
                 end;
          { Returns the number of fields, including calc or lookup fields }
          Result := j;

          { Set the definitive Fields Offset array size }
          SetLength(FInternalFieldsValueOffset, j);
         end
     { No TDataset.fields are defined so we use Strings to store any values
      (stored in FAnsiStrings array) }
     else begin
          SetLength(FInternalFieldsValueOffset, FDataFieldsCount);
          for i := 0 to Pred(FDataFieldsCount) do
              begin
              FInternalFieldsValueOffset[i] := FRecordSize;
              Inc(FRecordSize, GetFieldSize(ftString));
              end;
          Result := FDataFieldsCount;
          end;

  { Finally, downsize the record of the extra field status byte }
  Dec(FRecordSize, SizeOf(Byte));
end;

procedure TSqlitePassRecordset.PrepareDataReader(FDataset: TDataset);
var
i, j: Integer;
Field: TField;

procedure SetDataReader(i: Integer);
begin
with FDataReader[i] do
    begin

    { Safe assignation to a dummy procedure }
    Load := @SqliteValueToDummy;
    BufferSize := GetFieldSize(FDataset.FieldDefs[i-FRowIdTablesCount].DataType);

    Case FDataset.FieldDefs[i-FRowIdTablesCount].DataType of
      ftBoolean:
      begin
      Case FDatabase.DatatypeOptions.FBooleanStorage of
           asInteger : Load := @SqliteValueToWordBool;
           asText    : Load := @SqliteValueTextToWordBool;
           end;
      end;

      ftSmallint, ftInteger, ftWord:
      begin
      Load := @SqliteValueToInteger;
      end;

      ftAutoInc, ftLargeint:
      begin
      Load := @SqliteValueToInt64;
      end;

      ftFloat, ftCurrency :
      begin
      Load := @SqliteValueToDouble;
      end;

      ftBCD:
      begin
      Load := @SqliteValueToCurrency;
      end;

      ftDate:
      begin
      Case FDatabase.DatatypeOptions.DateStorage of
           asInteger : Load := @SqliteValueToInteger;
           asText    : Load := @SqliteValueTextToDate;
           end;
      end;

      ftTime:
      begin
      Case FDatabase.DatatypeOptions.TimeStorage of
           asInteger : Load := @SqliteValueToInteger;
           asText    : Load := @SqliteValueTextToTime;
           end;
      end;

      ftDateTime:
      begin
      Case FDatabase.DatatypeOptions.DateTimeStorage of
           dtsDateTime : Load := @SqliteValueToDouble;
           dtsText     : Load := @SqliteValueDateTimeTextToDouble;
           dtsJulian   : Load := @SqliteValueJulianDateTimeToDouble;
           dtsUnix     : Load := @SqliteValueUnixDateTimeToDouble;
           dtsMac      : Load := @SqliteValueMacDateTimeToDouble;
           end;
      end;

      ftString, ftFixedChar:
      begin
      if FDatabase.FDatatypeOptions.FUnicodeEncoding = ueRawText
         then Load := @SqliteValueToRawString
         else Case FDatabase.SystemEncoding of
                   sysANSI  : Load := @SqliteValueToAnsiString;
                   sysUTF8  : Load := @SqliteValueToUTF8String;
                   sysUTF16 : Load := @SqliteValueToAnsiString;
                   end;
      end;

      ftWideString:
      begin
      if FDatabase.FDatatypeOptions.FUnicodeEncoding = ueRawText
         then Load := @SqliteValueToRawWideString
         else Load := @SqliteValueToUTF16WideString;
      end;

      ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary, ftBytes, ftVarBytes
     {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF} :
      begin
      Load := @SqliteValueToBlob;
      end;

      else begin { Unkown field type ? }
           if FDatabase.FDatatypeOptions.FUnicodeEncoding = ueRawText
              then begin
                   Case FDatabase.SystemEncoding of
                        sysANSI  : Load := @SqliteValueToRawString;
                        sysUTF8  : Load := @SqliteValueToRawString;
                        sysUTF16 : Load := @SqliteValueToRawWideString;
                        end;
                   end
              else begin
                   Case FDatabase.SystemEncoding of
                        sysANSI  : Load := @SqliteValueToAnsiString;
                        sysUTF8  : Load := @SqliteValueToUTF8String;
                        sysUTF16 : Load := @SqliteValueToUTF16WideString;
                        end;
                   end;
           end;
      end; { Case}
   end; { With }
end;



begin
SetLength(FDataReader,0);
Finalize(FDataReader);

if Assigned(FDataset)
   then begin
        { Avoid int64 compiler warning...}
        SetLength(FDataReader, Int64(FRowIdTablesCount) + FFieldsCount);

        { RowId Columns }
        for i := 0 to Pred(FRowIdTablesCount) do
            with FDataReader[i] do
               begin
               Load := @SqliteValueToInt64;
               BufferSize := SizeOf(Int64);
               end;

        { Dataset Data Fields Columns }
        for i := FRowIdTablesCount to Pred(FColumnsCount)
            do SetDataReader(i);

       { Other fields Types (Calculated or lookup) }
        j := FColumnsCount;
        for i := 0 to Pred(FDataset.Fields.Count)
            do begin
               Field := FDataset.Fields[i];
               if Field.FieldKind <> fkData then
                  begin
                  with FDataReader[j] do
                       begin
                       Load := @SqliteValueToDummy;
                       BufferSize := GetFieldSize(Field.DataType);
                       end;
                  Inc(j);
                  end;
               end;
        end
   else begin { Not Assigned(FDataset) }
        SetLength(FDataReader, FDataFieldsCount);
        For i := 0 to Pred(FDataFieldsCount) do
            begin
            Case FDatabase.FSystemEncoding of
                 sysANSI  : FDataReader[i].Load := @SqliteValueToAnsiString;
                 sysUTF8  : FDataReader[i].Load := @SqliteValueToUTF8String;
                 sysUTF16 : FDataReader[i].Load := @SqliteValueToUTF16WideString;
                 end;
            FDataReader[i].BufferSize := GetFieldSize(ftString);
            end;
        end;
end;


procedure TSqlitePassRecordset.GetRecords(Const Sql: String);
var
IsNull, IsFiltered: Boolean;
PreparedStmt: Pointer;
Buffer: PRecBuffer;
ColumnIndex, ErrorCode: Integer;

begin

Try
{ Set the screen cursor }
FDatabase.ShowBusyScreenCursor;

{ Clear Memory pointers, blocs...etc }
IsFiltered := False;

{ Prepare the SQL statement }
PreparedStmt := nil;
FDatabase.Engine.PrepareStmt(PreparedStmt, Sql);

{FColumnsCount is made of :
 n columns to store each table rowid + n columns for data }
FColumnsCount := SqliteDbv3_column_count(PreparedStmt);

{ Sometimes there is no data at all }
if FColumnsCount = 0
   then Abort;

{ Get the Datafields Count. }
FDataFieldsCount := FColumnsCount - FRowIdTablesCount;

{ Set the record size and get the total number of fields including calculated and lookup }
FFieldsCount := SetFRecordSize;

{ Set the Data Reader and Translator }
PrepareDataReader(FDataset);

if Assigned(FDataset) then
  begin
  FRecordMemoryBlocCapacity := FDataset.RecordsCacheCapacity;
  FDirectPost := (FDataset.FWriteMode = wmDirect);
  IsFiltered := FDataset.Filtered;
  end;

ErrorCode :=SqliteDbv3_Step(PreparedStmt);

{ Load the records : Proceed with every records until we reach UpperLimit or we get an error }
While (ErrorCode = SQLITE_ROW) do
  begin
  { Update Records Count and Capacity }
  _AddRecord;

  { Set the buffer to the new record }
  Buffer := FRecords[FRecordsCount];

  { Move the pointer to the first RowId or field Data buffer address }
  Inc(Buffer, FRowIdsOffset);

  { We get the RowId values for the current data row }
  for ColumnIndex := 0 to Pred(FRowIdTablesCount) do
      With FDataReader[ColumnIndex] do
      begin
      Load(Self, Buffer, PreparedStmt, ColumnIndex);
      Inc(Buffer, BufferSize);
      end;

  { Skip the first field Status flag and move to the field value buffer address }
  Inc(Buffer, SizeOf(Byte));

  { We get the fields values for the current data row }
  for ColumnIndex := FRowIdTablesCount to Pred(FColumnsCount) do
      begin
      Try
      IsNull := SqliteDbv3_column_type(PreparedStmt, ColumnIndex) = SQLITE_NULL;
      With FDataReader[ColumnIndex] do
           begin
           if IsNull
              then SetFieldNullValue(Buffer, True)
              else Load(Self, Buffer, PreparedStmt, ColumnIndex);
           Inc(Buffer, BufferSize);
           end;
      except
      { Exception while retreiving data are trapped here
        so we can continu with the next available value }
      On E: Exception do;
      end;
      end;

  { Initialize Calculated and lookup fields as NULL }
  for ColumnIndex := FColumnsCount to Pred(Int64(FRowIdTablesCount)+FFieldsCount) do
      begin
      SetFieldNullValue(Buffer, True);
      Inc(Buffer, FDataReader[ColumnIndex].BufferSize);
      end;

  Inc(FRecordsCount);
  ErrorCode :=SqliteDbv3_Step(PreparedStmt);
end; { While }

// TODO (FStatementTimeout=0) or ((FStatementTimeout>0) and (integer(GetTickCount)>=t));

{ Create a utility record to be able to restore a record modified by user to
  it previous state if needed }
FActiveRecordOldValues := AllocMem(FRecordSize);

{ Disable the remaining records flag in the last memorybloc }
DisableRemainingRecords;

{ Need to show only filtered records ? }
if IsFiltered
   then ShowFilteredRecords;

finally
FDatabase.RestoreScreenCursor;
{ Free the prepared Statement }
FDatabase.Engine.UnPrepareStmt(PreparedStmt);
end;
end;

procedure TSqlitePassRecordset.DisableRemainingRecords;
var
i, LastMemoryBlocIndex: Integer;
RecBuffer: PRecBuffer;
begin
LastMemoryBlocIndex := Pred(FRecordsMemoryBlocs.Count);
if LastMemoryBlocIndex > -1 then
   begin
   For i := FRecordsUsedInMemoryBloc to Pred(FRecordMemoryBlocCapacity) do
      begin
      RecBuffer := PRecBuffer(FRecordsMemoryBlocs[LastMemoryBlocIndex]) + (FRecordSize * i);
      SetRecordState(RecBuffer, rsEnabledFlag, rsNotEnabled);
      end;
   end;
end;

procedure TSqlitePassRecordset.ShowFilteredRecords;
var
SavedState: TDataSetState;

   { Build a new list of pointers on records that passed the filters.
   This sub procedure is used when RecordLowerLimit and RecordUpperLimit
   are not set }
   procedure _ShowFilteredRecords;
   var
   i,j,Delta: Integer;
   RecBuffer: PRecBuffer;
   Accept: Boolean;

   begin
   { Clear Records Pointers }
   SetFRecordsCapacity(0);
   FRecordsCount := 0;

   { Scan the records and apply filters }
   For i := 0 to Pred(FRecordsMemoryBlocs.Count) do
     begin
     RecBuffer := PRecBuffer(FRecordsMemoryBlocs[i]);
     For  j := 0 to Pred(FRecordMemoryBlocCapacity) do
       begin
       { Upsize the list if needed }
       if FRecordsCount = FRecordsCapacity then
          begin
          if FRecordsCapacity > 64 then Delta := FRecordsCapacity div 4
             else if FRecordsCapacity > 8 then Delta := 16
                     else Delta := 4;
          SetFRecordsCapacity(FRecordsCapacity + Delta);
          end;

       if GetRecordState(RecBuffer, rsEnabledFlag, rsEnabled) then
          begin
          Accept := FDataset.FFieldFilters.AcceptRecord(RecBuffer);
          If Accept then
             begin
             FRecords[FRecordsCount] := RecBuffer;
             FCurrentRecordIndex := FRecordsCount;
             Inc(FRecordsCount);
             if Assigned(FDataset.OnFilterRecord)
                 then FDataset.OnFilterRecord(FDataset, Accept);
             If Accept
                then SetRecordState(RecBuffer, rsVisibleFlag, rsVisible)
                else Dec(FRecordsCount);
             end;
          end;
       {End}
       { Move to the next record on the Memory Bloc Address }
       Inc(RecBuffer, FRecordSize);
       end; {i}
   end; {j}
   end;

   { Build a new list of pointers on all existing records.
   then shrink the list to fit into RecordLowerLimit and RecordUpperLimit
   and delete the pointers that don't pass the filters.
   This sub procedure is used when RecordLowerLimit and RecordUpperLimit
   are set }
   procedure _ShowFilteredRecordsWithLimits;
   var
   i, LowerLimit, UpperLimit: Integer;
   RecBuffer: PRecBuffer;
   Accept: Boolean;
      
   begin
   { Rebuild a list a all records available }
   ShowAllRecords;

    { Upper limit }
    UpperLimit := Min(FDataset.FFilterRecordUpperLimit, FRecordsCount);
    if UpperLimit > 0
       then FRecordsCount := UpperLimit
       else if UpperLimit < 0 then
            begin
            System.Move(FRecords[FRecordsCount], FRecords,(FRecordsCount + UpperLimit) * SizeOf(pRecBuffer));
            Inc(FRecordsCount, UpperLimit);
            end;

    { Lower limit }
     LowerLimit := Min(FDataset.FFilterRecordLowerLimit, FRecordsCount);
     System.Move(FRecords[LowerLimit], FRecords[0], (FRecordsCount - LowerLimit) * SizeOf(pRecBuffer));
     Dec(FRecordsCount, LowerLimit);

     { Scan the records and apply filters }
     For i := 0 to Pred(FRecordsCount) do
       begin
       RecBuffer := FRecords[i];
       if GetRecordState(RecBuffer, rsEnabledFlag, rsEnabled) then
          begin
          Accept := FDataset.FFieldFilters.AcceptRecord(RecBuffer);
          If Accept then
             begin
             if Assigned(FDataset.OnFilterRecord) then
                begin
                FCurrentRecordIndex := i;
                FDataset.OnFilterRecord(FDataset, Accept);
                end;
             If Accept
                then SetRecordState(RecBuffer, rsVisibleFlag, rsVisible)
                else DeleteRecord(FCurrentRecordIndex);
             end;
            end;
         end; {i}
   end;


begin
   FDatabase.ShowBusyScreenCursor;
   SavedState := FDataset.SetTempState(dsFilter);
   FDataset.FFieldFilters.InitActiveFilters;
   if   (fcLowerLimit in FDataset.FFilterChanges) or (FDataset.FFilterRecordLowerLimit <> 0)
     or (fcUpperLimit in FDataset.FFilterChanges) or (FDataset.FFilterRecordUpperLimit <> 0)
        then _ShowFilteredRecordsWithLimits
        else _ShowFilteredRecords;

   FDataset.FFilterChanges := [];
   FDataset.RestoreState(SavedState);
   FDatabase.RestoreScreenCursor;
end;

procedure TSqlitePassRecordset.GotoRecord(RecBufferBookmark: PRecBuffer);
var
i: Integer;
begin
if RecBufferBookmark <> nil then
   begin
   for i := 0 to Pred(FRecordsCount) do
       if RecBufferBookmark = FRecords[i] then
          begin
          FCurrentRecordIndex := i;
          Break;
          end;
   end;
end;


procedure TSqlitePassRecordset.BuildAllRecordsList;
var
i, j, Delta: Integer;
RecBuffer: PRecBuffer;
begin
{ Clear Records Pointers }
SetFRecordsCapacity(0);
FRecordsCount := 0;
For i :=0 to Pred(FRecordsMemoryBlocs.Count) do
  begin
  RecBuffer := PRecBuffer(FRecordsMemoryBlocs[i]);
  For j := 0 to Pred(FRecordMemoryBlocCapacity) do
    begin
    { Upsize the list if needed }
    if FRecordsCount = FRecordsCapacity then
       begin
       if FRecordsCapacity > 64 then Delta := FRecordsCapacity div 4
          else if FRecordsCapacity > 8 then Delta := 16
                  else Delta := 4;
       SetFRecordsCapacity(FRecordsCapacity + Delta);
       end;

    if GetRecordState(RecBuffer, rsEnabledFlag, rsEnabled)
       and GetRecordState(RecBuffer, rsDeletedFlag, rsNotDeleted) then
       begin
       SetRecordState(RecBuffer, rsVisibleFlag, rsVisible);
       FRecords[FRecordsCount] := RecBuffer;
       Inc(FRecordsCount);
       end;
    { Move to the next record on the Memory Bloc Address }
    Inc(RecBuffer, FRecordSize);
    end; {j}
  end; {i}
end;

procedure TSqlitePassRecordset.ShowAllRecords;
var
RecBufferBookMark: PRecBuffer;

begin
FDatabase.ShowBusyScreenCursor;

{ Save a bookmark on current record }
if (FRecordsCount > 0) and (ActiveRecordIndex > -1)
   then RecBufferBookMark := FRecords[ActiveRecordIndex]
   else RecBufferBookMark := nil;

BuildAllRecordsList;

{ Sort the dataset if needed }
if FDataset.Sorted
    then FDataset.Sort;

{ Go back to the bookmarked record }
GotoRecord(RecBufferBookMark);
FDatabase.RestoreScreenCursor;
end;

{ functions to translate a SQLite value to its internal storage value in memory }

procedure SqliteValueToDummy
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
{ Just a empty procedure to ensure DataReader procedure pointer is always assigned }
end;

procedure SqliteValueToWordBool
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
BooleanValue: WordBool;
begin
BooleanValue := SqliteDbv3_column_int(PreparedStmt,ColumnIndex) <> 0;
Move(BooleanValue, FieldValueBuffer^, SizeOf(WordBool));
end;

procedure SqliteValueTextToWordBool
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
BooleanValue: WordBool;
begin
Recordset.FDatabase.FTranslator.BooleanTextToWordBool(SqliteDbv3_column_text(PreparedStmt,ColumnIndex),BooleanValue);
Move(BooleanValue, FieldValueBuffer^, SizeOf(WordBool));
end;


procedure SqliteValueToInteger(Const Recordset: TSqlitePassRecordset;
                               Const FieldValueBuffer: PRecBuffer;
                               Const PreparedStmt: Pointer;
                               Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
IntegerValue: Integer;
begin
    IntegerValue := SqliteDbv3_column_int(PreparedStmt,ColumnIndex);
    Move(IntegerValue, FieldValueBuffer^, SizeOf(Integer));
end;

procedure SqliteValueToInt64(Const Recordset: TSqlitePassRecordset;
                             Const FieldValueBuffer: PRecBuffer;
                             Const PreparedStmt: Pointer;
                             Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
Int64Value: Int64;
begin
  Int64Value := SqliteDbv3_column_int64(PreparedStmt,ColumnIndex);
  Move(Int64Value, FieldValueBuffer^, SizeOf(Int64));
end;

procedure SqliteValueToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
  DoubleValue := SqliteDbv3_column_double(PreparedStmt,ColumnIndex);
  Move(DoubleValue, FieldValueBuffer^, SizeOf(Double));
end;

procedure SqliteValueToCurrency
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
Int64Value: Int64;
CurrencyValue: Currency;
begin
  Int64Value := SqliteDbv3_column_int64(PreparedStmt,ColumnIndex);
  CurrencyValue := Int64Value / 10000;
  Move(CurrencyValue, FieldValueBuffer^, SizeOf(Currency));
end;

procedure SqliteValueTextToDate
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
IntegerValue: Integer;
begin
  Recordset.FDatabase.FTranslator.DateTextToInteger(SqliteDbv3_column_text(PreparedStmt,ColumnIndex),IntegerValue);
  Move(IntegerValue, FieldValueBuffer^, SizeOf(Integer));
end;

procedure SqliteValueTextToTime
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
IntegerValue: Integer;
begin
  Recordset.FDatabase.FTranslator.TimeTextToInteger(SqliteDbv3_column_text(PreparedStmt,ColumnIndex),IntegerValue);
  Move(IntegerValue, FieldValueBuffer^, SizeOf(Integer));
end;


procedure SqliteValueJulianDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
  Recordset.FDatabase.FTranslator.JulianDateTimeToDouble(SqliteDbv3_column_double(PreparedStmt,ColumnIndex),DoubleValue);
  Move(DoubleValue, FieldValueBuffer^, SizeOf(Double));
end;

procedure SqliteValueDateTimeTextToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
  Recordset.FDatabase.FTranslator.DateTimeTextToDouble(SqliteDbv3_column_text(PreparedStmt,ColumnIndex),DoubleValue);
  Move(DoubleValue, FieldValueBuffer^, SizeOf(Double));
end;

procedure SqliteValueUnixDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
  Recordset.FDatabase.FTranslator.UnixDateTimeToDouble(SqliteDbv3_column_int64(PreparedStmt,ColumnIndex),DoubleValue);
  Move(DoubleValue, FieldValueBuffer^, SizeOf(Double));
end;

procedure SqliteValueMacDateTimeToDouble
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
  Recordset.FDatabase.FTranslator.MacDateTimeToDouble(SqliteDbv3_column_int64(PreparedStmt,ColumnIndex),DoubleValue);
  Move(DoubleValue, FieldValueBuffer^, SizeOf(Double));
end;

procedure SqliteValueToRawString( Const Recordset: TSqlitePassRecordset;
                                  Const FieldValueBuffer: PRecBuffer;
                                  Const PreparedStmt: Pointer;
                                  Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
RawStr: AnsiString;
StringSize: Cardinal;
begin
StringSize := SqliteDbv3_column_bytes(PreparedStmt, ColumnIndex);
SetString(RawStr, PAnsiChar(SqliteDbv3_column_blob(PreparedStmt, ColumnIndex)), StringSize);
Recordset._AddAnsiString(FieldValueBuffer, RawStr);
end;


procedure SqliteValueToAnsiString(Const Recordset: TSqlitePassRecordset;
                                  Const FieldValueBuffer: PRecBuffer;
                                  Const PreparedStmt: Pointer;
                                  Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset._AddAnsiString(FieldValueBuffer, SqlitePassUtils.UTF8toAnsi(SqliteDbv3_column_text(PreparedStmt, ColumnIndex)));
end;

procedure SqliteValueToUTF8String(Const Recordset: TSqlitePassRecordset;
                                  Const FieldValueBuffer: PRecBuffer;
                                  Const PreparedStmt: Pointer;
                                  Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset._AddAnsiString(FieldValueBuffer, SqliteDbv3_column_text(PreparedStmt, ColumnIndex));
end;


procedure SqliteValueToRawWideString( Const Recordset: TSqlitePassRecordset;
                                  Const FieldValueBuffer: PRecBuffer;
                                  Const PreparedStmt: Pointer;
                                  Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
RawStr: AnsiString;
StringSize: Cardinal;
begin
StringSize := SqliteDbv3_column_bytes(PreparedStmt, ColumnIndex);
SetString(RawStr, PAnsiChar(SqliteDbv3_column_blob(PreparedStmt, ColumnIndex)), StringSize);
Recordset._AddWideString(FieldValueBuffer, RawStr);
end;

procedure SqliteValueToUTF16WideString(Const Recordset: TSqlitePassRecordset;
                                   Const FieldValueBuffer: PRecBuffer;
                                   Const PreparedStmt: Pointer;
                                   Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
{$IFDEF UseTNTComponents}
 Recordset._AddWideString(FieldValueBuffer, SqliteDbv3_column_text(PreparedStmt, ColumnIndex));
{$ELSE}
 Recordset._AddWideString(FieldValueBuffer, SqliteDbv3_column_text16(PreparedStmt, ColumnIndex));
{$ENDIF}
end;

procedure SqliteValueToBlob(Const Recordset: TSqlitePassRecordset;
                            Const FieldValueBuffer: PRecBuffer;
                            Const PreparedStmt: Pointer;
                            Const ColumnIndex: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
BlobStream: TMemoryStream;
BlobSize: Cardinal;
begin
BlobSize := SqliteDbv3_column_bytes(PreparedStmt, ColumnIndex);
if BlobSize > 0 then
   begin
   BlobStream := TMemoryStream(Recordset.FMemoryStreams[Recordset._AddMemoryStream(FieldValueBuffer)]);
   BlobStream.Write(SqliteDbv3_column_blob(PreparedStmt, ColumnIndex)^, BlobSize);
   end
   else Recordset.SetFieldNullValue(FieldValueBuffer , True);
end;


{ --- Navigating --- }

Procedure TSqlitePassRecordset.First;
begin
FCurrentRecordIndex := -1;
end;

Procedure TSqlitePassRecordset.Prior;
begin
MoveBy(-1)
end;

Procedure TSqlitePassRecordset.Next;
begin
MoveBy(1)
end;

Procedure TSqlitePassRecordset.Last;
begin
FCurrentRecordIndex := FRecordsCount;
{$IFDEF FPC}
//FOldActiveRecordIndex := FRecordsCount;
{$ENDIF}
end;

Procedure TSqlitePassRecordset.MoveBy(Offset: Integer);
begin
MoveState := grError;

Case Offset of

   { Move to Previous record }
   -1 : Dec(FCurrentRecordIndex);
    0 : ;
    1 : Inc(FCurrentRecordIndex);
    end;

if (FCurrentRecordIndex < 0)
   then begin
        MoveState := grBOF;
        FCurrentRecordIndex := -1;
        end
   else if (FCurrentRecordIndex > Pred(FRecordsCount))
           then begin
                MoveState := grEOF;
                FCurrentRecordIndex := FRecordsCount;
                end
           else begin
                MoveState := grOk;
                SetBookMarkFlag(FCurrentRecordIndex, bfCurrent);
                end;
end;


{ --- Managing Records (Create a new record, Add, Insert, Delete from records list) --- }

// TODO Set default values if needed }
(*For i := 0 to Pred(Fields.Count) do
    if Fields[i].FieldKind = fkData
       then Result^.FieldsValues[Pred(Fields[i].FieldNo)] := Fields[i].DefaultExpression;*)

procedure TSqlitePassRecordset.SetFRecordsCapacity(Const NewCapacity: Integer);
begin
  ReallocMem(FRecords, NewCapacity * SizeOf(PRecBuffer));
  FRecordsCapacity := NewCapacity;
end;

{ Add a new record to the end of the Records list and
  create a new memory bloc of records if necessary}
procedure TSqlitePassRecordset._AddRecord;
var
Delta: Integer;
begin
  { Get another bloc of memory if needed }
  if FRecordsUsedInMemoryBloc = FRecordMemoryBlocCapacity then
     begin
     FRecordsMemoryBlocs.Add(AllocMem(FRecordSize * FRecordMemoryBlocCapacity));
     FRecordsUsedInMemoryBloc := 0;
     end;
  { Upsize the list if needed }
  if FRecordsCount = FRecordsCapacity then
     begin
     if FRecordsCapacity > 64 then Delta := FRecordsCapacity div 4
        else if FRecordsCapacity > 8 then Delta := 16
                else Delta := 4;
     SetFRecordsCapacity(FRecordsCapacity + Delta);
     end;
  { Assign the new record on the Memory Bloc Address }
  FRecords[FRecordsCount] := PRecBuffer(FRecordsMemoryBlocs[Pred(FRecordsMemoryBlocs.Count)]) + (FRecordSize * FRecordsUsedInMemoryBloc);
  Inc(FRecordsUsedInMemoryBloc);
end;

{ Add a new 'temporary' record
  AddTempRecord checks first for any recycled records memory bloc to reuse
  and upsize the memory if necessary
  Returns the index of the next available record
  We don't inc FRecordCount so Dataset 'doesn't know' about the new record
  until the PostRecord procedure is called  }
function TSqlitePassRecordset.AddTempRecord: Integer;
begin
Result := FRecordsCount;
if FRecycledRecords.Count > 0
   then FRecords[Result] := PRecBuffer(FRecycledRecords[Pred(FRecycledRecords.Count)])
   else _AddRecord;
SetFieldsNullValue(Result);
end;

{ Add a new record }
function TSqlitePassRecordset.AddRecord: Integer;
begin
  Result := AddTempRecord;
  PostRecord;
end;

{ Insert a new record :
  The record itself is added to the next available place
  in the last memory bloc.
  Then a new pointer is assigned to this record and inserted at the right place in the FRecords array }
Procedure TSqlitePassRecordset.InsertRecord(Index: Integer);
var
TempRecord: PRecBuffer;
begin
 TempRecord := FRecords[FRecordsCount];
 System.Move(FRecords[Index], FRecords[Index + 1],(FRecordsCount - Index) * SizeOf(PRecBuffer));
 FRecords[Index] := TempRecord;
end;

{ Make the record added with AddTempRecord 'visible' for the dataset }
procedure TSqlitePassRecordset.PostRecord;
begin
{ Remove the temp record from the Recycled list }
if FRecycledRecords.Count > 0
   then FRecycledRecords.Delete(Pred(FRecycledRecords.Count));

{ Go back to the last active record position }
If (FOldActiveRecordIndex > -1)
  then begin
       InsertRecord(FOldActiveRecordIndex);
       FCurrentRecordIndex := FOldActiveRecordIndex;
       FOldActiveRecordIndex := -1;
      { Set the record status flag to "inserted" }
       SetRecordState(FRecords[FCurrentRecordIndex], rsInsertedFlag, rsInserted);
       SetRecordState(FRecords[FCurrentRecordIndex], rsEnabledFlag, rsEnabled);
       end
  else begin
       SetRecordState(FRecords[FRecordsCount], rsInsertedFlag, rsInserted);
       SetRecordState(FRecords[FRecordsCount], rsEnabledFlag, rsEnabled);
       end;
{ Makes the new record visible to the dataset.GetRecord function }
Inc(FRecordsCount);

{ Set the recordset changed flag to True -> Sorting, Filtering need update }
FRecordsChanged := True;
end;

procedure TSqlitePassRecordset.Cancel;
begin
{ Move back to the Old selected record }
if FOldActiveRecordIndex > -1 then
   begin
   FCurrentRecordIndex := FOldActiveRecordIndex;
   FOldActiveRecordIndex := -1;
   end;
end;

Procedure TSqlitePassRecordset._AddAnsiString(Const FieldValueBuffer: PRecBuffer;
                                             Const StrValue: AnsiString);
var
StrIndex: Integer;
begin
 StrIndex := FAnsiStrings.Count;
 FAnsiStrings.Add(StrValue);
 Move(StrIndex, FieldValueBuffer^, SizeOf(Integer));
end;

{ Add a String and Returns the Index of the new String in FAnsiStrings Array }
Procedure TSqlitePassRecordset.AddAnsiString(Const FieldValueBuffer: PRecBuffer;
                                             Const StrValue: AnsiString);
var
StrIndex,
LastRecycledString: Integer;
begin
 if FRecycledAnsiStrings.Count > 0
    then begin
         LastRecycledString := Pred(FRecycledAnsiStrings.Count);
         StrIndex := FRecycledAnsiStrings[LastRecycledString];
         FAnsiStrings[StrIndex] := StrValue;
         FRecycledAnsiStrings.Delete(LastRecycledString);
         end
    else begin
         StrIndex := FAnsiStrings.Count;
         FAnsiStrings.Add(StrValue);
         end;

 Move(StrIndex, FieldValueBuffer^, SizeOf(Integer));
 SetFieldNullValue(FieldValueBuffer, False);
end;

procedure TSqlitePassRecordset.AddAnsiString(Const RecordIndex, FieldIndex: Integer;
                                             Const StrValue: AnsiString);
var
FieldValueBuffer: PRecBuffer;
begin
 FieldValueBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);
 AddAnsiString(FieldValueBuffer, StrValue);
end;

procedure TSqlitePassRecordset.ClearAnsiString(Const FieldValueBuffer: PRecBuffer);
var
StrIndex: Integer;
begin
if Not FieldIsNull(FieldValueBuffer) then
   begin
   StrIndex := PInteger(FieldValueBuffer)^;
   FAnsiStrings[StrIndex] := '';
(*   if FDirectPost
      then FRecycledAnsiStrings.Add(PInteger(StrIndex));*)
   if FDirectPost
       then FRecycledAnsiStrings.Add(PInteger(FieldValueBuffer)^);
   SetFieldNullValue(FieldValueBuffer, True);
   end;
end;

procedure TSqlitePassRecordset.ClearAnsiString(Const RecordIndex, FieldIndex: Integer);
begin
  ClearAnsiString(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

procedure TSqlitePassRecordset._AddWideString(Const FieldValueBuffer: PRecBuffer;
                                              Const StrValue: UTF16WideString);
var
StrIndex: Integer;
begin
 StrIndex := FWideStrings.Count;
 FWideStrings.Add(StrValue);
 Move(StrIndex, FieldValueBuffer^, SizeOf(Integer));
end;

{ Add a WideString and Returns the Index of the new WideString in FWideStrings Array }
procedure TSqlitePassRecordset.AddWideString(Const FieldValueBuffer: PRecBuffer;
                                             Const StrValue: UTF16WideString);
var
StrIndex,
LastRecycledWideString: Integer;
begin
 if FRecycledWideStrings.Count > 0
    then begin
         LastRecycledWideString := Pred(FRecycledWideStrings.Count);
         StrIndex := FRecycledWideStrings[LastRecycledWideString];
         FWideStrings[StrIndex] := StrValue;
         FRecycledWideStrings.Delete(LastRecycledWideString);
         end
    else begin
         StrIndex := FWideStrings.Count;
         FWideStrings.Add(StrValue);
         end;
 Move(StrIndex, FieldValueBuffer^, SizeOf(Integer));
 SetFieldNullValue(FieldValueBuffer, False);
 end;

procedure TSqlitePassRecordset.AddWideString(Const RecordIndex, FieldIndex: Integer;
                                             Const StrValue: UTF16WideString);
var
FieldValueBuffer: PRecBuffer;
begin
 FieldValueBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);
 AddWideString(FieldValueBuffer, StrValue);
end;

procedure TSqlitePassRecordset.ClearWideString(Const FieldValueBuffer: PRecBuffer);
var
StrIndex: Integer;
begin
if Not FieldIsNull(FieldValueBuffer) then
   begin
   StrIndex := PInteger(FieldValueBuffer)^;
   FWideStrings[StrIndex] := '';
   FRecycledWideStrings.Add(PInteger(FieldValueBuffer)^);
   SetFieldNullValue(FieldValueBuffer, True);
   end;
end;

procedure TSqlitePassRecordset.ClearWideString(Const RecordIndex, FieldIndex: Integer);
begin
  ClearWideString(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

function TSqlitePassRecordset._AddMemoryStream(Const FieldValueBuffer: PRecBuffer): Integer;
begin
 Result := FMemoryStreams.Add(TMemoryStream.Create);
 Move(Result, FieldValueBuffer^, SizeOf(Integer));
end;

{ Add a MemoryStream and Returns the index of the MemoryStream in FMemoryStreams }
function TSqlitePassRecordset.AddMemoryStream(Const FieldValueBuffer: PRecBuffer): Integer;
var
LastRecycledIndex: Integer;
begin
  if FRecycledMemoryStreams.Count > 0
     then begin
     LastRecycledIndex := Pred(FRecycledMemoryStreams.Count);
     Result := FRecycledMemoryStreams[LastRecycledIndex];
     FRecycledMemoryStreams.Delete(LastRecycledIndex);
     end
     else Result := FMemoryStreams.Add(TMemoryStream.Create);
  Move(Result, FieldValueBuffer^, SizeOf(Integer));
  SetFieldNullValue(FieldValueBuffer, False);
end;

function TSqlitePassRecordset.AddMemoryStream(Const RecordIndex, FieldIndex: Integer): Integer;
var
FieldValueBuffer: PRecBuffer;
begin
 FieldValueBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);
 Result := AddMemoryStream(FieldValueBuffer);
end;

procedure TSqlitePassRecordset.ClearMemoryStream(Const FieldValueBuffer: PRecBuffer);
var
BlobIndex: Integer;
begin
if Not FieldIsNull(FieldValueBuffer) then
   begin
   BlobIndex := PInteger(FieldValueBuffer)^;
   TMemoryStream(FMemoryStreams[BlobIndex]).Clear;
   FRecycledMemoryStreams.Add(PInteger(FieldValueBuffer)^);
   SetFieldNullValue(FieldValueBuffer, True);
   end;
end;

procedure TSqlitePassRecordset.ClearMemoryStream(Const RecordIndex, FieldIndex: Integer);
begin
   ClearMemoryStream(GetFieldValueBuffer(FRecords[RecordIndex], FieldIndex));
end;

Function TSqlitePassRecordset.GetFieldAsBlob(Const RecordIndex, FieldIndex: Integer): TMemoryStream;
begin
 Result := GetFieldAsBlob(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

Function TSqlitePassRecordset.GetFieldAsBlob(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): TMemoryStream;
begin
 Result := GetFieldAsBlob(GetFieldValueBuffer(RecordBuffer, FieldIndex));
end;

Function TSqlitePassRecordset.GetFieldAsBlob(Const FieldBuffer: PRecBuffer): TMemoryStream;
begin
if FieldIsNull(FieldBuffer)
   then Result := nil
   else Result := FMemoryStreams[PInteger(FieldBuffer)^];
end;

{ Delete FRecords[Index] from the FRecords array and downsize the array
  The record itself is not freed
  NO checking of array bounds }
procedure TSqlitePassRecordset.DeleteRecord(Index: Integer);
begin
  if FDirectPost then
     begin
     ClearRecord(Index);
     FRecycledRecords.Add(PRecBuffer(FRecords[Index]));
     Dec(FRecordsUsedInMemoryBloc);
     end;
  SetRecordState(FRecords[Index], rsDeletedFlag, rsDeleted);
  Dec(FRecordsCount);
  if Index < FRecordsCount
     then System.Move(FRecords[Succ(Index)], FRecords[Index],(FRecordsCount - Index) * SizeOf(pRecBuffer));
  FRecords[FRecordsCount] := nil;
  { Set the recordset changed flag to True -> Sorting, Filtering need update }
  FRecordsChanged := True;
end;

{ Copy a record from Source to Dest }
procedure TSqlitePassRecordset.CopyRecord(Const Source, Dest: PRecBuffer);
var
i, SourceBlobStreamIndex, DestBlobStreamIndex: Integer;
SourceFieldValueBuffer, DestFieldValueBuffer: PRecBuffer;
begin
{ We should not need to copy record if FDataset is not assigned, so it is not
  implemented }
if Not Assigned(FDataset) then Exit;

Move(Source^, Dest^, FRecordSize);
For i := 0 to Pred(FDataset.Fields.Count) do
   begin
   Case FDataset.Fields[i].DataType of

       { Copy Blobs }
        ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary, ftBytes, ftVarBytes
       {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF} :
         begin
         SourceFieldValueBuffer := GetFieldValueBuffer(Source, FDataset.Fields[i].Tag);
         if Not FieldIsNull(SourceFieldValueBuffer) then
            begin
            SourceBlobStreamIndex := PInteger(SourceFieldValueBuffer)^;
            if SourceBlobStreamIndex > -1 then
               begin
               DestFieldValueBuffer := GetFieldValueBuffer(Dest, FDataset.Fields[i].Tag);
               DestBlobStreamIndex := AddMemoryStream(DestFieldValueBuffer);
               TMemoryStream(FMemoryStreams[DestBlobStreamIndex]).LoadFromStream(TMemoryStream(FMemoryStreams[SourceBlobStreamIndex]));
               end;
            end;
         end;

// TODO - Not Necessary ?
{ Copy WideStrings }
{      ftWideString, ftBytes, ftVarBytes, ftFixedChar, ftWideWideString:
         begin
         FieldPos := FDataset.Fields[i].Tag;
         StrIndex :=  PInteger(Source + FInternalFieldsOffset[FieldPos])^;
         SourceWideString := FWideStrings[StrIndex];
         AddWideString(Dest + FInternalFieldsOffset[FieldPos], SourceWideString);
         end; }

     end; { Case }
end; { for }
end;

procedure TSqlitePassRecordset.SetFieldsNullValue(Const RecordIndex: Integer);
var
i: Integer;
begin
for i := 0 to High(FInternalFieldsValueOffset)
   do SetFieldNullValue(RecordIndex, i, True);
end;


procedure TSqlitePassRecordset.ClearRecord(Index: Integer);
var
i: Integer;
begin
For i := 0 to Pred(FDataset.Fields.Count) do
   begin
   Case FDataset.Fields[i].DataType of
        { Clear Blobs }
        ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary, ftBytes, ftVarBytes
        {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF}
                              : ClearMemoryStream(Index, FDataset.Fields[i].Tag);
        { Clear Strings content }
        ftString, ftFixedChar : ClearAnsiString(Index, FDataset.Fields[i].Tag);
        ftWideString          : ClearWideString(Index, FDataset.Fields[i].Tag);
        end;
   end; { for }
{ Clear Buffer }
FillChar(FRecords[Index]^, FRecordSize, 0);
{ Mark the record as disabled }
PByte(FRecords[Index] + FRecordStateOffset)^ := PByte(FRecords[Index] + FRecordStateOffset)^ and not (1 shl rsNotEnabled);
end;

Procedure TSqlitePassRecordset.ExchangeRecords(Const RecordIndex1, RecordIndex2: Integer);
var
TempRecord: PRecBuffer;
begin
 TempRecord := FRecords[RecordIndex1];
 FRecords[RecordIndex1] := FRecords[RecordIndex2];
 FRecords[RecordIndex2] := TempRecord;
end;


{ --- Active record old values backup / restore when editing --- }

procedure TSqlitePassRecordset.SaveActiveRecordOldValues;
begin
  CopyRecord(FRecords[ActiveRecordIndex], FActiveRecordOldValues);
end;

procedure TSqlitePassRecordset.RestoreActiveRecordOldValues;
begin
  CopyRecord(FActiveRecordOldValues, FRecords[ActiveRecordIndex]);
end;

procedure TSqlitePassRecordset.ClearActiveRecordOldValues;
var
i: Integer;
begin
For i := 0 to Pred(FDataset.Fields.Count) do
   Case FDataset.Fields[i].DataType of
       { Clear Blobs }
       ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary, ftBytes, ftVarBytes
       {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF} : ClearMemoryStream(GetFieldValueBuffer(FActiveRecordOldValues, FDataset.Fields[i].Tag));
       end;
{ Initialize the new record with 'blank' values }
FillChar(FActiveRecordOldValues^, FRecordSize, 0);
{ Set the recordset changed flag to True -> Sorting, Filtering need update }
FRecordsChanged := True;
end;


{ Read / Write Record BookMarkFlag }
Function TSqlitePassRecordset.GetBookMarkFlag(Const RecordIndex: Integer): TBookMarkFlag;
begin
  Result := PBookMarkFlag(FRecords[RecordIndex])^;
end;

Procedure TSqlitePassRecordset.SetBookMarkFlag(Const RecordIndex: Integer; Const Value: TBookMarkFlag);
begin
{$IFDEF FPC}
  Move(Value, FRecords[RecordIndex]^, SizeOf(Value));
{$ELSE}
  PBookMarkFlag(FRecords[RecordIndex])^ := Value;
{$ENDIF}
end;

{ Read / Write a record BookMarkData }
Function TSqlitePassRecordset.GetBookMarkData(Const RecordIndex: Integer): Cardinal;
begin
  Result := PCardinal(FRecords[RecordIndex]+ FBookMarkDataOffset)^;
  if FOldActiveRecordIndex = -1
     then FOldActiveRecordIndex := RecordIndex;
end;

Procedure TSqlitePassRecordset.SetBookMarkData(Const RecordIndex: Integer; Const Value: Cardinal);
begin
  PCardinal(FRecords[RecordIndex]+ FBookMarkDataOffset)^ := Value;
end;

{ Read / Write Record RecordState }
Function TSqlitePassRecordset.GetRecordState(Const RecordBuffer: PRecBuffer;
                                             Const RecordStateType: TSqlitePassRecordStateType;
                                             Const Value: TSqlitePassRecordStateValue):  Boolean;
begin
  if Value = 0
     then Result := (PByte(RecordBuffer + FRecordStateOffset)^ and (1 shl RecordStateType)) = Value
     else Result := (PByte(RecordBuffer + FRecordStateOffset)^ and (1 shl RecordStateType)) <> 0;
end;


Function TSqlitePassRecordset.GetRecordState(Const RecordIndex: Integer;
                                             Const RecordStateType: TSqlitePassRecordStateType;
                                             Const Value: TSqlitePassRecordStateValue):  Boolean;
begin
 Result := GetRecordState(FRecords[RecordIndex], RecordStateType, Value);
end;

Procedure TSqlitePassRecordset.SetRecordState(Const RecordBuffer: PRecBuffer; Const RecordStateType: TSqlitePassRecordStateType; Const Value: TSqlitePassRecordStateValue);
begin
   If Value = 0
      then PByte(RecordBuffer + FRecordStateOffset)^ := PByte(RecordBuffer + FRecordStateOffset)^ and not (1 shl RecordStateType)
      else PByte(RecordBuffer + FRecordStateOffset)^ := PByte(RecordBuffer + FRecordStateOffset)^ or (1 shl RecordStateType);
end;

Procedure TSqlitePassRecordset.SetRecordState(Const RecordIndex: Integer; Const RecordStateType: TSqlitePassRecordStateType; Const Value: TSqlitePassRecordStateValue);
begin
 SetRecordState(FRecords[RecordIndex], RecordStateType, Value);
end;

Procedure TSqlitePassRecordset.ResetRecordState(Const RecordBuffer: PRecBuffer);
begin
  PByte(RecordBuffer + FRecordStateOffset)^ := 0;
  ResetFieldsState(RecordBuffer);
end;

{ Read / Write Record Rowid - TableNo is zero indexed }
Function TSqlitePassRecordset.GetRowId(Const RecordBuffer: PRecBuffer; Const TableNo: Word): Int64;
begin
  Result := PInt64(RecordBuffer + FRowIdsOffset + (TableNo * SizeOf(Int64)))^;
end;

Function TSqlitePassRecordset.GetRowId(Const RecordIndex: Integer; Const TableNo: Word): Int64;
begin
  Result := PInt64(FRecords[RecordIndex] + FRowIdsOffset + (TableNo * SizeOf(Int64)))^;
end;

Procedure TSqlitePassRecordset.SetRowId(Const RecordBuffer: PRecBuffer; Const TableNo: Word; Const Value: Int64);
begin
  PInt64(RecordBuffer + FRowIdsOffset + (TableNo * SizeOf(Int64)))^ := Value;
end;

Procedure TSqlitePassRecordset.SetRowId(Const RecordIndex: Integer; Const TableNo: Word; Const Value: Int64);
begin
  PInt64(FRecords[RecordIndex] + FRowIdsOffset + (TableNo * SizeOf(Int64)))^ := Value;
end;

{ Returns a pointer (PRecBuffer) to the field value inside a record
  RecordIndex and FieldIndex are zero indexed }
Function TSqlitePassRecordset.GetFieldValueBuffer(Const RecordIndex, FieldIndex: Integer): PRecBuffer;
begin
  Result := FRecords[RecordIndex] + FInternalFieldsValueOffset[FieldIndex];
end;

Function TSqlitePassRecordset.GetFieldValueBuffer(Const RecordBuffer: PRecBuffer;Const FieldIndex: Integer): PRecBuffer;
begin
  Result := RecordBuffer + FInternalFieldsValueOffset[FieldIndex];
end;


{ Strings }
Function TSqlitePassRecordset.GetFieldAsAnsiString(Const FieldValueBuffer: PRecBuffer): AnsiString;
var
StrIndex: Integer;
begin
if FieldIsNull(FieldValueBuffer)
   then Result := ''
   else begin
        StrIndex := PInteger(FieldValueBuffer)^;
        Result := FAnsiStrings[StrIndex];
        end;
end;

Function TSqlitePassRecordset.GetFieldAsAnsiString(Const RecordIndex, FieldIndex: Integer): AnsiString;
begin
  Result := GetFieldAsAnsiString(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

Procedure TSqlitePassRecordset.SetFieldAsAnsiString(Const FieldValueBuffer: PRecBuffer;
                                                    Const Value: AnsiString);
var
StrIndex: Integer;
begin
if FieldIsNull(FieldValueBuffer)
   then AddAnsiString(FieldValueBuffer, Value)
   else begin
        StrIndex :=  PInteger(FieldValueBuffer)^;
        FAnsiStrings[StrIndex] := Value;
        end;
end;

Procedure TSqlitePassRecordset.SetFieldAsAnsiString(Const RecordIndex, FieldIndex: Integer;
                                                    Const Value: AnsiString);
begin
 SetFieldAsAnsiString(GetFieldValueBuffer(RecordIndex, FieldIndex), Value);
end;

{ WideStrings }
Function TSqlitePassRecordset.GetFieldAsWideString(Const FieldValueBuffer: PRecBuffer): UTF16WideString;
var
StrIndex: Integer;
begin
if FieldIsNull(FieldValueBuffer)
   then Result := ''
   else begin
        StrIndex := PInteger(FieldValueBuffer)^;
        Result := FWideStrings[StrIndex];
        end;
end;


Function TSqlitePassRecordset.GetFieldAsWideString(Const RecordIndex, FieldIndex: Integer): UTF16WideString;
begin
  Result := GetFieldAsWideString(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

Procedure TSqlitePassRecordset.SetFieldAsWideString(Const FieldValueBuffer: PRecBuffer;
                                                    Const Value: UTF16WideString);
var
StrIndex: Integer;
begin
if FieldIsNull(FieldValueBuffer)
   then AddWideString(FieldValueBuffer, Value)
   else begin
        StrIndex :=  PInteger(FieldValueBuffer)^;
        FWideStrings[StrIndex] := Value;
        end;
end;


Procedure TSqlitePassRecordset.SetFieldAsWideString(Const RecordIndex, FieldIndex: Integer;
                                                    Const Value: UTF16WideString);
begin
  SetFieldAsWideString(GetFieldValueBuffer(RecordIndex, FieldIndex), Value);
end;

{ Variants }
Function TSqlitePassRecordset.GetFieldAsVariant(Const RecordIndex, FieldIndex: Integer;
                                                Const Field: TField): Variant;
var
FieldValueBuffer: PRecBuffer;
begin
Result := UnAssigned;
FieldValueBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);

if FieldIsNull(FieldValueBuffer)
   then Result := VarNull
   else Case Field.DataType of
        ftWord,
        ftSmallint,
        ftInteger,
        ftDate,
        ftTime      : Result := PInteger(FieldValueBuffer)^;

        ftLargeInt  : {$IFDEF HasInt64Variant}
                      Result := PInt64(FieldValueBuffer)^;
                      {$ELSE}
                      Result := IntToStr(PInt64(FieldValueBuffer)^);
                      {$ENDIF}
        ftBoolean   : Result := PWordBool(FieldValueBuffer)^;

        ftFloat,
        ftCurrency  : Result := PDouble(FieldValueBuffer)^;

        ftBCD       : Result := PCurrency(FieldValueBuffer)^;

        ftDateTime  : Result := VarFromDateTime(PDouble(FieldValueBuffer)^);
        
        ftString, ftBytes
                    : Result := GetFieldAsAnsiString(FieldValueBuffer);
        ftWideString: Result := GetFieldAsWideString(FieldValueBuffer);

        end;
end;


{ Read and Write a record 'Null' Boolean Flag }
Function TSqlitePassRecordset.FieldIsNull(Const RecordIndex, FieldIndex: Integer): Boolean;
begin
  Result := FieldIsNull(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

Function TSqlitePassRecordset.FieldIsNull(Const FieldValueBuffer: PRecBuffer): Boolean;
begin
 Result := (PByte(FieldValueBuffer - SizeOf(Byte))^ and (1 shl fsNullFlag)) <> fsNotNull;
end;

Function TSqlitePassRecordset.FieldIsNull(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): Boolean;
begin
  Result := FieldIsNull(GetFieldValueBuffer(RecordBuffer, FieldIndex));
end;

Procedure TSqlitePassRecordset.SetFieldNullValue(Const RecordIndex, FieldIndex: Integer; Const Value: Boolean);
begin
  SetFieldNullValue(GetFieldValueBuffer(RecordIndex, FieldIndex), Value);
end;

Procedure TSqlitePassRecordset.SetFieldNullValue(Const FieldValueBuffer: PRecBuffer; Const Value: Boolean);
begin
  If Value
     then PByte(FieldValueBuffer - SizeOf(Byte))^ := PByte(FieldValueBuffer - SizeOf(Byte))^ or (1 shl fsNullFlag)
     else PByte(FieldValueBuffer - SizeOf(Byte))^ := PByte(FieldValueBuffer - SizeOf(Byte))^ and not (1 shl fsNullFlag);
end;

{ Read and Write a record 'Modified' Boolean Flag }
Function TSqlitePassRecordset.FieldWasModified(Const RecordIndex, FieldIndex: Integer): Boolean;
begin
  Result := FieldWasModified(GetFieldValueBuffer(RecordIndex, FieldIndex));
end;

Function TSqlitePassRecordset.FieldWasModified(Const FieldValueBuffer: PRecBuffer): Boolean;
begin
 Result := (PByte(FieldValueBuffer - SizeOf(Byte))^ and (1 shl fsModifiedFlag)) <> fsNotModified;
end;

Function TSqlitePassRecordset.FieldWasModified(Const RecordBuffer: PRecBuffer; Const FieldIndex: Integer): Boolean;
begin
  Result := FieldWasModified(GetFieldValueBuffer(RecordBuffer, FieldIndex));
end;

Procedure TSqlitePassRecordset.SetFieldWasModifiedValue(Const RecordIndex, FieldIndex: Integer; Const Value: Boolean);
begin
  SetFieldWasModifiedValue(GetFieldValueBuffer(RecordIndex, FieldIndex), Value);
end;

Procedure TSqlitePassRecordset.SetFieldWasModifiedValue(Const FieldValueBuffer: PRecBuffer; Const Value: Boolean);
begin
  If Value
     then PByte(FieldValueBuffer - SizeOf(Byte))^ := PByte(FieldValueBuffer - SizeOf(Byte))^ or (1 shl fsModifiedFlag)
     else PByte(FieldValueBuffer - SizeOf(Byte))^ := PByte(FieldValueBuffer - SizeOf(Byte))^ and not (1 shl fsModifiedFlag);
end;

Procedure TSqlitePassRecordset.ResetFieldsState(Const RecordBuffer: PRecBuffer);
var
i: Integer;
FieldBuffer: PRecBuffer;
IsNull: Boolean;
begin
 For i := 0 to High(FInternalFieldsValueOffset) do
     begin
     FieldBuffer := GetFieldValueBuffer(RecordBuffer, i);
     IsNull := FieldIsNull(FieldBuffer);
     PByte(FieldBuffer - SizeOf(Byte))^  := 0;
     SetFieldNullValue(FieldBuffer, IsNull);
     end;
end;

Procedure TSqlitePassRecordset.ResetFieldsState(Const RecordIndex: Integer);
begin
  ResetFieldsState(FRecords[RecordIndex]);
end;

{ --- Direct access to fields values, when no TFields are defined ---
  RecordIndex and FieldIndex are zero based }

{ Returns a String for a given RecordIndex and FieldIndex }
Function TSqlitePassRecordset.GetTextItem(Const RecordIndex, FieldIndex: Integer): String;
var
StrIndex: Integer;
FieldValueBuffer: PRecBuffer;
begin
FieldValueBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);
if FieldIsNull(FieldValueBuffer)
   then Result := ''
   else begin
        StrIndex := PInteger(FieldValueBuffer)^;
        Case FDatabase.FSystemEncoding of
             sysANSI  : Result := FAnsiStrings[StrIndex];
             sysUTF8  : Result := FAnsiStrings[StrIndex];
             sysUTF16 : Result := FWideStrings[StrIndex];
             end;
        end;
end;

{ Set a String (Ansi or WideString) for a given RecordIndex and FieldIndex }
Procedure TSqlitePassRecordset.SetTextItem(Const RecordIndex, FieldIndex: Integer; Const Value: String);
var
StrIndex: Integer;
begin
  StrIndex := PInteger(GetFieldValueBuffer(RecordIndex, FieldIndex))^;
  Case FDatabase.FSystemEncoding of
       sysANSI  : FAnsiStrings[StrIndex] := Value;
       sysUTF8  : FAnsiStrings[StrIndex] := Value;
       sysUTF16 : FWideStrings[StrIndex] := Value;
       end;
end;

{ Add a String (Ansi or WideString) for a given RecordIndex and FieldIndex
  Field Index is zero Indexed }
Procedure TSqlitePassRecordset.AddTextItem(Const RecordIndex, FieldIndex: Integer; Const Value: String);
var
FieldBuffer: PRecBuffer;
begin
  FieldBuffer := GetFieldValueBuffer(RecordIndex, FieldIndex);
  Case FDatabase.FSystemEncoding of
       sysANSI  : AddAnsiString(FieldBuffer, SqlitePassUtils.UTF8ToAnsi(PAnsiChar(Value)));
       sysUTF8  : AddAnsiString(FieldBuffer, Value);
       sysUTF16 : AddWideString(FieldBuffer, Value);
       end;
end;

{ Indexing }
procedure TSqlitePassRecordset.BuildIndexFromRecordsList(InMemoryIndex: TSqlitePassInMemoryIndex; Const Ascending: Boolean);
var
i: Integer;
begin
{ Clear Index Pointers }
InMemoryIndex.FIndexedRecords.Clear;
if Assigned(FRecords) then
   begin
   InMemoryIndex.FIndexedRecords.Capacity := Pred(FRecordsCount);
   If Ascending
      then for i := 0 to Pred(FRecordsCount) do InMemoryIndex.FIndexedRecords.Add(FRecords[i])
      else for i := Pred(FRecordsCount) downto 0 do InMemoryIndex.FIndexedRecords.Add(FRecords[i]);
   end;
end;

procedure TSqlitePassRecordset.BuildRecordsListFromIndex(InMemoryIndex: TSqlitePassInMemoryIndex; Const Ascending: Boolean);
var
i, j: Integer;
begin
If Ascending
  then begin
       for i :=0 to Pred(InMemoryIndex.FIndexedRecords.Count)
         do FRecords[i] := InMemoryIndex.FIndexedRecords[i];
       end
  else begin
       j := 0;
       for i := Pred(FRecordsCount) downto 0 do
         begin
         FRecords[j] := InMemoryIndex.FIndexedRecords[i];
         Inc(j);
         end;
       end;
end;

{ Sorting }

Procedure TSqlitePassRecordset.SortRecords(var SortedFields: TSqlitePassInMemoryIndexInfos;
                                           RecordBufferList: PRecBufferList);
var
InMemoryIndex: TSqlitePassInMemoryIndex;
TempRecord: PRecBuffer;
MidRecordBuffer: PPRecBuffer;
MidRecordBuffers: TMidRecordBuffers;

procedure InternalQuickSort(iLo, iHi: Integer);
var
i, Lo, Hi: Integer;

begin
Repeat
  { 16 is a constant in order to switch to Insertion sort when QuickSort becomes
    inefficient }
  if (iHi-iLo) < 16
    then begin
         InsertionSort(Self, RecordBufferList, MidRecordBuffers, SortedFields, iLo, iHi);
         Lo:=iHi;
         end
    else begin
         Lo := iLo;
         Hi := iHi;
         MidRecordBuffer^ := RecordBufferList[(Lo + Hi) shr 1];

         { Init MidRecordsBuffers array }
         for i := 0 to High(MidRecordBuffers) do
           begin
           MidRecordBuffers[i].FieldBuffer := MidRecordBuffer^ + SortedFields[i].FieldValueOffset;
           MidRecordBuffers[i].FieldIsNull := (PByte(MidRecordBuffers[i].FieldBuffer - SizeOf(Byte))^ and (1 shl fsNullFlag)) = fsNull;
           end;

         Repeat
           While CompareRecord(Self, MidRecordBuffers, SortedFields, RecordBufferList[Lo]) < 0 do Inc(Lo);
           While CompareRecord(Self, MidRecordBuffers, SortedFields, RecordBufferList[Hi]) > 0 do Dec(Hi);

           if (Lo <= Hi) then
             begin
             TempRecord := RecordBufferList[Lo];
             RecordBufferList[Lo] := RecordBufferList[Hi];
             RecordBufferList[Hi] := TempRecord;
             Inc(Lo);
             Dec(Hi);
             end
         until lo > Hi;

         if Hi>iLo then InternalQuickSort(iLo, Hi);
         iLo := Lo;
         end;

 Until Lo >= iHi
end;

{ SortRecords }
begin
// réf 1 : 7.797 sec for 300.000 Strings with release (initial code }
// réf 2 : 6.66 sec for 300.000 Strings with release + selection sort
// réf 3 : 6.35 sec for 300.000 Strings with release + Insertion sort limit 25
// réf 4 : 6.28 sec for 300.000 Strings with release + Insertion sort limit 16
// réf 5 : 6.25 sec for 300.000 Strings with release + Insertion sort limit 16 + Unrolled loops

// -> Réf 4 is the current implementation

  If (FRecordsCount = 0) or (High(SortedFields) = -1) then Exit;

  FDatabase.ShowBusyScreenCursor;
  New(MidRecordBuffer);
  SetLength(MidRecordBuffers, Succ(High(SortedFields)));
  Try
  InMemoryIndex := FDataset.FInMemoryIndexes.IndexByField(SortedFields[0].Field);

  if FRecords = RecordBufferList
    { We want to sort the records }
     then begin
          if Assigned(InMemoryIndex) and InMemoryIndex.IsReady
             { An Index is ready ? We use it to pre-sort the records list }
             then begin
                  BuildRecordsListFromIndex(InMemoryIndex, SortedFields[0].Ascending);
                  { More than one field to sort ? then we need to sort the other fields }
                  if High(SortedFields) > 0
                     then InternalQuickSort(0, Pred(FRecordsCount));
                  end
             { No Index is ready ? We sort the records list }
             else begin
                  InternalQuickSort(0, Pred(FRecordsCount));
                  { We always build an index on the sorted records }
                  if Assigned(InMemoryIndex)
                     then BuildIndexFromRecordsList(InMemoryIndex, SortedFields[0].Ascending);
                  end;   
           end
     { We just want to buil an index }
     else InternalQuickSort(0, Pred(FRecordsCount));

  FRecordsChanged := False;
  finally
  FDatabase.RestoreScreenCursor;
  SetLength(MidRecordBuffers,0);
  Dispose(MidRecordBuffer);
  end;
end;

function CompareRecord(Recordset: TSqlitePassRecordset; var MidRecordBuffers: TMidRecordBuffers; var SortedFields: TSqlitePassInMemoryIndexInfos; RecordBuffer: PRecBuffer): Integer;{$IFDEF HasInline} inline; {$ENDIF}
var
i:integer;
FieldBuffer: PRecBuffer;
FieldBufferIsNull: Boolean;
begin
Result := 0;

for i := 0 to High(SortedFields) do
    begin
    FieldBuffer := RecordBuffer + SortedFields[i].FieldValueOffset;
    FieldBufferIsNull := ((PByte(FieldBuffer - SizeOf(Byte))^ and (1 shl fsNullFlag)) <> fsNotNull);

    { The two values are NOT Null }
    if Not (FieldBufferIsNull or MidRecordBuffers[i].FieldIsNull)
       then Result := SortedFields[i].ValueComparator(Recordset, FieldBuffer, MidRecordBuffers[i].FieldBuffer)
            else if MidRecordBuffers[i].FieldIsNull and Not FieldBufferIsNull then Result := 1
            else if FieldBufferIsNull and Not MidRecordBuffers[i].FieldIsNull then Result := -1
            else Result := 0;
    { Skips the others fields comparisons if values are already different }
    if Result <> 0 then Break;
    end; { for }
end;

procedure InsertionSort(Recordset: TSqlitePassRecordset; RecordBufferList: PRecBufferList; var MidRecordBuffers: TMidRecordBuffers; var SortedFields: TSqlitePassInMemoryIndexInfos;Const Lo, Hi: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
i, j, k: Integer;
MidRecordBuffer: PRecBuffer;
begin
  for i := Lo to Hi do
    begin
    j := i;
    MidRecordBuffer := RecordBufferList[j];

    { Init MidRecordsBuffers array }
    for k := 0 to High(MidRecordBuffers) do
      begin
      MidRecordBuffers[k].FieldBuffer := MidRecordBuffer + SortedFields[k].FieldValueOffset;
      MidRecordBuffers[k].FieldIsNull := (PByte(MidRecordBuffers[k].FieldBuffer - SizeOf(Byte))^ and (1 shl fsNullFlag)) = fsNull;
      end; { For k }

    while (j > 0) and (CompareRecord(Recordset, MidRecordBuffers, SortedFields,RecordBufferList[j-1]) > 0) do
      begin
      RecordBufferList[j] := RecordBufferList[j-1];
      Dec(j);
      end;  { While }

    RecordBufferList[j] := MidRecordBuffer;
    end; { For }
end;
{ Sort Comparison functions }
function CompareDummyValue(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
{ Just a empty procedure to ensure ValueComparator
  procedure pointer is always assigned.
  It returns a arbitry value based on basic pointer comparison}
if PInteger(FieldValueBuffer1)^ > PInteger(FieldValueBuffer2)^ then Result := 1
   else if PInteger(FieldValueBuffer1)^ < PInteger(FieldValueBuffer2)^ then Result := -1
   else Result := 0;
end;


{ Sort Comparison functions - Desc }
function CompareWordBoolValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PWordBool(FieldValueBuffer1)^ > PWordBool(FieldValueBuffer2)^ then Result := -1
   else if PWordBool(FieldValueBuffer1)^ < PWordBool(FieldValueBuffer2)^ then Result := 1
        else Result := 0;
end;

function CompareIntegerValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PInteger(FieldValueBuffer1)^ > PInteger(FieldValueBuffer2)^ then Result := -1
   else if PInteger(FieldValueBuffer1)^ < PInteger(FieldValueBuffer2)^ then Result := 1
        else Result := 0;
end;

function CompareInt64ValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PInt64(FieldValueBuffer1)^ > PInt64(FieldValueBuffer2)^ then Result := -1
   else if PInt64(FieldValueBuffer1)^ < PInt64(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareDoubleValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PDouble(FieldValueBuffer1)^ > PDouble(FieldValueBuffer2)^ then Result := -1
   else if PDouble(FieldValueBuffer1)^ < PDouble(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareCurrencyValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PCurrency(FieldValueBuffer1)^ > PCurrency(FieldValueBuffer2)^ then Result := -1
   else if PCurrency(FieldValueBuffer1)^ < PCurrency(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareAnsiStringValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
  Result := AnsiCompareStr(Recordset.FAnsiStrings[PInteger(FieldValueBuffer2)^],
                           Recordset.FAnsiStrings[PInteger(FieldValueBuffer1)^]);
 { Case IN-sensitive
   curiously, AnsiCompareText is somewhat slower than AnsiCompareStr
   Result := AnsiCompareText(Recordset.FAnsiStrings[PInteger(FieldValueBuffer1)^],
                             Recordset.FAnsiStrings[PInteger(FieldValueBuffer2)^]); }
end;

function CompareWideStringValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
 Result := AnsiCompareStr(Recordset.FWideStrings[PInteger(FieldValueBuffer2)^],
                          Recordset.FWideStrings[PInteger(FieldValueBuffer1)^]);
end;

function CompareMemoValueDesc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
  Result := CompareMemoValueAsc(Recordset, FieldValueBuffer2, FieldValueBuffer1);
end;


{ Sort Comparison functions - Asc }
function CompareWordBoolValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PWordBool(FieldValueBuffer1)^ < PWordBool(FieldValueBuffer2)^ then Result := -1
   else if PWordBool(FieldValueBuffer1)^ > PWordBool(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareIntegerValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PInteger(FieldValueBuffer1)^ < PInteger(FieldValueBuffer2)^ then Result := -1
   else if PInteger(FieldValueBuffer1)^ > PInteger(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareInt64ValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PInt64(FieldValueBuffer1)^ < PInt64(FieldValueBuffer2)^ then Result := -1
   else if PInt64(FieldValueBuffer1)^ > PInt64(FieldValueBuffer2)^ then Result := 1
           else Result := 0;
end;

function CompareDoubleValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PDouble(FieldValueBuffer1)^ < PDouble(FieldValueBuffer2)^ then Result := -1
   else if PDouble(FieldValueBuffer1)^ > PDouble(FieldValueBuffer2)^ then Result := 1
        else Result := 0;
end;

function CompareCurrencyValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
if PCurrency(FieldValueBuffer1)^ < PCurrency(FieldValueBuffer2)^ then Result := -1
   else if PCurrency(FieldValueBuffer1)^ > PCurrency(FieldValueBuffer2)^ then Result := 1
        else Result := 0;
end;

function CompareAnsiStringValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
 Result := AnsiCompareStr(Recordset.FAnsiStrings[PInteger(FieldValueBuffer1)^],
                          Recordset.FAnsiStrings[PInteger(FieldValueBuffer2)^]);
 { Case IN-sensitive
   curiously, AnsiCompareText is somewhat slower than AnsiCompareStr
   Result := AnsiCompareText(Recordset.FAnsiStrings[PInteger(FieldValueBuffer1)^],
                             Recordset.FAnsiStrings[PInteger(FieldValueBuffer2)^]);}
end;

function CompareWideStringValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
begin
 Result := AnsiCompareStr(Recordset.FWideStrings[PInteger(FieldValueBuffer1)^],
                          Recordset.FWideStrings[PInteger(FieldValueBuffer2)^]);
end;

function CompareMemoValueAsc(Const Recordset: TSqlitePassRecordset; Const FieldValueBuffer1, FieldValueBuffer2: Pointer): Integer; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoryStream1, MemoryStream2: TMemorySTream;
MemoText1, MemoText2: UTF16WideString;

begin
  MemoryStream1 := Recordset.FMemoryStreams[PInteger(FieldValueBuffer1)^];
  MemoryStream2 := Recordset.FMemoryStreams[PInteger(FieldValueBuffer2)^];
  if MemoryStream1.Size > 0
     then SetString(MemoText1, pChar(MemoryStream1.Memory), MemoryStream1.Size)
     else MemoText1 := '';

  if MemoryStream2.Size > 0
     then SetString(MemoText2, pChar(MemoryStream2.Memory), MemoryStream2.Size)
     else MemoText2 := '';

  Result := AnsiCompareStr(MemoText1, MemoText2);
end;


