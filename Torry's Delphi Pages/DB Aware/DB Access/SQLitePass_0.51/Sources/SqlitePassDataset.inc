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


{ TSqlitePassDatasetVersionInfo }

constructor TSqlitePassDatasetVersionInfo.Create;
begin
inherited Create;
FComponent := SqlitePassDatasetVersion;
FPackage := SqlitePassPackageVersion;
end;

procedure TSqlitePassDatasetVersionInfo.SetFComponent(
  const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;

procedure TSqlitePassDatasetVersionInfo.SetFPackage(const Value: String);
begin
{ Read only - Used to display infos in object inspector }
end;


{ --- TSqlitePassDataset --- }

constructor TSqlitePassDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF FPC}
  ObjectView := True;   { ObjectView is set to True to speed up Fields Binding }
  {$ENDIF}
  if Assigned(AOwner) and (AOwner is TSqlitePassDatabase)
     then FDatabase := TSqlitePassDatabase(AOwner)
     else FDatabase := nil;
  FDatasetName := '';
  FDatasetType := dtUnknown;
  FInInternalOpen := False;
  FReadOnly := False;
  FWriteMode := wmDirect;
  FRecordsCacheCapacity := 100;
  FRecordset := TSqlitePassRecordset.Create(Self);
  { Indexing }
  FInMemoryIndexes := TSqlitePassInMemoryIndexes.Create(Self);
  FInMemoryIndexed := True;
  SetFInMemoryIndexedBy('');
  { Locate }
  FLocateSmartRefresh := False;
  FLocateFieldFilters := TSqlitePassFieldFilters.Create(Self, nil);
  { Lookup Fields }
  FLookupFieldsDisplayedRecordsOnly := False;
  FLookupFieldsList := TList.Create;
  FLookUpFieldsKeyFieldFilters := TSqlitePassFieldFilters.Create(Self, nil);
  FLookUpFieldsKeyFieldFilters.FResultRecordsMaxCount := 1;
  { LookUp }
  FLookUpCache := False;
  FLookUpSmartRefresh := False;
  FLookupResultFields := TList.Create;
  FLookUpKeyFieldFilters := TSqlitePassFieldFilters.Create(Self, nil);
  { Calculated Fields }
  FCalcFieldsList := TList.Create;
  FCalcDisplayedRecordsOnly := False;
  { Sorting }
  FSortMode := smDirect;
  SetFSortedBy('');
  { }
  FSQLChanges := [];
  FSQL := TStringList.Create;
  FSQLSelectStmt := TSqlitePassSelectStmt.Create(Self);
  FIndexDefs := TSqlitePassDatasetIndexDefs.Create(Self);
  FVersionInfo := TSqlitePassDatasetVersionInfo.Create;
  FDatabaseAutoActivate := True;
  FParams := TParams.Create(Self);
  { Master - Detail Filtering }
  FOnFilterRecord := nil;
  FDetailFields := TList.Create;
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FMasterAutoActivate := True;
  { Filters Filtering }
  FFilterChanges := [];
  FFieldFilters := TSqlitePassFieldFilters.Create(Self, OnFieldFiltersChange);
  FFilterMode := fmSQLDirect;
  { Lower - Upper Limit filtering }
  FFilterRecordLowerLimit := 0;
  FFilterRecordUpperLimit := 0;
  { BookmarkSize is used by TDataset }
  BookmarkSize := SizeOf(Integer);
  FRefetchRows := False;
  { Ensure correct info to FSQLStmt with FSQL.assign or gettext/settext }
  FSQL.OnChange := UpdateFSQL;
//  FOnExportProgress := nil;
//  FOnImportProgress := nil;
End;

destructor TSqlitePassDataset.Destroy;
Begin
  Close;

  { Force Database.Datasets to remove this dataset from the datasets list }
  Database := nil;

  FVersionInfo.Free;
  FIndexDefs.Free;
  FParams.Free;
  OnFilterRecord := nil;
  FFieldFilters.Free;
  { We Clear the FActiveFieldFilters first
    otherwise, filter items would be freed twice when calling FFieldFilters.Free }
  FMasterLink.OnMasterChange := nil;
  FMasterLink.OnMasterDisable := nil;
  FMasterLink.Free;
  FDetailFields.Free;
  FSQLSelectStmt.Free;
  FSQL.OnChange := nil;
  FSQL.Free;
  FCalcFieldsList.Free;
  FLocateFieldFilters.Free;
  FLookupFieldsKeyFieldFilters.Free;
  FLookupFieldsList.Free;
  FLookupKeyFieldFilters.Free;
  FLookupResultFields.Free;
  FInMemoryIndexes.Free;
  FRecordset.Free;
   // TODO Check FreeNotification / RemoveFreeNotification
  if Assigned(FDatabase) then FDatabase.FreeNotification(Self);
  {$IFDEF FPC}
    //FDatabase.RemoveFreeNotification(Self);
  {$ENDIF}
  inherited Destroy;
End;

Function TSqlitePassDataset.GetFDatabase:TSqlitePassDatabase;
Begin
 Result:=FDatabase;
End;

Procedure TSqlitePassDataset.SetFDatabase(Value:TSqlitePassDatabase);
Begin
 if Active then FDatabase.DatabaseError.RaiseException(Msg2011);

 { Previous FDatabase }
 if Assigned(FDatabase) then
    begin
    { We remove the previous dataset from the Database datasets list }
    FDatabase.Datasets.Remove(Self);
    // TODO : Check FSQLSelectStmt.FDatabase assignement
    FSQLSelectStmt.FDatabase:= nil;
    FRecordset.FDatabase := nil;
    end;

 FDatabase := Value;

 { New Database }
 if Assigned(FDatabase) then
    begin
    { We add this dataset to the Database datasets list }
    FDatabase.Datasets.Add(Self);
    { Re-Links FDatabase to the new one }
    FSQLSelectStmt.FDatabase:= FDatabase;
    FRecordset.FDatabase := FDatabase;
    end;
end;


Procedure TSqlitePassDataset.SetFDatasetName(const Value: String);
var
WasActive: Boolean;
Begin
if (Value = FDatasetFullName) and (Value <> '')
   then Exit;

if (csDesigning in ComponentState) and (FDatasetName <> '') and (Fields.Count > 0)
   then if MessageDlg(Msg2028, mtWarning, [mbYes, mbNo], 0) = mrNo
           then Exit;

FDatasetFullName := Value;
FDatasetName := Copy(Value, AnsiPos('.',Value)+1,Length(Value));

{ Resets properties which are specicific to a dataset
  and avoid to call Dataset.Close / Open twice if not csLoading }
if not (csLoading in ComponentState) then
   begin
   WasActive := Active;
   if WasActive
       then Close;

   { Resets properties which are specicific to a dataset
     and must be modified when dataset Name is changed }
   Filtered := False;
   Filter := '';
   FilterRecordLowerLimit := 0;
   FilterRecordUpperLimit := 0;
   Sorted := False;
   SortedBy := '';
   Indexed := False;
   IndexedBy := '';
   MasterSource := nil;

   { Sets the new dataset type and re-open if necessary }
   SetFDatasetType;
   Active := WasActive;
   end
   else SetFDatasetType;
End;


procedure TSqlitePassDataset.SetFDatasetType;
var
WasActive: Boolean;
i: integer;
SelectedFields, FieldIdentifier, TableIdentifier: String;
begin
FDatasetType := dtUnknown;
{ disable notification }
FSQL.OnChange := nil;
WasActive := Active;

if (Database = nil) or (Not Database.Connected) then exit;

Try
if WasActive
   then Close;

if FDatasetName = ''
   then FDatasetType := dtSQLDirect
   else if FDatabase.TableDefs.FindTable(FDatasetFullName) <> nil
           then FDatasetType := dtTable
           else if FDatabase.QueryDefs.FindQuery(FDatasetFullName) <> nil
                   then FDatasetType := dtQuery
                   else if FDatabase.Views.FindView(FDatasetFullName) <> nil
                        then FDatasetType := dtView
                        else if FDatabase.SQLStmtDefs.FindSQLStmt(FDatasetFullName) <> nil
                             then FDatasetType := dtSQLSelect;

Case FDatasetType of
     dtTable:   begin
                SelectedFields := '';
                UpdateIndexDefs;
                TableIdentifier := FDatasetFullName;
                FSQLSelectStmt.QuoteString(TableIdentifier);
                { We check if the designtime designer is active.
                  If so we select all the table fields to display them in the field designer
                  Else we select only the wanted fields to save memory }
                If Assigned(Fields) and (Fields.Count > 0) and not (csDesigning in ComponentState)
                   then begin
                        For i := 0 to Pred(Fields.Count)
                            do if Fields[i].FieldKind = fkData
                                  then begin
                                       FieldIdentifier := Fields[i].FieldName;
                                       FSQLSelectStmt.QuoteString(FieldIdentifier);
                                       SelectedFields  := SelectedFields + FieldIdentifier + ', ';
                                       end;
                        System.Delete(SelectedFields,Length(SelectedFields)-1,2);
                        end
                   else SelectedFields := '*';

                   FSQL.Text := 'SELECT ' +  SelectedFields + ' FROM ' + TableIdentifier + ';'
                end;
     dtQuery:   begin
                FSQL.Text := FDatabase.QueryDefs.QueryByName(FDatasetFullName).Sql;
                UpdateParamsList;
                end;
     dtView:    FSQL.Text := FDatabase.Views.ViewByName(FDatasetName).SqlSelectStmt;
     dtSQLSelect:FSQL.Text := FDatabase.SQLStmtDefs.SQLStmtByName(FDatasetName).SQL;
     else       begin { Dataset name doesn't exist in database ?}
                FDatasetFullName := '';
                FDatasetName := '';
                Include(FSQLChanges, scSelect);
                UpdateFSQLStmt;
                end;
     end;
Finally
{ Enable notification }
FSQL.OnChange := UpdateFSQL;

Active := WasActive;

{ if designtime and dataset is closed, force fielddefs update to display infos}
if (Not Active) and (csDesigning in ComponentState) and (aoRead in FSQLSelectStmt.AvailableOperations)
   then InternalInitFieldDefs;
end;
end;

procedure TSqlitePassDataset.SetFSQL(const Value: TStringList);
begin
{ This is necessary to avoid error at designtime... }
if Value.Text <> FSQL.Text
   then FSQL.Assign(Value);
end;

{ This procedure is called whenever a change is made to FSQL
  via TStringList(FSQL).OnChange := UpdateFSQL,
  to ensure correct assignement with FSQL.assign or gettext/settext }
Procedure TSqlitePassDataset.UpdateFSQL(Sender: TObject);
Begin
if (csLoading in ComponentState) then exit;
{ We consider, once the SQL statement is changed by end user,
  that the Dataset doesn't match a known database entry (table, query or view)
  Setting the DatasetName to '' will call SetFDatasetName and SetFDatasetType }
DatasetName := '';
Include(FSQLChanges, scSelect);
End;

{ This procedure must be called whenever a change is made to FSQL
  to ensure correct info to FSQLStmt }
procedure TSqlitePassDataset.UpdateFSQLStmt;
var
i: Integer;
WhereStmt, MasterDetailStmt, FilterStmt, OrderbyStmt, FieldName, FieldValue: String;

begin
if   ((FSQLChanges = []) and not (fcSQLFilter in FFilterChanges))
  or ((FSQLChanges = [scOrderBy]) and (FSortMode = smDirect))
     then Exit;

Try
{ Select part }
if scSelect in FSQLChanges then
   begin
   FSQLSelectStmt.SQL := FSQL.Text;
   FSQLSelectStmt.Prepare;
   Exclude(FSQLChanges, scSelect);
   FRefetchRows := True;
   end;

{ Where part }
if (scWhere in FSQLChanges) or (fcSQLFilter in FFilterChanges) then
   begin
   { Restore the "Where" part of the original SQL Stmt }
   WhereStmt := FSQLSelectStmt.OriginalSQLSections.GetSectionText(kwWhere);

   { Master/Detail Filtering }
   MasterDetailStmt := '';
   if (MasterFields <> '') and (FInInternalOpen or Active) and Assigned(FDetailFields)
      and Assigned(FMasterLink.Datasource) and (FMasterLink.Active) and (FDetailFields.Count > 0) then
      begin
      MasterDetailStmt := kwBraceOpen;
      for i := 0 to Pred(FDetailFields.Count) do
          begin
          FieldName := TField(FDetailFields[i]).FieldName;
          FSQLSelectStmt.QuoteString(FieldName);
          { FieldValue is always quoted with DoubleQuotes regardless to defined QuoteStyle
            because qsbrackets will raise an Sql error }
          FieldValue := '"' + TField(FMasterLink.Fields[i]).AsString + '"';
          MasterDetailStmt := MasterDetailStmt + FieldName + kwSpace + kwEqual + KwSpace + FieldValue;
          if i < Pred(FDetailFields.Count)
             then MasterDetailStmt := MasterDetailStmt + KwSpace + kwAnd + KwSpace;
          end;
      MasterDetailStmt := MasterDetailStmt + kwBraceClose;
      end;

   { Filter Filtering }
   FilterStmt := '';
   if Filtered and (FFilterSQLStmt <> '')
      then FilterStmt := kwSpace + kwBraceOpen + FFilterSQLStmt + kwBraceClose;

   if MasterDetailStmt <> '' then
      if WhereStmt = ''
         then WhereStmt := kwWhere + kwSpace + MasterDetailStmt
         else WhereStmt := WhereStmt + kwAnd + kwSpace + MasterDetailStmt;

   if FilterStmt <> '' then
      if WhereStmt = ''
         then WhereStmt := kwWhere + kwSpace + FilterStmt
         else WhereStmt := WhereStmt + kwAnd + kwSpace + FilterStmt;

   if WhereStmt <> ''
      then FSQLSelectStmt.SQLSections.ReplaceSectionText(kwWhere, WhereStmt);

   Exclude(FFilterChanges, fcSQLFilter);
   Exclude(FSQLChanges, scWhere);
   FRefetchRows := True;
   end;

{ OrderBy part - Sorting }
if scOrderBy in FSQLChanges then
   begin
   OrderByStmt := FSQLSelectStmt.OriginalSQLSections.GetSectionText(kwOrderBy);
   if Sorted and (Length(FSortedBy) > 0) then
      begin
      if OrderByStmt = ''
         then OrderByStmt := kwOrderBy + kwSpace + FSortedBy
         else OrderByStmt := OrderByStmt + kwAnd + kwSpace + MasterDetailStmt;
      end;

   FSQLSelectStmt.SQLSections.ReplaceSectionText(KwOrderBy, OrderByStmt);
   Exclude(FSQLChanges, scOrderBy);
   FRefetchRows := True;
   end;
{$IFDEF DEBUG_SQLITEPASS}
   FDatabase.Debugger.Display('SQL Statement : ' + FSQLSelectStmt.SQL);
{$ENDIF}

Except
 On EDatabaseError do
    FDatabase.DatabaseError.RaiseExceptionFmt(Msg3013, [CRLF, FSQLSelectStmt.SQL]);
end;
end;

Procedure TSqlitePassDataset.UpdateInternalFieldsInfos;
begin
{ Just an empty call }
Inherited;
end;

function TSqlitePassDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
  {$IFDEF UseTNTComponents}
  If Result = TStringField     then Result := TTntStringField;
  if Result = TMemoField then Result := TTntMemoField;
  {$ENDIF}
end;

Function TSqlitePassDataset.GetFReadOnly: Boolean;
Begin
If Assigned(FDatabase) and FDatabase.ReadOnly
   then FReadOnly := True;

If Active
   and (not (DatasetType in [dtTable, dtQuery, dtSQLSelect, dtSQLDirect]))
   or (FSqlSelectStmt.AvailableOperations = [aoRead])
   then FReadOnly := True;

Result  := FReadOnly;
End;

Procedure TSqlitePassDataset.SetFReadOnly(const Value:Boolean);
Begin
  FReadOnly := Value;
End;

Procedure TSqlitePassDataset.Loaded;
begin
try
  {DatasetType cannot be set properly while TSqlitePassDataset
   is loading from DFM or LFM. So we do it now }
  SetFDatasetType;
  inherited Loaded;
  //Active := FWaitingForConnection;
  //FWaitingForConnection := False;
except
  Application.HandleException(Self)
end;
end;

Procedure TSqlitePassDataset.Notification(AComponent: TComponent; Operation: TOperation); //Ok
Begin
 If (Operation = opRemove) and (AComponent = FDatabase) then FDatabase := Nil;
 Inherited Notification(AComponent, Operation);
End;


function TSqlitePassDataset.CheckCanOpen(VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
begin
Result := Active or FInInternalOpen;

if Not Result then
   begin
   // TODO : Handle FDatabase.DatabaseError.RaiseException even if FDatabase is not Assigned...
   if not Assigned(FDatabase)
      then DatabaseError(Msg2018);

   if (FDatasetName='') And (FSQL.Count=0)
      then FDatabase.DatabaseError.RaiseException(Msg2019, -1, Self,  VerboseLevel);

   if FDatabaseAutoActivate and (Not FDatabase.Connected)
      then FDatabase.Connected:=True;

   if Not FDatabase.Connected
      then if csDesigning in ComponentState
              then FDatabase.DatabaseError.RaiseException(Msg2020, -1, Self, VerboseLevel)
              else FDatabase.DatabaseError.RaiseException(Msg2021, -1, Self, VerboseLevel)
      else Result := True;
    end;
end;

{procedure InternalOpen; virtual; abstract;

 TDataset.Open -> DoInternalOpen -> InternalOpen
------------------------------------------
Open the dataset. You must call InternalInitFieldDefs;
if DefaultFields is True, then you must call CreateFields,
which will create the necessary TFields from the fielddefs.}
Procedure TSqlitePassDataset.InternalOpen;
Begin

CheckCanOpen;

try
  FInInternalOpen := True;
  try
  { Init FieldDefs }
  InternalInitFieldDefs;

  { If fields definitions are stored in *.lfm or *.dfm using design time fields editor,
    then they are created at runtime when the form is created, even if the dataset is closed.
    If no persistent fields have been created in the Fields Editor
    then FPC uses the TDataset.CreateFields (It calls FieldDefs.CreateField) to create fields dynamically. }
  if DefaultFields then CreateFields;

  { Activates or deactivates Lookup Cache (TField.CacheLookUp) }
  ActivateLookUpCache;

  { TDataset.BindFields sets up all sorts of internal
    things like field offsets, as well as creating any calculated
    or lookup fields. It is within BindFields that CalcFieldsSize
    is actually set for later use in InitBufferPointers }
  BindFields(True);

  { Init the InMemory Filters }
  FFieldFilters.InitFilters;
  FLocateFieldFilters.InitFilters;
  FLookupKeyFieldFilters.InitFilters;
  FLookupFieldsKeyFieldFilters.InitFilters;
  ProcessFilterText;
  
  { Init the InMemory Indexes }
  FInMemoryIndexes.InitIndexes;

  { Execute the Query }
  InternalRefresh;

  { Now Fields are defined, we can process MasterFields }
  if Assigned(MasterSource) then
     begin
     if FMasterAutoActivate and Assigned(MasterSource.Dataset)
        and (Not TSqlitePassDataset(MasterSource.Dataset).FInInternalOpen)
        then MasterSource.Dataset.Open;
     ProcessMasterDetailFields;
     end;

  { Exception ? }
  except
  On EDatabaseError do
     begin
     FRecordset.Close;
     FSQLSelectStmt.FAvailableOperations := [];
     FDatabase.DatabaseError.RaiseException(Msg2029);
     end;
  end;
finally
  FInInternalOpen := False;
end;
end;

Procedure TSqlitePassDataset.Refresh;
begin
  FRefetchRows := True;
  Inherited;
end;

{procedure InternalRefresh; virtual; abstract;
-------------------------------------------
Refresh the dataset.}
procedure TSqlitePassDataset.InternalRefresh;
begin
Try
   { Update the SQL Statement }
   UpdateFSQLStmt;

   If Not (aoRead in FSQLSelectStmt.AvailableOperations)
      then Exit;

   {  Queries data using SQL sentence : Refetch the records from Sqlite database engine }
   Try
   if FRefetchRows then
      begin
      FRecordset.Open(FSQLSelectStmt.SQL, FSQLSelectStmt.FPrimaryKeyCount);
      MapFieldsToMemoryFieldBuffers;
      if Not FLookupFieldsDisplayedRecordsOnly
         then FillLookupFields(0, Pred(FRecordset.FRecordsCount));
      if Not FCalcDisplayedRecordsOnly
         then FillCalcFields(0, Pred(FRecordset.FRecordsCount));
      FRefetchRows := False;
      end;

   SortRecords;
   RefreshLocate;
   RefreshLookup;
   Except
    Try
    { if an error occurs, we try to get back to the original SQL Stmt }
      FSQLSelectStmt.RestoreOriginalStmt;
      Filtered := False;
      UpdateFSQLStmt;
      FRecordset.Open(FSQLSelectStmt.SQL, FSQLSelectStmt.FPrimaryKeyCount);
    Except
      Raise;
    end;
   end;
Except
 Close;
 Raise;
end;
end;

procedure TSqlitePassDataset.RefreshActive;
begin
if Active then Inherited Refresh;
end;


{ Internal information to map a internal Memory field Buffer index
  for a given TDataset.Field.Index }
procedure TSqlitePassDataset.MapFieldsToMemoryFieldBuffers;
var
i, j: Integer;
begin
FLookupFieldsList.Clear;
FCalcFieldsList.Clear;
j := 0;
for i := 0 to Pred(Fields.Count) do
    begin
    Case Fields[i].FieldKind of
         fkData:      Fields[i].Tag := Pred(Fields[i].FieldNo);

         fkCalculated,
         fkLookup:    begin
                      Fields[i].Tag := FRecordset.FDataFieldsCount + j;
                      if Fields[i].FieldKind = fkCalculated
                         then FCalcFieldsList.Add(Fields[i])
                         else FLookupFieldsList.Add(Fields[i]);
                      Inc(j);
                      end;
         end; { Case }
    end;
end;

Procedure TSqlitePassDataset.InternalClearFieldDefs;
begin
{$IFDEF DEBUG_SQLITEPASS}
   FDatabase.Debugger.Display('InternalClearFieldDefs');
{$ENDIF}
{ Clear the internal items }
While FieldDefs.Count > 0
  do FieldDefs[0].Free;
{ Clear the list pointers }
FieldDefs.Clear;
end;

{procedure InternalClose; virtual; abstract;
-------------------------------------------
Closes the dataset. Any resources allocated in InternalOpen should be freed
here.}
Procedure TSqlitePassDataset.InternalClose;
Begin
 FDatasetType := dtUnknown;
 FSQLSelectStmt.Unprepare;
 FInMemoryIndexes.ClearAndFreeItems;
 FFieldFilters.ClearAndFreeItems;
 FLocateFieldFilters.ClearAndFreeItems;
 FLookupKeyFieldFilters.ClearAndFreeItems;
 FLookupFieldsKeyFieldFilters.ClearAndFreeItems;
 FLookupFieldsList.Clear; { a sublist of Fields to hold Lookupfields}
 ClearLookup;
 FCalcFieldsList.Clear;
 BindFields(False);
 if DefaultFields
    then DestroyFields;
End;


{procedure InternalGotoBookmark(ABookmark: Pointer); virtual; abstract;
----------------------------------------------------------------------
Set the record position on the position that is associated with the
ABookMark data. The ABookMark data is the data that is acquired through
the GetBookMarkData call, and should be kept for each record.}
Procedure TSqlitePassDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
 FRecordset.CurrentRecordIndex := Integer(ABookmark^);
End;

Function TSqlitePassDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
Begin
// TODO should test Pointer is not Nil and Record was not deleted
Result := (PInteger(Bookmark)^ > -1) and (PInteger(Bookmark)^ < RecordCount);
End;

{ TDataset: function GetBookmarkFlag(Buffer: PRecBuffer): TBookmarkFlag; virtual; abstract;
  --------------------------------------------------------------------------
  Returns the BookmarkFlag (bfCurrent, bfBOF, bfEOF, bfInserted)
  associated with a recordBuffer.
  -------------------------------------------------------------------------- }
Function TSqlitePassDataset.GetBookmarkFlag(Buffer: PRecBuffer): TBookmarkFlag;
begin
 Result := FRecordSet.GetBookMarkFlag(PInteger(Buffer)^);
{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('TSqlitePassDataset.GetBookmarkFlag (bfCurrent, bfBOF, bfEOF, bfInserted) : Dataset.RecordIndex = ' + IntToStr(PInteger(Buffer)^)
             + ' BookmakFlag = ' + IntToStr(Ord(Result)));
     Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
     Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
     Display('');
     end;
{$ENDIF}
End;

{TDataset: procedure SetBookmarkFlag(Buffer: PRecBuffer; Value: TBookmarkFlag); virtual; abstract;
-----------------------------------------------------------------------------
Set the BookmarkFlag 'Value' (bfCurrent, bfBOF, bfEOF, bfInserted)
of the record Buffer.
----------------------------------------------------------------------------- }
Procedure TSqlitePassDataset.SetBookmarkFlag(Buffer: PRecBuffer; Value: TBookmarkFlag);
Begin
FRecordSet.SetBookMarkFlag(PInteger(Buffer)^,Value);
{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('TSqlitePassDataset.SetBookmarkFlag (bfCurrent, bfBOF, bfEOF, bfInserted) : Dataset.RecordIndex = ' + IntToStr(PInteger(Buffer)^)
             + ' BookmakFlag = ' + IntToStr(Ord(Value)));
     Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
     Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
     Display('');
     end;
{$ENDIF}
End;

{procedure GetBookmarkData(Buffer: PRecBuffer; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------
Puts the bookmarkdata for Buffer into the area pointed to by Data.}

{$IFNDEF Delphi2009}

Procedure TSqlitePassDataset.GetBookmarkData(Buffer: PRecBuffer; Data: Pointer);
Begin
PCardinal(Data)^ := FRecordset.GetBookMarkData(PInteger(Buffer)^);
{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('TSqlitePassDataset.GetBookmarkData : Record = ' + IntToStr(PInteger(Buffer)^)
             + ' BookmakData = ' + IntToStr(PCardinal(Data)^));
     Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
     Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
     Display('');
     end;
{$ENDIF}
End;

{procedure SetBookmarkData(Buffer: PRecBuffer; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------
Move the 'Data' value to the BookmarkData associated with Buffer}
Procedure TSqlitePassDataset.SetBookmarkData(Buffer: PRecBuffer; Data: Pointer);
Begin
FRecordSet.SetBookMarkData(PInteger(Buffer)^,PCardinal(Data)^);
{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('TSqlitePassDataset.SetBookmarkData : Record = ' + IntToStr(PInteger(Buffer)^)
             + ' BookmakData = ' + IntToStr(PCardinal(Data)^));
     Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
     Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
     Display('');
     end;
{$ENDIF}
End;

{$ENDIF Dellphi2009}

Function TSqlitePassDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  ResultCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  { Tests for nil BookMarks First }
  Result := ResultCodes[Bookmark1 = nil, Bookmark2 = nil];
  If Result = 2 then
     begin
     Result := 0;
     if PInteger(Bookmark1)^ < PInteger(Bookmark2)^
        then Result := -1
        else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^
             then Result :=  1;
     End;
end;

{ ----- RecNo ----- }

function TSqlitePassDataset.GetRecordCount: Longint;
begin
   CheckActive;
   Result := FRecordset.RecordsCount;
end;

function TSqlitePassDataset.GetRecNo: Integer;
begin
  Result := GetActiveRecord;
end;

procedure TSqlitePassDataset.SetRecNo(Value: Integer);
begin
Dec(Value);
if Value <> GetActiveRecord then
   begin
   CheckBrowseMode;
   DoBeforeScroll;
   FRecordset.CurrentRecordIndex := Value;
   Resync([rmCenter]);
   DoAfterScroll;
   end;
{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('TSqlitePassDataset.SetRecNo : RecordNo = ' + IntToStr(Value));
     Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
     Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
     Display('');
     end;
{$ENDIF}
end;

Procedure TSqlitePassDataset.InternalInitIndexDefs;
Begin
{$IFDEF DEBUG_SQLITEPASS}
   FDatabase.Debugger.Display('InternalInitIndexDefs');
{$ENDIF}
 UpdateIndexDefs;
End;

Procedure TSqlitePassDataset.UpdateIndexDefs;
Begin
 FIndexDefs.RefreshFromCache;
End;

{procedure InternalInitFieldDefs; virtual; abstract;
-----------------------------------------------------------------------
This method is called from InternalOpen, and
initialize FieldDef definitions for all fields in a record.
It adds these definitions to the FFielddefs object.
----------------------------------------------------------------------}
Procedure TSqlitePassDataset.InternalInitFieldDefs;
var
i, CheckColumnCount, ColumnCount: Integer;
StmtFieldDef: TSqlitePassSelectStmtFieldDef;
NewFieldDef: TFieldDef;
WasActive: Boolean;

begin
{$IFDEF DEBUG_SQLITEPASS}
   FDatabase.Debugger.Display('InternalInitFieldDefs');
{$ENDIF}

try
CheckCanOpen(vlSilent);
WasActive := Active;

if FDatasetType = dtUnknown
   then SetFDatasetType;

InternalClearFieldDefs;

{ First, we need to process the SQL Statement }
Include(FSQLChanges, scSelect);
UpdateFSQLStmt;

ColumnCount := SqliteDbv3_column_count(SqlSelectStmt.StmtHandle) - SqlSelectStmt.FPrimaryKeyCount;
CheckColumnCount := 0;

{ We build the TFieldDefs list from the SqlSelectStmt.FieldDefs list }
For i := 0 to Pred(SqlSelectStmt.FieldDefs.Count) do
    begin
    StmtFieldDef := SqlSelectStmt.FieldDefs[i];
    NewFieldDef := TFieldDef.Create(FieldDefs, GetUniqueFieldName(StmtFieldDef),StmtFieldDef.DataType,
                                    StmtFieldDef.Size, False {= Not Required}, i+1);

    { Then we fill the following properties }
    With NewFieldDef do
      begin
      Precision := StmtFieldDef.Precision;

    { AutoInc Fields are not always ReadOnly so we don't set them as ReadOnly
      if StmtFieldDef.FAutoInc
         then Attributes := Attributes + [faReadOnly]; }

     { This will also set TFieldDef.Attributes +/- [faRequired] }
      Required := StmtFieldDef.NotNull and Not (faHiddenCol in Attributes)
                  And Not (faReadOnly in Attributes) and Not StmtFieldDef.PrimaryKey;

      Inc(CheckColumnCount);
      end; { With }
     end;{ For i }

{ Raises an Exception if Sqlite Engine doesn't return the same ColumCount
  as parsed SQL Statement ...}
If ColumnCount <> CheckColumnCount
   then FDatabase.DatabaseError.RaiseException(Msg2025, -1, Self);

InternalInitIndexDefs;

if not WasActive
   then close;
except
Raise;
end;
end;

{procedure InternalHandleException; virtual; abstract;
-----------------------------------------------------
not needed yet. Just implement an empty call.}
Procedure TSqlitePassDataset.InternalHandleException;
begin
 Application.HandleException(Self);
end;


Function TSqlitePassDataset.GetActiveRecord: Integer;
begin
Result := -1;
Case State of
     dsBrowse      : if FRecordset.FRecordsCount > 0
                        then Result := PInteger(ActiveBuffer)^;
     dsBlockRead,
     dsNewValue,
     dsCurValue,
     dsEdit,
     dsInsert      : Result := PInteger(ActiveBuffer)^;

     dsCalcFields  : Result := FRecordset.CurrentRecordIndex;

     dsOldValue    :;// Result := FRecordset.OldValueRecordIndex;
     dsFilter      : Result := FRecordset.CurrentRecordIndex;
     end;
end;


{function GetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual; abstract;
----------------------------------------------------------------------------------
Puts the data for field 'Field' from the active buffer into Buffer.
This is called whenever a field value is demanded by a TField, so it must be
efficient.
the TDataset class doesn’t know anything about how our record buffer is organized.
The next question then becomes that if the TDataset class doesn’t know about our
record structure, how it does it pull out field values from this structure?
How it does it put them back in when the user is editing a record?
The answer is that it uses the GetFieldData and SetFieldData methods which a
custom dataset class must override.
These methods are called by the Tdataset.fields when it needs to retrieve
or set a specific field value from the current record buffer.
GetFieldData is prototyped as follows:

function TDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;

The Field parameter is the field for which the value needs to be retrieved
We send back the needed value in the buffer.
Returns False if a null value was retrieved}
Function TSqlitePassDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
//StrSize: Integer;
FieldPos, ActiveRecIndex: Integer;
StrValue: AnsiString;
WideStrValue: UTF16WideString;
FieldValueBuffer: PRecBuffer;
begin
Result := False;
ActiveRecIndex := GetActiveRecord;
FieldPos := Field.Tag;

{ When buffer is nil, the TDataset or TField.GetIsNull verifies if the current field IsNull }
if (ActiveRecIndex > -1) then
   begin
   FieldValueBuffer := FRecordset.GetFieldValueBuffer(ActiveRecIndex, FieldPos);
   Result :=  not FRecordset.FieldIsNull(FieldValueBuffer);
   end
   else FieldValueBuffer := nil;

if Result and Assigned(Buffer) then
   begin
   Case Field.DataType of
        ftBoolean    : Move(FieldValueBuffer^, Buffer^, SizeOf(WordBool));
        ftWord,
        ftSmallint,
        ftInteger    : Move(FieldValueBuffer^, Buffer^, SizeOf(Integer));

        ftAutoInc,
        ftLargeInt   : Move(FieldValueBuffer^, Buffer^, SizeOf(Int64));

        ftBCD        : {$IFDEF HasFmtBcd}
                       CurrToBCD(PCurrency(FieldValueBuffer)^,  TBCD(Buffer^));
                       {$ELSE}
                       Move(FieldValueBuffer^, Buffer^, SizeOf(Currency));
                       {$ENDIF}
        ftFloat,
        ftCurrency   : Move(FieldValueBuffer^, Buffer^, SizeOf(Double));
        ftDate       : PDateTimeRec(Buffer)^.Date := PInteger(FieldValueBuffer)^;
        ftTime       : PDateTimeRec(Buffer)^.Time := PInteger(FieldValueBuffer)^;
        ftDateTime   : PDateTimeRec(Buffer)^.DateTime := PDouble(FieldValueBuffer)^;

        ftString, ftFixedChar
                     : begin
                       StrValue := FRecordset.GetFieldAsAnsiString(FieldValueBuffer);
                       StrCopy(Buffer, PAnsiChar(StrValue));
                       // TODO - Limit the string size to Field.Datasize
                       // UTF8 strings Length is not = to Size...
                       (*
                       StrSize := Min(Length(StrValue)+1,Field.DataSize);
                       StrLCopy(Buffer, PAnsiChar(StrValue), Field.DataSize);
                       *)
                       end;

        ftWideString:  begin
                       WideStrValue := FRecordset.GetFieldAsWideString(FieldValueBuffer);
                       SqlitePassUtils.WStrCopy(Buffer, PWideChar(WideStrValue));
                       end;
        end; { Case }
  end; { if Result and Assigned(Buffer) }

{$IFDEF DEBUG_SQLITEPASS}
With FDatabase.Debugger do
     begin
     Display('');
     Display('[ TSqlitePassDataset.GetFieldData ]');
     Display('  Dataset.Name = ' + DatasetName);
     Display('  Dataset.ActiveRecordIndex = ' + IntToStr(ActiveRecIndex));
     Display('  FieldName = ' + Field.DisplayName);
     Display('  FieldPos  = ' + IntToStr(FieldPos));

     If Result
        then begin
        Case Field.DataType of
             ftInteger: Display('  FieldValue  = ' + IntToStr(Integer(FRecordset.GetFieldValueBuffer(ActiveRecIndex, FieldPos)^)));
             ftString: Display ('  FieldValue  = ' + StrValue);
             end;
        end
        else begin
             Display('FieldValue = FAILS ');
             if (ActiveRecIndex > -1) then
                if FRecordset.FieldIsNull(ActiveRecIndex, FieldPos)
                   then Display('FieldIsNullValue = TRUE')
                   else Display('FieldIsNullValue = FALSE')
             end;
     Display('');
     end;
{$ENDIF}
End;

{procedure SetFieldData(Field: TField; Buffer: Pointer); virtual; abstract;
--------------------------------------------------------------------------
Move the data associated with Field from Buffer to the activebuffer.}
Procedure TSqlitePassDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
FieldPos, ActiveRecIndex: Integer;
FieldValueBuffer: PRecBuffer;

begin
if not (State in dsWriteModes)
   then FDatabase.DatabaseError.RaiseException(SNotEditing, -1, Self);

if Field.ReadOnly
   then DatabaseErrorFmt(Msg2032, [Field.FieldName]);

ActiveRecIndex := GetActiveRecord;

if (ActiveRecIndex > -1) then
   begin
   FieldPos := Field.Tag;
   FieldValueBuffer := FRecordset.GetFieldValueBuffer(ActiveRecIndex, FieldPos);
   if Assigned(Buffer) then
      begin
      Field.Validate(Buffer);
      case Field.DataType of
           ftBoolean    : Move(Buffer^, FieldValueBuffer^, SizeOf(WordBool));
           ftWord,
           ftSmallint,
           ftInteger    : Move(Buffer^, FieldValueBuffer^, SizeOf(Integer));
           ftAutoInc,
           ftLargeInt   : Move(Buffer^, FieldValueBuffer^, SizeOf(Int64));
           ftBCD        : {$IFDEF HasFmtBcd}
                          BCDToCurr(TBCD(Buffer^), PCurrency(FieldValueBuffer)^);
                          {$ELSE}
                          Move(Buffer^, FieldValueBuffer^, SizeOf(Currency));
                          {$ENDIF}
           ftFloat,
           ftCurrency   : Move(Buffer^, FieldValueBuffer^, SizeOf(Double));
           ftDate       : PInteger(FieldValueBuffer)^ := PDateTimeRec(Buffer)^.Date;
           ftTime       : PInteger(FieldValueBuffer)^ := PDateTimeRec(Buffer)^.Time;
           ftDateTime   : PDouble(FieldValueBuffer)^ := PDateTimeRec(Buffer)^.DateTime;
           ftString, ftFixedChar
                        : FRecordset.SetFieldAsAnsiString(FieldValueBuffer, pAnsiChar(Buffer));

           ftWideString
                        : FRecordset.SetFieldAsWideString(FieldValueBuffer, pWideChar(Buffer));
           end; { Case }
       FRecordset.SetFieldNullValue(FieldValueBuffer, False);
       end { Assigned Buffer }
      { We have a null value }
       else begin
            case Field.DataType of
               ftString, ftFixedChar: FRecordset.ClearAnsiString(FieldValueBuffer);
               ftWideString : FRecordset.ClearWideString(FieldValueBuffer);
               else FRecordset.SetFieldNullValue(FieldValueBuffer, True);
               end;
             end;
   { Field modified flag is always tested in TSqlitePassSelectStmtTableDef.PostUpdate
     even when FWriteMode = wmPostPoned.
     Inserted flag is directly set by TSqlitePassRecordset.PostRecord}
   FRecordset.SetFieldWasModifiedValue(FieldValueBuffer, True);
   FRecordset.SetRecordState(ActiveRecIndex, rsModifiedFlag, rsModified);
  { The DataEvent is used to set TDataset.SetModified to True.
    Then TDataset.CheckBrowseMode will be able to Post the record if necessary
    when scrolling the dataset }
   if not (State in [dsInternalCalc, dsCalcFields, dsFilter, dsNewValue])
      then DataEvent(deFieldChange, Longint(Field));

   end; { if (ActiveRecIndex > -1) and Assigned(Buffer) }

 {$IFDEF DEBUG_SQLITEPASS}
 FDatabase.Debugger.Display(pChar('TSqlitePassDataset.SetFieldData : '
                                + ' Dataset = ' + DatasetName
                                + ' ActiveRecordIndex = ' + IntToStr(ActiveRecIndex)
                                + ' Field = ' + Field.DisplayName
                                + ' FieldPos  = ' + IntToStr(FieldPos)));
 {$ENDIF}
end;

{procedure InternalFirst; virtual; abstract;
-------------------------------------------
This is called when 'TDataset.First' is called;
After this method, TDataset calls 'Getrecord' asking for the next record.
'Next' will inc(CurrentRecordIndex). Finally 'GetRecord' will retrieve
Record[FMemRecordset.CurrentRecordIndex] i.e. Record[0]
or return grBOF if we are already at BOF}
Procedure TSqlitePassDataset.InternalFirst;
Begin
FRecordset.First;
End;

{procedure InternalLast; virtual; abstract;
------------------------------------------
This is called when 'TDataset.Last' is called;
After this method, TDataset calls 'Getrecord' asking for the prior record.
'Prior' will Dec(CurrentRecordIndex). Finally 'GetRecord' will retrieve
Record[FMemRecordset.CurrentRecordIndex] i.e. Record[Pred(RecordCount)]
or return grEOF if we are already at EOF}
Procedure TSqlitePassDataset.InternalLast;
Begin
FRecordset.Last;
End;

{procedure InternalSetToRecord(Buffer: PRecBuffer); virtual; abstract;
----------------------------------------------------------------
Set the current record to the record in Buffer; if bookmark data
is specified in this buffer, that data can be used to determine which
record this should be.}
Procedure TSqlitePassDataset.InternalSetToRecord(Buffer: PRecBuffer);
Begin
FRecordset.CurrentRecordIndex := PInteger(Buffer)^;
End;

procedure TSqlitePassDataset.InternalInsert;
begin
inherited InternalInsert;
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalInsert begin');
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalInsert End');
FDatabase.Debugger.Display('');
{$ENDIF}
end;

Procedure TSqlitePassDataset.InternalEdit;
Begin
inherited InternalEdit;
FRecordset.SaveActiveRecordOldValues;
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalEdit begin');
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalEdit end');
FDatabase.Debugger.Display('');
{$ENDIF}
End;

{procedure InternalDelete; virtual; abstract;
--------------------------------------------
Deletes the current Record.}
Procedure TSqlitePassDataset.InternalDelete;
var
RecordBuffer: PRecBuffer;

begin
try
  RecordBuffer := FRecordset.FRecords[PInteger(ActiveBuffer)^];
  if FWriteMode = wmDirect
     then FSQLSelectStmt.Post(ptDelete, RecordBuffer);

  FRecordset.DeleteRecord(PInteger(ActiveBuffer)^);
 { The index list changed and need to be rebuild, so we reset it. }
  FInMemoryIndexes.ClearIndexes;
except
  FDatabase.DatabaseError.RaiseException(Msg2024);
end;
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalDelete begin');
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalDelete end');
FDatabase.Debugger.Display('');
{$ENDIF}
end;

Procedure TSqlitePassDataset.InternalCancel;
Begin
inherited InternalCancel;
Case State of
     dsEdit:   begin
               FRecordset.RestoreActiveRecordOldValues;
               FRecordset.ClearActiveRecordOldValues;
               end;

     dsInsert: FRecordset.Cancel;
     end;

{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalCancel begin');
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalCancel end');
FDatabase.Debugger.Display('');
{$ENDIF}
End;

{procedure InternalPost; virtual; abstract;
------------------------------------------
Post the data in the active buffer to the underlying dataset. }
Procedure TSqlitePassDataset.InternalPost;
var
RecordBuffer: PRecBuffer;
begin
try
RecordBuffer := FRecordset.FRecords[PInteger(ActiveBuffer)^];
Case State of
     dsEdit   : begin
                if FWriteMode = wmDirect then
                   begin
                   FSQLSelectStmt.Post(ptUpdate, RecordBuffer);
                   FRecordset.ResetRecordState(RecordBuffer);
                   end;
                FRecordset.ClearActiveRecordOldValues;
                end;
     dsInsert : begin
                if FWriteMode = wmDirect then
                   begin
                   FSQLSelectStmt.Post(ptInsert, RecordBuffer);
                   FRecordset.ResetRecordState(RecordBuffer);
                   end;
                FRecordset.PostRecord;
                end;
     end;
{ The index list changed and need to be rebuild, so we reset it. }
FInMemoryIndexes.ClearIndexes;
FillLookupFields(PInteger(ActiveBuffer)^, PInteger(ActiveBuffer)^);
Except
FDatabase.DatabaseError.RaiseException(Msg2022);
end;
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalPost begin');
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalPost end');
FDatabase.Debugger.Display('');
{$ENDIF}
end;


{procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); virtual; abstract;
---------------------------------------------------------------------------------
Adds a record to the dataset. The record's data is in Buffer and Append
indicates whether the record should be appended (True) or Inserted (False).
Note that for SQL based datasets, this has no meaning.}
Procedure TSqlitePassDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
Begin
{ Used when 'enduser' wants to add an entire record to the dataset }
// TODO Check (TDataSet.AppenRecord/TDataSet.InsertRecord/TDataSet.AddRecord(const Values: array of const; Append: Boolean);
End;


procedure TSqlitePassDataset.GetFieldList(List: TList; const FieldNames: string);
var
Tokenizer: TSqlitePassSQLTokenizer;
begin
Tokenizer := TSqlitePassSQLTokenizer.Create;
 Try
 Tokenizer.Text := FieldNames;
 Tokenizer.Clean([ttSingleQuote, ttGraveQuote, ttDoubleQuote, ttSquareOpen, ttSquareClose, ttHash]);
 Inherited GetFieldList(List, Tokenizer.Text);
 finally
 Tokenizer.Free;
 end;
end;

{function IsCursorOpen: Boolean; virtual; abstract;
--------------------------------------------------
This function should return True if data is available, even if the dataset
is not active.}
Function TSqlitePassDataset.IsCursorOpen: Boolean;
Begin
  Result := FRecordset.Active and (aoRead in FSQLSelectStmt.AvailableOperations);
End;

{function AllocRecordBuffer: PRecBuffer; virtual; abstract;
-----------------------------------------------------
Must allocate enough memory to store a complete record in the dataset.
Optionally, this buffer must contain enough memory to store bookmarkdata.
The descendent must be able to construct a bookmark from this buffer.}
Function TSqlitePassDataset.AllocRecordBuffer: PRecBuffer;
Begin
{ test added to avoid memory leak
  when destroying component a [uncessary?] call is sent from TDataSet.SetBufListSize...}
if Not (csDestroying in ComponentState)
   then GetMem(Result, GetRecordSize)
   else Result := nil;
End;


{procedure FreeRecordBuffer(var Buffer: PRecBuffer); virtual; abstract;
-----------------------------------------------------------------
Must free the memory allocated in the AllocRecordBuffer call.}
Procedure TSqlitePassDataset.FreeRecordBuffer(var Buffer: PRecBuffer);
Begin
FreeMem(Buffer, GetRecordSize);
End;


{procedure InternalInitRecord(Buffer: PRecBuffer); virtual; abstract;
---------------------------------------------------------------
This method is called to initialize a new field buffer when the dataset
is put into insert or append mode. We ask the InternalRecorset to add a new record }
Procedure TSqlitePassDataset.InternalInitRecord(Buffer: PRecBuffer);
begin
PInteger(Buffer)^ := FRecordset.AddTempRecord;
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('');
FDatabase.Debugger.Display('InternalInitRecord begin');
FDatabase.Debugger.Display('  New record index = ' + IntToStr(PInteger(Buffer)^));
FDatabase.Debugger.Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
FDatabase.Debugger.Display('  Recordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
FDatabase.Debugger.Display('InternalInitRecord end');
FDatabase.Debugger.Display('');
{$ENDIF}
End;


Function TSqlitePassDataset.GetCanModify: Boolean;
Begin
 Result := Active and (DatasetType in [dtTable, dtQuery, dtSQLSelect, dtSQLDirect]) and (not ReadOnly);
End;

{function GetRecordSize: Word; virtual; abstract;
------------------------------------------------
Since we just use pointers to keep track of TSqlitePassMemRecords, the dataset just needs
to allocate enough size to hold a pointer value (i.e. longint or integer}
Function TSqlitePassDataset.GetRecordSize: Word;
begin
  Result := SizeOf(Pointer);
End;


{function GetRecord(Buffer: PRecBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual; abstract;
-----------------------------------------------------------------------------------
This method must do 3 things:
1) Get the record data for the next/current/previous record, depending
   on the GetMode value. It should return
    grOK    if all was OK.
    grBOF   if the previous record was requested, and we are at the start.
    grEOF   if the next record was requested, and we are at the end.
    grError if an error occurred.

2) If DoCheck is True, and the result is grError, then an exception must be
    raised.

3) It should initialize bookmark data for this record with flag 'bfCurrent'
   This data can be stored in the buffer, if space was allocated for it with
   AllocRecordBuffer.
}
Function TSqlitePassDataset.GetRecord(Buffer: PRecBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
Begin

case GetMode of
  gmCurrent: FRecordset.MoveBy(0);
  gmNext:    FRecordset.Next;
  gmPrior:   FRecordset.Prior;
  end;

Result := FRecordset.MoveState;

Case Result of
    grOk   : begin
             PInteger(Buffer)^ := FRecordSet.FCurrentRecordIndex;
             if FLookupFieldsDisplayedRecordsOnly
                then FillLookupFields(FRecordSet.FCurrentRecordIndex, FRecordSet.FCurrentRecordIndex);
             if FCalcDisplayedRecordsOnly and Active
                then GetCalcFields(Buffer);
             end;
    grError: If DoCheck then FDatabase.DatabaseError.RaiseException(FDatasetName + ' : ' + Msg1040);
    end;

{$IFDEF DEBUG_SQLITEPASS}
 With FDatabase.Debugger do
      begin
      Display('');
      Display('TSqlitePassDataset.GetRecord begin');
      Display('  Getmode (gmCurrent, gmNext, gmPrior) = ' + IntToStr(Ord(GetMode)));
      Display('  Dataset.state (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc) = ' + IntToStr(Ord(Self.State)));
      Display('  Dataset.Movestate (grOK, grBOF, grEOF, grError)= ' + IntToStr(Ord(Result)));
      Display('  Dataset.ActiveRecord = ' + IntToSTr(GetActiveRecord));
      Display('  FRecordset.CurrentRecordIndex = ' + IntToStr(FRecordset.CurrentRecordIndex));
      Display('TSqlitePassDataset.GetRecord end');
      Display('');
      end;
{$ENDIF}
end;


{ --- Indexing records in memory --- }

{ TSqlitePassInMemoryIndex }

constructor TSqlitePassInMemoryIndex.Create(Indexes: TSqlitePassInMemoryIndexes; Field: TField);
begin
 Inherited Create;
 FIndexes := Indexes;
 FField:= Field;
 FIndexedRecords := TList.Create;
 SetLength(FIndexInfos, 1);
end;

destructor TSqlitePassInMemoryIndex.Destroy;
begin
 SetLength(FIndexInfos, 0);
 FIndexedRecords.Free;
 FField := nil;
 FIndexes := nil;
 Inherited Destroy;
end;

procedure TSqlitePassInMemoryIndex.SetFIndexed(Const Value: Boolean);
begin
if Value <> FIndexed then
   begin
   FIndexed := Value;
   if FIndexes.FDataset.Active then
      begin
      if Value
         then BuildIndex
         else FIndexedRecords.Clear;
      end;
   end;
end;


procedure TSqlitePassInMemoryIndex.BuildIndex;
begin
{ We build the index if it is not ready }
if Not IsReady then
   begin
   With FIndexes.FDataset do
        begin
        GetSortedFields(FField.FieldName, FIndexInfos);
        FRecordset.BuildIndexFromRecordsList(Self, True);
        FRecordset.SortRecords(FIndexInfos, PRecBufferList(FIndexedRecords.List));
        end;
   end;
end;

procedure TSqlitePassInMemoryIndex.ClearIndex;
begin
 FIndexedRecords.Clear;
end;

function TSqlitePassInMemoryIndex.IsReady: Boolean;
begin
 Result:= (FIndexedRecords.Count > 0)
          and (FIndexedRecords.Count = FIndexes.FDataset.FRecordset.FRecordsCount);
end;


{ TSqlitePassInMemoryIndexes }

constructor TSqlitePassInMemoryIndexes.Create(Dataset: TSqlitePassDataset);
begin
 Inherited Create;
 FDataset := Dataset;
end;

destructor TSqlitePassInMemoryIndexes.Destroy;
begin
 ClearAndFreeItems;
 FDataset := nil;
 Inherited Destroy;
end;

procedure TSqlitePassInMemoryIndexes.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

{ Create a InMemoryIndex for every Fields including Calc and Lookup Fields }
procedure TSqlitePassInMemoryIndexes.InitIndexes;
var
i: Integer;
Index: TSqlitePassInMemoryIndex;

begin
Clear;
for i := 0 to Pred(FDataset.Fields.Count) do
  begin
  Index := TSqlitePassInMemoryIndex.Create(Self, FDataset.Fields[i]);
  Add(Index);
  end;

{ mark the selected indexes }
if FDataset.FInMemoryIndexed
   then SetSelectedIndexes;
end;

procedure TSqlitePassInMemoryIndexes.SetSelectedIndexes;
var
i: Integer;
Index: TSqlitePassInMemoryIndex;
IndexedFields: TList;

begin
IndexedFields := TList.Create;
 try
 FDataset.GetFieldList(IndexedFields, FDataset.FInMemoryIndexedBy);
 for i := 0 to Pred(IndexedFields.Count) do
     begin
     Index := IndexByField(IndexedFields[i]);
     Index.Indexed := True;
     end;
 finally
 IndexedFields.Free;
 end;
end;

procedure TSqlitePassInMemoryIndexes.BuildIndexes;
var
i: Integer;
begin
{ Build the Indexes }
for i := 0 to Pred(Count) do
    if Items[i].Indexed
       then Items[i].BuildIndex;
end;

procedure TSqlitePassInMemoryIndexes.ClearIndexes;
var
i: Integer;
begin
{ Clears the Indexes }
for i := 0 to Pred(Count)
    do Items[i].ClearIndex;
end;

function TSqlitePassInMemoryIndexes.GetItem(Index: Integer): TSqlitePassInMemoryIndex;
begin
  Result:=TSqlitePassInMemoryIndex(Inherited Items[Index]);
end;

procedure TSqlitePassInMemoryIndexes.SetItem(Index: Integer;
  const Value: TSqlitePassInMemoryIndex);
begin
  Put(Index,Value);
end;

function TSqlitePassInMemoryIndexes.IndexByField(const Value: TField): TSqlitePassInMemoryIndex;
begin
 Result := Items[Value.Tag];
end;

function TSqlitePassInMemoryIndexes.IndexByFieldName(const Value: String): TSqlitePassInMemoryIndex;
var
Field: TField;
begin
  Result := nil;
  Field := FDataset.Fields.FindField(Value);
  If Field <> Nil
     then Result := Items[Field.Tag];//TODO Remove this in TSqlitePassList descendants : TSqlitePassInMemoryIndex(inherited Items[Field.Tag]);
  if Result = nil
     then DatabaseErrorFmt(Msg2100, [Value], nil); //FDatabase
end;

function TSqlitePassInMemoryIndexes.GetIndexedFilter(Filters: TSqlitePassFieldFilters): TSqlitePassFieldFilter;
type
TFieldTypes = Set of TFieldType;
var
TempIndex: TSqlitePassInMemoryIndex;
TempFilter: TSqlitePassFieldFilter;

    function GetBestIndex(IndexFieldType: TFieldTypes): TSqlitePassFieldFilter;
    var
    i: Integer;
    begin
    Result := nil;
    for i := 0 to Pred(Filters.Count) do
        begin
        TempIndex := IndexByField(Filters[i].FField);
        if (TempIndex.FField.DataType in IndexFieldType)
           and (TempIndex.Indexed or TempIndex.IsReady) then
           begin
           Result := Filters[i];
           break;
           end;
        end;
    end;

begin
Result := nil;
If FDataset.Indexed then
   begin
   TempFilter := GetBestIndex([ftInteger, ftSmallInt, ftWord, ftDate, ftTime]);
   if Not Assigned(TempFilter)
      then TempFilter := GetBestIndex([ftAutoInc, ftLargeint]);
   if Not Assigned(TempFilter)
      then TempFilter := GetBestIndex([ftDateTime, ftFloat, ftBCD, ftCurrency]);
   if Not Assigned(TempFilter)
      then TempFilter := GetBestIndex([ftString, ftFixedChar]);
   if Not Assigned(TempFilter)
      then TempFilter := GetBestIndex([ftWideString]);

   if Assigned(TempFilter)
      and (TempFilter.Count = 1)
      and (TempFilter.Items[0].FComparisonOperator in [CmpEqual, CmpGreater, CmpGreateOrEqual, CmpLesserOrEqual, CmpLesser])
          then begin
               TempIndex.BuildIndex;
               Result := TempFilter;
               end;
   end;
end;


{ ======= FILTERING ======== }

{ Filtering with Master/Detail }

function TSqlitePassDataset.GetMasterDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TSqlitePassDataset.SetMasterDataSource(Value: TDataSource);
begin
  if Value = nil
     then begin
          FMasterLink.DataSource := Value;
          MasterFields := '';
          end
     else begin
          // TODO  if not (Value.Dataset is TSqlitePassDataset) then Raise Error
          if IsLinkedTo(Value)
             or (Assigned(TSqlitePassDataset(Value.Dataset).MasterSource)
                 and (TSqlitePassDataset(Value.Dataset).MasterSource.Dataset = Self))
                 then FDatabase.DatabaseError.RaiseException(Msg2026, -1, Self)
                 else FMasterLink.DataSource := Value;
          end;
end;

function TSqlitePassDataset.GetMasterFields: String;
begin
  Result := FMasterFields;
end;

procedure TSqlitePassDataset.SetMasterFields(const Value: String);
begin
FMasterFields := Value;

{ We parse MasterFields/DetailFields only when dataset.fields are defined }
if not (csLoading in ComponentState) and Active
   then ProcessMasterDetailFields;
end;

procedure TSqlitePassDataset.ProcessMasterDetailFields;
var
JoinedFieldEnd, PosEqual: Integer;
ParsedValue, DetailFieldName, CurrentJoinedFieldsNames, MasterFields: String;
begin
FDetailFields.Clear;
ParsedValue := '';
if FMasterFields <> '' then
   begin
   MasterFields := FMasterFields;
     Repeat
     JoinedFieldEnd := AnsiPos(';', MasterFields);

     If JoinedFieldEnd = 0
        then JoinedFieldEnd := Length(MasterFields)+1;

     If JoinedFieldEnd > 0 then
        begin
        CurrentJoinedFieldsNames := System.Copy(MasterFields, 1, JoinedFieldEnd-1);
        System.Delete(MasterFields, 1, JoinedFieldEnd);
        PosEqual := AnsiPos('=', CurrentJoinedFieldsNames);
        If PosEqual = 0
           then PosEqual := Length(CurrentJoinedFieldsNames)+1;
        If PosEqual > 0 then
           begin
           DetailFieldName := System.Copy(CurrentJoinedFieldsNames, PosEqual+1, JoinedFieldEnd - PosEqual);
           FDetailFields.Add(Fields.FindField(DetailFieldName));
           System.Delete(CurrentJoinedFieldsNames, PosEqual, JoinedFieldEnd - PosEqual);
           ParsedValue := ParsedValue + CurrentJoinedFieldsNames + ';';
           end;
        end;
      Until MasterFields = '';
    if ParsedValue[Length(ParsedValue)] = ';'
       then System.Delete(ParsedValue, Length(ParsedValue), 1);
    end;

FMasterLink.FieldNames := ParsedValue;
Include(FSQLChanges, scWhere);
RefreshActive;

//TODO : Check GetDetailLinkFields
end;

procedure TSqlitePassDataset.MasterChanged(Sender: TObject);
begin
 Include(FSQLChanges, scWhere);
 RefreshActive;
end;

procedure TSqlitePassDataset.MasterDisabled(Sender: TObject);
begin
 Include(FSQLChanges, scWhere);
 RefreshActive;
end;

procedure TSqlitePassDataset.DoOnNewRecord;
var
  i: Integer;
begin
{ Set default detail fields values from master fields if needed }
if MasterFields <> '' then
   begin
   For i := 0 to Pred(FDetailFields.Count)
       do TField(FDetailFields[i]).Value := TField(FMasterLink.Fields[i]).Value;
   end;

inherited DoOnNewRecord;
end;


{ TSqlitePassFieldFilterExpression }

constructor TSqlitePassFieldFilterExp.Create(Owner: TSqlitePassFieldFilter);
begin
 Inherited Create;
 FValue := nil;
 FValueMem := nil;
 FFieldFilter := Owner;
 AcceptValue := nil;
end;

destructor TSqlitePassFieldFilterExp.Destroy;
begin
 If Assigned(FValueMem)
    then FreeMem(FValueMem);
 FValueMem := nil;
 FValue := nil;
 FFieldFilter := nil;
 AcceptValue := nil;
 Inherited Destroy;
end;

procedure TSqlitePassFieldFilterExp.SetFComparisonOperator(
  Value: TSqlitePassFilterComparisonOperators);


  function TextComparator: Integer;
  var
  StarFirstPos, StarLastPos: Integer;
  Text: string;
  begin
  Text := FTextValue;
  Text := StringReplace(Text, '%', '*', [rfReplaceAll]);
  StarFirstPos := AnsiPos('*', Text);
  StarLastPos  := SqlitePassUtils.RPos('*', Text, Length(Text));

  Result := 2;
  if (StarFirstPos <> StarLastPos) and (StarFirstPos > 0)
     then Result := 3
     else Result := StarFirstPos;
  end;

  begin
  FComparisonOperator := Value;

  { Default behavior }
  AcceptValue := @AcceptDummyValue;

  { Binary Search Default behavior }
  IsEqual   := @AcceptDummyValue;
  IsLesser  := @AcceptDummyValue;

  Case FComparisonOperator of
       CmpNull         : AcceptValue := @AcceptValueCmpNull;
       CmpNotNull      : AcceptValue := @AcceptValueCmpNotNull;
       else begin
            Case FFieldFilter.FField.DataType of

            ftInteger, ftSmallInt, ftWord, ftDate, ftTime:
               begin
               Case FComparisonOperator of
                    CmpEqual        : AcceptValue := @AcceptIntegerValueCmpEqual;
                    CmpNotEqual     : AcceptValue := @AcceptIntegerValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptIntegerValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptIntegerValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptIntegerValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptIntegerValueCmpLesser;
                    end;
               IsEqual    := @AcceptIntegerValueCmpEqual;
               IsLesser   := @AcceptIntegerValueCmpLesser;
               end;

            ftAutoInc, ftLargeint:
               begin
               Case FComparisonOperator of
                    CmpEqual        : AcceptValue := @AcceptInt64ValueCmpEqual;
                    CmpNotEqual     : AcceptValue := @AcceptInt64ValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptInt64ValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptInt64ValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptInt64ValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptInt64ValueCmpLesser;
                    end;
               IsEqual    := @AcceptInt64ValueCmpEqual;
               IsLesser   := @AcceptInt64ValueCmpLesser;
               end;

            ftFloat, ftDateTime, ftCurrency:
               begin
               Case FComparisonOperator of
                    CmpEqual        : AcceptValue := @AcceptDoubleValueCmpEqual;
                    CmpNotEqual     : AcceptValue := @AcceptDoubleValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptDoubleValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptDoubleValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptDoubleValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptDoubleValueCmpLesser;
                    end;
               IsEqual    := @AcceptDoubleValueCmpEqual;
               IsLesser   := @AcceptDoubleValueCmpLesser;
               end;

            ftBCD:
               begin
               Case FComparisonOperator of
                    CmpEqual        : AcceptValue := @AcceptCurrencyValueCmpEqual;
                    CmpNotEqual     : AcceptValue := @AcceptCurrencyValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptCurrencyValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptCurrencyValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptCurrencyValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptCurrencyValueCmpLesser;
                    end;
               IsEqual    := @AcceptCurrencyValueCmpEqual;
               IsLesser   := @AcceptCurrencyValueCmpLesser;
               end;

            ftBoolean:
               Case FComparisonOperator of
                    CmpEqual        : AcceptValue := @AcceptWordBoolValueCmpEqual;
                    CmpNotEqual     : AcceptValue := @AcceptWordBoolValueCmpNotEqual;
                    end;

            ftString, ftFixedChar :
               if Not (loCaseInsensitive in FFieldFilter.FFieldFilters.FOptions)
               { Case Sensitive }
               then begin
                    Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptAnsiStringCsValueCmpEqual;
                                      1 : AcceptValue := @AcceptAnsiStringCsValueCmpEndWith;
                                      2 : AcceptValue := @AcceptAnsiStringCsValueCmpStartWith;
                                      3 : AcceptValue := @AcceptAnsiStringCsValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptAnsiStringCsValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptAnsiStringCsValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptAnsiStringCsValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptAnsiStringCsValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptAnsiStringCsValueCmpLesser;
                    end; { Case }
                    IsEqual    := @AcceptAnsiStringCsValueCmpEqual;
                    IsLesser   := @AcceptAnsiStringCsValueCmpLesser;
                    end
               { Not Case Sensitive }
               else begin
                    Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptAnsiStringValueCmpEqual;
                                      1 : AcceptValue := @AcceptAnsiStringValueCmpStartWith;
                                      2 : AcceptValue := @AcceptAnsiStringValueCmpEndWith;
                                      3 : AcceptValue := @AcceptAnsiStringValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptAnsiStringValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptAnsiStringValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptAnsiStringValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptAnsiStringValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptAnsiStringValueCmpLesser;
                    end; {Case}
                    IsEqual    := @AcceptAnsiStringValueCmpEqual;
                    IsLesser   := @AcceptAnsiStringValueCmpLesser;
                    end; {if}

            ftWideString:
               if Not (loCaseInsensitive in FFieldFilter.FFieldFilters.FOptions)
               { Case Sensitive }
               then begin Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptWideStringCsValueCmpEqual;
                                      1 : AcceptValue := @AcceptWideStringCsValueCmpEndWith;
                                      2 : AcceptValue := @AcceptWideStringCsValueCmpStartWith;
                                      3 : AcceptValue := @AcceptWideStringCsValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptWideStringCsValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptWideStringCsValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptWideStringCsValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptWideStringCsValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptWideStringCsValueCmpLesser;
                    end;
                    IsEqual    := @AcceptWideStringCsValueCmpEqual;
                    IsLesser   := @AcceptWideStringCsValueCmpLesser;
                    end
               { Not Case Sensitive }
               else begin Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptWideStringValueCmpEqual;
                                      1 : AcceptValue := @AcceptWideStringValueCmpEndWith;
                                      2 : AcceptValue := @AcceptWideStringValueCmpStartWith;
                                      3 : AcceptValue := @AcceptWideStringValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptWideStringValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptWideStringValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptWideStringValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptWideStringValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptWideStringValueCmpLesser;
                    end; {Case}
                    IsEqual    := @AcceptWideStringValueCmpEqual;
                    IsLesser   := @AcceptWideStringValueCmpLesser;
                    end;{if}


            ftMemo:
               if Not (loCaseInsensitive in FFieldFilter.FFieldFilters.FOptions)
               { Case Sensitive }
               then begin Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptMemoCsValueCmpEqual;
                                      1 : AcceptValue := @AcceptMemoCsValueCmpEndWith;
                                      2 : AcceptValue := @AcceptMemoCsValueCmpStartWith;
                                      3 : AcceptValue := @AcceptMemoCsValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptMemoCsValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptMemoCsValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptMemoCsValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptMemoCsValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptMemoCsValueCmpLesser;
                    end;
                    IsEqual    := @AcceptMemoCsValueCmpEqual;
                    IsLesser   := @AcceptMemoCsValueCmpLesser;
                    end
               { Not Case Sensitive }
               else begin Case FComparisonOperator of
                    CmpEqual        :Case TextComparator of
                                      0 : AcceptValue := @AcceptMemoValueCmpEqual;
                                      1 : AcceptValue := @AcceptMemoValueCmpStartWith;
                                      2 : AcceptValue := @AcceptMemoValueCmpEndWith;
                                      3 : AcceptValue := @AcceptMemoValueCmpAnywhere;
                                      end;
                    CmpNotEqual     : AcceptValue := @AcceptMemoValueCmpNotEqual;
                    CmpGreater      : AcceptValue := @AcceptMemoValueCmpGreater;
                    CmpGreateOrEqual: AcceptValue := @AcceptMemoValueCmpGreaterOrEqual;
                    CmpLesserOrEqual: AcceptValue := @AcceptMemoValueCmpLesserOrEqual;
                    CmpLesser       : AcceptValue := @AcceptMemoValueCmpLesser;
                    end; {Case}
                    IsEqual    := @AcceptMemoValueCmpEqual;
                    IsLesser   := @AcceptMemoValueCmpLesser;
                    end; {if}

            end; { Case FFieldFilter.FField.DataType }
            end; { else begin }
  end;
end;

procedure TSqlitePassFieldFilterExp.SetFValue(const StrValue: String);
var
StrSize: Integer;
TempStrValue: String;

begin
FTextValue := StrValue;

If Assigned(FValueMem)
   then FreeMem(FValueMem);

FValue := nil;

if FComparisonOperator in [cmpNull, cmpNotNull]
   then FValue := FFieldFilter.FField
   else begin
        Case FFieldFilter.FField.DataType of

           ftInteger, ftSmallInt, ftWord:
             begin
             GetMem(FValueMem,SizeOf(Integer));
             PInteger(FValueMem)^ := StrToIntDef(StrValue,0);
             end;

           ftDate:
             begin
             GetMem(FValueMem,SizeOf(Integer));
             TSqlitePassDataset(FFieldFilter.FField.Dataset).FDatabase.FTranslator.DateTextToInteger(StrValue,PInteger(FValueMem)^);
             end;

           ftTime :
             begin
             GetMem(FValueMem,SizeOf(Integer));
             TSqlitePassDataset(FFieldFilter.FField.Dataset).FDatabase.FTranslator.TimeTextToInteger(StrValue,PInteger(FValueMem)^);
             end;

           ftString, ftFixedChar, ftMemo, FtFmtMemo:
             begin
             // TODO - String Size
             TempStrValue := StringReplace(StrValue,'*','', [rfReplaceAll]);
             TempStrValue := StringReplace(TempStrValue,'%','', [rfReplaceAll]);
             StrSize := Length(TempStrValue)+1;
             GetMem(FValueMem, StrSize);
             StrCopy(FValueMem, PAnsiChar(TempStrValue));
             end;

           ftWideString:
             begin
             TempStrValue := StringReplace(StrValue,'*','', [rfReplaceAll]);
             TempStrValue := StringReplace(TempStrValue,'%','', [rfReplaceAll]);
             // TODO - Check WideString Size...// Length
             StrSize := (Length(TempStrValue)*2)+1;
             GetMem(FValueMem, StrSize);
             SqlitePassUtils.WStrCopy(FValueMem, PWideChar(TempStrValue));
             end;

           ftFloat, ftCurrency:
             begin
             GetMem(FValueMem,SizeOf(Double));
             PDouble(FValueMem)^ := StrToFloat(StrValue);
             end;

           ftBCD:
             begin
             GetMem(FValueMem,SizeOf(Currency));
             PCurrency(FValueMem)^ := StrToFloat(StrValue);
             end;

           ftDateTime:
             begin
             GetMem(FValueMem,SizeOf(Double));
             TSqlitePassDataset(FFieldFilter.FField.Dataset).FDatabase.FTranslator.DateTimeTextToDouble(StrValue,PDouble(FValueMem)^);
             end;

           ftBoolean:
             begin
             GetMem(FValueMem,SizeOf(WordBool));
             TSqlitePassDataset(FFieldFilter.FField.Dataset).FDatabase.FTranslator.BooleanTextToWordBool(StrValue, PWordBool(FValueMem)^);
             end;

           ftAutoInc, ftLargeint:
             begin
             GetMem(FValueMem,SizeOf(Int64));
             PInt64(FValueMem)^ := StrToInt64Def(StrValue,0);
             end;
         end; { Case }
         FValue := FValueMem;
         end;
end;

function TSqlitePassFieldFilterExp.ComparisonOpAsText: String;
begin
Case FComparisonOperator of
     CmpEqual         : Result := ' = ';
     CmpNotEqual      : Result := ' <> ';
     CmpGreater       : Result := ' > ';
     CmpGreateOrEqual : Result := ' >= ';
     CmpLesserOrEqual : Result := ' <= ';
     CmpLesser        : Result := ' < ';
     CmpNull          : Result := ' Null ';
     CmpNotNull       : Result := ' NotNull ';
     else               Result := '';
     end;
end;

function TSqlitePassFieldFilterExp.LogicalOpAsText: String;
begin
Case FLogicalOperator of
     opAnd : Result := ' And';
     opOr  : Result := ' Or';
     else    Result := '';
     end;
end;

function TSqlitePassFieldFilterExp.GetExpressionText: String;
var
Tokenizer: TSqlitePassSQLTokenizer;
QuoteChar, TempValue: String;
begin
Tokenizer := TSqlitePassSQLTokenizer.Create;
Try
  Tokenizer.Text := Value;
  Tokenizer.Clean([ttSingleQuote, ttGraveQuote, ttDoubleQuote, ttSquareOpen, ttSquareClose, ttHash]);
  TempValue := Tokenizer.Text;

  Case FFieldFilter.FField.Datatype of
      ftString, ftFixedChar, ftWideString : QuoteChar := '"';
      ftDateTime, ftDate, ftTime: QuoteChar := '#';
      else QuoteChar := '';
      end;
  TempValue := QuoteChar + TempValue + QuoteChar;
  Result := LogicalOpAsText + ComparisonOpAsText + ' ' + TempValue;
finally
  Tokenizer.Free;
end;
end;

procedure TSqlitePassFieldFilterExp.BinarySetResultRecordsRanges(var RangeStart, RangeStop: Integer);
var
Lo, Hi, Mid: Integer;
Recordset: TSqlitePassRecordset;

  function GetPriorLimit(Index: Integer): Integer;
  begin
  While (Index > -1) do
    begin
    if Not IsEqual(Recordset,
                  Recordset.GetFieldValueBuffer(Recordset.FRecords[Index], FFieldFilter.FField.Tag),
                  FValue) then Break;
    Dec(Index);
    end;

  Result := Index;
  end;

  function GetNextLimit(Index: Integer): Integer;
  begin
  While (Index < Recordset.FRecordsCount) do
    begin
    if Not IsEqual(Recordset,
                  Recordset.GetFieldValueBuffer(Recordset.FRecords[Index], FFieldFilter.FField.Tag),
                  FValue) then Break;
    Inc(Index);
    end;

  Result := Index;
  end;


{ --- BinaryLocate Main procedure --- }
begin
Lo := RangeStart;
Hi := RangeStop;
Recordset := FFieldFilter.FFieldFilters.FDataset.FRecordset;

while (Lo < Hi) do
  begin
  Mid := Lo + ((Hi - Lo) Div 2);

  { if (Recordset.FRecords[Mid] < value) }
  If IsLesser(Recordset,
              Recordset.GetFieldValueBuffer(Recordset.FRecords[Mid], FFieldFilter.FField.Tag),
              FValue)
      then Lo := Mid + 1
      else Hi := Mid;
  end; { while (Lo < Hi) do }

{ Binary search is finished now. Did we find a matching record ? }

 { if ((Low < Recordset.RecordsCount) and (Recordset.FRecords[Lo] = value)) }
 if   ((Lo < Recordset.RecordsCount)
   and (IsEqual(Recordset,
                Recordset.GetFieldValueBuffer(Recordset.FRecords[Lo], FFieldFilter.FField.Tag),
                FValue))) then

   begin   { Found }
   if FFieldFilter.FFieldFilters.FResultRecordsMaxCount = 1
      then begin
           RangeStart := Lo;
           RangeStop  := Lo;
           end
      else begin
           Case FComparisonOperator of
                CmpEqual        : begin
                                  RangeStart := GetPriorLimit(Lo) + 1;
                                  RangeStop  := GetNextLimit(Lo) - 1;
                                  end;
                CmpGreater      : begin
                                  RangeStart := GetNextLimit(Lo);
                                  RangeStop  := Pred(Recordset.FRecordsCount);
                                  end;
                CmpGreateOrEqual: begin
                                  RangeStart := GetPriorLimit(Lo) + 1;
                                  RangeStop  := Pred(Recordset.FRecordsCount);
                                  end;
                CmpLesserOrEqual: begin
                                  RangeStart := 0;
                                  RangeStop  := GetNextLimit(Lo) - 1;
                                  end;
                CmpLesser       : begin
                                  RangeStart := 0;
                                  RangeStop  := GetPriorLimit(Lo);
                                  end;
                end; { Case }
           RangeStart := Max(0, RangeStart);
           RangeStop  := Min(RangeStop, Pred(Recordset.FRecordsCount));
           end;
   end
   { Not Found }
   else begin
        RangeStart := -1;//0
        RangeStop  := -1;//0//Pred(Recordset.FRecordsCount);
        end;
end;


{ The following filters functions compare a FieldValue with a FilterValue }

{ this is the default comparison fonction assigned to AcceptValue function }
function AcceptDummyValue(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean;
begin
 Result := True;
end;

{ Null Filters }
function AcceptValueCmpNull(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
 Result := Recordset.FieldIsNull(FieldValue);
end;

function AcceptValueCmpNotNull(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
 Result := Not AcceptValueCmpNull(Recordset, FieldValue, FilterValue);
end;

{ Integer Filters }
function AcceptIntegerValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInteger(FieldValue)^ =  PInteger(FilterValue)^;
end;

function AcceptIntegerValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInteger(FieldValue)^ <>  PInteger(FilterValue)^;
end;

function AcceptIntegerValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInteger(FieldValue)^ >  PInteger(FilterValue)^;
end;

function AcceptIntegerValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := (PInteger(FieldValue)^ >=  PInteger(FilterValue)^);
end;

function AcceptIntegerValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := (PInteger(FieldValue)^ <=  PInteger(FilterValue)^);
end;

function AcceptIntegerValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInteger(FieldValue)^ <  PInteger(FilterValue)^;
end;

{ Int64 Filters }
function AcceptInt64ValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ =  PInt64(FilterValue)^;
end;

function AcceptInt64ValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ <>  PInt64(FilterValue)^;
end;

function AcceptInt64ValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ >  PInt64(FilterValue)^;
end;

function AcceptInt64ValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ >=  PInt64(FilterValue)^;
end;

function AcceptInt64ValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ <=  PInt64(FilterValue)^;
end;

function AcceptInt64ValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PInt64(FieldValue)^ <  PInt64(FilterValue)^;
end;

{ Double Filters }
function AcceptDoubleValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ =  PDouble(FilterValue)^;
end;

function AcceptDoubleValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ <>  PDouble(FilterValue)^;
end;

function AcceptDoubleValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ >  PDouble(FilterValue)^;
end;

function AcceptDoubleValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ >=  PDouble(FilterValue)^;
end;

function AcceptDoubleValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ <=  PDouble(FilterValue)^;
end;

function AcceptDoubleValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PDouble(FieldValue)^ <  PDouble(FilterValue)^;
end;

{ Currency Filters }
function AcceptCurrencyValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ =  PCurrency(FilterValue)^;
end;

function AcceptCurrencyValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ <>  PCurrency(FilterValue)^;
end;

function AcceptCurrencyValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ >  PCurrency(FilterValue)^;
end;

function AcceptCurrencyValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ >=  PCurrency(FilterValue)^;
end;

function AcceptCurrencyValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ <=  PCurrency(FilterValue)^;
end;

function AcceptCurrencyValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PCurrency(FieldValue)^ <  PCurrency(FilterValue)^;
end;

{ WordBool Filters }
function AcceptWordBoolValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PWordBool(FieldValue)^ =  PWordBool(FilterValue)^;
end;

function AcceptWordBoolValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := PWordBool(FieldValue)^ <>  PWordBool(FilterValue)^;
end;

{ AnsiString Filters - Case Sensitive }
function AcceptAnsiStringCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) = 0;
end;

function AcceptAnsiStringCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) <> 0;
end;

function AcceptAnsiStringCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) > 0;
end;

function AcceptAnsiStringCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) < 0);
end;

function AcceptAnsiStringCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) > 0);
end;

function AcceptAnsiStringCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) < 0;
end;

function AcceptAnsiStringCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrLComp(pAnsiChar(FilterValue), pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), Length(pAnsiChar(FilterValue))) = 0;
end;

function AcceptAnsiStringCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PAnsiChar;
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result then
   begin
   StrFieldValue := PAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]);
   ResultPos := Length(StrFieldValue) - Length(PAnsiChar(FilterValue));
   Result := SqlitePassUtils.RPos(PAnsiChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
   end;
end;

function AcceptAnsiStringCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrPos(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) <> nil;
end;

{ AnsiString Filters - Not Case Sensitive }
function AcceptAnsiStringValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(PAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), PAnsiChar(FilterValue)) = 0;
end;

function AcceptAnsiStringValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) <> 0;
end;

function AcceptAnsiStringValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) > 0;
end;

function AcceptAnsiStringValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrIComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) < 0);
end;

function AcceptAnsiStringValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrIComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) > 0)
end;

function AcceptAnsiStringValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), pAnsiChar(FilterValue)) < 0;
end;

function AcceptAnsiStringValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrLIComp(pAnsiChar(FilterValue), pAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]), Length(pAnsiChar(FilterValue))) = 0;
end;

function AcceptAnsiStringValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PAnsiChar;
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result then
  begin
  StrFieldValue := PAnsiChar(Recordset.FAnsiStrings[PInteger(FieldValue)^]);
  ResultPos := Length(StrFieldValue) - Length(PAnsiChar(FilterValue));
  Result := SqlitePassUtils.RPos(PAnsiChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
  end;
end;

function AcceptAnsiStringValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
//TODO Move UpperFilterValue to FilterExp and check efficiency...
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrPos(AnsiStrUpper(pAnsiChar(FilterValue)),AnsiStrUpper(pAnsiChar(FieldValue))) <> nil;
end;


{ WideString Filters - Not Case Sensitive }

{ TODO - For compatibility with Delphi < 2009, we keep generic PChar Type. Could be changed with IfDef in futur release }
function AcceptWideStringCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) = 0;
end;

function AcceptWideStringCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) <> 0;
end;

function AcceptWideStringCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) > 0;
end;

function AcceptWideStringCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result :=  Not ((AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue))) < 0);
end;

function AcceptWideStringCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result :=  Not ((AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue))) > 0);
end;

function AcceptWideStringCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := ((AnsiStrComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue))) < 0);
end;

function AcceptWideStringCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrLComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue), Length(PChar(FilterValue))) = 0;
end;

function AcceptWideStringCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PChar;
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result then
  begin
  StrFieldValue := PChar(Recordset.FWideStrings[PInteger(FieldValue)^]);
  ResultPos := Length(StrFieldValue) - Length(PChar(FilterValue));
  Result := SqlitePassUtils.RPos(PChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
  end;
end;

function AcceptWideStringCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrPos(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) <> nil;
end;

{ WideString Filters - Not Case Sensitive }
function AcceptWideStringValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) = 0;
end;

function AcceptWideStringValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) <> 0;
end;

function AcceptWideStringValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) > 0;
end;

function AcceptWideStringValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) < 0);
end;

function AcceptWideStringValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := Not (AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) > 0)
end;

function AcceptWideStringValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result :=  AnsiStrIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FilterValue)) < 0;
end;

function AcceptWideStringValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrLIComp(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]), PChar(FieldValue), Length(PChar(FilterValue))) = 0;
end;

function AcceptWideStringValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PChar;
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result then
  begin
  StrFieldValue := PChar(Recordset.FWideStrings[PInteger(FieldValue)^]);
  ResultPos := Length(StrFieldValue) - Length(PChar(FilterValue));
  Result := SqlitePassUtils.RPos(PChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
  end;
end;

function AcceptWideStringValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
//var
//TODO Move UpperFilterValue to FilterExp and check efficiency...
//UpperFieldValue, UpperFilterValue: String;
begin
Result := Not Recordset.FieldIsNull(FieldValue);
if Result
   then Result := AnsiStrPos(AnsiStrUpper(PChar(FilterValue)),AnsiStrUpper(PChar(Recordset.FWideStrings[PInteger(FieldValue)^]))) <> nil;
end;


{ Memo }
function GetMemoValue(Const Recordset: TSqlitePassRecordset; Const FieldValue: Pointer): AnsiString; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoryStream: TMemorySTream;
begin
  MemoryStream := Recordset.FMemoryStreams[PInteger(FieldValue)^];
  if MemoryStream.Size > 0
     then SetString(Result, pAnsiChar(MemoryStream.Memory), MemoryStream.Size)
     else Result := '';
end;

{ Memo Filters - Not Case Sensitive }
function AcceptMemoCsValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result := AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) = 0;
end;

function AcceptMemoCsValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result := AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) <> 0;
end;

function AcceptMemoCsValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result := AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) > 0;
end;

function AcceptMemoCsValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result :=  Not (AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) < 0);
end;

function AcceptMemoCsValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result :=  Not (AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) > 0);
end;

function AcceptMemoCsValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
 MemoText := GetMemoValue(Recordset, FieldValue);
 Result := AnsiStrComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) < 0;
end;

function AcceptMemoCsValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrLComp(pAnsiChar(FilterValue), pAnsiChar(MemoText), Length(pAnsiChar(FilterValue))) = 0;
end;

function AcceptMemoCsValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PAnsiChar;
begin
StrFieldValue := PAnsiChar(GetMemoValue(Recordset, FieldValue));
ResultPos := Length(StrFieldValue) - Length(PAnsiChar(FilterValue));
Result := SqlitePassUtils.RPos(PAnsiChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
end;

function AcceptMemoCsValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrPos(pAnsiChar(MemoText), pAnsiChar(FilterValue)) <> nil;
end;

{ Memo Filters - Not Case Sensitive }
function AcceptMemoValueCmpEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) = 0;
end;

function AcceptMemoValueCmpNotEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) <> 0;
end;

function AcceptMemoValueCmpGreater(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) > 0;
end;

function AcceptMemoValueCmpGreaterOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := Not (AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) < 0);
end;

function AcceptMemoValueCmpLesserOrEqual(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := Not (AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) > 0)
end;

function AcceptMemoValueCmpLesser(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result :=  AnsiStrIComp(pAnsiChar(MemoText), pAnsiChar(FilterValue)) < 0;
end;

function AcceptMemoValueCmpStartWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrLIComp(pAnsiChar(MemoText), pAnsiChar(FieldValue), Length(pAnsiChar(FilterValue))) = 0;
end;

function AcceptMemoValueCmpEndWith(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
ResultPos: Integer;
StrFieldValue: PAnsiChar;
begin
StrFieldValue := PAnsiChar(GetMemoValue(Recordset, FieldValue));
ResultPos := Length(StrFieldValue) - Length(PAnsiChar(FilterValue));
Result := SqlitePassUtils.RPos(PAnsiChar(FilterValue), StrFieldValue, ResultPos + 1) > ResultPos;
end;

function AcceptMemoValueCmpAnywhere(Const Recordset: TSqlitePassRecordset; Const FieldValue, FilterValue: Pointer):Boolean; {$IFDEF HasInline} inline; {$ENDIF}
var
MemoText: AnsiString;
begin
  MemoText := GetMemoValue(Recordset, FieldValue);
  Result := AnsiStrPos(AnsiStrUpper(PAnsiChar(MemoText)),AnsiStrUpper(PAnsiChar(FieldValue))) <> nil;
end;



{ TSqlitePassFieldFilter }

constructor TSqlitePassFieldFilter.Create(FieldFilters: TSqlitePassFieldFilters; Field: TField);
begin
 Inherited Create;
 FKeyFieldIndex := -1;
 FCanGetValueFromRecordBuffer :=  False;
 FInMemoryIndex:= nil; 
 FFieldFilters := FieldFilters;
 FField:= Field;
end;

destructor TSqlitePassFieldFilter.Destroy;
begin
 ClearAndFreeItems;
 FField := nil;
 FFieldFilters := nil;
 FInMemoryIndex:= nil;
 Inherited Destroy;
end;

function TSqlitePassFieldFilter.GetHasFilterExpression: Boolean;
begin
 Result := Count > 0;
end;

procedure TSqlitePassFieldFilter.NotifyFilterChanged;
begin
 FFieldFilters.NotifyFiltersChanged;
end;

procedure TSqlitePassFieldFilter.SetFFiltered(Value: Boolean);
begin
if Value <> FFiltered then
   begin
   FFiltered := Value;
   NotifyFilterChanged
   end;
end;

function TSqlitePassFieldFilter.GetItem(
  Index: Integer): TSqlitePassFieldFilterExp;
begin
  Result:=TSqlitePassFieldFilterExp(Inherited Items[Index]);
end;

procedure TSqlitePassFieldFilter.SetItem(Index: Integer;
  const Value: TSqlitePassFieldFilterExp);
begin
  Put(Index,Value);
  NotifyFilterChanged
end;

procedure TSqlitePassFieldFilter.AddFilterExpression
          (LogicalOperator:TSqlitePassFilterLogicalOperators;
           ComparisonOperator: TSqlitePassFilterComparisonOperators;
           FilterValue: String);
var
FilterExp: TSqlitePassFieldFilterExp;
begin
if (ComparisonOperator <> cmpUnknown) and (FilterValue <> '') then
    begin
    FilterExp := TSqlitePassFieldFilterExp.Create(Self);
      try
      if Count > 0
          then FilterExp.FLogicalOperator := LogicalOperator
          else FilterExp.FLogicalOperator := opNone;
      FilterExp.Value := FilterValue;
      FilterExp.ComparisonOperator := ComparisonOperator;
      Add(FilterExp);
      NotifyFilterChanged;
      except
      FilterExp.Free;
      end;
    end;
end;

function TSqlitePassFieldFilter.GetFilterText: String;
var
i: Integer;
begin
Result := '';
if Count > 0 then
   begin
   Result := '"' + FField.FieldName + '"';
   for i := 0 to Pred(Count)
     do Result := Result + ' ' + Items[i].GetExpressionText;
   Result := Result + ';';
   end;
end;

procedure TSqlitePassFieldFilter.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
Filtered := False;
NotifyFilterChanged;
end;

procedure TSqlitePassFieldFilter.ClearFilter;
begin
ClearAndFreeItems;
end;

function TSqlitePassFieldFilter.AcceptValue(Const FieldValue: Pointer): Boolean;
var
i: Integer;
FilterExp: TSqlitePassFieldFilterExp;
Recordset: TSqlitePassRecordset;
begin
Recordset := TSqlitePassDataset(FField.Dataset).FRecordset;
Result := True;

for i := 0 to Pred(Count) do
  begin
  FilterExp := Items[i];
  Case FilterExp.LogicalOperator of
    opNone : Result := FilterExp.AcceptValue(Recordset, FieldValue, FilterExp.FValue);
    opAnd  : begin
             Result := Result And FilterExp.AcceptValue(Recordset, FieldValue, FilterExp.FValue);
             If Not Result then Break;
             end;
    opOr   : Result := Result Or FilterExp.AcceptValue(Recordset, FieldValue, FilterExp.FValue);
    end;
  end;
end;

procedure TSqlitePassFieldFilter.SetKeyField(Const KeyField: TField);
begin
 FKeyFieldIndex := KeyField.Tag;
 FCanGetValueFromRecordBuffer := (KeyField.DataType = FField.DataType);
end;



{ TSqlitePassFieldFilters }

constructor TSqlitePassFieldFilters.Create(Dataset: TSqlitePassDataset;  OnChange: TNotifyEvent);
begin
 Inherited Create;
 FUpdating := False;
 FInMemoryIndexedFilter := nil;
 FDataset := Dataset;
 FOptions := [];
 FActiveFilters := TList.Create;
 FResultRecordsList := TSqlitePassIntegerList.Create;
 FResultRecordsListCurrentIndex := -1;
 FResultRecordsListCurrentRecordIndex := -1;
 FResultRecordsMaxCount := MaxInt;
 FOnChange := OnChange;
end;

destructor TSqlitePassFieldFilters.Destroy;
begin
 FOnChange := nil;
 FInMemoryIndexedFilter := nil;
 FActiveFilters.Clear;
 FActiveFilters.Free;
 ClearAndFreeItems;
 FResultRecordsList.Free;
 FDataset := nil;
 Inherited Destroy;
end;

procedure TSqlitePassFieldFilters.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

{ Create a FieldFilter for every Fields including Calc and Lookup Fields }
procedure TSqlitePassFieldFilters.InitFilters;
var
i: Integer;
Filter: TSqlitePassFieldFilter;
begin
Clear;
for i := 0 to Pred(FDataset.Fields.Count) do
  begin
  Filter := TSqlitePassFieldFilter.Create(Self, FDataset.Fields[i]);
  Add(Filter);
  end;
end;


procedure TSqlitePassFieldFilters.ParseFilterText(const Value: String);
var
CurrentPos: Integer;
Tokenizer: TSqlitePassSQLTokenizer;
Field: TField;
LogOperator: TSqlitePassFilterLogicalOperators;
CompOperator: TSqlitePassFilterComparisonOperators;
FilterValue: String;
begin

if FFilterText = Value
   then Exit;

FFilterText := Value;
ClearFilters;

Tokenizer := TSqlitePassSQLTokenizer.Create;
Try
Tokenizer.Text := FFilterText;
{ Now he have to clean and parse the statement }
Tokenizer.Clean([ttSquareOpen, ttSquareClose, ttWhiteSpace, ttDoubleQuote, ttSingleQuote, ttGraveQuote, ttHash]);

   Try
   While Not Tokenizer.Eof do
       begin
       { We have a FieldName }
       If Tokenizer.Token.TokenType = ttIdentifier then
          begin
          Field := FDataset.Fields.FieldByName(Tokenizer.Token.Text);
          if Field = nil then Exit;
          Tokenizer.Next;
          { We parse filter for the current field }
          While Not Tokenizer.Eof do
            begin
            CurrentPos := Tokenizer.TokenIndex;
            { Logical Operator }
            Case Tokenizer.Token.TokenType of
                 ttAnd           : begin
                                   LogOperator := opAnd;
                                   Tokenizer.Next;
                                   end;
                 ttOr            : begin
                                   LogOperator := opOr;
                                   Tokenizer.Next;
                                   end;
                 ttComma         : LogOperator := opOr;
                 else              LogOperator := opNone;
                 end;

            { Comparison Operator }
            Case Tokenizer.Token.TokenType of
                 ttEqual         : CompOperator := CmpEqual;
                 ttNotEqual      : CompOperator := CmpNotEqual;
                 ttGreater       : CompOperator := CmpGreater;
                 ttGreaterOrEqual: CompOperator := CmpGreateOrEqual;
                 ttLesser        : CompOperator := CmpLesser;
                 ttLesserOrEqual : CompOperator := CmpLesserOrEqual;
                 ttNull          : CompOperator := CmpNull;
                 ttNotNull       : CompOperator := CmpNotNull;
                 ttComma         : ; { Keeps the previous CompOperator}
                 else              CompOperator := cmpUnknown;
                 end;

            if not (CompOperator in [CmpNull, cmpNotNull, cmpUnknown])
               then Tokenizer.Next;

            { FilterValue }
            FilterValue := '';
            While Not Tokenizer.Eof do
              begin
                if Tokenizer.Token.TokenType in [ttIdentifier, ttStar, ttPercent, ttNumber, ttDateTime] then
                begin
                FilterValue := FilterValue + Tokenizer.Token.Text;
                Tokenizer.Next;
                end
                else Break;
              end;

            if CompOperator <> cmpUnknown
               then FilterByField(Field).AddFilterExpression(LogOperator, CompOperator, FilterValue)
               else Exit;

            if Not Tokenizer.Eof then
               begin
               if Tokenizer.Token.TokenType = ttSemiColon then
                 begin
                 Tokenizer.Next(ttIdentifier);
                 Break;
                 end;
               end;

            { if no match was found -> syntax error ? we move to the next token
              otherwise we could loop undefinitively }
            if CurrentPos = Tokenizer.TokenIndex
               then Tokenizer.Next;

            end; { While Not Tokenizer.Eof }
       end { If Tokenizer.Token.TokenType = ttIdentifier}
       else Tokenizer.Next;
  end; { While Not Tokenizer.Eof }
  Except
  FDataset.FDatabase.DatabaseError.RaiseException(Msg2040 + Tokenizer.GetTextBefore(ttStar));
  end;

  EnableFilters;
finally
Tokenizer.Free;
end;

end;

procedure TSqlitePassFieldFilters.ParseFilterText(const KeyFields: String; const KeyValues: Variant; FilterOptions: TLocateOptions);
var
i: Integer;
KeyFieldsList: TList;
TempFilterText, PartialCmp: String;

  function GetFilterText(KeyField: TField; KeyValue: Variant): String;
  begin
  if KeyField.Datatype in [ftString, ftWideString, ftMemo]
     then Result :=  KeyField.FieldName + '= "' + PartialCmp + VarAsType(KeyValue, VarString) + PartialCmp + '"'
     else Result :=  KeyField.FieldName + '= "' + VarAsType(KeyValue, VarString) + '"';
  end;

begin
  TempFilterText := '';
  FOptions := FilterOptions;

  if loPartialKey in FOptions
     then PartialCmp := '*'
     else PartialCmp := '';

  KeyFieldsList:= TList.Create;
  FDataset.GetFieldList(KeyFieldsList, KeyFields);

  Case KeyFieldsList.Count of
     0    : ;
     1    : TempFilterText := GetFilterText(TField(KeyFieldsList[0]), KeyValues);
     else   begin
            for i := 0 to Pred(KeyFieldsList.Count)
              do TempFilterText := GetFilterText(TField(KeyFieldsList[i]), KeyValues[i]);
            end;
     end;
  KeyFieldsList.Free;
  ParseFilterText(TempFilterText);
end;


{ Create a sublist from "AllFilters" -> "ActiveFilters" of active (filtered) Filters
 to speed up filters operations }
procedure TSqlitePassFieldFilters.InitActiveFilters;
var
i: Integer;
FFieldFilter: TSqlitePassFieldFilter;
begin
FActiveFilters.Clear;
for i := 0 to Pred(FDataset.Fields.Count) do
    begin
    FFieldFilter := FilterByField(FDataset.Fields[i]);
    if Assigned(FFieldFilter) and FFieldFilter.Filtered
       then FActiveFilters.Add(FFieldFilter);
    end;
end;

function TSqlitePassFieldFilters.GetFilterText(FilteredOnly: Boolean = False): String;
var
i: Integer;
begin
Result := '';
 for i := 0 to Pred(Count) do
   begin
   if (FilteredOnly and Items[i].Filtered) or (Not FilteredOnly)
      then Result := Result + Items[i].GetFilterText;
   end;
end;

procedure TSqlitePassFieldFilters.ClearFilters;
var
i: Integer;
begin
{ Clears the filters }
for i := 0 to Pred(Count)
    do Items[i].ClearFilter;

FActiveFilters.Clear;
ResetResultRecords;
end;

function TSqlitePassFieldFilters.GetItem(Index: Integer): TSqlitePassFieldFilter;
begin
  Result:=TSqlitePassFieldFilter(Inherited Items[Index]);
end;

procedure TSqlitePassFieldFilters.SetItem(Index: Integer;
  const Value: TSqlitePassFieldFilter);
begin
  Put(Index,Value);
end;

function TSqlitePassFieldFilters.FilterByField(const Value: TField): TSqlitePassFieldFilter;
var
i: Integer;
begin
 Result := nil;
 for i := 0 to Pred(Count) do
    begin
    if Value = Items[i].FField
       then Result := Items[i];
    if Assigned(Result)
       then Break
    end;

 if Result = nil
    then DatabaseErrorFmt(Msg2100, [Value], nil);
end;

function TSqlitePassFieldFilters.FilterByFieldName(const Value: String): TSqlitePassFieldFilter;
begin
  Result := FilterByField(FDataset.Fields.FindField(Value));
end;

procedure TSqlitePassFieldFilters.EnableFilters;
var
i: Integer;
begin
{ Enable the filters }
for i := 0 to Pred(Count) do
    With Items[i] do Filtered := (Count > 0);
end;

procedure TSqlitePassFieldFilters.DisableFilters;
var
i: Integer;
begin
{ Disable the filters }
for i := 0 to Pred(Count)
    do Items[i].Filtered := False;
end;

function TSqlitePassFieldFilters.GetFFiltered: Boolean;
var
i: Integer;
begin
Result := False;
{ Is there at least one filter active ? }
for i := 0 to Pred(Count) do
    begin
    Result := Items[i].Filtered;
    If Result
       then Break;
    end;
end;

procedure TSqlitePassFieldFilters.BeginUpdate;
begin
 FUpdating := True;
end;

procedure TSqlitePassFieldFilters.EndUpdate;
begin
 if FUpdating then
    begin
    FUpdating := False;
    NotifyFiltersChanged;
    end;
end;

procedure TSqlitePassFieldFilters.NotifyFiltersChanged;
begin
  if (Not FUpdating) and Assigned(FOnChange)
      then FOnChange(Self);
end;

{ Filtering with internal FFieldDefs.Filters when FilterMode = fmDirect or fmSqlDirect or
  by locate or lookup Filters }
function TSqlitePassFieldFilters.AcceptRecord(Const RecordBuffer: PRecBuffer): Boolean;
var
i, FieldPos: Integer;
FFieldFilter: TSqlitePassFieldFilter;
begin
Result := True;
for i := 0 to Pred(FActiveFilters.Count) do
  begin
  FFieldFilter := FActiveFilters[i];
  FieldPos := FFieldFilter.FField.Tag;
  Result := FFieldFilter.AcceptValue(FDataset.FRecordset.GetFieldValueBuffer(RecordBuffer, FieldPos));
  If Not Result
     then Break;
  end;
end;


procedure TSqlitePassFieldFilters.ScanRecords;
begin
  ResetResultRecords;
  if FActiveFilters.Count > 0 then
     begin
     { Prepare the Binary Search }
     InitBinaryScan;
     BinaryScanRecords;
     LinearScanRecords;
     end;
end;

{ Try to get one sorted or indexed colum to perform a binary search }
procedure TSqlitePassFieldFilters.InitBinaryScan;
begin
  FCanDoBinaryScan := False;
  FInMemoryIndexedFilter := FDataset.FInMemoryIndexes.GetIndexedFilter(TSqlitePassFieldFilters(FActiveFilters));
  { Can we perform a binary search ? }
  if Assigned(FInMemoryIndexedFilter)
     then if      (FInMemoryIndexedFilter.Count = 1) { Only one Filter Expression }
             and  (FInMemoryIndexedFilter[0].FComparisonOperator in [CmpEqual, CmpNotEqual, CmpGreater, CmpGreateOrEqual, CmpLesserOrEqual, CmpLesser])
             and  Not (loPartialKey in FInMemoryIndexedFilter.FFieldFilters.FOptions)
                  then FCanDoBinaryScan := True;
end;

{  Binary search :
  A binary search can be used if one of the KeyFields is already sorted
  and the Field Filter doesn't use wildcards.
  The CompOperator must be in [<, =<, =, >, >=]
  In this case we can find the locate range limits using a binary search.
  It will set a range Start and Stop in order to speed up the linear search. }
procedure TSqlitePassFieldFilters.BinaryScanRecords;
begin
  if FCanDoBinaryScan
     then FInMemoryIndexedFilter[0].BinarySetResultRecordsRanges(FRangeStart, FRangeStop);
end;

{ Linear search :
  The linear search is always used to finish the locate operation.
  It applies the filter to the records included in range start and stop.
  If the filter accepts the record content, then the record index is added to
  the located records list.}
procedure TSqlitePassFieldFilters.LinearScanRecords;
var
i, ResultCount: Integer;
begin
FResultRecordsList.Clear;
ResultCount := 0;
if (FRangeStart <> -1) and (FRangeStop <> -1) then
   begin
   For i := FRangeStart to FRangeStop do
     begin
     if AcceptRecord(FDataset.FRecordset.FRecords[i])
        then begin
             FResultRecordsList.Add(i);
             Inc(ResultCount);
             if ResultCount > FResultRecordsMaxCount
                then Break;
             end;
     end;
  end;
end;


Procedure TSqlitePassFieldFilters.ResetResultRecords;
begin
  FResultRecordsList.Clear;
  FResultRecordsListCurrentIndex := -1;
  FResultRecordsListCurrentRecordIndex := -1;
  FMoveState:= grError;
  FRangeStart := 0;
  FRangeStop := Pred(FDataset.FRecordset.RecordsCount);
end;

function TSqlitePassFieldFilters.MoveToRecord: Boolean;
begin
  FMoveState:= grError;

  if FResultRecordsList.Count > 0 then
     begin
     if (FResultRecordsListCurrentIndex > -1) and (FResultRecordsListCurrentIndex < FResultRecordsList.Count) then
        begin
        FResultRecordsListCurrentRecordIndex := FResultRecordsList.Items[FResultRecordsListCurrentIndex];
        if FResultRecordsListCurrentIndex = 0
           then FMoveState := grBOF
           else if FResultRecordsListCurrentIndex = Pred(FResultRecordsList.Count)
                   then FMoveState := grEOF
                   else FMoveState := grOk;
        end;
  end;
  Result := (FMoveState <> grError);
end;

function TSqlitePassFieldFilters.First: Boolean;
begin
 FResultRecordsListCurrentIndex := 0;
 Result := MoveToRecord;
end;

function TSqlitePassFieldFilters.Next: Boolean;
begin
 if FResultRecordsListCurrentIndex < FResultRecordsList.Count
    then Inc(FResultRecordsListCurrentIndex);
 Result := MoveToRecord;
end;

function TSqlitePassFieldFilters.Prior: Boolean;
begin
 if (FResultRecordsListCurrentIndex > 0)
    then Dec(FResultRecordsListCurrentIndex);
 Result := MoveToRecord;
end;

function TSqlitePassFieldFilters.Last: Boolean;
begin
 FResultRecordsListCurrentIndex := Pred(FResultRecordsList.Count);
 Result := MoveToRecord;
end;

procedure TSqlitePassFieldFilters.Assign(Source: TSqlitePassFieldFilters);
var
i: Integer;
begin
  FDataset    := Source.FDataset;
  FOptions    := Source.FOptions;
  FResultRecordsMaxCount := Source.FResultRecordsMaxCount;
  ClearAndFreeItems;
  InitFilters;
  ParseFilterText(Source.FilterText);
  for i := 0 to Pred(Count)
     do Items[i].FFiltered := Source[i].Filtered;
end;


{ Filtering with other filters }

procedure TSqlitePassDataset.OnFieldFiltersChange(Sender: TObject);
begin
  Include(FFilterChanges, fcDirectFilters);
  CheckRefreshFilteredRecords;
end;

procedure TSqlitePassDataset.CheckRefreshFilteredRecords;
begin
If Filtered and (Not Filters.FUpdating)
   then RefreshFilteredRecords;
end;

procedure TSqlitePassDataset.RefreshFilteredRecords;
begin
If Not Active then Exit;
if Filtered
   then begin
        Case FilterMode of
             fmDirect   : FRecordset.ShowFilteredRecords;
             fmSQL      : if (fcSQLFilter in FFilterChanges) then inherited Refresh;
             fmSQLDirect: begin
                          if (fcSQLFilter in FFilterChanges)
                              then inherited Refresh
                              else FRecordset.ShowFilteredRecords;
                          end;
             end { Case }
        end
   else begin { Not Filtered }
        Case FilterMode of
             fmDirect   : FRecordset.ShowAllRecords;
             fmSQL      : if (fcSQLFilter in FFilterChanges) then inherited Refresh;
             fmSQLDirect: begin
                          if (fcSQLFilter in FFilterChanges)
                             then inherited Refresh
                             else FRecordset.ShowAllRecords;
                          end;
             end { Case }
        end;
Resync([rmCenter]);
end;

procedure TSqlitePassDataset.SetFiltered(Value: Boolean);
begin
 Inherited SetFiltered(Value);
 If Not Filters.FUpdating
    then RefreshFilteredRecords;
end;

procedure TSqlitePassDataset.SetFilterOptions(Value: TFilterOptions);
var
Options: TLocateOptions;
begin
  Options := [];
  if foCaseInsensitive in Value
     then Include(Options, loCaseInsensitive);
  if not (foNoPartialCompare in Value)
     then Include(Options, loPartialKey);

  FFieldFilters.FOptions := Options;
  FLocateFieldFilters.FOptions := Options;
  FLookupKeyFieldFilters.FOptions := Options;
  FLookupFieldsKeyFieldFilters.FOptions := Options;
end;

procedure TSqlitePassDataset.SetFilterText(const Value: String);
begin
  if Value <> Filter then
     begin
     Inherited SetFilterText(Value);
     { We parse MasterFields/DetailFields only when dataset.fields are defined }
     if not (csLoading in ComponentState) and Active
        then ProcessFilterText;
     end;
end;

procedure TSqlitePassDataset.ProcessFilterText;
var
Tokenizer: TSqlitePassSQLTokenizer;

begin
  Tokenizer := TSqlitePassSQLTokenizer.Create;
  Tokenizer.Text := Filter;
  FFilterSQLStmt := '';
  FFilterDirectStmt := '';

  Case FFilterMode of
   fmDirect    : FFilterDirectStmt := Filter;
   fmSQL       : FFilterSQLStmt := Filter;
   fmSQLDirect : begin
                 if Tokenizer.Locate(ttSemiColon) then
                    begin
                    FFilterSQLStmt    := Tokenizer.GetTextBefore(ttSemiColon);
                    FFilterDirectStmt := Tokenizer.GetTextAfter(ttEOF);
                    end
                    else FFilterDirectStmt := Filter;
                 end;
    end;

  Tokenizer.Free;

  if FPreviousFilterSQLStmt <> FFilterSQLStmt then
    begin
    Include(FFilterChanges, fcSQLFilter);
    FPreviousFilterSQLStmt := FFilterSQLStmt;
    end;

  if FPreviousFilterDirectStmt <> FFilterDirectStmt then
     begin
     Include(FFilterChanges, fcDirectFilters);
     FPreviousFilterDirectStmt := FFilterDirectStmt;
     FFieldFilters.ParseFilterText(Filter);
     end;

  if FFilterChanges <> []
     then CheckRefreshFilteredRecords;

end;


(*procedure TSqlitePassDataset.SetFilterText(const Value: String);
var
Tokenizer: TSqlitePassSQLTokenizer;

begin
  if Value <> Filter then
     begin
     Inherited SetFilterText(Value);
     Tokenizer := TSqlitePassSQLTokenizer.Create;
     Tokenizer.Text := Value;
     FFilterSQLStmt := '';
     FFilterDirectStmt := '';

     Case FFilterMode of
       fmDirect    : FFilterDirectStmt := Value;
       fmSQL       : FFilterSQLStmt := Value;
       fmSQLDirect : begin
                     if Tokenizer.Locate(ttSemiColon) then
                        begin
                        FFilterSQLStmt    := Tokenizer.GetTextBefore(ttSemiColon);
                        FFilterDirectStmt := Tokenizer.GetTextAfter(ttEOF);
                        end
                        else FFilterDirectStmt := Value;
                     end;
        end;

      Tokenizer.Free;

      if FPreviousFilterSQLStmt <> FFilterSQLStmt then
        begin
        Include(FFilterChanges, fcSQLFilter);
        FPreviousFilterSQLStmt := FFilterSQLStmt;
        end;

      if FPreviousFilterDirectStmt <> FFilterDirectStmt then
         begin
         Include(FFilterChanges, fcDirectFilters);
         FPreviousFilterDirectStmt := FFilterDirectStmt;
         FFieldFilters.ParseFilterText(Value);
         end;

  if FFilterChanges <> []
     then CheckRefreshFilteredRecords;
  end;
end;*)

procedure TSqlitePassDataset.SetFFilterRecordLowerLimit(
  const Value: Integer);
begin
  if Value <> FFilterRecordLowerLimit then
     begin
     FFilterRecordLowerLimit := Value;
     Include(FFilterChanges, fcLowerLimit);
     CheckRefreshFilteredRecords;
     end;
end;

procedure TSqlitePassDataset.SetFFilterRecordUpperLimit(
  const Value: Integer);
begin
  if Value <> FFilterRecordUpperLimit then
     begin
     FFilterRecordUpperLimit := Value;
     Include(FFilterChanges, fcUpperLimit);
     CheckRefreshFilteredRecords;
     end;
end;

{ Implementation of virtual method TDataset.FindRecord }
function TSqlitePassDataset.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
SetFound(False);
if GoForward
   then if Restart
           then First
           else Next
   else if Restart
           then Last
           else Prior;
SetFound((FRecordset.MoveState <> grError) and (FRecordset.FRecordsCount > 0));
Result := Found;
end;

{ --------- Locate --------- }

function TSqlitePassDataset.LocateFirst: Boolean;
begin
 Result := FLocateFieldFilters.First;
 if Result
    then MoveBy(FLocateFieldFilters.FResultRecordsListCurrentRecordIndex - PInteger(ActiveBuffer)^);
end;

function TSqlitePassDataset.LocateNext: Boolean;
begin
 Result := FLocateFieldFilters.Next;
 if Result
    then MoveBy(FLocateFieldFilters.FResultRecordsListCurrentRecordIndex - PInteger(ActiveBuffer)^);
end;

function TSqlitePassDataset.LocatePrior: Boolean;
begin
 Result := FLocateFieldFilters.Prior;
 if Result
    then MoveBy(FLocateFieldFilters.FResultRecordsListCurrentRecordIndex - PInteger(ActiveBuffer)^);
end;

function TSqlitePassDataset.LocateLast: Boolean;
begin
 Result := FLocateFieldFilters.Last;
 if Result
    then MoveBy(FLocateFieldFilters.FResultRecordsListCurrentRecordIndex - PInteger(ActiveBuffer)^);
end;


{ LocateRecords :
  this function returns a list of records matching a given set of filters values.
  The binary search is always based on the current recordset sort order, because
  we need to navigate through the FLocateFoundRecords list that holds a subset of indexes
  of the FRecordset.FRecords
  if the recordset is not sorted or the sort order can't be used for a binary search
  then a simple linear search is used.
  if the recordset is modified, the FLocateFoundRecords is cleared and build again if FLocateAutoRefresh is True.}

function TSqlitePassDataset.LocateRecords: Boolean;
begin
  FLocateFieldFilters.InitActiveFilters;
  FLocateFieldFilters.ScanRecords;
  { Moves to the first matching record }
  Result := LocateFirst;
end;

function TSqlitePassDataset.Locate(const KeyFields: String;
                                   const KeyValues: Variant;
                                   Options: TLocateOptions): Boolean;
begin
  FLocateFieldFilters.ParseFilterText(KeyFields, KeyValues, Options);
  Result := LocateRecords;
end;

function TSqlitePassDataset.Locate(const LocateStmt: String;
                                   Options: TLocateOptions): Boolean;
begin
  FLocateFieldFilters.FOptions := Options;
  FLocateFieldFilters.ParseFilterText(LocateStmt);
  Result := LocateRecords;
end;

procedure TSqlitePassDataset.RefreshLocate;
begin
if FLocateSmartRefresh
   then LocateRecords;
end;

function TSqlitePassDataset.GetLocateMoveState: TGetResult;
begin
 Result := FLocateFieldFilters.FMoveState;
end;


{ --------- LookUp --------- }

{Lookup récupère les infos des champs souhaités selon que certains champs
correspondent aux critères de recherche }

function TSqlitePassDataset.GetLookupMoveState: TGetResult;
begin
 Result := FLookupKeyFieldFilters.FMoveState;
end;

function TSqlitePassDataset.LookupFirst(var LookupResult: Variant): Boolean;
begin
 Result := FLookupKeyFieldFilters.First;
 LookupResult := GetLookUpResultValues(Result);
end;

function TSqlitePassDataset.LookupNext(var LookupResult: Variant): Boolean;
begin
 Result := FLookupKeyFieldFilters.Next;
 LookupResult := GetLookUpResultValues(Result);
end;

function TSqlitePassDataset.LookupPrior(var LookupResult: Variant): Boolean;
begin
 Result := FLookupKeyFieldFilters.Prior;
 LookupResult := GetLookUpResultValues(Result);
end;

function TSqlitePassDataset.LookupLast(var LookupResult: Variant): Boolean;
begin
 Result := FLookupKeyFieldFilters.Last;
 LookupResult := GetLookUpResultValues(Result);
end;


function TSqlitePassDataset.GetLookUpResultValues(Const LookupOk: Boolean): Variant;
var
i, RecordIndex: integer;

begin
if LookupOk then
   begin
   RecordIndex := FLookupKeyFieldFilters.FResultRecordsListCurrentRecordIndex;
   Case FLookUpResultFields.Count of
       0 : Result := Null;
       1 : Result := FRecordset.GetFieldAsVariant(RecordIndex, TField(FLookUpResultFields[0]).Tag, TField(FLookUpResultFields[0]));
      else begin
           Result := VarArrayCreate([0, FLookUpResultFields.Count - 1], VarVariant);
           for i := 0 to Pred(FLookUpResultFields.Count)
              do Result[i] := FRecordset.GetFieldAsVariant(RecordIndex, TField(FLookUpResultFields[i]).Tag, TField(FLookUpResultFields[i]));
           end;
       end { Case }    
   end
   else Result := Null;
end;

function TSqlitePassDataset.Lookup(const KeyFields: String; const KeyValues: Variant; const ResultFields: String): Variant;
begin
 Result := LookupEx(KeyFields, KeyValues, ResultFields, 1);
end;

function TSqlitePassDataset.LookupEx(const KeyFields: String; const KeyValues: Variant;
                                     const ResultFields: String;
                                     const LookUpResultMaxRecordCount: Integer = 0): Variant;
var
i: Integer;
LookupFields: TList;
TempLookupFilterText: String;

begin
  Result := Null;
  LookupFields:= TList.Create;
  GetFieldList(LookupFields, KeyFields);
  If LookupFields.Count = 0 then Exit;

  TempLookupFilterText := '';
  if LookupFields.Count > 1
     then begin
          for i := 0 to Pred(LookupFields.Count)
            do TempLookupFilterText := TempLookupFilterText
                                      + TField(LookupFields[i]).FieldName
                                      + '= "' + VarAsType(KeyValues[i], VarString) + '"; ';
          end
     else TempLookupFilterText :=  TField(LookupFields[0]).FieldName + '= "' + VarAsType(KeyValues, VarString) + '"';
  LookupFields.Free;

  Result := LookupEx(TempLookupFilterText, ResultFields, LookUpResultMaxRecordCount);
end;


function TSqlitePassDataset.LookupEx(const LookupStmt: String; const ResultFields: String;
                                     const LookUpResultMaxRecordCount: Integer = 0): Variant;
begin
  if LookupStmt <> ''
     then begin
          FLookupResultFields.Clear;
          GetFieldList(FLookupResultFields, ResultFields);
          FLookupKeyFieldFilters.ParseFilterText(LookupStmt);
          if LookUpResultMaxRecordCount = 0
             then FLookupKeyFieldFilters.FResultRecordsMaxCount := MaxInt
             else FLookupKeyFieldFilters.FResultRecordsMaxCount := LookUpResultMaxRecordCount;
          { Performs the Lookup }
          LookupRecords;
          { Moves to the first matching record }
          LookupFirst(Result);
          end
     else ClearLookup;
end;

procedure TSqlitePassDataset.ClearLookup;
begin
  FLookUpResultFields.Clear;
  FLookupKeyFieldFilters.ResetResultRecords;
end;

procedure TSqlitePassDataset.LookupRecords;
begin
  FLookupKeyFieldFilters.InitActiveFilters;
  FLookupKeyFieldFilters.ScanRecords;

  { Necessary call to notify lookupcontrols to update display }
  DataEvent(deDataSetChange, 0);
end;

procedure TSqlitePassDataset.RefreshLookup;
begin
 if FLookupSmartRefresh
     then LookupRecords;
end;

procedure TSqlitePassDataset.SetFLookUpCache(Const Value: Boolean);
begin
if Active then FDatabase.DatabaseError.RaiseException(Msg2030);
FLookUpCache := Value;
end;

procedure TSqlitePassDataset.ActivateLookUpCache;
var
i: Integer;
begin
if FLookUpCache then
   for i := 0 to Pred(FieldCount) do
       if Fields[i].FieldKind = fkLookup
          then Fields[i].LookupCache := True;
end;


{ Calculated Fields }

{ Calulated Fields -> (Enable sorting, locating on calculated Fields) --- }
procedure TSqlitePassDataset.SetFCalcDisplayedRecordsOnly(const Value: Boolean);
begin
 if Active then FDatabase.DatabaseError.RaiseException(Msg2049);
 FCalcDisplayedRecordsOnly := Value;
 if Not Value
    then FSortMode := smDirect;
end;

Procedure TSqlitePassDataset.FillCalcFields(Const RecordIndexStart, RecordIndexStop: Integer);
var
i: Integer;
SavedState: TDataSetState;

begin
if Assigned(OnCalcFields) then
   begin
   DisableControls;
   Try
     SavedState := SetTempState(dsFilter);
     FDatabase.ShowBusyScreenCursor;
     For i := RecordIndexStart to RecordIndexStop do
       begin
       FRecordset.CurrentRecordIndex := i;
       OnCalcFields(Self);
       end;
   finally
     FRecordset.CurrentRecordIndex := -1;
     RestoreState(SavedState);
     EnableControls;
     FDatabase.RestoreScreenCursor;
   end;
   end;
end;

{ Lookup Fields -> (Enable sorting, locating on lookup Fields) --- }

{ Note : With D4, lookup keyfield cannot be an int64 value because variant cannot return int64
  so we don't override TDataset.Lookup and use our own implementation  }

procedure TSqlitePassDataset.SetFLookupFieldsDisplayedRecordsOnly(const Value: Boolean);
begin
 if Active then FDatabase.DatabaseError.RaiseException(Msg2047);
 FLookupFieldsDisplayedRecordsOnly := Value;
 if Not Value
    then FSortMode := smDirect;
end;

{ FillLookupFields gets the lookupfield(s).ResultField values for a range of records.
  It always try to use an index (FLookupFieldIndex) generated automatically from the Field.LookupKeyFields.
  If the number of records to fill is > 1 then the index is always generated.
  If we just want to lookup one record and the index is not already set, then we dont't spend time
  building a new index. Instead, we use a linear search.}

Procedure TSqlitePassDataset.FillLookupFields(Const RecordIndexStart, RecordIndexStop: Integer);
var
i, j, k, LookupFieldIndex: Integer;
Str: AnsiString;
WideStr: UTF16WideString;
FieldValueBuffer, LookupFieldValueBuffer: PRecBuffer;
Field: TField;
LookupDataset: TSqlitePassDataset;
LookupRecordset: TSqlitePassRecordset;
LookupFilter: TSqlitePassFieldFilter;
begin
if FLookupFieldsList.Count > 0 then
   begin
   Try
   FDatabase.ShowBusyScreenCursor;
   for i := 0 to Pred(FLookupFieldsList.Count) do
     begin
     Field := TField(FLookupFieldsList[i]);
     LookupDataset := TSqlitePassDataset(Field.LookupDataset);

     if Not LookupDataset.Active
        then LookupDataset.Open;

     { Rebuild the lookup indexes if necessary }
     LookupDataset.InitFieldLookupKeyFieldFilter(Field);
     LookupRecordset := LookupDataset.FRecordset;
     LookupFieldIndex := LookupDataset.FieldByName(Field.LookupResultField).Tag;
     { We scan the recordset and lookup the value for the current LookupField }
     for j := RecordIndexStart to RecordIndexStop do
       begin
       { Set the FieldLookupKeyFieldFiltersValues}
       for k := 0 to Pred(LookupDataset.FLookupFieldsKeyFieldFilters.FActiveFilters.Count) do
         begin
         LookupFilter := LookupDataset.FLookupFieldsKeyFieldFilters.FActiveFilters[k];
         if LookupFilter.FCanGetValueFromRecordBuffer
            then LookupFilter[0].FValue := FRecordset.GetFieldValueBuffer(j, LookupFilter.FKeyFieldIndex)
            else LookupFilter[0].Value  := FRecordset.GetFieldAsVariant(j,
                                           LookupFilter.FKeyFieldIndex,
                                           LookupFilter.Field);
         end;

       LookupFieldValueBuffer := LookupDataset.GetLookupFieldBuffer(Field, LookupFieldIndex);
       FieldValueBuffer := FRecordset.GetFieldValueBuffer(j, Field.Tag);

       { Fill the LookupField value }
      If (Assigned(LookupFieldValueBuffer) and LookupRecordset.FieldIsNull(LookupFieldValueBuffer))
         or Not Assigned(LookupFieldValueBuffer)
         { Null Value or Error ? }
         then begin
              case Field.DataType of
                   ftString, ftFixedChar: FRecordset.ClearAnsiString(FieldValueBuffer);
                   ftWideString : FRecordset.ClearWideString(FieldValueBuffer);
                   else FRecordset.SetFieldNullValue(FieldValueBuffer, True);
                   end;
              end
         else begin
              case Field.DataType of
                   ftBoolean    : Move(LookupFieldValueBuffer^, FieldValueBuffer^, SizeOf(WordBool));
                   ftWord,
                   ftSmallint,
                   ftInteger,
                   ftDate,
                   ftTime       : Move(LookupFieldValueBuffer^, FieldValueBuffer^, SizeOf(Integer));

                   ftAutoInc,
                   ftLargeInt   : Move(LookupFieldValueBuffer^, FieldValueBuffer^, SizeOf(Int64));

                   ftBCD, ftCurrency,
                   ftFloat, ftDateTime
                                : Move(LookupFieldValueBuffer^, FieldValueBuffer^, SizeOf(Double));

                   ftString, ftFixedChar
                                : begin
                                  Str := LookupRecordset.GetFieldAsAnsiString(LookupFieldValueBuffer);
                                  FRecordset.SetFieldAsAnsiString(FieldValueBuffer, Str);
                                  end;

                   ftWideString
                                : begin
                                  WideStr := LookupRecordset.GetFieldAsAnsiString(LookupFieldValueBuffer);
                                  FRecordset.SetFieldAsWideString(FieldValueBuffer, WideStr);
                                  end;
                   end; { Case }
               FRecordset.SetFieldNullValue(FieldValueBuffer, False);
               end
       end;
     end;
   finally
   FDatabase.RestoreScreenCursor;
   end;
end;
end;


{ This procedure is called from the dataset which owns the lookup field
  so, FieldByName or similar functions used here refer to the Field.LookupDataset}
procedure TSqlitePassDataset.InitFieldLookupKeyFieldFilter(Const Field: TField);
var
i : Integer;
LookupKeyFields, KeyFields: TList;
TempLookupFilterText: String;
KeyField, LookupKeyField: TField;
LookupKeyFieldFilter: TSqlitePassFieldFilter;
begin

  KeyFields:= TList.Create;
  LookupKeyFields:= TList.Create;

  Try
  Field.Dataset.GetFieldList(KeyFields, Field.KeyFields);
  GetFieldList(LookupKeyFields, Field.LookupKeyFields);

  if (LookupKeyFields.Count = 0) or (KeyFields.Count <> LookupKeyFields.Count) then Exit;

  TempLookupFilterText := '';
  FLookupFieldsKeyFieldFilters.ParseFilterText(TempLookupFilterText);

  { Prepares the Filter statement with "0" as the default filter value.
    This value will be changed when we iterate trough records to get the keyfields values }
  for i := 0 to Pred(LookupKeyFields.Count)
      do TempLookupFilterText := TempLookupFilterText + '"'+TField(LookupKeyFields[i]).FieldName + '" = "0";';

  if TempLookupFilterText <> '' then
     begin
     FLookupFieldsKeyFieldFilters.ParseFilterText(TempLookupFilterText);
     for i := 0 to Pred(KeyFields.Count) do
        begin
        KeyField := TField(KeyFields[i]);
        LookupKeyField := TField(LookupKeyFields[i]);
        LookupKeyFieldFilter := FLookupFieldsKeyFieldFilters.FilterByFieldName(LookupKeyField.FieldName);
        LookupKeyFieldFilter.SetKeyField(KeyField);
        end;
     end;

  FLookupFieldsKeyFieldFilters.InitActiveFilters;
  FLookupFieldsKeyFieldFilters.InitBinaryScan;

  finally
  KeyFields.Free;
  LookupKeyFields.Free;
  end;
end;


function TSqlitePassDataset.GetLookupFieldBuffer(Const Field: TField;
                                                 Const LookupFieldIndex: Integer): PRecBuffer;
begin
  Result := nil;
  With FLookupFieldsKeyFieldFilters do
  begin
  ResetResultRecords;
  BinaryScanRecords;
  LinearScanRecords;
  if First
     then Result := FRecordset.GetFieldValueBuffer(FRecordset.FRecords[FResultRecordsListCurrentRecordIndex],
                                                   LookupFieldIndex);
  end;                                                 
end;


{ ----- Indexing ----- }

procedure TSqlitePassDataset.SetFInMemoryIndexed(const Value: Boolean);
begin
   FInMemoryIndexed := Value;
end;

procedure TSqlitePassDataset.SetFInMemoryIndexedBy(const Value: String);
begin
if Value <> FInMemoryIndexedBy then
   begin
   FInMemoryIndexedBy := Value;
   if FInMemoryIndexedBy = ''
      then Indexed := False;
   if Active
      then FInMemoryIndexes.SetSelectedIndexes;   
   end;
end;

Procedure TSqlitePassDataset.Index;
var
WasIndexed:  Boolean;
Begin
 WasIndexed := FInMemoryIndexed;
 { Indexes the dataset }
 Indexed := True;
 { Back to previous state }
 FInMemoryIndexed := WasIndexed;
End;


{ ----- Sorting ----- }

procedure TSqlitePassDataset.SetFSortMode(const Value: TSqlitePassSortMode);
begin
  FSortMode := Value;
  If FSortMode = smSQL then
     begin
     FLookupFieldsDisplayedRecordsOnly := True;
     FCalcDisplayedRecordsOnly := True;
     end;
end;

procedure TSqlitePassDataset.SetFSorted(const Value: Boolean);
begin
  FSorted := Value;
  if FSorted
     then RefreshActive;
end;

procedure TSqlitePassDataset.SetFSortedBy(const Value: String);
begin
if Value <> FSortedBy then
   begin
   FSortedBy := Value;
   if FSortedBy = ''
      then begin
           FSorted := False;
           SetLength(FSortedFields,0);
           end
      else Include(FSQLChanges, scOrderBy);
   if FSorted
      then RefreshActive;
   end;
end;

Procedure TSqlitePassDataset.Sort;
var
WasSorted:  Boolean;
Begin
 WasSorted := FSorted;
 { Sorts the dataset }
 Sorted := True;
 { Back to previous state }
 FSorted := WasSorted;
End;

{ Creates a array of sorted fields from a 'sortedby' string } 
procedure TSqlitePassDataset.GetSortedFields(Const SortedByStmt: String;
                                             var SortedFieldsArray: TSqlitePassInMemoryIndexInfos);
var
SqlStmt: TSqlitePassSqlStmt;
i: Integer;
begin
i := 0;
{ Allocate enough space for 99 Sorted fields }
SetLength(SortedFieldsArray,0);
SetLength(SortedFieldsArray,99);
SqlStmt := TSqlitePassSqlStmt.Create(FDatabase);
SqlStmt.Tokenizer.Text := SortedByStmt;

if SqlStmt.Tokenizer.Locate(ttIdentifier) then
   begin
   While Not SqlStmt.Tokenizer.EOF do
    begin
    with SortedFieldsArray[i] do
          begin
          Field := FieldByName(SqlStmt.Tokenizer.Token.Text);
          FieldValueOffset := FRecordset.FInternalFieldsValueOffset[Field.Tag];
          if SqlStmt.Tokenizer.Next(ttIdentifier)
             then Ascending := Uppercase(SqlStmt.Tokenizer.Token.Text) = 'ASC'
             else Ascending := True;
          ValueComparator := GetValueComparator(Field.DataType, Ascending);
          end;
    SqlStmt.Tokenizer.Next(ttIdentifier);
    Inc(i);
    end;
end;

{ Downsize the array }
SetLength(SortedFieldsArray, i);
SqlStmt.Free;
end;


function TSqlitePassDataset.GetValueComparator(Datatype: TFieldType; Const Ascending: Boolean): TSqlitePassCompareColumnValue;
begin
Result := @CompareDummyValue;

if Ascending
    then Case Datatype of
              ftBoolean :
              Result := @CompareWordBoolValueAsc;

              ftWord, ftSmallint, ftInteger, ftDate, ftTime :
              Result := @CompareIntegerValueAsc;

              ftAutoInc, ftLargeInt :
              Result := @CompareInt64ValueAsc;

              ftCurrency, ftFloat, ftDateTime :
              Result := @CompareDoubleValueAsc;

              ftBCD:
              Result := @CompareCurrencyValueAsc;

              ftString, ftFixedChar :
              Result := @CompareAnsiStringValueAsc;

              ftWideString :
              Result := @CompareWideStringValueAsc;

              ftMemo, ftFmtMemo {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF}:
              Result := @CompareMemoValueAsc;
              end { Case }
    else Case Datatype of
              ftBoolean :
              Result := @CompareWordBoolValueDesc;

              ftWord, ftSmallint, ftInteger, ftDate, ftTime :
              Result := @CompareIntegerValueDesc;

              ftAutoInc, ftLargeInt :
              Result := @CompareInt64ValueDesc;

              ftCurrency, ftFloat, ftDateTime :
              Result := @CompareDoubleValueDesc;

              ftBCD :
              Result := @CompareCurrencyValueDesc;

              ftString, ftFixedChar :
              Result := @CompareAnsiStringValueDesc;

              ftWideString :
              Result := @CompareWideStringValueDesc;

              ftMemo, ftFmtMemo {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF}:
              Result := @CompareMemoValueDesc;
              end; { Case }
end;

Procedure TSqlitePassDataset.SortRecords;
begin
if FSorted then
   begin
   GetSortedFields(FSortedBy, FSortedFields);
   FRecordset.SortRecords(FSortedFields, FRecordset.FRecords);
   Exclude(FSQLChanges, scOrderBy);
   end;
end;

{ ----- Sorting - END ----- }


{ Returns a Unique field name, adding a number if necessary to duplicate names }
function TSqlitePassDataset.GetUniqueFieldName(FieldDef: TSqlitePassSelectStmtFieldDef): String;

 function AddUniqueNumber(FieldName: String): String;
 var
 i: Integer;
 NewName: String;
 begin
 i := 0;
 Result := FieldDef.FieldName;
 if FieldDefs.IndexOf(FieldName) <> -1
    then begin
         Repeat
         Inc(i);
         NewName := FieldName + '_' + IntToStr(i);
         Until FieldDefs.IndexOf(NewName) = -1;
         Result := NewName;
         end;
 end;
 
begin
Result := FieldDef.FieldName;
//TODO - Check
FSQLSelectStmt.unquotestring(Result);
if FieldDefs.IndexOf(Result) <> -1
   then Result := FieldDef.FieldFullName;
if FieldDefs.IndexOf(Result) <> -1
   then Result := AddUniqueNumber(FieldDef.FieldFullName);

end;


{ ----- WriteMode ----- }

procedure TSqlitePassDataset.SetFWriteMode(Const Value: TSqlitePassWriteMode);
begin
if Active then FDatabase.DatabaseError.RaiseException(Msg2033);
FWriteMode := Value;
end;


procedure TSqlitePassDataset.ApplyChanges;
var
i, j: Integer;
RecBuffer: PRecBuffer;
begin
if WriteMode <> wmPostponed then Exit;

FDatabase.FEngine.Transaction.Start;
try
For i :=0 to Pred(FRecordset.FRecordsMemoryBlocs.Count) do
  begin
  RecBuffer := PRecBuffer(FRecordset.FRecordsMemoryBlocs[i]);
  For j := 0 to Pred(FRecordset.FRecordMemoryBlocCapacity) do
    begin
    if FRecordset.GetRecordState(RecBuffer, rsEnabledFlag, rsEnabled) then
       begin
       if FRecordset.GetRecordState(RecBuffer, rsInsertedFlag, rsInserted)
          then FSQLSelectStmt.Post(ptInsert, RecBuffer)
          else if FRecordset.GetRecordState(RecBuffer, rsModifiedFlag, rsModified)
                  then FSQLSelectStmt.Post(ptUpdate, RecBuffer);
       if FRecordset.GetRecordState(RecBuffer, rsDeletedFlag, rsDeleted)
          then FSQLSelectStmt.Post(ptDelete, RecBuffer);
       end;
    FRecordset.ResetRecordState(RecBuffer);
    { Move to the next record on the Memory Bloc Address }
    Inc(RecBuffer, FRecordset.FRecordSize);
    end; {j}
  end; {i}
FDatabase.FEngine.Transaction.Commit;
except
FDatabase.FEngine.Transaction.Rollback;
end;
end;

procedure TSqlitePassDataset.EmptyTable;
begin
  if DatasetType = dtTable then
     begin
     FDatabase.Engine.ExecSQL('DELETE FROM "' + DatasetName + '";');
     Refresh;
     end;
end;

{ ----- Queries params ----- Begin }

Procedure TSqlitePassDataset.SetParamsList(Value: TParams);
begin
 FParams.AssignValues(Value);
end;

Procedure TSqlitePassDataset.UpdateParamsList;
var
 i, ParamCount: integer;
 NewParamsList: TParams;
 ParamName: String;
begin
if not (csReading in ComponentState) then
   if ParamCheck or (csDesigning in ComponentState) then
      begin
      NewParamsList := TParams.Create(Self);
      try
          ParamCount := SqliteDbv3_bind_parameter_count(FSQLSelectStmt.StmtHandle);
          for i := 0 to Pred(ParamCount) do
              begin
              ParamName := SqliteDbv3_bind_parameter_name(FSQLSelectStmt.StmtHandle, i);
              NewParamsList.CreateParam(ftString, ParamName, ptInput);
             //TODO NewParamsList.ParseSQL(FSQLSelectStmt.SQL, True);
              end;
          NewParamsList.AssignValues(FParams);
          FParams.Clear;
          FParams.Assign(NewParamsList);
      finally
          NewParamsList.Free;
      end;
    end;
end;

Function TSqlitePassDataset.GetParamsCount: Integer;
begin
 Result := FParams.Count;
end;

Procedure TSqlitePassDataset.DefineProperties(Filer: TFiler);
  Function WriteData: Boolean;
  begin
      if Filer.Ancestor <> nil then
          Result := not FParams.IsEqual(TSqlitePassDataset(Filer.Ancestor).FParams)
      else
          Result := FParams.Count > 0;
  end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

Procedure TSqlitePassDataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

Procedure TSqlitePassDataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{ ----- Queries Params ----- End }

procedure TSqlitePassDataset.FreeRecordPointers(Buffer: pAnsiChar);
begin
{ TODO : Doesn't seem necessary to implement this procedure
  since no call was found to this procedure in VCL  }
end;

procedure TSqlitePassDataset.AllocateBLOBPointers(Buffer: pAnsiChar);
begin
{ TODO : Doesn't seem necessary to implement this procedure
  since no call was found to this procedure in VCL  }
end;

procedure TSqlitePassDataset.FreeBlobPointers(Buffer: pAnsiChar);
begin
{ TODO : TSqlitePassDataset.FreeBlobPointers
  Doesn't seem necessary to implement this procedure
  since Blobs are cleared by TSqlitePassMemRecord.Clear
  and no call was found to this procedure in VCL  }
end;

Procedure TSqlitePassDataset.ClearCalcFields(Buffer: PRecBuffer);
var
i: Integer;
begin
For i := 0 to Pred(FCalcFieldsList.Count)
    do FRecordset.SetFieldNullValue(PInteger(Buffer)^, TField(FCalcFieldsList[i]).Tag, True);
{$IFDEF DEBUG_SQLITEPASS}
FDatabase.Debugger.Display('[ --------- ClearCalcFields ------- ]');
{$ENDIF}
end;

{$IFNDEF HasFmtBcd}
function TSqlitePassDataset.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TSqlitePassDataset.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;
{$ENDIF}

Function TSqlitePassDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
Begin
  Result:=TSqlitePassBlobStream.Create(TBlobField(Field),Mode);
End;

procedure TSqlitePassDataset.CloseBlob(Field: TField);
begin
{ TSqlitePassDataset.CloseBlob
  This procedure doesn't need to be implemented
  Blobs are kept in memory and freed by TSqlitePassRecordset }
end;


{ TSqlitePassDatasetIndex }

constructor TSqlitePassDatasetIndex.Create(Owner: TSqlitePassDatasetIndexDefs);
begin
inherited Create;
FIndexDefs := Owner;
end;

destructor TSqlitePassDatasetIndex.Destroy;
begin
FIndexDefs := nil;
inherited Destroy;
end;

procedure TSqlitePassDatasetIndex.DeleteIndex;
begin
 FindexDefs.FDataset.Database.Engine.ExecSQL('Drop index ' + FIndexName);
 FIndexDefs.FDataset.FDatabase.IndexDefs.Refresh;
end;

procedure TSqlitePassDatasetIndex.Reindex;
begin
 FIndexDefs.FDataset.FDatabase.Engine.ExecSQL('Reindex ' + FIndexName);
end;


{ TSqlitePassDatasetIndexDefs }

constructor TSqlitePassDatasetIndexDefs.Create(Owner: TSqlitePassDataset);
begin
 inherited Create;
 FDataset := Owner;
end;

destructor TSqlitePassDatasetIndexDefs.Destroy;
begin
 FDataset := nil;
 inherited Destroy;
end;

function TSqlitePassDatasetIndexDefs.GetItem(Index: Integer): TSqlitePassIndex;
begin
 Result:=TSqlitePassIndex(Inherited Items[Index]);
end;

procedure TSqlitePassDatasetIndexDefs.SetItem(Index: Integer;
  const Value: TSqlitePassIndex);
begin
 Put(Index,Value);
end;

procedure TSqlitePassDatasetIndexDefs.RefreshFromCache;
var
i: Integer;
TableName: String;
begin
{ Clears only the list, not the items which are pointing to Database indexdefs }
Clear;
TableName := FDataset.FDatasetName;
if TableName <> ''
   then begin
        for i := 0 to Pred(FDataset.FDatabase.IndexDefs.count)
         do begin
            if AnsiCompareText(FDataset.FDatabase.IndexDefs[i].TableName, TableName) = 0
               then Add(FDataset.FDatabase.IndexDefs[i]);
            end;
   end;
end;


{ TBlobStream }

Constructor TSqlitePassBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
var
FBlobFieldIndex: Integer;
FieldValueBuffer: PRecBuffer;
Begin
 FField := Field;
 FDataset := TSqlitePassDataset(FField.DataSet);
 FActiveRecIndex := FDataset.GetActiveRecord;
 if FActiveRecIndex = -1 then Exit;

 FieldValueBuffer := FDataset.FRecordset.GetFieldValueBuffer(FActiveRecIndex, Field.Tag);
 if FDataset.FRecordset.FieldIsNull(FieldValueBuffer)
    then FBlobFieldIndex := FDataset.FRecordset.AddMemoryStream(FieldValueBuffer)
    else FBlobFieldIndex := PInteger(FieldValueBuffer)^;

 FRecBlobStream := TMemoryStream(FDataset.FRecordset.FMemoryStreams[FBlobFieldIndex]);
 FOpened := True;

 Case Mode of
      bmRead  : LoadFromStream(FRecBlobStream);
      bmWrite : begin
                FRecBlobStream.Clear;
                FModified := True;
                end;
      end;
end;

Destructor TSqlitePassBlobStream.Destroy;
var
FieldValueBuffer: PRecBuffer;
begin
 if FModified then
   try
     SaveToStream(FRecBlobStream);
     FieldValueBuffer := FDataset.FRecordset.GetFieldValueBuffer(FActiveRecIndex, FField.Tag);
     if FRecBLobStream.Size = 0
        then FDataset.FRecordset.SetFieldNullValue(FieldValueBuffer, True)
        else FDataset.FRecordset.SetFieldNullValue(FieldValueBuffer, False);
     FDataset.FRecordset.SetFieldWasModifiedValue(FieldValueBuffer, True);
     FField.Modified := True;
     FDataSet.DataEvent(deFieldChange, Longint(FField));
     FOpened := False;
     FModified := False;
   Except
     Application.HandleException(Self);
   End;
inherited Destroy;
end;

function TSqlitePassBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
 Result := 0;
 if FOpened
    then Result := inherited Read(Buffer, Count);
end;

Function TSqlitePassBlobStream.Write(const Buffer; Count: Longint): Longint;
Begin
 Result := 0;
 if FOpened then
    begin
    Result := inherited Write(Buffer, Count);
    FModified := True;
    end;
end;





