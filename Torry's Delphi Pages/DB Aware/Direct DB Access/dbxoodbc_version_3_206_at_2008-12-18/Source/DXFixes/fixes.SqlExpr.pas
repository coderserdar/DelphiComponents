[...]
// QC: 6315, 5867.
function TSQLConnection.CloneConnection: TSQLConnection;
var
  SelfParent: TSQLConnection;
  I: Integer;
  {+}//Correction connection string for Cloned Connection
  Status: SQLResult;
  buf : string;
  Len : smallint;
  {+.}
begin      // do not allow nested clones
  if Self.FIsCloned then
    SelfParent := Self.FCloneParent
  else
    SelfParent := Self;
  Result := TSQLConnection.Create(nil);
  Result.FIsCloned := True;
  Result.FLoadParamsOnConnect := False;
  Result.LoginPrompt := False;
  Result.FDriverName := SelfParent.FDriverName;
  Result.FConnectionName := SelfParent.FConnectionName;
  Result.Name := SelfParent.Name + 'Clone1';
  Result.FParams.AddStrings(SelfParent.FParams);
  Result.FGetDriverFunc := SelfParent.FGetDriverFunc;
  Result.FLibraryName := SelfParent.FLibraryName;
  Result.FVendorLib := SelfParent.VendorLib;
  {+}
  if Self.Connected and ((SelfParent.FMaxStmtsPerConn>0)) then
  begin
    Len := 0;
    Status := SelfParent.FISQLConnection.getOption(eConnConnectionName, nil, 0, Len);
    if (Status <> 0) or (Len <= 0) then
      Len := 1024;
    SetLength(buf, Len);
    FillChar(buf[1], Len, #0);
    Status := SelfParent.FISQLConnection.getOption(eConnConnectionName, PChar(buf), Len, Len);
    if (Status = 0) and ( Len > 0 ) then
      Result.Params.Values[DATABASENAME_KEY] := PChar(buf);
  end;
  {+.}
  Result.Connected := Self.Connected;
  Result.FCloneParent := SelfParent;
  for I := 0 to FMonitorUsers.Count -1 do
    TSQLMonitor(FMonitorUsers[I]).SwitchConnection( Result );
  {+}
  Result.FTableScope := SelfParent.TableScope;
  {+.}
end;
[...]
function TCustomSQLDataSet.GetQueryFromType: string;
var
  STableName : String;
begin
  case CommandType of
     ctTable:
       begin
         if Self.FSchemaName <> '' then
           STableName := AddQuoteCharToObjectName(Self, FSchemaName + '.' + FCommandText,
                      FSQLConnection.QuoteChar)
         else
           STableName := AddQuoteCharToObjectName(Self, FCommandText, FSQLConnection.QuoteChar);
         if FSortFieldNames > '' then
           Result := SSelectStarFrom + STableName + SOrderBy + FSortFieldNames
         else
           if FNativeCommand = '' then
             Result := SSelectStarFrom + STableName
           else
           begin
             {+}
             if Self.FSchemaName <> '' then
               STableName := AddQuoteCharToObjectName(Self, FSchemaName + '.' + FNativeCommand, FSQLConnection.QuoteChar)
             else
               STableName := AddQuoteCharToObjectName(Self, FNativeCommand, FSQLConnection.QuoteChar);
             {+.}
             Result := SSelectStarFrom + STableName;
           end;
       end;
     ctStoredProc:
       begin
         if FSchemaName <> '' then
           Result := FSchemaName + '.' + copy(FCommandText, 1, Length(FCommandText))
         else
           Result := copy(FCommandText, 1, Length(FCommandText));
       end;
     else
       if (FSortFieldNames > '') and (Pos(SOrderBy,
          LowerCase(FCommandText)) = 0) then
         Result := FNativeCommand + SOrderBy + FSortFieldNames
       else
         Result := FNativeCommand;
  end;
end;
[...]
// QC: 6316.
// Damage of a code: access abroad the buffer.
// Crash of the application when the name of the database exceeds 256 characters.
// Such long name is possible at usage dbxoodbc.
// The name of database file can be set as a file path.
procedure TCustomSQLDataSet.SetSchemaOption;
var
  Status: SQLResult;
  Len : smallint;
{+}
  buf0,buf1,buf2 : String;
  UserName : String;
  ObjectName : String;
  CatalogName : String;
begin
  Status := 0;
  ObjectName := FSchemaInfo.ObjectName;
  SetLength(buf0, 256);
  SetLength(buf1, 256);
  SetLength(buf2, 256);
  FillChar(buf0[1], Length(buf0), #0);
  FillChar(buf1[1], Length(buf1), #0);
  if ObjectName <> '' then
  begin
    Status := GetInternalConnection.FISQLConnection.setOption(eConnQualifiedName, LongInt(ObjectName));
    if Status <> 0 then
      SQLError(Status, exceptConnection);
    {+}
    try
      Len := 0;
      Status := GetInternalConnection.FISQLConnection.getOption(eConnCatalogName, PChar(buf0), 0, Len);
      if (Status = 0) and (Len > Length(buf0)) then
      begin
        SetLength(buf0, Len);
        FillChar(buf0[1], Length(buf0), #0);
      end;
    except
      //no open odbc driver and it driver not check buffer length or not return required buffer length
    end;
    {+.}
    Status := GetInternalConnection.FISQLConnection.getOption(eConnCatalogName, PChar(buf0), Length(buf0), Len);
    if Status <> 0 then
      SQLError(Status, exceptConnection);
    {+}
    try
      Len := 0;
      Status := GetInternalConnection.FISQLConnection.getOption(eConnSchemaName, PChar(buf1), 0, Len);
      if (Status = 0) and (Len > Length(buf1)) then
      begin
        SetLength(buf1, Len);
        FillChar(buf1[1], Length(buf1), #0);
      end;
    except
      //no open odbc driver and it driver not check buffer length or not return required buffer length
    end;
    {+.}
    Status := GetInternalConnection.FISQLConnection.getOption(eConnSchemaName, PChar(buf1), Length(buf1), Len);
    if Status <> 0 then
      SQLError(Status, exceptConnection);
    FillChar(buf2[1], Length(buf2), #0);
    {+}
    try
      Len := 0;
      Status := GetInternalConnection.FISQLConnection.getOption(eConnObjectName, PChar(buf2), 0, Len);
      if (Status = 0) and (Len > Length(buf2)) then
      begin
        SetLength(buf2, Len);
        FillChar(buf2[1], Length(buf2), #0);
      end;
    except
      //no open odbc driver and it driver not check buffer length or not return required buffer length
    end;
    {+.}
    Status := GetInternalConnection.FISQLConnection.getOption(eConnObjectName, PChar(buf2), Length(buf2), Len);
    if Status <> 0 then
      SQLError(Status, exceptConnection);
    FSchemaInfo.ObjectName := PChar(buf2);
  end;
  if buf0[1] = #0 then // undefined schema name
  begin
    CatalogName := GetInternalConnection.FParams.Values[DATABASENAME_KEY];
    if CatalogName <> '' then
      buf0 := CatalogName;  // *** !!! FIXED: CatalogName can be very long (full name can contain login, network and other options).
  end;
  if buf0[1] <> #0 then // set catalog name option
    Status := GetInternalConnection.FSQLMetaData.setOption(eMetaCatalogName, LongInt(PChar(buf0)));
  if Status <> 0 then
    SQLError(Status, exceptMetaData);
  if (buf1[1] = #0) and (SchemaName <> '') then
    buf1 := SchemaName; // *** FIXED: SchemaName can be very long.
  if buf1[1] = #0 then
  begin
    UserName := GetInternalConnection.FParams.Values[szUSERNAME];
    if UserName <> '' then
      buf1 := UserName; // *** FIXED: UserName can be very long.
  end;
  if buf1[1] <> #0 then // set schema name option
{+.}
    Status := GetInternalConnection.FSQLMetaData.setOption(eMetaSchemaName, LongInt(PChar(buf1)));
  if Status <> 0 then
    SQLError(Status, exceptMetaData);
end;
[...]
