[...]
// QC: 7872.
// Correction of double exception in case of a error at fetching rows.
function TDataSetProvider.InternalGetRecords(Count: Integer; out RecsOut: Integer;
  Options: TGetRecordOptions; const CommandText: WideString;
  var Params: OleVariant): OleVariant;
begin
  try
    if grReset in Options then
    begin
      Reset;
      { When doing only a reset and not getting more data then exit }
      if Count = 0 then Exit;
    end;
    if not DataSet.Active then
    begin
      DataSet.Open;
      FDataSetOpened := True;
    end;
    if (Count = 0) or (grMetaData in Options) then
    begin
      FDataDS.Free;
      FDataDS := nil;
      FRecordsSent := 0;
    end;
    DataSet.CheckBrowseMode;
    DataSet.BlockReadSize := Count;
    try
      Result := inherited InternalGetRecords(Count, RecsOut, Options,
        CommandText, Params);
      Inc(FRecordsSent, RecsOut);
      if (RecsOut <> Count) then Reset;
    finally
      if DataSet.Active then
      begin
        DataSet.BlockReadSize := 0;
        {+} // ******* BEGIN *********
        if (Count > 0) and (RecsOut = Count) then
        {+.}// ******** END **********
          DataSet.Next;
      end;
    end;
  except
    Reset;
    raise;
  end;
end;
[...]
