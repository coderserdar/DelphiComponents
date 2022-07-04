{*********************************************************************************************
***********************************************************************************************}

function GetRecordNulls(const OwnerDS : TMyDBCustomDataSet; const Target : PRecordBuffer) : PBooleans; {$IFDEF FPC} inline; {$ENDIF}
begin
  result := PBooleans( @(Target^.Data[OwnerDS.FRecordSize]));
end;

function GetRecordCalcs(const OwnerDS : TMyDBCustomDataSet; const Target : PRecordBuffer) : PBytes; {$IFDEF FPC} inline; {$ENDIF}
begin
  result := PBytes(@(PBytes(Target)^[OwnerDS.FRecordBufferSize - OwnerDS.CalcFieldsSize]));
end;

procedure AssignOldBuffer(const OwnerDS : TMyDBCustomDataSet;
                          var Target : tRecordBuffer;
                          const OldBuffer : PRecordBuffer);  {$IFDEF FPC} inline; {$ENDIF}
begin
  if assigned(Target.OldBuffer) then
    OwnerDS.ReleaseOldBuffer(Target);
  inc(OldBuffer^.References);
  Target.OldBuffer := OldBuffer;
end;

procedure ReferenceOldBuffer(var Target : tRecordBuffer);  {$IFDEF FPC} inline; {$ENDIF}
begin
  if assigned(Target.OldBuffer) then
  begin
    inc(Target.OldBuffer^.References);
  end;
end;

