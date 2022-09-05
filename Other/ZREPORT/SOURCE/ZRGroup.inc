{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                              TZRGroupBands                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRGroupBands.Create(aOwner: TZRGroup);
begin
  inherited Create;
  fOwner := aOwner;
end;

function TZRGroupBands.GetHasBand(BandType: TZRBandType): Boolean;
begin
  Result := Owner.GetBand(BandType) <> nil;
end;

procedure TZRGroupBands.SetHasBand(BandType: TZRBandType; Value: Boolean);
begin
  if (Owner.Master <> nil) and (GetHasBand(BandType) <> Value) then begin
    if Value then begin
      Owner.SetBand(BandType, Owner.Master.CreateBand(BandType, Owner));
      Owner.Master.SortBands;
    end else
      Owner.GetBand(BandType).Free;
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                 TZRGroup                               !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRGroup.Create(aOwner: TComponent);
begin
  inherited;
  fBands   := TZRGroupBands.Create(Self);
  fEnabled := True;
end;

destructor TZRGroup.Destroy;
begin
  SetMaster(nil);
  inherited;
  Header.Free;
  //fHeader := nil;
  Footer.Free;
  //fFooter := nil;
  Bands.Free;
  //fBands := nil;
end;

function TZRGroup.GetBand(BandType: TZRBandType): TZRCustomBand;
begin
  case BandType of
    zbtGroupHeader: Result := fHeader;
    zbtGroupFooter: Result := fFooter;
    else            Result := nil;
  end;
end;

procedure TZRGroup.SetBand(BandType: TZRBandType; Value: TZRCustomBand);
begin
  case BandType of
    zbtGroupHeader: fHeader := Value;
    zbtGroupFooter: fFooter := Value;
  end;
  if (Value <> nil) then Value.FreeNotification(Self);
end;

procedure TZRGroup.SetVariable(Value : TZRVariable);
begin
  if Variable <> Value then begin
    fVariable := Value;
    if Variable <> nil then Variable.FreeNotification(Self);
  end;
end;

procedure TZRGroup.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if aComponent = fHeader then fHeader := nil else
    if aComponent = fFooter then fFooter := nil else
    if aComponent = fVariable then fVariable := nil;
  end;
end;

procedure TZRGroup.Notify(Operation: TZROperation);
begin
  if Assigned(Master) and not (csDestroying in Master.ComponentState) then
    Master.Perform(ZR_NOTIFY, Integer(Operation), Integer(Self));
end;

procedure TZRGroup.SetName(const Value: TComponentName);
var
  Old,
  New: TComponentName;
begin
  Old := Name;
  inherited;
  New := Name;
  Notify(zopRename);
  RenameComponent(Header, Old, New);
  RenameComponent(Footer, Old, New);
end;

procedure TZRGroup.SetMaster(Value : TZRCustomController);
begin
  if Master <> Value then begin
    Notify(zopRemove);
    if Assigned(Master) then Master.RemoveGroup(Self);
    fMaster := Value;
    if Assigned(Master) then Master.AddGroup(Self);
    Notify(zopInsert);
  end;
end;

function TZRGroup.GetOrder: Integer;
begin
  if Master <> nil then
    Result := Master.GroupList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TZRGroup.SetOrder(Value: Integer);
begin
  if (Master <> nil) and (GetOrder <> Value) then begin
    Master.MoveGroup(Order, Value);
    Notify(zopSort);
  end;
end;

function TZRGroup.HasParent : Boolean;
begin
  Result := True;
end;

function TZRGroup.GetParentComponent: TComponent;
begin
  Result := Master;
end;

procedure TZRGroup.SetParentComponent(Value: TComponent);
begin
  if Value is TZRCustomController then
    SetMaster( TZRCustomController(Value) );
end;

procedure TZRGroup.Prepare;
begin
  fStarted  := Assigned(Variable);
  fFinished := False;
end;

procedure TZRGroup.Unprepare;
begin
  Clear;
end;

procedure TZRGroup.Clear;
begin
  fStarted  := False;
  fFinished := False;
end;

procedure TZRGroup.Reset;
var
  i : Integer;
begin
  if Finished then begin
    fStarted  := Assigned(Variable);
    fFinished := False;
    with Master.VariableList do
      for i := 0 to Count-1 do
        if TZRVariable(Items[i]) is TZRAggregator then
          with TZRAggregator(Items[i]) do begin
            ResetValues(Order+1);
            UpdateValues(Order+1, Value);
          end;
  end;
end;

procedure TZRGroup.Finish;
begin
  fFinished := Assigned(Variable);
  with Master.GroupList do
    if Order < Count-1 then
      TZRGroup(Items[Order+1]).Finish;
end;

function TZRGroup.Check : Boolean;
begin
  Result := Enabled and (Started or Assigned(Variable)) and not Variable.IsRepeated;
end;

