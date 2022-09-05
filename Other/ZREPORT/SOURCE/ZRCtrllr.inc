{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!             TZRControllerCollection & TZRControllerLibrary             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRControllerCollection.Create(aOwner: TZRCustomController);
begin
  inherited Create;
  fOwner := aOwner;
end;

function TZRControllerCollection.InternalFindEntry(const Name: String; var aLibrary: TZRElementLibrary; var aIndex: Integer): Boolean;
begin
  Result := inherited InternalFindEntry(Name, aLibrary, aIndex);
  if not Result and not (Owner is TZReport) then
    Result := Owner.Master.Collection.FindEntry(Name, aLibrary, aIndex); 
end;

function TZRControllerLibrary.GetController: TZRCustomController;
begin
  Result := TZRControllerCollection(Owner).Owner;
end;

function TZRControllerLibrary.GetCount : Integer;
begin
  Result := Controller.VariableList.Count;
end;

function TZRControllerLibrary.GetItem(Index: Integer): String;
var
  V : TZRVariable;
begin
  V := TZRVariable(Controller.VariableList[Index]);
  if (V is TZRField) and Assigned(TZRField(V).DataSet) and (TZRField(V).DataField <> '') then
    Result := TZRField(V).DataSet.Name + '.' + TZRField(V).DataField
  else
    Result := V.Name;
end;

function TZRControllerLibrary.IndexOf(const Name: String): Integer;
var
  i : Integer;
  S : String;
  V : TZRVariable;
begin
  Result := -1;   i := 0;
  S := AnsiUpperCase(Name);
  while (i < Count) and (Result < 0) do begin
    V := TZRVariable(Controller.VariableList[i]);
    if AnsiUpperCase(V.Name) = S then Result := i else
    if (V is TZRField) and (
       (AnsiUpperCase(TZRField(V).DataField) = S) or
       (AnsiUpperCase(TZRField(V).DataSet.Name+'.'+TZRField(V).DataField) = S)) then Result := i;
    Inc(i);
  end;
end;

function TZRControllerLibrary.Calculate(Index : Integer; Arguments: TList): Variant;
begin
  Result := TZRVariable(Controller.VariableList[Index]).Value;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRSubDetail                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRSubDetail.Create(aOwner: TComponent);
begin
  inherited;
  fBands := TZRSubDetailBands.Create(Self);
end;

destructor TZRSubDetail.Destroy;
begin
  inherited;
  fBands.Free;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                           TZRCustomController                          !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRDataOptions.Create;
begin
  inherited Create;
  fRangeBegin := zrbFirst;
  fRangeEnd   := zreLast;
end;             

constructor TZRCustomController.Create(aOwner: TComponent);
begin
  inherited;
  Color    := clController;
  BandType := zbtController;
  fDataOptions  := TZRDataOptions.Create;
  fCollection   := TZRControllerCollection.Create(Self);
  fLibrary      := Collection.RegisterLibrary(TZRControllerLibrary, 'zlController', '', szrVendorZYZ);
  fBandList     := TList.Create;
  fGroupList    := TList.Create;
  fVariableList := TList.Create;
end;

destructor TZRCustomController.Destroy;
begin
  while VariableList.Count > 0 do TZRVariable  (VariableList.Last).Free;
  while GroupList   .Count > 0 do TZRGroup     (GroupList   .Last).Free;
  while BandList    .Count > 0 do TZRCustomBand(BandList    .Last).Free;
  inherited;
  VariableList.Free;
  GroupList   .Free;
  BandList    .Free;
  Collection  .Free;
  DataOptions .Free;
end;

procedure TZRCustomController.AddBand(Band: TZRCustomBand);
begin
  if Assigned(BandList) then BandList.Add(Band);
  SortBands;
end;

procedure TZRCustomController.RemoveBand(Band: TZRCustomBand);
begin
  if Assigned(BandList) then BandList.Remove(Band);
  UpdateBounds;
end;

function TZRCustomController.GetBand(BandType: TZRBandType): TZRCustomBand;
var
  i: Integer;
begin
  Result := nil;
  if (BandType = zbtDetail) and (Self <> Report) then
    Result := Self
  else if Assigned(BandList) then
    for i:= 0 to BandList.Count-1 do
      if TZRCustomBand(BandList[i]).BandType = BandType then begin
        Result := TZRCustomBand(BandList[i]);
        Exit;
      end;
end;

function TZRCustomController.CreateBand(BandType: TZRBandType; Group: TZRGroup): TZRCustomBand;
var
  i   : Integer;
  N, S: TComponentName;
begin
  if BandType = zbtController then
    Result := TZRSubDetail.Create(Self.Owner)
  else
    Result := TZRBand.Create(Self.Owner);
  Result.Parent := Self;   //Report;
  Result.SetBandType(BandType);
  Result.Group  := Group;
  //Result.Master := Self;

  if Result.Group <> nil then begin
    case Result.BandType of
      zbtGroupHeader : N := 'Header';
      zbtGroupFooter : N := 'Footer';
    end;
    N := Group.Name + N;
  end else
    N := Self.Name + BandNameMap[BandType];
  S := '';    i:= 0;
  while Owner.FindComponent(N + S) <> nil do begin
    S:= IntToStr(i);   Inc(i);
  end;
  Result.Name := N + S;
end;

procedure TZRCustomController.SortBands;
var
  i, j: Integer;
  BT  : TZRBandType;

  procedure SortBand(aBand : TZRCustomBand);
  var
    BT : TZRChildBands;
    No : Integer;
  begin
    if aBand <> nil then begin
      No := BandList.IndexOf(aBand);
      if No >= 0 then begin
        BandList.Move(No, i);
        Inc(i);
        for BT := Low(TZRChildBands) to High(TZRChildBands) do
          SortBand(aBand.GetChildLink(BT));
      end;
    end;
  end;

begin
  if (csDestroying in ComponentState) or
     (csLoading    in ComponentState) then Exit;

  i := 0;

  for BT := zbtPageHeader to Pred(zbtGroupHeader) do SortBand(GetBand(BT));
  for j := 0 to GroupList.Count-1 do SortBand(TZRGroup(GroupList[j]).Header);
  for BT := Succ(zbtGroupHeader) to Pred(zbtGroupFooter) do SortBand(GetBand(BT));
  for j := GroupList.Count-1 downto 0 do SortBand(TZRGroup(GroupList[j]).Footer);
  for BT := Succ(zbtGroupFooter) to zbtPageFooter do SortBand(GetBand(BT));


  for i:= 0 to BandList.Count-1 do SetChildOrder(BandList[i], i);
  Notify(zopSort);
  UpdateBounds;
end;

procedure TZRCustomController.AddGroup(Group: TZRGroup);
begin
  GroupList.Add(Group);
  if Group.Header <> nil then Group.Header.Master := Self;
  if Group.Footer <> nil then Group.Footer.Master := Self;
end;

procedure TZRCustomController.RemoveGroup(Group: TZRGroup);
begin
  GroupList.Remove(Group);
end;

procedure TZRCustomController.MoveGroup(Old, New: Integer);
begin
  if Old <> New then begin
    GroupList.Move(Old, New);
    SortBands;
  end;
end;

function TZRCustomController.CreateGroup: TZRGroup;
begin
  Result := TZRGroup.Create(Owner);
  Result.Master := Self;
  SetComponentName(Result, Name + 'Group');
end;

procedure TZRCustomController.AddVariable(Variable: TZRVariable);
begin
  VariableList.Add(Variable);
end;

procedure TZRCustomController.RemoveVariable(Variable: TZRVariable);
begin
  VariableList.Remove(Variable);
end;

function TZRCustomController.CreateVariable(VariableClass: TZRVariableClass; Suffix: String): TZRVariable;
begin
  Result := VariableClass.Create(Owner);
  Result.Master := Self;
  SetComponentName(Result, 'zrv'{Name} + Suffix);
end;

procedure TZRCustomController.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i:= 0 to VariableList.Count-1 do
    if TZRVariable(VariableList[i]).Master = Self then Proc(VariableList[i]);
  for i:= 0 to GroupList.Count-1 do
    if TZRGroup(GroupList[i]).Master = Self then Proc(GroupList[i]);
  for i:= 0 to LabelList.Count-1 do
    if TZReportControl(LabelList[i]).Band = Self then Proc(LabelList[i]);
  for i:= 0 to BandList.Count-1 do
    if TZRCustomBand(BandList[i]).Master = Self then Proc(BandList[i]);
end;

procedure TZRCustomController.SetDataSet(Value : TDataSet);
var
  SaveDS: TDataSet;
  i     : Integer;
begin
  if DataSet <> Value then begin
    SaveDS   := DataSet;
    fDataSet := Value;
    if (csDesigning in ComponentState) then
      for i := 0 to VariableList.Count-1 do
        if (TZRVariable(VariableList[i]) is TZRField) and (TZRField(VariableList[i]).DataSet = SaveDS) then
          TZRField(VariableList[i]).DataSet := DataSet;
    Notify(zopDataSet);
    if (DataSet <> nil) then DataSet.FreeNotification(Self);
  end;
end;

procedure TZRCustomController.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if (aComponent = DataSet) then DataSet := nil;
  end;
end;

procedure TZRCustomController.SetName(const Value: TComponentName);
var
  OldName,
  NewName: String;
  i      : Integer;
begin
  OldName:= Name;
  inherited;
  NewName:= Name;
  if (csDesigning in ComponentState) and not
     (csLoading   in ComponentState) and
     (NewName <> OldName) then begin
    for i:= 0 to VariableList.Count-1 do RenameComponent(TZRVariable  (VariableList[i]), OldName, NewName);
    for i:= 0 to GroupList   .Count-1 do RenameComponent(TZRGroup     (GroupList   [i]), OldName, NewName);
    {if Self <> Report then}
    for i:= 0 to BandList    .Count-1 do RenameComponent(TZRCustomBand(BandList    [i]), OldName, NewName);
  end;
  if Assigned(fLibrary) then begin
    fLibrary.Name        := Name;
    fLibrary.Description := Format(LoadStr(szrControllerLibrary), [Name]);
  end;
end;

procedure TZRCustomController.Prepare;

  procedure DataOpen;
  begin
    if Assigned(DataSet) then begin
      fSaveActive := DataSet.Active;
      if DataOptions.AutoOpen then DataSet.Open;
    end;
    if DataSetOK and fSaveActive then fCurrent := DataSet.Bookmark;
  end;

var
  i : Integer;
begin
  inherited;
  fPrintType   := zbtDetail;
  fColumnState := [];
  fStepState   := [];
  fPrevState   := [];
  fPrintState  := [];
  fPrintList   := TList.Create;
  fSaveActive  := False;
  fCurrent     := '';
  DataOpen;
  for i := 0 to VariableList.Count-1 do
    TZRVariable  (VariableList[i]).Prepare;
  for i := 0 to GroupList   .Count-1 do
    TZRGroup     (GroupList   [i]).Prepare;
  for i := 0 to BandList    .Count-1 do
    TZRCustomBand(BandList    [i]).Prepare;
  fColumnStarted  := False;
  fColumnFinished := False;
end;

procedure TZRCustomController.Unprepare;

  procedure DataClose;
  begin
    if DataSetOK and fSaveActive then try DataSet.Bookmark := fCurrent; except end;
    if Assigned(DataSet) and DataOptions.AutoOpen then DataSet.Active := fSaveActive;
  end;

var
  i : Integer;
begin
  for i := 0 to VariableList.Count-1 do
    TZRVariable  (VariableList[i]).Unprepare;
  for i := 0 to GroupList   .Count-1 do
    TZRGroup     (GroupList   [i]).Unprepare;
  for i := 0 to BandList    .Count-1 do
    TZRCustomBand(BandList    [i]).Unprepare;
  DataClose;
  fPrintList.Free;
  fPrintList := nil;
  inherited;
end;

function TZRCustomController.DataSetOK: Boolean;
begin
  Result := Assigned(DataSet) and (DataSet.Active);
end;

function TZRCustomController.GetRecordCount: Integer;
begin
  if DataSetOK then begin
    Result := fRecordCount;
    if Result <= 0 then
      Result := fRecordNumber;
  end else
    Result := fRecordNumber;
end;

procedure TZRCustomController.Step;
begin
end;

procedure TZRCustomController.Execute;

  procedure StateInclude(var State: TZRControllerState; BandType: TZRBandType);
  begin
    Include(State, BandType);

    if zbtPageHeader   in State then Include(State, zbtColumnHeader);
    if zbtPageFooter   in State then Include(State, zbtColumnFooter);

    if zbtHeader       in State then Include(State, zbtColumnHeader);
    if zbtFooter       in State then Include(State, zbtColumnFooter);

    if (zbtHeader       in State) then Include(State, zbtGroupHeader);
    if (zbtFooter       in State) then Include(State, zbtGroupFooter);

    if (zbtColumnHeader in State) then Include(State, zbtDetailHeader);
    if (zbtColumnFooter in State) then Include(State, zbtDetailFooter);

    if (zbtGroupHeader  in State) then Include(State, zbtDetailHeader);
    if (zbtGroupFooter  in State) then Include(State, zbtDetailFooter);
  end;

  procedure UpdateValues;
  var
    i : Integer;
  begin
    if HasData then begin
      fPrevState := fStepState;
      fStepState := [];
      for i:= 0 to VariableList.Count-1 do
        TZRVariable(VariableList[i]).Clear;
      for i:= 0 to VariableList.Count-1 do
        TZRVariable(VariableList[i]).Update;
      Inc(fRecordNumber);
      StateInclude(fStepState, zbtDetail);
    end else
      StateInclude(fStepState, zbtFooter);
  end;

  procedure DataStart;
  var
    i : Integer;
  begin
    fRecordNumber := 0;
    fRecordCount  := 0;
    fHasData   := False;
    for i:= 0 to VariableList.Count-1 do
      TZRVariable(VariableList[i]).Reset;
    for i:= 0 to GroupList.Count-1 do
      TZRGroup(GroupList[i]).Reset;
    if DataSetOK then begin
      if DataOptions.RangeBegin = zrbFirst then DataSet.First;
      fHasData     := not DataSet.IsEmpty;
      fRecordCount := DataSet.RecordCount;
    end;
    if not (csDesigning in ComponentState) and Assigned(OnDataStart) then OnDataStart(Self, fHasData);
    UpdateValues;
  end;

  procedure DataNext;
  begin
    fHasData := False;
    if DataSetOK then begin
      fHasData := True;
      if DataOptions.RangeEnd = zreCurrent then
        fHasData := DataSet.Bookmark <> fCurrent;
      if fHasData then begin
        DataSet.Next;
        fHasData := not (DataSet.BOF or DataSet.EOF);
      end;
    end;
    if not (csDesigning in ComponentState) and Assigned(OnDataNext) then OnDataNext(Self, fHasData);
    UpdateValues;
  end;

  procedure DataOpen;
  begin
    if Assigned(DataSet) and DataOptions.AutoOpen then DataSet.Open;
  end;

  procedure DataClose;
  begin
    if Assigned(DataSet) and DataOptions.AutoOpen then DataSet.Active := fSaveActive;
  end;

  procedure Undo;
  var
    i: Integer;
  begin
    if HasData then begin
      if DataSetOK then DataSet.Prior;
      for i:= 0 to VariableList.Count-1 do
        TZRVariable(VariableList[i]).Undo;
      Dec(fRecordNumber);
      Swap(fStepState, fPrevState);
    end;
  end;

  procedure Redo;
  var
    i: Integer;
  begin
    if HasData then begin
      if DataSetOK then DataSet.Next;
      for i:= 0 to VariableList.Count-1 do
        TZRVariable(VariableList[i]).Redo;
      Inc(fRecordNumber);
      Swap(fStepState, fPrevState);
    end;
  end;

  procedure DoStart;
  begin
    fStepState   := [];
    fPrintState  := [];
    fColumnState := [zbtDetailFooter,zbtColumnFooter];
    DataOpen;
    DataStart;
    StateInclude(fStepState, zbtHeader);
  end;

  function CheckGroups: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i:= 0 to GroupList.Count-1 do
      if not HasData or TZRGroup(GroupList[i]).Check then begin
        TZRGroup(GroupList[i]).Finish;
        if HasData then StateInclude(fStepState, zbtGroupHeader);
        Result := True;
        Exit;
      end;
  end;

  procedure ResetGroups;
  var
    i: Integer;
  begin
    if HasData then
      for i:= 0 to GroupList.Count-1 do
        TZRGroup(GroupList[i]).Reset;
  end;

  procedure BuildPrintList;
    procedure AddBand(Band: TZRCustomBand);
    begin
      if Assigned(Band) and Band.Enabled then begin
        PrintList.Add(Band);
        Include(fPrintState, Band.PrintType);
      end;
    end;
    procedure AddType(BandType: TZRBandType);
    begin
      if BandType in StepState then AddBand(GetBand(BandType));
    end;
  var
    i : Integer;
  begin
     fPrintState := [];
     PrintList.Clear;
     AddType(zbtPageHeader  );
     AddType(zbtHeader      );
     AddType(zbtColumnHeader);
     if zbtGroupHeader in StepState then begin
       for i := 0 to GroupList.Count-1 do
         with TZRGroup(GroupList[i]) do
           if Enabled and ((zbtHeader in StepState) or Started ) then AddBand(Header);
     end;
     AddType(zbtDetailHeader);
     AddType(zbtDetail      );
     AddType(zbtDetailFooter);
     if zbtGroupFooter in StepState then begin
       for i := GroupList.Count-1 downto 0 do
         with TZRGroup(GroupList[i]) do
           if Enabled and ((zbtFooter in StepState) or Finished) then AddBand(Footer);
     end;
     AddType(zbtColumnFooter);
     AddType(zbtFooter      );
     AddType(zbtPageFooter  );
  end;

  procedure PrintBands;
  var
    Space : TPoint;
    i     : Integer;
    aBand : TZRCustomBand;
  begin
    if Report.Printer.Cancelled then Exit;

    BuildPrintList;

    if (Report.Printer.Options.PaperType <> zptContinuous) then begin

      Space := Report.AvailableSpace;

      CalculatePrintSize;
      Dec(Space.Y, PrintSize.Y);

      if Space.Y < 0 then
        {if not (zbtHeader in PrintState) then }begin
          NewColumn;
          Space := Report.AvailableSpace;
          Dec(Space.Y, PrintSize.Y);
        end else
          {Log.WriteString(IntToStr(Report.PageCount)+': Ќу € попал (не лезут бэнды текущего шага)!!!')};

      CalculateFooterSize(nil);
      Dec(Space.Y, FooterSize.Y);

      if Space.Y < 0 then
        {if not (zbtHeader in StepState) then} begin
          NewColumn
        end else
          {Log.WriteString(IntToStr(Report.PageCount)+': Ќу € попал (не лезут footer`ы)!!!')};
    end;

    for i := 0 to PrintList.Count-1 do begin
      aBand := TZRCustomBand(PrintList[i]);
      Dec(fPrintSize.Y, aBand.Height);
      Report.PrintBand(aBand);
      Exclude(fPrintState , aBand.PrintType);
      PrintList[i] := nil;
    end;
  end;

  procedure DoStep;
  var
    GroupBroken: Boolean;
  begin
    Step;
    DataNext;
    //if DataSetOK then DataSet.DisableControls;
    GroupBroken := CheckGroups;
    Undo;
    if (zbtGroupHeader in fPrevState) then StateInclude(fStepState, zbtGroupFooter);
    PrintBands;
    //if DataSetOK then DataSet.EnableControls;
    Redo;
    if GroupBroken then ResetGroups;
  end;

  procedure DoFinish;
  var
    i: Integer;
  begin
    for i:= 0 to GroupList.Count-1 do
      TZRGroup(GroupList[i]).Clear;
    fStepState   := [];
    fPrintState  := [zbtDetailHeader,zbtColumnHeader];
    fColumnState := [zbtDetailFooter,zbtColumnFooter];
    fRecordNumber := 0;
    fRecordCount  := 0;
    DataClose;
  end;

begin
  if not fIsExecuting and Enabled then
    try
      fIsExecuting := True;
      DoStart;
      if PrintIfEmpty or HasData then
        repeat
          DoStep;
        until Report.Printer.Cancelled or not HasData;
    finally
      DoFinish;
      fIsExecuting := False;
    end;
end;

procedure TZRCustomController.CalculatePrintSize;

  procedure CalculateBand(aBand: TZRCustomBand);
  begin
    if Assigned(aBand) and aBand.Enabled then with aBand do begin
      if Visible then begin
        RequestBounds;
        Inc(fPrintSize.Y, Height);
      end;
      if HasChild then CalculateBand(ChildBand);
    end;
  end;

var
  i : Integer;
begin
  if Assigned(Master) and (zbtFooter in StepState) then
    fPrintSize := Master.PrintSize
  else begin
    fPrintSize.X := 0;
    fPrintSize.Y := 0;
  end;
  for i := 0 to PrintList.Count-1 do
    CalculateBand(TZRCustomBand(PrintList[i]));
end;

procedure TZRCustomController.CalculateFooterSize(Current: TZRCustomBand);

  procedure CalculateBand(aBand: TZRCustomBand);
  begin
    if Assigned(aBand) and (aBand <> Current) then with aBand do
      if Enabled and Visible and (
         not (PrintType in PrintState) and (            // footer не собираютс€ печатать,
             (HeaderMap[PrintType] in PrintState) or    // но его header или собираютс€ печатать
             (HeaderMap[PrintType] in ColumnState)) or  // или уже печатали на этой колонке
         not (PrintType in ColumnState)) then begin     // footer еще не печатали на этой колонке
        RequestBounds;
        Inc(fFooterSize.Y, Height);
      end;
  end;

begin
  if Assigned(Master) then begin
    Master.CalculateFooterSize(nil);
    fFooterSize := Master.FooterSize;
  end else begin
    fFooterSize.X := 0;
    fFooterSize.Y := 0;
  end;
  CalculateBand(GetBand(zbtDetailFooter));
  CalculateBand(GetBand(zbtColumnFooter));
  CalculateBand(GetBand(zbtPageFooter  ));
end;

procedure TZRCustomController.NewColumn;
  procedure PrintHeader(BandType: TZRBandType);
  begin
    if not (BandType in PrintState) then Report.PrintBand(GetBand(BandType));
  end;
var
  i         : Integer;
  SaveState : TZRControllerState;
begin
  if not ColumnStarted then
    try
      fColumnStarted  := True;
      if (Self <> Report) then EndColumn;
      if Assigned(Master) then Master.NewColumn;
      if (Report.Printer.Options.PaperType <> zptContinuous) then begin
        SaveState := fColumnState;
        fColumnState := [];
        PrintHeader(zbtColumnHeader);
        for i := 0 to GroupList.Count-1 do
          with TZRGroup(GroupList[i]) do begin
            if Enabled and (RepeatHeader and not Started) then Report.PrintBand(Header);
          end;
        if (zbtDetail       in PrintState) or
           (zbtDetailFooter in PrintState) then PrintHeader(zbtDetailHeader);
      end;
    finally
      fColumnStarted  := False;
    end;
end;

procedure TZRCustomController.EndColumn;
  procedure PrintFooter(BandType: TZRBandType);
  begin
    if not (BandType in ColumnState) then Report.PrintBand(GetBand(BandType));
  end;
begin
  if not ColumnFinished then
    try
      fColumnFinished := True;
      if (Report.Printer.Options.PaperType <> zptContinuous) then begin
        PrintFooter(zbtDetailFooter);
        PrintFooter(zbtColumnFooter);
      end;
      if Assigned(Master) then Master.EndColumn;
    finally
      fColumnFinished := False;
    end;
end;

procedure TZRCustomController.SetColumnInit(const Value: Boolean);
begin
  if {(Report.Printer.Options.PaperType <> zptContinuous) and}
     (ColumnStarted or ColumnFinished) then
    fColumnInit := False
  else
    fColumnInit := Value;
end;

