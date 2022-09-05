procedure Swap(var a, b: TZRControllerState); overload;
var
  t : TZRControllerState;
begin
  t := a;   a := b;   b := t;
end;

procedure Swap(var a, b: Integer); overload;
var
  t : Integer;
begin
  t := a;   a := b;   b := t;
end;

procedure Swap(var a, b: Variant); overload;
var
  t : Variant;
begin
  VarCopy(t, b);
  VarCopy(b, a);
  VarCopy(a, t);
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZRVariable                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRVariable.Create(AOwner: TComponent);
begin
  inherited;
  fFormat := TZFormat.Create;
  fStatus := zvsPrepare;
end;

destructor TZRVariable.Destroy;
begin
  Unprepare;
  SetMaster(nil);
  inherited;
  fFormat.Free;
end;

procedure TZRVariable.Notify(Operation: TZROperation);
begin
  if Assigned(Master) and not (csDestroying in Master.ComponentState) then
    Master.Perform(ZR_NOTIFY, Integer(Operation), Integer(Self));
end;

procedure TZRVariable.SetMaster(Value : TZRCustomController);
begin
  if Master <> Value then begin
    Notify(zopRemove);
    if Assigned(Master) then (Master as TZRCustomController).RemoveVariable(Self);
    fMaster := Value;
    if Master <> nil then (Master as TZRCustomController).AddVariable(Self);
    Notify(zopInsert);
  end;
end;

procedure TZRVariable.SetName(const Value: TComponentName);
begin
  inherited;
  Notify(zopRename);
end;

function TZRVariable.HasParent: Boolean;
begin
  Result := True;
end;

function TZRVariable.GetParentComponent: TComponent;
begin
  Result := Master;
end;

procedure TZRVariable.SetParentComponent(Value: TComponent);
begin
  if Value is TZRCustomController then
    SetMaster( TZRCustomController(Value) );
end;

procedure TZRVariable.SetFormat(const Value: TZFormat);
begin
  fFormat.Assign(Value);
end;

function TZRVariable.GetValue: Variant;
begin
  VarClear(Result);
  if (Status = zvsNeedUpdate) then Update;
  if (Status = zvsReady     ) then Result:= fValue;
end;

function TZRVariable.GetText : String;
begin
  Result := Format.Format(Value);
end;

function TZRVariable.GetIsEmpty: Boolean;
begin
  Result:= (Status = zvsReady) and ValueIsEmpty(fValue);
end;

function TZRVariable.GetIsRepeated: Boolean;
begin
  Result:= (Status = zvsReady) and (VarIsEmpty(fPrev) and VarIsEmpty(fValue) or (fPrev = fValue));
end;

procedure TZRVariable.Clear;
begin
  fStatus := zvsNeedUpdate;
end;

procedure TZRVariable.Prepare;
begin
  Reset;
end;

procedure TZRVariable.Unprepare;
begin
  fStatus := zvsPrepare;
end;

function TZRVariable.EvaluateValue: Variant;
begin
  VarClear(Result);
end;

procedure TZRVariable.Update;
begin
  if Status = zvsUpdating then
    ZRError(szrCircularVariable, [Name]);
  if Status = zvsNeedUpdate then begin
    fStatus:= zvsUpdating;
    VarCopy(fPrev, fValue);
    fValue := EvaluateValue;
    fStatus:= zvsReady;
  end;
end;

procedure TZRVariable.Undo;
begin
  if Status = zvsReady then begin
    fStatus:= zvsUpdating;
    Swap(fValue, fPrev);
    fStatus:= zvsReady;
  end;
end;

procedure TZRVariable.Redo;
begin
  if Status = zvsReady then begin
    fStatus:= zvsUpdating;
    Swap(fPrev, fValue);
    fStatus:= zvsReady;
  end;
end;

procedure TZRVariable.ResetValue;
begin
  VarClear(fValue);
end;

procedure TZRVariable.Reset;
begin
  if Status in [zvsNeedUpdate, zvsReady] then begin
    fStatus := zvsNeedUpdate;
    ResetValue;
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRExpression                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

function TZRExpression.UDF: Boolean;
begin
  Result:= Assigned(OnGetValue);
end;

function TZRExpression.Prepared: Boolean;
begin
  Result:= UDF or (fEvaluator <> nil);
end;

procedure TZRExpression.Prepare;
begin
  if not UDF then begin
    fEvaluator:= TZREvaluator.Create(Self);
    fEvaluator.Collection := Master.Collection;
    fEvaluator.Expression := Expression;
    fEvaluator.Parse;
  end;
  inherited;
end;

procedure TZRExpression.Unprepare;
begin
  inherited;
  if not UDF then begin
    fEvaluator.Free;
    fEvaluator := nil;
  end;
end;

function TZRExpression.GetText : String;
begin
  if not Master.IsPrinting {csDesigning in ComponentState} then
    Result := Expression
  else
    Result := inherited GetText;
end;

procedure TZRExpression.SetExpression(const Value: String);
var
  WasPrepared: Boolean;
begin
  if fExpression <> Value then begin
    WasPrepared:= Prepared;
    if WasPrepared then Unprepare;
    fExpression := Trim(Value);
    if WasPrepared then Prepare;
  end;
end;

function TZRExpression.EvaluateValue : Variant;
begin
  VarClear(Result);
  if UDF then OnGetValue(Self, Result) else
  if Prepared then Result := fEvaluator.Value;
end;

{ TZRField }

function TZRField.EvaluateValue: Variant;
begin
  VarClear(Result);
  if Assigned(DataSet) and DataSet.Active {and not DataSet.IsEmpty} then begin
    if (fFieldNo >= 0) then begin
      if DataSet.DefaultFields then fField := DataSet.Fields[fFieldNo]
    end else
      fField := nil;
  end else
    fField := nil;
  if Assigned(fField) then Result := fField.AsVariant;
end;

procedure TZRField.Prepare;
begin
  inherited;
  if Assigned(DataSet) and DataSet.Active then begin
    fField   := DataSet.FindField(DataField);
    if Assigned(fField) then
      fFieldNo := fField.Index
    else
      fFieldNo := -1;
  end else begin
    fField   := nil;
    fFieldNo := -1;
  end;
end;

procedure TZRField.Unprepare;
begin
  inherited;
  fField   := nil;
  fFieldNo := -1;
end;

function TZRField.GetText: String;
begin
  if not Master.IsPrinting {csDesigning in ComponentState} then
    Result := DataField
  else
    Result := inherited GetText;
end;

procedure TZRField.SetDataSet(Value : TDataSet);
begin
  if DataSet <> Value then begin
    Unprepare;
    fDataSet := Value;
    if DataSet <> nil then DataSet.FreeNotification(Self);
  end;
end;

procedure TZRField.SetDataField(const Value: String);
var
  {p : Integer;}
  F : TField;
begin
  if DataField <> Value then begin
    {
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      if DataField <> '' then
        p := pos(DataField, Name)
      else
        p := pos('Field', Name)
    else
      p := -1;
    }
    fDataField := Value;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then begin
      {
      if (p > 0) then SetComponentName(Self, copy(Name, 1, p-1) + Value);
      }
      if Assigned(DataSet) then begin
        F := DataSet.FindField(DataField);
        if Assigned(F) then begin
          Format.Width := F.DisplayWidth;
          if (F is TFloatField) and TFloatField(F).Currency then
            Format.FloatFormat := ffCurrency;
        end;
      end;
    end;
  end;
end;

procedure TZRField.SetMaster(Value: TZRCustomController);
begin
  inherited;
  if (Master <> nil) and (DataSet = nil) then DataSet := Master.DataSet;
end;

procedure TZRField.Notification(Component : TComponent; Operation : TOperation);
begin
  inherited;
  if (Component = DataSet) and (Operation = opRemove) then DataSet := nil;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRAggregator                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

function TZRAggregator.GetCount(Index : Integer): Integer;
begin
  Result := fCount[Index];
end;
function TZRAggregator.GetMin(Index : Integer): Variant;
begin
  Result := fMin[Index];
end;
function TZRAggregator.GetMax(Index : Integer): Variant;
begin
  Result := fMax[Index];
end;
function TZRAggregator.GetSum(Index : Integer): Variant;
begin
  Result := fSum[Index];
end;

function TZRAggregator.GetAvg(Index : Integer): Variant;
begin
  if Count[Index] > 0 then
    {if ValueKind(Sum[Index]) = zvkInteger then
      Result:= Sum[Index] div Count[Index]
    else}
      Result:= Sum[Index] / Count[Index]
  else
    Result:= 0;
end;

procedure TZRAggregator.Prepare;
begin
  fLevels := Master.GroupList.Count+1;
  SetLength(fCount, fLevels);
  SetLength(fSum, fLevels);
  SetLength(fMin, fLevels);
  SetLength(fMax, fLevels);
  SetLength(fPrevCount, fLevels);
  SetLength(fPrevSum  , fLevels);
  SetLength(fPrevMin  , fLevels);
  SetLength(fPrevMax  , fLevels);
  inherited;
end;

procedure TZRAggregator.Unprepare;
begin
  inherited;
  SetLength(fCount, 0);
  SetLength(fSum, 0);
  SetLength(fMin, 0);
  SetLength(fMax, 0);
  SetLength(fPrevCount, 0);
  SetLength(fPrevSum  , 0);
  SetLength(fPrevMin  , 0);
  SetLength(fPrevMax  , 0);
  fLevels := 0;
end;

function TZRAggregator.GetText: String;
begin
  if Assigned(Variable) then
    Result := '$('+Variable.Text+')'
  else
    Result := inherited GetText;
end;

procedure TZRAggregator.ResetValue;
begin
  if Assigned(Variable) then Variable.Reset;
end;

function TZRAggregator.EvaluateValue : Variant;
begin
  VarClear(Result);
  if Assigned(Variable) then Result := Variable.Value;
end;

procedure TZRAggregator.ResetValues(Index: Integer);
begin
  fCount[Index] := 0;
  VarClear(fSum[Index]);
  VarClear(fMin[Index]);
  VarClear(fMax[Index]);
end;

procedure TZRAggregator.UpdateValues(Index: Integer; Value: Variant);
const
  Summable  = [zvkInteger, zvkFloat, zvkCurrency];
var
  VK : TZValueKind;
begin
  fPrevCount[Index] := fCount[Index];
  fPrevSum  [Index] := fSum  [Index];
  fPrevMin  [Index] := fMin  [Index];
  fPrevMax  [Index] := fMax  [Index];
  if fCount[Index] = 0 then begin
    fSum[Index] := 0;
    fMax[Index] := Value;
    fMin[Index] := Value;
  end;
  VK := ValueKind(Value);
  if VK <> zvkError then begin
    if VK in Summable then fSum[Index] := fSum[Index] + Value;
    if (ValueKind(fMax[Index]) = zvkError) or (Value > fMax[Index]) then fMax[Index] := Value;
    if (ValueKind(fMin[Index]) = zvkError) or (Value < fMin[Index]) then fMin[Index] := Value;
  end;
  Inc(fCount[Index]);
end;

procedure TZRAggregator.UndoValues(Index: Integer);
begin
  Swap(fCount[Index], fPrevCount[Index]);
  Swap(fSum  [Index], fPrevSum  [Index]);
  Swap(fMin  [Index], fPrevMin  [Index]);
  Swap(fMax  [Index], fPrevMax  [Index]);
end;

procedure TZRAggregator.RedoValues(Index: Integer);
begin
  Swap(fCount[Index], fPrevCount[Index]);
  Swap(fSum  [Index], fPrevSum  [Index]);
  Swap(fMin  [Index], fPrevMin  [Index]);
  Swap(fMax  [Index], fPrevMax  [Index]);
end;

procedure TZRAggregator.Reset;
var
  i : Integer;
begin
  inherited;
  for i := Low(fCount) to High(fCount) do ResetValues(i);
end;

procedure TZRAggregator.Update;
var
  i : Integer;
begin
  if Status <> zvsReady then begin
    inherited;
    if (Status = zvsReady) {and (ValueKind(Value) in Aggregatable)} then
      for i := Low(fCount) to High(fCount) do UpdateValues(i, Value);
  end;
end;

procedure TZRAggregator.Undo;
var
  i : Integer;
begin
  inherited;
  if Status = zvsReady then
    for i := Low(fCount) to High(fCount) do UndoValues(i);
end;

procedure TZRAggregator.Redo;
var
  i : Integer;
begin
  inherited;
  if Status = zvsReady then
    for i := Low(fCount) to High(fCount) do RedoValues(i);
end;

procedure TZRAggregator.SetVariable(Value : TZRVariable);
var
  S : String;
  p : Integer;
begin
  if Variable <> Value then begin
    Unprepare;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) and Assigned(Variable) then begin

      if (Variable is TZRField) and
         (Variable.Name = 'zrv' + TZRField(Variable).DataField) then
        S := TZRField(Variable).DataField
      else if (copy(Variable.Name, 1, 3) = 'zrv') then
        S := copy(Variable.Name, 4, length(Variable.Name))
      else
        S := '';

      if (S <> '') and MatchMask('*'+S, Name) then
        p := pos(S, Name)
      else
        p := -1;
      {
      if copy(Variable.Name, 1, length(Variable.Master.Name)) = Variable.Master.Name then
        S := copy(Variable.Name, length(Variable.Master.Name)+1, 128)
      else
        S := Variable.Name;
      p := pos(S, Name);
      }
    end else begin
      S := '';  p := -1;
    end;
    fVariable := Value;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) and Assigned(Variable) then begin
      {
      if copy(Variable.Name, 1, length(Variable.Master.Name)) = Variable.Master.Name then
        S := copy(Variable.Name, length(Variable.Master.Name)+1, 128)
      else
        S := Variable.Name;
      if (p > 0) then SetComponentName(Self, copy(Name, 1, p-1) + S);
      }
      if (Variable is TZRField) and
         (Variable.Name = 'zrv' + TZRField(Variable).DataField) then
        S := TZRField(Variable).DataField
      else if (copy(Variable.Name, 1, 3) = 'zrv') then
        S := copy(Variable.Name, 4, length(Variable.Name))
      else
        S := '';
      if (p > 0) then SetComponentName(Self, copy(Name, 1, p-1) + S);
      Format := Variable.Format;
    end;
  end;
end;


