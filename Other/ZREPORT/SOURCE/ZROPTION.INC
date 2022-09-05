{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZRBands                                !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRBands.Create(aOwner: TZRCustomController);
begin
  inherited Create;
  fOwner := aOwner;
end;

function TZRBands.GetHasBand(BandType: TZRBandType): Boolean;
begin
  Result := Owner.GetBand(BandType) <> nil;
end;

procedure TZRBands.SetHasBand(BandType: TZRBandType; Value: Boolean);
var
  aBand : TZRCustomBand;
begin
  if GetHasBand(BandType) <> Value then begin
    if Value then
      Owner.CreateBand(BandType, nil)
    else begin
      aBand := Owner.GetBand(BandType);
      if not (csDesigning in aBand.ComponentState) or
        (aBand.LabelList.Count = 0) or
        (MessageDlg( Format( LoadStr(szrBandDelete), [aBand.Name]),
         mtConfirmation, [mbOk, mbAbort], 0) = mrOK) then aBand.Free;
    end;
  end;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                 TZRMargins                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRMargins.Create(AOwner: TZReport);
begin
  inherited Create;
  fOwner  := AOwner;
  fMargins:= Rect(2, 1, 1, 1);
end;

destructor TZRMargins.Destroy;
begin
//  fOwner.Margins := nil;
  inherited;
end;

function TZRMargins.GetValue(Index: Integer): Integer;
begin
  case Index of
    0  : Result := fMargins.Left;
    1  : Result := fMargins.Top;
    2  : Result := fMargins.Right;
    3  : Result := fMargins.Bottom;
    else Result := 0;
  end;
end;

procedure TZRMargins.SetValue(Index: Integer; Value: Integer);
begin
  if GetValue(Index) <> Value then begin
    if Value < 0 then Value := 0;
    case Index of
      0: fMargins.Left   := Value;
      1: fMargins.Top    := Value;
      2: fMargins.Right  := Value;
      3: fMargins.Bottom := Value;
    end;
    Changed;
  end;
end;

procedure TZRMargins.Changed;
begin
  fOwner.UpdateBounds;
end;

procedure TZRMargins.Assign(Source: TPersistent);
begin
  if Source is TZRMargins then begin
    fMargins:= TZRMargins(Source).fMargins;
    Changed;
  end else
    inherited;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                 TZRColumns                             !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRColumns.Create(AOwner: TZReport);
begin
  inherited Create;
  fOwner  := AOwner;
  fCount  := 1;
  fSpace  := 2;
end;

destructor TZRColumns.Destroy;
begin
//  fOwner.Columns := nil;
  inherited;
end;

procedure TZRColumns.SetCount(const Value: Integer);
begin
  if Count <> Value then begin
    if Value > 0 then
      fCount := Value
    else
      fCount := 1;
    Changed;
  end;
end;

procedure TZRColumns.SetSpace(const Value: Integer);
begin
  if Space <> Value then begin
    if Value > 0 then
      fSpace := Value
    else
      fSpace := 1;
    Changed;
  end;
end;

procedure TZRColumns.Assign(Source: TPersistent);
begin
  if Source is TZRColumns then begin
    fSpace:= TZRColumns(Source).Space;
    fCount:= TZRColumns(Source).Count;
    Changed;
  end else
    inherited;
end;

procedure TZRColumns.Changed;
begin
  Owner.UpdateBounds;//AlignBands;
end;

function TZRColumns.Width: Integer;
begin
  with Owner do
    if Assigned(Printer) then
      Result := (Printer.PageWidth - Margins.Left - Margins.Right - Space*(Count-1)) div Count
    else
      Result := (PageWidth - Margins.Left - Margins.Right - Space*(Count-1)) div Count;
end;

function TZRColumns.Height: Integer;
begin
  with fOwner do
    if Assigned(Printer) then
      Result := (Printer.PageHeight - Margins.Top - Margins.Bottom)
    else
      Result := (PageHeight - Margins.Top - Margins.Bottom);
end;

