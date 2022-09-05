{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRCustomBand                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

const
  BandNameMap : array[TZRBandType] of String = (
    'PageHeader',
    'Header',
    'ColumnHeader',
    'GroupHeader',
    'DetailHeader',
    'Detail',
    'DetailFooter',
    'GroupFooter',
    'ColumnFooter',
    'Footer',
    'PageFooter',
    'Child',
    'SubDetail' );

const
  HeaderMap: array[zbtDetailFooter..zbtPageFooter] of TZRBandType = (
    zbtDetailHeader,
    zbtGroupHeader ,
    zbtColumnHeader,
    zbtHeader      ,
    zbtPageHeader  );
  FooterMap: array[zbtPageHeader..zbtDetailHeader] of TZRBandType = (
    zbtPageFooter  ,
    zbtFooter      ,
    zbtColumnFooter,
    zbtGroupFooter ,
    zbtDetailFooter);

constructor TZRCustomBand.Create(aOwner: TComponent);
var
  BT: TZRBandType;
begin
  inherited;
  Color        := clBand;
  BandType     := zbtHeader;
  fLabelList   := TList.Create;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Height       := 2;

  fLoadedGroup  := -1;
  for BT := Low(TZRChildBands) to High(TZRChildBands) do begin
    fLoadedParents[BT] := -1;
    fParentLinks  [BT] := nil;
  end;
end;

destructor TZRCustomBand.Destroy;
var
  BT : TZRBandType;
  CL,
  PL : TZRCustomBand;
begin
  for BT:= Low(TZRChildBands) to High(TZRChildBands) do begin
    CL := GetChildLink(BT);
    if CL <> nil then begin
      PL := GetParentLink(BT);
      SetParentLink(BT, nil);
      if (PL <> nil) and not (csDestroying in PL.ComponentState) then
        CL.SetParentLink(BT, PL)
      else
        CL.Free;
    end;
  end;
  SetMaster(nil);
  inherited;
  LabelList.Free;
end;

procedure TZRCustomBand.Draw;

  procedure DrawGrid;
  var
    i,j,d,x,y: Integer;
  begin
    with Canvas do
      begin
        if Self = Report then begin
          Brush.Color:= clGridReport;
          Pen  .Color:= clGridReport;
        end else begin
          Brush.Color:= clGridBand;
          Pen  .Color:= clGridBand;
        end;
        Brush.Style:= bsSolid;
        Pen  .Style:= psSolid;
        Pen  .Width:= 1;

        d:= CharSize.X;
        y:= CharSize.Y div 2;
        for i:= 1 to Width-1 do
          begin
            x:= i*d;
            MoveTo(x, 1);
            LineTo(x, ClientRect.Bottom-1);
            if i mod 5 = 1 then
              begin
                Dec(x, CharSize.X div 2);
                Ellipse(x, y-1, x+1, y+1);
                j:= y;
                while j < ClientHeight do begin
                  Ellipse(x, j-1, x+1, j+1);
                  Inc(j, 10*y);
                end;
              end;
          end;
        d:= CharSize.Y;
        for i:= 1 to Height - 1 do
          begin
            y:= i*d;
            MoveTo(1, y);
            LineTo(ClientRect.Right-1, y);
          end;
      end;
  end;

begin
  inherited;
  if not Report.Minimized then begin
    DrawGrid;
    if not (Self is TZReport) then with Canvas do begin
      Brush.Style:= bsClear;
      Font.Name  := 'Small Fonts';
      Font.Size  := 6;
      Font.Color := clWindowText;
      TextOut(2, ClientRect.Bottom - TextHeight('W') - 4, BandNameMap[BandType]);
      Font:= Self.Font;
    end;
  end;
end;

procedure TZRCustomBand.SetParent(aParent: TWinControl);
var
  aMaster: TZRCustomController;
begin
  if (aParent <> nil) and (aParent is TZRCustomController) then begin
    aMaster := (aParent as TZRCustomController);
    if not (aParent is TZReport) then aParent := aMaster.Report;
  end else
    aMaster := nil;
  inherited;
  SetMaster(aMaster);
end;

procedure TZRCustomBand.SetBandType(const Value: TZRBandType);
begin
  if BandType <> Value then begin
    fBandType := Value;
    if Assigned(Master) then Master.SortBands;
  end;
end;

procedure TZRCustomBand.SetStretch(const Value: Boolean);
begin
  if Value and (BandType in NoStretchBands) then
    ZRError(szrNoStretch, [BandNameMap[BandType]]);
  fStretch := Value;
end;

procedure TZRCustomBand.SetMaster(const Value: TZRCustomController);
var
  Root : TZRCustomController;
begin
  if Master <> Value then begin
    Root := Value;
    while (Root <> nil) and (Root <> Self) do Root := Root.Master;

    if Root = nil then begin
      if (Master <> nil) then begin
        Notify(zopRemove);
        Master.RemoveBand(Self);
      end;
      fMaster := Value;
      if (Master <> nil) then begin
        Notify(zopInsert);
        Master.AddBand(Self);
      end;
    end else

      if csDesigning in ComponentState then
        ZRError(szrCircularMaster, []);
  end;
end;

function TZRCustomBand.HasParent : Boolean;
begin
  Result := GetParentComponent <> nil;
end;

function TZRCustomBand.GetParentComponent: TComponent;
begin
  if (Self is TZReport) then
    Result := inherited GetParentComponent
  else
    Result := Master;
end;

procedure TZRCustomBand.SetParentComponent(Value: TComponent);
begin
  if (Self is TZReport) or not (Value is TZRCustomController) then
    inherited
  else
    Master := TZRCustomController(Value);
end;

procedure TZRCustomBand.ReadBandType(Reader: TReader);
begin
  BandType := TZRBandType(GetEnumValue(TypeInfo(TZRBandType), Reader.ReadIdent));
end;

procedure TZRCustomBand.WriteBandType(Writer: TWriter);
begin
  Writer.WriteIdent(GetEnumName(TypeInfo(TZRBandType), Ord(BandType)));
end;

procedure TZRCustomBand.SetGroup(Value : TZRGroup);
begin
  if (BandType in [zbtGroupHeader, zbtGroupFooter]) and (Group <> Value) then begin
    if (Group <> nil) then Group.SetBand(BandType, nil);
    fGroup := Value;
    if (Group <> nil) then Group.SetBand(BandType, Self);
  end;
end;

function TZRCustomBand.GetParentLink(Index: TZRBandType): TZRCustomBand;
begin
  Result := fParentLinks[Index];
end;

procedure TZRCustomBand.SetParentLink(Index: TZRBandType; Value : TZRCustomBand);
begin
  if fParentLinks[Index] <> Value then begin
    fParentLinks[Index] := Value;
    if not (csLoading in ComponentState) then begin
      if Value <> nil then begin
        Master := Value.Master;
        Notify(zopChild);
      end;
      if Assigned(Master) then Master.SortBands;
    end;
  end;
end;

function TZRCustomBand.GetChildLink(Index: TZRBandType) : TZRCustomBand;
var
  i : Integer;
begin
  if Assigned(Master) then with Master do
    for i := 0 to BandList.Count-1 do begin
      Result := TZRCustomBand(BandList[i]);
      if (Result.BandType = Index) and
         (Result.GetParentLink(Index) = Self) then Exit;
    end;
  Result := nil;
end;

function TZRCustomBand.GetHasChild : Boolean;
begin
  Result := GetChildLink(zbtChild) <> nil;
end;

procedure TZRCustomBand.SetHasChild(Value : Boolean);
var
  aChild : TZRCustomBand;
begin
  if Assigned(Master) and (GetHasChild <> Value) then begin
    if Value then begin
      if (BandType in NoChildBands) then
        ZRError(szrNoChildBand, [BandNameMap[BandType]]);
      aChild := Master.CreateBand(zbtChild, nil);
      aChild.ParentBand := Self;
    end else
      ChildBand.Free;
  end;
end;

function TZRCustomBand.GetHasController : Boolean;
begin
  Result := GetChildLink(zbtController) <> nil;
end;

procedure TZRCustomBand.SetHasController(Value : Boolean);
var
  aController : TZRCustomBand;
begin
  if Assigned(Master) and (HasController <> Value) then begin
    if Value then begin
      if (BandType in NoControllerBands) then
        ZRError(szrNoControllerBand, [BandNameMap[BandType]]);
      aController := Master.CreateBand(zbtController, nil);
      aController.MasterBand := Self;
    end else
      SubController.Free;
  end;
end;

procedure TZRCustomBand.ReadGroupOrder(Reader: TReader);
begin
  fLoadedGroup := Reader.ReadInteger;
end;

procedure TZRCustomBand.WriteGroupOrder(Writer: TWriter);
begin
  Writer.WriteInteger(Group.Order);
end;

procedure TZRCustomBand.ReadParentIndex(Reader: TReader);
begin
  fLoadedParents[zbtChild] := Reader.ReadInteger;
end;

procedure TZRCustomBand.WriteParentIndex(Writer: TWriter);
begin
  Writer.WriteInteger(Master.BandList.IndexOf(ParentBand));
end;

procedure TZRCustomBand.ReadMasterIndex(Reader: TReader);
begin
  fLoadedParents[zbtController] := Reader.ReadInteger;
end;

procedure TZRCustomBand.WriteMasterIndex(Writer: TWriter);
begin
  Writer.WriteInteger(Master.BandList.IndexOf(MasterBand));
end;

procedure TZRCustomBand.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('BandType'   , ReadBandType   , WriteBandType   , not (Self is TZRCustomController) );
  Filer.DefineProperty('GroupOrder' , ReadGroupOrder , WriteGroupOrder , Group      <> nil);
  Filer.DefineProperty('ParentIndex', ReadParentIndex, WriteParentIndex, ParentBand <> nil);
  Filer.DefineProperty('MasterIndex', ReadMasterIndex, WriteMasterIndex, MasterBand <> nil);
end;

procedure TZRCustomBand.Loaded;
begin
  inherited;
  if not (Self is TZReport) then begin
    if (BandType in [zbtGroupHeader, zbtGroupFooter]) then Group := Master.GroupList[fLoadedGroup];
    if (BandType = zbtChild     ) then begin
      if fLoadedParents[zbtChild] >= 0 then
        ParentBand := Master.BandList[fLoadedParents[zbtChild]]
      else
        ParentBand := Master;
    end;
    if (BandType = zbtController) then begin
      if fLoadedParents[zbtController] >= 0 then
        MasterBand := Master.BandList[fLoadedParents[zbtController]]
      else
        MasterBand := Master;
    end;
  end;
end;

procedure TZRCustomBand.SetName(const Value: TComponentName);
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
    for i:= 0 to LabelList.Count-1 do RenameComponent(TZReportControl(LabelList[i]), OldName, NewName);
  end;
end;

procedure TZRCustomBand.AlignBounds;
var
  i : Integer;
begin
  inherited;

  if IsPrinting then begin

    if pDoPrint then begin

      if (BandType in NoStretchBands) and (Height <> fHeight) then
        ZRError(szrNoStretch, [BandNameMap[BandType]]);

      for i:= 0 to LabelList.Count-1 do
        with TZReportControl(LabelList[i]) do
          if pDoPrint then UpdateBounds;
      if Stretch then
        for i:= 0 to LabelList.Count-1 do
          with TZReportControl(LabelList[i]) do
            if pDoPrint then AlignBounds;
    end;

  end else begin

    if not IsAligning and Assigned(Report) then Report.AlignBands;
    for i:= 0 to LabelList.Count-1 do
      with TZReportControl(LabelList[i]) do UpdateBounds;

  end;
end;

procedure TZRCustomBand.Prepare;
var
  i : Integer;
begin
  inherited;
  for i := 0 to LabelList.Count-1 do
    TZReportControl(LabelList[i]).Prepare;
  fPrintType := BandType;
end;

procedure TZRCustomBand.Unprepare;
var
  i : Integer;
begin
  for i := 0 to LabelList.Count-1 do
    TZReportControl(LabelList[i]).Unprepare;
  inherited;
end;

procedure TZRCustomBand.DoBeforePrint;
var
  i : Integer;
begin
  inherited;
  if pDoPrint then begin
    for i:= 0 to LabelList.Count-1 do
      with TZReportControl(LabelList[i]) do DoBeforePrint;
    UpdateBounds;
  end;
end;

procedure TZRCustomBand.DoAfterPrint;
var
  i : Integer;
begin
  inherited;
  if pDoPrint then
    for i:= 0 to LabelList.Count-1 do
      with TZReportControl(LabelList[i]) do DoAfterPrint;
end;

procedure TZRCustomBand.PrintControl(Control: TZReportControl; OfsX, OfsY: Integer);
begin
  if Assigned(Control) then with Control do
    if pDoPrint then
      try
        DoPrint(OfsX, OfsY);
      except
        pDoPrint := False;
        raise;
      end;
end;

procedure TZRCustomBand.DoPrint(OfsX, OfsY: Integer);
var
  i : Integer;
begin
  //Exclude(Master.fStepState  , PrintType);
  Include(Master.fColumnState, PrintType);
  Master.ColumnInit := True;
  if PrintType in [Low(HeaderMap)..High(HeaderMap)] then
    Exclude(Master.fColumnState, HeaderMap[PrintType]);
  if PrintType in [Low(FooterMap)..High(FooterMap)] then
    Exclude(Master.fColumnState, FooterMap[PrintType]);
  if PrintType = zbtGroupHeader then Group.fStarted  := False;
  for i:= 0 to LabelList.Count-1 do
    PrintControl(TZReportControl(LabelList[i]), OfsX, OfsY);
  inherited;
end;

{
function TZRCustomBand.EstimateSpace: Integer;
var
  BT: TZRBandType;
begin
  Result := Height;
  if BandType = zbtColumnHeader then BT := zbtColumnFooter else BT := Succ(BandType);
end;
}
