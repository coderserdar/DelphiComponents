{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit FxDCube;

interface

uses
 Windows, Messages, Classes, SysUtils, Controls, StdCtrls, Graphics, Dialogs,
 DB, Grids, Forms, FxConsts, FxGrid, FxDB, FxCommon, FxStore, FxCache,
 ExtCtrls, Buttons, ComCtrls, Menus, Mask, ValEdit, FxMap;

resourcestring
  SDataSetNotActive='DataSet not Active';

type
  ECubeDesignError = class(Exception);

  TFDCubeEditor = class(TForm)
    lbFields: TListBox;
    CaptionEdit: TEdit;
    labCaption: TLabel;
    labActive: TLabel;
    labBin: TLabel;
    labStart: TLabel;
    labType: TLabel;
    ActiveEdit: TComboBox;
    BinEdit: TComboBox;
    TypeEdit: TComboBox;
    Pager: TPageControl;
    DimensionInfo: TTabSheet;
    MemoryControl: TTabSheet;
    labFiels: TLabel;
    labFormat: TLabel;
    FormatEdit: TEdit;
    StartEdit: TMaskEdit;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    NActiveDims: TLabel;
    nDemandDims: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    NActiveSums: TLabel;
    NDemandSums: TLabel;
    Label6: TLabel;
    MaxDims: TEdit;
    MaxSums: TEdit;
    MaxCells: TEdit;
    Label5: TLabel;
    Label12: TLabel;
    nDemandCells: TLabel;
    GetCellCounts: TButton;
    CubeLimits: TGroupBox;
    ValueCount: TLabel;
    Label3: TLabel;
    nCurrDims: TLabel;
    nCurrSums: TLabel;
    nCurrCells: TLabel;
    AlignEdit: TComboBox;
    labAlignment: TLabel;
    edWidth: TEdit;
    labWidth: TLabel;
    rgDsgnOptions: TRadioGroup;
    procedure lbFieldsClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure HandleFieldEdit(Sender: TObject);
    procedure HandleBeginEdit(Sender: TObject; var Key: Char);
    procedure ActiveEditChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure PagerChange(Sender: TObject);
    procedure GetCellCountsClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure HelpButtonClick(Sender: TObject);
    procedure AlignEditChange(Sender: TObject);
    procedure rgDsgnOptionsClick(Sender: TObject);
    { Private declarations }
  private
    FDSSCube: TFxCube;
    myMap: TFxMap;
    FDataSet: TDataSet;
    bBinEdited: boolean;
    bEditing: boolean;
    fLastSelected: integer;
    bForceRefresh: boolean;
    bSetNameEditing: boolean;
    bSetValuesEditing: boolean;
    { editable field copies }
    FNameCopy: String;
    FFormatCopy: String;
    FActiveCopy: TActiveFlags;
    FTypeCopy:  TDimFlags;
    FBinTypeCopy: TBinType;
    FStartValueCopy: string;
    function SInitialize(AnObject: TFxCube): Boolean;
    procedure InitEdit;
    procedure CommitEdit;
    procedure GetValueCounts;
    { Protected declarations }
  protected
    { Public declarations }
  public
    procedure UpdateLists;
    procedure InitializePanel;
    procedure InitCapacityPage;
    procedure SFinalize;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure ShowFDCubeEditor(aCube: TFxCube);

var
  FDCubeEditor: TFDCubeEditor;

implementation

{$R *.dfm}


  { Query Editor }

procedure ShowFDCubeEditor(ACube: TFxCube);
begin
  if not Assigned(ACube) then Exit;
  with TFDCubeEditor.Create(Application)do
  try
    if SInitialize(ACube) then
      ShowModal;
  finally
    Free;
  end;
end;

constructor TFDCubeEditor.Create(AOwner: TComponent);
begin
  bSetNameEditing := false;
  bSetValuesEditing := false;
  bForceRefresh := false;
  myMap := nil;
  inherited Create(AOwner);
end;

destructor TFDCubeEditor.Destroy;
begin
  myMap.free;
  inherited Destroy;
end;

function TFDCubeEditor.SInitialize(anObject: TFxCube): boolean;
begin
  FDSSCube:= anObject;
  FDataSet:= FDSSCube.DataSet;
  if not Assigned(FDataSet) then
    raise ECubeDesignError.Create(sNoDataSet);
  if not FDataSet.Active then
    raise ECubeDesignError.Create(SDataSetNotActive);
  Pager.ActivePage := DimensionInfo;
  { Build the dimension map for this set }
  myMap := TFxMap.Create(FDSSCube, TFxMapItem);
  myMap.Assign(FDSSCube.DimensionMap);
  myMap.Build;
  MaxDims.text := IntToStr(FDSSCube.MaxDimensions);
  MaxSums.text := IntToStr(FDSSCube.MaxSummaries);
  MaxCells.text:= IntToStr(FDSSCube.MaxCells);
  UpdateLists;
  InitCapacityPage;
  InitializePanel;
  Result := True;
end;

procedure TFDCubeEditor.SFinalize;
var
  i,dims, sums: integer;
  ValueCount, CP: Cardinal;
  bEstimating: boolean;
begin
  CommitEdit;
  dims := strtoint(MaxDims.text);
  sums := strtoint(MaxSums.text);
  if (myMap.ActiveDimensionCount > dims) then
    raise ECubeDesignError.createFmt(sMaxAllowedDims,[dims]);
  if (myMap.ActiveSummaryCount > sums) then
    raise ECubeDesignError.createResFmt(@sMaxAllowedsums,[sums]);
  FDSSCube.MaxDimensions := dims;
  FDSSCube.MaxSummaries := sums;
  FDSSCube.MaxCells := strtoInt(MaxCells.text);
  if (FDSSCube.CurrentSummary <= sums) then
    FDSSCube.currentSummary := 0;
  {
    See if the valuecount information is still available.  If not, an attempt is
    made to be friendly to the user by making a fairly pessimistic assumption about
    the size of a dimension whose valuecount is not available.  If all AsNeeded or
    Active Cells can still be loaded, no attempt is made to fetch the cell info
    at this point.
  }
  if (FDSSCube.maxCells > 0) then
  begin
    bEstimating := false;
    CP := 1;
    for I:= 0 to myMap.Count-1 do begin
      if (myMap[i].DimensionType = dimDimension) and (myMap[i].ActiveFlag <> diInactive) then
      begin
        ValueCount := myMap[i].ValueCount;
        if (ValueCount <= 0) then
        begin
          bEstimating := true;
          ValueCount := LargeValueCount;
        end;
        CP := CP * ValueCount;
      end;
    end;
    if (CP > FDSSCube.MaxCells) and bEstimating then
    begin
      if (MessageDlg(sGetValueCounts, mtConfirmation, [mbYes, mbNo], 0) = 6) then
      begin
        GetValueCounts;
      end;
    end;
  end;
  for i := 0 to myMap.count-1 do
  begin
    myMap[i].ActiveFlag := myMap[i].ActiveFlag; { resets the Active boolean }
  end;
  FDSSCube.Refresh(myMap, bForceRefresh);
  UpdateDesigner(FDSSCube);
end;

procedure TFDCubeEditor.UpdateLists;
var
  i: Integer;
  Dim: TFxMapItem;
begin
  lbFields.Clear;
  for I:= 0 to myMap.Count-1 do begin
    Dim:= myMap[i];
    if Dim.Active then
      lbFields.Items.Add(Dim.FieldName + '*')
    else
      lbFields.Items.Add(Dim.FieldName);
  end;
  if lbFields.Count>0 then lbFields.ItemIndex:=0;
  FLastSelected:= lbFields.ItemIndex;
end;

procedure TFDCubeEditor.InitializePanel;
begin
  InitEdit;
  if (csDesigning in FDSSCube.ComponentState) then
    rgDsgnOptions.ItemIndex:=Ord(FDSSCube.DesignState)
  else begin
    rgDsgnOptions.Enabled:=False;
    GetCellCounts.Visible:= False;
    MaxCells.Enabled:= False;
    MaxDims.Enabled:= False;
    MaxSums.Enabled:= False;
  end;
end;

procedure TFDCubeEditor.lbFieldsClick(Sender: TObject);
begin
  with lbFields do begin
    CommitEdit;
    FLastSelected:=ItemIndex;
    InitEdit;
  end;
end;

procedure TFDCubeEditor.InitEdit;
var
  DM: TFxMapItem;
begin
  if FLastSelected<>-1 then begin
    DM:= myMap[FLastSelected];
    ValueCount.Caption:=IntToStr(DM.ValueCount);
    FActiveCopy := DM.ActiveFlag;
    FNameCopy   := DM.Name;
    FTypeCopy   := DM.DimensionType;
    FFormatCopy := DM.Format;
    // Preset values
    ActiveEdit.ItemIndex:= Ord(DM.ActiveFlag);
    TypeEdit.ItemIndex := Ord(DM.DimensionType);
    TypeEdit.Enabled := True;
    CaptionEdit.Text := DM.Name;
    CaptionEdit.Enabled := not (AnsiUpperCase(DM.Name) = sCountStar);
    FormatEdit.Text:= DM.Format;
    BinEdit.ItemIndex:= Ord(DM.BinType);
    AlignEdit.ItemIndex:=Ord(DM.Alignment);
    edWidth.Text:=IntToStr(DM.Width);
    FBinTypeCopy:= TFxMapItem(DM).BinType;
    if DM.DimensionType=dimDimension then begin
      BinEdit.enabled := true;
      labBin.Enabled := true;
      if (DM.Field.DataType in [ftDate,ftDateTime]) and (DM.BinType in [binQuarter,binMonth,binYear,binWeek])then
      begin
        FStartValueCopy := TFxMapItem(DM).StartValue;
        StartEdit.text := TFxMapItem(DM).StartValue;
        StartEdit.enabled := true;
        labStart.Enabled := true;
      end else if DM.BinType = binSet then begin
        StartEdit.enabled := true;
        labStart.Enabled := true;
        StartEdit.Text := DM.StartValue;
      end else begin
        StartEdit.Text := '';
        StartEdit.enabled := false;
        labStart.Enabled := false;
      end;
    end else begin
      BinEdit.enabled := false;
      labBin.Enabled := false;
      StartEdit.Text := '';
      StartEdit.enabled := false;
      labStart.Enabled := false;
    end;
  end else begin
    ActiveEdit.itemindex := -1;
    FormatEdit.text := '';
    AlignEdit.ItemIndex:=0;
    CaptionEdit.text := '';
    TypeEdit.itemindex := -1;
    BinEdit.itemindex := -1;
    StartEdit.text := '';
  end;
  bEditing := false;
  bBinEdited := false;
end;

procedure TFDCubeEditor.CommitEdit;
var
  OldDM, DM: TFxMapItem;
  newBinType: tBinType;
  ValueCount: Integer;
begin
  if bEditing and (fLastSelected >= 0) and (fLastSelected < myMap.count) then
  begin
    DM := myMap[fLastSelected];
    DM.Name:=CaptionEdit.text;
    DM.DimensionType:= TDimFlags(TypeEdit.ItemIndex);
    DM.ActiveFlag   := TActiveFlags(ActiveEdit.itemindex);
    DM.Format       := FormatEdit.text;
    DM.Alignment    := TAlignment(AlignEdit.ItemIndex);
    DM.Width        := StrToInt(edWidth.Text);
    if bBinEdited then begin
      { estimate the valuecount of this new bin, based on the old one, if possible }
      ValueCount := 0;
      newBinType := TBinType(BinEdit.ItemIndex);
      if (FDSSCube.DimensionMap.count > fLastSelected) then begin
        oldDM := FDSSCube.DimensionMap[fLastSelected];
        if (OldDM.BinType = NewBinType) then
          ValueCount := OldDM.ValueCount
        else
          case newBinType of
          binMonth:
          begin
            if (OldDM.BinType = binQuarter) then
              ValueCount := OldDM.ValueCount * 3
            else if (OldDM.BinType = binYear) then
              ValueCount := OldDM.ValueCount * 12;
          end;
          binQuarter:
          begin
            if (OldDM.BinType = binMonth) then
              ValueCount := OldDM.ValueCount div 3
            else if (OldDM.BinType = binYear) then
              ValueCount := OldDM.ValueCount * 4;
          end;
          binYear:
          begin
            if (OldDM.BinType = binMonth) then
              ValueCount := OldDM.ValueCount div 12
            else if (OldDM.BinType = binQuarter) then
              ValueCount := OldDM.ValueCount div 4;
          end;
          binSet:
          begin
            ValueCount := 2;
          end;
          binNone:
          begin
            if (OldDM.BinType = binSet) and assigned (OldDM.BinData) then
              ValueCount := OldDM.BinData.GetAllBinValueCount;
          end;
        end;
      end;
      try
        DM.BinType := TBinType(BinEdit.ItemIndex);
        if (DM.Field.DataType in [ftDate,ftDateTime]) and (DM.BinType in [binQuarter,binYear,binMonth,binWeek]) then
        begin
          if (StartEdit.text <> '') then
            TFxMapItem(DM).StartValue := DatetoStr(StrtoDate(StartEdit.text));
        end
        else if (DM.BinType = binSet) then
        begin
          DM.StartValue := StartEdit.text;
        end;
      except
        on exception do
        begin
          lbFields.itemIndex := fLastSelected;
          raise ECubeDesignError.createRes(@sIllegalValueForBin);
        end;
      end;
      DM.ValueCount := ValueCount;
    end;
  end;
  bBinEdited := false;
  bEditing := false;
end;

procedure TFDCubeEditor.InitCapacityPage;
var
  i: integer;
  iActiveSums, iNeededSums, ICurrSums: integer;
  iActiveDims, iNeededDims, iCurrDims: integer;
  iNeededCP, iActiveCP, iCurrCP:Cardinal;
begin
  if not assigned(myMap) then Exit;
  iActiveSums := 0;
  iNeededSums := 0;
  iCurrSUms := 0;
  iActiveDims := 0;
  iNeededDims := 0;
  iCurrDims := 0;
  iNeededCP := 1;
  iActiveCP := 1;
  iCurrCP := 1;
  for I:= 0 to myMap.Count-1 do begin
    if myMap[I].DimensionType = dimDimension then begin
      if myMap[i].Active then begin
        iCurrDims:= iCurrDims + 1;
        if myMap[I].ValueCount <= 0 then
          iCurrCP:= 0
        else
          iCurrCP:= iCurrCP * myMap[i].ValueCount;
      end;
      case myMap[I].ActiveFlag of
        diActive:
        begin
          iActiveDims := iActiveDims + 1;
          if (myMap[i].ValueCount <= 0) then
            iActiveCP := 0
          else
            iActiveCP := iActiveCP * myMap[i].ValueCount;
        end;
        diAsNeeded:
        begin
          iNeededDims := iNeededDims + 1;
          if (myMap[i].ValueCount <= 0) then
            iNeededCP := 0
          else
            iNeededCP := iNeededCP * myMap[i].ValueCount;
        end;
      end;
    end else if not myMap[i].Derived then begin
      if (myMap[i].Active) then iCurrSums := iCurrSums + 1;
      case myMap[i].ActiveFlag of
        diActive: iActiveSums := iActiveSums + 1;
        diAsNeeded: iNeededSums := iNeededSums + 1;
      end;
    end;
  end;
  nActiveDims.caption := inttostr(iActiveDims);
  nActiveSums.caption := inttostr(iActiveSums);
  nDemandDims.caption := inttostr(iNeededDims+iActiveDims);
  nDemandSums.caption := inttostr(iNeededSums+iActiveSums);
  nCurrDims.caption := inttostr(iCurrDims);
  nCurrSums.caption := inttostr(iCurrSums);
  if (iActiveCP > 0) and (iNeededCP > 0) then
    nDemandCells.Caption := inttostr(Cardinal(iNeededSums + iActiveSums) * iActiveCP * iNeededCP)
  else
    nDemandCells.Caption := sNotAvailable;
  if (iCurrCP > 0) then
    nCurrCells.Caption := IntToStr(iCurrCP * Cardinal(iCurrSums))
  else
    nCurrCells.Caption := sNotAvailable;
end;

procedure TFDCubeEditor.OKButtonClick(Sender: TObject);
begin
  try
    SFinalize;
  except
    on E: exception do
    begin
      raise Exception.create(E.Message);
      Exit;
    end;
  end;
  Close;
end;

procedure TFDCubeEditor.HandleFieldEdit(Sender: TObject);
var
  DM: TFxMapItem;
begin
  if Sender=edWidth then begin
    bEditing :=True;
  end;
  if (Sender = FormatEdit) then begin
    bEditing := true;
    if (TEdit(Sender).text <> '') then FFormatCopy := TEdit(Sender).text;
  end;
  if Sender=CaptionEdit then begin
    bEditing:=true;
    if TEdit(Sender).text<>'' then FNameCopy := TEdit(Sender).text;
  end;
  if (Sender = ActiveEdit) then  begin
    bEditing := true;
    FActiveCopy := TActiveFlags(TComboBox(Sender).itemindex);
  end;
  if (Sender = TypeEdit) then begin
    bEditing := True;
    FTypeCopy := TDimFlags(TComboBox(Sender).ItemIndex);
  end;
  if (Sender = BinEdit) then begin
    bBinEdited:= true;
    bEditing  := true;
    FBinTypeCopy := TBinType(TComboBox(Sender).itemindex);
    DM := myMap[fLastSelected];
    if FBinTypeCopy<>DM.BinType then begin
      if FBinTypeCopy in [binQuarter, binYear, binMonth,binWeek]then begin
        if DM.Field.DataType in [ftDate,ftDateTime] then begin
          FStartValueCopy := TFxMapItem(DM).StartValue;
          StartEdit.text := TFxMapItem(DM).StartValue;
          StartEdit.enabled := true;
          labStart.Enabled := true;
        end
        else
        begin
          ShowMessage(sDateBinningNotAllowed);
          TComboBox(Sender).itemIndex := 0;
        end;
      end
      else if (FBinTypeCopy = binSet) then
      begin
        StartEdit.enabled := true;
        StartEdit.text := '';
        labStart.Enabled := true;
      end
      else
      begin
        StartEdit.Text := '';
        StartEdit.enabled := false;
        labStart.Enabled := false;
      end;
    end;
  end;
  if (Sender = StartEdit) then begin
    bEditing := true;
    bBinEdited := true;
  end;
end;

procedure TFDCubeEditor.HandleBeginEdit(Sender: TObject; var Key: Char);
begin
  bEditing := true;
end;

procedure TFDCubeEditor.ActiveEditChange(Sender: TObject);
begin
  bEditing := true;
end;

procedure TFDCubeEditor.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFDCubeEditor.PagerChange(Sender: TObject);
begin
  CommitEdit;
  if (Pager.ActivePage.Name = 'MemoryControl') then
    InitCapacityPage;
end;

procedure TFDCubeEditor.GetValueCounts;
var
  Map: TFxMap;
  i: integer;
  Cells, Dims, Sums: integer;
  dState: TCubeDataState;
begin
  Map := TFxMap.create(FDSSCube, TFxMapItem);
  dState := FDSSCube.DesignState;
  Dims := strtoint(MaxDims.text);
  Sums := strtoint(maxSums.text);
  Cells := strtoint(maxCells.text);
  try
    Map.assign(myMap);
    FDSSCube.maxDimensions := 16;
    FDSSCube.maxSummaries := 32;
    FDSSCube.maxCells := 200000000;
    for i := 0 to Map.count-1 do
      Map[i].ActiveFlag := diAsNeeded;
    FDSSCube.DesignState := dsDimensionData;
    if assigned(FDSSCube.DataSet) then
    begin
      bForceRefresh := true;
      if (FDSSCube.DataSet.Active) then
        FDSSCube.Refresh(Map, true)
      else
      begin
        if assigned(FDSSCube.DimensionMap) then
          FDSSCube.DimensionMap.assign(Map);
        FDSSCube.DataSet.Active := true;
      end;
      for i := 0 to myMap.count-1 do
        myMap[i].ValueCount := FDSSCube.DimensionMap[i].ValueCount;
    end;
  finally
    Map.free;
    FDSSCube.DesignState := dstate;
    FDSSCube.MaxDimensions := Dims;
    FDSSCube.MaxSummaries := Sums;
    FDSSCube.MaxCells := Cells;
  end;
end;

procedure TFDCubeEditor.GetCellCountsClick(Sender: TObject);
begin
  GetValueCounts;
  UpdateLists;
  InitCapacityPage;
end;

procedure TFDCubeEditor.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then
    OKButtonClick(Self)
  else if (Key = Chr(27)) then
    CancelButtonClick(self);
end;

procedure TFDCubeEditor.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(hcDDecisionCubeEditor);
end;

procedure TFDCubeEditor.AlignEditChange(Sender: TObject);
begin
  bEditing := true;
end;

procedure TFDCubeEditor.rgDsgnOptionsClick(Sender: TObject);
begin
  with Sender as TRadioGroup do
    FDSSCube.DesignState:= TCubeDataState(ItemIndex);
end;

end.
