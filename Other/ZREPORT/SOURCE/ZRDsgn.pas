unit ZRDsgn;

interface

{$I ZRDefine.inc}

uses
  Windows, Messages,                                        // WinAPI
  SysUtils,                                                 // Delphi RTL
  Classes, Graphics, Controls, Forms, Dialogs,              // Delphi VCL
  ComCtrls, ExtCtrls, StdCtrls, Buttons,
  Menus, ActnList, ImgList, Grids,
{$IFDEF D6Above}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  RTLConsts,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  ZReport;                                                  // ZReport

type
  TZRDesignForm = class(TForm)
    StatusBar: TStatusBar;
    paDialog: TPanel;
    btClose: TButton;
    pcDesign: TPageControl;
    tsBands: TTabSheet;
    tsVars: TTabSheet;
    alBands: TActionList;
    acBandsNew: TAction;
    acBandsRename: TAction;
    acBandsDelete: TAction;
    pmBands: TPopupMenu;
    pmBandsNew: TMenuItem;
    pmBandsRename: TMenuItem;
    pmBandsDelete: TMenuItem;
    ilTree: TImageList;
    alGroups: TActionList;
    acGroupsNew: TAction;
    acGroupsDelete: TAction;
    acGroupsUp: TAction;
    acGroupsDown: TAction;
    pmGroups: TPopupMenu;
    pmGroupsNew: TMenuItem;
    pmGroupsDelete: TMenuItem;
    pmGroupsSeparator: TMenuItem;
    pmGroupsUp: TMenuItem;
    pmGroupsDown: TMenuItem;
    ilButtons: TImageList;
    alVariables: TActionList;
    acVariablesField: TAction;
    acVariablesExpression: TAction;
    acVariablesTotal: TAction;
    acVariablesDelete: TAction;
    ilVariables: TImageList;
    gbBands: TGroupBox;
    tvBands: TTreeView;
    btBandsNew: TBitBtn;
    btBandsDelete: TBitBtn;
    gbGroups: TGroupBox;
    sbGroupsUp: TSpeedButton;
    sbGroupsDown: TSpeedButton;
    lbGroups: TListBox;
    btGroupsNew: TBitBtn;
    btGroupsDelete: TBitBtn;
    gbData: TGroupBox;
    laDataFields: TLabel;
    grControllers: TStringGrid;
    lbDataFields: TListBox;
    gbVariables: TGroupBox;
    laVariableFields: TLabel;
    laVariableExpressions: TLabel;
    laVariableTotals: TLabel;
    lbVariableExpressions: TListBox;
    btVariablesExpression: TBitBtn;
    btVariablesDelete: TBitBtn;
    lbVariableFields: TListBox;
    lbVariableTotals: TListBox;
    btVariablesField: TBitBtn;
    btVariablesTotal: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pcDesignChange(Sender: TObject);
    procedure lbGroupsClick(Sender: TObject);
    procedure tvBandsClick(Sender: TObject);
    procedure tvBandsChange(Sender: TObject; Node: TTreeNode);
    procedure grControllersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure acBandsNewExecute(Sender: TObject);
    procedure acBandsRenameExecute(Sender: TObject);
    procedure acBandsDeleteExecute(Sender: TObject);
    procedure acGroupsNewExecute(Sender: TObject);
    procedure acGroupsDeleteExecute(Sender: TObject);
    procedure acGroupsUpExecute(Sender: TObject);
    procedure acGroupsDownExecute(Sender: TObject);
    procedure acVariablesExecute(Sender: TObject);
    procedure acVariablesDeleteExecute(Sender: TObject);
    procedure tvBandsGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure tvBandsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvBandsEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure lbDataFieldsClick(Sender: TObject);
    procedure lbVariablesClick(Sender: TObject);
    procedure lbVariableFieldsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbVariableFieldsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure lbDataFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbDataFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbVariableTotalsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbVariableTotalsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure tvBandsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvBandsDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
{$IFDEF D6Above}
    fDesigner : IDesigner;
{$ELSE}
    fDesigner : IFormDesigner;
{$ENDIF}
    fReport : TZReport;
    None      : String;

    procedure SetReport(const Value: TZReport);

    procedure acBandsUpdate;
    procedure acGroupsUpdate;
    procedure acVariablesUpdate;

    function  FindNode(Band: TZRCustomBand): TTreeNode;
    function  FindController(Controller : TZRCustomController): Integer;
    function  FindListBox(Variable: TZRVariable): TListBox;

    procedure FillBandsTree;
    procedure FillControllersList;
    procedure FillGroupsList(Band: TZRCustomBand);
    procedure FillVariablesList(Controller : TZRCustomController);

    procedure DeselectLists(Sender: TObject);
    procedure SortControllers;

    procedure ZRNotify(var Message: TZRNotify); message ZR_NOTIFY;
  public
    { Public declarations }
{$IFDEF D6Above}
    property Designer : IDesigner read fDesigner write fDesigner;
{$ELSE}
    property Designer : IFormDesigner read fDesigner write fDesigner;
{$ENDIF}
    property Report   : TZReport read fReport write SetReport;
  end;

implementation

uses ZRConst, ZRUtils;

{$R *.DFM}

procedure TZRDesignForm.FormCreate(Sender: TObject);
begin
  None             := LoadStr(szrDesignNone    );
  btClose .Caption := LoadStr(szrDesignClose   );

  tsBands .Caption := LoadStr(szrDesignTSBands );
  tsVars  .Caption := LoadStr(szrDesignTSVars  );

  gbBands .Caption := LoadStr(szrDesignGBBands );
  gbGroups.Caption := LoadStr(szrDesignGBGroups);
  tvBands .Hint    := LoadStr(szrDesignTVBands );
  lbGroups.Hint    := LoadStr(szrDesignLBGroups);

  acBandsNew    .Caption := LoadStr(szrDesignACBandsNew    );
  acBandsDelete .Caption := LoadStr(szrDesignACBandsDelete );
  acBandsRename .Caption := LoadStr(szrDesignACBandsRename );
  acGroupsNew   .Caption := LoadStr(szrDesignACGroupsNew   );
  acGroupsDelete.Caption := LoadStr(szrDesignACGroupsDelete);

  sbGroupsUp  .Caption := '';
  sbGroupsDown.Caption := '';

  gbData       .Caption     := LoadStr(szrDesignGBData        );
  grControllers.Hint        := LoadStr(szrDesignGRControllers );
  grControllers.Cells[0, 0] := LoadStr(szrDesignCellController);
  grControllers.Cells[1, 0] := LoadStr(szrDesignCellDataSet   );

  laDataFields    .Caption  := LoadStr(szrDesignLADataFields);
  lbDataFields    .Hint     := LoadStr(szrDesignLBDataFields);

  gbVariables      .Caption     := LoadStr(szrDesignGBVariables          );
  laVariableFields.Caption      := LoadStr(szrDesignLAVariableFields     );
  lbVariableFields.Hint         := LoadStr(szrDesignLBVariableFields     );
  laVariableExpressions.Caption := LoadStr(szrDesignLAVariableExpressions);
  lbVariableExpressions.Hint    := LoadStr(szrDesignLBVariableExpressions);
  laVariableTotals.Caption      := LoadStr(szrDesignLAVariableTotals     );
  lbVariableTotals.Hint         := LoadStr(szrDesignLBVariableTotals     );

  acVariablesField     .Caption := LoadStr(szrDesignACVariablesField     );
  acVariablesExpression.Caption := LoadStr(szrDesignACVariablesExpression);
  acVariablesTotal     .Caption := LoadStr(szrDesignACVariablesTotal     );
  acVariablesDelete    .Caption := LoadStr(szrDesignACVariablesDelete    );
end;

procedure TZRDesignForm.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TZRDesignForm.FormDestroy(Sender: TObject);
begin
  Report.DesignForm := nil;
end;

procedure TZRDesignForm.SetReport(const Value: TZReport);
begin
  if Report <> Value then begin
    fReport := Value;
    FillControllersList;
    FillBandsTree;
  end;
end;

procedure TZRDesignForm.FormShow(Sender: TObject);
var
  R : TGridRect;
begin
  Caption := Format('%s.%s', [Report.Owner.Name, Report.Name]);;

  tvBands.Selected  := tvBands.TopItem;

  R.Left := 0;  R.Top := 1;  R.Right := 1;  R.Bottom := 1;
  grControllers.Selection := R;

  DeselectLists(nil);

  pcDesignChange(Self);
end;

procedure TZRDesignForm.pcDesignChange(Sender: TObject);
var
  Dummy : Boolean;
begin
  if (pcDesign.ActivePage = tsBands) then begin
    tvBands.SetFocus;
    tvBandsClick(Self);
  end else
  if (pcDesign.ActivePage = tsVars ) then begin
    grControllers.SetFocus;
    grControllersSelectCell(Self, 0, grControllers.Selection.Top, Dummy);
  end;
end;

procedure TZRDesignForm.tvBandsGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.HasChildren then
    Node.ImageIndex := 1
  else
    Node.ImageIndex := 0;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TZRDesignForm.acBandsUpdate;
var
  IsSelected : Boolean;
  Band       : TZRCustomBand;
begin
  IsSelected := (tvBands.Selected <> nil);
  if IsSelected then
    Band := TZRCustomBand(tvBands.Selected.Data)
  else
    Band := nil;
  acBandsNew   .Enabled := IsSelected and
                           not (Band is TZRCustomController) and
                           not (Band.BandType in NoControllerBands) and
                           not (Band.HasController);
  acBandsRename.Enabled := IsSelected;
  acBandsDelete.Enabled := IsSelected and not (Band is TZReport);
end;

procedure TZRDesignForm.acGroupsUpdate;
begin
  acGroupsNew   .Enabled := (tvBands.Selected <> nil) and
                            (TZRCustomBand(tvBands.Selected.Data) is TZRCustomController);
  acGroupsDelete.Enabled := (lbGroups.ItemIndex >= 0);
  acGroupsUp    .Enabled := (lbGroups.ItemIndex > 0);
  acGroupsDown  .Enabled := (lbGroups.ItemIndex >= 0) and
                            (lbGroups.ItemIndex < lbGroups.Items.Count-1);
end;

procedure TZRDesignForm.acVariablesUpdate;
begin
  acVariablesField     .Enabled := True;
  acVariablesExpression.Enabled := True;
  acVariablesTotal     .Enabled := (lbVariableExpressions.Items.Count > 0) or
                                   (lbVariableFields     .Items.Count > 0);
  acVariablesDelete    .Enabled := (lbVariableExpressions.ItemIndex >= 0) or
                                   (lbVariableFields     .ItemIndex >= 0) or
                                   (lbVariableTotals     .ItemIndex >= 0);
end;

function TZRDesignForm.FindNode(Band: TZRCustomBand): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to tvBands.Items.Count-1 do
    if tvBands.Items[i].Data = Band then begin
      Result := tvBands.Items[i];
      break;
    end;
end;

function TZRDesignForm.FindController(Controller: TZRCustomController): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 1 to grControllers.RowCount-1 do
    if (grControllers.Objects[0, i] = Controller) then begin
      Result := i;
      break;
    end;
end;

function TZRDesignForm.FindListBox(Variable: TZRVariable): TListBox;
begin
  if Variable is TZRField      then Result := lbVariableFields      else
  if Variable is TZRExpression then Result := lbVariableExpressions else
  if Variable is TZRAggregator then Result := lbVariableTotals      else
  Result := nil;
end;

procedure TZRDesignForm.FillBandsTree;

  procedure AddController(Controller: TZRCustomController; Node: TTreeNode); forward;

  procedure AddBand(Band: TZRCustomBand; Node: TTreeNode);
  var
    N : TTreeNode;
  begin
    if Assigned(Band) then begin
      N := tvBands.Items.AddChildObject(Node, Band.Name, Band);
      AddBand(Band.ChildBand, N);
      AddController(TZRCustomController(Band.SubController), N);
    end;
  end;

  procedure AddController(Controller: TZRCustomController; Node: TTreeNode);
  var
    BT : TZRBandType;
    i  : Integer;
  begin
    if Assigned(Controller) then begin
      for BT := zbtPageHeader to Pred(zbtGroupHeader) do
        AddBand(Controller.GetBand(BT), Node);
      for i := 0 to Controller.GroupList.Count-1 do
        AddBand(TZRGroup(Controller.GroupList[i]).Header, Node);
      for BT := Succ(zbtGroupHeader) to Pred(zbtGroupFooter) do
        AddBand(Controller.GetBand(BT), Node);
      for i := Controller.GroupList.Count-1 downto 0 do
        AddBand(TZRGroup(Controller.GroupList[i]).Footer, Node);
      for BT := Succ(zbtGroupFooter) to zbtPageFooter do
        AddBand(Controller.GetBand(BT), Node);
    end;
  end;

begin
  tvBands.Items.Clear;
  AddController(Report, tvBands.Items.AddChildObject(nil, Report.Name, Report));
  tvBands.Selected := tvBands.TopItem;
  acBandsUpdate;
end;

procedure TZRDesignForm.FillControllersList;
  procedure AddController(Controller : TZRCustomController);
  var
    i : Integer;
  begin
    if Assigned(Controller) then begin
      with grControllers do begin
        RowCount := RowCount + 1;
        Cells  [0, RowCount-1] := Controller.Name;
        if Assigned(Controller.DataSet) then
          Cells  [1, RowCount-1] := Controller.DataSet.Name
        else
          Cells  [1, RowCount-1] := None;
        Objects[0, RowCount-1] := Controller;
      end;
      for i := 0 to Controller.BandList.Count-1 do
        if TZRCustomBand(Controller.BandList[i]).BandType = zbtController then
          AddController(TZRCustomController(Controller.BandList[i]));
    end;
  end;
begin
  with grControllers do begin
    RowCount  := 1;
    AddController(Report);
    FixedRows := 1;
  end;
end;

procedure TZRDesignForm.FillGroupsList(Band: TZRCustomBand);
var
  Group     : TZRGroup;
  i         : Integer;
begin
  lbGroups.Clear;
  if (Band is TZRCustomController) then
    for i:= 0 to TZRCustomController(Band).GroupList.Count-1 do begin
      Group := TZRCustomController(Band).GroupList[i];
      lbGroups.Items.AddObject(Group.Name, Group);
    end;
  acGroupsUpdate;
end;

procedure TZRDesignForm.FillVariablesList(Controller : TZRCustomController);
  function HasField(FieldName: String): Boolean;
  var
    i : Integer;
    F : TZRField;
  begin
    for i := 0 to lbVariableFields.Items.Count-1 do begin
      F := TZRField(lbVariableFields.Items.Objects[i]);
      if (F.DataSet = F.Master.DataSet) and (F.DataField = FieldName) then begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
var
  Variable : TZRVariable;
  ListBox  : TListBox;
  Fields   : TStringList;
  i        : Integer;
begin
  lbVariableExpressions.Clear;
  lbVariableFields     .Clear;
  lbVariableTotals     .Clear;
  for i := 0 to Controller.VariableList.Count-1 do begin
    Variable := TZRVariable(Controller.VariableList[i]);
    ListBox  := FindListBox(Variable);
    ListBox.Items.AddObject(Variable.Name, Variable)
  end;
  Fields := TStringList.Create;
  if Assigned(Controller.DataSet) then
    Controller.DataSet.GetFieldNames(Fields);
  with lbDataFields do begin
    Items.BeginUpdate;
    Clear;
    for i := 0 to Fields.Count-1 do
      if not HasField(Fields[i]) then Items.Add(Fields[i]);
    Items.EndUpdate;
  end;
  Fields.Free;
  acVariablesUpdate;
end;

procedure TZRDesignForm.DeselectLists(Sender: TObject);
  procedure ClearList(ListBox: TListBox);
  var
    i: Integer;
  begin
    if ListBox.SelCount > 0 then
      for i := 0 to ListBox.Items.Count-1 do ListBox.Selected[i] := False;
    ListBox.ItemIndex := -1;
  end;
begin
  if (Sender <> lbGroups             ) then ClearList(lbGroups             );
  if (Sender <> lbDataFields         ) then ClearList(lbDataFields         );
  if (Sender <> lbVariableFields     ) then ClearList(lbVariableFields     );
  if (Sender <> lbVariableExpressions) then ClearList(lbVariableExpressions);
  if (Sender <> lbVariableTotals     ) then ClearList(lbVariableTotals     );
end;

function CompareNodes(lParam1, lParam2, lParamSort: Longint): Integer; stdcall;
  function CompareBands(B1, B2: TZRCustomBand): Integer;
  var
    M1, M2 : TZRCustomController;
    I1, I2 : Integer;
  begin
    M1 := B1.Master;
    M2 := B2.Master;
    if (M1 = nil) or (M2 = nil) then
      Result := 0
    else if M1 = M2 then begin
      I1:= M1.BandList.IndexOf(B1);
      I2:= M2.BandList.IndexOf(B2);
      if I1 > I2 then Result :=  1 else
      if I1 < I2 then Result := -1 else Result := 0;
    end else if B1 = M2 then begin
      if B2.BandType < zbtDetail then Result := 1 else Result := -1;
    end else if M1 = B2 then begin
      if B1.BandType < zbtDetail then Result := -1 else Result := 1;
    end else Result := 0;
  end;
begin
  Result := CompareBands(TZRCustomBand(TTreeNode(lParam1).Data), TZRCustomBand(TTreeNode(lParam2).Data));
end;

procedure TZRDesignForm.SortControllers;
var
  Index : Integer;

  procedure SortController(C : TZRCustomController);
  var
    p, i  : Integer;
    TempO : TObject;
    TempS : String;
  begin
    p := FindController(C);
    if (p >= 0) and (p <> Index) then begin
      TempO := grControllers.Objects[0, Index];
      grControllers.Objects[0, Index] := grControllers.Objects[0, p];
      grControllers.Objects[0, p] := TempO;
        TempS := grControllers.Cells[0, Index];
        grControllers.Cells[0, Index] := grControllers.Cells[0, p];
        grControllers.Cells[0, p] := TempS;
      TempS := grControllers.Cells[1, Index];
      grControllers.Cells[1, Index] := grControllers.Cells[1, p];
      grControllers.Cells[1, p] := TempS;
    end;
    Inc(Index);
    for i := 0 to C.BandList.Count-1 do
      if TZRCustomBand(C.BandList[i]).BandType = zbtController then
        SortController(TZRCustomController(C.BandList[i]));
  end;
begin
  Index := 1;
  SortController(Report);
end;

{ Event handling }

procedure TZRDesignForm.lbGroupsClick(Sender: TObject);
begin
  acGroupsUpdate;
  if (pcDesign.ActivePage = tsBands) and (lbGroups.ItemIndex >= 0) then
    Designer.SelectComponent(TZRGroup(lbGroups.Items.Objects[lbGroups.ItemIndex]));
end;

procedure TZRDesignForm.grControllersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Band: TZRCustomBand;
  Node: TTreeNode;
begin
  if (pcDesign.ActivePage = tsVars) and (ARow >= 1) then begin
    Band := TZRCustomBand(grControllers.Objects[0, ARow]);
    Node := FindNode(Band);
    if Node <> nil then begin
      tvBands.Selected := Node;
      tvBandsClick(Self);
    end;
    FillVariablesList(TZRCustomController(Band));
  end;
end;

procedure TZRDesignForm.tvBandsClick(Sender: TObject);
begin
  tvBandsChange(Sender, tvBands.Selected);
  Designer.SelectComponent(TZRCustomBand(tvBands.Selected.Data));
end;

procedure TZRDesignForm.tvBandsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  tvBandsClick(tvBands);
end;

procedure TZRDesignForm.tvBandsChange(Sender: TObject; Node: TTreeNode);
var
  Band : TZRCustomBand;
  R    : TGridRect;
begin
  Band := TZRCustomBand(tvBands.Selected.Data);
  acBandsUpdate;
  FillGroupsList(Band);
  if (pcDesign.ActivePage = tsBands) then begin
    if Band.BandType <> zbtController then Band := Band.Master;
    R.Left := 0; R.Right := 1;
    R.Top := FindController(Band as TZRCustomController);
    R.Bottom := R.Top;
    grControllers.Selection := R;
  end;
end;

procedure TZRDesignForm.tvBandsEdited(Sender: TObject; Node: TTreeNode; var S: String);
var
  Band: TZRCustomBand;
begin
  Band := TZRCustomBand(Node.Data);
  try
    Band.Name := S;
    if Screen.ActiveForm = Self then begin
      Designer.SelectComponent(nil);
      Designer.SelectComponent(Band);
    end;
  except
    S := Band.Name;
  end;
end;

procedure TZRDesignForm.lbDataFieldsClick(Sender: TObject);
begin
  DeselectLists(Sender);
  Designer.SelectComponent(TZRCustomBand(grControllers.Objects[0, grControllers.Selection.Top]));
end;

procedure TZRDesignForm.lbVariablesClick(Sender: TObject);
var
  B : TListBox;
  i : Integer;
begin
  DeselectLists(Sender);
  B := (Sender as TListBox);
  i := B.ItemIndex;
  if (pcDesign.ActivePage = tsVars) and (i >= 0) and (i < B.Items.Count) then
    Designer.SelectComponent(TZRVariable(B.Items.Objects[i]));
  acVariablesUpdate;
end;

{ Drag & drop }

procedure TZRDesignForm.tvBandsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ChildBand  : TZRCustomBand;
  ParentBand : TZRCustomBand;
  BT         : TZRBandType;
begin
  Accept := (Source = Sender) and
            ((Source as TTreeView).Selected <> nil) and
            ((Sender as TTreeView).DropTarget <> nil);
  if Accept then begin
    ChildBand  := (Source as TTreeView).Selected.Data;
    ParentBand := (Sender as TTreeView).DropTarget.Data;
    //Accept := (ChildBand.Master = ParentBand.Master);
    if Accept then for BT := Low(TZRChildBands) to High(TZRChildBands) do
      if (ChildBand.BandType = BT) and (ChildBand.GetParentLink(BT) <> nil) then begin
        Accept := not (ParentBand is TZReport) and (ParentBand.GetChildLink(BT) = nil);
        Exit;
      end;
    Accept := False;
  end;
end;

procedure TZRDesignForm.tvBandsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ChildBand  : TZRCustomBand;
  ParentBand : TZRCustomBand;
begin
  ChildBand  := (Source as TTreeView).Selected.Data;
  ParentBand := (Sender as TTreeView).DropTarget.Data;
  ChildBand.SetParentLink(ChildBand.BandType, ParentBand);
  FillBandsTree;
  tvBands.Selected := FindNode(ChildBand);
  Designer.Modified;
end;

procedure TZRDesignForm.lbVariableFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbDataFields);
end;

procedure TZRDesignForm.lbVariableFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  C : TZRCustomController;
  L : TStringList;

  procedure AddFieldVariable(FieldName: String);
  var
    i : Integer;
    V : TZRVariable;
  begin
    for i := 0 to C.VariableList.Count-1 do begin
      V := TZRVariable(C.VariableList[i]);
      if (V is TZRField) and (TZRField(V).DataField = FieldName) then Exit;
    end;
    V := C.CreateVariable(TZRField, FieldName);
    with TZRField(V) do begin
      DataSet   := C.DataSet;
      DataField := FieldName;
    end;
    Designer.Modified;
  end;

var
  i : Integer;
begin
  C := TZRCustomController(grControllers.Objects[0, grControllers.Selection.Top]);
  L := TStringList.Create;
  try
    for i := 0 to lbDataFields.Items.Count-1 do
      if lbDataFields.Selected[i] then L.Add(lbDataFields.Items[i]);
    for i := 0 to L.Count-1 do
      AddFieldVariable(L[i]);
  finally
    FillVariablesList(C);
    L.Free;
  end;
end;

procedure TZRDesignForm.lbDataFieldsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbVariableFields);
end;

procedure TZRDesignForm.lbDataFieldsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  acVariablesDeleteExecute(Source);
end;

procedure TZRDesignForm.lbVariableTotalsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbVariableFields) or
            (Source = lbVariableExpressions);
end;

procedure TZRDesignForm.lbVariableTotalsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  C : TZRCustomController;
  L : TList;
  i : Integer;

  procedure AddTotalVariable(Variable: TZRVariable);
  var
    V : TZRVariable;
    N : String;
  begin
    if (Variable is TZRField) and
       (Variable.Name = 'zrv' + TZRField(Variable).DataField) then
      N := TZRField(Variable).DataField
    else if (copy(Variable.Name, 1, 3) = 'zrv') then
      N := copy(Variable.Name, 4, length(Variable.Name))
    else
      N := '';
    {
    if copy(Variable.Name, 1, length(C.Name)) = C.Name then
      N := copy(Variable.Name, length(C.Name)+1, 128)
    else
      N := '';
    }
    V := C.CreateVariable(TZRAggregator, 'Total'+N);
    TZRAggregator(V).Variable := Variable;
    Designer.Modified;
  end;

begin
  C := TZRCustomController(grControllers.Objects[0, grControllers.Selection.Top]);
  L := TList.Create;
  try
    for i := 0 to TListBox(Source).Items.Count-1 do
      if TListBox(Source).Selected[i] then L.Add(TListBox(Source).Items.Objects[i]);
    for i := 0 to L.Count-1 do
      AddTotalVariable(TZRVariable(L[i]));
  finally
    L.Free;
  end;
end;

{ Action processing }

procedure TZRDesignForm.acBandsNewExecute(Sender: TObject);
var
  Band : TZRCustomBand;
begin
  Band := TZRCustomBand(tvBands.Selected.Data);
  Band.HasController := True;
  Band := Band.SubController;
  Designer.SelectComponent(nil);
  Designer.SelectComponent(Band);
end;

procedure TZRDesignForm.acBandsRenameExecute(Sender: TObject);
begin
  tvBands.Selected.EditText;
end;

procedure TZRDesignForm.acBandsDeleteExecute(Sender: TObject);
begin
  if MessageDlg(
       Format(LoadStr(szrDesignSureBandDelete),
              [TZRCustomBand(tvBands.Selected.Data).Name]),
       mtConfirmation, [mbYes, mbCancel], 0) in [mrOK, mrYes] then begin
    TZRCustomBand(tvBands.Selected.Data).Free;
    Designer.SelectComponent(nil);
  end;
end;

procedure TZRDesignForm.acGroupsNewExecute(Sender: TObject);
var
  Controller : TZRCustomController;
  Group      : TZRGroup;
begin
  Controller := TZRCustomController(tvBands.Selected.Data);
  Group := Controller.CreateGroup;
  Designer.Modified;
  Designer.SelectComponent(nil);
  Designer.SelectComponent(Group);
end;

procedure TZRDesignForm.acGroupsDeleteExecute(Sender: TObject);
begin
  if MessageDlg(
       Format(LoadStr(szrDesignSureGroupDelete),
              [TZRGroup(lbGroups.Items.Objects[lbGroups.ItemIndex]).Name]),
       mtConfirmation, [mbYes, mbCancel], 0) in [mrOK, mrYes] then begin
    lbGroups.Items.Objects[lbGroups.ItemIndex].Free;
    Designer.Modified;
    Designer.SelectComponent(nil);
  end;
end;

procedure TZRDesignForm.acGroupsUpExecute(Sender: TObject);
var
  Group : TZRGroup;
begin
  Group := TZRGroup(lbGroups.Items.Objects[lbGroups.ItemIndex]);
  Group.Order := Group.Order - 1;
  Designer.Modified;
end;

procedure TZRDesignForm.acGroupsDownExecute(Sender: TObject);
var
  Group : TZRGroup;
begin
  Group := TZRGroup(lbGroups.Items.Objects[lbGroups.ItemIndex]);
  Group.Order := Group.Order + 1;
  Designer.Modified;
end;

procedure TZRDesignForm.acVariablesExecute(Sender: TObject);
var
  Controller : TZRCustomController;
  Variable   : TZRVariable;
begin
  Variable   := nil;
  Controller := TZRCustomController(grControllers.Objects[0, grControllers.Selection.Top]);
  if Sender = acVariablesField      then Variable := Controller.CreateVariable(TZRField     , 'Field'     ) else
  if Sender = acVariablesExpression then Variable := Controller.CreateVariable(TZRExpression, 'Expression') else
  if Sender = acVariablesTotal      then Variable := Controller.CreateVariable(TZRAggregator, 'Total'     );
  Designer.Modified;
  Designer.SelectComponent(nil);
  Designer.SelectComponent(Variable);
end;

procedure TZRDesignForm.acVariablesDeleteExecute(Sender: TObject);
var
  ListBox : TListBox;
  i       : Integer;
  L       : TList;
begin
  if lbVariableFields     .SelCount > 0 then ListBox := lbVariableFields      else
  if lbVariableExpressions.SelCount > 0 then ListBox := lbVariableExpressions else
  if lbVariableTotals     .SelCount > 0 then ListBox := lbVariableTotals      else Exit;
  L := TList.Create;
  try
    for i := 0 to ListBox.Items.Count-1 do
      if ListBox.Selected[i] then L.Add(ListBox.Items.Objects[i]);
    for i := 0 to L.Count-1 do
      TObject(L[i]).Free;
  finally
    Designer.Modified;
    Designer.SelectComponent(nil);
    FillVariablesList(TZRCustomController(grControllers.Objects[0, grControllers.Selection.Top]));
    L.Free;
  end;
end;

{ Report notifications handling }

procedure TZRDesignForm.ZRNotify(var Message: TZRNotify);
var
  Band     : TZRCustomBand;
  Node     : TTreeNode;
  Index    : Integer;
  Group    : TZRGroup;
  Variable : TZRVariable;
  ListBox  : TListBox;
  i        : Integer;
begin
  if (Message.Sender is TZRCustomBand) then begin
    Band:= TZRCustomBand(Message.Sender);
    case Message.Operation of

      zopInsert : begin
        Node := FindNode(Band.Master);
        if Node <> nil then begin
          if Band.Master <> Report then Node := Node.Parent;
          tvBands.Items.AddChildObject(Node, Band.Name, Band);
        end;
        if Band.BandType = zbtController then begin
          Index := FindController(Band.Master);
          if Index >= 0 then with grControllers do begin
            RowCount := RowCount + 1;
            for i := RowCount-1 downto Index+1 do begin
              Cells  [0, i] := Cells  [0, i-1];
              Cells  [1, i] := Cells  [1, i-1];
              Objects[0, i] := Objects[0, i-1];
            end;
            Cells  [0, Index] := Band.Name;
            if Assigned(TZRCustomController(Band).DataSet) then
              Cells  [1, Index] := TZRCustomController(Band).DataSet.Name
            else
              Cells  [1, Index] := None;
            Objects[0, Index] := Band;
          end;
        end;
      end;

      zopChild  : begin
        Node := FindNode(Band);
        if Band.GetParentLink(Band.BandType) <> nil then begin
          Node.MoveTo(FindNode(Band.GetParentLink(Band.BandType)), naAddChild);
          Band := Band.GetParentLink(Band.BandType);
          if Band.HasChild then begin
            Node := FindNode(Band.ChildBand);
            Node.MoveTo(FindNode(Band), naAddChildFirst);
          end;
        end;
      end;

      zopRemove : begin
        Node := FindNode(Band);
        if Node <> nil then Node.Delete;
        if Band.BandType = zbtController then begin
          Index := FindController(Band as TZRCustomController);
          if Index >= 0 then with grControllers do begin
            for i := Index to RowCount-2 do begin
              Cells  [0, i] := Cells  [0, i+1];
              Cells  [1, i] := Cells  [1, i+1];
              Objects[0, i] := Objects[0, i+1];
            end;
            RowCount := RowCount-1;
          end;
        end;
      end;

      zopRename : begin
        Node := FindNode(Band);
        if Node <> nil then Node.Text := Band.Name;
        if Band = Report then Caption := Format('%s.%s', [Report.Owner.Name, Report.Name]);;
        if Band.BandType = zbtController then begin
          Index := FindController(Band as TZRCustomController);
          if Index >= 0 then grControllers.Cells[0, Index] := Band.Name;
        end;
      end;

      zopSort   : begin
        Node := FindNode(Band);
        if Node <> nil then begin
          if Band <> Report then Node := Node.Parent;
          Node.CustomSort(CompareNodes, 0);
        end;
        if Band.BandType = zbtController then SortControllers;
      end;

      zopDataSet: begin
        Index := FindController(Band as TZRCustomController);
        if (Index >= 0) then begin
          if Assigned(TZRCustomController(Band).DataSet) then
            grControllers.Cells[1, Index] := TZRCustomController(Band).DataSet.Name
          else
            grControllers.Cells[1, Index] := None;
          FillVariablesList(Band as TZRCustomController);
        end;
      end;

    end;

  end else
  if (Message.Sender is TZRGroup) then begin
    Group := TZRGroup(Message.Sender);
    case Message.Operation of
      zopInsert : begin
                    lbGroups.Items.InsertObject(Group.Order, Group.Name, Group);
                  end;
      zopRename : begin
                    Index := lbGroups.Items.IndexOfObject(Group);
                    if Index >=0 then lbGroups.Items[Index] := Group.Name;
                  end;
      zopRemove : begin
                    Index := lbGroups.Items.IndexOfObject(Group);
                    if Index >= 0 then lbGroups.Items.Delete(Index);
                  end;
      zopSort   : begin
                    Index := lbGroups.Items.IndexOfObject(Group);
                    lbGroups.Items.Move(Index, Group.Order);
                  end;
    end;
    lbGroups.ItemIndex := Group.Order;
    if ActiveControl = lbGroups then lbGroupsClick(lbGroups);
  end else
  if (Message.Sender is TZRVariable) then begin
    Variable := TZRVariable(Message.Sender);
    ListBox  := FindListBox(Variable);
    case Message.Operation of
      zopInsert : begin
                    ListBox.Items.AddObject(Variable.Name, Variable);
                  end;
      zopRename : begin
                    Index := ListBox.Items.IndexOfObject(Variable);
                    if Index >= 0 then ListBox.Items[Index] := Variable.Name;
                  end;
      zopRemove : begin
                    Index := ListBox.Items.IndexOfObject(Variable);
                    if Index >= 0 then ListBox.Items.Delete(Index);
                  end;
      zopSort   : begin
                  end;
    end;
    if ActiveControl = ListBox then lbVariablesClick(ListBox);
  end;
end;

end.

