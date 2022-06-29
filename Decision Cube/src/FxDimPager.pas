unit FxDimPager;

interface

uses Windows, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, ValEdit, FxCommon, FxDb,
  ImgList, FxMap, FxCache;

const
  doOrient='Layout';
  doState='State';
  doScale='Scale';
  OrientValues:array[0..1] of string=('Row','Column');
  StateValues:array[0..1] of string=('Hide','Show');
  ScaleValues:array[0..4]of string=('','Year','Quarter','Month','Week');
  BinValues:array [TBinType]of string=('','Year','Quarter','Month','Week','Set','Custom');
type

  TExEditor=class(TValueListEditor)
  public
    procedure AddCombo
     (const AName:string; const Values:array of string; Append:Boolean=True);
  end;
  TEllipsEvent=procedure (Sender:TObject; Param:Integer; var Value:string)of object;

  TDimPager=class;

  TPagerDataLink=class(TDecisionDataLink)
  private
    FPager: TDimPager;
  protected
    procedure DecisionDataEvent(Event: TDecisionDataEvent); override;
  public
    constructor Create(Pager: TDimPager);
    destructor Destroy; override;
    property Pager:TDimPager read FPager;
  end;

  TDimToolBar=class(TToolBar)
  public
    constructor Create(AOwner:TComponent);override;
  end;

  TDimEditor=class(TExEditor)
  public
    constructor Create(AOwner:TComponent);override;
  end;

  TDrillBox=class(TListBox)
  public
    constructor Create(AOwner:TComponent);override;
  end;

  TDimTab=class(TTabSheet)
  private
    FBox:TDrillBox;
    FEditor:TDimEditor;
    FDim: Integer;
    FTools:TDimToolBar;
    function GetDecisionSource: TFxSource;
    function GetPageControl: TDimPager;
    procedure BinDim(const Value:string);
    procedure DrillValue(Sender:TObject);
    procedure EditorChanged(Sender:TObject);
    procedure EyeClick(Sender:TObject);
    procedure LayClick(Sender:TObject);
    procedure MoveDim(const Value:string);
    procedure PinClick(Sender:TObject);
    procedure SetDim(const Value: Integer);
    procedure ShowTab;
    procedure ToggleState(const Value:string);
    procedure OnEditBtn(Sender:TObject);
    procedure SetPageControl(const Value: TDimPager);
  protected
    procedure DoShow;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property DecisionSource:TFxSource read GetDecisionSource;
    property Dim:Integer read FDim write SetDim;
    property PageControl:TDimPager read GetPageControl write SetPageControl;
  end;

  TDimPager=class(TPageControl)
  private
    FDataLink:TPagerDataLink;
    FImages:TImageList;
    FOnBinChanged:TNotifyEvent;
    FOnEditBtnClick:TEllipsEvent;
    function GetActivePage:TDimTab;
    function GetDecisionSource:TFxSource;
    procedure NewPanelSetup;
    procedure Pivot;
    procedure SetDecisionSource(const Value: TFxSource);
    procedure SetActivePage(const Value: TDimTab);
    function GetPages(Index: Integer): TDimTab;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property DataLink:TPagerDataLink read FDataLink;
    property Images:TImageList read FImages;
    property Pages[Index:Integer]:TDimTab read GetPages;
  published
    property ActivePage:TDimTab read GetActivePage write SetActivePage;
    property DecisionSource:TFxSource read GetDecisionSource write SetDecisionSource;
    property OnBinChanged:TNotifyEvent read FOnBinChanged write FOnBinChanged;
    property OnEditBtnClick:TEllipsEvent read FOnEditBtnClick write FOnEditBtnClick;
  end;

  TSumCombo=class;

  TComboDataLink=class(TDecisionDataLink)
  private
    FCombo:TSumCombo;
  protected
    procedure DecisionDataEvent(Event: TDecisionDataEvent); override;
  public
    constructor Create(Combo: TSumCombo);
    destructor Destroy; override;
    property Combo:TSumCombo read FCombo;
  end;

  TSumCombo=class(TCustomComboBox)
  private
    procedure SetDecisionSource(const Value: TFxSource);
    function GetDecisionSource: TFxSource;
  private
    FDataLink:TComboDataLink;
    procedure ChangeSummary;
    procedure DataEvent;
    property DataLink:TComboDataLink read FDataLink;
  protected
    procedure Change;override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    property DecisionSource:TFxSource read GetDecisionSource write SetDecisionSource;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
//    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
//    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
//    property Items; { Must be published after OnMeasureItem }
  end;

implementation

uses SysUtils, FxStore, Db, Grids, StrUtils, Graphics, Forms;

{ TPagerDataLink }

constructor TPagerDataLink.Create(Pager: TDimPager);
begin
  inherited Create;
  FPager:=Pager;
end;

procedure TPagerDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
  if FBlocked then Exit;
  FBlocked:=True;
  try
    case Event of
      xeSummaryChanged:
        Pager.Invalidate;
      xePivot:
        Pager.Pivot;
      xeStateChanged,
      xeNewMetaData:
        Pager.NewPanelSetup;
      xeSourceChange:
        Pager.NewPanelSetup;
    end;
  finally
    FBlocked := False;
  end;
end;

destructor TPagerDataLink.Destroy;
begin
  inherited;
end;

{ TDimPager }

const
  Bitmaps:array[0..5]of string=
   ('EyeClose','EyeOpen','PIND','PINU','VERT','HORIZ');

constructor TDimPager.Create(AOwner: TComponent);
var
  I:Integer;
//  H:LongWord;
begin
  inherited Create(AOwner);
  FDataLink:=TPagerDataLink.Create(Self);
  FDataLink.FPager:=Self;
  FImages:=TImageList.Create(Self);
  FImages.Height:=12;
  FImages.Width :=16;
//  H:=FindClassHInstance(TDimPager);
  for I:=0 to High(Bitmaps)do
    FImages.GetInstRes(HInstance,rtBitmap,BitMaps[I],16,[],clTeal);
//  NewPanelSetup;
end;

destructor TDimPager.Destroy;
begin
  FreeAndNil(FImages);
  FDataLink.Free;
  inherited Destroy;
end;

function TDimPager.GetActivePage: TDimTab;
begin
  Result:=TDimTab(inherited ActivePage);
end;

function TDimPager.GetDecisionSource: TFxSource;
begin
   Result:=FDataLink.DecisionSource;
end;

function TDimPager.GetPages(Index: Integer): TDimTab;
begin
  Result:=TDimTab(inherited Pages[Index]);
end;

procedure TDimPager.NewPanelSetup;
var
  I: Integer;
  Tab:TDimTab;
begin
  if csDestroying in ComponentState then Exit;
  if not assigned(DecisionSource) or not DecisionSource.Ready then begin
    while Self.PageCount>0 do Pages[0].Free;
    Exit;
  end;
  with DecisionSource.DecisionCube.DataCache do begin
    for I:=0 to Dimensions.Count-1 do begin
      if I<PageCount then begin
        Pages[I].Dim:=I;
        Pages[I].FTools.Images:=FImages;
      end else begin
        Tab:=TDimTab.Create(Self);
        //Tab.Name:=Dimensions[I].Name;
        Tab.Caption:=Dimensions[I].Name;
        Tab.PageControl:=Self;
        Tab.FTools.Images:=FImages;
        Tab.Dim:=I;
      end;
    end;
    while PageCount>Dimensions.Count do
      Pages[PageCount-1].Free;
  end;
  if ActivePage<>nil then ActivePage.ShowTab;
end;

procedure TDimPager.Pivot;
begin
  if ActivePage<>nil then ActivePage.ShowTab;
end;

procedure TDimPager.SetActivePage(const Value: TDimTab);
begin
  inherited ActivePage := Value;
end;

procedure TDimPager.SetDecisionSource(const Value: TFxSource);
begin
  DataLink.DecisionSource := Value;
end;

{ TDimTab }

procedure TDimTab.BinDim(const Value: string);
var
  Map:TFxMap;
begin
  with DecisionSource do begin
    Map:=DecisionCube.DimensionMap;
    PageControl.DataLink.FBlocked:=True;
    try
      case AnsiIndexStr(Value,ScaleValues)of
        0: Map[Dim].BinType:=binNone;
        1: Map[Dim].BinType:=binYear;
        2: Map[Dim].BinType:=binQuarter;
        3: Map[Dim].BinType:=binMonth;
        4: Map[Dim].BinType:=binWeek;
      end;
    finally
      PageControl.DataLink.FBlocked:=False;
      if Assigned(PageControl.OnBinChanged)then
        PageControl.OnBinChanged(Self);
    end;
  end;
end;

const
  BtnHints:array [0..2] of string=
    ('Show/Hide','Column/Row','Drill');

constructor TDimTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(FTools)then begin
    FTools:=TDimToolBar.Create(Self);
    FTools.FreeNotification(Self);
  end;
  if not Assigned(FEditor)then begin
    FEditor:=TDimEditor.Create(Self);
    FEditor.FreeNotification(Self);
    FEditor.OnEditButtonClick:=OnEditBtn;
  end;
  if not Assigned(FBox)then begin
    FBox:=TDrillBox.Create(Self);
    FBox.Align:=alClient;
  end;
end;

destructor TDimTab.Destroy;
begin
//  FEditor.Free;
//  FBox.Free;
  inherited;
end;

procedure TDimTab.DoShow;
begin
  inherited;
  if Assigned(FEditor)and Assigned(FBox) then
    ShowTab;
end;

procedure TDimTab.DrillValue(Sender:TObject);
begin
  with Sender as TListBox, DecisionSource do begin
    PageControl.FDataLink.FBlocked:=True;
    DrillValue(Dim,Integer(Items.Objects[ItemIndex]));
    PageControl.FDataLink.FBlocked:=False;
  end;
end;

procedure TDimTab.EditorChanged(Sender: TObject);
const
  Table:array [0..2]of string=(doOrient,doState,doScale);
var
  Key:string;
begin
  with Sender as TValueListStrings, FEditor do begin
    Key:=Keys[Row];
    case AnsiIndexStr(Key,Table)of
      0: MoveDim(Values[Key]);
      1: ToggleState(Values[Key]);
      2: BinDim(Values[Key]);
    end;
  end;
end;

procedure TDimTab.EyeClick(Sender: TObject);
begin
  with Sender as TToolButton do begin
    DecisionSource.ToggleDim(Dim);
  end;
end;

function TDimTab.GetDecisionSource: TFxSource;
begin
  Result:=PageControl.DecisionSource;
end;

function TDimTab.GetPageControl: TDimPager;
begin
  Result:=inherited PageControl as TDimPager;
end;

procedure TDimTab.LayClick(Sender: TObject);
begin
  with DecisionSource,Pivot[Dim]^ do begin
    if iGroup=dgRow then
      MoveDimIndexes(iGroup,dgCol,iIndex,0,False)
    else if iGroup=dgCol then
      MoveDimIndexes(iGroup,dgRow,iIndex,0,False);
  end;
end;

procedure TDimTab.MoveDim(const Value:string);
var
  ToGroup:TDimGroup;
begin
  with DecisionSource,Pivot[Dim]^ do begin
    case AnsiIndexStr(Value,OrientValues)of
      0: ToGroup:=dgRow;
      1: ToGroup:=dgCol;
    else
      ToGroup:=dgRow;
      Assert(False);
    end;
    MoveDimIndexes(iGroup,ToGroup,iIndex,0,False);
  end;
end;

procedure TDimTab.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTools) and (Operation = opRemove) then
    FTools:= nil;
  if (AComponent = FEditor) and (Operation = opRemove) then
    FEditor:= nil;
  if (AComponent = FBox) and (Operation = opRemove) then
    FBox:= nil;
end;

procedure TDimTab.OnEditBtn(Sender: TObject);
var
  Key,Str:string;
  Map:TFxMap;
begin
  if Assigned(PageControl.FOnEditBtnClick) then begin
    PageControl.OnEditBtnClick(Self,2,Str);
    with FEditor do begin
      Key:=Keys[Row];
      Values[Key]:=Str;
    end;
    Map:=TFxMap.Create(Self,TFxMapItem);
    try
      Map.Assign(PageControl.DecisionSource.DecisionCube.DimensionMap);
      PageControl.DecisionSource.DecisionCube.Refresh(Map,True);
    finally
      Map.Free;
    end;
  end;
end;

procedure TDimTab.PinClick(Sender: TObject);
begin
  with Sender as TToolButton do begin
    if DecisionSource.Pivot[Dim]^.iState=dmDrilled then
      DecisionSource.ToggleDim(Dim)
    else
      DecisionSource.DrillValue(Dim,-1);
  end;
end;

procedure TDimTab.SetDim(const Value: Integer);
var
  R:Integer;
  IsDate,Exists:Boolean;
  Def:TFxMapItem;
begin
  FDim:=Value;
  with DecisionSource,FEditor do begin
    Def:=DimDef(Dim);
    Self.Caption:=Def.Name;
    IsDate :=Def.Field.DataType in [ftDate,ftDateTime];
    Exists :=FindRow(doScale,R);
    if IsDate and not Exists then
      AddCombo(doScale,ScaleValues,True)
    else if not IsDate and Exists then
      DeleteRow(R);
    ClientHeight:=RowCount*(DefaultRowHeight+GridLineWidth);
  end;
end;

procedure TDimTab.SetPageControl(const Value: TDimPager);
begin
  inherited PageControl:=Value;
end;

type
  TB=class(TToolButton);

procedure TDimTab.SetParent(AParent: TWinControl);
var
  I:Integer;
  Btn:TToolButton;
begin
  inherited SetParent(AParent);
  if FTools<>nil then begin
    FTools.Parent := Self;
    if FTools.ButtonCount=0 then begin
      for I:=0 to 2 do begin
        Btn:=TToolButton.Create(FTools);
        TB(Btn).SetToolBar(FTools);
        Btn.Hint:=BtnHints[2-I];
        Btn.ImageIndex:=2-I;
      end;
      FTools.Buttons[0].OnClick:=EyeClick;
      FTools.Buttons[1].OnClick:=LayClick;
      FTools.Buttons[2].OnClick:=PinClick;
    end;
  end;
  if FEditor<>nil then begin
    FEditor.Top:=FTools.Height;
    FEditor.Parent := Self;
    FEditor.Visible:= True;
    with FEditor do
      ClientHeight:=RowCount*(DefaultRowHeight+GridLineWidth);
  end;
  if FBox<>nil then begin
    FBox.Parent := Self;
    FBox.Visible:= False;
  end;
end;

procedure TDimTab.ShowTab;
var
  I:Integer;
begin
  with DecisionSource,FEditor,Pivot[Dim]^ do begin
    OnStringsChange:=nil;
    case iGroup of
      dgRow: Values[doOrient]:=OrientValues[0];
      dgCol: Values[doOrient]:=OrientValues[1];
    else
      Values[doState]:='';
    end;
    if iGroup=dgCol then
      FTools.Buttons[1].ImageIndex:=4
    else
      FTools.Buttons[1].ImageIndex:=5;
    with FTools.Buttons[0]do begin
      Down:= iState=dmOpen;
      ImageIndex:=Ord(iState=dmOpen);
    end;
    Values[doState]:=StateValues[Ord(iState in [dmOpen])];
    with FTools.Buttons[2] do begin
      Down:= iState=dmDrilled;
      ImageIndex:=2+Ord(Down);
    end;
    if Ready and FindRow(doScale,I) then
      Values[Keys[I]]:=BinValues[DimDef(Dim).BinType];
    ClientHeight:=RowCount*(DefaultRowHeight+GridLineWidth);
    OnStringsChange:=EditorChanged;
  end;
  with DecisionSource,FBox,Pivot[Dim]^ do begin
    OnClick:=nil;
    Visible:= (iState=dmDrilled);// and (iValue<>-1);
    Items.BeginUpdate;
    try
      Clear;
      if Visible then begin
        for I:=0 to DecisionCube.DataCache.Dimensions[Dim].Count-1 do
          Items.AddObject(GetMemberAsString(Dim,I),Pointer(I));
      end;
      if iValue<>-1 then begin
        ItemIndex:=Items.IndexOfObject(Pointer(iValue));
      end;
    finally
      Items.EndUpdate;
      OnClick:=Self.DrillValue;
    end;
  end;
  Invalidate;
end;

procedure TDimTab.ToggleState(const Value: string);
begin
  with DecisionSource do begin
    ToggleDim(Dim);
  end;
end;

{ TExEditor }

procedure TExEditor.AddCombo(const AName: string;
  const Values: array of string; Append:Boolean=True);
var
  I:Integer;
begin
  InsertRow(AName,'',Append);
  with ItemProps[AName]do begin
    EditStyle:=esPickList;
    ReadOnly:=True;
    for I:=0 to High(Values)do
      PickList.Add(Values[I]);
  end;
end;

{ TComboDataLink }

constructor TComboDataLink.Create(Combo: TSumCombo);
begin
  inherited Create;
  FCombo:=Combo;
end;

procedure TComboDataLink.DecisionDataEvent(Event: TDecisionDataEvent);
begin
  if FBlocked then Exit;
  FBlocked:=True;
  try
    case Event of
      xeSummaryChanged: Combo.ChangeSummary;
      xePivot: ;
      xeStateChanged,
      xeNewMetaData,
      xeSourceChange: Combo.DataEvent;
    end;
  finally
    FBlocked := False;
  end;
end;

destructor TComboDataLink.Destroy;
begin

  inherited;
end;

{ TSumCombo }

procedure TSumCombo.Change;
var
  I:Integer;
begin
  inherited;
  I:=0;
  if ItemIndex<>-1 then I:=Integer(Items.Objects[ItemIndex]);
  DecisionSource.SetCurrentSummary(I);
end;

procedure TSumCombo.ChangeSummary;
begin
  ItemIndex:=Items.IndexOfObject(Pointer(DecisionSource.CurrentSum));
end;

constructor TSumCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink:=TComboDataLink.Create(Self);
end;

procedure TSumCombo.DataEvent;
var
  I:Integer;
  S:TAbstractSummary;
begin
  if csDestroying in ComponentState then Exit;
  Items.BeginUpdate;
  try
    Clear;
    Enabled:=False;
    if (DecisionSource<>nil) and DecisionSource.Active then
    with DecisionSource do begin
      for I:=0 to DecisionCube.Summaries.Count-1 do begin
        S:=DecisionCube.Summaries[I];
        if S.Def.Visible then
          Items.AddObject(S.Name,Pointer(I));
      end;
      ItemIndex:=Items.IndexOfObject(Pointer(CurrentSum));
      Enabled:=True;
    end;
  finally
    Items.EndUpdate;
  end;
end;

destructor TSumCombo.Destroy;
begin
  FDataLink.Free;
  inherited;
end;

function TSumCombo.GetDecisionSource: TFxSource;
begin
  Result:=DataLink.DecisionSource;
end;

procedure TSumCombo.SetDecisionSource(const Value: TFxSource);
begin
  DataLink.DecisionSource:=Value;
end;

{ TDimToolBar }

constructor TDimToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='DimToolBar';
  SetSubComponent(True);
  Flat:=True;
end;

{ TDimEditors }

constructor TDimEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='DimEditor';
  SetSubComponent(True);
  DisplayOptions:=[doAutoColResize];
  Align:=alTop;
  BorderStyle:=bsNone;
  Color:=clWindow;
  Ctl3D:=False;
  DefaultColWidth:=50;
  DefaultRowHeight:=16;
  FixedCols:=1;
  FixedColor:=clBtnFace;
  Options:=Options-[goFixedVertLine];
  TitleCaptions[0]:='Parameter';
  TitleCaptions[1]:='Value';
  AddCombo(doState,StateValues);
  AddCombo(doOrient,OrientValues);
end;

{ TDrillBox }

constructor TDrillBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='DrillBox';
  SetSubComponent(True);
  Visible:=False;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TDimPager, TControl);
  GroupDescendentsWith(TSumCombo, TControl);
  RegisterClass(TDimTab  );
  RegisterClass(TDimPager);
  RegisterClass(TSumCombo);
end.

