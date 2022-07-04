unit o_GTDBColumnManager;

interface
uses
   Classes
  ,DBGrids
  ,DB
  ,Forms
  ,ComCtrls
  ,Controls
  ,StdCtrls
  ,ExtCtrls
  ,Contnrs, Grids
  ;

type
{------------------------------------------------------------------------------}
  TgtFrmColumnManager = class;
{------------------------------------------------------------------------------}
  TgtColMgrCaptions = class(TPersistent)
  private
    FBtnCancelCaption: string;
    FLabelFieldName: string;
    FLabelColor: string;
    FLabelVisible: string;
    FLabelFont: string;
    FColumnTitleCaption: string;
    FLabelTitleCaption: string;
    FColumnFieldName: string;
    FLabelReadOnly: string;
    FLabelWidth: string;
    FFormCaption: string;
    FBtnOkCaption: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create;
    destructor  Destroy;override;
  public
    procedure Assign(Source : TPersistent);override;
  published
    { Published declarations}
    property FormCaption       : string read FFormCaption        write FFormCaption;
    property BtnOk             : string read FBtnOkCaption       write FBtnOkCaption;
    property BtnCancel         : string read FBtnCancelCaption   write FBtnCancelCaption;
    property LabelFieldName    : string read FLabelFieldName     write FLabelFieldName;
    property LabelTitleCaption : string read FLabelTitleCaption  write FLabelTitleCaption;
    property LabelFont         : string read FLabelFont          write FLabelFont;
    property LabelWidth        : string read FLabelWidth         write FLabelWidth;
    property LabelColor        : string read FLabelColor         write FLabelColor;
    property LabelVisible      : string read FLabelVisible       write FLabelVisible;
    property LabelReadOnly     : string read FLabelReadOnly      write FLabelReadOnly;
    property ColumnFieldName   : string read FColumnFieldName    write FColumnFieldName;
    property ColumnTitleCaption: string read FColumnTitleCaption write FColumnTitleCaption;
  end;
{------------------------------------------------------------------------------}
  TgtDBColumnManager = class(TComponent)
  private
    FStoragePath: string;
    FFileExt: string;
    FDBGrid: TDBGrid;
    FCaptions: TgtColMgrCaptions;
    procedure SetDBGrid(const Value: TDBGrid);
    procedure SetFileExt(const Value: string);
    procedure SetCaptions(const Value: TgtColMgrCaptions);
    procedure SetStoragePath(const Value: string);
    function GetGridDataSet: TDataSet;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent : TComponent; Operation : TOperation);override;
    procedure ParseColumns;
    procedure ParseFieldNames;
    procedure ApplyCaptions;
    procedure InternalOnStatusChange(Sender :TObject);
  protected
    FForm            : TgtFrmColumnManager;
    FOldStatusChange : TNotifyEvent;
    FColumnsLoaded   : Boolean;
    property GridDataSet : TDataSet      read GetGridDataSet;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    procedure SaveColumns;
    procedure LoadColumns;
    procedure ManageColumns;
  published
    { Published declarations}
    property DBGrid      : TDBGrid           read FDBGrid       write SetDBGrid;
    property StoragePath : string            read FStoragePath  write SetStoragePath;
    property FileExt     : string            read FFileExt      write SetFileExt;
    property Captions    : TgtColMgrCaptions read FCaptions     write SetCaptions;
  end;
{------------------------------------------------------------------------------}
  TgtFrmColumnManager = class(TForm)
    TabControl1: TTabControl;
    TabControl2: TTabControl;
    btnOk: TButton;
    btnCancel: TButton;
    ColumnsListView: TListView;
    GroupBox1: TGroupBox;
    DBGridPreview: TDBGrid;
    Splitter1: TSplitter;
    TabControl3: TTabControl;
    Image1: TImage;
    lblFieldName: TLabel;
    cmbFieldNames: TComboBox;
    lblTitleCaption: TLabel;
    edtTitleCaption: TEdit;
    lblWidth: TLabel;
    edtColumnWidth: TEdit;
    UpDown1: TUpDown;
    lblFont: TLabel;
    edtColumnFont: TEdit;
    btnSetFont: TButton;
    lblColor: TLabel;
    btnSetColor: TButton;
    shpColumnColor: TShape;
    chkBoxVisible: TCheckBox;
    chkReadOnly: TCheckBox;
    procedure ColumnsListViewClick(Sender: TObject);
    procedure cmbFieldNamesSelect(Sender: TObject);
    procedure edtTitleCaptionChange(Sender: TObject);
    procedure edtColumnWidthChange(Sender: TObject);
    procedure btnSetFontClick(Sender: TObject);
    procedure btnSetColorClick(Sender: TObject);
    procedure chkBoxVisibleClick(Sender: TObject);
    procedure chkReadOnlyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ColumnsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FCurrentColumn : TColumn;
    procedure LoadColumnDetails(AColumn : TColumn);
    function GetColumnManager: TgtDBColumnManager;
  protected
    property ColumnManager : TgtDBColumnManager read GetColumnManager;
  public
    { Public declarations }
  end;
{------------------------------------------------------------------------------}

implementation
uses
   SysUtils
  ,Dialogs
  ;

{$R *.dfm}


const
  ERR_GRID_MUST_HAVE_A_NAME =
  'Grid must have a name!';

{ TgtColMgrCaptions }
{------------------------------------------------------------------------------}
constructor TgtColMgrCaptions.Create;
begin
  FFormCaption            := 'Grid Column Manager';
  FBtnOkCaption           := 'Ok';
  FBtnCancelCaption       := 'Cancel';
  FLabelFieldName         := 'FieldName:';
  FLabelTitleCaption      := 'Title:';
  FLabelFont              := 'Font:';
  FLabelWidth             := 'Width:';
  FLabelColor             := 'Color:';
  FLabelVisible           := 'Visible';
  FLabelReadOnly          := 'ReadOnly';
  FColumnFieldName        := 'FieldName';
  FColumnTitleCaption     := 'Title';
end;
{------------------------------------------------------------------------------}
destructor TgtColMgrCaptions.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtColMgrCaptions.Assign(Source: TPersistent);
begin
  if Source is TgtColMgrCaptions then
  begin
    FFormCaption            := TgtColMgrCaptions(Source).FormCaption;
    FBtnOkCaption           := TgtColMgrCaptions(Source).BtnOk;
    FBtnCancelCaption       := TgtColMgrCaptions(Source).BtnCancel;
    FLabelFieldName         := TgtColMgrCaptions(Source).LabelFieldName;
    FLabelTitleCaption      := TgtColMgrCaptions(Source).LabelTitleCaption;
    FLabelFont              := TgtColMgrCaptions(Source).LabelFont;
    FLabelWidth             := TgtColMgrCaptions(Source).LabelWidth;
    FLabelColor             := TgtColMgrCaptions(Source).LabelColor;
    FLabelVisible           := TgtColMgrCaptions(Source).LabelVisible;
    FLabelReadOnly          := TgtColMgrCaptions(Source).LabelReadOnly;
    FColumnFieldName        := TgtColMgrCaptions(Source).ColumnFieldName;
    FColumnTitleCaption     := TgtColMgrCaptions(Source).ColumnTitleCaption;
  end
  else
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}

{ TgtDBColumnManager }
{------------------------------------------------------------------------------}
constructor TgtDBColumnManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptions     := TgtColMgrCaptions.Create;
  FStoragePath  := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  FFileExt      := '.col';
end;
{------------------------------------------------------------------------------}
destructor TgtDBColumnManager.Destroy;
begin
  FCaptions.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = FDBGrid then
      DBGrid := nil;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.LoadColumns;
begin
  if Assigned(FDBGrid) and FileExists(FStoragePath + FDBGrid.Name + FFileExt) then
  begin
    if Assigned(GridDataSet) then
      if GridDataSet.Active then
        FDBGrid.Columns.LoadFromFile(FStoragePath + FDBGrid.Name + FFileExt);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.ManageColumns;
begin
  FForm := TgtFrmColumnManager.Create(Self);
  try
    ParseColumns;
    ParseFieldNames;
    ApplyCaptions;
    FForm.ShowModal;
  finally
    FreeAndNil(FForm);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.SaveColumns;
begin
  if Assigned(FDBGrid) then
  begin
    FDBGrid.Columns.SaveToFile(FStoragePath + FDBGrid.Name + FFileExt);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.ParseColumns;
var
  i       : Integer;
  LstItem : TListItem;
  Cln     : TColumn;
begin
  FForm.ColumnsListView.Items.BeginUpdate;
  try
    FForm.DBGridPreview.Columns.Clear;
    for i:= 0 to Pred(FDBGrid.Columns.Count) do
    begin
      Cln               := FForm.DBGridPreview.Columns.Add;
      Cln.FieldName     := FDBGrid.Columns[i].FieldName;
      Cln.Width         := FDBGrid.Columns[i].Width;
      Cln.Title.Caption := FDBGrid.Columns[i].Title.Caption;
      Cln.Color         := FDBGrid.Columns[i].Color;
      Cln.Font          := FDBGrid.Columns[i].Font;
      Cln.Visible       := FDBGrid.Columns[i].Visible;
      Cln.ReadOnly      := FDBGrid.Columns[i].ReadOnly;
    end;
    for i:=0 to Pred(FForm.DBGridPreview.Columns.Count) do
    begin
      LstItem         := FForm.ColumnsListView.Items.Add;
      LstItem.Caption := FForm.DBGridPreview.Columns[i].FieldName;
      LstItem.SubItems.Add(FForm.DBGridPreview.Columns[i].Title.Caption);
      LstItem.Data    := FForm.DBGridPreview.Columns[i];
    end;
  finally
    FForm.ColumnsListView.Items.EndUpdate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.ParseFieldNames;
var
  i : Integer;
begin
  if Assigned(GridDataSet) then
  begin
    FForm.cmbFieldNames.Items.Clear;
    for i:=0 to Pred(GridDataSet.FieldCount) do
    begin
      FForm.cmbFieldNames.Items.Add(GridDataSet.Fields[i].FieldName);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.ApplyCaptions;
begin
  FForm.Caption                            := FCaptions.FormCaption;
  FForm.btnOk.Caption                      := FCaptions.BtnOk;
  FForm.btnCancel.Caption                  := FCaptions.BtnCancel;
  FForm.lblFieldName.Caption               := FCaptions.LabelFieldName;
  FForm.lblTitleCaption.Caption            := FCaptions.LabelTitleCaption;
  FForm.lblWidth.Caption                   := FCaptions.LabelWidth;
  FForm.lblFont.Caption                    := FCaptions.LabelFont;
  FForm.lblColor.Caption                   := FCaptions.LabelColor;
  FForm.chkBoxVisible.Caption              := FCaptions.LabelVisible;
  FForm.chkReadOnly.Caption                := FCaptions.LabelReadOnly;
  FForm.ColumnsListView.Columns[0].Caption := FCaptions.ColumnFieldName;
  FForm.ColumnsListView.Columns[1].Caption := FCaptions.ColumnTitleCaption;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.InternalOnStatusChange(Sender: TObject);
begin
  if Assigned(FOldStatusChange) then
    FOldStatusChange(Sender);
  case GridDataSet.State of
    dsBrowse :
      begin
        if not Self.FColumnsLoaded then
          LoadColumns;
      end;
    dsInactive :
      begin
        Self.FColumnsLoaded := False;
      end;
  end;
end;
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.SetDBGrid(const Value: TDBGrid);
begin
  if Assigned(FDBgrid) then
    FDBGrid.RemoveFreeNotification(Self);

  FDBGrid := Value;

  if Assigned(FDBGrid) then
  begin
    if Length(Trim(FDBGrid.Name)) = 0 then
      raise Exception.Create(ERR_GRID_MUST_HAVE_A_NAME);
    FDBGrid.FreeNotification(Self);
    if Assigned(FDBGrid.DataSource) then
    begin
      FOldStatusChange := FDBGrid.DataSource.OnStateChange;
      FDBGrid.DataSource.OnStateChange := InternalOnStatusChange;
    end;
    LoadColumns;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.SetFileExt(const Value: string);
begin
  if not SameText(FFileExt,Value) then
  begin
    FFileExt := Value;
    if pos('.',FFileExt) = 0 then
      FFileExt := '.' + FFileExt;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.SetCaptions(const Value: TgtColMgrCaptions);
begin
  FCaptions.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtDBColumnManager.SetStoragePath(const Value: string);
begin
  if not SameText(FStoragePath,Value) then
    FStoragePath := IncludeTrailingPathDelimiter(Value);
end;
{------------------------------------------------------------------------------}
function TgtDBColumnManager.GetGridDataSet: TDataSet;
begin
  Result := nil;
  if Assigned(FDBGrid) then
    if Assigned(FDBGrid.DataSource) then
      Result := FDBGrid.DataSource.DataSet;
end;
{------------------------------------------------------------------------------}



{ TgtFrmColumnManager }
{------------------------------------------------------------------------------}
function TgtFrmColumnManager.GetColumnManager: TgtDBColumnManager;
begin
  Result := TgtDBColumnManager(Self.Owner);
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.LoadColumnDetails(AColumn: TColumn);
begin
  FCurrentColumn := AColumn;
  if Assigned(AColumn) then
  begin
    Self.cmbFieldNames.ItemIndex    := Self.cmbFieldNames.Items.IndexOf(AColumn.FieldName);
    Self.edtTitleCaption.Text       := AColumn.Title.Caption;
    Self.edtColumnWidth.Text        := IntToStr(AColumn.Width);
    Self.edtColumnFont.Text         := AColumn.Font.Name;
    Self.shpColumnColor.Brush.Color := AColumn.Color;
    Self.chkBoxVisible.Checked      := AColumn.Visible;
    Self.chkReadOnly.Checked        := AColumn.ReadOnly;
  end
  else
  begin
    //Default - Empty Values.
    Self.cmbFieldNames.ItemIndex    := -1;
    Self.edtTitleCaption.Text       := '';
    Self.edtColumnWidth.Text        := '';
    Self.edtColumnFont.Text         := '';
    Self.shpColumnColor.Brush.Color := 0;
    Self.chkBoxVisible.Checked      := False;
    Self.chkReadOnly.Checked        := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.ColumnsListViewClick(Sender: TObject);
begin
  if Assigned(TListView(Sender).Selected) then
    LoadColumnDetails(TColumn(TListView(Sender).Selected.Data));
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.ColumnsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Assigned(Item) then
    LoadColumnDetails(TColumn(Item.Data));
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.cmbFieldNamesSelect(Sender: TObject);
begin
  if Assigned(FCurrentColumn) then
    if TComboBox(Sender).ItemIndex >= 0 then
      FCurrentColumn.FieldName := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.edtTitleCaptionChange(Sender: TObject);
begin
  if Assigned(FCurrentColumn) then
    FCurrentColumn.Title.Caption := TEdit(Sender).Text;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.edtColumnWidthChange(Sender: TObject);
begin
  if Assigned(FCurrentColumn) then
    FCurrentColumn.Width := StrToInt(TEdit(Sender).Text);
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.btnSetFontClick(Sender: TObject);
var
  FntDlg : TFontDialog;
begin
  if Assigned(FCurrentColumn) then
  begin
    FntDlg := TFontDialog.Create(nil);
    try
      FntDlg.Font := FCurrentColumn.Font;
      if FntDlg.Execute then
      begin
        FCurrentColumn.Font     := FntDlg.Font;
        Self.edtColumnFont.Text := FCurrentColumn.Font.Name;
      end;
    finally
      FreeAndNil(FntDlg);
    end;
  end;
end;
procedure TgtFrmColumnManager.Button1Click(Sender: TObject);
begin

end;

{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.btnSetColorClick(Sender: TObject);
var
  ClrDlg : TColorDialog;
begin
  if Assigned(FCurrentColumn) then
  begin
    ClrDlg := TColorDialog.Create(nil);
    try
      ClrDlg.Color := FCurrentColumn.Color;
      if ClrDlg.Execute then
      begin
        FCurrentColumn.Color            := ClrDlg.Color;
        Self.shpColumnColor.Brush.Color := FCurrentColumn.Color;
      end;
    finally
      FreeAndNil(ClrDlg);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.chkBoxVisibleClick(Sender: TObject);
begin
  if Assigned(FCurrentColumn) then
    FCurrentColumn.Visible := TCheckBox(Sender).Checked;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.chkReadOnlyClick(Sender: TObject);
begin
  if Assigned(FCurrentColumn) then
    FCurrentColumn.ReadOnly := TCheckBox(Sender).Checked;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.btnOkClick(Sender: TObject);
begin
  Self.ColumnManager.DBGrid.Columns.Assign(Self.DBGridPreview.Columns);
  Self.ColumnManager.SaveColumns;
  Self.ModalResult := mrOk;
end;
{------------------------------------------------------------------------------}
procedure TgtFrmColumnManager.btnCancelClick(Sender: TObject);
begin
  Self.ColumnsListView.Selected := nil;
  Self.ModalResult := mrCancel;
end;
{------------------------------------------------------------------------------}








end.
