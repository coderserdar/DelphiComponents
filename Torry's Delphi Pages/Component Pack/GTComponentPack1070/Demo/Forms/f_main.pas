unit f_main;

interface

uses
   Windows
  ,Messages
  ,SysUtils
  ,Variants
  ,Classes
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,ComCtrls
  ,ImgList
  ,Buttons
  ,ExtCtrls

  {GT Component Pack Units}  
  ,o_BenchMark
  ,o_ClassFinder
  ,o_FileInfo
  ,o_FormEvents
  ,o_GTMessageBox
  ,o_GTStatusBar
  ,o_GTGroupBox
  ,o_GTDateControls
  ,o_GTIPEdit
  ,o_FileInfoListView
  ,o_GTButtons
  ,o_ProcessListView
  ,o_GTCombos
  ,o_GTFileDownload
  ,o_GTInfotraydialog
  ,o_RegionalSettings
  ,o_StringStore
  ,o_LinkLabel
  ,o_GTListBox
  ,o_GTMenuStyler
  ,Menus, DB, ADODB, Grids, DBGrids, o_GTDBGridExporter, o_GTDBControls, DBCtrls,
  Mask, o_GTYouTubeVideoPlayer, jpeg, o_GTSplashScreen, o_GTFormFader,
  o_MenuTreeView, o_GTDBColumnManager, o_GTDBLookUpEdit
  ;

type
  TFrmMain = class(TForm)
    MainPageControl            : TPageControl;
    TabSheet1                  : TTabSheet;
    BenchMarkMemo              : TMemo;
    edtBenchMarkFrom           : TEdit;
    Label1                     : TLabel;
    edtBenchMarkTo             : TEdit;
    Label2                     : TLabel;
    btnRunBenchMark            : TButton;
    BenchMarkProgressBar       : TProgressBar;
    TabSheet2                  : TTabSheet;
    RegisteredClassesLstBox    : TListBox;
    btnRefreshRegisterClasses  : TButton;
    cmbNewClasses              : TComboBox;
    Label3                     : TLabel;
    btnRegisterNewClass        : TButton;
    TabSheet3                  : TTabSheet;
    FileInfoListView           : TListView;
    TabSheet4                  : TTabSheet;
    chboxEnableFormEvents      : TCheckBox;
    TabSheet6                  : TTabSheet;
    TabSheet7                  : TTabSheet;
    TabSheet8                  : TTabSheet;
    TabSheet10                 : TTabSheet;
    Label5                     : TLabel;
    Edit2                      : TEdit;
    Label6                     : TLabel;
    btnSetIP                   : TButton;
    GroupBox1                  : TGroupBox;
    Label7                     : TLabel;
    GroupBox2                  : TGroupBox;
    Label8                     : TLabel;
    TabSheet9                  : TTabSheet;
    DateTimePicker1            : TDateTimePicker;
    btnSetDate                 : TButton;
    btnShowMessage             : TButton;
    edtMessageBoxCaption       : TLabeledEdit;
    edtMessageText             : TLabeledEdit;
    cmbMsgBoxIcon              : TComboBox;
    Label9                     : TLabel;
    Label10                    : TLabel;
    cmbMsgBoxType              : TComboBox;
    Label11                    : TLabel;
    cmbMsgBoxModalityLevel     : TComboBox;
    Label12                    : TLabel;
    cmbMsgBoxDefButton         : TComboBox;
    chkBoxMsgBoxShowHelpButton : TCheckBox;
    TabSheet11                 : TTabSheet;
    edtDialogButton            : TEdit;
    Label13                    : TLabel;
    cmbDialogButtonAction      : TComboBox;
    Label14                    : TLabel;
    TabSheet12                 : TTabSheet;
    Label15                    : TLabel;
    cmbControlPanelApplet      : TComboBox;
    TabSheet5                  : TTabSheet;
    Label4                     : TLabel;
    Label16                    : TLabel;
    Label17                    : TLabel;
    Label18                    : TLabel;
    Label19                    : TLabel;
    Label20                    : TLabel;
    TabSheet13                 : TTabSheet;
    Label21                    : TLabel;
    edtInternetFileDownload    : TEdit;
    btnStartDownload           : TButton;
    DownLoadProgressBar        : TProgressBar;
    btnCancelDownload          : TButton;
    Label22                    : TLabel;
    edtLocalFilePath           : TEdit;
    TabSheet14                 : TTabSheet;
    Label23                    : TLabel;
    Label24                    : TLabel;
    Label25                    : TLabel;
    Label26                    : TLabel;
    Label27                    : TLabel;
    Button2                    : TButton;
    RadioGroup1                : TRadioGroup;
    Edit3                      : TEdit;
    Memo1                      : TMemo;
    Edit4                      : TEdit;
    CheckBox2                  : TCheckBox;
    Edit5                      : TEdit;
    UpDown1                    : TUpDown;
    CheckBox3                  : TCheckBox;
    CheckBox4                  : TCheckBox;
    ImageList1: TImageList;
    Edit1                      : TEdit;
    Button1                    : TButton;
    CheckBox1                  : TCheckBox;
    Panel1                     : TPanel;
    btnClose                   : TButton;
    TabSheet15                 : TTabSheet;
    Panel2                     : TPanel;
    btnGetRegionalSettings     : TButton;
    RegionalSettingsMemo       : TMemo;
    TabSheet16                 : TTabSheet;
    Panel3                     : TPanel;
    GroupBox3                  : TGroupBox;
    GroupBox4                  : TGroupBox;
    GroupBox5                  : TGroupBox;
    StringStoreMemo1           : TMemo;
    StringStoreMemo2           : TMemo;
    StringStoreMemo3           : TMemo;
    GroupBox6                  : TGroupBox;
    Panel4                     : TPanel;
    Label28                    : TLabel;
    cmbStringStore             : TComboBox;
    MainStringStoreMemo        : TMemo;
    TabSheet17                 : TTabSheet;
    TabSheet18                 : TTabSheet;
    Panel5                     : TPanel;
    Memo2                      : TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    MenuItem1: TMenuItem;
    Undo1: TMenuItem;
    N3: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    Options1: TMenuItem;
    Active1: TMenuItem;
    N6: TMenuItem;
    ChangeBackground1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Undo2: TMenuItem;
    N4: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    N5: TMenuItem;
    SelectAll2: TMenuItem;
    MainMenuImageList: TImageList;
    TabSheet19: TTabSheet;
    RadioGroup2: TRadioGroup;
    btnApplyMenuStyle: TButton;
    Label29: TLabel;
    est1: TMenuItem;
    Est2: TMenuItem;
    TabSheet20: TTabSheet;
    Label30: TLabel;
    edtDBDemosPath: TEdit;
    btnConnectToDB: TButton;
    dbDemosConn: TADOConnection;
    GroupBox7: TGroupBox;
    OrdersDBGrid: TDBGrid;
    dsOrders: TDataSource;
    OrdersTable: TADOTable;
    gtDBGridExporter1: TgtDBGridExporter;
    DBGridExporterProgressBar: TProgressBar;
    Label31: TLabel;
    edtDBGridExporterFilePath: TEdit;
    btnSetGridExporterFileName: TButton;
    GroupBox8: TGroupBox;
    DBNavigator1: TDBNavigator;
    gtDBProgressBar1: TgtDBProgressBar;
    gtDBTrackBar1: TgtDBTrackBar;
    gtDBEdit1: TgtDBEdit;
    gtDBDateTimePicker1: TgtDBDateTimePicker;
    TabSheet21: TTabSheet;
    gtYouTubeVideoPlayer1: TgtYouTubeVideoPlayer;
    gtSplashScreen1: TgtSplashScreen;
    gtFormFader1: TgtFormFader;
    TabSheet22: TTabSheet;
    gtMenuTreeView1: TgtMenuTreeView;
    Button3: TButton;
    gtDBLookUpEdit1: TgtDBLookUpEdit;
    gtDBColumnManager1: TgtDBColumnManager;
    btnManageColumns: TButton;
    CustomersTable: TADOTable;
    dsCustomers: TDataSource;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    procedure InternalFormEventsMinimize(Sender: TObject);
    procedure InternalFormEventsRestore(Sender: TObject);

    procedure InternalMessageBoxAfterExecute(Sender: TObject;ExecutionResult: TgtMessageExecResult);
    procedure InternalMessageBoxHelpButtonClick(Sender: TObject);

    procedure InternalgtFileDownloadDownloadBegin(Sender: TObject);
    procedure InternalgtFileDownloadDownloadCancelled(Sender: TObject);
    procedure InternalgtFileDownloadDownloadComplete(Sender: TObject);
    procedure InternalgtFileDownloadDownloadError(Sender: TObject);
    procedure InternalgtFileDownloadDownloadProgress(Sender: TObject; const Max,CurrentPos: Integer; var Cancel: Boolean);

    procedure InternalgtStatusBarPanelComboSelect   (Sender: TObject;PanelIndex: Integer);
    procedure InternalgtStatusBarPanelButtonClick   (Sender: TObject;PanelIndex: Integer);
    procedure InternalgtStatusBarPanelCheckBoxClick (Sender: TObject;PanelIndex: Integer);
    procedure InternalgtStatusBarPanelDateTimeChange(Sender: TObject;PanelIndex: Integer);

    procedure InternalgtInfoTrayDialogButtonClick(Sender: TObject);

    procedure btnSetDateClick(Sender: TObject);
    procedure btnSetIPClick(Sender: TObject);

    procedure btnShowMessageClick(Sender: TObject);


    procedure InternalgtDialogButtonAfterExecute(Sender: TObject);
    procedure InternalgtDialogButtonMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure InternalgtControlPanelDialogMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function  GetInfoType(Index:Integer):TInfoType;


    procedure btnStartDownloadClick         (Sender: TObject);
    procedure btnCancelDownloadClick        (Sender: TObject);
    procedure Button2Click                  (Sender: TObject);
    procedure btnRunBenchMarkClick          (Sender: TObject);
    procedure btnCloseClick                 (Sender: TObject);
    procedure btnRefreshRegisterClassesClick(Sender: TObject);
    procedure btnRegisterNewClassClick      (Sender: TObject);
    procedure chboxEnableFormEventsClick    (Sender: TObject);
    procedure btnGetRegionalSettingsClick   (Sender: TObject);
    procedure cmbStringStoreSelect(Sender: TObject);
    procedure btnApplyMenuStyleClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnConnectToDBClick(Sender: TObject);
    procedure btnSetGridExporterFileNameClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnManageColumnsClick(Sender: TObject);

  private
    { Private declarations }
    FSQLServerCombo   : TgtSQLServerCombo;
    FNetResourceCombo : TgtNetResourceCombo;
    FPrinterCombo     : TgtPrinterCombo;
    FFontCombo        : TgtFontCombo;
    FileDownload      : TgtFileDownload;
    FBenchMark        : TgtBenchMark;
    FRegisteredClasses: TgtRegisteredClasses;
    FFileInfo         : TgtFileInfo;
    FFileInfoListView : TgtFileInfoListView;
    FFormEvents       : TgtFormEvents;
    FMessageBox       : TgtMessageBox;
    FInfoTrayDialog   : TgtInfoTrayDialog;
    FDateCtrlManager  : TgtDateCtrlManager;
    FYearCombo        : TgtYearCombo;
    FMonthCombo       : TgtMonthCombo;
    FDayCombo         : TgtDayCombo;
    FProcessListView  : TgtProcessListView;
    FStatusBar        : TgtStatusBar;
    FGroupBox         : TgtGroupBox;
    FIpEditControl    : TgtIPEdit;
    FDialogButton     : TgtDialogButton;
    FCplDialog        : TgtControlPanelDialog;
    FRegionalSettings : TgtRegionalSettings;
    FStringStore      : TgtStringStore;
    FLinkLabel        : TgtLinkLabel;
    FListBox          : TgtListBox;
    FMenuStyler       : TgtMenuStyler;
    procedure GetFileInfo(AFileName : string);
  protected
    procedure CreateCombos;
    procedure CreateFileDownload;
    procedure CreateBenchMark;
    procedure CreateDateCotnrols;
    procedure CreateStatusBar;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  end;

var
  FrmMain: TFrmMain;

implementation
uses
   XPMan
  ,f_About
  ;

{$R *.dfm}
{------------------------------------------------------------------------------}
constructor TFrmMain.Create(AOwner: TComponent);
var
  i  : Integer;
begin
  inherited;
  MainPageControl.ActivePageIndex := 0;

  FRegisteredClasses := TgtRegisteredClasses.Create(Self);
  btnRefreshRegisterClassesClick(nil);

  FFileInfo          := TgtFileInfo.Create(Self);
  FFileInfoListView  := TgtFileInfoListView.Create(Self);
  FFileInfoListView.Parent := TabSheet3;
  FFileInfoListView.Align  := alClient;
  GetFileInfo(Application.ExeName);


  FFormEvents            := TgtFormEvents.Create(Self);
  FFormEvents.OnMinimize := InternalFormEventsMinimize;
  FFormEvents.OnRestore  := InternalFormEventsRestore;


  FMessageBox                   := TgtMessageBox.Create(Self);
  FMessageBox.AfterExecute      := InternalMessageBoxAfterExecute;
  FMessageBox.OnHelpButtonClick := InternalMessageBoxHelpButtonClick;


  FInfoTrayDialog               := TgtInfoTrayDialog.Create(Self);


  FProcessListView                 := TgtProcessListView.Create(Self);
  FProcessListView.Parent          := TabSheet12;
  FProcessListView.RefreshInterval := 5000;
  FProcessListView.Align           := alClient;


  FGroupBox                        := TgtGroupBox.Create(Self);
  FGroupBox.Parent                 := TabSheet8;
  FGroupBox.Caption                := 'GroupBox which affects the enable status of the controls it containts';
  FGroupBox.Align                  := alTop;
  Edit1.Parent                     := FGroupBox;
  Button1.Parent                   := FGroupBox;
  CheckBox1.Parent                 := FGroupBox;


  FIpEditControl                   := TgtIPEdit.Create(Self);
  FIpEditControl.Parent            := TabSheet10;
  FIpEditControl.Left              := 16;
  FIpEditControl.Top               := 48;


  FDialogButton                    := TgtDialogButton.Create(Self);
  FDialogButton.Parent             := TabSheet11;
  FDialogButton.Left               := 475;
  FDialogButton.Top                := 80;
  FDialogButton.OnAfterExecute     := InternalgtDialogButtonAfterExecute;
  FDialogButton.OnMouseDown        := InternalgtDialogButtonMouseDown;


  FCplDialog                       := TgtControlPanelDialog.Create(Self);
  FCplDialog.Parent                := TabSheet11;
  FCplDialog.Left                  := 24;
  FCplDialog.Top                   := 152;
  FCplDialog.Width                 := 200;
  FCplDialog.Caption               := 'Control Panel Applet';
  FCplDialog.OnMouseDown           := InternalgtControlPanelDialogMouseDown;

  FRegionalSettings                := TgtRegionalSettings.Create(Self);


  FStringStore                     := TgtStringStore.Create(Self);
  FStringStore.AddNew(StringStoreMemo1.Name);
  FStringStore.AddNew(StringStoreMemo2.Name);
  FStringStore.AddNew(StringStoreMemo3.Name);

  FStringStore.ItemByName[StringStoreMemo1.Name].AddStrings(StringStoreMemo1.Lines);
  FStringStore.ItemByName[StringStoreMemo2.Name].AddStrings(StringStoreMemo2.Lines);
  FStringStore.ItemByName[StringStoreMemo3.Name].AddStrings(StringStoreMemo3.Lines);

  for i:= 0 to Pred(FStringStore.Count) do
  begin
    cmbStringStore.Items.Add(FStringStore.Items[i].Name)
  end;



  FLinkLabel           := TgtLinkLabel.Create(Self);
  FLinkLabel.Parent    := TabSheet17;
  FLinkLabel.LinkType  := ltURL;
  FLinkLabel.URL       := 'http://www.gtdelphicomponents.gr';
  FLinkLabel.Caption   := 'www.gtdelphicomponents.gr';
  FLinkLabel.Align     := alTop;
  FLinkLabel.Alignment := taCenter;




  FListBox                 := TgtListBox.Create(Self);
  FListBox.Parent          := TabSheet18;
  FListBox.Align           := alClient;
  FListBox.EditorColor     := clCream;
  FListBox.EditorFontColor := clBlack;
  FListBox.EditorEnabled   := True;
  FListBox.Items.AddStrings(FStringStore.ItemByName[StringStoreMemo1.Name]);

  CreateCombos;
  CreateFileDownload;
  CreateBenchMark;
  CreateDateCotnrols;
  CreateStatusBar;


  FMenuStyler := TgtMenuStyler.Create(Self);
  FMenuStyler.HookAllMenus := True;
  FMenuStyler.MenuStyle    := msDefault;

  gtDBGridExporter1.FileName := edtDBGridExporterFilePath.Text;

  gtDBColumnManager1.LoadColumns;

  gtMenuTreeView1.MapEvents := True;

end;
{------------------------------------------------------------------------------}
destructor TFrmMain.Destroy;
begin
  gtDBColumnManager1.SaveColumns;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnRunBenchMarkClick(Sender: TObject);
var
  i : Integer;
begin
  BenchMarkMemo.Lines.Clear;
  BenchMarkProgressBar.Position := 0;
  BenchMarkProgressBar.Max := StrToInt(edtBenchMarkTo.Text);
  i := StrToInt(edtBenchMarkFrom.Text);
  FBenchMark.BenchMarkInfo.ProcedureName := 'For Loop';
  FBenchMark.Start;
  for i := i  to Pred(BenchMarkProgressBar.Max) do
  begin
    BenchMarkProgressBar.Position := BenchMarkProgressBar.Position + 1;
    Application.ProcessMessages;
  end;
  FBenchMark.Stop;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnConnectToDBClick(Sender: TObject);
begin
  if dbDemosConn.Connected then
    dbDemosConn.Connected := False;
  dbDemosConn.ConnectionString :=
  Format('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Persist Security Info=False',[edtDBDemosPath.Text]);
  dbDemosConn.Connected := True;
  OrdersTable.Active    := True;
  CustomersTable.Active := True;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnRefreshRegisterClassesClick(Sender: TObject);
begin
  RegisteredClassesLstBox.Items.Clear;
  RegisteredClassesLstBox.Items.AddStrings(FRegisteredClasses.ClassList);
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnRegisterNewClassClick(Sender: TObject);
begin
  case cmbNewClasses.ItemIndex of
    0 : RegisterClass(TButton);
    1 : RegisterClass(TForm);
    2 : RegisterClass(TLabel);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.GetFileInfo(AFileName: string);
var
  LstItem : TListItem;
begin
  FFileInfo.FileName         := AFileName;
  FFileInfoListView.FileName := AFileName;

  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'CompanyName';
  LstItem.SubItems.Add(FFileInfo.CompanyName);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'File Description';
  LstItem.SubItems.Add(FFileInfo.FileDescription);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'FileName';
  LstItem.SubItems.Add(FFileInfo.FileName);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'File Version';
  LstItem.SubItems.Add(FFileInfo.FileVersion);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Internal Name';
  LstItem.SubItems.Add(FFileInfo.InternalName);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Legal Copyright';
  LstItem.SubItems.Add(FFileInfo.LegalCopyright);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Legal TradeMarks';
  LstItem.SubItems.Add(FFileInfo.LegalTrademarks);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Original Filename';
  LstItem.SubItems.Add(FFileInfo.OriginalFilename);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Product Name';
  LstItem.SubItems.Add(FFileInfo.ProductName);
  LstItem := FileInfoListView.Items.Add;
  LstItem.Caption := 'Product Version';
  LstItem.SubItems.Add(FFileInfo.ProductVersion);
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalFormEventsMinimize(Sender: TObject);
begin
  if chboxEnableFormEvents.Checked then
    ShowMessage('On Minimize');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalFormEventsRestore(Sender: TObject);
begin
  if chboxEnableFormEvents.Checked then
    ShowMessage('On Restore');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.chboxEnableFormEventsClick(Sender: TObject);
begin
  if chboxEnableFormEvents.Checked then
    FFormEvents.Form := Self
  else
    FFormEvents.Form := nil;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtStatusBarPanelComboSelect(Sender: TObject;PanelIndex: Integer);
begin
  case PanelIndex of
    2 :ShowMessage('Color Selected');
    3 :ShowMessage('Item Selected');
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtStatusBarPanelButtonClick(Sender: TObject;PanelIndex: Integer);
begin
  case PanelIndex of
    0:ShowMessage('Button Clicked');
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtStatusBarPanelCheckBoxClick(Sender: TObject;PanelIndex: Integer);
begin
  case PanelIndex of
    1 :
      begin
        if FStatusBar.GTPanels[PanelIndex].ExtraSettings.CheckBoxChecked then
          ShowMessage('Checked...')
        else
          ShowMessage('UnChecked...')
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtStatusBarPanelDateTimeChange(Sender: TObject;PanelIndex: Integer);
begin
  case PanelIndex of
    4:ShowMessage('Date Changed');
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnSetDateClick(Sender: TObject);
begin
  FDateCtrlManager.DateTime := DateTimePicker1.DateTime;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnSetGridExporterFileNameClick(Sender: TObject);
begin
  gtDBGridExporter1.FileName := edtDBGridExporterFilePath.Text;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnSetIPClick(Sender: TObject);
begin
  FIpEditControl.IP := Edit2.Text;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnShowMessageClick(Sender: TObject);
begin
  FMessageBox.MessageCaption       := edtMessageBoxCaption.Text;
  FMessageBox.MessageText          := edtMessageText.Text;
  FMessageBox.MessageBoxIcon       := TgtMessageBoxIcon(cmbMsgBoxIcon.ItemIndex);
  FMessageBox.MessageBoxType       := TgtMessageBoxType(cmbMsgBoxType.ItemIndex);
  FMessageBox.MessageModalityLevel := TgtMessageModalityLevel(cmbMsgBoxModalityLevel.ItemIndex);
  FMessageBox.MessageDefButton     := TgtMessageDefButton(cmbMsgBoxDefButton.ItemIndex);
  FMessageBox.ShowHelpButton       := chkBoxMsgBoxShowHelpButton.Checked;
  FMessageBox.Execute;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalMessageBoxAfterExecute(Sender: TObject;ExecutionResult: TgtMessageExecResult);
begin
  case ExecutionResult of
    merNone           :ShowMessage('None');
    merOk             :ShowMessage('Ok');
    merCancel         :ShowMessage('Cancel');
    merAbort          :ShowMessage('Abort');
    merRetry          :ShowMessage('Retry');
    merIgnore         :ShowMessage('Ignore');
    merYes            :ShowMessage('Yes');
    merNo             :ShowMessage('No');
    merClose          :ShowMessage('Close');
    merTryAgain       :ShowMessage('TryAgain');
    merContinue       :ShowMessage('Continue');
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalMessageBoxHelpButtonClick(Sender: TObject);
begin
  ShowMessage('Help Button Clicked');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtDialogButtonMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDialogButton.Action := TgtButtonAction(cmbDialogButtonAction.ItemIndex);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtDialogButtonAfterExecute(Sender: TObject);
begin
  case FDialogButton.Action of
    gbaNone                 : edtDialogButton.Text :='';
    gbaSelectFolder         : edtDialogButton.Text := FDialogButton.ActionResult.FolderResult;
    gbaSelectPrinter        : edtDialogButton.Text := FDialogButton.ActionResult.PrinterResult;
    gbaSelectComputer       : edtDialogButton.Text := FDialogButton.ActionResult.ComputerResult;
    gbaSelectFile           : edtDialogButton.Text := FDialogButton.ActionResult.FileResult;
    gdaSelectPicture        : edtDialogButton.Text := FDialogButton.ActionResult.PictureResult;
    gbaSelectColor          : edtDialogButton.Text := IntToStr(FDialogButton.ActionResult.ColorResult);
    gbaSelectFont           : edtDialogButton.Text := FDialogButton.ActionResult.FontResult.Name;
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtControlPanelDialogMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FCplDialog.ControlPanelApplet := TgtControlPanelApplet(cmbControlPanelApplet.ItemIndex);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtFileDownloadDownloadBegin(Sender: TObject);
begin
  DownloadProgressBar.Position := 0;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtFileDownloadDownloadCancelled(Sender: TObject);
begin
  ShowMessage('Download Cancelled');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtFileDownloadDownloadComplete(Sender: TObject);
begin
  ShowMessage('Download Complete');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtFileDownloadDownloadError(Sender: TObject);
begin
  ShowMessage('Download Error');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtFileDownloadDownloadProgress(Sender: TObject;const Max, CurrentPos: Integer; var Cancel: Boolean);
begin
  DownloadProgressBar.Max      := Max;
  DownloadProgressBar.Position := CurrentPos;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnStartDownloadClick(Sender: TObject);
begin
  FileDownLoad.DownloadFile(edtInternetFileDownload.Text,edtLocalFilePath.Text);
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.About1Click(Sender: TObject);
begin
  ShowAboutDialog;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnApplyMenuStyleClick(Sender: TObject);
begin
  case RadioGroup2.ItemIndex of
    0 : FMenuStyler.MenuStyle := msDefault;
    1 : FMenuStyler.MenuStyle := msOfficeBlue;
    2 : FMenuStyler.MenuStyle := msObsidian;
    3 : FMenuStyler.MenuStyle := msLuna;
    4 : FMenuStyler.MenuStyle := msSilver;
    5 : FMenuStyler.MenuStyle := msVista;
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnCancelDownloadClick(Sender: TObject);
begin
  FileDownLoad.CancelDownload;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.Button2Click(Sender: TObject);
begin
  FInfoTrayDialog.AutoClose     := CheckBox2.Checked;
  FInfoTrayDialog.ShowCountDown := CheckBox3.Checked;
  FInfoTrayDialog.ShowButton    := CheckBox4.Checked;
  FInfoTrayDialog.CloseAfter    := UpDown1.Position;
  FInfoTrayDialog.Caption       := Edit3.Text;
  FInfoTrayDialog.ButtonCaption := Edit4.Text;
  FInfoTrayDialog.MessageText   := Memo1.Lines;
  FInfoTrayDialog.InfoType      := GetInfoType(RadioGroup1.ItemIndex);
  FInfoTrayDialog.DisplayInfo;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.Button3Click(Sender: TObject);
begin
  gtMenuTreeView1.PopulateMenu(Self.MainMenu1);
end;
{------------------------------------------------------------------------------}
function TFrmMain.GetInfoType(Index: Integer): TInfoType;
begin
  Result := itInfo;
  case Index of
    0 : Result := itInfo;
    1 : Result := itHelp;
    2 : Result := itOk;
    3 : Result := itWarning;
    4 : Result := itError;
    5 : Result := itLock;
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.InternalgtInfoTrayDialogButtonClick(Sender: TObject);
begin
  ShowMessage('InfoTrayDialog Button Clicked');
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.CreateCombos;
begin
  FPrinterCombo          := TgtPrinterCombo.Create(Self);
  FPrinterCombo.Parent   := TabSheet5;
  FPrinterCombo.Left     := 120;
  FPrinterCombo.Top      := 13;
  FPrinterCombo.Width    := 209;

  FFontCombo             := TgtFontCombo.Create(Self);
  FFontCombo.Parent      := TabSheet5;
  FFontCombo.Left        := 120;
  FFontCombo.Top         := 41;
  FFontCombo.Width       := 209;

  FSQLServerCombo        := TgtSQLServerCombo.Create(Self);
  FSQLServerCombo.Parent := TabSheet5;
  FSQLServerCombo.Left   := 120;
  FSQLServerCombo.Top    := 73;
  FSQLServerCombo.Width  := 209;

  FNetResourceCombo        := TgtNetResourceCombo.Create(Self);
  FNetResourceCombo.Parent := TabSheet5;
  FNetResourceCombo.Left   := 120;
  FNetResourceCombo.Top    := 105;
  FNetResourceCombo.Width  := 209;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.CreateFileDownload;
begin
  FileDownLoad                     := TgtFileDownload.Create(Self);
  FileDownload.OnDownloadBegin     := InternalgtFileDownloadDownloadBegin;
  FileDownload.OnDownloadCancelled := InternalgtFileDownloadDownloadCancelled;
  FileDownload.OnDownloadComplete  := InternalgtFileDownloadDownloadComplete;
  FileDownload.OnDownloadError     := InternalgtFileDownloadDownloadError;
  FileDownload.OnDownloadProgress  := InternalgtFileDownloadDownloadProgress;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.CreateBenchMark;
begin
  FBenchMark := TgtBenchMark.Create(Self);
  FBenchMark.DisplayOutPut := True;
  FBenchMark.OutPutControl  := BenchMarkMemo;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.CreateDateCotnrols;
begin
  FDateCtrlManager          := TgtDateCtrlManager.Create(Self);
  FDayCombo                 := TgtDayCombo.Create(Self);
  FDayCombo.Parent          := TabSheet9;
  FDayCombo.Left            := 16;
  FDayCombo.Top             := 40;

  FDateCtrlManager.DayCombo := FDayCombo;

  FMonthCombo                 := TgtMonthCombo.Create(Self);
  FMonthCombo.Parent          := TabSheet9;
  FMonthCombo.Left            := 176;
  FMonthCombo.Top             := 40;

  FDateCtrlManager.MonthCombo := FMonthCombo;

  FYearCombo                 := TgtYearCombo.Create(Self);
  FYearCombo.Parent          := TabSheet9;
  FYearCombo.Left            := 328;
  FYearCombo.Top             := 40;

  FDateCtrlManager.YearCombo := FYearCombo;

  DateTimePicker1.DateTime  := Now;

  FDateCtrlManager.DateTime := Now;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.CreateStatusBar;
var
  GTPanel : TgtStatusPanel;
begin
  FStatusBar        := TgtStatusBar.Create(Self);
  FStatusBar.Parent := TabSheet7;

  //Button Panel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkButton;
  GTPanel.ExtraSettings.ButtonCaption   := 'GT Status Bar';
  GTPanel.Width                         := 100;

  //CheckBox Panel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkCheckBox;
  GTPanel.ExtraSettings.CheckBoxCaption := 'GT Status Bar';
  GTPanel.Width                         := 100;

  //ColorBox Panel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkColorBox;
  GTPanel.Width                         := 100;

  //ComboBox Panel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkComboBox;
  GTPanel.ExtraSettings.ComboItems.Add('Item1');
  GTPanel.ExtraSettings.ComboItems.Add('Item2');
  GTPanel.ExtraSettings.ComboItems.Add('Item3');
  GTPanel.ExtraSettings.ComboItems.Add('Item4');
  GTPanel.Width                         := 100;

  //DatePanel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkDateTime;
  GTPanel.Width                         := 100;
  GTPanel.ExtraSettings.DT_Date         := Now;

  //Image Panel
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkImage;
  ImageList1.GetBitmap(0,GTPanel.ExtraSettings.Image.Bitmap);
  GTPanel.ExtraSettings.ImageCenter     := True;
  GTPanel.Width                         := 100;

  //Custom Panel
  //In custom panel you can put any control you want into this panel.
  GTPanel                               := FStatusBar.GTPanels.Add;
  GTPanel.Kind                          := spkCustom;
  GTPanel.Width                         := 100;



  FStatusBar.OnPanelButtonClick   := InternalgtStatusBarPanelButtonClick;
  FStatusBar.OnPanelCheckBoxClick := InternalgtStatusBarPanelCheckBoxClick;
  FStatusBar.OnPanelComboSelect   := InternalgtStatusBarPanelComboSelect;
  FStatusBar.OnPanelDateTimeChange:= InternalgtStatusBarPanelDateTimeChange;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnGetRegionalSettingsClick(Sender: TObject);
begin
  RegionalSettingsMemo.Lines.Add('<======================= String Values =============================>');
  RegionalSettingsMemo.Lines.Add('LanguageName               '+#9+':'+#9+FRegionalSettings.SysLocalLanguageName);
  RegionalSettingsMemo.Lines.Add('CountryName                '+#9+':'+#9+FRegionalSettings.SysCountryName);
  RegionalSettingsMemo.Lines.Add('AnsiCodePage               '+#9+':'+#9+FRegionalSettings.SysAnsiCodePage);
  RegionalSettingsMemo.Lines.Add('OemCodePage                '+#9+':'+#9+FRegionalSettings.SysOemCodePage);
  RegionalSettingsMemo.Lines.Add('DateSeparator              '+#9+':'+#9+FRegionalSettings.SysDateSeparator);
  RegionalSettingsMemo.Lines.Add('TimeSeparator              '+#9+':'+#9+FRegionalSettings.SysTimeSeparator);
  RegionalSettingsMemo.Lines.Add('TimeFormat                 '+#9+':'+#9+FRegionalSettings.SysTimeFormat);
  RegionalSettingsMemo.Lines.Add('ShortDateFormat            '+#9+':'+#9+FRegionalSettings.SysShortDateFormat);
  RegionalSettingsMemo.Lines.Add('LongDateFormat             '+#9+':'+#9+FRegionalSettings.SysLongDateFormat);
  RegionalSettingsMemo.Lines.Add('DecimalSeparator           '+#9+':'+#9+FRegionalSettings.SysDecimalSeparator);
  RegionalSettingsMemo.Lines.Add('ThousandSeparator          '+#9+':'+#9+FRegionalSettings.SysThousandSeparator);
  RegionalSettingsMemo.Lines.Add('DecimalDigits              '+#9+':'+#9+FRegionalSettings.SysDecimalDigits);
  RegionalSettingsMemo.Lines.Add('DigitGrouping              '+#9+':'+#9+FRegionalSettings.SysDigitGrouping);
  RegionalSettingsMemo.Lines.Add('CurrencySymbol             '+#9+':'+#9+FRegionalSettings.SysCurrencySymbol);
  RegionalSettingsMemo.Lines.Add('CurrencyDecimalSeparator   '+#9+':'+#9+FRegionalSettings.SysCurrencyDecimalSeparator);
  RegionalSettingsMemo.Lines.Add('CurrencyThousandSeparator  '+#9+':'+#9+FRegionalSettings.SysCurrencyThousandSeparator);
  RegionalSettingsMemo.Lines.Add('CurrencyDecimalDigits      '+#9+':'+#9+FRegionalSettings.SysCurrencyDecimalDigits);
  RegionalSettingsMemo.Lines.Add('');
  RegionalSettingsMemo.Lines.Add('Day Names');
  RegionalSettingsMemo.Lines.AddStrings(FRegionalSettings.SysDayNames);
  RegionalSettingsMemo.Lines.Add('');
  RegionalSettingsMemo.Lines.Add('Month Names');
  RegionalSettingsMemo.Lines.AddStrings(FRegionalSettings.SysMonthNames);
  RegionalSettingsMemo.Lines.Add('<======================= Integer Values =============================>');


  RegionalSettingsMemo.Lines.Add('LanguageID   :'+IntToStr(FRegionalSettings.SysLanguageId));
  RegionalSettingsMemo.Lines.Add('Country Code :'+IntToStr(FRegionalSettings.SysCountryCode));
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.btnManageColumnsClick(Sender: TObject);
begin
  gtDBColumnManager1.ManageColumns;
end;
{------------------------------------------------------------------------------}
procedure TFrmMain.cmbStringStoreSelect(Sender: TObject);
begin
  if TComboBox(Sender).ItemIndex >=0 then
  begin
    MainStringStoreMemo.Lines.Clear;
    MainStringStoreMemo.Lines.AddStrings(FStringStore.Items[TComboBox(Sender).ItemIndex]);
  end;
end;
{------------------------------------------------------------------------------}

end.
