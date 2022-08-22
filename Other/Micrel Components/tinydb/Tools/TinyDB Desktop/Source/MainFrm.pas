unit MainFrm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ImgList, ToolWin, TinyDB, Db, ShellApi, BaseFrm;

const
  STDBVersion = '2.9';

type
  TOpenTableMode = (otGrid, otCard);

  TMainForm = class(TBaseForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    WindowMenu: TMenuItem;
    HelpMenu: TMenuItem;
    FileN1Item: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    ToolsMenu: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    StatusBar: TStatusBar;
    ActionList1: TActionList;
    FileNewDBAction: TAction;
    FileExitAction: TAction;
    FileOpenDBAction: TAction;
    WindowCascadeAction: TWindowCascade;
    WindowTileHorizontalAction: TWindowTileHorizontal;
    WindowArrangeAllAction: TWindowArrange;
    WindowMinimizeAllAction: TWindowMinimizeAll;
    HelpAboutAction: TAction;
    WindowTileVerticalAction: TWindowTileVertical;
    WindowTileItem2: TMenuItem;
    ImageList: TImageList;
    HelpN1: TMenuItem;
    HelpContentAction: TAction;
    HelpContentItem: TMenuItem;
    TinyTable: TTinyTable;
    TinyDatabase: TTinyDatabase;
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    NewToolButton: TToolButton;
    OpenToolButton: TToolButton;
    ToolButton3: TToolButton;
    CascToolButton: TToolButton;
    HTileToolButton: TToolButton;
    VTileToolButton: TToolButton;
    TopPanel: TPanel;
    ToolbarPanel: TPanel;
    LeftDockPanel: TPanel;
    FileCloseDBAction: TAction;
    ToolCompactItem: TMenuItem;
    ToolRepairItem: TMenuItem;
    ToolN1: TMenuItem;
    ToolChgPwdItem: TMenuItem;
    ToolCompactAction: TAction;
    ToolRepairAction: TAction;
    ToolChgPwdAction: TAction;
    ViewToolBarAction: TAction;
    ViewStatusBarAction: TAction;
    ViewMenu: TMenuItem;
    ViewToolBarItem: TMenuItem;
    ViewStatusBarItem: TMenuItem;
    ViewRefreshAction: TAction;
    ViewN1: TMenuItem;
    ViewRefreshItem: TMenuItem;
    ToolOptionAction: TAction;
    ToolN3: TMenuItem;
    O1: TMenuItem;
    ToolButton1: TToolButton;
    HelpToolButton: TToolButton;
    HelpAdviseAction: TAction;
    HelpBugReportAction: TAction;
    HelpHomepageAction: TAction;
    HelpN2: TMenuItem;
    HelpFeedbackItem: TMenuItem;
    HelpHomepageItem: TMenuItem;
    HelpAdviseItem: TMenuItem;
    HelpBugReportItem: TMenuItem;
    DatabaseMenu: TMenuItem;
    DBNewTableAction: TAction;
    DBDeleteTableAction: TAction;
    DBRenameTableAction: TAction;
    DBEmptyTableAction: TAction;
    DBNewTableItem: TMenuItem;
    DBDeleteTableItem: TMenuItem;
    DBEmptyTableItem: TMenuItem;
    DBRenameTableItem: TMenuItem;
    DBN1: TMenuItem;
    DBDeleteIndexAction: TAction;
    DBDeleteIndexItem: TMenuItem;
    DBN2: TMenuItem;
    DBRenameFieldAction: TAction;
    DBCommentsAction: TAction;
    DBPropertyAction: TAction;
    DBRenameFieldItem: TMenuItem;
    DBN3: TMenuItem;
    DBCommentsItem: TMenuItem;
    DBPropertyItem: TMenuItem;
    DBOpenTableAction: TAction;
    O2: TMenuItem;
    ViewOpenTableGridAction: TAction;
    ViewOpenTableCardAction: TAction;
    ViewN2: TMenuItem;
    ViewOpenTableModeItem: TMenuItem;
    ViewOpenTableGridItem: TMenuItem;
    ViewOpenTableCardItem: TMenuItem;
    NewTableToolButton: TToolButton;
    ToolButton4: TToolButton;
    RefreshToolButton: TToolButton;
    CompactToolButton: TToolButton;
    RepairToolButton: TToolButton;
    ChgPwdToolButton: TToolButton;
    ToolButton12: TToolButton;
    FileN2Item: TMenuItem;
    FileMRU1Item: TMenuItem;
    FileMRU2Item: TMenuItem;
    FileMRU3Item: TMenuItem;
    FileMRU4Item: TMenuItem;
    ToolBar1: TToolBar;
    TableComboBox: TComboBox;
    DBRenameIndexAction: TAction;
    DBRenameIndexItem: TMenuItem;
    OpenAsGridToolButton: TToolButton;
    OpenAsCardToolButton: TToolButton;
    DBSearchAction: TAction;
    DBN4: TMenuItem;
    DBSearchItem: TMenuItem;
    DBFilterAction: TAction;
    DBFilterItem: TMenuItem;
    ProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    OptionsToolButton: TToolButton;
    ToolButton10: TToolButton;
    ToolChgEncAction: TAction;
    ToolChgEncItem: TMenuItem;
    ViewN3Item: TMenuItem;
    ViewLanguageItem: TMenuItem;
    DBDesignTableAction: TAction;
    DBDesignTableItem: TMenuItem;
    FileExportTextAction: TAction;
    FileN3Item: TMenuItem;
    FileExportItem: TMenuItem;
    FileExportTextItem: TMenuItem;
    ToolButton2: TToolButton;
    SplitterPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNewDBActionExecute(Sender: TObject);
    procedure FileOpenDBActionExecute(Sender: TObject);
    procedure FileCloseDBActionExecute(Sender: TObject);
    procedure FileExitActionExecute(Sender: TObject);
    procedure HelpContentActionExecute(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure LeftDockPanelDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure LeftDockPanelDockOver(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure LeftDockPanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure LeftDockPanelGetSiteInfo(Sender: TObject;
      DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean);
    procedure ToolCompactActionExecute(Sender: TObject);
    procedure ToolRepairActionExecute(Sender: TObject);
    procedure ToolChgPwdActionExecute(Sender: TObject);
    procedure ViewToolBarActionExecute(Sender: TObject);
    procedure ViewStatusBarActionExecute(Sender: TObject);
    procedure ViewRefreshActionExecute(Sender: TObject);
    procedure ToolOptionActionExecute(Sender: TObject);
    procedure HelpAdviseActionExecute(Sender: TObject);
    procedure HelpBugReportActionExecute(Sender: TObject);
    procedure HelpHomepageActionExecute(Sender: TObject);
    procedure DBNewTableActionExecute(Sender: TObject);
    procedure DBDeleteTableActionExecute(Sender: TObject);
    procedure DBRenameTableActionExecute(Sender: TObject);
    procedure DBEmptyTableActionExecute(Sender: TObject);
    procedure DBDeleteIndexActionExecute(Sender: TObject);
    procedure DBRenameFieldActionExecute(Sender: TObject);
    procedure DBCommentsActionExecute(Sender: TObject);
    procedure DBPropertyActionExecute(Sender: TObject);
    procedure DBOpenTableActionExecute(Sender: TObject);
    procedure ViewOpenTableGridActionExecute(Sender: TObject);
    procedure ViewOpenTableCardActionExecute(Sender: TObject);
    procedure FileMRU1ItemClick(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure DBRenameIndexActionExecute(Sender: TObject);
    procedure DatabaseMenuClick(Sender: TObject);
    procedure TableComboBoxClick(Sender: TObject);
    procedure TableComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBSearchActionExecute(Sender: TObject);
    procedure DBFilterActionExecute(Sender: TObject);
    procedure TinyDatabaseOperationProgress(Sender: TObject; Percent: Integer);
    procedure FormActivate(Sender: TObject);
    procedure ToolChgEncActionExecute(Sender: TObject);
    procedure DBDesignTableActionExecute(Sender: TObject);
    procedure FileExportTextActionExecute(Sender: TObject);
    procedure SplitterPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitterPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitterPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
    FFieldList: TStrings;
    FOpenTableMode: TOpenTableMode;
    FPassword: string;
    FSptMouseDown: Boolean;
    FSptMouseX: Integer;

    procedure InitFieldList;
    procedure InitMRUMenuItem;
    procedure InitLanguageMenu;
    procedure AdjustCaption(DatabaseName: string);
    procedure SetOpenTableMode(Mode: TOpenTableMode);
    procedure DoParam;
    procedure DoFileDropMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure CheckLanguageMenu(Index: Integer);

    procedure LanguageItemClick(Sender: TObject);
    procedure ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure ShowDockPanel(APanel: TPanel; MakeVisible: Boolean; Client: TControl);
    function GetFieldStrByType(FieldType: TFieldType): string;
    function GetFieldCommentsByType(FieldType: TFieldType): string;
    procedure AdjustUI(Opened: Boolean);
    procedure InitTableComboxBox;
    procedure OpenDatabase(DatabaseName: string);
    procedure BeginProgress;
    procedure EndProgress;
    procedure DoProgress(Percent: Integer);

    property FieldList: TStrings read FFieldList;
    property OpenTableMode: TOpenTableMode read FOpenTableMode write SetOpenTableMode;
    property Password: string read FPassword write FPassword;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  Misc, MRUMgr, DBInfoFrm, AboutFrm, OptionFrm, NewDBFrm,
  InputFrm, TableListFrm, DBCommentsFrm, DBPropertyFrm,
  ChgPwdFrm, ChgEncFrm, SearchFrm, FilterFrm, DBTableFrm,
  NewTableFrm, LangMgr;

procedure TMainForm.TransLanguage;
begin
  inherited;
  InitFieldList;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FFieldList := TStringList.Create;
  MRUManager := TMRUMgr.Create(GetIniFileName);
  InitMRUMenuItem;
  LoadOptions;
  BoundsRect := Options.FormBounds;
  AdjustUI(False);
  SetOpenTableMode(Options.OpenTableMode);
  DragAcceptFiles(Handle, True);
  Application.OnMessage := ApplicationMessage;

  //Init language
  AppLangMgr.InitPath(ExtractFilePath(Application.ExeName));
  InitLanguageMenu;
  if Options.LanguageName = '' then
    AppLangMgr.InitLang(AppLangMgr.DefaultLangIndex)
  else
    AppLangMgr.InitLang(AppLangMgr.IndexOfLangName(Options.LanguageName));
  CheckLanguageMenu(AppLangMgr.CurrentLangIndex);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  DoParam;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FFieldList.Free;
  MRUManager.Free;
  if Options.RememberSize then
  begin
    if WindowState = wsMaximized then
      Options.FormBounds := GetWorkAreaRect
    else
      Options.FormBounds := BoundsRect;
  end;
  SaveOptions;
end;

procedure TMainForm.InitFieldList;
begin
  FFieldList.Clear;
  FFieldList.AddObject('AutoInc=' + AppLangMgr.Trans('ftmAutoInc'), TObject(ftAutoInc));
  FFieldList.AddObject('Smallint=' + AppLangMgr.Trans('ftmSmallint'), TObject(ftSmallint));
  FFieldList.AddObject('Integer=' + AppLangMgr.Trans('ftmInteger'), TObject(ftInteger));
  FFieldList.AddObject('Word=' + AppLangMgr.Trans('ftmWord'), TObject(ftWord));
  FFieldList.AddObject('Largeint=' + AppLangMgr.Trans('ftmLargeint'), TObject(ftLargeint));
  FFieldList.AddObject('Boolean=' + AppLangMgr.Trans('ftmBoolean'), TObject(ftBoolean));
  FFieldList.AddObject('Float=' + AppLangMgr.Trans('ftmFloat'), TObject(ftFloat));
  FFieldList.AddObject('Currency=' + AppLangMgr.Trans('ftmCurrency'), TObject(ftCurrency));
  FFieldList.AddObject('Date=' + AppLangMgr.Trans('ftmDate'), TObject(ftDate));
  FFieldList.AddObject('Time=' + AppLangMgr.Trans('ftmTime'), TObject(ftTime));
  FFieldList.AddObject('DateTime=' + AppLangMgr.Trans('ftmDateTime'), TObject(ftDateTime));
  FFieldList.AddObject('String=' + AppLangMgr.Trans('ftmString'), TObject(ftString));
  FFieldList.AddObject('WideString=' + AppLangMgr.Trans('ftmWideString'), TObject(ftWideString));
  FFieldList.AddObject('FixedChar=' + AppLangMgr.Trans('ftmFixedChar'), TObject(ftFixedChar));
  FFieldList.AddObject('Memo=' + AppLangMgr.Trans('ftmMemo'), TObject(ftMemo));
  FFieldList.AddObject('Graphic=' + AppLangMgr.Trans('ftmGraphic'), TObject(ftGraphic));
  FFieldList.AddObject('Blob=' + AppLangMgr.Trans('ftmBlob'), TObject(ftBlob));
end;

procedure TMainForm.InitMRUMenuItem;
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  for I := 1 to MRUManager.Items.Count do
  begin
    MenuItem := FindComponent('FileMRU' + IntToStr(I) + 'Item') as TMenuItem;
    MenuItem.Caption := '&' + IntToStr(MenuItem.Tag) + ' ' + MRUManager.GetDisplayString(I - 1);
    MenuItem.Visible := True;
  end;
  FileN2Item.Visible := MRUManager.Items.Count > 0;
end;

procedure TMainForm.InitTableComboxBox;
var
  I: Integer;
begin
  TableComboBox.Items.Clear;
  for I := 0 to TinyDatabase.TableDefs.Count - 1 do
    TableComboBox.Items.Add(TinyDatabase.TableDefs[I].Name);
  TableComboBox.ItemIndex := -1;
end;

procedure TMainForm.InitLanguageMenu;
var
  LangName: string;
  I: Integer;
  MenuItem: TMenuItem;
begin
  for I := 0 to AppLangMgr.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(Self);
    ViewLanguageItem.Add(MenuItem);
    LangName := AppLangMgr.Items[I].LangName;
    MenuItem.Caption := LangName;
    MenuItem.Hint := LangName;
    MenuItem.GroupIndex := 1;
    MenuItem.RadioItem := True;
    MenuItem.Tag := I;
    MenuItem.OnClick := LanguageItemClick;
  end;
  ViewLanguageItem.Enabled := AppLangMgr.Count > 0;
end;

procedure TMainForm.AdjustCaption(DatabaseName: string);
var
  S: string;
begin
  if AppLangMgr.GetFormString(string(Self.ClassName) + '.Caption', S) then
    Caption := S
  else
    Caption := 'TinyDB Desktop';
  if DatabaseName <> '' then Caption := Caption + ' (' + DatabaseName + ') ';
end;

procedure TMainForm.SetOpenTableMode(Mode: TOpenTableMode);
begin
  FOpenTableMode := Mode;
  if Mode = otGrid then
  begin
    ViewOpenTableGridAction.Checked := True;
    ViewOpenTableCardAction.Checked := False;
  end
  else
  begin
    ViewOpenTableGridAction.Checked := False;
    ViewOpenTableCardAction.Checked := True;
  end;
end;

procedure TMainForm.DoParam;
begin
  if ParamCount >= 1 then
    OpenDatabase(ParamStr(1));
end;

procedure TMainForm.DoFileDropMessage(var Msg: tagMSG; var Handled: Boolean);
const
  BufferLength = MAX_PATH;
var
  DroppedFilename: string;
  FileIndex: UINT;
  QtyDroppedFiles: Word;
  FileNameBuffer: array[0..BufferLength] of Char;
begin
  case Msg.Message of
    WM_DROPFILES:
      begin
        FileIndex := $FFFFFFFF;
        QtyDroppedFiles := DragQueryFile(Msg.WParam, FileIndex,
          FileNameBuffer, BufferLength);
        for FileIndex := 0 to (QtyDroppedFiles - 1) do
        begin
          DragQueryFile(Msg.WParam, FileIndex, FileNameBuffer, BufferLength);
          DroppedFilename := StrPas(FileNameBuffer);
          try
            OpenDatabase(DroppedFilename);
          except
            on E: Exception do Application.ShowException(E);
          end;
          Break; //only open one database file
        end;
        DragFinish(Msg.WParam);
        Handled := True;
      end;
  end;
end;

procedure TMainForm.CheckLanguageMenu(Index: Integer);
begin
  if (Index >= 0) and (Index < ViewLanguageItem.Count) then
    ViewLanguageItem.Items[Index].Checked := True;
end;

procedure TMainForm.LanguageItemClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  Index: Integer;
  I: Integer;
begin
  MenuItem := Sender as TMenuItem;
  Index := MenuItem.Tag;
  AppLangMgr.InitLang(Index);
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TBaseForm then
      (Screen.Forms[I] as TBaseForm).TransLanguage;
  end;
  CheckLanguageMenu(AppLangMgr.CurrentLangIndex);
  Options.LanguageName := AppLangMgr.Items[AppLangMgr.CurrentLangIndex].LangName;
  SaveOptions;
end;

procedure TMainForm.ShowDockPanel(APanel: TPanel; MakeVisible: Boolean; Client: TControl);
begin
  if not MakeVisible and (APanel.VisibleDockClientCount > 1) then
    Exit;

  if APanel = LeftDockPanel then
    SplitterPanel.Visible := MakeVisible;

  if MakeVisible then
  begin
    if APanel = LeftDockPanel then
    begin
      APanel.Width := ClientWidth div 4;
      SplitterPanel.Left := APanel.Width + SplitterPanel.Width;
    end
  end
  else
  begin
    if APanel = LeftDockPanel then
      APanel.Width := 0;
  end;
  if MakeVisible and (Client <> nil) then
  begin
    Client.Show;
  end;
end;

function TMainForm.GetFieldStrByType(FieldType: TFieldType): string;
var
  I: Integer;
begin
  I := FFieldList.IndexOfObject(TObject(FieldType));
  if I = -1 then
    Result := ''
  else
    Result := FFieldList.Names[I];
end;

function TMainForm.GetFieldCommentsByType(FieldType: TFieldType): string;
var
  I: Integer;
begin
  I := FFieldList.IndexOfObject(TObject(FieldType));
  if I = -1 then
    Result := ''
  else
    Result := FFieldList.Values[FFieldList.Names[I]];
end;

procedure TMainForm.AdjustUI(Opened: Boolean);
begin
  if Opened then
    AdjustCaption(TinyDatabase.FileName)
  else
    AdjustCaption('');
  FileCloseDBAction.Enabled := Opened;
  FileExportItem.Enabled := Opened;
  ViewRefreshAction.Enabled := Opened;
  ToolCompactAction.Enabled := Opened;
  ToolRepairAction.Enabled := Opened;
  ToolChgPwdAction.Enabled := Opened;
  ToolChgEncAction.Enabled := Opened;
  DatabaseMenu.Visible := Opened;
  DBOpenTableAction.Enabled := Opened;
  DBNewTableAction.Enabled := Opened;
  DBDeleteTableAction.Enabled := Opened;
  DBRenameTableAction.Enabled := Opened;
  DBEmptyTableAction.Enabled := Opened;
  DBDeleteIndexAction.Enabled := Opened;
  DBRenameFieldAction.Enabled := Opened;
  DBCommentsAction.Enabled := Opened;
  DBPropertyAction.Enabled := Opened;
  ViewOpenTableModeItem.Enabled := Opened;
  ViewOpenTableGridAction.Enabled := Opened;
  ViewOpenTableCardAction.Enabled := Opened;
  TableComboBox.Enabled := Opened;
  TableComboBox.Color := Iif(Opened, clWindow, $00DCDCDC);
  if not Opened then TableComboBox.Text := '';
end;

procedure TMainForm.OpenDatabase(DatabaseName: string);
var
  Count: Integer;
  Password: string;
begin
  TinyDatabase.Close;
  TinyDatabase.FileName := DatabaseName;
  TinyDatabase.Open;
  Count := 1;
  while not TinyDatabase.CanAccess and (Count <= 3) do
  begin
    if ShowInputForm(Password, AppLangMgr.Trans('Database password'), True) then
    begin
      FPassword := Password;
      TinyDatabase.Password := FPassword;
      if not TinyDatabase.CanAccess then
      begin
        Application.MessageBox(PChar(AppLangMgr.Trans('Password is incorrect. you can not open the database.')), PChar(Application.Title), 48);
        Inc(Count);
      end;
    end
    else
    begin
      TinyDatabase.Close;
      TinyDatabase.FileName := '';
      Break;
    end;
  end;
  if (TinyDatabase.FileName <> '') and (TinyDatabase.CanAccess) then
  begin
    ShowDBInfoForm(TinyDatabase);
    AdjustUI(True);
    MRUManager.AddMRU(TinyDatabase.FileName);
    InitMRUMenuItem;
    InitTableComboxBox;
  end;
end;

procedure TMainForm.BeginProgress;
begin
  ProgressPanel.Visible := True;
  ProgressBar.Position := 0;
end;

procedure TMainForm.EndProgress;
begin
  ProgressPanel.Visible := False;
end;

procedure TMainForm.DoProgress(Percent: Integer);
begin
  ProgressBar.Position := Percent;
end;

procedure TMainForm.FileNewDBActionExecute(Sender: TObject);
var
  Value: TNewDBFormData;
begin
  if ShowNewDBForm(Value) then
  begin
    TinyDatabase.Close;
    if TinyDatabase.CreateDatabase(Value.FileName, Value.Compress, TCompressLevel(Value.CompLevel), Value.CompAlgo, Value.Encrypt, Value.EncAlgo, Value.Password, True) then
    begin
      TinyDatabase.FileName := Value.FileName;
      TinyDatabase.Password := Value.Password;
      TinyDatabase.Open;
      FPassword := Value.Password;
      AdjustUI(True);
      ShowDBInfoForm(TinyDatabase);
      DBInfoForm.InvokeNewTableForm;
      MRUManager.AddMRU(TinyDatabase.FileName);
      InitMRUMenuItem;
      InitTableComboxBox;
    end
    else
    begin
      TinyDatabase.FileName := '';
      Application.MessageBox(PChar(AppLangMgr.Trans('Fail to create database.')), PChar(Application.Title), 16);
    end;
  end;
end;

procedure TMainForm.FileOpenDBActionExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenDatabase(OpenDialog.FileName);
end;

procedure TMainForm.FileCloseDBActionExecute(Sender: TObject);
begin
  if DBInfoForm <> nil then
  begin
    DBInfoForm.Close;
    AdjustUI(False);
  end;
end;

procedure TMainForm.FileExportTextActionExecute(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
    SaveDialog.Filter := 'Text Files(*.txt)|*.txt|All Files(*.*)|*.*';
    SaveDialog.DefaultExt := '.txt';
    if SaveDialog.Execute then
    begin
      DBInfoForm.ExportDBToText(SaveDialog.FileName);
      Application.MessageBox(PChar(AppLangMgr.Trans('Exporting finished.')), PChar(Application.Title), 48);
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TMainForm.FileExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HelpContentActionExecute(Sender: TObject);
begin
  ShowHelp;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TMainForm.HelpAdviseActionExecute(Sender: TObject);
var
  S: string;
begin
  S := 'About TinyDB ' + STDBVersion;
  ShellExecute(Handle, 'Open', PChar('mailto:haoxg@21cn.com?subject="' + S + '"'), '', '', 1);
end;

procedure TMainForm.HelpBugReportActionExecute(Sender: TObject);
var
  S: string;
begin
  S := 'TinyDB ' + STDBVersion + ' Bug Report';
  ShellExecute(Handle, 'Open', PChar('mailto:haoxg@21cn.com?subject="' + S + '"'), '', '', 1);
end;

procedure TMainForm.HelpHomepageActionExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'Open', 'http://www.TinyDatabase.com', '', '', 1);
end;

procedure TMainForm.LeftDockPanelDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(Sender as TPanel, True, nil);
  (Sender as TPanel).DockManager.ResetBounds(True);
end;

procedure TMainForm.LeftDockPanelDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := Source.Control is TDBInfoForm;
  if Accept then
  begin
    ARect.TopLeft := LeftDockPanel.ClientToScreen(Point(0, 0));
    ARect.BottomRight := LeftDockPanel.ClientToScreen(
      Point(Self.ClientWidth div 4, LeftDockPanel.Height));
    Source.DockRect := ARect;
  end;
end;

procedure TMainForm.LeftDockPanelUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(Sender as TPanel, False, nil);
end;

procedure TMainForm.LeftDockPanelGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  CanDock := DockClient is TDBInfoForm;
end;

procedure TMainForm.ToolCompactActionExecute(Sender: TObject);
var
  R: Integer;
  Ok: Boolean;
begin
  R := Application.MessageBox(PChar(AppLangMgr.Trans('Pack the database?')), PChar(Application.Title), 36);
  if R = ID_NO then Exit;

  BeginProgress;
  Ok := TinyDatabase.Compact;
  EndProgress;
  if Ok then
    Application.MessageBox(PChar(AppLangMgr.Trans('Packing finished.')), PChar(Application.Title), 48)
  else
    Application.MessageBox(PChar(AppLangMgr.Trans('Fail to pack database.')), PChar(Application.Title), 16);
end;

procedure TMainForm.ToolRepairActionExecute(Sender: TObject);
var
  R: Integer;
  Ok: Boolean;
begin
  R := Application.MessageBox(PChar(AppLangMgr.Trans('Repair the database?')), PChar(Application.Title), 36);
  if R = ID_NO then Exit;

  BeginProgress;
  Ok := TinyDatabase.Repair;
  EndProgress;
  if Ok then
    Application.MessageBox(PChar(AppLangMgr.Trans('Repairing finished.')), PChar(Application.Title), 48)
  else
    Application.MessageBox(PChar(AppLangMgr.Trans('Fail to repair database.')), PChar(Application.Title), 16);
end;

procedure TMainForm.ToolChgPwdActionExecute(Sender: TObject);
var
  Value: TChgPwdFormData;
  Ok: Boolean;
begin
  Value.CheckPwd := TinyDatabase.Encrypted;
  if ShowChgPwdForm(Value) then
  begin
    BeginProgress;
    Ok := TinyDatabase.ChangePassword(Value.Password, Value.CheckPwd);
    EndProgress;
    if Ok then
      Application.MessageBox(PChar(AppLangMgr.Trans('Change password successfully.')), PChar(Application.Title), 48)
    else
      Application.MessageBox(PChar(AppLangMgr.Trans('Fail to change password.')), PChar(Application.Title), 16);
  end;
end;

procedure TMainForm.ToolChgEncActionExecute(Sender: TObject);
var
  Value: TChgEncFormData;
  Ok: Boolean;
begin
  Value.Encrypt := TinyDatabase.Encrypted;
  Value.EncAlg := TinyDatabase.EncryptAlgoName;
  Value.Password := FPassword;
  if ShowChgEncForm(Value) then
  begin
    BeginProgress;
    Ok := TinyDatabase.ChangeEncrypt(Value.Encrypt, Value.EncAlg, Value.Password);
    EndProgress;
    if Ok then
      Application.MessageBox(PChar(AppLangMgr.Trans('Change encrypt algorithm successfully.')), PChar(Application.Title), 48)
    else
      Application.MessageBox(PChar(AppLangMgr.Trans('Fail to change encrypt algorithm.')), PChar(Application.Title), 16);
  end;
end;

procedure TMainForm.ToolOptionActionExecute(Sender: TObject);
begin
  if ShowOptionForm then
  begin
    SetOpenTableMode(Options.OpenTableMode);
  end;
end;

procedure TMainForm.ViewToolBarActionExecute(Sender: TObject);
begin
  ViewToolBarAction.Checked := not ViewToolBarAction.Checked;
  CoolBar.Visible := ViewToolBarAction.Checked;
end;

procedure TMainForm.ViewStatusBarActionExecute(Sender: TObject);
begin
  ViewStatusBarAction.Checked := not ViewStatusBarAction.Checked;
  StatusBar.Visible := ViewStatusBarAction.Checked;
end;

procedure TMainForm.ViewRefreshActionExecute(Sender: TObject);
begin
  DBInfoForm.RefreshDBInfo;
end;

procedure TMainForm.DBNewTableActionExecute(Sender: TObject);
begin
  DBInfoForm.InvokeNewTableForm;
  InitTableComboxBox;
end;

procedure TMainForm.DBDeleteTableActionExecute(Sender: TObject);
begin
  DBInfoForm.TableMenuDeleteItemClick(nil);
  InitTableComboxBox;
end;

procedure TMainForm.DBRenameTableActionExecute(Sender: TObject);
begin
  DBInfoForm.TableMenuRenameItemClick(nil);
end;

procedure TMainForm.DBEmptyTableActionExecute(Sender: TObject);
begin
  DBInfoForm.TableMenuEmptyItemClick(nil);
end;

procedure TMainForm.DBDesignTableActionExecute(Sender: TObject);
var
  Value: TTableListFormData;
begin
  Value.TinyDatabase := TinyDatabase;
  Value.TableName := '';
  if ShowTableListForm(Value) then
  begin
    DBInfoForm.DesignTable(Value.TableName);
    InitTableComboxBox;
  end;
end;

procedure TMainForm.DBDeleteIndexActionExecute(Sender: TObject);
begin
  DBInfoForm.IndexMenuDeleteItemClick(nil);
end;

procedure TMainForm.DBRenameFieldActionExecute(Sender: TObject);
begin
  DBInfoForm.FieldMenuRenameItemClick(nil);
end;

procedure TMainForm.DBRenameIndexActionExecute(Sender: TObject);
begin
  DBInfoForm.IndexMenuRenameItemClick(nil);
end;

procedure TMainForm.DBCommentsActionExecute(Sender: TObject);
begin
  ShowDBCommentsForm(TinyDatabase);
end;

procedure TMainForm.DBPropertyActionExecute(Sender: TObject);
begin
  ShowDBPropertyForm(TinyDatabase);
end;

procedure TMainForm.DBOpenTableActionExecute(Sender: TObject);
var
  Value: TTableListFormData;
begin
  Value.TinyDatabase := TinyDatabase;
  Value.TableName := '';
  if ShowTableListForm(Value) then
  begin
    DBInfoForm.OpenTable(Value.TableName);
  end;
end;

procedure TMainForm.DBSearchActionExecute(Sender: TObject);
var
  Value: TSearchFormData;
begin
  Value.TinyTable := (ActiveMDIChild as TDBTableForm).TinyTable;
  ShowSearchForm(Value);
end;

procedure TMainForm.DBFilterActionExecute(Sender: TObject);
var
  Value: TFilterFormData;
begin
  Value.TinyTable := (ActiveMDIChild as TDBTableForm).TinyTable;
  ShowFilterForm(Value);
end;

procedure TMainForm.ViewOpenTableGridActionExecute(Sender: TObject);
begin
  SetOpenTableMode(otGrid);
end;

procedure TMainForm.ViewOpenTableCardActionExecute(Sender: TObject);
begin
  SetOpenTableMode(otCard);
end;

procedure TMainForm.FileMRU1ItemClick(Sender: TObject);
var
  DatabaseName: string;
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  DatabaseName := MRUManager.Items[MenuItem.Tag - 1];
  OpenDatabase(DatabaseName);
end;

procedure TMainForm.StatusBarResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := StatusBar.Width - StatusBar.Panels[1].Width;
  ProgressPanel.Top := StatusBar.Top + 2;
  ProgressPanel.Left := 0;
  ProgressPanel.Width := StatusBar.Panels[0].Width;
end;

procedure TMainForm.DatabaseMenuClick(Sender: TObject);
var
  Node: TTreeNode;
  NodeData: TNodeData;
begin
  DBDeleteTableAction.Enabled := False;
  DBRenameTableAction.Enabled := False;
  DBEmptyTableAction.Enabled := False;
  DBRenameFieldAction.Enabled := False;
  DBDeleteIndexAction.Enabled := False;
  DBRenameIndexAction.Enabled := False;
  DBSearchAction.Enabled := False;
  DBFilterAction.Enabled := False;

  Node := DBInfoForm.DBStructTreeView.Selected;
  if Node = nil then Exit;
  if Node.Data = nil then Exit;

  NodeData := PNodeData(Node.Data)^;
  if NodeData.NodeType = ntTable then
  begin
    DBDeleteTableAction.Enabled := True;
    DBRenameTableAction.Enabled := True;
    DBEmptyTableAction.Enabled := True;
  end
  else if NodeData.NodeType = ntField then
  begin
    DBRenameFieldAction.Enabled := True;
  end
  else if NodeData.NodeType = ntIndex then
  begin
    DBDeleteIndexAction.Enabled := True;
    DBRenameIndexAction.Enabled := True;
  end;
  if ActiveMDIChild is TDBTableForm then
  begin
    DBSearchAction.Enabled := True;
    DBFilterAction.Enabled := True;
  end;
end;

procedure TMainForm.TableComboBoxClick(Sender: TObject);
var
  TableName: string;
begin
  if TableComboBox.Text = '' then
    if TableComboBox.Items.Count > 0 then
      TableComboBox.ItemIndex := 0;
  TableName := TableComboBox.Text;
  if TinyDatabase.TableDefs.IndexOf(TableName) <> -1 then
    if DBInfoForm <> nil then
      DBInfoForm.OpenTable(TableName);
end;

procedure TMainForm.TableComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TableName: string;
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    TableName := TableComboBox.Text;
    if TinyDatabase.TableDefs.IndexOf(TableName) = -1 then
    begin
      Application.MessageBox(PChar(AppLangMgr.Trans('Table name [%s] does not exist.', [TableName])), PChar(Application.Title), 48);
      TableComboBox.SelectAll;
      TableComboBox.SetFocus;
    end
    else
    begin
      DBInfoForm.OpenTable(TableName);
    end;
  end;
end;

procedure TMainForm.TinyDatabaseOperationProgress(Sender: TObject;
  Percent: Integer);
begin
  DoProgress(Percent);
end;

procedure TMainForm.ApplicationMessage(var Msg: TMsg; var Handled: Boolean);
begin
  case Msg.Message of
    WM_DROPFILES:
      DoFileDropMessage(Msg, Handled);
  end;
end;

procedure TMainForm.SplitterPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSptMouseDown := True;
  FSptMouseX := X;
end;

procedure TMainForm.SplitterPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSptMouseDown := False;
end;

procedure TMainForm.SplitterPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  W: Integer;
begin
  // because of the bugs of the TSplitter in MDI form under D6.
  if FSptMouseDown then
  begin
    W := SplitterPanel.Left + X - FSptMouseX;
    if W < 0 then W := 0;
    if W > ClientWidth - 10 then W := ClientWidth - 10;
    LeftDockPanel.Width := W;
  end;
end;

end.
