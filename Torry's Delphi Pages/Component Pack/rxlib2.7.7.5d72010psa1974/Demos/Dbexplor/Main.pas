{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{                                                       }
{*******************************************************}

unit Main;

interface

uses WinTypes, WinProcs, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, Gauges, DbPrgrss, Placemnt,
  DB, DBTables, DBCtrls, SpeedBar, RXDBCtrl, DBSecur, AppEvent, MRUList,
  RXCtrls, RXSplit;

type
  TDBExplorerMainForm = class(TForm)
    SQLFontContainer: TLabel;
    FormPlacement: TFormStorage;
    Panel1: TPanel;
    StatusLine: TPanel;
    Panel2: TPanel;
    DBGauge: TGauge;
    Panel4: TPanel;
    DBStatusLabel: TDBStatusLabel;
    RecNoPanel: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    FileCloseItem: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    UtilitiesMenu: TMenuItem;
    PackTableItem: TMenuItem;
    DeleteTableItem: TMenuItem;
    EmptyTableItem: TMenuItem;
    ReindexItem: TMenuItem;
    RenameTableItem: TMenuItem;
    ExportTableItem: TMenuItem;
    CheckPXSubMenu: TMenuItem;
    CheckPXItem: TMenuItem;
    CheckPXAllItem: TMenuItem;
    Options1: TMenuItem;
    AutoActivateItem: TMenuItem;
    SystemTablesItem: TMenuItem;
    KeepConnectionsItem: TMenuItem;
    N3: TMenuItem;
    OptionsItem: TMenuItem;
    CustomizeSpeedbar: TMenuItem;
    WindowMenu: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    SpeedBar: TSpeedBar;
    Panel3: TPanel;
    DBNavigator: TDBNavigator;
    SpeedItem1: TSpeedItem;
    CloseButton: TSpeedItem;
    SpeedItem3: TSpeedItem;
    PackBtn: TSpeedItem;
    DeleteBtn: TSpeedItem;
    EmptyBtn: TSpeedItem;
    RenameBtn: TSpeedItem;
    ExportBtn: TSpeedItem;
    RepairBtn: TSpeedItem;
    SpeedItem4: TSpeedItem;
    SpeedItem2: TSpeedItem;
    KeepConnectionsSpd: TSpeedItem;
    OptionsBtn: TSpeedItem;
    SpeedItem5: TSpeedItem;
    SpeedItem6: TSpeedItem;
    DBRecordNo: TDBStatusLabel;
    WindowTileVerticalItem: TMenuItem;
    N2: TMenuItem;
    ImportDataItem: TMenuItem;
    ImportBtn: TSpeedItem;
    StartTransItem: TMenuItem;
    CommitItem: TMenuItem;
    RollbackItem: TMenuItem;
    DBProgress1: TDBProgress;
    N4: TMenuItem;
    BdePropsItem: TMenuItem;
    AppEvents: TAppEvents;
    Flatspeedbarbuttons1: TMenuItem;
    ClosedDatabases: TMRUManager;
    ReopenMenu: TMenuItem;
    HelpList: TMRUManager;
    UserHelpItem: TMenuItem;
    ToolSplitter: TRxSplitter;
    procedure FormCreate(Sender: TObject);
    procedure WindowCascadeItemClick(Sender: TObject);
    procedure UpdateMenuItems(Sender: TObject);
    procedure WindowTileItemClick(Sender: TObject);
    procedure WindowArrangeItemClick(Sender: TObject);
    procedure FileCloseItemClick(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure WindowMinimizeItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure FormPlacementSavePlacement(Sender: TObject);
    procedure FormPlacementRestorePlacement(Sender: TObject);
    procedure CustomizeToolbarItemClick(Sender: TObject);
    procedure AutoActivateItemClick(Sender: TObject);
    procedure SystemTablesItemClick(Sender: TObject);
    function DBStatusLabelGetDataName(Sender: TObject): string;
    procedure PackTableClick(Sender: TObject);
    procedure DeleteTableClick(Sender: TObject);
    procedure EmptyTableClick(Sender: TObject);
    procedure RenameTableClick(Sender: TObject);
    procedure CheckPXAllClick(Sender: TObject);
    procedure CheckPXItemClick(Sender: TObject);
    procedure ReindexItemClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure KeepConnectionsItemClick(Sender: TObject);
    procedure ImportClick(Sender: TObject);
    procedure StartTransItemClick(Sender: TObject);
    procedure CommitItemClick(Sender: TObject);
    procedure RollbackItemClick(Sender: TObject);
    procedure BdePropsItemClick(Sender: TObject);
    procedure AppActivate(Sender: TObject);
    procedure SpeedBarApplyAlign(Sender: TObject; Align: TAlign;
      var Apply: Boolean);
    procedure Flatspeedbarbuttons1Click(Sender: TObject);
    procedure ClosedDatabasesClick(Sender: TObject; const RecentName,
      Caption: String; UserData: Longint);
    procedure ShowHint(Sender: TObject);
    procedure UserHelpItemClick(Sender: TObject);
    procedure HelpListClick(Sender: TObject; const RecentName,
      Caption: String; UserData: Longint);
    procedure DBRecordNoDblClick(Sender: TObject);
  private
    { Private declarations }
    procedure CreateMDIChild(const AName: string);
    procedure DatabaseLogin(Database: TDatabase; LoginParams: TStrings);
  protected
    procedure CreateWnd; override;
  public
    { Public declarations }
    procedure ApplyOptions;
    procedure UpdateMenus;
  end;

var
  DBExplorerMainForm: TDBExplorerMainForm;

implementation

{$R *.DFM}

uses {$IFDEF WIN32} DBInpReq, {$ENDIF} VCLUtils, ChildWin, OpenDlg, AppUtils,
  IniFiles, RxShell, LoginDlg, DbUtils, BdeUtils, About, Options, OptDlg,
  BdeProp, UserHelp;

const
  SEmptyWarning = 'Table %s will be emptied. All data will be lost. Continue?';
  SDeleteWarning = 'Table %s will be deleted. All data will be lost. Continue?';

{ TMainForm }

procedure TDBExplorerMainForm.CreateWnd;
begin
  inherited CreateWnd;
{$IFDEF WIN32}
  ShowMDIClientEdge(ClientHandle, True);
{$ENDIF}
end;

procedure TDBExplorerMainForm.ApplyOptions;
var
  I: Integer;
begin
  SpeedItem4.Down := AutoActivate;
  AutoActivateItem.Checked := AutoActivate;
  SpeedItem2.Down := SystemTables;
  SystemTablesItem.Checked := SystemTables;
  FlatSpeedbarButtons1.Checked := sbFlatBtns in Speedbar.Options;
  KeepConnectionsSpd.Down := Session.KeepConnections;
  KeepConnectionsItem.Checked := Session.KeepConnections;
  DBRecordNo.CalcRecCount := SQLCalcCount;
  for I := MDIChildCount - 1 downto 0 do begin
    if AutoActivate then
      TMDIChild(MDIChildren[I]).SetToCurrentTable;
    TMDIChild(MDIChildren[I]).UpdateSystemTables;
    TMDIChild(MDIChildren[I]).UpdateDataFieldFormats;
    TMDIChild(MDIChildren[I]).SQLMemo.Font := SQLFontContainer.Font;
  end;
end;

procedure TDBExplorerMainForm.FormCreate(Sender: TObject);
begin
  Screen.OnActiveFormChange := UpdateMenuItems;
  Caption := Application.Title + ' ' + SDbxVersion;
{$IFDEF VER100}
  {DBNavigator.Flat := True;}
{$ENDIF}
end;

procedure TDBExplorerMainForm.ShowHint(Sender: TObject);
begin
  StatusLine.Caption := Application.Hint;
end;

procedure TDBExplorerMainForm.CreateMDIChild(const AName: string);
{$IFDEF WIN32}
const
  SQuerySession = '_Query_';
var
  SName: string;
  I: Integer;
{$ENDIF}
var
  TempDatabase, QueryDatabase: TDatabase;
  ChildForm: TMDIChild;
begin
{$IFDEF WIN32}
  Session.Open;
  Sessions.CurrentSession := Session;
{$ENDIF}
  TempDatabase := Session.FindDatabase(AName);
  if TempDatabase = nil then begin
    TempDatabase := TDatabase.Create(Session);
    TempDatabase.DatabaseName := AName;
    TempDatabase.OnLogin := DatabaseLogin;
    TempDatabase.Temporary := True;
    TempDatabase.KeepConnection := Session.KeepConnections;
{$IFDEF WIN32}
    TempDatabase.SessionName := Session.SessionName;
{$ENDIF}
  end;
{$IFDEF WIN32}
  TempDatabase.Session.OpenDatabase(TempDatabase.DatabaseName);
{$ELSE}
  Session.OpenDatabase(TempDatabase.DatabaseName);
{$ENDIF}
  ChildForm := TMDIChild.Create(Application);
  with ChildForm do begin
    SQLMemo.Font := SQLFontContainer.Font;
    DatabaseName := AName;
{$IFDEF WIN32}
    I := 0;
    repeat
      SName := SQuerySession + IntToStr(I);
      Inc(I);
    until Sessions.FindSession(SName) = nil;
    with TSession.Create(ChildForm) do begin
      SessionName := SName;
      Active := True;
    end;
    QueryDatabase := TDatabase.Create(ChildForm);
    with QueryDatabase do
    try
      SessionName := SName;
      DatabaseName := AName;
      Params.Assign(TempDatabase.Params);
      LoginPrompt := False;
      Connected := True;
    except
      Free;
      raise;
    end;
    QueryDB := QueryDatabase;
    Query1.SessionName := SName;
    Query1.DatabaseName := QueryDB.DatabaseName;
{$ENDIF}
  end;
{$IFDEF WIN32}
  Sessions.CurrentSession := Session;
{$ENDIF}
  ClosedDatabases.Remove(AName);
  UpdateMenus;
end;

procedure TDBExplorerMainForm.FileOpenItemClick(Sender: TObject);
var
  DBName: string;
begin
  if GetOpenDatabase(DBName) then begin
    Screen.OnActiveFormChange := nil;
    try
      CreateMDIChild(DBName);
    finally
      Screen.OnActiveFormChange := UpdateMenuItems;
    end;
  end;
end;

procedure TDBExplorerMainForm.FileCloseItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then ActiveMDIChild.Close;
end;

procedure TDBExplorerMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TDBExplorerMainForm.WindowCascadeItemClick(Sender: TObject);
begin
  Cascade;
end;

procedure TDBExplorerMainForm.WindowTileItemClick(Sender: TObject);
begin
  if Sender = WindowTileItem then { TileHorizontal } TileMode := tbHorizontal
  else if Sender = WindowTileVerticalItem then TileMode := tbVertical;
  Tile;
end;

procedure TDBExplorerMainForm.WindowArrangeItemClick(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TDBExplorerMainForm.WindowMinimizeItemClick(Sender: TObject);
var
  I: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  for I := MDIChildCount - 1 downto 0 do
    MDIChildren[I].WindowState := wsMinimized;
end;

procedure TDBExplorerMainForm.UpdateMenuItems(Sender: TObject);
var
  TabEnable: Boolean;
begin
  TabEnable := (MDIChildCount > 0) and (ActiveMDIChild <> nil);
  ImportDataItem.Enabled := TabEnable;
  ImportBtn.Enabled := TabEnable;
  ExportTableItem.Enabled := TabEnable;
  ExportBtn.Enabled := TabEnable;
  ReindexItem.Enabled := TabEnable;
  PackTableItem.Enabled := TabEnable;
  PackBtn.Enabled := TabEnable;
  DeleteTableItem.Enabled := TabEnable;
  DeleteBtn.Enabled := TabEnable;
  EmptyTableItem.Enabled := TabEnable;
  EmptyBtn.Enabled := TabEnable;
  RenameTableItem.Enabled := TabEnable;
  RenameBtn.Enabled := TabEnable;
  DBNavigator.Enabled := TabEnable;
  if TabEnable then begin
    DBNavigator.DataSource := (ActiveMDIChild as TMDIChild).DataSource;
    DBStatusLabel.DataSource := (ActiveMDIChild as TMDIChild).DataSource;
    DBRecordNo.DataSource := (ActiveMDIChild as TMDIChild).DataSource;
  end
  else begin
    DBNavigator.DataSource := nil;
    DBStatusLabel.DataSource := nil;
    DBRecordNo.DataSource := nil;
  end;
  { Check and repair commands }
  CheckPXItem.Enabled := TabEnable {and (ActiveMDIChild as TMDIChild).CheckStandard};
  RepairBtn.Enabled := TabEnable {and (ActiveMDIChild as TMDIChild).CheckStandard};
  CheckPXAllItem.Enabled := TabEnable;
  { Window commands }
  FileCloseItem.Enabled := MDIChildCount > 0;
  CloseButton.Enabled := MDIChildCount > 0;
  StartTransItem.Enabled := TabEnable and
    (ActiveMDIChild as TMDIChild).TransOperEnabled(teStart);
  CommitItem.Enabled := TabEnable and
    (ActiveMDIChild as TMDIChild).TransOperEnabled(teCommit);
  RollbackItem.Enabled := TabEnable and
    (ActiveMDIChild as TMDIChild).TransOperEnabled(teRollback);
  WindowCascadeItem.Enabled := MDIChildCount > 0;
  WindowTileItem.Enabled := MDIChildCount > 0;
  WindowTileVerticalItem.Enabled := MDIChildCount > 0;
  WindowArrangeItem.Enabled := MDIChildCount > 0;
  WindowMinimizeItem.Enabled := MDIChildCount > 0;
end;

procedure TDBExplorerMainForm.UpdateMenus;
begin
  UpdateMenuItems(nil);
end;

procedure TDBExplorerMainForm.FormDestroy(Sender: TObject);
begin
  Screen.OnActiveFormChange := nil;
end;

procedure TDBExplorerMainForm.HelpAboutItemClick(Sender: TObject);
begin
  ShowAbout;
end;

procedure TDBExplorerMainForm.FormPlacementSavePlacement(Sender: TObject);
begin
  SaveOptions(FormPlacement.IniFile);
end;

procedure TDBExplorerMainForm.FormPlacementRestorePlacement(Sender: TObject);
begin
  LoadOptions(FormPlacement.IniFile);
  ApplyOptions;
{$IFNDEF WIN32}
  AppTaskbarIcons(True);
{$ENDIF}
end;

procedure TDBExplorerMainForm.CustomizeToolbarItemClick(Sender: TObject);
begin
  Speedbar.Customize(0);
end;

procedure TDBExplorerMainForm.AutoActivateItemClick(Sender: TObject);
begin
  AutoActivate := not AutoActivate;
  ApplyOptions;
end;

procedure TDBExplorerMainForm.SystemTablesItemClick(Sender: TObject);
begin
  SystemTables := not SystemTables;
  ApplyOptions;
end;

function TDBExplorerMainForm.DBStatusLabelGetDataName(
  Sender: TObject): string;
begin
  Result := '';
end;

procedure TDBExplorerMainForm.DBRecordNoDblClick(Sender: TObject);
begin
  SQLCalcCount := not SQLCalcCount;
  DBRecordNo.CalcRecCount := SQLCalcCount;
end;

procedure TDBExplorerMainForm.PackTableClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).PackCurrentTable;
end;

procedure TDBExplorerMainForm.DeleteTableClick(Sender: TObject);
var
  Tab: TTable;
begin
  if ActiveMDIChild <> nil then begin
    Tab := (ActiveMDIChild as TMDIChild).CurrentTable;
    if Tab <> nil then begin
      if MessageDlg(Format(SDeleteWarning, [Tab.TableName]), mtWarning,
        [mbYes, mbNo], 0) = mrYes then
      begin
        Tab.DisableControls;
        try
          if Tab.Active then Tab.Close;
          Tab.DeleteTable;
          (ActiveMDIChild as TMDIChild).RefreshData;
        finally
          Tab.EnableControls;
        end;
      end;
    end;
  end;
end;

procedure TDBExplorerMainForm.EmptyTableClick(Sender: TObject);
var
  Tab: TTable;
begin
  if ActiveMDIChild <> nil then begin
    Tab := (ActiveMDIChild as TMDIChild).CurrentTable;
    if Tab <> nil then begin
      if MessageDlg(Format(SEmptyWarning, [Tab.TableName]), mtWarning,
        [mbYes, mbNo], 0) = mrYes then
      begin
        Tab.DisableControls;
        try
          if Tab.Active then Tab.Close;
          Tab.EmptyTable;
          Tab.Open;
        finally
          Tab.EnableControls;
        end;
      end;
    end;
  end;
end;

procedure TDBExplorerMainForm.RenameTableClick(Sender: TObject);
begin
  NotImplemented;
end;

procedure TDBExplorerMainForm.CheckPXAllClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then begin
    (ActiveMDIChild as TMDIChild).CheckAndRepairParadoxTable(True);
  end;
end;

procedure TDBExplorerMainForm.CheckPXItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then begin
    (ActiveMDIChild as TMDIChild).CheckAndRepairParadoxTable(False);
  end;
end;

procedure TDBExplorerMainForm.DatabaseLogin(Database: TDatabase;
  LoginParams: TStrings);
var
  DBase: TDatabase;
begin
  DBase := Session.FindDatabase(Database.DatabaseName);
  if (DBase <> nil) and DBase.Connected
{$IFDEF WIN32}
    and (Database.Session <> Session)
{$ENDIF}
  then LoginParams.Assign(DBase.Params)
  else OnLoginDialog(Database, LoginParams, 3, True);
  Database.Params.Values['USER NAME'] := LoginParams.Values['USER NAME'];
  Database.Params.Values['PASSWORD'] := LoginParams.Values['PASSWORD'];
end;

procedure TDBExplorerMainForm.ExportClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).ExportCurrentTable;
end;

procedure TDBExplorerMainForm.ImportClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).ImportToCurrentTable;
end;

procedure TDBExplorerMainForm.OptionsClick(Sender: TObject);
begin
  ShowDialog(TOptionsDialog);
end;

procedure TDBExplorerMainForm.ReindexItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then begin
    (ActiveMDIChild as TMDIChild).ReindexTable;
  end;
end;

procedure TDBExplorerMainForm.KeepConnectionsItemClick(Sender: TObject);
begin
  SetKeepConnections(not Session.KeepConnections);
  KeepConnectionsItem.Checked := Session.KeepConnections;
  KeepConnectionsSpd.Down := Session.KeepConnections;
end;

procedure TDBExplorerMainForm.StartTransItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).StartTransaction;
end;

procedure TDBExplorerMainForm.CommitItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).Commit;
end;

procedure TDBExplorerMainForm.RollbackItemClick(Sender: TObject);
begin
  if ActiveMDIChild <> nil then
    (ActiveMDIChild as TMDIChild).Rollback;
end;

procedure TDBExplorerMainForm.BdePropsItemClick(Sender: TObject);
begin
  ShowDialog(TBdePropertyDlg);
end;

procedure TDBExplorerMainForm.AppActivate(Sender: TObject);
begin
  if Screen.ActiveForm <> nil then Screen.ActiveForm.BringToFront;
end;

procedure TDBExplorerMainForm.SpeedBarApplyAlign(Sender: TObject;
  Align: TAlign; var Apply: Boolean);
begin
  Apply := Align in [alTop, alBottom];
end;

procedure TDBExplorerMainForm.Flatspeedbarbuttons1Click(Sender: TObject);
begin
  if sbFlatBtns in Speedbar.Options then
    Speedbar.Options := Speedbar.Options - [sbFlatBtns]
  else
    Speedbar.Options := Speedbar.Options + [sbFlatBtns];
  Flatspeedbarbuttons1.Checked := sbFlatBtns in Speedbar.Options;
end;

procedure TDBExplorerMainForm.ClosedDatabasesClick(Sender: TObject;
  const RecentName, Caption: String; UserData: Longint);
begin
  Screen.OnActiveFormChange := nil;
  try
    CreateMDIChild(RecentName);
  finally
    Screen.OnActiveFormChange := UpdateMenuItems;
  end;
end;

procedure TDBExplorerMainForm.UserHelpItemClick(Sender: TObject);
begin
  CustomizeHelp(HelpList.Strings);
end;

procedure TDBExplorerMainForm.HelpListClick(Sender: TObject;
  const RecentName, Caption: String; UserData: Longint);
begin
  if GetLongHint(RecentName) <> '' then begin
    FileExecute(GetLongHint(RecentName), '', '', esNormal);
  end;
end;

end.
