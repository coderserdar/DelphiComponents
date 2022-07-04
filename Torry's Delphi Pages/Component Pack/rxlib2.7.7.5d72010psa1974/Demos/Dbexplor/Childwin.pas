unit ChildWin;

{$I RX.INC}

interface

uses WinTypes, WinProcs, Messages, Classes, Graphics, Forms, Controls, DB,
  DBLists, Tabs, ExtCtrls, RXSplit, DBTables, Grids, DBGrids, RXDBCtrl,
  RXQuery, StdCtrls, Buttons, Placemnt, DBIndex, DBSecur, Menus, Dialogs,
  RXShell, PicClip;

type
  TTransOperation = (teStart, teCommit, teRollback);

  TMDIChild = class(TForm)
    TableList: TDatabaseItems;
    DataSource1: TDataSource;
    TablesGrid: TrxDBGrid;
    rxSplitter1: TrxSplitter;
    Panel1: TPanel;
    Notebook1: TNotebook;
    TabSet1: TTabSet;
    FieldList1: TTableItems;
    IndexList1: TTableItems;
    DataSource2: TDataSource;
    Table1: TTable;
    rxDBGrid2: TrxDBGrid;
    Panel2: TPanel;
    SQLMemo: TMemo;
    Panel3: TPanel;
    RunSQL: TSpeedButton;
    Panel4: TPanel;
    Label1: TLabel;
    Panel5: TPanel;
    rxDBGrid3: TrxDBGrid;
    Query1: TrxQuery;
    TableListTABNAME: TStringField;
    TableListEXTENSION: TStringField;
    TableListTYPE: TStringField;
    FieldList1TYPE: TWordField;
    FieldList1SUBTYPE: TWordField;
    FieldList1UNITS1: TWordField;
    FieldList1UNITS2: TWordField;
    FieldList1LENGTH: TWordField;
    IndexList1TAGNAME: TStringField;
    IndexList1UNIQUE: TBooleanField;
    FieldList1TypeName: TStringField;
    FieldList1SubTypeName: TStringField;
    TableListPict: TBooleanField;
    TableListNAME: TStringField;
    FieldList1NAME: TStringField;
    IndexList1NAME: TStringField;
    FormStorage: TFormStorage;
    rxSplitter2: TrxSplitter;
    Panel6: TPanel;
    Panel7: TPanel;
    DBIndexCombo1: TDBIndexCombo;
    Label2: TLabel;
    PopupTablesMenu: TPopupMenu;
    FilterItem: TMenuItem;
    N1: TMenuItem;
    CloseItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TrayIconImage: TImage;
    TrayAbortImage: TImage;
    TrayMenu: TPopupMenu;
    CancelItem: TMenuItem;
    TableListVIEW: TBooleanField;
    PriorSQL: TSpeedButton;
    NextSQL: TSpeedButton;
    PopupSQLMenu: TPopupMenu;
    Undo1: TMenuItem;
    N2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    N4: TMenuItem;
    Saveas1: TMenuItem;
    Load1: TMenuItem;
    PriorSQLItem: TMenuItem;
    NextSQLItem: TMenuItem;
    Runquery1: TMenuItem;
    RefIntList: TTableItems;
    RefIntListNAME: TStringField;
    RefIntListOTHERTABLE: TStringField;
    RefIntListINTTYPE: TStringField;
    FieldList1Required: TBooleanField;
    CloseTableItem: TMenuItem;
    DbImages: TPicClip;
    IndexList1PRIMARY: TBooleanField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure TableListCalcFields(DataSet: TDataset);
    procedure RunSQLClick(Sender: TObject);
    procedure FieldList1CalcFields(DataSet: TDataset);
    procedure TablesGridDrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure TablesGridDblClick(Sender: TObject);
    procedure TablesGridKeyPress(Sender: TObject; var Key: Char);
    procedure GridDblClick(Sender: TObject);
    procedure AfterPost(DataSet: TDataset);
    procedure CloseItemClick(Sender: TObject);
    procedure FilterItemClick(Sender: TObject);
    procedure PopupSQLMenuClick(Sender: TObject);
    procedure PopupSQLMenuPopup(Sender: TObject);
    procedure SQLMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CancelQueryClick(Sender: TObject);
    procedure AfterOpen(DataSet: TDataset);
    procedure NavigateSQLClick(Sender: TObject);
    procedure FormStorageRestorePlacement(Sender: TObject);
    procedure FormStorageSavePlacement(Sender: TObject);
    procedure DataSource2StateChange(Sender: TObject);
    procedure RefIntListCalcFields(DataSet: TDataset);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CloseTableItemClick(Sender: TObject);
    procedure rxDBGrid2CheckButton(Sender: TObject; ACol: Longint;
      Field: TField; var Enabled: Boolean);
    procedure rxDBGrid2TitleBtnClick(Sender: TObject; ACol: Longint;
      Field: TField);
    procedure rxDBGrid2GetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
  private
    { Private declarations }
    FSQLHistoryIndex: Integer;
    FSQLHistory: TStrings;
    FQueryRunning: Boolean;
    FQueryStartTime: Longint;
    FRefIntListTYPE: TField; { for Delphi 16/32 compatibility }
{$IFDEF WIN32}
    FTrayIcon: TRxTrayIcon;
    FAbortQuery: Boolean;
{$ENDIF}
    function GetDatabaseName: string;
    function GetActiveDataSource: TDataSource;
    procedure SetDatabaseName(const Value: string);
    procedure CloseCurrent;
    procedure InternalOpenCurrent(const TabName: string);
    procedure UpdateFieldFormats(DataSet: TDataSet);
    procedure UpdateSQLHistory;
    procedure EnableSQLHistoryItems;
    procedure ExecSQL;
    procedure StartWatch;
    procedure StopWatch;
{$IFDEF WIN32}
    procedure QueryAborting(DataSet: TDataSet; var AbortQuery: Boolean);
{$ENDIF}
  public
    { Public declarations }
{$IFDEF WIN32}
    QueryDB: TDatabase;
{$ENDIF}
    function CheckStandard: Boolean;
    procedure UpdateSystemTables;
    procedure UpdateDataFieldFormats;
    procedure SetToCurrentTable;
    procedure PackCurrentTable;
    procedure CheckAndRepairParadoxTable(AllTables: Boolean);
    procedure ExportCurrentTable;
    procedure ImportToCurrentTable;
    procedure ReindexTable;
    function CurrentTable: TTable;
    function TransOperEnabled(Operation: TTransOperation): Boolean;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure RefreshData;
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property DataSource: TDataSource read GetActiveDataSource;
  end;

implementation

{$B-}
{$R *.DFM}

uses SysUtils, Clipbrd, DBConsts, TUtil, VCLUtils, ObjStr, Options,
  {$IFDEF WIN32} Bde, {$ELSE} DbiTypes, DbiProcs, {$ENDIF} FileUtil,
  EditStr, EditPict, ViewBlob, DbUtils, BdeUtils, Main, FiltDlg,
  DestTab, SrcTab, BdeInfo;

{ TMDIChild }

function TMDIChild.GetDatabaseName: string;
begin
  Result := TableList.DatabaseName;
end;

procedure TMDIChild.SetDatabaseName(const Value: string);
begin
  if Self.DatabaseName <> Value then begin
    TableList.Close;
    try
      TableList.DatabaseName := Value;
      TableList.SystemItems := SystemTables;
      Table1.DatabaseName := Value;
      Query1.DatabaseName := Value;
      FieldList1.DatabaseName := Value;
      IndexList1.DatabaseName := Value;
      RefIntList.DatabaseName := Value;
      TableList.Open;
      if Value <> '' then Caption := Format('Database: %s', [Value]);
    except
      Close;
      raise;
    end;
  end;
end;

procedure TMDIChild.RefreshData;
begin
  TableList.Close;
  try
    TableList.Open;
  except
    Close;
    raise;
  end;
end;

function TMDIChild.GetActiveDataSource: TDataSource;
begin
  Result := DataSource2;
end;

procedure TMDIChild.UpdateDataFieldFormats;
begin
  UpdateFieldFormats(Table1);
  UpdateFieldFormats(Query1);
  rxDBGrid2.Refresh;
  rxDBGrid3.Refresh;
end;

procedure TMDIChild.UpdateFieldFormats(DataSet: TDataSet);
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do begin
    case DataSet.Fields[I].DataType of
      ftFloat, ftCurrency, ftBCD:
        begin
          TNumericField(DataSet.Fields[I]).DisplayFormat := defFloatFormat;
          TNumericField(DataSet.Fields[I]).EditFormat := '#.##';
        end;
      ftDate: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defDateFormat;
      ftTime: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defTimeFormat;
      ftDateTime: TDateTimeField(DataSet.Fields[I]).DisplayFormat := defDateTimeFormat;
    end;
  end;
end;

procedure TMDIChild.UpdateSystemTables;
begin
  TableList.SystemItems := SystemTables;
end;

function TMDIChild.CurrentTable: TTable;
var
  Val: string;
begin
  if not TableList.Active then begin
    Result := nil;
    Exit;
  end;
  Val := TableListTABNAME.AsString;
  if Table1.Active then begin
    if Table1.TableName <> Val then SetToCurrentTable;
  end
  else begin
    Table1.TableName := Val;
  end;
  Result := Table1;
end;

function TMDIChild.CheckStandard: Boolean;
begin
  Result := False;
  if TableList.Database <> nil then
    Result := not TableList.Database.IsSQLBased;
end;

function TMDIChild.TransOperEnabled(Operation: TTransOperation): Boolean;
var
  InTransNow: Boolean;
begin
  Result := False;
  if (TableList.Database <> nil) and (TableList.Database.IsSQLBased) then
  begin
    InTransNow := TransActive(TableList.Database);
    case Operation of
      teStart: Result := not InTransNow;
      teCommit: Result := InTransNow;
      teRollback: Result := InTransNow;
    end;
  end;
end;

procedure TMDIChild.StartTransaction;
begin
  if TransOperEnabled(teStart) then begin
    TableList.Database.StartTransaction;
  end;
  TDBExplorerMainForm(Application.MainForm).UpdateMenus;
end;

procedure TMDIChild.Commit;
begin
  if TransOperEnabled(teCommit) then
  try
    if TransActive(TableList.Database) then
    try
      TableList.Database.Commit;
    except
      Rollback;
      raise;
    end;
    MessageDlg('Changes successfully commited to a database.',
      mtInformation, [mbOk], 0);
  finally
    TDBExplorerMainForm(Application.MainForm).UpdateMenus;
  end;
end;

procedure TMDIChild.Rollback;
begin
  try
    if TransActive(TableList.Database) then TableList.Database.Rollback;
  finally
    TDBExplorerMainForm(Application.MainForm).UpdateMenus;
  end;
end;

procedure TMDIChild.CheckAndRepairParadoxTable(AllTables: Boolean);
var
  KeepActive: Boolean;
  FullName: string;
begin
  if (not CheckStandard) or (not TableList.Active) then
    raise EDatabaseError.Create('Cannot perform this operation on a SQL database');
  KeepActive := Table1.Active;
  CloseCurrent;
  if not FQueryRunning then Query1.Close;
  try
    if AllTables then begin
      CheckTables(DatabaseName, crConfirmRepair);
      MessageDlg('Verification complete.', mtInformation, [mbOk], 0);
    end
    else begin
      FullName := DatabaseName;
      if not IsDirectory(FullName) then FullName := GetAliasPath(FullName);
      FullName := NormalDir(FullName) + TableListTABNAME.AsString;
      CheckTable(FullName, crConfirmRepair);
    end;
  finally
    if KeepActive then SetToCurrentTable;
  end;
end;

procedure TMDIChild.ExportCurrentTable;
var
  DestName: string;
  TabType: TTableType;
  RecCount: Longint;
  DestTable: TTable;
begin
  if (DataSource.DataSet <> nil) then begin
    if DataSource.DataSet.Active then DataSource.DataSet.CheckBrowseMode;
    if (DataSource.DataSet is TTable) then begin
      DestName := ExtractFileName(TTable(DataSource.DataSet).TableName);
      if not CheckStandard then begin
        if Pos('.', DestName) > 0 then
          DestName := Copy(DestName, Pos('.', DestName) + 1, MaxInt);
        if DestName = '' then DestName := '$table';
      end;
    end
    else begin
      if not DataSource.DataSet.Active then _DBError(SDataSetClosed);
      DestName := 'Query';
    end;
  end;
  TabType := ttDefault;
  RecCount := 0;
  if not GetDestTable(DestName, TabType, RecCount) then Exit;
  Update;
  DestTable := TTable.Create(Self);
  try
    DestTable.TableName := DestName;
    ExportDataSet(DataSource.DataSet as TBDEDataSet, DestTable, TabType,
      ASCIICharSet, ASCIIDelimited, RecCount);
    MessageDlg(Format('Table %s successfully created.', [DestTable.TableName]),
      mtInformation, [mbOk], 0);
  finally
    DestTable.Free;
  end;
end;

procedure TMDIChild.ImportToCurrentTable;
var
  DestTable: TTable;
  SrcName: string;
  MaxRecCnt: Longint;
  BatchMode: TBatchMode;
  Mappings: TStrings;
  SrcTable: TTable;
begin
  DestTable := CurrentTable;
  if DestTable <> nil then begin
    Mappings := TStringList.Create;
    try
      if GetImportParams(DestTable, SrcName, MaxRecCnt, Mappings,
        BatchMode) then
      begin
        SrcTable := TTable.Create(Self);
        try
          SrcTable.TableName := SrcName;
          ImportDataSet(SrcTable, DestTable, MaxRecCnt, Mappings, BatchMode);
        finally
          SrcTable.Free;
        end;
      end;
    finally
      Mappings.Free;
    end;
  end;
end;

procedure TMDIChild.InternalOpenCurrent(const TabName: string);
begin
  FieldList1.TableName := TabName;
  IndexList1.TableName := TabName;
  RefIntList.TableName := TabName;
  try
    if not Table1.Active then Table1.TableName := TabName;
    Table1.Open;
    FieldList1.Open;
    IndexList1.Open;
    if DataSource2.DataSet = RefIntList then
      RefIntList.Open;
  except
    CloseCurrent;
    raise;
  end;
end;

procedure TMDIChild.ReindexTable;
var
  Val: string;
begin
  if DataSource.DataSet = nil then Exit;
  StartWait;
  DataSource.DataSet.DisableControls;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then
      try
        BdeUtils.ReindexTable(Table1);
      finally
        InternalOpenCurrent(Val);
      end;
    end;
  finally
    DataSource.DataSet.EnableControls;
    StopWait;
  end;
end;

procedure TMDIChild.PackCurrentTable;
var
  Val: string;
begin
  StartWait;
  DataSource.DataSet.DisableControls;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then begin
        Table1.Open;
        try
          PackTable(Table1);
        finally
          InternalOpenCurrent(Val);
        end;
      end;
    end;
  finally
    DataSource.DataSet.EnableControls;
    StopWait;
  end;
end;

procedure TMDIChild.CloseCurrent;
begin
  Table1.Close;
  Table1.IndexFieldNames := '';
  FieldList1.Close;
  IndexList1.Close;
  RefIntList.Close;
end;

procedure TMDIChild.SetToCurrentTable;
var
  Val: string;
begin
  if DataSource.DataSet <> nil then
    DataSource.DataSet.DisableControls;
  StartWait;
  try
    CloseCurrent;
    if TableList.Active then begin
      Val := TableListTABNAME.AsString;
      if Table1.TableName <> Val then Table1.TableName := Val;
      if Val <> '' then InternalOpenCurrent(Val);
    end;
  finally
    StopWait;
    if DataSource.DataSet <> nil then
      DataSource.DataSet.EnableControls;
  end;
end;

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TMDIChild.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  if AutoActivate then SetToCurrentTable;
end;

procedure TMDIChild.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  KeepPage: Integer;
  KeepDS: TDataSet;
begin
  KeepPage := Notebook1.PageIndex;
  KeepDS := DataSource2.DataSet;
  try
    case NewTab of
      0: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := Table1;
         end;
      1: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := FieldList1;
         end;
      2: begin
           Notebook1.PageIndex := 0;
           DataSource2.DataSet := IndexList1;
         end;
      3: begin
           Notebook1.PageIndex := 0;
           if (RefIntList.TableName = Table1.TableName) and Table1.Active then
           begin
             StartWait;
             try
               RefIntList.Open;
             finally
               StopWait;
             end;
           end;
           DataSource2.DataSet := RefIntList;
         end;
      4: begin
           Notebook1.PageIndex := 1;
           if not FQueryRunning then DataSource2.DataSet := Query1
           else DataSource2.DataSet := nil;
         end;
    end;
  except
    AllowChange := False;
    Notebook1.PageIndex := KeepPage;
    DataSource2.DataSet := KeepDS;
    raise;
  end;
end;

procedure TMDIChild.TableListCalcFields(DataSet: TDataset);
begin
  TableListTABNAME.AsString := TableList.ItemName;
end;

procedure TMDIChild.RefIntListCalcFields(DataSet: TDataset);
begin
  if FRefIntListTYPE = nil then RefIntListINTTYPE.AsString := ''
  else
    case RINTType(FRefIntListTYPE.AsInteger) of
      rintMASTER: RefIntListINTTYPE.AsString := 'Master';
      rintDEPENDENT: RefIntListINTTYPE.AsString := 'Dependent';
      else RefIntListINTTYPE.AsString := '';
    end;
end;

procedure TMDIChild.StartWatch;begin
  if FQueryRunning then SysUtils.Abort;
  FQueryStartTime := GetTickCount;
end;

procedure TMDIChild.StopWatch;
var
  H, M, S: Longint;
begin
  if (Query1.OpenStatus in [qsExecuted, qsOpened]) and
    (FQueryStartTime > 0) then
  begin
    S := (GetTickCount - FQueryStartTime) div 1000;
    M := S div 60;
    S := S - (M * 60);
    H := M div 60;
    M := M - (H * 60);
    FQueryStartTime := 0;
    MessageDlg(Format('Query successfully executed. Time elapsed: %d:%d:%d.',
      [H, M, S]), mtInformation, [mbOk], 0);
  end;
end;

procedure TMDIChild.ExecSQL;
begin
  StartWatch;
  StartWait;
  try
    Query1.OpenOrExec(True);
  finally
    StopWait;
  end;
  if ShowExecTime then StopWatch
  else if (Query1.OpenStatus = qsExecuted) then
    MessageDlg('Query successfully executed.', mtInformation, [mbOk], 0);
end;

{$IFDEF WIN32}

procedure TMDIChild.QueryAborting(DataSet: TDataSet; var AbortQuery: Boolean);
begin
  if FQueryRunning and (DataSet = Query1) and EnableQueryAbort then
  begin
    CancelItem.Enabled := True;
    FTrayIcon.Icon := TrayAbortImage.Picture.Icon;
    AbortQuery := FAbortQuery;
  end;
end;

{$ENDIF WIN32}

procedure TMDIChild.CancelQueryClick(Sender: TObject); { for 32-bit only }
begin
{$IFDEF WIN32}
  if FQueryRunning then begin
    FAbortQuery := True;
    FTrayIcon.Hint := Format('%s: query aborting...', [DatabaseName]);
    FTrayIcon.Icon := TrayIconImage.Picture.Icon;
  end;
  CancelItem.Enabled := False;
{$ENDIF WIN32}
end;

procedure TMDIChild.RunSQLClick(Sender: TObject);
begin
  if FQueryRunning then Exit;
  Query1.Close;
  Query1.SQL := SQLMemo.Lines;
  if SQLMemo.Lines.Count = 0 then Exit;
  Query1.RequestLive := LiveQueries;
  Query1.Params.Clear;
  Query1.Macros.Clear;
  Query1.Unprepare;
  UpdateSQLHistory;
  ExecSQL;
end;

procedure TMDIChild.UpdateSQLHistory;
begin
  if (SQLMemo.Modified) and (SQLMemo.Lines.Count > 0) then begin
    while FSQLHistory.Count >= SQLHistoryCapacity do
      if FSQLHistory.Count > 0 then FSQLHistory.Delete(0);
    if (SQLHistoryCapacity > 0) then begin
      FSQLHistoryIndex := FSQLHistory.AddObject('',
        TStringList.Create);
      TStrings(FSQLHistory.Objects[FSQLHistoryIndex]).Assign(SQLMemo.Lines);
      SQLMemo.Modified := False;
    end;
  end;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.EnableSQLHistoryItems;
begin
  PriorSQL.Enabled := ((FSQLHistoryIndex > 0) or (FSQLHistoryIndex = -1)) and
    (FSQLHistory.Count > 0);
  PriorSQLItem.Enabled := PriorSQL.Enabled;
  NextSQL.Enabled := (FSQLHistoryIndex <> -1);
  NextSQLItem.Enabled := NextSQL.Enabled;
end;

procedure TMDIChild.FieldList1CalcFields(DataSet: TDataset);
var
  F: TField;
begin
  FieldList1TypeName.AsString := FieldTypeName(FieldList1TYPE.AsInteger);
  FieldList1SubTypeName.AsString := FieldSubtypeName(FieldList1SUBTYPE.AsInteger);
  F := Table1.FindField(FieldList1NAME.AsString);
  if F <> nil then
    FieldList1Required.AsBoolean := (F.Tag = 2) or F.Required;
end;

procedure TMDIChild.TablesGridDrawDataCell(Sender: TObject;
  const Rect: TRect; Field: TField; State: TGridDrawState);
var
  I: Integer;
begin
  if Field.FieldName = 'Pict' then begin
    if TableListVIEW.AsBoolean then I := 1 else I := 0;
    DbImages.DrawCenter(TablesGrid.Canvas, Rect, I);
  end;
end;

procedure TMDIChild.TablesGridDblClick(Sender: TObject);
begin
  {if not AutoActivate then }SetToCurrentTable;
end;

procedure TMDIChild.TablesGridKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Char(VK_RETURN)) {and not AutoActivate }then
    SetToCurrentTable;
end;

procedure TMDIChild.GridDblClick(Sender: TObject);
var
  F: TField;
begin
  if GetActiveDataSource.State in [dsBrowse, dsEdit, dsInsert] then begin
    F := (Sender as TrxDBGrid).SelectedField;
    if F = nil then Exit;
    if (F.DataType in [ftMemo]) then
      StrListEdit(GetActiveDataSource.DataSet, F.FieldName)
    else if (F.DataType in [ftGraphic]) then
      PictureEdit(GetActiveDataSource.DataSet, F.FieldName)
    else if (F.DataType in ftBlobTypes) then
      BlobView(GetActiveDataSource.DataSet, F.FieldName);
    (Sender as TrxDBGrid).Update;
  end;
end;

procedure TMDIChild.AfterPost(DataSet: TDataset);
begin
  try
    DataSet.Refresh;
  except
  end;
end;

procedure TMDIChild.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMDIChild.FilterItemClick(Sender: TObject);
var
  TabMask: string;
  P: TPoint;
begin
  TabMask := TableList.FileMask;
  P.X := TablesGrid.Left + 25;
  P.Y := TablesGrid.Top + 25;
  P := ClientToScreen(P);
  if ShowFilterDialog(TabMask, P.X, P.Y) then
    TableList.FileMask := TabMask;
end;

procedure TMDIChild.PopupSQLMenuClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    1: if SQLMemo.Perform(EM_CANUNDO, 0, 0) <> 0 then
         SQLMemo.Perform(EM_UNDO, 0, 0);
    2: SQLMemo.CutToClipboard;
    3: SQLMemo.CopyToClipboard;
    4: SQLMemo.PasteFromClipboard;
    5: SQLMemo.SelectAll;
    6: if SaveDialog1.Execute then begin
         SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
         SQLMemo.Lines.SaveToFile(SaveDialog1.FileName);
       end;
    7: if OpenDialog1.Execute then begin
         OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
         SQLMemo.Lines.LoadFromFile(OpenDialog1.FileName);
         SQLMemo.Modified := True;
         UpdateSQLHistory;
       end;
    8: RunSQLClick(Sender);
    9: NavigateSQLClick(PriorSQL);
   10: NavigateSQLClick(NextSQL);
  end;
end;

procedure TMDIChild.PopupSQLMenuPopup(Sender: TObject);
var
  EnableCopy: Boolean;
begin
  EnableCopy := SQLMemo.SelLength <> 0;
  Undo1.Enabled := (SQLMemo.Perform(EM_CANUNDO, 0, 0) <> 0);
  Cut1.Enabled := EnableCopy;
  Copy1.Enabled := EnableCopy;
  Paste1.Enabled := Clipboard.HasFormat(CF_TEXT);
  SelectAll1.Enabled := SQLMemo.Lines.Count > 0;
  Saveas1.Enabled := SQLMemo.Lines.Count > 0;
  Runquery1.Enabled := SQLMemo.Lines.Count > 0;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.SQLMemoChange(Sender: TObject);
begin
  RunSQL.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
  Runquery1.Enabled := (SQLMemo.Lines.Count > 0) and not FQueryRunning;
end;

procedure TMDIChild.FormCreate(Sender: TObject);
var
  FldDef: TFieldDef;
begin
  { for Delphi32 compatibility }
  TableListNAME.Size := DBIMAXTBLNAMELEN;
  IndexList1NAME.Size := DBIMAXTBLNAMELEN;
  RefIntListOTHERTABLE.Size := DBIMAXTBLNAMELEN;
  FSQLHistoryIndex := -1;
  FSQLHistory := TObjectStrings.Create;
  Notebook1.PageIndex := 0;
  FQueryRunning := False;
  EnableSQLHistoryItems;
{$IFDEF WIN32}
  FTrayIcon := TrxTrayIcon.Create(Self);
  { dynamic creation for 16-bit compatibility }
  with FTrayIcon do begin
    Active := False;
    Icon := TrayIconImage.Picture.Icon;
    PopupMenu := TrayMenu;
  end;
  {$IFNDEF RX_D4}
  Query1.OnServerYield := QueryAborting;
  {$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  RefIntList.FieldDefs.Add('TYPE', ftInteger, 0, False);
{$ELSE}
  RefIntList.FieldDefs.Add('TYPE', ftWord, 0, False);
{$ENDIF}
  FldDef := RefIntList.FieldDefs.Find('TYPE');
  if FldDef <> nil then begin
    FRefIntListTYPE := FldDef.CreateField(RefIntList);
    FRefIntListTYPE.Visible := False;
  end;
end;

procedure TMDIChild.FormDestroy(Sender: TObject);
var
  DBase: TDatabase;
begin
  CloseCurrent;
  Query1.Close;
  DBase := TableList.Database;
  TableList.Close;
  Session.CloseDatabase(DBase);
  FSQLHistory.Free;
  FSQLHistory := nil;
end;

procedure TMDIChild.AfterOpen(DataSet: TDataset);
var
  I: Integer;
begin
  UpdateFieldFormats(DataSet);
  for I := 0 to DataSet.FieldCount - 1 do
    if DataSet.Fields[I].Required then begin
      DataSet.Fields[I].Required := False;
      DataSet.Fields[I].Tag := 2;
    end;
end;

procedure TMDIChild.NavigateSQLClick(Sender: TObject);
var
  NewSQL: Boolean;
begin
  if (FSQLHistory = nil) or (FSQLHistory.Count = 0) then Exit;
  NewSQL := False;
  if Sender = PriorSQL then begin
    if FSQLHistoryIndex > 0 then Dec(FSQLHistoryIndex)
    else if FSQLHistoryIndex = -1 then begin
      UpdateSQLHistory;
      FSQLHistoryIndex := FSQLHistory.Count - 1;
    end;
  end
  else if Sender = NextSQL then begin
    if FSQLHistoryIndex = -1 then UpdateSQLHistory;
    if FSQLHistoryIndex < FSQLHistory.Count - 1 then
      Inc(FSQLHistoryIndex)
    else begin
      NewSQL := True;
    end;
  end;
  if NewSQL then begin
    FSQLHistoryIndex := -1;
    SQLMemo.Clear;
    SQLMemo.Modified := False;
  end
  else begin
    SQLMemo.Lines.Assign(TStrings(FSQLHistory.Objects[FSQLHistoryIndex]));
    SQLMemo.Modified := False;
  end;
  EnableSQLHistoryItems;
end;

procedure TMDIChild.FormStorageRestorePlacement(Sender: TObject);
begin
  RestoreFields(FieldList1, FormStorage.IniFile, False);
  RestoreFields(IndexList1, FormStorage.IniFile, False);
  RestoreFields(RefIntList, FormStorage.IniFile, False);
end;

procedure TMDIChild.FormStorageSavePlacement(Sender: TObject);
begin
  SaveFields(FieldList1, FormStorage.IniFile);
  SaveFields(IndexList1, FormStorage.IniFile);
  SaveFields(RefIntList, FormStorage.IniFile);
end;

procedure TMDIChild.DataSource2StateChange(Sender: TObject);
var
  CanEdit: Boolean;
begin
  CanEdit := (DataSource2.DataSet <> nil) and DataSource2.DataSet.CanModify;
  with rxDBGrid2 do begin
    ReadOnly := not CanEdit;
  end;
  with rxDBGrid3 do begin
    ReadOnly := not CanEdit;
  end;
end;

procedure TMDIChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FQueryRunning then
    MessageDlg('You cannot close database while query is running.',
    mtWarning, [mbOk], 0);
  CanClose := not FQueryRunning;
  if CanClose then begin
    TDBExplorerMainForm(Application.MainForm).ClosedDatabases.Add(DatabaseName, 0);
    if TransOperEnabled(teCommit) then begin
      case MessageDlg('You have uncommited changes. Commit changes to a database?',
        mtWarning, mbYesNoCancel, 0) of
        mrYes: Commit;
        mrNo: Rollback;
        mrCancel: CanClose := False;
      end;
    end;
  end;
end;

procedure TMDIChild.CloseTableItemClick(Sender: TObject);
begin
  CloseCurrent;
end;

procedure TMDIChild.rxDBGrid2CheckButton(Sender: TObject; ACol: Longint;
  Field: TField; var Enabled: Boolean);
begin
  Enabled := (TRxDBGrid(Sender).DataSource.DataSet is TTable) and
    (Field <> nil) and not (Field is TBlobField) and
    (TTable(TRxDBGrid(Sender).DataSource.DataSet).IndexDefs.Count > 0);
end;

procedure TMDIChild.rxDBGrid2TitleBtnClick(Sender: TObject; ACol: Longint;
  Field: TField);
begin
  if TRxDBGrid(Sender).DataSource.DataSet is TTable then
  try
    TTable(TRxDBGrid(Sender).DataSource.DataSet).IndexFieldNames :=
      Field.FieldName;
  except
    TTable(TRxDBGrid(Sender).DataSource.DataSet).IndexFieldNames := '';
  end;
end;

procedure TMDIChild.rxDBGrid2GetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
begin
  if (TRxDBGrid(Sender).DataSource.DataSet is TTable) and (Field <> nil) and
    (Field.IsIndexField) then
  begin
    SortMarker := smDown;
  end;
end;

end.