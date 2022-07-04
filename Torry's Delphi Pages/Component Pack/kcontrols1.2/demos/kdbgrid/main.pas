unit Main;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, LResources, SQLdb, odbcconn,
{$ELSE}
  Windows, Messages, ADODB, Mask,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBCtrls, KGrids, KDBGrids,
  ExtCtrls, KFunctions, KGraphics, ActnList, KControls, KIcon, 
  KDialogs, Grids, DBGrids;

type

  { TMainForm }

  TMainForm = class(TForm)
    ACPrint: TAction;
    BUModify: TButton;
    BUPrint: TButton;
    EDConnectionString: TEdit;
    PSDMain: TKPrintSetupDialog;
    Label1: TLabel;
    Label2: TLabel;
    EDTable: TEdit;
    EDFirstCol: TDBEdit;
    BUOpen: TButton;
    BUClose: TButton;
    ALMain: TActionList;
    ACOpen: TAction;
    ACClose: TAction;
    DBNav: TDBNavigator;
    DSMain: TDataSource;
    Label3: TLabel;
    ACModify: TAction;
    BUAutoSize: TButton;
    DBGrid: TKDBGrid;
    procedure DBGridDrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
      State: TKGridDrawState);
    procedure DBGridMouseClickCell(Sender: TObject; ACol, ARow: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DBGridCustomSortRows(Sender: TObject; ByIndex: Integer;
      SortMode: TKGridSortMode; var Sorted: Boolean);
    procedure DBGridEditorCreate(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TWinControl);
    procedure ACOpenExecute(Sender: TObject);
    procedure ACOpenUpdate(Sender: TObject);
    procedure ACCloseExecute(Sender: TObject);
    procedure ACCloseUpdate(Sender: TObject);
    procedure ACModifyExecute(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ACPrintExecute(Sender: TObject);
    procedure ACPrintUpdate(Sender: TObject);
    procedure BUAutoSizeClick(Sender: TObject);
  private
    { Private declarations }
  public
  {$IFDEF FPC}
    CN: TODBCConnection;
    Table: TSQLQuery;
    Trans: TSQLTransaction;
    Query: string;
  {$ELSE}
    CN: TADOConnection;
    Table: TADOTable;
  {$ENDIF}
  {$IFDEF FPC}
    procedure MyAfterPost(DataSet: TDataSet);
  {$ENDIF}
  end;

var
  MainForm: TMainForm;

implementation


procedure TMainForm.FormCreate(Sender: TObject);
begin
{$IFDEF FPC}
  CN := TODBCConnection.Create(Self);
  CN.LoginPrompt := False;
  Trans := TSQLTransaction.Create(Self);
  Trans.DataBase := CN;
  Table := TSQLQuery.Create(Self);
  Table.DataBase := CN;
  Table.Active := False;
  Table.ParseSQL := True;
  Table.UsePrimaryKeyAsKey := False;
  Table.Transaction := Trans;
  Table.UpdateMode := upWhereChanged;
  Table.AfterPost := MyAfterPost;
{$ELSE}
  CN := TAdoConnection.Create(Self);
  CN.LoginPrompt := False;
  Table := TADOTable.Create(Self);
  Table.Connection := CN;
  Table.Active := False;
{$ENDIF}
  DSMain.DataSet := Table;

  DBGrid.DoubleBuffered := True; // TKGrid is pretty flicker free but this is still better
  DBGrid.ColWidths[0] := 40;
  DBGrid.Cols[2].CellHint := True; //turn on cell hints for the 3rd column

  Randomize;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  // no way for kgrid to get notified...
  DBGrid.HideCellHint;
end;

procedure TMainForm.ACCloseExecute(Sender: TObject);
begin
  Table.Close;
end;

procedure TMainForm.ACCloseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Table.Active;
end;

procedure TMainForm.ACModifyExecute(Sender: TObject);
var
  RandomCol, RandomRow: Integer;
begin
  DBGrid.EditorMode := False;
  RandomCol := Random(DBGrid.ColCount - DBGrid.FixedCols) + DBGrid.FixedCols;
  RandomRow := Random(DBGrid.RowCount - DBGrid.FixedRows) + DBGrid.FixedRows;
  DBGrid.Cells[RandomCol, RandomRow] := IntToStr(Random(1000000));
  DBGrid.FocusCell(RandomCol, RandomRow);
  // if DBGrid.Commit is called here the modified cell is written into database
  // immediately, but the modified cell might not be focused anymore so don't call it here
end;

{$IFDEF FPC}
procedure TMainForm.MyAfterPost(DataSet: TDataSet);
begin
  try
    Table.ApplyUpdates; // must call this to apply the post in Lazarus
    Trans.Commit;
  except
    on E: Exception do
      MessageBox(Handle, PChar('Problem with updating: ' + E.Message), PChar(Caption), MB_OK);
  end;
end;
{$ENDIF}

procedure TMainForm.ACOpenExecute(Sender: TObject);
begin
  try
  {$IFDEF FPC}
    if CN.DatabaseName <> EDConnectionString.Text then
    begin
      CN.Connected := False;
      CN.DatabaseName := EDConnectionString.Text;
      CN.Connected := True;
    end;
    Query := 'SET NAMES ''utf8''';
    Table.SQL.Clear;
    Table.SQL.Add(Query);
    Table.Open;
    Table.Close;
    Query := 'SELECT * FROM ' + EDTable.Text;
    Table.SQL.Clear;
    Table.SQL.Add(Query);
  {$ELSE}
    if CN.ConnectionString <> EDConnectionString.Text then
    begin
      CN.Connected := False;
      CN.ConnectionString := EDConnectionString.Text;
      CN.Connected := True;
    end;
    Table.TableName := EDTable.Text;
  {$ENDIF}
    Table.Open;
    DBGrid.PageSetup.Title := 'Table: ' + EDTable.Text;
    EDFirstCol.DataField := Table.FieldDefs[0].Name;
  except
    MessageBox(Handle, 'Cannot connect or find the table!', PChar(Caption), MB_OK);
  end;
end;

procedure TMainForm.ACOpenUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not Table.Active;
end;

procedure TMainForm.ACPrintExecute(Sender: TObject);
begin
  PSDMain.Execute;
end;

procedure TMainForm.ACPrintUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DBGrid.CanPrint and Table.Active;
end;

procedure TMainForm.BUAutoSizeClick(Sender: TObject);
begin
  DBGrid.AutoSizeRow(DBGrid.Row);
end;

procedure TMainForm.DBGridCustomSortRows(Sender: TObject; ByIndex: Integer;
  SortMode: TKGridSortMode; var Sorted: Boolean);
var
  ColumnName, Sort: WideString;
begin
  // adapt as you wish or better what your db supports
  if TKDBGridCol(TKCustomGrid(Sender).Cols[ByIndex]).DataType in
    [ftString, ftWideString, ftInteger, ftSmallInt, ftWord, ftLargeInt, ftFloat,
     ftDate, ftTime, ftDateTime, ftBCD, ftFmtBCD, ftCurrency, ftBoolean] then
  begin
    ColumnName := TKDBGridCol(TKCustomGrid(Sender).Cols[ByIndex]).Name; // get column name
    case SortMode of
      smNone: Sort := '';
      smDown: Sort := '[' + ColumnName +'] ASC';
      smUp: Sort := '[' + ColumnName +'] DESC';
    end;
  {$IFDEF FPC}
    if Sort <> '' then Sort := ' ORDER BY ' + Sort;
    Table.Close;
    Table.SQL.Clear;
    Table.SQL.Add(Query + Sort);
    Table.Open;
  {$ELSE}
    Table.Sort := Sort;
  {$ENDIF}
    Sorted := True;
  end else
    Sorted := False;
end;

procedure TMainForm.DBGridEditorCreate(Sender: TObject; ACol, ARow: Integer;
  var AEditor: TWinControl);
begin
  // you have still full control about the inplace editor but
  // here we just use the default handling
  // (if you delete this event the same is used in TKDBGridCell)
  DBGrid.DefaultEditorCreate(ACol, ARow, AEditor);
  // use default handling for other inplace editor events
end;

procedure TMainForm.DBGridMouseClickCell(Sender: TObject; ACol, ARow: Integer);
begin
  // show hint immediately when cell is clicked
//  DBGrid.ShowCellHint;
end;

procedure TMainForm.DBGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
begin
  with TKCustomGrid(Sender) do
  begin
    Cell[ACol, ARow].ApplyDrawProperties;
    CellPainter.GraphicHPadding := 3;
    CellPainter.DefaultDraw;
  end;
end;

{$IFDEF FPC}
initialization
  {$i main.lrs}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
end.
