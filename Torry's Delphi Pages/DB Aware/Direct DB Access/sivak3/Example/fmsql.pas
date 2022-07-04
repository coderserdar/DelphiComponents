unit fmsql;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sql3_defs, SynEdit, SynMemo, DB, Grids, DBGrids,
  SynEditHighlighter, SynHighlighterSQL, ActnList, Menus, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, SynHighlighterGeneral, DBCtrls;

type
  Tfsql = class(TForm)
    Pages: TPanel;
    simple: TSivak3SimpleTable;
    exec: TSivak3Exec;
    table: TSivak3Table;
    ds: TDataSource;
    syntax: TSynSQLSyn;
    tabInfo: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    grid: TDBGrid;
    tabSQL: TPanel;
    splitter: TSplitter;
    sql: TSynMemo;
    res: TSynMemo;
    Panel1: TPanel;
    tabDepend: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    TreeDepend: TTreeView;
    Memo: TSynMemo;
    InfoCaption: TStaticText;
    dependCaption: TStaticText;
    sqlCaption: TStaticText;
    actions: TActionList;
    aExec: TAction;
    aClear: TAction;
    aSaveSql: TAction;
    aLoadSql: TAction;
    OpenD: TOpenDialog;
    SaveD: TSaveDialog;
    indexpopup: TPopupMenu;
    aCreateUIndex: TAction;
    aCreateIndex: TAction;
    CreateIndex1: TMenuItem;
    CreateUniqueIndex1: TMenuItem;
    sqlpopup: TPopupMenu;
    ExecuteSQL1: TMenuItem;
    N1: TMenuItem;
    LoadSQL1: TMenuItem;
    SaveSQL1: TMenuItem;
    N2: TMenuItem;
    CreateIndex2: TMenuItem;
    CreateUniqueIndex2: TMenuItem;
    Clearresults1: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    aAddRec: TAction;
    aDelRec: TAction;
    aAlter: TAction;
    aRename: TAction;
    triggerpopup: TPopupMenu;
    aBeforeInsert: TAction;
    aBeforeUpdate: TAction;
    aBeforeDelete: TAction;
    aAfterInsert: TAction;
    aAfterUpdate: TAction;
    aAfterDelete: TAction;
    aUpdateOf: TAction;
    aInsteadOfInsert: TAction;
    BeforeInsert1: TMenuItem;
    BeforeDelete1: TMenuItem;
    BeforeUpdate1: TMenuItem;
    N3: TMenuItem;
    AfterInsert1: TMenuItem;
    AfterUpdate1: TMenuItem;
    aAfterDelete1: TMenuItem;
    N4: TMenuItem;
    UpdateOf1: TMenuItem;
    InsteadOf1: TMenuItem;
    aInsteadOfUpdate: TAction;
    aInsteadOfDelete: TAction;
    N5: TMenuItem;
    InsteadOfDelete1: TMenuItem;
    InsteadOfUpdate1: TMenuItem;
    sqlcreatepopup: TPopupMenu;
    aCreateTable: TAction;
    aCreateView: TAction;
    CreateTable1: TMenuItem;
    CreateView1: TMenuItem;
    CreateIndex3: TMenuItem;
    CreateIndex4: TMenuItem;
    CreateUniqueIndex3: TMenuItem;
    CreateTrigger1: TMenuItem;
    BeforeInsert2: TMenuItem;
    BeforeUpdate2: TMenuItem;
    BeforeDelete2: TMenuItem;
    N6: TMenuItem;
    AfterInsert2: TMenuItem;
    AfterUpdate2: TMenuItem;
    AfterDelete1: TMenuItem;
    N7: TMenuItem;
    InsteadOfInsert1: TMenuItem;
    InsteadOfUpdate2: TMenuItem;
    InsteadOfDelete2: TMenuItem;
    N8: TMenuItem;
    UpdateOffield1: TMenuItem;
    SpeedButton11: TSpeedButton;
    N9: TMenuItem;
    aOpenBlob: TAction;
    tabtab: TSynMemo;
    syntaxtab: TSynGeneralSyn;
    SpeedButton13: TSpeedButton;
    aRefresh: TAction;
    Status: TStatusBar;
    aModify: TAction;
    SpeedButton14: TSpeedButton;
    dbm: TDBMemo;
    Panel4: TPanel;
    SpeedButton12: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    aPragma: TAction;
    N10: TMenuItem;
    Showpragmavalues1: TMenuItem;
    SetPragma1: TMenuItem;
    autovacuum1: TMenuItem;
    automaticindex1: TMenuItem;
    cachesize1: TMenuItem;
    casesensitivelike1: TMenuItem;
    countchanges1: TMenuItem;
    foreignkeys1: TMenuItem;
    fullcolumnnames1: TMenuItem;
    lockingmode1: TMenuItem;
    readuncommitted1: TMenuItem;
    recursivetriggers1: TMenuItem;
    reverseunorderedselects1: TMenuItem;
    securedelete1: TMenuItem;
    shortcolumnnames1: TMenuItem;
    synchronous1: TMenuItem;
    aSystem: TAction;
    Systemtable1: TMenuItem;
    aSelectSql: TAction;
    SelectSQL1: TMenuItem;
    aFilter: TAction;
    SpeedButton15: TSpeedButton;
    Popup: TPopupMenu;
    Addrecord1: TMenuItem;
    Deleterecord1: TMenuItem;
    N11: TMenuItem;
    Filter1: TMenuItem;
    BLOB1: TMenuItem;
    N12: TMenuItem;
    dropfilter: TMenuItem;
    query: TSivak3Query;
    SpeedButton16: TSpeedButton;
    aLocate: TAction;
    ImageInfo: TImage;
    ImageDep: TImage;
    ImageSQL: TImage;
    Bevel2: TBevel;
    aCreateInsert: TAction;
    CreateInsertSQL1: TMenuItem;
    N13: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeDependGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeDependChange(Sender: TObject; Node: TTreeNode);
    procedure InfoCaptionClick(Sender: TObject);
    procedure dependCaptionClick(Sender: TObject);
    procedure sqlCaptionClick(Sender: TObject);
    procedure aExecExecute(Sender: TObject);
    procedure execFetchRecord(Sender: TObject; ColCount: Integer; Values, Names: array of String; var Cancel: Boolean);
    procedure aSaveSqlExecute(Sender: TObject);
    procedure aLoadSqlExecute(Sender: TObject);
    procedure aClearExecute(Sender: TObject);
    procedure DropDownClick(Sender: TObject);
    procedure aAddRecExecute(Sender: TObject);
    procedure aDelRecExecute(Sender: TObject);
    procedure SQLCreateExecute(Sender: TObject);
    procedure aOpenBlobExecute(Sender: TObject);
    procedure gridDblClick(Sender: TObject);
    procedure gridColEnter(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aModifyExecute(Sender: TObject);
    procedure aPragmaExecute(Sender: TObject);
    procedure SetPragmaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aSystemExecute(Sender: TObject);
    procedure aSelectSqlExecute(Sender: TObject);
    procedure gridTitleClick(Column: TColumn);
    procedure aFilterExecute(Sender: TObject);
    procedure dropfilterClick(Sender: TObject);
    procedure aLocateExecute(Sender: TObject);
    procedure aCreateInsertExecute(Sender: TObject);
  private
    { Private declarations }
    db: TSivak3Database;
    Node,
    node_primary, node_foreign, node_indexes, node_triggers: TTreeNode;
    outcount: Integer;
    isview: Boolean;
    objname: String;
    objtype: String;
    curline: String;
    procedure InitSqlWindow;
    procedure RefreshSqlWindow;
    procedure AddToTree(Node: TTreeNode; list: TStrings; imageIx: Integer);
    procedure LoadIni;
    procedure SaveIni;
    procedure DropDownSpeed(sp: TSpeedButton);
    procedure ShowDbState;
    procedure ExecAsExec;
    procedure ExecAsQuery;
  public
    { Public declarations }
  end;


function SqlWindow(ANode: TTreeNode): HWND;

implementation

uses
  udm, IniFiles, fmblob, fmfilter, fparmsfill, fmlocate;

{$R *.dfm}

function SqlWindow(ANode: TTreeNode): HWND;
var
  f: Tfsql;
  n: TTreeNode;
  s: String;
begin
  n := ANode;
  while Assigned(n.Parent) and (n.Level > 2) do
  n := n.Parent;
  s := '  ' + n.Text + ' - ' + TSivak3Database(ANode.Data).Description;
  Result := FindWindow('Tfsql', PChar(s));
  if Result = 0 then
  begin
    f := Tfsql.Create(Application);
    try
      f.Node := ANode;
      Result := f.Handle;
      f.Caption := s;
      f.InitSqlWindow;
    finally
      f.Show;
    end;  
  end;
  SetForegroundWindow(Result);
end;

{ Tfsql }

procedure Tfsql.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  dm.RegisterSqlClient(Self);
  actions.Images := dm.ImagesBtn;
  Popup.Images := dm.ImagesBtn;
  node_primary := TreeDepend.Items.AddChild(nil, 'Primary key');
  node_primary.ImageIndex := 11;
  node_foreign := TreeDepend.Items.AddChild(nil, 'Foreign keys');
  node_foreign.ImageIndex := 12;
  node_indexes := TreeDepend.Items.AddChild(nil, 'Indexes');
  node_indexes.ImageIndex := 10;
  node_triggers := TreeDepend.Items.AddChild(nil, 'Triggers');
  node_triggers.ImageIndex := 5;
  db := nil;
  objname := '';
  objtype := '';
  isview := false;
  LoadIni;
  i := Application.MainForm.Left + Application.MainForm.Width;
  if i + Width > Screen.Width then
  i := Screen.Width - Width;
  if i < 0 then
  i := 0;
  Left := i;
  Top := Application.MainForm.Top + (dm.ClientCount * 24);
end;

procedure Tfsql.FormShow(Sender: TObject);
begin
  if res.Height > Height - 128 then
  res.Height := Height div 3;
end;

procedure Tfsql.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if WindowState = wsNormal then
  SaveIni;
  Action := caFree;
end;

procedure Tfsql.FormDestroy(Sender: TObject);
begin
  dm.UnregisterSqlClient(Self);
end;

procedure Tfsql.LoadIni;
var
  f: TIniFile;
begin
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    Width := f.ReadInteger('SQLWIN', 'width', Width);
    Height := f.ReadInteger('SQLWIN', 'heigth', Height);
    res.Height := f.ReadInteger('SQLWIN', 'splitter', 256);
  finally
    f.Free;
  end;
end;

procedure Tfsql.SaveIni;
var
  f: TIniFile;
begin
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    f.WriteInteger('SQLWIN', 'width', Width);
    f.WriteInteger('SQLWIN', 'heigth', Height);
    f.WriteInteger('SQLWIN', 'splitter', res.Height);
  finally
    f.Free;
  end;
end;

procedure Tfsql.InitSqlWindow;
begin
  db := TSivak3Database(Node.Data);
  while Assigned(Node.Parent) do
  begin
    if Node.Level in [2, 4] then
    objname := Node.Text;
    Node := Node.Parent;
    objtype := db.ObjectType[objname];
    Pages.Caption := '  ' + objtype + ': ' + objname;
  end;

  isview := objtype = 'view';
  InfoCaption.Enabled := (objtype = 'table') or (objtype = 'view');
  dependCaption.Enabled := InfoCaption.Enabled; //objtype = 'table';

  exec.Database := db;
  simple.Database := db;
  table.Database := db;
  query.Database := db;
  Tag := Integer(db);
  
  if InfoCaption.Enabled then
  begin
    aSelectSql.Enabled := true;
    aCreateUIndex.Enabled := not isview;
    aCreateIndex.Enabled := not isview;
    aBeforeInsert.Enabled := not isview;
    aBeforeUpdate.Enabled := not isview;
    aBeforeDelete.Enabled := not isview;
    aAfterInsert.Enabled := not isview;
    aAfterUpdate.Enabled := not isview;
    aAfterDelete.Enabled := not isview;
    aUpdateOf.Enabled := not isview;
    aCreateInsert.Enabled := not isview;
    aInsteadOfInsert.Enabled := isview;
    aInsteadOfUpdate.Enabled := isview;
    aInsteadOfDelete.Enabled := isview;
  end;

  if isview then
  tabInfo.Caption := 'View ' + objname + ' '
  else tabInfo.Caption := 'Table ' + objname + ' ';

  RefreshSqlWindow;
  if InfoCaption.Enabled then
  InfoCaptionClick(InfoCaption)
  else sqlCaptionClick(sqlCaption);
end;

procedure Tfsql.RefreshSqlWindow;
var
  l: TStringList;
  i: Integer;
begin
  node_primary.DeleteChildren;
  node_foreign.DeleteChildren;
  node_indexes.DeleteChildren;
  node_triggers.DeleteChildren;

  syntax.TableNames.Clear;
  db.GetTableNames(syntax.TableNames);
  db.GetTriggerNames(syntax.TableNames);
  db.GetIndexNames(syntax.TableNames);
  Status.Panels[2].Text := '  ' + db.DatabaseFile;
  Status.Panels[3].Text := '  driver = ' + String(db.Version) + '  (' + db.Driver + ')';
  ShowDbState;

  l := TStringList.Create;
  try
    if InfoCaption.Enabled then
    begin
      if not isview then
      begin
        db.GetForeignKeyList(l, objname);
        AddToTree(node_foreign, l, 17);
        db.GetIndexNames(l, objname);
        AddToTree(node_indexes, l, 18);
      end;
      db.GetTriggerNames(l, objname);
      AddToTree(node_triggers, l, 13);

      if table.Active then
      table.Refresh
      else
      begin
        table.TableName := objname;
        table.Open;
      end;

      if not str_empty(table.RowidField) then
      begin
        TreeDepend.Items.AddChild(node_primary, table.RowidField);
        node_primary.Expand(false);
      end;
      
      simple.Open('pragma table_info(' + CheckQuotation(objname) + ')');
      try
        dm.TableTable(simple, tabtab.Lines, syntaxtab.KeyWords);
        grid.ReadOnly := isview;
        for i := 0 to grid.Columns.Count - 1 do
        if grid.Columns[i].Width > 200 then
        grid.Columns[i].Width := 200;

      finally
        simple.Close;
      end;
      gridColEnter(grid);
    end
    else sql.Text := db.GetDBObjectBody(objtype, objname);
    TreeDepend.Selected := node_primary;
  finally
    l.Free;
  end;
end;

procedure Tfsql.AddToTree(Node: TTreeNode; list: TStrings; imageIx: Integer);
var
  i: Integer;
begin
  for i := 0 to list.Count - 1 do
  with TreeDepend.Items.AddChild(Node, list.Strings[i]) do
  ImageIndex := imageIx;
  list.Clear;
  Node.Expand(true);
end;

procedure Tfsql.TreeDependGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.StateIndex := Node.ImageIndex;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure Tfsql.TreeDependChange(Sender: TObject; Node: TTreeNode);
var
  s, n: String;
begin
  if Assigned(Node) then
  case Node.Level of
  0: Memo.Text := db.GetDBObjectBody(objtype, objname);
  1: begin
       n := Node.Text;
       if (Node.Parent = node_primary) or (Node.Parent = node_foreign) then
       begin
        s := 'table';
        n := objname;
       end
       else s := db.ObjectType[Node.Text];

       if not str_empty(s) then
       Memo.Text := db.GetDBObjectBody(s, n);
     end;
  end;
end;

procedure Tfsql.InfoCaptionClick(Sender: TObject);
begin
  if InfoCaption.Enabled then
  begin
    tabInfo.Show;
    tabDepend.Hide;
    tabSQL.Hide;
    InfoCaption.Font.Style := [fsBold];
    dependCaption.Font.Style := [fsBold, fsUnderline];
    sqlCaption.Font.Style := [fsBold, fsUnderline];
  end
  else sqlCaptionClick(Sender);
end;

procedure Tfsql.dependCaptionClick(Sender: TObject);
begin
  if dependCaption.Enabled then
  begin
    tabDepend.Show;
    tabInfo.Hide;
    tabSQL.Hide;
    InfoCaption.Font.Style := [fsBold, fsUnderline];
    dependCaption.Font.Style := [fsBold];
    sqlCaption.Font.Style := [fsBold, fsUnderline];
  end
  else sqlCaptionClick(Sender);
end;

procedure Tfsql.sqlCaptionClick(Sender: TObject);
begin
  tabSQL.Show;
  tabInfo.Hide;
  tabDepend.Hide;
  InfoCaption.Font.Style := [fsBold, fsUnderline];
  dependCaption.Font.Style := [fsBold, fsUnderline];
  sqlCaption.Font.Style := [fsBold];
end;

procedure Tfsql.execFetchRecord(Sender: TObject; ColCount: Integer; Values, Names: array of String; var Cancel: Boolean);
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 0 to ColCount - 1 do
  s := s + str_fixlen(Names[i], 24) + ' | ';
  if CompareStr(s, curline) <> 0 then
  begin
    if length(curline) > 0 then
    res.Lines.Add('');
    res.Lines.Add(s);
    res.Lines.Add(str_fill('-', length(s)));
  end;
  curline := s;

  s := '';
  for i := 0 to ColCount - 1 do
  s := s + str_fixlen(Values[i], 24) + ' | ';
  res.Lines.Add(s);

  Inc(outcount, 1);
  Cancel := outcount >= 512;
  if Cancel then
  res.Lines.Add('Max number of rows is exceeded (512).');
end;

procedure Tfsql.aExecExecute(Sender: TObject);
begin
  query.SQL.Text := sql.Text;
  query.Prepare;
  if query.Params.Count > 0 then
  ExecAsQuery
  else ExecAsExec;
end;

procedure Tfsql.ExecAsExec;
begin
  curline := '';
  outcount := 0;
  exec.CommandText.Text := sql.Text;
  try
    if res.Lines.Count > 0 then
    res.Lines.Add('');
    exec.Execute;
    res.Lines.Add('OK');
    res.GotoLineAndCenter(res.Lines.Count);
    ShowDbState;
    //if aRefresh.Enabled then
    //aRefresh.Execute;
  except
    on e: exception do
    res.Lines.Add(e.Message);
  end;
end;

procedure Tfsql.ExecAsQuery;
var
  values, names: array of String;
  i, colcount: Integer;
  cancel: Boolean;
begin
  curline := '';
  outcount := 0;
  cancel := false;
  if query.Active then
  query.Close;
  if fill_params(query) then
  query.ExecSQL;
  if query.Active then
  try
    colcount := query.FieldCount;
    SetLength(values, colcount);
    SetLength(names, colcount);
    for i := 0 to colcount - 1 do
    names[i] := query.Fields[i].FieldName;
    query.First;
    while not query.Eof and not cancel do
    begin
      for i := 0 to colcount - 1 do
      values[i] := query.Fields[i].AsString;
      execFetchRecord(query, colcount, values, names, cancel);
      query.Next;
    end;
  finally
    SetLength(values, 0);
    SetLength(names, 0);
  end;
end;

procedure Tfsql.aSaveSqlExecute(Sender: TObject);
begin
  SaveD.Filter := 'SQL commands (*.sql)|*.sql|Text files (*.txt)|*.txt|Any files (*.*)|*.*';
  if SaveD.Execute then
  sql.Lines.SaveToFile(SaveD.FileName);
end;

procedure Tfsql.aLoadSqlExecute(Sender: TObject);
begin
  OpenD.Filter := 'SQL commands (*.sql)|*.sql|Text files (*.txt)|*.txt|Any files (*.*)|*.*';
  if OpenD.Execute then
  sql.Lines.LoadFromFile(OpenD.FileName);
end;

procedure Tfsql.aClearExecute(Sender: TObject);
begin
  res.Clear;
end;

procedure Tfsql.aCreateInsertExecute(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  if not table.Active then Exit;
  s := 'INSERT INTO ' + CheckQuotation(table.TableName) + ' ('#13#10;
  for i := 0 to table.Fields.Count - 1 do
  s := s + CheckQuotation(table.Fields[i].FieldName) + ',';
  s[Length(s)] := ')';
  s := s + ' VALUES('#13#10;
  for i := 0 to table.Fields.Count - 1 do
  s := s + GetParamName(table.Fields[i].FieldName) + ',';
  s[Length(s)] := ')';
  sql.Text := s;
end;

procedure Tfsql.DropDownSpeed(sp: TSpeedButton);
var
  r: TRect;
begin
  if Assigned(sp) then
  if Assigned(sp.Parent) and Assigned(sp.PopupMenu) then
  begin
    GetWindowRect(sp.Parent.Handle, r);
    sp.PopupMenu.Popup(r.Left + sp.Left, r.Bottom);
  end;
end;

procedure Tfsql.DropDownClick(Sender: TObject);
begin
  DropDownSpeed(TSpeedButton(Sender));
end;

procedure Tfsql.aAddRecExecute(Sender: TObject);
begin
  if table.Active then
  begin
    grid.SetFocus;
    table.Append;
  end;
end;

procedure Tfsql.aDelRecExecute(Sender: TObject);
begin
  if table.Active then
  if MessageBox(Handle, 'Delete selected record?', 'Confirm', MB_YESNO or MB_ICONWARNING) = IDYES then
  table.Delete;
end;

procedure Tfsql.SQLCreateExecute(Sender: TObject);
var
  s, o: String;
begin
  o := objname;
  if str_empty(o) or not InfoCaption.Enabled then
  o := '/*table/view name*/';
  s := StringReplace(TAction(Sender).Hint, '%TABLE%', o, [rfReplaceAll]);
  s := StringReplace(s, '%VIEW%', o, [rfReplaceAll]);
  sql.Clear;
  sql.Lines.Add(s);
  case TAction(Sender).Tag of
  8:  begin
        sql.Lines.Add('/* FOR EACH ROW WHEN (expression ...) */');
        sql.Lines.Add('BEGIN');
        sql.Lines.Add('  /* SQL commands...,  NEW. references..., OLD. references... */');
        sql.Lines.Add('  /* ... or fire exception */');
        sql.Lines.Add('  /* SELECT RAISE(ABORT, "any message ...") */');
        sql.Lines.Add('END;');
      end;
  16: begin
        sql.Lines.Add('  field_name field_type --NOT NULL CHECK (expression),');
        sql.Lines.Add('  ...');
        sql.Lines.Add('--PRIMARY KEY (field_name),');
        sql.Lines.Add('--UNIQUE (field_name),');
        sql.Lines.Add('--CHECK (expression)');
        sql.Lines.Add('--FOREIGN KEY(field_name(s)) REFERENCES table(field(s))');
        sql.Lines.Add(');');
      end;
  32: begin
        sql.Lines.Add('AS');
        sql.Lines.Add('SELECT /*field list */ FROM ...');
      end;
  end;    
  sqlCaptionClick(sqlCaption);
end;

procedure Tfsql.aOpenBlobExecute(Sender: TObject);
begin
  if Assigned(ds.DataSet) then
  if ds.DataSet.Active then
  if Assigned(grid.SelectedField) then
  if grid.SelectedField.IsBlob then
  if blob_window(TBlobField(grid.SelectedField)) then;

end;

procedure Tfsql.gridDblClick(Sender: TObject);
begin
  if aOpenBlob.Enabled then
  aOpenBlob.Execute;
end;

procedure Tfsql.gridColEnter(Sender: TObject);
var
  b: Boolean;
begin
  b := false;
  if Assigned(grid.SelectedField) then
  if Assigned(grid.SelectedField.DataSet) then
  if grid.SelectedField.DataSet.Active then
  begin
    dbm.DataField := grid.SelectedField.FieldName;
    if not grid.SelectedField.DataSet.IsEmpty then
    b := grid.SelectedField.IsBlob;
  end;
  aOpenBlob.Enabled := b;
end;

procedure Tfsql.aRefreshExecute(Sender: TObject);
begin
  RefreshSqlWindow;
  SendMessage(Application.MainForm.Handle, WM_USER + 128, 0, Integer(Node));
end;

procedure Tfsql.aModifyExecute(Sender: TObject);
begin
  sql.Text := Memo.Text;
  sql.Lines.Insert(0, '--DROP ...;');
  sql.Lines.Insert(1, '');
  sqlCaptionClick(sqlCaption);
end;

procedure Tfsql.aPragmaExecute(Sender: TObject);
begin
  sql.Text := dm.GetPragma;
  aExec.Execute;
end;

procedure Tfsql.SetPragmaClick(Sender: TObject);
begin
  sql.Text := TMenuItem(Sender).Hint;
end;

procedure Tfsql.aSystemExecute(Sender: TObject);
begin
  sql.Text := 'select * from ' + SYSTEM_TABLE + ' order by type, name';
  aExec.Execute;
end;

procedure Tfsql.aSelectSqlExecute(Sender: TObject);
begin
  sql.Text := 'select * from ' + CheckQuotation(objname) + '; -- where ... order by ...';
  aExec.Execute;
end;

procedure Tfsql.ShowDbState;
begin
  if db.AutoCommitMode then
  Status.Panels[0].Text := 'AutoCommit'
  else Status.Panels[0].Text := '';
  if db.InTransaction then
  Status.Panels[1].Text := 'Transaction'
  else Status.Panels[1].Text := '';
end;

procedure Tfsql.gridTitleClick(Column: TColumn);
begin
  if Assigned(Column) and Assigned(grid.DataSource.DataSet) then
  TSivak3Table(grid.DataSource.DataSet).IndexFieldNames := Column.FieldName;
end;

procedure Tfsql.aFilterExecute(Sender: TObject);
begin
  create_filter(table);
  dropfilter.Enabled := table.Filtered;
  if table.Filtered then
  Status.Panels[3].Text := 'Filter: ' + table.Filter
  else Status.Panels[3].Text := '  driver = ' + String(db.Version) + '  (' + db.Driver + ')';
end;

procedure Tfsql.dropfilterClick(Sender: TObject);
begin
  table.Filtered := false;
  dropfilter.Enabled := false;
end;

procedure Tfsql.aLocateExecute(Sender: TObject);
begin
  create_locate(table);
end;

end.
