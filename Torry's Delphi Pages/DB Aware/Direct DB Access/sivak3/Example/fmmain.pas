unit fmmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, sql3_defs, ActnList, Menus, ExtCtrls, ComCtrls, ToolWin;

type
  Tfmain = class(TForm)
    TreeObj: TTreeView;
    botp: TPanel;
    ppu: TPanel;
    ppb: TPanel;
    al: TActionList;
    aConnect: TAction;
    Popup: TPopupMenu;
    aConnect1: TMenuItem;
    aRegEdit: TAction;
    aRegNew: TAction;
    N1: TMenuItem;
    Editdatabaseregistration1: TMenuItem;
    Newdatabaseregistration1: TMenuItem;
    aDisconnect: TAction;
    Disconnect1: TMenuItem;
    aRegDel: TAction;
    Deleteregistration1: TMenuItem;
    aSQL: TAction;
    N2: TMenuItem;
    SQL1: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Connecttodatabase1: TMenuItem;
    Disconnect2: TMenuItem;
    N3: TMenuItem;
    Newdatabaseregistration2: TMenuItem;
    Editdatabaseregistration2: TMenuItem;
    Deleteregistration2: TMenuItem;
    N4: TMenuItem;
    SQL2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    aRefresh: TAction;
    Refresh1: TMenuItem;
    Refresh2: TMenuItem;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure FetchList(ParentNode: TTreeNode; list: TStringList; db: TSivak3Database; imageIx: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TreeObjChange(Sender: TObject; Node: TTreeNode);
    procedure aConnectExecute(Sender: TObject);
    procedure aRegNewExecute(Sender: TObject);
    procedure aDisconnectExecute(Sender: TObject);
    procedure aRegEditExecut(Sender: TObject);
    procedure aRegDelExecute(Sender: TObject);
    procedure TreeObjGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure aSQLExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeObjDblClick(Sender: TObject);
    procedure TreeObjClick(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure TreeObjCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
  private
    { Private declarations }
    procedure set_state;
    procedure GetDBList;
    procedure RefreshMainNode;
    procedure SaveRegistration(db: TSivak3Database; title: String);
    procedure OpenDatabase(ParentNode: TTreeNode);
    procedure GetTableProps(ParentNode: TTreeNode);
    procedure RefreshMessage(var M: TMessage); message WM_USER + 128;
  public
    { Public declarations }
  end;

var
  fmain: Tfmain;

implementation

uses
  IniFiles, udm, fmconnect, fmsql, fmabout;

{$R *.dfm}

procedure Tfmain.FormCreate(Sender: TObject);
var
  f: TIniFile;
begin
  GetDBList;
  set_state;
  al.Images := dm.ImagesBtn;
  ToolBar.Images := dm.ImagesBtn;
  TreeObj.Images := dm.ImagesDB;
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    Left := f.ReadInteger('MAINWIN', 'left', 8);
    Top := f.ReadInteger('MAINWIN', 'top', 8);
    Width := f.ReadInteger('MAINWIN', 'width', Width);
    Height := f.ReadInteger('MAINWIN', 'heigth', Screen.Height - 88);
  finally
    f.Free;
  end;
end;

procedure Tfmain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  f: TIniFile;
begin
  f := TIniFile.Create(get_work_folder_file('sql3man.ini'));
  try
    f.WriteInteger('MAINWIN', 'left', Left);
    f.WriteInteger('MAINWIN', 'top', Top);
    f.WriteInteger('MAINWIN', 'width', Width);
    f.WriteInteger('MAINWIN', 'heigth', Height);
  finally
    f.Free;
  end;
end;

procedure Tfmain.set_state;
var
  n: TTreeNode;
  d: TSivak3Database;
begin
  n := TreeObj.Selected;
  if Assigned(n) then
  begin
    d := TSivak3Database(n.Data);
    aConnect.Enabled := (n.Level = 0) and not d.Connected;
    aDisconnect.Enabled := (n.Level = 0) and d.Connected;
    aRegEdit.Enabled := aConnect.Enabled;
    aRegDel.Enabled := aConnect.Enabled;
    aSQL.Enabled := d.Connected;
    aRefresh.Enabled := d.Connected; //aDisconnect.Enabled;

    d := TSivak3Database(n.Data);
    if d.Connected then
    botp.Caption := 'Connected, database version: ' + String(d.Version)
    else botp.Caption := 'Disconnected.';

  end
  else
  begin
    aConnect.Enabled := false;
    aDisconnect.Enabled := false;
    aRegEdit.Enabled := false;
    aRegDel.Enabled := false;
    aSQL.Enabled := false;
    aRefresh.Enabled := false;
  end;
end;

procedure Tfmain.GetDBList;
var
  f: TIniFile;
  l: TStringList;
  d: TSivak3Database;
  i: Integer;
  s: String;
begin

  s := get_work_folder_file('conn.ini');
  f := TIniFile.Create(s);
  l := TStringList.Create;
  try
    f.ReadSections(l);
    for i := 0 to l.Count - 1 do
    begin
      d := TSivak3Database.Create(Self);
      d.Description := l.Strings[i];
      d.DatabaseFile := f.ReadString(l.Strings[i], 'DatabaseFile', '');
      d.Driver := f.ReadString(l.Strings[i], 'Driver', 'sqlite3.dll');
      d.Tag := f.ReadInteger(l.Strings[i], 'Tag', 0);
      with TreeObj.Items.AddChildObject(nil, f.ReadString(l.Strings[i], 'Title', 'Title'), d) do
      ImageIndex := 1;
    end;
  finally
    l.Free;
    f.Free;
  end;
end;

procedure Tfmain.SaveRegistration(db: TSivak3Database; title: String);
var
  f: TIniFile;
  s: String;
begin
  s := get_work_folder_file('conn.ini');
  f := TIniFile.Create(s);
  try
    f.WriteString(db.Description, 'Title', title);
    f.WriteString(db.Description, 'DatabaseFile', db.DatabaseFile);
    f.WriteString(db.Description, 'Driver', db.Driver);
    f.WriteInteger(db.Description, 'Tag', db.Tag);
  finally
    f.Free;
  end;
end;

procedure Tfmain.FetchList(ParentNode: TTreeNode; list: TStringList; db: TSivak3Database; imageIx: Integer);
var
  i: Integer;
  r: Cardinal;
begin
  for i := 0 to list.Count - 1 do
  begin
    r := Cardinal(list.Objects[i]);
    with TreeObj.Items.AddChildObject(ParentNode, list[i], db) do
    if r and FIELD_PRIMARY_FLAG = FIELD_PRIMARY_FLAG then
    ImageIndex := 11
    else
    if r and FIELD_ALL_FLAG <> 0 then
    ImageIndex := 17
    else
    ImageIndex := imageIx;
  end;
end;

procedure Tfmain.RefreshMainNode;
var
  ParentNode: TTreeNode;
  d: TSivak3Database;
  l: TStringList;
  t: TTreeNode;
begin
  ParentNode := TreeObj.Selected;
  if not Assigned(ParentNode) then Exit;
  while Assigned(ParentNode.Parent) do
  ParentNode := ParentNode.Parent;

  d := TSivak3Database(ParentNode.Data);
  ParentNode.DeleteChildren;
  l := TStringList.Create;
  try
    t := TreeObj.Items.AddChildObject(ParentNode, 'Tables', d);
    t.ImageIndex := 2;
    l.Clear;
    d.GetTableNames(l);
    FetchList(t, l, d, 18);

    t := TreeObj.Items.AddChildObject(ParentNode, 'Views', d);
    t.ImageIndex := 9;
    l.Clear;
    d.GetViewNames(l);
    FetchList(t, l, d, 18);

    t := TreeObj.Items.AddChildObject(ParentNode, 'Triggers', d);
    t.ImageIndex := 5;
    l.Clear;
    d.GetTriggerNames(l);
    FetchList(t, l, d, 18);

    t := TreeObj.Items.AddChildObject(ParentNode, 'Indexes', d);
    t.ImageIndex := 10;
    l.Clear;
    d.GetIndexNames(l);
    FetchList(t, l, d, 18);
  finally
    l.Free;
  end;
end;

procedure Tfmain.OpenDatabase(ParentNode: TTreeNode);
begin
  dm.OpenDatabase(ParentNode);
  RefreshMainNode;
end;

procedure Tfmain.GetTableProps(ParentNode: TTreeNode);
var
  l: TStringList;
  d: TSivak3Database;
  t: TTreeNode;
begin
  l := TStringList.Create;
  try
    d := TSivak3Database(ParentNode.Data);
    t := TreeObj.Items.AddChildObject(ParentNode, 'Fields', d);
    t.ImageIndex := 4;
    d.GetFieldNames(l, ParentNode.Text);
    FetchList(t, l, d, 18);

    t := TreeObj.Items.AddChildObject(ParentNode, 'Triggers', d);
    t.ImageIndex := 5;
    l.Clear;
    d.GetTriggerNames(l, ParentNode.Text);
    FetchList(t, l, d, 18);
    if ParentNode.Parent.Text = 'Views' then Exit;

    t := TreeObj.Items.AddChildObject(ParentNode, 'Foreign keys', d);
    t.ImageIndex := 12;
    l.Clear;
    d.GetForeignKeyList(l, ParentNode.Text);
    FetchList(t, l, d, 18);

    t := TreeObj.Items.AddChildObject(ParentNode, 'Indexes', d);
    t.ImageIndex := 10;
    l.Clear;
    d.GetIndexNames(l, ParentNode.Text);
    FetchList(t, l, d, 18);
  finally
    l.Free;
  end;
end;

procedure Tfmain.TreeObjChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) then
  if not Node.HasChildren then
  case node.Level of
  2: if Node.Parent.Text = 'Tables' then
     GetTableProps(Node)
     else
     if Node.Parent.Text = 'Views' then
     GetTableProps(Node);
  end;
  set_state;
end;

procedure Tfmain.TreeObjClick(Sender: TObject);
begin
  TreeObjChange(Sender, TreeObj.Selected);
end;

procedure Tfmain.aConnectExecute(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TreeObj.Selected;
  if Assigned(n) then
  if n.Level = 0 then
  begin
    OpenDatabase(n);
    n.Expand(false);
  end;
  set_state;
end;

procedure Tfmain.aDisconnectExecute(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TreeObj.Selected;
  if Assigned(n) then
  if n.Level = 0 then
  dm.CloseDatabase(n);
  set_state;
end;

procedure Tfmain.aRegNewExecute(Sender: TObject);
var
  db: TSivak3Database;
  s: String;
begin
  db := TSivak3Database.Create(Self);
  s := 'Title';
  if ConnectionDialog(db, s) = mrOk then
  begin
    db.Description := 'DBS3-' + FormatDateTime('yymmdd-hhnn', Now) + '-' + IntToStr(GetTickCount);
    SaveRegistration(db, s);
    TreeObj.Items.AddChildObject(nil, s, db);
  end
  else db.Free;
end;

procedure Tfmain.aRegEditExecut(Sender: TObject);
var
  db: TSivak3Database;
  n: TTreeNode;
  s: String;
begin
  n := TreeObj.Selected;
  if not Assigned(n) then Exit;
  s := n.Text;
  db := TSivak3Database(n.Data);
  if ConnectionDialog(db, s) = mrOk then
  begin
    SaveRegistration(db, s);
    n.Text := s;
  end;
end;

procedure Tfmain.aRegDelExecute(Sender: TObject);
var
  db: TSivak3Database;
  f: TIniFile;
  n: TTreeNode;
  s: String;
begin
  n := TreeObj.Selected;
  if not Assigned(n) then Exit;
  if MessageBox(Handle, 'Are you sure?', 'Confirm delete', MB_YESNO or MB_ICONWARNING) <> IDYES then Exit;
  s := get_work_folder_file('conn.ini');
  db := TSivak3Database(n.Data);
  f := TIniFile.Create(s);
  try
    s := db.Description;
    f.EraseSection(s);
    n.Delete;
    db.Free;
  finally
    f.Free;
  end;
end;

procedure Tfmain.TreeObjGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.StateIndex := Node.ImageIndex;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure Tfmain.aSQLExecute(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TreeObj.Selected;
  if Assigned(n) then
  SqlWindow(n);
end;

procedure Tfmain.TreeObjDblClick(Sender: TObject);
begin
  if aSQL.Enabled then
  aSQL.Execute
  else
  if aConnect.Enabled then
  aConnect.Execute;
end;

procedure Tfmain.aRefreshExecute(Sender: TObject);
begin
  if Assigned(TreeObj.Selected) then
  begin
    RefreshMainNode;
    TreeObj.Selected.Expand(false);
  end;
end;

procedure Tfmain.About1Click(Sender: TObject);
begin
  ShowAbout;
end;

procedure Tfmain.RefreshMessage(var M: TMessage);
begin
  TreeObj.Selected := TTreeNode(M.LParam);
  if aRefresh.Enabled then
  aRefresh.Execute;
end;

procedure Tfmain.TreeObjCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
var
  h: THitTests;
  p: TPoint;
begin
  p := TreeObj.ScreenToClient(Mouse.CursorPos);
  h := TreeObj.GetHitTestInfoAt(p.X, p.Y);
  AllowCollapse := not (htOnItem in h);
end;

end.
