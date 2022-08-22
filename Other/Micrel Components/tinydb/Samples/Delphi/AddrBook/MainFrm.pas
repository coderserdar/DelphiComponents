
{**********************************************************}
{                                                          }
{  AddrBook 1.0                                            }
{  ---- Sample for Using TinyDB                            }
{                                                          }
{  Author: DayDream Studio                                 }
{  Email: webmaster@tinydb.com                             }
{  URL: http://www.TinyDB.com                              }
{                                                          }
{**********************************************************}

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ActnList, ComCtrls,
  StdCtrls, ExtCtrls, ToolWin, ImgList, TinyDB, Db,
  CardFrm;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    NewCardAction: TAction;
    FileMenu: TMenuItem;
    FileNewMemberItem: TMenuItem;
    CoolBar1: TCoolBar;
    ToolBar: TToolBar;
    ListView: TListView;
    StatusBar: TStatusBar;
    NewMemberToolButton: TToolButton;
    Panel1: TPanel;
    Panel2: TPanel;
    ImageList: TImageList;
    PropertyToolButton: TToolButton;
    DeleteCardToolButton: TToolButton;
    PropertyAction: TAction;
    DeleteCardAction: TAction;
    FileN1Item: TMenuItem;
    FileN3Item: TMenuItem;
    FilePropertyItem: TMenuItem;
    FileDeleteCardItem: TMenuItem;
    FileExitItem: TMenuItem;
    ViewMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ViewToolBarItem: TMenuItem;
    ViewStatusBarItem: TMenuItem;
    ViewN1Item: TMenuItem;
    ViewLargeIconItem: TMenuItem;
    ViewSmallIconItem: TMenuItem;
    ViewListItem: TMenuItem;
    ViewReportItem: TMenuItem;
    ViewN2Item: TMenuItem;
    ViewRefreshItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    Splitter1: TSplitter;
    TreeView: TTreeView;
    NewFolderAction: TAction;
    FileNewFolderItem: TMenuItem;
    ToolButton1: TToolButton;
    FolderTinyTable: TTinyTable;
    TinyDatabase: TTinyDatabase;
    CardTinyTable: TTinyTable;
    TreeViewPopupMenu: TPopupMenu;
    TreePopNewFolderItem: TMenuItem;
    TreePopNewCardItem: TMenuItem;
    TreePopN1Item: TMenuItem;
    TreePopDeleteFolderItem: TMenuItem;
    ListViewPopupMenu: TPopupMenu;
    ListPopNewCardItem: TMenuItem;
    ListPopDeleteCardItem: TMenuItem;
    ListPopPropertyItem: TMenuItem;
    Panel3: TPanel;
    Label1: TLabel;
    NameSearchPanel: TPanel;
    NameSearchEdit: TEdit;
    EncryptAction: TAction;
    FileEncryptItem: TMenuItem;
    FileN2Item: TMenuItem;
    EncryptToolButton: TToolButton;
    procedure NewCardActionExecute(Sender: TObject);
    procedure PropertyActionExecute(Sender: TObject);
    procedure DeleteCardActionExecute(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure ViewToolBarItemClick(Sender: TObject);
    procedure ViewStatusBarItemClick(Sender: TObject);
    procedure ViewLargeIconItemClick(Sender: TObject);
    procedure ViewRefreshItemClick(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure NewFolderActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure TreePopDeleteFolderItemClick(Sender: TObject);
    procedure TreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeViewPopupMenuPopup(Sender: TObject);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListViewPopupMenuPopup(Sender: TObject);
    procedure TreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure NameSearchPanelResize(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure NameSearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EncryptActionExecute(Sender: TObject);
  private
    { Private declarations }
    FPassword: string;

    function GetDBFileName: string;
    function CreateDatabase: Boolean;
    function OpenDatabase: Boolean;
    procedure FillFolderTreeView;
    procedure FillListView(FolderID: Integer);
    procedure UpdateListItem(ListItem: TListItem; CardData: TCardFormData);
    procedure LoadCardData(var CardData: TCardFormData);
    procedure SaveCardData(CardData: TCardFormData);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses StrRes, InputFrm, ChgPwdFrm, AboutFrm;

{$R *.DFM}

procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

function TMainForm.GetDBFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'AddrData.dat';
end;

function TMainForm.CreateDatabase: Boolean;
var
  DBFileName: string;
begin
  Result := True;
  DBFileName := GetDBFileName;

  try
    TinyDatabase.CreateDatabase(DBFileName, True, clNormal, 'ZLIB', False, 'MyEncAlgo', '');
    TinyDatabase.DatabaseName := DBFileName;

    TinyDatabase.CreateTable('Card', [
      FieldItem('ID', ftAutoInc),
      FieldItem('FolderID', ftInteger),
      FieldItem('Name', ftString, 32),
      FieldItem('Nickname', ftString, 32),
      FieldItem('Emails', ftMemo),
      FieldItem('HandTel', ftString, 20),
      FieldItem('BP', ftString, 20),
      FieldItem('Oicq', ftString, 20),
      FieldItem('Icq', ftString, 20),
      FieldItem('Sex', ftInteger),
      FieldItem('Birthday', ftDate),
      FieldItem('UseBirth', ftBoolean),
      FieldItem('Constellation', ftInteger),
      FieldItem('BloodType', ftInteger),
      FieldItem('Taste', ftString, 127),
      FieldItem('Homepage', ftString, 127),
      FieldItem('HomeZip', ftString, 6),
      FieldItem('HomeAddr', ftString, 255),
      FieldItem('HomeTel1', ftString, 32),
      FieldItem('HomeTel2', ftString, 32),
      FieldItem('HomeFax', ftString, 32),
      FieldItem('CorpZip', ftString, 6),
      FieldItem('CorpAddr', ftString, 255),
      FieldItem('CorpJob', ftString, 32),
      FieldItem('CorpDept', ftString, 32),
      FieldItem('CorpTel1', ftString, 32),
      FieldItem('CorpTel2', ftString, 32),
      FieldItem('CorpFax', ftString, 32),
      FieldItem('CorpHomepage', ftString, 255),
      FieldItem('ExtMemo', ftMemo)
      ] );

    TinyDatabase.CreateTable('Folder', [
      FieldItem('ID', ftAutoInc),
      FieldItem('ParentID', ftInteger),
      FieldItem('Name', ftString, 32)
      ] );

    TinyDatabase.CreateIndex('Card', 'ByFolderID', [], ['FolderID']);
    TinyDatabase.CreateIndex('Card', 'ByName', [], ['Name']);
    TinyDatabase.CreateIndex('Folder', 'ByParentID', [], ['ParentID']);

    FolderTinyTable.DatabaseName := DBFileName;
    FolderTinyTable.TableName := 'Folder';
    FolderTinyTable.Open;
    FolderTinyTable.AppendRecord([0, 0, '<' + SDefaultStr + '>']);
    FolderTinyTable.Close;
  except
    DeleteFile(DBFileName);
    Result := False;
  end;
end;

function TMainForm.OpenDatabase: Boolean;
var
  DBFileName: string;
  Password: string;
  Count: Integer;
begin
  TinyDatabase.Close;
  DBFileName := GetDBFileName;
  TinyDatabase.DatabaseName := DBFileName;
  TinyDatabase.Open;
  Count := 1;
  while not TinyDatabase.CanAccess and (Count <= 3) do
  begin
    if ShowInputForm(Password, Self.Caption, SInputPassword, True) then
    begin
      TinyDatabase.Password := Password;
      if not TinyDatabase.CanAccess then
      begin
        MessageBox(Application.Handle, PChar(SPasswordWrong), PChar(Application.Title), 48);
        Inc(Count);
      end;
    end else
    begin
      TinyDatabase.DatabaseName := '';
      Break;
    end;
  end;
  if (TinyDatabase.DatabaseName <> '') and (TinyDatabase.CanAccess) then
  begin
    FPassword := Password;
    FolderTinyTable.DatabaseName := DBFileName;
    FolderTinyTable.TableName := 'Folder';
    FolderTinyTable.Open;
    CardTinyTable.DatabaseName := DBFileName;
    CardTinyTable.TableName := 'Card';
    CardTinyTable.Open;
    Result := True;
  end else
    Result := False;
end;

procedure TMainForm.FillFolderTreeView;

  procedure FillTreeNode(TreeNode: TTreeNode);
  var
    I, FolderID: Integer;
    Node: TTreeNode;
  begin
    FolderID := Integer(TreeNode.Data);
    FolderTinyTable.Filter := 'ParentID=' + IntToStr(FolderID);
    FolderTinyTable.Filtered := True;
    FolderTinyTable.First;
    for I := 0 to FolderTinyTable.RecordCount - 1 do
    begin
      Node := TreeView.Items.AddChild(TreeNode, FolderTinyTable.FieldByName('Name').AsString);
      Node.Data := Pointer(FolderTinyTable.FieldByName('ID').AsInteger);
      Node.ImageIndex := 1;
      Node.SelectedIndex := 1;
      FolderTinyTable.Next;
    end;
    for I := 0 to TreeNode.Count - 1 do
    begin
      FillTreeNode(TreeNode.Item[I]);
    end;
  end;

begin
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  with TreeView.Items.Add(nil, SAddressBook) do
  begin
    Data := Pointer(0);
    ImageIndex := 0;
    SelectedIndex := 0;
  end;
  FillTreeNode(TreeView.Items[0]);
  TreeView.FullExpand;
  TreeView.Items.EndUpdate;
  TreeView.Items[0].Selected := True;
end;

procedure TMainForm.FillListView(FolderID: Integer);
var
  I: Integer;
  ListItem: TListItem;
  List: TStrings;
  Email: string;
begin
  List := TStringList.Create;
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  try
    CardTinyTable.Filter := 'FolderID=' + IntToStr(FolderID);
    CardTinyTable.Filtered := True;

    CardTinyTable.First;
    for I := 0 to CardTinyTable.RecordCount - 1 do
    begin
      ListItem := ListView.Items.Add;
      ListItem.Data := Pointer(CardTinyTable.FieldByName('ID').AsInteger);
      ListItem.Caption := CardTinyTable.FieldByName('Name').AsString;
      List.Clear;
      List.CommaText := CardTinyTable.FieldByName('Emails').AsString;
      if List.Count > 0 then Email := List[0]
      else Email := '';
      ListItem.SubItems.Add(Email);
      ListItem.SubItems.Add(CardTinyTable.FieldByName('HandTel').AsString);
      ListItem.ImageIndex := 2;
      CardTinyTable.Next;
    end;
  finally
    ListView.Items.EndUpdate;
    List.Free;
  end;
  StatusBar.SimpleText := IntToStr(ListView.Items.Count) + ' Items';
end;

procedure TMainForm.UpdateListItem(ListItem: TListItem; CardData: TCardFormData);
var
  List: TStrings;
  Email: string;
begin
  List := TStringList.Create;
  try
    ListItem.Caption := CardData.Name;
    List.CommaText := CardData.Emails;
    if List.Count > 0 then Email := List[0]
    else Email := '';
    ListItem.SubItems[0] := Email;
    ListItem.SubItems[1] := CardData.HandTel;
  finally
    List.Free;
  end;
end;

procedure TMainForm.LoadCardData(var CardData: TCardFormData);
begin
  with CardData do
  begin
    Name := CardTinyTable.FieldByName('Name').AsString;
    Nickname := CardTinyTable.FieldByName('Nickname').AsString;
    Emails := CardTinyTable.FieldByName('Emails').AsString;
    HandTel := CardTinyTable.FieldByName('HandTel').AsString;
    BP := CardTinyTable.FieldByName('BP').AsString;
    Oicq := CardTinyTable.FieldByName('Oicq').AsString;
    Icq := CardTinyTable.FieldByName('Icq').AsString;
    Sex := CardTinyTable.FieldByName('Sex').AsInteger;
    Birthday := CardTinyTable.FieldByName('Birthday').AsDateTime;
    UseBirth := CardTinyTable.FieldByName('UseBirth').AsBoolean;
    Constellation := CardTinyTable.FieldByName('Constellation').AsInteger;
    BloodType := CardTinyTable.FieldByName('BloodType').AsInteger;
    Taste := CardTinyTable.FieldByName('Taste').AsString;
    Homepage := CardTinyTable.FieldByName('Homepage').AsString;
    HomeZip := CardTinyTable.FieldByName('HomeZip').AsString;
    HomeAddr := CardTinyTable.FieldByName('HomeAddr').AsString;
    HomeTel1 := CardTinyTable.FieldByName('HomeTel1').AsString;
    HomeTel2 := CardTinyTable.FieldByName('HomeTel2').AsString;
    HomeFax := CardTinyTable.FieldByName('HomeFax').AsString;
    CorpZip := CardTinyTable.FieldByName('CorpZip').AsString;
    CorpAddr := CardTinyTable.FieldByName('CorpAddr').AsString;
    CorpJob := CardTinyTable.FieldByName('CorpJob').AsString;
    CorpDept := CardTinyTable.FieldByName('CorpDept').AsString;
    CorpTel1 := CardTinyTable.FieldByName('CorpTel1').AsString;
    CorpTel2 := CardTinyTable.FieldByName('CorpTel2').AsString;
    CorpFax := CardTinyTable.FieldByName('CorpFax').AsString;
    CorpHomepage := CardTinyTable.FieldByName('CorpHomepage').AsString;
    ExtMemo := CardTinyTable.FieldByName('ExtMemo').AsString;
  end;
end;

procedure TMainForm.SaveCardData(CardData: TCardFormData);
begin
  with CardData do
  begin
    CardTinyTable.FieldByName('Name').AsString := Name;
    CardTinyTable.FieldByName('Nickname').AsString := Nickname;
    CardTinyTable.FieldByName('Emails').AsString := Emails;
    CardTinyTable.FieldByName('HandTel').AsString := HandTel;
    CardTinyTable.FieldByName('BP').AsString := BP;
    CardTinyTable.FieldByName('Oicq').AsString := Oicq;
    CardTinyTable.FieldByName('Icq').AsString := Icq;
    CardTinyTable.FieldByName('Sex').AsInteger := Sex;
    CardTinyTable.FieldByName('Birthday').AsDateTime := Birthday;
    CardTinyTable.FieldByName('UseBirth').AsBoolean := UseBirth;
    CardTinyTable.FieldByName('Constellation').AsInteger := Constellation;
    CardTinyTable.FieldByName('BloodType').AsInteger := BloodType;
    CardTinyTable.FieldByName('Taste').AsString := Taste;
    CardTinyTable.FieldByName('Homepage').AsString := Homepage;
    CardTinyTable.FieldByName('HomeZip').AsString := HomeZip;
    CardTinyTable.FieldByName('HomeAddr').AsString := HomeAddr;
    CardTinyTable.FieldByName('HomeTel1').AsString := HomeTel1;
    CardTinyTable.FieldByName('HomeTel2').AsString := HomeTel2;
    CardTinyTable.FieldByName('HomeFax').AsString := HomeFax;
    CardTinyTable.FieldByName('CorpZip').AsString := CorpZip;
    CardTinyTable.FieldByName('CorpAddr').AsString := CorpAddr;
    CardTinyTable.FieldByName('CorpJob').AsString := CorpJob;
    CardTinyTable.FieldByName('CorpDept').AsString := CorpDept;
    CardTinyTable.FieldByName('CorpTel1').AsString := CorpTel1;
    CardTinyTable.FieldByName('CorpTel2').AsString := CorpTel2;
    CardTinyTable.FieldByName('CorpFax').AsString := CorpFax;
    CardTinyTable.FieldByName('CorpHomepage').AsString := CorpHomepage;
    CardTinyTable.FieldByName('ExtMemo').AsString := ExtMemo;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  DBFileName: string;
begin
  DBFileName := GetDBFileName;
  if not FileExists(DBFileName) then
  begin
    if not CreateDatabase then
    begin
      MessageBox(Application.Handle, PChar(SFailToCreateDb), PChar(Application.Title), 16);
      Application.Terminate;
    end;
  end;

  if not OpenDatabase then
  begin
    Application.Terminate;
  end;

  FillFolderTreeView;
  NameSearchEdit.Text := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  CardTinyTable.Close;
  FolderTinyTable.Close;
end;

procedure TMainForm.NewCardActionExecute(Sender: TObject);
var
  CardData: TCardFormData;
  FolderID: Integer;
begin
  if TreeView.Selected = nil then
    TreeView.Items[1].Selected := True;
  if TreeView.Selected.AbsoluteIndex = 0 then
    TreeView.Items[1].Selected := True;

  FolderID := Integer(TreeView.Selected.Data);
  if FolderID < 1 then FolderID := 1;

  CardData.UseBirth := False;
  if ShowCardForm(CardData) then
  begin
    CardTinyTable.Filtered := False;
    CardTinyTable.Append;
    CardTinyTable.FieldByName('FolderID').AsInteger := FolderID;
    SaveCardData(CardData);
    CardTinyTable.Post;

    FillListView(FolderID);
  end;
end;

procedure TMainForm.NewFolderActionExecute(Sender: TObject);
var
  ParentID: Integer;
  FolderName: string;
  TreeNode: TTreeNode;
begin
  if TreeView.Selected = nil then Exit;
  ParentID := Integer(TreeView.Selected.Data);

  if ShowInputForm(FolderName, SNewFolder, SFolderName) then
  begin
    FolderTinyTable.Filtered := False;
    FolderTinyTable.Append;
    FolderTinyTable.FieldByName('ParentID').AsInteger := ParentID;
    FolderTinyTable.FieldByName('Name').AsString := FolderName;
    FolderTinyTable.Post;

    TreeNode := TreeView.Items.AddChild(TreeView.Selected, FolderName);
    TreeNode.Data := Pointer(FolderTinyTable.FieldByName('ID').AsInteger);
    TreeNode.ImageIndex := 1;
    TreeNode.SelectedIndex := 1;
    TreeNode.MakeVisible;
    TreeNode.Selected := True;
  end;
end;

procedure TMainForm.PropertyActionExecute(Sender: TObject);
var
  ListItem: TListItem;
  CardID: Integer;
  CardData: TCardFormData;
begin
  if ListView.Selected = nil then Exit;
  ListItem := ListView.Selected;

  CardID := Integer(ListItem.Data);
  CardTinyTable.Filtered := False;
  CardTinyTable.IndexName := '';
  if CardTinyTable.FindKey([CardID]) then
  begin
    LoadCardData(CardData);
    if ShowCardForm(CardData) then
    begin
      CardTinyTable.Edit;
      SaveCardData(CardData);
      CardTinyTable.Post;
      UpdateListItem(ListItem, CardData);
    end;
  end;
end;

procedure TMainForm.DeleteCardActionExecute(Sender: TObject);
var
  CardID: Integer;
  R: Integer;
begin
  if ListView.Selected = nil then Exit;
  R := MessageBox(Application.Handle, PChar(SQueryDeleteCard), PChar(Application.Title), 36);
  if R = ID_NO then Exit;

  CardID := Integer(ListView.Selected.Data);
  CardTinyTable.Filtered := False;
  CardTinyTable.IndexName := '';
  if CardTinyTable.FindKey([CardID]) then
  begin
    CardTinyTable.Delete;
    ListView.Selected.Delete;
  end;
end;

procedure TMainForm.EncryptActionExecute(Sender: TObject);
var
  Data: TChgPwdFormData;
begin
  Data.CheckPwd := TinyDatabase.Encrypted;
  if ShowChgPwdForm(Data) then
  begin
    if TinyDatabase.ChangePassword(Data.Password, Data.CheckPwd) then
    begin
      MessageBox(Handle, PChar(SChgPwdSucc), PChar(Application.Title), 48)
    end else
      MessageBox(Handle, PChar(SChgPwdFail), PChar(Application.Title), 48);
  end;
end;

procedure TMainForm.FileExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ViewToolBarItemClick(Sender: TObject);
begin
  ViewToolBarItem.Checked := not ViewToolBarItem.Checked;
  ToolBar.Visible := ViewToolBarItem.Checked;
end;

procedure TMainForm.ViewStatusBarItemClick(Sender: TObject);
begin
  ViewStatusBarItem.Checked := not ViewStatusBarItem.Checked;
  StatusBar.Visible := ViewStatusBarItem.Checked;
end;

procedure TMainForm.ViewLargeIconItemClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  ListView.ViewStyle := TViewStyle((Sender as TMenuItem).Tag);
end;

procedure TMainForm.ViewRefreshItemClick(Sender: TObject);
begin
  if TreeView.Selected = nil then Exit;
  FillListView(Integer(TreeView.Selected.Data));
end;

procedure TMainForm.HelpAboutItemClick(Sender: TObject);
begin
  ShowAboutForm;
end;

{$WRITEABLECONST ON}

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
const
  OldFolderID: Integer = -1;
var
  FolderID: Integer;
begin
  FolderID := Integer(Node.Data);
  if FolderID = OldFolderID then Exit;
  OldFolderID := FolderID;
  FillListView(FolderID);
end;

procedure TMainForm.TreePopDeleteFolderItemClick(Sender: TObject);
var
  TreeNode: TTreeNode;
  FolderID, R: Integer;
begin
  if TreeView.Selected = nil then Exit;
  TreeNode := TreeView.Selected;
  if TreeNode.AbsoluteIndex in [0,1] then Exit;
  R := MessageBox(Application.Handle, PChar(SQueryDeleteFolder), PChar(Application.Title), 36);
  if R = ID_NO then Exit;

  FolderID := Integer(TreeNode.Data);
  FolderTinyTable.Filtered := False;
  FolderTinyTable.IndexName := '';
  if FolderTinyTable.FindKey([FolderID]) then
  begin
    FolderTinyTable.Delete;

    CardTinyTable.Filtered := False;
    CardTinyTable.IndexName := 'ByFolderID';
    while CardTinyTable.FindKey([FolderID]) do
    begin
      CardTinyTable.Delete;
    end;

    TreeNode.Delete;
  end;
end;

procedure TMainForm.TreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  if Node.AbsoluteIndex in [0, 1] then AllowEdit := False
  else AllowEdit := True;
end;

procedure TMainForm.TreeViewEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
var
  FolderID: Integer;
begin
  FolderID := Integer(Node.Data);
  FolderTinyTable.Filtered := False;
  FolderTinyTable.IndexName := '';
  if FolderTinyTable.FindKey([FolderID]) then
  begin
    FolderTinyTable.Edit;
    FolderTinyTable.FieldByName('Name').AsString := S;
    FolderTinyTable.Post;

    Node.Text := FolderTinyTable.FieldByName('Name').AsString;
  end;
end;

procedure TMainForm.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  HitTest: THitTests;
begin
  HitTest := TreeView.GetHitTestInfoAt(X, Y);
  if (htOnIcon in HitTest) or (htOnLabel in HitTest)then
  begin
    Node := TreeView.GetNodeAt(X, Y);
    if Node <> nil then Node.Selected := True;
  end;
end;

procedure TMainForm.TreeViewPopupMenuPopup(Sender: TObject);
begin
  TreePopDeleteFolderItem.Enabled := not (TreeView.Selected.AbsoluteIndex in [0,1]);
end;

procedure TMainForm.ListViewPopupMenuPopup(Sender: TObject);
var
  Selected: Boolean;
begin
  Selected := (ListView.Selected <> nil);

  ListPopDeleteCardItem.Enabled := Selected;
  ListPopPropertyItem.Enabled := Selected;
end;

procedure TMainForm.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  NameSearchEdit.Text := Item.Caption;
end;

procedure TMainForm.NameSearchPanelResize(Sender: TObject);
begin
  NameSearchEdit.Width := NameSearchPanel.ClientWidth;
end;

procedure TMainForm.NameSearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
begin
  if Key = VK_RETURN then
  begin
    KillMessage(Handle, WM_CHAR);
    NameSearchEdit.SelectAll;
    for I := 0 to ListView.Items.Count - 1 do
    begin
      if CompareText(ListView.Items[I].Caption, NameSearchEdit.Text) = 0 then
      begin
        ListView.Items[I].MakeVisible(False);
        ListView.Items[I].Selected := True;
        NameSearchEdit.SelectAll;
        Exit;
      end;
    end;
    MessageBeep(0);
  end;
end;

end.
