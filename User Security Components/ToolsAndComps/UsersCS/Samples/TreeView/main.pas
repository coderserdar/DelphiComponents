unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, users_basic, users_cs, DBTables, StdCtrls, ComCtrls, Db;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    NewApplication1: TMenuItem;
    NewForm1: TMenuItem;
    NewFrame1: TMenuItem;
    MenuItem1: TMenuItem;
    Open1: TMenuItem;
    OpenProject1: TMenuItem;
    Edit1: TMenuItem;
    Clipboardfunctions1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    ViewClipboardContents1: TMenuItem;
    SearchOptions1: TMenuItem;
    Find1: TMenuItem;
    FindinFiles1: TMenuItem;
    Replace1: TMenuItem;
    Security1: TMenuItem;
    MenuItem2: TMenuItem;
    ChangePassword1: TMenuItem;
    N3: TMenuItem;
    MenuItem3: TMenuItem;
    UsersCSReg1: TUsersCSReg;
    dbUsers: TDatabase;
    UsersCS1: TUsersCS;
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure OpenProject1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure UsersCS1BeforeLogin;
  private
    { Private declarations }
    procedure MenuToTreeView;
    function Retira(Str: String; Chr: Char): String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dm;

{$R *.DFM}

procedure TForm1.ChangeUserPasswordExecute(Sender: TObject);
begin
  UsersCS1.ChangeUserPassword;
end;

procedure TForm1.UserAdministrationExecute(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.LoginExecute(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate
  else
    MenuToTreeView;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('User Name: master'+#10#13+'Password: master');
  LoginExecute(NIL);
end;

procedure TForm1.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
begin
  Node.Data:=NIL;
end;

function TForm1.Retira(Str: String; Chr: Char): String;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to Length(Str) do
    begin
      if Str[i]<>Chr then
        Result:=Result+Str[i];
    end;
end;

procedure TForm1.MenuToTreeView;
var
  i: Integer;
  Root,Node: TTreeNode;
  ComponentInfo: TComponentInfo; {defined at users_basic.pas}
procedure SubItems(MenuItem: TMenuItem; ParentNode: TTreeNode);
var
  i: Integer;
  Node: TTreeNode;
begin
  for i:=0 to MenuItem.Count-1 do
    begin
      if MenuItem.Items[i].Caption='-' then
        Continue;
      ComponentInfo:=UsersCSReg1.GetComponentInfo(MenuItem.Items[i].Name);
      if (ComponentInfo.ComponentStatus in [csNotRegistered,csEnabledVisible]) then
        begin
          Node:=TreeView1.Items.AddChild(ParentNode,Retira(MenuItem.Items[i].Caption,'&'));
          Node.Data:=MenuItem.Items[i];
          if MenuItem.Items[i].Count>0 then
             SubItems(MenuItem.Items[i],Node);
        end;
   end;
end;

begin
  TreeView1.Items.Clear;
  Menu:=NIL; // turns off the main menu
  Root:=TreeView1.Items.Add(NIL,'Menu');
  for i:=0 to MainMenu1.Items.Count-1 do
    begin
      ComponentInfo:=UsersCSReg1.GetComponentInfo(MainMenu1.Items[i].Name);
      if (ComponentInfo.ComponentStatus in [csNotRegistered,csEnabledVisible]) then
        begin
          Node:=TreeView1.Items.AddChild(Root,Retira(MainMenu1.Items[i].Caption,'&'));
          Node.Data:=MainMenu1.Items[i];
          if MainMenu1.Items[i].Count>0 then
            SubItems(MainMenu1.Items[i],Node)
       end;
    end;
  TreeView1.FullExpand;
end;

procedure TForm1.OpenProject1Click(Sender: TObject);
begin
  ShowMessage(Retira((Sender as TMenuItem).Caption,'&'));
end;

procedure TForm1.TreeView1DblClick(Sender: TObject);
begin
  if Treeview1.Selected=NIL then
    Exit;
  try
    if Treeview1.Selected.Data<>NIL then
      if Assigned(TMenuItem(Treeview1.Selected.Data).OnClick) then
        TMenuItem(Treeview1.Selected.Data).Click;
  finally
    ;
  end;
end;

procedure TForm1.UsersCS1BeforeLogin;
begin
  if not dbUsers.Connected then
    dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
