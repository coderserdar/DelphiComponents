unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  users_basic, users_CS, Db, DBTables, Menus, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    UsersCS1: TUsersCS;
    dbUsers: TDatabase;
    UsersCSReg1: TUsersCSReg;
    procedure FormCreate(Sender: TObject);
    procedure UsersCS1AddComponentsAtRunTime;
    procedure dbUsersBeforeConnect(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeUserPassword(Sender: TObject);
    procedure UserAdministration(Sender: TObject);
    procedure Login(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  MenuItemParent, MenuItem: TMenuItem;
begin
  MenuItemParent:=TMenuItem.Create(Self);
  MenuItemParent.Name:='Customer';
  MenuItemParent.Caption:='Customer';
  MainMenu1.Items.Add(MenuItemParent);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='Add';
  MenuItem.Caption:='Add';
  MenuItemParent.Add(MenuItem);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='Edit';
  MenuItem.Caption:='Edit';
  MenuItemParent.Add(MenuItem);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='List';
  MenuItem.Caption:='List';
  MenuItemParent.Add(MenuItem);

  MenuItemParent:=TMenuItem.Create(Self);
  MenuItemParent.Name:='Security';
  MenuItemParent.Caption:='Security';
  MainMenu1.Items.Add(MenuItemParent);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='UserAdm';
  MenuItem.Caption:='User Administration';
  MenuItem.OnClick:=UserAdministration;
  MenuItemParent.Add(MenuItem);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='ChangePwd';
  MenuItem.Caption:='Change Password';
  MenuItem.OnClick:=ChangeUserPassword;
  MenuItemParent.Add(MenuItem);

  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:='Login';
  MenuItem.Caption:='Login';
  MenuItem.OnClick:=Login;
  MenuItemParent.Add(MenuItem);
  // * in order to add componenst at runtime, the AutoLogin must be false
  //   the login method must be called by the developer, so you are able to
  //   add your run-time components before the component updates the
  //   security database.
  Login(Sender);
  // * the code above executes the Login procedure, that executes the
  //   component login method.
end;

procedure TForm1.ChangeUserPassword(Sender: TObject);
begin
  UsersCS1.ChangeUserPassword;
end;

procedure TForm1.UserAdministration(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.Login(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate;
end;

procedure TForm1.UsersCS1AddComponentsAtRunTime;
var
  i,j: Integer;
begin
  // AddComponentAtRunTime(FormName, ComponentName, ComponentCaption, ComponentParent: String);
  UsersCS1.AddComponentsAtRunTime(Form1.Name,MainMenu1.Name,'Menu',Form1.Name);
  for i:=0 to MainMenu1.Items.Count-1 do
    begin
      UsersCS1.AddComponentsAtRunTime(Form1.Name,MainMenu1.Items[i].Name, MainMenu1.Items[i].Caption, MainMenu1.Name);
      for j:=0 to MainMenu1.Items[i].Count-1 do
        UsersCS1.AddComponentsAtRunTime(Form1.Name,MainMenu1.Items[i].Items[j].Name, MainMenu1.Items[i].Items[j].Caption, MainMenu1.Items[i].Name);
    end
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
