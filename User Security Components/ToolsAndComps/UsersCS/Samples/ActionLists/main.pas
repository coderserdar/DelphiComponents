unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, users_cs, DBTables, StdCtrls, Db, ExtCtrls;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    ShowUserName: TAction;
    ShowUserPassword: TAction;
    ChangeUserPassword: TAction;
    UserAdministration: TAction;
    Login: TAction;
    dbUsers: TDatabase;
    Users1: TMenuItem;
    UserAdministration1: TMenuItem;
    ChangeUserPassword1: TMenuItem;
    Login1: TMenuItem;
    N1: TMenuItem;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    N2: TMenuItem;
    ShowUserName1: TMenuItem;
    ShowUserPassword1: TMenuItem;
    Memo1: TMemo;
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    procedure ShowUserNameExecute(Sender: TObject);
    procedure ShowUserPasswordExecute(Sender: TObject);
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dbUsersBeforeConnect(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ShowUserNameExecute(Sender: TObject);
begin
  ShowMessage('User Name is '+UsersCS1.ActualUser.UserName);
end;

procedure TForm1.ShowUserPasswordExecute(Sender: TObject);
begin
  ShowMessage('User Password is '+UsersCS1.ActualUser.Password);
end;

procedure TForm1.ChangeUserPasswordExecute(Sender: TObject);
begin
  UsersCS1.ChangeUserPassword;
end;

procedure TForm1.UserAdministrationExecute(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('User Name: master'+#10#13+'Password: master');
  Login.Enabled:=True;
  Login.Execute;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

procedure TForm1.LoginExecute(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate;
end;

end.
