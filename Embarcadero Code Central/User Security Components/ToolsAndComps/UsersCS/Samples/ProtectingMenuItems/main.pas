unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, DBTables, StdCtrls, users_cs, Db;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    dbUsers: TDatabase;
    Users1: TMenuItem;
    UserAdministration1: TMenuItem;
    ChangeUserPassword1: TMenuItem;
    Login1: TMenuItem;
    N1: TMenuItem;
    Memo1: TMemo;
    Customer1: TMenuItem;
    ViewPrivateData1: TMenuItem;
    NewCustomer1: TMenuItem;
    N2: TMenuItem;
    ViewOrders1: TMenuItem;
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    N3: TMenuItem;
    Exit1: TMenuItem;
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure dbUsersBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

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
    Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('User Name: master'+#10#13+'Password: master');
//  Login.Enabled:=True;
  dbUsers.Connected:=True;
  LoginExecute(NIL);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Form2=NIL then
    begin
      Form2:=TForm2.Create(Application);
      Form2.Show();
    end;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
