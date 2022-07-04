unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  users_basic, users_cs, StdCtrls, DBTables;

type
  TForm1 = class(TForm)
    btnLogout: TButton;
    UsersCS1: TUsersCS;
    btnLogin: TButton;
    Database1: TDatabase;
    Button1: TButton;
    procedure btnLoginClick(Sender: TObject);
    procedure UsersCS1Login(var UserName, Password: String;
      var ModalResult: TModalResult);
    procedure UsersCS1BeforeLogin;
    procedure UsersCS1AfterLogin;
    procedure btnLogoutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    UsrPassword: String;
    UsrName: String;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.UsersCS1Login(var UserName, Password: String;
  var ModalResult: TModalResult);
begin
  UserName:=UsrName;
  Password:=UsrPassword;
  ModalResult:=mrOk;
end;

procedure TForm1.UsersCS1BeforeLogin;
begin
  UsersCS1.OnLogin:=NIL;
end;

procedure TForm1.UsersCS1AfterLogin;
begin
  UsersCS1.OnLogin:=UsersCS1Login;
  UsersCS1.BeforeLogin:=NIL;
  if UsersCS1.LastLoginOk then
    begin
      UsrPassword:=UsersCS1.ActualUser.Password;
      UsrName:=UsersCS1.ActualUser.UserName;
    end;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  UsersCS1.Login;
  if UsersCS1.LastLoginOk then
    begin
      btnLogout.Enabled:=True;
      btnLogin.Enabled:=False;
    end;
end;

procedure TForm1.btnLogoutClick(Sender: TObject);
begin
  UsersCS1.Logout;
  btnLogout.Enabled:=False;
  btnLogin.Enabled:=True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

end.
