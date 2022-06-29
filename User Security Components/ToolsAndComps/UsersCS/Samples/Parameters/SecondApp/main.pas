unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, users_basic, users_cs, Db, DBTables;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    MainMenu1: TMainMenu;
    Users1: TMenuItem;
    Admin1: TMenuItem;
    Memo1: TMemo;
    dbUsers: TDatabase;
    procedure Admin1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UsersCS1Login(var UserName, Password: String;
      var ModalResult: TModalResult);
    procedure UsersCS1BeforeLogin;
    procedure dbUsersBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Admin1Click(Sender: TObject);
begin
  UsersCS1.UsersADM;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamCount = 0 then
    UsersCS1.OnLogin:=Nil; {if the app is lauched from the Explorer, for example
    sets the OnLogin to nil to execute the default Login Dlg}
  if not UsersCS1.Login then
    Close;
end;

procedure TForm1.UsersCS1Login(var UserName, Password: String;
  var ModalResult: TModalResult);
begin
  UserName:=ParamStr(1);
  Password:=ParamStr(2);
  ModalResult:=MrOk;
end;

procedure TForm1.UsersCS1BeforeLogin;
begin
  UsersCs1.MaxBadLogins := 1;  
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
