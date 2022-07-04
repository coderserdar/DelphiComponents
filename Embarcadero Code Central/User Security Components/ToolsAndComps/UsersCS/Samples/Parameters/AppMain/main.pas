unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, users_basic, users_cs, Db, DBTables;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    MainMenu1: TMainMenu;
    AppLaucher1: TMenuItem;
    LaunchApp1: TMenuItem;
    Users1: TMenuItem;
    Admin1: TMenuItem;
    ChangePassword1: TMenuItem;
    dbUsers: TDatabase;
    procedure FormCreate(Sender: TObject);
    procedure Admin1Click(Sender: TObject);
    procedure ChangePassword1Click(Sender: TObject);
    procedure LaunchApp1Click(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not UsersCS1.Login then
    Close;
end;

procedure TForm1.Admin1Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.ChangePassword1Click(Sender: TObject);
begin
  UsersCS1.ChangeUserPassword;
end;

procedure TForm1.LaunchApp1Click(Sender: TObject);
begin
  WinExec(PChar(ExtractFilePath(Application.ExeName)+'SecondApp.EXE '+
    UsersCS1.ActualUser.UserName+' '+UsersCS1.ActualUser.Password),SW_SHOW);
    {Passing the username and password as parameters to other app}
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
