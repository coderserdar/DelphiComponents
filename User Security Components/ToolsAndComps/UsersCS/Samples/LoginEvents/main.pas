unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, users_cs, DBTables, StdCtrls, Db, ExtCtrls;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    dbUsers: TDatabase;
    Memo1: TMemo;
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure dbUsersBeforeConnect(Sender: TObject);
    procedure UsersCS1BeforeLogin;
    procedure UsersCS1WhileDoLogin;
    procedure UsersCS1AfterLogin;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses splash;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
//  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName);
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

procedure TForm1.UsersCS1BeforeLogin;
begin
  frmSplash:=TfrmSplash.Create(NIL);
end;

procedure TForm1.UsersCS1WhileDoLogin;
var
  i: TTime;
begin
  frmSplash.Show;
  frmSplash.Update;
  i:=Time+0.000009;
  while i>Time do ;
end;

procedure TForm1.UsersCS1AfterLogin;
begin
  if UsersCS1.LastLoginOk then
    Panel1.Caption:='User: '+UsersCS1.ActualUser.UserName;
  frmSplash.Release;
end;

end.
