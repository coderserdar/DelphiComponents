unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, users_cs, DBTables, StdCtrls, Db;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    ChangeUserPassword: TAction;
    UserAdministration: TAction;
    Login: TAction;
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    dbUsers: TDatabase;
    Users1: TMenuItem;
    UserAdministration1: TMenuItem;
    ChangeUserPassword1: TMenuItem;
    Login1: TMenuItem;
    N1: TMenuItem;
    Memo2: TMemo;
    Label1: TLabel;
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UsersCS1ValidatePassword(Password: String;
      var Accept: Boolean);
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
  Login.Enabled:=True;
  Login.Execute;
end;

procedure TForm1.UsersCS1ValidatePassword(Password: String;
  var Accept: Boolean);
var
  i: Integer;

function IsNumber(chr: Char): Boolean;
begin
  Result:=chr in ['0'..'9'];
end;

begin
  // the minimum size for the password is already validated by the property MinPwdSize
  //  we will verify the demand of at least a number
  Accept:=False;
  for i:=1 to Length(Password) do
    begin
      if IsNumber(Password[i]) then
      // accepts password and breaks the loop
        begin
          Accept:=True;
          Break;
        end;
    end;
  if not Accept then
    ShowMessage('Password must contain at least one numeric character.');
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
//  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName);
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
