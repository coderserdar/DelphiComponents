unit frmMainForm_Rep;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  users_basic, users_cs, Menus;

type
  TfrmMainForm_Rep = class(TForm)
    MainMenu1: TMainMenu;
    Security1: TMenuItem;
    UserAdministraion1: TMenuItem;
    ChangePassword1: TMenuItem;
    N1: TMenuItem;
    Login1: TMenuItem;
    UsersCSReg1: TUsersCSReg;
    UsersCS1: TUsersCS;
    procedure UserAdministraion1Click(Sender: TObject);
    procedure ChangePassword1Click(Sender: TObject);
    procedure Login1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm_Rep1: TfrmMainForm_Rep;

implementation

{$R *.DFM}


procedure TfrmMainForm_Rep.UserAdministraion1Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TfrmMainForm_Rep.ChangePassword1Click(Sender: TObject);
begin
  UsersCS1.ChangeUserPassword;
end;

procedure TfrmMainForm_Rep.Login1Click(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate;
end;

end.
