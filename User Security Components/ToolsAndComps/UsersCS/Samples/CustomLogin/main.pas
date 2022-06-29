unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, users_cs, DBTables, StdCtrls, Db;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    dbUsers: TDatabase;
    Memo1: TMemo;
    Button1: TButton;
    procedure UsersCS1Login(var UserName, Password: String;
      var ModalResult: TModalResult);
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

uses custom_login;

{$R *.DFM}

procedure TForm1.UsersCS1Login(var UserName, Password: String;
  var ModalResult: TModalResult);
begin
  frmCustomLogin:=TfrmCustomLogin.Create(NIL);
  try
    frmCustomLogin.ShowModal;
    UserName:=frmCustomLogin.Usuario.Text;
    Password:=frmCustomLogin.Senha.Text;
    ModalResult:=frmCustomLogin.ModalResult
  finally
    frmCustomLogin.Free;
  end;    
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  while not UsersCS1.Login do
    ;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
//  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName);
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
