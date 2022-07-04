unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, users_basic, users_cs, StdCtrls;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    dbUsers: TDatabase;
    Button1: TButton;
    Button2: TButton;
    UsersCSReg1: TUsersCSReg;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
    Application.Terminate;
  UsersCS1.UsersAdmCentral;
  Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
//UsersCS1.UsersAdmCentral;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
//UsersCS1.UsersAdm;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
