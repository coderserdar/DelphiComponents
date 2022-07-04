unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, users_basic, users_cs, DBTables, Db;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    dbUsers: TDatabase;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Users currently running this application: '+IntToStr(userscs1.GetNumberOfCurrentUsers));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not UsersCs1.Login then
    Application.Terminate;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
