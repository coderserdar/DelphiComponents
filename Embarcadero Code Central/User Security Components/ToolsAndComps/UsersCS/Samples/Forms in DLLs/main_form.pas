unit main_form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  users_basic, users_cs, StdCtrls, DBTables, Db;

type
  TfrmMainForm = class(TForm)
    UsersCS1: TUsersCS;
    Button1: TButton;
    Database1: TDatabase;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Database1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

procedure ExecForm1(const TheSecurityComponent: TUsersCS); external 'DLLForm1.dll';  

implementation

{$R *.DFM}

procedure TfrmMainForm.Button1Click(Sender: TObject);
begin
  ExecForm1(UsersCS1);
end;

procedure TfrmMainForm.Button2Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TfrmMainForm.Database1BeforeConnect(Sender: TObject);
begin
  Database1.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
 