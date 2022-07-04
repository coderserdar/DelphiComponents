unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, users_basic, users_cs, DBTables, StdCtrls, Grids,
  DBGrids, ExtCtrls, Db;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    UsersCS1: TUsersCS;
    UsersCSReg1: TUsersCSReg;
    dbUsers: TDatabase;
    Users1: TMenuItem;
    UserAdministration1: TMenuItem;
    ChangeUserPassword1: TMenuItem;
    Login1: TMenuItem;
    N1: TMenuItem;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Panel3: TPanel;
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UsersCS1ShowAdditionalInfo(var aForm: TForm;
      var aPanel: TPanel; GetActualUserId: TGetActualUserId);
    procedure dbUsersBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dtm_Sample, additional_info;

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
  while not UsersCS1.Login do
    ;
  Panel3.Caption:='User: ' + UsersCS1.ActualUser.UserName+' ['+UsersCS1.ActualUser.RealName+']';
  SampleData.Companies.Close;
  if UsersCS1.Actualuser.AdditionalInfo<>'' then
  // modification of the Where clause to restrict the data the user can view
    SampleData.Companies.SQL[2]:='Where '+UsersCS1.Actualuser.AdditionalInfo
  else
    SampleData.Companies.SQL[2]:=' ';
  SampleData.Companies.Open;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('User Name: master'+#10#13+'Password: master');
  UsersCS1.Login;
  if not UsersCS1.LastLoginOk then
    Exit;
  SampleData:=TSampleData.Create(Application);
  SampleData.Companies.Close;
  if UsersCS1.Actualuser.AdditionalInfo<>'' then
  // modification of the Where clause to restrict the data the user can view
    SampleData.Companies.SQL[2]:='Where '+UsersCS1.Actualuser.AdditionalInfo
  else
    SampleData.Companies.SQL[2]:=' ';
  SampleData.Companies.Open;
end;

procedure TForm1.UsersCS1ShowAdditionalInfo(var aForm: TForm;
  var aPanel: TPanel; GetActualUserId: TGetActualUserId);
begin
  frmAdditionalInfo:=TfrmAdditionalInfo.Create(NIL);
  aForm :=frmAdditionalInfo;
  aPanel:=frmAdditionalInfo.ThePanel;
  frmAdditionalInfo.GetActualUserId:=GetActualUserId;
  frmAdditionalInfo.AppKey:=UsersCS1.AppKey;
  // the form frmAdditionalInfo will be released by the TUsersCS component
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
//  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName);
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
