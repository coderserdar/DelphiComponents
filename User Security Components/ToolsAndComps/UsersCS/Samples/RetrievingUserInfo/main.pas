unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, users_basic, users_cs, DBTables, StdCtrls, Db;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    dbUsers: TDatabase;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  UserName: String;
  aUser: TUser;

function BoolToString(Value: Boolean): String;
begin
  if Value then Result:='Yes' else Result:='No';
end;

begin
  if InputQuery('Retrieving User Info','Type a User Name:',UserName) then
    begin
      aUser:=TUser.Create;
      aUser:=UsersCS1.GetUserInfo(UserName);
      if aUser=NIL then
        raise Exception.Create('Can''t retrieve information for the user '+UserName)
      else
        begin
          Memo1.Lines.Clear;
          Memo1.Lines.Add('This sample application shows how to retrieve ');
          Memo1.Lines.Add('security information for a user that differs for the ');
          Memo1.Lines.Add('current user in the application.');
          Memo1.Lines.Add('');
          Memo1.Lines.Add('Information for the User '+UserName);
          Memo1.Lines.Add('');
          Memo1.Lines.Add('User Name: '+aUser.UserName);
          Memo1.Lines.Add('Real Name: '+aUser.RealName);
          Memo1.Lines.Add('Profile: '+aUser.Profile);
          Memo1.Lines.Add('Additional Info: '+aUser.AdditionalInfo);
          Memo1.Lines.Add('User Id: '+IntToStr(aUser.UserId));
          Memo1.Lines.Add('Profile Id: '+IntToStr(aUser.ProfileId));
          Memo1.Lines.Add('Last Password Change: '+DateToStr(aUser.LastPwdChange));
          Memo1.Lines.Add('User Expire: '+BoolToString(aUser.UserExpire));
          if aUser.UserExpire then
            Memo1.Lines.Add('Expiration Date: '+DateToStr(aUser.ExpirationDate));
          Memo1.Lines.Add('User is Active: '+BoolToString(aUser.UserActive));
          Memo1.Lines.Add('User is in Audit More: '+BoolToString(aUser.AuditMode));
          Memo1.Lines.Add('User is Administrator: '+BoolToString(aUser.UserIsAdmin));
          Memo1.Lines.Add('User is Master: '+BoolToString(aUser.UserIsMaster));
          Memo1.Lines.Add('Last Login: '+DateTimeToStr(aUser.LastLoginDateTime));
        end;
      aUser.Free;  
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  UsersCs1.UsersAdm;
end;

procedure TForm1.UsersCS1BeforeLogin;
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
