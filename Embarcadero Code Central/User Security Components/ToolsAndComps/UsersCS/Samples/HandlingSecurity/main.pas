unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, DBTables, ExtCtrls, DBCtrls, ComCtrls, ToolWin,
  users_basic, users_cs, Menus, StdCtrls, ImgList;

type
  TForm1 = class(TForm)
    UsersCS1: TUsersCS;
    ImageList1: TImageList;
    DataSource1: TDataSource;
    Table1: TTable;
    DBGrid1: TDBGrid;
    Table1EventNo: TAutoIncField;
    Table1VenueNo: TIntegerField;
    Table1Event_Name: TStringField;
    Table1Event_Date: TDateField;
    Table1Event_Time: TTimeField;
    Table1Event_Description: TMemoField;
    Table1Ticket_price: TCurrencyField;
    Table1Event_Photo: TGraphicField;
    MainMenu1: TMainMenu;
    Security1: TMenuItem;
    UserAdministration1: TMenuItem;
    N1: TMenuItem;
    Login1: TMenuItem;
    Database1: TDatabase;
    UsersCSReg1: TUsersCSReg;
    Memo1: TMemo;
    procedure UsersCSReg1ApplySecurity(Sender: TObject;
      ComponentInfo: TComponentInfo; var Handled: Boolean);
    procedure UserAdministration1Click(Sender: TObject);
    procedure Login1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure UsersCS1AfterLogin;
    procedure Database1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.UsersCSReg1ApplySecurity(Sender: TObject;
  ComponentInfo: TComponentInfo; var Handled: Boolean);
begin
  if Sender is TColumn then
    begin
      if ComponentInfo.Name='DBGrid1Col3' then
      // check the component name in the Component Registration Form
      // (ComponentList property)
        begin
          if ComponentInfo.ComponentStatus in [csDisabledVisible, csInvisible]  then
            begin
              DBGrid1.Columns[3].Color:=clGreen;
              DBGrid1.Columns[3].Font.Color:=clWhite;
              DBGrid1.Columns[3].Font.Style:=DBGrid1.Columns[3].Font.Style+[fsBold];
              DBGrid1.Columns[3].ReadOnly:=True;
              DBGrid1.Repaint;
              // it means the developer is handlying the security of this component
              Handled:=True;
            end;
        end;
    end;
end;

procedure TForm1.UserAdministration1Click(Sender: TObject);
begin
  UsersCS1.UsersAdm;
end;

procedure TForm1.Login1Click(Sender: TObject);
begin
  if not UsersCS1.Login then
    Application.Terminate;
end;


procedure TForm1.FormActivate(Sender: TObject);
begin
  Table1.Open;
end;

procedure TForm1.UsersCS1AfterLogin;
begin
  if not UsersCS1.ActualUser.UserIsMaster then
    ShowMessage('Your last login: '+DateTimeToStr(UsersCS1.ActualUser.LastLoginDateTime));
end;

procedure TForm1.Database1BeforeConnect(Sender: TObject);
begin
  Database1.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
