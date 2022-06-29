unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, users_basic, users_cs, DBTables, StdCtrls, ExtCtrls, Db;

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
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    edtTotal: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtDiscount: TEdit;
    Label4: TLabel;
    edtTotalAndDiscount: TEdit;
    Button1: TButton;
    ActionList1: TActionList;
    actnDiscont: TAction;
    procedure ShowUserNameExecute(Sender: TObject);
    procedure ShowUserPasswordExecute(Sender: TObject);
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure UserAdministrationExecute(Sender: TObject);
    procedure LoginExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure dbUsersBeforeConnect(Sender: TObject);
  private
    { Private declarations }
    procedure DiscontExecute;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses discount_auth;

var
  additional_info1, additional_info2: String;

{$R *.DFM}

procedure TForm1.ShowUserNameExecute(Sender: TObject);
begin
  ShowMessage('User Name is '+UsersCS1.ActualUser.UserName);
end;

procedure TForm1.ShowUserPasswordExecute(Sender: TObject);
begin
  ShowMessage('User Password is '+UsersCS1.ActualUser.Password);
end;

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
  LoginExecute(NIL);
end;

procedure TForm1.DiscontExecute;
var
  ComponentInfo: TComponentInfo;
  Discount: String;
begin
  frmDiscountAuth:=TfrmDiscountAuth.Create(NIL);
  try
    frmDiscountAuth.ShowModal;
    //  TLoginStatus = (lsInvalidUserName, lsInvalidPassword, lsValidUser, lsUserInactive);
    if UsersCS1.VerifyUser(frmDiscountAuth.UserName.Text, frmDiscountAuth.Password.Text)=lsValidUser then
      // if the user is valid
      begin
        ComponentInfo:=UsersCSReg1.GetUserComponentInfo(frmDiscountAuth.UserName.Text, actnDiscont.Name);
        // TComponentStatus = (csEnabledVisible, csDisabledVisible, csInvisible, csNotRegistered);
        if ComponentInfo.ComponentStatus = csEnabledVisible then
          begin
            Discount:=InputBox('Discount', 'Type the Discount:', '10');
            edtDiscount.Text:=Discount;
            edtTotalAndDiscount.Text:=IntToStr(StrToInt(edtTotal.Text)-StrToInt(edtDiscount.Text));
            Randomize;
            additional_info1:='Discount for Sale Number '+IntToStr(Random(9999))+': '+CurrencyString+Discount; //FormatFloat(CurrencyString+' 0.00',StrToInt(Discount));
            additional_info2:='Discount Authorized by '+frmDiscountAuth.UserName.Text;
          end
        else
          raise Exception.Create('You cannot authorize the discount.');
      end
    else
      raise Exception.Create('You cannot authorize the discount.');
  finally
    frmDiscountAuth.Free;
  end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_F1 then
    DiscontExecute;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if additional_info1<>'' then
  // if there was a discount
    UsersCSReg1.Audit(actnDiscont.Name, additional_info1, additional_info2);
  ShowMessage('Sale Done.');
  edtDiscount.Text:='0';
  edtTotalAndDiscount.Text:='0';
  additional_info1:='';
  additional_info2:='';
end;

procedure TForm1.dbUsersBeforeConnect(Sender: TObject);
begin
//  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName);
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
