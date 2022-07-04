{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtLoginDialog                                  }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{                                                       }
{                                                       }
{                                                       }
{*******************************************************}
unit LoginDialog;


{ TgtLoginDialog Ver 1.0 Programmed by George Trifidis Free Software E-mail:info@gtdelphicomponents.gr
*
* TgtLoginDialog is TComponent descenant which includes a TForm component.
* It provides full interface for the programmer to use a login interface
* without writing any code. It's very easy to use.
*
* Properties :
  FAppEvents:TApplicationEvents;// Application Message manipulation
  FLoginForm:TFrmLogin;// The Login Form
  FDataSet:TDataset; // The dataset where usernames and passwords are stored
  FMaxAttempts:Integer;// If  set 0 unlimited attempts
  FShowOnAppStart:Boolean;// If true then dialog popups after mainform is showed.
  FShowModal:Boolean;// If True Login form is modal or not
  FListUsers:Boolean;// If True then in the combobox are added all the username from the dataset
  FFormCaption,FUserLabelCaption,FGroupBoxCaption,
  FPassLabelCaption,FLoginBtnCaption,FCancelBtnCaption:String;// Easy to understand (i suppose);
  FUserFieldName,FPassFieldName:String;// THIS CRUSIAL YOU MUST ENTER THE FIELDNAMES OF THE
    USERNAME AND PASSWORD FIELD E.G.:'UserName','Password'.
  FErrorMsgTitle,FErrorMsgText:String;
  The text and title of the messagebox if a wrong username or password is entered.

  For any comments or suggestions or if you use or add new properties or capabilities
  please add to you thanx list or email me the new additions.
  E-mail :info@gtdelphicomponents.gr
}

interface

uses
  SysUtils, Classes,Windows,Messages,DB,Forms,
  Controls, StdCtrls,AppEvnts,Buttons, ExtCtrls,
  ComCtrls,Graphics
  ;
const
  WM_AFTER_SHOW    = WM_USER + 300;
  WM_AFTER_CREATE  = WM_USER + 301;
type
  {------------------------------------------------------------------------------}
  TFrmLogin = class;
  {------------------------------------------------------------------------------}
  TOnAfterSuccessLogin = procedure (Sender : TObject ; UserName : string) of Object;
  {------------------------------------------------------------------------------}
  TgtLoginDialog = class(TComponent)
  private
    { Private declarations }
    FAppEvents:TApplicationEvents;
    FLoginForm:TFrmLogin;
    FDataSet:TDataset;
    FMaxAttempts:Integer;
    FShowOnAppStart:Boolean;
    FShowModal:Boolean;
    FListUsers:Boolean;
    FFormCaption,FUserLabelCaption,FGroupBoxCaption,
    FPassLabelCaption,FLoginBtnCaption,FCancelBtnCaption:String;
    FUserFieldName,FPassFieldName:String;
    FErrorMsgTitle,FErrorMsgText:String;
    FAttCount:Integer;
    FOnAfterSuccessLogin : TOnAfterSuccessLogin;
    FIsFirstAppRun: Boolean;
    FUserActiveFieldName: String;
    FPaspartouUserName: String;
    FPaspartouPassWord: String;
    procedure SetDataSet(const Value: TDataset);
  protected
    { Protected declarations }
    procedure OnMsg(var Msg:tagMsg;var Handled:Boolean);
    procedure SetShowOnAppStart(const Value:Boolean);
    procedure SetListUsers(const Value:Boolean);
    procedure ApplyCaptions;
    procedure UserList;
    function  IsValidUser(aUserName,aPassword:string):Boolean;
  public
    { Public declarations }
    constructor Create (AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   ShowLogin;
  published
    { Published declarations }
    property  Dataset             :TDataset read FDataset              write SetDataSet;
    property  MaxAttempts         :integer  read FMaxAttempts          write FMaxAttempts      nodefault;
    property  ShowOnAppStart      :Boolean  read FShowOnAppStart       write SetShowOnAppStart nodefault;
    property  ShowModal           :Boolean  read FShowModal            write FShowModal        nodefault;
    property  FormCaption         :String   read FFormCaption          write FFormCaption;
    property  UserLabelCaption    :String   read FUserLabelCaption     write FUserLabelCaption;
    property  PassLabelCaption    :String   read FPassLabelCaption     write FPassLabelCaption;
    property  GroupBoxCaption     :String   read FGroupBoxCaption      write FGroupBoxCaption;
    property  LoginButtonCaption  :String   read FLoginBtnCaption      write FLoginBtnCaption;
    property  CancelBtnCaption    :String   read FCancelBtnCaption     write FCancelBtnCaption;
    property  UserFieldName       :String   read FUserFieldName        write FUserFieldName;
    property  PassFieldName       :String   read FPassFieldName        write FPassFieldName;
    property  UserActiveFieldName :String   read FUserActiveFieldName  write FUserActiveFieldName;
    property  ListUsers           :Boolean  read FListUsers            write FListUsers        nodefault;
    property  ErrorMsgTitle       :String   read FErrorMsgTitle        write FErrorMsgTitle;
    property  ErrorMsgText        :String   read FErrorMsgText         write FErrorMsgText;
    property  IsFirstAppRun       :Boolean  read FIsFirstAppRun;

    property  PaspartouUserName   :String   read FPaspartouUserName    write FPaspartouUserName;
    property  PaspartouPassWord   :String   read FPaspartouPassWord    write FPaspartouPassWord;

    property  OnAfterSuccesLogin:TOnAfterSuccessLogin read FOnAfterSuccessLogin write FOnAfterSuccessLogin;
  end;
{------------------------------------------------------------------------------}
 TFrmLogin = class(TForm)
    TabControl1: TTabControl;
    Bevel1: TBevel;
    LoginBtn: TBitBtn;
    CancelBtn: TBitBtn;
    GroupBox1: TGroupBox;
    UsrLbl: TLabel;
    PassLbl: TLabel;
    UserCombo: TComboBox;
    PassTxt: TEdit;
    Image1: TImage;
    Image2: TImage;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure LoginBtnClick(Sender: TObject);
    procedure PassTxtKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
{------------------------------------------------------------------------------}


implementation
{$R *.dfm}


{ TgtLoginDialog }
{------------------------------------------------------------------------------}
constructor TgtLoginDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormCaption     :='User Validation';
  FUserLabelCaption:='UserName :';
  FPassLabelCaption:='Password :';
  FLoginBtnCaption :='Login';
  FCancelBtnCaption:='Quit';
  FGroupBoxCaption :='Please enter your username and password';
  FErrorMsgTitle   :='User Validation';
  FErrorMsgText    :='Wrong username or password.Please enter again.';
  FAttCount        :=1;
  FAppEvents:=TApplicationEvents.Create(Self);
  FAppEvents.OnMessage:=OnMsg;
  PostMessage(TForm(Owner).Handle, WM_AFTER_CREATE, 0, 0);
  FIsFirstAppRun := True;
end;
{------------------------------------------------------------------------------}
destructor TgtLoginDialog.Destroy;
begin
  if Assigned(FLoginForm) then
    FreeAndNil(FLoginForm);
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.SetShowOnAppStart(const Value: Boolean);
begin
  FShowOnAppStart:=Value;
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.OnMsg(var Msg: tagMsg; var Handled: Boolean);
begin
  case Msg.message of
    WM_AFTER_SHOW:
      begin
        if not Assigned(FLoginForm) then
        FLoginForm:=TFrmLogin.Create(Self);
        ApplyCaptions;
        if FListUsers then
          UserList;
        if FShowModal then
          FLoginForm.ShowModal
        else
          FLoginForm.Show;
      end;
    WM_AFTER_CREATE:
      begin
        if not (csDesigning in ComponentState) then
         if FShowOnAppStart then
           begin
             PostMessage(TForm(Owner).Handle, WM_AFTER_SHOW, 0, 0);
           end;
      end;
  end;

end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.ApplyCaptions;
begin
  if FFormCaption<>'' then
    FLoginForm.Caption:=FFormCaption;
  if FUserLabelCaption<>'' then
    FLoginForm.UsrLbl.Caption:=FUserLabelCaption;
  if FPassLabelCaption<>'' then
    FLoginForm.PassLbl.Caption:=FPassLabelCaption;
  if FLoginBtnCaption<>'' then
    FLoginForm.LoginBtn.Caption:=FLoginBtnCaption;
  if FCancelBtnCaption<>'' then
    FLoginForm.CancelBtn.Caption:=FCancelBtnCaption;
  if FGroupBoxCaption<>'' then
    FLoginForm.GroupBox1.Caption:=FGroupBoxCaption;
  FLoginForm.PassTxt.Text := '';
  FLoginForm.UserCombo.Text := '';
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.ShowLogin;
begin
  PostMessage(TForm(Owner).Handle, WM_AFTER_SHOW, 0, 0);
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.SetListUsers(const Value: Boolean);
begin
  FListUsers:=Value;
  if FListUsers then UserList;
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.UserList;
var aFieldList:TStrings;
begin
  aFieldList:=TStringList.Create;
  try
    if not (csDesigning in ComponentState) then
      begin
        if not Assigned(FDataset) then exit;
        FDataset.Active:=True;
        FDataSet.GetFieldNames(aFieldList);
        if aFieldList.IndexOf(FUserFieldName)=-1 then Exit;
        FDataset.First;
        if not Assigned(FLoginForm) then
          FLoginForm := TFrmLogin.Create(Self);
        FLoginForm.UserCombo.Items.Clear;
        FDataset.First;
        while not FDataset.Eof do
          begin
            if Trim(UserActiveFieldName) <> '' then
            begin
              if FDataSet.FieldByName(UserActiveFieldName).AsBoolean  then
                FLoginForm.UserCombo.Items.Add(FDataSet.FieldByName(FUserFieldName).AsString);
            end
            else
                FLoginForm.UserCombo.Items.Add(FDataSet.FieldByName(FUserFieldName).AsString);
            FDataset.Next;
          end;
      end;
   finally
    aFieldList.Free;
   end;
end;
{------------------------------------------------------------------------------}
function TgtLoginDialog.IsValidUser(aUserName, aPassword: string): Boolean;
begin
  if FMaxAttempts<>0 then
  if FAttCount>FMaxAttempts then
    begin
      Application.Terminate;
    end;
  Result:=False;
  if (SameText(PaspartouUserName,aUserName) and SameText(PaspartouPassWord,aPassWord)) then
  begin
    Result := True;
    if Assigned(FOnAfterSuccessLogin) then
      FOnAfterSuccessLogin(Self,aUserName);
    FIsFirstAppRun := False;
    Exit;
  end;
  if ( (Trim(aUserName)='') and  (Trim(aPassWord)='')) then
  begin
    FAttCount:=FAttCount+1;
    MessageBox(HWND(nil),PChar(FErrorMsgText),PChar(FErrorMsgTitle),MB_OK+MB_ICONERROR);
  end;
  FDataset.Active:=True;
  if FDataset.Locate(FUserFieldName,aUserName,[loCaseInsensitive]) then
    begin
      if ((FDataset.FieldByName(FUserFieldName).AsString=aUserName)
           and (FDataset.FieldByName(FPassFieldName).AsString=aPassword)) then
           begin
            Result:=True;
            if Assigned(FOnAfterSuccessLogin) then
              FOnAfterSuccessLogin(Self,aUserName);
            FIsFirstAppRun := False;
           end
         else
          begin
           FAttCount:=FAttCount+1;
           MessageBox(HWND(nil),PChar(FErrorMsgText),PChar(FErrorMsgTitle),MB_OK+MB_ICONERROR);
          end;
    end;
{  else
  begin
    FAttCount:=FAttCount+1;
    MessageBox(HWND(nil),PChar(FErrorMsgText),PChar(FErrorMsgTitle),MB_OK+MB_ICONERROR);
  end;}
end;
{------------------------------------------------------------------------------}
procedure TgtLoginDialog.SetDataSet(const Value: TDataset);
begin
  FDataset := Value;
end;
{------------------------------------------------------------------------------}






{ TFrmLogin }
{------------------------------------------------------------------------------}
procedure TFrmLogin.CancelBtnClick(Sender: TObject);
begin
  if TgtLoginDialog(Owner).IsFirstAppRun then
    Application.Terminate
  else
    ModalResult := mrCancel;
end;
{------------------------------------------------------------------------------}
procedure TFrmLogin.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  case ModalResult of
    mrCancel :
      begin
        CanClose := True;
      end;
    else
      CanClose:=TgtLoginDialog(Owner).IsValidUser(UserCombo.Text,Passtxt.Text);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFrmLogin.LoginBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;
{------------------------------------------------------------------------------}
procedure TFrmLogin.PassTxtKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then ModalResult := mrOk;
end;
{------------------------------------------------------------------------------}

end.
