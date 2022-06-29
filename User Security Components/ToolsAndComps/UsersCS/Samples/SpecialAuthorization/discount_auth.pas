unit discount_auth;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls;

type
  TfrmDiscountAuth = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    lblUserName: TLabel;
    lblPassword: TLabel;
    UserName: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Password: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure OKBtnEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDiscountAuth: TfrmDiscountAuth;

implementation

{$R *.DFM}

procedure TfrmDiscountAuth.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    vk_return:
      begin
        key:=0;
        perform(WM_NEXTDLGCTL,0,0);
      end;
  end;
end;

procedure TfrmDiscountAuth.FormActivate(Sender: TObject);
begin
//  Caption:=LoadStr(strLoginMainForm);
//  lblUserName.Caption:=LoadStr(strLogin_lblUserName);
//  OKBtn.Caption:=LoadStr(strLogin_OKBtn);
//  CancelBtn.Caption:=LoadStr(strLogin_CancelBtn);
//  lblPassword.Caption:=LoadStr(strLogin_lblPassword);
  UserName.SetFocus;
  UserName.SelectAll;
end;

procedure TfrmDiscountAuth.OKBtnEnter(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

end.

