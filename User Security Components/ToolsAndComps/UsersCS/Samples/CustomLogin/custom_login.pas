unit custom_login;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TfrmCustomLogin = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    lblUserName: TLabel;
    lblPassword: TLabel;
    Usuario: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Senha: TEdit;
    Image1: TImage;
    StaticText1: TStaticText;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCustomLogin: TfrmCustomLogin;

implementation

{$R *.DFM}


procedure TfrmCustomLogin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {keyPreview must be TRUE}
  case key of
    vk_return:
      begin
        key:=0;
        perform(WM_NEXTDLGCTL,0,0);
      end;
  end;
end;

procedure TfrmCustomLogin.FormCreate(Sender: TObject);
begin
  keyPreview:=True;
end;

procedure TfrmCustomLogin.OKBtnClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TfrmCustomLogin.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

end.
