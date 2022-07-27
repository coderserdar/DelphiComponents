unit Passtest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MD5;

type
  TfrmPasswordTest = class(TForm)
    edtStorePassword: TEdit;
    btnSetPassword: TButton;
    lblPassword: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    lblPasswordHashStore: TLabel;
    mmoSetExplanation: TMemo;
    Bevel3: TBevel;
    btnCheckPassword: TButton;
    mmoCheckExplanation: TMemo;
    lblPasswordHashCheck: TLabel;
    Bevel4: TBevel;
    Label1: TLabel;
    edtCheckPassword: TEdit;
    MD51: TMD5;
    procedure btnSetPasswordClick(Sender: TObject);
    procedure btnCheckPasswordClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPasswordTest: TfrmPasswordTest;

implementation

{$R *.DFM}

procedure TfrmPasswordTest.btnSetPasswordClick(Sender: TObject);
begin
     MD51.Init;
     MD51.HashString(edtStorePassword.Text);
     MD51.Finish;
     lblPasswordHashStore.Caption := MD51.GetHashString;
end; {btnSetPasswordClick}

procedure TfrmPasswordTest.btnCheckPasswordClick(Sender: TObject);
begin
     MD51.Init;
     MD51.HashString(edtCheckPassword.Text);
     MD51.Finish;
     lblPasswordHashCheck.Caption := MD51.GetHashString;
     if lblPasswordHashCheck.Caption = lblPasswordHashStore.Caption then
     begin
          ShowMessage('The Password is correct!');
     end
     else
     begin
          ShowMessage('The Passwords do not match');
     end; {if}
end; {btnCheckPasswordClick}

end.
