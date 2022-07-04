unit DBLoginUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TDbLogin = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    DatabaseName: TStaticText;
    Bevel1: TBevel;
    Label2: TLabel;
    Username: TEdit;
    Label3: TLabel;
    Password: TEdit;
    Bevel2: TBevel;
    Label4: TLabel;
    DbPassword: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DbLogin: TDbLogin;

implementation

{$R *.DFM}

procedure TDbLogin.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDbLogin.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
