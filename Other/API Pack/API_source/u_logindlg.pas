unit u_logindlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, API_abform, API_grbutton,
  API_base, API_edit;

type
  Tf_logindlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_abform1: TAPI_abform;
    Edit1: TAPI_edit;
    MaskEdit1: TAPI_edit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure MaskEdit1KeyPress(Sender: TObject; var Key: Char);
  private
  public
    login: boolean;
    username: string;
    password: string;
    clearpass: boolean;
  end;

var
  f_logindlg: Tf_logindlg;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure Tf_logindlg.FormCreate(Sender: TObject);
begin
  // nothing..
end;

//------------------------------------------------------------------------------
procedure Tf_logindlg.FormShow(Sender: TObject);
begin
  edit1.text:= username;
  if not clearpass then maskedit1.text:= password
    else maskedit1.text:= '';
  login:= FALSE;
end;

//------------------------------------------------------------------------------
procedure Tf_logindlg.BitBtn1Click(Sender: TObject);
begin
  login:= TRUE;
  username:= edit1.text;
  password:= maskedit1.text;
  close;
end;

//------------------------------------------------------------------------------
procedure Tf_logindlg.BitBtn2Click(Sender: TObject);
begin
  login:= FALSE;
  close;
end;

//------------------------------------------------------------------------------
procedure Tf_logindlg.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then bitbtn1Click(self);
end;

//------------------------------------------------------------------------------
procedure Tf_logindlg.MaskEdit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then bitbtn1Click(self);
end;

end.
