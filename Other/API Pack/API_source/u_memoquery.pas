unit u_memoquery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, API_abform, API_grbutton, API_base;

type
  Tf_memoquery = class(TForm)
    Memo1: TMemo;
    API_abform1: TAPI_abform;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
  public
    memo: string;
    clear: boolean;
  end;

var
  f_memoquery: Tf_memoquery;

implementation

{$R *.dfm}

procedure Tf_memoquery.FormCreate(Sender: TObject);
begin
  // nothing..
end;

procedure Tf_memoquery.FormShow(Sender: TObject);
begin
  if clear then
  begin
    memo1.clear;
    memo:='';
  end;
  memo1.SetFocus;
end;

procedure Tf_memoquery.BitBtn1Click(Sender: TObject);
begin
  memo:=memo1.Text;
  close;
end;

procedure Tf_memoquery.BitBtn2Click(Sender: TObject);
begin
  memo:='';
  close;
end;

procedure Tf_memoquery.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then bitbtn1Click(self);
end;

end.
