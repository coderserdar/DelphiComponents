unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  api_strings;

{$R *.dfm}

procedure TForm1.FormActivate(Sender: TObject);
begin
  label2.caption:= inttostr(length(edit1.text));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  p: integer;
  fp: integer;
begin
  p:= strtoint(edit2.text);
  label1.caption:=
    api_strings.FindTagText(edit1.text, p, fp);
  if fp>0 then
    edit2.text:= inttostr(fp);
end;

end.
