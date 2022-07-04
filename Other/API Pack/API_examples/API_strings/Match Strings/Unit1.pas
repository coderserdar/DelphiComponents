unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  API_strings;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if api_strings.Match(edit1.Text, edit2.Text) then
    label3.Caption:= 'TRUE' else
    label3.caption:= 'NOT FOUND';
end;

procedure TForm1.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then
    button1click(self);
end;

end.
