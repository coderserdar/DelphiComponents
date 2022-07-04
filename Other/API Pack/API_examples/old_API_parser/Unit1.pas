unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_listbox, ExtCtrls, API_grbutton, API_edit,
  API_parser;

type
  TForm1 = class(TForm)
    API_edit1: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    API_listbox1: TAPI_listbox;
    API_parser1: TAPI_parser;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_parser1.InText:=api_edit1.text;
  api_listbox1.items:=api_parser1.List;
end;

procedure TForm1.API_edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then
    api_grbutton1click(self);
end;

end.
