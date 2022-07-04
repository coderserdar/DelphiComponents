unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, API_stringgrid, StdCtrls;

type
  TForm1 = class(TForm)
    API_stringgrid1: TAPI_stringgrid;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s: string;
begin
  api_stringgrid1.ExportHtmlText(s, true);
  showmessage(s);
end;

end.
