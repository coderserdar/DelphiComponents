unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, StdCtrls, API_edit, API_listbox;

type
  TForm1 = class(TForm)
    API_listbox1: TAPI_listbox;
    API_edit1: TAPI_edit;
    API_grbutton1: TAPI_grbutton;
    procedure API_grbutton1Click(Sender: TObject);
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
  api_listbox1.Items.add(api_edit1.text);
end;

end.
