unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_skin, ExtCtrls, API_grbutton;

type
  TForm2 = class(TForm)
    API_skin1: TAPI_skin;
    API_grbutton5: TAPI_grbutton;
    procedure API_grbutton5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.API_grbutton5Click(Sender: TObject);
begin
  close;
end;

end.
