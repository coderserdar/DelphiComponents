unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, API_inedit, ExtCtrls, API_grbutton;

type
  TForm1 = class(TForm)
    API_grbutton1: TAPI_grbutton;
    API_inedit1: TAPI_inedit;
    CheckBox1: TCheckBox;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_inedit1Hide(Sender: TObject);
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
  api_inedit1.OnlyNumbers:=checkbox1.checked;

  api_inedit1.text:=api_grbutton1.caption;

  api_inedit1.Open(
    rect(
      api_grbutton1.Left,
      api_grbutton1.Top,
      api_grbutton1.Left+api_grbutton1.width,
      api_grbutton1.top+api_grbutton1.Height
      )
    );
end;

procedure TForm1.API_inedit1Hide(Sender: TObject);
begin
  api_grbutton1.Caption:=api_inedit1.Text;
end;

end.
