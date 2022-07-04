unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_gradient, StdCtrls, API_base, API_graphics;

type
  TForm1 = class(TForm)
    API_gradient1: TAPI_gradient;
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  api_gradient1.GradientStyle:= TGradientStyle(combobox1.itemindex);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  combobox1.itemindex:= 2;
  combobox1change(self);
end;

end.
