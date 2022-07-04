unit unit_Form3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CHForm, StdCtrls, CHAdvancedLabel;

type
  TfrmCHForm3 = class(TForm)
    CHForm1: TCHForm;
    Button1: TButton;
    CHAdvancedLabel1: TCHAdvancedLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHForm3: TfrmCHForm3;

implementation

{$R *.DFM}

procedure TfrmCHForm3.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
