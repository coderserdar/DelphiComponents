unit unit_Form4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CHForm, CHButton, CHAdvancedLabel;

type
  TfrmCHForm4 = class(TForm)
    CHForm1: TCHForm;
    CHButton1: TCHButton;
    CHAdvancedLabel1: TCHAdvancedLabel;
    procedure CHButton1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHForm4: TfrmCHForm4;

implementation

{$R *.DFM}

procedure TfrmCHForm4.CHButton1Click(Sender: TObject);
begin
  Close;
end;

end.
