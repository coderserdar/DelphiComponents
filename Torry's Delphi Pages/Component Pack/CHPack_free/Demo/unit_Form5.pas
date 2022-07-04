unit unit_Form5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CHButton, CHForm, CHAdvancedLabel;

type
  TfrmCHForm5 = class(TForm)
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
  frmCHForm5: TfrmCHForm5;

implementation

{$R *.DFM}

procedure TfrmCHForm5.CHButton1Click(Sender: TObject);
begin
  Close;
end;

end.
