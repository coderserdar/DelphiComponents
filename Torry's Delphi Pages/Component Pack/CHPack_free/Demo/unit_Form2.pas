unit unit_Form2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  CHForm, CHAdvancedLabel;

type
  TfrmCHForm2 = class(TForm)
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
  frmCHForm2: TfrmCHForm2;

implementation

{$R *.DFM}

procedure TfrmCHForm2.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
