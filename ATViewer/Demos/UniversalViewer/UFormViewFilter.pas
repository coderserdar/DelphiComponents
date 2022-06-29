unit UFormViewFilter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormViewFilter = class(TForm)
    CheckBox1: TCheckBox;
    Bevel1: TBevel;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormViewFilter: TFormViewFilter;

implementation

{$R *.dfm}

end.
