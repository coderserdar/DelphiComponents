unit fmOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TOptions = class(TForm)
    cbLanguage: TComboBox;
    Label1: TLabel;
    cbEncrypt: TCheckBox;
    BitBtn1: TBitBtn;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Options: TOptions;

implementation

{$R *.DFM}

end.
