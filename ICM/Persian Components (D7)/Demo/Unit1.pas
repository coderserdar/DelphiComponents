unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, PersianCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn2: TBitBtn;
    PersianCheckBox1: TPersianCheckBox;
    PersianRadioButton2: TPersianRadioButton;
    PersianRadioButton1: TPersianRadioButton;
    PersianEdit1: TPersianEdit;
    PersianMemo1: TPersianMemo;
    PersianLabel1: TPersianLabel;
    PersianLabel2: TPersianLabel;
    PersianButton1: TPersianButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
