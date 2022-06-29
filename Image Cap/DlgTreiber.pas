unit DlgTreiber;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls,videoCap;

type
  TDlgEinstell = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgEinstell: TDlgEinstell;

implementation

{$R *.DFM}

end.
