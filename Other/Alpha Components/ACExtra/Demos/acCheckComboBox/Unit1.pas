unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, sMaskEdit, sCustomComboEdit, sComboEdit,
  aceCheckComboBox, acTitleBar, sSkinManager, ImgList, acAlphaImageList;

type
  TForm1 = class(TForm)
    acCheckComboBox1: TacCheckComboBox;
    sTitleBar1: TsTitleBar;
    sSkinManager1: TsSkinManager;
    sAlphaImageList1: TsAlphaImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
