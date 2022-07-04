unit fmDisplayArchive;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TDisplayArchive = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    BitBtn1: TBitBtn;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DisplayArchive: TDisplayArchive;

implementation

{$R *.DFM}

end.
