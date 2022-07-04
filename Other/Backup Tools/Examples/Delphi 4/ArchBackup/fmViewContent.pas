unit fmViewContent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TViewContent = class(TForm)
    Panel1: TPanel;
    lbFiles: TListBox;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewContent: TViewContent;

implementation

{$R *.DFM}

end.
