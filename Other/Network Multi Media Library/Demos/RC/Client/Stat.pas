unit Stat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmStat = class(TForm)
    GroupBox1: TGroupBox;
    lbDeltaBytesReceived: TLabel;
    lbDeltasReceived: TLabel;
    lbTotalBytesReceived: TLabel;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStat: TfrmStat;

implementation

{$R *.dfm}

end.
