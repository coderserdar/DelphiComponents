unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FRMBASICO_REP, users_basic, users_cs, DBCtrls, ExtCtrls, ComCtrls,
  Buttons, StdCtrls;

type
  Tfrm01 = class(TfrmBasico_Rep)
    Memo1: TMemo;
    Button1: TButton;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    BitBtn1: TBitBtn;
    SpeedButton1: TSpeedButton;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm01: Tfrm01;

implementation

uses Unit1;

{$R *.DFM}

end.
