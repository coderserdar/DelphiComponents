unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FRMBASICO_REP, users_basic, users_cs, DBCtrls, ExtCtrls, ComCtrls,
  StdCtrls;

type
  Tfrm02 = class(TfrmBasico_Rep)
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioGroup1: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm02: Tfrm02;

implementation

uses Unit1;

{$R *.DFM}

end.
