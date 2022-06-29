unit UFormViewGoto;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormViewGoto = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    boxPosition: TGroupBox;
    edPos: TEdit;
    chkPercent: TRadioButton;
    chkHex: TRadioButton;
    chkDec: TRadioButton;
    chkSelStart: TRadioButton;
    chkSelEnd: TRadioButton;
    chkLine: TRadioButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ATxMsgProc, ATxUtils;

{$R *.DFM}

procedure TFormViewGoto.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewGoto.inc}
end;

end.
