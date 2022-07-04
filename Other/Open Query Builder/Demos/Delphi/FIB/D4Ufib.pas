unit D4Ufib;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QBuilder, QBEFIB, ExtCtrls;

type
  TQBDemoForm = class(TForm)
    Panel1: TPanel;
    BtnQBuilderFIB: TButton;
    OQBDialogFIB: TOQBuilderDialog;
    OQBEngineFIB: TOQBEngineFIB;
    Memo: TMemo;
    procedure BtnQBuilderFIBClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QBDemoForm: TQBDemoForm;

implementation

{$R *.DFM}

procedure TQBDemoForm.BtnQBuilderFIBClick(Sender: TObject);
begin
  if OQBDialogFIB.Execute then
    Memo.Lines.Assign(OQBDialogFIB.SQL);
end;

end.
