unit D4Ubde;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QBuilder, QBEBDE, StdCtrls, ExtCtrls;

type
  TQBDemoForm = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    BtnQBuilderBDE: TButton;
    OQBDialogBDE: TOQBuilderDialog;
    OQBEngineBDE: TOQBEngineBDE;
    procedure BtnQBuilderBDEClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QBDemoForm: TQBDemoForm;

implementation

{$R *.DFM}

procedure TQBDemoForm.BtnQBuilderBDEClick(Sender: TObject);
begin
  if OQBDialogBDE.Execute then
    Memo.Lines.Assign(OQBDialogBDE.SQL);
end;

end.
