unit D4Uibo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QBuilder, QBEIBO, ExtCtrls;

type
  TQBDemoForm = class(TForm)
    Panel1: TPanel;
    BtnQBuilderIBO: TButton;
    OQBDialogIBO: TOQBuilderDialog;
    OQBEngineIBO: TOQBEngineIBO;
    Memo: TMemo;
    procedure BtnQBuilderIBOClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QBDemoForm: TQBDemoForm;

implementation

{$R *.DFM}

procedure TQBDemoForm.BtnQBuilderIBOClick(Sender: TObject);
begin
  if OQBDialogIBO.Execute then
    Memo.Lines.Assign(OQBDialogIBO.SQL);
end;

end.
