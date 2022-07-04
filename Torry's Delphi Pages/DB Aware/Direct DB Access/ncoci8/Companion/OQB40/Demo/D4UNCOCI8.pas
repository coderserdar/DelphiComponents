unit D4UNCOCI8;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QBuilder, StdCtrls, ExtCtrls, oqbeNCOCI8, Db, NCOciDB;

type
  TQBDemoForm = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    BtnQBuilder: TButton;
    OQBDialog: TOQBuilderDialog;
    OQBEngineNCOCI8: TOQBEngineNCOCI8;
    OCIDatabase1: TOCIDatabase;
    procedure BtnQBuilderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QBDemoForm: TQBDemoForm;

implementation

{$R *.DFM}

procedure TQBDemoForm.BtnQBuilderClick(Sender: TObject);
begin
  if OQBDialog.Execute then
    Memo.Lines.Assign(OQBDialog.SQL);
end;

end.
