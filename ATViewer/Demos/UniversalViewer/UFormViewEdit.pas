unit UFormViewEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls;

type
  TFormViewEdit = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edString: TTntEdit;
    labCaption: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ATxMsgProc;

{$R *.dfm}

procedure TFormViewEdit.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewEdit.inc}
end;

end.
