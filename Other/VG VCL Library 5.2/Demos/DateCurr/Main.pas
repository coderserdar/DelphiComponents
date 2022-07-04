unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  vgStndrt, StdCtrls;

type
  TMainForm = class(TForm)
    ds: TDateTimeStorage;
    cs: TCurrencyStorage;
    Memo1: TMemo;
    cmClose: TButton;
    procedure cmCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.cmCloseClick(Sender: TObject);
begin
  Close;
end;

end.
