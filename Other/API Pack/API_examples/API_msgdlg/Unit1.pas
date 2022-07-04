unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, API_msgdlg, StdCtrls;

type
  TForm1 = class(TForm)
    tAPI_msgdlg1: tAPI_msgdlg;
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  tapi_msgdlg1.Caption:=edit1.text;
  tapi_msgdlg1.Msg.Clear;
  tapi_msgdlg1.Msg.AddStrings(memo1.Lines);
  tapi_msgdlg1.Execute;
end;

end.
