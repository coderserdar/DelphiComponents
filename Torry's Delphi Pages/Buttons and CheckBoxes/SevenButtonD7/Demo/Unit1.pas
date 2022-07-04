unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SevenButton, ExtCtrls, pngimage;

type
  TForm1 = class(TForm)
    SevenButton1: TSevenButton;
    SevenButton2: TSevenButton;
    SevenButton3: TSevenButton;
    SevenButton4: TSevenButton;
    procedure SevenButton1Click(Sender: TObject);
    procedure SevenButton2Click(Sender: TObject);
    procedure SevenButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.dfm}

procedure TForm1.SevenButton1Click(Sender: TObject);
var
  wResult: TModalResult;
begin
  wResult := Form2.ShowModal;
  if wResult = mrOk then ShowMessage('ok');
  if wResult = mrCancel then ShowMessage('cancel');
end;


procedure TForm1.SevenButton2Click(Sender: TObject);
begin
  ShowMessage('Hello world !');
end;

procedure TForm1.SevenButton3Click(Sender: TObject);
begin
  Close;
end;

end.

