unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_processlimit, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    tAPI_processlimit1: tAPI_processlimit;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure tAPI_processlimit1Exists(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if tAPI_processlimit1.Exists then
    label1.Caption:='There is others' else
    label1.caption:='Running alone';
end;

procedure TForm1.tAPI_processlimit1Exists(Sender: TObject);
begin
  application.Terminate;
end;

end.
