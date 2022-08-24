unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DecoProgressBarPlus, DecoProgressBar, ExtCtrls;

type
  TfrmMain = class(TForm)
    Timer1: TTimer;
    DecoProgressBar1: TDecoProgressBarPlus;
    DecoProgressBar2: TDecoProgressBarPlus;
    DecoProgressBar3: TDecoProgressBarPlus;
    DecoProgressBar4: TDecoProgressBarPlus;
    DecoProgressBar5: TDecoProgressBarPlus;
    DecoProgressBar6: TDecoProgressBarPlus;
    DecoProgressBar7: TDecoProgressBarPlus;
    DecoProgressBar8: TDecoProgressBarPlus;
    DecoProgressBar9: TDecoProgressBarPlus;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateBars;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled:=False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  timer1.Enabled:=True;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled:=False;
  UpdateBars;
  timer1.Enabled:=True;
end;

procedure TfrmMain.UpdateBars;
var
  i:Integer;
  PB:TDecoProgressBarPlus;
begin
  for i:=0 To ComponentCount-1 do
    if Components[i] is TDecoProgressBarPlus then
    begin
      PB:=TDecoProgressBarPlus(Components[i]);
      if PB.Tag=0 then
      begin
        if PB.Position<PB.MaxValue then
          PB.Position:=PB.Position+1
        else
          PB.Position:=PB.MinValue;
      end;
    end;
end;

end.
