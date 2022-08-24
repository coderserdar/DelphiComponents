unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DecoProgressBar, ExtCtrls;

type
  TfrmMain = class(TForm)
    DecoProgressBar1: TDecoProgressBar;
    Timer1: TTimer;
    DecoProgressBar2: TDecoProgressBar;
    DecoProgressBar3: TDecoProgressBar;
    DecoProgressBar4: TDecoProgressBar;
    DecoProgressBar5: TDecoProgressBar;
    DecoProgressBar6: TDecoProgressBar;
    DecoProgressBar7: TDecoProgressBar;
    DecoProgressBar8: TDecoProgressBar;
    DecoProgressBar9: TDecoProgressBar;
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
  PB:TDecoProgressBar;
begin
  for i:=0 To ComponentCount-1 do
    if Components[i] is TDecoProgressBar then
    begin
      PB:=TDecoProgressBar(Components[i]);
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
