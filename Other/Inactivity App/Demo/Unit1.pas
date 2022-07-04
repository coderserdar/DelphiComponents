unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CInactivityApp, StdCtrls;

type
  TForm1 = class(TForm)
    InactivityApp1: TInactivityApp;
    lblProceso: TLabel;
    Button1: TButton;
    lblState: TLabel;
    mmLog: TMemo;
    lbl1: TLabel;
    lbl2: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure InactivityApp1ResetTimer(Sender: TObject);
    procedure InactivityApp1InactivityStep(Sender: TObject);
    procedure InactivityApp1InactivityComplete(Sender: TObject);
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
  // Cambio de estado
  InactivityApp1.Active := not InactivityApp1.Active;

  // Estado.
  if (InactivityApp1.Active) then begin
    lblState.Caption := 'Activado';
    lblState.Font.Color := clGreen;
  end
  else begin
    lblState.Caption := 'Descactivado';
    lblState.Font.Color := clred;
  end;

  // Limpiar el log...
  mmLog.Clear;

end;

procedure TForm1.InactivityApp1ResetTimer(Sender: TObject);
begin

  lblProceso.Caption := '';
  mmLog.Lines.Add('ResetTimer');

end;

procedure TForm1.InactivityApp1InactivityStep(Sender: TObject);
begin
  lblProceso.Caption := lblProceso.Caption + '.';
  mmLog.Lines.Add('InactivityStep');

  Label1.Caption := IntToStr(InactivityApp1.__TotalInactivity);
  Label2.Caption := IntToStr(InactivityApp1.__DifInactivity);
end;

procedure TForm1.InactivityApp1InactivityComplete(Sender: TObject);
begin
  lblProceso.Caption := 'Inactivo!!';
  mmLog.Lines.Add('InactivityComplete');
end;

end.
