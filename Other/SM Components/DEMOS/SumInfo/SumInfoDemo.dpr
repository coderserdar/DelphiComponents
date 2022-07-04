program SumInfoDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Scalabium.com: TMSWordDocument component';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
