program StylingDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Demonstration of using the tag cloud stylers';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
