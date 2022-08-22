program Demo;

uses
  Forms,
  Demomain in 'DEMOMAIN.PAS' {frmDemoTLogFile};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemoTLogFile, frmDemoTLogFile);
  Application.Run;
end.
