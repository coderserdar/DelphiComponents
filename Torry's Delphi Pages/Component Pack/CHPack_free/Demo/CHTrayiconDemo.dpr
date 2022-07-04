program CHTrayiconDemo;

uses
  Forms,
  CHDemoTrayicon in 'CHDemoTrayicon.pas' {frmTrayiconDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTrayiconDemo, frmTrayiconDemo);
  Application.Run;
end.
