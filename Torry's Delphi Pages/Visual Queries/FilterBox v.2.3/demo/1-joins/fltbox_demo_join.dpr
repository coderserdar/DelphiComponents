program fltbox_demo_join;

uses
  Forms,
  fltbox_demo_join_main in 'fltbox_demo_join_main.pas' {MainForm},
  fltbox_demo_join_datastruct in 'fltbox_demo_join_datastruct.pas' {DataStructureForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataStructureForm, DataStructureForm);
  Application.Run;
end.
