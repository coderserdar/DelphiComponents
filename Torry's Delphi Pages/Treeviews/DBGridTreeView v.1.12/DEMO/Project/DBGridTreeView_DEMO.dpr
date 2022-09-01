program DBGridTreeView_DEMO;

uses
//  QMemory,
  Forms,
  MainForm in 'MainForm.pas' {DBGridTreeViewDemo_MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDBGridTreeViewDemo_MainForm, DBGridTreeViewDemo_MainForm);
  Application.Run;
end.
