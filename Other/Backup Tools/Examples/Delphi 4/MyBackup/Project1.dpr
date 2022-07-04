program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  fmBrowseDialog in 'fmBrowseDialog.pas' {BrowseDialog},
  fmSelectFiles in 'fmSelectFiles.pas' {SelectFiles},
  MyArchBackup in '..\..\..\Components\MyArchBackup.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TBrowseDialog, BrowseDialog);
  Application.CreateForm(TSelectFiles, SelectFiles);
  Application.Run;
end.
