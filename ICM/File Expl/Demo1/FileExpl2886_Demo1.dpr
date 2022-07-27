program FileExpl2886_Demo1;

uses
  Forms,
  FileExpl2886_Demo1Main in 'FileExpl2886_Demo1Main.pas' {MainForm},
  About in 'About.pas' {AboutForm},
  FileDate in 'FileDate.pas' {FileDateForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'LsFileExplorer28 Demo1';
  Application.CreateForm(TMainForm1, MainForm1);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TFileDateForm, FileDateForm);
  Application.Run;
end.
