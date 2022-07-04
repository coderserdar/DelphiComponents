program GTComponentMegaPackDemo;

uses
  Forms,
  f_main in 'Forms\f_main.pas' {FrmMain},
  f_About in 'Forms\f_About.pas' {FrmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GT Component Mega Pack Demo';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
