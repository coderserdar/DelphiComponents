program DMProjectReader;

uses
  Forms,
  fMain in 'fMain.pas' {Form1},
  fAbout in 'fAbout.pas' {fmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.Run;
end.
