program LoginEvents;

uses
  Forms,
  main in 'main.pas' {Form1},
  splash in 'splash.pas' {frmSplash};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
