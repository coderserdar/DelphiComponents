program demo;


uses
  Forms,
  u_components in 'u_components.pas';

{$R *.res}


begin
	Application.Initialize;
	Application.Title := 'Demo';
  Application.CreateForm(TMyform, Myform);
  Application.Run;
end.
