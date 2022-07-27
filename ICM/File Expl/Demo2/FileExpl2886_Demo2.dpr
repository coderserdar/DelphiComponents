program FileExpl2886_Demo2;

uses
  Forms,
  FileExpl2886_Demo2Main in 'FileExpl2886_Demo2Main.pas' {Form1},
  About in 'About.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FileExpl2879_Demo2';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
