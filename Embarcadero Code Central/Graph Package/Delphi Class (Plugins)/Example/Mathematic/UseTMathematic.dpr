program UseTMathematic;

uses
  Forms,
  FormMathematic in 'FormMathematic.pas' {Form1},
  FormAbout in 'FormAbout.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
