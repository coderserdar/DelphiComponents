program Selfextr;

uses
  Forms,
  Selfextf in 'SELFEXTF.PAS' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
