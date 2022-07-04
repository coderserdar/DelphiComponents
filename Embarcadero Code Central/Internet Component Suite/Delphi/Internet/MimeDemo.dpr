program MimeDemo;

uses
  Forms,
  MimeDmo1 in 'MIMEDMO1.PAS' {MimeDecodeForm};

{$R *.RES}

begin
  Application.CreateForm(TMimeDecodeForm, MimeDecodeForm);
  Application.Run;
end.
