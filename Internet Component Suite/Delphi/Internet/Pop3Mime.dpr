program Pop3Mime;

uses
  Forms,
  Pop3Mim1 in 'POP3MIM1.PAS' {MimeDecodeForm};

{$R *.RES}

begin
  Application.CreateForm(TMimeDecodeForm, MimeDecodeForm);
  Application.Run;
end.
