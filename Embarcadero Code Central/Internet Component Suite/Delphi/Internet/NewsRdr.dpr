program NewsRdr;

uses
  Forms,
  NewsRdr1 in 'NewsRdr1.pas' {NNTPForm};

{$R *.RES}

begin
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
