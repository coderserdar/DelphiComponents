program PopTst;

uses
  Forms,
  PopTst1 in 'PopTst1.pas' {POP3ExcercizerForm},
  PopTst2 in 'PopTst2.pas' {MessageForm};

{$R *.RES}

begin
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.
