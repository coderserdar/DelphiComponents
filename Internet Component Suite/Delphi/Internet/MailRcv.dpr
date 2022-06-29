program MailRcv;

uses
  Forms,
  MailRcv1 in 'MailRcv1.pas' {POP3ExcercizerForm},
  MailRcv2 in 'MailRcv2.pas' {MessageForm};

{$R *.RES}

begin
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.
