program MailRob;

uses
  Forms,
  MailRob1 in 'MailRob1.pas' {MailRobForm};

{$R *.RES}

begin
  Application.CreateForm(TMailRobForm, MailRobForm);
  Application.Run;
end.
