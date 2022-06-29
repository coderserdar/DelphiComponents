program twschat;

uses
  Forms,
  TWSChat1 in 'TWSChat1.pas' {TWSChatForm};

{$R *.RES}

begin
  Application.CreateForm(TTWSChatForm, TWSChatForm);
  Application.Run;
end.
