unit LangConsts;

interface

var
  MSG_TEST1:string;
  MSG_TEST2:string;

procedure LanguageChanged;

implementation

uses
  plsLangMan;

procedure LanguageChanged;
begin
  MSG_TEST1:=LanguageManager.LangText('MSG_TEST1'{,'Test message 1'});
  MSG_TEST2:=LanguageManager.LangText('MSG_TEST2'{,'Test message 2'});
end;

end.
