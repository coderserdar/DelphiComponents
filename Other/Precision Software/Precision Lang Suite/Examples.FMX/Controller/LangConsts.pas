unit LangConsts;

interface

var
  str_test_msg : string = 'Test message';

procedure LanguageChanged;

implementation

uses
  FMX.plsLangMan;

procedure LanguageChanged;
begin
  with LanguageManager do
  begin
    str_test_msg := LangText('str_test_msg',str_test_msg);
  end;
end;

end.
