unit LangConsts;

interface

var
  MSG_TEST1 : string = 'Test message 1';
  MSG_TEST2 : string = 'Test message 2';

procedure LanguageChanged;

implementation

uses
  plsLangMan, SysUtils, rtlconsts;

procedure LanguageChanged1;
begin
  with LanguageManager do
  begin
    MSG_TEST1 := LangText('MSG_TEST1',MSG_TEST1);
    MSG_TEST2 := LangText('MSG_TEST2',MSG_TEST2);
  end;
end;

procedure LanguageChangedRes1;
begin
  with LanguageManager do
  begin
    LangResourceStr(@rtlconsts.SSelectDirCap, PChar(LangText('@rtlconsts.SSelectDirCap',rtlconsts.SSelectDirCap)));
  end;
end;

procedure LanguageChanged;
begin
  LanguageChanged1;
  LanguageChangedRes1;
end;

end.
