unit LangConsts;

interface

var
  // constants
  MSG_TEST1:string;
  MSG_TEST2:string;

procedure LanguageChanged;

implementation

uses
  plsLangMan, SysUtils, rtlconsts;

procedure LanguageChanged;
begin
  MSG_TEST1:=LanguageManager.LangText('MSG_TEST1'{,'Test message 1'});
  MSG_TEST2:=LanguageManager.LangText('MSG_TEST2'{,'Test message 2'});

  // modify resourcestrings
  LanguageManager.LangResourceStr(@rtlconsts.SSelectDirCap, PChar(LanguageManager.LangText('@rtlconsts.SSelectDirCap')));
end;

end.
