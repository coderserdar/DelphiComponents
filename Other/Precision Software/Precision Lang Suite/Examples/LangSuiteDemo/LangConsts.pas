unit LangConsts;

interface

var
  // constants
  MSG_TEST1:string;
  MSG_TEST2:string;

procedure LanguageChanged;

implementation

uses
  plsLangMan, SysUtils, Consts;

var
  // resourcestrings
  pls_consts_SSelectDirCap:string;

procedure LanguageChanged;
begin
  MSG_TEST1:=LanguageManager.LangText('MSG_TEST1'{,'Test message 1'});
  MSG_TEST2:=LanguageManager.LangText('MSG_TEST2'{,'Test message 2'});

  // modify resourcestrings
  pls_consts_SSelectDirCap:=LanguageManager.LangText('@consts.SSelectDirCap');
  LanguageManager.LangResourceStr(@consts.SSelectDirCap, PChar(pls_consts_SSelectDirCap));

end;

end.
