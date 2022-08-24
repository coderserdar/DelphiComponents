unit LangConsts;

interface

var
  STR_LASTOPDURATION : string = 'Last operation duration: %d ms';
  STR_NODECAPTION : string = 'Level %d, Index %d';

procedure LanguageChanged;

implementation

uses
  plsLangMan, SysUtils;

procedure LanguageChanged;
begin
  with LanguageManager do
  begin
    STR_LASTOPDURATION := LangText('STR_LASTOPDURATION',STR_LASTOPDURATION);
    STR_NODECAPTION := LangText('STR_NODECAPTION',STR_NODECAPTION);
  end;
end;

end.
