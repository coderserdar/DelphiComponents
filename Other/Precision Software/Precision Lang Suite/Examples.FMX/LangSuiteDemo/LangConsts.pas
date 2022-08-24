unit LangConsts;

interface

var
  // constants
  MSG_TEST1:string;
  MSG_TEST2:string;

procedure LanguageChanged;

implementation

uses
  FMX.plsLangMan, SysUtils
  {$IF Defined(MSWINDOWS) OR Defined(FPC)}
  , FMX.Consts
  {$IFEND}
  ;

{$IF Defined(MSWINDOWS) OR Defined(FPC)}
var
  // resourcestrings
  pls_FMX_Consts_StrInvalidThePosition : string;
{$IFEND}

procedure LanguageChanged;
begin
  MSG_TEST1:=LanguageManager.LangText('MSG_TEST1'{,'Test message 1'});
  MSG_TEST2:=LanguageManager.LangText('MSG_TEST2'{,'Test message 2'});
  
  {$IF Defined(MSWINDOWS) OR Defined(FPC)}
  // modify resourcestrings
  with LanguageManager do
  begin
    pls_FMX_Consts_StrInvalidThePosition := LangText('@FMX.Consts.StrInvalidThePosition',FMX.Consts.StrInvalidThePosition);
    LangResourceStr(@FMX.Consts.StrInvalidThePosition, PChar(pls_FMX_Consts_StrInvalidThePosition));
  end;
  {$IFEND}
end;

end.
