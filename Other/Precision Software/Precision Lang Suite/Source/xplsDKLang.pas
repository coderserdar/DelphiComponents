{------------------------------------------------------------------------------
  xplsDKLang.pas

  Precision Language Suite

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    Routines for easier migration from DKLang components

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  This unit can be freely used in any application. The complete
  source code remains property of the author and may not be distributed,
  published, given or sold in any form as such. No parts of the source
  code can be included in any other component or application without
  written authorization of the author.

  Copyright (c) 2008 - 2014  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------
  History:

  - Version: 2.5.2
    * added: Support for Delphi XE6 and XE7

  - Version: 2.5.1
    * changed - minor improvements
------------------------------------------------------------------------------}

{ Routines for easier migration from DKLang components }

unit xplsDKLang;

interface

uses
  plsLangMan;

{ You can use this function for easier migration from DKLang engine, because it forwards existing calls of DKLangConstW
  to the LanguageManager.LangText method, which is an equivalent in PLS Engine. }
function DKLangConstW(const wsName: string): string; overload;
{ You can use this function for easier migration from DKLang engine, because it forwards existing calls of DKLangConstW
  to the LanguageManager.LangText method, which is an equivalent in PLS Engine. }
function DKLangConstW(const wsName: string; const aParams: array of const): string; overload;

implementation

uses
  SysUtils;

function DKLangConstW(const wsName: string): string;
begin
  Result := LanguageManager.LangText(wsName);
end;

function DKLangConstW(const wsName: string; const aParams: array of const): string;
begin
  Result := Format(LanguageManager.LangText(wsName), aParams);
end;

end.
