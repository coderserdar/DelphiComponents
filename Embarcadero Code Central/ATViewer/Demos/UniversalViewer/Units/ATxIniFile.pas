{
TATIniFile is a helper class that:
 - Inherits from TMemIniFile (recommended) or TIniFile
 - When inherits from TMemIniFile, it removes double-quotes from strings on reading
 - Should NOT be used for saving

TATIniFileSave is another helper class that:
 - Should be used for saving
 - Writes strings with double-quotes, when needed
}

{$I ViewerOptions.inc}

unit ATxIniFile;

interface

uses
  IniFiles;

type
  TATIniFile = class({$ifdef MEMINI} TMemIniFile {$else} TIniFile {$endif})
  public
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure DeleteKey(const Section, Ident: string); override;
    {$ifdef MEMINI}
    function ReadString(const Section, Ident, Default: string): string; override;
    {$endif}
  end;

type
  TATIniFileSave = class(TIniFile)
  public
    procedure WriteString(const Section, Ident, Value: string); override;
  end;


implementation

const
  cQuote = '"';

function SQuoted(const S: string): boolean;
begin
  Result := (Length(S) >= 2) and
    (S[1] = cQuote) and (S[Length(S)] = cQuote);
end;

procedure SQuoteIfNeeded(var S: string);
const
  cChars = [cQuote, ' ', #9];
begin
  if (S <> '') then
    if (S[1] in cChars) or (S[Length(S)] in cChars) then
      S := cQuote + S + cQuote;
end;

procedure TATIniFile.WriteString(const Section, Ident, Value: string);
begin
  Assert(False, 'TATIniFile.WriteString should not be used');
end;

procedure TATIniFile.DeleteKey(const Section, Ident: string);
begin
  Assert(False, 'TATIniFile.DeleteKey should not be used');
end;

{$ifdef MEMINI}
function TATIniFile.ReadString(const Section, Ident, Default: string): string;
begin
  Result := inherited ReadString(Section, Ident, Default);
  if SQuoted(Result) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;
{$endif}

procedure TATIniFileSave.WriteString(const Section, Ident, Value: string);
var
  S: string;
begin
  S := Value;
  SQuoteIfNeeded(S);
  try
    inherited WriteString(Section, Ident, S);
  except
    //except for RO files
  end
end;


end.
