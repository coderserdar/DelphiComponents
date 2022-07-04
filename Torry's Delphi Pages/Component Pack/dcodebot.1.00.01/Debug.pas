
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Debug;

interface

{$I STD.INC}

uses
  Windows{$IFDEF DEBUGMODE}, SysUtils{$ENDIF};

{$IFDEF DEBUGMODE}
procedure Write(const S: string);
{$ENDIF}

implementation

{$IFDEF DEBUGMODE}
procedure Write(const S: string);
var
  FileName: string;
  DebugFile: TextFile;
begin
  SetLength(FileName, MAX_PATH);
  GetCurrentDirectory(MAX_PATH, PChar(FileName));
  FileName := IncludeTrailingBackslash(PChar(FileName)) + 'debug.txt';
  AssignFile(DebugFile, FileName);
  if FileExists(FileName) then
    Append(DebugFile)
  else
    Rewrite(DebugFile);
  try
    WriteLn(DebugFile, FormatDateTime('[mm/dd/yyy hh:mm:ss] ', Now) + S);
  finally
    CloseFile(DebugFile);
  end;
end;
{$ENDIF}

end.
