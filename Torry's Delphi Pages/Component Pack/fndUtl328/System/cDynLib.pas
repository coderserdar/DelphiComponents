{$INCLUDE ..\cDefines.inc}
unit cDynLib;

{                                                                              }
{                    Dynamically Loaded Libraries 3.02                         }
{                                                                              }
{             This unit is copyright © 2001-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                    Its original file name is cDynLib.pas                     }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{ Revision history:                                                            }
{   2001/06/26  1.01  Added TDynamicLibrary.                                   }
{   2002/06/06  3.02  Created cDynLib unit from cSysUtils.                     }
{                                                                              }

interface

uses
  { Delphi }
  Windows,
  SysUtils;



{                                                                              }
{ DLL functions                                                                }
{                                                                              }
type
  EDynamicLibrary = class (Exception);

function  LoadDLL(const FileName: String): HMODULE;
procedure UnloadDLL(const Handle: HMODULE);
function  LoadAnyDLL(const FileNames: Array of String): HMODULE;
function  GetDLLProcAddress(const Handle: HMODULE; const ProcName: String): FARPROC;



{                                                                              }
{ TDynamicLibrary                                                              }
{                                                                              }
type
  TDynamicLibrary = class
  protected
    FHandle : HMODULE;

    function  GetProcAddress(const ProcName: String): FARPROC;

  public
    constructor Create(const FileName: String); overload;
    constructor Create(const FileNames: Array of String); overload;
    destructor  Destroy; override;

    property  Handle: HMODULE read FHandle;
    property  ProcAddress[const ProcName: String]: FARPROC read GetProcAddress; default;
  end;



implementation



{                                                                              }
{ Dynamic Libraries                                                            }
{                                                                              }
function DoLoadLibrary(const FileName: String): HMODULE;
begin
  Result := LoadLibrary(PChar(FileName));
end;

function LoadDLL(const FileName: String): HMODULE;
begin
  Result := DoLoadLibrary(FileName);
  if Result <= HINSTANCE_ERROR then
    raise EDynamicLibrary.Create('Could not load DLL: ' + FileName + ': Error #' +
                          IntToStr(GetLastError));
end;

function LoadAnyDLL(const FileNames: Array of String): HMODULE;
var I : Integer;
begin
  For I := 0 to Length(FileNames) - 1 do
    begin
      Result := DoLoadLibrary(FileNames [I]);
      if Result <> 0 then
        exit;
    end;
  raise EDynamicLibrary.Create('Could not load DLLs: Error #' +
                        IntToStr(GetLastError));
end;

procedure UnloadDLL(const Handle: HMODULE);
begin
  FreeLibrary(Handle);
end;

function GetDLLProcAddress(const Handle: HMODULE; const ProcName: String): FARPROC;
begin
  Result := GetProcaddress(Handle, PChar(ProcName));
  if Result = nil then
    raise EDynamicLibrary.Create('Could not import function from DLL: ' + ProcName +
                          ': Error #' + IntToStr(GetLastError));
end;



{                                                                              }
{ TDynamicLibrary                                                              }
{                                                                              }
constructor TDynamicLibrary.Create(const FileName: String);
begin
  inherited Create;
  FHandle := LoadDLL(FileName);
  Assert(FHandle <> 0, 'FHandle <> 0');
end;

constructor TDynamicLibrary.Create(const FileNames: Array of String);
begin
  inherited Create;
  FHandle := LoadAnyDLL(FileNames);
  Assert(FHandle <> 0, 'FHandle <> 0');
end;

destructor TDynamicLibrary.Destroy;
begin
  if FHandle <> 0 then
    begin
      UnloadDLL(FHandle);
      FHandle := 0;
    end;
  inherited Destroy;
end;

function TDynamicLibrary.GetProcAddress(const ProcName: String): FARPROC;
begin
  Assert(FHandle <> 0, 'FHandle <> 0');
  Result := GetDLLProcAddress(FHandle, ProcName);
end;



end.

