{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgTlHelp unit.                                }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$IFDEF _D3_}
{$WEAKPACKAGEUNIT}
{$ENDIF _D3_}
{$D-,L-}

unit vgTlHelp;

interface
uses Windows, Classes;

{ ToolHelp }
procedure GetProcessList(List: TStrings);
procedure GetModuleList(List: TStrings);
procedure GetParentProcessInfo(var ID: DWORD; var Path: String);

implementation
uses TlHelp32, SysUtils, vgUtils;

{ ToolHelp }
procedure GetProcessList(List: TStrings);
var
  I: Integer;
  hSnapshoot: THandle;
  pe32: TProcessEntry32;
begin
  List.Clear;
  hSnapshoot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if (hSnapshoot = THandle(-1)) then Exit;
  
  pe32.dwSize := SizeOf(TProcessEntry32);
  if (Process32First(hSnapshoot, pe32)) then
  repeat
    I := List.Add(Format('%x, %x: %s',
      [pe32.th32ProcessID, pe32.th32ParentProcessID, pe32.szExeFile]));
    List.Objects[I] := Pointer(pe32.th32ProcessID);
  until not Process32Next(hSnapshoot, pe32);

  CloseHandle (hSnapshoot);
end;

procedure GetModuleList(List: TStrings);
var
  I: Integer;
  hSnapshoot: THandle;
  me32: TModuleEntry32;
begin
  List.Clear;
  hSnapshoot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
  if (hSnapshoot = THandle(-1)) then Exit;
  me32.dwSize := SizeOf(TModuleEntry32);
  if (Module32First(hSnapshoot, me32)) then
  repeat
    I := List.Add(me32.szModule);
    List.Objects[I] := Pointer(me32.th32ModuleID);
  until not Module32Next(hSnapshoot, me32);

  CloseHandle (hSnapshoot);
end;

procedure GetParentProcessInfo(var ID: DWORD; var Path: String);
var
  ProcessID: DWORD;
  hSnapshoot: THandle;
  pe32: TProcessEntry32;
begin
  ProcessID := GetCurrentProcessID;
  ID := DWORD(-1);
  Path := '';

  hSnapshoot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if (hSnapshoot = THandle(-1)) then Exit;

  pe32.dwSize := SizeOf(TProcessEntry32);
  if (Process32First(hSnapshoot, pe32)) then
  repeat
    if pe32.th32ProcessID = ProcessID then
    begin
      ID := pe32.th32ParentProcessID;
      Break;
    end;
  until not Process32Next(hSnapshoot, pe32);

  if ID <> DWORD(-1) then
  begin
    if (Process32First(hSnapshoot, pe32)) then
    repeat
      if pe32.th32ProcessID = ID then
      begin
        Path := pe32.szExeFile;
        Break;
      end;
    until not Process32Next(hSnapshoot, pe32);
  end;
  CloseHandle (hSnapshoot);
end;

end.
