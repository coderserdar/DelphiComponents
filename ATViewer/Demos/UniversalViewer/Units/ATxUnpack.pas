{
Code from TC Plugins Manager project:
http://www.totalcmd.net/plugring/tc_plugman.html
}

{$I-}

unit ATxUnpack;

interface

function FExecProcess(const cmd: string; ShowCmd: integer; DoWait: boolean): boolean;
function FUnpack(const fn, sdir: string): boolean;

var
  OptUnpack: record
    RarPath: string;
    RunMinimized: boolean;
  end;


implementation

uses
  Windows, SysUtils, ShellAPI,
  ATxUtils;

const
  cShowCmd: array[boolean] of DWORD = (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE);

//-------------------------------------------------
function FExecShell(const cmd, params, dir: string; ShowCmd: integer; DoWait: boolean): boolean;
var
  si: TShellExecuteInfo;
begin
  FillChar(si, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  si.lpFile:= PChar(cmd);
  si.lpParameters:= PChar(params);
  si.lpDirectory:= PChar(dir);
  si.nShow:= ShowCmd;
  Result:= ShellExecuteEx(@si);
  if Result and DoWait then
    WaitForSingleObject(si.hProcess, INFINITE);
  CloseHandle(si.hProcess);
end;

//-------------------------------------------------
function FExecProcess(const cmd: string; ShowCmd: integer; DoWait: boolean): boolean;
var
  pi: TProcessInformation;
  si: TStartupInfo;
begin
  FillChar(pi, SizeOf(pi), 0);
  FillChar(si, SizeOf(si), 0);
  si.cb:= SizeOf(si);
  si.dwFlags:= STARTF_USESHOWWINDOW;
  si.wShowWindow:= ShowCmd;

  Result:= CreateProcess(nil, PChar(cmd), nil, nil, false, 0,
    nil, nil, si, pi);
  if Result then
    begin
    if DoWait then WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    end;
end;

//-------------------------------------------------
function ExecShell(const cmd, params: string): boolean;
begin
  Result:= FExecShell(cmd, params, '', cShowCmd[OptUnpack.RunMinimized], true);
end;

function Exec(const cmd: string): boolean;
begin
  Result:= FExecProcess(cmd, cShowCmd[OptUnpack.RunMinimized], true);
end;

//-------------------------------------------------
function UnZip(const fn, sdir: string): boolean;
var
  param: string;
begin
  Result:= true;

  param:= Format('x -y "%s" "%s\"', [fn, sdir]);

  if ExecShell('WinRAR.exe', param) then Exit;

  if (OptUnpack.RarPath<>'') and
    ExecShell(SExpandVars(OptUnpack.RarPath), param) then Exit;

  if Exec(Format('unzip.exe -o "%s" -d "%s"', [fn, sdir])) then Exit;
  if Exec(Format('"%s\unzip.exe" -o "%s" -d "%s"', [ExtractFileDir(ParamStr(0)), fn, sdir])) then Exit;
  if Exec(Format('pkunzip.exe -d -o "%s" "%s\"', [fn, sdir])) then Exit;
  if Exec(Format('wzunzip.exe -d -o "%s" "%s\"', [fn, sdir])) then Exit;
  if Exec(Format('pkzipc.exe -ext -dir -over=all "%s" "%s\"', [fn, sdir])) then Exit;

  Result:= false;
end;


//-------------------------------------------------
function UnRar(const fn, sdir: string): boolean;
var
  param: string;
begin
  Result:= true;

  param:= Format('x -y "%s" "%s\"', [fn, sdir]);

  if ExecShell('WinRAR.exe', param) then Exit;

  if (OptUnpack.RarPath<>'') and
    ExecShell(SExpandVars(OptUnpack.RarPath), param) then Exit;

  if Exec('Rar.exe '+param) then Exit;
  if Exec('UnRar.exe '+param) then Exit;
  if Exec('"'+ExtractFileDir(ParamStr(0))+'\UnRar.exe" '+param) then Exit;

  Result:= false;
end;


//-------------------------------------------------
function FUnpack(const fn, sdir: string): boolean;
var
  s: string;
begin
  s := LowerCase(ExtractFileExt(fn));
  if s = '.zip' then Result := UnZip(fn, sdir) else
   if s = '.rar' then Result := UnRar(fn, sdir) else
    Result := false;

  if Result then
    Sleep(200);
end;


end.
