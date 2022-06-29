unit ATxShellExtension;

interface

function ApplyShellExtension(AEnable: boolean): boolean;
function IsShellExtensionEnabled: boolean;


implementation

uses
  Windows, ATxSProc, ATxParamStr, ATxRegistry;

const
  RegKey0 = '*\shell\Universal Viewer';
  RegKey1 = RegKey0 + '\command';
  RegKeyF0 = 'Directory\shell\Universal Viewer';
  RegKeyF1 = RegKeyF0 + '\command';

//Files
function ShellValue: WideString;
begin
  Result:= SFormatW('"%s" "%1"', [SParamExe]);
end;

//Folders
function ShellValueF: WideString;
begin
  Result:= SFormatW('"%s" "@@%1"', [SParamExe]);
end;

function ApplyShellExtension(AEnable: boolean): boolean;
begin
  if AEnable then
    begin
    Result:=
      SetRegKeyStr(HKEY_CLASSES_ROOT, RegKey1, '', ShellValue) and
      SetRegKeyStr(HKEY_CLASSES_ROOT, RegKeyF1, '', ShellValueF);
    end
  else
    begin
    Result:=
      (RegDeleteKey(HKEY_CLASSES_ROOT, RegKey1) = ERROR_SUCCESS) and
      (RegDeleteKey(HKEY_CLASSES_ROOT, RegKey0) = ERROR_SUCCESS) and
      (RegDeleteKey(HKEY_CLASSES_ROOT, RegKeyF1) = ERROR_SUCCESS) and
      (RegDeleteKey(HKEY_CLASSES_ROOT, RegKeyF0) = ERROR_SUCCESS);
    end;
end;

function IsShellExtensionEnabled: boolean;
begin
  Result:=
    (SCompareIW(GetRegKeyStr(HKEY_CLASSES_ROOT, RegKey1, '', ''), ShellValue) = 0) and
    (SCompareIW(GetRegKeyStr(HKEY_CLASSES_ROOT, RegKeyF1, '', ''), ShellValueF) = 0);
end;


end.
