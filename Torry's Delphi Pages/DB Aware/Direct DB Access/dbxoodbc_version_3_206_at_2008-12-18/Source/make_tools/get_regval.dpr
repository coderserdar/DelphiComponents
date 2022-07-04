// (c) "Vadim V.Lopushansky"<pult@ukr.net>, Kiev 2002 Year.
// Version 1.04

program get_regval;

{$APPTYPE CONSOLE}
{$B-}
//{$O-,D+}
{$O+,D-,R-}

uses
  SysUtils,
  Windows,
  Classes,
  Registry;

{-------------------------------------------------------------------------------------------------}

const
  sVersion: string = '2007.06.14';

{ Relocation info stripped from file. }
const
  IMAGE_FILE_RELOCS_STRIPPED = $0001;
{$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}

{-------------------------------------------------------------------------------------------------}

var
  R: TRegistry;
  LArgs: TStringList;
  sRes: string;
  i: Integer;

function PathGetShortName(const Path: string): string;
var
  Required: Integer;
begin
  Result := Path;
  Required := GetShortPathName(PChar(Path), nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    Required := GetShortPathName(PChar(Path), PChar(Result), Required);
    if (Required <> 0) and (Required = Length(Result) - 1) then
      SetLength(Result, Required)
    else
      Result := Path;
  end;
end;

begin
  // Insert user code here
  if (ParamCount = 0) or (not ParamCount in [3, 4, 5]) or (ParamStr(1) = '/?') then
  begin
    WriteLn('"get_regval.exe" utility version: "' + sVersion + '".');
    WriteLn('Usage As:');
    WriteLn(' Read string registry value from HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE');
    WriteLn('    >get_regval.exe   (HKCU|HKLM)   REGISTRY_PATH   REGICTRY_KEY   [switches]');
    WriteLn(' switches:');
    WriteLn('    /p - remove last splash');
    WriteLn('    /s - convert result path (file system) to short path');
    WriteLn('    /set - print cmd command');
    WriteLn('  examples: ');
    WriteLn('    >get_regval.exe HKCU \Identities "Default User ID"');
    WriteLn('    >get_regval.exe HKCU \Software\Borland\Delphi\7.0 RootDir');
    WriteLn('    >get_regval.exe HKCU \Software\Borland\Delphi\7.0 RootDir /p /s');
    WriteLn;
    WriteLn(' You can replace blank symbol " " to two symbols "\\"');
    WriteLn('  example: ');
    WriteLn('    >get_regval.exe HKCU \Identities Default\\User\\ID');
    Halt(1);
  end;

  LArgs := nil;
  R := TRegistry.Create(KEY_READ);
  try
    LArgs := TStringList.Create;
    LArgs.BeginUpdate;
    for i := 4 to ParamCount do
    begin
      LArgs.Add(LowerCase(ParamStr(i)));
    end;
    LArgs.EndUpdate;

    if UpperCase(ParamStr(1)) = 'HKCU' then
      R.RootKey := HKEY_CURRENT_USER
    else if UpperCase(ParamStr(1)) = 'HKLM' then
      R.RootKey := HKEY_LOCAL_MACHINE
    else
      Halt(4);

    if not R.OpenKeyReadOnly(StringReplace(ParamStr(2), '\\', ' ', [rfReplaceAll])) then
    begin
        //WriteLn('ERROR: Cannot open registry key: HKEY_CURRENT_USER\Identities');
      Halt(2);
    end;

    sRes := R.ReadString(StringReplace(ParamStr(3), '\\', ' ', [rfReplaceAll]));
    if (Length(sRes) > 0) and (sRes[Length(sRes)] = '\') and (LArgs.IndexOf('/p') >= 0) then
    begin
      SetLength(sRes, Length(sRes) - 1);
    end;

    if (Length(sRes) > 0) and (LArgs.IndexOf('/s') >= 0) then
    begin
      sRes := PathGetShortName(sRes);
    end;

    if LArgs.IndexOf('/set') >= 0 then
    begin
      WriteLn('set REGVAL=' + sRes);
    end
    else
    begin
      WriteLn(sRes);
    end;

    FreeAndNil(R);
    FreeAndNil(LArgs);

  except
    on e: Exception do
    begin
          //WriteLn('ERROR: '+e.Message);
      if (LowerCase(ParamStr(4)) = '/set') or (LowerCase(ParamStr(5)) = '/set') then
      begin
        WriteLn('set REGVAL=');
      end;
      R.Free;
      LArgs.Free;
      Halt(3);
    end;
  end;
end.
