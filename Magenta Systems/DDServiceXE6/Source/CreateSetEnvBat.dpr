program CreateSetEnvBat;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Registry, Classes;

type
  TCompilerVersion = (cv5, cv7, cv2006, cv2007, cv2009, cv2010, cvXE, cvXE2, cvXE3, cvXE4, cvXE5, cvXE6);

const
  RegKeys : array[TCompilerVersion] of string = (
    'Software\Borland\Delphi\5.0',
    'Software\Borland\Delphi\7.0',
    //'Software\Borland\Delphi\8.0',
    //'Software\Borland\BDS\3.0',     //2005
    'Software\Borland\BDS\4.0',       //2006
    'Software\Borland\BDS\5.0',       //2007
    'Software\CodeGear\BDS\6.0',      //2009
    'Software\CodeGear\BDS\7.0',      //2010
    'Software\Embarcadero\BDS\8.0',   //XE
    'Software\Embarcadero\BDS\9.0',   //XE2 
    'Software\Embarcadero\BDS\10.0',  //XE3
    'Software\Embarcadero\BDS\11.0',  //XE4
    'Software\Embarcadero\BDS\12.0',  //XE5
    'Software\Embarcadero\BDS\14.0'); //XE6
  sCompilerRoot = 'CompilerRoot';
  sCompilerVersion = 'CompilerVersion';
  sSvcMgrPas = 'SvcMgrPas';

  procedure SetPathAndVersion;
  var
    CompilerFound: Boolean;
    EnvList: TStringList;
    ShortPath: string;
    LongPath: string;
    I: TCompilerVersion;
    dwRes: DWORD;
  begin
    CompilerFound := GetEnvironmentVariable(PChar(sCompilerRoot), nil, 0) <> 0;
    if not CompilerFound then
    begin
      EnvList := TStringList.Create;
      try
        with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          for I := High(RegKeys) downto Low(RegKeys) do
          begin
            if OpenKeyReadOnly(RegKeys[I]) and ValueExists('RootDir') then
            begin
              LongPath := ReadString('RootDir');
              SetLength(ShortPath, Max_Path);
              dwRes := GetShortPathName(PChar(LongPath), PChar(ShortPath), Max_Path);
              if (dwRes > 0) and (dwRes <= Max_Path) then
                  SetLength(ShortPath, dwRes)
              else
                  ShortPath := LongPath;
              ShortPath := IncludeTrailingPathDelimiter(ShortPath);
              EnvList.Add('SET ' + sCompilerRoot + '=' + ShortPath);
              case I of
                cv5, cv7, cvXE:
                  EnvList.Add('SET ' + sSvcMgrPas + '=' + ShortPath +
                              'Source\Vcl\SvcMgr.pas');
                cv2006, cv2007, cv2009, cv2010:
                  EnvList.Add('SET ' + sSvcMgrPas + '=' + ShortPath +
                              'Source\Win32\vcl\SvcMgr.pas');
                cvXE2, cvXE3, cvXE4, cvXE5, cvXE6:
                  EnvList.Add('SET ' + sSvcMgrPas + '=' + ShortPath +
                              'Source\vcl\Vcl.SvcMgr.pas');
              end;
              case I of
                 cv5    : EnvList.Add('SET ' + sCompilerVersion + '=5');
                 cv7    : EnvList.Add('SET ' + sCompilerVersion + '=7');
                 cv2006 : EnvList.Add('SET ' + sCompilerVersion + '=10');
                 cv2007 : EnvList.Add('SET ' + sCompilerVersion + '=11');
                 cv2009 : EnvList.Add('SET ' + sCompilerVersion + '=12');
                 cv2010 : EnvList.Add('SET ' + sCompilerVersion + '=14');
                 cvXE   : EnvList.Add('SET ' + sCompilerVersion + '=15');
                 cvXE2  : EnvList.Add('SET ' + sCompilerVersion + '=16');
                 cvXE3  : EnvList.Add('SET ' + sCompilerVersion + '=17');
                 cvXE4  : EnvList.Add('SET ' + sCompilerVersion + '=18');
                 cvXE5  : EnvList.Add('SET ' + sCompilerVersion + '=19');
                 cvXE6  : EnvList.Add('SET ' + sCompilerVersion + '=20');
              end;
              EnvList.SaveToFile('SetEnv.bat');
              Exit;
            end;
          end;
          Writeln('Compiler not installed!');
          Halt(1);
        finally
          Free;
        end;
      finally
        EnvList.Free;
      end;
    end;
  end;

begin
  SetPathAndVersion;
end.
