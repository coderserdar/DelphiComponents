program CreateSetEnvBat;

// Oct 2019 - Angus added XE7 to 10.3
// Jul 2020 - Angus added 10.4
// Sep 2021 - Angus added 11.0
// May 2022 - Angus update EnvList for 11.0


{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Registry, Classes;

type
  TCompilerVersion = (cv5, cv7, cv2006, cv2007, cv2009, cv2010, cvXE, cvXE2,
     cvXE3,cvXE4, cvXE5, cvXE6, cvXE7, cvXE8,
     cvD10, cvD101, cvD102, cvD103, cvD104, cvD110);

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
    'Software\Embarcadero\BDS\14.0',  //XE6
    'Software\Embarcadero\BDS\15.0',  //XE7
    'Software\Embarcadero\BDS\16.0',  //XE8
    'Software\Embarcadero\BDS\17.0',  //10 Seattle
    'Software\Embarcadero\BDS\18.0',  //10.1 Berlin
    'Software\Embarcadero\BDS\19.0',  //10.2 Tokyo
    'Software\Embarcadero\BDS\20.0',  //10.3 Rio
    'Software\Embarcadero\BDS\21.0',  //10.4 Sydney
    'Software\Embarcadero\BDS\22.0'); //11.0 Alexandria

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
                cvXE2, cvXE3, cvXE4, cvXE5, cvXE6, cvXE7, cvXE8, cvD10,
                                     cvD101, cvD102, cvD103, cvD104, cvD110:
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
                 cvXE7  : EnvList.Add('SET ' + sCompilerVersion + '=21');
                 cvXE8  : EnvList.Add('SET ' + sCompilerVersion + '=22');
                 cvD10  : EnvList.Add('SET ' + sCompilerVersion + '=23');
                 cvD101 : EnvList.Add('SET ' + sCompilerVersion + '=24');
                 cvD102 : EnvList.Add('SET ' + sCompilerVersion + '=25');
                 cvD103 : EnvList.Add('SET ' + sCompilerVersion + '=26');
                 cvD104 : EnvList.Add('SET ' + sCompilerVersion + '=27');
                 cvD110 : EnvList.Add('SET ' + sCompilerVersion + '=28');
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
