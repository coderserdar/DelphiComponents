unit DnsHelper;

{ Unit to return dynamic DNS address for all platforms, primarily
by running the winipcfg.exe or ipconfig.exe programs and redirecting
the report output to a text file which is parsed for the address.

On Windows 98, ME, W2K and XP, it's more efficient to use the IP Helper
APIs which also return the dynamic IP address.

Please note this is not my work, it was posted on usenet somewhere, sometime,
I just corrected some bugs.

Angus Robertson, Magenta Systems Ltd, England
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
10th October 2001

26th July 2002 - Release 4.90
2 Aug 2005 - Release 4.9x - made GetConsoleOutput public  
}

interface

uses
  SysUtils, Classes, Windows;

function GetDnsIp : string;
procedure GetConsoleOutput (const CommandLine : string; var Output : TStringList);

implementation

const
  //
  // NOTE: For '9x, we must use /batch <filename> or the GUI will appear so
  // we use a dummy file
  //
  IPCFG_WIN9X   = 'winipcfg.exe /all /batch $dmytmpgdns$.txt';
  IPCFG_WINNT   = 'ipconfig.exe /all';

  IPCFG_DNS_SERVER_LINE = 'DNS Servers';

  REG_NT_NAMESERVER_PATH1        =
                    'System\CurrentControlSet\Services\Tcpip\Parameters';
  REG_NT_NAMESERVER_PATH2        =
           'System\CurrentControlSet\Services\Tcpip\Parameters\Temporary';
  REG_NT_NAMESERVER             = 'NameServer';

  REG_9X_NAMESERVER_PATH        = 'System\CurrentControlSet\Services\MSTCP';
  REG_9X_NAMESERVER             = 'NameServer';

function BackSlashStr (const s : string) : string;
begin
  Result := s;
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
end;

function GetWindowsPath : string;
var
  Temp  : array [0..MAX_PATH] of char;
begin
  Temp [0] := #0 ;
  GetWindowsDirectory (Temp, SizeOf(Temp));
  Result := BackSlashStr (Temp);
end;

function GetSystemPath : string;
var
  Temp  : array [0..MAX_PATH] of char;
begin
  Temp [0] := #0 ;
  GetSystemDirectory (Temp, SizeOf(Temp));
  Result := Temp ;
end;

function LooksLikeIP(StrIn: string): boolean;
var
  IPAddr : string;
  period, octet, i : Integer;
begin
  result := false;  // default
  IPAddr := StrIn;
  for i := 1 to 4 do begin
    if i = 4 then period := 255 else period := pos('.',IPAddr);
    if period = 0 then exit;
    try
      octet := StrToInt(copy(IPAddr,1,period - 1));
    except
      exit;
    end;  // below, octet < 1 if i = 1, < 0 if i > 1
    if (octet < (1 div i)) or (octet > 254) then exit;
    if i = 4 then result := true else IPAddr := copy(IPAddr,period+1,255);
  end;
end;

procedure GetConsoleOutput (const CommandLine : string;
  var Output : TStringList);
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutFile, AppProcess, AppThread : THandle;
  RootDir, WorkDir, StdOutFileName:string;
const
  FUNC_NAME = 'GetConsoleOuput';
begin
    StdOutFile:=0;
    AppProcess:=0;
    AppThread:=0;
    try

    // Initialize dirs
    RootDir:=ExtractFilePath(ParamStr(0));
    WorkDir:=ExtractFilePath(CommandLine);

    // Check WorkDir
    if not (FileSearch(ExtractFileName(CommandLine),WorkDir)<>'') then
      WorkDir:=RootDir;

    // Initialize output file security attributes
    FillChar(SA,SizeOf(SA),#0);
    SA.nLength:=SizeOf(SA);
    SA.lpSecurityDescriptor:=nil;
    SA.bInheritHandle:=True;

    // Create Output File
    StdOutFileName:=RootDir+'output.tmp';
    StdOutFile:=CreateFile(PChar(StdOutFileName),
                   GENERIC_READ or GENERIC_WRITE,
                   FILE_SHARE_READ or FILE_SHARE_WRITE,
                   @SA,
                   CREATE_ALWAYS, // Always create it
                   FILE_ATTRIBUTE_TEMPORARY or // Will cache in memory
                                               // if possible
                   FILE_FLAG_WRITE_THROUGH,
                   0);

    // Check Output Handle
    if StdOutFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt('Function %s() failed!' + #10#13 +
        'Command line = %s',[FUNC_NAME,CommandLine]);

    // Initialize Startup Info
    FillChar(SI,SizeOf(SI),#0);
    with SI do begin
      cb:=SizeOf(SI);
      dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow:=SW_HIDE;
      hStdInput:=GetStdHandle(STD_INPUT_HANDLE);
      hStdError:=StdOutFile;
      hStdOutput:=StdOutFile;
    end;

    // Create the process
    if CreateProcess(nil, PChar(CommandLine), nil, nil,
                     True, 0, nil,
                     PChar(WorkDir), SI, PI) then begin
      WaitForSingleObject(PI.hProcess,INFINITE);
      AppProcess:=PI.hProcess;
      AppThread:=PI.hThread;
      end
    else
      raise Exception.CreateFmt('CreateProcess() in function %s() failed!'
                   + #10#13 + 'Command line = %s',[FUNC_NAME,CommandLine]);

    CloseHandle(StdOutFile);
    StdOutFile:=0;

    Output.Clear;
    Output.LoadFromFile (StdOutFileName);

  finally
    // Close handles
    if StdOutFile <> 0 then CloseHandle(StdOutFile);
    if AppProcess <> 0 then CloseHandle(AppProcess);
    if AppThread <> 0 then CloseHandle(AppThread);

    // Delete Output file
    if FileExists(StdOutFileName) then
      SysUtils.DeleteFile(StdOutFileName);
  end;
end;

function GetBasicOsType : LongWord;
var
  VerInfo       : TOsVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx (VerInfo);
  Result := VerInfo.dwPlatformId;
end;

function GetIpCfgExePath : string;

begin
  Result := '';
  Case GetBasicOsType of
    VER_PLATFORM_WIN32_WINDOWS  : Result := GetWindowsPath + IPCFG_WIN9X;
    VER_PLATFORM_WIN32_NT       : Result := GetSystemPath + '\' + IPCFG_WINNT;
  end;
end;

function GetDnsIpFromReg : string;

 procedure ReadReg (SubKey, Vn: PChar) ;
 var
  OpenKey       : HKEY;
  DataType,
  DataSize      : integer;
  Temp          : array [0..2048] of char;
 begin
    if RegOpenKeyEx (HKEY_LOCAL_MACHINE, SubKey, REG_OPTION_NON_VOLATILE,
                                    KEY_READ, OpenKey) = ERROR_SUCCESS then
    begin
       DataType := REG_SZ;
       DataSize := SizeOf(Temp);
       if RegQueryValueEx (OpenKey, Vn, nil, @DataType, @Temp,
                    @DataSize) = ERROR_SUCCESS then Result := string(Temp);
       RegCloseKey (OpenKey);
    end;
  end ;

begin
  Result := '';
  case GetBasicOsType of
    VER_PLATFORM_WIN32_WINDOWS :
    begin
      ReadReg (REG_9X_NAMESERVER_PATH, REG_9X_NAMESERVER) ;
    end;
    VER_PLATFORM_WIN32_NT :    // this fails on W2K and XP 
    begin
      ReadReg (REG_NT_NAMESERVER_PATH1, REG_NT_NAMESERVER) ;  // fixed IP
      if result = '' then
              ReadReg (REG_NT_NAMESERVER_PATH2, REG_NT_NAMESERVER) ;  // dynamic IP
    end;
  end;
end;

function GetDnsIpFromIpCfgOut (const Output : TStringList;
  var DnsIp : string) : boolean;
var
  i     : integer;
begin
  Result := FALSE;
  if Output.Count >= 1 then
    for i := 0 to Output.Count - 1 do
    begin
      if Pos(IPCFG_DNS_SERVER_LINE, Output[i]) > 0 then
      begin
        DnsIp := Trim(Copy (Output[i], Pos(':', Output[i])+1,
          Length(Output[i])));
        Result := LooksLikeIp (DnsIp);
      end;
    end;
end;

function GetDnsIp : string;
var
  Output        : TStringList;
  DnsIp,
  CmdLine       : string;
begin
  CmdLine := GetIpCfgExePath;
  if CmdLine <> '' then
  begin
    Output := TStringList.Create;
    try
      try
         GetConsoleOutput (CmdLine, Output);
      except
      end ;
      if GetDnsIpFromIpCfgOut (Output, DnsIp) then
        Result := DnsIp
      else
      begin
        //
        // Attempt to locate via registry
        //
        Result := GetDnsIpFromReg;
      end;
    finally
      Output.Free;
    end;
  end;
end;



end.
