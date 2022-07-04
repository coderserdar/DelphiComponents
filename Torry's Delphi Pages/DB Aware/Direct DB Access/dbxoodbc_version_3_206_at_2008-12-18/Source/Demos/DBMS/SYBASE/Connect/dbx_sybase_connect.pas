{
  Version: 2006.03.07
}
unit dbx_sybase_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, DbxOpenOdbcInterface, Registry;

type
  TSybaseServerType = (sstUnknown, sstASE11, sstASA8, sstASA7);

procedure SybaseConnect(SQLConnection: TSQLConnection;
  SybaseServerType: TSybaseServerType;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
);

function GetSybaseDriver(SybaseServerType: TSybaseServerType): string;
function IsPresentedSybaseDriver(SybaseServerType: TSybaseServerType): Boolean;
function GetSybaseDriverName(SybaseServerType: TSybaseServerType): string;

implementation

// Adaptive Server Anywhere 8
//"Driver={Adaptive Server Anywhere 8.0};ServerName=abaci;DBN=abaci;UID=PSIREAD;PWD=READONLY;links=tcpip()"

// Sybase System 11
// DSN=PSI;UID=PSIREAD;PWD=HAUSER;SRVR=PSI;DB=PSI

procedure SybaseConnect(SQLConnection: TSQLConnection;
  SybaseServerType: TSybaseServerType;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
);

var
  sConnectionString: string;
begin
  SQLConnection.Close;

    SQLConnection.DriverName    := '@MyDriver';
    SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
    SQLConnection.LibraryName   := 'dbxoodbc.dll';
    sConnectionString :=
         'UID=' + UserName
      + ';PWD=' + Password;

    SQLConnection.LoginPrompt := LoginPrompt;
    SQLConnection.Params.Clear;
    SQLConnection.Params.Values['Trim Char'] := 'True';

    if DirectOdbc then
    begin
      if SybaseServerType = sstUnknown then
        DirectOdbc := False;
      {
      els if DNS_NAME <> '' then
        DirectOdbc := False;
      {}
    end;
    if (SybaseServerType = sstUnknown) and ( DNS_NAME = '') then
      raise Exception.Create('Undefined parameter DNS_NAME');

    if DirectOdbc then
      SQLConnection.VendorLib := 'odbc32.dll'
    else
      SQLConnection.VendorLib := GetSybaseDriver(SybaseServerType);

    case SybaseServerType of
      sstUnknown, sstASE11:
        begin
          if ServerName <> '' then
            sConnectionString := sConnectionString + ';SRVR=' + ServerName;

          if DatabaseName <> '' then
            sConnectionString := sConnectionString + ';DB=' + DatabaseName;

          sConnectionString := sConnectionString + ';' + 'coCatPrefix=DB';
        end;
      sstASA7, sstASA8:
        begin
          if ServerName <> '' then
            sConnectionString := sConnectionString + ';ServerName=' + ServerName;

          if DatabaseName <> '' then
            sConnectionString := sConnectionString + ';DBN=' + DatabaseName;

          sConnectionString := sConnectionString + ';links=tcpip();' + 'coCatPrefix=DBN';
        end;
      else
        raise Exception.Create('Unsupported Sybase server type');
    end;

    if DirectOdbc then
    begin
      sConnectionString := 'DRIVER={' + GetSybaseDriverName(SybaseServerType) + '};' + sConnectionString;
    end
    else
    begin
      if DNS_NAME <> '' then
        sConnectionString := 'DNS=' + DNS_NAME + ';' + sConnectionString
      else
        sConnectionString := 'DRIVER={' + GetSybaseDriverName(SybaseServerType) + '};' + sConnectionString;
    end;

    //OutputDebugString(PAnsiChar('*** VendorLibrary: '+ SQLConnection.VendorLib));
    //OutputDebugString(PAnsiChar('*** ConnectionString: '+ sConnectionString));

    {$IF CompilerVersion > 14.01}
       // Delphi 7 Up
       SQLConnection.Params.Values[DATABASENAME_KEY]  := '?';
       SQLConnection.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + sConnectionString;
    {$ELSE}
       // Delphi 6
       if Length(sConnectionString) > 255 then
         SetLength(sConnectionString, 255); // AV protect :(
       SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
    {$IFEND}

  SQLConnection.Open;
end;

function GetSybaseDriverName(SybaseServerType: TSybaseServerType): string;
begin
  case SybaseServerType of
    sstUnknown:
      Result := '';
    sstASE11:
      Result := 'Sybase System 11';
    sstASA8:
      Result := 'Adaptive Server Anywhere 8.0';
    sstASA7:
      Result := 'Adaptive Server Anywhere 7.0';
    else
      raise Exception.Create('Unsupported Sybase server type');
  end;
end;

function GetSybaseDriver(SybaseServerType: TSybaseServerType): string;
var
  sDriverName: string;
begin
  if SybaseServerType = sstUnknown then
  begin
    Result := 'odbc32.dll';
    Exit;
  end;
  Result := '';
  sDriverName := GetSybaseDriverName(SybaseServerType);
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + sDriverName, False) and
        ValueExists('Driver') then
      begin
        Result := ReadString('Driver');
        Exit;
      end;
    finally
      Free;
    end;
  except
    // access denied to HKEY_LOCAL_MACHINE
  end;
  case SybaseServerType of
    //sstUnknown:
    //  Result := 'odbc32.dll';
    sstASE11:
      Result := 'sysybnt.dll';
    sstASA8:
      Result := 'dbodbc8.dll';
    sstASA7:
      Result := 'dbodbc7.dll';
    else
      raise Exception.Create('Unsupported Sybase server type');
  end;
end;

function IsPresentedSybaseDriver(SybaseServerType: TSybaseServerType): Boolean;
begin
  Result := FileExists(GetSybaseDriver(SybaseServerType));
end;

end.
