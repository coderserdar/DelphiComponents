{
  Version: 2008.03.24
}
unit dbx_mssql_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, DbxOpenOdbcInterface, Registry;

procedure MsSqlConnect(SQLConnection: TSQLConnection;
  ServerVersion: Integer;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  OSAuthentication: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = '';
  const LanguageName: string = '';
  // Allow use of DBX3
  bUnicodeDbxDriver: Boolean = False;
  // Allow or nor use of adbc unicode api
  bUnicodeODBCAPI: Boolean = False;
  // Allow or nor use of TWideStringField
  bAnsiFields: Boolean = True
);


function GetDirectMsSqlDriver(ServerVersion: Integer = 0): string;
function GetLocalMsSqlServer(var ServerVersion: Integer; var Server: string): Boolean;

implementation

//DSN=dsn_mssql;UID=user_name;PWD=mypassword;SERVER=server_name_or_ip;
//DATABASE=database_name;Trusted_Connection=Yes;APP=application_name;
//WSID=client_host_name;Network=DBMSSOCN

procedure MsSqlConnect(SQLConnection: TSQLConnection;
  ServerVersion: Integer;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  OSAuthentication: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = '';
  const LanguageName: string = '';
  bUnicodeDbxDriver: Boolean = False;
  bUnicodeODBCAPI: Boolean = False;
  bAnsiFields: Boolean = True
);
var
  sConnectionString: string;
begin
  SQLConnection.Close;

    SQLConnection.DriverName    := '@MyDriver';
    if not bUnicodeDbxDriver then
    begin
      if not bUnicodeODBCAPI then
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
      end
      else
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCAW';
      end;
    end
    else
    begin
      if bUnicodeODBCAPI then
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
      end
      else
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCWA';
      end;
    end;

    SQLConnection.LibraryName   := 'dbxoodbc.dll';

    if UserName = '' then
      OSAuthentication := True;

    if OSAuthentication then
    begin
      LoginPrompt := False;
      sConnectionString := 'Trusted_Connection=Yes';
    end
    else
    begin
      sConnectionString :=
          'UID=' + UserName
        + ';PWD=' + Password
    end;
    SQLConnection.LoginPrompt := LoginPrompt;
    SQLConnection.Params.Clear;
    SQLConnection.Params.Values['Trim Char'] := 'True';

    SQLConnection.VendorLib := '';
    if DirectOdbc then
      SQLConnection.VendorLib := GetDirectMsSqlDriver(ServerVersion);
    if SQLConnection.VendorLib = '' then
    begin
      DirectOdbc := False;
      SQLConnection.VendorLib := 'odbc32.dll';
    end;

    if (DNS_NAME = '') and (ServerName = '') then
      sConnectionString := sConnectionString + ';SERVER=127.0.0.1'
    else if ServerName <> '' then
      sConnectionString := sConnectionString + ';SERVER=' + ServerName;

    if DatabaseName <> '' then
      sConnectionString := sConnectionString + ';DATABASE=' + DatabaseName;

    if LanguageName <> '' then
      sConnectionString := sConnectionString + ';LANGUAGE=' + DatabaseName;

    if AdditionalOptions <> '' then
      sConnectionString := sConnectionString + ';' + AdditionalOptions;

    sConnectionString := sConnectionString + ';' + 'coCatPrefix=DATABASE';

    if DirectOdbc then
    begin
      case ServerVersion of
        2000:
          sConnectionString := 'DRIVER={SQL Server};' + sConnectionString
        else // 2005 UP
          sConnectionString := 'DRIVER={SQL Native Client};' + sConnectionString
      end; // of: case ServerVersion
    end
    else
    begin
      if DNS_NAME <> '' then
        sConnectionString := 'DNS=' + DNS_NAME + ';' + sConnectionString
      else
      begin
        case ServerVersion of
          2000:
            sConnectionString := 'DRIVER={SQL Server};' + sConnectionString
          else // 2005 UP
            sConnectionString := 'DRIVER={SQL Native Client};' + sConnectionString
        end; // of: case ServerVersion
      end;
    end;

    //bAnsiFields
    if not bUnicodeDbxDriver then
    begin
      //
      // DBX2 not supported TWideStringField
      //
      //if not bAnsiFields then
      //  sConnectionString := sConnectionString + ';coEnableUnicode=1';
      sConnectionString := sConnectionString + ';coEnableUnicode=0';
    end
    else
    begin
      if not bAnsiFields then
        sConnectionString := sConnectionString + ';coEnableUnicode=0';
    end;

    {$IF CompilerVersion > 14.01}
       // Delphi 7 Up
     {$IF CompilerVersion = 18.50}
       // Delphi 2007 bug: not transfer connection options CUSTOM_INFO ("Custom String")
       if Length(sConnectionString) > 255 then
         SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
       SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
     {$ELSE}
       SQLConnection.Params.Values[DATABASENAME_KEY]  := '?';
       SQLConnection.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + sConnectionString;
     {$IFEND}
    {$ELSE}
       // Delphi 6
       if Length(sConnectionString) > 255 then
         SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
       SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
    {$IFEND}

  SQLConnection.Open;
end;

function GetDirectMsSqlDriver(ServerVersion: Integer): string;
begin
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;

      case ServerVersion of
        2000:
          if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\SQL Server', False) and
            ValueExists('Driver') then
          begin
            Result := Trim(ReadString('Driver'));
            Exit;
          end;
        else { 2005, 2007 }
          if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\SQL Native Client', False) and
            ValueExists('Driver') then
          begin
            Result := Trim(ReadString('Driver'));
            Exit;
          end;
      end; // of: case ServerVersion
    finally
      Free;
    end;
  except
  end;

  case ServerVersion of
    2000:
      Result :='sqlsrv32.dll'
    else { 2005, 2007 }
      Result := 'sqlncli.dll';
  end; // of: case ServerVersion

  if not FileExists(Result) then
    SetLength(Result, 0);
end;

function GetLocalMsSqlServer(var ServerVersion: Integer; var Server: string): Boolean;
var
  R: TRegistry;
  S: string;
  vKeyNames: TStringList;
begin
  vKeyNames := nil;
  R := TRegistry.Create(KEY_READ);
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    //
    // 2005:
    //
    // "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL"
    try
      if R.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL') then
      begin
        vKeyNames := TStringList.Create;
        R.GetValueNames(vKeyNames);
        if vKeyNames.Count > 0 then
        begin
          S := vKeyNames[0];
          Server := '127.0.0.1\' + S;
          ServerVersion := 2005;
          Result := True;
          Exit;
        end;
      end;
    except
    end;
    //
    // 2000:
    //
    // "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SQL Server\80\Tools\Service Manager"
    try
      if R.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SQL Server\80\Tools\Service Manager') then
      begin
        //Default
        Server := '127.0.0.1';
        ServerVersion := 2000;
        Result := True;
        Exit;
      end;
    except
    end;
    //.
  finally
    R.Free;
    vKeyNames.Free;
  end;
  Result := False;
end;

end.
