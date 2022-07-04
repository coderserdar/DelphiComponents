{
  Version: 2008.10.23
}
unit dbx_ora_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, DbxOpenOdbcInterface, Registry;

function OracleBuildConnection(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
): Boolean;

function OracleConnect(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
): Boolean;


function IsPresentedOracleDriver(): Boolean;
function IsPresentedMicrosoftOracleDriver(): Boolean;

var
  oracle_dbxoodbc_driver_name: string = 'DbxWOracle'; // Delphi 2009: must correspond to name in "dbxdrivers.ini".

  // Look in registry: HKEY_LOCAL_MACHINE\SOFTWARE\ODBC\ODBCINST.INI
  cDriverName1: string = 'Oracle ODBC Driver';
  cDriverName2: string = 'Oracle in 10g';

implementation


function OracleBuildConnection(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
): Boolean;

var
  sConnectionString, sDrv: string;
begin
  //Result := False;

  SQLConnection.Close;

  SQLConnection.DriverName    := oracle_dbxoodbc_driver_name; //'@MyDriver';

  {$IF CompilerVersion < 17.50}
  SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
  {$ELSE}
  SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
  {$IFEND}
  SQLConnection.LibraryName   := 'dbxoodbc.dll';
  SQLConnection.LoginPrompt := bLoginPrompt;

  SQLConnection.Params.Clear;
  SQLConnection.Params.Values['Trim Char'] := 'True';

  if not bMicrosoftDriver then
  begin
    sConnectionString :=
        'UID=' + UserName
      + ';PWD=' + Password
      + ';DBQ=' + TnsName
      + ';DBA=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;FRL=F;MTS=T;CSR=T;PFC=10;TLO=0'
      + ';coLockMode=-1;coSchemFlt=1;coCatPrefix=UID';
    if bDirectOdbc then
      SQLConnection.VendorLib := 'sqora32.dll';
  end
  else
  begin
    sConnectionString :=
        'UID=' + UserName
      + ';PWD=' + Password
      + ';SERVER=' + TnsName
      + ';coLockMode=-1;coSchemFlt=1;coCatPrefix=UID';
    if bDirectOdbc then
      SQLConnection.VendorLib := 'msorcl32.dll';
  end;

  if AdditionalOptions <> '' then
    sConnectionString := sConnectionString + ';' + AdditionalOptions;

  if not bDirectOdbc then
  begin
    SQLConnection.VendorLib := 'odbc32.dll';
    if DNS_NAME <> '' then
      sConnectionString := 'DNS=' + DNS_NAME + ';' + sConnectionString
    else
    begin
      if not bMicrosoftDriver then
        sDrv := '{Oracle ODBC Driver}'
      else
        sDrv := '{Microsoft ODBC for Oracle}';
      sConnectionString := 'DRIVER=' + sDrv + ';' + sConnectionString;
    end;
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
       SQLConnection.Params.Values[CUSTOM_INFO] := string(cConnectionOptionsNames[coConnectionString]) + '=' + sConnectionString;
     {$IFEND}
  {$ELSE}
     // Delphi 6
     if Length(sConnectionString) > 255 then
       SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
     SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
  {$IFEND}
  Result := True;
end;

function OracleConnect(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
): Boolean;
begin
  Result := OracleBuildConnection(SQLConnection, TnsName, UserName, Password,
    bMicrosoftDriver, bDirectOdbc, bLoginPrompt, DNS_NAME, AdditionalOptions);
  if Result and (not SQLConnection.Connected) then
    SQLConnection.Open;
end;

function IsPresentedOracleDriver(): Boolean;
begin
  //Result := False;
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName1, False) and
        ValueExists('Driver') then
      begin
        Result := True;
        Exit;
      end
      else if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName2, False) and
        ValueExists('Driver') then
      begin
        Result := True;
        Exit;
      end;
    finally
      Free;
    end;
  except
  end;
  Result := FileExists('sqora32.dll');
end;

function IsPresentedMicrosoftOracleDriver(): Boolean;
begin
  Result := False;
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\Microsoft ODBC for Oracle', False) and
        ValueExists('Driver') then
      begin
         Result := True;
      end;
    finally
      Free;
    end;
  except
  end;
end;

end.
