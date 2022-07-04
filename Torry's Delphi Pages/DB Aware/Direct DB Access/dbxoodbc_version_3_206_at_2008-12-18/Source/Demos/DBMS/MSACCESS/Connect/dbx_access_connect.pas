{
  Version: 2008.03.04
}
unit dbx_access_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, Registry, DbxOpenOdbcInterface;

procedure AccessConnect(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bUnicodeOdbcApi: Boolean = False;
  bAnsiStringField: Boolean = True;
  bUnicodeDriver: Boolean = False
);

procedure AccessConnectW(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bAnsiStringField: Boolean = False;
  bUnicodeOdbcApi: Boolean = True
);

function GetAccessDriver(DirectOdbc: Boolean): string;
function IsPresentedAccessDriver(DirectOdbc: Boolean): Boolean;

const
  cAccessDefaultAdditionalOptions: string = 'MaxBufferSize=2048;PageTimeout=17;coSafeMode=1;coBlobFragmntns=1';

implementation

// DRIVER={Microsoft Access Driver (*.mdb)};DBQ=C:\CVS\D_10\dbxoodbc\dbExprStress\dbstress.mdb;DriverId=25;FIL=MS Access;MaxBufferSize=2048;PageTimeout=17;coSafeMode=1;coBlobFragmntns=1

procedure AccessConnectCustom(SQLConnection: TSQLConnection;
  mdb_file_name: string;
  DNS_NAME: string;
  DirectOdbc: Boolean;
  LoginPrompt: Boolean;
  UserName: string;
  Password: string;
  AdditionalOptions: string;
  bAnsiStringField: Boolean;
  bUnicodeOdbcApi: Boolean;
  bUnicodeDriver: Boolean
);
var
  sConnectionString: string;
begin
  SQLConnection.Close;

  mdb_file_name := Trim(mdb_file_name);
  DNS_NAME := Trim(DNS_NAME);

  if (mdb_file_name = '') and (DNS_NAME = '') then
    Exit;

  SQLConnection.DriverName    := '@MyDriver';
  if bUnicodeDriver then
  begin
    if bUnicodeOdbcApi then
    begin
      SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
    end
    else
    begin
      SQLConnection.GetDriverFunc := 'getSQLDriverODBCWA';
    end;
  end
  else
  begin
    if not bUnicodeOdbcApi then
    begin
      SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
    end
    else
    begin
      SQLConnection.GetDriverFunc := 'getSQLDriverODBCAW';
    end;
  end;
  SQLConnection.LibraryName   := 'dbxoodbc.dll';

  UserName := Trim(UserName);
  if UserName = '' then
    LoginPrompt := False;

  SQLConnection.LoginPrompt := LoginPrompt;
  SQLConnection.Params.Clear;
  SQLConnection.Params.Values['Trim Char'] := 'True';

  if DNS_NAME <> '' then
    DirectOdbc := False;

  SQLConnection.VendorLib := GetAccessDriver(DirectOdbc);
  if DirectOdbc and (SQLConnection.VendorLib = '') then
  begin
    DirectOdbc := False;
    SQLConnection.VendorLib := GetAccessDriver(DirectOdbc);
  end;

  sConnectionString := '';

  if DNS_NAME <> '' then
    sConnectionString := 'DSN=' + mdb_file_name + ';'
  else
    sConnectionString := 'DRIVER={Microsoft Access Driver (*.mdb)};';
  //if not DirectOdbc then
  //  sConnectionString := 'DRIVER={Microsoft Access Driver (*.mdb)};';

  if mdb_file_name <> '' then
    sConnectionString := sConnectionString + 'DBQ=' + mdb_file_name + ';';
  if LoginPrompt then
    sConnectionString := sConnectionString + 'UID=' + UserName + ';PWD=' + Password + ';';

  sConnectionString := sConnectionString + 'DriverId=25;FIL=MS Access;';

  AdditionalOptions := Trim(AdditionalOptions);
  if AdditionalOptions = '' then
    sConnectionString := sConnectionString + cAccessDefaultAdditionalOptions
  else
    sConnectionString := sConnectionString + AdditionalOptions;

  if bUnicodeDriver then
  begin
    if bAnsiStringField then
      sConnectionString := sConnectionString + ';coEnableUnicode=0;';
  end
  else
  begin
    //
    // DBX2 not supported TWideStringField
    //
    //if not bAnsiStringField then
    //  sConnectionString := sConnectionString + ';coEnableUnicode=1;';

    sConnectionString := sConnectionString + ';coEnableUnicode=0;';
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

function GetAccessDriver(DirectOdbc: Boolean): string;
const
  cDriverName = 'Microsoft Access Driver (*.mdb)';
begin
  if DirectOdbc then
  begin
    try
      with TRegistry.Create(KEY_READ) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName, False) and
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
      Result := 'odbcjt32.dll';
      Exit;
    end;
  end;
  Result := 'odbc32.dll';
end;

function IsPresentedAccessDriver(DirectOdbc: Boolean): Boolean;
var
  S: string;
begin
  S := GetAccessDriver(DirectOdbc);
  Result := (S <> '') and FileExists(S);
end;

procedure AccessConnect;//(SQLConnection: TSQLConnection;
//  const mdb_file_name: string;
//  const DNS_NAME: string = '';
//  DirectOdbc: Boolean = True;
//  LoginPrompt: Boolean = False;
//  UserName: string = '';
//  Password: string = '';
//  const AdditionalOptions: string = '';
//  bUnicodeOdbcApi: Boolean = False;
//  bAnsiStringField: Boolean = True;
//  bUnicodeDriver: Boolean = False
//);
begin
  AccessConnectCustom(SQLConnection, mdb_file_name, DNS_NAME, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, bUnicodeDriver);
end;

procedure AccessConnectW;//(SQLConnection: TSQLConnection;
//  const mdb_file_name: string;
//  const DNS_NAME: string = '';
//  DirectOdbc: Boolean = True;
//  LoginPrompt: Boolean = False;
//  UserName: string = '';
//  Password: string = '';
//  const AdditionalOptions: string = '';
//  bAnsiStringField: Boolean = False;
//  bUnicodeOdbcApi: Boolean = True
//);
begin
  AccessConnectCustom(SQLConnection, mdb_file_name, DNS_NAME, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, {bUnicodeDriver} True);
end;

end.
