{
  Version: 2008.11.03
}
unit dbx_excel_connect;

interface

uses
  Windows, SysUtils, Classes, Registry,
  {$IF CompilerVersion > 18.00}
  DBXCommon,
  {$ELSE}
  DBXpress,
  {$IFEND}
  {$IF CompilerVersion >= 19.00}
  DBXDynalink,
  {$IFEND}
  {$IF CompilerVersion > 17.00}
  WideStrings,
  {$IFEND}
  SqlConst, SqlExpr,
  DbxOpenOdbcCallback, DbxOpenOdbcInterface;

procedure ExcelConnect(SQLConnection: TSQLConnection;
  const xls_file_name: string;
  const dns_name: string = '';
  bAutoCreate: Boolean = True;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bUnicodeOdbcApi: Boolean = False;
  bAnsiStringField: Boolean = True;
  bUnicodeDriver: Boolean = False
);

procedure ExcelConnectW(SQLConnection: TSQLConnection;
  const xls_file_name: string;
  const dns_name: string = '';
  bAutoCreate: Boolean = True;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bAnsiStringField: Boolean = False;
  bUnicodeOdbcApi: Boolean = True
);

function GetExcelDriverName: string;
function GetExcelDriver(DirectOdbc: Boolean): string;
function IsPresentedExcelDriver(DirectOdbc: Boolean): Boolean;

const
  cExcelDefaultAdditionalOptions: string = 'MaxBufferSize=2048;PageTimeout=17;ReadOnly=0;coCatPrefix=DBQ';
var
  excel_dbxoodbc_driver_name: string = 'DbxWMSJetExcel'; // Delphi 2009: must correspond to name in "dbxdrivers.ini".

implementation

//
// VendorLib=odbcjt32.dll;DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17
// DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17
//

//  Excel:OLEDB:ODBC
//  rsConnString_MSODBCExcel = 'Provider=MSDASQL.1;Persist Security Info=False;Extended Properties="'
//    + 'DRIVER=Microsoft Excel Driver (*.xls);CREATE_DB=%s;DBQ=%s;DefaultDir=%s;DriverId=790;'
//    + 'FIL=excel 8.0;MaxBufferSize=2048;PageTimeout=5;ReadOnly=0;"';

const
  // Look in registry: HKEY_LOCAL_MACHINE\SOFTWARE\ODBC\ODBCINST.INI
  cDriverName1 = 'Microsoft Excel Driver (*.xls)';
  cDriverName2 = 'Microsoft Excel-Treiber (*.xls)';

procedure ExcelConnectCustom(SQLConnection: TSQLConnection;
  xls_file_name: string;
  dns_name: string;
  bAutoCreate: Boolean;
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

  xls_file_name := Trim(xls_file_name);
  dns_name := Trim(dns_name);

  if (xls_file_name = '') and (dns_name = '') then
    Exit;

  SQLConnection.TableScope := [tsTable];

  SQLConnection.DriverName        := excel_dbxoodbc_driver_name;
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

  if dns_name <> '' then
    DirectOdbc := False;

  SQLConnection.VendorLib := GetExcelDriver(DirectOdbc);

  {$IF CompilerVersion > 18.50}
  SQLConnection.Params.Text :=
    'DriverUnit=DBXDynalink' + #13#10 +
    'DriverPackageLoader=TDBXDynalinkDriverLoader' + #13#10 +
    ';DriverPackageLoader=TDBXDynalinkDriverCommonLoader' + #13#10 +
    'DriverPackage=DbxCommonDriver120.bpl' + #13#10 +
    'LibraryName=dbxoodbc.dll' + #13#10 +
    'GetDriverFunc=' + SQLConnection.GetDriverFunc + #13#10 +
    'VendorLib=' + SQLConnection.VendorLib + #13#10 +
    excel_dbxoodbc_driver_name + ' TransIsolation=ReadCommited' + #13#10 +
    'RowsetSize=20';
  {$IFEND}

  SQLConnection.Params.Values['Trim Char'] := 'True';

  if DirectOdbc and (SQLConnection.VendorLib = '') then
  begin
    DirectOdbc := False;
    SQLConnection.VendorLib := GetExcelDriver(DirectOdbc);
  end;

  sConnectionString := '';

  if dns_name <> '' then
    sConnectionString := 'DSN=' + xls_file_name + ';'
  else
    sConnectionString := 'DRIVER={' + GetExcelDriverName + '};';

// VendorLib=odbcjt32.dll;DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17
// DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17

  if xls_file_name <> '' then
  begin
    if bAutoCreate and not FileExists(xls_file_name) then
      sConnectionString := sConnectionString + 'CREATE_DB=' + xls_file_name + ';';
    sConnectionString := sConnectionString + 'DBQ=' + xls_file_name + ';'
      + 'DefaultDir=' + ExtractFilePath(xls_file_name) + ';';
  end;
  if LoginPrompt then
    sConnectionString := sConnectionString + 'UID=' + UserName + ';PWD=' + Password + ';';

  sConnectionString := sConnectionString + 'DriverId=790;';
  //sConnectionString := sConnectionString + 'DriverId=790;FIL=MS Excel;';

  AdditionalOptions := Trim(AdditionalOptions);
  if AdditionalOptions = '' then
    sConnectionString := sConnectionString + cExcelDefaultAdditionalOptions
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
       SQLConnection.Params.Values[CUSTOM_INFO] := string(cConnectionOptionsNames[coConnectionString]) + '=' + sConnectionString;
     {$IFEND}
  {$ELSE}
     // Delphi 6
     if Length(sConnectionString) > 255 then
       SetLength(sConnectionString, 255); // Buffer overflow AV protect :(
     SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
  {$IFEND}

  SQLConnection.Open;
end;

// VendorLib=odbcjt32.dll;DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17
// DRIVER={Microsoft Excel Driver (*.xls)};DBQ=E:\My Documents\2001\Blank-Zakaz_ç.xls;DefaultDir=E:\My Documents\2001;DriverId=790;MaxBufferSize=2048;PageTimeout=17

function GetExcelDriver(DirectOdbc: Boolean): string;
begin
  if DirectOdbc then
  begin
    try
      with TRegistry.Create(KEY_READ) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName1, False) and
          ValueExists('Driver') then
        begin
          Result := ReadString('Driver');
          Exit;
        end
        else if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName2, False) and
          ValueExists('Driver') then
        begin
          Result := ReadString('Driver');
          Exit;
        end;
      finally
        Free;
      end;
    except
      // Access denied to HKEY_LOCAL_MACHINE
      Result := 'odbcjt32.dll';
      Exit;
    end;
  end;
  Result := 'odbc32.dll';
end;

function GetExcelDriverName: string;
begin
  begin
    try
      with TRegistry.Create(KEY_READ) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName1, False) and
          ValueExists('Driver') then
        begin
          Result := cDriverName1;
          Exit;
        end
        else if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName2, False) and
          ValueExists('Driver') then
        begin
          Result := cDriverName2;
          Exit;
        end;
      finally
        Free;
      end;
    except
      // Access denied to HKEY_LOCAL_MACHINE
      Result := cDriverName1;
      Exit;
    end;
  end;
  Result := cDriverName1;
end;

function IsPresentedExcelDriver(DirectOdbc: Boolean): Boolean;
var
  S: string;
begin
  S := GetExcelDriver(DirectOdbc);
  Result := (S <> '') and FileExists(S);
end;

procedure ExcelConnect;//(SQLConnection: TSQLConnection;
//  const xls_file_name: string;
//  const dns_name: string = '';
//  bAutoCreate: Boolean = True;
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
  ExcelConnectCustom(SQLConnection, xls_file_name, dns_name, bAutoCreate, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, bUnicodeDriver);
end;

procedure ExcelConnectW;//(SQLConnection: TSQLConnection;
//  const xls_file_name: string;
//  const dns_name: string = '';
//  bAutoCreate: Boolean = True;
//  DirectOdbc: Boolean = True;
//  LoginPrompt: Boolean = False;
//  UserName: string = '';
//  Password: string = '';
//  const AdditionalOptions: string = '';
//  bAnsiStringField: Boolean = False;
//  bUnicodeOdbcApi: Boolean = True
//);
begin
  ExcelConnectCustom(SQLConnection, xls_file_name, dns_name, bAutoCreate, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, {bUnicodeDriver} True);
end;

end.
