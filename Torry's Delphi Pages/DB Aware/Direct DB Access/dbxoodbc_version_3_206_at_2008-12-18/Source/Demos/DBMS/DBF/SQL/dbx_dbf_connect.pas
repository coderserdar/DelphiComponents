{
  Version: 2008.11.10
}
unit dbx_dbf_connect;

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

type
  TOdbcDbfDriverType = (
    odt_dbf_auto,
    odt_dbf_msjet,
//todo: odt_dbf_msjet_foxpro
    odt_dbf_msjet_foxpro,
    odt_dbf_merant,
    odt_dbf_intersolv
  );

procedure DBFConnect(SQLConnection: TSQLConnection;
  const dbf_file_name: string;
  const dns_name: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bUnicodeOdbcApi: Boolean = False;
  bAnsiStringField: Boolean = True;
  bUnicodeDriver: Boolean = False;
  OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_msjet
);

procedure DBFConnectW(SQLConnection: TSQLConnection;
  const dbf_file_name: string;
  const dns_name: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bAnsiStringField: Boolean = False;
  bUnicodeOdbcApi: Boolean = True;
  OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_msjet
);

function GetDbfDriverName(OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;
function GetDbfDriver(DirectOdbc: Boolean; OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;
function IsPresentedDbfDriver(DirectOdbc: Boolean; OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): Boolean;
function GetConnectinDefaultOptions(OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;

const
  cDbfDefaultAdditionalOptions_MsJet: string = 'MaxBufferSize=4096;MaxScanRows=16;PageTimeout=17;ReadOnly=0;SafeTransactions=1;Threads=3;UserCommitSync=Yes;coCatPrefix=DefaultDir';
//todo:
  cDbfDefaultAdditionalOptions_VFP: string = '';
  cDbfDefaultAdditionalOptions_IV: string = 'CT=dBASE5;LCK=RECORD;AUT=1;CSZ=16;DFE=DBF;FOC=0;IS=1;LCOMP=dBASE;MS=1;USF=1;ULN=0;ULQ=1;coCatPrefix=DB';
  cDbfDefaultAdditionalOptions_PB: string = 'CT=dBASE5;LCK=RECORD;AUT=1;CSZ=16;DFE=DBF;FOC=0;IS=1;LCOMP=dBASE;MS=1;USF=1;ULN=0;ULQ=1;coCatPrefix=DB';

var
  dbf_dbxoodbc_driver_name: string = 'DbxWMSJetDBase'; // Delphi 2009: must correspond to name in "dbxdrivers.ini".

implementation

(*********************************
 *** DBF Connection Strings ***:
 *********************************

DBF:OdbcExplor:Connection String:

CollatingSequence=International;DBQ=..\DB;DefaultDir=..\DB;Deleted=1;Driver={Microsoft dBase Driver (*.dbf)};DriverId=533;FIL=dBase 5.0;FILEDSN=C:\Program Files\Common Files\ODBC\Data Sources\MS DBase.dsn;MaxBufferSize=2048;MaxScanRows=8;PageTimeout=17;SafeTransactions=0;Statistics=0;Threads=3;UID=admin;UserCommitSync=Yes;

DSN=dsn_dbf;DefaultDir=C:\PROJECTS\_WEBSERVICES\PROJECTS\_REPORTSERVICES\CLIENT2;DriverId=533;FIL=dBase 5.0;MaxBufferSize=2048;PageTimeout=5;
DRIVER={Driver do Microsoft dBase (*.dbf)};DefaultDir=E:\My Documents\2001;DriverId=533;MaxBufferSize=2048;PageTimeout=5;

VendorLib=odbcjt32.dll;DRIVER={Microsoft dBase Driver (*.dbf)};DefaultDir=E:\Borland\Borland Shared\Data\DBASE\VER_5;DriverId=533;MaxBufferSize=2048;PageTimeout=17

VendorLib=IVDBF15.DLL;DB=C:\.Ricoh\w2h\TT;CT=dBASE5;LCK=RECORD;AUT=1;CSZ=16;DFE=DBF;FOC=0;IS=1;LCOMP=dBASE;MS=1;USF=1;ULN=0;ULQ=1

VendorLib=PBDBF13.DLL;DB=E:\Borland\Borland Shared\Data\DBASE\VER_5;CT=dBASE5;LCK=RECORD;AUT=1;CSZ=16;DFE=DBF;FOC=0;IS=1;LCOMP=dBASE;MS=1;USF=1;ULN=0;ULQ=1

*********************************)

const
  // Look in registry: HKEY_LOCAL_MACHINE\SOFTWARE\ODBC\ODBCINST.INI
  cDriverName1 = 'Microsoft dBase Driver (*.dbf)';
  cDriverName2 = 'Driver do Microsoft dBase (*.dbf)';
  cDriverName_VFP = 'Microsoft FoxPro VFP Driver (*.dbf)';

procedure DBFConnectCustom(SQLConnection: TSQLConnection;
  dbf_file_name: string;
  dns_name: string;
  DirectOdbc: Boolean;
  LoginPrompt: Boolean;
  UserName: string;
  Password: string;
  AdditionalOptions: string;
  bAnsiStringField: Boolean;
  bUnicodeOdbcApi: Boolean;
  bUnicodeDriver: Boolean;
  OdbcDbfDriverType: TOdbcDbfDriverType
);
var
  sConnectionString: string;
begin
  SQLConnection.Close;

  dbf_file_name := Trim(dbf_file_name);
  dns_name := Trim(dns_name);

  if (dbf_file_name = '') and (dns_name = '') then
    Exit;

  if OdbcDbfDriverType = odt_dbf_auto then
    OdbcDbfDriverType := odt_dbf_msjet;

  SQLConnection.TableScope := [tsTable];

  SQLConnection.DriverName        := dbf_dbxoodbc_driver_name;
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

  SQLConnection.VendorLib := GetDbfDriver(DirectOdbc, OdbcDbfDriverType);

  {$IF CompilerVersion > 18.50}
  SQLConnection.Params.Text :=
    'DriverUnit=DBXDynalink' + #13#10 +
    'DriverPackageLoader=TDBXDynalinkDriverLoader' + #13#10 +
    ';DriverPackageLoader=TDBXDynalinkDriverCommonLoader' + #13#10 +
    'DriverPackage=DbxCommonDriver120.bpl' + #13#10 +
    'LibraryName=dbxoodbc.dll' + #13#10 +
    'GetDriverFunc=' + SQLConnection.GetDriverFunc + #13#10 +
    'VendorLib=' + SQLConnection.VendorLib + #13#10 +
    dbf_dbxoodbc_driver_name + ' TransIsolation=ReadCommited' + #13#10 +
    'RowsetSize=20';
  {$IFEND}

  SQLConnection.Params.Values['Trim Char'] := 'True';

  if DirectOdbc and (SQLConnection.VendorLib = '') then
  begin
    DirectOdbc := False;
    SQLConnection.VendorLib := GetDbfDriver(DirectOdbc, OdbcDbfDriverType);
  end;

  sConnectionString := '';

  if dns_name <> '' then
    sConnectionString := 'DSN=' + dbf_file_name + ';'
  else
    sConnectionString := 'DRIVER={' + GetDbfDriverName + '};';

  if dbf_file_name <> '' then
  begin
    sConnectionString := sConnectionString
      + 'DefaultDir=' + dbf_file_name + ';';
  end;
  if LoginPrompt then
    sConnectionString := sConnectionString + 'UID=' + UserName + ';PWD=' + Password + ';';

  sConnectionString := sConnectionString + 'DriverId=533;';

  AdditionalOptions := Trim(AdditionalOptions);
  if AdditionalOptions = '' then
    sConnectionString := sConnectionString + GetConnectinDefaultOptions(OdbcDbfDriverType)
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

function GetDbfDriver;//(DirectOdbc: Boolean; OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;
begin
  if DirectOdbc then
  begin
    try
      with TRegistry.Create(KEY_READ) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        case OdbcDbfDriverType of
          odt_dbf_auto, odt_dbf_msjet:
            begin
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
            end;
          odt_dbf_msjet_foxpro:
            begin
// todo: ...
              Result := 'VFPODBC.DLL';
              Exit;
            end;
          odt_dbf_merant:
            begin
              Result := 'IVDBF15.DLL';
              Exit;
            end;
          odt_dbf_intersolv:
            begin
              Result := 'PBDBF13.DLL';
              Exit;
            end;
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

function GetDbfDriverName;//(OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;
begin
  begin
    try
      with TRegistry.Create(KEY_READ) do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        case OdbcDbfDriverType of
          odt_dbf_auto, odt_dbf_msjet:
            begin
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
            end;
          //Microsoft Visual FoxPro Driver (*.dbf)
          odt_dbf_msjet_foxpro:
            begin
              if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\' + cDriverName_VFP, False) and
                ValueExists('Driver') then
              begin
                Result := cDriverName_VFP;
                Exit;
              end;
            end;
          odt_dbf_merant:
            begin
// todo:
              Result := '';
              Exit;
            end;
          odt_dbf_intersolv:
            begin
// todo:
              Result := '';
              Exit;
            end;
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

function IsPresentedDbfDriver;//(DirectOdbc: Boolean; OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): Boolean;
var
  S: string;
begin
  S := GetDbfDriver(DirectOdbc, OdbcDbfDriverType);
  Result := (S <> '') and FileExists(S);
end;

function GetConnectinDefaultOptions(OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_auto): string;
begin
  case OdbcDbfDriverType of
    odt_dbf_auto, odt_dbf_msjet:
      begin
        Result := cDbfDefaultAdditionalOptions_MsJet;
      end;
    //Microsoft Visual FoxPro Driver (*.dbf)
    odt_dbf_msjet_foxpro:
      begin
// todo:
        Result := '';
      end;
    odt_dbf_merant:
      begin
        Result := cDbfDefaultAdditionalOptions_IV;
      end;
    odt_dbf_intersolv:
      begin
        Result := cDbfDefaultAdditionalOptions_PB;
      end;
  end;
end;

procedure DBFConnect;//(SQLConnection: TSQLConnection;
//  const dbf_file_name: string;
//  const dns_name: string = '';
//  DirectOdbc: Boolean = True;
//  LoginPrompt: Boolean = False;
//  UserName: string = '';
//  Password: string = '';
//  const AdditionalOptions: string = '';
//  bUnicodeOdbcApi: Boolean = False;
//  bAnsiStringField: Boolean = True;
//  bUnicodeDriver: Boolean = False;
//  OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_msjet
//);
begin
  DBFConnectCustom(SQLConnection, dbf_file_name, dns_name, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, bUnicodeDriver, OdbcDbfDriverType);
end;

procedure DBFConnectW;//(SQLConnection: TSQLConnection;
//  const dbf_file_name: string;
//  const dns_name: string = '';
//  DirectOdbc: Boolean = True;
//  LoginPrompt: Boolean = False;
//  UserName: string = '';
//  Password: string = '';
//  const AdditionalOptions: string = '';
//  bAnsiStringField: Boolean = False;
//  bUnicodeOdbcApi: Boolean = True;
//  OdbcDbfDriverType: TOdbcDbfDriverType = odt_dbf_msjet
//);
begin
  DBFConnectCustom(SQLConnection, dbf_file_name, dns_name, DirectOdbc,
    LoginPrompt, UserName, Password, AdditionalOptions,
    bAnsiStringField, bUnicodeOdbcApi, {bUnicodeDriver} True, OdbcDbfDriverType);
end;

end.
