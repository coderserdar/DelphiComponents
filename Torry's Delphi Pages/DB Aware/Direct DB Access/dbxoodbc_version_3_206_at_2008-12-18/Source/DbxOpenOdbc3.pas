{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.205 2008-11-10

  Copyright (c) 2001-2009 Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbc3;

{$include DbxOpenOdbc_options.inc}

interface

{$IFDEF _DBX30_}

uses
  DSIntf,
  DbxOpenOdbcInterface,
  OdbcApi,
  FmtBcd,
  {$IFDEF _TRACE_CALLS_}
    DbxOpenOdbcTrace,
  {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  DbxOpenOdbcCallback,
  {$ENDIF}
  DbxOpenOdbc,
  {$IFDEF _D11UP_}
  DbxOpenOdbcDbx3Types,
  {$ELSE}
  DBXpress,
  {$ENDIF}
  Classes,
  SysUtils;

{ getSQLDriverODBC is the starting point for everything else... }

// priority unicode odbc api
function getSQLDriverODBCW(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
// priority ansi odbc api
function getSQLDriverODBCWA(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;

type

  { TSqlDriverOdbc3 implements ISQLDriver }

  TSqlDriverOdbc3 = class(TSqlDriverOdbc, ISQLDriver)
  protected
    { begin ISQLDriver methods }
    function getSQLConnection(
      out pConn: ISQLConnection
      ): SQLResult; stdcall;
    { end ISQLDriver methods }
  public
    constructor Create(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
  end;

  { TSqlConnectionOdbc implements ISQLConnection }

  TSqlConnectionOdbc3 = class(TSqlConnectionOdbc, ISQLConnection, ISQLConnection30)
  protected
    fServerName, fUserName, fPassword: WideString;
  protected
    { begin: ISQLConnection30 interface methods: }
     function connect(): SQLResult; overload; stdcall;
     function connect(ServerName: PWideChar; UserName: PWideChar;
                            Password: PWideChar): SQLResult; overload; stdcall;
     function getSQLCommand(out pComm: ISQLCommand30): SQLResult; stdcall;
     function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult; stdcall;
     function SetOption(eConnectOption: TSQLConnectionOption;
              lValue: LongInt): SQLResult; stdcall;
     function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
              MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
     function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
     function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
     { end: ISQLConnection30 interface methods. }
  public
    constructor Create(OwnerDbxDriver: TSqlDriverOdbc); override;
  end;

  { TSqlCommandOdbc implements ISQLCommand }

  TSqlCommandOdbc3 = class(TSqlCommandOdbc, ISQLCommand, ISQLCommand30)
  protected
    { begin ISQLCommand methods }
    function SetOption(
       eSqlCommandOption: TSQLCommandOption;
       ulValue: Integer): SQLResult; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption;
       PropValue: Pointer;
       MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function prepare(SQL: PWideChar; ParamCount: Word): SQLResult; stdcall;
    function execute(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult; stdcall;
    function getNextCursor(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    { end ISQLCommand methods }
  end;

  { TSqlCursorOdbc implements ISQLCursor }

  TSqlCursorOdbc3 = class(TSqlCursorOdbc, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSQLMetaDataOdbc implements ISQLMetaData }

  TSQLMetaDataOdbc3 = class(TSQLMetaDataOdbc, ISQLMetaData, ISQLMetaData30)
  protected
    { begin ISQLMetaData methods }
    function SetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function getObjectList(
      eObjType: TSQLObjectType;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getTables(
      TableName: PWideChar;
      TableType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getProcedures(
      ProcedureName: PWideChar;
      ProcType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getColumns(
      TableName: PWideChar;
      ColumnName: PWideChar;
      ColType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getProcedureParams(
      ProcName: PWideChar;
      ParamName: PWideChar;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getIndices(
      TableName: PWideChar;
      IndexType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen:
      Smallint
      ): SQLResult; stdcall;
    { end ISQLMetaData methods }
  end;

  { TSqlCursorMetaDataTables - implements cursor returned by ISQLMetaData.GetTables }
  TSqlCursorMetaDataTables3 = class(TSqlCursorMetaDataTables, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataColumns - implements cursor returned by ISQLMetaData.GetColumns }

  TSqlCursorMetaDataColumns3 = class(TSqlCursorMetaDataColumns, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataIndexes - implements cursor returned by ISQLMetaData.GetIndices }

  TSqlCursorMetaDataIndexes3 = class(TSqlCursorMetaDataIndexes, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataProcedures - implements cursor returned by ISQLMetaData.GetProcedures }

  TSqlCursorMetaDataProcedures3 = class(TSqlCursorMetaDataProcedures, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataProcedureParams - implements cursor returned by ISQLMetaData.GetProcedureParams }

  TSqlCursorMetaDataProcedureParams3 = class(TSqlCursorMetaDataProcedureParams, ISQLCursor, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PAnsiChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

var
  cConnectionOptionsDefault3: TConnectionOptions;

{$ENDIF _DBX30_}

implementation

{$IFDEF _DBX30_}

uses
  Windows, DB, {WideStrUtils,}
  {$IFNDEF _D11UP_}
  SqlExpr,
  {$ENDIF}
  SqlTimst, DateUtils, DbxOpenOdbcFuncs;

{ Public function getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBCW;//(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
  try try
  LogEnterProc('getSQLDriverODBCW', ['sVendorLib =', StrPas(sVendorLib), 'sResourceFile =', sResourceFile, 'Obj =', Pointer(Obj)] );
  {$ENDIF}
  Pointer(Obj) := nil;
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}True);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' + sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc3.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}True);
  {$IFDEF _TRACE_CALLS_}
    except
      on e:Exception do
      begin
        LogExceptProc('getSQLDriverODBCW', e);
        raise;
      end;
    end;
    finally
      LogExitProc('getSQLDriverODBCW', ['Result =', Result, 'Obj =', Pointer(Obj)]);
    end;
  {$ENDIF}
end;

function getSQLDriverODBCWA;//(sVendorLib, sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
  try try
  LogEnterProc('getSQLDriverODBCWA', ['sVendorLib =', StrPas(sVendorLib), 'sResourceFile =', sResourceFile, 'Obj =', Pointer(Obj)] );
  {$ENDIF}
  Pointer(Obj) := nil;
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}False);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' + sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc3.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}False);
  {$IFDEF _TRACE_CALLS_}
    except
      on e:Exception do
      begin
        LogExceptProc('getSQLDriverODBCWA', e);
        raise;
      end;
    end;
    finally
      LogExitProc('getSQLDriverODBCWA', ['Result =', Result, 'Obj =', Pointer(Obj)]);
    end;
  {$ENDIF}
end;

function BeginLock(const LockName: string; out hLock: THandle; TimeOut: DWORD = INFINITE): Boolean;
var
  S: string;
begin
  S := LockName;
  if S <> '' then
  begin
    S := StringReplace(S, '\', '|', [rfReplaceAll]);
    S := StringReplace(S, #0, '@', [rfReplaceAll]);
    if Length(S) > MAX_PATH  then
      SetLength(S, MAX_PATH);
  end;
  hLock := CreateMutex(nil, True, PChar(S));
  Result := hLock <> 0;
  if Result then
  begin
    Result := WaitForSingleObject( hLock, TimeOut ) = WAIT_OBJECT_0;
    if not Result then
    begin
      CloseHandle(hLock);
      hLock := 0;
    end;
  end
  else
  begin
    hLock := 0;
    //RaiseLastOsError();
  end;
end;

procedure EndLock(var hLock: THandle);
begin
  if hLock <> 0 then
  begin
    CloseHandle(hLock);
    hLock := 0;
  end;
end;

{ TSqlDriverOdbc3 }

var
  gClientVersionOverload: Integer = -1;

procedure HostAppPlatformInitialize();
var
  hLock: THandle;
begin
  BeginLock('{6603C100-8CE4-4B5A-ACF1-3D99B2F90B3C}:' + IntToStr(GetCurrentProcessID), hLock, 6000);
  try
    if gClientVersionOverload >= 0 then
      Exit;
    gClientVersionOverload := 0;
    //
    // Check that application works over CLR
    //
    if (GetModuleHandle('dbxintf.dll') <> 0) or (GetModuleHandle('bdpdbx25.dll') <> 0) then
    begin
      Include(gHostPlatform, hpCLR);
      //gClientVersionOverload := 40;
    end;
    //
    // Delphi 2007: 'dbxadapter30.dll'
    //
    if (GetModuleHandle('dbxadapter30.dll') <> 0) then
      gClientVersionOverload := 40;
    //
    // Delphi 2009: 'dbxadapter.dll'
    //
    if (GetModuleHandle('dbxadapter.dll') <> 0) then
      gClientVersionOverload := 41;
  finally
    EndLock(hLock);
  end;
end;

constructor TSqlDriverOdbc3.Create(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc3.Create'); {$ENDIF _TRACE_CALLS_}
  fDBXVersion := 30; { == 3.0 }
  if gClientVersionOverload < 0 then
    HostAppPlatformInitialize();
  if gClientVersionOverload > 0 then
    fClientVersion := gClientVersionOverload { >= 4.0 }
  else
    fClientVersion := 30; { == 3.0 }
  inherited Create(AOdbcApi, bIsUnicodeOdbcApi);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc3.Create', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc3.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlDriverOdbc3.getSQLConnection;//(out pConn: ISQLConnection): SQLResult;
begin
  Pointer(pConn) := nil;
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlDriverOdbc3.getSQLConnection'); {$ENDIF _TRACE_CALLS_}
  ISQLConnection(pConn) := TSqlConnectionOdbc3.Create(Self);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlDriverOdbc3.getSQLConnection', e);  raise; end; end;
    finally LogExitProc('TSqlDriverOdbc3.getSQLConnection', ['Result =', Result, 'pConn =', Pointer(pConn)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlConnectionOdbc3 }

constructor TSqlConnectionOdbc3.Create(OwnerDbxDriver: TSqlDriverOdbc);
begin
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc3.Create'); {$ENDIF _TRACE_CALLS_}

  inherited Create(OwnerDbxDriver);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.Create', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.Create'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.connect: SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlConnectionOdbc3.connect(A)'); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  Result := inherited connect(PAnsiChar(AnsiString(fServerName)), PAnsiChar(AnsiString(fUserName)),  PAnsiChar(AnsiString(fPassword)));
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.connect(A)', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.connect(A)', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.connect(ServerName, UserName,
  Password: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlConnectionOdbc.connect(B)', ['ServerName=', PWideChar(ServerName), 'UserName =', PWideChar(UserName), 'Password =', PWideChar(Password)]); {$ENDIF _TRACE_CALLS_}
  fServerName := ServerName;
  fUserName := UserName;
  fPassword := Password;
  // ansi:
  //
  Result := inherited connect(PAnsiChar(AnsiString(fServerName)), PAnsiChar(AnsiString(fUserName)),  PAnsiChar(AnsiString(fPassword)));
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc.connect(B)', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc.connect(B)', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc3.getErrorMessage', ['ErrorPtr =', Pointer(Error), 'UserName =']); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fConnectionErrorLines, Error);
    fConnectionErrorLines.Clear;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.getErrorMessage', ['Result =', Result, 'Error =', PWideChar(Error)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.getErrorMessageLen(out ErrorLen: SmallInt): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fConnectionErrorLines) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.getErrorMessageLen', ['Result =', Result, 'ErrorLen =', ErrorLen]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
var
  xeDOption: TXSQLConnectionOption absolute eDOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
  sPropName: string;
  // ---
{$IFDEF _TRACE_CALLS_}
  isError: Boolean;

  function Result2Str: AnsiString;
  begin
    if (xeDOption in cXSQLConnectionOptionStringTypes) then
    begin
      if (PropValue <> nil) then
        Result := AnsiString(WideString(PWideChar(PropValue)))
      else
        Result := '';
      if sPropName <> '' then
        Result := Result + '"; for property: "' + AnsiString(sPropName);
      Exit;
    end;
    case xeDOption of
      //xeConnServerVersion,
      //xeConnDatabaseName,
      //xeConnServerCharSet,
      //xeConnObjectQuoteChar,
      //xeConnConnectionName:
      //xeConnQualifiedName,
      //xeConnCatalogName,
      //xeConnSchemaName,
      //xeConnObjectName,
      //xeConnQuotedObjectName,
      //xeConnCustomInfo,
      //xeConnConnectionString,
      //xeVendorProperty:
      //  Result := StrPas(PAnsiChar(PropValue));
      //xeConnDecimalSeparator:
      //    Result := PAnsiChar(PropValue)^;
      xeConnAutoCommit,
      xeConnBlockingMode:
        Result := AnsiString(BoolToStr(Boolean(PropValue^)));
      xeConnBlobSize:
        Result := AnsiString(IntToStr(Integer(PropValue^)));
      xeConnTxnIsoLevel:
        case TTransIsolationLevel(PropValue^) of
          xilREPEATABLEREAD:
            Result := 'xilREPEATABLEREAD';
          xilREADCOMMITTED:
            Result := 'xilREADCOMMITTED';
          xilDIRTYREAD:
            Result := 'xilDIRTYREAD';
          else
            Result := AnsiString(IntToStr(Integer(PropValue^)));
        end;
      xeConnSupportsTransaction,
      xeConnMultipleTransaction,
      xeConnTrimChar:
        Result := AnsiString(BoolToStr(Boolean(PropValue^)));
      else
        Result := AnsiString(IntToStr(Longint(PropValue^)));
    end; //of: case
  end;
{$ENDIF _TRACE_CALLS_}
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    isError := True;
    try try
    {$R+}
    LogEnterProc('TSqlConnectionOdbc3.GetOption', ['eDOption = '+cSQLConnectionOption[xeDOption], 'MaxLength =', MaxLength, 'Length =', Length]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  Length := 0;
  if (xeDOption in cXSQLConnectionOptionStringTypes)  then
  begin
    if TSqlDriverOdbc3(fOwnerDbxDriver).fClientVersion > 40 then // Delphi 2009
    begin
      if xeDOption = xeVendorProperty then
      begin
        sPropName := string(WideString(PWideChar(PropValue)));
        if sPropName <> '' then
        begin
          {$IFDEF _TRACE_CALLS_}
          LogInfoProc('PropertyName = "' + AnsiString(sPropName) + '"');
          {$ENDIF}
          iMaxChars := MaxLength div SizeOf(WideChar) + 1;
          (*
          if iMaxChars = 1 then // detected for: 'SupportsParameterMetaData'
          begin
            // ERROR: uncorrect buffer length
            if PropValue <> nil then
              PWideChar(PropValue)^ := cNullWideChar;
            Length := 0;
            {$IFDEF _TRACE_CALLS_}
            isError := False;
            {$ENDIF}
            Result := DBXERR_NOTSUPPORTED; // DBXERR_WARNING;
            Exit;
          end;
          //*)
          sOptionValue := '';
          if SameText(sPropName, 'ProductName')  then
            sOptionValue := 'dbxoodbc'
            //sOptionValue := AnsiString('dbxoodbc, version: ') + DbxOpenOdbcVersion
          else
          if SameText(sPropName, 'SupportsParameterMetaData')  then
          begin
            Length := 0;
            Result := DBXERR_NOTSUPPORTED; // DBXERR_WARNING;
            Exit;
            //sOptionValue := AnsiString('false');
            //iMaxChars := MAX_VERSION_STRING_LENGTH; // ERROR: uncorrect buffer length
          end;
          if sOptionValue <> '' then
          begin
            if System.Length(sOptionValue) > iMaxChars then
              SetLength(sOptionValue, iMaxChars);
            StringToWStr(sOptionValue, PWideChar(PropValue));
            Length := System.Length(sOptionValue) * SizeOf(WideChar);
            {$IFDEF _TRACE_CALLS_}
            isError := False;
            {$ENDIF}
            Exit;
          end
          else
          begin
            PWideChar(PropValue)^ := cNullWideChar;
            Length := 0;
            {$IFDEF _TRACE_CALLS_}
            isError := False;
            {$ENDIF}
            Exit;
          end;
        end;
      end;
    end;

    if PropValue <> nil then
      PWideChar(PropValue)^ := cNullWideChar;

    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, 0);
    Result := inherited GetOption(eDOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      if PropValue <> nil then
        Length := StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars) * SizeOf(WideChar)
      else
        Length := System.Length(sOptionValue) * SizeOf(WideChar);
    end;
  end
  else
  begin
    Result := inherited GetOption(eDOption, PropValue, MaxLength, Length);
  end;
  {$IFDEF _TRACE_CALLS_}
    isError := False;
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.GetOption', e);  raise; end; end;
    finally
      if not isError then
      begin
        if Result = DBXERR_NONE then
          LogInfoProc('Result.PropValue = "' + Result2Str() + '"')
        else
          LogInfoProc('Result <> SQL_SUCCESS');
      end;
      LogExitProc('TSqlConnectionOdbc3.GetOption', ['Result =', Result, 'Length =', Length]);
    end;
  {$ENDIF}
end;

function TSqlConnectionOdbc3.SetOption(eConnectOption: TSQLConnectionOption; lValue: Integer): SQLResult;
var
  xeConnectOption: TXSQLConnectionOption absolute eConnectOption;
  sOptionValue: AnsiString;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    try try
    {$R+}
    LogEnterProc('TSqlConnectionOdbc3.SetOption', ['eConnectOption = '+cSQLConnectionOption[xeConnectOption], 'lValuePtr =', Pointer(lValue)]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (xeConnectOption in cXSQLConnectionOptionStringTypes) then
  begin
    if (lValue <> 0) then
    begin
      sOptionValue := AnsiString(WideString(PWideChar(lValue)));
      Result := inherited SetOption(eConnectOption, Integer(PAnsiChar(sOptionValue)));
    end;
  end
  else
    Result := inherited SetOption(eConnectOption, lValue);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.getSQLCommand(out pComm: ISQLCommand30): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc3.getSQLCommand', ['pComm =', Pointer(pComm)]); {$ENDIF _TRACE_CALLS_}
  Pointer(pComm) := nil;
  if fConnected and (not fConnectionClosed) then
  begin
    pComm := TSqlCommandOdbc3.Create(Self);
  end
  else
  begin
    CheckMaxLines(fConnectionErrorLines);
    fConnectionErrorLines.Add('getSQLCommand called but not yet connected');
    Result := DBX_FIRSTSTATICERRORS;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.getSQLCommand', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.getSQLCommand', ['Result =', Result, 'pComm =', Pointer(pComm)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlConnectionOdbc3.getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlConnectionOdbc3.getSQLMetaData', ['pMetaData =', Pointer(pMetaData)]); {$ENDIF _TRACE_CALLS_}
  Pointer(pMetaData) := nil;
  if fConnected and (not fConnectionClosed) then
  begin
    pMetaData := TSqlMetaDataOdbc3.Create({SupportWideString:}True, Self);
  end
  else
  begin
    CheckMaxLines(fConnectionErrorLines);
    fConnectionErrorLines.Add('getSQLMetaData called but not yet connected');
    Result := DBX_FIRSTSTATICERRORS;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlConnectionOdbc3.getSQLMetaData', e);  raise; end; end;
    finally LogExitProc('TSqlConnectionOdbc3.getSQLMetaData', ['Result =', Result, 'pMetaData =', Pointer(pMetaData)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCommandOdbc3 }

function TSqlCommandOdbc3.prepare(SQL: PWideChar; ParamCount: Word): SQLResult;
var
  S: AnsiString;
begin
  // ansi:
  //
  //S := WideString(SQL);
  //Result := inherited prepare(PAnsiChar(S), ParamCount);

  // unicode:
  //
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.prepare', ['SQL =', PWideChar(SQL), 'ParamCount =', ParamCount, 'StoredProc =', fStoredProc]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(TSqlConnectionOdbc3(fOwnerDbxConnection).fDbxTraceCallbackEven) then
    TSqlConnectionOdbc3(fOwnerDbxConnection).DbxCallBackSendMsgFmt(cTDBXTraceFlags_Prepare, 'ParamCount: %d; SQL: %s', [ParamCount, AnsiString(WideString(SQL))]);
  {$ENDIF}
  if SQL = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  if TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi then
  begin
    Result := DoPrepare(PAnsiChar(SQL), ParamCount, {UpdateParams:}True,
      {bPrepareSQL:}TSqlConnectionOdbc3(fOwnerDbxConnection).fPrepareSQL, {bUseUnicodeOdbc:} True);
  end
  else
  begin
    S := AnsiString(WideString(SQL));
    Result := DoPrepare(PAnsiChar(S), ParamCount, {UpdateParams:}True,
      {bPrepareSQL:}TSqlConnectionOdbc3(fOwnerDbxConnection).fPrepareSQL, {bUseUnicodeOdbc:} False);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.prepare', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.prepare', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.execute(var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
begin
  // unicode:
  //
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.execute'); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(TSqlConnectionOdbc3(fOwnerDbxConnection).fDbxTraceCallbackEven) then
    TSqlConnectionOdbc3(fOwnerDbxConnection).DbxCallBackSendMsg(cTDBXTraceFlags_Execute, fSQL);
  {$ENDIF}
  Result := DoExecute(vCursor25, {bUseUnicodeOdbc:}TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.execute', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.execute', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
  S: AnsiString;
begin
  // unicode:
  //
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.ExecuteImmediate', ['SQL =', PWideChar(SQL)]); {$ENDIF _TRACE_CALLS_}
  {$IFDEF _DBXCB_}
  if Assigned(TSqlConnectionOdbc3(fOwnerDbxConnection).fDbxTraceCallbackEven) then
    TSqlConnectionOdbc3(fOwnerDbxConnection).DbxCallBackSendMsg(cTDBXTraceFlags_Execute, AnsiString(WideString(SQL)));
  {$ENDIF}
  if SQL <> nil then
  begin
    if TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi and (fStoredProc <> 1) then
    begin
      Result := DoExecuteImmediate(PAnsiChar(SQL), vCursor25, {bUseUnicodeOdbc:} True)
    end
    else
    begin
      S := AnsiString(WideString(SQL));
      Result := DoExecuteImmediate(PAnsiChar(S), vCursor25, {bUseUnicodeOdbc:} False);
    end;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.ExecuteImmediate', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.ExecuteImmediate', ['Result =', Result, 'CursorPtr =', Pointer(Cursor)]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessage(Error);
end;

function TSqlCommandOdbc3.getErrorMessageLen(out ErrorLen: SmallInt): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessageLen(ErrorLen);
end;

function TSqlCommandOdbc3.getNextCursor(var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
begin
  Result := inherited getNextCursor(vCursor25);
end;

function TSqlCommandOdbc3.GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer; MaxLength: Smallint; out Length: SmallInt): SQLResult;
var
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
begin
  {$IFDEF _TRACE_CALLS_}
    Result := DBXERR_NONE;
    try try
    {$R+}
    LogEnterProc('TSqlCommandOdbc3.GetOption', ['eSqlCommandOption =', cSQLCommandOption[xeSqlCommandOption], 'PropValuePtr =', PropValue, 'MaxLength =', MaxLength]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
    Length := 0;
  {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (xeSqlCommandOption in cXSQLCommandOptionStringTypes) and (PropValue <> nil) and (MaxLength >= SizeOf(WideChar)) then
  begin
    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, 0);
    Result := inherited GetOption(eSqlCommandOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      Length := StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars) * SizeOf(WideChar);
    end;
  end
  else
    Result := inherited GetOption(eSqlCommandOption, PropValue, MaxLength, Length);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.GetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.GetOption', ['Result =', Result, 'Length =', Length]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer): SQLResult;
var
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;
  sOptionValue: AnsiString;
  {$IFDEF _TRACE_CALLS_}
  function ulValue2Str: AnsiString;
  begin
    case xeSqlCommandOption of
      xeCommBlockRead:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommCursorName:
        Result := AnsiString(WideString(PWideChar(ulValue)));
      xeCommStoredProc:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommPackageName:
        Result := AnsiString(WideString(PWideChar(ulValue)));
      xeCommTrimChar:
        Result := AnsiString(BoolToStr(Boolean(ulValue)));
      xeCommCatalogName:
        Result := AnsiString(WideString(PWideChar(ulValue)));
      xeCommSchemaName:
        Result := AnsiString(WideString(PWideChar(ulValue)));
      else
        Result := AnsiString(IntToStr(ulValue));
    end;
  end;
  {$ENDIF _TRACE_CALLS_}
begin
  {$IFDEF _TRACE_CALLS_}
    Result := DBXERR_NONE;
    try try
    {$R+}
    LogEnterProc('TSqlCommandOdbc3.SetOption', ['eSqlCommandOption =', cSQLCommandOption[xeSqlCommandOption], 'ulValue =', ulValue2Str()]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (xeSqlCommandOption in cXSQLCommandOptionStringTypes) and (ulValue <> 0) then
  begin
    sOptionValue := AnsiString(WideString(PWideChar(ulValue)));
    Result := inherited SetOption(eSqlCommandOption, Integer(PAnsiChar(sOptionValue)));
  end
  else
    Result := inherited SetOption(eSqlCommandOption, ulValue);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.SetOption', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorOdbc3 }

function TSqlCursorOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessage(Error);
end;

function TSqlCursorOdbc3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessageLen(ErrorLen);
end;

function TSqlCursorOdbc3.getColumnNameLength(ColumnNumber: WORD; var pLen: Word): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc3.getColumnNameLength', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  pLen := min(WideStringLengthFromStr(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName), SizeOf(DBINAME128)-1);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getColumnNameLength', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getColumnNameLength', ['pLen =', pLen]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWideChar(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName, pColumnName, SizeOf(DBINAME128) - 1);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorOdbc3.getInt64', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  with TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]) do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Int64(Value^) := 0
    else
      Int64(Value^) := fOdbcHostVarAddress.ptrSqlBigInt^;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getInt64', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getInt64', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Pointer(Value), IsBlank);
end;

function TSqlCursorOdbc3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  vColValueSize, vColSizeBuff: SqlUInteger;
  RCh: PAnsiChar;
  aOdbcBindCol: TOdbcBindCol;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then
      Value^ := cNullWideChar;
    IsBlank := True;
    try try
    LogEnterProc('TSqlCursorOdbc3.getWideString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    Exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
    begin
      if not fIsBuffer then
        FetchLateBoundData(ColumnNumber)
      else
        FetchLongData(ColumnNumber);
    end;

    // check buffer overflow (for bad odbc drivers).
    vColValueSize := fColValueSizePtr^; // buffer length in  bytes
    IsBlank := (vColValueSize = OdbcApi.SQL_NULL_DATA) or
      (vColValueSize = OdbcApi.SQL_NO_TOTAL);

    vColSizeBuff := fColSize * SizeOf(WideChar);
    if (not IsBlank) and (vColValueSize > 0) then
    begin
      if vColValueSize > vColSizeBuff then
        vColValueSize := vColSizeBuff;
    end
    else
    begin
      vColValueSize := 0;
      IsBlank := True;
    end;

    if IsBlank then
    begin
      Value^ := cNullWideChar
    end
    else
    begin
      if vColValueSize = 0 then
      begin
        Value^ := cNullWideChar
      end
      else
      {$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
      //
      // QC: 58473:
      // Option TrimChar shall ignore because of error in Delphi for TWideStringFiled with FixedSize.
      // Delphi not will not send important information about field type (fldstFIXED) when is called method ".setParameter".
      // See: SqlExpr.pas: procedure D2006UP: SetQueryProcParams.
      //
      {$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}
      {begin: trim char:}
      if TSqlCommandOdbc3(fOwnerCommand).fTrimChar and (fDbxSubType and fldstFIXED <> 0) then
      begin
        RCh := PAnsiChar(DWORD(fOdbcHostVarAddress.ptrAnsiChar) + DWORD(vColValueSize - SizeOf(WideChar)));
        // debug: PAnsiChar(DWORD(aOdbcBindCol.fOdbcHostVarAddress.ptrAnsiChar) + DWORD(vColValueSize - SizeOf(WideChar)))
        // debug: aOdbcBindCol.fOdbcHostVarAddress.ptrWideChar
        while (RCh >= fOdbcHostVarAddress.ptrAnsiChar) and (PWideChar(RCh)^ = WideChar(' ')) do
          Dec(RCh, SizeOf(WideChar));
        vColValueSize := Integer(RCh)  + SizeOf(WideChar) - Integer(fOdbcHostVarAddress.ptrAnsiChar);
        if vColValueSize > 0 then
        begin
          Move(fOdbcHostVarAddress.ptrAnsiChar^, Value^, vColValueSize);
          Value[vColValueSize div SizeOf(WideChar)] := cNullWideChar;
        end
        else
          Value^ := cNullWideChar;
      end
      else
      {end: trim char.}
      begin
        Move(fOdbcHostVarAddress.ptrWideChar^, PWideChar(Value)^, vColValueSize);

        Inc(PAnsiChar(Value), vColValueSize);
        vColValueSize := vColValueSize div SizeOf(WideChar);

        if (fDbxSubType and fldstFIXED <> 0) and (vColValueSize < fColSize) then
        begin
          // we shall add lacking spaces if driver their cuts itself
          while vColValueSize <= fColSize do
          begin
            Value^ := WideChar(' ');
            Inc(PAnsiChar(Value), SizeOf(WideChar));
            Inc(vColValueSize, SizeOf(WideChar));
          end
        end;

        Value^ := cNullWideChar;
      end;
    end;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getWideString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSQLMetaDataOdbc3 }

function TSQLMetaDataOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc3.getErrorMessage', ['ErrorPtr =', Pointer(Error)]); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
    StringsToWStr(fMetaDataErrorLines, Error);
  fMetaDataErrorLines.Clear;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getErrorMessage', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSQLMetaDataOdbc3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fMetaDataErrorLines) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getErrorMessageLen', ['Result =', Result, 'ErrorLen =', ErrorLen]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.GetOption', ['MaxLength =', MaxLength]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (xeDOption in cXSQLMetaDataOptionStringTypes) and (PropValue <> nil) and (MaxLength >= SizeOf(WideChar)) then
  begin
    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, 0);
    Result := inherited GetOption(eDOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      Length := StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars) * SizeOf(WideChar);
    end;
  end
  else
    Result := inherited GetOption(eDOption, PropValue, MaxLength, Length);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.GetOption', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.GetOption', ['Result =', Result, 'Length =', Length]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.SetOption(eDOption: TSQLMetaDataOption; PropValue: Integer): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
  sOptionValue: AnsiString;
  {$IFDEF _TRACE_CALLS_}
  function PropValue2Str: AnsiString;
  begin
    case xeDOption of
      xeMetaCatalogName,
      xeMetaSchemaName,
      xeMetaDatabaseName,
      xeMetaPackageName:
        Result := AnsiString(WideString(PWideChar(PropValue)));
      else
        Result := AnsiString(IntToStr(Integer(PropValue)));
    end;
  end;
  {$ENDIF _TRACE_CALLS_}
begin
  {$IFDEF _TRACE_CALLS_}
    Result := DBXERR_NONE;
    try try
    {$R+}
    LogEnterProc('TSQLMetaDataOdbc3.SetOption', ['eDOption =', cSQLMetaDataOption[xeDOption], 'PropValue =', PropValue2Str()]);
    {$IFDEF RANGECHECKS_OFF} {$R-} {$ENDIF}
  {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (xeDOption in cXSQLMetaDataOptionStringTypes) and (PropValue <> 0) then
  begin
    sOptionValue := AnsiString(WideString(PWideChar(PropValue)));
    Result := inherited SetOption(eDOption, Integer(PAnsiChar(sOptionValue)));
  end
  else
    Result := inherited SetOption(eDOption, PropValue);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.SetOption', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.SetOption', ['Result =', Result]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSQLMetaDataOdbc3.getTables(TableName: PWideChar; TableType: Longword; out Cursor: ISQLCursor30): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.getTables', ['TableName =', TableName, 'TableType =', TableType]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetTables(PAnsiChar(TableName), TableType, Pointer(Cursor), {Unicode}True);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getTables', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getTables'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getProcedures(ProcedureName: PWideChar; ProcType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sOptionValue: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.getProcedures', ['ProcedureName =', ProcedureName, 'ProcType =', ProcType]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (ProcedureName = nil) or (Trim(WideString(ProcedureName)) = '') then
    Result := inherited getProcedures(nil, ProcType, vCursor)
  else
  begin
    sOptionValue := AnsiString(WideString(PWideChar(ProcedureName)));
    Result := inherited getProcedures(PAnsiChar(sOptionValue), ProcType, vCursor)
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getProcedures', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getProcedures'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getColumns(TableName, ColumnName: PWideChar; ColType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sTableName, sColumnName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.getColumns', ['TableName =', TableName, 'ColumnName =', ColumnName, 'ColType =', ColType]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  if (TableName <> nil) then
    sTableName := AnsiString(WideString(TableName));
  if (ColumnName <> nil) then
    sColumnName := AnsiString(WideString(ColumnName));
  Result := inherited getColumns(PAnsiChar(sTableName), PAnsiChar(sColumnName), ColType, vCursor)
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getColumns', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getColumns'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getIndices(TableName: PWideChar; IndexType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sTableName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.getIndices', ['TableName =', TableName, 'IndexType =', IndexType]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  if (TableName <> nil) then
    sTableName := AnsiString(WideString(TableName));
  Result := inherited getIndices(PAnsiChar(sTableName), IndexType, vCursor)
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getIndices', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getIndices'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSQLMetaDataOdbc3.getProcedureParams(ProcName, ParamName: PWideChar; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sProcName, sParamName: AnsiString;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSQLMetaDataOdbc3.getProcedureParams', ['ProcName =', ProcName, 'ParamName =', ParamName]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  if (ProcName <> nil) then
    sProcName := AnsiString(WideString(ProcName));
  if (ParamName <> nil) then
    sParamName := AnsiString(WideString(ParamName));
  Result := inherited getProcedureParams(PAnsiChar(sProcName), PAnsiChar(sParamName), vCursor)
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSQLMetaDataOdbc3.getProcedureParams', e);  raise; end; end;
    finally LogExitProc('TSQLMetaDataOdbc3.getProcedureParams'); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataTables3 }

function TSqlCursorMetaDataTables3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables3.getErrorMessage'); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables3.getErrorMessage'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables3.getErrorMessageLen'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataTables3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataTables3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataTables3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataTables3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataTables3.getWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetWideString(ColumnNumber, Value, IsBlank);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataTables3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataTables3.getWideString', ['IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataColumns3 }

function TSqlCursorMetaDataColumns3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns3.getErrorMessage'); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns3.getErrorMessage'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataColumns3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns3.getErrorMessageLen'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataColumns3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataColumns3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataColumns3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataColumns3.getWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetWideString(ColumnNumber, Value, IsBlank);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataColumns3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataColumns3.getWideString', ['IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataIndexes3 }

function TSqlCursorMetaDataIndexes3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes3.getErrorMessage'); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes3.getErrorMessage'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes3.getErrorMessageLen'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataIndexes3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataIndexes3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataIndexes3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataIndexes3.getWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetWideString(ColumnNumber, Value, IsBlank);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataIndexes3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataIndexes3.getWideString', ['IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataProcedures3 }

function TSqlCursorMetaDataProcedures3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataIndexes3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures3.getErrorMessage'); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures3.getErrorMessage'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedures3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures3.getErrorMessageLen'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedures3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataProcedures3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataProcedures3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedures3.getWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetWideString(ColumnNumber, Value, IsBlank);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedures3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedures3.getWideString', ['IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSqlCursorMetaDataProcedureParams3 }

function TSqlCursorMetaDataProcedureParams3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedureParams3.getColumnName', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  // ansi:
  //
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams3.getColumnName', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams3.getColumnName'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams3.getErrorMessage'); {$ENDIF _TRACE_CALLS_}
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams3.getErrorMessage', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams3.getErrorMessage'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_} try try LogEnterProc('TSqlCursorMetaDataProcedureParams3.getErrorMessageLen'); {$ENDIF _TRACE_CALLS_}
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams3.getErrorMessageLen', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams3.getErrorMessageLen'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorMetaDataProcedureParams3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataProcedureParams3.getString(ColumnNumber: Word; Value: PAnsiChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataProcedureParams3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorMetaDataProcedureParams3.getWideString', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  Result := DoGetWideString(ColumnNumber, Value, IsBlank);
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorMetaDataProcedureParams3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorMetaDataProcedureParams3.getWideString', ['IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

procedure DoRegisterDbXpressLibW();
begin
  {$IFNDEF _D11UP_}
    {$IFDEF _D10UP_}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCW);
    {$ELSE}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCAW);
    {$ENDIF}
  {$ENDIF}
end;

procedure DoRegisterDbXpressLibWA();
begin
  {$IFNDEF _D11UP_}
    {$IFDEF _D10UP_}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCWA);
    {$ELSE}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBC);
    {$ENDIF}
  {$ENDIF}
end;

initialization
begin
  cConnectionOptionsDefault3 := cConnectionOptionsDefault;
  {$IFDEF _INT64_BUGS_FIXED_}
  cConnectionOptionsDefault3[coMapInt64ToBcd] := osOff;
  {$ENDIF}
  cConnectionOptionsDefault3[coEnableUnicode] := osOn;

  RegisterDbXpressLibProc(DoRegisterDbXpressLibW, oaUnicode);
  RegisterDbXpressLibProc(DoRegisterDbXpressLibWA, oaUnicodeToAnsi);

// Deprecated:
//  This allows option of static linking the DbExpress driver into your app
//{$IFDEF MSWINDOWS}
//  {$IFNDEF _DENT_}
//    {$IFNDEF _D11UP_}
//      DoRegisterDbXpressLibW();
//    {$ENDIF}
//  {$ENDIF}
//{$ENDIF}

end;

{$ENDIF _DBX30_}

end.
