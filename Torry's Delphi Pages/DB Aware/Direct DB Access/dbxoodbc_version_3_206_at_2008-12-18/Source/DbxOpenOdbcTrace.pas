{
  Part of Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.203 2008-11-03

  Copyright (c) 2003-2009 by Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}

{$IFDEF _D9UP_}{$REGION 'COMMENTS'}{$ENDIF}
(*

For review of debug messages (outside of IDE) it is possible to take advantage of DebugView program.

Debug View:
http://www.microsoft.com/technet/sysinternals/Miscellaneous/DebugView.mspx
  OLD: http://www.sysinternals.com/ntw2k/freeware/debugview.shtml

DebugView is an application that lets you monitor debug output on your local system, or any computer
on the network that you can reach via TCP/IP. It is capable of displaying both kernel-mode and Win32
debug output, so you don't need a debugger to catch the debug output your applications or device
drivers generate, nor do you need to modify your applications or drivers to use non-standard debug
output APIs.

DebugView works on Windows 95, 98, Me, NT 4, 2000, XP and .NET Server.

Download (169KB):
http://www.microsoft.com/technet/sysinternals/Miscellaneous/DebugView.mspx
  OLD: http://www.sysinternals.com/files/dbgvnt.zip

Under Linux debug messages will be dispatched on a console (stderr).

*)
{$IFDEF _D9UP_}{$ENDREGION}{$ENDIF}

unit DbxOpenOdbcTrace;

interface

{$include DbxOpenOdbc_options.inc}
{.$D+,L+}

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF IFDEF MSWINDOWS}
    Classes,
    SysUtils,
    {$IFDEF _D11UP_}
    DbxOpenOdbcDbx3Types,
    {$ELSE}
    DBXpress,
    {$ENDIF}
    DbxOpenOdbcInterface,
    OdbcApi;

procedure OutputDebugString(const S:AnsiString); {$IFDEF _INLINE_} inline; {$ENDIF} overload;
procedure OutputDebugString(const ws:WideString); {$IFDEF _INLINE_} inline; {$ENDIF} overload;

procedure LogEnterProc(const ProcName: AnsiString; const Params: AnsiString=''); overload;
procedure LogEnterProc(const ProcName: AnsiString; const Params: array of const); overload;

procedure LogInfoProc(const Params: AnsiString=''); overload; {$IFDEF _INLINE_} inline; {$ENDIF}
procedure LogInfoProc(const Params: array of const); overload;

procedure LogExitProc(const ProcName: AnsiString; const ExitInfo: AnsiString=''); overload;
procedure LogExitProc(const ProcName: AnsiString; const ExitInfo: array of const); overload;

procedure LogExceptProc(const ProcName: AnsiString; E:Exception; const ExceptInfo: AnsiString=''); overload;
procedure LogExceptProc(const ProcName: AnsiString; E:Exception; const ExceptInfo: array of const); overload;

function BoolToStr(Value: Boolean): AnsiString; {$IFDEF _INLINE_} inline; {$ENDIF}
function ValueToStr(const Value: array of const): AnsiString;

const

  cSQLDriverOption: array[TXSQLDriverOption] of AnsiString = (
    'eDrvBlobSize', 'eDrvCallBack', 'eDrvCallBackInfo', 'eDrvRestrict'
    {.$IFDEF _D10UP_}
    ,'eDrvVersion', 'eDrvProductVersion'
    {.$ENDIF}
  );

  cSQLConnectionOption: array[TXSQLConnectionOption] of AnsiString = (
    'eConnAutoCommit', 'eConnBlockingMode', 'eConnBlobSize', 'eConnRoleName',
    'eConnWaitOnLocks', 'eConnCommitRetain', 'eConnTxnIsoLevel',
    'eConnNativeHandle', 'eConnServerVersion', 'eConnCallBack', 'eConnHostName',
    'eConnDatabaseName', 'eConnCallBackInfo', 'eConnObjectMode',
    'eConnMaxActiveComm', 'eConnServerCharSet', 'eConnSqlDialect'
    {.$IFDEF _K3UP_},
    'eConnRollbackRetain', 'eConnObjectQuoteChar', 'eConnConnectionName',
    'eConnOSAuthentication', 'eConnSupportsTransaction', 'eConnMultipleTransaction',
    'eConnServerPort', 'eConnOnLine', 'eConnTrimChar'
    {.$ENDIF}
    {.$IFDEF _D7UP_}, 'eConnQualifiedName',
    'eConnCatalogName', 'eConnSchemaName', 'eConnObjectName', 'eConnQuotedObjectName',
    'eConnCustomInfo', 'eConnTimeOut'
    {.$ENDIF}
    ,'eConnConnectionString',
      // Delphi 2005:
    'eConnTDSPacketSize',
    'eConnClientHostName', 'eConnClientAppName', 'eConnCompressed', 'eConnEncrypted',
    'eConnPrepareSQL', 'eConnDecimalSeparator',
    // Delphi 2007
    'e41',
    'eVendorProperty'
  );

  cSQLMetaDataOption: array[TXSQLMetaDataOption] of AnsiString = (
    'eMetaCatalogName', 'eMetaSchemaName', 'eMetaDatabaseName',
    'eMetaDatabaseVersion', 'eMetaTransactionIsoLevel', 'eMetaSupportsTransaction',
    'eMetaMaxObjectNameLength', 'eMetaMaxColumnsInTable', 'eMetaMaxColumnsInSelect',
    'eMetaMaxRowSize', 'eMetaMaxSQLLength', 'eMetaObjectQuoteChar',
    'eMetaSQLEscapeChar', 'eMetaProcSupportsCursor', 'eMetaProcSupportsCursors',
    'eMetaSupportsTransactions',
    {.$IFDEF _D7UP_}
    'eMetaPackageName',
    {.$ENDIF}
    // Delphi 2005:
    'eMetaDefaultSchemaName'
  );

  cSQLCommandOption: array[TXSQLCommandOption] of AnsiString = (
    'eCommRowsetSize', 'eCommBlobSize', 'eCommBlockRead', 'eCommBlockWrite',
    'eCommParamCount', 'eCommNativeHandle', 'eCommCursorName', 'eCommStoredProc',
    'eCommSQLDialect', 'eCommTransactionID'
    {.$IFDEF _D7UP_}
    , 'eCommPackageName', 'eCommTrimChar',
    'eCommQualifiedName', 'eCommCatalogName', 'eCommSchemaName', 'eCommObjectName',
    'eCommQuotedObjectName',
    {.$ENDIF}
    // Delphi 2005:
    'eCommPrepareSQL', 'eCommDecimalSeparator'
  );

  cSTMTParamType: array[TSTMTParamType] of AnsiString = (
    'paramUNKNOWN', 'paramIN', 'paramOUT', 'paramINOUT', 'paramRET'
  );

  cSQLCursorOption: array[TSQLCursorOption] of AnsiString = (
    'eCurObjectAttrName', 'eCurObjectTypeName', 'eCurParentFieldID'
  );

  cOdbcDriverType: array[TOdbcDriverType] of AnsiString = (
    'eOdbcDriverTypeUnspecified',
    'eOdbcDriverTypeGupta', 'eOdbcDriverTypeMsSqlServer', 'eOdbcDriverTypeMsSqlServer2005Up',
    'eOdbcDriverTypeIbmDb2', 'eOdbcDriverTypeIbmDb2AS400',
    'eOdbcDriverTypeMsJet',
    'eOdbcDriverTypeMySql', 'eOdbcDriverTypeMySql3',
    'eOdbcDriverTypeInterbase', 'eOdbcDriverTypeInformix',
    'eOdbcDriverTypeOracle', 'eOdbcDriverTypeSybase',
    'eOdbcDriverTypeSQLLite', 'eOdbcDriverTypeThinkSQL', 'eOdbcDriverTypeMerantOle',
    'eOdbcDriverTypePervasive', 'eOdbcDriverTypeNexusDbFlashFiler', 'eOdbcDriverTypePostgreSQL',
    'eOdbcDriverTypeInterSystemCache', 'eOdbcDriverTypeMerantDBASE',  'eOdbcDriverTypeSAPDB',
    'eOdbcDriverTypeParadox', 'eOdbcDriverTypeFoxPro', 'eOdbcDriverTypeClipper',
    'eOdbcDriverTypeBtrieve', 'eOdbcDriverTypeOpenIngres', 'eOdbcDriverTypeProgress',
    'eOdbcDriverTypeOterroRBase'
  );

  cDbmsType: array[TDbmsType] of AnsiString = (
    'eDbmsTypeUnspecified',
    'eDbmsTypeGupta', 'eDbmsTypeMsSqlServer', 'eDbmsTypeMsSqlServer2005Up',
    'eDbmsTypeIbmDb2', 'eDbmsTypeIbmDb2AS400',
    'eDbmsTypeMySql', 'eDbmsTypeMySqlMax',
    'eDbmsTypeMsAccess', 'eDbmsTypeExcel', 'eDbmsTypeText', 'eDbmsTypeDBase', 'eDbmsTypeParadox',
    'eDbmsTypeOracle', 'eDbmsTypeInterbase', 'eDbmsTypeInformix', 'eDbmsTypeSybase',
    'eDbmsTypeSQLLite', 'eDbmsTypeThinkSQL', 'eDbmsTypeSapDb', 'eDbmsTypePervasiveSQL',
    'eDbmsTypeFlashFiler', 'eDbmsTypePostgreSQL', 'eDbmsTypeInterSystemCache',
    'eDbmsTypeFoxPro', 'eDbmsTypeClipper', 'eDbmsTypeBtrieve', 'eDbmsTypeOpenIngres',
    'eDbmsTypeProgress', 'eDbmsTypeOterroRBase'
   );

function GetTransactionDescStr(TranID: pTTransactionDesc):AnsiString;

implementation

function GetCurThreadInfoStr: AnsiString; {$IFDEF _INLINE_} inline; {$ENDIF}
var
 tID: WORD;
begin
  tID := GetCurrentThreadID;
  if tID = MainThreadID then
    Result := ':(    0) '
  else
    Result := ':('+AnsiString(Format('%5d',[tID]))+') ';
  {$IFDEF LINUX}
  // add process id info:
  //  Result := '['+Format('%10d',[DWORD(getpid)])+']' + Result;
  {$ENDIF}
end;

procedure OutputDebugString(const S:AnsiString); {$IFDEF _INLINE_} inline; {$ENDIF}
{$IFDEF LINUX}
var
  sTI: AnsiString;
{$ENDIF}
begin
  {$IFDEF LINUX}
    sTI := GetCurThreadInfoStr;
    __write(stderr, sTI, Length(sTI));
    __write(stderr, S,   Length(S)  );
    __write(stderr, EOL, Length(EOL));
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Windows.OutputDebugStringA(PAnsiChar(GetCurThreadInfoStr+S));
  {$ENDIF}
end;

procedure OutputDebugString(const ws:WideString); {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  OutputDebugString(AnsiString(ws));
end;

const
  cBoolean: array[Boolean] of AnsiString  = ('False', 'True');

function BoolToStr;//(Value: Boolean): AnsiString;
const
  cBoolean: array[Boolean] of AnsiString  = ('False', 'True');
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function ValueToStr(const Value: array of const): AnsiString;
var
  pVarRec: ^TVarRec;
begin
  Result := '';
  if Length(Value) = 0 then
    exit;
  try
    pVarRec := @Value[0];
    case pVarRec.VType of
      vtInteger:
        Result := AnsiString(IntToStr(pVarRec.VInteger));
      vtBoolean:
        Result := cBoolean[pVarRec.VBoolean];
      vtChar   :
        Result := pVarRec.VChar;
      vtExtended:
        Result := AnsiString(FloatToStr(pVarRec.VExtended^));
      vtPointer:
        Result := AnsiString(format('$%x',[Integer(pVarRec.VPointer)]));
      vtPChar,
      vtAnsiString  :
        if Assigned(pVarRec.VPChar)  then
          Result := pVarRec.VPChar
        else
          Result := '';
      vtObject :
        if Assigned(pVarRec.VObject) then
          Result := AnsiString(format('$%x ClassName :%s',[Integer(@pVarRec.VObject), pVarRec.VObject.ClassName]))
        else
          Result := '';
      vtClass  :
         if Assigned(pVarRec.VClass) then
           Result := AnsiString(format('ClassReference :%s',[pVarRec.VClass.ClassName]))
         else
           Result := '';
      vtWideChar   :
        Result := AnsiString(pVarRec.VWideChar);
      vtPWideChar  :
        if Assigned( pVarRec.VPWideChar ) then
          Result := AnsiString(WideString(pVarRec.VPWideChar))
        else
          Result := '';
      vtCurrency   :
        if Assigned(pVarRec.VCurrency) then
          Result := AnsiString(FloatToStr(pVarRec.VCurrency^))
        else
          Result := '';
      vtVariant    :
        if Assigned(pVarRec.VVariant) then
          Result := 'Variant Assigend'
        else
          Result := '';
      vtInterface  :
        if Assigned(pVarRec.VInterface) then
          Result := AnsiString(format('Interface address: $%x',[Integer(pVarRec.VInterface)]))
        else
          Result := '';
      vtWideString :
        if Assigned(pVarRec.VWideString) then
          Result := AnsiString(WideString(pVarRec.VWideString))
        else
          Result := '';
      vtInt64:
        Result := AnsiString(IntToStr(pVarRec.VInt64^));
      {$IFDEF _D12UP_}
      vtUnicodeString:
        Result := AnsiString(string(pVarRec.vUnicodeString));
      {$ENDIF}
      else
        Exit;
    end;
  except
    on e: Exception do
    begin
      OutputDebugString(AnsiString('*** TRACCER INTERNAL ERROR: (function ValueToStr). Detail: ') + AnsiString(e.Message));
    end;
  end;
end;

function ArrayToStr(const Consts: array of Const; const sSeparator :AnsiString = ' ';
  const bQuoteData:boolean = True): AnsiString;
var
  i:integer;
  sV:AnsiString;
  pVarRec: ^TVarRec;
begin
  Result := '';
  if Length(Consts) = 0 then
    exit;
  for i := 0 to Length(Consts) - 1 do
  try
    pVarRec := @Consts[i];
    case pVarRec.VType of
      vtInteger:
        sV := AnsiString(IntToStr(pVarRec.VInteger));
      vtBoolean:
        sV := cBoolean[pVarRec.VBoolean];
      vtChar   :
        sV := pVarRec.VChar;
      vtPointer:
        sV := AnsiString(format('$%x',[Integer(pVarRec.VPointer)]));
      vtPChar,
      vtAnsiString  :
        if Assigned(pVarRec.VPChar)  then
          sV := pVarRec.VPChar
        else
          sV := '';
      vtObject :
        if Assigned(pVarRec.VObject) then
          sV := AnsiString(format('$%x ClassName :%s',[Integer(@pVarRec.VObject), pVarRec.VObject.ClassName]))
        else
          sV := '';
      vtClass  :
         if Assigned(pVarRec.VClass) then
           sV := AnsiString(format('ClassReference :%s',[pVarRec.VClass.ClassName]))
         else
           sV := '';
      vtWideChar   :
        sV := AnsiString(pVarRec.VWideChar);
      vtPWideChar  :
        if Assigned( pVarRec.VPWideChar ) then
          sV := AnsiString(WideString(pVarRec.VPWideChar))
        else
          sV := '';
      vtCurrency   :
        if Assigned(pVarRec.VCurrency) then
          sV := AnsiString(FloatToStr(pVarRec.VCurrency^))
        else
          sV := '';
      vtVariant    :
        if Assigned(pVarRec.VVariant) then
          sV := 'Variant Assigend'
        else
          sV := '';
      vtInterface  :
        if Assigned(pVarRec.VInterface) then
          sV := AnsiString(format('Interface address: $%x',[Integer(pVarRec.VInterface)]))
        else
          sV := '';
      vtWideString :
        if Assigned(pVarRec.VWideString) then
          sV := AnsiString(WideString(pVarRec.VWideString))
        else
          sV := '';
      vtInt64:
        sV := AnsiString(IntToStr(pVarRec.VInt64^));
      {$IFDEF _D12UP_}
      vtUnicodeString:
        sV := AnsiString(string(pVarRec.vUnicodeString));
      {$ENDIF}
      else
        Exit;
    end;
    if bQuoteData and (i mod 2 = 1) then
      sV := '"'+sV+'"';
    Insert(sV, Result, Length(Result)+1);
    if i < Length(Consts) - 1 then
      Insert(sSeparator, Result, Length(Result) + 1);
  except
    on e: Exception do
    begin
      OutputDebugString(AnsiString('*** TRACCER INTERNAL ERROR: (function ArrayToStr). Detail: ') + AnsiString(e.Message));
    end;
  end;
end;

threadvar
  bExceptionFlag: Integer{ = 0};

procedure LogEnterProc(const ProcName: AnsiString; const Params: AnsiString=''); overload;
begin
  inc(bExceptionFlag);

  if Params = '' then
    OutputDebugString('->:' + ProcName + ';')
  else
    OutputDebugString('->:' + ProcName + ',  Params: ' + Params + ';');
end;

procedure LogEnterProc(const ProcName: AnsiString; const Params: array of const); overload;
begin
  LogEnterProc(ProcName, ArrayToStr(Params));
end;

procedure LogInfoProc(const Params: AnsiString=''); overload; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if Params <> '' then
    OutputDebugString('// '+' Info: ' + Params + ';');
end;

procedure LogInfoProc(const Params: array of const); overload;
begin
  LogInfoProc(ArrayToStr(Params));
end;

procedure LogExitProc(const ProcName: AnsiString; const ExitInfo: AnsiString=''); overload;
begin
  if bExceptionFlag > 0 then
    dec(bExceptionFlag);

  if ExitInfo = '' then
    OutputDebugString('~-:' + ProcName + ';')
  else
    OutputDebugString('~-:' + ProcName+',  ExitInfo: ' + ExitInfo + ';') ;
end;

procedure LogExitProc(const ProcName: AnsiString; const ExitInfo: array of const); overload;
begin
  LogExitProc(ProcName, ArrayToStr(ExitInfo));
end;

procedure LogExceptProc(const ProcName: AnsiString; E:Exception; const ExceptInfo: AnsiString=''); overload;

  function GetExceptionInfoStr(E:Exception):AnsiString;
  var
    LastError :Integer;
  begin
    Result :=  AnsiString('  ' + E.ClassName+': ' + E.Message + ';');
    LastError := GetLastError;
    if LastError <> 0 then
      Result := Result + AnsiString('  SystemError: ')
        + AnsiString(SysErrorMessage(LastError)) + AnsiChar(';');
  end;

var
  ei: AnsiString;
begin
  if bExceptionFlag = 0 then
    exit;
  bExceptionFlag := 0;
  if E is EAbort then
  begin
    ei := ' Aborted;';
  end
  else
  if Assigned(E) then
    ei := GetExceptionInfoStr(E)
  else
    ei := '';

  if ExceptInfo = '' then
    OutputDebugString('##:' + ProcName + ';' + ei)
  else
    OutputDebugString('##:' + ProcName + ',  ExceptInfo: ' + ExceptInfo + ';' + ei) ;
end;

procedure LogExceptProc(const ProcName: AnsiString; E:Exception; const ExceptInfo: array of const); overload;
begin
  LogExceptProc(ProcName, E, ArrayToStr(ExceptInfo));
end;

function GetTransactionDescStr(TranID: pTTransactionDesc): AnsiString;
begin
  if TranID = nil then
    Result := ''
  else
  begin
    case TranId.IsolationLevel of
      xilREPEATABLEREAD:
        // Dirty reads and nonrepeatable reads are not possible. Phantoms are possible
        Result := 'xilREPEATABLEREAD(SQL_TXN_REPEATABLE_READ)';
      xilREADCOMMITTED:
        // Dirty reads are not possible. Nonrepeatable reads and phantoms are possible
        Result := 'xilREADCOMMITTED(SQL_TXN_READ_COMMITTED)';
      xilDIRTYREAD:
        // Dirty reads, nonrepeatable reads, and phantoms are possible.
        Result := 'xilDIRTYREAD(SQL_TXN_READ_UNCOMMITTED)';
      xilCUSTOM:
        // Custom Level
        Result := AnsiString('xilCUSTOM('+IntToStr(TranID.CustomIsolation)+')');
      else
        Result := AnsiString('Unknown IsolationLevel:' + IntToStr(Integer(TranId.IsolationLevel)));
    end;
  end;
end;

{$IFDEF _TRACE_CALLS_}
initialization
  OutputDebugString('*** Debug Start :'+ParamStr(0));
finalization
  OutputDebugString('*** Debug Done  :'+ParamStr(0));
{$ENDIF}
end.
