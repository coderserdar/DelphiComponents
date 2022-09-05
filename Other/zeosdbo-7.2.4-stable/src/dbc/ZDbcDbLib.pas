{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               DBLib Connectivity Classes                }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcDbLib;

interface

{$I ZDbc.inc}

uses
{$IFDEF FPC}
  {$IFDEF WIN32}
    Comobj,
  {$ENDIF}
{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcConnection, ZDbcIntfs, ZCompatibility, ZDbcLogging, ZPlainDbLibDriver,
  ZPlainDbLibConstants, ZTokenizer, ZGenericSqlAnalyser, ZURL;

type
  TDBLibProvider = (dpMsSQL, dpSybase);

  {** Implements DBLib Database Driver. }
  {$WARNINGS OFF}
  TZDBLibDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;
  {$WARNINGS ON}

  {** Represents a DBLib specific connection interface. }
  IZDBLibConnection = interface (IZConnection)
    ['{6B0662A2-FF2A-4415-B6B0-AAC047EA0671}']

    function FreeTDS: Boolean;
    function GetProvider: TDBLibProvider;
    function GetPlainDriver: IZDBLibPlainDriver;
    function GetConnectionHandle: PDBPROCESS;
    function GetServerAnsiCodePage: Word;
    procedure InternalExecuteStatement(const SQL: RawByteString);
    procedure CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: RawByteString);
  end;

  {** Implements a generic DBLib Connection. }
  TZDBLibConnection = class(TZAbstractConnection, IZDBLibConnection)
  private
    FProvider: TDBLibProvider;
    FFreeTDS: Boolean;
    FServerAnsiCodePage: Word;
    FPlainDriver: IZDBLibPlainDriver;
    function FreeTDS: Boolean;
    function GetProvider: TDBLibProvider;
    procedure InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
    procedure DetermineMSDateFormat;
    function DetermineMSServerCollation: String;
    function DetermineMSServerCodePage(const Collation: String): Word;
  protected
    FHandle: PDBPROCESS;
    procedure InternalCreate; override;
    procedure InternalExecuteStatement(const SQL: RawByteString); virtual;
    procedure InternalLogin; virtual;
    function GetPlainDriver: IZDBLibPlainDriver;
    function GetConnectionHandle: PDBPROCESS;
    procedure CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: RawbyteString); virtual;
    function GetServerCollation: String;
  public
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetReadOnly(ReadOnly: Boolean); override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    function GetWarnings: EZSQLWarning; override;
    procedure ClearWarnings; override;
    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;
    function GetServerAnsiCodePage: Word;
  end;

var
  {** The common driver manager object. }
  DBLibDriver: IZDriver;

implementation

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF}
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZSysUtils, ZMessages, ZDbcUtils, ZDbcDbLibStatement, ZEncoding, ZFastCode,
  ZDbcDbLibMetadata, ZSybaseToken, ZSybaseAnalyser{$IFDEF OLDFPC}, ZClasses{$ENDIF};

var
  DBLIBCriticalSection: TCriticalSection;

  { TZDBLibDriver }

{**
  Constructs this object with default properties.
}
constructor TZDBLibDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZDBLibMSSQL7PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZDBLibSybaseASE125PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS42MsSQLPlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS42SybasePlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS50PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS70PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS71PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZFreeTDS72PlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
}
{$WARNINGS OFF}
function TZDBLibDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := nil;
  DBLIBCriticalSection.Enter;
  try
    Result := TZDBLibConnection.Create(Url);
  finally
    DBLIBCriticalSection.Release
  end;
end;
{$WARNINGS ON}

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZDBLibDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZDBLibDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZDBLibDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZSybaseTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZDBLibDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZSybaseStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZDBLibConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZDBLibConnection.InternalCreate;
begin
  FDisposeCodePage := False;
  if ZFastCode.Pos('mssql', LowerCase(Url.Protocol)) > 0  then
  begin
    FMetadata := TZMsSqlDatabaseMetadata.Create(Self, Url);
    FProvider := dpMsSQL;
  end
  else
    if ZFastCode.Pos('sybase', LowerCase(Url.Protocol)) > 0 then
    begin
      FMetadata := TZSybaseDatabaseMetadata.Create(Self, Url);
      FProvider := dpSybase;
    end
    else
      FMetadata := nil;
  FFreeTDS := ZFastCode.Pos('FreeTDS', Url.Protocol) > 0;

  FHandle := nil;
end;

{**
  Destroys this object and cleanups the memory.
}
function TZDBLibConnection.FreeTDS: Boolean;
begin
  Result := FFreeTDS;
end;

function TZDBLibConnection.GetProvider: TDBLibProvider;
begin
  Result := FProvider;
end;

{**
  Executes simple statements internally.
}
procedure TZDBLibConnection.InternalExecuteStatement(const SQL: RawByteString);
var
  ASQL: RawByteString;
begin
  FHandle := GetConnectionHandle;
  if GetPlainDriver.dbCancel(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcExecute, SQL);
  if FProvider = dpMsSQL then
    ASQL := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(Sql, '\'#13, '\\'#13, [rfReplaceAll])
  else
    ASQL := SQL;

  if (GetPlainDriver.dbcmd(FHandle, Pointer(ASQL)) <> DBSUCCEED) or
     (GetPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) then
    CheckDBLibError(lcExecute, ASQL);
  repeat
    GetPlainDriver.dbresults(FHandle);
    GetPlainDriver.dbcanquery(FHandle);
  until GetPlainDriver.dbmorecmds(FHandle) = DBFAIL;
  CheckDBLibError(lcExecute, ASQL);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
end;

{**
  Login procedure can be overriden for special settings.
}
procedure TZDBLibConnection.InternalLogin;
var
  Loginrec: PLOGINREC;
  LogMessage: RawByteString;
  S: string;
  lLogFile  : String;
begin
  LogMessage := 'CONNECT TO "'+AnsiString(HostName)+'"';
  LoginRec := GetPLainDriver.dbLogin;
  try
//Common parameters
    S := Info.Values['workstation'];
    if S <> '' then
      GetPlainDriver.dbSetLHost(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['appname'];
    if S <> '' then
      GetPlainDriver.dbSetLApp(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['language'];
    if S <> '' then
      GetPlainDriver.dbSetLNatLang(LoginRec, PAnsiChar(AnsiString(S)));

    S := Info.Values['timeout'];
    if S <> '' then
      GetPlainDriver.dbSetLoginTime(StrToIntDef(S, 60));

    if FFreeTDS then
    begin
      if StrToBoolEx(Info.Values['log']) or StrToBoolEx(Info.Values['logging']) or
         StrToBoolEx(Info.Values['tds_dump']) then begin
           lLogFile := Info.Values['logfile'];
           if lLogFile = '' then
            lLogFile := Info.Values['log_file'];
           if lLogFile = '' then
            lLogFile := Info.Values['tds_dump_file'];
           if lLogFile = '' then
            lLogFile := ChangeFileExt(ParamStr(0), '.tdslog');
           (GetPlainDriver as IZFreeTDSPlainDriver).tdsDump_Open(lLogFile);
         end;
    end;


    //mssql specific parameters
    if ( FProvider = dpMsSQL ) then
    begin
      if ( StrToBoolEx(Info.Values['NTAuth']) or StrToBoolEx(Info.Values['trusted'])
        or StrToBoolEx(Info.Values['secure']) ) and ( not FFreeTDS ) then
      begin
        GetPlainDriver.dbsetlsecure(LoginRec);
        LogMessage := LogMessage + ' USING WINDOWS AUTHENTICATION';
      end
      else
      begin
        GetPlainDriver.dbsetluser(LoginRec, PAnsiChar(ConSettings^.User));
        GetPlainDriver.dbsetlpwd(LoginRec, PAnsiChar(AnsiString(Password)));
        LogMessage := LogMessage + ' AS USER "'+ConSettings^.User+'"';
      end;

      if FFreeTDS then begin
        S := Info.Values['codepage'];
        if S <> '' then begin
          GetPlainDriver.dbSetLCharSet(LoginRec, Pointer({$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S)));
          CheckCharEncoding(s);
        end;
      end;
    end;

    //sybase specific parameters
    if FProvider = dpSybase then
    begin
      S := Info.Values['codepage'];
      if S <> '' then
        GetPlainDriver.dbSetLCharSet(LoginRec, Pointer({$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(S)));
      GetPlainDriver.dbsetluser(LoginRec, PAnsiChar(ConSettings^.User));
      GetPlainDriver.dbsetlpwd(LoginRec, PAnsiChar(AnsiString(Password)));
        LogMessage := LogMessage + ' AS USER "'+ConSettings^.User+'"';
    end;

    CheckDBLibError(lcConnect, LogMessage);
    s := HostName;
    // add port number if FreeTDS is used, the port number was specified and no server instance name was given:
    if FreeTDS and (Port <> 0) and (ZFastCode.Pos('\', HostName) = 0)  then s := s + ':' + ZFastCode.IntToStr(Port);
    FHandle := GetPlainDriver.dbOpen(LoginRec, PAnsiChar(AnsiString(s)));
    CheckDBLibError(lcConnect, LogMessage);
    if not Assigned(FHandle) then raise EZSQLException.Create('The connection to the server failed, no proper handle was returned. Insufficient memory, unable to connect for any reason. ');

    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);
  finally
    GetPLainDriver.dbLoginFree(LoginRec);
  end;
end;

function TZDBLibConnection.GetPlainDriver: IZDBLibPlainDriver;
begin
  if FPlainDriver = nil then
    FPlainDriver := PlainDriver as IZDBLibPlainDriver;
  Result := FPlainDriver;
end;

function TZDBLibConnection.GetConnectionHandle: PDBPROCESS;
begin
  if FProvider = dpMsSQL then
    if GetPlainDriver.dbDead(FHandle) then
    begin
      Closed := True;
      Open;
    end;
  Result := FHandle;
end;

procedure TZDBLibConnection.CheckDBLibError(LogCategory: TZLoggingCategory; const LogMessage: RawByteString);
begin
  try
    GetPlainDriver.CheckError(FHandle);
  except
    on E: Exception do
    begin
      DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZDBLibConnection.Open;
var
  LogMessage: RawByteString;
begin
   if not Closed then
      Exit;

  InternalLogin;

  LogMessage := 'USE '+ ConSettings^.Database;
  if GetPlainDriver.dbUse(FHandle, Pointer(ConSettings^.Database)) <> DBSUCCEED then
    CheckDBLibError(lcConnect, LogMessage);
  DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

  LogMessage := 'set textlimit=2147483647';
  if GetPlainDriver.dbsetopt(FHandle, GetPlainDriver.GetVariables.dboptions[Z_TEXTLIMIT] , '2147483647') <> DBSUCCEED then
    CheckDBLibError(lcConnect, LogMessage);
  DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

  InternalExecuteStatement('set textsize 2147483647 set quoted_identifier on');

  inherited Open;

  if not (GetTransactionIsolation in [tiNone, tiReadCommitted]) then
    InternalSetTransactionIsolation(GetTransactionIsolation);

  if not AutoCommit then
    InternalExecuteStatement('begin transaction');

  (GetMetadata.GetDatabaseInfo as IZDbLibDatabaseInfo).InitIdentifierCase(GetServerCollation);

  if (FProvider = dpMsSQL) and (not FreeTDS) then
  begin
  {note: this is a hack from a user-request of synopse project!
    Purpose is to notify Zeos all Character columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char(UTF8) encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions, reports!}
    FDisposeCodePage := True;
    ConSettings^.ClientCodePage := New(PZCodePage);
    ConSettings^.ClientCodePage^.CP := ZOSCodePage; //need a tempory CP for the SQL preparation
    ConSettings^.ClientCodePage^.Encoding := ceAnsi;
    ConSettings^.ClientCodePage^.Name := DetermineMSServerCollation;
    FServerAnsiCodePage := DetermineMSServerCodePage(ConSettings^.ClientCodePage^.Name);
    if UpperCase(Info.Values['ResetCodePage']) = 'UTF8' then
    begin
      ConSettings^.ClientCodePage^.CP := zCP_UTF8;
      ConSettings^.ClientCodePage^.Encoding := ceUTF8;
      ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := True;
    end
    else
    begin
      ConSettings^.ClientCodePage^.CP := FServerAnsiCodePage;
      ConSettings^.ClientCodePage^.IsStringFieldCPConsistent := False;
    end;
    ConSettings^.AutoEncode := True; //Must be set because we can't determine a column-codepage! e.g NCHAR vs. CHAR Fields
    SetConvertFunctions(ConSettings);
    DetermineMSDateFormat;
  end
  else
  begin
    FServerAnsiCodePage := ConSettings^.ClientCodePage^.CP;
    ConSettings^.ReadFormatSettings.DateFormat := 'yyyy/mm/dd';
    ConSettings^.ReadFormatSettings.DateTimeFormat := ConSettings^.ReadFormatSettings.DateFormat+' '+ConSettings^.ReadFormatSettings.TimeFormat;
  end;
  { EH:
  http://technet.microsoft.com/en-us/library/ms180878%28v=sql.105%29.aspx
   Using DATE and DATETIME in ISO 8601 format is multi-language supported:
   DATE Un-separated
   DATETIME as YYYY-MM-DDTHH:NN:SS
  }
  ConSettings^.WriteFormatSettings.DateFormat := 'YYYYMMDD';
  ConSettings^.WriteFormatSettings.DateTimeFormat := 'YYYY-MM-DDTHH:NN:SS';
  SetDateTimeFormatProperties(False);
  if Info.Values['ANSI_PADDING'] <> '' then
    if StrToBoolEx(Info.Values['ANSI_PADDING']) or
       (UpperCase(Info.Values['ANSI_PADDING']) = 'ON') then
      InternalExecuteStatement('SET ANSI_PADDING ON')
    else
    begin
      InternalExecuteStatement('SET ANSI_DEFAULTS OFF');
      InternalExecuteStatement('SET ANSI_PADDING OFF');
    end;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @return a new Statement object
}
function TZDBLibConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibStatement.Create(Self, Info);
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZDBLibConnection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibPreparedStatementEmulated.Create(Self, SQL, Info);
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZDBLibConnection.CreateCallableStatement(
  const SQL: string; Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZDBLibCallableStatement.Create(Self, SQL, Info);
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZDBLibConnection.SetAutoCommit(Value: Boolean);
begin
  if GetAutoCommit = Value then
    Exit;
  if not Closed and Value
  then InternalExecuteStatement('commit');
  inherited SetAutoCommit(Value);
  if not Closed and not Value then
    InternalExecuteStatement('begin transaction');
end;

const
  DBLibIsolationLevels: array[Boolean, TZTransactIsolationLevel] of AnsiString = ((
   'SET TRANSACTION ISOLATION LEVEL READ COMMITTED',
   'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
   'SET TRANSACTION ISOLATION LEVEL READ COMMITTED',
   'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ',
   'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE'),
   ('1','0','1','2','3'));

procedure TZDBLibConnection.InternalSetTransactionIsolation(Level: TZTransactIsolationLevel);
begin
  InternalExecuteStatement(DBLibIsolationLevels[FProvider = dpSybase, Level]);
end;

procedure TZDBLibConnection.DetermineMSDateFormat;
var
  Tmp: RawByteString;
begin
  Tmp := 'SELECT dateformat FROM master.dbo.syslanguages WHERE name = @@LANGUAGE';
  if (GetPlainDriver.dbcmd(FHandle, Pointer(Tmp)) <> DBSUCCEED) or
     (GetPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbnextrow(FHandle) <> REG_ROW) then
    CheckDBLibError(lcOther, Tmp)
  else
    ZSetString(PAnsiChar(GetPlainDriver.dbdata(FHandle, 1)),
      GetPlainDriver.dbDatLen(FHandle, 1), ConSettings^.ReadFormatSettings.DateFormat);
  GetPlainDriver.dbCancel(FHandle);
  if ConSettings^.ReadFormatSettings.DateFormat = 'dmy' then
    ConSettings^.ReadFormatSettings.DateFormat := 'DD/MM/YYYY'
  else if ConSettings^.ReadFormatSettings.DateFormat = 'mdy' then
    ConSettings^.ReadFormatSettings.DateFormat := 'MM/DD/YYYY'
  else
    ConSettings^.ReadFormatSettings.DateFormat := 'YYYY/MM/DD';
  ConSettings^.ReadFormatSettings.DateTimeFormat := ConSettings^.ReadFormatSettings.DateFormat+' HH:NN:SS:ZZZ';
end;

function TZDBLibConnection.DetermineMSServerCollation: String;
var
  Tmp: RawByteString;
begin
  Tmp := 'SELECT DATABASEPROPERTYEX('+
    {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(ConSettings^.Database, #39)+
    ', ''Collation'') as DatabaseCollation';
  if (GetPlainDriver.dbcmd(FHandle, Pointer(Tmp)) <> DBSUCCEED) or
     (GetPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbnextrow(FHandle) <> REG_ROW) then
    CheckDBLibError(lcOther, Tmp)
  else
    {$IFDEF UNICODE}
    Result := USASCII7ToUnicodeString(PAnsiChar(GetPlainDriver.dbdata(FHandle, 1)), GetPlainDriver.dbDatLen(FHandle, 1));
    {$ELSE}
    ZSetString(PAnsiChar(GetPlainDriver.dbdata(FHandle, 1)), GetPlainDriver.dbDatLen(FHandle, 1), Result{%H-});
    {$ENDIF}
  GetPlainDriver.dbCancel(FHandle);
end;

function TZDBLibConnection.GetServerCollation: String;
begin
  if FProvider = dpMsSQL
  then Result := DetermineMSServerCollation
  else Result := 'unknown';
end;

function TZDBLibConnection.DetermineMSServerCodePage(const Collation: String): Word;
var
  Tmp: RawByteString;
begin
  Result := High(Word);
  Tmp := 'SELECT COLLATIONPROPERTY('+
    {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiQuotedStr(
      {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Collation), #39)+
    ', ''Codepage'') as Codepage';
  if (GetPlainDriver.dbcmd(FHandle, Pointer(Tmp)) <> DBSUCCEED) or
     (GetPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbresults(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbcmdrow(FHandle) <> DBSUCCEED) or
     (GetPlainDriver.dbnextrow(FHandle) <> REG_ROW) then
    CheckDBLibError(lcOther, Tmp)
  else
  begin
    ZSetString(PAnsiChar(GetPlainDriver.dbdata(FHandle, 1)), GetPlainDriver.dbDatLen(FHandle, 1), Tmp);
    Result := RawToIntDef(Tmp, High(Word)); //see: http://sourceforge.net/p/zeoslib/tickets/119/
  end;
  GetPlainDriver.dbCancel(FHandle);
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZDBLibConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if GetTransactionIsolation = Level then
    Exit;

  if not Closed and not AutoCommit then
    InternalExecuteStatement('commit');
  inherited;
  if not Closed then begin
    InternalSetTransactionIsolation(Level);
    if not GetAutoCommit then
      InternalExecuteStatement('begin transaction');
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Commit;
begin
  if AutoCommit then
    raise Exception.Create(SCannotUseCommit);
  if not Closed then begin
    InternalExecuteStatement('commit');
    InternalExecuteStatement('begin transaction');
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZDBLibConnection.Rollback;
begin
  if AutoCommit then
    raise Exception.Create(SCannotUseRollBack);
  if not Closed then begin
    InternalExecuteStatement('rollback');
    InternalExecuteStatement('begin transaction');
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZDBLibConnection.InternalClose;
var
  LogMessage: RawByteString;
begin
  if Closed or not Assigned(PlainDriver) then
    Exit;
  if not GetPlainDriver.dbDead(FHandle) then
    InternalExecuteStatement('if @@trancount > 0 rollback');

  LogMessage := 'CLOSE CONNECTION TO "'+ConSettings^.ConvFuncs.ZStringToRaw(HostName, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)+'" DATABASE "'+ConSettings^.Database+'"';

  if GetPlainDriver.dbclose(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcDisConnect, LogMessage);
  FHandle := nil;
  DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZDBLibConnection.SetReadOnly(ReadOnly: Boolean);
begin
{ TODO -ofjanos -cAPI : I think it is not supported in this way }
  inherited;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZDBLibConnection.SetCatalog(const Catalog: string);
var
  RawCat, LogMessage: RawByteString;
begin
  if (Catalog <> '') and not Closed then
  begin
    RawCat := ConSettings^.ConvFuncs.ZStringToRaw(Catalog, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
    LogMessage := 'SET CATALOG '+RawCat;
    if FProvider = dpMsSQL then
    begin
      if GetPLainDriver.dbUse(FHandle, PAnsiChar(RawCat)) <> DBSUCCEED then
        CheckDBLibError(lcOther, LogMessage);
    end
    else
      if GetPLainDriver.dbUse(FHandle, PAnsiChar(RawCat)) <> DBSUCCEED then
        CheckDBLibError(lcOther, LogMessage);
    DriverManager.LogMessage(lcOther, ConSettings^.Protocol, LogMessage);
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZDBLibConnection.GetCatalog: string;
begin
  Result := String(GetPlainDriver.dbName(FHandle));
  CheckDBLibError(lcOther, 'GETCATALOG');
end;

{**
  Returns the first warning reported by calls on this Connection.
  <P><B>Note:</B> Subsequent warnings will be chained to this
  SQLWarning.
  @return the first SQLWarning or null
}
function TZDBLibConnection.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported for this <code>Connection</code> object.
  After a call to this method, the method <code>getWarnings</code>
    returns null until a new warning is reported for this Connection.
}
procedure TZDBLibConnection.ClearWarnings;
var
  LogMessage: RawByteString;
begin
  if Closed then
    Exit;

  if not GetPlainDriver.dbDead(FHandle) then
    InternalExecuteStatement('if @@trancount > 0 rollback');

  LogMessage := 'CLOSE CONNECTION TO "'+RawByteString(HostName)+'" DATABASE "'+ConSettings^.Database+'"';
  if GetPlainDriver.dbclose(FHandle) <> DBSUCCEED then
    CheckDBLibError(lcDisConnect, LogMessage);
  DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);

  FHandle := nil;
  inherited;
end;

function TZDBLibConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  Result := GetSQLHexString(PAnsiChar(Value), Length(Value), True);
end;

function TZDBLibConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  Result := GetSQLHexString(PAnsiChar(Value), Length(Value), True);
end;

function TZDBLibConnection.GetServerAnsiCodePage: Word;
begin
  Result := FServerAnsiCodePage;
end;

initialization
  DBLibDriver := TZDBLibDriver.Create;
  DriverManager.RegisterDriver(DBLibDriver);
  DBLIBCriticalSection := TCriticalSection.Create;
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(DBLibDriver);
  DBLibDriver := nil;
  FreeAndNil(DBLIBCriticalSection);
end.
