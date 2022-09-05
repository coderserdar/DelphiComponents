{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDHibernateSQLThread;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼���
* ��Ԫ���ƣ����ݻ�ԭ�߳��൥Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
*
*  Example for using this unit
*  procedure BackupDB;
*  var
*    BackupThread : TCnCustomSQLBackupThread;
*  begin
*    BackupThread := TCnCustomSQLBackupThread.Create(true);
*    with BackupThread do
*    begin
*      FreeOnTerminate := true;
*      DBHost := '127.0.0.1';
*      UserID := 'sa';
*      UserPwd := '';
*      DatabaseName := '';
*      BackupFileName := 'C:\MyBackup';
*      Execute;
*    end;
*  end;
*
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils, DB, ADODB, Windows, ShellAPI, Registry;

{$M+}

type
  { SQL Server operations state }
  TCnSQLStates = (ssSuccess, ssFail);

  { thread of backup SQL Server database }
  TCnCustomSQLBackupThread = class(TThread)
  private
    FDatabaseName: string;
    FUserPwd: string;
    FBackupFileName: string;
    FUserID: string;
    FDBHost: string;
    FBackupState: TCnSQLStates;
    FErrorMessage: string;
    FOnFinish: TNotifyEvent;
  protected
    { Run thread }
  public
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
    property BackupState: TCnSQLStates read FBackupState;
    property ErrorMessage: string read FErrorMessage;
  published
    { The host name of SQL Server }
    property DBHost: string read FDBHost write FDBHost;
    { The user name for login SQL Server }
    property UserID: string read FUserID write FUserID;
    { The user password for login SQL Server}
    property UserPwd: string read FUserPwd write FUserPwd;
    { The database's name }
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    { Where you put the backup file at }
    property BackupFileName: string read FBackupFileName write FBackupFileName;
    { finish thread }
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  { thread of restore SQL Server database }
  TCnCustomSQLRestoreThread = class(TThread)
  private
    FDatabaseName: string;
    FUserPwd: string;
    FBackupFileName: string;
    FUserID: string;
    FDBHost: string;
    FBackupState: TCnSQLStates;
    FErrorMessage: string;
    FLogicDatabaseName: string;
    FLogicLogName: string;
    FOnFinish: TNotifyEvent;
  protected
    { Run thread }
  public
    constructor Create(CreateSuspended: Boolean);
    procedure Execute; override;
    property BackupState: TCnSQLStates read FBackupState;
    property ErrorMessage: string read FErrorMessage;
  published
    { The host name of SQL Server }
    property DBHost: string read FDBHost write FDBHost;
    { The user name for login SQL Server }
    property UserID: string read FUserID write FUserID;
    { The user password for login SQL Server}
    property UserPwd: string read FUserPwd write FUserPwd;
    { The database's name }
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    { Where you put the backup file at }
    property BackupFileName: string read FBackupFileName write FBackupFileName;
    { The logic database file name }
    property LogicDatabaseName: string read FLogicDatabaseName write FLogicDatabaseName;
    { The logic log file name }
    property LogicLogName: string read FLogicLogName write FLogicLogName;
    { finish thread }
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  { Global methods }

  { whether SQL Server installed or not }
function SQLServerInstalled: Boolean;
{ get SQL Server install path if installed }

function GetSQLServerPath: string;
{ Services operations }

procedure SQLServicesOperation(CommandStr: string);
{ server data root }

function GetSQLServerDataRoot: string;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

resourcestring
  { connection string }
  ConnStr           =
    'Provider=SQLOLEDB.1;' +
    'Data Source=%s;' +                 // Host Name
  'Persist Security Info=True;' +
    'User ID=%s;' +                     // User Name
  'Password=%s;' +                      // User Password
  'Initial Catalog=%s';                 // DatabaseName
  { backup database T-SQL codes }
  BackupStr         =
    'BACKUP DATABASE [%s] TO ' +        // Database Name
  'DISK=N'#39'%s'#39' WITH NOINIT,' +   // Physics File Name
  'NOUNLOAD,NOSKIP,STATS=10,NOFORMAT';
  { restore database T-SQL codes }
  RestoreStr        =
    'RESTORE DATABASE [%s] FROM ' +     // Database Name
  'DISK=N'#39'%s'#39' WITH FILE=1,' +   // Physics File Name
  'NOUNLOAD,STATS=10,RECOVERY,' +
    'MOVE '#39'%s'#39' to '#39'%s'#39',' + // Logic Database Name
  'MOVE '#39'%s'#39' to '#39'%s'#39;    // Logic Log Name
  { restore database without move file T-SQL codes }
  RestoreStr2       =
    'RESTORE DATABASE [%s] FROM ' +     // Database Name
  'DISK=N'#39'%s'#39' WITH FILE=1,' +   // Physics File Name
  'NOUNLOAD,STATS=10,RECOVERY';

  { Global methods }

  /// <summary>
  /// whether SQL Server installed
  /// </summary>
  /// <returns>installed(true)/not installed(false)</returns>
function SQLServerInstalled: Boolean;
var
  Reg: TRegistry;
  Path: string;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKey('SOFTWARE\Microsoft\MSSQLServer\Setup', true);
  Path := Reg.ReadString('SQLPath');
  if (Path <> '') and (DirectoryExists(Path)) then
    Result := true
  else
    Result := False;
  Reg.CloseKey;
  Reg.Free;
end;

/// <summary>
/// get SQL Server installed path
/// </summary>
/// <returns>path(if installed)</returns>
function GetSQLServerPath: string;
var
  Reg: TRegistry;
  Path: string;
begin
  if not SQLServerInstalled then
  begin
    Result := '';
    Exit;
  end;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKey('SOFTWARE\Microsoft\MSSQLServer\Setup', true);
  Path := Reg.ReadString('SQLPath');
  Result := Path + '\Data\';
  Reg.CloseKey;
  Reg.Free;
end;

/// <summary>
/// do operations order with commandstr
/// </summary>
/// <param name="CommandStr">command line</param>
procedure SQLServicesOperation(CommandStr: string);
var
  sCommandLine: string;
  bCreateProcess: Boolean;
  lpStartupInfo: TStartupInfo;
  lpProcessInformation: TProcessInformation;
begin
  sCommandLine := CommandStr;
  // fill record space
  FillChar(lpStartupInfo, SizeOf(TStartupInfo), #0);
  lpStartupInfo.cb := SizeOf(TStartupInfo);
  lpStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  lpStartupInfo.wShowWindow := SW_HIDE;
  // create process
  bCreateProcess := CreateProcess(nil, PChar(sCommandLine), nil, nil, true, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, lpStartupInfo, lpProcessInformation);
  // wait for process finished
  if bCreateProcess then
    WaitForSingleObject(lpProcessInformation.hProcess, INFINITE);
end;

function GetSQLServerDataRoot: string;
var
  Reg: TRegistry;
  Path: string;
begin
  if not SQLServerInstalled then
  begin
    Result := '';
    Exit;
  end;
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKey('SOFTWARE\Microsoft\MSSQLServer\Setup', true);
  Path := Reg.ReadString('SQLDataRoot');
  Result := Path + '\Data\';
  Reg.CloseKey;
  Reg.Free;
end;

{ TCustomDBBackupRestoreThread }

constructor TCnCustomSQLBackupThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  // if you want to get the state when thread finished,
  // add a method of type TNotifyEvent and bind it to:
  // OnTerminate event of this class.
end;

procedure TCnCustomSQLBackupThread.Execute;
var
  Qry: TADOQuery;
begin
  Qry := TADOQuery.Create(nil);
  with Qry do
  begin
    ConnectionString := Format(ConnStr, [DBHost, UserID, UserPwd, DatabaseName]);
    SQL.Text := Format(BackupStr, [DatabaseName, BackupFileName]);
    try
      ExecSQL;
      FBackupState := ssSuccess;
      FErrorMessage := '';
    except
      on E: Exception do
      begin
        FBackupState := ssFail;
        FErrorMessage := E.Message;
      end;
    end;
    Free;
  end;
  if Assigned(onFinish) then
    OnFinish(Self);
end;

{ TCnCustomSQLRestoreThread }

constructor TCnCustomSQLRestoreThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  // if you want to get the state when thread finished,
  // add a method of type TNotifyEvent and bind it to:
  // OnTerminate event of this class.
end;

procedure TCnCustomSQLRestoreThread.Execute;
var
  Qry: TADOQuery;
  phyData: string;
  phyLog: string;
begin
  // Stop SQL Server Services
  // this operation may let all user connects logoff.
  SQLServicesOperation('net stop MSSQLSERVER');
  // restart SQL Server
  SQLServicesOperation('net start MSSQLSERVER');
  // Restore database
  Qry := TADOQuery.Create(nil);
  with Qry do
  begin
    // if you want to restore "master"
    // change "master" to other Database's name
    phyData := GetSQLServerDataRoot + LogicDatabaseName;
    phyLog := GetSQLServerDataRoot + LogicLogName;
    ConnectionString := Format(ConnStr, [DBHost, UserID, UserPwd, 'master']);
    SQL.Text := Format(RestoreStr, [DatabaseName, BackupFileName, LogicDatabaseName, phyData, LogicLogName, phyLog]);
    try
      ExecSQL;
      FBackupState := ssSuccess;
      FErrorMessage := '';
    except
      SQL.Text := Format(RestoreStr2, [DatabaseName, BackupFileName]);
      try
        ExecSQL;
        FBackupState := ssSuccess;
        FErrorMessage := '';
      except
        on E: Exception do
        begin
          FBackupState := ssFail;
          FErrorMessage := E.Message;
        end;
      end;
    end;
    Free;
  end;
  if Assigned(onFinish) then
    OnFinish(Self);
end;

{$ENDIF SUPPORT_ADO}
end.
