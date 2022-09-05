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

unit CnDHibernateBackupRestore; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate�ؼ���
* ��Ԫ���ƣ����ݻ�ԭ�߳̿ؼ���Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
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
  Windows, Messages, SysUtils, Classes, CnDHibernateSQLThread;

type
  TCnDHibernateBackupRestore = class(TComponent)
  private
    FAbout: string;
    FBackupFileName: string;
    FDatabaseName: string;
    FUserPwd: string;
    FUserID: string;
    FDBHost: string;
    FOnFinishBackup: TNotifyEvent;
    FLogicDatabaseName: string;
    FLogicLogName: string;
    FOnFinishRestore: TNotifyEvent;
    FOnBeginBackup: TNotifyEvent;
    FOnBeginRestore: TNotifyEvent; 
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Backup;
    procedure Restore;
  published
    property About: string read FAbout write FAbout;
    property DBHost: string read FDBHost write FDBHost;
    property UserID: string read FUserID write FUserID;
    property UserPwd: string read FUserPwd write FUserPwd;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property BackupFileName: string read FBackupFileName write FBackupFileName;
    property OnBeginBackup: TNotifyEvent read FOnBeginBackup write FOnBeginBackup;
    property OnFinishBackup: TNotifyEvent read FOnFinishBackup write FOnFinishBackup;
    property LogicDatabaseName: string read FLogicDatabaseName write FLogicDatabaseName;
    property LogicLogName: string read FLogicLogName write FLogicLogName;
    property OnBeginRestore: TNotifyEvent read FOnBeginRestore write FOnBeginRestore;
    property OnFinishRestore: TNotifyEvent read FOnFinishRestore write FOnFinishRestore;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateBackupRestore }

procedure TCnDHibernateBackupRestore.Backup;
var
  backupThread: TCnCustomSQLBackupThread;
begin
  if Assigned(OnBeginBackup) then
    OnBeginBackup(Self);
  backupThread := TCnCustomSQLBackupThread.Create(True);
  with backupThread do
  begin
    FreeOnTerminate := True;
    DBHost := self.FDBHost;
    UserID := self.FUserID;
    UserPwd := self.FUserPwd;
    DatabaseName := self.FDatabaseName;
    BackupFileName := self.FBackupFileName;
    if Assigned(OnFinishBackup) then
      OnFinish := OnFinishBackup;
    Execute;
  end;
end;

constructor TCnDHibernateBackupRestore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCnDHibernateBackupRestore.Destroy;
begin
  inherited Destroy;
end;

procedure TCnDHibernateBackupRestore.Restore;
var
  RestoreThread: TCnCustomSQLRestoreThread;
begin
  if Assigned(OnBeginRestore) then
    OnBeginRestore(Self);
  RestoreThread := TCnCustomSQLRestoreThread.Create(True);
  with RestoreThread do
  begin
    FreeOnTerminate := True;
    DBHost := self.FDBHost;
    UserID := self.FUserID;
    UserPwd := self.FUserPwd;
    DatabaseName := self.FDatabaseName;
    BackupFileName := self.FBackupFileName;
    LogicDatabaseName := Self.FLogicDatabaseName;
    LogicLogName := Self.FLogicLogName;
    if Assigned(OnFinishRestore) then
      OnFinish := OnFinishRestore;
    Execute;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
