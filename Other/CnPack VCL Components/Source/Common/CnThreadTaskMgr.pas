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

unit CnThreadTaskMgr;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����߳��������
* ��Ԫ���ߣ��ܾ��� zjy@cnpack.org
* ��    ע��
* ����ƽ̨��Win7 + Delphi 7
* ���ݲ��ԣ�
* �� �� �����õ�Ԫ�ʹ����е��ַ����Ѿ����ػ�����ʽ
* �޸ļ�¼��
*           2011.07.06 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

type
  TCnTaskStatus = (tsWaiting, tsWorking, tsFinished, tsFailure);

  TCnTask = class;
  TCnTaskThread = class;
  TCnThreadTaskMgr = class;

  TCnExecuteTaskEvent = procedure (ATask: TCnTask) of object;

  TCnTask = class
  protected
    FStartTick: Cardinal;
    FTimeOut: Cardinal;
    FStatus: TCnTaskStatus;
    FData: Pointer;
    FOnExecute: TCnExecuteTaskEvent;
  public
    constructor Create;
    destructor Destroy; override;
    property Status: TCnTaskStatus read FStatus;
    property TimeOut: Cardinal read FTimeOut write FTimeOut;
    property Data: Pointer read FData write FData;
    property OnExecute: TCnExecuteTaskEvent read FOnExecute write FOnExecute;
  end;

  TCnTaskThread = class(TThread)
  protected
    FMgr: TCnThreadTaskMgr;   
    FTask: TCnTask;
    procedure Execute; override;
    procedure DoExecute; virtual;
    function CanTerminate: Boolean; virtual;
  public
    constructor Create(AMgr: TCnThreadTaskMgr; ATask: TCnTask); virtual;
    destructor Destroy; override;
    property Task: TCnTask read FTask;
  end;

  TCnTaskThreadClass = class of TCnTaskThread;
  
  TCnThreadTaskMgr = class
  private
    FTasks: TStringList;
    FMaxThreads: Integer;
    FThreads: TThreadList;
    FThreadCount: Integer;
    FThreadMonitor: TThread;
    FWaitTasks: TThreadList;
    FWorkingTasks: TThreadList;
    FFinishTasks: TThreadList;
    procedure SetMaxThreads(const Value: Integer);
    function GetCount: Integer;
    function GetThreadListCount(AList: TThreadList): Integer;
    function GetFinishCount: Integer;
    function GetWaitingCount: Integer;
    function GetWorkingCount: Integer;
  protected
    function GetThreadClass: TCnTaskThreadClass; virtual;
    function FindTask(ATaskId: string): TCnTask;
  public
    constructor Create;
    destructor Destroy; override;

    // ����һ������
    procedure AddTask(ATaskId: string; ATask: TCnTask);
    
    // ���ͬʱ�����߳���
    property MaxThreads: Integer read FMaxThreads write SetMaxThreads;
    // ��������
    property Count: Integer read GetCount;
    property WaitingCount: Integer read GetWaitingCount;
    property WorkingCount: Integer read GetWorkingCount;
    property FinishCount: Integer read GetFinishCount;
  end;
  
implementation

type
  TCnMonitorThread = class(TThread)
  protected
    FMgr: TCnThreadTaskMgr;      
    procedure Execute; override;
  public
    constructor Create(AMgr: TCnThreadTaskMgr);
    destructor Destroy; override;
  end;

{ TCnTask }

constructor TCnTask.Create;
begin
  FStatus := tsWaiting;
  FStartTick := 0;
  FTimeOut := 3 * 60 * 1000;
end;

destructor TCnTask.Destroy;
begin
  inherited;
end;

{ TCnTaskThread }

function TCnTaskThread.CanTerminate: Boolean;
begin
  Result := True;
end;

constructor TCnTaskThread.Create(AMgr: TCnThreadTaskMgr; ATask: TCnTask);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FTask := ATask;
  FMgr := AMgr;
  FMgr.FWaitTasks.Remove(FTask);
  FMgr.FWorkingTasks.Add(FTask);
  FMgr.FThreads.Add(Self);
  if FMgr.FThreadMonitor = nil then
    TCnMonitorThread.Create(FMgr);
  InterlockedIncrement(FMgr.FThreadCount);
end;

destructor TCnTaskThread.Destroy;
begin
  if (FTask <> nil) and (FTask.FStatus <> tsFinished) then
    FTask.FStatus := tsFailure;
  if FMgr <> nil then
  begin
    InterlockedDecrement(FMgr.FThreadCount);
    FMgr.FWorkingTasks.Remove(FTask);
    FMgr.FFinishTasks.Add(FTask);
  end;
  inherited;
end;

procedure TCnTaskThread.DoExecute;
begin
  if Assigned(FTask) and Assigned(FTask.FOnExecute) then
    FTask.FOnExecute(FTask);
end;

procedure TCnTaskThread.Execute;
begin
  FTask.FStatus := tsWorking;
  FTask.FStartTick := GetTickCount;
  try
    DoExecute;
  except
    ;
  end;
  // �Զ��˳�ʱ���д���ǿ���˳�ʱ�ɼ����̴߳���
  if FMgr <> nil then
    FMgr.FThreads.Remove(Self);
end;

{ TCnMonitorThread }

constructor TCnMonitorThread.Create(AMgr: TCnThreadTaskMgr);
begin            
  inherited Create(false);
  FreeOnTerminate := True;  
  FMgr := AMgr;   
  FMgr.FThreadMonitor := Self;
end;

destructor TCnMonitorThread.Destroy;
begin
  FMgr.FThreadMonitor := nil;
  inherited;
end;

procedure TCnMonitorThread.Execute;
var
  Threads, Tasks: TList;
  Task: TCnTask;
  TaskThread: TCnTaskThread;
  i: integer;
begin
  while not Terminated do
  begin
    Sleep(100);
    Threads := FMgr.FThreads.LockList;
    try
      for i := Threads.Count - 1 downto 0 do
      begin
        TaskThread := TCnTaskThread(Threads[i]);
        try
          // ��ʱ�ж�
          if (TaskThread.FTask <> nil) and
            (TaskThread.FTask.FTimeOut <> 0) and
            (TaskThread.FTask.FStartTick <> 0) then
          begin
            if Abs(GetTickCount - TaskThread.FTask.FStartTick) >=
              Integer(TaskThread.FTask.FTimeOut) then
            begin
              if TaskThread.CanTerminate then
              begin
                Windows.TerminateThread(TaskThread.Handle, 0);
                TaskThread.Free;
              end
              else
              begin
                TaskThread.FMgr := nil;
                TaskThread.FTask := nil;
              end;  
              Threads.Remove(TaskThread);
            end;
          end;
        except
          ;
        end;
      end;
    finally
      FMgr.FThreads.UnlockList;
    end;

    if FMgr.FThreadCount < FMgr.FMaxThreads then
    begin
      Task := nil;
      Tasks := FMgr.FWaitTasks.LockList;
      try
        if Tasks.Count > 0 then
        begin
          Task := TCnTask(Tasks[0]);
        end;
      finally
        FMgr.FWaitTasks.UnlockList;
      end;
      if Task <> nil then
        FMgr.GetThreadClass.Create(FMgr, Task);
    end;
  end;
end;

{ TCnThreadTaskMgr }

constructor TCnThreadTaskMgr.Create;
begin
  FTasks := TStringList.Create;
  FTasks.Sorted := True;
  FWaitTasks := TThreadList.Create;
  FWorkingTasks := TThreadList.Create;
  FFinishTasks := TThreadList.Create;
  FThreads := TThreadList.Create;
  FMaxThreads := 10;
end;

destructor TCnThreadTaskMgr.Destroy;
var
  i: integer;
  Threads: TList;
  TaskThread: TCnTaskThread;
begin
  if FThreadMonitor <> nil then
  begin                   
    TerminateThread(FThreadMonitor.Handle, 0);
    FThreadMonitor.Free;
  end;

  Threads := FThreads.LockList;
  try
    for i := Threads.Count - 1 downto 0 do
    begin
      TaskThread := TCnTaskThread(Threads[i]);
      if TaskThread.CanTerminate then
      begin
        TerminateThread(TaskThread.Handle, 0);
        TaskThread.Free;
      end
      else
      begin
        TaskThread.FMgr := nil;
        TaskThread.FTask := nil;
      end;  
    end;
  finally
    FThreads.UnlockList;
  end;
  FThreads.Free;

  for i := FTasks.Count - 1 downto 0 do
    FTasks.Objects[i].Free;
  FTasks.Free;
  
  FWaitTasks.Free;
  FWorkingTasks.Free;
  FFinishTasks.Free;
  inherited;
end;

function TCnThreadTaskMgr.FindTask(ATaskId: string): TCnTask;
begin
  if (ATaskId <> '') and (FTasks.IndexOf(ATaskId) >= 0) then
    Result := TCnTask(FTasks.Objects[FTasks.IndexOf(ATaskId)])
  else
    Result := nil;
end;

function TCnThreadTaskMgr.GetCount: integer;
begin
  Result := FTasks.Count;
end;

function TCnThreadTaskMgr.GetThreadClass: TCnTaskThreadClass;
begin
  Result := TCnTaskThread;
end;

function TCnThreadTaskMgr.GetThreadListCount(AList: TThreadList): Integer;
var
  lst: TList;
begin
  lst := AList.LockList;
  try
    Result := lst.Count;
  finally
    AList.UnlockList;
  end;
end;

function TCnThreadTaskMgr.GetFinishCount: Integer;
begin
  Result := GetThreadListCount(FFinishTasks);
end;

function TCnThreadTaskMgr.GetWaitingCount: Integer;
begin
  Result := GetThreadListCount(FWaitTasks);
end;

function TCnThreadTaskMgr.GetWorkingCount: Integer;
begin
  Result := GetThreadListCount(FWorkingTasks);
end;

procedure TCnThreadTaskMgr.AddTask(ATaskId: string; ATask: TCnTask);
begin
  FTasks.AddObject(ATaskId, ATask);
  case ATask.FStatus of
    tsWaiting, tsWorking:
      begin
        ATask.FStatus := tsWaiting;
        FWaitTasks.Add(ATask);
        if FThreadCount < FMaxThreads then
          GetThreadClass.Create(Self, ATask);
      end;
    tsFinished, tsFailure:
      begin
        FFinishTasks.Add(ATask);
      end;
  end;
end;

procedure TCnThreadTaskMgr.SetMaxThreads(const Value: Integer);
begin
  if Value > 0 then
  begin
    FMaxThreads := Value;
  end;
end;
                       
end.

