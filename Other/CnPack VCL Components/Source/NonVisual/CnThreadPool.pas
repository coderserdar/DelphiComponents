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

unit CnThreadPool;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ��̳߳�ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�Chinbo��Shenloqi��
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 7.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����ݲ����ϱ��ػ�����ʽ
* �޸ļ�¼��2008.4.1
*               �ҵ��˵�������̳߳ص���Ҫ�ο�ʵ�ֵ�Ԫ��������ԭ����
*                 Aleksej Petrov�İ�Ȩ��Ϣ��������ԭ���Ļ����������˺ܶ�����Ҳ
*                 ��ǿ��һЩ���ܣ����������˼·�ʹ��ʵ�ַ������Ǹ�ԭ����һ����
*                 �ٴθ�лAleksej Petrov��Ҳ��лLeeon�����ҵ���ԭ������Ϣ
*           2007.7.13
*               �޸���DeadTaskAsNew�ᵼ�����޵ݹ�Ͳ���Ч��BUG
*               �����Ҫʹ��DeadTaskAsNew����Ҫʵ��TCnTaskDataObject.Clone����
*               ������һ��TCnThreadPool.AddRequests����
*           2004.8.9
*               ������TCnPoolingThread.StillWorking
*               ��������TickCount�����BUG
*           2004.3.14
*               ����һЩBUG            
*           2003.12.24
*               ʹ��FTaskCount��FIdleThreadCount�ӿ�ִ��Ч��
*               ������MinAtLeast������ȷ������BUG
*               ��DefaultGetInfo֮�е���FreeFinishedThreads
*           2003.12.21
*               ��ɲ����Ե�Ԫ
*           2003.12.16
 *              ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

(********************************************************
  This component is modified from Aleksej Petrov's threadpool, fixed some
  memory leaks and enhanced the scheduling implementation and fixed some bugs.

 {*************************************************************}

 {   Component for processing request queue in pool of threads }

 {   Copyright (c) 2001, Aleksej Petrov, AKPetrov@pisem.net    }

 {    Free for noncommercial use.                              }
 { Please contact with author for use in commercial projects.  }
********************************************************)

(********************************************************
��Ԫ˵����
    �õ�Ԫʵ�����̳߳صĹ���



��ƣ�
    ʵ���̳߳أ�����Ҫ�����������򵥵ľ���ʹ��һ��ָ
  ��ĳһ�ṹ��ָ�룬������������Ȼ����һ���õķ���������
  һ�ֱȽϼ򵥵ķ�������ʹ�ö������˳��������֮�󣬾�
  Ҫ��һ��������Ĺ����б�����Լ򵥵���TListʵ�֣�����
  TList���¼�����һЩ��Ȼ���Ҫ��ִ�д���������̣߳�����
  ��Ҫ��TThread�̳У�Ȼ��Ҫ��֪ͨ���ƣ�������������ʱ���
  ���㣬����ƽ����ơ�
    ��򵥵�ʵ���ǹ����߳�����һ������֮������ߣ�������
  ��ʹ��һ����ʱ�����ڸ���Щ�߳̽��з��䣬������������
  Ч�ʲ��ߣ�Ҳ���������������ʱ��ͽ���һ�η��䣬������
  ����������������̵߳�������⣬Ч�ʵȶ����ǺõĽ����
  �������ź������ȽϺõĽ��������⡣���̳߳����̵߳�
  �ͷ�����Լ�ͨ����ʱ��ʵ�֡�
    ��������������ι��ƹ��������̳߳صĹ���ǿ�ȣ�Ȼ���
  ����ʱ��Ҫ�����̣߳���ʱ��Ҫ�����߳�:)

���ƣ�
    ���ǵ��̳߳ص�ʹ�÷�Χ��������ʵ���в����˲���ֻ����
  NTϵ�в�ʵ�ֵ�API�����Ը������NTϵ�в���ϵͳ�ϻ������
  ��Ч������ȻҲ��֧�ַ�Windows������
    ��Ϊʹ����WaitableTimer��������9x�������̳߳ز������
  �̳߳��е��߳���Ŀ��Ӧ���ǿ���ͨ��SetTimer���������
  SetTimer��Ҫһ����Ϣѭ������WM_TIMER��Ϣ�������ϴ��Ҳ�
  ����ʵ��:)
    ���⻹��һ������ķ�������mmsystem��timesetevent����,
  ������������Ŀ���Ӧ�ñ������ĸ���
    �������ͨ��TTimer��ʵ�ֵĻ���Ӧ�þͿ���ʵ�ֿ�ƽ̨��
  �����...

�ڴ�й©��
    һ������¸�����������ڴ�й¶��Ȼ�����̳߳ر��ͷ�ʱ
  �̳߳��л������ڹ������̣߳�����Щ�߳��������ⲿ������
  �ƻ�ʱ��Ϊ���߳��ܹ������˳�������ʹ����TerminateThread
  ������������������������̷߳�����ڴ棬�Ϳ�������ڴ�
  й¶�����˵�����������һ�㷢����Ӧ�ó����˳�ʱ�ƻ�����
  ���������£�����һ��Ӧ�ó����˳�������ϵͳ������Ӧ����
  ����������ʵ����Ӧ�ò����ڴ�й¶:)
    ��ʹ��ʹ����TerminateThread��Ҳ��Ӧ�û���ɳ����˳�ʱ
  �������쳣����RunTime Error 216

�༰������

  TCnCriticalSection -- �ٽ����ķ�װ
    ��NT��ʵ����TryEnterCriticalSection����SyncObjs.pas��
  ��TCriticalSectionû�з�װ���������TCnCriticalSection
  ��װ��������ֱ�Ӵ�TObject�̳У�����Ӧ��СһЩ:)
    TryEnter -- TryEnterCriticalSection
    TryEnterEx -- 9xʱ�����Enter��NT����TryEnter

  TCnTaskDataObject -- �̳߳��̴߳������ݵķ�װ
    �̳߳��е��̴߳���������������ݵĻ���
    һ������£�Ҫʵ��ĳ���ض����������Ҫʵ����Ӧ��һ��
  �Ӹ���̳е���
    Duplicate -- �Ƿ�����һ������������ͬ����ͬ�򲻴���
    Info -- ��Ϣ�����ڵ������

  TCnPoolingThread -- �̳߳��е��߳�
    �̳߳��е��̵߳Ļ��࣬һ������²���Ҫ�̳и���Ϳ���
  ʵ�ִ󲿷ֵĲ����������߳���ҪһЩ�ⲿ����ʱ���Լ̳и�
  ��Ĺ����������������һ�ַ����ǿ������̳߳ص�����¼�
  �н�����Щ����
    AverageWaitingTime -- ƽ���ȴ�ʱ��
    AverageProcessingTime -- ƽ������ʱ��
    Duplicate -- �Ƿ����ڴ�����ͬ������
    Info -- ��Ϣ�����ڵ������
    IsDead -- �Ƿ�����
    IsFinished -- �Ƿ�ִ�����
    IsIdle -- �Ƿ����
    NewAverage -- ����ƽ��ֵ�������㷨��
    StillWorking -- �����߳���Ȼ������
    Execute -- �߳�ִ�к�����һ�㲻��̳�
    Create -- ���캯��
    Destroy -- ��������
    Terminate -- �����߳�

  TCnThreadPool -- �̳߳�
    �ؼ����¼���û��ʹ��ͬ����ʽ��װ��������Щ�¼��Ĵ���
  Ҫ�̰߳�ȫ�ſ���
    HasSpareThread -- �п��е��߳�
    AverageWaitingTime -- ƽ���ȴ�ʱ��
    AverageProcessingTime -- ƽ������ʱ��
    TaskCount -- ������Ŀ
    ThreadCount -- �߳���Ŀ
    CheckTaskEmpty -- ��������Ƿ��Ѿ����
    GetRequest -- �Ӷ����л�ȡ����
    DecreaseThreads -- �����߳�
    IncreaseThreads -- �����߳�
    FreeFinishedThreads -- �ͷ���ɵ��߳�
    KillDeadThreads -- ������߳�
    Info -- ��Ϣ�����ڵ������
    OSIsWin9x -- ����ϵͳ��Win9x
    AddRequest -- ��������
    RemoveRequest -- �Ӷ�����ɾ������
    CreateSpecial -- �����Զ����̳߳��̵߳Ĺ��캯��
    AdjustInterval -- �����̵߳�ʱ����
    DeadTaskAsNew -- �����̵߳��������¼ӵ�����
    MinAtLeast -- �߳�����������С��Ŀ
    ThreadDeadTimeout -- �߳�������ʱ
    ThreadsMinCount -- �����߳���
    ThreadsMaxCount -- ����߳���
    OnGetInfo -- ��ȡ��Ϣ�¼�
    OnProcessRequest -- ���������¼�
    OnQueueEmpty -- ���п��¼�
    OnThreadInitializing -- �̳߳�ʼ���¼�
    OnThreadFinalizing -- �߳���ֹ���¼�
********************************************************)

interface

{$I CnPack.inc}

{.$DEFINE DEBUG}//�Ƿ����������Ϣ

uses
  SysUtils, Windows, Classes,
  CnConsts, CnClasses, CnCompConsts;

type
  TCnCriticalSection = class
  protected
    FSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    function TryEnterEx: Boolean;
  end;

  TCnTaskDataObject = class
  public
    function Clone: TCnTaskDataObject; virtual; abstract;
    function Duplicate(DataObj: TCnTaskDataObject;
      const Processing: Boolean): Boolean; virtual;
    function Info: string; virtual;
  end;

  { TCnPoolingThread }

  TCnThreadPool = class;

  TCnThreadState = (ctsInitializing, ctsWaiting,
    ctsGetting,
    ctsProcessing, ctsProcessed,
    ctsTerminating, ctsForReduce);

  TCnPoolingThread = class(TThread)
  private
    hInitFinished: THandle;
    sInitError: string;
{$IFDEF DEBUG}
    procedure Trace(const Str: string);
{$ENDIF DEBUG}
    procedure ForceTerminate;
  protected
    FAverageWaitingTime: Integer;
    FAverageProcessing: Integer;
    uWaitingStart: DWORD;
    uProcessingStart: DWORD;
    uStillWorking: DWORD;
    FWorkCount: Int64;
    FCurState: TCnThreadState;
    hThreadTerminated: THandle;
    FPool: TCnThreadPool;
    FProcessingDataObject: TCnTaskDataObject;

    csProcessingDataObject: TCnCriticalSection;

    function AverageProcessingTime: DWORD;
    function AverageWaitingTime: DWORD;
    function CloneData: TCnTaskDataObject;
    function Duplicate(DataObj: TCnTaskDataObject): Boolean; virtual;
    function Info: string; virtual;
    function IsDead: Boolean; virtual;
    function IsFinished: Boolean; virtual;
    function IsIdle: Boolean; virtual;
    function NewAverage(OldAvg, NewVal: Integer): Integer; virtual;

    procedure Execute; override;
  public
    constructor Create(aPool: TCnThreadPool); virtual;
    destructor Destroy; override;

    procedure StillWorking;
    procedure Terminate(const Force: Boolean = False);
  end;

  TCnPoolingThreadClass = class of TCnPoolingThread;

  { TCnThreadPool }

  TCheckDuplicate = (cdQueue, cdProcessing);
  TCheckDuplicates = set of TCheckDuplicate;

  TGetInfo = procedure(Sender: TCnThreadPool;
    var InfoText: string) of object;
  TProcessRequest = procedure(Sender: TCnThreadPool;
    aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread) of object;

  TEmptyKind = (ekQueueEmpty, ekTaskEmpty);
  TQueueEmpty = procedure(Sender: TCnThreadPool;
    EmptyKind: TEmptyKind) of object;

  TThreadInPoolInitializing = procedure(Sender: TCnThreadPool;
    aThread: TCnPoolingThread) of object;
  TThreadInPoolFinalizing = procedure(Sender: TCnThreadPool;
    aThread: TCnPoolingThread) of object;

  TCnThreadPool = class(TCnComponent)
  private
    csQueueManagment: TCnCriticalSection;
    csThreadManagment: TCnCriticalSection;

    FQueue: TList;
    FThreads: TList;
    FThreadsKilling: TList;
    FThreadsMinCount, FThreadsMaxCount: Integer;
    FThreadDeadTimeout: DWORD;
    FThreadClass: TCnPoolingThreadClass;
    FAdjustInterval: DWORD;
    FDeadTaskAsNew: Boolean;
    FMinAtLeast: Boolean;
    FIdleThreadCount, FTaskCount: Integer;

    FThreadInitializing: TThreadInPoolInitializing;
    FThreadFinalizing: TThreadInPoolFinalizing;
    FProcessRequest: TProcessRequest;
    FQueueEmpty: TQueueEmpty;
    FGetInfo: TGetInfo;

    procedure SetAdjustInterval(const Value: DWORD);
{$IFDEF DEBUG}
    procedure Trace(const Str: string);
{$ENDIF DEBUG}
  protected
    FLastGetPoint: Integer;
    hSemRequestCount: THandle;
    hTimReduce: THandle;

    function HasSpareThread: Boolean;
    function HasTask: Boolean;
    function FinishedThreadsAreFull: Boolean; virtual;
    procedure CheckTaskEmpty;
    procedure GetRequest(var Request: TCnTaskDataObject);
    procedure DecreaseThreads;
    procedure IncreaseThreads;
    procedure FreeFinishedThreads;
    procedure KillDeadThreads;

    procedure DoProcessRequest(aDataObj: TCnTaskDataObject;
      aThread: TCnPoolingThread); virtual;
    procedure DoQueueEmpty(EmptyKind: TEmptyKind); virtual;
    procedure DoThreadInitializing(aThread: TCnPoolingThread); virtual;
    procedure DoThreadFinalizing(aThread: TCnPoolingThread); virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    uTerminateWaitTime: DWORD;
    QueuePackCount: Integer;

    constructor Create(AOwner: TComponent); override;
    constructor CreateSpecial(AOwner: TComponent;
      AClass: TCnPoolingThreadClass);
    destructor Destroy; override;

    function AverageWaitingTime: Integer;
    function AverageProcessingTime: Integer;
    function Info: string;
    function OSIsWin9x: Boolean;
    function TaskCount: Integer;
    function ThreadCount: Integer;
    function ThreadInfo(const i: Integer): string;
    function ThreadKillingCount: Integer;
    function ThreadKillingInfo(const i: Integer): string;
    procedure DefaultGetInfo(Sender: TCnThreadPool; var InfoText: string);

    function AddRequest(aDataObject: TCnTaskDataObject;
      CheckDuplicate: TCheckDuplicates = [cdQueue]): Boolean;
    procedure AddRequests(aDataObjects: array of TCnTaskDataObject;
      CheckDuplicate: TCheckDuplicates = [cdQueue]);
    procedure RemoveRequest(aDataObject: TCnTaskDataObject);
  published
    property AdjustInterval: DWORD read FAdjustInterval write SetAdjustInterval
      default 10000;
    property DeadTaskAsNew: Boolean read FDeadTaskAsNew write FDeadTaskAsNew
      default True;
    property MinAtLeast: Boolean read FMinAtLeast write FMinAtLeast
      default False;
    property ThreadDeadTimeout: DWORD read FThreadDeadTimeout
      write FThreadDeadTimeout default 0;
    property ThreadsMinCount: Integer read FThreadsMinCount write FThreadsMinCount default 0;
    property ThreadsMaxCount: Integer read FThreadsMaxCount write FThreadsMaxCount default 10;

    property OnGetInfo: TGetInfo read FGetInfo write FGetInfo;
    property OnProcessRequest: TProcessRequest read FProcessRequest
      write FProcessRequest;
    property OnQueueEmpty: TQueueEmpty read FQueueEmpty write FQueueEmpty;
    property OnThreadInitializing: TThreadInPoolInitializing
      read FThreadInitializing write FThreadInitializing;
    property OnThreadFinalizing: TThreadInPoolFinalizing read FThreadFinalizing
      write FThreadFinalizing;
  end;

{$IFDEF DEBUG}
  TLogWriteProc = procedure(const Str: string; const ID: Integer);

var
  TraceLog: TLogWriteProc = nil;
{$ENDIF DEBUG}

const
  CCnTHREADSTATE: array[TCnThreadState] of string = (
    'ctsInitializing', 'ctsWaiting',
    'ctsGetting',
    'ctsProcessing', 'ctsProcessed',
    'ctsTerminating', 'ctsForReduce');

implementation

uses
  Math;

const
  MaxInt64 = High(Int64);

var
  FOSIsWin9x: Boolean;

{$IFDEF DEBUG}

procedure SimpleTrace(const Str: string; const ID: Integer);
begin
  OutputDebugString(PChar(IntToStr(ID) + ':' + Str))
end;
{$ENDIF DEBUG}

function GetTickDiff(const AOldTickCount, ANewTickCount : Cardinal):Cardinal;
begin
  if ANewTickCount >= AOldTickCount then
  begin
    Result := ANewTickCount - AOldTickCount;
  end
  else
  begin
    Result := High(Cardinal) - AOldTickCount + ANewTickCount;
  end;
end;

{ TCnCriticalSection }

constructor TCnCriticalSection.Create;
begin
  inherited;
  InitializeCriticalSection(FSection)
end;

destructor TCnCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

procedure TCnCriticalSection.Enter;
begin
  EnterCriticalSection(FSection)
end;

procedure TCnCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection)
end;

function TCnCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection)
end;

function TCnCriticalSection.TryEnterEx: Boolean;
begin
  if FOSIsWin9x then
  begin
    Enter;
    Result := True
  end
  else
    Result := TryEnter
end;

{ TCnTaskDataObject }

function TCnTaskDataObject.Duplicate(DataObj: TCnTaskDataObject;
  const Processing: Boolean): Boolean;
begin
  Result := False
end;

function TCnTaskDataObject.Info: string;
begin
  Result := IntToHex(Cardinal(Self), 8)
end;

{ TCnPoolingThread }

constructor TCnPoolingThread.Create(aPool: TCnThreadPool);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Create');
{$ENDIF DEBUG}

  inherited Create(True);
  FPool := aPool;

  FAverageWaitingTime := 0;
  FAverageProcessing := 0;
  FWorkCount := 0;
  sInitError := '';
  FreeOnTerminate := False;
  hInitFinished := CreateEvent(nil, True, False, nil);
  hThreadTerminated := CreateEvent(nil, True, False, nil);
  csProcessingDataObject := TCnCriticalSection.Create;
  try
    Resume;
    WaitForSingleObject(hInitFinished, INFINITE);
    if sInitError <> '' then
      raise Exception.Create(sInitError);
  finally
    CloseHandle(hInitFinished);
  end;

{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Created OK');
{$ENDIF DEBUG}
end;

destructor TCnPoolingThread.Destroy;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Destroy');
{$ENDIF DEBUG}

  FreeAndNil(FProcessingDataObject);
  CloseHandle(hThreadTerminated);
  csProcessingDataObject.Free;
  inherited;
end;

function TCnPoolingThread.AverageProcessingTime: DWORD;
begin
  if FCurState in [ctsProcessing] then
    Result := NewAverage(FAverageProcessing, GetTickDiff(uProcessingStart, GetTickCount))
  else
    Result := FAverageProcessing
end;

function TCnPoolingThread.AverageWaitingTime: DWORD;
begin
  if FCurState in [ctsWaiting, ctsForReduce] then
    Result := NewAverage(FAverageWaitingTime, GetTickDiff(uWaitingStart, GetTickCount))
  else
    Result := FAverageWaitingTime
end;

function TCnPoolingThread.Duplicate(DataObj: TCnTaskDataObject): Boolean;
begin
  csProcessingDataObject.Enter;
  try
    Result := (FProcessingDataObject <> nil) and
      DataObj.Duplicate(FProcessingDataObject, True);
  finally
    csProcessingDataObject.Leave
  end
end;

procedure TCnPoolingThread.ForceTerminate;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.ForceTerminate');
{$ENDIF DEBUG}

  TerminateThread(Handle, 0)
end;

procedure TCnPoolingThread.Execute;
type
  THandleID = (hidRequest, hidReduce, hidTerminate);
var
  WaitedTime: Integer;
  Handles: array[THandleID] of THandle;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Execute');
{$ENDIF DEBUG}

  FCurState := ctsInitializing;
  try
    FPool.DoThreadInitializing(Self);
  except
    on E: Exception do
      sInitError := E.Message;
  end;
  SetEvent(hInitFinished);

{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Execute: Initialized');
{$ENDIF DEBUG}

  Handles[hidRequest] := FPool.hSemRequestCount;
  Handles[hidReduce] := FPool.hTimReduce;
  Handles[hidTerminate] := hThreadTerminated;

  uWaitingStart := GetTickCount;
  FProcessingDataObject := nil;

  while not Terminated do
  begin
    if not (FCurState in [ctsWaiting, ctsForReduce]) then
      InterlockedIncrement(FPool.FIdleThreadCount);
    FCurState := ctsWaiting;
    case WaitForMultipleObjects(Length(Handles), @Handles, False, INFINITE) of
      WAIT_OBJECT_0 + Ord(hidRequest):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidRequest');
{$ENDIF DEBUG}

          WaitedTime := GetTickDiff(uWaitingStart, GetTickCount);
          FAverageWaitingTime := NewAverage(FAverageWaitingTime, WaitedTime);

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);
          FCurState := ctsGetting;
          FPool.GetRequest(FProcessingDataObject);
          if FWorkCount < MaxInt64 then
            FWorkCount := FWorkCount + 1;
          uProcessingStart := GetTickCount;
          uStillWorking := uProcessingStart;

          FCurState := ctsProcessing;
          try
{$IFDEF DEBUG}
            Trace('Processing: ' + FProcessingDataObject.Info);
{$ENDIF DEBUG}

            FPool.DoProcessRequest(FProcessingDataObject, Self)
          except
{$IFDEF DEBUG}
            on E: Exception do
              Trace('OnProcessRequest Exception: ' + E.Message);
{$ENDIF DEBUG}
          end;

          csProcessingDataObject.Enter;
          try
            FreeAndNil(FProcessingDataObject)
          finally
            csProcessingDataObject.Leave
          end;
          FAverageProcessing :=
            NewAverage(FAverageProcessing, GetTickDiff(uProcessingStart, GetTickCount));

          FCurState := ctsProcessed;
          FPool.CheckTaskEmpty;
          uWaitingStart := GetTickCount;
        end;
      WAIT_OBJECT_0 + Ord(hidReduce):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidReduce');
{$ENDIF DEBUG}

          if not (FCurState in [ctsWaiting, ctsForReduce]) then
            InterlockedIncrement(FPool.FIdleThreadCount);
          FCurState := ctsForReduce;
          FPool.DecreaseThreads;
        end;
      WAIT_OBJECT_0 + Ord(hidTerminate):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidTerminate');
{$ENDIF DEBUG}

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);

          FCurState := ctsTerminating;
          Break
        end;
    end;
  end;

  if FCurState in [ctsWaiting, ctsForReduce] then
    InterlockedDecrement(FPool.FIdleThreadCount);
  FCurState := ctsTerminating;
  FPool.DoThreadFinalizing(Self);
end;

function TCnPoolingThread.Info: string;
begin
  Result := 'AverageWaitingTime=' + IntToStr(AverageWaitingTime) + '; ' +
    'AverageProcessingTime=' + IntToStr(AverageProcessingTime) + '; ' +
    'FCurState=' + CCnTHREADSTATE[FCurState] + '; ' +
    'FWorkCount=' + IntToStr(FWorkCount);
  if not FPool.OSIsWin9x then
  begin
    if csProcessingDataObject.TryEnter then
    try
      Result := Result + '; ' + 'FProcessingDataObject=';
      if FProcessingDataObject = nil then
        Result := Result + 'nil'
      else
        Result := Result + FProcessingDataObject.Info
    finally
      csProcessingDataObject.Leave
    end
  end
  else
  begin
    if FProcessingDataObject = nil then
      Result := Result + '; ' + 'FProcessingDataObject=nil'
    else
      Result := Result + '; ' + 'FProcessingDataObject!=nil'
  end
end;

function TCnPoolingThread.IsDead: Boolean;
begin
  Result := Terminated or
    ((FPool.ThreadDeadTimeout > 0) and
    (FCurState = ctsProcessing) and
    (GetTickDiff(uStillWorking, GetTickCount) > FPool.ThreadDeadTimeout));
{$IFDEF DEBUG}
  if Result then
    Trace('Thread is dead, Info = ' + Info);
{$ENDIF DEBUG}
end;

function TCnPoolingThread.IsFinished: Boolean;
begin
  Result := WaitForSingleObject(Handle, 0) = WAIT_OBJECT_0
end;

function TCnPoolingThread.IsIdle: Boolean;
begin
  Result := (FCurState in [ctsWaiting, ctsForReduce]) and
    (AverageWaitingTime > 200) and
    (AverageWaitingTime * 2 > AverageProcessingTime)
end;

function TCnPoolingThread.NewAverage(OldAvg, NewVal: Integer): Integer;
begin
  if FWorkCount >= 8 then
    Result := (OldAvg * 7 + NewVal) div 8
  else if FWorkCount > 0 then
    Result := (OldAvg * FWorkCount + NewVal) div FWorkCount
  else
    Result := NewVal
end;

procedure TCnPoolingThread.StillWorking;
begin
  uStillWorking := GetTickCount
end;

procedure TCnPoolingThread.Terminate(const Force: Boolean);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Terminate');
{$ENDIF DEBUG}

  inherited Terminate;

  if Force then
  begin
    ForceTerminate;
    Free
  end
  else
    SetEvent(hThreadTerminated)
end;

{$IFDEF DEBUG}

procedure TCnPoolingThread.Trace(const Str: string);
begin
  TraceLog(Str, ThreadID);
end;
{$ENDIF DEBUG}

function TCnPoolingThread.CloneData: TCnTaskDataObject;
begin
  csProcessingDataObject.Enter;
  try
    Result := nil;
    if FProcessingDataObject <> nil then
      Result := FProcessingDataObject.Clone;
  finally
    csProcessingDataObject.Leave;
  end;
end;

{ TCnThreadPool }

constructor TCnThreadPool.Create(AOwner: TComponent);
var
  DueTo: Int64;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Create');
{$ENDIF DEBUG}

  inherited;

  csQueueManagment := TCnCriticalSection.Create;
  csThreadManagment := TCnCriticalSection.Create;
  FQueue := TList.Create;
  FThreads := TList.Create;
  FThreadsKilling := TList.Create;
  FThreadsMinCount := 0;
  FThreadsMaxCount := 1;
  FThreadDeadTimeout := 0;
  FThreadClass := TCnPoolingThread;
  FAdjustInterval := 10000;
  FDeadTaskAsNew := True;
  FMinAtLeast := False;
  FLastGetPoint := 0;
  uTerminateWaitTime := 10000;
  QueuePackCount := 127;
  FIdleThreadCount := 0;
  FTaskCount := 0;

  hSemRequestCount := CreateSemaphore(nil, 0, $7FFFFFFF, nil);

  DueTo := -1;
  hTimReduce := CreateWaitableTimer(nil, False, nil);

  if hTimReduce = 0 then
    hTimReduce := CreateEvent(nil, False, False, nil)
  else
    SetWaitableTimer(hTimReduce, DueTo, FAdjustInterval, nil, nil, False);
end;

constructor TCnThreadPool.CreateSpecial(AOwner: TComponent;
  AClass: TCnPoolingThreadClass);
begin
  Create(AOwner);
  if AClass <> nil then
    FThreadClass := AClass
end;

destructor TCnThreadPool.Destroy;
var
  i, n: Integer;
  Handles: array of THandle;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Destroy');
{$ENDIF DEBUG}

  csThreadManagment.Enter;
  try
    SetLength(Handles, FThreads.Count);
    n := 0;
    for i := 0 to FThreads.Count - 1 do
      if FThreads[i] <> nil then
      begin
        Handles[n] := TCnPoolingThread(FThreads[i]).Handle;
        TCnPoolingThread(FThreads[i]).Terminate(False);
        Inc(n);
      end;
    WaitForMultipleObjects(n, @Handles[0], True, uTerminateWaitTime);

    for i := 0 to FThreads.Count - 1 do
    begin
      {if FThreads[i] <> nil then
        TCnPoolingThread(FThreads[i]).Terminate(True)
      else}
      TCnPoolingThread(FThreads[i]).Free;
    end;

    FThreads.Free;

    FreeFinishedThreads;
    for i := 0 to FThreadsKilling.Count - 1 do
    begin
      {if FThreadsKilling[i] <> nil then
        TCnPoolingThread(FThreadsKilling[i]).Terminate(True)
      else}
      TCnPoolingThread(FThreadsKilling[i]).Free;
    end;

    FThreadsKilling.Free;
  finally
    csThreadManagment.Free;
  end;

  csQueueManagment.Enter;
  try
    for i := FQueue.Count - 1 downto 0 do
      TObject(FQueue[i]).Free;
    FQueue.Free;
  finally
    csQueueManagment.Free;
  end;

  CloseHandle(hSemRequestCount);
  CloseHandle(hTimReduce);

  inherited;
end;

function TCnThreadPool.AddRequest(aDataObject: TCnTaskDataObject;
  CheckDuplicate: TCheckDuplicates): Boolean;
var
  i: Integer;
begin
{$IFDEF DEBUG}
  Trace('AddRequest:' + aDataObject.Info);
{$ENDIF DEBUG}

  Result := False;

  csQueueManagment.Enter;
  try
    if cdQueue in CheckDuplicate then
      for i := 0 to FQueue.Count - 1 do
        if (FQueue[i] <> nil) and
          aDataObject.Duplicate(TCnTaskDataObject(FQueue[i]), False) then
        begin
{$IFDEF DEBUG}
          Trace('Duplicate:' + TCnTaskDataObject(FQueue[i]).Info);
{$ENDIF DEBUG}

          FreeAndNil(aDataObject);
          Exit
        end;

    csThreadManagment.Enter;
    try
      IncreaseThreads;

      if cdProcessing in CheckDuplicate then
        for i := 0 to FThreads.Count - 1 do
          if TCnPoolingThread(FThreads[i]).Duplicate(aDataObject) then
          begin
{$IFDEF DEBUG}
            Trace('Duplicate:' + TCnPoolingThread(FThreads[i]).FProcessingDataObject.Info);
{$ENDIF DEBUG}

            FreeAndNil(aDataObject);
            Exit
          end
    finally
      csThreadManagment.Leave;
    end;

    FQueue.Add(aDataObject);
    Inc(FTaskCount);
    ReleaseSemaphore(hSemRequestCount, 1, nil);
{$IFDEF DEBUG}
    Trace('ReleaseSemaphore');
{$ENDIF DEBUG}

    Result := True;

  finally
    csQueueManagment.Leave;
  end;

{$IFDEF DEBUG}
  Trace('Added Request:' + aDataObject.Info);
{$ENDIF DEBUG}
end;

procedure TCnThreadPool.AddRequests(
  aDataObjects: array of TCnTaskDataObject;
  CheckDuplicate: TCheckDuplicates);
var
  i: Integer;
begin
  for i := 0 to Length(aDataObjects) - 1 do
    AddRequest(aDataObjects[i], CheckDuplicate)
end;

procedure TCnThreadPool.CheckTaskEmpty;
var
  i: Integer;
begin
  csQueueManagment.Enter;
  try
    if (FLastGetPoint < FQueue.Count) then
      Exit;

    csThreadManagment.Enter;
    try
      for i := 0 to FThreads.Count - 1 do
        if TCnPoolingThread(FThreads[i]).FCurState in [ctsProcessing] then
          Exit
    finally
      csThreadManagment.Leave
    end;

    DoQueueEmpty(ekTaskEmpty)

  finally
    csQueueManagment.Leave
  end
end;

procedure TCnThreadPool.DecreaseThreads;
var
  i: Integer;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.DecreaseThreads');
{$ENDIF DEBUG}

  if csThreadManagment.TryEnter then
  try
    KillDeadThreads;
    FreeFinishedThreads;

    for i := FThreads.Count - 1 downto FThreadsMinCount do
      if TCnPoolingThread(FThreads[i]).IsIdle then
      begin
        TCnPoolingThread(FThreads[i]).Terminate(False);
        FThreadsKilling.Add(FThreads[i]);
        FThreads.Delete(i);
        Break
      end
  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.DefaultGetInfo(Sender: TCnThreadPool;
  var InfoText: string);
var
  i: Integer;
  sLine: string;
begin
  sLine := StringOfChar('=', 15);
  with Sender do
  begin
    FreeFinishedThreads;
    InfoText := 'MinCount=' + IntToStr(ThreadsMinCount) +
      '; MaxCount=' + IntToStr(ThreadsMaxCount) +
      '; AdjustInterval=' + IntToStr(AdjustInterval) +
      '; DeadTimeOut=' + IntToStr(ThreadDeadTimeout) + #13#10 +
      'ThreadCount=' + IntToStr(ThreadCount) +
      '; KillingCount=' + IntToStr(ThreadKillingCount) +
      '; SpareThreadCount=' + IntToStr(FIdleThreadCount) +
      '; TaskCount=' + IntToStr(TaskCount) + #13#10 +
      'AverageWaitingTime=' + IntToStr(AverageWaitingTime) +
      '; AverageProcessingTime=' + IntToStr(AverageProcessingTime) + #13#10 +
      {sLine + }'Working Threads Info' + sLine;
    for i := 0 to ThreadCount - 1 do
      InfoText := InfoText + #13#10 + ThreadInfo(i);
    InfoText := InfoText + #13#10 + {sLine +} 'Killing Threads Info' + sLine;
    for i := 0 to ThreadKillingCount - 1 do
      InfoText := InfoText + #13#10 + ThreadKillingInfo(i)
  end
end;

procedure TCnThreadPool.DoProcessRequest(aDataObj: TCnTaskDataObject;
  aThread: TCnPoolingThread);
begin
  if Assigned(FProcessRequest) then
    FProcessRequest(Self, aDataObj, aThread)
end;

procedure TCnThreadPool.DoQueueEmpty(EmptyKind: TEmptyKind);
begin
  if Assigned(FQueueEmpty) then
    FQueueEmpty(Self, EmptyKind)
end;

procedure TCnThreadPool.DoThreadFinalizing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadFinalizing) then
    FThreadFinalizing(Self, aThread)
end;

procedure TCnThreadPool.DoThreadInitializing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadInitializing) then
    FThreadInitializing(Self, aThread)
end;

procedure TCnThreadPool.FreeFinishedThreads;
var
  i: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    for i := FThreadsKilling.Count - 1 downto 0 do
      if TCnPoolingThread(FThreadsKilling[i]).IsFinished then
      begin
        TCnPoolingThread(FThreadsKilling[i]).Free;
        FThreadsKilling.Delete(i)
      end

  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.GetRequest(var Request: TCnTaskDataObject);
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.GetRequest');
{$ENDIF DEBUG}

  csQueueManagment.Enter;
  try
    while (FLastGetPoint < FQueue.Count) and (FQueue[FLastGetPoint] = nil) do
      Inc(FLastGetPoint);

    if (FQueue.Count > QueuePackCount) and
      (FLastGetPoint >= FQueue.Count * 3 div 4) then
    begin
{$IFDEF DEBUG}
      Trace('FQueue.Pack');
{$ENDIF DEBUG}

      FQueue.Pack;
      FTaskCount := FQueue.Count;
      FLastGetPoint := 0
    end;

    Request := TCnTaskDataObject(FQueue[FLastGetPoint]);
    FQueue[FLastGetPoint] := nil;
    Dec(FTaskCount);
    Inc(FLastGetPoint);

    if (FLastGetPoint = FQueue.Count) then
    begin
      DoQueueEmpty(ekQueueEmpty);
      FQueue.Clear;
      FTaskCount := 0;
      FLastGetPoint := 0
    end

  finally
    csQueueManagment.Leave
  end
end;

function TCnThreadPool.HasSpareThread: Boolean;
begin
  Result := FIdleThreadCount > 0
end;

function TCnThreadPool.HasTask: Boolean;
begin
  Result := FTaskCount > 0
end;

function TCnThreadPool.FinishedThreadsAreFull: Boolean;
begin
  csThreadManagment.Enter;
  try
    if FThreadsMaxCount > 0 then
      Result := FThreadsKilling.Count >= FThreadsMaxCount div 2
    else
      Result := FThreadsKilling.Count >= 50;
  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.IncreaseThreads;
var
  iAvgWait, iAvgProc: Integer;
  i: Integer;
begin
  csThreadManagment.Enter;
  try
    KillDeadThreads;
    FreeFinishedThreads;

    if FThreads.Count = 0 then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count = 0');
{$ENDIF DEBUG}

      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
      end
    end
    else if FMinAtLeast and (FThreads.Count < FThreadsMinCount) then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMinCount');
{$ENDIF DEBUG}

      for i := FThreads.Count to FThreadsMinCount - 1 do
      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
      end
    end
    else if (FThreads.Count < FThreadsMaxCount) and HasTask and not HasSpareThread then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMaxCount');
{$ENDIF DEBUG}
      i := TaskCount;
      if i <= 0 then
        Exit;

      iAvgWait := Max(AverageWaitingTime, 1);
      if iAvgWait > 100 then
        Exit;

      iAvgProc := Max(AverageProcessingTime, 2);
{$IFDEF DEBUG}
      Trace(Format(
        'ThreadCount(%D);ThreadsMaxCount(%D);AvgWait(%D);AvgProc(%D);TaskCount(%D);Killing(%D)',
        [FThreads.Count, FThreadsMaxCount, iAvgWait, iAvgProc, i, ThreadKillingCount]));
{$ENDIF DEBUG}

      //if i * iAvgWait * 2 > iAvgProc * FThreads.Count then
      if ((iAvgProc + iAvgWait) * i > iAvgProc * FThreads.Count) then
      begin
        try
          FThreads.Add(FThreadClass.Create(Self));
        except
{$IFDEF DEBUG}
          on E: Exception do
            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
        end
      end
    end

  finally
    csThreadManagment.Leave
  end
end;

function TCnThreadPool.Info: string;
begin
  if csThreadManagment.TryEnter then
  begin
    try
      if Assigned(FGetInfo) then
      begin
        FGetInfo(Self, Result)
      end
      else
        DefaultGetInfo(Self, Result)
    finally
      csThreadManagment.Leave
    end;
  end
  else
  begin
    Result := 'Too busy to get info.';
  end;
end;

procedure TCnThreadPool.KillDeadThreads;
var
  i, iLen: Integer;
  LThread: TCnPoolingThread;
  LObjects: array of TCnTaskDataObject;
begin
  if FinishedThreadsAreFull then Exit;

  iLen := 0;
  SetLength(LObjects, iLen);
  if csThreadManagment.TryEnter then
  try
    for i := FThreads.Count - 1 downto 0 do
    begin
      LThread := TCnPoolingThread(FThreads[i]);
      if LThread.IsDead then
      begin
        if FDeadTaskAsNew then
        begin
          Inc(iLen);
          SetLength(LObjects, iLen);
          LObjects[iLen - 1] := LThread.CloneData;
        end;

        LThread.Terminate(False);
        FThreadsKilling.Add(LThread);
        FThreads.Delete(i);

//        else
//        try
//          FThreads.Add(FThreadClass.Create(Self));
//        except
//{$IFDEF DEBUG}
//          on E: Exception do
//            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
//{$ENDIF DEBUG}
//        end
      end
    end
  finally
    csThreadManagment.Leave
  end;
  AddRequests(LObjects, []);
end;

function TCnThreadPool.OSIsWin9x: Boolean;
begin
  Result := FOsIsWin9x;
end;

function TCnThreadPool.AverageProcessingTime: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for i := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[i]).AverageProcessingTime);
    Result := Result div FThreads.Count
  end
  else
    Result := 20
end;

function TCnThreadPool.AverageWaitingTime: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for i := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[i]).AverageWaitingTime);
    Result := Result div FThreads.Count
  end
  else
    Result := 10
end;

procedure TCnThreadPool.RemoveRequest(aDataObject: TCnTaskDataObject);
begin
  csQueueManagment.Enter;
  try
    FQueue.Remove(aDataObject);
    Dec(FTaskCount);
    FreeAndNil(aDataObject)
  finally
    csQueueManagment.Leave
  end
end;

procedure TCnThreadPool.SetAdjustInterval(const Value: DWORD);
var
  DueTo: Int64;
begin
  FAdjustInterval := Value;
  if hTimReduce <> 0 then
    SetWaitableTimer(hTimReduce, DueTo, Value, nil, nil, False)
end;

function TCnThreadPool.TaskCount: Integer;
begin
  Result := FTaskCount;
end;

function TCnThreadPool.ThreadCount: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    Result := FThreads.Count
  finally
    csThreadManagment.Leave
  end
  else
    Result := -1
end;

function TCnThreadPool.ThreadInfo(const i: Integer): string;
begin
  Result := '';

  if csThreadManagment.TryEnter then
  try
    if i < FThreads.Count then
      Result := TCnPoolingThread(FThreads[i]).Info
  finally
    csThreadManagment.Leave
  end
end;

function TCnThreadPool.ThreadKillingCount: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    Result := FThreadsKilling.Count
  finally
    csThreadManagment.Leave
  end
  else
    Result := -1
end;

function TCnThreadPool.ThreadKillingInfo(const i: Integer): string;
begin
  Result := '';

  if csThreadManagment.TryEnter then
  try
    if i < FThreadsKilling.Count then
      Result := TCnPoolingThread(FThreadsKilling[i]).Info
  finally
    csThreadManagment.Leave;
  end;
end;

procedure TCnThreadPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnThreadPoolName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnThreadPoolComment;
end;

{$IFDEF DEBUG}

procedure TCnThreadPool.Trace(const Str: string);
begin
  TraceLog(Str, 0)
end;
{$ENDIF DEBUG}

var
  V: TOSVersionInfo;
initialization
  V.dwOSVersionInfoSize := SizeOf(V);
  FOSIsWin9x := GetVersionEx(V) and
    (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
    
{$IFDEF DEBUG}
  TraceLog := SimpleTrace;
{$ENDIF DEBUG}

finalization

end.

