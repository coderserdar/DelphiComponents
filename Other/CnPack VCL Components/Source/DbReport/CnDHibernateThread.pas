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

unit CnDHibernateThread; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ��߳̿ؼ���Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��   ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.23 V1.8
*          ��ֲ�� Delphi2009
*        2006.09.04 V1.0
*          ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, CnDHibernateBase;

type
  TCnNotifyEventParams = procedure(Sender: TObject; params: ICnMap) of object;

  TCnDHibernateThread = class(TComponent)
  private
    FThreadCount: Integer;
    FExclusif: Boolean;
    FRunOnCreate: Boolean;
    FOnbegin: TNotifyEvent;
    FOnExecute: TCnNotifyEventParams;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    FAbout: string;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function Execute(p: ICnMap): Thandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: Thandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    procedure QuitThread(Thread: Thandle);
    procedure Suspend(Thread: Thandle);
    procedure Resume(Thread: Thandle);

    property About: string read FAbout write FAbout;
    property Exclusif: Boolean read FExclusif write FExclusif;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Onbegin: TNotifyEvent read FOnbegin write FOnBegin;
    property OnExecute: TCnNotifyEventParams read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
  end;

  TCnHideThread = class(TThread)
  private
    FExecuteEvent: TCnNotifyEventParams;
    FParams: ICnMap;
  public
    constructor Create(event: TCnNotifyEventParams; params: ICnMap); virtual;
    procedure Execute; override;
  end;

procedure Synchronize(Method: TNotifyEvent);

procedure SynchronizeParams(Method: TCnNotifyEventParams; p: ICnMap);
  
{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

var
  mtx: THandle;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil);
  ReleaseMutex(mtx);
end;

procedure SynchronizeParams(Method: TCnNotifyEventParams; p: ICnMap);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil, p);
  ReleaseMutex(mtx);
end;

constructor TCnDHibernateThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := true;
  FExclusif := true;
  FreeOnTerminate := true;
end;

destructor TCnDHibernateThread.Destroy;
begin
  inherited Destroy;
end;

function TCnDHibernateThread.Execute(p: ICnMap): Thandle;
var
  HideThread: TCnHideThread;
begin
  result := 0;
  if Assigned(FOnExecute) then
  begin
    if Exclusif then
      if OneThreadIsRunning then
        exit;
    inc(FThreadCount);
    HideThread := TCnHideThread.Create(FOnExecute, p);
    HideThread.FreeOnTerminate := FFreeOnTerminate;
    HideThread.OnTerminate := DoTerminate;
    DoCreate;
    if FRunOnCreate then
      HideThread.Resume;
    result := HideThread.Handle;   { HideThread.ThreadID }
  end;
end;

function TCnDHibernateThread.GetPriority(Thread: Thandle): TThreadPriority;
begin
  result := tpIdle;
  if Thread <> 0 then
    result := TThreadPriority(GetThreadPriority(thread));
end;

procedure TCnDHibernateThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, integer(priority));
end;

procedure TCnDHibernateThread.QuitThread(Thread: Thandle);
begin
  TerminateThread(Thread, 0);
end;

procedure TCnDHibernateThread.Suspend(Thread: Thandle);
begin
  SuspendThread(Thread);
end;

procedure TCnDHibernateThread.Resume(Thread: Thandle);
begin
  ResumeThread(thread);
end;

procedure TCnDHibernateThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

procedure TCnDHibernateThread.DoTerminate;
begin
  Dec(FThreadCount);
  if Assigned(FOnFinish) then
    FOnFinish(nil);
  if FThreadCount = 0 then
    if Assigned(FOnFinishAll) then
      FOnFinishAll(nil);
end;

function TCnDHibernateThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

constructor TCnHideThread.Create(event: TCnNotifyEventParams; params: ICnMap);
begin
  inherited Create(true);
  FExecuteEvent := event;
  FParams := params;
end;

procedure TCnHideThread.Execute;
begin
  FExecuteEvent(nil, FParams);
end;

initialization
  mtx := CreateMutex(nil, False, 'DHibernateThreadMutex');

finalization
  CloseHandle(mtx); 
  
{$ENDIF SUPPORT_ADO}
end.
