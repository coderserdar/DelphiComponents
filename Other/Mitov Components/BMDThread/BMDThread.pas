{*> Ver: V2.2 *********      History      ***************************\

Beta V1.0b0     04/29/1999      Released
Beta V1.0b1     05/24/1999      The Synchro method editors have been added.
Beta V1.0b2     08/01/2001      Some modifications by Walter Campelo.
Beta V1.0b3     08/03/2001      Ported to Delphi 6 Restricted group connection to itself.
Beta V1.0b4     08/11/2001      Bug in supporting subgroups fixed.
Beta V1.1b0     02/22/2001      Delphi 7 support added.
V1.2            12/05/2004      Delphi 2005 support added. UpdateEnabled added.
V1.3            12/12/2004      OnTerminate bug fixed.
V1.4            01/27/2006      Delphi 2006 support added.
V1.5            01/27/2008      Delphi 2007 support added.
V1.6            01/25/2009      Delphi 2009 support added.
V1.6.1          01/28/2009      Added missing D2006 and D2005 project files.
V1.7            09/14/2009      Delphi 2010 support added.
V1.7.1          09/20/2009      Fixed D2006 package.
V1.8            11/20/2011      Delphi XE and XE2 support added. Changed to CheckSynchronize on Destroy.
V1.8.1          11/28/2011      Added 64 bit support.
V1.9            10/22/2012      Delphi XE3 support added.
V2.0            05/16/2014      Delphi XE4, XE5 and XE6 support added.
V2.1            11/10/2015      Delphi XE7, XE8 and 10 Seattle support added.
V2.2            02/14/2022      Delphi 10.1 Berlin, 10.2 Tokyo, 10.3 Rio, 10.4 Sydney, and 11 Alexandria support added.

License:      Copyright (C) 1999 - 2022 by Boian Mitov
              mitov@mitov.com
              www.mitov.com
              www.openwire.org

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

\***************************************************************************}
unit BMDThread;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Syncobjs, ExtCtrls;

type
  { Forward class declarations }
  TBMDThread = class;
  TBMDExecuteThread = class;
  TBMDThreadGroup = class;

  { Thread events }
  TBMDThreadNotifyEvent = procedure(Sender: TObject; Thread: TBMDExecuteThread) of object;
  TBMDThreadDataNotifyEvent = procedure(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer) of object;
  TBMDThreadUpdateNotifyEvent = procedure(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer; Percent: Integer) of object;
  TBMDThreadSynchroNotifyEvent = procedure(Sender: TBMDThread; Thread: TBMDExecuteThread) of object;
  TBMDThreadSynchroDataNotifyEvent = procedure(Sender: TBMDThread; Thread: TBMDExecuteThread; var Data: Pointer) of object;

  { TBMDExecuteThread class }
  TBMDExecuteThread = class(TThread)
  private
    { Private declarations }
  protected
    { Protected declarations }
    ThreadOwner: TBMDThread;
    FData: Pointer;
    FNotifyEvent: TNotifyEvent;
    FThreadNotifyEvent: TBMDThreadSynchroNotifyEvent;
    FThreadDataNotifyEvent: TBMDThreadSynchroDataNotifyEvent;
    UpdateData: Pointer;
    procedure Execute(); override;
  public
    { Public declarations }
    PercentProgress: Integer;
    constructor Create(AThreadOwner: TBMDThread; AUpdateData: Pointer); virtual;
    procedure DoNotifySynchro;
    procedure DoThreadNotifySynchro;
    procedure DoThreadNotifySynchroData;
    procedure Synchronize(SynchroEvent: TNotifyEvent); overload;
    procedure Synchronize(SynchroEvent: TBMDThreadSynchroNotifyEvent); overload;
    procedure Synchronize(SynchroEvent: TBMDThreadSynchroDataNotifyEvent; Data: Pointer); overload;
    // The folowing two functions are obsolete and should not be used in future !!!
    // Obsolete !!! Don't use any more !!! >>>>
    procedure Synchronize(SynchroEvent: TBMDThreadNotifyEvent); overload;
    procedure Synchronize(SynchroEvent: TBMDThreadDataNotifyEvent; Data: Pointer); overload;
    // <<< Obsolete !!! Don't use any more !!!
    property Terminated;
  end;

  { TBMDThread class }
  TBMDThreadBase = class(TComponent)
  private
    FOnUpdate: TBMDThreadUpdateNotifyEvent;
    FOnStart: TBMDThreadDataNotifyEvent;
    FOnTerminate: TBMDThreadDataNotifyEvent;
    FThreadGroup: TBMDThreadGroup;
    FUpdatePriority: Integer;
  protected
    UpdateCriticalSection: TCriticalSection;
    function GetRuning(): Boolean; virtual; abstract;
    procedure SetThreadGroup(AThreadGroup: TBMDThreadGroup);
    procedure ReportStarted(ThreadGroup: TBMDThreadGroup); virtual; abstract;
    procedure ReportTerminated(ThreadGroup: TBMDThreadGroup); virtual; abstract;
    function  AddMe(AThreadGroup: TBMDThreadGroup) : Boolean; virtual; abstract;
    procedure RemoveMe(AThreadGroup: TBMDThreadGroup); virtual; abstract;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Start(); overload; virtual; abstract;
    procedure Start(Data: Pointer); overload; virtual; abstract;
    procedure Stop(); virtual; abstract;
    procedure Suspend(); virtual; abstract;
    procedure Resume(); virtual; abstract;
  public
    property Runing: Boolean read GetRuning;
    property ThreadGroup: TBMDThreadGroup read FThreadGroup write SetThreadGroup;
    property UpdatePriority: Integer read FUpdatePriority write FUpdatePriority default 0;
    property OnUpdate: TBMDThreadUpdateNotifyEvent read FOnUpdate write FOnUpdate;
    property OnStart: TBMDThreadDataNotifyEvent read FOnStart write FOnStart;
    property OnTerminate: TBMDThreadDataNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  { TBMDThread class }
  TBMDThread = class(TBMDThreadBase)
  protected
    FUpdateTimer: TTimer;
    FOnExecute: TBMDThreadDataNotifyEvent;
    FExecuteThread: TBMDExecuteThread;
    FPriority: TThreadPriority;
    FUpdateEnabled: Boolean;
    FInterrupted: Boolean;
    procedure OnUpdateTimer(Sender: TObject); virtual;
    procedure Execute(); virtual;
    procedure OnThreadTerminate(Sender: TObject); virtual;
    procedure OnThreadTerminateClose(Sender: TObject); virtual;
    procedure ReportStarted(ThreadGroup: TBMDThreadGroup); override;
    procedure ReportTerminated(ThreadGroup: TBMDThreadGroup); override;
    function  AddMe(AThreadGroup: TBMDThreadGroup) : Boolean; override;
    procedure RemoveMe(AThreadGroup: TBMDThreadGroup); override;
    procedure SetTimeInterval(TimeInterval: Cardinal);
    function GetTimeInterval(): Cardinal;
    procedure SetPriority(Value: TThreadPriority);
    function GetPriority(): TThreadPriority;
    function GetRuning(): Boolean; override;
    function GetEmptyEntry(): TPersistent;
    procedure SetUpdateEnabled(Value: Boolean);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Start(); overload; override;
    procedure Start(Data: Pointer); overload; override;
    procedure Stop(); override;
    procedure Suspend(); override;
    procedure Resume(); override;
    property Runing;
    property Interrupted: Boolean read FInterrupted;
    property Thread: TBMDExecuteThread read FExecuteThread;
  published
    property RefreshInterval: Cardinal read GetTimeInterval write SetTimeInterval default 100;
    property Priority: TThreadPriority read GetPriority write SetPriority default tpNormal;
    property ThreadGroup;
    property UpdatePriority;
    property UpdateEnabled : Boolean read FUpdateEnabled write SetUpdateEnabled; 
    property SynchroMethods: TPersistent read GetEmptyEntry;
    property OnExecute: TBMDThreadDataNotifyEvent read FOnExecute write FOnExecute;
    property OnUpdate;
    property OnStart;
    property OnTerminate;
  end;

  TBMDThreadGroup = class(TBMDThreadBase)
  protected
    ThreadItemsList: TList;
    ThreadsList: TList;
    FOnThreadUpdate: TBMDThreadUpdateNotifyEvent;
    FOnThreadStart: TBMDThreadDataNotifyEvent;
    FOnThreadTerminate: TBMDThreadDataNotifyEvent;
    // Protected by CriticalSection
    FCountRuning: Integer;
    function GetRuning(): Boolean; override;
    function GetCountRuning(): Integer; virtual;
    function GetThreadItemsCount(): Integer;
    function GetThreadItems(Index: Integer): TBMDThreadBase;
    function GetThreadsCount(): Integer;
    function GetThreads(Index: Integer): TBMDThread;
    procedure ReportStarted(ThreadGroup: TBMDThreadGroup); override;
    procedure ReportTerminated(ThreadGroup: TBMDThreadGroup); override;
    function  AddMe(AThreadGroup: TBMDThreadGroup) : Boolean; override;
    procedure RemoveMe(AThreadGroup: TBMDThreadGroup); override;
    function LinkedToMe( ThreadBase : TBMDThreadBase ) : Boolean;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Start(); overload; override;
    procedure Start(Data: Pointer); overload; override;
    procedure Stop(); override;
    procedure Suspend(); override;
    procedure Resume(); override;
    function  AddThread(AThread: TBMDThread) : Boolean;
    procedure RemoveThread(AThread: TBMDThread);
    function  AddThreadGroup(AThreadGroup: TBMDThreadGroup) : Boolean;
    procedure RemoveThreadGroup(AThreadGroup: TBMDThreadGroup);
    procedure ThreadStarted(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer);
    procedure ThreadUpdate(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer; AUpdatePriority: Integer; PercentProgres: Integer);
    procedure ThreadTerminated(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer);
    
  public
    property Runing;
    property CountRuning: Integer read GetCountRuning;
    property ThreadItemsCount: Integer read GetThreadItemsCount;
    property ThreadItems[Index: Integer]: TBMDThreadBase read GetThreadItems;
    property ThreadsCount: Integer read GetThreadsCount;
    property Threads[Index: Integer]: TBMDThread read GetThreads;

  published
    property ThreadGroup;
    property UpdatePriority;
    property OnUpdate;
    property OnStart;
    property OnTerminate;
    property OnThreadUpdate: TBMDThreadUpdateNotifyEvent read FOnThreadUpdate write FOnThreadUpdate;
    property OnThreadStart: TBMDThreadDataNotifyEvent read FOnThreadStart write FOnThreadStart;
    property OnThreadTerminate: TBMDThreadDataNotifyEvent read FOnThreadTerminate write FOnThreadTerminate;
  end;

implementation

{ TBMDExecuteThread }

constructor TBMDExecuteThread.Create(AThreadOwner: TBMDThread; AUpdateData: Pointer);
begin
  inherited Create(True);
  ThreadOwner := AThreadOwner;
  UpdateData := AUpdateData;
  OnTerminate := ThreadOwner.OnThreadTerminate;
  FreeOnTerminate := True;
  Priority := ThreadOwner.FPriority;
  PercentProgress := 0;
end;

procedure TBMDExecuteThread.DoNotifySynchro;
begin
  if (Assigned(FNotifyEvent)) then
    FNotifyEvent(ThreadOwner);
end;

procedure TBMDExecuteThread.DoThreadNotifySynchro;
begin
  if (Assigned(FThreadNotifyEvent)) then
    FThreadNotifyEvent(ThreadOwner, Self);
end;

procedure TBMDExecuteThread.DoThreadNotifySynchroData;
begin
  if (Assigned(FThreadDataNotifyEvent)) then
    FThreadDataNotifyEvent(ThreadOwner, Self, FData);
end;

procedure TBMDExecuteThread.Synchronize(SynchroEvent: TNotifyEvent);
begin
  if (Terminated) then
    Exit;
  if (not Assigned(SynchroEvent)) then
    Exit;
  FNotifyEvent := SynchroEvent;
  inherited Synchronize(DoNotifySynchro);
end;

procedure TBMDExecuteThread.Synchronize(SynchroEvent: TBMDThreadSynchroNotifyEvent);
begin
  if (Terminated) then
    Exit;
  if (not Assigned(SynchroEvent)) then
    Exit;
  FThreadNotifyEvent := SynchroEvent;
  inherited Synchronize(DoThreadNotifySynchro);
end;

//---------------------------------------------------------------------------
// The folowing two functions are obsolete and should not be used in future !!!
// <<< Obsolete !!! Don't use any more !!!

procedure TBMDExecuteThread.Synchronize(SynchroEvent: TBMDThreadNotifyEvent);
begin
  Synchronize(TBMDThreadSynchroNotifyEvent(SynchroEvent));
end;

procedure TBMDExecuteThread.Synchronize(SynchroEvent: TBMDThreadDataNotifyEvent; Data: Pointer);
begin
  Synchronize(TBMDThreadSynchroDataNotifyEvent(SynchroEvent), Data);
end;
// End of Obsolete !!! Don't use any more !!! >>>>

procedure TBMDExecuteThread.Synchronize(SynchroEvent: TBMDThreadSynchroDataNotifyEvent; Data: Pointer);
begin
  if (Terminated) then
    Exit;
  if (not Assigned(SynchroEvent)) then
    Exit;
  FThreadDataNotifyEvent := SynchroEvent;
  FData := Data;
  inherited Synchronize(DoThreadNotifySynchroData);
end;

procedure TBMDExecuteThread.Execute();
begin
  ThreadOwner.Execute();
end;

{ TBMDThreadBase }

constructor TBMDThreadBase.Create(Owner: TComponent);
begin
  inherited;
  FUpdatePriority := 0;
  FThreadGroup := nil;
  UpdateCriticalSection := TCriticalSection.Create;
end;

destructor TBMDThreadBase.Destroy;
begin
  SetThreadGroup(nil);
  UpdateCriticalSection.Free;
  inherited;
end;

procedure TBMDThreadBase.SetThreadGroup(AThreadGroup: TBMDThreadGroup);
begin
  if (AThreadGroup = FThreadGroup) then
    Exit;
    
  if (FThreadGroup <> nil) then
    RemoveMe(FThreadGroup);

  FThreadGroup := nil;
  if (AThreadGroup <> nil) then
    begin
    if( AddMe(AThreadGroup)) then
      FThreadGroup := AThreadGroup;
      
    end;

end;

{ TBMDThread }

constructor TBMDThread.Create(Owner: TComponent);
begin
  inherited;
  FExecuteThread := nil;
  FPriority := tpNormal;
  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Enabled := False;
  FUpdateTimer.OnTimer := OnUpdateTimer;
  FUpdateTimer.Interval := 100;
end;

destructor TBMDThread.Destroy;
begin
  FUpdateTimer.Enabled := False;
  if (FExecuteThread <> nil) then
  begin
    FExecuteThread.OnTerminate := OnThreadTerminateClose;
    FExecuteThread.Terminate();
    while (FExecuteThread <> nil) do
{$IFDEF VER130} // Delphi 5.0
      Application.ProcessMessages();
{$ELSE}
      CheckSynchronize();
{$ENDIF}

  end;
  FUpdateTimer.Free;
  inherited;
end;

procedure TBMDThread.OnUpdateTimer(Sender: TObject);
begin
  if (FExecuteThread = nil) then
    Exit;
  if (ThreadGroup <> nil) then
    ThreadGroup.ThreadUpdate(Self, FExecuteThread, FExecuteThread.UpdateData, UpdatePriority, FExecuteThread.PercentProgress);
  if (Assigned(OnUpdate)) then
  begin
    UpdateCriticalSection.Enter();
    OnUpdate(Self, FExecuteThread, FExecuteThread.UpdateData, FExecuteThread.PercentProgress);
    UpdateCriticalSection.Leave();
  end;
end;

procedure TBMDThread.Execute();
begin
  if (Assigned(OnExecute)) then
    OnExecute(Self, FExecuteThread, FExecuteThread.UpdateData);
  FInterrupted := FExecuteThread.Terminated;
end;

procedure TBMDThread.OnThreadTerminate(Sender: TObject);
begin
  FUpdateTimer.Enabled := False;
//  FExecuteThread.Resume();
  FExecuteThread := nil;
  ReportTerminated(ThreadGroup);
  if (Assigned(OnTerminate)) then
  begin
    UpdateCriticalSection.Enter();
    OnTerminate(Self, Sender as TBMDExecuteThread, (Sender as TBMDExecuteThread).UpdateData);
    UpdateCriticalSection.Leave();
  end;
  FUpdateTimer.Enabled := FUpdateEnabled;
end;

procedure TBMDThread.OnThreadTerminateClose(Sender: TObject);
begin
  FExecuteThread := nil;
end;

procedure TBMDThread.ReportStarted(ThreadGroup: TBMDThreadGroup);
begin
  if (Runing) then
    if (ThreadGroup <> nil) then
      ThreadGroup.ThreadStarted(Self, FExecuteThread, FExecuteThread.UpdateData);
end;

procedure TBMDThread.ReportTerminated(ThreadGroup: TBMDThreadGroup);
begin
  if (not Runing) then
    if (ThreadGroup <> nil) then
      ThreadGroup.ThreadTerminated(Self, FExecuteThread, FExecuteThread.UpdateData);
end;

function TBMDThread.AddMe(AThreadGroup: TBMDThreadGroup) : Boolean;
begin
  Result := AThreadGroup.AddThread(Self);
end;

procedure TBMDThread.RemoveMe(AThreadGroup: TBMDThreadGroup);
begin
  AThreadGroup.RemoveThread(Self);
end;

procedure TBMDThread.Suspend();
begin
  if (FExecuteThread <> nil) then
    FExecuteThread.Suspend();
end;

procedure TBMDThread.Resume();
begin
  if (FExecuteThread <> nil) then
    FExecuteThread.Resume();
end;

function TBMDThread.GetTimeInterval(): Cardinal;
begin
  Result := FUpdateTimer.Interval;
end;

procedure TBMDThread.SetTimeInterval(TimeInterval: Cardinal);
begin
  FUpdateTimer.Interval := TimeInterval;
end;

function TBMDThread.GetRuning(): Boolean;
begin
  Result := (FExecuteThread <> nil);
end;

procedure TBMDThread.SetPriority(Value: TThreadPriority);
begin
  FPriority := Value;
  if (FExecuteThread <> nil) then
    FExecuteThread.Priority := Value;
end;

procedure TBMDThread.SetUpdateEnabled(Value: Boolean);
begin
  if( Value = FUpdateEnabled ) then
    Exit;

  FUpdateEnabled := Value;
  FUpdateTimer.Enabled := Value; 
end;

function TBMDThread.GetPriority(): TThreadPriority;
begin
  Result := FPriority;
end;

procedure TBMDThread.Start(Data: Pointer);
begin
  if (FExecuteThread <> nil) then
    Exit;
  FExecuteThread := TBMDExecuteThread.Create(Self, Data);
  FInterrupted := False;
  ReportStarted(ThreadGroup);
  if (Assigned(OnStart)) then
  begin
    UpdateCriticalSection.Enter();
    OnStart(Self, FExecuteThread, FExecuteThread.UpdateData);
    UpdateCriticalSection.Leave();
  end;
  FExecuteThread.Resume();
  FUpdateTimer.Enabled := FUpdateEnabled;
end;

procedure TBMDThread.Start();
begin
  Start(nil);
end;

procedure TBMDThread.Stop();
begin
  if (FExecuteThread <> nil) then
  begin
    FExecuteThread.Resume();
    FExecuteThread.Terminate();
  end;
end;

{ TBMDThreadGroup }

constructor TBMDThreadGroup.Create(Owner: TComponent);
begin
  inherited;
  FCountRuning := 0;
  OnThreadStart := nil;
  OnThreadTerminate := nil;
  ThreadItemsList := TList.Create;
  ThreadsList := TList.Create;
end;

destructor TBMDThreadGroup.Destroy;
begin
  while (ThreadsCount <> 0) do
    Threads[0].ThreadGroup := nil;

  while (ThreadItemsCount <> 0) do
    ThreadItems[0].ThreadGroup := nil;

  ThreadsList.Free;
  ThreadItemsList.Free;
  inherited;
end;

function TBMDThreadGroup.GetRuning(): Boolean;
begin
  Result := (GetCountRuning() > 0);
end;

function TBMDThreadGroup.GetCountRuning(): Integer;
begin
  UpdateCriticalSection.Enter();
  Result := FCountRuning;
  UpdateCriticalSection.Leave();
end;

procedure TBMDThreadGroup.Suspend();
var
  I: Integer;
begin
  for I := 0 to ThreadItemsCount - 1 do
    ThreadItems[I].Suspend();
end;

procedure TBMDThreadGroup.Resume();
var
  I: Integer;
begin
  for I := 0 to ThreadItemsCount - 1 do
    ThreadItems[I].Resume();
end;

function TBMDThreadGroup.AddMe(AThreadGroup: TBMDThreadGroup) : Boolean;
begin
  Result := AThreadGroup.AddThreadGroup(Self);
end;

procedure TBMDThreadGroup.RemoveMe(AThreadGroup: TBMDThreadGroup);
begin
  AThreadGroup.RemoveThreadGroup(Self);
end;

procedure TBMDThreadGroup.ThreadStarted(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer);
var
  TmpCountRuning: Integer;
begin
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  TmpCountRuning := FCountRuning;
  Inc(FCountRuning);
  if (Assigned(OnThreadStart)) then
    OnThreadStart(Sender, Thread, Data);
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
  if (ThreadGroup <> nil) then
    ThreadGroup.ThreadStarted(Sender, Thread, Data);
  if (TmpCountRuning = 0) then
  begin
    if (Assigned(OnStart)) then
      OnStart(Sender, Thread, Data);
  end;
end;

procedure TBMDThreadGroup.ThreadUpdate(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer; AUpdatePriority: Integer; PercentProgres: Integer);
var
  I: Integer;
  IntThread: TBMDThreadBase;
begin
  if (Assigned(OnThreadUpdate)) then
    OnThreadUpdate(Sender, Thread, Data, PercentProgres);

  if (Assigned(OnUpdate)) then
  begin
    for I := 0 to ThreadItemsCount - 1 do
    begin
      IntThread := ThreadItems[I];
      if (IntThread.Runing) then
        if (IntThread.UpdatePriority > AUpdatePriority) then
          Exit;
    end;

    OnUpdate(Sender, Thread, Data, PercentProgres);
  end;
end;

function TBMDThreadGroup.GetThreadItemsCount(): Integer;
begin
  Result := ThreadItemsList.Count;
end;

function TBMDThreadGroup.GetThreadItems(Index: Integer): TBMDThreadBase;
begin
  Result := TBMDThreadBase(ThreadItemsList.Items[Index]);
end;

function TBMDThreadGroup.GetThreadsCount(): Integer;
begin
  Result := ThreadsList.Count;
end;

function TBMDThreadGroup.GetThreads(Index: Integer): TBMDThread;
begin
  Result := TBMDThread(ThreadsList.Items[Index]);
end;

procedure TBMDThreadGroup.ThreadTerminated(Sender: TObject; Thread: TBMDExecuteThread; var Data: Pointer);
var
  TempCount: Integer;
begin
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  if (FCountRuning <> 0) then
    Dec(FCountRuning);
  TempCount := FCountRuning;
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
  if (Assigned(OnThreadTerminate)) then
    OnThreadTerminate(Sender, Thread, Data);
  if (ThreadGroup <> nil) then
    ThreadGroup.ThreadTerminated(Sender, Thread, Data);
  if (TempCount = 0) then
  begin
    if (Assigned(OnTerminate)) then
      OnTerminate(Sender, Thread, Data);
  end;
end;

procedure TBMDThreadGroup.ReportStarted(ThreadGroup: TBMDThreadGroup);
var
  I: Integer;
begin
  if (ThreadGroup <> nil) then
    for I := 0 to ThreadItemsCount - 1 do
      ThreadItems[I].ReportStarted(ThreadGroup);
end;

procedure TBMDThreadGroup.ReportTerminated(ThreadGroup: TBMDThreadGroup);
var
  I: Integer;
begin
  if (ThreadGroup <> nil) then
    for I := 0 to ThreadItemsCount - 1 do
      ThreadItems[I].ReportTerminated(ThreadGroup);
end;

procedure TBMDThreadGroup.Start();
begin
  Start(nil);
end;

procedure TBMDThreadGroup.Start(Data: Pointer);
var
  I: Integer;
begin
  for I := 0 to ThreadItemsCount - 1 do
    ThreadItems[I].Start(Data);
end;

procedure TBMDThreadGroup.Stop();
var
  I: Integer;
begin
  for I := 0 to ThreadItemsCount - 1 do
    ThreadItems[I].Stop();
end;

function TBMDThreadGroup.LinkedToMe( ThreadBase : TBMDThreadBase ) : Boolean;
var
  AThreadBase : TBMDThreadBase;
  
begin
  AThreadBase := Self;
  Result := True;
  while( AThreadBase <> NIL ) do
    begin
    if( AThreadBase = ThreadBase ) then
      Exit;

    AThreadBase := AThreadBase.ThreadGroup;
    end;
    
  Result := False;
end;

function TBMDThreadGroup.AddThread(AThread: TBMDThread) : Boolean;
begin
  Result := False;
  if( LinkedToMe( AThread )) then
    Exit;

  if (AThread.Runing) then
    AThread.ReportStarted(Self);
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  ThreadItemsList.Add(AThread);
  ThreadsList.Add(AThread);
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
  Result := True;
end;

function TBMDThreadGroup.AddThreadGroup(AThreadGroup: TBMDThreadGroup) : Boolean;
var
  I: Integer;
begin
  Result := False;
  if( LinkedToMe( AThreadGroup )) then
    Exit;

  if (AThreadGroup.Runing) then
    AThreadGroup.ReportStarted(Self);
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  ThreadItemsList.Add(AThreadGroup);
  for I := 0 to AThreadGroup.ThreadsCount - 1 do
    ThreadsList.Add(AThreadGroup.Threads[I]);
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
  Result := True;
end;

procedure TBMDThreadGroup.RemoveThread(AThread: TBMDThread);
begin
  if (AThread.Runing) then
    AThread.ReportTerminated(Self);
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  ThreadItemsList.Remove(AThread);
  ThreadsList.Remove(AThread);
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
end;

procedure TBMDThreadGroup.RemoveThreadGroup(AThreadGroup: TBMDThreadGroup);
var
  I: Integer;
begin
  if (AThreadGroup.Runing) then
    AThreadGroup.ReportTerminated(Self);
  // Protected by CriticalSection
  UpdateCriticalSection.Enter();
  ThreadItemsList.Remove(AThreadGroup);
  for I := 0 to AThreadGroup.ThreadsCount - 1 do
    ThreadsList.Remove(AThreadGroup.Threads[I]);
  UpdateCriticalSection.Leave();
  // End of Protected by CriticalSection
end;

function TBMDThread.GetEmptyEntry(): TPersistent;
begin
  Result := nil;
end;

end.

