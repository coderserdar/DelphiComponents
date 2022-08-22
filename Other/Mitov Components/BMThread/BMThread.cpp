//---------------------------------------------------------------------------
#include <vcl.h>
#include <clipbrd.hpp>
#pragma hdrstop
#include "BMThread.h"

//---------------------------------------------------------------------------
#pragma package( smart_init )

//---------------------------------------------------------------------------
//ValidCtrCheck is used to assure that the components created do not have
//any pure virtual functions.
//

namespace		Bmthread
{                                     
//  bool Invoking = false;

// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TBMThread *)
{
    new TBMThread(NULL);
}
static inline void ValidCtrCheck(TBMThreadGroup *)
{
    new TBMThreadGroup(NULL);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMThreadBase::TBMThreadBase(TComponent* Owner)
    : TComponent(Owner),
    FUpdatePriority( 0 ),
    FThreadGroup( NULL )
{
  UpdateCriticalSection = new TCriticalSection;
}
//---------------------------------------------------------------------------
__fastcall TBMThreadBase::~TBMThreadBase()
{
  SetThreadGroup( NULL );
  delete UpdateCriticalSection;
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadBase::SetThreadGroup( TBMThreadGroup *_ThreadGroup )
{
  if( _ThreadGroup == FThreadGroup )
    return;

  if( FThreadGroup )
    RemoveMe( FThreadGroup );
//    FThreadGroup->RemoveThread( this );

  FThreadGroup = _ThreadGroup;

  if( FThreadGroup )
    AddMe( FThreadGroup );
//    FThreadGroup->AddThread( this );

}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMThread::TBMThread(TComponent* Owner)
    : TBMThreadBase(Owner),
    FExecuteThread( NULL )
{
  FPriority = tpNormal;

  FUpdateTimer = new TTimer( this );
  FUpdateTimer->Enabled = false;
  FUpdateTimer->OnTimer = OnUpdateTimer;
  FUpdateTimer->Interval = 100;
}
//---------------------------------------------------------------------------
__fastcall TBMThread::~TBMThread()
{
  FUpdateTimer->Enabled = false;
  if( FExecuteThread )
    {
    FExecuteThread->OnTerminate = OnThreadTerminateClose;
    FExecuteThread->Terminate();
    while( FExecuteThread )
      Application->ProcessMessages();
//    FExecuteThread = NULL;
    }

  delete FUpdateTimer;
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::SetTimeInterval( Cardinal TimeInterval )
{
  FUpdateTimer->Interval = TimeInterval;
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Execute()
{
  if( OnExecute )
    OnExecute( this, FExecuteThread, FExecuteThread->UpdateData );

  FInterrupted = FExecuteThread->Terminated;
}
//---------------------------------------------------------------------------
bool __fastcall TBMThread::GetRuning()
{
  return( FExecuteThread != NULL );
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::SetGetUpdateEnabled( bool Value )
{
  if( Value == FUpdateEnabled )
    return;

  FUpdateEnabled = Value;
  FUpdateTimer->Enabled = Value; 
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Start( void *Data )
{
  if( FExecuteThread )
    return;

  FExecuteThread = new TBMExecuteThread( this, Data );
  FInterrupted = false;

  ReportStarted( ThreadGroup );

  if( OnStart )
    {
    UpdateCriticalSection->Enter();
    OnStart( this, FExecuteThread, FExecuteThread->UpdateData );
    UpdateCriticalSection->Leave();
    }

  FExecuteThread->Resume();
  FUpdateTimer->Enabled = FUpdateEnabled;
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Start()
{
  Start( NULL );
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::OnThreadTerminateClose( System::TObject* Sender )
{
  FExecuteThread = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::OnThreadTerminate( System::TObject* Sender )
{
  FUpdateTimer->Enabled = false;
  
//  FExecuteThread->Resume();
  FExecuteThread = NULL;

  ReportTerminated( ThreadGroup );

  if( OnTerminate )
    {
    UpdateCriticalSection->Enter();
    OnTerminate( this,( TBMExecuteThread * ) Sender,(( TBMExecuteThread * ) Sender)->UpdateData );
    UpdateCriticalSection->Leave();
    }

  FUpdateTimer->Enabled = FUpdateEnabled;

}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Stop()
{
  if( FExecuteThread )
    {
    FExecuteThread->Resume();
    FExecuteThread->Terminate();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::AddMe( TBMThreadGroup *ThreadGroup )
{
  FThreadGroup->AddThread( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::RemoveMe( TBMThreadGroup *ThreadGroup )
{
  FThreadGroup->RemoveThread( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Suspend()
{
  if( FExecuteThread )
    FExecuteThread->Suspend();
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::Resume()
{
  if( FExecuteThread )
    FExecuteThread->Resume();
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::ReportStarted( TBMThreadGroup *ThreadGroup )
{
  if( Runing )
    if( ThreadGroup )
      ThreadGroup->ThreadStarted( this, FExecuteThread, FExecuteThread->UpdateData );
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::ReportTerminated( TBMThreadGroup *ThreadGroup )
{
  if( ! Runing )
    if( ThreadGroup )
      ThreadGroup->ThreadTerminated( this, FExecuteThread, FExecuteThread->UpdateData );
}
//---------------------------------------------------------------------------
Cardinal  __fastcall TBMThread::GetTimeInterval()
{
  return FUpdateTimer->Interval;
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::OnUpdateTimer(System::TObject* Sender)
{
  if( ! FExecuteThread )
    return;

  if( ThreadGroup )
    ThreadGroup->ThreadUpdate( this, FExecuteThread, FExecuteThread->UpdateData, UpdatePriority, FExecuteThread->PercentProgress );
    
  if( OnUpdate )
    {
    UpdateCriticalSection->Enter();
    OnUpdate( this, FExecuteThread, FExecuteThread->UpdateData, FExecuteThread->PercentProgress );
    UpdateCriticalSection->Leave();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMThread::SetPriority( TThreadPriority Value )
{
  FPriority = Value;
  if( FExecuteThread )
    FExecuteThread->Priority = Value;
}
//---------------------------------------------------------------------------
TThreadPriority __fastcall TBMThread::GetPriority()
{
  return FPriority;
}
//---------------------------------------------------------------------------
TPersistent *__fastcall TBMThread::GetEmptyEntry()
{
  return NULL;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMExecuteThread::TBMExecuteThread( TBMThread *_ThreadOwner, void *_UpdateData ) :
  TThread( true ),
  ThreadOwner( _ThreadOwner ),
  UpdateData( _UpdateData )
{
  OnTerminate = ThreadOwner->OnThreadTerminate;
  FreeOnTerminate = true;
  Priority = ThreadOwner->FPriority;

  PercentProgress = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::Synchronize( TNotifyEvent SynchroEvent )
{
  if( Terminated )
    return;

  if( ! SynchroEvent )
    return;

  FNotifyEvent = SynchroEvent;
  TThread::Synchronize( &DoNotifySynchro );
}
//---------------------------------------------------------------------------
// The folowing two functions are obsolete and should not be used in future !!!
// Obsolete !!! Don't use any mote !!! >>>>
// For backward compatibility !!!
void __fastcall TBMExecuteThread::Synchronize( TBMThreadNotifyEvent SynchroEvent )
{
  Synchronize(( TBMThreadSynchroNotifyEvent ) SynchroEvent );
}
// <<< Obsolete !!! Don't use any mote !!!
//---------------------------------------------------------------------------
// Obsolete !!! Don't use any mote !!! >>>>
// For backward compatibility !!!
void __fastcall TBMExecuteThread::Synchronize( TBMThreadDataNotifyEvent SynchroEvent, void *Data )
{
  Synchronize(( TBMThreadSynchroDataNotifyEvent ) SynchroEvent, Data );
}
// <<< Obsolete !!! Don't use any mote !!!
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::Synchronize( TBMThreadSynchroNotifyEvent SynchroEvent )
{
  if( Terminated )
    return;

  if( ! SynchroEvent )
    return;

  FThreadNotifyEvent = SynchroEvent;
  TThread::Synchronize( &DoThreadNotifySynchro );
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::Synchronize( TBMThreadSynchroDataNotifyEvent SynchroEvent, void *Data )
{
  if( Terminated )
    return;
    
  if( ! SynchroEvent )
    return;

  FThreadDataNotifyEvent = SynchroEvent;
  FData = Data;
  TThread::Synchronize( &DoThreadNotifySynchroData );
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::DoNotifySynchro()
{
  if( FNotifyEvent )
    FNotifyEvent( ThreadOwner );
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::DoThreadNotifySynchro()
{
  if( FThreadNotifyEvent )
    FThreadNotifyEvent( ThreadOwner, this );
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::DoThreadNotifySynchroData()
{
  if( FThreadDataNotifyEvent )
    FThreadDataNotifyEvent( ThreadOwner, this, FData );
}
//---------------------------------------------------------------------------
void __fastcall TBMExecuteThread::Execute(void)
{
  ThreadOwner->Execute();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMThreadGroup::TBMThreadGroup(TComponent* Owner)
    : TBMThreadBase(Owner),
    FCountRuning( 0 )
{
  OnThreadStart = NULL;
  OnThreadTerminate = NULL;
  
  ThreadItemsList = new TList;
  ThreadsList = new TList;
}
//---------------------------------------------------------------------------
__fastcall TBMThreadGroup::~TBMThreadGroup()
{
  while( ThreadsCount )
    Threads [ 0 ]->ThreadGroup = NULL;

  delete ThreadsList;
  delete ThreadItemsList;
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::AddThread( TBMThread *_Thread )
{
  if( _Thread->Runing )
    _Thread->ReportStarted( this );

//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();
  ThreadItemsList->Add( _Thread );
  ThreadsList->Add( _Thread );
  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::AddThreadGroup( TBMThreadGroup *_ThreadGroup )
{
  if( _ThreadGroup->Runing )
    _ThreadGroup->ReportStarted( this );

//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();
  ThreadItemsList->Add( _ThreadGroup );
  for( int i = 0; i < _ThreadGroup->ThreadsCount; i ++ )
    ThreadsList->Add( _ThreadGroup->Threads [ i ] );
    
  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::RemoveThread( TBMThread *_Thread )
{
  if( _Thread->Runing )
    _Thread->ReportTerminated( this );

//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();
  ThreadItemsList->Remove( _Thread );
  ThreadsList->Remove( _Thread );
  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::RemoveThreadGroup( TBMThreadGroup *_ThreadGroup )
{
  if( _ThreadGroup->Runing )
    _ThreadGroup->ReportTerminated( this );

//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();
  ThreadItemsList->Remove( _ThreadGroup );
  for( int i = 0; i < _ThreadGroup->ThreadsCount; i ++ )
    ThreadsList->Remove( _ThreadGroup->Threads [ i ] );

  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::Start()
{
  Start( NULL );
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::Start( void *Data )
{
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->Start( Data );
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::Stop()
{
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->Stop();
}
//---------------------------------------------------------------------------
bool __fastcall TBMThreadGroup::GetRuning()
{
  return( GetCountRuning() > 0 );
}
//---------------------------------------------------------------------------
int __fastcall TBMThreadGroup::GetCountRuning()
{
  UpdateCriticalSection->Enter();
  int Return = FCountRuning;
  UpdateCriticalSection->Leave();

  return Return;
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::Suspend()
{
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->Suspend();
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::Resume()
{
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->Resume();
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::AddMe( TBMThreadGroup *ThreadGroup )
{
  FThreadGroup->AddThreadGroup( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::RemoveMe( TBMThreadGroup *ThreadGroup )
{
  FThreadGroup->RemoveThreadGroup( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::ThreadStarted( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data )
{
//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();
  int TmpCountRuning = FCountRuning;
   
  FCountRuning ++;
  if( OnThreadStart )
    OnThreadStart( Sender, Thread, Data );

  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------

  if( ThreadGroup )
    ThreadGroup->ThreadStarted( Sender, Thread, Data );

  if( ! TmpCountRuning )
    {
    if( OnStart )
      OnStart( Sender, Thread, Data );
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::ThreadUpdate( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data, int _UpdatePriority, int PercentProgres )
{
  if( OnThreadUpdate )
    OnThreadUpdate( Sender, Thread, Data, PercentProgres );

  if( OnUpdate )
    {
    for( int i = 0; i < ThreadItemsCount; i ++ )
      {
      TBMThreadBase *Thread = ThreadItems [ i ];
      if( Thread->Runing )
        if( Thread->UpdatePriority > _UpdatePriority )
          return;
      }
      
    OnUpdate( Sender, Thread, Data, PercentProgres );
    }
}
//---------------------------------------------------------------------------
int __fastcall TBMThreadGroup::GetThreadItemsCount()
{
  return ThreadItemsList->Count;
}
//---------------------------------------------------------------------------
TBMThreadBase *__fastcall TBMThreadGroup::GetThreadItems( int Index )
{
  return((TBMThreadBase *)ThreadItemsList->Items [ Index ] );
}
//---------------------------------------------------------------------------
int __fastcall TBMThreadGroup::GetThreadsCount()
{
  return ThreadsList->Count;
}
//---------------------------------------------------------------------------
TBMThread *__fastcall TBMThreadGroup::GetThreads( int Index )
{
  return((TBMThread *)ThreadsList->Items [ Index ] );
}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::ThreadTerminated( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data )
{
//---------------------------------------------------------------------------
// Protected by CriticalSection
  UpdateCriticalSection->Enter();

  if( FCountRuning )
    FCountRuning --;

  int TempCount = FCountRuning;
  UpdateCriticalSection->Leave();
//---------------------------------------------------------------------------

  if( OnThreadTerminate )
    OnThreadTerminate( Sender, Thread, Data );

  if( ThreadGroup )
    ThreadGroup->ThreadTerminated( Sender, Thread, Data );

  if( ! TempCount )
    {
    if( OnTerminate )
      OnTerminate( Sender, Thread, Data );
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::ReportStarted( TBMThreadGroup *ThreadGroup )
{
  if( ThreadGroup )
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->ReportStarted( ThreadGroup );

}
//---------------------------------------------------------------------------
void __fastcall TBMThreadGroup::ReportTerminated( TBMThreadGroup *ThreadGroup )
{
  if( ThreadGroup )
    for( int i = 0; i < ThreadItemsCount; i ++ )
      ThreadItems [ i ]->ReportTerminated( ThreadGroup );

}
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
    TComponentClass classes[] = {__classid(TBMThread), __classid(TBMThreadGroup) };
    RegisterComponents("BMitov", classes, 1);
    }

};
