/*> Ver: V1.8 *********      History      ***************************\

Beta V1.0b0     10/22/1998      Released
Beta V1.0b1     11/06/1998      TBMTrhreadGroup's been added. Some new methods and properties.
Beta V1.0b2     11/09/1998      Percentage's been added to the update events.
Beta V1.0b3     11/09/1998      Missing header file's been added.
Beta V1.0b4     02/10/1999      Critical section bug in GetCountRuning () has been fixed.
Beta V1.0b5     04/15/1999      BCB 4.0 Support.
Beta V1.0b6     04/29/1999      Fixed naming convention.
Beta V1.0b7     05/12/1999      The Synchro method editors have been added.
Beta V1.0b8     05/19/1999      The dfm file has been changed to avoid some warnings in BCB 3.0.
Beta V1.0b9     08/11/2001      Some fixes for subthread groups support.
Beta V1.1b0     02/07/2003      BCB 6.0 support.
V1.2            12/05/2004      UpdateEnabled added.
V1.3            12/12/2004      OnTerminate bug fixed.
V1.4            03/01/2008      Added C++ Builder 2006 and C++ Builder 2007 support.
V1.5            01/25/2009      Added C++ Builder 2009 support.
V1.6            09/14/2009      Added C++ Builder 2010 support.
V1.7            10/23/2012      Added C++ Builder XE, XE2, and XE3 support.
V1.8            11/11/2015      Added C++ Builder XE4, XE5, XE6, XE7, XE8, and 10 Seattle support.

Legal issues: Copyright (C) 1998 - 2015 by Boian Mitov
              mitov@mitov.com
              www.mitov.com

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

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef BMThreadH
#define BMThreadH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <syncobjs.hpp>
#include <Buttons.hpp>
#include <StdCtrls.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//#include <ExptIntf.hpp>
//---------------------------------------------------------------------------
namespace Bmthread
{
//---------------------------------------------------------------------------
class PACKAGE TBMExecuteThread;
class PACKAGE TBMThread;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TBMThreadNotifyEvent)( System::TObject* Sender, TBMExecuteThread *Thread );
typedef void __fastcall (__closure *TBMThreadSynchroNotifyEvent)( TBMThread *Sender, TBMExecuteThread *Thread );
typedef void __fastcall (__closure *TBMThreadDataNotifyEvent)( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data );
typedef void __fastcall (__closure *TBMThreadSynchroDataNotifyEvent)( TBMThread *Sender, TBMExecuteThread *Thread, void *&Data );
typedef void __fastcall (__closure *TBMThreadUpdateNotifyEvent)( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data, int Percent );
//---------------------------------------------------------------------------
class PACKAGE TBMExecuteThread : public TThread
{
  friend class TBMThread;

protected :
  TBMThread *const ThreadOwner;

protected :
  void                     *FData;
  TNotifyEvent              FNotifyEvent;
  TBMThreadSynchroNotifyEvent      FThreadNotifyEvent;
  TBMThreadSynchroDataNotifyEvent  FThreadDataNotifyEvent;

  void                     *UpdateData;
  
public :
  int                       PercentProgress;

protected :
//  void __fastcall SynchroMethod();

protected :
  void __fastcall DoNotifySynchro();
  void __fastcall DoThreadNotifySynchro();
  void __fastcall DoThreadNotifySynchroData();

public :
  void __fastcall Synchronize ( TNotifyEvent SynchroEvent );
  void __fastcall Synchronize ( TBMThreadSynchroNotifyEvent SynchroEvent );
  void __fastcall Synchronize ( TBMThreadSynchroDataNotifyEvent SynchroEvent, void *Data );
// The folowing two functions are obsolete and should not be used in future !!!
// Obsolete !!! Don't use any mote !!! >>>>
  void __fastcall Synchronize ( TBMThreadNotifyEvent SynchroEvent );                    // For backward compatibility !!!
  void __fastcall Synchronize ( TBMThreadDataNotifyEvent SynchroEvent, void *Data );    // For backward compatibility !!!
// <<< Obsolete !!! Don't use any mote !!!

protected :
  virtual void __fastcall Execute();

public :
  __fastcall TBMExecuteThread ( TBMThread *_ThreadOwner, void *_UpdateData );

public :
  __property Terminated;
};
//---------------------------------------------------------------------------
class PACKAGE TBMThreadGroup;
//---------------------------------------------------------------------------
//CE_Desc_Begin(TBMThreadBase)
/*This is the base class for the TBMThread and TBMThreadGroup classes.
Never create an instance of this class. Always create TBMThread or TBMThreadGroup. */
//CE_Desc_End
class PACKAGE TBMThreadBase : public TComponent
{
  friend class TBMThreadGroup;
  typedef TComponent inherited;

protected:
  TBMThreadUpdateNotifyEvent    FOnUpdate;
  TBMThreadDataNotifyEvent      FOnStart;
  TBMThreadDataNotifyEvent      FOnTerminate;

  TBMThreadGroup           *FThreadGroup;

  int                       FUpdatePriority;

protected:
  TCriticalSection         *UpdateCriticalSection;

public:
    __fastcall  TBMThreadBase ( TComponent* Owner );
    __fastcall ~TBMThreadBase();

protected:
  virtual bool      __fastcall GetRuning() = 0;

  void    __fastcall SetThreadGroup ( TBMThreadGroup *_ThreadGroup );

protected:
  virtual void __fastcall ReportStarted ( TBMThreadGroup *ThreadGroup ) = 0;
  virtual void __fastcall ReportTerminated ( TBMThreadGroup *ThreadGroup ) = 0;
  virtual void __fastcall AddMe ( TBMThreadGroup *ThreadGroup ) = 0;
  virtual void __fastcall RemoveMe ( TBMThreadGroup *ThreadGroup ) = 0;

public:
   virtual void __fastcall Start() = 0;
   virtual void __fastcall Start ( void *Data ) = 0;
   virtual void __fastcall Stop() = 0;

   virtual void __fastcall Suspend() = 0;
   virtual void __fastcall Resume() = 0;

public:
  __property bool               Runing          = { read=GetRuning };
//CE_Desc_Begin(TBMThreadBase.ThreadGroup)
/*ccccccc*/
//CE_Desc_End
  __property TBMThreadGroup    *ThreadGroup     = { read = FThreadGroup, write = SetThreadGroup };
  __property int                UpdatePriority  = { read = FUpdatePriority, write = FUpdatePriority, default = 0 }; 

  __property TBMThreadUpdateNotifyEvent OnUpdate        = { read = FOnUpdate,  write = FOnUpdate };
  __property TBMThreadDataNotifyEvent   OnStart         = { read = FOnStart,  write = FOnStart };
  __property TBMThreadDataNotifyEvent   OnTerminate     = { read = FOnTerminate,  write = FOnTerminate };
};
//---------------------------------------------------------------------------
class PACKAGE TBMThreadGroup : public TBMThreadBase
{
protected:
  TList     *ThreadItemsList;
  TList     *ThreadsList;

protected:
  TBMThreadUpdateNotifyEvent    FOnThreadUpdate;
  TBMThreadDataNotifyEvent      FOnThreadStart;
  TBMThreadDataNotifyEvent      FOnThreadTerminate;


//---------------------------------------------------------------------------
// Protected by CriticalSection
  int                       FCountRuning;
//---------------------------------------------------------------------------

protected:
  virtual bool      __fastcall GetRuning();
  virtual int       __fastcall GetCountRuning();
  
  int            __fastcall GetThreadItemsCount();
  TBMThreadBase *__fastcall GetThreadItems ( int Index );

  int            __fastcall GetThreadsCount();
  TBMThread     *__fastcall GetThreads ( int Index );

protected:
  virtual void __fastcall ReportStarted ( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall ReportTerminated ( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall AddMe ( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall RemoveMe ( TBMThreadGroup *ThreadGroup );

public:
    virtual __fastcall  TBMThreadGroup ( TComponent* Owner );
    virtual __fastcall ~TBMThreadGroup();

public:
   virtual void __fastcall Start();
   virtual void __fastcall Start ( void *Data );
   virtual void __fastcall Stop();

   virtual void __fastcall Suspend();
   virtual void __fastcall Resume();

public:
   void __fastcall AddThread ( TBMThread *_Thread );
   void __fastcall RemoveThread ( TBMThread *_Thread );
   
   void __fastcall AddThreadGroup ( TBMThreadGroup *_ThreadGroup );
   void __fastcall RemoveThreadGroup ( TBMThreadGroup *_ThreadGroup );

public:
   void __fastcall ThreadStarted ( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data );
   void __fastcall ThreadUpdate ( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data, int _UpdatePriority, int PercentProgress );
   void __fastcall ThreadTerminated ( System::TObject* Sender, TBMExecuteThread *Thread, void *&Data );

public:
  __property Runing;
  __property int                        CountRuning          = { read = GetCountRuning };

  __property int                        ThreadItemsCount     = { read = GetThreadItemsCount };
  __property TBMThreadBase             *ThreadItems [int]    = { read = GetThreadItems };

  __property int                        ThreadsCount         = { read = GetThreadsCount };
  __property TBMThread                 *Threads [int]        = { read = GetThreads };
  
__published:
  __property ThreadGroup;
  __property UpdatePriority;

  __property OnUpdate;
  __property OnStart;
  __property OnTerminate;

  __property TBMThreadUpdateNotifyEvent OnThreadUpdate       = { read = FOnThreadUpdate,    write = FOnThreadUpdate };
  __property TBMThreadDataNotifyEvent   OnThreadStart        = { read = FOnThreadStart,     write = FOnThreadStart };
  __property TBMThreadDataNotifyEvent   OnThreadTerminate    = { read = FOnThreadTerminate, write = FOnThreadTerminate };
};
//---------------------------------------------------------------------------
class PACKAGE TBMThread : public TBMThreadBase
{
private:
  friend class TBMExecuteThread;
  friend class TBMThreadGroup;

protected:
  TTimer                   *FUpdateTimer;
  TBMThreadDataNotifyEvent  FOnExecute;
  TBMExecuteThread         *FExecuteThread;
  TThreadPriority           FPriority;
  bool                      FUpdateEnabled;

  bool                      FInterrupted;

protected:
  virtual void __fastcall OnUpdateTimer( System::TObject* Sender );
  virtual void __fastcall Execute();

  virtual void __fastcall OnThreadTerminate( System::TObject* Sender );
  virtual void __fastcall OnThreadTerminateClose( System::TObject* Sender );

  virtual void __fastcall ReportStarted( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall ReportTerminated( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall AddMe( TBMThreadGroup *ThreadGroup );
  virtual void __fastcall RemoveMe( TBMThreadGroup *ThreadGroup );

protected:
  void      __fastcall SetTimeInterval( Cardinal TimeInterval );
  Cardinal  __fastcall GetTimeInterval();

  void      __fastcall SetPriority( TThreadPriority Value );
  TThreadPriority __fastcall GetPriority();

  void      __fastcall SetGetUpdateEnabled( bool Value );

  virtual bool      __fastcall GetRuning();

  TPersistent *__fastcall GetEmptyEntry();

public:
    __fastcall  TBMThread ( TComponent* Owner );
    __fastcall ~TBMThread();

public:
   virtual void __fastcall Start();
   virtual void __fastcall Start ( void *Data );
   virtual void __fastcall Stop();

   virtual void __fastcall Suspend();
   virtual void __fastcall Resume();

public:
  __property Runing;
  __property bool                       Interrupted     = { read=FInterrupted };
  __property TBMExecuteThread          *Thread          = { read=FExecuteThread };

private:

__published:
  __property Cardinal                   RefreshInterval = { read = GetTimeInterval, write = SetTimeInterval, default = 100 };
  __property TThreadPriority            Priority = {read=GetPriority, write=SetPriority, default = tpNormal };
  __property TPersistent               *SynchroMethods = { read=GetEmptyEntry };
  __property bool                       UpdateEnabled = { read=FUpdateEnabled, write=SetGetUpdateEnabled };
  __property ThreadGroup;
  __property UpdatePriority;

  __property TBMThreadDataNotifyEvent   OnExecute       = { read = FOnExecute, write = FOnExecute };
  __property OnUpdate;
  __property OnStart;
  __property OnTerminate;
};
//---------------------------------------------------------------------------
};
//---------------------------------------------------------------------------
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmthread;
#endif
//---------------------------------------------------------------------------
#endif

