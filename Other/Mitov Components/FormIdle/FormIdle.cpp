//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "FormIdle.h"

#if (__BORLANDC__ >= 0x0530)
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
static inline void ValidCtrCheck(TFormIdle *)
{
    new TFormIdle(NULL);
}
//---------------------------------------------------------------------------
HHOOK 		TFormIdle::HookHandle;
TList  *TFormIdle::IdleList = NULL;
//---------------------------------------------------------------------------
DWORD __stdcall TFormIdle::ForegroundIdleProc(
    int code,	// hook code
    DWORD wParam,	// not used
    LONG lParam	// not used
   )
{
  if ( code == HC_ACTION )
    {
    if ( IdleList )
      for ( int i = 0; i < IdleList->Count; i ++ )
        ((TFormIdle *)IdleList->Items [ i ] )->DoIdle ();
    }

  return CallNextHookEx( HookHandle, code, wParam, lParam );
}
//---------------------------------------------------------------------------
__fastcall TFormIdle::TFormIdle(TComponent* Owner)
	: TComponent(Owner)
{
  if ( ComponentState.Contains( csDesigning ))
    return;

  if ( ! IdleList )
    {
    IdleList = new TList;
    HookHandle = (HHOOK)SetWindowsHookEx(
      WH_FOREGROUNDIDLE	,	// type of hook to install
      (HOOKPROC)ForegroundIdleProc,	// address of hook procedure
      HInstance,	// handle of application instance
      (DWORD)GetCurrentThreadId () 	// identity of thread to install hook for
      );
    }

  IdleList->Add ( this );
}
//---------------------------------------------------------------------------
__fastcall TFormIdle::~TFormIdle()
{
  if ( ComponentState.Contains( csDesigning ))
    return;

  IdleList->Remove ( this );

  if ( ! IdleList->Count )
    {
    UnhookWindowsHookEx ( HookHandle );
    delete IdleList;
    IdleList = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TFormIdle::DoIdle ()
{
  if ( FOnIdle )
    FOnIdle ( this );
}
//---------------------------------------------------------------------------
namespace Formidle
{
    void __fastcall PACKAGE Register()
	{
		TComponentClass classes[1] = {__classid(TFormIdle)};
		RegisterComponents("BMitov", classes, 0);
	}
}
//---------------------------------------------------------------------------
