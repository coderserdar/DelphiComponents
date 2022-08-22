//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "BMMessageHook.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TBMMessageHook *)
{
    new TBMMessageHook(NULL);
}
//---------------------------------------------------------------------------
HHOOK TBMMessageHook::WndProcHookHandle;
//HHOOK TBMMessageHook::WndRetProcHookHandle;
HHOOK TBMMessageHook::GetMsgProcHookHandle;
//---------------------------------------------------------------------------
TList  *TBMMessageHook::HooksList = NULL;
//---------------------------------------------------------------------------
bool    TBMMessageHook::CallWndBusy = false;
//---------------------------------------------------------------------------
LRESULT __stdcall TBMMessageHook::CallWndProc (
    int code,	// hook code
    WPARAM wParam,	// current-process flag
    LPARAM lParam 	// address of structure with message data
    )
{
  if ( ! CallWndBusy )
    if ( code == HC_ACTION )
      {
      CallWndBusy = true;
      if ( HooksList )
        for ( int i = 0; i < HooksList->Count; i ++ )
          ((TBMMessageHook *)HooksList->Items [ i ] )->DoCallWndProc ( wParam, lParam );

      CallWndBusy = false;
      }

  return CallNextHookEx( WndProcHookHandle, code, wParam, lParam );
}
//---------------------------------------------------------------------------
/*
LRESULT __stdcall TBMMessageHook::CallWndRetProc (
    int code,	// hook code
    WPARAM wParam,	// current-process flag
    LPARAM lParam 	// address of structure with message data
    )
{
  if ( ! CallWndBusy )
    if ( code == HC_ACTION )
      {
      CallWndBusy = true;
      if ( HooksList )
        for ( int i = 0; i < HooksList->Count; i ++ )
          ((TBMMessageHook *)HooksList->Items [ i ] )->DoCallWndRetProc ( wParam, lParam );

      CallWndBusy = false;
      }

  CallNextHookEx( WndRetProcHookHandle, code, wParam, lParam );
  return 0;
}
*/
//---------------------------------------------------------------------------
LRESULT __stdcall TBMMessageHook::GetMsgProc (
    int code,	// hook code
    WPARAM wParam,	// current-process flag
    LPARAM lParam 	// address of structure with message data
    )
{
  if ( ! CallWndBusy )
    if ( code == HC_ACTION )
      {
      CallWndBusy = true;
      if ( HooksList )
        for ( int i = 0; i < HooksList->Count; i ++ )
          ((TBMMessageHook *)HooksList->Items [ i ] )->DoGetMsgProc ( wParam, lParam );

      CallWndBusy = false;
      }

  return CallNextHookEx( GetMsgProcHookHandle, code, wParam, lParam );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMMessageHook::TBMMessageHook(TComponent* Owner)
    : TComponent (Owner)
{
  FOnSendMessage = NULL;
//  FOnAfterSendMessage = NULL;
  FOnGetPostedMessage = NULL;

  FOnAppSendMessage = NULL;
//  FOnAppAfterSendMessage = NULL;
  FOnAppGetPostedMessage = NULL;


  Owner->FreeNotification ( this );
  
  if ( ComponentState.Contains( csDesigning ))
    return;

  if ( ! HooksList )
    {
    HooksList = new TList;

    WndProcHookHandle = (HHOOK)SetWindowsHookEx(
      WH_CALLWNDPROC,	// type of hook to install
      (HOOKPROC)CallWndProc,	// address of hook procedure
      NULL, //HInstance,	// handle of application instance
      (DWORD)GetCurrentThreadId () 	// identity of thread to install hook for
      );

/*
    WndRetProcHookHandle = (HHOOK)SetWindowsHookEx(
      WH_CALLWNDPROCRET,	// type of hook to install
      (HOOKPROC)CallWndRetProc,	// address of hook procedure
      NULL, //HInstance,	// handle of application instance
      (DWORD)GetCurrentThreadId () 	// identity of thread to install hook for
      );
*/

    GetMsgProcHookHandle = (HHOOK)SetWindowsHookEx(
      WH_GETMESSAGE,	// type of hook to install
      (HOOKPROC)GetMsgProc,	// address of hook procedure
      NULL, //HInstance,	// handle of application instance
      (DWORD)GetCurrentThreadId () 	// identity of thread to install hook for
      );
    }

  HooksList->Add ( this );
}
//---------------------------------------------------------------------------
__fastcall TBMMessageHook::~TBMMessageHook()
{
  if ( ComponentState.Contains( csDesigning ))
    return;
               
  if ( ! HooksList )
    return;

  HooksList->Remove ( this );

  if ( ! HooksList->Count )
    {
    UnhookWindowsHookEx ( GetMsgProcHookHandle );
//    UnhookWindowsHookEx ( WndRetProcHookHandle );
    UnhookWindowsHookEx ( WndProcHookHandle );
    delete HooksList;
    HooksList = NULL;
    }
}
//---------------------------------------------------------------------------
namespace
{
  class TTmpComponent : public TComponent
  {
  public :
    void __fastcall Notification ( TComponent* AComponent, TOperation Operation );
  };
};
//---------------------------------------------------------------------------
void __fastcall TBMMessageHook::SetHookedTo ( TWinControl *HookedTo )
{
  if ( HookedTo == FHookedTo )
    return;

  if ( FHookedTo )
    ((TTmpComponent *)HookedTo )->Notification ( this, opRemove );

  FHookedTo = HookedTo;

  if ( FHookedTo )
    FHookedTo->FreeNotification ( this );

}
//---------------------------------------------------------------------------
void __fastcall TBMMessageHook::Notification(TComponent* AComponent, TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  if ( dynamic_cast<TWinControl *> ( AComponent ) && Operation == opRemove && AComponent == FHookedTo )
    {
//    ((TTmpComponent *)AComponent )->Notification ( this, opRemove );
    FHookedTo = NULL;
    }

}
//---------------------------------------------------------------------------
namespace
{
HWND GetOwnerHandle ( TComponent *Comp )
{
  if ( ! Comp )
    return NULL;

  TWinControl *WinControl = dynamic_cast<TWinControl *> ( Comp );
  if ( WinControl )
    if ( WinControl->HandleAllocated () )
      return WinControl->Handle;

    else
      return NULL;

  return GetOwnerHandle ( Comp->Owner );
}
};
//---------------------------------------------------------------------------
void __fastcall TBMMessageHook::DoCallWndProc ( WPARAM &wParam, LPARAM &lParam )
{
  if ( FOnSendMessage || FOnAppSendMessage )
    {
    CWPSTRUCT *pData = ( CWPSTRUCT * )lParam;
    if ( GetOwnerHandle ( Owner ) == pData->hwnd )
      {
      if ( pData->message == WM_DESTROY )
        {
//        HooksList->Remove ( this );
        return;
        }
      }

    TMessage Msg;
    Msg.Msg = pData->message;
    Msg.LParam = pData->lParam;
    Msg.WParam = pData->wParam;

    if ( FOnAppSendMessage )
      FOnAppSendMessage ( this, pData->hwnd, Msg );

    if ( FHookedTo )
      if ( FHookedTo->HandleAllocated () )
        if ( FHookedTo->Handle == pData->hwnd )
          if ( FOnSendMessage )
            FOnSendMessage ( this, Msg );

    pData->message = Msg.Msg;
    pData->lParam = Msg.LParam;
    pData->wParam = Msg.WParam;
    }
}
//---------------------------------------------------------------------------
/*
void __fastcall TBMMessageHook::DoCallWndRetProc ( WPARAM &wParam, LPARAM &lParam )
{
  if ( FOnAfterSendMessage || FOnAppAfterSendMessage )
    {
    CWPRETSTRUCT *pData = ( CWPRETSTRUCT * )lParam;
    TMessage Msg;
    Msg.Msg = pData->message;
    Msg.LParam = pData->lParam;
    Msg.WParam = pData->wParam;

    if ( FOnAppAfterSendMessage )
      FOnAppAfterSendMessage ( this, pData->hwnd, Msg );

    if ( FHookedTo )
      if ( FHookedTo->HandleAllocated () )
        if ( FHookedTo->Handle == pData->hwnd )
          if ( FOnAfterSendMessage )
            FOnAfterSendMessage ( this, Msg );

    pData->message = Msg.Msg;
    pData->lParam = Msg.LParam;
    pData->wParam = Msg.WParam;
    }

}
*/
//---------------------------------------------------------------------------
void __fastcall TBMMessageHook::DoGetMsgProc ( WPARAM &wParam, LPARAM &lParam )
{
  if ( FOnGetPostedMessage || FOnAppGetPostedMessage )
    {
    MSG *pData = ( MSG * )lParam;
    TMessage Msg;
    Msg.Msg = pData->message;
    Msg.LParam = pData->lParam;
    Msg.WParam = pData->wParam;

    if ( FOnAppGetPostedMessage )
      FOnAppGetPostedMessage ( this, pData->hwnd, Msg );

    if ( FHookedTo )
      if ( FHookedTo->HandleAllocated () )
        if ( FHookedTo->Handle == pData->hwnd )
          if ( FOnGetPostedMessage )
            FOnGetPostedMessage ( this, Msg );

    pData->message = Msg.Msg;
    pData->lParam = Msg.LParam;
    pData->wParam = Msg.WParam;
    }

}
//---------------------------------------------------------------------------

