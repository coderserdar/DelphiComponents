/* Ver: V1.2 *************      History      *****************************\

Beta V1.0b0		10/22/1998		Released
Beta V1.0b1		04/15/1999		BCB 4.0 Support.
     V1.1		06/01/2003		BCB 6.0 Support.
     V1.2		03/16/2008		Addes C++ Builder 2006 and C++ Builder 2007 support.

Legal issues: Copyright (C) 1997, 2008 by Boian Mitov
              <mitov@mitov.com>
              <http://www.mitov.com>
              <http://www.openwire.org>

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
#ifndef BMMessageHookH
#define BMMessageHookH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__

#elif (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
#else
  typedef TFormDesigner * _di_IFormDesigner;
#endif
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TMessageNotifyEvent)( System::TObject* Sender, TMessage &Message );
typedef void __fastcall (__closure *TAppMessageNotifyEvent)( System::TObject* Sender, HWND WindowHandle, TMessage &Message );
//---------------------------------------------------------------------------
class PACKAGE TBMMessageHook : public TComponent
{
  typedef TComponent inherited;

private:
protected:
  static HHOOK WndProcHookHandle;
//  static HHOOK WndRetProcHookHandle;
  static HHOOK GetMsgProcHookHandle;

  static bool  CallWndBusy;

  static LRESULT __stdcall CallWndProc ( int code, WPARAM wParam, LPARAM lParam );
//  static LRESULT __stdcall CallWndRetProc ( int code, WPARAM wParam, LPARAM lParam );
  static LRESULT __stdcall GetMsgProc ( int code, WPARAM wParam, LPARAM lParam );

protected:
  static TList  *HooksList;

protected:
  TWinControl            *FHookedTo;

  TMessageNotifyEvent     FOnSendMessage;
//  TMessageNotifyEvent     FOnAfterSendMessage;
  TMessageNotifyEvent     FOnGetPostedMessage;

  TAppMessageNotifyEvent     FOnAppSendMessage;
//  TAppMessageNotifyEvent     FOnAppAfterSendMessage;
  TAppMessageNotifyEvent     FOnAppGetPostedMessage;

protected:
  void __fastcall DoCallWndProc ( WPARAM &wParam, LPARAM &lParam );
//  void __fastcall DoCallWndRetProc ( WPARAM &wParam, LPARAM &lParam );
  void __fastcall DoGetMsgProc ( WPARAM &wParam, LPARAM &lParam );


protected:
  void __fastcall SetHookedTo ( TWinControl *HookedTo );

protected:
  virtual void __fastcall Notification( TComponent* AComponent, TOperation Operation );

public:
    __fastcall TBMMessageHook(TComponent* Owner);
    __fastcall ~TBMMessageHook();

public:
  __property TAppMessageNotifyEvent  OnAppSendMessage = { read=FOnAppSendMessage, write=FOnAppSendMessage };
//  __property TAppMessageNotifyEvent  OnAppAfterSendMessage = { read=FOnAppAfterSendMessage, write=FOnAppAfterSendMessage };
  __property TAppMessageNotifyEvent  OnAppGetPostedMessage = { read=FOnAppGetPostedMessage, write=FOnAppGetPostedMessage };

__published:
  __property TWinControl            *HookedTo = { read=FHookedTo, write=SetHookedTo };
  __property TMessageNotifyEvent     OnSendMessage = { read=FOnSendMessage, write=FOnSendMessage };
//  __property TMessageNotifyEvent     OnAfterSendMessage = { read=FOnAfterSendMessage, write=FOnAfterSendMessage };
  __property TMessageNotifyEvent     OnGetPostedMessage = { read=FOnGetPostedMessage, write=FOnGetPostedMessage };

};
//---------------------------------------------------------------------------
#endif
