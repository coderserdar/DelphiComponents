/*> Ver: V1.7 *********      History      ***************************\

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

Legal issues: Copyright (C) 1998 - 2012 by Boian Mitov
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

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef BMSynchroMethodsH
#define BMSynchroMethodsH
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
#include <ExptIntf.hpp>
//---------------------------------------------------------------------------
#if (__BORLANDC__ >= 0x0650)
  #define __BCB_XE3__
#endif

#if (__BORLANDC__ >= 0x0640)
  #define __BCB_XE2__
#endif

#if (__BORLANDC__ >= 0x0580)
  #define __BCB_2007__
#endif

#if (__BORLANDC__ >= 0x0570)
  #define __BCB_60__
  #define __BCB_2006__
#endif

#if (__BORLANDC__ >= 0x0560)
  #define __BCB_60__
#endif

#if (__BORLANDC__ >= 0x0550)
  #define __BCB_50__
#endif

#if (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
#endif

#if (__BORLANDC__ >= 0x0700)
  #define __BCB_80__
#endif

#ifdef __BCB_60__
  #include <DesignIntf.hpp>
  #include <DesignEditors.hpp>
  typedef _di_IDesigner TAFormDesigner;

#else
  #include <dsgnintf.hpp>
  #ifdef __BCB_40__
    typedef _di_IFormDesigner TAFormDesigner;

  #else
    typedef TFormDesigner * TAFormDesigner;

  #endif
#endif

namespace Bmsynchromethods
{
//---------------------------------------------------------------------------
class TEventsEditorNotification;
class TSynchroMethodsForm : public TForm
{
__published:	// IDE-managed Components
        TStatusBar *StatusBar;
        TPageControl *PageControl;
        TTabSheet *ThreadSynchroTabSheet;
        TTabSheet *TabSheet2;
        TListView *ThreadEventsListView;
        TListView *StdEventsListView;
        TPanel *Panel1;
        TGroupBox *CreateGroupBox;
        TBitBtn *BtnNoDataEvent;
        TBitBtn *BtnDataEvent;
        TBitBtn *RenameButton1;
        TBitBtn *ShowButton1;
        TPanel *Panel2;
        TBitBtn *NewStdButton;
        TBitBtn *RenameButton2;
        TBitBtn *ShowButton2;
        TPanel *Panel3;
        TBitBtn *CopyCallButton1;
        TBitBtn *GenCallButton1;
        TPanel *Panel4;
        TBitBtn *CopyCallButton2;
        TBitBtn *GenCallButton2;
        TBitBtn *RefreshButton1;
        TBitBtn *RefreshButton2;
        TTimer *Timer1;
        void __fastcall BtnNoDataEventClick(TObject *Sender);
        void __fastcall BtnDataEventClick(TObject *Sender);
        void __fastcall BtnNewStdEventClick(TObject *Sender);
        void __fastcall ThreadEventsListViewDblClick(TObject *Sender);
        void __fastcall ThreadEventsListViewChange(TObject *Sender,
          TListItem *Item, TItemChange Change);
        void __fastcall RenameButton1Click(TObject *Sender);
        void __fastcall ThreadEventsListViewEdited(TObject *Sender,
          TListItem *Item, String &S);
        void __fastcall ThreadEventsListViewKeyPress(TObject *Sender, char &Key);
        void __fastcall GenCallButton1Click(TObject *Sender);
        void __fastcall PageControlChange(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall CopyCallButton1Click(TObject *Sender);
        void __fastcall RefreshButton1Click(TObject *Sender);
        void __fastcall Timer1Timer(TObject *Sender);
    void __fastcall FormResize(TObject *Sender);
	void __fastcall ThreadEventsListViewMouseUp(TObject *Sender,
		  TMouseButton Button, TShiftState Shift, int X, int Y);

private:	// User declarations
  TListView     *ActiveListView;
  TStringList   *ThreadEventsList;
  TStringList   *StdEventsList;

  TEventsEditorNotification  *UpdateNotifier;

  TAFormDesigner Designer;
#ifdef __BCB_2007__
  int IDENotifierID;
#endif

public:		// User declarations

private:	// User declarations
  void __fastcall AddMethodNoDataEntry ( const String S );
  void __fastcall AddMethodDataEntry ( const String S );
  void __fastcall AddStandardNotifyEntry ( const String S );
  TListItem * __fastcall AddMethodNoDataItem ( const String S );
  TListItem * __fastcall AddMethodDataItem ( const String S );
  TListItem * __fastcall AddStandardNotifyItem ( const String S );
  void __fastcall ValidateComponents ();
  void __fastcall ClearLists ();

#ifdef __BCB_2007__
  void __fastcall OnUpdateNotify( TObject *Sender );
  void __fastcall OnCloseNotify( TObject *Sender );
#else
  void __fastcall OnFileNotify( TFileNotification NotifyCode, const String FileName, bool &Cancel );
  void __fastcall OnEventNotify( TEventNotification NotifyCode, bool &Cancel );
#endif

public:		// User declarations
  __fastcall TSynchroMethodsForm(TComponent* Owner, TAFormDesigner _Designer );
  __fastcall ~TSynchroMethodsForm();

public:		// User declarations
  void __fastcall UpdateView ();
  void __fastcall SetDesigner ( TAFormDesigner Designer );
};
};
//---------------------------------------------------------------------------
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmsynchromethods;
#endif
//---------------------------------------------------------------------------
#endif

