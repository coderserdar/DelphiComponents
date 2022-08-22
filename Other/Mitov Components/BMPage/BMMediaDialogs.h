/*> Ver: V1.5 ****************      History      ***************************\

Beta V1.0b1     04/20/1999      Released
Beta V1.0b2     04/21/1999      BCB 3.0 Suuport.
Beta V1.0b3     05/19/1999      Missing .hpp file has been added.
V1.4            12/06/2004      BCB 6.0 Suuport.
V1.5            11/22/2008      RAD 2007 support.

Legal issues: Copyright (C) 1998, 2008 by Boian Mitov
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
#ifndef BMMediaDialogsH
#define BMMediaDialogsH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <MPlayer.hpp>
//#include <dsgnintf.hpp>
//---------------------------------------------------------------------------
#if ( __BORLANDC__ >= 0x0570 )
  #define _USE_VISTA_DIALOGS_
#endif
//---------------------------------------------------------------------------
#if (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif
namespace Bmmediadialogs
{
class PACKAGE TBMMediaDialogImpl : public Classes::TComponent
{
  typedef TComponent inherited;

  friend class TBMOpenMediaDialog;
  friend class TBMSaveMediaDialog;

private:
    TBMOpenMediaDialog      *OwnerDialog;

    Extctrls::TPanel        *FPicturePanel;
    Stdctrls::TLabel        *FPictureLabel;
    Buttons::TSpeedButton   *FPreviewButton;
    Extctrls::TPanel        *FPaintPanel;
    Mplayer::TMediaPlayer   *FPlayer;
    Extctrls::TTimer        *FTimer;
    Extctrls::TTimer        *FUpdateTimer;

    Extctrls::TPanel        *FCommandPanel;
    Stdctrls::TCheckBox     *FLoopCheckBox;
    Stdctrls::TCheckBox     *FShowCheckBox;

    String                  FTmpFile;
    String                  CurrentFileName;
    TRect                   OriginalDisplayRect;

    bool                    InPreview;
    bool                    IsValidView;
    bool                    IsVisible;

    TRect   CalculatedRect;

    TRect   CurrentRect;
    TForm *PreviewForm;

    void __fastcall PreviewClick(System::TObject* Sender);
    void __fastcall PreviewKeyPress(System::TObject* Sender, TCHAR &Key);

    void __fastcall MediaNotify (System::TObject* Sender);
    void __fastcall UpdateNotify (System::TObject* Sender);
        
    bool __fastcall ValidFile ( String FileName );

    void __fastcall OnResize (System::TObject* Sender);
    void __fastcall OnShowCheck (System::TObject* Sender);
    void __fastcall OnPreviewShow (System::TObject* Sender);

protected:
    void __fastcall DoFolderChange(void);
    void __fastcall DoClose(void);
    void __fastcall DoSelectionChange(void);
    void __fastcall DoShow(void);

    void __fastcall ExecuteEnter (void);
    void __fastcall ExecuteExit (void);

public :
        __fastcall virtual TBMMediaDialogImpl ( TOpenDialog* AOwner);
};
//---------------------------------------------------------------------------
class PACKAGE TBMOpenMediaDialog : public TOpenDialog
{
    typedef Dialogs::TOpenDialog inherited;

    friend class TBMMediaDialogImpl;

private:
    TBMMediaDialogImpl *FMediaDialogImpl;

private:
#ifdef _USE_VISTA_DIALOGS_
    bool    FUseVistaDialogs;
#endif

protected:
    DYNAMIC void __fastcall DoFolderChange(void);
    DYNAMIC void __fastcall DoClose(void);
    DYNAMIC void __fastcall DoSelectionChange(void);
    DYNAMIC void __fastcall DoShow(void);
    virtual void __fastcall WndProc(Messages::TMessage &Message);

protected:
    bool __fastcall GetAutoShow();
    void __fastcall SetAutoShow( bool Value );

    bool __fastcall GetLoop();
    void __fastcall SetLoop( bool Value );

public:
    __fastcall virtual TBMOpenMediaDialog(Classes::TComponent* AOwner);
    __fastcall virtual ~TBMOpenMediaDialog(void);

public:
#if (__BORLANDC__ >= 0x0590)
    virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
#endif
    inline bool __fastcall  Execute(void);

__published:
  __property bool AutoShow = { read=GetAutoShow, write=SetAutoShow, default=true };
  __property bool Loop = { read=GetLoop, write=SetLoop, default=true };
#ifdef _USE_VISTA_DIALOGS_
  __property bool UseVistaDialogs = { read=FUseVistaDialogs, write=FUseVistaDialogs, default=true };
#endif
};
//---------------------------------------------------------------------------
class PACKAGE TBMSaveMediaDialog : public TSaveDialog
{
    typedef Dialogs::TSaveDialog inherited;

    friend class TBMMediaDialogImpl;

private:
    TBMMediaDialogImpl *FMediaDialogImpl;

private:
#ifdef _USE_VISTA_DIALOGS_
    bool    FUseVistaDialogs;
#endif

protected:
    DYNAMIC void __fastcall DoFolderChange(void);
    DYNAMIC void __fastcall DoClose(void);
    DYNAMIC void __fastcall DoSelectionChange(void);
    DYNAMIC void __fastcall DoShow(void);

protected:
    bool __fastcall GetAutoShow();
    void __fastcall SetAutoShow( bool Value );

    bool __fastcall GetLoop();
    void __fastcall SetLoop( bool Value );

public:
    __fastcall virtual TBMSaveMediaDialog(Classes::TComponent* AOwner);
    __fastcall virtual ~TBMSaveMediaDialog(void);

public:
#if (__BORLANDC__ >= 0x0590)
    virtual bool __fastcall Execute(HWND ParentWnd)/* overload */;
#endif
    inline bool __fastcall  Execute(void);

__published:
  __property bool AutoShow = { read=GetAutoShow, write=SetAutoShow, default=true };
  __property bool Loop = { read=GetLoop, write=SetLoop, default=true };
#ifdef _USE_VISTA_DIALOGS_
  __property bool UseVistaDialogs = { read=FUseVistaDialogs, write=FUseVistaDialogs, default=true };
#endif
};
};
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmmediadialogs;
#endif
//---------------------------------------------------------------------------
#endif
