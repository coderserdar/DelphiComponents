/*> Ver: V3.2 ************************************************************\

**> History :

Beta V1.0       07/08/1998  Released
Beta V1.1       01/12/1999  Tab colorization's been implemented.
                                Image change on hot tracking's been added.
                                Images graying, highlighting, etc.

Beta V1.2       01/26/1999  Now uses DIB's and is much faster.
                                Another windows bug has been fixed.

Beta V1.3       02/24/1999      Raised, Inserted and Shadowed text and bitmaps
                                are available for the highlighted tab. Cloud
                                on the highlighted tab is available.

Beta V1.4       02/24/1999      Transparency has been fixed for some of all videocards !
Beta V1.5       02/25/1999      Flashing capabilities has been added.
Beta V1.6       03/05/1999      The DIBs are temporary out becouse of an error in BCB.
Beta V1.7       03/12/1999      Fixed DIBs support. Sound support added !
Beta V1.8       03/15/1999      Enhanced sound support.
Beta V1.9       03/20/1999      a resource leak has been fixed.
Beta V2.0       03/30/1999      Extended wave support by BMWaveComponent.
Beta V2.1       04/10/1999      BCB 4.0 support.
Beta V2.2       04/23/1999      Bitmaps on each sheet. Color property.
                                BCB 4.0 Buttons formats support.

Beta V2.3       04/28/1999      BCB 4.0 Left & Right tabs support.
Beta V2.4       05/24/1999      Transparency support.
Beta V2.5       05/25/1999      Fixed BCB 3.0 compiler error.
Beta V2.6       06/01/1999      Fixed bug in dynamic bitmap changing.
                                Implementing its own synchronization.

Beta V2.7       06/07/1999      Fixed transparency bug.
Beta V2.8       06/16/1999      Fixed design time page update bug in BCB 3.0 .
                                Fixed bug with disapearing components when 2
                                BMPages are one over another and the second is
                                transparent.

Beta V2.9       06/18/1999      Extended transparency level added.
Beta V3.0b1     07/07/1999      Various transparency fixes.
Beta V3.0b2     07/04/2000      Fixed bug with invisible tabs.
Beta V3.0b3     02/21/2001      Fixed a tab font related bug.
V3.1            12/06/2004      Added C++ Builder 6 support.
V3.2            12/07/2008      Added C++ Builder 2006, 2007, and 2009 support.

**> Legal issues :

              Copyright (C) 1998, 2008 by Boian Mitov
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
#ifndef BMPAGEH
#define BMPAGEH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ComCtrls.hpp>

//----------------------------------------------------------------------------
#include <vcl\CommCtrl.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Messages.hpp>
#include <vcl\Windows.hpp>
#include <vcl\System.hpp>
#include "BMWave.h"

#if (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif

#define BM_BASE  0xE000
#define BM_CHANGE_VIEW  ( BM_BASE + 0 )

namespace Bmpage
{
class TBMPageControlEditor;
//-- type declarations -------------------------------------------------------
enum TBMTabStyle  { tabRightJustify, tabCenterText, tabLabelLeft, tabOwnerDraw };
enum TDrawStyle { dsNormal, dsRaised, dsInset, dsShadow };
enum TBackgroundStyle  { bgsStretched, bgsTailed };
typedef Set<TBMTabStyle, tabRightJustify, tabOwnerDraw>  TTabStyles;
#ifndef __BCB_40__
    enum TTabStyle { tsTabs, tsButtons };
    enum TBMPatchValues { tpLeft=20, tpRight=21 };
#endif

class TBMTabSheet;

class PACKAGE TBMPageControl : public Comctrls::TPageControl
{
  typedef Comctrls::TPageControl inherited;

public :
  enum TTabColorization { tcNone, tcPage, tcActive, tcActivePage };
  enum TCloudColorization { ccNone, ccPage, ccMain };

protected:
    bool                FShowGlyphs;
    bool                FGrayInactive;
    bool                FHighlightGlyphts;
    bool                FFlashing;
    bool                FEnableSounds;
    int                 FFlashedDelay;
    int                 FUnFlashedDelay;
    int                 FFlashingDelay;

#ifndef __BCB_40__
    TTabStyle FStyle;
#endif

    TTabStyles          FTabStyles;
    TTabColorization    FTabColorization;
    TCloudColorization  FCloudColorization;

    TColor              FColorUnselected;
    TColor              FColorSelected;
    TColor              FHotTrackColor;
    TColor              FHotTrackCloudColor;

    TDrawStyle          FHotTrackTextStyle;
    TDrawStyle          FHotTrackGlyphtStyle;

    bool                FTransparent;

    Bmwave::TBMCustomSound      *FHighlightSound;
    Bmwave::TBMCustomSound      *FPulseSound;
    Bmwave::TBMCustomSound      *FSelectSound;

protected:
    Controls::TControlCanvas *FCanvas;
    Controls::TImageList     *Glyphs;
    Controls::TImageList     *GrayedGlyphs;
    Controls::TImageList     *HighlightedGlyphs;
    Controls::TImageList     *SelectedHighlightedGlyphs;
    Stdctrls::TDrawItemEvent  FOnDrawItem;
    Graphics::TBitmap        *TabBmp;
    int                       FHighlitedItem;
    Graphics::TBitmap        *GrayedImage;
    Graphics::TBitmap        *HighlightedImage;

    Graphics::TBitmap        *MaskBmp;
    Graphics::TBitmap        *MaskBmpBlack;
    Graphics::TBitmap        *ImageBmp;

    Graphics::TBitmap        *OffsetBitmap;

    Graphics::TBitmap        *FParentBackgroundBuffer;
    bool                      PackGroundUpdated;

    int                       FCloudSize;
    bool                      FEnableFlash;
    int                       Delay;
    bool                      AlreadyPlayngEntryFlag;
    bool                      FMouseInside;

    TColorRef *DIBArray;
    int        ImageSize;

protected:
    TBMTabSheet* __fastcall GetPage(int Index);

    void __fastcall SetGlyphs               ( Controls::TImageList* Value );
    void __fastcall SetTabStyles            ( TTabStyles Value );
    void __fastcall SetShowGlyphs           ( bool Value );
    void __fastcall SetTabColorization      ( TTabColorization _TabColorization );
    void __fastcall SetCloudColorization    ( TCloudColorization _CloudColorization );
    void __fastcall SetColorUnselected      ( TColor _Color );
    void __fastcall SetColorSelected        ( TColor _Color );
    void __fastcall SetHotTrackColor        ( TColor _Color );
    void __fastcall SetHotTrackCloudColor   ( TColor _Color );
    void __fastcall SetGrayInactive         ( bool Value );
    void __fastcall SetHighlightGlyphts     ( bool Value );
    void __fastcall SetFlashing             ( bool Value );
    void __fastcall SetHotTrackTextStyle    ( TDrawStyle _Style );
    void __fastcall SetHotTrackGlyphtStyle  ( TDrawStyle _Style );
    void __fastcall SetUnFlashedDelay       ( int Value );
    void __fastcall SetFlashingDelay        ( int Value );
    void __fastcall SetFlashedDelay         ( int Value );
    void __fastcall SetHighlightSound       ( Bmwave::TBMCustomSound *Value );
    void __fastcall SetPulseSound           ( Bmwave::TBMCustomSound *Value );
    void __fastcall SetSelectSound          ( Bmwave::TBMCustomSound *Value );
    void __fastcall SetTransparent          ( bool Value );

#ifndef __BCB_40__
    void __fastcall SetStyle ( TTabStyle Value );
#endif

    MESSAGE void __fastcall CNDrawItem      ( Messages::TWMDrawItem &Msg );
    MESSAGE void __fastcall WMSize          ( Messages::TMessage    &Msg );
    MESSAGE void __fastcall WMMove          ( Messages::TMessage    &Msg );

    MESSAGE void __fastcall WMMouseMove     ( Messages::TMessage    &Msg );
    MESSAGE void __fastcall WMPaint         ( Messages::TMessage    &Msg );

    MESSAGE void __fastcall CMMouseEnter    ( Messages::TMessage    &Msg );
    MESSAGE void __fastcall CMMouseLeave    ( Messages::TMessage    &Msg );
    MESSAGE void __fastcall CMColorChanged  ( Messages::TMessage    &Msg );

    MESSAGE void __fastcall WMVHScroll      ( Messages::TMessage    &Msg );

    MESSAGE void __fastcall BMChangeView    ( Messages::TMessage    &Msg );

protected:
    virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
    virtual void __fastcall UpdateGlyphs(void);
    virtual void __fastcall CreateWnd(void);
    virtual void __fastcall DrawItem(int TabIndex, int PageIndex, const TRect &Rect, Windows::TOwnerDrawState State );

    virtual void __fastcall DestroyWnd(void);
    virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation );

    void __fastcall ViewChange ();

    int  __fastcall GetPageCount ();

    void __fastcall WMEraseBkgnd ( TMessage& Msg );

    void __fastcall InvalidateBorder();
    void __fastcall DrawGlyphtShadow( int ShadowOffset, int XImagePos, int YImagePos );
    DYNAMIC void __fastcall Change(void);
    DYNAMIC bool __fastcall CanChange(void);

    bool __fastcall HasTransparentcy ();
    int __fastcall ConvertTabToPageIndex ( int Index );
    int __fastcall GetCurSelection ();
    int __fastcall GetCurFocus ();

//    virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);

public:
    void __fastcall UpdateHighlighted ();
    void __fastcall PlayPulseWave();
    void __fastcall PlayHighlightWave();
    void __fastcall PlaySelectWave();
    bool __fastcall HaveHighlightWave ();
    bool __fastcall HaveSelectWave ();
    void __fastcall ValidateParentImage ();
    virtual void __fastcall Invalidate(void);
#ifdef __BCB_40__
    void __fastcall ValidateBtnRect ( TRect TmpRect );
#endif

public:
    __property TBMTabSheet* Pages[int Index] = { read=GetPage };

__published:

#ifndef __BCB_40__
    __property TTabStyle Style = {read=FStyle, write=SetStyle, default = tsTabs};
#endif

    __property TTabStyles Styles                        = { read=FTabStyles, write=SetTabStyles, default=0 };
    __property Stdctrls::TDrawItemEvent OnDrawItem      = { read=FOnDrawItem, write=FOnDrawItem };
    __property bool             ShowGlyphs              = { read=FShowGlyphs, write=SetShowGlyphs, default=true };
    __property TTabColorization TabColorization         = { read=FTabColorization, write=SetTabColorization, default=tcPage };
    __property TCloudColorization  CloudColorization    = { read=FCloudColorization, write=SetCloudColorization, default=ccMain };
    __property TColor ColorUnselected                   = { read=FColorUnselected, write=SetColorUnselected, default=clBtnShadow };
    __property TColor ColorSelected                     = { read=FColorSelected, write=SetColorSelected, default=clBtnFace };
    __property TColor HotTrackColor                     = { read=FHotTrackColor, write=SetHotTrackColor, default=clHighlight };
    __property TColor HotTrackCloudColor                = { read=FHotTrackCloudColor, write=SetHotTrackCloudColor, default=clYellow };
    __property bool   GrayInactive                      = { read=FGrayInactive, write=SetGrayInactive, default=true };
    __property bool   HighlightGlyphts                  = { read=FHighlightGlyphts, write=SetHighlightGlyphts, default=true };
    __property bool   Flashing                          = { read=FFlashing, write=SetFlashing, default=true };
    __property int    FlashedDelay                      = { read=FFlashedDelay, write=SetFlashedDelay, default= 6 };
    __property int    FlashingDelay                     = { read=FFlashingDelay, write=SetFlashingDelay, default= 6 };
    __property int    UnFlashedDelay                    = { read=FUnFlashedDelay, write=SetUnFlashedDelay, default= 200 };
    __property bool   EnableSounds                      = { read=FEnableSounds, write=FEnableSounds, default= true };
    __property bool   Transparent                       = { read=FTransparent, write=SetTransparent, default = false };

    __property Bmwave::TBMCustomSound *HighlightSound   = { read=FHighlightSound, write=SetHighlightSound };
    __property Bmwave::TBMCustomSound *PulseSound       = { read=FPulseSound, write=SetPulseSound };
    __property Bmwave::TBMCustomSound *SelectSound      = { read=FSelectSound, write=SetSelectSound };

    __property TDrawStyle       HotTrackTextStyle       = { read=FHotTrackTextStyle, write=SetHotTrackTextStyle, default=dsRaised };
    __property TDrawStyle       HotTrackGlyphtStyle     = {read=FHotTrackGlyphtStyle, write=SetHotTrackGlyphtStyle, default=dsRaised };
    
    __property Color;
    __property ParentColor; 


public:
    __fastcall TBMPageControl(HWND ParentWindow);
    __fastcall virtual  TBMPageControl(Classes::TComponent* AOwner);
    __fastcall virtual ~TBMPageControl(void);

  BEGIN_MESSAGE_MAP
    VCL_MESSAGE_HANDLER ( CN_DRAWITEM,     TWMDrawItem,   CNDrawItem     )
    VCL_MESSAGE_HANDLER ( WM_PAINT,        TMessage,      WMPaint        )
    VCL_MESSAGE_HANDLER ( WM_SIZE,         TMessage,      WMSize         )
    VCL_MESSAGE_HANDLER ( WM_MOVE,         TMessage,      WMMove         )
    VCL_MESSAGE_HANDLER ( WM_ERASEBKGND,   TMessage,      WMEraseBkgnd   )
    VCL_MESSAGE_HANDLER ( CM_MOUSEENTER,   TMessage,      CMMouseEnter   )
    VCL_MESSAGE_HANDLER ( CM_MOUSELEAVE,   TMessage,      CMMouseLeave   )
    VCL_MESSAGE_HANDLER ( CM_COLORCHANGED, TMessage,      CMColorChanged )
    VCL_MESSAGE_HANDLER ( WM_MOUSEMOVE,    TMessage,      WMMouseMove    )
    VCL_MESSAGE_HANDLER ( WM_HSCROLL,      TMessage,      WMVHScroll     )
    VCL_MESSAGE_HANDLER ( WM_VSCROLL,      TMessage,      WMVHScroll     )
    VCL_MESSAGE_HANDLER ( BM_CHANGE_VIEW,  TMessage,      BMChangeView   )
  END_MESSAGE_MAP(inherited);

  friend class  TBMPageControlEditor;
  friend class  TBMTabSheet;
};

class PACKAGE TBMTabSheet : public Comctrls::TTabSheet
{
  typedef Comctrls::TTabSheet inherited;

  Controls::TControlCanvas* FCanvas;
  Graphics::TBitmap *FGlyph;
  Graphics::TBitmap *RealGlyph;

  Graphics::TBitmap *FGlyphUnselected;
  Graphics::TBitmap *RealGlyphUnselected;

  Graphics::TBitmap *FGlyphHighlighted;
  Graphics::TBitmap *RealGlyphHighlighted;

  Graphics::TBitmap *FGlyphSelectedHighlighted;
  Graphics::TBitmap *RealGlyphSelectedHighlighted;
  TColor             FHotTrackCloudColor;

  Graphics::TBitmap *FBackgroundBitmap;

  TBackgroundStyle   FBackgroundStyle;      

  Graphics::TBitmap *FBackgroundBuffer;

  bool               FTransparent;

  int                FTransparentLevel;

  Bmwave::TBMCustomSound      *FHighlightSound;
  Bmwave::TBMCustomSound      *FPulseSound;
  Bmwave::TBMCustomSound      *FSelectSound;

public:
  __fastcall virtual TBMTabSheet(Classes::TComponent* AOwner);
  __fastcall virtual ~TBMTabSheet(void);

protected :
  void __fastcall TheChangeGlypht ( Graphics::TBitmap *FGlyph, Graphics::TBitmap *RealGlyph );
  void __fastcall ChangeGlypht ( System::TObject* Sender );
  void __fastcall ChangeGlyphUnselectedt ( System::TObject* Sender );
  void __fastcall ChangeGlyphHighlightedt ( System::TObject* Sender );
  void __fastcall ChangeGlyphSelectedHighlightedt ( System::TObject* Sender );
  void __fastcall SetGlyph ( Graphics::TBitmap *AGlyph );
  void __fastcall SetGlyphUnselected ( Graphics::TBitmap *AGlyph );
  void __fastcall SetGlyphHighlighted ( Graphics::TBitmap *AGlyph );
  void __fastcall SetGlyphSelectedHighlighted ( Graphics::TBitmap *AGlyph );
  void __fastcall SetBackgroundBitmap ( Graphics::TBitmap *AGlyph );
  void __fastcall SetBackgroundStyle ( TBackgroundStyle Value );
  void __fastcall SetColor ( TColor AColor );
  void __fastcall SetHotTrackCloudColor   ( TColor _Color );
  void __fastcall SetHighlightSound       ( Bmwave::TBMCustomSound *Value );
  void __fastcall SetPulseSound           ( Bmwave::TBMCustomSound *Value );
  void __fastcall SetSelectSound          ( Bmwave::TBMCustomSound *Value );
  void __fastcall SetTransparent          ( bool Value );
  void __fastcall SetTransparentLevel     ( int Value );

  void __fastcall ImageChange ( System::TObject* Sender );

  MESSAGE void __fastcall WMEraseBkgnd ( TMessage& Msg );
  MESSAGE void __fastcall WMPaint      ( TMessage& Msg );
  MESSAGE void __fastcall WMSize( TMessage& Msg );
  MESSAGE void __fastcall WMMove( TMessage& Msg );
  MESSAGE void __fastcall CMColorChanged   ( TMessage& Msg );
  MESSAGE void __fastcall BMChangeView    ( Messages::TMessage    &Msg );

  void __fastcall InvalidateFull ();
  void __fastcall ProcessTransparentLevel ();
  void __fastcall ViewChange ();

  void __fastcall Init ();
  Graphics::TBitmap *__fastcall RefreshBuffer ();
  void __fastcall InvalidateParentBorder ();

  virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation );

public:
  __fastcall TBMTabSheet(HWND ParentWindow);

  friend class  TBMPageControl;

public:
  void __fastcall DrawBkg ( HDC hDC );
  
__published:
  __property Graphics::TBitmap *Glyph = {read=FGlyph, write=SetGlyph, nodefault};
  __property Graphics::TBitmap *GlyphUnselected = {read=FGlyphUnselected, write=SetGlyphUnselected, nodefault};
  __property Graphics::TBitmap *GlyphHighlighted = {read=FGlyphHighlighted, write=SetGlyphHighlighted, nodefault};
  __property Graphics::TBitmap *GlyphSelectedHighlighted = {read=FGlyphSelectedHighlighted, write=SetGlyphSelectedHighlighted, nodefault};
  __property Graphics::TBitmap *BackgroundBitmap = {read=FBackgroundBitmap, write=SetBackgroundBitmap, nodefault};
  __property TColor HotTrackCloudColor  = {read=FHotTrackCloudColor, write=SetHotTrackCloudColor, default = clYellow};
  __property Bmwave::TBMCustomSound *HighlightSound   = { read=FHighlightSound, write=SetHighlightSound };
  __property Bmwave::TBMCustomSound *PulseSound       = { read=FPulseSound, write=SetPulseSound };
  __property Bmwave::TBMCustomSound *SelectSound      = { read=FSelectSound, write=SetSelectSound };
  __property TBackgroundStyle BackgroundStyle         = { read=FBackgroundStyle, write=SetBackgroundStyle, default = bgsStretched };
  __property bool   Transparent                       = { read=FTransparent, write=SetTransparent, default = false };
  __property int    TransparentLevel                  = { read=FTransparentLevel, write=SetTransparentLevel, default = 100 };

  __property Color;
  __property ParentColor;

  BEGIN_MESSAGE_MAP
    VCL_MESSAGE_HANDLER ( WM_ERASEBKGND,   TMessage, WMEraseBkgnd   )
    VCL_MESSAGE_HANDLER ( WM_PAINT,        TMessage, WMPaint        )
    VCL_MESSAGE_HANDLER ( WM_SIZE,         TMessage, WMSize         )
    VCL_MESSAGE_HANDLER ( WM_MOVE,         TMessage, WMMove         )
    VCL_MESSAGE_HANDLER ( CM_COLORCHANGED, TMessage, CMColorChanged )
    VCL_MESSAGE_HANDLER ( BM_CHANGE_VIEW,  TMessage, BMChangeView   )
  END_MESSAGE_MAP(inherited);
};
//-- var, const, procedure ---------------------------------------------------
extern void __fastcall Register(void);
//-- template instantiations -------------------------------------------------
//template class TTabStyles ;

}  /* namespace Bmpage */


#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmpage;
#endif
//-- end unit ----------------------------------------------------------------
#endif
