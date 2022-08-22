/*> Ver: V2.2 **************************************************************\

**> History :

Beta V1.0       03/20/1999      Released
Beta V1.1       03/22/1999      Release mouse button bug fixed.
Beta V1.2       03/24/1999      Extended wave support by BMWaveComponent.
Beta V1.3       03/30/1999      Header files fix.
Beta V1.4       04/15/1999      BCB 4.0 Support.
Beta V1.5       05/19/1999      Small fix for BCB 3.0.
Beta V1.6       05/21/1999      Improved update on transparency.
Beta V1.7       05/21/1999      Centered glyph added by Eric DUPONT.
                                Implementing its own synchronization.
                                
Beta V1.8       06/18/1999      Improved transparency.
Beta V1.9       07/08/1999      BM_CHANGE_VIEW added.
V2.0            12/06/2004      BCB 6.0 Support.
V2.1            11/30/2008      BCB 2006, 2007 and 2009 Support.
V2.2            03/03/2010      BCB 2010 Support.


**> Legal issues :

              Copyright (C) 1998, 2010 by Boian Mitov
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


****************************************************************************
Modifications by Eric DUPONT (roderic@roderic.com) marked as follow :
//---> Begin ED
// End ED
****************************************************************************

\***************************************************************************/
//---------------------------------------------------------------------------
#ifndef BMShapedButtonH
#define BMShapedButtonH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#if ( __BORLANDC__ >= 0x0560 )
    #include <DesignEditors.hpp>
#else
    #include <dsgnintf.hpp>
#endif

#include <Buttons.hpp>
//---------------------------------------------------------------------------
//---> Begin ED
#define _USE_BMWAVE_
//---------------------------------------------------------------------------
#ifdef _USE_BMWAVE_
  #include "BMWave.h"
#endif
#include "CarteMsg.h"
// End ED
//---------------------------------------------------------------------------

#define BM_BASE  0xE000
#define BM_CHANGE_VIEW  ( BM_BASE + 0 )

#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
  #define max std::max

#elif (__BORLANDC__ >= 0x0540)
  #define __BCB_40__
  #define __DRAW_TRUE__ , true
#else
  #define __DRAW_TRUE__
  typedef TFormDesigner * _di_IFormDesigner;
#endif

namespace Bmshapedbutton
{
enum TButtonDisplayMode { dmBoth, dmGlyphOnly, dmTextOnly };
enum TBMButtonState { bsUp, bsDisabled, bsDown, bsExclusive, bsMouseInExclusive, bsMouseInDown, bsMouseIn };
enum TBMCloudType { ctNone, ctSmall, ctLarge };

//---> Begin ED
enum TButtonLayoutPlus { blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom, blGlyphCentre };
// End ED

typedef short int TBMNumGlyphs;

class TButtonGlyph;

class PACKAGE TBMShapedButton : public TCustomControl
{
      typedef Controls::TCustomControl inherited;

private:
      Classes::TNotifyEvent FOnMouseEnter;
      Classes::TNotifyEvent FOnMouseExit;
      bool FAllowAllUp;
      TAlignment FAlignment;
      bool FCancel;
      bool FDefault;

      TColor              FHotTrackColor;
      bool                FFlashing;
      int                 FFlashedDelay;
      int                 FUnFlashedDelay;
      int                 FFlashingDelay;

      TColor              FCloudColor;

//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
      Bmwave::TBMCustomSound      *FHighlightSound;
      Bmwave::TBMCustomSound      *FPulseSound;
      Bmwave::TBMCustomSound      *FSelectSound;
//---> Begin ED
#endif
// End ED

      TBMCloudType        FCloudType;
      bool                FEnableFlash;
      int                 FCloudSize;

      bool                AlreadyPlayngEntryFlag;

//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
      bool                FEnableSounds;
//---> Begin ED
#endif
// End ED

      TColorRef          *FCloudArray;
      TColorRef          *FCloudArray1;

      int FReliefSize;

      int                 Delay;

      TButtonDisplayMode FDisplayMode;
      bool FDown;
      bool FDropdownArrow;
      bool FDropdownCombo;
      Menus::TPopupMenu* FDropdownMenu;
      bool FFlat;
      TButtonGlyph *FGlyph;
      int FGroupIndex;
      Classes::THelpContext FHelpContext;
//---> Begin ED
      //Buttons::TButtonLayout FLayout;
      TButtonLayoutPlus FLayout;
// End ED
      int FMargin;
      TModalResult FModalResult;
      bool FBorder;
      bool FOldDisabledStyle;
      bool FTransparent;
      bool FRepeating;
      int FRepeatDelay;
      int FRepeatInterval;
      bool FShowBorderWhenInactive;
      int FSpacing;
      bool FWordWrap;
      bool FInClick;
      bool FMouseInControl;
      bool FMouseIsDown;
      bool FMenuIsDown;
      bool FUsesDropdown;
      Extctrls::TTimer* FRepeatTimer;
      void __fastcall GlyphChanged(System::TObject* Sender);
      void __fastcall UpdateExclusive(void);
      void __fastcall SetAlignment(Classes::TAlignment Value);
      void __fastcall SetAllowAllUp(bool Value);
      bool __fastcall GetCallDormant(void);
      void __fastcall SetCallDormant(bool Value);
      void __fastcall SetDown(bool Value);
      void __fastcall SetDisplayMode(TButtonDisplayMode Value);
      void __fastcall SetDropdownArrow(bool Value);
      void __fastcall SetDropdownCombo(bool Value);
      void __fastcall SetDropdownMenu(Menus::TPopupMenu* Value);
      void __fastcall SetFlat(bool Value);
//    void __fastcall SetEnableCloud(bool Value);
      void __fastcall SetCloudType ( TBMCloudType Value );

      void __fastcall SetFlashing             ( bool Value );
      void __fastcall SetUnFlashedDelay       ( int Value );
      void __fastcall SetFlashingDelay        ( int Value );
      void __fastcall SetFlashedDelay         ( int Value );

      Graphics::TBitmap* __fastcall GetGlyph(void);
      void __fastcall SetGlyph(Graphics::TBitmap* Value);
//	Graphics::TBitmap* __fastcall GetGlyphMask(void);
//	void __fastcall SetGlyphMask(Graphics::TBitmap* Value);
      void __fastcall SetGroupIndex(int Value);
      int __fastcall GetImageIndex(void);
      void __fastcall SetImageIndex(int Value);
      TCustomImageList* __fastcall GetImages(void);
      void __fastcall SetImages(TCustomImageList* Value);
//---> Begin ED
      //void __fastcall SetLayout(Buttons::TButtonLayout Value);
      void __fastcall SetLayout(TButtonLayoutPlus Value);
// End ED
      void __fastcall SetMargin(int Value);
      void __fastcall SetBorder(bool Value);
      TBMNumGlyphs __fastcall GetNumGlyphs(void);
      void __fastcall SetNumGlyphs(TBMNumGlyphs Value);
      void __fastcall SetOldDisabledStyle(bool Value);
      void __fastcall SetTransparent(bool Value);
      void __fastcall SetSpacing(int Value);
      void __fastcall SetWordWrap(bool Value);
      void __fastcall SetReliefSize ( int Value );
      void __fastcall SetCloudColor   ( TColor _Color );
//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
      void __fastcall SetHighlightSound ( Bmwave::TBMCustomSound *Value );
      void __fastcall SetPulseSound     ( Bmwave::TBMCustomSound *Value );
      void __fastcall SetSelectSound    ( Bmwave::TBMCustomSound *Value );
//---> Begin ED
#endif
// End ED

      void __fastcall RemoveButtonMouseTimer(void);
      void __fastcall UpdateTracking(void);
      bool __fastcall PointInButton(int X, int Y);
      void __fastcall ButtonMouseTimerHandler(System::TObject* Sender);
      void __fastcall RepeatTimerHandler(System::TObject* Sender);
      HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Messages::TWMMouse &Message);
      HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
      MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
      MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
      HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
      MESSAGE void __fastcall CMTextChanged(Messages::TMessage &Message);
      MESSAGE void __fastcall CMSysColorChange(Messages::TMessage &Message);
      HIDESBASE MESSAGE void __fastcall WMCancelMode(Messages::TWMNoParams &Message);
      void __fastcall EraseBkg(TMessage& Msg);

      MESSAGE void __fastcall CMMouseEnter    ( TMessage& Msg );
      MESSAGE void __fastcall CMMouseLeave    ( TMessage& Msg );
      MESSAGE void __fastcall WMSize          ( Messages::TMessage    &Msg );
      MESSAGE void __fastcall WMMove          ( Messages::TMessage    &Msg );
      MESSAGE void __fastcall BMChangeView    ( Messages::TMessage    &Msg );

//---> Begin ED
#ifdef _USE_BMWAVE_
      void __fastcall PlaySound ( Bmwave::TBMCustomSound * S );
      void __fastcall PlayHighlightSound()   { PlaySound ( FHighlightSound ); };
      void __fastcall PlayPulseSound()       { PlaySound ( FPulseSound ); };
      void __fastcall PlaySelectSound()      { PlaySound ( FSelectSound ); };
#endif
// End ED

protected:
#ifdef __BCB_40__
      bool __fastcall IsCheckedStored(void);
      bool __fastcall IsHelpContextStored(void);
      bool __fastcall IsImageIndexStored(void);
#endif

      TBMButtonState FState;
      DYNAMIC HPALETTE __fastcall GetPalette(void);
      virtual void __fastcall Loaded(void);
      virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation );
      DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X,
		int Y);
//	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
      DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int
		Y);
      virtual void __fastcall Paint(void);

#ifdef __BCB_40__
      DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
      DYNAMIC TMetaClass* __fastcall GetActionLinkClass(void);
      virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
#endif

      void __fastcall ClickUp ( bool IsUpClick );

public:
      __property bool CallDormant = {read=GetCallDormant, write=SetCallDormant, nodefault};

public:
      __fastcall TBMShapedButton(Classes::TComponent* Owner);
      __fastcall virtual ~TBMShapedButton(void);

public:
      DYNAMIC void __fastcall Click(void);
      void __fastcall MouseEntered(void);
      void __fastcall MouseLeft(void);
      void __fastcall UpdateHighlighted ();

//---> Begin ED
public:
      TPoint __fastcall RecommendedSize();

protected:
      void __fastcall AdjustBounds(void);
      virtual bool __fastcall CanAutoSize(int &NewWidth, int &NewHeight);
// End ED

__published:

#ifdef __BCB_40__
      __property Action;
      __property Anchors;
      __property Constraints;
#endif

      __property Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=2};
      __property bool AllowAllUp = {read=FAllowAllUp, write=SetAllowAllUp, default=0};
      __property bool Cancel = {read=FCancel, write=FCancel, default=0};
      __property Color ;
      __property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
      __property bool Default = {read=FDefault, write=FDefault, default=0};
      __property TButtonDisplayMode DisplayMode = {read=FDisplayMode, write=SetDisplayMode, default=0};
      __property bool Down = {read=FDown, write=SetDown,
#ifdef __BCB_40__
      stored=IsCheckedStored,
#endif
      default=0};
      __property DragCursor ;
      __property DragMode ;
      __property bool DropdownArrow = {read=FDropdownArrow, write=SetDropdownArrow, default=1};
      __property bool DropdownCombo = {read=FDropdownCombo, write=SetDropdownCombo, default=0};
      __property Menus::TPopupMenu* DropdownMenu = {read=FDropdownMenu, write=SetDropdownMenu};
      __property Caption ;
      __property Enabled ;
      __property bool Flat = {read=FFlat, write=SetFlat, default=1};
      __property Font ;
      __property Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
//	__property Graphics::TBitmap* GlyphMask = {read=GetGlyphMask, write=SetGlyphMask};
      __property Classes::THelpContext HelpContext = {read=FHelpContext, write=FHelpContext,
#ifdef __BCB_40__
      stored=IsHelpContextStored,
#endif
      default=0};

      __property int ImageIndex = {read=GetImageIndex, write=SetImageIndex,
#ifdef __BCB_40__
      stored=IsImageIndexStored,
#endif
      default=-1};

      __property TCustomImageList* Images = {read=GetImages, write=SetImages};
//---> Begin ED
      //__property Buttons::TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
      __property TButtonLayoutPlus Layout = {read=FLayout, write=SetLayout, default=0};
// End ED
      __property int Margin = {read=FMargin, write=SetMargin, default=-1};
      __property TModalResult ModalResult = {read=FModalResult, write=FModalResult, default=0};
      __property bool Border = {read=FBorder, write=SetBorder, default=0};
      __property TBMNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, default=1};
      __property bool OldDisabledStyle = {read=FOldDisabledStyle, write=SetOldDisabledStyle, default=0};
      __property bool Transparent = {read=FTransparent, write=SetTransparent, default=true};
//---> Begin ED
      __property AutoSize;
// End ED
      __property ParentFont ;
      __property ParentColor ;
      __property ParentShowHint ;
      __property bool Repeating = {read=FRepeating, write=FRepeating, default=0};
      __property int RepeatDelay = {read=FRepeatDelay, write=FRepeatDelay, default=400};
      __property int RepeatInterval = {read=FRepeatInterval, write=FRepeatInterval, default=100};
//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
      __property Bmwave::TBMCustomSound *HighlightWave = { read=FHighlightSound, write=SetHighlightSound };
      __property Bmwave::TBMCustomSound *PulseWave     = { read=FPulseSound, write=SetPulseSound };
      __property Bmwave::TBMCustomSound *SelectWave    = { read=FSelectSound, write=SetSelectSound };
      __property bool EnableSounds                     = { read=FEnableSounds, write=FEnableSounds, default= true };
//---> Begin ED
#endif
// End ED
      __property bool ShowBorderWhenInactive = {read=FShowBorderWhenInactive, write=FShowBorderWhenInactive
		, default=0};
      __property TColor HotTrackColor    = { read=FHotTrackColor, write=FHotTrackColor, default=clHighlight };
      __property TColor CloudColor                = { read=FCloudColor, write=SetCloudColor, default=clYellow };
//    __property bool   EnableCloud                       = { read=FEnableCloud, write=SetEnableCloud, default=true };
      __property TBMCloudType CloudType                   = { read=FCloudType, write=SetCloudType, default=ctSmall };

      __property bool   Flashing                          = { read=FFlashing, write=SetFlashing, default=true };
      __property int    FlashedDelay                      = { read=FFlashedDelay, write=SetFlashedDelay, default= 6 };
      __property int    FlashingDelay                     = { read=FFlashingDelay, write=SetFlashingDelay, default= 6 };
      __property int    UnFlashedDelay                    = { read=FUnFlashedDelay, write=SetUnFlashedDelay, default= 300 };
//    __property bool   HugeCloud                         = { read=FHugeCloud, write=FHugeCloud, default= true };
      __property ShowHint ;
      __property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
      __property Visible ;
      __property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
      __property int ReliefSize = {read=FReliefSize, write=SetReliefSize, default=2};
      __property OnClick ;
      __property OnDblClick ;
      __property OnDragDrop ;
      __property OnDragOver ;
      __property OnEndDrag ;
      __property OnMouseDown ;
      __property Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
      __property Classes::TNotifyEvent OnMouseExit = {read=FOnMouseExit, write=FOnMouseExit};
      __property OnMouseMove ;
      __property OnMouseUp ;
      __property OnStartDrag ;

//---> Begin ED
  DECLARATION_CARTE_MESSAGES
/*  BEGIN_MESSAGE_MAP
    VCL_MESSAGE_HANDLER ( WM_SIZE,              TMessage,           WMSize           )
    VCL_MESSAGE_HANDLER ( WM_MOVE,              TMessage,           WMMove           )
    VCL_MESSAGE_HANDLER ( CM_MOUSEENTER,        TMessage,           CMMouseEnter     )
    VCL_MESSAGE_HANDLER ( CM_MOUSELEAVE,        TMessage,           CMMouseLeave     )
    VCL_MESSAGE_HANDLER ( WM_LBUTTONDBLCLK,     TWMLButtonDblClk,   WMLButtonDblClk  )
    VCL_MESSAGE_HANDLER ( CM_ENABLEDCHANGED,    TMessage,           CMEnabledChanged )
    VCL_MESSAGE_HANDLER ( CM_DIALOGCHAR, TCMDialogChar,             CMDialogChar     )
    VCL_MESSAGE_HANDLER ( CM_DIALOGKEY,         TCMDialogKey,       CMDialogKey      )
    VCL_MESSAGE_HANDLER ( CM_FONTCHANGED,       TMessage,           CMFontChanged    )
    VCL_MESSAGE_HANDLER ( CM_TEXTCHANGED,       TMessage,           CMTextChanged    )
    VCL_MESSAGE_HANDLER ( CM_SYSCOLORCHANGE,    TMessage,           CMSysColorChange )
    VCL_MESSAGE_HANDLER ( WM_CANCELMODE,        TWMCancelMode,      WMCancelMode     )
    VCL_MESSAGE_HANDLER ( WM_ERASEBKGND,        TMessage,           EraseBkg         )
    VCL_MESSAGE_HANDLER ( BM_CHANGE_VIEW,       TMessage,           BMChangeView     )
  END_MESSAGE_MAP(inherited);*/
// End ED
};



#ifdef __BCB_40__

#pragma pack(push, 4)
class PACKAGE TBMShapedButtonActionLink : public Controls::TControlActionLink
{
      typedef Controls::TControlActionLink inherited;

      friend class TBMShapedButton;

protected:
      TBMShapedButton* FClient;

protected:
      virtual void __fastcall AssignClient(System::TObject* AClient);
      virtual bool __fastcall IsCheckedLinked(void);
      virtual bool __fastcall IsHelpContextLinked(void);
      virtual bool __fastcall IsImageIndexLinked(void);
      virtual void __fastcall SetChecked(bool Value);
      virtual void __fastcall SetHelpContext(Classes::THelpContext Value);
      virtual void __fastcall SetImageIndex(int Value);
public:
      #pragma option push -w-inl
      inline __fastcall virtual TBMShapedButtonActionLink(System::TObject*
		AClient) : Controls::TControlActionLink(AClient) { }
      #pragma option pop
      #pragma option push -w-inl
      inline __fastcall virtual ~TBMShapedButtonActionLink(void) { }
      #pragma option pop

};

#pragma pack(pop)

typedef TMetaClass*TBMShapedButtonActionLinkClass;
#endif

};
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmshapedbutton;
#endif
//---------------------------------------------------------------------------
#endif
