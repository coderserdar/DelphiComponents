/*> Ver: V2.5 *********      History      ***************************\

Beta V1.0b0     01/10/1997      Released
Beta V1.0b1     01/15/1997      Fixed bug related with usage as secondary
                                  splitter on the same panel.
                                Fixed splkBottom or splkRight bug.
Beta V1.0b2     01/15/1998      Removed #pragma link EmptyPanel" Thanks to
                                  Jeff Moxon for the bug report.

Beta V2.0b1     03/14/1998      Now supports BCB 3.0
Beta V2.0b2     08/16/1998      A windows messages hooking bug's been fixed.
Beta V2.0b3     11/20/1998      The mouse right click problem's been fixed
                              ( Thanks to Reiner Rosin for the bug report).
Beta V2.0b4     04/15/1999      BCB 4.0 Support.
Beta V2.0b5     04/29/1999      Scrolling windowses bug fixed.
Beta V2.0b6     08/10/1999      Resizing fixes for Win98. Added missing #include.
Beta V2.0b7     11/05/2000      Dynamic update, Collapsing.
Beta V2.0b8     11/06/2000      Small bug fixed in button position update.
Beta V2.0b9     11/08/2000      Small firx for multiple splitters within the same project.
Beta V2.0b10    11/15/2000      Added color for the slitter area.
Beta V2.1b0     03/20/2003      BCB 6.0 support.

V2.2            12/07/2004      Copy and paste support added.
V2.3            03/16/2008      Addes C++ Builder 2006 and C++ Builder 2007 support.
V2.4            03/24/2008      BMMessageHook moved into a shared package.
V2.5            11/23/2008      Addes C++ Builder 2009 support.

Legal issues: Copyright (C) 1997 - 2008 by Boian Mitov
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
#ifndef BMSplitterH
#define BMSplitterH

#include <buttons.hpp>


//---------------------------------------------------------------------------
#if (__BORLANDC__ < 0x0530)
//---------------------------------------------------------------------------
// BCB 1.0
//---------------------------------------------------------------------------
  #define PACKAGE
  #define DYNAMIC

#endif
//---------------------------------------------------------------------------

class TBMMessageHook;
//---------------------------------------------------------------------------
namespace Bmsplitter
{
//---------------------------------------------------------------------------
class TSplitterPanelOnly;
//---------------------------------------------------------------------------
enum TBMSplitterType { splkLeft, splkTop, splkRight, splkBottom };
//---------------------------------------------------------------------------
};
//---------------------------------------------------------------------------
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmsplitter;
#endif

#define WM_TBMSplitterFIRSTMESSAGE  (WM_USER + 450)

//---------------------------------------------------------------------------
class PACKAGE TBMSplitter : public TCustomPanel
{
  typedef TCustomPanel inherited;
  
private:
//public :
  TPanel           *ControlPanel;
//  TPanel           *FirstPanel;

protected:        //  For properties.
  // Align
  TBMSplitterType   FKind;

  // Splitter
  int               FSplitterWidth;
  bool              FAutoSelectCursor;
  TPanelBevel       FSplitterBevel;
  int               FSplitterBevelWidth;

  int               RestorePromile;

  TResizeStyle      FResizeStyle;

  // Limits
  int               FMinPercent;
  int               FMaxPercent;

  // StartUp
  bool              FInitialPercentEnabled;
  int               FInitialPercent;

  // Hint
  int               FHintX;
  int               FHintY;
  bool              FShowPercentHint;

  TSpeedButton     *SpeedButton;

protected:        // For internal use only.

  enum TKindOf { koLeftRight, koTopBottom };

  TKindOf           KindOf;

  int               OldRight;
  int               OldBottom;

  bool              MouseIsDown;

  bool              FCollapsed;
  bool              FCanCollapse;

  int               MainWidth;
  int               MainHeight;

  int               FButtonWidth;

  int               ControlWidth;

  TAlign            OldAlign;
  THintWindow      *HintWindow;
  TPoint            AbsMousePos;

  long              FPromile;

  TRect             ParentRect;

  TWinControl      *FOwner;

  TBMMessageHook   *MessageHook;

  TPoint AbsSplitterPos;   // Current position of the splitter (useful when
                        // splitting)

protected: //  For properties.
  void __fastcall SetKind( TBMSplitterType _Kind );
  void __fastcall SetMinPercent( int newMinPercent );
  void __fastcall SetMaxPercent( int newMaxPercent );

  void __fastcall SetSplitterWidth( int Value );

  void __fastcall SetAutoSelectCursor( bool Value );

  void __fastcall SetSplitterBevel( TPanelBevel FSplitterBevel );
  void __fastcall SetSplitterBevelWidth( int _SplitterBevelWidth );

  void __fastcall SetInitialPercent( int _InitialPercent );
  void __fastcall SetInitialPercentEnabled( bool _Enabled );

protected: //  General purpose
  void __fastcall ControlMouseDown(TObject *Sender, TMouseButton Button,
    TShiftState Shift, int X, int Y);

  void __fastcall ControlMouseUp(TObject *Sender, TMouseButton Button,
    TShiftState Shift, int X, int Y);

  void __fastcall ControlMouseMove(TObject *Sender, TShiftState Shift,
    int X, int Y);

  void __fastcall DrawSplitter();
  void __fastcall FixControl();
  void __fastcall Calibrate();
  void __fastcall CheckAlign();

  void __fastcall OnOwnerSendMessage( System::TObject* Sender, Messages::TMessage &Message );

  void __fastcall FixCursor();
  void __fastcall FixAlign();
  void __fastcall vShowHint( int X, int Y );
  void __fastcall HideHint();
  void __fastcall CalculatePromile();
  void __fastcall SplitterResize();
  void __fastcall ResizeToSplitter();
  void __fastcall SetCollapsed( bool Value );
  void __fastcall SetCanCollapse( bool Value );
  void __fastcall SetCollapseButton( bool Value );
  void __fastcall SetButtonWidth( int Value );
  bool __fastcall GetCollapseButton();
  void __fastcall SetButtonSize();

  TColor __fastcall GetSliderColor();
  void __fastcall SetSliderColor( TColor Value );

  bool __fastcall GetSliderParentColor();
  void __fastcall SetSliderParentColor( bool Value );

  virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);

  void __fastcall OnButtonClick( TObject *Sender );

  virtual void __fastcall CreateWnd(void);
  virtual void __fastcall WndProc( Messages::TMessage &Message );

//  virtual void __fastcall AdjustClientRect(Types::TRect &Rect);
  
//  DYNAMIC /*virtual*/ TComponent* __fastcall GetChildOwner(void);
//  DYNAMIC /*virtual*/ TComponent* __fastcall GetChildParent(void);

#if (__BORLANDC__ < 0x0530)
//---------------------------------------------------------------------------
// BCB 1.0
//---------------------------------------------------------------------------
//  virtual void __fastcall GetChildren(TGetChildProc Proc);
#else
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
//  DYNAMIC void __fastcall GetChildren(TGetChildProc Proc, TComponent* Root);
#endif

  void __fastcall GetParentRectangle ();
//  void __fastcall GetParentRectangle ( int Width, int Height );
  void __fastcall GetParentSize ();
//  void __fastcall GetParentSize ( int Width, int Height );
  void __fastcall CalculateSize ();
  void __fastcall CheckMinMaxPercent ();
  TPoint __fastcall GetPoint ();
  
  void __fastcall EraseBkg(TMessage& Msg);
  void __fastcall WmMoveSize (TMessage& Msg);
  void __fastcall WmInternal(TMessage& Msg);

BEGIN_MESSAGE_MAP
#if (__BORLANDC__ < 0x0530)
  VCL_MESSAGE_HANDLER( WM_ERASEBKGND,TMessage,EraseBkg )
#endif  
  VCL_MESSAGE_HANDLER( WM_MOVE, TMessage, WmMoveSize )
//  VCL_MESSAGE_HANDLER( WM_NCCALCSIZE, TMessage, WmMoveSize )
//  VCL_MESSAGE_HANDLER( WM_HSCROLL, TMessage, WmMoveSize )
  VCL_MESSAGE_HANDLER( WM_TBMSplitterFIRSTMESSAGE,TMessage,WmInternal )
  
END_MESSAGE_MAP(TCustomPanel)

public:
    __fastcall TBMSplitter(TComponent* Owner);
    __fastcall virtual ~TBMSplitter(void);

public :
  __property TPanel *SplitterPanel  = { read=ControlPanel };

__published:
    // Alignment
    __property TBMSplitterType Kind = { read=FKind, write=SetKind, default=splkLeft };

    // General properties
    __property bool AutoSelectCursor = { read=FAutoSelectCursor, write=SetAutoSelectCursor, default=true };

    // Limits properties
    __property int MinPercent = { read=FMinPercent, write=SetMinPercent, default=5 };
    __property int MaxPercent = { read=FMaxPercent, write=SetMaxPercent, default=95 };

    // StartUp
    __property bool InitialPercentEnabled = { read=FInitialPercentEnabled, write=SetInitialPercentEnabled, default=false};
    __property int InitialPercent = { read=FInitialPercent, write=SetInitialPercent, default=50 };

    // Splitter properties
    __property int SplitterWidth = { read=FSplitterWidth, write=SetSplitterWidth, default=4};
    __property TPanelBevel SplitterBevel = { read=FSplitterBevel, write=SetSplitterBevel, default=bvLowered};
    __property int SplitterBevelWidth = { read=FSplitterBevelWidth, write=SetSplitterBevelWidth, default=1};

    __property TResizeStyle ResizeStyle = { read=FResizeStyle, write=FResizeStyle, default=rsLine};
    
    // Hint properties
    __property int HintX = { read=FHintX, write=FHintX, default=5};
    __property int HintY = { read=FHintY, write=FHintY, default=5};
    __property bool ShowPercentHint = { read=FShowPercentHint, write=FShowPercentHint, default=true };
    __property bool Collapsed = { read=FCollapsed, write=SetCollapsed };
    __property bool CanCollapse = {read=FCanCollapse, write=SetCanCollapse};
    __property bool CollapseButton = {read=GetCollapseButton, write=SetCollapseButton};

    __property int  ButtonWidth = { read=FButtonWidth, write=SetButtonWidth, default=40 };

    __property TColor SplitterColor = { read=GetSliderColor, write=SetSliderColor, nodefault };
    __property bool   SplitterParentColor = { read=GetSliderParentColor, write=SetSliderParentColor };


__published:
    __property BevelInner ;
    __property BevelOuter = { default = bvNone };
    __property BevelWidth ;
    __property BorderWidth ;
    __property BorderStyle ;
    __property DragCursor ;
    __property DragMode ;
    __property Enabled ;
    __property Color ;
    __property Ctl3D ;
    __property Locked ;
    __property ParentColor ;
    __property ParentCtl3D ;
    __property ParentFont ;
    __property ParentShowHint ;
    __property PopupMenu ;
    __property ShowHint ;
    __property TabOrder ;
    __property TabStop ;
    __property Visible ;
    
    __property OnClick ;
    __property OnDblClick ;
    __property OnDragDrop ;
    __property OnDragOver ;
    __property OnEndDrag ;
    __property OnEnter ;
    __property OnExit ;
    __property OnMouseDown ;
    __property OnMouseMove ;
    __property OnMouseUp ;
    __property OnResize ;
    __property OnStartDrag ;
};
//---------------------------------------------------------------------------
#endif

