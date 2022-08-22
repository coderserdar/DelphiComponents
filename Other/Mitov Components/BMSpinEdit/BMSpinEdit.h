/*> Ver: V2.2 ************************************************************\

**> History :

Beta V1.0       02/21/2001  Released
Beta V1.1       03/05/2001  Some minor fixes by Mauro Venturini
Beta V1.2       03/08/2001  Stay on top bug has been fixed.
Beta V1.3       03/10/2001  The Gauge height and progress colorisation have been added.
                            Arrow keys support has been added.

Beta V1.4       03/12/2001  TrackBarEnabled and IncrementGauge have been added.
Beta V1.5       03/21/2001  Round has been fixed
                            GaugeMinValue, GaugeMaxValue and GaugeAroundCenter have been added.

Beta V1.6       05/05/2001  Keeps focus on the parent form when shows popup.
Beta V1.7       09/04/2001  Some spelling errors fixed.
Beta V1.8       12/04/2003  Small bug fixed by Mateo Zanotelli
V1.9            12/06/2004  BCB 6.0 Compliant.
V2.0            11/21/2008  BCB 2006, 2007 and 2009 Compliant.
V2.1            11/22/2009  BCB 2010 Compliant.
V2.2            11/28/2011  BCB XE and XE2 Compliant.

**> Legal issues :

              Copyright (C) 2001-2011 by Boian Mitov
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
#ifndef BMSpinEditH
#define BMSpinEditH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <comctrls.hpp>
#include <buttons.hpp>
//---------------------------------------------------------------------------
#if ( __BORLANDC__ >= 0x0560 )
  #define __BCB_60__
  #define max std::max
  #define min std::min
#endif

//---------------------------------------------------------------------------
enum
{
  WM_NEEDS_UPDATE = WM_USER + 0x900,
};

//---------------------------------------------------------------------------
class PACKAGE TBMSpinEdit : public TCustomEdit
{
  typedef TCustomEdit inherited;

private:
  class PACKAGE TAGauge : public TCustomControl
  {
  private:
    TBMSpinEdit *FEdit;
    TColor       FGaugeBeginColor;
    TColor       FGaugeEndColor;
    double       FIncrement;
    double       FGaugeAroundCenter;

  protected:
    virtual void __fastcall Paint(void);

    void __fastcall SetGaugeBeginColor ( TColor Value );
    void __fastcall SetGaugeEndColor ( TColor Value );

    void __fastcall PaintBackground(Graphics::TBitmap* AnImage);
    void __fastcall PaintAsBar(Graphics::TBitmap* AnImage, const TRect& PaintRect);
    void __fastcall EraseBkg(TMessage& Msg) {}
    void __fastcall WMLBtnDown(TMessage& Msg);
    void __fastcall WMLBtnUp(TMessage& Msg);
    void __fastcall WMMouseMove(TMessage& Msg);

  public :
    __fastcall TAGauge( TBMSpinEdit *Owner );
    BEGIN_MESSAGE_MAP
      VCL_MESSAGE_HANDLER ( WM_ERASEBKGND,        TMessage,           EraseBkg         )
      VCL_MESSAGE_HANDLER ( WM_LBUTTONDOWN,       TMessage,           WMLBtnDown       )
      VCL_MESSAGE_HANDLER ( WM_LBUTTONUP,         TMessage,           WMLBtnUp         )
      VCL_MESSAGE_HANDLER ( WM_MOUSEMOVE,         TMessage,           WMMouseMove      )
    END_MESSAGE_MAP(TCustomControl);

  public :
    __property TColor GaugeBeginColor = {read=FGaugeBeginColor,write=SetGaugeBeginColor};
    __property TColor GaugeEndColor = {read=FGaugeEndColor,write=SetGaugeEndColor};
    __property double Increment = {read=FIncrement, write=FIncrement, default=0};
    __property double GaugeAroundCenter = {read=FGaugeAroundCenter, write=FGaugeAroundCenter, default=0};
  };
  
  friend TAGauge;
  
protected: // To be static.

  TForm     *Form;
  TTrackBar *TrackBar;

protected:                    
  TAGauge   *ProgressBar;

  double FMinValue;
  double FMaxValue;
  double FGaugeMinValue;
  double FGaugeMaxValue;
  double FIncrement;
  bool   FTrackBarEnabled;

  bool  FEditorEnabled;
  bool  InChanging;

  TTrackBarOrientation  FTrackBarOrientation;

  bool  FWrap;

  int   FPrecision;
  int   FGaugeHeight;
  int   FTrackBarWidth;

  TUpDown       *FUpDown;
  TSpeedButton  *FSpeedButton;
  TPanel        *FPanel;

  TPoint MoveFormToPoint;

  int    FTrackBarHeight;

protected:
  int  __fastcall GetMinHeight(void);
  void __fastcall SetEditRect(void);
  double __fastcall GetValue(void);
  double __fastcall CheckValue(double NewValue);
  void __fastcall SetValue(double NewValue);
  void __fastcall UpDownChanging(TObject *Sender, bool &AllowChange);
  void __fastcall UpDownClick(TObject *Sender, TUDBtnType Button);
  void __fastcall SpeedButtonMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SpeedButtonMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SpeedButtonMouseMove(System::TObject* Sender,
      TShiftState Shift, int X, int Y);

  void __fastcall OnFormShow(TObject *Sender);

  void __fastcall UpdateFormView();

  DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);

protected:
  void __fastcall   SetMaxValue( double Value );
  void __fastcall   SetMinValue( double Value );
  void __fastcall   SetGaugeMaxValue( double Value );
  void __fastcall   SetGaugeMinValue( double Value );

  void __fastcall   SetGaugeAroundCenter( double Value );
  double __fastcall GetGaugeAroundCenter();

  void __fastcall   SetIncrement( double Value );
  void __fastcall   SetIncrementGauge( double Value );
  double __fastcall GetIncrementGauge();

  void __fastcall   SetGaugeBeginColor( TColor Value );
  TColor __fastcall GetGaugeBeginColor();
  void __fastcall   SetGaugeEndColor( TColor Value );
  TColor __fastcall GetGaugeEndColor();

  void __fastcall SetTrackBarWidth( int Value );
  void __fastcall SetPrecision( int Value );
  void __fastcall SetGaugeHeight( int Value );
  void __fastcall SetTrackBarOrientation( TTrackBarOrientation Orientation );
  void __fastcall SetTrackBarEnabled( bool Value );


protected:
  virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
  virtual void __fastcall CreateHandle(void);
  virtual void __fastcall CreateParams(TCreateParams &Params);
  virtual void __fastcall CreateWnd(void);
  DYNAMIC void __fastcall Change(void);

  MESSAGE void __fastcall WMSize(TWMSize &Message);
  MESSAGE void __fastcall CMEnter(TWMNoParams &Message);
  MESSAGE void __fastcall CMExit(TWMNoParams &Message);
  MESSAGE void __fastcall WMPaste(TWMNoParams &Message);
  MESSAGE void __fastcall WMCut(TWMNoParams &Message);
  MESSAGE void __fastcall WMUpdateMe(TMessage &Message);

  void __fastcall SetButtonHandle();

public:
        __fastcall TBMSpinEdit(TComponent* Owner);


 BEGIN_MESSAGE_MAP
    VCL_MESSAGE_HANDLER( WM_SIZE,           TWMSize,      WMSize               )
    VCL_MESSAGE_HANDLER( CM_ENTER,          TWMNoParams,  CMEnter              )
    VCL_MESSAGE_HANDLER( CM_EXIT,           TWMNoParams,  CMExit               )
    VCL_MESSAGE_HANDLER( WM_PASTE,          TWMNoParams,  WMPaste              )
    VCL_MESSAGE_HANDLER( WM_CUT,            TWMNoParams,  WMCut                )
    VCL_MESSAGE_HANDLER( WM_NEEDS_UPDATE,   TMessage,     WMUpdateMe           )
  END_MESSAGE_MAP(inherited);

__published:
    __property Anchors ;
    __property AutoSelect ;
    __property AutoSize ;
    __property Color ;
    __property Constraints ;
    __property Ctl3D ;
    __property DragCursor ;
    __property DragMode ;
    __property bool EditorEnabled = {read=FEditorEnabled, write=FEditorEnabled, default=true};
    __property Enabled ;
    __property Font ;
    __property ParentColor ;
    __property ParentCtl3D ;
    __property ParentFont ;
    __property ParentShowHint ;
    __property PopupMenu ;
    __property ReadOnly ;
    __property ShowHint ;
    __property TabOrder ;
    __property TabStop ;
    __property Visible ;
    __property OnChange ;
    __property OnClick ;
    __property OnDblClick ;
    __property OnDragDrop ;
    __property OnDragOver ;
    __property OnEndDrag ;
    __property OnEnter ;
    __property OnExit ;
    __property OnKeyDown ;
    __property OnKeyPress ;
    __property OnKeyUp ;
    __property OnMouseDown ;
    __property OnMouseMove ;
    __property OnMouseUp ;

    __property double Increment = {read=FIncrement, write=SetIncrement, default=1};
    __property double IncrementGauge = {read=GetIncrementGauge, write=SetIncrementGauge, default=0};
    __property double MaxValue = {read=FMaxValue, write=SetMaxValue, nodefault};
    __property double MinValue = {read=FMinValue, write=SetMinValue, nodefault};

    __property double GaugeMaxValue = {read=FGaugeMaxValue, write=SetGaugeMaxValue, nodefault};
    __property double GaugeMinValue = {read=FGaugeMinValue, write=SetGaugeMinValue, nodefault};

    __property double GaugeAroundCenter = {read=GetGaugeAroundCenter, write=SetGaugeAroundCenter, nodefault};

    __property double Value = {read=GetValue, write=SetValue, nodefault};
    __property TColor GaugeBeginColor = {read=GetGaugeBeginColor,write=SetGaugeBeginColor, default=clHighlight};
    __property TColor GaugeEndColor = {read=GetGaugeEndColor,write=SetGaugeEndColor, default=clRed};
    __property int    TrackBarWidth = {read=FTrackBarWidth, write=SetTrackBarWidth, default=150};
    __property int    Precision = {read=FPrecision, write=SetPrecision, default=2};
    __property int    GaugeHeight = {read=FGaugeHeight, write=SetGaugeHeight, default=5 };

    __property TTrackBarOrientation TrackBarOrientation = { read=FTrackBarOrientation, write=SetTrackBarOrientation, default=trHorizontal };
    __property bool TrackBarEnabled = { read=FTrackBarEnabled, write=SetTrackBarEnabled, default=true };
};
//---------------------------------------------------------------------------
#endif
