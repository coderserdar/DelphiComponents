//---------------------------------------------------------------------------
#include <vcl.h>
#include <algorithm>
#include <memory>       //for auto_ptr STL class
#include <math.h>
#pragma hdrstop

#include "BMSpinEdit.h"
#pragma resource "BMSpinEditInt.res"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
//---------------------------------------------------------------------------
namespace
{
  inline int Round( double Value )
    {
    if( Value > 0 ) 
      return int( Value + 0.5 );
      
    else
      return int( Value - 0.5 );
    }
};

__fastcall TBMSpinEdit::TAGauge::TAGauge( TBMSpinEdit *Owner )
  : TCustomControl (Owner)
{
  FGaugeAroundCenter = 0;
  FIncrement = 0;
  FGaugeBeginColor = clHighlight;
  FGaugeEndColor = clRed;
  FEdit = Owner;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::SetGaugeBeginColor ( TColor Value )
{
  if ( FGaugeBeginColor != Value )
    {
    FGaugeBeginColor = Value;
    Invalidate();
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::SetGaugeEndColor ( TColor Value )
{
  if ( FGaugeEndColor != Value )
    {
    FGaugeEndColor = Value;
    Invalidate();
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::WMLBtnDown(TMessage& Msg)
{
  MouseCapture = true;
  WMMouseMove(Msg);

  FEdit->SetFocus ();
  FEdit->SelLength = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::WMLBtnUp(TMessage& Msg)
{
  WMMouseMove(Msg);
  MouseCapture = false;
  if (FEdit->AutoSelect && !(FEdit->ControlState.Contains(csLButtonDown)))
    FEdit->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::WMMouseMove(TMessage& Msg)
{
  if ( ! MouseCapture )
    return;

  short Pos = (short)Msg.LParamLo;

  double LocalMinValue = FEdit->GaugeMinValue;             
  double LocalMaxValue = FEdit->GaugeMaxValue;
               
  if ((LocalMinValue==0) && (LocalMaxValue==0))
    {
    LocalMinValue = FEdit->MinValue;
    LocalMaxValue = FEdit->MaxValue;
    }
                                                         
  double Diff = LocalMaxValue - LocalMinValue;             
  
//  double Diff = FEdit->MaxValue - FEdit->MinValue;
  if ( Diff <= 0 )
    return;

  if ( Pos <= 0 )
    {
    FEdit->Value = LocalMinValue;
    return;
    }

  double Value = (double)( Pos * Diff / ( Width - 2 ) ) + LocalMinValue;//SolveForX(PercentDone, W);

  if ( FIncrement > 0 )                              
    Value = FIncrement * Round( Value / FIncrement );

  FEdit->Value = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::PaintBackground(Graphics::TBitmap* AnImage)
{
  TRect  ARect;
  AnImage->Canvas->CopyMode = cmBlackness;
  ARect = Rect(0, 0, Width, Height);
  AnImage->Canvas->CopyRect(ARect, AnImage->Canvas, ARect);
  AnImage->Canvas->CopyMode = cmSrcCopy;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::PaintAsBar(Graphics::TBitmap* AnImage, const TRect& PaintRect)
{
  Longint  FillSize;
  Integer  W, H;

  double LocalMinValue = FEdit->GaugeMinValue;
  double LocalMaxValue = FEdit->GaugeMaxValue;
  if ( LocalMinValue==0 && LocalMaxValue==0 )
    {
    LocalMinValue = FEdit->MinValue;
    LocalMaxValue = FEdit->MaxValue;
    }

//  double Diff = LocalMaxValue - LocalMinValue;



  double Diff = FEdit->MaxValue - FEdit->MinValue;
  if ( Diff <= 0 )
    Diff = 100;

  W = PaintRect.Right - PaintRect.Left + 1 - 2;
  H = PaintRect.Bottom - PaintRect.Top + 1;
  
//  >>


  double EditValue = FEdit->Value;
  FillSize = min(Round((EditValue - LocalMinValue) * W / Diff + 1.0),W); 

  TRect LocalPaintRect = PaintRect;        
  LocalPaintRect.Bottom = H;
  long CenterPos;
  bool DoFillRect;
  bool IsCentered;
           
  if (( GaugeAroundCenter > LocalMinValue ) && ( GaugeAroundCenter < LocalMaxValue ))
    {  
    CenterPos = min(Round((GaugeAroundCenter - LocalMinValue) * W / Diff + 1.0 ),W);        
    LocalPaintRect.Left = min( CenterPos, FillSize );
    LocalPaintRect.Right = max( CenterPos, FillSize );
    DoFillRect = ( EditValue != GaugeAroundCenter );
    IsCentered = true;                                
    }
    
  else
    {
    LocalPaintRect.Right = FillSize;
    DoFillRect = FillSize > 0;
    IsCentered = false;
    CenterPos = 0;   // only to initialize it
    }                                                 

  double LocalCenterValue = FEdit->GaugeAroundCenter;
  long Correction;

  if ( IsCentered && ( LocalCenterValue > LocalMinValue ) && ( LocalCenterValue < LocalMaxValue ))
      {
      if( EditValue > LocalCenterValue )
        Correction = Round( ( EditValue - LocalCenterValue ) * 256 / ( LocalMaxValue - LocalCenterValue ) + 1.0 );    

      else                                                                                                            
        Correction = Round(( LocalCenterValue - EditValue ) * 256 / ( LocalCenterValue - LocalMinValue ) + 1.0 );     

      }

  else                                                                                                                
    Correction = Round(( EditValue - LocalMinValue ) * 256 / Diff + 1.0 );

//  >>
 
  FillSize = ( FEdit->Value - FEdit->MinValue ) * W / Diff + 1;//SolveForX(PercentDone, W);
  if (FillSize > W)
    FillSize = W;

//  int Correction = ( FEdit->Value - FEdit->MinValue ) * 256 / Diff + 1;//SolveForX(PercentDone, W);

  AnImage->Canvas->Brush->Color = FEdit->Color; //BackColor;
  AnImage->Canvas->FillRect(PaintRect);

  TColor BeginColor = (TColor)ColorToRGB( FGaugeBeginColor );
  TColor EndColor = (TColor)ColorToRGB( FGaugeEndColor );

  BYTE Red   = GetRValue( BeginColor ) + ((GetRValue( EndColor ) - GetRValue( BeginColor )) * Correction / 256 );
  BYTE Green = GetGValue( BeginColor ) + ((GetGValue( EndColor ) - GetGValue( BeginColor )) * Correction / 256 );
  BYTE Blue  = GetBValue( BeginColor ) + ((GetBValue( EndColor ) - GetBValue( BeginColor )) * Correction / 256 );

  TColor ResultColor = (TColor)RGB( Red, Green, Blue );

  AnImage->Canvas->Pen->Color = ResultColor; //ForeColor;
  AnImage->Canvas->Pen->Width = 1;
  AnImage->Canvas->Brush->Color = ResultColor; //ForeColor;

/*
  if DoFillRect then                                   // <- KLE, 20.3.2001
    AnImage.Canvas.FillRect(LocalPaintRect);           // <- KLE, 20.3.2001
  if IsCentered then                                   // <- KLE, 20.3.2001
    with AnImage.Canvas do begin                       // <- KLE, 20.3.2001
      Pen.Color := clWindowFrame;                      // <- KLE, 20.3.2001
      Pen.Style := psSolid;                            // <- KLE, 20.3.2001
      MoveTo(CenterPos,LocalPaintRect.Top);            // <- KLE, 20.3.2001
      LineTo(CenterPos,LocalPaintRect.Bottom);         // <- KLE, 20.3.2001
    end;                                               // <- KLE, 20.3.2001
*/
  if (DoFillRect)
    AnImage->Canvas->FillRect( LocalPaintRect );
/*
    AnImage->Canvas->FillRect(Rect(PaintRect.Left,
                                        PaintRect.Top,
                                        FillSize,
                                        H));
*/

  if( IsCentered )
    {
    AnImage->Canvas->Pen->Color = clWindowFrame;
    AnImage->Canvas->Pen->Style = psSolid;
    AnImage->Canvas->MoveTo( CenterPos, LocalPaintRect.Top );
    AnImage->Canvas->LineTo( CenterPos, LocalPaintRect.Bottom );
    } 
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::TAGauge::Paint(void)
{
  std::auto_ptr<Graphics::TBitmap> TheImage(new Graphics::TBitmap());
  std::auto_ptr<Graphics::TBitmap> OverlayImage(new Graphics::TBitmap());
  TRect PaintTRect;

  TheImage->Height = Height;
  TheImage->Width = Width;
  PaintBackground(TheImage.get());
  PaintTRect = ClientRect;

  OverlayImage->Width = TheImage->Width;
  OverlayImage->Height = TheImage->Height;
  OverlayImage->Canvas->Brush->Color = TColor(clWindowFrame);
  OverlayImage->Canvas->Brush->Style = bsSolid;
  OverlayImage->Canvas->FillRect(Rect(0, 0, Width, Height));

  PaintBackground(OverlayImage.get());
  PaintAsBar(OverlayImage.get(), PaintTRect);

  OverlayImage->Canvas->Brush->Color = clBtnFace;
  OverlayImage->Canvas->FrameRect ( Rect ( 0, 0, Width, Height ));

  TheImage->Canvas->CopyMode = cmSrcInvert;
  TheImage->Canvas->Draw(0, 0, OverlayImage.get());
  TheImage->Canvas->CopyMode = cmSrcCopy;
  Canvas->CopyMode = cmSrcCopy;
  Canvas->Draw(0, 0, TheImage.get());
}
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TBMSpinEdit *)
{
        new TBMSpinEdit(NULL);
}
//---------------------------------------------------------------------------
#define BORDER_WIDTH    4
//---------------------------------------------------------------------------
namespace
{
class PACKAGE TBMPopupForm : public TForm
{
public :
  __fastcall TBMPopupForm( Classes::TComponent* AOwner ) :
    TForm( AOwner, 0 )
      {}

protected :
  virtual void __fastcall CreateParams(Controls::TCreateParams &Params)
    {
    TForm::CreateParams( Params );
    Params.Style &= ~( WS_POPUP | WS_CAPTION | WS_SIZEBOX ); 
    Params.Style |= WS_CHILD;
    Params.ExStyle |= WS_EX_PALETTEWINDOW;
    }
    
  virtual void __fastcall CreateWnd()
    {
    TForm::CreateWnd();
    ::SetParent( Handle, GetDesktopWindow());
    }
    
public :
  __property MouseCapture;
};
};
//---------------------------------------------------------------------------
__fastcall TBMSpinEdit::TBMSpinEdit(TComponent* Owner)
        : inherited(Owner)/*, Edit ( NULL )*/, ProgressBar ( NULL ), FUpDown ( NULL )
{
  FTrackBarOrientation = trHorizontal;
  FTrackBarWidth = 150;
  FMaxValue = 100;
  FMinValue = 0;

  FGaugeMinValue = 0.0;
  FGaugeMaxValue = 0.0;

  FTrackBarEnabled = true;

  FPrecision = 2;
  FGaugeHeight = 5;
  InChanging = false;

  FWrap = false;

  FPanel = new TPanel ( this );
  FPanel->Parent = this;
  FPanel->BevelOuter = bvNone;

  FPanel->Color = clBtnFace;

  ProgressBar = new TAGauge ( this );
  ProgressBar->Parent = this;
  
  FUpDown = new TUpDown ( FPanel );
  FUpDown->ArrowKeys = true;
  FUpDown->Left = 1;
  FUpDown->Top = 1;
  FUpDown->Width = 13;
  FUpDown->Height = 17;
  FUpDown->Visible = true;

  FUpDown->Parent = FPanel;
  FUpDown->OnClick = UpDownClick;
  FUpDown->Wrap = true;
  FUpDown->Min = 0;
  FUpDown->Max = 100;
  FUpDown->Position = 1;
  FUpDown->Increment = 1;

  FSpeedButton = new TSpeedButton ( FPanel );
  FSpeedButton->Parent = FPanel;
  FSpeedButton->Top = 1;
  FSpeedButton->Left = 13 + 1 + 1;
  FSpeedButton->Visible = true;
  FSpeedButton->Width = 13;

  FSpeedButton->NumGlyphs = 1;
  FSpeedButton->Invalidate();
  FSpeedButton->OnMouseDown = SpeedButtonMouseDown;
  FSpeedButton->OnMouseUp = SpeedButtonMouseUp;
  FSpeedButton->OnMouseMove = SpeedButtonMouseMove;

  Form = new TBMPopupForm ( this );
  Form->FormStyle = fsStayOnTop;
  Form->OnShow = OnFormShow;
  Form->Position = poDesigned;
//  Form->OnMouseMove = 
//  Form->BorderStyle = bsNone;
  TPanel *Panel = new TPanel( Form );
  Panel->Parent = Form;
  Panel->BorderWidth = BORDER_WIDTH;
  Panel->Align = alClient;

  TrackBar = new TTrackBar ( Panel );
  TrackBar->Parent = Panel;
  TrackBar->Max = 1000;
  TrackBar->Min = 0;
  TrackBar->Frequency = 50;
  TrackBar->TabStop = false;

  FTrackBarHeight = TrackBar->Height - 10;

  TrackBar->Align = alClient;

  TrackBar->ThumbLength = 15;
  TrackBar->TickMarks = tmTopLeft;

  SetButtonHandle();

  Cursor = crArrow;

  Value = 0;

  Increment = 1;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::KeyDown(Word &Key, Classes::TShiftState Shift)
{
  if( Key == VK_UP )
    {
    FUpDown->OnClick( this, Comctrls::btNext );
    Key = 0;
    }

  else if( Key == VK_DOWN )
    {
    FUpDown->OnClick( this, Comctrls::btPrev );
    Key = 0;
    }

  inherited::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetTrackBarOrientation ( TTrackBarOrientation Orientation )
{
  if ( FTrackBarOrientation != Orientation )
    {
    FTrackBarOrientation = Orientation;
    SetButtonHandle();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetButtonHandle()
{
  TrackBar->Orientation = TrackBarOrientation;
  if ( FTrackBarOrientation == trVertical )
    {

#if ( __BORLANDC__ >= 0x0630 )
    FSpeedButton->Glyph->Handle = LoadBitmap( HInstance, TEXT("DropLeft"));
#else
    FSpeedButton->Glyph->Handle = LoadBitmap((void*) HInstance, TEXT("DropLeft"));
#endif
    Form->Width = FTrackBarHeight;
    Form->Height = FTrackBarWidth;
    }

  else
    {
#if ( __BORLANDC__ >= 0x0630 )
    FSpeedButton->Glyph->Handle = LoadBitmap( HInstance, TEXT("DropDown"));
#else
    FSpeedButton->Glyph->Handle = LoadBitmap((void*) HInstance, TEXT("DropDown"));
#endif
    Form->Height = FTrackBarHeight;
    Form->Width = FTrackBarWidth;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::CreateParams(TCreateParams &Params)
{
  TCustomEdit::CreateParams(Params);
  //Params->Style &= ~WS_BORDER;
  Params.Style |=  /*ES_MULTILINE |*/ WS_CLIPCHILDREN;
//  Params.Style &= ~WS_TABSTOP;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::CreateWnd()
{
  TCustomEdit::CreateWnd();             
  SetEditRect();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetEditRect(void)
{
  TRect Loc;

  SendMessage(Handle, EM_GETRECT, 0, long(&Loc));
  Loc.Bottom = ClientHeight + 1;  // +1 is workaround for windows paint bug
  Loc.Right = ClientWidth; //- FUpDown->Width - 2;
  Loc.Top = 0;
  Loc.Left = 0;
  SendMessage(Handle, EM_SETRECTNP, 0, long(&Loc));
  SendMessage(Handle, EM_GETRECT, 0, long(&Loc));  // debug
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::WMSize(TWMSize &Message)
{
  int MinHeight;

  MinHeight = GetMinHeight();
    // text edit bug: if size to less than minheight, then edit ctrl does
    //  not display the text
  if (Height < MinHeight)
    Height = MinHeight;

  else if (FUpDown && FSpeedButton )
    {
    SetEditRect();
    };
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::WMUpdateMe(TMessage &Message)
{
  FUpDown->Position = 50;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::UpDownClick(TObject *Sender, TUDBtnType Button)
{
  SetFocus ();

  // Fixed by Mateo Zanotelli
  if ( Button == Comctrls::btNext )
    {
    if ( Value >= MaxValue - Increment )
      {
      if ( FWrap )
        Value = MinValue;

      else
        Value = MaxValue;
      }
      
    else
      Value = Value + Increment;
/*
    Value = Value + Increment;
    if ( Value >= MaxValue - Increment )
      {
      if ( FWrap )
        Value = MinValue;

      else
        Value = MaxValue;
      }
*/
    }

  else
    {
    if ( Value <= MinValue + Increment )
      {
      if ( FWrap )
        Value = MaxValue;

      else
        Value = MinValue;
      }
      
    else
      Value = Value - Increment;
    }

  PostMessage( Handle, WM_NEEDS_UPDATE, 0, 0 );
  if (AutoSelect && !(ControlState.Contains(csLButtonDown)))
    SelectAll();
    
  ProgressBar->Invalidate ();
}
//---------------------------------------------------------------------------
class TMyTrackBar : public TTrackBar
{
public :
  __property MouseCapture;
};
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::OnFormShow(TObject *Sender)
{
  TRect Rect;

  TrackBar->Perform ( TBM_GETTHUMBRECT, 0, (int)&Rect );
  if ( FTrackBarOrientation == trVertical )
    Form->Top = MoveFormToPoint.y - Rect.Top - ( Rect.Height() / 2 ) - BORDER_WIDTH;

  else
    Form->Left = MoveFormToPoint.x - Rect.Left - ( Rect.Width() / 2 ) - BORDER_WIDTH;

//  FSpeedButton->Enabled = false;
  FSpeedButton->GroupIndex = 333553;
  FSpeedButton->Down = true;

//  Form->MouseCapture = true;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SpeedButtonMouseMove(System::TObject* Sender,
      TShiftState Shift, int X, int Y)
{
  if ( ! Form->Visible )
    return;

  TPoint MousePoint = FSpeedButton->ClientToScreen ( ::Point(X,Y));
  
  TRect Rect;
  TrackBar->Perform ( TBM_GETCHANNELRECT, 0, (int)&Rect );
  TPoint SliderLeftTop;
  TPoint SliderRightBottom;

  TRect Rect1;
  TrackBar->Perform ( TBM_GETTHUMBRECT, 0, (int)&Rect1 );

  long SliderSize;
  long Position;
  if ( FTrackBarOrientation == trVertical )
    {
    SliderLeftTop = Form->ClientToScreen ( ::Point ( Rect.Top, Rect.Left ));
    SliderRightBottom = Form->ClientToScreen ( ::Point ( Rect.Bottom, Rect.Right ));
    
    int TumbHeight = Rect1.Height();
    SliderSize = SliderRightBottom.y - SliderLeftTop.y - TumbHeight;
    Position = MousePoint.y - SliderLeftTop.y - TumbHeight;
    Position = SliderSize - Position;
    }

  else
    {
    SliderLeftTop = Form->ClientToScreen ( ::Point ( Rect.Left, Rect.Top ));
    SliderRightBottom = Form->ClientToScreen ( ::Point ( Rect.Right, Rect.Bottom ));
    
    int TumbWidth = Rect1.Width();
    SliderSize = SliderRightBottom.x - SliderLeftTop.x - TumbWidth;
    Position = MousePoint.x - SliderLeftTop.x - TumbWidth;
    }

  if ( Position > SliderSize )
    Position = SliderSize;

  if ( Position < 0 )
    Position = 0;

  double V = ( Position * ( FMaxValue - MinValue )) / SliderSize + MinValue;

  if ( IncrementGauge > 0 )
    V = IncrementGauge * Round( V / IncrementGauge );

  Value = V;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SpeedButtonMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if ( Button != mbLeft )
    return;
    
  MoveFormToPoint = FSpeedButton->ClientToScreen ( ::Point(X,Y));

  if ( FTrackBarOrientation == trVertical )
    {
    TPoint Pos = FPanel->ClientToScreen ( Point ( FSpeedButton->Left + FSpeedButton->Width, FSpeedButton->Top ));
    Form->Left = Pos.x;
    Form->Top = Pos.y + FSpeedButton->Height;
    }

  else
    {
    TPoint Pos = FPanel->ClientToScreen ( Point ( FSpeedButton->Left, FSpeedButton->Top ));
    Form->Left = Pos.x;
    Form->Top = Pos.y + FSpeedButton->Height;
    }

  UpdateFormView();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::UpdateFormView()
{
  double Diff = MaxValue - MinValue;
  if ( Diff <= 0 )
    Diff = 100;

  Longint FillSize = ( Value - MinValue ) * 1000 / Diff;//SolveForX(PercentDone, W);

  if (FillSize > 1000 )
     FillSize = 1000;

  if ( FTrackBarOrientation == trVertical )
    {
    TrackBar->Position = TrackBar->Max - FillSize;
    Form->Height = FTrackBarWidth;
    }

  else
    {
    TrackBar->Position = FillSize;
    Form->Width = FTrackBarWidth;
    }

  SetFocus ();
  SelLength = 0;
  Form->Visible = true;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SpeedButtonMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if ( Button != mbLeft )
    return;
    
  Form->Visible = false;
//  FSpeedButton->Enabled = true;
  FSpeedButton->Down = false;
  FSpeedButton->GroupIndex = 0;
  FSpeedButton->Enabled = false;
  FSpeedButton->Enabled = true;
  if ( AutoSelect && !( ControlState.Contains(csLButtonDown)))
    SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::Change(void)
{
//  ProgressBar->Invalidate ();
  ProgressBar->Repaint ();
  // Improved by Mauro Venturini
  if ((! ComponentState.Contains(csLoading )) && OnChange )
    OnChange( this );
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::CMEnter(TWMNoParams &Message)
{
  if (AutoSelect && !(ControlState.Contains(csLButtonDown)))
    SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::CMExit(TWMNoParams &Message)
{
  if (CheckValue (Value) != Value)
    SetValue (CheckValue (Value));
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::WMPaste(TWMNoParams &Message)
{
  if (!FEditorEnabled || ReadOnly)
    return;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::WMCut(TWMNoParams &Message)
{
  if (!FEditorEnabled || ReadOnly)
    return;
}
//---------------------------------------------------------------------------
int __fastcall TBMSpinEdit::GetMinHeight(void)
{
  HDC DC;
  HFONT SaveFont;
  int I, Result;
  TTextMetric SysMetrics, Metrics;

  DC = GetDC(NULL);
  GetTextMetrics(DC, &SysMetrics);
  SaveFont = (HFONT)SelectObject(DC, Font->Handle);
  GetTextMetrics(DC, &Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I = SysMetrics.tmHeight;
  if (I > Metrics.tmHeight)
    I = Metrics.tmHeight;

  Result = Metrics.tmHeight + I / 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
  return Result + GaugeHeight + 1;
}
//---------------------------------------------------------------------------
double __fastcall TBMSpinEdit::GetValue(void)
{
  if ( Text.IsEmpty() || Text == "-" )
    {
    Text = "0";
    return 0.0;
    }

  double Result;
  try {
    Result = Text.ToDouble();
    }
  catch(...) {
  	Text=AnsiString((double)MinValue);
    return MinValue;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetValue(double NewValue)
{
  if ( NewValue == Value )
    return;

  AnsiString Format = "0.";
  for ( int i = 0; i < FPrecision; i ++ )
    Format += "0";

  Text = AnsiString::FormatFloat( Format, (double )CheckValue(NewValue));
  ProgressBar->Repaint ();
//  ProgressBar->Invalidate ();
  if ( Form->Visible )
    UpdateFormView();

  // Improved by Mauro Venturini
  if ((! ComponentState.Contains(csLoading )) && OnChange )
    OnChange( this );
}
//---------------------------------------------------------------------------
double __fastcall TBMSpinEdit::CheckValue(double NewValue)
{
  double Result;
  Result = NewValue;
  if (MaxValue != MinValue)
    {
    if (NewValue < MinValue)
      Result = MinValue;

    else if (NewValue > MaxValue)
      Result = MaxValue;
    }

//  Result = ((double)((long)( pow( 10, FPrecision ) * Result ))) / pow( 10, FPrecision );
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetMaxValue ( double _Value )
{
  if ( FMaxValue != _Value )
    {
    FMaxValue = _Value;
    if ( Value > FMaxValue )
      Value = FMaxValue;
      
    ProgressBar->Repaint ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetMinValue ( double _Value )
{
  if ( FMinValue != _Value )
    {
    FMinValue = _Value;
    if ( Value < FMinValue )
      Value = FMinValue;

    ProgressBar->Repaint ();
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetGaugeMaxValue ( double _Value )
{
  if ( FGaugeMaxValue != _Value )
    {
    FGaugeMaxValue = _Value;
    ProgressBar->Repaint ();
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetGaugeMinValue ( double _Value )
{
  if ( FGaugeMinValue != _Value )
    {
    FGaugeMinValue = _Value;
    ProgressBar->Repaint ();
    }

}
//---------------------------------------------------------------------------
void __fastcall   TBMSpinEdit::SetGaugeAroundCenter( double _Value )
{
  if( ProgressBar->GaugeAroundCenter != _Value )
    {
    ProgressBar->GaugeAroundCenter = _Value;
    ProgressBar->Repaint();
    }
}
//---------------------------------------------------------------------------
double __fastcall TBMSpinEdit::GetGaugeAroundCenter()
{
  return ProgressBar->GaugeAroundCenter;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetIncrementGauge ( double Value )
{
  ProgressBar->Increment = Value;
}
//---------------------------------------------------------------------------
double __fastcall TBMSpinEdit::GetIncrementGauge ()
{
  return ProgressBar->Increment;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetIncrement ( double Value )
{
  FIncrement = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::CreateHandle(void)
{
  inherited::CreateHandle();
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetTrackBarEnabled( bool Value )
{
  if ( Value != FTrackBarEnabled )
    {
    FTrackBarEnabled = Value;
    FSpeedButton->Visible = Value;
    SetBounds(Left, Top, Width, Height);
    SetEditRect();
    FSpeedButton->Visible = Value;
    FSpeedButton->Refresh();
    Invalidate();                        
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetBounds(int ALeft, int ATop, int AWidth, int AHeight)
{
  if ( FPanel )
    {
    int FSpW = ( FSpeedButton->Visible ) ? (FSpeedButton->Width) : 0;       // <-- KLE 10.03.2001
    
    FPanel->SetBounds ( AWidth - 4 - FUpDown->Width - FSpW - 3, 0, FUpDown->Width + FSpeedButton->Width + 3, AHeight - 3 - GaugeHeight - 1 );
    FUpDown->Height = FPanel->Height - 2;
    FSpeedButton->Height = FPanel->Height - 2;
    }

  if ( ProgressBar )
    ProgressBar->SetBounds ( 0, AHeight - 4 - GaugeHeight - 1, AWidth - 4, GaugeHeight + 2 );

  inherited::SetBounds ( ALeft, ATop, AWidth, AHeight );
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetGaugeBeginColor( TColor Value )
{
  ProgressBar->GaugeBeginColor = Value;
}
//---------------------------------------------------------------------------
TColor __fastcall TBMSpinEdit::GetGaugeBeginColor()
{
  return ProgressBar->GaugeBeginColor;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetGaugeEndColor( TColor Value )
{
  ProgressBar->GaugeEndColor = Value;
}
//---------------------------------------------------------------------------
TColor __fastcall TBMSpinEdit::GetGaugeEndColor()
{
  return ProgressBar->GaugeEndColor;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetTrackBarWidth ( int Value )
{
  if ( Value < 50 )
    Value = 50;
    
  if ( Value > 300 )
    Value = 300;
    
  FTrackBarWidth = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetPrecision ( int _Value )
{
  if ( _Value < 0 )
    _Value = 0;

  if ( _Value > 6 )
    _Value = 6;

  if ( FPrecision != _Value )
    {
    FPrecision = _Value;
    SetValue( Value );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSpinEdit::SetGaugeHeight ( int Value )
{
  if( FGaugeHeight != Value )
    {
    FGaugeHeight = Value;
    SetBounds( Left, Top, Width, Height ); 
    }
    
}
//---------------------------------------------------------------------------
namespace Bmspinedit
{
        void __fastcall PACKAGE Register()
        {
                 TComponentClass classes[1] = {__classid(TBMSpinEdit)};
                 RegisterComponents("BMitov", classes, 0);
        }

}
//---------------------------------------------------------------------------
