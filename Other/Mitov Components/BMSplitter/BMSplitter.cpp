//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#include <vcl/comctrls.hpp>
#include <stdlib.h>
#include <assert.h>

#pragma hdrstop

#include "BMSplitter.h"
#include "BMMessageHook.h"

//---------------------------------------------------------------------------
#if (__BORLANDC__ >= 0x0530)
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TBMSplitter *)
{
    new TBMSplitter (NULL);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMSplitter::TBMSplitter(TComponent* _Owner)
    : inherited(_Owner),
    MouseIsDown ( false ),
    FResizeStyle(rsLine)
{
  FButtonWidth = 40;
  SpeedButton = NULL;
  RestorePromile = 500;

  MessageHook = new TBMMessageHook ( this );
  MessageHook->OnSendMessage = OnOwnerSendMessage;
  MessageHook->OnGetPostedMessage = OnOwnerSendMessage;

  BevelOuter = bvNone;
  FMinPercent = 5;
  FMaxPercent = 95;
  FSplitterBevelWidth = 1;

  OldRight = 0;
  OldBottom = 0;

  FInitialPercentEnabled = false;
  FInitialPercent = 50;

  FPromile = FInitialPercent * 10;

  FShowPercentHint=true;
  HintWindow = NULL;
  OldAlign = Align;
  FOwner = NULL;
  FSplitterWidth = 4;
  FHintX = 5;
  FHintY = 5;
  FAutoSelectCursor = true;

/*
  FirstPanel = new TPanel (this);
  FirstPanel->Parent = this;
  FirstPanel->Align = alClient;
  FirstPanel->BevelOuter = bvNone;
  FirstPanel->BorderWidth = 0;
*/
  ControlPanel = new TPanel (this);
  ControlPanel->Parent = this;
  ControlPanel->Align = alRight;
  ControlPanel->Width = FSplitterWidth;

  FSplitterBevel = bvLowered;
  ControlPanel->BevelOuter = FSplitterBevel;
  ControlPanel->BevelWidth = FSplitterBevelWidth;

  ControlPanel->BevelInner = bvNone;
  ControlPanel->BorderWidth = 0;

  ControlPanel->OnMouseDown = ControlMouseDown;
  ControlPanel->OnMouseUp = ControlMouseUp;
  ControlPanel->OnMouseMove = ControlMouseMove;
  ControlPanel->ControlStyle = ControlPanel->ControlStyle >> csAcceptsControls;

  SetKind ( splkLeft );
}
//---------------------------------------------------------------------------
__fastcall TBMSplitter::~TBMSplitter(void)
{
  delete MessageHook;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetKind ( TBMSplitterType _Kind )
{
  FKind = _Kind;
  switch ( FKind )
    {
    default :   
        assert ( 0 );

    case splkLeft   :
      Align = alLeft;
      KindOf = koLeftRight;
      break;

    case splkRight  :
      Align = alRight;
      KindOf = koLeftRight;
      break;

    case splkTop    :
      Align = alTop;
      KindOf = koTopBottom;
      break;

    case splkBottom :
      Align = alBottom;
      KindOf = koTopBottom;
      break;
    }

  FixCursor();
  FixControl();
  Calibrate();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetSplitterBevelWidth ( int _SplitterBevelWidth )
{
  FSplitterBevelWidth = _SplitterBevelWidth;
  ControlPanel->BevelWidth = FSplitterBevelWidth;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetSplitterBevel ( TPanelBevel _FSplitterBevel )
{
  FSplitterBevel = _FSplitterBevel;
  ControlPanel->BevelOuter = FSplitterBevel;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::ControlMouseDown(TObject *Sender, TMouseButton Button,
    TShiftState Shift, int X, int Y)
{
  if ( Button != mbLeft )
    return;

  MouseIsDown = true;
  AbsSplitterPos = ClientToScreen( Point( ControlPanel->Left, ControlPanel->Top ));

  AbsMousePos = ControlPanel->ClientToScreen( ::Point( X,Y ));

  DrawSplitter();

  vShowHint ( X, Y );
  
  RestorePromile = FPromile;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetCollapsed( bool Value )
{
//  if ( ! FCanCollapse )
//    return;

  if ( FCollapsed != Value )
    {
    FCollapsed = Value;
    if ( FCollapsed )
      {
      RestorePromile = FPromile;

      if ( KindOf == koLeftRight )
        Width = FSplitterWidth;

      else
        Height = FSplitterWidth;
      }

    else
      {
      if( FCanCollapse || CollapseButton )
        {
        FPromile = RestorePromile;
        SplitterResize();
        }
      }
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetCanCollapse( bool Value )
{
  if ( FCanCollapse != Value )
    {
    FCanCollapse = Value;
    if ( ! FCanCollapse )
      SetCollapsed( false );
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetButtonWidth( int Value )
{
  if ( FButtonWidth != Value )
    {
    FButtonWidth = Value;
    SetButtonSize();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::OnButtonClick( TObject *Sender )
{
  Collapsed = ! Collapsed; 
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetCollapseButton( bool Value )
{
  if ( GetCollapseButton() != Value )
    {
    if ( Value )
      {
      SpeedButton = new TSpeedButton( this );
      SpeedButton->Parent = ControlPanel;
      SpeedButton->Cursor = crHandPoint;
      SpeedButton->OnClick = OnButtonClick;
      SetButtonSize();
      }

    else
      {
      delete SpeedButton;
      SpeedButton = NULL;
      }
    }

}
//---------------------------------------------------------------------------
bool __fastcall TBMSplitter::GetCollapseButton()
{
  return ( SpeedButton != NULL );
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::HideHint ()
{
  if ( HintWindow )
    {
    HintWindow->ReleaseHandle ();
    delete HintWindow;
    HintWindow = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::vShowHint ( int X, int Y )
{
  if ( ! FShowPercentHint )
    return;

  bool FlCollapsing = false;

  HintWindow = new THintWindow( this );

  TPoint Pt = GetPoint ();

  int XX = ControlPanel->ClientOrigin.x + X - Parent->ClientOrigin.x - ParentRect.Left;
  if ( XX > MainWidth )
    XX = MainWidth;

  if ( XX < ParentRect.Left )
    XX = ParentRect.Left;

  int YY = ControlPanel->ClientOrigin.y + Y - Parent->ClientOrigin.y - ParentRect.Top;

  if ( YY > MainHeight )
    YY = MainHeight;

  if ( YY < ParentRect.Top )
    YY = ParentRect.Top;

  int x;

  if ( KindOf == koLeftRight )
    {
    if ( Pt.x == -1 )
      {
      FlCollapsing = true;
      Pt.x = 0;
      }

    XX = Pt.x;
    x = ( XX - ParentRect.Left ) * 100 / ( MainWidth - FSplitterWidth );
    XX += FSplitterWidth;
    }

  else
    {
    if ( Pt.y == -1 )
      {
      FlCollapsing = true;
      Pt.y = 0;
      }

    YY = Pt.y;
    x = ( YY - ParentRect.Top ) * 100 / ( MainHeight - FSplitterWidth );
    YY += FSplitterWidth;
    }

  if ( x < 0 )
    x = 0;

  if ( x > 100 )
    x = 100;

  String HintText = ( FlCollapsing ) ? (String)"Hide" : (String)x + " %";

  TRect Rect;
  Rect.Left = XX + Parent->ClientOrigin.x + FHintX;
  Rect.Top = YY + Parent->ClientOrigin.y + FHintY;

  Rect.Right = Rect.Left + HintWindow->Canvas->TextWidth ( HintText ) + 4;
  Rect.Bottom = Rect.Top + HintWindow->Canvas->TextHeight ( HintText ) + 2;

  HintWindow->ActivateHint( Rect, HintText );
}
//---------------------------------------------------------------------------
TPoint __fastcall TBMSplitter::GetPoint ()
{
  TPoint Pt = Parent->ScreenToClient( AbsSplitterPos );

  if( KindOf == koLeftRight )
    {
    if( Pt.x < ParentRect.Left + ( FMinPercent * ( MainWidth - SplitterWidth ) / 100 ) + 1 )
      {
      if ( FCanCollapse || CollapseButton )
        Pt.x = -1; // Collapsing.

      else
        Pt.x = ParentRect.Left + ( FMinPercent * ( MainWidth - SplitterWidth ) / 100 ) + 1;
      }

    if( Pt.x > ParentRect.Left + ( FMaxPercent * ( MainWidth - SplitterWidth ) / 100 ) + 1 )
      {
      if ( FMaxPercent == 100 )
        Pt.x = MainWidth - SplitterWidth;

      else
        Pt.x = ParentRect.Left + ( FMaxPercent * ( MainWidth - SplitterWidth ) / 100 ) + 1;
      }
    }

  else
    {
    if( Pt.y < ParentRect.Top + ( FMinPercent * ( MainHeight - SplitterWidth ) / 100 ) + 1 )
      {
      if( FCanCollapse || CollapseButton )
        Pt.y = -1; // Collapsing.

      else
        Pt.y = ParentRect.Top + ( FMinPercent * ( MainHeight - SplitterWidth ) / 100 ) + 1;
      }

    if ( Pt.y > ParentRect.Top + ( FMaxPercent * ( MainHeight - SplitterWidth ) / 100 ) + 1 )
      {
      if ( FMaxPercent == 100 )
        Pt.y = MainHeight - SplitterWidth;

      else
        Pt.y = ParentRect.Top + ( FMaxPercent * ( MainHeight - SplitterWidth ) / 100 ) + 1;
      }
    }

  return Pt;
}
//---------------------------------------------------------------------------
////////////////////////////////////////////////////////////////////////////////
// DrawSplitter: draws the splitter when splitting

void __fastcall TBMSplitter::DrawSplitter()
{
  if ( FResizeStyle != rsLine && FResizeStyle != rsPattern )
    return;
    
  assert ( HintWindow == 0 );

  HDC DC = GetDCEx(Parent->Handle, 0,
               DCX_CACHE|DCX_CLIPSIBLINGS|DCX_LOCKWINDOWUPDATE);

  TCanvas *MCanvas = new TCanvas;
  MCanvas->Handle = DC;
  MCanvas->Pen->Style = psClear; //psSolid;
  MCanvas->Brush->Color = clBlack;
  MCanvas->Pen->Mode = pmNotXor;
  if( FResizeStyle == rsPattern )
    MCanvas->Brush->Bitmap = AllocPatternBitmap( clBlack, clWhite );

  TPoint Pt = GetPoint ();
  if ( Pt.x == -1 )
    Pt.x = 0;

  if ( Pt.y == -1 )
    Pt.y = 0;
    
  MCanvas->Rectangle( ::Rect( Pt.x, Pt.y, Pt.x + ControlPanel->Width, Pt.y + ControlPanel->Height ));

  delete MCanvas;

  ReleaseDC ( Parent->Handle, DC );
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::ControlMouseUp(TObject *Sender, TMouseButton Button,
    TShiftState Shift, int X, int Y)
{
  if ( Button != mbLeft )
    return;

  if ( MouseIsDown )
    {
    HideHint ();
    DrawSplitter();

    ResizeToSplitter();
    MouseIsDown = false;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::ResizeToSplitter()
{
  TPoint Pt = GetPoint ();
  FCollapsed = false;
  if ( Pt.x == -1 )
    {
    FCollapsed = true;
    Pt.x = 0;
    }

  if ( Pt.y == -1 )
    {
    FCollapsed = true;
    Pt.y = 0;
    }


  switch ( Align )
    {
    case alBottom :
      Height = ParentRect.Bottom - Pt.y;
      break;

    case alTop :
      Height = Pt.y - ParentRect.Top + SplitterWidth;
      break;

    case alLeft :
      {
      Width = Pt.x - ParentRect.Left + SplitterWidth;
      break;
      }

    case alRight :
      Width = ParentRect.Right - Pt.x;
      break;

    default :
      assert ( 0 );

    }

  CalculatePromile();

  CheckMinMaxPercent ();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::CalculatePromile()
{
  if ( FCollapsed && ( FCanCollapse || CollapseButton ) )
    return;

  GetParentSize();

  if( ComponentState.Contains( csLoading ))
    return;

  if( MainWidth == 0 || MainHeight == 0 )
    return;

  FPromile = ( KindOf == koLeftRight ) ?
    ( Width  - SplitterWidth ) * 1000 / ( MainWidth  - SplitterWidth ):
    ( Height - SplitterWidth ) * 1000 / ( MainHeight - SplitterWidth );

}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SplitterResize()
{
  if ( FCollapsed && ( FCanCollapse || CollapseButton ))
    {
    SetButtonSize();
    return;
    }
    
  if ( KindOf == koLeftRight )
    {
    int NewWidth = ( ( MainWidth - SplitterWidth ) * FPromile / 1000 ) + SplitterWidth;

    if ( NewWidth > MainWidth )
      NewWidth = MainWidth;

    if ( NewWidth < FSplitterWidth )
      NewWidth = FSplitterWidth;

    if ( Width != NewWidth )
      Width = NewWidth;

    }

  else
    {
    int NewHeight = ( ( MainHeight - SplitterWidth ) * FPromile / 1000 ) + SplitterWidth;

    if ( NewHeight > MainHeight )
      NewHeight = MainHeight;

    if ( NewHeight < FSplitterWidth )
      NewHeight = FSplitterWidth;

    if ( Height != NewHeight )
      Height = NewHeight;
    }

  SetButtonSize();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::WmInternal(TMessage& Msg)
{
  if( FOwner )
    {
    if( Align == alBottom )
      {
      if ( Top + Height == OldBottom )
        return;

      OldBottom = Top + Height;
      }

    else if( Align == alRight )
      {
      if( Left + Width == OldRight )
        return;

      OldRight = Left + Width;
      }

    GetParentSize();
    SplitterResize();
    }
}
//---------------------------------------------------------------------------
#if (__BORLANDC__ < 0x0530)
void __fastcall TBMSplitter::EraseBkg(TMessage& Msg)
{
  if ( ComponentState.Contains ( csDesigning ))
    inherited::Dispatch( &Msg );

}
#endif
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::WmMoveSize (TMessage& Msg)
{
  PostMessage ( Handle, WM_TBMSplitterFIRSTMESSAGE, 0, 0 );
  inherited::Dispatch( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall PostToAll( TWinControl *Parent )
{
  for( int i = 0; i < Parent->ControlCount; i ++ )
    {
    TWinControl *WinControl = dynamic_cast<TWinControl *>( Parent->Controls[ i ] );
    if ( WinControl )
      {
      if ( dynamic_cast<TBMSplitter *> ( WinControl ))
        {
        PostMessage ( WinControl->Handle, WM_TBMSplitterFIRSTMESSAGE, 0, 0 );
        }
        
      PostToAll( WinControl );
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::OnOwnerSendMessage ( System::TObject* Sender, TMessage &aMsg )
{
  switch ( aMsg.Msg )
    {
    case WM_SIZE:
      {
      if ( aMsg.WParam == SIZE_RESTORED )
        {
        if ( dynamic_cast <TScrollingWinControl *> ( FOwner ))
          {
          FOwner->Width = FOwner->Width - 1;
          FOwner->Width = FOwner->Width;
          }
        }

      TWinControl *MainParent;
      for ( MainParent = Parent; MainParent->Parent; MainParent = MainParent->Parent )
        NULL;

      PostToAll( MainParent );
      break;
      }

    case WM_SHOWWINDOW:
      CheckAlign ();

/*
    case WM_EXITSIZEMOVE:
    case WM_NCCALCSIZE:

    case WM_VSCROLL:
    case WM_HSCROLL:
*/
      {
      PostMessage ( Handle, WM_TBMSplitterFIRSTMESSAGE, 0, 0 );
      }
    }
          
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::ControlMouseMove(TObject *Sender, TShiftState Shift,
    int X, int Y)
{
  TPoint NewAbsMousePos = ControlPanel->ClientToScreen ( ::Point( X, Y ));

  bool FlNeedRedraw;

  if ( MouseIsDown )
    {
    TPoint NewSplitterPos = AbsSplitterPos;

    if ( KindOf == koLeftRight )
      {
      NewSplitterPos.x =  NewAbsMousePos.x; //AbsOrigPos; // + X;
      FlNeedRedraw = ( AbsMousePos.x != NewAbsMousePos.x );
      }

    else
      {
//      NewSplitterPos.y = ScreenToClient ( TPoint ( 0, AbsOrigPos )).y; // + Y;
      NewSplitterPos.y = NewAbsMousePos.y; //AbsOrigPos; // + Y;
      FlNeedRedraw = ( AbsMousePos.y != NewAbsMousePos.y );
      }

    if ( AbsMousePos.x != NewAbsMousePos.x || AbsMousePos.y != NewAbsMousePos.y )
      {
      HideHint ();
      if ( FlNeedRedraw )
        DrawSplitter();   // Undraw the old splitter

      AbsSplitterPos = NewSplitterPos;

      if ( FlNeedRedraw )
        {
        DrawSplitter();   // Draw the new one
        if ( FResizeStyle == rsUpdate )
          {
          ResizeToSplitter();
          AbsSplitterPos = ClientToScreen( ::Point( ControlPanel->Left, ControlPanel->Top ));
          }
        }

      vShowHint ( X, Y );
      }
    }

  AbsMousePos = NewAbsMousePos;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::CheckAlign ()
{
  if ( Align != OldAlign )
    {
    OldAlign = Align;
    FixAlign ();
    FixControl ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::WndProc(Messages::TMessage &Message)
{
  if ( Message.Msg == WM_PAINT )
    {
    CheckAlign ();
    }

  inherited::WndProc( Message );
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::FixControl ()
{
  switch ( Align )
    {
    case alRight :
      ControlPanel->Align = alLeft;
      break;

    case alTop :
      ControlPanel->Align = alBottom;
      break;

    case alBottom :
      ControlPanel->Align = alTop;
      break;

    case alLeft :
      ControlPanel->Align = alRight;
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetButtonSize()
{
  if ( ! SpeedButton )
    return;

  if ( KindOf == koLeftRight )
    {
    SpeedButton->Left = 0;
    SpeedButton->Top = ( Height - FButtonWidth ) / 2;
    SpeedButton->Width = FSplitterWidth;
    SpeedButton->Height = FButtonWidth;
    }

  else
    {
    SpeedButton->Top = 0;
    SpeedButton->Left = ( Width - FButtonWidth ) / 2;
    SpeedButton->Width = FButtonWidth;
    SpeedButton->Height = FSplitterWidth;
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetSplitterWidth ( int Value )
{
  FSplitterWidth = Value;

  if ( KindOf == koLeftRight )
    ControlPanel->Width = FSplitterWidth;

  else
    ControlPanel->Height = FSplitterWidth;

  SetButtonSize();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::FixAlign ()
{
  switch ( Align )
    {
    default :
      Align = alLeft;
      
    case alLeft :
      SetKind ( splkLeft );
      break;

    case alRight :
      SetKind ( splkRight );
      break;

    case alTop :
      SetKind ( splkTop );
      break;
      
    case alBottom :
      SetKind ( splkBottom );
      break;
    }

  FixCursor ();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::FixCursor ()
{
  if ( KindOf == koLeftRight )
    ControlPanel->Width = FSplitterWidth;

  else
    ControlPanel->Height = FSplitterWidth;

  if ( AutoSelectCursor )
    ControlPanel->Cursor = ( KindOf == koLeftRight ) ? crHSplit : crVSplit;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetAutoSelectCursor ( bool Value )
{
  FAutoSelectCursor = Value;
  FixCursor ();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetInitialPercentEnabled ( bool _Enabled )
{
  FInitialPercentEnabled = _Enabled;
  if ( ComponentState.Contains ( csDesigning ))
    Calibrate();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetInitialPercent ( int _InitialPercent )
{
  if ( _InitialPercent < 0 || _InitialPercent > 100 )
    return;

  if ( _InitialPercent < FMinPercent || _InitialPercent > FMaxPercent )
    return;
    
  FInitialPercent = _InitialPercent;
  if ( ComponentState.Contains ( csDesigning ))
    Calibrate();

}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetBounds(int ALeft, int ATop, int AWidth, int AHeight)
{
  inherited::SetBounds( ALeft, ATop, AWidth, AHeight );
  SetButtonSize();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::Calibrate()
{
  if ( FInitialPercentEnabled )
    {
    FPromile = FInitialPercent * 10;
    if( FCollapsed )
      RestorePromile = FPromile;

    SplitterResize();
    }

  else
    {
    CalculatePromile();
    SetButtonSize();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::CreateWnd(void)
{
  inherited::CreateWnd();
  FOwner = Parent;
  GetParentSize ();

  Calibrate();

  MessageHook->HookedTo = FOwner;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::GetParentSize()
{
  GetParentRectangle();
  CalculateSize();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::CalculateSize()
{
  switch ( Align )
    {
    default :
      Align = alLeft;

    case alLeft :
      ParentRect.Left = Left;
      break;

    case alRight :
      ParentRect.Right = Left + Width;
      break;

    case alTop :
      ParentRect.Top = Top;
      break;

    case alBottom :
      ParentRect.Bottom = Top + Height;
      break;
    }

  MainWidth = ParentRect.Right - ParentRect.Left;
  MainHeight = ParentRect.Bottom - ParentRect.Top;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::GetParentRectangle()
{
  if( ! FOwner )
    return;

  TCustomTabControl *TabControlOwner = dynamic_cast <TCustomTabControl *> ( FOwner );
  if ( TabControlOwner )
    {
    ParentRect = ((TTabControl *)TabControlOwner )->DisplayRect;
    return;
    }

  ParentRect = FOwner->ClientRect;

  TCustomPanel *PanelOwner = dynamic_cast <TCustomPanel *> ( FOwner );
  if ( PanelOwner )
    {
    int BevelSize = ((TPanel *)PanelOwner )->BorderWidth;
    if ( ((TPanel *)PanelOwner)->BevelOuter != bvNone )
      BevelSize += ((TPanel *)PanelOwner)->BevelWidth;


    if ( ((TPanel *)PanelOwner)->BevelInner != bvNone )
      BevelSize += ((TPanel *)PanelOwner)->BevelWidth;

    InflateRect ( (RECT *)( &ParentRect ), -BevelSize, -BevelSize );
    return;
    }

}
//---------------------------------------------------------------------------
/*
void __fastcall TBMSplitter::AdjustClientRect(Types::TRect &Rect)
{
  switch ( FKind )
    {
    default :
        assert ( 0 );

    case splkLeft :
      Rect.Right -= ControlPanel->Width;
      break;

    case splkRight :
      Rect.Left += ControlPanel->Width;
      break;

    case splkTop :
      Rect.Bottom -= ControlPanel->Height;
      break;

    case splkBottom :
      Rect.Top += ControlPanel->Height;
      break;
    }
}
*/
//---------------------------------------------------------------------------
/*
class   TTmpPanel : public TPanel
{
  friend TBMSplitter;
};
//---------------------------------------------------------------------------
TComponent* __fastcall TBMSplitter::GetChildOwner(void)
{
  return ((TTmpPanel *) FirstPanel )->GetChildOwner ();
}
//---------------------------------------------------------------------------
TComponent* __fastcall TBMSplitter::GetChildParent(void)
{
  return ((TTmpPanel *) FirstPanel )->GetChildParent ();
}
//---------------------------------------------------------------------------
#if (__BORLANDC__ < 0x0530)
//---------------------------------------------------------------------------
// BCB 1.0
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::GetChildren(TGetChildProc Proc)
{
  FirstPanel->GetChildren ( Proc );
}
#else
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::GetChildren(TGetChildProc Proc, TComponent* Root)
{
  ((TTmpPanel *) FirstPanel )->GetChildren ( Proc, Root );
}
#endif
//---------------------------------------------------------------------------
*/
void __fastcall TBMSplitter::SetMinPercent ( int newMinPercent )
{
  if ( newMinPercent < 0 || newMinPercent > 100 )
    return;

  if ( newMinPercent >= FMaxPercent )
    return;

  FMinPercent = newMinPercent;

  CheckMinMaxPercent ();
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::CheckMinMaxPercent ()
{
 if ( FPromile > ( FMaxPercent + 1 ) * 10 )
    {
    if ( KindOf == koLeftRight )
      Width = ( FMaxPercent + 1 ) * MainWidth / 100;

    else
      Height = ( FMaxPercent + 1 ) * MainHeight / 100;
    }

  if ( FPromile < FMinPercent * 10 )
    {
    if ( FCollapsed && ( FCanCollapse || CollapseButton ) )
      {
      if ( KindOf == koLeftRight )
        Width = FSplitterWidth;

      else
        Height = FSplitterWidth;
      }
      
    else
      {
      if ( KindOf == koLeftRight )
        Width = FMinPercent * MainWidth / 100;

      else
        Height = FMinPercent * MainHeight / 100;
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetMaxPercent ( int newMaxPercent )
{
  if ( newMaxPercent < 0 || newMaxPercent > 100 )
    return;
    
  if ( newMaxPercent <= FMinPercent )
    return;

  FMaxPercent = newMaxPercent;
  CheckMinMaxPercent ();
}
//---------------------------------------------------------------------------
TColor __fastcall TBMSplitter::GetSliderColor()
{
  return ControlPanel->Color;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetSliderColor( TColor Value )
{
  ControlPanel->Color = Value; 
}
//---------------------------------------------------------------------------
bool __fastcall TBMSplitter::GetSliderParentColor()
{
  return ControlPanel->ParentColor;
}
//---------------------------------------------------------------------------
void __fastcall TBMSplitter::SetSliderParentColor( bool Value )
{
  ControlPanel->ParentColor = Value;
}
//---------------------------------------------------------------------------
namespace Bmsplitter
{
    void __fastcall PACKAGE Register()
    {
        TComponentClass classes[1] = {__classid(TBMSplitter)};
        RegisterComponents("BMitov", classes, 0);
    }
}
//---------------------------------------------------------------------------
