//---------------------------------------------------------------------------
#include <vcl\vcl.h>
//#include <syncobjs.hpp>
#include <assert.h>
#include <stdlib.h>
#include <mmsystem.h>
#pragma hdrstop

#include "BMPAGE.h"

//#define __DEBUG_LOG

#if (__BORLANDC__ >= 0x0530)
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
#pragma package(smart_init)
//#pragma link "syncobjs"
//#pragma resource "*.dfm"
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------
#define TIMER_RESOLUTION 10   // expressed in milliseconds, binds timer
                               // to 0.1-second resolution
#define TIMER_INTERVAL 10   // expressed in milliseconds, binds timer
                             // to 0.1-second resolution


//---------------------------------------------------------------------------
#ifdef __DEBUG_LOG
void WriteLn ( String Text )
{
  static HANDLE handle = NULL;
  if ( ! handle )
    {
    AllocConsole ();
    handle = GetStdHandle ( STD_OUTPUT_HANDLE );
    }

  Text += "\n";
  WriteConsole ( handle, Text.c_str (), Text.Length (), 0, 0 );
}
  #define DEBUG_LOG_MSG(A)    WriteLn(A)
  
#else
  #define DEBUG_LOG_MSG(A)

#endif
//---------------------------------------------------------------------------

namespace
{
class TClientHdc
{
  HDC hdc;
  HWND Handle;

public :
  TClientHdc ( HWND _Handle ) : Handle ( _Handle ) { hdc = GetDC ( Handle ); }
  virtual ~TClientHdc () { ReleaseDC( Handle, hdc ); }

  operator HDC (){  return hdc; }
};

class TCaptureGraphicControl : public TGraphicControl
{
public :
  TCanvas * _fastcall GetCanvas () { return Canvas; }

 __fastcall virtual TCaptureGraphicControl(Classes::TComponent* AOwner ) : TGraphicControl ( AOwner ) {}
};
};

namespace Bmpage
{
/*
TBMWaveEditForm *BMWaveEditForm;
//---------------------------------------------------------------------------
__fastcall TBMWaveEditForm::TBMWaveEditForm(TComponent* Owner)
    : TForm(Owner)
{
}
*/
//---------------------------------------------------------------------------

      class TTempImageList : public TImageList
      {
      public :
        void __fastcall GetImages(int Index, Graphics::TBitmap* Image, Graphics::TBitmap* Mask)
          {
          TImageList::GetImages( Index, Image, Mask );
          }
      };
//---------------------------------------------------------------------------
const int DefaultTabWidth = 100;
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TBMPageControl *)
{
    new TBMPageControl(( TComponent * ) NULL);
}
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TBMTabSheet *)
{
    new TBMTabSheet(( TComponent * ) NULL);
}
//---------------------------------------------------------------------------
static unsigned int uTimerID = 0;
static TList    *BmTabsList;
//---------------------------------------------------------------------------
class TBMCriticalSection
{
protected :
  CRITICAL_SECTION csCriticalSection;

  int                   Entries;

public :
  TBMCriticalSection ();
  ~TBMCriticalSection ();

public :
  void Begin ();
  void End ();

  void Enter () { Begin (); }
  void Leave () { End ();   }
};
//---------------------------------------------------------------------------
TBMCriticalSection::TBMCriticalSection ()
{
  Entries = 0;
  InitializeCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
TBMCriticalSection::~TBMCriticalSection ()
{
  DeleteCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
void TBMCriticalSection::Begin ()
{
//  if ( ! Entries ++ )
    EnterCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
void TBMCriticalSection::End ()
{
//  if ( ! ( --Entries ))
    LeaveCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
TBMCriticalSection *Section;
//---------------------------------------------------------------------------
static void CALLBACK
timeoutCallback(unsigned int timerId,
                unsigned int msg,
                unsigned long timeoutFunction,
                unsigned long dw1,
                unsigned long dw2)
{
  uTimerID = -1;
  if ( ! Section )
    return;

  Section->Enter ();
  for ( int i = 0; i < BmTabsList->Count; i ++ )
    {
    ((TBMPageControl *)BmTabsList->Items [ i ])->UpdateHighlighted ();
    }
  Section->Leave ();

  uTimerID = timeSetEvent( TIMER_INTERVAL,
               TIMER_RESOLUTION,
               timeoutCallback,
               0,
               TIME_ONESHOT);
}
//---------------------------------------------------------------------------
class TLocalStarter
{
public :
  TLocalStarter ()
    {
    Section = new TBMCriticalSection;
    BmTabsList = new TList;
    uTimerID = timeSetEvent( TIMER_INTERVAL,
                  TIMER_RESOLUTION,
                  timeoutCallback,
                  0,
                  TIME_ONESHOT);
    }

  ~TLocalStarter ()
    {
    TBMCriticalSection *TmpPointer = Section;
    Section = NULL;
    if ( uTimerID )
      timeKillEvent( uTimerID );

    delete BmTabsList;

    delete TmpPointer;
    }
};
//---------------------------------------------------------------------------
TLocalStarter   __LocalStarter;
//---------------------------------------------------------------------------
__fastcall TBMPageControl::TBMPageControl(HWND ParentWindow) :
    Comctrls::TPageControl( ParentWindow)
{
}
//---------------------------------------------------------------------------
__fastcall TBMPageControl::TBMPageControl(TComponent* _Owner) :
    TPageControl(_Owner),
    FShowGlyphs ( true ),
    Glyphs ( NULL ),
    GrayedGlyphs ( NULL ),
    HighlightedGlyphs ( NULL ),
    SelectedHighlightedGlyphs ( NULL )
{
#ifndef __BCB_40__
  FStyle = tsTabs;
#endif

  FEnableSounds = true;
  AlreadyPlayngEntryFlag = false;

  FHighlightSound = NULL;
  FPulseSound = NULL;
  FSelectSound = NULL;

//  FHighlightWave = new TBMWaveData;
//  FPulseWave = new TBMWaveData;
//  FSelectWave = new TBMWaveData;

  FCloudSize = 0;
  FEnableFlash = false;
  Delay = 0;
  FMouseInside = false;

  FUnFlashedDelay = 200;
  FFlashingDelay = 6;
  FFlashedDelay = 6;

  FFlashing = true;

  FParentBackgroundBuffer = new Graphics::TBitmap;
  PackGroundUpdated = false;
  
  FCanvas = new TControlCanvas;
  Glyphs = new Controls::TImageList ( this );
  GrayedGlyphs = new Controls::TImageList ( this );
  HighlightedGlyphs = new Controls::TImageList ( this );
  SelectedHighlightedGlyphs = new Controls::TImageList ( this );
  TabBmp = new Graphics::TBitmap;


  OffsetBitmap = new Graphics::TBitmap;
  
  GrayedImage = new Graphics::TBitmap;
  GrayedImage->Width = 16;
  GrayedImage->Height = 16;
  GrayedImage->PixelFormat = pf32bit;

  HighlightedImage = new Graphics::TBitmap;
  HighlightedImage->Width = 16;
  HighlightedImage->Height = 16;
  HighlightedImage->PixelFormat = pf32bit;

  MaskBmp = new Graphics::TBitmap;
  MaskBmpBlack = new Graphics::TBitmap;
  ImageBmp = new Graphics::TBitmap;
//  FullPageBmp = new Graphics::TBitmap;

  MaskBmp->Width = 16;
  MaskBmp->Height = 16;

  ImageBmp->Width = 16;
  ImageBmp->Height = 16;

  MaskBmpBlack->Width = 16;
  MaskBmpBlack->Height = 16;

  DIBArray = new TColorRef [ 16 * 16 ];

  FTabColorization = tcPage;
  FCloudColorization = ccMain;
  FColorUnselected = clBtnShadow;
  FColorSelected = clBtnFace;

  FHighlitedItem = -1;

  FHotTrackColor = clHighlight;
  FHotTrackCloudColor = clYellow;

  FHotTrackTextStyle = dsRaised;
  FHotTrackGlyphtStyle = dsRaised;

  FGrayInactive = true;
  FHighlightGlyphts = true;

  Section->Enter ();
  BmTabsList->Add ( this );
  Section->Leave ();

  ControlStyle = ControlStyle >> csOpaque;
}

//---------------------------------------------------------------------------
__fastcall TBMPageControl::~TBMPageControl()
{
  Section->Enter ();
  BmTabsList->Remove ( this );
  Section->Leave ();
  if ( Glyphs )
    {
    delete Glyphs;
    Glyphs = NULL;
    }

  if ( GrayedGlyphs )
    {
    delete GrayedGlyphs;
    GrayedGlyphs = NULL;
    }

  if ( HighlightedGlyphs )
    {
    delete HighlightedGlyphs;
    HighlightedGlyphs = NULL;
    }

  if ( SelectedHighlightedGlyphs )
    {
    delete SelectedHighlightedGlyphs;
    SelectedHighlightedGlyphs = NULL;
    }

  delete FCanvas;
  delete TabBmp;
  delete OffsetBitmap;
  delete GrayedImage;
  delete HighlightedImage;

  delete DIBArray;

//  delete FullPageBmp;
  delete MaskBmpBlack;
  delete MaskBmp;
  delete ImageBmp;

  delete FParentBackgroundBuffer;

//  delete FSelectWave;
//  delete FPulseWave;
//  delete FHighlightWave;
}
//---------------------------------------------------------------------------
/*
void __fastcall TBMPageControl::SetBounds(int ALeft, int ATop, int AWidth, int AHeight)
{
  inherited::SetBounds(ALeft, ATop, AWidth, AHeight);

  FParentBackgroundBuffer->Width = AWidth;
  FParentBackgroundBuffer->Height = AHeight;
}
*/
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CreateParams(Controls::TCreateParams &Params)
{
  TPageControl::CreateParams( Params );

//  if ( FTabStyles.Contains ( tabButton ))
#ifndef __BCB_40__
  if ( Style != tsTabs )
    Params.Style |= TCS_BUTTONS;
#endif

  if ( FTabStyles.Contains ( tabRightJustify ))
    Params.Style |= TCS_RIGHTJUSTIFY;

//  if ( FTabStyles.Contains ( tabIconLeft ))
//    Params.Style |= TCS_FORCEICONLEFT;

//  if ( FTabStyles.Contains ( tabLabelLeft ))
//    Params.Style |= TCS_FORCELABELLEFT;

  Params.Style &= ( ~TCS_HOTTRACK );

//  if FHotTrack and (not (csDesigning in ComponentState)) then
//    Style = Style or TCS_HOTTRACK;

//  if ( FTabStyles.Contains ( tabOwnerDraw ))
  Params.Style |= TCS_OWNERDRAWFIXED;

}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CreateWnd()
{
  TPageControl::CreateWnd();

  if ( FShowGlyphs )
    Perform ( TCM_SETIMAGELIST, 0, Glyphs->Handle );
    
  UpdateGlyphs();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetShowGlyphs ( bool Value )
{
  if ( FShowGlyphs != Value )
    {
    FShowGlyphs = Value;
    RecreateWnd ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetTabStyles(TTabStyles Value)
{
  if ( FTabStyles != Value )
    {
    FTabStyles = Value;

    if ( FTabStyles.Contains ( tabRightJustify ))
      TPageControl::MultiLine = true;

//    if ( FTabStyles.Contains ( tabLabelLeft ))
//      FTabStyles << tabIconLeft;

//    if ( FTabStyles.Contains ( tabIconLeft ) && ( ! TabWidth ))
//      TabWidth = DefaultTabWidth;

    RecreateWnd ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::Notification(Classes::TComponent* AComponent, Classes::TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  if (Operation == opRemove )
    {
    if ( AComponent == FHighlightSound )
      SetHighlightSound ( NULL );

    if ( AComponent == FPulseSound )
      SetPulseSound ( NULL );

    if ( AComponent == FSelectSound )
      SetSelectSound ( NULL );
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::DestroyWnd(void)
{
  if ( ! TPageControl::MultiLine )
    FTabStyles >> tabRightJustify;

  TPageControl::DestroyWnd();
}
//---------------------------------------------------------------------------
TBMTabSheet* __fastcall TBMPageControl::GetPage(int Index)
{
  return (TBMTabSheet* ) TPageControl::Pages [ Index ];
}
//---------------------------------------------------------------------------
int  __fastcall TBMPageControl::GetPageCount ()
{
  int Count = 0;
  for ( int i = 0; i < PageCount; i ++ )
    if ( Pages [ i ]->TabVisible == true )
      Count ++;

  return Count;
}
//---------------------------------------------------------------------------
namespace
{

void __fastcall FullBroadcastIt ( TWinControl *Parent, Cardinal Msg, int WParam, int LParam )
{
  for ( int i = 0; i < Parent->ControlCount; i ++ )
    {
    TControl *Control = Parent->Controls [ i ];

    Control->Perform ( Msg, WParam, LParam );
    TWinControl *WinControl = dynamic_cast<TWinControl *> ( Control );
    if ( WinControl )
      FullBroadcastIt ( WinControl, Msg, WParam, LParam );
    }

}
//---------------------------------------------------------------------------
void __fastcall FullBroadcast ( TControl *TheControl, Cardinal Msg, int WParam, int LParam )
{
  TWinControl *Parent = TheControl->Parent;

  if ( ! Parent )
    {
    TWinControl *WinControl = dynamic_cast<TWinControl *> ( TheControl );
    if ( WinControl )
      FullBroadcastIt ( WinControl, Msg, WParam, LParam );

    return;
    }

  FullBroadcastIt ( Parent, Msg, WParam, LParam );
}
//---------------------------------------------------------------------------
class TParentControl : public TWinControl
{
  friend void __fastcall CopyParentImage( TControl *Control, TRect SelfR, int X, int Y, TWinControl *ParentControl, TCanvas *Dest );

  public :
    virtual void __fastcall PaintWindow(HDC DC) { TWinControl::PaintWindow (DC); }

};
//---------------------------------------------------------------------------
void __fastcall CopyParentImage( TControl *Control, TRect SelfR, int X, int Y, TWinControl *ParentControl, TCanvas *Dest )
{
  int I, Count, SaveIndex;
  HDC DC;
  RECT R, CtlR;

  if ( ParentControl == NULL || Control->Parent == NULL )
    return;

  Count = ParentControl->ControlCount;
  DC = Dest->Handle;
//  RECT SelfRComp = Bounds( Control->Left, Control->Top, Control->Width, Control->Height);

  TPoint  TopLeft = Control->Parent->ClientToScreen ( Point ( SelfR.Left, SelfR.Top ));
  TopLeft = ParentControl->ScreenToClient ( TopLeft );
//  OffsetRect ( &SelfRComp, TopLeft1.x, TopLef1.y );
  RECT SelfRComp = Bounds( TopLeft.x, TopLeft.y, Control->Width, Control->Height);

//  TPoint  TopLeft1 = Control->Parent->ClientToScreen ( ::Point ( 0, 0 ));
//  TPoint  TopLeft2 = ParentControl->Parent->ClientToScreen ( ::Point ( 0, 0 ));

//  OffsetRect ( &SelfRComp, TopLeft1.x - TopLeft2.x, TopLef1.y - TopLeft2.y );
 
//  Copy parent control image
/*
  TForm *From = dynamic_cast <TForm *> ( ParentControl );
  if ( From )
    {
    TColor OldColor = Dest->Brush->Color;
    Dest->Brush->Color = From->Color;
    Dest->FillRect (Rect ( 0, 0, Control->Width, Control->Height));
    Dest->Brush->Color = OldColor;
    }
*/
  SaveIndex = SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, NULL);

  IntersectClipRect(DC, 0, 0, ParentControl->ClientWidth,
    ParentControl->ClientHeight);
    
  ParentControl->Perform ( WM_ERASEBKGND, (int)DC, 0 );

/*
  TBMTabSheet *Sheet = dynamic_cast <TBMTabSheet *> ( ParentControl );
  if ( Sheet )
    Sheet->DrawBkg ( DC );
*/

/*
  HRGN hRgn = NULL; // = CreateRgn ();

  GetUpdateRgn ( ParentControl->Handle, hRgn, FALSE );
*/
  RECT UpdRect;

  GetUpdateRect( ParentControl->Handle, &UpdRect, FALSE );

  (( TParentControl *)ParentControl)->PaintWindow(DC);
//  ParentControl->Invalidate ();
  RestoreDC(DC, SaveIndex);

  InvalidateRect ( ParentControl->Handle, &UpdRect, TRUE );
  
//  Copy images of graphic controls
  for ( I = 0; I < Count; I ++ )
    {
    if ((ParentControl->Controls[I] != NULL) &&
      ! ( dynamic_cast<TWinControl* > ( ParentControl->Controls[I] ) ) )
      {
      if ( ParentControl->Controls[I] == Control )
        break;

      CtlR = Bounds(ParentControl->Controls[I]->Left, ParentControl->Controls[I]->Top, ParentControl->Controls[I]->Width, ParentControl->Controls[I]->Height);
      if ( IntersectRect(&R, &SelfRComp, &CtlR) && ParentControl->Controls[I]->Visible )
        {
        SaveIndex = SaveDC(DC);
        SetViewportOrgEx(DC, ParentControl->Controls[I]->Left + X, ParentControl->Controls[I]->Top + Y, NULL);
        IntersectClipRect(DC, 0, 0, ParentControl->Controls[I]->Width, ParentControl->Controls[I]->Height);
        ParentControl->Controls[I]->Perform( WM_PAINT, (int)DC, 0);
        RestoreDC(DC, SaveIndex);
        }
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall CopyParentImage( TControl *Control, TWinControl *ParentControl, TCanvas *Dest )
{
  if ( ParentControl == NULL || Control->Parent == NULL )
    return;

  TRect SelfR = Bounds( Control->Left, Control->Top, Control->Width, Control->Height);

  TPoint  TopLeft = Control->Parent->ClientToScreen ( Point ( SelfR.Left, SelfR.Top ));
  TopLeft = ParentControl->ScreenToClient ( TopLeft );
  int X = - TopLeft.x;
  int Y = - TopLeft.y;

  CopyParentImage( Control, SelfR, X, Y, ParentControl, Dest );
}
//---------------------------------------------------------------------------
void __fastcall CopyParentImage( TControl *Control, TCanvas *Dest )
{
  CopyParentImage( Control, Control->Parent, Dest );
}
//---------------------------------------------------------------------------
TColor MapGrayColor( TColor Color )
{
  unsigned char Index;

//  Color = (TColor)ColorToRGB(Color);
//  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 77 + GetGValue ( Color ) * 150 + GetBValue(Color) * 29 )) >> 8);
  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 29 + GetGValue ( Color ) * 150 + GetBValue(Color) * 77 )) >> 8);
  return (TColor)RGB(Index, Index, Index);
}

TColor MapHighColor ( TColor Color )
{
//  Color = ColorToRGB(Color);
//  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 77 + GetGValue ( Color ) * 150 + GetBValue(Color) * 29 )) >> 8);
  int Red = GetRValue ( Color );
  if ( Red == 0 )
    Red = 50;

  else
    {
    Red = Red + Red / 4;
    if ( Red > 255 )
      Red = 255;
    }

  int Green = GetGValue ( Color );
  if ( Green == 0 )
    Green = 50;

  else
    {
    Green = Green + Green / 4;
    if ( Green > 255 )
      Green = 255;
    }


  int Blue = GetBValue ( Color );
  if ( Blue == 0 )
    Blue = 50;

  else
    {
    Blue = Blue + Blue / 4;
    if ( Blue > 255 )
      Blue = 255;
    }

  return (TColor)RGB( Red, Green, Blue );
}



/*
TColor MapUltraHighColor ( TColor Color )
{
  Color = (TColor)ColorToRGB(Color);
//  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 77 + GetGValue ( Color ) * 150 + GetBValue(Color) * 29 )) >> 8);
  int Red = GetRValue ( Color );
  if ( Red == 0 )
    Red = 150;

  else
    {
    Red += 40;
    Red <<= 3;
    if ( Red > 255 )
      Red = 255;
    }

  int Green = GetGValue ( Color );
  if ( Green == 0 )
    Green = 150;

  else
    {
    Green += 40;
    Green <<= 3;
    if ( Green > 255 )
      Green = 255;
    }


  int Blue = GetBValue ( Color );
  if ( Blue == 0 )
    Blue = 150;

  else
    {
    Blue += 40;
    Blue <<= 3;
    if ( Blue > 255 )
      Blue = 255;
    }

  return (TColor)RGB( Red, Green, Blue );
}
*/

//---------------------------------------------------------------------------

typedef TColor (*TColorMapFunc) ( TColor );

void __fastcall CallForDIB ( Graphics::TBitmap *SourceBitmap, TColorMapFunc ColorMapFunc, TColorRef *Pixels )
{
#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef *Colors;
  } Info;
#pragma pack(pop)


    int W, H;
    int Y, X;
    TColorRef *Pixel;
    HDC DC;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;

//    Pixels = new TColorRef [ W * H ];
    try
      {
      DC = CreateCompatibleDC(0);
      memset ( &Info, 0, sizeof(Info));
      Info.Header.biSize = sizeof(TBitmapInfoHeader);
      Info.Header.biWidth = W;
      Info.Header.biHeight = -H;  //{ negative number makes it a top-down DIB }
      Info.Header.biPlanes = 1;
      Info.Header.biBitCount = 32;
      Info.Colors = NULL;
      GetDIBits ( DC, SourceBitmap->Handle, 0, H, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );

      for ( Y = 0; Y < H; Y ++ )
        {
        Pixel = Pixels + (Y * W);
        for ( X = 0; X < W; X++ )
          {
          *Pixel = ColorMapFunc ( (TColor)*Pixel );
          Pixel ++;
          }

        }

      SetDIBits (DC, SourceBitmap->Handle, 0, H, Pixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);

      DeleteDC (DC);
      }
        
    __finally
      {
//      delete Pixels;
      }

}


};
//---------------------------------------------------------------------------
namespace Bmpage
{
//---------------------------------------------------------------------------
inline void __fastcall ReverseColor ( void *DIBArray, long ImageSize )
{
/*  We don't need of this at the moment. Could be used in the future.
  RGBQUAD * RGBArray = (RGBQUAD *)DIBArray;
  for ( unsigned int i = 0; i < ImageSize / sizeof ( TColor ); i ++ )
    {
    BYTE b = RGBArray [ i ].rgbRed;
    RGBArray [ i ].rgbRed = RGBArray [ i ].rgbBlue;
    RGBArray [ i ].rgbBlue = b;
    }
*/
}
//---------------------------------------------------------------------------
inline long __fastcall BytesPerScanline ( long PixelsPerScanline, long BitsPerPixel, long Alignment )
{
  Alignment --;
  long Result = ((PixelsPerScanline * BitsPerPixel) + Alignment) & ( ~Alignment );
  return Result / 8;
}
//---------------------------------------------------------------------------
};
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::UpdateHighlighted ()
{
  if ( ! FFlashing )
    return;

  if ( Delay )
    {
    Delay --;
    return;
    }

  if ( FHighlitedItem != -1 && FEnableFlash )
    {
    FCloudSize ++;
    if ( FCloudSize > 3 )
      FCloudSize = 0;

    Delay = FFlashingDelay;

    if ( FCloudSize == 0 )
      Delay = FUnFlashedDelay;

    else if ( FCloudSize == 2 )
      {
      Delay = FFlashedDelay;

      if ( ! AlreadyPlayngEntryFlag )
        PlayPulseWave ();

      AlreadyPlayngEntryFlag = false;
      }

    RECT Rect;
    Perform ( TCM_GETITEMRECT, FHighlitedItem, (int)&Rect );
    if ( FHighlitedItem != GetCurFocus ())
      Rect.top += 2;

    Rect.left += 2;
    Rect.right -= 2;
    Rect.bottom -= 2;
    InvalidateRect( Handle, &Rect, false );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::PlaySelectWave ()
{
  if ( ! FEnableSounds )
    return;

  if ( FHighlitedItem == -1 )
    return;

  if ( (! ( Pages [ FHighlitedItem ]->FSelectSound )) || Pages [ FHighlitedItem ]->FSelectSound->Empty () )
    {
    if ( FSelectSound && ! FSelectSound->Empty () )
      FSelectSound->Play ();

    else
      PlayPulseWave();
    }

  else
    if ( Pages [ FHighlitedItem ]->FSelectSound )
      Pages [ FHighlitedItem ]->FSelectSound->Play ();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::PlayPulseWave()
{
  if ( ! FEnableSounds )
    return;

  if ( FHighlitedItem == -1 )
    return;

  if ( (!(Pages [ FHighlitedItem ]->FPulseSound)) || Pages [ FHighlitedItem ]->FPulseSound->Empty () )
    {
    if ( FPulseSound )
      FPulseSound->Play ();
    }

  else
    if ( Pages [ FHighlitedItem ]->FPulseSound )
      Pages [ FHighlitedItem ]->FPulseSound->Play ();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::PlayHighlightWave()
{
  if ( ! FEnableSounds )
    return;

  if ( FHighlitedItem == -1 )
    return;

  if ( (!(Pages [ FHighlitedItem ]->FHighlightSound )) || Pages [ FHighlitedItem ]->FHighlightSound->Empty () )
    {
    if ( FHighlightSound ) 
      FHighlightSound->Play ();
    }

  else
    if ( Pages [ FHighlitedItem ]->FHighlightSound )
      Pages [ FHighlitedItem ]->FHighlightSound->Play ();
}
//---------------------------------------------------------------------------
bool __fastcall TBMPageControl::HaveHighlightWave ()
{
  if ( FHighlitedItem != -1 )
    return ( ( Pages [ FHighlitedItem ]->FHighlightSound && ( ! Pages [ FHighlitedItem ]->FHighlightSound->Empty () )) ||
             ( FHighlightSound && ( ! FHighlightSound->Empty () )));

  return ( FHighlightSound && ! FHighlightSound->Empty () );
}
//---------------------------------------------------------------------------
bool __fastcall TBMPageControl::HaveSelectWave ()
{
  if ( FHighlitedItem != -1 )
    return (( Pages [ FHighlitedItem ]->FSelectSound && ( ! Pages [ FHighlitedItem ]->FSelectSound->Empty () )) || ( FSelectSound && ( ! FSelectSound->Empty () )));

  return ( FSelectSound && ! FSelectSound->Empty () );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::UpdateGlyphs()
{
  TTCItem TCItem;
  int     Control;
  int     Loop;

  if ( GetPageCount () != Tabs->Count )
    {
    PostMessage ( Handle, WM_SIZE, SIZE_RESTORED, 0 );
    return;
    }

  Glyphs->Clear ();
  Glyphs->Width = 16;
  Glyphs->Height = 16;

  GrayedGlyphs->Clear ();
  GrayedGlyphs->Width = 16;
  GrayedGlyphs->Height = 16;

  HighlightedGlyphs->Clear ();
  HighlightedGlyphs->Width = 16;
  HighlightedGlyphs->Height = 16;


  SelectedHighlightedGlyphs->Clear ();
  SelectedHighlightedGlyphs->Width = 16;
  SelectedHighlightedGlyphs->Height = 16;

  int Count = 0;

  for ( Loop = 0; Loop < PageCount; Loop ++ )
    {
    TBMTabSheet *Page = Pages[ Loop ];
    if ( ! Page->TabVisible )
      continue;

    Graphics::TBitmap *Image = Page->RealGlyph;

    if ( Image )
      {
      Image->Height = Glyphs->Height;
      Glyphs->AddMasked ( Image, Image->Canvas->Pixels [ 0 ][ Image->Height - 1 ] );
      ((TTempImageList *)Glyphs)->GetImages ( Count, ImageBmp, MaskBmpBlack );

      if ( ! Page->FGlyphUnselected->Empty )
        {
        Page->RealGlyphUnselected->Height = Glyphs->Height;
        GrayedGlyphs->AddMasked ( Page->RealGlyphUnselected, Page->RealGlyphUnselected->Canvas->Pixels [ 0 ][ Image->Height - 1 ] );
        }

      else
        {
        GrayedImage->Canvas->Draw ( 0, 0, Image );
        CallForDIB ( GrayedImage, MapGrayColor, DIBArray ); 
        GrayedGlyphs->Add( GrayedImage, MaskBmpBlack );
        }

      if ( ! Page->FGlyphSelectedHighlighted->Empty )
        {
        Page->RealGlyphHighlighted->Height = Glyphs->Height;
        SelectedHighlightedGlyphs->AddMasked ( Page->RealGlyphSelectedHighlighted, Page->RealGlyphSelectedHighlighted->Canvas->Pixels [ 0 ][ Image->Height - 1 ] );
        }

      else
        {
        HighlightedImage->Canvas->Draw ( 0, 0, Image );
        CallForDIB ( HighlightedImage, MapHighColor, DIBArray );
        SelectedHighlightedGlyphs->Add( HighlightedImage, MaskBmpBlack );
        }

      if ( ! Page->FGlyphHighlighted->Empty )
        {
        Page->RealGlyphHighlighted->Height = Glyphs->Height;
        HighlightedGlyphs->AddMasked ( Page->RealGlyphHighlighted, Page->RealGlyphHighlighted->Canvas->Pixels [ 0 ][ Image->Height - 1 ] );
        }

      else
        {
        Graphics::TBitmap *TheImage = Page->FGlyphUnselected;
        if ( TheImage->Empty )
          TheImage = Image;

        HighlightedImage->Canvas->Draw ( 0, 0, TheImage );
        CallForDIB ( HighlightedImage, MapHighColor, DIBArray );
        HighlightedGlyphs->Add( HighlightedImage, MaskBmpBlack );
        }

      TCItem.mask = TCIF_IMAGE;
      TCItem.iImage = Count;
      Control = Count;

      Count ++;

      Perform ( TCM_SETITEM, Control, ( long )&TCItem );
      }
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::ValidateParentImage ()
{
  if ( ! PackGroundUpdated )
    {
    FParentBackgroundBuffer->Width = Width;
    FParentBackgroundBuffer->Height = Height;
    CopyParentImage ( this, FParentBackgroundBuffer->Canvas );
    PackGroundUpdated = true;
    }
    
}
//---------------------------------------------------------------------------
#ifdef __BCB_40__
void __fastcall TBMPageControl::ValidateBtnRect ( TRect TmpRect )
{
  if ( FTransparent )
    Canvas->CopyRect( TmpRect, FParentBackgroundBuffer->Canvas, TmpRect );

   else
    Canvas->FillRect ( TmpRect);
}
#endif
//---------------------------------------------------------------------------
int __fastcall TBMPageControl::ConvertTabToPageIndex ( int Index )
{
  int TmpIndex = Index;
  
  for ( int i = 0; i < PageCount; i ++ )
    {
    if ( Pages [ i ]->TabVisible != true )
      continue;

    if ( ! TmpIndex-- )
      return i;
    }
    
  return Index;
}
//---------------------------------------------------------------------------
int __fastcall TBMPageControl::GetCurSelection ()
{
  return ConvertTabToPageIndex ( Perform ( TCM_GETCURSEL, 0, 0 ));
}
//---------------------------------------------------------------------------
int __fastcall TBMPageControl::GetCurFocus ()
{
  return ConvertTabToPageIndex ( Perform ( TCM_GETCURFOCUS, 0, 0 ));
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMPaint(TMessage& Msg)
{
#ifdef __BCB_40__
  if ( Style == tsFlatButtons )
    {
    int Index = GetCurSelection ();

    if ( Index < 0 )
      Index = GetCurFocus ();

    if ( FTransparent )
      ValidateParentImage ();
      
    for ( int i = 0; i < PageCount; i ++ )
      {
      if ( i != Index )
        {
        TRect Rect;
        Perform ( TCM_GETITEMRECT, i, (int)&Rect );
        Rect.Left -= 2;
        Rect.Top -= 2;
        Rect.Right += 2;
        Rect.Bottom += 2;
        Canvas->Brush->Color = Color;

        TRect TmpRect;
        TmpRect = ::Rect ( Rect.Left, Rect.Top, Rect.Left + 4, Rect.Bottom );
        ValidateRect ( Handle, (RECT *) &TmpRect );
        ValidateBtnRect ( TmpRect );

        TmpRect = ::Rect ( Rect.Right - 4, Rect.Top, Rect.Right, Rect.Bottom );

        ValidateRect ( Handle, (RECT *) &TmpRect );
        ValidateBtnRect ( TmpRect );

        TmpRect = ::Rect ( Rect.Left, Rect.Top, Rect.Right, Rect.Top + 4 );

        ValidateRect ( Handle, (RECT *) &TmpRect );
        ValidateBtnRect ( TmpRect );

        TmpRect = ::Rect ( Rect.Left, Rect.Bottom - 4, Rect.Right, Rect.Bottom );

        ValidateRect ( Handle, (RECT *) &TmpRect );
        ValidateBtnRect ( TmpRect );
        }
      }
    }
#endif
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
#ifndef __BCB_40__
void __fastcall TBMPageControl::SetStyle ( TTabStyle Value )
{
  if ( FStyle != Value )
    {
    FStyle = Value;
    RecreateWnd ();
    }
}
#endif
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMEraseBkgnd ( TMessage& Msg )
{
  Graphics::TBitmap *BackgroundBmp = NULL;

  OffsetBitmap->Width = Width;
  OffsetBitmap->Height = Height;

  TCanvas *OffsetCanvas = OffsetBitmap->Canvas;

  int Index = GetCurSelection ();

  if ( Index < 0 )
    Index = GetCurFocus ();

  FCanvas->Handle = (HDC) Msg.WParam;
  OffsetCanvas->Brush->Color = Color;
  if ( FTransparent )
    CopyParentImage ( this, OffsetCanvas );

  else
    OffsetCanvas->FillRect ( ::Rect ( 0,0, Width, Height ));

  if ( Index >= 0 && Index < PageCount )
    {
    TBMTabSheet *ActivePg = Pages [ Index ];

    RECT Rect;

    Perform ( TCM_GETITEMRECT, ActivePg->TabIndex, (int)&Rect );
    if ( FTabColorization == tcPage || FTabColorization == tcActivePage )
      {
      OffsetCanvas->Brush->Color = ActivePg->Color;
      BackgroundBmp = ActivePg->RefreshBuffer ();
      }

    else
      OffsetCanvas->Brush->Color = FColorSelected;
//    if ( FTabColorization != tcNone && ! FTabStyles.Contains ( tabButton ))
    if ( FTabColorization != tcNone && Style == tsTabs )
      {
      TRect FRect;
      switch ( TabPosition )
        {
        case tpTop:
          FRect = ::Rect ( 2, Rect.bottom, Width - 2, Height - 2 );
          break;

        case tpLeft:
          FRect = ::Rect ( Rect.right, 2, Width - 2, Height - 2 );
          break;

        case tpRight:
          FRect = ::Rect ( 2, 2, Rect.left, Height - 2 );
          break;

        case tpBottom:
          FRect = ::Rect ( 2, 2, Width - 2, Rect.top - 2);
          break;
        }

/*
      if ( ActivePg->FTransparent )
        {
        ValidateParentImage ();
        OffsetCanvas->CopyRect( FRect, FParentBackgroundBuffer->Canvas, FRect );
        }

      else
*/
      if ( /*ActivePg->FTransparent ||*/ BackgroundBmp && ( ! BackgroundBmp->Empty ))
        OffsetCanvas->CopyRect( FRect, BackgroundBmp->Canvas, FRect );

      else
        OffsetCanvas->FillRect ( FRect );
      }

    if ( FTabColorization != tcNone )
      {
      int TabCounter = 0;
      for ( int i = 0; i < PageCount; i ++ )
        {
        if ( Pages [ i ]->TabVisible != true )
          continue;

        RECT Rect;
        Perform ( TCM_GETITEMRECT, TabCounter, (int)&Rect );
        TabCounter ++;
        
        if ( Rect.left >= 0 && Rect.top >= 0 && Rect.bottom >= 0 && Rect.right >= 0 )
          {
          Rect.left += 2;
          Rect.right -= 4;
          Rect.top += 2;
          if ( Rect.left < Rect.right + 10 )
            {
            if ( Rect.right < Width )
              {
              BackgroundBmp = NULL;

              if ( FTabColorization == tcPage )
                {
                OffsetCanvas->Brush->Color = Pages [ i ]->Color;
                BackgroundBmp = Pages [ i ]->RefreshBuffer ();
                }

              else
                {
                if ( i == Index )
                  {
                  OffsetCanvas->Brush->Color = (( FTabColorization == tcActivePage ) ? Pages [ i ]->Color : FColorSelected );
                  BackgroundBmp = Pages [ i ]->RefreshBuffer ();
                  }

                else
                  {
                  OffsetCanvas->Brush->Color = FColorUnselected;
                  }
                }

/*
              if ( ( i == Index || FTabColorization == tcPage ) && Pages [ i ]->FTransparent )
                {
                OffsetCanvas->CopyRect( Rect, FParentBackgroundBuffer->Canvas, Rect );
                }

              else
*/
              if ( BackgroundBmp && ( ! BackgroundBmp->Empty ))
                OffsetCanvas->CopyRect( Rect, BackgroundBmp->Canvas, Rect );

              else
                OffsetCanvas->FillRect ( Rect );
              }
            }
          }
        }
      }
    }

  FCanvas->Draw ( 0, 0, OffsetBitmap );
  FCanvas->Handle = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::InvalidateBorder ()
{
//  if ( FTabStyles.Contains ( tabButton ))
  if ( Style != tsTabs )
    return;

  int Index = GetCurSelection ();

  if ( Index < 0 )
    Index = GetCurFocus ();
  
  if ( Index >= 0 && Index < PageCount )
    {
    TBMTabSheet *ActivePage = Pages [ Index ];

#ifndef __BCB_40__
    ActivePage->Invalidate ();
#endif
    ActivePage->InvalidateParentBorder ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetTabColorization  ( TTabColorization _TabColorization )
{
  if ( FTabColorization != _TabColorization )
    {
    FTabColorization = _TabColorization;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetCloudColorization    ( TCloudColorization _CloudColorization )
{
  if ( FCloudColorization != _CloudColorization )
    {
    FCloudColorization = _CloudColorization;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetGrayInactive ( bool Value )
{
  if ( FGrayInactive != Value )
    {
    FGrayInactive = Value;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetFlashing ( bool Value )
{
  FFlashing = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHighlightGlyphts ( bool Value )
{
  if ( FHighlightGlyphts != Value )
    {
    FHighlightGlyphts = Value;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHotTrackColor    ( TColor _Color )
{
  if ( FHotTrackColor != _Color )
    {
    FHotTrackColor = _Color;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHotTrackCloudColor   ( TColor _Color )
{
  if ( FHotTrackCloudColor != _Color )
    {
    FHotTrackCloudColor = _Color;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHotTrackTextStyle    ( TDrawStyle _Style )
{
  if ( FHotTrackTextStyle != _Style )
    {
    FHotTrackTextStyle = _Style;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetUnFlashedDelay ( int Value )
{
  if ( Value < 10 )
    FUnFlashedDelay = 10;

  else
    FUnFlashedDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetFlashingDelay ( int Value )
{
  if ( Value < 10 )
    FFlashingDelay = 10;

  else
    FFlashingDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetFlashedDelay ( int Value )
{
  if ( Value < 10 )
    FFlashedDelay = 10;

  else
    FFlashedDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHotTrackGlyphtStyle  ( TDrawStyle _Style )
{
  if ( FHotTrackGlyphtStyle != _Style )
    {
    FHotTrackGlyphtStyle = _Style;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetColorUnselected  ( TColor _Color )
{
  if ( FColorUnselected != _Color )
    {
    FColorUnselected = _Color;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetColorSelected  ( TColor _Color )
{
  if ( FColorSelected != _Color )
    {
    FColorSelected = _Color;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
bool __fastcall TBMPageControl::CanChange(void)
{
  FCloudSize = 0;
  Delay = 12;
  return inherited::CanChange();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::Change(void)
{
  PackGroundUpdated = false;
  FCloudSize = 0;
  Delay = 12;
  inherited::Change();
  InvalidateBorder ();

//  if ( HaveSelectWave () )
  AlreadyPlayngEntryFlag = true;

  PlaySelectWave ();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMSize(Messages::TMessage &Msg)
{
  inherited::Dispatch ( &Msg );

  if ( Msg.WParam == SIZE_RESTORED )
    UpdateGlyphs();

  Invalidate ();

  if ( ActivePage )
//    if ( ((TBMTabSheet *)ActivePage)->FTransparent )
      ActivePage->Invalidate ();

}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMMove(Messages::TMessage &Msg)
{
  Invalidate ();

  for ( int i = 0; i < PageCount; i ++ )
    if ( Pages [ i ]->FTransparent )
      Pages [ i ]->FBackgroundBuffer->Assign ( NULL );

  if ( ActivePage )
    if ( ((TBMTabSheet *)ActivePage)->FTransparent )
      ActivePage->Invalidate ();

  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::Invalidate(void)
{
  PackGroundUpdated = false;
  inherited::Invalidate();
  InvalidateRect(

    Handle,    // handle of window with changed update region
    (RECT *)&::Rect ( 0, 0, Width, Height ),    // address of rectangle coordinates
    true    // erase-background flag
   );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CMColorChanged   ( TMessage& Msg )
{
  Invalidate();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CMMouseEnter ( TMessage& Msg )
{
  Section->Enter ();
  FMouseInside = true;
  Section->Leave ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CMMouseLeave ( TMessage& Msg )
{
  DEBUG_LOG_MSG ( "Leave" );
  Section->Enter ();
  FMouseInside = false;
  if ( FHighlitedItem != -1 )
    {
    DEBUG_LOG_MSG ( "Leave->Disable" );
    RECT Rect;
    Perform ( TCM_GETITEMRECT, FHighlitedItem, (int)&Rect );
    Rect.left += 2;
    Rect.right -= 2;
    FHighlitedItem = -1;
    FEnableFlash = false;
    InvalidateRect( Handle, &Rect, false );
    }

  FHighlitedItem = -1;
  FCloudSize = 0;
  FEnableFlash = false;
  Delay = 12;
  Section->Leave ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
bool __fastcall TBMPageControl::HasTransparentcy ()
{
  if ( FTransparent )
    return true;

  for ( int i = 0; i < PageCount; i ++ )
    if ( Pages [ i ]->FTransparent )
      return true;

  return false;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMVHScroll       ( Messages::TMessage    &Msg )
{
  inherited::Dispatch ( &Msg );
  if ( HasTransparentcy () )
    Invalidate ();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::BMChangeView    ( Messages::TMessage    &Msg )
{
  inherited::Dispatch ( &Msg );
  Invalidate ();
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::WMMouseMove  ( Messages::TMessage &Msg)
{
  inherited::Dispatch ( &Msg );

  if ( HotTrack && ! ComponentState.Contains (csDesigning) && FMouseInside )
    {
    DEBUG_LOG_MSG ( "Enter" );
    Section->Enter ();
    TC_HITTESTINFO  Info;
    Info.pt.x = LOWORD( Msg.LParam );
    Info.pt.y = HIWORD( Msg.LParam );
    int Index = Perform ( TCM_HITTEST, 0, (int)&Info );
    if ( FHighlitedItem != Index )
      {
      RECT Rect;
      if ( FHighlitedItem != -1 )
        {
        Perform ( TCM_GETITEMRECT, FHighlitedItem, (int)&Rect );
        Rect.left += 2;
        Rect.right -= 2;
        InvalidateRect( Handle, &Rect, false );
        }

      FHighlitedItem = Index;

      if ( FHighlitedItem != -1 )
        {
        Perform ( TCM_GETITEMRECT, FHighlitedItem, (int)&Rect );
        Rect.left += 2;
        Rect.right -= 2;
        InvalidateRect( Handle, &Rect, false );

        if ( HaveHighlightWave () )
          {
          AlreadyPlayngEntryFlag = true;
          PlayHighlightWave ();
          }
        }

      FCloudSize = 0;
      FEnableFlash = false;
      Delay = 12;
      }

    Section->Leave ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::CNDrawItem(Messages::TWMDrawItem &Msg)
{
  TOwnerDrawState State;
  int iState = Msg.DrawItemStruct->itemState & 0xFF;
  State = *((TOwnerDrawState *)(&( iState )));

  FCanvas->Handle = Msg.DrawItemStruct->hDC;
  FCanvas->Font = Font;
  FCanvas->Brush = Brush;
  if ( (int) Msg.DrawItemStruct->itemID >= 0 )
    {
//    RECT Rect;
//    Perform ( TCM_GETITEMRECT, (int) Msg.DrawItemStruct->itemID, (int)&Rect );

    int Index = ConvertTabToPageIndex ( Msg.DrawItemStruct->itemID );
    DrawItem ( Msg.DrawItemStruct->itemID, Index, TRect ( Msg.DrawItemStruct->rcItem ), State );
//    DrawItem ( Msg.DrawItemStruct->itemID, Rect, State );
    }

  else
    FCanvas->FillRect ( TRect ( Msg.DrawItemStruct->rcItem ));

  FCanvas->Handle = NULL;
//  Msg.Result = true;
}
//---------------------------------------------------------------------------
void __fastcall DoElliptic( Graphics::TBitmap *bm, TRect Rect, int fr, int fg, int fb, int dr, int dg, int db, int FCloudSize )
{
  int I;
  int R, G, B;
  int Pw, Ph, Dw, Dh;
  int x1,y1,x2,y2;

  bm->Canvas->Pen->Style = psClear;
  bm->Canvas->Pen->Mode = pmCopy;
  
  int Width  = Rect.Right - Rect.Left;
  int Height = Rect.Bottom - Rect.Top;

  int   Delimiter;

  if ( FCloudSize == 2 )
    {
    x1 = Rect.Left + Width / -2;
    x2 = Rect.Left + Width + (Width / 2);
    y1 = Rect.Top + Height / -2;
    y2 = Rect.Top + Height + (Height / 2);
    Pw = x2 - x1;
    Ph = y2 - y1;
    Delimiter = 175;
    }

  else if ( FCloudSize == 1 || FCloudSize == 3 )
    {
    x1 = Rect.Left + Width / -3;
    x2 = Rect.Left + Width + (Width / 3);
    y1 = Rect.Top + Height / -3;
    y2 = Rect.Top + Height + (Height / 3);
    Pw = x2 - x1;
    Ph = y2 - y1;
    Delimiter = 100;
    }

  else
    {
    x1 = Rect.Left;
    x2 = Rect.Left + Width;
    y1 = Rect.Top;
    y2 = Rect.Top + Height;
    Pw = Width / 2;
    Ph = Height / 2;
    Delimiter = 50;
    }

  for ( I = 0; I <= 50; I++ )          //Make ellipses of color
    {
    R = fr + I * dr / 50;    //Find the RGB values
    G = fg + I * dg / 50;
    B = fb + I * db / 50;
    bm->Canvas->Brush->Color = (TColor)RGB ( R, G, B );   //Plug colors into brush
    Dw = Pw * I / Delimiter;
    Dh = Ph * I / Delimiter;
    bm->Canvas->Ellipse(x1 + Dw,y1 + Dh,x2 - Dw,y2 - Dh);
    }

  bm->Canvas->Pen->Style = psSolid;
}
//---------------------------------------------------------------------------
void __fastcall ColorizeDIB ( Graphics::TBitmap *SourceBitmap, TColor ColorizeTo, int Level )
{
#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef *Colors;
  } Info;
#pragma pack(pop)


    int W, H;
    int Y, X;
    TColorRef *Pixel;
    HDC DC;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;

    TColorRef *Pixels;
    Pixels = new TColorRef [ W * H ];

    try
      {
      DC = CreateCompatibleDC(0);
      memset ( &Info, 0, sizeof(Info));
      Info.Header.biSize = sizeof(TBitmapInfoHeader);
      Info.Header.biWidth = W;
      Info.Header.biHeight = -H;  // negative number makes it a top-down DIB
      Info.Header.biPlanes = 1;
      Info.Header.biBitCount = 32;
      Info.Colors = NULL;

      GetDIBits ( DC, SourceBitmap->Handle, 0, H, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );

      int k = ( 255 * ( 100 - Level )) / 100;
      int i;
      int A0;
      int A1;
      int A2;

      int B0;
      int B1;
      int B2;

      int Red;
      int Green;
      int Blue;

      for ( Y = 0; Y < H; Y ++ )
        {
        Pixel = Pixels + ( Y * W );
        for ( X = 0; X < W; X++ )
          {
//        k = GetGValue ( *Pixel1 );
          i = 256 - k;
          A0 = *Pixel & 0xFF;
          A1 = ( *Pixel >> 8 ) & 0xFF;
          A2 = ( *Pixel >> 16 ) & 0xFF;

          B2 = ColorizeTo & 0xFF;
          B1 = ( ColorizeTo >> 8 ) & 0xFF;
          B0 = ( ColorizeTo >> 16 ) & 0xFF;

          Red   = A0 * i / 256 + B0 * k / 256;
          Green = A1 * i / 256 + B1 * k / 256;
          Blue  = A2 * i / 256 + B2 * k / 256;

//          *Pixel = (TColor)RGB( GetRValue ( *Pixel ) + GetRValue ( *Pixel2 ), GetGValue ( *Pixel ) + GetGValue ( *Pixel2 ), GetBValue ( *Pixel ) + GetBValue ( *Pixel2 ) );
          *Pixel = ( TColor )RGB ( Red, Green, Blue );
//          *Pixel = ColorMapFunc ( (TColor)*Pixel );
          Pixel ++;
          }

        }

      SetDIBits (DC, SourceBitmap->Handle, 0, H, Pixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);

      DeleteDC (DC);
      }

    __finally
      {
      delete Pixels;
      }

}
//---------------------------------------------------------------------------
void __fastcall MixDIBs ( Graphics::TBitmap *SourceBitmap, Graphics::TBitmap *SupportBitmap, int MixLevel )
{
#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef *Colors;
  } Info;
#pragma pack(pop)


    int W, H;
    int Y, X;
    TColorRef *Pixel;
    TColorRef *Pixel1;
    HDC DC;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;

    TColorRef *Pixels, *Pixels1;
    Pixels = new TColorRef [ W * H ];
    Pixels1 = new TColorRef [ W * H ];

    try
      {
      DC = CreateCompatibleDC(0);
      memset ( &Info, 0, sizeof(Info));
      Info.Header.biSize = sizeof(TBitmapInfoHeader);
      Info.Header.biWidth = W;
      Info.Header.biHeight = -H;  // negative number makes it a top-down DIB
      Info.Header.biPlanes = 1;
      Info.Header.biBitCount = 32;
      Info.Colors = NULL;

//      memcpy ( &Info1, &Info, sizeof(Info));
//      Info1.Header.biWidth = W;
//      Info1.Header.biHeight = -H;  // negative number makes it a top-down DIB

      GetDIBits ( DC, SourceBitmap->Handle, 0, H, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );
      GetDIBits ( DC, SupportBitmap->Handle, 0, H, Pixels1, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );

      int k = ( 255 * ( 100 - MixLevel )) / 100;
      int i;
      int A0;
      int A1;
      int A2;

      int B0;
      int B1;
      int B2;

      int Red;
      int Green;
      int Blue;

      for ( Y = 0; Y < H; Y ++ )
        {
        Pixel = Pixels + (Y * W );
        Pixel1 = Pixels1 + (Y * W);
        for ( X = 0; X < W; X++ )
          {
          i = 256 - k;
          A0 = *Pixel & 0xFF;
          A1 = ( *Pixel >> 8 ) & 0xFF;
          A2 = ( *Pixel >> 16 ) & 0xFF;

          B0 = *Pixel1 & 0xFF;
          B1 = ( *Pixel1 >> 8 ) & 0xFF;
          B2 = ( *Pixel1 >> 16 ) & 0xFF;

          Red   = A0 * i / 256 + B0 * k / 256;
          Green = A1 * i / 256 + B1 * k / 256;
          Blue  = A2 * i / 256 + B2 * k / 256;

//          *Pixel = (TColor)RGB( GetRValue ( *Pixel ) + GetRValue ( *Pixel2 ), GetGValue ( *Pixel ) + GetGValue ( *Pixel2 ), GetBValue ( *Pixel ) + GetBValue ( *Pixel2 ) );
          *Pixel = ( TColor )RGB ( Red, Green, Blue );
//          *Pixel = ColorMapFunc ( (TColor)*Pixel );
          Pixel ++;
          Pixel1 ++;
          }

        }

      SetDIBits (DC, SourceBitmap->Handle, 0, H, Pixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);

      DeleteDC (DC);
      }

    __finally
      {
      delete Pixels;
      delete Pixels1;
      }

}
//---------------------------------------------------------------------------
void __fastcall CloudDIB ( Graphics::TBitmap *SourceBitmap, Graphics::TBitmap *SupportBitmap, TPoint Offset,TColor ColorizeTo )
{
#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef *Colors;
  } Info, Info1;
#pragma pack(pop)


    int W, H;
    int W1, H1;
    int Y, X;
    TColorRef *Pixel;
    TColorRef *Pixel1;
    HDC DC;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;

    W1 = SupportBitmap->Width;
    H1 = SupportBitmap->Height;

    TColorRef *Pixels, *Pixels1;
    Pixels = new TColorRef [ W * H ];
    Pixels1 = new TColorRef [ W1 * H1 ];

    try
      {
      DC = CreateCompatibleDC(0);
      memset ( &Info, 0, sizeof(Info));
      Info.Header.biSize = sizeof(TBitmapInfoHeader);
      Info.Header.biWidth = W;
      Info.Header.biHeight = -H;  // negative number makes it a top-down DIB
      Info.Header.biPlanes = 1;
      Info.Header.biBitCount = 32;
      Info.Colors = NULL;

      memcpy ( &Info1, &Info, sizeof(Info));
      Info1.Header.biWidth = W1;
      Info1.Header.biHeight = -H1;  // negative number makes it a top-down DIB

      GetDIBits ( DC, SourceBitmap->Handle, 0, H, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );
      GetDIBits ( DC, SupportBitmap->Handle, 0, H1, Pixels1, ( TBitmapInfo * )(&Info1),  DIB_RGB_COLORS );

      int k;
      int i;
      int A0;
      int A1;
      int A2;

      int B0;
      int B1;
      int B2;

      int Red;
      int Green;
      int Blue;

      for ( Y = 0; Y < H1; Y ++ )
        {
        Pixel = Pixels + ((Y + Offset.y ) * W) + Offset.x;
        Pixel1 = Pixels1 + (Y * W1);
        for ( X = 0; X < W1; X++ )
          {
          k = GetGValue ( *Pixel1 );
          i = 256 - k;
          A0 = *Pixel & 0xFF;
          A1 = ( *Pixel >> 8 ) & 0xFF;
          A2 = ( *Pixel >> 16 ) & 0xFF;

          B2 = ColorizeTo & 0xFF;
          B1 = ( ColorizeTo >> 8 ) & 0xFF;
          B0 = ( ColorizeTo >> 16 ) & 0xFF;

          Red   = A0 * i / 256 + B0 * k / 256;
          Green = A1 * i / 256 + B1 * k / 256;
          Blue  = A2 * i / 256 + B2 * k / 256;

//          *Pixel = (TColor)RGB( GetRValue ( *Pixel ) + GetRValue ( *Pixel2 ), GetGValue ( *Pixel ) + GetGValue ( *Pixel2 ), GetBValue ( *Pixel ) + GetBValue ( *Pixel2 ) );
          *Pixel = ( TColor )RGB ( Red, Green, Blue );
//          *Pixel = ColorMapFunc ( (TColor)*Pixel );
          Pixel ++;
          Pixel1 ++;
          }

        }

      SetDIBits (DC, SourceBitmap->Handle, 0, H, Pixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);

      DeleteDC (DC);
      }

    __finally
      {
      delete Pixels;
      delete Pixels1;
      }

}
//---------------------------------------------------------------------------
void HiglightRectangle1 ( Graphics::TBitmap *TabBmp, TRect Rect, TColor FBeginClr, TColor FEndClr, int FCloudSize )
{
  FBeginClr = (TColor)ColorToRGB ( FBeginClr );
  int FromR = GetRValue ( FBeginClr );;  //Strip out separate RGB values
  int FromG = GetGValue ( FBeginClr );
  int FromB = GetBValue ( FBeginClr );

  int DiffR = (int)GetRValue ( FEndClr ) - FromR;   //Find the difference
  int DiffG = (int)GetGValue ( FEndClr ) - FromG;
  int DiffB = (int)GetBValue ( FEndClr ) - FromB;

  DoElliptic( TabBmp, Rect, FromR, FromG, FromB, DiffR, DiffG, DiffB, FCloudSize );
}
//---------------------------------------------------------------------------
void HiglightRectangle ( Graphics::TBitmap *Bmp, TRect Rect, TColor FEndClr, int FCloudSize )
{
  Graphics::TBitmap *TmpBmp = new Graphics::TBitmap;
//  TmpBmp->Width = Rect.Right - Rect.Left; //Bmp->Width;
//  TmpBmp->Height = Rect.Bottom - Rect.Top; //Bmp->Height;
  TmpBmp->Width = Bmp->Width - Rect.Left;
  TmpBmp->Height = Bmp->Height - Rect.Top;
  TmpBmp->Canvas->Brush->Color = clBlack;
  TRect ZeroRect = ::Rect ( 0, 0, TmpBmp->Width, TmpBmp->Height );
  TmpBmp->Canvas->FillRect ( ZeroRect );
  DoElliptic( TmpBmp, ZeroRect, 0, 0, 0, 255, 255, 255, FCloudSize );

  CloudDIB ( Bmp, TmpBmp, ::Point ( Rect.Left, Rect.Top ), FEndClr );

  delete TmpBmp;
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::DrawGlyphtShadow ( int ShadowOffset, int XImagePos, int YImagePos )
{
  ImageBmp->Canvas->FillRect ( ::Rect ( 0, 0, 16, 16 ));

  MaskBmp->Canvas->CopyMode = cmSrcErase;
  MaskBmp->Canvas->Draw ( 0, 0, ImageBmp );

  int OldMode = TabBmp->Canvas->CopyMode;
  TabBmp->Canvas->CopyMode = cmSrcAnd;
  TabBmp->Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + ShadowOffset, MaskBmpBlack );
  TabBmp->Canvas->Draw ( XImagePos, YImagePos + ShadowOffset, MaskBmpBlack );
  TabBmp->Canvas->Draw ( XImagePos + ShadowOffset, YImagePos, MaskBmpBlack );
  TabBmp->Canvas->CopyMode = cmSrcPaint;
  TabBmp->Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + ShadowOffset, MaskBmp );
  TabBmp->Canvas->Draw ( XImagePos, YImagePos + ShadowOffset, MaskBmp );
  TabBmp->Canvas->Draw ( XImagePos + ShadowOffset, YImagePos, MaskBmp );
  TabBmp->Canvas->CopyMode = OldMode;
}
//---------------------------------------------------------------------------
void __fastcall MakeAngleFont ( TCanvas *Canvas, int Angle )
{
  LOGFONT Lf;

  GetObject ( Canvas->Font->Handle, sizeof ( LOGFONT ), &Lf );

  Lf.lfEscapement = Angle;
  Lf.lfOrientation = Angle;
  
  Lf.lfOutPrecision = OUT_TT_ONLY_PRECIS;
  Canvas->Font->Handle = CreateFontIndirect ( &Lf );
  SetGraphicsMode ( Canvas->Handle, GM_ADVANCED );
  Canvas->Brush->Style = bsClear;
//  Canvas->TextOut ( 100, 100, "Hello world !!!" );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::DrawItem ( int TabIndex, int PageIndex, const TRect &RectIn, Windows::TOwnerDrawState State )
{
  TRect Rect = RectIn;

  int TopCorrection = 0;
  int CentralizeCorection = 0;
  int LeflMargin = 4;
  int XLeflMargin = 0;
  int YLeflMargin = 0;

  FEnableFlash = true;

  TabBmp->Canvas->Font->Assign( Font );
//  if ( FTabStyles.Contains ( tabButton ))
  if ( Style != tsTabs )
    {
    Rect.Left -= 2;
    Rect.Right += 2;
    if ( State.Contains ( odSelected ))
      {
      TopCorrection = -1;
      }

    else
      {
      TopCorrection = -2;
      }
    }

  else
    {
    switch ( TabPosition )
      {
      case tpTop:
        break;

      case tpLeft:
//        if ( State.Contains ( odSelected ))
          Rect.Right += 2;

        break;

      case tpRight:
          Rect.Left -= 2;

        if ( ! State.Contains ( odSelected ))
          TopCorrection = -3;

        break;

      case tpBottom:
        if ( State.Contains ( odSelected ))
          Rect.Bottom += 2;

        else
          {
          TopCorrection = -4;
          Rect.Top -= 2;
          Rect.Bottom -= 2;
          }

        break;
      }
    }

  if ( FOnDrawItem )
    FOnDrawItem ( this, PageIndex, Rect, State );

  else
    {
    TabBmp->Width = Rect.Right - Rect.Left;
    TabBmp->Height = Rect.Bottom - Rect.Top + 2;
//    TabBmp->Canvas->TextFlags = TabBmp->Canvas->TextFlags & ( ~ETO_OPAQUE );
//    TabBmp->Canvas->TextFlags = TabBmp->Canvas->TextFlags | ETO_OPAQUE;


    TabBmp->Canvas->Font->Assign( Font );
    TabBmp->Canvas->Brush = Brush;
    Graphics::TBitmap *BackgroundBmp = NULL;
    if ( FTabColorization != tcNone )
      {
      if ( FTabColorization == tcPage )
        {
        TabBmp->Canvas->Brush->Color = Pages [ PageIndex ]->Color;
        BackgroundBmp = Pages [ PageIndex ]->RefreshBuffer ();
        }

      else
        {
        if ( State.Contains ( odSelected ))
          {
          TabBmp->Canvas->Brush->Color = (( FTabColorization == tcActivePage ) ? Pages [ PageIndex ]->Color : FColorSelected );
          BackgroundBmp = Pages [ PageIndex ]->RefreshBuffer ();
          }

        else
          {
          TabBmp->Canvas->Brush->Color = FColorUnselected;
          }
//        TabBmp->Canvas->Brush->Color = ( State.Contains ( odSelected )) ? (( FTabColorization == tcActivePage ) ? Pages [ Index ]->FColor : FColorSelected ) : FColorUnselected;
        }
        
////      TabBmp->Canvas->Brush->Color = clGreen;
      }

    int TheLeft = 0;

    if ( State.Contains ( odSelected ))
      {
      TheLeft = 2;
      TabBmp->Width = TabBmp->Width - 4;
      TabBmp->Height = TabBmp->Height - 2;
      }

    TRect RectBtn = ::Rect ( 0, 0, TabBmp->Width, TabBmp->Height );
/*
    if ( ( FTabColorization == tcPage || State.Contains ( odSelected ) ) && Pages [ Index ]->FTransparent )
      {
      ValidateParentImage ();

      TRect SRect = RectBtn;
      OffsetRect ( (RECT *)&SRect, Rect.Left + TheLeft, Rect.Top );
      TabBmp->Canvas->CopyRect ( RectBtn, FParentBackgroundBuffer->Canvas, SRect );
      }

    else
*/
    if ( BackgroundBmp && ( ! BackgroundBmp->Empty ))
      {
      TRect SRect = RectBtn;
      OffsetRect ( (RECT *)&SRect, Rect.Left + TheLeft, Rect.Top );
      TabBmp->Canvas->CopyRect ( RectBtn, BackgroundBmp->Canvas, SRect );
      }

    else
      TabBmp->Canvas->FillRect ( RectBtn );

    if ( FCloudColorization != ccNone && PageIndex == FHighlitedItem )
      {
      int CloudVCorection = 0;
      int CloudYCorection = 0;
      int CloudXCorection = 0;
      if ( TabPosition == tpBottom )
        CloudYCorection = 2;

//      if ( TabPosition == tpRight )
//        CloudXCorection = 2;

//      if ( FTabStyles.Contains ( tabButton ))
      if ( Style != tsTabs )
        {
        if ( State.Contains ( odSelected ))
          CloudVCorection = 6;

        else
          CloudVCorection = 2;
        }

      else
        {
        if ( !State.Contains ( odSelected ))
            CloudVCorection = 4;
            
        else
          if ( TabPosition == tpLeft )
            CloudVCorection = -2;
        }


      TColor EndColor = FHotTrackCloudColor;

      if ( FCloudColorization == ccPage )
        EndColor = Pages [ PageIndex ]->FHotTrackCloudColor;

      TRect CloudRect;
      
      if ( TabPosition == tpLeft )
        CloudRect = ::Rect ( TopCorrection + CloudXCorection, 2, TabBmp->Width + TopCorrection + CloudVCorection, TabBmp->Height - 2 );
//        CloudRect = ::Rect ( TopCorrection + CloudXCorection, 2 + CloudYCorection, TabBmp->Width + TopCorrection + CloudVCorection, TabBmp->Height - 6 );

      else if ( TabPosition == tpRight )
        CloudRect = ::Rect ( 4 - CloudVCorection/*- TopCorrection */, 2, TabBmp->Width + TopCorrection - CloudXCorection, TabBmp->Height - 6 );
//        CloudRect = ::Rect ( 4 + TopCorrection + CloudXCorection, 2 + CloudYCorection, TabBmp->Width - 6 + TopCorrection + CloudVCorection, TabBmp->Height - 6 );
//        CloudRect = ::Rect ( 4 + TopCorrection + CloudXCorection, 2 + CloudYCorection, TabBmp->Width - 6 + TopCorrection + CloudVCorection, TabBmp->Height - 6 );

      else
        CloudRect = ::Rect ( 2 + CloudXCorection, 4 + TopCorrection + CloudYCorection, TabBmp->Width - 6, TabBmp->Height - 6 + TopCorrection + CloudVCorection );

      if (( ! Pages [ PageIndex ]->FBackgroundBitmap->Empty ) || Pages [ PageIndex ]->FTransparent )
        HiglightRectangle ( TabBmp, CloudRect, EndColor, ( FFlashing ) ? FCloudSize : 1 );

      else
        HiglightRectangle1 ( TabBmp, CloudRect, TabBmp->Canvas->Brush->Color, EndColor, ( FFlashing ) ? FCloudSize : 1 );
      }

    String Text = Tabs->Strings[TabIndex];

    int TextWidth = TabBmp->Canvas->TextWidth ( Text );
    if ( FTabStyles.Contains ( tabCenterText ))
      {
      CentralizeCorection = TabBmp->Width - TextWidth - (( FShowGlyphs ) ? ( 16 + 4 ) : 0 );
      CentralizeCorection /= 2;
      }

    Controls::TImageList     *ImageList;
    if ( FShowGlyphs )
      {
      if ( HotTrack && ! ComponentState.Contains (csDesigning))
        {
        if ( PageIndex == FHighlitedItem && FHighlightGlyphts )
          {
          if ( State.Contains ( odSelected ) )
            ImageList = SelectedHighlightedGlyphs;

          else
            ImageList = HighlightedGlyphs;
          }

        else if ( State.Contains ( odSelected ) )
          ImageList = Glyphs;

        else
          {
          if ( FGrayInactive )
            ImageList = GrayedGlyphs;

          else
            ImageList = Glyphs;
          }
        }

      else
        {
        if ( FGrayInactive && ! State.Contains ( odSelected ) )
          ImageList = GrayedGlyphs;

        else
          ImageList = Glyphs;
        }


      int XImagePosV;
      int YImagePosV;

      if ( FTabStyles.Contains ( tabLabelLeft ))
        {
        XImagePosV = TextWidth + CentralizeCorection + 2 + 6;
        YImagePosV = 4 + TopCorrection;
        }

      else
        {
        XImagePosV = CentralizeCorection + 2;
        YImagePosV = 4 + TopCorrection;
        LeflMargin = Glyphs->Width + 4;
        }

      int XImagePos;
      int YImagePos;

#ifdef __BCB_40__
      if ( TabPosition == tpLeft || TabPosition == tpRight )
        {
        XImagePos = YImagePosV;
        YImagePos = XImagePosV;
        if ( TabPosition == tpLeft )
          {
          YImagePos = Rect.Height () - YImagePos - 16;
          if ( State.Contains ( odSelected ))
            {
            LeflMargin += 3;
            YImagePos -= 3;
            XImagePos -= 1;
            TopCorrection -= 1;
            }
          }

        else
          {
          if ( State.Contains ( odSelected ))
            {
            LeflMargin += 3;
            YImagePos += 3;
            }
          }
        }

      else
#endif
        {
        XImagePos = XImagePosV;
        YImagePos = YImagePosV;
        }

      if ( PageIndex == FHighlitedItem && FHotTrackGlyphtStyle != dsNormal )
        {
        ((TTempImageList *)ImageList)->GetImages ( PageIndex, ImageBmp, MaskBmpBlack );
        MaskBmp->Canvas->CopyMode = cmSrcCopy;
        MaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );
        
        if ( FHotTrackGlyphtStyle == dsInset || FHotTrackGlyphtStyle == dsRaised )
          {
          if ( FHotTrackGlyphtStyle == dsRaised )
            ImageBmp->Canvas->Brush->Color = clBtnHighlight;

          else
            ImageBmp->Canvas->Brush->Color = clBtnShadow;


          DrawGlyphtShadow ( -1, XImagePos, YImagePos );

          if ( FHotTrackGlyphtStyle == dsRaised )
            ImageBmp->Canvas->Brush->Color = clBtnShadow;

          else
            ImageBmp->Canvas->Brush->Color = clBtnHighlight;

          MaskBmp->Canvas->CopyMode = cmSrcCopy;
          MaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );

          DrawGlyphtShadow ( 1, XImagePos, YImagePos );
          }

        else if ( FHotTrackGlyphtStyle == dsShadow )
          {
          ImageBmp->Canvas->Brush->Color = clBtnShadow;
          DrawGlyphtShadow ( 1, XImagePos, YImagePos );
          }
        }

      ImageList->Draw ( TabBmp->Canvas, XImagePos, YImagePos, PageIndex __DRAW_TRUE__ );
      }

    TabBmp->Canvas->Brush->Style = bsClear;

    switch ( TabPosition )
      {
      case tpLeft:
        MakeAngleFont ( TabBmp->Canvas, 900 );
//        LeflMargin = Rect.Height () - LeflMargin; // - TabBmp->Canvas->TextWidth ( Text );
//        LeflMargin = TabBmp->Canvas->TextWidth ( Text );
        break;

      case tpRight:
        MakeAngleFont ( TabBmp->Canvas, 900 + 1800 );
        break;

      case tpTop:
      case tpBottom:
          ;
//        MakeAngleFont ( TabBmp->Canvas, 0 );
      }

#ifdef __BCB_40__
    if ( TabPosition == tpLeft || TabPosition == tpRight )
      {
      XLeflMargin = 4 + TopCorrection;
      YLeflMargin = CentralizeCorection + LeflMargin;
      if ( TabPosition == tpLeft )
        {
        YLeflMargin = Rect.Height () - YLeflMargin;
        }

      else
        {
        XLeflMargin = Rect.Width () - 6 - TopCorrection;
/*
        if ( State.Contains ( odSelected ))
          XLeflMargin = Rect.Width () - 6; //XLeflMargin;

        else
          XLeflMargin = Rect.Width () - 4; //XLeflMargin;
*/
        }
      }

    else
#endif
      {
      XLeflMargin = CentralizeCorection + LeflMargin;
      YLeflMargin = 4 + TopCorrection;
      }

    if ( PageIndex == FHighlitedItem )
      {
      if ( FHotTrackTextStyle == dsInset || FHotTrackTextStyle == dsRaised )
        {
        if ( FHotTrackTextStyle == dsRaised )
          TabBmp->Canvas->Font->Color = clBtnHighlight;

        else
          TabBmp->Canvas->Font->Color = clBtnShadow;

        TabBmp->Canvas->TextOut ( XLeflMargin - 1, YLeflMargin - 1, Text );
        TabBmp->Canvas->TextOut ( XLeflMargin - 1, YLeflMargin, Text );
        TabBmp->Canvas->TextOut ( XLeflMargin, YLeflMargin - 1, Text );

        if ( FHotTrackTextStyle == dsRaised )
          TabBmp->Canvas->Font->Color = clBtnShadow;

        else
          TabBmp->Canvas->Font->Color = clBtnHighlight;

        TabBmp->Canvas->TextOut ( XLeflMargin + 1, YLeflMargin + 1, Text );
        TabBmp->Canvas->TextOut ( XLeflMargin + 1, YLeflMargin, Text );
        TabBmp->Canvas->TextOut ( XLeflMargin, YLeflMargin + 1, Text );
        }

      else if ( FHotTrackTextStyle == dsShadow )
        {
        TabBmp->Canvas->Font->Color = clBtnShadow;
        TabBmp->Canvas->TextOut ( LeflMargin + 1, YLeflMargin + 1, Text );
        }

      TabBmp->Canvas->Font->Color = FHotTrackColor;
      }

    else
      TabBmp->Canvas->Font->Color = Font->Color;

    TabBmp->Canvas->TextOut ( XLeflMargin, YLeflMargin, Text );

    int StoreMode = TabBmp->Canvas->CopyMode;

    TabBmp->Canvas->CopyMode = StoreMode;

    FCanvas->Draw ( Rect.Left + TheLeft, Rect.Top, TabBmp );
    TabBmp->Canvas->Brush->Style = bsSolid;

    }

}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetHighlightSound ( Bmwave::TBMCustomSound* Value )
{
  FHighlightSound = Value;
//  FHighlightWave->Assign ( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetPulseSound ( Bmwave::TBMCustomSound* Value )
{
  FPulseSound = Value;
//  FPulseWave->Assign ( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetSelectSound ( Bmwave::TBMCustomSound* Value )
{
  FSelectSound = Value;
//  FSelectWave->Assign ( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::ViewChange ()
{
  FullBroadcast ( this, BM_CHANGE_VIEW, 0, 0 );
}
//---------------------------------------------------------------------------
void __fastcall TBMPageControl::SetTransparent ( bool Value )
{
  if ( Value != FTransparent )
    {
    FTransparent = Value;
    Invalidate ();
    ViewChange ();
    }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMTabSheet::TBMTabSheet(Classes::TComponent* AOwner) :
  Comctrls::TTabSheet ( AOwner)
{
  Init ();
}
//---------------------------------------------------------------------------
__fastcall TBMTabSheet::TBMTabSheet(HWND ParentWindow) :
   Comctrls::TTabSheet(ParentWindow )
{
  Init ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::Init ()
{
  FTransparentLevel = 100;
  FBackgroundStyle = bgsStretched;
  FHighlightSound = NULL;
  FPulseSound = NULL;
  FSelectSound = NULL;
/*
  FHighlightWave = new TBMWaveData;
  FPulseWave = new TBMWaveData;
  FSelectWave = new TBMWaveData;
*/  
  FCanvas = new TControlCanvas;
  
//  Color = clBtnFace;
  FHotTrackCloudColor = clYellow;
  Brush->Color = Color;

  FGlyph = new Graphics::TBitmap;
  FGlyph->OnChange = ChangeGlypht;

  RealGlyph = new Graphics::TBitmap;

  FGlyphUnselected = new Graphics::TBitmap;
  FGlyphUnselected->OnChange = ChangeGlyphUnselectedt;

  RealGlyphUnselected = new Graphics::TBitmap;

  FGlyphHighlighted = new Graphics::TBitmap;
  FGlyphHighlighted->OnChange = ChangeGlyphHighlightedt;

  RealGlyphHighlighted = new Graphics::TBitmap;

  FGlyphSelectedHighlighted = new Graphics::TBitmap;
  FGlyphSelectedHighlighted->OnChange = ChangeGlyphSelectedHighlightedt;

  RealGlyphSelectedHighlighted = new Graphics::TBitmap;

  FBackgroundBitmap = new Graphics::TBitmap;
  FBackgroundBitmap->OnChange = ImageChange;
  FBackgroundBuffer = new Graphics::TBitmap;

  RealGlyph->Width = 16;
  RealGlyph->Height = 16;

  RealGlyph->Canvas->CopyMode = cmSrcCopy;
  RealGlyph->Canvas->Pen->Color = clBlack;
  RealGlyph->Canvas->Brush->Color = clBlack;
  RealGlyph->Canvas->Rectangle( 0, 0, RealGlyph->Height, RealGlyph->Width );

  RealGlyphUnselected->Width = 16;
  RealGlyphUnselected->Height = 16;

  RealGlyphUnselected->Canvas->CopyMode = cmSrcCopy;
  RealGlyphUnselected->Canvas->Pen->Color = clBlack;
  RealGlyphUnselected->Canvas->Brush->Color = clBlack;
  RealGlyphUnselected->Canvas->Rectangle( 0, 0, RealGlyphUnselected->Height, RealGlyphUnselected->Width );

  RealGlyphHighlighted->Width = 16;
  RealGlyphHighlighted->Height = 16;

  RealGlyphSelectedHighlighted->Canvas->CopyMode = cmSrcCopy;
  RealGlyphSelectedHighlighted->Canvas->Pen->Color = clBlack;
  RealGlyphSelectedHighlighted->Canvas->Brush->Color = clBlack;
  RealGlyphSelectedHighlighted->Canvas->Rectangle( 0, 0, RealGlyphHighlighted->Height, RealGlyphHighlighted->Width );

  RealGlyphSelectedHighlighted->Width = 16;
  RealGlyphSelectedHighlighted->Height = 16;

  RealGlyphSelectedHighlighted->Canvas->CopyMode = cmSrcCopy;
  RealGlyphSelectedHighlighted->Canvas->Pen->Color = clBlack;
  RealGlyphSelectedHighlighted->Canvas->Brush->Color = clBlack;
  RealGlyphSelectedHighlighted->Canvas->Rectangle( 0, 0, RealGlyphHighlighted->Height, RealGlyphHighlighted->Width );
}
//---------------------------------------------------------------------------
__fastcall TBMTabSheet::~TBMTabSheet(void)
{
  delete RealGlyph;
  delete FGlyph;

  delete RealGlyphUnselected;
  delete FGlyphUnselected;

  delete RealGlyphHighlighted;
  delete FGlyphHighlighted;

  delete RealGlyphSelectedHighlighted;
  delete FGlyphSelectedHighlighted;

  delete FBackgroundBitmap;
  delete FBackgroundBuffer;

  RealGlyph = NULL;
  FGlyph = NULL;

  RealGlyphUnselected = NULL;
  FGlyphUnselected = NULL;

  RealGlyphHighlighted = NULL;
  FGlyphHighlighted = NULL;

  RealGlyphSelectedHighlighted = NULL;
  FGlyphSelectedHighlighted = NULL;

  PageControl = NULL;

  delete FCanvas;
//  delete FSelectWave;
//  delete FHighlightWave;
//  delete FPulseWave;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ViewChange ()
{
  if ( PageControl )
    FullBroadcast ( PageControl, BM_CHANGE_VIEW, 0, 0 );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ImageChange ( System::TObject* Sender )
{
  FBackgroundBuffer->Assign ( NULL );
  InvalidateFull ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::TheChangeGlypht ( Graphics::TBitmap *_FGlyph, Graphics::TBitmap *_RealGlyph )
{
  if ( _FGlyph->Empty )
    _RealGlyph->Canvas->Rectangle( 0, 0, _RealGlyph->Height, _RealGlyph->Width );

  else
    {
    TRect Rect;
    Rect.Top = 0;
    Rect.Left = 0;
    Rect.Bottom = 16;
    Rect.Right = 16;
    _RealGlyph->Canvas->CopyRect ( Rect, _FGlyph->Canvas, Rect );
    }

  if ( PageControl )
    if ( dynamic_cast <TBMPageControl *> ( PageControl ))
    ( ( TBMPageControl *)PageControl )->UpdateGlyphs ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::Notification(Classes::TComponent* AComponent, Classes::TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  if (Operation == opRemove )
    {
    if ( AComponent == FHighlightSound )
      SetHighlightSound ( NULL );

    if ( AComponent == FPulseSound )
      SetPulseSound ( NULL );

    if ( AComponent == FSelectSound )
      SetSelectSound ( NULL );
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetHighlightSound ( Bmwave::TBMCustomSound* Value )
{
  FHighlightSound = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetPulseSound ( Bmwave::TBMCustomSound* Value )
{
  FPulseSound = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetSelectSound ( Bmwave::TBMCustomSound* Value )
{
  FSelectSound = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ChangeGlypht ( System::TObject* Sender )
{
  TheChangeGlypht ( FGlyph, RealGlyph );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ChangeGlyphUnselectedt ( System::TObject* Sender )
{
  TheChangeGlypht ( FGlyphUnselected, RealGlyphUnselected );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ChangeGlyphHighlightedt ( System::TObject* Sender )
{
  TheChangeGlypht ( FGlyphHighlighted, RealGlyphHighlighted );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ChangeGlyphSelectedHighlightedt ( System::TObject* Sender )
{
  TheChangeGlypht ( FGlyphSelectedHighlighted, RealGlyphSelectedHighlighted );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetGlyph ( Graphics::TBitmap *AGlyph )
{
  FGlyph->Assign ( AGlyph );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetGlyphUnselected ( Graphics::TBitmap *AGlyph )
{
  FGlyphUnselected->Assign ( AGlyph );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetGlyphHighlighted ( Graphics::TBitmap *AGlyph )
{
  FGlyphHighlighted->Assign ( AGlyph );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetGlyphSelectedHighlighted ( Graphics::TBitmap *AGlyph )
{
  FGlyphSelectedHighlighted->Assign ( AGlyph );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetBackgroundBitmap ( Graphics::TBitmap *AGlyph )
{
  FBackgroundBitmap->Assign ( AGlyph );
  FBackgroundBuffer->Assign ( NULL );
  InvalidateFull ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetTransparent ( bool Value )
{
  if ( Value != FTransparent )
    {
    FTransparent = Value;
    FBackgroundBuffer->Assign ( NULL );
    InvalidateFull ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetTransparentLevel ( int Value )
{
  if ( Value > 100 )
    Value = 100;

  if ( Value < 1 )
    Value = 1;
    
  if ( Value != FTransparentLevel )
    {
    FTransparentLevel = Value;
    if ( FTransparent )
      {
      FBackgroundBuffer->Assign ( NULL );
      InvalidateFull ();
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetBackgroundStyle ( TBackgroundStyle Value )
{
  if ( FBackgroundStyle != Value )
    {
    FBackgroundStyle = Value;
    FBackgroundBuffer->Assign ( NULL );
    InvalidateFull ();
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::InvalidateFull ()
{
  Invalidate ();
  if ( PageControl )
    PageControl->Invalidate ();

  ViewChange ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::SetHotTrackCloudColor   ( TColor AColor )
{
  if ( FHotTrackCloudColor != AColor )
    {
    FHotTrackCloudColor = AColor;
    InvalidateFull ();
    }
}
//---------------------------------------------------------------------------
/*
void __fastcall TBMTabSheet::SetColor ( TColor AColor )
{
  if ( FColor != AColor )
    {
    FColor = AColor;
    Brush->Color = AColor;
    InvalidateFull ();
    }
}
*/
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::ProcessTransparentLevel ()
{
  if ( FBackgroundBitmap->Empty )
    ColorizeDIB ( FBackgroundBuffer, (TColor)ColorToRGB ( Color ), FTransparentLevel );

  else
    {
    Graphics::TBitmap *TmpBackgroundBuffer = new Graphics::TBitmap;
    TmpBackgroundBuffer->Width = FBackgroundBuffer->Width;
    TmpBackgroundBuffer->Height = FBackgroundBuffer->Height;
     
    if ( FBackgroundStyle == bgsStretched )
      TmpBackgroundBuffer->Canvas->StretchDraw( ::Rect ( 2, 2, TmpBackgroundBuffer->Width - 2, TmpBackgroundBuffer->Height - 2 ), FBackgroundBitmap );

    else
      {
      for ( int x = 0; x < TmpBackgroundBuffer->Width; x += FBackgroundBitmap->Width )
        for ( int y = 0; y < TmpBackgroundBuffer->Height; y += FBackgroundBitmap->Height )
          {
          TmpBackgroundBuffer->Canvas->Draw( x, y , FBackgroundBitmap );
          }
          
      }
      
    MixDIBs ( FBackgroundBuffer, TmpBackgroundBuffer, FTransparentLevel );
    delete TmpBackgroundBuffer;
    }

}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TBMTabSheet::RefreshBuffer ()
{
  if ( FBackgroundBitmap->Empty && ( ! ( FTransparent && PageControl )))
    return NULL;

  if ( FBackgroundBuffer->Empty || FBackgroundBuffer->Width != ((TBMPageControl *)PageControl)->Width || FBackgroundBuffer->Height != ((TBMPageControl *)PageControl)->Height )
    {
    FBackgroundBuffer->Width = ((TBMPageControl *)PageControl)->Width;
    FBackgroundBuffer->Height = ((TBMPageControl *)PageControl)->Height;
    if ( FTransparent && PageControl )
      {
      ((TBMPageControl*)PageControl)->ValidateParentImage ();
      FBackgroundBuffer->Canvas->Draw( 0, 0, ((TBMPageControl*)PageControl)->FParentBackgroundBuffer );
      if ( FTransparentLevel < 100 )
        ProcessTransparentLevel ();
      }

    else if ( FBackgroundStyle == bgsStretched )
      FBackgroundBuffer->Canvas->StretchDraw( ::Rect ( 2, 2, FBackgroundBuffer->Width - 2, FBackgroundBuffer->Height - 2 ), FBackgroundBitmap );

    else
      {
      for ( int x = 0; x < FBackgroundBuffer->Width; x += FBackgroundBitmap->Width )
        for ( int y = 0; y < FBackgroundBuffer->Height; y += FBackgroundBitmap->Height )
          {
          FBackgroundBuffer->Canvas->Draw( x, y , FBackgroundBitmap );
          }
          
      }
    }
    
  return FBackgroundBuffer;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::InvalidateParentBorder ()
{
//  if ( ((TBMPageControl *)PageControl)->FTabStyles.Contains ( tabButton ))
  if ( ((TBMPageControl *)PageControl)->Style != tsTabs )
    return;

  int VCorection = 0;
  if ( PageControl->TabPosition == tpBottom )
    VCorection = 2;

  if ( PageControl->TabPosition != tpLeft )
    InvalidateRect( PageControl->Handle, &(RECT)::Rect ( Left - 2, Top - 2 - VCorection, Left, Top + Height + 2 - VCorection ), true );
    
  if ( PageControl->TabPosition != tpRight )
    InvalidateRect( PageControl->Handle, &(RECT)::Rect ( Width + Left, Top - 2, PageControl->Width - 2, Top + Height + 2 - VCorection ), true );

  InvalidateRect( PageControl->Handle, &(RECT)::Rect ( Left - 2, Top - 2 - VCorection, PageControl->Width - 2, Top ), true );
  
  if ( PageControl->TabPosition != tpBottom )
    InvalidateRect( PageControl->Handle, &(RECT)::Rect ( Left - 2, Top + Height, PageControl->Width - 2, Top + Height + 2 ), true );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::CMColorChanged ( TMessage& Msg )
{
  Brush->Color = Color;
  
  if ( FTransparent )
    FBackgroundBuffer->Assign ( NULL );

  InvalidateFull ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::BMChangeView    ( Messages::TMessage    &Msg )
{
  FBackgroundBuffer->Assign ( NULL );
  inherited::Dispatch ( &Msg );
  Invalidate ();
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::WMPaint(TMessage& Msg)
{
  InvalidateParentBorder ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::WMSize( TMessage& Msg )
{
  if ( ! FBackgroundBitmap->Empty )
    Invalidate ();

  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::WMMove(TMessage& Msg)
{
  if ( ! FBackgroundBitmap->Empty )
    {
    if ( FTransparent )
       FBackgroundBuffer->Assign ( NULL );

    Invalidate ();
    }

  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::DrawBkg ( HDC hDC )
{
  FCanvas->Handle = (HDC) hDC;

  if ( ! FBackgroundBitmap->Empty || ( FTransparent && Parent ))
/*
    {
    ((TBMPageControl*)PageControl)->ValidateParentImage ();
    TRect Rect;
    TPoint Point = ::Point ( Left, Top);

    Rect = ::Rect ( -Point.x, -Point.y, PageControl->Width -Point.x, PageControl->Height - Point.y );
    FCanvas->Draw( Rect.Left, Rect.Top, ((TBMPageControl*)PageControl)->FParentBackgroundBuffer );
//    CopyParentImage ( this, Parent->Parent, FCanvas );
    }

  else if ( ! FBackgroundBitmap->Empty )
*/
    {
    TRect Rect;
    TPoint Point = ::Point ( Left, Top);

    Rect = ::Rect ( -Point.x, -Point.y, PageControl->Width -Point.x, PageControl->Height - Point.y );

    FCanvas->Draw( Rect.Left, Rect.Top, RefreshBuffer () );
    }

  else
    {
    TColor _Color = FCanvas->Brush->Color;
    FCanvas->Brush->Color = Color;

    FCanvas->FillRect ( ::Rect ( 0, 0, Width, Height ));
//    inherited::Dispatch ( &Msg );

    FCanvas->Brush->Color = _Color;
    }

  FCanvas->Handle = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMTabSheet::WMEraseBkgnd ( TMessage& Msg )
{
  InvalidateParentBorder ();
  DrawBkg ( (HDC) Msg.WParam );
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall PACKAGE Register()
{
  TComponentClass classes1[1] = {__classid(TBMPageControl)};
  TComponentClass classes2[1] = {__classid(TBMTabSheet) };
  RegisterClasses(classes2, 0 );
  RegisterComponents("BMitov", classes1, 0);
  
//  RegisterComponentEditor(classes1[0], __classid(TBMPageControlEditor));
//  RegisterComponentEditor(classes2[0], __classid(TBMPageControlEditor));
}
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------


