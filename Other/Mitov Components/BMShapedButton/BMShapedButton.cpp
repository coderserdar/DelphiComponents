//---------------------------------------------------------------------------
#include <vcl.h>
#include <mmsystem.h>
#include <algorithm>
//#include <syncobjs.hpp>
//---> Begin ED
#include <stdlib.h>
// End ED

#pragma hdrstop

#include "BMShapedButton.h"

#pragma package(smart_init)

#pragma link "Controls"
//#pragma link "syncobjs"

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

//#define _BMSHAPED_TRACE


#ifdef _BMSHAPED_TRACE
  #include <fstream.h>
#endif

#define TIMER_RESOLUTION 10   // expressed in milliseconds, binds timer
                              // to 0.1-second resolution
#define TIMER_INTERVAL 10     // expressed in milliseconds, binds timer
                              // to 0.1-second resolution

namespace Bmshapedbutton
{

#ifdef _BMSHAPED_TRACE
int DebugOffset = 0;

class TMiniTrace
{
  String Message;

public :
  TMiniTrace ( String _Message ) :
    Message ( _Message )
    {
    ofstream of ( "c:\\Trece.txt", ios::app );
    for ( int i = 0; i < DebugOffset; i ++ )
      of << ' ';

    of << Message.c_str () << " >>\n";
    DebugOffset ++;
    }

  ~TMiniTrace ()
    {
    DebugOffset --;
    ofstream of ( "c:\\Trece.txt", ios::app );

    for ( int i = 0; i < DebugOffset; i ++ )
      of << ' ';

    of << Message.c_str () << " <<\n";
    }
};

  #define TR(A)   TMiniTrace __Trace ( A )
#else
  #define TR(A)
#endif


static  TTimer *ButtonMouseTimer = NULL;
static  int ButtonCount = 0;
static  Graphics::TBitmap *Pattern = NULL;
static  TColor PatternBtnFace, PatternBtnHighlight;
static  bool ButtonsStayDown = true;
static  TBMShapedButton *ButtonMouseInControl = NULL;

static const int DropdownComboWidth = 11;

static  const long int ROP_DSPDxax = 0x00E20746;
static  const long int ROP_PSDPxax = 0x00B8074A;
static  const long int ROP_DSna = 0x00220326;  //{ D & ~S }

//---------------------------------------------------------------------------
static unsigned int uTimerID = 0;
static TList    *BmBtnsList;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TBMCriticalSection
{
protected :
  CRITICAL_SECTION	csCriticalSection;

  int                   Entries;

public :
  TBMCriticalSection ();
  ~TBMCriticalSection ();

public :
  void	Begin ();
  void	End ();

  void	Enter () { Begin (); }
  void	Leave () { End ();   }
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
void	TBMCriticalSection::Begin ()
{
//  if ( ! Entries ++ )
    EnterCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
void	TBMCriticalSection::End ()
{
//  if ( ! ( --Entries ))
    LeaveCriticalSection ( &csCriticalSection );
}
//---------------------------------------------------------------------------
TBMCriticalSection *Section;
//---------------------------------------------------------------------------
static void CALLBACK
timeoutCallback(unsigned int /*timerId*/,
                unsigned int /*msg*/,
                unsigned long /*timeoutFunction*/,
                unsigned long /*dw1*/,
                unsigned long /*dw2*/)
{
  uTimerID = -1;
  if ( ! Section )
    return;

  Section->Enter ();
  for ( int i = 0; i < BmBtnsList->Count; i ++ )
    {
    ((TBMShapedButton *)BmBtnsList->Items [ i ])->UpdateHighlighted ();
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
    BmBtnsList = new TList;
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

    delete BmBtnsList;

    delete TmpPointer;
    }
};
//---------------------------------------------------------------------------
TLocalStarter   __LocalStarter;
//---------------------------------------------------------------------------

static inline void ValidCtrCheck(TBMShapedButton *)
{
    new TBMShapedButton(NULL);
}

//---------------------------------------------------------------------------
inline bool Odd( int Y )
{
  return Y == ( Y / 2 ) * 2;
}
//---------------------------------------------------------------------------
void __fastcall CreateBrushPattern ()
{
  int X, Y;

  PatternBtnFace = (TColor)GetSysColor(COLOR_BTNFACE);
  PatternBtnHighlight = (TColor)GetSysColor(COLOR_BTNHIGHLIGHT);
  Pattern = new Graphics::TBitmap;
//  with Pattern do {
    Pattern->Width = 8;
    Pattern->Height = 8;
//    with Canvas do {
      Pattern->Canvas->Brush->Style = bsSolid;
      Pattern->Canvas->Brush->Color = clBtnFace;
      Pattern->Canvas->FillRect (Rect(0, 0, Pattern->Width, Pattern->Height));
      for ( Y = 0; Y <= 7; Y ++ )
        for ( X = 0; X <= 7; X ++ )
          if ( Odd(Y) == Odd(X) )  // toggles between even/odd pixels
            Pattern->Canvas->Pixels [ X ] [ Y ] = clBtnHighlight;     // on even/odd rows
//    }
//  }
}
//---------------------------------------------------------------------------
class TGlyphList : public TImageList
{
  typedef TImageList inherited;
private :
    TBits *Used;
    int FCount;
    __fastcall int AllocateIndex ();

    const int FReliefSize;
    Graphics::TBitmap *TmpImage;
    Graphics::TBitmap *TmpImage1;

private :
//    void __fastcall CheckImage ( TGraphic *Image );
    void __fastcall BugfreeReplaceMasked ( int Index, Graphics::TBitmap *NewImage, TColor MaskColor );

public :
    __fastcall TGlyphList ( int AWidth, int AHeight, int _FReliefSize );
    __fastcall ~TGlyphList ();
    int __fastcall Add ( Graphics::TBitmap *Image, Graphics::TBitmap *Mask );
    int __fastcall AddMasked ( Graphics::TBitmap *Image, TColor MaskColor );
    void __fastcall Delete ( int Index );

public :
    __property int Count = { read=FCount };
};
//---------------------------------------------------------------------------
__fastcall TGlyphList::TGlyphList ( int AWidth, int AHeight, int _FReliefSize ) :
  TImageList (AWidth, AHeight),
  FReliefSize ( _FReliefSize )
{
  Used = new TBits;
  TmpImage = new Graphics::TBitmap;
  TmpImage1 = new Graphics::TBitmap;

  TmpImage->Width = AWidth;
  TmpImage->Height = AHeight;
  TmpImage1->Width = AWidth;
  TmpImage1->Height = AHeight;
}

__fastcall TGlyphList::~TGlyphList ()
{
  delete TmpImage;
  delete TmpImage1;
  delete Used;
}

int __fastcall TGlyphList::AllocateIndex ()
{
  int Result = Used->OpenBit ();
  if ( Result >= Used->Size )
  {
    Result = inherited::Add(NULL, NULL);
    Used->Size = Result + 1;
  }
  
  Used->Bits [ Result ] = true;

  return Result;
}

int __fastcall TGlyphList::Add ( Graphics::TBitmap *Image, Graphics::TBitmap *Mask )
{
  int Result = AllocateIndex ();

/*
  TmpImage->Canvas->Brush->Color = (TColor) ColorToRGB( Image->Canvas->Pixels [ 0 ][ Image->Height - 1] );
  TmpImage->Canvas->FillRect ( ::Rect ( 0, 0, Width, Height ));
  TmpImage->Canvas->Draw ( ( Width - Image->Width ) / 2, ( Height - Image->Height ) / 2, Image );

  TmpImage1->Canvas->Brush->Color = (TColor) ColorToRGB( Mask->Canvas->Pixels [ 0 ][ Mask->Height - 1] );
  TmpImage1->Canvas->FillRect ( ::Rect ( 0, 0, Width, Height ));
  TmpImage1->Canvas->Draw ( ( Width - Mask->Width ) / 2, ( Height - Mask->Height ) / 2, Mask );
*/

  Replace (Result, Image, Mask);
//  Replace (Result, TmpImage, TmpImage1);
  FCount ++;
  return Result;
}

/*
void __fastcall TGlyphList::CheckImage ( TGraphic *Image )
{
  if ( ! Image )
    return;

  if ((Image->Height < Height - FReliefSize * 2 ) || (Image->Width < Width - FReliefSize * 2 ))
    throw new EInvalidOperation ( "Invalid Image Size" );
}
*/

void __fastcall TGlyphList::BugfreeReplaceMasked ( int Index, Graphics::TBitmap *NewImage, TColor MaskColor )
{
    int TempIndex;
    Graphics::TBitmap *Image;
    Graphics::TBitmap *Mask;

    if ( HandleAllocated () )
      {
//      CheckImage( NewImage );
      if ( MaskColor == clDefault )
        TmpImage->Canvas->Brush->Color = NewImage->Canvas->Pixels [ 0 ][ NewImage->Height - 1];

      else
        TmpImage->Canvas->Brush->Color = MaskColor;

      TmpImage->Canvas->FillRect ( ::Rect ( 0, 0, Width, Height ));
      TmpImage->Canvas->Draw ( ( Width - NewImage->Width ) / 2, ( Height - NewImage->Height ) / 2, NewImage );
      TempIndex = inherited::AddMasked(TmpImage, MaskColor);
      if ( TempIndex != -1 )
        {
        try
          {
          Image = NULL;
          Mask = NULL;
          try
            {
            Image = new Graphics::TBitmap;
            Image->Height = Height;
            Image->Width = Width;
            Mask = new Graphics::TBitmap;
            Mask->Monochrome = true;
            /* ^ Prevents the "invisible glyph" problem when used with certain
                color schemes. (Fixed in Delphi 3.01) */
            Mask->Height = Height;
            Mask->Width = Width;
            ImageList_Draw ((_IMAGELIST*)Handle, TempIndex, Image->Canvas->Handle, 0, 0, ILD_NORMAL);
            ImageList_Draw ((_IMAGELIST*)Handle, TempIndex, Mask->Canvas->Handle, 0, 0, ILD_MASK);
            if ( ! ImageList_Replace((_IMAGELIST*)Handle, Index, Image->Handle, Mask->Handle) )
              throw new EInvalidOperation ( "Replace Image" );
            }
          __finally
            {
            delete Image;
            delete Mask;
            }
          }
        __finally
          {
          inherited::Delete(TempIndex);
          }
        }
      else
        throw new EInvalidOperation ( "Replace Image" );

      }
      
  Change ();
}
  
int __fastcall TGlyphList::AddMasked ( Graphics::TBitmap *Image, TColor MaskColor )
{
  int Result = AllocateIndex ();
  /* This works two very serious bugs in the Delphi 2/BCB and Delphi 3
    implementations of the ReplaceMasked method. In the Delphi 2 and BCB
    versions of the ReplaceMasked method, it incorrectly uses ILD_NORMAL as
    the last parameter for the second ImageList_Draw call, in effect causing
    all white colors to be considered transparent also. And in the Delphi 2/3
    and BCB versions it doesn't set Monochrome to true on the Mask bitmap,
    causing the bitmaps to be invisible on certain color schemes. */
    
  BugfreeReplaceMasked (Result, Image, MaskColor);
  FCount ++;

  return Result;
}

void __fastcall TGlyphList::Delete ( int Index )
{
  if ( Used->Bits[Index] )
    {
    FCount --;
    Used->Bits[Index] = false;
    }
}

//{ TGlyphCache }


//---------------------------------------------------------------------------
class TGlyphCache
{
private:
    TList *GlyphLists;

public:
    __fastcall TGlyphCache ();
    __fastcall ~TGlyphCache ();

    TGlyphList *__fastcall GetList( int AWidth, int AHeight, int FReliefSize );
    void __fastcall ReturnList( TGlyphList *List );
    bool __fastcall Empty ();
};
class TGlyphCache;

static TGlyphCache *GlyphCache  = NULL;

__fastcall TGlyphCache::TGlyphCache()
{
  GlyphLists = new TList;
}

__fastcall TGlyphCache::~TGlyphCache()
{
  delete GlyphLists;
}

TGlyphList *__fastcall TGlyphCache::GetList( int AWidth, int AHeight, int FReliefSize )
{
  TGlyphList *Result;
  int I;
  for ( I = GlyphLists->Count; I --; )
    {
    Result = (TGlyphList *)GlyphLists->Items[I];
//    with Result do
      if ((AWidth == Result->Width) && (AHeight == Result->Height) )
        return Result;
    }
    
  Result = new TGlyphList (AWidth, AHeight, FReliefSize );
  GlyphLists->Add(Result);
  return Result;
}

void __fastcall TGlyphCache::ReturnList( TGlyphList *List )
{
  if ( ! List )
    return;

  if ( List->Count == 0 )
    {
    GlyphLists->Remove(List);
    delete List;
//---> Begin ED
//    List = NULL;
// End ED
    }
}

bool __fastcall TGlyphCache::Empty ()
{
  return GlyphLists->Count == 0;
}


//---------------------------------------------------------------------------
struct TBoolInt
{
  bool  B;
  int   I;
};


class TButtonGlyph : public TObject
{
  friend class TBMShapedButton;

private :
    Graphics::TBitmap *FOriginal; //, *FOriginalMask;
    Graphics::TBitmap *ImageBmp;
//    Graphics::TBitmap *DrawBmp;
    Graphics::TBitmap *TheMaskBmp;
    Graphics::TBitmap *MaskBmpBlack;
    bool FCallDormant;
    TGlyphList *FGlyphList [ 2 ];
    int FImageIndex;
    TCustomImageList *FImageList;
    TChangeLink *FImageChangeLink;
    int FIndexs [2][7];
    TColor FTransparentColor;
    TBMNumGlyphs FNumGlyphs;
    TNotifyEvent FOnChange;
    bool FOldDisabledStyle;
    void __fastcall GlyphChanged ( TObject *Sender );
    void __fastcall SetGlyph ( Graphics::TBitmap *Value );
//    void __fastcall SetGlyphMask ( Graphics::TBitmap *Value );
    void __fastcall SetNumGlyphs ( TBMNumGlyphs Value );
    void __fastcall UpdateNumGlyphs ();
    TBoolInt __fastcall CreateButtonGlyph ( TBMButtonState State, int FReliefSize, bool InDesign );
    void __fastcall DrawButtonGlyph ( TCanvas *Canvas, const TPoint GlyphPos, TBMButtonState State, int FReliefSize, bool InDesign );
    void __fastcall DrawGlyphtShadow ( TCanvas *Canvas, TCanvas *MaskCanvas, int ShadowOffset, int XImagePos, int YImagePos, bool Extended );

    void __fastcall DrawButtonText ( TCanvas *Canvas, const String Caption,
      TRect TextBounds, bool WordWrap, TAlignment Alignment, TBMButtonState State, int FReliefSize, TColor FHotTrackColor );

    //---> Begin ED
    TPoint __fastcall GetGlyphSize ( bool DrawGlyph );
    TPoint __fastcall GetArrowSize ( bool DropArrow );
    //void __fastcall CalcButtonLayout ( TCanvas *Canvas, const TRect Client,
    //  const TPoint Offset, bool DrawGlyph, bool DrawCaption, const String Caption,
    //  bool WordWrap, TButtonLayout Layout, int Margin, int Spacing, bool DropArrow,
    //  TPoint &GlyphPos, TPoint &ArrowPos, TRect &TextBounds, int FReliefSize );
    void __fastcall CalcButtonLayout ( TCanvas *Canvas, const TRect Client,
      const TPoint Offset, bool DrawGlyph, bool DrawCaption, const String Caption,
      bool WordWrap, TButtonLayoutPlus Layout, int Margin, int Spacing, bool DropArrow,
      TPoint &GlyphPos, TPoint &ArrowPos, TRect &TextBounds, int FReliefSize );
    // End ED

private:
  void __fastcall GenerateMaskBitmapFromDIB ( Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *SourceBitmap,
      const TPoint SourceOffset, const TPoint SourceSize, TColor *TransColors, int ArraySize );

  void __fastcall GenerateMaskBitmap ( Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *SourceBitmap,
    const TPoint SourceOffset, const TPoint SourceSize, const TColor *TransColors, int ArraySize );

  void __fastcall ReplaceBitmapColorsFromMask (Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *DestBitmap,
    const TPoint DestOffset, const TPoint DestSize, const TColor ReplaceColor );


  Graphics::TBitmap *__fastcall CopyBitmapToDDB ( Graphics::TBitmap *SourceBitmap );

public:
    __fastcall TButtonGlyph ();
    virtual __fastcall ~TButtonGlyph ();
    //---> Begin ED
    TPoint __fastcall RecommendedSize ( TCanvas *Canvas, bool DrawGlyph, bool DrawCaption,
      String const Caption, bool WordWrap, TButtonLayoutPlus Layout, int Margin, int Spacing,
      bool DropArrow, int FReliefSize, bool Border, bool Flat );
    // returns the text rectangle
    //TRect __fastcall Draw ( TCanvas *Canvas, const TRect Client, const TPoint Offset,
    //  bool DrawGlyph, bool DrawCaption, String const Caption, bool WordWrap,
    //  TAlignment Alignment, TButtonLayout Layout, int Margin, int Spacing,
    //  bool DropArrow, TBMButtonState State, int FReliefSize, TColor FHotTrackColor, bool InDesign );
    TRect __fastcall Draw ( TCanvas *Canvas, const TRect Client, const TPoint Offset,
      bool DrawGlyph, bool DrawCaption, String const Caption, bool WordWrap,
      TAlignment Alignment, TButtonLayoutPlus Layout, int Margin, int Spacing,
      bool DropArrow, TBMButtonState State, int FReliefSize, TColor FHotTrackColor, bool InDesign );
    // End ED
    void __fastcall Invalidate ();
    void __fastcall DrawButtonDropArrow ( TCanvas *Canvas, const int X, const int Y, TBMButtonState State );
    __property Graphics::TBitmap *Glyph = { read=FOriginal, write=SetGlyph };
//    __property Graphics::TBitmap *GlyphMask = { read=FOriginalMask, write=SetGlyphMask };
    __property TBMNumGlyphs NumGlyphs = {read=FNumGlyphs, write=SetNumGlyphs };
    __property TNotifyEvent OnChange = {read=FOnChange, write=FOnChange };
};
//---------------------------------------------------------------------------
__fastcall TButtonGlyph::TButtonGlyph ()
{
  bool B;
  TBMButtonState I;

  FCallDormant = true;
  FImageIndex = -1;

  FOriginal = new Graphics::TBitmap;
  ImageBmp = new Graphics::TBitmap;
//  TabBmp = new Graphics::TBitmap;
  TheMaskBmp = new Graphics::TBitmap;
  MaskBmpBlack = new Graphics::TBitmap;

  FOriginal->OnChange = GlyphChanged;
//  FOriginalMask = new Graphics::TBitmap;
//  FOriginalMask->OnChange = GlyphChanged;
  FNumGlyphs = 1;
  for ( B = false; B <= true; B ++ )
    for ( I = Bmshapedbutton::bsUp; I <= Bmshapedbutton::bsMouseIn; I = (TBMButtonState)( I + 1 ) )
      FIndexs[B][I] = -1;

  if ( ! GlyphCache )
    GlyphCache = new TGlyphCache;
}
//---------------------------------------------------------------------------
__fastcall TButtonGlyph::~TButtonGlyph ()
{
//  delete FOriginalMask;
  delete FImageChangeLink;
  Invalidate ();
  if ( GlyphCache && GlyphCache->Empty ())
    {
    delete GlyphCache;
    GlyphCache = NULL;
  }

  delete FOriginal;
  delete ImageBmp;
//  delete TabBmp;
  delete MaskBmpBlack;
  delete TheMaskBmp;
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::Invalidate ()
{
  bool B;
  TBMButtonState I;

  ImageBmp->Width = FOriginal->Width;
  ImageBmp->Height = FOriginal->Height;

//  TabBmp->Width = FOriginal->Width;
//  TabBmp->Height = FOriginal->Height;

  MaskBmpBlack->Width = FOriginal->Width;
  MaskBmpBlack->Height = FOriginal->Height;

  TheMaskBmp->Width = FOriginal->Width;
  TheMaskBmp->Height = FOriginal->Height;

  for ( B = false; B <= true; B ++ )
    {
    for ( I = Bmshapedbutton::bsUp; I <= Bmshapedbutton::bsMouseIn; I = (TBMButtonState)( I + 1 ) )
//for B = false to true do {
//  for I = Low(I) to High(I) do
      if ( FIndexs[B][I] != -1 )
        {
        FGlyphList[B]->Delete ( FIndexs[B][I]);
        FIndexs[B][I] = -1;
        }

    GlyphCache->ReturnList (FGlyphList[B]);
    FGlyphList[B] = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::GlyphChanged ( TObject *Sender )
{
  if ((Sender == FOriginal) && (FOriginal->Width != 0) && (FOriginal->Height != 0))
    FTransparentColor = (TColor) ( FOriginal->Canvas->Pixels[0][FOriginal->Height-1] | 0x02000000 );
  Invalidate ();
  if ( FOnChange )
    FOnChange (this);
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::UpdateNumGlyphs ()
{

  int Glyphs;

  if ((FOriginal->Width != 0) && (FOriginal->Height != 0) &&
     (FOriginal->Width % FOriginal->Height == 0) )
    {
    Glyphs = FOriginal->Width / FOriginal->Height;
    if ( Glyphs > ((short)Glyphs) )
      Glyphs = 1;
    }
    
  else
    Glyphs = 1;
    
  SetNumGlyphs ((TBMNumGlyphs)Glyphs);
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::SetGlyph ( Graphics::TBitmap *Value )
{
  Invalidate ();
  FOriginal->Assign (Value);
  UpdateNumGlyphs ();
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::SetNumGlyphs ( TBMNumGlyphs Value )
{
  Invalidate ();
  if ((FImageList != NULL) /* || (Value < Low(TNumGlyphs97)) ||
     (Value > High(TNumGlyphs97) */ )
    FNumGlyphs = 1;

  else
    FNumGlyphs = Value;

  GlyphChanged (NULL);
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::GenerateMaskBitmapFromDIB ( Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *SourceBitmap,
    const TPoint SourceOffset, const TPoint SourceSize, TColor *TransColors, int ArraySize )
  /* This a special void __fastcall meant for generating monochrome masks from
    >4 bpp color DIB sections. Because each video driver seems to sport its own
    interpretation of how to handle DIB sections, a workaround void __fastcall like
    this was necessary. */
{
//  typedef TColorRef TColorArray [ 536870910 + 1 ];

#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef Colors[2];
  } Info;
#pragma pack(pop)


    int W, H;
    int I, Y, X;
//    TColorArray *Pixels;
    TColorRef *Pixels;
    TColorRef *Pixel;
    Pointer MonoPixels;
    Byte *MonoPixel, *StartMonoPixel;
    int MonoScanLineSize, CurBit;
    HDC DC;
    HBITMAP MaskBmp;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;
    MonoScanLineSize = SourceSize.x / 8;
    if ( SourceSize.x % 8 != 0 )
      MonoScanLineSize ++;
      
    if ( MonoScanLineSize % 4 != 0 )  //{ Compensate for scan line boundary }
      MonoScanLineSize = (MonoScanLineSize & ~3) + 4;
      
    MonoPixels = new Byte [ MonoScanLineSize * SourceSize.y ];  //{ AllocMem is used because it initializes to zero }
    memset ( MonoPixels, 0, MonoScanLineSize * SourceSize.y );  
    try
      {
//      Pixels = (TColorArray *)new Byte [ W * H * 4 ];
      Pixels = new TColorRef [ W * H ];
      try
        {
        memset ( &Info, 0, sizeof(Info));
        Info.Header.biSize = sizeof(TBitmapInfoHeader);
        Info.Header.biWidth = W;
        Info.Header.biHeight = -H;  //{ negative number makes it a top-down DIB }
        Info.Header.biPlanes = 1;
        Info.Header.biBitCount = 32;
        Info.Colors[1] = clWhite;
        DC = CreateCompatibleDC(0);
        GetDIBits ( DC, SourceBitmap->Handle, 0, H, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );
        DeleteDC (DC);

        for ( I = 0; I < ArraySize; I ++ )
          if ( TransColors[I] == -1 )
            {
            TransColors[I] = (TColor)( ( (int)Pixels [ W * (H-1) ]) & 0x0FFFFFF );
            }
//              { ^ 'and' operation is necessary because the high byte is undefined }

        MonoPixel = (unsigned char *)MonoPixels;
        for ( Y = SourceOffset.y; Y < SourceOffset.y + SourceSize.y; Y ++ )
          {
          StartMonoPixel = MonoPixel;
          CurBit = 7;
          Pixel = Pixels + (Y * W) + SourceOffset.x;
          for ( X = 0; X < SourceSize.x; X ++ )
            {
            for ( I = 0; I < ArraySize; I ++ )
              if ( Cardinal( *Pixel & 0xFFFFFF ) == Cardinal(TransColors[I]) )
                {
//                { ^ 'and' operation is necessary because the high byte is undefined }
                *MonoPixel |= (Byte)( 1 << CurBit );
                break;
                }
                
            CurBit --;
            if ( CurBit < 0 )
              {
              MonoPixel ++;
              CurBit = 7;
              }
              
//            Inc (Integer(Pixel), SizeOf(Longint));  { proceed to the next pixel }
            Pixel ++;
            }
            
          MonoPixel = StartMonoPixel + MonoScanLineSize;
          }
        }

      __finally
        {
        delete [] Pixels;
        }
//      }
      

//      { Write new bits into a new HBITMAP, and assign this handle to MaskBitmap }
      MaskBmp = CreateBitmap(SourceSize.x, SourceSize.y, 1, 1, NULL);
//      with Info.Header do {
        Info.Header.biWidth = SourceSize.x;
        Info.Header.biHeight = -SourceSize.y;  //{ negative number makes it a top-down DIB }
        Info.Header.biPlanes = 1;
        Info.Header.biBitCount = 1;
//      }
      DC = CreateCompatibleDC(0);
      SetDIBits (DC, MaskBmp, 0, SourceSize.y, MonoPixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);
      DeleteDC (DC);
      }
    __finally
      {
//---> Begin ED
//      delete MonoPixels;
      delete [] MonoPixels;
// End ED
      }

    MaskBitmap->Handle = MaskBmp;
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::GenerateMaskBitmap ( Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *SourceBitmap,
    const TPoint SourceOffset, const TPoint SourceSize, const TColor *TransColors, int ArraySize )
  /* Returns handle of a monochrome bitmap, with pixels in SourceBitmap of color
    TransColor set to white in the resulting bitmap. All other colors of
    SourceBitmap are set to black in the resulting bitmap. This uses the
    regular ROP_DSPDxax BitBlt method. */
{
    HDC CanvasHandle;
    TColorRef SaveBkColor;
    HDC DC;
    HBITMAP MaskBmp, SaveBmp;
    int I;

  const  DWORD ROP [2] =
    {
    SRCPAINT,
    SRCCOPY
    };
    
    CanvasHandle = SourceBitmap->Canvas->Handle;

    MaskBmp = CreateBitmap(SourceSize.x, SourceSize.y, 1, 1, NULL);
    DC = CreateCompatibleDC(0);
    SaveBmp = SelectObject(DC, MaskBmp);
    SaveBkColor = GetBkColor(CanvasHandle);
    for ( I = 0; I < ArraySize; I++ )
      {
      SetBkColor (CanvasHandle, ColorToRGB(TransColors[I]));
      BitBlt (DC, 0, 0, SourceSize.x, SourceSize.y, CanvasHandle,
        SourceOffset.x, SourceOffset.y, ROP[I == 0]);
      }

    SetBkColor (CanvasHandle, SaveBkColor);
    SelectObject (DC, SaveBmp);
    DeleteDC (DC);

    MaskBitmap->Handle = MaskBmp;
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::ReplaceBitmapColorsFromMask (Graphics::TBitmap *MaskBitmap, Graphics::TBitmap *DestBitmap,
    const TPoint DestOffset, const TPoint DestSize, const TColor ReplaceColor )
{
    HDC DestDC;

    HBRUSH SaveBrush;
    
    TColorRef SaveTextColor, SaveBkColor;
    
    DestDC = DestBitmap->Canvas->Handle;

    SaveBrush = SelectObject(DestDC, CreateSolidBrush(ColorToRGB(ReplaceColor)));
    SaveTextColor = SetTextColor(DestDC, clBlack);
    SaveBkColor = SetBkColor(DestDC, clWhite);
    BitBlt (DestDC, DestOffset.x, DestOffset.y, DestSize.x, DestSize.y,
      MaskBitmap->Canvas->Handle, 0, 0, ROP_DSPDxax);
    SetBkColor (DestDC, SaveBkColor);
    SetTextColor (DestDC, SaveTextColor);
    DeleteObject (SelectObject(DestDC, SaveBrush));
}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TButtonGlyph::CopyBitmapToDDB ( Graphics::TBitmap *SourceBitmap )
{
//  { Makes a device-dependent duplicate of SourceBitmap-> The color palette,
//    if any, is preserved. }

    HBITMAP SB;
    HPALETTE SavePalette;
    HDC DC;

#pragma pack(push, 1)
  struct TBitmapInfo
  {
  TBitmapInfoHeader Header;
  TColorRef Colors [256];
  } BitmapInfo;
#pragma pack(pop)

    Pointer Bits;

    Graphics::TBitmap *Result = new Graphics::TBitmap;
    try
      {
      Result->Palette = CopyPalette(SourceBitmap->Palette);
      Result->Width = SourceBitmap->Width;
      Result->Height = SourceBitmap->Height;
      SB = SourceBitmap->Handle;
      if ( SB == 0 )
        return Result;  //{ it would have a null handle if its width or height was zero }

      SavePalette = 0;
      DC = CreateCompatibleDC(0);
      try
        {
        if ( Result->Palette != 0 )
          {
          SavePalette = SelectPalette(DC, Result->Palette, false);
          RealizePalette (DC);
          }

        BitmapInfo.Header.biSize = sizeof(TBitmapInfoHeader);

        BitmapInfo.Header.biBitCount = 0;  //{ instructs GetDIBits not to fill in the color table }
//        { First retrieve the BitmapInfo header only }
        if ( GetDIBits ( DC, SB, 0, 0, NULL, (( BITMAPINFO *)&BitmapInfo), DIB_RGB_COLORS) != 0 )
          {
          Bits = new Byte [ BitmapInfo.Header.biSizeImage ];
          try
            {
//            { Then read the actual bits }
            if ( GetDIBits(DC, SB, 0, SourceBitmap->Height, Bits, ( BITMAPINFO * )&BitmapInfo, DIB_RGB_COLORS) != 0 )
//              { And copy them to the resulting bitmap }
              SetDIBits (DC, Result->Handle, 0, SourceBitmap->Height, Bits, ( BITMAPINFO * )&BitmapInfo, DIB_RGB_COLORS);
            }
          __finally
            {
            delete [] Bits;
            }
          }
        }
      __finally
        {
        if ( SavePalette != 0 )
          SelectPalette (DC, SavePalette, false);

        DeleteDC (DC);
        }
      }
    catch (...)
      {
      delete Result;
      throw;
      }

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
namespace
{
TColor MapGrayColor( TColor Color )
{
  unsigned char Index;

//  Color = (TColor)ColorToRGB(Color);
//  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 77 + GetGValue ( Color ) * 150 + GetBValue(Color) * 29 )) >> 8);
  Index = (unsigned char)(((unsigned long int)( GetRValue ( Color ) * 29 + GetGValue ( Color ) * 150 + GetBValue(Color) * 77 )) >> 8);
  return (TColor)RGB(Index, Index, Index);
}
//---------------------------------------------------------------------------
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
//---------------------------------------------------------------------------
typedef TColor (*TColorMapFunc) ( TColor );
//---------------------------------------------------------------------------
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
//      DC = CreateCompatibleDC(0);
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
//---------------------------------------------------------------------------
void __fastcall CloudDIB ( Graphics::TBitmap *SourceBitmap, Graphics::TBitmap *SupportBitmap, TColor ColorizeTo, TColorRef *&Pixels, TColorRef *&Pixels2 )
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
    TColorRef *Pixel2;
    HDC DC;

    W = SourceBitmap->Width;
    H = SourceBitmap->Height;

//    TColorRef *Pixels, *Pixels2;
    if ( ! Pixels )
      {
      Pixels = new TColorRef [ W * H ];
      Pixels2 = new TColorRef [ W * H ];
      }
      
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
      GetDIBits ( DC, SupportBitmap->Handle, 0, H, Pixels2, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );

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

      for ( Y = 0; Y < H; Y ++ )
        {
        Pixel = Pixels + (Y * W);
        Pixel2 = Pixels2 + (Y * W);
        for ( X = 0; X < W; X++ )
          {
          k = GetGValue ( *Pixel2 );
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
          Pixel2 ++;
          }

        }

      SetDIBits (DC, SourceBitmap->Handle, 0, H, Pixels, (( TBitmapInfo *)&Info),
        DIB_RGB_COLORS);

      DeleteDC (DC);
      }
        
    __finally
      {
//      delete Pixels;
//      delete Pixels2;
      }

}


};

//---------------------------------------------------------------------------

TBoolInt __fastcall TButtonGlyph::CreateButtonGlyph ( TBMButtonState State, int FReliefSize, bool InDesign )
{

      class TTempImageList : public TImageList
      {
      public :
        void __fastcall GetImages(int Index, Graphics::TBitmap* Image, Graphics::TBitmap* Mask)
          {
          TImageList::GetImages( Index, Image, Mask );
          }
      };


const DWORD ROPs [2] =
  {
  ROP_PSDPxax,
  ROP_DSPDxax
  };

  Graphics::TBitmap *OriginalBmp;
//  Graphics::TBitmap *OriginalMaskBmp;
  Graphics::TBitmap *TmpImage;
  Graphics::TBitmap *DDB;
  Graphics::TBitmap *MonoBmp;
  Graphics::TBitmap *MaskBmp;
  Graphics::TBitmap *UseMaskBmp;
  TBoolInt Result;

  TBMButtonState I;
  bool B;
  int AddPixels, IWidth, IHeight, IWidthA, IHeightA;
  TRect IRect, IRectA, SourceRect, R;
  HDC DC;
//  bool UsesMask;
  bool IsHighColorDIB;

//  if ((State != Bmshapedbutton::bsDisabled ) && ( State >= NumGlyphs))
//    State = Bmshapedbutton::bsUp;

  Result.B = true;
  Result.I = FIndexs[true][State];
  if ( Result.I == -1 )
    {
    Result.B = false;
    Result.I = FIndexs[false][State];
    }

  if ( Result.I != -1 )
    return Result;

  if ( FImageList == NULL )
    {
    if ((FOriginal->Width == 0) || (FOriginal->Height == 0))
      return Result;

//    UsesMask = (FOriginalMask->Width != 0) && (FOriginalMask->Height != 0);
    }

  else
    {
    if ((FImageIndex < 0) || (FImageIndex >= FImageList->Count))
      return Result;

//    UsesMask = false;
    }

  B = State != Bmshapedbutton::bsDisabled;
//  { + AddPixels is to make sure the highlight color on generated disabled glyphs
//    doesn't get cut off }
  if ( ! FImageList )
    {
    IWidthA = FOriginal->Width / FNumGlyphs;
    IHeightA = FOriginal->Height;
    }

  else
    {
    IWidthA = ((TImageList*)FImageList)->Width;
    IHeightA = ((TImageList*)FImageList)->Height;
    }

  IRectA = Rect(0, 0, IWidthA, IHeightA);
  AddPixels = ( State == Bmshapedbutton::bsDisabled );
  IWidth = IWidthA + AddPixels;
  IHeight = IHeightA + AddPixels;
  IRect = Rect(0, 0, IWidth, IHeight);
  if ( FGlyphList[B] == NULL )
    {
    if ( GlyphCache == NULL )
      GlyphCache = new TGlyphCache;

    FGlyphList[B] = GlyphCache->GetList(IWidth + FReliefSize * 2, IHeight + FReliefSize * 2, FReliefSize );
    }
//  {$IFDEF TB97D3}
  IsHighColorDIB = (FImageList == NULL) && (FOriginal->PixelFormat > pf4bit);
//  {$ENDIF}
  OriginalBmp = NULL;
//  OriginalMaskBmp = NULL;
  TmpImage = NULL;
  MaskBmp = NULL;
  try
    {
    OriginalBmp = new Graphics::TBitmap;
    OriginalBmp->Assign (FOriginal);
//    OriginalMaskBmp = new Graphics::TBitmap;
//    OriginalMaskBmp->Assign (FOriginalMask);
    TmpImage = new Graphics::TBitmap;
    TmpImage->Width = IWidth;
    TmpImage->Height = IHeight;
    TmpImage->Canvas->Brush->Color = clBtnFace;
    if ( FImageList == NULL )
      TmpImage->Palette = CopyPalette(OriginalBmp->Palette);
    I = State;
    if ( I >= NumGlyphs )
      I = Bmshapedbutton::bsUp;

    SourceRect = Bounds( I * IWidthA, 0, IWidthA, IHeightA);
    if ( FImageList != NULL )
      {
      MaskBmp = new Graphics::TBitmap;
      MaskBmp->Monochrome = true;
      MaskBmp->Width = IWidthA;
      MaskBmp->Height = IHeightA;
      int TheHandle = FImageList->Handle;
      ImageList_Draw ( (_IMAGELIST *)TheHandle, FImageIndex, MaskBmp->Canvas->Handle,
        0, 0, ILD_MASK);
    }

    if ( State != Bmshapedbutton::bsDisabled )
      {
      if ( FImageList != NULL )
        {
//        FImageList->Draw(TmpImage->Canvas, 0, 0, FImageIndex __DRAW_TRUE__ );
//        TmpImage->Canvas->Brush->Color = (TColor)( 1 << 0xFF );
//        TmpImage->Canvas->FillRect ( ::Rect ( 0, 0, TmpImage->Width, TmpImage->Height ));
        int TheHandle = FImageList->Handle;
        ImageList_Draw ((_IMAGELIST *)TheHandle, FImageIndex, TmpImage->Canvas->Handle,
          0, 0, ILD_NORMAL );

        ImageBmp->Width = ((TImageList *)FImageList)->Width;
        ImageBmp->Height = ((TImageList *)FImageList)->Height;

        MaskBmpBlack->Width = ((TImageList *)FImageList)->Width;
        MaskBmpBlack->Height = ((TImageList *)FImageList)->Height;

        TheMaskBmp->Width = ((TImageList *)FImageList)->Width;
        TheMaskBmp->Height = ((TImageList *)FImageList)->Height;
//        TmpImage->SaveToFile ( "c:\\boza.bmp" );
        }

      else
        {
        TmpImage->Canvas->CopyRect (IRectA, OriginalBmp->Canvas, SourceRect);
        }


        {
//          {$IFDEF TB97D3}
          /* Use clDefault instead of FTransparentColor whereever possible to
            ensure compatibility with all video drivers when using high-color
            (> 4 bpp) DIB glyphs */
        Graphics::TBitmap *TmpBmp1 = new Graphics::TBitmap;
        Graphics::TBitmap *MaskTmpBmp1 = new Graphics::TBitmap;
        TmpBmp1->Width = TheMaskBmp->Width / FNumGlyphs + 2 * FReliefSize;
        TmpBmp1->Height = TheMaskBmp->Height + 2 * FReliefSize;
        TCanvas *Canvas = TmpBmp1->Canvas;

        MaskTmpBmp1->Width = TmpBmp1->Width;
        MaskTmpBmp1->Height = TmpBmp1->Height;
        TCanvas *MaskCanvas = MaskTmpBmp1->Canvas;

        if ( State == bsExclusive || State == bsMouseInExclusive || State == bsMouseIn || State == bsDown || State == bsMouseInDown )
          {
          Canvas->Brush->Color = (TColor)ColorToRGB ( TmpImage->Canvas->Pixels [ 0 ][ TmpImage->Height -1 ] );
          Canvas->FillRect ( ::Rect ( 0, 0, TmpBmp1->Width, TmpBmp1->Height ));
          
          MaskCanvas->Brush->Color = clWhite;
          MaskCanvas->FillRect ( ::Rect ( 0, 0, TmpBmp1->Width, TmpBmp1->Height ));
          
          TColor Color1, Color2;
          if ( State == bsDown || State == bsExclusive || State == bsMouseInExclusive || State == bsMouseInDown )
            {
            Color1 = (TColor)ColorToRGB ( clBtnShadow );
            Color2 = (TColor)ColorToRGB ( clBtnHighlight );
            }

          else
            {
            Color1 = (TColor)ColorToRGB ( clBtnHighlight );
            Color2 = (TColor)ColorToRGB ( clBtnShadow );
            }

          TImageList *TempList = new TImageList ( TmpImage->Width, TmpImage->Height );
//          TempList->Width = ImageBmp->Width;
//          TempList->Height = ImageBmp->Height;

          if ( State == bsMouseIn || State == bsMouseInDown || State == bsMouseInExclusive )
            {
            TColorRef *DIBArray = new TColorRef [ TmpImage->Width * TmpImage->Height ];
            CallForDIB ( TmpImage, MapHighColor, DIBArray );
//---> Begin ED
//            delete DIBArray;
            delete[]DIBArray;
// End ED
            }

          TempList->AddMasked ( TmpImage, clDefault );
//          ((TTempImageList *)FGlyphList[B])->GetImages ( bsUp, ImageBmp, MaskBmpBlack );
          ((TTempImageList *)TempList)->GetImages ( 0, ImageBmp, MaskBmpBlack );

          TheMaskBmp->Canvas->CopyMode = cmSrcCopy;
          TheMaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );
          ImageBmp->Canvas->Brush->Color = Color2;
          DrawGlyphtShadow ( Canvas, MaskCanvas, FReliefSize, FReliefSize, FReliefSize, true );

          TheMaskBmp->Canvas->CopyMode = cmSrcCopy;
          TheMaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );
          ImageBmp->Canvas->Brush->Color = Color1;
          DrawGlyphtShadow ( Canvas, MaskCanvas, -FReliefSize, FReliefSize, FReliefSize, false );

          TheMaskBmp->Canvas->CopyMode = cmSrcCopy;
          TheMaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );
          ImageBmp->Canvas->Brush->Color = Color1;
          DrawGlyphtShadow ( Canvas, MaskCanvas, - ( FReliefSize - 1 ), FReliefSize, FReliefSize, true );

          TheMaskBmp->Canvas->CopyMode = cmSrcCopy;
          TheMaskBmp->Canvas->Draw ( 0, 0, MaskBmpBlack );
          ImageBmp->Canvas->Brush->Color = Color2;
          DrawGlyphtShadow ( Canvas, MaskCanvas, FReliefSize - 1, FReliefSize, FReliefSize, false );

          TempList->Draw ( Canvas, FReliefSize, FReliefSize, 0 __DRAW_TRUE__ );
//          Canvas->Draw ( FReliefSize, FReliefSize, TmpImage );
//          FIndexs[B][State] = FGlyphList[B]->AddMasked ( TmpImage, clDefault );
//          FIndexs[B][State] = FGlyphList[B]->AddMasked ( TmpBmp1, clDefault );
          FIndexs[B][State] = FGlyphList[B]->Add ( TmpBmp1, MaskTmpBmp1 );

          TempList->Delete ( 0 );

          delete TempList;
          }

        else
          {
          if ( ! InDesign )
            {
            TColorRef *DIBArray = new TColorRef [ TmpImage->Width * TmpImage->Height ];
            CallForDIB ( TmpImage, MapGrayColor, DIBArray );
//---> Begin ED
//            delete DIBArray;
            delete[]DIBArray;
// End ED
            }

          Canvas->Brush->Color = (TColor)ColorToRGB ( TmpImage->Canvas->Pixels [ 0 ][ TmpImage->Height -1 ] );
          Canvas->FillRect ( ::Rect ( 0, 0, TmpBmp1->Width, TmpBmp1->Height ));
          Canvas->Draw ( FReliefSize, FReliefSize, TmpImage );
          FIndexs[B][State] = FGlyphList[B]->AddMasked ( TmpBmp1 /*TmpImage*/, clDefault );
          }
          
        delete TmpBmp1;
        delete MaskTmpBmp1;
        }

/*
      else
        {
        // To be fixed !!!
        int TheHandle = FImageList->Handle;
        ImageList_Draw ((_IMAGELIST *)TheHandle, FImageIndex, TmpImage->Canvas->Handle,
          0, 0, ILD_NORMAL);

        FIndexs[B][State] = FGlyphList[B]->Add(TmpImage, MaskBmp);
        }
*/
      }

    else
      {
      MonoBmp = NULL;
      DDB = NULL;
      try
        {
        MonoBmp = new Graphics::TBitmap;
        /* Uses the CopyBitmapToDDB to work around a Delphi 3 flaw. If you copy
          a DIB to a second bitmap via Assign, change the HandleType of the
          second bitmap to bmDDB, then try to read the Handle property, Delphi
          converts it back to a DIB. */
        if ( FImageList == NULL )
          {
          DDB = CopyBitmapToDDB(OriginalBmp);
/*
          DDB = new Graphics::TBitmap;
          DDB->Width = OriginalBmp->Width;
          DDB->Height = OriginalBmp->Height;
          DDB->HandleType = bmDDB;
          DDB->Canvas->Draw ( 0, 0, OriginalBmp );
*/
          }

        else
          {
          DDB = new Graphics::TBitmap;
          DDB->Width = IWidthA;
          DDB->Height = IHeightA;
          ImageList_Draw ((_IMAGELIST *)FImageList->Handle, FImageIndex, DDB->Canvas->Handle,
            0, 0, ILD_NORMAL);
          }
          
        if ( NumGlyphs > 1 )
            {
//          with TmpImage->Canvas do {
            TmpImage->Canvas->CopyRect (IRectA, DDB->Canvas, SourceRect);

//            { Convert white to clBtnHighlight }
            TColor Colors [1];
            if ( ! IsHighColorDIB )
              {
              Colors [ 0 ] = (TColor)GetNearestColor(OriginalBmp->Canvas->Handle, clWhite);

              GenerateMaskBitmap (MonoBmp, DDB, ::Point ( SourceRect.Left, SourceRect.Top ),
                ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1 );
              }

            else
              {
              Colors [ 0 ] = clWhite;
              GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, ::Point ( SourceRect.Left, SourceRect.Top ),
                ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1);
            ReplaceBitmapColorsFromMask (MonoBmp, TmpImage, ::Point ( IRectA.Left, IRectA.Top ),
              ::Point ( IRectA.Right, IRectA.Bottom ), clBtnHighlight);
              }

//            { Convert gray to clBtnShadow }
            if ( ! IsHighColorDIB )
              {
              Colors [ 0 ] = (TColor)GetNearestColor(OriginalBmp->Canvas->Handle, clGray);
              GenerateMaskBitmap (MonoBmp, DDB, ::Point ( SourceRect.Left, SourceRect.Top ),
                ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1 );
              }

            else
              {
              Colors [ 0 ] = clGray;
              GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, ::Point ( SourceRect.Left, SourceRect.Top ),
                ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1 );
              }
              
            ReplaceBitmapColorsFromMask (MonoBmp, TmpImage, ::Point ( IRectA.Left, IRectA.Top ),
              ::Point ( IRectA.Right, IRectA.Bottom ), clBtnShadow);

//            if ( ! UsesMask )
              {
              /* Generate the transparent mask in MonoBmp-> The reason why
                it doesn't just use a mask color is because the mask needs
                to be of the glyph -before- the clBtnHighlight/Shadow were
                translated */
                
              if ( ! IsHighColorDIB )
                {
                Colors [ 0 ] = FTransparentColor;
                GenerateMaskBitmap (MonoBmp, DDB,
                  ::Point ( SourceRect.Left, SourceRect.Top ), ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1 );
                }
              else
                {
                Colors [ 0 ] = (TColor)-1;
                GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                  ::Point ( SourceRect.Left, SourceRect.Top ), ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1 );
                }
              }
/*
            else
//              {
              MonoBmp->Canvas->CopyRect (IRectA, OriginalMaskBmp->Canvas, SourceRect);
*/
//            with MonoBmp do {
              MonoBmp->Width = MonoBmp->Width + AddPixels;
              MonoBmp->Height = MonoBmp->Height + AddPixels;
              /* Set the additional bottom and right row on disabled glyph
                masks to white so that it always shines through, since the
                bottom and right row on TmpImage was left uninitialized */
              MonoBmp->Canvas->Pen->Color = clWhite;
              TPoint Array [ 3 ];
              Array [ 0 ] = ::Point(0, MonoBmp->Height-1);
              Array [ 1 ] = ::Point(MonoBmp->Width-1, MonoBmp->Height-1);
              Array [ 2 ] = ::Point(MonoBmp->Width-1, -1);

//              MonoBmp->Canvas->Polyline ([Point(0, MonoBmp->Height-1), Point(MonoBmp->Width-1, MonoBmp->Height-1),
//                Point(MonoBmp->Width-1, -1)]);
              MonoBmp->Canvas->Polyline (Array, 2);
//            }
            Graphics::TBitmap *TmpBmp1 = new Graphics::TBitmap;
            Graphics::TBitmap *TmpBmp2 = new Graphics::TBitmap;
            TmpBmp1->Width = FGlyphList[B]->Width;
            TmpBmp1->Height = FGlyphList[B]->Height;
            TmpBmp2->Width = TmpBmp1->Width;
            TmpBmp2->Height = TmpBmp1->Height;
            TCanvas *Canvas = TmpBmp1->Canvas;
            Canvas->Brush->Color = (TColor)ColorToRGB ( TmpImage->Canvas->Pixels [ 0 ][ TmpImage->Height -1 ] );
            Canvas->FillRect ( ::Rect ( 0, 0, TmpBmp1->Width, TmpBmp1->Height ));
            Canvas->Draw ( FReliefSize, FReliefSize, TmpImage );

            Canvas = TmpBmp2->Canvas;
            Canvas->Brush->Color = (TColor)ColorToRGB ( MonoBmp->Canvas->Pixels [ 0 ][ MonoBmp->Height -1 ] );
            Canvas->FillRect ( ::Rect ( 0, 0, TmpBmp2->Width, TmpBmp2->Height ));
            Canvas->Draw ( FReliefSize, FReliefSize, MonoBmp );

            FIndexs[B][State] = FGlyphList[B]->Add(TmpBmp1, TmpBmp2);
            delete TmpBmp2;
            delete TmpBmp1;
            }

        else
          {
          // Create a disabled version
          if ( FOldDisabledStyle )
            {
            // "Old" TSpeedButton style
            if ( FImageList == NULL )
              {
//              if ( ! UsesMask )
                {
                if ( IsHighColorDIB )
                  {
                  TColor Colors [ 1 ];
                  Colors [ 0 ] = clBlack;

                  GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                    ::Point ( SourceRect.Left, SourceRect.Top ), ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 1);
                  }
                  
                else
                  {
//                  with MonoBmp do {
                    MonoBmp->Assign (DDB);  // must be a DDB for this to work right
                    MonoBmp->Canvas->Brush->Color = clBlack;
                    MonoBmp->Monochrome = true;
                    //}
                  }
                }
                
/*
              else
                {
                MonoBmp->Assign (DDB);  // must be a DDB for this to work right
                Graphics::TBitmap *Temp = new Graphics::TBitmap;

//                with new Graphics::TBitmap do
                  try
                    {
                    Temp->Monochrome = true;
                    Temp->Width = OriginalMaskBmp->Width;
                    Temp->Height = OriginalMaskBmp->Height;
                    R = Rect(0, 0, Temp->Width, Temp->Height);
                    Temp->Canvas->CopyRect (R, OriginalMaskBmp->Canvas, R);
                    DC = Temp->Canvas->Handle;
//                    with MonoBmp->Canvas do {
                      ::BitBlt ( MonoBmp->Canvas->Handle, 0, 0, IWidthA, IHeightA, DC,
                        SourceRect.Left, SourceRect.Top, ROP_DSna);
                      ::BitBlt (MonoBmp->Canvas->Handle, 0, 0, IWidthA, IHeightA, DC,
                        SourceRect.Left, SourceRect.Top, SRCPAINT);
//                    }
                    }

                  __finally
                    {
                    delete Temp;
                    }

                MonoBmp->Canvas->Brush->Color = clBlack;
                MonoBmp->Monochrome = true;
                }
*/
              }

            else
              {
//              with MonoBmp do {
                MonoBmp->Width = IWidthA;
                MonoBmp->Height = IHeightA;
                MonoBmp->Canvas->Brush->Color = clWhite;
                MonoBmp->Canvas->FillRect (IRectA);
                ImageList_Draw ((_IMAGELIST *)FImageList->Handle, FImageIndex, MonoBmp->Canvas->Handle,
                  0, 0, ILD_TRANSPARENT);
                MonoBmp->Canvas->Brush->Color = clBlack;
                MonoBmp->Monochrome = true;
//                }
              }
            }
          else {
            // The new Office 97 / MFC look
            if ( /* ! UsesMask && */ (FImageList == NULL) )
              {
//              with TmpImage->Canvas do {
              TColor Colors [ 3 ];
                if ( ! IsHighColorDIB )
                  {
                  Colors [ 0 ] = FTransparentColor;
                  Colors [ 1 ] = clWhite;
                  Colors [ 2 ] = clSilver;

                  GenerateMaskBitmap (MonoBmp, DDB, ::Point ( IRectA.Left, IRectA.Top ),
                    ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 3 );
                  }
                    
                else
                  {
                  Colors [ 0 ] = (TColor)-1;
                  Colors [ 1 ] = clWhite;
                  Colors [ 2 ] = clSilver;

                  GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                    ::Point ( SourceRect.Left, SourceRect.Top ), ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 3 );
                  }
//              }
              }
            else
              {
              // Generate the mask in MonoBmp-> Make clWhite and clSilver transparent.
              TColor Colors [ 2 ];
              Colors [ 0 ] = clWhite;
              Colors [ 1 ] = clSilver;
              if ( ! IsHighColorDIB )
                {
                GenerateMaskBitmap (MonoBmp, DDB, ::Point ( SourceRect.Left, SourceRect.Top ),
                  ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 2 );
                }

              else
                {
                GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, ::Point ( SourceRect.Left, SourceRect.Top ),
                  ::Point ( IRectA.Right, IRectA.Bottom ), Colors, 2 );
                }
                
/*
              if ( ! FImageList )
                UseMaskBmp = OriginalMaskBmp;

              else
*/
                UseMaskBmp = MaskBmp;
                
              // and all the white colors in UseMaskBmp
//              with new Graphics::TBitmap do
                Graphics::TBitmap *Temp = new Graphics::TBitmap;
                try
                  {
                  Temp->Monochrome = true;
                  Temp->Width = UseMaskBmp->Width;
                  Temp->Height = UseMaskBmp->Height;
                  R = Rect(0, 0, Temp->Width, Temp->Height);
                  Temp->Canvas->CopyRect (R, UseMaskBmp->Canvas, R);
                  DC = Temp->Canvas->Handle;
//                  with MonoBmp->Canvas do {
                    BitBlt (MonoBmp->Canvas->Handle, 0, 0, IWidthA, IHeightA, DC,
                      SourceRect.Left, SourceRect.Top, ROP_DSna);
                    BitBlt (MonoBmp->Canvas->Handle, 0, 0, IWidthA, IHeightA, DC,
                      SourceRect.Left, SourceRect.Top, SRCPAINT);
                  }
                __finally
                  {
                  delete Temp;
                  }
              }
            }

//          with TmpImage->Canvas do {
            TmpImage->Canvas->Brush->Color = clBtnFace;
            TmpImage->Canvas->FillRect (IRect);
            TmpImage->Canvas->Brush->Color = clBtnHighlight;
            DC = TmpImage->Canvas->Handle;
            SetTextColor (DC, clBlack);
            SetBkColor (DC, clWhite);
            BitBlt (DC, 1, 1, IWidthA, IHeightA,
              MonoBmp->Canvas->Handle, 0, 0, ROPs[FOldDisabledStyle]);
            TmpImage->Canvas->Brush->Color = clBtnShadow;
            DC = TmpImage->Canvas->Handle;
            SetTextColor (DC, clBlack);
            SetBkColor (DC, clWhite);
            BitBlt (DC, 0, 0, IWidthA, IHeightA,
              MonoBmp->Canvas->Handle, 0, 0, ROPs[FOldDisabledStyle]);
//          }

          FIndexs[B][State] = FGlyphList[B]->AddMasked(TmpImage, clBtnFace);
          }
        }
      __finally
        {
        delete DDB;
        delete MonoBmp;
        }
      }
    }
  __finally
    {
    delete MaskBmp;
    delete TmpImage;
//    delete OriginalMaskBmp;
    delete OriginalBmp;
  }
  Result.B = B;
  Result.I = FIndexs[B][State];
  /* Note: Due to a bug in graphics.pas, Delphi 2's VCL crashes if Dormant is
    called on an empty bitmap, so to prevent this it must check Width/Height
    first */
/*
  if {$IFNDEF TB97D3} (FOriginal->Width != 0) and (FOriginal->Height != 0) and {$ENDIF}
     FCallDormant then
    FOriginal->Dormant;
  {$IFNDEF TB97D3} if (FOriginalMask->Width != 0) and (FOriginalMask->Height != 0) then {$ENDIF}
    FOriginalMask->Dormant;
*/
  if ( FCallDormant )
    FOriginal->Dormant ();

//  FOriginalMask->Dormant ();

  return Result;
}


void __fastcall TButtonGlyph::DrawGlyphtShadow ( TCanvas *Canvas, TCanvas *MaskCanvas, int ShadowOffset, int XImagePos, int YImagePos, bool Extended )
{
  MaskCanvas->CopyMode = cmSrcAnd;

  ImageBmp->Canvas->FillRect ( ::Rect ( 0, 0, ImageBmp->Width, ImageBmp->Height ));

  TheMaskBmp->Canvas->CopyMode = cmSrcErase;
  TheMaskBmp->Canvas->Draw ( 0, 0, ImageBmp );

  int OldMode = Canvas->CopyMode;
/*
  for ( int i = ( ShadowOffset > 0 ) ? 1 : -1; ( ShadowOffset > 0 ) ? i <= ShadowOffset : i >= ShadowOffset ; ( ShadowOffset > 0 ) ? i ++ : i -- )
    {
    for ( int j = ( ShadowOffset > 0 ) ? 1 : -1; ( ShadowOffset > 0 ) ? j <= ShadowOffset : j >= ShadowOffset; ( ShadowOffset > 0 ) ? j ++ : j -- )
      {
      Canvas->CopyMode = cmSrcAnd;
      Canvas->Draw ( XImagePos + i, YImagePos + j, MaskBmpBlack );
      Canvas->Draw ( XImagePos, YImagePos + j, MaskBmpBlack );
      Canvas->Draw ( XImagePos + i, YImagePos, MaskBmpBlack );
      Canvas->CopyMode = cmSrcPaint;
      Canvas->Draw ( XImagePos + i, YImagePos + j, TheMaskBmp );
      Canvas->Draw ( XImagePos, YImagePos + j, TheMaskBmp );
      Canvas->Draw ( XImagePos + i, YImagePos, TheMaskBmp );
      }
    }
*/
//  for ( int i = ( ShadowOffset > 0 ) ? 1 : -1; ( ShadowOffset > 0 ) ? i <= ShadowOffset : i >= ShadowOffset ; ( ShadowOffset > 0 ) ? i ++ : i -- )
  for ( int i = 0; ( ShadowOffset > 0 ) ? i <= ShadowOffset : i >= ShadowOffset ; ( ShadowOffset > 0 ) ? i ++ : i -- )
    {
    Canvas->CopyMode = cmSrcAnd;
    if ( Extended )
      {
      Canvas->Draw ( XImagePos + i - ShadowOffset, YImagePos + i, MaskBmpBlack );
      MaskCanvas->Draw ( XImagePos + i - ShadowOffset, YImagePos + i, MaskBmpBlack );
      }

    Canvas->Draw ( XImagePos + i, YImagePos + ShadowOffset, MaskBmpBlack );
    MaskCanvas->Draw ( XImagePos + i, YImagePos + ShadowOffset, MaskBmpBlack );
    Canvas->Draw ( XImagePos + i, YImagePos + i, MaskBmpBlack );
    MaskCanvas->Draw ( XImagePos + i, YImagePos + i, MaskBmpBlack );
    Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + i, MaskBmpBlack );
    MaskCanvas->Draw ( XImagePos + ShadowOffset, YImagePos + i, MaskBmpBlack );

    Canvas->CopyMode = cmSrcPaint;
    if ( Extended )
      Canvas->Draw ( XImagePos + i - ShadowOffset, YImagePos + i, TheMaskBmp );

    Canvas->Draw ( XImagePos + i, YImagePos + ShadowOffset, TheMaskBmp );
    Canvas->Draw ( XImagePos + i, YImagePos + i, TheMaskBmp );
    Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + i, TheMaskBmp );
    }

//  for ( int j = ( ShadowOffset > 0 ) ? 1 : -1; ( ShadowOffset > 0 ) ? j <= ShadowOffset : j >= ShadowOffset; ( ShadowOffset > 0 ) ? j ++ : j -- )
/*
  for ( int j = 0; ( ShadowOffset > 0 ) ? j <= ShadowOffset : j >= ShadowOffset; ( ShadowOffset > 0 ) ? j ++ : j -- )
    {
    Canvas->CopyMode = cmSrcAnd;
    Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + j, MaskBmpBlack );
    Canvas->CopyMode = cmSrcPaint;
    Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + j, TheMaskBmp );
    }
*/

/*
  for ( int i = ( ShadowOffset > 0 ) ? 1 : -1; ( ShadowOffset > 0 ) ? i <= ShadowOffset : i >= ShadowOffset ; ( ShadowOffset > 0 ) ? i ++ : i -- )
    {
    Canvas->CopyMode = cmSrcAnd;
    Canvas->Draw ( XImagePos + i, YImagePos + i, MaskBmpBlack );
//    Canvas->Draw ( XImagePos, YImagePos + i, MaskBmpBlack );
//    Canvas->Draw ( XImagePos + i, YImagePos, MaskBmpBlack );
    Canvas->CopyMode = cmSrcPaint;
    Canvas->Draw ( XImagePos + i, YImagePos + i, TheMaskBmp );
//    Canvas->Draw ( XImagePos, YImagePos + i, TheMaskBmp );
//    Canvas->Draw ( XImagePos + i, YImagePos, TheMaskBmp );
    }
*/

  Canvas->CopyMode = OldMode;
}

void __fastcall DoElliptic( TCanvas *Canvas, TRect Rect, int fr, int fg, int fb, int dr, int dg, int db, int FCloudSize, bool FHugeCloud )
{
  int I;
  int R, G, B;
  int Pw, Ph, Dw, Dh;    
  int x1,y1,x2,y2;

  Canvas->Pen->Style = psClear;
  Canvas->Pen->Mode = pmCopy;
  
  int Width  = Rect.Right - Rect.Left;
  int Height = Rect.Bottom - Rect.Top;

  int   Delimiter;

/*
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
*/



  if ( FHugeCloud )
    {
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
      Pw = ( Width ) / 2;
      Ph = ( Height ) / 2;
      Delimiter = 50;
      }
    }

  else
    {
    int hc, vc;

    if ( FCloudSize == 2 )
      {
      hc = 0;
      vc = 0;
      }

    else if ( FCloudSize == 1 || FCloudSize == 3 )
      {
      hc = (Width / 9);
      vc = (Height / 9);
      }

    else
      {
      hc = (Width / 6);
      vc = (Height / 6);
      }

    x1 = Rect.Left + hc;
    x2 = Rect.Left + Width - hc;
    y1 = Rect.Top + vc;
    y2 = Rect.Top + Height - vc;
    Pw = ( x2 - x1 ) / 2;
    Ph = ( y2 - y1 ) / 2;
    Delimiter = 50;
    }

  for ( I = 0; I <= 50; I++ )          //Make ellipses of color
    {
    R = fr + I * dr / 50;    //Find the RGB values
    G = fg + I * dg / 50;
    B = fb + I * db / 50;
    Canvas->Brush->Color = (TColor)RGB ( R, G, B );   //Plug colors into brush
    Dw = Pw * I / Delimiter;
    Dh = Ph * I / Delimiter;
    Canvas->Ellipse(x1 + Dw,y1 + Dh,x2 - Dw,y2 - Dh);
    }

  Canvas->Pen->Style = psSolid;
}
//---------------------------------------------------------------------------
void HiglightRectangle1 ( Graphics::TBitmap *Bmp, TRect Rect, TColor FBeginClr, TColor FEndClr, int FCloudSize, bool FHugeCloud )
{
  FBeginClr = (TColor)ColorToRGB ( FBeginClr );
  
  int FromR = GetRValue ( FBeginClr );  //Strip out separate RGB values
  int FromG = GetGValue ( FBeginClr );
  int FromB = GetBValue ( FBeginClr );
  int DiffR = (int)GetRValue ( FEndClr ) - FromR;   //Find the difference
  int DiffG = (int)GetGValue ( FEndClr ) - FromG;
  int DiffB = (int)GetBValue ( FEndClr ) - FromB;

  DoElliptic( Bmp->Canvas, Rect, FromR, FromG, FromB, DiffR, DiffG, DiffB, FCloudSize, FHugeCloud );
}
//---------------------------------------------------------------------------
void HiglightRectangle ( Graphics::TBitmap *Bmp, TRect Rect, TColor FEndClr, int FCloudSize, bool FHugeCloud, TColorRef *&Pixels, TColorRef *&Pixels2 )
{
  Graphics::TBitmap *TmpBmp = new Graphics::TBitmap;
  TmpBmp->Width = Bmp->Width;
  TmpBmp->Height = Bmp->Height;
  TmpBmp->Canvas->Brush->Color = clBlack;
  TmpBmp->Canvas->FillRect ( Rect );
  DoElliptic( TmpBmp->Canvas, Rect, 0, 0, 0, 255, 255, 255, FCloudSize, FHugeCloud );

  CloudDIB ( Bmp, TmpBmp, FEndClr, Pixels, Pixels2 );

  delete TmpBmp;
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::DrawButtonGlyph ( TCanvas *Canvas, const TPoint GlyphPos, TBMButtonState State, int FReliefSize, bool InDesign )
{
  TBoolInt Index;

  Index = CreateButtonGlyph(State, FReliefSize, InDesign );
  if ( Index.I != -1 )
    {
    ImageList_DrawEx ((_IMAGELIST *)( FGlyphList[Index.B]->Handle ), Index.I, Canvas->Handle,
      GlyphPos.x, GlyphPos.y, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT);
    }
}
//---------------------------------------------------------------------------
void __fastcall OffsetTextOut ( TCanvas *Canvas, int XTextOffset, int YTextOffset, TRect TextBounds, const String Caption, UINT Format )
{
  TextBounds.Left += XTextOffset;
  TextBounds.Right += XTextOffset;
  TextBounds.Top += YTextOffset;
  TextBounds.Bottom += YTextOffset;

  DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format );
}
//---------------------------------------------------------------------------
void __fastcall DrawTextShadow ( TCanvas *Canvas, int ShadowOffset, const String Caption, TRect TextBounds, UINT Format, bool Extended )
{
  for ( int i = 0; ( ShadowOffset > 0 ) ? i <= ShadowOffset : i >= ShadowOffset ; ( ShadowOffset > 0 ) ? i ++ : i -- )
    {
    Canvas->CopyMode = cmSrcAnd;
    if ( Extended )
      OffsetTextOut ( Canvas, i - ShadowOffset, i, TextBounds, Caption, Format );
//      Canvas->Draw ( XImagePos + i - ShadowOffset, YImagePos + i, MaskBmpBlack );

    OffsetTextOut ( Canvas, i, ShadowOffset, TextBounds, Caption, Format );
    OffsetTextOut ( Canvas, i, i, TextBounds, Caption, Format );
    OffsetTextOut ( Canvas, ShadowOffset, i, TextBounds, Caption, Format );
//    Canvas->Draw ( XImagePos + i, YImagePos + ShadowOffset, MaskBmpBlack );
//    Canvas->Draw ( XImagePos + i, YImagePos + i, MaskBmpBlack );
//    Canvas->Draw ( XImagePos + ShadowOffset, YImagePos + i, MaskBmpBlack );
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::DrawButtonText ( TCanvas *Canvas, const String Caption,
      TRect TextBounds, bool WordWrap, TAlignment Alignment, TBMButtonState State, int FReliefSize, TColor FHotTrackColor )
{
  const UINT AlignmentFlags [ 3 ] = { DT_LEFT, DT_RIGHT, DT_CENTER };
  UINT Format;

  Format = DT_VCENTER | AlignmentFlags[Alignment];
  if ( ! WordWrap )
    Format = Format | DT_SINGLELINE;

  else
    Format = Format | DT_WORDBREAK;

//  with Canvas do {
    Canvas->Brush->Style = bsClear;
    if ( State == Bmshapedbutton::bsDisabled )
      {
      OffsetRect ( (RECT *)&TextBounds, 1, 1);
      Canvas->Font->Color = clBtnHighlight;
      DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format );
      OffsetRect ( (RECT *)&TextBounds, -1, -1);
      Canvas->Font->Color = clBtnShadow;
      DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format);
      }
      
    else
      {
      TColor OldColor = Canvas->Font->Color;
      
      if ( State == bsMouseIn || State == bsDown || State == bsExclusive  || State == bsMouseInExclusive || State == bsMouseInDown )
        {
        if ( State == bsDown || State == bsExclusive  || State == bsMouseInExclusive || State == bsMouseInDown )
          {
          Canvas->Font->Color = clBtnHighlight;
          DrawTextShadow ( Canvas, FReliefSize, Caption, TextBounds, Format, true );

          Canvas->Font->Color = clBtnShadow;
          DrawTextShadow ( Canvas, -FReliefSize, Caption, TextBounds, Format, false );

//          Canvas->Font->Color = clBtnShadow;
//          DrawTextShadow ( Canvas, - ( FReliefSize - 1 ), Caption, TextBounds, Format, true );

//          Canvas->Font->Color = clBtnHighlight;
//          DrawTextShadow ( Canvas, FReliefSize - 1, Caption, TextBounds, Format, false );
          }

        else
          {
          Canvas->Font->Color = clBtnShadow;
          DrawTextShadow ( Canvas, FReliefSize, Caption, TextBounds, Format, true );

          Canvas->Font->Color = clBtnHighlight;
          DrawTextShadow ( Canvas, -FReliefSize, Caption, TextBounds, Format, false );

          Canvas->Font->Color = clBtnHighlight;
          DrawTextShadow ( Canvas, - ( FReliefSize - 1 ), Caption, TextBounds, Format, true );

          Canvas->Font->Color = clBtnShadow;
          DrawTextShadow ( Canvas, FReliefSize - 1, Caption, TextBounds, Format, false );
          }

/*
        Canvas->Font->Color = Color2;
        DrawTextShadow ( Canvas, FReliefSize, Caption, TextBounds, Format, true );

        Canvas->Font->Color = Color1;
        DrawTextShadow ( Canvas, -FReliefSize, Caption, TextBounds, Format, false );

        Canvas->Font->Color = Color1;
        DrawTextShadow ( Canvas, - ( FReliefSize - 1 ), Caption, TextBounds, Format, true );

        Canvas->Font->Color = Color2;
        DrawTextShadow ( Canvas, FReliefSize - 1, Caption, TextBounds, Format, false );
*/
        }

      if ( State == bsMouseIn || State == bsMouseInDown || State == bsMouseInExclusive )
        Canvas->Font->Color = FHotTrackColor;
        
      else
        Canvas->Font->Color = OldColor;

      DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format);
      
      Canvas->Font->Color = OldColor;
      }
//  }
}
//---------------------------------------------------------------------------
void __fastcall TButtonGlyph::DrawButtonDropArrow ( TCanvas *Canvas, const int X, const int Y, TBMButtonState State )
{
//  with Canvas do {
    TPoint Array [ 3 ];
    if ( State == Bmshapedbutton::bsDisabled )
      {
      Canvas->Pen->Color = clBtnHighlight;
      Canvas->Brush->Color = clBtnHighlight;
      Array [ 0 ] = Point(X+5, Y+1);
      Array [ 1 ] = Point(X+9, Y+1);
      Array [ 2 ] = Point(X+7, Y+3);

      Canvas->Polygon (Array,2);
      Canvas->Pen->Color = clBtnShadow;
      Canvas->Brush->Color = clBtnShadow;
      Array [ 0 ] = Point(X+4, Y);
      Array [ 1 ] = Point(X+8, Y);
      Array [ 2 ] = Point(X+6, Y+2);
      Canvas->Polygon (Array,2);
      }
      
    else
      {
      Canvas->Pen->Color = Canvas->Font->Color;
      Canvas->Brush->Color = Canvas->Font->Color;
      Array [ 0 ] = Point(X+4, Y);
      Array [ 1 ] = Point(X+8, Y);
      Array [ 2 ] = Point(X+6, Y+2);
      Canvas->Polygon (Array, 2);
      }
//  }
}
//---> Begin ED
//---------------------------------------------------------------------------
TPoint __fastcall TButtonGlyph::GetGlyphSize ( bool DrawGlyph )
{
  TPoint GlyphSize;

  GlyphSize.x = 0;
  GlyphSize.y = 0;
  if ( DrawGlyph )
    {
    if ( ! FImageList )
      {
      if ( FOriginal != NULL )
        {
        GlyphSize.x = FOriginal->Width / FNumGlyphs;
        GlyphSize.y = FOriginal->Height;
        }
      }
      
    else {
      GlyphSize.x = ((TImageList * )FImageList)->Width;
      GlyphSize.y = ((TImageList * )FImageList)->Height;
    }
  }

  return GlyphSize;
}
//---------------------------------------------------------------------------
TPoint __fastcall TButtonGlyph::GetArrowSize ( bool DropArrow )
{
  TPoint ArrowSize;

  if ( DropArrow )
    {
    ArrowSize.x = 9;
    ArrowSize.y = 3;
    }
    
  else {
    ArrowSize.x = 0;
    ArrowSize.y = 0;
  }

  return ArrowSize;
}
//---------------------------------------------------------------------------
TPoint __fastcall TButtonGlyph::RecommendedSize ( TCanvas *Canvas, bool DrawGlyph, bool DrawCaption,
      String const Caption, bool WordWrap, TButtonLayoutPlus Layout, int Margin, int Spacing,
      bool DropArrow, int FReliefSize, bool Border, bool Flat )
{
  TPoint result ( 2 * FReliefSize, 2 * FReliefSize );

  if ( Border )
     {
     if ( Flat )
        {
        result.x += 2;
        result.y += 2;
        }
     else
        {
        result.x += 4;
        result.y += 4;
        }
     }

  if (!DrawCaption || Caption.IsEmpty()) Layout = blGlyphLeft;

  TPoint GlyphSize = GetGlyphSize ( DrawGlyph ) , TextSize, ArrowSize = GetArrowSize ( DropArrow );
  bool LayoutLeftOrRight;

  LayoutLeftOrRight = ( Layout == blGlyphLeft || Layout == blGlyphRight );
  if ( ! LayoutLeftOrRight && ((GlyphSize.x == 0) || (GlyphSize.y == 0)) )
    {
    Layout = blGlyphLeft;
    LayoutLeftOrRight = true;
    }

  if ( DrawCaption && ( ! Caption.IsEmpty () ) )
    {
    TRect TextBounds = Rect(0, 0, Screen->DesktopWidth, 0 );
      
    UINT Format = DT_CALCRECT;
    if ( WordWrap )
      {
      Format = Format | DT_WORDBREAK;
      }

    DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format);
    TextSize = Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
    }

  else
    {
    TextSize = Point(0,0);
  }

  // if there is no text or no bitmap, then Spacing is irrelevant
  if ((TextSize.x == 0) || (TextSize.y == 0) ||
     (GlyphSize.x == 0) || (GlyphSize.y == 0) )
    Spacing = 0;

  switch ( Layout )
    {
    case blGlyphRight:
    case blGlyphLeft:
      result.x += GlyphSize.x + TextSize.x;
      result.y += max(TextSize.y, GlyphSize.y);
      break;

    case blGlyphTop:
    case blGlyphBottom:
      result.x += max(TextSize.x, GlyphSize.x);
      result.y += GlyphSize.y + TextSize.y;
      break;

    default:
      break;
    }

  // adjust Margin and Spacing
  if ( Margin < 0 )
    {

    if ( Layout == blGlyphCentre ) Margin = 0;
    else {

    if ( Spacing < 0 )
      {
      TPoint TotalSize = Point(GlyphSize.x + TextSize.x + ArrowSize.x,
        GlyphSize.y + TextSize.y);
      if ( LayoutLeftOrRight )
        Margin = result.x - TotalSize.x;
      else
        Margin = result.y - TotalSize.y;

      Spacing = Margin;
      }

    else
      {
      TPoint TotalSize = Point(GlyphSize.x + Spacing + TextSize.x + ArrowSize.x,
        GlyphSize.y + Spacing + TextSize.y);
      if ( LayoutLeftOrRight )
        Margin = result.x + Spacing - TotalSize.x + 1 - 2 * FReliefSize;

      else
        Margin = result.y + Spacing - TotalSize.y + 1 - 2 * FReliefSize;
      }
    }
    }

  else
    {
    if ( Spacing < 0 )
       {
       if (Layout != blGlyphCentre)
          {
          TPoint TotalSize = Point(result.x - (GlyphSize.x + ArrowSize.x),
             result.y - GlyphSize.y);

          if ( LayoutLeftOrRight )
             Spacing = TotalSize.x - TextSize.x;

          else
             Spacing = TotalSize.y - TextSize.y;
       }
    }
  }


  if ((Layout == blGlyphTop) || (Layout == blGlyphBottom))
     result.y += 2 * Margin + Spacing;
  else
      {
      if (Layout == blGlyphCentre)
         {
         int aux_a = (TextSize.x-GlyphSize.x)/2;
         int aux_b = aux_a+GlyphSize.x;
         result.x += 2 * Margin +
         ( ( 2*abs(Spacing)+TextSize.x > GlyphSize.x ) ?
         ( TextSize.x +
         ( (abs(Spacing) > abs(aux_a)) ? (GlyphSize.x+abs(Spacing)-aux_b):0 ) ):
         GlyphSize.x );
         result.y += 2 * Margin + max( TextSize.y, GlyphSize.y );
         }
      else result.x += 2 * Margin + Spacing;
      }

  return result;
}
//---------------------------------------------------------------------------
//void __fastcall TButtonGlyph::CalcButtonLayout ( TCanvas *Canvas, const TRect Client,
//      const TPoint Offset, bool DrawGlyph, bool DrawCaption, const String Caption,
//      bool WordWrap, TButtonLayout Layout, int Margin, int Spacing, bool DropArrow,
//      TPoint &GlyphPos, TPoint &ArrowPos, TRect &TextBounds, int FReliefSize )
void __fastcall TButtonGlyph::CalcButtonLayout ( TCanvas *Canvas, const TRect Client,
      const TPoint Offset, bool DrawGlyph, bool DrawCaption, const String Caption,
      bool WordWrap, TButtonLayoutPlus Layout, int Margin, int Spacing, bool DropArrow,
      TPoint &GlyphPos, TPoint &ArrowPos, TRect &TextBounds, int FReliefSize )
// End ED
{
  TPoint TextPos;
//---> Begin ED
  //TPoint ClientSize, GlyphSize, TextSize, ArrowSize;
  TPoint ClientSize, GlyphSize = GetGlyphSize ( DrawGlyph ) , TextSize, ArrowSize = GetArrowSize ( DropArrow );
// End ED
  TPoint TotalSize;
  UINT Format;
  int Margin1, Spacing1;
  bool LayoutLeftOrRight;

  // calculate the item sizes
  ClientSize = Point(Client.Right-Client.Left, Client.Bottom-Client.Top);

//---> Begin ED

  if (!DrawCaption || Caption.IsEmpty()) Layout = blGlyphLeft;

/*
// End ED
  GlyphSize.x = 0;
  GlyphSize.y = 0;
  if ( DrawGlyph )
    {
    if ( ! FImageList )
      {
      if ( FOriginal != NULL )
        {
        GlyphSize.x = FOriginal->Width / FNumGlyphs;
        GlyphSize.y = FOriginal->Height;
        }
      }
      
    else {
      GlyphSize.x = ((TImageList * )FImageList)->Width;
      GlyphSize.y = ((TImageList * )FImageList)->Height;
    }
  }

  if ( DropArrow )
    {
    ArrowSize.x = 9;
    ArrowSize.y = 3;
    }
    
  else {
    ArrowSize.x = 0;
    ArrowSize.y = 0;
  }
//---> Begin ED
*/
// End ED

  LayoutLeftOrRight = ( Layout == blGlyphLeft || Layout == blGlyphRight );
  if ( ! LayoutLeftOrRight && ((GlyphSize.x == 0) || (GlyphSize.y == 0)) )
    {
    Layout = blGlyphLeft;
    LayoutLeftOrRight = true;
    }

  if ( DrawCaption && ( ! Caption.IsEmpty () ) )
    {
    TextBounds = Rect(0, 0, Client.Right-Client.Left, 0 );
    if ( LayoutLeftOrRight )
      TextBounds.Right -= ArrowSize.x;
      
    Format = DT_CALCRECT;
    if ( WordWrap )
      {
      Format = Format | DT_WORDBREAK;
      Margin1 = 4;
      if ( LayoutLeftOrRight && (GlyphSize.x != 0) && (GlyphSize.y != 0) )
        {
        if ( Spacing < 0 )
          Spacing1 = 4 + FReliefSize * 2;

        else
          Spacing1 = Spacing + FReliefSize * 2;
          
        TextBounds.Right -= GlyphSize.x + Spacing1;
        if ( Margin >= 0 )
          Margin1 = Margin;
          
        else
          if ( Spacing >= 0 )
            Margin1 = Spacing;
        }

      TextBounds.Right -= Margin1 * 2;
      }
      
    DrawText(Canvas->Handle, PChar(Caption.c_str ()), Caption.Length(), (RECT *)&TextBounds, Format);
    TextSize = Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
    }

  else
    {
    TextBounds = Rect(0, 0, 0, 0);
    TextSize = Point(0,0);
  }

  //---> Begin ED
  if ( Layout != blGlyphCentre )
  {
  // End ED

  /* If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.*/
  if ( LayoutLeftOrRight )
    {
    GlyphPos.y = ((ClientSize.y - GlyphSize.y + 1) / 2 ) - FReliefSize;
    TextPos.y = ((ClientSize.y - TextSize.y + 1) / 2 );
    }

  else
    {
    GlyphPos.x = (ClientSize.x - GlyphSize.x - ArrowSize.x + 1) / 2 - FReliefSize;
    TextPos.x = (ClientSize.x - TextSize.x + 1) / 2;
    if ((GlyphSize.x == 0) || (GlyphSize.y == 0) )
      ArrowPos.x = TextPos.x + TextSize.x;

    else
      ArrowPos.x = GlyphPos.x + GlyphSize.x;
    }
    
  //---> Begin ED
  }
  // End ED

  // if there is no text or no bitmap, then Spacing is irrelevant
  if ((TextSize.x == 0) || (TextSize.y == 0) ||
     (GlyphSize.x == 0) || (GlyphSize.y == 0) )
    Spacing = 0;

  // adjust Margin and Spacing
  if ( Margin < 0 )
    {

    //---> Begin ED
    if ( Layout == blGlyphCentre ) Margin = 0;
    else {
    // End ED

    if ( Spacing < 0 )
      {
      TotalSize = Point(GlyphSize.x + TextSize.x + ArrowSize.x,
        GlyphSize.y + TextSize.y);
      if ( LayoutLeftOrRight )
        Margin = (ClientSize.x - TotalSize.x) / 3;
      else
        Margin = (ClientSize.y - TotalSize.y) / 3;

      Spacing = Margin;
      }

    else
      {
      TotalSize = Point(GlyphSize.x + Spacing + TextSize.x + ArrowSize.x,
        GlyphSize.y + Spacing + TextSize.y);
      if ( LayoutLeftOrRight )
        Margin = (ClientSize.x - TotalSize.x + 1) / 2 - FReliefSize;

      else
        Margin = (ClientSize.y - TotalSize.y + 1) / 2 - FReliefSize;
      }
    //---> Begin ED
    }
    // End ED
    }

  else
    {
    if ( Spacing < 0 )
       {
       //---> Begin ED
       if (Layout != blGlyphCentre)
          {
       // End ED
          TotalSize = Point(ClientSize.x - (Margin + GlyphSize.x + ArrowSize.x),
             ClientSize.y - (Margin + GlyphSize.y));

          if ( LayoutLeftOrRight )
             Spacing = (TotalSize.x - TextSize.x) / 2;

          else
             Spacing = (TotalSize.y - TextSize.y) / 2;
       //---> Begin ED
       }
       // End ED
    }
  }

  switch ( Layout )
    {
//---> Begin ED
/*
    case blGlyphLeft:
      {
      GlyphPos.x = Margin;
      TextPos.x = GlyphPos.x + GlyphSize.x + Spacing + 2 * FReliefSize;
      ArrowPos.x = TextPos.x + TextSize.x;
      break;
      }

    case blGlyphRight:
      {
      ArrowPos.x = ClientSize.x - Margin - ArrowSize.x;
      GlyphPos.x = ArrowPos.x - GlyphSize.x - FReliefSize;
      TextPos.x = GlyphPos.x - Spacing - TextSize.x; // - FReliefSize;
      break;
      }

    case blGlyphTop:
      {
      GlyphPos.y = Margin;
      TextPos.y = GlyphPos.y + GlyphSize.y + Spacing;
      break;
      }

    case blGlyphBottom:
      {
      GlyphPos.y = ClientSize.y - Margin - GlyphSize.y - FReliefSize;
      TextPos.y = GlyphPos.y - Spacing - TextSize.y;
      break;
      }
*/

    case blGlyphLeft:
      {
      GlyphPos.x = Margin;
      TextPos.x = GlyphPos.x + GlyphSize.x + Spacing + FReliefSize;
      ArrowPos.x = TextPos.x + TextSize.x;
      break;
      }

    case blGlyphRight:
      {
      ArrowPos.x = ClientSize.x - Margin - ArrowSize.x;
      GlyphPos.x = ArrowPos.x - GlyphSize.x - 2*FReliefSize;
      TextPos.x = GlyphPos.x - Spacing - TextSize.x + FReliefSize;
      break;
      }

    case blGlyphTop:
      {
      GlyphPos.y = Margin;
      TextPos.y = GlyphPos.y + GlyphSize.y + Spacing + FReliefSize;
      break;
      }

    case blGlyphBottom:
      {
      GlyphPos.y = ClientSize.y - Margin - GlyphSize.y - 2*FReliefSize;
      TextPos.y = GlyphPos.y - Spacing - TextSize.y;
      break;
      }

    case blGlyphCentre:
    	{
        int aux_a = (TextSize.x-GlyphSize.x)/2;
        int aux_b = aux_a+GlyphSize.x;
        if ( 2*abs(Spacing)+TextSize.x > GlyphSize.x )
           {
           int dec = (ClientSize.x - ( TextSize.x +
           ( (abs(Spacing) > abs(aux_a)) ? (GlyphSize.x+abs(Spacing)-aux_b):0 ) ) ) / 2;
           TextPos.x = dec - FReliefSize;
           if (Spacing > abs(aux_a))
              {
              if (Spacing <= aux_b) TextPos.x += GlyphSize.x + Spacing - aux_b;
              else TextPos.x += Spacing - aux_a;
              GlyphPos.x = dec - FReliefSize;
              }
           else GlyphPos.x = TextPos.x + aux_a - Spacing;
           }
        else
            {
            GlyphPos.x = (ClientSize.x - GlyphSize.x) / 2 - FReliefSize;
            TextPos.x  = GlyphPos.x + abs(aux_a) + Spacing;
            }
        if ( TextSize.y > GlyphSize.y )
           {
           TextPos.y  = (ClientSize.y - TextSize.y) / 2 - FReliefSize;
           GlyphPos.y = TextPos.y + (TextSize.y-GlyphSize.y)/2;
           }
        else
            {
            GlyphPos.y = (ClientSize.y - GlyphSize.y) / 2 - FReliefSize;
            TextPos.y  = GlyphPos.y - (TextSize.y-GlyphSize.y)/2;
            }
        ArrowPos.x = max ( GlyphPos.x + GlyphSize.x, TextPos.x + TextSize.x );
        break;
        }
// End ED

    }

  if ((GlyphSize.x == 0) || (GlyphSize.y == 0) )
    ArrowPos.y = TextPos.y + (TextSize.y - ArrowSize.y) / 2;

  else
    ArrowPos.y = GlyphPos.y + (GlyphSize.y - ArrowSize.y) / 2;

// fixup the result variables
//  with GlyphPos do {
    GlyphPos.x += Client.Left + Offset.x;
    GlyphPos.y += Client.Top + Offset.y;
//  }
//  with ArrowPos do {
    ArrowPos.x += Client.Left + Offset.x;
    ArrowPos.y += Client.Top + Offset.y;
//  }
  OffsetRect ( (RECT *) &TextBounds, TextPos.x + Client.Left + Offset.x,
    TextPos.y + Client.Top + Offset.x);
}
//---------------------------------------------------------------------------
//---> Begin ED
//TRect __fastcall TButtonGlyph::Draw ( TCanvas *Canvas, const TRect Client, const TPoint Offset,
//      bool DrawGlyph, bool DrawCaption, String const Caption, bool WordWrap,
//      TAlignment Alignment, TButtonLayout Layout, int Margin, int Spacing,
//      bool DropArrow, TBMButtonState State, int FReliefSize, TColor FHotTrackColor, bool InDesign )
TRect __fastcall TButtonGlyph::Draw ( TCanvas *Canvas, const TRect Client, const TPoint Offset,
      bool DrawGlyph, bool DrawCaption, String const Caption, bool WordWrap,
      TAlignment Alignment, TButtonLayoutPlus Layout, int Margin, int Spacing,
      bool DropArrow, TBMButtonState State, int FReliefSize, TColor FHotTrackColor, bool InDesign )
// End ED
{
  TPoint GlyphPos, ArrowPos;
  TRect Result;

  CalcButtonLayout ( Canvas, Client, Offset, DrawGlyph, DrawCaption, Caption,
    WordWrap, Layout, Margin, Spacing, DropArrow, GlyphPos, ArrowPos, Result, FReliefSize );

  if ( DrawGlyph )
    DrawButtonGlyph ( Canvas, GlyphPos, State, FReliefSize, InDesign );

  if ( DrawCaption )
    DrawButtonText ( Canvas, Caption, Result, WordWrap, Alignment, State, FReliefSize, FHotTrackColor );

  if ( DropArrow )
    DrawButtonDropArrow ( Canvas, ArrowPos.x, ArrowPos.y, State);

  return Result;
}


//---------------------------------------------------------------------------
//{$IFNDEF TB97D4}
class TDropdownList : public TComponent
{
  typedef TComponent inherited;

  friend class TBMShapedButton;

private :
    TList *List;
    HWND Window;

    void __fastcall WndProc ( TMessage &Message );

protected :
    virtual void __fastcall Notification ( TComponent *AComponent, TOperation Operation );

public:
    __fastcall TDropdownList ( TComponent *AOwner );
    __fastcall ~TDropdownList ();
    void __fastcall AddMenu ( TPopupMenu *Menu );

};
//---------------------------------------------------------------------------
static TDropdownList *DropdownList;

class TLocalInit
{
public :
  TLocalInit ()
    {
    #ifdef _BMSHAPED_TRACE
    ofstream of ( "c:\\Trece.txt", ios::trunc );
    #endif
    }

  ~TLocalInit ()
    {
    if ( DropdownList )
      delete DropdownList;
    }
};

TLocalInit __LocalInet;

//---------------------------------------------------------------------------
__fastcall TDropdownList::TDropdownList ( TComponent *AOwner ) : TComponent ( AOwner )
{
  List = new TList;
}
//---------------------------------------------------------------------------
__fastcall TDropdownList::~TDropdownList ()
{
  delete List;
}
//---------------------------------------------------------------------------
void __fastcall TDropdownList::WndProc ( TMessage &Message )
//{ This void __fastcall is based on code from TPopupList.WndProc (menus.pas) }
{
  int I;
  TMenuItem *MenuItem;
  TFindItemKind FindKind;
  int ContextID;
  
  try
    {
//    with List do
      switch ( Message.Msg )
        {
        case WM_COMMAND:
          for ( I = 0; I < List->Count; I ++ )
            if ( (( TPopupMenu *) List->Items[I])->DispatchCommand( (( TWMCommand *) &Message)->ItemID))
              return;

          break;

        case WM_INITMENUPOPUP:
          for ( I = 0; I < List->Count; I ++ )
            if ( (( TPopupMenu *) List->Items[I])->DispatchPopup( (( TWMInitMenuPopup *)&Message)->MenuPopup))
              return;

          break;

        case WM_MENUSELECT:
//          with TWMMenuSelect(Message) do {
            {
            FindKind = fkCommand;
            if ( ((TWMMenuSelect *)&Message)->MenuFlag && MF_POPUP != 0 )
              FindKind = fkHandle;

            for ( I = 0; I < List->Count; I ++ )
              {
              MenuItem = (( TPopupMenu *) List->Items[I])->FindItem(((TWMMenuSelect *)&Message)->IDItem, FindKind);
              if ( MenuItem )
                {
                Application->Hint = MenuItem->Hint;
                return;
                }
              }

            Application->Hint = "";

            break;
            }

        case WM_HELP:
//          with TWMHelp(Message).HelpInfo^ do {
            {
            for ( I = 0; I < List->Count; I ++ )
              if ( (( TPopupMenu *) List->Items[I])->Handle == ((TWMHelp *)&Message)->HelpInfo->hItemHandle )
                {
                ContextID = (( TMenu *) List->Items[I])->GetHelpContext( ((TWMHelp *)&Message)->HelpInfo->iCtrlId, true);
                if ( ContextID == 0 )
                  ContextID = (( TMenu *) List->Items[I])->GetHelpContext( (int)((TWMHelp *)&Message)->HelpInfo->hItemHandle, false );

                if ( Screen->ActiveForm == NULL )
                  return;
                  
                if ( Screen->ActiveForm->BorderIcons.Contains ( biHelp ) )
                  Application->HelpCommand (HELP_CONTEXTPOPUP, ContextID);

                else
                  Application->HelpContext (ContextID);

                return;
                }
            }
        }

//    with Message do
      DefWindowProc(Window, Message.Msg, Message.WParam, Message.LParam);
    }
    
  catch (...)
    {
    Application->HandleException (this);
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TDropdownList::AddMenu ( TPopupMenu * Menu )
{
  if ( List->IndexOf(Menu) == -1 )
    {
    if ( List->Count == 0 )
      Window = AllocateHWnd(WndProc);
      
    Menu->FreeNotification (this);
    List->Add (Menu);
    }
}
//---------------------------------------------------------------------------
void __fastcall TDropdownList::Notification ( TComponent *AComponent, TOperation Operation )
{
  inherited::Notification ( AComponent, Operation );
  if ( Operation == opRemove )
    {
    List->Remove (AComponent);
    if ( List->Count == 0 )
      DeallocateHWnd (Window);
    }

}
//---------------------------------------------------------------------------
//{$ENDIF}
//---> Begin ED
COMMENCE_DEFINITION_CARTE_MESSAGES(TBMShapedButton)
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
FINI_DEFINITION_CARTE_MESSAGES(inherited)
// End ED
//---------------------------------------------------------------------------
__fastcall TBMShapedButton::TBMShapedButton(TComponent* Owner)
//---> Begin ED
//    : inherited (Owner)
    : inherited (Owner), FOnMouseEnter(NULL), FOnMouseExit(NULL)
// End ED
{
  FUnFlashedDelay = 300;
  FFlashingDelay = 6;
  FFlashedDelay = 6;

//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
  FEnableSounds = true;
//---> Begin ED
#endif
// End ED

  AlreadyPlayngEntryFlag = false;
  FCloudColor = clYellow;
  
  FCloudArray = NULL;
  FCloudArray1 = NULL;

//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED

//  FHighlightWave = new TBMWaveData;
//  FPulseWave = new TBMWaveData;
//  FSelectWave = new TBMWaveData;

  FHighlightSound = NULL;
  FPulseSound     = NULL;
  FSelectSound    = NULL;

//---> Begin ED
#endif
// End ED

  FCloudType = ctSmall;

  FEnableFlash = false;
  FCloudSize = 0;

  FFlashing = true;

  FInClick = false;

  FHotTrackColor = clHighlight;
  FReliefSize = 2;
  if ( ! ButtonMouseTimer )
    {
    ButtonMouseTimer = new TTimer ( NULL );
    ButtonMouseTimer->Enabled = false;
    ButtonMouseTimer->Interval = 125;  // 8 times a second
    }

  SetBounds (Left, Top, 23, 22);

  ControlStyle = ControlStyle << csCaptureMouse;
  ControlStyle = ControlStyle << csDoubleClicks;
  ControlStyle = ControlStyle << csOpaque;
  ControlStyle = ControlStyle >> csSetCaption;

  Color = clBtnFace;
  FGlyph = new TButtonGlyph;
  ((TButtonGlyph *) FGlyph )->OnChange = GlyphChanged;
  ParentFont = true;
  FAlignment = taCenter;
  FFlat = true;
  FTransparent = true;
  FSpacing = 4;
  FMargin  = -1;
  FLayout  = blGlyphLeft;
  FDropdownArrow  = true;
  FRepeatDelay    = 400;
  FRepeatInterval = 100;
  ButtonCount ++;

  Section->Enter ();
  BmBtnsList->Add ( this );
  Section->Leave ();
}

__fastcall TBMShapedButton::~TBMShapedButton()
{
  Section->Enter ();
  BmBtnsList->Remove ( this );
  Section->Leave ();
  
  RemoveButtonMouseTimer ();
  delete FGlyph;
  /* The Notification method, which is sometimes called while the component is
    being destroyed, reads FGlyph and expects it to be valid, so it must be
    reset to NULL */
  FGlyph = NULL;

  if ( ! ( -- ButtonCount ))
    {
    delete Pattern;
    Pattern = NULL;
    delete ButtonMouseTimer;
    ButtonMouseTimer = NULL;
    }

/*
  delete FSelectWave;
  delete FPulseWave;
  delete FHighlightWave;
*/
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TParentControl : public TWinControl
{
  friend void __fastcall CopyParentImage( TControl *Control, TCanvas *Dest );
  
  public :
  	virtual void __fastcall PaintWindow(HDC DC) { TWinControl::PaintWindow (DC); }

};
//---------------------------------------------------------------------------
void __fastcall CopyParentImage( TControl *Control, TCanvas *Dest )
{
  int I, Count, X, Y, SaveIndex;
  HDC DC;
  RECT R, SelfR, CtlR;

  if ( Control->Parent == NULL )
    return;

  Count = Control->Parent->ControlCount;
  DC = Dest->Handle;
  SelfR = Bounds( Control->Left, Control->Top, Control->Width, Control->Height);
  X = -Control->Left;
  Y = -Control->Top;
//  Copy parent control image 

  SaveIndex = SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, NULL);


  RECT UpdRect;

  IntersectClipRect(DC, 0, 0, Control->Parent->ClientWidth,
  Control->Parent->ClientHeight);

  GetUpdateRect( (( TParentControl *)Control->Parent)->Handle, &UpdRect, FALSE );

  (( TParentControl *)Control->Parent)->Perform ( WM_ERASEBKGND, (int)DC, 0 );

  (( TParentControl *)Control->Parent)->PaintWindow(DC);
  RestoreDC(DC, SaveIndex);

  InvalidateRect ( (( TParentControl *)Control->Parent)->Handle, &UpdRect, TRUE );

//  Copy images of graphic controls
  for ( I = 0; I < Count; I ++ )
    {
    if ((Control->Parent->Controls[I] != NULL) &&
      ! ( dynamic_cast<TWinControl* > ( Control->Parent->Controls[I] ) ) )
      {
      if ( Control->Parent->Controls[I] == Control )
        break;

      CtlR = Bounds(Control->Parent->Controls[I]->Left, Control->Parent->Controls[I]->Top, Control->Parent->Controls[I]->Width, Control->Parent->Controls[I]->Height);
      if ( IntersectRect(&R, &SelfR, &CtlR) && Control->Parent->Controls[I]->Visible )
        {
        SaveIndex = SaveDC(DC);
        SetViewportOrgEx(DC, Control->Parent->Controls[I]->Left + X, Control->Parent->Controls[I]->Top + Y, NULL);
        IntersectClipRect(DC, 0, 0, Control->Parent->Controls[I]->Width, Control->Parent->Controls[I]->Height);
        Control->Parent->Controls[I]->Perform( WM_PAINT, (int)DC, 0);
        RestoreDC(DC, SaveIndex);
        }
      }
    }
}
//---------------------------------------------------------------------------
//---> Begin ED
TPoint __fastcall TBMShapedButton::RecommendedSize()
{
  Canvas->Font = Font;
  return (( TButtonGlyph *)FGlyph)->RecommendedSize(Canvas,
  FDisplayMode != dmTextOnly, FDisplayMode != dmGlyphOnly,
  Caption, FWordWrap, FLayout, FMargin, FSpacing,
  FDropdownArrow && ! FDropdownCombo && FUsesDropdown, FReliefSize, FBorder, FFlat);
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::AdjustBounds(void)
{
  if (!ComponentState.Contains(csReading) && AutoSize)
     {
     TPoint p  = RecommendedSize();
     SetBounds(Left, Top, p.x, p.y);
     }
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButton::CanAutoSize(int &NewWidth, int &NewHeight)
{
  TPoint p  = RecommendedSize();
  NewWidth  = p.x;
  NewHeight = p.y;
  return ((NewWidth != Width) || (NewHeight != Height));
}
// End ED
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::Paint(void)
/*
const
  EdgeStyles: array[Boolean, Boolean] of UINT = (
    (EDGE_RAISED, EDGE_SUNKEN),
    (BDR_RAISEDINNER, BDR_SUNKENOUTER));
  FlagStyles: array[Boolean] of UINT = (BF_RECT or BF_SOFT or BF_MIDDLE, BF_RECT);
*/
{

  const UINT EdgeStyles [ 2 ] [ 2 ] =
    {
    {EDGE_RAISED, EDGE_SUNKEN},
    {BDR_RAISEDINNER, BDR_SUNKENOUTER}
    };

  const UINT FlagStyles [ 2 ] =
    {
    BF_RECT | BF_SOFT | BF_MIDDLE,
    BF_RECT
    };

  Graphics::TBitmap *Bmp;
  TCanvas *DrawCanvas;
  TRect PaintRect, R;
  TPoint Offset;
  bool StateDownOrExclusive;

  Bmp = new Graphics::TBitmap;


/*
  if ( FOpaque || ! FFlat )
    Bmp = new Graphics::TBitmap;

  else
    Bmp = NULL;
*/

  try
    {
//    if (FOpaque || ! FFlat )
      {
      Bmp->Width = Width;
      Bmp->Height = Height;
      DrawCanvas = Bmp->Canvas;
      if ( FTransparent && FFlat )
        {
        DrawCanvas->Brush->Color = (( TForm * )Parent )->Color;
        DrawCanvas->FillRect (ClientRect);
        CopyParentImage(this, DrawCanvas );
        }

      else
        {
        DrawCanvas->Brush->Color = Color;
        DrawCanvas->FillRect (ClientRect);
        }
      }

//    else
//      DrawCanvas = Canvas;
//    DrawCanvas->Font = this.Font;
    DrawCanvas->Font = Font;
    PaintRect = Rect(0, 0, Width, Height);
    StateDownOrExclusive = ( FState == bsDown || FState == bsExclusive  || FState == bsMouseInExclusive || FState == bsMouseInDown );

    if ((( FBorder) &&
        ( ! FFlat || StateDownOrExclusive ||
        (FMouseInControl && (FState != Bmshapedbutton::bsDisabled)))) ||
       ( ComponentState.Contains ( csDesigning ) ) )
      {
      if (DropdownCombo && FUsesDropdown )
        {
        R = PaintRect;
        R.Left = R.Right - DropdownComboWidth;
        R.Right -= 2;
        DrawEdge (DrawCanvas->Handle, (RECT *)&R,
          EdgeStyles[FFlat][StateDownOrExclusive && FMenuIsDown],
          FlagStyles [ FFlat ] );
        PaintRect.Right -= DropdownComboWidth;
        }

      DrawEdge (DrawCanvas->Handle, (RECT *)&PaintRect,
        EdgeStyles[FFlat] [StateDownOrExclusive && ( ! (DropdownCombo && FUsesDropdown) || !FMenuIsDown)],
        FlagStyles[FFlat]);
      }
      
    else
      if (DropdownCombo && FUsesDropdown )
        PaintRect.Right -= DropdownComboWidth;
        
    if ( FBorder )
      {
      if (FFlat )
        InflateRect ( (RECT *)&PaintRect, -1, -1);

      else
        InflateRect ( (RECT *)&PaintRect, -2, -2);
      }

    if (StateDownOrExclusive && ( ! (DropdownCombo && FUsesDropdown) || ! FMenuIsDown) )
      {
/*
      if ((FState == bsExclusive || FState == bsMouseInExclusive ) && ( ! FFlat || ! FMouseInControl) )
        {
        //if ( ! Pattern )
        //  CreateBrushPattern ();

        //DrawCanvas->Brush->Bitmap = Pattern;
        //DrawCanvas->FillRect(PaintRect);
        }
*/

      Offset.x = 1;
      Offset.y = 1;
      }
    else
      {
      Offset.x = 0;
      Offset.y = 0;
      }

//    if ( FCloud ) ??????
      if ( FCloudType != ctNone && ( FState == bsMouseIn || FState == bsMouseInDown || FState == bsMouseInExclusive ))
        if ( FTransparent && FFlat )
          HiglightRectangle ( Bmp, PaintRect, FCloudColor, ( FFlashing ) ? FCloudSize : 1, FCloudType == ctLarge, FCloudArray, FCloudArray1 );

        else
          HiglightRectangle1 ( Bmp, PaintRect, Color, FCloudColor, ( FFlashing ) ? FCloudSize : 1, FCloudType == ctLarge );

    (( TButtonGlyph *)FGlyph)->Draw ( DrawCanvas, PaintRect, Offset,
      FDisplayMode != dmTextOnly, FDisplayMode != dmGlyphOnly,
      Caption, FWordWrap, FAlignment, FLayout, FMargin, FSpacing,
      FDropdownArrow && ! FDropdownCombo && FUsesDropdown, FState, FReliefSize, HotTrackColor, ComponentState.Contains ( csDesigning ) );
    if ( FDropdownCombo && FUsesDropdown )
      (( TButtonGlyph *)FGlyph)->DrawButtonDropArrow (DrawCanvas, Width - DropdownComboWidth-2,
        Height / 2 - 1, FState);

//    if (FOpaque || ! FFlat )
    Canvas->Draw (0, 0, Bmp );
    }
    
  __finally
    {
//    if (FOpaque || ! FFlat )
    delete Bmp;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::EraseBkg( TMessage& /*Msg*/ )
{
//  if ( ComponentState.Contains( csDesigning ))
//    TCustomPanel::Dispatch( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::RemoveButtonMouseTimer (void)
{
  if ( ButtonMouseInControl == this )
    {
    ButtonMouseTimer->Enabled = false;
    ButtonMouseInControl = NULL;
    }
}

void __fastcall TBMShapedButton::UpdateTracking ()
{
  TPoint P;

  if (Enabled )
    {
    GetCursorPos (&P);
    // Use FindDragTarget instead of PtInRect since we want to check based on
    //  the Z order
    FMouseInControl = ! ( FindDragTarget(P, true) == this );
    if (FMouseInControl )
      MouseLeft ();

    else
      MouseEntered ();
  }
/*
  if ( FMouseInControl )
    MouseLeft ();

  else
    MouseEntered ();
*/
}

void __fastcall TBMShapedButton::Loaded ()
{
  TBMButtonState State;

  inherited::Loaded ();
  
  if (Enabled )
    State = bsUp;
    
  else
    State = Bmshapedbutton::bsDisabled;
    
  (( TButtonGlyph *)FGlyph)->CreateButtonGlyph (State, FReliefSize, ComponentState.Contains ( csDesigning ));
}

void __fastcall TBMShapedButton::Notification(Classes::TComponent* AComponent, Classes::TOperation Operation )
{
  inherited::Notification( AComponent, Operation );
  if (Operation == opRemove )
    {
    if (AComponent == DropdownMenu )
      DropdownMenu = NULL;

    if ( FGlyph && (AComponent == Images) )
      Images = NULL;

//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED

    if ( AComponent == FHighlightSound )
      SetHighlightSound ( NULL );

    if ( AComponent == FPulseSound )
      SetPulseSound ( NULL );

    if ( AComponent == FSelectSound )
      SetSelectSound ( NULL );

//---> Begin ED
#endif
// End ED

    }

}

bool __fastcall TBMShapedButton::PointInButton(int X, int Y)
{
  return (X >= 0) && (X < ClientWidth-(DropdownComboWidth * (FDropdownCombo && FUsesDropdown))) &&
    (Y >= 0) && (Y < ClientHeight);
}

void __fastcall TBMShapedButton::MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y )
{
  if ( ! Enabled )
    {
    inherited::MouseDown( Button, Shift, X, Y );
    return;
    }

  if (Button != mbLeft )
    {
    MouseEntered ();
    inherited::MouseDown( Button, Shift, X, Y );
    }

  else
    {
    AlreadyPlayngEntryFlag = true;

//---> Begin ED
#ifdef _USE_BMWAVE_
    PlaySelectSound ();
#endif
// End ED

    // We know mouse has to be over the control if (the mouse went down.
    MouseEntered ();
    FMenuIsDown = FUsesDropdown && ( ! FDropdownCombo || (X >= Width-DropdownComboWidth));
    try
      {
      if ( ! FDown )
        {
        if ( FMouseInControl )
          FState = bsMouseInDown;

        else
          FState = bsDown;

        Invalidate ();
        }

      else
        if (FAllowAllUp )
          Invalidate ();
  
      if ( ! FMenuIsDown )
        FMouseIsDown = true;

      inherited::MouseDown( Button, Shift, X, Y );
      
      if (FMenuIsDown )
        Click ();

      else
        if (FRepeating )
          {
          Click ();
          if ( ! FRepeatTimer )
            FRepeatTimer = new TTimer ( NULL );
            
          FRepeatTimer->Enabled = false;
          FRepeatTimer->Interval = FRepeatDelay;
          FRepeatTimer->OnTimer = RepeatTimerHandler;
          FRepeatTimer->Enabled = true;
          }
          
      }
    __finally
      {
      FMenuIsDown = false;
      }
    }
}

/*
void __fastcall TBMShapedButton::MouseMove(Classes::TShiftState Shift, int X, int Y)
{
  TPoint P;
  TBMButtonState NewState;
  bool PtInButton;

  inherited::MouseMove( Shift, X, Y );

  / * Check if (mouse just entered the control. It works better to check this
    in MouseMove rather than using CM_MOUSEENTER, since the VCL doesn't send
    a CM_MOUSEENTER in all cases
    Use FindDragTarget instead of PtInRect since we want to check based on
    the Z order * /

  P = ClientToScreen(::Point(X, Y));
  if ((ButtonMouseInControl != this) && (FindDragTarget(P, true) == this) )
    {
    if ( ButtonMouseInControl )
      ButtonMouseInControl->MouseLeft ();
      // Like Office 97, only draw the active borders when the application is active
    if (FShowBorderWhenInactive or ApplicationIsActive ) {
      ButtonMouseInControl = this;
      ButtonMouseTimer.OnTimer = ButtonMouseTimerHandler;
      ButtonMouseTimer.Enabled = true;
      MouseEntered;
    }
  }

  if (FMouseIsDown ) {
    PtInButton = PointInButton(X, Y);
    if (PtInButton and Assigned(FRepeatTimer) )
      FRepeatTimer->Enabled = true;
    if (FDown )
      NewState = bsExclusive
    else {
      if (PtInButton )
        NewState = bsDown
      else
        NewState = bsUp;
    }
    if (NewState != FState ) {
      FState = NewState;
      Redraw (true);
    }
  }
}
*/

void __fastcall TBMShapedButton::RepeatTimerHandler(System::TObject* /*Sender*/)
{
  TPoint P;
  
  FRepeatTimer->Interval = FRepeatInterval;
  
  GetCursorPos ( &P );
  
  P = ScreenToClient(P);
  
  if (Repeating && FMouseIsDown && MouseCapture && PointInButton( P.x, P.y ) )
    Click ();
    
  else
    FRepeatTimer->Enabled = false;
}

void __fastcall TBMShapedButton::WMCancelMode(Messages::TWMNoParams &Message)
{
  delete FRepeatTimer;
  FRepeatTimer = NULL;
  if (FMouseIsDown )
    {
    FMouseIsDown = false;
    MouseLeft ();
    }
    
  /* Delphi's default processing of WM_CANCELMODE sends a "fake" WM_LBUTTONUP
    message to the control, so inherited must only be called after setting
    FMouseIsDown to false */
    
  inherited::Dispatch ( &Message );
}

void __fastcall TBMShapedButton::MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y )
{
  delete FRepeatTimer;
  FRepeatTimer = NULL;
  //{ Remove active border when right button is clicked }
  if ((Button == mbRight) && Enabled )
    {
    FMouseIsDown = false;
    MouseLeft ();
    }
    
  inherited::MouseUp( Button, Shift, X, Y );
  if ((Button == mbLeft) && FMouseIsDown )
    {
    FMouseIsDown = false;
    if (PointInButton(X, Y ) && ! FRepeating )
      ClickUp ( true );

    else
      MouseLeft ();
    }

}

void __fastcall TBMShapedButton::ClickUp ( bool IsUpClick )
{
//{$IFNDEF TB97D4}
  // TPM_RIGHTBUTTON works better on Windows 3.x
const UINT ButtonFlags [2] =
    {
    TPM_RIGHTBUTTON,
    TPM_LEFTBUTTON
    };
    
//  UINT AlignFlags [ TPopupAlignment ] =
  UINT AlignFlags [ 3 ] =
    {
    TPM_LEFTALIGN,
    TPM_RIGHTALIGN,
    TPM_CENTERALIGN
    };
    
//{$ENDIF}

  TPopupAlignment SaveAlignment;
/*
  {$IFDEF TB97D4}
  TTrackButton SaveTrackButton;
  {$ENDIF}
*/
  TPoint PopupPoint;
  TList *RepostList; // {pointers to TMsg's}
  TMsg Msg;
  bool Repost;
  int I;
  TPoint P;
  TCustomForm *Form;
  
  if ( FRepeating && ! FMenuIsDown )
    {
    inherited::Click ();
    return;
    }
    
  FInClick = true;
  try
    {
    if ( ! IsUpClick )
      if (GroupIndex != 0 )
        SetDown (! FDown);

    if (ButtonsStayDown )
      {
      if ( FState == bsUp || FState == bsMouseIn )
        {
        if ( FMouseInControl )
          FState = bsMouseInDown;

        else
          FState = bsDown;

        Invalidate ();
        }
      }

    else
      {
      if (FState == bsDown || FState == bsMouseInDown )
        {
        if ( FDown && (FGroupIndex != 0) )
          {
          if ( FMouseInControl )
            FState = bsMouseInExclusive;

          else
            FState = bsExclusive;
          }

        else
          FState = bsUp;

        Invalidate ();
        }
      }

//    { Stop tracking }
    MouseLeft ();
    if (( ! FUsesDropdown) || (FDropdownCombo && ! FMenuIsDown) )
      {
      Form = GetParentForm(this);
      if (Form != NULL )
        Form->ModalResult = ModalResult;

      if ( ! IsUpClick )
        inherited::Click ();
      }
      
    else
      {
      if ( ! FDropdownCombo )
        if ( ! IsUpClick )
          inherited::Click ();
/*
      { It must release its capture before displaying the popup menu since
        this control uses csCaptureMouse. If it doesn't, the VCL seems to
        get confused and think the mouse is still captured even after the
        popup menu is displayed, causing mouse problems after the menu is
        dismissed. }
*/
      MouseCapture = false;
      try
        {
        SaveAlignment = DropdownMenu->Alignment;
/*
        {$IFDEF TB97D4}
        SaveTrackButton = DropdownMenu.TrackButton;
        {$ENDIF}
*/
        try
          {
          DropdownMenu->Alignment = paLeft;
          PopupPoint = Point(0, Height);
/*
          if (( Parent is TCustomToolWindow97 ) and
             (GetDockTypeOf(TCustomToolWindow97Access(Parent).DockedTo) == dtLeftRight) )

            {
            // Drop out right or left side
            if (TCustomToolWindow97Access(Parent).DockedTo.Position == dpLeft )
              PopupPoint = Point(Width, 0);
            else
              {
              PopupPoint = Point(0, 0);
              DropdownMenu->Alignment = paRight;
              }
            }
*/

          PopupPoint = ClientToScreen(PopupPoint);
//         with DropdownMenu do {
            DropdownMenu->PopupComponent = this;
            /* In Delphi versions prior to 4 it avoids using the Popup method
              of TPopupMenu because it always uses the "track right button"
              flag (which disallowed the "click and drag" selecting motion many
              people are accustomed to). Delphi 4 has a TrackButton __property
              to control the tracking button, so it can use the Popup method. */
//            {$IFNDEF TB97D4}
            if (( dynamic_cast <TPopupMenu *> ( DropdownMenu )) && DropdownList )
              {
              if ( DropdownMenu->OnPopup )
                DropdownMenu->OnPopup (DropdownMenu);
                
              TrackPopupMenu ( DropdownMenu->Handle, AlignFlags[Alignment] || ButtonFlags[NewStyleControls],
                PopupPoint.x, PopupPoint.y, 0, DropdownList->Window, NULL);
              }

            else
              {
/*
            {$ELSE}

              if (NewStyleControls )
                TrackButton = tbLeftButton;

              else
                TrackButton = tbRightButton;

            {$ENDIF}
*/
              DropdownMenu->Popup (PopupPoint.x, PopupPoint.y);
//            {$IFNDEF TB97D4}
            }
//            {$ENDIF}
          }
        __finally
          {
          DropdownMenu->Alignment = SaveAlignment;
/*
          {$IFDEF TB97D4}
          DropdownMenu.TrackButton = SaveTrackButton;
          {$ENDIF}
*/
          }
        }
      __finally
        {
        /* To prevent a mouse click from redisplaying the menu, filter all
          mouse up/down messages, and repost the ones that don't need
          removing. This is sort of bulky, but it's the only way I could
          find that works perfectly and like Office 97. */
          
        RepostList = new TList;
        try
          {
          while ( PeekMessage (&Msg, 0, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK,
             PM_REMOVE || PM_NOYIELD ) )
              {
             /* ^ The WM_LBUTTONDOWN to WM_MBUTTONDBLCLK range encompasses all
               of the DOWN and DBLCLK messages for the three buttons */
//            with Msg do {
              Repost = true;
//              switch ( Msg.Message )
              switch ( Msg.message )
                {
                case WM_LBUTTONDOWN :
                case WM_LBUTTONDBLCLK:
                case WM_RBUTTONDOWN:
                case WM_RBUTTONDBLCLK:
                case WM_MBUTTONDOWN:
                case WM_MBUTTONDBLCLK:
                  {
                  TSmallPoint SmallPoint = *((TSmallPoint *)&Msg.lParam );
                  P.x = SmallPoint.x;
                  P.y = SmallPoint.y;
//                  P = SmallPointToPoint(TSmallPoint(*(TSmallPoint *)&Msg.lParam));
                  ::ClientToScreen ( Msg.hwnd, &P );
                  if (FindDragTarget(P, true) == this )
                    Repost = false;
                  }
                }
                
              if (Repost )
                {
                RepostList->Add ( new Byte [ sizeof(TMsg)] );
                (*((TMsg*)RepostList->Last ())) = Msg;
                }
              }
            }
            
        __finally
          {
          for ( I = 0; I < RepostList->Count; I ++ )
            {
//            with PMsg(RepostList[I])^ do
            PostMessage ( ((TMsg *)RepostList->Items [I])->hwnd, ((TMsg *)RepostList->Items [I])->message, ((TMsg *)RepostList->Items [I])->wParam, ((TMsg *)RepostList->Items [I])->lParam );
            delete (TMsg *) ( RepostList->Items[I] );
            }
            
          delete RepostList;
          }
        }
      }
    }
  __finally
    {
    FInClick = false;
    if (FState == bsDown || FState == bsMouseInDown )
      FState = bsUp;
    /* Need to check if (it's destroying in case the OnClick handler freed
      the button. If it doesn't check this here, it can sometimes cause an
      access violation */
    if ( !( ComponentState.Contains ( csDestroying ) ))
      {
      Invalidate ();
      UpdateTracking ();
      }
  }
}

void __fastcall TBMShapedButton::Click ()
{
  ClickUp ( false );
}

HPALETTE __fastcall TBMShapedButton::GetPalette ()
{
  return Glyph->Palette;
}

Graphics::TBitmap *__fastcall TBMShapedButton::GetGlyph ()
{
  return (( TButtonGlyph *) FGlyph )->Glyph;
}

void __fastcall TBMShapedButton::SetGlyph ( Graphics::TBitmap *Value )
{
  (( TButtonGlyph *) FGlyph )->Glyph = Value;
//---> Begin ED
  AdjustBounds ();
// End ED
  Invalidate ();
}

/*
Graphics::TBitmap* __fastcall TBMShapedButton::GetGlyphMask(void)
{
  return (( TButtonGlyph *) FGlyph )->GlyphMask;
}

void __fastcall TBMShapedButton::SetGlyphMask(Graphics::TBitmap* Value)
{
  (( TButtonGlyph *) FGlyph )->GlyphMask = Value;
  Redraw (true);
}
*/

int __fastcall TBMShapedButton::GetImageIndex(void)
{
  return (( TButtonGlyph *) FGlyph )->FImageIndex;
}

void __fastcall TBMShapedButton::SetImageIndex ( int Value )
{
  if ((( TButtonGlyph *) FGlyph )->FImageIndex != Value )
    {
    (( TButtonGlyph *) FGlyph )->FImageIndex = Value;
    if (((( TButtonGlyph *) FGlyph )->FImageList) )
      (( TButtonGlyph *) FGlyph )->GlyphChanged (NULL);
    }
}

TCustomImageList* __fastcall TBMShapedButton::GetImages(void)
{
  return (( TButtonGlyph *) FGlyph )->FImageList;
}

void __fastcall TBMShapedButton::SetReliefSize ( int Value )
{
  if ( Value < 1 )
    Value = 1;

  if ( Value > 6 )
    Value = 6;

  if ( FReliefSize != Value )
    {
    FReliefSize = Value;
    (( TButtonGlyph *) FGlyph )->Invalidate ();
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }

}

void __fastcall TBMShapedButton::SetImages ( TCustomImageList* Value )
{
//  with TButtonGlyph(FGlyph) do
    if ( (( TButtonGlyph *) FGlyph )->FImageList != Value ) {
      if ( (( TButtonGlyph *) FGlyph )->FImageList != NULL )
        (( TButtonGlyph *) FGlyph )->FImageList->UnRegisterChanges ((( TButtonGlyph *) FGlyph )->FImageChangeLink);
      (( TButtonGlyph *) FGlyph )->FImageList = Value;
      if ((( TButtonGlyph *) FGlyph )->FImageList != NULL ) {
        if ((( TButtonGlyph *) FGlyph )->FImageChangeLink == NULL ) {
          (( TButtonGlyph *) FGlyph )->FImageChangeLink = new TChangeLink;
          (( TButtonGlyph *) FGlyph )->FImageChangeLink->OnChange = GlyphChanged;
        }
        (( TButtonGlyph *) FGlyph )->FImageList->RegisterChanges ((( TButtonGlyph *) FGlyph )->FImageChangeLink);
        (( TButtonGlyph *) FGlyph )->FImageList->FreeNotification (this);
      }
      else {
        delete (( TButtonGlyph *) FGlyph )->FImageChangeLink;
        (( TButtonGlyph *) FGlyph )->FImageChangeLink = NULL;
      }
      (( TButtonGlyph *) FGlyph )->UpdateNumGlyphs ();
    }
}

TBMNumGlyphs __fastcall TBMShapedButton::GetNumGlyphs(void)
{
  return (( TButtonGlyph *) FGlyph )->NumGlyphs;
}

void __fastcall TBMShapedButton::SetNumGlyphs(TBMNumGlyphs Value)
{
/*
  if (Value < Low(TBMNumGlyphs) )
    Value = Low(TBMNumGlyphs)
  else
  if (Value > High(TBMNumGlyphs) )
    Value = High(TBMNumGlyphs);
*/
  if (Value != (( TButtonGlyph *) FGlyph )->NumGlyphs )
    {
    (( TButtonGlyph *) FGlyph )->NumGlyphs = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::GlyphChanged( TObject */*Sender*/ )
{
//---> Begin ED
  AdjustBounds ();
// End ED
  Invalidate ();
}

void __fastcall TBMShapedButton::UpdateExclusive ()
{
  int I;
  TControl *Ctl;

  if ((FGroupIndex != 0) && (Parent != NULL) )
//    with Parent do
      for ( I = 0; I < Parent->ControlCount; I ++ )
        {
        Ctl = Parent->Controls[I];
        if ((Ctl != this) && ( dynamic_cast<TBMShapedButton *> (Ctl) ) )
            {
//          with TBMShapedButton(Ctl) do
            if ( ((TBMShapedButton *)Ctl)->FGroupIndex == this->FGroupIndex )
              {
              if (this->Down && ((TBMShapedButton *)Ctl)->FDown )
                {
                ((TBMShapedButton *)Ctl)->FDown = false;
                ((TBMShapedButton *)Ctl)->FState = bsUp;
                ((TBMShapedButton *)Ctl)->Invalidate();
                }

              ((TBMShapedButton *)Ctl)->FAllowAllUp = this->AllowAllUp;
              }
            }
            
        }
        
}

void __fastcall TBMShapedButton::SetDown ( bool Value )
{
  if (FGroupIndex == 0 )
    Value = false;
  if (Value != FDown )
    {
    if (FDown && ( ! FAllowAllUp) )
      return;

    FDown = Value;
    if ( ! Enabled )
      FState = Bmshapedbutton::bsDisabled;

    else
      {
      if (Value )
        {
        if ( FMouseInControl )
          FState = bsMouseInExclusive;

        else
          FState = bsExclusive;
        }

      else
        FState = bsUp;
      }

    Invalidate ();
    if (Value )
      UpdateExclusive ();
    }
}

void __fastcall TBMShapedButton::SetFlat ( bool Value )
{
  if (FFlat != Value )
    {
    FFlat = Value;
//    if (FOpaque || ! FFlat )
//      ControlStyle = ControlStyle << csOpaque;
      
//    else
//      ControlStyle = ControlStyle >> csOpaque;

//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
    
}

void __fastcall TBMShapedButton::SetGroupIndex ( int Value )
{
  if (FGroupIndex != Value )
    {
    FGroupIndex = Value;
    UpdateExclusive ();
    }
    
}

//---> Begin ED
//void __fastcall TBMShapedButton::SetLayout ( TButtonLayout Value )
void __fastcall TBMShapedButton::SetLayout(TButtonLayoutPlus Value)
// End ED
{
  if (FLayout != Value )
    {
    FLayout = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetMargin ( int Value )
{
  if ((FMargin != Value) && (Value >= -1) )
    {
    FMargin = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetBorder ( bool Value )
{
  if (FBorder != Value )
    {
    FBorder = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetOldDisabledStyle ( bool Value )
{
  if (FOldDisabledStyle != Value )
    {
    FOldDisabledStyle = Value;
//    with TButtonGlyph(FGlyph) do
      {
      ((TButtonGlyph * )FGlyph )->FOldDisabledStyle = Value;
      Invalidate ();
      }

//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetTransparent ( bool Value )
{
  if (FTransparent != Value )
    {
    FTransparent = Value;
//    if ( FOpaque || ! FFlat )
//      ControlStyle = ControlStyle << csOpaque;

//    else
//      ControlStyle = ControlStyle >> csOpaque;

    Invalidate ();
    }

}

void __fastcall TBMShapedButton::SetSpacing ( int Value )
{
  if (Value != FSpacing )
    {
    FSpacing = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetAllowAllUp ( bool Value )
{
  if (FAllowAllUp != Value )
    {
    FAllowAllUp = Value;
    UpdateExclusive ();
    }
}

void __fastcall TBMShapedButton::SetDropdownMenu ( TPopupMenu *Value )
{
  if (FDropdownMenu != Value ) {
    FDropdownMenu = Value;
    FUsesDropdown = ( Value != NULL );
    if ( Value )
      {
      Value->FreeNotification (this);
/*
      {$IFNDEF TB97D4}
      if (DropdownList == NULL )
        DropdownList = TDropdownList.Create(NULL);
      DropdownList.AddMenu (Value);
      {$ENDIF}
*/
      }
      
    if (FDropdownArrow )
      Invalidate ();
  }
}

void __fastcall TBMShapedButton::SetWordWrap ( bool Value )
{
  if (FWordWrap != Value )
    {
    FWordWrap = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetAlignment ( TAlignment Value )
{
  if (FAlignment != Value )
    {
    FAlignment = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
    
}

void __fastcall TBMShapedButton::SetDropdownArrow ( bool Value )
{
  if (FDropdownArrow != Value )
    {
    FDropdownArrow = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
    
}

void __fastcall TBMShapedButton::SetDropdownCombo ( bool Value )
{
  int W;

  if (FDropdownCombo != Value ) {
    FDropdownCombo = Value;
//---> Begin ED
//    if (! ( ComponentState.Contains ( csLoading )) ) {
    if ( ! ( ComponentState.Contains ( csLoading )) && !AutoSize ) {
// End ED
      if ( Value )
        Width = Width + DropdownComboWidth;
        
      else
        {
        W = Width - DropdownComboWidth;
        if (W < 1 ) W = 1;
        Width = W;
        }
      }
      
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

void __fastcall TBMShapedButton::SetDisplayMode ( TButtonDisplayMode Value )
{
  if (FDisplayMode != Value )
    {
    FDisplayMode = Value;
//---> Begin ED
    AdjustBounds ();
// End ED
    Invalidate ();
    }
}

bool __fastcall TBMShapedButton::GetCallDormant ()
{
  return (( TButtonGlyph *) FGlyph )->FCallDormant;
}

void __fastcall TBMShapedButton::SetCallDormant ( bool Value )
{
  (( TButtonGlyph *) FGlyph )->FCallDormant = Value;
}

void __fastcall TBMShapedButton::WMLButtonDblClk(Messages::TWMMouse &Message)
{
  inherited::Dispatch ( &Message );
  if (FDown )
    DblClick ();
}

void __fastcall TBMShapedButton::CMEnabledChanged(Messages::TMessage &Message)
{
  if ( ! Enabled )
    {
    FState = Bmshapedbutton::bsDisabled;
    FMouseInControl = false;
    FMouseIsDown = false;
    RemoveButtonMouseTimer ();
    Perform (WM_CANCELMODE, 0, 0);
    }
  else if (FState == Bmshapedbutton::bsDisabled )
    if (FDown && (FGroupIndex != 0) )
      {
      if ( FMouseInControl )
        FState = bsMouseInExclusive;

      else
        FState = bsExclusive;
      }

    else
      FState = bsUp;
      
  Invalidate ();
  inherited::Dispatch ( &Message );
}

void __fastcall TBMShapedButton::CMDialogChar(Messages::TWMKey &Message)
{
//  with Message do
    if (IsAccel(Message.CharCode, Caption) && Parent && Parent->CanFocus () &&
       Enabled && Visible && (DisplayMode != dmGlyphOnly) )
      {
      /* NOTE: There is a bug in TSpeedButton where accelerator keys are still
        processed even when the button is not visible. The 'and Visible'
        corrects it, so TBMShapedButton doesn't have this problem. */
      Click ();
//      return 1;
      }

    else
      inherited::Dispatch ( &Message );
}

void __fastcall TBMShapedButton::CMDialogKey(Messages::TWMKey &Message)
{
//  with Message do
    if ((((Message.CharCode == VK_RETURN) && FDefault) ||
        ((Message.CharCode == VK_ESCAPE) && FCancel)) &&
       (KeyDataToShiftState(Message.KeyData) == TShiftState ()) &&
       Parent && Parent->CanFocus () && Enabled && Visible )
      {
      Click ();
//      Result = 1;
      }

    else
      inherited::Dispatch ( &Message );
}

void __fastcall TBMShapedButton::CMFontChanged(Messages::TMessage &/*Message*/)
{
//---> Begin ED
  AdjustBounds ();
// End ED
  Invalidate ();
}

void __fastcall TBMShapedButton::CMTextChanged(Messages::TMessage &/*Message*/)
{
//---> Begin ED
  AdjustBounds ();
// End ED
  Invalidate ();
}

void __fastcall TBMShapedButton::CMSysColorChange(Messages::TMessage &Message)
{
  inherited::Dispatch ( &Message );
  if ( Pattern &&
     ((PatternBtnFace != TColor(GetSysColor(COLOR_BTNFACE))) ||
      (PatternBtnHighlight != TColor(GetSysColor(COLOR_BTNHIGHLIGHT)))) )
    {
    delete Pattern;
    Pattern = NULL;
    }

//  with TButtonGlyph(FGlyph) do {
  ((TButtonGlyph *) FGlyph )->Invalidate ();
  ((TButtonGlyph *) FGlyph )->CreateButtonGlyph (FState, FReliefSize, ComponentState.Contains ( csDesigning ) );
//  }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::WMSize(Messages::TMessage &Msg)
{
  Invalidate ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::WMMove(Messages::TMessage &Msg)
{
  Invalidate ();
  inherited::Dispatch ( &Msg );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::BMChangeView    ( Messages::TMessage    &Msg )
{
  inherited::Dispatch ( &Msg );
  Invalidate ();
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::CMMouseEnter    ( TMessage& Msg )
{
  if ( Enabled )
    {
    if ( ! ComponentState.Contains ( csDesigning ) )
      {
      if ( ! AlreadyPlayngEntryFlag )
        {
//---> Begin ED
/*
        if ( FHighlightSound )
          {
          if ( ! FHighlightSound->Empty () )
            {
*/
        AlreadyPlayngEntryFlag = true;
#ifdef _USE_BMWAVE_
        PlayHighlightSound ();
#endif
/*
            }
          }
*/
// End ED
        }
      }
    }              

  MouseEntered ();
  FMouseInControl = true;
  inherited::Dispatch ( &Msg );
}

void __fastcall TBMShapedButton::CMMouseLeave    ( TMessage& Msg )
{
  MouseLeft ();
  FMouseInControl = false;
  inherited::Dispatch ( &Msg );
  AlreadyPlayngEntryFlag = false;

  if ( FCloudArray )
    {
//---> Begin ED
//    delete FCloudArray;
//    delete FCloudArray1;
    delete[]FCloudArray;
    delete[]FCloudArray1;
// End ED
    FCloudArray = NULL;
    FCloudArray1 = NULL;
    }
}

void __fastcall TBMShapedButton::MouseEntered()
{
  if ( Enabled )
    {
    FEnableFlash = true;
    Delay = 8;
    if ( FState == bsExclusive )
      FState = bsMouseInExclusive;

    if (Enabled && ! FMouseInControl )
      {
      FMouseInControl = true;
      if ( FState == bsMouseIn && FMouseIsDown )
        FState = bsMouseInDown;

      if (FState == bsUp )
        FState = bsMouseIn;

      if (FFlat || (NumGlyphs >= 7) )
        Invalidate ();

      if ( FOnMouseEnter )
        FOnMouseEnter (this);
      }
    }

}

void __fastcall TBMShapedButton::MouseLeft ()
{
  TBMButtonState OldState;

  if (Enabled )
    {
    if ( FState == bsMouseInExclusive )
      FState = bsExclusive;

    if ( FMouseInControl )
      {
      if ( ! FMouseIsDown )
        {
        FMouseInControl = false;
        RemoveButtonMouseTimer ();
        OldState = FState;
        if ((FState == bsMouseIn) || ( ! FInClick && (FState == bsDown || FState == bsMouseInDown)) )
          {
          if (FDown && (FGroupIndex != 0) )
            {
            if ( FMouseInControl )
              FState = bsMouseInExclusive;

            else
              FState = bsExclusive;
            }

          else
            FState = bsUp;
          }

        if (FFlat || ((NumGlyphs >= 7) || ((OldState == bsMouseIn) ^ (FState != OldState))) )
          Invalidate ();

        if ( FOnMouseExit )
          FOnMouseExit (this);
        }

      else
        {
        if ( ! FInClick )
          {
          FState = bsMouseIn;
          Invalidate ();
          }
        }
      }

    else
      {
      FState = bsUp;
      Invalidate ();
      FEnableFlash = false;
      }
    }

}
//---------------------------------------------------------------------------
//---> Begin ED
#ifdef _USE_BMWAVE_
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::PlaySound ( Bmwave::TBMCustomSound * S )
{
  if ( FEnableSounds && S && ! S->Empty () ) S->Play ();
}
//---------------------------------------------------------------------------
/*
void __fastcall TBMShapedButton::PlaySelectSound ()
{
  if ( ! FEnableSounds )
    return;

  if ( FSelectSound )
    FSelectSound->Play ();
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::PlayPulseSound ()
{
  if ( ! FEnableSounds )
    return;

  if ( FPulseSound )
    FPulseSound->Play ();
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::PlayHighlightSound ()
{
  if ( ! FEnableSounds )
    return;

  if ( FHighlightSound )
    FHighlightSound->Play ();
}
*/
//---------------------------------------------------------------------------
#endif
// End ED
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetFlashing ( bool Value )
{
  FFlashing = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetUnFlashedDelay ( int Value )
{
  if ( Value < 10 )
    FUnFlashedDelay = 10;

  else
    FUnFlashedDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetFlashingDelay ( int Value )
{
  if ( Value < 10 )
    FFlashingDelay = 10;

  else
    FFlashingDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetFlashedDelay ( int Value )
{
  if ( Value < 10 )
    FFlashedDelay = 10;

  else
    FFlashedDelay = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetCloudType ( TBMCloudType Value )
{
  if ( FCloudType != Value )
    {
    FCloudType = Value;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetCloudColor   ( TColor _Color )
{
  if ( FCloudColor != _Color )
    {
    FCloudColor = _Color;
    Invalidate ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::UpdateHighlighted ()
{
  if ( ! FFlashing )
    return;

  if ( Delay )
    {
    Delay --;
    return;
    }

  if ( ( FState == bsMouseIn || FState == bsMouseInDown || FState == bsMouseInExclusive ) && FEnableFlash )
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
//---> Begin ED
        {
#ifdef _USE_BMWAVE_
        PlayPulseSound ();
#endif
        }
// End ED

      AlreadyPlayngEntryFlag = false;
      }

//    Invalidate ();
    Invalidate ();
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::ButtonMouseTimerHandler ( TObject */*Sender*/ )
{
  TPoint P;
  /* The button mouse timer is used to periodically check if (mouse has left.
    Normally it receives a CM_MOUSELEAVE, but the VCL does not send a
    CM_MOUSELEAVE if (the mouse is moved quickly from the button to another
    application's window. For some reason, this problem doesn't seem to occur
    on Windows NT 4 -- only 95 and 3.x.

    The timer (which ticks 8 times a second) is only enabled when the
    application is active and the mouse is over a button, so it uses virtually
    no processing power.

    For something interesting to try: If you want to know just how often this
    is called, try putting a Beep call in here */

  GetCursorPos (&P);
  if (FindDragTarget(P, true) != this )
    MouseLeft ();
}
//---------------------------------------------------------------------------
//---> Begin ED
#ifdef _USE_BMWAVE_
// End ED
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetHighlightSound ( Bmwave::TBMCustomSound *Value )
{
  FHighlightSound = Value;
//  FHighlightSound->Assign ( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetPulseSound ( Bmwave::TBMCustomSound *Value )
{
  FPulseSound = Value;
//  FPulseWave->Assign ( Value );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::SetSelectSound ( Bmwave::TBMCustomSound *Value )
{
  FSelectSound = Value;
//  FSelectWave->Assign ( Value );
}
//---------------------------------------------------------------------------
//---> Begin ED
#endif
// End ED
//---------------------------------------------------------------------------
#ifdef __BCB_40__
bool __fastcall TBMShapedButton::IsCheckedStored ()
{
  TR ( "TBMShapedButton::IsCheckedStored" );
  if ( ActionLink )
    return ! (( TBMShapedButtonActionLink *)ActionLink)->IsCheckedLinked ();
    
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButton::IsHelpContextStored ()
{
  TR ( (String)"TBMShapedButton::IsHelpContextStored" + (String)(long)ActionLink );
  if ( ActionLink )
    return ! (( TBMShapedButtonActionLink *)ActionLink)->IsHelpContextLinked ();

  return true;
//  return (ActionLink == NULL) | ! (( TBMShapedButtonActionLink *)ActionLink)->IsHelpContextLinked ();
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButton::IsImageIndexStored ()
{
  TR ( "TBMShapedButton::IsImageIndexStored" );
  if ( ActionLink )
    return ! (( TBMShapedButtonActionLink *)ActionLink)->IsImageIndexLinked ();
    
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::ActionChange(System::TObject* Sender, bool CheckDefaults)
{
  TR ( "TBMShapedButton::ActionChange" );
  
  inherited::ActionChange( Sender, CheckDefaults );

  TCustomAction *CustomAction = dynamic_cast <TCustomAction *> ( Sender );

  if ( CustomAction )
    {
//    with TCustomAction(Sender) do
//    begin
    if ( ! CheckDefaults || ( Down == false))
      Down = CustomAction->Checked;

    if ( ! CheckDefaults || ( HelpContext == 0))
      HelpContext = CustomAction->HelpContext;

    if ( ! CheckDefaults || (ImageIndex == -1))
      ImageIndex = CustomAction->ImageIndex;
        
    }

}
//---------------------------------------------------------------------------
TMetaClass* __fastcall TBMShapedButton::GetActionLinkClass(void)
{
  TR ( "TBMShapedButton::GetActionLinkClass" );
  return __classid ( TBMShapedButtonActionLink );
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButton::AssignTo(Classes::TPersistent* Dest)
{
  TR ( "TBMShapedButton::AssignTo" );
  inherited::AssignTo(Dest);
  TCustomAction *CustomAction = dynamic_cast <TCustomAction *> ( Dest );

  if ( CustomAction )
    CustomAction->Checked = Down;
}
#endif
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#ifdef __BCB_40__
void __fastcall TBMShapedButtonActionLink::AssignClient(System::TObject* AClient)
{
  inherited::AssignClient ( AClient );
  FClient = dynamic_cast <TBMShapedButton *> ( AClient );
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButtonActionLink::IsCheckedLinked(void)
{
  return inherited::IsCheckedLinked () &&
    (FClient->Down == ( (TCustomAction *)Action )->Checked );
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButtonActionLink::IsHelpContextLinked(void)
{
  TR ( "TBMShapedButtonActionLink::IsHelpContextLinked" );
  return inherited::IsHelpContextLinked () &&
    (FClient->HelpContext == ((TCustomAction *)Action )->HelpContext );
}
//---------------------------------------------------------------------------
bool __fastcall TBMShapedButtonActionLink::IsImageIndexLinked(void)
{
  return inherited::IsImageIndexLinked () &&
    (FClient->ImageIndex == ((TCustomAction *)Action )->ImageIndex);
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButtonActionLink::SetChecked(bool Value)
{
  if ( IsCheckedLinked () )
    FClient->Down = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButtonActionLink::SetHelpContext( Classes::THelpContext Value )
{
  if ( IsHelpContextLinked () )
    FClient->HelpContext = Value;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedButtonActionLink::SetImageIndex(int Value)
{
  if ( IsImageIndexLinked () )
    FClient->ImageIndex = Value;
}
#endif
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    void __fastcall PACKAGE Register()
    {
      TComponentClass classes[1] = {__classid(TBMShapedButton)};
      RegisterComponents("BMitov", classes, 0);

//      RegisterPropertyEditor(__typeinfo(TBMWaveData), 0L, "", __classid(TBMWaveDataProperty));
    }
}
//---------------------------------------------------------------------------
