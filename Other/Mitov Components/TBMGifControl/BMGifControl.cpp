//---------------------------------------------------------------------------
#include <vcl.h>
#include <mmsystem.h>
#include <stdlib.h>
#include <memory.h>
#include <assert.h>
#pragma hdrstop

#include "BMGifControl.h"

//#define _GIF_TRACE

#ifdef _GIF_TRACE
  #include <fstream.h>
#endif
                   
//---------------------------------------------------------------------------
#pragma package(smart_init)


//---------------------------------------------------------------------------
namespace Bmgifcontrol
{
  Word CF_GIF; // Clipboard format for GIF image.
//---------------------------------------------------------------------------
static inline ValidCtrCheck(TBMGIFControl *)
{
	new TBMGIFControl(( TComponent * ) NULL);
}

//---------------------------------------------------------------------------

class TEnterCriticalSection
{
  TCriticalSection *Section;

public :
  TEnterCriticalSection ( TCriticalSection *_Section ) :
    Section ( _Section )
      {
      Section->Enter ();
      }

  ~TEnterCriticalSection ()
      {
      Section->Leave ();
      }
};

#define CRITICAL(a)  TEnterCriticalSection __Enter ( a )

#ifdef _GIF_TRACE
int DebugOffset = 0;

class TMiniTrace
{
  AnsiString Message;

public :
  TMiniTrace ( AnsiString _Message ) :
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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const char LoopExt [11] = "NETSCAPE2.0";
const Byte AE_LOOPING          = 0x01;    //{ looping Netscape extension }

struct TScreenDescriptor
{
  Word ScreenWidth;            //{ logical screen width }
  Word ScreenHeight;           //{ logical screen height }
  Byte PackedFields;
  Byte BackgroundColorIndex;   //{ Index to global color table }
  Byte AspectRatio;            //{ actual ratio = (AspectRatio + 15) / 64 }
};

enum TExtensionType { etGraphic, etPlainText, etApplication, etComment};

struct TGraphicControlExtension
{
  Byte BlockSize; /* should be 4 */
  Byte PackedFields;
  Word DelayTime; /* in centiseconds */

  Byte TransparentColorIndex;
  Byte Terminator;
};

struct TPlainTextExtension
{
  Byte BlockSize; /* should be 12 */
  Word Left, Top, Width, Height;
  Byte CellWidth, CellHeight;
  Byte FGColorIndex, BGColorIndex;
};

struct TAppExtension
{
  Byte BlockSize; /* should be 11 */
//  AppId: array[1..8] of Byte;
  Byte AppId [ 8 ];
  Byte Authentication [ 3 ];
};


struct TExtensionRecord
{
  TExtensionType ExtensionType;
  union
    {
    TGraphicControlExtension    GCE;
    TPlainTextExtension         PTE;
    TAppExtension               APPE;
    };
/*
    case ExtensionType: TExtensionType of
      etGraphic: (GCE: TGraphicControlExtension);
      etPlainText: (PTE: TPlainTextExtension);
      etApplication: (APPE: TAppExtension);
*/
};


class  TExtension : public TPersistent
{
  typedef TPersistent inherited;
  public:
    virtual __fastcall ~TExtension ();

  public:
    TExtensionType FExtType;
    TStrings *FData;
    TExtensionRecord FExtRec;

  public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
    bool __fastcall IsLoopExtension ();
};
//---------------------------------------------------------------------------
__fastcall TExtension::~TExtension ()
{
  if ( FData )
    delete FData;
}
//---------------------------------------------------------------------------
void __fastcall TExtension::Assign(Classes::TPersistent* Source)
{
  if (( Source != NULL ) && ( dynamic_cast<TExtension *> (Source )))
    {
    TExtension *Extt = ( TExtension *) Source; 
    FExtType = Extt->FExtType;
    FExtRec = Extt->FExtRec;
    if ( Extt->FData != NULL )
      {
      if ( FData == NULL )
         FData = new TStringList;

      FData->Assign(Extt->FData);
      }
    }

  else
    inherited::Assign(Source);
}
//---------------------------------------------------------------------------
bool __fastcall TExtension::IsLoopExtension ()
{
/*
  Result = (FExtType = etApplication) and CompareMem(@FExtRec.APPE.AppId,
    @LoopExt[1], FExtRec.APPE.BlockSize) and (FData.Count > 0) and
    (Length(FData[0]) >= 3) and (Byte(FData[0][1]) = AE_LOOPING);
*/
  return (FExtType == etApplication) && CompareMem( &FExtRec.APPE.AppId,
    (void *)LoopExt, FExtRec.APPE.BlockSize) && (FData->Count > 0) &&
    (FData->Strings[0].Length() >= 3) && (Byte(FData->Strings[0][1]) == AE_LOOPING);
}
//---------------------------------------------------------------------------
namespace
{
//---------------------------------------------------------------------------
  AnsiString GIFSignature = "GIF";
  AnsiString GIFVersionStr [TBMGIFData::MaxVer + 1] = { "\0\0\0", "87a", "89a" };

  const long int PaletteMask = 33554432;

  const int  CODE_TABLE_SIZE = 4096;
  const int  HASH_TABLE_SIZE = 17777;
  const Word MAX_LOOP_COUNT  = 30000;

  const int  GCE_TRANSPARENT     = 0x1; //{ whether a transparency Index is given }
  const int  GCE_USER_INPUT      = 0x2; //{ whether or not user input is expected }
  const int  GCE_DISPOSAL_METHOD = 0x1C; //{ the way in which the graphic is to be treated after being displayed }
  const int  GCE_RESERVED        = 0xE0; //{ reserved - must be set to $00 }

  const int  ID_LOCAL_COLOR_TABLE  = 0x80;  //{ set if a local color table follows }
  const int  ID_INTERLACED         = 0x40;  //{ set if image is interlaced }
  const int  ID_SORT               = 0x20;  //{ set if color table is sorted }
  const int  ID_RESERVED           = 0x0C;  //{ reserved - must be set to $00 }
  const int  ID_COLOR_TABLE_SIZE   = 0x07;  //{ Size of color table as above }

  const Byte LSD_COLOR_TABLE_SIZE  = 0x07;  //{ Size of global color table - 3 bits }
                                           //{ Actual Size = 2^value+1    - value is 3 bits }

  const Byte LSD_COLOR_RESOLUTION  = 0x07;  //{ Color resolution - 3 bits }
  const int  LSD_GLOBAL_COLOR_TABLE= 0x80;  //{ set if global color table follows L.S.D. }
  Byte ExtLabels [] = { 0xF9, 0x01, 0xFF, 0xFE };

  const char CHR_EXT_INTRODUCER    = '!';
  const char CHR_IMAGE_SEPARATOR   = ',';
  const char CHR_TRAILER           = ';';  //{ indicates the end of the GIF Data stream }

  const int MAX_BITS = 8;

  const int   MAX_COLORS = 4096;

  const int   MAX_N_COLS = 2049;
  const int   MAX_N_HASH = 5191;

//---------------------------------------------------------------------------
const Byte Scale04[4] = {0, 85, 170, 255};
const Byte Scale06[6] = {0, 51, 102, 153, 204, 255};
const Byte Scale07[7] = {0, 43, 85, 128, 170, 213, 255};
const Byte Scale08[8] = {0, 36, 73, 109, 146, 182, 219, 255};
//---------------------------------------------------------------------------
Byte TruncIndex04 [255];
Byte TruncIndex06 [255];
Byte TruncIndex07 [255];
Byte TruncIndex08 [255];
//---------------------------------------------------------------------------
Byte NearestIndex( Byte Value, Byte const Bytes [], int Size )
{
  Byte B;
  int I;                                
  Word Diff, DiffMin;                 
                                          
  Byte Result = 0;                               

  B = Bytes[0];                       
  DiffMin = (Word)abs(Value - B);               
  for ( I = 0; I < Size; I ++ )            
    {
    B = Bytes[I];
    Diff = (Word)abs(Value - B);
    if ( Diff < DiffMin )
      {
      DiffMin = Diff;
      Result = (Byte)I;
      }
    }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall InitTruncTables ()
{
  int I;

//  { For 7 Red X 8 Green X 4 Blue palettes etc. }
  for ( I = 0; I <= 255; I ++ )
    {
    TruncIndex04[I] = NearestIndex(Byte(I), Scale04, 4 );
    TruncIndex06[I] = NearestIndex(Byte(I), Scale06, 6 );
    TruncIndex07[I] = NearestIndex(Byte(I), Scale07, 7 );
    TruncIndex08[I] = NearestIndex(Byte(I), Scale08, 8 );
    }
}


static unsigned int uTimerID = -1;
static TThreadList    *BmGifsList;
static TCriticalSection *CriticalSection;
static bool CallBackEnabled = false;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
static void CALLBACK
timeoutCallback(unsigned int timerId,
                unsigned int msg,
                unsigned long timeoutFunction,
                unsigned long dw1,
                unsigned long dw2)
{
  uTimerID = -1;

  if ( CallBackEnabled )
    {
    if ( CriticalSection )
      {
      CRITICAL (CriticalSection);
      TList *List = BmGifsList->LockList ();
      for ( int i = 0; i < List->Count; i ++ )
        ((TBMGIFControl *) List->Items [ i ] )->OnTimer ();

      BmGifsList->UnlockList();
      }
    }
    
  uTimerID = timeSetEvent( 1,
                           1,
                           timeoutCallback,
                           0,
                           TIME_ONESHOT);
}
//---------------------------------------------------------------------------
class TLocalInit
{

public :
  TLocalInit ()
    {
    #ifdef _GIF_TRACE
    ofstream of ( "c:\\Trece.txt", ios::trunc );
    #endif

    TR( "TLocalInit" );

    InitTruncTables ();
  	TComponentClass classes2[2] = {__classid(TBMGIFFrame), __classid(TBMGIFImage) };
    RegisterClasses(classes2, 1 );

//    TPicture::RegisterFileFormat(__classid(TPicture), "gif", "GIF Image", __classid(TBMGIFImage));
    TPicture::RegisterFileFormat(__classid(TBMGIFImage), "gif", "GIF Image", __classid(TBMGIFImage));
//    TPicture::RegisterFileFormat(NULL, "gif", "GIF Image", __classid(TBMGIFImage));
    CF_GIF = (Word)RegisterClipboardFormat( "GIF Image" );
//    TPicture::RegisterClipboardFormat( __classid(TPicture), CF_GIF, __classid(TBMGIFImage));
    TPicture::RegisterClipboardFormat( __classid(TBMGIFImage), CF_GIF, __classid(TBMGIFImage));
//    TPicture::RegisterClipboardFormat( NULL, CF_GIF, __classid(TBMGIFImage));

    CriticalSection = new TCriticalSection;
    BmGifsList = new TThreadList;

    CallBackEnabled = true;
    uTimerID = timeSetEvent( 1,
                             1,
                             timeoutCallback,
                             0,
                             TIME_ONESHOT);

    }

  ~TLocalInit ()
    {
    TR( "~TLocalInit" );

    CallBackEnabled = false;

    if ( uTimerID != -1 )
      timeKillEvent ( uTimerID );


    delete BmGifsList;
    TPicture::UnregisterGraphicClass(__classid(TPicture), __classid(TBMGIFImage));
    TCriticalSection *Tmp = CriticalSection;
    CriticalSection = NULL;
    delete Tmp;
    }
};

TLocalInit LocalInit;
//---------------------------------------------------------------------------
typedef TRGBQuad TRGBPalette [255];

typedef TRGBPalette *PRGBPalette;

//Word CF_GIF;

struct TBMGIFHeader
{
  char Signature [3];  // { contains 'GIF' }
  char Version [3];    // { '87a' or '89a' }
};

struct TImageDescriptor
{
  Word ImageLeftPos;   //{ column in pixels in respect to left of logical screen }
  Word ImageTopPos;    //{ row in pixels in respect to top of logical screen }
  Word ImageWidth;     //{ width of image in pixels }
  Word ImageHeight;    //{ height of image in pixels }
  Byte PackedFields;
};


typedef Word TIntCodeTable [ CODE_TABLE_SIZE ];

//typedef TIntCodeTable *PIntCodeTable;
typedef Word *PIntCodeTable;




//---------------------------------------------------------------------------
TExtension *FindExtension ( TList *Extensions, TExtensionType ExtType )
{
  int I;
  TExtension *Result = NULL;
  
  if ( Extensions )
    {
//    for ( I = Extensions->Count; --I; )
    for ( I = Extensions->Count; I--; )
      {
      Result = ( TExtension *) Extensions->Items[I];
      if (Result && Result->FExtType == ExtType )
        return Result;
      }
    }

  return NULL;
}
//---------------------------------------------------------------------------
void FreeExtensions( TList *Extensions )
{
  if ( Extensions )
    {
    while ( Extensions->Count > 0 )
      {
//      TObject(Extensions[0]).Free;
      delete ((TObject *) Extensions->Items [0] );
      Extensions->Delete ( 0 );
      }

    delete Extensions;
    }
}
//---------------------------------------------------------------------------
class TParentControl : public TWinControl
{
  friend CopyParentImage;
  public :
  	virtual void __fastcall PaintWindow(HDC DC) { TWinControl::PaintWindow (DC); }

};
//---------------------------------------------------------------------------
void __fastcall CopyParentImage( TControl *Control, TCanvas *Dest )
{
  int I, Count, X, Y, SaveIndex;
  HDC DC;
//  TRect R, SelfR, CtlR;
  RECT R, SelfR, CtlR;

  if ( Control->Parent == NULL )
    return;

  Count = Control->Parent->ControlCount;
  DC = Dest->Handle;
  SelfR = Bounds( Control->Left, Control->Top, Control->Width, Control->Height);
  X = -Control->Left;
  Y = -Control->Top;
//  { Copy parent control image }
  SaveIndex = SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, NULL);
  IntersectClipRect(DC, 0, 0, Control->Parent->ClientWidth,
    Control->Parent->ClientHeight);
  (( TParentControl *)Control->Parent)->PaintWindow(DC);
  RestoreDC(DC, SaveIndex);
//  { Copy images of graphic controls }
  for ( I = 0; I < Count; I ++ )
    {
    if ((Control->Parent->Controls[I] != NULL) &&
      ! ( dynamic_cast<TWinControl* > ( Control->Parent->Controls[I] ) ) )
      {
      if ( Control->Parent->Controls[I] == Control )
        break;

//      with Control->Parent->Controls[I] do begin
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
//      }
    }
}
//---------------------------------------------------------------------------
int WidthOf( TRect R )
{
  return R.Right - R.Left;
}
//---------------------------------------------------------------------------
int HeightOf( TRect R )
{
  return R.Bottom - R.Top;
}
//---------------------------------------------------------------------------
long int ItemToRGB( TBMGIFColorItem Item )
{
  return RGB(Item.Red, Item.Green, Item.Blue);
}
//---------------------------------------------------------------------------
TPixelFormat ColorsToPixelFormat( Word Colors )
{
  if ( Colors <= 2 )
    return pf1bit;

  if ( Colors <= 16 )
    return pf4bit;
    
  if ( Colors <= 256 )
    return pf8bit;
  
  return pf24bit;
}
//---------------------------------------------------------------------------
int __fastcall PaletteEntries( HPALETTE Palette )
{
  int Result;
  GetObject(Palette, sizeof(int), &Result );
  return Result;
}
//---------------------------------------------------------------------------
long int PaletteColor( TColor Color )
{
  return ColorToRGB(Color) | PaletteMask;
}
//---------------------------------------------------------------------------
void __fastcall StretchBltTransparent( HDC DstDC, int DstX, int DstY, int DstW, int DstH,
  HDC SrcDC, int SrcX, int SrcY, int SrcW, int SrcH, HPALETTE Palette,
  TColorRef TransparentColor )
{
  TColorRef Color;
  HBITMAP bmAndBack, bmAndObject, bmAndMem, bmSave;
  HBITMAP bmBackOld, bmObjectOld, bmMemOld, bmSaveOld;
  HDC MemDC, BackDC, ObjectDC, SaveDC;
  HPALETTE palDst, palMem, palSave, palObj;

//  { Create some DCs to hold temporary data }
  BackDC = CreateCompatibleDC(DstDC);
  ObjectDC = CreateCompatibleDC(DstDC);
  MemDC = CreateCompatibleDC(DstDC);
  SaveDC = CreateCompatibleDC(DstDC);
//  { Create a bitmap for each DC }
  bmAndObject = CreateBitmap(SrcW, SrcH, 1, 1, NULL);
  bmAndBack = CreateBitmap(SrcW, SrcH, 1, 1, NULL);
  bmAndMem = CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave = CreateCompatibleBitmap(DstDC, SrcW, SrcH);
//  { Each DC must select a bitmap object to store pixel data }
  bmBackOld = SelectObject(BackDC, bmAndBack);
  bmObjectOld = SelectObject(ObjectDC, bmAndObject);
  bmMemOld = SelectObject(MemDC, bmAndMem);
  bmSaveOld = SelectObject(SaveDC, bmSave);
//  { Select palette }
  palDst = 0; palMem = 0; palSave = 0; palObj = 0;
  if ( Palette != 0 )
    {
    palDst = SelectPalette(DstDC, Palette, true);
    RealizePalette(DstDC);
    palSave = SelectPalette(SaveDC, Palette, false);
    RealizePalette(SaveDC);
    palObj = SelectPalette(ObjectDC, Palette, false);
    RealizePalette(ObjectDC);
    palMem = SelectPalette(MemDC, Palette, true);
    RealizePalette(MemDC);
    }
    
//  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
//  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
//  { Set the background color of the source DC to the color,         }
//  { contained in the parts of the bitmap that should be transparent }
  Color = (TColor)SetBkColor(SaveDC, (TColor)PaletteColor((TColor)TransparentColor));
//  { Create the object mask for the bitmap by performing a BitBlt()  }
//  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
//  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
//  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
//  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
//  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
//  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
//  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
//  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0,
    SRCCOPY);
//  { Restore palette }
  if ( Palette )
    {
    SelectPalette(MemDC, palMem, false);
    SelectPalette(ObjectDC, palObj, false);
    SelectPalette(SaveDC, palSave, false);
    SelectPalette(DstDC, palDst, true);
    }
    
//  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
//  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
}
//---------------------------------------------------------------------------
void __fastcall StretchBltTransparentMask( HDC DstDC, int DstX, int DstY, int DstW, int DstH,
  HDC SrcDC, int SrcX, int SrcY, int SrcW, int SrcH, HPALETTE Palette,
  HDC MaskSrcDC, HPALETTE MaskPalette )
{
//  TColorRef Color; // Transparency
  HBITMAP bmAndBack, bmAndObject, bmAndMem, bmSave, bmSaveTemp;
  HBITMAP bmBackOld, bmObjectOld, bmMemOld, bmSaveOld, bmSaveOldTemp;
  HDC MemDC, BackDC, ObjectDC, SaveDC, SaveDCTemp;
  HPALETTE palDst, palMem, palSave, palSaveTemp, palObj;

//  { Create some DCs to hold temporary data }
  BackDC = CreateCompatibleDC(DstDC);
  ObjectDC = CreateCompatibleDC(DstDC);
  MemDC = CreateCompatibleDC(DstDC);
  SaveDC = CreateCompatibleDC(DstDC);
  SaveDCTemp = CreateCompatibleDC(DstDC);
//  { Create a bitmap for each DC }
  bmAndObject = CreateBitmap(SrcW, SrcH, 1, 1, NULL);
  bmAndBack = CreateBitmap(SrcW, SrcH, 1, 1, NULL);
  bmAndMem = CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave = CreateCompatibleBitmap(DstDC, SrcW, SrcH);
  bmSaveTemp = CreateCompatibleBitmap(DstDC, DstW, DstH);
//  { Each DC must select a bitmap object to store pixel data }
  bmBackOld = SelectObject(BackDC, bmAndBack);
  bmObjectOld = SelectObject(ObjectDC, bmAndObject);
  bmMemOld = SelectObject(MemDC, bmAndMem);
  bmSaveOld = SelectObject(SaveDC, bmSave);
  bmSaveOldTemp = SelectObject(SaveDCTemp, bmSaveTemp);
//  { Select palette }
  palDst = 0; palMem = 0; palSave = 0; palObj = 0, palSaveTemp = 0;
  if ( Palette != 0 )
    {
    palDst = SelectPalette(DstDC, Palette, true);
    RealizePalette(DstDC);
    palSave = SelectPalette(SaveDC, Palette, false);
    RealizePalette(SaveDC);
    palSaveTemp = SelectPalette(SaveDCTemp, Palette, false);
    RealizePalette(SaveDCTemp);
    palObj = SelectPalette(ObjectDC, Palette, false);
    RealizePalette(ObjectDC);
    palMem = SelectPalette(MemDC, Palette, true);
    RealizePalette(MemDC);
    }
    
//  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
  SetMapMode(SaveDCTemp, GetMapMode(DstDC));
//  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  StretchBlt(SaveDCTemp, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCCOPY);
//  { Set the background color of the source DC to the color,         }
//  { contained in the parts of the bitmap that should be transparent }

// Transparency  Color = (TColor)SetBkColor(SaveDC, (TColor)PaletteColor((TColor)TransparentColor));

//  { Create the object mask for the bitmap by performing a BitBlt()  }
//  { from the source bitmap to a monochrome bitmap                   }

// Transparency  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
//??  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, MaskSrcDC, 0, 0, SRCCOPY); // Transparency
  StretchBlt(ObjectDC, 0, 0, DstW, DstH, MaskSrcDC, 0, 0, SrcW, SrcH, SRCCOPY); // Transparency

//  { Set the background color of the source DC back to the original  }

// Transparency  SetBkColor(SaveDC, Color);

//  { Create the inverse of the object mask }
//??  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  BitBlt(BackDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, NOTSRCCOPY);
//  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
//  { Mask out the places where the bitmap will be placed }
//??  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  BitBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SRCAND);
//  { Mask out the transparent colored pixels on the bitmap }
//??  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  BitBlt(SaveDCTemp, 0, 0, DstW, DstH, BackDC, 0, 0, SRCAND);
//  { XOR the bitmap with the background on the destination DC }
//??  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  BitBlt(MemDC, 0, 0, DstW, DstH, SaveDCTemp, 0, 0, SRCPAINT);
//  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SRCCOPY);
//  { Restore palette }
  if ( Palette )
    {
    SelectPalette(MemDC, palMem, false);
    SelectPalette(ObjectDC, palObj, false);
    SelectPalette(SaveDC, palSave, false);
    SelectPalette(SaveDCTemp, palSaveTemp, false);
    SelectPalette(DstDC, palDst, true);
    }
    
//  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  DeleteObject(SelectObject(SaveDCTemp, bmSaveOldTemp));
//  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
  DeleteDC(SaveDCTemp);
}
//---------------------------------------------------------------------------
Byte __fastcall ColorsToBits( Word ColorCount )
{
  Word GIFColors [ MAX_BITS ] = { 2, 4, 8, 16, 32, 64, 128, 256 };

  TBMGIFBits I;

  Byte Result = 0;
  for ( I = 0; I < MAX_BITS; I ++ )
    if ( ColorCount == GIFColors[I] )
      {
      Result = (TBMGIFBits)( I + 1 );
      return Result;
      }
      
  throw EInvalidGraphicOperation ( "Wrong GIF Colors" );
}
//---------------------------------------------------------------------------
void __fastcall StretchBitmapTransparent( TCanvas *Dest, Graphics::TBitmap *Bitmap,
  TColor TransparentColor, int DstX, int DstY, int DstW, int DstH, int SrcX, int SrcY,
  int SrcW, int SrcH )
{
  TNotifyEvent CanvasChanging;
  Graphics::TBitmap *Temp;

  if ( DstW <= 0 )
    DstW = Bitmap->Width;
    
  if ( DstH <= 0 )
    DstH = Bitmap->Height;

  if ((SrcW <= 0) || (SrcH <= 0))
    {
    SrcX = 0;
    SrcY = 0;
    SrcW = Bitmap->Width;
    SrcH = Bitmap->Height;
    }

  if ( ! Bitmap->Monochrome )
    SetStretchBltMode ( Dest->Handle, STRETCH_DELETESCANS);

  CanvasChanging = Bitmap->Canvas->OnChanging;
  try
    {
    Bitmap->Canvas->OnChanging = NULL;
//{$IFDEF RX_D3}
/*
    {if Bitmap.HandleType = bmDIB then begin
      Temp = TBitmap.Create;
      Temp.Assign(Bitmap);
      Temp.HandleType = bmDDB;
    end
    else}
*/
    Temp = Bitmap;
//{$ELSE}
//    Temp = Bitmap;
//{$ENDIF}
    try
      {
      if ( TransparentColor == clNone )
        {
        StretchBlt( Dest->Handle, DstX, DstY, DstW, DstH, Temp->Canvas->Handle,
          SrcX, SrcY, SrcW, SrcH, Dest->CopyMode);
        }

      else
        {
//{$IFDEF RX_D3}

        if ( TransparentColor == clDefault )
          TransparentColor = Temp->Canvas->Pixels[ 0 ] [Temp->Height - 1];

//{$ENDIF}
        if ( Temp->Monochrome )
          TransparentColor = clWhite;

        else
          TransparentColor = (TColor)ColorToRGB(TransparentColor);

        StretchBltTransparent(Dest->Handle, DstX, DstY, DstW, DstH,
          Temp->Canvas->Handle, SrcX, SrcY, SrcW, SrcH, Temp->Palette,
          TransparentColor );
        }
      }
    catch (...)
      {
      throw;
      }
    }
//    finally
//{$IFDEF RX_D3}
//      {if Bitmap.HandleType = bmDIB then Temp.Free;}
//{$ENDIF}
//    end;
  __finally
    {
    Bitmap->Canvas->OnChanging = CanvasChanging;
    }



}
//---------------------------------------------------------------------------
void __fastcall StretchBitmapTransparentMask( TCanvas *Dest, Graphics::TBitmap *Bitmap,
  Graphics::TBitmap *BitmapMask, int DstX, int DstY, int DstW, int DstH, int SrcX, int SrcY,
  int SrcW, int SrcH )
{
  TNotifyEvent CanvasChanging;
  Graphics::TBitmap *Temp;
  Graphics::TBitmap *TempMask;

  if ( DstW <= 0 )
    DstW = Bitmap->Width;
    
  if ( DstH <= 0 )
    DstH = Bitmap->Height;

  if ((SrcW <= 0) || (SrcH <= 0))
    {
    SrcX = 0;
    SrcY = 0;
    SrcW = Bitmap->Width;
    SrcH = Bitmap->Height;
    }

  if ( ! Bitmap->Monochrome )
    SetStretchBltMode ( Dest->Handle, STRETCH_DELETESCANS);

  CanvasChanging = Bitmap->Canvas->OnChanging;
  try
    {
    Bitmap->Canvas->OnChanging = NULL;
//{$IFDEF RX_D3}
/*
    {if Bitmap.HandleType = bmDIB then begin
      Temp = TBitmap.Create;
      Temp.Assign(Bitmap);
      Temp.HandleType = bmDDB;
    end
    else}
*/
    Temp = Bitmap;
    TempMask = BitmapMask;
//{$ELSE}
//    Temp = Bitmap;
//{$ENDIF}
    try
      {
// Transparency >>>> 
      StretchBltTransparentMask(Dest->Handle, DstX, DstY, DstW, DstH,
          Temp->Canvas->Handle, SrcX, SrcY, SrcW, SrcH, Temp->Palette,
          TempMask->Canvas->Handle, TempMask->Palette );
// <<<< Transparency

/* Transparency
      if ( TransparentColor == clNone )
        {
        StretchBlt( Dest->Handle, DstX, DstY, DstW, DstH, Temp->Canvas->Handle,
          SrcX, SrcY, SrcW, SrcH, Dest->CopyMode);
        }

      else
        {
//{$IFDEF RX_D3}

        if ( TransparentColor == clDefault )
          TransparentColor = Temp->Canvas->Pixels[ 0 ] [Temp->Height - 1];

//{$ENDIF}
        if ( Temp->Monochrome )
          TransparentColor = clWhite;

        else
          TransparentColor = (TColor)ColorToRGB(TransparentColor);

        StretchBltTransparent(Dest->Handle, DstX, DstY, DstW, DstH,
          Temp->Canvas->Handle, SrcX, SrcY, SrcW, SrcH, Temp->Palette,
          TransparentColor );
        }
*/
      }
    catch (...)
      {
      throw;
      }
    }
//    finally
//{$IFDEF RX_D3}
//      {if Bitmap.HandleType = bmDIB then Temp.Free;}
//{$ENDIF}
//    end;
  __finally
    {
    Bitmap->Canvas->OnChanging = CanvasChanging;
    }



}
//---------------------------------------------------------------------------
void __fastcall StretchBitmapRectTransparentMask( TCanvas *Dest, int DstX, int DstY,
  int DstW, int DstH, TRect SrcRect, Graphics::TBitmap *Bitmap, Graphics::TBitmap *MaskBitmap )
{
//  with SrcRect do
    StretchBitmapTransparentMask( Dest, Bitmap, MaskBitmap,
    DstX, DstY, DstW, DstH, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top);
}
//---------------------------------------------------------------------------
void __fastcall StretchBitmapRectTransparent( TCanvas *Dest, int DstX, int DstY,
  int DstW, int DstH, TRect SrcRect, Graphics::TBitmap *Bitmap, TColor TrClolor )
{
//  with SrcRect do
    StretchBitmapTransparent( Dest, Bitmap, TrClolor,
    DstX, DstY, DstW, DstH, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top);
}
//---------------------------------------------------------------------------
void __fastcall FillRGBPalette( TBMGIFColorTable const ColorTable, TRGBPalette &Colors )
{
  int I;

//  FillChar(Colors, sizeof(Colors), $80);
  memset ( Colors, 0x80, sizeof(Colors) );
  for ( I = 0; I < ColorTable.Count; I ++ )
    {
    Colors[I].rgbRed = ColorTable.Colors[I].Red;
    Colors[I].rgbGreen = ColorTable.Colors[I].Green;
    Colors[I].rgbBlue = ColorTable.Colors[I].Blue;
    Colors[I].rgbReserved = 0;
//    if ( ! Colors[I].rgbRed && ! Colors[I].rgbGreen &&  !Colors[I].rgbBlue )
//      Colors[I].rgbRed = 2;

    }
    
/*
  I --;
  Colors[I].rgbRed ++;
  Colors[I].rgbGreen ++;
  Colors[I].rgbBlue ++;
*/
}
//---------------------------------------------------------------------------
void __fastcall FillMaskRGBPalette( TBMGIFColorTable const ColorTable, TRGBPalette &Colors, int FTransIndex )
{
  int I;

//  FillChar(Colors, sizeof(Colors), $80);
  memset ( Colors, 0x80, sizeof(Colors) );
  for ( I = 0; I < ColorTable.Count; I ++ )
    {
    Byte ColorValue = ( FTransIndex == I ) ? (Byte)0xFF : (Byte)0;
//    Byte ColorValue = ( FTransIndex == I ) ? (Byte)0 : (Byte)0xFF;

    Colors[I].rgbRed = ColorValue;
    Colors[I].rgbGreen = ColorValue;
    Colors[I].rgbBlue = ColorValue;
/*
    Colors[I].rgbRed = ColorTable.Colors[I].Red;
    Colors[I].rgbGreen = ColorTable.Colors[I].Green;
    Colors[I].rgbBlue = ColorTable.Colors[I].Blue;
*/
    Colors[I].rgbReserved = 0;
    }

}
//---------------------------------------------------------------------------
//TDictTable = array[0..CODE_TABLE_SIZE - 1] of TImageDict;

struct TImageDict
{
  Word Tail, Index;
  Byte Col;
};
  
typedef TImageDict *PImageDict;


struct TReadContext
{
  long int Inx, Size;
  Byte Buf [255 + 4];
  long int CodeSize;
  long int ReadMask;
};

typedef TReadContext *PReadContext;

struct TWriteContext
{
  long int Inx;
  long int CodeSize;
  Byte Buf [255 + 4];
};

struct TOutputContext
{
  long int W, H, X, Y;
  long int BitsPerPixel, Pass;
  bool Interlace;
  long int LineIdent;
  Pointer Data, CurrLineData;
};


typedef TImageDict TDictTable [CODE_TABLE_SIZE];
typedef  TDictTable *PDictTable;

typedef  void __fastcall (__closure *TProgressProc)(Graphics::TProgressStage Stage, Byte PercentDone, const System::AnsiString Msg );
//---------------------------------------------------------------------------
void * __fastcall HugeOffset(void * HugePtr, int Amount)
{
  return PChar(HugePtr) + Amount;
}
//---------------------------------------------------------------------------
long int InitHash( long int P )
{
  return (P + 3) * 301;
}
//---------------------------------------------------------------------------
void __fastcall WriteCode( TStream *Stream, long int Code, TWriteContext &Context )
{
  long int BufIndex;
  Byte Bytes;

  BufIndex = Context.Inx >> 3;
  Code = Code << (Context.Inx & 7);
  Context.Buf[BufIndex] = (Byte)( Context.Buf[BufIndex] | (Code));
  Context.Buf[BufIndex + 1] = (Byte)(Code >> 8);
  Context.Buf[BufIndex + 2] = (Byte)(Code >> 16);
  Context.Inx = Context.Inx + Context.CodeSize;
  if ( Context.Inx >= 255 * 8 )
    {
//    { Flush out full buffer }
    Bytes = 255;
    Stream->WriteBuffer(&Bytes, 1);
    Stream->WriteBuffer(Context.Buf, Bytes);
    Move(Context.Buf + 255, Context.Buf, 2);
//    FillChar(Context.Buf + 2, 255, 0);
    memset ( Context.Buf + 2, 0, 255 );
    Context.Inx = Context.Inx - (255 * 8);
    }
}
//---------------------------------------------------------------------------
void __fastcall FlushCode( TStream *Stream, TWriteContext &Context )
{
  Byte Bytes;

  Bytes = (Byte)((Context.Inx + 7) >> 3 );
  if ( Bytes > 0 )
    {
    Stream->WriteBuffer(&Bytes, 1);
    Stream->WriteBuffer(Context.Buf, Bytes);
    }
    
//  { Data block terminator - a block of zero Size }
  Bytes = 0;
  Stream->WriteBuffer(&Bytes, 1);
}
//---------------------------------------------------------------------------
int InterlaceStep( int Y, int Height, long int &Pass )
{
  int Result = Y;
/*
  case Pass of
    0, 1: Inc(Result, 8);
    2: Inc(Result, 4);
    3: Inc(Result, 2);
  end;
*/
  switch ( Pass )
    {
    case 0:
    case 1:
      Result += 8;
      break;
      
    case 2:
      Result += 4;
      break;
      
    case 3:
      Result += 2;
      break;
    }
    
  if ( Result >= Height )
    {
    if ( Pass == 0 )
      {
      Pass = 1;
      Result = 4;
      if (Result < Height)
        return Result;
      }

    if ( Pass == 1 )
      {
      Pass = 2;
      Result = 2;
      if (Result < Height)
        return Result;
        
      }
      
    if ( Pass == 2 )
      {
      Pass = 3;
      Result = 1;
      }
    }

  return Result;
}
//---------------------------------------------------------------------------
long int ReadCode( TStream *Stream, TReadContext &Context )
{
  long int RawCode;
  long int ByteIndex;
  Byte Bytes;
  long int BytesToLose;

  while ((Context.Inx + Context.CodeSize > Context.Size) &&
    (Stream->Position < Stream->Size))
  {
//    { not enough bits in buffer - refill it }
//    { Not very efficient, but infrequently called }
    BytesToLose = Context.Inx >> 3;
//    { Note biggest Code Size is 12 bits. And this can at worst span 3 Bytes }
    Move(Context.Buf + (Word)BytesToLose, Context.Buf, 3);
    Context.Inx &= 7;
    Context.Size = Context.Size - (BytesToLose << 3);
    Stream->ReadBuffer(&Bytes, 1);
    if ( Bytes > 0 )
      Stream->ReadBuffer(Context.Buf + ( Word )(Context.Size >> 3), Bytes);
    Context.Size = Context.Size + (Bytes << 3);
    }
    
  ByteIndex = Context.Inx >> 3;
  RawCode = Context.Buf[Word(ByteIndex)] +
    (Word(Context.Buf[Word(ByteIndex + 1)]) << 8);
  if ( Context.CodeSize > 8 )
    RawCode = RawCode + (Longint(Context.Buf[ByteIndex + 2]) << 16);
    
  RawCode = RawCode >> (Context.Inx & 7);
  Context.Inx = Context.Inx + Byte(Context.CodeSize);
  return RawCode & Context.ReadMask;
}
//---------------------------------------------------------------------------
void __fastcall BOutput( Byte Value, TOutputContext &Context )
{
  PByte P;

  if (Context.Y >= Context.H)
    return;

  switch ( Context.BitsPerPixel )
    {
    case 1:
         P = (PByte)HugeOffset(Context.CurrLineData, Context.X >> 3);
         if ((Context.X & 0x07) != 0)
           *P != (Word)(Value << (7 - (Word(Context.X & 7))));

         else
           *P = Byte(Value << 7);
           
       break;

    case 4:
         P = (PByte)HugeOffset(Context.CurrLineData, Context.X >> 1);
         if ( Context.X & 1 )
           *P |= Value;
           
         else
           *P = Byte(Value << 4);
           
       break;

    case 8:
         P = (PByte)HugeOffset(Context.CurrLineData, Context.X);
         *P = Value;
       break;
    }

  Context.X ++;
  if ( Context.X < Context.W )
    return;
    
  Context.X = 0;
  if ( Context.Interlace )
    Context.Y = InterlaceStep(Context.Y, Context.H, Context.Pass);
    
  else
    Context.Y ++;
    
  Context.CurrLineData = HugeOffset(Context.Data,
    (Context.H - 1 - Context.Y) * Context.LineIdent);
}
//---------------------------------------------------------------------------
void __fastcall ReadGIFData( TStream *Stream, TBitmapInfoHeader const Header,
  bool Interlaced, bool LoadCorrupt, Byte IntBitPerPixel, Pointer Data,
  bool &Corrupted, TProgressProc ProgressProc )
{
  Byte MinCodeSize, Temp;
  long int MaxCode, BitMask, InitCodeSize;
  Word ClearCode, EndingCode, FirstFreeCode, FreeCode;
  long int I, OutCount, Code;
  Word CurCode, OldCode, InCode, FinalChar;
  PIntCodeTable Prefix, Suffix, OutCode;
  TReadContext ReadCtxt;
  TOutputContext OutCtxt;
  bool TableFull;

  Corrupted = false;
  OutCount = 0; OldCode = 0; FinalChar = 0;
  TableFull = false;
//  Prefix = (PIntCodeTable)AllocMem(sizeof(TIntCodeTable));
  Prefix = (PIntCodeTable)new Byte [ sizeof(TIntCodeTable) ];
  try
    {
//    Suffix = (PIntCodeTable)AllocMem(sizeof(TIntCodeTable));
    Suffix = (PIntCodeTable)new Byte [ sizeof(TIntCodeTable) ];
    try
      {
//      OutCode = (PIntCodeTable)AllocMem(sizeof(TIntCodeTable) + sizeof(Word));
      OutCode = (PIntCodeTable) new Byte [ sizeof(TIntCodeTable) + sizeof(Word) ];
      try
        {
        if ( ProgressProc )
          ProgressProc(psStarting, 0, "");
          
        try
          {
          Stream->ReadBuffer(&MinCodeSize, 1);
          if ((MinCodeSize < 2) || (MinCodeSize > 9))
            throw EInvalidGraphicOperation ( "Bad GIF Code Size" );
//            GifError(LoadStr(SBadGIFCodeSize));
//          { Initial read context }
          ReadCtxt.Inx = 0;
          ReadCtxt.Size = 0;
          ReadCtxt.CodeSize = MinCodeSize + 1;
          ReadCtxt.ReadMask = (1 << ReadCtxt.CodeSize) - 1;
//          { Initialise pixel-output context }
          OutCtxt.X = 0; OutCtxt.Y = 0;
          OutCtxt.Pass = 0;
          OutCtxt.W = Header.biWidth;
          OutCtxt.H = Header.biHeight;
          OutCtxt.BitsPerPixel = Header.biBitCount;
          OutCtxt.Interlace = Interlaced;
          OutCtxt.LineIdent = ((Header.biWidth * Header.biBitCount + 31)
            / 32) * 4;
          OutCtxt.Data = Data;
          OutCtxt.CurrLineData = HugeOffset(Data, (Header.biHeight - 1) *
            OutCtxt.LineIdent);
          BitMask = (1 << IntBitPerPixel) - 1;
//          { 2 ^ MinCodeSize accounts for all colours in file }
          ClearCode = (Word) ( 1 << MinCodeSize );
          EndingCode = (Word) ( ClearCode + 1 );
          FreeCode = (Word) ( ClearCode + 2 );
          FirstFreeCode = FreeCode;
//          { 2^ (MinCodeSize + 1) includes clear and eoi Code and space too }
          InitCodeSize = ReadCtxt.CodeSize;
          MaxCode = 1 << ReadCtxt.CodeSize;
          Code = ReadCode(Stream, ReadCtxt);
          while ((Code != EndingCode) && (Code != 0xFFFF) &&
            (OutCtxt.Y < OutCtxt.H))
            {
            if (Code == ClearCode)
              {
              ReadCtxt.CodeSize = InitCodeSize;
              MaxCode = 1 << ReadCtxt.CodeSize;
              ReadCtxt.ReadMask = MaxCode - 1;
              FreeCode = FirstFreeCode;
              Code = ReadCode(Stream, ReadCtxt);
              CurCode = (Word)Code;
              OldCode = (Word)Code;
              if (Code == 0xFFFF)
                break;

              FinalChar = (Word)(CurCode & BitMask);
              BOutput( (Byte)FinalChar, OutCtxt );
              TableFull = false;
              }

            else
              {
              CurCode = (Word)Code;
              InCode = (Word)Code;
              if ( CurCode >= FreeCode )
                {
                CurCode = OldCode;
//                (*OutCode)[OutCount] = FinalChar;
                OutCode [OutCount] = FinalChar;
                OutCount++;
                }

              while (CurCode > BitMask)
                {
                if (OutCount > CODE_TABLE_SIZE)
                  {
                  if ( LoadCorrupt )
                    {
                    CurCode = (Word)BitMask;
                    OutCount = 1;
                    Corrupted = true;
                    break;
                    }

                  else
                    throw EInvalidGraphicOperation ( "GIF Decode Error" );
//                    GifError(LoadStr(SGIFDecodeError));
                  }
                  
//                (*OutCode)[OutCount] = (*Suffix)[CurCode];
                OutCode[OutCount] = Suffix[CurCode];
                OutCount ++;
//                CurCode = (*Prefix)[CurCode];
                CurCode = Prefix[CurCode];
                }
                
              if ( Corrupted )
                break;

              FinalChar = (Word)( CurCode & BitMask );
//              (*OutCode)[OutCount] = FinalChar;
              OutCode[OutCount] = FinalChar;
              OutCount ++;
              for ( I = OutCount; I-- ; )
//                BOutput(Byte((*OutCode)[I]), OutCtxt);
                BOutput(Byte(OutCode[I]), OutCtxt);

              OutCount = 0;
//              { Update dictionary }
              if ( ! TableFull )
                {
//                (*Prefix)[FreeCode] = OldCode;
//                (*Suffix)[FreeCode] = FinalChar;
                Prefix[FreeCode] = OldCode;
                Suffix[FreeCode] = FinalChar;
//                { Advance to next free slot }
                FreeCode ++;
                if (FreeCode >= MaxCode)
                  {
                  if (ReadCtxt.CodeSize < 12)
                    {
                    ReadCtxt.CodeSize ++;
                    MaxCode = MaxCode << 1;
                    ReadCtxt.ReadMask = (1 << ReadCtxt.CodeSize) - 1;
                    }
                    
                  else
                    TableFull = true;
                  }
                }
                
              OldCode = InCode;
              }
              
            Code = ReadCode(Stream, ReadCtxt);
            if ( Stream->Size > 0 )
              {
//              Temp = Trunc(100.0 * (Stream->Position / Stream->Size));
              Temp = (Byte)(( 100 * Stream->Position ) / Stream->Size );
              if ( ProgressProc)
                ProgressProc(psRunning, Temp, "" );
              }
            } //{ while }
            
          if ( Code == 0xFFFF )
            throw EInvalidGraphicOperation ( "Read Error" );
//            GifError(ResStr(SReadError));

          }
          
        __finally
          {
          if (ProgressProc)
            {
            if ( ExceptObject () == NULL )
              ProgressProc(psEnding, 100, "" );

            else
              ProgressProc(psEnding, 0, ((Exception *)ExceptObject ())->Message);
            }
          }
        }
      __finally
        {
        delete [] OutCode;
        }
      }
    __finally
      {
      delete [] Suffix;
      }
    }
  __finally
    {
    delete [] Prefix;
    }
//  }
}
//---------------------------------------------------------------------------
// Possible VAR & problem !!!!
void __fastcall ReadImageStream( TStream *Stream, TStream *Dest, TImageDescriptor &Desc,
  bool &Interlaced, bool &LocalColors, bool &Corrupted, Byte &BitsPerPixel,
  TBMGIFColorTable &ColorTable )
{
//  Application->MessageBox("ReadImageStream >>>>", "Debug Box", MB_OK );

  TR( "ReadImageStream" );

  Byte CodeSize, BlockSize;

  Corrupted = false;
//  Stream->ReadBuffer(&Desc, sizeof(TImageDescriptor));
  Stream->ReadBuffer(&Desc, 9);
  Interlaced = ((Desc.PackedFields & ID_INTERLACED) != 0 );
  if ( Desc.PackedFields & ID_LOCAL_COLOR_TABLE )
    {
//    { Local colors table follows }
    BitsPerPixel = (Byte)( 1 + ( Desc.PackedFields & ID_COLOR_TABLE_SIZE ));
    LocalColors = true;
    ColorTable.Count = 1 << BitsPerPixel;
    Stream->ReadBuffer( ColorTable.Colors,
      ColorTable.Count * sizeof(TBMGIFColorItem));
    }
    
  else
    {
    LocalColors = false;
    memset ( &ColorTable, 0, sizeof(ColorTable) );
//    FillChar(ColorTable, sizeof(ColorTable), 0);
    }
    
  Stream->ReadBuffer(&CodeSize, 1);
  Dest->Write(&CodeSize, 1);
  do
    {
    Stream->Read(&BlockSize, 1);
    if ((Stream->Position + BlockSize) > Stream->Size )
      {
      Corrupted = true;
      return; // {!!?}
      }
      
    Dest->Write(&BlockSize, 1);
    if ((Stream->Position + BlockSize) > Stream->Size )
      {
      BlockSize = (Byte)(Stream->Size - Stream->Position);
      Corrupted = true;
      }
      
    if ( BlockSize > 0 )
      Dest->CopyFrom(Stream, BlockSize);
    }

  while ( ! ((BlockSize == 0) || (Stream->Position >= Stream->Size)));
}
//---------------------------------------------------------------------------
void __fastcall WriteGIFData ( TStream *Stream, TBitmapInfoHeader &Header,
  bool Interlaced, Pointer Data, TProgressProc ProgressProc )
{
  // { LZW encode data }
  long int LineIdent;
  Byte MinCodeSize, Col, Temp;
  long int InitCodeSize, X, Y;
  long int Pass;
  long int MaxCode; // { 1 << CodeSize }
  long int ClearCode, EndingCode, LastCode, Tail;
  long int I, HashValue;
  Word LenString;
  PDictTable Dict;
  TList *HashTable;
  PByte PData;
  TWriteContext WriteCtxt;


  LineIdent = ((Header.biWidth * Header.biBitCount + 31) / 32) * 4;
  Tail = 0;
  HashValue = 0;
  Dict = (PDictTable)AllocMem(sizeof(TDictTable));
  try
    {
    HashTable = new TList;
    try
      {
      for ( I = 0; I < HASH_TABLE_SIZE; I ++ )
        HashTable->Add(NULL);
        
//      { Initialise encoder variables }
      InitCodeSize = Header.biBitCount + 1;
      if ( InitCodeSize == 2 )
        InitCodeSize ++;
        
      MinCodeSize = (Byte)(InitCodeSize - 1 );
      Stream->WriteBuffer(&MinCodeSize, 1);
      ClearCode = 1 << MinCodeSize;
      EndingCode = ClearCode + 1;
      LastCode = EndingCode;
      MaxCode = 1 << InitCodeSize;
      LenString = 0;
//      { Setup write context }
      WriteCtxt.Inx = 0;
      WriteCtxt.CodeSize = InitCodeSize;
//      FillChar(WriteCtxt.Buf, sizeof(WriteCtxt.Buf), 0);
        memset ( WriteCtxt.Buf, 0, sizeof(WriteCtxt.Buf) );
      WriteCode(Stream, ClearCode, WriteCtxt);
      for ( I = 0; I < HASH_TABLE_SIZE; I ++ )
        HashTable->Items[I] = NULL;
        
      Data = HugeOffset(Data, (Header.biHeight - 1) * LineIdent);
      Y = 0; Pass = 0;
      if ( ProgressProc )
        ProgressProc(psStarting, 0, "" );

      try
        {
        while (Y < Header.biHeight)
          {
          PData = (PByte)HugeOffset(&Data, -(Y * LineIdent));
          for ( X = 0; X < Header.biWidth; X ++ )
            {
            switch ( Header.biBitCount )
              {
              case 8:
                Col = *PData;
                PData = (PByte)HugeOffset(PData, 1);
                break;
                
              case 4:
                if ( ( X & 1 ) != 0 )
                  {
                  Col = (Byte)(*PData & 0xF );
                  PData = (PByte)HugeOffset(PData, 1);
                  }

                else
                  Col = (Byte)(*PData >> 4);
                  
                break;
                
              default : // { must be 1 }
                {
                if ( ( X & 7 ) == 7 )
                  {
                  Col = (Byte)(*PData & 1 );
                  PData = (PByte)HugeOffset(PData, 1);
                  }
                else
                  Col = (Byte)(( *PData >> (7 - (X & 0x7))) & 0x1);
                }

              }

            LenString ++;
            if ( LenString == 1 )
              {
              Tail = Col;
              HashValue = InitHash(Col);
              }
              
            else
              {
              HashValue = HashValue * (Col + LenString + 4);
              I = HashValue % HASH_TABLE_SIZE;
              HashValue = HashValue % HASH_TABLE_SIZE;
              while ((HashTable->Items[I] != NULL) &&
                ((((PImageDict)HashTable->Items[I])->Tail != Tail) ||
                (((PImageDict)HashTable->Items[I])->Col != Col)))
//                ((PImageDict(HashTable[I])^.Tail != Tail) ||
//                (PImageDict(HashTable[I])^.Col != Col)))
                {
                I++;
                if (I >= HASH_TABLE_SIZE)
                  I = 0;
                  
                }
                
              if (HashTable->Items[I] != NULL ) //{ Found in the strings table }
                Tail = ((PImageDict)HashTable->Items[I])->Index;
                
              else
                {
//                { Not found }
                WriteCode(Stream, Tail, WriteCtxt);
                LastCode ++;
                HashTable->Items[I] = &( ( *Dict )[LastCode] );
                ((PImageDict)HashTable->Items[I])->Index = (Word)LastCode;
                ((PImageDict)HashTable->Items[I])->Tail = (Word)Tail;
                ((PImageDict)HashTable->Items[I])->Col = Col;
                Tail = Col;
                HashValue = InitHash(Col);
                LenString = 1;
                if (LastCode >= MaxCode)
                  {
//                  { Next Code will be written longer }
                  MaxCode = MaxCode << 1;
                  WriteCtxt.CodeSize ++;
                  }
                  
                else if (LastCode >= CODE_TABLE_SIZE - 2)
                  {
//                  { Reset tables }
                  WriteCode(Stream, Tail, WriteCtxt);
                  WriteCode(Stream, ClearCode, WriteCtxt);
                  LenString = 0;
                  LastCode = EndingCode;
                  WriteCtxt.CodeSize = InitCodeSize;
                  MaxCode = 1 << InitCodeSize;
                  for ( I = 0; I < HASH_TABLE_SIZE; I++ )
                    HashTable->Items[I] = NULL;
                  }
                }
              }
            }// { for X loop }
          if ( Interlaced )
            Y = InterlaceStep(Y, Header.biHeight, Pass);
            
          else
            Y ++;
            
//          Temp = Trunc(100.0 * (Y / Header.biHeight));
          Temp = (Byte)(( 100 * Y ) / Header.biHeight );
          if ( ProgressProc )
            ProgressProc(psRunning, Temp, "" );
            
          } // { while Y loop }
        WriteCode(Stream, Tail, WriteCtxt);
        WriteCode(Stream, EndingCode, WriteCtxt);
        FlushCode(Stream, WriteCtxt);
        }
      __finally
        {
        if (ProgressProc)
          {
          if ( ! ExceptObject () )
            ProgressProc(psEnding, 100, "");
            
          else
            ProgressProc(psEnding, 0, ((Exception *)ExceptObject ())->Message);
          }
        }
      }
    __finally
      {
      if ( HashTable )
        delete HashTable;
      }
    }
  __finally
    {
    SysFreeMem ( Dict );
//    FreeMem(Dict, sizeof(TDictTable));
    }
}

//---------------------------------------------------------------------------
void __fastcall FillColorTable( TBMGIFColorTable &ColorTable, TRGBPalette const Colors, int Count )
{
  int I;

//  FillChar(ColorTable, sizeof(ColorTable), 0);
  memset ( &ColorTable, 0, sizeof(ColorTable) );

  ColorTable.Count = min(256, Count);
  for ( I = 0; I < ColorTable.Count; I ++ )
    {
    ColorTable.Colors[I].Red = Colors[I].rgbRed;
    ColorTable.Colors[I].Green = Colors[I].rgbGreen;
    ColorTable.Colors[I].Blue = Colors[I].rgbBlue;
    }
}
//---------------------------------------------------------------------------
TColor GrayColor( TColor Color )
{
  int Index;
  
  Index = Byte(Longint(Word(GetRValue(Color)) * 77 +
    Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) >> 8);
    
  return (TColor)RGB(Index, Index, Index);
}
//---------------------------------------------------------------------------
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMGIFItem::~TBMGIFItem ()
{
  if ( FImageData )
    delete FImageData;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFItem::FreeHandle ()
{
  if ( FImageData )
    FImageData->SetSize(0);
}
//---------------------------------------------------------------------------
__fastcall TBMGIFData::TBMGIFData ()
{
  FComment = new TStringList;
}
//---------------------------------------------------------------------------
__fastcall TBMGIFData::~TBMGIFData ()
{
  delete FComment;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFData::FreeHandle ()
{
  if ( FComment )
    FComment->Clear ();
}
//---------------------------------------------------------------------------
__fastcall TBMGIFFrame::TBMGIFFrame ( TBMGIFImage *AOwner )
{
  CRITICAL ( CriticalSection );
  {
  LocalCritical = new TCriticalSection;
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::TBMGIFFrame" );
  FBitmap = NULL;
  FMaskBitmap = NULL;

  FFullBitmap = NULL;
  FFullMaskBitmap = NULL;

  FExtensions = NULL;
  FOwner = AOwner;
  NewImage ();
  }
}
//---------------------------------------------------------------------------
__fastcall TBMGIFFrame::~TBMGIFFrame ()
{
  CRITICAL ( CriticalSection );
  TR( "TBMGIFFrame::~TBMGIFFrame" );

  {
  CRITICAL ( LocalCritical );

  if ( FBitmap )
    delete FBitmap;

  if ( FMaskBitmap )
    delete FMaskBitmap;

  if ( FFullBitmap )
    delete FFullBitmap;

  if ( FFullMaskBitmap )
    delete FFullMaskBitmap;

  FreeExtensions ( FExtensions );

  FImage->Release ();
  }
  
  delete LocalCritical;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::SetAnimateInterval( Word Value )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::SetAnimateInterval" );

  if ( FAnimateInterval != Value )
    {
    FAnimateInterval = Value;
    if ( Value > 0 )
      FOwner->FVersion = TBMGIFData::gv89a;

    FOwner->Changed ( FOwner );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::SetDisposalMethod( TBMGIFData::TDisposalMethod Value )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::SetDisposalMethod" );

  if ( FDisposal != Value )
    {
    FDisposal = Value;
    if ( Value != TBMGIFData::dmUndefined )
      FOwner->FVersion = TBMGIFData::gv89a;

    FOwner->Changed(FOwner);
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::SetTopLeft (const tagPOINT &Value)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::SetTopLeft" );

  if (( FTopLeft.x != Value.x ) || (FTopLeft.y != Value.y ))
    {
    FTopLeft.x = Value.x;
    FTopLeft.y = Value.y;
    FOwner->FScreenWidth = (Word) max((long)FOwner->FScreenWidth,
      FImage->FSize.x + FTopLeft.x);
    FOwner->FScreenHeight = (Word) max((long)FOwner->FScreenHeight,
      FImage->FSize.y + FTopLeft.y);
    FOwner->Changed(FOwner);
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::ExpandBitmaps ()
{
  CRITICAL ( LocalCritical );
  try
    {
    TR( "TBMGIFFrame::ExpandBitmaps" );

    TMemoryStream *Mem = NULL;
    TMemoryStream *MaskMem = NULL;

    if ( !FBitmap || FBitmap->Empty )
      {
      NewBitmaps ();
      if ( FImage->FImageData )
        {
        try
          {
          Mem = new TMemoryStream;
          MaskMem = new TMemoryStream;
          try
            {
            SaveToBitmapStream(Mem, MaskMem);
            FBitmap->LoadFromStream(Mem);
            FMaskBitmap->LoadFromStream(MaskMem);
            FBitmap->HandleType = bmDDB;
            FMaskBitmap->HandleType = bmDDB;

//          FBitmap->SaveToFile ( "C:\\Bitmap.bmp" );
//          FMaskBitmap->SaveToFile ( "C:\\MaskBitmap.bmp" );
            }

          __finally
            {
            delete Mem;
            delete MaskMem;
            }
          }
        catch ( EAbort * )
          {
          }

        catch (...)
          {
          throw;
          }

//    __except ( EAbort ) {} /* OnProgress can raise EAbort to cancel image load */
//      else throw;
        }
      }
    }
    
  __finally
    {
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::ExpandFullBitmaps ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::ExpandFullBitmaps" );
  FOwner->GetFrameBitmaps( this, FFullBitmap, FFullMaskBitmap );
}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TBMGIFFrame::GetMaskBitmap ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetMaskBitmap" );

  if ( ! FMaskBitmap )
    ExpandBitmaps ();

  return FMaskBitmap;
}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TBMGIFFrame::GetBitmap ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetBitmap" );

  if ( ! FBitmap )
    ExpandBitmaps ();

  return FBitmap;
}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TBMGIFFrame::GetFullMaskBitmap ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetFullMaskBitmap" );

  if ( ! FFullMaskBitmap )
    ExpandFullBitmaps ();

  return FFullMaskBitmap;
}
//---------------------------------------------------------------------------
Graphics::TBitmap *__fastcall TBMGIFFrame::GetFullBitmap ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetFullBitmap" );

  if ( ! FFullBitmap )
    ExpandFullBitmaps ();

  return FFullBitmap;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFFrame::GetHeight ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetHeight" );

  if ( ! FBitmap )
    ExpandBitmaps ();

  if ( FBitmap && FImage->FImageData )
    return FBitmap->Height;

  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFFrame::GetWidth ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetWidth" );

  if ( ! FBitmap )
    ExpandBitmaps ();

  if ( FBitmap && FImage->FImageData )
    return FBitmap->Width;

  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFFrame::GetColorCount ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetColorCount" );

  int Result = FImage->FColorMap.Count;
  if ( (Result == 0) && FBitmap && FBitmap->Palette )
    return PaletteEntries ( FBitmap->Palette );

  return Result;
}
//---------------------------------------------------------------------------
#if 0  // Just for test !!!!

TMemoryStream* BitmapToMemoryStream ( Graphics::TBitmap *ABitmap, Graphics::TPixelFormat PixelFormat, TMappingMethod Method );
Graphics::TPixelFormat __fastcall GetBitmapPixelFormat(Graphics::TBitmap* Bitmap);
#endif

//---------------------------------------------------------------------------
enum TMappingMethod { mmHistogram, mmQuantize, mmTrunc784, mmTrunc666, mmTripel, mmGrayscale };
const TMappingMethod DefaultMappingMethod = mmHistogram;


Graphics::TPixelFormat __fastcall GetBitmapPixelFormat(Graphics::TBitmap* Bitmap)
{
  return Bitmap->PixelFormat;
}
//---------------------------------------------------------------------------
void __fastcall InitializeBitmapInfoHeader( HBITMAP Bitmap, TBitmapInfoHeader &BI,
  TPixelFormat PixelFormat )
{
  TR( "InitializeBitmapInfoHeader" );

  Windows::TBitmap BM;

  GetObject(Bitmap, sizeof(BM), &BM);
//  with BI do begin
    BI.biSize = sizeof(BI);
    BI.biWidth = BM.bmWidth;
    BI.biHeight = BM.bmHeight;
    switch ( PixelFormat )
      {
      case pf1bit:
        BI.biBitCount = 1;
        break;

      case pf4bit:
        BI.biBitCount = 4;
        break;

      case pf8bit:
        BI.biBitCount = 8;
        break;

      case pf24bit:
        BI.biBitCount = 24;
        break;

      default :
        BI.biBitCount = (Word)(BM.bmBitsPixel * BM.bmPlanes);
      }

    BI.biPlanes = 1;
    BI.biXPelsPerMeter = 0;
    BI.biYPelsPerMeter = 0;
    BI.biClrUsed = 0;
    BI.biClrImportant = 0;
    BI.biCompression = BI_RGB;
    if ( BI.biBitCount >= 9 && BI.biBitCount <= 32 )
      BI.biBitCount = 24;
      
    BI.biSizeImage = (((BI.biWidth * BI.biBitCount + 31) / 32) * 4) * BI.biHeight;
//  end;
}
//---------------------------------------------------------------------------
void __fastcall InternalGetDIBSizes( HBITMAP Bitmap, int &InfoHeaderSize,
  long int &ImageSize, TPixelFormat BitCount )
{
  TBitmapInfoHeader BI;

  InitializeBitmapInfoHeader(Bitmap, BI, BitCount);
//  with BI do begin
    switch ( BI.biBitCount )
      {
      case 24:
        InfoHeaderSize = sizeof(TBitmapInfoHeader);
        break;
        
      default :
        InfoHeaderSize = sizeof(TBitmapInfoHeader) + sizeof(TRGBQuad) *
        (1 << BI.biBitCount);
      }
      
  ImageSize = BI.biSizeImage;
}
//---------------------------------------------------------------------------
bool __fastcall InternalGetDIB ( HBITMAP Bitmap, HPALETTE Palette,
  void *BitmapInfo, void *Bits, TPixelFormat PixelFormat )
{
  HPALETTE OldPal;
  HWND Focus;
  HDC DC;

//  InitializeBitmapInfoHeader(Bitmap, ( TBitmapInfoHeader )BitmapInfo, PixelFormat);
  InitializeBitmapInfoHeader(Bitmap, *( (TBitmapInfoHeader *)BitmapInfo), PixelFormat);
  OldPal = 0;
  Focus = GetFocus;
  DC = GetDC(Focus);
  
  bool Result;
  
  try
    {
    if ( Palette )
      {
      OldPal = SelectPalette(DC, Palette, False);
      RealizePalette(DC);
      }
      
    Result = GetDIBits(DC, Bitmap, 0, ((TBitmapInfoHeader *)BitmapInfo)->biHeight,
      &Bits, ((TBitmapInfo*)BitmapInfo), DIB_RGB_COLORS) != 0;
    }
    
  __finally
    {
    if ( OldPal != 0 )
      SelectPalette(DC, OldPal, False);

    ReleaseDC(Focus, DC);
    }

  return Result;
}
//---------------------------------------------------------------------------
Pointer __fastcall DIBFromBit( HBITMAP Src, HPALETTE Pal, TPixelFormat PixelFormat, long int &Length )
{
  int HeaderSize;
  long int ImageSize;
  PBitmapFileHeader FileHeader;
  PBitmapInfoHeader BI;
  Pointer Bits;
 
  if ( Src == 0 )
    throw new EInvalidGraphic ( "Invalid Bitmap !" );
    
  InternalGetDIBSizes(Src, HeaderSize, ImageSize, PixelFormat);
  Length = sizeof(TBitmapFileHeader) + HeaderSize + ImageSize;
//  Pointer Result = AllocMemo(Length);
  Pointer Result = new char [ Length ];
  try
    {
//    FillChar(Result, Length, 0);
    memset(Result, 0, Length);
    FileHeader = (PBitmapFileHeader)Result;
//    with FileHeader^ do
//    begin
      FileHeader->bfType = 0x4D42;
      FileHeader->bfSize = Length;
      FileHeader->bfOffBits = sizeof(*FileHeader) + HeaderSize;
//    end;
    BI = PBitmapInfoHeader(Longint(FileHeader) + sizeof(*FileHeader));
    Bits = Pointer(Longint(BI) + HeaderSize);
    InternalGetDIB(Src, Pal, BI, Bits, PixelFormat);
    }
  catch (...)
    {
    delete [] Result;
    throw;
    }

  return Result;  
}
//---------------------------------------------------------------------------
struct TQColor
{
  Byte RGB [ 3 ];
  Byte NewColorIndex;
  long int Count;
  TQColor *PNext;
};

typedef TQColor *PQColor;

struct TNewColor
{
  Byte RGBMin [ 3 ], RGBWidth [ 3 ];
  long int NumEntries;
  long int Count;
  TQColor *QuantizedColors;
};

typedef TNewColor TNewColorArray [255];
typedef TQColor TQColorArray [MAX_COLORS];

typedef TQColorArray *PQColorArray;
typedef TNewColorArray *PNewColorArray;

//---------------------------------------------------------------------------
long int WidthBytes( long int I )
{
  return ((I + 31) / 32) * 4;
}
//---------------------------------------------------------------------------
void __fastcall HMemCpy( Pointer DstPtr, Pointer SrcPtr, long int Amount )
{
  Move( SrcPtr, DstPtr, Amount);
}
//---------------------------------------------------------------------------
typedef PQColor TQColorList [ MaxListSize];
typedef TQColorList *PQColorList;
//---------------------------------------------------------------------------
void __fastcall PInsert( PQColorList ColorList, int Number, int SortRGBAxis )
{
  PQColor Q1, Q2;
  int I, J;
  PQColor Temp;

  for ( I = 1; I < Number; I ++ )
    {
    Temp = (*ColorList)[I];
    J = I - 1;
    while (J >= 0)
      {
      Q1 = Temp;
      Q2 = (*ColorList)[J];
      if (Q1->RGB[SortRGBAxis] - Q2->RGB[SortRGBAxis] > 0)
        break;
        
      (*ColorList)[J + 1] = (*ColorList)[J];
      J--;
      }
      
    (*ColorList)[J + 1] = Temp;
    }

}
//---------------------------------------------------------------------------
void __fastcall PSort( PQColorList ColorList, int Number, int SortRGBAxis )
{
  PQColor Q1, Q2;
  int I, J, N, Nr;
  PQColor Temp, Part;

  if ( Number < 8 )
    {
    PInsert(ColorList, Number, SortRGBAxis);
    return;
    }

  Part = (*ColorList)[Number / 2];
  I = -1;
  J = Number;
  for (;;)
    {
    do
      {
//  repeat
//    repeat

      I++;
      Q1 = (*ColorList)[I];
      Q2 = Part;
      N = Q1->RGB[SortRGBAxis] - Q2->RGB[SortRGBAxis];
      }
    while (!(N >= 0));
    do
      {
      J--;
      Q1 = (*ColorList)[J];
      Q2 = Part;
      N = Q1->RGB[SortRGBAxis] - Q2->RGB[SortRGBAxis];
      }
    while (!(N <= 0));
    if (I >= J)
      break;

    Temp = (*ColorList)[I];
    (*ColorList)[I] = (*ColorList)[J];
    (*ColorList)[J] = Temp;
    }

  Nr = Number - I;
  if (I < Number / 2)
    {
    PSort(ColorList, I, SortRGBAxis);
    PSort(PQColorList(ColorList + I), Nr, SortRGBAxis);
    }
    
  else
    {
    PSort(PQColorList(ColorList + I), Nr, SortRGBAxis);
    PSort(ColorList, I, SortRGBAxis);
    }
    
}
//---------------------------------------------------------------------------
int DivideMap( PNewColorArray NewColorSubdiv, int ColorMapSize,
  int &NewColormapSize, Pointer lpStr )
{
  int I, J;
  int MaxSize, Index;
  int NumEntries, MinColor;
  int MaxColor;
  long int Sum, Count;
  PQColor QuantizedColor;
  PQColorList SortArray;
  int SortRGBAxis;

  Index = 0; SortRGBAxis = 0;
  while (ColorMapSize > NewColormapSize)
    {
    MaxSize = -1;
    for ( I = 0; I < NewColormapSize; I ++ )
      {
      for ( J = 0; J <= 2; J ++ )
        {
        if (((*NewColorSubdiv)[I].RGBWidth[J] > MaxSize) &&
          ((*NewColorSubdiv)[I].NumEntries > 1))
          {
          MaxSize = (*NewColorSubdiv)[I].RGBWidth[J];
          Index = I;
          SortRGBAxis = J;
          }
        }
      }
    if (MaxSize == -1)
      return 1;
      
    SortArray = PQColorList(lpStr);
    J = 0;
    QuantizedColor = (*NewColorSubdiv)[Index].QuantizedColors;
    while ((J < (*NewColorSubdiv)[Index].NumEntries) &&
      (QuantizedColor != NULL ))
      {
      ( *SortArray)[J] = QuantizedColor;
      J ++;
      QuantizedColor = QuantizedColor->PNext;
      }
      
    PSort(SortArray, (*NewColorSubdiv)[Index].NumEntries, SortRGBAxis);
    for ( J = 0; J <= (*NewColorSubdiv)[Index].NumEntries - 2; J ++ )
      (*SortArray)[J]->PNext = (*SortArray)[J + 1];
    (*SortArray)[(*NewColorSubdiv)[Index].NumEntries - 1]->PNext = NULL;
    (*NewColorSubdiv)[Index].QuantizedColors = (*SortArray)[0];
    QuantizedColor = (*SortArray)[0];
    Sum = (*NewColorSubdiv)[Index].Count / 2 - QuantizedColor->Count;
    NumEntries = 1;
    Count = QuantizedColor->Count;
    Sum -= QuantizedColor->PNext->Count;
    while ((Sum >= 0) && (QuantizedColor->PNext) &&
      (QuantizedColor->PNext->PNext))
      {
      QuantizedColor = QuantizedColor->PNext;
      NumEntries++;
      Count, QuantizedColor->Count++;
      Sum, QuantizedColor->PNext->Count--;
      }
      
    MaxColor = (QuantizedColor->RGB[SortRGBAxis]) << 4;
    MinColor = (QuantizedColor->PNext->RGB[SortRGBAxis]) << 4;
    (*NewColorSubdiv)[NewColormapSize].QuantizedColors = QuantizedColor->PNext;
    QuantizedColor->PNext = NULL;
    (*NewColorSubdiv)[NewColormapSize].Count = Count;
    (*NewColorSubdiv)[Index].Count -= Count;
    (*NewColorSubdiv)[NewColormapSize].NumEntries =
      (*NewColorSubdiv)[Index].NumEntries - NumEntries;
    (*NewColorSubdiv)[Index].NumEntries = NumEntries;
    for ( J = 0; J <= 2; J ++ )
      {
      (*NewColorSubdiv)[NewColormapSize].RGBMin[J] =
        (*NewColorSubdiv)[Index].RGBMin[J];
      (*NewColorSubdiv)[NewColormapSize].RGBWidth[J] =
        (*NewColorSubdiv)[Index].RGBWidth[J];
      }
      
    (*NewColorSubdiv)[NewColormapSize].RGBWidth[SortRGBAxis] = (Byte)
      ((*NewColorSubdiv)[NewColormapSize].RGBMin[SortRGBAxis] +
      (*NewColorSubdiv)[NewColormapSize].RGBWidth[SortRGBAxis] -
      MinColor);
    (*NewColorSubdiv)[NewColormapSize].RGBMin[SortRGBAxis] = (Byte)MinColor;
    (*NewColorSubdiv)[Index].RGBWidth[SortRGBAxis] = (Byte)
      (MaxColor - (*NewColorSubdiv)[Index].RGBMin[SortRGBAxis]);
    NewColormapSize ++;
    }
    
  return 1;
}
//---------------------------------------------------------------------------
void Quantize(const TBitmapInfoHeader bmp, Pointer gptr, Pointer Data8,
  int &ColorCount, TRGBPalette &OutputColormap )
{
//  PWord = ^Word;

  PByteArray P;
  Pointer LineBuffer, Data;
  long int LineWidth;
  long int TmpLineWidth, NewLineWidth;
  long int I, J;
  Word Index;
  int NewColormapSize, NumOfEntries;
  long int Mems;
  long int cRed, cGreen, cBlue;
  Pointer lpStr, Temp, Tmp;
  TNewColorArray *NewColorSubdiv;
  PQColorArray ColorArrayEntries;
  PQColor QuantizedColor;
 
  LineWidth = WidthBytes(Longint(bmp.biWidth) * bmp.biBitCount);
  Mems = (Longint(sizeof(TQColor)) * (MAX_COLORS)) +
    (Longint(sizeof(TNewColor)) * 256) + LineWidth +
    (Longint(sizeof(PQColor)) * (MAX_COLORS));
  lpStr = new char [ Mems ];
  try
    {
    Temp = new char [ Longint(bmp.biWidth) * Longint(bmp.biHeight) *
      sizeof(Word) ];
    try
      {
      ColorArrayEntries = PQColorArray(lpStr);
      NewColorSubdiv = PNewColorArray(HugeOffset(lpStr,
        Longint(sizeof(TQColor)) * (MAX_COLORS)));
      LineBuffer = HugeOffset(lpStr, (Longint(sizeof(TQColor)) * (MAX_COLORS)) +
        (Longint(sizeof(TNewColor)) * 256));
      for ( I = 0; I < MAX_COLORS; I ++ )
        {
        (*ColorArrayEntries)[I].RGB[0] = (Byte)( I >> 8 );
        (*ColorArrayEntries)[I].RGB[1] = (Byte)((I >> 4) & 0x0F );
        (*ColorArrayEntries)[I].RGB[2] = (Byte)(I & 0x0F);
        (*ColorArrayEntries)[I].Count = 0;
        }
        
      Tmp = Temp;
      for ( I = 0; I < bmp.biHeight; I ++ )
        {
        HMemCpy(LineBuffer, HugeOffset(gptr, (bmp.biHeight - 1 - I) *
          LineWidth), LineWidth);
        P = (PByteArray)LineBuffer;
        for ( J = 0 ; J < bmp.biWidth; J ++ )
          {
          Index = (Word)(( (long int)((*P)[2] & 0xF0) << 4) +
            Longint((*P)[1] & 0xF0) + ((long int)((*P)[0] & 0xF0) >> 4));
          (*ColorArrayEntries)[Index].Count ++;
          P = (PByteArray)HugeOffset(P, 3);
          *((Word *)Tmp) = Index;
          Tmp = HugeOffset(Tmp, 2);
          }
        }
      for ( I = 0; I <= 255; I ++ )
        {
        ( *NewColorSubdiv )[I].QuantizedColors = NULL;
        ( *NewColorSubdiv )[I].Count = 0;
        ( *NewColorSubdiv )[I].NumEntries = 0;
        for ( J = 0; J < 2; J ++ )
          {
          ( *NewColorSubdiv )[I].RGBMin[J] = 0;
          ( *NewColorSubdiv )[I].RGBWidth[J] = 255;
          }
        }
        
      I = 0;
      while ( I < MAX_COLORS )
        {
        if ( ( *ColorArrayEntries )[I].Count > 0 )
          break;

        I ++;
        }
        
      QuantizedColor = & (( *ColorArrayEntries ) [I]);
      (*NewColorSubdiv)[0].QuantizedColors = & (( *ColorArrayEntries)[I]);
      NumOfEntries = 1;
      I ++;
      while ( I < MAX_COLORS )
        {
        if ( ( *ColorArrayEntries)[I].Count > 0 )
          {
          QuantizedColor->PNext = & (( *ColorArrayEntries)[I]);
          QuantizedColor = & (( *ColorArrayEntries)[I]);
          NumOfEntries ++;
          }
          
        I ++;
        }
        
      QuantizedColor->PNext = NULL;
      (*NewColorSubdiv)[0].NumEntries = NumOfEntries;
      (*NewColorSubdiv)[0].Count = ( long int )(bmp.biWidth) * ( long int )(bmp.biHeight);
      NewColormapSize = 1;
      DivideMap(NewColorSubdiv, ColorCount, NewColormapSize,
        HugeOffset(lpStr, Longint(sizeof(TQColor)) * (MAX_COLORS) +
        Longint(sizeof(TNewColor)) * 256 + LineWidth));
      if (NewColormapSize < ColorCount)
        {
        for ( I = NewColormapSize; I < ColorCount; I ++ )
//          FillChar(OutputColormap[I], sizeof(TRGBQuad), 0);
          memset (OutputColormap + I, 0, sizeof(TRGBQuad) );
        }
        
      for ( I = 0; I < NewColormapSize; I ++ )
        {
        J = ( *NewColorSubdiv)[I].NumEntries;
        if ( J > 0 )
          {
          QuantizedColor = ( *NewColorSubdiv)[I].QuantizedColors;
          cRed = 0;
          cGreen = 0;
          cBlue = 0;
          while (QuantizedColor != NULL)
            {
            QuantizedColor->NewColorIndex = (Byte)I;
            cRed, QuantizedColor->RGB[0] ++;
            cGreen, QuantizedColor->RGB[1] ++;
            cBlue, QuantizedColor->RGB[2] ++;
            QuantizedColor = QuantizedColor->PNext;
            }

//          with OutputColormap[I] do begin
            OutputColormap[I].rgbRed = (Word)((Longint(cRed << 4) | 0x0F) / J);
            OutputColormap[I].rgbGreen = (Word)((Longint(cGreen << 4) | 0x0F) / J );
            OutputColormap[I].rgbBlue = (Word)((Longint(cBlue << 4) | 0x0F) / J );
            OutputColormap[I].rgbReserved = 0;
            if ((OutputColormap[I].rgbRed <= 0x10) && (OutputColormap[I].rgbGreen <= 0x10) && (OutputColormap[I].rgbBlue <= 0x10))
//              FillChar(OutputColormap[I], sizeof(TRGBQuad), 0); { clBlack }
              memset(OutputColormap + I, 0, sizeof(TRGBQuad)); //{ clBlack }
//          end;
          }
        }
        
      TmpLineWidth = Longint(bmp.biWidth) * sizeof(Word);
      NewLineWidth = WidthBytes(Longint(bmp.biWidth) * 8);
      ZeroMemory(Data8, NewLineWidth * bmp.biHeight);
      for ( I = 0; I < bmp.biHeight; I ++ )
        {
        LineBuffer = HugeOffset(Temp, (bmp.biHeight - 1 - I) * TmpLineWidth);
        Data = HugeOffset(Data8, I * NewLineWidth);
        for ( J = 0; J < bmp.biWidth; J ++ )
          {
          *(PByte(Data)) = (*ColorArrayEntries)[* ((Word *)(LineBuffer))].NewColorIndex;
          LineBuffer = HugeOffset(LineBuffer, 2);
          Data = HugeOffset(Data, 1);
          }
        }
      }
    __finally
      {
      delete [] Temp;
      }
    }

  __finally
    {
    delete [] lpStr;
    }

  ColorCount = NewColormapSize;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TruncPal7R8G4B(TRGBPalette &Colors )
{
  int I;
  Byte R, G, B;

//  FillChar(Colors, sizeof(TRGBPalette), $80);
  memset(Colors, 0x80, sizeof(TRGBPalette));
  I = 0;
  for ( R = 0; R <= 6; R ++ )
    for ( G = 0; G <= 7; G ++ )
      for ( B = 0; B <= 3; B ++ )
        {
        Colors[I].rgbRed = Scale07[R];
        Colors[I].rgbGreen = Scale08[G];
        Colors[I].rgbBlue = Scale04[B];
        Colors[I].rgbReserved = 0;
        I ++;
        }

        
}
//---------------------------------------------------------------------------
//{ truncate to 7Rx8Gx4B one line }
void __fastcall TruncLine7R8G4B ( Pointer Src, Pointer Dest, int CX )
{
  int X;
  Byte R, G, B;

  for ( X = 0; X < CX; X++ )
    {
    B = TruncIndex04[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    G = TruncIndex08[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    R = TruncIndex07[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    *((Byte *)Dest) = (Byte)( 4 * (8 * R + G) + B );
    Dest = HugeOffset(Dest, 1);
    }
    
}
//---------------------------------------------------------------------------
typedef void __fastcall   ( *TTruncLine ) ( Pointer Src, Pointer Dest, int CX );
//---------------------------------------------------------------------------

void __fastcall Trunc(const TBitmapInfoHeader Header, Pointer Src, Pointer Dest,
  int DstBitsPerPixel, TTruncLine TruncLineProc )
{
  long int SrcScanline, DstScanline;
  int Y;

  SrcScanline = (Header.biWidth * 3 + 3) & ~ 3;
  DstScanline = ((Header.biWidth * DstBitsPerPixel + 31) / 32) * 4;
  for ( Y = 0; Y < Header.biHeight; Y ++ )
    TruncLineProc(HugeOffset(Src, Y * SrcScanline),
      HugeOffset(Dest, Y * DstScanline), Header.biWidth);
}
//---------------------------------------------------------------------------
//{ truncate to 7Rx8Gx4B }
void __fastcall Trunc7R8G4B ( const TBitmapInfoHeader Header,  const Pointer Data24, Pointer Data8 )
{
  Trunc(Header, Data24, Data8, 8, TruncLine7R8G4B);
}
//---------------------------------------------------------------------------
/* return 6Rx6Gx6B palette
  This function makes the palette for the 6 red X 6 green X 6 blue palette.
  216 palette entrys used. Remaining 40 Left blank.
*/
void __fastcall TruncPal6R6G6B( TRGBPalette &Colors )
{
  Byte R, G, B;
  int I;
 
//  FillChar(Colors, SizeOf(TRGBPalette), $80);
  memset (Colors, 0x80, sizeof(TRGBPalette) );
  I = 0;
  for ( R = 0; R < 5; R ++ )
    for ( G = 0; G < 5; G ++ )
      for ( B = 0; B < 5; B ++ )
        {
        Colors[I].rgbRed = Scale06[R];
        Colors[I].rgbGreen = Scale06[G];
        Colors[I].rgbBlue = Scale06[B];
        Colors[I].rgbReserved = 0;
        I ++;
        }
}
//---------------------------------------------------------------------------
void __fastcall TruncLine6R6G6B( Pointer Src, Pointer Dest, int CX )
{
  int X;
  Byte R, G, B;

  for ( X = 0; X < CX; X ++ )
    {
    B = TruncIndex06[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    G = TruncIndex06[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    R = TruncIndex06[*((Byte*)Src)]; Src = HugeOffset(Src, 1);
    *((Byte*)Dest) = (Byte) ( 6 * (6 * R + G) + B );
    Dest = HugeOffset(Dest, 1);
    }
}
//---------------------------------------------------------------------------
void __fastcall Trunc6R6G6B( TBitmapInfoHeader const Header,
  Pointer const Data24, Pointer Data8 )
{
  Trunc(Header, Data24, Data8, 8, TruncLine6R6G6B);
}
//---------------------------------------------------------------------------
void __fastcall TripelPal( TRGBPalette &Colors )
{
  int I;

//  FillChar(Colors, sizeof(TRGBPalette), 0);
  memset(Colors, 0, sizeof(TRGBPalette));
  for ( I = 0; I <= 0x40; I ++ )
    {
    Colors[I].rgbRed = (Word)( I << 2 );
    Colors[I + 0x40].rgbGreen = (Word)( I << 2 );
    Colors[I + 0x80].rgbBlue = (Word)( I << 2 );
    }
    
}
//---------------------------------------------------------------------------
void __fastcall Tripel( TBitmapInfoHeader const Header, Pointer Data24, Pointer Data8 )
{
  long int SrcScanline, DstScanline;
  int Y, X;
  PByte Src, Dest;
  Byte R, G, B;

  SrcScanline = (Header.biWidth * 3 + 3) & ~ 3;
  DstScanline = (Header.biWidth + 3) & ~ 3;
  for ( Y = 0; Y < Header.biHeight; Y ++ )
    {
    Src = (Byte *)Data24;
    Dest = (Byte *)Data8;
    for ( X = 0; X < Header.biWidth; X ++ )
      {
      B = *Src; Src = (Byte *)HugeOffset(Src, 1);
      G = *Src; Src = (Byte *)HugeOffset(Src, 1);
      R = *Src; Src = (Byte *)HugeOffset(Src, 1);
      switch ((X + Y) % 3)
        {
        case 0: *Dest = Byte(R >> 2); break;
        case 1: *Dest = Byte(0x40 + (G >> 2)); break;
        case 2: *Dest = Byte(0x80 + (B >> 2)); break;
        }
        
      Dest = (Byte *)HugeOffset(Dest, 1);
      }
      
    Data24 = HugeOffset(Data24, SrcScanline);
    Data8 = HugeOffset(Data8, DstScanline);
    }

}
//---------------------------------------------------------------------------
struct TFreqRecord
{
  Byte B, G, R;
  long int Frequency;
  Byte Nearest;
};
//---------------------------------------------------------------------------
Word Hash( Byte R, Byte G, Byte B )
{
  return Word(Longint(Longint(R + G) * Longint(G + B) *
    Longint(B + R)) % MAX_N_HASH);
}
//---------------------------------------------------------------------------
class THist
{
public :
  long int ColCount;
  Byte Rm, Gm, Bm;
  TFreqRecord Freqs [MAX_N_COLS];
  Word HashTable [MAX_N_HASH];

public :
  THist ( Byte R, Byte G, Byte B );

public :
  bool __fastcall Add ( TBitmapInfoHeader const Header, Pointer Data24 );
  void __fastcall Pal ( TRGBPalette &Colors, int ColorsWanted );
  void __fastcall Map ( TBitmapInfoHeader const Header, Pointer Data24, Pointer Data8 );
  void __fastcall Clear ( Byte R, Byte G, Byte B );
};

typedef THist *PHist;
//---------------------------------------------------------------------------
THist::THist( Byte R, Byte G, Byte B )
{
//{ create empty histogram }


//  PHist Result;

//  GetMem(Result, sizeof(THist));
//  Result = new THist;
//  with Result^ do begin
    Rm = R;
    Gm = G;
    Bm = B;
    ColCount = 0;
//  end;
//  FillChar(Result->HashTable, MAX_N_HASH * SizeOf(Word), 255);
  memset (HashTable, 255, MAX_N_HASH * sizeof(Word) );
}
//---------------------------------------------------------------------------
void __fastcall THist::Clear( Byte R, Byte G, Byte B )
{
//  with Hist^ do begin
    Rm = R;
    Gm = G;
    Bm = B;
    ColCount = 0;
//  end;
//  FillChar(Hist->HashTable, MAX_N_HASH * SizeOf(Word), 255);
  memset(HashTable, MAX_N_HASH * sizeof(Word), 255);
}
//---------------------------------------------------------------------------
void __fastcall THist::Pal ( TRGBPalette &Colors, int ColorsWanted )
{
//{ work out a palette from Hist }
  long int I, J;
  long int MinDist, Dist;
  long int MaxJ, MinJ;
  long int DeltaB, DeltaG, DeltaR;
  long int MaxFreq;

  I = 0; MaxJ = 0; MinJ = 0;
//  { Now find the ColorsWanted most frequently used ones }
  while ((I < ColorsWanted) && (I < ColCount) )
    {
    MaxFreq = 0;
    for ( J = 0; J < ColCount; J ++ )
      {
      if ( Freqs[J].Frequency > MaxFreq )
        {
        MaxJ = J;
        MaxFreq = Freqs[J].Frequency;
        }
      }
    Freqs[MaxJ].Nearest = Byte(I);
    Freqs[MaxJ].Frequency = 0;  //{ Prevent later use of Freqs[MaxJ] }
    Colors[I].rgbBlue = Freqs[MaxJ].B;
    Colors[I].rgbGreen = Freqs[MaxJ].G;
    Colors[I].rgbRed = Freqs[MaxJ].R;
    Colors[I].rgbReserved = 0;
    I ++;
    }
    
//  { Unused palette entries will be medium grey }
  while ( I <= 255 )
    {
    Colors[I].rgbRed = 0x80;
    Colors[I].rgbGreen = 0x80;
    Colors[I].rgbBlue = 0x80;
    Colors[I].rgbReserved = 0;
    I ++;
    }

//  { For the rest, find the closest one in the first ColorsWanted }
  for ( I = 0; I < ColCount; I ++ )
    {
    if ( Freqs[I].Frequency != 0 )
      {
      MinDist = 3 * 256 * 256;
      for ( J = 0; J < ColorsWanted; J ++ )
        {
        DeltaB = Freqs[I].B - Colors[J].rgbBlue;
        DeltaG = Freqs[I].G - Colors[J].rgbGreen;
        DeltaR = Freqs[I].R - Colors[J].rgbRed;
        Dist = Longint(DeltaR * DeltaR) + Longint(DeltaG * DeltaG) +
          Longint(DeltaB * DeltaB);
        if (Dist < MinDist)
          {
          MinDist = Dist;
          MinJ = J;
          }
        }

      Freqs[I].Nearest = Byte(MinJ);
      }
    }

}
//---------------------------------------------------------------------------
void __fastcall THist::Map ( TBitmapInfoHeader const Header, Pointer Data24, Pointer Data8 )
//{ map bitmap data to Hist palette }
{
  int Step24;
  int Step8;
  long int HashColor, Index;
//  Byte Rm, Gm, Bm;
  Byte R, G, B;
  long int X, Y;
 
  Step24 = ((Header.biWidth * 3 + 3) & ~ 3) - Header.biWidth * 3;
  Step8 = ((Header.biWidth + 3) & ~ 3) - Header.biWidth;
//  Rm = Hist.Rm;
//  Gm = Hist.Gm;
//  Bm = Hist.Bm;
  for ( Y = 0; Y < Header.biHeight; Y ++ )
    {
    for ( X = 0; X < Header.biWidth; X ++ )
      {
      B = *((Byte *)Data24) & Bm; Data24 = HugeOffset(Data24, 1);
      G = *((Byte *)Data24) & Gm; Data24 = HugeOffset(Data24, 1);
      R = *((Byte *)Data24) & Rm; Data24 = HugeOffset(Data24, 1);
      HashColor = Hash(R, G, B);
      for (;;)
        {
        Index = HashTable[HashColor];
        if ((Freqs[Index].R == R) && (Freqs[Index].G == G) &&
          (Freqs[Index].B == B)) break;
        HashColor ++;
        if (HashColor == MAX_N_HASH)
          HashColor = 0;
          
        }
        
      *((Byte*)Data8) = Freqs[Index].Nearest;
      Data8 = HugeOffset(Data8, 1);
      }
      
    Data24 = HugeOffset(Data24, Step24);
    Data8 = HugeOffset(Data8, Step8);
    }

}
//---------------------------------------------------------------------------
bool __fastcall THist::Add ( TBitmapInfoHeader const Header, Pointer Data24 )
//{ add bitmap data to histogram }
{
  int Step24;
  Word HashColor, Index;
//  Byte Rm, Gm, Bm;
  Byte R, G, B;
  long int X, Y;
//  long int ColCount;

  Step24 = ((Header.biWidth * 3 + 3) & ~ 3) - Header.biWidth * 3;
/*
  Rm = Hist.Rm;
  Gm = Hist.Gm;
  Bm = Hist.Bm;
  ColCount = Hist.ColCount;
*/
  for ( Y = 0; Y < Header.biHeight; Y ++ )
    {
    for ( X = 0; X < Header.biWidth; X ++ )
      {
      B = *((Byte*)Data24) & Bm; Data24 = HugeOffset(Data24, 1);
      G = *((Byte*)Data24) & Gm; Data24 = HugeOffset(Data24, 1);
      R = *((Byte*)Data24) & Rm; Data24 = HugeOffset(Data24, 1);
      HashColor = Hash(R, G, B);
      for (;;)
        {
        Index = HashTable[HashColor];
        if ((Index == 0xFFFF) || ((Freqs[Index].R == R) &&
          (Freqs[Index].G == G) && (Freqs[Index].B == B)))
          break;
            
        HashColor ++;
        if (HashColor == MAX_N_HASH)
          HashColor = 0;
        }
        
//      { Note: loop will always be broken out of }
//      { We don't allow HashTable to fill up above half full }
      if (Index == 0xFFFF)
        {
//        { Not found in Hash table }
        if (ColCount == MAX_N_COLS)
          return false;

        Freqs[ColCount].Frequency = 1;
        Freqs[ColCount].B = B;
        Freqs[ColCount].G = G;
        Freqs[ColCount].R = R;
        HashTable[HashColor] = (Word)ColCount;
        ColCount ++;
        }
        
      else
//        { Found in Hash table, update index }
        Freqs[Index].Frequency ++;

      }
      
    Data24 = HugeOffset(Data24, Step24);
    }
    
//  ColCount = ColCount;
  return true;
}
//---------------------------------------------------------------------------
void __fastcall Histogram( TBitmapInfoHeader const Header, TRGBPalette &Colors,
  Pointer Data24, Pointer Data8, int ColorsWanted, Byte Rm, Byte Gm, Byte Bm )
//{ map single bitmap to frequency optimised palette }
{
  PHist Hist;

//  Hist = CreateHistogram(Rm, Gm, Bm);
  Hist = new THist (Rm, Gm, Bm);;
  try
    {
    for (;;)
      {
      if ( Hist->Add (Header, Data24))
        break;

      else
        {
        if (Gm > Rm)
          Gm <<= 1;

        else if (Rm > Bm)
          Rm <<= 1;
          
        else
          Bm <<= 1;

        Hist->Clear( Rm, Gm, Bm);
        }
      }
//    { Above loop will always be exited as if masks get rough   }
//    { enough, ultimately number of unique colours < MAX_N_COLS }
    Hist->Pal(Colors, ColorsWanted);
    Hist->Map(Header, Data24, Data8);
    }
  __finally
    {
    delete Hist;
    }

}
//---------------------------------------------------------------------------
int PixelFormatToColors( TPixelFormat PixelFormat )
{
  switch ( PixelFormat )
    {
    case pf1bit: return 2;
    case pf4bit: return 16;
    case pf8bit: return 256;
    default :    return 0;
    }
}
//---------------------------------------------------------------------------
void __fastcall GrayPal( TRGBPalette &Colors )
{
  int I;

//  FillChar(Colors, SizeOf(TRGBPalette), 0);
  memset(Colors, 0, sizeof(TRGBPalette));
  for ( I = 0; I <= 255; I ++ )
    memset (Colors + I, I, 3);
//    FillChar(Colors[I], 3, I);
}
//---------------------------------------------------------------------------
void __fastcall GrayScale ( TBitmapInfoHeader const Header, Pointer Data24, Pointer Data8 )
{
  long int SrcScanline, DstScanline;
  int Y, X;
  PByte Src, Dest;
  Byte R, G, B;

  SrcScanline = (Header.biWidth * 3 + 3) & ~ 3;
  DstScanline = (Header.biWidth + 3) & ~ 3;
  for ( Y = 0; Y < Header.biHeight; Y ++ )
    {
    Src = (PByte)Data24;
    Dest = (PByte)Data8;
    for ( X = 0; X < Header.biWidth; X ++ )
      {
      B = *Src; Src = (PByte)HugeOffset(Src, 1);
      G = *Src; Src = (PByte)HugeOffset(Src, 1);
      R = *Src; Src = (PByte)HugeOffset(Src, 1);
      *Dest = Byte(Longint(Word(R) * 77 + Word(G) * 150 + Word(B) * 29) >> 8);
      Dest = (PByte)HugeOffset(Dest, 1);
      }
       
    Data24 = HugeOffset(Data24, SrcScanline);
    Data8 = HugeOffset(Data8, DstScanline);
    }

}
//---------------------------------------------------------------------------
TMemoryStream* BitmapToMemoryStream ( Graphics::TBitmap *Bitmap, Graphics::TPixelFormat PixelFormat, TMappingMethod Method )
//TMemoryStream *__fastcall BitmapToMemoryStream( Graphics::TBitmap *Bitmap, Graphics::TPixelFormat PixelFormat, TMappingMethod Method )
{
  TR( "BitmapToMemoryStream" );

  PBitmapFileHeader FileHeader;
  PBitmapInfoHeader BI, NewBI;
  Pointer Bits;
  PRGBPalette NewPalette;
  int NewHeaderSize;
  long int ImageSize, Length, Len;
  Pointer P, InitData;
  int ColorCount;

  TMemoryStream* Result;

  if ( Bitmap->Handle == 0 )
    throw new EInvalidGraphic ( "Invalid Bitmap !" );

  if ((GetBitmapPixelFormat(Bitmap) == PixelFormat) &&
    ( Method != mmGrayscale))

    {
    Result = new TMemoryStream;
    try
      {
      Bitmap->SaveToStream(Result);
      Result->Position = 0;
      }
    catch (...)
      {
      delete Result;
      throw;
      }

    return Result;
    }

  if ( ! ( PixelFormat == pf1bit || PixelFormat == pf4bit || PixelFormat == pf8bit || PixelFormat == pf24bit ) )
    throw new EInvalidGraphic ( "Function not yet implemented" );
    
  else if ( PixelFormat == pf1bit || PixelFormat == pf4bit)
    {
    P = DIBFromBit(Bitmap->Handle, Bitmap->Palette, PixelFormat, Length);
    try
      {
      Result = new TMemoryStream;
      try
        {
        Result->Write( P, Length);
        Result->Position = 0;
        }
        
      catch (...)
        {
        delete Result;
        throw;
        }
      }

    __finally
      {
      delete P;
      }
      
    return Result;
    }
//  { pf8bit - expand to 24bit first }
  InitData = DIBFromBit(Bitmap->Handle, Bitmap->Palette, pf24bit, Len);
  try
    {
    BI = PBitmapInfoHeader(Longint(InitData) + sizeof(TBitmapFileHeader));
    if ( BI->biBitCount != 24 )
      throw new EInvalidGraphic ( "Function not yet implemented" );
      
    Bits = Pointer(Longint(BI) + sizeof(TBitmapInfoHeader));
    InternalGetDIBSizes(Bitmap->Handle, NewHeaderSize, ImageSize, PixelFormat);
    Length = sizeof(TBitmapFileHeader) + NewHeaderSize;
    P = new char [ Length ];
    try
      {
      ZeroMemory(P, Length);
      NewBI = PBitmapInfoHeader(Longint(P) + sizeof(TBitmapFileHeader));
      NewPalette = PRGBPalette(Longint(NewBI) + sizeof(TBitmapInfoHeader));
      FileHeader = PBitmapFileHeader(P);
      InitializeBitmapInfoHeader(Bitmap->Handle, *NewBI, PixelFormat);
      switch ( Method )
        {
        case mmQuantize:
          {
          ColorCount = 256;
          Quantize(*BI, Bits, Bits, ColorCount, *NewPalette);
          NewBI->biClrImportant = ColorCount;
          }
          
        case mmTrunc784:
          {
          TruncPal7R8G4B(*NewPalette);
          Trunc7R8G4B(*BI, Bits, Bits);
          NewBI->biClrImportant = 224;
          }
          
        case mmTrunc666:
          {
          TruncPal6R6G6B(*NewPalette);
          Trunc6R6G6B(*BI, Bits, Bits);
          NewBI->biClrImportant = 216;
          }
          
        case mmTripel:
          {
          TripelPal(*NewPalette);
          Tripel(*BI, Bits, Bits);
          }
          
        case mmHistogram:
          {
          Histogram(*BI, *NewPalette, Bits, Bits,
              PixelFormatToColors(PixelFormat), 255, 255, 255);
          }
          
        case mmGrayscale:
          {
          GrayPal(*NewPalette);
          GrayScale(*BI, Bits, Bits);
          }
        }
//      with FileHeader^ do begin
        FileHeader->bfType = 0x4D42;
        FileHeader->bfSize = Length;
        FileHeader->bfOffBits = sizeof(*FileHeader) + NewHeaderSize;
//      end;
      Result = new TMemoryStream;
      try
        {
        Result->Write(P, Length);
        Result->Write(Bits, ImageSize);
        Result->Position = 0;
        }
      catch (...)
        {
        delete Result;
        throw;
        }
      }
    __finally
      {
      delete P;
      }
    }
  __finally
    {
    delete InitData;
    }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::GrayscaleImage( bool ForceEncoding )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GrayscaleImage" );
  TMemoryStream *Mem = NULL;

  if ( ! FGrayscale && ( FBitmap || FImage->FImageData ))
    {
    Mem = BitmapToMemoryStream(Bitmap, pf8bit, mmGrayscale);
    try
      {
      FImage->Release ();
      FImage = new TBMGIFItem;
      FImage->Reference ();
      if ( ForceEncoding )
        EncodeBitmapStream ( Mem );
        
      FGrayscale = true;
/*  Transparency 
      if ( FTransparentColor != clNone )
        FTransparentColor = GrayColor(FTransparentColor);
*/
      FBitmap->LoadFromStream(Mem);
      }

    __finally
      {
      if ( Mem )
        delete Mem;
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::Assign( Classes::TPersistent* Source )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::Assign" );

  TStrings *AComment = NULL;

  if ( ! Source )
    {
    NewImage ();
    if ( FBitmap )
      delete FBitmap;

    FBitmap = NULL;
    }

  else if ( dynamic_cast<TBMGIFFrame *> ( Source ) )
    {
    TR( "TBMGIFFrame::Assign(TBMGIFFrame)" );
    if ( Source != this  )
      {
      TBMGIFFrame * BMGIFFrame = (TBMGIFFrame *)Source;

      FImage->Release ();
      FImage = BMGIFFrame->FImage;
      if ( BMGIFFrame->FOwner != FOwner )
        FLocalColors = true;

      else
        FLocalColors = BMGIFFrame->FLocalColors;

      FImage->Reference ();
      FTopLeft = BMGIFFrame->FTopLeft;
      FInterlaced = BMGIFFrame->FInterlaced;
      if ( BMGIFFrame->Bitmap )
        {
        NewBitmaps ();
        FBitmap->Assign( BMGIFFrame->FBitmap );
//        BMGIFFrame->FBitmap->SaveToFile ( "c:\\Strange.bmp" );
        FMaskBitmap->Assign( BMGIFFrame->FMaskBitmap );
        NewFullBitmaps ();
//        BMGIFFrame->FBitmap->SaveToFile ( "c:\\Strange1.bmp" );
        }
        
/* Transparency
      FTransparentColor = BMGIFFrame->FTransparentColor;
*/
      FTransIndex = BMGIFFrame->FTransIndex;
      FAnimateInterval = BMGIFFrame->FAnimateInterval;
      FDisposal = BMGIFFrame->FDisposal;
      FGrayscale = BMGIFFrame->FGrayscale;
      FCorrupted = BMGIFFrame->FCorrupted;
      AComment = BMGIFFrame->FindComment(false);
      if ( AComment && ( AComment->Count > 0) )
        SetComment(AComment);
      }
    }
    
  else if ( dynamic_cast <TBMGIFImage *>( Source ))
    {
    TR( "TBMGIFFrame::Assign(TBMGIFImage)" );
    TBMGIFImage *BMGIFImage = (TBMGIFImage *)Source;
    
    if ( BMGIFImage->Count > 0)
      {
      if ( BMGIFImage->FrameIndex >= 0 )
        Assign(BMGIFImage->Frames[BMGIFImage->FrameIndex]);
        
      else
        Assign(BMGIFImage->Frames[0]);
      }
      
    else Assign(NULL);
    }
  else if ( dynamic_cast <TGraphic *> ( Source ) )
    {
    // TBitmap, TJPEGImage...
    TR( "TBMGIFFrame::Assign(TGraphic)" );
    TGraphic *BMGraphic = (TGraphic *)Source;
     
    if ( BMGraphic->Empty  )
      {
      Assign( NULL );
      return;
      }
      
    NewImage ();
    NewBitmaps ();
    NewFullBitmaps ();
    try
      {
      FBitmap->Assign(Source);
      if ( dynamic_cast <Graphics::TBitmap *>(Source))
        FBitmap->Monochrome = ((Graphics::TBitmap *)Source)->Monochrome;
      }
    catch (...)
      {
      FBitmap->Canvas->Brush->Color = clFuchsia;
      FBitmap->Width = BMGraphic->Width;
      FBitmap->Height = BMGraphic->Height;
      FBitmap->Canvas->Draw(0, 0, BMGraphic );
      }
      
//{$IFDEF RX_D3}
/* Transparency
    if ( BMGraphic->Transparent  )
      if ( dynamic_cast <Graphics::TBitmap *>(Source))
        FTransparentColor = ((Graphics::TBitmap *)Source)->TransparentColor;

      else FTransparentColor = (TColor)GetNearestColor(FBitmap->Canvas->Handle,
        ColorToRGB(FBitmap->Canvas->Brush->Color));
*/
    }
/*
{$ELSE}
    if ( (Source is TIcon) or (Source is TMetafile) then
      FTransparentColor = GetNearestColor(FBitmap.Canvas.Handle,
        ColorToRGB(FBitmap.Canvas.Brush.Color));
{$ENDIF}
*/
  else
    inherited::Assign(Source);
    
  if ( FOwner )
    FOwner->UpdateScreenSize ();
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::AssignTo ( Classes::TPersistent* Dest )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::AssignTo" );

  if ((dynamic_cast<TBMGIFFrame *>(Dest)) || (dynamic_cast<TBMGIFImage *> (Dest) ))
    Dest->Assign(this);
    
  else if ( dynamic_cast<TGraphic *> ( Dest ) )
    {
    Dest->Assign(Bitmap);
//{$IFDEF RX_D3}
// Transparency >>>>
    if (dynamic_cast<Graphics::TBitmap *> ( Dest ) )
      ((Graphics::TBitmap *)Dest)->Transparent = true;
// <<<< Transparency
/* Transparency
    if (dynamic_cast<Graphics::TBitmap *> ( Dest ) && (FTransparentColor != clNone) )
      {
      ((Graphics::TBitmap *)Dest)->TransparentColor = (TColor )GetNearestColor(
        ((Graphics::TBitmap *)Dest)->Canvas->Handle, ColorToRGB(FTransparentColor));
      ((Graphics::TBitmap *)Dest)->Transparent = true;
      }
*/
//{$ENDIF}
    }
    
  else
    inherited::AssignTo(Dest);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::NewBitmaps ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::NewBitmaps" );

  if ( FBitmap )
    delete FBitmap;
    
  FBitmap = new Graphics::TBitmap;
  
  if ( FMaskBitmap )
    delete FMaskBitmap;

  FMaskBitmap = new Graphics::TBitmap;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::NewFullBitmaps ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::NewFullBitmaps" );

  if ( FFullBitmap )
    delete FFullBitmap;

  FFullBitmap = NULL;                       
  
  if ( FFullMaskBitmap )
    delete FFullMaskBitmap;

  FFullMaskBitmap = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::NewImage ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::NewImage" );

  if ( FImage )
    FImage->Release ();
    
  FImage = new TBMGIFItem;
  FImage->Reference ();
  FGrayscale = false;
  FCorrupted = false;
/*  Transparency
  FTransparentColor = clNone;
*/
  FTransIndex = -1; //Transparency
  
  FTopLeft = Point(0, 0);
  FInterlaced = false;
  FLocalColors = false;
  FAnimateInterval = 0;
  FDisposal = TBMGIFData::dmUndefined;
}
//---------------------------------------------------------------------------
Classes::TStrings* __fastcall TBMGIFFrame::FindComment(bool ForceCreate)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::FindComment" );

  TExtension *Ext;

  Ext = FindExtension ( FExtensions, etComment );
  if ( ! Ext  && ForceCreate )
    {
    Ext = new TExtension;
    try
      {
      Ext->FExtType = etComment;
      if ( ! FExtensions )
        FExtensions = new TList;

      FExtensions->Add(Ext);
      }
    catch (...)
      {
      delete Ext;
      throw;
      }
    }
    
  if ( Ext )
    {
    if ( ! Ext->FData && ForceCreate )
      Ext->FData = new TStringList;

    return Ext->FData;
    }

  return NULL;
}
//---------------------------------------------------------------------------
Classes::TStrings* __fastcall TBMGIFFrame::GetComment(void)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::GetComment" );
  return FindComment ( true );
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::SetComment(Classes::TStrings* Value)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::SetComment" );
  GetComment ()->Assign(Value);
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFFrame::FindTransparent ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::FindTransparent" );

  return FTransIndex;

/* Transparency
  int Result;
  if ( FTransparentColor != clNone )
    for ( Result = 0; Result < FImage->FColorMap.Count; Result ++ )
      if ( ItemToRGB(FImage->FColorMap.Colors[Result]) == ColorToRGB(FTransparentColor) )
        return Result;
        
  return -1;
*/
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::UpdateExtensions(void)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::UpdateExtensions" );
/*
  function FindTransparent: Integer;
  {
    if ( FTransparentColor != clNone )
      for Result = 0 to FImage.FColorMap.Count - 1 do
        if ( ItemToRGB(FImage.FColorMap.Colors[Result]) =
          ColorToRGB(FTransparentColor) ) Exit;
    Result = -1;
  }
*/
  TExtension *Ext;
  int I;
  
  Ext = FindExtension(FExtensions, etGraphic);
/* Transparency
  if ( (FAnimateInterval > 0) || (FTransparentColor != clNone) ||
*/
  if ( (FAnimateInterval > 0) || // (FTransparentColor != clNone) ||
    (FDisposal != TBMGIFData::dmUndefined) )
    {
    if ( ! Ext )
      {
      Ext = new TExtension;
      Ext->FExtType = etGraphic;
      if ( ! FExtensions )
        FExtensions = new TList;
        
      FExtensions->Add(Ext);
/*
      with Ext.FExtRec.GCE do {
        BlockSize = 4;
        PackedFields = 0;
        Terminator = 0;
      }
*/
      Ext->FExtRec.GCE.BlockSize = 4;
      Ext->FExtRec.GCE.PackedFields = 0;
      Ext->FExtRec.GCE.Terminator = 0;
      }
    }
    
  if ( Ext )
    {
/*
    with Ext.FExtRec.GCE do {
      DelayTime = FAnimateInterval / 10;
      I = FindTransparent;
      if ( I >= 0 ) {
        TransparentColorIndex = I;
        PackedFields = PackedFields or GCE_TRANSPARENT;
      end
      else PackedFields = PackedFields and not GCE_TRANSPARENT;
      PackedFields = PackedFields or (Ord(FDisposal) >> 2);
*/
    Ext->FExtRec.GCE.DelayTime = (Word) ( FAnimateInterval / 10 );
    I = FindTransparent ();
    if ( I >= 0 )
      {
      Ext->FExtRec.GCE.TransparentColorIndex = (Byte)I;
      Ext->FExtRec.GCE.PackedFields |= GCE_TRANSPARENT;
      }

    else
      Ext->FExtRec.GCE.PackedFields &= ~GCE_TRANSPARENT;
      
//    Ext->FExtRec.GCE.PackedFields |= (Ord(FDisposal) >> 2);
    Ext->FExtRec.GCE.PackedFields |= (Byte)(FDisposal << 2);
    }

    
  if ( FExtensions )
    {
//    for ( I = FExtensions->Count; --I;)
    for ( I = FExtensions->Count; I--;)
      {
      Ext = (TExtension *)FExtensions->Items[I];
      if ( Ext && ( Ext->FExtType == etComment ) && (( ! Ext->FData ) || ( Ext->FData->Count == 0)) )
        {
        delete Ext;
        FExtensions->Delete(I);
        }
      }
    }
  if ( (FExtensions) && (FExtensions->Count > 0) )
    FOwner->FVersion = TBMGIFData::gv89a;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::EncodeBitmapStream(Classes::TMemoryStream* Stream)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::EncodeBitmapStream" );

  PBitmapInfoHeader BI;
  
  int ColorCount, W, H;
  Pointer Bits, Pal;
  
  ColorCount = 0;
  Stream->Position = 0;
  BI = PBitmapInfoHeader(Longint(Stream->Memory) + sizeof(TBitmapFileHeader));
  W = BI->biWidth; H = BI->biHeight;
  Pal = PRGBPalette((long int)BI + sizeof (TBitmapInfoHeader));
  Bits = Pointer( ( unsigned int )(Stream->Memory) + PBitmapFileHeader(Stream->Memory)->bfOffBits);
  switch ( BI->biBitCount )
    {
    case 1:
      ColorCount = 2;
      break;

    case 4:
      ColorCount = 16;
      break;

    case 8:
      ColorCount = 256;
      break;
      
    default :
      throw EInvalidGraphicOperation ( "EncodeError" );
    }

  FInterlaced = false;
  FillColorTable ( FImage->FColorMap, *PRGBPalette(Pal), ColorCount);
  if ( ! FImage->FImageData )
    FImage->FImageData = new TMemoryStream;
    
  else FImage->FImageData->SetSize(0);
  try
    {
    WriteGIFData(FImage->FImageData, *BI, FInterlaced, Bits, FOwner->DoProgress);
    }
    
  catch ( EAbort * )
    {
    NewImage (); // { OnProgress can raise EAbort to cancel image save }
    throw;
    }
    
  FImage->FBitsPerPixel = 1;
  while ( FImage->FColorMap.Count > ( 1 << FImage->FBitsPerPixel ))
    FImage->FBitsPerPixel ++;
  if ( FOwner->FImage->FColorMap.Count == 0 )
    {
    FOwner->FImage->FColorMap = FImage->FColorMap;
    FOwner->FImage->FBitsPerPixel = FImage->FBitsPerPixel;
    FLocalColors = false;
  }
  else FLocalColors = true;
  FImage->FSize.x = W;
  FImage->FSize.y = H;
  FOwner->FScreenWidth = (Word) max((long)FOwner->FScreenWidth, FImage->FSize.x + FTopLeft.x);
  FOwner->FScreenHeight = (Word) max((long)FOwner->FScreenHeight, FImage->FSize.y + FTopLeft.y);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::EncodeRasterData ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::EncodeRasterData" );

  TMappingMethod Method;
  TMemoryStream *Mem;
  
  if (( ! FBitmap ) || FBitmap->Empty )
   throw EInvalidGraphicOperation ( "No GIF Data" );

  TPixelFormat PixelFormat = GetBitmapPixelFormat(FBitmap);
  if ( ! ( PixelFormat == pf1bit || PixelFormat == pf4bit || PixelFormat == pf8bit))
    {
    if ( FGrayscale )
      Method = mmGrayscale;
      
    else
      Method = DefaultMappingMethod;
      
    Mem = BitmapToMemoryStream(FBitmap, pf8bit, Method);
    if ( (Method == mmGrayscale) ) FGrayscale = true;
    }
    
  else Mem = new TMemoryStream;
  try
    {
    if ( Mem->Size == 0 )
      FBitmap->SaveToStream(Mem);
      
    EncodeBitmapStream(Mem);
    }
    
  __finally
    {
    if ( Mem )
      delete Mem;
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::WriteImageDescriptor(Classes::TStream* Stream)
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::WriteImageDescriptor" );

  TImageDescriptor ImageDesc;

  ImageDesc.PackedFields = 0;
  if ( FLocalColors )
    {
    FImage->FBitsPerPixel = 1;
    while ( FImage->FColorMap.Count > ( 1 << FImage->FBitsPerPixel ))
      FImage->FBitsPerPixel ++;

    ImageDesc.PackedFields = (Byte)((ImageDesc.PackedFields | ID_LOCAL_COLOR_TABLE) +
      (FImage->FBitsPerPixel - 1));
    }

  if ( FInterlaced )
    ImageDesc.PackedFields |= ID_INTERLACED;

  ImageDesc.ImageLeftPos = (Word)FTopLeft.x;
  ImageDesc.ImageTopPos = (Word)FTopLeft.y;
  ImageDesc.ImageWidth = (Word)FImage->FSize.x;
  ImageDesc.ImageHeight = (Word)FImage->FSize.y;
    
//  Stream->Write(&ImageDesc, sizeof(TImageDescriptor));
  Stream->Write(&ImageDesc, 9);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::WriteLocalColorMap( TStream *Stream )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::WriteLocalColorMap" );
  
  if ( FLocalColors )
//    with FImage->FColorMap do
      Stream->Write(& (FImage->FColorMap.Colors[0]), FImage->FColorMap.Count * sizeof(TBMGIFColorItem));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::WriteRasterData( TStream *Stream )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::WriteRasterData" );
  Stream->WriteBuffer( FImage->FImageData->Memory, FImage->FImageData->Size);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::Reduce ()
{
  CRITICAL ( LocalCritical );
  NewFullBitmaps ();

  if ( FBitmap )
    delete FBitmap;

  FBitmap = NULL;

  if ( FMaskBitmap )
    delete FMaskBitmap;

  FMaskBitmap = NULL;
}
//---------------------------------------------------------------------------
TPixelFormat __fastcall TBMGIFFrame::ConvertBitsPerPixel ()
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::ConvertBitsPerPixel" );

  switch ( FImage->FBitsPerPixel )
    {
    case 1:
      return pf1bit;

    case 2:
    case 3:
    case 4:
      return pf4bit;

    case 5:
    case 6:
    case 7:
    case 8:
      return pf8bit;

    default :
      throw EInvalidGraphicOperation ( "Wrong GIF Colors" );
    }

//  return pfDevice;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::SaveToBitmapStream( TMemoryStream *Stream, TMemoryStream *MaskStream )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::SaveToBitmapStream" );

  unsigned long int HeaderSize;
  unsigned long int Length;
  TBitmapInfoHeader BI;
  TBitmapFileHeader BitFile;
  TRGBPalette Colors;
  TRGBPalette MaskColors;
  Pointer Bits;
//  Pointer MaskBits;
  bool Corrupt;
  
//  with BI do {
    BI.biSize = sizeof(TBitmapInfoHeader);
    BI.biWidth = FImage->FSize.x;
    BI.biHeight = FImage->FSize.y;
    BI.biPlanes = 1;
    BI.biBitCount = 0;
/*
    case ConvertBitsPerPixel of
      pf1bit: biBitCount = 1;
      pf4bit: biBitCount = 4;
      pf8bit: biBitCount = 8;
    }
*/
    switch ( ConvertBitsPerPixel () )
      {
      case pf1bit:
        BI.biBitCount = 1;
        break;

      case pf4bit:
        BI.biBitCount = 4;
        break;
        
      case pf8bit:
        BI.biBitCount = 8;
        break;
      }
      
    BI.biCompression = BI_RGB;
    BI.biSizeImage = (((BI.biWidth * BI.biBitCount + 31) / 32) * 4) * BI.biHeight;
    BI.biXPelsPerMeter = 0;
    BI.biYPelsPerMeter = 0;
    BI.biClrUsed = 0;
    BI.biClrImportant = 0;
//  }
  HeaderSize = sizeof(TBitmapFileHeader) + sizeof(TBitmapInfoHeader) +
    sizeof(TRGBQuad) * (1 << BI.biBitCount);
  Length = HeaderSize + BI.biSizeImage;
  Stream->SetSize(0);
  Stream->Position = 0;
  MaskStream->SetSize(0);
  MaskStream->Position = 0;
//  with BitFile do {
    BitFile.bfType = 0x4D42; //{ BM }
    BitFile.bfSize = Length;
    BitFile.bfOffBits = HeaderSize;
//  }
  Stream->Write(&BitFile, sizeof(TBitmapFileHeader));
  Stream->Write(&BI, sizeof(TBitmapInfoHeader));

  MaskStream->Write(&BitFile, sizeof(TBitmapFileHeader));
  MaskStream->Write(&BI, sizeof(TBitmapInfoHeader));

  FillRGBPalette(FImage->FColorMap, Colors);
  Stream->Write(Colors, sizeof(TRGBQuad) * (1 << BI.biBitCount));

  FillMaskRGBPalette(FImage->FColorMap, MaskColors, FTransIndex );
//  Stream->Write(MaskColors, sizeof(TRGBQuad) * (1 << BI.biBitCount));
  MaskStream->Write(MaskColors, sizeof(TRGBQuad) * (1 << BI.biBitCount));

  Bits = new Byte [ BI.biSizeImage ];
//  MaskBits = new Byte [ BI.biSizeImage ];

  try
    {
    ZeroMemory(Bits, BI.biSizeImage);
//    ZeroMemory(MaskBits, BI.biSizeImage);

    FImage->FImageData->Position = 0;
    ReadGIFData(FImage->FImageData, BI, FInterlaced, true /*LoadCorrupt*/,
      FImage->FBitsPerPixel, Bits, Corrupt, FOwner->DoProgress);
      
    FCorrupted |= Corrupt;
    Stream->WriteBuffer(Bits, BI.biSizeImage);
    MaskStream->WriteBuffer(Bits, BI.biSizeImage);
    }

  __finally
    {
    delete [] Bits;
//    delete [] MaskBits;
//    FreeMemo(Bits);
    }
//  }
  Stream->Position = 0;
  MaskStream->Position = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::LoadFromStream( TStream *Stream )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::LoadFromStream" );
  TImageDescriptor ImageDesc;
  int I, TransIndex;
  
  FImage->FImageData = new TMemoryStream;
  try
    {
    ReadImageStream ( Stream, FImage->FImageData, ImageDesc, FInterlaced,
      FLocalColors, FCorrupted, FImage->FBitsPerPixel, FImage->FColorMap);
    FImage->FImageData->Position = 0;
//    with ImageDesc do {
    FTopLeft = Point( ImageDesc.ImageLeftPos, ImageDesc.ImageTopPos);
    FImage->FSize = Point(ImageDesc.ImageWidth, ImageDesc.ImageHeight);
    FImage->FPackedFields = ImageDesc.PackedFields;
//    }
    if ( ! FLocalColors )
      FImage->FColorMap = FOwner->FImage->FColorMap;
      
    FAnimateInterval = 0;
    if ( FExtensions )
      {
      for ( I = 0; I < FExtensions->Count; I ++ )
        {
//        with TExtension(FExtensions[I]) do
//          {
          if ( ((TExtension *)FExtensions->Items[I])->FExtType == etGraphic )
            {
            if ( (((TExtension *)FExtensions->Items[I])->FExtRec.GCE.PackedFields & GCE_TRANSPARENT) != 0 )
              {
              TransIndex = ((TExtension *)FExtensions->Items[I])->FExtRec.GCE.TransparentColorIndex;
              if ( FImage->FColorMap.Count > TransIndex )
/* Transparency
                FTransparentColor = (TColor)ItemToRGB(FImage->FColorMap.Colors[TransIndex]);
*/
                FTransIndex = TransIndex;
              }

            else
/* Transparency
              FTransparentColor = clNone;
*/
              FTransIndex = -1;

            FAnimateInterval = (Word)max(((TExtension *)FExtensions->Items[I])->FExtRec.GCE.DelayTime * 10,
              (int)FAnimateInterval);
            FDisposal = TBMGIFData::TDisposalMethod((((TExtension *)FExtensions->Items[I])->FExtRec.GCE.PackedFields &
              GCE_DISPOSAL_METHOD) >> 2);
            }
            
        }
      }
    }
  catch (...)
    {
    if ( FImage->FImageData )
      delete FImage->FImageData;
      
    FImage->FImageData = NULL;
    throw;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::Draw(Graphics::TCanvas* ACanvas, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::Draw" );
  ACanvas->StretchDraw(ARect, Bitmap);
}
//---------------------------------------------------------------------------
/*
void __fastcall TBMGIFFrame::DrawMasked (Graphics::TCanvas* ACanvas, Graphics::TBitmap* AMask, const Windows::TRect &ARect )
{
  TR( "TBMGIFFrame::DrawMasked" );
  int OldCopyMode = ACanvas->CopyMode;

  ACanvas->CopyMode = cmSrcAnd;
  TRect SourceRect = Bounds ( 0, 0, FOwner->Width, FOwner->Height );
  ACanvas->StretchDraw(SourceRect, AMask);

  ACanvas->CopyMode = cmSrcPaint;
  ACanvas->StretchDraw(ARect, Bitmap);

  ACanvas->CopyMode = OldCopyMode;
}
*/
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::DrawMasked (Graphics::TBitmap* ABitmap, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::DrawMasked" );
/*
  MaskBitmap->SaveToFile ( "c:\\ddMMMMask.bmp" );
  Bitmap->SaveToFile ( "c:\\ddBBBBBmp.bmp" );
  StretchBitmapRectTransparentMask( ABitmap->Canvas, ARect.Left, ARect.Top, Bitmap->Width,
              Bitmap->Height, Bounds(0, 0, Bitmap->Width,
              Bitmap->Height), Bitmap, MaskBitmap );
*/
  int OldCopyMode = ABitmap->Canvas->CopyMode;

  Graphics::TBitmap* TmpBmp = new Graphics::TBitmap;
  Graphics::TBitmap* Tmp2Bmp = new Graphics::TBitmap;
  TmpBmp->Width = MaskBitmap->Width;
  TmpBmp->Height = MaskBitmap->Height;
  Tmp2Bmp->Width = MaskBitmap->Width;
  Tmp2Bmp->Height = MaskBitmap->Height;


  TmpBmp->Canvas->CopyMode = cmSrcCopy;
//  TRect CpyRect = Rect ( 0, 0, MaskBitmap->Width, MaskBitmap->Height );
//  TmpBmp->Canvas->StretchDraw( CpyRect, Bitmap);
  TmpBmp->Canvas->Draw( 0, 0, Bitmap);


  Tmp2Bmp->Canvas->CopyMode = cmSrcInvert;
//  Tmp2Bmp->Canvas->StretchDraw( CpyRect, MaskBitmap );
  Tmp2Bmp->Canvas->Draw( 0, 0, MaskBitmap );
//  Tmp2Bmp->SaveToFile ( "c:\\TmpmSource.bmp" );

  TmpBmp->Canvas->CopyMode = cmSrcAnd;
//  TmpBmp->Canvas->StretchDraw( CpyRect, Tmp2Bmp);
  TmpBmp->Canvas->Draw( 0, 0, Tmp2Bmp);
//  TmpBmp->SaveToFile ( "c:\\Source.bmp" );

  ABitmap->Canvas->CopyMode = cmSrcAnd;
//  ABitmap->Canvas->StretchDraw( ARect, MaskBitmap);
  ABitmap->Canvas->Draw( ARect.Left, ARect.Top, MaskBitmap);

  ABitmap->Canvas->CopyMode = cmSrcPaint;
//  ABitmap->Canvas->StretchDraw(ARect, TmpBmp);
  ABitmap->Canvas->Draw( ARect.Left, ARect.Top, TmpBmp);
  ABitmap->Canvas->CopyMode = OldCopyMode;

  delete Tmp2Bmp;
  delete TmpBmp;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::DrawFull(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::DrawFull" );
  ABitmap->Canvas->StretchDraw(ARect, GetFullBitmap ());
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::DrawMask(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::DrawMask" );
  ABitmap->Canvas->StretchDraw( ARect, MaskBitmap);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::DrawMaskOr(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::DrawMaskOr" );

  TRect SourceRect1 = Rect ( 0, 0, ARect.Left - ARect.Right, ARect.Bottom - ARect.Top );

  TRect SourceRect = Bounds ( 0, 0, FOwner->Width, FOwner->Height );

//  InvertRect( ACanvas->Handle, &((RECT)SourceRect ) );

  int OldCopyMode = ABitmap->Canvas->CopyMode;


//  InvertRect( MaskBitmap->Canvas->Handle, &((RECT)SourceRect1));
  ABitmap->Canvas->CopyMode = cmSrcAnd;
//  ABitmap->Canvas->StretchDraw( ARect, MaskBitmap);
  ABitmap->Canvas->Draw( ARect.Left, ARect.Top, MaskBitmap);

//  ACanvas->CopyRect ( ARect, MaskBitmap->Canvas, SourceRect1 );

//  InvertRect( MaskBitmap->Canvas->Handle, &((RECT)SourceRect1) );

//  ACanvas->CopyRect( ARect, MaskBitmap->Canvas, SourceRect);
//  MaskBitmap->Canvas->CopyRect( ARect, ACanvas, SourceRect);
  ABitmap->Canvas->CopyMode = OldCopyMode;
//  InvertRect( ACanvas->Handle, &((RECT)SourceRect) );
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFFrame::DrawFullMask(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect )
{
  CRITICAL ( LocalCritical );
  TR( "TBMGIFFrame::DrawFullMask" );
  ABitmap->Canvas->StretchDraw( ARect, GetFullMaskBitmap ());
}
//---------------------------------------------------------------------------
//{ TGIFImage }

__fastcall TBMGIFImage::TBMGIFImage()
{
  TR( "TBMGIFImage::TBMGIFImage" );
  NewImage ();
//{$IFDEF RX_D3}
  inherited::SetTransparent(true);
//{$ENDIF}
}
//---------------------------------------------------------------------------
__fastcall TBMGIFImage::~TBMGIFImage()
{
  TR( "TBMGIFImage::~TBMGIFImage" );
  OnChange = NULL;
  FImage->Release ();
  ClearItems ();
  delete FItems;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::GetFrameBitmaps( TBMGIFFrame *TheFrame, Graphics::TBitmap *&Bitmap, Graphics::TBitmap *&MaskBitmap )
{
  TR( "TBMGIFImage::GetFrameBitmaps" );
  int I, Last, First;
  HPALETTE SavePal;


  int Index = FItems->IndexOf ( TheFrame );

  Bitmap = new Graphics::TBitmap;
  MaskBitmap = new Graphics::TBitmap;
  try
    {
//    with Result do {      // Problem ???? !!!!
      Bitmap->Width = ScreenWidth;
      Bitmap->Height = ScreenHeight;

      MaskBitmap->Width = ScreenWidth;
      MaskBitmap->Height = ScreenHeight;
//      Bitmap->PixelFormat = pf24bit;
//      Bitmap->HandleType = bmDDB;
      SavePal = 0;
      if ( Palette != 0 )
        {
        SavePal = SelectPalette(Bitmap->Canvas->Handle, Palette, false);
        RealizePalette(Bitmap->Canvas->Handle);
        }
      MaskBitmap->Canvas->Brush->Color = clWhite; // Transparency
/*
      if ( (BackgroundColor != clNone) && Transparent )
        Bitmap->Canvas->Brush->Color = (TColor)PaletteColor(BackgroundColor);

      else
        Bitmap->Canvas->Brush->Color = (TColor)PaletteColor(clWindow);

      Bitmap->Canvas->FillRect(Bounds(0, 0, Bitmap->Width, Bitmap->Height));
*/

      Bitmap->Canvas->FillRect(Bounds(0, 0, Bitmap->Width, Bitmap->Height)); // Transparency
      MaskBitmap->Canvas->FillRect(Bounds(0, 0, MaskBitmap->Width, MaskBitmap->Height)); // Transparency

      Last = min(Index, Count - 1);
      First = max(0, Last - 1);
      TRect Rect = Bounds(Frames[Last]->Origin.x, Frames[Last]->Origin.y, Frames[Last]->Width, Frames[Last]->Height);

      while ( First > 0 )
        {
        if ( (Frames[First]->DisposalMethod == TBMGIFData::dmRestoreBackground) &&
          (ScreenWidth == Frames[First]->Width) &&
          (ScreenHeight == Frames[First]->Height) )
          break;
        First --;
        }

      for ( I = First; I < Last; I ++ )
        {
        switch ( Frames[I]->DisposalMethod )
          {
          case TBMGIFData::dmUndefined:
          case TBMGIFData::dmLeave:
//            Bitmap->SaveToFile ( "c:\\OldRRRRBmp.bmp" );
//            MaskBitmap->SaveToFile ( "c:\\OldMRRRRBmp.bmp" );
            Frames[I]->DrawMaskOr(MaskBitmap, Bounds(Frames[I]->Origin.x, Frames[I]->Origin.y, Frames[I]->Width, Frames[I]->Height));
            Frames[I]->DrawMasked (Bitmap, Bounds(Frames[I]->Origin.x, Frames[I]->Origin.y, Frames[I]->Width, Frames[I]->Height));
//            Frames[I]->FBitmap->SaveToFile ( "c:\\bRRRRBmp.bmp" );
//            Frames[I]->FMaskBitmap->SaveToFile ( "c:\\bMRRRRBmp.bmp" );
//            Bitmap->SaveToFile ( "c:\\RRRRBmp.bmp" );
//            MaskBitmap->SaveToFile ( "c:\\MRRRRBmp.bmp" );
            break;

          case TBMGIFData::dmRestoreBackground:
            if ( I > First )
              {
              Bitmap->Canvas->FillRect(Bounds(Frames[I]->Origin.x, Frames[I]->Origin.y, Frames[I]->Width, Frames[I]->Height));
              MaskBitmap->Canvas->FillRect(Bounds(Frames[I]->Origin.x, Frames[I]->Origin.y, Frames[I]->Width, Frames[I]->Height));
              }

            break;

          case TBMGIFData::dmRestorePrevious:
//              { { do nothing } }
            ;
          }
        }            

//      with FImage.Frames[Last] do
//      TRect Rect = Bounds(Frames[Last]->Origin.x, Frames[Last]->Origin.y, Frames[Last]->Width, Frames[Last]->Height);

      Frames[Last]->DrawMaskOr (MaskBitmap, Rect );
      Frames[Last]->DrawMasked(Bitmap, Rect);
//      Frames[Last]->FBitmap->SaveToFile ( "c:\\bRRRRBmp.bmp" );
//      Frames[Last]->FMaskBitmap->SaveToFile ( "c:\\bMRRRRBmp.bmp" );
//      Bitmap->SaveToFile ( "c:\_RRFrame.bmp" );
//      MaskBitmap->SaveToFile ( "c:\_RRFrameMask.bmp" );
//{$IFDEF RX_D3}
// Transparency >>>>
//      if ( FTransparent )
        Bitmap->Transparent = true;
// <<<< Transparency
/* Transparency !!!!!!!!!!!! Temporary out. To be implemented.
      if ( (TransColor != clNone) && FTransparent )
        {
//        Frames[Last]->TransparentColor = (TColor)PaletteColor(TransColor);
        Bitmap->TransparentColor = (TColor)PaletteColor(TransColor);
        Bitmap->Transparent = true;
//      }
*/
//{$ENDIF}
      if ( Palette != 0 )
        SelectPalette(Bitmap->Canvas->Handle, SavePal, false);
      }
//    }

  catch (...)
    {
    delete Bitmap;
    delete MaskBitmap;
    throw;
    }

//  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::Clear ()
{
  TR( "TBMGIFImage::Clear" );
  Assign(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ClearItems ()
{
  TR( "TBMGIFImage::ClearItems" );
  if ( FItems != NULL )
    while ( FItems->Count > 0 )
      {
      delete (TObject *)( FItems->Items[0] );
      FItems->Delete(0);
      }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::Assign(Classes::TPersistent* Source)
{
  TR( "TBMGIFImage::Assign" );
  int I;
  TBMGIFFrame *AFrame;
  
  if ( Source == NULL)
    {
    TR( "TBMGIFImage::Assign(NULL)" );
    NewImage ();
    Changed(this);
    }
    
  else if ( ( dynamic_cast <TBMGIFImage *> ( Source ) ) && (Source != this) )
    {
    TR( "TBMGIFImage::Assign(TBMGIFImage)" );
    
    TBMGIFImage *BMGIFImage = (TBMGIFImage *)Source;
    FImage->Release ();
    FImage = BMGIFImage->FImage;
    FImage->Reference ();
    FVersion = BMGIFImage->FVersion;
    FBackgroundColor = BMGIFImage->FBackgroundColor;
    FRepeatCount = BMGIFImage->FRepeatCount;
    FLooping = BMGIFImage->FLooping;
    if ( FItems == NULL )
      FItems = new TList;
      
    else
      ClearItems ();
      
//    with TGIFImage(Source) do {  // WARNING Possible mistake !!!!
      for ( I = 0; I < BMGIFImage->FItems->Count; I ++ )
        {
        TR( (AnsiString)"TBMGIFImage::Assign I = " + I );
        AFrame = new TBMGIFFrame (this);
        try
          {
          TR( (AnsiString)"TBMGIFImage::Assign >> I = " + I );
          AFrame->FImage->FBitsPerPixel =
            ((TBMGIFFrame*)BMGIFImage->FItems->Items[I])->FImage->FBitsPerPixel;
          AFrame->Assign(((TBMGIFFrame*)BMGIFImage->FItems->Items[I]));
          AFrame->FLocalColors = ((TBMGIFFrame*)BMGIFImage->FItems->Items[I])->FLocalColors;
//          this.FItems.Add(AFrame);
          FItems->Add(AFrame);
          }
          
        catch (...)
          {
          TR( (AnsiString)"TBMGIFImage::Assign ??? I = " + I );
          if ( AFrame )
            delete AFrame;
            
          throw;
          }
        }
//      }
//      this.FScreenWidth = FScreenWidth;
//      this.FScreenHeight = FScreenHeight;
      FScreenWidth = BMGIFImage->FScreenWidth;
      FScreenHeight = BMGIFImage->FScreenHeight;
//    }
    FFrameIndex = BMGIFImage->FFrameIndex;
    Changed(this);
  }
  else if ( dynamic_cast <TBMGIFFrame *> ( Source ))
    {
    TR( "TBMGIFImage::Assign(TBMGIFFrame)" );
    TBMGIFFrame *BMGIFFrame = (TBMGIFFrame*) Source;
    NewImage ();
//    with TBMGIFFrame(Source).FOwner->FImage do {
      FImage->FAspectRatio = BMGIFFrame->FOwner->FImage->FAspectRatio;
      FImage->FBitsPerPixel = BMGIFFrame->FOwner->FImage->FBitsPerPixel;
      FImage->FColorResBits = BMGIFFrame->FOwner->FImage->FColorResBits;
      Move(& (BMGIFFrame->FOwner->FImage->FColorMap), & ( FImage->FColorMap ), sizeof(FImage->FColorMap));
//    }
    
    FFrameIndex = FItems->Add( new TBMGIFFrame(this));
    (( TBMGIFFrame *)(FItems->Items[FFrameIndex]))->Assign(Source);
    if ( FVersion == TBMGIFData::gvUnknown )
      FVersion = TBMGIFData::gv87a;
      
    Changed(this);
  }
  else if ( dynamic_cast <Graphics::TBitmap *> ( Source ) )
    {
    TR( "TBMGIFImage::Assign(TBitmap)" );
    NewImage ();
    AddFrame ((Graphics::TBitmap *)(Source));
    Changed(this);
    }

#if 0 // Temporery out !
  else if ( dynamic_cast <TAnimatedCursorImage*>( Source )  )
    {
    NewImage ();
    FBackgroundColor = clWindow;
    with TAnimatedCursorImage(Source) do {
      for I = 0 to IconCount - 1 do {
        AddFrame(TIcon(Icons[I]));
        this.Frames[FrameIndex].FAnimateInterval =
          Longint(Frames[I].JiffRate * 100) / 6;
      }
    }
    Changed(this);
    }
#endif

  else
    inherited::Assign(Source);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::AssignTo(Classes::TPersistent* Dest)
{
  TR( "TBMGIFImage::AssignTo" );

  if ( dynamic_cast <TBMGIFImage *> ( Dest ) )
    Dest->Assign(this);
    
  else if ( dynamic_cast <TGraphic *> ( Dest ))
    {
    if ( Empty )
      Dest->Assign(NULL);
      
    else if ( FFrameIndex >= 0 )
      ((TBMGIFFrame* )FItems->Items[FFrameIndex])->AssignTo(Dest);
      
    else Dest->Assign(Bitmap);
    }
  else
    inherited::AssignTo(Dest);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::Draw(Graphics::TCanvas* ACanvas, const Windows::TRect &ARect)
{
  if ( FFrameIndex >= 0 )
    ((TBMGIFFrame* )FItems->Items[FFrameIndex])->Draw(ACanvas, ARect);
}
//---------------------------------------------------------------------------
Graphics::TColor __fastcall TBMGIFImage::GetBackgroundColor(void)
{
  return FBackgroundColor;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetBackgroundColor(Graphics::TColor Value)
{
  if ( Value != FBackgroundColor )
    {
    FBackgroundColor = Value;
    Changed(this);
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetLooping(bool Value)
{
  if ( Value != FLooping )
    {
    FLooping = Value;
    Changed(this);
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetRepeatCount(Word Value)
{
  Word Min = min(Value, MAX_LOOP_COUNT);
  
  if ( Min != FRepeatCount )
    {
    FRepeatCount = Min;
    Changed(this);
    }
    
}
//---------------------------------------------------------------------------
Graphics::TPixelFormat __fastcall TBMGIFImage::GetPixelFormat(void)
{
  int I;
  
  Graphics::TPixelFormat Result = pfDevice;
  if ( ! Empty )
    {
    Result = ColorsToPixelFormat( ( Word ) FImage->FColorMap.Count );
    for ( I = 0; I <  FItems->Count; I ++ )
      {
      if ( (Frames[I]->FImage->FImageData == NULL) ||
        (Frames[I]->FImage->FImageData->Size == 0) )
        {
        if ( Frames[I]->FBitmap )
//          Result = TPixelFormat(max(Ord(Result),
//            Ord(GetBitmapPixelFormat(Frames[I]->FBitmap))));
          Result = TPixelFormat(max(Result,
            GetBitmapPixelFormat(Frames[I]->FBitmap)));

        else
//          Result = TPixelFormat(Max(Ord(Result), Ord(pfDevice)));
          Result = TPixelFormat(max(Result, pfDevice));
        }
        
      else if ( Frames[I]->FLocalColors )
        Result = TPixelFormat(max(Result,
          ColorsToPixelFormat((Word)Frames[I]->FImage->FColorMap.Count)));
      }
    }

  return Result;
}
//---------------------------------------------------------------------------
Graphics::TColor __fastcall TBMGIFImage::GetTransparentColor(void)
{
/*  Tranparency
  if ( (FItems->Count > 0) && (FFrameIndex >= 0) )
    return ((TBMGIFFrame*)FItems->Items[FFrameIndex])->FTransparentColor;
*/
  return clNone;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetCount(void)
{
  return FItems->Count;
}
//---------------------------------------------------------------------------
TBMGIFFrame* __fastcall TBMGIFImage::GetFrame(int Index)
{
  return (TBMGIFFrame*)FItems->Items[Index];
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetFrameIndex(int Value)
{
  Value = min(FItems->Count - 1, max(-1, Value));
  if ( FFrameIndex != Value )
    {
    FFrameIndex = Value;
//{$IFDEF RX_D3}
    PaletteModified = true;
//{$ENDIF}
    Changed(this);
    }
}
//---------------------------------------------------------------------------
//{$IFDEF WIN32}
bool __fastcall TBMGIFImage::Equals(Graphics::TGraphic* Graphic)
{
  return (dynamic_cast <TBMGIFImage *> (Graphic) ) &&
    (FImage == ((TBMGIFImage*)Graphic )->FImage);
}
//{$ENDIF}
//---------------------------------------------------------------------------
Graphics::TBitmap* __fastcall TBMGIFImage::GetBitmap(void)
{
  TR( "TBMGIFImage::GetBitmap" );

  Graphics::TBitmap *Bmp;
  Graphics::TBitmap *Result;
  
  if ( (FItems->Count > 0) )
    {
    if ( (FFrameIndex >= 0) && (FFrameIndex < FItems->Count) )
      Result = (( TBMGIFFrame* )FItems->Items[FFrameIndex])->Bitmap;

    else
      Result = (( TBMGIFFrame* )FItems->Items[0])->Bitmap;
    }
    
  else
    {
    FFrameIndex = 0;
    Bmp = new Graphics::TBitmap;
    try
      {
      Bmp->Handle = 0;
      Assign(Bmp);
      Result = (( TBMGIFFrame* )FItems->Items[FFrameIndex])->Bitmap;
      }
      
    __finally
      {
      if ( Bmp )
        delete Bmp;
      }
    }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetGlobalColorCount(void)
{
  return FImage->FColorMap.Count;
}
//---------------------------------------------------------------------------
bool __fastcall TBMGIFImage::GetEmpty(void)
{
  int I;
  
  I = max(FFrameIndex, 0);
  return (FItems->Count == 0) ||
    ((((TBMGIFFrame*)FItems->Items[I])->FBitmap == NULL) &&
    ((((TBMGIFFrame*)FItems->Items[I])->FImage->FImageData == NULL) ||
    (((TBMGIFFrame*)FItems->Items[I])->FImage->FImageData->Size == 0)));
}
//---------------------------------------------------------------------------
HPALETTE __fastcall TBMGIFImage::GetPalette(void)
{
  if ( FItems->Count > 0 )
    return Bitmap->Palette;

  return 0;
}
//---------------------------------------------------------------------------
bool __fastcall TBMGIFImage::GetTransparent(void)
{
  return false;
/* Tranparency
  int I;

//{$IFDEF RX_D3}
  if ( inherited::GetTransparent () )
//{$ENDIF}
    for ( I = 0; I < FItems->Count; I ++ )
      if ( Frames[I]->TransparentColor != clNone ) {
        return true;
      }

  return false;
*/
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetHeight(void)
{
  if ( ~ Empty && (FFrameIndex >= 0) && (FFrameIndex < Count) )
    return ((TBMGIFFrame *)FItems->Items[FFrameIndex])->Bitmap->Height;
    
  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetWidth(void)
{
  if ( ~ Empty && (FFrameIndex >= 0) && (FFrameIndex < Count) )
    return ((TBMGIFFrame *)FItems->Items[FFrameIndex])->Bitmap->Width;
    
  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetScreenWidth(void)
{
  if ( Empty )
    return 0;

  return FScreenWidth;
}
//---------------------------------------------------------------------------
int __fastcall TBMGIFImage::GetScreenHeight(void)
{
  if ( Empty )
    return 0;

  return FScreenHeight;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::LoadFromClipboardFormat(Word AFormat, int AData, HPALETTE APalette)
{
  Graphics::TBitmap *Bmp;
  TMemoryStream *Stream;
  long int Size;
  Pointer Buffer;
//  THandle Data;
  HGLOBAL Data;

//  { !! check for ( gifclipboard Data, mime type image/gif }
  Data = GetClipboardData(CF_GIF);
  if ( Data != 0 ) {
    Buffer = GlobalLock(Data);
    try
      {
      Stream = new TMemoryStream;
      try
        {
        Stream->Write(Buffer, GlobalSize(Data));
        Stream->Position = 0;
        Stream->Read(&Size, sizeof(Size));
        ReadStream(Size, Stream, false);
        if ( Count > 0 )
          {
          FFrameIndex = 0;
          AData = (int)GetClipboardData(CF_BITMAP);
          if ( AData != 0 )
            {
            Frames[0]->NewBitmaps ();
            Frames[0]->NewFullBitmaps ();
            Frames[0]->FBitmap->LoadFromClipboardFormat(CF_BITMAP,
              (int)AData, APalette);
            }
          }
        }
      __finally
        {
        if ( Stream )
          delete Stream;
        }
      }
    __finally
      {
      GlobalUnlock(Data);
      }
    }
    
  else {
    Bmp = new Graphics::TBitmap;
    try
      {
      Bmp->LoadFromClipboardFormat(AFormat, AData, APalette);
      Assign(Bmp);
      }
      
    __finally
      {
      delete Bmp;
      }
    }
//  }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::LoadFromStream(Classes::TStream* Stream)
{
  TR( "TBMGIFImage::LoadFromStream" );
  ReadStream(Stream->Size - Stream->Position, Stream, true);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::UpdateScreenSize(void)
{
  int I;

  FScreenWidth = 0;
  FScreenHeight = 0;
  for ( I = 0; I < FItems->Count; I ++ )
    if ( Frames[I] != NULL )
      {
      FScreenWidth = (Word)max( (long) FScreenWidth, Frames[I]->Width +
        Frames[I]->FTopLeft.x);
      FScreenHeight = (Word)max( (long) FScreenHeight, Frames[I]->Height +
        Frames[I]->FTopLeft.y);
      }
      
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::AddFrame(Graphics::TGraphic* Value)
{
  FFrameIndex = FItems->Add( new TBMGIFFrame(this));
  ((TBMGIFFrame*)FItems->Items[FFrameIndex])->Assign(Value);
  if ( FVersion == TBMGIFData::gvUnknown )
    FVersion = TBMGIFData::gv87a;

  if ( FItems->Count > 1 )
    FVersion = TBMGIFData::gv89a;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::DeleteFrame(int Index)
{
  delete Frames[Index];
  FItems->Delete(Index);
  UpdateScreenSize ();
  if ( FFrameIndex >= FItems->Count )
    FFrameIndex --;
    
  Changed(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::MoveFrame(int CurIndex, int NewIndex)
{
  FItems->Move(CurIndex, NewIndex);
  FFrameIndex = NewIndex;
  Changed(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::NewImage ()
{
  if ( FImage != NULL )
    FImage->Release ();
    
  FImage = new TBMGIFData;
  FImage->Reference ();
  if ( FItems == NULL )
    FItems = new TList;
    
  ClearItems ();
  FFrameIndex = -1;
  FBackgroundColor = clNone;
  FRepeatCount = 1;
  FLooping = false;
  FVersion = TBMGIFData::gvUnknown;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::UniqueImage ()
{
  TBMGIFData *Temp;
  
  if ( FImage == NULL )
    NewImage ();
    
  else if ( FImage->RefCount > 1 )
    {
    Temp = new TBMGIFData;
//    with Temp )
    try
      {
      Temp->FComment->Assign(FImage->FComment);
      Temp->FAspectRatio = FImage->FAspectRatio;
      Temp->FBitsPerPixel = FImage->FBitsPerPixel;
      Temp->FColorResBits = FImage->FColorResBits;
      Temp->FColorMap = FImage->FColorMap;
      }
      
    catch (...)
      {
      delete Temp;
      throw;
      }

    FImage->Release ();
    FImage = Temp;
    FImage->Reference ();
    }
}
//---------------------------------------------------------------------------
Classes::TStrings* __fastcall TBMGIFImage::GetComment(void)
{
  return FImage->FComment;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetComment(Classes::TStrings* Value)
{
  UniqueImage ();
  FImage->FComment->Assign(Value);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::DecodeAllFrames(void)
{
  TR( "TBMGIFImage::DecodeAllFrames" );

  int FrameNo;
  
//  for ( FrameNo = FItems->Count; --FrameNo; )
  for ( FrameNo = FItems->Count; FrameNo--; )
    ((TBMGIFFrame*)FItems->Items[FrameNo])->GetBitmap ();
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::EncodeFrames(bool ReverseDecode)
{
  TR( "TBMGIFImage::EncodeFrames" );

  int FrameNo;

  for ( FrameNo = 0; FrameNo < FItems->Count; FrameNo ++ )
    {
//    with TBMGIFFrame(FItems->Items[FrameNo]) )       // (( TBMGIFFrame * )FItems->Items[FrameNo])->
//      {
    if ( ((( TBMGIFFrame * )FItems->Items[FrameNo])->FImage->FImageData == NULL) || ((( TBMGIFFrame * )FItems->Items[FrameNo])->FImage->FImageData->Size == 0) )
      {
      delete ( (( TBMGIFFrame * )FItems->Items[FrameNo])->FImage->FImageData );
      (( TBMGIFFrame * )FItems->Items[FrameNo])->FImage->FImageData = NULL;
      (( TBMGIFFrame * )FItems->Items[FrameNo])->EncodeRasterData ();
      if ( ReverseDecode && ( (( TBMGIFFrame * )FItems->Items[FrameNo])->FBitmap->Palette == 0) )
        {
        delete ( (( TBMGIFFrame * )FItems->Items[FrameNo])->FBitmap );
        (( TBMGIFFrame * )FItems->Items[FrameNo])->FBitmap = NULL;
        (( TBMGIFFrame * )FItems->Items[FrameNo])->GetBitmap ();
        }
      }

    (( TBMGIFFrame * )FItems->Items[FrameNo])->UpdateExtensions ();
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::EncodeAllFrames(void)
{
  TR( "TBMGIFImage::EncodeAllFrames" );
  EncodeFrames(true);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ReadData(Classes::TStream* Stream)
{
  TR( "TBMGIFImage::ReadData" );
  long int Size;
  
  Stream->Read(&Size, sizeof(Size));
  ReadStream(Size, Stream, true);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ReadSignature(Classes::TStream* Stream)
{
  TR( "TBMGIFImage::ReadSignature" );
  TBMGIFData::TBMGIFVersion I;
//  AnsiString S [3];
  AnsiString S;
  
  FVersion = TBMGIFData::gvUnknown;
  S.SetLength(3);
  Stream->Read(S.c_str (), 3);
  if ( GIFSignature != S )
    throw EInvalidGraphicOperation ( "GIF Version error" );
//    GifError(LoadStr(SGIFVersion));
    
  S.SetLength(3);
  Stream->Read(S.c_str (), 3);
  for ( I = TBMGIFData::gvUnknown; I <= TBMGIFData::MaxVer; I = (TBMGIFData::TBMGIFVersion)( I + 1 ) )
    if ( memcmp ( S.c_str (), GIFVersionStr[I].c_str (), 3 ) == 0 )
      {
      FVersion = I;
      break;
      }
      
  if ( FVersion == TBMGIFData::gvUnknown )
    throw EInvalidGraphicOperation ( "GIF Version error" );
//    GifError(LoadStr(SGIFVersion));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ReadScreenDescriptor( TStream *Stream, TScreenDescriptor & ScreenDesc )
{
  TR( "TBMGIFImage::ReadScreenDescriptor" );
//  Stream->Read(&ScreenDesc, sizeof(ScreenDesc));
  Stream->Read(&ScreenDesc, 7);
  FScreenWidth = ScreenDesc.ScreenWidth;
  FScreenHeight = ScreenDesc.ScreenHeight;
//  with FImage ) {
    FImage->FAspectRatio = ScreenDesc.AspectRatio;
    FImage->FBitsPerPixel = (Byte) ( 1 + (ScreenDesc.PackedFields &
      LSD_COLOR_TABLE_SIZE) );
    FImage->FColorResBits = (Byte) ( 1 + ((ScreenDesc.PackedFields &
      LSD_COLOR_RESOLUTION) >> 4 ));
//  }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ReadGlobalColorMap( TStream *Stream, TScreenDescriptor & ScreenDesc )
{
  TR( "TBMGIFImage::ReadGlobalColorMap" );
  if ( (ScreenDesc.PackedFields & LSD_GLOBAL_COLOR_TABLE) != 0 )
    {
//      with FImage->FColorMap ) {
    FImage->FColorMap.Count = 1 << FImage->FBitsPerPixel;
    Stream->Read(FImage->FColorMap.Colors, FImage->FColorMap.Count * sizeof(TBMGIFColorItem));
    if ( FImage->FColorMap.Count > ScreenDesc.BackgroundColorIndex )
      FBackgroundColor = (TColor)ItemToRGB(FImage->FColorMap.Colors[ScreenDesc.BackgroundColorIndex]);
    }
//      }
}
//---------------------------------------------------------------------------
TStrings *__fastcall TBMGIFImage::ReadDataBlock( TStream *Stream )
{
  TR( "TBMGIFImage::ReadDataBlock" );

  Byte BlockSize;
  AnsiString S;

  TStrings *Result = new TStringList;

  try
    {
    do
      {
      Stream->Read(&BlockSize, sizeof(Byte));
      if ( BlockSize != 0 )
        {
        S.SetLength(BlockSize);
        Stream->Read(S.c_str (), BlockSize);
        Result->Add(S);
        }
      }

    while ( ! ((BlockSize == 0) || (Stream->Position >= Stream->Size)));
    }

  catch (...)
    {
    delete Result;
    throw;
    }

  return Result;
}
//---------------------------------------------------------------------------
TExtension  *__fastcall TBMGIFImage::ReadExtension( TStream *Stream )
{
  TR( "TBMGIFImage::ReadExtension" );

  Byte ExtensionLabel;
  TExtension  *Result = new TExtension;

  try
    {
    Stream->Read(&ExtensionLabel, sizeof(Byte));
//    with Result )
        if ( ExtensionLabel == ExtLabels[etGraphic] )
          {
//          { graphic control extension }
          Result->FExtType = etGraphic;
          Stream->Read(&Result->FExtRec.GCE, sizeof(TGraphicControlExtension));
        }
        else if ( ExtensionLabel == ExtLabels[etComment] ) {
//          { comment extension }
          Result->FExtType = etComment;
          Result->FData = ReadDataBlock(Stream);
        }
        else if ( ExtensionLabel == ExtLabels[etPlainText] )
          {
//          { plain text extension }
          Result->FExtType = etPlainText;
//          Stream->Read(&Result->FExtRec.PTE, sizeof(TPlainTextExtension));
          Stream->Read(&Result->FExtRec.PTE, 13);
          Result->FData = ReadDataBlock(Stream);
          }
          
        else if ( ExtensionLabel == ExtLabels[etApplication] )
          {
//          { application extension }
          Result->FExtType = etApplication;
          Stream->Read(&Result->FExtRec.APPE, sizeof(TAppExtension));
          Result->FData = ReadDataBlock(Stream);
          }

        else
          throw EInvalidGraphicOperation ( "Unrecognized GIF Ext" );
//          GifError(Format(LoadStr(SUnrecognizedGIFExt), [ExtensionLabel]));
      }
                
    catch (...)
      {
      delete Result;
      throw;
      }
      
  return Result;
}
//---------------------------------------------------------------------------
TList *__fastcall TBMGIFImage::ReadExtensionBlock( TStream *Stream, char &SeparatorChar )
{
  TR( "TBMGIFImage::ReadExtensionBlock" );

    TExtension *NewExt;
    
    TList *Result = NULL;
    try
      {
      while ( SeparatorChar == CHR_EXT_INTRODUCER )
        {
        NewExt = ReadExtension(Stream);
        if ( (NewExt->FExtType == etPlainText) ) {
//          { plain text data blocks are not supported,
//            clear all previous readed extensions }
          FreeExtensions(Result);
          Result = NULL;
        }
        if ( ( NewExt->FExtType == etPlainText || NewExt->FExtType == etApplication ) )
          {
//          { check for ( loop extension }
          if ( NewExt->IsLoopExtension () ) {
            FLooping = true;
            FRepeatCount = min(MakeWord(Byte(NewExt->FData->Strings[0][2]),
              Byte(NewExt->FData->Strings[0][3])), MAX_LOOP_COUNT);     // Not sure for it !!!
          }
//          { not supported yet, must be ignored }
          delete NewExt;
        }
        else {
          if ( Result == NULL ) Result = new TList;
          Result->Add(NewExt);
        }
        if ( Stream->Size > Stream->Position )
          Stream->Read(&SeparatorChar, sizeof(Byte));

        else SeparatorChar = CHR_TRAILER;
        }
        
      if ( (Result != NULL) && (Result->Count == 0) )
        {
        delete Result;
        Result = NULL;
        }
      }
    catch (...)
      {
      if ( Result )
        delete Result;
        
      throw;
      }
    
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::ReadStream(int Size, Classes::TStream* Stream, bool ForceDecode)
{
//  ForceDecode  = true;  // ??????
  
  TR( "TBMGIFImage::ReadStream" );

  char SeparatorChar;
  TBMGIFFrame *NewItem;
  TList *Extensions;
  TScreenDescriptor ScreenDesc;
  TMemoryStream *Data;

  int I;
  TExtension *Ext;

  NewImage ();
//  with FImage ) {
    Data = new TMemoryStream;
    try
      {
//      ((TMemoryStream *)Data)->SetSize(Size);
      Data->SetSize(Size);
      Stream->ReadBuffer(Data->Memory, Size);
      if ( Size > 0 ) {
        Data->Position = 0;
        ReadSignature(Data);
        ReadScreenDescriptor(Data, ScreenDesc);
        ReadGlobalColorMap(Data, ScreenDesc);
//        Data->Read(&SeparatorChar, 1);
        Data->Read(&SeparatorChar, sizeof(Byte));
        while ( (! (SeparatorChar == CHR_TRAILER || SeparatorChar == '\0' )) && !
          (Data->Position >= Data->Size) )
          {
          Extensions = ReadExtensionBlock(Data, SeparatorChar);
          if ( SeparatorChar == CHR_IMAGE_SEPARATOR )
            {
            try
              {
              NewItem = new TBMGIFFrame(this);
              try
                {
                if ( FImage->FColorMap.Count > 0 )
                  NewItem->FImage->FBitsPerPixel =
                    ColorsToBits((Word)FImage->FColorMap.Count);
                NewItem->FExtensions = Extensions;
                Extensions = NULL;
                NewItem->LoadFromStream(Data);
                FItems->Add(NewItem);
                }
              catch (...)
                {
                delete NewItem;
                throw;
                }

              if ( ! (Data->Position >= Data->Size) )
                {
                Data->Read(&SeparatorChar, sizeof(Byte));
                while ( (SeparatorChar == '\0') && (Data->Position < Data->Size) )
                  Data->Read(&SeparatorChar, sizeof(Byte));
                }

              else
                SeparatorChar = CHR_TRAILER;

              if ( ! ( SeparatorChar == CHR_EXT_INTRODUCER  ||
                       SeparatorChar == CHR_IMAGE_SEPARATOR ||
                       SeparatorChar == CHR_TRAILER ) )
                {
                SeparatorChar = '\0';
//                {GifError(LoadStr(SGIFDecodeError));}
                }
              }
            catch (...)
              {
              FreeExtensions(Extensions);
              throw;
              }
            }
          else if ( (FImage->FComment->Count == 0) && (Extensions != NULL) )
            {
            try
              {
//              { trailig extensions }
              for ( I = 0; I < Extensions->Count; I ++ )
                {
                Ext = ( TExtension* ) Extensions->Items[I];
                if ( (Ext != NULL) && (Ext->FExtType == etComment) )
                  {
                  if ( FImage->FComment->Count > 0 )
//                    FImage->FComment->Add(#13#10#13#10);
                    FImage->FComment->Add("\r\n\r\n");
                  FImage->FComment->AddStrings(Ext->FData);
                  }
                }
              }
            __finally
              {
              FreeExtensions(Extensions);
              }
            }
//          }
          else if ( ! ( SeparatorChar == CHR_TRAILER || SeparatorChar == '\0' ) )
            throw EInvalidGraphicOperation ( "Read Error" );
//            GifError(ResStr(SReadError));
          }
        }
      }
    __finally
      {
      delete Data;
      }
//    }

  if ( Count > 0 )
    {
    FFrameIndex = 0;
    if ( ForceDecode )
    try
      {
      GetBitmap (); // { force bitmap creation }
      }
      
    catch (...)
      {
      delete Frames[0];
      FItems->Delete(0);
      throw;
      }
    }

  PaletteModified = true;
  Changed(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SaveToClipboardFormat(Word &AFormat, int &AData, HPALETTE &APalette)
{
  TR( "TBMGIFImage::SaveToClipboardFormat" );

  TMemoryStream *Stream;
  HGLOBAL Data;
  Pointer Buffer;
  int I;
  
//  { !! check for ( gifclipboard format, mime type image/gif }
  if ( FItems->Count == 0 )
    return;
    
  Frames[0]->Bitmap->SaveToClipboardFormat(AFormat, AData, APalette);
  for ( I = 0; I < FItems->Count; I ++ )
//    with Frames[I] ) {
      if ( ( Frames[I]->FImage->FImageData == NULL ) || ( Frames[I]->FImage->FImageData->Size == 0) )
        return;
//    }
  Stream = new TMemoryStream;
  try
    {
    WriteStream(Stream, true);
    Stream->Position = 0;
    Data = GlobalAlloc(HeapAllocFlags, Stream->Size);
    try
      {
      if ( Data != 0 ) {
        Buffer = GlobalLock(Data);
        try
          {
          Stream->Read(Buffer, Stream->Size);
          SetClipboardData(CF_GIF, Data);
          }
        __finally
          {
          GlobalUnlock(Data);
          }
        }
      }
    catch (...)
      {
      GlobalFree(Data);
      throw;
      }
    }
  __finally
    {
    delete Stream;
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteData(Classes::TStream* Stream)
{
  TR( "TBMGIFImage::WriteData" );
  WriteStream(Stream, true);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetHeight(int Value)
{
  TR( "TBMGIFImage::SetHeight" );
  throw EInvalidGraphicOperation ( "Change GIF Size" );
//  GifError(LoadStr(SChangeGIFSize));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SetWidth(int Value)
{
  TR( "TBMGIFImage::SetWidth" );
  throw EInvalidGraphicOperation ( "Change GIF Size" );
//  GifError(LoadStr(SChangeGIFSize));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteSignature(TStream *Stream)
{
  TR( "TBMGIFImage::WriteSignature" );
  TBMGIFHeader Header;

  strcpy ( Header.Signature, GIFSignature.c_str () );
  Move(GIFVersionStr[FVersion].c_str (), Header.Version, 3);
  Stream->Write(&Header, sizeof(TBMGIFHeader));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteScreenDescriptor(TStream *Stream)
{
  TR( "TBMGIFImage::WriteScreenDescriptor" );

  Byte ColorResBits;
  TScreenDescriptor ScreenDesc;
  int I;

  UpdateScreenSize ();
//  with ScreenDesc ) {
    ScreenDesc.ScreenWidth = FScreenWidth;
    ScreenDesc.ScreenHeight = FScreenHeight;
    ScreenDesc.AspectRatio = FImage->FAspectRatio;
    ScreenDesc.PackedFields = 0;
    ScreenDesc.BackgroundColorIndex = 0;
    if ( FImage->FColorMap.Count > 0 )
      {
      ScreenDesc.PackedFields |= LSD_GLOBAL_COLOR_TABLE;
      ColorResBits = ColorsToBits((Word)FImage->FColorMap.Count);
      if ( FBackgroundColor != clNone )
        for ( I = 0; I < FImage->FColorMap.Count; I ++ )
          if ( ColorToRGB(FBackgroundColor) ==
            ItemToRGB(FImage->FColorMap.Colors[I]) )
            {
            ScreenDesc.BackgroundColorIndex = (Byte)I;
            break;
            }
            
      ScreenDesc.PackedFields = (Byte)(ScreenDesc.PackedFields + ((ColorResBits - 1) << 4) +
        (FImage->FBitsPerPixel - 1));
      }
//  }

//  Stream->Write(&ScreenDesc, sizeof(ScreenDesc));
  Stream->Write(&ScreenDesc, 7);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteDataBlock( TStream *Stream, TStrings *Data )
{
  TR( "TBMGIFImage::WriteDataBlock" );

    int I;
    AnsiString S;
    Byte BlockSize;

    for ( I = 0; I < Data->Count; I ++ )
      {
      S = Data->Strings [I];
      BlockSize = (Byte)min( S.Length(), 255);
      if ( BlockSize > 0 ) {
        Stream->Write(&BlockSize, sizeof(Byte));
        Stream->Write(S.c_str (), BlockSize);
      }
    }
    BlockSize = 0;
    Stream->Write(&BlockSize, sizeof(Byte));
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteExtensionBlock(TStream *Stream, TList *Extensions )
{
  TR( "TBMGIFImage::WriteExtensionBlock" );
  
    int I;
    TExtension *Ext;
    Byte ExtensionLabel;
    char SeparateChar;

    SeparateChar = CHR_EXT_INTRODUCER;
    for ( I = 0; I < Extensions->Count; I ++ )
      {
      Ext = (TExtension *) Extensions->Items[I];
      if ( Ext != NULL ) {
        Stream->Write(&SeparateChar, sizeof(Byte));
        ExtensionLabel = ExtLabels[Ext->FExtType];
        Stream->Write(&ExtensionLabel, sizeof(Byte));
        switch ( Ext->FExtType )
          {
          case etGraphic:
            {
            Stream->Write(&Ext->FExtRec.GCE, sizeof(TGraphicControlExtension));
            break;
            }

          case etComment:
            WriteDataBlock(Stream, Ext->FData);
            break;

          case etPlainText:
            {
//            Stream->Write(&Ext->FExtRec.PTE, sizeof(TPlainTextExtension));
            Stream->Write(&Ext->FExtRec.PTE, 13);
            WriteDataBlock(Stream, Ext->FData);
            break;
            }
            
          case etApplication:
            {
            Stream->Write(&Ext->FExtRec.APPE, sizeof(TAppExtension));
            WriteDataBlock(Stream, Ext->FData);
            break;
            }
            
          }
        }
      }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::WriteStream(Classes::TStream* Stream, bool WriteSize)
{
//  WriteSize = true; // ??????????
  TR( "TBMGIFImage::WriteStream" );

  char Separator;
  Byte Temp;
  int FrameNo;
  TBMGIFFrame *Frame;
  TMemoryStream *Mem;
  long int Size;
  TStringList *StrList;
  
  if ( FItems->Count == 0 )
    throw EInvalidGraphicOperation ( "No GIF Data" );
//    GifError(LoadStr(SNoGIFData));
    
  EncodeFrames(false);
  Mem = new TMemoryStream;
  try
    {
    if ( FImage->FComment->Count > 0 )
      FVersion = TBMGIFData::gv89a;
      
    WriteSignature(Mem);
    WriteScreenDescriptor(Mem);
    if ( FImage->FColorMap.Count > 0 )
      {
//      with FImage->FColorMap )
      Mem->Write(&FImage->FColorMap.Colors[0], FImage->FColorMap.Count * sizeof(TBMGIFColorItem));
      }
      
    if ( FLooping && (FItems->Count > 1) )
      {
//      { write looping extension }
      Separator = CHR_EXT_INTRODUCER;
      Mem->Write(&Separator, sizeof(Byte));
      Temp = ExtLabels[etApplication];
      Mem->Write(&Temp, sizeof(Byte));
      Temp = sizeof(TAppExtension) - sizeof(Byte);
      Mem->Write(&Temp, sizeof(Byte));
      Mem->Write(LoopExt, Temp);
      StrList = new TStringList;
      try
        {
        StrList->Add(Char(AE_LOOPING) + Char( FRepeatCount & 0xFF ) +
          Char(HiByte(FRepeatCount)));
        WriteDataBlock(Mem, StrList);
        }
      __finally
        {
        delete StrList;
        }
      }
//    }
    Separator = CHR_IMAGE_SEPARATOR;
    for ( FrameNo = 0; FrameNo < FItems->Count; FrameNo ++ )
      {
      Frame = ( TBMGIFFrame* )FItems->Items[FrameNo];
      if ( Frame->FExtensions != NULL )
        WriteExtensionBlock(Mem, Frame->FExtensions);
      Mem->Write(&Separator, sizeof(Byte));
      Frame->WriteImageDescriptor(Mem);
      Frame->WriteLocalColorMap(Mem);
      Frame->WriteRasterData(Mem);
    }
    if ( FImage->FComment->Count > 0 ) {
      Separator = CHR_EXT_INTRODUCER;
      Mem->Write(&Separator, sizeof(Byte));
      Temp = ExtLabels[etComment];
      Mem->Write(&Temp, sizeof(Byte));
      WriteDataBlock(Mem, FImage->FComment);
    }
    Separator = CHR_TRAILER;
    Mem->Write(&Separator, sizeof(Byte));
    Size = Mem->Size;
    if ( WriteSize )
      Stream->Write(&Size, sizeof(Size));
      
    Stream->Write(Mem->Memory, Size);
    }
    
  __finally
    {
    delete Mem;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::Grayscale(bool ForceEncoding)
{
  TR( "TBMGIFImage::Grayscale" );

  int I;

  if ( FItems->Count == 0 )
    throw EInvalidGraphicOperation ( "No GIF Data" );
//    GifError(LoadStr(SNoGIFData));
    
  for ( I = 0; I < FItems->Count - 1; I ++ )
    Frames[I]->GrayscaleImage(ForceEncoding);
    
  if ( FBackgroundColor != clNone )
    FBackgroundColor = GrayColor(FBackgroundColor);
//{$IFDEF RX_D3}
  PaletteModified = true;
//{$ENDIF}
  Changed(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::SaveToStream(Classes::TStream* Stream)
{
  TR( "TBMGIFImage::SaveToStream" );
  
  WriteStream(Stream, false);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFImage::DoProgress(Graphics::TProgressStage Stage, Byte PercentDone, const System::AnsiString Msg)
{
  Progress(this, Stage, PercentDone, false, Rect(0, 0, 0, 0), Msg);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMImageControl::TBMImageControl ( TComponent *AOwner ) :
  TGraphicControl ( AOwner )
{
  TR( "TBMImageControl::TBMImageControl" );

  ControlStyle = ControlStyle << csClickEvents;
  ControlStyle = ControlStyle << csCaptureMouse;
  ControlStyle = ControlStyle << csOpaque;
  ControlStyle = ControlStyle << csReplicatable;
  ControlStyle = ControlStyle << csDoubleClicks;
  
  Height = 105;
  Width = 105;
  ParentColor = true;
}
//---------------------------------------------------------------------------
void __fastcall TBMImageControl::PaintImage ()
{
  bool Save;

  Save = FDrawing;
  FDrawing = true;
  try
    {
    DoPaintImage ();
    }
  __finally
    {
    FDrawing = Save;
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMImageControl::PaintDesignRect ()
{
  if ( ComponentState.Contains ( csDesigning ))
    {
//    with Canvas do begin
      Canvas->Pen->Style = psDash;
      Canvas->Brush->Style = bsClear;
      Canvas->Rectangle(0, 0, Width, Height);
    }
}
//---------------------------------------------------------------------------
bool __fastcall TBMImageControl::DoPaletteChange(void)
{
  TCustomForm *ParentForm;
  TGraphic *Tmp;

  bool Result = false;
  Tmp = FGraphic;
  if (( Visible && ( ! ( ComponentState.Contains ( csLoading ) )) && (Tmp != NULL)
    && (Tmp->PaletteModified) ))
    
    {
    if ( GetPalette () != 0)
      {
      ParentForm = GetParentForm(this);
      if ( ParentForm && ParentForm->Active && ParentForm->HandleAllocated () )
        {
        if ( FDrawing )
          ParentForm->Perform(WM_QUERYNEWPALETTE, 0, 0);
          
        else
          PostMessage(ParentForm->Handle, WM_QUERYNEWPALETTE, 0, 0);

        Result = true;
//{$IFDEF RX_D3}
        Tmp->PaletteModified = false;
//{$ENDIF}
        }
      }
//{$IFDEF RX_D3}
    else
      {
      Tmp->PaletteModified = false;
     }
//{$ENDIF}
    }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBMImageControl::PictureChanged ()
{
  if (FGraphic != NULL)
    if ( DoPaletteChange () && FDrawing )
      Update ();

  if ( ! FDrawing )
    Invalidate ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TBMGIFControl::TBMGIFControl(Classes::TComponent* AOwner) :
  inherited ( AOwner )
{
  CRITICAL ( CriticalSection );
  LocalCritical = new TCriticalSection;
  {
  CRITICAL ( LocalCritical );
  RestoreCount = 1;
  Delay = 10;
  FImage = new TBMGIFImage;
  FGraphic = FImage;
  FImage->OnChange = ImageChanged;
  FAutoSize = true;
  FLoop = true;
  FTransparent = true;

  BmGifsList->Add ( this );
  }
}
//---------------------------------------------------------------------------
__fastcall TBMGIFControl::~TBMGIFControl()
{
  CRITICAL ( CriticalSection );
  {
  CRITICAL ( LocalCritical );
  BmGifsList->Remove ( this );

  FOnStart = NULL;
  FOnStop = NULL;
  FOnChange = NULL;
  FOnFrameChanged = NULL;
  Animate = false;
  FImage->OnChange = NULL;
  delete FImage;
  }

  delete LocalCritical;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::AdjustBounds ()
{
  CRITICAL ( LocalCritical );
  if ( ! ( ComponentState.Contains ( csReading ) ) )
    {
    if ( FAutoSize && ! FImage->Empty )
      SetBounds(Left, Top, FImage->ScreenWidth, FImage->ScreenHeight);
    }
}
//---------------------------------------------------------------------------
Word __fastcall TBMGIFControl::GetDelayTime(int Index)
{
  CRITICAL ( LocalCritical );
  if ( (FFrameIndex >= 0) && (FFrameIndex < FImage->Count) &&
    (FImage->Count > 1) )
    return max(FImage->Frames[FFrameIndex]->AnimateInterval, (unsigned short)1);

  return 0;
}
//---------------------------------------------------------------------------
HPALETTE __fastcall TBMGIFControl::GetPalette(void)
{
  CRITICAL ( LocalCritical );
  if ( ! FImage->Empty )
    return FImage->Palette;
    
  return 0;
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::ImageChanged(System::TObject* Sender)
{
  CRITICAL ( LocalCritical );
  AdjustBounds ();
  FFrameIndex = FImage->FrameIndex;
  if ( (FFrameIndex >= 0) && (FImage->Count > 0) )
    Delay = GetDelayTime(FFrameIndex);
  Change ();
  PictureChanged ();
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetImage(TBMGIFImage* Value)
{
  CRITICAL ( LocalCritical );
  FImage->Assign(Value);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetCenter(bool Value)
{
  CRITICAL ( LocalCritical );
  if ( Value != FCenter )
    {
    FCenter = Value;
    PictureChanged ();
    if ( Animate )
      Repaint ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetStretch(bool Value)
{
  CRITICAL ( LocalCritical );
  if ( Value != FStretch )
    {
    FStretch = Value;
    PictureChanged ();
    if ( Animate )
      Repaint ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetTransparent(bool Value)
{
  CRITICAL ( LocalCritical );
  if ( Value != FTransparent )
    {
    FTransparent = Value;
    PictureChanged ();
    if ( Animate )
      Repaint ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetFrameIndex(int Value)
{
  CRITICAL ( LocalCritical );
  if ( Value != FFrameIndex )
    {
    if ( (Value < FImage->Count) && (Value >= 0) )
      {
      FFrameIndex = Value;
      if ( (FFrameIndex >= 0) && (FImage->Count > 0) )
        Delay = GetDelayTime(FFrameIndex);

      FrameChanged ();
      PictureChanged ();
      if ( !FAnimate )
        Reduce ();
      }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::Reduce ()
{
  CRITICAL ( LocalCritical );
  for ( int I = 0; I < FImage->FItems->Count; I ++ )
    if ( I != FFrameIndex )
      ((TBMGIFFrame*)FImage->FItems->Items[I])->Reduce ();
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::DoPaintImage(void)
{
  CRITICAL ( LocalCritical );
  if ( ClientWidth == 0 || ClientHeight == 0 )
    return;
    
  Graphics::TBitmap *TmpImage, *Frame, *MaskFrame = NULL;
  TRect Dest;
// Transparency  TColor TransColor;
  TmpImage = new Graphics::TBitmap;

  try
    {
//    with TmpImage do {
      TmpImage->Width = ClientWidth;
      TmpImage->Height = ClientHeight;
      TmpImage->Canvas->Brush->Color = Color;
      TmpImage->Canvas->FillRect(Bounds(0, 0, TmpImage->Width, TmpImage->Height));
//      { copy image from parent and back-level controls }
// Transparency       if ( FImage->Transparent || FImage->Empty )
      if ( FTransparent ) // Transparency
        CopyParentImage(this, TmpImage->Canvas);
      if ( ! FImage->Empty && (FImage->ScreenWidth > 0) &&
        (FImage->ScreenHeight> 0) )
      {
// Transparency        TransColor = clNone;
// Transparency        Frame = GetFrameBitmap(FrameIndex, TransColor);
        Frame = FImage->Frames [ FrameIndex ]->GetFullBitmap ();
        MaskFrame = FImage->Frames [ FrameIndex ]->GetFullMaskBitmap ();

//        Frame = FImage->GetFrameBitmap(FrameIndex, MaskFrame);
//        try
//          {
          if ( FStretch )
            Dest = ClientRect;
            
          else if ( FCenter )
            Dest = Bounds(( ClientWidth - Frame->Width) / 2,
              (ClientHeight - Frame->Height) / 2,
              Frame->Width, Frame->Height);
              
          else
            Dest = Rect(0, 0, Frame->Width, Frame->Height);
            
//          if ( (TransColor == clNone) || ! FTransparent )
/*
          if ( ! FTransparent )
            TmpImage->Canvas->StretchDraw(Dest, Frame);
          else
*/
            {
/* Transparency
            StretchBitmapRectTransparent(TmpImage->Canvas, Dest.Left, Dest.Top,
              WidthOf( Dest), HeightOf(Dest), Bounds(0, 0, Frame->Width,
              Frame->Height), Frame, TransColor);
*/

//            Frame->SaveToFile ( "Image.bmp" );
//            MaskFrame->SaveToFile ( "Mask.bmp" );
            StretchBitmapRectTransparentMask(TmpImage->Canvas, Dest.Left, Dest.Top,
              WidthOf( Dest), HeightOf(Dest), Bounds(0, 0, Frame->Width,
              Frame->Height), Frame, MaskFrame );
//            TmpImage->SaveToFile ( "Result.bmp" );
//            exit ( 0 );

/*
            StretchBitmapRectTransparent(TmpImage->Canvas, Dest.Left, Dest.Top,
              WidthOf( Dest), HeightOf(Dest), Bounds(0, 0, Frame->Width,
              Frame->Height), MaskFrame, clGreen );
*/
            }
          }
/*
        __finally
          {
//          delete Frame;
//          if ( MaskFrame )
//            delete MaskFrame;
          }
*/
//        }
//      }
//    }
//    TmpImage->Canvas->Draw(ClientRect.Left, ClientRect.Top, TmpImage);
    Canvas->Draw(ClientRect.Left, ClientRect.Top, TmpImage);
    }
  __finally
    {
    delete TmpImage;
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::Paint ()
{
  CRITICAL ( LocalCritical );
  PaintImage ();
  if ( (FImage->Transparent || FImage->Empty) )
    PaintDesignRect ();
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::OnTimer ()
{
  CRITICAL ( LocalCritical );
  if ( ! FAnimate )
    return;

  if ( Delay )
    Delay --;

  else
    {
    CRITICAL (CriticalSection);
    TimerExpired();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::TimerExpired()
{
  CRITICAL ( LocalCritical );
  if ( ! RestoreCount -- )
    {
//    (( TBMGIFFrame *)FImage->FItems->Items [ FFrameIndex ] )->Reduce ();
    RestoreCount = 2 * FImage->FItems->Count - 1;
    }

  TCustomForm *F;

  if ( Visible && (FImage->Count > 1) )
    {
    if ( FFrameIndex < FImage->Count - 1 )
      FFrameIndex ++;

    else FFrameIndex = 0;
    FTimerRepaint = true;
    try
      {
      FrameChanged ();
      Repaint ();
      }
      
    __finally
      {
      FTimerRepaint = false;
      if ( (FFrameIndex >= 0) && (FFrameIndex < FImage->Count) )
        Delay = GetDelayTime(FFrameIndex);
      }
      
    if ( ! FLoop && (FFrameIndex == 0) )
      {
      SetAnimate(false);
      if ( ( ComponentState.Contains ( csDesigning ) ) )
        {
        F = GetParentForm(this);
        if ( (F != NULL) && ( F->Designer != NULL) )
          F->Designer->Modified ();
        }
      }
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::Change ()
{
  CRITICAL ( LocalCritical );
  if ( FOnChange )
    FOnChange(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::FrameChanged ()
{
  CRITICAL ( LocalCritical );
  if ( FOnFrameChanged)
    FOnFrameChanged(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::Stop ()
{
  CRITICAL ( LocalCritical );
  if ( FOnStop)
    FOnStop(this);

  Reduce ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::Start ()
{
  CRITICAL ( LocalCritical );
  if ( FOnStart)
    FOnStart(this);
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetAutoSize( bool Value )
{
  CRITICAL ( LocalCritical );
  if ( Value != FAutoSize )
    {
    FAutoSize = Value;
    AdjustBounds ();
    PictureChanged ();
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::SetAnimate( bool Value )
{
  CRITICAL ( LocalCritical );
  if ( FAnimate != Value )
    {
    if ( Value )
      {
      FAnimate = true;
      Delay = 10;
      Start ();
      }

    else
      {
      FAnimate = false;
      Stop ();
      PictureChanged ();
      }
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMGIFControl::WMSize(Messages::TWMSize &Message)
{
  CRITICAL ( LocalCritical );
  inherited::Dispatch( &Message );
  AdjustBounds ();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
/*
class TPictEditor : public Classes::TComponent
{
	typedef Classes::TComponent inherited;

private:
	System::TMetaClass*FGraphicClass;
	Graphics::TPicture* FPicture;
//	TPictureEditDialog* FPicDlg;
	bool FDecreaseColors;
	void __fastcall SetPicture(Graphics::TPicture* Value);
	void __fastcall SetGraphicClass(System::TMetaClass* Value);

public:
	__fastcall virtual TPictEditor(Classes::TComponent* AOwner);
	__fastcall virtual ~TPictEditor(void);
	bool __fastcall Execute(void);
//	__property TPictureEditDialog* PicDlg = {read=FPicDlg};
	__property System::TMetaClass* GraphicClass = {read=FGraphicClass, write=SetGraphicClass};
	__property Graphics::TPicture* Picture = {read=FPicture, write=SetPicture};
};
//---------------------------------------------------------------------------
class TPictProperty : public Dsgnintf::TPropertyEditor
{
	typedef Dsgnintf::TPropertyEditor inherited;

public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual System::AnsiString __fastcall GetValue(void);
	virtual void __fastcall SetValue(const System::AnsiString Value);
public:
	__fastcall virtual ~TPictProperty(void) { }

public:
	__fastcall TPictProperty(void) : Dsgnintf::TPropertyEditor() { }

};
//---------------------------------------------------------------------------
void __fastcall TPictProperty::Edit ()
{
  TPictEditor *PictureEditor;
  TPersistent *Comp;

  PictureEditor = new TPictEditor ( NULL );
  try
    {
    Comp = GetComponent(0);
    if ( dynamic_cast<TComponent *> (Comp) )
      PictureEditor.FPicDlg.Caption = TComponent(Comp).Name + '.' + GetName;

    PictureEditor.Picture = TPicture(Pointer(GetOrdValue));
    if PictureEditor.Execute then begin
      SetOrdValue(Longint(PictureEditor.Picture));
      if (PictureEditor.Picture.Graphic <> nil) and
        (PictureEditor.Picture.Graphic.ClassType = TBitmap) and
        PictureEditor.FDecreaseColors then
        SetBMPColors(TPicture(Pointer(GetOrdValue)).Bitmap);
    end;
  finally
    PictureEditor.Free;
  end;
end;

function TPictProperty.GetAttributes: TPropertyAttributes;
begin
  Result = [paDialog];
end;

function TPictProperty.GetValue: string;
var
  Picture: TPicture;
begin
  Picture = TPicture(GetOrdValue);
  if Picture.Graphic = nil then
    Result = ResStr(srNone)
  else Result = '(' + Picture.Graphic.ClassName + ')';
end;

procedure TPictProperty.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;
//---------------------------------------------------------------------------
class TGraphicPropertyEditor : public Dsgnintf::TClassProperty
{
	typedef Dsgnintf::TClassProperty inherited;

public:
	virtual void __fastcall Edit(void);
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual System::AnsiString __fastcall GetValue(void);
	virtual void __fastcall SetValue(const System::AnsiString Value);
public:
	__fastcall virtual ~TGraphicPropertyEditor(void) { }

public:
	__fastcall TGraphicPropertyEditor(void) : Dsgnintf::TClassProperty() { }

};
//---------------------------------------------------------------------------
procedure TGraphicPropertyEditor.Edit;
var
  PictureEditor: TPictEditor;
  Comp: TPersistent;
begin
  PictureEditor = TPictEditor.Create(nil);
  try
    Comp = GetComponent(0);
    if Comp is TComponent then
      PictureEditor.FPicDlg.Caption = TComponent(Comp).Name + '.' + GetName
    else PictureEditor.FPicDlg.Caption = GetName;
    PictureEditor.GraphicClass = TGraphicClass(GetTypeData(GetPropType)^.ClassType);
    PictureEditor.Picture.Graphic = TGraphic(Pointer(GetOrdValue));
    if PictureEditor.Execute then
      if (PictureEditor.Picture.Graphic = nil) or
         (PictureEditor.Picture.Graphic is PictureEditor.GraphicClass) then
        SetOrdValue(LongInt(PictureEditor.Picture.Graphic))
      else raise Exception.Create(ResStr(SInvalidPropertyValue));
  finally
    PictureEditor.Free;
  end;
end;
*/
//---------------------------------------------------------------------------
class TGraphicsEditor : public Dsgnintf::TDefaultEditor
{
	typedef Dsgnintf::TDefaultEditor inherited;

public:
	virtual void __fastcall EditProperty(Dsgnintf::TPropertyEditor* PropertyEditor, bool &Continue, bool
		&FreeEditor);
public:
	__fastcall virtual TGraphicsEditor(Classes::TComponent* AComponent, Dsgnintf::TFormDesigner*
		ADesigner) : Dsgnintf::TDefaultEditor(AComponent, ADesigner) { }

public:
	__fastcall virtual ~TGraphicsEditor(void) { }

};
//---------------------------------------------------------------------------
void __fastcall TGraphicsEditor::EditProperty(Dsgnintf::TPropertyEditor* PropertyEditor, bool &Continue, bool &FreeEditor )
{
  TR( "TGraphicsEditor::EditProperty" );
  AnsiString PropName;

  PropName = PropertyEditor->GetName ();
/*
  if ( PropName == "PICTURE" ||
       PropName == "IMAGE" ||
       PropName == "GLYPH" )
*/
  if ( PropName == "Picture" ||
       PropName == "Image" ||
       PropName == "Glyph" )
    {
    TR( "PropertyEditor->Edit" );
    PropertyEditor->Edit ();
    Continue = false;
    }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  void __fastcall PACKAGE Register()
	{
	TComponentClass classes1[1] = {__classid(TBMGIFControl)};
    RegisterComponents("BMitov", classes1, 0);
    RegisterComponentEditor(classes1[0], __classid(TGraphicsEditor) );

//    RegisterPropertyEditor(__typeinfo(TPicture), NULL, "", __classid(TPictProperty));
//    RegisterPropertyEditor(__typeinfo(TGraphic), NULL, "", __classid(TGraphicPropertyEditor));
//    RegisterComponentEditor(__classid(TImage), __classid(TGraphicsEditor));

    
/*
  	TComponentClass classes2[2] = {__classid(TBMGIFFrame), __classid(TBMGIFImage) };
    RegisterClasses(classes2, 1 );

    TPicture::RegisterFileFormat(__classid(TPicture), "gif", "GIF Image", __classid(TBMGIFImage));
    CF_GIF = (Word)RegisterClipboardFormat( "GIF Image" );
    TPicture::RegisterClipboardFormat( __classid(TPicture), CF_GIF, __classid(TBMGIFImage));
*/
/*
  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TGraphicsEditor);
*/
//    RegisterComponentEditor(classes1[0], __classid(TBMPageControlEditor));
//    RegisterComponentEditor(classes2[0], __classid(TBMPageControlEditor));
	}
};
//---------------------------------------------------------------------------
void __fastcall TGraphic::SetPalette( HPALETTE Value )
{
}
//---------------------------------------------------------------------------


