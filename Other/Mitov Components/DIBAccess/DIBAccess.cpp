//---------------------------------------------------------------------------
#include <vcl.h>
#include <stdlib.h>
#include <assert.h>
#pragma hdrstop

#include "DIBAccess.h"

#if (__BORLANDC__ >= 0x0530)
//---------------------------------------------------------------------------
// BCB 3.0
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TDIBAccess *)
{
    new TDIBAccess(NULL);
}
//---------------------------------------------------------------------------
__fastcall TDIBAccess::TDIBAccess(TComponent* Owner)
    : TComponent(Owner),
    Bitmap ( NULL ),
    FImage ( NULL ),
    RemoveNotifier ( NULL ),
    MaxLineAccessed ( 0 ),
    MinLineAccessed ( 1000 )
{
  Correct = false;
  InfoHeaderSize = 0;
  ImageSize = 0;
}
//---------------------------------------------------------------------------
__fastcall TDIBAccess::~TDIBAccess()
{
  if ( FImage )
    {
    if ( CountBeginUpdate )
      if ( SameBitmap () )
        SetDIB ( Bitmap->Palette, BitmapInfo, DIBArray );

    assert ( RemoveNotifier );
    if ( RemoveNotifier )
      {
      RemoveNotifier->LinkedTo = NULL;
      delete RemoveNotifier;
      RemoveNotifier = NULL;
      }

    if ( DIBArray )
      {
      delete DIBArray;
      DIBArray = NULL;
      delete BitmapInfo;
      BitmapInfo = NULL;
      }
    }
  }
//---------------------------------------------------------------------------
bool __fastcall TDIBAccess::SameBitmap ()
{
  if ( ! Correct )
    return false;

  if ( OldHandle == HBitmap && OldHeight == Height && OldWidth == Width )
    return true;

  if ( DIBArray )
    {
    delete DIBArray;
    DIBArray = NULL;
    delete BitmapInfo;
    BitmapInfo = NULL;
    }

  Correct = false;
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::ShootImage ()
{
  OldHandle = HBitmap;
  OldHeight = Height;
  OldWidth  = Width;
  Correct = true;
}
//---------------------------------------------------------------------------
/*
bool __fastcall TDIBAccess::CheckImage ()
{
  if ( HBitmap )
    if ( HBitmap == OldHandle )
      if ( Height == OldHeight )
        if ( Width == OldWidth )
          return true;

  return false;  // The Bitmap has been changed
}
*/
//---------------------------------------------------------------------------
HBITMAP __fastcall TDIBAccess::GetHBitmap ()
{
  if ( Image->Picture->Bitmap )
    return Bitmap->Handle;

  return NULL;
}
//---------------------------------------------------------------------------
Graphics::TBitmap * __fastcall TDIBAccess::GetBitmap ()
{
  return Image->Picture->Bitmap;
}
//---------------------------------------------------------------------------
inline long __fastcall BytesPerScanline ( long PixelsPerScanline, long BitsPerPixel, long Alignment )
{
  Alignment --;
  long Result = ((PixelsPerScanline * BitsPerPixel) + Alignment) & ( ~Alignment );
  return Result / 8;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::InitializeBitmapInfoHeader ( TBitmapInfoHeader &BI )
{
  TDIBSection DS;
  int Bytes;

  DS.dsBmih.biSize = 0;

  Bytes = GetObject ( HBitmap, sizeof ( DS ), &DS);

  if ( Bytes == 0 )
    throw new EInvalidGraphic ( "Bad bmp" );

  else if (( Bytes >= (sizeof(DS.dsBm) + sizeof(DS.dsBmih))) &&
    (DS.dsBmih.biSize >= sizeof(DS.dsBmih)) )
    {
    BI = DS.dsBmih;
    BI.biHeight = -BI.biHeight;
    }

  else
    {
    memset ( &BI, sizeof(BI), 0 );
    BI.biSize = sizeof ( BI );
    BI.biWidth = DS.dsBm.bmWidth;
    BI.biHeight = -DS.dsBm.bmHeight;
    }

  BI.biBitCount = 32;
//  BI.biBitCount = 24;

  BI.biPlanes = 1;
  BI.biSizeImage = 0;
  BI.biClrUsed = 0;
  BI.biClrImportant = 0;
  if ( BI.biSizeImage == 0 )
    BI.biSizeImage = Graphics::BytesPerScanline ( BI.biWidth, BI.biBitCount, 32 ) * abs ( BI.biHeight );
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::ReverseColor ()
{
  RGBQUAD * RGBArray = (RGBQUAD *)DIBArray;
  for ( unsigned int i = 0; i < ImageSize / sizeof ( TColor ); i ++ )
    {
    BYTE b = RGBArray [ i ].rgbRed; 
    RGBArray [ i ].rgbRed = RGBArray [ i ].rgbBlue; 
    RGBArray [ i ].rgbBlue = b; 
    }
    
}
//---------------------------------------------------------------------------
bool __fastcall TDIBAccess::SetDIB ( HPALETTE Palette, void *BitmapInfo, void *Bits )
{
  if ( MaxLineAccessed < MinLineAccessed )
    return true;

  ReverseColor ();

  HPALETTE OldPal;
  HDC DC;
  InitializeBitmapInfoHeader ( *(( TBitmapInfoHeader *) BitmapInfo ) );
  OldPal = 0;
  DC = CreateCompatibleDC(0);

  bool Result = false;

  try
    {
    if ( Palette )
      {
      OldPal = SelectPalette( DC, Palette, false );
      RealizePalette ( DC );
      }


    Result = SetDIBits ( DC, HBitmap, 0, -(( TBitmapInfoHeader *) BitmapInfo )->biHeight, Bits,
      (( TBitmapInfo *) BitmapInfo ), DIB_RGB_COLORS) != 0;
    }

  catch (...) {}

  if ( OldPal )
    SelectPalette( DC, OldPal, false );
    
  DeleteDC(DC);

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TDIBAccess::GetDIB( HPALETTE Palette, void *BitmapInfo, void *Bits )
{
  HPALETTE OldPal;
  HDC DC;
  InitializeBitmapInfoHeader ( *(( TBitmapInfoHeader *) BitmapInfo ) );
  OldPal = 0;
  DC = CreateCompatibleDC(0);

  bool Result = false;

  try
    {
    if ( Palette )
      {
      OldPal = SelectPalette( DC, Palette, false );
      RealizePalette ( DC );
      }

    Result = GetDIBits ( DC, HBitmap, 0, -(( TBitmapInfoHeader *) BitmapInfo )->biHeight, Bits,
      (( TBitmapInfo *) BitmapInfo ), DIB_RGB_COLORS) != 0;
    }

  catch (...) {}

  if ( OldPal )
    SelectPalette( DC, OldPal, false );

  DeleteDC(DC);


  MaxLineAccessed = 0; 
  MinLineAccessed = Height;

  if ( Result )
    ReverseColor ();
    
  ShootImage ();
  
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::GetDIBSizes ()
{
  TBitmapInfoHeader BI;

  InitializeBitmapInfoHeader ( BI );
  
  if ( BI.biBitCount > 8 )
    {
    InfoHeaderSize = sizeof ( TBitmapInfoHeader );
    if ((BI.biCompression & BI_BITFIELDS ) != 0 )
      InfoHeaderSize += 12;

    }

  else
    InfoHeaderSize = sizeof ( TBitmapInfoHeader ) + sizeof ( TRGBQuad ) *
      ( 1 << BI.biBitCount );

  ImageSize = BI.biSizeImage;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::CheckCreation ()
{
  if ( HBitmap )
    {
    int CurrentInfoHeaderSize = InfoHeaderSize;
    int CurrentImageSize = ImageSize;

    GetDIBSizes ();
    if ( CurrentInfoHeaderSize != InfoHeaderSize )
      {
      if ( BitmapInfo )
        delete BitmapInfo;
        
      BitmapInfo = new char [ InfoHeaderSize ];
      }

    if ( CurrentImageSize != ImageSize )
      {
      if ( DIBArray )
        delete DIBArray;
        
      DIBArray = new TColor [ ImageSize / sizeof ( TColor ) ];
      }
    }
    
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::BeginUpdate(void)
{
  if ( ! CountBeginUpdate )
    {
    CheckCreation ();
    if ( HBitmap )
      GetDIB( Bitmap->Palette, BitmapInfo, DIBArray );
    }

  CountBeginUpdate ++;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::EndUpdate(void)
{
  if ( CountBeginUpdate )
    CountBeginUpdate --;

  if ( ! CountBeginUpdate )
    {
    if ( SameBitmap () )
      SetDIB ( Bitmap->Palette, BitmapInfo, DIBArray );
    }
}
//---------------------------------------------------------------------------
TColor __fastcall TDIBAccess::GetPixel(int X, int Y)
{
  if ( ! HBitmap )
    return clBlack;

  if ( CountBeginUpdate )
    {
    if ( ! SameBitmap ())
      return clBlack;

    if (( X >= 0 && X < Bitmap->Width ) &&
        ( Y >= 0 && Y < Bitmap->Height ))
      return DIBArray [ Bitmap->Width * Y + X ];             
    }

  else
    return Bitmap->Canvas->Pixels [ X ] [ Y ];
    
  return clBlack;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::SetPixel(int X, int Y, TColor Value)
{
  if ( ! HBitmap )
    return;

  if ( CountBeginUpdate )
    {
    if ( ! SameBitmap ())
      return;
      
    if (( X >= 0 && X < Bitmap->Width ) &&
        ( Y >= 0 && Y < Bitmap->Height ))
      {
      DIBArray [ Bitmap->Width * Y + X ] = Value;
      if ( Y > MaxLineAccessed )
        MaxLineAccessed = Y;
          
      if ( Y < MinLineAccessed )
        MinLineAccessed = Y;
      }
    }

  else
    Bitmap->Canvas->Pixels [ X ] [ Y ] = Value;
}
//---------------------------------------------------------------------------
int __fastcall TDIBAccess::GetWidth ()
{
  if ( FImage )
    return FImage->Width;

  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TDIBAccess::GetHeight ()
{
  if ( FImage )
    return FImage->Height;

  return 0;
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::ImageRemoveNotification ()
{
  assert ( FImage != NULL );
  
  if ( FImage )
    {
    delete DIBArray;
    DIBArray = NULL;
    delete BitmapInfo;
    BitmapInfo = NULL;
    RemoveNotifier = NULL;
    FImage = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TDIBAccess::SetImage ( TImage *_Image )
{
  if ( _Image == FImage )
    return;

  if ( FImage )
    {
    assert ( RemoveNotifier );
    if ( RemoveNotifier )
      {
      RemoveNotifier->LinkedTo = NULL;
      delete RemoveNotifier;
      RemoveNotifier = NULL;
      }

    if ( CountBeginUpdate )
      if ( SameBitmap () )
        SetDIB ( Bitmap->Palette, BitmapInfo, DIBArray );

    if ( DIBArray )
      {
      delete DIBArray;
      DIBArray = NULL;
      delete BitmapInfo;
      BitmapInfo = NULL;
      }
    }

  FImage = _Image;
  if ( FImage )
    {
    assert ( RemoveNotifier == NULL );
    RemoveNotifier = new TRemoveNotifier ( FImage );
    RemoveNotifier->Parent = FImage->Parent;
    RemoveNotifier->LinkedTo = this;

//    Bitmap = FImage->Picture->Bitmap;
//    HBitmap = Bitmap->Handle;

/*
    if ( HBitmap )
      {
      GetDIBSizes ();
      BitmapInfo = new char [ InfoHeaderSize ];
      DIBArray = new TColor [ ImageSize / sizeof ( TColor ) ];
/{
      if ( CountBeginUpdate )
        GetDIB ( Bitmap->Palette, BitmapInfo, DIBArray );
}/
      }
*/
    }

}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TRemoveNotifier::TRemoveNotifier ( TComponent* Owner ) :
  inherited ( Owner )
{
}
//---------------------------------------------------------------------------
__fastcall TRemoveNotifier::~TRemoveNotifier ()
{
  if ( LinkedTo )
    LinkedTo->ImageRemoveNotification ();

}
//---------------------------------------------------------------------------
namespace Dibaccess
{
    void __fastcall PACKAGE Register()
    {
        TComponentClass classes[1] = {__classid(TDIBAccess)};
        RegisterComponents("BMitov", classes, 0);
    }
}
//---------------------------------------------------------------------------
