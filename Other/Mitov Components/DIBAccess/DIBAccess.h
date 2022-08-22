/*> Ver: V1.1 *************      History      *****************************\

Beta V1.0b0		07/08/1998		Released
Beta V1.0b1		05/18/1999		Memory management has been changed.
Beta V1.1		12/06/2004		Memory management has been changed.

Legal issues: Copyright (C) 1997 - 2005 by Boian Mitov
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
#ifndef DIBAccessH
#define DIBAccessH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
#if (__BORLANDC__ < 0x0530)
//---------------------------------------------------------------------------
// BCB 1.0
//---------------------------------------------------------------------------
  #define PACKAGE

#endif
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
class TRemoveNotifier;
//---------------------------------------------------------------------------
class PACKAGE TDIBAccess : public TComponent
{
  friend class TRemoveNotifier;
  
private:
protected:
  int InfoHeaderSize;
  int ImageSize;

  int CountBeginUpdate;

  TImage                *FImage;

  HBITMAP   OldHandle;
  int       OldHeight;
  int       OldWidth;

  bool      Correct;

  TColor    *DIBArray;
  void      *BitmapInfo;

  int        MaxLineAccessed;
  int        MinLineAccessed;

  TRemoveNotifier   *RemoveNotifier;

protected:
  void __fastcall InitializeBitmapInfoHeader ( TBitmapInfoHeader &BI );
  bool __fastcall SetDIB( HPALETTE Palette, void *BitmapInfo, void *Bits );
  bool __fastcall GetDIB( HPALETTE Palette, void *BitmapInfo, void *Bits );
  void __fastcall GetDIBSizes ();
  void __fastcall ReverseColor ();
  void __fastcall ShootImage ();
  bool __fastcall SameBitmap ();
  void __fastcall CheckCreation ();
//  bool __fastcall CheckImage ();

  void __fastcall ImageRemoveNotification ();

protected:
  TColor __fastcall GetPixel(int X, int Y);
  void __fastcall SetPixel(int X, int Y, TColor Value);
  void __fastcall SetImage ( TImage *_Image );
  Graphics::TBitmap *__fastcall GetBitmap ();
  HBITMAP __fastcall GetHBitmap ();

  int __fastcall GetWidth ();
  int __fastcall GetHeight ();

protected:
  __property Graphics::TBitmap *Bitmap = { read=GetBitmap };
  __property HBITMAP HBitmap = { read=GetHBitmap };

public:
    __fastcall TDIBAccess(TComponent* Owner);
    __fastcall ~TDIBAccess();

public:
  void __fastcall BeginUpdate(void);
  void __fastcall EndUpdate(void);

  __property int  Width =  { read=GetWidth  };
  __property int  Height = { read=GetHeight };

public:
  __property TColor  Pixels [ int X ][ int Y ] = { read=GetPixel, write=SetPixel };

__published:
  __property TImage *Image = { read = FImage, write = SetImage };
};
//---------------------------------------------------------------------------
//class PACKAGE TRemoveNotifier : public TComponent
class PACKAGE TRemoveNotifier : public TGraphicControl
{
  typedef TGraphicControl inherited;
  
  friend class TDIBAccess;

protected :
  TDIBAccess *LinkedTo;

protected :
public :
  __fastcall TRemoveNotifier ( TComponent* Owner );
  __fastcall ~TRemoveNotifier ();
};
//---------------------------------------------------------------------------
#endif
