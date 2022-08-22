//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "BMShapedForm.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TBMShapedForm *)
{
    new TBMShapedForm(NULL);
}
//---------------------------------------------------------------------------
__fastcall TBMShapedForm::TBMShapedForm(TComponent* Owner)
    : inherited(Owner)
{
  OSOwned = false;
  FRegion = NULL;
  Align = alClient;
  AutoSize =false;
  Stretch = false;
  OldAutoSize = false;

  Picture->OnChange = PictureUpdate;
  FMoveable =true;

//---> Begin ED
  InMove = false;
// End ED

  OldWidth = 0;
  OldHeight = 0;
  OldFWidth = 0;
  OldFHeight = 0;
  OldStretch = false;
}
//---------------------------------------------------------------------------
__fastcall TBMShapedForm::~TBMShapedForm()
{
  if ( FRegion && ! OSOwned )
    DeleteObject(FRegion);

  FRegion = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::PictureUpdate(System::TObject* /*Sender*/)
{
  if ( ! ComponentState.Contains ( csDesigning ) )
    ; //SetWindowRgn ( Parent->Handle, NULL, false );

  else
    if ( FRegion && ! OSOwned )
      DeleteObject(FRegion);

  FRegion = NULL;

  if ( AutoSize )
    {
    Parent->ClientWidth = Picture->Width;
    Parent->ClientHeight = Picture->Height;
    FImageWidth = Picture->Width;
    FImageHeight = Picture->Height;
    }

  else
    {
    FImageWidth = Width;
    FImageHeight = Height;
    }


  ObtainRegion ();
  RefreshForm ();
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::Loaded(void)
{
  inherited::Loaded();
  ObtainRegion ();
  RefreshForm ();
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::SetParent(Controls::TWinControl* Value)
{
  inherited::SetParent(Value);

  if ( Value )
    {

    if ( dynamic_cast <TForm *> ( Value ) )
      {
      //        TForm(Value).BorderStyle:=bsNone
      for ( int i = 0; i < Value->ControlCount; i ++ )
        {
        if ( dynamic_cast <TBMShapedForm *> ( Value->Controls [ i ] ) && Value->Controls [ i ] != this )
          throw new Exception ( "Please Drop only one TBMShapedForm per Form" );

        }
      }

    else
      throw new Exception ( "Please Drop on a Form" );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::RefreshForm(void)
{
  if ( ! ComponentState.Contains ( csDesigning ) && FRegion )
    {
    Visible = false;
	SetWindowRgn( Parent->Handle, FRegion, true );
    OSOwned = true;
    Visible = true;
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y)
{
  inherited::MouseDown( Button, Shift, X, Y );

  FMoveX = X;
  FMoveY = Y;

//---> Begin ED
  InMove = true;
// End ED

}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::MouseMove(Classes::TShiftState Shift, int X, int Y)
{
  inherited::MouseMove ( Shift, X, Y );

//---> Begin ED
//  if ((FMoveable) && ( Shift == ( Classes::TShiftState () << ssLeft)))
  if ((FMoveable) && (InMove) && ( Shift == ( Classes::TShiftState () << ssLeft)))
// End ED
    {
    Parent->Left = Parent->Left + ( X - FMoveX);
    Parent->Top = Parent->Top + ( Y - FMoveY);
    Parent->Perform ( WM_PAINT,0,0 );
    }
}
//---> Begin ED
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y)
{
  inherited::MouseUp( Button, Shift, X, Y );
  InMove = false;
}
// End ED
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::AddRegion( int x1, int x2, int y )
{
  HRGN Aux;

  if ( FRegion == 0 )
    FRegion =CreateRectRgn ( x1,y,x2+1,y+1 );

  else
    {
    Aux = CreateRectRgn (x1,y,x2+1,y+1);
    CombineRgn( FRegion ,FRegion, Aux, RGN_OR);
    DeleteObject ( Aux );
    }
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::SetBounds(int ALeft, int ATop, int AWidth, int AHeight)
{
  inherited::SetBounds(ALeft, ATop, AWidth, AHeight);

  if ( ComponentState.Contains ( csLoading ))
    return;
    
  if ( Parent )
    {
    PictureUpdate(this);
    }

}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::ObtainRegion (void)
{
#pragma pack(push, 1)
  struct TInfo
  {
  TBitmapInfoHeader Header;
  TColorRef *Colors;
  } Info;
#pragma pack(pop)

  if ( ComponentState.Contains ( csDesigning ))
    return;

  if ( ! Parent )
    return;
    
  if ( Picture->Bitmap->Empty )
    return;

  if ( FRegion )
    {
    if ( FImageWidth == OldFWidth && FImageHeight == OldFHeight )
      return;
      
    if ( ! ComponentState.Contains ( csDesigning ) && FRegion )
      ; //SetWindowRgn ( Parent->Handle, NULL, false );

    else
      DeleteObject(FRegion);
    }

  OldFWidth = Width;
  OldFHeight = Height;
  
  FRegion = NULL;

  TPoint ClientPos = Parent->ClientToScreen ( ::Point ( 0, 0 ));

  if ( Parent->Parent )
    {
    TPoint ParentPos = Parent->Parent->ClientToScreen ( ::Point ( Parent->Left, Parent->Top ));
    ClientPos.x -= ParentPos.x;
    ClientPos.y -= ParentPos.y;
    }

  else
    {
    ClientPos.x -= Parent->Left;
    ClientPos.y -= Parent->Top;
    }


  int RStart, REnd;

  Graphics::TBitmap *Bmp = new Graphics::TBitmap;
  Bmp->Width = FImageWidth;
  Bmp->Height = FImageHeight;

  Bmp->Canvas->StretchDraw( ::Rect ( 0, 0, FImageWidth, FImageHeight ), Picture->Bitmap );

//  Bmp->PixelFormat = pf32bit;

  TColorRef *Pixels = new TColorRef [ FImageWidth * FImageHeight ];

  HDC DC = CreateCompatibleDC(0);
  memset ( &Info, 0, sizeof(Info));
  Info.Header.biSize = sizeof(TBitmapInfoHeader);
  Info.Header.biWidth = FImageWidth;
  Info.Header.biHeight = -FImageHeight;  // negative number makes it a top-down DIB
  Info.Header.biPlanes = 1;
  Info.Header.biBitCount = 32;       
  Info.Colors = NULL;
  GetDIBits ( DC, Bmp->Handle, 0, FImageHeight, Pixels, ( TBitmapInfo * )(&Info),  DIB_RGB_COLORS );
  DeleteDC (DC);

  TColor FTransparentColor = ((TColor ) ( Pixels [ ( FImageHeight - 1 ) * FImageWidth ] & 0x00FFFFFF) );

  for ( int j=0; j < FImageHeight; j ++ )
    {
    RStart = -1;
//---> Begin ED
//    REnd = -1;
// End ED
    for ( int i = 0; i < FImageWidth; i ++ )
      {
      TColor TheColor = ((TColor )( Pixels [ j * FImageWidth + i ] & 0x00FFFFFF));
      if ((( TheColor != FTransparentColor ) ||
            ( i == FImageWidth - 1 )) &&
           (RStart == -1) )
        {
        RStart = i;
        }

      if ((( TheColor == FTransparentColor ) ||
            (i== FImageWidth-1)) &&
           (RStart != -1) )
        {
//        REnd = i - 1;
        REnd = i - ( TheColor == FTransparentColor );
        AddRegion(RStart,REnd,j);
        RStart = -1;
        }
      }
    }

  if ( FRegion )
    OffsetRgn ( FRegion, ClientPos.x, ClientPos.y );

//---> Begin ED
//  delete Pixels;
  delete[]Pixels;
// End ED
  delete Bmp;
  
  OSOwned = false;
}
//---------------------------------------------------------------------------
void __fastcall TBMShapedForm::Paint(void)
{
  if ( ! Picture->Bitmap->Empty )
    {
    if ( OldAutoSize != AutoSize )
      {
      if ( ! AutoSize && ! Stretch )
        {
        Stretch = true;
        OldStretch = true;
        }

      if ( AutoSize && Stretch )
        {
        Stretch = false;
        OldStretch = false;
        }
      }

    if ( OldStretch != Stretch )
      {
      if ( Stretch && AutoSize )
        AutoSize = false;

      if ( ! Stretch && ! AutoSize )
        AutoSize = true;
       
      }

    if ( AutoSize )
      if ( Stretch )
        Stretch = false;

    if ( Stretch )
      if ( AutoSize )
        AutoSize = false;

    if ( ! AutoSize && ! Stretch )
      AutoSize = true;

    OldStretch = Stretch;
/*
    if ( OldAutoSize != AutoSize || OldWidth != Width || OldHeight != Height )
      {
      PictureUpdate( this );
      OldAutoSize = AutoSize;
      OldWidth = Width;
      OldHeight = Height;
      }
*/
    OldAutoSize = AutoSize;
    }

  inherited::Paint();
}
//---------------------------------------------------------------------------
namespace Bmshapedform
{
    void __fastcall PACKAGE Register()
    {
        TComponentClass classes[1] = {__classid(TBMShapedForm)};
        RegisterComponents("BMitov", classes, 0);
    }
}
//---------------------------------------------------------------------------

