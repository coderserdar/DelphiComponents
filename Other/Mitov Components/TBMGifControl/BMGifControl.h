/*> Ver: Beta V1.0b6 ****************      History      ***************************\

Beta V1.0b1		02/11/1999		Released.
Beta V1.0b2		02/21/1999		Stretch bug has been fixed.
Beta V1.0b3		02/26/1999		Memory leak has been fixed.
                                Now using MediaTimer instead of thread.
                                Auto reducing the resource usage.

Beta V1.0b4		03/04/1999		Access violation on package closing fix.
Beta V1.0b5		03/20/1999		Small start up bug fix.
Beta V1.0b6		03/29/1999		Critical sections have been added.

  This component is based initialy on the RxGifAnimation component !
  Many of the basic routines and structures has been used the way they have been
  designed in the Rx library, just with relatively minor changes. Those include :
  1. The Gif image description structures.
  2. The hache table (small redesign).
    However the component has been redesigned complitely, and differs seriosly
  from the one in the Rx library. Regardles of this the authors of the Rx library
  are the initial athors of the original component so here is their header :

{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}


Legal issues: Copyright (C) 1998, 1999 by Boian Mitov
              <mitov@ec.rockwell.com>
              <http://members.xoom.com/mitov>

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
#ifndef BMGifControlH
#define BMGifControlH
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>
#include <vcl\DsgnIntf.hpp>
#include <syncobjs.hpp>

//-- user supplied -----------------------------------------------------------

//-- type declarations -------------------------------------------------------

namespace Bmgifcontrol
{

typedef Shortint TBMGIFBits;

struct TScreenDescriptor;
class  TExtension;


#pragma pack(push, 1)
struct TBMGIFColorItem
{
	Byte Red;
	Byte Green;
	Byte Blue;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TBMGIFColorTable
{
	int Count;
	TBMGIFColorItem Colors[256];
} ;
#pragma pack(pop)

class TBMGIFImage;
class TBMGIFData;
class TBMGIFData : public Graphics::TSharedImage
{
	typedef Graphics::TSharedImage inherited;

    friend class TBMGIFFrame;
    friend class TBMGIFImage;

public :
    enum TBMGIFVersion { gvUnknown, gv87a, gv89a, MaxVer = gv89a };
    enum TDisposalMethod { dmUndefined, dmLeave, dmRestoreBackground, dmRestorePrevious, dmReserved4, dmReserved5,
	  dmReserved6, dmReserved7 };

private:
	Classes::TStrings* FComment;
	Byte FAspectRatio;
	Byte FBitsPerPixel;
	Byte FColorResBits;
	TBMGIFColorTable FColorMap;

protected:
	virtual void __fastcall FreeHandle(void);


public:
	__fastcall TBMGIFData(void);
	__fastcall virtual ~TBMGIFData(void);

public:
//    __fastcall void Release () { inherited::Release (); }
};

class TBMGIFFrame;
class TBMGIFImage : public Graphics::TGraphic
{
	typedef Graphics::TGraphic inherited;

    friend TBMGIFFrame;
    friend class TBMGIFControl;

private:
	TBMGIFData* FImage;
	TBMGIFData::TBMGIFVersion FVersion;
	Classes::TList* FItems;
	int FFrameIndex;
	Word FScreenWidth;
	Word FScreenHeight;
	Graphics::TColor FBackgroundColor;
	bool FLooping;
	Word FRepeatCount;
    
private:
	Graphics::TBitmap* __fastcall GetBitmap(void);
	int __fastcall GetCount(void);
	Classes::TStrings* __fastcall GetComment(void);
	int __fastcall GetScreenWidth(void);
	int __fastcall GetScreenHeight(void);
	int __fastcall GetGlobalColorCount(void);
	void __fastcall UpdateScreenSize(void);
	void __fastcall SetComment(Classes::TStrings* Value);
	TBMGIFFrame* __fastcall GetFrame(int Index);
	void __fastcall SetFrameIndex(int Value);
	void __fastcall SetBackgroundColor(Graphics::TColor Value);
	void __fastcall SetLooping(bool Value);
	void __fastcall SetRepeatCount(Word Value);
	void __fastcall ReadSignature(Classes::TStream* Stream);
	void __fastcall DoProgress(Graphics::TProgressStage Stage, Byte PercentDone, const System::AnsiString 
		Msg);
	Graphics::TColor __fastcall GetTransparentColor(void);
	Graphics::TColor __fastcall GetBackgroundColor(void);
	Graphics::TPixelFormat __fastcall GetPixelFormat(void);
	void __fastcall EncodeFrames(bool ReverseDecode);
	void __fastcall ReadStream(int Size, Classes::TStream* Stream, bool ForceDecode);
	void __fastcall WriteStream(Classes::TStream* Stream, bool WriteSize);
	
protected:
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual void __fastcall Draw(Graphics::TCanvas* ACanvas, const Windows::TRect &ARect);
	virtual bool __fastcall Equals(Graphics::TGraphic* Graphic);
	virtual bool __fastcall GetEmpty(void);
	virtual int __fastcall GetHeight(void);
	virtual int __fastcall GetWidth(void);
	virtual HPALETTE __fastcall GetPalette(void);
	virtual bool __fastcall GetTransparent(void);
	void __fastcall ClearItems(void);
	void __fastcall NewImage(void);
	void __fastcall UniqueImage(void);
	virtual void __fastcall ReadData(Classes::TStream* Stream);
	virtual void __fastcall SetHeight(int Value);
	virtual void __fastcall SetWidth(int Value);
	virtual void __fastcall WriteData(Classes::TStream* Stream);

protected:
    void __fastcall ReadScreenDescriptor( TStream *Stream, TScreenDescriptor & ScreenDesc );
    void __fastcall ReadGlobalColorMap( TStream *Stream, TScreenDescriptor & ScreenDesc );
    TStrings *__fastcall ReadDataBlock( TStream *Stream );
    TExtension  *__fastcall ReadExtension ( TStream *Stream );
    TList *__fastcall ReadExtensionBlock( TStream *Stream, char &SeparatorChar );
    void __fastcall WriteSignature(TStream *Stream);
    void __fastcall WriteScreenDescriptor(TStream *Stream);
    void __fastcall WriteDataBlock( TStream *Stream, TStrings *Data );
    void __fastcall WriteExtensionBlock(TStream *Stream, TList *Extensions );
    
public:
    void __fastcall GetFrameBitmaps(TBMGIFFrame *TheFrame, Graphics::TBitmap *&Bitmap, Graphics::TBitmap *&MaskBitmap );

protected:
	__property Graphics::TBitmap* Bitmap = {read=GetBitmap};
	
public:
	__fastcall virtual TBMGIFImage(void);
	__fastcall virtual ~TBMGIFImage(void);
	void __fastcall Clear(void);
	void __fastcall DecodeAllFrames(void);
	void __fastcall EncodeAllFrames(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	virtual void __fastcall LoadFromStream(Classes::TStream* Stream);
	virtual void __fastcall SaveToStream(Classes::TStream* Stream);
	virtual void __fastcall LoadFromClipboardFormat(Word AFormat, int AData, HPALETTE APalette);
	virtual void __fastcall SaveToClipboardFormat(Word &AFormat, int &AData, HPALETTE &APalette);
	virtual void __fastcall AddFrame(Graphics::TGraphic* Value);
	void __fastcall DeleteFrame(int Index);
	void __fastcall MoveFrame(int CurIndex, int NewIndex);
	void __fastcall Grayscale(bool ForceEncoding);
	__property Graphics::TColor BackgroundColor = {read=GetBackgroundColor, write=SetBackgroundColor, nodefault
		};
	__property Classes::TStrings* Comment = {read=GetComment, write=SetComment};
	__property int Count = {read=GetCount, nodefault};
	__property TBMGIFFrame* Frames[int Index] = {read=GetFrame/*, default*/};
	__property int FrameIndex = {read=FFrameIndex, write=SetFrameIndex, nodefault};
	__property int GlobalColorCount = {read=GetGlobalColorCount, nodefault};
	__property bool Looping = {read=FLooping, write=SetLooping, nodefault};
	__property Graphics::TPixelFormat PixelFormat = {read=GetPixelFormat, nodefault};
	__property Word RepeatCount = {read=FRepeatCount, write=SetRepeatCount, nodefault};
	__property int ScreenWidth = {read=GetScreenWidth, nodefault};
	__property int ScreenHeight = {read=GetScreenHeight, nodefault};
	__property Graphics::TColor TransparentColor = {read=GetTransparentColor, nodefault};
	__property TBMGIFData::TBMGIFVersion Version = {read=FVersion, nodefault};
};

class TBMGIFItem : public Graphics::TSharedImage 
{
	typedef Graphics::TSharedImage inherited;

    friend class TBMGIFFrame;
    friend class TBMGIFImage;
	
private:
	Classes::TMemoryStream* FImageData;
	tagPOINT FSize;
	Byte FPackedFields;
	Byte FBitsPerPixel;
	TBMGIFColorTable FColorMap;
	
protected:
	virtual void __fastcall FreeHandle(void);
	
public:
	__fastcall virtual ~TBMGIFItem(void);
public:
	/* TObject.Create */ __fastcall TBMGIFItem(void) : Graphics::TSharedImage() { }
	
};

class TBMGIFFrame : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;

    friend class TBMGIFImage;
	
private:
	TBMGIFImage* FOwner;
	Graphics::TBitmap* FBitmap;
	Graphics::TBitmap* FMaskBitmap;

	Graphics::TBitmap* FFullBitmap;
	Graphics::TBitmap* FFullMaskBitmap;
    
	TBMGIFItem* FImage;
	Classes::TList* FExtensions;
	tagPOINT FTopLeft;
	bool FInterlaced;
	bool FCorrupted;
	bool FGrayscale;
/* Transparency
	Graphics::TColor FTransparentColor;
*/

    TCriticalSection *LocalCritical;
    int FTransIndex; //  Transparency

	Word FAnimateInterval;
	TBMGIFData::TDisposalMethod FDisposal;
	bool FLocalColors;
	Graphics::TBitmap* __fastcall GetBitmap(void);
	Graphics::TBitmap* __fastcall GetMaskBitmap(void);

	int __fastcall GetHeight(void);
	int __fastcall GetWidth(void);
	int __fastcall GetColorCount(void);
	Classes::TStrings* __fastcall FindComment(bool ForceCreate);
	Classes::TStrings* __fastcall GetComment(void);
	void __fastcall SetComment(Classes::TStrings* Value);
//	void __fastcall SetTransparentColor(Graphics::TColor Value);
	void __fastcall SetDisposalMethod(TBMGIFData::TDisposalMethod Value);
	void __fastcall SetAnimateInterval(Word Value);
	void __fastcall SetTopLeft(const tagPOINT &Value);
	void __fastcall NewBitmaps(void);
	void __fastcall NewFullBitmaps(void);
	void __fastcall NewImage(void);
	void __fastcall SaveToBitmapStream(Classes::TMemoryStream *Stream, Classes::TMemoryStream *MaskStream);
	void __fastcall EncodeBitmapStream(Classes::TMemoryStream* Stream);
	void __fastcall EncodeRasterData(void);
	void __fastcall UpdateExtensions(void);
	void __fastcall WriteImageDescriptor(Classes::TStream* Stream);
	void __fastcall WriteLocalColorMap(Classes::TStream* Stream);
	void __fastcall WriteRasterData(Classes::TStream* Stream);

protected:
	void __fastcall LoadFromStream(Classes::TStream* Stream);
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	void __fastcall GrayscaleImage(bool ForceEncoding);
	__property Graphics::TBitmap* Bitmap = {read=GetBitmap};
	__property Graphics::TBitmap* MaskBitmap = {read=GetMaskBitmap};

    int __fastcall FindTransparent ();
    TPixelFormat __fastcall ConvertBitsPerPixel ();
    void __fastcall ExpandBitmaps ();
    void __fastcall ExpandFullBitmaps ();

public:
    void __fastcall Reduce ();
    
public:
	__fastcall virtual TBMGIFFrame(TBMGIFImage* AOwner);
	__fastcall virtual ~TBMGIFFrame(void);
    
public:
	Graphics::TBitmap* __fastcall GetFullBitmap(void);
	Graphics::TBitmap* __fastcall GetFullMaskBitmap(void);

public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	void __fastcall Draw(Graphics::TCanvas* ACanvas, const Windows::TRect &ARect);
//    void __fastcall DrawMasked (Graphics::TCanvas* ACanvas, Graphics::TBitmap* AMask, const Windows::TRect &ARect );
    void __fastcall DrawMasked (Graphics::TBitmap* ABitmap, const Windows::TRect &ARect );
    void __fastcall DrawMask(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect );
    void __fastcall DrawMaskOr(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect );
	void __fastcall DrawFull(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect);
    void __fastcall DrawFullMask(Graphics::TBitmap* ABitmap, const Windows::TRect &ARect );
	__property Word AnimateInterval = {read=FAnimateInterval, write=SetAnimateInterval, nodefault};
	__property int ColorCount = {read=GetColorCount, nodefault};
	__property Classes::TStrings* Comment = {read=GetComment, write=SetComment};
	__property TBMGIFData::TDisposalMethod DisposalMethod = {read=FDisposal, write=SetDisposalMethod, nodefault};
	__property bool Interlaced = {read=FInterlaced, nodefault};
	__property bool Corrupted = {read=FCorrupted, nodefault};
//	__property Graphics::TColor TransparentColor = {read=FTransparentColor, write=SetTransparentColor,
//		nodefault};
	__property tagPOINT Origin = {read=FTopLeft, write=SetTopLeft};
	__property int Height = {read=GetHeight, nodefault};
	__property int Width = {read=GetWidth, nodefault};
};

//-- var, const, procedure ---------------------------------------------------
//extern PACKAGE void __fastcall rxgif_dummy(void);

class TBMImageControl : public Controls::TGraphicControl
{
	typedef Controls::TGraphicControl inherited;

private:
	bool FDrawing;

protected:
	Graphics::TGraphic* FGraphic;
	bool __fastcall DoPaletteChange(void);
	virtual void __fastcall DoPaintImage(void) = 0;
	void __fastcall PaintDesignRect(void);
	void __fastcall PaintImage(void);
	void __fastcall PictureChanged(void);

public:
	__fastcall virtual TBMImageControl(Classes::TComponent* AOwner);
public:
	/* TGraphicControl.Destroy */ __fastcall virtual ~TBMImageControl(void) { }

};

class TBMGIFControl : public TBMImageControl
{
	typedef TBMImageControl inherited;

private:
	bool FAnimate;
	bool FAutoSize;
	TBMGIFImage* FImage;
	TObject* FTimer;
	int FFrameIndex;
	bool FStretch;
    int  Delay;
    int  RestoreCount;
	bool FLoop;
	bool FCenter;
	bool FTransparent;
	bool FTimerRepaint;
	Classes::TNotifyEvent FOnStart;
	Classes::TNotifyEvent FOnStop;
	Classes::TNotifyEvent FOnChange;
	Classes::TNotifyEvent FOnFrameChanged;

	Word __fastcall GetDelayTime(int Index);
	void __fastcall AdjustBounds(void);
	void __fastcall SetAutoSize(bool Value);
	void __fastcall SetAnimate(bool Value);
	void __fastcall SetCenter(bool Value);
	void __fastcall SetImage(TBMGIFImage* Value);
	void __fastcall SetFrameIndex(int Value);
	void __fastcall SetStretch(bool Value);
	void __fastcall SetTransparent(bool Value);
	void __fastcall ImageChanged(System::TObject* Sender);
	void __fastcall TimerExpired();
	MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);

    TCriticalSection *LocalCritical;

protected:
	DYNAMIC HPALETTE __fastcall GetPalette(void);
	virtual void __fastcall Paint(void);
	virtual void __fastcall DoPaintImage(void);
	DYNAMIC void __fastcall Change(void);
	DYNAMIC void __fastcall FrameChanged(void);
	DYNAMIC void __fastcall Start(void);
	DYNAMIC void __fastcall Stop(void);

public:
	void __fastcall OnTimer();

public:
    void __fastcall Reduce ();

public:
	__fastcall virtual TBMGIFControl(Classes::TComponent* AOwner);
	__fastcall virtual ~TBMGIFControl(void);

__published:
	__property bool Animate = {read=FAnimate, write=SetAnimate, default=0};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property bool Center = {read=FCenter, write=SetCenter, default=0};
	__property int FrameIndex = {read=FFrameIndex, write=SetFrameIndex, default=0};
	__property TBMGIFImage* Image = {read=FImage, write=SetImage};
	__property bool Loop = {read=FLoop, write=FLoop, default=1};
	__property bool Stretch = {read=FStretch, write=SetStretch, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=1};
	__property Align ;
	__property Cursor ;
	__property DragCursor ;
	__property DragMode ;
	__property Enabled ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ShowHint ;
	__property Visible ;
	__property Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Classes::TNotifyEvent OnFrameChanged = {read=FOnFrameChanged, write=FOnFrameChanged};
	__property Classes::TNotifyEvent OnStart = {read=FOnStart, write=FOnStart};
	__property Classes::TNotifyEvent OnStop = {read=FOnStop, write=FOnStop};
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragOver ;
	__property OnDragDrop ;
	__property OnEndDrag ;
	__property OnMouseMove ;
	__property OnMouseDown ;
	__property OnMouseUp ;
	__property OnStartDrag ;

};


  extern Word CF_GIF; // Clipboard format for GIF image.

};

#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Bmgifcontrol;
#endif
//-- end unit ----------------------------------------------------------------
#endif
