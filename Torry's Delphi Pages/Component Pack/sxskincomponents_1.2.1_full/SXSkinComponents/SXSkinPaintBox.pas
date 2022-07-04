unit SXSkinPaintBox;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

uses Windows, Controls, Classes, SXSkinControl, GR32, Graphics, Types;

type

  TOnCPFastPaintProc=procedure(Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer)of object;
  TOnCPPaintProc=procedure(Bitmap:TBitmap32;DstRect:TRect)of object;

  TSXSkinCustomPaintBox=class(TSXSkinCustomControl)
   private
    FOnFastPaint:TOnCPFastPaintProc;
    FOnPaint:TOnCPPaintProc;
    FOnResize:TNotifyEvent;
    FBitmap:TBitmap32;
    procedure BitmapChanged(Sender:TObject;const Area:TRect;const Info:Cardinal);
   public
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
    procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
               Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
               WithSubItems:Boolean); override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property Bitmap:TBitmap32 read FBitmap;
    property OnResize:TNotifyEvent read FOnResize write FOnResize;
    property OnFastPaint:TOnCPFastPaintProc read FOnFastPaint write FOnFastPaint;
    property OnPaint:TOnCPPaintProc read FOnPaint write FOnPaint;
  end;

  TSXSkinPaintBox=class(TSXSkinCustomPaintBox)
   published
    property Align;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property Font;
    //property HintData;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SkinLibrary;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnFastPaint;    
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses SXSkinLibrary, Math;

{ TSXSkinCustomPaintBox }

procedure TSXSkinCustomPaintBox.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
var R:TRect;
begin
 if csDesigning in ComponentState then
  begin
   Bitmap.FrameRectTS(X-Rect.Left,Y-Rect.Top,X-Rect.Left+ClientWidth,Y-Rect.Top+ClientHeight,Color32(0,0,0,128));
  end else
   begin
    if Assigned(FOnFastPaint) then
     FOnFastPaint(Rect,Rgn,Bitmap,X,Y) else
    if Assigned(FOnPaint) then
     begin
      R:=ClientRect;
      OffsetRect(R,X-Rect.Left,Y-Rect.Top);
      FOnPaint(Bitmap,R);
     end else
      begin
       R:=Rect;
       OffsetRect(R,X-Rect.Left,Y-Rect.Top);
       FBitmap.DrawTo(Bitmap,R,Rect);
      end;
   end;
 inherited;
end;

procedure TSXSkinCustomPaintBox.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 inherited;
 if (Width<>FBitmap.Width) or (Height<>Bitmap.Height) then
  begin
   FBitmap.SetSize(Width,Height);
   FBitmap.Clear(0);
   if Assigned(FOnResize) then
    FOnResize(Self);
  end;
end;

procedure TSXSkinCustomPaintBox.BitmapChanged(Sender:TObject;const Area:TRect;const Info:Cardinal);
begin
 if HandleAllocated then
  InvalidateRect(Handle,@Area,False);
end;

constructor TSXSkinCustomPaintBox.Create(AOwner:TComponent);
begin
 inherited;
 FBitmap:=TBitmap32.Create;
 FBitmap.DrawMode:=dmBlend;
 FBitmap.CombineMode:=cmMerge;
 FBitmap.OnAreaChanged:=BitmapChanged;
end;

destructor TSXSkinCustomPaintBox.Destroy;
begin
 FBitmap.Free;
 inherited;
end;

end.
