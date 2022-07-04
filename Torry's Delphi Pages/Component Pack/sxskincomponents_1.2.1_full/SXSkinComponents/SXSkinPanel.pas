unit SXSkinPanel;

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

uses Windows, Controls, Classes, SXSkinControl, GR32, Graphics;

type

  TSXSkinCustomPanel=class(TSXSkinCustomControl)
   private
    FCapturesMouse:Boolean;
   protected
    function CapturesMouseAt(X,Y:Integer):Boolean; override;
   public
    function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
    procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
               Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
               WithSubItems:Boolean); override;
    constructor Create(AOwner:TComponent); override;
    property CapturesMouse:Boolean read FCapturesMouse write FCapturesMouse default True;
  end;

  TSXSkinPanel=class(TSXSkinCustomPanel)
   published
    property Align;
    property Anchors;
    property AutoSize;
    property CapturesMouse;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses SXSkinLibrary, Math;

{ TSXSkinCustomPanel }

procedure TSXSkinCustomPanel.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
const XorColor=$00FFD8CE;           
begin
 if csDesigning in ComponentState then
  begin
   Bitmap.FrameRectTS(X-Rect.Left,Y-Rect.Top,X-Rect.Left+ClientWidth,Y-Rect.Top+ClientHeight,Color32(0,0,0,128));
  end;
 inherited;  
end;

function TSXSkinCustomPanel.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
begin
 Result:=True;
end;

function TSXSkinCustomPanel.CapturesMouseAt(X,Y:Integer):Boolean;
begin
 Result:=FCapturesMouse;
end;

constructor TSXSkinCustomPanel.Create(AOwner:TComponent);
begin
 inherited;
 FCapturesMouse:=True;
end;

end.
