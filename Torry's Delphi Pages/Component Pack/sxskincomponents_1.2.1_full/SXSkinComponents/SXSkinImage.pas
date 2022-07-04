unit SXSkinImage;

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

uses Windows, Classes, SXSkinControl, GR32, SysUtils, Types, Messages, Controls;

type

  TSXSkinCustomImage=class(TSXSkinCustomControl)
   private
    CEID:Integer;
    function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   protected
    function NeedToPaintBackground:Boolean; override;
    function CapturesMouseAt(X,Y:Integer):Boolean; override;
   public
    procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
               Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
               WithSubItems:Boolean); override;   
    function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
    constructor Create(AOwner:TComponent); override;
  end;

  TSXSkinImage=class(TSXSkinCustomImage)
   published
    property Align;
    property Anchors;
    property AutoSize;
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
    property SkinStyle;
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
    property OnMouseEnter;
    property OnMouseLeave;
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

{ TSXSkinCustomImage }

procedure TSXSkinCustomImage.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
var  A:Integer;
 Style:TSXSkinGeneralStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(SkinStyle,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     StdVariableComparer.SetSize(Width,Height);
     Style.DrawToBitmap(Self,CEID,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,StdVariableComparer);
    end;
  end;
 inherited;
end;

function TSXSkinCustomImage.NeedToPaintBackground:Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(SkinStyle,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.NeedToPaintBackground;
    end;
  end;
end;

function TSXSkinCustomImage.OnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if VarName='W' then
  begin
   Result:=Width; exit;
  end;
 if VarName='H' then
  begin
   Result:=Height; exit;
  end;
 Error:=True;
end;

function TSXSkinCustomImage.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var  A:Integer;
 Style:TSXSkinGeneralStyle;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(SkinStyle,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     StdVariableComparer.SetSize(Width,Height);
     Result:=Style.IsTransparent(Self,CEID,X,Y,Width,Height,SkinLibrary,Limit,StdVariableComparer);
    end;
  end;
end;

function TSXSkinCustomImage.CapturesMouseAt(X,Y:Integer):Boolean;
var  A:Integer;
 Style:TSXSkinGeneralStyle;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(SkinStyle,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     StdVariableComparer.SetSize(Width,Height);
     Result:=Style.CapturesMouseAt(Self,CEID,X,Y,Width,Height,SkinLibrary,StdVariableComparer);
    end;
  end;
end;

constructor TSXSkinCustomImage.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 CEID:=GetNewCElementID;
end;

end.
