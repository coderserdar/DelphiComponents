unit SXSkinLabel;

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

uses Windows, Classes, SXSkinControl, GR32, StdCtrls, Dialogs, SysUtils,
     Messages, Controls, GR32_Blend, SXSkinLibrary, Types, Graphics;

type

 TSXSkinCustomLabel=class(TSXSkinCustomControl)
  private
   FAutoSizeWidth:Boolean;
   FAutoSizeHeight:Boolean;
   FWordWrap:Boolean;
   FTextLeftOffset:Integer;
   FTextTopOffset:Integer;
   FTextRightOffset:Integer;
   FTextBottomOffset:Integer;
   FAlignment:TAlignment;
   FVerticalAlignment:TVerticalAlignment;
   FUseCustomFont:Boolean;
   FUseCustomFontColor:Boolean;
   FOnMouseEnter:TNotifyEvent;
   FOnMouseLeave:TNotifyEvent;
   //
   FMouseOver:Boolean;
   FTextRect:TRect;
   FTextBitmap:TBitmap32;
   FLastFontData:TSXFontData;
   procedure SetCaption(const Value:TCaption);
   procedure SetAlignment(Value:TAlignment);
   procedure SetVerticalAlignment(Value:TVerticalAlignment);
   procedure SetAutoSizeWidth(Value:Boolean);
   procedure SetAutoSizeHeight(Value:Boolean);
   procedure SetWordWrap(Value:Boolean);
   procedure SetTextLeftOffset(Value:Integer);
   procedure SetTextTopOffset(Value:Integer);
   procedure SetTextRightOffset(Value:Integer);
   procedure SetTextBottomOffset(Value:Integer);
   procedure SetUseCustomFont(Value:Boolean);
   procedure SetUseCustomFontColor(Value:Boolean);   
   function HasUnusualSkinStyle:Boolean;
   procedure GetCurrentLState(var LState:TSXSkinLabelStateParam);
   procedure ResetTextParams;
   procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
   procedure CMEnabledChanged(var Message:TMessage); message CM_ENABLEDCHANGED;
  protected
   function NeedRepaintOnStateChange:Boolean;
   procedure Loaded; override;
   procedure InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
   function GetCaption:TCaption;
   procedure CMMouseEnter(var Msg:TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
   property Caption:TCaption read GetCaption write SetCaption;
   property TextLeftOffset:Integer read FTextLeftOffset write SetTextLeftOffset default 0;
   property TextTopOffset:Integer read FTextTopOffset write SetTextTopOffset default 0;
   property TextRightOffset:Integer read FTextRightOffset write SetTextRightOffset default 0;
   property TextBottomOffset:Integer read FTextBottomOffset write SetTextBottomOffset default 0;
   property WordWrap:Boolean read FWordWrap write SetWordWrap default False;
   property AutoSizeWidth:Boolean read FAutoSizeWidth write SetAutoSizeWidth default True;
   property AutoSizeHeight:Boolean read FAutoSizeHeight write SetAutoSizeHeight default True;
   property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
   property VerticalAlignment:TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
   property SkinStyle stored HasUnusualSkinStyle;
   property UseCustomFont:Boolean read FUseCustomFont write SetUseCustomFont default False;
   property UseCustomFontColor:Boolean read FUseCustomFontColor write SetUseCustomFontColor default False;
  public
   procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;  
   procedure InvalidateText;
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
  published
   property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
 end;

 TSXSkinLabel=class(TSXSkinCustomLabel)
  published
   property Align;
   property Alignment;
   property Anchors;
   property AutoSizeWidth;
   property AutoSizeHeight;
   property Caption;
   property Color;
   property Constraints;
   property Cursor;
   property DragCursor;
   property Enabled;
   property Font;
   //property HintData;
   property ParentColor;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property SkinLibrary;
   property SkinStyle;
   property TextLeftOffset;
   property TextTopOffset;
   property TextRightOffset;
   property TextBottomOffset;
   property UseCustomFont;
   property UseCustomFontColor;
   property VerticalAlignment;
   property Visible;
   property WordWrap;
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

uses Math, SXBitmap32Utils;

{ TSXSkinCustomLabel }

function TSXSkinCustomLabel.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_Label';
end;

procedure TSXSkinCustomLabel.SetAlignment(Value:TAlignment);
begin
 if Value<>FAlignment then
  begin
   FAlignment:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetVerticalAlignment(Value:TVerticalAlignment);
begin
 if Value<>FVerticalAlignment then
  begin
   FVerticalAlignment:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetAutoSizeWidth(Value:Boolean);
begin
 if Value<>FAutoSizeWidth then
  begin
   FAutoSizeWidth:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetAutoSizeHeight(Value:Boolean);
begin
 if Value<>FAutoSizeHeight then
  begin
   FAutoSizeHeight:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetWordWrap(Value:Boolean);
begin
 if Value<>FWordWrap then
  begin
   FWordWrap:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetTextLeftOffset(Value:Integer);
begin
 if Value<>FTextLeftOffset then
  begin
   FTextLeftOffset:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetTextTopOffset(Value:Integer);
begin
 if Value<>FTextTopOffset then
  begin
   FTextTopOffset:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetTextRightOffset(Value:Integer);
begin
 if Value<>FTextRightOffset then
  begin
   FTextRightOffset:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetTextBottomOffset(Value:Integer);
begin
 if Value<>FTextBottomOffset then
  begin
   FTextBottomOffset:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetUseCustomFont(Value:Boolean);
begin
 if Value<>FUseCustomFont then
  begin
   FUseCustomFont:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.SetUseCustomFontColor(Value:Boolean);
begin
 if Value<>FUseCustomFontColor then
  begin
   FUseCustomFontColor:=Value;
   if not (csLoading in ComponentState) then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.GetCurrentLState(var LState:TSXSkinLabelStateParam);
var  A:Integer;
 Style:TSXSkinLabelStyle;

 procedure SetLStateFrom(const T:TSXSkinLabelStateParam);
 begin
  AddFontData(LState.FD,T.FD);
 end;

begin
 ClearFontData(LState.FD);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinLabelStyle) then
    begin
     Style:=TSXSkinLabelStyle(SkinLibrary.Styles[A]);
     if not Enabled then
      begin
       SetLStateFrom(Style.RState);
       SetLStateFrom(Style.NState);
      end else
     if FMouseOver then
      begin
       SetLStateFrom(Style.HState);
       SetLStateFrom(Style.NState);
      end else SetLStateFrom(Style.NState);
    end;
  end;
 if FUseCustomFont then
  begin
   LState.FD.SetFontName:=False;
   LState.FD.SetFontSize:=False;
   LState.FD.SetFontStyle:=False;
   LState.FD.SetHasShadow:=False;
   LState.FD.SetSmoothLevel:=False;
   LState.FD.SmoothLevel:=0;
  end;
 if FUseCustomFontColor then
  LState.FD.SetFontColor:=False;  
 SetDefaultFontData(LState.FD,Font);
end;

procedure TSXSkinCustomLabel.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
var   LState:TSXSkinLabelStateParam;
       A,W,H:Integer;
       Flags:Cardinal;
 TextRect,RR:TRect;

 procedure DrawTextToBitmap(Bitmap:TBitmap32);
 begin
  Bitmap.Font:=Canvas.Font;
  if LState.FD.HasShadow then
   begin
    OffsetRect(TextRect,1,1);
    if LState.FD.SmoothLevel=0 then
     DrawAlphaText(Bitmap,Caption,TextRect,Flags,LState.FD.ShadowColor) else
      DrawSmoothText(Bitmap,Caption,TextRect,Flags,LState.FD.SmoothLevel,LState.FD.ShadowColor);
    OffsetRect(TextRect,-1,-1);
   end;
  if LState.FD.SmoothLevel=0 then
   DrawAlphaText(Bitmap,Caption,TextRect,Flags,LState.FD.FontColor) else
    DrawSmoothText(Bitmap,Caption,TextRect,Flags,LState.FD.SmoothLevel,LState.FD.FontColor);
 end;

begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinLabelStyle) then
    begin
     GetCurrentLState(LState);
     if (Caption<>'') and RectInRegion(Rgn,FTextRect) then
      begin
       Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP;
       case Alignment of
        taLeftJustify:  Flags:=Flags or DT_LEFT;
        taRightJustify: Flags:=Flags or DT_RIGHT;
        taCenter:       Flags:=Flags or DT_CENTER;
       end;
       if FWordWrap then Flags:=Flags or DT_WORDBREAK;
       TextRect:=FTextRect;
       if LState.FD.DoPrepaint and (LState.FD.SmoothLevel>0) and (FTextBitmap=nil) then
        begin
         FTextBitmap:=TBitmap32.Create;
         FTextBitmap.DrawMode:=dmBlend;
         FTextBitmap.CombineMode:=cmMerge;
         W:=TextRect.Right-TextRect.Left;
         H:=TextRect.Bottom-TextRect.Top;
         if LState.FD.HasShadow then
          begin
           Inc(W); Inc(H);
          end;
         FTextBitmap.SetSize(W,H);
         FTextBitmap.Clear(0);
         RR:=TextRect;
         OffsetRect(TextRect,-TextRect.Left,-TextRect.Top);
         DrawTextToBitmap(FTextBitmap);
         TextRect:=RR;
        end;
       OffsetRect(TextRect,X-Rect.Left,Y-Rect.Top);
       if LState.FD.DoPrepaint and (LState.FD.SmoothLevel>0) then
        FTextBitmap.DrawTo(Bitmap,TextRect.Left,TextRect.Top) else
         DrawTextToBitmap(Bitmap);
      end;
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomLabel.CMMouseEnter(var Msg:TMessage);
begin
 FMouseOver:=True;
 if Enabled and NeedRepaintOnStateChange then
  begin
   InvalidateText;
   ResetTextParams;
   InvalidateText;
  end;
 if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
end;

procedure TSXSkinCustomLabel.CMMouseLeave(var Msg:TMessage);
begin
 FMouseOver:=False;
 if Enabled and NeedRepaintOnStateChange then
  begin
   InvalidateText;
   ResetTextParams;
   InvalidateText;
  end;
 if Assigned(FOnMouseLeave) then
  FOnMouseLeave(Self);
end;

procedure TSXSkinCustomLabel.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  ResetTextParams;
 inherited;
end;

function TSXSkinCustomLabel.NeedRepaintOnStateChange:Boolean;
var LState:TSXSkinLabelStateParam;
begin
 GetCurrentLState(LState);
 Result:=not SameFontData(LState.FD,FLastFontData);
end;

procedure TSXSkinCustomLabel.ResetTextParams;
var LState:TSXSkinLabelStateParam;
     Flags:Cardinal;
  NewBRect:TRect;
         A:Integer;
   OldRect:TRect;
begin
 if Parent=nil then exit;
 if FTextBitmap<>nil then
  begin
   FTextBitmap.Free;
   FTextBitmap:=nil;
  end;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentLState(LState);
   FLastFontData:=LState.FD;
   Canvas.Font.Name:=LState.FD.FontName;
   Canvas.Font.Size:=LState.FD.FontSize;
   Canvas.Font.Style:=LState.FD.FontStyle;
   FTextRect:=Rect(FTextLeftOffset,FTextTopOffset,Width-FTextRightOffset,Height-FTextBottomOffset);
   Flags:=DT_CALCRECT or DT_NOPREFIX or DT_TOP;
   if FWordWrap then Flags:=Flags or DT_WORDBREAK;
   case Alignment of
    taLeftJustify:  Flags:=Flags or DT_LEFT;
    taRightJustify: Flags:=Flags or DT_RIGHT;
    taCenter:       Flags:=Flags or DT_CENTER;
   end;
   OldRect:=FTextRect;
   if LState.FD.SmoothLevel=0 then
    DrawText(Canvas.Handle,PChar(Caption),-1,FTextRect,Flags) else
     DrawSmoothText(Canvas,Caption,FTextRect,Flags,LState.FD.SmoothLevel);
   if FAutoSizeWidth or FAutoSizeHeight then
    begin
     if (Alignment=taRightJustify) and (FTextRect.Right<OldRect.Right) then
      begin
       FTextRect.Left:=OldRect.Right-FTextRect.Right+FTextRect.Left;
       FTextRect.Right:=OldRect.Right;
      end;
     if (Alignment=taCenter) and (FTextRect.Right<OldRect.Right) then
      begin
       A:=(OldRect.Right-FTextRect.Right) div 2;
       Inc(FTextRect.Left,A);
       Inc(FTextRect.Right,A);
      end;
     if (VerticalAlignment=taVerticalCenter) and (FTextRect.Bottom<OldRect.Bottom) then
      begin
       A:=(OldRect.Bottom-FTextRect.Bottom) div 2;
       Inc(FTextRect.Top,A);
       Inc(FTextRect.Bottom,A);
      end else
     if (VerticalAlignment=taAlignBottom) and (FTextRect.Bottom<OldRect.Bottom) then
      begin
       FTextRect.Top:=OldRect.Bottom-FTextRect.Bottom+FTextRect.Top;
       FTextRect.Bottom:=OldRect.Bottom;
      end;
     NewBRect:=BoundsRect;
     if FAutoSizeWidth then
      begin
       NewBRect.Right:=NewBRect.Left+FTextLeftOffset+FTextRect.Right-
                       FTextRect.Left+FTextRightOffset;
      end;
     if FAutoSizeHeight then
      begin
       NewBRect.Bottom:=NewBRect.Top+FTextTopOffset+FTextRect.Bottom-
                       FTextRect.Top+FTextBottomOffset;
      end;
     if not EqualRect(BoundsRect,NewBRect) then
      begin
       InternalSetBounds(NewBRect.Left,NewBRect.Top,NewBRect.Right-NewBRect.Left,
                         NewBRect.Bottom-NewBRect.Top);
       ResetTextParams;
      end;
    end;
  end;  
end;

function TSXSkinCustomLabel.GetCaption:TCaption;
begin
 Result:=inherited Caption;
end;

procedure TSXSkinCustomLabel.SetCaption(const Value:TCaption);
begin
 if csLoading in ComponentState then
  begin
   inherited Caption:=Value;
   exit;
  end;
 if Caption<>Value then
  begin
   inherited Caption:=Value;
   InvalidateText;
   ResetTextParams;
   InvalidateText;
  end;
end;

procedure TSXSkinCustomLabel.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
var DoReset:Boolean;
begin
 if not (csLoading in ComponentState) then
  begin
   if FAutoSizeWidth then AWidth:=Width;
   if FAutoSizeHeight then AHeight:=Height;
  end;
 if (ALeft=Left) and (ATop=Top) and (AWidth=Width) and (AHeight=Height) then exit;
 DoReset:=(Width<>AWidth) or (Height<>AHeight);
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
 if not (csLoading in ComponentState) then
  begin
   if DoReset then
    ResetTextParams;
   Invalidate;
  end;
end;

procedure TSXSkinCustomLabel.InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

procedure TSXSkinCustomLabel.CMFontChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  begin
   if NeedRepaintOnStateChange then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.CMEnabledChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  begin
   if not Enabled then
    FMouseOver:=False;
   if NeedRepaintOnStateChange then
    begin
     InvalidateText;
     ResetTextParams;
     InvalidateText;
    end;
  end;
end;

procedure TSXSkinCustomLabel.InvalidateText;
begin
 InvalidateRect(Handle,@FTextRect,False);
end;

procedure TSXSkinCustomLabel.Loaded;
begin
 inherited;
 ResetTextParams;
end;

constructor TSXSkinCustomLabel.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 ControlStyle:=ControlStyle+[csSetCaption];
 FAutoSizeWidth:=True;
 FAutoSizeHeight:=True;
 SkinStyle:='_Label';
end;

destructor TSXSkinCustomLabel.Destroy;
begin
 FTextBitmap.Free;
 inherited;
end;

end.
