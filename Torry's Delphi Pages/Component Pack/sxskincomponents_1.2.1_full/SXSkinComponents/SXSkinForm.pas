unit SXSkinForm;

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

uses GR32_Image, GR32, Windows, Graphics, Classes, Messages, Forms, StdCtrls,
     ExtCtrls, SysUtils, Controls, SXSkinLibrary, Types;

{$IFNDEF COMPILER_7_UP}
 {$EXTERNALSYM WM_NCMOUSELEAVE}
 const WM_NCMOUSELEAVE = $02A2;
{$ENDIF}

type

 TSXSkinCustomForm=class(TComponent)
  private
   FSkinLibrary:TSXSkinLibrary;
   FSkinStyle:String;
   FForm:TForm;
   FCaptionHeight:Integer;
   FFormActive:Boolean;
   FCustomCaptionHeight:Integer;
   FIconRect:TRect;
   FTextRect:TRect;
   FCloseRect:TRect;
   FMaximizeRect:TRect;
   FMinimizeRect:TRect;
   FHelpRect:TRect;
   FCloseOver:Boolean;
   FMaximizeOver:Boolean;
   FMinimizeOver:Boolean;
   FHelpOver:Boolean;
   FCloseDown:Boolean;
   FMaximizeDown:Boolean;
   FMinimizeDown:Boolean;
   FHelpDown:Boolean;
   OldWindowProc:TWndMethod;
   TmpWidth:Integer;
   TmpHeight:Integer;
   ResizeTopLeftRgn:HRGN;
   ResizeTopRightRgn:HRGN;
   ResizeBottomLeftRgn:HRGN;
   ResizeBottomRightRgn:HRGN;
   ResizeLeftRgn:HRGN;
   ResizeRightRgn:HRGN;
   ResizeTopRgn:HRGN;
   ResizeBottomRgn:HRGN;
   CaptionRgn:HRGN;
   LastLeft,LastTop:Integer;
   FTextBitmap:TBitmap32;
   FIconStyle:String;
   FUseTFormIcon:Boolean;
   //
   CEID_Caption:Integer;
   CEID_LeftFrame:Integer;
   CEID_RightFrame:Integer;
   CEID_BottomFrame:Integer;
   CEID_Icon:Integer;
   CEID_Close:Integer;
   CEID_Maximize:Integer;
   CEID_Minimize:Integer;
   CEID_Help:Integer;
   CEID_Restore:Integer;
   function HasUnusualSkinStyle:Boolean;
   function HasUnusualIconStyle:Boolean;
   procedure SetSkinStyle(const Value:String);
   procedure SetSkinLibrary(Value:TSXSkinLibrary);
   procedure SetUseTFormIcon(Value:Boolean);
   procedure SetCustomCaptionHeight(Value:Integer);
   procedure NewWndProc(var Message:TMessage);
   procedure GetCurrentFState(var FState:TSXSkinFormStateParam;
              PSkinFilePath:PString=nil;PZipFilePath:PString=nil);
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure CalcResizeRegions;
   function HitTest(X,Y:Integer):Integer;
   procedure ProcessNCMouseMove;
  protected
   procedure Notification(AComponent:TComponent;Operation:TOperation); override;
  public
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   procedure DrawCaptionAndBorder(Rgn:HRGN);
   procedure DrawCaptionToRect(Bitmap:TBitmap32;Rect:TRect);
   procedure ResetMaskRegion;
   procedure CalcCaptionElementsRects;
   procedure SkinChanged;
   property CustomCaptionHeight:Integer read FCustomCaptionHeight write SetCustomCaptionHeight default 0;
   property IconStyle:String read FIconStyle write FIconStyle stored HasUnusualIconStyle;
   property SkinLibrary:TSXSkinLibrary read FSkinLibrary write SetSkinLibrary;
   property SkinStyle:String read FSkinStyle write SetSkinStyle stored HasUnusualSkinStyle;
   property UseTFormIcon:Boolean read FUseTFormIcon write SetUseTFormIcon default True;
 end;

 TSXSkinForm=class(TSXSkinCustomForm)
  published
   property CustomCaptionHeight;
   property IconStyle;
   property SkinLibrary;
   property SkinStyle;
   property UseTFormIcon;
 end;

implementation

uses SXSkinRegionManager, SXMathEval, SXBitmap32Utils, SXSkinUtils;

type

 TSXForm=class(TForm);

{ TSXSkinCustomForm }

function TSXSkinCustomForm.HasUnusualIconStyle:Boolean;
begin
 Result:=SkinStyle<>'_Selective_StdIcon';
end; 

function TSXSkinCustomForm.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_Form';
end;

procedure TSXSkinCustomForm.SetSkinLibrary(Value:TSXSkinLibrary);
begin
 if FSkinLibrary<>Value then
  begin
   if FSkinLibrary<>nil then
    begin
     FSkinLibrary.RemoveFreeNotification(Self);
     FSkinLibrary.RemoveSkinComponent(Self);
    end;
   FSkinLibrary:=Value;
   if FSkinLibrary<>nil then
    begin
     FSkinLibrary.FreeNotification(Self);
     FSkinLibrary.AddSkinComponent(Self);
    end;
   if not (csDestroying in ComponentState) then
    SkinChanged;
  end;
end;

procedure TSXSkinCustomForm.SetSkinStyle(const Value:String);
begin
 if FSkinStyle<>Value then
  begin
   FSkinStyle:=Value;
   SkinChanged;
  end;
end;

procedure TSXSkinCustomForm.SetUseTFormIcon(Value:Boolean);
begin
 if FUseTFormIcon<>Value then
  begin
   FUseTFormIcon:=Value;
   if FForm<>nil then
    SendMessage(FForm.Handle,WM_NCPAINT,0,0);
  end;
end;

procedure TSXSkinCustomForm.SetCustomCaptionHeight(Value:Integer);
begin
 if FCustomCaptionHeight<>Value then
  begin
   FCustomCaptionHeight:=Value;
   if FForm<>nil then
    begin
     SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or
        SWP_NOREPOSITION or SWP_NOSIZE or SWP_NOACTIVATE);
     FForm.Invalidate;
    end;
  end;
end;

procedure TSXSkinCustomForm.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
  begin
   if AComponent=FSkinLibrary then
    FSkinLibrary:=nil;
  end;
end;

procedure TSXSkinCustomForm.SkinChanged;
begin
 ResetMaskRegion;
 CalcResizeRegions;
 CalcCaptionElementsRects;
 if FForm<>nil then
  SendMessage(FForm.Handle,WM_NCPAINT,0,0);
end;

procedure TSXSkinCustomForm.GetCurrentFState(var FState:TSXSkinFormStateParam;
           PSkinFilePath:PString=nil;PZipFilePath:PString=nil);
var  A:Integer;
 Style:TSXSkinFormStyle;
begin
 Finalize(FState);
 FillChar(FState,sizeof(FState),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed and (FForm<>nil) then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinFormStyle) then
    begin
     Style:=TSXSkinFormStyle(SkinLibrary.Styles[A]);
     if PSkinFilePath<>nil then
      PSkinFilePath^:=Style.SkinFilePath;
     if PZipFilePath<>nil then
      PZipFilePath^:=Style.ZipFilePath;
     Style.GetCurrentFState(FState,FForm.WindowState=wsMaximized,
        FForm.WindowState=wsMinimized,FFormActive);
    end;
  end;
end;

procedure TSXSkinCustomForm.DrawCaptionToRect(Bitmap:TBitmap32;Rect:TRect);
var   FState:TSXSkinFormStateParam;
 TextRect,RR:TRect;
         W,H:Integer;
       Flags:Cardinal;

 procedure DrawStyleToRect(CElementID:Integer;const StyleName:String;X,Y,Width,Height:Integer);
 var   A:Integer;
  GStyle:TSXSkinGeneralStyle;
   ARect:TRect;
     Rgn:HRGN;
 begin
  if (Width<=0) or (Height<=0) then exit;
  if StyleName<>'' then
   begin
    A:=SkinLibrary.Styles.GetGStyleIndex(StyleName,Width,Height);
    if A>=0 then
     begin
      GStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
      ARect:=Types.Rect(0,0,Width,Height);
      OffsetRect(ARect,Rect.Left,Rect.Top);
      Rgn:=CreateRectRgnIndirect(ARect);
      GStyle.DrawToBitmap(FForm,CElementID,Bitmap,X+Rect.Left,Y+Rect.Top,Width,
                          Height,ARect,Rgn,SkinLibrary);
      DeleteObject(Rgn);
     end;
   end;
 end;

 procedure DrawTextToBitmap(Bitmap:TBitmap32);
 begin
  SetDefaultFontData(FState.TextFont,FForm.Font);
  Bitmap.Font.Name:=FState.TextFont.FontName;
  Bitmap.Font.Size:=FState.TextFont.FontSize;
  Bitmap.Font.Style:=FState.TextFont.FontStyle;
  if FState.TextFont.HasShadow then
   begin
    OffsetRect(TextRect,1,1);
    if FState.TextFont.SmoothLevel=0 then
     begin
      if FState.TextFont.ShadowColor shr 24=$FF then
       begin
        Bitmap.Font.Color:=WinColor(FState.TextFont.ShadowColor);
        Bitmap.Textout(TextRect,Flags,FForm.Caption);
        end else DrawAlphaText(Bitmap,FForm.Caption,TextRect,Flags,FState.TextFont.ShadowColor);
     end else
      DrawSmoothText(Bitmap,FForm.Caption,TextRect,Flags,FState.TextFont.SmoothLevel,FState.TextFont.ShadowColor);
    OffsetRect(TextRect,-1,-1);
   end;
  if FState.TextFont.SmoothLevel=0 then
   begin
    if FState.TextFont.FontColor shr 24=$FF then
     begin
      Bitmap.Font.Color:=WinColor(FState.TextFont.FontColor);
      Bitmap.Textout(TextRect,Flags,FForm.Caption);
     end else DrawAlphaText(Bitmap,FForm.Caption,TextRect,Flags,FState.TextFont.FontColor);
   end else DrawSmoothText(Bitmap,FForm.Caption,TextRect,Flags,FState.TextFont.SmoothLevel,FState.TextFont.FontColor);
 end;

 procedure DrawButton(CElementID:Integer;const ButtonRect:TRect;
            ButtonOver,ButtonDown:Boolean;const ButtonStyle:String;ButtonEnabled:Boolean);
 var   A:Integer;
  BStyle:TSXSkinButtonStyle;
  BState:TSXSkinButtonStateParam;
 begin
  if not IsRectEmpty(ButtonRect) and (ButtonStyle<>'') then
   begin
    A:=SkinLibrary.Styles.GetIndexByName(ButtonStyle);
    if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
     begin
      BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
      BStyle.GetCurrentBState(BState,False,ButtonEnabled,ButtonOver,
                              ButtonOver and ButtonDown,ButtonDown);
      if BState.Style<>'' then
       DrawStyleToRect(CElementID,BState.Style,ButtonRect.Left,ButtonRect.Top,
            ButtonRect.Right-ButtonRect.Left,ButtonRect.Bottom-ButtonRect.Top);
     end;
   end;
 end;

begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed and (FForm<>nil) then
  begin
   GetCurrentFState(FState);
   if FState.CaptionStyle<>'' then
    DrawStyleToRect(CEID_Caption,FState.CaptionStyle,0,0,FForm.Width,FCaptionHeight);
   if not IsRectEmpty(FIconRect) then
    begin
     if FUseTFormIcon then
      begin
       H:=FForm.Icon.Handle;
       if H=0 then H:=Application.Icon.Handle;
       DrawIconEx(Bitmap.Handle,FIconRect.Left+Rect.Left,FIconRect.Top+Rect.Top,H,
                  FIconRect.Right-FIconRect.Left,FIconRect.Bottom-FIconRect.Top,
                  0,0,DI_NORMAL);
      end else DrawStyleToRect(CEID_Icon,FIconStyle,FIconRect.Left,FIconRect.Top,
                   FIconRect.Right-FIconRect.Left,FIconRect.Bottom-FIconRect.Top);
    end;
   //
   DrawButton(CEID_Close,FCloseRect,FCloseOver,FCloseDown,FState.CloseButton,biSystemMenu in FForm.BorderIcons);
   if FForm.WindowState=wsMaximized then
    DrawButton(CEID_Restore,FMaximizeRect,FMaximizeOver,FMaximizeDown,FState.RestoreButton,biMaximize in FForm.BorderIcons) else
     DrawButton(CEID_Maximize,FMaximizeRect,FMaximizeOver,FMaximizeDown,FState.MaximizeButton,biMaximize in FForm.BorderIcons);
   DrawButton(CEID_Minimize,FMinimizeRect,FMinimizeOver,FMinimizeDown,FState.MinimizeButton,biMinimize in FForm.BorderIcons);
   DrawButton(CEID_Help,FHelpRect,FHelpOver,FHelpDown,FState.HelpButton,True);
   //
   if FForm.Caption<>'' then
    begin
     Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP or DT_VCENTER or DT_SINGLELINE or
            DT_MODIFYSTRING or DT_END_ELLIPSIS;
     case FState.TextAlignment of
      taLeftJustify:  Flags:=Flags or DT_LEFT;
      taRightJustify: Flags:=Flags or DT_RIGHT;
      taCenter:       Flags:=Flags or DT_CENTER;
     end;
     TextRect:=FTextRect;
     if FState.TextFont.DoPrepaint and (FState.TextFont.SmoothLevel>0) and (FTextBitmap=nil) then
      begin
       FTextBitmap:=TBitmap32.Create;
       FTextBitmap.DrawMode:=dmBlend;
       FTextBitmap.CombineMode:=cmMerge;
       W:=TextRect.Right-TextRect.Left;
       H:=TextRect.Bottom-TextRect.Top;
       if FState.TextFont.HasShadow then
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
     OffsetRect(TextRect,-Rect.Left,-Rect.Top);
     if FState.TextFont.DoPrepaint and (FState.TextFont.SmoothLevel>0) then
      FTextBitmap.DrawTo(Bitmap,TextRect.Left,TextRect.Top) else
       DrawTextToBitmap(Bitmap);
    end;
  end;
end;

procedure TSXSkinCustomForm.DrawCaptionAndBorder(Rgn:HRGN);
var    DC:HDC;
        A:Integer;
    Style:TSXSkinFormStyle;
   FState:TSXSkinFormStateParam;
     Rgn2:HRGN;
       CB:TBitmap32;

 procedure DrawStyleToRect(CElementID:Integer;const StyleName:String;X,Y,Width,Height:Integer);
 var   A:Integer;
       B:TBitmap32;
  GStyle:TSXSkinGeneralStyle;
   ARect:TRect;
     Rgn:HRGN;
 begin
  if (Width<=0) or (Height<=0) then exit;
  if StyleName<>'' then
   begin
    B:=TBitmap32.Create;
    try
     B.SetSize(Width,Height);
     B.Draw(Rect(0,0,Width,Height),Rect(X,Y,X+Width,Y+Height),DC);
     A:=SkinLibrary.Styles.GetGStyleIndex(StyleName,Width,Height);
     if A>=0 then
      begin
       GStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       ARect:=Rect(0,0,Width,Height);
       Rgn:=CreateRectRgnIndirect(ARect);
       GStyle.DrawToBitmap(FForm,CElementID,B,0,0,Width,Height,ARect,Rgn,
                           SkinLibrary);
       DeleteObject(Rgn);
      end;
     B.DrawTo(DC,X,Y);
    finally
     B.Free;
    end;
   end;
 end;

begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed and (FForm<>nil) then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinFormStyle) then
    begin
     Style:=TSXSkinFormStyle(SkinLibrary.Styles[A]);
     if (FCaptionHeight=0) and (Style.LeftFrameWidth=0) and
        (Style.RightFrameWidth=0) and (Style.BottomFrameHeight=0) then exit;
     GetCurrentFState(FState);
//     Rgn2:=CreateRectRgn(0,0,0,0);
//     CombineRgn(Rgn2,Rgn,0,RGN_COPY);
//     OffsetRgn(Rgn2,-FForm.Left,-FForm.Top);
     //DC:=GetDCEx(FForm.Handle,Rgn,DCX_CACHE or DCX_WINDOW or DCX_INTERSECTRGN);
     DC:=GetWindowDC(FForm.Handle);
     //SelectClipRgn(DC,Rgn2);
     try
      CB:=TBitmap32.Create;
      try
       CB.SetSize(FForm.Width,FCaptionHeight);
       DrawCaptionToRect(CB,Rect(0,0,FForm.Width,FCaptionHeight));
       CB.DrawTo(DC,0,0);
      finally
       CB.Free;
      end;
      //if RectInRegion(Rgn2,Rect(0,0,FForm.Width,FCaptionHeight)) then
      // DrawStyleToRect(FState.CaptionStyle,0,0,FForm.Width,FCaptionHeight);
      //if RectInRegion(Rgn2,Rect(0,FCaptionHeight,Style.LeftFrameWidth,FForm.Height-Style.BottomFrameHeight)) then
       DrawStyleToRect(CEID_LeftFrame,FState.LeftFrameStyle,0,FCaptionHeight,
                       Style.LeftFrameWidth,FForm.Height-Style.BottomFrameHeight-FCaptionHeight);
      //if RectInRegion(Rgn2,Rect(FForm.Width-Style.RightFrameWidth,FCaptionHeight,FForm.Width,FForm.Height-Style.BottomFrameHeight)) then
       DrawStyleToRect(CEID_RightFrame,FState.RightFrameStyle,FForm.Width-Style.RightFrameWidth,
                       FCaptionHeight,Style.RightFrameWidth,FForm.Height-Style.BottomFrameHeight-FCaptionHeight);
      //if RectInRegion(Rgn2,Rect(0,FForm.Height-Style.BottomFrameHeight,FForm.Width,FForm.Height)) then
       DrawStyleToRect(CEID_BottomFrame,FState.BottomFrameStyle,0,FForm.Height-Style.BottomFrameHeight,
                       FForm.Width,Style.BottomFrameHeight);
     finally
      //SelectClipRgn(DC,0);
      ReleaseDC(FForm.Handle,DC);
//      DeleteObject(Rgn2);
     end;
    end;
  end;
end;

function TSXSkinCustomForm.OnGetVariable(const VarName:String;var Error:Boolean):Single;
begin
 Result:=0;
 if VarName='W' then
  begin
   Result:=TmpWidth;
   exit;
  end;
 if VarName='H' then
  begin
   Result:=TmpHeight;
   exit;
  end;
 if VarName='DefCH' then
  begin
   Result:=GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYFRAME);
   exit;
  end;
 if VarName='CH' then
  begin
   Result:=FCaptionHeight;
   exit;
  end;
 if VarName='IL' then
  begin
   Result:=FIconRect.Left;
   exit;
  end;
 if VarName='IT' then
  begin
   Result:=FIconRect.Top;
   exit;
  end;
 if VarName='IR' then
  begin
   Result:=FIconRect.Right;
   exit;
  end;
 if VarName='IB' then
  begin
   Result:=FIconRect.Bottom;
   exit;
  end;
 Error:=True;
end;

procedure TSXSkinCustomForm.CalcCaptionElementsRects;
var FState:TSXSkinFormStateParam;
         R:TRect;
      Need:Boolean;

 procedure ModifyTextRect;
 const Margin=3;
 begin
  if R.Left<=FTextRect.Left then
   FTextRect.Left:=R.Right+Margin else
  if R.Right>=FTextRect.Right then
   FTextRect.Right:=R.Left-Margin else
  if R.Left-FTextRect.Left>FTextRect.Right-R.Right then
   FTextRect.Right:=R.Left-Margin else
    FTextRect.Left:=R.Right+Margin;
 end;

begin
 if (FForm=nil) or (csDesigning in ComponentState) then exit;
 GetCurrentFState(FState,nil,nil);
 //
 Need:=(FForm.BorderStyle in [bsSizeable,bsSingle]) and (biSystemMenu in FForm.BorderIcons);
 if Need and (FState.IconRect<>'') then
  GetRectFromString(FState.IconRect,FIconRect,OnGetVariable) else
   FIconRect:=Rect(0,0,0,0);
 //
 if FState.TextRect<>'' then
  GetRectFromString(FState.TextRect,FTextRect,OnGetVariable) else
   FTextRect:=Rect(0,0,0,0);
 //
 Need:=(FForm.BorderStyle in [bsSizeable,bsSingle,bsDialog,bsToolWindow,bsSizeToolWin]) and
       (biSystemMenu in FForm.BorderIcons);
 if Need and (FState.CloseRect<>'') then
  GetRectFromString(FState.CloseRect,FCloseRect,OnGetVariable) else
   FCloseRect:=Rect(0,0,0,0);
 //
 Need:=(FForm.BorderStyle in [bsSizeable,bsSingle]) and (biSystemMenu in FForm.BorderIcons) and
       ((biMinimize in FForm.BorderIcons) or (biMaximize in FForm.BorderIcons));
 if Need and (FState.MaximizeRect<>'') then
  GetRectFromString(FState.MaximizeRect,FMaximizeRect,OnGetVariable) else
   FMaximizeRect:=Rect(0,0,0,0);
 //
 Need:=(FForm.BorderStyle in [bsSizeable,bsSingle]) and (biSystemMenu in FForm.BorderIcons) and
       ((biMinimize in FForm.BorderIcons) or (biMaximize in FForm.BorderIcons));
 if Need and (FState.MinimizeRect<>'') then
  GetRectFromString(FState.MinimizeRect,FMinimizeRect,OnGetVariable) else
   FMinimizeRect:=Rect(0,0,0,0);
 //
 Need:=(FForm.BorderStyle in [bsSizeable,bsSingle,bsDialog]) and
       (biSystemMenu in FForm.BorderIcons) and not (biMinimize in FForm.BorderIcons) and
       not (biMaximize in FForm.BorderIcons) and (biHelp in FForm.BorderIcons);
 if Need and (FState.HelpRect<>'') then
  GetRectFromString(FState.HelpRect,FHelpRect,OnGetVariable) else
   FHelpRect:=Rect(0,0,0,0);
 //
 if not IsRectEmpty(FTextRect) then
  begin
   if IntersectRect(R,FTextRect,FCloseRect) then
    ModifyTextRect;
   if IntersectRect(R,FTextRect,FMaximizeRect) then
    ModifyTextRect;
   if IntersectRect(R,FTextRect,FMinimizeRect) then
    ModifyTextRect;
   if IntersectRect(R,FTextRect,FHelpRect) then
    ModifyTextRect;
   if IntersectRect(R,FTextRect,FIconRect) then
    ModifyTextRect;
  end;
end;

procedure TSXSkinCustomForm.ResetMaskRegion;
var    FState:TSXSkinFormStateParam;
           R1:HRGN;
  BH,RW,CH,CW:Integer;
 SkinFilePath:String;
  ZipFilePath:String;

 procedure AddRegion(const MaskRegion:String;X,Y,Width,Height:Integer;Resized:Boolean);
 var R2:HRGN;
 begin
  if MaskRegion<>'' then
   begin
    TmpWidth:=Width;
    TmpHeight:=Height;
    R2:=EvaluateRegion(MaskRegion,SkinFilePath,ZipFilePath,Width,Height,
                       SkinLibrary,Resized,OnGetVariable);
    if (X<>0) or (Y<>0) then
     OffsetRgn(R2,X,Y);
   end else
    begin
     R2:=CreateRectRgn(X,Y,X+Width,Y+Height);
    end;
  CombineRgn(R1,R1,R2,RGN_OR);
  DeleteObject(R2);
 end;

begin
 if (FForm=nil) or (csDesigning in ComponentState) then exit;
 GetCurrentFState(FState,@SkinFilePath,@ZipFilePath);
 if FState.FullMask<>'' then
  begin
   TmpWidth:=FForm.Width;
   TmpHeight:=FForm.Height;
   R1:=EvaluateRegion(FState.FullMask,SkinFilePath,ZipFilePath,FForm.Width,FForm.Height,
                       SkinLibrary,False,OnGetVariable);
   SetWindowRgn(FForm.Handle,R1,True);
   DeleteObject(R1);
   exit;
  end;
 if ((FState.MaskLeftWidth<>0) or (FState.MaskRightWidth<>0)) and
    ((FState.MaskTopHeight<>0) or (FState.MaskBottomHeight<>0)) then
  begin
   R1:=CreateRectRgn(FState.MaskLeftWidth,FState.MaskTopHeight,
                     FForm.Width-FState.MaskRightWidth,
                     FForm.Height-FState.MaskBottomHeight);
   //
   BH:=FForm.Height-FState.MaskTopHeight;
   if FState.MaskBottomHeight<BH then
    BH:=FState.MaskBottomHeight;
   RW:=FForm.Width-FState.MaskLeftWidth;
   if FState.MaskRightWidth<RW then
    RW:=FState.MaskRightWidth;
   CH:=FForm.Height-FState.MaskTopHeight-FState.MaskBottomHeight;
   if CH<0 then CH:=0;
   CW:=FForm.Width-FState.MaskLeftWidth-FState.MaskRightWidth;
   if CW<0 then CW:=0;
   //
   if FState.MaskTopHeight>0 then
    begin
     if FState.MaskLeftWidth>0 then
      AddRegion(FState.MaskTopLeft,0,0,FState.MaskLeftWidth,FState.MaskTopHeight,True);
     if CW>0 then
      AddRegion(FState.MaskTop,FState.MaskLeftWidth,0,CW,FState.MaskTopHeight,False);
     if RW>0 then
      AddRegion(FState.MaskTopRight,FForm.Width-RW,0,RW,FState.MaskTopHeight,True);
    end;
   if CH>0 then
    begin
     if FState.MaskLeftWidth>0 then
      AddRegion(FState.MaskLeft,0,FState.MaskTopHeight,FState.MaskLeftWidth,CH,False);
     if RW>0 then
      AddRegion(FState.MaskRight,FForm.Width-RW,FState.MaskTopHeight,RW,CH,False);
    end;
   if BH>0 then
    begin
     if FState.MaskLeftWidth>0 then
      AddRegion(FState.MaskBottomLeft,0,FForm.Height-BH,FState.MaskLeftWidth,BH,True);
     if CW>0 then
      AddRegion(FState.MaskBottom,FState.MaskLeftWidth,FForm.Height-BH,CW,BH,False);
     if RW>0 then
      AddRegion(FState.MaskBottomRight,FForm.Width-RW,FForm.Height-BH,RW,BH,True);
    end;
   SetWindowRgn(FForm.Handle,R1,True);
   DeleteObject(R1);
  end else
   begin
    SetWindowRgn(FForm.Handle,0,True);
   end;
end;

procedure TSXSkinCustomForm.CalcResizeRegions;
var    FState:TSXSkinFormStateParam;
 SkinFilePath:String;
  ZipFilePath:String;
begin
 if ResizeTopLeftRgn<>0 then
  DeleteObject(ResizeTopLeftRgn);
 if ResizeTopRightRgn<>0 then
  DeleteObject(ResizeTopRightRgn);
 if ResizeBottomLeftRgn<>0 then
  DeleteObject(ResizeBottomLeftRgn);
 if ResizeBottomRightRgn<>0 then
  DeleteObject(ResizeBottomRightRgn);
 if ResizeLeftRgn<>0 then
  DeleteObject(ResizeLeftRgn);
 if ResizeRightRgn<>0 then
  DeleteObject(ResizeRightRgn);
 if ResizeTopRgn<>0 then
  DeleteObject(ResizeTopRgn);
 if ResizeBottomRgn<>0 then
  DeleteObject(ResizeBottomRgn);
 if CaptionRgn<>0 then
  DeleteObject(CaptionRgn);
 //
 if FForm=nil then exit;
 TmpWidth:=FForm.Width;
 TmpHeight:=FForm.Height;
 GetCurrentFState(FState,@SkinFilePath,@ZipFilePath);
 if FState.ResizeTopLeft<>'' then
  ResizeTopLeftRgn:=EvaluateRegion(FState.ResizeTopLeft,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeTopRight<>'' then
  ResizeTopRightRgn:=EvaluateRegion(FState.ResizeTopRight,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeBottomLeft<>'' then
  ResizeBottomLeftRgn:=EvaluateRegion(FState.ResizeBottomLeft,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeBottomRight<>'' then
  ResizeBottomRightRgn:=EvaluateRegion(FState.ResizeBottomRight,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeTop<>'' then
  ResizeTopRgn:=EvaluateRegion(FState.ResizeTop,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeBottom<>'' then
  ResizeBottomRgn:=EvaluateRegion(FState.ResizeBottom,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeLeft<>'' then
  ResizeLeftRgn:=EvaluateRegion(FState.ResizeLeft,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.ResizeRight<>'' then
  ResizeRightRgn:=EvaluateRegion(FState.ResizeRight,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
 if FState.CaptionRegion<>'' then
  CaptionRgn:=EvaluateRegion(FState.CaptionRegion,SkinFilePath,ZipFilePath,
     FForm.Width,FForm.Height,SkinLibrary,False,OnGetVariable);
end;

function TSXSkinCustomForm.HitTest(X,Y:Integer):Integer;
begin
 if PtInRect(FIconRect,Point(X,Y)) then
  begin
   Result:=HTSYSMENU;
   exit;
  end;
 if PtInRect(FCloseRect,Point(X,Y)) then
  begin
   Result:=HTCLOSE;
   exit;
  end;
 if PtInRect(FMaximizeRect,Point(X,Y)) then
  begin
   Result:=HTMAXBUTTON;
   exit;
  end;
 if PtInRect(FMinimizeRect,Point(X,Y)) then
  begin
   Result:=HTMINBUTTON;
   exit;
  end;
 if PtInRect(FHelpRect,Point(X,Y)) then
  begin
   Result:=HTHELP;
   exit;
  end;
 if FForm.BorderStyle in [bsSizeable,bsSizeToolWin] then
  begin
   if (ResizeTopRgn<>0) and PtInRegion(ResizeTopRgn,X,Y) then
    begin
     Result:=HTTOP;
     exit;
    end;
   if (ResizeTopLeftRgn<>0) and PtInRegion(ResizeTopLeftRgn,X,Y) then
    begin
     Result:=HTTOPLEFT;
     exit;
    end;
   if (ResizeTopRightRgn<>0) and PtInRegion(ResizeTopRightRgn,X,Y) then
    begin
     Result:=HTTOPRIGHT;
     exit;
    end;
   if (ResizeBottomRightRgn<>0) and PtInRegion(ResizeBottomRightRgn,X,Y) then
    begin
     Result:=HTBOTTOMRIGHT;
     exit;
    end;
   if (ResizeRightRgn<>0) and PtInRegion(ResizeRightRgn,X,Y) then
    begin
     Result:=HTRIGHT;
     exit;
    end;
   if (ResizeBottomRgn<>0) and PtInRegion(ResizeBottomRgn,X,Y) then
    begin
     Result:=HTBOTTOM;
     exit;
    end;
   if (ResizeLeftRgn<>0) and PtInRegion(ResizeLeftRgn,X,Y) then
    begin
     Result:=HTLEFT;
     exit;
    end;
   if (ResizeBottomLeftRgn<>0) and PtInRegion(ResizeBottomLeftRgn,X,Y) then
    begin
     Result:=HTBOTTOMLEFT;
     exit;
    end;
  end;
 if (CaptionRgn<>0) and PtInRegion(CaptionRgn,X,Y) then
  begin
   Result:=HTCAPTION;
   exit;
  end;
 Result:=HTCLIENT;
end;

procedure TSXSkinCustomForm.ProcessNCMouseMove;
var   PT:TPoint;
 Changed:Boolean;

 procedure CheckButton(const ButtonRect:TRect;var ButtonOver:Boolean);
 var IsOver:Boolean;
 begin
  IsOver:=PtInRect(ButtonRect,PT);
  if IsOver<>ButtonOver then
   begin
    ButtonOver:=not ButtonOver;
    Changed:=True;
   end;
 end;

begin
 PT:=Mouse.CursorPos;
 Dec(PT.X,FForm.Left);
 Dec(PT.Y,FForm.Top);
 Changed:=False;
 CheckButton(FCloseRect,FCloseOver);
 if biMaximize in FForm.BorderIcons then
  CheckButton(FMaximizeRect,FMaximizeOver);
 if biMinimize in FForm.BorderIcons then
  CheckButton(FMinimizeRect,FMinimizeOver);
 if biHelp in FForm.BorderIcons then
  CheckButton(FHelpRect,FHelpOver);  
 if Changed then
  SendMessage(FForm.Handle,WM_NCPAINT,0,0);
end;

procedure TSXSkinCustomForm.NewWndProc(var Message:TMessage);
var PT:TPoint;
     A:Integer;
     E:Boolean;
   Rgn:HRGN;
//     H:HBRUSH;
//     R:TRect;
 Style:TSXSkinFormStyle;
begin
 case Message.Msg of
  WM_NCACTIVATE:        begin
                         OldWindowProc(Message);
                         FFormActive:=TWMNCActivate(Message).Active;
                         SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                        end;
  WM_NCCALCSIZE:        begin
                         with TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0] do
                          begin
                           if not TWMNCCalcSize(Message).CalcValidRects then
                            TWMNCCalcSize(Message).Result:=0 else
                             begin
                              TWMNCCalcSize(Message).Result:=0{ or WVR_REDRAW};
                             end;
                           if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed and (FForm<>nil) then
                            begin
                             if (FForm.BorderStyle in [bsToolWindow,bsSizeToolWin]) and
                                (SkinStyle='_Form') then
                              SkinStyle:='_FormSmallCaption' else
                             if (FForm.BorderStyle in [bsDialog,bsSingle,bsSizeable]) and
                                (SkinStyle='_FormSmallCaption') then
                              SkinStyle:='_Form';
                             A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
                             if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinFormStyle) then
                              begin
                               Style:=TSXSkinFormStyle(SkinLibrary.Styles[A]);
                               Inc(Left,Style.LeftFrameWidth);
                               TmpWidth:=FForm.Width;
                               TmpHeight:=FForm.Height;
                               if FCustomCaptionHeight=0 then
                                begin
                                 FCaptionHeight:=round(SXEvalMathString(Style.CaptionHeight,OnGetVariable,E));
                                 if E then FCaptionHeight:=0;
                                end else FCaptionHeight:=FCustomCaptionHeight;
                               Inc(Top,FCaptionHeight);
                               Dec(Right,Style.RightFrameWidth);
                               Dec(Bottom,Style.BottomFrameHeight);
                               if Right<Left then Right:=Left;
                               if Bottom<Top then Bottom:=Top;
                              end;
                            end;
                          end;
                         //if //(csDesigning in ComponentState) and (FForm.Parent<>nil) and
                         //   (FForm.Menu<>nil) and (FForm.Menu.Items.Count>0) then
                         // Inc(TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0].Top,GetSystemMetrics(SM_CYMENU));
                        end;
  WM_NCHITTEST:         begin
                         if FCaptionHeight>0 then
                          begin
                           PT:=Point(Message.LParam and $FFFF,Message.LParam shr 16);
                           NormalizeWinPoint(PT);
                           if FForm<>nil then
                            begin
                             Dec(PT.X,FForm.Left);
                             Dec(PT.Y,FForm.Top);
                            end;
                           Message.Result:=HitTest(PT.X,PT.Y);
                          end else OldWindowProc(Message);
                        end;
  WM_NCMOUSEMOVE,
  WM_NCMOUSELEAVE:      begin
                         ProcessNCMouseMove;
                         OldWindowProc(Message);
                        end;
  WM_NCLBUTTONDOWN:     begin
                         if FCloseOver then
                          begin
                           FCloseDown:=True;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                          end else
                         if FMaximizeOver then
                          begin
                           FMaximizeDown:=True;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                          end else
                         if FMinimizeOver then
                          begin
                           FMinimizeDown:=True;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                          end else
                         if FHelpOver then
                          begin
                           FHelpDown:=True;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                          end else
                         OldWindowProc(Message);
                        end;
  WM_NCLBUTTONUP,
  WM_LBUTTONUP:         begin
                         if FCloseDown then
                          begin
                           FCloseDown:=False;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                           if FCloseOver then
                            SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_CLOSE,0);
                          end else
                         if FMaximizeDown then
                          begin
                           FMaximizeDown:=False;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                           if FMaximizeOver then
                            begin
                             if FForm.WindowState=wsMaximized then
                              SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_RESTORE,0) else
                               SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_MAXIMIZE,0);
                            end;
                          end else
                         if FMinimizeDown then
                          begin
                           FMinimizeDown:=False;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                           if FMinimizeOver then
                            SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_MINIMIZE,0);
                          end else
                         if FHelpDown then
                          begin
                           FHelpDown:=False;
                           SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                           if FHelpOver then
                            SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_CONTEXTHELP,0);
                          end else
                           OldWindowProc(Message);
                        end;
  {WM_NCRBUTTONUP:       begin
                         //OldWindowProc(Message);
                         P:=Mouse.CursorPos;
                         SendMessage(FForm.Handle,WM_SYSCOMMAND,SC_MOUSEMENU,MakeLong(TWMNCRButtonUp(Message).XCursor,TWMNCRButtonUp(Message).YCursor));
                        end;}
  WM_NCPAINT:           begin
                         Rgn:=Message.WParam;
                         if (Rgn=0) or (Rgn=1) then
                          begin
                           Rgn:=CreateRectRgn(FForm.Left,FForm.Top,FForm.Left+FForm.Width,
                                              FForm.Top+FForm.Height);
                           DrawCaptionAndBorder(Rgn);
                           DeleteObject(Rgn);
                          end else
                           DrawCaptionAndBorder(Rgn);
                         //OffsetRgn(Rgn,-FForm.Left,-FForm.Top);
                         //if Rgn=1 then
                         // Rgn:=CreateRectRgn(FForm.Left,FForm.Top,FForm.Left+FForm.Width,
                         //                     FForm.Top+FForm.Height);
                         //P:=Point(0,0);
                         //MapWindowPoints(0,FForm.Handle,P,1);
                         //OffsetRgn(Rgn,-LastLeft,-LastTop);
                         //H:=CreateSolidBrush(RGB(random(256),random(256),random(256)));
                         //FillRgn(GetWindowDC(FForm.Handle),Rgn,H);
                         //DeleteObject(H);
                         Message.Result:=0;
                        end;
  WM_WINDOWPOSCHANGING: begin
                         LastLeft:=FForm.Left;
                         LastTop:=FForm.Top;
                         OldWindowProc(Message);
                        end;
  WM_SIZE:              begin
                         OldWindowProc(Message);
                         ResetMaskRegion;
                         CalcResizeRegions;
                         CalcCaptionElementsRects;
                         SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                         exit;
                        end;
  WM_STYLECHANGED:      begin
                         ResetMaskRegion;
                         CalcResizeRegions;
                         CalcCaptionElementsRects;
                         OldWindowProc(Message);
//                         SetWindowPos(FForm.ClientHandle,0,FForm.Left,FForm.Top,FForm.Width,FForm.Height,
//                              0{SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER});

                        end;
  WM_SETTEXT:           begin
                         OldWindowProc(Message);
                         SendMessage(FForm.Handle,WM_NCPAINT,0,0);
                        end;
  else                  OldWindowProc(Message);
 end;
end;

constructor TSXSkinCustomForm.Create(AOwner:TComponent);
var A:Integer;
begin
 inherited;
 if not (AOwner is TForm) then raise Exception.Create(AOwner.Name+' is not a form!');
 FUseTFormIcon:=True;
 FForm:=TForm(AOwner);
 if FForm=nil then exit;
 SkinStyle:='_Form';
 IconStyle:='_Selective_StdIcon';
 if not (csLoading in ComponentState) and (csDesigning in ComponentState) and
    (SkinLibrary=nil) then
  begin
   for A:=0 to FForm.ComponentCount-1 do
    if FForm.Components[A] is TSXSkinLibrary then
     begin
      SkinLibrary:=TSXSkinLibrary(FForm.Components[A]);
      break;
     end;
  end;
 if not (csDesigning in ComponentState) then
  begin
   OldWindowProc:=FForm.WindowProc;
   FForm.WindowProc:=NewWndProc;
   TSXForm(FForm).ReCreateWnd;
   SetWindowLong(FForm.Handle,GWL_STYLE,GetWindowLong(FForm.Handle,GWL_STYLE) and not WS_CAPTION);
  end;
end;

destructor TSXSkinCustomForm.Destroy;
begin
 if ResizeTopLeftRgn<>0 then
  DeleteObject(ResizeTopLeftRgn);
 if ResizeTopRightRgn<>0 then
  DeleteObject(ResizeTopRightRgn);
 if ResizeBottomLeftRgn<>0 then
  DeleteObject(ResizeBottomLeftRgn);
 if ResizeBottomRightRgn<>0 then
  DeleteObject(ResizeBottomRightRgn);
 if ResizeLeftRgn<>0 then
  DeleteObject(ResizeLeftRgn);
 if ResizeRightRgn<>0 then
  DeleteObject(ResizeRightRgn);
 if ResizeTopRgn<>0 then
  DeleteObject(ResizeTopRgn);
 if ResizeBottomRgn<>0 then
  DeleteObject(ResizeBottomRgn);
 if CaptionRgn<>0 then
  DeleteObject(CaptionRgn);
 FTextBitmap.Free;
 inherited;
end;

end.
