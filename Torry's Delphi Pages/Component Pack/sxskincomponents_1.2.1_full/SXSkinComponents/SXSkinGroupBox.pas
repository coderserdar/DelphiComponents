unit SXSkinGroupBox;

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

const

 VARGB_W   =  1;
 VARGB_H   =  2;
 VARGB_CL  =  3;
 VARGB_CT  =  4;
 VARGB_CR  =  5;
 VARGB_CB  =  6;
 VARGB_GlX =  7;
 VARGB_GlY =  8;
 VARGB_GlW =  9;
 VARGB_GlH = 10;
 VARGB_TL  = 11;
 VARGB_TT  = 12;
 VARGB_TR  = 13;
 VARGB_TB  = 14;

type

 TSXGroupBoxGlyphType=(gtNone,
                       gtSimple,
                       gtCheckBox,
                       gtRadioButton);

 TSXGroupBoxGlyphPosition=(gbgpLeft,gbgpRight);

 TSXSkinCustomGroupBox=class;

 TSXSkinCustomGroupBoxThread=class(TThread)
  public
   Control:TSXSkinCustomGroupBox;
   constructor Create;
   procedure Execute; override;
   procedure DoEvent;
 end;

 TSXSkinGroupBoxResetParam=(gbrpText,
                            gbrpTextOnFontChange,
                            gbrpTextIfWordWrap,
                            gbrpGlyph,
                            gbrpGlyphOnStyleChange,
                            gbrpGlyphOnSizeChange,
                            gbrpInvalidateOnStyleChange,
                            gbrpTransparentRect);

 TSXSkinGroupBoxResetParams=set of TSXSkinGroupBoxResetParam;

 TSXGroupBoxVariableComparer=class(TSXVariableComparer)
  private
   Control:TSXSkinCustomGroupBox;
   function GetValue(VarID:Integer):Integer;
  protected
   function VarListOnGetVariable(const VarName:String;var Error:Boolean):Single; override;
  public
   function GetVarValsForVarList(VarList:TList):TList; override;
   function Changed(VarList:TList;OldVarVals:TList):Boolean; override;
   procedure Update(VarList:TList;VarVals:TList); override;
   procedure DestroyVarList(VarList:TList); override;
   procedure DestroyVarVals(VarList:TList;VarVals:TList); override;
 end;

 TSXSkinCustomGroupBox=class(TSXSkinCustomControl)
  private
   FGlyphType:TSXGroupBoxGlyphType;
   FWordWrap:Boolean;
   FCaptionAlignment:TAlignment;
   FAlignment:TAlignment;
   FOnMouseEnter:TNotifyEvent;
   FOnMouseLeave:TNotifyEvent;
   FGlyphStyle:String;
   FState:TCheckBoxState;
   FAllowGrayed:Boolean;
   FTextOffset:Integer;
   FGlyphPosition:TSXGroupBoxGlyphPosition;
   FUseLabelStyle:Boolean;
   FOnChange:TNotifyEvent;
   FOnUserModified:TNotifyEvent;
   //
   FMouseOver:Boolean;
   FDown:Boolean;
   FCaptionRect:TRect;
   FGlyphRect:TRect;
   FTextRect:TRect;
   FTransparentRect:TRect;
   FTextBitmap:TBitmap32;
   FLastFontData:TSXFontData;
   FLastGlyphStyle:String;
   FLastGBStyle:String;
   FLastGBOverStyle:String;
   FLastTextLeftOffset:Integer;
   FLastTextTopOffset:Integer;
   FLastTextRightOffset:Integer;
   FLastTextBottomOffset:Integer;
   //
   FLastGlyphTransform:TSXTransformEffectData;
   FThread:TSXSkinCustomGroupBoxThread;
   FLastGlyph:TBitmap32;
   FLastFocused:Boolean;
   FDoneSteps:Integer;
   //
   CEID_Back:Integer;
   CEID_Glyph:Integer;
   VComparer:TSXGroupBoxVariableComparer;
   procedure SetCaption(const Value:TCaption);
   procedure SetAlignment(Value:TAlignment);
   procedure SetCaptionAlignment(Value:TAlignment);
   procedure SetWordWrap(Value:Boolean);
   procedure SetGlyphType(Value:TSXGroupBoxGlyphType);
   procedure SetTextOffset(Value:Integer);
   procedure SetGlyphPosition(Value:TSXGroupBoxGlyphPosition);
   procedure SetUseLabelStyle(Value:Boolean);
   procedure SetAllowGrayed(Value:Boolean);
   procedure SetState(Value:TCheckBoxState);
   procedure SetChecked(Value:Boolean);
   procedure SetGlyphStyle(const Value:String);
   function HasUnusualSkinStyle:Boolean;
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
   procedure WMSetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
   procedure WMKillFocus(var Msg:TWMKillFocus); message WM_KILLFOCUS;
   procedure GetCurrentFontData(var FD:TSXFontData);
   procedure GetCurrentGlyphStyle(var GStyle:String;PGlyphWidth:PInteger=nil;
              PGlyphHeight:PInteger=nil);
   procedure CreateThreadIfNeeded;
   procedure DoThreadActions;
   function CreateCurrentGlyph:TBitmap32;
   function CreateCurrentBlendedGlyph:TBitmap32;
   procedure StartGlyphChanging(T:TSXGlyphChangeAction);
   procedure GetCurrentGlyphTransformEffect(Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
   procedure ResetGroupBoxParams(Params:TSXSkinGroupBoxResetParams=[]);
   procedure GetCurrentGBState(var GBState:TSXSkinGroupBoxStateParam);
  protected
   function CapturesMouseAt(X,Y:Integer):Boolean; override;
   procedure SetEnabled(Value:Boolean); override;
   procedure DoClick;
   procedure Loaded; override;
   function GetCaption:TCaption;
   function GetChecked:Boolean;
   procedure InternalMouseEnter;
   procedure InternalMouseLeave;
   procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
   procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
   procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure DoKeyDown(var Msg:TMessage); message CN_KEYDOWN;
   procedure DoKeyUp(var Msg:TMessage); message CN_KEYUP;
   procedure AdjustClientRect(var Rect:TRect); override;
   property Caption:TCaption read GetCaption write SetCaption;
   property WordWrap:Boolean read FWordWrap write SetWordWrap default False;
   property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
   property CaptionAlignment:TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
   property TextOffset:Integer read FTextOffset write SetTextOffset default 2;
   property GlyphPosition:TSXGroupBoxGlyphPosition read FGlyphPosition write SetGlyphPosition default gbgpLeft;
   property UseLabelStyle:Boolean read FUseLabelStyle write SetUseLabelStyle default True;
   property AllowGrayed:Boolean read FAllowGrayed write SetAllowGrayed default False;
   property Checked:Boolean read GetChecked write SetChecked stored False;
   property GlyphStyle:String read FGlyphStyle write SetGlyphStyle;
   property SkinStyle stored HasUnusualSkinStyle;
  public
   function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
   procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;  
   procedure DoUncheck;
   procedure InvalidateGroupBoxArea;
   procedure InvalidateGlyph;
   procedure InvalidateText;
   procedure Click; reintroduce;
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   property GlyphType:TSXGroupBoxGlyphType read FGlyphType write SetGlyphType default gtNone;
   property State:TCheckBoxState read FState write SetState default cbUnchecked; 
  published
   property OnChange:TNotifyEvent read FOnChange write FOnChange;
   property OnUserModified:TNotifyEvent read FOnUserModified write FOnUserModified;
   property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
 end;

 TSXSkinGroupBox=class(TSXSkinCustomGroupBox)
  published
   property Align;
   property Alignment;
   property AllowGrayed;
   property Anchors;
   property Caption;
   property CaptionAlignment;
   property Checked;
   property Color;
   property Constraints;
   property Cursor;
   property DragCursor;
   property Enabled;
   property Font;
   property GlyphPosition;
   property GlyphStyle;
   property GlyphType;
   //property HintData;
   property ParentColor;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property SkinLibrary;
   property SkinStyle;
   property State;
   property TabOrder;
   property TabStop default False;   
   property TextOffset;
   property UseLabelStyle;
   property Visible;
   property WordWrap;
   property OnCanResize;
   property OnChange;
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
   property OnUserModified;
 end;

implementation

uses Math, SXBitmap32Utils, SXSkinRadioButton;

{ TSXSkinCustomGroupBoxThread }

constructor TSXSkinCustomGroupBoxThread.Create;
begin
 inherited Create(True);
 FreeOnTerminate:=False;
end;

procedure TSXSkinCustomGroupBoxThread.Execute;
begin
 while not Terminated do
  begin
   SleepEx(30,True);
   if not Suspended then
    Synchronize(DoEvent);
  end;
end;

procedure TSXSkinCustomGroupBoxThread.DoEvent;
begin
 if Assigned(Control) then
  Control.DoThreadActions;
end;

{ TSXGroupBoxVariableComparer }

function TSXGroupBoxVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARGB_W else
 if VarName='H' then
  CurVarVal:=VARGB_H else
 if VarName='CL' then
  CurVarVal:=VARGB_CL else
 if VarName='CT' then
  CurVarVal:=VARGB_CT else
 if VarName='CR' then
  CurVarVal:=VARGB_CR else
 if VarName='CB' then
  CurVarVal:=VARGB_CB else
 if VarName='GlX' then
  CurVarVal:=VARGB_GlX else
 if VarName='GlY' then
  CurVarVal:=VARGB_GlY else
 if VarName='GlW' then
  CurVarVal:=VARGB_GlW else
 if VarName='GlH' then
  CurVarVal:=VARGB_GlH else
 if VarName='TL' then
  CurVarVal:=VARGB_TL else
 if VarName='TT' then
  CurVarVal:=VARGB_TT else
 if VarName='TR' then
  CurVarVal:=VARGB_TR else
 if VarName='TB' then
  CurVarVal:=VARGB_TB;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXGroupBoxVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 if Control<>nil then
  begin
   case VarID of
    VARGB_W:   Result:=Control.Width;
    VARGB_H:   Result:=Control.Height;
    VARGB_CL:  Result:=Control.FCaptionRect.Left;
    VARGB_CT:  Result:=Control.FCaptionRect.Top;
    VARGB_CR:  Result:=Control.FCaptionRect.Right;
    VARGB_CB:  Result:=Control.FCaptionRect.Bottom;
    VARGB_GlX: Result:=Control.FGlyphRect.Left;
    VARGB_GlY: Result:=Control.FGlyphRect.Top;
    VARGB_GlW: Result:=Control.FGlyphRect.Right-Control.FGlyphRect.Left;
    VARGB_GlH: Result:=Control.FGlyphRect.Bottom-Control.FGlyphRect.Top;
    VARGB_TL:  Result:=Control.FTextRect.Left;
    VARGB_TT:  Result:=Control.FTextRect.Top;
    VARGB_TR:  Result:=Control.FTextRect.Right;
    VARGB_TB:  Result:=Control.FTextRect.Bottom;
   end;
  end;
end;

function TSXGroupBoxVariableComparer.GetVarValsForVarList(VarList:TList):TList;
var A:Integer;
begin
 if VarList=nil then
  begin
   Result:=nil;
   exit;
  end;
 Result:=TList.Create;
 for A:=0 to VarList.Count-1 do
  Result.Add(Pointer(GetValue(Integer(VarList[A]))));
end;

function TSXGroupBoxVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
var A:Integer;
begin
 Result:=False;
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  if Integer(OldVarVals[A])<>GetValue(Integer(VarList[A])) then
   begin
    Result:=True;
    exit;
   end;
end;

procedure TSXGroupBoxVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXGroupBoxVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXGroupBoxVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

{ TSXSkinCustomGroupBox }

function TSXSkinCustomGroupBox.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_GroupBox';
end;

procedure TSXSkinCustomGroupBox.SetAlignment(Value:TAlignment);
begin
 if Value<>FAlignment then
  begin
   FAlignment:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpText]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetCaptionAlignment(Value:TAlignment);
begin
 if Value<>FCaptionAlignment then
  begin
   FCaptionAlignment:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams;
  end;
end;

procedure TSXSkinCustomGroupBox.SetWordWrap(Value:Boolean);
begin
 if Value<>FWordWrap then
  begin
   FWordWrap:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpText]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetGlyphType(Value:TSXGroupBoxGlyphType);
begin
 if Value<>FGlyphType then
  begin
   FGlyphType:=Value;
   if not (csLoading in ComponentState) then
    begin
     if csDesigning in ComponentState then
      begin
       case FGlyphType of
        gtNone:        FGlyphStyle:='';
        gtCheckBox:    if Copy(FGlyphStyle,1,9)<>'_CheckBox' then
                        FGlyphStyle:='_CheckBox';
        gtRadioButton: if Copy(FGlyphStyle,1,12)<>'_RadioButton' then
                        FGlyphStyle:='_RadioButton';
       end;
       TabStop:=FGlyphType in [gtCheckBox,gtRadioButton];       
      end;
     ResetGroupBoxParams([gbrpGlyphOnStyleChange,gbrpGlyphOnSizeChange]);
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.SetTextOffset(Value:Integer);
begin
 if Value<>FTextOffset then
  begin
   FTextOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetGlyphPosition(Value:TSXGroupBoxGlyphPosition);
begin
 if Value<>FGlyphPosition then
  begin
   FGlyphPosition:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams;
  end;
end;

procedure TSXSkinCustomGroupBox.SetUseLabelStyle(Value:Boolean);
begin
 if Value<>FUseLabelStyle then
  begin
   FUseLabelStyle:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpTextOnFontChange]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetAllowGrayed(Value:Boolean);
begin
 if Value<>FAllowGrayed then
  begin
   FAllowGrayed:=Value;
   if not FAllowGrayed and (State=cbGrayed) then
    State:=cbUnchecked;
  end;
end;

procedure TSXSkinCustomGroupBox.SetChecked(Value:Boolean);
begin
 if Value<>GetChecked then
  begin
   if Value then State:=cbChecked else
    State:=cbUnchecked;
  end;
end;

procedure TSXSkinCustomGroupBox.SetGlyphStyle(const Value:String);
begin
 if Value<>FGlyphStyle then
  begin
   FGlyphStyle:=Value;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpGlyphOnStyleChange,gbrpTextIfWordWrap,gbrpTransparentRect]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetState(Value:TCheckBoxState);
var A:Integer;
begin
 if Value<>FState then
  begin
   FState:=Value;
   if not (csLoading in ComponentState) then
    begin
     ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                          gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                          gbrpTransparentRect]);
     if (FGlyphType=gtRadioButton) and (FState=cbChecked) and (Parent<>nil) then
      begin
       for A:=0 to Parent.ControlCount-1 do
        if (Parent.Controls[A]<>Self) and (Parent.Controls[A] is TSXSkinCustomRadioButton) and
           TSXSkinCustomRadioButton(Parent.Controls[A]).Checked then
         TSXSkinCustomRadioButton(Parent.Controls[A]).Checked:=False else
        if (Parent.Controls[A]<>Self) and (Parent.Controls[A] is TSXSkinCustomGroupBox) and
           (TSXSkinCustomGroupBox(Parent.Controls[A]).GlyphType=gtRadioButton) and
           (TSXSkinCustomGroupBox(Parent.Controls[A]).State=cbChecked) then
         TSXSkinCustomGroupBox(Parent.Controls[A]).State:=cbUnchecked;
      end;
     if Assigned(FOnChange) then
      FOnChange(Self);
    end;
  end;
end;

function TSXSkinCustomGroupBox.OnGetVariable(const VarName:String;var Error:Boolean):Single;
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
 if VarName='CL' then
  begin
   Result:=FCaptionRect.Left; exit;
  end;
 if VarName='CT' then
  begin
   Result:=FCaptionRect.Top; exit;
  end;
 if VarName='CR' then
  begin
   Result:=FCaptionRect.Right; exit;
  end;
 if VarName='CB' then
  begin
   Result:=FCaptionRect.Bottom; exit;
  end;
 if VarName='GlX' then
  begin
   Result:=FGlyphRect.Left; exit;
  end;
 if VarName='GlY' then
  begin
   Result:=FGlyphRect.Top; exit;
  end;
 if VarName='GlW' then
  begin
   Result:=FGlyphRect.Right-FGlyphRect.Left; exit;
  end;
 if VarName='GlH' then
  begin
   Result:=FGlyphRect.Bottom-FGlyphRect.Top; exit;
  end;
 if VarName='TL' then
  begin
   Result:=FTextRect.Left; exit;
  end;
 if VarName='TT' then
  begin
   Result:=FTextRect.Top; exit;
  end;
 if VarName='TR' then
  begin
   Result:=FTextRect.Right; exit;
  end;
 if VarName='TB' then
  begin
   Result:=FTextRect.Bottom; exit;
  end;
 Error:=True;
end;

procedure TSXSkinCustomGroupBox.GetCurrentGBState(var GBState:TSXSkinGroupBoxStateParam);
var  A:Integer;
 Style:TSXSkinGroupBoxStyle;
begin
 Finalize(GBState);
 FillChar(GBState,sizeof(GBState),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinGroupBoxStyle) then
    begin
     Style:=TSXSkinGroupBoxStyle(SkinLibrary.Styles[A]);
     Style.GetCurrentGBState(GBState,FMouseOver,FLastFocused,Enabled);
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
var     A:Integer;
      W,H:Integer;
  GBState:TSXSkinGroupBoxStateParam;
       RR:TRect;
       FD:TSXFontData;
    Style:TSXSkinGeneralStyle;
 CurGlyph:TBitmap32;
 TextRect:TRect;
    Flags:Cardinal;

 procedure DrawTextToBitmap(Bitmap:TBitmap32);
 begin
  Bitmap.Font:=Canvas.Font;
  if FD.HasShadow then
   begin
    OffsetRect(TextRect,1,1);
    if FD.SmoothLevel=0 then
     DrawAlphaText(Bitmap,Caption,TextRect,Flags,FD.ShadowColor) else
      DrawSmoothText(Bitmap,Caption,TextRect,Flags,FD.SmoothLevel,FD.ShadowColor);
    OffsetRect(TextRect,-1,-1);
   end;
  if FD.SmoothLevel=0 then
   DrawAlphaText(Bitmap,Caption,TextRect,Flags,FD.FontColor) else
    DrawSmoothText(Bitmap,Caption,TextRect,Flags,FD.SmoothLevel,FD.FontColor);
 end;

begin
 FLastFocused:=Focused and TabStop;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentGBState(GBState);
   //Style
   A:=SkinLibrary.Styles.GetGStyleIndex(GBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
   //OverStyle
   if GBState.OverStyle<>'' then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(GBState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
      end;
    end;
   //Glyph
   if RectInRegion(Rgn,FGlyphRect) then
    begin
     GetCurrentGlyphStyle(FLastGlyphStyle);
     CurGlyph:=CreateCurrentBlendedGlyph;
     try
      if CurGlyph<>nil then
       Bitmap.Draw(X-Rect.Left+FGlyphRect.Left,Y-Rect.Top+FGlyphRect.Top,CurGlyph);
     finally
      CurGlyph.Free;
     end;
    end;
   //Text 
   if (Caption<>'') and RectInRegion(Rgn,FTextRect) then
    begin
     Bitmap.Font:=Canvas.Font;
     GetCurrentFontData(FD);
     Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP;
     case FAlignment of
      taLeftJustify:  Flags:=Flags or DT_LEFT;
      taRightJustify: Flags:=Flags or DT_RIGHT;
      taCenter:       Flags:=Flags or DT_CENTER;
     end;
     if FWordWrap then Flags:=Flags or DT_WORDBREAK;
     TextRect:=FTextRect;
     OffsetRect(TextRect,X-Rect.Left+FLastTextLeftOffset,Y-Rect.Top+FLastTextTopOffset);
     Dec(TextRect.Right,FLastTextLeftOffset+FLastTextRightOffset);
     Dec(TextRect.Bottom,FLastTextTopOffset+FLastTextBottomOffset);
     if FD.DoPrepaint and (FD.SmoothLevel>0) and (FTextBitmap=nil) then
      begin
       FTextBitmap:=TBitmap32.Create;
       FTextBitmap.DrawMode:=dmBlend;
       FTextBitmap.CombineMode:=cmMerge;
       W:=TextRect.Right-TextRect.Left;
       H:=TextRect.Bottom-TextRect.Top;
       if FD.HasShadow then
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
     if FD.DoPrepaint and (FD.SmoothLevel>0) then
      FTextBitmap.DrawTo(Bitmap,TextRect.Left,TextRect.Top) else
       DrawTextToBitmap(Bitmap);
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomGroupBox.CMMouseLeave(var Msg:TMessage);
begin
 if Enabled then
  InternalMouseLeave;
end;

procedure TSXSkinCustomGroupBox.InternalMouseEnter;
begin
 StartGlyphChanging(gcaHighlightIn);
 FMouseOver:=True;
 ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                      gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                      gbrpTransparentRect]);
 if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
end;

procedure TSXSkinCustomGroupBox.InternalMouseLeave;
begin
 if FMouseOver then
  begin
   StartGlyphChanging(gcaHighlightOut);
   FMouseOver:=False;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
   if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  end;
end;

procedure TSXSkinCustomGroupBox.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   FLastGlyph.Free;
   FLastGlyph:=nil;
   if (FThread<>nil) and not FThread.Suspended then
    FThread.Suspend;
   ResetGroupBoxParams([gbrpText,gbrpGlyph,gbrpTransparentRect,gbrpInvalidateOnStyleChange]);
  end;
 inherited;
end;

function TSXSkinCustomGroupBox.GetCaption:TCaption;
begin
 Result:=inherited Caption;
end;

function TSXSkinCustomGroupBox.GetChecked:Boolean;
begin
 Result:=State<>cbUnchecked;
end;

procedure TSXSkinCustomGroupBox.SetCaption(const Value:TCaption);
begin
 if csLoading in ComponentState then
  begin
   inherited Caption:=Value;
   exit;
  end;
 if Caption<>Value then
  begin
   inherited Caption:=Value;
   ResetGroupBoxParams([gbrpText]);
  end;
end;

procedure TSXSkinCustomGroupBox.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
var DoReset:Boolean;
begin
 if (ALeft=Left) and (ATop=Top) and (AWidth=Width) and (AHeight=Height) then exit;
 DoReset:=(Width<>AWidth) or (Height<>AHeight);
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
 if not (csLoading in ComponentState) then
  begin
   if DoReset then
    ResetGroupBoxParams([gbrpTextIfWordWrap,gbrpTransparentRect]) else
     ResetGroupBoxParams([gbrpTransparentRect]);
  end;
end;

procedure TSXSkinCustomGroupBox.CMFontChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  ResetGroupBoxParams([gbrpTextOnFontChange]);
end;

procedure TSXSkinCustomGroupBox.GetCurrentGlyphStyle(var GStyle:String;
           PGlyphWidth:PInteger=nil;PGlyphHeight:PInteger=nil);
var     A,B:Integer;
    GBStyle:TSXSkinGroupBoxStyle;
    CBStyle:TSXSkinCheckBoxStyle;
    CBState:TSXSkinCheckBoxStateParam;
    RBStyle:TSXSkinRadioButtonStyle;
    RBState:TSXSkinRadioButtonStateParam;
    MSStyle:TSXSkinMultiStateStyle;
 GlyphStyle:TSXSkinGeneralStyle;
      Image:TSXSkinStyleImageElement;

 procedure SetGStyle(const S:String); overload;
 begin
  if GStyle='' then GStyle:=S;
 end;

 procedure SetGStyle(const T:TSXSkinCheckBoxStateParam); overload;
 begin
  if GStyle='' then GStyle:=T.GlyphStyle;
  if (PGlyphWidth<>nil) and (PGlyphWidth^=0) then
   PGlyphWidth^:=T.GlyphWidth;
  if (PGlyphHeight<>nil) and (PGlyphHeight^=0) then
   PGlyphHeight^:=T.GlyphHeight;
 end;

 procedure SetGStyle(const T:TSXSkinRadioButtonStateParam); overload;
 begin
  if GStyle='' then GStyle:=T.GlyphStyle;
  if (PGlyphWidth<>nil) and (PGlyphWidth^=0) then
   PGlyphWidth^:=T.GlyphWidth;
  if (PGlyphHeight<>nil) and (PGlyphHeight^=0) then
   PGlyphHeight^:=T.GlyphHeight;
 end;

begin
 if PGlyphWidth<>nil then PGlyphWidth^:=0;
 if PGlyphHeight<>nil then PGlyphHeight^:=0;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinGroupBoxStyle) then
    begin
     GBStyle:=TSXSkinGroupBoxStyle(SkinLibrary.Styles[A]);
     if PGlyphWidth<>nil then PGlyphWidth^:=GBStyle.GlyphWidth;
     if PGlyphHeight<>nil then PGlyphHeight^:=GBStyle.GlyphHeight;
    end;
   GStyle:='';
   A:=SkinLibrary.Styles.GetIndexByName(FGlyphStyle);
   if A>=0 then
    begin
     if SkinLibrary.Styles[A] is TSXSkinCheckBoxStyle then
      begin
       CBStyle:=TSXSkinCheckBoxStyle(SkinLibrary.Styles[A]);
       CBStyle.GetCurrentCBState(CBState,FState,FDown,FMouseOver,FLastFocused,Enabled);
       SetGStyle(CBState.GlyphStyle);
      end else
     if SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle then
      begin
       RBStyle:=TSXSkinRadioButtonStyle(SkinLibrary.Styles[A]);
       RBStyle.GetCurrentRBState(RBState,Checked,FDown,FMouseOver,FLastFocused,Enabled);
       SetGStyle(RBState.GlyphStyle);
      end else
     if SkinLibrary.Styles[A] is TSXSkinMultiStateStyle then
      begin
       MSStyle:=TSXSkinMultiStateStyle(SkinLibrary.Styles[A]);
       if not Enabled then
        begin
         SetGStyle(MSStyle.RStyle);
         SetGStyle(MSStyle.NStyle);
        end else
       if FMouseOver then
        begin
         SetGStyle(MSStyle.HStyle);
         SetGStyle(MSStyle.NStyle);
        end else SetGStyle(MSStyle.NStyle);
      end else GStyle:=FGlyphStyle;
    end else
     begin
      if PGlyphWidth<>nil then PGlyphWidth^:=0;
      if PGlyphHeight<>nil then PGlyphHeight^:=0;
     end;
   if (GStyle<>'') and ((PGlyphWidth<>nil) or (PGlyphHeight<>nil)){ and
      ((PGlyphWidth^=0) and (PGlyphHeight^=0))} then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(GStyle,0,0);
     if A>=0 then
      begin
       GlyphStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       for B:=0 to GlyphStyle.Elements.Count-1 do
        if GlyphStyle.Elements[B] is TSXSkinStyleImageElement then
         begin
          Image:=TSXSkinStyleImageElement(GlyphStyle.Elements[B]);
          Image.ValidateBitmap(SkinLibrary);
          if Image.Bitmap<>nil then
           begin
            if (PGlyphWidth<>nil){ and (PGlyphWidth^=0)} then
             PGlyphWidth^:=Image.Bitmap.Width;
            if (PGlyphHeight<>nil){ and (PGlyphHeight^=0)} then
             PGlyphHeight^:=Image.Bitmap.Height;
           end;
         end;
      end;
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.GetCurrentFontData(var FD:TSXFontData);
var    A:Integer;
   Style:TSXSkinGroupBoxStyle;
  LStyle:TSXSkinLabelStyle;
 CBStyle:TSXSkinCheckBoxStyle;
 CBState:TSXSkinCheckBoxStateParam;
 RBStyle:TSXSkinRadioButtonStyle;
 RBState:TSXSkinRadioButtonStateParam;
begin
 ClearFontData(FD);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinGroupBoxStyle) then
    begin
     Style:=TSXSkinGroupBoxStyle(SkinLibrary.Styles[A]);
     A:=SkinLibrary.Styles.GetIndexByName(Style.LabelStyle);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinLabelStyle) then
      LStyle:=TSXSkinLabelStyle(SkinLibrary.Styles[A]) else LStyle:=nil;
     CBStyle:=nil; RBStyle:=nil;
     if not FUseLabelStyle then
      begin
       case FGlyphType of
        gtCheckBox:    begin
                        A:=SkinLibrary.Styles.GetIndexByName(FGlyphStyle);
                        if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinCheckBoxStyle) then
                         CBStyle:=TSXSkinCheckBoxStyle(SkinLibrary.Styles[A]);
                       end;
        gtRadioButton: begin
                        A:=SkinLibrary.Styles.GetIndexByName(FGlyphStyle);
                        if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
                         RBStyle:=TSXSkinRadioButtonStyle(SkinLibrary.Styles[A]);
                       end;
       end;
      end;
     if CBStyle<>nil then
      begin
       CBStyle.GetCurrentCBState(CBState,FState,FDown,FMouseOver,FLastFocused,Enabled);
       AddFontData(FD,CBState.FD);
      end else
     if RBStyle<>nil then
      begin
       RBStyle.GetCurrentRBState(RBState,Checked,FDown,FMouseOver,FLastFocused,Enabled);
       AddFontData(FD,RBState.FD);
      end else
     if LStyle<>nil then
      begin
       if not Enabled then
        begin
         AddFontData(FD,LStyle.RState.FD);
         AddFontData(FD,LStyle.NState.FD);
        end else
       if FMouseOver then
        begin
         AddFontData(FD,LStyle.HState.FD);
         AddFontData(FD,LStyle.NState.FD);
        end else AddFontData(FD,LStyle.NState.FD);
      end;
    end;
  end;
 SetDefaultFontData(FD,Font); 
end;

procedure TSXSkinCustomGroupBox.CreateThreadIfNeeded;
begin
 if FThread=nil then
  begin
   FThread:=TSXSkinCustomGroupBoxThread.Create;
   FThread.Control:=Self;
  end; 
end;

procedure TSXSkinCustomGroupBox.GetCurrentGlyphTransformEffect(Action:TSXGlyphChangeAction;
           var Effect:TSXTransformEffectData);
var    A:Integer;
 CBStyle:TSXSkinCheckBoxStyle;
 RBStyle:TSXSkinRadioButtonStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(FGlyphStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinCheckBoxStyle) then
    begin
     CBStyle:=TSXSkinCheckBoxStyle(SkinLibrary.Styles[A]);
     case Action of
      gcaHighlightIn:  Effect:=CBStyle.HInGlyphEffect;
      gcaHighlightOut: Effect:=CBStyle.HOutGlyphEffect;
      gcaDown:         Effect:=CBStyle.DownGlyphEffect;
      gcaUp:           Effect:=CBStyle.UpGlyphEffect;
      gcaCheck:        Effect:=CBStyle.CheckGlyphEffect;
      gcaUncheck:      Effect:=CBStyle.UncheckGlyphEffect;
      gcaEnable:       Effect:=CBStyle.EnableGlyphEffect;
      gcaDisable:      Effect:=CBStyle.DisableGlyphEffect;
      gcaFocus:        Effect:=CBStyle.FocusGlyphEffect;
      gcaUnfocus:      Effect:=CBStyle.UnfocusGlyphEffect;
     end;
    end;
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     RBStyle:=TSXSkinRadioButtonStyle(SkinLibrary.Styles[A]);
     case Action of
      gcaHighlightIn:  Effect:=RBStyle.HInGlyphEffect;
      gcaHighlightOut: Effect:=RBStyle.HOutGlyphEffect;
      gcaDown:         Effect:=RBStyle.DownGlyphEffect;
      gcaUp:           Effect:=RBStyle.UpGlyphEffect;
      gcaCheck:        Effect:=RBStyle.CheckGlyphEffect;
      gcaUncheck:      Effect:=RBStyle.UncheckGlyphEffect;
      gcaEnable:       Effect:=RBStyle.EnableGlyphEffect;
      gcaDisable:      Effect:=RBStyle.DisableGlyphEffect;
      gcaFocus:        Effect:=RBStyle.FocusGlyphEffect;
      gcaUnfocus:      Effect:=RBStyle.UnfocusGlyphEffect;
     end;
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.StartGlyphChanging(T:TSXGlyphChangeAction);
var        B:TBitmap32;
  NeedThread:Boolean;
  GTransform:TSXTransformEffectData;
begin
 GetCurrentGlyphTransformEffect(T,GTransform);
 NeedThread:=HasTransformEffect(GTransform);
 if NeedThread then
  begin
   CreateThreadIfNeeded;
   B:=CreateCurrentBlendedGlyph;
   FLastGlyph.Free;
   FLastGlyph:=B;
   //
   FDoneSteps:=1;
   if FThread.Suspended then
    FThread.Resume;
  end else
   if (FThread<>nil) and not FThread.Suspended then
    begin
     FDoneSteps:=MaxInt;
     FThread.Suspend;
    end;
 FLastGlyphTransform:=GTransform;
end;

procedure TSXSkinCustomGroupBox.InvalidateGroupBoxArea;
var Rgn,Rgn2,Rgn3:HRGN;
begin
 if HandleAllocated then
  begin
   if IsRectEmpty(FTransparentRect) then
    InvalidateRect(Handle,nil,False) else
     begin
      Rgn:=CreateRectRgn(0,0,Width,Height);
      Rgn2:=CreateRectRgnIndirect(FTransparentRect);
      Rgn3:=CreateRectRgn(0,0,0,0);
      CombineRgn(Rgn3,Rgn,Rgn2,RGN_DIFF);
      InvalidateRgn(Handle,Rgn3,False);
      DeleteObject(Rgn);
      DeleteObject(Rgn2);
      DeleteObject(Rgn3);
     end; 
  end;
end;

procedure TSXSkinCustomGroupBox.InvalidateGlyph;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FGlyphRect,False);
end;

procedure TSXSkinCustomGroupBox.InvalidateText;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FTextRect,False);
end;

procedure TSXSkinCustomGroupBox.DoThreadActions;
begin
 if not (csDestroying in ComponentState) then
  begin
   Inc(FDoneSteps);
   if (FThread<>nil) and not FThread.Suspended and
      (not HasTransformEffect(FLastGlyphTransform) or (FDoneSteps>=FLastGlyphTransform.StepsNum)) then
    begin
     FThread.Suspend;
    end;
   InvalidateGlyph;
   Update;
  end;
end;

function TSXSkinCustomGroupBox.CreateCurrentGlyph:TBitmap32;
var GStyle:String;

 procedure SetGlyphByStyleName(const StyleName:String;var BB:TBitmap32);
 var       C:Integer;
  GlyphStyle:TSXSkinGeneralStyle;
         Rgn:HRGN;
 begin
  C:=SkinLibrary.Styles.GetGStyleIndex(StyleName,FGlyphRect.Right-FGlyphRect.Left,
                                       FGlyphRect.Bottom-FGlyphRect.Top);
  if C>=0 then
   begin
    GlyphStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[C]);
    BB:=TBitmap32.Create;
    BB.SetSize(FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top);
    BB.Clear(0);
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    Rgn:=CreateRectRgn(0,0,BB.Width,BB.Height);
    GlyphStyle.DrawToBitmap(Self,CEID_Glyph,BB,0,0,BB.Width,BB.Height,Rect(0,0,BB.Width,BB.Height),
         Rgn,SkinLibrary);
    DeleteObject(Rgn);
   end;
 end;

begin
 Result:=nil;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentGlyphStyle(GStyle);
   if GStyle<>'' then
    SetGlyphByStyleName(GStyle,Result);
  end;
end;

function TSXSkinCustomGroupBox.CreateCurrentBlendedGlyph:TBitmap32;
var B:TBitmap32;
begin
 Result:=CreateCurrentGlyph;
 if (Result<>nil) and HasTransformEffect(FLastGlyphTransform) and
    (FDoneSteps<FLastGlyphTransform.StepsNum) and (FLastGlyph<>nil) then
  begin
   B:=Result;
   try
    Result:=TBitmap32.Create;
    Result.SetSize(B.Width,B.Height);
    Result.DrawMode:=dmBlend;
    Result.CombineMode:=cmMerge;
    ApplyTransformEffectToBitmaps(FLastGlyph,B,FLastGlyphTransform,FDoneSteps,Result);
   finally
    B.Free;
   end;
  end;
end;

procedure TSXSkinCustomGroupBox.WMSetFocus(var Msg:TWMSetFocus);
begin
 StartGlyphChanging(gcaFocus);
 FLastFocused:=True;
 ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                      gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                      gbrpTransparentRect]);
end;

procedure TSXSkinCustomGroupBox.WMKillFocus(var Msg:TWMKillFocus);
begin
 StartGlyphChanging(gcaUnfocus);
 FLastFocused:=False;
 ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                      gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                      gbrpTransparentRect]);
 if FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
  end;
end;

procedure TSXSkinCustomGroupBox.MouseMove(Shift:TShiftState;X,Y:Integer);
var B:Boolean;
    P:TPoint;
begin
 inherited;
 if Enabled then
  begin
   P:=Point(X,Y);
   B:=PtInRect(FGlyphRect,P) or PtInRect(FTextRect,P);
   if B<>FMouseOver then
    begin
     if B then InternalMouseEnter else
      InternalMouseLeave;
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if Enabled and FMouseOver and (Button=mbLeft) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   SetFocus;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
  end;
 inherited;
end;

procedure TSXSkinCustomGroupBox.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var OldDown,WasChecked:Boolean;
begin
 if Enabled then
  begin
   OldDown:=FDown;
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
   if OldDown and FMouseOver then
    begin
     WasChecked:=Checked;
     Click;
     if Assigned(FOnUserModified) and not ((FGlyphType=gtRadioButton) and WasChecked) then
      FOnUserModified(Self);
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomGroupBox.DoClick;
var A:Integer;
begin
 if Visible and Enabled and FDown then
  begin
   case FGlyphType of
    gtCheckBox:    begin
                    FDown:=False;
                    if FState=cbChecked then
                     StartGlyphChanging(gcaUncheck) else
                      StartGlyphChanging(gcaCheck);
                    case FState of
                     cbUnchecked: if FAllowGrayed then
                                   FState:=cbGrayed else FState:=cbChecked;
                     cbGrayed:    FState:=cbChecked;
                     cbChecked:   FState:=cbUnchecked;
                    end;
                    ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                                         gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                                         gbrpTransparentRect]);
                    if Assigned(FOnChange) then
                     FOnChange(Self);
                   end;
    gtRadioButton: begin
                    if FState=cbUnchecked then
                     begin
                      FDown:=False;
                      StartGlyphChanging(gcaCheck);
                      if Parent<>nil then
                       begin
                        for A:=0 to Parent.ControlCount-1 do
                         begin
                          if (Parent.Controls[A]<>Self) and (Parent.Controls[A] is TSXSkinCustomRadioButton) and
                             TSXSkinCustomRadioButton(Parent.Controls[A]).Checked then
                           begin
                            TSXSkinCustomRadioButton(Parent.Controls[A]).DoUncheck;
                           end else
                          if (Parent.Controls[A]<>Self) and (Parent.Controls[A] is TSXSkinCustomGroupBox) and
                             (TSXSkinCustomGroupBox(Parent.Controls[A]).GlyphType=gtRadioButton) and
                             TSXSkinCustomGroupBox(Parent.Controls[A]).Checked then
                           begin
                            TSXSkinCustomGroupBox(Parent.Controls[A]).DoUncheck;
                           end;                           
                         end;
                       end;
                      FState:=cbChecked;
                      ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                                           gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                                           gbrpTransparentRect]);
                      if Assigned(FOnChange) then
                       FOnChange(Self);
                     end else
                      begin
                       StartGlyphChanging(gcaUp);
                       FDown:=False;
                       ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                                            gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                                            gbrpTransparentRect]);
                      end;
                   end;
   end;
  end else FDown:=False;
end;

procedure TSXSkinCustomGroupBox.Loaded;
begin
 inherited;
 ResetGroupBoxParams([gbrpText,gbrpGlyph,gbrpTransparentRect,gbrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomGroupBox.Click;
begin
 FDown:=True;
 DoClick;
 inherited;
end;

procedure TSXSkinCustomGroupBox.DoKeyDown(var Msg: TMessage);
begin
 inherited;
 if Enabled and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
  end;
end;

procedure TSXSkinCustomGroupBox.DoKeyUp(var Msg: TMessage);
var OldDown,WasChecked:Boolean;
begin
 inherited;
 if Enabled and FDown then
  begin
   OldDown:=FDown;
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                        gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange,
                        gbrpTransparentRect]);
   if OldDown and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
    begin
     WasChecked:=Checked;
     Click;
     if Assigned(FOnUserModified) and not ((FGlyphType=gtRadioButton) and WasChecked) then
      FOnUserModified(Self);
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.AdjustClientRect(var Rect:TRect);
var    A:Integer;
   Style:TSXSkinGroupBoxStyle;
begin
 inherited AdjustClientRect(Rect);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinGroupBoxStyle) then
    begin
     Style:=TSXSkinGroupBoxStyle(SkinLibrary.Styles[A]);
     if Style.ClientRect<>'' then
      GetRectFromString(Style.ClientRect,Rect,OnGetVariable);
    end;
  end;
end;

procedure TSXSkinCustomGroupBox.DoUncheck;
begin
 StartGlyphChanging(gcaUncheck);
 FState:=cbUnchecked;
 ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                      gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomGroupBox.ResetGroupBoxParams(Params:TSXSkinGroupBoxResetParams=[]);
var     GW,GH:Integer;
       GStyle:String;
 OldGlyphRect:TRect;
  OldTextRect:TRect;
    TransRect:TRect;
 GlyphChanged:Boolean;
  TextChanged:Boolean;
   SetGBState:Boolean;
      GBState:TSXSkinGroupBoxStateParam;
           FD:TSXFontData;
   B,GTHeight:Integer;
        Flags:Cardinal;

 procedure DoSetGBState;
 begin
  if not SetGBState then
   GetCurrentGBState(GBState);
 end;

begin
 if Parent=nil then exit;
 OldGlyphRect:=FGlyphRect;
 OldTextRect:=FTextRect;
 GlyphChanged:=False;
 TextChanged:=False;
 SetGBState:=False;
 if Params*[gbrpGlyph,gbrpGlyphOnStyleChange,gbrpGlyphOnSizeChange]<>[] then
  begin
   GetCurrentGlyphStyle(GStyle,@GW,@GH);
   if gbrpGlyph in Params then GlyphChanged:=True else
    begin
     if gbrpGlyphOnSizeChange in Params then
      GlyphChanged:=(GW<>FGlyphRect.Right-FGlyphRect.Left) or
                    (GH<>FGlyphRect.Bottom-FGlyphRect.Top);
     if not GlyphChanged and (gbrpGlyphOnStyleChange in Params) then
      GlyphChanged:=(FLastGlyphStyle<>GStyle);
    end;
   if GlyphChanged then
    FGlyphRect:=Rect(0,0,GW,GH);
   FLastGlyphStyle:=GStyle;
  end;
 if Params*[gbrpText,gbrpTextOnFontChange,gbrpTextIfWordWrap]<>[] then
  begin
   DoSetGBState;
   GetCurrentFontData(FD);
   if gbrpText in Params then TextChanged:=True else
    if FWordWrap and (gbrpTextIfWordWrap in Params) then TextChanged:=True else
    if gbrpTextOnFontChange in Params then
     TextChanged:=not SameFontData(FD,FLastFontData);
   if TextChanged then
    begin
     if FTextBitmap<>nil then
      begin
       FTextBitmap.Free;
       FTextBitmap:=nil;
      end;
     FLastFontData:=FD;
     Canvas.Font.Name:=FD.FontName;
     Canvas.Font.Size:=FD.FontSize;
     Canvas.Font.Style:=FD.FontStyle;
     if FGlyphRect.Right-FGlyphRect.Left=0 then
      FTextRect:=Rect(0,0,Width-2*GBState.CaptionPosition-GBState.CaptionLeftOffset-GBState.CaptionRightOffset,Height) else
       FTextRect:=Rect(0,0,Width-2*GBState.CaptionPosition-FGlyphRect.Right+
        FGlyphRect.Left-GBState.CaptionLeftOffset-GBState.CaptionRightOffset-
        FTextOffset,Height);
     Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP or DT_CALCRECT;
     case FAlignment of
      taLeftJustify:  Flags:=Flags or DT_LEFT;
      taRightJustify: Flags:=Flags or DT_RIGHT;
      taCenter:       Flags:=Flags or DT_CENTER;
     end;
     if FWordWrap then Flags:=Flags or DT_WORDBREAK;
     Dec(FTextRect.Right,GBState.TextLeftOffset+GBState.TextRightOffset);
     Dec(FTextRect.Bottom,GBState.TextTopOffset+GBState.TextBottomOffset);
     if FD.SmoothLevel=0 then
      DrawText(Canvas.Handle,PChar(Caption),-1,FTextRect,Flags) else
       DrawSmoothText(Canvas,Caption,FTextRect,Flags,FD.SmoothLevel);
     Inc(FTextRect.Right,GBState.TextLeftOffset+GBState.TextRightOffset);
     Inc(FTextRect.Bottom,GBState.TextTopOffset+GBState.TextBottomOffset);
     //
     FLastTextLeftOffset:=GBState.TextLeftOffset;
     FLastTextTopOffset:=GBState.TextTopOffset;
     FLastTextRightOffset:=GBState.TextRightOffset;
     FLastTextBottomOffset:=GBState.TextBottomOffset;
    end;
  end;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   DoSetGBState;
   case FCaptionAlignment of
    taLeftJustify:  begin
                     if FGlyphRect.Right-FGlyphRect.Left=0 then
                      FCaptionRect:=Rect(GBState.CaptionPosition,0,
                        GBState.CaptionPosition+GBState.CaptionLeftOffset+
                        FTextRect.Right-FTextRect.Left+GBState.CaptionRightOffset,0) else
                      FCaptionRect:=Rect(GBState.CaptionPosition,0,
                        GBState.CaptionPosition+GBState.CaptionLeftOffset+
                        FGlyphRect.Right-FGlyphRect.Left+FTextOffset+
                        FTextRect.Right-FTextRect.Left+GBState.CaptionRightOffset,0);
                    end;
    taRightJustify: begin
                     if FGlyphRect.Right-FGlyphRect.Left=0 then
                      FCaptionRect:=Rect(Width-GBState.CaptionPosition-
                        GBState.CaptionLeftOffset-FTextRect.Right+
                        FTextRect.Left-GBState.CaptionRightOffset,0,
                        Width-GBState.CaptionPosition,0) else
                      FCaptionRect:=Rect(Width-GBState.CaptionPosition-
                        GBState.CaptionLeftOffset-FGlyphRect.Right+
                        FGlyphRect.Left-FTextOffset-FTextRect.Right+
                        FTextRect.Left-GBState.CaptionRightOffset,0,
                        Width-GBState.CaptionPosition,0);
                    end;
    taCenter:       begin
                     if FGlyphRect.Right-FGlyphRect.Left=0 then
                      B:=GBState.CaptionLeftOffset+FTextRect.Right-
                        FTextRect.Left+GBState.CaptionRightOffset else
                      B:=GBState.CaptionLeftOffset+FGlyphRect.Right-
                        FGlyphRect.Left+FTextOffset+FTextRect.Right-
                        FTextRect.Left+GBState.CaptionRightOffset;
                     FCaptionRect:=Rect((Width-B) div 2,0,(Width+B) div 2,0);
                    end;
   end;
   GTHeight:=Max(FGlyphRect.Bottom-FGlyphRect.Top,FTextRect.Bottom-FTextRect.Top);
   FCaptionRect.Bottom:=GBState.CaptionTopOffset+GBState.CaptionBottomOffset+GTHeight;
   case FGlyphPosition of
    gbgpLeft:  begin
                OffsetRect(FGlyphRect,-FGlyphRect.Left+FCaptionRect.Left+
                  GBState.CaptionLeftOffset,-FGlyphRect.Top+FCaptionRect.Top+
                  GBState.CaptionTopOffset+(GTHeight-FGlyphRect.Bottom+FGlyphRect.Top) div 2);
                if FGlyphRect.Right-FGlyphRect.Left=0 then
                 OffsetRect(FTextRect,-FTextRect.Left+FGlyphRect.Right,
                   -FTextRect.Top+FCaptionRect.Top+GBState.CaptionTopOffset+
                   (GTHeight-FTextRect.Bottom+FTextRect.Top) div 2) else
                 OffsetRect(FTextRect,-FTextRect.Left+FGlyphRect.Right+FTextOffset,
                   -FTextRect.Top+FCaptionRect.Top+GBState.CaptionTopOffset+
                   (GTHeight-FTextRect.Bottom+FTextRect.Top) div 2);
               end;
    gbgpRight: begin
                OffsetRect(FTextRect,-FTextRect.Left+FCaptionRect.Left+
                  GBState.CaptionLeftOffset,-FTextRect.Top+FCaptionRect.Top+
                  GBState.CaptionTopOffset+(GTHeight-FTextRect.Bottom+FTextRect.Top) div 2);
                OffsetRect(FGlyphRect,-FGlyphRect.Left+FTextRect.Right+FTextOffset,
                   -FGlyphRect.Top+FCaptionRect.Top+GBState.CaptionTopOffset+
                   (GTHeight-FGlyphRect.Bottom+FGlyphRect.Top) div 2);
               end;
   end;
  end;
 //Invalidating Regions
 if GlyphChanged or not EqualRect(OldGlyphRect,FGlyphRect) then
  begin
   if HandleAllocated then
    begin
     InvalidateRect(Handle,@OldGlyphRect,False);
     InvalidateRect(Handle,@FGlyphRect,False);
    end;
  end;
 if TextChanged or not EqualRect(OldTextRect,FTextRect) then
  begin
   if HandleAllocated then
    begin
     InvalidateRect(Handle,@OldTextRect,False);
     InvalidateRect(Handle,@FTextRect,False);
    end;
  end;
 //
 if gbrpInvalidateOnStyleChange in Params then
  begin
   DoSetGBState;
   if (GBState.Style<>FLastGBStyle) or (GBState.OverStyle<>FLastGBOverStyle) then
    begin
     if HandleAllocated then
      InvalidateGroupBoxArea;
     FLastGBStyle:=GBState.Style;
     FLastGBOverStyle:=GBState.OverStyle;
    end;
  end;
 if gbrpTransparentRect in Params then
  begin
   DoSetGBState;
   if GBState.TransparentRect<>'' then
    begin
     GetRectFromString(GBState.TransparentRect,TransRect,OnGetVariable);
     if not EqualRect(TransRect,FTransparentRect) then
      begin
       InvalidateGroupBoxArea;
       FTransparentRect:=TransRect;
       InvalidateGroupBoxArea;
      end;
    end else FTransparentRect:=Types.Rect(0,0,0,0);
  end;
end;

procedure TSXSkinCustomGroupBox.SetEnabled(Value:Boolean);
begin
 if Enabled<>Value then
  begin
   if not (csLoading in ComponentState) then
    begin
     if Enabled then
      StartGlyphChanging(gcaDisable) else
       StartGlyphChanging(gcaEnable);
    end;
   inherited;
   if not Enabled then
    begin
     FMouseOver:=False;
     FDown:=False;
    end;
   if not (csLoading in ComponentState) then
    ResetGroupBoxParams([gbrpTextOnFontChange,gbrpGlyphOnSizeChange,
                         gbrpGlyphOnStyleChange,gbrpInvalidateOnStyleChange]);
  end;
end;

function TSXSkinCustomGroupBox.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 GBState:TSXSkinGroupBoxStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentGBState(GBState);
   A:=SkinLibrary.Styles.GetGStyleIndex(GBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
    end;
   if Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(GBState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
      end;
    end;
   if Result and (FGlyphStyle<>'') then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(FGlyphStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Glyph,X,Y,Width,Height,SkinLibrary,Limit);
      end;
    end;
  end;
end;

function TSXSkinCustomGroupBox.CapturesMouseAt(X,Y:Integer):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 GBState:TSXSkinGroupBoxStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentGBState(GBState);
   A:=SkinLibrary.Styles.GetGStyleIndex(GBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
    end;
   if not Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(GBState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
      end;
    end;
   if not Result and (FGlyphStyle<>'') then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(FGlyphStyle,FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Glyph,X-FGlyphRect.Left,Y-FGlyphRect.Top,
          FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top,SkinLibrary);
      end;
    end;
   if not Result then
    Result:=PtInRect(FTextRect,Point(X,Y));
  end;
end;

constructor TSXSkinCustomGroupBox.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 CEID_Back:=GetNewCElementID;
 CEID_Glyph:=GetNewCElementID;
 VComparer:=TSXGroupBoxVariableComparer.Create;
 VComparer.Control:=Self;
 VComparer.OnGetVariable:=OnGetVariable;
 ControlStyle:=ControlStyle+[csSetCaption]-[csDoubleClicks];
 FUseLabelStyle:=True;
 FTextOffset:=2;
 SkinStyle:='_GroupBox';
end;

destructor TSXSkinCustomGroupBox.Destroy;
begin
 FThread.Free;
 FTextBitmap.Free;
 FLastGlyph.Free;
 inherited;
 VComparer.Free; 
end;

end.
