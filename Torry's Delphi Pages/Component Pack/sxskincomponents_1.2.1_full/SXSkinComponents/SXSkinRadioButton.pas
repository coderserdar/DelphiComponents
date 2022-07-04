unit SXSkinRadioButton;

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

 VARRB_W   =  1;
 VARRB_H   =  2;
 VARRB_GlX =  3;
 VARRB_GlY =  4;
 VARRB_GlW =  5;
 VARRB_GlH =  6;
 VARRB_TL  =  7;
 VARRB_TT  =  8;
 VARRB_TR  =  9;
 VARRB_TB  = 10;

type

 TSXSkinCustomRadioButton=class;

 TSXSkinCustomRadioButtonThread=class(TThread)
  public
   Control:TSXSkinCustomRadioButton;
   constructor Create;
   procedure Execute; override;
   procedure DoEvent;
 end;

 TSXSkinRadioButtonResetParam=(rbrpText,
                               rbrpTextOnFontChange,
                               rbrpTextIfWordWrap,
                               rbrpGlyph,
                               rbrpGlyphOnStyleChange,
                               rbrpGlyphOnSizeChange,
                               rbrpInvalidateOnStyleChange);

 TSXSkinRadioButtonResetParams=set of TSXSkinRadioButtonResetParam;

 TSXSkinRadioButtonTransform=(cbtRadioButton,cbtGlyph);

 TSXRadioButtonVariableComparer=class(TSXVariableComparer)
  private
   Control:TSXSkinCustomRadioButton;
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

 TSXSkinCustomRadioButton=class(TSXSkinCustomControl)
  private
   FGlyphPosition:TSXGlyphPosition;
   FAutoSizeWidth:Boolean;
   FAutoSizeHeight:Boolean;
   FAutoAlignFirstLine:Boolean;
   FChecked:Boolean;
   FWordWrap:Boolean;
   FTextOffset:Integer;
   FAlignment:TAlignment;
   FVerticalAlignment:TVerticalAlignment;
   FOnChange:TNotifyEvent;
   FOnUserModified:TNotifyEvent;
   FOnMouseEnter:TNotifyEvent;
   FOnMouseLeave:TNotifyEvent;
   FMouseOver:Boolean;
   FDown:Boolean;
   FThread:TSXSkinCustomRadioButtonThread;
   //
   FLastRadioButtonTransform:TSXTransformEffectData;
   FLastGlyphTransform:TSXTransformEffectData;
   FDoneSteps:Integer;
   FLastGlyph:TBitmap32;
   FLastRadioButton:TBitmap32;
   //
   FGlyphRect:TRect;
   FGlyphWidth:Integer;
   FGlyphHeight:Integer;
   FTextRect:TRect;
   FLastFocused:Boolean;
   FTextBitmap:TBitmap32;
   FLastFontData:TSXFontData;
   FLastStyle:String;
   FLastOverStyle:String;
   FLastGlyphStyle:String;
   FLastTextLeftOffset:Integer;
   FLastTextTopOffset:Integer;
   FLastTextRightOffset:Integer;
   FLastTextBottomOffset:Integer;
   //
   CEID_Back:Integer;
   CEID_Glyph:Integer;
   VComparer:TSXRadioButtonVariableComparer;
   procedure SetCaption(const Value:TCaption);
   procedure SetAutoAlignFirstLine(Value:Boolean);
   procedure SetAlignment(Value:TAlignment);
   procedure SetVerticalAlignment(Value:TVerticalAlignment);
   procedure SetGlyphPosition(Value:TSXGlyphPosition);
   procedure SetAutoSizeWidth(Value:Boolean);
   procedure SetAutoSizeHeight(Value:Boolean);
   procedure SetChecked(Value:Boolean);
   procedure SetWordWrap(Value:Boolean);
   procedure SetTextOffset(Value:Integer);
   function HasUnusualSkinStyle:Boolean;
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure GetCurrentRBState(var RBState:TSXSkinRadioButtonStateParam);
   function CreateCurrentGlyph:TBitmap32;
   function CreateCurrentBlendedGlyph:TBitmap32;
   procedure DoThreadActions;
   procedure StartGlyphChanging(T:TSXGlyphChangeAction);
   procedure CreateThreadIfNeeded;
   procedure ResetRadioButtonParams(Params:TSXSkinRadioButtonResetParams=[]);
   procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
   procedure InvalidateGlyph;
   procedure GetCurrentTransformEffect(T:TSXSkinRadioButtonTransform;
              Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
   procedure InternalMouseEnter;
   procedure InternalMouseLeave;
  protected
   function CapturesMouseAt(X,Y:Integer):Boolean; override;
   procedure SetEnabled(Value:Boolean); override;
   procedure Loaded; override;
   procedure InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
   function GetCaption:TCaption;
   procedure DoClick;
   procedure MouseLeave; override;
   procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
   procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure DoKeyDown(var Msg:TMessage); message CN_KEYDOWN;
   procedure DoKeyUp(var Msg:TMessage); message CN_KEYUP;
   procedure WMSetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
   procedure WMKillFocus(var Msg:TWMKillFocus); message WM_KILLFOCUS;
   procedure PaintCurrentCBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentBlendedCBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle:HDC;
              DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer);
   property Caption:TCaption read GetCaption write SetCaption;
   property GlyphPosition:TSXGlyphPosition read FGlyphPosition write SetGlyphPosition default gpLeftTop;
   property TextOffset:Integer read FTextOffset write SetTextOffset default 2;
   property WordWrap:Boolean read FWordWrap write SetWordWrap default False;
   property AutoSizeWidth:Boolean read FAutoSizeWidth write SetAutoSizeWidth default True;
   property AutoSizeHeight:Boolean read FAutoSizeHeight write SetAutoSizeHeight default True;
   property AutoAlignFirstLine:Boolean read FAutoAlignFirstLine write SetAutoAlignFirstLine default True;
   property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
   property VerticalAlignment:TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
   property SkinStyle stored HasUnusualSkinStyle;
  public
   function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
   procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;  
   procedure DoUncheck;
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   procedure Click; reintroduce;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
  published
   property Checked:Boolean read FChecked write SetChecked default False;
   property TabStop default True;
   property OnChange:TNotifyEvent read FOnChange write FOnChange;
   property OnUserModified:TNotifyEvent read FOnUserModified write FOnUserModified;
   property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
 end;

 TSXSkinRadioButton=class(TSXSkinCustomRadioButton)
  published
   property Align;
   property Alignment;
   property Anchors;
   property AutoAlignFirstLine;
   property AutoSizeWidth;
   property AutoSizeHeight;
   property Caption;
   property Checked;
   property Color;
   property Constraints;
   property Cursor;
   property DragCursor;
   property Enabled;
   property Font;
   property GlyphPosition;
   //property HintData;
   property ParentColor;
   property ParentFont;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property SkinLibrary;
   property SkinStyle;
   property TabOrder;
   property TabStop default True;
   property TextOffset;
   property VerticalAlignment;
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

uses Math, SXBitmap32Utils, SXSkinGroupBox;

{ TSXSkinCustomRadioButtonThread }

constructor TSXSkinCustomRadioButtonThread.Create;
begin
 inherited Create(True);
 FreeOnTerminate:=False;
end;

procedure TSXSkinCustomRadioButtonThread.Execute;
begin
 while not Terminated do
  begin
   SleepEx(30,True);
   if not Suspended then
    Synchronize(DoEvent);
  end;
end;

procedure TSXSkinCustomRadioButtonThread.DoEvent;
begin
 if Assigned(Control) then
  Control.DoThreadActions;
end;

{ TSXRadioButtonVariableComparer }

function TSXRadioButtonVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARRB_W else
 if VarName='H' then
  CurVarVal:=VARRB_H else
 if VarName='GlX' then
  CurVarVal:=VARRB_GlX else
 if VarName='GlY' then
  CurVarVal:=VARRB_GlY else
 if VarName='GlW' then
  CurVarVal:=VARRB_GlW else
 if VarName='GlH' then
  CurVarVal:=VARRB_GlH else
 if VarName='TL' then
  CurVarVal:=VARRB_TL else
 if VarName='TT' then
  CurVarVal:=VARRB_TT else
 if VarName='TR' then
  CurVarVal:=VARRB_TR else
 if VarName='TB' then
  CurVarVal:=VARRB_TB;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXRadioButtonVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 if Control<>nil then
  begin
   case VarID of
    VARRB_W:   Result:=Control.Width;
    VARRB_H:   Result:=Control.Height;
    VARRB_GlX: Result:=Control.FGlyphRect.Left;
    VARRB_GlY: Result:=Control.FGlyphRect.Top;
    VARRB_GlW: Result:=Control.FGlyphWidth;
    VARRB_GlH: Result:=Control.FGlyphHeight;
    VARRB_TL:  Result:=Control.FTextRect.Left;
    VARRB_TT:  Result:=Control.FTextRect.Top;
    VARRB_TR:  Result:=Control.FTextRect.Right;
    VARRB_TB:  Result:=Control.FTextRect.Bottom;
   end;
  end;
end;

function TSXRadioButtonVariableComparer.GetVarValsForVarList(VarList:TList):TList;
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

function TSXRadioButtonVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
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

procedure TSXRadioButtonVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXRadioButtonVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXRadioButtonVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

{ TSXSkinCustomRadioButton }

function TSXSkinCustomRadioButton.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_RadioButton';
end;

procedure TSXSkinCustomRadioButton.SetEnabled(Value:Boolean);
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
    ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                            rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomRadioButton.SetGlyphPosition(Value:TSXGlyphPosition);
begin
 if Value<>FGlyphPosition then
  begin
   FGlyphPosition:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.SetAlignment(Value:TAlignment);
begin
 if Value<>FAlignment then
  begin
   FAlignment:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams([rbrpText]);
  end;
end;

procedure TSXSkinCustomRadioButton.SetVerticalAlignment(Value:TVerticalAlignment);
begin
 if Value<>FVerticalAlignment then
  begin
   FVerticalAlignment:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.SetAutoSizeWidth(Value:Boolean);
begin
 if Value<>FAutoSizeWidth then
  begin
   FAutoSizeWidth:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.SetAutoSizeHeight(Value:Boolean);
begin
 if Value<>FAutoSizeHeight then
  begin
   FAutoSizeHeight:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.SetAutoAlignFirstLine(Value:Boolean);
begin
 if Value<>FAutoAlignFirstLine then
  begin
   FAutoAlignFirstLine:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.SetChecked(Value:Boolean);
var A:Integer;
begin
 if Value<>FChecked then
  begin
   FChecked:=Value;
   if not (csLoading in ComponentState) then
    begin
     ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                             rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
     if FChecked and (Parent<>nil) then
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
     if FChecked and Assigned(FOnChange) then
      FOnChange(Self);
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.SetWordWrap(Value:Boolean);
begin
 if Value<>FWordWrap then
  begin
   FWordWrap:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams([rbrpText]);
  end;
end;

procedure TSXSkinCustomRadioButton.SetTextOffset(Value:Integer);
begin
 if Value<>FTextOffset then
  begin
   FTextOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetRadioButtonParams([rbrpText]);
  end;
end;

function TSXSkinCustomRadioButton.OnGetVariable(const VarName:String;var Error:Boolean):Single;
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
   Result:=FGlyphWidth; exit;
  end;
 if VarName='GlH' then
  begin
   Result:=FGlyphHeight; exit;
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

procedure TSXSkinCustomRadioButton.GetCurrentRBState(var RBState:TSXSkinRadioButtonStateParam);
var  A:Integer;
 Style:TSXSkinRadioButtonStyle;
begin
 Finalize(RBState);
 FillChar(RBState,sizeof(RBState),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     Style:=TSXSkinRadioButtonStyle(SkinLibrary.Styles[A]);
     Style.GetCurrentRBState(RBState,FChecked,FDown,FMouseOver,FLastFocused,Enabled);
    end;
  end;
 SetDefaultFontData(RBState.FD,Font);
end;

function TSXSkinCustomRadioButton.CreateCurrentGlyph:TBitmap32;
var RBState:TSXSkinRadioButtonStateParam;
          A:Integer;

 procedure SetGlyphByStyleName(const StyleName:String;var BB:TBitmap32);
 var       C:Integer;
  GlyphStyle:TSXSkinGeneralStyle;
         Rgn:HRGN;
 begin
  C:=SkinLibrary.Styles.GetGStyleIndex(StyleName,FGlyphWidth,FGlyphHeight);
  if C>=0 then
   begin
    GlyphStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[C]);
    BB:=TBitmap32.Create;
    BB.SetSize(FGlyphWidth,FGlyphHeight);
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
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     GetCurrentRBState(RBState);
     if RBState.GlyphStyle<>'' then
      SetGlyphByStyleName(RBState.GlyphStyle,Result);
    end;
  end;
end;

function TSXSkinCustomRadioButton.CreateCurrentBlendedGlyph:TBitmap32;
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

procedure TSXSkinCustomRadioButton.PaintCurrentCBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
 RBState:TSXSkinRadioButtonStateParam;
   Style:TSXSkinGeneralStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentRBState(RBState);
   A:=SkinLibrary.Styles.GetGStyleIndex(RBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
   //
   if HasTransformEffect(FLastRadioButtonTransform) and (FDoneSteps<FLastRadioButtonTransform.StepsNum) and
      FLastRadioButtonTransform.DrawCaption then
    begin
     PaintCurrentOverStyleCaptionToBitmap(Bitmap.Handle,Rect,Rect,Rgn,Bitmap,X,Y);
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.PaintCurrentBlendedCBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var BB:TBitmap32;
 CurCB:TBitmap32;
  Rgn2:HRGN;
begin
 if HasTransformEffect(FLastRadioButtonTransform) and
    (FDoneSteps<FLastRadioButtonTransform.StepsNum) and (FLastRadioButton<>nil) then
  begin
   CurCB:=TBitmap32.Create;
   BB:=TBitmap32.Create;
   try
    CurCB.DrawMode:=dmBlend;
    CurCB.CombineMode:=cmMerge;
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    CurCB.SetSize(Width,Height);
    CurCB.Clear(0);
    Rgn2:=CreateRectRgn(0,0,Width,Height);
    PaintCurrentCBStyle(CurCB,0,0,Types.Rect(0,0,Width,Height),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(Width,Height);
    ApplyTransformEffectToBitmaps(FLastRadioButton,CurCB,FLastRadioButtonTransform,FDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurCB.Free;
   end;
  end else PaintCurrentCBStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomRadioButton.PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer);
var  RBState:TSXSkinRadioButtonStateParam;
       A,W,H:Integer;
       Flags:Cardinal;
    CurGlyph:TBitmap32;
 TextRect,RR:TRect;
       Style:TSXSkinGeneralstyle;

 procedure DrawTextToBitmap(Bitmap:TBitmap32);
 begin
  Bitmap.Font:=Canvas.Font;
  if RBState.FD.HasShadow then
   begin
    OffsetRect(TextRect,1,1);
    if RBState.FD.SmoothLevel=0 then
     DrawAlphaText(Bitmap,Caption,TextRect,Flags,RBState.FD.ShadowColor) else
      DrawSmoothText(Bitmap,Caption,TextRect,Flags,RBState.FD.SmoothLevel,RBState.FD.ShadowColor);
    OffsetRect(TextRect,-1,-1);
   end;
  if RBState.FD.SmoothLevel=0 then
   DrawAlphaText(Bitmap,Caption,TextRect,Flags,RBState.FD.FontColor) else
    DrawSmoothText(Bitmap,Caption,TextRect,Flags,RBState.FD.SmoothLevel,RBState.FD.FontColor);
 end;

begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     GetCurrentRBState(RBState);
     //OverStyle
     if RBState.OverStyle<>'' then
      begin
       A:=SkinLibrary.Styles.GetGStyleIndex(RBState.OverStyle,Width,Height);
       if A>=0 then
        begin
         Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
         Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
        end;
      end;
     //Glyph
     if RectInRegion(Rgn,FGlyphRect) then
      begin
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
       FLastFocused:=Focused;
       Bitmap.Font:=Canvas.Font;
       Flags:=DT_NOPREFIX or DT_TOP;
       case Alignment of
        taLeftJustify:  Flags:=Flags or DT_LEFT;
        taRightJustify: Flags:=Flags or DT_RIGHT;
        taCenter:       Flags:=Flags or DT_CENTER;
       end;
       if FWordWrap then Flags:=Flags or DT_WORDBREAK;
       TextRect:=FTextRect;
       OffsetRect(TextRect,X-Rect.Left+FLastTextLeftOffset,Y-Rect.Top+FLastTextTopOffset);
       Dec(TextRect.Right,FLastTextLeftOffset+FLastTextRightOffset);
       Dec(TextRect.Bottom,FLastTextTopOffset+FLastTextBottomOffset);
       if RBState.FD.DoPrepaint and (RBState.FD.SmoothLevel>0) and (FTextBitmap=nil) then
        begin
         FTextBitmap:=TBitmap32.Create;
         FTextBitmap.DrawMode:=dmBlend;
         FTextBitmap.CombineMode:=cmMerge;
         W:=TextRect.Right-TextRect.Left;
         H:=TextRect.Bottom-TextRect.Top;
         if RBState.FD.HasShadow then
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
       if RBState.FD.DoPrepaint and (RBState.FD.SmoothLevel>0) then
        FTextBitmap.DrawTo(Bitmap,TextRect.Left,TextRect.Top) else
         DrawTextToBitmap(Bitmap);
      end;
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   PaintCurrentBlendedCBStyle(Bitmap,X,Y,Rect,Rgn);
   if not (HasTransformEffect(FLastRadioButtonTransform) and (FDoneSteps<FLastRadioButtonTransform.StepsNum) and
           FLastRadioButtonTransform.DrawCaption) then
    PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle,DestCanvasRect,Rect,Rgn,Bitmap,X,Y);
  end;
 inherited;
end;

procedure TSXSkinCustomRadioButton.InternalMouseEnter;
begin
 if Enabled then
  StartGlyphChanging(gcaHighlightIn);
 FMouseOver:=True;
 if Enabled then
  begin
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
 if Assigned(FOnMouseEnter) then
  FOnMouseEnter(Self);
end;

procedure TSXSkinCustomRadioButton.InternalMouseLeave;
begin
 if Enabled then
  StartGlyphChanging(gcaHighlightOut);
 FMouseOver:=False;
 if Enabled then
  begin
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
 if Assigned(FOnMouseLeave) then
  FOnMouseLeave(Self);
end;

procedure TSXSkinCustomRadioButton.MouseLeave;
begin
 if FMouseOver then
  InternalMouseLeave;
 inherited;
end;

procedure TSXSkinCustomRadioButton.MouseMove(Shift:TShiftState;X,Y:Integer);
var B:Boolean;
    P:TPoint;
begin
 inherited;
 if Enabled then
  begin
   P:=Point(X,Y);
   B:=(X>=0) and (X<Width) and (Y>=0) and (Y<Height) and CapturesMouseAt(X,Y);
   if B<>FMouseOver then
    begin
     if B then InternalMouseEnter else
      InternalMouseLeave;
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
 if Enabled and FMouseOver and (Button=mbLeft) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   SetFocus;
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
 inherited;
end;

procedure TSXSkinCustomRadioButton.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var OldDown,WasChecked:Boolean;
begin
 if Enabled then
  begin
   OldDown:=FDown;
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
   if OldDown and FMouseOver then
    begin
     WasChecked:=FChecked;
     Click;
     if Assigned(FOnUserModified) and (not WasChecked) then
      FOnUserModified(Self);
    end;
  end;
 inherited; 
end;

procedure TSXSkinCustomRadioButton.DoClick;
var A:Integer;
begin
 if Visible and Enabled and FDown then
  begin
   if not FChecked then
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
            (TSXSkinCustomGroupBox(Parent.Controls[A]).State=cbChecked) then
          begin
           TSXSkinCustomGroupBox(Parent.Controls[A]).DoUncheck;
          end;
        end;
      end;
     FChecked:=True;
     ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                             rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
     if Assigned(FOnChange) then
      FOnChange(Self);
    end else
     begin
      StartGlyphChanging(gcaUp);
      FDown:=False;
      ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                              rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
     end;
  end else FDown:=False;
end;

procedure TSXSkinCustomRadioButton.Click;
begin
 FDown:=True;
 DoClick;
 inherited;
end;

procedure TSXSkinCustomRadioButton.InvalidateGlyph;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FGlyphRect,False);
end;

procedure TSXSkinCustomRadioButton.DoThreadActions;
begin
 if not (csDestroying in ComponentState) then
  begin
   Inc(FDoneSteps);
   if (FThread<>nil) and not FThread.Suspended and
      (not HasTransformEffect(FLastRadioButtonTransform) or (FDoneSteps>=FLastRadioButtonTransform.StepsNum)) and
      (not HasTransformEffect(FLastGlyphTransform) or (FDoneSteps>=FLastGlyphTransform.StepsNum)) then
    begin
     FThread.Suspend;
    end;
   if HasTransformEffect(FLastRadioButtonTransform) and (FDoneSteps<=FLastRadioButtonTransform.StepsNum) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
    end else InvalidateGlyph;
   Update;
  end;
end;

procedure TSXSkinCustomRadioButton.CreateThreadIfNeeded;
begin
 if FThread=nil then
  begin
   FThread:=TSXSkinCustomRadioButtonThread.Create;
   FThread.Control:=Self;
  end; 
end;

procedure TSXSkinCustomRadioButton.StartGlyphChanging(T:TSXGlyphChangeAction);
var        B:TBitmap32;
         Rgn:HRGN;
  NeedThread:Boolean;
 CBTransform:TSXTransformEffectData;
  GTransform:TSXTransformEffectData;
begin
 GetCurrentTransformEffect(cbtRadioButton,T,CBTransform);
 GetCurrentTransformEffect(cbtGlyph,T,GTransform);
 NeedThread:=HasTransformEffect(CBTransform) or
             HasTransformEffect(GTransform);
 if NeedThread then
  begin
   CreateThreadIfNeeded;
   if HasTransformEffect(GTransform) then
    B:=CreateCurrentBlendedGlyph else B:=nil;
   FLastGlyph.Free;
   FLastGlyph:=B;
   //
   if HasTransformEffect(CBTransform) then
    begin
     B:=TBitmap32.Create;
     B.DrawMode:=dmBlend;
     B.CombineMode:=cmMerge;
     B.SetSize(Width,Height);
     B.Clear(0);
     Rgn:=CreateRectRgn(0,0,Width,Height);
     PaintCurrentBlendedCBStyle(B,0,0,Rect(0,0,Width,Height),Rgn);
     if not (HasTransformEffect(FLastRadioButtonTransform) and (FDoneSteps<FLastRadioButtonTransform.StepsNum)) and
             CBTransform.DrawCaption then
      PaintCurrentOverStyleCaptionToBitmap(B.Handle,B.BoundsRect,Rect(0,0,Width,Height),Rgn,B,0,0);
     DeleteObject(Rgn);
    end else B:=nil;
   FLastRadioButton.Free;
   FLastRadioButton:=B;
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
 FLastRadioButtonTransform:=CBTransform;
 FLastGlyphTransform:=GTransform;
end;

procedure TSXSkinCustomRadioButton.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   FLastGlyph.Free;
   FLastGlyph:=nil;
   FLastRadioButton.Free;
   FLastRadioButton:=nil;
   FTextBitmap.Free;
   FTextBitmap:=nil;
   if (FThread<>nil) and not FThread.Suspended then
    FThread.Suspend;
   ResetRadioButtonParams([rbrpText,rbrpGlyph]);
  end;
 inherited;
end;

function TSXSkinCustomRadioButton.GetCaption:TCaption;
begin
 Result:=inherited Caption;
end;

procedure TSXSkinCustomRadioButton.SetCaption(const Value:TCaption);
begin
 if csLoading in ComponentState then
  begin
   inherited Caption:=Value;
   exit;
  end;
 if Caption<>Value then
  begin
   inherited Caption:=Value;
   ResetRadioButtonParams([rbrpText]);
  end;
end;

procedure TSXSkinCustomRadioButton.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
var DoReset:Boolean;
begin
 if not (csLoading in ComponentState) then
  begin
   if FAutoSizeWidth then AWidth:=Width;
   if FAutoSizeHeight then AHeight:=Height;
  end;
 if (ALeft=Left) and (ATop=Top) and (AWidth=Width) and (AHeight=Height) then exit; 
 DoReset:=((Width<>AWidth) and WordWrap) or ((Height<>AHeight) and (VerticalAlignment<>taAlignTop));
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
 if not (csLoading in ComponentState) then
  begin
   if DoReset then
    ResetRadioButtonParams([rbrpText]) else
     ResetRadioButtonParams;
  end;
end;

procedure TSXSkinCustomRadioButton.InternalSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

procedure TSXSkinCustomRadioButton.CMFontChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  ResetRadioButtonParams([rbrpTextOnFontChange]);
end;

procedure TSXSkinCustomRadioButton.Loaded;
begin
 inherited;
 ResetRadioButtonParams([rbrpText,rbrpGlyph]);
end;

procedure TSXSkinCustomRadioButton.WMSetFocus(var Msg:TWMSetFocus);
begin
 StartGlyphChanging(gcaFocus);
 FLastFocused:=True;
 ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                         rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomRadioButton.WMKillFocus(var Msg:TWMKillFocus);
begin
 StartGlyphChanging(gcaUnfocus);
 FLastFocused:=False;
 ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                         rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
 if FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomRadioButton.DoKeyDown(var Msg:TMessage);
begin
 inherited;
 if Enabled and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomRadioButton.DoKeyUp(var Msg:TMessage);
begin
 inherited;
 if Enabled and FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                           rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
   if not Checked and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
    begin
     Click;
     if Assigned(FOnUserModified) then
      FOnUserModified(Self);
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.DoUncheck;
begin
 StartGlyphChanging(gcaUncheck);
 FChecked:=False;
 ResetRadioButtonParams([rbrpTextOnFontChange,rbrpGlyphOnSizeChange,
                         rbrpGlyphOnStyleChange,rbrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomRadioButton.GetCurrentTransformEffect(T:TSXSkinRadioButtonTransform;
           Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
var    A:Integer;
 CBStyle:TSXSkinRadioButtonStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     CBStyle:=TSXSkinRadioButtonStyle(SkinLibrary.Styles[A]);
     case T of
      cbtRadioButton: begin
                    case Action of
                     gcaHighlightIn:  Effect:=CBStyle.HInRadioButtonEffect;
                     gcaHighlightOut: Effect:=CBStyle.HOutRadioButtonEffect;
                     gcaDown:         Effect:=CBStyle.DownRadioButtonEffect;
                     gcaUp:           Effect:=CBStyle.UpRadioButtonEffect;
                     gcaCheck:        Effect:=CBStyle.CheckRadioButtonEffect;
                     gcaUncheck:      Effect:=CBStyle.UncheckRadioButtonEffect;
                     gcaEnable:       Effect:=CBStyle.EnableRadioButtonEffect;
                     gcaDisable:      Effect:=CBStyle.DisableRadioButtonEffect;
                     gcaFocus:        Effect:=CBStyle.FocusRadioButtonEffect;
                     gcaUnfocus:      Effect:=CBStyle.UnfocusRadioButtonEffect;
                    end;
                   end;
      cbtGlyph:    begin
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
     end;
    end;
  end;
end;

procedure TSXSkinCustomRadioButton.ResetRadioButtonParams(Params:TSXSkinRadioButtonResetParams=[]);
var OldGlyphRect:TRect;
     OldTextRect:TRect;
    GlyphChanged:Boolean;
     TextChanged:Boolean;
      A,B,Offset:Integer;
         RBState:TSXSkinRadioButtonStateParam;
      SetRBState:Boolean;
           Flags:Cardinal;
      GlyphStyle:TSXSkinGeneralStyle;
           Image:TSXSkinStyleImageElement;
   AAGlyphOffset:Integer;
    AATextOffset:Integer;
      FTextWidth:Integer;
     FTextHeight:Integer;
           TH,TW:Integer;
        NewWidth:Integer;
       NewHeight:Integer;

 procedure DoSetRBState;
 begin
  if not SetRBState then
   GetCurrentRBState(RBState);
 end;

begin
 if Parent=nil then exit;
 OldGlyphRect:=FGlyphRect;
 OldTextRect:=FTextRect;
 GlyphChanged:=False;
 TextChanged:=False;
 SetRBState:=False;
 if Params*[rbrpGlyph,rbrpGlyphOnStyleChange,rbrpGlyphOnSizeChange]<>[] then
  begin
   DoSetRBState;
   //
   FGlyphWidth:=RBState.GlyphWidth;
   FGlyphHeight:=RBState.GlyphHeight;
   if (RBState.GlyphStyle<>'') and ((FGlyphWidth=0) or (FGlyphHeight=0)) then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(RBState.GlyphStyle,0,0);
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
            if FGlyphWidth=0 then FGlyphWidth:=Image.Bitmap.Width;
            if FGlyphHeight=0 then FGlyphHeight:=Image.Bitmap.Height;
           end;
         end;
      end;
    end;
   //
   if rbrpGlyph in Params then GlyphChanged:=True else
    begin
     if rbrpGlyphOnSizeChange in Params then
      GlyphChanged:=(FGlyphWidth<>FGlyphRect.Right-FGlyphRect.Left) or
                    (FGlyphHeight<>FGlyphRect.Bottom-FGlyphRect.Top);
     if not GlyphChanged and (rbrpGlyphOnStyleChange in Params) then
      GlyphChanged:=(FLastGlyphStyle<>RBState.GlyphStyle);
    end;
   if GlyphChanged then
    FGlyphRect:=Rect(0,0,FGlyphWidth,FGlyphHeight);
   FLastGlyphStyle:=RBState.GlyphStyle;
  end;
 if Params*[rbrpText,rbrpTextOnFontChange,rbrpTextIfWordWrap]<>[] then
  begin
   DoSetRBState;
   if rbrpText in Params then TextChanged:=True else
    if FWordWrap and (rbrpTextIfWordWrap in Params) then TextChanged:=True else
    if rbrpTextOnFontChange in Params then
     TextChanged:=not SameFontData(RBState.FD,FLastFontData);
   if TextChanged then
    begin
     if FTextBitmap<>nil then
      begin
       FTextBitmap.Free;
       FTextBitmap:=nil;
      end;
     FLastFontData:=RBState.FD;
     Canvas.Font.Name:=RBState.FD.FontName;
     Canvas.Font.Size:=RBState.FD.FontSize;
     Canvas.Font.Style:=RBState.FD.FontStyle;
     FTextRect:=Rect(0,0,Width,Height);
     if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
      begin
       A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
       if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
        begin
         case FGlyphPosition of
          gpLeftTop,
          gpLeft,
          gpLeftBottom,
          gpRightTop,
          gpRight,
          gpRightBottom: begin
                          Dec(FTextRect.Right,FGlyphRect.Right-FGlyphRect.Left);
                          if (FLastGlyphStyle<>'') and (Caption<>'') then
                           Dec(FTextRect.Right,FTextOffset);
                          Dec(FTextRect.Right,RBState.CaptionLeftOffset);
                          Dec(FTextRect.Right,RBState.CaptionRightOffset);
                          //
                          Dec(FTextRect.Bottom,RBState.CaptionTopOffset);
                          Dec(FTextRect.Bottom,RBState.CaptionBottomOffset);
                         end;
          gpTop,
          gpBottom:      begin
                          Dec(FTextRect.Right,RBState.CaptionLeftOffset);
                          Dec(FTextRect.Right,RBState.CaptionRightOffset);
                          //
                          Dec(FTextRect.Right,FGlyphRect.Bottom-FGlyphRect.Top);
                          if (FLastGlyphStyle<>'') and (Caption<>'') then
                           Dec(FTextRect.Right,FTextOffset);
                          Dec(FTextRect.Bottom,RBState.CaptionTopOffset);
                          Dec(FTextRect.Bottom,RBState.CaptionBottomOffset);
                         end;
         end;
         Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP or DT_CALCRECT;
         case FAlignment of
          taLeftJustify:  Flags:=Flags or DT_LEFT;
          taRightJustify: Flags:=Flags or DT_RIGHT;
          taCenter:       Flags:=Flags or DT_CENTER;
         end;
         if FWordWrap then Flags:=Flags or DT_WORDBREAK;
         Dec(FTextRect.Right,RBState.TextLeftOffset+RBState.TextRightOffset);
         Dec(FTextRect.Bottom,RBState.TextTopOffset+RBState.TextBottomOffset);
         if RBState.FD.SmoothLevel=0 then
          DrawText(Canvas.Handle,PChar(Caption),-1,FTextRect,Flags) else
           DrawSmoothText(Canvas,Caption,FTextRect,Flags,RBState.FD.SmoothLevel);
         Inc(FTextRect.Right,RBState.TextLeftOffset+RBState.TextRightOffset);
         Inc(FTextRect.Bottom,RBState.TextTopOffset+RBState.TextBottomOffset);
         FLastTextLeftOffset:=RBState.TextLeftOffset;
         FLastTextTopOffset:=RBState.TextTopOffset;
         FLastTextRightOffset:=RBState.TextRightOffset;
         FLastTextBottomOffset:=RBState.TextBottomOffset;
        end;
      end;
    end;
   FLastFontData:=RBState.FD;
  end;
 //
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinRadioButtonStyle) then
    begin
     DoSetRBState;
     FTextWidth:=FTextRect.Right-FTextRect.Left;
     FTextHeight:=FTextRect.Bottom-FTextRect.Top;
     AATextOffset:=0;
     AAGlyphOffset:=0;
     if FAutoAlignFirstLine and (FGlyphPosition in [gpLeftTop,gpRightTop]) and
        not IsRectEmpty(FGlyphRect) then
      begin
       if RBState.FD.SmoothLevel=0 then
        TH:=Canvas.TextHeight(Caption) else
         GetRenderedTextSize(Canvas,Caption,RBState.FD.SmoothLevel,TW,TH);
       AATextOffset:=(FGlyphHeight-TH) div 2-RBState.TextTopOffset;
       if AATextOffset<0 then
        begin
         AAGlyphOffset:=-AATextOffset;
         AATextOffset:=0;
        end;
      end else
     if FGlyphPosition in [gpLeft,gpRight] then
      begin
       AAGlyphOffset:=(FTextHeight-FGlyphHeight) div 2;
       if AAGlyphOffset<0 then
        begin
         AATextOffset:=-AAGlyphOffset;
         AAGlyphOffset:=0;
        end;
      end else
     if FGlyphPosition in [gpLeftBottom,gpRightBottom] then
      begin
       AAGlyphOffset:=FTextHeight-FGlyphHeight;
       if AAGlyphOffset<0 then
        begin
         AATextOffset:=-AAGlyphOffset;
         AAGlyphOffset:=0;
        end;
      end;
     //
     NewWidth:=Width; NewHeight:=Height;
     if FAutoSizeWidth or FAutoSizeHeight then
      begin
       case FGlyphPosition of
        gpLeftTop,
        gpLeft,
        gpLeftBottom,
        gpRightTop,
        gpRight,
        gpRightBottom: begin
                        NewWidth:=FGlyphWidth+FTextWidth+FTextOffset+
                                  RBState.CaptionLeftOffset+
                                  RBState.CaptionRightOffset;
                        NewHeight:=RBState.CaptionTopOffset+
                                   RBState.CaptionBottomOffset+
                                   Max(FGlyphHeight+AAGlyphOffset,FTextHeight+AATextOffset);
                       end;
        gpTop,
        gpBottom:      begin
                        NewWidth:=RBState.CaptionLeftOffset+
                                  RBState.CaptionRightOffset+
                                  Max(FGlyphWidth,FTextWidth);
                        NewHeight:=RBState.CaptionTopOffset+
                                   RBState.CaptionBottomOffset+FTextOffset+
                                   FGlyphHeight+FTextHeight;
                       end;
       end;
       if not FAutoSizeWidth then NewWidth:=Width;
       if not FAutoSizeHeight then NewHeight:=Height;
      end;
     //
     case FGlyphPosition of
      gpLeftTop,
      gpLeft,
      gpLeftBottom:  begin
                      FGlyphRect.Left:=RBState.CaptionLeftOffset;
                      FGlyphRect.Top:=RBState.CaptionTopOffset+AAGlyphOffset;
                      //
                      FTextRect.Left:=FGlyphRect.Left+FGlyphWidth+FTextOffset;
                      FTextRect.Top:=RBState.CaptionTopOffset+AATextOffset;
                     end;
      gpTop:         begin
                      FGlyphRect.Left:=(NewWidth-FGlyphWidth+RBState.CaptionLeftOffset-RBState.CaptionRightOffset) div 2;
                      FGlyphRect.Top:=RBState.CaptionTopOffset;
                      //
                      FTextRect.Left:=(NewWidth-FTextWidth+RBState.CaptionLeftOffset-RBState.CaptionRightOffset) div 2;
                      FTextRect.Top:=FGlyphRect.Top+FGlyphHeight+FTextOffset;
                     end;
      gpRightTop,
      gpRight,
      gpRightBottom: begin
                      FTextRect.Left:=RBState.CaptionLeftOffset;
                      FTextRect.Top:=RBState.CaptionTopOffset+AATextOffset;
                      //
                      FGlyphRect.Left:=FTextRect.Left+FTextWidth+FTextOffset;
                      FGlyphRect.Top:=RBState.CaptionTopOffset+AAGlyphOffset;
                     end;
      gpBottom:      begin
                      FTextRect.Left:=(NewWidth-FTextWidth+RBState.CaptionLeftOffset-RBState.CaptionRightOffset) div 2;
                      FTextRect.Top:=RBState.CaptionTopOffset;
                      //
                      FGlyphRect.Left:=(NewWidth-FGlyphWidth+RBState.CaptionLeftOffset-RBState.CaptionRightOffset) div 2;
                      FGlyphRect.Top:=FTextRect.Top+FTextHeight+FTextOffset;
                     end;
     end;
     FGlyphRect.Right:=FGlyphRect.Left+FGlyphWidth;
     FGlyphRect.Bottom:=FGlyphRect.Top+FGlyphHeight;
     FTextRect.Right:=FTextRect.Left+FTextWidth;
     FTextRect.Bottom:=FTextRect.Top+FTextHeight;
     //
     if (Width<>NewWidth) or (Height<>NewHeight) then
      InternalSetBounds(Left,Top,NewWidth,NewHeight);
     if not FAutoSizeHeight and (FVerticalAlignment<>taAlignTop) then
      begin
       Offset:=0;
       case FVerticalAlignment of
        taVerticalCenter: Offset:=(Height-Max(FGlyphRect.Bottom,FTextRect.Bottom)-
                                   Min(FGlyphRect.Top,FTextRect.Top)) div 2;
        taAlignBottom:    Offset:=Height-Max(FGlyphRect.Bottom,FTextRect.Bottom)-
                                  2*Min(FGlyphRect.Top,FTextRect.Top);
       end;
       Inc(FGlyphRect.Top,Offset);
       Inc(FGlyphRect.Bottom,Offset);
       Inc(FTextRect.Top,Offset);
       Inc(FTextRect.Bottom,Offset);
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
 if rbrpInvalidateOnStyleChange in Params then
  begin
   DoSetRBState;
   if (RBState.Style<>FLastStyle) or (RBState.OverStyle<>FLastOverStyle) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
     FLastStyle:=RBState.Style;
     FLastOverStyle:=RBState.OverStyle;
    end;
  end;
end;

function TSXSkinCustomRadioButton.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 RBState:TSXSkinRadioButtonStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentRBState(RBState);
   A:=SkinLibrary.Styles.GetGStyleIndex(RBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
    end;
   if Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(RBState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
      end;
    end;
   if Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(RBState.GlyphStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Glyph,X,Y,Width,Height,SkinLibrary,Limit);
      end;
    end;
  end;
end;

function TSXSkinCustomRadioButton.CapturesMouseAt(X,Y:Integer):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 RBState:TSXSkinRadioButtonStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentRBState(RBState);
   A:=SkinLibrary.Styles.GetGStyleIndex(RBState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
    end;
   if not Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(RBState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
      end;
    end;
   if not Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(RBState.GlyphStyle,FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Glyph,X-FGlyphRect.Left,Y-FGlyphRect.Top,
          FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top,
          SkinLibrary);
      end;
    end;
  end;
end;

constructor TSXSkinCustomRadioButton.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 CEID_Back:=GetNewCElementID;
 CEID_Glyph:=GetNewCElementID;
 VComparer:=TSXRadioButtonVariableComparer.Create;
 VComparer.Control:=Self;
 VComparer.OnGetVariable:=OnGetVariable;
 ControlStyle:=ControlStyle+[csSetCaption]-[csDoubleClicks];
 FAutoSizeWidth:=True;
 FAutoSizeHeight:=True;
 FAutoAlignFirstLine:=True;
 FTextOffset:=2;
 FGlyphPosition:=gpLeftTop;
 SkinStyle:='_RadioButton';
 TabStop:=True;
end;

destructor TSXSkinCustomRadioButton.Destroy;
begin
 FThread.Free;
 FTextBitmap.Free;
 FLastGlyph.Free;
 FLastRadioButton.Free;
 inherited;
 VComparer.Free; 
end;

end.
