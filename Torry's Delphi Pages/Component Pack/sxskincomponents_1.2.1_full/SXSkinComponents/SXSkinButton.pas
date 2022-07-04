unit SXSkinButton;

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

uses Windows, Classes, SXSkinControl, GR32, StdCtrls, Dialogs, SysUtils, Forms,
     Messages, Controls, GR32_Blend, SXSkinLibrary, Types, Graphics, Menus,
     GR32_Polygons;

const

 VARB_W   = 1;
 VARB_H   = 2;
 VARB_GlX = 3;
 VARB_GlY = 4;
 VARB_GlW = 5;
 VARB_GlH = 6;

type

 TSXSkinCustomButton=class;

 TSXSkinCustomButtonThread=class(TThread)
  public
   Control:TSXSkinCustomButton;
   constructor Create;
   procedure Execute; override;
   procedure DoEvent;
 end;

 TSXSkinButtonResetParam=(brpText,
                          brpTextOnFontChange,
                          brpTextIfWordWrap,
                          brpGlyph,
                          brpGlyphOnStyleChange,
                          brpGlyphOnSizeChange,
                          brpDDGlyph,
                          brpDDGlyphOnStyleChange,
                          brpInvalidateOnStyleChange);

 TSXSkinButtonResetParams=set of TSXSkinButtonResetParam;

 TSXSkinButtonTransform=(btButton,btGlyph,btDDGlyph);

 TSXButtonVariableComparer=class(TSXVariableComparer)
  private
   Control:TSXSkinCustomButton;
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

 TSXSkinCustomButton=class(TSXSkinCustomControl)
  private
   FGlyphPosition:TSXGlyphPosition;
   FWordWrap:Boolean;
   FCaptionLeftOffset:Integer;
   FCaptionTopOffset:Integer;
   FCaptionRightOffset:Integer;
   FCaptionBottomOffset:Integer;
   FCanBeChecked:Boolean;
   FChecked:Boolean;
   FDropDown:Boolean;
   FDropDownMenu:TPopupMenu;
   FGlyphStyle:String;
   FDropDownGlyphStyle:String;
   FTextOffset:Integer;
   FAlignment:TAlignment;
   FGlyphWidth:Integer;
   FGlyphHeight:Integer;
   FOnClick:TNotifyEvent;
   FMouseOver:Boolean;
   FDown:Boolean;
   FThread:TSXSkinCustomButtonThread;
   //
   FLastButtonTransform:TSXTransformEffectData;
   FLastGlyphTransform:TSXTransformEffectData;
   FLastDDGlyphTransform:TSXTransformEffectData;
   FDoneSteps:Integer;
   FLastGlyph:TBitmap32;
   FLastDropDownGlyph:TBitmap32;
   FLastButton:TBitmap32;
   //
   FGlyphRect:TRect;
   FTextRect:TRect;
   FDropDownGlyphRect:TRect;
   FTextBitmap:TBitmap32;
   FLastFontData:TSXFontData;
   FLastStyle:String;
   FLastOverStyle:String;
   FLastGlyphStyle:String;
   FLastGlyphFilterData:TSXFilterData;
   FLastDropDownGlyphStyle:String;
   FLastFocused:Boolean;
   CEID_Back:Integer;
   CEID_Glyph:Integer;
   CEID_DDGlyph:Integer;
   VComparer:TSXButtonVariableComparer;
   procedure SetCaption(const Value:TCaption);
   procedure SetAlignment(Value:TAlignment);
   procedure SetGlyphPosition(Value:TSXGlyphPosition);
   procedure SetWordWrap(Value:Boolean);
   procedure SetCaptionLeftOffset(Value:Integer);
   procedure SetCaptionTopOffset(Value:Integer);
   procedure SetCaptionRightOffset(Value:Integer);
   procedure SetCaptionBottomOffset(Value:Integer);
   procedure SetTextOffset(Value:Integer);
   procedure SetCanBeChecked(Value:Boolean);
   procedure SetChecked(Value:Boolean);
   procedure SetDropDown(Value:Boolean);
   procedure SetGlyphStyle(const Value:String);
   procedure SetDropDownGlyphStyle(const Value:String);
   procedure SetGlyphWidth(Value:Integer);
   procedure SetGlyphHeight(Value:Integer);
   procedure SetDropDownMenu(Value:TPopupMenu);
   function HasUnusualSkinStyle:Boolean;
   function HasUnusualDropDownGlyphStyle:Boolean;
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   function CreateCurrentGlyph:TBitmap32;
   function CreateCurrentBlendedGlyph:TBitmap32;
   function CreateCurrentDropDownGlyph:TBitmap32;
   function CreateCurrentBlendedDropDownGlyph:TBitmap32;
   procedure DoThreadActions;
   procedure StartGlyphChanging(T:TSXGlyphChangeAction);
   procedure GetCurrentTransformEffect(T:TSXSkinButtonTransform;
              Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
   procedure CreateThreadIfNeeded;
   procedure ResetButtonParams(Params:TSXSkinButtonResetParams=[]);
   procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
   procedure InvalidateGlyph;
   procedure GetCurrentBState(var BState:TSXSkinButtonStateParam);
   procedure GetCurrentGlyphStyle(var GStyle:String;PGlyphWidth:PInteger=nil;
              PGlyphHeight:PInteger=nil;PUseFilter:PBoolean=nil);
   procedure GetCurrentDropDownGlyphStyle(var GStyle:String;PGlyphWidth:PInteger=nil;
              PGlyphHeight:PInteger=nil);
   procedure InternalMouseEnter;
   procedure InternalMouseLeave;
  protected
   function CapturesMouseAt(X,Y:Integer):Boolean; override;
   procedure SetEnabled(Value:Boolean); override;
   procedure Loaded; override;
   function GetCaption:TCaption;
   procedure DoClick;
   procedure Notification(AComponent:TComponent;Operation:TOperation); override;
   procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
   procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseLeave; override;
   procedure DoKeyDown(var Msg:TMessage); message CN_KEYDOWN;
   procedure DoKeyUp(var Msg:TMessage); message CN_KEYUP;
   procedure WMSetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
   procedure WMKillFocus(var Msg:TWMKillFocus); message WM_KILLFOCUS;
   procedure PaintCurrentBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentBlendedBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle:HDC;
              DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer);
   property Caption:TCaption read GetCaption write SetCaption;
   property GlyphPosition:TSXGlyphPosition read FGlyphPosition write SetGlyphPosition default gpLeft;
   property CaptionLeftOffset:Integer read FCaptionLeftOffset write SetCaptionLeftOffset default 0;
   property CaptionTopOffset:Integer read FCaptionTopOffset write SetCaptionTopOffset default 0;
   property CaptionRightOffset:Integer read FCaptionRightOffset write SetCaptionRightOffset default 0;
   property CaptionBottomOffset:Integer read FCaptionBottomOffset write SetCaptionBottomOffset default 0;
   property WordWrap:Boolean read FWordWrap write SetWordWrap default False;
   property Alignment:TAlignment read FAlignment write SetAlignment default taLeftJustify;
   property SkinStyle stored HasUnusualSkinStyle;
   property TextOffset:Integer read FTextOffset write SetTextOffset default 5;
   property CanBeChecked:Boolean read FCanBeChecked write SetCanBeChecked default False;
   property Checked:Boolean read FChecked write SetChecked default False;
   property DropDown:Boolean read FDropDown write SetDropDown default False;
   property DropDownMenu:TPopupMenu read FDropDownMenu write SetDropDownMenu;
   property GlyphStyle:String read FGlyphStyle write SetGlyphStyle;
   property DropDownGlyphStyle:String read FDropDownGlyphStyle write SetDropDownGlyphStyle stored HasUnusualDropDownGlyphStyle;
   property GlyphWidth:Integer read FGlyphWidth write SetGlyphWidth default 0;
   property GlyphHeight:Integer read FGlyphHeight write SetGlyphHeight default 0;
  public
   function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
   procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   procedure Click; reintroduce;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
  published
   property TabStop default True;
   property OnClick:TNotifyEvent read FOnClick write FOnClick;
 end;

 TSXSkinButton=class(TSXSkinCustomButton)
  published
   property Align;
   property Alignment;
   property Anchors;
   property CanBeChecked;
   property Caption;
   property CaptionLeftOffset;
   property CaptionTopOffset;
   property CaptionRightOffset;
   property CaptionBottomOffset;
   property Checked;
   property Color;
   property Constraints;
   property Cursor;
   property DragCursor;
   property DropDown;
   property DropDownGlyphStyle;
   property DropDownMenu;
   property Enabled;
   property Font;
   property GlyphHeight;
   property GlyphPosition;
   property GlyphStyle;
   property GlyphWidth;
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

{ TSXSkinCustomButtonThread }

constructor TSXSkinCustomButtonThread.Create;
begin
 inherited Create(True);
 FreeOnTerminate:=False;
end;

procedure TSXSkinCustomButtonThread.Execute;
begin
 while not Terminated do
  begin
   SleepEx(30,True);
   if not Suspended then
    Synchronize(DoEvent);
  end;
end;

procedure TSXSkinCustomButtonThread.DoEvent;
begin
 if Assigned(Control) then
  Control.DoThreadActions;
end;

{ TSXButtonVariableComparer }

function TSXButtonVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARB_W else
 if VarName='H' then
  CurVarVal:=VARB_H else
 if VarName='GlX' then
  CurVarVal:=VARB_GlX else
 if VarName='GlY' then
  CurVarVal:=VARB_GlY else
 if VarName='GlW' then
  CurVarVal:=VARB_GlW else
 if VarName='GlH' then
  CurVarVal:=VARB_GlH;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXButtonVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 if Control<>nil then
  begin
   case VarID of
    VARB_W:   Result:=Control.Width;
    VARB_H:   Result:=Control.Height;
    VARB_GlX: Result:=Control.FGlyphRect.Left;
    VARB_GlY: Result:=Control.FGlyphRect.Top;
    VARB_GlW: Result:=Control.FGlyphRect.Right-Control.FGlyphRect.Left;
    VARB_GlH: Result:=Control.FGlyphRect.Bottom-Control.FGlyphRect.Top;
   end;
  end;
end;

function TSXButtonVariableComparer.GetVarValsForVarList(VarList:TList):TList;
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

function TSXButtonVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
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

procedure TSXButtonVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXButtonVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXButtonVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

{ TSXSkinCustomButton }

function TSXSkinCustomButton.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_Button';
end;

function TSXSkinCustomButton.HasUnusualDropDownGlyphStyle:Boolean;
begin
 Result:=FDropDownGlyphStyle<>'_MultiState_DropDown';
end;

procedure TSXSkinCustomButton.SetEnabled(Value:Boolean);
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
    ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                       brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
  end; 
end;

procedure TSXSkinCustomButton.SetGlyphPosition(Value:TSXGlyphPosition);
begin
 if Value<>FGlyphPosition then
  begin
   FGlyphPosition:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetAlignment(Value:TAlignment);
begin
 if Value<>FAlignment then
  begin
   FAlignment:=Value;
   if not (csLoading in ComponentState) then
    begin
     ResetButtonParams([brpText]);
    end;
  end;
end;

procedure TSXSkinCustomButton.SetWordWrap(Value:Boolean);
begin
 if Value<>FWordWrap then
  begin
   FWordWrap:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpText]);
  end;
end;

procedure TSXSkinCustomButton.SetCaptionLeftOffset(Value:Integer);
begin
 if Value<>FCaptionLeftOffset then
  begin
   FCaptionLeftOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetCaptionTopOffset(Value:Integer);
begin
 if Value<>FCaptionTopOffset then
  begin
   FCaptionTopOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetCaptionRightOffset(Value:Integer);
begin
 if Value<>FCaptionRightOffset then
  begin
   FCaptionRightOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetCaptionBottomOffset(Value:Integer);
begin
 if Value<>FCaptionBottomOffset then
  begin
   FCaptionBottomOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetTextOffset(Value:Integer);
begin
 if Value<>FTextOffset then
  begin
   FTextOffset:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetCanBeChecked(Value:Boolean);
begin
 if Value<>FCanBeChecked then
  begin
   FCanBeChecked:=Value;
   if not (csLoading in ComponentState) then
    begin
     if not Value and Checked then
      Checked:=False;
    end;
  end;
end;

procedure TSXSkinCustomButton.SetChecked(Value:Boolean);
begin
 if Value<>FChecked then
  begin
   FChecked:=Value;
   if not (csLoading in ComponentState) then
    begin
     if Value and not FCanBeChecked then
      FChecked:=False else
       ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                          brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
    end;
  end;
end;

procedure TSXSkinCustomButton.SetDropDown(Value:Boolean);
begin
 if Value<>FDropDown then
  begin
   FDropDown:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpDDGlyphOnStyleChange]);
  end;
end;

procedure TSXSkinCustomButton.SetGlyphStyle(const Value:String);
begin
 if Value<>FGlyphStyle then
  begin
   FGlyphStyle:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpGlyphOnStyleChange,brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetDropDownGlyphStyle(const Value:String);
begin
 if Value<>FDropDownGlyphStyle then
  begin
   FDropDownGlyphStyle:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpDDGlyphOnStyleChange,brpTextIfWordWrap]);
  end;
end;

procedure TSXSkinCustomButton.SetGlyphWidth(Value:Integer);
begin
 if Value<>FGlyphWidth then
  begin
   FGlyphWidth:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpGlyphOnSizeChange]);
  end;
end;

procedure TSXSkinCustomButton.SetGlyphHeight(Value:Integer);
begin
 if Value<>FGlyphHeight then
  begin
   FGlyphHeight:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpGlyphOnSizeChange]);
  end;
end;

procedure TSXSkinCustomButton.SetDropDownMenu(Value:TPopupMenu);
begin
 if FDropDownMenu<>Value then
  begin
   if FDropDownMenu<>nil then
    FDropDownMenu.RemoveFreeNotification(Self);
   FDropDownMenu:=Value;
   if FDropDownMenu<>nil then
    FDropDownMenu.FreeNotification(Self);
  end;
end;

procedure TSXSkinCustomButton.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
  begin
   if AComponent=FDropDownMenu then
    FDropDownMenu:=nil;
  end;
end;

function TSXSkinCustomButton.OnGetVariable(const VarName:String;var Error:Boolean):Single;
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
   Result:=FGlyphRect.Right-FGlyphRect.Left; exit;
  end;
 if VarName='GlH' then
  begin
   Result:=FGlyphRect.Bottom-FGlyphRect.Top; exit;
  end;
 Error:=True;
end;

function TSXSkinCustomButton.CreateCurrentGlyph:TBitmap32;
var GStyle:String;
 UseFilter:Boolean;
    BState:TSXSkinButtonStateParam;

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
   GetCurrentGlyphStyle(GStyle,nil,nil,@UseFilter);
   if GStyle<>'' then
    SetGlyphByStyleName(GStyle,Result);
   if UseFilter and (Result<>nil) then
    begin
     GetCurrentBState(BState);
     ApplyFilterToBitmap(Result,BState.DefGlyphFilter);
    end;
  end;
end;

function TSXSkinCustomButton.CreateCurrentBlendedGlyph:TBitmap32;
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

function TSXSkinCustomButton.CreateCurrentDropDownGlyph:TBitmap32;
var GStyle:String;

 procedure SetGlyphByStyleName(const StyleName:String;var BB:TBitmap32);
 var       C:Integer;
  GlyphStyle:TSXSkinGeneralStyle;
         Rgn:HRGN;
 begin
  C:=SkinLibrary.Styles.GetGStyleIndex(StyleName,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
                                       FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
  if C>=0 then
   begin
    GlyphStyle:=TSXSkinGeneralStyle(SkinLibrary.Styles[C]);
    BB:=TBitmap32.Create;
    BB.SetSize(FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
               FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
    BB.Clear(0);
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    Rgn:=CreateRectRgn(0,0,BB.Width,BB.Height);
    GlyphStyle.DrawToBitmap(Self,CEID_DDGlyph,BB,0,0,BB.Width,BB.Height,Rect(0,0,BB.Width,BB.Height),
         Rgn,SkinLibrary);
    DeleteObject(Rgn);
   end;
 end;

begin
 Result:=nil;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentDropDownGlyphStyle(GStyle);
   if GStyle<>'' then
    SetGlyphByStyleName(GStyle,Result);
  end;
end;

function TSXSkinCustomButton.CreateCurrentBlendedDropDownGlyph:TBitmap32;
var B:TBitmap32;
begin
 Result:=CreateCurrentDropDownGlyph;
 if (Result<>nil) and HasTransformEffect(FLastDDGlyphTransform) and
    (FDoneSteps<FLastDDGlyphTransform.StepsNum) and (FLastDropDownGlyph<>nil) then
  begin
   B:=Result;
   try
    Result:=TBitmap32.Create;
    Result.SetSize(B.Width,B.Height);
    Result.DrawMode:=dmBlend;
    Result.CombineMode:=cmMerge;
    ApplyTransformEffectToBitmaps(FLastDropDownGlyph,B,FLastDDGlyphTransform,FDoneSteps,Result);
   finally
    B.Free;
   end;
  end;
end;

procedure TSXSkinCustomButton.PaintCurrentBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
  BState:TSXSkinButtonStateParam;
   Style:TSXSkinGeneralStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentBState(BState);
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
   //
   if HasTransformEffect(FLastButtonTransform) and (FDoneSteps<FLastButtonTransform.StepsNum) and
      FLastButtonTransform.DrawCaption then
    begin
     PaintCurrentOverStyleCaptionToBitmap(Bitmap.Handle,Rect,Rect,Rgn,Bitmap,X,Y);
    end;
  end;
end;

procedure TSXSkinCustomButton.PaintCurrentBlendedBStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var     BB:TBitmap32;
 CurButton:TBitmap32;
       Rgn2:HRGN;
begin
 if HasTransformEffect(FLastButtonTransform) and
    (FDoneSteps<FLastButtonTransform.StepsNum) and (FLastButton<>nil) then
  begin
   CurButton:=TBitmap32.Create;
   BB:=TBitmap32.Create;
   try
    CurButton.DrawMode:=dmBlend;
    CurButton.CombineMode:=cmMerge;
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    CurButton.SetSize(Width,Height);
    CurButton.Clear(0);
    Rgn2:=CreateRectRgn(0,0,Width,Height);
    PaintCurrentBStyle(CurButton,0,0,Types.Rect(0,0,Width,Height),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(Width,Height);
    ApplyTransformEffectToBitmaps(FLastButton,CurButton,FLastButtonTransform,FDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurButton.Free;
   end;
  end else PaintCurrentBStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomButton.PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer);
var   BState:TSXSkinButtonStateParam;
       A,W,H:Integer;
    CurGlyph:TBitmap32;
       Flags:Cardinal;
 RR,TextRect:TRect;
       Style:TSXSkinGeneralStyle;

 procedure DrawTextToBitmap(Bitmap:TBitmap32);
 begin
  Bitmap.Font:=Canvas.Font;
  if BState.FD.HasShadow then
   begin
    OffsetRect(TextRect,1,1);
    if BState.FD.SmoothLevel=0 then
     DrawAlphaText(Bitmap,Caption,TextRect,Flags,BState.FD.ShadowColor) else
      DrawSmoothText(Bitmap,Caption,TextRect,Flags,BState.FD.SmoothLevel,BState.FD.ShadowColor);
    OffsetRect(TextRect,-1,-1);
   end;
  if BState.FD.SmoothLevel=0 then
   DrawAlphaText(Bitmap,Caption,TextRect,Flags,BState.FD.FontColor) else
    DrawSmoothText(Bitmap,Caption,TextRect,Flags,BState.FD.SmoothLevel,BState.FD.FontColor);
 end;

begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentBState(BState);
   //OverStyle
   if BState.OverStyle<>'' then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,Width,Height);
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
   //DropDownGlyph
   if FDropDown and RectInRegion(Rgn,FDropDownGlyphRect) then
    begin
     CurGlyph:=CreateCurrentBlendedDropDownGlyph;
     try
      if CurGlyph<>nil then
       Bitmap.Draw(X-Rect.Left+FDropDownGlyphRect.Left,Y-Rect.Top+FDropDownGlyphRect.Top,CurGlyph);
     finally
      CurGlyph.Free;
     end;
    end;
   //Text
   if (Caption<>'') and RectInRegion(Rgn,FTextRect) then
    begin
     Bitmap.Font:=Canvas.Font;
     Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP;
     case FAlignment of
      taLeftJustify:  Flags:=Flags or DT_LEFT;
      taRightJustify: Flags:=Flags or DT_RIGHT;
      taCenter:       Flags:=Flags or DT_CENTER;
     end;
     if FWordWrap then Flags:=Flags or DT_WORDBREAK;
     TextRect:=FTextRect;
     OffsetRect(TextRect,X-Rect.Left,Y-Rect.Top);
     if BState.FD.DoPrepaint and (BState.FD.SmoothLevel>0) and (FTextBitmap=nil) then
      begin
       FTextBitmap:=TBitmap32.Create;
       FTextBitmap.DrawMode:=dmBlend;
       FTextBitmap.CombineMode:=cmMerge;
       W:=TextRect.Right-TextRect.Left;
       H:=TextRect.Bottom-TextRect.Top;
       if BState.FD.HasShadow then
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
     if BState.FD.DoPrepaint and (BState.FD.SmoothLevel>0) then
      FTextBitmap.DrawTo(Bitmap,TextRect.Left,TextRect.Top) else
       DrawTextToBitmap(Bitmap);
    end;
  end;
end;

procedure TSXSkinCustomButton.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   PaintCurrentBlendedBStyle(Bitmap,X,Y,Rect,Rgn);
   if not (HasTransformEffect(FLastButtonTransform) and (FDoneSteps<FLastButtonTransform.StepsNum) and
           FLastButtonTransform.DrawCaption) then
    PaintCurrentOverStyleCaptionToBitmap(DestCanvasHandle,DestCanvasRect,Rect,Rgn,Bitmap,X,Y);
  end;
 inherited;
end;

procedure TSXSkinCustomButton.InternalMouseEnter;
begin
 if Enabled and not FDown then
  StartGlyphChanging(gcaHighlightIn);
 FMouseOver:=True;
 if Enabled and not FDown then
  begin
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomButton.InternalMouseLeave;
begin
 if Enabled and not FDown then
  StartGlyphChanging(gcaHighlightOut);
 FMouseOver:=False;
 if Enabled and not FDown then
  begin
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomButton.MouseLeave;
begin
 if FMouseOver then
  InternalMouseLeave;
 inherited;
end;

procedure TSXSkinCustomButton.MouseMove(Shift:TShiftState;X,Y:Integer);
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

procedure TSXSkinCustomButton.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var P:TPoint;
begin
 if Enabled and FMouseOver and (Button=mbLeft) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   SetFocus;
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
   if FDropDown and Assigned(FDropDownMenu) then
    begin
     FMouseOver:=False;
     Click;
     P:=ScreenToClient(Mouse.CursorPos);
     SendMessage(Handle,WM_MOUSEMOVE,0,P.X or (P.Y shl 16));
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomButton.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var OldDown:Boolean;
          P:TPoint;
begin
 if Enabled then
  begin
   OldDown:=FDown;
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
   if OldDown and FMouseOver then
    begin
     Click;
     P:=ScreenToClient(Mouse.CursorPos);
     SendMessage(Handle,WM_MOUSEMOVE,0,P.X or (P.Y shl 16));
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomButton.DoClick;
var PT:TPoint;
begin
 if Visible and Enabled and FDown then
  begin
   if FDropDown and Assigned(FDropDownMenu) then
    begin
     PT:=ClientToScreen(Point(0,Height));
     Application.CancelHint;
     FDropDownMenu.Popup(PT.X,PT.Y);
     FDown:=False;
     ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                        brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
     if FCanBeChecked then
      Checked:=not Checked;
    end else
     begin
      StartGlyphChanging(gcaUp);
      FDown:=False;
      ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                         brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
      if FCanBeChecked then
       Checked:=not Checked;
      if Assigned(FOnClick) then
       FOnClick(Self);
     end;
  end else FDown:=False;
end;

procedure TSXSkinCustomButton.Click;
begin
 FDown:=True;
 DoClick;
 inherited;
end;

procedure TSXSkinCustomButton.InvalidateGlyph;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FGlyphRect,False);
end;

procedure TSXSkinCustomButton.DoThreadActions;
begin
 if not (csDestroying in ComponentState) then
  begin
   Inc(FDoneSteps);
   if (FThread<>nil) and not FThread.Suspended and
      (not HasTransformEffect(FLastButtonTransform) or (FDoneSteps>=FLastButtonTransform.StepsNum)) and
      (not HasTransformEffect(FLastGlyphTransform) or (FDoneSteps>=FLastGlyphTransform.StepsNum)) and
      (not HasTransformEffect(FLastDDGlyphTransform) or (FDoneSteps>=FLastDDGlyphTransform.StepsNum)) then
    begin
     FThread.Suspend;
    end;
   if HasTransformEffect(FLastButtonTransform) and (FDoneSteps<=FLastButtonTransform.StepsNum) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
    end else InvalidateGlyph;
   Update;
  end;
end;

procedure TSXSkinCustomButton.CreateThreadIfNeeded;
begin
 if FThread=nil then
  begin
   FThread:=TSXSkinCustomButtonThread.Create;
   FThread.Control:=Self;
  end;
end;

procedure TSXSkinCustomButton.GetCurrentTransformEffect(T:TSXSkinButtonTransform;
           Action:TSXGlyphChangeAction;var Effect:TSXTransformEffectData);
var   A:Integer;
 BStyle:TSXSkinButtonStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
    begin
     BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
     case T of
      btButton:  begin
                  case Action of
                   gcaHighlightIn:  Effect:=BStyle.HInButtonEffect;
                   gcaHighlightOut: Effect:=BStyle.HOutButtonEffect;
                   gcaDown:         Effect:=BStyle.DownButtonEffect;
                   gcaUp:           Effect:=BStyle.UpButtonEffect;
                   gcaCheck:        Effect:=BStyle.CheckButtonEffect;
                   gcaUncheck:      Effect:=BStyle.UncheckButtonEffect;
                   gcaEnable:       Effect:=BStyle.EnableButtonEffect;
                   gcaDisable:      Effect:=BStyle.DisableButtonEffect;
                   gcaFocus:        Effect:=BStyle.FocusButtonEffect;
                   gcaUnfocus:      Effect:=BStyle.UnfocusButtonEffect;
                  end;
                 end;
      btGlyph:   begin
                  case Action of
                   gcaHighlightIn:  Effect:=BStyle.HInGlyphEffect;
                   gcaHighlightOut: Effect:=BStyle.HOutGlyphEffect;
                   gcaDown:         Effect:=BStyle.DownGlyphEffect;
                   gcaUp:           Effect:=BStyle.UpGlyphEffect;
                   gcaCheck:        Effect:=BStyle.CheckGlyphEffect;
                   gcaUncheck:      Effect:=BStyle.UncheckGlyphEffect;
                   gcaEnable:       Effect:=BStyle.EnableGlyphEffect;
                   gcaDisable:      Effect:=BStyle.DisableGlyphEffect;
                   gcaFocus:        Effect:=BStyle.FocusGlyphEffect;
                   gcaUnfocus:      Effect:=BStyle.UnfocusGlyphEffect;
                  end;
                 end;
      btDDGlyph: begin
                  case Action of
                   gcaHighlightIn:  Effect:=BStyle.HInDDGlyphEffect;
                   gcaHighlightOut: Effect:=BStyle.HOutDDGlyphEffect;
                   gcaDown:         Effect:=BStyle.DownDDGlyphEffect;
                   gcaUp:           Effect:=BStyle.UpDDGlyphEffect;
                   gcaCheck:        Effect:=BStyle.CheckDDGlyphEffect;
                   gcaUncheck:      Effect:=BStyle.UncheckDDGlyphEffect;
                   gcaEnable:       Effect:=BStyle.EnableDDGlyphEffect;
                   gcaDisable:      Effect:=BStyle.DisableDDGlyphEffect;
                   gcaFocus:        Effect:=BStyle.FocusDDGlyphEffect;
                   gcaUnfocus:      Effect:=BStyle.UnfocusDDGlyphEffect;
                  end;
                 end;
     end;
    end;
  end;
end;

procedure TSXSkinCustomButton.StartGlyphChanging(T:TSXGlyphChangeAction);
var         B:TBitmap32;
          Rgn:HRGN;
   NeedThread:Boolean;
   BTransform:TSXTransformEffectData;
   GTransform:TSXTransformEffectData;
 DDGTransform:TSXTransformEffectData;
begin
 GetCurrentTransformEffect(btButton,T,BTransform);
 GetCurrentTransformEffect(btGlyph,T,GTransform);
 GetCurrentTransformEffect(btDDGlyph,T,DDGTransform);
 NeedThread:=HasTransformEffect(BTransform) or
             HasTransformEffect(GTransform) or
             HasTransformEffect(DDGTransform);
 if NeedThread then
  begin
   CreateThreadIfNeeded;
   if HasTransformEffect(GTransform) then
    B:=CreateCurrentBlendedGlyph else B:=nil;
   FLastGlyph.Free;
   FLastGlyph:=B;
   //
   if HasTransformEffect(DDGTransform) then
    B:=CreateCurrentBlendedDropDownGlyph else B:=nil;
   FLastDropDownGlyph.Free;
   FLastDropDownGlyph:=B;
   //
   if HasTransformEffect(BTransform) then
    begin
     B:=TBitmap32.Create;
     B.DrawMode:=dmBlend;
     B.CombineMode:=cmMerge;
     B.SetSize(Width,Height);
     B.Clear(0);
     Rgn:=CreateRectRgn(0,0,Width,Height);
     PaintCurrentBlendedBStyle(B,0,0,Rect(0,0,Width,Height),Rgn);
     if not (HasTransformEffect(FLastButtonTransform) and (FDoneSteps<FLastButtonTransform.StepsNum)) and
             BTransform.DrawCaption then
      PaintCurrentOverStyleCaptionToBitmap(B.Handle,B.BoundsRect,Rect(0,0,Width,Height),Rgn,B,0,0);
     DeleteObject(Rgn);
    end else B:=nil;
   FLastButton.Free;
   FLastButton:=B;
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
 FLastButtonTransform:=BTransform;
 FLastGlyphTransform:=GTransform;
 FLastDDGlyphTransform:=DDGTransform;
end;

procedure TSXSkinCustomButton.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   FLastGlyph.Free;
   FLastGlyph:=nil;
   FLastDropDownGlyph.Free;
   FLastDropDownGlyph:=nil;
   FLastButton.Free;
   FLastButton:=nil;
   FTextBitmap.Free;
   FTextBitmap:=nil;
   if (FThread<>nil) and not FThread.Suspended then
    FThread.Suspend;
   ResetButtonParams([brpText,brpGlyph,brpDDGlyph]);
  end;
 inherited;
end;

function TSXSkinCustomButton.GetCaption:TCaption;
begin
 Result:=inherited Caption;
end;

procedure TSXSkinCustomButton.SetCaption(const Value:TCaption);
begin
 if Caption<>Value then
  begin
   inherited Caption:=Value;
   if not (csLoading in ComponentState) then
    ResetButtonParams([brpText]);
  end;
end;

procedure TSXSkinCustomButton.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
 if (ALeft=Left) and (ATop=Top) and (AWidth=Width) and (AHeight=Height) then exit;
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
 if not (csLoading in ComponentState) then
  ResetButtonParams([brpTextIfWordWrap]);
end;

procedure TSXSkinCustomButton.CMFontChanged(var Message:TMessage);
begin
 inherited;
 if not (csLoading in ComponentState) then
  ResetButtonParams([brpTextOnFontChange]);
end;

procedure TSXSkinCustomButton.Loaded;
begin
 inherited;
 ResetButtonParams([brpText,brpGlyph,brpDDGlyph]);
end;

procedure TSXSkinCustomButton.WMSetFocus(var Msg:TWMSetFocus);
begin
 if not (csLoading in ComponentState) then
  StartGlyphChanging(gcaFocus);
 FLastFocused:=True;
 if not (csLoading in ComponentState) then
  ResetButtonParams([brpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomButton.WMKillFocus(var Msg:TWMKillFocus);
begin
 if not (csLoading in ComponentState) then
  StartGlyphChanging(gcaUnfocus);
 FLastFocused:=False;
 if not (csLoading in ComponentState) then
  ResetButtonParams([brpInvalidateOnStyleChange]);
 if FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetButtonParams([brpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomButton.DoKeyDown(var Msg: TMessage);
begin
 inherited;
 if Enabled and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
  end;
end;

procedure TSXSkinCustomButton.DoKeyUp(var Msg: TMessage);
begin
 inherited;
 if Enabled and FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetButtonParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
   if Msg.WParam in [VK_SPACE,VK_RETURN] then
    begin
     Click;
    end;
  end;
end;

procedure TSXSkinCustomButton.GetCurrentBState(var BState:TSXSkinButtonStateParam);
var  A:Integer;
 Style:TSXSkinButtonStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
    begin
     Style:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
     Style.GetCurrentBState(BState,FChecked,Enabled,FMouseOver,FDown,FLastFocused);
    end;
  end else
   begin
    Finalize(BState);
    FillChar(BState,sizeof(BState),0);
   end;
 SetDefaultFontData(BState.FD,Font);
end;

procedure TSXSkinCustomButton.GetCurrentGlyphStyle(var GStyle:String;PGlyphWidth:PInteger=nil;
           PGlyphHeight:PInteger=nil;PUseFilter:PBoolean=nil);
var     A,B:Integer;
    MSStyle:TSXSkinMultiStateStyle;
   MSCStyle:TSXSkinMultiStateCheckStyle;
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
 GStyle:='';
 if PGlyphWidth<>nil then PGlyphWidth^:=FGlyphWidth;
 if PGlyphHeight<>nil then PGlyphHeight^:=FGlyphHeight;
 if PUseFilter<>nil then PUseFilter^:=False;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(FGlyphStyle);
   if A>=0 then
    begin
     if SkinLibrary.Styles[A] is TSXSkinMultiStateStyle then
      begin
       MSStyle:=TSXSkinMultiStateStyle(SkinLibrary.Styles[A]);
       if not Enabled then
        begin
         SetGStyle(MSStyle.RStyle);
         SetGStyle(MSStyle.NStyle);
        end else
       if FDown then
        begin
         SetGStyle(MSStyle.DStyle);
         SetGStyle(MSStyle.NStyle);
        end else
       if FMouseOver then
        begin
         SetGStyle(MSStyle.HStyle);
         SetGStyle(MSStyle.NStyle);
        end else SetGStyle(MSStyle.NStyle);
      end else
     if SkinLibrary.Styles[A] is TSXSkinMultiStateCheckStyle then
      begin
       MSCStyle:=TSXSkinMultiStateCheckStyle(SkinLibrary.Styles[A]);
       if FChecked then
        begin
         if not Enabled then
          begin
           SetGStyle(MSCStyle.RCStyle);
           SetGStyle(MSCStyle.RUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
         if FDown then
          begin
           SetGStyle(MSCStyle.DCStyle);
           SetGStyle(MSCStyle.DUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
         if FMouseOver then
          begin
           SetGStyle(MSCStyle.HCStyle);
           SetGStyle(MSCStyle.HUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
           begin
            SetGStyle(MSCStyle.NCStyle);
            SetGStyle(MSCStyle.NUStyle);
           end;
        end else
         begin
          if not Enabled then
           begin
            SetGStyle(MSCStyle.RUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else
          if FDown then
           begin
            SetGStyle(MSCStyle.DUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else
          if FMouseOver then
           begin
            SetGStyle(MSCStyle.HUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else SetGStyle(MSCStyle.NUStyle);
         end;
      end else
       begin
        GStyle:=FGlyphStyle;
        if PUseFilter<>nil then
         PUseFilter^:=True;
       end;  
    end;
   if (GStyle<>'') and ((PGlyphWidth<>nil) or (PGlyphHeight<>nil)) and
      ((PGlyphWidth^=0) and (PGlyphHeight^=0)) then
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
            if (PGlyphWidth<>nil) and (PGlyphWidth^=0) then
             PGlyphWidth^:=Image.Bitmap.Width;
            if (PGlyphHeight<>nil) and (PGlyphHeight^=0) then
             PGlyphHeight^:=Image.Bitmap.Height;
           end;
         end;
      end;
    end;
  end;
end;

procedure TSXSkinCustomButton.GetCurrentDropDownGlyphStyle(var GStyle:String;PGlyphWidth:PInteger=nil;
           PGlyphHeight:PInteger=nil);
var     A,B:Integer;
     BStyle:TSXSkinButtonStyle;
    MSStyle:TSXSkinMultiStateStyle;
   MSCStyle:TSXSkinMultiStateCheckStyle;
 GlyphStyle:TSXSkinGeneralStyle;
      Image:TSXSkinStyleImageElement;

 procedure SetGStyle(const S:String); overload;
 begin
  if GStyle='' then GStyle:=S;
 end;

begin
 GStyle:='';
 if PGlyphWidth<>nil then PGlyphWidth^:=FGlyphWidth;
 if PGlyphHeight<>nil then PGlyphHeight^:=FGlyphHeight;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
    begin
     BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
     if PGlyphWidth<>nil then PGlyphWidth^:=BStyle.DropDownGlyphWidth;
     if PGlyphHeight<>nil then PGlyphHeight^:=BStyle.DropDownGlyphHeight;
    end;
   A:=SkinLibrary.Styles.GetIndexByName(FDropDownGlyphStyle);
   if A>=0 then
    begin
     if SkinLibrary.Styles[A] is TSXSkinMultiStateStyle then
      begin
       MSStyle:=TSXSkinMultiStateStyle(SkinLibrary.Styles[A]);
       if not Enabled then
        begin
         SetGStyle(MSStyle.RStyle);
         SetGStyle(MSStyle.NStyle);
        end else
       if FDown then
        begin
         SetGStyle(MSStyle.DStyle);
         SetGStyle(MSStyle.NStyle);
        end else
       if FMouseOver then
        begin
         SetGStyle(MSStyle.HStyle);
         SetGStyle(MSStyle.NStyle);
        end else SetGStyle(MSStyle.NStyle);
      end else
     if SkinLibrary.Styles[A] is TSXSkinMultiStateCheckStyle then
      begin
       MSCStyle:=TSXSkinMultiStateCheckStyle(SkinLibrary.Styles[A]);
       if FChecked then
        begin
         if not Enabled then
          begin
           SetGStyle(MSCStyle.RCStyle);
           SetGStyle(MSCStyle.RUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
         if FDown then
          begin
           SetGStyle(MSCStyle.DCStyle);
           SetGStyle(MSCStyle.DUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
         if FMouseOver then
          begin
           SetGStyle(MSCStyle.HCStyle);
           SetGStyle(MSCStyle.HUStyle);
           SetGStyle(MSCStyle.NCStyle);
           SetGStyle(MSCStyle.NUStyle);
          end else
           begin
            SetGStyle(MSCStyle.NCStyle);
            SetGStyle(MSCStyle.NUStyle);
           end;
        end else
         begin
          if not Enabled then
           begin
            SetGStyle(MSCStyle.RUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else
          if FDown then
           begin
            SetGStyle(MSCStyle.DUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else
          if FMouseOver then
           begin
            SetGStyle(MSCStyle.HUStyle);
            SetGStyle(MSCStyle.NUStyle);
           end else SetGStyle(MSCStyle.NUStyle);
         end;
      end else GStyle:=FDropDownGlyphStyle;
    end;
   if (GStyle<>'') and ((PGlyphWidth<>nil) or (PGlyphHeight<>nil)) and
      ((PGlyphWidth^=0) and (PGlyphHeight^=0)) then
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
            if (PGlyphWidth<>nil) and (PGlyphWidth^=0) then
             PGlyphWidth^:=Image.Bitmap.Width;
            if (PGlyphHeight<>nil) and (PGlyphHeight^=0) then
             PGlyphHeight^:=Image.Bitmap.Height;
           end;
         end;
      end;
    end;
  end;
end;

procedure TSXSkinCustomButton.ResetButtonParams(Params:TSXSkinButtonResetParams=[]);
var OldGlyphRect:TRect;
     OldTextRect:TRect;
  OldDDGlyphRect:TRect;
    GlyphChanged:Boolean;
     TextChanged:Boolean;
  DDGlyphChanged:Boolean;
          GStyle:String;
       A,W,H,X,Y:Integer;
          BState:TSXSkinButtonStateParam;
       SetBState:Boolean;
          BStyle:TSXSkinButtonStyle;
           Flags:Cardinal;
     CaptionRect:TRect;

 procedure DoSetBState;
 begin
  if not SetBState then
   GetCurrentBState(BState);
 end;

begin
 if Parent=nil then exit;
 OldGlyphRect:=FGlyphRect;
 OldTextRect:=FTextRect;
 OldDDGlyphRect:=FDropDownGlyphRect;
 GlyphChanged:=False;
 TextChanged:=False;
 DDGlyphChanged:=False;
 SetBState:=False;
 if Params*[brpGlyph,brpGlyphOnStyleChange,brpGlyphOnSizeChange]<>[] then
  begin
   DoSetBState;
   GetCurrentGlyphStyle(GStyle,@W,@H);
   if brpGlyph in Params then GlyphChanged:=True else
    begin
     if brpGlyphOnSizeChange in Params then
      GlyphChanged:=(W<>FGlyphRect.Right-FGlyphRect.Left) or
                    (H<>FGlyphRect.Bottom-FGlyphRect.Top);
     if not GlyphChanged and (brpGlyphOnStyleChange in Params) then
      GlyphChanged:=(FLastGlyphStyle<>GStyle) or not SameFilterData(FLastGlyphFilterData,BState.DefGlyphFilter);
    end;
   if GlyphChanged then FGlyphRect:=Rect(0,0,W,H);
   FLastGlyphStyle:=GStyle;
   FLastGlyphFilterData:=BState.DefGlyphFilter;
  end;
 if Params*[brpDDGlyph,brpDDGlyphOnStyleChange]<>[] then
  begin
   GetCurrentDropDownGlyphStyle(GStyle,@W,@H);
   if brpDDGlyph in Params then DDGlyphChanged:=True else
    begin
     if brpDDGlyphOnStyleChange in Params then
      DDGlyphChanged:=FLastDropDownGlyphStyle<>GStyle;
    end;
   if DDGlyphChanged then FDropDownGlyphRect:=Rect(0,0,W,H);
   FLastDropDownGlyphStyle:=GStyle;
  end;
 if Params*[brpText,brpTextOnFontChange,brpTextIfWordWrap]<>[] then
  begin
   DoSetBState;
   if brpText in Params then TextChanged:=True else
    if FWordWrap and (brpTextIfWordWrap in Params) then TextChanged:=True else
    if brpTextOnFontChange in Params then
     TextChanged:=not SameFontData(BState.FD,FLastFontData);
   if TextChanged then
    begin
     if FTextBitmap<>nil then
      begin
       FTextBitmap.Free;
       FTextBitmap:=nil;
      end;
     FLastFontData:=BState.FD;
     Canvas.Font.Name:=BState.FD.FontName;
     Canvas.Font.Size:=BState.FD.FontSize;
     Canvas.Font.Style:=BState.FD.FontStyle;
     FTextRect:=ClientRect;
     if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
      begin
       A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
       if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
        begin
         BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
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
                          Dec(FTextRect.Right,FCaptionLeftOffset);
                          Dec(FTextRect.Right,FCaptionRightOffset);
                          Dec(FTextRect.Right,BState.CaptionLeftOffset);
                          Dec(FTextRect.Right,BState.CaptionRightOffset);
                          if FDropDown then
                           begin
                            Dec(FTextRect.Right,BStyle.DropDownGlyphOffset);
                            Dec(FTextRect.Right,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left);
                           end;
                          //
                          Dec(FTextRect.Bottom,FCaptionTopOffset);
                          Dec(FTextRect.Bottom,FCaptionBottomOffset);
                          Dec(FTextRect.Bottom,BState.CaptionTopOffset);
                          Dec(FTextRect.Bottom,BState.CaptionBottomOffset);
                         end;
          gpTop,
          gpBottom:      begin
                          Dec(FTextRect.Right,FCaptionLeftOffset);
                          Dec(FTextRect.Right,FCaptionRightOffset);
                          Dec(FTextRect.Right,BState.CaptionLeftOffset);
                          Dec(FTextRect.Right,BState.CaptionRightOffset);
                          if FDropDown then
                           begin
                            Dec(FTextRect.Right,BStyle.DropDownGlyphOffset);
                            Dec(FTextRect.Right,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left);
                           end;
                          //
                          Dec(FTextRect.Right,FGlyphRect.Bottom-FGlyphRect.Top);
                          if (FLastGlyphStyle<>'') and (Caption<>'') then
                           Dec(FTextRect.Right,FTextOffset);
                          Dec(FTextRect.Bottom,FCaptionTopOffset);
                          Dec(FTextRect.Bottom,FCaptionBottomOffset);
                          Dec(FTextRect.Bottom,BState.CaptionTopOffset);
                          Dec(FTextRect.Bottom,BState.CaptionBottomOffset);
                         end;
         end;
         Flags:=DT_NOPREFIX or DT_NOCLIP or DT_TOP or DT_CALCRECT;
         case FAlignment of
          taLeftJustify:  Flags:=Flags or DT_LEFT;
          taRightJustify: Flags:=Flags or DT_RIGHT;
          taCenter:       Flags:=Flags or DT_CENTER;
         end;
         if FWordWrap then Flags:=Flags or DT_WORDBREAK;
         if BState.FD.SmoothLevel=0 then
          DrawText(Canvas.Handle,PChar(Caption),-1,FTextRect,Flags) else
           DrawSmoothText(Canvas,Caption,FTextRect,Flags,BState.FD.SmoothLevel);
        end;
      end;
    end;
   FLastFontData:=BState.FD; 
  end;
 //
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
    begin
     DoSetBState;
     BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
     CaptionRect:=Rect(0,0,0,0);
     //Setting CaptionRect.Right and CaptionRect.Bottom
     case FGlyphPosition of
      gpLeftTop,
      gpLeft,
      gpLeftBottom,
      gpRightTop,
      gpRight,
      gpRightBottom: begin
                      Inc(CaptionRect.Right,FGlyphRect.Right-FGlyphRect.Left);
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(CaptionRect.Right,FTextOffset);
                      Inc(CaptionRect.Right,FTextRect.Right-FTextRect.Left);
                      if FDropDown then
                       begin
                        Inc(CaptionRect.Right,BStyle.DropDownGlyphOffset);
                        Inc(CaptionRect.Right,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left);
                       end;
                      //
                      CaptionRect.Bottom:=Max(FGlyphRect.Bottom-FGlyphRect.Top,
                                              FTextRect.Bottom-FTextRect.Top);
                      if FDropDown then
                       CaptionRect.Bottom:=Max(CaptionRect.Bottom,
                            FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
                     end;
      gpTop,
      gpBottom:      begin
                      CaptionRect.Right:=Max(FGlyphRect.Right-FGlyphRect.Left,
                                             FTextRect.Right-FTextRect.Left);
                      if FDropDown then
                       begin
                        Inc(CaptionRect.Right,BStyle.DropDownGlyphOffset);
                        Inc(CaptionRect.Right,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left);
                       end;
                      //
                      Inc(CaptionRect.Bottom,FGlyphRect.Bottom-FGlyphRect.Top);
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(CaptionRect.Bottom,FTextOffset);
                      Inc(CaptionRect.Bottom,FTextRect.Bottom-FTextRect.Top);
                      if FDropDown then
                       CaptionRect.Bottom:=Max(CaptionRect.Bottom,
                            FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
                     end;
     end;
     //Offset CaptionRect to Button Center with Offsets
     W:=(Width+FCaptionLeftOffset-FCaptionRightOffset+BState.CaptionLeftOffset-
        BState.CaptionRightOffset-CaptionRect.Right) div 2;
     H:=(Height+FCaptionTopOffset-FCaptionBottomOffset+BState.CaptionTopOffset-
        BState.CaptionBottomOffset-CaptionRect.Bottom) div 2;
     OffsetRect(CaptionRect,W,H);
     //Setting FGlyphRect, FTextRect and FDropDownGlyphRect
     OffsetRect(FGlyphRect,-FGlyphRect.Left,-FGlyphRect.Top);
     OffsetRect(FTextRect,-FTextRect.Left,-FTextRect.Top);
     OffsetRect(FDropDownGlyphRect,-FDropDownGlyphRect.Left,-FDropDownGlyphRect.Top);
     case FGlyphPosition of
      gpLeftTop:     begin
                      OffsetRect(FGlyphRect,CaptionRect.Left,CaptionRect.Top);
                      //
                      X:=FGlyphRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,X,Y);
                     end;
      gpTop:         begin
                      X:=(CaptionRect.Left+CaptionRect.Right-FGlyphRect.Right) div 2;
                      OffsetRect(FGlyphRect,X,CaptionRect.Top);
                      //
                      X:=(CaptionRect.Left+CaptionRect.Right-FTextRect.Right) div 2;
                      Y:=FGlyphRect.Bottom;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(Y,FTextOffset);
                      OffsetRect(FTextRect,X,Y);
                     end;
      gpRightTop:    begin
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,CaptionRect.Left,Y);
                      //
                      X:=FTextRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      OffsetRect(FGlyphRect,X,CaptionRect.Top);
                     end;
      gpRight:       begin
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,CaptionRect.Left,Y);
                      //
                      X:=FTextRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FGlyphRect.Bottom) div 2;
                      OffsetRect(FGlyphRect,X,Y);
                     end;
      gpRightBottom: begin
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,CaptionRect.Left,Y);
                      //
                      X:=FTextRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      Y:=CaptionRect.Bottom-FGlyphRect.Bottom;
                      OffsetRect(FGlyphRect,X,Y);
                     end;
      gpBottom:      begin
                      X:=(CaptionRect.Left+CaptionRect.Right-FTextRect.Right) div 2;
                      OffsetRect(FTextRect,X,CaptionRect.Top);
                      //
                      X:=(CaptionRect.Left+CaptionRect.Right-FGlyphRect.Right) div 2;
                      Y:=FTextRect.Bottom;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(Y,FTextOffset);
                      OffsetRect(FGlyphRect,X,Y);
                     end;
      gpLeftBottom:  begin
                      Y:=CaptionRect.Bottom-FGlyphRect.Bottom;
                      OffsetRect(FGlyphRect,CaptionRect.Left,Y);
                      //
                      X:=FGlyphRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,X,Y);
                     end;
      gpLeft:        begin
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FGlyphRect.Bottom) div 2;
                      OffsetRect(FGlyphRect,CaptionRect.Left,Y);
                      //
                      X:=FGlyphRect.Right;
                      if (FLastGlyphStyle<>'') and (Caption<>'') then
                       Inc(X,FTextOffset);
                      Y:=(CaptionRect.Top+CaptionRect.Bottom-FTextRect.Bottom) div 2;
                      OffsetRect(FTextRect,X,Y);
                     end;
     end;
     if FDropDown then
      begin
       X:=CaptionRect.Right-FDropDownGlyphRect.Right;
       Y:=(CaptionRect.Top+CaptionRect.Bottom-FDropDownGlyphRect.Bottom) div 2;
       OffsetRect(FDropDownGlyphRect,X,Y);
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
 if FDropDown and (DDGlyphChanged or not EqualRect(OldDDGlyphRect,FDropDownGlyphRect)) then
  begin
   if HandleAllocated then
    begin
     InvalidateRect(Handle,@OldDDGlyphRect,False);
     InvalidateRect(Handle,@FDropDownGlyphRect,False);
    end; 
  end;
 //
 if brpInvalidateOnStyleChange in Params then
  begin
   DoSetBState;
   if (BState.Style<>FLastStyle) or (BState.OverStyle<>FLastOverStyle) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
     FLastStyle:=BState.Style;
     FLastOverStyle:=BState.OverStyle;
    end;
  end;
end;

function TSXSkinCustomButton.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 BState:TSXSkinButtonStateParam;
 GStyle:String;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentBState(BState);
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
    end;
   if Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
      end;
    end;
   if Result then
    begin
     GetCurrentGlyphStyle(GStyle);
     A:=SkinLibrary.Styles.GetGStyleIndex(GStyle,FGlyphRect.Right-FGlyphRect.Left,
                                          FGlyphRect.Bottom-FGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Glyph,X-FGlyphRect.Left,Y-FGlyphRect.Top,
               FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top,
               SkinLibrary,Limit);
      end;
    end;
   if Result and FDropDown then
    begin
     GetCurrentDropDownGlyphStyle(GStyle);
     A:=SkinLibrary.Styles.GetGStyleIndex(GStyle,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
                                          FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_DDGlyph,X-FDropDownGlyphRect.Left,Y-FDropDownGlyphRect.Top,
               FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
               FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top,SkinLibrary,
               Limit);
      end;
    end;
  end;
end;

function TSXSkinCustomButton.CapturesMouseAt(X,Y:Integer):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 BState:TSXSkinButtonStateParam;
 GStyle:String;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentBState(BState);
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
    end;
   if not Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
      end;
    end;
   if not Result then
    begin
     GetCurrentGlyphStyle(GStyle);
     A:=SkinLibrary.Styles.GetGStyleIndex(GStyle,FGlyphRect.Right-FGlyphRect.Left,
                                          FGlyphRect.Bottom-FGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Glyph,X-FGlyphRect.Left,Y-FGlyphRect.Top,
               FGlyphRect.Right-FGlyphRect.Left,FGlyphRect.Bottom-FGlyphRect.Top,
               SkinLibrary);
      end;
    end;
   if not Result and FDropDown then
    begin
     GetCurrentDropDownGlyphStyle(GStyle);
     A:=SkinLibrary.Styles.GetGStyleIndex(GStyle,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
                                          FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_DDGlyph,X-FDropDownGlyphRect.Left,
               Y-FDropDownGlyphRect.Top,FDropDownGlyphRect.Right-FDropDownGlyphRect.Left,
               FDropDownGlyphRect.Bottom-FDropDownGlyphRect.Top,SkinLibrary);
      end;
    end;
  end;
end;

constructor TSXSkinCustomButton.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 CEID_Back:=GetNewCElementID;
 CEID_Glyph:=GetNewCElementID;
 CEID_DDGlyph:=GetNewCElementID;
 VComparer:=TSXButtonVariableComparer.Create;
 VComparer.Control:=Self;
 VComparer.OnGetVariable:=OnGetVariable;
 ControlStyle:=ControlStyle+[csSetCaption]-[csDoubleClicks];
 FTextOffset:=5;
 FGlyphPosition:=gpLeft;
 SkinStyle:='_Button';
 FDropDownGlyphStyle:='_MultiState_DropDown';
 TabStop:=True;
end;

destructor TSXSkinCustomButton.Destroy;
begin
 FThread.Free;
 FTextBitmap.Free;
 FLastGlyph.Free;
 FLastDropDownGlyph.Free;
 FLastButton.Free;
 inherited;
 VComparer.Free; 
end;

end.
