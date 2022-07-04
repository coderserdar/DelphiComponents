unit SXSkinUpDown;

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
     Messages, Controls, GR32_Blend, SXSkinLibrary, Types, Graphics, Menus;

const

 VARUD_W  =  1;
 VARUD_H  =  2;
 VARUD_UL =  3;
 VARUD_UT =  4;
 VARUD_UR =  5;
 VARUD_UB =  6;
 VARUD_DL =  7;
 VARUD_DT =  8;
 VARUD_DR =  9;
 VARUD_DB = 10;

type

 TSXSkinCustomUpDown=class;

 TSXSkinCustomUpDownThread=class(TThread)
  public
   Control:TSXSkinCustomUpDown;
   constructor Create;
   procedure Execute; override;
   procedure DoEvent;
 end;

 TSXSkinUpDownResetParam=(udrpButtonRects,
                          udrpInvalidateOnStyleChange,
                          udrpInvalidateUpButtonOnStyleChange,
                          udrpInvalidateDownButtonOnStyleChange);

 TSXSkinUpDownResetParams=set of TSXSkinUpDownResetParam;

 TSXUpDownNotifyEvent=procedure(Sender:TObject;UpButton:Boolean) of object;

 TSXUpDownVariableComparer=class(TSXVariableComparer)
  private
   Control:TSXSkinCustomUpDown;
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

 TSXSkinCustomUpDown=class(TSXSkinCustomControl)
  private
   FOnClick:TSXUpDownNotifyEvent;
   FHorizontal:Boolean;
   FMouseOver:Boolean;
   FMouseOverUp:Boolean;
   FMouseOverDown:Boolean;
   FDownUp:Boolean;
   FDownDown:Boolean;
   FThread:TSXSkinCustomUpDownThread;
   FEnabledUp:Boolean;
   FEnabledDown:Boolean;
   //
   FLastUpDownTransform:TSXTransformEffectData;
   FUpDownDoneSteps:Integer;
   FLastUpDown:TBitmap32;
   //
   FLastUpButtonTransform:TSXTransformEffectData;
   FUpButtonDoneSteps:Integer;
   FLastUpButton:TBitmap32;
   //
   FLastDownButtonTransform:TSXTransformEffectData;
   FDownButtonDoneSteps:Integer;
   FLastDownButton:TBitmap32;
   //
   FUpButtonRect:TRect;
   FDownButtonRect:TRect;
   FLastStyle:String;
   FLastOverStyle:String;
   FLastUpButtonStyle:String;
   FLastUpButtonOverStyle:String;
   FLastDownButtonStyle:String;
   FLastDownButtonOverStyle:String;
   FLastFocused:Boolean;
   SeqClickNum:Integer;
   CEID_Back:Integer;
   CEID_Up:Integer;
   CEID_Down:Integer;
   VComparer:TSXUpDownVariableComparer;
   function HasUnusualSkinStyle:Boolean;
   function OnGetVariable(const VarName:String;var Error:Boolean):Single;
   procedure SetEnabledUp(Value:Boolean);
   procedure SetEnabledDown(Value:Boolean);
   procedure SetHorizontal(Value:Boolean);
   procedure DoThreadActions;
   procedure CreateThreadIfNeeded;
   procedure ResetUpDownParams(Params:TSXSkinUpDownResetParams=[]);
   procedure InternalMouseEnter;
   procedure InternalMouseLeave;
   procedure InternalMouseEnterUpButton;
   procedure InternalMouseLeaveUpButton;
   procedure InternalMouseEnterDownButton;
   procedure InternalMouseLeaveDownButton;
   procedure StartChangingEffect(T:TSXGlyphChangeAction);
   procedure StartUpButtonChangingEffect(T:TSXGlyphChangeAction);
   procedure StartDownButtonChangingEffect(T:TSXGlyphChangeAction);
   procedure GetUpDownTransformEffect(Action:TSXGlyphChangeAction;
              var Effect:TSXTransformEffectData);
   procedure GetUpButtonTransformEffect(Action:TSXGlyphChangeAction;
              var Effect:TSXTransformEffectData);
   procedure GetDownButtonTransformEffect(Action:TSXGlyphChangeAction;
              var Effect:TSXTransformEffectData);
   procedure GetCurrentUDState(var UDState:TSXSkinUpDownStateParam);
   procedure GetCurrentUpButtonState(var BState:TSXSkinButtonStateParam);
   procedure GetCurrentDownButtonState(var BState:TSXSkinButtonStateParam);
   procedure WndProc(var Msg:TMessage); override;
  protected
   function UpButtonCapturesMouseAt(X,Y:Integer):Boolean;
   function DownButtonCapturesMouseAt(X,Y:Integer):Boolean;   
   function CapturesMouseAt(X,Y:Integer):Boolean; override;
   procedure SetEnabled(Value:Boolean); override;
   procedure Loaded; override;
   procedure Notification(AComponent:TComponent;Operation:TOperation); override;
   procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
   procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
   procedure MouseLeave; override;
   procedure InvalidateUpButton;
   procedure InvalidateDownButton;
   procedure DoClick(UpButton:Boolean);
   procedure DoKeyDown(var Msg:TMessage); message CN_KEYDOWN;
   procedure DoKeyUp(var Msg:TMessage); message CN_KEYUP;
   procedure WMSetFocus(var Msg:TWMSetFocus); message WM_SETFOCUS;
   procedure WMKillFocus(var Msg:TWMKillFocus); message WM_KILLFOCUS;
   procedure PaintUpButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintDownButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintBlendedUpButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintBlendedDownButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintOverStyleButtonsToBitmap(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentUDStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   procedure PaintCurrentBlendedUDStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
   property SkinStyle stored HasUnusualSkinStyle;
  public
   function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; override;
   procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
              Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
              WithSubItems:Boolean); override;
   procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
   procedure SkinChanged; override;
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
  published
   property EnabledDown:Boolean read FEnabledDown write SetEnabledDown default True;
   property EnabledUp:Boolean read FEnabledUp write SetEnabledUp default True;
   property Horizontal:Boolean read FHorizontal write SetHorizontal default False;
   property TabStop default True;
   property OnClick:TSXUpDownNotifyEvent read FOnClick write FOnClick;
   property OnContextPopup;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnMouseWheel;
   property OnMouseWheelDown;
   property OnMouseWheelUp;
   property OnResize;
   property OnStartDock;
   property OnStartDrag;
 end;

 TSXSkinUpDown=class(TSXSkinCustomUpDown)
  published
   property Align;
   property Anchors;
   property Constraints;
   property Cursor;
   property DragCursor;
   property Enabled;
   property EnabledDown;
   property EnabledUp;
   //property HintData;
   property Horizontal;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property SkinLibrary;
   property SkinStyle;
   property TabOrder;
   property TabStop default True;
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

uses Math, SXBitmap32Utils;

{ TSXSkinCustomUpDownThread }

constructor TSXSkinCustomUpDownThread.Create;
begin
 inherited Create(True);
 FreeOnTerminate:=False;
end;

procedure TSXSkinCustomUpDownThread.Execute;
begin
 while not Terminated do
  begin
   SleepEx(30,True);
   if not Suspended then
    Synchronize(DoEvent);
  end;
end;

procedure TSXSkinCustomUpDownThread.DoEvent;
begin
 if Assigned(Control) then
  Control.DoThreadActions;
end;

{ TSXUpDownVariableComparer }

function TSXUpDownVariableComparer.VarListOnGetVariable(const VarName:String;var Error:Boolean):Single;
var CurVarVal:Integer;
begin
 Result:=1234;
 CurVarVal:=-1;
 if VarName='W' then
  CurVarVal:=VARUD_W else
 if VarName='H' then
  CurVarVal:=VARUD_H else
 if VarName='UL' then
  CurVarVal:=VARUD_UL else
 if VarName='UT' then
  CurVarVal:=VARUD_UT else
 if VarName='UR' then
  CurVarVal:=VARUD_UR else
 if VarName='UB' then
  CurVarVal:=VARUD_UB else
 if VarName='DL' then
  CurVarVal:=VARUD_DL else
 if VarName='DT' then
  CurVarVal:=VARUD_DT else
 if VarName='DR' then
  CurVarVal:=VARUD_DR else
 if VarName='DB' then
  CurVarVal:=VARUD_DB;
 if CurVarVal>=0 then
  begin
   if CurValList=nil then
    CurValList:=TList.Create;
   CurValList.Add(Pointer(CurVarVal));
  end;
end;

function TSXUpDownVariableComparer.GetValue(VarID:Integer):Integer;
begin
 Result:=0;
 if Control<>nil then
  begin
   case VarID of
    VARUD_W:  Result:=Control.Width;
    VARUD_H:  Result:=Control.Height;
    VARUD_UL: Result:=Control.FUpButtonRect.Left;
    VARUD_UT: Result:=Control.FUpButtonRect.Top;
    VARUD_UR: Result:=Control.FUpButtonRect.Right;
    VARUD_UB: Result:=Control.FUpButtonRect.Bottom;
    VARUD_DL: Result:=Control.FDownButtonRect.Left;
    VARUD_DT: Result:=Control.FDownButtonRect.Top;
    VARUD_DR: Result:=Control.FDownButtonRect.Right;
    VARUD_DB: Result:=Control.FDownButtonRect.Bottom;
   end;
  end;
end;

function TSXUpDownVariableComparer.GetVarValsForVarList(VarList:TList):TList;
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

function TSXUpDownVariableComparer.Changed(VarList:TList;OldVarVals:TList):Boolean;
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

procedure TSXUpDownVariableComparer.Update(VarList:TList;VarVals:TList);
var A:Integer;
begin
 if VarList=nil then exit;
 for A:=0 to VarList.Count-1 do
  VarVals[A]:=Pointer(GetValue(Integer(VarList[A])));
end;

procedure TSXUpDownVariableComparer.DestroyVarList(VarList:TList);
begin
 VarList.Free;
end;

procedure TSXUpDownVariableComparer.DestroyVarVals(VarList:TList;VarVals:TList);
begin
 VarVals.Free;
end;

{ TSXSkinCustomUpDown }

function TSXSkinCustomUpDown.HasUnusualSkinStyle:Boolean;
begin
 Result:=SkinStyle<>'_UpDown';
end;

procedure TSXSkinCustomUpDown.SetEnabled(Value:Boolean);
begin
 if Enabled<>Value then
  begin
   if not (csLoading in ComponentState) then
    begin
     if Enabled then
      begin
       StartChangingEffect(gcaDisable);
       if FEnabledUp then
        StartUpButtonChangingEffect(gcaDisable);
       if FEnabledDown then
        StartDownButtonChangingEffect(gcaDisable);
      end else
       begin
        StartChangingEffect(gcaEnable);
        if FEnabledUp then
         StartUpButtonChangingEffect(gcaEnable);
        if FEnabledDown then
         StartDownButtonChangingEffect(gcaEnable);
       end;
    end;
   inherited;
   if not Enabled then
    begin
     FMouseOver:=False;
     FMouseOverUp:=False;
     FMouseOverDown:=False;
     FDownUp:=False;
     FDownDown:=False;
    end;
   if not (csLoading in ComponentState) then
    ResetUpDownParams([udrpInvalidateOnStyleChange,udrpInvalidateUpButtonOnStyleChange,
                       udrpInvalidateDownButtonOnStyleChange]);
  end;
end;

procedure TSXSkinCustomUpDown.SetEnabledUp(Value:Boolean);
begin
 if Value<>FEnabledUp then
  begin
   FEnabledUp:=Value;
   if not (csLoading in ComponentState) then
    begin
     if FEnabledUp then
      StartUpButtonChangingEffect(gcaEnable) else
       StartUpButtonChangingEffect(gcaDisable);
     ResetUpDownParams([udrpInvalidateUpButtonOnStyleChange]);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.SetEnabledDown(Value:Boolean);
begin
 if Value<>FEnabledDown then
  begin
   FEnabledDown:=Value;
   if not (csLoading in ComponentState) then
    begin
     if FEnabledDown then
      StartDownButtonChangingEffect(gcaEnable) else
       StartDownButtonChangingEffect(gcaDisable);
     ResetUpDownParams([udrpInvalidateDownButtonOnStyleChange]);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.SetHorizontal(Value:Boolean);
begin
 if Value<>FHorizontal then
  begin
   FHorizontal:=Value;
   if not (csLoading in ComponentState) and (csDesigning in ComponentState) then
    begin
     if FHorizontal and (SkinStyle='_UpDown') then
      SkinStyle:='_UpDownH' else
     if not FHorizontal and (SkinStyle='_UpDownH') then
      SkinStyle:='_UpDown';
    end;
  end;
end;

procedure TSXSkinCustomUpDown.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
end;

function TSXSkinCustomUpDown.OnGetVariable(const VarName:String;var Error:Boolean):Single;
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
 if VarName='UL' then
  begin
   Result:=FUpButtonRect.Left; exit;
  end;
 if VarName='UT' then
  begin
   Result:=FUpButtonRect.Top; exit;
  end;
 if VarName='UR' then
  begin
   Result:=FUpButtonRect.Right; exit;
  end;
 if VarName='UB' then
  begin
   Result:=FUpButtonRect.Bottom; exit;
  end;
 if VarName='DL' then
  begin
   Result:=FDownButtonRect.Left; exit;
  end;
 if VarName='DT' then
  begin
   Result:=FDownButtonRect.Top; exit;
  end;
 if VarName='DR' then
  begin
   Result:=FDownButtonRect.Right; exit;
  end;
 if VarName='DB' then
  begin
   Result:=FDownButtonRect.Bottom; exit;
  end;
 Error:=True;
end;

procedure TSXSkinCustomUpDown.GetCurrentUDState(var UDState:TSXSkinUpDownStateParam);
var  A:Integer;
 Style:TSXSkinUpDownStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     Style.GetCurrentUDState(UDState,Enabled,FMouseOver,FDownUp or FDownDown,FLastFocused);
    end;
  end else
   begin
    Finalize(UDState);
    FillChar(UDState,sizeof(UDState),0);
   end;
end;

procedure TSXSkinCustomUpDown.GetCurrentUpButtonState(var BState:TSXSkinButtonStateParam);
var   A:Integer;
  Style:TSXSkinUpDownStyle;
 BStyle:TSXSkinButtonStyle;
begin
 Finalize(BState);
 FillChar(BState,sizeof(BState),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     A:=SkinLibrary.Styles.GetIndexByName(Style.UpButton);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
      begin
       BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
       BStyle.GetCurrentBState(BState,False,Enabled and FEnabledUp,FMouseOverUp,
                                      FDownUp,False);
      end;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.GetCurrentDownButtonState(var BState:TSXSkinButtonStateParam);
var   A:Integer;
  Style:TSXSkinUpDownStyle;
 BStyle:TSXSkinButtonStyle;
begin
 Finalize(BState);
 FillChar(BState,sizeof(BState),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     A:=SkinLibrary.Styles.GetIndexByName(Style.DownButton);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
      begin
       BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
       BStyle.GetCurrentBState(BState,False,Enabled and FEnabledDown,FMouseOverDown,
                                      FDownDown,False);
      end;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.PaintUpButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
  BState:TSXSkinButtonStateParam;
   Style:TSXSkinGeneralStyle;
  BWidth:Integer;
 BHeight:Integer;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentUpButtonState(BState);
   BWidth:=FUpButtonRect.Right-FUpButtonRect.Left;
   BHeight:=FUpButtonRect.Bottom-FUpButtonRect.Top;
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,BWidth,BHeight);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Up,Bitmap,X,Y,BWidth,BHeight,Rect,Rgn,SkinLibrary);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.PaintDownButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
  BState:TSXSkinButtonStateParam;
   Style:TSXSkinGeneralStyle;
  BWidth:Integer;
 BHeight:Integer;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentDownButtonState(BState);
   BWidth:=FDownButtonRect.Right-FDownButtonRect.Left;
   BHeight:=FDownButtonRect.Bottom-FDownButtonRect.Top;
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,BWidth,BHeight);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Down,Bitmap,X,Y,BWidth,BHeight,Rect,Rgn,SkinLibrary);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.PaintBlendedUpButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var     BB:TBitmap32;
 CurButton:TBitmap32;
       Rgn2:HRGN;
     BWidth:Integer;
    BHeight:Integer;
begin
 if HasTransformEffect(FLastUpButtonTransform) and
    (FUpButtonDoneSteps<FLastUpButtonTransform.StepsNum) and (FLastUpButton<>nil) then
  begin
   BWidth:=FLastUpButton.Width;
   BHeight:=FLastUpButton.Height;
   CurButton:=TBitmap32.Create;
   BB:=TBitmap32.Create;
   try
    CurButton.DrawMode:=dmBlend;
    CurButton.CombineMode:=cmMerge;
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    CurButton.SetSize(BWidth,BHeight);
    CurButton.Clear(0);
    Rgn2:=CreateRectRgn(0,0,BWidth,BHeight);
    PaintUpButtonStyle(CurButton,0,0,Types.Rect(0,0,BWidth,BHeight),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(BWidth,BHeight);
    ApplyTransformEffectToBitmaps(FLastUpButton,CurButton,FLastUpButtonTransform,
                                  FUpButtonDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurButton.Free;
   end;
  end else PaintUpButtonStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomUpDown.PaintBlendedDownButtonStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var     BB:TBitmap32;
 CurButton:TBitmap32;
       Rgn2:HRGN;
     BWidth:Integer;
    BHeight:Integer;
begin
 if HasTransformEffect(FLastDownButtonTransform) and
    (FDownButtonDoneSteps<FLastDownButtonTransform.StepsNum) and (FLastDownButton<>nil) then
  begin
   BWidth:=FLastDownButton.Width;
   BHeight:=FLastDownButton.Height;
   CurButton:=TBitmap32.Create;
   BB:=TBitmap32.Create;
   try
    CurButton.DrawMode:=dmBlend;
    CurButton.CombineMode:=cmMerge;
    BB.DrawMode:=dmBlend;
    BB.CombineMode:=cmMerge;
    CurButton.SetSize(BWidth,BHeight);
    CurButton.Clear(0);
    Rgn2:=CreateRectRgn(0,0,BWidth,BHeight);
    PaintDownButtonStyle(CurButton,0,0,Types.Rect(0,0,BWidth,BHeight),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(BWidth,BHeight);
    ApplyTransformEffectToBitmaps(FLastDownButton,CurButton,FLastDownButtonTransform,
                                  FDownButtonDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurButton.Free;
   end;
  end else PaintDownButtonStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomUpDown.PaintOverStyleButtonsToBitmap(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
 UDState:TSXSkinUpDownStateParam;
   Style:TSXSkinGeneralStyle;
  BWidth:Integer;
 BHeight:Integer;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   if not IsRectEmpty(FUpButtonRect) then
    begin
     BWidth:=FUpButtonRect.Right-FUpButtonRect.Left;
     BHeight:=FUpButtonRect.Bottom-FUpButtonRect.Top;
     Rgn:=CreateRectRgn(0,0,BWidth,BHeight);
     PaintBlendedUpButtonStyle(Bitmap,X-Rect.Left+FUpButtonRect.Left,
          Y-Rect.Top+FUpButtonRect.Top,Types.Rect(0,0,BWidth,BHeight),Rgn);
     DeleteObject(Rgn);
    end;
   if not IsRectEmpty(FDownButtonRect) then
    begin
     BWidth:=FDownButtonRect.Right-FDownButtonRect.Left;
     BHeight:=FDownButtonRect.Bottom-FDownButtonRect.Top;
     Rgn:=CreateRectRgn(0,0,BWidth,BHeight);
     PaintBlendedDownButtonStyle(Bitmap,X-Rect.Left+FDownButtonRect.Left,
          Y-Rect.Top+FDownButtonRect.Top,Types.Rect(0,0,BWidth,BHeight),Rgn);
     DeleteObject(Rgn);
    end;
   //
   GetCurrentUDState(UDState);
   A:=SkinLibrary.Styles.GetGStyleIndex(UDState.OverStyle,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.PaintCurrentUDStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var    A:Integer;
 UDState:TSXSkinUpDownStateParam;
   Style:TSXSkinGeneralStyle;
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentUDState(UDState);
   A:=SkinLibrary.Styles.GetGStyleIndex(UDState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Style.DrawToBitmap(Self,CEID_Back,Bitmap,X,Y,Width,Height,Rect,Rgn,SkinLibrary,VComparer);
    end;
   //
   if HasTransformEffect(FLastUpDownTransform) and (FUpDownDoneSteps<FLastUpDownTransform.StepsNum) and
      FLastUpDownTransform.DrawCaption then
    begin
     PaintOverStyleButtonsToBitmap(Bitmap,X,Y,Rect,Rgn);
    end;
  end;
end;

procedure TSXSkinCustomUpDown.PaintCurrentBlendedUDStyle(Bitmap:TBitmap32;X,Y:Integer;Rect:TRect;Rgn:HRGN);
var     BB:TBitmap32;
 CurButton:TBitmap32;
       Rgn2:HRGN;
begin
 if HasTransformEffect(FLastUpDownTransform) and
    (FUpDownDoneSteps<FLastUpDownTransform.StepsNum) and (FLastUpDown<>nil) then
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
    PaintCurrentUDStyle(CurButton,0,0,Types.Rect(0,0,Width,Height),Rgn2);
    DeleteObject(Rgn2);
    BB.SetSize(Width,Height);
    ApplyTransformEffectToBitmaps(FLastUpDown,CurButton,FLastUpDownTransform,FUpDownDoneSteps,BB);
    BB.DrawTo(Bitmap,X-Rect.Left,Y-Rect.Top);
   finally
    BB.Free;
    CurButton.Free;
   end;
  end else PaintCurrentUDStyle(Bitmap,X,Y,Rect,Rgn);
end;

procedure TSXSkinCustomUpDown.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
begin
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   PaintCurrentBlendedUDStyle(Bitmap,X,Y,Rect,Rgn);
   if not (HasTransformEffect(FLastUpDownTransform) and (FUpDownDoneSteps<FLastUpDownTransform.StepsNum) and
           FLastUpDownTransform.DrawCaption) then
    PaintOverStyleButtonsToBitmap(Bitmap,X,Y,Rect,Rgn);
  end;
 inherited;
end;

procedure TSXSkinCustomUpDown.InternalMouseEnter;
begin
 if Enabled and not FDownUp and not FDownDown then
  StartChangingEffect(gcaHighlightIn);
 FMouseOver:=True;
 if Enabled and not FDownUp and not FDownDown then
  ResetUpDownParams([udrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.InternalMouseLeave;
begin
 if Enabled and not FDownUp and not FDownDown then
  StartChangingEffect(gcaHighlightOut);
 FMouseOver:=False;
 if FMouseOverUp then
  InternalMouseLeaveUpButton;
 if FMouseOverDown then
  InternalMouseLeaveDownButton;
 if Enabled and not FDownUp and not FDownDown then
  ResetUpDownParams([udrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.InternalMouseEnterUpButton;
begin
 if Enabled and FEnabledUp and not FDownUp then
  StartUpButtonChangingEffect(gcaHighlightIn);
 FMouseOverUp:=True;
 if Enabled and not FDownUp then
  ResetUpDownParams([udrpInvalidateUpButtonOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.InternalMouseLeaveUpButton;
begin
 if Enabled and FEnabledUp and not FDownUp then
  StartUpButtonChangingEffect(gcaHighlightOut);
 FMouseOverUp:=False;
 if Enabled and not FDownUp then
  ResetUpDownParams([udrpInvalidateUpButtonOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.InternalMouseEnterDownButton;
begin
 if Enabled and FEnabledDown and not FDownDown then
  StartDownButtonChangingEffect(gcaHighlightIn);
 FMouseOverDown:=True;
 if Enabled and not FDownDown then
  ResetUpDownParams([udrpInvalidateDownButtonOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.InternalMouseLeaveDownButton;
begin
 if Enabled and FEnabledDown and not FDownDown then
  StartDownButtonChangingEffect(gcaHighlightOut);
 FMouseOverDown:=False;
 if Enabled and not FDownDown then
  ResetUpDownParams([udrpInvalidateDownButtonOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.MouseLeave;
begin
 if FMouseOver then
  InternalMouseLeave;
 inherited;
end;

procedure TSXSkinCustomUpDown.MouseMove(Shift:TShiftState;X,Y:Integer);
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
   if FMouseOver then
    begin
     B:=PtInRect(FUpButtonRect,P) and UpButtonCapturesMouseAt(X,Y);
     if B<>FMouseOverUp then
      begin
       if B then InternalMouseEnterUpButton else
        InternalMouseLeaveUpButton;
      end;
     B:=PtInRect(FDownButtonRect,P) and DownButtonCapturesMouseAt(X,Y);
     if B<>FMouseOverDown then
      begin
       if B then InternalMouseEnterDownButton else
        InternalMouseLeaveDownButton;
      end;
    end;
   if not FMouseOver then
    begin
     if FMouseOverUp then
      InternalMouseLeaveUpButton;
     if FMouseOverDown then
      InternalMouseLeaveDownButton;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var      P:TPoint;
 NeedTimer:Boolean;
begin
 if Enabled and (Button=mbLeft) then
  begin
   NeedTimer:=False;
   if EnabledUp and FMouseOverUp then
    begin
     StartChangingEffect(gcaDown);
     StartUpButtonChangingEffect(gcaDown);
     FDownUp:=True;
     SetFocus;
     ResetUpDownParams([udrpInvalidateOnStyleChange,udrpInvalidateUpButtonOnStyleChange]);
     //
     DoClick(True);
     P:=ScreenToClient(Mouse.CursorPos);
     SendMessage(Handle,WM_MOUSEMOVE,0,P.X or (P.Y shl 16));
     //
     NeedTimer:=True;
    end;
   if EnabledDown and FMouseOverDown then
    begin
     StartChangingEffect(gcaDown);
     StartDownButtonChangingEffect(gcaDown);
     FDownDown:=True;
     SetFocus;
     ResetUpDownParams([udrpInvalidateOnStyleChange,udrpInvalidateDownButtonOnStyleChange]);
     //
     DoClick(False);
     P:=ScreenToClient(Mouse.CursorPos);
     SendMessage(Handle,WM_MOUSEMOVE,0,P.X or (P.Y shl 16));
     //
     NeedTimer:=True;
    end;
   if NeedTimer then
    begin
     SeqClickNum:=0;
     KillTimer(Handle,1);
     SetTimer(Handle,1,500,nil);
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomUpDown.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if Enabled then
  begin
   StartChangingEffect(gcaUp);
   KillTimer(Handle,1);
   if FDownUp then
    begin
     StartUpButtonChangingEffect(gcaUp);
     FDownUp:=False;
     ResetUpDownParams([udrpInvalidateUpButtonOnStyleChange]);
    end;
   if FDownDown then
    begin
     StartDownButtonChangingEffect(gcaUp);
     FDownDown:=False;
     ResetUpDownParams([udrpInvalidateDownButtonOnStyleChange]);
    end;
   ResetUpDownParams([udrpInvalidateOnStyleChange]);
  end;
 inherited;
end;

procedure TSXSkinCustomUpDown.DoClick(UpButton:Boolean);
begin
 if Assigned(FOnClick) then
  FOnClick(Self,UpButton);
end;

procedure TSXSkinCustomUpDown.InvalidateUpButton;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FUpButtonRect,False);
end;

procedure TSXSkinCustomUpDown.InvalidateDownButton;
begin
 if HandleAllocated then
  InvalidateRect(Handle,@FDownButtonRect,False);
end;

procedure TSXSkinCustomUpDown.DoThreadActions;
begin
 if not (csDestroying in ComponentState) then
  begin
   Inc(FUpDownDoneSteps);
   Inc(FUpButtonDoneSteps);
   Inc(FDownButtonDoneSteps);
   if (FThread<>nil) and not FThread.Suspended and
      (not HasTransformEffect(FLastUpDownTransform) or (FUpDownDoneSteps>=FLastUpDownTransform.StepsNum)) and
      (not HasTransformEffect(FLastUpButtonTransform) or (FUpButtonDoneSteps>=FLastUpButtonTransform.StepsNum)) and
      (not HasTransformEffect(FLastDownButtonTransform) or (FDownButtonDoneSteps>=FLastDownButtonTransform.StepsNum)) then
    begin
     FThread.Suspend;
    end;
   if HasTransformEffect(FLastUpDownTransform) and (FUpDownDoneSteps<=FLastUpDownTransform.StepsNum) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
    end else
     begin
      if HasTransformEffect(FLastUpButtonTransform) and (FUpButtonDoneSteps<=FLastUpButtonTransform.StepsNum) then
       InvalidateUpButton;
      if HasTransformEffect(FLastDownButtonTransform) and (FDownButtonDoneSteps<=FLastDownButtonTransform.StepsNum) then
       InvalidateDownButton;
     end;
   Update;
  end;
end;

procedure TSXSkinCustomUpDown.CreateThreadIfNeeded;
begin
 if FThread=nil then
  begin
   FThread:=TSXSkinCustomUpDownThread.Create;
   FThread.Control:=Self;
  end;
end;

procedure TSXSkinCustomUpDown.GetUpDownTransformEffect(Action:TSXGlyphChangeAction;
           var Effect:TSXTransformEffectData);
var  A:Integer;
 Style:TSXSkinUpDownStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     case Action of
      gcaHighlightIn:  Effect:=Style.HInUpDownEffect;
      gcaHighlightOut: Effect:=Style.HOutUpDownEffect;
      gcaDown:         Effect:=Style.DownUpDownEffect;
      gcaUp:           Effect:=Style.UpUpDownEffect;
      gcaEnable:       Effect:=Style.EnableUpDownEffect;
      gcaDisable:      Effect:=Style.DisableUpDownEffect;
      gcaFocus:        Effect:=Style.FocusUpDownEffect;
      gcaUnfocus:      Effect:=Style.UnfocusUpDownEffect;
     end;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.GetUpButtonTransformEffect(Action:TSXGlyphChangeAction;
           var Effect:TSXTransformEffectData);
var   A:Integer;
  Style:TSXSkinUpDownStyle;
 BStyle:TSXSkinButtonStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     A:=SkinLibrary.Styles.GetIndexByName(Style.UpButton);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
      begin
       BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
       case Action of
        gcaHighlightIn:  Effect:=BStyle.HInButtonEffect;
        gcaHighlightOut: Effect:=BStyle.HOutButtonEffect;
        gcaDown:         Effect:=BStyle.DownButtonEffect;
        gcaUp:           Effect:=BStyle.UpButtonEffect;
        gcaEnable:       Effect:=BStyle.EnableButtonEffect;
        gcaDisable:      Effect:=BStyle.DisableButtonEffect;
        gcaFocus:        Effect:=BStyle.FocusButtonEffect;
        gcaUnfocus:      Effect:=BStyle.UnfocusButtonEffect;
       end;
      end;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.GetDownButtonTransformEffect(Action:TSXGlyphChangeAction;
           var Effect:TSXTransformEffectData);
var   A:Integer;
  Style:TSXSkinUpDownStyle;
 BStyle:TSXSkinButtonStyle;
begin
 FillChar(Effect,sizeof(Effect),0);
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
   if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
    begin
     Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
     A:=SkinLibrary.Styles.GetIndexByName(Style.DownButton);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinButtonStyle) then
      begin
       BStyle:=TSXSkinButtonStyle(SkinLibrary.Styles[A]);
       case Action of
        gcaHighlightIn:  Effect:=BStyle.HInButtonEffect;
        gcaHighlightOut: Effect:=BStyle.HOutButtonEffect;
        gcaDown:         Effect:=BStyle.DownButtonEffect;
        gcaUp:           Effect:=BStyle.UpButtonEffect;
        gcaEnable:       Effect:=BStyle.EnableButtonEffect;
        gcaDisable:      Effect:=BStyle.DisableButtonEffect;
        gcaFocus:        Effect:=BStyle.FocusButtonEffect;
        gcaUnfocus:      Effect:=BStyle.UnfocusButtonEffect;
       end;
      end;
    end;
  end;
end;

procedure TSXSkinCustomUpDown.StartChangingEffect(T:TSXGlyphChangeAction);
var         B:TBitmap32;
          Rgn:HRGN;
    Transform:TSXTransformEffectData;
begin
 GetUpDownTransformEffect(T,Transform);
 if HasTransformEffect(Transform) then
  begin
   CreateThreadIfNeeded;
   //
   B:=TBitmap32.Create;
   B.DrawMode:=dmBlend;
   B.CombineMode:=cmMerge;
   B.SetSize(Width,Height);
   B.Clear(0);
   Rgn:=CreateRectRgn(0,0,Width,Height);
   PaintCurrentBlendedUDStyle(B,0,0,Rect(0,0,Width,Height),Rgn);
   if not (HasTransformEffect(FLastUpDownTransform) and (FUpDownDoneSteps<FLastUpDownTransform.StepsNum)) and
           Transform.DrawCaption then
    PaintOverStyleButtonsToBitmap(B,0,0,Rect(0,0,Width,Height),Rgn);
   DeleteObject(Rgn);
   FLastUpDown.Free;
   FLastUpDown:=B;
   //
   FUpDownDoneSteps:=1;
   if FThread.Suspended then
    FThread.Resume;
  end;
 FLastUpDownTransform:=Transform;
end;

procedure TSXSkinCustomUpDown.StartUpButtonChangingEffect(T:TSXGlyphChangeAction);
var         B:TBitmap32;
          Rgn:HRGN;
       BWidth:Integer;
      BHeight:Integer;
    Transform:TSXTransformEffectData;
begin
 GetUpButtonTransformEffect(T,Transform);
 if HasTransformEffect(Transform) then
  begin
   CreateThreadIfNeeded;
   //
   BWidth:=FUpButtonRect.Right-FUpButtonRect.Left;
   BHeight:=FUpButtonRect.Bottom-FUpButtonRect.Top;
   B:=TBitmap32.Create;
   B.DrawMode:=dmBlend;
   B.CombineMode:=cmMerge;
   B.SetSize(BWidth,BHeight);
   B.Clear(0);
   Rgn:=CreateRectRgn(0,0,BWidth,BHeight);
   PaintBlendedUpButtonStyle(B,0,0,Rect(0,0,BWidth,BHeight),Rgn);
   DeleteObject(Rgn);
   FLastUpButton.Free;
   FLastUpButton:=B;
   //
   FUpButtonDoneSteps:=1;
   if FThread.Suspended then
    FThread.Resume;
  end;
 FLastUpButtonTransform:=Transform;
end;

procedure TSXSkinCustomUpDown.StartDownButtonChangingEffect(T:TSXGlyphChangeAction);
var         B:TBitmap32;
          Rgn:HRGN;
       BWidth:Integer;
      BHeight:Integer;
    Transform:TSXTransformEffectData;
begin
 GetDownButtonTransformEffect(T,Transform);
 if HasTransformEffect(Transform) then
  begin
   CreateThreadIfNeeded;
   //
   BWidth:=FDownButtonRect.Right-FDownButtonRect.Left;
   BHeight:=FDownButtonRect.Bottom-FDownButtonRect.Top;
   B:=TBitmap32.Create;
   B.DrawMode:=dmBlend;
   B.CombineMode:=cmMerge;
   B.SetSize(BWidth,BHeight);
   B.Clear(0);
   Rgn:=CreateRectRgn(0,0,BWidth,BHeight);
   PaintBlendedDownButtonStyle(B,0,0,Rect(0,0,BWidth,BHeight),Rgn);
   DeleteObject(Rgn);
   FLastDownButton.Free;
   FLastDownButton:=B;
   //
   FDownButtonDoneSteps:=1;
   if FThread.Suspended then
    FThread.Resume;
  end;
 FLastDownButtonTransform:=Transform;
end;

procedure TSXSkinCustomUpDown.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   FLastUpDown.Free;
   FLastUpDown:=nil;
   FLastUpButton.Free;
   FLastUpButton:=nil;
   FLastDownButton.Free;
   FLastDownButton:=nil;
   if (FThread<>nil) and not FThread.Suspended then
    FThread.Suspend;
   ResetUpDownParams([udrpButtonRects]);
  end;
 inherited;
end;

procedure TSXSkinCustomUpDown.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
var OldWidth:Integer;
   OldHeight:Integer;
begin
 if (ALeft=Left) and (ATop=Top) and (AWidth=Width) and (AHeight=Height) then exit;
 OldWidth:=Width;
 OldHeight:=Height;
 inherited SetBounds(ALeft,ATop,AWidth,AHeight);
 if (Width<>OldWidth) or (Height<>OldHeight) then
  ResetUpDownParams([udrpButtonRects]);
end;

procedure TSXSkinCustomUpDown.Loaded;
begin
 inherited;
 ResetUpDownParams([udrpButtonRects]);
end;

procedure TSXSkinCustomUpDown.WMSetFocus(var Msg:TWMSetFocus);
begin
 if not (csLoading in ComponentState) then
  StartChangingEffect(gcaFocus);
 FLastFocused:=True;
 if not (csLoading in ComponentState) then
  ResetUpDownParams([udrpInvalidateOnStyleChange]);
end;

procedure TSXSkinCustomUpDown.WMKillFocus(var Msg:TWMKillFocus);
begin
 if not (csLoading in ComponentState) then
  StartChangingEffect(gcaUnfocus);
 FLastFocused:=False;
 if not (csLoading in ComponentState) then
  ResetUpDownParams([udrpInvalidateOnStyleChange]);
 if FDownUp then
  begin
   StartChangingEffect(gcaUp);
   StartUpButtonChangingEffect(gcaUp);
   FDownUp:=False;
   ResetUpDownParams([udrpInvalidateOnStyleChange,udrpInvalidateUpButtonOnStyleChange]);
  end;
 if FDownDown then
  begin
   StartChangingEffect(gcaUp);
   StartDownButtonChangingEffect(gcaUp);
   FDownDown:=False;
   ResetUpDownParams([udrpInvalidateOnStyleChange,udrpInvalidateDownButtonOnStyleChange]);
  end;
end;

procedure TSXSkinCustomUpDown.DoKeyDown(var Msg: TMessage);
begin
 inherited;
{ if Enabled and (Msg.WParam in [VK_SPACE,VK_RETURN]) then
  begin
   StartGlyphChanging(gcaDown);
   FDown:=True;
   ResetUpDownParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
  end;
}
end;

procedure TSXSkinCustomUpDown.DoKeyUp(var Msg: TMessage);
begin
 inherited;
 {
 if Enabled and FDown then
  begin
   StartGlyphChanging(gcaUp);
   FDown:=False;
   ResetUpDownParams([brpTextOnFontChange,brpGlyphOnStyleChange,
                      brpDDGlyphOnStyleChange,brpInvalidateOnStyleChange]);
   if Msg.WParam in [VK_SPACE,VK_RETURN] then
    begin
     Click;
    end;
  end;
 }
end;

procedure TSXSkinCustomUpDown.ResetUpDownParams(Params:TSXSkinUpDownResetParams=[]);
var OldUpButtonRect:TRect;
  OldDownButtonRect:TRect;
    UpButtonChanged:Boolean;
  DownButtonChanged:Boolean;
            UDState:TSXSkinUpDownStateParam;
             BState:TSXSkinButtonStateParam;
              Style:TSXSkinUpDownStyle;
                  A:Integer;
begin
 if Parent=nil then exit;
 OldUpButtonRect:=FUpButtonRect;
 OldDownButtonRect:=FDownButtonRect;
 UpButtonChanged:=False;
 DownButtonChanged:=False;
 //
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   if udrpButtonRects in Params then
    begin
     A:=SkinLibrary.Styles.GetIndexByName(SkinStyle);
     if (A>=0) and (SkinLibrary.Styles[A] is TSXSkinUpDownStyle) then
      begin
       Style:=TSXSkinUpDownStyle(SkinLibrary.Styles[A]);
       if Style.UpButtonRect<>'' then
        GetRectFromString(Style.UpButtonRect,FUpButtonRect,OnGetVariable);
       if Style.DownButtonRect<>'' then
        GetRectFromString(Style.DownButtonRect,FDownButtonRect,OnGetVariable);
      end;
     UpButtonChanged:=not EqualRect(FUpButtonRect,OldUpButtonRect);
     DownButtonChanged:=not EqualRect(FDownButtonRect,OldDownButtonRect);
    end;
   if udrpInvalidateUpButtonOnStyleChange in Params then
    begin
     GetCurrentUpButtonState(BState);
     if (BState.Style<>FLastUpButtonStyle) or (BState.OverStyle<>FLastUpButtonOverStyle) then
      begin
       UpButtonChanged:=True;
       FLastUpButtonStyle:=BState.Style;
       FLastUpButtonOverStyle:=BState.OverStyle;
      end;
    end;
   if udrpInvalidateDownButtonOnStyleChange in Params then
    begin
     GetCurrentDownButtonState(BState);
     if (BState.Style<>FLastDownButtonStyle) or (BState.OverStyle<>FLastDownButtonOverStyle) then
      begin
       DownButtonChanged:=True;
       FLastDownButtonStyle:=BState.Style;
       FLastDownButtonOverStyle:=BState.OverStyle;
      end;
    end;
  end;  
 //Invalidating Regions
 if UpButtonChanged and HandleAllocated then
  begin
   InvalidateRect(Handle,@OldUpButtonRect,False);
   InvalidateRect(Handle,@FUpButtonRect,False);
  end;
 if DownButtonChanged and HandleAllocated then
  begin
   InvalidateRect(Handle,@OldDownButtonRect,False);
   InvalidateRect(Handle,@FDownButtonRect,False);
  end;
 //
 if udrpInvalidateOnStyleChange in Params then
  begin
   GetCurrentUDState(UDState);
   if (UDState.Style<>FLastStyle) or (UDState.OverStyle<>FLastOverStyle) then
    begin
     if HandleAllocated then
      InvalidateRect(Handle,nil,False);
     FLastStyle:=UDState.Style;
     FLastOverStyle:=UDState.OverStyle;
    end;
  end;
end;

function TSXSkinCustomUpDown.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 UDState:TSXSkinUpDownStateParam;
  BState:TSXSkinButtonStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentUDState(UDState);
   A:=SkinLibrary.Styles.GetGStyleIndex(UDState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
    end;
   if Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(UDState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,Limit,VComparer);
      end;
    end;
   if Result then
    begin
     GetCurrentUpButtonState(BState);
     A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,FUpButtonRect.Right-FUpButtonRect.Left,
                                          FUpButtonRect.Bottom-FUpButtonRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Up,X-FUpButtonRect.Left,Y-FUpButtonRect.Top,
               FUpButtonRect.Right-FUpButtonRect.Left,FUpButtonRect.Bottom-FUpButtonRect.Top,
               SkinLibrary,Limit);
      end;
     if Result then
      begin
       A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,FUpButtonRect.Right-FUpButtonRect.Left,
                                            FUpButtonRect.Bottom-FUpButtonRect.Top);
       if A>=0 then
        begin
         Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
         Result:=Style.IsTransparent(Self,CEID_Up,X-FUpButtonRect.Left,Y-FUpButtonRect.Top,
                 FUpButtonRect.Right-FUpButtonRect.Left,FUpButtonRect.Bottom-FUpButtonRect.Top,
                 SkinLibrary,Limit);
        end;
      end;
    end;
   if Result then
    begin
     GetCurrentDownButtonState(BState);
     A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,FDownButtonRect.Right-FDownButtonRect.Left,
                                          FDownButtonRect.Bottom-FDownButtonRect.Top);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.IsTransparent(Self,CEID_Down,X-FDownButtonRect.Left,Y-FDownButtonRect.Top,
               FDownButtonRect.Right-FDownButtonRect.Left,FDownButtonRect.Bottom-FDownButtonRect.Top,
               SkinLibrary,Limit);
      end;
     if Result then
      begin
       A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,FDownButtonRect.Right-FDownButtonRect.Left,
                                            FDownButtonRect.Bottom-FDownButtonRect.Top);
       if A>=0 then
        begin
         Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
         Result:=Style.IsTransparent(Self,CEID_Down,X-FDownButtonRect.Left,Y-FDownButtonRect.Top,
                 FDownButtonRect.Right-FDownButtonRect.Left,FDownButtonRect.Bottom-FDownButtonRect.Top,
                 SkinLibrary,Limit);
        end;
      end;
    end;
  end;
end;

function TSXSkinCustomUpDown.UpButtonCapturesMouseAt(X,Y:Integer):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 BState:TSXSkinButtonStateParam;
begin
 Result:=True;
 GetCurrentUpButtonState(BState);
 A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,FUpButtonRect.Right-FUpButtonRect.Left,
                                      FUpButtonRect.Bottom-FUpButtonRect.Top);
 if A>=0 then
  begin
   Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
   Result:=Style.CapturesMouseAt(Self,CEID_Up,X-FUpButtonRect.Left,Y-FUpButtonRect.Top,
           FUpButtonRect.Right-FUpButtonRect.Left,FUpButtonRect.Bottom-FUpButtonRect.Top,
           SkinLibrary);
  end;
 if not Result then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,FUpButtonRect.Right-FUpButtonRect.Left,
                                        FUpButtonRect.Bottom-FUpButtonRect.Top);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Up,X-FUpButtonRect.Left,Y-FUpButtonRect.Top,
             FUpButtonRect.Right-FUpButtonRect.Left,FUpButtonRect.Bottom-FUpButtonRect.Top,
             SkinLibrary);
    end;
  end;
end;

function TSXSkinCustomUpDown.DownButtonCapturesMouseAt(X,Y:Integer):Boolean;
var   A:Integer;
  Style:TSXSkinGeneralStyle;
 BState:TSXSkinButtonStateParam;
begin
 Result:=True;
 GetCurrentDownButtonState(BState);
 A:=SkinLibrary.Styles.GetGStyleIndex(BState.Style,FDownButtonRect.Right-FDownButtonRect.Left,
                                      FDownButtonRect.Bottom-FDownButtonRect.Top);
 if A>=0 then
  begin
   Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
   Result:=Style.CapturesMouseAt(Self,CEID_Down,X-FDownButtonRect.Left,Y-FDownButtonRect.Top,
           FDownButtonRect.Right-FDownButtonRect.Left,FDownButtonRect.Bottom-FDownButtonRect.Top,
           SkinLibrary);
  end;
 if not Result then
  begin
   A:=SkinLibrary.Styles.GetGStyleIndex(BState.OverStyle,FDownButtonRect.Right-FDownButtonRect.Left,
                                        FDownButtonRect.Bottom-FDownButtonRect.Top);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Down,X-FDownButtonRect.Left,Y-FDownButtonRect.Top,
             FDownButtonRect.Right-FDownButtonRect.Left,FDownButtonRect.Bottom-FDownButtonRect.Top,
             SkinLibrary);
    end;
  end;
end;

function TSXSkinCustomUpDown.CapturesMouseAt(X,Y:Integer):Boolean;
var    A:Integer;
   Style:TSXSkinGeneralStyle;
 UDState:TSXSkinUpDownStateParam;
begin
 Result:=True;
 if (SkinLibrary<>nil) and SkinLibrary.CanBeUsed then
  begin
   GetCurrentUDState(UDState);
   A:=SkinLibrary.Styles.GetGStyleIndex(UDState.Style,Width,Height);
   if A>=0 then
    begin
     Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
     Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
    end;
   if not Result then
    begin
     A:=SkinLibrary.Styles.GetGStyleIndex(UDState.OverStyle,Width,Height);
     if A>=0 then
      begin
       Style:=TSXSkinGeneralStyle(SkinLibrary.Styles[A]);
       Result:=Style.CapturesMouseAt(Self,CEID_Back,X,Y,Width,Height,SkinLibrary,VComparer);
      end;
    end;
   if not Result then
    Result:=UpButtonCapturesMouseAt(X,Y);
   if not Result then
    Result:=DownButtonCapturesMouseAt(X,Y);
  end;
end;

procedure TSXSkinCustomUpDown.WndProc(var Msg: TMessage);
begin
  if Msg.Msg=WM_TIMER then
   begin
    Inc(SeqClickNum);
    if SeqClickNum=1 then
     begin
      KillTimer(Handle,1);
      SetTimer(Handle,1,200,nil);
     end else
    if SeqClickNum=5 then
     begin
      KillTimer(Handle,1);
      SetTimer(Handle,1,150,nil);
     end else
    if SeqClickNum=10 then
     begin
      KillTimer(Handle,1);
      SetTimer(Handle,1,100,nil);
     end;
    if FDownUp and FMouseOverUp then
     DoClick(True) else
    if FDownDown and FMouseOverDown then
     DoClick(False);
   end;
 inherited;
end;

constructor TSXSkinCustomUpDown.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 CEID_Back:=GetNewCElementID;
 CEID_Up:=GetNewCElementID;
 CEID_Down:=GetNewCElementID;
 VComparer:=TSXUpDownVariableComparer.Create;
 VComparer.Control:=Self;
 VComparer.OnGetVariable:=OnGetVariable;
 ControlStyle:=ControlStyle-[csDoubleClicks];
 FEnabledUp:=True;
 FEnabledDown:=True;
 SkinStyle:='_UpDown';
 TabStop:=True;
end;

destructor TSXSkinCustomUpDown.Destroy;
begin
 FThread.Free;
 FLastUpDown.Free;
 FLastUpButton.Free;
 FLastDownButton.Free;
 inherited;
 VComparer.Free; 
end;

end.
