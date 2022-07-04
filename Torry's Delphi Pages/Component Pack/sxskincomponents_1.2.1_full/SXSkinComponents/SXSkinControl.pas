unit SXSkinControl;

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
     ExtCtrls, SysUtils, Controls, SXSkinLibrary;

type

  TSXWinControl=class(TWinControl)
   protected
    function CapturesMouseAt(X,Y:Integer):Boolean; virtual;
    function GetMouseCaptureControlAt(X,Y:Integer;CheckFront:Boolean=True):TWinControl;
  end;

  TSXSkinCustomControl=class(TCustomControl)
   private
    FSkinLibrary:TSXSkinLibrary;
    //FHintData:TSXHintData;
    FOnMouseDown:TMouseEvent;
    FOnMouseEnter:TNotifyEvent;
    FOnMouseLeave:TNotifyEvent;
    FOnMouseMove:TMouseMoveEvent;
    FOnMouseUp:TMouseEvent;
    DrawBR:TRect;
    DrawCR:TRect;
    DrawRgn:HRGN;
    FSkinStyle:String;
    FMCaptureCtrl:TWinControl;
    FLDownClickCtrl:TWinControl;
    LastCapturedMouse:Boolean;
    procedure SetSkinStyle(const Value:String);
    procedure SetSkinLibrary(Value:TSXSkinLibrary);
    procedure WMPaint(var Msg:TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    function GetMouseCaptureControlAt(X,Y:Integer;CheckFront:Boolean=True):TWinControl;
   protected
    FPressed:Boolean;
    function CapturesMouseAt(X,Y:Integer):Boolean; virtual;
    procedure SetParent(AParent:TWinControl); override;
    procedure Notification(AComponent:TComponent;Operation:TOperation); override;
    procedure CreateParams(var Params:TCreateParams); override;
    function NeedToPaintBackground:Boolean; virtual;
    procedure Paint; override;
    procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Msg:TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg:TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Msg:TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    //procedure CMHintShow(var Message:TCMHintShow); message CM_HINTSHOW;
    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
    procedure MouseMove(Shift:TShiftState;X,Y:Integer); override;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
    procedure MouseLeave; virtual;
   public
    procedure SetLoaded; virtual;
    procedure PaintRectToBitmap(DestCanvasHandle:HDC;DestCanvasRect:TRect;
               Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
               WithSubItems:Boolean); virtual;   
    procedure SkinChanged; virtual;
    function IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean; virtual;
    function CanShowControl:Boolean; virtual;
    procedure SetBounds(ALeft,ATop,AWidth,AHeight:Integer); override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property Font;
    //property HintData:TSXHintData read FHintData write FHintData;
    property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
    property SkinLibrary:TSXSkinLibrary read FSkinLibrary write SetSkinLibrary;
    property SkinStyle:String read FSkinStyle write SetSkinStyle;
   published
    property Left default 0;
    property Top default 0;
  end;

var  TestingRegions:Boolean=False;
 ControlsNotToPaint:TList;
   CanvasNotToPaint:TList;
         PaintCaret:Boolean=False;

implementation

uses SXSkinPanel;

function DoShowControl(Control:TControl):Boolean;
begin
 if Control is TSXSkinCustomControl then
  Result:=TSXSkinCustomControl(Control).CanShowControl else
   Result:=Control.Visible or (csDesigning in Control.ComponentState);
end;

{ TSXWinControl }

function TSXWinControl.CapturesMouseAt(X,Y:Integer):Boolean;
begin
 Result:=True;
end;

function TSXWinControl.GetMouseCaptureControlAt(X,Y:Integer;CheckFront:Boolean=True):TWinControl;
var P:TPoint;
 C,C2:TWinControl;
    A:Integer;
begin
 if CheckFront then
  begin
   P:=Point(X,Y);
   C:=Self;
   while C.Parent<>nil do C:=C.Parent;
   if C<>Self then
    P:=ClientToParent(P,C);
   C2:=C;
   repeat
    C:=C2; C2:=nil;
    for A:=C.ControlCount-1 downto 0 do
     if (C.Controls[A] is TWinControl) and C.Controls[A].Visible and PtInRect(C.Controls[A].BoundsRect,P) then
      begin
       P:=C.Controls[A].ParentToClient(P);
       C2:=TWinControl(C.Controls[A]);
       break;
      end;
   until C2=nil;
   if C is TSXWinControl then
    Result:=TSXWinControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
   if C is TSXSkinCustomControl then
    Result:=TSXSkinCustomControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
     Result:=C;
   exit;
  end;
 Result:=nil;
 if CapturesMouseAt(X,Y) then Result:=Self else
  if Parent<>nil then
   begin
    A:=Parent.ControlCount-1;
    while (A>=0) and (Parent.Controls[A]<>Self) do Dec(A);
    Dec(A);
    while A>=0 do
     begin
      if (Parent.Controls[A] is TWinControl) and Parent.Controls[A].Visible and
         PtInRect(Parent.Controls[A].BoundsRect,Point(X+Left,Y+Top)) then
       begin
        P:=Point(X+Left-Parent.Controls[A].Left,Y+Top-Parent.Controls[A].Top);
        C2:=TWinControl(Parent.Controls[A]);
        repeat
         C:=C2; C2:=nil;
         for A:=C.ControlCount-1 downto 0 do
          if (C.Controls[A] is TWinControl) and C.Controls[A].Visible and PtInRect(C.Controls[A].BoundsRect,P) then
           begin
            P:=C.Controls[A].ParentToClient(P);
            C2:=TWinControl(C.Controls[A]);
            break;
           end;
        until C2=nil;
        if C is TSXWinControl then
         Result:=TSXWinControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
        if C is TSXSkinCustomControl then
         Result:=TSXSkinCustomControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
          Result:=C;
        exit;  
       end;
      Dec(A);    
     end;
    if Parent is TSXWinControl then
     Result:=TSXWinControl(Parent).GetMouseCaptureControlAt(X+Left,Y+Top,False) else
    if Parent is TSXSkinCustomControl then
     Result:=TSXSkinCustomControl(Parent).GetMouseCaptureControlAt(X+Left,Y+Top,False) else
      Result:=Parent;
   end;
end;

{ TSXSkinCustomControl }

procedure TSXSkinCustomControl.CreateParams(var Params:TCreateParams);
begin
 inherited;
 with Params do
  begin
   if not (csDesigning in ComponentState) then
    begin
     Style:=Style and not WS_CLIPCHILDREN;
     Style:=Style and not WS_CLIPSIBLINGS;
    end;
  end;
end;

function TSXSkinCustomControl.IsTransparent(X,Y:Integer;Limit:Integer=10):Boolean;
begin
 Result:=False;
end;

function TSXSkinCustomControl.CanShowControl:Boolean;
begin
 Result:=Visible or (csDesigning in ComponentState);
end;

procedure TSXSkinCustomControl.SetSkinLibrary(Value:TSXSkinLibrary);
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

procedure TSXSkinCustomControl.SetSkinStyle(const Value:String);
begin
 if FSkinStyle<>Value then
  begin
   FSkinStyle:=Value;
   SkinChanged;
  end;
end;

procedure TSXSkinCustomControl.SetParent(AParent:TWinControl);
var A:Integer;
   PC:TControl;
begin
 inherited;
 if Parent=nil then exit;
 if not (csLoading in ComponentState) and (csDesigning in ComponentState) and
    (SkinLibrary=nil) then
  begin
   PC:=Parent;
   repeat
    if (PC is TSXSkinCustomControl) and (TSXSkinCustomControl(PC).SkinLibrary<>nil) then
     SkinLibrary:=TSXSkinCustomControl(PC).SkinLibrary else
    if PC is TWinControl then
     begin
      for A:=0 to TWinControl(PC).ControlCount-1 do
       if (TWinControl(PC).Controls[A] is TSXSkinCustomControl) and
          (TSXSkinCustomControl(TWinControl(PC).Controls[A]).SkinLibrary<>nil) then
        begin
         SkinLibrary:=TSXSkinCustomControl(TWinControl(PC).Controls[A]).SkinLibrary;
         break;
        end;
      for A:=0 to TWinControl(PC).ComponentCount-1 do
       if TWinControl(PC).Components[A] is TSXSkinLibrary then
        begin
         SkinLibrary:=TSXSkinLibrary(TWinControl(PC).Components[A]);
         break;
        end;
     end;
    PC:=PC.Parent; 
   until (PC=nil) or not (PC is TWinControl) or (SkinLibrary<>nil);
  end;
end;

procedure TSXSkinCustomControl.Notification(AComponent:TComponent;Operation:TOperation);
begin
 inherited Notification(AComponent,Operation);
 if Operation=opRemove then
  begin
   if AComponent=FSkinLibrary then
    FSkinLibrary:=nil;
  end;
end;

procedure TSXSkinCustomControl.MouseLeave;
begin
 LastCapturedMouse:=False;
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TSXSkinCustomControl.CMMouseLeave(var Msg:TMessage);
var P:TPoint;
    C:TWinControl;
begin
 P:=ScreenToClient(Mouse.CursorPos);
 C:=GetMouseCaptureControlAt(P.X,P.Y);
 if (FMCaptureCtrl<>nil) and (C<>FMCaptureCtrl) then
  begin
   SendMessage(FMCaptureCtrl.Handle,CM_MOUSELEAVE,0,0);
   FMCaptureCtrl:=nil;
  end;
 if C<>Self then
  begin
   inherited;
   if LastCapturedMouse then
    MouseLeave;
  end;
end;

procedure TSXSkinCustomControl.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if LastCapturedMouse then
  begin
   FPressed:=True;
   if TabStop and CanFocus then SetFocus;
   if Assigned(FOnMouseDown) then FOnMouseDown(Self,Button,Shift,X,Y);
  end;
end;

procedure TSXSkinCustomControl.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if FPressed then
  begin
   FPressed:=False;
   if Assigned(FOnMouseUp) then FOnMouseUp(Self,Button,Shift,X,Y);
  end;
end;

procedure TSXSkinCustomControl.MouseMove(Shift:TShiftState;X,Y:Integer);
var MouseDown:Boolean;
begin
 MouseDown:=Shift*[ssLeft,ssRight,ssMiddle]<>[];
 if (not MouseDown and LastCapturedMouse) or (MouseDown and FPressed) then
  begin
   inherited;
   if Assigned(FOnMouseMove) then FOnMouseMove(Self,Shift,X,Y);
  end;
end;

function TSXSkinCustomControl.GetMouseCaptureControlAt(X,Y:Integer;CheckFront:Boolean=True):TWinControl;
var P:TPoint;
 C,C2:TWinControl;
    A:Integer;
begin
 if CheckFront then
  begin
   P:=Point(X,Y);
   C:=Self;
   while C.Parent<>nil do C:=C.Parent;
   if C<>Self then
    P:=ClientToParent(P,C);
   C2:=C;
   repeat
    C:=C2; C2:=nil;
    for A:=C.ControlCount-1 downto 0 do
     if (C.Controls[A] is TWinControl) and C.Controls[A].Visible and PtInRect(C.Controls[A].BoundsRect,P) then
      begin
       Dec(P.X,C.Controls[A].Left);
       Dec(P.Y,C.Controls[A].Top);
       C2:=TWinControl(C.Controls[A]);
       break;
      end;
   until C2=nil;
   if C is TSXWinControl then
    Result:=TSXWinControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
   if C is TSXSkinCustomControl then
    Result:=TSXSkinCustomControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
     Result:=C;
   exit;
  end;
 Result:=nil;
 if CapturesMouseAt(X,Y) then Result:=Self else
  if Parent<>nil then
   begin
    A:=Parent.ControlCount-1;
    while (A>=0) and (Parent.Controls[A]<>Self) do Dec(A);
    Dec(A);
    while A>=0 do
     begin
      if (Parent.Controls[A] is TWinControl) and Parent.Controls[A].Visible and
         PtInRect(Parent.Controls[A].BoundsRect,Point(X+Left,Y+Top)) then
       begin
        P:=Point(X+Left-Parent.Controls[A].Left,Y+Top-Parent.Controls[A].Top);
        C2:=TWinControl(Parent.Controls[A]);
        repeat
         C:=C2; C2:=nil;
         for A:=C.ControlCount-1 downto 0 do
          if (C.Controls[A] is TWinControl) and C.Controls[A].Visible and PtInRect(C.Controls[A].BoundsRect,P) then
           begin
            P:=C.Controls[A].ParentToClient(P);
            C2:=TWinControl(C.Controls[A]);
            break;
           end;
        until C2=nil;
        if C is TSXWinControl then
         Result:=TSXWinControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
        if C is TSXSkinCustomControl then
         Result:=TSXSkinCustomControl(C).GetMouseCaptureControlAt(P.X,P.Y,False) else
          Result:=C;
        exit;  
       end;
      Dec(A);    
     end;
    if Parent is TSXWinControl then
     Result:=TSXWinControl(Parent).GetMouseCaptureControlAt(X+Left,Y+Top,False) else
    if Parent is TSXSkinCustomControl then
     Result:=TSXSkinCustomControl(Parent).GetMouseCaptureControlAt(X+Left,Y+Top,False) else
      Result:=Parent;
   end;
end;

procedure TSXSkinCustomControl.WMMouseMove(var Msg:TWMMouseMove);
var P:TPoint;
    C:TWinControl;
begin
 if not CapturesMouseAt(Msg.XPos,Msg.YPos) and (Parent<>nil) then
  begin
   C:=GetMouseCaptureControlAt(Msg.XPos,Msg.YPos);
   if (C<>nil) and (C<>Self) then
    begin
     if (FMCaptureCtrl<>nil) and (FMCaptureCtrl<>C) then
      SendMessage(FMCaptureCtrl.Handle,CM_MOUSELEAVE,0,0);
     FMCaptureCtrl:=C;
     P:=C.ScreenToClient(ClientToScreen(Point(Msg.XPos,Msg.YPos)));
     SendMessage(C.Handle,WM_MOUSEMOVE,TMessage(Msg).WParam,P.X or (P.Y shl 16));
    end;
  end else
   begin
    if FMCaptureCtrl<>nil then
     begin
      SendMessage(FMCaptureCtrl.Handle,CM_MOUSELEAVE,0,0);
      FMCaptureCtrl:=nil;
     end;
    if not LastCapturedMouse then
     begin
      LastCapturedMouse:=True;
      if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
     end;
   end;
 inherited;
end;

procedure TSXSkinCustomControl.WMLButtonDown(var Msg:TWMLButtonDown);
var P:TPoint;
    C:TWinControl;
begin
 if not CapturesMouseAt(Msg.XPos,Msg.YPos) and (Parent<>nil) then
  begin
   C:=GetMouseCaptureControlAt(Msg.XPos,Msg.YPos);
   if (C<>nil) and (C<>Self) then
    begin
     FLDownClickCtrl:=TWinControl(C);
     P:=C.ScreenToClient(ClientToScreen(Point(Msg.XPos,Msg.YPos)));
     SendMessage(TWinControl(C).Handle,WM_LBUTTONDOWN,TMessage(Msg).WParam,P.X or (P.Y shl 16));
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomControl.WMLButtonUp(var Msg:TWMLButtonUp);
var P:TPoint;
begin
 if (FLDownClickCtrl<>nil) and (FLDownClickCtrl<>Self) then
  begin
   P:=FLDownClickCtrl.ScreenToClient(Parent.ClientToScreen(P));
   SendMessage(TWinControl(FLDownClickCtrl).Handle,WM_LBUTTONUP,TMessage(Msg).WParam,P.X or (P.Y shl 16));
   FLDownClickCtrl:=nil;
  end;
 inherited;
end;

procedure TSXSkinCustomControl.WMLButtonDblClk(var Msg:TWMLButtonDblClk);
var P:TPoint;
    C:TWinControl;
begin
 if not CapturesMouseAt(Msg.XPos,Msg.YPos) and (Parent<>nil) then
  begin
   C:=GetMouseCaptureControlAt(Msg.XPos,Msg.YPos);
   if (C<>nil) and (C<>Self) then
    begin
     FLDownClickCtrl:=C;
     P:=C.ScreenToClient(ClientToScreen(Point(Msg.XPos,Msg.YPos)));
     if csDoubleClicks in C.ControlStyle then
      SendMessage(C.Handle,WM_LBUTTONDBLCLK,TMessage(Msg).WParam,P.X or (P.Y shl 16)) else
       SendMessage(C.Handle,WM_LBUTTONDOWN,TMessage(Msg).WParam,P.X or (P.Y shl 16));
    end;
  end;
 inherited;
end;

procedure TSXSkinCustomControl.SetLoaded;
begin
 Loaded;
end;

procedure TSXSkinCustomControl.PaintRectToBitmap(DestCanvasHandle:HDC;
           DestCanvasRect:TRect;Rect:TRect;Rgn:HRGN;Bitmap:TBitmap32;X,Y:Integer;
           WithSubItems:Boolean);
var  A:Integer;
  R,R2:TRect;
     P:TPoint;
    BB:Boolean;
 XX,YY:Integer;
begin
 if WithSubItems then
  for A:=0 to ControlCount-1 do
   begin
    R2:=Controls[A].BoundsRect;
    if DoShowControl(Controls[A]) and RectInRegion(Rgn,R2) then
     begin
      IntersectRect(R,Rect,R2);
      if (Controls[A] is TSXSkinCustomControl) and (ControlsNotToPaint.IndexOf(Controls[A])<0) then
       begin
        XX:=R.Left-Rect.Left+X;
        YY:=R.Top-Rect.Top+Y;
        OffsetRect(R,-Controls[A].Left,-Controls[A].Top);
        OffsetRgn(Rgn,-Controls[A].Left,-Controls[A].Top);
        TSXSkinCustomControl(Controls[A]).PaintRectToBitmap(DestCanvasHandle,
           DestCanvasRect,R,Rgn,Bitmap,XX,YY,True);
        if TSXSkinCustomControl(Controls[A]).HandleAllocated and
           not (csDesigning in ComponentState) then
         ValidateRgn(TSXSkinCustomControl(Controls[A]).Handle,Rgn);
        OffsetRgn(Rgn,Controls[A].Left,Controls[A].Top);
       end else
      if ((Controls[A] is TGraphicControl) or ((Controls[A] is TWinControl) and not (csCustomPaint in Controls[A].ControlState))) and
         (ControlsNotToPaint.IndexOf(Controls[A])<0) then
       begin
        XX:=X-Rect.Left+Controls[A].Left;
        YY:=Y-Rect.Top+Controls[A].Top;
        GetWindowOrgEx(Bitmap.Handle,P);
        SetWindowOrgEx(Bitmap.Handle,P.X-XX,P.Y-YY,nil);
        ControlsNotToPaint.Add(Controls[A]);
        try
         BB:=PaintCaret;
         PaintCaret:=True;
         Controls[A].Perform(WM_PRINT,Integer(Bitmap.Handle),PRF_CHECKVISIBLE or PRF_CHILDREN or PRF_CLIENT or PRF_NONCLIENT or PRF_OWNED);
         PaintCaret:=BB;
        finally
         ControlsNotToPaint.Extract(Controls[A]);
        end;
        SetWindowOrgEx(Bitmap.Handle,P.X,P.Y,nil);
        if Controls[A] is TWinControl then
         begin
          R:=Controls[A].BoundsRect;
          OffsetRect(R,-Controls[A].Left,-Controls[A].Top);
          if not (csDesigning in ComponentState) then
           //
           //TO DO: Change to ValidateRgn
           //
//          if Controls[A] is TSXWinControl then
//           ExcludeClipRect(DestCanvasHandle,DestCanvasRect.Left+XX,DestCanvasRect.Top+YY,
//              DestCanvasRect.Left+XX+Controls[A].Width,DestCanvasRect.Top+YY+Controls[A].Height) else
            ValidateRect(TWinControl(Controls[A]).Handle,@R);              
         end;
       end;
     end;
   end;
end;

function TSXSkinCustomControl.NeedToPaintBackground:Boolean;
begin
 Result:=True;
end;

procedure TSXSkinCustomControl.WMPaint(var Msg:TWMPaint);
var BB:Boolean;
begin
 if not ((csDesigning in ComponentState) and (csPaintCopy in ControlState)) then
  begin
   if GetUpdateRgn(Handle,DrawRgn,False)=ERROR then
    begin
     DeleteObject(DrawRgn);
     DrawRgn:=CreateRectRgn(0,0,Width,Height);
    end;
   BB:=GetUpdateRect(Handle,DrawCR,False);
  end else BB:=False;
 if not BB then
  DrawCR:=Rect(0,0,Width,Height);
 DrawBR:=DrawCR;
 OffsetRect(DrawBR,Left,Top);
 inherited;
end;

procedure TSXSkinCustomControl.Paint;
const XorColor=$00FFD8CE;
var    B:TBitmap32;
   A,X,Y:Integer;
       R:TRect;
  TDrawCR:TRect;
  TDrawBR:TRect;
 TDrawRgn:HRGN;
       PC:TControl;
    DX,DY:Integer;
       BR:TRect;
   DoSibl:Boolean;
begin
 TDrawCR:=DrawCR;
 TDrawBR:=DrawBR;
 TDrawRgn:=DrawRgn;
 DrawRgn:=CreateRectRgn(0,0,Width,Height);
 DrawCR:=Rect(0,0,Width,Height);
 DrawBR:=BoundsRect;
 if not CanShowControl or (csLoading in ComponentState) or (csDestroying in ComponentState) then exit;
 if (csDesigning in ComponentState) and not (Self is TSXSkinPanel) and ((FSkinLibrary=nil){ or not FSkinLibrary.Active}) then
  begin
   DeleteObject(TDrawRgn);
   with Canvas do
    begin
     Pen.Style:=psDot;
     Pen.Mode:=pmXor;
     Pen.Color:=XorColor;
     Brush.Style:=bsClear;
     Rectangle(0,0,ClientWidth,ClientHeight);
    end;
   exit;
  end;
 if CanvasNotToPaint.IndexOf(Pointer(Canvas.Handle))>=0 then exit;
 B:=TBitmap32.Create;
 try
  B.DrawMode:=dmBlend;
  B.CombineMode:=cmBlend;
  B.SetSize(TDrawCR.Right-TDrawCR.Left,TDrawCR.Bottom-TDrawCR.Top);
  DoSibl:=True;
  if NeedToPaintBackground then
   begin
    PC:=Parent;
    BR:=TDrawBR;
    DX:=0; DY:=0;
    while (PC<>nil) and (PC is TSXSkinCustomControl) and (PC.Parent<>nil) and
          (PC.Parent is TSXSkinCustomControl) do
     begin
      Inc(DX,PC.Left);
      Inc(DY,PC.Top);
      OffsetRect(BR,PC.Left,PC.Top);
      OffsetRgn(TDrawRgn,PC.Left,PC.Top);
      PC:=PC.Parent;
     end;
    if (PC<>nil) and (PC is TSXSkinCustomControl) then
     begin
      OffsetRgn(TDrawRgn,Left,Top);
      Inc(DX,Left); Inc(DY,Top);
      if PC=Parent then
       TSXSkinCustomControl(PC).PaintRectToBitmap(Canvas.Handle,TDrawCR,BR,TDrawRgn,B,0,0,False) else
        begin
         DoSibl:=False;
         TSXSkinCustomControl(PC).PaintRectToBitmap(Canvas.Handle,TDrawCR,BR,TDrawRgn,B,0,0,True);
        end;
     end else B.Clear(Color32(Color));
    OffsetRgn(TDrawRgn,-DX,-DY);
   end;
  if DoSibl then
   begin
    OffsetRgn(TDrawRgn,Left,Top);
    for A:=0 to Parent.ControlCount-1 do
     if DoShowControl(Parent.Controls[A]) and (Parent.Controls[A] is TSXSkinCustomControl) and
        (ControlsNotToPaint.IndexOf(Parent.Controls[A])<0) then
      begin
       if Parent.Controls[A]=Self then
        begin
         OffsetRgn(TDrawRgn,-Left,-Top);
         PaintRectToBitmap(Canvas.Handle,TDrawCR,TDrawCR,TDrawRgn,B,0,0,True);
         OffsetRgn(TDrawRgn,Left,Top);
        end else
       if RectInRegion(TDrawRgn,Parent.Controls[A].BoundsRect) then
        begin
         IntersectRect(R,TDrawBR,Parent.Controls[A].BoundsRect);
         X:=R.Left-TDrawBR.Left;
         Y:=R.Top-TDrawBR.Top;
         OffsetRect(R,-Parent.Controls[A].Left,-Parent.Controls[A].Top);
         OffsetRgn(TDrawRgn,-Parent.Controls[A].Left,-Parent.Controls[A].Top);
         TSXSkinCustomControl(Parent.Controls[A]).PaintRectToBitmap(Canvas.Handle,TDrawCR,R,TDrawRgn,B,X,Y,True);
         if TSXSkinCustomControl(Parent.Controls[A]).HandleAllocated and
            not (csDesigning in ComponentState) then
          ValidateRgn(TSXSkinCustomControl(Parent.Controls[A]).Handle,TDrawRgn);
         OffsetRgn(TDrawRgn,Parent.Controls[A].Left,Parent.Controls[A].Top);
        end;
      end;
    OffsetRgn(TDrawRgn,-Left,-Top);
   end;
{  for A:=0 to ControlCount-1 do
   if not (Controls[A] is TSXSkinCustomControl) then
    ExcludeClipRect(Canvas.Handle,Controls[A].Left,Controls[A].Top,
       Controls[A].Left+Controls[A].Width,Controls[A].Top+Controls[A].Height); }
 // ExcludeClipRect(Canvas.Handle,10,10,Width-10,Height-10);
  BitBlt(Canvas.Handle,TDrawCR.Left,TDrawCR.Top,B.Width,B.Height,B.Handle,0,0,SRCCOPY);
  if TestingRegions then
   begin
    Canvas.Brush.Style:=bsSolid;
    Canvas.Brush.Color:=RGB(random(256),random(256),random(256));
    FillRgn(Canvas.Handle,DrawRgn,Canvas.Brush.Handle);
   end;
 finally
  B.Free;
 end;
 DeleteObject(TDrawRgn);
end;

{procedure TSXSkinCustomControl.CMHintShow(var Message:TCMHintShow);
begin
 inherited;
 Message.Result:=0;
 Message.HintInfo.CursorRect:=Rect(0,0,Width,Height);
 Message.HintInfo.HintStr:=' ';
 Message.HintInfo.HintData:=FHintData;
 SetHintTimeout(Message.HintInfo^);
end;}

procedure TSXSkinCustomControl.WMEraseBkgnd(var Msg:TWmEraseBkgnd);
begin
 if csDesigning in ComponentState then
  begin
   Msg.Result:=1;
   inherited;
  end else Msg.Result:=1;
end;

procedure TSXSkinCustomControl.SetBounds(ALeft,ATop,AWidth,AHeight:Integer);
//var R:TRect;
begin
 if Parent=nil then
  begin
   inherited;
   exit;
  end;
 if CanShowControl and NeedToPaintBackground and ((Left<>ALeft) or (Top<>ATop) or (Width<>AWidth) or (Height<>AHeight)) then
  begin
   if csDesigning in ComponentState then
    begin
     inherited;
     Invalidate;
    end else
   {if Self is TSXSkinPanel then
    begin
     inherited;
     R:=Rect(0,0,Width,Height);
     ValidateRect(Handle,@R);
    end else}
     begin
      Visible:=False;
      inherited;
      Visible:=True;
     end;
  end else inherited;
end;

procedure TSXSkinCustomControl.SkinChanged;
begin
 if not (csLoading in ComponentState) then
  begin
   Realign;
   if HandleAllocated then
    InvalidateRect(Handle,nil,False);
  end;
end;

function TSXSkinCustomControl.CapturesMouseAt(X,Y:Integer):Boolean;
begin
 Result:=True;
end;

constructor TSXSkinCustomControl.Create(AOwner:TComponent);
begin
 inherited Create(AOwner);
 //FHintData:=TSXHintData.Create;
 ControlStyle:=ControlStyle+[csAcceptsControls,csCaptureMouse,csOpaque];
 DoubleBuffered:=False;
 TabStop:=False;
 DrawRgn:=CreateRectRgn(0,0,0,0);
end;

destructor TSXSkinCustomControl.Destroy;
begin
 DeleteObject(DrawRgn);
 SkinLibrary:=nil;
 //FHintData.Free;
 inherited Destroy;
end;

initialization

 ControlsNotToPaint:=TList.Create;
 CanvasNotToPaint:=TList.Create;

finalization

 ControlsNotToPaint.Free;
 CanvasNotToPaint.Free;

end.
