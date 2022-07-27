{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmScrollableControl
Purpose  : Finally got tired of having to implement a scrolling control each time
           I wanted one.  So here is a base control....
Date     : 04-14-2002
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmScrollableControl;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, Forms, Classes, Controls, StdCtrls, dialogs, sysutils;

type
  TScrollEvent = procedure(Sender:TObject; ScrollBar:integer) of object;

  TrmCustomScrollableControl = class(TCustomControl)
  private
    { Private declarations }
    fIndex: integer;
    fTopIndex: integer;
    fxPos: integer;
    fOnScroll: TScrollEvent;
    fShowFocusRect: boolean;
    FHideSelection: boolean;
    fMultiSelect: boolean;
    FScrollBars: TScrollStyle;
    fSelStart: integer;
    fSelEnd : integer;
    FBorderStyle: TBorderStyle;
    fmousebusy : boolean;

    procedure wmSize(var MSG: TWMSize); message wm_size;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;

    procedure setIndex(const Value: integer);
    function GetHScrollPos: integer;
    function GetVScrollPos: integer;
    procedure SetHScrollPos(const Value: integer);
    procedure SetVScrollPos(const Value: integer);
    procedure SetShowFocusRect(const Value: boolean);
    procedure SetHideSelection(const Value: boolean);
    function GetHScrollSize: integer;
    function GetVScrollSize: integer;
    procedure setMultiselect(const Value: boolean);
    procedure SetSelCount(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelCount: integer;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    procedure SetTopIndex(const Value: integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure ScrollToVisible;

    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure DoItemIndexChange; virtual;

    procedure CalcMultiSelect(oldIndex, oldSelStart, oldSelEnd:integer);

    procedure VerticalScrollChange(sender:TObject); virtual;
    procedure HorizontalScrollChange(sender:TObject); virtual;
    function MaxItemLength: integer; virtual;
    function MaxItemCount:integer; virtual;
    function VisibleItems: integer; virtual;
    function MaxItemHeight: integer; virtual;
    function MaxItemWidth: integer; virtual;

    property InternalTopIndex : integer read fTopIndex write SetTopIndex;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property Multiselect : boolean read fMultiSelect write setMultiselect;
    property ItemIndex: integer read fIndex write setIndex default -1;
    property VScrollPos: integer read GetVScrollPos write SetVScrollPos;
    property HScrollPos: integer read GetHScrollPos write SetHScrollPos;
    property VScrollSize: integer read GetVScrollSize;
    property HScrollSize: integer read GetHScrollSize;
    property HideSelection:boolean read FHideSelection write SetHideSelection default false;
    property SelStart : integer read fSelStart write SetSelStart;
    property SelCount : integer read GetSelCount write SetSelCount;
    property ShowFocusRect: boolean read fShowFocusRect write SetShowFocusRect default true;
    property OnScroll: TScrollEvent read fOnScroll write fOnScroll;
  public
    { Public declarations }
    constructor create(aowner: TComponent); override;
  end;

implementation

uses Math, rmlibrary;

{ TrmCustomScrollableControl }

constructor TrmCustomScrollableControl.create(aowner: TComponent);
begin
  inherited;

  fmousebusy := false;
  ControlStyle := controlstyle + [csopaque];
  height := 200;
  width := 400;
  fIndex := -1;
  fTopIndex := 0;
  fSelStart := fIndex;
  fSelEnd := fIndex;
  fXPos := 0;
  fShowFocusRect := true;
  FHideSelection := false;
end;

procedure TrmCustomScrollableControl.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or ScrollBar[FScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

function TrmCustomScrollableControl.GetHScrollPos: integer;
var
  wScrollInfo: TScrollInfo;
begin
  if (fScrollBars = ssHorizontal) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS;
    end;

    if GetScrollInfo(Handle, SB_HORZ, wScrollInfo) then
      result := wScrollInfo.nPos
    else
      result := 0;
  end
  else
    result := fxPos;
end;

function TrmCustomScrollableControl.GetVScrollPos: integer;
var
  wScrollInfo: TScrollInfo;
begin
  if (fScrollBars = ssVertical) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS;
    end;

    if GetScrollInfo(Handle, SB_VERT, wScrollInfo) then
      result := wScrollInfo.nPos
    else
      result := 0;
  end
  else
    result := InternalTopIndex;
end;

procedure TrmCustomScrollableControl.ScrollToVisible;
begin
  if not (csCreating in ControlState) then
  begin
     if (InternalTopIndex < fIndex) then
     begin
       if (InternalTopIndex + (VisibleItems - 1) < fIndex) then
         InternalTopIndex := SetInRange(findex - (visibleItems-1), 0, MaxItemCount);
     end
     else if fIndex < InternalTopIndex then
       InternalTopIndex := fIndex;

     if InternalTopIndex < 0 then
     begin
       InternalTopIndex := 0;
       fIndex := 0;
       Try
          DoItemIndexChange;
       except
          //Do nothing...
       end;
     end;
  end;
end;

procedure TrmCustomScrollableControl.SetHScrollPos(const Value: integer);
var
  wScrollInfo: TScrollInfo;
begin
  if (fScrollBars = ssHorizontal) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS;
      nPos := Value;
    end;

    fxPos := SetScrollInfo(Handle, SB_HORZ, wScrollInfo, true);
  end
  else
    fxPos := SetInRange(Value, 0, HScrollSize);
  Invalidate;
end;

procedure TrmCustomScrollableControl.setIndex(const Value: integer);
begin
  if MaxItemCount > 0 then
  begin
     fIndex := SetInRange(Value, 0, MaxItemCount-1);
     fSelStart := fIndex;
     fSelEnd := fIndex;
     ScrollToVisible;
     UpdateVScrollBar;
     Invalidate;
     Try
        DoItemIndexChange;
     except
        //Do nothing...
     end;
  end;
end;

procedure TrmCustomScrollableControl.SetVScrollPos(const Value: integer);
var
  wScrollInfo: TScrollInfo;
begin
  if (fScrollBars = ssVertical) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS or SIF_DISABLENOSCROLL;
      nMin := 0;
      nMax := 0;
      nPos := Value;
    end;

    InternalTopIndex := SetScrollInfo(Handle, SB_VERT, wScrollInfo, true);
  end
  else
  begin
    InternalTopIndex := SetInRange(value, 0, VScrollSize);
  end;
  Invalidate;
end;

procedure TrmCustomScrollableControl.UpdateVScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  fTopIndex := SetInRange(fTopIndex, 0, VScrollSize);

  if (fScrollBars = ssVertical) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS or SIF_RANGE or SIF_DISABLENOSCROLL;
      nMin := 0;
      nMax := VScrollSize;
      nPos := fTopIndex;
    end;

    SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
    fTopIndex := VScrollPos;
  end;

  VerticalScrollChange(self);

  if assigned(fOnScroll) then
    fOnScroll(self, SB_VERT);
end;

procedure TrmCustomScrollableControl.UpdateHScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  fxPos := SetInRange(fxPos, 0, HScrollSize);

  if (fScrollBars = ssHorizontal) or (FScrollBars = ssBoth) then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS or SIF_RANGE or SIF_DISABLENOSCROLL;
      nMin := 0;
      nMax := HScrollSize;
      nPos := fxPos;
    end;

    SetScrollInfo(Handle, SB_HORZ, wScrollInfo, True);
    fxPos := HScrollPos;
  end;

  HorizontalScrollChange(self);

  if assigned(fOnScroll) then
    fOnScroll(self, SB_HORZ);
end;


function TrmCustomScrollableControl.VisibleItems: integer;
begin
  result := ClientHeight div MaxItemHeight;
end;

procedure TrmCustomScrollableControl.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: fxPos := MaxItemLength - clientwidth;
    SB_LINEDOWN: inc(fxPos, MaxItemWidth);
    SB_LINEUP: dec(fxPos, MaxItemWidth);
    SB_TOP: fxPos := 0;
    SB_PAGEDOWN: inc(fxPos, (MaxItemLength - clientwidth) div 2);
    SB_PAGEUP: dec(fxPos, (MaxItemLength - clientwidth) div 2);
    SB_THUMBPOSITION: fxPos := Msg.Pos;
    SB_THUMBTRACK: fxPos := Msg.Pos;
  else
    exit;
  end;

  UpdateHScrollBar;
  Invalidate;
end;

procedure TrmCustomScrollableControl.wmSize(var MSG: TWMSize);
begin
  UpdatevScrollBar;
  UpdateHScrollBar;
  inherited;
end;

procedure TrmCustomScrollableControl.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: InternalTopIndex := MaxItemCount - VisibleItems;
    SB_LINEDOWN: InternalTopIndex := InternalTopIndex+1;
    SB_LINEUP: InternalTopIndex := InternalTopIndex-1;
    SB_TOP: InternalTopIndex := 0;
    SB_PAGEDOWN: InternalTopIndex := InternalTopIndex+VisibleItems;
    SB_PAGEUP: InternalTopIndex := InternalTopIndex-VisibleItems;
    SB_THUMBPOSITION: InternalTopIndex := Msg.Pos;
    SB_THUMBTRACK: InternalTopIndex := Msg.Pos;
  else
    exit;
  end;

  UpdateVScrollBar;
  Invalidate;
end;

procedure TrmCustomScrollableControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TrmCustomScrollableControl.KeyDown(var Key: Word; Shift: TShiftState);
var
   wOldSelEnd, wOldindex, wOldSelStart: integer;
begin
  inherited;
  wOldindex := fIndex;
  wOldSelStart := fSelStart;
  wOldSelEnd := fSelEnd;

  case key of
    vk_end: fIndex := MaxItemCount - 1;
    vk_DOWN: inc(fIndex);
    vk_up: dec(fIndex);
    vk_home: fIndex := 0;
    VK_Right:
      begin
        if (shift = []) then
          inc(fxPos)
        else if (shift = [ssCtrl]) then
          fxPos := MaxItemLength - ClientWidth;
      end;
    VK_LEFT:
      begin
        if (shift = []) then
          dec(fxPos)
        else if (shift = [ssCtrl]) then
          fxPos := 0;
      end;
    vk_next: inc(fIndex, VisibleItems - 1);
    vk_prior: Dec(fIndex, VisibleItems - 1);
  else
    exit;
  end;

  if MaxItemCount > 0 then
     fIndex := SetInRange(fIndex, 0, MaxItemCount-1)
  else
     fIndex := -1;

  if (shift = [ssShift]) and (fMultiSelect) then
     CalcMultiSelect(wOldIndex, wOldSelStart, wOldSelEnd)
  else
  begin
     fSelStart := fIndex;
     fSelEnd := fIndex;
  end;

  Try
     DoItemIndexChange;
  except
     //Do nothing...
  end;

  ScrollToVisible;
  UpdateVScrollBar;
  UpdateHScrollBar;

  Invalidate;
end;

procedure TrmCustomScrollableControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  wOldSelEnd, wOldindex, wOldSelStart: integer;
begin
   inherited;

   wOldindex := fIndex;
   wOldSelStart := fSelStart;
   wOldSelEnd := fSelEnd;

   if Button = mbLeft then
   begin
     if CanFocus then
       setfocus;

     if y < 0 then
        y := -MaxItemHeight;

     if y > clientheight then
        y := clientheight+MaxItemHeight;

     if MaxItemCount > 0 then
        fIndex := SetInRange(InternalTopIndex + (y div maxitemheight), 0, MaxItemCount-1)
     else
        fIndex := -1;

     if (ssShift in Shift) and (fMultiSelect) then
        CalcMultiSelect(wOldIndex, wOldSelStart, wOldSelEnd)
     else
     begin
        fSelStart := fIndex;
        fSelEnd := fIndex;
     end;

     if fIndex <> wOldIndex then
     begin
        Try
           DoItemIndexChange;
        except
           //Do nothing...
        end;

        ScrollToVisible;
        UpdateVScrollBar;
        UpdateHScrollBar;
     end;

     Invalidate;
   end;
end;

procedure TrmCustomScrollableControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  wOldSelEnd, wOldindex, wOldSelStart: integer;
begin
  inherited;

  wOldindex := fIndex;
  wOldSelStart := fSelStart;
  wOldSelEnd := fSelEnd;

  if focused and (ssLeft in Shift) then
  begin
     if CanFocus then
       setfocus;

     if MaxItemCount > 0 then
        fIndex := SetInRange(InternalTopIndex + (y div maxitemheight), 0, MaxItemCount-1)
     else
        fIndex := -1;

     if (fMultiSelect) then
        CalcMultiSelect(wOldIndex, wOldSelStart, wOldSelEnd)
     else
     begin
        fSelStart := fIndex;
        fSelEnd := fIndex;
     end;

     if fIndex <> wOldIndex then
     begin
        Try
           DoItemIndexChange;
        except
           //Do nothing...
        end;
        
        ScrollToVisible;
        UpdateVScrollBar;
        UpdateHScrollBar;
        repaint;
     end
     else
     invalidate;

     sleep(1); //fix for scrolling too quick!
  end;
end;

procedure TrmCustomScrollableControl.SetShowFocusRect(const Value: boolean);
begin
  if fShowFocusRect <> Value then
  begin
    fShowFocusRect := Value;
    invalidate;
  end;
end;

function TrmCustomScrollableControl.GetHScrollSize: integer;
begin
  if MaxItemLength - ClientWidth < 0 then
    result := 0
  else
    result := (MaxItemLength - ClientWidth);
end;

function TrmCustomScrollableControl.GetVScrollSize: integer;
begin
  if MaxItemCount - VisibleItems < 0 then
    result := 0
  else
    result := (MaxItemCount - VisibleItems);
end;

function TrmCustomScrollableControl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
   inherited DoMouseWheel(Shift, WheelDelta, MousePos);
   result := true;
   InternalTopIndex := SetInRange(vScrollPos + ((WheelDelta div 120) * -1), 0, VScrollSize);
   UpdateVScrollBar;
   invalidate;
end;

procedure TrmCustomScrollableControl.SetTopIndex(const Value: integer);
begin
  if (MaxItemCount - VisibleItems)-1 < 0 then
    fTopIndex := 0
  else
    fTopIndex := setinrange(Value, 0, (MaxItemCount - VisibleItems));
  UpdateVScrollBar;
  invalidate;
end;

procedure TrmCustomScrollableControl.SetHideSelection(const Value: boolean);
begin
  FHideSelection := Value;
  Invalidate;
end;

procedure TrmCustomScrollableControl.setMultiselect(const Value: boolean);
begin
  fMultiSelect := Value;
  if not fMultiSelect then
  begin
     fSelStart := fIndex;
     fSelEnd := fIndex;
  end;
  invalidate;
end;

procedure TrmCustomScrollableControl.SetSelCount(const Value: integer);
begin
  if fMultiSelect then
     fSelEnd := fSelStart + value
  else
     fSelEnd := fSelStart;
  fIndex := fSelEnd;

  Try
     DoItemIndexChange;
  except
     //Do nothing...
  end;

  Invalidate;
end;

procedure TrmCustomScrollableControl.SetSelStart(const Value: integer);
begin
  fSelStart := Value;
  fSelEnd := Value;
  fIndex := value;

  Try
     DoItemIndexChange;
  except
     //Do nothing...
  end;

  invalidate;
end;

function TrmCustomScrollableControl.GetSelCount: integer;
begin
   Result := fSelEnd - fSelStart;
end;

procedure TrmCustomScrollableControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TrmCustomScrollableControl.SetScrollBars(
  const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

function TrmCustomScrollableControl.MaxItemCount: integer;
begin
   result := 0;
end;

function TrmCustomScrollableControl.MaxItemLength: integer;
begin
   result := 0;
end;

function TrmCustomScrollableControl.MaxItemHeight: integer;
begin
   result := 0;
end;

function TrmCustomScrollableControl.MaxItemWidth: integer;
begin
   result := 0;
end;

procedure TrmCustomScrollableControl.CalcMultiSelect(oldIndex, oldSelStart,
  oldSelEnd:integer);
begin
   if (OldIndex - findex) < 0 then  //Down movement...
   begin
      if (OldIndex = OldSelEnd) then //continue down movement...
      begin
         fSelStart := OldSelStart;
         fSelEnd := fIndex;
      end
      else                             //Start a downmovement...
      begin
         if (fIndex > OldSelEnd) then //possible big down movement
         begin
            if (OldSelEnd <> OldSelStart) then //last selection wasn't a single line...
            begin
               fSelStart := OldSelEnd;
               fSelEnd := fIndex;
            end
            else                                 //last selection was a single line...
            begin
               fSelStart := OldSelStart;
               fSelEnd := fIndex;
            end;
         end
         else                          //nope. only a small down movement...
         begin
            fSelStart := fIndex;
            fSelEnd := OldSelEnd;
         end;
      end;
   end
   else
   begin                             //Up movement...
      if (OldIndex = OldSelStart) then //continue up movement...
      begin
         fSelStart := fIndex;
         fSelEnd := OldSelEnd;
      end
      else                             //Start a up movement...
      begin
         if (fIndex < OldSelStart) then //possible up down movement
         begin
            if (OldSelEnd <> OldSelStart) then //last selection wasn't a single line...
            begin
               fSelStart := fIndex;
               fSelEnd := OldSelStart;
            end
            else                                 //last selection was a single line...
            begin
               fSelStart := fIndex;
               fSelEnd := OldSelEnd;
            end;
         end
         else                          //nope. only a small up movement...
         begin
            fSelStart := OldSelStart;
            fSelEnd := fIndex;
         end;
      end;
   end;
end;

procedure TrmCustomScrollableControl.CMBorderChanged(
  var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TrmCustomScrollableControl.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TrmCustomScrollableControl.HorizontalScrollChange(
  sender: TObject);
begin
  //Do Nothing...
end;

procedure TrmCustomScrollableControl.VerticalScrollChange(sender: TObject);
begin
  //Do Nothing...
end;

procedure TrmCustomScrollableControl.DoItemIndexChange;
begin
  //Do Nothing...
end;

end.

