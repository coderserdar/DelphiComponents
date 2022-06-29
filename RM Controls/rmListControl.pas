{================================================================================
Copyright (C) 1997-2001 Mills Enterprise

Unit     : rmListControl
Purpose  : This unit was created for use in the rmDiff controls and has been
           found to be usefull in other areas.  Basically it's a listbox with
           a few interesting properties.
Date     : 06-24-2000
Author   : Ryan J. Mills
Version  : 1.80
================================================================================}

unit rmListControl;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormatDrawingEvent = procedure(Sender:TObject; Canvas: TCanvas; Selected: boolean; var str: string) of object;
  TScrollEvent = procedure(Sender:TObject; ScrollBar:integer) of object;

  TrmListControl = class(TCustomControl)
  private
    { Private declarations }
    fItems: TStringList;
    fIndex: integer;
    fTopIndex: integer;
    fLongest: integer;
    fxPos: integer;
    fOnScroll: TScrollEvent;
    fFormatDrawing: TFormatDrawingEvent;
    fShowFocusRect: boolean;
    fShowHScrollBars: boolean;
    fShowVScrollBars: boolean;

    procedure SetItems(const Value: TStringList);
    function vLines: integer;
    procedure ItemsChanged(Sender: TObject);
    function LLLength: integer;
    procedure setIndex(const Value: integer);

    procedure UpdateVScrollBar;
    procedure UpdateHScrollBar;
    procedure ScrollToVisible;

    function GetHScrollPos: integer;
    function GetVScrollPos: integer;
    procedure SetHScrollPos(const Value: integer);
    procedure SetVScrollPos(const Value: integer);

    procedure cmFOCUSCHANGED(var MSG: TMessage); message CM_FOCUSCHANGED;
    procedure cmFontChanged(var Msg:TMessage); message cm_fontchanged;
    procedure wmSize(var MSG: TWMSize); message wm_size;
    procedure wmEraseBKGrnd(var msg: tmessage); message wm_erasebkgnd;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetShowFocusRect(const Value: boolean);
    procedure SetShowVScrollBars(const Value: boolean);
    procedure SetShowHScrollBars(const Value: boolean);
    function GetHScrollSize: integer;
    function GetVScrollSize: integer;
    procedure SetTopIndex(const Value: integer);
  protected
    procedure paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    property InternalTopIndex : integer read fTopIndex write SetTopIndex;
  public
    { Public declarations }
    constructor create(aowner: TComponent); override;
    destructor destroy; override;
    property Items: TStringList read fItems write SetItems;
    property ItemIndex: integer read fIndex write setIndex default 0;
    property VScrollPos: integer read GetVScrollPos write SetVScrollPos;
    property HScrollPos: integer read GetHScrollPos write SetHScrollPos;
    property VScrollSize: integer read GetVScrollSize;
    property HScrollSize: integer read GetHScrollSize;
  published
    property Align;
    property Font;
    property ShowVScrollBars: boolean read fShowVScrollBars write SetShowVScrollBars default true;
    property ShowHScrollBars: boolean read fShowHScrollBars write SetShowHScrollBars default true;
    property ShowFocusRect: boolean read fShowFocusRect write SetShowFocusRect default true;
    property OnScroll: TScrollEvent read fOnScroll write fOnScroll;
    property OnFormatDrawing: TFormatDrawingEvent read fFormatDrawing write fFormatDrawing;
  end;

implementation

uses Math, rmlibrary;

{ TrmListControl }

constructor TrmListControl.create(aowner: TComponent);
begin
  inherited;

  ControlStyle := controlstyle + [csopaque];
  height := 200;
  width := 400;
  fLongest :=-1;
  fIndex := 0;
  fTopIndex := 0;
  fItems := TStringList.Create;
  fItems.OnChange := ItemsChanged;
  fXPos := 0;
  fShowFocusRect := true;
  fShowVScrollBars := true;
  fShowHScrollBars := true;
end;

procedure TrmListControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if fShowVScrollBars then
    Params.style := Params.style or WS_VSCROLL;

  if fShowHScrollBars then
    Params.style := Params.style or WS_HSCROLL;
end;

destructor TrmListControl.destroy;
begin
  fItems.free;
  inherited;
end;

function TrmListControl.GetHScrollPos: integer;
var
  wScrollInfo: TScrollInfo;
begin
  if fShowHScrollBars then
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

function TrmListControl.GetVScrollPos: integer;
var
  wScrollInfo: TScrollInfo;
begin
  if fShowVScrollBars then
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

procedure TrmListControl.ItemsChanged(Sender: TObject);
begin
  fIndex := 0;
  InternalTopIndex := 0;

  UpdateVScrollBar;
  UpdateHScrollBar;
  Invalidate;
end;

function TrmListControl.LLLength: integer;
var
  loop: integer;
begin
  if (fLongest = -1) and (fItems.count > 0) then
  begin
    for loop := 0 to fItems.count - 1 do
      fLongest := Max(fLongest, Canvas.TextWidth(fItems[loop]));
  end;
  result := fLongest;
end;

procedure TrmListControl.paint;
var
  lcount, loop: integer;
  wRect: TRect;
  wstr: string;
begin
  Canvas.brush.Color := clWindow;
  wRect := rect(0, 0, ClientWidth, Canvas.textheight('X'));
  if fitems.count > 0 then
  begin
    lcount := vLines;
    if lcount + InternalTopIndex > fitems.Count then
      lcount := fitems.count - InternalTopIndex;
    loop := InternalTopIndex;
    while loop < InternalTopIndex + lcount do
    begin
      if loop = fIndex then
      begin
        if Focused then
        begin
           Canvas.brush.Color := clHighlight;
           Canvas.Font.color := clHighlightText;
        end
        else
        begin
           Canvas.brush.Color := clBtnFace;
           Canvas.Font.color := clWindowText;
        end;
      end;

      wstr := fItems[loop];

      if Assigned(fFormatDrawing) then
        fFormatDrawing(Self, Canvas, (loop = fIndex), wstr);

      Canvas.TextRect(wRect, -HScrollPos, wRect.top, wstr);

      if Focused and fShowFocusRect and (loop = fIndex) then
        Canvas.DrawFocusRect(wRect);

      offsetrect(wrect, 0, canvas.textheight('X'));

      Canvas.brush.Color := clWindow;
      Canvas.Font.color := clWindowText;

      inc(loop);
    end;
  end;
  wRect.Bottom := ClientHeight;
  Canvas.FillRect(wRect);
end;

procedure TrmListControl.ScrollToVisible;
begin
  if (InternalTopIndex < fIndex) then
  begin
    if (InternalTopIndex + (vLines - 1) < fIndex) then
      InternalTopIndex := (fIndex - (vLines - 1));
  end
  else if fIndex < InternalTopIndex then
    InternalTopIndex := fIndex;

  if InternalTopIndex < 0 then
  begin
    InternalTopIndex := 0;
    fIndex := 0;
  end;
end;

procedure TrmListControl.SetHScrollPos(const Value: integer);
var
  wScrollInfo: TScrollInfo;
begin
  if fShowHScrollBars then
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

procedure TrmListControl.setIndex(const Value: integer);
begin
  if fitems.count > 0 then
  begin
     fIndex := SetInRange(Value, 0, fItems.count-1);
     ScrollToVisible;
     Invalidate;
  end;
end;

procedure TrmListControl.SetItems(const Value: TStringList);
begin
  fItems.assign(Value);
end;

procedure TrmListControl.SetVScrollPos(const Value: integer);
var
  wScrollInfo: TScrollInfo;
begin
  if fShowVScrollBars then
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

procedure TrmListControl.UpdateVScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  InternalTopIndex := SetInRange(InternalTopIndex, 0, VScrollSize);

  if fShowVScrollBars then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS or SIF_RANGE or SIF_DISABLENOSCROLL;
      nMin := 0;
      nMax := VScrollSize;
      nPos := InternalTopIndex;
    end;

    SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
    InternalTopIndex := VScrollPos;
  end;
  
  if assigned(fOnScroll) then
    fOnScroll(self, SB_VERT);
end;

procedure TrmListControl.UpdateHScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  fxPos := SetInRange(fxPos, 0, HScrollSize);

  if fShowHScrollBars then
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

  if assigned(fOnScroll) then
    fOnScroll(self, SB_HORZ);
end;


function TrmListControl.vLines: integer;
begin
  result := ClientHeight div canvas.TextHeight('X');
end;

procedure TrmListControl.wmEraseBKGrnd(var msg: tmessage);
begin
  msg.result := 1;
end;

procedure TrmListControl.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: fxPos := LLLength - clientwidth;
    SB_LINEDOWN: inc(fxPos, canvas.TextWidth('X'));
    SB_LINEUP: dec(fxPos, canvas.TextWidth('X'));
    SB_TOP: fxPos := 0;
    SB_PAGEDOWN: inc(fxPos, (LLLength - clientwidth) div 2);
    SB_PAGEUP: dec(fxPos, (LLLength - clientwidth) div 2);
    SB_THUMBPOSITION: fxPos := Msg.Pos;
    SB_THUMBTRACK: fxPos := Msg.Pos;
  else
    exit;
  end;

  UpdateHScrollBar;
  Invalidate;
end;

procedure TrmListControl.wmSize(var MSG: TWMSize);
begin
  UpdatevScrollBar;
  UpdateHScrollBar;
  inherited;
end;

procedure TrmListControl.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: InternalTopIndex := fItems.Count - vLines;
    SB_LINEDOWN: InternalTopIndex := InternalTopIndex+1;
    SB_LINEUP: InternalTopIndex := InternalTopIndex-1;
    SB_TOP: InternalTopIndex := 0;
    SB_PAGEDOWN: InternalTopIndex := InternalTopIndex+vLines;
    SB_PAGEUP: InternalTopIndex := InternalTopIndex-vLines;
    SB_THUMBPOSITION: InternalTopIndex := Msg.Pos;
    SB_THUMBTRACK: InternalTopIndex := Msg.Pos;
  else
    exit;
  end;

  UpdateVScrollBar;
  Invalidate;
end;

procedure TrmListControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TrmListControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case key of
    vk_end:
      begin
        if shift = [] then
          fIndex := fItems.count - 1;
      end;
    vk_DOWN:
      begin
        if shift = [] then
          inc(fIndex);
      end;
    vk_up:
      begin
        if shift = [] then
          dec(fIndex);
      end;
    vk_home:
      begin
        if shift = [] then
          fIndex := 0;
      end;
    VK_Right:
      begin
        if shift = [] then
          inc(fxPos)
        else if shift = [ssCTRL] then
          fxPos := LLLength - ClientWidth;
      end;
    VK_LEFT:
      begin
        if shift = [] then
          dec(fxPos)
        else if shift = [ssCTRL] then
          fxPos := 0;
      end;
    vk_next:
      begin
        if shift = [] then
          inc(fIndex, vLines - 1);
      end;
    vk_prior:
      begin
        if shift = [] then
          Dec(fIndex, vLines - 1);
      end;
  else
    exit;
  end;

  fIndex := SetInRange(fIndex, 0, fItems.count-1);
  ScrollToVisible;
  UpdateVScrollBar;
  UpdateHScrollBar;

  Invalidate;
end;

procedure TrmListControl.cmFOCUSCHANGED(var MSG: TMessage);
begin
  inherited;
  invalidate;
end;

procedure TrmListControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  wLine: integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    if CanFocus then
      setfocus;
    wLine := InternalTopIndex + (y div Canvas.TextHeight('X'));
    if wLine < fItems.count then
    begin
      fIndex := wLine;
      ScrollToVisible;
      UpdateVScrollBar;
      invalidate;
    end;
  end;
end;

procedure TrmListControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  wLine: integer;
begin
  inherited;
  if focused and (Shift = [ssLeft]) then
  begin
    wLine := InternalTopIndex + (y div Canvas.TextHeight('X'));
    if wLine < fItems.count then
    begin
      fIndex := wLine;
      ScrollToVisible;
      UpdateVScrollBar;
      invalidate;
    end;
  end;
end;

procedure TrmListControl.SetShowFocusRect(const Value: boolean);
begin
  if fShowFocusRect <> Value then
  begin
    fShowFocusRect := Value;
    invalidate;
  end;
end;

procedure TrmListControl.SetShowVScrollBars(const Value: boolean);
begin
  if fShowVScrollBars <> Value then
  begin
    fShowVScrollBars := Value;
    recreatewnd;
  end;
end;

function TrmListControl.GetHScrollSize: integer;
begin
  if LLLength - ClientWidth < 0 then
    result := 0
  else
    result := (LLLength - ClientWidth);
end;

function TrmListControl.GetVScrollSize: integer;
begin
  if fItems.Count - vLines < 0 then
    result := 0
  else
    result := (fItems.Count - vLines);
end;

procedure TrmListControl.SetShowHScrollBars(const Value: boolean);
begin
  if fShowHScrollBars <> Value then
  begin
    fShowHScrollBars := Value;
    recreatewnd;
  end;
end;

procedure TrmListControl.cmFontChanged(var Msg: TMessage);
begin
   inherited;
   Canvas.font.Assign(font);
end;

function TrmListControl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
   wdata : integer;
begin
   inherited DoMouseWheel(Shift, WheelDelta, MousePos);
   result := true;
   wData := (WheelDelta div canvas.textheight('X'));
   InternalTopIndex := SetInRange(vScrollPos + wData, 0, VScrollSize);
   UpdateVScrollBar;
   invalidate;
end;

procedure TrmListControl.SetTopIndex(const Value: integer);
begin
  fTopIndex := Value;
end;

end.

