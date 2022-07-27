{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmInspector
Purpose  : This control is similar to Delphi's properties inspector.
Date     : 01-18-2001
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmInspector;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, rmTreeNonView;

type
  TrmInspector = class;
  TrmCustomInspectorItemClass = class of TrmCustomInspectorItem;
  TmsgEvent = procedure(msg:TMessage) of object;

  TrmInspectorIndexChangingEvent = procedure(CurrentNode, NewNode: TrmTreeNonViewNode; var AllowChange: boolean) of object;

  TrmCustomInspectorItem = class(TComponent)
  private
    fHint: string;
    fInspector : TrmInspector;
    fNode: TrmTreeNonViewNode;
  protected
    function GetStringValue: string; virtual; abstract;
    procedure SetStringValue(const Value: string); virtual; abstract;
  public
    function EditorClass: TWinControlClass; virtual; abstract;
    procedure GetValueFromEditor(Editor: TWinControl); virtual; abstract;
    procedure SetValueIntoEditor(Editor: TWinControl); virtual; abstract;
    procedure SetupEditor(Inspector: TrmInspector; Editor: TWinControl); virtual; abstract;
    property InspectorControl : TrmInspector read fInspector;
    property PathNode : TrmTreeNonViewNode read fNode;
  published
    property AsString: string read GetStringValue write SetStringValue;
    property Hint: string read fHint write fHint;
  end;

  TrmInspector = class(TCustomControl)
  private
    { Private declarations }
    fItems: TrmTreeNonView;
    fTopIndex: integer;
    fEditorFocus: boolean;
    fEditControl: TWinControl;

    fShowFocusRect: boolean;
    fShowVScrollBars: boolean;
    fItemHeight: integer;
    FBorderStyle: TBorderStyle;

    fIndex: integer;
    fCurrentNode: TrmTreeNonViewNode;

    fSplit: integer;
    fSplitMove: boolean;
    fOnIndexChanged: TNotifyEvent;
    fonIndexChanging: TrmInspectorIndexChangingEvent;
    fOnEditorExit: TNotifyEvent;
    fOnEditorCreated: TNotifyEvent;
    fOnEditorEnter: TNotifyEvent;
    fOnEditorKeyUp: TKeyEvent;
    fOnEditorKeyDown: TKeyEvent;
    fOnEditorKeyPress: TKeyPressEvent;
    fToolTip: Boolean;
    fOnkeydown : TKeyEvent;
    fReadonly: boolean;
    fOnComplexEdit: TNotifyEvent;
    fOnWnd: TMsgEvent;

    function vLines: integer;
    function ItemHeight: integer;
    function VisibleItemCount: integer;

    function IsItemVisible(Item:TrmCustomInspectorItem):Boolean;
    function VisibileItemIndex(Item:TrmCustomInspectorItem):integer;

    function IsNodeVisible(Node:TrmTreeNonViewNode):boolean;
    function VisibileNodeIndex(Node:TrmTreeNonViewNode):integer;

    procedure UpdateVScrollBar;
    procedure ScrollToVisible;

    function GetVScrollPos: integer;
    procedure SetVScrollPos(const Value: integer);

    procedure DoNodeTextChanged(Sender:TObject; Node:TrmTreeNonViewNode);

    procedure cmFontChanged(var Msg: TMessage); message cm_fontchanged;
    procedure wmSize(var MSG: TWMSize); message wm_size;
    procedure wmEraseBKGrnd(var msg: tmessage); message wm_erasebkgnd;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetShowFocusRect(const Value: boolean);
    procedure SetShowVScrollBars(const Value: boolean);
    function GetVScrollSize: integer;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure setSplit(const Value: integer);
    function GetSepChar: char;
    procedure SetItems(const Value: TrmTreeNonViewNodes);
    procedure SetSepChar(const Value: char);
    function GetItems: TrmTreeNonViewNodes;
    procedure SetItemIndex(const Value: integer);
    function GetCurrentInspectorItem: TrmCustomInspectorItem;
    procedure SetNewHint(Node: TrmTreeNonViewNode; Rect: TRect; Data: Boolean);
    procedure CMCancelMode(var Message: TMessage); message cm_CancelMode;
    procedure CMMouseLeave(var Message: TMessage); message cm_MouseLeave;
    procedure SetReadonly(const Value: boolean);
    function GetCurrentPath: string;
    procedure SetCurrentPath(const Value: string);
    procedure SetCurrentNode(const Value: TrmTreeNonViewNode);
  protected
    procedure paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetVisibleItem(VisibilityIndex: integer): TrmTreeNonViewNode;
    function GetVisibleItemRect(VisibilityIndex: integer; Data: Boolean): TRect;
    property ShowVScrollBars: boolean read fShowVScrollBars write SetShowVScrollBars default true;
    property ShowFocusRect: boolean read fShowFocusRect write SetShowFocusRect default true;
    procedure DoExit; override;
    procedure DoNodeDelete(Sender: TObject; Node: TrmTreeNonViewNode); virtual;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateEditorSizePos;
  public
    { Public declarations }
    constructor create(aowner: TComponent); override;
    destructor destroy; override;
    procedure DoComplexEdit(Sender:TObject);
    procedure loaded; override;
    procedure AssignItems(Value:TrmInspector);
    function ParentPath(st:string):string;
    function AddInspectorItem(Path: string; Value: string; ClassType: TrmCustomInspectorItemClass): TrmCustomInspectorItem;
    function FindInspectorItem(Path: string): TrmCustomInspectorItem;
    procedure ClearItems;
    Procedure DeleteItem(Path:string);
    procedure WndProc(var Message: TMessage); override;
    property CurrentItemPath:string read GetCurrentPath write SetCurrentPath;
    property CurrentItem: TrmCustomInspectorItem read GetCurrentInspectorItem;
    property CurrentNode:TrmTreeNonViewNode read fCurrentNode write SetCurrentNode;
    property VScrollPos: integer read GetVScrollPos write SetVScrollPos;
    property VScrollSize: integer read GetVScrollSize;
    property Editor: TWinControl read fEditControl;
    property SepChar: char read GetSepChar write SetSepChar;
    property OnWnd : TMsgEvent read fOnWnd write fOnWnd;
  published
    property Align;
    property Font;
    property Readonly: boolean read fReadonly write SetReadonly default false;
    property ToolTip: Boolean read fToolTip write fToolTip default true;
    property TabStop;
    property TabOrder;
    property ItemIndex: integer read fIndex write SetItemIndex default -1;
    property Items: TrmTreeNonViewNodes read GetItems write SetItems;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property SplitPos: integer read fSplit write setSplit;
    property ShowHint;
    property OnItemIndexChanging: TrmInspectorIndexChangingEvent read fonIndexChanging write fOnIndexChanging;
    property OnItemIndexChanged: TNotifyEvent read fOnIndexChanged write fOnIndexChanged;
    property OnComplexEdit: TNotifyEvent read fOnComplexEdit write fOnComplexEdit;
    property OnEditorCreated: TNotifyEvent read fOnEditorCreated write fOnEditorCreated;
    property OnEditorKeyDown: TKeyEvent read fOnEditorKeyDown write fOnEditorKeyDown;
    property OnEditorKeyUp: TKeyEvent read fOnEditorKeyUp write fOnEditorKeyUp;
    property OnEditorKeyPress: TKeyPressEvent read fOnEditorKeyPress write fOnEditorKeyPress;
    property OnEditorExit: TNotifyEvent read fOnEditorExit write fOnEditorExit;
    property OnEditorEnter: TNotifyEvent read fOnEditorEnter write fOnEditorEnter;
  end;

implementation

uses rmlibrary, rmHint, rmInspectorItems;

type
  TWinControlInvasion = class(TWinControl)
  end;

var
  fHint: TrmHintWindow;

{$R *.RES}

{ TrmInspector }

constructor TrmInspector.create(aowner: TComponent);
begin
  inherited;

  ControlStyle := controlstyle + [csopaque];
  height := 200;
  width := 225;
  fItemHeight := -1;
  fIndex := -1;
  fReadonly := false;
  fTopIndex := 0;
  fEditControl := nil;
  fCurrentNode := nil;
  fToolTip := true;
  Canvas.Font.Assign(Font);
  fItems := TrmTreeNonView.Create(nil);
  fItems.OnNodeTextChanged := DoNodeTextChanged;
  fItems.OnDeletion := DoNodeDelete;
  fBorderStyle := bsNone;
  fSplit := 100;
  fShowFocusRect := true;
  fShowVScrollBars := true;
  SepChar := #1;
end;

procedure TrmInspector.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if fShowVScrollBars then
      Style := Style or WS_VSCROLL;

    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

destructor TrmInspector.destroy;
begin
  SetNewHint(nil, rect(0, 0, 0, 0), false);
  fItems.free;
  inherited;
end;

function TrmInspector.GetVScrollPos: integer;
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
    result := 0;
end;

procedure TrmInspector.paint;
var
  lcount, loop: integer;
  wNEdit, wNRect: TRect;
  wObj: TrmCustomInspectorItem;
  wNode: TrmTreeNonViewNode;
  wBmp, wImage: TBitMap;
  wLevel: integer;
  fEditControlAdjusted : boolean;
begin
   wNRect := GetVisibleItemRect(0, false);
   wNEdit := GetVisibleItemRect(0, true);
   wImage := TBitMap.create;
   try
     wImage.Height := clientheight;
     wImage.width := clientwidth;

     wImage.Canvas.brush.Color := clBtnFace;

     if (csdesigning in componentstate) then
     begin
        wImage.Canvas.pen.Style := psDash;
        try
           wImage.Canvas.pen.color := clBtnText;
           wImage.Canvas.Rectangle(clientrect);
        finally
           wImage.Canvas.pen.Style := psSolid;
        end;
     end
     else
        wImage.Canvas.Fillrect(ClientRect);

     wImage.Canvas.Font.assign(Canvas.Font);


     fEditControlAdjusted := false;
     wBmp := TBitMap.create;
     try
       if fitems.Items.Count > 0 then
       begin
         lcount := vLines;
         if lcount + fTopIndex > VisibleItemCount then
           lcount := VisibleItemCount - fTopIndex;
         loop := fTopIndex;
         while loop < fTopIndex + lcount do
         begin
           wNode := GetVisibleItem(loop);
           if assigned(wNode) then
           begin
             wObj := TrmCustomInspectorItem(wNode.Data);
             wLevel := (wNode.Level * 12);
           end
           else
           begin
             wObj := nil;
             wLevel := 0;
           end;

           wImage.Canvas.Brush.Color := clBtnFace;
           wImage.canvas.Font.Color := clBtnText;
           if assigned(wObj) then
           begin
             wImage.Canvas.TextRect(wNRect, 12 + wLevel, wNRect.top, wNode.Text);

             if loop = fIndex then
             begin
               wImage.Canvas.Brush.Color := clWindow;
               wImage.canvas.Font.Color := clWindowText;
             end;
             wImage.Canvas.TextRect(wNEdit, wNEdit.Left, wNEdit.top, wObj.AsString);
           end
           else
           begin
             if assigned(wNode) then
             begin
               wImage.Canvas.TextRect(wNRect, 12 + wLevel, wNRect.top, wNode.Text);
               if loop = fIndex then
               begin
                 wImage.Canvas.Brush.Color := clWindow;
                 wImage.canvas.Font.Color := clWindowText;
               end;
               wImage.Canvas.TextRect(wNEdit, wNEdit.Left, wNEdit.top, '<empty value>');
             end
             else
             begin
               wImage.Canvas.TextRect(wNRect, 12, wNRect.top, '<empty node>');
               if loop = fIndex then
               begin
                 Canvas.Brush.Color := clWindow;
                 canvas.Font.Color := clWindowText;
               end;
               wImage.Canvas.TextRect(wNEdit, wNEdit.Left, wNEdit.top, '<empty value>');
             end;
           end;

           if assigned(wNode) and wNode.HasChildren then
           begin
             if wNode.Expanded then
               wBmp.LoadFromResourceName(HInstance, 'rminspectorcollapse')
             else
               wBmp.LoadFromResourceName(HInstance, 'rminspectorexpand');

             wBmp.TransparentMode := tmAuto;
             wBmp.Transparent := true;
             wImage.Canvas.Draw(wLevel + 1, wNRect.Top + ((ItemHeight div 2) - (wBmp.height div 2)), wbmp);
           end;

           if (loop = fIndex) then
           begin
             wImage.Canvas.Pen.Color := clBtnHighLight;
             if not fEditorFocus then
             begin
               self.Setfocus;
               wBmp.LoadFromResourceName(HInstance, 'rminspectorindicator');
               ReplaceColors(wBmp, clBtnFace, clBtnText);
               wBmp.Transparent := true;
               wBMP.TransparentColor := clBtnFace;
               wImage.Canvas.Draw(0, wNRect.Top + ((ItemHeight div 2) - (wBmp.height div 2)), wbmp);
             end;
             if assigned(fEditControl) then
             begin
                fEditControl.BoundsRect := wNEdit;
                if TWinControlInvasion(fEditControl).Text <> CurrentItem.AsString then
                   TWinControlInvasion(fEditControl).Text := CurrentItem.AsString;
             end;
             fEditControlAdjusted := true;
           end
           else
             wImage.Canvas.Pen.Color := clBtnShadow;

           wImage.Canvas.MoveTo(0, wnrect.Bottom - 1);
           wImage.Canvas.lineto(clientwidth, wnRect.Bottom - 1);

           wImage.Canvas.Pen.Color := clBtnFace;
           wImage.Canvas.MoveTo(0, wnrect.Bottom - 2);
           wImage.Canvas.lineto(clientwidth, wnRect.Bottom - 2);

           wImage.Canvas.Pen.Color := clBtnShadow;
           wImage.canvas.moveto(fsplit - 1, wnRect.Top - 1);
           wImage.canvas.Lineto(fsplit - 1, wnRect.Bottom);

           wImage.Canvas.Pen.Color := clBtnHighlight;
           wImage.canvas.moveto(fsplit, wnRect.Top - 1);
           wImage.canvas.Lineto(fsplit, wnRect.Bottom);

           offsetrect(wNRect, 0, ItemHeight);
           offsetrect(wNEdit, 0, ItemHeight);

           wImage.Canvas.brush.Color := clBtnFace;
           wImage.Canvas.Font.color := clBtnText;

           inc(loop);
         end;
       end;
     finally
       wBmp.free;
     end;
     Canvas.Draw(0, 0, wImage);
     if not fEditControlAdjusted then
     begin
        if assigned(fEditControl) then
           fEditControl.Top := clientheight+1;
     end;
   finally
     wImage.Free;
   end;
end;

procedure TrmInspector.ScrollToVisible;
begin
  if (fTopIndex < fIndex) then
  begin
    if (((fTopIndex + vLines) - 2) < fIndex) then
      fTopIndex := (fIndex - (vLines - 2));
  end
  else if fIndex < fTopIndex then
    fTopIndex := fIndex;

  if fTopIndex < 0 then
  begin
    fTopIndex := 0;
    ItemIndex := 0;
  end;
end;

procedure TrmInspector.SetVScrollPos(const Value: integer);
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

    fTopIndex := SetScrollInfo(Handle, SB_VERT, wScrollInfo, true);
  end
  else
  begin
    fTopIndex := SetInRange(value, 0, VScrollSize);
  end;
  Invalidate;
end;

procedure TrmInspector.UpdateVScrollBar;
var
  wScrollInfo: TScrollInfo;
begin
  if csloading in componentstate then exit;
  if csDestroying in ComponentState then exit;

  fTopIndex := SetInRange(fTopIndex, 0, VScrollSize);

  if fShowVScrollBars then
  begin
    with wScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_POS or SIF_RANGE;
      nMin := 0;
      nMax := VScrollSize;
      nPos := fTopIndex;
    end;

    SetScrollInfo(Handle, SB_VERT, wScrollInfo, True);
    fTopIndex := VScrollPos;
  end;

end;

function TrmInspector.vLines: integer;
begin
  result := ClientHeight div ItemHeight;
  if (clientheight mod itemheight > 0) then
     inc(result);
end;

procedure TrmInspector.wmEraseBKGrnd(var msg: tmessage);
begin
  msg.result := 1;
end;

procedure TrmInspector.wmSize(var MSG: TWMSize);
begin
  UpdatevScrollBar;
  SplitPos := splitPos;
  UpdateEditorSizePos;
  inherited;
end;

procedure TrmInspector.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  case Msg.ScrollCode of
    SB_BOTTOM: fTopIndex := VisibleItemCount - vLines;
    SB_LINEDOWN: inc(fTopIndex);
    SB_LINEUP: dec(fTopIndex);
    SB_TOP: fTopIndex := 0;
    SB_PAGEDOWN: inc(fTopIndex, vLines);
    SB_PAGEUP: dec(fTopIndex, vLines);
    SB_THUMBPOSITION: fTopIndex := Msg.Pos;
    SB_THUMBTRACK: fTopIndex := Msg.Pos;
  else
    exit;
  end;

  UpdateVScrollBar;
  Invalidate;
end;

procedure TrmInspector.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TrmInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  wNode: TrmTreeNonViewNode;
begin
  SetNewHint(nil, rect(0, 0, 0, 0), false);
  if shift = [] then
  begin
    case key of
      vk_tab:
        begin
           if not fReadonly then
           begin
              fEditorFocus := true;
              fEditControl.SetFocus;
           end;
        end;
      vk_DOWN:
        begin
          ItemIndex := ItemIndex + 1;
        end;
      vk_up:
        begin
          if ItemIndex-1 >= 0 then
             ItemIndex := ItemIndex - 1;
        end;
      vk_next:
        begin
          ItemIndex := ItemIndex + (vLines - 1);
        end;
      vk_prior:
        begin
          ItemIndex := ItemIndex - (vLines - 1);
          if ItemIndex < 0 then
             ItemIndex := 0;
        end;
      vk_Left:
        begin
          wNode := GetVisibleItem(findex);
          if assigned(wNode) and wNode.HasChildren then
          begin
            wNode.Expanded := false;
          end;
        end;
      vk_Right:
        begin
          wNode := GetVisibleItem(findex);
          if assigned(wNode) and wNode.HasChildren then
          begin
            wNode.Expanded := true;
          end;
        end;
    else
      inherited;
      exit;
    end;
    key := 0;
    ScrollToVisible;
    UpdateVScrollBar;
    Invalidate;
    exit;
  end
  else
  inherited;
end;

procedure TrmInspector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  wLine: integer;
  wNode: TrmTreeNonViewNode;
  wLevel: integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    SetNewHint(nil, rect(0, 0, 0, 0), false);
    if CanFocus then
      setfocus;

    if (x >= fsplit - 1) and (x <= fsplit + 1) then
    begin
      fSplitMove := true;
    end
    else
    begin
      wLine := fTopIndex + (y div ItemHeight);

      if wLine < VisibleItemCount then
      begin
        wNode := GetVisibleItem(wLine);
        wLevel := (wNode.Level * 12);
        if (x > wLevel) and (x < 12 + wLevel) then
        begin
          if assigned(wNode) and wNode.HasChildren then
          begin
            wNode.Expanded := not wNode.Expanded;
          end;
        end;
        fEditorFocus := (x > fSplit) and not FReadOnly;
        ItemIndex := wLine;
        ScrollToVisible;
        UpdateVScrollBar;
        invalidate;
      end;
    end;
  end;
end;

procedure TrmInspector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  wLine, wyPos, wxPos, wLevel: integer;
  wnvNode: TrmTreeNonViewNode;
  wRect: TRect;
  wData: boolean;
begin
  inherited;
  wLine := fTopIndex + (y div ItemHeight);

  if fSplitMove then
  begin
    if splitpos <> x then
    begin
       splitpos := x;
       UpdateEditorSizePos;
       Repaint;
    end;
  end
  else
  begin
    wnvNode := nil;
    wData := x > SplitPos;
    if wLine < VisibleItemCount then
    begin
      if focused and (Shift = [ssLeft]) then
      begin
        ItemIndex := wLine;
        ScrollToVisible;
        UpdateVScrollBar;
        invalidate;
      end;
      if (Shift = []) and Application.Active then
      begin
        wnvNode := GetVisibleItem(wLine);
        wRect := rect(0, 0, 0, 0);
        if assigned(wnvNode) then
        begin
          wyPos := ((y div itemheight) * itemheight);
          wLevel := ((wnvNode.Level * 12) + 12);
          if wData then
          begin
            if (wnvNode <> fCurrentNode) then
            begin
              wxPos := SplitPos - 1;
              if Canvas.textwidth(TrmCustomInspectorItem(wnvNode.Data).AsString) > (clientwidth - splitpos) then
                wRect := Rect(wxPos, wyPos, wxPos + Canvas.textwidth(TrmCustomInspectorItem(wnvNode.Data).AsString) + 3, wypos + itemheight)
              else
                wnvNode := nil;
            end
            else
              wnvNode := nil;
          end
          else
          begin
            wxPos := wLevel - 2;
            if Canvas.textwidth(wnvNode.text) + wLevel > (splitpos) then
              wRect := Rect(wxpos, wypos, wxpos + Canvas.textwidth(wnvNode.text) + 3, wypos + itemheight)
            else
              wnvNode := nil;
          end;
        end;
      end;
    end;
    SetNewHint(wnvNode, wRect, wData);
  end;
  if (x >= fsplit - 1) and (x <= fsplit + 1) then
    Cursor := crHSplit
  else
    Cursor := crDefault;
end;

procedure TrmInspector.SetShowFocusRect(const Value: boolean);
begin
  if fShowFocusRect <> Value then
  begin
    fShowFocusRect := Value;
    invalidate;
  end;
end;

procedure TrmInspector.SetShowVScrollBars(const Value: boolean);
begin
  if fShowVScrollBars <> Value then
  begin
    fShowVScrollBars := Value;
    recreatewnd;
  end;
end;

function TrmInspector.GetVScrollSize: integer;
begin
  if VisibleItemCount - vLines < 0 then
    result := 0
  else
    result := (VisibleItemCount - vLines)+1;
end;

procedure TrmInspector.cmFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.font.Assign(font);
  fItemHeight := -1;
end;

function TrmInspector.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  wdata: integer;
begin
  inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  result := true;
  wData := (WheelDelta div ItemHeight);
  fTopIndex := SetInRange(vScrollPos + wData, 0, VScrollSize);
  UpdateVScrollBar;
  invalidate;
end;

function TrmInspector.ItemHeight: integer;
var
  wTextMetric: TTextMetric;
begin
  if fItemHeight = -1 then
  begin
    GetTextMetrics(canvas.handle, wTextMetric);
    fItemHeight := wTextMetric.tmHeight + 3;
  end;
  result := fItemHeight
end;

procedure TrmInspector.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

function TrmInspector.VisibleItemCount: integer;
var
  wNode: TrmTreeNonViewNode;
begin
   wNode := fItems.Items.GetFirstNode;
   result := 0;
   while wNode <> nil do
   begin
     inc(result);
     if wNode.Expanded then
       wNode := wNode.GetFirstChild
     else
     begin
       if (wNode.GetNextSibling = nil) then
       begin
         while (wNode <> nil) and (wNode.GetNextSibling = nil) do
           wNode := wNode.Parent;
         if wNode <> nil then
           wNode := wNode.GetNextSibling;
       end
       else
         wNode := wNode.GetNextSibling;
     end;
   end;
end;

function TrmInspector.GetVisibleItem(VisibilityIndex: integer): TrmTreeNonViewNode;
var
  loop: integer;
  wNode: TrmTreeNonViewNode;
begin
  wNode := nil;   
  if Visibilityindex <> -1 then
  begin
     loop := 0;
     wNode := fItems.Items.GetFirstNode;
     while (wNode <> nil) and (loop < VisibilityIndex) do
     begin
       inc(loop);
       if wNode.Expanded then
         wNode := wNode.GetFirstChild
       else
       begin
         if (wNode.GetNextSibling = nil) then
         begin
           while (wNode <> nil) and (wNode.GetNextSibling = nil) do
             wNode := wNode.Parent;
           if wNode <> nil then
             wNode := wNode.GetNextSibling;
         end
         else
           wNode := wNode.GetNextSibling;
       end;
     end;
  end;
  result := wNode;
end;

procedure TrmInspector.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if fSplitMove then
    fSplitMove := false;
end;

procedure TrmInspector.setSplit(const Value: integer);
begin
  try
     fsplit := SetInRange(value, 15, clientwidth - 15);
  except
     fsplit := 15;
  end;
  invalidate;
end;

function TrmInspector.GetSepChar: char;
begin
  result := fItems.SepChar;
end;

procedure TrmInspector.SetItems(const Value: TrmTreeNonViewNodes);
begin
  fItems.Items.Assign(Value);
  if csDesigning in Componentstate then
     Paint
  else
     Paint;
end;

procedure TrmInspector.SetSepChar(const Value: char);
begin
  if value <> fItems.SepChar then
    fItems.SepChar := value;
end;

function TrmInspector.GetItems: TrmTreeNonViewNodes;
begin
  Result := fItems.Items;
end;

procedure TrmInspector.SetItemIndex(const Value: integer);
var
  wNode1, wNode2: TrmTreeNonViewNode;
  wObj1, wObj2: TrmCustomInspectorItem;
  wAllowChange: boolean;
  wValue: integer;
  wRect: TRect;
begin
  if fIndex <> value then
  begin
    wNode1 := GetVisibleItem(fIndex);
    wValue := SetInRange(Value, -1, VisibleItemCount - 1);
    wNode2 := GetVisibleItem(wValue);
    if wNode1 <> wNode2 then
    begin
      wAllowChange := true;
      if assigned(fonIndexChanging) then
        fonIndexChanging(wNode1, wNode2, wAllowChange);
      if wAllowChange then
      begin
        fCurrentNode := nil;
        if assigned(wNode1) then
          wObj1 := TrmCustomInspectorItem(wNode1.Data)
        else
          wObj1 := nil;

        if assigned(wNode2) then
          wObj2 := TrmCustomInspectorItem(wNode2.Data)
        else
          wObj2 := nil;

        if assigned(wObj1) then
        begin
          try
            if not fReadOnly then
               wObj1.GetValueFromEditor(fEditControl);
            FreeAndNil(fEditControl);
          except
            wObj1.SetValueIntoEditor(fEditControl);
            raise;
          end;
        end;

        if assigned(wObj2) then
        begin
          fEditControl := wobj2.EditorClass.Create(self);
          fEditControl.parent := self;
          fEditControl.Visible := false;
          if assigned(fOnEditorCreated) then
          try
            fOnEditorCreated(self);
          except
          end;
          fOnkeydown := TWinControlInvasion(fEditControl).OnKeyDown;
          TWinControlInvasion(fEditControl).OnKeyDown := EditorKeyDown;

          wObj2.SetupEditor(self, fEditControl);
          wObj2.SetValueIntoEditor(fEditControl);

          FCurrentNode := wNode2;

          wRect := GetVisibleItemRect(wValue, True);
          dec(wRect.Bottom, 2);
          fEditControl.BoundsRect := wRect;
          if fReadOnly then
          begin
             fEditControl.Visible := false;
             fEditorFocus := false;
          end
          else
             fEditControl.Visible := true;

          if fEditorFocus and (fEditControl.CanFocus) then
            fEditControl.SetFocus;
        end;

        fIndex := wValue;

        if assigned(fOnIndexChanged) then
        try
          fOnIndexChanged(self);
        except
        end;
        invalidate;
      end;
    end;
  end;
end;

function TrmInspector.AddInspectorItem(Path: string; Value: string; ClassType: TrmCustomInspectorItemClass): TrmCustomInspectorItem;
var
  wObj: TrmCustomInspectorItem;
  wNode : TrmTreeNonViewNode;
begin
  result := nil;
  wNode := fItems.FindPathNode(path);
  if not assigned(wNode) then
  begin
     wNode := fItems.AddPathNode(nil, Path);
     try
       wObj := ClassType.create(self);
       wObj.fInspector := Self;
       wObj.AsString := Value;
       wObj.fNode := wNode;
       wNode.Data := wobj;
       result := wObj;
       Invalidate;
     except
       on E:Exception do
       begin
          wNode.Delete;
          showmessage(E.messagE);
       end;
     end;
  end
  else
  raise EAbort.create('The specified path item already exists');
end;

function TrmInspector.FindInspectorItem(Path: string): TrmCustomInspectorItem;
var
   wItem : TrmTreeNonViewNode;
begin
  wItem := fItems.FindPathNode(Path);
  if assigned(wItem) then
     result := TrmCustomInspectorItem(wItem.Data)
  else
     result := nil;
end;

procedure TrmInspector.DoExit;
var
  wNode: TrmTreeNonViewNode;
  wObj: TrmCustomInspectorItem;
begin
  if assigned(fEditControl) and fEditControl.visible then
  begin
    wNode := GetVisibleItem(fIndex);
    if assigned(wNode) and assigned(wNode.Data) then
    begin
      wObj := TrmCustomInspectorItem(wNode.Data);
      try
        wObj.GetValueFromEditor(fEditControl);
      except
        fEditControl.SetFocus;
        raise;
      end;
    end;
  end;
  inherited;

end;

procedure TrmInspector.loaded;
begin
  inherited;
  UpdatevScrollBar;
  Invalidate;
end;

procedure TrmInspector.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case key of
      vk_tab:
        begin
          key := 0;
          fEditorFocus := false;
          self.SetFocus;
          Invalidate;
        end;
      vk_up:
        begin
          key := 0;
          if itemindex-1 >= 0 then
             ItemIndex := itemindex - 1;
        end;
      vk_down:
        begin
          key := 0;
          ItemIndex := itemindex + 1;
        end;
    end;
  end;

  if assigned(fOnKeyDown) then
     fOnkeydown(Sender, key, shift);
end;

function TrmInspector.GetCurrentInspectorItem: TrmCustomInspectorItem;
begin
  if assigned(fcurrentNode) then
     result := TrmCustomInspectorItem(fCurrentNode.Data)
  else
     result := nil;
end;

procedure TrmInspector.SetNewHint(Node: TrmTreeNonViewNode; Rect: TRect; Data: boolean);
begin
  if fToolTip and assigned(Node) then
  begin
    if assigned(fHint) then
    begin
      if (not Data and (Node.text = fHint.Caption)) or
        (Data and (TrmCustomInspectorItem(Node.Data).AsString = fHint.Caption)) then
        exit
    end
    else
    begin
      fHint.free;
      fHint := nil;
    end;

    if assigned(Node) then
    begin
      Rect.TopLeft := Self.ClientToScreen(Rect.TopLeft);
      Rect.BottomRight := Self.ClientToScreen(Rect.BottomRight);

      if not assigned(fHint) then
        fHint := TrmHintWindow.Create(nil);
      fHint.Color := clInfoBk;
      fHint.Font.Assign(Canvas.font);
      if Data then
        fHint.ActivateHint(Rect, TrmCustomInspectorItem(Node.Data).AsString)
      else
        fHint.ActivateHint(Rect, Node.Text);
    end;
  end
  else
  begin
    fHint.free;
    fHint := nil;
  end;
end;

procedure TrmInspector.CMCancelMode(var Message: TMessage);
begin
  try
    SetNewHint(nil, rect(0, 0, 0, 0), false);
  except
      //Do Nothing...
  end;
  inherited;
end;

procedure TrmInspector.CMMouseLeave(var Message: TMessage);
begin
  try
    SetNewHint(nil, rect(0, 0, 0, 0), false);
  except
      //Do Nothing...
  end;
  inherited;
end;

function TrmInspector.GetVisibleItemRect(VisibilityIndex: integer; Data: Boolean): TRect;
var
  wRect: TRect;
begin
  if Data then
    wRect := rect(fSplit + 2, 0, clientwidth, itemHeight-1)
  else
    wRect := rect(0, 0, fSplit - 2, ItemHeight);

  offsetRect(wRect, 0, itemheight * visibilityindex);
  result := wRect;
end;

procedure TrmInspector.UpdateEditorSizePos;
var
   wRect : TRect;
begin
  if assigned(fEditControl) then
  begin
     wRect := GetVisibleItemRect(fIndex, true);
     dec(wRect.Bottom);
     fEditControl.BoundsRect := wRect;
  end;
end;

procedure TrmInspector.SetReadonly(const Value: boolean);
begin
  fReadonly := Value;
  if assigned(fEditControl) then
  begin
     fEditControl.Visible := not fReadOnly;
     if fReadOnly then
     begin
        fEditorFocus := not fReadOnly;
        Setfocus;
        invalidate;
     end
  end;
end;

procedure TrmInspector.AssignItems(Value: TrmInspector);
begin
   Items.assign(Value.Items);
   Invalidate;
end;

function TrmInspector.GetCurrentPath: string;
begin
   if assigned(fCurrentNode) then
      result := fCurrentNode.NodePath
   else
      result := '';
end;

procedure TrmInspector.SetCurrentPath(const Value: string);
var
   wNode : TrmTreeNonViewNode;
begin
   wNode := fItems.FindPathNode(value);
   if assigned(wNode) then
      SetCurrentNode(wNode)
   else
      raise exception.create('Unable to find Inspector Item');
end;

function TrmInspector.ParentPath(st: string): string;
var
   wstr : string;
begin
   wstr := '';
   if pos(sepchar, st) = 1 then
      delete(st, 1, 1);
   while pos(SepChar, st) > 0 do
   begin
      wstr := wstr + copy(st, 1, pos(SepChar, st)-1);
      delete(st, 1, pos(sepchar, st));
   end;
   result := sepchar+wstr;
end;

procedure TrmInspector.ClearItems;
begin
   fItems.Items.Clear;
   fIndex := -1;
   fTopIndex := 0;
   Invalidate;  
end;

procedure TrmInspector.DeleteItem(Path: string);
var
   wNode : TrmTreeNonViewNode;
begin
  wNode := fItems.FindPathNode(Path);
  if assigned(wNode) then
  begin
     if Path = CurrentItemPath then
        fIndex := -1;
     wNode.Delete;
     Invalidate;
  end;
end;

procedure TrmInspector.DoNodeDelete(Sender: TObject;
  Node: TrmTreeNonViewNode);
var
   wItem : TrmCustomInspectorItem;
begin
   if assigned(Node) and assigned(Node.Data) then
   begin
      if Node = fCurrentNode then
      begin
         fCurrentNode := nil;
         if assigned(fOnIndexChanged) then
         try
            fOnIndexChanged(self);
         except
         end;
      end;
         
      wItem := TrmCustomInspectorItem(Node.Data);
      Node.Data := nil;
      wItem.free;
   end;
end;

procedure TrmInspector.DoComplexEdit(Sender: TObject);
begin
   if assigned(fOnComplexEdit) then
      fOnComplexEdit(fEditControl);
end;

procedure TrmInspector.SetCurrentNode(const Value: TrmTreeNonViewNode);
var
   wstr : string;
   wNode : TrmTreeNonViewNode;
begin
   wstr := fItems.NodePath(Value);
   if assigned(Value) and (wstr <> '') then
   begin
      fCurrentNode := Value;
      if not IsNodeVisible(fCurrentNode) then
      begin
         wNode := fCurrentNode;
         while wNode <> nil do
         begin
            wNode := wNode.parent;
            if wNode <> nil then
               wNode.Expanded := true; 
         end;
      end;
      ItemIndex := VisibileNodeIndex(fCurrentNode);
   end;
end;

function TrmInspector.IsItemVisible(Item: TrmCustomInspectorItem): Boolean;
begin
   if assigned(Item) and (Item.InspectorControl = self) then
      result := IsNodeVisible(Item.PathNode)
   else
      result := false;
end;

function TrmInspector.IsNodeVisible(Node: TrmTreeNonViewNode): boolean;
begin
   if assigned(Node) then
   begin
      Result := true;
      while result and (Node <> nil) do
      begin
         node := node.parent;
         if Node <> nil then
            result := result and node.Expanded;
      end;
   end
   else
      result := false;
end;

function TrmInspector.VisibileItemIndex(Item: TrmCustomInspectorItem): integer;
begin
   if assigned(Item) and (Item.InspectorControl = self) then
      result := VisibileNodeIndex(Item.PathNode)
   else
      result := -1;
end;

function TrmInspector.VisibileNodeIndex(Node: TrmTreeNonViewNode): integer;
var
  wNode: TrmTreeNonViewNode;
begin
   result := 0;
   wNode := fItems.Items.GetFirstNode;
   while (wNode <> nil) and (wNode <> Node) do
   begin
     inc(result);
     if wNode.Expanded then
       wNode := wNode.GetFirstChild
     else
     begin
       if (wNode.GetNextSibling = nil) then
       begin
         while (wNode <> nil) and (wNode.GetNextSibling = nil) do
           wNode := wNode.Parent;
         if wNode <> nil then
           wNode := wNode.GetNextSibling;
       end
       else
         wNode := wNode.GetNextSibling;
     end;
   end;
   if wNode = nil then
      result := -1;
end;

procedure TrmInspector.DoNodeTextChanged(Sender: TObject;
  Node: TrmTreeNonViewNode);
begin
   Invalidate;
end;

procedure TrmInspector.WndProc(var Message: TMessage);
begin
  inherited;
  if assigned(fOnWnd) then
     fOnWnd(message);
end;

initialization
   RegisterClass(TrmCustomInspectorItem);

end.

