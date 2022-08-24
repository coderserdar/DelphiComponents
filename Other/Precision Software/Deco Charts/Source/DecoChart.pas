{------------------------------------------------------------------------------
  DecoChart.pas

  DecoCharts for VCL

  written by  Precision software & consulting
              e-mail: info@be-precision.com
              web: http://www.be-precision.com

  Purpose:    TCustomDecoChart implements a basic functionality for descendant
              charting components.

  The source code is given as is. The author is not responsible
  for any possible damage done due to the use of this code.
  You can freely use this component in your products, if you have purchased
  the license. Except where otherwise noted, the complete source code remains
  property of the author and may not be distributed, published, given or sold
  in any form as such. No parts of the source code can be included in any
  other component or application without written authorization of the author.

  Copyright (c) 2008-2013  Precision software & consulting
  All rights reserved
------------------------------------------------------------------------------}

{ Change log:

  Version 1.3 (2013-11-18)
  - added: Support for Delphi XE3/XE4/XE5
  - improved: Minor improvements and fixes

  Version 1.2 (2012-03-11)
  - The first release
}

{ This unit contains TCustomDecoChart component declaration and implementation.
  There is also declaration of general charting data classes, such as TDecoChartItem,
  TDecoChartItems and TSortedDecoChartItems. }
unit DecoChart;

interface

uses
  Classes, Windows, Messages, Graphics, Controls, Forms, SysUtils,
  DecoCommon;

type
  TCustomDecoChart = class;

  { A base class for chart items (grid cells, rows, value items, etc.) }
  TDecoChartItem = class(TCollectionItem)
  private
    FCaption: string;
    FHint: string;
    FTag: Longint;
    FData: Pointer;
    FVisible: Boolean;
    // Calculated bounds of an item
    FItemRect: TRect;
  protected
    function GetDisplayName: string; override;
    procedure SetCaption(const Value: string); virtual;
    procedure SetHint(const Value: string); virtual;
    procedure SetVisible(Value: Boolean); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    // Caption of the item
    property Caption: string read FCaption write SetCaption;
    // Any custom data reference
    property Data: Pointer read FData write FData;
    // Hint for the item
    property Hint: string read FHint write SetHint;
    // Current bounds of an item
    property ItemRect: TRect read FItemRect write FItemRect;
    // Show or hide the item by using this property
    property Visible: Boolean read FVisible write SetVisible default True;
  published
    // Any custom tag value (integer type)
    property Tag: Longint read FTag write FTag default 0;
  end;

  // Event handler for custom sorting. If no handler is assigned, internal sorting will be used (by the caption in an ascending order).
  TDecoChartCompareEvent = function(Sender:TObject; Item1, Item2:TCollectionItem):integer of object;
  // Common event handler type, used for various item events (OnSelectItem, OnHoverChange, OnItemClick, etc.)
  TDecoChartItemEvent = procedure(Sender:TObject; Item:TDecoChartItem) of object;

  { Collection of chart items. See the description of TDecoChartItem for more info. See also TSortedDecoChartItems. }
  TDecoChartItems = class(TOwnedCollection)
  private
    FDecoChart: TCustomDecoChart;
  protected
    procedure Update(Item: TCollectionItem); override;
    {$IF CompilerVersion >= 15.0}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Deleting(Item: TCollectionItem);
    {$IFEND}
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass); virtual;
    { Returns the next deco chart item. If Item argument is nil, the function returns the first item.
      If VisibleOnly argument is True, the function looks for visible items only. }
    function GetNextItem(Item: TDecoChartItem; VisibleOnly:Boolean = False): TDecoChartItem;
    { Returns the prior deco chart item. If Item argument is nil, the function returns the last item.
      If VisibleOnly argument is True, the function looks for visible items only. }
    function GetPriorItem(Item: TDecoChartItem; VisibleOnly:Boolean = False): TDecoChartItem;
    // Reference to the owner component
    property DecoChart: TCustomDecoChart read FDecoChart;
  end;

  { Collection of chart items, that supports sorting. See the description of TDecoChartItem for more info. See also TDecoChartItems. }
  TSortedDecoChartItems = class(TDecoChartItems)
  private
    // Direct reference to the private FItems field of ancestor (TCollection). Used for fast sorting of items (see DecoCommon.DecoUseSpeedSort variable).
    FItemsRef: TList;
    FSorting: Boolean;
    FAutoSort: Boolean;
    FOnCompareItems: TDecoChartCompareEvent;
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure QuickSort(L, R: Integer);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass); override;
    // Creates and inserts a new item to the specified position in the collection
    function Insert(Index:Integer): TCollectionItem;
    // Moves an item to another position in the collection. Moved item is specified by CurrIndex parameter.
    procedure Move(CurrIndex, NewIndex: Integer);
    // Sorts the items collection alpabetically (or with the help of custom event handler - OnCompareItems)
    procedure Sort; virtual;
    // If True, then sorting is performed automatically on every Update call, if the owner chart has the Sorted property also set to True.
    property AutoSort:Boolean read FAutoSort write FAutoSort;
    { Event handler, that allows you to perform your own sorting mechanism for items. }
    property OnCompareItems: TDecoChartCompareEvent read FOnCompareItems write FOnCompareItems;
  end;

  { TCustomDecoChart implements a basic functionality for descendant chart components, such as TCustomDecoCompareGrid. }
  TCustomDecoChart = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FGlowSize: Integer;
    FHoverCursor: TCursor;
    FParentBackgroundSet: Boolean;
    FOnAfterPaint: TDecoPaintEvent;
    FOnBeforePaint: TDecoPaintEvent;
    FShowFocusRect: Boolean;
    FSorted: Boolean;
    FTextMargin: Integer;
    FUpdateCount: Integer;
    FCaptionFont: TFont;
    FShowCaption: Boolean;
    {$IF CompilerVersion >= 20.0}
    procedure DoPaddingChange(Sender: TObject);
    {$IFEND}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMBorderChanged(var Message: TMessage); message CM_BORDERCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: {$IF CompilerVersion >= 20.0} TCMParentFontChanged {$ELSE} TMessage {$IFEND}); message CM_PARENTFONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TWMKey); message CM_WANTSPECIALKEY;
    procedure SetCaptionFont(const Value: TFont);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetGlowSize(Value: Integer);
    procedure SetOnAfterPaint(Value: TDecoPaintEvent);
    procedure SetOnBeforePaint(Value: TDecoPaintEvent);
    procedure SetSorted(Value: Boolean);
    procedure SetTextMargin(Value: Integer);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
  protected
    // Temporary variable, that indicates a drawing onto the vector based canvas (ie. TMetafileCanvas). It is used in PrintTo method.
    _EMFExporting:Boolean;
    // A temporary variable that indicates if the component has a focus
    FActive:Boolean;
    // Reference to the helper function for drawing the texts
    FDrawTextFunc:TDrawTextFunc;
    // Temporary variable that indicates the need of recalculating the successor drawing rectangles (bars, captions, items, etc.)
    FRebuildNeeded: Boolean;
    { Helper variable - a margin between texts and graphic elements. It is based on a TextMargin property,
      or (if TextMargin is set to -1) the currently selected font metrics. }
    FSpacing: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CaptionFontChange(Sender: TObject);
    procedure SetParentBackground(Value: Boolean); override;
    function IsEmpty:Boolean; virtual;
    function IsAnyItemSelected:Boolean; virtual;
    function IsItemSelected(const Item:TCollectionItem):Boolean; virtual;
    procedure SelectFirstItem; virtual;
    function GetFocusRect:TRect; virtual;
    procedure CancelSelection; virtual;
    function IsAnyItemHovered:Boolean; virtual;
    function IsItemHovered(const Item:TCollectionItem):Boolean; virtual;
    procedure CancelHover; virtual;
    procedure GetHoverHintInfo(var aHint:string; var aRect:TRect); virtual;
    procedure DrawChart(TargetCanvas:TCanvas); virtual;
    procedure DrawFocus(TargetCanvas:TCanvas; aRect:TRect); virtual;
    { Set this property to True to draw the component's background as transparent.
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoCompareGridPlus) it is. }
    property ParentBackground stored FParentBackgroundSet default False;
    // Base method for drawing the component, that calls other methods to draw the bars, texts, etc.
    procedure Paint; override;
    // Holds the increment of "block repaint" calls made by BeginUpdate / EndUpdate methods.
    property UpdateCount: Integer read FUpdateCount;
  public
    // Constructor of the component. You can call it directly, when creating the component in run-time.
    constructor Create(AOwner: TComponent); override;
    // Component destructor. You don't have to call it directly, unless you want to destroy the component at run-time.
    destructor Destroy; override;
    { This method calculates the drawing rectangles (and other aspects) of each displayed element and stores the values to the temporary variables (a cache).
      The repainting of the component itself is then faster. This method is called when the values are modified, on resizing the component, etc. }
    procedure Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil); virtual;
    // Sorts the items. Use this method for one time sorting when the Sorted property is False. See also OnCompareItems event.
    procedure Sort; virtual;
    // Blocks "rebuilding" and repainting of the component until the corresponding EndUpdate is called.
    procedure BeginUpdate; virtual;
    // Re-enables repainting of the chart previously blocked by the BeginUpdate call.
    procedure EndUpdate; virtual;
    { Allows you to paint the component to the passed TargetCanvas. Dest is a destination rectangle, where the component will be drawn.
      Set AsEMF paramenter to True, when the TargetCanvas belongs to a vector based device (ie TMetafileCanvas).
      Refer to the DecoCompareGrid demo project as an example, to see how to use this method to save/export the chart into a graphic formats. }
    procedure PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False); virtual;
    // Temporary property, that indicates if the component has a focus
    property Active: Boolean read FActive;
  published
    // Setups the align in the parent control
    property Align;
    // Component anchors
    property Anchors;
    property BevelEdges;
    property BevelKind;
    // Bidirectional text drawing mode
    property BiDiMode;
    // Border style of the component
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    { A chart title. See also ShowCaption and CaptionFont. }
    property Caption;
    // Font for the chart title (Caption)
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    // Background color of the chart. See also ParentBackground.
    property Color default clWhite;
    { Component size constraints }
    property Constraints;
    property Ctl3D;
    { Controls double-buffering of the component. TCustomDecoChart component descendants are always painted as double-buffered, but this property is important
      for other descendants (such as TDecoCompareGridPlus), that use a composited rendering. }
    property DoubleBuffered;
    // Defines the drag cursor
    property DragCursor;
    // Defines the drag kind
    property DragKind;
    // Defines the drag mode
    property DragMode;
    // Enabling or disabling the component
    property Enabled;
    // Font for texts (values, captions, etc.). See also CaptionFont.
    property Font;
    { A glow size attribut for drawing texts (title, captions, values, etc.).
      It is not published in components that use a standard GDI drawing methods, but in the descendants (such as TDecoCompareGridPlus) it is. }
    property GlowSize: Integer read FGlowSize write SetGlowSize default 0;
    // The cursor for hovered items
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor default crDefault;
    { An event handler that allows you to perform a custom drawing on the component surface after the default drawing is finished. }
    property OnAfterPaint: TDecoPaintEvent read FOnAfterPaint write SetOnAfterPaint;
    { An event handler that allows you to perform a custom drawing on the component surface before the default drawing will start. }
    property OnBeforePaint: TDecoPaintEvent read FOnBeforePaint write SetOnBeforePaint;
    {$IF CompilerVersion >= 20}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFEND}
    { OnCanResize event }
    property OnCanResize;
    { OnClick event }
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IF CompilerVersion >= 18.0}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    {$IFEND}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    {$IF CompilerVersion >= 20.0}
    // A padding for the component
    property Padding;
    {$IFEND}
    property ParentBiDiMode;
    // Takes the same background color as its parent control (when not using the ParentBackground mode)
    property ParentColor default False;
    property ParentCtl3D;
    {$IF CompilerVersion >= 20.0}
    { ParentDoubleBuffered property. See also DoubleBuffered property. }
    property ParentDoubleBuffered;
    {$IFEND}
    // Takes the font of its parent control
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    // If True, the title of the chart (Caption) will be displayed. See also Caption property.
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    // If True, a focus rectangle will be drawn around the currently focused chart item.
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    { If True, the hints will be shown when the mouse is above the control }
    property ShowHint;
    property TabOrder;
    property TabStop;
    // If True, the chart items will be automatically sorted. See also Sort method and OnCompareItems event).
    property Sorted: Boolean read FSorted write SetSorted nodefault;
    // Margin for texts in cells. When the value is -1, the margin is calculated automatically based on the selected font.
    property TextMargin: Integer read FTextMargin write SetTextMargin default -1;
    { Controls visibility of the control }
    property Visible;
  end;

implementation

uses
  DecoGDI;

//////////////////////////////////////////////// TDecoChartItem /////////////////////////////////////////////////
constructor TDecoChartItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTag := 0;
  FData := nil;
  FHint := '';
  FCaption := '';
  FVisible := True;
  SetRectEmpty(FItemRect);
end;

function TDecoChartItem.GetDisplayName: string;
begin
  Result:=FCaption;
  if Length(Result) = 0 then
    Result := inherited GetDisplayName;
end;

procedure TDecoChartItem.Assign(Source: TPersistent);
begin
  if Source is TDecoChartItem then
  begin
    FTag := TDecoChartItem(Source).FTag;
    FData := TDecoChartItem(Source).FData;
    FHint := TDecoChartItem(Source).FHint;
    FCaption := TDecoChartItem(Source).FCaption;
    FVisible := TDecoChartItem(Source).FVisible;
    Changed(True);
  end
  else
    inherited;
end;

procedure TDecoChartItem.SetCaption(const Value: string);
begin
  if Value<>FCaption then
  begin
    FCaption:=Value;
    Changed(True);
  end;
end;

procedure TDecoChartItem.SetHint(const Value: string);
begin
  if Value<>FHint then
  begin
    FHint:=Value;
    Changed(False);
  end;
end;

procedure TDecoChartItem.SetVisible(Value: Boolean);
begin
  if (Value<>FVisible) then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

//////////////////////////////////////////////// TDecoChartItems ///////////////////////////////////////////////
constructor TDecoChartItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  if ItemClass = nil then
    inherited Create(AOwner, TDecoChartItem)
  else
    inherited Create(AOwner, ItemClass);
  FDecoChart:=TCustomDecoChart(Owner);
end;

procedure TDecoChartItems.Update(Item: TCollectionItem);
begin
  if Item=nil then
    FDecoChart.FRebuildNeeded:=True;
  FDecoChart.Invalidate;
end;

{$IF CompilerVersion >= 15.0}
procedure TDecoChartItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnDeleting:
      begin
        if FDecoChart.IsItemSelected(Item) then
          FDecoChart.CancelSelection;
        if FDecoChart.IsItemHovered(Item) then
          FDecoChart.CancelHover;
      end;
  end;
  inherited;
end;
{$ELSE}
procedure TDecoChartItems.Deleting(Item: TCollectionItem);
begin
  if FDecoChart.IsItemSelected(Item) then
    FDecoChart.CancelSelection;
  if FDecoChart.IsItemHovered(Item) then
    FDecoChart.CancelHover;
  inherited;
end;
{$IFEND}

function TDecoChartItems.GetNextItem(Item: TDecoChartItem; VisibleOnly:Boolean = False): TDecoChartItem;
var
  i,j:Integer;
begin
  Result := nil;
  if ItemClass.InheritsFrom(TDecoChartItem) then
  begin
    if Item = nil then
      j:=0
    else
      j:=Item.Index+1;
    if not VisibleOnly then
    begin
      if (j>=0) and (j<Count) then
        Result:=TDecoChartItem(Items[j]);
    end
    else
      for i:=j To Count-1 do
        if TDecoChartItem(Items[i]).Visible then
        begin
          Result:=TDecoChartItem(Items[i]);
          break;
        end;
  end;
end;

function TDecoChartItems.GetPriorItem(Item: TDecoChartItem; VisibleOnly:Boolean = False): TDecoChartItem;
var
  i,j:Integer;
begin
  Result := nil;
  if ItemClass.InheritsFrom(TDecoChartItem) then
  begin
    if Item = nil then
      j:=Count-1
    else
      j:=Item.Index-1;
    if not VisibleOnly then
    begin
      if (j>=0) and (j<Count) then
        Result:=TDecoChartItem(Items[j]);
    end
    else
    for i:=j downto 0 do
      if TDecoChartItem(Items[i]).Visible then
      begin
        Result:=TDecoChartItem(Items[i]);
        break;
      end;
  end;
end;

//////////////////////////////////////////////// TSortedDecoChartItems ///////////////////////////////////////////////
constructor TSortedDecoChartItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FAutoSort := True;
  FSorting := False;
  if DecoUseSpeedSort then
    FItemsRef := GetCollectionList(Self)
  else
    FItemsRef := nil;
end;

procedure TSortedDecoChartItems.Update(Item: TCollectionItem);
begin
  if not FSorting then
  begin
    if Item=nil then
    begin
      FDecoChart.FRebuildNeeded:=True;
      if FDecoChart.FSorted and FAutoSort then
        Sort;
    end;
    FDecoChart.Invalidate;
  end;
end;

function TSortedDecoChartItems.Insert(Index:Integer): TCollectionItem;
begin
  if FDecoChart.Sorted then
    Result := Add
  else
  begin
    if Assigned(FItemsRef) then
    begin
      Result := Add;
      FItemsRef.Move(Count-1,Index);
    end
    else
      Result := inherited Insert(Index);
  end;
end;

procedure TSortedDecoChartItems.Move(CurrIndex, NewIndex: Integer);
begin
  if not FDecoChart.Sorted then
  begin
    if Assigned(FItemsRef) then
      FItemsRef.Move(CurrIndex,NewIndex)
    else
      Items[CurrIndex].Index := NewIndex;
  end
end;

procedure TSortedDecoChartItems.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      if Assigned(FOnCompareItems) then
      begin
        while FOnCompareItems(FDecoChart,Items[i],Items[p]) < 0 do Inc(I);
        while FOnCompareItems(FDecoChart,Items[j],Items[p]) > 0 do Dec(J);
      end
      else
      if ItemClass.InheritsFrom(TDecoChartItem) then
      begin
        while AnsiCompareText(TDecoChartItem(Items[i]).FCaption,TDecoChartItem(Items[p]).FCaption) < 0 do Inc(I);
        while AnsiCompareText(TDecoChartItem(Items[j]).FCaption,TDecoChartItem(Items[p]).FCaption) > 0 do Dec(J);
      end;
      if I <= J then
      begin
        if I <> J then
        begin
          if Assigned(FItemsRef) then
            FItemsRef.Exchange(I,J)
          else
          begin
            Items[J].Index := I;
            Items[I+1].Index := J;
          end;
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TSortedDecoChartItems.Sort;
begin
  FSorting := True;
  try
    if Count>0 then
      QuickSort(0, Count - 1)
  finally
    FSorting := False;
  end;
end;

//////////////////////////////////////////// TCustomDecoChart ////////////////////////////////////////////
constructor TCustomDecoChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable,
    csSetCaption {$IF CompilerVersion >= 20.0}, csPannable {$IFEND} ];
  FDrawTextFunc := DoDrawNormalText;
  FCaptionFont := TFont.Create;
  FUpdateCount := 0;
  FActive:=False;
  FBorderStyle := bsNone;
  FGlowSize := 0;
  FHoverCursor := crDefault;
  FShowFocusRect := True;
  FSorted := False;
  FTextMargin := -1;
  FShowCaption := True;
  _EMFExporting:=False;

  FRebuildNeeded := True;

  FCaptionFont.Assign(Font);
  FCaptionFont.OnChange := CaptionFontChange;

  Width := 128;
  Height := 128;
  {$IF CompilerVersion >= 20.0}
  Padding.OnChange:=DoPaddingChange;
  {$IFEND}
  Color := clWhite;
  ParentBackground := False;
end;

destructor TCustomDecoChart.Destroy;
begin
  FCaptionFont.Free;
  inherited;
end;

procedure TCustomDecoChart.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style  and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCustomDecoChart.Loaded;
begin
  inherited;
end;

procedure TCustomDecoChart.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  FRebuildNeeded:=True;
  Invalidate;
  inherited;
end;

procedure TCustomDecoChart.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomDecoChart.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FActive:=False;
  Invalidate;
end;

procedure TCustomDecoChart.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FActive:=True;
  Invalidate;
end;

procedure TCustomDecoChart.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message do
  begin
    if (CursorWnd = Handle) and (not (csDesigning in ComponentState)) and (Screen.Cursor = crDefault) and IsAnyItemHovered then
    begin
      Message.Result := 1;
      Windows.SetCursor(Screen.Cursors[FHoverCursor])
    end
    else
      inherited;
  end;
end;

procedure TCustomDecoChart.CMWantSpecialKey(var Message: TWMKey);
begin
  inherited;
  if (Message.CharCode in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_ESCAPE, VK_HOME, VK_END, VK_NEXT, VK_PRIOR]) then
    Message.Result := 1
  else
  if (Message.CharCode = VK_TAB) and FActive then
  begin
    if (not IsAnyItemSelected) and (not IsEmpty) then
      SelectFirstItem
    else
      Invalidate;
  end;
end;

procedure TCustomDecoChart.CMBorderChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoChart.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TCustomDecoChart.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoChart.CMFontChanged(var Message: TMessage);
begin
  FRebuildNeeded:=True;
  inherited;
end;

procedure TCustomDecoChart.CMHintShow(var Msg: TMessage);
begin
  with TCMHintShow(Msg) do
  begin
    if not ShowHint then
      Result := 1
    else
    begin
      if not IsAnyItemHovered then
        HintInfo.HintStr:=Hint
      else
        GetHoverHintInfo(HintInfo.HintStr, HintInfo.CursorRect);
      if Length(HintInfo.HintStr)>0 then
        Result := 0
      else
        Result := 1
    end;
  end
end;

procedure TCustomDecoChart.CMParentFontChanged(var Message: {$IF CompilerVersion >= 20.0} TCMParentFontChanged {$ELSE} TMessage {$IFEND});
begin
  inherited;
  if ParentFont then
    FCaptionFont.Assign(Font);
end;

procedure TCustomDecoChart.CMMouseLeave(var Message: TMessage);
begin
  if IsAnyItemHovered then
    CancelHover;
  inherited;
end;

procedure TCustomDecoChart.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomDecoChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (not FActive) and HandleAllocated and Visible and (not (ssDouble in Shift)) then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

{$IF CompilerVersion >= 20.0}
procedure TCustomDecoChart.DoPaddingChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Realign;
  Invalidate;
end;
{$IFEND}

procedure TCustomDecoChart.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  FParentBackgroundSet := True;
  inherited;
end;

procedure TCustomDecoChart.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomDecoChart.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

procedure TCustomDecoChart.CaptionFontChange(Sender: TObject);
begin
  FRebuildNeeded:=True;
  Invalidate;
end;

procedure TCustomDecoChart.SetGlowSize(Value: Integer);
begin
  if Value <> FGlowSize then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

procedure TCustomDecoChart.SetShowCaption(Value: Boolean);
begin
  if Value<>FShowCaption then
  begin
    FShowCaption:=Value;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoChart.SetShowFocusRect(Value: Boolean);
begin
  if Value<>FShowFocusRect then
  begin
    FShowFocusRect:=Value;
    Invalidate;
  end;
end;

procedure TCustomDecoChart.SetSorted(Value: Boolean);
begin
  if Value<>FSorted then
  begin
    FSorted := Value;
    if FSorted then
      Sort;
  end;
end;

procedure TCustomDecoChart.SetOnBeforePaint(Value: TDecoPaintEvent);
begin
  FOnBeforePaint:=Value;
  Invalidate;
end;

procedure TCustomDecoChart.SetOnAfterPaint(Value: TDecoPaintEvent);
begin
  FOnAfterPaint:=Value;
  Invalidate;
end;

procedure TCustomDecoChart.SetTextMargin(Value: Integer);
begin
  if Value<>FTextMargin then
  begin
    FTextMargin:=Value;
    if FTextMargin<-1 then
      FTextMargin:=-1;
    FRebuildNeeded:=True;
    Invalidate;
  end;
end;

procedure TCustomDecoChart.Sort;
begin
  //
end;

procedure TCustomDecoChart.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomDecoChart.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Invalidate
  else
  if FUpdateCount<0 then
  begin
    FUpdateCount:=0;
    Invalidate;
  end;
end;

function TCustomDecoChart.IsEmpty:Boolean;
begin
  Result := False;
end;

function TCustomDecoChart.IsAnyItemSelected:Boolean;
begin
  Result := False;
end;

function TCustomDecoChart.IsItemSelected(const Item:TCollectionItem):Boolean;
begin
  Result := False;
end;

procedure TCustomDecoChart.SelectFirstItem;
begin
  //
end;

function TCustomDecoChart.GetFocusRect:TRect;
begin
  Result := ClientRect;
  Inc(Result.Bottom);
end;

procedure TCustomDecoChart.CancelSelection;
begin
  //
end;

function TCustomDecoChart.IsAnyItemHovered:Boolean;
begin
  Result := False;
end;

function TCustomDecoChart.IsItemHovered(const Item:TCollectionItem):Boolean;
begin
  Result := False;
end;

procedure TCustomDecoChart.CancelHover;
begin
  //
end;

procedure TCustomDecoChart.GetHoverHintInfo(var aHint:string; var aRect:TRect);
begin
  //
end;

procedure TCustomDecoChart.Rebuild(Dest:TRect; TargetCanvas:TCanvas=nil);
begin
  FRebuildNeeded:=False;
end;

procedure TCustomDecoChart.DrawChart(TargetCanvas:TCanvas);
begin
  //
end;

procedure TCustomDecoChart.DrawFocus(TargetCanvas:TCanvas; aRect:TRect);
begin
  TargetCanvas.Font.Color:=Font.Color;
  windows.DrawFocusRect(TargetCanvas.Handle,aRect);
end;

procedure TCustomDecoChart.Paint;
var
  aRect,cRect: TRect;
  Buffer : TBitmap;
  tmpCanvas : TCanvas;
begin
  if FUpdateCount>0 then
    Exit;
  aRect := ClientRect;
  if FRebuildNeeded then
    Rebuild(aRect);
  cRect := Canvas.ClipRect;
  Buffer := TBitmap.Create;
  try
    Buffer.Width := aRect.Right-aRect.Left;
    Buffer.Height := aRect.Bottom-aRect.Top;
    tmpCanvas := Buffer.Canvas;
    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);

    tmpCanvas.Brush.Style := bsSolid;
    tmpCanvas.Brush.Color := Color;
    tmpCanvas.FillRect(cRect);

    tmpCanvas.Font := Font;
    if not Enabled then
      tmpCanvas.Font.Color:=clGrayText;

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,tmpCanvas);

    DrawChart(tmpCanvas);

    IntersectClipRect(tmpCanvas.Handle,cRect.Left,cRect.Top,cRect.Right,cRect.Bottom);
    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,tmpCanvas);

    Canvas.CopyRect(cRect, tmpCanvas, cRect);

  finally
    Buffer.Free;
  end;

  if FActive and FShowFocusRect and IsAnyItemSelected then
    DrawFocus(Canvas, GetFocusRect);
end;

procedure TCustomDecoChart.PrintTo(TargetCanvas:TCanvas; Dest:TRect; AsEMF:Boolean=False);
begin
  _EMFExporting:=AsEMF;
  try
    FRebuildNeeded := True;
    Rebuild(Dest,TargetCanvas);

    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Brush.Color := Color;
    TargetCanvas.FillRect(Dest);

    TargetCanvas.Font := Font;
    if not Enabled then
      TargetCanvas.Font.Color:=clGrayText;

    if Assigned(OnBeforePaint) then
      OnBeforePaint(Self,TargetCanvas);

    DrawChart(TargetCanvas);

    if Assigned(OnAfterPaint) then
      OnAfterPaint(Self,TargetCanvas);

  finally
    _EMFExporting := False;
    FRebuildNeeded := True;
    Rebuild(ClientRect);
  end;
end;

end.
