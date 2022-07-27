{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTabs3x
Purpose  : Rewrite of the original Delphi Win3x tab with future enhancements
           (Win2k)
Date     : 05-15-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmTabs3x;

interface

{$I CompilerDefines.INC}

uses Windows, Classes, Graphics, Forms, Controls, Messages;

type
  TScrollBtn = (sbLeft, sbRight);
  TTabType = (ttWin3x, ttWin2k);

  TrmScroller = class(TCustomControl)
  private
    { property usage }
    FMin: Longint;
    FMax: Longint;
    FPosition: Longint;
    FOnClick: TNotifyEvent;
    FChange: Integer;

    { private usage }
    Bitmap: TBitmap;
    Pressed: Boolean;
    Down: Boolean;
    Current: TScrollBtn;
    pWidth: Integer;
    pHeight: Integer;

    { property access methods }
    procedure SetMin(Value: Longint);
    procedure SetMax(Value: Longint);
    procedure SetPosition(Value: Longint);

    { private methods }
    function CanScrollLeft: Boolean;
    function CanScrollRight: Boolean;
    procedure DoMouseDown(X: Integer);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Min: Longint read FMin write SetMin default 0;
    property Max: Longint read FMax write SetMax default 0;
    property Position: Longint read FPosition write SetPosition default 0;
    property Change: Integer read FChange write FChange default 1;
  end;

  TrmTabSet = class;

  TrmTabList = class(TStringList)
  private
    Tabs: TrmTabSet;
  public
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    function Add(const S: string): Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
  end;

  { eash TEdgeType is made up of one or two of these parts }
  TEdgePart = (epSelectedLeft, epUnselectedLeft, epSelectedRight,
    epUnselectedRight);

  { represents the intersection between two tabs, or the edge of a tab }
  TEdgeType = (etNone, etFirstIsSel, etFirstNotSel, etLastIsSel, etLastNotSel,
    etNotSelToSel, etSelToNotSel, etNotSelToNotSel);

  TTabStyle = (tsStandard, tsOwnerDraw);

  TMeasureTabEvent = procedure(Sender: TObject; Index: Integer; var TabWidth: Integer) of object;
  TDrawTabEvent = procedure(Sender: TObject; TabCanvas: TCanvas; R: TRect; Index: Integer; Selected: Boolean) of object;
  TTabChangeEvent = procedure(Sender: TObject; NewTab: Integer; var AllowChange: Boolean) of object;

  TrmTabSet = class(TCustomControl)
  private
    { property instance variables }
    FStartMargin: Integer;
    FEndMargin: Integer;
    FTabs: TStrings;
    FTabIndex: Integer;
    FFirstIndex: Integer;
    FVisibleTabs: Integer;
    FSelectedColor: TColor;
    FUnselectedColor: TColor;
    FBackgroundColor: TColor;
    FDitherBackground: Boolean;
    FAutoScroll: Boolean;
    FStyle: TTabStyle;
    FOwnerDrawHeight: Integer;
    FOnMeasureTab: TMeasureTabEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnChange: TTabChangeEvent;
    FTabType: TTabType;

    { private instance variables }
    TabPositions: TList;
    FTabHeight: Integer;
{    FTopEdge, FBottomEdge: integer;}
    FScroller: TrmScroller;
    FDoFix: Boolean;
    FDisabledTabs: TStrings;
    fEdgeWidth: integer;

    { property access methods }
    procedure SetSelectedColor(Value: TColor);
    procedure SetUnselectedColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetDitherBackground(Value: Boolean);
    procedure SetAutoScroll(Value: Boolean);
    procedure SetStartMargin(Value: Integer);
    procedure SetEndMargin(Value: Integer);
    procedure SetTabIndex(Value: Integer);
    procedure SetFirstIndex(Value: Integer);
    procedure SetTabList(Value: TStrings);
    procedure SetTabStyle(Value: TTabStyle);
    procedure SetTabHeight(Value: Integer);
    procedure SetTabType(const Value: TTabType);
    procedure SetDisabledTabList(const Value: TStrings);

    { private methods }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure PaintEdge(Canvas: TCanvas; X, Y, H: Integer; Edge: TEdgeType);
    procedure CreateBrushPattern(Bitmap: TBitmap);
    function Calc3xTabPositions(Start, Stop: Integer; Canvas: TCanvas; First: Integer): Integer;
    function Calc2kTabPositions(Start, Stop: Integer; Canvas: TCanvas; First: Integer): Integer;
    procedure CreateScroller;
    procedure FixTabPos;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure ScrollClick(Sender: TObject);
    procedure ReadIntData(Reader: TReader);
    procedure ReadBoolData(Reader: TReader);
    procedure SetEdgeWidth(const Value: integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer; Selected: Boolean); virtual;
    function CanChange(NewIndex: Integer): Boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure MeasureTab(Index: Integer; var TabWidth: Integer); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Paint2k;
    procedure Paint3x;
    function TabEnabled(index:integer):boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(Pos: TPoint): Integer;
    function ItemRect(Item: Integer): TRect;
    procedure SelectNext(Direction: Boolean);
    property Canvas;
    property FirstIndex: Integer read FFirstIndex write SetFirstIndex default 0;

  published
    property Align;
    property Anchors;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default True;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property Constraints;
    property DitherBackground: Boolean read FDitherBackground write SetDitherBackground default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeWidth : integer read fEdgeWidth write SetEdgeWidth default 9; //This controls the angle of the tab edges
    property Enabled;
    property DisabledTabs: TStrings read FDisabledTabs write SetDisabledTabList;
    property EndMargin: Integer read FEndMargin write SetEndMargin default 5;
    property Font;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 5;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBtnFace;
    property Style: TTabStyle read FStyle write SetTabStyle default tsStandard;
    property TabHeight: Integer read FOwnerDrawHeight write SetTabHeight default 20;
    property Tabs: TStrings read FTabs write SetTabList;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property TabType: TTabType read fTabType write SetTabType default ttWin3x;
    property UnselectedColor: TColor read FUnselectedColor write SetUnselectedColor default clWindow;
    property Visible;
    property VisibleTabs: Integer read FVisibleTabs;
    property OnClick;
    property OnChange: TTabChangeEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMeasureTab: TMeasureTabEvent read FOnMeasureTab write FOnMeasureTab;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses Consts, SysUtils, rmLibrary;

{$R rmTabs3x.RES}

type
  TTabPos = record
    Size, StartPos: Word;
  end;

{ TrmScroller }

constructor TrmScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Bitmap := TBitmap.Create;
  pWidth := 24;
  pHeight := 13;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FChange := 1;
end;

destructor TrmScroller.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TrmScroller.Paint;
begin
  with Canvas do
  begin
    { paint left button }
    if CanScrollLeft then
    begin
      if Down and (Current = sbLeft) then
        Bitmap.Handle := LoadBitmap(HInstance, 'RMSBLEFTDN')
      else
        Bitmap.Handle := LoadBitmap(HInstance, 'RMSBLEFT');
    end
    else
      Bitmap.Handle := LoadBitmap(HInstance, 'RMSBLEFTDIS');
    Draw(0, 0, Bitmap);

    { paint right button }
    if CanScrollRight then
    begin
      if Down and (Current = sbRight) then
        Bitmap.Handle := LoadBitmap(HInstance, 'RMSBRIGHTDN')
      else
        Bitmap.Handle := LoadBitmap(HInstance, 'RMSBRIGHT');
    end
    else
      Bitmap.Handle := LoadBitmap(HInstance, 'RMSBRIGHTDIS');
    Draw((pWidth div 2) - 1, 0, Bitmap);
  end;
end;

procedure TrmScroller.WMSize(var Message: TWMSize);
begin
  inherited;
  Width := pWidth - 1;
  Height := pHeight;
end;

procedure TrmScroller.SetMin(Value: Longint);
begin
  if Value < FMax then FMin := Value;
end;

procedure TrmScroller.SetMax(Value: Longint);
begin
  if Value > FMin then FMax := Value;
end;

procedure TrmScroller.SetPosition(Value: Longint);
begin
  if Value <> FPosition then
  begin
    if Value < Min then Value := Min;
    if Value > Max then Value := Max;
    FPosition := Value;
    Invalidate;
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

function TrmScroller.CanScrollLeft: Boolean;
begin
  Result := Position > Min;
end;

function TrmScroller.CanScrollRight: Boolean;
begin
  Result := Position < Max;
end;

procedure TrmScroller.DoMouseDown(X: Integer);
begin
  if X < pWidth div 2 then
    Current := sbLeft
  else
    Current := sbRight;
  case Current of
    sbLeft:
      if not CanScrollLeft then Exit;
    sbRight:
      if not CanScrollRight then Exit;
  end;
  Pressed := True;
  Down := True;
  Invalidate;
  SetCapture(Handle);
end;

procedure TrmScroller.WMLButtonDown(var Message: TWMLButtonDown);
begin
  DoMouseDown(Message.XPos);
end;

procedure TrmScroller.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  DoMouseDown(Message.XPos);
end;

procedure TrmScroller.WMMouseMove(var Message: TWMMouseMove);
var
  P: TPoint;
  R: TRect;
begin
  if Pressed then
  begin
    P := Point(Message.XPos, Message.YPos);
    R := Rect(0, 0, pWidth div 2, pHeight);
    if Current = sbRight then OffsetRect(R, pWidth div 2, 0);
    if PtInRect(R, P) <> Down then
    begin
      Down := not Down;
      Invalidate;
    end;
  end;
end;

procedure TrmScroller.WMLButtonUp(var Message: TWMLButtonUp);
var
  NewPos: Longint;
begin
  ReleaseCapture;
  Pressed := False;

  if Down then
  begin
    Down := False;
    NewPos := Position;
    case Current of
      sbLeft: Dec(NewPos, Change);
      sbRight: Inc(NewPos, Change);
    end;
    Position := NewPos;
  end;
end;

{ TrmTabList }

function TrmTabList.Add(const S: string): Integer;
begin
  Result := inherited Add(S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;

procedure TrmTabList.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, S);
  if Tabs <> nil then
  begin
    if Index <= Tabs.FTabIndex then Inc(Tabs.FTabIndex);
    Tabs.Invalidate;
  end;
end;

procedure TrmTabList.Delete(Index: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := Tabs.Tabindex;
  inherited Delete(Index);

  if OldIndex < Count then
    Tabs.FTabIndex := OldIndex
  else
    Tabs.FTabIndex := Count - 1;
  Tabs.Invalidate;
  Tabs.Invalidate;
  if OldIndex = Index then Tabs.Click; { deleted selected tab }
end;

procedure TrmTabList.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  if Tabs <> nil then
    Tabs.Invalidate;
end;

procedure TrmTabList.Clear;
begin
  inherited Clear;
  Tabs.FTabIndex := -1;
  Tabs.Invalidate;
end;

procedure TrmTabList.AddStrings(Strings: TStrings);
begin
  SendMessage(Tabs.Handle, WM_SETREDRAW, 0, 0);
  inherited AddStrings(Strings);
  SendMessage(Tabs.Handle, WM_SETREDRAW, 1, 0);
  Tabs.Invalidate;
end;

{ TrmTabSet }

constructor TrmTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csDoubleClicks, csOpaque];
  fEdgeWidth := 9;
  Width := 185;
  Height := 21;

  TabPositions := TList.Create;
  fTabHeight := 20;

  FTabs := TrmTabList.Create;
  TrmTabList(FTabs).Tabs := Self;
  FDisabledTabs := TStringList.create;

  CreateScroller;

  FTabIndex := -1;
  FFirstIndex := 0;
  FVisibleTabs := 0; { set by draw routine }
  FStartMargin := 5;
  FEndMargin := 5;

  { initialize default values }
  FSelectedColor := clBtnFace;
  FUnselectedColor := clWindow;
  FBackgroundColor := clBtnFace;
  FDitherBackground := True;
  fTabType := ttWin3x;
  FAutoScroll := True;
  FStyle := tsStandard;
  FOwnerDrawHeight := 20;

  ParentFont := False;
  Font.Name := DefFontData.Name;
  Font.Height := DefFontData.Height;
  Font.Style := [];

  { create the edge bitmaps }
end;

procedure TrmTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_VREDRAW or CS_HREDRAW);
end;

procedure TrmTabSet.CreateScroller;
begin
  FScroller := TrmScroller.Create(Self);
  with FScroller do
  begin
    Parent := Self;
    Top := 3;
    Min := 0;
    Max := 0;
    Position := 0;
    Visible := False;
    OnClick := ScrollClick;
  end;
end;

destructor TrmTabSet.Destroy;
begin
  FTabs.Free;
  TabPositions.Free;
  inherited Destroy;
end;

procedure TrmTabSet.ScrollClick(Sender: TObject);
begin
  FirstIndex := TrmScroller(Sender).Position;
end;

{ cache the tab position data, and return number of visible tabs }

function TrmTabSet.Calc3xTabPositions(Start, Stop: Integer; Canvas: TCanvas;
  First: Integer): Integer;
var
  Index: Integer;
  TabPos: TTabPos;
  W: Integer;
begin
  TabPositions.Count := 0; { erase all previously cached data }
  Index := First;
  while (Start < Stop) and (Index < Tabs.Count) do
  begin
    with Canvas do
    begin
      TabPos.StartPos := Start;
      W := TextWidth(Tabs[Index]);

      { Owner }
      if (FStyle = tsOwnerDraw) then MeasureTab(Index, W);

      TabPos.Size := W;
      Inc(Start, TabPos.Size + EdgeWidth); { next usable position }

      if Start <= Stop then
      begin
        TabPositions.Add(Pointer(TabPos)); { add to list }
        Inc(Index);
      end;
    end;
  end;
  Result := Index - First;
end;

function TrmTabSet.ItemAtPos(Pos: TPoint): Integer;
var
  TabPos: TTabPos;
  I: Integer;
begin
  Result := -1;
  if (Pos.Y < 0) or (Pos.Y > ClientHeight) then Exit;
  for I := 0 to TabPositions.Count - 1 do
  begin
    Pointer(TabPos) := TabPositions[I];
    if (Pos.X >= TabPos.StartPos) and (Pos.X <= TabPos.StartPos + TabPos.Size) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TrmTabSet.ItemRect(Item: Integer): TRect;
var
  TabPos: TTabPos;
  wYPos : integer;
begin
  wyPos := 0;
  if align = altop then
     wypos := clientheight - fTabHeight;

  if (TabPositions.Count > 0) and (Item >= 0) and (Item < TabPositions.Count) then
  begin
    Pointer(TabPos) := TabPositions[Item];
    Result := Rect(TabPos.StartPos, wYPos, TabPos.StartPos + TabPos.Size, wYPos+FTabHeight);
    InflateRect(Result, 1, -2);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TrmTabSet.Paint;
begin
  case ftabtype of
    ttWin3x: Paint3x;
    ttWin2k: Paint2k;
  end;
end;

procedure TrmTabSet.Paint3x;
var
  MemBitmap, BrushBitmap: TBitmap;
  TabStart, LastTabPos: Integer;
  TabPos: TTabPos;
  Tab: Integer;
  Leading: TEdgeType;
  Trailing: TEdgeType;
  isFirst, isLast, isSelected, isPrevSelected: Boolean;
  R: TRect;
  wYPos : integer;
begin
  if not HandleAllocated then Exit;

  MemBitmap := TBitmap.create;
  try
     { Set the size of the off-screen bitmap.  Make sure that it is tall enough to
       display the entire tab, even if the screen won't display it all.  This is
       required to avoid problems with using FloodFill. }
    MemBitmap.Width := ClientWidth;
    if ClientHeight < FTabHeight + 5 then
      MemBitmap.Height := FTabHeight + 5
    else
      MemBitmap.Height := ClientHeight;

    wyPos := 0;
    if align = altop then
       wypos := clientheight - fTabHeight;

    MemBitmap.Canvas.Font := Self.Canvas.Font;

    TabStart := StartMargin + EdgeWidth; { where does first text appear? }
    LastTabPos := Width - EndMargin; { tabs draw until this position }
    FScroller.Left := Width - FScroller.Width - 2;

     { do initial calculations for how many tabs are visible }
    FVisibleTabs := Calc3xTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
      FirstIndex);

     { enable the scroller if FAutoScroll = True and not all tabs are visible }
    if AutoScroll and (FVisibleTabs < Tabs.Count) then
    begin
      Dec(LastTabPos, FScroller.Width - 4);
       { recalc the tab positions }
      FVisibleTabs := Calc3xTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
        FirstIndex);

       { set the scroller's range }
      FScroller.Visible := True;
      ShowWindow(FScroller.Handle, SW_SHOW);
      FScroller.Min := 0;
      FScroller.Max := Tabs.Count - VisibleTabs;
      FScroller.Position := FirstIndex;
    end
    else if VisibleTabs >= Tabs.Count then
    begin
      FScroller.Visible := False;
      ShowWindow(FScroller.Handle, SW_HIDE);
    end;

    if FDoFix then
    begin
      FixTabPos;
      FVisibleTabs := Calc3xTabPositions(TabStart, LastTabPos, MemBitmap.Canvas,
        FirstIndex);
    end;
    FDoFix := False;

     { draw background of tab area }
    with MemBitmap.Canvas do
    begin
      BrushBitmap := TBitmap.create;
      try
        CreateBrushPattern(BrushBitmap);
        Brush.Bitmap := BrushBitmap;
        FillRect(Rect(0, 0, MemBitmap.Width, MemBitmap.Height));

        Pen.Width := 1;
        if align <> alTop then
        begin
           Pen.Color := clBtnShadow;
           MoveTo(0, 0);
           LineTo(MemBitmap.Width + 1, 0);

           Pen.Color := clWindowFrame;
           MoveTo(0, 1);
           LineTo(MemBitmap.Width + 1, 1);
        end
        else 
        begin
           Pen.Color := clBtnHighlight;
           MoveTo(0, height-1);
           LineTo(MemBitmap.Width + 1, height-1);

           Pen.Color := cl3DLight;
           MoveTo(0, height);
           LineTo(MemBitmap.Width + 1, height);
        end
      finally
        BrushBitmap.free;
      end;
    end;

    for Tab := 0 to TabPositions.Count - 1 do
    begin
      Pointer(TabPos) := TabPositions[Tab];

      isFirst := Tab = 0;
      isLast := Tab = VisibleTabs - 1;
      isSelected := Tab + FirstIndex = TabIndex;
      isPrevSelected := (Tab + FirstIndex) - 1 = TabIndex;

       { Rule: every tab paints its leading edge, only the last tab paints a
         trailing edge }
      Trailing := etNone;

      if isLast then
      begin
        if isSelected then
          Trailing := etLastIsSel
        else
          Trailing := etLastNotSel;
      end;

      if isFirst then
      begin
        if isSelected then
          Leading := etFirstIsSel
        else
          Leading := etFirstNotSel;
      end
      else { not first }
      begin
        if isPrevSelected then
          Leading := etSelToNotSel
        else if isSelected then
          Leading := etNotSelToSel
        else
          Leading := etNotSelToNotSel;
      end;

       { draw leading edge }
      if Leading <> etNone then
        PaintEdge(MemBitmap.Canvas, TabPos.StartPos - EdgeWidth, wypos, FTabHeight - 1, Leading);

       { set up the canvas }
      R := Rect(TabPos.StartPos, wypos, TabPos.StartPos + TabPos.Size, wypos+FTabHeight);
      with MemBitmap.Canvas do
      begin
        if isSelected then
          Brush.Color := SelectedColor
        else
          Brush.Color := UnselectedColor;
        ExtTextOut(Handle, TabPos.StartPos, wypos, ETO_OPAQUE, @R,
          nil, 0, nil);
      end;

       { restore font for drawing the text }
      MemBitmap.Canvas.Font := Self.Canvas.Font;

       { Owner }
      if (FStyle = tsOwnerDraw) then
        DrawTab(MemBitmap.Canvas, R, Tab + FirstIndex, isSelected)
      else
      begin
        with MemBitmap.Canvas do
        begin
          Inc(R.Top, 2);

          if TabEnabled(Tab + FirstIndex) then
             Font.Color := clWindowText
          else
             Font.Color := clGrayText;

          DrawText(Handle, PChar(Tabs[Tab + FirstIndex]),
            Length(Tabs[Tab + FirstIndex]), R, DT_CENTER);
        end;
      end;

       { draw trailing edge  }
      if Trailing <> etNone then
        PaintEdge(MemBitmap.Canvas, TabPos.StartPos + TabPos.Size, wypos, FTabHeight - 1, Trailing);

       { draw connecting lines above and below the text }

      with MemBitmap.Canvas do
      begin
        Pen.Color := clWindowFrame;
        if align<>alTop then
        begin
           MoveTo(TabPos.StartPos, FTabHeight-1);
           LineTo(TabPos.StartPos + TabPos.Size, FTabHeight-1);

           if isSelected then
           begin
             Pen.Color := clBtnShadow;
             MoveTo(TabPos.StartPos, FTabHeight - 2);
             LineTo(TabPos.StartPos + TabPos.Size, FTabHeight - 2);
           end
           else
           begin
             Pen.Color := clWindowFrame;
             MoveTo(TabPos.StartPos, 1);
             LineTo(TabPos.StartPos + TabPos.Size, 1);

             Pen.Color := clBtnShadow;
             MoveTo(TabPos.StartPos, 0);
             LineTo(TabPos.StartPos + TabPos.Size + 1, 0);
           end;
        end
        else
        begin
           MoveTo(TabPos.StartPos, wypos);
           LineTo(TabPos.StartPos + TabPos.Size, wypos);

           if isSelected then
           begin
             Pen.Color := clBtnHighlight;
             MoveTo(TabPos.StartPos, wypos+1);
             LineTo(TabPos.StartPos + TabPos.Size, wypos+1);
           end;
        end
      end;
    end;

     { draw onto the screen }
    Canvas.Draw(0, 0, MemBitmap);
  finally
    MemBitmap.free;
  end;
end;

procedure TrmTabSet.Paint2k;
var
  MemBitmap: TBitmap;
  TabStart, LastTabPos: Integer;
  TabPos: TTabPos;
  Tab: Integer;
  isFirst, isSelected: Boolean;
  R: TRect;
  loop: integer;
  wYPos : integer;
begin
  if not HandleAllocated then Exit;

  MemBitmap := TBitmap.create;
  try
     { Set the size of the off-screen bitmap.  Make sure that it is tall enough to
       display the entire tab, even if the screen won't display it all.  This is
       required to avoid problems with using FloodFill. }
    MemBitmap.Width := ClientWidth;
    if ClientHeight < FTabHeight + 5 then
      MemBitmap.Height := FTabHeight + 5
    else
      MemBitmap.Height := ClientHeight;

    wyPos := 0;
    if align = altop then
       wypos := clientheight - fTabHeight;

    MemBitmap.Canvas.Font := Self.Canvas.Font;

    TabStart := StartMargin + EdgeWidth; { where does first text appear? }
    LastTabPos := Width - EndMargin; { tabs draw until this position }
    FScroller.Left := Width - FScroller.Width - 2;

     { do initial calculations for how many tabs are visible }
    FVisibleTabs := Calc2kTabPositions(TabStart, LastTabPos, MemBitmap.Canvas, FirstIndex);

     { enable the scroller if FAutoScroll = True and not all tabs are visible }
    if AutoScroll and (FVisibleTabs < Tabs.Count) then
    begin
      Dec(LastTabPos, FScroller.Width - 4);
       { recalc the tab positions }
      FVisibleTabs := Calc2kTabPositions(TabStart, LastTabPos, MemBitmap.Canvas, FirstIndex);

       { set the scroller's range }
      FScroller.Visible := True;
      ShowWindow(FScroller.Handle, SW_SHOW);
      FScroller.Min := 0;
      FScroller.Max := Tabs.Count - VisibleTabs;
      FScroller.Position := FirstIndex;
    end
    else if VisibleTabs >= Tabs.Count then
    begin
      FScroller.Visible := False;
      ShowWindow(FScroller.Handle, SW_HIDE);
    end;

    if FDoFix then
    begin
      FixTabPos;
      FVisibleTabs := Calc2kTabPositions(TabStart, LastTabPos, MemBitmap.Canvas, FirstIndex);
    end;
    FDoFix := False;

     { draw background of tab area }
    with MemBitmap.Canvas do
    begin
      Brush.Color := clBtnShadow;
      FillRect(Rect(0, 0, MemBitmap.Width, MemBitmap.Height));

      if align<>altop then
      begin
         Pen.Color := clbtnFace;
         for loop := 0 to 1 do
         begin
           MoveTo(0, loop);
           LineTo(MemBitmap.Width + 1, loop);
         end;

         Pen.Color := clWindowFrame;
         MoveTo(0, 2);
         LineTo(MemBitmap.Width + 1, 2);
      end
      else
      begin
         Pen.Color := clbtnFace;
         for loop := clientheight-2 to clientheight do
         begin
           MoveTo(0, loop);
           LineTo(MemBitmap.Width + 1, loop);
         end;

         Pen.Color := clBtnHighlight;
         MoveTo(0, clientHeight-3);
         LineTo(MemBitmap.Width + 1, clientheight-3);
      end;
    end;

    for Tab := 0 to TabPositions.Count - 1 do
    begin
      if not TabEnabled(Tab + FirstIndex) then
         continue;
          
      Pointer(TabPos) := TabPositions[Tab];

      isFirst := Tab = 0;
      isSelected := Tab + FirstIndex = TabIndex;

      R := Rect(TabPos.StartPos - (EdgeWidth div 2), wypos, (TabPos.StartPos + TabPos.Size) + (EdgeWidth div 2), wypos+FTabHeight);

      with MemBitmap.Canvas do
      begin
        if isSelected then
        begin
          Brush.Color := clBtnFace;
          FillRect(R);

          Font.Color := clBtnText;
          Inc(R.Top, 1);

          if Align = albottom then
             R := Rect(TabPos.StartPos, wypos+2, TabPos.StartPos + TabPos.Size, wypos+FTabHeight);

          DrawText(Handle, PChar(Tabs[Tab + FirstIndex]), Length(Tabs[Tab + FirstIndex]), R, DT_CENTER);

          Pen.Color := clBtnHighlight;
          if align <> alTop then
          begin
             MoveTo(TabPos.StartPos - (EdgeWidth div 2), wYpos+2);
             LineTo(TabPos.StartPos - (EdgeWidth div 2), wYpos+FTabHeight);

             Pen.Color := cl3DDkShadow;
             MoveTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2), wYpos+2);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2), wYpos+FTabHeight);

             MoveTo(TabPos.StartPos - (EdgeWidth div 2), wYpos+FTabHeight);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2), wYpos+FTabHeight);

             Pen.Color := clBtnShadow;
             MoveTo(TabPos.StartPos - (EdgeWidth div 2)+1, FTabHeight - 1);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2)-1, FTabHeight - 1);
          end
          else
          begin
             MoveTo(TabPos.StartPos - (EdgeWidth div 2), wYpos);
             LineTo(TabPos.StartPos - (EdgeWidth div 2), wYpos+FTabHeight-2);

             MoveTo(TabPos.StartPos - (EdgeWidth div 2) + 1, wypos);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2) - 1, wypos);

             Pen.Color := cl3DDkShadow;
             MoveTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2), wYpos);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2), wYpos+FTabHeight-2);
          end;
        end
        else
        begin
          R := Rect(TabPos.StartPos, wypos+2, TabPos.StartPos + TabPos.Size, wypos+FTabHeight);
          Brush.Style := bsClear;

          if TabEnabled(Tab + FirstIndex) then
             Font.Color := clBtnHighlight
          else
             Font.Color := cl3DDkShadow;

          Inc(R.Top, 1);

          if Align = alTop then
             R := Rect(TabPos.StartPos, wypos, TabPos.StartPos + TabPos.Size, wypos+FTabHeight);

          DrawText(Handle, PChar(Tabs[Tab + FirstIndex]), Length(Tabs[Tab + FirstIndex]), R, DT_CENTER);

          if align <> altop then
          begin
             Pen.Color := clBtnHighlight;
             if isFirst then
             begin
               MoveTo(TabPos.StartPos - (EdgeWidth div 2), 5);
               LineTo(TabPos.StartPos - (EdgeWidth div 2), (2 + FTabHeight) - 3);
             end;

             MoveTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2) + 1, 5);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2) + 1, (2 + FTabHeight) - 3);
          end
          else
          begin
             Pen.Color := clBtnHighlight;
             if isFirst then
             begin
               MoveTo(TabPos.StartPos - (EdgeWidth div 2), wypos);
               LineTo(TabPos.StartPos - (EdgeWidth div 2), wypos + FTabHeight - 5);
             end;

             MoveTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2) + 1, wypos);
             LineTo(TabPos.StartPos + TabPos.Size + (EdgeWidth div 2) + 1, wypos + fTabHeight - 5);
          end;

          Pen.Color := clWindowFrame;
          if align<>alTop then
          begin
             MoveTo(TabPos.StartPos, 2);
             LineTo(TabPos.StartPos + TabPos.Size, 2);
          end;
        end;
      end;
    end;

     { draw onto the screen }
    Canvas.Draw(0, 0, MemBitmap);
  finally
    MemBitmap.free;
  end;
end;

procedure TrmTabSet.PaintEdge(Canvas: TCanvas; X, Y, H: Integer; Edge: TEdgeType);

  procedure DrawBR(Canvas: TCanvas; X, Y, H: integer; FillColor:TColor);
  begin
    with Canvas do
    begin
      Pen.Color := FillColor;
      Brush.Color := FillColor;
      Polygon([ Point(X + EdgeWidth, Y),
                Point(X + 1, Y + H),
                Point(X, Y + H),
                Point(X, Y),
                Point(X + EdgeWidth, Y)]);

      Pen.Color := clWindowFrame;
      PolyLine([Point(X + EdgeWidth, Y), Point(X, Y + H)]);

      if FillColor = SelectedColor then
      begin
         Pen.Color := clBtnShadow;
         PolyLine([Point(X + EdgeWidth - 1, Y), Point(X - 1, Y + H)]);
      end
      else
      begin
         Pen.Color := clBtnShadow;
         PolyLine([Point(X, 0), Point(X + EdgeWidth+1, 0)]);
         Pen.Color := clWindowFrame;
         PolyLine([Point(X, 1), Point(X + EdgeWidth+1, 1)]);
      end;
    end;
  end;

  procedure DrawBL(Canvas: TCanvas; X, Y, H: integer; FillColor:TColor);
  begin
    with Canvas do
    begin
      Pen.Color := FillColor;
      Brush.Color := FillColor;
      Polygon([ Point(X, Y),
                Point(X + EdgeWidth - 1, Y + H),
                Point(X + EdgeWidth, Y + H),
                Point(X + EdgeWidth, Y),
                Point(X, Y)]);

      Pen.Color := clWindowFrame;
      PolyLine([Point(X, Y), Point(X + EdgeWidth, Y + H)]);

      if Fillcolor = SelectedColor then
      begin
         Pen.Color := clBtnHighlight;
         PolyLine([Point(X + 1, Y), Point(X + EdgeWidth + 1, Y + H)]);
      end
      else
      begin
         Pen.Color := clBtnShadow;
         PolyLine([Point(X, 0), Point(X + EdgeWidth+1, 0)]);

         Pen.Color := clWindowFrame;
         PolyLine([Point(X, 1), Point(X + EdgeWidth+1, 1)]);
      end;

    end;
  end;

  procedure DrawTR(Canvas: TCanvas; X, Y, H: integer; FillColor:TColor);
  begin
    with Canvas do
    begin
      Pen.Color := FillColor;
      Brush.Color := FillColor;
      Polygon([ Point(X + EdgeWidth, Y + H),
                Point(X+1, Y),
                Point(X, Y),
                Point(X, Y + H),
                Point(X + EdgeWidth, Y + H)]);

      Pen.Color := clWindowFrame;
      PolyLine([ Point(X, Y), Point(X+EdgeWidth, Y + h)]);

      if FillColor = SelectedColor then
      begin
         Pen.Color := clBtnShadow;
         PolyLine([ Point(X-1, Y), Point(X+EdgeWidth-1, Y + h)]);
      end;
    end;
  end;

  procedure DrawTL(Canvas: TCanvas; X, Y, H: integer; FillColor:TColor);
  begin
    with Canvas do
    begin
      Pen.Color := FillColor;
      Brush.Color := FillColor;
      Polygon([ Point(X, Y + H),
                Point(X + EdgeWidth - 1, Y),
                Point(X + EdgeWidth, Y),
                Point(X + EdgeWidth, Y + H),
                Point(X, Y + H)]);

      Pen.Color := clWindowFrame;
      PolyLine([ Point(X+EdgeWidth, Y), Point(X, Y + h)]);

      if Fillcolor = SelectedColor then
      begin
         Pen.Color := clBtnHighlight;
         PolyLine([Point(x+edgewidth+1,y), Point(x+1,y+h)]);
      end;
    end;
  end;


begin
  Canvas.Brush.Color := clWhite;
  Canvas.Font.Color := clBlack;
  case align of
    alTop:
      begin
         case Edge of
           etFirstIsSel:
             DrawTL(Canvas, X, Y, H, SelectedColor);
           etLastIsSel:
             DrawTR(Canvas, X, Y, H, SelectedColor);
           etFirstNotSel:
             DrawTL(Canvas, X, Y, H, UnselectedColor);
           etLastNotSel:
             DrawTR(Canvas, X, Y, H, UnselectedColor);
           etNotSelToSel:
             begin
               DrawTR(Canvas, X, Y, H, UnselectedColor);
               DrawTL(Canvas, X, Y, H, SelectedColor);
             end;
           etSelToNotSel:
             begin
               DrawTL(Canvas, X, Y, H, UnselectedColor);
               DrawTR(Canvas, X, Y, H, SelectedColor);
             end;
           etNotSelToNotSel:
             begin
               DrawTL(Canvas, X, Y, H, UnselectedColor);
               DrawTR(Canvas, X, Y, H, UnselectedColor);
             end;
         end;
      end;
  else
    begin
       case Edge of
         etFirstIsSel:
           DrawBL(Canvas, X, Y, H, SelectedColor);
         etLastIsSel:
           DrawBR(Canvas, X, Y, H, SelectedColor);
         etFirstNotSel:
           DrawBL(Canvas, X, Y, H, UnselectedColor);
         etLastNotSel:
           DrawBR(Canvas, X, Y, H, UnselectedColor);
         etNotSelToSel:
           begin
             DrawBR(Canvas, X, Y, H, UnselectedColor);
             DrawBL(Canvas, X, Y, H, SelectedColor);
           end;
         etSelToNotSel:
           begin
             DrawBL(Canvas, X, Y, H, UnselectedColor);
             DrawBR(Canvas, X, Y, H, SelectedColor);
           end;
         etNotSelToNotSel:
           begin
             DrawBL(Canvas, X, Y, H, UnselectedColor);
             DrawBR(Canvas, X, Y, H, UnselectedColor);
           end;
       end;
    end;
  end;
end;

procedure TrmTabSet.CreateBrushPattern(Bitmap: TBitmap);
var
  X, Y: Integer;
begin
  Bitmap.Width := 8;
  Bitmap.Height := 8;
  with Bitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FBackgroundColor;
    FillRect(Rect(0, 0, Width, Height));
    if FDitherBackground then
      for Y := 0 to 7 do
        for X := 0 to 7 do
          if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
            Pixels[X, Y] := clWhite; { on even/odd rows }
  end;
end;

procedure TrmTabSet.FixTabPos;
var
  FLastVisibleTab: Integer;

  function GetRightSide: Integer;
  begin
    Result := Width - EndMargin;
    if AutoScroll and (FVisibleTabs < Tabs.Count - 1) then
      Dec(Result, FScroller.Width + 4);
  end;

  function ReverseCalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    Last: Integer): Integer;
  var
    W: Integer;
  begin
    if HandleAllocated then
    begin
      Result := Last;
      while (Start >= Stop) and (Result >= 0) do
        with Canvas do
        begin
          W := TextWidth(Tabs[Result]);
          if (FStyle = tsOwnerDraw) then MeasureTab(Result, W);
          Dec(Start, W + EdgeWidth); { next usable position }
          if Start >= Stop then Dec(Result);
        end;
      if (Start < Stop) or (Result < 0) then Inc(Result);
    end
    else
      Result := FFirstIndex;
  end;

begin
  if Tabs.Count > 0 then
  begin
    FLastVisibleTab := FFirstIndex + FVisibleTabs - 1;
    if FTabIndex > FLastVisibleTab then
      FFirstIndex := ReverseCalcNumTabs(GetRightSide, StartMargin + EdgeWidth,
        Canvas, FTabIndex)
    else if (FTabIndex >= 0) and (FTabIndex < FFirstIndex) then
      FFirstIndex := FTabIndex;
  end;
end;

procedure TrmTabSet.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetUnselectedColor(Value: TColor);
begin
  if Value <> FUnselectedColor then
  begin
    FUnselectedColor := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetBackgroundColor(Value: TColor);
begin
  if Value <> FBackgroundColor then
  begin
    FBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetDitherBackground(Value: Boolean);
begin
  if Value <> FDitherBackground then
  begin
    FDitherBackground := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then
  begin
    FAutoScroll := Value;
    FScroller.Visible := False;
    ShowWindow(FScroller.Handle, SW_HIDE);
    Invalidate;
  end;
end;

procedure TrmTabSet.SetStartMargin(Value: Integer);
begin
  if Value <> FStartMargin then
  begin
    FStartMargin := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetEndMargin(Value: Integer);
begin
  if Value <> FEndMargin then
  begin
    FEndMargin := Value;
    Invalidate;
  end;
end;

function TrmTabSet.CanChange(NewIndex: Integer): Boolean;
begin
  if TabEnabled(NewIndex) then
  begin
     Result := true;
     if Assigned(FOnChange) then
       FOnChange(Self, NewIndex, Result);
  end
  else
  result := false;
end;

procedure TrmTabSet.SetTabIndex(Value: Integer);
var
   newValue:integer;
   found : boolean;
   nCount : integer;
begin
  if Value <> FTabIndex then
  begin
    if (Value < -1) or (Value >= Tabs.Count) then
      raise Exception.Create(SInvalidTabIndex);

    if (value = -1) then
    begin
      FTabIndex := Value;
      FixTabPos;
      Click;
      Invalidate;
    end
    else
    if CanChange(Value) then
    begin
      FTabIndex := Value;
      FixTabPos;
      Click;
      Invalidate;
    end
    else
    begin
       found := false;
       newValue := Value+1;
       nCount := 0;
       while (newValue <> Value) do
       begin
          if newValue >= fTabs.count then
             newValue := 0;

          if (newValue < fTabs.count) and (not TabEnabled(newValue)) then
          begin
             inc(newValue);
             inc(nCount);
          end
          else
          begin
             found := true;
             break;
          end;

          if nCount >= fTabs.count then
          begin
             found := false;
             newValue := -1;
             break;
          end;
       end;
       if (found and CanChange(newValue)) or (not found) then
       begin
         FTabIndex := newValue;
         FixTabPos;
         Click;
         Invalidate;
       end;
    end;
  end;
end;

procedure TrmTabSet.SelectNext(Direction: Boolean);
var
  NewIndex: Integer;
begin
  if Tabs.Count > 1 then
  begin
    NewIndex := TabIndex;
    if Direction then
      Inc(NewIndex)
    else
      Dec(NewIndex);
    if NewIndex = Tabs.Count then
      NewIndex := 0
    else if NewIndex < 0 then
      NewIndex := Tabs.Count - 1;
    SetTabIndex(NewIndex);
  end;
end;

procedure TrmTabSet.SetFirstIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < Tabs.Count) then
  begin
    FFirstIndex := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetTabList(Value: TStrings);
begin
  FTabs.Assign(Value);
  FTabIndex := -1;
  if FTabs.Count > 0 then
    TabIndex := 0
  else
    Invalidate;
end;

procedure TrmTabSet.SetTabStyle(Value: TTabStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TrmTabSet.SetTabHeight(Value: Integer);
var
  SaveHeight: Integer;
begin
  if Value <> FOwnerDrawHeight then
  begin
    SaveHeight := FOwnerDrawHeight;
    try
      FOwnerDrawHeight := Value;
      FTabHeight := value;
      Invalidate;
    except
      FOwnerDrawHeight := SaveHeight;
      fTabHeight := SaveHeight;
      raise;
    end;
  end;
end;

procedure TrmTabSet.DrawTab(TabCanvas: TCanvas; R: TRect; Index: Integer;
  Selected: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabCanvas, R, Index, Selected);
end;

procedure TrmTabSet.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TrmTabSet.MeasureTab(Index: Integer; var TabWidth: Integer);
begin
  if Assigned(FOnMeasureTab) then
    FOnMeasureTab(Self, Index, TabWidth);
end;

procedure TrmTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  TabPos: TTabPos;
  I: Integer;
  Extra: Integer;
  MinLeft: Integer;
  MaxRight: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (((align<>alTop) and (Y <= FTabHeight)) or ((align=alTop) and (y >= clientheight-FTabHeight))) then
  begin
    if Y < FTabHeight div 2 then
      Extra := EdgeWidth div 3
    else
      Extra := EdgeWidth div 2;

    for I := 0 to TabPositions.Count - 1 do
    begin
      Pointer(TabPos) := TabPositions[I];
      MinLeft := TabPos.StartPos - Extra;
      MaxRight := TabPos.StartPos + TabPos.Size + Extra;
      if (X >= MinLeft) and (X <= MaxRight) and TabEnabled(FirstIndex + I) then
      begin
        SetTabIndex(FirstIndex + I);
        Break;
      end;
    end;
  end;
end;

procedure TrmTabSet.WMSize(var Message: TWMSize);
var
  NumVisTabs, LastTabPos: Integer;

  function CalcNumTabs(Start, Stop: Integer; Canvas: TCanvas;
    First: Integer): Integer;
  var
    W: Integer;
  begin
    Result := First;
    while (Start < Stop) and (Result < Tabs.Count) do
      with Canvas do
      begin
        W := TextWidth(Tabs[Result]);
        if (FStyle = tsOwnerDraw) then MeasureTab(Result, W);
        Inc(Start, W + EdgeWidth); { next usable position }
        if Start <= Stop then Inc(Result);
      end;
  end;

begin
  inherited;
  if Tabs.Count > 1 then
  begin
    LastTabPos := Width - EndMargin;
    NumVisTabs := CalcNumTabs(StartMargin + EdgeWidth, LastTabPos, Canvas, 0);
    if (FTabIndex = Tabs.Count) or (NumVisTabs > FVisibleTabs) or
      (NumVisTabs = Tabs.Count) then FirstIndex := Tabs.Count - NumVisTabs;
    FDoFix := True;
  end;
  Invalidate;
end;

procedure TrmTabSet.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Invalidate;
end;

procedure TrmTabSet.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

procedure TrmTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
  begin
    if IsAccel(Message.CharCode, FTabs[I]) then
    begin
      Message.Result := 1;
      if FTabIndex <> I then
        SetTabIndex(I);
      Exit;
    end;
  end;
  inherited;
end;

procedure TrmTabSet.DefineProperties(Filer: TFiler);
begin
  { Can be removed after version 1.0 }
  if Filer is TReader then inherited DefineProperties(Filer);
  Filer.DefineProperty('TabOrder', ReadIntData, nil, False);
  Filer.DefineProperty('TabStop', ReadBoolData, nil, False);
end;

procedure TrmTabSet.ReadIntData(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TrmTabSet.ReadBoolData(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TrmTabSet.SetTabType(const Value: TTabType);
begin
  fTabType := Value;
  Invalidate;
end;

procedure TrmTabSet.SetDisabledTabList(const Value: TStrings);
begin
  FDisabledTabs.Assign(Value);
  Invalidate;
end;

function TrmTabSet.TabEnabled(index: integer): boolean;
begin
   result := FDisabledTabs.IndexOf(fTabs[index]) = -1;
end;

function TrmTabSet.Calc2kTabPositions(Start, Stop: Integer;
  Canvas: TCanvas; First: Integer): Integer;
var
  Index: Integer;
  TabPos: TTabPos;
  W: Integer;
begin
  TabPositions.Count := 0; { erase all previously cached data }
  Index := First;
  while (Start < Stop) and (Index < Tabs.Count) do
  begin
    with Canvas do
    begin
      if TabEnabled(index) then
      begin
         TabPos.StartPos := Start;
         W := TextWidth(Tabs[Index]);

         { Owner }
         if (FStyle = tsOwnerDraw) then MeasureTab(Index, W);

         TabPos.Size := W;
         Inc(Start, TabPos.Size + EdgeWidth); { next usable position }
      end;

      if Start <= Stop then
      begin
        TabPositions.Add(Pointer(TabPos)); { add to list }
        Inc(Index);
      end;
    end;
  end;
  Result := Index - First;
end;

procedure TrmTabSet.SetEdgeWidth(const Value: integer);
begin
  fEdgeWidth := Value;
  Invalidate;
end;

end.

