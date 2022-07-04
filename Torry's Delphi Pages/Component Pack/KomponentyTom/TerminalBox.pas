unit TerminalBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls, Forms,
  SyncObjs;

type

  TTerminalBox = class;

  TObjectItem = class
    Color1: TColor;
    Color2: TColor;
    LinkObject: TObject;
    private
      procedure FreeEndNil;
  end;

  TPaintTextMode = (ptOnlyUse, ptFull, ptAddLine);

  TTermStrings = class(TStringList)
  private
    Term: TTerminalBox;
    FNoEndUpdate: Boolean;
    MaxWidthLine: Integer;
    function GetObjectItem(Index: Integer): TObjectItem;
    procedure UpdateTerm(AMode: TPaintTextMode);
    procedure ResetTerm(Redraw: Boolean);
    procedure CalcLenLine;
    function GetWidthLine(S: string): Integer;
    function IsHorizScroll: Boolean;
  protected
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetTextStr(const Value: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
    property ObjectItems[Index: Integer]: TObjectItem read GetObjectItem;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

  TMonitorLine = class(TPersistent)
  private
    FEnabled: Boolean;
    FDelete: Integer;
    FMax: Integer;
    Term: TTerminalBox;
    procedure IncCount;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Max: Integer read FMax write FMax default 1024;
    property Delete: Integer read FDelete write FDelete default 256;
  end;

{
  TThreadeKol = class(TThread)
  private
    KolejkaStr: TStringList;
    Lock: TCriticalSection;
    Terminal: TTerminalBox;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Push(const AStr: string; const AObject: TObject);
    function Pop(var AStr: string; var AObj: TObject): Boolean;
    procedure Pop2;
  end;
}

  TColorLine = record
    ColorFont: TColor;
    ColorBack: TColor;
  end;

  TTerminalBox = class(TCustomControl)
  private
    { Private declarations }
//    ThKol: TThreadeKol;
    FStrings: TStrings;
    FColorFont: TColor;
    FColorBack: TColor;
    FFullLineDraw: Boolean;
    FAlignment: TAlignment;
    FDrawGrid: Boolean;
    FOrigX: Integer;
    FOrigY: Integer;
    FHeightText: Integer;
    FScrollBars: TScrollStyle;
    FBorderStyle: TBorderStyle;
    FMonitor: TMonitorLine;
    IsCalcHeightTxt: Boolean;
    NoChangeFont: Boolean;
    OldOnChangeFont: TNotifyEvent;
//    TempTimer: Integer;
    FDefColorBack: Boolean;
    FAutoScroll: Boolean;
    FAutoShowScrollBar: Boolean;
    procedure ResetOrig;
    procedure SetStrings(const Value: TStrings);
    function GetHeightText: Integer;
    function GetVisibleLine: Integer;
    procedure DrawGridLine;
    procedure DrawLine(AIndex: Integer; AStr: string;
      AColorFont, AColorBack: TColor);
    procedure ClearLine(AIndex: Integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetDrawGrid(const Value: Boolean);
    procedure SetFullLineDraw(const Value: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure DoChangeFont(Sender: TObject);
    procedure CalcHeightText;
    procedure CalcWidthText;
    procedure SetLineStyle(const Value: TPenStyle);
    function GetLineStyle: TPenStyle;
    function GetColorLine: TColor;
    procedure SetColorLine(const Value: TColor);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetDefColorBack(const Value: Boolean);
    procedure SetAlignment(const Value: TAlignment);
    function GetColorsLine(Index: Integer): TColorLine;
    procedure SetColorsLine(Index: Integer; const Value: TColorLine);
    procedure SetAutoShowScrollBar(const Value: Boolean);
//    procedure AddObjectTh(AString: string; AObject: TObject);
  protected
    { Protected declarations }
    procedure UpdateVScrollBar(Redraw: Boolean);
    procedure UpdateHScrollBar(Redraw: Boolean);
    procedure PaintText(AMode: TPaintTextMode);
    function IsVisibleLine(Index: Integer): Boolean;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Refresh;
    procedure RefreshText;
    procedure Clear; virtual;
    procedure AddString(AString: string);
    procedure AddObject(AString: string; AObject: TObject);
    procedure InsertString(Index: Integer; AString: string);
    procedure InsertObject(Index: Integer; AString: string; AObject: TObject);
    procedure GoToEnd;
    property Colors[Index: Integer]: TColorLine read GetColorsLine write SetColorsLine;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Monitor: TMonitorLine read FMonitor write FMonitor;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Font: TFont read GetFont write SetFont;
    property Lines: TStrings read FStrings write SetStrings;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Color: TColor read GetColor write SetColor;
    property ColorFont: TColor read FColorFont write FColorFont;
    property ColorBack: TColor read FColorBack write FColorBack;
    property ColorLine: TColor read GetColorLine write SetColorLine;
    property DefColorBack: Boolean read FDefColorBack write SetDefColorBack;
    property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid;
    property FullLineDraw: Boolean read FFullLineDraw write SetFullLineDraw;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property AutoShowScrollBar: Boolean read FAutoShowScrollBar write SetAutoShowScrollBar;
  end;

implementation
uses Types, RTLConsts;

const
  SB_TEMP_USE_BOTTOM = 16;

{ TTerminalBox }

constructor TTerminalBox.Create(AOwner: TComponent);

begin
  inherited;
//  ThKol:= TThreadeKol.Create(False);
//  ThKol.Terminal:= Self;
  FAutoScroll:= True;
  Width:= 200;
  Height:= 200;
  FMonitor:= TMonitorLine.Create;
  FMonitor.Term:= Self;
  FMonitor.Max:= 1024;
  FMonitor.Delete:= 256;
  ParentFont:= False;
  Color:= clBlack;
  FColorBack:= Color;
  Canvas.Font.Color:= FColorFont;
  Canvas.Pen.Color:= clGray;
  FDrawGrid:= True;
  FBorderStyle := bsSingle;
  FStrings:= TTermStrings.Create;
  TTermStrings(FStrings).Term:= Self;
  OldOnChangeFont:= Canvas.Font.OnChange;
  Canvas.Font.OnChange:= DoChangeFont;
end;

procedure TTerminalBox.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

destructor TTerminalBox.Destroy;
begin
  FStrings.Free;
//  ThKol.Free;
  inherited;
end;

procedure TTerminalBox.DrawGridLine;
var i, h, l, y, temp: Integer;

begin
  h:= GetHeightText;
  l:= GetVisibleLine;
  temp:= 0;
  with Canvas do
  begin
    for i:= 1 to l do
    begin
      y:= i * h + temp;
      MoveTo(0, y);
      LineTo(ClientWidth, y);
      Inc(temp);
    end;
  end;
end;

procedure TTerminalBox.DrawLine(AIndex: Integer; AStr: string;
  AColorFont, AColorBack: TColor);

const
  Alignments: array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);

var R: TRect;

begin
  with Canvas do
  begin
    NoChangeFont:= True;
    try
      Font.Color:= AColorFont;
    finally
      NoChangeFont:= False;
    end;
    R.Left:= -FOrigX;
    R.Top:= AIndex * (GetHeightText + 1);
    R.Right:= ClientWidth;
    R.Bottom:= R.Top + GetHeightText;
    if not FullLineDraw then
    begin
      Brush.Color:= Color;
      Brush.Style:= bsSolid;
      FillRect(R);
      if not FDefColorBack then Brush.Color:= AColorBack;
    end
    else begin
      Brush.Style:= bsSolid;
      if FDefColorBack then
        Brush.Color:= Color
      else
        Brush.Color:= AColorBack;
      FillRect(R);
      Brush.Style:= bsClear;
    end;
//    ExtTextOut(Handle, R.Left, R.Top, ETO_OPAQUE, @R, PChar(AStr), Length(AStr), 0);
    DrawText(Handle, PChar(AStr), -1, R, DT_SINGLELINE or Alignments[Alignment]);
  end;
end;

procedure TTerminalBox.ClearLine(AIndex: Integer);
var R: TRect;
begin
  with Canvas do
  begin
    R.Left:= 0;
    R.Top:= AIndex * (GetHeightText + 1);
    R.Right:= ClientWidth;
    R.Bottom:= R.Top + GetHeightText;
    Brush.Color:= Color;
    Brush.Style:= bsSolid;    
    FillRect(R);
  end;
end;

function TTerminalBox.GetFont: TFont;
begin
  Result:= Canvas.Font;
end;

function TTerminalBox.GetHeightText: Integer;
begin
  if not IsCalcHeightTxt then CalcHeightText;
  Result:= FHeightText;
end;

function TTerminalBox.GetVisibleLine: Integer;
begin
  Result:= ClientHeight div (GetHeightText + 1);
end;

procedure TTerminalBox.GoToEnd;
begin
  SendMessage(Handle, WM_VSCROLL, SB_TEMP_USE_BOTTOM, 0);
end;

function TTerminalBox.IsVisibleLine(Index: Integer): Boolean;
var delta: Integer;
begin
  delta:= Index - FOrigY;
  Result:= (delta >= 0) and (delta < GetVisibleLine);
end;

procedure TTerminalBox.Paint;
begin
  if FDrawGrid then DrawGridLine;
  PaintText(ptOnlyUse);
end;

procedure TTerminalBox.PaintText(AMode: TPaintTextMode);
var i, l: Integer;
    O: TObjectItem;

begin
  l:= GetVisibleLine;
  if (FStrings.Count - FOrigY) < l then l:= FStrings.Count - FOrigY;
  with TTermStrings(FStrings) do
  for i:= 0 to l - 1 do
  begin
    O:= GetObjectItem(i + FOrigY);
    DrawLine(i, Strings[i + FOrigY], O.Color1, O.Color2);
  end;
  case AMode of
    ptFull:
       for i:= l to GetVisibleLine - 1 do ClearLine(i);
    ptAddLine:
      if l <= GetVisibleLine then ClearLine(l);
  end;
end;

procedure TTerminalBox.SetDrawGrid(const Value: Boolean);
begin
  if FDrawGrid <> Value then
  begin
    FDrawGrid := Value;
    Invalidate;
  end;
end;

procedure TTerminalBox.SetFont(const Value: TFont);
begin
  Canvas.Font:= Value;
end;

procedure TTerminalBox.SetFullLineDraw(const Value: Boolean);
begin
  if FFullLineDraw <> Value then
  begin
    FFullLineDraw := Value;
    Invalidate;
  end;
end;

procedure TTerminalBox.SetStrings(const Value: TStrings);
begin
  if Value is TStrings then FStrings.Assign(Value);
end;

procedure TTerminalBox.UpdateVScrollBar(Redraw: Boolean);
var SI, SIOld: TScrollInfo;
begin
  GetScrollInfo(Handle, SB_VERT, SIOld);
  SI.fMask:= SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
  SI.nPage:= GetVisibleLine;
  SI.cbSize:= SizeOf(SI);
  SI.nMin:= 0;
  SI.nMax:= Lines.Count - 1;
  SI.nPos:= FOrigY;
  if (SIOld.nMax <> SI.nMax) or (SIOld.nPage <> SI.nPage) then
    SetScrollInfo(Handle, SB_VERT, SI, Redraw);
  if (FOrigY > 0) and (SI.nPage >= SI.nMax) then FOrigY:= 0;
end;

procedure TTerminalBox.UpdateHScrollBar(Redraw: Boolean);
var SI, SIOld: TScrollInfo;
begin
  GetScrollInfo(Handle, SB_HORZ, SIOld);
  SI.fMask:= SIF_RANGE or SIF_PAGE or SIF_DISABLENOSCROLL;
  SI.nMax:= TTermStrings(FStrings).MaxWidthLine;
  SI.nPage:= ClientWidth;
  SI.cbSize:= SizeOf(SI);
  SI.nMin:= 0;
  SI.nPos:= FOrigX;
  if (SIOld.nMax <> SI.nMax) or (SIOld.nPage <> SI.nPage) then
  begin
    if FAutoShowScrollBar then ShowScrollBar(Handle, SB_HORZ, SI.nPage < SI.nMax);
    SetScrollInfo(Handle, SB_HORZ, SI, Redraw);
  end;
  if (FOrigX > 0) and (SI.nPage >= SI.nMax) then FOrigX:= 0;
end;

{ TMemoStrings }

procedure TTermStrings.CalcLenLine;
var i, lo: Integer;
begin
  MaxWidthLine:= 0;
  for i:= 0 to Count - 1 do
  begin
    lo:= GetWidthLine(Get(i));
    if lo > MaxWidthLine then MaxWidthLine:= lo;
  end;
end;

procedure TTermStrings.Clear;
var i: Integer;
    oj: TObjectItem;

begin
  for i:= 0 to Count - 1 do
  begin
    oj:= ObjectItems[i];
    if oj <> nil then oj.FreeEndNil;
  end;
  inherited;
  ResetTerm(UpdateCount = 0);
end;

procedure TTermStrings.Delete(Index: Integer);
var Old: Integer;
    oj: TObjectItem;
begin
  oj:= ObjectItems[Index];
  if oj <> nil then oj.FreeEndNil;
  inherited;
  if UpdateCount = 0 then
  begin
    if Term.IsVisibleLine(Index) then
    begin
      if Count < Term.GetVisibleLine then
        UpdateTerm(ptAddLine)
      else begin
        if (Term.FOrigY > 0) and
           ((Count - Term.FOrigY) < Term.GetVisibleLine) then
        begin
          Dec(Term.FOrigY);
          SetScrollPos(Term.Handle, SB_VERT, Term.FOrigY, True);
        end;
        UpdateTerm(ptFull);
      end;
    end else begin
      if Term.FOrigY > 0 then
      begin
        if Index < Term.FOrigY then UpdateTerm(ptFull);
      end else
        Term.UpdateVScrollBar(True);
    end;
    if IsHorizScroll then
    begin
      Old:= MaxWidthLine;
      CalcLenLine;
      if Old <> MaxWidthLine then ResetTerm(True);
    end;
  end;
end;

function TTermStrings.GetObject(Index: Integer): TObject;
begin
  Result:= GetObjectItem(Index).LinkObject;
end;

function TTermStrings.GetObjectItem(Index: Integer): TObjectItem;
begin
  Result:= TObjectItem(inherited GetObject(Index));
end;

function TTermStrings.GetWidthLine(S: string): Integer;
begin
  Result:= Term.Canvas.TextWidth(S);
end;

procedure TTermStrings.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
var
  O: TObjectItem;
  l: Integer;
begin
  O:= TObjectItem.Create;
  with O do
  begin
    Color1:= Term.ColorFont;
    Color2:= Term.ColorBack;
    LinkObject:= AObject;
  end;
  inherited InsertItem(Index, S, O);
  if IsHorizScroll then
  begin
    l:= GetWidthLine(S);
    if l > MaxWidthLine then
    begin
     MaxWidthLine:= l;
     if UpdateCount = 0 then Term.UpdateHScrollBar(False);
    end;
  end;
  if UpdateCount = 0 then
  begin
    if Term.IsVisibleLine(Index) then
    begin
      if Index = Count - 1 then
      begin
        Term.DrawLine(Index, Get(Index), O.Color1, O.Color2);
        Term.UpdateVScrollBar(True);
      end else
        UpdateTerm(ptAddLine);
    end else
      Term.UpdateVScrollBar(True);
  end;
end;


function TTermStrings.IsHorizScroll: Boolean;
begin
  Result:= Term.ScrollBars in [ssHorizontal, ssBoth];
end;

procedure TTermStrings.Put(Index: Integer; const S: string);
var O: TObjectItem;
    Old: Integer;
begin
  inherited;
  if IsHorizScroll then
  begin
    Old:= MaxWidthLine;
    CalcLenLine;
    if Old <> MaxWidthLine then Term.UpdateHScrollBar(UpdateCount = 0);
  end;
  O:= GetObjectItem(Index);
  with Term, O do
  begin
    Color1:= ColorFont;
    Color2:= ColorBack;
    if (UpdateCount = 0) and IsVisibleLine(Index) then
      DrawLine(Index - FOrigY, Get(Index), Color1, Color2);
  end;
end;

procedure TTermStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
  Changing;
  GetObjectItem(Index).LinkObject:= AObject;
  Changed;
end;

procedure TTermStrings.ResetTerm(Redraw: Boolean);
begin
  with Term do
  begin
    ResetOrig;
    if Redraw then PaintText(ptFull);
    if IsHorizScroll then CalcLenLine;
    UpdateVScrollBar(Redraw);
    UpdateHScrollBar(Redraw);
  end;
end;

procedure TTermStrings.SetTextStr(const Value: string);
begin
  BeginUpdate;
  try
    inherited;
  finally
    FNoEndUpdate:= True;
    try
      EndUpdate;
    finally
      FNoEndUpdate:= False;
    end;
  end;
  ResetTerm(UpdateCount = 0);
end;

procedure TTermStrings.SetUpdateState(Updating: Boolean);
begin
  if not Updating and not FNoEndUpdate then ResetTerm(True);
end;

procedure TTermStrings.UpdateTerm(AMode: TPaintTextMode);
begin
  Term.PaintText(AMode);
  Term.UpdateVScrollBar(True);
end;

procedure TTerminalBox.Refresh;
begin
  Paint;
end;

procedure TTerminalBox.RefreshText;
begin
  PaintText(ptFull);
end;

procedure TTerminalBox.WMSize(var Message: TWMSize);
begin
  inherited;
  if FScrollBars in [ssVertical, ssBoth] then
  begin
//    Perform(WM_VSCROLL, SB_TOP, 0);
    UpdateVScrollBar(True);
  end;
  if FScrollBars in [ssHorizontal, ssBoth] then
  begin
//    Perform(WM_HSCROLL, SB_TOP, 0);
    UpdateHScrollBar(True);       
  end;
end;

procedure TTerminalBox.ResetOrig;
begin
  FOrigX:= 0;
  FOrigY:= 0;
end;

procedure TTerminalBox.Clear;
begin
  FStrings.Clear;
end;

procedure TTerminalBox.AddString(AString: string);
begin
  AddObject(AString, nil);
end;

procedure TTerminalBox.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    if FScrollBars = ssVertical then UpdateVScrollBar(False);
    if FScrollBars = ssHorizontal then UpdateVScrollBar(False);
    if FScrollBars = ssBoth then
    begin
      UpdateVScrollBar(False);    
      UpdateHScrollBar(False);
    end;
    RecreateWnd;
  end;
end;

procedure TTerminalBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TTerminalBox.WMHScroll(var Message: TWMHScroll);

  procedure _Rigth;
  begin
    FOrigX:= TTermStrings(FStrings).MaxWidthLine - ClientWidth;
    PaintText(ptOnlyUse);
  end;

  procedure _Left;
  begin
    FOrigX:= 0;
    PaintText(ptOnlyUse);
  end;

  procedure InLeft;
  begin
    if FOrigX < TTermStrings(FStrings).MaxWidthLine - ClientWidth then
    begin
      Inc(FOrigX, Canvas.TextWidth('X'));
      PaintText(ptOnlyUse);
    end;
  end;

  procedure InRigth;
  begin
    if FOrigX > 0 then
    begin
      Dec(FOrigX, Canvas.TextWidth('X'));
      PaintText(ptOnlyUse);
    end;
  end;


  procedure ThumbPos;
  begin
    FOrigX:= Message.Pos;
    PaintText(ptOnlyUse);
  end;

  procedure ThumbTrack;
  begin
    FOrigX:= Message.Pos;
    PaintText(ptOnlyUse);
  end;

  procedure PageLeft;
  begin
    if FOrigX < TTermStrings(FStrings).MaxWidthLine - ClientWidth then
    begin
      Inc(FOrigX, ClientWidth);
      PaintText(ptOnlyUse);
    end;
  end;

  procedure PageRight;
  begin
    Dec(FOrigX, ClientWidth);
    PaintText(ptOnlyUse);
  end;

begin
  case Message.ScrollCode of
    SB_BOTTOM: _Rigth;
    SB_TOP: _Left;
    SB_LINEDOWN: InLeft;
    SB_LINEUP: InRigth;
    SB_THUMBPOSITION: ThumbPos;
    SB_THUMBTRACK: ThumbTrack;
    SB_PAGEDOWN: PageLeft;
    SB_PAGEUP: PageRight;
  end;
  if GetScrollPos(Handle, SB_HORZ) <> FOrigX then
    SetScrollPos(Handle, SB_HORZ, FOrigX, True);
end;

procedure TTerminalBox.WMVScroll(var Message: TWMVScroll);
//var
//  CopTempTimer: Integer;

  procedure Bottom;
  begin
    FOrigY:= FStrings.Count - GetVisibleLine;
    if FOrigY < 0 then FOrigY:= 0;
    PaintText(ptOnlyUse);
  end;

  procedure Top;
  begin
    FOrigY:= 0;
    PaintText(ptOnlyUse);
  end;

  procedure LineDown;
  begin
    if FOrigY + GetVisibleLine < FStrings.Count then
    begin
      Inc(FOrigY);
      PaintText(ptOnlyUse);
    end;
  end;

  procedure LineUp;
  begin
    if FOrigY > 0 then
    begin
      Dec(FOrigY);
      PaintText(ptOnlyUse);
    end;
  end;

  procedure PageDown;
  var Old: Integer;
  begin
    if FOrigY + GetVisibleLine * 2 < FStrings.Count then
    begin
      Inc(FOrigY, GetVisibleLine);
      PaintText(ptOnlyUse);
    end else begin
      Old:= FOrigY;
      FOrigY:= FStrings.Count - GetVisibleLine;
      if Old <> FOrigY then PaintText(ptOnlyUse);
    end;
  end;

  procedure PageUp;
  begin
    if FOrigY > 0 then
    begin
      Dec(FOrigY, GetVisibleLine);
      if FOrigY < 0 then FOrigY:= 0;
      PaintText(ptOnlyUse);
    end;
  end;

  procedure ThumbPos;
  begin
    FOrigY:= Message.Pos;
    PaintText(ptOnlyUse);
  end;

  procedure ThumbTrack;
  begin
    FOrigY:= Message.Pos;
    PaintText(ptOnlyUse);
  end;

  procedure TempUseBottom;
  begin
     Bottom;  
//
//    if TempTimer > 0 then
//    begin
//       CopTempTimer:= TempTimer;
//       Dec(TempTimer);
//    end else
//      CopTempTimer:= 0;
//    FOrigY:= FStrings.Count - GetVisibleLine;
//    if TempTimer = 0 then PaintText(ptFull);

  end;

begin
  case Message.ScrollCode of
    SB_BOTTOM: Bottom;
    SB_TOP: Top;
    SB_LINEDOWN: LineDown;
    SB_LINEUP: LineUp;
    SB_PAGEDOWN: PageDown;
    SB_PAGEUP: PageUp;
    SB_THUMBPOSITION: ThumbPos;
    SB_THUMBTRACK: ThumbTrack;
    SB_TEMP_USE_BOTTOM: TempUseBottom;
  end;
  if GetScrollPos(Handle, SB_VERT) <> FOrigY then
  begin
//    if Message.ScrollCode <> SB_TEMP_USE_BOTTOM then
      SetScrollPos(Handle, SB_VERT, FOrigY, True)
//    else
//      SetScrollPos(Handle, SB_VERT, FOrigY, TempTimer = 0);
  end;// else
//    if CopTempTimer = 1 then
//      SetScrollPos(Handle, SB_VERT, FOrigY, True);
end;

procedure TTerminalBox.CalcHeightText;
begin
  FHeightText:= Canvas.TextHeight('Aj');
  IsCalcHeightTxt:= True;
end;

procedure TTerminalBox.CalcWidthText;
begin
  TTermStrings(FStrings).CalcLenLine;
end;

procedure TTerminalBox.DoChangeFont(Sender: TObject);
begin
  if Assigned(OldOnChangeFont) then OldOnChangeFont(Sender);
  if not NoChangeFont then
  begin
    IsCalcHeightTxt:= False;
//    CalcHeightText;
    CalcWidthText;
    Invalidate;
  end;
end;

procedure TTerminalBox.SetLineStyle(const Value: TPenStyle);
begin
  if Canvas.Pen.Style <> Value then
  begin
    Canvas.Pen.Style := Value;
    Canvas.Brush.Color:= Color;
    Canvas.Brush.Style:= bsSolid;
    if FDrawGrid then Invalidate;
  end;
end;

function TTerminalBox.GetLineStyle: TPenStyle;
begin
  Result:= Canvas.Pen.Style;
end;

function TTerminalBox.GetColorLine: TColor;
begin
  Result:= Canvas.Pen.Color;
end;

procedure TTerminalBox.SetColorLine(const Value: TColor);
begin
  if Canvas.Pen.Color <> Value then
  begin
    Canvas.Pen.Color:= Value;
    Invalidate;
  end;
end;

function TTerminalBox.GetColor: TColor;
begin
  Result:= inherited Color;
end;

procedure TTerminalBox.SetColor(const Value: TColor);
begin
  inherited Color:= Value;
end;

procedure TTerminalBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  DoChangeFont(nil);
end;

procedure TTerminalBox.SetDefColorBack(const Value: Boolean);
begin
  if FDefColorBack <> Value then
  begin
    FDefColorBack := Value;
    Invalidate;
  end;
end;

procedure TTerminalBox.WMMouseWheel(var Message: TWMMouseWheel);
var D: Smallint;
begin
  if FScrollBars in [ssVertical, ssBoth] then
  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if Message.WheelDelta > 0 then
        D:= 1
      else
        D:= -1;
      if (D <> 0) and (SetScrollPos(Handle, SB_VERT, FOrigY - D, True) <> -1) then
      begin
        FOrigY:= GetScrollPos(Handle, SB_VERT);
        PaintText(ptOnlyUse);
      end;
    end;
  end;
end;

procedure TTerminalBox.AddObject(AString: string; AObject: TObject);
begin
//  ThKol.Push(AString, AObject);
  FStrings.AddObject(AString, AObject);
  if FMonitor.FEnabled then FMonitor.IncCount;
  if FStrings.Count > GetVisibleLine then
  begin
//    Inc(TempTimer);
//    PostMessage(Handle, WM_VSCROLL, SB_TEMP_USE_BOTTOM, 0);
    SendMessage(Handle, WM_VSCROLL, SB_TEMP_USE_BOTTOM, 0);
  end else
    PaintText(ptOnlyUse);
end;

procedure TTerminalBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

{
procedure TTerminalBox.AddObjectTh(AString: string; AObject: TObject);
begin
  FStrings.AddObject(AString, AObject);
  if FMonitor.FEnabled then FMonitor.IncCount;
  if FStrings.Count > GetVisibleLine then
  begin
//    Inc(TempTimer);
//    PostMessage(Handle, WM_VSCROLL, SB_TEMP_USE_BOTTOM, 0);
    SendMessage(Handle, WM_VSCROLL, SB_TEMP_USE_BOTTOM, 0);
  end else
    PaintText(ptOnlyUse);
end;
}

function TTerminalBox.GetColorsLine(Index: Integer): TColorLine;
begin
  with Result do
  begin
    ColorFont:= TTermStrings(FStrings).GetObjectItem(Index).Color1;
    ColorBack:= TTermStrings(FStrings).GetObjectItem(Index).Color2;
  end;
end;

procedure TTerminalBox.SetColorsLine(Index: Integer;
  const Value: TColorLine);
var
  i: Integer;
begin
  TTermStrings(FStrings).GetObjectItem(Index).Color1:= Value.ColorFont;
  TTermStrings(FStrings).GetObjectItem(Index).Color2:= Value.ColorBack;
  i:= Index - FOrigY;
  if (i >= 0) and (i < GetVisibleLine) then
    DrawLine(i, Lines[index], Value.ColorFont, Value.ColorBack);
end;

procedure TTerminalBox.InsertObject(Index: Integer; AString: string;
  AObject: TObject);
begin
  FStrings.InsertObject(Index, AString, AObject);
  if FMonitor.FEnabled then FMonitor.IncCount;
  if Index <= (FOrigY + GetVisibleLine) then PaintText(ptOnlyUse);
end;

procedure TTerminalBox.InsertString(Index: Integer; AString: string);
begin
  InsertObject(Index, AString, nil);
end;

procedure TTerminalBox.SetAutoShowScrollBar(const Value: Boolean);
begin
  if FAutoShowScrollBar <> Value then
  begin
    FAutoShowScrollBar := Value;
    UpdateHScrollBar(True);
  end;
end;

{ TMonitorLine }

procedure TMonitorLine.IncCount;
var i: Integer;
begin
  with Term.Lines do
  if Count > FMax then
  begin
    BeginUpdate;
    try
      for i:= 1 to FDelete do Delete(0);
    finally
      EndUpdate;
    end;
  end;
end;

{ TThreadeKol }
{
constructor TThreadeKol.Create(CreateSuspended: Boolean);
begin
  inherited;
  Lock:= TCriticalSection.Create;
  KolejkaStr:= TStringList.Create;
end;

destructor TThreadeKol.Destroy;
begin
  KolejkaStr.Free;
  Lock.Free;
  inherited;
end;

procedure TThreadeKol.Execute;
var S: string;
    O: TObject;
    c: Integer;

begin
  while not Terminated do
    if Terminal <> nil then
    begin
      if KolejkaStr.Count > 0 then
      begin
  //      Lock.Enter;
        try
          c:= KolejkaStr.Count;
          while c > 0 do
          begin
            Pop2;
            c:= KolejkaStr.Count;
          end;
        finally
    //      Lock.Leave;
        end;
      end else
        Sleep(1);
    end
    else
      Sleep(1);
end;

procedure TThreadeKol.Pop2;
begin
  Terminal.AddObjectTh(KolejkaStr[0], KolejkaStr.Objects[0]);
  KolejkaStr.Delete(0);
end;

function TThreadeKol.Pop(var AStr: string; var AObj: TObject): Boolean;
begin
  Lock.Enter;
  try
    if KolejkaStr.Count > 0 then
    begin
      Result:= True;
      AStr:= KolejkaStr[0];
      AObj:= KolejkaStr.Objects[0];
      KolejkaStr.Delete(0);
    end else begin
      Result:= False;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TThreadeKol.Push(const AStr: string; const AObject: TObject);
begin
  Lock.Enter;
  try
    KolejkaStr.AddObject(AStr, Aobject);
  finally
    Lock.Leave;
  end;
end;
}

{ TObjectItem }

procedure TObjectItem.FreeEndNil;
begin
  FreeAndNil(Self);
end;

end.

