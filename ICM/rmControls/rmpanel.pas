{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmPanel
Purpose  : This is a regular panel that has a splitter bar on the oppositly
           aligned side
Date     : 07-10-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmPanel;

interface

uses windows, messages, graphics, classes, forms, controls, sysutils, extctrls;

{$I CompilerDefines.INC}

type
  TrmCaptionPosition = (cpStandard, cpTopEdge);

  TrmPanel = class(TCustomPanel)
  private
    { Private }
    FActiveControl: TWinControl;
    fCapPos : TrmCaptionPosition;
    FBrush: TBrush;
    FDeltaPos: integer;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible, fPanelSizing: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnMoved: TNotifyEvent;
    fonVisibleChanged: TNotifyEvent;
    fResizeBtn: boolean;
    fLastOpenSize: integer;
    fMouseOverBtn: boolean;
    fBtnDown: boolean;
    fDotCount: integer;
    fResizing: boolean;
    fSplitterPanel: boolean;
    fIBWidth: integer;
    fBtnOpenClose : boolean;
    fOnCanResize : TCanResizeEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function CanResize(var NewSize: Integer): Boolean; {$IFDEF D4_OR_HIGHER} reintroduce; {$ENDIF} virtual;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure SetResizeBtn(const Value: boolean);
    procedure SetDotCount(const Value: integer);
    procedure SetSplitterPanel(const Value: boolean);
    procedure SetCapPos(const Value: TrmCaptionPosition);
    procedure SetIBWidth(const Value: integer);
    procedure UpdateSize(X, Y: Integer);
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure ReleaseLineDC;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StopSizing;
    function DoCanResize(var NewSize: Integer): Boolean;
    procedure UpdateControlSize;
    function Convert(wRect:TRect):TRect;

    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize) ; message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage) ; message WM_NCPAINT;
    procedure WMNCHitTest(var Message: TWMNCHitTest) ; message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown) ; message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove) ; message WM_NCMOUSEMOVE;
    procedure wmEraseBkgrnd(var Msg:TMessage); message WM_EraseBkgnd;

    procedure WMLButtonUp(var Message: TWMLButtonUp) ; message WM_LBUTTONUP;
  protected
    { Protected }
    function GripSize: integer;
    function GripRect: TRect;
    function BtnRect: TRect;
    procedure PaintGrip;
    function GetClosedState: boolean;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public }
    constructor create(AOwner: TComponent); override;
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property DockManager;
    property IsClosed : boolean read GetClosedState;
    property BtnOpenClose : boolean read fBtnOpenClose;
    procedure ClosePanel;
    procedure OpenPanel;
  published
    { Published }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter default bvNone;
    property BevelWidth;
    property BorderStyle;
    property BiDiMode;
    property BorderWidth;
    property InternalBorderWidth : integer read fIBWidth write SetIBWidth default 0;
    property Caption;
    property CaptionPosition : TrmCaptionPosition read fCapPos write SetCapPos default cpStandard;
    property Color;
    property Constraints;
    property Ctl3D;
    property SplitterPanel: boolean read fSplitterPanel write SetSplitterPanel default false;
    property UseDockManager default True;
    property DotCount: integer read fDotCount write SetDotCount default 10;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsPattern;
    property ResizeBtn: boolean read fResizeBtn write SetResizeBtn default false;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnVisibleChanged: TNotifyEvent read fOnVisibleChanged write fOnVisibleChanged;
  end;

implementation

{$R *.RES}

uses rmLibrary;

{ TrmPanel }

const
  DotSize = 4;

type
  TWinControlAccess = class(TWinControl);

procedure TrmPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Cursor := crDefault;
  fMouseOverBtn := false;
  PaintGrip;
end;

constructor TrmPanel.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Caption := '';
  fCapPos := cpStandard;
  fIBWidth := 0;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
  fPanelSizing := false;
  FDeltaPos := 0;
  fResizeBtn := false;
  fLastOpenSize := 0;
  fMouseOverBtn := false;
  fBtnDown := false;
  fDotCount := 10;
  fResizing := false;
  fSplitterPanel := false;
end;

function TrmPanel.GetClientRect: TRect;
var
  wRect: TRect;
begin
  wRect := inherited GetClientRect;
  if CaptionPosition = cpTopEdge then
  begin
     Canvas.Font := Self.Font;
     wRect.Top := wRect.Top + Canvas.textHeight('W');
  end;
  result := wRect;
end;

function TrmPanel.GripRect: TRect;
var
  wRect: TRect;
begin
  case align of
    alTop: wRect := Rect(0, height - gripsize, width, height);
    alBottom: wRect := Rect(0, -GripSize, width, 0);
    alLeft: wRect := Rect(width - gripsize, 0, width, height);
    alRight: wRect := Rect(-GripSize, 0, 0, height);
  else
    wRect := Rect(-1, -1, -1, -1);
  end;
  result := wRect;
end;

procedure TrmPanel.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft: Width := FNewSize;
      alTop: Height := FNewSize;
      alRight: Width := width+(fnewsize-foldsize);//SetBounds(left-(fnewsize-foldsize),top,width+(fnewsize-foldsize),height);
      alBottom: Height := height+(fnewsize-foldsize);//SetBounds(left,top-(fnewsize-foldsize),width, height+(fnewsize-foldsize));
    end;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TrmPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  wRect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if CaptionPosition = cpStandard then
  begin
     inherited;
  end
  else
  begin
     with Canvas do
     begin
       Font := Self.Font;
       FontHeight := TextHeight('W');
       Brush.Color := Color;
       FillRect(Rect(0,0,width,height));
     end;

     wRect := ClientRect;
     wRect.Top := wRect.Top - (FontHeight div 2);

     if BevelOuter <> bvNone then
     begin
       AdjustColors(BevelOuter);
       Frame3D(Canvas, wRect, TopColor, BottomColor, BevelWidth);
     end;
     Frame3D(Canvas, wRect, Color, Color, BorderWidth);
     if BevelInner <> bvNone then
     begin
       AdjustColors(BevelInner);
       Frame3D(Canvas, wRect, TopColor, BottomColor, BevelWidth);
     end;

     with Canvas do
     begin
       wRect := GetClientRect;

       wRect.Top := wRect.Top - FontHeight;
       wRect.Bottom := Top + FontHeight;

       Case Alignment of
          taLeftJustify:
             begin
                 wRect.Left := 8;
                 wRect.Right := wRect.Left + TextWidth(Caption);
             end;
          taRightJustify:
             begin
                 wRect.Right := wRect.Right - 8;
                 wRect.Left := wRect.Right - TextWidth(Caption);
             end;
          taCenter:
             begin
                Try
                   wRect.Left := (width - TextWidth(Caption)) div 2;
                except
                   wRect.Left := 0; 
                end;
                wRect.right := wRect.Left+TextWidth(caption)+1;  
             end;
       end;

       OffsetRect(wRect, Borderwidth, BorderWidth);

       Flags := DT_EXPANDTABS or DT_VCENTER or DT_CENTER;
       Flags := DrawTextBiDiModeFlags(Flags);
       DrawText(Handle, PChar(Caption), -1, wRect, Flags);
     end;
  end;
  PaintGrip;
end;

procedure TrmPanel.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if assigned(fonVisibleChanged) then
    fOnVisibleChanged(self);
end;

procedure TrmPanel.SetResizeBtn(const Value: boolean);
begin
  if fResizeBtn <> Value then
  begin
    fResizeBtn := Value;
    Realign;
    Invalidate;
  end;
end;

function TrmPanel.GripSize: integer;
begin
  if fResizeBtn then
    result := 6
  else
    result := 3;
end;

function TrmPanel.BtnRect: TRect;
var
  wRect: TRect;
  wGripH, wGripW: integer;
begin
  wrect := GripRect;
  wGripH := wRect.Bottom - wrect.Top;
  wGripW := wRect.Right - wrect.Left;
  case Align of
    alTop, alBottom:
      begin
        result.Left := ((wGripW div 2) - ((DotCount * DotSize) div 2));
        result.right := result.left + (DotCount * DotSize);
        if align = altop then
        begin
          result.top := Height - wGripH;
          result.Bottom := height;
        end
        else
        begin
          result.top := -wGripH;
          result.Bottom := 0;
        end;
        InflateRect(result, 12, 0);
      end;
    alLeft, alRight:
      begin
        result.Top := ((wGripH div 2) - ((DotCount * DotSize) div 2));
        result.Bottom := result.Top + (DotCount * DotSize);
        if align = alLeft then
        begin
          result.Left := Width - wGripW;
          result.Right := Width;
        end
        else
        begin
          result.Left := -wGripW;
          result.Right := 0;
        end;
        InflateRect(result, 0, 12);
      end;
  else
    result := Rect(0, 0, 0, 0);
  end;
end;

procedure TrmPanel.SetDotCount(const Value: integer);
begin
  if (value >= 5) and (value <= 20) then
  begin
    fDotCount := value;
    PaintGrip;
  end
  else
    raise ERangeError.Create('Value must be between 5 and 20');
end;

procedure TrmPanel.PaintGrip;
var
  DC : HDC;
  loop: integer;
  wrect: TRect;
  adjust: integer;
  wBmp: TBitmap;
  wArrow: TBitmap;
  wxpos, wypos : integer;
begin
  if not (SplitterPanel or ResizeBtn) then
     exit;

  wBmp := TBitMap.create;
  try
    wRect := GripRect;
    wBmp.Height := wRect.Bottom - wRect.Top;
    wBmp.Width := wRect.Right - wRect.Left;

    wBmp.canvas.brush.color := Color;
    wBmp.canvas.FillRect(Rect(0, 0, wbmp.width, wbmp.height));

    if fResizeBtn then
    begin
      wrect := BtnRect;

      if (align in [albottom, alTop]) then
      begin
        OffsetRect(wRect, 0, -wRect.Top);
        for loop := 0 to DotCount - 1 do
        begin
          adjust := (loop * DotSize) + 12;
          wBmp.canvas.pixels[wRect.Left + 1 + adjust, wRect.Top + 1] := clbtnhighlight;
          wBmp.canvas.pixels[wRect.Left + 2 + adjust, wRect.Top + 1] := clHighlight;
          wBmp.canvas.pixels[wRect.Left + 1 + adjust, wRect.Top + 2] := clHighlight;
          wBmp.canvas.pixels[wRect.Left + 2 + adjust, wRect.Top + 2] := clHighlight;

          if loop < DotCount then
          begin
            wBmp.canvas.pixels[wRect.Left + 3 + adjust, wRect.Top + 3] := clbtnhighlight;
            wBmp.canvas.pixels[wRect.Left + 4 + adjust, wRect.Top + 3] := clHighlight;
            wBmp.canvas.pixels[wRect.Left + 3 + adjust, wRect.Top + 4] := clHighlight;
            wBmp.canvas.pixels[wRect.Left + 4 + adjust, wRect.Top + 4] := clHighlight;
          end;
        end;

        wArrow := TBitmap.create;
        try
          if fLastOpenSize = 0 then
          begin
            if align = altop then
              wArrow.LoadFromResourceName(Hinstance, 'RMPUPARROW')
            else
              wArrow.LoadFromResourceName(Hinstance, 'RMPDNARROW');
          end
          else
          begin
            if align = alBottom then
              wArrow.LoadFromResourceName(Hinstance, 'RMPUPARROW')
            else
              wArrow.LoadFromResourceName(Hinstance, 'RMPDNARROW');
          end;

          ReplaceColors(wArrow, Color, clHighLight);

          wBmp.Canvas.Draw(wRect.Left + 1, wRect.Top + 2, wArrow);
          wBmp.Canvas.Draw((wRect.Right - 1) - wArrow.width, wRect.Top + 2, wArrow);

        finally
          wArrow.free;
        end;

      end
      else if (align in [alLeft, alRight]) then
      begin
        OffsetRect(wRect, -wRect.Left, 0);
        for loop := 0 to DotCount - 1 do
        begin
          adjust := (loop * DotSize) + 12;
          wBmp.canvas.pixels[wRect.Left + 1, wRect.Top + 1 + adjust] := clbtnhighlight;
          wBmp.canvas.pixels[wRect.Left + 1, wRect.Top + 2 + adjust] := clHighlight;
          wBmp.canvas.pixels[wRect.Left + 2, wRect.Top + 1 + adjust] := clHighlight;
          wBmp.canvas.pixels[wRect.Left + 2, wRect.Top + 2 + adjust] := clHighlight;

          if loop < DotCount then
          begin
            wBmp.canvas.pixels[wRect.Left + 3, wRect.Top + 2 + adjust] := clbtnhighlight;
            wBmp.canvas.pixels[wRect.Left + 3, wRect.Top + 4 + adjust] := clHighlight;
            wBmp.canvas.pixels[wRect.Left + 4, wRect.Top + 3 + adjust] := clHighlight;
            wBmp.canvas.pixels[wRect.Left + 4, wRect.Top + 4 + adjust] := clHighlight;
          end;
        end;

        wArrow := TBitmap.create;
        try
          if fLastOpenSize = 0 then
          begin
            if align = alLeft then
              wArrow.LoadFromResourceName(Hinstance, 'RMPLTARROW')
            else
              wArrow.LoadFromResourceName(Hinstance, 'RMPRTARROW');
          end
          else
          begin
            if align = alRight then
              wArrow.LoadFromResourceName(Hinstance, 'RMPLTARROW')
            else
              wArrow.LoadFromResourceName(Hinstance, 'RMPRTARROW');
          end;
          ReplaceColors(wArrow, Color, clHighLight);

          wBmp.Canvas.Draw(wRect.Left + 2, wRect.Top + 1, wArrow);
          wBmp.Canvas.Draw(wRect.Left + 2, (wRect.Bottom - 1) - wArrow.Height, wArrow);
        finally
          wArrow.free;
        end;

      end;

      if fMouseOverBtn then
      begin
        if fBtnDown then
          Frame3D(wBmp.Canvas, wRect, clbtnshadow, clbtnhighlight, 1)
        else
          Frame3D(wBmp.Canvas, wRect, clbtnhighlight, clbtnshadow, 1);
      end;
    end;
    wRect := GripRect;

    wxpos := 0;
    wypos := 0;
    case align of
      albottom, alright:
         begin
           wxpos := 0;
           wypos := 0;
         end;
      alleft :
         begin
            wxpos := width-wbmp.width;
            wypos := 0;
         end;
      altop :
         begin
            wxpos := 0;
            wypos := height-wbmp.height;
         end;
    end;

    DC := GetWindowDC(Handle) ;
    try
       BitBlt(DC, wxpos, wypos, wBMP.width, wBMP.height, wBMP.Canvas.Handle, 0, 0, SRCCOPY) ;
    finally
       ReleaseDC(Handle, DC) ;
    end;


//    Canvas.Draw(wRect.Left, wRect.Top, wBmp);
  finally
    wBmp.free;
  end;
end;

procedure TrmPanel.SetSplitterPanel(const Value: boolean);
begin
  if fSplitterPanel <> Value then
  begin
    fSplitterPanel := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TrmPanel.SetCapPos(const Value: TrmCaptionPosition);
begin
  if fCapPos <> Value then
  begin
     fCapPos := Value;
     Realign;
     Invalidate;
  end;
end;

procedure TrmPanel.SetIBWidth(const Value: integer);
begin
  if fIBWidth <> Value then
  begin
     fIBWidth := Value;
     Realign;
     invalidate;
  end;
end;

procedure TrmPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -fIBWidth, -fIBWidth);
end;

procedure TrmPanel.CMFontChanged(var Message: TMessage);
begin
   Inherited;
   Realign;
   Invalidate;
end;

procedure TrmPanel.CMParentFontChanged(var Message: TMessage);
begin
   Inherited;
   Realign;
   Invalidate;
end;

procedure TrmPanel.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
   inherited;

   if SplitterPanel or ResizeBtn then
   begin
      with Message.CalcSize_Params^ do
      begin
        case align of
          alTop: rgrc[0].Bottom := rgrc[0].Bottom - GripSize;
          alBottom: rgrc[0].Top := rgrc[0].Top + GripSize;
          alLeft: rgrc[0].Right := rgrc[0].Right - GripSize;
          alRight: rgrc[0].Left := rgrc[0].Left + GripSize;
        end;
      end;
   end;
end;

procedure TrmPanel.WMNCPaint(var Message: TMessage);
begin
   if SplitterPanel or ResizeBtn then
   begin
      PaintGrip;
      Message.result := 0;
   end
   else
   inherited;
end;

procedure TrmPanel.WMNCHitTest(var Message: TWMNCHitTest);
var
   wpt: TPoint;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   if SplitterPanel or ResizeBtn then
   begin
      wpt := Point(Message.XPos, Message.YPos) ;

      if resizebtn and ptinrect(Convert(btnrect),wpt) then
      begin
         fMouseOverBtn := true;
         message.result := htCaption;
      end
      else
      begin
          fMouseOverBtn := false;

          if splitterpanel and ptinrect(convert(GripRect),wpt) then
          begin
            if (fLastOpenSize = 0) then
               message.result := htClient
            else
               Message.Result := htnowhere;
          end
          else
            message.result := htClient
      end;
   end
   else
   begin
      fMouseOverBtn := false;
      inherited;
   end;
end;

function TrmPanel.Convert(wRect: TRect): TRect;
begin
   result.topleft := clienttoscreen(wrect.topleft);
   result.bottomright := clienttoscreen(wrect.bottomright);
end;

procedure TrmPanel.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TrmPanel.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := Width + Split;
    alRight: S := Width - Split;
    alTop: S := Height + Split;
    alBottom: S := Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TrmPanel.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TrmPanel.DrawLine;
var
  P: TPoint;
  h, w: integer;
  wRect: TRect;
begin
  wRect := GripRect;
  wRect.TopLeft := Parent.ScreenToClient(Self.ClientToScreen(wRect.TopLeft));
  wRect.BottomRight := Parent.ScreenToClient(Self.ClientToScreen(wRect.BottomRight));

  FLineVisible := not FLineVisible;
  case Align of
    alLeft:
      begin
        P.X := wRect.left + FDeltaPos;
        P.Y := Top;
        h := height;
        w := GripSize;
      end;
    alRight:
      begin
        P.X := left + FDeltaPos;
        P.Y := Top;
        h := height;
        w := GripSize;
      end;
    alBottom:
      begin
        P.X := 0;
        P.Y := top + FDeltaPos;
        h := GripSize;
        w := width;
      end;
    alTop:
      begin
        P.X := 0;
        P.Y := wRect.top + FDeltaPos;
        h := GripSize;
        w := width;
      end;
  else
    exit;
  end;
  with P do PatBlt(FLineDC, X, Y, W, H, PATINVERT);
end;

procedure TrmPanel.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TrmPanel.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TrmPanel.StopSizing;
begin
  if FLineVisible then DrawLine;
  ReleaseLineDC;
  if Assigned(FActiveControl) then
  begin
    TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
    FActiveControl := nil;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

function TrmPanel.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;

  if not fBtnOpenClose and Assigned(fOnCanResize) then
  begin

     fOnCanResize(Self, NewSize, Result);
  end;
end;

function TrmPanel.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result then
     NewSize := SetInRange(NewSize, fMinsize, fMaxSize);
end;

procedure TrmPanel.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  wPt: TPoint;
  wCloseBtn: boolean;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

  if ResizeBtn then
  begin
     wPt := Point(message.XCursor, message.YCursor);
     wCloseBtn := (ResizeBtn and PtInRect(convert(BtnRect), wPt));

     if wCloseBtn then
     begin
        SendCancelMode(Self) ;
        MouseCapture := true;
     end;

     if wCloseBtn then
     begin
       fBtnDown := true;
       PaintGrip;
     end
     else
     begin
       fBtnDown := false;
     end;

     Message.result := 0;
  end
  else
  inherited;
end;

procedure TrmPanel.WMLButtonUp(var Message: TWMLButtonUp);
var
   wbtndown : boolean;
   wpt : tpoint;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   wBtnDown := fBtnDown;
   fBtnDown := false;
   MouseCapture := false;
   PaintGrip;
   wpt := point(message.XPos, Message.YPos);
   if wBtnDown and ptInRect(btnrect, wpt) then
   begin
      fBtnOpenClose := true;
      try
         if isClosed then
            OpenPanel
         else
            ClosePanel;
      finally
         fBtnOpenClose := false;
      end;
      Message.Result := 0;
   end
   else
      inherited;
end;

procedure TrmPanel.WMNCMouseMove(var Message: TWMNCMouseMove);
var
   wpt : Tpoint;
begin
   if csDesigning in ComponentState then
   begin
      inherited;
      exit;
   end;

   inherited;

   wPt := Point(message.XCursor, message.YCursor);

   fMouseOverBtn := (ResizeBtn and PtInRect(Convert(BtnRect), wPt));

   if SplitterPanel and PtInRect(convert(GripRect), wPt) and not (fMouseOverBtn) and (fLastOpenSize = 0) then
   begin
     case align of
       alTop, alBottom: Cursor := crVSplit;
       alRight, alLeft: Cursor := crHSplit;
     else
       Cursor := crDefault;
     end;
   end
   else
     Cursor := crDefault;

   PaintGrip;
end;

procedure TrmPanel.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  if not (csReading in ComponentState) and Fresizing then
    with Message.MinMaxInfo^.ptMinTrackSize do
    begin
      case align of
         alright, alLeft:
            begin
               if x < fMinsize then
                  x := fMinsize;

               if x < GripSize then
                  x := gripsize;
            end;
         alTop, alBottom:
            begin
               if y < fMinsize then
                  y := fMinsize;

               if y < GripSize then
                  y := gripsize;
            end;
      end;
    end;
end;

procedure TrmPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  wPt: TPoint;
  wCloseBtn: boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  wPt := Point(x, y);
  wCloseBtn := (fResizeBtn and PtInRect(BtnRect, wPt));
  if SplitterPanel and PtInRect(GripRect, wPt) and not wCloseBtn and (fLastOpenSize = 0) then
  begin
    fBtnDown := false;
    if Button = mbLeft then
    begin
      FPanelSizing := true;
      FDownPos := Point(X, Y);
      fDeltaPos := 0;
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then Dec(FMaxSize, Width);
        Inc(FMaxSize, Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      case align of
        alright, alleft : FOldSize := width;
        altop, albottom : FOldSize := height;
      end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end
  else if (fResizeBtn and PtInRect(BtnRect, wPt) and (Button = mbLeft)) then
  begin
    fBtnDown := true;
    PaintGrip;
  end
  else
  begin
    fBtnDown := false;
  end;
end;

procedure TrmPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  wPt: TPoint;
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and FPanelSizing then
  begin
    if ResizeStyle = rsUpdate then
    begin
        case align of
          alright, alleft : FOldSize := width;
          altop, albottom : FOldSize := height;
        end;
        case align of
          alLeft: NewSize := x;
          alRight: NewSize := width - x;
          alTop: NewSize := y;
          alBottom: NewSize := height - y;
        else
          exit;
        end;
        if (FOldSize <> NewSize) and DoCanResize(NewSize) then
        begin
           fNewSize := newsize;
           UpdateControlSize;
        end;
    end
    else
    begin
      CalcSplitSize(X, Y, NewSize, Split);
      if DoCanResize(NewSize) then
      begin
        if ResizeStyle in [rsLine, rsPattern] then DrawLine;
        FNewSize := NewSize;
        FSplit := Split;

        case align of
          alLeft: fDeltaPos := x - fDownPos.x;
          alRight: fDeltaPos := x;
          alTop: fDeltaPos := y - fDownPos.y;
          alBottom: fDeltaPos := y;
        else
          fDeltaPos := 0;
        end;

        if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      end;
    end;
  end
  else
  begin
    wPt := Point(x, y);

    fMouseOverBtn := (fResizeBtn and PtInRect(BtnRect, wPt));

    if SplitterPanel and PtInRect(GripRect, wPt) and not (fMouseOverBtn) and (fLastOpenSize = 0) then
    begin
      case align of
        alTop, alBottom: Cursor := crVSplit;
        alRight, alLeft: Cursor := crHSplit;
      else
        Cursor := crDefault;
      end;
    end
    else
      Cursor := crDefault;

    PaintGrip
  end;
end;

procedure TrmPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  wPt: TPoint;
begin
  inherited;
  if FPanelSizing then
  begin
    fPanelSizing := false;
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end
  else
  begin
    wPt := Point(x, y);

    if fResizeBtn and PtInRect(convert(BtnRect), wPt) and fBtnDown then
    begin
      if (fLastOpenSize <> 0) then
      begin
        case align of
          alTop, alBottom:
             begin
                if align = alBottom then
                   SetBounds(left,top-flastopensize,width, height+flastopensize)
                else
                   clientheight := fLastOpenSize;
             end;
          alLeft, alRight:
             begin
                if align = alRight then
                   SetBounds(left-fLastOpenSize,top,width+fLastOpenSize,height)
                else
                   clientWidth := fLastOpenSize;
             end;
        end;
        fLastOpenSize := 0;
      end
      else
      begin
        case align of
          alTop:
            begin
              fLastOpenSize := clientheight;
              clientheight := 0;
            end;
          alBottom:
            begin
              Parent.DisableAlign;
              try
                fLastOpenSize := clientheight;
                clientheight := 0;
                height := GripSize;
              finally
                Parent.EnableAlign;
              end;
            end;
          alLeft:
            begin
              fLastOpenSize := clientWidth;
              clientWidth := 0;
            end;
          alRight:
            begin
              Parent.DisableAlign;
              try
                fLastOpenSize := clientWidth;
                clientWidth := 0;
                width := GripSize;
              finally
                Parent.EnableAlign;
              end;
            end;
        end;
        Update;

      end;
    end;
    fBtnDown := false;
  end;
end;

function TrmPanel.GetClosedState: boolean;
begin
   result := fLastOpenSize <> 0;
end;

procedure TrmPanel.wmEraseBkgrnd(var Msg: TMessage);
begin
   msg.result := 1;
end;

procedure TrmPanel.ClosePanel;
begin
   if not isclosed then
   begin
     case align of
       alTop:
         begin
           fLastOpenSize := clientheight;
           clientheight := 0;
         end;
       alBottom:
         begin
           Parent.DisableAlign;
           try
             fLastOpenSize := clientheight;
             clientheight := 0;
             if clientheight <> 0 then
                flastopensize := 0;
           finally
             Parent.EnableAlign;
           end;
         end;
       alLeft:
         begin
           fLastOpenSize := clientWidth;
           clientWidth := 0;
           if clientwidth <> 0 then
              flastopensize := 0;
         end;
       alRight:
         begin
           Parent.DisableAlign;
           try
             fLastOpenSize := clientWidth;
             clientWidth := 0;
           finally
             Parent.EnableAlign;
           end;
         end;
     end;
     Realign;
   end;
end;

procedure TrmPanel.OpenPanel;
begin
  if IsClosed then
  begin
    case align of
     alTop, alBottom:
        begin
           if align = alBottom then
              SetBounds(left,top-flastopensize,width, height+flastopensize)
           else
              clientheight := fLastOpenSize;
        end;
     alLeft, alRight:
        begin
           if align = alRight then
              SetBounds(left-fLastOpenSize,top,width+fLastOpenSize,height)
           else
              clientWidth := fLastOpenSize;
        end;
    end;
    fLastOpenSize := 0;
    Realign;
  end;
end;

end.

