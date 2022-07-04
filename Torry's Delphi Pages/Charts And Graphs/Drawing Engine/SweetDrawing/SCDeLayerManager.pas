{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDeLayerManager;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, {$IFDEF SCDE_DELPHI6_UP} Variants, Types, {$ENDIF} SCDECommon,
  SCDEConsts, SCDEControl, SCDEBitmap, SCDrawingCommons, SCDrawingSurface,
  SCDrawingEditor;

type
  TSCDeCustomLayerManager = class;
  TSCDeListLayer = class;

  TSCDeListItem = class
  private
    FShape: TSCDeShapeBase;
  protected
    constructor Create(AShape: TSCDeShapeBase);
    property Shape: TSCDeShapeBase read FShape;
  end;

  TSCDeListShape = class(TSCDeListItem)
  private
    FOwner: TSCDeListLayer;
  protected
    constructor Create(AOwner: TSCDeListLayer; AShape: TSCDeShapeBase);
  public
    destructor Destroy; override;

    property Shape;
    property Owner: TSCDeListLayer read FOwner;
  end;

  TSCDeListLayer = class(TSCDeListItem)
  private
    FShapes: TList;
    FLayer: TSCDeLayer;
    FExpanded: Boolean;
    FOwner: TSCDeCustomLayerManager;
    procedure SetExpanded(Value: Boolean);
    function  GetShapeCount: Integer;
    function  GetShape(Index: Integer): TSCDeListShape;

    procedure ClearShapes;
    procedure UpdateShapes;
    procedure ShapeRemoved(S: TSCDeListShape);
  protected
    constructor Create(AOwner: TSCDeCustomLayerManager; ALayer: TSCDeLayer);
  public
    destructor Destroy; override;

    property Layer: TSCDeLayer read FLayer;
    property Owner: TSCDeCustomLayerManager read FOwner;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Count: Integer read GetShapeCount;
    property Shapes[Index: Integer]: TSCDeListShape read GetShape;
  end;

  TSCDeLayerManagerColors = class(TPersistent)
  private
    FActiveLayer: TColor;
    FActiveLayerText: TColor;
    FCaption: TColor;
    FCaptionText: TColor;
    FSelectionIndicator: TColor;
    FSelection: TColor;
    FSelectionText: TColor;
    FOwner: TSCDeCustomLayerManager;
    procedure SetActiveLayer(Value: TColor);
    procedure SetActiveLayerText(Value: TColor);
    procedure SetCaption(Value: TColor);
    procedure SetCaptionText(Value: TColor);
    procedure SetSelectionIndicator(Value: TColor);
    procedure SetSelection(Value: TColor);
    procedure SetSelectionText(Value: TColor);
  protected
    procedure Changed;

    function GetOwner: TPersistent; override;
    property Owner: TSCDeCustomLayerManager read FOwner;
  public
    constructor Create(AOwner: TSCDeCustomLayerManager); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveLayer: TColor read FActiveLayer write SetActiveLayer default $00B49184;
    property ActiveLayerText: TColor read FActiveLayerText write SetActiveLayerText default clBtnText;
    property Caption: TColor read FCaption write SetCaption default clBtnFace;
    property CaptionText: TColor read FCaptionText write SetCaptionText default clBtnText;
    property SelectionIndicator: TColor read FSelectionIndicator write SetSelectionIndicator default clLime;
    property Selection: TColor read FSelection write SetSelection default clHighlight;
    property SelectionText: TColor read FSelectionText write SetSelectionText default clHighlightText;
  end;

  TSCDeListHitPart = (scdlhNone, scdlhLayerExpandButton,
    scdlhLayerCaption, scdlhLayerHideButton, scdlhLayerLockButton,
    scdlhItem, scdlhItemHideButton, scdlhItemLockButton);

  TSCDeListHitInfo = record
    Pos: TPoint;
    Layer: Integer;
    Shape: Integer;
    Part: TSCDeListHitPart;
  end;

  TSCDeListScrollbar = class(TSCDeControlScrollbar)
  published
    property Background;
    property ButtonColors;
    property Icons;
    property SlideLine;
    property Thumb;
  end;

  TSCDeCustomLayerManager = class(TSCDeCustomScrollControl)
  private
    FLayers: TList;
    FDropping: Boolean;
    FTopPosition: Integer;
    FShowPreview: Boolean;
    FShowSelectionIndicator: Boolean;
    FColors: TSCDeLayerManagerColors;
    FEditor: TSCDeCustomEditor;
    FEventClient: TSCDeEditorClient;
    FLayerBitmap: TBitmap;
    FExpandBitmap: TBitmap;
    FActiveBitmap: TBitmap;
    FLockedBitmap: TBitmap;
    FVisibleBitmap: TBitmap;
    FCollapseBitmap: TBitmap;
    FInvisibleLayerBitmap: TBitmap;
    FCreatingWnd: Integer;
    FScrollbarChanging: Integer;
    FScrollPosChanging: Integer;
    FHotInfo: TSCDeListHitInfo;
    FClickInfo: TSCDeListHitInfo;
    procedure SetColors(Value: TSCDeLayerManagerColors);
    procedure SetEditor(Value: TSCDeCustomEditor);
    procedure SetShowPreview(Value: Boolean);
    procedure SetShowSelectionIndicator(Value: Boolean);
    procedure SetTopPosition(Value: Integer);

    procedure ClearLayers;
    procedure LayerRemoved(L: TSCDeListLayer); overload;
    procedure ShapeRemoved(S: TSCDeListShape); overload;

    function  GetCaptionHeight: Integer;
    function  GetPreviewHeight: Integer;

    function  LoadBitmap(ResName: String): TBitmap;
    procedure LoadBitmaps;
    procedure ReleaseBitmaps;

    procedure ColorsChanged;

    procedure InitializeEventClient;
    function  InitializeItemInfo(Ii: TSCDeListHitInfo): TSCDeListHitInfo;

    procedure SelectionChanged(Sender: TObject);
    procedure ActiveLayerChanged(Sender: TObject);
    procedure LayerChanged(Sender: TObject; Shape: TSCDeShapeBase);
    procedure LayerAdded(Sender: TObject; Shape: TSCDeShapeBase);
    procedure LayerRemoved(Sender: TObject; Shape: TSCDeShapeBase); overload;
    procedure ShapeAdded(Sender: TObject; Shape: TSCDeShapeBase);
    procedure ShapeRemoved(Sender: TObject; Shape: TSCDeShapeBase); overload;
    procedure ShapeCleared(Sender: TObject; Shape: TSCDeShapeBase);

    procedure LayerExpandChanged(L: TSCDeListLayer);

    procedure VerifyTopPos(var TopPos: Integer);

    function  GetPageUpPos(TopPos: Integer): Integer;
    function  GetPageDownPos(TopPos: Integer): Integer;

    procedure ReCalculateTopPosition;
    function  GetScrollCount: Integer;
    function  UpdateScrollbar: Boolean;

    procedure DrawText(Text: String; R: TRect; FrCl: TColor);
    procedure DrawLayerCaption(L: TSCDeListLayer; R: TRect);
    procedure DrawLayerItem(Li: TSCDeListShape; R: TRect);

    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;

    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure DragCanceled; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function  GetScrollbarClass: TSCDeCustomControlScrollbarClass; override;
    function  CanScrollToPos(Kind: TSCDeScrollbarKind; var NewValue: Integer): Boolean; override;
    procedure DoScrollerPositionChanged(Kind: TSCDeScrollbarKind); override;

    procedure UpdateLayers;

    function  GetTopItem: TSCDeListItem;
    function  GetVisibleItemCount: Integer;

    function  GetLayerHeight(L: TSCDeListLayer): Integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    property Dropping: Boolean read FDropping write FDropping;
    property Colors: TSCDeLayerManagerColors read FColors write SetColors;
    property Editor: TSCDeCustomEditor read FEditor write SetEditor;
    property ShowPreview: Boolean read FShowPreview write SetShowPreview default True;
    property ShowSelectionIndicator: Boolean read FShowSelectionIndicator
      write SetShowSelectionIndicator default True;
    property TopPosition: Integer read FTopPosition write SetTopPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HitInfo(P: TPoint): TSCDeListHitInfo;

    property Color default clWindow;
    property ParentColor default False;
    property TabStop default True;
  end;

  TSCDeLayerManager = class(TSCDeCustomLayerManager)
  published
    property Align;
    property Anchors;
    property BorderProps;
    property Color;
    property Colors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Editor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Picture;
    property PictureProps;
    property PopupMenu;
    property Scrollbars;
    property ShowPreview;
    property ShowSelectionIndicator;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPictureChange;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{$R *.res}

type
  TSCDeFakeEditor = class(TSCDeCustomEditor);

{ TSCDeCustomLayerManager }

procedure TSCDeCustomLayerManager.ColorsChanged;
begin
  if (FLayers <> nil) and (FLayers.Count > 0) then
    Invalidate;
end;

constructor TSCDeCustomLayerManager.Create(AOwner: TComponent);
begin
  FLayers := TList.Create;

  inherited Create(AOwner);

  SetBounds(Left, Top, 210, 300);
  ParentColor := False;
  Color := clWindow;
  Border := scdcb3DLowered;
  DoubleBuffered := True;
  TabStop := True;

  LoadBitmaps;

  FShowPreview := True;
  FShowSelectionIndicator := True;

  InitializeItemInfo(FHotInfo);
  InitializeItemInfo(FClickInfo);

  FColors := TSCDeLayerManagerColors.Create(Self);

  FEventClient := TSCDeEditorClient.Create(nil);
  InitializeEventClient;
end;

destructor TSCDeCustomLayerManager.Destroy;
begin
  ClearLayers;
  FreeAndNil(FLayers);
  FreeAndNil(FColors);

  if FEditor <> nil then
    FEditor.UnRegisterClient(FEventClient);

  ReleaseBitmaps;

  inherited Destroy;
end;

procedure TSCDeCustomLayerManager.DrawLayerCaption(L: TSCDeListLayer; R: TRect);
var
  R2: TRect;
  S: String;
  B: TBitmap;
  I, Ch: Integer;
  Fe: TSCDeFakeEditor;
  BkCl, FrCl, Cl: TColor;
  HasSelection, IsActive: Boolean;
begin
  R2 := R;
  Fe := TSCDeFakeEditor(FEditor);
  
  BkCl := clBtnFace;
  if (FColors.Caption <> clNone) then
    BkCl := FColors.Caption;

  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := BkCl;

    FillRect(R);
  end;

  Ch := GetCaptionHeight;

  // Expand - Collapse Section
  B := FCollapseBitmap;
  if not L.Expanded then
    B := FExpandBitmap;

  if B <> nil then
    Canvas.Draw(R.Left + (20 - B.Width) div 2,
      R.Top + (Ch - B.Height) div 2, B);

  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Bottom);
  end;

  // Visible Section
  if L.Layer.Visible then
  begin
    B := FVisibleBitmap;
    if B <> nil then
      Canvas.Draw(R.Left + (20 - B.Width) div 2,
        R.Top + (Ch - B.Height) div 2, B);
  end;

  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top + 2);
    LineTo(R.Left, R.Bottom - 2);

    Pen.Color := clBtnHighlight;

    MoveTo(R.Left + 1, R.Top + 2);
    LineTo(R.Left + 1, R.Bottom - 2);
  end;

  // Active - Locked Section
  IsActive := Fe.ActiveLayer = L.Layer;

  B := nil;
  if L.Layer.Locked then
    B := FLockedBitmap
  else if IsActive then
    B := FActiveBitmap;

  if B <> nil then
    Canvas.Draw(R.Left + (20 - B.Width) div 2,
      R.Top + (Ch - B.Height) div 2, B);

  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Bottom);
  end;

  Inc(R.Left);
  if IsActive then
  begin
    Cl := clHighlight;
    if (FColors.ActiveLayer <> clNone) then
      Cl := FColors.ActiveLayer;

    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Cl;

      FillRect(R);
    end;
  end;

  B := FLayerBitmap;
  if B <> nil then
    Canvas.Draw(R.Left + (20 - B.Width) div 2,
      R.Top + (Ch - B.Height) div 2, B);

  Inc(R.Left, 20);
  if FShowSelectionIndicator then
    Dec(R.Right, 20);

  if not IsRectEmpty(R) then
  begin
    FrCl := clBtnText;
    if IsActive then
    begin
      FrCl := clHighlightText;
      if FColors.ActiveLayerText <> clNone then
        FrCl := FColors.ActiveLayerText;
    end else
    if FColors.CaptionText <> clNone then
      FrCl := FColors.CaptionText;

    S := Trim(L.Layer.Name);
    if Length(S) = 0 then
      S := 'Layer - ' + IntToStr(L.Layer.Index);

    Inc(R.Left, 2);
    Self.DrawText(S, R, FrCl);
  end;

  // Selected Shape Indicator
  if FShowSelectionIndicator then
  begin
    Inc(R.Right, 20);
    R.Left := R.Right - 20;

    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Color := clBtnShadow;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
    end;

    HasSelection := False;
    for I := 0 to L.Count-1 do
      if FEditor.IsSelected(L.Shapes[I].Shape) then
      begin
        HasSelection := True;
        Break;
      end;

    if HasSelection then
    begin
      R.Left := R.Left + ((R.Right - R.Left - 8) div 2);
      R.Right := R.Left + 8;

      R.Top := R.Top + ((R.Bottom - R.Top - 8) div 2);
      R.Bottom := R.Top + 8;

      with Canvas do
      begin
        Pen.Style := psSolid;
        Pen.Color := clWindowFrame;

        Brush.Style := bsSolid;
        Brush.Color := FColors.SelectionIndicator;

        Rectangle(R);
      end;
    end;
  end;

  R := R2;
  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnHighlight;

    MoveTo(R.Left, R.Top);
    LineTo(R.Right, R.Top);

    Inc(R.Left, 20);
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Right, R.Bottom - 1);
  end;
end;

procedure TSCDeCustomLayerManager.DrawLayerItem(Li: TSCDeListShape; R: TRect);
var
  S: String;
  B: TBitmap;
  R2, R3: TRect;
  PB: TSCDeBitmap;
  IsActive: Boolean;
  Fe: TSCDeFakeEditor;
  BkCl, FrCl, Cl: TColor;
  I, J, L, T, XCnt, YCnt, Ph: Integer;
begin
  R2 := R;
  Fe := TSCDeFakeEditor(FEditor);

  BkCl := clBtnFace;
  if (FColors.Caption <> clNone) then
    BkCl := FColors.Caption;

  R.Right := R.Left + 60;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := BkCl;

    FillRect(R);
  end;

  Ph := GetPreviewHeight;

  // Expand - Collapse Section
  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Bottom);
  end;

  // Visible Section
  if Li.Shape.Visible then
  begin
    B := FVisibleBitmap;
    if (Li.Shape.Owner <> nil) and not Li.Shape.Owner.Visible then
      B := FInvisibleLayerBitmap;

    if B <> nil then
      Canvas.Draw(R.Left + (20 - B.Width) div 2,
        R.Top + (Ph - B.Height) div 2, B);
  end;

  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top + 2);
    LineTo(R.Left, R.Bottom - 2);

    Pen.Color := clBtnHighlight;

    MoveTo(R.Left + 1, R.Top + 2);
    LineTo(R.Left + 1, R.Bottom - 2);
  end;

  // Active - Locked Section
  IsActive := Fe.IsSelected(Li.Shape);

  B := nil;
  if Li.Shape.Locked then
    B := FLockedBitmap;

  if B <> nil then
    Canvas.Draw(R.Left + (20 - B.Width) div 2,
      R.Top + (Ph - B.Height) div 2, B);

  R := R2;
  Inc(R.Left, 60);
  
  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Bottom);
  end;

  Inc(R.Left);
  if IsActive then
  begin
    Cl := clHighlight;
    if (FColors.Selection <> clNone) then
      Cl := FColors.Selection;

    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Cl;

      FillRect(R);
    end;
  end;

  if FShowPreview then
  begin
    Inc(R.Left, 8);

    R3 := R;

    InflateRect(R3, 0, -2);
    Dec(R3.Bottom);

    R3.Right := R3.Left + Round((R3.Bottom - R3.Top)*(Fe.Layer.Width/Fe.Layer.Height));

    if not IsRectEmpty(R3) then
    begin
      PB := TSCDeBitmap.Create;
      try
        PB.SetBounds(R3.Right - R3.Left, R3.Bottom - R3.Top);

        with PB.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := clWhite;

          FillRect(Rect(0, 0, PB.Width, PB.Height));

          Brush.Style := bsSolid;
          Brush.Color := $00DFDFDF;

          XCnt := TSCDeUtils.Round(PB.Width / 4);
          YCnt := TSCDeUtils.Round(PB.Height / 4);

          for I := 0 to XCnt do
          begin
            L := 4*I;
            if L < PB.Width then
              for J := 0 to YCnt do
              begin
                T := 4*J;
                if (Odd(J) <> Odd(I)) and (T < PB.Height) then
                  FillRect(Rect(L, T, L + 4, T + 4));
              end;
          end;
        end;

        Li.Shape.Paint(PB.Canvas, 0, 0, PB.Width/Fe.Layer.Width);

        with PB.Canvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := cl3DDkShadow;

          FrameRect(Rect(0, 0, PB.Width, PB.Height));
        end;

        Canvas.Draw(R3.Left, R3.Top, PB);
        Inc(R.Left, PB.Width);
      finally
        PB.Free;
      end;
    end;
  end;

  FrCl := Self.Font.Color;
  if IsActive then
  begin
    FrCl := clHighlightText;
    if FColors.SelectionText <> clNone then
      FrCl := FColors.SelectionText;
  end;

  S := Trim(Li.Shape.Name);
  if Length(S) = 0 then
    S := 'Shape - ' + IntToStr(Li.Shape.Index);

  Inc(R.Left, 4);
  Self.DrawText(S, R, FrCl);

  R := R2;
  Inc(R.Left, 20);

  with Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := clBtnShadow;

    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Right, R.Bottom - 1);
  end;
end;

procedure TSCDeCustomLayerManager.DrawText(Text: String; R: TRect;
  FrCl: TColor);
var
  F: Integer;
begin
  with Canvas do
  begin
    Brush.Style := bsClear;

    Font.Assign(Self.Font);
    Font.Color := FrCl;
  end;

  F := DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
    DT_END_ELLIPSIS;

  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), R, F);
end;

function TSCDeCustomLayerManager.GetCaptionHeight: Integer;
begin
  Result := 18;
end;

function TSCDeCustomLayerManager.GetLayerHeight(L: TSCDeListLayer): Integer;
begin
  Result := 0;
  if (L <> nil) and (L.Layer <> nil) then
  begin
    Result := GetCaptionHeight + 1;
    if L.Expanded then
      Inc(Result, GetPreviewHeight*L.Layer.ItemCount);
  end;
end;

function TSCDeCustomLayerManager.GetPreviewHeight: Integer;
begin
  Result := 37;
  if not FShowPreview then
    Result := 18;
end;

function TSCDeCustomLayerManager.GetTopItem: TSCDeListItem;
var
  I, J, C: Integer;
  L: TSCDeListLayer;
begin
  Result := nil;
  C := FTopPosition;

  for I := 0 to FLayers.Count-1 do
  begin
    L := TSCDeListLayer(FLayers[I]);
    Result := L;

    if (C = 0) then
      Exit;

    Dec(C);

    if L.Expanded then
      for J := 0 to L.Count-1 do
      begin
        Result := L.Shapes[J];

        if (C = 0) then
          Exit;

        Dec(C);
      end;
  end;
end;

function TSCDeCustomLayerManager.GetVisibleItemCount: Integer;
var
  I: Integer;
  L: TSCDeListLayer;
begin
  Result := 0;
  for I := 0 to FLayers.Count-1 do
  begin
    Inc(Result);

    L := TSCDeListLayer(FLayers[I]);
    if L.Expanded then
      Inc(Result, L.Count);
  end;
end;

function TSCDeCustomLayerManager.HitInfo(P: TPoint): TSCDeListHitInfo;
var
  CR, R, R2: TRect;
  L: TSCDeListLayer;
  I, J, T, C, Ch, Ph: Integer;
begin
  InitializeItemInfo(Result);
  Result.Pos := P;

  if (FEditor <> nil) and (FLayers.Count > 0) then
  begin
    CR := Self.ClientRect;

    if not IsRectEmpty(CR) and PtInRect(CR, P) then
    begin
      T := 0;
      C := FTopPosition;

      Ch := GetCaptionHeight;
      Ph := GetPreviewHeight;

      for I := 0 to FLayers.Count-1 do
      begin
        if T >= CR.Bottom then
          Exit;

        L := TSCDeListLayer(FLayers[I]);

        if C = 0 then
        begin
          R := CR;
          R.Top := T;
          R.Bottom := T + Ch;

          if PtInRect(R, P) then
          begin
            Result.Layer := I;

            R2 := Rect(0, 0, 9, 9);
            OffsetRect(R2, R.Left + 5, R.Top + ((Ch - 9) div 2));

            if PtInRect(R2, P) then
              Result.Part := scdlhLayerExpandButton
            else begin
              Inc(R.Left, 20);

              R2 := Rect(0, 0, 20, Ch);
              OffsetRect(R2, R.Left, R.Top);

              if PtInRect(R2, P) then
                Result.Part := scdlhLayerHideButton
              else begin
                Inc(R.Left, 20);

                R2 := Rect(0, 0, 20, Ch);
                OffsetRect(R2, R.Left, R.Top);

                if PtInRect(R2, P) then
                  Result.Part := scdlhLayerLockButton
                else begin
                  Inc(R.Left, 20);

                  if PtInRect(R, P) then
                    Result.Part := scdlhLayerCaption;
                end;
              end;
            end;

            Exit;
          end;

          Inc(T, Ch);
          if T >= CR.Bottom then
            Exit;
        end;
      
        if C > 0 then
          Dec(C);

        if L.Expanded then
          for J := 0 to L.Count-1 do
          begin
            if C = 0 then
            begin
              R := CR;
              R.Top := T;
              R.Bottom := T + Ph;

              if PtInRect(R, P) then
              begin
                Result.Layer := I;
                Result.Shape := J;

                Inc(R.Left, 20);

                R2 := Rect(0, 0, 20, Ph);
                OffsetRect(R2, R.Left, R.Top);

                if PtInRect(R2, P) then
                  Result.Part := scdlhItemHideButton
                else begin
                  Inc(R.Left, 20);

                  R2 := Rect(0, 0, 20, Ph);
                  OffsetRect(R2, R.Left, R.Top);

                  if PtInRect(R2, P) then
                    Result.Part := scdlhItemLockButton
                  else begin
                    Inc(R.Left, 20);

                    if PtInRect(R, P) then
                      Result.Part := scdlhItem;
                  end;
                end;

                Exit;
              end;

              Inc(T, Ph);
              if T >= CR.Bottom then
                Exit;
            end;

            if C > 0 then
              Dec(C);
          end;

        Inc(T);
      end;
    end;
  end;
end;

procedure TSCDeCustomLayerManager.InitializeEventClient;
begin
  with FEventClient do
  begin
    OnLayerChange := LayerChanged;
    OnSelectionChange := SelectionChanged;
    OnActiveLayerChange := ActiveLayerChanged;
    OnInsertLayer := LayerAdded;
    OnRemoveLayer := LayerRemoved;
    OnInsertShape := ShapeAdded;
    OnRemoveShape := ShapeRemoved;
    OnClearShape  := ShapeCleared;
  end;
end;

function TSCDeCustomLayerManager.LoadBitmap(ResName: String): TBitmap;
begin
  Result := TBitmap.Create;
  with Result do
  begin
    Handle := Windows.LoadBitmap(HInstance, PChar(ResName));
    TransparentMode := tmAuto;
    Transparent := True;
  end;

  if Result.Handle = 0 then
    FreeAndNil(Result);
end;

procedure TSCDeCustomLayerManager.LoadBitmaps;
begin
  FExpandBitmap := LoadBitmap('SCDE_LM_EXPAND');
  FCollapseBitmap := LoadBitmap('SCDE_LM_COLLAPSE');
  FVisibleBitmap := LoadBitmap('SCDE_LM_VISIBLE');
  FActiveBitmap := LoadBitmap('SCDE_LM_ACTIVE');
  FLockedBitmap := LoadBitmap('SCDE_LM_LOCK');
  FLayerBitmap := LoadBitmap('SCDE_LM_LAYER');
  FInvisibleLayerBitmap := LoadBitmap('SCDE_LM_INVISIBLE_LAYER');
end;

procedure TSCDeCustomLayerManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEditor) then
  begin
    FEditor.UnRegisterClient(FEventClient);

    FEditor := nil;
    ClearLayers;
  end;
end;

procedure TSCDeCustomLayerManager.Paint;
var
  CR, R: TRect;
  L: TSCDeListLayer;
  Li: TSCDeListShape;
  I, J, T, C, Ch, Ph: Integer;
begin
  inherited Paint;

  if (FEditor <> nil) and (FLayers.Count > 0) and
    not (csDesigning in ComponentState) then
  begin
    CR := Self.ClientRect;

    T := 0;
    C := FTopPosition;

    Ch := GetCaptionHeight;
    Ph := GetPreviewHeight;

    for I := 0 to FLayers.Count-1 do
    begin
      if T >= CR.Bottom then
        Exit;

      L := TSCDeListLayer(FLayers[I]);

      if C = 0 then
      begin
        R := CR;
        R.Top := T;
        R.Bottom := T + Ch;

        DrawLayerCaption(L, R);

        Inc(T, Ch);
        if T >= CR.Bottom then
          Exit;
      end;
      
      if C > 0 then
        Dec(C);

      if L.Expanded then
        for J := 0 to L.Count-1 do
        begin
          Li := L.Shapes[J];

          if C = 0 then
          begin
            R := CR;
            R.Top := T;
            R.Bottom := T + Ph;

            DrawLayerItem(Li, R);

            Inc(T, Ph);
            if T >= CR.Bottom then
              Exit;
          end;

          if C > 0 then
            Dec(C);
        end;

      with Canvas do
      begin
        Pen.Style := psSolid;
        Pen.Color := clBtnShadow;

        MoveTo(CR.Left, T - 1);
        LineTo(CR.Right, T - 1);

        Pen.Color := cl3DDkShadow;

        MoveTo(CR.Left, T);
        LineTo(CR.Right, T);
      end;

      Inc(T);
    end;
  end;
end;

procedure TSCDeCustomLayerManager.ReCalculateTopPosition;
var
  OldPos: Integer;
begin
  OldPos := FTopPosition;
  VerifyTopPos(FTopPosition);

  if OldPos <> FTopPosition then
    UpdateScrollbar;
end;

procedure TSCDeCustomLayerManager.ReleaseBitmaps;
begin
  if FExpandBitmap <> nil then FreeAndNil(FExpandBitmap);
  if FCollapseBitmap <> nil then FreeAndNil(FCollapseBitmap);
  if FVisibleBitmap <> nil then FreeAndNil(FVisibleBitmap);
  if FActiveBitmap <> nil then FreeAndNil(FActiveBitmap);
  if FLockedBitmap <> nil then FreeAndNil(FLockedBitmap);
  if FLayerBitmap <> nil then FreeAndNil(FLayerBitmap);
  if FInvisibleLayerBitmap <> nil then FreeAndNil(FInvisibleLayerBitmap);
end;

procedure TSCDeCustomLayerManager.SetColors(Value: TSCDeLayerManagerColors);
begin
  FColors.Assign(Value);
end;

procedure TSCDeCustomLayerManager.SetEditor(Value: TSCDeCustomEditor);
begin
  if FEditor <> Value then
  begin
    if FEditor <> nil then
    begin
      FEditor.UnRegisterClient(FEventClient);
    {$IFDEF SCDE_DELPHI5_UP}
      FEditor.RemoveFreeNotification(Self);
    {$ENDIF}
    end;

    FEditor := Value;
    
    if FEditor <> nil then
    begin
      FEditor.RegisterClient(FEventClient);
      FEditor.FreeNotification(Self);
    end;

    FTopPosition := 0;
    UpdateLayers;
  end;
end;

function TSCDeCustomLayerManager.UpdateScrollbar: Boolean;
var
  ScrlVisible: Boolean;
  SiOld, SiNew: TSCDeScrollInfo;
  Dist, VisibleCount, ScrollCount: Integer;
begin
  Result := False;
  if HandleAllocated and (FCreatingWnd = 0) and
    not (csLoading in ComponentState) then
  begin
    SiOld := Self.GetScrollbarInfo(scdskVertical);
    ScrlVisible := SiOld.Visible and (SiOld.Page <= SiOld.Max);

    SiNew := SiOld;

    SiNew.Min := 0;

    ScrollCount := GetScrollCount;
    VisibleCount := GetVisibleItemCount;

    if (FEditor <> nil) and (FLayers.Count > 0) and
      (VisibleCount > 1) and (ScrollCount > 0) and
      not (csDesigning in ComponentState) then
    begin
      SINew.Page := VisibleCount - ScrollCount;

      SINew.Min  := 0;
      SINew.Max  := VisibleCount;
      SINew.Pos  := FTopPosition;
    end else
    begin
      SiNew.Page := 1;
      SiNew.Min  := 0;
      SiNew.Max  := 0;
      SiNew.Pos  := 0;
    end;

    SiNew.TrimPageSize := True;
    SiNew.LargeChange  := SiNew.Page;

    Dist := SiNew.Max - SiNew.Min;
    SiNew.Visible := (SiNew.Page > -1) and (SiNew.Max > 0) and
      (Dist > 0) and (SiNew.Page < Dist);

    if (SiNew.Min <> SiOld.Min) or (SiNew.Max <> SiOld.Max) or
      (SiNew.Page <> SiOld.Page) or (SiNew.Pos <> SiOld.Pos) or
      (SiNew.Visible <> SiOld.Visible) then
    begin
      Self.SetScrollbarInfo(scdskVertical, SiNew);

      if Integer(SiNew.Page) > SiNew.Max then
        SetTopPosition(SiNew.Min);
    end;

    Result := ScrlVisible <> (Integer(SiNew.Page) <= SiNew.Max);
    if Result then
      Invalidate;
  end;
end;

procedure TSCDeCustomLayerManager.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  L: TSCDeListLayer;
  Li: TSCDeListShape;
  Hit: TSCDeListHitInfo;
  A: array of TSCDeShapeBase;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (FEditor <> nil) and (FLayers.Count > 0) then
  begin
    Hit := HitInfo(Point(X, Y));

    case Hit.Part of
      scdlhLayerExpandButton:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        L.Expanded := not L.Expanded;
      end;

      scdlhLayerHideButton:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        L.Layer.Visible := not L.Layer.Visible;
        Invalidate;
      end;

      scdlhLayerLockButton:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        L.Layer.Locked := not L.Layer.Locked;
        Invalidate;
      end;

      scdlhLayerCaption:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        
        if not L.Layer.Locked then
        begin
          if (L.Layer.ItemCount > 0) then
          begin
            SetLength(A, L.Layer.ItemCount);
            for I := 0 to L.Layer.ItemCount-1 do
              A[I] := TSCDeShapeBase(L.Layer.Items[I]);

            FEditor.SelectList(A);
          end;

          TSCDeFakeEditor(FEditor).ActiveLayer := L.Layer;
        end;
      end;

      scdlhItem:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);

        if not L.Layer.Locked then
        begin
          Li := L.Shapes[Hit.Shape];

          if not (ssShift in Shift) then
            FEditor.Select(Li.Shape)
          else begin
            if FEditor.IsSelected(Li.Shape) then
              FEditor.RemoveSelection(Li.Shape)
            else if FEditor.CanSelect(Li.Shape) then
              FEditor.AddSelection(Li.Shape);
          end;

          TSCDeFakeEditor(FEditor).ActiveLayer := L.Layer;
        end;
      end;

      scdlhItemHideButton:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        Li := L.Shapes[Hit.Shape];

        Li.Shape.Visible := not Li.Shape.Visible;
        Invalidate;
      end;

      scdlhItemLockButton:
      begin
        L := TSCDeListLayer(FLayers[Hit.Layer]);
        Li := L.Shapes[Hit.Shape];

        Li.Shape.Locked := not Li.Shape.Locked;
        Invalidate;
      end;
    end;
  end;
end;

procedure TSCDeCustomLayerManager.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

end;

procedure TSCDeCustomLayerManager.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

end;

procedure TSCDeCustomLayerManager.LayerAdded(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  L: TSCDeListLayer;
begin
  L := TSCDeListLayer.Create(nil, TSCDeLayer(Shape));
  L.FOwner := Self;
  FLayers.Add(L);

  if not UpdateScrollbar then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.LayerRemoved(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  I: Integer;
  L: TSCDeListLayer;
begin
  for I := 0 to FLayers.Count-1 do
  begin
    L := TSCDeListLayer(FLayers[I]);

    if L.Layer = Shape then
    begin
      L.FOwner := nil;
      FLayers.Delete(I);

      if not UpdateScrollbar then
        Invalidate;

      Break;
    end;
  end;
end;

procedure TSCDeCustomLayerManager.ShapeAdded(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  I: Integer;
  L, Sl: TSCDeListLayer;
  Li: TSCDeListShape;
begin
  Sl := nil;
  for I := 0 to FLayers.Count-1 do
  begin
    L := TSCDeListLayer(FLayers[I]);

    if L.Layer = Shape.Owner then
    begin
      Sl := L;
      Li := TSCDeListShape.Create(nil, Shape);

      Li.FOwner := L;
      L.FShapes.Add(Li);

      Break;
    end;

    if Sl <> nil then
      Break;
  end;

  if (Sl <> nil) and Sl.Expanded and UpdateScrollbar then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.ShapeRemoved(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  I, J: Integer;
  L, Sl: TSCDeListLayer;
  Li: TSCDeListShape;
begin
  Sl := nil;
  for I := 0 to FLayers.Count-1 do
  begin
    L := TSCDeListLayer(FLayers[I]);

    for J := 0 to L.Count-1 do
    begin
      Li := L.Shapes[J];

      if Li.Shape = Shape then
      begin
        Sl := L;
        Li.FOwner := nil;
        L.FShapes.Delete(J);

        Break;
      end;
    end;

    if Sl <> nil then
      Break;
  end;

  if (Sl <> nil) and Sl.Expanded and UpdateScrollbar then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.ActiveLayerChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TSCDeCustomLayerManager.LayerChanged(Sender: TObject;
  Shape: TSCDeShapeBase);
begin
  if FShowPreview then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.SelectionChanged(Sender: TObject);
begin
  Invalidate;
end;

function TSCDeCustomLayerManager.GetScrollCount: Integer;
var
  CR: TRect;
  L: TSCDeListLayer;
  I, J, H, Cnt, Ch, Ph: Integer;
begin
  Result := 0;

  CR := ClientRect;
  if not IsRectEmpty(CR) then
  begin
    Ch := GetCaptionHeight;
    Ph := GetPreviewHeight;

    H := CR.Bottom - CR.Top;

    Cnt := 0;
    for I := FLayers.Count-1 downto 0 do
    begin
      L := TSCDeListLayer(FLayers[I]);
      if L.Expanded then
        for J := L.Count-1 downto 0 do
        begin
          Dec(H, Ph);
          if (H < 0) then
            Break;

          Inc(Cnt);
        end;

      Dec(H, Ch + 1);
      if (H < 0) then
        Break;

      Inc(Cnt);
    end;

    Result := GetVisibleItemCount - Cnt;
    if Result < 0 then
      Result := 0;
  end;
end;

procedure TSCDeCustomLayerManager.LayerExpandChanged(L: TSCDeListLayer);
begin
  if not UpdateScrollbar then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.VerifyTopPos(var TopPos: Integer);
var
  Max: Integer;
begin
  if (TopPos < 0) or (csDesigning in ComponentState) then
    TopPos := 0
  else begin
    Max := GetScrollCount;
    if TopPos > Max then
      TopPos := Max;
  end;
end;

procedure TSCDeCustomLayerManager.SetTopPosition(Value: Integer);
var
  OldPos: Integer;
begin
  Inc(FScrollPosChanging);
  try
    if (csDesigning in ComponentState) then
      Value := 0;

    VerifyTopPos(Value);

    OldPos := FTopPosition;

    if FTopPosition <> Value then
    begin
      FTopPosition := Value;
      UpdateScrollbar;
    end;

    FTopPosition := Value;
    if FScrollbarChanging = 0 then
      TSCDeListScrollbar(ScrollbarVert).Position := FTopPosition;

    if OldPos <> FTopPosition then
      Invalidate;
  finally
    Dec(FScrollPosChanging);
  end;
end;

function TSCDeCustomLayerManager.GetScrollbarClass: TSCDeCustomControlScrollbarClass;
begin
  Result := TSCDeListScrollbar;
end;

function TSCDeCustomLayerManager.CanScrollToPos(Kind: TSCDeScrollbarKind;
  var NewValue: Integer): Boolean;
var
  Max: Integer;
begin
  Result := True;
  if NewValue < 0 then
    NewValue := 0
  else
  if Kind = scdskVertical then
  begin
    Max := GetScrollCount;
    if NewValue > Max then
      NewValue := Max;
  end;
end;

procedure TSCDeCustomLayerManager.DoScrollerPositionChanged(
  Kind: TSCDeScrollbarKind);
var
  Sb: TSCDeListScrollbar;
begin
  if FScrollPosChanging = 0 then
  begin
    Inc(FScrollbarChanging);
    try
      if Kind = scdskVertical then
      begin
        Sb := TSCDeListScrollbar(ScrollbarVert);
        SetTopPosition(Sb.Position);
      end;
    finally
      Dec(FScrollbarChanging);
    end;
  end;  
end;

procedure TSCDeCustomLayerManager.WndProc(var Message: TMessage);
begin
  if (DragMode = dmAutomatic) and
    not (csDesigning in ComponentState) and not Dragging and
    ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) then
  begin
    if IsControlMouseMsg(TWMMouse(Message)) then
      Exit;

    if not Focused and CanFocus then
      SetFocus;

    ControlState := ControlState + [csLButtonDown];
    Dispatch(Message);

    Exit;
  end;

  inherited WndProc(Message);
end;

procedure TSCDeCustomLayerManager.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    Style := Style and not WS_BORDER or WS_CLIPSIBLINGS or
      WS_CLIPCHILDREN;
  end;
end;

procedure TSCDeCustomLayerManager.CreateWnd;
begin
  Inc(FCreatingWnd);
  try
    inherited CreateWnd;
  finally
    Dec(FCreatingWnd);
  end;

  UpdateScrollbar;
end;

procedure TSCDeCustomLayerManager.Loaded;
begin
  inherited Loaded;
  UpdateScrollbar;
end;

procedure TSCDeCustomLayerManager.DragCanceled;
var
  P: TPoint;
  Message: TWMMouse;
begin
  with Message do
  begin
    Msg := WM_LBUTTONDOWN;
    GetCursorPos(P);
    Pos := PointToSmallPoint(ScreenToClient(P));
    Keys := 0;
    Result := 0;
  end;
  
  DefaultHandler(Message);
  Message.Msg := WM_LBUTTONUP;
  DefaultHandler(Message);
end;

procedure TSCDeCustomLayerManager.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Message.WheelDelta < 0 then
      SetTopPosition(FTopPosition + 1)
    else
    if Message.WheelDelta > 0 then
      SetTopPosition(FTopPosition - 1);
  end;
end;

procedure TSCDeCustomLayerManager.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateScrollbar;
end;

procedure TSCDeCustomLayerManager.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  if not (csDesigning in ComponentState) then
  begin
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;

    GetScrollInfo(Self.Handle, SB_VERT, SI);

    case Message.ScrollCode of
      SB_LINEUP:
        SetTopPosition(FTopPosition - 1);
      SB_LINEDOWN:
        SetTopPosition(FTopPosition + 1);
      SB_PAGEUP:
        SetTopPosition(GetPageUpPos(FTopPosition));
      SB_PAGEDOWN:
        SetTopPosition(GetPageDownPos(FTopPosition));
      SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if SI.nTrackPos <= SI.nMin then
          SetTopPosition(SI.nMin)
        else
        if SI.nTrackPos >= SI.nMax then
          SetTopPosition(SI.nMax)
        else
          SetTopPosition(SI.nTrackPos);
      end;
      SB_TOP:
        SetTopPosition(SI.nMin);
      SB_BOTTOM:
        SetTopPosition(SI.nMax);
    end;
  end;
end;

function TSCDeCustomLayerManager.GetPageDownPos(TopPos: Integer): Integer;
begin
  Result := TopPos + 3;
end;

function TSCDeCustomLayerManager.GetPageUpPos(TopPos: Integer): Integer;
begin
  Result := TopPos - 3;
end;

procedure TSCDeCustomLayerManager.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSCDeCustomLayerManager.ShapeCleared(Sender: TObject;
  Shape: TSCDeShapeBase);
var
  I, J: Integer;
  L, Sl: TSCDeListLayer;
  Li: TSCDeListShape;
begin
  Sl := nil;
  for I := 0 to FLayers.Count-1 do
  begin
    L := TSCDeListLayer(FLayers[I]);

    if L.Layer = Shape then
    begin
      Sl := L;

      for J := L.Count-1 downto 0 do
      begin
        Li := L.Shapes[J];

        L.FShapes.Delete(J);
        Li.FOwner := nil;
      end;

      Break;
    end;
  end;

  if (Sl <> nil) and Sl.Expanded and UpdateScrollbar then
    Invalidate;
end;

function TSCDeCustomLayerManager.InitializeItemInfo(
  Ii: TSCDeListHitInfo): TSCDeListHitInfo;
begin
  Result.Pos := Point(0, 0);
  Result.Layer := -1;
  Result.Shape := -1;
  Result.Part := scdlhNone;
end;

procedure TSCDeCustomLayerManager.ShapeRemoved(S: TSCDeListShape);
begin
  ReCalculateTopPosition;
  Invalidate;
end;

procedure TSCDeCustomLayerManager.LayerRemoved(L: TSCDeListLayer);
begin
  if (L <> nil) and (L.Owner = Self) then
  begin
    L.FOwner := nil;
    L.FLayer := nil;

    FLayers.Remove(L);

    ReCalculateTopPosition;
    Invalidate;
  end;
end;

procedure TSCDeCustomLayerManager.ClearLayers;
var
  L: TSCDeListLayer;
begin
  while FLayers.Count > 0 do
  begin
    L := TSCDeListLayer(FLayers[FLayers.Count-1]);
    FLayers.Delete(FLayers.Count-1);

    L.FOwner := nil;
    L.FLayer := nil;

    L.Free;
  end;
end;

procedure TSCDeCustomLayerManager.UpdateLayers;
var
  I: Integer;
  L: TSCDeListLayer;
begin
  ClearLayers;
  if (FEditor <> nil) and not (csDesigning in ComponentState) then
    for I := 0 to FEditor.LayerCount-1 do
    begin
      L := TSCDeListLayer.Create(Self, TSCDeFakeEditor(FEditor).Layers[I]);
      FLayers.Add(L);
    end;

  if not UpdateScrollbar then
    Invalidate;
end;

procedure TSCDeCustomLayerManager.SetShowPreview(Value: Boolean);
begin
  if FShowPreview <> Value then
  begin
    FShowPreview := Value;
    if (FLayers.Count > 0) and not UpdateScrollbar then
      Invalidate;
  end;
end;

procedure TSCDeCustomLayerManager.SetShowSelectionIndicator(
  Value: Boolean);
begin
  if FShowSelectionIndicator <> Value then
  begin
    FShowSelectionIndicator := Value;
    if FLayers.Count > 0 then
      Invalidate;
  end;
end;

{ TSCDeListLayer }

procedure TSCDeListLayer.ClearShapes;
var
  S: TSCDeListShape;
begin
  while FShapes.Count > 0 do
  begin
    S := TSCDeListShape(FShapes[FShapes.Count-1]);
    FShapes.Delete(FShapes.Count-1);

    S.FOwner := nil;
    S.FShape := nil;

    S.Free;
  end;
end;

constructor TSCDeListLayer.Create(AOwner: TSCDeCustomLayerManager;
  ALayer: TSCDeLayer);
begin
  FShapes := TList.Create;

  inherited Create(ALayer);
  FOwner := AOwner;
  FLayer := ALayer;
  FExpanded := True;

  UpdateShapes;
end;

destructor TSCDeListLayer.Destroy;
begin
  ClearShapes;
  if FOwner <> nil then
    FOwner.LayerRemoved(Self);

  inherited Destroy;
end;

function TSCDeListLayer.GetShape(Index: Integer): TSCDeListShape;
begin
  Result := TSCDeListShape(FShapes[Index]);
end;

function TSCDeListLayer.GetShapeCount: Integer;
begin
  Result := FShapes.Count;
end;

procedure TSCDeListLayer.SetExpanded(Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if FOwner <> nil then
      FOwner.LayerExpandChanged(Self);
  end;
end;

procedure TSCDeListLayer.ShapeRemoved(S: TSCDeListShape);
begin
  if (S <> nil) and (S.Owner = Self) then
  begin
    S.FOwner := nil;
    S.FShape := nil;

    FShapes.Remove(S);

    if FOwner <> nil then
      FOwner.ShapeRemoved(S);
  end;
end;

procedure TSCDeListLayer.UpdateShapes;
var
  I: Integer;
  S: TSCDeListShape;
begin
  ClearShapes;
  if FLayer <> nil then
    for I := 0 to FLayer.ItemCount-1 do
    begin
      S := TSCDeListShape.Create(Self, FLayer[I]);
      FShapes.Add(S);
    end;
end;

{ TSCDeListItem }

constructor TSCDeListItem.Create(AShape: TSCDeShapeBase);
begin
  inherited Create;
  FShape := AShape;
end;

{ TSCDeListShape }

constructor TSCDeListShape.Create(AOwner: TSCDeListLayer;
  AShape: TSCDeShapeBase);
begin
  inherited Create(AShape);
  FOwner := AOwner;
end;

destructor TSCDeListShape.Destroy;
begin
  if FOwner <> nil then
    FOwner.ShapeRemoved(Self);
  inherited Destroy;
end;

{ TSCDeLayerManagerColors }

procedure TSCDeLayerManagerColors.Assign(Source: TPersistent);
begin
  if Source is TSCDeLayerManagerColors then
  begin
    with TSCDeLayerManagerColors(Source) do
    begin
      Self.FActiveLayer := ActiveLayer;
      Self.FActiveLayerText := ActiveLayerText;
      Self.FCaption := Caption;
      Self.FCaptionText := CaptionText;
      Self.FSelectionIndicator := SelectionIndicator;
      Self.FSelection := Selection;
      Self.FSelectionText := SelectionText;
    end;

    Changed;
  end else
    inherited Assign(Source);
end;

procedure TSCDeLayerManagerColors.Changed;
begin
  if FOwner <> nil then FOwner.ColorsChanged;
end;

constructor TSCDeLayerManagerColors.Create(AOwner: TSCDeCustomLayerManager);
begin
  inherited Create;
  FOwner := AOwner;
  FActiveLayer := $00B49184;
  FActiveLayerText := clBtnText;
  FCaption := clBtnFace;
  FCaptionText := clBtnText;
  FSelectionIndicator := clLime;
  FSelection := clHighlight;
  FSelectionText := clHighlightText;
end;

function TSCDeLayerManagerColors.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSCDeLayerManagerColors.SetActiveLayer(Value: TColor);
begin
  if FActiveLayer <> Value then
  begin
    FActiveLayer := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetActiveLayerText(Value: TColor);
begin
  if FActiveLayerText <> Value then
  begin
    FActiveLayerText := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetCaption(Value: TColor);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetCaptionText(Value: TColor);
begin
  if FCaptionText <> Value then
  begin
    FCaptionText := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetSelectionIndicator(Value: TColor);
begin
  if FSelectionIndicator <> Value then
  begin
    FSelectionIndicator := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetSelection(Value: TColor);
begin
  if FSelection <> Value then
  begin
    FSelection := Value;
    Changed;
  end;
end;

procedure TSCDeLayerManagerColors.SetSelectionText(Value: TColor);
begin
  if FSelectionText <> Value then
  begin
    FSelectionText := Value;
    Changed;
  end;
end;

end.
