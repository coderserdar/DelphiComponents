{************************************************}
{                                                }
{  ATImageBox Component                          }
{  Copyright (C) 2007-09 Alexey Torgashin        }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

{$BOOLEVAL OFF} //Short boolean evaluation required
{$I ATImageBoxOptions.inc} //ATImageBox options

unit ATImageBox;

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls,
  {$ifdef TNT} TntGraphics, {$endif}
  Forms;

const
  cViewerDefaultResampleDelay = 300;
  cViewerImageScales: array[1 .. 30] of Integer = (
    7, 10, 15, 20, 25, 30,
    40, 50, 60, 70, 80, 90, 100,
    125, 150, 175, 200, 250, 300, 350, 400, 450, 500,
    600, 700, 800, 1000, 1200, 1400, 1600);


type
  TPictureWide = {$ifdef TNT}TTntPicture{$else}TPicture{$endif};
  TATScrollAltEvent = procedure(Sender: TObject; Inc: Boolean) of object;

type
  TATImage = class(TGraphicControl)
  private
    FPicture: TPictureWide;
    FOnPaint: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FResample: Boolean;
    FResampleBackColor: TColor;
    FDrawing: Boolean;
    FProportional: Boolean;
    FTimer: TTimer; //Helper timer to do resampling after a delay
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPictureWide);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);    
    procedure SetResample(Value: Boolean);
    procedure TimerTimer(Sender: TObject);
    procedure PaintResampled;
    function GetResampleDelay: Integer;
    procedure SetResampleDelay(AValue: Integer);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPictureWide read FPicture write SetPicture;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Resample: Boolean read FResample write SetResample default False;
    property ResampleDelay: Integer read GetResampleDelay write SetResampleDelay default cViewerDefaultResampleDelay;
    property ResampleBackColor: TColor read FResampleBackColor write FResampleBackColor default clWhite;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDock;
    property OnStartDrag;
  end;

type
  TATImageBox = class(TScrollBox)
  private
    FFocusable: Boolean;
    FImage: TATImage;
    FImageLabel: TLabel;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FImageFit: Boolean;
    FImageFitOnlyBig: Boolean;
    FImageCenter: Boolean;
    FImageScale: Integer;
    FImageKeepPosition: Boolean;
    FImageDrag: Boolean;
    FImageDragCursor: TCursor;
    FImageScaleCursor: TCursor;
    FImageDragging: Boolean;
    FImageDraggingPoint: TPoint;
    FImageMouseDown: Boolean;
    FOnScroll: TNotifyEvent;
    FOnScrollAlt: TATScrollAltEvent;
    FOnOptionsChange: TNotifyEvent;
    procedure DoScroll;
    procedure DoScrollAlt(AInc: Boolean);
    procedure DoOptionsChange;
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure UpdateImagePosition(AResetPosition: Boolean = False);
    procedure UpdateImageLabelPosition;
    procedure SetImageFit(AValue: Boolean);
    procedure SetImageFitOnlyBig(AValue: Boolean);
    procedure SetImageCenter(AValue: Boolean);
    procedure SetImageScale(AValue: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImagePaint(Sender: TObject);
    procedure ImageProgress(Sender: TObject;
      Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);

  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateImageInfo;
    procedure IncreaseImageScale(AIncrement: Boolean);
    property Image: TATImage read FImage;
    property ImageLabel: TLabel read FImageLabel;
    property ImageWidth: Integer read FImageWidth;
    property ImageHeight: Integer read FImageHeight;
    property ImageScale: Integer read FImageScale write SetImageScale;

  protected
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  published
    property Focusable: Boolean read FFocusable write FFocusable default True;
    property ImageFitToWindow: Boolean read FImageFit write SetImageFit default False;
    property ImageFitOnlyBig: Boolean read FImageFitOnlyBig write SetImageFitOnlyBig default True;
    property ImageCenter: Boolean read FImageCenter write SetImageCenter default True;
    property ImageKeepPosition: Boolean read FImageKeepPosition write FImageKeepPosition default True;
    property ImageDrag: Boolean read FImageDrag write FImageDrag default True;
    property ImageDragCursor: TCursor read FImageDragCursor write FImageDragCursor default crSizeAll;
    property ImageScaleCursor: TCursor read FImageScaleCursor write FImageScaleCursor default crSizeNS;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnScrollAlt: TATScrollAltEvent read FOnScrollAlt write FOnScrollAlt;
    property OnOptionsChange: TNotifyEvent read FOnOptionsChange write FOnOptionsChange;
  end;


procedure Register;


implementation

uses
  SysUtils
  {$ifdef GIF} , GifImage {$endif};


{ Constants }

const
  cImageLineSize = 50; //Line size: pixels to scroll by arrows and mouse sheel
  cImageGapSize = 20; //Gap size: PgUp/PgDn/Home/End scroll by control size minus gap size


{ Helper functions }

function IMax(N1, N2: Integer): Integer;
begin
  if N1 >= N2 then
    Result := N1
  else
    Result := N2;
end;

function IMin(N1, N2: Integer): Integer;
begin
  if N1 <= N2 then
    Result := N1
  else
    Result := N2;
end;

{
We need to "fix" icon sizes. Icon should be drawn once before its sizes are to be read.
http://qc.codegear.com/wc/qcmain.aspx?d=6018
}
procedure FixIcon(AIcon: TIcon);
var
  Bmp: TBitmap;
begin
  try
    Bmp:= TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.Canvas.Draw(0, 0, AIcon);
    finally
      Bmp.Free;
    end;
  except
  end;
end;

{
Scaling doesn't work with icons. So, we need to convert icon to a bitmap,
preferrably with PixelFormat = pf24bit.
}
function FixImageFormat(AImage: TATImage; ABackColor: TColor): Boolean;
var
  bmp: TBitmap;
begin
  Result := True;
  with AImage.Picture do
    if (not (Graphic is TBitmap)) or ((TBitmap(Graphic).PixelFormat <> pf24Bit)) then
      try
        bmp := TBitmap.Create;
        try
          bmp.PixelFormat := pf24bit;
          bmp.Width := Graphic.Width;
          bmp.Height := Graphic.Height;
          bmp.Canvas.Brush.Color:= ABackColor;
          bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
          bmp.Canvas.Draw(0, 0, Graphic);
          AImage.Picture.Graphic := bmp;
        finally
          bmp.Free;
        end;
      except
        Result := False;
      end;
end;

{ TATImage }

constructor TATImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FPicture := TPictureWide.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  Height := 105;
  Width := 105;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := cViewerDefaultResampleDelay;
  FTimer.OnTimer := TimerTimer;
  FResampleBackColor := clWhite;
end;

destructor TATImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TATImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

function TATImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := Picture.Width;
  h := Picture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

procedure TATImage.Paint;
var
  Save: Boolean;
begin
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  Save := FDrawing;
  FDrawing := True;
  try
    //Do the standard rendering
    with inherited Canvas do
      StretchDraw(DestRect, Picture.Graphic);

    //Do the delayed resampling rendering
    if FResample 
      //Do not resample metafiles:
      and not (Picture.Graphic is TMetafile)
      {$ifdef GIF}
      //Do not resample *animated* GIF images:
      and not ((Picture.Graphic is TGifImage) and ((Picture.Graphic as TGifImage).Images.Count > 1))
      {$endif} then
    begin
      FTimer.Enabled := False;
      FTimer.Enabled := True;
    end;
  finally
    FDrawing := Save;
  end;
end;

procedure TATImage.PaintResampled;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;

  try
    with Bmp do
    begin
      PixelFormat := pf24bit;
      Width := Picture.Width;
      Height := Picture.Height;
      Canvas.Brush.Color := FResampleBackColor;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      Canvas.Draw(0, 0, Picture.Graphic);
    end;

    with inherited Canvas do
    begin
      SetStretchBltMode(Handle, STRETCH_HALFTONE);
      SetBrushOrgEx(Handle, 0, 0, nil);
      StretchBlt(
        Handle,
        DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
        Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
        SRCCOPY);
    end;
  finally
    Bmp.Free;
  end;

  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

function TATImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
        (Tmp.PaletteModified) then
  begin
        if (Tmp.Palette = 0) then
          Tmp.PaletteModified := False
        else
        begin
          ParentForm := GetParentForm(Self);
          if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
          begin
                if FDrawing then
                  ParentForm.Perform(wm_QueryNewPalette, 0, 0)
                else
                  PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
                Result := True;
                Tmp.PaletteModified := False;
          end;
        end;
  end;
end;

procedure TATImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
    if DoPaletteChange then Update
    else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TATImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    PictureChanged(Self);
  end;
end;

procedure TATImage.SetPicture(Value: TPictureWide);
begin
  FPicture.Assign(Value);
end;

procedure TATImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;
end;

procedure TATImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TATImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;
end;

procedure TATImage.SetResample(Value: Boolean);
begin
  //Resampling works only under WinNT, since
  //STRETCH_HALFTONE doesn't work under Win9x
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    if Value <> FResample then
    begin
      FResample := Value;
      PictureChanged(Self);
    end;
end;

procedure TATImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
        SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;
  if G <> nil then
  begin
        if not ((G is TMetaFile) or (G is TIcon)) then
          G.Transparent := FTransparent;
        D := DestRect;
        if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
           (D.Right >= Width) and (D.Bottom >= Height) then
          ControlStyle := ControlStyle + [csOpaque]
        else  // picture might not cover entire clientrect
          ControlStyle := ControlStyle - [csOpaque];
        if DoPaletteChange and FDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;
end;

function TATImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

procedure TATImage.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  PaintResampled;
end;

function TATImage.GetResampleDelay: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TATImage.SetResampleDelay(AValue: Integer);
begin
  FTimer.Interval := AValue;
end;


{ TATImageBox }

constructor TATImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //Init inherited properties
  AutoScroll := False;
  DoubleBuffered := True; //To remove flicker when new image is loaded
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;

  //Init fields
  FFocusable:= True;
  FImageFit := False;
  FImageFitOnlyBig := True;
  FImageCenter := True;
  FImageWidth := 0;
  FImageHeight := 0;
  FImageScale := 100;
  FImageKeepPosition := True;
  FImageDrag := True;
  FImageDragCursor := crSizeAll;
  FImageScaleCursor := crSizeNS;
  FImageDragging := False;
  FImageDraggingPoint := Point(0, 0);
  FImageMouseDown := False;

  //Init objects
  FImage := TATImage.Create(Self);
  with FImage do
  begin
    Parent := Self;
    Align := alNone;
    AutoSize := False;
    IncrementalDisplay := True;
    OnMouseDown := ImageMouseDown;
    OnMouseUp := ImageMouseUp;
    OnMouseMove := ImageMouseMove;
    OnPaint := ImagePaint;
    OnProgress := ImageProgress;
  end;

  FImageLabel := TLabel.Create(Self);
  with FImageLabel do
  begin
    Parent := Self;
    Visible := False;
    Brush.Style := bsClear;
    Font.Style := [fsBold];
    Font.Color := clWhite;
    Caption := '';
  end;

  //Init event handlers
  OnMouseWheelUp := MouseWheelUp;
  OnMouseWheelDown := MouseWheelDown;
end;

procedure TATImageBox.DoScroll;
begin
  UpdateImageLabelPosition;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATImageBox.DoScrollAlt(AInc: Boolean);
begin
  if Assigned(FOnScrollAlt) then
    FOnScrollAlt(Self, AInc);
end;

procedure TATImageBox.WMHScroll(var Msg: TMessage);
begin
  inherited;
  DoScroll;
end;

procedure TATImageBox.WMVScroll(var Msg: TMessage);
begin
  inherited;
  DoScroll;
end;

procedure TATImageBox.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = []) then
  begin
    with VertScrollBar do
      Position := Position - cImageLineSize;
    DoScroll;
  end
  else
  if (Shift = [ssShift]) then
  begin
    with HorzScrollBar do
      Position := Position - cImageLineSize;
    DoScroll;
  end
  else
  if (Shift = [ssCtrl]) or FImageMouseDown then
  begin
    IncreaseImageScale(True);
    FImageDragging := False;
    if FImageMouseDown then
      Screen.Cursor := FImageScaleCursor;
  end;

  Handled := True;
end;

procedure TATImageBox.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = []) then
  begin
    with VertScrollBar do
      Position := Position + cImageLineSize;
    DoScroll;
  end
  else
  if (Shift = [ssShift]) then
  begin
    with HorzScrollBar do
      Position := Position + cImageLineSize;
    DoScroll;
  end
  else
  if (Shift = [ssCtrl]) or FImageMouseDown then
  begin
    IncreaseImageScale(False);
    FImageDragging := False;
    if FImageMouseDown then
      Screen.Cursor := FImageScaleCursor;
  end;

  Handled := True;
end;

procedure TATImageBox.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TATImageBox.KeyDown(var Key: Word; Shift: TShiftState);

  function PageSize(AClientSize: Integer): Integer;
  begin
    Result := IMax(AClientSize - cImageGapSize, AClientSize div 3 * 2);
  end;

begin
  case Key of
    VK_LEFT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position - cImageLineSize;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssCtrl] then
      begin
        with HorzScrollBar do
          Position := 0;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssAlt] then
      begin
        DoScrollAlt(False);
        Key := 0;
      end;
    end;

    VK_RIGHT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position + cImageLineSize;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssCtrl] then
      begin
        with HorzScrollBar do
          Position := Range;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssAlt] then
      begin
        DoScrollAlt(True);
        Key := 0;
      end;
    end;

    VK_HOME:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position - PageSize(ClientWidth);
        DoScroll;
        Key := 0;
      end;

    VK_END:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position + PageSize(ClientWidth);
        DoScroll;
        Key := 0;
      end;

    VK_UP:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position - cImageLineSize;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssCtrl] then
      begin
        with VertScrollBar do
          Position := 0;
        DoScroll;
        Key := 0;
      end;
    end;

    VK_DOWN:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position + cImageLineSize;
        DoScroll;
        Key := 0;
      end
      else
      if Shift = [ssCtrl] then
      begin
        with VertScrollBar do
          Position := Range;
        DoScroll;
        Key := 0;
      end;
    end;

    VK_PRIOR:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position - PageSize(ClientHeight);
        DoScroll;
        Key := 0;
      end;

    VK_NEXT:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position + PageSize(ClientHeight);
        DoScroll;
        Key := 0;
      end;
  end;
end;


procedure TATImageBox.UpdateImagePosition(AResetPosition: Boolean = False);
var
  AKeepPosition: Boolean;
  AWidth, AHeight,
  ANewWidth, ANewHeight,
  ANewLeft, ANewTop,
  AScrollMaxX, AScrollMaxY: Integer;
  ARatio, AImageRatio,
  ACenterRatioX, ACenterRatioY: Double;
begin
  AKeepPosition := FImageKeepPosition and (not AResetPosition);

  AWidth := ClientWidth;
  AHeight := ClientHeight;

  //Save center position, need to restore it later
  ACenterRatioX := 0;
  ACenterRatioY := 0;

  if FImage.Width > 0 then
  begin
    if FImage.Left >= 0 then
      ACenterRatioX := (AWidth div 2 - FImage.Left) / FImage.Width
    else
      ACenterRatioX := (AWidth div 2 + HorzScrollBar.Position) / FImage.Width;
  end;

  if FImage.Height > 0 then
  begin
    if FImage.Top >= 0 then
      ACenterRatioY := (AHeight div 2 - FImage.Top) / FImage.Height
    else
      ACenterRatioY := (AHeight div 2 + VertScrollBar.Position) / FImage.Height;
  end;

  //Set controls params
  if not AKeepPosition then
  begin
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
  end;

  AutoScroll := not FImageFit;

  FImage.AutoSize := (not FImageFit) and (FImageScale = 100);
  FImage.Stretch := not FImage.AutoSize;

  {
  //Note: commented, because we convert icon to bitmap in UpdateImageInfo.
  //Work around VCL draw bug for icons:
  if FImageIsIcon then
    begin
    FImage.AutoSize := False;
    FImage.Stretch := True;
    FImage.Width := FImageWidth;
    FImage.Height := FImageHeight;
    end;
    }

  //Fit and recalculate ImageScale
  FImage.Left := 0;
  FImage.Top := 0;

  AWidth := ClientWidth;
  AHeight := ClientHeight;

  if FImageFit then
  begin
    {
    //Note: code commented in as it causes wrong scaling sometimes.
    //If image is already fit, don't scale it:
    if (FImage.Width = AWidth) and
      (FImage.Height = AHeight) then
    begin
      ANewWidth := FImage.Width;
      ANewHeight := FImage.Height;
    end
    else
    }
    //Need to scale
    begin
      ANewWidth := FImageWidth;
      ANewHeight := FImageHeight;

      if FImageFitOnlyBig and
        (FImageWidth <= AWidth) and (FImageHeight <= AHeight) then
      begin
        FImageScale := 100;
      end
      else
      begin
        if (AWidth > 0) and (AHeight > 0) and
          (FImageWidth > 0) and (FImageHeight > 0) then
        begin
          ARatio := AWidth / AHeight;
          AImageRatio := FImageWidth / FImageHeight;
          if ARatio >= AImageRatio then
          begin
            ANewHeight := AHeight;
            ANewWidth := Trunc(ANewHeight * AImageRatio);
            FImageScale := AHeight * 100 div FImageHeight;
          end
          else
          begin
            ANewWidth := AWidth;
            ANewHeight := Trunc(ANewWidth / AImageRatio);
            FImageScale := AWidth * 100 div FImageWidth;
          end;
        end;
      end
    end
  end //if FImageFit
  else
  begin
    ANewWidth := Round(FImageWidth * FImageScale / 100);
    ANewHeight := Round(FImageHeight * FImageScale / 100);
  end;

  //Update image position
  ANewLeft := 0;
  ANewTop := 0;

  if FImageCenter then
  begin
    if AWidth > ANewWidth then
      ANewLeft := (AWidth - ANewWidth) div 2;
    if AHeight > ANewHeight then
      ANewTop := (AHeight - ANewHeight) div 2;
  end;

  FImage.SetBounds(
    ANewLeft - HorzScrollBar.Position,
    ANewTop - VertScrollBar.Position,
    ANewWidth,
    ANewHeight);

  //Restore saved center position
  if AKeepPosition then
  begin
    if ANewLeft = 0 then
    begin
      AScrollMaxX := IMax(ANewWidth - AWidth, 0);
      HorzScrollBar.Position :=
        IMin(AScrollMaxX, Trunc(ACenterRatioX * ANewWidth) - AWidth div 2);
    end
    else
      HorzScrollBar.Position := 0;

    if ANewTop = 0 then
    begin
      AScrollMaxY := IMax(ANewHeight - AHeight, 0);
      VertScrollBar.Position :=
        IMin(AScrollMaxY, Trunc(ACenterRatioY * ANewHeight) - AHeight div 2);
    end
    else
      VertScrollBar.Position := 0;
  end;

  DoScroll;
end;

procedure TATImageBox.UpdateImageLabelPosition;
begin
  FImageLabel.Left := 0;
  FImageLabel.Top := 0;
end;

procedure TATImageBox.SetImageFit(AValue: Boolean);
begin
  if AValue <> FImageFit then
  begin
    FImageFit := AValue;
    if not FImageFit then
      FImageScale := 100;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.SetImageFitOnlyBig(AValue: Boolean);
begin
  if AValue <> FImageFitOnlyBig then
  begin
    FImageFitOnlyBig := AValue;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.SetImageCenter(AValue: Boolean);
begin
  if AValue <> FImageCenter then
  begin
    FImageCenter := AValue;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.UpdateImageInfo;
begin
  FImageWidth := 0;
  FImageHeight := 0;
  FImageScale := 100;

  if Assigned(FImage.Picture) and Assigned(FImage.Picture.Graphic) then
  begin
    if FImage.Picture.Graphic is TIcon then
    begin
      FImage.Transparent := False; //Icons are converted to bitmap in FixImageFormat,
                                   //so we must clear the Transparent property,
                                   //otherwise bitmap will look incorrectly a little
      FixIcon(FImage.Picture.Graphic as TIcon);
      FixImageFormat(FImage, Color);
    end;

    FImageWidth := FImage.Picture.Width;
    FImageHeight := FImage.Picture.Height;

    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.Resize;
begin
  inherited;
  UpdateImagePosition;
end;

procedure TATImageBox.SetImageScale(AValue: Integer);
begin
  Assert(
    (AValue >= 0) and (AValue < MaxShort),
    'Invalid scale value');

  if FImageScale <> AValue then
  begin
    FImageScale := AValue;
    FImageFit := False;
    UpdateImagePosition;
    DoOptionsChange;
  end;
end;

procedure TATImageBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FFocusable then
    SetFocus;
end;

procedure TATImageBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFocusable then
    SetFocus;

  if (Button = mbLeft) then
  begin
    FImageMouseDown := True;
    if FImageDrag then
    begin
      FImageDragging := True;
      FImageDraggingPoint := Point(X, Y);
      Screen.Cursor := FImageDragCursor;
    end;
  end;
end;

procedure TATImageBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FImageMouseDown := False;
    FImageDragging := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TATImageBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FImageDrag and FImageDragging then
  begin
    HorzScrollBar.Position := HorzScrollBar.Position + (FImageDraggingPoint.X - X);
    VertScrollBar.Position := VertScrollBar.Position + (FImageDraggingPoint.Y - Y);
    DoScroll;
  end;
end;

procedure TATImageBox.IncreaseImageScale(AIncrement: Boolean);
var
  i: Integer;
begin
  if AIncrement then
  begin
    for i := Low(cViewerImageScales) to High(cViewerImageScales) do
      if cViewerImageScales[i] > ImageScale then
      begin
        ImageScale := cViewerImageScales[i];
        Break
      end;
  end
  else
  begin
    for i := High(cViewerImageScales) downto Low(cViewerImageScales) do
      if cViewerImageScales[i] < ImageScale then
      begin
        ImageScale := cViewerImageScales[i];
        Break
      end;
  end;
end;

procedure TATImageBox.DoOptionsChange;
begin
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange(Self);
end;

procedure TATImageBox.ImageProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  Update;
  Invalidate;
end;

type
  TLabelCracker = class(TLabel);

procedure TATImageBox.ImagePaint(Sender: TObject);
begin
  //Debug:
  //MessageBeep(MB_OK);
  //Need to repaint the label since it's overdrawn by resampled image:
  if FImageLabel.Visible then
    TLabelCracker(FImageLabel).Paint;
end;


{ Registration }

procedure Register;
begin
  RegisterComponents('Samples', [TATImageBox]);
end;

end.
