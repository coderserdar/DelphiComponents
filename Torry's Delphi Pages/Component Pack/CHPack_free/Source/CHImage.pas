unit CHImage;

{ ##############################################################################
  TCHImage

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  0.8.0 - 21.07.2002    - First Release
  1.0.0 - 28.12.2002    - BUG: Bitmap "center" work correct now
                        - NEW: many nice "ImageEffects"
                        - CHANGE: Remove "Fade" properties (see now "ImageEffect")
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Controls, Graphics;

type
  TFrameStyle = (fsSolid, fsDot);
  TBitmapMode = (bmNormal, bmStretch, bmTile, bmCenter);
  TTransMode = (tmAuto, tmCustom);
  TAngle = 0..360;
  TImageEffect = (ieDarkness, ieLightness, ieContrast, ieBlur, ieSaturation, iePixel,
    ieSolorize, iePosterize, ieFlashLight);

  TCHCustomImage = class;

  TCHFrame = class(TPersistent)
  private
    FOwner : TCHCustomImage;
    FFrameActive : Boolean;
    FFrameColor : TColor;
    FFrameStyle : TFrameStyle;
    FFrameWidth :  Integer;

    procedure SetFrameActive(const Value : Boolean);
    procedure SetFrameColor(const Value : TColor);
    procedure SetFrameStyle(const Value : TFrameStyle);
    procedure SetFrameWidth(const Value : Integer);
  protected

  public
    constructor Create(AOwner : TCHCustomImage); virtual;
  published
    property Active : Boolean read FFrameActive write SetFrameActive;
    property Color : TColor read FFrameColor write SetFrameColor;
    property Style : TFrameStyle read FFrameStyle write SetFrameStyle;
    property Width : Integer read FFrameWidth write SetFrameWidth;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHPicture = class(TPersistent)
  private
    FOwner : TCHCustomImage;
    FPicture : TPicture;
    FGraphicMode : TBitmapMode;
    FTransColor: TColor;
    FTransMode: TTransMode;
    FPixelFormat: TPixelFormat;

    procedure SetPicture(const Value : TPicture);
    procedure SetGraphicMode(const Value : TBitmapMode);
    procedure SetTransColor(const Value: TColor);
    procedure SetTransMode(const Value: TTransMode);
    procedure SetPixelFormat(const Value: TPixelFormat);
  protected

  public
    constructor Create(AOwner : TCHCustomImage); virtual;
  published
    property Format : TPixelFormat read FPixelFormat Write SetPixelFormat;
    property Graphic : TPicture read FPicture write SetPicture;
    property Mode : TBitmapMode read FGraphicMode write SetGraphicMode;
    property TransparentColor : TColor read FTransColor Write SetTransColor;
    property TransparentMode : TTransMode read FTransMode Write SetTransMode;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHRotation = class(TPersistent)
  private
    FOwner : TCHCustomImage;
    FAngle: TAngle;
    FBackcolor: TColor;
    FxAxis: Word;
    FyAxis: Word;
    procedure SetAngle(const Value: TAngle);
    procedure SetBackcolor(const Value: TColor);
    procedure SetxAxis(const Value: Word);
    procedure SetyAxis(const Value: Word);
  protected

  public
    constructor Create(AOwner : TCHCustomImage); virtual;
  published
    property Angle : TAngle read FAngle Write SetAngle;
    property xAxis : Word read FxAxis Write SetxAxis;
    property yAxis : Word read FyAxis Write SetyAxis;
    property Backcolor : TColor read FBackcolor Write SetBackcolor;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHImageEffect = class(TPersistent)
  private
    FOwner : TCHCustomImage;
    FEffectActive: Boolean;
    FValue: Byte;
    FImageEffect: TImageEffect;
    procedure SetEffectActive(const Value: Boolean);
    procedure SetImageEffect(const Value: TImageEffect);
    procedure SetValue(const Value: Byte);

  protected

  public
    constructor Create(AOwner : TCHCustomImage); virtual;
  published
    property Active : Boolean read FEffectActive Write SetEffectActive;
    property Effectname : TImageEffect read FImageEffect Write SetImageEffect;
    property Value : Byte read FValue Write SetValue;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHCustomImage = class(TGraphicControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FPic : TCHPicture;
    FFrame : TCHFrame;
    FImageEffect : TCHImageEffect;
    FRotation : TCHRotation;

    FOnProgress: TProgressEvent;
    FShowDesignFrame : Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    FAutosize : Boolean;
    FImageRect : TRect;
    FClientRect : TRect;
    FDrawBitmap : TBitmap;
    FRotateBmp : TBitmap;
    FOriginalBmp : TBitmap;
    FOriginalGraphic : TBitmap;
    FOnEffectEnd: TNotifyEvent;
    FOnEffectStart: TNotifyEvent;

    procedure DrawFrame;
    procedure DrawFrameDesignTime;
    procedure GetImageRect;
    procedure DoAutosize(const Value: Boolean);
    procedure RotateBitmap(var OriginalBmp, RotatedBmp : TBitmap; Angle : Double;
      xAxis, yAxis : Integer; BackColor : TColor);
    procedure SetGraphicFormat;
    procedure DoEffect;
    procedure RestoreImage;
    function IntToByte(const Value : Integer) : Byte;
    function GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetTransparent(const Value: Boolean);
    procedure SetProportional(Value: Boolean);

    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure SetShowDesignFrame(const Value: Boolean);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Canvas: TCanvas read GetCanvas;

    procedure DoDarkness(const Value : Byte);
    procedure DoLightness(const Value : Byte);
    procedure DoContrast(const Value : Byte);
    procedure DoBlur(const Value : Byte);
    procedure DoSaturation(const Value : Byte);
    procedure DoPixel(const Value : Byte);
    procedure DoFlashLight(const Value : Byte);
    procedure DoSolorize(const Value : Byte);
    procedure DoPosterize(const Value : Byte);
  published
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnEffectStart : TNotifyEvent read FOnEffectStart write FOnEffectStart;
    property OnEffectEnd : TNotifyEvent read FOnEffectEnd write FOnEffectEnd;

    property Autosize : Boolean read FAutosize Write DoAutosize;
    property Effect : TCHImageEffect read FImageEffect Write FImageEffect;
    property Frame : TCHFrame read FFrame Write FFrame;
    property Picture : TCHPicture read FPic Write FPic;
    property Rotation : TCHRotation read FRotation Write FRotation;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay;
    property Proportional: Boolean read FProportional write SetProportional;
    property ShowDesignFrame : Boolean read FShowDesignFrame write SetShowDesignFrame;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHImage = class(TCHCustomImage)
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses consts;

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHImage]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPic := TCHPicture.Create(Self);
  FFrame := TCHFrame.Create(Self);
  FRotation := TCHRotation.Create(Self);
  FImageEffect := TCHImageEffect.Create(self);

  FDrawBitmap := TBitmap.Create;
  FOriginalGraphic := TBitmap.Create;
  // set graphic format (default 24bit)
  SetGraphicFormat;

  Height := 105;
  Width := 105;
  FShowDesignFrame := True;
  ControlStyle := ControlStyle + [csReplicatable];

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomImage.Destroy;
begin
  FDrawBitmap.Free;
  FPic.Free;
  FFrame.Free;
  FRotation.Free;
  FImageEffect.Free;
  FOriginalGraphic.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) and not (FPic.FPicture.Graphic = nil) then
  begin
    // save original graphic
    with FOriginalGraphic do
    begin
      Height := Self.Height;
      Width := Self.Width;
      Assign(Self.Picture.Graphic);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPic.FPicture.Graphic <> nil then
	  Result := FPic.FPicture.Graphic.Palette;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomImage.DestRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  w := FPic.FPicture.Width;
  h := FPic.FPicture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
  if (FPic.Mode = bmStretch) or (Proportional and ((w > cw) or (h > ch))) then
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

  if FPic.Mode = bmCenter then
	  OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.Paint;
var
  Save: Boolean;
  X, Y, nFrameW : Integer;
begin
  if FFrame.Active then
  begin
    X := FFrame.FFrameWidth;
    Y := FFrame.FFrameWidth;
  end
  else
  begin
    X := 0;
    Y := 0;
  end;

  // Image
  GetImageRect;

  // draw border at DesigneTime
  if (csDesigning in ComponentState) then
    DrawFrameDesignTime;

  // set Image
  if not (FPic.FPicture.Graphic = nil) then
  begin
    FDrawBitmap.Assign(FPic.FPicture.Graphic);

    // Rotation
    if FRotation.Angle > 0 then
    begin
      try
        FOriginalBmp := TBitmap.Create;
        FRotateBmp := TBitmap.Create;

        FOriginalBmp.Assign(FDrawBitmap);
        RotateBitmap(FOriginalBmp, FRotateBmp, FRotation.FAngle,
          FRotation.FxAxis, FRotation.FyAxis, FRotation.FBackcolor);
        FDrawBitmap.Assign(FRotateBmp);
      finally
        FRotateBmp.Free;
        FOriginalBmp.Free;
      end;
    end;

    // Transparent
    with FDrawBitmap do
    begin
      if FTransparent then
      begin
        if FPic.FTransMode = tmCustom then
          TransparentColor := FPic.FTransColor
        else
          TransparentColor := FDrawBitmap.Canvas.Pixels[0, FDrawBitmap.Height - 1];
        Transparent := True;
      end
      else
        Transparent := False;
    end;

    Save := FDrawing;
    FDrawing := True;
    with inherited Canvas do
    begin
      // Normal
      if FPic.Mode = bmNormal then
        Draw(X, Y, FDrawBitmap)
      // Stretch
      else if FPic.Mode = bmStretch then
      begin
        StretchDraw(FImageRect, FDrawBitmap);
      end
      // Tile
      else if FPic.Mode = bmTile then
      begin
        Brush.Bitmap := FDrawBitmap;
        FillRect(FImageRect);
      end
      // Center
      else if FPic.Mode = bmCenter then
      begin
        if FFrame.Active then
          nFrameW := FFrame.FFrameWidth
        else
          nFrameW := 0;

        X := ((FImageRect.Right - FImageRect.Left) div 2) - (FDrawBitmap.Width div 2) + nFrameW;
        Y := ((FImageRect.Bottom - FImageRect.Top) div 2) - (FDrawBitmap.Height div 2) + nFrameW;
        Draw(X, Y, FDrawBitmap)
      end
    end;
    FDrawing := Save;
  end;

  // Frame
  DrawFrame;


end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := FPic.FPicture.Graphic;
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

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
	if DoPaletteChange then
    Update
	else
    Paint;
  end;
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if FPic.FPicture.Graphic = nil then
  begin
	Bitmap := TBitmap.Create;
	try
	  Bitmap.Width := Width;
	  Bitmap.Height := Height;
	  FPic.FPicture.Graphic := Bitmap;
	finally
	  Bitmap.Free;
	end;
  end;
  if FPic.FPicture.Graphic is TBitmap then
	  Result := TBitmap(FPic.FPicture.Graphic).Canvas
  else
	raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
	  FTransparent := Value;
	  PictureChanged(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
	  FProportional := Value;
	  PictureChanged(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
begin
  if AutoSize and (FPic.FPicture.Width > 0) and (FPic.FPicture.Height > 0) then
	  SetBounds(Left, Top, FPic.FPicture.Width, FPic.FPicture.Height);
  G := FPic.FPicture.Graphic;
  if G <> nil then
  begin
	if not ((G is TMetaFile) or (G is TIcon)) then
	  G.Transparent := FTransparent;
	if (not G.Transparent) and (FPic.Mode = bmStretch) and not Proportional then
	  ControlStyle := ControlStyle + [csOpaque]
	else  // picture might not cover entire clientrect
	  ControlStyle := ControlStyle - [csOpaque];
	if DoPaletteChange and FDrawing then
    Update;
  end
  else
    ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then
    Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (FPic.FPicture.Width > 0) and
    (FPic.FPicture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := FPic.FPicture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := FPic.FPicture.Height;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.GetImageRect;
begin
  FClientRect := GetClientRect;
  if FFrame.Active then
  begin
    try
      FImageRect.Left := FClientRect.Left + FFrame.FFrameWidth;
      FImageRect.Top :=  FClientRect.Top + Frame.FFrameWidth;
      FImageRect.Right := FClientRect.Right - FFrame.FFrameWidth;
      FImageRect.Bottom := FClientRect.Bottom - FFrame.FFrameWidth;
    except
      FImageRect := ClientRect;
    end;
  end
  else
  begin
    FImageRect := GetClientRect;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.RotateBitmap(var OriginalBmp, RotatedBmp : TBitmap; Angle : Double;
  xAxis, yAxis : Integer; BackColor : TColor);
const
    MaxPixelCount = 32768;
    Black : RGBTriple = (rgbtBlue:0; rgbtGreen:0; rgbtRed:0);

type
  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple;

var
  BW, BH, i, j, x, y : Word;
  cosTheta, sinTheta, Theta, jPrimeSinTheta, jPrimeCosTheta :  DOUBLE;
  iOriginal, iPrime, iPrimeRotated, jOriginal, jPrime, jPrimeRotated :  INTEGER;
  ScanlineBytes, iRot, jRot : Integer;
  RowOriginal, RowRotated : pRGBArray;
  POriginalStart, POriginal : Pointer;
  TmpBmp : TBitmap;
  BRect : TRect;
begin
  BRect := Bounds(0, 0, Self.Width, Self.Height);

  TmpBmp := TBitmap.Create;
  TmpBmp.Width := Self.Width;
  TmpBmp.Height := Self.Height;

  TmpBmp.Canvas.Brush.Color := BackColor;
  TmpBmp.Canvas.Brush.Style := bsSolid;
  TmpBmp.Canvas.FillRect(BRect);
  X := ((FImageRect.Right - FImageRect.Left) div 2) - (FDrawBitmap.Width div 2) + FFrame.FFrameWidth;
  Y := ((FImageRect.Bottom - FImageRect.Top) div 2) - (FDrawBitmap.Height div 2) + FFrame.FFrameWidth;
  TmpBmp.Canvas.Draw(X, Y, OriginalBmp);

  OriginalBmp.Assign(TmpBmp);
  TmpBmp.Free;

  if OriginalBmp.PixelFormat <> pf24bit then
    OriginalBmp.PixelFormat := pf24bit;

  RotatedBmp.Width  := Self.Width;
  RotatedBmp.Height := Self.Height;
  RotatedBmp.Canvas.Brush.Color := BackColor;
  RotatedBmp.Canvas.Brush.Style := bsSolid;
  RotatedBmp.Canvas.FillRect(BRect);

  //xAxis := Self.Width div 2;
  //yAxis := Self.Height div 2;

  RotatedBmp.PixelFormat := OriginalBmp.PixelFormat;

  Theta := -Angle * PI / 180;
  sinTheta := SIN(Theta);
  cosTheta := COS(Theta);

  ScanlineBytes := Integer(OriginalBmp.Scanline[1]) - Integer(OriginalBmp.Scanline[0]);

  BW := OriginalBmp.Width  - 1;
  BH := OriginalBmp.Height - 1;
  iRot := (2 * xAxis) + 1;
  jRot := (2 * YAxis) + 1;

  RowRotated := RotatedBmp.ScanLine[BH];
  POriginalStart := OriginalBmp.ScanLine[0];

  for j := BH downto 0 do
  begin

    jPrime := (2 * j) - jRot;
    jPrimeSinTheta := jPrime * sinTheta;
    jPrimeCosTheta := jPrime * cosTheta;
    POriginal := POriginalStart;
    for i := BW downto 0 do
    begin

      iPrime := (2 * i) - iRot;
      iPrimeRotated := ROUND(iPrime * cosTheta - jPrimeSinTheta);
      iOriginal := (iPrimeRotated - 1) div 2 + xAxis;
      if (iOriginal >= 0) and (iOriginal <= BW) then
      begin
        jPrimeRotated := ROUND(iPrime * sinTheta + jPrimeCosTheta);
        jOriginal := (jPrimeRotated - 1) div 2 + yAxis;
        if (jOriginal >= 0) and (jOriginal <= BH) then
        begin
          RowOriginal   := Pointer(Integer(POriginal) + (jOriginal * ScanLineBytes));
          RowRotated[i] := RowOriginal[iOriginal];
        end
        else
//          RowRotated[i].rgbtBlue  := GetBValue(BackColor);
//          RowRotated[i].rgbtGreen := GetGValue(BackColor);
//          RowRotated[i].rgbtRed   := GetRValue(BackColor);

          //RowRotated[i] := Black;
      end
      else
//        RowRotated[i].rgbtBlue  := GetBValue(BackColor);
//        RowRotated[i].rgbtGreen := GetGValue(BackColor);
//        RowRotated[i].rgbtRed   := GetRValue(BackColor);

        //RowRotated[i] := Black;
    end;
    Dec(Integer(RowRotated), ScanLineBytes);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.DrawFrameDesignTime;
var
  X, Y, PixStep : Integer;
begin
  PixStep := 2;
  if FShowDesignFrame then
  begin
    with inherited Canvas do
    begin
      // top
      X := 0;
      while X < Self.Width -1 do
      begin
        Pixels[X, 0] := FFrame.Color;
        Inc(X, PixStep);
      end;
      // bottom
      X := 0;
      while X < Self.Width -1 do
      begin
        Pixels[X, (Self.Height -1)] := FFrame.Color;
        Inc(X, PixStep);
      end;
      // left
      Y := 0;
      while Y < Self.Height -1 do
      begin
        Pixels[0, Y] := FFrame.Color;
        Inc(Y, PixStep);
      end;
      // right
      Y := 0;
      while Y < Self.Height -1 do
      begin
        Pixels[(Self.Width -1), Y] := FFrame.Color;
        Inc(Y, PixStep);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.DrawFrame;
var
  X, Y, PixStep, nWidth : Integer;
begin
  PixStep := 1;
  if FFrame.Active then
  begin
    if FFrame.Style = fsDot then
      PixStep := 2
    else if FFrame.Style = fsSolid then
      PixStep := 1;

    with inherited Canvas do
    begin
      for nWidth := 0 to FFrame.Width - 1 do
      begin
        // top
        X := 0;
        while X < Self.Width -1 do
        begin
          Pixels[X, nWidth] := FFrame.Color;
          Inc(X, PixStep);
        end;
        // bottom
        X := 0;
        while X < Self.Width -1 do
        begin
          Pixels[X, (Self.Height -1) - nWidth] := FFrame.Color;
          Inc(X, PixStep);
        end;
        // left
        Y := 0;
        while Y < Self.Height -1 do
        begin
          Pixels[nWidth, Y] := FFrame.Color;
          Inc(Y, PixStep);
        end;
        // right
        Y := 0;
        while Y < Self.Height -1 do
        begin
          Pixels[(Self.Width -1) - nWidth, Y] := FFrame.Color;
          Inc(Y, PixStep);
        end;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.SetShowDesignFrame(const Value: Boolean);
begin
  if FShowDesignFrame <> Value then
  begin
    FShowDesignFrame := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.DoAutosize(const Value: Boolean);
begin
  FAutosize := Value;
  if FAutosize and (FPic.FPicture.Width > 0) and (FPic.FPicture.Height > 0) then
  begin
    SetBounds(Left, Top, FPic.FPicture.Width, FPic.FPicture.Height);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.Resize;
begin
  inherited Resize;
  FRotation.FxAxis := Width div 2;
  FRotation.FyAxis := Height div 2;
  DoAutosize(FAutosize);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.RestoreImage;
begin
  FPic.FPicture.Bitmap.Assign(FOriginalGraphic);
  FDrawBitmap.Assign(FOriginalGraphic);
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.SetGraphicFormat;
begin
  if Assigned(FDrawBitmap) then
    FDrawBitmap.PixelFormat := FPic.FPixelFormat;
  if Assigned(FOriginalGraphic) then
    FOriginalGraphic.PixelFormat := FPic.FPixelFormat;
  if Assigned(FOriginalBmp) then
    FOriginalBmp.PixelFormat := FPic.FPixelFormat;
  if Assigned(FRotateBmp) then
    FRotateBmp.PixelFormat := FPic.FPixelFormat;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Darkness }
procedure TCHCustomImage.DoDarkness(const Value: Byte);
var
  WorkBmp : TBitmap;
  RGBTriple : ^TRGBTriple;
  WorkArray : array[0..255]of Byte;
  I, X, Y : Integer;
Begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);

      for I := 0 to 255 do
        if Integer(I - Value) < 0 then
          WorkArray[I] := 0
        else
          WorkArray[I] := I - Value;

      for Y := 0 to WorkBmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          RGBTriple.rgbtBlue := WorkArray[RGBTriple.rgbtBlue];
          RGBTriple.rgbtRed := WorkArray[RGBTriple.rgbtRed];
          RGBTriple.rgbtGreen := WorkArray[RGBTriple.rgbtGreen];
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Lightness }
procedure TCHCustomImage.DoLightness(const Value: Byte);
var
  WorkBmp : TBitmap;
  RGBTriple : ^TRGBTriple;
  WorkArray : array[0..255]of Byte;
  I, X, Y : Integer;
Begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);

      for I := 0 to 255 do
        if Integer(I + Value) > 255 then
          WorkArray[I] := 255
        else
          WorkArray[I] := I + Value;

      for Y := 0 to WorkBmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          RGBTriple.rgbtBlue := WorkArray[RGBTriple.rgbtBlue];
          RGBTriple.rgbtRed := WorkArray[RGBTriple.rgbtRed];
          RGBTriple.rgbtGreen := WorkArray[RGBTriple.rgbtGreen];
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Contrast }
procedure TCHCustomImage.DoContrast(const Value: Byte);
var
  WorkBmp : TBitmap;
  RGBTriple : ^TRGBTriple;
  WorkArray : array[0..255]of Byte;
  I, X, Y, Z : Integer;
  Factor : Double;
Begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);

      Factor := (Value / 100) + 1;

      for I := 0 to 255 do
      begin
        Z := Round((Integer(I) - 128) * Factor) + 128;
        if Z > 255 then
          WorkArray[I] := 255
        else if Z < 0 then
          WorkArray[I] := 0
        else
          WorkArray[I] := Z;
      end;

      for Y := 0 to WorkBmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          RGBTriple.rgbtBlue := WorkArray[RGBTriple.rgbtBlue];
          RGBTriple.rgbtRed := WorkArray[RGBTriple.rgbtRed];
          RGBTriple.rgbtGreen := WorkArray[RGBTriple.rgbtGreen];
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Blur }
procedure TCHCustomImage.DoBlur(const Value: Byte);
var
  WorkBmp : TBitmap;
  pbarray, pbarray2, pbarray3 : PByteArray;
  X, Y, Z: Integer;
  tmpValue : Byte;
  StoreArray : array[0..3,0..2] of Byte;
Begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);

    tmpValue := Value;
    if tmpValue >= Height then
      tmpValue := Height -1;

    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);

      for Y := 0 to WorkBmp.Height -1 do
      begin
        pbarray := WorkBmp.ScanLine[Y];
        if (Y - tmpValue < 0) then
          pbarray2 := WorkBmp.ScanLine[Y]
        else
          pbarray2 := WorkBmp.ScanLine[Y - tmpValue];

        if (Y + tmpValue < WorkBmp.Height) then
          pbarray3 := Workbmp.ScanLine[Y + tmpValue]
        else
          pbarray3 := WorkBmp.ScanLine[WorkBmp.Height - Y];

        for X := 0 to WorkBmp.Width -1 do
        begin
          if X - tmpValue < 0 then
            Z := X
          else
            Z := X - tmpValue;
          StoreArray[0,0] := pbarray2[Z * 3];
          StoreArray[0,1] := pbarray2[Z * 3 +1];
          StoreArray[0,2] := pbarray2[Z * 3 +2];
          StoreArray[1,0] := pbarray3[Z * 3];
          StoreArray[1,1] := pbarray3[Z * 3 +1];
          StoreArray[1,2] := pbarray3[Z * 3 +2];

          if X + tmpValue < WorkBmp.Width then
            Z := X + tmpValue
          else
            Z := Workbmp.Width - X;
          StoreArray[2,0] := pbarray2[Z * 3];
          StoreArray[2,1] := pbarray2[Z * 3 +1];
          StoreArray[2,2] := pbarray2[Z * 3 +2];
          StoreArray[3,0] := pbarray3[Z * 3];
          StoreArray[3,1] := pbarray3[Z * 3 +1];
          StoreArray[3,2] := pbarray3[Z * 3 +2];
          pbarray[X * 3]    := (StoreArray[0,0] + StoreArray[1,0] + StoreArray[2,0] + StoreArray[3,0]) shr 2;
          pbarray[X * 3 +1] := (StoreArray[0,1] + StoreArray[1,1] + StoreArray[2,1] + StoreArray[3,1]) shr 2;
          pbarray[X * 3 +2] := (StoreArray[0,2] + StoreArray[1,2] + StoreArray[2,2] + StoreArray[3,2]) shr 2;
        end;
      end;

      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Flashlight }
procedure TCHCustomImage.DoFlashLight(const Value: Byte);
var
  WorkBmp : TBitmap;
  X ,Y, Count : Integer;
  tmpValue : Byte;
  RGBTriple : ^TRGBTriple;
begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);

    tmpValue := Value;
    if tmpValue > 10 then
        tmpValue := 10;
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);
      for Count := 1 to tmpValue do
      begin
        for Y := 0 to WorkBmp.Height -1 do
        begin
          RGBTriple := WorkBmp.ScanLine[Y];
          for X := 0 to Workbmp.Width -1 do
          begin
            RGBTriple.rgbtRed := IntToByte(Trunc(Sin((RGBTriple.rgbtRed) / 255 * pi / 2) * 255));
            RGBTriple.rgbtGreen := IntToByte(Trunc(Sin((RGBTriple.rgbtGreen) / 255 * pi / 2) * 255));
            RGBTriple.rgbtBlue := IntToByte(Trunc(Sin((RGBTriple.rgbtBlue) / 255 * pi / 2) * 255));
            Inc(RGBTriple);
          end;
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Pixeling }
procedure TCHCustomImage.DoPixel(const Value: Byte);
var
  WorkBmp : TBitmap;
  X ,Y, I, J, R, G, B : Integer;
  pbarray, pbarray2 : PByteArray;
begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);
      Y := 0;
      repeat
        pbarray := WorkBmp.ScanLine[Y];
        repeat
          J := 1;
          repeat
            pbarray2 := WorkBmp.ScanLine[Y];
            X := 0;
            repeat
              R := pbarray[X * 3];
              G := pbarray[X * 3 +1];
              B := pbarray[X * 3 +2];
              I := 1;
              repeat
                pbarray2[X * 3] := R;
                pbarray2[X * 3 +1] := G;
                pbarray2[X * 3 +2] := B;
                Inc(X);
                Inc(I);
              until (X >= WorkBmp.Width) or (I > Value);
            until (X >= WorkBmp.Width);
            Inc(J);
            Inc(Y);
          until (Y >= WorkBmp.Height) or (J > Value);
        until (Y >= WorkBmp.Height) or (X >= WorkBmp.Width);
      until (Y >= WorkBmp.Height);
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Posterize }
procedure TCHCustomImage.DoPosterize(const Value: Byte);
var
  WorkBmp : TBitmap;
  X ,Y : Integer;
  tmpValue : Byte;
  RGBTriple : ^TRGBTriple;
begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);

    // for not allowed Floating decimal division
    tmpValue := Value;
    if tmpValue = 0 then
      tmpValue := 1;

    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);
      for Y := 0 to WorkBmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          RGBTriple.rgbtRed := Round(RGBTriple.rgbtRed / tmpValue) * tmpValue;
          RGBTriple.rgbtGreen := Round(RGBTriple.rgbtGreen / tmpValue) * tmpValue;
          RGBTriple.rgbtBlue := Round(RGBTriple.rgbtBlue / tmpValue) * tmpValue;
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Saturation }
procedure TCHCustomImage.DoSaturation(const Value: Byte);
var
  WorkBmp : TBitmap;
  X ,Y, Z : Integer;
  tmpValue :Byte;
  RGBTriple : ^TRGBTriple;
begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);

    tmpValue := 255 - Value;
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);
      for Y := 0 to Workbmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          Z := (RGBTriple.rgbtRed + RGBTriple.rgbtGreen + RGBTriple.rgbtBlue) div 3;
          RGBTriple.rgbtRed := IntToByte(Z + (((RGBTriple.rgbtRed - Z) * tmpValue) div 255));
          RGBTriple.rgbtGreen := IntToByte(Z + (((RGBTriple.rgbtGreen - Z) * tmpValue) div 255));
          RGBTriple.rgbtBlue := IntToByte(Z + (((RGBTriple.rgbtBlue - Z) * tmpValue) div 255));
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Solorize }
procedure TCHCustomImage.DoSolorize(const Value: Byte);
var
  WorkBmp : TBitmap;
  X ,Y, Z : Integer;
  tmpValue : Byte;
  RGBTriple : ^TRGBTriple;
begin
  if not (FPic.FPicture.Graphic = nil) then
  begin
    if Assigned(FOnEffectStart) then
  	  FOnEffectStart(Self);

    tmpValue := 255 - Value;
    WorkBmp := TBitmap.Create;
    try
      WorkBmp.PixelFormat := pf24bit;
      WorkBmp.Assign(FOriginalGraphic);
      for Y := 0 to WorkBmp.Height -1 do
      begin
        RGBTriple := WorkBmp.ScanLine[Y];
        for X := 0 to WorkBmp.Width -1 do
        begin
          Z := (RGBTriple.rgbtRed + RGBTriple.rgbtGreen + RGBTriple.rgbtBlue) div 3;
          if Z > tmpValue then
          begin
            RGBTriple.rgbtRed := 255 - RGBTriple.rgbtRed;
            RGBTriple.rgbtGreen := 255 - RGBTriple.rgbtGreen;
            RGBTriple.rgbtBlue := 255 - RGBTriple.rgbtBlue;
          end;
          Inc(RGBTriple);
        end;
      end;
      FPic.FPicture.Bitmap.Assign(WorkBmp);
    finally
      WorkBmp.Free;
    end;

    if Assigned(FOnEffectEnd) then
  	  FOnEffectEnd(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ---  CHFrame  ---}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFrame.Create(AOwner: TCHCustomImage);
begin
  inherited Create;
  FOwner := AOwner;
  FFrameActive := False;
  FFrameColor := clBlack;
  FFrameWidth := 1;
  FFrameStyle := fsSolid;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFrame.SetFrameActive(const Value: Boolean);
begin
  if FFrameActive <> Value then
  begin
    FFrameActive := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFrame.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFrame.SetFrameStyle(const Value: TFrameStyle);
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFrame.SetFrameWidth(const Value: Integer);
begin
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    FOwner.Invalidate;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ---  CHPicture  ---}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHPicture.Create(AOwner: TCHCustomImage);
begin
  inherited Create;
  FOwner := AOwner;
  FPicture := TPicture.Create;
  FPicture.OnChange := FOwner.PictureChanged;
  FPicture.OnProgress := FOwner.Progress;
  FGraphicMode := bmNormal;
  FPixelFormat := pf24bit;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPicture.SetPicture(const Value: TPicture);
begin
  // save original graphic
  with FOwner.FOriginalGraphic do
  begin
    Height := Value.Height;
    Width := Value.Width;
    Assign(Value.Graphic);
  end;

  // set graphic for canvas
  FPicture.Assign(Value);
  FOwner.FRotation.FxAxis := FOwner.Width div 2;
  FOwner.FRotation.FyAxis := FOwner.Height div 2;
  FOwner.DoEffect;
  FOwner.Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPicture.SetGraphicMode(const Value: TBitmapMode);
begin
  if FGraphicMode <> Value then
  begin
    FGraphicMode := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPicture.SetTransColor(const Value: TColor);
begin
  if FTransColor <> Value then
  begin
    FTransColor := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPicture.SetTransMode(const Value: TTransMode);
begin
  if FTransMode <> Value then
  begin
    FTransMode := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPicture.SetPixelFormat(const Value: TPixelFormat);
begin
  if FPixelFormat <> Value then
  begin
    FPixelFormat := Value;
    FOwner.SetGraphicFormat;
  end;
end;




{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ---  CHRotation  ---}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHRotation.Create(AOwner: TCHCustomImage);
begin
  inherited Create;
  FOwner := AOwner;
  FAngle := 0;
  FBackcolor := FOwner.Color;
  FxAxis := FOwner.Width div 2;
  FyAxis := FOwner.Height div 2;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRotation.SetAngle(const Value: TAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRotation.SetBackcolor(const Value: TColor);
begin
  if FBackcolor <> Value then
  begin
    FBackcolor := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRotation.SetxAxis(const Value: Word);
begin
  if FxAxis <> Value then
  begin
    FxAxis := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHRotation.SetyAxis(const Value: Word);
begin
  if FyAxis <> Value then
  begin
    FyAxis := Value;
    FOwner.Invalidate;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHImageEffect }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHImageEffect.Create(AOwner: TCHCustomImage);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomImage.DoEffect;
begin
  if FImageEffect.FEffectActive then
  begin
    if FImageEffect.Effectname = ieDarkness then
      DoDarkness(FImageEffect.Value)
    else if FImageEffect.Effectname = ieLightness then
      DoLightness(FImageEffect.Value)
    else if FImageEffect.Effectname = ieContrast then
      DoContrast(FImageEffect.Value)
    else if FImageEffect.Effectname = ieBlur then
      DoBlur(FImageEffect.Value)
    else if FImageEffect.Effectname = ieSolorize then
      DoSolorize(FImageEffect.Value)
    else if FImageEffect.Effectname = iePosterize then
      DoPosterize(FImageEffect.Value)
    else if FImageEffect.Effectname = iePixel then
      DoPixel(FImageEffect.Value)
    else if FImageEffect.Effectname = ieSaturation then
      DoSaturation(FImageEffect.Value)
    else if FImageEffect.Effectname = ieFlashLight then
      DoFlashLight(FImageEffect.Value);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHImageEffect.SetEffectActive(const Value: Boolean);
begin
  if FEffectActive <> Value then
  begin
    FEffectActive := Value;
    if FEffectActive then
      FOwner.DoEffect
    else
      FOwner.RestoreImage;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHImageEffect.SetImageEffect(const Value: TImageEffect);
begin
  if FImageEffect <> Value then
  begin
    FImageEffect := Value;
    if FEffectActive then
      FOwner.DoEffect
    else
      FOwner.RestoreImage;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHImageEffect.SetValue(const Value: Byte);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if FEffectActive then
      FOwner.DoEffect
    else
      FOwner.RestoreImage;
  end;
end;





function TCHCustomImage.IntToByte(const Value: Integer): Byte;
begin
  if Value > 255 then
    Result := 255
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
end;

end.
