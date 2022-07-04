{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

// Modified version of slider.pas
//  - added property "ThumbVisible" to hide/show thumb on ruler.
//  - added property "Transparent"
//  - added property "ViewElapsedImage" to show different images for elapsed range
//                    and non-elapsed range. (applied only for Horizontal slider)

unit slider;

interface

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE RX_D3} { Delphi 3.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}


{$R slider.res}

{$IFDEF WIN32}
uses Windows, Controls, ExtCtrls, Classes, Graphics, Messages;
{$ELSE}
uses WinTypes, WinProcs, Controls, ExtCtrls, Classes, Graphics, Messages;
{$ENDIF}

type
  TNumThumbStates = 1..2;
  TImageArrayRange = 0..4;
  TSliderOrientation = (soHorizontal, soVertical);
  TSliderOption = (soShowFocus, soShowPoints, soSmooth);
  TSliderOptions = set of TSliderOption;
  TSliderImage = (siHThumb, siHRuler, siVThumb, siVRuler, siHRuler2);
  TSliderImages = set of TSliderImage;
  TImageArray = array[TImageArrayRange] of TBitmap;
  TJumpMode = (jmNone, jmHome, jmEnd, jmNext, jmPrior);

  TSliderThumb = class;

{ TSlider }

  TSlider = class(TCustomControl)
  private
    { Images }
    FUserImages: TSliderImages;
    FImages: TImageArray;
    FEdgeSize: Integer;
    { Elements }
    FRuler: TBitmap;
    FRuler2: TBitmap;
    FRulerOrg: TPoint;
    FThumb: TSliderThumb;
    FPointsRect: TRect;
    { Styles }
    FOrientation: TSliderOrientation;
    FOptions: TSliderOptions;
    FThumbVisible : Boolean;        // ** Added
    { Values }
    FMinValue: Longint;
    FMaxValue: Longint;
    FIncrement: Longint;
    FValue: Longint;
    FTransparent: Boolean;          // ** Added
    FViewElapsedImage: Boolean;     // ** Added
    { Internal }
    FHit: Integer;
    FActive: Boolean;
    FSliding: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FMousePos: TPoint;
    FStartJump: TJumpMode;
    { Events }
    FOnChange: TNotifyEvent;
    FOnDrawPoints: TNotifyEvent;
    FOnStartTracking: TNotifyEvent;
    FOnStopTracking : TNotifyEvent;
    { Get/Set properties methods }
    function GetImage(Index: Integer): TBitmap;
    procedure SetImage(Index: Integer; Value: TBitmap);
    procedure SetEdgeSize(Value: Integer);
    function GetNumThumbStates: TNumThumbStates;
    procedure SetNumThumbStates(Value: TNumThumbStates);
    procedure SetOrientation(Value: TSliderOrientation);
    procedure SetOptions(Value: TSliderOptions);
    procedure SetMinValue(Value: Longint);
    procedure SetMaxValue(Value: Longint);
    procedure SetIncrement(Value: Longint);
    function GetThumbOffset: Integer;
    procedure SetThumbOffset(Value: Integer);
    procedure SetValue(Value: Longint);
    procedure SetThumbVisible(Value : Boolean);
    procedure SetTransparent(Value : Boolean);
    procedure SetViewElapsedImage(Value : Boolean);
    { Internal methods }
    procedure ThumbJump(Jump: TJumpMode);
    function JumpTo(X, Y: Integer): TJumpMode;
    procedure StopTracking;
    procedure TimerTrack;
    function StoreImage(Index: Integer): Boolean;
    procedure CreateElements;
    procedure BuildRuler;
    procedure AdjustElements;
    procedure ImageChanged;
    procedure ReadUserImages(Stream: TStream);
    procedure WriteUserImages(Stream: TStream);
    function GetValueByOffset(Offset: Integer): Longint;
    function GetOffsetByValue(Value: Longint): Integer;
    function GetRulerLength: Integer;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    property ThumbOffset: Integer read GetThumbOffset write SetThumbOffset;
  protected
    procedure Change; dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure ThumbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure ThumbMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure ThumbMouseUp(Sender: TObject; Thumb: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DrawParentImage(Control: TControl; Dest: TCanvas);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawPoints(PointsStep, PointsHeight,
      ExtremePointsHeight: Integer); virtual;
    property Canvas;
  published
    property ImageHThumb: TBitmap index 0 read GetImage write SetImage stored StoreImage;
    property ImageHRuler: TBitmap index 1 read GetImage write SetImage  stored StoreImage;
    property ImageVThumb: TBitmap index 2 read GetImage write SetImage stored StoreImage;
    property ImageVRuler: TBitmap index 3 read GetImage write SetImage stored StoreImage;
    property ImageHRuler2: TBitmap index 4 read GetImage write SetImage stored StoreImage;
    property EdgeSize: Integer read FEdgeSize write SetEdgeSize default 2;
    property NumThumbStates: TNumThumbStates read GetNumThumbStates write SetNumThumbStates;
    property Orientation: TSliderOrientation read FOrientation write SetOrientation
      default soHorizontal;
    property Options: TSliderOptions read FOptions write SetOptions;
    property Increment: Longint read FIncrement write SetIncrement;
    property MinValue: Longint read FMinValue write SetMinValue;
    property MaxValue: Longint read FMaxValue write SetMaxValue;
    property Value: Longint read FValue write SetValue;
    property ThumbVisible : Boolean read FThumbVisible write SetThumbVisible;   // ** Added
    property Transparent: Boolean read FTransparent write SetTransparent; // ** Added
    property ViewElapsedImage: Boolean read FViewElapsedImage write SetViewElapsedImage; // ** Added
    property Align;
    property Visible;
    property Enabled;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawPoints: TNotifyEvent read FOnDrawPoints write FOnDrawPoints;
    property OnStartTracking : TNotifyEvent read FOnStartTracking write FOnStartTracking;
    property OnStopTracking : TNotifyEvent read FOnStopTracking write FOnStopTracking;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
  end;

{ TSliderThumb }

  TSliderThumb = class(TCustomControl)
  private
    FBitmap: TBitmap;
    FTransparentColor: TColor;
    FDown: Boolean;
    FNumStates: TNumThumbStates;
    procedure SetBitmap(Value: TBitmap);
    procedure SetTransparentColor(Value: TColor);
    procedure SetDown(Value: Boolean);
    procedure SetNumStates(Value: TNumThumbStates);
    procedure AdjustBounds;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor
      default clOlive;
    property Down: Boolean read FDown write SetDown default False;
    property NumStates: TNumThumbStates read FNumStates write SetNumStates default 2;
  end;

procedure Register;

implementation

uses Forms, SysUtils;

const
{ TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  TransparentMask = $02000000;

const
  crHand     = 14000;

const
  ImagesResNames: array[TImageArrayRange] of PChar =
    ('W95_HTB', 'W95_HRL', 'W95_VTB', 'W95_VRL', 'W95_HRL2');
  CursorResName = 'AD_HAND';
 { Indent = 6; }
  Indent = 0;   // Modified : '99. 3. 4. by Silhwan Hyun
  JumpInterval = 400;

{ TSliderThumb }

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function Max(A, B: Longint): Longint;
begin
  if A > B then Result := A
  else Result := B;
end;

function Min(A, B: Longint): Longint;
begin
  if A < B then Result := A
  else Result := B;
end;

procedure DrawTransparentBitmapRect(DC: HDC; Bitmap, Bitmap2: HBitmap; xStart, yStart,
  Width, Height: Integer; Rect: TRect; TransparentColor: TColorRef;
  ViewElapsedImage : boolean; ThumbLeft : integer);
var
{$IFDEF WIN32}
  BM: Windows.TBitmap;
{$ELSE}
  BM: WinTypes.TBitmap;
{$ENDIF}
  cColor: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmAndMem2, bmSave, bmSave2: HBitmap;
  bmBackOld, bmObjectOld, bmMemOld, bmMemOld2, bmSaveOld, bmSaveOld2: HBitmap;
  hdcMem, hdcMem2, hdcBack, hdcObject, hdcTemp, hdcTemp2, hdcSave, hdcSave2: HDC;
  ptSize, ptRealSize, ptBitSize, ptOrigin: TPoint;
begin
  hdcTemp := CreateCompatibleDC(DC);
  SelectObject(hdcTemp, Bitmap);      { Select the bitmap    }
  hdcTemp2 := CreateCompatibleDC(DC);
  SelectObject(hdcTemp2, Bitmap2);      { Select the bitmap    }

  GetObject(Bitmap, SizeOf(BM), @BM);
  ptRealSize.x := Min(Rect.Right - Rect.Left, BM.bmWidth - Rect.Left);
  ptRealSize.y := Min(Rect.Bottom - Rect.Top, BM.bmHeight - Rect.Top);
  DPtoLP(hdcTemp, ptRealSize, 1);
  ptOrigin.x := Rect.Left;
  ptOrigin.y := Rect.Top;
  DPtoLP(hdcTemp, ptOrigin, 1);       { Convert from device  }
                                      { to logical points    }
  ptBitSize.x := BM.bmWidth;          { Get width of bitmap  }
  ptBitSize.y := BM.bmHeight;         { Get height of bitmap }
  DPtoLP(hdcTemp, ptBitSize, 1);
  if (ptRealSize.x = 0) or (ptRealSize.y = 0) then begin
    ptSize := ptBitSize;
    ptRealSize := ptSize;
  end
  else ptSize := ptRealSize;
  if (Width = 0) or (Height = 0) then begin
    Width := ptSize.x;
    Height := ptSize.y;
  end;

  { Create some DCs to hold temporary data }
  hdcBack   := CreateCompatibleDC(DC);
  hdcObject := CreateCompatibleDC(DC);
  hdcMem    := CreateCompatibleDC(DC);
  if ViewElapsedImage then
     hdcMem2 := CreateCompatibleDC(DC);
  hdcSave   := CreateCompatibleDC(DC);
  hdcSave2  := CreateCompatibleDC(DC);
  { Create a bitmap for each DC. DCs are required for a number of }
  { GDI functions                                                 }
  { Monochrome DC }
  bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndMem    := CreateCompatibleBitmap(DC, Max(ptSize.x, Width), Max(ptSize.y, Height));
  if ViewElapsedImage then
     bmAndMem2 := CreateCompatibleBitmap(DC, Max(ptSize.x, Width), Max(ptSize.y, Height));
  bmSave      := CreateCompatibleBitmap(DC, ptBitSize.x, ptBitSize.y);
  bmSave2     := CreateCompatibleBitmap(DC, ptBitSize.x, ptBitSize.y);

  { Each DC must select a bitmap object to store pixel data }
  bmBackOld   := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld    := SelectObject(hdcMem, bmAndMem);
  if ViewElapsedImage then
     bmMemOld2 := SelectObject(hdcMem2, bmAndMem2);
  bmSaveOld   := SelectObject(hdcSave, bmSave);
  bmSaveOld2  := SelectObject(hdcSave2, bmSave2);

  { Set proper mapping mode }
  SetMapMode(hdcTemp, GetMapMode(DC));

  { Save the bitmap sent here, because it will be overwritten }
  BitBlt(hdcSave, 0, 0, ptBitSize.x, ptBitSize.y, hdcTemp, 0, 0, SRCCOPY);
  if ViewElapsedImage then
    BitBlt(hdcSave2, 0, 0, ptBitSize.x, ptBitSize.y, hdcTemp2, 0, 0, SRCCOPY);

  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  cColor := SetBkColor(hdcTemp, TransparentColor);
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, ptOrigin.x, ptOrigin.y,
    SRCCOPY);
  { Set the background color of the source DC back to the original  }
  { color                                                           }
  SetBkColor(hdcTemp, cColor);
  { Create the inverse of the object mask }
  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
    NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(hdcMem, 0, 0, Width, Height, DC, xStart, yStart,
    SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(hdcMem, 0, 0, Width, Height, hdcObject, 0, 0,
    ptSize.x, ptSize.y, SRCAND);
  {BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);}
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(hdcTemp, ptOrigin.x, ptOrigin.y, ptSize.x, ptSize.y, hdcBack, 0, 0,
    SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(hdcMem, 0, 0, Width, Height, hdcTemp, ptOrigin.x, ptOrigin.y,
    ptSize.x, ptSize.y, SRCPAINT);
  {BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, ptOrigin.x, ptOrigin.y,
    SRCPAINT);}
  { Copy the destination to the screen }
  if ViewElapsedImage then
    BitBlt(hdcMem2, 0, 0,
           Width, Height,
           hdcMem, 0, 0, SRCCOPY)
  else
    BitBlt(DC, xStart, yStart, Max(ptRealSize.x, Width), Max(ptRealSize.y, Height),
           hdcMem, 0, 0, SRCCOPY);
  { Place the original bitmap back into the bitmap sent here }
  BitBlt(hdcTemp, 0, 0, ptBitSize.x, ptBitSize.y, hdcSave, 0, 0, SRCCOPY);

  if ViewElapsedImage then
  begin
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  cColor := SetBkColor(hdcTemp2, TransparentColor);
     { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp2, ptOrigin.x, ptOrigin.y,
    SRCCOPY);
  { Set the background color of the source DC back to the original  }
  { color                                                           }
  SetBkColor(hdcTemp2, cColor);
  { Create the inverse of the object mask }
  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0,
    NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(hdcMem, 0, 0, Width, Height, DC, xStart, yStart,
    SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(hdcMem, 0, 0, Width, Height, hdcObject, 0, 0,
    ptSize.x, ptSize.y, SRCAND);
  {BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);}
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(hdcTemp2, ptOrigin.x, ptOrigin.y, ptSize.x, ptSize.y, hdcBack, 0, 0,
    SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(hdcMem, 0, 0, Width, Height, hdcTemp2, ptOrigin.x, ptOrigin.y,
    ptSize.x, ptSize.y, SRCPAINT);

  BitBlt(hdcMem2, 0, 0, ThumbLeft{Max(ptRealSize.x, Width)} - xStart, Height,
    hdcMem, 0, 0, SRCCOPY);
  { Copy the destination to the screen }
  BitBlt(DC, xStart, yStart, Max(ptRealSize.x, Width), Max(ptRealSize.y, Height),
    hdcMem2, 0, 0, SRCCOPY);

    { Place the original bitmap back into the bitmap sent here }
    BitBlt(hdcTemp2, 0, 0, ptBitSize.x, ptBitSize.y, hdcSave2, 0, 0, SRCCOPY);
  end;

  { Delete the memory bitmaps }
  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  if ViewElapsedImage then
     DeleteObject(SelectObject(hdcMem2, bmMemOld2));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));
  DeleteObject(SelectObject(hdcSave2, bmSaveOld2));
  { Delete the memory DCs }
  DeleteDC(hdcMem);
  if ViewElapsedImage then
     DeleteDC(hdcMem2);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcSave2);
  DeleteDC(hdcTemp);
  DeleteDC(hdcTemp2);
end;

procedure InternalDrawTransBmpRect(Dest: TCanvas; X, Y, W, H: Integer;
  Rect: TRect; Bitmap, Bitmap2: TBitmap; TransparentColor: TColor;
  ViewElapsedImage: boolean; ThumbLeft: integer);
var
  MemImage: TBitmap;
  MemImage2: TBitmap;
  R: TRect;
begin
  MemImage := TBitmap.Create;
  MemImage2 := TBitmap.Create;
  try
    R := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
    if TransparentColor = clNone then begin
      if (WidthOf(Rect) <> 0) and (HeightOf(Rect) <> 0) then R := Rect;
      MemImage.Width := WidthOf(R);
      MemImage.Height := HeightOf(R);
      MemImage.Canvas.CopyRect(Bounds(0, 0, MemImage.Width, MemImage.Height),
        Bitmap.Canvas, R);
      MemImage2.Width := WidthOf(R);
      MemImage2.Height := HeightOf(R);
      MemImage2.Canvas.CopyRect(Bounds(0, 0, MemImage2.Width, MemImage2.Height),
        Bitmap2.Canvas, R);
      if (W = 0) or (H = 0) then
      begin
        Dest.Draw(X, Y, MemImage);
        Dest.Draw(X, Y, MemImage2);
      end else
      begin
        Dest.StretchDraw(Bounds(X, Y, W, H), MemImage);
        Dest.StretchDraw(Bounds(X, Y, W, H), MemImage2);
      end;
    end
    else begin
      MemImage.Width := WidthOf(R);
      MemImage.Height := HeightOf(R);
      MemImage.Canvas.CopyRect(R, Bitmap.Canvas, R);
      MemImage2.Width := WidthOf(R);
      MemImage2.Height := HeightOf(R);
      MemImage2.Canvas.CopyRect(R, Bitmap2.Canvas, R);
{$IFDEF RX_D3}
      if TransparentColor = clDefault then
        TransparentColor := MemImage.Canvas.Pixels[0, MemImage.Height - 1];
{$ENDIF RX_D3}
      DrawTransparentBitmapRect(Dest.Handle, MemImage.Handle, MemImage2.Handle,
                                X, Y, W, H, Rect,
                                ColorToRGB(TransparentColor and not TransparentMask),
                                ViewElapsedImage, ThumbLeft);
      { TBitmap.TransparentColor property return TColor value equal   }
      { to (Bitmap.Canvas.Pixels[0, Height - 1] or TransparentMask).  }
    end;
  finally
    MemImage.Free;
    MemImage2.Free;
  end;
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; XOrigin, YOrigin: Integer;
  Rect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  InternalDrawTransBmpRect(Dest, XOrigin, YOrigin, 0, 0, Rect, Bitmap, Bitmap,
    TransparentColor, false, 0);
end;

procedure DrawBitmapTransparent(Dest: TCanvas; XOrigin, YOrigin: Integer;
  Bitmap, Bitmap2: TBitmap; TransparentColor: TColor;
  ViewElapsedImage: boolean; ThumbLeft: integer);
begin
  InternalDrawTransBmpRect(Dest, XOrigin, YOrigin, 0, 0, Rect(0, 0, 0, 0),
    Bitmap, Bitmap2, TransparentColor, ViewElapsedImage, ThumbLeft);
end;

constructor TSliderThumb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse];
  FBitmap := TBitmap.Create;
  FTransparentColor := clOlive;
  FDown := False;
  FNumStates := 2;
end;

destructor TSliderThumb.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;


procedure TSliderThumb.Paint;
var
  R: TRect;
begin
  R := Rect(0, 0, FBitmap.Width, FBitmap.Height);
  if NumStates > 1 then begin
    if Down then
      R.Left := FBitmap.Width div 2
    else
      R.Right := FBitmap.Width div 2;
  end;
  DrawBitmapRectTransparent(Canvas, 0, 0, R, FBitmap, FTransparentColor);
end;

procedure TSliderThumb.AdjustBounds;
begin
  if FBitmap <> nil then
    SetBounds(Left, Top, FBitmap.Width div NumStates, FBitmap.Height);
end;

procedure TSliderThumb.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
  AdjustBounds;
end;

procedure TSliderThumb.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then begin
    FTransparentColor := Value;
    Invalidate;
  end;
end;

procedure TSliderThumb.SetDown(Value: Boolean);
begin
  if FDown <> Value then begin
    FDown := Value;
    Invalidate;
  end;
end;

procedure TSliderThumb.SetNumStates(Value: TNumThumbStates);
begin
  if FNumStates <> Value then begin
    FNumStates := Value;
    AdjustBounds;
  end;
end;

{ TSlider }

constructor TSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csCaptureMouse];
  ControlState := ControlState + [csCreating];
  Width := 140;
  Height := 40;
  FOrientation := soHorizontal;
  FOptions := [soShowFocus, soShowPoints, soSmooth];
  FEdgeSize := 2;
  FMinValue := 0;
  FMaxValue := 100;
  FIncrement := 10;
  FThumbVisible := true;  //*************//
  TabStop := True;
  CreateElements;
  ControlState := ControlState - [csCreating];
end;

destructor TSlider.Destroy;
var
  I: Integer;
begin
  FRuler.Free;
  FRuler2.Free;
  for I := Low(FImages) to High(FImages) do FImages[I].Free;
  inherited Destroy;
end;

procedure TSlider.DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    {$IFDEF PDJ_2}
    GetViewportOrgEx(DC, @Position);
    {$ELSE}
    GetViewportOrgEx(DC, Position);
    {$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    {Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0); }
    ///Fixed Thorsten Claus {25.01.2002}
    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TSlider.Paint;
var
  R: TRect;
begin
  if FTransparent then
    DrawParentImage(Self, Self.Canvas);
  if FRuler.Width > 0 then
    DrawBitmapTransparent(Canvas, FRulerOrg.X, FRulerOrg.Y, FRuler, FRuler2, clOlive,
                         (Orientation = soHorizontal) and FViewElapsedImage and FThumbVisible,
                          GetThumbOffset);
  if (soShowFocus in Options) and FActive and not (csDesigning in ComponentState) then begin
    R := ClientRect;
    InflateRect(R, -2, -2);
    Canvas.DrawFocusRect(R);
  end;
  if (soShowPoints in Options) then begin
    if Assigned(FOnDrawPoints) then FOnDrawPoints(Self)
    else DefaultDrawPoints(Increment, 3, 4);
  end;
end;

procedure TSlider.DefaultDrawPoints(PointsStep, PointsHeight,
  ExtremePointsHeight: Integer);
const
  MinInterval = 3;
var
  RulerLength: Integer;
  Interval, Scale, PointsCnt, X, H: Integer;
  X1, X2, Y1, Y2: Integer;
  I: Longint;
begin
  RulerLength := GetRulerLength;
  Scale := 0;
  repeat
    Inc(Scale);
    PointsCnt := (MaxValue - MinValue) div (Scale * PointsStep) + 1;
    if PointsCnt > 1 then
      Interval := (RulerLength - PointsCnt) div (PointsCnt - 1)
    else Interval := RulerLength;
  until (Interval >= MinInterval) or (Interval = RulerLength);
  I := MinValue;
  while not (I > MaxValue) do begin
    H := PointsHeight;
    if (I = MinValue) or (I = MaxValue) then H := ExtremePointsHeight;
    X := GetOffsetByValue(I);
    if Orientation = soHorizontal then begin
      X1 := X + FThumb.Width div 2;
      Y1 := FPointsRect.Top;
      X2 := X1 + 1;
      Y2 := Y1 + H;
    end
    else begin
      X1 := FPointsRect.Left;
      Y1 := X + FThumb.Height div 2;
      X2 := X1 + H;
      Y2 := Y1 + 1;
    end;
    Canvas.Rectangle(X1, Y1, X2, Y2);
    Inc(I, Scale * PointsStep);
  end;
end;

procedure TSlider.CreateElements;
var
  I: Integer;
begin
  FRuler := TBitmap.Create;
  FRuler2 := TBitmap.Create;
  FThumb := TSliderThumb.Create(Self);
  with FThumb do begin
    Parent := Self;
    Cursor := crHand;
    NumStates := 2;
    Visible := FThumbVisible;      //**************//
    OnMouseDown := ThumbMouseDown;
    OnMouseMove := ThumbMouseMove;
    OnMouseUp := ThumbMouseUp;
  end;
  for I := Low(FImages) to High(FImages) do SetImage(I, nil);
  AdjustElements;
end;

procedure TSlider.BuildRuler;
var
  DstR, BmpR: TRect;
  I, L, B, N, C, Offs, Len, RulerWidth: Integer;
  TmpBmp, TmpBmp2: TBitmap;
  Index, Index2: Integer;
begin
  TmpBmp := TBitmap.Create;
  TmpBmp2 := TBitmap.Create;
  try
    Index2 := Integer(siHRuler2);
    if Orientation = soHorizontal then
      Index := Integer(siHRuler)
    else Index := Integer(siVRuler);
    if Orientation = soHorizontal then begin
      L := Width - 2 * Indent;
      if L < 0 then L := 0;
      TmpBmp.Width := L;
      TmpBmp.Height := FImages[Index].Height;
      TmpBmp2.Width := TmpBmp.Width;
      TmpBmp2.Height := TmpBmp.Height;
      L := TmpBmp.Width - 2 * FEdgeSize;
      B := FImages[Index].Width - 2 * FEdgeSize;
      RulerWidth := FImages[Index].Width;
    end
    else begin
      TmpBmp.Width := FImages[Index].Width;
      TmpBmp.Height := Height - 2 * Indent;
      TmpBmp2.Width := TmpBmp.Width;
      TmpBmp2.Height := TmpBmp.Height;
      L := TmpBmp.Height - 2 * FEdgeSize;
      B := FImages[Index].Height - 2 * FEdgeSize;
      RulerWidth := FImages[Index].Height;
    end;
    N := (L div B) + 1;
    C := L mod B;
    for I := 0 to N - 1 do begin
      if I = 0 then begin
        Offs := 0;
        Len := RulerWidth - FEdgeSize;
      end
      else begin
        Offs := FEdgeSize + I * B;
        if I = N - 1 then Len := C + FEdgeSize
        else Len := B;
      end;
      if Orientation = soHorizontal then
        DstR := Rect(Offs, 0, Offs + Len, TmpBmp.Height)
      else DstR := Rect(0, Offs, TmpBmp.Width, Offs + Len);
      if I = 0 then Offs := 0
      else
        if I = N - 1 then Offs := FEdgeSize + B - C
        else Offs := FEdgeSize;
      if Orientation = soHorizontal then
      begin
        BmpR := Rect(Offs, 0, Offs + DstR.Right - DstR.Left, TmpBmp.Height);
        TmpBmp2.Canvas.CopyRect(DstR, FImages[Index2].Canvas, BmpR);
      end else
        BmpR := Rect(0, Offs, TmpBmp.Width, Offs + DstR.Bottom - DstR.Top);

      TmpBmp.Canvas.CopyRect(DstR, FImages[Index].Canvas, BmpR);
    end;

    FRuler.Assign(TmpBmp);
    FRuler2.Assign(TmpBmp2);
  finally
    TmpBmp.Free;
    TmpBmp2.Free;
  end;
end;

procedure TSlider.AdjustElements;
var
  SaveValue: Longint;
begin
  SaveValue := Value;
  BuildRuler;
  if Orientation = soHorizontal then begin
    if FThumb.Height > FRuler.Height then begin
      FThumb.SetBounds(Indent, Indent, FThumb.Width, FThumb.Height);
      FRulerOrg := Point(Indent, Indent + (FThumb.Height - FRuler.Height) div 2);
      FPointsRect := Rect(FRulerOrg.X, Indent + FThumb.Height + 1, FRulerOrg.X + FRuler.Width, Height - 1);
    end
    else begin
      FThumb.SetBounds(Indent, Indent + (FRuler.Height - FThumb.Height) div 2,
        FThumb.Width, FThumb.Height);
      FRulerOrg := Point(Indent, Indent);
      FPointsRect := Rect(FRulerOrg.X, Indent + FRuler.Height + 1, FRulerOrg.X + FRuler.Width, Height - 1);
    end;
  end
  else begin
    if FThumb.Width > FRuler.Width then begin
      FThumb.SetBounds(Indent, Indent, FThumb.Width, FThumb.Height);
      FRulerOrg := Point(Indent + (FThumb.Width - FRuler.Width) div 2, Indent);
      FPointsRect := Rect(Indent + FThumb.Width + 1, FRulerOrg.Y, Width - 1, FRulerOrg.Y + FRuler.Height);
    end
    else begin
      FThumb.SetBounds(Indent + (FRuler.Width - FThumb.Width) div 2, Indent,
        FThumb.Width, FThumb.Height);
      FRulerOrg := Point(Indent, Indent);
      FPointsRect := Rect(Indent + FRuler.Width + 1, FRulerOrg.Y, Width - 1, FRulerOrg.Y + FRuler.Height);
    end;
  end;
  Value := SaveValue;
end;

procedure TSlider.ImageChanged;
begin
  AdjustElements;
  Invalidate;
end;

procedure TSlider.Loaded;
var
  I : Integer;
begin
  inherited Loaded;
  for I := Low(FImages) to High(FImages) do
    if TSliderImage(I) in FUserImages then SetImage(I, FImages[I]);
end;

procedure TSlider.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSlider.DefineProperties(Filer: TFiler);

{$IFDEF WIN32}
  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := FUserImages <> TSlider(Filer.Ancestor).FUserImages
    else Result := FUserImages <> [];
  end;
{$ENDIF}

begin
  if Filer is TReader then inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('UserImages', ReadUserImages, WriteUserImages,
    {$IFDEF WIN32} DoWrite {$ELSE} FUserImages <> [] {$ENDIF});
end;

procedure TSlider.ReadUserImages(Stream: TStream);
begin
  Stream.ReadBuffer(FUserImages, SizeOf(FUserImages));
end;

procedure TSlider.WriteUserImages(Stream: TStream);
begin
  Stream.WriteBuffer(FUserImages, SizeOf(FUserImages));
end;

function TSlider.StoreImage(Index: Integer): Boolean;
begin
  Result := TSliderImage(Index) in FUserImages;
end;

function TSlider.GetImage(Index: Integer): TBitmap;
begin
  Result := FImages[Index];
end;

procedure TSlider.SetImage(Index: Integer; Value: TBitmap);
begin
  if Value = nil then begin
    if FImages[Index] = nil then FImages[Index] := TBitmap.Create;
    FImages[Index].Handle := LoadBitmap(HInstance, ImagesResNames[Index]);
    Exclude(FUserImages, TSliderImage(Index));
  end
  else begin
    FImages[Index].Assign(Value);
    Include(FUserImages, TSliderImage(Index));
  end;
  if Orientation = soHorizontal then begin
    if Index = 0 then FThumb.Bitmap := FImages[Index];
  end
  else begin
    if Index = 2 then FThumb.Bitmap := FImages[Index];
  end;
  if not (csCreating in ControlState) then ImageChanged;
end;

procedure TSlider.SetEdgeSize(Value: Integer);
var
  MaxSize: Integer;
begin
  if Orientation = soHorizontal then
    MaxSize := FImages[Integer(siHRuler)].Width
  else MaxSize := FImages[Integer(siVRuler)].Height;
  if Value * 2 < MaxSize then
    if Value <> FEdgeSize then begin
      FEdgeSize := Value;
      ImageChanged;
    end;
end;

function TSlider.GetNumThumbStates: TNumThumbStates;
begin
  Result := FThumb.NumStates;
end;

procedure TSlider.SetNumThumbStates(Value: TNumThumbStates);
begin
  FThumb.NumStates := Value;
end;

procedure TSlider.SetOrientation(Value: TSliderOrientation);
var
  Index: Integer;
begin
  if Orientation <> Value then begin
    FOrientation := Value;
    if Value = soHorizontal then Index := Integer(siHThumb)
    else Index := Integer(siVThumb);
    FThumb.Bitmap := FImages[Index];
    ImageChanged;
  end;
end;

procedure TSlider.SetOptions(Value: TSliderOptions);
begin
  if Value <> FOptions then begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TSlider.SetMinValue(Value: Longint);
begin
  if FMinValue <> Value then begin
    if Value <= MaxValue - Increment then begin
      FMinValue := Value;
      if (soShowPoints in Options) then Invalidate;
    end;
  end;
end;

procedure TSlider.SetMaxValue(Value: Longint);
begin
  if FMaxValue <> Value then begin
    if Value >= MinValue + Increment then begin
      FMaxValue := Value;
      if (soShowPoints in Options) then Invalidate;
    end;
  end;
end;

procedure TSlider.SetIncrement(Value: Longint);
begin
  if (Value > 0) and (FIncrement <> Value) then begin
    FIncrement := Value;
    Self.Value := FValue;
    Invalidate;
  end;
end;

function TSlider.GetValueByOffset(Offset: Integer): Longint;
begin
  if Orientation = soVertical then Offset := ClientHeight - Offset - FThumb.Height;
  Result := Round((Offset - Indent) * (MaxValue - MinValue) / GetRulerLength);
  if not (soSmooth in Options) then
    Result := (Result div Increment) * Increment;
  Result := MinValue + Result;
end;

function TSlider.GetOffsetByValue(Value: Longint): Integer;
begin
  Result := Round((Value - MinValue) * GetRulerLength /
    (MaxValue - MinValue)) + Indent;
  if Orientation = soVertical then Result := ClientHeight - Result - FThumb.Height;
end;

function TSlider.GetThumbOffset: Integer;
begin
  if Orientation = soHorizontal then Result := FThumb.Left
  else Result := FThumb.Top;
end;

procedure TSlider.SetThumbOffset(Value: Integer);
var
  RulerLength: Integer;
  ValueBefore: Longint;
begin
  ValueBefore := FValue;
  RulerLength := GetRulerLength;
  if Value < Indent then Value := Indent
  else if Value > Indent + RulerLength then
    Value := Indent + RulerLength;
  if not (soSmooth in Options) then
    Value := GetOffsetByValue(GetValueByOffset(Value));
  if Orientation = soHorizontal then FThumb.Left := Value
  else FThumb.Top := Value {- FThumb.Height};
  if FSliding then begin
    FValue := GetValueByOffset(Value);
    if ValueBefore <> FValue then Change;
  end;
end;

function TSlider.GetRulerLength: Integer;
begin
  if Orientation = soHorizontal then begin
    Result := FRuler.Width;
    Dec(Result, FThumb.Width);
  end
  else begin
    Result := FRuler.Height;
    Dec(Result, FThumb.Height);
  end;
end;

procedure TSlider.SetValue(Value: Longint);
var
  Changed: Boolean;
begin
  if Value > MaxValue then Value := MaxValue;
  if Value < MinValue then Value := MinValue;
  Changed := FValue <> Value;
  FValue := Value;
  ThumbOffset := GetOffsetByValue(Value);
  if Changed then
     Invalidate;
  if Changed and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSlider.ThumbJump(Jump: TJumpMode);
begin
  if Jump <> jmNone then begin
    case Jump of
      jmHome: Value := MinValue;
      jmPrior: Value := ((Value div Increment) * Increment) - Increment;
      jmNext: Value := ((Value div Increment) * Increment) + Increment;
      jmEnd: Value := MaxValue;
    end;
  end;
end;

function TSlider.JumpTo(X, Y: Integer): TJumpMode;
begin
  Result := jmNone;
  if (Orientation = soHorizontal) then begin
    if (FThumb.Left > X) then Result := jmPrior
    else if (FThumb.Left + FThumb.Width < X) then Result := jmNext;
  end
  else if (Orientation = soVertical) then begin
    if (FThumb.Top > Y) then Result := jmNext
    else if (FThumb.Top + FThumb.Height < Y) then Result := jmPrior;
  end;
end;

procedure TSlider.WMTimer(var Message: TMessage);
begin
  TimerTrack;
end;

procedure TSlider.CMFocusChanged(var Message: TCMFocusChanged);
var
  Active: Boolean;
begin
  with Message do Active := (Sender = Self);
  if Active <> FActive then begin
    FActive := Active;
    if (soShowFocus in Options) then Invalidate;
  end;
  inherited;
end;

procedure TSlider.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TSlider.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csReading in ComponentState) then ImageChanged;
end;

procedure TSlider.StopTracking;
begin
  if FTracking then begin
    if FTimerActive then begin
      KillTimer(Handle, 1);
      FTimerActive := False;
    end;
    FTracking := False;
    MouseCapture := False;
   { if (Orientation = soHorizontal) and FViewElapsedImage then
       Invalidate; }
  end;
end;

procedure TSlider.TimerTrack;
var
  Jump: TJumpMode;
begin
  Jump := JumpTo(FMousePos.X, FMousePos.Y);
  if Jump = FStartJump then begin
    ThumbJump(Jump);
    if not FTimerActive then begin
      SetTimer(Handle, 1, JumpInterval, nil);
      FTimerActive := True;
    end;
  end;
end;

procedure TSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not FThumbVisible then
     exit;

  if (Button = mbLeft) and not (ssDouble in Shift) then begin
    SetFocus;
    MouseCapture := True;
    FTracking := True;
    FMousePos := Point(X, Y);
    FStartJump := JumpTo(X, Y);
    TimerTrack;
  end;
end;

procedure TSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then FMousePos := Point(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TSlider.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 // if FThumbVisible then
     StopTracking;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  Jump: TJumpMode;
begin
  Jump := jmNone;
  if Shift = [] then begin
    if Key = VK_HOME then Jump := jmHome
    else if Key = VK_END then Jump := jmEnd;
    if Orientation = soHorizontal then begin
      if Key = VK_LEFT then Jump := jmPrior
      else if Key = VK_RIGHT then Jump := jmNext;
    end
    else begin
      if Key = VK_UP then Jump := jmNext
      else if Key = VK_DOWN then Jump := jmPrior;
    end;
  end;
  if Jump <> jmNone then begin
    Key := 0;
    ThumbJump(Jump);
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TSlider.ThumbMouseDown;
begin
  SetFocus;
  if Button = mbLeft then begin
    if Orientation = soHorizontal then FHit := X
    else FHit := Y;
    FSliding := True;
    FThumb.Down := True;
    if Assigned(FOnStartTracking) then FOnStartTracking(Self);
  end;
end;

procedure TSlider.ThumbMouseMove;
var
  Offset: Integer;
  P: TPoint;
begin
  P := ScreenToClient(FThumb.ClientToScreen(Point(X, Y)));
  if csLButtonDown in FThumb.ControlState then begin
    if Orientation = soHorizontal then Offset := P.X
    else Offset := P.Y;
    Dec(Offset, FHit);
    ThumbOffset := Offset;
  end;
end;

procedure TSlider.ThumbMouseUp;
begin
  FSliding := False;
  FThumb.Down := False;
  if Assigned(FOnStopTracking) then FOnStopTracking(Self);
end;

procedure TSlider.SetThumbVisible(Value : Boolean);
begin
  if Value <> FThumbVisible then
  begin
    FThumbVisible := Value;
    FThumb.Visible := Value;
    Invalidate;   
  end;
end;

procedure TSlider.SetTransparent(Value : Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TSlider.SetViewElapsedImage(Value : Boolean);
begin
  if Value <> FViewElapsedImage then
  begin
    FViewElapsedImage := Value;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Wabbit''s', [TSlider]);
end;

initialization
  Screen.Cursors[crHand] := LoadCursor(HInstance, CursorResName);
end.