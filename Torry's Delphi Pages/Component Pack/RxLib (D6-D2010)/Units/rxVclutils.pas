{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit rxVclutils;

{$I RX.INC}
{$P+,W-,R-,V-}

interface

uses
  Windows, Classes, Graphics, Forms, Controls, Dialogs;

{ Windows resources (bitmaps and icons) VCL-oriented routines }

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY, DstW,
  DstH: Integer; SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
function MakeBitmap(ResID: PChar): TBitmap;
function MakeBitmapID(ResID: Word): TBitmap;
function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
  Index: Integer);
procedure ImageListDrawDisabled(Images: TImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);

function MakeIcon(ResID: PChar): TIcon;
function MakeIconID(ResID: Word): TIcon;
function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;

{ Service routines }

procedure NotImplemented;
procedure ResourceNotFound(ResID: PChar);
function PointInRect(const P: TPoint; const R: TRect): Boolean;
function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
function PaletteColor(Color: TColor): Longint;
function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;
procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
procedure CopyParentImage(Control: TControl; Dest: TCanvas);
procedure Delay(MSecs: Longint);
procedure CenterControl(Control: TControl);
procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
function MakeVariant(const Values: array of Variant): Variant;
function CreateRotatedFont(Font: TFont; Angle: Integer): HFont;
function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
function MsgDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
{$IFDEF CBUILDER}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWnd;
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWnd;
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF CBUILDER}
function IsForegroundTask: Boolean;
procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
function GetAveCharSize(Canvas: TCanvas): TPoint;
function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
procedure FreeUnusedOle;
procedure Beep;
function GetWindowsVersion: string;
function LoadDLL(const LibName: string): THandle;
function RegisterServer(const ModuleName: string): Boolean;

{ Gradient filling routine }

type
  TFillDirection = (fdTopToBottom, fdBottomToTop, fdLeftToRight, fdRightToLeft);

procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);

{ String routines }

function GetEnvVar(const VarName: string): string;
function AnsiUpperFirstChar(const S: string): string;
function StringToPChar(var S: string): PChar;
function StrPAlloc(const S: string): PChar;
procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
function DropT(const S: string): string;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
function GetMemoSize(fpBlock: Pointer): Longint;
function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean;
{$IFNDEF RX_D5}
procedure FreeAndNil(var Obj);
{$ENDIF}

{ Manipulate huge pointers routines }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);

{ Standard Windows colors that are not defined by Delphi }

const
  clCream = TColor($A6CAF0);
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($FFFBF0);
  clMedGray = TColor($A4A0A0); 

{ ModalResult constants }

{$IFNDEF RX_D3}
const
  mrNoToAll  = mrAll + 1;
  mrYesToAll = mrNoToAll + 1;
{$ENDIF}

{$IFNDEF RX_D4}

{ Mouse Wheel message }

{$IFDEF VER90}
const
  WM_MOUSEWHEEL    =    $020A;
  WHEEL_DELTA      =      120;
  WHEEL_PAGESCROLL = MAXDWORD;

  SM_MOUSEWHEELPRESENT    =    75;
  MOUSEEVENTF_WHEEL       = $0800;
  SPI_GETWHEELSCROLLLINES =   104;
  SPI_SETWHEELSCROLLLINES =   105;
{$ENDIF}

type
  TWMMouseWheel = record
    Msg: Cardinal;
    Keys: Word;
    Delta: Word;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;

{$ENDIF RX_D4}

{ Cursor routines }

const
  WaitCursor: TCursor = crHourGlass;

procedure StartWait;
procedure StopWait;
function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
function LoadAniCursor(Instance: THandle; ResID: PChar): HCursor;

{ Windows API level routines }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPalette;
  TransparentColor: TColorRef);
procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBitmap;
  DstX, DstY: Integer; TransparentColor: TColorRef);
function PaletteEntries(Palette: HPALETTE): Integer;
function WindowClassName(Wnd: HWnd): string;
function ScreenWorkArea: TRect;
procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
procedure ActivateWindow(Wnd: HWnd);
procedure ShowWinNoAnimate(Handle: HWnd; CmdShow: Integer);
procedure CenterWindow(Wnd: HWnd);
procedure ShadeRect(DC: HDC; const Rect: TRect);
procedure KillMessage(Wnd: HWnd; Msg: Cardinal);

{ Convert dialog units to pixels and backwards }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
function DialogUnitsToPixelsY(DlgUnits: Word): Word;
function PixelsToDialogUnitsX(PixUnits: Word): Word;
function PixelsToDialogUnitsY(PixUnits: Word): Word;

{ Grid drawing }

type
  TVertAlignment = (vaTopJustify, vaCenter, vaBottomJustify);

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean
  {$IFDEF RX_D4}; ARightToLeft: Boolean = False {$ENDIF});
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment); {$IFDEF RX_D4} overload; {$ENDIF}
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean); {$IFDEF RX_D4} overload; {$ENDIF}
{$IFDEF RX_D4}
procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean); overload;
procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean); overload;
{$ENDIF}
procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);

{ TScreenCanvas }

type
  TScreenCanvas = class(TCanvas)
  private
    FDeviceContext: HDC;
  protected
    procedure CreateHandle; override;
  public
    destructor Destroy; override;
    procedure SetOrigin(X, Y: Integer);
    procedure FreeHandle;
  end;

function CheckWin32(OK: Boolean): Boolean; { obsolete, use Win32Check }
{$IFNDEF RX_D3}
function Win32Check(RetVal: Bool): Bool;
{$ENDIF}
procedure RaiseWin32Error(ErrorCode: DWORD);

{$IFNDEF RX_D3} { for Delphi 3.0 and previous versions compatibility }
type
  TCustomForm = TForm;
  TDate = TDateTime;
  TTime = TDateTime;

function ResStr(Ident: Cardinal): string;
{$ELSE}
function ResStr(const Ident: string): string;
{$ENDIF RX_D3}

{$IFNDEF RX_D4}
type
  Longword = Longint;
{$ENDIF}

implementation

Uses SysUtils, Messages, rxMaxMin, Consts, RxConst, {$IFDEF RX_V110} SysConst, {$ENDIF}
  {$IFDEF RX_D6} RTLConsts, Variants, {$ENDIF}  // Polaris
  {$IFDEF RX_D12} Character, {$ENDIF}
  CommCtrl, RxCConst;

{ Exceptions }

procedure ResourceNotFound(ResID: PChar);
var
  S: string;
begin
  if LongRec(ResID).Hi = 0 then
    S := IntToStr(LongRec(ResID).Lo)
  else
    S := StrPas(ResID);
  raise EResNotFound.CreateFmt(ResStr(SResNotFound), [S]);
end;

{ Bitmaps }

function MakeModuleBitmap(Module: THandle; ResID: PChar): TBitmap;
begin
  Result := TBitmap.Create;
  try
    if Module <> 0 then
    begin
      if LongRec(ResID).Hi = 0 then
        Result.LoadFromResourceID(Module, LongRec(ResID).Lo)
      else
        Result.LoadFromResourceName(Module, StrPas(ResID));
    end
    else
    begin
      Result.Handle := LoadBitmap(Module, ResID);
      if Result.Handle = 0 then
        ResourceNotFound(ResID);
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function MakeBitmap(ResID: PChar): TBitmap;
begin
  Result := MakeModuleBitmap(hInstance, ResID);
end;

function MakeBitmapID(ResID: Word): TBitmap;
begin
  Result := MakeModuleBitmap(hInstance, MakeIntResource(ResID));
end;

procedure AssignBitmapCell(Source: TGraphic; Dest: TBitmap; Cols, Rows,
  Index: Integer);
var
  CellWidth, CellHeight: Integer;
begin
  if (Source <> nil) and (Dest <> nil) then
  begin
    if Cols <= 0 then
      Cols := 1;
    if Rows <= 0 then
      Rows := 1;
    if Index < 0 then
      Index := 0;
    CellWidth := Source.Width div Cols;
    CellHeight := Source.Height div Rows;
    with Dest do
    begin
      Width := CellWidth;
      Height := CellHeight;
    end;
    if Source is TBitmap then
    begin
      Dest.Canvas.CopyRect(Bounds(0, 0, CellWidth, CellHeight),
        TBitmap(Source).Canvas, Bounds((Index mod Cols) * CellWidth,
        (Index div Cols) * CellHeight, CellWidth, CellHeight));
{$IFDEF RX_D3}
      Dest.TransparentColor := TBitmap(Source).TransparentColor;
{$ENDIF RX_D3}
    end
    else
    begin
      Dest.Canvas.Brush.Color := clSilver;
      Dest.Canvas.FillRect(Bounds(0, 0, CellWidth, CellHeight));
      Dest.Canvas.Draw(-(Index mod Cols) * CellWidth,
        -(Index div Cols) * CellHeight, Source);
    end;
{$IFDEF RX_D3}
    Dest.Transparent := Source.Transparent;
{$ENDIF RX_D3}
  end;
end;

type
  TParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if (Control = nil) or (Control.Parent = nil) then
    Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
  with Control.Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    with Control do
    begin
      SelfR := Bounds(Left, Top, Width, Height);
      X := -Left;
      Y := -Top;
    end;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
        Control.Parent.ClientHeight);
      with TParentControl(Control.Parent) do
      begin
        Perform(WM_ERASEBKGND, DC, 0);
        PaintWindow(DC);
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if Control.Parent.Controls[I] = Control then
        Break
      else
        if (Control.Parent.Controls[I] <> nil) and
          (Control.Parent.Controls[I] is TGraphicControl) then
          with TGraphicControl(Control.Parent.Controls[I]) do
          begin
            CtlR := Bounds(Left, Top, Width, Height);
            if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
              ControlState := ControlState + [csPaintCopy];
              SaveIndex := SaveDC(DC);
              try
                SaveIndex := SaveDC(DC);
                SetViewportOrgEx(DC, Left + X, Top + Y, nil);
                IntersectClipRect(DC, 0, 0, Width, Height);
                Perform(WM_PAINT, DC, 0);
              finally
                RestoreDC(DC, SaveIndex);
                ControlState := ControlState - [csPaintCopy];
              end;
            end;
          end;
    end;
  finally
    with Control.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

{ Transparent bitmap }

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPalette;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBitmap;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPalette;
begin
  { Create some DCs to hold temporary data }
  BackDC := CreateCompatibleDC(DstDC);
  ObjectDC := CreateCompatibleDC(DstDC);
  MemDC := CreateCompatibleDC(DstDC);
  SaveDC := CreateCompatibleDC(DstDC);
  { Create a bitmap for each DC }
  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);
  { Each DC must select a bitmap object to store pixel data }
  bmBackOld := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld := SelectObject(MemDC, bmAndMem);
  bmSaveOld := SelectObject(SaveDC, bmSave);
  { Select palette }
  palDst := 0; palMem := 0; palSave := 0; palObj := 0;
  if Palette <> 0 then
  begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;
  { Set proper mapping mode }
  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));
  { Save the bitmap sent here }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);
  { Set the background color of the source DC to the color,         }
  { contained in the parts of the bitmap that should be transparent }
  Color := SetBkColor(SaveDC, PaletteColor(TransparentColor));
  { Create the object mask for the bitmap by performing a BitBlt()  }
  { from the source bitmap to a monochrome bitmap                   }
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);
  { Set the background color of the source DC back to the original  }
  SetBkColor(SaveDC, Color);
  { Create the inverse of the object mask }
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  { Copy the background of the main DC to the destination }
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  { Mask out the places where the bitmap will be placed }
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  { Mask out the transparent colored pixels on the bitmap }
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  { XOR the bitmap with the background on the destination DC }
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  { Copy the destination to the screen }
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0,
    SRCCOPY);
  { Restore palette }
  if Palette <> 0 then
  begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
  { Delete the memory bitmaps }
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));
  { Delete the memory DCs }
  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;

procedure DrawTransparentBitmapRect(DC: HDC; Bitmap: HBitmap; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; TransparentColor: TColorRef);
var
  hdcTemp: HDC;
begin
  hdcTemp := CreateCompatibleDC(DC);
  try
    SelectObject(hdcTemp, Bitmap);
    with SrcRect do
      StretchBltTransparent(DC, DstX, DstY, DstW, DstH, hdcTemp,
        Left, Top, Right - Left, Bottom - Top, 0, TransparentColor);
  finally
    DeleteDC(hdcTemp);
  end;
end;

procedure DrawTransparentBitmap(DC: HDC; Bitmap: HBitmap;
  DstX, DstY: Integer; TransparentColor: TColorRef);
var
  BM: Windows.TBitmap;
begin
  GetObject(Bitmap, SizeOf(BM), @BM);
  DrawTransparentBitmapRect(DC, Bitmap, DstX, DstY, BM.bmWidth, BM.bmHeight,
    Rect(0, 0, BM.bmWidth, BM.bmHeight), TransparentColor);
end;

procedure StretchBitmapTransparent(Dest: TCanvas; Bitmap: TBitmap;
  TransparentColor: TColor; DstX, DstY, DstW, DstH, SrcX, SrcY,
  SrcW, SrcH: Integer);
var
  CanvasChanging: TNotifyEvent;
begin
  if DstW <= 0 then
    DstW := Bitmap.Width;
  if DstH <= 0 then
    DstH := Bitmap.Height;
  if (SrcW <= 0) or (SrcH <= 0) then
  begin
    SrcX := 0; SrcY := 0;
    SrcW := Bitmap.Width;
    SrcH := Bitmap.Height;
  end;
  if not Bitmap.Monochrome then
    SetStretchBltMode(Dest.Handle, STRETCH_DELETESCANS);
  CanvasChanging := Bitmap.Canvas.OnChanging;
{$IFDEF RX_D3}
  Bitmap.Canvas.Lock;
{$ENDIF}
  try
    Bitmap.Canvas.OnChanging := nil;
    if TransparentColor = clNone then
    begin
      StretchBlt(Dest.Handle, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
        SrcX, SrcY, SrcW, SrcH, Dest.CopyMode);
    end
    else
    begin
{$IFDEF RX_D3}
      if TransparentColor = clDefault then
        TransparentColor := Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
{$ENDIF}
      if Bitmap.Monochrome then
        TransparentColor := clWhite
      else
        TransparentColor := ColorToRGB(TransparentColor);
      StretchBltTransparent(Dest.Handle, DstX, DstY, DstW, DstH,
        Bitmap.Canvas.Handle, SrcX, SrcY, SrcW, SrcH, Bitmap.Palette,
        TransparentColor);
    end;
  finally
    Bitmap.Canvas.OnChanging := CanvasChanging;
{$IFDEF RX_D3}
    Bitmap.Canvas.Unlock;
{$ENDIF}
  end;
end;

procedure StretchBitmapRectTransparent(Dest: TCanvas; DstX, DstY,
  DstW, DstH: Integer; SrcRect: TRect; Bitmap: TBitmap;
  TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, DstW, DstH, Left, Top, Right - Left, Bottom - Top);
end;

procedure DrawBitmapRectTransparent(Dest: TCanvas; DstX, DstY: Integer;
  SrcRect: TRect; Bitmap: TBitmap; TransparentColor: TColor);
begin
  with SrcRect do
    StretchBitmapTransparent(Dest, Bitmap, TransparentColor,
    DstX, DstY, Right - Left, Bottom - Top, Left, Top, Right - Left,
    Bottom - Top);
end;

procedure DrawBitmapTransparent(Dest: TCanvas; DstX, DstY: Integer;
  Bitmap: TBitmap; TransparentColor: TColor);
begin
  StretchBitmapTransparent(Dest, Bitmap, TransparentColor, DstX, DstY,
    Bitmap.Width, Bitmap.Height, 0, 0, Bitmap.Width, Bitmap.Height);
end;

{ ChangeBitmapColor. This function create new TBitmap object.
  You must destroy it outside by calling TBitmap.Free method. }

function ChangeBitmapColor(Bitmap: TBitmap; Color, NewColor: TColor): TBitmap;
var
  R: TRect;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Height := Bitmap.Height;
      Width := Bitmap.Width;
      R := Bounds(0, 0, Width, Height);
      Canvas.Brush.Color := NewColor;
      Canvas.FillRect(R);
      Canvas.BrushCopy(R, Bitmap, R, Color);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ CreateDisabledBitmap. Creating TBitmap object with disable button glyph
  image. You must destroy it outside by calling TBitmap.Free method. }

const
  ROP_DSPDxax = $00E20746;

function CreateDisabledBitmapEx(FOriginal: TBitmap; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean): TBitmap;
var
  MonoBmp: TBitmap;
  IRect: TRect;
begin
  IRect := Rect(0, 0, FOriginal.Width, FOriginal.Height);
  Result := TBitmap.Create;
  try
    Result.Width := FOriginal.Width;
    Result.Height := FOriginal.Height;
    MonoBmp := TBitmap.Create;
    try
      with MonoBmp do
      begin
        Width := FOriginal.Width;
        Height := FOriginal.Height;
        Canvas.CopyRect(IRect, FOriginal.Canvas, IRect);
{$IFDEF RX_D3}
        HandleType := bmDDB;
{$ENDIF}
        Canvas.Brush.Color := OutlineColor;
        if Monochrome then
        begin
          Canvas.Font.Color := clWhite;
          Monochrome := False;
          Canvas.Brush.Color := clWhite;
        end;
        Monochrome := True;
      end;
      with Result.Canvas do
      begin
        Brush.Color := BackColor;
        FillRect(IRect);
        if DrawHighlight then
        begin
          Brush.Color := HighlightColor;
          SetTextColor(Handle, clBlack);
          SetBkColor(Handle, clWhite);
          BitBlt(Handle, 1, 1, WidthOf(IRect), HeightOf(IRect),
            MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
        end;
        Brush.Color := ShadowColor;
        SetTextColor(Handle, clBlack);
        SetBkColor(Handle, clWhite);
        BitBlt(Handle, 0, 0, WidthOf(IRect), HeightOf(IRect),
          MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
    finally
      MonoBmp.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateDisabledBitmap(FOriginal: TBitmap; OutlineColor: TColor): TBitmap;
begin
  Result := CreateDisabledBitmapEx(FOriginal, OutlineColor,
    clBtnFace, clBtnHighlight, clBtnShadow, True);
end;

procedure ImageListDrawDisabled(Images: TImageList; Canvas: TCanvas;
  X, Y, Index: Integer; HighlightColor, GrayColor: TColor; DrawHighlight: Boolean);
var
  Bmp: TBitmap;
  SaveColor: TColor;
begin
  SaveColor := Canvas.Brush.Color;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := Images.Width;
    Bmp.Height := Images.Height;
    with Bmp.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Images.Width, Images.Height));
      ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
    end;
    Bmp.Monochrome := True;
    if DrawHighlight then
    begin
      Canvas.Brush.Color := HighlightColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X + 1, Y + 1, Images.Width,
        Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
    Canvas.Brush.Color := GrayColor;
    SetTextColor(Canvas.Handle, clWhite);
    SetBkColor(Canvas.Handle, clBlack);
    BitBlt(Canvas.Handle, X, Y, Images.Width,
      Images.Height, Bmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
  finally
    Bmp.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

{ Brush Pattern }

function CreateTwoColorsBrushPattern(Color1, Color2: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := 8;
  Result.Height := 8;
  with Result.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color1;
    FillRect(Rect(0, 0, Result.Width, Result.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
          Pixels[X, Y] := Color2;      { on even/odd rows }
  end;
end;

{ Icons }

function MakeIcon(ResID: PChar): TIcon;
begin
  Result := MakeModuleIcon(hInstance, ResID);
end;

function MakeIconID(ResID: Word): TIcon;
begin
  Result := MakeModuleIcon(hInstance, MakeIntResource(ResID));
end;

function MakeModuleIcon(Module: THandle; ResID: PChar): TIcon;
begin
  Result := TIcon.Create;
  Result.Handle := LoadIcon(Module, ResID);
  if Result.Handle = 0 then
  begin
    Result.Free;
    Result := nil;
  end;
end;

{ Create TBitmap object from TIcon }

function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
var
  IWidth, IHeight: Integer;
begin
  IWidth := Icon.Width;
  IHeight := Icon.Height;
  Result := TBitmap.Create;
  try
    Result.Width := IWidth;
    Result.Height := IHeight;
    with Result.Canvas do
    begin
      Brush.Color := BackColor;
      FillRect(Rect(0, 0, IWidth, IHeight));
      Draw(0, 0, Icon);
    end;
{$IFDEF RX_D3}
    Result.TransparentColor := BackColor;
    Result.Transparent := True;
{$ENDIF}
  except
    Result.Free;
    raise;
  end;
end;

function CreateIconFromBitmap(Bitmap: TBitmap; TransparentColor: TColor): TIcon;
begin
  with TImageList.CreateSize(Bitmap.Width, Bitmap.Height) do
  try
{$IFDEF RX_D3}
    if TransparentColor = clDefault then
      TransparentColor := Bitmap.TransparentColor;
{$ENDIF}
    AllocBy := 1;
    AddMasked(Bitmap, TransparentColor);
    Result := TIcon.Create;
    try
      GetIcon(0, Result);
    except
      Result.Free;
      raise;
    end;
  finally
    Free;
  end;
end;

{ Dialog units }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

function DialogUnitsToPixelsY(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

function PixelsToDialogUnitsX(PixUnits: Word): Word;
begin
  Result := PixUnits * 4 div LoWord(GetDialogBaseUnits);
end;

function PixelsToDialogUnitsY(PixUnits: Word): Word;
begin
  Result := PixUnits * 8 div HiWord(GetDialogBaseUnits);
end;

{ Service routines }

type
  THack = class(TCustomControl);

function LoadDLL(const LibName: string): THandle;
var
  ErrMode: Cardinal;
begin
  ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  Result := LoadLibrary(PChar(LibName));
  SetErrorMode(ErrMode);
  if Result < HINSTANCE_ERROR then
{$IFDEF RX_D6}      // Polaris
    RaiseLastOSError;
{$ELSE}
    Win32Check(False);
{$ENDIF}
end;

function RegisterServer(const ModuleName: string): Boolean;
{ RegisterServer procedure written by Vladimir Gaitanoff, 2:50/430.2 }
type
  TProc = procedure;
var
  Handle: THandle;
  DllRegServ: Pointer;
begin
  Result := False;
  Handle := LoadDLL(ModuleName);
  try
    DllRegServ := GetProcAddress(Handle, 'DllRegisterServer');
    if Assigned(DllRegServ) then
    begin
      TProc(DllRegServ);
      Result := True;
    end;
  finally
    FreeLibrary(Handle);
  end;
end;

procedure Beep;
begin
  MessageBeep(0);
end;

procedure FreeUnusedOle;
begin
  FreeLibrary(GetModuleHandle('OleAut32'));
end;

procedure NotImplemented;
begin
  Screen.Cursor := crDefault;
  MessageDlg(LoadStr(SNotImplemented), mtInformation, [mbOk], 0);
  Abort;
end;

procedure PaintInverseRect(const RectOrg, RectEnd: TPoint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetDC(0);
  try
    R := Rect(RectOrg.X, RectOrg.Y, RectEnd.X, RectEnd.Y);
    InvertRect(DC, R);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure DrawInvertFrame(ScreenRect: TRect; Width: Integer);
var
  DC: HDC;
  I: Integer;
begin
  DC := GetDC(0);
  try
    for I := 1 to Width do
    begin
      DrawFocusRect(DC, ScreenRect);
      InflateRect(ScreenRect, -1, -1);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

function PointInRect(const P: TPoint; const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= P.X) and (Top <= P.Y) and (Right >= P.X) and (Bottom >= P.Y);
end;

function PointInPolyRgn(const P: TPoint; const Points: array of TPoint): Boolean;
type
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;
var
  Rgn: HRgn;
begin
  Rgn := CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, WINDING);
  try
    Result := PtInRegion(Rgn, P.X, P.Y);
  finally
    DeleteObject(Rgn);
  end;
end;

function PaletteColor(Color: TColor): Longint;
begin
  Result := ColorToRGB(Color) or PaletteMask;
end;

procedure KillMessage(Wnd: HWnd; Msg: Cardinal);
{ Delete the requested message from the queue, but throw back }
{ any WM_QUIT msgs that PeekMessage may also return.          }
{ Copied from DbGrid.pas                                      }
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.WParam);
end;

function CreateRotatedFont(Font: TFont; Angle: Integer): HFont;
var
  LogFont: TLogFont;
begin
  FillChar(LogFont, SizeOf(LogFont), 0);
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;
    lfOrientation := 0;
    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Ord(fsItalic in Font.Style);
    lfUnderline := Ord(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
{$IFDEF RX_D3}
    lfCharSet := Byte(Font.Charset);
    if AnsiCompareText(Font.Name, 'Default') = 0 then
      StrPCopy(lfFaceName, string(DefFontData.Name))
    else
      StrPCopy(lfFaceName, Font.Name);
{$ELSE}
  {$IFDEF VER93}
    lfCharSet := Byte(Font.Charset);
  {$ELSE}
    lfCharSet := DEFAULT_CHARSET;
  {$ENDIF}
    StrPCopy(lfFaceName, Font.Name);
{$ENDIF}
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Font.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LogFont);
end;

procedure Delay(MSecs: Longint);
var
  FirstTickCount, Now: Longint;
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
    { allowing access to other controls, etc. }
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount);
end;

function PaletteEntries(Palette: HPALETTE): Integer;
begin
  GetObject(Palette, SizeOf(Integer), @Result);
end;

procedure CenterControl(Control: TControl);
var
  X, Y: Integer;
begin
  X := Control.Left;
  Y := Control.Top;
  if Control is TForm then
  begin
    with Control do
    begin
      if (TForm(Control).FormStyle = fsMDIChild) and
        (Application.MainForm <> nil) then
      begin
        X := (Application.MainForm.ClientWidth - Width) div 2;
        Y := (Application.MainForm.ClientHeight - Height) div 2;
      end
      else
      begin
        X := (Screen.Width - Width) div 2;
        Y := (Screen.Height - Height) div 2;
      end;
    end;
  end
  else
    if Control.Parent <> nil then
      with Control do
      begin
        Parent.HandleNeeded;
        X := (Parent.ClientWidth - Width) div 2;
        Y := (Parent.ClientHeight - Height) div 2;
      end;
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  with Control do
    SetBounds(X, Y, Width, Height);
end;

procedure FitRectToScreen(var Rect: TRect);
var
  X, Y, Delta: Integer;
begin
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  with Rect do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

procedure CenterWindow(Wnd: HWnd);
var
  R: TRect;
begin
  GetWindowRect(Wnd, R);
  R := Rect((GetSystemMetrics(SM_CXSCREEN) - R.Right + R.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - R.Bottom + R.Top) div 2,
    R.Right - R.Left, R.Bottom - R.Top);
  FitRectToScreen(R);
  SetWindowPos(Wnd, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOZORDER);
end;

procedure MergeForm(AControl: TWinControl; AForm: TForm; Align: TAlign;
  Show: Boolean);
var
  R: TRect;
  AutoScroll: Boolean;
begin
  AutoScroll := AForm.AutoScroll;
  AForm.Hide;
  THack(AForm).DestroyHandle;
  with AForm do
  begin
    BorderStyle := bsNone;
    BorderIcons := [];
    Parent := AControl;
  end;
  AControl.DisableAlign;
  try
    if Align <> alNone then
      AForm.Align := Align
    else
    begin
      R := AControl.ClientRect;
      AForm.SetBounds(R.Left + AForm.Left, R.Top + AForm.Top, AForm.Width,
        AForm.Height);
    end;
    AForm.AutoScroll := AutoScroll;
    AForm.Visible := Show;
  finally
    AControl.EnableAlign;
  end;
end;

{ ShowMDIClientEdge function has been copied from Inprise's FORMS.PAS unit,
  Delphi 4 version }
procedure ShowMDIClientEdge(ClientHandle: THandle; ShowEdge: Boolean);
var
  Style: Longint;
begin
  if ClientHandle <> 0 then
  begin
    Style := GetWindowLong(ClientHandle, GWL_EXSTYLE);
    if ShowEdge then
      if Style and WS_EX_CLIENTEDGE = 0 then
        Style := Style or WS_EX_CLIENTEDGE
      else
        Exit
    else
      if Style and WS_EX_CLIENTEDGE <> 0 then
        Style := Style and not WS_EX_CLIENTEDGE
      else
        Exit;
    SetWindowLong(ClientHandle, GWL_EXSTYLE, Style);
    SetWindowPos(ClientHandle, 0, 0,0,0,0, SWP_FRAMECHANGED or SWP_NOACTIVATE or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

function MakeVariant(const Values: array of Variant): Variant;
begin
  if High(Values) - Low(Values) > 1 then
    Result := VarArrayOf(Values)
  else
    if High(Values) - Low(Values) = 1 then
      Result := Values[Low(Values)]
    else
      Result := Null;
end;

{ Shade rectangle }

procedure ShadeRect(DC: HDC; const Rect: TRect);
const
  HatchBits: array[0..7] of Word = ($11, $22, $44, $88, $11, $22, $44, $88);
var
  Bitmap: HBitmap;
  SaveBrush: HBrush;
  SaveTextColor, SaveBkColor: TColorRef;
begin
  Bitmap := CreateBitmap(8, 8, 1, 1, @HatchBits);
  SaveBrush := SelectObject(DC, CreatePatternBrush(Bitmap));
  try
    SaveTextColor := SetTextColor(DC, clWhite);
    SaveBkColor := SetBkColor(DC, clBlack);
    with Rect do
      PatBlt(DC, Left, Top, Right - Left, Bottom - Top, $00A000C9);
    SetBkColor(DC, SaveBkColor);
    SetTextColor(DC, SaveTextColor);
  finally
    DeleteObject(SelectObject(DC, SaveBrush));
    DeleteObject(Bitmap);
  end;
end;

function ScreenWorkArea: TRect;
begin
  if not SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) then
    with Screen do
      Result := Bounds(0, 0, Width, Height);
end;

function WindowClassName(Wnd: HWnd): string;
var
  Buffer: array[0..255] of Char;
begin
  SetString(Result, Buffer, GetClassName(Wnd, Buffer, SizeOf(Buffer) - 1));
end;

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
{$IFDEF RX_D3}
    Result := Info.iMinAnimate <> 0
{$ELSE}
    Result := Info.iMinAnimate
{$ENDIF}
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWnd; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then
    SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then
    SetAnimation(True);
end;

procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
begin
  if IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      SetFocus(Wnd);
    end;
  end;
end;

function GetWindowParent(Wnd: HWnd): HWnd;
begin
  Result := GetWindowLong(Wnd, GWL_HWNDPARENT);
end;

procedure ActivateWindow(Wnd: HWnd);
begin
  if Wnd <> 0 then
  begin
    ShowWinNoAnimate(Wnd, SW_SHOW);
    SetForegroundWindow(Wnd);
  end;
end;

{$IFDEF CBUILDER}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWnd;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWnd;
{$ENDIF CBUILDER}
var
  BufClass, BufTitle: PChar;
begin
  Result := 0;
  if (MainFormClass = '') and (ATitle = '') then
    Exit;
  BufClass := nil;
  BufTitle := nil;
  if (MainFormClass <> '') then
    BufClass := StrPAlloc(MainFormClass);
  if (ATitle <> '') then
    BufTitle := StrPAlloc(ATitle);
  try
    Result := FindWindow(BufClass, BufTitle);
  finally
    StrDispose(BufTitle);
    StrDispose(BufClass);
  end;
end;

function WindowsEnum(Handle: HWnd; Param: Longint): Bool; export; stdcall;
begin
  if WindowClassName(Handle) = 'TAppBuilder' then
  begin
    Result := False;
    PLongint(Param)^ := 1;
  end
  else
    Result := True;
end;

{$IFDEF CBUILDER}
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF CBUILDER}
var
  PrevWnd, PopupWnd, ParentWnd: HWnd;
  IsDelphi: Longint;
begin
  Result := False;
  PrevWnd := FindPrevInstance(MainFormClass, ATitle);
  if PrevWnd <> 0 then
  begin
    ParentWnd := GetWindowParent(PrevWnd);
    while (ParentWnd <> GetDesktopWindow) and (ParentWnd <> 0) do
    begin
      PrevWnd := ParentWnd;
      ParentWnd := GetWindowParent(PrevWnd);
    end;
    if WindowClassName(PrevWnd) = 'TApplication' then
    begin
      IsDelphi := 0;
      EnumThreadWindows(GetWindowTask(PrevWnd), @WindowsEnum,
        LPARAM(@IsDelphi));
      if Boolean(IsDelphi) then
        Exit;
      if IsIconic(PrevWnd) then
      begin { application is minimized }
        SendMessage(PrevWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
        Result := True;
        Exit;
      end
      else
        ShowWinNoAnimate(PrevWnd, SW_SHOWNOACTIVATE);
    end
    else
      ActivateWindow(PrevWnd);
    PopupWnd := GetLastActivePopup(PrevWnd);
    if (PrevWnd <> PopupWnd) and IsWindowVisible(PopupWnd) and
      IsWindowEnabled(PopupWnd) then
      SetForegroundWindow(PopupWnd)
    else
      ActivateWindow(PopupWnd);
    Result := True;
  end;
end;

{ Standard Windows MessageBox function }

function MsgBox(const Caption, Text: string; Flags: Integer): Integer;
begin
{$IFNDEF RX_D5}
  SetAutoSubClass(True);
  try
{$ENDIF}
    Result := Application.MessageBox(PChar(Text), PChar(Caption), Flags);
{$IFNDEF RX_D5}
  finally
    SetAutoSubClass(False);
  end;
{$ENDIF}
end;

function MsgDlg(const Msg: string; AType: TMsgDlgType;
  AButtons: TMsgDlgButtons; HelpCtx: Longint): Word;
begin
  Result := MessageDlg(Msg, AType, AButtons, HelpCtx);
end;

{ Gradient fill procedure - displays a gradient beginning with a chosen    }
{ color and ending with another chosen color. Based on TGradientFill       }
{ component source code written by Curtis White, cwhite@teleport.com.      }
procedure GradientFillRect(Canvas: TCanvas; ARect: TRect; StartColor,
  EndColor: TColor; Direction: TFillDirection; Colors: Byte);
var
  StartRGB: array[0..2] of Byte;    { Start RGB values }
  RGBDelta: array[0..2] of Integer; { Difference between start and end RGB values }
  ColorBand: TRect;                 { Color band rectangular coordinates }
  I, Delta: Integer;
  Brush: HBrush;
begin
  if IsRectEmpty(ARect) then
    Exit;
  if Colors < 2 then
  begin
    Brush := CreateSolidBrush(ColorToRGB(StartColor));
    FillRect(Canvas.Handle, ARect, Brush);
    DeleteObject(Brush);
    Exit;
  end;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  case Direction of
    fdTopToBottom, fdLeftToRight:
    begin
      { Set the Red, Green and Blue colors }
      StartRGB[0] := GetRValue(StartColor);
      StartRGB[1] := GetGValue(StartColor);
      StartRGB[2] := GetBValue(StartColor);
      { Calculate the difference between begin and end RGB values }
      RGBDelta[0] := GetRValue(EndColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(EndColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(EndColor) - StartRGB[2];
    end;
    fdBottomToTop, fdRightToLeft:
    begin
      { Set the Red, Green and Blue colors }
      { Reverse of TopToBottom and LeftToRight directions }
      StartRGB[0] := GetRValue(EndColor);
      StartRGB[1] := GetGValue(EndColor);
      StartRGB[2] := GetBValue(EndColor);
      { Calculate the difference between begin and end RGB values }
      { Reverse of TopToBottom and LeftToRight directions }
      RGBDelta[0] := GetRValue(StartColor) - StartRGB[0];
      RGBDelta[1] := GetGValue(StartColor) - StartRGB[1];
      RGBDelta[2] := GetBValue(StartColor) - StartRGB[2];
    end;
  end; {case}
  { Calculate the color band's coordinates }
  ColorBand := ARect;
  if Direction in [fdTopToBottom, fdBottomToTop] then
  begin
    Colors := Max(2, Min(Colors, HeightOf(ARect)));
    Delta := HeightOf(ARect) div Colors;
  end
  else
  begin
    Colors := Max(2, Min(Colors, WidthOf(ARect)));
    Delta := WidthOf(ARect) div Colors;
  end;
  with Canvas.Pen do
  begin { Set the pen style and mode }
    Style := psSolid;
    Mode := pmCopy;
  end;
  { Perform the fill }
  if Delta > 0 then
    for I := 0 to Colors do
    begin
      case Direction of
        { Calculate the color band's top and bottom coordinates }
        fdTopToBottom, fdBottomToTop:
        begin
          ColorBand.Top := ARect.Top + I * Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
        { Calculate the color band's left and right coordinates }
        fdLeftToRight, fdRightToLeft:
        begin
          ColorBand.Left := ARect.Left + I * Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
      end; {case}
      { Calculate the color band's color }
      Brush := CreateSolidBrush(RGB(
        StartRGB[0] + MulDiv(I, RGBDelta[0], Colors - 1),
        StartRGB[1] + MulDiv(I, RGBDelta[1], Colors - 1),
        StartRGB[2] + MulDiv(I, RGBDelta[2], Colors - 1)));
      FillRect(Canvas.Handle, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  if Direction in [fdTopToBottom, fdBottomToTop] then
    Delta := HeightOf(ARect) mod Colors
  else
    Delta := WidthOf(ARect) mod Colors;
  if Delta > 0 then
  begin
    case Direction of
      { Calculate the color band's top and bottom coordinates }
      fdTopToBottom, fdBottomToTop:
      begin
        ColorBand.Top := ARect.Bottom - Delta;
        ColorBand.Bottom := ColorBand.Top + Delta;
      end;
      { Calculate the color band's left and right coordinates }
      fdLeftToRight, fdRightToLeft:
      begin
        ColorBand.Left := ARect.Right - Delta;
        ColorBand.Right := ColorBand.Left + Delta;
      end;
    end; {case}
    case Direction of
      fdTopToBottom, fdLeftToRight:
        Brush := CreateSolidBrush(EndColor);
      else {fdBottomToTop, fdRightToLeft }
        Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(Canvas.Handle, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
var
  I: Integer;
begin
  Result := Text;
  I := 1;
  while (I <= Length(Text)) and (Canvas.TextWidth(Result) > MaxWidth) do
  begin
    Inc(I);
    Result := Copy(Text, 1, Max(0, Length(Text) - I)) + '...';
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else
    Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size,
    HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then
  begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;

function GetMemoSize(fpBlock: Pointer): Longint;
var
  hMem: THandle;
begin
  Result := 0;
  if fpBlock <> nil then
  begin
    hMem := GlobalHandle(fpBlock);
    if hMem <> 0 then
      Result := GlobalSize(hMem);
  end;
end;

function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,fpBlock1
        MOV     EDI,fpBlock2
        MOV     ECX,Size
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

{$IFNDEF RX_D5}
procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;
  P.Free;
end;
{$ENDIF}

{ Manipulate huge pointers routines by Ray Lischner, The Waite Group, Inc. }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PAnsiChar(HugePtr) + Amount;
end;

procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PAnsiChar(HugePtr) - Amount;
end;

function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
begin
  Result := PAnsiChar(HugePtr) + Amount;
end;

procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);
begin
  Move(SrcPtr^, DstPtr^, Amount);
end;

procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
var
  SrcPtr, DstPtr: PAnsiChar;
begin
  SrcPtr := PAnsiChar(Base) + Src * SizeOf(Pointer);
  DstPtr := PAnsiChar(Base) + Dst * SizeOf(Pointer);
  Move(SrcPtr^, DstPtr^, Size * SizeOf(Pointer));
end;

{ String routines }

{$W+}
function GetEnvVar(const VarName: string): string;
var
  S: array[0..2048] of Char;
begin
  if GetEnvironmentVariable(PChar(VarName), S, Length(S) - 1) > 0 then
    Result := StrPas(S)
  else
    Result := '';
end;
{$W-}

{ function GetParamStr copied from SYSTEM.PAS unit of Delphi 2.0 }
function GetParamStr(P: PChar; var Param: string): PChar;
var
  Len: Integer;
  Buffer: array[Byte] of Char;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then
      Inc(P, 2{* SizeOf(Char)})
    else
      Break;
  end;
  Len := 0;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function ParamCountFromCommandLine(CmdLine: PChar): Integer;
var
  S: string;
  P: PChar;
begin
  P := CmdLine;
  Result := 0;
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then
      Break;
    Inc(Result);
  end;
end;

function ParamStrFromCommandLine(CmdLine: PChar; Index: Integer): string;
var
  P: PChar;
begin
  P := CmdLine;
  while True do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then
      Break;
    Dec(Index);
  end;
end;

procedure SplitCommandLine(const CmdLine: string; var ExeName, Params: string);
var
  Buffer: PChar;
  Cnt, I: Integer;
  S: string;
begin
  ExeName := '';
  Params := '';
  Buffer := StrPAlloc(CmdLine);
  try
    Cnt := ParamCountFromCommandLine(Buffer);
    if Cnt > 0 then
    begin
      ExeName := ParamStrFromCommandLine(Buffer, 0);
      for I := 1 to Cnt - 1 do
      begin
        S := ParamStrFromCommandLine(Buffer, I);
        if Pos(' ', S) > 0 then
          S := '"' + S + '"';
        Params := Params + S;
        if I < Cnt - 1 then
          Params := Params + ' ';
      end;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

function AnsiUpperFirstChar(const S: string): string;
{$IFNDEF RX_D12}
var
  Temp: string[1];
{$ENDIF}
begin
  Result := AnsiLowerCase(S);
  if S <> '' then
{$IFDEF RX_D12}
    Result[1]:= ToUpper(Result[1]);
{$ELSE}
  begin
    Temp := Result[1];
    Temp := AnsiUpperCase(Temp);
    Result[1] := Char(Temp[1]);
  end;
{$ENDIF}
end;

function StrPAlloc(const S: string): PChar;
begin
  Result := StrPCopy(StrAlloc(Length(S) + 1), S);
end;

function StringToPChar(var S: string): PChar;
begin
  Result := PChar(S);
end;

function DropT(const S: string): string;
begin
  if (UpCase(S[1]) = 'T') and (Length(S) > 1) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

{ Cursor routines }

{$IFNDEF RX_D3}
const
  RT_ANICURSOR = MakeIntResource(21);
{$ENDIF}
function LoadAniCursor(Instance: THandle; ResID: PChar): HCursor;
{ Unfortunately I don't know how we can load animated cursor from
  executable resource directly. So I write this routine using temporary
  file and LoadCursorFromFile function. }
var
  S: TFileStream;
  Path, FileName: array[0..MAX_PATH] of Char;
  Rsrc: HRSRC;
  Res: THandle;
  Data: Pointer;
begin
  Result := 0;
  Rsrc := FindResource(Instance, ResID, RT_ANICURSOR);
  if Rsrc <> 0 then
  begin
    Win32Check(GetTempPath(MAX_PATH, Path) <> 0);
    Win32Check(GetTempFileName(Path, 'ANI', 0, FileName) <> 0);
    try
      Res := LoadResource(Instance, Rsrc);
      try
        Data := LockResource(Res);
        if Data <> nil then
        try
          S := TFileStream.Create(StrPas(FileName), fmCreate);
          try
            S.WriteBuffer(Data^, SizeOfResource(Instance, Rsrc));
          finally
            S.Free;
          end;
          Result := LoadCursorFromFile(FileName);
        finally
          UnlockResource(Res);
        end;
      finally
        FreeResource(Res);
      end;
    finally
      Windows.DeleteFile(FileName);
    end;
  end;
end;

function DefineCursor(Instance: THandle; ResID: PChar): TCursor;
var
  Handle: HCursor;
begin
  Handle := LoadCursor(Instance, ResID);
  if Handle = 0 then
    Handle := LoadAniCursor(Instance, ResID);
  if Handle = 0 then
    ResourceNotFound(ResID);
  for Result := 100 to High(TCursor) do { Look for an unassigned cursor index }
    if (Screen.Cursors[Result] = Screen.Cursors[crDefault]) then
    begin
      Screen.Cursors[Result] := Handle;
      Exit;
    end;
  DestroyCursor(Handle);
  raise EOutOfResources.Create(ResStr(SOutOfResources));
end;

const
  WaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := WaitCursor;
  end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
  begin
    Dec(WaitCount);
    if WaitCount = 0 then
      Screen.Cursor := SaveCursor;
  end;
end;

{ Grid drawing }

const
  DrawBitmap: TBitmap = nil;

procedure UsesBitmap;
begin
  if DrawBitmap = nil then
    DrawBitmap := TBitmap.Create;
end;

procedure ReleaseBitmap; far;
begin
  if DrawBitmap <> nil then
    DrawBitmap.Free;
  DrawBitmap := nil;
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; WordWrap: Boolean
  {$IFDEF RX_D4}; ARightToLeft: Boolean = False {$ENDIF});
const
  AlignFlags: array [TAlignment] of Integer =
    (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
     DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  WrapFlags: array[Boolean] of Integer = (0, DT_WORDBREAK);
{$IFDEF RX_D4}
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
{$ENDIF}
var
  B, R: TRect;
  I, Left: Integer;
begin
  UsesBitmap;
  I := ColorToRGB(ACanvas.Brush.Color);
  if not WordWrap and (Integer(GetNearestColor(ACanvas.Handle, I)) = I) and
    (Pos(#13, Text) = 0) then
  begin { Use ExtTextOut for solid colors }
{$IFDEF RX_D4}
    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
{$ENDIF}
    case Alignment of
      taLeftJustify: Left := ARect.Left + DX;
      taRightJustify: Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
{$IFDEF RX_D4}
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
{$ELSE}
    ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY, ETO_OPAQUE or
      ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil);
{$ENDIF}
  end
  else
  begin { Use FillRect and DrawText for dithered colors }
{$IFDEF RX_D3}
    DrawBitmap.Canvas.Lock;
    try
{$ENDIF}
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1,
          Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
{$IFDEF RX_D4}
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]
          or RTL[ARightToLeft] or WrapFlags[WordWrap]);
{$ELSE}
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlags[Alignment] or WrapFlags[WordWrap]);
{$ENDIF}
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
{$IFDEF RX_D3}
    finally
      DrawBitmap.Canvas.Unlock;
    end;
{$ENDIF}
  end;
end;

{$IFDEF RX_D4}

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean; ARightToLeft: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify: H := MinOffs;
    vaCenter:
      with THack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top - Canvas.TextHeight('W')) div 2);
  else {vaBottomJustify}
      with THack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top - Canvas.TextHeight('W'));
  end;
  WriteText(THack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap,
    ARightToLeft);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; ARightToLeft: Boolean);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter, ARightToLeft);
end;

{$ENDIF}

procedure DrawCellTextEx(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment; WordWrap: Boolean);
const
  MinOffs = 2;
var
  H: Integer;
begin
  case VertAlign of
    vaTopJustify: H := MinOffs;
    vaCenter:
      with THack(Control) do
        H := Max(1, (ARect.Bottom - ARect.Top - Canvas.TextHeight('W')) div 2);
    else {vaBottomJustify}
      with THack(Control) do
        H := Max(MinOffs, ARect.Bottom - ARect.Top - Canvas.TextHeight('W'));
  end;
  WriteText(THack(Control).Canvas, ARect, MinOffs, H, S, Align, WordWrap);
end;

procedure DrawCellText(Control: TCustomControl; ACol, ARow: Longint;
  const S: string; const ARect: TRect; Align: TAlignment;
  VertAlign: TVertAlignment);
begin
  DrawCellTextEx(Control, ACol, ARow, S, ARect, Align, VertAlign,
    Align = taCenter);
end;

procedure DrawCellBitmap(Control: TCustomControl; ACol, ARow: Longint;
  Bmp: TGraphic; Rect: TRect);
begin
  Rect.Top := (Rect.Bottom + Rect.Top - Bmp.Height) div 2;
  Rect.Left := (Rect.Right + Rect.Left - Bmp.Width) div 2;
  THack(Control).Canvas.Draw(Rect.Left, Rect.Top, Bmp);
end;

{ TScreenCanvas }

destructor TScreenCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

procedure TScreenCanvas.CreateHandle;
begin
  if FDeviceContext = 0 then
    FDeviceContext := GetDC(0);
  Handle := FDeviceContext;
end;

procedure TScreenCanvas.FreeHandle;
begin
  if FDeviceContext <> 0 then
  begin
    Handle := 0;
    ReleaseDC(0, FDeviceContext);
    FDeviceContext := 0;
  end;
end;

procedure TScreenCanvas.SetOrigin(X, Y: Integer);
var
  FOrigin: TPoint;
begin
  SetWindowOrgEx(Handle, -X, -Y, @FOrigin);
end;

procedure RaiseWin32Error(ErrorCode: DWORD);
{$IFDEF RX_D3}
var
  {$IFDEF RX_D6}      // Polaris
  Error: EOSError;
  {$ELSE}
  Error: EWin32Error;
  {$ENDIF}
{$ENDIF}
begin
  if ErrorCode <> ERROR_SUCCESS then
  begin
{$IFDEF RX_D3}
    {$IFDEF RX_D6}      // Polaris
    Error := EOSError.CreateFmt(SOSError, [ErrorCode, SysErrorMessage(ErrorCode)]);
    {$ELSE}
    Error := EWin32Error.CreateFmt(SWin32Error, [ErrorCode,
      SysErrorMessage(ErrorCode)]);
    {$ENDIF}
    Error.ErrorCode := ErrorCode;
    raise Error;
{$ELSE}
    raise Exception.CreateFmt('%s (%d)', [SysErrorMessage(ErrorCode), ErrorCode]);
{$ENDIF}
  end;
end;

{ Win32Check is used to check the return value of a Win32 API function
  which returns a BOOL to indicate success. }

{$IFNDEF RX_D3}
function Win32Check(RetVal: Bool): Bool;
var
  LastError: DWORD;
begin
  if not RetVal then
  begin
    LastError := GetLastError;
    raise Exception.CreateFmt('%s (%d)', [SysErrorMessage(LastError), LastError]);
  end;
  Result := RetVal;
end;
{$ENDIF RX_D3}

function CheckWin32(OK: Boolean): Boolean;
begin
  Result := Win32Check(Ok);
end;

{$IFNDEF RX_D3}
function ResStr(Ident: Cardinal): string;
begin
  Result := LoadStr(Ident);
end;
{$ELSE}
function ResStr(const Ident: string): string;
begin
  Result := Ident;
end;
{$ENDIF}

{ Check if this is the active Windows task }
{ Copied from implementation of FORMS.PAS  }

type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: HWnd;
    Found: Boolean;
  end;

function CheckTaskWindow(Window: HWnd; Data: Longint): WordBool;
  stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data)^.FocusWnd = Window then
  begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;

function IsForegroundTask: Boolean;
var
  Info: TCheckTaskInfo;
begin
  Info.FocusWnd := GetActiveWindow;
  Info.Found := False;
  EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
end;

function GetWindowsVersion: string;
const
  sWindowsVersion = 'Windows %s %d.%.2d.%.3d %s';
var
  Ver: TOsVersionInfo;
  Platform: string[4];
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s: Platform := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Platform := '98'
          else
            Platform := '95';
        end;
      VER_PLATFORM_WIN32_NT: Platform := 'NT';
    end;
    Result := Trim(Format(sWindowsVersion, [Platform, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;

initialization

finalization
  ReleaseBitmap;

end.
