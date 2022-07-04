unit TBXGraphics;

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Types, Classes, SysUtils, Graphics, CommCtrl, ImgList, Dialogs;

const
  { PixelBlockTransfer flags }
  PBT_COPY             = 0;
  PBT_EXCHANGE         = 1;

  { Render flags }
  RF_BLEND             = 0;
  RF_EXTRACT           = 1;

  PBR_OP_MASK          = $0FF;
  PBR_COPY             = $000;
  PBR_BLEND            = $001;
  PBR_RESAMPLE_MASK    = $F00;
  PBR_NEAREST          = $000;
  PBR_LINEAR           = $100;

type
  PBitmapV4Header = ^TBitmapV4Header;
  {$EXTERNALSYM BITMAPV4HEADER}
  BITMAPV4HEADER = packed record
    bV4Size: DWORD;
    bV4Width: Longint;
    bV4Height: Longint;
    bV4Planes: Word;
    bV4BitCount: Word;
    bV4Compression: DWORD;
    bV4SizeImage: DWORD;
    bV4XPelsPerMeter: Longint;
    bV4YPelsPerMeter: Longint;
    bV4ClrUsed: DWORD;
    bV4ClrImportant: DWORD;
    bV4RedMask: DWORD;
    bV4GreenMask: DWORD;
    bV4BlueMask: DWORD;
    bV4AlphaMask: DWORD;
    bV4CSType: DWORD;
    bV4Endpoints: TCIEXYZTriple;
    bV4GammaRed: DWORD;
    bV4GammaGreen: DWORD;
    bV4GammaBlue: DWORD;
  end;
  TBitmapV4Header = BITMAPV4HEADER;

  { TDIB32 }
  { TDIB32 is a simple wrapper for 32bpp device independent DIBs }

  PRGBQuad = ^TRGBQuad;
  TRGBQuad = Cardinal;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;
  TRGBQuadDynArray = array of TRGBQuad;

  PPixelData32 = ^TPixelData32;
  TPixelData32 = packed record
    Bits: PRGBQuad;
    ContentRect: TRect;
    RowStride: Integer;
  end;

  EDIB32Error = class(Exception);

  TColorEffects = class;

  TDIB32 = class
  private
    FBitmapInfo: TBitmapInfo;
    FBits: PRGBQuadArray;
    FContentRect: TRect;
    FDC: HDC;
    FHeight: Integer;
    FHandle: HBITMAP;
    FWidth: Integer;
  protected
    procedure Consume(Src: TDIB32);
    function  Extract: TDIB32;
    function  GetPixelData(out PD: TPixelData32): Boolean;
    function  InternalCreateIcon(AsIcon: Boolean; HotSpotX, HotSpotY: Integer): HICON;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TDIB32); virtual;
    procedure BlendTo(const DstData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte = $FF); overload;
    procedure BlendTo(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte = $FF); overload;
    procedure BlendTo(DstDIB: TDIB32; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte = $FF); overload;
    procedure Clear(Clr: TRGBQuad);
    procedure CopyFrom(const SrcData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean = False);
    procedure CopyTo(const DstData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean = False); overload;
    procedure CopyTo(DstDC: HDC; const DstRect: TRect); overload;
    procedure CopyTo(DstDC: HDC; DstX, DstY: Integer); overload;
    procedure CopyTo(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure CopyTo(DstDC: HDC; const DstRect, SrcRect: TRect); overload;
    procedure CopyTo(DstDIB: TDIB32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean = False); overload;
    function  CreateCursor(HotSpotX, HotSpotY: Integer): HCURSOR;
    function  CreateIcon: HICON;
    class procedure Error(ResStringRec: PResStringRec);
    class procedure ErrorFmt(ResStringRec: PResStringRec; const Args: array of const);
    procedure RenderEffectTo(const DstData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; ColorEffects: TColorEffects); overload;
    procedure RenderEffectTo(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect; ColorEffects: TColorEffects); overload;
    procedure RenderEffectTo(DstDIB: TDIB32; DstX, DstY: Integer; const SrcRect: TRect; ColorEffects: TColorEffects); overload;
    procedure RenderIcon(Icon: HICON; X, Y, W, H: Integer; Flags: Cardinal);
    procedure RenderImage(ImageList: HIMAGELIST; ImageIndex: Integer; X, Y, W, H: Integer; Flags: Cardinal);
    procedure SetSize(W, H: Integer; KeepContent: Boolean = False);
    procedure SetAlpha(Rect: TRect; Alpha: Byte);
    procedure SolidFillRect(Rect: TRect; Clr: TRGBQuad);
    procedure StretchTo(DstData: TPixelData32; const DstRect: TRect; const SrcRect: TRect; Flags: Cardinal); overload;
    procedure StretchTo(DstDIB: TDIB32; const DstRect: TRect; const SrcRect: TRect; Flags: Cardinal); overload;
    procedure StretchTo(DstDC: HDC; const DstRect: TRect; const SrcRect: TRect; Flags: Cardinal); overload;
    procedure VCLDrawDisabled(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect);
    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property Bits: PRGBQuadArray read FBits;
    property ContentRect: TRect read FContentRect;
    property DC: HDC read FDC;
    property Handle: HBITMAP read FHandle;
    property Height: Integer read FHeight;
    property RowStride: Integer read FWidth;
    property Width: Integer read FWidth;
  end;

  TPNGBitmap = class(TGraphic)
  private
    FDIB: TDIB32;
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property DIB: TDIB32 read FDIB;
  end;

  TOverlayIndex = 1..15;

  TDIBList32 = class
  private
    FCapacity: Integer;
    FDIB: TDIB32;
    FCount: Integer;
    FGrow: Integer;
    FOverlays: array [TOverlayIndex] of Integer;
    FImageFlags: TCardinalDynArray;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetCapacity(Value: Integer);
  protected
    procedure InternalDelete(Index, NumImages: Integer);
    procedure InternalInsert(Index, NumImages: Integer);
    function  PrepareImport(Index: Integer; ReplaceExisting: Boolean): Integer;
  public
    constructor Create(AWidth, AHeight: Integer; AGrow: Integer = 0);
    destructor Destroy; override;
    procedure APIDrawEx(Index: Integer; DC: HDC; X, Y, DX, DY: Integer; Bk, Fg: TColorRef; Style: Cardinal);
    procedure Assign(Src: TDIBList32);
    procedure Blend(ImageIndex: Integer; DC: HDC; X, Y: Integer; MasterAlpha: Byte);
    procedure DrawEffect(ImageIndex: Integer; DC: HDC; X, Y: Integer; ColorEffects: TColorEffects);
    function  ImportIcon(Icon: HICON; ImageIndex: Integer = -1; ReplaceExisting: Boolean = False): Integer; // also works for cursors
    function  ImportImage(DIB: TDIB32; ImageIndex: Integer = -1; ReplaceExisting: Boolean = False): Integer; overload;
    function  ImportImage(DIB: TDIB32; SrcRect: TRect; ImageIndex: Integer = -1; ReplaceExisting: Boolean = False): Integer; overload;
    function  ImportImage(ImageList: HIMAGELIST; SrcIndex: Integer; DstIndex: Integer = -1; ReplaceExisting: Boolean = False): Integer; overload;
    procedure Delete(Index: Integer; NumImages: Integer = 1);
    procedure DeleteAll;
    function  Equal(List: TDIBList32): Boolean;
    class procedure Error(ResStringRec: PResStringRec);
    class procedure ErrorFmt(ResStringRec: PResStringRec; const Args: array of const);
    procedure Exchange(Index1, Index2: Integer);
    function  GetCursor(ImageIndex: Integer; HotSpotX, HotSpotY: Integer): HCURSOR;
    function  GetImage(ImageIndex: Integer): TDIB32;
    function  GetImageRect(ImageIndex: Integer): TRect;
    function  GetIcon(ImageIndex: Integer): HICON;
    procedure Move(CurIndex, NewIndex: Integer; NumImages: Integer = 1);
    procedure SetOverlayImage(ImageIndex: Integer; OverlayIndex: TOverlayIndex);
    procedure SetSize(NewWidth, NewHeight: Integer; NewGrow: Integer = 0);
    procedure VCLDrawDisabled(Index: Integer; DC: HDC; X, Y: Integer);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property DIB: TDIB32 read FDIB;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TTBXImageList = class(TCustomImageList)
  private
    FDIBList: TDIBList32;
    FChangeLock: Integer;
    FImagesLoaded: Boolean;
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Change; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure EndUpdate;
{$IFDEF JR_D7}
    procedure ReadData(Stream: TStream); override;
{$ELSE}
    procedure ReadData(Stream: TStream);
{$ENDIF}
    procedure ReadPNG(Stream: TStream); virtual;
    procedure WritePNG(Stream: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  Add(Image: TDIB32): Integer;
    function  AddIcon(Image: TIcon): Integer; overload;
    function  AddIcon(IconHandle: HIcon): Integer; overload;
    function  AddImage(Value: TCustomImageList; Index: Integer): Integer;
    procedure AddImages(Value: TCustomImageList);
    function  AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Image, Mask: TBitmap);
    procedure InsertIcon(Index: Integer; Image: TIcon);
    procedure InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Replace(Index: Integer; Image, Mask: TBitmap);
    procedure ReplaceIcon(Index: Integer; Image: TIcon);
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    property DIBList: TDIBList32 read FDIBList;
  published
    property Height;
    property Width;
    property Onchange;
  end;

{ Color effects }

  TColorEffectProc = procedure(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);

  TColorEffectItem = packed record
    ID: Integer;
    Param: Cardinal;
    Proc: TColorEffectProc;
  end;
  TColorEffectItemDynArray = packed array of TColorEffectItem;

  TColorEffects = class
  protected
    Effects: TColorEffectItemDynArray;
  public
    function  AddEffect(ID, Param: Cardinal): Boolean;
    procedure Clear;
    procedure Transform(var Src: Cardinal; const Dst: Cardinal); overload;
    procedure Transform(var SrcFirst, DstFirst: Cardinal; Count: Integer); overload;
  end;

function RegisterColorEffect(ID: Integer; EffectProc: TColorEffectProc): Integer;

{ memory fill/move routines optimized for 32-bit data transfer }
procedure FillLongword(var Dst; Count: Integer; Value: Longword);
procedure MoveLongword(const Src; var Dst; Count: Integer);
procedure BidiMoveLongword(const Src; var Dst; Count: Integer);
function ColorRefToRGB(C: TColorRef): Cardinal;
function ColorRefToARGB(C: TColorRef): Cardinal;


procedure PixelBlockTransfer(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  Flags: Cardinal);

procedure PixelBlockResample(
  const DstData: TPixelData32;
  const DstRect: TRect;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  Flags: Cardinal);

procedure PixelBlockBlend(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  MasterAlpha: Byte);

procedure PixelBlockEffect(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  ColorEffects: TColorEffects);

const
  CLE_INVERT     = 1;
  CLE_DESATURATE = 2;
  CLE_MULTIPLY   = 3;
  CLE_ADD        = 4;
  CLE_OPACITY    = 5;
  CLE_BLEND      = 6;
  CLE_DARKSHADOW = 7;

{ Streaming }
const
  GS_UNKNOWN = 0;
  GS_BMP = 1;
  GS_PNG = 2;
  GS_ICO = 3;

function CheckGraphicStream(Stream: TStream): Integer;
procedure LoadBMPGraphic(Stream: TStream; DIB: TDIB32);
procedure LoadPNGGraphic(Stream: TStream; DIB: TDIB32);
procedure LoadICOGraphic(Stream: TStream; DIB: TDIB32);
procedure SavePNGGraphic(Stream: TStream; DIB: TDIB32); overload;
procedure SavePNGGraphic(const FileName: WideString; DIB: TDIB32); overload;


{ Support for monochromatic glyphs }
type
  TMonoGlyph = class
  protected
    Width: Integer;
    Height: Integer;
    Bitmap, OldBitmap: HBitmap;
    BitmapDC: HDC;
  public
    constructor Create(AWidth, AHeight: Integer; const Bits);
    destructor Destroy; override;
    procedure Draw(DC: HDC; X, Y: Integer; Color: TColor);
    procedure DrawTiled(DC: HDC; R: TRect; Color: TColor);
  end;

{$IFNDEF JR_D7}
type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

const
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;
{$ENDIF}

resourcestring
  SInvalidParameters = 'Invalid parameters';
  SInvalidImageDimensions = 'Invalid image dimensions';

implementation

uses
  Consts, RTLConsts, Math, TBXGraphicsPNG;

function CreateDIBSectionV4(DC: HDC; const BMI: TBitmapV4Header; Usage: UINT;
   var Bits: Pointer; Handle: THandle; Offset: DWORD): HBITMAP; stdcall; external gdi32 name 'CreateDIBSection';

type
  PColorEffectInfo = ^TColorEffectInfo;
  TColorEffectInfo = record
    ID: Integer;
    ApplyProc: TColorEffectProc;
  end;

var
  ColorEffectRegistry: array of TColorEffectInfo;

var
  DIV_TABLE: packed array [Byte, Byte] of Byte;           // [$00..$FF]
  ALPHAMUL_TABLE: packed array [Byte, Byte] of Cardinal;  // [$00000000..$FF000000]

type
  PDivTableLine = ^TDivTableLine;
  TDivTableLine = packed array [Byte] of Byte;

procedure PrepareTables;
var
  C, A, R: Cardinal;
begin
  for A := 0 to $FF do
    for C := 0 to $FF do
    begin
      if C >= A then R := $FF
      else if A = 0 then R := $7F
      else R := Round(C * $FF / A);
      DIV_TABLE[A, C] := R;
      ALPHAMUL_TABLE[A, C] := Round(C * A / 255) shl 24;
    end;
end;

{ Misc. routines }

procedure FillLongword(var Dst; Count: Integer; Value: Longword);
asm
{ eax = Dst;  edx = Count; ecx = Value }
    push edi
    mov edi,eax
    mov eax,ecx
    mov ecx,edx
    test ecx,ecx
    js @1
    rep stosd
@1: pop edi
end;

procedure MoveLongword(const Src; var Dst; Count: Integer);
asm
// eax = Src; edx = Dst; ecx = Count
    push esi
    push edi
    mov esi,eax
    mov edi,edx
    mov eax,ecx
    cmp edi,esi
    je @1
    rep movsd
@1: pop edi
    pop esi
end;

procedure BidiMoveLongword(const Src; var Dst; Count: Integer);
asm
// eax = Src; edx = Dst; ecx = Count
    push esi
    push edi
    mov esi,eax
    mov edi,edx
    mov eax,ecx
    cmp edi,esi
    ja @1
    je @2
    rep movsd
    jmp @2
@1: lea esi,[esi+ecx*4-4]
    lea edi,[edi+ecx*4-4]
    std
    rep movsd
    cld
@2: pop edi
    pop esi
end;

function ColorRefToRGB(C: TColorRef): Cardinal;
asm
    bswap eax
    shr eax, 8
    and eax, $00FFFFFF
end;

function ColorRefToARGB(C: TColorRef): Cardinal;
asm
    bswap eax
    shr eax, 8
    or eax, $FF000000
end;

{ these are some parts from the upcoming g32 release }
type
  TBlendContextLL = record
    SrcColor: Cardinal;
    DstColor: Cardinal;
    ResColor: Cardinal;
    OP: Cardinal;
    SrcPremultRB: Cardinal;
    SrcPremultG:  Cardinal;
    DstPremultRB: Cardinal;
    DstPremultG:  Cardinal;
  end;

procedure BlendPixel_ATOP_LL(var Dst: Cardinal; Src: Cardinal);
var
  Fs, Fd: Cardinal;
begin
  if Src > $00FFFFFF then
    if Src < $FF000000 then
    begin
      Fs := Src shr 24 + Src shr 31;
      Fd := 256 - Fs;
      Dst :=
        (((Fs * (Src and $FF00FF) + Fd * (Dst and $FF00FF) + $7F007F) and $FF00FF00) or
        ((Fs * (Src and $00FF00) + Fd * (Dst and $00FF00) + $007F00) and $00FF0000)) shr 8;
    end
    else Dst := Src;
end;

procedure BlendPixel_ATOP_LLC(var Context: TBlendContextLL; var Dst: Cardinal; Src: Cardinal);
var
  Fs, Fd: Cardinal;
begin
  if Src > $00FFFFFF then
    if Src >= $FF000000 then Dst := Src
    else if (Src = Context.SrcColor) and (Dst = Context.DstColor) then Dst := Context.ResColor
    else
    begin
      Fs := Src shr 24 + Src shr 31;
      if (Src <> Context.SrcColor) or (Fs <> Context.OP) then
      begin
        Context.SrcPremultRB := Fs * (Src and $FF00FF) + $7F007F;
        Context.SrcPremultG  := Fs * (Src and $00FF00) + $007F00;
        Context.SrcColor := Src;
      end;
      if (Dst <> Context.DstColor) or (Fs <> Context.OP) then
      begin
        Fd := 256 - Fs;
        Context.DstPremultRB := Fd * (Dst and $FF00FF);
        Context.DstPremultG  := Fd * (Dst and $00FF00);
        Context.DstColor := Dst;
      end;
      Dst :=
        (((Context.SrcPremultRB + Context.DstPremultRB) and $FF00FF00) or
        ((Context.SrcPremultG + Context.DstPremultG) and $00FF0000)) shr 8;
      Context.OP := Fs;
      Context.ResColor := Dst;
    end;
end;

procedure BlendPixels_ATOP_LL(var SrcFirst, DstFirst: Cardinal; Count: Integer; MasterAlpha: Byte);
var
  AlphaLine: PRGBQuadArray;
  Context: TBlendContextLL;
  Src, I: Cardinal;
begin
  if (MasterAlpha = 0) or (Count = 0) then Exit;
  if MasterAlpha = $FF then
  begin
    if Count > 1 then
    begin
      Context.SrcColor := 0;
      Context.DstColor := 0;
      for I := 0 to Count - 1 do BlendPixel_ATOP_LLC(Context, PRGBQuadArray(@DstFirst)[I], PRGBQuadArray(@SrcFirst)[I]);
    end
    else BlendPixel_ATOP_LL(DstFirst, SrcFirst);
  end
  else
  begin
    AlphaLine := @ALPHAMUL_TABLE[MasterAlpha, 0];
    if Count > 1 then
    begin
      Context.SrcColor := 0;
      Context.DstColor := 0;
      for I := 0 to Count - 1 do
      begin
        Src := PRGBQuadArray(@SrcFirst)[I];
        BlendPixel_ATOP_LLC(Context, PRGBQuadArray(@DstFirst)[I], AlphaLine[Src shr 24] or (Src and $00FFFFFF));
      end;
    end
    else BlendPixel_ATOP_LL(DstFirst, AlphaLine[SrcFirst shr 24] or (SrcFirst and $00FFFFFF));
  end;
end;

procedure PixelBlockTransfer(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  Flags: Cardinal);
var
  R: TRect;
  SrcIndex, DstIndex, Count: Integer;
  RowIndex: Integer;
  ExchangeBuffer: TRGBQuadDynArray;
begin
  if not (Assigned(SrcData.Bits) and Assigned(DstData.Bits)) then Exit;
  IntersectRect(R, SrcRect, SrcData.ContentRect);
  OffsetRect(R, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R, R, DstData.ContentRect);
  if IsRectEmpty(R) then Exit;
  DstIndex := R.Left + R.Top * DstData.RowStride;
  SrcIndex := R.Left + SrcRect.Left - DstX + (R.Top + SrcRect.Top - DstY) * SrcData.RowStride;

  Count := R.Right - R.Left;
  if Flags and PBT_EXCHANGE = 0 then
    for RowIndex := R.Top to R.Bottom - 1 do
    begin
      BidiMoveLongword(PRGBQuadArray(SrcData.Bits)[SrcIndex], PRGBQuadArray(DstData.Bits)[DstIndex], Count);
      Inc(SrcIndex, SrcData.RowStride);
      Inc(DstIndex, DstData.RowStride);
    end
  else
  begin
    SetLength(ExchangeBuffer, Count);
    for RowIndex := R.Top to R.Bottom - 1 do
    begin
      MoveLongWord(PRGBQuadArray(DstData.Bits)[DstIndex], ExchangeBuffer[0], Count);
      MoveLongWord(PRGBQuadArray(SrcData.Bits)[SrcIndex], PRGBQuadArray(DstData.Bits)[DstIndex], Count);
      MoveLongWord(ExchangeBuffer[0], PRGBQuadArray(SrcData.Bits)[SrcIndex], Count);
      Inc(SrcIndex, SrcData.RowStride);
      Inc(DstIndex, DstData.RowStride);
    end;
  end;
end;

type
  TPointRec = packed record Pos: Integer; Weight: Cardinal; end;
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;

function BuildMappingTable(
  DstLo, DstHi: Integer;
  DstClipLo, DstClipHi: Integer;
  SrcLo, SrcHi: Integer): TMappingTable;
var
  SrcW, DstW, ClipW: Integer;
  Scale, RevScale: Single;
  Center, Pos: Single;
  Count: Integer;
  Left, Right: Integer;
  I, J, K, P: Integer;
  Weight: Integer;
  W, Err: Single;

  function LinearFilter(Value: Single): Single;
  begin
    if Value < -1 then Result := 0
    else if Value < 0 then Result := 1 + Value
    else if Value < 1 then Result := 1 - Value
    else Result := 0;
  end;

begin
  Result := nil;
  SrcW := SrcHi - SrcLo;
  if SrcW = 0 then Exit;
  DstW := DstHi - DstLo;
  if DstW = 0 then Exit;
  ClipW := DstClipHi - DstClipLo;
  SetLength(Result, ClipW);
  if ClipW = 0 then Exit;

  Scale := DstW / SrcW;
  RevScale := 1 / Scale;

  K := 0;
  if Scale <= 1 then
  begin
    for I := 0 to ClipW - 1 do
    begin
      Pos := SrcLo + (I - DstLo + DstClipLo) * RevScale;
      Left := Floor(Pos);
      Right := Floor(Pos + RevScale);
      Count := 256;
      Err := 0;
      for J := Left to Right do
      begin
        if J = Left then W := Left + 1 - Pos
        else if J = Right then W := Pos + RevScale - Right
        else W := 1;

        W := 256 * Scale * W;
        Weight := Round(W + Err);
        Err := Err + W - Weight;
        Assert(Abs(Err) < 0.7);

        if Weight <> 0 then
        begin
          Dec(Count, Weight);
          P := Min(Max(J, SrcLo), SrcHi - 1);
          K := Length(Result[I]);
          if (K > 0) and (Result[I][K - 1].Pos = P) then Inc(Result[I][K - 1].Weight, Weight)
          else
          begin
            SetLength(Result[I], K + 1);
            Result[I][K].Pos := P;
            Result[I][K].Weight := Weight;
          end;
        end;
      end;
      if Length(Result[I]) = 0 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := (Left + Right) div 2;
        Result[I][0].Weight := 256;
      end
      else if Count <> 0 then
        Inc(Result[I][K div 2].Weight, Count);
    end;
  end
  else // scale > 1
  begin
    for I := 0 to ClipW - 1 do
    begin
      Center := SrcLo - 0.5 + (I - DstLo + DstClipLo + 0.5) * RevScale;
      Left := Floor(Center);
      if Left < SrcLo then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := SrcLo;
        Result[I][0].Weight := 256;
      end
      else if Left >= SrcHi - 1 then
      begin
        SetLength(Result[I], 1);
        Result[I][0].Pos := SrcHi - 1;
        Result[I][0].Weight := 256;
      end
      else
      begin
        SetLength(Result[I], 2);
        Weight := Round(256 * (Center - Left));
        Result[I][0].Pos := Left;
        Result[I][0].Weight := 256 - Weight;
        Result[I][1].Pos := Left + 1;
        Result[I][1].Weight := Weight;
      end;
    end;
  end;
end;

procedure PixelBlockShrink(
  const DstData: TPixelData32;
  const DstRect: TRect;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  Flags: Cardinal);
type
  TBufferEntry = packed record A, R, G, B: Cardinal; end;
  TQuadBuffer = array of TBufferEntry;
var
  DstClip: TRect;
  SrcW, SrcH, ClipW, ClipH: Integer;
  MapX, MapY: TMappingTable;
  ClusterX, ClusterY: TCluster;
  HorzBuffer: TQuadBuffer;
  Buffer: TRGBQuadDynArray;
  I, J, X, Y: Integer;
  C, Ca, Cr, Cg, Cb: Cardinal;
  W: Cardinal;
  DstLine, Dst, SrcLine: PRGBQuadArray;
  Blend: Boolean;
begin
  ClusterX := nil;
  ClusterY := nil;
  MapX := nil;
  MapY := nil;
  IntersectRect(DstClip, DstRect, DstData.ContentRect);
  ClipW := DstClip.Right - DstClip.Left;
  ClipH := DstClip.Bottom - DstClip.Top;
  if (ClipW <= 0) or (ClipH <= 0) then Exit;
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  if (SrcW <= 0) or (SrcH <= 0) then Exit;
  MapX := BuildMappingTable(DstRect.Left, DstRect.Right, DstClip.Left, DstClip.Right, 0, SrcW);
  MapY := BuildMappingTable(DstRect.Top, DstRect.Bottom, DstClip.Top, DstClip.Bottom, 0, SrcH);
  if (MapX = nil) or (MapY = nil) then Exit;

  SetLength(HorzBuffer, SrcW);

  Blend := Flags and PBR_OP_MASK = PBR_BLEND;
  if Blend then SetLength(Buffer, ClipW) else SetLength(Buffer, 0);

  DstLine := PRGBQuadArray(Integer(DstData.Bits) + (DstClip.Left + DstClip.Top * DstData.RowStride) shl 2);
  for J := 0 to ClipH - 1 do
  begin
    ClusterY := MapY[J];
    SrcLine := PRGBQuadArray(Integer(SrcData.Bits) + (SrcRect.Left + SrcRect.Top * SrcData.RowStride) shl 2);
    for I := 0 to SrcW - 1 do
    begin
      Ca := 0; Cr := 0; Cg := 0; Cb := 0;
      for Y := 0 to Length(ClusterY) - 1 do
      begin
        C := SrcLine[ClusterY[Y].Pos * SrcData.RowStride];
        W := ClusterY[Y].Weight;
        Inc(Ca, (C shr 24) * W);
        Inc(Cr, (C shr 16 and $FF) * W);
        Inc(Cg, (C shr 8 and $FF) * W);
        Inc(Cb, (C and $FF) * W);
      end;
      with HorzBuffer[I] do
      begin
        R := Cr;
        G := Cg;
        B := Cb;
        A := Ca;
      end;
      Inc(Integer(SrcLine), SizeOf(TRGBQuad));
    end;

    if Blend then Dst := @Buffer[0] else Dst := DstLine;

    for I := 0 to ClipW - 1 do
    begin
      ClusterX := MapX[I];
      Ca := 0; Cr := 0; Cg := 0; Cb := 0;
      for X := 0 to Length(ClusterX) - 1 do
      begin
        W := ClusterX[X].Weight;
        with HorzBuffer[ClusterX[X].Pos] do
        begin
          Inc(Ca, A * W);
          Inc(Cr, R * W);
          Inc(Cg, G * W);
          Inc(Cb, B * W);
        end;
      end;
      Ca := (Ca + $7F00) and $FF0000;
      Cr := (Cr + $7F00) and $FF0000;
      Cg := (Cg + $7F00) and $FF0000;
      Cb := (Cb + $7F00) and $FF0000;
      Dst[I] := Ca shl 8 or Cr or Cg shr 8 or Cb shr 16;
    end;

    if Blend then BlendPixels_ATOP_LL(Buffer[0], DstLine[0], ClipW, $FF);
    Inc(Integer(DstLine), DstData.RowStride shl 2);
  end;
end;


procedure PixelBlockResample(
  const DstData: TPixelData32;
  const DstRect: TRect;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  Flags: Cardinal);
var
  SrcW, SrcH, DstW, DstH, ClipW, ClipH: Integer;
  ScaleH, ScaleV, RevScaleH, RevScaleV: Single;
  DstClip: TRect;
  MapHorz, MapVert, BiasHorz, BiasVert: TIntegerDynArray;
  MappingHorz, MappingVert: TMappingTable;
  X, Y, P: Integer;
  T, TL, TR: Single;
  Interpolate, Blend: Boolean;
  DstLine, Dst, SrcLine: PRGBQuadArray;
  Buffer: TRGBQuadDynArray;
  WX, WX2, WY, WY2: Cardinal;
  SrcIndex, IndexL, IndexR: Integer;
  V, VT, VB, AL, RL, GL, BL, AR, RR, GR, BR, A, R, G, B: Cardinal;
begin
  { source clipping is not supported, SrcRect must lie entirely within SrcData.ContentRect }
  if (SrcRect.Left < SrcData.ContentRect.Left) or (SrcRect.Right > SrcData.ContentRect.Right) or
    (SrcRect.Top < SrcData.ContentRect.Top) or (SrcRect.Bottom > SrcData.ContentRect.Bottom) or
    (SrcRect.Right <= SrcRect.Left) or (SrcRect.Bottom <= SrcRect.Top) then Exit;

  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  if (DstW <= 0) or (DstH <= 0) then Exit;
  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;

  ScaleH := DstW / SrcW;
  ScaleV := DstH / SrcH;
  RevScaleH := 1 / ScaleH;
  RevScaleV := 1 / ScaleV;

  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    if Flags and PBR_OP_MASK = PBR_BLEND then
      PixelBlockBlend(DstData, DstRect.Left, DstRect.Top, SrcData, SrcRect, $FF)
    else
      PixelBlockTransfer(DstData, DstRect.Left, DstRect.Top, SrcData, SrcRect, PBT_COPY);
    Exit;
  end
  else if (ScaleH < 1.0) or (ScaleV < 1.0) then
  begin
    PixelBlockShrink(DstData, DstRect, SrcData, SrcRect, Flags);
    Exit;
  end;

  IntersectRect(DstClip, DstRect, DstData.ContentRect);
  if IsRectEmpty(DstClip) then Exit;
  ClipW := DstClip.Right - DstClip.Left;
  ClipH := DstClip.Bottom - DstClip.Top;

  Interpolate := Flags and PBR_LINEAR <> 0;
  SetLength(MapHorz, 0);
  SetLength(BiasHorz, 0);
  SetLength(MapVert, 0);
  SetLength(BiasVert, 0);

  if ScaleH <> 1.0 then
  begin
    SetLength(MapHorz, ClipW);
    SetLength(BiasHorz, ClipW);
    for X := 0 to ClipW - 1 do
    begin
      T := SrcRect.Left - 0.5 + (X + DstClip.Left - DstRect.Left + 0.5) * RevScaleH;
      if Interpolate then P := Floor(T) else P := Round(T);
      if P < SrcRect.Left then
      begin
        MapHorz[X] := SrcRect.Left;
        BiasHorz[X] := 256;
      end
      else if P >= SrcRect.Right - 1 then
      begin
        MapHorz[X] := SrcRect.Right - 1;
        BiasHorz[X] := 256;
      end
      else
      begin
        MapHorz[X] := P;
        if Interpolate then BiasHorz[X] := 256 - Round(Frac(T) * 256)
        else BiasHorz[X] := 256;
      end;
    end;
  end;

  if ScaleV <> 1.0 then
  begin
    SetLength(MapVert, ClipH);
    SetLength(BiasVert, ClipH);
    for Y := 0 to ClipH - 1 do
    begin
      T := SrcRect.Top - 0.5 + (Y + DstClip.Top - DstRect.Top + 0.5) * RevScaleV;
      if Interpolate then P := Floor(T) else P := Round(T);
      if P < SrcRect.Top then
      begin
        MapVert[Y] := SrcRect.Top;
        BiasVert[Y] := 255;
      end
      else if P >= SrcRect.Bottom - 1 then
      begin
        MapVert[Y] := SrcRect.Bottom - 1;
        BiasVert[Y] := 256;
      end
      else
      begin
        MapVert[Y] := P;
        if Interpolate then BiasVert[Y] := 256 - Round(Frac(T) * 256)
        else BiasVert[Y] := 256;
      end;
    end;
  end;

  Blend := Flags and PBR_BLEND = PBR_BLEND;
  if Blend then SetLength(Buffer, ClipW) else SetLength(Buffer, 0);

  DstLine := PRGBQuadArray(Integer(DstData.Bits) + (DstClip.Left + DstClip.Top * DstData.RowStride) shl 2);
  SrcLine := PRGBQuadArray(Integer(SrcData.Bits) + (SrcRect.Top * SrcData.RowStride) shl 2);

  for Y := 0 to ClipH - 1 do
  begin
    if Blend then Dst := @Buffer[0] else Dst := DstLine;

    if Interpolate then
    begin
      if MapVert <> nil then
      begin
        SrcLine := PRGBQuadArray(Integer(SrcData.Bits) + (MapVert[Y] * SrcData.RowStride shl 2));
        WY := BiasVert[Y];
        WY2 := 256 - WY;
      end
      else
      begin
        WY := 256;
        WY2 := 0;
      end;
      IndexL := -2;
      IndexR := -2;
      AL := 0; RL := 0; GL := 0; BL := 0;
      AR := 0; RR := 0; GR := 0; BR := 0;

      if WY >= 256 then
        for X := 0 to ClipW - 1 do
        begin
          if MapHorz <> nil then
          begin
            SrcIndex := MapHorz[X];
            WX := BiasHorz[X];
            WX2 := 256 - WX;
          end
          else
          begin
            SrcIndex := X + SrcRect.Left;
            WX := 256;
            WX2 := 0;
          end;

          if WX >= 256 then Dst[X] := SrcLine[SrcIndex]
          else
          begin
            if SrcIndex <> IndexL then
            begin
              if SrcIndex = IndexR then
              begin
                IndexL := IndexR;
                AL := AR;
                RL := RR;
              end
              else
              begin
                IndexL := SrcIndex;
                V := SrcLine[IndexL];
                AL := V shr 8 and $00FF00FF;
                RL := V and $00FF00FF;
              end;
            end;
            if IndexR <> IndexL + 1 then
            begin
              IndexR := IndexL + 1;
              V := SrcLine[IndexR];
              AR := V shr 8 and $00FF00FF;
              RR := V and $00FF00FF;
            end;
            Dst[X] :=
              ((AL * WX + AR * WX2 + $007F007F) and $FF00FF00) or
              ((RL * WX + RR * WX2 + $007F007F) and $FF00FF00) shr 8;
          end;
        end
      else { WY < 256 }
        for X := 0 to ClipW - 1 do
        begin
          if MapHorz <> nil then
          begin
            SrcIndex := MapHorz[X];
            WX := BiasHorz[X];
            WX2 := 256 - WX;
          end
          else
          begin
            SrcIndex := X + SrcRect.Left;
            WX := 256;
            WX2 := 0;
          end;

          if WX >= 256 then
          begin
            if SrcIndex <> IndexL then
            begin
              if SrcIndex = IndexR then
              begin
                IndexL := IndexR;
                AL := AR;
                RL := RR;
                GL := GR;
                BL := BR;
              end
              else
              begin
                IndexL := SrcIndex;
                VT := SrcLine[IndexL];
                VB := SrcLine[IndexL + SrcData.RowStride];
                AL := WY * (VT shr 24) + WY2 * (VB shr 24);
                RL := (WY * (VT and $FF0000) + WY2 * (VB and $FF0000)) shr 16;
                GL := (WY * (VT and $FF00) + WY2 * (VB and $FF00)) shr 8;
                BL := WY * (VT and $FF) + WY2 * (VB and $FF);
              end;
            end;
            Dst[X] :=
              ((AL + $7F) shl 16 and $FF000000) or
              ((RL + $7F) shl  8 and $00FF0000) or
              ((GL + $7F)        and $0000FF00) or
              ((BL + $7F) shr  8              );
          end
          else
          begin
            if SrcIndex <> IndexL then
            begin
              if SrcIndex = IndexR then
              begin
                IndexL := IndexR;
                AL := AR;
                RL := RR;
                GL := GR;
                BL := BR;
              end
              else
              begin
                IndexL := SrcIndex;
                VT := SrcLine[IndexL];
                VB := SrcLine[IndexL + SrcData.RowStride];
                AL := WY * (VT shr 24) + WY2 * (VB shr 24);
                RL := (WY * (VT and $FF0000) + WY2 * (VB and $FF0000)) shr 16;
                GL := (WY * (VT and $FF00) + WY2 * (VB and $FF00)) shr 8;
                BL := WY * (VT and $FF) + WY2 * (VB and $FF);
              end;
            end;
          end;
          if IndexR <> IndexL + 1 then
          begin
            if WX >= 256 then
            begin
              IndexR := IndexL;
            end
            else
            begin
              IndexR := IndexL + 1;
              VT := SrcLine[IndexR];
              VB := SrcLine[IndexR + SrcData.RowStride];
              AR := WY * (VT shr 24) + WY2 * (VB shr 24);
              RR := (WY * (VT and $FF0000) + WY2 * (VB and $FF0000)) shr 16;
              GR := (WY * (VT and $FF00) + WY2 * (VB and $FF00)) shr 8;
              BR := WY * (VT and $FF) + WY2 * (VB and $FF);
            end;
          end;
          Dst[X] :=
            ((AL * WX + AR * WX2 + $7F00) shl 8 and $FF000000) or
            ((RL * WX + RR * WX2 + $7F00) and $00FF0000) or
            ((GL * WX + GR * WX2 + $7F00) shr 8 and $0000FF00) or
            ((BL * WX + BR * WX2 + $7F00) shr 16);
        end;
    end
    else { not Interpolate }
    begin
      if MapVert <> nil then SrcLine := PRGBQuadArray(Integer(SrcData.Bits) + (MapVert[Y] * SrcData.RowStride shl 2));
      if MapHorz <> nil then for X := 0 to ClipW - 1 do Dst[X] := SrcLine[MapHorz[X]]
      else MoveLongWord(SrcLine[SrcRect.Left], Dst[0], ClipH);
    end;

    if MapVert = nil then Inc(Integer(SrcLine), SrcData.RowStride shl 2);
    if Blend then BlendPixels_ATOP_LL(Buffer[0], DstLine[0], ClipW, $FF);
    Inc(Integer(DstLine), DstData.RowStride shl 2);
  end;

end;

procedure PixelBlockBlend(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  MasterAlpha: Byte);
var
  R: TRect;
  SrcIndex, DstIndex, Count: Integer;
  RowIndex: Integer;
begin
  if (MasterAlpha = 0) or not (Assigned(SrcData.Bits) and Assigned(DstData.Bits)) then Exit;
  IntersectRect(R, SrcRect, SrcData.ContentRect);
  OffsetRect(R, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R, R, DstData.ContentRect);
  if IsRectEmpty(R) then Exit;
  DstIndex := R.Left + R.Top * DstData.RowStride;
  SrcIndex := R.Left + SrcRect.Left - DstX + (R.Top + SrcRect.Top - DstY) * SrcData.RowStride;
  Count := R.Right - R.Left;

  for RowIndex := R.Top to R.Bottom - 1 do
  begin
    BlendPixels_ATOP_LL(PRGBQuadArray(SrcData.Bits)[SrcIndex], PRGBQuadArray(DstData.Bits)[DstIndex], Count, MasterAlpha);
    Inc(SrcIndex, SrcData.RowStride);
    Inc(DstIndex, DstData.RowStride);
  end
end;

procedure PixelBlockEffect(
  const DstData: TPixelData32;
  DstX, DstY: Integer;
  const SrcData: TPixelData32;
  const SrcRect: TRect;
  ColorEffects: TColorEffects);
var
  R: TRect;
  SrcIndex, DstIndex, Count: Integer;
  RowIndex: Integer;
begin
  if not (Assigned(SrcData.Bits) and Assigned(DstData.Bits)) then Exit;
  IntersectRect(R, SrcRect, SrcData.ContentRect);
  OffsetRect(R, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R, R, DstData.ContentRect);
  if IsRectEmpty(R) then Exit;
  DstIndex := R.Left + R.Top * DstData.RowStride;
  SrcIndex := R.Left + SrcRect.Left - DstX + (R.Top + SrcRect.Top - DstY) * SrcData.RowStride;
  Count := R.Right - R.Left;
  for RowIndex := R.Top to R.Bottom - 1 do
  begin
    ColorEffects.Transform(PRGBQuadArray(SrcData.Bits)[SrcIndex], PRGBQuadArray(DstData.Bits)[DstIndex], Count);
    Inc(SrcIndex, SrcData.RowStride);
    Inc(DstIndex, DstData.RowStride);
  end
end;

{ we need access to some private fields in TCustomImageList }
type
{$HINTS OFF}
  TCustomImageListCrack = class(TComponent)
  private
    FHeight: Integer;
    FWidth: Integer;
    FAllocBy: Integer;
    FHandle: HImageList;
    FDrawingStyle: TDrawingStyle;
    FMasked: Boolean;
    FShareImages: Boolean;
    FImageType: TImageType;
    FBkColor: TColor;
    FBlendColor: TColor;
    FClients: TList;
    FBitmap: TBitmap;
    FMonoBitmap: TBitmap;
    FChanged: Boolean;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
  end;
{$HINTS ON}

{$IFNDEF JR_D7}
type
  { THackerFiler is used to find the address of the private
    method TCustomImageList.ReadData (we don't need this in D7 and later because
    they declare this method as virtual and protected }

  THackerFiler = class(TFiler)
  protected
    ReaderProc: TStreamProc;
  public
    procedure DefineProperty(const Name: string; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string; ReadData, WriteData: TStreamProc; HasData: Boolean); override;
    procedure FlushBuffer; override;
  end;

procedure THackerFiler.DefineProperty(const Name: string; ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin
end;

procedure THackerFiler.DefineBinaryProperty(const Name: string; ReadData, WriteData: TStreamProc; HasData: Boolean);
begin
  if Name = 'Bitmap' then ReaderProc := TStreamProc(ReadData);
end;

procedure THackerFiler.FlushBuffer;
begin
end;
{$ENDIF}

//----------------------------------------------------------------------------//

{ TDIB32 }

procedure TDIB32.BlendTo(const DstData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte);
var
  SrcData: TPixelData32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if (MasterAlpha = 0) or not Assigned(Bits) or not Assigned(DstData.Bits) then Exit;
  if GetPixelData(SrcData) then
    PixelBlockBlend(DstData, DstX, DstY, SrcData, SrcRect, MasterAlpha);
end;

procedure TDIB32.BlendTo(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte);
const
  MaxBufferSz = 64;
var
  DstRect, R: TRect;
  SrcIndex, BufferIndex, HCount, VCount: Integer;
  Y: Integer;
  Buffer: TDIB32;
begin
  if (MasterAlpha = 0) or not Assigned(Bits) or
    not (GetClipBox(DstDC, DstRect) in [SIMPLEREGION, COMPLEXREGION]) then Exit;

  IntersectRect(R, SrcRect, ContentRect);
  OffsetRect(R, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R, R, DstRect);
  if IsRectEmpty(R) then Exit;
  DstRect := R;
  OffsetRect(R, SrcRect.Left - DstX, SrcRect.Top - DstY);

  BufferIndex := 0;
  SrcIndex := R.Left + R.Top * RowStride;
  HCount := R.Right - R.Left;
  VCount := R.Bottom - R.Top;

  Buffer := TDIB32.Create;

  if (HCount > MaxBufferSz) or (VCount > MaxBufferSz) then
  begin
    Buffer.SetSize(HCount, 1);
    for Y := DstRect.Top to DstRect.Bottom - 1 do
    begin
      Windows.BitBlt(Buffer.DC, 0, 0, HCount, 1, DstDC, DstRect.Left, Y, SRCCOPY);
      BlendPixels_ATOP_LL(Bits[SrcIndex], Buffer.Bits[0], HCount, MasterAlpha);
      Windows.BitBlt(DstDC, DstRect.Left, Y, HCount, 1, Buffer.DC, 0, 0, SRCCOPY);
      Inc(SrcIndex, RowStride);
    end;
  end
  else
  begin
    Buffer.SetSize(HCount, VCount);
    Windows.BitBlt(Buffer.DC, 0, 0, HCount, VCount, DstDC, DstRect.Left, DstRect.Top, SRCCOPY);
    for Y := DstRect.Top to DstRect.Bottom - 1 do
    begin
      BlendPixels_ATOP_LL(Bits[SrcIndex], Buffer.Bits[BufferIndex], HCount, MasterAlpha);
      Inc(SrcIndex, RowStride);
      Inc(BufferIndex, HCount);
    end;
    Windows.BitBlt(DstDC, DstRect.Left, DstRect.Top, HCount, VCount, Buffer.DC, 0, 0, SRCCOPY);
  end;

  Buffer.Free;
end;

procedure TDIB32.Assign(Src: TDIB32);
begin
  if Assigned(Src) then
  begin
    SetSize(Src.Width, Src.Height);
    MoveLongword(Src.Bits^, Bits^, Src.Width * Src.Height);
  end
  else SetSize(0, 0);
end;

procedure TDIB32.BlendTo(DstDIB: TDIB32; DstX, DstY: Integer; const SrcRect: TRect; MasterAlpha: Byte);
var
  SrcData: TPixelData32;
  DstData: TPixelData32;
begin
  if not (Assigned(Self) and Assigned(DstDIB)) then Error(@SInvalidParameters);
  if (MasterAlpha = 0) or not Assigned(Bits) or not Assigned(DstDIB.Bits) then Exit;
  GetPixelData(SrcData);
  DstDIB.GetPixelData(DstData);
  PixelBlockBlend(DstData, DstX, DstY, SrcData, SrcRect, MasterAlpha);
end;

procedure TDIB32.Clear(Clr: TRGBQuad);
begin
  if Assigned(Bits) then FillLongWord(FBits^, Width * Height, Clr);
end;

procedure TDIB32.Consume(Src: TDIB32);
begin
  { move all data from Src to Self; note: existing content is disposed }
  if (Src = Self) or not Assigned(Src) or not Assigned(Self) then Error(@SInvalidParameters);
  SetSize(0, 0);
  FHandle := Src.Handle;
  Move(Src.FBitmapInfo, FBitmapInfo, SizeOf(FBitmapInfo));
  FBits := Src.Bits;
  FDC := Src.DC;
  FHeight := Src.Height;
  FWidth := Src.Width;
  FContentRect := Src.ContentRect;
  Src.FHandle := 0;
  Src.FBitmapInfo.bmiHeader.biWidth := 0;
  Src.FBitmapInfo.bmiHeader.biHeight := 0;
  Src.FBits := nil;
  Src.FDC := 0;
  Src.FWidth := 0;
  Src.FHeight := 0;
  ZeroMemory(@Src.FContentRect, SizeOf(FContentRect));
end;

procedure TDIB32.CopyFrom(const SrcData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean);
const
  PBTFLAGS: array [Boolean] of Cardinal = (PBT_COPY, PBT_EXCHANGE);
var
  DstData: TPixelData32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  GetPixelData(DstData);
  PixelBlockTransfer(DstData, DstX, DstY, SrcData, SrcRect, PBTFLAGS[Exchange]);
end;

procedure TDIB32.CopyTo(const DstData: TPixelData32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean);
const
  PBTFLAGS: array [Boolean] of Cardinal = (PBT_COPY, PBT_EXCHANGE);
var
  SrcData: TPixelData32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  GetPixelData(SrcData);
  PixelBlockTransfer(DstData, DstX, DstY, SrcData, SrcRect, PBTFLAGS[Exchange]);
end;

procedure TDIB32.CopyTo(DstDC: HDC; const DstRect: TRect);
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if (DstDC <> 0) and Assigned(FBits) then with DstRect do
    StretchDIBits(DstDC,
      Left, Top, Right - Left, Bottom - Top,
      0, 0, Right - Left, Bottom - Top,
      FBits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TDIB32.CopyTo(DstDC: HDC; DstX, DstY: Integer);
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if (DstDC <> 0) and Assigned(FBits) then
    StretchDIBits(DstDC,
      DstX, DstY, Width, Height,
      0, 0, Width, Height,
      FBits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TDIB32.CopyTo(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect);
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if (DstDC <> 0) and Assigned(FBits) then with SrcRect do
    StretchDIBits(DstDC,
      DstX, DstY, Right - Left, Bottom - Top,
      Left, Top, Right - Left, Bottom - Top,
      FBits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TDIB32.CopyTo(DstDC: HDC; const DstRect, SrcRect: TRect);
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if (DstDC <> 0) and Assigned(FBits) then
    StretchDIBits(DstDC,
      DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      FBits, FBitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure TDIB32.CopyTo(DstDIB: TDIB32; DstX, DstY: Integer; const SrcRect: TRect; Exchange: Boolean);
const
  PBTFLAGS: array [Boolean] of Cardinal = (PBT_COPY, PBT_EXCHANGE);
var
  SrcData: TPixelData32;
  DstData: TPixelData32;
begin
  if not (Assigned(Self) and Assigned(DstDIB)) then Error(@SInvalidParameters);
  GetPixelData(SrcData);
  DstDIB.GetPixelData(DstData);
  PixelBlockTransfer(DstData, DstX, DstY, SrcData, SrcRect, PBTFLAGS[Exchange]);
end;

constructor TDIB32.Create;
begin
  ZeroMemory(@FBitmapInfo, SizeOf(FBitmapInfo));
  FBitmapInfo.bmiHeader.biSize := 40;//SizeOf(TBitmapInfo);
  FBitmapInfo.bmiHeader.biPlanes := 1;
  FBitmapInfo.bmiHeader.biCompression := BI_RGB;
  FBitmapInfo.bmiHeader.biBitCount := 32;
  FBitmapInfo.bmiHeader.biWidth := 0;
  FBitmapInfo.bmiHeader.biHeight := 0;
end;

function TDIB32.CreateCursor(HotSpotX, HotSpotY: Integer): HCURSOR;
begin
  Result := InternalCreateIcon(False, HotSpotX, HotSpotY);
end;

function TDIB32.CreateIcon: HICON;
begin
  Result := InternalCreateIcon(True, 0, 0);
end;

destructor TDIB32.Destroy;
begin
  SetSize(0, 0);
  inherited;
end;

class procedure TDIB32.Error(ResStringRec: PResStringRec);
  function ReturnAddr: Pointer;
  asm
    mov eax,[ebp+4];
  end;
begin
  raise EDIB32Error.CreateRes(ResStringRec) at ReturnAddr;
end;

class procedure TDIB32.ErrorFmt(ResStringRec: PResStringRec; const Args: array of const);
  function ReturnAddr: Pointer;
  asm
    mov eax,[ebp+4];
  end;
begin
  raise EDIB32Error.CreateResFmt(ResStringRec, Args) at ReturnAddr;
end;

function TDIB32.Extract: TDIB32;
begin
  { move all date to Result and empty Self }
  Result := TDIB32.Create;
  Result.FHandle := Handle;
  Move(FBitmapInfo, Result.FBitmapInfo, SizeOf(FBitmapInfo));
  Result.FBits := Bits;
  Result.FDC := DC;
  Result.FHeight := Height;
  Result.FWidth := Width;
  Result.FContentRect := ContentRect;

  FHandle := 0;
  FBitmapInfo.bmiHeader.biWidth := 0;
  FBitmapInfo.bmiHeader.biHeight := 0;
  FBits := nil;
  FDC := 0;
  FWidth := 0;
  FHeight := 0;
  ZeroMemory(@FContentRect, SizeOf(FContentRect));
end;

function TDIB32.GetPixelData(out PD: TPixelData32): Boolean;
begin
  Result := Assigned(Bits);
  PD.Bits := PRGBQuad(Bits);
  PD.ContentRect := ContentRect;
  PD.RowStride := RowStride;
end;

function TDIB32.InternalCreateIcon(AsIcon: Boolean; HotSpotX, HotSpotY: Integer): HICON;
var
  MonoBitmap: HBITMAP;
  ii: ICONINFO;
begin
  if Assigned(Bits) then
  begin
    MonoBitmap := CreateBitmap(Width, Height, 1, 1, nil);
    ii.fIcon := AsIcon;
    ii.xHotspot := HotSpotX;
    ii.yHotspot := HotSpotY;
    ii.hbmMask := MonoBitmap;
    ii.hbmColor := Handle;
    Result := CreateIconIndirect(ii);
    DeleteObject(MonoBitmap);
  end
  else Result := 0;
end;

procedure TDIB32.RenderEffectTo(const DstData: TPixelData32; DstX,
  DstY: Integer; const SrcRect: TRect; ColorEffects: TColorEffects);
var
  SrcData: TPixelData32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if not Assigned(Bits) or not Assigned(DstData.Bits) then Exit;
  GetPixelData(SrcData);
  PixelBlockEffect(DstData, DstX, DstY, SrcData, SrcRect, ColorEffects);
end;

procedure TDIB32.RenderEffectTo(DstDC: HDC; DstX, DstY: Integer;
  const SrcRect: TRect; ColorEffects: TColorEffects);
const
  MaxBufferSz = 64;
var
  DstRect, R: TRect;
  SrcIndex, BufferIndex, HCount, VCount: Integer;
  Y: Integer;
  Buffer: TDIB32;
begin
  if not Assigned(Bits) or
    not (GetClipBox(DstDC, DstRect) in [SIMPLEREGION, COMPLEXREGION]) then Exit;

  IntersectRect(R, SrcRect, ContentRect);
  OffsetRect(R, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R, R, DstRect);
  if IsRectEmpty(R) then Exit;
  DstRect := R;
  OffsetRect(R, SrcRect.Left - DstX, SrcRect.Top - DstY);

  BufferIndex := 0;
  SrcIndex := R.Left + R.Top * RowStride;
  HCount := R.Right - R.Left;
  VCount := R.Bottom - R.Top;

  Buffer := TDIB32.Create;

  if (HCount > MaxBufferSz) or (VCount > MaxBufferSz) then
  begin
    Buffer.SetSize(HCount, 1);
    for Y := DstRect.Top to DstRect.Bottom - 1 do
    begin
      Windows.BitBlt(Buffer.DC, 0, 0, HCount, 1, DstDC, DstRect.Left, Y, SRCCOPY);
      ColorEffects.Transform(Bits[SrcIndex], Buffer.Bits[0], HCount);
      Windows.BitBlt(DstDC, DstRect.Left, Y, HCount, 1, Buffer.DC, 0, 0, SRCCOPY);
      Inc(SrcIndex, RowStride);
    end;
  end
  else
  begin
    Buffer.SetSize(HCount, VCount);
    Windows.BitBlt(Buffer.DC, 0, 0, HCount, VCount, DstDC, DstRect.Left, DstRect.Top, SRCCOPY);
    for Y := DstRect.Top to DstRect.Bottom - 1 do
    begin
      ColorEffects.Transform(Bits[SrcIndex], Buffer.Bits[BufferIndex], HCount);
      Inc(SrcIndex, RowStride);
      Inc(BufferIndex, HCount);
    end;
    Windows.BitBlt(DstDC, DstRect.Left, DstRect.Top, HCount, VCount, Buffer.DC, 0, 0, SRCCOPY);
  end;

  Buffer.Free;
end;

procedure TDIB32.RenderEffectTo(DstDIB: TDIB32; DstX, DstY: Integer;
  const SrcRect: TRect; ColorEffects: TColorEffects);
var
  SrcData: TPixelData32;
  DstData: TPixelData32;
begin
  if not (Assigned(Self) and Assigned(DstDIB)) then Error(@SInvalidParameters);
  if not Assigned(Bits) or not Assigned(DstDIB.Bits) then Exit;
  GetPixelData(SrcData);
  DstDIB.GetPixelData(DstData);
  PixelBlockEffect(DstData, DstX, DstY, SrcData, SrcRect, ColorEffects);
end;

procedure TDIB32.RenderIcon(Icon: HICON; X, Y, W, H: Integer; Flags: Cardinal);
var
  Rect: TRect;
  Buffer1: TDIB32;
  SrcIdx, DstIdx: Integer;
  I, J, Cnt: Integer;
  C1, C2: Cardinal;
  Alpha: Cardinal;
  DivTableLine: PDivTableLine;
begin
  if W <= 0 then W := GetSystemMetrics(SM_CYICON);
  if H <= 0 then H := GetSystemMetrics(SM_CXICON);
  Rect.Left := Max(0, X);
  Rect.Top := Max(0, Y);
  Rect.Right := Min(Width, X + W);
  Rect.Bottom := Min(Height, Y + H);
  if (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top) then Exit;

  if Flags = RF_BLEND then
  begin
    Windows.DrawIconEx(DC, X, Y, Icon, W, H, 0, 0, DI_NORMAL);
    Exit;
  end;

  SolidFillRect(Rect, $FF000000);
  Windows.DrawIconEx(DC, X, Y, Icon, W, H, 0, 0, DI_NORMAL);

  Buffer1 := TDIB32.Create;
  try
    Buffer1.SetSize(W, H);
    Buffer1.SolidFillRect(Buffer1.ContentRect, $FFFFFFFF);
    Windows.DrawIconEx(Buffer1.DC, X - Rect.Left, Y - Rect.Top, Icon, W, H, 0, 0, DI_NORMAL);

    SrcIdx := 0;
    DstIdx := Rect.Left + Rect.Top * RowStride;
    Cnt := Rect.Right - Rect.Left;

    for J := Rect.Top to Rect.Bottom - 1 do
    begin
      for I := 0 to Cnt - 1 do
      begin
        C1 := Bits[DstIdx + I];
        C2 := Buffer1.Bits[SrcIdx + I];
        if C1 = C2 then Bits[DstIdx + I] := C1 or $FF000000
        else
        begin
          { assuming DrawIconEx uses linear blending we can extract the alpha channel }
          Alpha := $FF + Integer(C1 and $FF) - Integer(C2 and $FF);
          if Alpha <= 0 then Bits[DstIdx + I] := $007F7F7F
          else
          begin
            { restore original (non premultiplied) color components }
            Assert(Alpha <= $FF);
            DivTableLine := PDivTableLine(@DIV_TABLE[Alpha, 0]);
            C1 :=
              DivTableLine[C1 and $FF] or
              DivTableLine[C1 shr 8 and $FF] shl 8 or
              DivTableLine[C1 shr 16 and $FF] shl 16 or
              Alpha shl 24;
            Bits[DstIdx + I] := C1;
          end;
        end;
      end;

      Inc(SrcIdx, Cnt);
      Inc(DstIdx, RowStride);
    end;
  finally
    Buffer1.Free;
  end;
end;

procedure TDIB32.RenderImage(ImageList: HIMAGELIST; ImageIndex, X, Y, W, H: Integer; Flags: Cardinal);
var
  Rect: TRect;
  Buffer2: TDIB32;
  SrcIdx, DstIdx: Integer;
  I, J, HCount, VCount: Integer;
  C1, C2: Cardinal;
  Alpha: Cardinal;
  DivTableLine: PDivTableLine;
begin
  if not ImageList_GetIconSize(ImageList, I, J) then Exit;
  if W <= 0 then W := I;
  if H <= 0 then H := J;
  Rect.Left := Max(0, X);
  Rect.Top := Max(0, Y);
  Rect.Right := Min(Width, X + W);
  Rect.Bottom := Min(Height, Y + H);
  if (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top) then Exit;

  if Flags = RF_BLEND then
  begin
    ImageList_DrawEx(ImageList, ImageIndex, DC, X, Y, W, H, CLR_NONE, CLR_NONE, ILD_NORMAL);
    Exit;
  end;

  HCount := Rect.Right - Rect.Left;
  VCount := Rect.Bottom - Rect.Top;

  SolidFillRect(Rect, $FF000000);
  ImageList_DrawEx(ImageList, ImageIndex, DC, X, Y, W, H, CLR_NONE, CLR_NONE, ILD_NORMAL);

  Buffer2 := TDIB32.Create;
  try
    Buffer2.SetSize(HCount, VCount);
    Buffer2.Clear($FFFFFFFF);
    ImageList_DrawEx(ImageList, ImageIndex, Buffer2.DC, X - Rect.Left, Y - Rect.Top, W, H, CLR_NONE, CLR_NONE, ILD_NORMAL);

    SrcIdx := 0;
    DstIdx := Rect.Left + Rect.Top * RowStride;

    for J := 0 to VCount - 1 do
    begin
      for I := 0 to HCount - 1 do
      begin
        C1 := Bits[DstIdx + I];
        C2 := Buffer2.Bits[SrcIdx + I];
        if C1 = C2 then Bits[DstIdx + I] := C1 or $FF000000
        else
        begin
          { assuming ImageList_DrawEx uses linear blending we can extract the alpha channel }
          Alpha := $FF + Integer(C1 and $FF) - Integer(C2 and $FF);
          if Alpha <= 0 then Bits[DstIdx + I] := $007F7F7F
          else
          begin
            { restore original (non premultiplied) color components }
            Assert(Alpha <= $FF);
            DivTableLine := PDivTableLine(@DIV_TABLE[Alpha, 0]);
            C1 :=
              DivTableLine[C1 and $FF] or
              DivTableLine[C1 shr 8 and $FF] shl 8 or
              DivTableLine[C1 shr 16 and $FF] shl 16 or
              Alpha shl 24;
            Bits[DstIdx + I] := C1;
          end;
        end;
      end;
      Inc(SrcIdx, HCount);
      Inc(DstIdx, RowStride);
    end;
  finally
    Buffer2.Free;
  end;
end;

procedure TDIB32.SetAlpha(Rect: TRect; Alpha: Byte);
var
  Y, Cnt, N: Integer;
  P: PByte;
begin
  Rect.Left := Max(Rect.Left, 0);
  Rect.Top := Max(Rect.Top, 0);
  Rect.Right := Min(Rect.Right, Width);
  Rect.Bottom := Min(Rect.Bottom, Height);
  if (Rect.Right > Rect.Left) and (Rect.Bottom > Rect.Top) then
  begin
    Cnt := Rect.Right - Rect.Left;
    P := @Bits[Rect.Top * RowStride + Rect.Left];
    Inc(P, 3);
    for Y := Rect.Top to Rect.Bottom - 1 do
    begin
      N := Cnt;
      while N >= 8 do
      begin
        P^ := Alpha;
        PByteArray(P)[04] := Alpha;
        PByteArray(P)[08] := Alpha;
        PByteArray(P)[12] := Alpha;
        PByteArray(P)[16] := Alpha;
        PByteArray(P)[20] := Alpha;
        PByteArray(P)[24] := Alpha;
        PByteArray(P)[28] := Alpha;
        Inc(P, 32);
        Dec(N, 8);
      end;
      while N > 0 do
      begin
        P^ := Alpha;
        Inc(P);
        Dec(N);
      end;
      Inc(P, (RowStride - Cnt) * 4);
    end;
  end;
end;

procedure TDIB32.SetSize(W, H: Integer; KeepContent: Boolean);
var
  ScreenDC: HDC;
  SaveData: TDIB32;
begin
  if W < 0 then W := 0;
  if H < 0 then H := 0;

  KeepContent := KeepContent and Assigned(Bits) and ((W > 0) or (H > 0));

  if KeepContent then SaveData := Extract else SaveData := nil;
  try

    if (W <> FWidth) or (H <> FHeight) then
    begin
      { Delete the old buffer }
      if (Width > 0) and (Height > 0) then
      begin
        DeleteDC(FDC);
        FDC := 0;
        DeleteObject(FHandle);
        FHandle := 0;
        FBits := nil;
      end;
      FContentRect.Right := 0;
      FContentRect.Bottom := 0;
      FWidth := 0;
      FHeight := 0;
      FBitmapInfo.bmiHeader.biWidth := 0;
      FBitmapInfo.bmiHeader.biHeight := 0;
      if (W > 0) and (H > 0) then
      begin
        FBitmapInfo.bmiHeader.biWidth := W;
        FBitmapInfo.bmiHeader.biHeight := -H;
        ScreenDC := GetDC(0);
        FHandle := CreateDIBSection(ScreenDC, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBits), 0, 0);
        ReleaseDC(0, ScreenDC);
        if FHandle = 0 then
        begin
          FBitmapInfo.bmiHeader.biWidth := 0;
          FBitmapInfo.bmiHeader.biHeight := 0;
          RaiseLastOSError;
        end;
        FDC := CreateCompatibleDC(0);
        if FDC = 0 then
        begin
          DeleteObject(FHandle);
          FHandle := 0;
          FBits := nil;
          FBitmapInfo.bmiHeader.biWidth := 0;
          FBitmapInfo.bmiHeader.biHeight := 0;
          RaiseLastOSError;
        end;
        SelectObject(FDC, FHandle);
        FWidth := W;
        FHeight := H;
        FContentRect.Right := W;
        FContentRect.Bottom := H;
      end
      else
      begin
        FWidth := W;
        FHeight := H;
        FContentRect.Right := W;
        FContentRect.Bottom := H;
      end;
    end;
  finally
    if Assigned(SaveData) then
    begin
      SaveData.CopyTo(Self, 0, 0, SaveData.ContentRect);
      SaveData.Free;
    end;
  end;
end;

procedure TDIB32.SolidFillRect(Rect: TRect; Clr: TRGBQuad);
var
  Y, Cnt, Idx: Integer;
begin
  Rect.Left := Max(Rect.Left, 0);
  Rect.Top := Max(Rect.Top, 0);
  Rect.Right := Min(Rect.Right, Width);
  Rect.Bottom := Min(Rect.Bottom, Height);
  if (Rect.Right > Rect.Left) and (Rect.Bottom > Rect.Top) then
  begin
    Idx := Rect.Top * RowStride + Rect.Left;
    Cnt := Rect.Right - Rect.Left;
    for Y := Rect.Top to Rect.Bottom - 1 do
    begin
      FillLongWord(Bits[Idx], Cnt, Clr);
      Inc(Idx, RowStride);
    end;
  end;
end;

procedure TDIB32.StretchTo(DstData: TPixelData32; const DstRect, SrcRect: TRect; Flags: Cardinal);
var
  SrcData: TPixelData32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if GetPixelData(SrcData) then
    PixelBlockResample(DstData, DstRect, SrcData, SrcRect, Flags);
end;

procedure TDIB32.StretchTo(DstDIB: TDIB32; const DstRect, SrcRect: TRect; Flags: Cardinal);
var
  SrcData, DstData: TPixelData32;
begin
  if not (Assigned(Self) and Assigned(DstDIB)) then Error(@SInvalidParameters);
  if GetPixelData(SrcData) and DstDIB.GetPixelData(DstData) then
    PixelBlockResample(DstData, DstRect, SrcData, SrcRect, Flags);
end;

procedure TDIB32.StretchTo(DstDC: HDC; const DstRect, SrcRect: TRect; Flags: Cardinal);
var
  Blending, Resampling: Boolean;
  ClippedDstRect, AdjustedDstRect: TRect;
  TmpDIB: TDIB32;
begin
  if not Assigned(Self) then Error(@SInvalidParameters);
  if not RectVisible(DstDC, DstRect) then Exit;

  Blending := Flags and PBR_OP_MASK = PBR_BLEND;
  Resampling := (DstRect.Right - DstRect.Left <> SrcRect.Right - SrcRect.Left) or
    (DstRect.Bottom - DstRect.Top <> SrcRect.Bottom - SrcRect.Top);

  if not Resampling then
  begin
    if Blending then BlendTo(DstDC, DstRect.Left, DstRect.Top, SrcRect)
    else CopyTo(DstDC, DstRect.Left, DstRect.Top, SrcRect);
  end
  else if not Blending and (Flags and PBR_LINEAR = 0) then CopyTo(DstDC, DstRect, SrcRect)
  else
  begin
    GetClipBox(DstDC, ClippedDstRect);
    IntersectRect(ClippedDstRect, ClippedDstRect, DstRect);

    AdjustedDstRect := DstRect;
    OffsetRect(AdjustedDstRect, -ClippedDstRect.Left, -ClippedDstRect.Top);

    TmpDIB := TDIB32.Create;
    try
      with ClippedDstRect do TmpDIB.SetSize(Right - Left, Bottom - Top);
      StretchTo(TmpDIB, AdjustedDstRect, SrcRect, Flags and PBR_RESAMPLE_MASK or PBR_COPY);
      if Blending then TmpDIB.BlendTo(DstDC, ClippedDstRect.Left, ClippedDstRect.Top, TmpDIB.ContentRect)
      else TmpDIB.CopyTo(DstDC, ClippedDstRect.Left, ClippedDstRect.Top, TmpDIB.ContentRect);
    finally
      TmpDIB.Free;
    end;
  end;
end;

procedure TDIB32.VCLDrawDisabled(DstDC: HDC; DstX, DstY: Integer; const SrcRect: TRect);
var
  DstRect, R, R2: TRect;
  SrcIndex, HCount, VCount: Integer;
  X, Y: Integer;
  Buffer: TDIB32;
  C, I: Cardinal;
  ClrHi, ClrLo: Cardinal;
  ShadowIndex: Integer;
begin
  if not Assigned(Bits) or
    not (GetClipBox(DstDC, DstRect) in [SIMPLEREGION, COMPLEXREGION]) then Exit;

  IntersectRect(R, SrcRect, ContentRect);
  if IsRectEmpty(R) then Exit;
  R2 := R;
  Inc(R2.Right); Inc(R2.Bottom); // take shadow extent into account
  OffsetRect(R2, DstX - SrcRect.Left, DstY - SrcRect.Top);
  IntersectRect(R2, R2, DstRect);
  if IsRectEmpty(R2) then Exit;

  HCount := R.Right - R.Left;
  VCount := R.Bottom - R.Top;

  SrcIndex := R.Left + R.Top * RowStride;
  ShadowIndex := 0;
  Buffer := TDIB32.Create;
  Buffer.SetSize(HCount, VCount);
  for Y := 0 to VCount - 1 do
  begin
    for X := 0 to HCount - 1 do
    begin
      C := Bits[SrcIndex + X];
      if C > $3FFFFFFF then
      begin
        I := ((C shr 16 and $FF) + (C shr 7 and $1FE) + (C and $FF) + $2) shr 2;
        if I <= 128 then
          Buffer.Bits[ShadowIndex] := C and $FF000000
        else if I < 192 then
          Buffer.Bits[ShadowIndex] := (C and $FF000000) shr 6 * (192 - I) and $FF000000
      end;
      Inc(ShadowIndex);
    end;
    Inc(SrcIndex, RowStride);
  end;

  ClrHi := ColorRefToRGB(GetSysColor(COLOR_BTNHIGHLIGHT));
  ClrLo := ColorRefToRGB(GetSysColor(COLOR_BTNSHADOW));
  for ShadowIndex := 0 to HCount * VCount - 1 do
    Buffer.Bits[ShadowIndex] := Buffer.Bits[ShadowIndex] or ClrHi;
  Buffer.BlendTo(DstDC, DstX + 1, DstY + 1, Buffer.ContentRect);
  for ShadowIndex := 0 to HCount * VCount - 1 do
    Buffer.Bits[ShadowIndex] := Buffer.Bits[ShadowIndex] and $FF000000 or ClrLo;
  Buffer.BlendTo(DstDC, DstX, DstY, Buffer.ContentRect);
  Buffer.Free;
end;

//----------------------------------------------------------------------------//

{ TPNGBitmap }

procedure TPNGBitmap.Assign(Source: TPersistent);
begin
  if Source = nil then
  begin
    if Assigned(DIB) then
    begin
      FDIB.Free;
      FDIB := nil;
    end;
    Changed(Self);
  end
  else if Source is TPNGBitmap then
  begin
    if not Assigned(DIB) then FDIB := TDIB32.Create;
    DIB.Assign(TPNGBitmap(Source).DIB);
    Changed(Self);
  end
  else if Source is TBitmap then
  begin
    if not Assigned(DIB) then FDIB := TDIB32.Create;
    FDIB.SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
    if (DIB.Width > 0) and (DIB.Height > 0) then
    begin
      BitBlt(FDIB.DC, 0, 0, DIB.Width, DIB.Height, TBitmap(Source).Canvas.Handle, 0, 0, SRCCOPY);
    end;
    Changed(Self);
  end
  else inherited;
end;

constructor TPNGBitmap.Create;
begin
  inherited;
  Transparent := True;
end;

destructor TPNGBitmap.Destroy;
begin
  FDIB.Free;
  inherited;
end;

procedure TPNGBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Flags: Integer;
begin
  if Assigned(DIB) then
  begin
    Flags := PBR_BLEND or PBR_LINEAR;
    DIB.StretchTo(ACanvas.Handle, Rect, DIB.ContentRect, Flags);
  end;
end;

function TPNGBitmap.GetEmpty: Boolean;
begin
  Result := not Assigned(DIB) or (DIB.Width = 0) or (DIB.Height = 0);
end;

function TPNGBitmap.GetHeight: Integer;
begin
  if Assigned(DIB) then Result := DIB.Height else Result := 0;
end;

function TPNGBitmap.GetTransparent: Boolean;
begin
  Result := True;
end;

function TPNGBitmap.GetWidth: Integer;
begin
  if Assigned(DIB) then Result := DIB.Width else Result := 0;
end;

procedure TPNGBitmap.LoadFromStream(Stream: TStream);
begin
  if not Assigned(DIB) then FDIB := TDIB32.Create;
  LoadPNGGraphic(Stream, DIB);
end;

procedure TPNGBitmap.SaveToStream(Stream: TStream);
begin
  if Assigned(DIB) then SavePNGGraphic(Stream, DIB);
end;

procedure TPNGBitmap.SetHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> GetHeight then
  begin
    if not Assigned(DIB) then FDIB := TDIB32.Create;
    DIB.SetSize(DIB.Width, Value);
    Changed(Self);
  end;
end;

procedure TPNGBitmap.SetTransparent(Value: Boolean);
begin
  inherited SetTransparent(True);
end;

procedure TPNGBitmap.SetWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> GetWidth then
  begin
    if not Assigned(DIB) then FDIB := TDIB32.Create;
    DIB.SetSize(Value, DIB.Height);
    Changed(Self);
  end;
end;



//----------------------------------------------------------------------------//

{ constants to determine image list grouth rate }

const
  AutoGrowBytes = 16384; // grow in 16K increments
  MaxGrowBytes = $100000; // do not grow faster than in 1 MB increments

{ TDIBList32 }

procedure TDIBList32.APIDrawEx(Index: Integer; DC: HDC; X, Y, DX, DY: Integer; Bk, Fg: TColorRef; Style: Cardinal);
var
  DstRect, SrcRect: TRect;
  Brush: HBRUSH;
  OverlayIndex, ImageIndex: Integer;
  CE: TColorEffects;
  C: Cardinal;
begin
  { APIDrawEx simulates ImageList_DrawEx call }
  { differences with the standard API call:
    Masks are not supported
    Bk = CLR_DEFAULT is treaded the same way as Bk = CLR_NONE
    Colors are always blended, dithering is not supported }
  if DX <= 0 then DX := Width else if DX > Width then DX := Width;
  if DY <= 0 then DY := Height else if DY > Height then DY := Height;

  DstRect := Bounds(X, Y, DX, DY);
  if not RectVisible(DC, DstRect) then Exit;

  if (Bk <> CLR_NONE) and (Bk <> CLR_DEFAULT) and (Style and ILD_TRANSPARENT = 0) then
  begin
    Brush := Windows.CreateSolidBrush(Bk);
    Windows.FillRect(DC, DstRect, Brush);
    DeleteObject(Brush);
  end;

  ImageIndex := Index and $FF;
  OverlayIndex := Index shr 8 and $0F;

  if (ImageIndex >= 0) and (ImageIndex < Count) then
  begin
    if Fg = CLR_DEFAULT then Fg := Windows.GetSysColor(COLOR_HIGHLIGHT);

    SrcRect := GetImageRect(ImageIndex);
    SrcRect.Right := SrcRect.Left + DX;
    SrcRect.Bottom := SrcRect.Top + DY;

    case Style and $0F of
      ILD_NORMAL, ILD_TRANSPARENT:
        begin
          DIB.BlendTo(DC, X, Y, SrcRect);
        end;
      ILD_BLEND25: // same as ILD_FOCUS
        begin
          if Fg = CLR_NONE then DIB.BlendTo(DC, X, Y, SrcRect, $40)
          else
          begin
            C := ColorRefToRGB(Fg);
            CE := TColorEffects.Create;
            CE.AddEffect(CLE_MULTIPLY, $20000000 or C);
            CE.AddEffect(CLE_ADD, $20000000 or C);
            DIB.RenderEffectTo(DC, X, Y, SrcRect, CE);
            CE.Free;
          end;
        end;
      ILD_BLEND50: // same as ILD_BLEND or ILD_SELECTED
        begin
          if Fg = CLR_NONE then DIB.BlendTo(DC, X, Y, SrcRect, $80)
          else
          begin
            C := ColorRefToRGB(Fg);
            CE := TColorEffects.Create;
            CE.AddEffect(CLE_MULTIPLY, $60000000 or C);
            CE.AddEffect(CLE_ADD, $20000000 or C);
            DIB.RenderEffectTo(DC, X, Y, SrcRect, CE);
            CE.Free;
          end;
        end;
    end;
  end;

  if (OverlayIndex >= 1) and (OverlayIndex <= 15) then
  begin
    OverlayIndex := FOverlays[OverlayIndex];
    if (OverlayIndex >= 0) and (OverlayIndex < Count) then
    begin
      SrcRect := GetImageRect(OverlayIndex);
      SrcRect.Right := SrcRect.Left + DX;
      SrcRect.Bottom := SrcRect.Top + DY;
      DIB.BlendTo(DC, X, Y, SrcRect);
    end;
  end;
end;

procedure TDIBList32.Assign(Src: TDIBList32);
var
  I: Integer;
begin
  if Assigned(Src) then
  begin
    Self.FCount := Src.FCount;
    Self.FWidth := Src.FWidth;
    Self.FHeight := Src.FHeight;
    Self.FImageFlags := Copy(Src.FImageFlags, 0, Length(Src.FImageFlags));
    Self.FCapacity := Src.FCapacity;
    for I := Low(TOverlayIndex) to High(TOverlayIndex) do Self.FOverlays[I] := Src.FOverlays[I];
    Self.FDIB.Assign(Src.FDIB);
  end
  else
  begin
    Self.DeleteAll;
  end;
end;

procedure TDIBList32.Blend(ImageIndex: Integer; DC: HDC; X, Y: Integer; MasterAlpha: Byte);
begin
  DIB.BlendTo(DC, X, Y, GetImageRect(ImageIndex), MasterAlpha);
end;

constructor TDIBList32.Create(AWidth, AHeight, AGrow: Integer);
begin
  FDIB := TDIB32.Create;
  if (AWidth <= 0) or (AHeight <= 0) then Error(@SInvalidParameters);

  FWidth := AWidth;
  FHeight := AHeight;

  if AGrow <= 0 then
  begin
    { calculate automatic grow rate for Grow depending on image size
      for AutoGrowBytes = 16384:
        - 64x64 (or larger) images grow with every new image
        - 32x32 images grow every 4-th image
        - 16x16 images grow every 32-nd image }
    FGrow := Max(1, (AutoGrowBytes - 1) div (Width * Height * 4) + 1);
  end
  else if AGrow * Width * Height * 4 > MaxGrowBytes then
    FGrow := Max(1, (MaxGrowBytes - 1) div (Width * Height * 4) + 1)
  else
    FGrow := AGrow;
end;

procedure TDIBList32.Delete(Index, NumImages: Integer);
begin
  if NumImages < 1 then Exit;
  if (Index < 0) or (Index + NumImages > Count) then Error(@SInvalidParameters);
  InternalDelete(Index, NumImages);
end;

procedure TDIBList32.DeleteAll;
begin
  DIB.SetSize(0, 0);
  SetLength(FImageFlags, 0);
  FCount := 0;
  FCapacity := 0;
end;

destructor TDIBList32.Destroy;
begin
  FDIB.Free;
  inherited;
end;

procedure TDIBList32.DrawEffect(ImageIndex: Integer; DC: HDC; X, Y: Integer; ColorEffects: TColorEffects);
begin
  Self.DIB.RenderEffectTo(DC, X, Y, Self.GetImageRect(ImageIndex), ColorEffects);
end;

function TDIBList32.Equal(List: TDIBList32): Boolean;
begin
  Result :=
    (List.Count = Count) and
    (List.Width = Width) and
    (List.Height = Height) and
    CompareMem(DIB.Bits, List.DIB.Bits, DIB.Width * DIB.Height);
end;

class procedure TDIBList32.Error(ResStringRec: PResStringRec);
  function ReturnAddr: Pointer;
  asm
    mov eax,[ebp+4];
  end;
begin
  raise EDIB32Error.CreateRes(ResStringRec) at ReturnAddr;
end;

class procedure TDIBList32.ErrorFmt(ResStringRec: PResStringRec; const Args: array of const);
  function ReturnAddr: Pointer;
  asm
    mov eax,[ebp+4];
  end;
begin
  raise EDIB32Error.CreateResFmt(ResStringRec, Args) at ReturnAddr;
end;

procedure TDIBList32.Exchange(Index1, Index2: Integer);
var
  PD: TPixelData32;
begin
  if (Index1 < 0) or (Index2 < 0) or (Index1 >= Count) or (Index2 >= Count) then Error(@SInvalidParameters);
  if (Index1 <> Index2) and DIB.GetPixelData(PD) then
    PixelBlockTransfer(PD, 0, Index1 * Height,
      PD, Rect(0, Index2 * Height, Width, Index2 * Height + Height),
      PBT_EXCHANGE);
end;

function TDIBList32.GetCursor(ImageIndex, HotSpotX, HotSpotY: Integer): HCURSOR;
var
  D: TDIB32;
begin
  if (ImageIndex >= 0) and (ImageIndex < Count) then
  begin
    D := GetImage(ImageIndex);
    try
      Result := D.CreateCursor(HotSpotX, HotSpotY);
    finally
      D.Free;
    end;
  end
  else Result := 0;
end;

function TDIBList32.GetIcon(ImageIndex: Integer): HICON;
var
  D: TDIB32;
begin
  if (ImageIndex >= 0) and (ImageIndex < Count) then
  begin
    D := GetImage(ImageIndex);
    try
      Result := D.CreateIcon;
    finally
      D.Free;
    end;
  end
  else Result := 0;
end;

function TDIBList32.GetImage(ImageIndex: Integer): TDIB32;
begin
  if (ImageIndex >= 0) and (ImageIndex < Count) then
  begin
    Result := TDIB32.Create;
    Result.SetSize(Width, Height);
    DIB.CopyTo(Result, 0, 0, GetImageRect(ImageIndex));
  end
  else
  begin
    Error(@SInvalidParameters);
    Result := nil;
  end;
end;

function TDIBList32.GetImageRect(ImageIndex: Integer): TRect;
begin
  if (ImageIndex >= 0) and (ImageIndex < Count) then
  begin
    Result := DIB.ContentRect;
    Inc(Result.Top, ImageIndex * Height);
    Result.Bottom := Result.Top + Integer(Height);
  end
  else Result := Rect(0, 0, 0, 0);
end;

function TDIBList32.ImportIcon(Icon: HICON; ImageIndex: Integer; ReplaceExisting: Boolean): Integer;
begin
  Result := PrepareImport(ImageIndex, ReplaceExisting);
  if (Result < 0) or (Icon = 0) then Error(@SInvalidParameters);
  with GetImageRect(Result) do
    DIB.RenderIcon(Icon, Left, Top, Right - Left, Bottom - Top, RF_EXTRACT);
end;

function TDIBList32.ImportImage(DIB: TDIB32; ImageIndex: Integer; ReplaceExisting: Boolean): Integer;
var
  R: TRect;
begin
  Result := PrepareImport(ImageIndex, ReplaceExisting);
  if (Result < 0) or (DIB = Self.DIB) or not Assigned(DIB) then Error(@SInvalidParameters);
  if Assigned(DIB) then
  begin
    R := GetImageRect(Result);
    DIB.CopyTo(Self.DIB, R.Left, R.Top, Rect(0, 0, Width, Height));
  end;
end;

function TDIBList32.ImportImage(DIB: TDIB32; SrcRect: TRect; ImageIndex: Integer; ReplaceExisting: Boolean): Integer;
var
  R: TRect;
begin
  Result := PrepareImport(ImageIndex, ReplaceExisting);
  if (Result < 0) or (DIB = Self.DIB) or not Assigned(DIB) then Error(@SInvalidParameters);
  R := GetImageRect(Result);
  if (Width = (SrcRect.Right - SrcRect.Left)) and
    (Height = (SrcRect.Bottom - SrcRect.Top)) then
    DIB.CopyTo(Self.DIB, R.Left, R.Top, SrcRect)
  else
    Error(@SInvalidParameters);
end;

function TDIBList32.ImportImage(ImageList: HIMAGELIST; SrcIndex, DstIndex: Integer; ReplaceExisting: Boolean): Integer;
begin
  Result := PrepareImport(DstIndex, ReplaceExisting);
  if (Result < 0) or (ImageList = 0) then Error(@SInvalidParameters);
  with GetImageRect(Result) do
    DIB.RenderImage(ImageList, SrcIndex, Left, Top, Right - Left, Bottom - Top, RF_EXTRACT);
end;

procedure TDIBList32.InternalDelete(Index, NumImages: Integer);
var
  NewCount, NewCapacity, PPI: Integer;
  OldDIB: TDIB32;
  OldImageFlags: TCardinalDynArray;
begin
  OldImageFlags := nil;
  if NumImages = 0 then Exit;

  OldDIB := nil;

  NewCount := Count - NumImages;
  try
    if NewCount < FCapacity - FGrow then
    begin
      NewCapacity := NewCount;
      OldDIB := DIB.Extract;
      try
        DIB.SetSize(Width, NewCapacity * Height);
      except
        DIB.Consume(OldDIB);
        raise;
      end;
      FCapacity := NewCapacity;
      OldImageFlags := FImageFlags;
      SetLength(FImageFlags, FCapacity);
    end;

    PPI := Width * Height;   // number of pixels per image

    if (Index > 0) and Assigned(OldDIB) then
    begin
      MoveLongword(OldDIB.Bits[0], DIB.Bits[0], Index * PPI);
      MoveLongword(OldImageFlags[0], FImageFlags[0], Index);
    end;

    if Index < NewCount then
    begin
      if Assigned(OldDIB) then
      begin
        MoveLongword(OldDIB.Bits[Index + NumImages * PPI], DIB.Bits[Index * PPI], (NewCount - Index) * PPI);
        MoveLongword(OldImageFlags[Index + NumImages], FImageFlags[Index], NewCount - Index);
      end
      else
      begin
        MoveLongword(DIB.Bits[(Index + NumImages) * PPI], DIB.Bits[Index * PPI], (NewCount - Index) * PPI);
        MoveLongword(FImageFlags[Index + NumImages], FImageFlags[Index], NewCount - Index);
      end;
    end;

    FCount := NewCount;
  finally
    if Assigned(OldDIB) then OldDIB.Free;
  end;
end;

procedure TDIBList32.InternalInsert(Index, NumImages: Integer);
var
  NewCount, NewCapacity, PPI: Integer;
  OldDIB: TDIB32;
  OldImageFlags: TCardinalDynArray;
begin
  OldImageFlags := nil;
  if NumImages = 0 then Exit;

  OldDIB := nil;

  NewCount := Count + NumImages;
  try
    if NewCount > FCapacity then
    begin
      NewCapacity := NewCount + FGrow - 1;
      OldDIB := DIB.Extract;
      try
        DIB.SetSize(Width, NewCapacity * Height);
      except
        DIB.Consume(OldDIB); // restore previous state
        raise;
      end;
      FCapacity := NewCapacity;
      OldImageFlags := FImageFlags;
      SetLength(FImageFlags, FCapacity);
    end;

    PPI := Width * Height;   // number of pixels per image

    if (Index > 0) and Assigned(OldDIB) then
    begin
      MoveLongword(OldDIB.Bits[0], DIB.Bits[0], Index * PPI);
      MoveLongword(OldImageFlags[0], FImageFlags[0], Index);
    end;

    if Index < FCount then
    begin
      if Assigned(OldDIB) then
      begin
        MoveLongword(OldDIB.Bits[Index * PPI], DIB.Bits[(Index + NumImages) * PPI], (FCount - Index) * PPI);
        MoveLongword(OldImageFlags[Index], FImageFlags[Index + NumImages], FCount - Index);
      end
      else
      begin
        BidiMoveLongword(DIB.Bits[Index * PPI], DIB.Bits[(Index + NumImages) * PPI], (FCount - Index) * PPI);
        BidiMoveLongword(FImageFlags[Index], FImageFlags[Index + NumImages], FCount - Index);
      end;
    end;

    FCount := NewCount;
  finally
    if Assigned(OldDIB) then OldDIB.Free;
  end;
end;

procedure TDIBList32.Move(CurIndex, NewIndex, NumImages: Integer);
var
  Buffer: TRGBQuadDynArray;
  PPI: Integer;
begin
  if (CurIndex = NewIndex) or (NumImages < 1) then Exit;
  if (CurIndex < 0) or (NewIndex < 0) or (CurIndex + NumImages > Count) or
    (NewIndex + NumImages > Count) then Error(@SInvalidParameters);

  PPI := Width * Height;
  SetLength(Buffer, PPI);
  MoveLongword(DIB.Bits[CurIndex * PPI], Buffer[0], NumImages * PPI);
  if NewIndex > CurIndex then
    BidiMoveLongWord(DIB.Bits[(CurIndex + NumImages) * PPI], DIB.Bits[CurIndex * PPI], (NewIndex - CurIndex) * PPI)
  else
    BidiMoveLongWord(DIB.Bits[NewIndex * PPI], DIB.Bits[(NewIndex + NumImages) * PPI], (CurIndex - NewIndex) * PPI);
  MoveLongword(Buffer[0], DIB.Bits[NewIndex * PPI], NumImages * PPI);
end;

function TDIBList32.PrepareImport(Index: Integer; ReplaceExisting: Boolean): Integer;
begin
  if Assigned(Self) then
  begin
    if ReplaceExisting then
    begin
      if (Index >= 0) and (Index < Count) then Result := Index
      else Result := -1;
    end
    else
    begin
      if (Index < 0) or (Index >= Count) then Result := Count
      else Result := Index;
      InternalInsert(Result, 1);
    end;
  end
  else Result := -1;
end;

procedure TDIBList32.SetCapacity(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCapacity <> Value then
  begin
    DIB.SetSize(Width, FCapacity * Height, True);
    SetLength(FImageFlags, FCapacity);
    if FCount > FCapacity then FCount := FCapacity;
  end;
end;

procedure TDIBList32.SetOverlayImage(ImageIndex: Integer; OverlayIndex: TOverlayIndex);
begin
  FOverlays[OverlayIndex] := ImageIndex;
end;

procedure TDIBList32.SetSize(NewWidth, NewHeight, NewGrow: Integer);
begin
  if (NewWidth <= 0) or (NewHeight <= 0) then Error(@SInvalidParameters);

  DIB.SetSize(0, 0);
  SetLength(FImageFlags, 0);
  FCount := 0;
  FCapacity := 0;

  FWidth := NewWidth;
  FHeight := NewHeight;

  if NewGrow <= 0 then
  begin
    { calculate automatic grow rate for Grow depending on image size
      for AutoGrowBytes = 16384:
        - 64x64 (or larger) images grow with every new image
        - 32x32 images grow every 4-th image
        - 16x16 images grow every 32-nd image }
    FGrow := Max(1, (AutoGrowBytes - 1) div (Width * Height * 4) + 1);
  end
  else if NewGrow * Width * Height * 4 > MaxGrowBytes then
    FGrow := Max(1, (MaxGrowBytes - 1) div (Width * Height * 4) + 1)
  else
    FGrow := NewGrow;
end;

procedure TDIBList32.VCLDrawDisabled(Index: Integer; DC: HDC; X, Y: Integer);
begin
  DIB.VCLDrawDisabled(DC, X, Y, GetImageRect(Index));
end;


//----------------------------------------------------------------------------//

function FindColorEffect(ID: Integer): Integer;
begin
  for Result := 0 to Length(ColorEffectRegistry) - 1 do
    if ColorEffectRegistry[Result].ID = ID then Exit;
  Result := -1;
end;

function RegisterColorEffect(ID: Integer; EffectProc: TColorEffectProc): Integer;
begin
  if FindColorEffect(ID) >= 0 then
   raise Exception.CreateFmt('color effect %d already registered', [ID]);
  Result := Length(ColorEffectRegistry);
  Assert(Assigned(EffectProc));
  SetLength(ColorEffectRegistry, Result + 1);
  ColorEffectRegistry[Result].ID := ID;
  ColorEffectRegistry[Result].ApplyProc := EffectProc;
end;


{ TColorEffects }

function TColorEffects.AddEffect(ID, Param: Cardinal): Boolean;
var
  I, L: Integer;
begin
  I := FindColorEffect(ID);
  Result := I >= 0;
  if Result then
  begin
    L := Length(Effects);
    SetLength(Effects, L + 1);
    Effects[L].ID := ID;
    Effects[L].Param := Param;
    Effects[L].Proc := ColorEffectRegistry[I].ApplyProc;
  end;
end;

procedure TColorEffects.Clear;
begin
  SetLength(Effects, 0);
end;

procedure TColorEffects.Transform(var Src: Cardinal; const Dst: Cardinal);
var
  I: Integer;
begin
  if Length(Effects) > 0 then
  begin
    for I := 0 to Length(Effects) - 1 do
      Effects[I].Proc(Src, Dst, Effects[I].Param);
  end;
end;

procedure TColorEffects.Transform(var SrcFirst, DstFirst: Cardinal; Count: Integer);
var
  S, D, SrcColor, DstColor, EffectColor, ResColor: Cardinal;
  I: Integer;
begin
  if Count <= 0 then Exit;
  SrcColor := SrcFirst;
  DstColor := DstFirst;
  EffectColor := SrcColor;
  Transform(EffectColor, DstColor);
  if EffectColor <= $00FFFFFF then ResColor := DstColor
  else if EffectColor >= $FF000000 then ResColor := EffectColor
  else
  begin
    ResColor := DstColor;
    BlendPixel_ATOP_LL(ResColor, EffectColor);
  end;
  DstFirst := ResColor;

  for I := 1 to Count - 1 do
  begin
    S := PRGBQuadArray(@SrcFirst)[I];
    D := PRGBQuadArray(@DstFirst)[I];
    if (S = SrcColor) and (D = DstColor) then PRGBQuadArray(@DstFirst)[I] := ResColor
    else
    begin
      SrcColor := S;
      DstColor := D;
      EffectColor := S;
      Transform(EffectColor, DstColor);
      if EffectColor <= $00FFFFFF then ResColor := DstColor
      else if EffectColor >= $FF000000 then ResColor := EffectColor
      else
      begin
        ResColor := DstColor;
        BlendPixel_ATOP_LL(ResColor, EffectColor);
      end;
      PRGBQuadArray(@DstFirst)[I] := ResColor;
    end;
  end;
end;

{ implementation of color effects }

procedure ClrEffect_INVERT(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
begin
  Src := Src xor $00FFFFFF;
end;

procedure ClrEffect_DESATURATE(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
var
  I: Cardinal;
begin
  { Param = [0..255] degree of desaturation }
  I := ((Src shr 16 and $FF) + (Src shr 7 and $1FF) + (Src and $FF)) shr 2;
  if Param >= $FF then Src := (Src and $FF000000) or I shl 16 or I shl 8 or I
  else
  begin
    Param := Param + Param shr 7;
    I := (I * Param) shr 8;
    Param := 256 - Param;
    Src := (Src and $FF000000) or
     (((Param * (Src and $00FF00FF) + I shl 24 + I shl 8 + $007F007F) and $FF00FF00) or
     ((Src and $0000FF00) * Param + I shl 16 + $00007F00) and $00FF0000) shr 8;
  end;
end;

procedure ClrEffect_MULTIPLY(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
var
  R, G, B, R2, G2, B2, W: Cardinal;
begin
  R := Src shr 16 and $FF;
  G := Src shr  8 and $FF;
  B := Src        and $FF;
  R2 := ((R + R shr 7) * (Param and $FF0000) + $7F0000) and $FF000000;
  G2 := ((G + G shr 7) * (Param and $00FF00) + $007F00) and $00FF0000;
  B2 := ((B + B shr 7) * (Param and $0000FF) + $00007F) and $0000FF00;
  if Param >= $FF000000 then Src := (Src and $FF000000) or ((R2 or G2 or B2) shr 8)
  else
  begin
    Param := Param shr 24 + Param shr 31;
    W := 256 - Param;
    R := ((R2 shr 24) * Param + R * W + $7F) and $FF00;
    G := ((G2 shr 16) * Param + G * W + $7F) and $FF00;
    B := ((B2 shr  8) * Param + B * W + $7F) and $FF00;
    Src := (Src and $FF000000) or R shl 8 or G or B shr 8;
  end;
end;

procedure ClrEffect_ADD(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
var
  R, G, B, A: Cardinal;
begin
  { Src.C := Src.C + Param.C * Param.A
    Src.A := Src.A }
  if Param <= $00FFFFFF then Exit;
  if Param >= $FF000000 then
  begin
    R := (Src and $FF0000) + (Param and $FF0000);
    if R > $FF0000 then R := $FF0000;
    G := (Src and $00FF00) + (Param and $00FF00);
    if G > $00FF00 then G := $00FF00;
    B := (Src and $0000FF) + (Param and $0000FF);
    if B > $0000FF then B := $0000FF;
    Src := (Src and $FF000000) or R or G or B;
  end
  else
  begin
    A := Param shr 24;
    A := A + A shr 7;
    R := (Src and $FF0000) + ((A * (Param and $FF0000) + $7F0000) shr 8 and $FF0000);
    if R > $FF0000 then R := $FF0000;
    G := (Src and $00FF00) + ((A * (Param and $00FF00) + $007F00) shr 8 and $00FF00);
    if G > $00FF00 then G := $00FF00;
    B := (Src and $0000FF) + (A * (Param and $0000FF) + $00007F) shr 8;
    if B > $0000FF then B := $0000FF;
    Src := (Src and $FF000000) or R or G or B;
  end;
end;

procedure ClrEffect_OPACITY(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
begin
  { Src.A := Src.A * Param / 255 }
  Param := Param + Param shr 7;
  Src := (Src and $00FFFFFF) or
    ((Src shr 24 * Param + $7F) shl 16 and $FF000000);
end;

procedure ClrEffect_BLEND(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
var
  Fs, Fd: Cardinal;
begin
  { Src.A := Src.A
    Src.C := Src.C * (256 - Param.A*256/255) + Param.C * Param.A*255/256 }
  if Param >= $FF000000 then Src := (Src and $FF000000) or (Param and $00FFFFFF)
  else
  begin
    Fs := Param shr 24 + Param shr 31;
    Fd := 256 - Fs;
    Src := Src and $FF000000 or
      (((Fs * (Param and $FF00FF) + Fd * (Src and $FF00FF) + $7F007F) and $FF00FF00) or
      ((Fs * (Param and $00FF00) + Fd * (Src and $00FF00) + $007F00) and $00FF0000)) shr 8;
  end;
end;

procedure ClrEffect_DARKSHADOW(var Src: Cardinal; const Dst: Cardinal; Param: Cardinal);
var
  I, A: Cardinal;
begin
  {  make bright colors transparent and replaces colors with Param.C
     note: Param.A is ignored
     Src.A := Src.A if Src.I <= 128
     Src.A := 0 if Src.I >= 192
     Src.A := Src.A * (192 - I) / 64 otherwise
     Src.C := Param.C }
  if Src > $00FFFFFF then
  begin
    I := ((Src shr 16 and $FF) + (Src shr 7 and $1FF) + (Src and $FF)) shr 2;
    A := Src shr 24;
    if I >= 255 then A := 0
    else if I > 192 then A := A * (256 - I) shr 6;
    Src := A shl 24 or (Param and $00FFFFFF);
  end;
end;

procedure RegisterColorEffects;
begin
  RegisterColorEffect(CLE_INVERT, ClrEffect_INVERT);
  RegisterColorEffect(CLE_DESATURATE, ClrEffect_DESATURATE);
  RegisterColorEffect(CLE_MULTIPLY, ClrEffect_MULTIPLY);
  RegisterColorEffect(CLE_ADD, ClrEffect_ADD);
  RegisterColorEffect(CLE_OPACITY, ClrEffect_OPACITY);
  RegisterColorEffect(CLE_BLEND, ClrEffect_BLEND);
  RegisterColorEffect(CLE_DARKSHADOW, ClrEffect_DARKSHADOW);
end;

{ Streaming }

function CheckGraphicStream(Stream: TStream): Integer;
var
  BMF: TBitmapFileHeader;
  CI: TCursorOrIcon;
  SavePos: Int64;
begin
  Result := GS_UNKNOWN;
  if CanLoadPNG(Stream) then Result := GS_PNG
  else
  begin
    SavePos := Stream.Position;
    if (Stream.Read(BMF, SizeOf(BMF)) = SizeOf(BMF)) and (BMF.bfType = $4D42) then
      Result := GS_BMP;
    Stream.Position := SavePos;
    if Result = GS_UNKNOWN then
    begin
      if (Stream.Read(CI, SizeOf(CI)) = SizeOf(CI)) and (CI.wType in [rc3_StockIcon, rc3_Icon]) then
        Result := GS_ICO;
    end;
  end;
end;

procedure LoadBMPGraphic(Stream: TStream; DIB: TDIB32);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromStream(Stream);
    DIB.SetSize(Bmp.Width, Bmp.Height);
    if (DIB.Width > 0) and (DIB.Height > 0) then
      BitBlt(DIB.DC, 0, 0, DIB.Width, DIB.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    DIB.SetAlpha(DIB.ContentRect, $FF);
  finally
    Bmp.Free;
  end;
end;

procedure LoadPNGGraphic(Stream: TStream; DIB: TDIB32);
var
  Reader: TPNGReader;
begin
  Reader := TPNGReader.Create;
  try
    Reader.LoadFromStream(Stream);
    DIB.SetSize(Reader.Width, Reader.Height);
    MoveLongword(Reader.FrameBuffer^, DIB.Bits[0], Reader.Width * Reader.Height);
  finally
    Reader.Free;
  end;
end;

procedure LoadICOGraphic(Stream: TStream; DIB: TDIB32);
var
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.LoadFromStream(Stream);
    DIB.SetSize(Icon.Width, Icon.Height);
    DIB.RenderIcon(Icon.Handle, 0, 0, DIB.Width, DIB.Height, RF_EXTRACT);
  finally
    Icon.Free;
  end;
end;

procedure SavePNGGraphic(Stream: TStream; DIB: TDIB32);
var
  Writer: TPNGWriter;
begin
  if (DIB.Width = 0) or (DIB.Height = 0) then raise EStreamError.CreateRes(@SInvalidImageDimensions);
  Writer := TPNGWriter.Create(DIB.Width, DIB.Height, DIB.Width, @DIB.Bits[0]);
  try
    Writer.WriteToStream(Stream);
  finally
    Writer.Free;
  end;
end;

procedure SavePNGGraphic(const FileName: WideString; DIB: TDIB32);
var
  FileHandle: THandle;
  HandleStream: THandleStream;
begin
  FileHandle := CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE, 0,
    nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then EFCreateError.CreateResFmt(@SFCreateError, [FileName]);
  HandleStream := THandleStream.Create(FileHandle);
  try
    SavePNGGraphic(HandleStream, DIB);
  finally
    HandleStream.Free;
    FileClose(FileHandle);
  end;
end;

{ TMonoGlyph }

constructor TMonoGlyph.Create(AWidth, AHeight: Integer; const Bits);
begin
  Width := AWidth;
  Height := AHeight;
  Bitmap := CreateBitmap(Width, Height, 1, 1, @Bits);
  BitmapDC := CreateCompatibleDC(0);
  OldBitmap := SelectObject(BitmapDC, Bitmap);
end;

destructor TMonoGlyph.Destroy;
begin
  SelectObject(BitmapDC, OldBitmap);
  DeleteDC(BitmapDC);
  DeleteObject(Bitmap);
  inherited;
end;

procedure TMonoGlyph.Draw(DC: HDC; X, Y: Integer; Color: TColor);
var
  OldTextColor, OldBkColor: TColorRef;
  OldBrush, Brush: HBRUSH;
begin
  if Color = clNone then Exit;
  OldTextColor := SetTextColor(DC, $FFFFFF);
  OldBkColor := SetBkColor(DC, $000000);
  if Color < 0 then Brush := GetSysColorBrush(Color and $000000FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);
  BitBlt(DC, X, Y, Width, Height, BitmapDC, 0, 0, $B8074A);
  SelectObject(DC, OldBrush);
  DeleteObject(Brush);
  SetBkColor(DC, OldBkColor);
  SetTextColor(DC, OldTextColor);
end;

procedure TMonoGlyph.DrawTiled(DC: HDC; R: TRect; Color: TColor);
var
  OldTextColor, OldBkColor: TColorRef;
  OldBrush, Brush: HBRUSH;
  NumRows, NumCols: Integer;
  I, J, X, Y: Integer;
begin
  if (Color = clNone) or (R.Right <= R.Left) or (R.Bottom <= R.Top) or not RectVisible(DC, R) then Exit;
  OldTextColor := SetTextColor(DC, $FFFFFF);
  OldBkColor := SetBkColor(DC, $000000);
  if Color < 0 then Brush := GetSysColorBrush(Color and $000000FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);

  NumRows := (R.Bottom - R.Top + Height - 1) div Height;
  NumCols := (R.Right - R.Left + Width - 1) div Width;
  SaveDC(DC);
  with R do IntersectClipRect(DC, Left, Top, Right, Bottom);
  Y := R.Top;
  for J := 0 to NumRows - 1 do
  begin
    X := R.Left;
    for I := 0 to NumCols - 1 do
    begin
      BitBlt(DC, X, Y, Width, Height, BitmapDC, 0, 0, $B8074A);
      Inc(X, Width);
    end;
    Inc(Y, Height);
  end;
  RestoreDC(DC, -1);

  SelectObject(DC, OldBrush);
  DeleteObject(Brush);
  SetBkColor(DC, OldBkColor);
  SetTextColor(DC, OldTextColor);
end;

{ TTBXImageList }

function TTBXImageList.Add(Image: TDIB32): Integer;
begin
  Result := DIBList.ImportImage(Image);
  inherited Add(nil, nil);
end;

function TTBXImageList.AddIcon(Image: TIcon): Integer;
begin
  Result := DIBList.ImportIcon(Image.Handle);
  inherited Add(nil, nil);
end;

function TTBXImageList.AddIcon(IconHandle: HIcon): Integer;
begin
  Result := DIBList.ImportIcon(IconHandle);
  inherited Add(nil, nil);
end;

function TTBXImageList.AddImage(Value: TCustomImageList; Index: Integer): Integer;
begin
  Result := DIBList.ImportImage(Value.Handle, Index);
  inherited Add(nil, nil);
end;

procedure TTBXImageList.AddImages(Value: TCustomImageList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Value.Count - 1 do
    begin
      DIBList.ImportImage(Value.Handle, I);
      inherited Add(nil, nil);
    end;
  finally
    EndUpdate;
  end;
end;

function TTBXImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  BeginUpdate;
  try
    Result := inherited AddMasked(Image, MaskColor);
    DIBList.ImportImage(Self.Handle, Result);
  finally
    EndUpdate;
  end;
end;

procedure TTBXImageList.Assign(Source: TPersistent);
var
  S: TTBXImageList;
  L: TCustomImageList;
  I: Integer;
begin
  if Source = nil then Clear
  else if Source is TTBXImageList then
  begin
    S := TTBXImageList(Source);
    BeginUpdate;
    try
      Width := S.Width;
      Height := S.Height;
      inherited Clear;
      DIBList.Assign(S.DIBList);
      for I := 0 to DIBList.Count - 1 do
        inherited Add(nil, nil);
    finally
      EndUpdate;
    end;
  end
  else if Source is TCustomImageList then
  begin
    L := TCustomImageList(Source);
    BeginUpdate;
    try
      Width := L.Width;
      Height := L.Height;
      inherited Clear;
      DIBList.SetSize(Width, Height);
      for I := 0 to L.Count - 1 do
        Self.AddImage(L, I);
    finally
      EndUpdate;
    end;
  end
  else inherited;
end;

procedure TTBXImageList.AssignTo(Dest: TPersistent);
begin
  inherited;
end;

procedure TTBXImageList.BeginUpdate;
begin
  Inc(TCustomImageListCrack(Pointer(Self)).FUpdateCount);
end;

procedure TTBXImageList.Change;
begin
  if (TCustomImageListCrack(Pointer(Self)).FUpdateCount = 0) and (FChangeLock = 0) then
  begin
    if (Width <> DIBList.Width) or (Height <> DIBList.Height) then
    begin
      DIBList.SetSize(Width, Height, Max(1, AllocBy));
    end;
  end;
  inherited;
end;

procedure TTBXImageList.Clear;
begin
  BeginUpdate;
  try
    inherited Clear;
    DIBList.DeleteAll;
  finally
    EndUpdate;
  end;
end;

constructor TTBXImageList.Create(AOwner: TComponent);
begin
  FChangeLock := 1;
  inherited Create(AOwner);
  FDIBList := TDIBList32.Create(Width, Height, AllocBy);
  FChangeLock := 0;
end;

procedure TTBXImageList.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Info: Longint;

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TTBXImageList) or
        not DIBList.Equal(TTBXImageList(Filer.Ancestor).DIBList)
    else
      Result := Count > 0;
  end;

begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft,
    LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop,
    LongRec(DesignInfo).Hi <> LongRec(Info).Hi);

  Filer.DefineBinaryProperty('Bitmap', ReadData, nil, False);
  Filer.DefineBinaryProperty('PngDIB', ReadPNG, WritePNG, DoWrite);
end;

procedure TTBXImageList.Delete(Index: Integer);
begin
  DIBList.Delete(Index);
  inherited Delete(Index);
end;

destructor TTBXImageList.Destroy;
begin
  Inc(FChangeLock);
  FDIBList.Free;
  inherited;
end;

procedure TTBXImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);

  function GetRGBColor(Value: TColor): Cardinal;
  begin
    Result := ColorToRGB(Value);
    case Result of
      clNone: Result := CLR_NONE;
      clDefault: Result := CLR_DEFAULT;
    end;
  end;

begin
  if (Index >= 0) and (Index < Count) then
    if Enabled then
      DIBList.APIDrawEx(Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
      DIBList.DIB.VCLDrawDisabled(Canvas.Handle, X, Y, DIBList.GetImageRect(Index));
end;

procedure TTBXImageList.EndUpdate;
begin
  with TCustomImageListCrack(Pointer(Self)) do
  begin
    if FUpdateCount > 0 then Dec(FUpdateCount);
    if FChanged then
    begin
      FChanged := False;
      Change;
    end;
  end;
end;

procedure TTBXImageList.Insert(Index: Integer; Image, Mask: TBitmap);
begin
  BeginUpdate;
  try
    inherited Insert(Index, Image, Mask);
    DIBList.ImportImage(Handle, Index, Index, False);
  finally
    EndUpdate;
  end;
end;

procedure TTBXImageList.InsertIcon(Index: Integer; Image: TIcon);
begin
  DIBList.ImportIcon(Image.Handle, Index);
  inherited Add(nil, nil);
end;

procedure TTBXImageList.InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
begin
  BeginUpdate;
  try
    inherited InsertMasked(Index, Image, MaskColor);
    DIBList.ImportImage(Handle, Index, Index);
  finally
    EndUpdate;
  end;
end;

procedure TTBXImageList.Move(CurIndex, NewIndex: Integer);
begin
  DIBList.Move(CurIndex, NewIndex);
  Change;
end;

procedure TTBXImageList.ReadData(Stream: TStream);
var
  I: Integer;
  {$IFNDEF JR_D7}HF: THackerFiler;{$ENDIF}
begin
  if not FImagesLoaded then
  begin
    FImagesLoaded := True;
    BeginUpdate;
    try
      {$IFDEF JR_D7}
      inherited ReadData(Stream);
      {$ELSE}
      HF := THackerFiler.Create(nil, 0);
      try
        inherited DefineProperties(HF);
        if Assigned(HF.ReaderProc) then HF.ReaderProc(Stream);
      finally
        HF.Free;
      end;
      {$ENDIF}
      for I := 0 to Count - 1 do
        DIBList.ImportImage(Handle, I);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TTBXImageList.ReadLeft(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Lo := Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TTBXImageList.ReadPNG(Stream: TStream);
var
  I, N: Integer;
  D: TDIB32;
  R: TRect;
begin
  if not FImagesLoaded then
  begin
    FImagesLoaded := True;
    Stream.ReadBuffer(N, 4);
    if N > 0 then
    begin
      BeginUpdate;
      try
        D := TDIB32.Create;
        try
          LoadPNGGraphic(Stream, D);
          R.Left := 0;
          R.Top := 0;
          R.Right := Width;
          R.Bottom := Height;
          if (D.Width = Width) and (D.Height = Height * N) then
            for I := 0 to N - 1 do
            begin
              DIBList.ImportImage(D, R);
              inherited Add(nil, nil);
              R.Top := R.Bottom;
              R.Bottom := R.Top + Height;
            end;
        finally
          D.Free;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TTBXImageList.ReadTop(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Hi := Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TTBXImageList.Replace(Index: Integer; Image, Mask: TBitmap);
begin
  BeginUpdate;
  try
    inherited Replace(Index, Image, Mask);
    DIBList.ImportImage(Handle, Index, Index, True);
  finally
    EndUpdate;
  end;
end;

procedure TTBXImageList.ReplaceIcon(Index: Integer; Image: TIcon);
begin
  DIBList.ImportIcon(Image.Handle, Index, True);
  Change;
end;

procedure TTBXImageList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
begin
  BeginUpdate;
  try
    inherited ReplaceMasked(Index, NewImage, MaskColor);
    DIBList.ImportImage(Handle, Index, Index, True);
  finally
    EndUpdate;
  end;
end;

procedure TTBXImageList.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TTBXImageList.WritePNG(Stream: TStream);
var
  N: Integer;
  DIB: TDIB32;
begin
  N := DIBList.Count;
  Stream.WriteBuffer(N, 4);
  if N > 0 then
  begin
    DIB := TDIB32.Create;
    try
      DIB.SetSize(DIBList.Width, DIBList.Height * DIBList.Count);
      DIBList.DIB.CopyTo(DIB, 0, 0, Rect(0, 0, DIBList.Width, DIBList.Height * DIBList.Count));
      SavePNGGraphic(Stream, DIB);
    finally
      DIB.Free;
    end;
  end;
end;

procedure TTBXImageList.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

//----------------------------------------------------------------------------//

procedure RegisterGraphicFormats;
begin
  TPicture.RegisterFileFormat('png', 'Portable network graphic images', TPNGBitmap);
end;

procedure UnregisterGraphicFormats;
begin
  TPicture.UnregisterGraphicClass(TPNGBitmap);
end;


initialization
  PrepareTables;
  RegisterColorEffects;
  RegisterGraphicFormats;

finalization
  UnregisterGraphicFormats;

end.
