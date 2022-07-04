{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDEBitmap;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, SCDECommon;

type
  TSCDeBitmapImage = class(TSharedImage)
  private
    FHandle: HBITMAP;
    FMaskHandle: HBITMAP;
    FPalette: HPALETTE;
    FDIBHandle: HBITMAP;
    FDIB: TDIBSection;
    FSaveStream: TMemoryStream;
    FOS2Format: Boolean;
    FHalftone: Boolean;
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TSCDeBitmap = class(TGraphic)
  private
    FImage: TSCDeBitmapImage;
    FCanvas: TCanvas;
    FIgnorePalette: Boolean;
    FMaskBitsValid: Boolean;
    FMaskValid: Boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    procedure Changing(Sender: TObject);
    procedure CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
    procedure DIBNeeded;
    procedure FreeContext;
    function  GetCanvas: TCanvas;
    function  GetHandle: HBITMAP; virtual;
    function  GetHandleType: TBitmapHandleType;
    function  GetMaskHandle: HBITMAP; virtual;
    function  GetMonochrome: Boolean;
    function  GetPixelFormat: TPixelFormat;
    function  GetScanline(Row: Integer): Pointer;
    function  GetTransparentColor: TColor;
    procedure NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
      const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
    procedure ReadStream(Stream: TStream; Size: Longint);
    procedure ReadDIB(Stream: TStream; ImageSize: LongWord);
    procedure SetHandle(Value: HBITMAP);
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMaskHandle(Value: HBITMAP);
    procedure SetMonochrome(Value: Boolean);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    function  TransparentColorStored: Boolean;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean);
  protected
    procedure Changed(Sender: TObject); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function  GetEmpty: Boolean; override;
    function  GetHeight: Integer; override;
    function  GetPalette: HPALETTE; override;
    function  GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadData(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Dormant;
    procedure FreeImage;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure Mask(TransparentColor: TColor);
    function  ReleaseHandle: HBITMAP;
    function  ReleaseMaskHandle: HBITMAP;
    function  ReleasePalette: HPALETTE;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetBounds(AWidth, AHeight: Integer); dynamic;

    property Canvas: TCanvas read GetCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property IgnorePalette: Boolean read FIgnorePalette write FIgnorePalette;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property TransparentColor: TColor read GetTransparentColor
      write SetTransparentColor stored TransparentColorStored;
    property TransparentMode: TTransparentMode read FTransparentMode
      write SetTransparentMode default tmAuto;
  end;

implementation

uses Consts;

var
  BitmapImageLock: TRTLCriticalSection;
  BitmapCanvasList: TThreadList = nil;

{ TSCDeBitmap }

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;

  TSCDeFakeCanvas = class(TCanvas);

{ TSCDeBitmapCanvas }
{ Create a canvas that gets its DC from the memory DC cache }
  TSCDeBitmapCanvas = class(TCanvas)
  private
    FHandle: HDC;
    FBitmap: TSCDeBitmap;
    FOldBitmap: HBITMAP;
    FOldPalette: HPALETTE;
    procedure FreeContext;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TSCDeBitmap);
    destructor Destroy; override;
  end;

const
  csAllValid = [csHandleValid..csBrushValid];

procedure InvalidOperation(const Str: String);
begin
  raise EInvalidGraphicOperation.Create(Str);
end;

procedure InvalidGraphic(const Str: String);
begin
  raise EInvalidGraphic.Create(Str);
end;

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure InvalidBitmap;
begin
  InvalidGraphic(SInvalidBitmap);
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

procedure InternalDeletePalette(Pal: HPalette);
begin
  if (Pal <> 0) and (Pal <> SystemPalette16) then
    DeleteObject(Pal);
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then InvalidBitmap
  else if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;

{ RGBTripleToQuad performs in-place conversion of an OS2 color
  table into a DIB color table.   }
procedure RGBTripleToQuad(var ColorTable);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 255 downto 1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin                     // order is significant for last item moved
      rgbRed := rgbtRed;
      rgbGreen := rgbtGreen;
      rgbBlue := rgbtBlue;
      rgbReserved := 0;
    end;
  P4^[0].rgbReserved := 0;
end;

{ RGBQuadToTriple performs the inverse of RGBTripleToQuad. }
procedure RGBQuadToTriple(var ColorTable; var ColorCount: Integer);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 1 to ColorCount-1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin
      rgbtRed := rgbRed;
      rgbtGreen := rgbGreen;
      rgbtBlue := rgbBlue;
    end;
  if ColorCount < 256 then
  begin
    FillChar(P3^[ColorCount], (256 - ColorCount) * sizeof(TRGBTriple), 0);
    ColorCount := 256;   // OS2 color tables always have 256 entries
  end;
end;

procedure ByteSwapColors(var Colors; Count: Integer);
var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  asm
        MOV   EDX, Colors
        MOV   ECX, Count
        DEC   ECX
        JS    @@END
        LEA   EAX, SysInfo
        CMP   [EAX].TSystemInfo.wProcessorLevel, 3
        JE    @@386
  @@1:  MOV   EAX, [EDX+ECX*4]
        BSWAP EAX
        SHR   EAX,8
        MOV   [EDX+ECX*4],EAX
        DEC   ECX
        JNS   @@1
        JMP   @@END
  @@386:
        PUSH  EBX
  @@2:  XOR   EBX,EBX
        MOV   EAX, [EDX+ECX*4]
        MOV   BH, AL
        MOV   BL, AH
        SHR   EAX,16
        SHL   EBX,8
        MOV   BL, AL
        MOV   [EDX+ECX*4],EBX
        DEC   ECX
        JNS   @@2
        POP   EBX
    @@END:
  end;
end;

function SystemPaletteOverride(var Pal: TMaxLogPalette): Boolean;
var
  DC: HDC;
  SysPalSize: Integer;
begin
  Result := False;
  if SystemPalette16 <> 0 then
  begin
    DC := GetDC(0);
    try
      SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
      if SysPalSize >= 16 then
      begin
        { Ignore the disk image of the palette for 16 color bitmaps.
          Replace with the first and last 8 colors of the system palette }
        GetPaletteEntries(SystemPalette16, 0, 8, Pal.palPalEntry);
        GetPaletteEntries(SystemPalette16, 8, 8, Pal.palPalEntry[Pal.palNumEntries - 8]);
        Result := True;
      end
    finally
      ReleaseDC(0,DC);
    end;
  end;
end;

function PaletteFromDIBColorTable(DIBHandle: THandle; ColorTable: Pointer;
  ColorCount: Integer): HPalette;
var
  DC: HDC;
  Save: THandle;
  Pal: TMaxLogPalette;
begin
  Result := 0;
  Pal.palVersion := $300;
  if DIBHandle <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    Save := SelectObject(DC, DIBHandle);
    Pal.palNumEntries := GetDIBColorTable(DC, 0, 256, Pal.palPalEntry);
    SelectObject(DC, Save);
    DeleteDC(DC);
  end
  else
  begin
    Pal.palNumEntries := ColorCount;
    Move(ColorTable^, Pal.palPalEntry, ColorCount * 4);
  end;
  if Pal.palNumEntries = 0 then Exit;
  if (Pal.palNumEntries <> 16) or not SystemPaletteOverride(Pal) then
    ByteSwapColors(Pal.palPalEntry, Pal.palNumEntries);
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

function PaletteToDIBColorTable(Pal: HPalette;
  var ColorTable: array of TRGBQuad): Integer;
begin
  Result := 0;
  if (Pal = 0) or
     (GetObject(Pal, sizeof(Result), @Result) = 0) or
     (Result = 0) then Exit;
  if Result > High(ColorTable)+1 then Result := High(ColorTable)+1;
  GetPaletteEntries(Pal, 0, Result, ColorTable);
  ByteSwapColors(ColorTable, Result);
end;

procedure UpdateDIBColorTable(DIBHandle: HBITMAP; Pal: HPalette;
  const DIB: TDIBSection);
var
  ScreenDC, DC: HDC;
  OldBM: HBitmap;
  ColorCount: Integer;
  Colors: array [Byte] of TRGBQuad;
begin
  if (DIBHandle <> 0) and (DIB.dsbmih.biBitCount <= 8) then
  begin
    ColorCount := PaletteToDIBColorTable(Pal, Colors);
    if ColorCount = 0 then Exit;
    ScreenDC := GetDC(0);
    DC := CreateCompatibleDC(ScreenDC);
    OldBM := SelectObject(DC, DIBHandle);
    try
      SetDIBColorTable(DC, 0, ColorCount, Colors);
    finally
      SelectObject(DC, OldBM);
      DeleteDC(DC);
      ReleaseDC(0, ScreenDC);
    end;
  end;
end;

procedure FixupBitFields(var DIB: TDIBSection);
begin
  if (DIB.dsbmih.biCompression and BI_BITFIELDS <> 0) and
    (DIB.dsBitFields[0] = 0) then
    if DIB.dsbmih.biBitCount = 16 then
    begin
      // fix buggy 16 bit color drivers
      DIB.dsBitFields[0] := $F800;
      DIB.dsBitFields[1] := $07E0;
      DIB.dsBitFields[2] := $001F;
    end else if DIB.dsbmih.biBitCount = 32 then
    begin
      // fix buggy 32 bit color drivers
      DIB.dsBitFields[0] := $00FF0000;
      DIB.dsBitFields[1] := $0000FF00;
      DIB.dsBitFields[2] := $000000FF;
    end;
end;

{ DeselectBitmap is called to ensure that a bitmap handle is not
  selected into any memory DC anywhere in the system.  If the bitmap
  handle is in use by a locked canvas, DeselectBitmap must wait for
  the canvas to unlock. }

procedure DeselectBitmap(AHandle: HBITMAP);
var
  I: Integer;
begin
  if AHandle = 0 then Exit;
  with BitmapCanvasList.LockList do
  try
    for I := Count - 1 downto 0 do
      with TSCDeBitmapCanvas(Items[I]) do
        if (FBitmap <> nil) and (FBitmap.FImage.FHandle = AHandle) then
          FreeContext;
  finally
    BitmapCanvasList.UnlockList;
  end;
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

function CopyBitmap(Handle: HBITMAP; OldPalette, NewPalette: HPALETTE;
  var DIB: TDIBSection; Canvas: TCanvas): HBITMAP;
var
  OldScr, NewScr: HBITMAP;
  ScreenDC, NewImageDC, OldImageDC: HDC;
  BI: PBitmapInfo;
  BitsMem: Pointer;
  SrcDIB: TDIBSection;
  MonoColors: array [0..1] of Integer;
  Pal1, Pal2: HPalette;
begin
  Result := 0;
  with DIB, dsbm, dsbmih do
  begin
    if (biSize <> 0) and ((biWidth = 0) or (biHeight = 0)) then Exit;
    if (biSize = 0) and ((bmWidth = 0) or (bmHeight = 0)) then Exit;
  end;

  DeselectBitmap(Handle);

  SrcDIB.dsbmih.biSize := 0;
  if Handle <> 0 then
    if GetObject(Handle, sizeof(SrcDIB), @SrcDIB) < sizeof(SrcDIB.dsbm) then
      InvalidBitmap;

  ScreenDC := GDICheck(GetDC(0));
  NewImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
  with DIB.dsbm do
  try
    if DIB.dsbmih.biSize < DWORD(sizeof(DIB.dsbmih)) then
      if (bmPlanes or bmBitsPixel) = 1 then // monochrome
        Result := GDICheck(CreateBitmap(bmWidth, bmHeight, 1, 1, nil))
      else  // Create DDB
        Result := GDICheck(CreateCompatibleBitmap(ScreenDC, bmWidth, bmHeight))
    else  // Create DIB
    begin
      GetMem(BI, sizeof(TBitmapInfo) + 256 * sizeof(TRGBQuad));
      with DIB.dsbmih do
      try
        biSize := sizeof(BI.bmiHeader);
        biPlanes := 1;
        if biBitCount = 0 then
          biBitCount := GetDeviceCaps(ScreenDC, BITSPIXEL) * GetDeviceCaps(ScreenDC, PLANES);
        BI.bmiHeader := DIB.dsbmih;
        bmWidth := biWidth;
        bmHeight := biHeight;

        if (biBitCount <= 8) then
        begin
          if (biBitCount = 1) and (SrcDIB.dsbm.bmBits = nil) then
          begin  // set mono DIB to white/black when converting from DDB.
            Integer(BI^.bmiColors[0]) := 0;
            PInteger(Integer(@BI^.bmiColors) + sizeof(Integer))^ := $FFFFFF;
          end
          else if (NewPalette <> 0) then
            PaletteToDIBColorTable(NewPalette, PRGBQuadArray(@BI.bmiColors)^)
          else if Handle <> 0 then
          begin
            NewScr := SelectObject(NewImageDC, Handle);
            if (SrcDIB.dsbmih.biSize > 0) and (SrcDIB.dsbm.bmBits <> nil) then
              biClrUsed := GetDIBColorTable(NewImageDC, 0, 256, BI^.bmiColors)
            else
              GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), nil, BI^, DIB_RGB_COLORS);
            SelectObject(NewImageDC, NewScr);
          end;
        end
        else if ((biBitCount = 16) or (biBitCount = 32)) and
          ((biCompression and BI_BITFIELDS) <> 0) then
        begin
          FixupBitFields(DIB);
          Move(DIB.dsBitFields, BI.bmiColors, sizeof(DIB.dsBitFields));
        end;

        Result := GDICheck(CreateDIBSection(ScreenDC, BI^, DIB_RGB_COLORS, BitsMem, 0, 0));
        if (BitsMem = nil) then GDIError;

        if (Handle <> 0) and (SrcDIB.dsbm.bmWidth = biWidth) and
          (SrcDIB.dsbm.bmHeight = biHeight) and (biBitCount > 8) then
        begin    // shortcut bitblt steps
          GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), BitsMem, BI^, DIB_RGB_COLORS);
          Exit;
        end;
      finally
        FreeMem(BI);
      end;
    end;

    GDICheck(Result);
    NewScr := GDICheck(SelectObject(NewImageDC, Result));
    try
      try
        Pal1 := 0;
        Pal2 := 0;
        if NewPalette <> 0 then
        begin
          Pal1 := SelectPalette(NewImageDC, NewPalette, False);
          RealizePalette(NewImageDC);
        end;
        try
          if Canvas <> nil then
          begin
            FillRect(NewImageDC, Rect(0, 0, bmWidth, bmHeight),
              Canvas.Brush.Handle);
            SetTextColor(NewImageDC, ColorToRGB(Canvas.Font.Color));
            SetBkColor(NewImageDC, ColorToRGB(Canvas.Brush.Color));
            if (DIB.dsbmih.biBitCount = 1) and (DIB.dsbm.bmBits <> nil) then
            begin
              MonoColors[0] := ColorToRGB(Canvas.Font.Color);
              MonoColors[1] := ColorToRGB(Canvas.Brush.Color);
              SetDIBColorTable(NewImageDC, 0, 2, MonoColors);
            end;
          end
          else
            PatBlt(NewImageDC, 0, 0, bmWidth, bmHeight, WHITENESS);
          if Handle <> 0 then
          begin
            OldImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              OldScr := GDICheck(SelectObject(OldImageDC, Handle));
              if OldPalette <> 0 then
              begin
                Pal2 := SelectPalette(OldImageDC, OldPalette, False);
                RealizePalette(OldImageDC);
              end;
              if Canvas <> nil then
              begin
                SetTextColor(OldImageDC, ColorToRGB(Canvas.Font.Color));
                SetBkColor(OldImageDC, ColorToRGB(Canvas.Brush.Color));
              end;
              BitBlt(NewImageDC, 0, 0, bmWidth, bmHeight, OldImageDC, 0, 0, SRCCOPY);
              if OldPalette <> 0 then
                SelectPalette(OldImageDC, Pal2, True);
              GDICheck(SelectObject(OldImageDC, OldScr));
            finally
              DeleteDC(OldImageDC);
            end;
          end;
        finally
          if NewPalette <> 0 then
            SelectPalette(NewImageDC, Pal1, True);
        end;
      finally
        SelectObject(NewImageDC, NewScr);
      end;
    except
      DeleteObject(Result);
      raise;
    end;
  finally
    DeleteDC(NewImageDC);
    ReleaseDC(0, ScreenDC);
    if (Result <> 0) then GetObject(Result, sizeof(DIB), @DIB);
  end;
end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;

function CopyBitmapAsMask(Handle: HBITMAP; Palette: HPALETTE;
  TransparentColor: TColorRef): HBITMAP;
var
  DIB: TDIBSection;
  ScreenDC, BitmapDC, MonoDC: HDC;
  BkColor: TColorRef;
  Remove: Boolean;
  SaveBitmap, SaveMono: HBITMAP;
begin
  Result := 0;
  if (Handle <> 0) and (GetObject(Handle, SizeOf(DIB), @DIB) <> 0) then
  begin
    DeselectBitmap(Handle);
    ScreenDC := 0;
    MonoDC := 0;
    try
      ScreenDC := GDICheck(GetDC(0));
      MonoDC := GDICheck(CreateCompatibleDC(ScreenDC));
      with DIB, dsBm do
      begin
        Result := CreateBitmap(bmWidth, bmHeight, 1, 1, nil);
        if Result <> 0 then
        begin
          SaveMono := SelectObject(MonoDC, Result);
          if TransparentColor = TColorRef(clNone) then
            PatBlt(MonoDC, 0, 0, bmWidth, bmHeight, Blackness)
          else
          begin
            BitmapDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              { Convert DIB to DDB }
              if bmBits <> nil then
              begin
                Remove := True;
                DIB.dsbmih.biSize := 0;
                Handle := CopyBitmap(Handle, Palette, Palette, DIB, nil);
              end
              else Remove := False;
              SaveBitmap := SelectObject(BitmapDC, Handle);
              if Palette <> 0 then
              begin
                SelectPalette(BitmapDC, Palette, False);
                RealizePalette(BitmapDC);
                SelectPalette(MonoDC, Palette, False);
                RealizePalette(MonoDC);
              end;
              BkColor := SetBkColor(BitmapDC, TransparentColor);
              BitBlt(MonoDC, 0, 0, bmWidth, bmHeight, BitmapDC, 0, 0, SrcCopy);
              SetBkColor(BitmapDC, BkColor);
              if SaveBitmap <> 0 then SelectObject(BitmapDC, SaveBitmap);
              if Remove then DeleteObject(Handle);
            finally
              DeleteDC(BitmapDC);
            end;
          end;
          if SaveMono <> 0 then SelectObject(MonoDC, SaveMono);
        end;
      end;
    finally
      if MonoDC <> 0 then DeleteDC(MonoDC);
      if ScreenDC <> 0 then ReleaseDC(0, ScreenDC);
    end;
  end;
end;

procedure TSCDeBitmap.SetBounds(AWidth, AHeight: Integer);
var
  DIB: TDIBSection;
begin
  with FImage do
    if (FDIB.dsbm.bmWidth <> AWidth) or (FDIB.dsbm.bmHeight <> AHeight) then
    begin
      HandleNeeded;
      DIB := FDIB;
      DIB.dsbm.bmWidth := AWidth;
      DIB.dsbmih.biWidth := AWidth;
      DIB.dsbm.bmHeight := AHeight;
      DIB.dsbmih.biHeight := AHeight;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

{ TSCDeBitmapImage }

destructor TSCDeBitmapImage.Destroy;
begin
  if FDIBHandle <> 0 then
  begin
    DeselectBitmap(FDIBHandle);
    DeleteObject(FDIBHandle);
    FDIBHandle := 0;
  end;
  FreeHandle;
  if FDIB.dshSection <> 0 then CloseHandle(FDIB.dshSection);
  FreeAndNil(FSaveStream);
  inherited Destroy;
end;

procedure TSCDeBitmapImage.FreeHandle;
begin
  if (FHandle <> 0) and (FHandle <> FDIBHandle) then
  begin
    DeselectBitmap(FHandle);
    DeleteObject(FHandle);
  end;
  if FMaskHandle <> 0 then
  begin
    DeselectBitmap(FMaskHandle);
    DeleteObject(FMaskHandle);
    FMaskHandle := 0;
  end;
  InternalDeletePalette(FPalette);
  FHandle := 0;
  FPalette := 0;
end;

{ TSCDeBitmapCanvas }

constructor TSCDeBitmapCanvas.Create(ABitmap: TSCDeBitmap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

destructor TSCDeBitmapCanvas.Destroy;
begin
  FreeContext;
  inherited Destroy;
end;

procedure TSCDeBitmapCanvas.FreeContext;
var
  H: HBITMAP;
begin
  if FHandle <> 0 then
  begin
    Lock;
    try
      if FOldBitmap <> 0 then SelectObject(FHandle, FOldBitmap);
      if FOldPalette <> 0 then SelectPalette(FHandle, FOldPalette, True);
      H := FHandle;
      Handle := 0;
      DeleteDC(H);
      BitmapCanvasList.Remove(Self);
    finally
      Unlock;
    end;
  end;
end;

procedure TSCDeBitmapCanvas.CreateHandle;
var
  H: HBITMAP;
begin
  if FBitmap <> nil then
  begin
    Lock;
    try
      FBitmap.HandleNeeded;
      DeselectBitmap(FBitmap.FImage.FHandle);

      FBitmap.PaletteNeeded;
      H := CreateCompatibleDC(0);

      if FBitmap.FImage.FHandle <> 0 then
        FOldBitmap := SelectObject(H, FBitmap.FImage.FHandle) else
        FOldBitmap := 0;
      if FBitmap.FImage.FPalette <> 0 then
      begin
        FOldPalette := SelectPalette(H, FBitmap.FImage.FPalette, True);
        RealizePalette(H);
      end
      else
        FOldPalette := 0;

      Handle := H;
      FHandle := Handle;

      BitmapCanvasList.Add(Self);
    finally
      Unlock;
    end;
  end;
end;

{ TSCDeBitmap }

constructor TSCDeBitmap.Create;
begin
  inherited Create;
  FTransparentColor := clDefault;
  FImage := TSCDeBitmapImage.Create;
  FImage.Reference;
  if DDBsOnly then HandleType := bmDDB;
end;

destructor TSCDeBitmap.Destroy;
begin
  FreeContext;
  FImage.Release;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TSCDeBitmap.Assign(Source: TPersistent);
var
  DIB: TDIBSection;
begin
  if (Source = nil) or (Source is TSCDeBitmap) then
  begin
    EnterCriticalSection(BitmapImageLock);
    try
      if Source <> nil then
      begin
        TSCDeBitmap(Source).FImage.Reference;
        FImage.Release;
        FImage := TSCDeBitmap(Source).FImage;
        Transparent := TSCDeBitmap(Source).Transparent;
        FTransparentColor := TSCDeBitmap(Source).FTransparentColor;
        FTransparentMode := TSCDeBitmap(Source).FTransparentMode;
      end
      else
      begin
        FillChar(DIB, Sizeof(DIB), 0);
        NewImage(0, 0, DIB, False);
      end;
    finally
      LeaveCriticalSection(BitmapImageLock);
    end;
    PaletteModified := Palette <> 0;
    Changed(Self);
  end
  else inherited Assign(Source);
end;

procedure TSCDeBitmap.CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
var
  NewHandle, NewPalette: THandle;
begin
  FreeContext;
  NewHandle := 0;
  NewPalette := 0;
  try
    if APalette = SystemPalette16 then
      NewPalette := APalette
    else
      NewPalette := CopyPalette(APalette);
    NewHandle := CopyBitmap(AHandle, APalette, NewPalette, DIB, FCanvas);
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
end;

{ Called by the FCanvas whenever an operation is going to be performed on the
  bitmap that would modify it.  Since modifications should only affect this
  TSCDeBitmap, the handle needs to be 'cloned' if it is being refered to by more
  than one TSCDeBitmap }
procedure TSCDeBitmap.Changing(Sender: TObject);
begin
  FreeImage;
  FImage.FDIB.dsbmih.biClrUsed := 0;
  FImage.FDIB.dsbmih.biClrImportant := 0;
  FreeAndNil(FImage.FSaveStream);
end;

procedure TSCDeBitmap.Changed(Sender: TObject);
begin
  FMaskBitsValid := False;
  inherited Changed(Sender);
end;

procedure TSCDeBitmap.Dormant;
begin
  FreeContext; // !! InternalDeletePalette fails without this
  DIBNeeded;
  FImage.FreeHandle;
end;

procedure TSCDeBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  MaskDC: HDC;
  Save: THandle;
begin
  with Rect, FImage do
  begin
    TSCDeFakeCanvas(ACanvas).RequiredState(csAllValid);
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if FPalette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.Handle, FPalette, True);
      RealizePalette(ACanvas.Handle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.Handle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.Handle, PLANES);
    DoHalftone := (BPP <= 8) and (BPP < (FDIB.dsbm.bmBitsPixel * FDIB.dsbm.bmPlanes));
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.Handle, pt);
      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      SetBrushOrgEx(ACanvas.Handle, pt.x, pt.y, @pt);
    end else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      { Call MaskHandleNeeded prior to creating the canvas handle since
        it causes FreeContext to be called. }
      if Transparent then MaskHandleNeeded;
      TSCDeFakeCanvas(Canvas).RequiredState(csAllValid);
      if Transparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := GDICheck(CreateCompatibleDC(0));
          Save := SelectObject(MaskDC, FMaskHandle);
          TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left,
            Bottom - Top, Canvas.Handle, 0, 0, FDIB.dsbm.bmWidth,
            FDIB.dsbm.bmHeight, MaskDC, 0, 0);
        finally
          if Save <> 0 then SelectObject(MaskDC, Save);
          if MaskDC <> 0 then DeleteDC(MaskDC);
        end;
      end
      else
        StretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle, 0, 0, FDIB.dsbm.bmWidth,
          FDIB.dsbm.bmHeight, ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.Handle, OldPalette, True);
    end;
  end;
end;

{ FreeImage:
  If there are multiple references to the image, create a unique copy of the image.
  If FHandle = FDIBHandle, the DIB memory will be updated when the drawing
  handle is drawn upon, so no changes are needed to maintain image integrity.
  If FHandle <> FDIBHandle, the DIB will not track with changes made to
  the DDB, so destroy the DIB handle (but keep the DIB pixel format info).  }
procedure TSCDeBitmap.FreeImage;
var
  P: HPalette;
begin
  with FImage do
    if RefCount > 1 then
    begin
      HandleNeeded;
      if FHalftone then
        P := 0
      else
        P := FPalette;
      CopyImage(FHandle, P, FDIB)
    end
    else if (FHandle <> 0) and (FHandle <> FDIBHandle) then
    begin
      if FDIBHandle <> 0 then
        if not DeleteObject(FDIBHandle) then GDIError;
      FDIBHandle := 0;
      FDIB.dsbm.bmBits := nil;
    end;
end;

function TSCDeBitmap.GetEmpty;
begin
  with FImage do
    Result := (FHandle = 0) and (FDIBHandle = 0);
end;

function TSCDeBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    HandleNeeded;
    FCanvas := TSCDeBitmapCanvas.Create(Self);
    FCanvas.OnChange := Changed;
    FCanvas.OnChanging := Changing;
  end;
  Result := FCanvas;
end;

{ Since the user might modify the contents of the HBITMAP it must not be
  shared by another TSCDeBitmap when given to the user nor should it be selected
  into a DC. }
function TSCDeBitmap.GetHandle: HBITMAP;
begin
  FreeContext;
  HandleNeeded;
  Changing(Self);
  Result := FImage.FHandle;
end;

function TSCDeBitmap.GetHandleType: TBitmapHandleType;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FHandle = FDIBHandle) then
      if FDIBHandle = 0 then
        if FDIB.dsbmih.biSize = 0 then
          Result := bmDDB
        else
          Result := bmDIB
      else
        Result := bmDIB
    else
      Result := bmDDB;
  end;
end;

function TSCDeBitmap.GetHeight: Integer;
begin
  Result := Abs(FImage.FDIB.dsbm.bmHeight);
end;

function TSCDeBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FImage.FMaskHandle;
end;

function TSCDeBitmap.GetMonochrome: Boolean;
begin
  with FImage.FDIB.dsbm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

function TSCDeBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := FImage.FPalette;
end;

function TSCDeBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := pfCustom;
  if HandleType = bmDDB then
    Result := pfDevice
  else
    with FImage.FDIB, dsbmih do
      case biBitCount of
        1: Result := pf1Bit;
        4: Result := pf4Bit;
        8: Result := pf8Bit;
       16: case biCompression of
             BI_RGB : Result := pf15Bit;
             BI_BITFIELDS: if dsBitFields[1] = $7E0 then Result := pf16Bit;
           end;
       24: Result := pf24Bit;
       32: if biCompression = BI_RGB then Result := pf32Bit;
      end;
end;

function TSCDeBitmap.GetScanLine(Row: Integer): Pointer;
begin
  Changing(Self);
  with FImage.FDIB, dsbm, dsbmih do
  begin
    if (Row < 0) or (Row > bmHeight) then
      InvalidOperation(SScanLine);

    DIBNeeded;
    GDIFlush;
    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Integer(Result) := Integer(bmBits) +
      Row * BytesPerScanline(biWidth, biBitCount, 32);
  end;
end;

function TSCDeBitmap.GetTransparentColor: TColor;
begin
  if FTransparentColor = clDefault then
  begin
    if Monochrome then
      Result := clWhite
    else
      Result := Canvas.Pixels[0, Height - 1];
  end
  else Result := ColorToRGB(FTransparentColor);
  Result := Result or $02000000;
end;

function TSCDeBitmap.GetWidth: Integer;
begin
  Result := FImage.FDIB.dsbm.bmWidth;
end;

procedure TSCDeBitmap.DIBNeeded;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FDIBHandle <> 0) then Exit;
    PaletteNeeded;
    if FDIB.dsbmih.biSize = 0 then
    begin
      GetObject(FHandle, sizeof(FDIB), @FDIB);
      with FDIB, dsbm, dsbmih do
      begin
        biSize := sizeof(dsbmih);
        biWidth := bmWidth;
        biHeight := bmHeight;
        biPlanes := 1;
        biBitCount := bmPlanes * bmBitsPixel;
      end;
    end;
    FDIBHandle := CopyBitmap(FHandle, FPalette, FPalette, FDIB, nil);
  end;
end;

procedure TSCDeBitmap.FreeContext;
begin
  if (FCanvas <> nil) then TSCDeBitmapCanvas(FCanvas).FreeContext;
end;

procedure TSCDeBitmap.HandleNeeded;
begin
  with FImage do
    if FHandle = 0 then
      FHandle := FDIBHandle;
end;

procedure TSCDeBitmap.Mask(TransparentColor: TColor);
var
  NewHandle, NewPalette: THandle;
  DIB: TDIBSection;
begin
  NewHandle := 0;
  NewPalette := 0;
  try
    FreeContext;
    HandleNeeded;
    NewHandle := CopyBitmapAsMask(FImage.FHandle, FImage.FPalette,
      ColorToRGB(TransparentColor));
    FillChar(DIB, SizeOf(DIB), 0);
    GetObject(NewHandle, SizeOf(DIB), @DIB);
    if FImage.FPalette = SystemPalette16 then
      NewPalette := FImage.FPalette
    else
      NewPalette := CopyPalette(FImage.FPalette);
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
  Changed(Self);
end;

procedure TSCDeBitmap.MaskHandleNeeded;
begin
  if FMaskValid and FMaskBitsValid then Exit;
  with FImage do
  begin
    { Delete existing mask if any }
    if FMaskHandle <> 0 then
    begin
      DeselectBitmap(FMaskHandle);
      DeleteObject(FMaskHandle);
      FMaskHandle := 0;
    end;
    FreeContext;
    HandleNeeded;
    FMaskHandle := CopyBitmapAsMask(FHandle, FPalette, GetTransparentColor);
    FMaskValid := True;
    FMaskBitsValid := True;
  end;
end;

procedure TSCDeBitmap.PaletteNeeded;
var
  DC: HDC;
begin
  with FImage do
  begin
    if FIgnorePalette or (FPalette <> 0) or (FDIBHandle = 0) then Exit;
    if FHandle = FDIBHandle then DeselectBitmap(FDIBHandle);
    FPalette := PaletteFromDIBColorTable(FDIBHandle, nil, 1 shl FDIB.dsbmih.biBitCount);
    if FPalette <> 0 then Exit;
    DC := GDICheck(GetDC(0));
    FHalftone := FHalftone or
      ((GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES)) <
      (FDIB.dsbm.bmBitsPixel * FDIB.dsbm.bmPlanes));
    if FHalftone then FPalette := CreateHalftonePalette(DC);
    ReleaseDC(0, DC);
    if FPalette = 0 then IgnorePalette := True;
  end;
end;

procedure TSCDeBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  if (AFormat <> CF_BITMAP) or (AData = 0) then
    InvalidGraphic(SUnknownClipboardFormat);

  FreeContext;
  FillChar(DIB, sizeof(DIB), 0);
  GetObject(AData, sizeof(DIB), @DIB);
  if DIB.dsbm.bmBits = nil then DIB.dsbmih.biSize := 0;
  CopyImage(AData, APalette, DIB);
  FImage.FOS2Format := False;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TSCDeBitmap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, Stream.Size - Stream.Position);
end;

procedure TSCDeBitmap.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TSCDeBitmap.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TSCDeBitmap.NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
  const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
var
  Image: TSCDeBitmapImage;
begin
  Image := TSCDeBitmapImage.Create;
  with Image do
  try
    FHandle := NewHandle;
    FPalette := NewPalette;
    FDIB := NewDIB;
    FOS2Format := OS2Format;
    if FDIB.dsbm.bmBits <> nil then FDIBHandle := FHandle;
    FSaveStream := RLEStream as TMemoryStream;
  except
    Image.Free;
    raise;
  end;
  //!! replace with InterlockedExchange()
  EnterCriticalSection(BitmapImageLock);
  try
    FImage.Release;
    FImage := Image;
    FImage.Reference;
  finally
    LeaveCriticalSection(BitmapImageLock);
  end;
  FMaskValid := False;
end;

procedure TSCDeBitmap.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  ReadStream(Stream, Size);
end;

procedure TSCDeBitmap.ReadDIB(Stream: TStream; ImageSize: LongWord);
const
  DIBPalSizes: array [Boolean] of Byte = (sizeof(TRGBQuad), sizeof(TRGBTriple));
var
  DC, MemDC: HDC;
  BitsMem: Pointer;
  OS2Header: TBitmapCoreHeader;
  BitmapInfo: PBitmapInfo;
  ColorTable: Pointer;
  HeaderSize: Integer;
  OS2Format: Boolean;
  BMHandle, OldBMP: HBITMAP;
  DIB: TDIBSection;
  Pal, OldPal: HPalette;
  RLEStream: TStream;
begin
  Pal := 0;
  BMHandle := 0;
  RLEStream := nil;
  Stream.Read(HeaderSize, sizeof(HeaderSize));
  OS2Format := HeaderSize = sizeof(OS2Header);
  if OS2Format then HeaderSize := sizeof(TBitmapInfoHeader);
  GetMem(BitmapInfo, HeaderSize + 12 + 256 * sizeof(TRGBQuad));
  with BitmapInfo^ do
  try
    try
      if OS2Format then  // convert OS2 DIB to Win DIB
      begin
        Stream.Read(Pointer(Longint(@OS2Header) + sizeof(HeaderSize))^,
          sizeof(OS2Header) - sizeof(HeaderSize));
        FillChar(bmiHeader, sizeof(bmiHeader), 0);
        with bmiHeader, OS2Header do
        begin
          biWidth := bcWidth;
          biHeight := bcHeight;
          biPlanes := bcPlanes;
          biBitCount := bcBitCount;
        end;
        Dec(ImageSize, sizeof(OS2Header));
      end
      else
      begin // support bitmap headers larger than TBitmapInfoHeader
        Stream.Read(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
          HeaderSize - sizeof(HeaderSize));
        Dec(ImageSize, HeaderSize);

        if (bmiHeader.biCompression <> BI_BITFIELDS) and
          (bmiHeader.biCompression <> BI_RGB) then
        begin // Preserve funky non-DIB data (like RLE) until modified
          RLEStream := TMemoryStream.Create;
          // source stream could be unidirectional.  don't reverse seek
          RLEStream.Write(HeaderSize, sizeof(HeaderSize));
          RLEStream.Write(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
            HeaderSize - sizeof(HeaderSize));
          RLEStream.CopyFrom(Stream, ImageSize);
          RLEStream.Seek(ImageSize, soFromEnd);
          Stream := RLEStream;  // the rest of the proc reads from RLEStream
        end;
      end;

      with bmiHeader do
      begin
        biSize := HeaderSize;
        ColorTable := Pointer(Longint(BitmapInfo) + HeaderSize);

        { check number of planes. DIBs must be 1 color plane (packed pixels) }
        if biPlanes <> 1 then InvalidBitmap;

        // 3 DWORD color element bit masks (ie 888 or 565) can precede colors
        // TBitmapInfoHeader sucessors include these masks in the headersize
        if (HeaderSize = sizeof(TBitmapInfoHeader)) and
          ((biBitCount = 16) or (biBitCount = 32)) and
          (biCompression = BI_BITFIELDS) then
        begin
          Stream.ReadBuffer(ColorTable^, 3 * sizeof(DWORD));
          Inc(Longint(ColorTable), 3 * sizeof(DWORD));
          Dec(ImageSize, 3 * sizeof(DWORD));
        end;

        // Read the color palette
        if biClrUsed = 0 then
          biClrUsed := GetDInColors(biBitCount);
        Stream.ReadBuffer(ColorTable^, biClrUsed * DIBPalSizes[OS2Format]);
        Dec(ImageSize, biClrUsed * DIBPalSizes[OS2Format]);

        // biSizeImage can be zero. If zero, compute the size.
        if biSizeImage = 0 then            // top-down DIBs have negative height
          biSizeImage := BytesPerScanLine(biWidth, biBitCount, 32) * Abs(biHeight);

        if biSizeImage < ImageSize then ImageSize := biSizeImage;
      end;

      { convert OS2 color table to DIB color table }
      if OS2Format then RGBTripleToQuad(ColorTable^);

      DC := GDICheck(GetDC(0));
      try
        if ((bmiHeader.biCompression <> BI_RGB) and
          (bmiHeader.biCompression <> BI_BITFIELDS)) or DDBsOnly then
        begin
          MemDC := 0;
          GetMem(BitsMem, ImageSize);
          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
            MemDC := GDICheck(CreateCompatibleDC(DC));
            OldBMP := SelectObject(MemDC, CreateCompatibleBitmap(DC, 1, 1));
            OldPal := 0;
            if bmiHeader.biClrUsed > 0 then
            begin
              Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);
              OldPal := SelectPalette(MemDC, Pal, False);
              RealizePalette(MemDC);
            end;

            try
              BMHandle := CreateDIBitmap(MemDC, BitmapInfo^.bmiHeader, CBM_INIT, BitsMem,
                BitmapInfo^, DIB_RGB_COLORS);
              if (BMHandle = 0) then
                if GetLastError = 0 then InvalidBitmap else scdRaiseLastOSError;
            finally
              if OldPal <> 0 then
                SelectPalette(MemDC, OldPal, True);
              DeleteObject(SelectObject(MemDC, OldBMP));
            end;
          finally
            if MemDC <> 0 then DeleteDC(MemDC);
            FreeMem(BitsMem);
          end;
        end
        else
        begin
          BMHandle := CreateDIBSection(DC, BitmapInfo^, DIB_RGB_COLORS, BitsMem, 0, 0);
          if (BMHandle = 0) or (BitsMem = nil) then
            if GetLastError = 0 then InvalidBitmap else scdRaiseLastOSError;

          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
          except
            DeleteObject(BMHandle);
            raise;
          end;
        end;
      finally
        ReleaseDC(0, DC);
      end;
      // Hi-color DIBs don't preserve color table, so create palette now
      if (bmiHeader.biBitCount > 8) and (bmiHeader.biClrUsed > 0) and (Pal = 0)then
        Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);

      FillChar(DIB, sizeof(DIB), 0);
      GetObject(BMHandle, Sizeof(DIB), @DIB);
      // GetObject / CreateDIBSection don't preserve these info values
      DIB.dsBmih.biXPelsPerMeter := bmiHeader.biXPelsPerMeter;
      DIB.dsBmih.biYPelsPerMeter := bmiHeader.biYPelsPerMeter;
      DIB.dsBmih.biClrUsed := bmiHeader.biClrUsed;
      DIB.dsBmih.biClrImportant := bmiHeader.biClrImportant;
    except
      RLEStream.Free;
      raise;
    end;
  finally
    FreeMem(BitmapInfo);
  end;
  NewImage(BMHandle, Pal, DIB, OS2Format, RLEStream);
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TSCDeBitmap.ReadStream(Stream: TStream; Size: Longint);
var
  Bmf: TBitmapFileHeader;
  DIB: TDIBSection;
begin
  FreeContext;
  if Size = 0 then
  begin
    FillChar(DIB, sizeof(DIB), 0);
    NewImage(0, 0, DIB, False);
  end
  else
  begin
    Stream.ReadBuffer(Bmf, sizeof(Bmf));
    if Bmf.bfType <> $4D42 then InvalidBitmap;
    ReadDIB(Stream, Size - sizeof(Bmf));
  end;
end;

procedure TSCDeBitmap.SetHandle(Value: HBITMAP);
var
  DIB: TDIBSection;
  APalette: HPALETTE;
begin
  with FImage do
    if FHandle <> Value then
    begin
      FreeContext;
      FillChar(DIB, sizeof(DIB), 0);
      if Value <> 0 then
        GetObject(Value, SizeOf(DIB), @DIB);
      if RefCount = 1 then
      begin
        APalette := FPalette;
        FPalette := 0;
      end
      else
        if FPalette = SystemPalette16 then
          APalette := SystemPalette16
        else
          APalette := CopyPalette(FPalette);
      try
        NewImage(Value, APalette, DIB, False);
      except
        InternalDeletePalette(APalette);
        raise;
      end;
      Changed(Self);
    end;
end;

procedure TSCDeBitmap.SetHandleType(Value: TBitmapHandleType);
var
  DIB: TDIBSection;
  AHandle: HBITMAP;
  NewPalette: HPALETTE;
  DoCopy: Boolean;
begin
  if Value = GetHandleType then Exit;
  with FImage do
  begin
    if (FHandle = 0) and (FDIBHandle = 0) then
      if Value = bmDDB then
        FDIB.dsbmih.biSize := 0
      else
        FDIB.dsbmih.biSize := sizeof(FDIB.dsbmih)
    else
    begin
      if Value = bmDIB then
      begin
        if (FDIBHandle <> 0) and (FDIBHandle = FHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIBNeeded;
        if RefCount = 1 then
        begin
          AHandle := FDIBHandle;
          FDIBHandle := 0;
          NewPalette := FPalette;
          FPalette := 0;
          NewImage(AHandle, NewPalette, FDIB, FOS2Format);
        end
        else
          CopyImage(FDIBHandle, FPalette, FDIB);
      end
      else
      begin
        if (FHandle <> 0) and (FHandle <> FDIBHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIB := FDIB;
        DIB.dsbmih.biSize := 0;   // flag to tell CopyBitmap to create a DDB
        DoCopy := RefCount = 1;
        if DoCopy then
          NewPalette := FPalette
        else
          NewPalette := CopyPalette(FPalette);
        AHandle := CopyBitmap(FDIBHandle, FPalette, NewPalette, DIB, nil);
        if DoCopy then
          FHandle := AHandle
        else
          NewImage(AHandle, NewPalette, DIB, FOS2Format);
      end;
      Changed(Self);
    end;
  end;
end;

procedure TSCDeBitmap.SetHeight(Value: Integer);
begin
  Self.SetBounds(FImage.FDIB.dsbm.bmWidth, Value);
end;

procedure TSCDeBitmap.SetMaskHandle(Value: HBITMAP);
begin
  with FImage do
    if FMaskHandle <> Value then
    begin
      FMaskHandle := Value;
      FMaskValid := True;
      FMaskBitsValid := True;
    end;
end;

procedure TSCDeBitmap.SetMonochrome(Value: Boolean);
var
  DIB: TDIBSection;
begin
  with FImage, FDIB.dsbmih do
    if Value <> ((biPlanes = 1) and (biBitCount = 1)) then
    begin
      HandleNeeded;
      DIB := FDIB;
      with DIB.dsbmih, DIB.dsbm do
      begin
        biSize := 0;   // request DDB handle
        biPlanes := Byte(Value);  // 0 = request screen BMP format
        biBitCount := Byte(Value);
        bmPlanes := Byte(Value);
        bmBitsPixel := Byte(Value);
      end;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

procedure TSCDeBitmap.SetPalette(Value: HPALETTE);
var
  AHandle: HBITMAP;
  DIB: TDIBSection;
begin
  if FImage.FPalette <> Value then
  begin
    with FImage do
      if (Value = 0) and (RefCount = 1) then
      begin
        InternalDeletePalette(FPalette);
        FPalette := 0;
      end
      else
      begin
        FreeContext;
        HandleNeeded;
        DIB := FDIB;
        AHandle := CopyBitmap(FHandle, FPalette, Value, DIB, nil);
        try
          NewImage(AHandle, Value, DIB, FOS2Format);
        except
          DeleteObject(AHandle);
          raise;
        end;
      end;
    UpdateDIBColorTable(FImage.FDIBHandle, Value, FImage.FDIB);
    PaletteModified := True;
    Changed(Self);
  end;
end;

procedure TSCDeBitmap.SetPixelFormat(Value: TPixelFormat);
const
  BitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);
var
  DIB: TDIBSection;
  Pal: HPalette;
  DC: HDC;
  KillPal: Boolean;
begin
  if Value = GetPixelFormat then Exit;
  case Value of
    pfDevice:
      begin
        HandleType := bmDDB;
        Exit;
      end;
    pfCustom:
      InvalidGraphic(SInvalidPixelFormat);
  else
    FillChar(DIB, sizeof(DIB), 0);
    DIB.dsbm := FImage.FDIB.dsbm;
    KillPal := False;
    with DIB, dsbm, dsbmih do
    begin
      bmBits := nil;
      biSize := sizeof(DIB.dsbmih);
      biWidth := bmWidth;
      biHeight := bmHeight;
      biPlanes := 1;
      biBitCount := BitCounts[Value];
      Pal := FImage.FPalette;
      case Value of
        pf4Bit: Pal := SystemPalette16;
        pf8Bit:
          begin
            DC := GDICheck(GetDC(0));
            Pal := CreateHalftonePalette(DC);
            KillPal := True;
            ReleaseDC(0, DC);
          end;
        pf16Bit:
          begin
            biCompression := BI_BITFIELDS;
            dsBitFields[0] := $F800;
            dsBitFields[1] := $07E0;
            dsBitFields[2] := $001F;
          end;
      end;
      try
        CopyImage(Handle, Pal, DIB);
        PaletteModified := Pal <> 0;
      finally
        if KillPal then DeleteObject(Pal);
      end;
      Changed(Self);
    end;
  end;
end;

procedure TSCDeBitmap.SetTransparentColor(Value: TColor);
begin
  if Value <> FTransparentColor then
  begin
    if Value = clDefault then
      FTransparentMode := tmAuto else
      FTransparentMode := tmFixed;
    FTransparentColor := Value;
    if FImage.RefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;
    Changed(Self);
  end;
end;

procedure TSCDeBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  if Value <> FTransparentMode then
  begin
    if Value = tmAuto then
      SetTransparentColor(clDefault) else
      SetTransparentColor(GetTransparentColor);
  end;
end;

procedure TSCDeBitmap.SetWidth(Value: Integer);
begin
  Self.SetBounds(Value, FImage.FDIB.dsbm.bmHeight);
end;

procedure TSCDeBitmap.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TSCDeBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
const
  PalSize: array [Boolean] of Byte = (sizeof(TRGBQuad), sizeof(TRGBTriple));
var
  Size, ColorCount: DWORD;
  HeaderSize: DWORD;
  BMF: TBitmapFileHeader;
  Save: THandle;
  BC: TBitmapCoreHeader;
  Colors: array [Byte] of TRGBQuad;
begin
  if FImage.FSaveStream <> nil then
  begin
    Size := FImage.FSaveStream.Size;
    if WriteSize then
      Stream.WriteBuffer(Size, sizeof(Size));
    Stream.Write(FImage.FSaveStream.Memory^, Size);
    Exit;
  end;
  DIBNeeded;
  with FImage do
  begin
    Size := 0;
    if FDIBHandle <> 0 then
    begin
      InternalGetDIBSizes(FDIBHandle, HeaderSize, Size, FDIB.dsbmih.biClrUsed);
      if FOS2Format then
      begin // OS2 format cannot have partial palette
        HeaderSize := sizeof(BC);
        if FDIB.dsbmih.biBitCount <= 8 then
          Inc(HeaderSize, sizeof(TRGBTriple) * (1 shl FDIB.dsbmih.biBitCount));
      end;
      Inc(Size, HeaderSize + sizeof(BMF));

      FillChar(BMF, sizeof(BMF), 0);
      BMF.bfType := $4D42;

      TSCDeFakeCanvas(Canvas).RequiredState([csHandleValid]);
      Save := GDICheck(SelectObject(FCanvas.Handle, FDIBHandle));
      ColorCount := GetDIBColorTable(FCanvas.Handle, 0, 256, Colors);
      SelectObject(FCanvas.Handle, Save);
      // GetDIBColorTable always reports the full palette; trim it back for partial palettes
      if (0 < FDIB.dsbmih.biClrUsed) and (FDIB.dsbmih.biClrUsed < ColorCount) then
        ColorCount := FDIB.dsbmih.biClrUsed;
      if (not FOS2Format) and (ColorCount = 0) and (FPalette <> 0) and not FHalftone then
      begin
        ColorCount := PaletteToDIBColorTable(FPalette, Colors);
        if FDIB.dsbmih.biBitCount > 8 then
        begin  // optional color palette for hicolor images (non OS2)
          Inc(Size, ColorCount * sizeof(TRGBQuad));
          Inc(HeaderSize, ColorCount * sizeof(TRGBQuad));
        end;
      end;

      BMF.bfSize := Size;
      BMF.bfOffBits := sizeof(BMF) + HeaderSize;
    end;

    if WriteSize then Stream.WriteBuffer(Size, SizeOf(Size));

    if Size <> 0 then
    begin
      FixupBitFields(FDIB);
      if (ColorCount <> 0) then
      begin
        if (FDIB.dsbmih.biClrUsed = 0) or (FDIB.dsbmih.biClrUsed <> ColorCount) then
          FDIB.dsbmih.biClrUsed := ColorCount;
        if FOS2Format then RGBQuadToTriple(Colors, Integer(ColorCount));
      end;
      if FOS2Format then
      begin
        with BC, FDIB.dsbmih do
        begin
          bcSize := sizeof(BC);
          bcWidth := biWidth;
          bcHeight := biHeight;
          bcPlanes := 1;
          bcBitCount := biBitCount;
        end;
        Stream.WriteBuffer(BMF, sizeof(BMF));
        Stream.WriteBuffer(BC, sizeof(BC));
      end
      else
      begin
        Stream.WriteBuffer(BMF, Sizeof(BMF));
        Stream.WriteBuffer(FDIB.dsbmih, Sizeof(FDIB.dsbmih));
        if (FDIB.dsbmih.biBitCount > 8) and
          ((FDIB.dsbmih.biCompression and BI_BITFIELDS) <> 0) then
          Stream.WriteBuffer(FDIB.dsBitfields, 12);
      end;
      Stream.WriteBuffer(Colors, ColorCount * PalSize[FOS2Format]);
      Stream.WriteBuffer(FDIB.dsbm.bmBits^, FDIB.dsbmih.biSizeImage);
    end;
  end;
end;

{ ReleaseHandle gives up ownership of the bitmap handle the TSCDeBitmap contains. }
function TSCDeBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  Changing(Self);
  with FImage do
  begin
    Result := FHandle;
    if FHandle = FDIBHandle then
    begin
      FDIBHandle := 0;
      FDIB.dsbm.bmBits := nil;
    end;
    FHandle := 0;
  end;
end;

function TSCDeBitmap.ReleaseMaskHandle: HBITMAP;
begin
  Result := GetMaskHandle;
  FImage.FMaskHandle := 0;
end;

function TSCDeBitmap.ReleasePalette: HPALETTE;
begin
  HandleNeeded;
  Changing(Self);
  Result := FImage.FPalette;
  FImage.FPalette := 0;
end;

procedure TSCDeBitmap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TSCDeBitmap.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  Format := CF_BITMAP;
  HandleNeeded;
  with FImage do
  begin
    DIB := FDIB;
    DIB.dsbmih.biSize := 0;   // copy to device bitmap
    DIB.dsbm.bmBits := nil;
    Data := CopyBitmap(FHandle, FPalette, FPalette, DIB, FCanvas);
  end;
  try
    APalette := CopyPalette(FImage.FPalette);
  except
    DeleteObject(Data);
    raise;
  end;
end;

function TSCDeBitmap.TransparentColorStored: Boolean;
begin
  Result := FTransparentMode = tmFixed;
end;

initialization
  InitializeCriticalSection(BitmapImageLock);
  BitmapCanvasList := TThreadList.Create;

finalization
  BitmapCanvasList.Free;
  DeleteCriticalSection(BitmapImageLock);

end.
