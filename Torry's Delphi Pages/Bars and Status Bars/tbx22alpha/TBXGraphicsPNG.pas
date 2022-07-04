unit TBXGraphicsPNG;

interface

uses
  Windows, Classes, SysUtils, Types, TBXGraphics, TBXGraphicsZLib;

type
  TCompressionType = (ctUnknown, ctLZ77);

  TColorScheme = (
    csUnknown,
    csIndexed,
    csG,
    csGA,
    csRGB,
    csRGBA
  );

  TChunkType = array [0..3] of Char;

  TPNGChunkHeader = packed record
    Length: Cardinal; 
    ChunkType: TChunkType;
  end;

  TPNGReader = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FStream: TStream;
    FFrameBuffer: TCardinalDynArray;
    FIDATSize: Integer;
    FRawBuffer: Pointer;
    FCurrentCRC: Cardinal;
    FCurrentSource: Pointer;
    FHeader: TPNGChunkHeader;
    FStreamRec: TZStreamRec;
    procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByteArray; BPP, BytesPerRow: Integer);
    procedure ConvertRow(Source: PByte; Target: PCardinal; Count: Integer; TargetStep: Integer);
    function  GetFrameBuffer: PCardinal;
    function  IsChunk(ChunkType: TChunkType): Boolean;
    procedure LoadBackgroundColor;
    procedure LoadIDAT;
    procedure LoadPalette(const Buffer; Count: Integer);
    procedure LoadTransparency;
    procedure ReadData;
    procedure ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
    procedure SetupColorDepth(ColorType, BitDepth: Integer);
    procedure ReadHeader;
  protected
    ColorScheme: TColorScheme;
    BitDepth: Integer;
    SamplesPerPixel: Integer;
    BitsPerPixel: Integer;
    BytesPerPixel: Integer;
    Compression: TCompressionType;
    Gamma: Single;
    Interlaced: Boolean;
    FilterMode: Byte;
    Palette: TCardinalDynArray;
    TransparentColor: Cardinal;
    TransparentR2: Cardinal;
    TransparentG2: Cardinal;
    TransparentB2: Cardinal;
    BackgroundColor: Cardinal;
    TransparentColorDefined: Boolean;
    BackgroundColorDefined: Boolean;
  public
    procedure LoadFromStream(Stream: TStream);
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    property FrameBuffer: PCardinal read GetFrameBuffer;
  end;

  TPNGWriter = class
  private
    FFrameBuffer: TCardinalDynArray;
    FWidth: Integer;
    FHeight: Integer;
  protected
    BitsPerSample: Integer;
    ColorScheme: TColorScheme;
    GrayScale: Boolean;
    NumPixels: Integer;
    UseAlpha: Boolean;
    UsePalette: Boolean;
    UseTransparency: Boolean;
    TransparentColor: Cardinal;
    TransparentIndices: TByteDynArray;
    Palette: TCardinalDynArray;
    procedure Analyze;
    procedure ApplyFilter(FilterType: Byte; Line, PrevLine, Target: PByteArray; BPP, BytesPerRow: Integer);
    procedure ConvertPalette;
    procedure ConvertRow(Src: PCardinal; Dst: PByte; Count: Integer);
    procedure WriteChunk(Stream: TStream; const ChunkType: string; const Data; DataLength: Cardinal);
    procedure WriteData(Stream: TStream);
    procedure WritePalette(Stream: TStream);
    procedure WriteTransparency(Stream: TStream);
  public
    constructor Create(AWidth, AHeight, ARowStride: Integer; Bits: PCardinal);
    procedure WriteToStream(Stream: TStream);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

function CanLoadPNG(Stream: TStream): Boolean;

implementation

uses
  Math;

resourcestring
  SInvalidImage = 'Invalid format';
  SInvalidColorFormat = 'Invalid color format';
  SStreamReadError = 'Stream read error';
  SUnsupportedImage = 'Unsupported image format';
  SUnsupportedCompressionScheme = 'Unsupported compression scheme';
  SInvalidCRC = 'CRC error';
  SCompressionError = 'Compression error';
  SInvalidPalette = 'Invalid palette';
  SUnknownCriticalChunk = 'Unexpected but critical chunk detected';
  SExtraCompressedData = 'Extra compressed data found';
  SInvalidPaletteIndex = 'Invalid palette index';
  SLZ77Error = 'LZ77 decompression error.';
  SInvalidDimensions = 'Invalid dimensions';


function SwapLong(Value: Cardinal): Cardinal; overload;
asm
    bswap eax
end;

procedure SwapLong(P: PInteger; Count: Cardinal); overload;
asm
  @1:
    mov ecx,[eax]
    bswap ecx
    mov [eax],ecx
    add eax,4
    dec edx
    jnz @1
end;

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array [0..MaxInt div 8 - 1] of Cardinal;

procedure Pack4Bits(Src: PCardinal; Dst: PByte; SrcCount: Integer);
begin
  while SrcCount >= 2 do
  begin
    Dst^ := Src^ shl 4 or PCardinalArray(Src)[1];
    Inc(Dst);
    Inc(Src, 2);
    Dec(SrcCount, 2);
  end;
  if SrcCount > 0 then
    Dst^ := Src^ shl 4;
end;

procedure Unpack4Bits(Src: PByte; Dst: PCardinal; DstCount, DstStep: Integer);
var
  B: Cardinal;
begin
  while DstCount >= 2 do
  begin
    B := Src^;
    Dst^ := B shr 4;
    PCardinalArray(Dst)[DstStep] := B and $F;
    Inc(Src);
    Inc(Dst, 2 * DstStep);
    Dec(DstCount, 2);
  end;
  if DstCount > 0 then
  begin
    B := Src^;
    Dst^ := B shr 4;
  end;
end;

procedure Pack2Bits(Src: PCardinal; Dst: PByte; SrcCount: Integer);
var
  C: Cardinal;
begin
  while SrcCount >= 4 do
  begin
    Dst^ := Src^ shl 6 or
      PCardinalArray(Src)[1] shl 4 or
      PCardinalArray(Src)[2] shl 2 or
      PCardinalArray(Src)[3];
    Inc(Dst);
    Inc(Src, 4);
    Dec(SrcCount, 4);
  end;
  if SrcCount > 0 then
  begin
    C := Src^ shl 6;
    if SrcCount > 1 then C := C or PCardinalArray(Src)[1] shl 4;
    if SrcCount > 2 then C := C or PCardinalArray(Src)[2] shl 2;
    Dst^ := C;
  end;
end;

procedure Unpack2Bits(Src: PByte; Dst: PCardinal; DstCount, DstStep: Integer);
var
  B: Cardinal;
begin
  while DstCount >= 4 do
  begin
    B := Src^;
    Dst^ := B shr 6 and $03;
    PCardinalArray(Dst)[DstStep] := B shr 4 and $03;
    PCardinalArray(Dst)[DstStep * 2] := B shr 2 and $03;
    PCardinalArray(Dst)[DstStep * 3] := B and $03;
    Inc(Src);
    Inc(Dst, 4 * DstStep);
    Dec(DstCount, 4);
  end;
  if DstCount > 0 then
  begin
    B := Src^;
    Dst^ := B shr 6 and $03;
    if DstCount > 1 then
    begin
      PCardinalArray(Dst)[DstStep] := B shr 4 and $03;
      if DstCount > 2 then PCardinalArray(Dst)[DstStep * 2] := B shr 2 and $03;
    end;
  end;
end;

procedure Pack1Bits(Src: PCardinal; Dst: PByte; SrcCount: Integer);
var
  C: Cardinal;
begin
  { Assuming src is already scaled to 1 bit }
  while SrcCount >= 8 do
  begin
    Dst^ := Src^ shl 7 or
      PCardinalArray(Src)[1] shl 6 or
      PCardinalArray(Src)[2] shl 5 or
      PCardinalArray(Src)[3] shl 4 or
      PCardinalArray(Src)[4] shl 3 or
      PCardinalArray(Src)[5] shl 2 or
      PCardinalArray(Src)[6] shl 1 or
      PCardinalArray(Src)[7];
    Inc(Dst);
    Inc(Src, 8);
    Dec(SrcCount, 8);
  end;
  if SrcCount > 0 then
  begin
    C := Src^ shl 7;
    if SrcCount > 1 then C := C or PCardinalArray(Src)[1] shl 6;
    if SrcCount > 2 then C := C or PCardinalArray(Src)[2] shl 5;
    if SrcCount > 3 then C := C or PCardinalArray(Src)[3] shl 4;
    if SrcCount > 4 then C := C or PCardinalArray(Src)[4] shl 3;
    if SrcCount > 5 then C := C or PCardinalArray(Src)[5] shl 2;
    if SrcCount > 6 then C := C or PCardinalArray(Src)[6] shl 1;
    Dst^ := C;
  end;
end;

procedure Unpack1Bits(Src: PByte; Dst: PCardinal; DstCount, DstStep: Integer);
var
  B: Cardinal;
begin
  while DstCount >= 8 do
  begin
    B := Src^;
    Dst^ := B shr 7 and $01;
    PCardinalArray(Dst)[DstStep] := B shr 6 and $01;
    PCardinalArray(Dst)[DstStep * 2] := B shr 5 and $01;
    PCardinalArray(Dst)[DstStep * 3] := B shr 4 and $01;
    PCardinalArray(Dst)[DstStep * 4] := B shr 3 and $01;
    PCardinalArray(Dst)[DstStep * 5] := B shr 2 and $01;
    PCardinalArray(Dst)[DstStep * 6] := B shr 1 and $01;
    PCardinalArray(Dst)[DstStep * 7] := B and $01;
    Inc(Src);
    Inc(Dst, 8 * DstStep);
    Dec(DstCount, 8);
  end;
  if DstCount > 0 then
  begin
    B := Src^;
    Dst^ := B shr 7 and $01;
    if DstCount > 1 then
    begin
      PCardinalArray(Dst)[DstStep] := B shr 6 and $01;
      if DstCount > 2 then
      begin
        PCardinalArray(Dst)[DstStep * 2] := B shr 5 and $01;
        if DstCount > 3 then
        begin
          PCardinalArray(Dst)[DstStep * 3] := B shr 4 and $01;
          if DstCount > 4 then
          begin
            PCardinalArray(Dst)[DstStep * 4] := B shr 3 and $01;
            if DstCount > 5 then
            begin
              PCardinalArray(Dst)[DstStep * 5] := B shr 2 and $01;
              if DstCount > 6 then
              begin
                PCardinalArray(Dst)[DstStep * 6] := B shr 1 and $01;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

const
  PNGMagic: array [0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);

  // recognized and handled chunk types
  IHDR = 'IHDR';
  IDAT = 'IDAT';
  IEND = 'IEND';
  PLTE = 'PLTE';
  gAMA = 'gAMA';
  tRNS = 'tRNS';
  bKGD = 'bKGD';

  CHUNKMASK = $20; // used to check bit 5 in chunk types

  { IHDRChunk ColorType Constants }
  CT_PALETTE = $1;
  CT_COLORS  = $2;
  CT_ALPHA   = $4;

type
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width: Cardinal;
    Height: Cardinal;
    BitDepth: Byte;
    ColorType: Byte;   // can be 0, 2, 3, 4, 6
    Compression: Byte; // 0 - LZ77, others are not yet defined
    Filter: Byte;      // filter mode 0 is the only one currently defined
    Interlaced: Byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  end;

function DecodeBuffer(var StreamRec: TZStreamRec; var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer): Integer;
begin
  StreamRec.next_in := Source;
  StreamRec.avail_in := PackedSize;
  StreamRec.next_out := Dest;
  StreamRec.avail_out := UnpackedSize;
  Result := ZLibCheck(inflate(StreamRec, Z_PARTIAL_FLUSH));
  Source := StreamRec.next_in;
  Dest := StreamRec.next_out;
end;

function CanLoadPNG(Stream: TStream): Boolean;
var
  Magic: array [0..7] of Byte;
  SavePosition: Int64;
begin
  SavePosition := Stream.Position;
  Result := (Stream.Read(Magic, 8) = 8) and CompareMem(@Magic[0], @PNGMagic[0], 8);
  Stream.Position := SavePosition;
end;

{ TPNGReader }

function TPNGReader.IsChunk(ChunkType: TChunkType): Boolean;
const
  Mask = not $20202020;
begin
  Result := (Cardinal(FHeader.ChunkType) and Mask) = (Cardinal(ChunkType) and Mask);
end;

function PaethPredictor(a, b, c: Byte): Byte;
var
  p, pa, pb, pc: Integer;
begin
  // a = left, b = above, c = upper left
  p := a + b - c;        // initial estimate
  pa := Abs(p - a);      // distances to a, b, c
  pb := Abs(p - b);
  pc := Abs(p - c);
  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then Result := a
  else if pb <= pc then Result := b else Result := c;
end;

procedure TPNGReader.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByteArray; BPP, BytesPerRow: Integer);
var
  I: Integer;
begin
  case Filter of
    0: // no filter, just copy data
      if Line <> Target then Move(Line[0], Target[0], BytesPerRow);

    1: // subtraction filter
      begin
        Move(Line[0], Target[0], BPP);
        for I := BPP to BytesPerRow - 1 do
          Target[I] := (Line[I] + Target[I - BPP]) and $FF;
      end;

    2: // Up filter
      for I := 0 to BytesPerRow - 1 do
        Target[I] := Line[I] + PrevLine[I];

    3: // average filter
      begin
        for I := 0 to BPP - 1 do
          Target[I] := Line[I] + PrevLine[I] shr 1;

        for I := BPP to BytesPerRow - 1 do
          Target[I] := Line[I] + (Target[I - BPP] + PrevLine[I]) shr 1;
      end;

   4: // paeth prediction
     begin
       for I := 0 to BPP - 1 do
         Target[I] := Line[I] + PaethPredictor(0, PrevLine[I], 0);

       for I := BPP to BytesPerRow - 1 do
         Target[I] := Line[I] + PaethPredictor(Target[I - BPP], PrevLine[I], PrevLine[I - BPP]);
     end;
   end;
end;

procedure TPNGReader.LoadFromStream(Stream: TStream);
var
  Magic: array [0..7] of Byte;
  Description: TIHDRChunk;
begin
  FStream := Stream;
  Stream.ReadBuffer(Magic, 8);
  if not CompareMem(@Magic, @PNGMagic, 8) then raise EStreamError.CreateRes(@SInvalidImage);

  ReadHeader;
  if not IsChunk(IHDR) then raise EStreamError.CreateRes(@SInvalidImage);
  ReadData;

  Move(FRawBuffer^, Description, SizeOf(TIHDRChunk));
  SwapLong(@Description, 2);

  if (Description.Width <= 0) or (Description.Height <= 0) then Exit;

  FWidth := Description.Width;
  FHeight := Description.Height;
  SetLength(FFrameBuffer, Width * Height);

  if Description.Compression <> 0 then
    EStreamError.CreateRes(@SUnsupportedCompressionScheme);

  SetupColorDepth(Description.ColorType, Description.BitDepth);

  FilterMode := Description.Filter;
  Interlaced := Description.Interlaced <> 0;

  SetLength(Palette, 0);
  BackgroundColor := $00000000;
  TransparentColor := $00000000;
  BackgroundColorDefined := False;
  TransparentColorDefined := False;

  FillChar(FStreamRec, SizeOf(FStreamRec), 0);
  try
    ZLibCheck(inflateInit_(FStreamRec, ZLIB_VERSION, SizeOf(TZStreamRec)));
    repeat
      ReadHeader;
      if IsChunk(IDAT) then LoadIDAT
      else if IsChunk(PLTE) then
      begin
        // palette chunk
        if FHeader.Length mod 3 <> 0 then raise EStreamError.CreateRes(@SInvalidPalette);
        ReadData;
        // load palette only if the image is indexed colors
        if Description.ColorType = 3 then LoadPalette(FRawBuffer^, FHeader.Length div 3);
        Continue;
      end
      else if IsChunk(gAMA) then
      begin
        ReadData;
        Gamma := SwapLong(PCardinal(FRawBuffer)^) / 100000;
        Continue;
      end
      else if IsChunk(bKGD) then
      begin
        LoadBackgroundColor;
        Continue;
      end
      else if IsChunk(tRNS) then
      begin
        LoadTransparency;
        Continue;
      end;

      Stream.Seek(FHeader.Length + 4, soFromCurrent);
      if IsChunk(IEND) then Break;

      if Byte(FHeader.ChunkType[0]) and CHUNKMASK = 0 then
        raise EStreamError.CreateRes(@SUnknownCriticalChunk);
    until False;

  finally
    ZLibCheck(inflateEnd(FStreamRec));
    if Assigned(FRawBuffer) then FreeMem(FRawBuffer);
  end;
end;

procedure TPNGReader.LoadBackgroundColor;
var
  Run: PWord;
  R, G, B: Cardinal;
begin
  ReadData;
  case ColorScheme of
    csG, csGA:
        case BitDepth of
          2: BackgroundColor := Swap(PWord(FRawBuffer)^) * 15 div 3;
          16: BackgroundColor := Swap(PWord(FRawBuffer)^) * 255 div 65535;
        else // 1, 4, 8 bits gray scale
          BackgroundColor := Byte(Swap(PWord(FRawBuffer)^));
        end;

    csRGB, csRGBA:
      begin
        Run := FRawBuffer;
        if BitDepth = 16 then
        begin
          R := Swap(Run^) * 255 div 65535; Inc(Run);
          G := Swap(Run^) * 255 div 65535; Inc(Run);
          B := Swap(Run^) * 255 div 65535;
        end
        else
        begin
          R := Swap(Run^) and $FF; Inc(Run);
          G := Swap(Run^) and $FF; Inc(Run);
          B := Swap(Run^) and $FF;
        end;
        BackgroundColor := R or G shl 8 or B shl 16;
      end;
  else // indexed color scheme (3)
    BackgroundColor := PByte(FRawBuffer)^;
  end;
  BackgroundColorDefined := True;
end;

procedure TPNGReader.LoadPalette(const Buffer; Count: Integer);
var
  P: PByte;
  I: Integer;
  R, G, B: Cardinal;
begin
  SetLength(Palette, Count);
  P := @Buffer;
  for I := 0 to Count - 1 do
  begin
    R := P^;
    G := PByteArray(P)[1];
    B := PByteArray(P)[2];
    Inc(P, 3);
    Palette[I] := $FF000000 or R shl 16 or G shl 8 or B;
  end;
end;

procedure TPNGReader.LoadIDAT;
const
  RowStart: array [0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  RowIncrement: array [0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnStart: array [0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  ColumnIncrement: array [0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
var
  Row: Integer;
  RowBuffer: array [Boolean] of TByteDynArray;
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  BytesPerRow, InterlaceRowBytes, InterlaceWidth: Integer;

  function ByteCount(NumPixels: Integer): Integer;
  begin
    case ColorScheme of
      csG, csIndexed: Result := (NumPixels * BitDepth + 7) div 8;
      csRGB: Result := NumPixels * BitDepth * 3 div 8;
      csGA: Result := NumPixels * BitDepth * 2 div 8;
      csRGBA: Result := NumPixels * BitDepth * 4 div 8;
    else
      Result := 0;
    end;
  end;

begin
  BytesPerRow := ByteCount(Width);

  SetLength(RowBuffer[True], BytesPerRow + 1);
  SetLength(RowBuffer[False], BytesPerRow + 1);
  FillChar(RowBuffer[True][0], BytesPerRow + 1, 0);
  FillChar(RowBuffer[False][0], BytesPerRow + 1, 0);
  EvenRow := True;

  // prepare interlaced images
  if Interlaced then
  begin
    for Pass := 0 to 6 do
    begin
      if Width <= ColumnStart[Pass] then Continue;
      InterlaceWidth := (Width - ColumnStart[Pass] + ColumnIncrement[Pass] - 1) div ColumnIncrement[Pass];
      InterlaceRowBytes := ByteCount(InterlaceWidth);
      ZeroMemory(@RowBuffer[not EvenRow][0], InterlaceRowBytes + 1);
      Row := RowStart[Pass];
      while Row < Height do
      begin
        ReadRow(RowBuffer[EvenRow], InterlaceRowBytes + 1);
        ApplyFilter(
          RowBuffer[EvenRow][0],
          @RowBuffer[EvenRow][1],
          @RowBuffer[not EvenRow][1],
          @RowBuffer[EvenRow][1],
          BytesPerPixel, InterlaceRowBytes);

        ConvertRow(@RowBuffer[EvenRow][1], @FFrameBuffer[Row * Width + ColumnStart[Pass]], InterlaceWidth, ColumnIncrement[Pass]);
        EvenRow := not EvenRow;
        Inc(Row, RowIncrement[Pass]);
      end;
    end;
  end
  else
  begin
    for Row := 0 to Height - 1 do
    begin
      ReadRow(RowBuffer[EvenRow], BytesPerRow + 1);
      ApplyFilter(
        RowBuffer[EvenRow][0],
        @RowBuffer[EvenRow][1],
        @RowBuffer[not EvenRow][1],
        @RowBuffer[EvenRow][1],
        BytesPerPixel, BytesPerRow);

      ConvertRow(@RowBuffer[EvenRow][1], @FFrameBuffer[Row * Width], Width, 1);
      EvenRow := not EvenRow;
    end;
  end;

  while IsChunk(IDAT) do
  begin
    ReadData;
    ReadHeader;
  end;
end;

procedure TPNGReader.LoadTransparency;
var
  Count, I: Integer;
begin
  ReadData;
  case ColorScheme of
    csG: // gray
      begin
        case BitDepth of
          2: TransparentColor := Swap(PWord(FRawBuffer)^) and $3;
          16: TransparentG2 := Swap(PWord(FRawBuffer)^);  // use G2 channel for grayscale
        else // 1, 4, 8 bits gray scale
          TransparentColor := Swap(PWord(FRawBuffer)^) and $FF;
        end;
        TransparentColorDefined := True;
      end;

    csRGB:  // RGB
      begin
        TransparentR2 := Swap(PWord(FRawBuffer)^);
        TransparentG2 := Swap(PWordArray(FRawBuffer)^[1]);
        TransparentB2 := Swap(PWordArray(FRawBuffer)^[2]);

        if BitDepth = 8 then
          TransparentColor :=
            (TransparentR2 and $FF) shl 16 or
            (TransparentG2 and $FF) shl 8 or
            (TransparentB2 and $FF);
        TransparentColorDefined := True;
      end;

    csGA, csRGBA: { ignore };
  else // csIndex
    Count := Min(FHeader.Length, Length(Palette));
    for I := 0 to Count - 1 do
      Palette[I] := Palette[I] and $00FFFFFF or (PByteArray(FRawBuffer)[I] shl 24);

    for I := Count to Length(Palette) - 1 do
      Palette[I] := Palette[I] or $FF000000;
  end;
end;

procedure TPNGReader.ReadHeader;
begin
  FStream.ReadBuffer(FHeader, SizeOf(TPNGChunkHeader));
  FCurrentCRC := CRC32(0, @FHeader.ChunkType, 4);
  FHeader.Length := SwapLong(FHeader.Length);
end;

procedure TPNGReader.ReadData;
var
  FileCRC: Cardinal;
begin
  ReallocMem(FRawBuffer, FHeader.Length);
  FStream.ReadBuffer(FRawBuffer^, FHeader.Length);
  FStream.ReadBuffer(FileCRC, SizeOf(FileCRC));
  FileCRC := SwapLong(FileCRC);
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  if FCurrentCRC <> FileCRC then raise EStreamError.CreateRes(@SInvalidCRC);
end;

procedure TPNGReader.ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
var
  LocalBuffer: Pointer;
  PendingOutput: Integer;
  ZLibStatus: Integer;
begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  repeat
    if FStreamRec.avail_in = 0 then
    begin
      FIDATSize := 0;
      while FIDATSize = 0 do
      begin
        if not IsChunk(IDAT) then Exit;
        ReadData;
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;
        ReadHeader;
      end;
    end;

    ZLibStatus := DecodeBuffer(FStreamRec, FCurrentSource, LocalBuffer,
      FIDATSize - (Integer(FCurrentSource) - Integer(FRawBuffer)), PendingOutput);

    if ZLibStatus = Z_STREAM_END then
    begin
      if (FStreamRec.avail_out <> 0) or (FStreamRec.avail_in <> 0) then
        raise EStreamError.CreateRes(@SExtraCompressedData);
      Break;
    end;

    if ZLibStatus <> Z_OK then
      raise EStreamError.CreateRes(@SCompressionError);

    PendingOutput := BytesPerRow - (Integer(LocalBuffer) - Integer(RowBuffer));
  until PendingOutput = 0;
end;

procedure TPNGReader.SetupColorDepth(ColorType, BitDepth: Integer);

  procedure Error;
  begin
    raise EStreamError.CreateRes(@SInvalidColorFormat);
  end;

begin
  case ColorType of
    0: // gray scale (allowed bit depths are: 1, 2, 4, 8, 16 bits)
      if BitDepth in [1, 2, 4, 8, 16] then
      begin
        ColorScheme := csG;
        SamplesPerPixel := 1;
        BytesPerPixel := (BitDepth + 7) div 8;
      end
      else Error;

    2: // RGB
      if BitDepth in [8, 16] then
      begin
        ColorScheme := csRGB;
        SamplesPerPixel := 3;
        BytesPerPixel := BitDepth * 3 div 8;
      end
      else Error;

    3: // palette
      if BitDepth in [1, 2, 4, 8] then
      begin
        ColorScheme := csIndexed;
        SamplesPerPixel := 1;
        BytesPerPixel := 1;
      end
      else Error;

    4: // gray scale with alpha
      if BitDepth in [8, 16] then
      begin
        ColorScheme := csGA;
        SamplesPerPixel := 1;
        BytesPerPixel := 2 * BitDepth div 8;
      end
      else Error;

    6: // RGB with alpha (8, 16)
      if BitDepth in [8, 16] then
      begin
        ColorScheme := csRGBA;
        SamplesPerPixel := 4;
        BytesPerPixel := BitDepth * 4 div 8;
      end
      else Error;
  else
    Error;
  end;

  Self.BitDepth := BitDepth;
  BitsPerPixel := SamplesPerPixel * BitDepth;
end;

procedure TPNGReader.ConvertRow(Source: PByte; Target: PCardinal; Count: Integer; TargetStep: Integer);
const
  Alphas: array [Boolean] of Cardinal = ($FF000000, $00000000);
  Gray4Palette: array [0..15] of Cardinal = ($00000000, $00111111,
    $00222222, $00333333, $00444444, $00555555, $00666666, $00777777, $00888888,
    $00999999, $00AAAAAA, $00BBBBBB, $00CCCCCC, $00DDDDDD, $00EEEEEE, $00FFFFFF);
  Gray2Palette: array [0..3] of Cardinal = ($00000000, $00555555, $00AAAAAA, $00FFFFFF);
  Gray1Palette: array [0..1] of Cardinal = ($00000000, $00FFFFFF);
var
  I: Integer;
  N, C: Cardinal;
  R, G, B, A: Cardinal;
begin
  N := Length(Palette);
  case ColorScheme of
    csIndexed:
      case BitDepth of
        8:
          if Length(Palette) = 256 then
            for I := 0 to Count - 1 do
            begin
              Target^ := Palette[PByteArray(Source)^[I]];
              Inc(Target, TargetStep);
            end
          else
            for I := 0 to Count - 1 do
            begin
              C := PByteArray(Source)^[I];
              if C >= N then raise EStreamError.CreateRes(@SInvalidPaletteIndex);
              Target^ := Palette[C];
              Inc(Target, TargetStep);
            end;

        4:
          begin
            Unpack4Bits(Source, Target, Count, TargetStep);
            if Length(Palette) = 16 then
              for I := 0 to Count - 1 do
              begin
                Target^ := Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                if C >= N then raise EStreamError.CreateRes(@SInvalidPaletteIndex);
                Target^ := Palette[C];
                Inc(Target, TargetStep);
              end;
          end;

        2:
          begin
            Unpack2Bits(Source, Target, Count, TargetStep);
            if Length(Palette) = 4 then
              for I := 0 to Count - 1 do
              begin
                Target^ := Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                if C >= N then raise EStreamError.CreateRes(@SInvalidPaletteIndex);
                Target^ := Palette[C];
                Inc(Target, TargetStep);
              end;
          end;

        1:
          begin
            Unpack1Bits(Source, Target, Count, TargetStep);
            if Length(Palette) = 2 then
              for I := 0 to Count - 1 do
              begin
                Target^ := Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                if C >= N then raise EStreamError.CreateRes(@SInvalidPaletteIndex);
                Target^ := Palette[C];
                Inc(Target, TargetStep);
              end;
          end;
      end;

    csG:
      case BitDepth of
        16:
          if not TransparentColorDefined then
            for I := 0 to Count - 1 do
            begin
              C := PByteArray(Source)^[I * 2];
              Target^ := $FF000000 or C shl 16 or C shl 8 or C;
              Inc(Target, TargetStep);
            end
          else
            for I := 0 to Count - 1 do
            begin
              C := Swap(PWordArray(Source)^[I]);
              Target^ := Alphas[TransparentG2 = C]
                or (C shl 8 and $FF0000) or (C and $FF00) or (C shr 8);
              Inc(Target, TargetStep);
            end;

        8:
          if not TransparentColorDefined then
            for I := 0 to Count - 1 do
            begin
              C := PByteArray(Source)^[I];
              Target^ := $FF000000 or C shl 16 or C shl 8 or C;
              Inc(Target, TargetStep);
            end
          else
            for I := 0 to Count - 1 do
            begin
              C := PByteArray(Source)^[I];
              Target^ := Alphas[C = TransparentColor] or $FF000000 or C shl 16 or C shl 8 or C;
              Inc(Target, TargetStep);
            end;
        4:
          begin
            Unpack4Bits(Source, Target, Count, TargetStep);
            if not TransparentColorDefined then
              for I := 0 to Count - 1 do
              begin
                Target^ := $FF000000 or Gray4Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                Target^ := Alphas[C = TransparentColor] or Gray4Palette[C];
                Inc(Target, TargetStep);
              end
          end;
        2:
          begin
            Unpack2Bits(Source, Target, Count, TargetStep);
            if not TransparentColorDefined then
              for I := 0 to Count - 1 do
              begin
                Target^ := $FF000000 or Gray2Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                Target^ := Alphas[C = TransparentColor] or Gray2Palette[C];
                Inc(Target, TargetStep);
              end
          end;
        1:
          begin
            Unpack1Bits(Source, Target, Count, TargetStep);
            if not TransparentColorDefined then
              for I := 0 to Count - 1 do
              begin
                Target^ := $FF000000 or Gray1Palette[Target^];
                Inc(Target, TargetStep);
              end
            else
              for I := 0 to Count - 1 do
              begin
                C := Target^;
                Target^ := Alphas[C = TransparentColor] or Gray1Palette[C];
                Inc(Target, TargetStep);
              end
          end;
      end;

    csGA:
      case BitDepth of
        16:
          for I := 0 to Count - 1 do
          begin
            C := Source^;
            A := PByteArray(Source)^[2];
            Target^ := A shl 24 or C shl 16 or C shl 8 or C;
            Inc(Target, TargetStep);
            Inc(Source, 4);     
          end;
        8:
          for I := 0 to Count - 1 do
          begin
            C := Source^;
            A := PByteArray(Source)^[1];
            Target^ := A shl 24 or C shl 16 or C shl 8 or C;
            Inc(Target, TargetStep);
            Inc(Source, 2);
          end;
      end;

    csRGB:
      case BitDepth of
        16:
          if not TransparentColorDefined then
            for I := 0 to Count - 1 do
            begin
              R := Source^;
              G := PByteArray(Source)^[2];
              B := PByteArray(Source)^[4];
              Target^ := $FF000000 or R shl 16 or G shl 8 or B;
              Inc(Source, 6);
              Inc(Target, TargetStep);
            end
          else
            for I := 0 to Count - 1 do
            begin
              R := PWordArray(Pointer(Source))^[0];
              G := PWordArray(Pointer(Source))^[1];
              B := PWordArray(Pointer(Source))^[2];
              Target^ := Alphas[(R = TransparentR2) and (G = TransparentG2) and (B = TransparentB2)] or
                (R shl 8 and $FF0000) or (G and $FF00) or (B shr 8);
              Inc(Source, 6);
              Inc(Target, TargetStep);
            end;
        8:
          if not TransparentColorDefined then
            for I := 0 to Count - 1 do
            begin
              R := Source^;
              G := PByteArray(Source)^[1];
              B := PByteArray(Source)^[2];
              Target^ := $FF000000 or R shl 16 or G shl 8 or B;
              Inc(Source, 3);
              Inc(Target, TargetStep);
            end
          else
            for I := 0 to Count - 1 do
            begin
              R := Source^;
              G := PByteArray(Source)^[1];
              B := PByteArray(Source)^[2];
              C := R shl 16 or G shl 8 or B;
              Target^ := Alphas[C = TransparentColor] or C;
              Inc(Source, 3);
              Inc(Target, TargetStep);
            end;
      end;

    csRGBA:
      case BitDepth of
        16:
          for I := 0 to Count - 1 do
          begin
            R := Source^;
            G := PByteArray(Source)^[2];
            B := PByteArray(Source)^[4];
            A := PByteArray(Source)^[6];
            Target^ := A shl 24 or R shl 16 or G shl 8 or B;
            Inc(Source, 8);
            Inc(Target, TargetStep);
          end;
        8:
          if not TransparentColorDefined then
            for I := 0 to Count - 1 do
            begin
              R := Source^;
              G := PByteArray(Source)^[1];
              B := PByteArray(Source)^[2];
              A := PByteArray(Source)^[3];
              Target^ := A shl 24 or R shl 16 or G shl 8 or B;
              Inc(Source, 4);
              Inc(Target, TargetStep);
            end;
      end;
  end;
end;

function TPNGReader.GetFrameBuffer: PCardinal;
begin
  if Assigned(FFrameBuffer) then Result := @FFrameBuffer[0]
  else Result := nil;
end;

{ TPNGWriter }

procedure TPNGWriter.Analyze;
var
  I, J: Integer;
  PalCount: Integer;
  Color, LastColor: Cardinal;
begin
  { try to build a 256 color palette }
  SetLength(Palette, 257);
  Palette[0] := FFrameBuffer[0];
  PalCount := 1;
  LastColor := FFrameBuffer[0];
  for I := 1 to NumPixels - 1 do
  begin
    Color := FFrameBuffer[I];
    if Color = LastColor then Continue;
    for J := 0 to PalCount - 1 do
      if Palette[J] = Color then
      begin
        LastColor := Color;
        Break;
      end;
    if Color <> LastColor then
    begin
      Palette[PalCount] := Color;
      LastColor := Color;
      Inc(PalCount);
      if PalCount > 256 then Break;
    end;
  end;

  if PalCount > 256 then
  begin
    { not using palette }
    UsePalette := False;
    SetLength(Palette, 0);

    { analyze alpha and fulll transparency }
    UseAlpha := False;
    UseTransparency := False;
    TransparentColor := 0;
    for I := 0 to NumPixels - 1 do
    begin
      Color := FFrameBuffer[I];
      if Color <= $00FFFFFF then  // complete transparency
      begin
        if not UseTransparency then TransparentColor := Color
        else if TransparentColor <> Color then
        begin
          { we have more than one color with full transparency, use alpha }
          UseTransparency := False;
          UseAlpha := True;
          Break;
        end;
      end
      else if Color < $FF000000 then // partial transparency
      begin
        UseTransparency := False;
        UseAlpha := True;
        Break;
      end;
    end;

    { see if the can use grayscale }
    GrayScale := True;
    LastColor := 0;
    for I := 0 to NumPixels - 1 do
    begin
      Color := FFrameBuffer[I];
      if Color = LastColor then Continue;
      if ((Color and $FF) <> (Color shr 8 and $FF)) or ((Color and $FF) <> (Color shr 16 and $FF)) then
      begin
        GrayScale := False;
        Break;
      end
      else LastColor := Color;
    end;

    BitsPerSample := 8;
    if UseAlpha then
    begin
      if GrayScale then ColorScheme := csGA
      else ColorScheme := csRGBA;
    end
    else
    begin
      if GrayScale then ColorScheme := csG
      else ColorScheme := csRGB;
    end;
  end
  else
  begin
    {256 colors or less }

    if PalCount > 16 then BitsPerSample := 8
    else if PalCount > 4 then BitsPerSample := 4
    else if PalCount > 2 then BitsPerSample := 2
    else BitsPerSample := 1;

    { see if the can use grayscale }
    if BitsPerSample in [1, 8] then
    begin
      GrayScale := True;
      for I := 0 to PalCount - 1 do
      begin
        Color := Palette[I];
        if ((Color and $FF) <> (Color shr 8 and $FF)) or ((Color and $FF) <> (Color shr 16 and $FF)) then
        begin
          GrayScale := False;
          Break;
        end;
      end
    end
    else GrayScale := False;

    if GrayScale then
    begin
      { choose between csG, csG + Transparent color or revert to using palette
        based on transparency of colors in the palette }
      UseTransparency := False;
      TransparentColor := $FFFFFFFF;
      for I := 0 to PalCount - 1 do
      begin
        Color := Palette[I];
        if Color <= $00FFFFFF then
        begin
          if not UseTransparency then
          begin
            TransparentColor := Color;
            UseTransparency := True;
          end
          else // more than one transparent color
          begin
            GrayScale := False;
            Break;
          end;
        end
        else if Color < $FF000000 then // partial transparency
        begin
          GrayScale := False;
          Break;
        end;
      end;
    end;

    if GrayScale then
    begin
      UsePalette := False;
      SetLength(Palette, 0);
      ColorScheme := csG;
      if UseTransparency then
      begin
        if BitsPerSample = 1 then TransparentColor := TransparentColor and $1
        else { assuming BitsPerSample = 8 }TransparentColor := TransparentColor and $FF;
      end;
    end
    else
    begin
      ColorScheme := csIndexed;
      UsePalette := True;
      UseTransparency := False;
      SetLength(Palette, PalCount);
      for I := 0 to PalCount - 1 do
        if Palette[I] < $FF000000 then
        begin
          { TransparentColor will be ignored, instead the transparency will
            be extracted from the palette }
          UseTransparency := True;
          Break;
        end;
    end;
  end;
end;

procedure TPNGWriter.ApplyFilter(FilterType: Byte; Line, PrevLine, Target: PByteArray; BPP, BytesPerRow: Integer);
var
  I: Integer;
begin
  case FilterType of
    0: // no filter
      if Line <> Target then Move(Line[0], Target[0], BytesPerRow);

    1: // subtraction filter
      begin
        Move(Line[0], Target[0], BPP);
        for I := BPP to BytesPerRow - 1 do
          Target[I] := (Line[I] - Line[I - BPP]) and $FF;
      end;

    2: // up filter
      begin
        for I := 0 to BytesPerRow - 1 do
          Target[I] := (Line[I] - PrevLine[I]) and $FF;
      end;

    3: // average filter
      begin
        for I := 0 to BPP - 1 do
          Target[I] := Line[I] - PrevLine[I] div 2;

        for I := BPP to BytesPerRow - 1 do
          Target[I] := Line[I] - (Line[I - BPP] + PrevLine[I]) shr 1;
      end;

    4: // paeth prediction
      begin
        for I := 0 to BPP - 1 do
          Target[I] := Line[I] - PaethPredictor(0, PrevLine[I], 0);
        for I := BPP to BytesPerRow - 1 do
          Target[I] := Line[I] - PaethPredictor(Line[I - BPP], PrevLine[I], PrevLine[I - BPP]);
      end;
  end;
end;

procedure TPNGWriter.ConvertPalette;
var
  I, J: Integer;
  Color, LastColor: Cardinal;
  Index: Integer;
begin
  case ColorScheme of
    csIndexed:
      begin
        Assert(Length(Palette) <= 256);
        LastColor := FFrameBuffer[0] xor $FFFFFFFF; // make sure LastColor <> FFrameBuffer[0]
        Index := 0;
        for I := 0 to NumPixels - 1 do
        begin
          Color := FFrameBuffer[I];
          if Color <> LastColor then
          begin
            Index := -1;
            for J := 0 to Length(Palette) - 1 do
              if Palette[J] = Color then
              begin
                Index := J;
                Break;
              end;
            Assert(Index >= 0);
            LastColor := Color;
          end;
          FFrameBuffer[I] := Index;
        end;
      end;
  end;
end;

procedure TPNGWriter.ConvertRow(Src: PCardinal; Dst: PByte; Count: Integer);
var
  I: Integer;
  C: Cardinal;

  procedure Error;
  begin
    raise Exception.CreateRes(@SInvalidColorFormat);
  end;

begin
  case ColorScheme of
    csIndexed:
      case BitsPerSample of
        8: for I := 0 to Count - 1 do PByteArray(Dst)[I] := PCardinalArray(Src)[I];
        4: Pack4Bits(Src, Dst, Count);
        2: Pack2Bits(Src, Dst, Count);
        1: Pack1Bits(Src, Dst, Count);
      else
        Error;
      end;
    csG:
      case BitsPerSample of
        8: for I := 0 to Count - 1 do PByteArray(Dst)[I] := PCardinalArray(Src)[I];
        1:
          begin
            for I := 0 to Count - 1 do
              PCardinalArray(Src)[I] := PCardinalArray(Src)[I] and 1;
            Pack1Bits(Src, Dst, Count);
          end;
      else
        Error;
      end;
    csGA:
      case BitsPerSample of
        8:
          for I := 0 to Count - 1 do
          begin
            C := PCardinalArray(Src)[I];
            Dst^ := C and $FF;
            PByteArray(Dst)[1] := C shr 24;
            Inc(Dst, 2);
          end;
      else
        Error;
      end;
    csRGB:
      case BitsPerSample of
        8:
          for I := 0 to Count - 1 do
          begin
            C := PCardinalArray(Src)[I];
            Dst^ := C shr 16 and $FF;
            PByteArray(Dst)[1] := C shr 8 and $FF;
            PByteArray(Dst)[2] := C and $FF;
            Inc(Dst, 3);
          end;
      else
        Error;
      end;
    csRGBA:
      case BitsPerSample of
        8:
          for I := 0 to Count - 1 do
          begin
            C := PCardinalArray(Src)[I];
            Dst^ := C shr 16 and $FF;
            PByteArray(Dst)[1] := C shr 8 and $FF;
            PByteArray(Dst)[2] := C and $FF;
            PByteArray(Dst)[3] := C shr 24;
            Inc(Dst, 4);
          end;
      end;
  end;
end;

constructor TPNGWriter.Create(AWidth, AHeight, ARowStride: Integer; Bits: PCardinal);
var
  Idx: Integer;
begin
  if (AWidth <= 0) or (AHeight <= 0) then raise Exception.CreateRes(@SInvalidDimensions);
  FWidth := AWidth;
  FHeight := AHeight;
  NumPixels := Width * Height;
  SetLength(FFrameBuffer, NumPixels);
  if AWidth = ARowStride then MoveLongword(Bits^, FFrameBuffer[0], NumPixels)
  else
  begin
    Idx := 0;
    while AHeight > 0 do
    begin
      MoveLongWord(Bits^, FFrameBuffer[Idx], Width);
      Inc(Bits, ARowStride);
      Inc(Idx, Width);
      Dec(AHeight);
    end;
  end;
end;

procedure TPNGWriter.WriteChunk(Stream: TStream; const ChunkType: string; const Data; DataLength: Cardinal);
var
  Hdr: TPNGChunkHeader;
  CRC: Cardinal;
begin
  Assert(Length(ChunkType) = 4);
  Hdr.Length := SwapLong(DataLength);
  Hdr.ChunkType[0] := ChunkType[1];
  Hdr.ChunkType[1] := ChunkType[2];
  Hdr.ChunkType[2] := ChunkType[3];
  Hdr.ChunkType[3] := ChunkType[4];
  Stream.WriteBuffer(Hdr, SizeOf(Hdr));
  CRC := CRC32(0, @Hdr.ChunkType, 4);
  Stream.WriteBuffer(Data, DataLength);
  CRC := CRC32(CRC, @Data, DataLength);
  CRC := SwapLong(CRC);
  Stream.WriteBuffer(CRC, 4);
end;

procedure TPNGWriter.WriteData(Stream: TStream);
var
  FilterType, F: Byte;
  UseFixedFilterType: Boolean;
  RowIndex: Integer;
  BPP: Integer;
  BytesPerRow: Integer;
  SrcBuffer: TByteDynArray;
  RowBuffer: array [0..4] of TByteDynArray;
  Buf: PByteArray;
  MemStream: TMemoryStream;
  CompressionStream: TCompressionStream;
  I, Sum, MinSum: Integer;

  procedure CalcByteCount(NumPixels: Integer);
  begin
    case ColorScheme of
      csG, csIndexed:
        begin
          BPP := 1;
          BytesPerRow := (NumPixels * BitsPerSample + 7) div 8;
        end;
      csRGB:
        begin
          BPP := 3;
          BytesPerRow := NumPixels * BitsPerSample * 3 div 8;
        end;
      csGA:
        begin
          BPP := 2;
          BytesPerRow := NumPixels * BitsPerSample * 2 div 8;
        end;
      csRGBA:
        begin
          BPP := 4;
          BytesPerRow := NumPixels * BitsPerSample * 4 div 8;
        end;
    end;
  end;

begin
  { Choose filter type according to PNG Spec 1.2 (Section 12.8 Filter selection) }
  FilterType := 0;
  UseFixedFilterType := (ColorScheme = csIndexed) or (BitsPerSample <= 4);

  CalcByteCount(Width);

  SetLength(RowBuffer[0], BytesPerRow);
  SetLength(SrcBuffer, BytesPerRow);

  if not UseFixedFilterType then
    for FilterType := 1 to 4 do SetLength(RowBuffer[FilterType], BytesPerRow)
  else if FilterType <> 0 then
    SetLength(RowBuffer[FilterType], BytesPerRow);

  ZeroMemory(@RowBuffer[0][0], BytesPerRow);

  MemStream := TMemoryStream.Create;
  try
    CompressionStream := TCompressionStream.Create(MemStream);
    try
      for RowIndex := 0 to Height - 1 do
      begin
        ConvertRow(@FFrameBuffer[RowIndex * Width], @SrcBuffer[0], Width);
        if not UseFixedFilterType then
          for FilterType := 1 to 4 do
            ApplyFilter(FilterType, @SrcBuffer[0], @RowBuffer[0][0], @RowBuffer[FilterType][0], BPP, BytesPerRow)
        else if FilterType > 0 then
          ApplyFilter(FilterType, @SrcBuffer[0], @RowBuffer[0][0], @RowBuffer[FilterType][0], BPP, BytesPerRow);
        Move(SrcBuffer[0], RowBuffer[0][0], BytesPerRow);

        if not UseFixedFilterType then
        begin
          { use a simple heuristics test to estimate the best filter as
            suggested in PNG specs }
          MinSum := MaxInt;
          FilterType := 0;
          for F := 0 to 4 do
          begin
            Sum := 0;
            Buf := PByteArray(@RowBuffer[F][0]);
            for I := 0 to BytesPerRow - 1 do Inc(Sum, Abs(Buf[I]));
            if Sum < MinSum then
            begin
              MinSum := Sum;
              FilterType := F;
            end;
          end;
        end;
        CompressionStream.Write(FilterType, 1);
        CompressionStream.Write(RowBuffer[FilterType][0], BytesPerRow);
      end;
    finally
      CompressionStream.Free;
    end;
    WriteChunk(Stream, 'IDAT', MemStream.Memory^, MemStream.Size);
  finally
    MemStream.Free;
  end;
end;

procedure TPNGWriter.WritePalette(Stream: TStream);
var
  Buffer: TByteDynArray;
  P: PByte;
  I, Count: Integer;
  Color: Cardinal;
begin
  Count := Length(Palette);
  if Count > 0 then
  begin
    SetLength(Buffer, Count * 3);
    P := @Buffer[0];
    for I := 0 to Count - 1 do
    begin
      Color := Palette[I];
      P^ := Color shr 16 and $FF;
      PByteArray(P)^[1] := Color shr 8 and $FF;
      PByteArray(P)^[2] := Color and $FF;
      Inc(P, 3);
    end;
    WriteChunk(Stream, 'PLTE', Buffer[0], Count * 3);
  end;
end;

procedure TPNGWriter.WriteToStream(Stream: TStream);
const
  ColorTypes: array [TColorScheme] of Byte = ($FF, 3, 0, 4, 2, 6);
var
  HdrChunk: TIHDRChunk;
  Buffer: TByteDynArray;
begin
  SetLength(Buffer, 0);
  Stream.Write(PNGMagic[0], 8);

  Analyze;

  HdrChunk.Width := SwapLong(Width);
  HdrChunk.Height := SwapLong(Height);
  HdrChunk.BitDepth := BitsPerSample;
  HdrChunk.ColorType := ColorTypes[ColorScheme];
  HdrChunk.Compression := 0;
  HdrChunk.Filter := 0;
  HdrChunk.Interlaced := 0;

  WriteChunk(Stream, 'IHDR', HdrChunk, SizeOf(HdrChunk));

  if ColorScheme = csIndexed then
  begin
    ConvertPalette;
    WritePalette(Stream);
  end;

  WriteTransparency(Stream);
  WriteData(Stream);
  WriteChunk(Stream, 'IEND', Buffer, 0);

end;

procedure TPNGWriter.WriteTransparency(Stream: TStream);
var
  Buffer: TByteDynArray;
  I: Integer;
  Color: Cardinal;
begin
  if UseTransparency then
  begin
    case ColorScheme of
      csIndexed:
        begin
          SetLength(Buffer, Length(Palette));
          for I := 0 to Length(Palette) - 1 do
          begin
            Color := Palette[I];
            Buffer[I] := Color shr 24;
          end;
          I := Length(Palette) - 1;
          while (I >= 0) and (Buffer[I] = $FF) do Dec(I);
          if I >= 0 then
            WriteChunk(Stream, 'tRNS', Buffer[0], I + 1);
        end;
      csG:
        begin
          SetLength(Buffer, 2);
          Buffer[0] := 0;
          Buffer[1] := TransparentColor;
          WriteChunk(Stream, 'tRNS', Buffer[0], 2);
        end;
      csRGB:
        begin
          SetLength(Buffer, 6);
          Buffer[0] := 0;
          Buffer[1] := TransparentColor shr 16 and $FF;
          Buffer[2] := 0;
          Buffer[3] := TransparentColor shr 8 and $FF;
          Buffer[4] := 0;
          Buffer[5] := TransparentColor and $FF;
          WriteChunk(Stream, 'tRNS', Buffer[0], 6);
        end;
    end;
  end;
end;

end.
