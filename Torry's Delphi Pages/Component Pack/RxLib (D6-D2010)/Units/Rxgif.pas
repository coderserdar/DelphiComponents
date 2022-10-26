{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxGIF;

interface

{$I RX.INC}

uses
  Windows, SysUtils, Classes, Graphics, RxGraph;

const
  RT_GIF = 'GIF'; { GIF Resource Type }

type

{$IFNDEF RX_D3}

  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
    const Msg: string) of object;

{ TSharedImage }

  TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference;
    procedure Release;
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  end;

{$ENDIF RX_D3}

  TGIFVersion = (gvUnknown, gv87a, gv89a);
  TGIFBits = 1..8;
  TDisposalMethod = (dmUndefined, dmLeave, dmRestoreBackground,
    dmRestorePrevious, dmReserved4, dmReserved5, dmReserved6, dmReserved7);

  TGIFColorItem = packed record
    Red, Green, Blue: Byte;
  end;

  TGIFColorTable = packed record
    Count: Integer;
    Colors: packed array[Byte] of TGIFColorItem;
  end;

  TGIFFrame = class;
  TGIFData = class;
  TGIFItem = class;

{ TGIFImage }

  TGIFImage = class(TGraphic)
  private
    FImage: TGIFData;
    FVersion: TGIFVersion;
    FItems: TList;
    FFrameIndex: Integer;
    FScreenWidth: Word;
    FScreenHeight: Word;
    FBackgroundColor: TColor;
    FLooping: Boolean;
    FCorrupted: Boolean;
    FRepeatCount: Word;
{$IFNDEF RX_D3}
    FOnProgress: TProgressEvent;
{$ENDIF}
    function GetBitmap: TBitmap;
    function GetCount: Integer;
    function GetComment: TStrings;
    function GetScreenWidth: Integer;
    function GetScreenHeight: Integer;
    function GetGlobalColorCount: Integer;
    procedure UpdateScreenSize;
    procedure SetComment(Value: TStrings);
    function GetFrame(Index: Integer): TGIFFrame;
    procedure SetFrameIndex(Value: Integer);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetLooping(Value: Boolean);
    procedure SetRepeatCount(Value: Word);
    procedure ReadSignature(Stream: TStream);
    procedure DoProgress(Stage: TProgressStage; PercentDone: Byte;
      const Msg: string);
    function GetCorrupted: Boolean;
    function GetTransparentColor: TColor;
    function GetBackgroundColor: TColor;
    function GetPixelFormat: TPixelFormat;
    procedure EncodeFrames(ReverseDecode: Boolean);
    procedure ReadStream(Size: Longint; Stream: TStream; ForceDecode: Boolean);
    procedure WriteStream(Stream: TStream; WriteSize: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetPalette: HPALETTE; {$IFDEF RX_D3} override; {$ENDIF}
    function GetTransparent: Boolean; {$IFDEF RX_D3} override; {$ENDIF}
    procedure ClearItems;
    procedure NewImage;
    procedure UniqueImage;
{$IFNDEF RX_D3}
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string); dynamic;
{$ENDIF}
    procedure ReadData(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
    property Bitmap: TBitmap read GetBitmap;   { volatile }
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    procedure DecodeAllFrames;
    procedure EncodeAllFrames;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: string;
      ResType: PChar);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer;
      ResType: PChar);
    function AddFrame(Value: TGraphic): Integer; virtual;
    procedure DeleteFrame(Index: Integer);
    procedure MoveFrame(CurIndex, NewIndex: Integer);
    procedure Grayscale(ForceEncoding: Boolean);
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property Comment: TStrings read GetComment write SetComment;
    property Corrupted: Boolean read GetCorrupted;
    property Count: Integer read GetCount;
    property Frames[Index: Integer]: TGIFFrame read GetFrame; default;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex;
    property GlobalColorCount: Integer read GetGlobalColorCount;
    property Looping: Boolean read FLooping write SetLooping;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property RepeatCount: Word read FRepeatCount write SetRepeatCount;
    property ScreenWidth: Integer read GetScreenWidth;
    property ScreenHeight: Integer read GetScreenHeight;
    property TransparentColor: TColor read GetTransparentColor;
    property Version: TGIFVersion read FVersion;
{$IFNDEF RX_D3}
    property Palette: HPALETTE read GetPalette;
    property Transparent: Boolean read GetTransparent;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
{$ENDIF}
  end;

{ TGIFFrame }

  TGIFFrame = class(TPersistent)
  private
    FOwner: TGIFImage;
    FBitmap: TBitmap;
    FImage: TGIFItem;
    FExtensions: TList;
    FTopLeft: TPoint;
    FInterlaced: Boolean;
    FCorrupted: Boolean;
    FGrayscale: Boolean;
    FTransparentColor: TColor;
    FAnimateInterval: Word;
    FDisposal: TDisposalMethod;
    FLocalColors: Boolean;
    function GetBitmap: TBitmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetColorCount: Integer;
    function FindComment(ForceCreate: Boolean): TStrings;
    function GetComment: TStrings;
    procedure SetComment(Value: TStrings);
    procedure SetTransparentColor(Value: TColor);
    procedure SetDisposalMethod(Value: TDisposalMethod);
    procedure SetAnimateInterval(Value: Word);
    procedure SetTopLeft(const Value: TPoint);
    procedure NewBitmap;
    procedure NewImage;
    procedure SaveToBitmapStream(Stream: TMemoryStream);
    procedure EncodeBitmapStream(Stream: TMemoryStream);
    procedure EncodeRasterData;
    procedure UpdateExtensions;
    procedure WriteImageDescriptor(Stream: TStream);
    procedure WriteLocalColorMap(Stream: TStream);
    procedure WriteRasterData(Stream: TStream);
  protected
    constructor Create(AOwner: TGIFImage); virtual;
    procedure LoadFromStream(Stream: TStream);
    procedure AssignTo(Dest: TPersistent); override;
    procedure GrayscaleImage(ForceEncoding: Boolean);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect;
      Transparent: Boolean);
    property AnimateInterval: Word read FAnimateInterval write SetAnimateInterval;
    property Bitmap: TBitmap read GetBitmap; { volatile }
    property ColorCount: Integer read GetColorCount;
    property Comment: TStrings read GetComment write SetComment;
    property DisposalMethod: TDisposalMethod read FDisposal write SetDisposalMethod;
    property Interlaced: Boolean read FInterlaced;
    property Corrupted: Boolean read FCorrupted;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property Origin: TPoint read FTopLeft write SetTopLeft;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

{ TGIFData }

  TGIFData = class(TSharedImage)
  private
    FComment: TStrings;
    FAspectRatio: Byte;
    FBitsPerPixel: Byte;
    FColorResBits: Byte;
    FColorMap: TGIFColorTable;
  protected
    procedure FreeHandle; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TGIFItem }

  TGIFItem = class(TSharedImage)
  private
    FImageData: TMemoryStream;
    FSize: TPoint;
    FPackedFields: Byte;
    FBitsPerPixel: Byte;
    FColorMap: TGIFColorTable;
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

{ Clipboard format for GIF image }

var
  CF_GIF: Word;

{ Load incomplete or corrupted images without exceptions }

const
  GIFLoadCorrupted: Boolean = True;

function GIFVersionName(Version: TGIFVersion): string;
procedure rxgif_dummy;

implementation

uses
  Consts, 
  {$IFDEF RX_D6} RTLConsts, {$ENDIF} // Polaris
  {$IFNDEF RX_D12} rxStrUtils, {$ENDIF}
  rxVclUtils, rxAniFile, RxConst, rxMaxMin, RxGConst;

{$R-}

procedure rxgif_dummy;
begin
end;

procedure GifError(const Msg: string);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EInvalidGraphicOperation.Create(Msg) at ReturnAddr;
end;

{$IFNDEF RX_D3}

{ TSharedImage }

procedure TSharedImage.Reference;
begin
  Inc(FRefCount);
end;

procedure TSharedImage.Release;
begin
  if Pointer(Self) <> nil then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
    begin
      FreeHandle;
      Free;
    end;
  end;
end;

{$ENDIF}

const
  GIFSignature = 'GIF';
  GIFVersionStr: array[TGIFVersion] of PAnsiChar = (#0#0#0, '87a', '89a');

function GIFVersionName(Version: TGIFVersion): string;
begin
  Result := string(StrPas(GIFVersionStr[Version]));
end;

const
  CODE_TABLE_SIZE = 4096;
  HASH_TABLE_SIZE = 17777;
  MAX_LOOP_COUNT  = 30000;

  CHR_EXT_INTRODUCER    = '!';
  CHR_IMAGE_SEPARATOR   = ',';
  CHR_TRAILER           = ';';  { indicates the end of the GIF Data stream }

{ Image descriptor bit masks }

  ID_LOCAL_COLOR_TABLE  = $80;  { set if a local color table follows }
  ID_INTERLACED         = $40;  { set if image is interlaced }
  ID_SORT               = $20;  { set if color table is sorted }
  ID_RESERVED           = $0C;  { reserved - must be set to $00 }
  ID_COLOR_TABLE_SIZE   = $07;  { Size of color table as above }

{ Logical screen descriptor packed field masks }

  LSD_GLOBAL_COLOR_TABLE = $80; { set if global color table follows L.S.D. }
  LSD_COLOR_RESOLUTION   = $70; { Color resolution - 3 bits }
  LSD_SORT               = $08; { set if global color table is sorted - 1 bit }
  LSD_COLOR_TABLE_SIZE   = $07; { Size of global color table - 3 bits }
                                { Actual Size = 2^value+1    - value is 3 bits }

{ Graphic control extension packed field masks }

  GCE_TRANSPARENT     = $01; { whether a transparency Index is given }
  GCE_USER_INPUT      = $02; { whether or not user input is expected }
  GCE_DISPOSAL_METHOD = $1C; { the way in which the graphic is to be treated after being displayed }
  GCE_RESERVED        = $E0; { reserved - must be set to $00 }

{ Application extension }

  AE_LOOPING          = $01; { looping Netscape extension }

  GIFColors: array[TGIFBits] of Word = (2, 4, 8, 16, 32, 64, 128, 256);

function ColorsToBits(ColorCount: Word): Byte; near;
var
  I: TGIFBits;
begin
  Result := 0;
  for I := Low(TGIFBits) to High(TGIFBits) do
    if ColorCount = GIFColors[I] then
    begin
      Result := I;
      Exit;
    end;
  GifError(LoadStr(SWrongGIFColors));
end;

function ColorsToPixelFormat(Colors: Word): TPixelFormat;
begin
  if Colors <= 2 then
    Result := pf1bit
  else
  if Colors <= 16 then
    Result := pf4bit
  else
  if Colors <= 256 then
    Result := pf8bit
  else
    Result := pf24bit;
end;

function ItemToRGB(Item: TGIFColorItem): Longint; near;
begin
  with Item do
    Result := RGB(Red, Green, Blue);
end;

function GrayColor(Color: TColor): TColor;
var
  Index: Integer;
begin
  Index := Byte(Longint(Word(GetRValue(Color)) * 77 +
    Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) shr 8);
  Result := RGB(Index, Index, Index);
end;

procedure GrayColorTable(var ColorTable: TGIFColorTable);
var
  I: Byte;
  Index: Integer;
begin
  for I := 0 to ColorTable.Count - 1 do
  begin
    with ColorTable.Colors[I] do
    begin
      Index := Byte(Longint(Word(Red) * 77 + Word(Green) * 150 +
        Word(Blue) * 29) shr 8);
      Red := Index;
      Green := Index;
      Blue := Index;
    end;
  end;
end;

function FindColorIndex(const ColorTable: TGIFColorTable;
  Color: TColor): Integer;
begin
  if (Color <> clNone) then
    for Result := 0 to ColorTable.Count - 1 do
      if ItemToRGB(ColorTable.Colors[Result]) = ColorToRGB(Color) then Exit;
  Result := -1;
end;

{ The following types and function declarations are used to call into
  functions of the GIF implementation of the GIF image
  compression/decompression standard. }

type
  TGIFHeader = packed record
    Signature: array[0..2] of AnsiChar; { contains 'GIF' }
    Version: array[0..2] of AnsiChar;   { '87a' or '89a' }
  end;

  TScreenDescriptor = packed record
    ScreenWidth: Word;            { logical screen width }
    ScreenHeight: Word;           { logical screen height }
    PackedFields: Byte;
    BackgroundColorIndex: Byte;   { Index to global color table }
    AspectRatio: Byte;            { actual ratio = (AspectRatio + 15) / 64 }
  end;

  TImageDescriptor = packed record
    ImageLeftPos: Word;   { column in pixels in respect to left of logical screen }
    ImageTopPos: Word;    { row in pixels in respect to top of logical screen }
    ImageWidth: Word;     { width of image in pixels }
    ImageHeight: Word;    { height of image in pixels }
    PackedFields: Byte;
  end;

{ GIF Extensions support }

type
  TExtensionType = (etGraphic, etPlainText, etApplication, etComment);

const
  ExtLabels: array[TExtensionType] of Byte = ($F9, $01, $FF, $FE);
  LoopExtNS: string[11] = 'NETSCAPE2.0';
  LoopExtAN: string[11] = 'ANIMEXTS1.0';

type
  TGraphicControlExtension = packed record
    BlockSize: Byte; { should be 4 }
    PackedFields: Byte;
    DelayTime: Word; { in centiseconds }
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

  TPlainTextExtension = packed record
    BlockSize: Byte; { should be 12 }
    Left, Top, Width, Height: Word;
    CellWidth, CellHeight: Byte;
    FGColorIndex, BGColorIndex: Byte;
  end;

  TAppExtension = packed record
    BlockSize: Byte; { should be 11 }
    AppId: array[1..8] of Byte;
    Authentication: array[1..3] of Byte;
  end;

  TExtensionRecord = packed record
    case ExtensionType: TExtensionType of
      etGraphic: (GCE: TGraphicControlExtension);
      etPlainText: (PTE: TPlainTextExtension);
      etApplication: (APPE: TAppExtension);
  end;

{ TExtension }

  TExtension = class(TPersistent)
  private
    FExtType: TExtensionType;
    FData: TStrings;
    FExtRec: TExtensionRecord;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsLoopExtension: Boolean;
  end;

destructor TExtension.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TExtension.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TExtension) then
  begin
    FExtType := TExtension(Source).FExtType;
    FExtRec := TExtension(Source).FExtRec;
    if TExtension(Source).FData <> nil then
    begin
      if FData = nil then
        FData := TStringList.Create;
      FData.Assign(TExtension(Source).FData);
    end;
  end
  else
    inherited Assign(Source);
end;

function TExtension.IsLoopExtension: Boolean;
begin
  Result := (FExtType = etApplication) and (FData.Count > 0) and
    (CompareMem(@FExtRec.APPE.AppId, @LoopExtNS[1], FExtRec.APPE.BlockSize) or
    CompareMem(@FExtRec.APPE.AppId, @LoopExtAN[1], FExtRec.APPE.BlockSize)) and
    (Length(FData[0]) >= 3) and (Byte(FData[0][1]) = AE_LOOPING);
end;

procedure FreeExtensions(Extensions: TList); near;
begin
  if Extensions <> nil then
  begin
    while Extensions.Count > 0 do
    begin
      TObject(Extensions[0]).Free;
      Extensions.Delete(0);
    end;
    Extensions.Free;
  end;
end;

function FindExtension(Extensions: TList; ExtType: TExtensionType): TExtension;
var
  I: Integer;
begin
  if Extensions <> nil then
    for I := Extensions.Count - 1 downto 0 do
    begin
      Result := TExtension(Extensions[I]);
      if (Result <> nil) and (Result.FExtType = ExtType) then
        Exit;
    end;
  Result := nil;
end;

{
function CopyExtensions(Source: TList): TList; near;
var
  I: Integer;
  Ext: TExtension;
begin
  Result := TList.Create;
  try
    for I := 0 to Source.Count - 1 do
      if (Source[I] <> nil) and (TObject(Source[I]) is TExtension) then begin
        Ext := TExtension.Create;
        try
          Ext.Assign(Source[I]);
          Result.Add(Ext);
        except
          Ext.Free;
          raise;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;
}

type
  TProgressProc = procedure (Stage: TProgressStage; PercentDone: Byte;
    const Msg: string) of object;

{ GIF reading/writing routines

  Procedures to read and write GIF files, GIF-decoding and encoding
  based on freeware C source code of GBM package by Andy Key
  (nyangau@interalpha.co.uk). The home page of GBM author is
  at http://www.interalpha.net/customer/nyangau/. }

type
  PIntCodeTable = ^TIntCodeTable;
  TIntCodeTable = array[0..CODE_TABLE_SIZE - 1] of Word;

  PReadContext = ^TReadContext;
  TReadContext = record
    Inx, Size: Longint;
    Buf: array[0..255 + 4] of Byte;
    CodeSize: Longint;
    ReadMask: Longint;
  end;

  PWriteContext = ^TWriteContext;
  TWriteContext = record
    Inx: Longint;
    CodeSize: Longint;
    Buf: array[0..255 + 4] of Byte;
  end;

  TOutputContext = record
    W, H, X, Y: Longint;
    BitsPerPixel, Pass: Integer;
    Interlace: Boolean;
    LineIdent: Longint;
    Data, CurrLineData: Pointer;
  end;

  PImageDict = ^TImageDict;
  TImageDict = record
    Tail, Index: Word;
    Col: Byte;
  end;

  PDictTable = ^TDictTable;
  TDictTable = array[0..CODE_TABLE_SIZE - 1] of TImageDict;

  PRGBPalette = ^TRGBPalette;
  TRGBPalette = array [Byte] of TRGBQuad;

function InitHash(P: Longint): Longint;
begin
  Result := (P + 3) * 301;
end;

function InterlaceStep(Y, Height: Integer; var Pass: Integer): Integer;
begin
  Result := Y;
  case Pass of
    0, 1: Inc(Result, 8);
    2: Inc(Result, 4);
    3: Inc(Result, 2);
  end;
  if Result >= Height then
  begin
    if Pass = 0 then
    begin
      Pass := 1; Result := 4;
      if (Result < Height) then
        Exit;
    end;
    if Pass = 1 then
    begin
      Pass := 2; Result := 2;
      if (Result < Height) then
        Exit;
    end;
    if Pass = 2 then
    begin
      Pass := 3;
      Result := 1;
    end;
  end;
end;

procedure ReadImageStream(Stream, Dest: TStream; var Desc: TImageDescriptor;
  var Interlaced, LocalColors, Corrupted: Boolean; var BitsPerPixel: Byte;
  var ColorTable: TGIFColorTable);
var
  CodeSize, BlockSize: Byte;
begin
  Corrupted := False;
  Stream.ReadBuffer(Desc, SizeOf(TImageDescriptor));
  Interlaced := (Desc.PackedFields and ID_INTERLACED) <> 0;
  if (Desc.PackedFields and ID_LOCAL_COLOR_TABLE) <> 0 then
  begin
    { Local colors table follows }
    BitsPerPixel := 1 + Desc.PackedFields and ID_COLOR_TABLE_SIZE;
    LocalColors := True;
    ColorTable.Count := 1 shl BitsPerPixel;
    Stream.ReadBuffer(ColorTable.Colors[0],
      ColorTable.Count * SizeOf(TGIFColorItem));
  end
  else
  begin
    LocalColors := False;
    FillChar(ColorTable, SizeOf(ColorTable), 0);
  end;
  Stream.ReadBuffer(CodeSize, 1);
  Dest.Write(CodeSize, 1);
  repeat
    Stream.Read(BlockSize, 1);
    if (Stream.Position + BlockSize) > Stream.Size then
    begin
      Corrupted := True;
      Stream.Position := Stream.Size;
      Exit;
    end;
    Dest.Write(BlockSize, 1);
    if (Stream.Position + BlockSize) > Stream.Size then
    begin
      BlockSize := Stream.Size - Stream.Position;
      Corrupted := True;
    end;
    if BlockSize > 0 then
      Dest.CopyFrom(Stream, BlockSize);
  until (BlockSize = 0) or (Stream.Position >= Stream.Size);
end;

procedure FillRGBPalette(const ColorTable: TGIFColorTable;
  var Colors: TRGBPalette);
var
  I: Byte;
begin
  FillChar(Colors, SizeOf(Colors), $80);
  for I := 0 to ColorTable.Count - 1 do
  begin
    Colors[I].rgbRed := ColorTable.Colors[I].Red;
    Colors[I].rgbGreen := ColorTable.Colors[I].Green;
    Colors[I].rgbBlue := ColorTable.Colors[I].Blue;
    Colors[I].rgbReserved := 0;
  end;
end;

function ReadCode(Stream: TStream; var Context: TReadContext): Longint;
var
  RawCode: Longint;
  ByteIndex: Longint;
  Bytes: Byte;
  BytesToLose: Longint;
begin
  while (Context.Inx + Context.CodeSize > Context.Size) and
    (Stream.Position < Stream.Size) do
  begin
    { not enough bits in buffer - refill it }
    { Not very efficient, but infrequently called }
    BytesToLose := Context.Inx shr 3;
    { Note biggest Code Size is 12 bits. And this can at worst span 3 Bytes }
    Move(Context.Buf[Word(BytesToLose)], Context.Buf[0], 3);
    Context.Inx := Context.Inx and 7;
    Context.Size := Context.Size - (BytesToLose shl 3);
    Stream.ReadBuffer(Bytes, 1);
    if Bytes > 0 then
      Stream.ReadBuffer(Context.Buf[Word(Context.Size shr 3)], Bytes);
    Context.Size := Context.Size + (Bytes shl 3);
  end;
  ByteIndex := Context.Inx shr 3;
  RawCode := Context.Buf[Word(ByteIndex)] +
    (Word(Context.Buf[Word(ByteIndex + 1)]) shl 8);
  if Context.CodeSize > 8 then
    RawCode := RawCode + (Longint(Context.Buf[ByteIndex + 2]) shl 16);
  RawCode := RawCode shr (Context.Inx and 7);
  Context.Inx := Context.Inx + Byte(Context.CodeSize);
  Result := RawCode and Context.ReadMask;
end;

procedure Output(Value: Byte; var Context: TOutputContext);
var
  P: PByte;
begin
  if (Context.Y >= Context.H) then
    Exit;
  case Context.BitsPerPixel of
    1: begin
         P := HugeOffset(Context.CurrLineData, Context.X shr 3);
         if (Context.X and $07 <> 0) then
           P^ := P^ or Word(value shl (7 - (Word(Context.X and 7))))
         else P^ := Byte(value shl 7);
       end;
    4: begin
         P := HugeOffset(Context.CurrLineData, Context.X shr 1);
         if (Context.X and 1 <> 0) then P^ := P^ or Value
         else P^ := Byte(value shl 4);
       end;
    8: begin
         P := HugeOffset(Context.CurrLineData, Context.X);
         P^ := Value;
       end;
  end;
  Inc(Context.X);
  if Context.X < Context.W then
    Exit;
  Context.X := 0;
  if Context.Interlace then
    Context.Y := InterlaceStep(Context.Y, Context.H, Context.Pass)
  else
    Inc(Context.Y);
  Context.CurrLineData := HugeOffset(Context.Data,
    (Context.H - 1 - Context.Y) * Context.LineIdent);
end;

procedure ReadGIFData(Stream: TStream; const Header: TBitmapInfoHeader;
  Interlaced, LoadCorrupt: Boolean; IntBitPerPixel: Byte; Data: Pointer;
  var Corrupted: Boolean; ProgressProc: TProgressProc);
var
  MinCodeSize, Temp: Byte;
  MaxCode, BitMask, InitCodeSize: Longint;
  ClearCode, EndingCode, FirstFreeCode, FreeCode: Word;
  I, OutCount, Code: Longint;
  CurCode, OldCode, InCode, FinalChar: Word;
  Prefix, Suffix, OutCode: PIntCodeTable;
  ReadCtxt: TReadContext;
  OutCtxt: TOutputContext;
  TableFull: Boolean;
begin
  Corrupted := False;
  OutCount := 0; OldCode := 0; FinalChar := 0;
  TableFull := False;
  Prefix := AllocMem(SizeOf(TIntCodeTable));
  try
    Suffix := AllocMem(SizeOf(TIntCodeTable));
    try
      OutCode := AllocMem(SizeOf(TIntCodeTable) + SizeOf(Word));
      try
        if Assigned(ProgressProc) then
          ProgressProc(psStarting, 0, '');
        try
          Stream.ReadBuffer(MinCodeSize, 1);
          if (MinCodeSize < 2) or (MinCodeSize > 9) then
          begin
            if LoadCorrupt then
            begin
              Corrupted := True;
              MinCodeSize := Max(2, Min(MinCodeSize, 9));
            end
            else
              GifError(LoadStr(SBadGIFCodeSize));
          end;
          { Initial read context }
          ReadCtxt.Inx := 0;
          ReadCtxt.Size := 0;
          ReadCtxt.CodeSize := MinCodeSize + 1;
          ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
          { Initialise pixel-output context }
          OutCtxt.X := 0; OutCtxt.Y := 0;
          OutCtxt.Pass := 0;
          OutCtxt.W := Header.biWidth;
          OutCtxt.H := Header.biHeight;
          OutCtxt.BitsPerPixel := Header.biBitCount;
          OutCtxt.Interlace := Interlaced;
          OutCtxt.LineIdent := ((Header.biWidth * Header.biBitCount + 31)
            div 32) * 4;
          OutCtxt.Data := Data;
          OutCtxt.CurrLineData := HugeOffset(Data, (Header.biHeight - 1) *
            OutCtxt.LineIdent);
          BitMask := (1 shl IntBitPerPixel) - 1;
          { 2 ^ MinCodeSize accounts for all colours in file }
          ClearCode := 1 shl MinCodeSize;
          EndingCode := ClearCode + 1;
          FreeCode := ClearCode + 2;
          FirstFreeCode := FreeCode;
          { 2^ (MinCodeSize + 1) includes clear and eoi Code and space too }
          InitCodeSize := ReadCtxt.CodeSize;
          MaxCode := 1 shl ReadCtxt.CodeSize;
          Code := ReadCode(Stream, ReadCtxt);
          while (Code <> EndingCode) and (Code <> $FFFF) and
            (OutCtxt.Y < OutCtxt.H) do
          begin
            if (Code = ClearCode) then
            begin
              ReadCtxt.CodeSize := InitCodeSize;
              MaxCode := 1 shl ReadCtxt.CodeSize;
              ReadCtxt.ReadMask := MaxCode - 1;
              FreeCode := FirstFreeCode;
              Code := ReadCode(Stream, ReadCtxt);
              CurCode := Code; OldCode := Code;
              if (Code = $FFFF) then Break;
              FinalChar := (CurCode and BitMask);
              Output(Byte(FinalChar), OutCtxt);
              TableFull := False;
            end
            else
            begin
              CurCode := Code;
              InCode := Code;
              if CurCode >= FreeCode then
              begin
                CurCode := OldCode;
                OutCode^[OutCount] := FinalChar;
                Inc(OutCount);
              end;
              while (CurCode > BitMask) do
              begin
                if (OutCount > CODE_TABLE_SIZE) then
                begin
                  if LoadCorrupt then
                  begin
                    CurCode := BitMask;
                    OutCount := 1;
                    Corrupted := True;
                    Break;
                  end
                  else
                    GifError(LoadStr(SGIFDecodeError));
                end;
                OutCode^[OutCount] := Suffix^[CurCode];
                Inc(OutCount);
                CurCode := Prefix^[CurCode];
              end;
              if Corrupted then
                Break;
              FinalChar := CurCode and BitMask;
              OutCode^[OutCount] := FinalChar;
              Inc(OutCount);
              for I := OutCount - 1 downto 0 do
                Output(Byte(OutCode^[I]), OutCtxt);
              OutCount := 0;
              { Update dictionary }
              if not TableFull then
              begin
                Prefix^[FreeCode] := OldCode;
                Suffix^[FreeCode] := FinalChar;
                { Advance to next free slot }
                Inc(FreeCode);
                if (FreeCode >= MaxCode) then
                begin
                  if (ReadCtxt.CodeSize < 12) then
                  begin
                    Inc(ReadCtxt.CodeSize);
                    MaxCode := MaxCode shl 1;
                    ReadCtxt.ReadMask := (1 shl ReadCtxt.CodeSize) - 1;
                  end
                  else
                    TableFull := True;
                end;
              end;
              OldCode := InCode;
            end;
            Code := ReadCode(Stream, ReadCtxt);
            if Stream.Size > 0 then
            begin
              Temp := Trunc(100.0 * (Stream.Position / Stream.Size));
              if Assigned(ProgressProc) then ProgressProc(psRunning, Temp, '');
            end;
          end; { while }
          if Code = $FFFF then
            GifError(ResStr(SReadError));
        finally
          if Assigned(ProgressProc) then
          begin
            if ExceptObject = nil then
              ProgressProc(psEnding, 100, '')
            else
              ProgressProc(psEnding, 0, Exception(ExceptObject).Message);
          end;
        end;
      finally
        FreeMem(OutCode, SizeOf(TIntCodeTable) + SizeOf(Word));
      end;
    finally
      FreeMem(Suffix, SizeOf(TIntCodeTable));
    end;
  finally
    FreeMem(Prefix, SizeOf(TIntCodeTable));
  end;
end;

procedure WriteCode(Stream: TStream; Code: Longint;
  var Context: TWriteContext);
var
  BufIndex: Longint;
  Bytes: Byte;
begin
  BufIndex := Context.Inx shr 3;
  Code := Code shl (Context.Inx and 7);
  Context.Buf[BufIndex] := Context.Buf[BufIndex] or (Code);
  Context.Buf[BufIndex + 1] := (Code shr 8);
  Context.Buf[BufIndex + 2] := (Code shr 16);
  Context.Inx := Context.Inx + Context.CodeSize;
  if Context.Inx >= 255 * 8 then
  begin
    { Flush out full buffer }
    Bytes := 255;
    Stream.WriteBuffer(Bytes, 1);
    Stream.WriteBuffer(Context.Buf, Bytes);
    Move(Context.Buf[255], Context.Buf[0], 2);
    FillChar(Context.Buf[2], 255, 0);
    Context.Inx := Context.Inx - (255 * 8);
  end;
end;

procedure FlushCode(Stream: TStream; var Context: TWriteContext);
var
  Bytes: Byte;
begin
  Bytes := (Context.Inx + 7) shr 3;
  if Bytes > 0 then
  begin
    Stream.WriteBuffer(Bytes, 1);
    Stream.WriteBuffer(Context.Buf, Bytes);
  end;
  { Data block terminator - a block of zero Size }
  Bytes := 0;
  Stream.WriteBuffer(Bytes, 1);
end;

procedure FillColorTable(var ColorTable: TGIFColorTable;
  const Colors: TRGBPalette; Count: Integer);
var
  I: Byte;
begin
  FillChar(ColorTable, SizeOf(ColorTable), 0);
  ColorTable.Count := Min(256, Count);
  for I := 0 to ColorTable.Count - 1 do
  begin
    ColorTable.Colors[I].Red := Colors[I].rgbRed;
    ColorTable.Colors[I].Green := Colors[I].rgbGreen;
    ColorTable.Colors[I].Blue := Colors[I].rgbBlue;
  end;
end;

procedure WriteGIFData(Stream: TStream; var Header: TBitmapInfoHeader;
  Interlaced: Boolean; Data: Pointer; ProgressProc: TProgressProc);
  { LZW encode data }
var
  LineIdent: Longint;
  MinCodeSize, Col, Temp: Byte;
  InitCodeSize, X, Y: Longint;
  Pass: Integer;
  MaxCode: Longint; { 1 shl CodeSize }
  ClearCode, EndingCode, LastCode, Tail: Longint;
  I, HashValue: Longint;
  LenString: Word;
  Dict: PDictTable;
  HashTable: TList;
  PData: PByte;
  WriteCtxt: TWriteContext;
begin
  LineIdent := ((Header.biWidth * Header.biBitCount + 31) div 32) * 4;
  Tail := 0; HashValue := 0;
  Dict := AllocMem(SizeOf(TDictTable));
  try
    HashTable := TList.Create;
    try
      for I := 0 to HASH_TABLE_SIZE - 1 do
        HashTable.Add(nil);
      { Initialise encoder variables }
      InitCodeSize := Header.biBitCount + 1;
      if InitCodeSize = 2 then
        Inc(InitCodeSize);
      MinCodeSize := InitCodeSize - 1;
      Stream.WriteBuffer(MinCodeSize, 1);
      ClearCode := 1 shl MinCodeSize;
      EndingCode := ClearCode + 1;
      LastCode := EndingCode;
      MaxCode := 1 shl InitCodeSize;
      LenString := 0;
      { Setup write context }
      WriteCtxt.Inx := 0;
      WriteCtxt.CodeSize := InitCodeSize;
      FillChar(WriteCtxt.Buf, SizeOf(WriteCtxt.Buf), 0);
      WriteCode(Stream, ClearCode, WriteCtxt);
      for I := 0 to HASH_TABLE_SIZE - 1 do
        HashTable[I] := nil;
      Data := HugeOffset(Data, (Header.biHeight - 1) * LineIdent);
      Y := 0; Pass := 0;
      if Assigned(ProgressProc) then
        ProgressProc(psStarting, 0, '');
      try
        while (Y < Header.biHeight) do
        begin
          PData := HugeOffset(Data, -(Y * LineIdent));
          for X := 0 to Header.biWidth - 1 do
          begin
            case Header.biBitCount of
              8: begin
                   Col := PData^;
                   PData := HugeOffset(PData, 1);
                 end;
              4: begin
                   if X and 1 <> 0 then
                   begin
                     Col := PData^ and $0F;
                     PData := HugeOffset(PData, 1);
                   end
                   else
                     Col := PData^ shr 4;
                 end;
              else { must be 1 }
                begin
                  if X and 7 = 7 then
                  begin
                    Col := PData^ and 1;
                    PData := HugeOffset(PData, 1);
                  end
                  else
                    Col := (PData^ shr (7 - (X and $07))) and $01;
                end;
            end; { case }
            Inc(LenString);
            if LenString = 1 then
            begin
              Tail := Col;
              HashValue := InitHash(Col);
            end
            else
            begin
              HashValue := HashValue * (Col + LenString + 4);
              I := HashValue mod HASH_TABLE_SIZE;
              HashValue := HashValue mod HASH_TABLE_SIZE;
              while (HashTable[I] <> nil) and
                ((PImageDict(HashTable[I])^.Tail <> Tail) or
                (PImageDict(HashTable[I])^.Col <> Col)) do
              begin
                Inc(I);
                if (I >= HASH_TABLE_SIZE) then
                  I := 0;
              end;
              if (HashTable[I] <> nil) then { Found in the strings table }
                Tail := PImageDict(HashTable[I])^.Index
              else
              begin
                { Not found }
                WriteCode(Stream, Tail, WriteCtxt);
                Inc(LastCode);
                HashTable[I] := @Dict^[LastCode];
                PImageDict(HashTable[I])^.Index := LastCode;
                PImageDict(HashTable[I])^.Tail := Tail;
                PImageDict(HashTable[I])^.Col := Col;
                Tail := Col;
                HashValue := InitHash(Col);
                LenString := 1;
                if (LastCode >= MaxCode) then
                begin
                  { Next Code will be written longer }
                  MaxCode := MaxCode shl 1;
                  Inc(WriteCtxt.CodeSize);
                end
                else
                if (LastCode >= CODE_TABLE_SIZE - 2) then
                begin
                  { Reset tables }
                  WriteCode(Stream, Tail, WriteCtxt);
                  WriteCode(Stream, ClearCode, WriteCtxt);
                  LenString := 0;
                  LastCode := EndingCode;
                  WriteCtxt.CodeSize := InitCodeSize;
                  MaxCode := 1 shl InitCodeSize;
                  for I := 0 to HASH_TABLE_SIZE - 1 do
                    HashTable[I] := nil;
                end;
              end;
            end;
          end; { for X loop }
          if Interlaced then
            Y := InterlaceStep(Y, Header.biHeight, Pass)
          else
            Inc(Y);
          Temp := Trunc(100.0 * (Y / Header.biHeight));
          if Assigned(ProgressProc) then
            ProgressProc(psRunning, Temp, '');
        end; { while Y loop }
        WriteCode(Stream, Tail, WriteCtxt);
        WriteCode(Stream, EndingCode, WriteCtxt);
        FlushCode(Stream, WriteCtxt);
      finally
        if Assigned(ProgressProc) then
        begin
          if ExceptObject = nil then
            ProgressProc(psEnding, 100, '')
          else
            ProgressProc(psEnding, 0, Exception(ExceptObject).Message);
        end;
      end;
    finally
      HashTable.Free;
    end;
  finally
    FreeMem(Dict, SizeOf(TDictTable));
  end;
end;

{ TGIFItem }

destructor TGIFItem.Destroy;
begin
  FImageData.Free;
  inherited Destroy;
end;

procedure TGIFItem.FreeHandle;
begin
  if FImageData <> nil then
    FImageData.SetSize(0);
end;

{ TGIFData }

constructor TGIFData.Create;
begin
  inherited Create;
  FComment := TStringList.Create;
end;

destructor TGIFData.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;

procedure TGIFData.FreeHandle;
begin
  if FComment <> nil then
    FComment.Clear;
end;

{ TGIFFrame }

constructor TGIFFrame.Create(AOwner: TGIFImage);
begin
  FOwner := AOwner;
  inherited Create;
  NewImage;
end;

destructor TGIFFrame.Destroy;
begin
  FBitmap.Free;
  FreeExtensions(FExtensions);
  FImage.Release;
  inherited Destroy;
end;

procedure TGIFFrame.SetAnimateInterval(Value: Word);
begin
  if FAnimateInterval <> Value then
  begin
    FAnimateInterval := Value;
    if Value > 0 then
      FOwner.FVersion := gv89a;
    FOwner.Changed(FOwner);
  end;
end;

procedure TGIFFrame.SetDisposalMethod(Value: TDisposalMethod);
begin
  if FDisposal <> Value then
  begin
    FDisposal := Value;
    if Value <> dmUndefined then
      FOwner.FVersion := gv89a;
    FOwner.Changed(FOwner);
  end;
end;

procedure TGIFFrame.SetTopLeft(const Value: TPoint);
begin
  if (FTopLeft.X <> Value.X) or (FTopLeft.Y <> Value.Y) then
  begin
    FTopLeft.X := Value.X;
    FTopLeft.Y := Value.Y;
    FOwner.FScreenWidth := Max(FOwner.FScreenWidth,
      FImage.FSize.X + FTopLeft.X);
    FOwner.FScreenHeight := Max(FOwner.FScreenHeight,
      FImage.FSize.Y + FTopLeft.Y);
    FOwner.Changed(FOwner);
  end;
end;

procedure TGIFFrame.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    if Value <> clNone then
      FOwner.FVersion := gv89a;
    FOwner.Changed(FOwner);
  end;
end;

function TGIFFrame.GetBitmap: TBitmap;
var
  Mem: TMemoryStream;
begin
  Result := FBitmap;
  if (Result = nil) or Result.Empty then
  begin
    NewBitmap;
    Result := FBitmap;
    if Assigned(FImage.FImageData) then
    try
      Mem := TMemoryStream.Create;
      try
        SaveToBitmapStream(Mem);
        FBitmap.LoadFromStream(Mem);
{$IFDEF RX_D3}
        if not FBitmap.Monochrome then
          FBitmap.HandleType := bmDDB;
{$ENDIF}
      finally
        Mem.Free;
      end;
    except
      raise;
    end;
  end;
end;

function TGIFFrame.GetHeight: Integer;
begin
  if Assigned(FBitmap) or Assigned(FImage.FImageData) then
    Result := Bitmap.Height
  else
    Result := 0;
end;

function TGIFFrame.GetWidth: Integer;
begin
  if Assigned(FBitmap) or Assigned(FImage.FImageData) then
    Result := Bitmap.Width
  else
    Result := 0;
end;

function TGIFFrame.GetColorCount: Integer;
begin
  Result := FImage.FColormap.Count;
  if (Result = 0) and Assigned(FBitmap) and (FBitmap.Palette <> 0) then
    Result := PaletteEntries(FBitmap.Palette);
end;

procedure TGIFFrame.GrayscaleImage(ForceEncoding: Boolean);
var
  Mem: TMemoryStream;
  TransIndex: Integer;
begin
  if not FGrayscale and (Assigned(FBitmap) or
    Assigned(FImage.FImageData)) then
  begin
    if Assigned(FImage.FImageData) and (FImage.FColorMap.Count > 0) then
    begin
      FBitmap.Free;
      FBitmap := nil;
      TransIndex := FindColorIndex(FImage.FColorMap, FTransparentColor);
      GrayColorTable(FImage.FColorMap);
      if TransIndex >= 0 then
        FTransparentColor := ItemToRGB(FImage.FColorMap.Colors[TransIndex])
      else
        FTransparentColor := clNone;
      FGrayscale := True;
      try
        GetBitmap;
      except
        on EAbort do;
        else
          raise;
      end;
    end
    else
    begin
      Mem := BitmapToMemoryStream(Bitmap, pf8bit, mmGrayscale);
      try
        FImage.Release;
        FImage := TGIFItem.Create;
        FImage.Reference;
        if ForceEncoding then EncodeBitmapStream(Mem);
        FGrayscale := True;
        if FTransparentColor <> clNone then
          FTransparentColor := GrayColor(FTransparentColor);
        FBitmap.LoadFromStream(Mem);
      finally
        Mem.Free;
      end;
    end;
  end;
end;

procedure TGIFFrame.Assign(Source: TPersistent);
var
  AComment: TStrings;
begin
  if Source = nil then
  begin
    NewImage;
    FBitmap.Free;
    FBitmap := nil;
  end
  else
  if (Source is TGIFFrame) then
  begin
    if Source <> Self then
    begin
      FImage.Release;
      FImage := TGIFFrame(Source).FImage;
      if TGIFFrame(Source).FOwner <> FOwner then
        FLocalColors := True
      else
        FLocalColors := TGIFFrame(Source).FLocalColors;
      FImage.Reference;
      FTopLeft := TGIFFrame(Source).FTopLeft;
      FInterlaced := TGIFFrame(Source).FInterlaced;
      if TGIFFrame(Source).FBitmap <> nil then
      begin
        NewBitmap;
        FBitmap.Assign(TGIFFrame(Source).FBitmap);
      end;
      FTransparentColor := TGIFFrame(Source).FTransparentColor;
      FAnimateInterval := TGIFFrame(Source).FAnimateInterval;
      FDisposal := TGIFFrame(Source).FDisposal;
      FGrayscale := TGIFFrame(Source).FGrayscale;
      FCorrupted := TGIFFrame(Source).FCorrupted;
      AComment := TGIFFrame(Source).FindComment(False);
      if (AComment <> nil) and (AComment.Count > 0) then
        SetComment(AComment);
    end;
  end
  else
  if Source is TGIFImage then
  begin
    if (TGIFImage(Source).Count > 0) then
    begin
      if (TGIFImage(Source).FrameIndex >= 0) then
        Assign(TGIFImage(Source).Frames[TGIFImage(Source).FrameIndex])
      else
        Assign(TGIFImage(Source).Frames[0]);
    end
    else
      Assign(nil);
  end
  else
  if Source is TGraphic then
  begin
    { TBitmap, TJPEGImage... }
    if TGraphic(Source).Empty then
    begin
      Assign(nil);
      Exit;
    end;
    NewImage;
    NewBitmap;
    try
      FBitmap.Assign(Source);
      if Source is TBitmap then
        FBitmap.Monochrome := TBitmap(Source).Monochrome;
    except
      FBitmap.Canvas.Brush.Color := clFuchsia;
      FBitmap.Width := TGraphic(Source).Width;
      FBitmap.Height := TGraphic(Source).Height;
      FBitmap.Canvas.Draw(0, 0, TGraphic(Source));
    end;
{$IFDEF RX_D3}
    if TGraphic(Source).Transparent then
    begin
      if Source is TBitmap then
        FTransparentColor := TBitmap(Source).TransparentColor
      else FTransparentColor := GetNearestColor(FBitmap.Canvas.Handle,
        ColorToRGB(FBitmap.Canvas.Brush.Color));
    end;
{$ELSE}
    if (Source is TIcon) or (Source is TMetafile) then
      FTransparentColor := GetNearestColor(FBitmap.Canvas.Handle,
        ColorToRGB(FBitmap.Canvas.Brush.Color));
{$ENDIF}
  end
  else
    inherited Assign(Source);
  if FOwner <> nil then
    FOwner.UpdateScreenSize;
end;

procedure TGIFFrame.AssignTo(Dest: TPersistent);
begin
  if (Dest is TGIFFrame) or (Dest is TGIFImage) then
    Dest.Assign(Self)
  else
  if Dest is TGraphic then
  begin
    Dest.Assign(Bitmap);
{$IFDEF RX_D3}
    if (Dest is TBitmap) and (FTransparentColor <> clNone) then
    begin
      TBitmap(Dest).TransparentColor := GetNearestColor(
        TBitmap(Dest).Canvas.Handle, ColorToRGB(FTransparentColor));
      TBitmap(Dest).Transparent := True;
    end;
{$ENDIF}
  end
  else
    inherited AssignTo(Dest);
end;

procedure TGIFFrame.NewBitmap;
begin
  FBitmap.Free;
  FBitmap := TBitmap.Create;
end;

procedure TGIFFrame.NewImage;
begin
  if FImage <> nil then
    FImage.Release;
  FImage := TGIFItem.Create;
  FImage.Reference;
  FGrayscale := False;
  FCorrupted := False;
  FTransparentColor := clNone;
  FTopLeft := Point(0, 0);
  FInterlaced := False;
  FLocalColors := False;
  FAnimateInterval := 0;
  FDisposal := dmUndefined;
end;

function TGIFFrame.FindComment(ForceCreate: Boolean): TStrings;
var
  Ext: TExtension;
begin
  Ext := FindExtension(FExtensions, etComment);
  if (Ext = nil) and ForceCreate then
  begin
    Ext := TExtension.Create;
    try
      Ext.FExtType := etComment;
      if FExtensions = nil then
        FExtensions := TList.Create;
      FExtensions.Add(Ext);
    except
      Ext.Free;
      raise;
    end;
  end;
  if (Ext <> nil) then
  begin
    if (Ext.FData = nil) and ForceCreate then
      Ext.FData := TStringList.Create;
    Result := Ext.FData;
  end
  else
    Result := nil;
end;

function TGIFFrame.GetComment: TStrings;
begin
  Result := FindComment(True);
end;

procedure TGIFFrame.SetComment(Value: TStrings);
begin
  GetComment.Assign(Value);
end;

procedure TGIFFrame.UpdateExtensions;
var
  Ext: TExtension;
  I: Integer;
begin
  Ext := FindExtension(FExtensions, etGraphic);
  if (FAnimateInterval > 0) or (FTransparentColor <> clNone) or
    (FDisposal <> dmUndefined) then
  begin
    if Ext = nil then
    begin
      Ext := TExtension.Create;
      Ext.FExtType := etGraphic;
      if FExtensions = nil then FExtensions := TList.Create;
      FExtensions.Add(Ext);
      with Ext.FExtRec.GCE do
      begin
        BlockSize := 4;
        PackedFields := 0;
        Terminator := 0;
      end;
    end;
  end;
  if Ext <> nil then
    with Ext.FExtRec.GCE do
    begin
      DelayTime := FAnimateInterval div 10;
      I := FindColorIndex(FImage.FColorMap, FTransparentColor);
      if I >= 0 then
      begin
        TransparentColorIndex := I;
        PackedFields := PackedFields or GCE_TRANSPARENT;
      end
      else
        PackedFields := PackedFields and not GCE_TRANSPARENT;
      PackedFields := (PackedFields and not GCE_DISPOSAL_METHOD) or
        (Ord(FDisposal) shl 2);
    end;
  if FExtensions <> nil then
    for I := FExtensions.Count - 1 downto 0 do
    begin
      Ext := TExtension(FExtensions[I]);
      if (Ext <> nil) and (Ext.FExtType = etComment) and
        ((Ext.FData = nil) or (Ext.FData.Count = 0)) then
      begin
        Ext.Free;
        FExtensions.Delete(I);
      end;
    end;
  if (FExtensions <> nil) and (FExtensions.Count > 0) then
    FOwner.FVersion := gv89a;
end;

procedure TGIFFrame.EncodeBitmapStream(Stream: TMemoryStream);
var
  BI: PBitmapInfoHeader;
  ColorCount, W, H: Integer;
  Bits, Pal: Pointer;
begin
  ColorCount := 0;
  Stream.Position := 0;
  BI := PBitmapInfoHeader(Longint(Stream.Memory) + SizeOf(TBitmapFileHeader));
  W := BI^.biWidth; H := BI^.biHeight;
  Pal := PRGBPalette(Longint(BI) + SizeOf(TBitmapInfoHeader));
  Bits := Pointer(Longword(Stream.Memory) + PBitmapFileHeader(Stream.Memory)^.bfOffBits);
  case BI^.biBitCount of
    1: ColorCount := 2;
    4: ColorCount := 16;
    8: ColorCount := 256;
  else
    GifError(LoadStr(SGIFEncodeError));
  end;
  FInterlaced := False;
  FillColorTable(FImage.FColorMap, PRGBPalette(Pal)^, ColorCount);
  if FImage.FImageData = nil then
    FImage.FImageData := TMemoryStream.Create
  else
    FImage.FImageData.SetSize(0);
  try
    WriteGIFData(FImage.FImageData, BI^, FInterlaced, Bits, FOwner.DoProgress);
  except
    on EAbort do
    begin
      NewImage; { OnProgress can raise EAbort to cancel image save }
      raise;
    end
    else
      raise;
  end;
  FImage.FBitsPerPixel := 1;
  while FImage.FColorMap.Count > 1 shl FImage.FBitsPerPixel do
    Inc(FImage.FBitsPerPixel);
  if FOwner.FImage.FColorMap.Count = 0 then
  begin
    FOwner.FImage.FColorMap := FImage.FColorMap;
    FOwner.FImage.FBitsPerPixel := FImage.FBitsPerPixel;
    FLocalColors := False;
  end
  else
    FLocalColors := True;
  FImage.FSize.X := W; FImage.FSize.Y := H;
  FOwner.FScreenWidth := Max(FOwner.FScreenWidth, FImage.FSize.X + FTopLeft.X);
  FOwner.FScreenHeight := Max(FOwner.FScreenHeight, FImage.FSize.Y + FTopLeft.Y);
end;

procedure TGIFFrame.EncodeRasterData;
var
  Method: TMappingMethod;
  Mem: TMemoryStream;
begin
  if not Assigned(FBitmap) or FBitmap.Empty then
    GifError(LoadStr(SNoGIFData));
  if not (GetBitmapPixelFormat(FBitmap) in [pf1bit, pf4bit, pf8bit]) then
  begin
    if FGrayscale then
      Method := mmGrayscale
    else
      Method := DefaultMappingMethod;
    Mem := BitmapToMemoryStream(FBitmap, pf8bit, Method);
    if (Method = mmGrayscale) then
      FGrayscale := True;
  end
  else
    Mem := TMemoryStream.Create;
  try
    if Mem.Size = 0 then
      FBitmap.SaveToStream(Mem);
    EncodeBitmapStream(Mem);
  finally
    Mem.Free;
  end;
end;

procedure TGIFFrame.WriteImageDescriptor(Stream: TStream);
var
  ImageDesc: TImageDescriptor;
begin
  with ImageDesc do
  begin
    PackedFields := 0;
    if FLocalColors then
    begin
      FImage.FBitsPerPixel := 1;
      while FImage.FColorMap.Count > 1 shl FImage.FBitsPerPixel do
        Inc(FImage.FBitsPerPixel);
      PackedFields := (PackedFields or ID_LOCAL_COLOR_TABLE) +
        (FImage.FBitsPerPixel - 1);
    end;
    if FInterlaced then
      PackedFields := PackedFields or ID_INTERLACED;
    ImageLeftPos := FTopLeft.X;
    ImageTopPos := FTopLeft.Y;
    ImageWidth := FImage.FSize.X;
    ImageHeight := FImage.FSize.Y;
  end;
  Stream.Write(ImageDesc, SizeOf(TImageDescriptor));
end;

procedure TGIFFrame.WriteLocalColorMap(Stream: TStream);
begin
  if FLocalColors then
    with FImage.FColorMap do
      Stream.Write(Colors[0], Count * SizeOf(TGIFColorItem));
end;

procedure TGIFFrame.WriteRasterData(Stream: TStream);
begin
  Stream.WriteBuffer(FImage.FImageData.Memory^, FImage.FImageData.Size);
end;

procedure TGIFFrame.SaveToBitmapStream(Stream: TMemoryStream);

  function ConvertBitsPerPixel: TPixelFormat;
  begin
    Result := pfDevice;
    case FImage.FBitsPerPixel of
      1: Result := pf1bit;
      2..4: Result := pf4bit;
      5..8: Result := pf8bit;
    else
      GifError(LoadStr(SWrongGIFColors));
    end;
  end;

var
  HeaderSize: Longword;
  Length: Longword;
  BI: TBitmapInfoHeader;
  BitFile: TBitmapFileHeader;
  Colors: TRGBPalette;
  Bits: Pointer;
  Corrupt: Boolean;
begin
  with BI do
  begin
    biSize := Sizeof(TBitmapInfoHeader);
    biWidth := FImage.FSize.X;
    biHeight := FImage.FSize.Y;
    biPlanes := 1;
    biBitCount := 0;
    case ConvertBitsPerPixel of
      pf1bit: biBitCount := 1;
      pf4bit: biBitCount := 4;
      pf8bit: biBitCount := 8;
    end;
    biCompression := BI_RGB;
    biSizeImage := (((biWidth * biBitCount + 31) div 32) * 4) * biHeight;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  HeaderSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) +
    SizeOf(TRGBQuad) * (1 shl BI.biBitCount);
  Length := HeaderSize + BI.biSizeImage;
  Stream.SetSize(0);
  Stream.Position := 0;
  with BitFile do
  begin
    bfType := $4D42; { BM }
    bfSize := Length;
    bfOffBits := HeaderSize;
  end;
  Stream.Write(BitFile, SizeOf(TBitmapFileHeader));
  Stream.Write(BI, SizeOf(TBitmapInfoHeader));
  FillRGBPalette(FImage.FColorMap, Colors);
  Stream.Write(Colors, SizeOf(TRGBQuad) * (1 shl BI.biBitCount));
  Bits := AllocMemo(BI.biSizeImage);
  try
    ZeroMemory(Bits, BI.biSizeImage);
    FImage.FImageData.Position := 0;
    ReadGIFData(FImage.FImageData, BI, FInterlaced, GIFLoadCorrupted,
      FImage.FBitsPerPixel, Bits, Corrupt, FOwner.DoProgress);
    FCorrupted := FCorrupted or Corrupt;
    Stream.WriteBuffer(Bits^, BI.biSizeImage);
  finally
    FreeMemo(Bits);
  end;
  Stream.Position := 0;
end;

procedure TGIFFrame.LoadFromStream(Stream: TStream);
var
  ImageDesc: TImageDescriptor;
  I, TransIndex: Integer;
begin
  FImage.FImageData := TMemoryStream.Create;
  try
    ReadImageStream(Stream, FImage.FImageData, ImageDesc, FInterlaced,
      FLocalColors, FCorrupted, FImage.FBitsPerPixel, FImage.FColorMap);
    if FCorrupted and not GIFLoadCorrupted then
      GifError(ResStr(SReadError));
    FImage.FImageData.Position := 0;
    with ImageDesc do
    begin
      if ImageHeight = 0 then
        ImageHeight := FOwner.FScreenHeight;
      if ImageWidth = 0 then
        ImageWidth := FOwner.FScreenWidth;
      FTopLeft := Point(ImageLeftPos, ImageTopPos);
      FImage.FSize := Point(ImageWidth, ImageHeight);
      FImage.FPackedFields := PackedFields;
    end;
    if not FLocalColors then
      FImage.FColorMap := FOwner.FImage.FColorMap;
    FAnimateInterval := 0;
    if FExtensions <> nil then
    begin
      for I := 0 to FExtensions.Count - 1 do
        with TExtension(FExtensions[I]) do
          if FExtType = etGraphic then
          begin
            if (FExtRec.GCE.PackedFields and GCE_TRANSPARENT) <> 0 then
            begin
              TransIndex := FExtRec.GCE.TransparentColorIndex;
              if FImage.FColorMap.Count > TransIndex then
                FTransparentColor := ItemToRGB(FImage.FColorMap.Colors[TransIndex]);
            end
            else
              FTransparentColor := clNone;
            FAnimateInterval := Max(FExtRec.GCE.DelayTime * 10,
              FAnimateInterval);
            FDisposal := TDisposalMethod((FExtRec.GCE.PackedFields and
              GCE_DISPOSAL_METHOD) shr 2);
          end;
    end;
  except
    FImage.FImageData.Free;
    FImage.FImageData := nil;
    raise;
  end;
end;

procedure TGIFFrame.Draw(ACanvas: TCanvas; const ARect: TRect;
  Transparent: Boolean);
begin
  if (FTransparentColor <> clNone) and Transparent then
  begin
    with ARect do
      StretchBitmapRectTransparent(ACanvas, Left, Top, Right - Left,
        Bottom - Top, Bounds(0, 0, Bitmap.Width, Bitmap.Height), Bitmap,
        FTransparentColor);
  end
  else
    ACanvas.StretchDraw(ARect, Bitmap);
end;

{ TGIFImage }

constructor TGIFImage.Create;
begin
  inherited Create;
  NewImage;
{$IFDEF RX_D3}
  inherited SetTransparent(True);
{$ENDIF}
end;

destructor TGIFImage.Destroy;
begin
  OnChange := nil;
  FImage.Release;
  ClearItems;
  FItems.Free;
  inherited Destroy;
end;

procedure TGIFImage.Clear;
begin
  Assign(nil);
end;

procedure TGIFImage.ClearItems;
begin
  if FItems <> nil then
    while FItems.Count > 0 do
    begin
      TObject(FItems[0]).Free;
      FItems.Delete(0);
    end;
end;

procedure TGIFImage.Assign(Source: TPersistent);
var
  I: Integer;
  AFrame: TGIFFrame;
begin
  if (Source = nil) then
  begin
    NewImage;
    Changed(Self);
  end
  else
  if (Source is TGIFImage) and (Source <> Self) then
  begin
    FImage.Release;
    FImage := TGIFImage(Source).FImage;
    FImage.Reference;
    FVersion := TGIFImage(Source).FVersion;
    FBackgroundColor := TGIFImage(Source).FBackgroundColor;
    FRepeatCount := TGIFImage(Source).FRepeatCount;
    FLooping := TGIFImage(Source).FLooping;
    FCorrupted := TGIFImage(Source).FCorrupted;
    if FItems = nil then
      FItems := TList.Create
    else
      ClearItems;
    with TGIFImage(Source) do
    begin
      for I := 0 to FItems.Count - 1 do
      begin
        AFrame := TGIFFrame.Create(Self);
        try
          AFrame.FImage.FBitsPerPixel :=
            TGIFFrame(FItems[I]).FImage.FBitsPerPixel;
          AFrame.Assign(TGIFFrame(FItems[I]));
          AFrame.FLocalColors := TGIFFrame(FItems[I]).FLocalColors;
          Self.FItems.Add(AFrame);
        except
          AFrame.Free;
          raise;
        end;
      end;
      Self.FScreenWidth := FScreenWidth;
      Self.FScreenHeight := FScreenHeight;
    end;
    FFrameIndex := TGIFImage(Source).FFrameIndex;
    Changed(Self);
  end
  else
  if Source is TGIFFrame then
  begin
    NewImage;
    with TGIFFrame(Source).FOwner.FImage do
    begin
      FImage.FAspectRatio := FAspectRatio;
      FImage.FBitsPerPixel := FBitsPerPixel;
      FImage.FColorResBits := FColorResBits;
      Move(FColorMap, FImage.FColorMap, SizeOf(FColorMap));
    end;
    FFrameIndex := FItems.Add(TGIFFrame.Create(Self));
    TGIFFrame(FItems[FFrameIndex]).Assign(Source);
    if FVersion = gvUnknown then
      FVersion := gv87a;
    Changed(Self);
  end
  else
  if Source is TBitmap then
  begin
    NewImage;
    AddFrame(TBitmap(Source));
    Changed(Self);
  end
  else
  if Source is TAnimatedCursorImage then
  begin
    NewImage;
    FBackgroundColor := clWindow;
    with TAnimatedCursorImage(Source) do
    begin
      for I := 0 to IconCount - 1 do
      begin
        AddFrame(TIcon(Icons[I]));
        Self.Frames[FrameIndex].FAnimateInterval :=
          Longint(Frames[I].JiffRate * 100) div 6;
      end;
    end;
    Changed(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TGIFImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TGIFImage then
    Dest.Assign(Self)
  else
  if Dest is TGraphic then
  begin
    if Empty then
      Dest.Assign(nil)
    else
    if FFrameIndex >= 0 then
      TGIFFrame(FItems[FFrameIndex]).AssignTo(Dest)
    else
      Dest.Assign(Bitmap);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TGIFImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FFrameIndex >= 0 then
    TGIFFrame(FItems[FFrameIndex]).Draw(ACanvas, ARect, Self.Transparent);
end;

function TGIFImage.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

procedure TGIFImage.SetBackgroundColor(Value: TColor);
begin
  if Value <> FBackgroundColor then
  begin
    FBackgroundColor := Value;
    Changed(Self);
  end;
end;

procedure TGIFImage.SetLooping(Value: Boolean);
begin
  if Value <> FLooping then
  begin
    FLooping := Value;
    Changed(Self);
  end;
end;

procedure TGIFImage.SetRepeatCount(Value: Word);
begin
  if Min(Value, MAX_LOOP_COUNT) <> FRepeatCount then
  begin
    FRepeatCount := Min(Value, MAX_LOOP_COUNT);
    Changed(Self);
  end;
end;

function TGIFImage.GetPixelFormat: TPixelFormat;
var
  I: Integer;
begin
  Result := pfDevice;
  if not Empty then
  begin
    Result := ColorsToPixelFormat(FImage.FColorMap.Count);
    for I := 0 to FItems.Count - 1 do
    begin
      if (Frames[I].FImage.FImageData = nil) or
        (Frames[I].FImage.FImageData.Size = 0) then
      begin
        if Assigned(Frames[I].FBitmap) then
          Result := TPixelFormat(Max(Ord(Result),
            Ord(GetBitmapPixelFormat(Frames[I].FBitmap))))
        else
          Result := TPixelFormat(Max(Ord(Result), Ord(pfDevice)));
      end
      else
      if Frames[I].FLocalColors then
        Result := TPixelFormat(Max(Ord(Result),
          Ord(ColorsToPixelFormat(Frames[I].FImage.FColorMap.Count))));
    end;
  end;
end;

function TGIFImage.GetCorrupted: Boolean;
var
  I: Integer;
begin
  Result := FCorrupted;
  if not Result then
    for I := 0 to FItems.Count - 1 do
      if Frames[I].Corrupted then
      begin
        Result := True;
        Exit;
      end;
end;

function TGIFImage.GetTransparentColor: TColor;
begin
  if (FItems.Count > 0) and (FFrameIndex >= 0) then
    Result := TGIFFrame(FItems[FFrameIndex]).FTransparentColor
  else
    Result := clNone;
end;

function TGIFImage.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGIFImage.GetFrame(Index: Integer): TGIFFrame;
begin
  Result := TGIFFrame(FItems[Index]);
end;

procedure TGIFImage.SetFrameIndex(Value: Integer);
begin
  Value := Min(FItems.Count - 1, Max(-1, Value));
  if FFrameIndex <> Value then
  begin
    FFrameIndex := Value;
{$IFDEF RX_D3}
    PaletteModified := True;
{$ENDIF}
    Changed(Self);
  end;
end;

function TGIFImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic is TGIFImage) and
    (FImage = TGIFImage(Graphic).FImage);
end;

function TGIFImage.GetBitmap: TBitmap;
var
  Bmp: TBitmap;
begin
  if (FItems.Count > 0) then
  begin
    if (FFrameIndex >= 0) and (FFrameIndex < FItems.Count) then
      Result := TGIFFrame(FItems[FFrameIndex]).Bitmap
    else
      Result := TGIFFrame(FItems[0]).Bitmap
  end
  else
  begin
    FFrameIndex := 0;
    Bmp := TBitmap.Create;
    try
      Bmp.Handle := 0;
      Assign(Bmp);
      Result := TGIFFrame(FItems[FFrameIndex]).Bitmap;
    finally
      Bmp.Free;
    end;
  end;
end;

function TGIFImage.GetGlobalColorCount: Integer;
begin
  Result := FImage.FColormap.Count;
end;

function TGIFImage.GetEmpty: Boolean;
var
  I: Integer;
begin
  I := Max(FFrameIndex, 0);
  Result := (FItems.Count = 0) or
    ((TGIFFrame(FItems[I]).FBitmap = nil) and
    ((TGIFFrame(FItems[I]).FImage.FImageData = nil) or
    (TGIFFrame(FItems[I]).FImage.FImageData.Size = 0)));
end;

function TGIFImage.GetPalette: HPalette;
begin
  if FItems.Count > 0 then
    Result := Bitmap.Palette
  else
    Result := 0;
end;

function TGIFImage.GetTransparent: Boolean;
var
  I: Integer;
begin
{$IFDEF RX_D3}
  if inherited GetTransparent then
{$ENDIF}
    for I := 0 to FItems.Count - 1 do
      if Frames[I].TransparentColor <> clNone then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TGIFImage.GetHeight: Integer;
begin
  if not Empty and (FFrameIndex >= 0) and (FFrameIndex < Count) then
    Result := TGIFFrame(FItems[FFrameIndex]).Bitmap.Height
  else
    Result := 0;
end;

function TGIFImage.GetWidth: Integer;
begin
  if not Empty and (FFrameIndex >= 0) and (FFrameIndex < Count) then
    Result := TGIFFrame(FItems[FFrameIndex]).Bitmap.Width
  else
    Result := 0;
end;

function TGIFImage.GetScreenWidth: Integer;
begin
  if Empty then
    Result := 0
  else
    Result := FScreenWidth;
end;

function TGIFImage.GetScreenHeight: Integer;
begin
  if Empty then
    Result := 0
  else
    Result := FScreenHeight;
end;

procedure TGIFImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Bmp: TBitmap;
  Stream: TMemoryStream;
  Size: Longint;
  Buffer: Pointer;
  Data: THandle;
begin
  { !! check for gif clipboard Data, mime type image/gif }
  Data := GetClipboardData(CF_GIF);
  if Data <> 0 then
  begin
    Buffer := GlobalLock(Data);
    try
      Stream := TMemoryStream.Create;
      try
        Stream.Write(Buffer^, GlobalSize(Data));
        Stream.Position := 0;
        Stream.Read(Size, SizeOf(Size));
        ReadStream(Size, Stream, False);
        if Count > 0 then
        begin
          FFrameIndex := 0;
          AData := GetClipboardData(CF_BITMAP);
          if AData <> 0 then
          begin
            Frames[0].NewBitmap;
            Frames[0].FBitmap.LoadFromClipboardFormat(CF_BITMAP, AData, APalette);
          end;
        end;
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(Data);
    end;
  end
  else
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
      Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TGIFImage.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream.Size - Stream.Position, Stream, True);
end;

procedure TGIFImage.LoadFromResourceName(Instance: THandle; const ResName: string;
  ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  try
    ReadStream(Stream.Size - Stream.Position, Stream, True);
  finally
    Stream.Free;
  end;
end;

procedure TGIFImage.LoadFromResourceID(Instance: THandle; ResID: Integer;
  ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, ResType);
  try
    ReadStream(Stream.Size - Stream.Position, Stream, True);
  finally
    Stream.Free;
  end;
end;

procedure TGIFImage.UpdateScreenSize;
var
  I: Integer;
begin
  FScreenWidth := 0;
  FScreenHeight := 0;
  for I := 0 to FItems.Count - 1 do
    if Frames[I] <> nil then
    begin
      FScreenWidth := Max(FScreenWidth, Frames[I].Width + Frames[I].FTopLeft.X);
      FScreenHeight := Max(FScreenHeight, Frames[I].Height + Frames[I].FTopLeft.Y);
    end;
end;

function TGIFImage.AddFrame(Value: TGraphic): Integer;
begin
  FFrameIndex := FItems.Add(TGIFFrame.Create(Self));
  TGIFFrame(FItems[FFrameIndex]).Assign(Value);
  if FVersion = gvUnknown then
    FVersion := gv87a;
  if FItems.Count > 1 then
    FVersion := gv89a;
  Result := FFrameIndex;
end;

procedure TGIFImage.DeleteFrame(Index: Integer);
begin
  Frames[Index].Free;
  FItems.Delete(Index);
  UpdateScreenSize;
  if FFrameIndex >= FItems.Count then
    Dec(FFrameIndex);
  Changed(Self);
end;

procedure TGIFImage.MoveFrame(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
  FFrameIndex := NewIndex;
  Changed(Self);
end;

procedure TGIFImage.NewImage;
begin
  if FImage <> nil then
    FImage.Release;
  FImage := TGIFData.Create;
  FImage.Reference;
  if FItems = nil then
    FItems := TList.Create;
  ClearItems;
  FCorrupted := False;
  FFrameIndex := -1;
  FBackgroundColor := clNone;
  FRepeatCount := 1;
  FLooping := False;
  FVersion := gvUnknown;
end;

procedure TGIFImage.UniqueImage;
var
  Temp: TGIFData;
begin
  if FImage = nil then
    NewImage
  else
  if FImage.RefCount > 1 then
  begin
    Temp := TGIFData.Create;
    with Temp do
    try
      FComment.Assign(FImage.FComment);
      FAspectRatio := FImage.FAspectRatio;
      FBitsPerPixel := FImage.FBitsPerPixel;
      FColorResBits := FImage.FColorResBits;
      FColorMap := FImage.FColorMap;
    except
      Temp.Free;
      raise;
    end;
    FImage.Release;
    FImage := Temp;
    FImage.Reference;
  end;
end;

function TGIFImage.GetComment: TStrings;
begin
  Result := FImage.FComment;
end;

procedure TGIFImage.SetComment(Value: TStrings);
begin
  UniqueImage;
  FImage.FComment.Assign(Value);
end;

procedure TGIFImage.DecodeAllFrames;
var
  FrameNo, I: Integer;
begin
  for FrameNo := 0 to FItems.Count - 1 do
    try
      TGIFFrame(FItems[FrameNo]).GetBitmap;
    except
      on EAbort do begin { OnProgress can raise EAbort to cancel image load }
        for I := FItems.Count - 1 downto FrameNo do
        begin
          TObject(FItems[I]).Free;
          FItems.Delete(I);
        end;
        FCorrupted := True;
        Break;
      end;
      else
        raise;
    end;
end;

procedure TGIFImage.EncodeFrames(ReverseDecode: Boolean);
var
  FrameNo: Integer;
begin
  for FrameNo := 0 to FItems.Count - 1 do
    with TGIFFrame(FItems[FrameNo]) do
    begin
      if (FImage.FImageData = nil) or (FImage.FImageData.Size = 0) then
      begin
        FImage.FImageData.Free;
        FImage.FImageData := nil;
        EncodeRasterData;
        if ReverseDecode and (FBitmap.Palette = 0) then
        begin
          FBitmap.Free;
          FBitmap := nil;
          try
            GetBitmap;
          except
            on EAbort do; { OnProgress can raise EAbort to cancel encoding }
            else
              raise;
          end;
        end;
      end;
      UpdateExtensions;
    end;
end;

procedure TGIFImage.EncodeAllFrames;
begin
  EncodeFrames(True);
end;

procedure TGIFImage.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  ReadStream(Size, Stream, True);
end;

procedure TGIFImage.ReadSignature(Stream: TStream);
var
  I: TGIFVersion;
  S: AnsiString;
begin
  FVersion := gvUnknown;
  SetLength(S, 3);
  Stream.Read(S[1], 3);
  if CompareText(GIFSignature, string(S)) <> 0 then
    GifError(LoadStr(SGIFVersion));
  SetLength(S, 3);
  Stream.Read(S[1], 3);
  for I := Low(TGIFVersion) to High(TGIFVersion) do
    if CompareText(string(S), string(StrPas(GIFVersionStr[I]))) = 0 then
    begin
      FVersion := I;
      Break;
    end;
  if FVersion = gvUnknown then
    GifError(LoadStr(SGIFVersion));
end;

procedure TGIFImage.ReadStream(Size: Longint; Stream: TStream;
  ForceDecode: Boolean);
var
  SeparatorChar: AnsiChar;
  NewItem: TGIFFrame;
  Extensions: TList;
  ScreenDesc: TScreenDescriptor;
  Data: TMemoryStream;

  procedure ReadScreenDescriptor(Stream: TStream);
  begin
    Stream.Read(ScreenDesc, SizeOf(ScreenDesc));
    FScreenWidth := ScreenDesc.ScreenWidth;
    FScreenHeight := ScreenDesc.ScreenHeight;
    with FImage do
    begin
      FAspectRatio := ScreenDesc.AspectRatio;
      FBitsPerPixel := 1 + (ScreenDesc.PackedFields and LSD_COLOR_TABLE_SIZE);
      FColorResBits := 1 + (ScreenDesc.PackedFields and LSD_COLOR_RESOLUTION) shr 4;
    end;
  end;

  procedure ReadGlobalColorMap(Stream: TStream);
  begin
    if (ScreenDesc.PackedFields and LSD_GLOBAL_COLOR_TABLE) <> 0 then
      with FImage.FColorMap do
      begin
        Count := 1 shl FImage.FBitsPerPixel;
        Stream.Read(Colors[0], Count * SizeOf(TGIFColorItem));
        if Count > ScreenDesc.BackgroundColorIndex then
          FBackgroundColor := ItemToRGB(Colors[ScreenDesc.BackgroundColorIndex]);
      end;
  end;

  function ReadDataBlock(Stream: TStream): TStrings;
  var
    BlockSize: Byte;
    S: string;
  begin
    Result := TStringlist.Create;
    try
      repeat
        Stream.Read(BlockSize, SizeOf(Byte));
        if BlockSize <> 0 then
        begin
          SetLength(S, BlockSize);
          Stream.Read(S[1], BlockSize);
          Result.Add(S);
        end;
      until (BlockSize = 0) or (Stream.Position >= Stream.Size);
    except
      Result.Free;
      raise;
    end;
  end;

  function ReadExtension(Stream: TStream): TExtension;
  var
    ExtensionLabel: Byte;
  begin
    Result := TExtension.Create;
    try
      Stream.Read(ExtensionLabel, SizeOf(Byte));
      with Result do
        if ExtensionLabel = ExtLabels[etGraphic] then
        begin
          { graphic control extension }
          FExtType := etGraphic;
          Stream.Read(FExtRec.GCE, SizeOf(TGraphicControlExtension));
        end
        else
        if ExtensionLabel = ExtLabels[etComment] then
        begin
          { comment extension }
          FExtType := etComment;
          FData := ReadDataBlock(Stream);
        end
        else
        if ExtensionLabel = ExtLabels[etPlainText] then
        begin
          { plain text extension }
          FExtType := etPlainText;
          Stream.Read(FExtRec.PTE, SizeOf(TPlainTextExtension));
          FData := ReadDataBlock(Stream);
        end
        else
        if ExtensionLabel = ExtLabels[etApplication] then
        begin
          { application extension }
          FExtType := etApplication;
          Stream.Read(FExtRec.APPE, SizeOf(TAppExtension));
          FData := ReadDataBlock(Stream);
        end
        else
          GifError(Format(LoadStr(SUnrecognizedGIFExt), [ExtensionLabel]));
    except
      Result.Free;
      raise;
    end;
  end;

  function ReadSeparator(Stream: TStream): AnsiChar;
  begin
    Result := #0;
    while (Stream.Size > Stream.Position) and (Result = #0) do
      Stream.Read(Result, SizeOf(Result));
  end;

  function ReadExtensionBlock(Stream: TStream; var SeparatorChar: AnsiChar): TList;
  var
    NewExt: TExtension;
  begin
    Result := nil;
    try
      while SeparatorChar = CHR_EXT_INTRODUCER do
      begin
        NewExt := ReadExtension(Stream);
        if (NewExt.FExtType = etPlainText) then
        begin
          { plain text data blocks are not supported,
            clear all previous readed extensions }
          FreeExtensions(Result);
          Result := nil;
        end;
        if (NewExt.FExtType in [etPlainText, etApplication]) then
        begin
          { check for loop extension }
          if NewExt.IsLoopExtension then
          begin
            FLooping := True;
            FRepeatCount := Min(MakeWord(Byte(NewExt.FData[0][2]),
              Byte(NewExt.FData[0][3])), MAX_LOOP_COUNT);
          end;
          { not supported yet, must be ignored }
          NewExt.Free;
        end
        else
        begin
          if Result = nil then
            Result := TList.Create;
          Result.Add(NewExt);
        end;
        if Stream.Size > Stream.Position then
          SeparatorChar := ReadSeparator(Stream)
        else
          SeparatorChar := CHR_TRAILER;
      end;
      if (Result <> nil) and (Result.Count = 0) then
      begin
        Result.Free;
        Result := nil;
      end;
    except
      if Result <> nil then
        Result.Free;
      raise;
    end;
  end;

var
  I: Integer;
  Ext: TExtension;
begin
  NewImage;
  with FImage do
  begin
    Data := TMemoryStream.Create;
    try
      TMemoryStream(Data).SetSize(Size);
      Stream.ReadBuffer(Data.Memory^, Size);
      if Size > 0 then
      begin
        Data.Position := 0;
        ReadSignature(Data);
        ReadScreenDescriptor(Data);
        ReadGlobalColorMap(Data);
        SeparatorChar := ReadSeparator(Data);
        while not CharInSet(SeparatorChar, [CHR_TRAILER, #0]) and not
          (Data.Position >= Data.Size) do
        begin
          Extensions := ReadExtensionBlock(Data, SeparatorChar);
          if SeparatorChar = CHR_IMAGE_SEPARATOR then
            try
              NewItem := TGIFFrame.Create(Self);
              try
                if FImage.FColorMap.Count > 0 then
                  NewItem.FImage.FBitsPerPixel :=
                    ColorsToBits(FImage.FColorMap.Count);
                NewItem.FExtensions := Extensions;
                Extensions := nil;
                NewItem.LoadFromStream(Data);
                FItems.Add(NewItem);
              except
                NewItem.Free;
                raise;
              end;
              if not (Data.Position >= Data.Size) then
                SeparatorChar := ReadSeparator(Data)
              else
                SeparatorChar := CHR_TRAILER;
              if not CharInSet(SeparatorChar, [CHR_EXT_INTRODUCER,
                CHR_IMAGE_SEPARATOR, CHR_TRAILER]) then
              begin
                SeparatorChar := #0;
                {GifError(LoadStr(SGIFDecodeError));}
              end;
            except
              FreeExtensions(Extensions);
              raise;
            end
          else
          if (FComment.Count = 0) and (Extensions <> nil) then begin
            try
              { trailig extensions }
              for I := 0 to Extensions.Count - 1 do
              begin
                Ext := TExtension(Extensions[I]);
                if (Ext <> nil) and (Ext.FExtType = etComment) then
                begin
                  if FComment.Count > 0 then
                    FComment.Add(#13#10#13#10);
                  FComment.AddStrings(Ext.FData);
                end;
              end;
            finally
              FreeExtensions(Extensions);
            end;
          end
          else if not CharInSet(SeparatorChar, [CHR_TRAILER, #0]) then
            GifError(ResStr(SReadError));
        end;
      end;
    finally
      Data.Free;
    end;
  end;
  if Count > 0 then
  begin
    FFrameIndex := 0;
    if ForceDecode then
    try
      GetBitmap; { force bitmap creation }
    except
      Frames[0].Free;
      FItems.Delete(0);
      raise;
    end;
  end;
{$IFDEF RX_D3}
  PaletteModified := True;
{$ENDIF}
  Changed(Self);
end;

procedure TGIFImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
var
  Stream: TMemoryStream;
  Data: THandle;
  Buffer: Pointer;
  I: Integer;
begin
  { !! check for gif clipboard format, mime type image/gif }
  if FItems.Count = 0 then Exit;
  Frames[0].Bitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  for I := 0 to FItems.Count - 1 do
    with Frames[I] do
      if (FImage.FImageData = nil) or (FImage.FImageData.Size = 0) then
        Exit;
  Stream := TMemoryStream.Create;
  try
    WriteStream(Stream, True);
    Stream.Position := 0;
    Data := GlobalAlloc(HeapAllocFlags, Stream.Size);
    try
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          Stream.Read(Buffer^, Stream.Size);
          SetClipboardData(CF_GIF, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TGIFImage.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TGIFImage.SetHeight(Value: Integer);
begin
  GifError(LoadStr(SChangeGIFSize));
end;

procedure TGIFImage.SetWidth(Value: Integer);
begin
  GifError(LoadStr(SChangeGIFSize));
end;

procedure TGIFImage.WriteStream(Stream: TStream; WriteSize: Boolean);
var
  Separator: AnsiChar;
  Temp: Byte;
  FrameNo: Integer;
  Frame: TGIFFrame;
  Mem: TMemoryStream;
  Size: Longint;
  StrList: TStringList;

  procedure WriteSignature(Stream: TStream);
  var
    Header: TGIFHeader;
  begin
    Header.Signature := GIFSignature;
    Move(GIFVersionStr[FVersion][0], Header.Version[0], 3);
    Stream.Write(Header, SizeOf(TGIFHeader));
  end;

  procedure WriteScreenDescriptor(Stream: TStream);
  var
    ColorResBits: Byte;
    ScreenDesc: TScreenDescriptor;
    I: Integer;
  begin
    UpdateScreenSize;
    with ScreenDesc do
    begin
      ScreenWidth := Self.FScreenWidth;
      ScreenHeight := Self.FScreenHeight;
      AspectRatio := FImage.FAspectRatio;
      PackedFields := 0;
      BackgroundColorIndex := 0;
      if FImage.FColorMap.Count > 0 then
      begin
        PackedFields := PackedFields or LSD_GLOBAL_COLOR_TABLE;
        ColorResBits := ColorsToBits(FImage.FColorMap.Count);
        if FBackgroundColor <> clNone then
          for I := 0 to FImage.FColorMap.Count - 1 do
            if ColorToRGB(FBackgroundColor) =
              ItemToRGB(FImage.FColorMap.Colors[I]) then
            begin
              BackgroundColorIndex := I;
              Break;
            end;
        PackedFields := PackedFields + ((ColorResBits - 1) shl 4) +
          (FImage.FBitsPerPixel - 1);
      end;
    end;
    Stream.Write(ScreenDesc, SizeOf(ScreenDesc));
  end;

  procedure WriteDataBlock(Stream: TStream; Data: TStrings);
  var
    I: Integer;
    S: string;
    BlockSize: Byte;
  begin
    for I := 0 to Data.Count - 1 do
    begin
      S := Data[I];
      BlockSize := Min(Length(S), 255);
      if BlockSize > 0 then
      begin
        Stream.Write(BlockSize, SizeOf(Byte));
        Stream.Write(S[1], BlockSize);
      end;
    end;
    BlockSize := 0;
    Stream.Write(BlockSize, SizeOf(Byte));
  end;

  procedure WriteExtensionBlock(Stream: TStream; Extensions: TList);
  var
    I: Integer;
    Ext: TExtension;
    ExtensionLabel: Byte;
    SeparateChar: AnsiChar;
  begin
    SeparateChar := CHR_EXT_INTRODUCER;
    for I := 0 to Extensions.Count - 1 do
    begin
      Ext := TExtension(Extensions[I]);
      if Ext <> nil then
      begin
        Stream.Write(SeparateChar, SizeOf(SeparateChar));
        ExtensionLabel := ExtLabels[Ext.FExtType];
        Stream.Write(ExtensionLabel, SizeOf(Byte));
        case Ext.FExtType of
          etGraphic:
            begin
              Stream.Write(Ext.FExtRec.GCE, SizeOf(TGraphicControlExtension));
            end;
          etComment: WriteDataBlock(Stream, Ext.FData);
          etPlainText:
            begin
              Stream.Write(Ext.FExtRec.PTE, SizeOf(TPlainTextExtension));
              WriteDataBlock(Stream, Ext.FData);
            end;
          etApplication:
            begin
              Stream.Write(Ext.FExtRec.APPE, SizeOf(TAppExtension));
              WriteDataBlock(Stream, Ext.FData);
            end;
        end;
      end;
    end;
  end;

begin
  if FItems.Count = 0 then
    GifError(LoadStr(SNoGIFData));
  EncodeFrames(False);
  Mem := TMemoryStream.Create;
  try
    if FImage.FComment.Count > 0 then
      FVersion := gv89a;
    WriteSignature(Mem);
    WriteScreenDescriptor(Mem);
    if FImage.FColorMap.Count > 0 then
    begin
      with FImage.FColorMap do
        Mem.Write(Colors[0], Count * SizeOf(TGIFColorItem));
    end;
    if FLooping and (FItems.Count > 1) then
    begin
      { write looping extension }
      Separator := CHR_EXT_INTRODUCER;
      Mem.Write(Separator, SizeOf(Byte));
      Temp := ExtLabels[etApplication];
      Mem.Write(Temp, SizeOf(Byte));
      Temp := SizeOf(TAppExtension) - SizeOf(Byte);
      Mem.Write(Temp, SizeOf(Byte));
      Mem.Write(LoopExtNS[1], Temp);
      StrList := TStringList.Create;
      try
        StrList.Add(Char(AE_LOOPING) + Char(LoByte(FRepeatCount)) +
          Char(HiByte(FRepeatCount)));
        WriteDataBlock(Mem, StrList);
      finally
        StrList.Free;
      end;
    end;
    Separator := CHR_IMAGE_SEPARATOR;
    for FrameNo := 0 to FItems.Count - 1 do
    begin
      Frame := TGIFFrame(FItems[FrameNo]);
      if Frame.FExtensions <> nil then
        WriteExtensionBlock(Mem, Frame.FExtensions);
      Mem.Write(Separator, SizeOf(Byte));
      Frame.WriteImageDescriptor(Mem);
      Frame.WriteLocalColorMap(Mem);
      Frame.WriteRasterData(Mem);
    end;
    if FImage.FComment.Count > 0 then
    begin
      Separator := CHR_EXT_INTRODUCER;
      Mem.Write(Separator, SizeOf(Byte));
      Temp := ExtLabels[etComment];
      Mem.Write(Temp, SizeOf(Byte));
      WriteDataBlock(Mem, FImage.FComment);
    end;
    Separator := CHR_TRAILER;
    Mem.Write(Separator, SizeOf(Byte));
    Size := Mem.Size;
    if WriteSize then
      Stream.Write(Size, SizeOf(Size));
    Stream.Write(Mem.Memory^, Size);
  finally
    Mem.Free;
  end;
end;

procedure TGIFImage.Grayscale(ForceEncoding: Boolean);
var
  I: Integer;
begin
  if FItems.Count = 0 then
    GifError(LoadStr(SNoGIFData));
  for I := 0 to FItems.Count - 1 do
    Frames[I].GrayscaleImage(ForceEncoding);
  if FBackgroundColor <> clNone then
  begin
    if FImage.FColorMap.Count > 0 then
    begin
      I := FindColorIndex(FImage.FColorMap, FBackgroundColor);
      GrayColorTable(FImage.FColorMap);
      if I >= 0 then
        FBackgroundColor := ItemToRGB(FImage.FColorMap.Colors[I])
      else
        FBackgroundColor := GrayColor(FBackgroundColor);
    end
    else
      FBackgroundColor := GrayColor(FBackgroundColor);
  end;
{$IFDEF RX_D3}
  PaletteModified := True;
{$ENDIF}
  Changed(Self);
end;

procedure TGIFImage.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TGIFImage.DoProgress(Stage: TProgressStage; PercentDone: Byte;
  const Msg: string);
begin
  Progress(Self, Stage, PercentDone, False, Rect(0, 0, 0, 0), Msg);
end;

{$IFNDEF RX_D3}
procedure TGIFImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;
{$ENDIF}

initialization
  CF_GIF := RegisterClipboardFormat('GIF Image');
  RegisterClasses([TGIFFrame, TGIFImage]);
{$IFDEF USE_RX_GIF}
  TPicture.RegisterFileFormat('gif', LoadStr(SGIFImage), TGIFImage);
  TPicture.RegisterClipboardFormat(CF_GIF, TGIFImage);
 {$IFDEF RX_D3}
finalization
  TPicture.UnRegisterGraphicClass(TGIFImage);
 {$ENDIF}
{$ENDIF}
end.