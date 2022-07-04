unit SXPNGUtils;

////////////////////////////////////////////////////////////////////////////////
// SXSkinComponents: Skinnable Visual Controls for Delphi and C++Builder      //
//----------------------------------------------------------------------------//
// Version: 1.2.1                                                             //
// Author: Alexey Sadovnikov                                                  //
// Web Site: http://www.saarixx.info/sxskincomponents/                        //
// E-Mail: sxskincomponents@saarixx.info                                      //
//----------------------------------------------------------------------------//
// LICENSE:                                                                   //
// 1. You may freely distribute this file.                                    //
// 2. You may not make any changes to this file.                              //
// 3. The only person who may change this file is Alexey Sadovnikov.          //
// 4. You may use this file in your freeware projects.                        //
// 5. If you want to use this file in your shareware or commercial project,   //
//    you should purchase a project license or a personal license of          //
//    SXSkinComponents: http://saarixx.info/sxskincomponents/en/purchase.htm  //
// 6. You may freely use, distribute and modify skins for SXSkinComponents.   //
// 7. You may create skins for SXSkinComponents.                              //
//----------------------------------------------------------------------------//
// Copyright (C) 2006-2007, Alexey Sadovnikov. All Rights Reserved.           //
////////////////////////////////////////////////////////////////////////////////

interface

{$I Compilers.inc}

{$TYPEDADDRESS OFF}
{$RANGECHECKS OFF} {$J+}

uses Windows, Classes, Graphics, SysUtils, GR32;

resourcestring

  EPNGInvalidCRCText='This "Portable Network Graphics" image is not valid '+
      'because it contains invalid pieces of data (crc error)';
  EPNGInvalidIHDRText='The "Portable Network Graphics" image could not be '+
      'loaded because one of its main piece of data (ihdr) might be corrupted';
  EPNGMissingMultipleIDATText='This "Portable Network Graphics" image is '+
    'invalid because it has missing image parts.';
  EPNGZLIBErrorText='Could not decompress the image because it contains '+
    'invalid compressed data.'#13#10+' Description: ';
  EPNGInvalidPaletteText='The "Portable Network Graphics" image contains '+
    'an invalid palette.';
  EPNGInvalidFileHeaderText='The file being readed is not a valid '+
    '"Portable Network Graphics" image because it contains an invalid header.'+
    ' This file may be corruped, try obtaining it again.';
  EPNGIHDRNotFirstText='This "Portable Network Graphics" image is not '+
    'supported or it might be invalid.'#13#10+'(IHDR chunk is not the first)';
  EPNGNotExistsText='The PNG file could not be loaded because it does not '+
    'exists.';
  EPNGSizeExceedsText='This "Portable Network Graphics" image is not '+
    'supported because either it''s width or height exceeds the maximum '+
    'size, which is 65535 pixels length.';
  EPNGUnknownPalEntryText='There is no such palette entry.';
  EPNGMissingPaletteText='This "Portable Network Graphics" could not be '+
    'loaded because it uses a color table which is missing.';
  EPNGUnknownCriticalChunkText='This "Portable Network Graphics" image '+
    'contains an unknown critical part which could not be decoded.';
  EPNGUnknownCompressionText='This "Portable Network Graphics" image is '+
    'encoded with an unknown compression scheme which could not be decoded.';
  EPNGUnknownInterlaceText='This "Portable Network Graphics" image uses '+
    'an unknown interlace scheme which could not be decoded.';
  EPNGCannotAssignChunkText='The chunks must be compatible to be assigned.';
  EPNGUnexpectedEndText='This "Portable Network Graphics" image is invalid '+
    'because the decoder found an unexpected end of the file.';
  EPNGNoImageDataText='This "Portable Network Graphics" image contains no '+
    'data.';
  EPNGCannotChangeSizeText='The "Portable Network Graphics" image can not '+
    'be resize by changing width and height properties. Try assigning the '+
    'image from a bitmap.';
  EPNGCannotAddChunkText='The program tried to add a existent critical '+
    'chunk to the current image which is not allowed.';
  EPNGCannotAddInvalidImageText='It''s not allowed to add a new chunk '+
    'because the current image is invalid.';
  EPNGCouldNotLoadResourceText='The PNG image could not be loaded from the '+
    'resource ID.';
  EPNGOutMemoryText='Some operation could not be performed because the '+
    'system is out of resources. Close some windows and try again.';
  EPNGCannotChangeTransparentText='Setting bit transparency color is not '+
    'allowed for PNG images containing alpha value for each pixel '+
    '(COLOR_RGBALPHA and COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText='This operation is not valid because the '+
    'current image contains no valid header.';

const

  ZLIBErrors:array[-6..2]of String=('incompatible version (-6)',
    'buffer error (-5)', 'insufficient memory (-4)', 'data error (-3)',
    'stream error (-2)', 'file error (-1)', '(0)',   'stream end (1)',
    'need dictionary (2)');

  Z_NO_FLUSH   = 0;
  Z_FINISH     = 4;
  Z_STREAM_END = 1;

  FILTER_NONE    = 0;
  FILTER_SUB     = 1;
  FILTER_UP      = 2;
  FILTER_AVERAGE = 3;
  FILTER_PAETH   = 4;

  COLOR_GRAYSCALE      = 0;
  COLOR_RGB            = 2;
  COLOR_PALETTE        = 3;
  COLOR_GRAYSCALEALPHA = 4;
  COLOR_RGBALPHA       = 6;

type

  EPNGOutMemory=class(Exception);
  EPNGError=class(Exception);
  EPNGUnexpectedEnd=class(Exception);
  EPNGInvalidCRC=class(Exception);
  EPNGInvalidIHDR=class(Exception);
  EPNGMissingMultipleIDAT=class(Exception);
  EPNGZLIBError=class(Exception);
  EPNGInvalidPalette=class(Exception);
  EPNGInvalidFileHeader=class(Exception);
  EPNGIHDRNotFirst=class(Exception);
  EPNGNotExists=class(Exception);
  EPNGSizeExceeds=class(Exception);
  EPNGMissingPalette=class(Exception);
  EPNGUnknownCriticalChunk=class(Exception);
  EPNGUnknownCompression=class(Exception);
  EPNGUnknownInterlace=class(Exception);
  EPNGNoImageData=class(Exception);
  EPNGCouldNotLoadResource=class(Exception);
  EPNGCannotChangeTransparent=class(Exception);
  EPNGHeaderNotPresent=class(Exception);

type

  TAlloc=function(AppData:Pointer;Items,Size:Integer):Pointer;
  TFree=procedure(AppData,Block:Pointer);

  TZStreamRec=packed record
   next_in:PChar;
   avail_in:Integer;
   total_in:Integer;
   next_out:PChar;
   avail_out:Integer;
   total_out:Integer;
   msg:PChar;
   internal:Pointer;
   zalloc:TAlloc;
   zfree:TFree;
   AppData:Pointer;
   data_type:Integer;
   adler:Integer;
   reserved:Integer;
  end;

  TRGBLine=array[Word]of TRGBTriple;
  PRGBLine=^TRGBLine;

  TMAXBITMAPINFO=packed record
   bmiHeader:TBitmapInfoHeader;
   bmiColors:packed array[0..255]of TRGBQuad;
  end;

  TPNGTransparencyMode=(ptmNone, ptmBit, ptmPartial);
  PCardinal=^Cardinal;
  PRGBPixel=^TRGBPixel;
  TRGBPixel=packed record
   B,G,R:Byte;
  end;

  TByteArray=array[Word]of Byte;
  PByteArray=^TByteArray;

  TPNGObject=class;
  PPointerArray=^TPointerArray;
  TPointerArray=array[Word]of Pointer;

  TPNGPointerList=class
   private
    fOwner:TPNGObject;
    fCount:Cardinal;
    fMemory:PPointerArray;
    function GetItem(Index:Cardinal):Pointer;
    procedure SetItem(Index:Cardinal;const Value:Pointer);
   protected
    function Remove(Value:Pointer):Pointer; virtual;
    procedure Insert(Value:Pointer;Position:Cardinal);
    procedure Add(Value:Pointer);
    property Item[Index:Cardinal]:Pointer read GetItem write SetItem;
    procedure SetSize(const Size:Cardinal);
    property Owner:TPNGObject read fOwner;
   public
    property Count:Cardinal read fCount write SetSize;
    constructor Create(AOwner:TPNGObject);
    destructor Destroy; override;
  end;

  TChunk=class;
  TChunkClass=class of TChunk;

  TPNGList=class(TPNGPointerList)
   private
    function GetItem(Index:Cardinal):TChunk;
   public
    procedure RemoveChunk(Chunk:TChunk); overload;
    function Add(ChunkClass:TChunkClass):TChunk;
    function ItemFromClass(ChunkClass:TChunkClass):TChunk;
    property Item[Index:Cardinal]:TChunk read GetItem;
  end;

  TChunkIHDR=class;
  TInterlaceMethod=(imNone, imAdam7);
  TCompressionLevel=0..9;
  TFilter=(pfNone, pfSub, pfUp, pfAverage, pfPaeth);
  TFilters=set of TFilter;

  TPNGObject=class(TGraphic)
   protected
    InverseGamma:array[Byte]of Byte;
    procedure InitializeGamma;
   private
    TempPalette:HPalette;
    fFilters:TFilters;
    fCompressionLevel:TCompressionLevel;
    fMaxIdatSize:Integer;
    fInterlaceMethod:TInterlaceMethod;
    fChunkList:TPNGList;
    procedure ClearChunks;
    function HeaderPresent:Boolean;
    procedure GetPixelInfo(var LineSize,Offset:Cardinal);
    procedure SetMaxIdatSize(const Value:Integer);
    function GetAlphaScanline(const LineIndex:Integer):PByteArray;
    function GetScanline(const LineIndex:Integer):Pointer;
    function GetTransparencyMode:TPNGTransparencyMode;
    function GetTransparentColor:TColor;
    procedure SetTransparentColor(const Value:TColor);
   protected
    function GetPalette:HPalette; override;
    function GetWidth:Integer; override;
    function GetHeight:Integer; override;
    procedure SetWidth(Value:Integer); override;
    procedure SetHeight(Value:Integer); override;
    procedure AssignPNG(Source:TPNGObject);
    function GetEmpty:Boolean; override;
    function GetHeader:TChunkIHDR;
    procedure DrawPartialTrans(DC:HDC;Rect:TRect);
    function GetTransparent:Boolean; override;
    function GetPixels(const X,Y:Integer):TColor; virtual;
    procedure SetPixels(const X,Y:Integer;const Value:TColor); virtual;
   public
    GammaTable:array[Byte]of Byte;
    procedure CreateAlpha;
    procedure RemoveTransparency;
    property TransparentColor:TColor read GetTransparentColor write SetTransparentColor;
    procedure AddtEXt(const Keyword,Text:String);
    procedure AddzTXt(const Keyword,Text:String);
    procedure SaveToClipboardFormat(var AFormat:Word;var AData:THandle;var APalette:HPalette); override;
    procedure LoadFromClipboardFormat(AFormat:Word;AData:THandle;APalette:HPalette); override;
    procedure RaiseError(ExceptionClass:ExceptClass;Text:String);
    property Scanline[const Index:Integer]:Pointer read GetScanline;
    property AlphaScanline[const Index:Integer]:PByteArray read GetAlphaScanline;
    property Header:TChunkIHDR read GetHeader;
    property TransparencyMode:TPNGTransparencyMode read GetTransparencyMode;
    procedure Assign(Source:TPersistent); override;
    procedure AssignTo(Dest:TPersistent); override;
    procedure AssignHandle(Handle:HBitmap;Transparent:Boolean;TransparentColor:ColorRef);
    procedure Draw(ACanvas:TCanvas; const Rect:TRect); override;
    property Width:Integer read GetWidth;
    property Height:Integer read GetHeight;
    property InterlaceMethod:TInterlaceMethod read FInterlaceMethod write FInterlaceMethod;
    property Filters:TFilters read fFilters write fFilters;
    property MaxIdatSize:Integer read fMaxIdatSize write SetMaxIdatSize;
    property Empty:Boolean read GetEmpty;
    property CompressionLevel:TCompressionLevel read FCompressionLevel write FCompressionLevel;
    property Chunks:TPNGList read fChunkList;
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream:TStream); override;
    procedure SaveToStream(Stream:TStream); override;
    procedure LoadFromResourceName(Instance:HInst; const Name:String);
    procedure LoadFromResourceID(Instance:HInst; ResID:Integer);
    property Pixels[const X,Y:Integer]:TColor read GetPixels write SetPixels;
  end;

  TChunkName=Array[0..3]of Char;

  TChunk=class
   private
    fData:Pointer;
    fDataSize:Cardinal;
    fOwner:TPNGObject;
    fName:TChunkName;
    function GetHeader:TChunkIHDR;
    function GetIndex:Integer;
    class function GetName:String; virtual;
    function GetChunkName:String;
   public
    property Index:Integer read GetIndex;
    property Header:TChunkIHDR read GetHeader;
    procedure ResizeData(const NewSize:Cardinal);
    property Data:Pointer read fData;
    property DataSize:Cardinal read fDataSize;
    procedure Assign(Source:TChunk); virtual;
    property Owner:TPNGObject read fOwner;
    constructor Create(Owner:TPNGObject); virtual;
    destructor Destroy; override;
    property Name:String read GetChunkName;
    function LoadFromStream(Stream:TStream; const ChunkName:TChunkName;
      Size:Integer):Boolean; virtual;
    function SaveData(Stream:TStream):Boolean;
    function SaveToStream(Stream:TStream):Boolean; virtual;
  end;

  TChunkIEND=class(TChunk);

  PIHDRData=^TIHDRData;
  TIHDRData=packed record
   Width,Height:Cardinal;
   BitDepth,
   ColorType,
   CompressionMethod,
   FilterMethod,
   InterlaceMethod:Byte;
  end;

  TChunkIHDR=class(TChunk)
   private
    ImageHandle:HBitmap;
    ImageDC:HDC;
    HasPalette:Boolean;
    BytesPerRow:Integer;
    ImageData:Pointer;
    ImageAlpha:Pointer;
    IHDRData:TIHDRData;
   protected
    procedure PrepareImageData;
    procedure FreeImageData;
   public
    BitmapInfo:TMaxBitmapInfo;
    property Alpha:Pointer read ImageAlpha;
    property Data:Pointer read ImageData;
    property ImageHandleValue:HBitmap read ImageHandle;
    property Width:Cardinal read IHDRData.Width write IHDRData.Width;
    property Height:Cardinal read IHDRData.Height write IHDRData.Height;
    property BitDepth:Byte read IHDRData.BitDepth write IHDRData.BitDepth;
    property ColorType:Byte read IHDRData.ColorType write IHDRData.ColorType;
    property CompressionMethod:Byte read IHDRData.CompressionMethod write IHDRData.CompressionMethod;
    property FilterMethod:Byte read IHDRData.FilterMethod write IHDRData.FilterMethod;
    property InterlaceMethod:Byte read IHDRData.InterlaceMethod write IHDRData.InterlaceMethod;
    function LoadFromStream(Stream:TStream; const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
    constructor Create(Owner:TPNGObject); override;
    destructor Destroy; override;
    procedure Assign(Source:TChunk); override;
  end;

  TChunkgAMA=class(TChunk)
   private
    function GetValue:Cardinal;
    procedure SetValue(const Value:Cardinal);
   public
    property Gamma:Cardinal read GetValue write SetValue;
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    constructor Create(Owner:TPNGObject); override;
    procedure Assign(Source:TChunk); override;
  end;

  TZStreamRec2=packed record
    ZLIB:TZStreamRec;
    Data:Pointer;
    fStream:TStream;
  end;

  TChunkPLTE=class(TChunk)
   private
    fCount:Integer;
    function GetPaletteItem(Index:Byte):TRGBQuad;
   public
    property Item[Index:Byte]:TRGBQuad read GetPaletteItem;
    property Count:Integer read fCount;
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
    procedure Assign(Source:TChunk); override;
  end;

  TChunktRNS=class(TChunk)
   private
    fBitTransparency:Boolean;
    function GetTransparentColor:ColorRef;
    procedure SetTransparentColor(const Value:ColorRef);
   public
    PaletteValues:array[Byte]of Byte;
    property BitTransparency:Boolean read fBitTransparency;
    property TransparentColor:ColorRef read GetTransparentColor write SetTransparentColor;
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
    procedure Assign(Source:TChunk); override;
  end;

  TChunkIDAT=class(TChunk)
   private
    Header:TChunkIHDR;
    ImageWidth,ImageHeight:Integer;
    Row_Bytes,Offset:Cardinal;
    Encode_Buffer:array[0..5]of PByteArray;
    Row_Buffer:array[Boolean]of PByteArray;
    RowUsed:Boolean;
    EndPos:Integer;
    procedure FilterRow;
    function FilterToEncode:Byte;
    function IDATZlibRead(var ZLIBStream:TZStreamRec2;Buffer:Pointer;
      Count:Integer;var EndPos:Integer;var crcfile:Cardinal):Integer;
    procedure IDATZlibWrite(var ZLIBStream:TZStreamRec2;Buffer:Pointer;const Length:Cardinal);
    procedure FinishIDATZlib(var ZLIBStream:TZStreamRec2);
    procedure PreparePalette;
   protected
    procedure DecodeInterlacedAdam7(Stream:TStream;var ZLIBStream:TZStreamRec2;
               const Size:Integer;var crcfile:Cardinal);
    procedure DecodeNonInterlaced(Stream:TStream;var ZLIBStream:TZStreamRec2;
               const Size:Integer;var crcfile:Cardinal);
   protected
    procedure EncodeNonInterlaced(Stream:TStream;var ZLIBStream:TZStreamRec2);
    procedure EncodeInterlacedAdam7(Stream:TStream;var ZLIBStream:TZStreamRec2);
   protected
    procedure CopyNonInterlacedRGB8(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedRGB16(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedPalette148(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedPalette2(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedGray2(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedGrayscale16(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedRGBAlpha8(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedRGBAlpha16(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedGrayscaleAlpha8(Src,Dest,Trans:PChar);
    procedure CopyNonInterlacedGrayscaleAlpha16(Src,Dest,Trans:PChar);
    procedure CopyInterlacedRGB8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedRGB16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedPalette148(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedPalette2(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedGray2(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedGrayscale16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedRGBAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedRGBAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedGrayscaleAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure CopyInterlacedGrayscaleAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
   protected
    procedure EncodeNonInterlacedRGB8(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedRGB16(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedGrayscale16(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedPalette148(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedRGBAlpha8(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedRGBAlpha16(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedGrayscaleAlpha8(Src,Dest,Trans:PChar);
    procedure EncodeNonInterlacedGrayscaleAlpha16(Src,Dest,Trans:PChar);
    procedure EncodeInterlacedRGB8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedRGB16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedPalette148(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedGrayscale16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedRGBAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedRGBAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedGrayscaleAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
    procedure EncodeInterlacedGrayscaleAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
   public
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
  end;

  TChunktIME=class(TChunk)
   private
    fYear:Word;
    fMonth,fDay,fHour,fMinute,fSecond:Byte;
   public
    property Year:Word read fYear write fYear;
    property Month:Byte read fMonth write fMonth;
    property Day:Byte read fDay write fDay;
    property Hour:Byte read fHour write fHour;
    property Minute:Byte read fMinute write fMinute;
    property Second:Byte read fSecond write fSecond;
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
  end;

  TChunktEXt=class(TChunk)
   private
    fKeyword,fText:String;
   public
    property Keyword:String read fKeyword write fKeyword;
    property Text:String read fText write fText;
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
    procedure Assign(Source:TChunk); override;
  end;

  TChunkzTXt=class(TChunktEXt)
    function LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean; override;
    function SaveToStream(Stream:TStream):Boolean; override;
  end;

procedure RegisterChunk(ChunkClass:TChunkClass);
function update_crc(crc:Cardinal;buf:PByteArray;len:Integer):Cardinal;
function ByteSwap(const a:integer):integer;

function inflateInit_(var strm:TZStreamRec;version:PChar;recsize:Integer):Integer; forward;
function inflate(var strm:TZStreamRec;flush:Integer):Integer; forward;
function inflateEnd(var strm:TZStreamRec):Integer; forward;
function deflateInit_(var strm:TZStreamRec;level:Integer; version:PChar;recsize:Integer):Integer; forward;
function deflate(var strm:TZStreamRec;flush:Integer):Integer; forward;
function deflateEnd(var strm:TZStreamRec):Integer; forward;

const zlib_version='1.1.4';

function adler32(adler:Integer;buf:PChar;len:Integer):Integer;

const
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;

  Z_OK            =  0;
  Z_NEED_DICT     =  2;
  Z_ERRNO         = -1;
  Z_STREAM_ERROR  = -2;
  Z_DATA_ERROR    = -3;
  Z_MEM_ERROR     = -4;
  Z_BUF_ERROR     = -5;
  Z_VERSION_ERROR = -6;

  Z_NO_COMPRESSION      =  0;
  Z_BEST_SPEED          =  1;
  Z_BEST_COMPRESSION    =  9;
  Z_DEFAULT_COMPRESSION = -1;

  Z_FILTERED         = 1;
  Z_HUFFMAN_ONLY     = 2;
  Z_DEFAULT_STRATEGY = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

  _z_errmsg:array[0..9]of PChar=(
    'need dictionary',      
    'stream end',
    '',
    'file error',
    'stream error',
    'data error',
    'insufficient memory',
    'buffer error',
    'incompatible version',
    ''
  );

procedure PNGObjectToBitmap32(PNG:TPNGObject;B32:TBitmap32);

implementation

var
  ChunkClasses:TPNGPointerList;
  crc_table:array[0..255]of Cardinal;
  crc_table_computed:Boolean;

{$L obj\deflate.obj}
{$L obj\trees.obj}
{$L obj\inflate.obj}
{$L obj\inftrees.obj}
{$L obj\adler32.obj}
{$L obj\infblock.obj}
{$L obj\infcodes.obj}
{$L obj\infutil.obj}
{$L obj\inffast.obj}

procedure _tr_init; external;
procedure _tr_tally; external;
procedure _tr_flush_block; external;
procedure _tr_align; external;
procedure _tr_stored_block; external;
function adler32; external;
procedure inflate_blocks_new; external;
procedure inflate_blocks; external;
procedure inflate_blocks_reset; external;
procedure inflate_blocks_free; external;
procedure inflate_set_dictionary; external;
procedure inflate_trees_bits; external;
procedure inflate_trees_dynamic; external;
procedure inflate_trees_fixed; external;
procedure inflate_codes_new; external;
procedure inflate_codes; external;
procedure inflate_codes_free; external;
procedure _inflate_mask; external;
procedure inflate_flush; external;
procedure inflate_fast; external;

procedure _memset(P:Pointer;B:Byte;count:Integer);cdecl;
begin
 FillChar(P^,count,B);
end;

procedure _memcpy(dest,source:Pointer;count:Integer);cdecl;
begin
 Move(source^,dest^,count);
end;

function deflateInit_(var strm:TZStreamRec;level:Integer;version:PChar;
  recsize:Integer):Integer; external;
function deflate(var strm:TZStreamRec;flush:Integer):Integer; external;
function deflateEnd(var strm:TZStreamRec):Integer; external;
function inflateInit_(var strm:TZStreamRec;version:PChar;
  recsize:Integer):Integer; external;
function inflate(var strm:TZStreamRec;flush:Integer):Integer; external;
function inflateEnd(var strm:TZStreamRec):Integer; external;
function inflateReset(var strm:TZStreamRec):Integer; external;

function zcalloc(AppData:Pointer;Items,Size:Integer):Pointer;
begin
 GetMem(Result,Items*Size);
end;

procedure zcfree(AppData,Block:Pointer);
begin
 FreeMem(Block);
end;

procedure PNGObjectToBitmap32(PNG:TPNGObject;B32:TBitmap32);
var PB:PByte;
    PC:PColor32;
     C:Cardinal;
begin
 B32.SetSize(PNG.Width,PNG.Height);
 if (PNG.Width=0) or (PNG.Height=0) then exit;
 SetStretchBltMode(B32.Canvas.Handle,COLORONCOLOR);
 StretchDiBits(B32.Canvas.Handle,0,0,PNG.Width,PNG.Height,0,0,
               PNG.Width,PNG.Height,PNG.Header.Data,
               pBitmapInfo(@PNG.Header.BitmapInfo)^,DIB_RGB_COLORS,SRCCOPY);
 if PNG.Header.ColorType in [COLOR_GRAYSCALEALPHA,COLOR_RGBALPHA] then
  begin
   PB:=Pointer(PNG.AlphaScanline[0]);
   if PB<>nil then
    begin
     PC:=Pointer(B32.Bits);
     for C:=0 to PNG.Width*PNG.Height-1 do
      begin
       PC^:=SetAlpha(PC^,PB^);
       Inc(PB); Inc(PC);
      end;
    end;
  end else
   begin
    PC:=Pointer(B32.Bits);
    for C:=0 to PNG.Width*PNG.Height-1 do
     begin
      PC^:=SetAlpha(PC^,$FF);
      Inc(PC);
     end;
   end;
end;

procedure DrawTransparentBitmap(dc:HDC;srcBits:Pointer;var srcHeader:TBitmapInfoHeader;
           srcBitmapInfo:PBitmapInfo;Rect:TRect;cTransparentColor:COLORREF);
var
  cColor:  COLORREF;
  bmAndBack,bmAndObject,bmAndMem:HBITMAP;
  bmBackOld,bmObjectOld,bmMemOld:HBITMAP;
  hdcMem,hdcBack,hdcObject,hdcTemp:HDC;
  ptSize,orgSize:TPOINT;
  OldBitmap,DrawBitmap:HBITMAP;
begin
  hdcTemp:=CreateCompatibleDC(dc);
  DrawBitmap:=CreateDIBitmap(dc,srcHeader,CBM_INIT,srcBits,srcBitmapInfo^,DIB_RGB_COLORS);
  OldBitmap:=SelectObject(hdcTemp,DrawBitmap);
  OrgSize.x:=Abs(srcHeader.biWidth);
  OrgSize.y:=Abs(srcHeader.biHeight);
  ptSize.x:=Rect.Right-Rect.Left;
  ptSize.y:=Rect.Bottom-Rect.Top;
  hdcBack:=CreateCompatibleDC(dc);
  hdcObject:=CreateCompatibleDC(dc);
  hdcMem:=CreateCompatibleDC(dc);
  bmAndBack:=CreateBitmap(ptSize.x,ptSize.y,1,1,nil);
  bmAndObject:=CreateBitmap(ptSize.x,ptSize.y,1,1,nil);
  bmAndMem:=CreateCompatibleBitmap(dc,ptSize.x,ptSize.y);
  bmBackOld:=SelectObject(hdcBack,bmAndBack);
  bmObjectOld:=SelectObject(hdcObject,bmAndObject);
  bmMemOld:=SelectObject(hdcMem,bmAndMem);
  cColor:=SetBkColor(hdcTemp,cTransparentColor);
  StretchBlt(hdcObject,0,0,ptSize.x,ptSize.y,hdcTemp,0,0,orgSize.x,orgSize.y,SRCCOPY);
  SetBkColor(hdcTemp,cColor);
  BitBlt(hdcBack,0,0,ptSize.x,ptSize.y,hdcObject,0,0,NOTSRCCOPY);
  BitBlt(hdcMem,0,0,ptSize.x,ptSize.y,dc,Rect.Left,Rect.Top,SRCCOPY);
  BitBlt(hdcMem,0,0,ptSize.x,ptSize.y,hdcObject,0,0,SRCAND);
  StretchBlt(hdcTemp,0,0,OrgSize.x,OrgSize.y,hdcBack,0,0,PtSize.x,PtSize.y,SRCAND);
  StretchBlt(hdcMem,0,0,ptSize.x,ptSize.y,hdcTemp,0,0,OrgSize.x,OrgSize.y,SRCPAINT);
  BitBlt(dc,Rect.Left,Rect.Top,ptSize.x,ptSize.y,hdcMem,0,0,SRCCOPY);
  DeleteObject(SelectObject(hdcBack,bmBackOld));
  DeleteObject(SelectObject(hdcObject,bmObjectOld));
  DeleteObject(SelectObject(hdcMem,bmMemOld));
  DeleteObject(SelectObject(hdcTemp,OldBitmap));
  DeleteDC(hdcMem);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcTemp);
end;

procedure make_crc_table;
var c:Cardinal;
  n,k:Integer;
begin
 for n:=0 to 255 do
  begin
   c:=Cardinal(n);
   for k:=0 to 7 do
    begin
     if Boolean(c and 1) then
      c:=$edb88320 xor (c shr 1) else c:=c shr 1;
    end;
   crc_table[n]:=c;
  end;
 crc_table_computed:=True;
end;

function update_crc(crc:Cardinal;buf:PByteArray;len:Integer):Cardinal;
var c:Cardinal;
    n:Integer;
begin
 c:=crc;
 if not crc_table_computed then
  make_crc_table;
 for n:=0 to len-1 do
  c:=crc_table[(c xor buf^[n]) and $FF] xor (c shr 8);
 Result:=c;
end;

function PaethPredictor(a,b,c:Byte):Byte;
var pa,pb,pc:Integer;
begin
 pa:=Abs(b-c);
 pb:=Abs(a-c);
 pc:=Abs(a+b-c*2);
 if (pa<=pb) and (pa<=pc) then Result:=a else
  if pb<=pc then Result:=b else Result:=c;
end;

function ByteSwap(const A:Integer):Integer;
asm
 BSWAP EAX
end;

function ByteSwap16(A:Word):Word;
asm
 BSWAP EAX
 SHR   EAX,16
end;

function BytesForPixels(const Pixels:Integer;const ColorType,BitDepth:Byte):Integer;
begin
 case ColorType of
  COLOR_GRAYSCALE,
  COLOR_PALETTE:        Result:=(Pixels*BitDepth+7) div 8;
  COLOR_RGB:            Result:=(Pixels*BitDepth*3) div 8;
  COLOR_GRAYSCALEALPHA: Result:=(Pixels*BitDepth*2) div 8;
  COLOR_RGBALPHA:       Result:=(Pixels*BitDepth*4) div 8;
  else                  Result:=0;
 end;
end;

type PChunkClassInfo=^TChunkClassInfo;
     TChunkClassInfo=record
      ClassName:TChunkClass;
     end;

procedure RegisterChunk(ChunkClass:TChunkClass);
var NewClass:PChunkClassInfo;
begin
  if ChunkClasses=nil then ChunkClasses:=TPNGPointerList.Create(nil);
  new(NewClass);
  NewClass^.ClassName:=ChunkClass;
  ChunkClasses.Add(NewClass);
end;

procedure FreeChunkClassList;
var  i:Integer;
begin
  if (ChunkClasses<>nil) then
  begin
    for i:=0 to ChunkClasses.Count-1 do
      Dispose(pChunkClassInfo(ChunkClasses.Item[i]));
    ChunkClasses.Free;
  end;
end;

procedure RegisterCommonChunks;
begin
  RegisterChunk(TChunkIEND);
  RegisterChunk(TChunkIHDR);
  RegisterChunk(TChunkIDAT);
  RegisterChunk(TChunkPLTE);
  RegisterChunk(TChunkgAMA);
  RegisterChunk(TChunktRNS);
  RegisterChunk(TChunktIME);
  RegisterChunk(TChunktEXt);
  RegisterChunk(TChunkzTXt);
end;

function CreateClassChunk(Owner:TPNGObject;Name:TChunkName):TChunk;
var
  i:Integer;
  NewChunk:TChunkClass;
begin
  NewChunk:=TChunk;
  if Assigned(ChunkClasses) then
    for i:=0 to ChunkClasses.Count-1 do
    begin
      if pChunkClassInfo(ChunkClasses.Item[i])^.ClassName.GetName=Name then
      begin
        NewChunk:=pChunkClassInfo(ChunkClasses.Item[i])^.ClassName;
        break;
      end;
    end;
  Result:=NewChunk.Create(Owner);
  Result.fName:=Name;
end;

const ZLIBAllocate=High(Word);

function ZLIBInitInflate(Stream:TStream):TZStreamRec2;
begin
 FillChar(Result,sizeof(TZStreamRec2),#0);
 with Result do
  begin
    GetMem(Data,ZLIBAllocate);
    fStream:=Stream;
  end;
 InflateInit_(Result.zlib,zlib_version,sizeof(TZStreamRec));
end;

function ZLIBInitDeflate(Stream:TStream;
  Level:TCompressionlevel;Size:Cardinal):TZStreamRec2;
begin
  FillChar(Result,sizeof(TZStreamRec2),#0);
  with Result,ZLIB do
  begin
    GetMem(Data,Size);
    fStream:=Stream;
    next_out:=Data;
    avail_out:=Size;
  end;
  deflateInit_(Result.zlib,Level,zlib_version,sizeof(TZStreamRec));
end;

procedure ZLIBTerminateDeflate(var ZLIBStream:TZStreamRec2);
begin
  DeflateEnd(ZLIBStream.zlib);
  FreeMem(ZLIBStream.Data,ZLIBAllocate);
end;

procedure ZLIBTerminateInflate(var ZLIBStream:TZStreamRec2);
begin
  InflateEnd(ZLIBStream.zlib);
  FreeMem(ZLIBStream.Data,ZLIBAllocate);
end;

function DecompressZLIB(const Input:Pointer;InputSize:Integer;
  var Output:Pointer;var OutputSize:Integer;
  var ErrorOutput:String):Boolean;
var
  StreamRec:TZStreamRec;
  Buffer:array[Byte]of Byte;
  InflateRet:Integer;
begin
  with StreamRec do
  begin
    Result:=True;
    OutputSize:=0;
    FillChar(StreamRec,sizeof(TZStreamRec),#0);
    InflateInit_(StreamRec,zlib_version,sizeof(TZStreamRec));
    next_in:=Input;
    avail_in:=InputSize;
    repeat
      if (avail_out=0) then
      begin
        next_out:=@Buffer;
        avail_out:=sizeof(Buffer);
      end;
      InflateRet:=inflate(StreamRec,0);
      if (InflateRet=Z_STREAM_END) or (InflateRet=0) then
      begin
        Inc(OutputSize,total_out);
        if Output=nil then
          GetMem(Output,OutputSize) else ReallocMem(Output,OutputSize);
        CopyMemory(Ptr(Longint(Output)+OutputSize-total_out),
          @Buffer,total_out);
      end
      else if InflateRet<0 then
      begin
        Result:=False;
        ErrorOutput:=StreamRec.msg;
        InflateEnd(StreamRec);
        exit;
      end;
    until InflateRet=Z_STREAM_END;
    InflateEnd(StreamRec);
  end;
end;

function CompressZLIB(Input:Pointer;InputSize,CompressionLevel:Integer;
  var Output:Pointer;var OutputSize:Integer;
  var ErrorOutput:String):Boolean;
var
  StreamRec:TZStreamRec;
  Buffer:array[Byte]of Byte;
  DeflateRet:Integer;
begin
  with StreamRec do
  begin
    Result:=True;
    OutputSize:=0;
    FillChar(StreamRec,sizeof(TZStreamRec),#0);
    DeflateInit_(StreamRec,CompressionLevel,zlib_version,sizeof(TZStreamRec));
    next_in:=Input;
    avail_in:=InputSize;
    while avail_in>0 do
    begin
      if avail_out=0 then
      begin
        next_out:=@Buffer;
        avail_out:=sizeof(Buffer);
      end;
      DeflateRet:=deflate(StreamRec,Z_FINISH);
      if (DeflateRet=Z_STREAM_END) or (DeflateRet=0) then
      begin
        Inc(OutputSize,total_out);
        if Output=nil then
          GetMem(Output,OutputSize) else ReallocMem(Output,OutputSize);
        CopyMemory(Ptr(Longint(Output)+OutputSize-total_out),
          @Buffer,total_out);
      end
      else if DeflateRet<0 then
      begin
        Result:=False;
        ErrorOutput:=StreamRec.msg;
        DeflateEnd(StreamRec);
        exit;
      end;
    end;
    DeflateEnd(StreamRec);
  end;
end;

constructor TPNGPointerList.Create(AOwner:TPNGObject);
begin
 inherited Create;
 fOwner:=AOwner;
end;

function TPNGPointerList.Remove(Value:Pointer):Pointer;
var
  I,Position:Integer;
begin
  Position:=-1;
  for I:=0 to Count-1 do
    if Value=Item[I] then Position:=I;
  if Position>=0 then
  begin
    Result:=Item[Position];
    Dec(fCount);
    if Position<Integer(FCount) then
      System.Move(fMemory^[Position+1],fMemory^[Position],
      (Integer(fCount)-Position)*sizeof(Pointer));
  end else Result:=nil
end;

procedure TPNGPointerList.Add(Value:Pointer);
begin
  Count:=Count+1;
  Item[Count-1]:=Value;
end;

destructor TPNGPointerList.Destroy;
begin
 if fMemory<>nil then
  FreeMem(fMemory,fCount*sizeof(Pointer));
 inherited Destroy;
end;

function TPNGPointerList.GetItem(Index:Cardinal):Pointer;
begin
  if (Index<=Count-1) then
    Result:=fMemory[Index]
  else
    Result:=nil;
end;

procedure TPNGPointerList.Insert(Value:Pointer;Position:Cardinal);
begin
  if (Position<Count) then
  begin
    SetSize(Count+1);
    if Position<Count then
      System.Move(fMemory^[Position],fMemory^[Position+1],
        (Count-Position-1)*sizeof(Pointer));
    Item[Position]:=Value;
  end;
end;

procedure TPNGPointerList.SetItem(Index:Cardinal;const Value:Pointer);
begin
  if (Index<=Count-1) then
    fMemory[Index]:=Value
end;

procedure TPNGPointerList.SetSize(const Size:Cardinal);
begin
  if (fMemory=nil) and (Size>0) then
    GetMem(fMemory,Size*sizeof(Pointer))
  else
    if Size>0 then
      ReallocMem(fMemory,Size*sizeof(Pointer))
    else
    begin
      FreeMem(fMemory);
      fMemory:=nil;
    end;
  fCount:=Size;
end;

procedure TPNGList.RemoveChunk(Chunk:TChunk);
begin
  Remove(Chunk);
  Chunk.Free
end;

function TPNGList.Add(ChunkClass:TChunkClass):TChunk;
var
  IHDR:TChunkIHDR;
  IEND:TChunkIEND;

  IDAT:TChunkIDAT;
  PLTE:TChunkPLTE;
begin
  Result:=nil;
  if (ChunkClass=TChunkIHDR) or (ChunkClass=TChunkIDAT) or
    (ChunkClass=TChunkPLTE) or (ChunkClass=TChunkIEND) then
    fOwner.RaiseError(EPNGError,EPNGCannotAddChunkText)
  else if ((ChunkClass=TChunkgAMA) and (ItemFromClass(TChunkgAMA)<>nil)) or
     ((ChunkClass=TChunktRNS) and (ItemFromClass(TChunktRNS)<>nil)) then
    fOwner.RaiseError(EPNGError,EPNGCannotAddChunkText)
  else if (ItemFromClass(TChunkIEND)=nil) or
    (ItemFromClass(TChunkIHDR)=nil) then
    fOwner.RaiseError(EPNGError,EPNGCannotAddInvalidImageText)
  else
  begin
    IHDR:=ItemFromClass(TChunkIHDR) as TChunkIHDR;
    IEND:=ItemFromClass(TChunkIEND) as TChunkIEND;
    Result:=ChunkClass.Create(Owner);
    if (ChunkClass=TChunkgAMA) then
      Insert(Result,IHDR.Index+1)
    else if (ChunkClass=TChunktRNS) then
    begin
      IDAT:=ItemFromClass(TChunkIDAT) as TChunkIDAT;
      PLTE:=ItemFromClass(TChunkPLTE) as TChunkPLTE;
      if Assigned(PLTE) then
        Insert(Result,PLTE.Index+1)
      else if Assigned(IDAT) then
        Insert(Result,IDAT.Index)
      else
        Insert(Result,IHDR.Index+1)
    end
    else
      Insert(Result,IEND.Index);
  end;
end;

function TPNGList.GetItem(Index:Cardinal):TChunk;
begin
  Result:=inherited GetItem(Index);
end;

function TPNGList.ItemFromClass(ChunkClass:TChunkClass):TChunk;
var
  i:Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if Item[i] is ChunkClass then
    begin
      Result:=Item[i];
      break;
    end;
end;

procedure TChunk.ResizeData(const NewSize:Cardinal);
begin
  fDataSize:=NewSize;
  ReallocMem(fData,NewSize+1);
end;

function TChunk.GetIndex:Integer;
var
  i:Integer;
begin
  Result:=-1;
  for i:=0 to Owner.Chunks.Count-1 do
    if Owner.Chunks.Item[i]=Self then
    begin
      Result:=i;
      exit;
    end;
end;

function TChunk.GetHeader:TChunkIHDR;
begin
  Result:=Owner.Chunks.Item[0] as TChunkIHDR;
end;

procedure TChunk.Assign(Source:TChunk);
begin
  fName:=Source.fName;
  ResizeData(Source.fDataSize);
  if fDataSize>0 then CopyMemory(fData,Source.fData,fDataSize);
end;

constructor TChunk.Create(Owner:TPNGObject);
var
  ChunkName:String;
begin
  inherited Create;
  ChunkName:=Copy(ClassName,Length('TChunk')+1,Length(ClassName));
  if Length(ChunkName)=4 then CopyMemory(@fName[0],@ChunkName[1],4);
  GetMem(fData,1);
  fDataSize:=0;
  fOwner:=Owner;
end;

destructor TChunk.Destroy;
begin
 FreeMem(fData,fDataSize+1);
 inherited Destroy;
end;

function TChunk.GetChunkName:String;
begin
  Result:=fName
end;

class function TChunk.GetName:String;
begin
  Result:=Copy(ClassName,Length('TChunk')+1,Length(ClassName));
end;

function TChunk.SaveData(Stream:TStream):Boolean;
var ChunkSize,ChunkCRC:Cardinal;
begin
  ChunkSize:=ByteSwap(DataSize);
  Stream.Write(ChunkSize,4);
  Stream.Write(fName,4);
  if DataSize>0 then Stream.Write(Data^,DataSize);
  ChunkCRC:=update_crc($ffffffff,@fName[0],4);
  ChunkCRC:=Byteswap(update_crc(ChunkCRC,Data,DataSize) xor $ffffffff);
  Stream.Write(ChunkCRC,4);
  Result:=True;
end;

function TChunk.SaveToStream(Stream:TStream):Boolean;
begin
  Result:=SaveData(Stream)
end;

function TChunk.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;
  Size:Integer):Boolean;
var
  CheckCRC:Cardinal;
  RightCRC:Cardinal;
begin
  ResizeData(Size);
  if Size>0 then Stream.Read(fData^,Size);
  Stream.Read(CheckCRC,4);
  CheckCrc:=ByteSwap(CheckCRC);
  RightCRC:=update_crc($ffffffff,@ChunkName[0],4);
  RightCRC:=update_crc(RightCRC,fData,Size) xor $ffffffff;
  Result:=RightCRC=CheckCrc;
  if not Result then
   begin
    Owner.RaiseError(EPNGInvalidCRC,EPNGInvalidCRCText);
    exit;
   end;
end;

function TChunktIME.LoadFromStream(Stream:TStream;
  const ChunkName:TChunkName;Size:Integer):Boolean;
begin
  Result:=inherited LoadFromStream(Stream,ChunkName,Size);
  if not Result or (Size<>7) then exit;
  fYear:=((PByte(Longint(Data) )^)*256)+ (PByte(Longint(Data)+1)^);
  fMonth:=PByte(Longint(Data)+2)^;
  fDay:=PByte(Longint(Data)+3)^;
  fHour:=PByte(Longint(Data)+4)^;
  fMinute:=PByte(Longint(Data)+5)^;
  fSecond:=PByte(Longint(Data)+6)^;
end;

function TChunktIME.SaveToStream(Stream:TStream):Boolean;
begin
  ResizeData(7);
  PWord(Data)^:=Year;
  PByte(Longint(Data)+2)^:=Month;
  PByte(Longint(Data)+3)^:=Day;
  PByte(Longint(Data)+4)^:=Hour;
  PByte(Longint(Data)+5)^:=Minute;
  PByte(Longint(Data)+6)^:=Second;
  Result:=inherited SaveToStream(Stream);
end;

function TChunkzTXt.LoadFromStream(Stream:TStream;
  const ChunkName:TChunkName;Size:Integer):Boolean;
var
  ErrorOutput:String;
  CompressionMethod:Byte;
  Output:Pointer;
  OutputSize:Integer;
begin
  Result:=inherited LoadFromStream(Stream,ChunkName,Size);
  if not Result or (Size<4) then exit;
  fKeyword:=PChar(Data);
  CompressionMethod:=PByte(Longint(fKeyword)+Length(fKeyword))^;
  fText:='';
  if CompressionMethod=0 then
  begin
    Output:=nil;
    if DecompressZLIB(PChar(Longint(Data)+Length(fKeyword)+2),
      Size-Length(fKeyword)-2,Output,OutputSize,ErrorOutput) then
    begin
      SetLength(fText,OutputSize);
      CopyMemory(@fText[1],Output,OutputSize);
    end;
    FreeMem(Output);
  end;
end;

function TChunkztXt.SaveToStream(Stream:TStream):Boolean;
var
  Output:Pointer;
  OutputSize:Integer;
  ErrorOutput:String;
begin
  Output:=nil;
  if fText='' then fText:=' ';
  if CompressZLIB(@fText[1],Length(fText),Owner.CompressionLevel,Output,
    OutputSize,ErrorOutput) then
  begin
    ResizeData(Length(fKeyword)+2+OutputSize);
    FillChar(Data^,DataSize,#0);
    if Keyword<>'' then
      CopyMemory(Data,@fKeyword[1],Length(Keyword));
    PByte(Ptr(Longint(Data)+Length(Keyword)+1))^:=0;
    if OutputSize>0 then
      CopyMemory(Ptr(Longint(Data)+Length(Keyword)+2),Output,OutputSize);
    Result:=SaveData(Stream);
  end else Result:=False;
  if Output<>nil then FreeMem(Output)
end;

procedure TChunktEXt.Assign(Source:TChunk);
begin
  fKeyword:=TChunktEXt(Source).fKeyword;
  fText:=TChunktEXt(Source).fText;
end;

function TChunktEXt.LoadFromStream(Stream:TStream;
  const ChunkName:TChunkName;Size:Integer):Boolean;
begin
  Result:=inherited LoadFromStream(Stream,ChunkName,Size);
  if not Result or (Size<3) then exit;
  fKeyword:=PChar(Data);
  SetLength(fText,Size-Length(fKeyword)-1);
  CopyMemory(@fText[1],Ptr(Longint(Data)+Length(fKeyword)+1),
    Length(fText));
end;

function TChunktEXt.SaveToStream(Stream:TStream):Boolean;
begin
  ResizeData(Length(fKeyword)+1+Length(fText));
  FillChar(Data^,DataSize,#0);
  if Keyword<>'' then
    CopyMemory(Data,@fKeyword[1],Length(Keyword));
  if Text<>'' then
    CopyMemory(Ptr(Longint(Data)+Length(Keyword)+1),@fText[1],
      Length(Text));
  Result:=inherited SaveToStream(Stream);
end;

constructor TChunkIHDR.Create(Owner:TPNGObject);
begin
 inherited Create(Owner);
end;

destructor TChunkIHDR.Destroy;
begin
  FreeImageData;
  inherited Destroy;
end;

procedure TChunkIHDR.Assign(Source:TChunk);
begin
  if Source is TChunkIHDR then
  begin
    IHDRData:=TChunkIHDR(Source).IHDRData;
    PrepareImageData;
    CopyMemory(ImageData,TChunkIHDR(Source).ImageData,
      BytesPerRow*Integer(Height));
    CopyMemory(ImageAlpha,TChunkIHDR(Source).ImageAlpha,
      Integer(Width)*Integer(Height));
    BitmapInfo.bmiColors:=TChunkIHDR(Source).BitmapInfo.bmiColors;
  end
  else
    Owner.RaiseError(EPNGError,EPNGCannotAssignChunkText);
end;

procedure TChunkIHDR.FreeImageData;
begin
  if ImageHandle<>0  then DeleteObject(ImageHandle);
  if ImageDC    <>0  then DeleteDC(ImageDC);
  if ImageAlpha<>nil then FreeMem(ImageAlpha);
  ImageHandle:=0;ImageDC:=0;ImageAlpha:=nil;ImageData:=nil;
end;

function TChunkIHDR.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;
  Size:Integer):Boolean;
begin
  Result:=inherited LoadFromStream(Stream,ChunkName,Size);
  if not Result then exit;
  if (fDataSize<sizeof(TIHdrData)) then
  begin
    Result:=False;
    Owner.RaiseError(EPNGInvalidIHDR,EPNGInvalidIHDRText);
    exit;
  end;
  IHDRData:=PIHDRData(fData)^;
  IHDRData.Width:=ByteSwap(IHDRData.Width);
  IHDRData.Height:=ByteSwap(IHDRData.Height);
  if (IHDRData.Width>High(Word)) or (IHDRData.Height>High(Word)) then
  begin
    Result:=False;
    Owner.RaiseError(EPNGSizeExceeds,EPNGSizeExceedsText);
    exit;
  end;
  if (IHDRData.CompressionMethod<>0) then
  begin
    Result:=False;
    Owner.RaiseError(EPNGUnknownCompression,EPNGUnknownCompressionText);
    exit;
  end;
  if (IHDRData.InterlaceMethod<>0) and (IHDRData.InterlaceMethod<>1) then
  begin
    Result:=False;
    Owner.RaiseError(EPNGUnknownInterlace,EPNGUnknownInterlaceText);
    exit;
  end;
  Owner.InterlaceMethod:=TInterlaceMethod(IHDRData.InterlaceMethod);
  PrepareImageData;
end;

function TChunkIHDR.SaveToStream(Stream:TStream):Boolean;
begin
 if BitDepth=2 then BitDepth:=4;
 ResizeData(sizeof(TIHDRData));
 PIHDRData(fData)^:=IHDRData;
 PIHDRData(fData)^.Width:=ByteSwap(PIHDRData(fData)^.Width);
 PIHDRData(fData)^.Height:=ByteSwap(PIHDRData(fData)^.Height);
 PIHDRData(fData)^.InterlaceMethod:=Byte(Owner.InterlaceMethod);
 Result:=inherited SaveToStream(Stream);
end;

procedure TChunkIHDR.PrepareImageData;

 procedure SetInfo(const Bitdepth:Integer;const Palette:Boolean);
 begin
  HasPalette:=Palette;
  FillChar(BitmapInfo,sizeof(BitmapInfo),#0);
  with BitmapInfo.bmiHeader do
   begin
    biSize:=sizeof(TBitmapInfoHeader);
    biHeight:=Height;
    biWidth:=Width;
    biPlanes:=1;
    biBitCount:=BitDepth;
    biCompression:=BI_RGB;
   end;
 end;

begin
  FillChar(BitmapInfo,sizeof(TMaxBitmapInfo),#0);
  FreeImageData;
  case ColorType of
    COLOR_GRAYSCALE,COLOR_PALETTE,COLOR_GRAYSCALEALPHA:
      case BitDepth of
        1,4,8:SetInfo(BitDepth,True);
        2:SetInfo(4,True);
        16:SetInfo(8,True);
      end;
    COLOR_RGB,COLOR_RGBALPHA: SetInfo(24,False);
  end;
  BytesPerRow:=(((BitmapInfo.bmiHeader.biBitCount*Width)+31)
    and not 31) div 8;
  if (ColorType=COLOR_RGBALPHA) or (ColorType=COLOR_GRAYSCALEALPHA) then
  begin
    GetMem(ImageAlpha,Integer(Width)*Integer(Height));
    FillChar(ImageAlpha^,Integer(Width)*Integer(Height),#0);
  end;
  ImageDC:=CreateCompatibleDC(0);
  ImageHandle:=CreateDIBSection(ImageDC,PBitmapInfo(@BitmapInfo)^,
    DIB_RGB_COLORS,ImageData,0,0);
  with Owner do
    if  TempPalette<>0 then
    begin
      DeleteObject(TempPalette);
      TempPalette:=0;
    end;
  zeromemory(ImageData,BytesPerRow*Integer(Height));
end;

procedure TChunktRNS.SetTransparentColor(const Value:ColorRef);
var
  i:Byte;
  LookColor:TRGBQuad;
begin
  FillChar(PaletteValues,sizeof(PaletteValues),#0);
  fBitTransparency:=True;
  with Header do
    case ColorType of
      COLOR_GRAYSCALE:
      begin
        Self.ResizeData(2);
        PWord(@PaletteValues[0])^:=ByteSwap16(GetRValue(Value));
      end;
      COLOR_RGB:
      begin
        Self.ResizeData(6);
        PWord(@PaletteValues[0])^:=ByteSwap16(GetRValue(Value));
        PWord(@PaletteValues[2])^:=ByteSwap16(GetGValue(Value));
        PWord(@PaletteValues[4])^:=ByteSwap16(GetBValue(Value));
      end;
      COLOR_PALETTE:
      begin
        LookColor.rgbRed:=GetRValue(Value);
        LookColor.rgbGreen:=GetGValue(Value);
        LookColor.rgbBlue:=GetBValue(Value);
        for i:=0 to 255 do
          if CompareMem(@BitmapInfo.bmiColors[i],@LookColor,3) then
            Break;
        FillChar(PaletteValues,i,255);
        Self.ResizeData(i+1)
      end;
    end;
end;

function TChunktRNS.GetTransparentColor:ColorRef;
var PaletteChunk:TChunkPLTE;
               i:Integer;
begin
 Result:=0;
 with Header do
  case ColorType of
   COLOR_GRAYSCALE: Result:=RGB(PaletteValues[0],PaletteValues[0],PaletteValues[0]);
   COLOR_RGB:       Result:=RGB(PaletteValues[1],PaletteValues[3],PaletteValues[5]);
   COLOR_PALETTE:   begin
                     PaletteChunk:=Owner.Chunks.ItemFromClass(TChunkPLTE) as TChunkPLTE;
                     for i:=0 to Self.DataSize-1 do
                      if PaletteValues[i]=0 then
                       with PaletteChunk.GetPaletteItem(i) do
                        begin
                         Result:=RGB(rgbRed,rgbGreen,rgbBlue);
                         break;
                        end;
                    end;
  end;
end;

function TChunktRNS.SaveToStream(Stream:TStream):Boolean;
begin
 if DataSize<=256 then
  CopyMemory(fData,@PaletteValues[0],DataSize);
 Result:=inherited SaveToStream(Stream);
end;

procedure TChunktRNS.Assign(Source:TChunk);
begin
 CopyMemory(@PaletteValues[0],@TChunkTrns(Source).PaletteValues[0],256);
 fBitTransparency:=TChunkTrns(Source).fBitTransparency;
 inherited Assign(Source);
end;

function TChunktRNS.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean;
var i,Differ255:Integer;
begin
 Result:=inherited LoadFromStream(Stream,ChunkName,Size);
 if not Result then exit;
 if Size>256 then Owner.RaiseError(EPNGInvalidPalette,EPNGInvalidPaletteText);
 FillChar(PaletteValues[0],256,255);
 CopyMemory(@PaletteValues[0],fData,Size);
 case Header.ColorType of
  COLOR_RGB,
  COLOR_GRAYSCALE: fBitTransparency:=True;
  COLOR_PALETTE:   begin
                    Differ255:=0;
                    for i:=0 to Size-1 do
                     if PaletteValues[i]<>255 then Inc(Differ255);
                    fBitTransparency:=(Differ255=1);
                   end;
 end;
end;

procedure TChunkIDAT.PreparePalette;
var Entries:Word;
          J:Integer;
begin
 with Header do
  if (ColorType=COLOR_GRAYSCALE) or (ColorType=COLOR_GRAYSCALEALPHA) then
   begin
    Entries:=(1 shl Byte(BitmapInfo.bmiHeader.biBitCount));
    for J:=0 to Entries-1 do
     with BitmapInfo.bmiColors[j] do
      begin
       rgbRed:=fOwner.GammaTable[MulDiv(j,255,Entries-1)];
       rgbGreen:=rgbRed;
       rgbBlue:=rgbRed;
      end;
   end;
end;

function TChunkIDAT.IDATZlibRead(var ZLIBStream:TZStreamRec2;
  Buffer:Pointer;Count:Integer;var EndPos:Integer;
  var crcfile:Cardinal):Integer;
var
  ProcResult:Integer;
  IDATHeader:array[0..3]of Char;
  IDATCRC:Cardinal;
begin
  with ZLIBStream,ZLIBStream.zlib do
  begin
    next_out:=Buffer;
    avail_out:=Count;
    while avail_out>0 do
    begin
      if (fStream.Position=EndPos) and (avail_out>0) and
        (avail_in=0) then
      begin
        fStream.Read(IDATCRC,4);

        if crcfile xor $ffffffff<>Cardinal(ByteSwap(IDATCRC)) then
        begin
          Result:=-1;
          Owner.RaiseError(EPNGInvalidCRC,EPNGInvalidCRCText);
          exit;
        end;
        fStream.Read(EndPos,4);
        fStream.Read(IDATHeader[0],4);
        if IDATHeader<>'IDAT' then
        begin
          Owner.RaiseError(EPNGMissingMultipleIDAT,EPNGMissingMultipleIDATText);
          result:=-1;
          exit;
        end;
        crcfile:=update_crc($ffffffff,@IDATHeader[0],4);
        EndPos:=fStream.Position+ByteSwap(EndPos);
      end;
      if avail_in=0 then
      begin
        if fStream.Position+ZLIBAllocate>EndPos then
          avail_in:=fStream.Read(Data^,EndPos-fStream.Position)
         else
          avail_in:=fStream.Read(Data^,ZLIBAllocate);
        crcfile:=update_crc(crcfile,Data,avail_in);
        if avail_in=0 then
        begin
          Result:=Count-avail_out;
          exit;
        end;
        next_in:=Data;
      end;
      ProcResult:=inflate(zlib,0);
      if (ProcResult<0) then
      begin
        Result:=-1;
        Owner.RaiseError(EPNGZLIBError,
          EPNGZLIBErrorText+zliberrors[procresult]);
        exit;
      end;
    end;
  end;
  Result:=Count;
end;

const   RowStart:array[0..6]of Integer=(0,0,4,0,2,0,1);
     ColumnStart:array[0..6]of Integer=(0,4,0,2,0,1,0);
    RowIncrement:array[0..6]of Integer=(8,8,8,4,4,2,2);
 ColumnIncrement:array[0..6]of Integer=(8,8,4,4,2,2,1);

procedure TChunkIDAT.CopyInterlacedRGB8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col*3);
 repeat
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+1)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
  Inc(Src,3);
  Inc(Dest,ColumnIncrement[Pass]*3-3);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedRGB16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col*3);
 repeat
  Byte(Dest^):=Owner.GammaTable[PByte(Longint(Src)+4)^]; Inc(Dest);
  Byte(Dest^):=Owner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=Owner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
  Inc(Src,6);
  Inc(Dest,ColumnIncrement[Pass]*3-3);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedPalette148(const Pass:Byte;Src,Dest,Trans:PChar);
const BitTable:array[1..8]of Integer=($1,$3,0,$F,0,0,0,$FF);
      StartBit:array[1..8]of Integer=(7,0,0,4, 0,0,0,0);
var CurBit,Col:Integer;
         Dest2:PChar;
begin
 Col:=ColumnStart[Pass];
 repeat
  CurBit:=StartBit[Header.BitDepth];
  repeat
   Dest2:=PChar(Longint(Dest)+(Header.BitDepth*Col) div 8);
   Byte(Dest2^):=Byte(Dest2^) or (((Byte(Src^) shr CurBit) and BitTable[Header.BitDepth]) shl
       (StartBit[Header.BitDepth]-(Col*Header.BitDepth mod 8)));
   Inc(Col,ColumnIncrement[Pass]);
   Dec(CurBit,Header.BitDepth);
  until CurBit<0;
  Inc(Src);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedPalette2(const Pass:Byte;Src,Dest,Trans:PChar);
var CurBit,Col:Integer;
         Dest2:PChar;
begin
 Col:=ColumnStart[Pass];
 repeat
  CurBit:=6;
  repeat
   Dest2:=PChar(Longint(Dest)+Col div 2);
   Byte(Dest2^):=Byte(Dest2^) or (((Byte(Src^) shr CurBit) and $3) shl (4-(4*Col) mod 8));
   Inc(Col,ColumnIncrement[Pass]);
   Dec(CurBit,2);
  until CurBit<0;
  Inc(Src);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedGray2(const Pass:Byte;Src,Dest,Trans:PChar);
var CurBit,Col:Integer;
         Dest2:PChar;
begin
 Col:=ColumnStart[Pass];
 repeat
  CurBit:=6;
  repeat
   Dest2:=PChar(Longint(Dest)+Col div 2);
   Byte(Dest2^):=Byte(Dest2^) or ((((Byte(Src^) shr CurBit) shl 2) and $F) shl (4-(Col*4) mod 8));
   Inc(Col,ColumnIncrement[Pass]);
   Dec(CurBit,2);
  until CurBit<0;
  Inc(Src);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedGrayscale16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col);
 repeat
  Dest^:=Src^; Inc(Dest);
  Inc(Src,2);
  Inc(Dest,ColumnIncrement[Pass]-1);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedRGBAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col*3);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Trans^:=PChar(Longint(Src)+3)^;
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+1)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
  Inc(Src,4);
  Inc(Dest,ColumnIncrement[Pass]*3-3);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedRGBAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col*3);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Trans^:=PChar(Longint(Src)+6)^;
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+4)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
  Inc(Src,8);
  Inc(Dest,ColumnIncrement[Pass]*3-3);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Dest^:=Src^; Inc(Src);
  Trans^:=Src^; Inc(Src);
  Inc(Dest,ColumnIncrement[Pass]);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Dest:=PChar(Longint(Dest)+Col);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Dest^:=Src^; Inc(Src,2);
  Trans^:=Src^; Inc(Src,2);
  Inc(Dest,ColumnIncrement[Pass]);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.DecodeInterlacedAdam7(Stream:TStream;
  var ZLIBStream:TZStreamRec2;const Size:Integer;var crcfile:Cardinal);
var
  CurrentPass:Byte;
  PixelsThisRow:Integer;
  CurrentRow:Integer;
  Trans,Data:PChar;
  CopyProc:procedure(const Pass:Byte;Src,Dest,
    Trans:PChar) of object;
begin
  CopyProc:=nil;
  case Header.ColorType of
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc:=CopyInterlacedRGB8;
       16: CopyProc:=CopyInterlacedRGB16;
      end;
    COLOR_PALETTE,COLOR_GRAYSCALE:
      case Header.BitDepth of
        1,4,8:CopyProc:=CopyInterlacedPalette148;
        2:if Header.ColorType=COLOR_PALETTE then
                   CopyProc:=CopyInterlacedPalette2
                 else
                   CopyProc:=CopyInterlacedGray2;
        16:CopyProc:=CopyInterlacedGrayscale16;
      end;
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8: CopyProc:=CopyInterlacedRGBAlpha8;
       16: CopyProc:=CopyInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8: CopyProc:=CopyInterlacedGrayscaleAlpha8;
       16: CopyProc:=CopyInterlacedGrayscaleAlpha16;
      end;
  end;
  for CurrentPass:=0 to 6 do
  begin
    PixelsThisRow:=(ImageWidth-ColumnStart[CurrentPass]+
      ColumnIncrement[CurrentPass]-1) div ColumnIncrement[CurrentPass];
    Row_Bytes:=BytesForPixels(PixelsThisRow,Header.ColorType,
      Header.BitDepth);
    ZeroMemory(Row_Buffer[not RowUsed],Row_Bytes);
    CurrentRow:=RowStart[CurrentPass];
    Data:=Ptr(Longint(Header.ImageData)+Header.BytesPerRow *
      (ImageHeight-1-CurrentRow));
    Trans:=Ptr(Longint(Header.ImageAlpha)+ImageWidth*CurrentRow);
    if Row_Bytes>0 then
      while CurrentRow<ImageHeight do
      begin
        if IDATZlibRead(ZLIBStream,@Row_Buffer[RowUsed][0],Row_Bytes+1,
          EndPos,CRCFile)=0 then break;
        FilterRow;
        CopyProc(CurrentPass,@Row_Buffer[RowUsed][1],Data,Trans);
        RowUsed:=not RowUsed;
        Inc(CurrentRow,RowIncrement[CurrentPass]);
        Dec(Data,RowIncrement[CurrentPass]*Header.BytesPerRow);
        Inc(Trans,RowIncrement[CurrentPass]*ImageWidth);
      end;
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedRGB8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+1)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
   Inc(Src,3);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedRGB16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+4)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
   Inc(Src,6);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedPalette148(Src,Dest,Trans:PChar);
begin
 CopyMemory(Dest,Src,Row_Bytes);
end;

procedure TChunkIDAT.CopyNonInterlacedGray2(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to Row_Bytes do
  begin
   Byte(Dest^):=((Byte(Src^) shr 2) and $F) or ((Byte(Src^)) and $F0); Inc(Dest);
   Byte(Dest^):=((Byte(Src^) shl 2) and $F) or ((Byte(Src^) shl 4) and $F0); Inc(Dest);
   Inc(Src);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedPalette2(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to Row_Bytes do
  begin
   Byte(Dest^):=((Byte(Src^) shr 4) and $3) or ((Byte(Src^) shr 2) and $30); Inc(Dest);
   Byte(Dest^):=(Byte(Src^) and $3) or ((Byte(Src^) shl 2) and $30); Inc(Dest);
   Inc(Src);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedGrayscale16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Dest^:=Src^; Inc(Dest);
   Inc(Src,2);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedRGBAlpha8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Trans^:=PChar(Longint(Src)+3)^;
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+1)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
   Inc(Src,4); Inc(Trans);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedRGBAlpha16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Trans^:=PChar(Longint(Src)+6)^;
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+4)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=fOwner.GammaTable[PByte(Longint(Src))^]; Inc(Dest);
   Inc(Src,8);Inc(Trans);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Dest^:=Src^; Inc(Src);
   Trans^:=Src^; Inc(Src);
   Inc(Dest); Inc(Trans);
  end;
end;

procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Dest^:=Src^; Inc(Src,2);
   Trans^:=Src^; Inc(Src,2);
   Inc(Dest); Inc(Trans);
  end;
end;

procedure TChunkIDAT.DecodeNonInterlaced(Stream:TStream;
  var ZLIBStream:TZStreamRec2;const Size:Integer;var crcfile:Cardinal);
var
  j:Cardinal;
  Trans,Data:PChar;
  CopyProc:procedure(
    Src,Dest,Trans:PChar) of object;
begin
  CopyProc:=nil;
  case Header.ColorType of
    COLOR_RGB:
      case Header.BitDepth of
        8:CopyProc:=CopyNonInterlacedRGB8;
       16:CopyProc:=CopyNonInterlacedRGB16;
      end;
    COLOR_PALETTE,COLOR_GRAYSCALE:
      case Header.BitDepth of
        1,4,8:CopyProc:=CopyNonInterlacedPalette148;
        2:if Header.ColorType=COLOR_PALETTE then
                   CopyProc:=CopyNonInterlacedPalette2
                 else
                   CopyProc:=CopyNonInterlacedGray2;
        16:CopyProc:=CopyNonInterlacedGrayscale16;
      end;
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8:CopyProc:=CopyNonInterlacedRGBAlpha8;
       16:CopyProc:=CopyNonInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8:CopyProc:=CopyNonInterlacedGrayscaleAlpha8;
       16:CopyProc:=CopyNonInterlacedGrayscaleAlpha16;
      end;
  end;
  Longint(Data):=Longint(Header.ImageData)+
    Header.BytesPerRow*(ImageHeight-1);
  Trans:=Header.ImageAlpha;
  for j:=0 to ImageHeight-1 do
  begin
    if IDATZlibRead(ZLIBStream,@Row_Buffer[RowUsed][0],Row_Bytes+1,EndPos,
      CRCFile)=0 then break;
    FilterRow;
    CopyProc(@Row_Buffer[RowUsed][1],Data,Trans);
    RowUsed:=not RowUsed;
    Dec(Data,Header.BytesPerRow);
    Inc(Trans,ImageWidth);
  end;
end;

procedure TChunkIDAT.FilterRow;
var           pp:Byte;
         vv,left:Integer;
 above,aboveleft:Integer;
             Col:Cardinal;
begin
  case Row_Buffer[RowUsed]^[0]of
    FILTER_NONE:begin end;
    FILTER_SUB:
      for Col:=Offset+1 to Row_Bytes do
        Row_Buffer[RowUsed][Col]:=(Row_Buffer[RowUsed][Col]+
          Row_Buffer[RowUsed][Col-Offset]) and 255;
    FILTER_UP:
      for Col:=1 to Row_Bytes do
        Row_Buffer[RowUsed][Col]:=(Row_Buffer[RowUsed][Col]+
          Row_Buffer[not RowUsed][Col]) and 255;
    FILTER_AVERAGE:
      for Col:=1 to Row_Bytes do
      begin
        above:=Row_Buffer[not RowUsed][Col];
        if col-1<Offset then
          left:=0
        else
          Left:=Row_Buffer[RowUsed][Col-Offset];
        Row_Buffer[RowUsed][Col]:=(Row_Buffer[RowUsed][Col]+
          (left+above) div 2) and 255;
      end;
    FILTER_PAETH:
    begin
      left:=0;
      aboveleft:=0;
      for Col:=1 to Row_Bytes do
      begin
        above:=Row_Buffer[not RowUsed][Col];
        if (col-1>=offset) then
        begin
          left:=row_buffer[RowUsed][col-offset];
          aboveleft:=row_buffer[not RowUsed][col-offset];
        end;
        vv:=row_buffer[RowUsed][Col];
        pp:=PaethPredictor(left,above,aboveleft);
        Row_Buffer[RowUsed][Col]:=(pp+vv) and $FF;
      end;
    end;

  end;
end;

function TChunkIDAT.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean;
var    ZLIBStream:TZStreamRec2;
 CRCCheck,CRCFile:Cardinal;
begin
 Header:=Owner.Chunks.Item[0] as TChunkIHDR;
 if Header.HasPalette then PreparePalette;
 ImageWidth:=Header.Width;
 ImageHeight:=Header.Height;
 CRCFile:=update_crc($ffffffff,@ChunkName[0],4);
 Owner.GetPixelInfo(Row_Bytes,Offset);
 ZLIBStream:=ZLIBInitInflate(Stream);
 EndPos:=Stream.Position+Size;
 GetMem(Row_Buffer[False],Row_Bytes+1);
 GetMem(Row_Buffer[True],Row_Bytes+1);
 ZeroMemory(Row_Buffer[False],Row_bytes+1);
 RowUsed:=True;
 case Owner.InterlaceMethod of
  imNone:  DecodeNonInterlaced(stream,ZLIBStream,Size,crcfile);
  imAdam7: DecodeInterlacedAdam7(stream,ZLIBStream,size,crcfile);
 end;
 ZLIBTerminateInflate(ZLIBStream);
 FreeMem(Row_Buffer[False],Row_Bytes+1);
 FreeMem(Row_Buffer[True],Row_Bytes+1);
 Stream.Read(CRCCheck,4);
 CRCFile:=CRCFile xor $ffffffff;
 CRCCheck:=ByteSwap(CRCCheck);
 Result:=CRCCheck=CRCFile;
 if not Result then
  begin
   Owner.RaiseError(EPNGInvalidCRC,EPNGInvalidCRCText);
   exit;
  end;
end;

const IDATHeader:array[0..3]of Char=('I','D','A','T');
      BUFFER=5;

function TChunkIDAT.SaveToStream(Stream:TStream):Boolean;
var ZLIBStream:TZStreamRec2;
begin
 Header:=Owner.Chunks.Item[0] as TChunkIHDR;
 ImageWidth:=Header.Width;
 ImageHeight:=Header.Height;
 Owner.GetPixelInfo(Row_Bytes,Offset);
 GetMem(Encode_Buffer[BUFFER],Row_Bytes);
 ZeroMemory(Encode_Buffer[BUFFER],Row_Bytes);
 GetMem(Encode_Buffer[FILTER_NONE],Row_Bytes);
 ZeroMemory(Encode_Buffer[FILTER_NONE],Row_Bytes);
 if pfSub in Owner.Filters then
  GetMem(Encode_Buffer[FILTER_SUB],Row_Bytes);
 if pfUp in Owner.Filters then
  GetMem(Encode_Buffer[FILTER_UP],Row_Bytes);
 if pfAverage in Owner.Filters then
  GetMem(Encode_Buffer[FILTER_AVERAGE],Row_Bytes);
 if pfPaeth in Owner.Filters then
  GetMem(Encode_Buffer[FILTER_PAETH],Row_Bytes);
 ZLIBStream:=ZLIBInitDeflate(Stream,Owner.fCompressionLevel,Owner.MaxIdatSize);
 case Owner.InterlaceMethod of
  imNone:  EncodeNonInterlaced(stream,ZLIBStream);
  imAdam7: EncodeInterlacedAdam7(stream,ZLIBStream);
 end;
 ZLIBTerminateDeflate(ZLIBStream);
 FreeMem(Encode_Buffer[BUFFER],Row_Bytes);
 FreeMem(Encode_Buffer[FILTER_NONE],Row_Bytes);
 if pfSub in Owner.Filters then
  FreeMem(Encode_Buffer[FILTER_SUB],Row_Bytes);
 if pfUp in Owner.Filters then
  FreeMem(Encode_Buffer[FILTER_UP],Row_Bytes);
 if pfAverage in Owner.Filters then
  FreeMem(Encode_Buffer[FILTER_AVERAGE],Row_Bytes);
 if pfPaeth in Owner.Filters then
  FreeMem(Encode_Buffer[FILTER_PAETH],Row_Bytes);
 Result:=True;
end;

procedure WriteIDAT(Stream:TStream;Data:Pointer;const Length:Cardinal);
var ChunkLen,CRC:Cardinal;
begin
 ChunkLen:=ByteSwap(Length);
 Stream.Write(ChunkLen,4);
 Stream.Write(IDATHeader[0],4);
 CRC:=update_crc($ffffffff,@IDATHeader[0],4);
 Stream.Write(Data^,Length);
 CRC:=Byteswap(update_crc(CRC,Data,Length) xor $ffffffff);
 Stream.Write(CRC,4);
end;

procedure TChunkIDAT.IDATZlibWrite(var ZLIBStream:TZStreamRec2;Buffer:Pointer;const Length:Cardinal);
begin
 with ZLIBStream,ZLIBStream.ZLIB do
  begin
   next_in:=Buffer;
   avail_in:=Length;
   while avail_in>0 do
    begin
     deflate(ZLIB,Z_NO_FLUSH);
     if avail_out=0 then
      begin
       WriteIDAT(fStream,Data,Owner.MaxIdatSize);
       next_out:=Data;
       avail_out:=Owner.MaxIdatSize;
      end;
    end;
  end;
end;

procedure TChunkIDAT.FinishIDATZlib(var ZLIBStream:TZStreamRec2);
begin
 with ZLIBStream,ZLIBStream.ZLIB do
  begin
   next_in:=nil;
   avail_in:=0;
   while deflate(ZLIB,Z_FINISH)<>Z_STREAM_END do
    begin
     WriteIDAT(fStream,Data,Owner.MaxIdatSize-avail_out);
     next_out:=Data;
     avail_out:=Owner.MaxIdatSize;
    end;
   if avail_out<Owner.MaxIdatSize then
    WriteIDAT(fStream,Data,Owner.MaxIdatSize-avail_out);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedRGB8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest);
   Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest);
   Inc(Src,3);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedRGB16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   PWord(Dest)^:=fOwner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest,2);
   PWord(Dest)^:=fOwner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest,2);
   PWord(Dest)^:=fOwner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest,2);
   Inc(Src,3);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedPalette148(Src,Dest,Trans:PChar);
begin
 CopyMemory(Dest,Src,Row_Bytes);
end;

procedure TChunkIDAT.EncodeNonInterlacedGrayscale16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   PWord(Dest)^:=PByte(Longint(Src))^; Inc(Dest,2);
   Inc(Src);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest);
   Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest);
   Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest);
   Dest^:=Trans^; Inc(Dest);
   Inc(Src,3); Inc(Trans);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest,2);
   PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest,2);
   PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest,2);
   PWord(Dest)^:=PByte(Longint(Trans))^; Inc(Dest,2);
   Inc(Src,3); Inc(Trans);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha8(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   Dest^:=Src^; Inc(Dest);
   Dest^:=Trans^; Inc(Dest);
   Inc(Src); Inc(Trans);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha16(Src,Dest,Trans:PChar);
var I:Integer;
begin
 for I:=1 to ImageWidth do
  begin
   PWord(Dest)^:=PByte(Src)^; Inc(Dest,2);
   PWord(Dest)^:=PByte(Trans)^; Inc(Dest,2);
   Inc(Src); Inc(Trans);
  end;
end;

procedure TChunkIDAT.EncodeNonInterlaced(Stream:TStream;var ZLIBStream:TZStreamRec2);
var
  j:Cardinal;
  Data,Trans:PChar;
  Filter:Byte;
  CopyProc:procedure(Src,Dest,Trans:PChar) of object;
begin
  CopyProc:=nil;
  case Header.ColorType of
    COLOR_RGB:
      case Header.BitDepth of
        8:CopyProc:=EncodeNonInterlacedRGB8;
       16:CopyProc:=EncodeNonInterlacedRGB16;
      end;
    COLOR_GRAYSCALE,COLOR_PALETTE:
      case Header.BitDepth of
        1,4,8:CopyProc:=EncodeNonInterlacedPalette148;
             16:CopyProc:=EncodeNonInterlacedGrayscale16;
      end;
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8:CopyProc:=EncodeNonInterlacedRGBAlpha8;
         16:CopyProc:=EncodeNonInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8: CopyProc:=EncodeNonInterlacedGrayscaleAlpha8;
       16: CopyProc:=EncodeNonInterlacedGrayscaleAlpha16;
      end;
  end;
  Longint(Data):=Longint(Header.ImageData)+
    Header.BytesPerRow*(ImageHeight-1);
  Trans:=Header.ImageAlpha;
  for j:=0 to ImageHeight-1 do
  begin
    CopyProc(Data,@Encode_Buffer[BUFFER][0],Trans);
    Filter:=FilterToEncode;
    IDATZlibWrite(ZLIBStream,@Filter,1);
    IDATZlibWrite(ZLIBStream,@Encode_Buffer[Filter][0],Row_Bytes);
    Dec(Data,Header.BytesPerRow);
    Inc(Trans,ImageWidth);
  end;
  FinishIDATZlib(ZLIBStream);
end;

procedure TChunkIDAT.EncodeInterlacedRGB8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col*3);
 repeat
  Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest);
  Byte(Dest^):=fOwner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest);
  Inc(Src,ColumnIncrement[Pass]*3);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedRGB16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col*3);
 repeat
  PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest,2);
  PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest,2);
  PWord(Dest)^:=Owner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest,2);
  Inc(Src,ColumnIncrement[Pass]*3);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedPalette148(const Pass:Byte;Src,Dest,Trans:PChar);
const BitTable:array[1..8]of Integer=($1,$3,0,$F,0,0,0,$FF);
      StartBit:array[1..8]of Integer=(7,0,0,4,0,0,0,0);
var CurBit,Col:Integer;
          Src2:PChar;
begin
 FillChar(Dest^,Row_Bytes,#0);
 Col:=ColumnStart[Pass];
 with Header.BitmapInfo.bmiHeader do
  repeat
   CurBit:=StartBit[biBitCount];
   repeat
    Src2:=PChar(Longint(Src)+(biBitCount*Col) div 8);
    Byte(Dest^):=Byte(Dest^) or (((Byte(Src2^) shr (StartBit[Header.BitDepth]-
                 (biBitCount*Col) mod 8))) and (BitTable[biBitCount])) shl CurBit;
    Inc(Col,ColumnIncrement[Pass]);
    Dec(CurBit,biBitCount);
   until CurBit<0;
   Inc(Dest);
  until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedGrayscale16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col);
 repeat
  PWord(Dest)^:=Byte(Src^); Inc(Dest,2);
  Inc(Src,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedRGBAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col*3);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src)+2)^]; Inc(Dest);
  Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src)+1)^]; Inc(Dest);
  Byte(Dest^):=Owner.InverseGamma[PByte(Longint(Src))^]; Inc(Dest);
  Dest^:=Trans^;Inc(Dest);
  Inc(Src,ColumnIncrement[Pass]*3);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedRGBAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col*3);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  PWord(Dest)^:=PByte(Longint(Src)+2)^; Inc(Dest,2);
  PWord(Dest)^:=PByte(Longint(Src)+1)^; Inc(Dest,2);
  PWord(Dest)^:=PByte(Longint(Src))^; Inc(Dest,2);
  PWord(Dest)^:=PByte(Trans)^; Inc(Dest,2);
  Inc(Src,ColumnIncrement[Pass]*3);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha8(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  Dest^:=Src^; Inc(Dest);
  Dest^:=Trans^; Inc(Dest);
  Inc(Src,ColumnIncrement[Pass]);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha16(const Pass:Byte;Src,Dest,Trans:PChar);
var Col:Integer;
begin
 Col:=ColumnStart[Pass];
 Src:=PChar(Longint(Src)+Col);
 Trans:=PChar(Longint(Trans)+Col);
 repeat
  PWord(Dest)^:=PByte(Src)^; Inc(Dest,2);
  PWord(Dest)^:=PByte(Trans)^; Inc(Dest,2);
  Inc(Src,ColumnIncrement[Pass]);
  Inc(Trans,ColumnIncrement[Pass]);
  Inc(Col,ColumnIncrement[Pass]);
 until Col>=ImageWidth;
end;

procedure TChunkIDAT.EncodeInterlacedAdam7(Stream:TStream;
  var ZLIBStream:TZStreamRec2);
var
  CurrentPass,Filter:Byte;
  PixelsThisRow:Integer;
  CurrentRow:Integer;
  Trans,Data:PChar;
  CopyProc:procedure(const Pass:Byte;
    Src,Dest,Trans:PChar) of object;
begin
  CopyProc:=nil;
  case Header.ColorType of
    COLOR_RGB:
      case Header.BitDepth of
        8:CopyProc:=EncodeInterlacedRGB8;
       16:CopyProc:=EncodeInterlacedRGB16;
      end;
    COLOR_PALETTE,COLOR_GRAYSCALE:
      case Header.BitDepth of
        1,4,8:CopyProc:=EncodeInterlacedPalette148;
             16:CopyProc:=EncodeInterlacedGrayscale16;
      end;
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8:CopyProc:=EncodeInterlacedRGBAlpha8;
         16:CopyProc:=EncodeInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
          8:CopyProc:=EncodeInterlacedGrayscaleAlpha8;
         16:CopyProc:=EncodeInterlacedGrayscaleAlpha16;
      end;
  end;
  for CurrentPass:=0 to 6 do
  begin
    PixelsThisRow:=(ImageWidth-ColumnStart[CurrentPass]+
      ColumnIncrement[CurrentPass]-1) div ColumnIncrement[CurrentPass];
    Row_Bytes:=BytesForPixels(PixelsThisRow,Header.ColorType,
      Header.BitDepth);
    ZeroMemory(Encode_Buffer[FILTER_NONE],Row_Bytes);

    CurrentRow:=RowStart[CurrentPass];
    Data:=Ptr(Longint(Header.ImageData)+Header.BytesPerRow *
      (ImageHeight-1-CurrentRow));
    Trans:=Ptr(Longint(Header.ImageAlpha)+ImageWidth*CurrentRow);
    if Row_Bytes>0 then
      while CurrentRow<ImageHeight do
      begin
        CopyProc(CurrentPass,Data,@Encode_Buffer[BUFFER][0],Trans);
        Filter:=FilterToEncode;
        IDATZlibWrite(ZLIBStream,@Filter,1);
        IDATZlibWrite(ZLIBStream,@Encode_Buffer[Filter][0],Row_Bytes);
        Inc(CurrentRow,RowIncrement[CurrentPass]);
        Dec(Data,RowIncrement[CurrentPass]*Header.BytesPerRow);
        Inc(Trans,RowIncrement[CurrentPass]*ImageWidth);
      end;
  end;
  FinishIDATZlib(ZLIBStream);
end;

function TChunkIDAT.FilterToEncode:Byte;
var Run,LongestRun,ii,jj:Cardinal;
    Last,Above,LastAbove:Byte;
begin
 if pfSub in Owner.Filters then
  for ii:=0 to Row_Bytes-1 do
   begin
    if (ii>=Offset) then
     last:=Encode_Buffer[BUFFER]^[ii-Offset] else last:=0;
    Encode_Buffer[FILTER_SUB]^[ii]:=Encode_Buffer[BUFFER]^[ii]-last;
   end;
 if pfUp in Owner.Filters then
  for ii:=0 to Row_Bytes-1 do
   Encode_Buffer[FILTER_UP]^[ii]:=Encode_Buffer[BUFFER]^[ii]-Encode_Buffer[FILTER_NONE]^[ii];
 if pfAverage in Owner.Filters then
  for ii:=0 to Row_Bytes-1 do
   begin
    if (ii>=Offset) then
     last:=Encode_Buffer[BUFFER]^[ii-Offset] else last:=0;
    above:=Encode_Buffer[FILTER_NONE]^[ii];
    Encode_Buffer[FILTER_AVERAGE]^[ii]:=Encode_Buffer[BUFFER]^[ii]-(above+last) div 2 ;
   end;
 if pfPaeth in Owner.Filters then
  begin
   last:=0;
   lastabove:=0;
   for ii:=0 to Row_Bytes-1 do
    begin
     if (ii>=Offset) then
      begin
       last:=Encode_Buffer[BUFFER]^[ii-Offset];
       lastabove:=Encode_Buffer[FILTER_NONE]^[ii-Offset];
      end;
     above:=Encode_Buffer[FILTER_NONE]^[ii];
     Encode_Buffer[FILTER_PAETH]^[ii]:=Encode_Buffer[BUFFER]^[ii]-PaethPredictor(last,above,lastabove);
    end;
  end;
 CopyMemory(@Encode_Buffer[FILTER_NONE]^[0],@Encode_Buffer[BUFFER]^[0],Row_Bytes);
 if (Owner.Filters=[pfNone]) or (Owner.Filters=[]) then
  begin
   Result:=FILTER_NONE;
   exit;
  end;
 LongestRun:=0;Result:=FILTER_NONE;
 for ii:=FILTER_NONE to FILTER_PAETH do
  if TFilter(ii) in Owner.Filters then
   begin
    Run:=0;
    if Owner.Filters=[TFilter(ii)] then
     begin
      Result:=ii;
      exit;
     end;
    for jj:=2 to Row_Bytes-1 do
     if (Encode_Buffer[ii]^[jj]=Encode_Buffer [ii]^[jj-1]) or
        (Encode_Buffer[ii]^[jj]=Encode_Buffer [ii]^[jj-2]) then Inc(Run);
    if (Run>LongestRun) then
     begin
      Result:=ii;
      LongestRun:=Run;
     end;
   end;
end;

function TChunkPLTE.GetPaletteItem(Index:Byte):TRGBQuad;
begin
 if Index>Count-1 then
  Owner.RaiseError(EPNGError,EPNGUnknownPalEntryText) else
   Result:=Header.BitmapInfo.bmiColors[Index];
end;

function TChunkPLTE.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean;
type PPalEntry=^PalEntry;
     PalEntry=record
      R,G,B:Byte;
     end;
var     J:Integer;
 PalColor:PPalEntry;
begin
 Result:=inherited LoadFromStream(Stream,ChunkName,Size);
 if not Result then exit;
 if (Size mod 3<>0) or (Size div 3>256) then
  begin
   Result:=False;
   Owner.RaiseError(EPNGInvalidPalette,EPNGInvalidPaletteText);
   exit;
  end;
 fCount:=Size div 3;
 PalColor:=Data;
 for J:=0 to fCount-1 do
  with Header.BitmapInfo.bmiColors[J] do
   begin
    rgbRed:=Owner.GammaTable[PalColor.R];
    rgbGreen:=Owner.GammaTable[PalColor.G];
    rgbBlue:=Owner.GammaTable[PalColor.B];
    rgbReserved:=0;
    Inc(PalColor);
   end;
end;

function TChunkPLTE.SaveToStream(Stream:TStream):Boolean;
var    J:Integer;
 DataPtr:PByte;
begin
 ResizeData(fCount*3);
 DataPtr:=fData;
 with Header do
  for J:=0 to fCount-1 do
   with BitmapInfo.bmiColors[j] do
    begin
     DataPtr^:=Owner.InverseGamma[rgbRed]; Inc(DataPtr);
     DataPtr^:=Owner.InverseGamma[rgbGreen]; Inc(DataPtr);
     DataPtr^:=Owner.InverseGamma[rgbBlue]; Inc(DataPtr);
    end;
 Result:=inherited SaveToStream(Stream);
end;

procedure TChunkPLTE.Assign(Source:TChunk);
begin
 if Source is TChunkPLTE then
  fCount:=TChunkPLTE(Source).fCount else
   Owner.RaiseError(EPNGError,EPNGCannotAssignChunkText);
end;

procedure TChunkgAMA.Assign(Source:TChunk);
begin
 if Source is TChunkgAMA then
  Gamma:=TChunkgAMA(Source).Gamma else
   Owner.RaiseError(EPNGError,EPNGCannotAssignChunkText);
end;

constructor TChunkgAMA.Create(Owner:TPNGObject);
begin
 inherited Create(Owner);
 Gamma:=1;
end;

function TChunkgAMA.GetValue:Cardinal;
begin
 if DataSize<>4 then
  begin
   ResizeData(4);
   Result:=1;
  end else Result:=Cardinal(ByteSwap(PCardinal(Data)^));
end;

function Power(Base,Exponent:Extended):Extended;
begin
 if Exponent=0.0 then
  Result:=1.0 else
 if (Base=0) or (Exponent=0) then Result:=0 else
  Result:=Exp(Exponent*Ln(Base));
end;

function TChunkgAMA.LoadFromStream(Stream:TStream;const ChunkName:TChunkName;Size:Integer):Boolean;
var  I:Integer;
 Value:Cardinal;
begin
 Result:=inherited LoadFromStream(Stream,ChunkName,Size);
 if not Result then exit;
 Value:=Gamma;
 if Value<>0 then
  with Owner do
   for I:=0 to 255 do
    begin
     GammaTable[I]:=Round(Power((I/255),1/(Value/100000*2.2))*255);
     InverseGamma[Round(Power((I/255),1/(Value/100000*2.2))*255)]:=I;
    end
end;

procedure TChunkgAMA.SetValue(const Value:Cardinal);
begin
 if DataSize<>4 then ResizeData(4);
 PCardinal(Data)^:=ByteSwap(Value);
end;

procedure TPNGObject.Assign(Source:TPersistent);
begin
 if Source=nil then ClearChunks else
  if Source is TPNGObject then
   AssignPNG(Source as TPNGObject) else
  if Source is TBitmap then
   with Source as TBitmap do
    AssignHandle(Handle,Transparent,ColorToRGB(TransparentColor)) else
  inherited;
end;

procedure TPNGObject.ClearChunks;
var I:Integer;
begin
 InitializeGamma;
 for I:=0 to Integer(Chunks.Count)-1 do
  TChunk(Chunks.Item[i]).Free;
 Chunks.Count:=0;
end;

constructor TPNGObject.Create;
begin
 inherited Create;
 fFilters:=[pfSub];
 fCompressionLevel:=7;
 fInterlaceMethod:=imNone;
 fMaxIdatSize:=High(Word);
 fChunkList:=TPNGList.Create(Self);
end;

destructor TPNGObject.Destroy;
begin
 ClearChunks;
 fChunkList.Free;
 if TempPalette<>0 then DeleteObject(TempPalette);
 inherited Destroy;
end;

procedure TPNGObject.GetPixelInfo(var LineSize,Offset:Cardinal);
begin
 if HeaderPresent then
  begin
   LineSize:=BytesForPixels(Header.Width,Header.ColorType,Header.BitDepth);
   case Header.ColorType of
    COLOR_GRAYSCALE:      if Header.BitDepth=16 then
                           Offset:=2 else Offset:=1;
    COLOR_PALETTE:        Offset:=1;
    COLOR_RGB:            Offset:=3*Header.BitDepth div 8;
    COLOR_GRAYSCALEALPHA: Offset:=2*Header.BitDepth div 8;
    COLOR_RGBALPHA:       Offset:=4*Header.BitDepth div 8;
    else                  Offset:=0;
   end;
  end else
   begin
    Offset:=0;
    LineSize:=0;
   end;
end;

function TPNGObject.GetHeight:Integer;
begin
 if HeaderPresent then
  Result:=TChunkIHDR(Chunks.Item[0]).Height else Result:=0;
end;

function TPNGObject.GetWidth:Integer;
begin
 if HeaderPresent then
  Result:=Header.Width else Result:=0;
end;

function TPNGObject.GetEmpty:Boolean;
begin
 Result:=(Chunks.Count=0);
end;

procedure TPNGObject.RaiseError(ExceptionClass:ExceptClass;Text:String);
begin
 raise ExceptionClass.Create(Text);
end;

procedure TPNGObject.SetMaxIdatSize(const Value:Integer);
begin
 if Value<High(Word) then
  fMaxIdatSize:=High(Word) else fMaxIdatSize:=Value;
end;

function TPNGObject.GetHeader:TChunkIHDR;
begin
 if (Chunks.Count<>0) and (Chunks.Item[0] is TChunkIHDR) then
  Result:=Chunks.Item[0] as TChunkIHDR else
   begin
    RaiseError(EPNGHeaderNotPresent,EPNGHeaderNotPresentText);
    Result:=nil;
   end
end;

procedure TPNGObject.DrawPartialTrans(DC:HDC;Rect:TRect);

 procedure AdjustRect(var Rect:TRect);
 var T:Integer;
 begin
  if Rect.Right<Rect.Left then
   begin
    T:=Rect.Right;
    Rect.Right:=Rect.Left;
    Rect.Left:=T;
   end;
  if Rect.Bottom<Rect.Top then
   begin
    T:=Rect.Bottom;
    Rect.Bottom:=Rect.Top;
    Rect.Top:=T;
   end;
 end;

type
  TPixelLine=array[Word]of TRGBQuad;
  pPixelLine=^TPixelLine;
const
  BitmapInfoHeader:TBitmapInfoHeader =
    (biSize:sizeof(TBitmapInfoHeader);
     biWidth:100;
     biHeight:100;
     biPlanes:1;
     biBitCount:32;
     biCompression:BI_RGB;
     biSizeImage:0;
     biXPelsPerMeter:0;
     biYPelsPerMeter:0;
     biClrUsed:0;
     biClrImportant:0);
var
  BitmapInfo:TBitmapInfo;
  BufferDC:HDC;
  BufferBits:Pointer;
  OldBitmap,
  BufferBitmap:HBitmap;
  Header:TChunkIHDR;
  TransparencyChunk:TChunktRNS;
  PaletteChunk:TChunkPLTE;
  TransValue,PaletteIndex:Byte;
  CurBit:Integer;
  Data:PByte;
  BytesPerRowDest,
  BytesPerRowSrc,
  BytesPerRowAlpha:Integer;
  ImageSource,ImageSourceOrg,
  AlphaSource:PByteArray;
  ImageData:pPixelLine;
  i,j,i2,j2:Integer;
  W,H:Cardinal;
  Stretch:Boolean;
  FactorX,FactorY:Double;
begin
  if (Rect.Right=Rect.Left) or (Rect.Bottom=Rect.Top) then exit;
  AdjustRect(Rect);
  W:=Rect.Right-Rect.Left;
  H:=Rect.Bottom-Rect.Top;
  Header:=Self.Header;
  Stretch:=(W<>Header.Width) or (H<>Header.Height);
  if Stretch then FactorX:=W / Header.Width else FactorX:=1;
  if Stretch then FactorY:=H / Header.Height else FactorY:=1;
  FillChar(BitmapInfo,sizeof(BitmapInfo),#0);
  BitmapInfoHeader.biWidth:=W;
  BitmapInfoHeader.biHeight:=-Integer(H);
  BitmapInfo.bmiHeader:=BitmapInfoHeader;
  BufferDC:=CreateCompatibleDC(0);
  if (BufferDC=0) then RaiseError(EPNGOutMemory,EPNGOutMemoryText);
  BufferBitmap:=CreateDIBSection(BufferDC,BitmapInfo,DIB_RGB_COLORS,
    BufferBits,0,0);
  if (BufferBitmap=0) or (BufferBits=Nil) then
  begin
    if BufferBitmap<>0 then DeleteObject(BufferBitmap);
    DeleteDC(BufferDC);
    RaiseError(EPNGOutMemory,EPNGOutMemoryText);
  end;
  OldBitmap:=SelectObject(BufferDC,BufferBitmap);
  BitBlt(BufferDC,0,0,W,H,DC,Rect.Left,Rect.Top,SRCCOPY);
  BytesPerRowAlpha:=Header.Width;
  BytesPerRowDest:=(((BitmapInfo.bmiHeader.biBitCount*W)+31)
    and not 31) div 8;
  BytesPerRowSrc:=(((Header.BitmapInfo.bmiHeader.biBitCount*Header.Width)+
    31) and not 31) div 8;
  ImageData:=BufferBits;
  AlphaSource:=Header.ImageAlpha;
  Longint(ImageSource):=Longint(Header.ImageData)+
    Header.BytesPerRow*Longint(Header.Height-1);
  ImageSourceOrg:=ImageSource;

  case Header.BitmapInfo.bmiHeader.biBitCount of
    24:
      for j:=1 to H do
      begin
        for i:=0 to W-1 do
        begin
          if Stretch then i2:=trunc(i / FactorX) else i2:=i;
          if (AlphaSource[i2]<>0) then
            if (AlphaSource[i2]=255) then
              ImageData[i]:=pRGBQuad(@ImageSource[i2*3])^
            else
              with ImageData[i] do
              begin
                rgbRed:=(255+ImageSource[2+i2*3]*AlphaSource[i2]+rgbRed *
                  (not AlphaSource[i2])) shr 8;
                rgbGreen:=(255+ImageSource[1+i2*3]*AlphaSource[i2]+
                  rgbGreen*(not AlphaSource[i2])) shr 8;
                rgbBlue:=(255+ImageSource[i2*3]*AlphaSource[i2]+rgbBlue *
                 (not AlphaSource[i2])) shr 8;
            end;
          end;

        Inc(Longint(ImageData),BytesPerRowDest);
        if Stretch then j2:=trunc(j / FactorY) else j2:=j;
        Longint(ImageSource):=Longint(ImageSourceOrg)-BytesPerRowSrc*j2;
        Longint(AlphaSource):=Longint(Header.ImageAlpha)+
          BytesPerRowAlpha*j2;
      end;
    1,4,8:if Header.ColorType=COLOR_GRAYSCALEALPHA then
      for j:=1 to H do
      begin
        for i:=0 to W-1 do
          with ImageData[i],Header.BitmapInfo do begin
            if Stretch then i2:=trunc(i / FactorX) else i2:=i;
            rgbRed:=(255+ImageSource[i2]*AlphaSource[i2]+
              rgbRed*(255-AlphaSource[i2])) shr 8;
            rgbGreen:=(255+ImageSource[i2]*AlphaSource[i2]+
              rgbGreen*(255-AlphaSource[i2])) shr 8;
            rgbBlue:=(255+ImageSource[i2]*AlphaSource[i2]+
              rgbBlue*(255-AlphaSource[i2])) shr 8;
          end;

        Longint(ImageData):=Longint(ImageData)+BytesPerRowDest;
        if Stretch then j2:=trunc(j / FactorY) else j2:=j;
        Longint(ImageSource):=Longint(ImageSourceOrg)-BytesPerRowSrc*j2;
        Longint(AlphaSource):=Longint(Header.ImageAlpha)+
          BytesPerRowAlpha*j2;
      end
    else
    begin
      TransparencyChunk:=TChunktRNS(Chunks.ItemFromClass(TChunktRNS));
      PaletteChunk:=TChunkPLTE(Chunks.ItemFromClass(TChunkPLTE));

      for j:=1 to H do
      begin
        i:=0;
        repeat
          CurBit:=0;
          if Stretch then i2:=trunc(i / FactorX) else i2:=i;
          Data:=@ImageSource[i2];

          repeat
            case Header.BitDepth of
              1:PaletteIndex:=(Data^ shr (7-(I Mod 8))) and 1;
            2,4:PaletteIndex:=(Data^ shr ((1-(I Mod 2))*4)) and $0F;
             else PaletteIndex:=Data^;
            end;
            with ImageData[i] do
            begin
              TransValue:=TransparencyChunk.PaletteValues[PaletteIndex];
              rgbRed:=(255+PaletteChunk.Item[PaletteIndex].rgbRed *
                 TransValue+rgbRed*(255-TransValue)) shr 8;
              rgbGreen:=(255+PaletteChunk.Item[PaletteIndex].rgbGreen *
                 TransValue+rgbGreen*(255-TransValue)) shr 8;
              rgbBlue:=(255+PaletteChunk.Item[PaletteIndex].rgbBlue *
                 TransValue+rgbBlue*(255-TransValue)) shr 8;
            end;
            Inc(i);Inc(CurBit,Header.BitmapInfo.bmiHeader.biBitCount);
          until CurBit>=8;
        until i>=Integer(W);
        Longint(ImageData):=Longint(ImageData)+BytesPerRowDest;
        if Stretch then j2:=trunc(j / FactorY) else j2:=j;
        Longint(ImageSource):=Longint(ImageSourceOrg)-BytesPerRowSrc*j2;
      end
    end;
  end;
  BitBlt(DC,Rect.Left,Rect.Top,W,H,BufferDC,0,0,SRCCOPY);
  SelectObject(BufferDC,OldBitmap);
  DeleteObject(BufferBitmap);
  DeleteDC(BufferDC);
end;

procedure TPNGObject.Draw(ACanvas:TCanvas;const Rect:TRect);
var
  Header:TChunkIHDR;
begin
  if Empty then exit;
  Header:=Chunks.GetItem(0) as TChunkIHDR;
  case Self.TransparencyMode of
    ptmPartial:
      DrawPartialTrans(ACanvas.Handle,Rect);
    ptmBit:DrawTransparentBitmap(ACanvas.Handle,
      Header.ImageData,Header.BitmapInfo.bmiHeader,
      PBitmapInfo(@Header.BitmapInfo),Rect,
      ColorToRGB(TransparentColor))
    else
    begin
      SetStretchBltMode(ACanvas.Handle,COLORONCOLOR);
      StretchDiBits(ACanvas.Handle,Rect.Left,
        Rect.Top,Rect.Right-Rect.Left,Rect.Bottom-Rect.Top,0,0,
        Header.Width,Header.Height,Header.ImageData,
        PBitmapInfo(@Header.BitmapInfo)^,DIB_RGB_COLORS,SRCCOPY)
    end
  end;
end;

const PNGHeader:array[0..7]of Char=(#137,#80,#78,#71,#13,#10,#26,#10);

procedure TPNGObject.LoadFromStream(Stream:TStream);
var   Header:array[0..7]of Char;
     HasIDAT:Boolean;
  ChunkCount:Cardinal;
 ChunkLength:Cardinal;
   ChunkName:TChunkName;
begin
 ChunkCount:=0;
 ClearChunks;
 Stream.Read(Header[0],8);
 if Header<>PNGHeader then
  begin
   RaiseError(EPNGInvalidFileHeader,EPNGInvalidFileHeaderText);
   exit;
  end;
 HasIDAT:=False;
 Chunks.Count:=10;
 repeat
  Inc(ChunkCount);
  if Chunks.Count<ChunkCount then
   Chunks.Count:=Chunks.Count+10;
  if Stream.Read(ChunkLength,4)=0 then
   begin
    Chunks.Count:=ChunkCount-1;
    RaiseError(EPNGUnexpectedEnd,EPNGUnexpectedEndText);
   end;
  ChunkLength:=ByteSwap(ChunkLength);
  Stream.Read(Chunkname,4);
  if (ChunkCount=1) and (ChunkName<>'IHDR') then
   begin
    Chunks.Count:=ChunkCount-1;
    RaiseError(EPNGIHDRNotFirst,EPNGIHDRNotFirstText);
    exit;
   end;
  if (HasIDAT and (ChunkName='IDAT')) or (ChunkName='cHRM') then
   begin
    Dec(ChunkCount);
    Stream.Seek(ChunkLength+4,soFromCurrent);
    continue;
   end;
  if ChunkName='IDAT' then HasIDAT:=True;
  Chunks.SetItem(ChunkCount-1,CreateClassChunk(Self,ChunkName));
  if (TChunk(Chunks.Item[ChunkCount-1]).ClassType=TChunk) and
     ((Byte(ChunkName[0]) AND $20)=0) and (ChunkName<>'') then
   begin
    Chunks.Count:=ChunkCount;
    RaiseError(EPNGUnknownCriticalChunk,EPNGUnknownCriticalChunkText);
   end;
  try
   if not TChunk(Chunks.Item[ChunkCount-1]).LoadFromStream(Stream,ChunkName,ChunkLength) then break;
  except
   Chunks.Count:=ChunkCount;
   raise;
  end;
 until (ChunkName='IEND');
 Chunks.Count:=ChunkCount;
 if not HasIDAT then
  RaiseError(EPNGNoImageData,EPNGNoImageDataText);
end;

procedure TPNGObject.SetHeight(Value:Integer);
begin
 RaiseError(EPNGError,EPNGCannotChangeSizeText);
end;

procedure TPNGObject.SetWidth(Value:Integer);
begin
 RaiseError(EPNGError,EPNGCannotChangeSizeText);
end;

procedure TPNGObject.SaveToClipboardFormat(var AFormat:Word;var AData:THandle;var APalette:HPalette);
begin
 with TBitmap.Create do
  try
   Width:=Self.Width;
   Height:=Self.Height;
   Self.Draw(Canvas,Rect(0,0,Width,Height));
   SaveToClipboardFormat(AFormat,AData,APalette);
  finally
   Free;
  end;
end;

procedure TPNGObject.LoadFromClipboardFormat(AFormat:Word;AData:THandle;APalette:HPalette);
begin
 with TBitmap.Create do
  try
   LoadFromClipboardFormat(AFormat,AData,APalette);
   Self.AssignHandle(Handle,False,0);
  finally
   Free;
  end;
end;

function TPNGObject.GetTransparent:Boolean;
begin
 Result:=(TransparencyMode<>ptmNone);
end;

procedure TPNGObject.SaveToStream(Stream:TStream);
var j:Integer;
begin
 Stream.Write(PNGHeader[0],8);
 for j:=0 to Chunks.Count-1 do
  Chunks.Item[j].SaveToStream(Stream)
end;

procedure BuildHeader(Header:TChunkIHDR;Handle:HBitmap;Info:pBitmap;HasPalette:Boolean);
var DC:HDC;
begin
 Header.Width:=Info.bmWidth;
 Header.Height:=Abs(Info.bmHeight);
 if Info.bmBitsPixel>=16 then
  Header.BitDepth:=8 else Header.BitDepth:=Info.bmBitsPixel;
 if Info.bmBitsPixel>=16 then
  Header.ColorType:=COLOR_RGB else Header.ColorType:=COLOR_PALETTE;
 Header.CompressionMethod:=0;
 Header.InterlaceMethod:=0;
 Header.PrepareImageData;
 DC:=CreateCompatibleDC(0);
 GetDIBits(DC,Handle,0,Header.Height,Header.ImageData,PBitmapInfo(@Header.BitmapInfo)^,DIB_RGB_COLORS);
 DeleteDC(DC);
end;

procedure TPNGObject.LoadFromResourceName(Instance:HInst;const Name:String);
var ResStream:TResourceStream;
begin
 try
  ResStream:=TResourceStream.Create(Instance,Name,RT_RCDATA);
 except
  RaiseError(EPNGCouldNotLoadResource,EPNGCouldNotLoadResourceText);
  exit;
 end;
 try
  LoadFromStream(ResStream);
 finally
  ResStream.Free;
 end;
end;

procedure TPNGObject.LoadFromResourceID(Instance:HInst;ResID:Integer);
begin
 LoadFromResourceName(Instance,String(ResID));
end;

procedure TPNGObject.AssignTo(Dest:TPersistent);
var DeskDC:HDC;
      TRNS:TChunkTRNS;
begin
 if Dest is TPNGObject then
  TPNGObject(Dest).AssignPNG(Self) else
 if (Dest is TBitmap) and HeaderPresent then
  begin
   DeskDC:=GetDC(0);
   TBitmap(Dest).Handle:=CreateDIBitmap(DeskDC,Header.BitmapInfo.bmiHeader,
      CBM_INIT,Header.ImageData,PBitmapInfo(@Header.BitmapInfo)^,DIB_RGB_COLORS);
   ReleaseDC(0,DeskDC);
   if TransparencyMode=ptmBit then
    begin
     TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
     TBitmap(Dest).TransparentColor:=TRNS.TransparentColor;
     TBitmap(Dest).Transparent:=True;
    end;
  end else inherited AssignTo(Dest);
end;

procedure TPNGObject.AssignHandle(Handle:HBitmap;Transparent:Boolean;TransparentColor:ColorRef);
var BitmapInfo:Windows.TBitmap;
    HasPalette:Boolean;
        Header:TChunkIHDR;
          PLTE:TChunkPLTE;
          IDAT:TChunkIDAT;
          IEND:TChunkIEND;
          TRNS:TChunkTRNS;
begin
 GetObject(Handle,sizeof(BitmapInfo),@BitmapInfo);
 HasPalette:=(BitmapInfo.bmBitsPixel<16);
 ClearChunks;
 Header:=TChunkIHDR.Create(Self);
 if HasPalette then PLTE:=TChunkPLTE.Create(Self) else PLTE:=nil;
 if Transparent then TRNS:=TChunkTRNS.Create(Self) else TRNS:=nil;
 IDAT:=TChunkIDAT.Create(Self);
 IEND:=TChunkIEND.Create(Self);
 TPNGPointerList(Chunks).Add(Header);
 if HasPalette then TPNGPointerList(Chunks).Add(PLTE);
 if Transparent then TPNGPointerList(Chunks).Add(TRNS);
 TPNGPointerList(Chunks).Add(IDAT);
 TPNGPointerList(Chunks).Add(IEND);
 BuildHeader(Header,Handle,@BitmapInfo,HasPalette);
 if HasPalette then PLTE.fCount:=1 shl BitmapInfo.bmBitsPixel;
 if Transparent then TRNS.TransparentColor:=TransparentColor;
end;

procedure TPNGObject.AssignPNG(Source:TPNGObject);
var J:Integer;
begin
 InterlaceMethod:=Source.InterlaceMethod;
 MaxIdatSize:=Source.MaxIdatSize;
 CompressionLevel:=Source.CompressionLevel;
 Filters:=Source.Filters;
 ClearChunks;
 Chunks.Count:=Source.Chunks.Count;
 for J:=0 to Chunks.Count-1 do
 with Source.Chunks do
  begin
   Chunks.SetItem(J,TChunkClass(TChunk(Item[J]).ClassType).Create(Self));
   TChunk(Chunks.Item[J]).Assign(TChunk(Item[J]));
  end;
end;

function TPNGObject.GetAlphaScanline(const LineIndex:Integer):PByteArray;
begin
 with Header do
  if (ColorType=COLOR_RGBALPHA) or (ColorType=COLOR_GRAYSCALEALPHA) then
   Longint(Result):=Longint(ImageAlpha)+(LineIndex*Longint(Width)) else Result:=nil;
end;

function TPNGObject.GetScanline(const LineIndex:Integer):Pointer;
begin
 with Header do
  Longint(Result):=(Longint(ImageData)+((Longint(Height)-1)*BytesPerRow))-(LineIndex*BytesPerRow);
end;

procedure TPNGObject.InitializeGamma;
var A:Integer;
begin
 for A:=0 to 255 do
  begin
   GammaTable[A]:=A;
   InverseGamma[A]:=A;
  end;
end;

function TPNGObject.GetTransparencyMode:TPNGTransparencyMode;
var TRNS:TChunkTRNS;
begin
 with Header do
  begin
   Result:=ptmNone;
   TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
   case ColorType of
    COLOR_RGBALPHA,
    COLOR_GRAYSCALEALPHA: Result:=ptmPartial;
    COLOR_RGB,
    COLOR_GRAYSCALE:      if TRNS<>nil then Result:=ptmBit;
    COLOR_PALETTE:        if TRNS<>nil then
                           if TRNS.BitTransparency then
                            Result:=ptmBit else Result:=ptmPartial
    end;
  end;
end;

procedure TPNGObject.AddtEXt(const Keyword,Text:String);
var TextChunk:TChunkTEXT;
begin
 TextChunk:=Chunks.Add(TChunkText) as TChunkTEXT;
 TextChunk.Keyword:=Keyword;
 TextChunk.Text:=Text;
end;

procedure TPNGObject.AddzTXt(const Keyword,Text:String);
var TextChunk:TChunkzTXt;
begin
 TextChunk:=Chunks.Add(TChunkzTXt) as TChunkzTXt;
 TextChunk.Keyword:=Keyword;
 TextChunk.Text:=Text;
end;

procedure TPNGObject.RemoveTransparency;
var TRNS:TChunkTRNS;
begin
 TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
 if TRNS<>nil then Chunks.RemoveChunk(TRNS)
end;

procedure TPNGObject.CreateAlpha;
var
  TRNS:TChunkTRNS;
begin
 with Header do
  case ColorType of
   COLOR_GRAYSCALE,
   COLOR_RGB:       begin
                     if ColorType=COLOR_GRAYSCALE then
                      ColorType:=COLOR_GRAYSCALEALPHA else
                       ColorType:=COLOR_RGBALPHA;
                     GetMem(ImageAlpha,Integer(Width)*Integer(Height));
                     FillChar(ImageAlpha^,Integer(Width)*Integer(Height),#255);
                    end;
   COLOR_PALETTE:   begin
                     if Chunks.ItemFromClass(TChunkTRNS)=nil then
                      TRNS:=Chunks.Add(TChunkTRNS) as TChunkTRNS else
                     TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
                     with TRNS do
                      begin
                       FillChar(PaletteValues[0],256,255);
                       fDataSize:=1 shl Header.BitDepth;
                       fBitTransparency:=False
                      end;
                    end;
  end;
end;

function TPNGObject.GetTransparentColor:TColor;
var TRNS:TChunkTRNS;
begin
 TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
 if Assigned(TRNS) then Result:=TRNS.TransparentColor else Result:=0
end;

{$OPTIMIZATION OFF}
procedure TPNGObject.SetTransparentColor(const Value:TColor);
var TRNS:TChunkTRNS;
begin
 if HeaderPresent then
  case Header.ColorType of
   COLOR_RGBALPHA,
   COLOR_GRAYSCALEALPHA: Self.RaiseError(EPNGCannotChangeTransparent,EPNGCannotChangeTransparentText);
   COLOR_PALETTE,
   COLOR_RGB,
   COLOR_GRAYSCALE:      begin
                          TRNS:=Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
                          if not Assigned(TRNS) then
                           TRNS:=Chunks.Add(TChunkTRNS) as TChunkTRNS;
                          TRNS.TransparentColor:=ColorToRGB(Value)
                         end;
  end;
end;

function TPNGObject.HeaderPresent:Boolean;
begin
 Result:=((Chunks.Count<>0) and (Chunks.Item[0] is TChunkIHDR))
end;

function GetByteArrayPixel(const PNG:TPNGObject;const X,Y:Integer):TColor;
var ByteData:Byte;
   DataDepth:Byte;
begin
 with PNG,Header do
  begin
   DataDepth:=BitDepth;
   if DataDepth>8 then DataDepth:=8;
   ByteData:=PByteArray(PNG.Scanline[Y])^[X div (8 div DataDepth)];
   ByteData:=(ByteData shr ((8-DataDepth)-(X mod (8 div DataDepth))*DataDepth));
   ByteData:=ByteData and ($FF shr (8-DataDepth));
   case ColorType of
    COLOR_PALETTE:   begin
                      with TChunkPLTE(PNG.Chunks.ItemFromClass(TChunkPLTE)).Item[ByteData] do
                       Result:=RGB(GammaTable[rgbRed],GammaTable[rgbGreen],GammaTable[rgbBlue]);
                     end; 
    COLOR_GRAYSCALE: begin
                      if BitDepth=1 then ByteData:=GammaTable[Byte(ByteData*255)] else
                       ByteData:=GammaTable[Byte(ByteData*((1 shl DataDepth)+1))];
                      Result:=RGB(ByteData,ByteData,ByteData);
                     end;
    else             Result:=0;
   end;
  end;
end;

procedure SetByteArrayPixel(const PNG:TPNGObject;const X,Y:Integer;const Value:TColor);
const ClearFlag:array[1..8]of Integer=(1,3,0,15,0,0,0,$FF);
var ByteData:PByte;
   DataDepth:Byte;
    ValEntry:Byte;
begin
 with PNG.Header do
  begin
   ValEntry:=GetNearestPaletteIndex(PNG.Palette,ColorToRGB(Value));
   DataDepth:=BitDepth;
   if DataDepth>8 then DataDepth:=8;
   ByteData:=@PByteArray(PNG.Scanline[Y])^[X div (8 div DataDepth)];
   ByteData^:=ByteData^ and not (ClearFlag[DataDepth] shl ((8-DataDepth)-
     (X mod (8 div DataDepth))*DataDepth));
   ByteData^:=ByteData^ or (ValEntry shl ((8-DataDepth)-
     (X mod (8 div DataDepth))*DataDepth));
  end;
end;

function GetRGBLinePixel(const PNG:TPNGObject;const X,Y:Integer):TColor;
begin
 with PRGBLine(PNG.Scanline[Y])^[X] do
  Result:=RGB(rgbtRed,rgbtGreen,rgbtBlue)
end;

procedure SetRGBLinePixel(const PNG:TPNGObject;const X,Y:Integer;Value:TColor);
begin
 with PRGBLine(PNG.Scanline[Y])^[X] do
  begin
   rgbtRed:=GetRValue(Value);
   rgbtGreen:=GetGValue(Value);
   rgbtBlue:=GetBValue(Value)
  end
end;

procedure TPNGObject.SetPixels(const X,Y:Integer;const Value:TColor);
begin
 if ((X>=0) and (X<=Width-1)) and
    ((Y>=0) and (Y<=Height-1)) then
  with Header do
   begin
    if ColorType in [COLOR_GRAYSCALE,COLOR_PALETTE] then
     SetByteArrayPixel(Self,X,Y,Value) else
      SetRGBLinePixel(Self,X,Y,Value);
   end;
end;

function TPNGObject.GetPixels(const X,Y:Integer):TColor;
begin
 if ((X>=0) and (X<=Width-1)) and
    ((Y>=0) and (Y<=Height-1)) then
  with Header do
   begin
    if ColorType in [COLOR_GRAYSCALE,COLOR_PALETTE] then
     Result:=GetByteArrayPixel(Self,X,Y) else
      Result:=GetRGBLinePixel(Self,X,Y);
   end else Result:=0;
end;

function TPNGObject.GetPalette:HPALETTE;
var LogPalette:TMaxLogPalette;
             I:Integer;
begin
 if (Header.ColorType in [COLOR_PALETTE,COLOR_GRAYSCALE])  then
  begin
   if TempPalette=0 then
    with LogPalette do
     begin
      palVersion:=$300;
      palNumEntries:=256;
      for I:=0 to LogPalette.palNumEntries-1 do
       begin
        palPalEntry[i].peRed:=Header.BitmapInfo.bmiColors[i].rgbRed;
        palPalEntry[i].peGreen:=Header.BitmapInfo.bmiColors[i].rgbGreen;
        palPalEntry[i].peBlue:=Header.BitmapInfo.bmiColors[i].rgbBlue;
        palPalEntry[i].peFlags:=0;
       end;
      TempPalette:=CreatePalette(pLogPalette(@LogPalette)^);
     end;
  end;
 Result:=TempPalette;
end;

initialization

 ChunkClasses:=nil;
 crc_table_computed:=False;
 RegisterCommonChunks;
 TPicture.RegisterFileFormat('PNG','Portable Network Graphics',TPNGObject);

finalization

 TPicture.UnregisterGraphicClass(TPNGObject);
 FreeChunkClassList;

end.
