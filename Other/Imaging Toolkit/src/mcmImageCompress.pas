// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  17545: mcmImageCompress.pas
//
//    Rev 1.21    2014-02-02 21:09:58  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.20    02-01-2009 18:58:18  mcm    Version: IMG 3.3
// Delphi 2009 Support
//
//    Rev 1.19    08-11-2007 21:23:02  mcm    Version: IMG 3.2
// Fixed compression error in CCITT Group 4.
//
//    Rev 1.18    21-01-2006 10:37:52  mcm    Version: IMG 2.13
// In CheckB1, added a check for b1 being less than MaxCol.
//
//    Rev 1.17    11-12-2005 20:52:14  mcm    Version: IMG 2.11
// Moved SymbolCount calculation from TmcmJPEGImage to TmcmHuffmanJPEG.
//
//    Rev 1.16    30-11-2005 21:38:34  mcm    Version: IMG 2.10
// Added support for TIFF, Modified Huffman, Reverse ordered bits.
//
//   Rev 1.15    28-03-2005 20:28:32  mcm    Version: IMG 2.9
// Corrected PCX decompression for 24 bit images.

//
//   Rev 1.14    20-03-2005 18:49:54  mcm
// Corrected an error reading 4 bit non planar PCX images.
// Yet a correction for CCITT G4.

//
//   Rev 1.13    16-03-2005 22:29:26  mcm    Version: IMG 2.9
// CCITT G4

//
//   Rev 1.12    16-03-2005 22:03:52  mcm    Version: IMG 2.9
// Correction for TIFF CCITT G4 images created by Kodak 3520 scanner.

//
//   Rev 1.11    20-02-2005 13:29:08  mcm
// Corrected an error when decompressing CCITT G4 data where a padding byte (the
// last byte in the decompression buffer) followed an EOL marker.

//
//   Rev 1.10    01-06-2004 00:00:18  mcm    Version: IMG 2.5
// Testing Extension codes.

//
//   Rev 1.9    26-01-2004 23:43:30  mcm    Version: IMG 2.3
// Disabled overflow cheching. Caused a problem when creating a Huffman table
// (CreateWriteTable). 

//
//   Rev 1.8    24-11-2003 20:18:42  mcm

//
//   Rev 1.7    29-09-2003 18:44:34  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.6    25-07-2003 00:10:08  mcm
// Modification required due to change in TmcmHashTable.
// Fixed a memory leak in SGI RLE and PackBits when compressing 32 bit RGBA
// images.

//
//   Rev 1.5    29-01-2003 15:40:48  mcm
// Correction for CCITT Group 4 decompression.

//
//   Rev 1.4    27-01-2003 14:34:06  mcm
// Added support for reading reversed bit order in CCITT Group 3 & 4 huffman
// compressed data. 
// Fixed reading/handling EOL marker at the end of TIFF strip (KODAK's Imaging).
// Added JPEG Huffman coding and decoding. 

//
//   Rev 1.3    27-09-2002 12:57:38  mcm    Version: IMG 1.2
// Optimised RLE encoding for Packbits.
// Added SGI RLE de- and endocing.

//
//   Rev 1.2    07-08-2002 14:26:48  mcm    Version: IMG 1.1
// Added 32 bit support to Packbits.

//
//   Rev 1.1    01-08-2002 11:54:10  mcm    Version: IMG 1.1
// Added TmcmImageCCITT compression class.
//
//   Rev 1.0    27-05-2002 16:22:00  mcm

unit mcmImageCompress;

//------------------------------------------------------------------------------
// Image compression and decompression.
//
// Supported compression schemes
// - RLE 4 Windows Bitmap (Run Length Encoding, 4 bit).
// - RLE 8 Windows Bitmap (Run Length Encoding, 8 bit).
// - PackBits TIFF (Apple Macintosh, byte oriented run-length, 1, 4, 8 and 24 bit).
// - RLE Targa
// - RLE PCX, ZSoft
// - RLE Silicon Graphics Image (SGI).
// - Huffman, Modified Huffman.
// - CCITT Group 3 (1-D & 2-D) and 4.
// - LZ
//------------------------------------------------------------------------------

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT Q+}{$DEFINE OVERFLOW_OFF}{$Q-}{$ENDIF}
// $Q - Overflow checking is disabled as FHuffmanBits[0] = -1 will generate a
//      run-time error EIntOverflow.

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImage, mcmImageTypeDef, mcmHashTable, mcmSlidingWindow, mcmDCT,
     DefHuffman, DefTIFF, DefJPEG, DefPNG;

//------------------------------------------------------------------------------
// Image compression classes.
//------------------------------------------------------------------------------

type
//------------------------------------------------------------------------------
  TmcmImageCompress = class(TPersistent)
  private
  protected
    FDib        : HBITMAP;          // Handle to device independent bitmap.
    FDibBits    : PVectorB;         // Byte pointer to bitmap data.
    FDIBSection : TDIBSection;      // DIB setcion (Windows)
    FImageSize  : longint;          // Bitmap size
    FLongWidth  : longint;          // Bitmap long line width.

    FCBuffer    : PVectorB;         // Byte pointer to compressed data
    FDBuffer    : PVectorB;         // Byte pointer to data to buffer
    FDSize      : longword;         // Compress buffer size.
    FDIncSize   : longword;         // Compress buffer increment size.

    FCol        : longint;          // Column index
    FRow        : longint;          // Row index
    FMaxCol     : longint;          // Maximum column (Width)
    FMaxRow     : longint;          // Maximum row (Height)
    FIndex      : longint;          // Index to bitmap data.
    FError      : TmcmErrorCode;    // Last error.
    FSupColors  : TmcmImageFormats; // Supported colour formats.

    FSwapRB     : boolean;
    FFlipVert   : boolean;          // Flip Image vertically.
    procedure   SetFlipVert(    Value      : boolean); virtual;
    procedure   SetImage   (    Value      : HBITMAP); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   AllocCompressBuffer(MinSize : cardinal);
    procedure   Clear; virtual;
    function    Compress   (var Buffer     : pointer;
                            var BufferSize : longword;
                            var DataCount  : longword) : TmcmErrorCode; virtual; abstract;
    function    Decompress (    Buffer     : pointer;
                            var BufferSize : longword;
                            var DataCount  : longword) : TmcmErrorCode; virtual; abstract;
    procedure   SwapByteRB (    Buffer     : pointer);
    property    FlipVert : boolean
      read      FFlipVert
      write     SetFlipVert;
    property    Image : HBITMAP
      read      FDib
      write     SetImage;
  published
  end;

//------------------------------------------------------------------------------
  TRLECountEqual = function(Buffer : Pointer; Index, MaxIndex : longint) : integer of object;
  TRLECountDiff  = function(Buffer : Pointer; Index, MaxIndex : longint) : integer of object;

  TRLEEncodeRec = record
                  Index : longint;
                  Count : longint;
                  Equal : boolean;
                  end;
  PRLEEncodeRec = ^TRLEEncodeRec;
  TRLEString = array[0..0] of TRLEEncodeRec;
  PRLEString = ^TRLEString;

  TmcmImageRLE  = class(TmcmImageCompress)
  private
    FMinEqual      : integer;
    FOptEqual      : integer;
    FMinDiff       : integer;
    FRLEStr        : PRLEString;
    FRLECountEqual : TRLECountEqual;
    FRLECountDiff  : TRLECountDiff;
  protected
    function    CountEqual4 (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountEqual8 (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountEqual16(    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountEqual24(    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountEqual32(    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountDiff4  (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountDiff8  (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountDiff16 (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountDiff24 (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CountDiff32 (    Buffer   : Pointer;
                                 Index,
                                 MaxIndex : longint) : integer;
    function    CodeRLELine (    RLEStr   : PRLEString;
                                 Buffer   : PVectorB;
                                 Index    : longint;
                                 MaxIndex : longint) : integer;
    procedure   SetImage    (    Value    : HBITMAP); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Clear; override;
  published
  end;

//------------------------------------------------------------------------------
  TmcmImageRLE4 = class(TmcmImageRLE)
  private
  protected
    procedure   SetImage  (    Value      : HBITMAP); override;
  public
    constructor Create; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
  published
  end;


//------------------------------------------------------------------------------
  TmcmImageRLE8 = class(TmcmImageRLE)
  private
  protected
  public
    constructor Create; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
  published
  end;

//------------------------------------------------------------------------------
  TmcmImagePackBits = class(TmcmImageRLE)
  private
    FPackWidth  : cardinal;  // Packed line width.
    FRGBBuffer  : PVectorB;
  protected
    procedure   SetImage  (    Value      : HBITMAP); override;
  public
    constructor Create; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
  published
  end;

//------------------------------------------------------------------------------
  TmcmImageTargaRLE = class(TmcmImageRLE)
  private
    FPackWidth  : cardinal;  // Packed line width.
    FRGBBuffer  : PVectorB;
  protected
    procedure   SetImage  (    Value      : HBITMAP); override;
  public
    constructor Create; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword): TmcmErrorCode; override;
  published
  end;

//------------------------------------------------------------------------------
  TmcmImagePCXRLE = class(TmcmImageCompress)
  private
    FPackWidth  : cardinal;  // Packed line width.
    FPlane      : integer;
    FNoPlanes   : integer;
  protected
    procedure   SetFlipVert(    Value      : boolean); override;
    procedure   SetImage   (    Value      : HBITMAP); override;
  public
    constructor Create; override;
    procedure   Clear; override;
    function    Compress   (var Buffer     : pointer;
                            var BufferSize : longword;
                            var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress (    Buffer     : pointer;
                            var BufferSize : longword;
                            var DataCount  : longword) : TmcmErrorCode; override;
    procedure   SetNoPlanes(NoPlanes : integer);
    procedure   SetPackedLineWidth(Value : cardinal);
  published
  end;

//------------------------------------------------------------------------------
  TmcmImageSGIRLE = class(TmcmImageRLE)
  private
    FPlanes    : integer;
    FPlaneInc  : integer;
    FRGBBuffer : PVectorB;
  protected
    procedure   SetImage   (    Value      : HBITMAP); override;
  public
    constructor Create; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
  published
  end;

//------------------------------------------------------------------------------
  THuffmanNode    = packed record
                    Count      : cardinal;
                    LeftIndex  : cardinal;
                    RightIndex : cardinal;
                    end;
  PHuffmanNode    = ^THuffmanNode;

  THuffmanTree    = array[0..0] of THuffmanNode;
  PHuffmanTree    = ^THuffmanTree;

  // THuffmanCode handles 256 (8 bit) different colors.
  THuffmanCode    = packed record
                    CodeLength : word;                 // Number of bits in code.
                    Code       : array[0..31] of byte; // Huffman code,
                                                       //
                                                       // MSB at index 0 bit 0.
                    end;

  THuffmanCodes   = array[0..0] of THuffmanCode;
  PHuffmanCodes   = ^THuffmanCodes;

  // Place holder for bit string
  THuffmanCodeStr = string[255];

  TmcmImageHuffman = class(TmcmImageCompress)
  // Supports compression/decompression of up to 256 gray levels or colors
  // (8 bit data).
  private
    FNoColors     : integer;       // Color resolution in image.
    FLastIndex    : integer;       // Last node index into binary tree.
    FDistTable    : PVectorC;      // Distance /bit length of huffman codes.
    FHuffmanTree  : PHuffmanTree;  // Binary tree,
    FHuffmanCodes : PHuffmanCodes; // Huffman codes.
    FNodeIndex    : cardinal;      // Used when decompressing
    FCodeBit      : integer;       // Bit position in compressed buffer.
    FCodeByte     : byte;          // Index to next byte in compressed buffer.
  protected
    function    ColourDistribution : TmcmErrorCode;
    function    InitCompress : TmcmErrorCode;
    function    InitDecompress : TmcmErrorCode;
    function    Sort(const Item1, Item2 : pointer) : integer; // TmcmPriorityQueue event method.
    procedure   BuildTree(var LastIndex : integer);
    procedure   BuildCodes(NodeIndex : integer);
    procedure   BuildLeaveCode(NodeIndex : integer; CodeStr : THuffmanCodeStr);
    procedure   ReadBits;
    procedure   SetImage  (    Value       : HBITMAP); override;
    function    WriteBits (    Data        : PVectorB;
                               DataSize    : cardinal;
                               Buffer      : PVectorB;
                           var BufferIndex : cardinal;
                               Align       : TmcmDataAlign) : TmcmErrorCode;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    GetDistribution(var Buffer : pointer;
                                var Size   : longword) : TmcmErrorCode;
    function    SetDistribution(Buffer : pointer;
                                Count  : longword) : TmcmErrorCode;
  published
  end;

//------------------------------------------------------------------------------
// TmcmImageModifiedHuffman
//
// TmcmImageModifiedHuffman is used to encode and decode TIFF Modified Huffman
// RLE images.
// The TmcmImageModifiedHuffman class uses static (pre-defined) Huffmann codes
// and sizes, declared in "DefTIFF.PAS".

  TCodeRun = (CR_WHITE, CR_BLACK, CR_CODE);
  TGetRunLength = function : cardinal of object;

  TmcmImageModifiedHuffman = class(TmcmImageCompress)
  private
    FLastIndex    : longint;       //
    FCodeRun      : TCodeRun;      //
    FWhiteTree    : PHuffmanTree;  // Binary tree for white run-length
    FBlackTree    : PHuffmanTree;  // Binary tree for black run-length
    FCurTree      : array[TCodeRun] of PHuffmanTree;  // Current tree;
    FNodeIndex    : cardinal;      // Used when decompressing

    FFillOrder    : boolean;       // False ->
                                   // True  ->
    FResetBit     : integer;       // If FFillOrder = False -> FResetBit = 0
                                   // else FResetBit = 8.
    FCodeBit      : integer;       // Bit position in de-/compressed buffer.
    FCodeByte     : byte;          // Index to next byte in compressed buffer.
    FCodeIndex    : cardinal;      // The index into the Buffer being decompressed.
    FCodeSize     : cardinal;      // The current BufferSize when decompressing data.
                                   // Must be initialised in the Decompress method.
    FDataBit      : integer;       //
    FDataByte     : byte;          //
    FEOLCount     : integer;       //
    FGetRunLength : TGetRunLength; //
    // FWordAligned  : boolean; // Indicate
  protected
    procedure   SetImage  (    Value      : HBITMAP); override;
    function    InitDecompress : TmcmErrorCode; virtual;
    procedure   FillBits(RunLength : cardinal);
    function    GetRunLengthDown : cardinal;
    function    GetRunLengthUp : cardinal;
    procedure   SetFillOrder(Value : boolean);
    function    WriteRunCodes(var Index       : longint;
                                  RunLength   : cardinal;
                                  MakeUpCodes : array of THuffmanCodeLen;
                                  TermCodes   : array of THuffmanCodeLen) : TmcmErrorCode;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   BuildTree(var LastIndex  : integer;
                              Data       : array of THuffmanCodeLen;
                              Count      : word;
                              TheTree    : PHuffmanTree;
                              Multiplier : word;
                              Offset     : word);
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    property    FillOrder : boolean
      read      FFillOrder
      write     SetFillOrder default False;
  published
  end;


//------------------------------------------------------------------------------
// TmcmImageCCITT
//
// TmcmImageCCITT is used to encode and decode TIFF CCITT Group 3 & 4 images.
// The TmcmImageCCITT class uses static (pre-defined) Huffmann codes and sizes,
// declared in "DefTIFF.PAS".

  TmcmCCITTScheme = (CCITT_MH, CCITT_MR, CCITT_MMR);

  TmcmImageCCITT = class(TmcmImageModifiedHuffman)
  private
    FCCITTScheme  : TmcmCCITTScheme; //
    FScheme       : TmcmCCITTScheme; //
    FByteAlgned   : boolean;  // True  -> Fill bits are added before EOL code
                              //          such that EOL ends on a byte boundary.
                              //          xxxx-0000-0000-0001.
    FUncompressed : boolean;  // True  -> Data is not Huffman compressed.

    FRefLine      : longint;      // Reference line used in 2-D compression.
    FCodeTree     : PHuffmanTree; // Binary tree for Two-dimensional codes.
    Fa0, Fb1      : longint;      //
  protected
    procedure   CheckB1(var b1 : longint; var a0 : longint; Color : byte);
    function    NextTransition(var Col, Row : longint) : longint;

    function    ReadMHLine : cardinal;
    function    ReadMRLine : cardinal;
    procedure   SetByteAlgned(Value : boolean);
    procedure   SetCCITTScheme(Value : TmcmCCITTScheme);
    procedure   SetUncompressed(Value : boolean);
    function    SyncEOL(BufferSize : cardinal) : TmcmErrorCode;
    procedure   WriteEOL(var Index : longint);
    function    WriteMHLine(var Index : longint) : cardinal;
    function    WriteMRLine(var Index : longint) : cardinal;
  public
    constructor Create; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    InitDecompress : TmcmErrorCode; override;

    property    ByteAlgned : boolean
      read      FByteAlgned
      write     SetByteAlgned default False;
    property    CCITTScheme : TmcmCCITTScheme
      read      FCCITTScheme
      write     SetCCITTScheme default CCITT_MH;
    property    Uncompressed : boolean
      read      FUncompressed
      write     SetUncompressed default False;
  published
  end;

//------------------------------------------------------------------------------
  TGetEndianNBits = function(Count : word) : cardinal of object;
  TSetEndianNBits = procedure(Bits : cardinal; Count : word) of object;

  TmcmHuffmanBase = class(TPersistent)
  private
    FError          : TmcmErrorCode;    // Last error.
    FTableDefined   : boolean;
  protected
    FCount          : cardinal; //
    FFrequency      : PVectorC; // Frequency of each value
    FHuffValue      : PVectorC; //
    FHuffSize       : PVectorC; // Size of Huffman code.
    FHuffCode       : PVectorC; // Huffman code.
    FMinCode        : PVectorC; // Smallest Huffman code of lenght n + 1.
    FMaxCode        : PVectorL; // Largest Huffman code of length n + 1.
    FValCode        : PVectorC; // Index into HuffValue. First value having a code
                                // lenght n + 1.
    FMaxCodeLength  : cardinal; // Maximum code length.
    FMaxHuffmanCode : cardinal; // Maximum number of code.
    FGetNBits       : TGetEndianNBits;
    function    GetFrequency(Index : cardinal) : cardinal;
    procedure   SetFrequency(Index : cardinal; Value : cardinal);
    function    GetHuffValue(Index : cardinal) : cardinal;
    procedure   SetHuffValue(Index : cardinal; Value : cardinal);
    function    GetHuffSize(Index : cardinal) : cardinal;
    procedure   SetHuffSize(Index : cardinal; Value : cardinal);
    function    GetHuffCode(Index : cardinal) : cardinal;
    procedure   SetHuffCode(Index : cardinal; Value : cardinal);
    function    GetMinCode(Index : cardinal) : cardinal;
    procedure   SetMinCode(Index : cardinal; Value : cardinal);
    function    GetMaxCode(Index : cardinal) : longint;
    procedure   SetMaxCode(Index : cardinal; Value : longint);
    function    GetValCode(Index : cardinal) : cardinal;
    procedure   SetValCode(Index : cardinal; Value : cardinal);
  public
    constructor Create(MaxNoCodes, MaxCodeLength : cardinal); virtual;
    destructor  Destroy; override;
    procedure   ClearTable; virtual;
    function    CreateReadTable(Count : cardinal) : TmcmErrorCode; virtual;
    function    CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode; virtual;
    procedure   Compress(Value : integer; var Code, Size : cardinal);
    function    Decompress : cardinal;

    property    Frequency[Index : cardinal] : cardinal
      read      GetFrequency
      write     SetFrequency;
    property    HuffValue[Index : cardinal] : cardinal
      read      GetHuffValue
      write     SetHuffValue;
    property    HuffSize[Index : cardinal] : cardinal
      read      GetHuffSize
      write     SetHuffSize;
    property    HuffCode[Index : cardinal] : cardinal
      read      GetHuffCode
      write     SetHuffCode;
    property    MinCode[Index : cardinal] : cardinal
      read      GetMinCode
      write     SetMinCode;
    property    MaxCode[Index : cardinal] : longint
      read      GetMaxCode
      write     SetMaxCode;
    property    ValCode[Index : cardinal] : cardinal
      read      GetValCode
      write     SetValCode;
    property    GetNBits  : TGetEndianNBits
      read      FGetNBits
      write     FGetNBits;
    property    TableDefined : boolean
      read      FTableDefined;
  published
  end;

  TmcmHuffmanJPEG = class(TmcmHuffmanBase)
  private
    FSymbolCount : integer;
  protected
    FHuffmanBits : array[0..2*JPEGMaxHuffmanCodeLength] of cardinal;
    function    GetHuffmanBits(Index : word) : cardinal;
    procedure   SetHuffmanBits(Index : word; Value : cardinal);
  public
    procedure   ClearTable; override;
    function    CreateReadTable(Count : cardinal; HuffBits : array of byte) : TmcmErrorCode; {$IFNDEF DCB3} reintroduce; {$ENDIF}
    function    CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode; override;

    property    HuffmanBits[Index : word] : cardinal
      read      GetHuffmanBits
      write     SetHuffmanBits;
    property    SymbolCount : integer
      read      FSymbolCount; 
  published
  end;

  TmcmHuffmanPNG = class(TmcmHuffmanBase)
  private
  protected
  public
    function    CreateReadTable(Count : cardinal) : TmcmErrorCode; override;
    function    CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode; override;
  published
  end;

//------------------------------------------------------------------------------
// TmcmImageLZ.

  PLZEnumExtraData = ^TLZEnumExtraData;  // Extra data record for the
  TLZEnumExtraData = packed record       // hash table's FindAll method.
    SW           : TmcmSlidingWindow;    // Sliding window class
    MaxLen       : integer;              // Maximum match length so far
    DistMaxMatch : integer;              // Distance of maximun match
  end;

  TLZEncoding = packed record
    AsDistLen : integer;
    AsChar    : AnsiChar;
    IsChar    : boolean;
    {$IFDEF WIN32}
    Filler    : word;
    {$ENDIF}
  end;

  TLZEncodingArray = array [0..7] of TLZEncoding;

  TOnCompressData = procedure(Sender : TObject; Buffer : pointer; BufferSize : cardinal) of object;

  TmcmImageLZ = class(TmcmImageCompress)
  private
    FWindowSize     : word;
    FMaxMatchLength : SmallInt;  // Same as LookAheadSize
    FDistanceShift  : word;
    FLengthMask     : word;
    FHashTable      : TmcmHashTable;
    FSlidingWindow  : TmcmSlidingWindow;
    FKey            : THashKey;
    FOffset         : longint;
    FCodeCount      : cardinal;
    FEncoderSize    : cardinal;
    FEncoders       : PVectorB;  // TLZEncodingArray;
    FEnumData       : TLZEnumExtraData;
    FPackWidth      : cardinal;  // Packed line width.
    FDataToCompress : cardinal;

    FOnNeedData     : TOnNeedData;
    FOnCompressData : TOnCompressData;

    FFlag           : byte;
    FFlagBit        : integer;
    FFlagIndex      : cardinal;
  protected
    function    AddCodeToEncodings(       ALiteral  : byte;
                                      var Encodings : PVectorB;
                                      var Count     : cardinal) : TmcmErrorCode; virtual;
    function    AddDistLenToEncodings(    ADistance : integer;
                                          ALength   : integer;
                                      var Encodings : PVectorB;
                                      var Count     : cardinal) : TmcmErrorCode; virtual;
    function    GetDistLenToEncodings(var ALiteral  : byte;
                                      var ADistance : integer;
                                      var ALength   : integer;
                                          Encodings : PVectorB;
                                      var Count     : cardinal) : boolean; virtual;
    procedure   MatchLongest(      ExtraData   : pointer;
                             const Key         : THashKey;
                                   Offset, Len, Code : longint);
    procedure   SetDistanceShift(Value : word);
    procedure   SetEncoderSize(Value : cardinal);
    procedure   SetImage  (    Value      : HBITMAP); override;
    procedure   SetOnNeedData(AOnNeedData : TOnNeedData);
    procedure   OnSlidingHasData(Sender : TObject; pData : Pointer; NoBytes : cardinal);
    procedure   OnSlidingNeedData(Sender : TObject; pData : Pointer; var NoBytes : cardinal);
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    Compress  (var Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    function    Decompress(    Buffer     : pointer;
                           var BufferSize : longword;
                           var DataCount  : longword) : TmcmErrorCode; override;
    property    EncoderSize : cardinal
      read      FEncoderSize
      write     SetEncoderSize;
    property    DistanceShift  : word
      read      FDistanceShift
      write     SetDistanceShift default 3;
    property    MaxMatchLength : SmallInt
      read      FMaxMatchLength
      write     FMaxMatchLength default 18;
    property    WindowSize : word
      read      FWindowSize
      write     FWindowSize default 4096;
    property    OnCompressData : TOnCompressData
      read      FOnCompressData
      write     FOnCompressData;
    property    OnNeedData : TOnNeedData
      read      FOnNeedData
      write     SetOnNeedData;
  published
  end;


implementation

uses {$IFNDEF GE_DXE2}
     SysUtils,
     {$ELSE}
     System.SysUtils,
     {$ENDIF}
     mcmPriorityQueue, mcmImageResStr;

//------------------------------------------------------------------------------
// TmcmImageompress
//------------------------------------------------------------------------------

constructor TmcmImageCompress.Create;
begin
  Inherited Create;
  FCBuffer    := Nil;
  FDBuffer    := Nil;
  FIndex      := 0;
  FCol        := 0;
  FRow        := 0;
  FMaxCol     := 0;
  FMaxRow     := 0;
  FSupColors  := [];
  FError      := EC_OK;
  FFlipVert   := False;
end; // TmcmImageCompress.Create.


destructor TmcmImageCompress.Destroy;
begin
  Clear;
  Inherited Destroy;
end; // TmcmImageCompress.Destroy.


procedure TmcmImageCompress.Clear;
begin
  FError := EC_OK;
  FIndex := 0;
  FCol   := 0;
  FRow   := 0;
  if Assigned(FDBuffer)
  then FreeMem(FDBuffer);
  FDBuffer := Nil;
end; // TmcmImageCompress.Clear.


procedure TmcmImageCompress.SetImage(Value : HBITMAP);
begin
  FDib := Value;
  if (FDib <> 0)
  then begin
       // Get bitmap information.
       if (GetObject(FDib, SizeOf(TDIBSection), @FDIBSection) <> 0)
       then begin
            // Copy BitmapInfoHeader and pointer to bitmap bits.
            FDibBits := FDIBSection.dsBm.bmBits;

            with FDIBSection.dsBmih
            do FLongWidth := ((longint(biWidth * biBitCount) + 31) div 32) * 4;
       end;

       FMaxCol := FDIBSection.dsBmih.biWidth;
       FMaxRow := FDIBSection.dsBmih.biHeight;
       FImageSize := longword(FLongWidth * FMaxRow);
  end
  else FImageSize := 0;

  FCol   := 0;
  // Calculate start index to bitmap data.
  SetFlipVert(FFlipVert);
end; // TmcmImageCompress.SetImage.


procedure TmcmImageCompress.SetFlipVert(Value : boolean);
begin
  FFlipVert := Value;
  if FFlipVert
  then begin
       FRow   := FDIBSection.dsBmih.biHeight - 1;
       FIndex := FRow * FLongWidth;
  end
  else begin
       FRow   := 0;
       FIndex := 0;
  end;
end; // TmcmImageCompress.SetFlipVert.


procedure TmcmImageCompress.SwapByteRB(Buffer : pointer);
type T24RGB = array[0..0] of TRGBTriple;
     T32RGB = array[0..0] of TRGBQuad;
var i      : longint;
    RVal   : byte;
begin
  case FDIBSection.dsBmih.biBitCount of
  24 : for i := 0 to (FDIBSection.dsBmih.biWidth - 1)
       do begin
          RVal := T24RGB(Buffer^)[i].RGBtblue;
          T24RGB(Buffer^)[i].RGBtblue := T24RGB(Buffer^)[i].RGBtred;
          T24RGB(Buffer^)[i].RGBtred  := RVal;
       end;
  32 : for i := 0 to (FDIBSection.dsBmih.biWidth - 1)
       do begin
          RVal := T32RGB(Buffer^)[i].rgbBlue;
          T32RGB(Buffer^)[i].rgbBlue := T32RGB(Buffer^)[i].rgbRed;
          T32RGB(Buffer^)[i].rgbRed  := RVal;
       end;
  end;
end; // TmcmImageCompress.SwapByteRB.


procedure TmcmImageCompress.AllocCompressBuffer(MinSize : cardinal);
begin
  // Allocate or re-allocate data buffer for compression.
  if (FDBuffer = Nil)
  then begin
       // Calc and set initial compress data buffer.
       // Initial guess is one quater image size -
       FDIncSize := FImageSize div 16;
       // though not less than 2 lines -
       if (FDIncSize < 2 * longword(FLongWidth))
       then FDIncSize := 2 * longword(FLongWidth);
       // though not less than supplied minimum size.
       if (FDIncSize < MinSize)
       then FDIncSize := MinSize;
       FDSize := 2 * FDIncSize;
  end
  // Increment data buffer.
  else begin
       if (FDIncSize < MinSize)
       then FDIncSize := MinSize;
       FDSize := FDSize + FDIncSize;
  end;
  ReallocMem(FDBuffer, FDSize);
end; // TmcmImageCompress.AllocCompressBuffer.


//------------------------------------------------------------------------------
// TmcmImageRLE
//------------------------------------------------------------------------------

constructor TmcmImageRLE.Create;
begin
  Inherited Create;
  FSupColors     := [];
  FRLEStr        := Nil;
  FRLECountEqual := Nil;
  FRLECountDiff  := Nil;
  FMinEqual      := 4;
  FOptEqual      := 5;
  FMinDiff       := 3;
end; // TmcmImageRLE.Create.


destructor TmcmImageRLE.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageRLE.Destroy.


procedure TmcmImageRLE.Clear;
begin
  if (FRLEStr <> Nil)
  then FreeMem(FRLEStr);
  FRLEStr := Nil;
  Inherited Clear;
end; // TmcmImageRLE.Clear.


function TmcmImageRLE.CountEqual4(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i, j     : longint;
    Value    : byte;
    Test     : byte;
    Continue : boolean;
begin
  i := Index + 1;
  j := Index shr 1;
  if Odd(Index)
  then Value := PVectorB(Buffer)^[j] and $0F
  else Value := (PVectorB(Buffer)^[j] and $F0) shr 4;
  Continue := True;
  while (i < MaxIndex) and Continue
  do begin
     j := i shr 1;
     if Odd(i)
     then Test := PVectorB(Buffer)^[j] and $0F
     else Test := (PVectorB(Buffer)^[j] and $F0) shr 4;
     if (Value = Test)
     then inc(i)
     else Continue := False;
  end;
  Result := i - Index;
end; // TmcmImageRLE.CountEqual4.


function TmcmImageRLE.CountDiff4(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i, j     : longint;
    Value    : byte;
    Test     : byte;
    Continue : boolean;
begin
  i := Index + 1;
  j := Index shr 1;
  if Odd(Index)
  then Value := PVectorB(Buffer)^[j] and $0F
  else Value := (PVectorB(Buffer)^[j] and $F0) shr 4;
  Continue := True;
  while (i < MaxIndex) and Continue
  do begin
     j := i shr 1;
     if Odd(i)
     then Test := PVectorB(Buffer)^[j] and $0F
     else Test := (PVectorB(Buffer)^[j] and $F0) shr 4;
     if (Value <> Test)
     then inc(i)
     else Continue := False;
  end;
  Result := i - Index - 1;
end; // TmcmImageRLE.CountDiff4


function TmcmImageRLE.CountEqual8(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorB(Buffer)^[Index] = PVectorB(Buffer)^[i])
  do inc(i);
  Result := i - Index;
end; // TmcmImageRLE.CountEqual8.


function TmcmImageRLE.CountDiff8(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorB(Buffer)^[i-1] <> PVectorB(Buffer)^[i])
  do inc(i);
  Result := i - Index - 1;
end; // TmcmImageRLE.CountDiff8.


function TmcmImageRLE.CountEqual16(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorW(Buffer)^[Index] = PVectorW(Buffer)^[i])
  do inc(i);
  Result := i - Index;
end; // TmcmImageRLE.CountEqual16.


function TmcmImageRLE.CountDiff16(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorW(Buffer)^[i-1] <> PVectorW(Buffer)^[i])
  do inc(i);
  Result := i - Index - 1;
end; // TmcmImageRLE.CountDiff16.


function TmcmImageRLE.CountEqual24(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and
        (PVectorRGB(Buffer)^[Index].rgbtBlue = PVectorRGB(Buffer)^[i].rgbtBlue) and
        (PVectorRGB(Buffer)^[Index].rgbtGreen = PVectorRGB(Buffer)^[i].rgbtGreen) and
        (PVectorRGB(Buffer)^[Index].rgbtRed = PVectorRGB(Buffer)^[i].rgbtRed)
  do inc(i);
  Result := i - Index;
end; // TmcmImageRLE.CountEqual24.


function TmcmImageRLE.CountDiff24(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i, j : longint;
begin
  i := Index + 1;
  j := Index;
  while (i < MaxIndex) and
        (PVectorRGB(Buffer)^[j].rgbtBlue <> PVectorRGB(Buffer)^[i].rgbtBlue) and
        (PVectorRGB(Buffer)^[j].rgbtGreen <> PVectorRGB(Buffer)^[i].rgbtGreen) and
        (PVectorRGB(Buffer)^[j].rgbtRed <> PVectorRGB(Buffer)^[i].rgbtRed)
  do begin
     inc(i);
     inc(j);
  end;
  Result := i - Index - 1;
end; // TmcmImageRLE.CountDiff24.


function TmcmImageRLE.CountEqual32(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorL(Buffer)^[Index] = PVectorL(Buffer)^[i])
  do inc(i);
  Result := i - Index;
end; // TmcmImageRLE.CountEqual32.


function TmcmImageRLE.CountDiff32(Buffer : Pointer; Index, MaxIndex : longint) : integer;
var i : longint;
begin
  i := Index + 1;
  while (i < MaxIndex) and (PVectorL(Buffer)^[i-1] <> PVectorL(Buffer)^[i])
  do inc(i);
  Result := i - Index - 1;
end; // TmcmImageRLE.CountDiff32.


function TmcmImageRLE.CodeRLELine(RLEStr   : PRLEString;
                                  Buffer   : PVectorB;
                                  Index    : longint;
                                  MaxIndex : longint) : integer;
// Returnes number of RLE strings.
var RLEIndex : longint;
    Rest     : integer;
    i, m     : longint;
    a, b     : longint;
begin
  //-----------------------------------------------------------------------
  // Calculate "runs" for current row.
  RLEStr[0].Index := 0;
  RLEStr[0].Count := 0;
  RLEStr[0].Equal := True;

  RLEIndex := 1;
  while (Index < MaxIndex)
  do begin
     RLEStr[RLEIndex].Index := Index;

     // Get count of repeated pixel values.
     a := FRLECountEqual(Buffer, Index, MaxIndex);
     if (a >= FMinEqual)
     then begin
          RLEStr[RLEIndex].Count := a;
          RLEStr[RLEIndex].Equal := True;
          inc(Index, a);
     end
     else begin
          // Get count of different pixels values.
          b := FRLECountDiff(Buffer, Index + a, MaxIndex) + a;
          RLEStr[RLEIndex].Count := b;
          RLEStr[RLEIndex].Equal := False;
          inc(Index, b);
     end;

     if (RLEStr[RLEIndex-1].Equal or RLEStr[RLEIndex].Equal)
     then inc(RLEIndex)
     else inc(RLEStr[RLEIndex-1].Count, RLEStr[RLEIndex].Count);
  end;
  RLEStr[RLEIndex].Count := 0;
  dec(RLEIndex);

  //-----------------------------------------------------------------------
  // Check that each different-pixel-"run" is equal to or bigger than
  // MinDiff.
  for i := 1 to RLEIndex
  do begin
     if Not(RLEStr[i].Equal)
     then begin
          if (0 < RLEStr[i].Count) and (RLEStr[i].Count < FMinDiff)
          then begin
               Rest := FMinDiff - RLEStr[i].Count;

               // Determind which "run" to merge with!
               if (1 < i) and (i < RLEIndex)
               then begin
                    if (RLEStr[i-1].Count > RLEStr[i+1].Count)
                    then m := i + 1
                    else m := i - 1;
               end
               else begin
                    if (i = 1)
                    then m := i + 1
                    else m := i - 1;
               end;

               if ((RLEStr[m].Count - Rest) < FOptEqual)
               then begin
                    if (m > i)
                    then RLEStr[m].Index := RLEStr[i].Index;
                    inc(RLEStr[m].Count, RLEStr[i].Count);
                    RLEStr[m].Equal := False;
                    RLEStr[i].Count := 0;
               end
               else begin
                    // Just take required pixels from neighbour "run".
                    dec(RLEStr[m].Count, Rest);
                    inc(RLEStr[i].Count, Rest);
                    if (m < i)
                    then dec(RLEStr[i].Index, Rest)
                    else inc(RLEStr[m].Index, Rest);
               end;
          end;
     end;
  end;

  //-----------------------------------------------------------------------
  // If neighbour "run" are both different pixel count "runs", then  merge
  // the two "runs".
  for i := 1 to RLEIndex
  do begin
     if Not(RLEStr[i].Equal)
     then begin
          m := i - 1;
          while (m > 1) and (RLEStr[m].Count = 0)
          do dec(m);

          if Not(RLEStr[m].Equal)
          then begin
               inc(RLEStr[m].Count, RLEStr[i].Count);
               RLEStr[i].Count := 0;
          end;
     end;
  end;
  Result := RLEIndex;
end; // TmcmImageRLE.CodeRLELine.


procedure TmcmImageRLE.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);
  case FDIBSection.dsBmih.biBitCount of
  4 : begin
        FRLECountEqual := CountEqual4;
        FRLECountDiff  := CountDiff4;
      end;
  8 : begin
        FRLECountEqual := CountEqual8;
        FRLECountDiff  := CountDiff8;
      end;
  15,
  16 : begin
        FRLECountEqual := CountEqual16;
        FRLECountDiff  := CountDiff16;
       end;
  24 : begin
        FRLECountEqual := CountEqual24;
        FRLECountDiff  := CountDiff24;
       end;
  32 : begin
        FRLECountEqual := CountEqual32;
        FRLECountDiff  := CountDiff32;
       end;
  end;

  // Alloc RLE string buffer.
  if (FRLEStr <> Nil)
  then FreeMem(FRLEStr);
  GetMem(FRLEStr, 2 * FLongWidth * SizeOf(TRLEEncodeRec));
end; // TmcmImageRLE.SetImage.


//------------------------------------------------------------------------------
// TmcmImageRLE4
//------------------------------------------------------------------------------

constructor TmcmImageRLE4.Create;
begin
  Inherited Create;
  FSupColors := [IF_GREY4, IF_PAL4];
  FRLEStr    := Nil;
  FMinDiff   := 1;
end; // TmcmImageRLE4.Create.


procedure TmcmImageRLE4.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);
  FRLECountEqual := CountEqual8;
  FRLECountDiff  := CountDiff8;
end; // TmcmImageRLE4.SetImage.


function TmcmImageRLE4.Compress(var Buffer     : pointer;
                                var BufferSize : longword;
                                var DataCount  : longword) : TmcmErrorCode;
const
    MaxRunLength = 127;
var Rest      : integer;
    i, j, n   : longint;
    MaxIndex  : longint;
    RLEIndex  : longint;
begin
  FError    := EC_OK;
  DataCount := 0;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  i := 0;
  FIndex := 0;
  try
    while (FIndex < FImageSize) and (FRow < FMaxRow)
    do begin
       FCol := 0;
       FIndex   := FRow * FLongWidth;
       MaxIndex := FIndex + (FMaxCol div 2);

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       RLEIndex := CodeRLELine(FRLEStr, FDibBits, FIndex, MaxIndex);

       //-----------------------------------------------------------------------
       // Write Compress line data (Bitmap RLE4).
       j := 1;
       while (j <= RLEIndex)
       do begin
          if (FRLEStr[j].Count > 0)
          then begin
               // Check that "run" is less than 256.
               if (FRLEStr[j].Count > MaxRunLength)
               then begin
                    Rest := FRLEStr[j].Count;
                    n := Trunc(FRLEStr[j].Count / MaxRunLength);
                    if (n = 1)
                    then FRLEStr[j].Count := FRLEStr[j].Count div 2
                    else FRLEStr[j].Count := 127; // 255;
                    dec(Rest, FRLEStr[j].Count);
               end
               else Rest := 0;

               if Not(FRLEStr[j].Equal) and (FRLEStr[j].Count = 1)
               then FRLEStr[j].Equal := True;

               if FRLEStr[j].Equal
               then begin // Same pixels
                    FDBuffer^[i] := FRLEStr[j].Count * 2; // Count
                    inc(i);
                    FDBuffer^[i] := FDibBits^[FRLEStr[j].Index]; // Value
                    inc(i);
               end
               else begin // Different pixels
                    FDBuffer^[i] := 0; // Count
                    inc(i);
                    FDBuffer^[i] := FRLEStr[j].Count * 2; // Value
                    inc(i);
                    // Count = 0, and Value greater than two, means literaly copy
                    // "value" bytes
                    CopyMemory(@FDBuffer^[i], @FDibBits^[FRLEStr[j].Index], FRLEStr[j].Count);
                    inc(i, FRLEStr[j].Count);

                    // An odd number of bytes are followed by a pad pixel.
                    case (FRLEStr[j].Count * 2 and $03) of
                    1, 2 : begin
                             FDBuffer^[i] := 0;
                             inc(i);
                           end;
                    end;
               end;

               // If this "run" has remaining pixels, add these in next "run".
               if (Rest <> 0)
               then begin
                    inc(FRLEStr[j].Index, FRLEStr[j].Count);
                    FRLEStr[j].Count := Rest;
               end
               else inc(j);
          end
          else inc(j);
       end;

       inc(FRow);
       if (FRow < FMaxRow)
       then begin // Add escape code "Goto next line" = 00
            FDBuffer^[i] := 0;
            inc(i);
            FDBuffer^[i] := 0;
            inc(i);
       end
       else begin // Add escape code "End of image" = 01
            FDBuffer^[i] := 0;
            inc(i);
            FDBuffer^[i] := 1;
            inc(i);
       end;
    end;
  except
    FError := EC_BADCOMPRESS;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then begin
       BufferSize := i;
       DataCount := FImageSize;
  end
  else BufferSize := 0;
  Result := FError;
end; // TmcmImageRLE4.Compress.


function TmcmImageRLE4.Decompress(    Buffer     : pointer;
                                  var BufferSize : longword;
                                  var DataCount  : longword) : TmcmErrorCode;
var Count  : byte;
    Count2 : byte;
    Value  : byte;
    Value2 : byte;
    i      : longint;
begin
  // NOTE: Starting at an odd pixel position is not currently supported. This MAY
  // and MAY NOT be a problem.
  FError := EC_OK;
  FCBuffer := Buffer;

  // Starte by filling image with black pixels.
  if (FRow = 0) and (FCol = 0)
  then FillMemory(FDibBits, FImageSize, 0 {Black});

  try
    i := 0;
    while (FIndex < FImageSize) and (longword(i) < BufferSize)
    do begin
       Count := FCBuffer^[i];
       inc(i);
       Value := FCBuffer^[i];
       inc(i);
       // Read Count and Value
       case Count of
       0  : begin // Escape code.
              case Value of
              0 : begin // Goto next line
                    inc(FRow);
                    FCol := 0;
                    FIndex := FRow * FLongWidth + FCol;
                  end;
              1 : begin // End of image.
                    FIndex := FImageSize;
                  end;
              2 : begin // Change position to (next two unsigned bytes) x,y-values to advance.
                    FCol := FCol + FCBuffer^[i+1];
                    FRow := FRow + FCBuffer^[i+2];
                    FIndex := FRow * FLongWidth + FCol;
                    inc(i, 2);
                  end;
              else begin // Value greater than two, means literaly copy "value" bytes
                     Value2 := Value shr 1;
                     CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], Value2);
                     inc(FCol, Value);

                     inc(FIndex, Value2);
                     inc(i, Value2);

                     // An odd number of bytes are followed by a pad pixel.
                     case (Value and $03) of
                     1, 2 : inc(i);
                     end;
                   end;
              end;
            end;
       else begin // Set Count bytes to Value.
              Count2 := Count shr 1;
              FillChar(FDibBits^[FIndex], Count2, Value);
              inc(FCol, Count);
              inc(FIndex, Count2);
            end;
       end;
    end;
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageRLE4.Decompress.


//------------------------------------------------------------------------------
// TmcmImageRLE8
//------------------------------------------------------------------------------

constructor TmcmImageRLE8.Create;
begin
  Inherited Create;
  FSupColors := [IF_GREY8, IF_PAL8];
end; // TmcmImageRLE8.Create.


function TmcmImageRLE8.Compress(var Buffer     : pointer;
                                var BufferSize : longword;
                                var DataCount  : longword) : TmcmErrorCode;
const
    MaxRunLength = 255;
var Rest      : integer;
    i, j, n   : longint;
    MaxIndex  : longint;
    RLEIndex  : longint;
begin
  FError    := EC_OK;
  DataCount := 0;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  i := 0;
  FIndex := 0;
  try
    while (FIndex < FImageSize) and (FRow < FMaxRow)
    do begin
       FCol := 0;
       FIndex   := FRow * FLongWidth;
       MaxIndex := FIndex + FMaxCol;

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       RLEIndex := CodeRLELine(FRLEStr, FDibBits, FIndex, MaxIndex);

       //-----------------------------------------------------------------------
       // Write Compress line data (Bitmap RLE8).
       j := 1;
       while (j <= RLEIndex)
       do begin
          if (FRLEStr[j].Count > 0)
          then begin
               // Check that "run" is less than 256.
               if (FRLEStr[j].Count > MaxRunLength)
               then begin
                    Rest := FRLEStr[j].Count;
                    n := Trunc(FRLEStr[j].Count / MaxRunLength);
                    if (n = 1)
                    then FRLEStr[j].Count := FRLEStr[j].Count div 2
                    else FRLEStr[j].Count := 255;
                    dec(Rest, FRLEStr[j].Count);
               end
               else Rest := 0;

               if FRLEStr[j].Equal
               then begin // Same pixels
                    FDBuffer^[i] := FRLEStr[j].Count; // Count
                    inc(i);
                    FDBuffer^[i] := FDibBits^[FRLEStr[j].Index]; // Value
                    inc(i);
               end
               else begin // Different pixels
                    FDBuffer^[i] := 0; // Count
                    inc(i);
                    FDBuffer^[i] := FRLEStr[j].Count; // Value
                    inc(i);
                    // Count = 0, and Value greater than two, means literaly copy
                    // "value" bytes
                    CopyMemory(@FDBuffer^[i], @FDibBits^[FRLEStr[j].Index], FRLEStr[j].Count);
                    inc(i, FRLEStr[j].Count);

                    // An odd number of bytes are followed by a pad byte (zero).
                    if Odd(FRLEStr[j].Count)
                    then begin
                         FDBuffer^[i] := 0;
                         inc(i);
                    end;
               end;

               // If this "run" has remaining pixels, add these in next "run".
               if (Rest <> 0)
               then begin
                    inc(FRLEStr[j].Index, FRLEStr[j].Count);
                    FRLEStr[j].Count := Rest;
               end
               else inc(j);
          end
          else inc(j);
       end;

       inc(FRow);
       if (FRow < FMaxRow)
       then begin // Add escape code "Goto next line" = 00
            FDBuffer^[i] := 0;
            inc(i);
            FDBuffer^[i] := 0;
            inc(i);
       end
       else begin // Add escape code "End of image" = 01
            FDBuffer^[i] := 0;
            inc(i);
            FDBuffer^[i] := 1;
            inc(i);
       end;
    end;
  except
    FError := EC_BADCOMPRESS;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then begin
       BufferSize := i;
       DataCount := FImageSize;
  end
  else BufferSize := 0;
  Result := FError;
end; // TmcmImageRLE8.Compress.


function TmcmImageRLE8.Decompress(    Buffer     : pointer;
                                  var BufferSize : longword;
                                  var DataCount  : longword) : TmcmErrorCode;
var Count : byte;
    Value : byte;
    i     : longint;
begin
  FError := EC_OK;
  FCBuffer := Buffer;

  // Starte by filling image with black pixels.
  if (FRow = 0) and (FCol = 0)
  then FillMemory(FDibBits, FImageSize, 0 {Black});

  try
    i := 0;
    while (FIndex < FImageSize) and (longword(i) < BufferSize)
    do begin
       Count := FCBuffer^[i];
       inc(i);
       Value := FCBuffer^[i];
       inc(i);
       // Read Count and Value
       case Count of
       0  : begin // Escape code.
              case Value of
              0 : begin // Goto next line
                    inc(FRow);
                    FCol := 0;
                    FIndex := FRow * FLongWidth + FCol;
                  end;
              1 : begin // End of image.
                    FIndex := FImageSize;
                  end;
              2 : begin // Change position to (next two unsigned bytes) x,y-values to advance.
                    FCol := FCol + FCBuffer^[i+1];
                    FRow := FRow + FCBuffer^[i+2];
                    FIndex := FRow * FLongWidth + FCol;
                    inc(i, 2);
                  end;
              else begin // Value greater than two, means literaly copy "value" bytes
                     CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], Value);
                     inc(FCol, Value);
                     inc(FIndex, Value);
                     inc(i, Value);

                     // An odd number of bytes are followed by a pad byte.
                     if Odd(Value)
                     then inc(i);
                   end;
              end;
            end;
       else begin // Set Count bytes to Value.
              FillChar(FDibBits^[FIndex], Count, Value);
              inc(FCol, Count);
              inc(FIndex, Count);
            end;
       end;
    end;
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageRLE8.Decompress.


//------------------------------------------------------------------------------
// TmcmImagePackedBits
//------------------------------------------------------------------------------

constructor TmcmImagePackBits.Create;
begin
  Inherited Create;
  FRGBBuffer := Nil;
  FSupColors := [IF_BW, IF_GREY4, IF_GREY8, IF_PAL4, IF_PAL8, IF_RGB24, IF_RGBA32{ IF_GRAY16}];
  FFlipVert  := True;

  FMinEqual  := 3;
  FOptEqual  := 3;
  FMinDiff   := 1;
end; // TmcmImagePackBits.Create.


procedure TmcmImagePackBits.Clear;
begin
  if (FRGBBuffer <> Nil) and
     ((FDIBSection.dsBmih.biBitCount = 24) or
      (FDIBSection.dsBmih.biBitCount = 32))
  then FreeMem(FRGBBuffer);
  FRGBBuffer := Nil;
  Inherited Clear;
end; // TmcmImagePackBits.Clear.


procedure TmcmImagePackBits.SetImage(Value : HBITMAP);
var BitCount : integer;
begin
  Inherited SetImage(Value);

  FRLECountEqual := CountEqual8;
  FRLECountDiff  := CountDiff8;

  FSwapRB := False;
  BitCount := FDIBSection.dsBmih.biBitCount * FDIBSection.dsBmih.biPlanes;
  case BitCount of
  1  : FPackWidth := (FDIBSection.dsBmih.biWidth + 7) div 8;
  4  : FPackWidth := (FDIBSection.dsBmih.biWidth + 1) div 2;
  8  : FPackWidth := FDIBSection.dsBmih.biWidth;
  15,
  16 : FPackWidth := 2 * FDIBSection.dsBmih.biWidth;
  24 : begin
         FPackWidth := 3 * FDIBSection.dsBmih.biWidth;
         FSwapRB := True;
       end;
  32 : begin
         FPackWidth := 4 * FDIBSection.dsBmih.biWidth;
         FSwapRB := True;
       end;
  end;

  if (FDIBSection.dsBmih.biBitCount in [24,32])
  then GetMem(FRGBBuffer, FLongWidth * SizeOf(Byte))
  else FRGBBuffer := Nil;
end; // TmcmImagePackBits.SetImage.


function TmcmImagePackBits.Compress(var Buffer     : pointer;
                                    var BufferSize : longword;
                                    var DataCount  : longword) : TmcmErrorCode;
const
    MaxRunLength = 128;
var Rest      : integer;
    i, j, n   : longint;
    RLEIndex  : longint;
    CountDown : longword;
begin
  FError    := EC_OK;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 4);

  CountDown := DataCount;
  DataCount := 0;

  i := 0;
  try
    while (CountDown > 0)
    do begin
       FCol := 0;
       FIndex   := FRow * FLongWidth;

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       if (FDIBSection.dsBmih.biBitCount in [24,32])
       then begin
            CopyMemory(FRGBBuffer, @FDibBits^[Findex], FLongWidth);
            SwapByteRB(FRGBBuffer);
       end
       else FRGBBuffer := @FDibBits^[FIndex];

       // Get optimised Run length encoding.
       RLEIndex := CodeRLELine(FRLEStr, FRGBBuffer, 0, FPackWidth);

       //-----------------------------------------------------------------------
       // Write Compress line data.
       j := 1;
       while (j <= RLEIndex)
       do begin
          if (FRLEStr[j].Count > 0)
          then begin
               // Check that "run" is less than 128.
               if (FRLEStr[j].Count > MaxRunLength)
               then begin
                    Rest := FRLEStr[j].Count;
                    n := Trunc(FRLEStr[j].Count / MaxRunLength);
                    if (n = 1)
                    then FRLEStr[j].Count := FRLEStr[j].Count div 2
                    else FRLEStr[j].Count := 128;
                    dec(Rest, FRLEStr[j].Count);
               end
               else Rest := 0;

               if FRLEStr[j].Equal
               then begin // Same pixels
                    FDBuffer^[i] := -(FRLEStr[j].Count - 1); // Count
                    inc(i);
                    FDBuffer^[i] := FRGBBuffer^[FRLEStr[j].Index]; // data
                    inc(i);
               end
               else begin // Different pixels
                    FDBuffer^[i] := (FRLEStr[j].Count - 1); // Count
                    inc(i);
                    CopyMemory(@FDBuffer^[i], @FRGBBuffer^[FRLEStr[j].Index], FRLEStr[j].Count);
                    inc(i, FRLEStr[j].Count);
               end;

               // If this "run" has remaining pixels, add these in next "run".
               if (Rest <> 0)
               then begin
                    inc(FRLEStr[j].Index, FRLEStr[j].Count);
                    FRLEStr[j].Count := Rest;
               end
               else inc(j);
          end
          else inc(j);
       end;
       dec(FRow);

       inc(DataCount, FLongWidth);
       dec(CountDown, FLongWidth);
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImagePackBits.Compress.


function TmcmImagePackBits.Decompress(    Buffer     : pointer;
                                      var BufferSize : longword;
                                      var DataCount  : longword): TmcmErrorCode;
var Count     : shortint;
    dCount    : integer;
    i, j      : longword;
    RGBIndex  : longint;
    SaveIndex : longint;
begin
  FError := EC_OK;
  FCBuffer := Buffer;
  DataCount := 0;
  SaveIndex := FIndex;

  try
    i := 0;
    j := 0;
    RGBIndex := FIndex;
    while (0 <= FIndex) and (FIndex < FImageSize) and (longword(i) < BufferSize)
    do begin
       Count := FCBuffer^[i];
       inc(i);
       case Count of
       // -128     :  // No operation.
       -127..-1 : begin // Copy byte dCount times.
                    dCount := -Count + 1;
                    FillChar(FDibBits^[FIndex], dCount, FCBuffer^[i]);
                    inc(i);
                  end;
       0..127   : begin // Copy bytes literally.
                    dCount := Count + 1;
                    CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], dCount);
                    inc(i, dCount);
                  end;
       else dCount := 0;
       end;
       inc(FIndex, dCount);
       inc(j, dCount);

       if (j >= FPackWidth)
       then begin // End of line. Calculate new index to bitmap data.
            if FFlipVert
            then dec(FRow)
            else inc(FRow);
            FIndex := FRow * FLongWidth;

            // Swap R and B colour component to conform to Windows bitmap.
            if FSwapRB
            then begin
                 SwapByteRB(@FDibBits^[RGBIndex]);
                 if FFlipVert
                 then dec(RGBIndex, FLongWidth)
                 else inc(RGBIndex, FLongWidth);
            end;
            j := 0;
       end;
    end;
    // Caluclate decompressed data size.
    DataCount := FIndex - SaveIndex;
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImagePackBits.Decompress.


//------------------------------------------------------------------------------
// TmcmImageTargaRLE
//------------------------------------------------------------------------------

constructor TmcmImageTargaRLE.Create;
begin
  Inherited Create;
  FSupColors := [IF_GREY8, IF_PAL8, IF_RGB15, IF_RGB16, IF_RGB24, IF_RGBA32];

  FRGBBuffer := Nil;
  FFlipVert  := True;
end; // TmcmImageTargaRLE.Create.


procedure TmcmImageTargaRLE.Clear;
begin
  FRGBBuffer := Nil;
  Inherited Clear;
end; // TmcmImageTargaRLE.Clear.


procedure TmcmImageTargaRLE.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);

  FSwapRB := False;
  FPackWidth := FDIBSection.dsBmih.biWidth * ((1 + FDIBSection.dsBmih.biBitCount) div 8);
  case FDIBSection.dsBmih.biBitCount of
  15,
  16,
  24,
  32 : begin
         FMinEqual  := 2;
         FOptEqual  := 2;
         FMinDiff   := 2;
       end;
  else begin
       FMinEqual  := 4;
       FOptEqual  := 5;
       FMinDiff   := 3;
  end
  end;
end; // TmcmImageTargaRLE.SetImage.


function TmcmImageTargaRLE.Compress(var Buffer     : pointer;
                                    var BufferSize : longword;
                                    var DataCount  : longword): TmcmErrorCode;
const
    MaxRunLength = 128;
var Rest      : integer;
    i, j, n   : longint;
    RLEIndex  : longint;
    CountDown : longword;
    RunLength : cardinal;
begin
  FError    := EC_OK;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  CountDown := DataCount;
  DataCount := 0;

  i := 0;
  try
    case FDIBSection.dsBmih.biBitCount of
    15,
    16 : RunLength := FPackWidth div 2;
    24 : RunLength := FPackWidth div 3;
    32 : RunLength := FPackWidth div 4;
    else RunLength := FPackWidth;
    end;

    while (CountDown > 0)
    do begin
       FCol := 0;
       FIndex := FRow * FLongWidth;

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       FRGBBuffer := @FDibBits^[FIndex];

       // Get optimised Run length encoding.
       RLEIndex := CodeRLELine(FRLEStr, FRGBBuffer, 0, RunLength);

       //--------------------------------------------------------------------------
       // Write Compress line data to buffer.
       j := 1;
       while (j <= RLEIndex)
       do begin
          if (FRLEStr[j].Count > 0)
          then begin
               // Check that "run" is less than 128.
               if (FRLEStr[j].Count > MaxRunLength)
               then begin
                    Rest := FRLEStr[j].Count;
                    n := Trunc(FRLEStr[j].Count / MaxRunLength);
                    if (n = 1)
                    then FRLEStr[j].Count := FRLEStr[j].Count div 2
                    else FRLEStr[j].Count := MaxRunLength;
                    dec(Rest, FRLEStr[j].Count);
               end
               else Rest := 0;

               case FDIBSection.dsBmih.biBitCount of
               8  : begin
                      if FRLEStr[j].Equal
                      then begin // Same pixels
                           FDBuffer^[i] := (FRLEStr[j].Count - 1) or $80; // Count
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[FRLEStr[j].Index]; // data
                           inc(i);
                      end
                      else begin // Different pixels
                           FDBuffer^[i] := (FRLEStr[j].Count - 1); // Count
                           inc(i);
                           CopyMemory(@FDBuffer^[i], @FRGBBuffer^[FRLEStr[j].Index], FRLEStr[j].Count);
                           inc(i, FRLEStr[j].Count);
                      end;
                    end;
               15,
               16 : begin
                      if FRLEStr[j].Equal
                      then begin // Same pixels
                           n := FRLEStr[j].Index * 2;
                           FDBuffer^[i] := (FRLEStr[j].Count - 1) or $80; // Count
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+1]; // data
                           inc(i);
                      end
                      else begin // Different pixels
                           FDBuffer^[i] := (FRLEStr[j].Count - 1); // Count
                           inc(i);
                           n := FRLEStr[j].Index * 2;
                           CopyMemory(@FDBuffer^[i], @FRGBBuffer^[n], FRLEStr[j].Count * 2);
                           inc(i, FRLEStr[j].Count * 2);
                      end;
                    end;
               24 : begin
                      if FRLEStr[j].Equal
                      then begin // Same pixels
                           n := FRLEStr[j].Index * 3;
                           FDBuffer^[i] := (FRLEStr[j].Count - 1) or $80; // Count
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+1]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+2]; // data
                           inc(i);
                      end
                      else begin // Different pixels
                           FDBuffer^[i] := (FRLEStr[j].Count - 1); // Count
                           inc(i);
                           n := FRLEStr[j].Index * 3;
                           CopyMemory(@FDBuffer^[i], @FRGBBuffer^[n], FRLEStr[j].Count * 3);
                           inc(i, FRLEStr[j].Count * 3);
                      end;
                    end;
               32 : begin
                      if FRLEStr[j].Equal
                      then begin // Same pixels
                           n := FRLEStr[j].Index * 4;
                           FDBuffer^[i] := (FRLEStr[j].Count - 1) or $80; // Count
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+1]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+2]; // data
                           inc(i);
                           FDBuffer^[i] := FRGBBuffer^[n+3]; // data
                           inc(i);
                      end
                      else begin // Different pixels
                           FDBuffer^[i] := (FRLEStr[j].Count - 1); // Count
                           inc(i);
                           n := FRLEStr[j].Index * 4;
                           CopyMemory(@FDBuffer^[i], @FRGBBuffer^[n], FRLEStr[j].Count * 4);
                           inc(i, FRLEStr[j].Count * 4);
                      end;
                    end;
               end;

               // If this "run" has remaining pixels, add these in next "run".
               if (Rest <> 0)
               then begin
                    inc(FRLEStr[j].Index, FRLEStr[j].Count);
                    FRLEStr[j].Count := Rest;
               end
               else inc(j);
          end
          else inc(j);
       end;

       if FFlipVert
       then dec(FRow)
       else inc(FRow);

       inc(DataCount, FLongWidth);
       dec(CountDown, FLongWidth);
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImageTargaRLE.Compress.


function TmcmImageTargaRLE.Decompress(    Buffer     : pointer;
                                      var BufferSize : longword;
                                      var DataCount  : longword): TmcmErrorCode;
var Count     : byte;
    dCount    : word;
    i, j, k   : cardinal;
    RGBIndex  : longint;
    SaveIndex : longint;
begin
  FError := EC_OK;
  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  try
    i := 0;
    j := FIndex mod FlongWidth;
    RGBIndex := FIndex;

    while (i < BufferSize)
    do begin
       Count := FCBuffer^[i];
       dCount := (Count and $7F) + 1;
       inc(i);

       case FDIBSection.dsBmih.biBitCount of
       8  : begin
              if (dCount < BufferSize)
              then begin
                   if (Count > 127)
                   then begin // Copy byte dCount times.
                        if (BufferSize > 1)
                        then begin
                             FillChar(FDibBits^[FIndex], dCount, FCBuffer^[i]);
                             inc(i);
                             dec(BufferSize, 2);
                        end;
                   end
                   else begin // Copy bytes literally.
                        if (BufferSize > dCount)
                        then begin
                             CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], dCount);
                             inc(i, dCount);
                             dec(BufferSize);
                             dec(BufferSize, dCount);
                        end;
                   end;
                   inc(FIndex, dCount);
                   inc(j, dCount);
              end;
            end;
       15,
       16 : begin
              if (Count > 127)
              then begin // Copy byte dCount times.
                   if (BufferSize > 2)
                   then begin
                        for k := 0 to (dCount - 1)
                        do begin
                           FDibBits^[FIndex] := FCBuffer^[i];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+1];
                           inc(FIndex);
                        end;
                        inc(i, 2);
                        dec(BufferSize, 3);
                   end;
                   dCount := 2 * dCount;
              end
              else begin // Copy bytes literally.
                   dCount := 2 * dCount;
                   if (BufferSize > dCount)
                   then begin
                        CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], dCount);
                        inc(i, dCount);
                        dec(BufferSize);
                        dec(BufferSize, dCount);
                        inc(FIndex, dCount);
                   end;
              end;
              inc(j, dCount);
            end;
       24 : begin
              if (Count > 127)
              then begin // Copy byte dCount times.
                   if (BufferSize > 3)
                   then begin
                        for k := 0 to (dCount - 1)
                        do begin
                           FDibBits^[FIndex] := FCBuffer^[i];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+1];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+2];
                           inc(FIndex);
                        end;
                        inc(i, 3);
                        dec(BufferSize, 4);
                   end;
                   dCount := 3 * dCount;
              end
              else begin // Copy bytes literally.
                   dCount := 3 * dCount;
                   if (BufferSize > dCount)
                   then begin
                        CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], dCount);
                        inc(i, dCount);
                        dec(BufferSize);
                        dec(BufferSize, dCount);
                        inc(FIndex, dCount);
                   end;
              end;
              inc(j, dCount);
            end;
       32 : begin
              if (Count > 127)
              then begin // Copy byte dCount times.
                   if (BufferSize > 4)
                   then begin
                        for k := 0 to (dCount - 1)
                        do begin
                           FDibBits^[FIndex] := FCBuffer^[i];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+1];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+2];
                           inc(FIndex);
                           FDibBits^[FIndex] := FCBuffer^[i+3];
                           inc(FIndex);
                        end;
                        inc(i, 4);
                        dec(BufferSize, 5);
                   end;
                   dCount := 4 * dCount;
              end
              else begin // Copy bytes literally.
                   dCount := 4 * dCount;
                   if (BufferSize > dCount)
                   then begin
                        CopyMemory(@FDibBits^[FIndex], @FCBuffer^[i], dCount);
                        inc(i, dCount);
                        dec(BufferSize);
                        dec(BufferSize, dCount);
                        inc(FIndex, dCount);
                   end;
              end;
              inc(j, dCount);
            end;
       end;

       if (j >= FPackWidth)
       then begin // End of line. Calculate new index to bitmap data.
            if FFlipVert
            then dec(FRow)
            else inc(FRow);
            FIndex := FRow * FLongWidth;

            // Check for end of image.
            if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
            then i := BufferSize;

            // Swap R and B colour component to conform to Windows bitmap.
            if FSwapRB
            then begin
                 SwapByteRB(@FDibBits^[RGBIndex]);
                 if FFlipVert
                 then dec(RGBIndex, FLongWidth)
                 else inc(RGBIndex, FLongWidth);
            end;
            j := 0;
       end;
    end;
    // Caluclate decompressed data size.
    DataCount := abs(FIndex - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageTargaRLE.Decompress.


//------------------------------------------------------------------------------
// TmcmImagePCXRLE
//------------------------------------------------------------------------------

constructor TmcmImagePCXRLE.Create;
begin
  Inherited Create;
  FPlane     := 1;
  FPackWidth := 0;
  FSupColors := [IF_BW, IF_GREY4, IF_PAL4, IF_GREY8, IF_PAL8, IF_RGB24];
  FFlipVert  := True;
end; // TmcmImagePCXRLE.Create.


procedure TmcmImagePCXRLE.Clear;
begin
  Inherited Clear;
end; // TmcmImagePCXRLE.Clear.


procedure TmcmImagePCXRLE.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);

  // An initial guess
  FPackWidth := FDIBSection.dsBmih.biWidth;

  // Set-up planes
  case FDIBSection.dsBmih.biBitCount of
  1  : begin
         FPlane := 1;
         FNoPlanes := 1;
       end;
  4  : begin
         FPlane := 0;
         FNoPlanes := 4;
       end;
  8  : begin
         FPlane := 1;
         FNoPlanes := 1;
         if Odd(FPackWidth)
         then inc(FPackWidth);
       end;
  24 : begin
         FPlane := 2;
         FNoPlanes := 3;
         FIndex := FIndex + FPlane;
       end;
  end;
end; // TmcmImagePCXRLE.SetImage.


procedure TmcmImagePCXRLE.SetFlipVert(Value : boolean);
begin
  Inherited SetFlipVert(Value);
  case FDIBSection.dsBmih.biBitCount of
  4  : ; // No need to modify
  24 : FIndex  := FIndex + FPlane;
  end;
end; // TmcmImagePCXRLE.SetFlipVert.


procedure TmcmImagePCXRLE.SetPackedLineWidth(Value : cardinal);
begin
  // The actual PCX line width.
  FPackWidth := Value;
end; // TmcmImagePCXRLE.SetPackedLineWidth.


procedure TmcmImagePCXRLE.SetNoPlanes(NoPlanes : integer);
begin
  FNoPlanes := NoPlanes;
end; // TmcmImagePCXRLE.SetNoPlanes.


function TmcmImagePCXRLE.Compress(var Buffer     : pointer;
                                  var BufferSize : longword;
                                  var DataCount  : longword) : TmcmErrorCode;
const
    MaxRunLength = 63;
var i         : longint;
    j, n, k   : cardinal;
    Count     : cardinal;
    dCount    : cardinal;
    CountDown : longword;
    pLineData : PVectorB;
    BitPlane  : byte;
    BitMask   : byte;

begin
  FError := EC_OK;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  case FDIBSection.dsBmih.biBitCount of
  4,
  24 : pLineData := AllocMem(FDIBSection.dsBmih.biWidth);
  else pLineData := Nil;
  end;

  CountDown := DataCount;
  DataCount := 0;

  i := 0;
  try
    try
      while (CountDown > 0)
      do begin
         FCol := 0;
         FIndex := FRow * FLongWidth;

         // Check size of compress data buffer.
         if (longint(FDSize - FDIncSize) < i)
         then AllocCompressBuffer(0);

         // Get bitmap line / planes
         case FDIBSection.dsBmih.biBitCount of
         1,
         8  : begin
                pLineData := @FDibBits^[FIndex];
                if FFlipVert
                then dec(FRow)
                else inc(FRow);
                inc(DataCount, FLongWidth);
                dec(CountDown, FLongWidth);
              end;
         4  : begin
                BitPlane := 1 shl FPlane;
                BitMask  := $80;
                FillChar(pLineData^, FDIBSection.dsBmih.biWidth, 0);
                j := 0;

                while (j < FPackWidth)
                do begin
                   if (((FDibBits^[FIndex] shr 4) and BitPlane) <> 0)
                   then pLineData^[j] := pLineData^[j] + BitMask;
                   BitMask := BitMask shr 1;

                   if ((FDibBits^[FIndex] and BitPlane) <> 0)
                   then pLineData^[j] := pLineData^[j] + BitMask;
                   BitMask := BitMask shr 1;

                   if (BitMask = 0)
                   then begin
                        BitMask := $80;
                        inc(j);
                   end;
                   inc(FIndex, 1);
                end;

                inc(FPlane);
                if (FPlane > 3)
                then begin
                     FPlane := 0;
                     if FFlipVert
                     then dec(FRow)
                     else inc(FRow);
                     inc(DataCount, FLongWidth);
                     dec(CountDown, FLongWidth);
                end;
              end;
         24 : begin
                inc(FIndex, FPlane);
                for j := 0 to (FDIBSection.dsBmih.biWidth - 1)
                do begin
                   pLineData^[j] := FDibBits^[FIndex];
                   inc(FIndex, 3);
                end;
                dec(FPlane);
                if (FPlane < 0)
                then begin
                     FPlane := 2;
                     if FFlipVert
                     then dec(FRow)
                     else inc(FRow);
                     inc(DataCount, FLongWidth);
                     dec(CountDown, FLongWidth);
                end;
              end;
         end;

         //--------------------------------------------------------------------------
         // Write Compress line data to buffer.
         j := 0;
         while (j < FPackWidth)
         do begin
            n := j + 1;
            while (n < FPackWidth) and (pLineData^[j] = pLineData^[n])
            do inc(n);
            Count := n - j;
            if (Count > 1) // Equal pixels
            then begin
                 while (Count > 0)
                 do begin
                    if (Count > MaxRunLength)
                    then begin
                         k := Trunc(Count / MaxRunLength);
                         if (k = 1)
                         then dCount := Count div 2
                         else dCount := MaxRunLength;
                    end
                    else dCount := Count;

                    FDBuffer^[i] := dCount or $C0; // Count
                    inc(i);
                    FDBuffer^[i] := pLineData^[j]; // data
                    inc(i);

                    dec(Count, dCount);
                 end;

                 j := n;
            end
            else begin // Different pixels
                 if ((pLineData^[j] and $C0) = $C0)
                 then begin
                      FDBuffer^[i] := $C1; // Count
                      inc(i);
                      FDBuffer^[i] := pLineData^[j]; // data
                      inc(i);
                      inc(j);
                 end
                 else begin
                      FDBuffer^[i] := pLineData^[j]; // data
                      inc(i);
                      inc(j);
                 end;
            end;
         end;
      end;

    finally
      case FDIBSection.dsBmih.biBitCount of
      4, 24 : FreeMem(pLineData);
      end;
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImagePCXRLE.Compress.


function TmcmImagePCXRLE.Decompress(    Buffer     : pointer;
                                    var BufferSize : longword;
                                    var DataCount  : longword) : TmcmErrorCode;
var i, j, k, n : cardinal;
    Count      : byte;
    SaveIndex  : longint;
    dCount     : word;
    Value      : byte;
    nCount     : cardinal;
begin
  FError := EC_OK;
  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  try
    i := 0;

    case FDIBSection.dsBmih.biBitCount of
    4  : begin
           nCount := (2 * FNoPlanes) - 1;
           j := (FIndex mod FLongWidth) div 4;

           while (i < BufferSize)
           do begin
              Count := FCBuffer^[i];
              inc(i);
              case FNoPlanes of
              1 : begin
                    if ((Count and $C0) = $C0)
                    then begin
                         dCount := Count and $3F;
                         if (i < BufferSize)
                         then begin
                              FillChar(FDibBits^[FIndex], dCount, FCBuffer^[i]);
                              inc(i);
                              inc(j, dCount);
                              inc(FIndex, dCount);
                         end
                         else begin
                              dec(i);
                              break;
                         end;
                    end
                    else begin
                         FDibBits^[FIndex] := Count;
                         inc(j);
                         inc(FIndex);
                    end;
                  end;
              4 : begin
                    Value := 1 shl FPlane;
                    if ((Count and $C0) = $C0)
                    then begin
                         dCount := Count and $3F;
                         if (i < BufferSize)
                         then begin
                              for k := 1 to dCount
                              do begin
                                 for n := nCount downto 0
                                 do begin
                                    if ((FCBuffer^[i] and (1 shl n)) <> 0)
                                    then begin
                                         if Odd(n)
                                         then FDibBits^[FIndex] := FDibBits^[FIndex] + (Value shl 4)
                                         else FDibBits^[FIndex] := FDibBits^[FIndex] + Value;
                                    end;
                                    if Not(Odd(n))
                                    then inc(FIndex);
                                 end;
                              end;
                              inc(i);
                              inc(j, dCount);
                         end
                         else begin
                              dec(i);
                              break;
                         end;
                    end
                    else begin
                         for n := 7 downto 0
                         do begin
                            if ((Count and (1 shl n)) <> 0)
                            then begin
                                 if Odd(n)
                                 then FDibBits^[FIndex] := FDibBits^[FIndex] + (Value shl 4)
                                 else FDibBits^[FIndex] := FDibBits^[FIndex] + Value;
                            end;
                            if Not(Odd(n))
                            then inc(FIndex);
                         end;
                         inc(j);
                    end;
                  end;
              end;

              if (j >= FPackWidth)
              then begin // End of line. Calculate new index to bitmap data.
                   case FNoPlanes of
                   1 : begin
                         FPlane := 0;
                         if FFlipVert
                         then dec(FRow)
                         else inc(FRow);
                       end;
                   4 : begin
                         inc(FPlane);
                         if (FPlane > 3)
                         then begin
                              FPlane := 0;
                              if FFlipVert
                              then dec(FRow)
                              else inc(FRow);
                         end;
                      end;
                   end;
                   FIndex := FRow * FLongWidth;

                   // Check for end of image.
                   if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                   then break;
                   j := 0;
              end;
           end;
         end;
    1,
    8  : begin
           j := FIndex mod FLongWidth;
           while (i < BufferSize)
           do begin
              Count := FCBuffer^[i];
              inc(i);

              if ((Count and $C0) = $C0)
              then begin
                   dCount := Count and $3F;
                   if (i < BufferSize)
                   then begin
                        FillChar(FDibBits^[FIndex], dCount, FCBuffer^[i]);
                        inc(i);
                        inc(j, dCount);
                        inc(FIndex, dCount);
                   end
                   else begin
                        dec(i);
                        break;
                   end;
              end
              else begin
                   FDibBits^[FIndex] := Count;
                   inc(j);
                   inc(FIndex);
              end;

              if (j >= FPackWidth)
              then begin // End of line. Calculate new index to bitmap data.
                   if FFlipVert
                   then dec(FRow)
                   else inc(FRow);
                   FIndex := FRow * FLongWidth;

                   // Check for end of image.
                   if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                   then break;
                   j := 0;
              end;
           end;
         end;
    24 : begin
           j := ((FIndex - FPlane) mod FLongWidth) div 3;
           while (i < BufferSize)
           do begin
              Count := FCBuffer^[i];
              inc(i);
              if ((Count and $C0) = $C0)
              then begin
                   dCount := Count and $3F;
                   if (i < BufferSize)
                   then begin
                        for k := 1 to dCount
                        do begin
                           FDibBits^[FIndex] := FCBuffer^[i];
                           inc(FIndex, 3);
                           inc(j);

                           if (j >= FPackWidth)
                           then begin // End of line. Calculate new index to bitmap data.
                                dec(FPlane);
                                if (FPlane < 0)
                                then begin
                                     FPlane := 2;
                                     if FFlipVert
                                     then dec(FRow)
                                     else inc(FRow);
                                end;
                                FIndex := FRow * FLongWidth + FPlane;

                                // Check for end of image.
                                if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                                then break;
                                j := 0;
                           end;

                        end;
                        inc(i);
                   end
                   else begin
                        dec(i);
                        break;
                   end;
              end
              else begin
                   FDibBits^[FIndex] := Count;
                   inc(j);
                   inc(FIndex, 3);

                   if (j >= FPackWidth)
                   then begin // End of line. Calculate new index to bitmap data.
                        dec(FPlane);
                        if (FPlane < 0)
                        then begin
                             FPlane := 2;
                             if FFlipVert
                             then dec(FRow)
                             else inc(FRow);
                        end;
                        FIndex := FRow * FLongWidth + FPlane;

                        // Check for end of image.
                        if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                        then break;
                        j := 0;
                   end;

              end;

              // Check for end of image.
              if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
              then break;
           end;

         end;
    end;

    BufferSize := BufferSize - i;
    // Caluclate decompressed data size.
    DataCount := abs(FIndex - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImagePCXRLE.Decompress.


//------------------------------------------------------------------------------
// TmcmImageSGIRLE
//------------------------------------------------------------------------------

constructor TmcmImageSGIRLE.Create;
begin
  Inherited Create;
  FFlipVert  := False;
  FSupColors := [IF_GREY8, IF_RGB24, IF_RGBA32];
  FRGBBuffer := Nil;
end; // TmcmImageSGIRLE.Create.


procedure TmcmImageSGIRLE.Clear;
begin
  if (FRGBBuffer <> Nil) and
     ((FDIBSection.dsBmih.biBitCount = 24) or
      (FDIBSection.dsBmih.biBitCount = 32))
  then FreeMem(FRGBBuffer);
  FRGBBuffer := Nil;
  Inherited Clear;
end; // TmcmImageSGIRLE.Clear.


procedure TmcmImageSGIRLE.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);

  FRLECountEqual := CountEqual8;
  FRLECountDiff  := CountDiff8;

  // Best settings for 8 bit run-length encoding.
  // Samples: Fish.bmp, coloplast.bmp, yc-cam.bmp.
  FMinEqual  := 3;
  FOptEqual  := 3;
  FMinDiff   := 1;

  case FDIBSection.dsBmih.biBitCount of
   8 : FPlanes := 0;
  24 : FPlanes := 2;
  32 : FPlanes := 3;
  end;
  FPlaneInc := FPlanes + 1;

  if (FDIBSection.dsBmih.biBitCount in [24,32])
  then GetMem(FRGBBuffer, FLongWidth * SizeOf(Byte))
  else FRGBBuffer := Nil;
end; // TmcmImageSGIRLE.SetImage.


function TmcmImageSGIRLE.Compress(var Buffer     : pointer;
                                  var BufferSize : longword;
                                  var DataCount  : longword) : TmcmErrorCode;
const MaxRunLength = $7F;
var i, j, n   : longint;
    Rest      : integer;
    RLEIndex  : longint;
    CountDown : longword;
begin
  // Compress just "one" line at a time.
  FError := EC_OK;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  if (FRow >= FMaxRow)
  then begin // Start new plane.
       FRow := 0;
       dec(FPlanes);
  end;
  FIndex := FRow * FLongWidth + FPlanes;

  CountDown := DataCount;
  DataCount := 0;

  i := 0;
  try
    while (CountDown > 0)
    do begin
       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       if (FDIBSection.dsBmih.biBitCount in [24,32])
       then begin
            for j := 0 to (CountDown - 1)
            do begin
               FRGBBuffer^[j] := FDibBits^[Findex];
               inc(FIndex, FPlaneInc);
            end;
       end
       else FRGBBuffer := @FDibBits^[FIndex];

       // Get optimised Run length encoding.
       RLEIndex := CodeRLELine(FRLEStr, FRGBBuffer, 0, FDIBSection.dsBmih.biWidth);

       //-----------------------------------------------------------------------
       // Write Compress line data.
       j := 1;
       while (j <= RLEIndex)
       do begin
          if (FRLEStr[j].Count > 0)
          then begin
               // Check that "run" is less than 127.
               if (FRLEStr[j].Count > MaxRunLength)
               then begin
                    Rest := FRLEStr[j].Count;
                    n := Trunc(FRLEStr[j].Count / MaxRunLength);
                    if (n = 1)
                    then FRLEStr[j].Count := FRLEStr[j].Count div 2
                    else FRLEStr[j].Count := MaxRunLength;
                    dec(Rest, FRLEStr[j].Count);
               end
               else Rest := 0;

               if FRLEStr[j].Equal
               then begin // Same pixels
                    FDBuffer^[i] := FRLEStr[j].Count; // Count / Code bit 8 = 0.
                    inc(i);
                    FDBuffer^[i] := FRGBBuffer^[FRLEStr[j].Index]; // data
                    inc(i);
               end
               else begin // Different pixels
                    FDBuffer^[i] := $80 or FRLEStr[j].Count; // Count / Code bit 8 = 1.
                    inc(i);
                    CopyMemory(@FDBuffer^[i], @FRGBBuffer^[FRLEStr[j].Index], FRLEStr[j].Count);
                    inc(i, FRLEStr[j].Count);
               end;

               // If this "run" has remaining pixels, add these in next "run".
               if (Rest <> 0)
               then begin
                    inc(FRLEStr[j].Index, FRLEStr[j].Count);
                    FRLEStr[j].Count := Rest;
               end
               else inc(j);
          end
          else inc(j);
       end;

       inc(DataCount, FDIBSection.dsBmih.biWidth);
       dec(CountDown, FDIBSection.dsBmih.biWidth);
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  inc(FRow);
  Buffer := FDBuffer;

  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImageSGIRLE.Compress.


function TmcmImageSGIRLE.Decompress(    Buffer     : pointer;
                                    var BufferSize : longword;
                                    var DataCount  : longword) : TmcmErrorCode;
var i, j  : longint;
    Code  : byte;
    Count : integer;
begin
  // Decompress just "one" line at a time.
  FError   := EC_OK;
  FCBuffer := Buffer;
  try
    //
    if (FRow >= FMaxRow)
    then begin
         FRow := 0;
         dec(FPlanes);
    end;
    FIndex := FRow * FLongWidth + FPlanes;

    i := 0;
    while (longword(i) < BufferSize)
    do begin
       Count := FCBuffer^[i] and $7F;
       Code  := FCBuffer^[i] and $80;
       inc(i);
       if (Code <> 0)
       then begin // Copy Count bytes literally.
            for j := i to (i + Count - 1)
            do begin
               FDibBits^[FIndex] := FCBuffer^[j];
               inc(FIndex, FPlaneInc);
            end;
            inc(i, Count);
       end
       else begin // Copy byte Count times.
            for j := 0 to (Count - 1)
            do begin
               FDibBits^[FIndex] := FCBuffer^[i];
               inc(FIndex, FPlaneInc);
            end;
            inc(i);
       end;
    end;
    inc(FRow);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageSGIRLE.Decompress.

//------------------------------------------------------------------------------
// TmcmImageHuffman
//------------------------------------------------------------------------------

constructor TmcmImageHuffman.Create;
begin
  Inherited Create;
  FError := EC_OK;
  FSupColors := [{IF_BW, IF_GREY4, IF_PAL4,} IF_GREY8, IF_PAL8{, IF_RGB24}];
  FHuffmanTree  := Nil;
  FHuffmanCodes := Nil;
  FDistTable    := Nil;
end; // TmcmImageHuffman.Create.


destructor TmcmImageHuffman.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageHuffman.Destroy


procedure TmcmImageHuffman.Clear;
begin
  if Assigned(FHuffmanTree)
  then FreeMem(FHuffmanTree);
  FHuffmanTree := Nil;
  if Assigned(FHuffmanCodes)
  then FreeMem(FHuffmanCodes);
  FHuffmanCodes := Nil;
  if Assigned(FDistTable)
  then FreeMem(FDistTable);
  FDistTable := Nil;
  Inherited Clear;
end; // TmcmImageHuffman.Clear.


procedure TmcmImageHuffman.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);
  if (Value <> 0)
  then begin
       FNoColors := 1 shl FDIBSection.dsBmih.biBitCount;
       FHuffmanTree  := AllocMem((2 * FNoColors - 1) * SizeOf(THuffmanNode));
       FHuffmanCodes := AllocMem(FNoColors * SizeOf(THuffmanCode));
  end;
end; // TmcmImageHuffman.SetImage.


function TmcmImageHuffman.ColourDistribution : TmcmErrorCode;
var x, y, i  : longint;
begin
  if Assigned(FHuffmanTree)
  then begin
       FillMemory(FHuffmanTree, (2 * FNoColors - 1) * SizeOf(THuffmanNode), 0);
       for y := 0 to (FDIBSection.dsBmih.biHeight - 1)
       do begin
          i := y * FLongWidth;
          for x := 0 to (FDIBSection.dsBmih.biWidth - 1)
          do begin
             inc(FHuffmanTree^[FDibBits^[i]].Count);
             inc(i);
          end;
       end;
       FError := EC_OK;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmImageHuffman.ColourDistribution.


function TmcmImageHuffman.GetDistribution(var Buffer : pointer;
                                          var Size   : longword) : TmcmErrorCode;
var i : word;
begin
  FError := InitCompress;
  if (FError = EC_OK)
  then begin
       Size := FNoColors * SizeOf(cardinal);
       FDistTable := AllocMem(Size);
       if Assigned(FDistTable)
       then begin
            try
              for i := 0 to (FNoColors - 1)
              do FDistTable^[i] := FHuffmanTree^[i].Count;
              Buffer := FDistTable;
            except
              FError := EC_UNKNOWN;
            end;
       end
       else FError := EC_NOMEMORY;
  end;
  Result := FError;
end; // TmcmImageHuffman.GetDistribution.


function TmcmImageHuffman.SetDistribution(Buffer : pointer;
                                          Count  : longword) : TmcmErrorCode;
var i : word;
begin
  try
    FillMemory(FHuffmanTree, (2 * FNoColors - 1) * SizeOf(THuffmanNode), 0);
    for i := 0 to (FNoColors - 1)
    do FHuffmanTree^[i].Count := PVectorC(Buffer)^[i];
    FError := InitDecompress;
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmImageHuffman.SetDistribution.


function TmcmImageHuffman.Sort(const Item1, Item2 : pointer) : integer;
var Count1 : cardinal;
    Count2 : cardinal;
begin
  // Method used by TmcmPriorityQueue to sort FHuffmanTree elements.
  Count1 := FHuffmanTree^[integer(Item1)].Count;
  Count2 := FHuffmanTree^[integer(Item2)].Count;
  if (Count1 >= Count2)
  then Result := -1
  else if (Count1 = Count2)
       then Result := 0
       else Result := 1;
end; // TmcmImageHuffman.Sort.


procedure TmcmImageHuffman.BuildTree(var LastIndex : integer);
var i             : integer;
    Node1         : integer;
    Node2         : integer;
    PriorityQueue : TmcmPriorityQueue;
begin
  PriorityQueue := TmcmPriorityQueue.Create(Sort);
  try
    // Add all non-zero nodes to queue
    for i := (FNoColors - 1) downto 0
    do if (FHuffmanTree^[i].Count > 0)
       then PriorityQueue.Add(pointer(i));

    while (PriorityQueue.Count > 1)
    do begin
       Node1 := integer(PriorityQueue.Remove);
       Node2 := integer(PriorityQueue.Remove);
       inc(LastIndex);
       with FHuffmanTree^[LastIndex]
       do begin
          RightIndex := Node1;
          LeftIndex  := Node2;
          Count      := FHuffmanTree^[Node1].Count + FHuffmanTree^[Node2].Count;
       end;
       PriorityQueue.Add(pointer(LastIndex));
    end;
  finally
    PriorityQueue.Free;
  end;
end; // TmcmImageHuffman.BuildTree.


procedure TmcmImageHuffman.BuildLeaveCode(NodeIndex : integer;
                                          CodeStr   : THuffmanCodeStr);
var i         : word;
    ByteIndex : word;
    BitIndex  : word;
begin
  // Compile Huffman codes for each leave in the tree.
  if (NodeIndex >= FNoColors)
  then begin
       inc(CodeStr[0]);

       // Left node
       CodeStr[integer(CodeStr[0])] := '0';
       BuildLeaveCode(FHuffmanTree^[NodeIndex].LeftIndex, CodeStr);

       // Right node.
       CodeStr[integer(CodeStr[0])] := '1';
       BuildLeaveCode(FHuffmanTree^[NodeIndex].RightIndex, CodeStr);

       dec(CodeStr[0]);
  end
  else begin
       // At a leave in the tree.
       // Compose the bit code
       FHuffmanCodes^[NodeIndex].CodeLength := integer(CodeStr[0]);
       ByteIndex := 0;
       BitIndex  := 7;
       with FHuffmanCodes^[NodeIndex]
       do begin
          for i := 1 to CodeLength
          do begin
             if (CodeStr[i] = '1')
             then Code[ByteIndex] := Code[ByteIndex] or BitMask[BitIndex];

             if (BitIndex = 0)
             then begin
                  inc(ByteIndex);
                  BitIndex := 7;
             end
             else dec(BitIndex);
          end;
       end;
  end;
end; // TmcmImageHuffman.BuildLeaveCode.


procedure TmcmImageHuffman.BuildCodes(NodeIndex : integer);
var CodeStr : THuffmanCodeStr;
begin
  // Initialise CodeStr.
  CodeStr := '';

  // Clear HuffmanCodes.
  FillMemory(@FHuffmanCodes^[0], FNoColors * SizeOf(THuffmanCode), 0);

  // Calculate Huffman codes.
  BuildLeaveCode(NodeIndex, CodeStr);
end; // TmcmImageHuffman.BuildCodes.


procedure TmcmImageHuffman.ReadBits;
begin
end; // TmcmImageHuffman.ReadBits.


function TmcmImageHuffman.WriteBits(    Data        : PVectorB;
                                        DataSize    : cardinal;
                                        Buffer      : PVectorB;
                                    var BufferIndex : cardinal;
                                        Align       : TmcmDataAlign) : TmcmErrorCode;
var j, l      : cardinal;
    CodeIndex : word;
    CodeBit   : word;
    CodeByte  : byte;
    DataByte  : byte;
    DataBit   : word;
begin
  Result := EC_OK;
  try
    DataBit  := 7;
    DataByte := 0;

    for j := 0 to (DataSize - 1)
    do begin
       CodeBit   := 7;
       CodeIndex := 0;
       CodeByte  := FHuffmanCodes^[Data^[j]].Code[CodeIndex];

       for l := 0 to (FHuffmanCodes^[Data^[j]].CodeLength - 1)
       do begin
          if ((CodeByte and BitMask[CodeBit]) <> 0)
          then DataByte := DataByte or BitMask[DataBit];

          if (CodeBit = 0)
          then begin
               CodeBit := 7;
               inc(CodeIndex);
               CodeByte := FHuffmanCodes^[Data^[j]].Code[CodeIndex];
          end
          else dec(CodeBit);

          if (DataBit = 0)
          then begin
               Buffer^[BufferIndex] := DataByte;
               DataByte := 0;
               DataBit  := 7;
               inc(BufferIndex);
          end
          else dec(DataBit);
       end;
    end;

    // Add last
    if (DataBit <> 7)
    then begin
         Buffer^[BufferIndex] := DataByte;
         inc(BufferIndex);
    end;

    // Align compressed data line to...
    case Align of
    DA_BYTE  : ; // Data is already byte aligned.
    DA_WORD  : if Odd(BufferIndex)
               then begin
                    Buffer^[BufferIndex] := 0;
                    inc(BufferIndex);
               end;
    DA_DWORD : begin
                 j := BufferIndex mod 4;
                 inc(BufferIndex, j);
               end;
    end;
  except
    On Exception
    do Result := EC_BADCOMPRESS;
  end;
end; // TmcmImageHuffman.WriteBits.


function TmcmImageHuffman.InitCompress : TmcmErrorCode;
begin
  // Get data distribution.
  ColourDistribution;

  // Build a "binary" tree.
  FLastIndex := FNoColors - 1;
  BuildTree(FLastIndex);

  // Comput Huffman codes from "binary" tree.
  BuildCodes(FLastIndex);

  Result := FError;
end; // TmcmImageHuffman.InitCompress.


function TmcmImageHuffman.InitDecompress : TmcmErrorCode;
begin
  // Build a "binary" tree.
  FLastIndex := FNoColors - 1;
  BuildTree(FLastIndex);

  // Set root node as current node.
  FNodeIndex := FLastIndex;
  FCodeBit   := 0;

  Result := FError;
end; // TmcmImageHuffman.InitDecompress.


function TmcmImageHuffman.Compress(var Buffer     : pointer;
                                   var BufferSize : longword;
                                   var DataCount  : longword) : TmcmErrorCode;
var i : cardinal;
begin
  FError := EC_OK;

  if (FDBuffer = Nil)
  then AllocCompressBuffer(FImageSize div 2);

  DataCount := 0;
  i := 0;
  try
    FIndex := 0;
    while (FRow < FMaxRow) and (FError = EC_OK)
    do begin
       FCol := 0;
       FIndex := FRow * FLongWidth;

       // Check size of compress data buffer.
       if (cardinal(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       FError := WriteBits(@FDibBits^[Findex], FMaxCol, FDBuffer, i, DA_BYTE);

       // Increment row index.
       inc(FRow);

       inc(DataCount, FLongWidth);
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImageHuffman.Compress.


function TmcmImageHuffman.Decompress(    Buffer     : pointer;
                                     var BufferSize : longword;
                                     var DataCount  : longword): TmcmErrorCode;
var i, j          : longint;
    SaveIndex     : longint;
begin
  FError := EC_OK;
  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  try
    i := 0;
    j := FIndex mod FLongWidth;

    while (cardinal(i) < BufferSize)
    do begin
       repeat

       if (FCodeBit = 0)
       then begin
            // Get byte to decode.
            FCodeByte := FCBuffer^[i];
            inc(i);
            FCodeBit := 7;
       end
       else dec(FCodeBit);

       // Get bit for direction in HufffmanTree.
       if ((FCodeByte and BitMask[FCodeBit]) = 0)
       then FNodeIndex := FHuffmanTree^[FNodeIndex].LeftIndex
       else FNodeIndex := FHuffmanTree^[FNodeIndex].RightIndex;

       // If Node is a leaf node, output data to image.
       if (FNodeIndex < 256)
       then begin
            FDibBits^[FIndex] := FNodeIndex;
            inc(FIndex);
            // Set root node as current node.
            FNodeIndex := FLastIndex;

            inc(j);
            if (j >= FMaxCol)
            then begin
                 if FFlipVert
                 then dec(FRow)
                 else inc(FRow);
                 j := 0;
                 FCodeBit := 0;

                 // Check for end of image.
                 if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                 then break;
                 FIndex := FRow * FLongWidth;
            end;
       end;

       until (FCodeBit = 0)
    end;
    // Calculate remainder of buffer.
    BufferSize := BufferSize - cardinal(i);

    // Caluclate decompressed data size.
    DataCount := abs(FIndex - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageHuffman.Decompress.


//------------------------------------------------------------------------------
// TmcmImageModifiedHuffman
//------------------------------------------------------------------------------

constructor TmcmImageModifiedHuffman.Create;
begin
  Inherited Create;
  FWhiteTree  := Nil;
  FBlackTree  := Nil;
  FFlipVert   := True;
  FResetBit   := 0;
  FillOrder   := False;
  FSupColors  := [IF_BW];
end; // TmcmImageModifiedHuffman.Create.


destructor TmcmImageModifiedHuffman.Destroy;
begin
  Inherited Destroy;
end; // TmcmImageModifiedHuffman.Destroy.


procedure TmcmImageModifiedHuffman.Clear;
begin
  if Assigned(FWhiteTree)
  then FreeMem(FWhiteTree);
  FWhiteTree := Nil;
  if Assigned(FBlackTree)
  then FreeMem(FBlackTree);
  FBlackTree := Nil;
  Inherited Clear;
end; // TmcmImageModifiedHuffman.Clear.


procedure TmcmImageModifiedHuffman.SetFillOrder(Value : boolean);
begin
  FFillOrder := Value;
  case FFillOrder of
  False : begin
            FGetRunLength := GetRunLengthDown;
            FResetBit     := 0;
          end;
  True  : begin
            FGetRunLength := GetRunLengthUp;
            FResetBit     := 8;
          end;
  end;
end; // TmcmImageModifiedHuffman.SetFillOrder.


procedure TmcmImageModifiedHuffman.SetImage(Value : HBITMAP);
begin
  Inherited SetImage(Value);
end; // TmcmImageModifiedHuffman.SetImage.


procedure TmcmImageModifiedHuffman.BuildTree(var LastIndex  : integer;
                                                 Data       : array of THuffmanCodeLen;
                                                 Count      : word;
                                                 TheTree    : PHuffmanTree;
                                                 Multiplier : word;
                                                 Offset     : word);
var i, j, k   : word;
    Index     : word;
    TreeCount : word;
begin
  if (LastIndex = 0)
  then TreeCount := 1
  else TreeCount := LastIndex;
  for i := 0 to (Count - 1)
  do begin
     Index := 0;
     k := 15;
     for j := 0 to (Data[i].Len - 1)
     do begin
        if ((Data[i].Code and BitMask[k]) = 0)
        then begin
             if (TheTree^[Index].LeftIndex = 0)
             then begin
                  TheTree^[Index].LeftIndex := TreeCount;
                  TheTree^[Index].Count := $FFFE;
                  inc(TreeCount);
             end;
             Index := TheTree^[Index].LeftIndex;
        end
        else begin
             if (TheTree^[Index].RightIndex = 0)
             then begin
                  TheTree^[Index].RightIndex := TreeCount;
                  TheTree^[Index].Count := $FFFE;
                  inc(TreeCount);
             end;
             Index := TheTree^[Index].RightIndex;
        end;
        dec(k);
     end;
     TheTree^[Index].Count := Multiplier * i + Offset; // Data[i].Code;
  end;
  LastIndex := TreeCount;
end; // TmcmImageModifiedHuffman.BuildTree.


procedure TmcmImageModifiedHuffman.FillBits(RunLength : cardinal);
var ThisRun : cardinal;
begin
  if (cardinal(FDataBit) < RunLength)
  then ThisRun := FDataBit
  else ThisRun := RunLength;

  // Fill bit's in current byte
  if (0 < ThisRun) and (ThisRun < 8)
  then begin
       dec(FDataBit, ThisRun);
       if (FCodeRun = CR_BLACK)
       then FDibBits^[FIndex] := FDibBits^[FIndex] or (((1 shl ThisRun) - 1) shl FDataBit);
       if (FDataBit = 0)
       then begin
            inc(FIndex);
            FDataBit := 8;
       end;
       dec(RunLength, ThisRun);
  end;

  // Fill entire bytes whenever possible
  if (RunLength > 7)
  then begin
    if (FCodeRun = CR_BLACK)
    then FillChar(FDibBits^[FIndex], RunLength div 8, $FF);
    inc(FIndex, RunLength div 8);
    RunLength := RunLength mod 8;
  end;

  // finally fill remaining bits
  if (RunLength > 0)
  then begin
       FDataBit := 8 - RunLength;
       if (FCodeRun = CR_BLACK)
       then FDibBits^[FIndex] := ((1 shl RunLength) - 1) shl FDataBit;
  end;
end; // TmcmImageModifiedHuffman.FillBits.


function TmcmImageModifiedHuffman.GetRunLengthDown : cardinal;
begin
  FNodeIndex := 0;
  repeat
    if (FCodeBit = 0)
    then begin
         if (FCodeIndex >= FCodeSize)
         then begin
              Result := $FFFF;
              Exit;
         end;

         // Get byte to decode.
         FCodeByte := FCBuffer^[FCodeIndex];
         inc(FCodeIndex);
         FCodeBit := 7;
    end
    else dec(FCodeBit);

    // Get bit for direction in HufffmanTree.
    if ((FCodeByte and BitMask[FCodeBit]) = 0)
    then FNodeIndex := FCurTree[FCodeRun]^[FNodeIndex].LeftIndex
    else FNodeIndex := FCurTree[FCodeRun]^[FNodeIndex].RightIndex;
  until (FCurTree[FCodeRun]^[FNodeIndex].Count <> $FFFE);
  Result := FCurTree[FCodeRun]^[FNodeIndex].Count;
end; // TmcmImageModifiedHuffman.GetRunLengthDown.


function TmcmImageModifiedHuffman.GetRunLengthUp : cardinal;
begin
  FNodeIndex := 0;
  repeat
    if (FCodeBit = 8)
    then begin
         if (FCodeIndex >= FCodeSize)
         then begin
              Result := $FFFF;
              Exit;
         end;

         // Get byte to decode.
         FCodeByte := FCBuffer^[FCodeIndex];
         inc(FCodeIndex);
         FCodeBit := 0;
    end;

    // Get bit for direction in HufffmanTree.
    if ((FCodeByte and BitMask[FCodeBit]) = 0)
    then FNodeIndex := FCurTree[FCodeRun]^[FNodeIndex].LeftIndex
    else FNodeIndex := FCurTree[FCodeRun]^[FNodeIndex].RightIndex;
    inc(FCodeBit);
  until (FCurTree[FCodeRun]^[FNodeIndex].Count <> $FFFE);
  Result := FCurTree[FCodeRun]^[FNodeIndex].Count;
end; // TmcmImageModifiedHuffman.GetRunLengthUp.


function TmcmImageModifiedHuffman.InitDecompress : TmcmErrorCode;
begin
  FWhiteTree  := AllocMem((255) * SizeOf(THuffmanNode));
  FBlackTree  := AllocMem((255) * SizeOf(THuffmanNode));

  if Assigned(FWhiteTree) and
     Assigned(FBlackTree)
  then begin
       FillMemory(FWhiteTree, (255) * SizeOf(THuffmanNode), $0);
       FillMemory(FBlackTree, (255) * SizeOf(THuffmanNode), $0);

       // Create white run-length tree
       FLastIndex := 0;
       BuildTree(FLastIndex, WhiteTermCodes, 64, FWhiteTree, 1, 0);
       BuildTree(FLastIndex, WhiteMakeUp, 27, FWhiteTree, 64, 64);
       BuildTree(FLastIndex, WhiteMakeUp[27], 1, FWhiteTree, 1, $FFFF);
       BuildTree(FLastIndex, WandBMakeUp, 13, FWhiteTree, 64, 1792);

       // Create black run-length tree
       FLastIndex := 0;
       BuildTree(FLastIndex, BlackTermCodes, 64, FBlackTree, 1, 0);
       BuildTree(FLastIndex, BlackMakeUp, 27, FBlackTree, 64, 64);
       BuildTree(FLastIndex, BlackMakeUp[27], 1, FBlackTree, 1, $FFFF);
       BuildTree(FLastIndex, WandBMakeUp, 13, FBlackTree, 64, 1792);

       FCurTree[CR_WHITE] := FWhiteTree;
       FCurTree[CR_BLACK] := FBlackTree;

       // Set root node as current node.
       FNodeIndex := 0;
       FCodeBit   := FResetBit; // 0;
       FDataBit   := 8;
  end
  else FError := EC_NOMEMORY;
  Result := FError;
end; // TmcmImageModifiedHuffman.InitDecompress.


function TmcmImageModifiedHuffman.WriteRunCodes(var Index       : longint;
                                                    RunLength   : cardinal;
                                                    MakeUpCodes : array of THuffmanCodeLen;
                                                    TermCodes   : array of THuffmanCodeLen) : TmcmErrorCode;
var i, j      : longint;
    ThisIndex : cardinal;
begin
  try
    // Check for run-length larger than 1792.
    while (RunLength >= 1792 (* WandBMakeUp[0] *))
    do begin
       ThisIndex := Trunc((RunLength - 1792) / 64.0);
       if (ThisIndex > 12)
       then ThisIndex := 12;

       // Write code to compress buffer.
       j := 15;
       for i := 1 to WandBMakeUp[ThisIndex].Len
       do begin
          if ((WandBMakeUp[ThisIndex].Code and BitMask[j]) <> 0)
          then FCodeByte := FCodeByte or BitMask[FCodeBit];
          if (FCodeBit = 0)
          then begin
               FDBuffer[Index] := FCodeByte;
               inc(Index);
               FCodeByte := 0;
               FCodeBit := 7;
          end
          else dec(FCodeBit);

          dec(j);
       end;
       // Decrement Run-length by writen codes size.
       RunLength := RunLength - (ThisIndex * 64 + 1792);
    end;

    // Check for run-length larger than 64.
    while (RunLength >= 64 (* White- Black-MakeUp[0] *))
    do begin
       ThisIndex := Trunc((RunLength - 64) / 64.0);
       if (ThisIndex > 26)
       then ThisIndex := 26;

       // Write code to compress buffer.
       j := 15;
       for i := 1 to MakeUpCodes[ThisIndex].Len
       do begin
          if ((MakeUpCodes[ThisIndex].Code and BitMask[j]) <> 0)
          then FCodeByte := FCodeByte or BitMask[FCodeBit];
          if (FCodeBit = 0)
          then begin
               FDBuffer[Index] := FCodeByte;
               inc(Index);
               FCodeByte := 0;
               FCodeBit := 7;
          end
          else dec(FCodeBit);

          dec(j);
       end;
       // Decrement Run-length by writen codes size.
       RunLength := RunLength - ((ThisIndex + 1) * 64);
    end;

    // Check for run-length larger than or equal to 0.

    // Write code to compress buffer.
    j := 15;
    for i := 1 to TermCodes[RunLength].Len
    do begin
       if ((TermCodes[RunLength].Code and BitMask[j]) <> 0)
       then FCodeByte := FCodeByte or BitMask[FCodeBit];
       if (FCodeBit = 0)
       then begin
            FDBuffer[Index] := FCodeByte;
            inc(Index);
            FCodeByte := 0;
            FCodeBit := 7;
       end
       else dec(FCodeBit);
       dec(j);
    end;
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmImageModifiedHuffman.WriteRunCodes.


function TmcmImageModifiedHuffman.Compress(var Buffer     : pointer;
                                           var BufferSize : longword;
                                           var DataCount  : longword) : TmcmErrorCode;
var i, j      : longint;
    CountDown : longword;
    RunLength : cardinal;
begin
  if (FDBuffer = Nil)
  then begin
       AllocCompressBuffer(FImageSize div 4);
       FCodeBit      := 7;
       FCodeByte     := 0;
       FDataBit      := 7;
       FDataByte     := 0;
  end;
  CountDown := DataCount;
  DataCount := 0;
  i := 0;
  try
    while (CountDown > 0)
    do begin
       FCol := 0;
       FIndex   := FRow * FLongWidth;

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       //-----------------------------------------------------------------------
       // Write Compress line data.
       j := 0;
       while (j < FMaxCol)
       do begin
          // Check for white bits, and get run-length
          RunLength := 0;
          while (j < FMaxCol) and ((FDibBits^[FIndex] and BitMask[FDataBit]) = 0)
          do begin
             inc(RunLength);
             if (FDataBit = 0)
             then begin
                  inc(FIndex);
                  FDataBit := 7;
             end
             else dec(FDataBit);
             inc(j);
          end;

          // Write white codes.
          WriteRunCodes(i, RunLength, WhiteMakeUp, WhiteTermCodes);

          if (j < FMaxCol)
          then begin
               // Check for black bits, and get run-length
               RunLength := 0;
               while (j < FMaxCol) and ((FDibBits^[FIndex] and BitMask[FDataBit]) <> 0)
               do begin
                  inc(RunLength);
                  if (FDataBit = 0)
                  then begin
                       inc(FIndex);
                       FDataBit := 7;
                  end
                  else dec(FDataBit);
                  inc(j);
               end;

               // Write white codes.
               WriteRunCodes(i, RunLength, BlackMakeUp, BlackTermCodes);
          end;
       end;

       if (FCodeBit <> 7)
       then begin
            FDBuffer[i] := FCodeByte;
            inc(i);
       end;

       if FFlipVert
       then dec(FRow)
       else inc(FRow);
       FIndex := FRow * FLongWidth;

       FCodeBit      := 7;
       FCodeByte     := 0;
       FDataBit      := 7;
       FDataByte     := 0;

       inc(DataCount, FLongWidth);
       dec(CountDown, FLongWidth);
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImageModifiedHuffman.Compress.


function TmcmImageModifiedHuffman.Decompress(    Buffer     : pointer;
                                             var BufferSize : longword;
                                             var DataCount  : longword) : TmcmErrorCode;
var j         : longint;
    SaveIndex : longint;
    RunLength : cardinal;
begin
  FError := EC_OK;
  if (FWhiteTree = Nil) or (FBlackTree = Nil)
  then FError := InitDecompress;

  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  FCodeSize := BufferSize;
  try
    FCodeIndex := 0;
    j := FIndex mod FLongWidth;

    FCodeRun := CR_WHITE;
    while (cardinal(FCodeIndex) < BufferSize) and (FError = EC_OK)
    do begin
       repeat
         RunLength := FGetRunLength;

         // If Node is a leaf node, output data to image.
         if (RunLength < $FFFE)
         then begin
              inc(j, RunLength);

              if (j > FMaxCol)
              then begin
                   FError := EC_BADCOMPRESS;
                   Break;
              end;

              // Fill run-length number of bits
              FillBits(RunLength);

              // Set root node as current node.
              FNodeIndex := 0;

              if (RunLength < 64)
              then begin
                   case FCodeRun of
                   CR_WHITE : FCodeRun := CR_BLACK;
                   CR_BLACK : FCodeRun := CR_WHITE;
                   end;

                   if (j >= FMaxCol)
                   then begin
                        FCodeRun := CR_WHITE;
                        if FFlipVert
                        then dec(FRow)
                        else inc(FRow);
                        j := 0;

                        // Check for end of image.
                        if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
                        then break;
                        FIndex := FRow * FLongWidth;

                        FCodeBit  := FResetBit; 
                        FDataBit  := 8;
                   end;
              end;
         end
         else begin
              if (RunLength = $FFFF)
              then begin
                   FCodeBit := FResetBit;
                   FDataBit := 8;
                   Break;
              end
              else begin
                   // We have troubles!!!
                   FError := EC_BADCOMPRESS;
                   Break;
              end;
         end;
       until (FCodeBit = 0)// or (cardinal(FCodeIndex) >= BufferSize)
    end;

    // Calculate remainder of buffer.
    BufferSize := BufferSize - cardinal(FCodeIndex);

    // Caluclate decompressed data size.
    DataCount := abs(FIndex - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageModifiedHuffman.Decompress.


//------------------------------------------------------------------------------
// TmcmImageCCITT
//------------------------------------------------------------------------------

constructor TmcmImageCCITT.Create;
begin
  Inherited Create;
  FCCITTScheme  := CCITT_MH;
  FSupColors    := [IF_BW];
  FByteAlgned   := False;
  FUncompressed := False;
  FCodeTree     := Nil;
  Fa0           := 0;
  Fb1           := 0;
end; // TmcmImageCCITT.Create.


procedure TmcmImageCCITT.Clear;
begin
  if Assigned(FCodeTree)
  then FreeMem(FCodeTree);
  FCodeTree := Nil;
  Inherited Clear;
end; // TmcmImageCCITT.Clear.


function TmcmImageCCITT.InitDecompress : TmcmErrorCode;
begin
  FError := Inherited InitDecompress;
  if (FCCITTScheme <> CCITT_MH) and (FCodeTree = Nil)
  then begin
       FCodeTree  := AllocMem((32) * SizeOf(THuffmanNode));
       if Assigned(FCodeTree)
       then begin
            FillMemory(FCodeTree, (32) * SizeOf(THuffmanNode), $0);

            // Create Mode (direction) tree.
            FLastIndex := 0;
            BuildTree(FLastIndex, ModeTermCodes, 11, FCodeTree, 1, 0);

            FCurTree[CR_CODE] := FCodeTree;
       end
       else FError := EC_NOMEMORY;
  end;
  Result := FError;
end; // TmcmImageCCITT.InitDecompress.


procedure TmcmImageCCITT.SetByteAlgned(Value : boolean);
begin
  FByteAlgned := Value;
end; // TmcmImageCCITT.SetByteAlgned.


procedure TmcmImageCCITT.SetCCITTScheme(Value : TmcmCCITTScheme);
begin
  FCCITTScheme := Value;
end; // TmcmImageCCITT.SetCCITTScheme.


procedure TmcmImageCCITT.SetUncompressed(Value : boolean);
begin
  FUncompressed := Value;
end; // TmcmImageCCITT.SetUncompressed.


procedure TmcmImageCCITT.WriteEOL(var Index : longint);
var i, j : longint;
begin
  if (FByteAlgned)
  then begin
       // Align EOL so that the next code begins one a byte boundary.
       case FCodeBit of
       0..2 : begin
                FDBuffer[Index] := FCodeByte;
                inc(Index);
                FCodeByte := 0;
                FCodeBit := 3;
              end;
       3    : ;
       4..7 : FCodeBit := 3;
       end;
  end;

  // Write code to compress buffer.
  j := 15;
  for i := 1 to WhiteMakeUp[27].Len
  do begin
     if ((WhiteMakeUp[27].Code and BitMask[j]) <> 0)
     then FCodeByte := FCodeByte or BitMask[FCodeBit];
     if (FCodeBit = 0)
     then begin
          FDBuffer[Index] := FCodeByte;
          inc(Index);
          FCodeByte := 0;
          FCodeBit := 7;
     end
     else dec(FCodeBit);
     dec(j);
  end;

  if (FCCITTScheme = CCITT_MR)
  then begin
       if (FScheme <> CCITT_MMR)
       then FCodeByte := FCodeByte or BitMask[FCodeBit];
       if (FCodeBit = 0)
       then begin
            FDBuffer[Index] := FCodeByte;
            inc(Index);
            FCodeByte := 0;
            FCodeBit := 7;
       end
       else dec(FCodeBit);
  end;
end; // TmcmImageCCITT.WriteEOL.


function TmcmImageCCITT.WriteMHLine(var Index : longint) : cardinal;
var j         : longint;
    RunLength : cardinal;
begin
  j := 0;
  while (j < FMaxCol)
  do begin
     // Check for white bits, and get run-length
     RunLength := 0;

     while (j < FMaxCol) and ((FDibBits^[FIndex] and BitMask[FDataBit]) = 0)
     do begin
        inc(RunLength);
        if (FDataBit = 0)
        then begin
             inc(FIndex);
             FDataBit := 7;
        end
        else dec(FDataBit);
        inc(j);
     end;

     // Write white codes.
     WriteRunCodes(Index, RunLength, WhiteMakeUp, WhiteTermCodes);

     if (j < FMaxCol)
     then begin
          // Check for black bits, and get run-length
          RunLength := 0;
          while (j < FMaxCol) and ((FDibBits^[FIndex] and BitMask[FDataBit]) <> 0)
          do begin
             inc(RunLength);
             if (FDataBit = 0)
             then begin
                  inc(FIndex);
                  FDataBit := 7;
             end
             else dec(FDataBit);
             inc(j);
          end;

          // Black white codes.
          WriteRunCodes(Index, RunLength, BlackMakeUp, BlackTermCodes);
     end;
  end;
  Fa0 := j;
  Result := j;
end; // TmcmImageCCITT.WriteMHLine.


function TmcmImageCCITT.WriteMRLine(var Index : longint) : cardinal;
var RunLength  : longint;
    ColorRun   : TCodeRun;
    a1, a2, b2 : longint;
begin
  Fa0 := -1;
  ColorRun := CR_WHITE;
  while (Fa0 < FMaxCol)
  do begin
     // Detect a1
     a1 := Fa0;
     NextTransition(a1, FRow);

     // Detect b1
     CheckB1(Fb1, Fa0, byte(ColorRun));

     // Detect b2
     b2 := Fb1;
     if (FRefLine <> FRow) and (b2 < FMaxCol)
     then NextTransition(b2, FRefLine);

     // Is b2 to the left of a1
     if (b2 < a1)
     then begin
          // Pass mode, write pass code.
          WriteRunCodes(Index, 0, ModeTermCodes, ModeTermCodes);
          Fa0 := b2;
     end
     else begin
          // Is |a1 - b1| <= 3
          RunLength := a1 - Fb1;
          if (abs(RunLength) <= 3)
          then begin
               // Vertical mode
               if (RunLength >= 0)
               then inc(RunLength, 2)            // Vertical Right V0, VR(1..3)
               else RunLength := abs(RunLength) +  5; // Vertical Left VL(1..3)
               WriteRunCodes(Index, RunLength, ModeTermCodes, ModeTermCodes);
               Fa0 := a1;
          end
          else begin
               // Horizontal mode
               WriteRunCodes(Index, 1, ModeTermCodes, ModeTermCodes);

               // Detect a2
               a2 := a1;
               NextTransition(a2, FRow);

               // Write first run-length (a1 - a0).
               RunLength := a1 - Fa0;

               if (ColorRun = CR_BLACK)
               then begin
                    WriteRunCodes(Index, RunLength, BlackMakeUp, BlackTermCodes);
                    ColorRun := CR_WHITE;
               end
               else begin
                    WriteRunCodes(Index, RunLength, WhiteMakeUp, WhiteTermCodes);
                    ColorRun := CR_BLACK;
               end;

               // Write second run-length (a2 - a1).
               RunLength := a2 - a1;
               if (ColorRun = CR_BLACK)
               then WriteRunCodes(Index, RunLength, BlackMakeUp, BlackTermCodes)
               else WriteRunCodes(Index, RunLength, WhiteMakeUp, WhiteTermCodes);

               Fa0 := a2;
          end;
          if (ColorRun = CR_BLACK)
          then ColorRun := CR_WHITE
          else ColorRun := CR_BLACK;
     end;
  end;
  Result := Fa0;
end; // TmcmImageCCITT.WriteMRLine.


function TmcmImageCCITT.NextTransition(var Col, Row : longint) : longint;
var Index    : longint;
    Bits     : word;
    RefColor : byte;
begin
  // Used in CCITT Group 3 two dimensional and Group 4 (MR & MMR).
  if (Col < 0)
  then begin
       Col := 0;
       Index := Row * FLongWidth + (Col div 8);
       Bits  := 7 - (Col mod 8);
       RefColor := 0; // White color
  end
  else begin
       Index := Row * FLongWidth + (Col div 8);
       Bits  := 7 - (Col mod 8);
       RefColor := (FDibBits^[Index] shr Bits) and $01;
  end;

  while (Col < FMaxCol) and
        (((FDibBits^[Index] shr Bits) and $01) = RefColor)
  do begin
     if (Bits = 0)
     then begin
          if (Col < FMaxCol - 1)
          then begin
               inc(Index);
               Bits := 7;
          end;
     end
     else dec(Bits);
     inc(Col);
  end;
  Result := (FDibBits^[Index] shr Bits) and $01;
end; // TmcmImageCCITT.NextTransition.


procedure TmcmImageCCITT.CheckB1(var b1 : longint; var a0 : longint; Color : byte);
var RefIndex : longint;
    RefBit   : word;
    RefColor : byte;
begin
  // Used in CCITT Group 3 two dimensional and Group 4 (MR & MMR).
  if (FRefLine <> FRow)
  then begin
       if (a0 < 0)
       then b1 := 0
       else b1 := a0;

       RefIndex := FRefLine * FLongWidth + (b1 div 8);
       RefBit   := 7 - (b1 mod 8);
       if (a0 < 0)
       then begin
            RefColor := 0; // White color
            a0 := 0;
       end
       else RefColor := (FDibBits^[RefIndex] shr RefBit) and $01;

       repeat
         while (b1 < FMaxCol) and
               (((FDibBits^[RefIndex] shr RefBit) and $01) = RefColor)
         do begin
            if (RefBit = 0)
            then begin
                 inc(RefIndex);
                 RefBit := 7;
            end
            else dec(RefBit);
            inc(b1);
         end;
         if (b1 < FMaxCol)
         then RefColor := (FDibBits^[RefIndex] shr RefBit) and $01;
       until (RefColor <> Color) or (b1 >= FMaxCol);
  end
  else if (a0 < 0)
       then a0 := 0;
end; // TmcmImageCCITT.CheckB1.


function TmcmImageCCITT.Compress(var Buffer     : pointer;
                                 var BufferSize : longword;
                                 var DataCount  : longword) : TmcmErrorCode;
var i         : longint;
    CountDown : longword;
    Line2D    : integer;
begin
  FScheme := FCCITTScheme;
  i := 0;
  Line2D := 0;
  if (FDBuffer = Nil)
  then begin
       AllocCompressBuffer(FImageSize div 4);
       FCodeBit      := 7;
       FCodeByte     := 0;
       FDataBit      := 7;
       FDataByte     := 0;
       if (FCCITTScheme <> CCITT_MMR)
       then WriteEOL(i);
  end;

  CountDown := DataCount;
  DataCount := 0;
  FRefLine  := FRow;
  Fb1       := FMaxCol;
  try
    while (CountDown > 0) and (FError = EC_OK)
    do begin
       FCol := 0;
       FIndex := FRow * FLongWidth;

       // Check size of compress data buffer.
       if (longint(FDSize - FDIncSize) < i)
       then AllocCompressBuffer(0);

       //-----------------------------------------------------------------------
       // Write Compress line data.
       case FScheme of
       CCITT_MH  : begin
                     WriteMHLine(i);
                   end;
       CCITT_MR  : begin
                     WriteMHLine(i);
                     FScheme := CCITT_MMR;
                     Line2D  := 0;
                   end;
       CCITT_MMR : begin
                     WriteMRLine(i);
                     if (FCCITTScheme = CCITT_MR)
                     then begin
                          inc(Line2D);
                          if (Line2D = 3)
                          then FScheme := CCITT_MR;
                     end;
                   end;
       else begin
            FError := EC_DECOMPRESSION;
       end;
       end;

       // Write End Of Line.
       if (FCCITTScheme <> CCITT_MMR)
       then WriteEOL(i);

       // Change reference (G3 & 4 in 2-D) line
       FRefLine := FRow;

       // Get next line to process.
       if FFlipVert
       then dec(FRow)
       else inc(FRow);
       FIndex := FRow * FLongWidth;
       FDataBit  := 7;
       FDataByte := 0;
       Fb1 := 0;

       inc(DataCount, FLongWidth);
       dec(CountDown, FLongWidth);
    end;
    if (FCCITTScheme = CCITT_MMR)
    then begin
         if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
         then begin
              // Write 2 x EOL
              WriteEOL(i);
              WriteEOL(i);
         end;
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;

  Buffer := FDBuffer;
  if (FError = EC_OK)
  then BufferSize := i
  else BufferSize := 0;

  Result := FError;
end; // TmcmImageCCITT.Compress.


function TmcmImageCCITT.ReadMHLine : cardinal;
var j         : longint;
    RunLength : cardinal;
begin
  j := 0;
  FCodeRun := CR_WHITE;
  RunLength := 0;
  while (j < FMaxCol) and (RunLength < $FFFF) and (FError = EC_OK)
  do begin
     RunLength := FGetRunLength;

     // If Node is a leaf node, output data to image.
     if (RunLength < $FFFE)
     then begin
          inc(j, RunLength);

          // Fill run-length number of bits
          FillBits(RunLength);

          // Set root node as current node.
          FNodeIndex := 0;

          if (RunLength < 64)
          then begin
               case FCodeRun of
               CR_WHITE : FCodeRun := CR_BLACK;
               CR_BLACK : FCodeRun := CR_WHITE;
               end;
          end;
     end;
  end;
  Fa0 := j;

  if (j > FMaxCol)
  then FError := EC_DECOMPRESSION;
  Result := RunLength;
end; // TmcmImageCCITT.ReadMHLine.


function TmcmImageCCITT.ReadMRLine : cardinal;
var i, j       : longint;
    ThisLength : cardinal;
    RunLength  : cardinal;
    Code       : cardinal;
    ColorRun   : TCodeRun;
    a1, b2     : longint;
begin
  ColorRun := CR_WHITE;
  j := 0;
  RunLength := 0;
  while (j < FMaxCol) and (RunLength < $FFFF) and (FError = EC_OK)
  do begin
     FCodeRun := CR_CODE;
     Code := FGetRunLength;
     case Code of
     0    : begin // Pass
              CheckB1(Fb1, Fa0, byte(ColorRun));
              b2 := Fb1;
              if (FRefLine <> FRow)
              then NextTransition(b2, FRefLine);
              RunLength := b2 - Fa0;
              inc(j, RunLength);
              FCodeRun := ColorRun;
              FillBits(RunLength);
              Fa0 := j;
              RunLength := $FFFE;
            end;
     1    : begin // Horizontal
              i := 0;
              while (i < 2)
              do begin
                 FCodeRun  := ColorRun;
                 RunLength := 0;
                 repeat
                   ThisLength := FGetRunLength;
                   inc(RunLength, ThisLength);
                 until (ThisLength < 64) or (ThisLength = $FFFF);
                 if (RunLength = $FFFF) // EOL marker
                 then Break;
                 inc(j, RunLength);

                 // Fill run-length number of bits
                 FillBits(RunLength);

                 if (ColorRun = CR_BLACK)
                 then ColorRun := CR_WHITE
                 else ColorRun := CR_BLACK;

                 inc(i);
              end;
              Fa0 := j;
              if (RunLength <> $FFFF)
              then RunLength := $FFFE;
            end;
     2..5 : begin // Vertical VR(0..3)
              CheckB1(Fb1, Fa0, byte(ColorRun));
              a1 := Fb1 + (longint(Code) - 2);
              Runlength := a1 - Fa0;
            end;
     6..8 : begin // Vertical VL(1..3)
              CheckB1(Fb1, Fa0, byte(ColorRun));
              a1 := Fb1 - (longint(Code) - 5);
              RunLength := a1 - Fa0;
            end;
     9    : begin // Extension 2-D
              FError := EC_COMPRESSION;
              Fa0 := 0;
              RunLength := $FFFE;
            end;
     10   : begin // Extension 1-D
              FError := EC_COMPRESSION;
              Fa0 := 0;
              RunLength := $FFFE;
            end;
     11   : begin // EOL
              RunLength := $FFFF;
            end;
     else   begin
              RunLength := $FFFE;
              FError := EC_BADDECOMPRESS;
            end;
     end;

     if (RunLength < $FFFE) and (j < FMaxCol)
     then begin
          // Fill run-length number of bits
          FCodeRun := ColorRun;
          inc(j, RunLength);
          FillBits(RunLength);
          Fa0 := j;
          RunLength := 0;

          if (ColorRun = CR_BLACK)
          then ColorRun := CR_WHITE
          else ColorRun := CR_BLACK;
     end;
  end;

  if (j > FMaxCol)
  then FError := EC_DECOMPRESSION;
  Result := RunLength;
end; // TmcmImageCCITT.ReadMRLine.


function TmcmImageCCITT.SyncEOL(BufferSize : cardinal) : TmcmErrorCode;
var Count : integer;
begin
  // Basically read until the EOL code is found.
  if (FEOLCount = 0)
  then begin
       Count := 0;
       while (Count < 11) and (FCodeIndex < BufferSize) and (FError = EC_OK)
       do begin
          if ((FCodeBit = 0) and (FFillOrder = False)) or
             ((FCodeBit = 8) and (FFillOrder = True))
          then begin
               // Get byte to decode.
               FCodeByte := FCBuffer^[FCodeIndex];
               inc(FCodeIndex);
               if FFillOrder
               then FCodeBit := 0
               else FCodeBit := 7;
          end
          else if Not(FFillOrder)
               then dec(FCodeBit);

          // Get bit for direction in HufffmanTree.
          if ((FCodeByte and BitMask[FCodeBit]) = 0)
          then inc(Count)
          else Count := 0;
          if FFillOrder
          then inc(FCodeBit);
     end;
  end;

  // Read 8 bits.
  Count := 0;
  while (Count < 49) and // Should be 9.
        (FCodeIndex < BufferSize) and (FError = EC_OK)
  do begin
     if ((FCodeBit = 0) and (FFillOrder = False)) or
        ((FCodeBit = 8) and (FFillOrder = True))
     then begin
          // Get byte to decode.
          FCodeByte := FCBuffer^[FCodeIndex];
          inc(FCodeIndex);
          if FFillOrder
          then FCodeBit := 0
          else FCodeBit := 7;
     end
     else if Not(FFillOrder)
          then dec(FCodeBit);

     // Get bit for direction in HufffmanTree.
     if ((FCodeByte and BitMask[FCodeBit]) = 0)
     then inc(Count)
     else Count := 49; // Should be 9.
     if FFillOrder
     then inc(FCodeBit);
  end;

  if (FCCITTScheme = CCITT_MR)
  then begin
       if ((FCodeBit = 0) and (FFillOrder = False)) or
          ((FCodeBit = 8) and (FFillOrder = True))
       then begin
            // Get byte to decode.
            FCodeByte := FCBuffer^[FCodeIndex];
            inc(FCodeIndex);
            if FFillOrder
            then FCodeBit := 0
            else FCodeBit := 7;
       end
       else if Not(FFillOrder)
            then dec(FCodeBit);

       // Get bit for direction in HufffmanTree.
       if ((FCodeByte and BitMask[FCodeBit]) = 0)
       then FScheme := CCITT_MMR
       else FScheme := CCITT_MH;
       if FFillOrder
       then inc(FCodeBit);
  end;

  FEOLCount := 0;
  Result := FError;
end; // TmcmImageCCITT.SyncEOL.


function TmcmImageCCITT.Decompress(    Buffer     : pointer;
                                   var BufferSize : longword;
                                   var DataCount  : longword) : TmcmErrorCode;
var SaveIndex : longint;
    RunLength : cardinal;
begin
  FError := EC_OK;
  if (FWhiteTree = Nil) or
     (FBlackTree = Nil) or
     (FCodeTree  = Nil)
  then FError := InitDecompress;

  FScheme := FCCITTScheme;

  DataCount := 0;
  SaveIndex := FIndex;
  FCBuffer  := Buffer;
  FCodeSize := BufferSize;
  try
    FCodeIndex := 0;
    FCodeBit := FResetBit;

    FCodeRun := CR_WHITE;
    FEOLCount := 0;
    if (FCCITTScheme <> CCITT_MMR)
    then SyncEOL(BufferSize);

    FRefLine := FRow;
    Fa0 := -1;
    Fb1 := FMaxCol;

    while (cardinal(FCodeIndex) < BufferSize) and (FError = EC_OK)
    do begin
       case FScheme of
       CCITT_MH  : RunLength := ReadMHLine;
       CCITT_MR  : RunLength := ReadMHLine;
       CCITT_MMR : RunLength := ReadMRLine;
       else begin
            FError := EC_DECOMPRESSION;
            RunLength := 0;
       end;
       end;

       if (RunLength = $FFFF) or ((cardinal(FCodeIndex) = BufferSize - 1) and (FCBuffer^[FCodeIndex] = 0))
       then begin
           inc(FEOLCount);
           // If we reached the end of the buffer, or just one byte is left
           // we'll exit Decompress.
           if (cardinal(FCodeIndex) >= BufferSize - 1)
           then begin
                if (RunLength = $FFFF)
                then Break;
                Fa0 := FMaxCol;
                FCodeIndex := BufferSize;
           end;
       end;

       if (FCCITTScheme <> CCITT_MMR)
       then SyncEOL(BufferSize);

       if (Fa0 <> 0)
       then begin
            FRefLine := FRow;
            if FFlipVert
            then dec(FRow)
            else inc(FRow);

            if (Fa0 <> FMaxCol) // Line must have correct length!
            then begin
                 FError := EC_BADDECOMPRESS;
                 Break;
            end;

            // Check for end of image.
            if (0 > FRow) or (FRow >= FDIBSection.dsBmih.biHeight)
            then begin
                 if (FCCITTScheme = CCITT_MMR)
                 then begin
                      // Read 2 x EOL
                      SyncEOL(BufferSize);
                      SyncEOL(BufferSize);
                 end;
                 FDataBit := 8;
                 break;
            end;
       end;

       FIndex   := FRow * FLongWidth;
       FDataBit := 8;
       Fa0 := -1;
       Fb1 := 0;
       FCodeRun := CR_WHITE;
    end;

    // Calculate remainder of buffer.
    BufferSize := BufferSize - cardinal(FCodeIndex);

    // Caluclate decompressed data size.
    DataCount := abs(FIndex - SaveIndex);
  except
    FError := EC_BADDECOMPRESS;
  end;
  Result := FError;
end; // TmcmImageCCITT.Decompress.


//------------------------------------------------------------------------------
// TmcmHuffmanBase
//------------------------------------------------------------------------------

constructor TmcmHuffmanBase.Create(MaxNoCodes, MaxCodeLength : cardinal);
begin
  FMaxHuffmanCode := MaxNoCodes;    // Used to be PNGMaxHuffmanCodes
  FMaxCodeLength  := MaxCodeLength; // Used to be PNGMaxLengthCodeSize
  FError := EC_OK;
  inc(FMaxHuffmanCode);
  FFrequency  := AllocMem(FMaxHuffmanCode * SizeOf(cardinal));
  FHuffValue  := AllocMem(FMaxHuffmanCode * SizeOf(cardinal));
  FHuffSize   := AllocMem(FMaxHuffmanCode * SizeOf(cardinal));
  FHuffCode   := AllocMem(FMaxHuffmanCode * SizeOf(cardinal));
  FMinCode    := AllocMem(FMaxCodeLength * SizeOf(cardinal));
  FMaxCode    := AllocMem(FMaxCodeLength * SizeOf(longint));
  FValCode    := AllocMem(FMaxCodeLength * SizeOf(cardinal));
  dec(FMaxHuffmanCode);
  ClearTable;
end; // TmcmHuffmanBase.Create.


destructor TmcmHuffmanBase.Destroy;
begin
  if Assigned(FFrequency)
  then FreeMem(FFrequency);
  if Assigned(FHuffValue)
  then FreeMem(FHuffValue);
  if Assigned(FHuffSize)
  then FreeMem(FHuffSize);
  if Assigned(FHuffCode)
  then FreeMem(FHuffCode);
  if Assigned(FMinCode)
  then FreeMem(FMinCode);
  if Assigned(FMaxCode)
  then FreeMem(FMaxCode);
  if Assigned(FValCode)
  then FreeMem(FValCode);
end; // TmcmHuffmanBase.Destroy.


procedure TmcmHuffmanBase.ClearTable;
begin
  try
    FTableDefined := False;
    FillChar(FFrequency^, FMaxHuffmanCode * SizeOf(cardinal), 0);
    FillChar(FHuffValue^, FMaxHuffmanCode * SizeOf(cardinal), 0);
    FillChar(FHuffSize^, FMaxHuffmanCode * SizeOf(cardinal), 0);
    FillChar(FHuffCode^, FMaxHuffmanCode * SizeOf(cardinal), 0);
    FillChar(FMinCode^, FMaxCodeLength * SizeOf(cardinal), 0);
    FillChar(FMaxCode^, FMaxCodeLength * SizeOf(longint), -1);
    FillChar(FValCode^, FMaxCodeLength * SizeOf(cardinal), 0);
  except
    On Exception
    do FError := EC_NOMEMORY;
  end;
end; // TmcmHuffmanBase.ClearTable.


function TmcmHuffmanBase.CreateReadTable(Count : cardinal) : TmcmErrorCode;
var i : cardinal;
begin
  FCount := Count;
  for i := 0 to (FMaxCodeLength - 1)
  do begin
     FValCode[i] := 0;
     FMinCode[i] := $0FFFFFFF;
     FMaxCode[i] := -1;
  end;

  Result := FError;
end; // TmcmHuffmanBase.CreateReadTable.


procedure TmcmHuffmanBase.Compress(Value : integer; var Code, Size : cardinal);
begin
  Code := FHuffCode^[Value];
  Size := FHuffSize^[Value];
end; // TmcmHuffmanBase.Compress.


function TmcmHuffmanBase.Decompress : cardinal;
var Code       : cardinal;
    CodeLength : cardinal;
    Index      : cardinal;
    Offset     : cardinal;
begin
  // This function decodes the next byte in the input buffer/stream using this
  // Huffman table.
  Code := GetNBits(1);

  CodeLength := 0;
  while (longint(Code) > FMaxCode[CodeLength]) and
        (CodeLength < FMaxCodeLength)
  do begin
     Code := (Code shl 1) or GetNBits(1);
     inc(CodeLength);
  end;

  if (CodeLength < FMaxCodeLength)
  then begin
       Offset := Code - FMinCode[CodeLength];
       Index := FValCode[CodeLength] + Offset;
       Result := FHuffValue[Index];
  end
  else begin
       // FError := EC_DECOMPRESSION;
       Result := Cardinal(-1);
  end;
end; // TmcmHuffmanBase.Decompress.


function TmcmHuffmanBase.CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode;
begin
  // The creation of Huffman tables for JPEG and PNG are quite similar and could
  // be merged, care taken!
  Result := FError;
end; // TmcmHuffmanBase.CreateWriteTable.


function TmcmHuffmanBase.GetFrequency(Index : cardinal) : cardinal;
begin
  Result := FFrequency^[Index];
end; // TmcmHuffmanBase.GetFrequency.


procedure TmcmHuffmanBase.SetFrequency(Index : cardinal; Value : cardinal);
begin
  FFrequency^[Index] := Value;
end; // TmcmHuffmanBase.SetFrequency.


function TmcmHuffmanBase.GetHuffValue(Index : cardinal) : cardinal;
begin
  Result := FHuffValue^[Index];
end; // TmcmHuffmanBase.GetHuffValue.


procedure TmcmHuffmanBase.SetHuffValue(Index : cardinal; Value : cardinal);
begin
  FHuffValue^[Index] := Value;
end; // TmcmHuffmanBase.SetHuffValue.


function TmcmHuffmanBase.GetHuffSize(Index : cardinal) : cardinal;
begin
  Result := FHuffSize^[Index];
end; // TmcmHuffmanBase.GetHuffSize.


procedure TmcmHuffmanBase.SetHuffSize(Index : cardinal; Value : cardinal);
begin
  FHuffSize^[Index] := Value;
end; // TmcmHuffmanBase.SetHuffSize.


function TmcmHuffmanBase.GetHuffCode(Index : cardinal) : cardinal;
begin
  Result := FHuffCode^[Index];
end; // TmcmHuffmanBase.GetHuffCode.


procedure TmcmHuffmanBase.SetHuffCode(Index : cardinal; Value : cardinal);
begin
  FHuffCode^[Index] := Value;
end; // TmcmHuffmanBase.SetHuffCode.


function TmcmHuffmanBase.GetMinCode(Index : cardinal) : cardinal;
begin
  Result := FMinCode^[Index];
end; // TmcmHuffmanBase.GetMinCode.


procedure TmcmHuffmanBase.SetMinCode(Index : cardinal; Value : cardinal);
begin
  FMinCode^[Index] := Value;
end; // TmcmHuffmanBase.SetMinCode.


function TmcmHuffmanBase.GetMaxCode(Index : cardinal) : longint;
begin
  Result := FMaxCode^[Index];
end; // TmcmHuffmanBase.GetMaxCode.


procedure TmcmHuffmanBase.SetMaxCode(Index : cardinal; Value : longint);
begin
  FMaxCode^[Index] := Value;
end; // TmcmHuffmanBase.SetMaxCode.


function TmcmHuffmanBase.GetValCode(Index : cardinal) : cardinal;
begin
  Result := FValCode^[Index];
end; // TmcmHuffmanBase.GetValCode.


procedure TmcmHuffmanBase.SetValCode(Index : cardinal; Value : cardinal);
begin
  FValCode^[Index] := Value;
end; // TmcmHuffmanBase.SetValCode.


//------------------------------------------------------------------------------
// TmcmHuffmanJPEG
//------------------------------------------------------------------------------

procedure TmcmHuffmanJPEG.ClearTable;
begin
  Inherited ClearTable;
  try
    // ---- JPEG Only -----
    // A dummy huffman value is added at the end of the table with the minimal
    // frequency. Since Huffman codes are created based on frequencies, this
    // dummy value is assigned the longest "all 1" huffman code, and ensures
    // that no Huffman code consists entirely of "1"'s.
    FFrequency^[FMaxHuffmanCode] := 1;
    FSymbolCount := 0;
  except
    On Exception
    do FError := EC_NOMEMORY;
  end;
end; // TmcmHuffmanJPEG.ClearTable.


function TmcmHuffmanJPEG.GetHuffmanBits(Index : word) : cardinal;
begin
  if (Index < JPEGMaxHuffmanCodeLength)
  then Result := FHuffmanBits[Index]
  else Result := 0;
end; // TmcmHuffmanJPEG.GetHuffmanBits.


procedure TmcmHuffmanJPEG.SetHuffmanBits(Index : word; Value : cardinal);
begin
  if (Index < JPEGMaxHuffmanCodeLength)
  then FHuffmanBits[Index] := Value;
end; // TmcmHuffmanJPEG.SetHuffmanBits.


function TmcmHuffmanJPEG.CreateReadTable(Count : cardinal; HuffBits : array of byte) : TmcmErrorCode;
var i, j, k : cardinal;
    Code    : word;
begin
  FError := Inherited CreateReadTable(Count);

  // Convert HuffBits containing the count of code length (1..16) into HuffSize
  // containing the length for each code.
  k := 0;
  for i := 0 to (FMaxCodeLength - 1)
  do begin
     j := 0;
     while (j < HuffBits[i])
     do begin
        FHuffSize^[k] := i + 1;
        inc(k);
        inc(j);
     end;
     FHuffSize^[k] := 0;
  end;

  // Calculate the huffman code for each huffman value.
  {$IFDEF HUFFDEBUG}
    OutputDebugString('Index, Huffman code');
    OutputDebugString('-------------------');
  {$ENDIF}
  Code := 0;
  k := 0;
  i := FHuffSize^[0];
  while (FHuffSize^[k] <> 0)
  do begin
     while (FHuffSize^[k] = i)
     do begin
        FHuffCode^[k] := Code;
        {$IFDEF HUFFDEBUG}
          // OutputDebugString(PChar(IntToStr(k) + ', ' + IntToHex(FHuffCode^[k], 4)));
          OutputDebugString(PChar(Format('%5.d, %5.x', [k, FHuffCode^[k]])));
        {$ENDIF}
        inc(Code);
        inc(k);
     end;
     inc(i);
     Code := Code shl 1;
  end;

  // Determin Min, Max and Index codes.
  // i = index into HuffBits
  // j = index into ValCode
  {$IFDEF HUFFDEBUG}
    OutputDebugString('Index,  Length, Min,    Max,    ValCode');
    OutputDebugString('------------------------');
  {$ENDIF}
  j := 0;
  for i := 0 to (FMaxCodeLength - 1)
  do begin
     if (HuffBits[i] <> 0)
     then begin
          FValCode^[i] := j;

          FMinCode^[i] := FHuffCode^[j];
          j := j + HuffBits[i];
          FMaxCode^[i] := FHuffCode^[j-1];
     end
     else FMinCode^[i] := FMaxHuffmanCode + 1;
     {$IFDEF HUFFDEBUG}
       OutputDebugString(PChar(Format('%7.d, %7.d, %7.d, %7.d, %7.d', [i, FHuffSize^[i], FMinCode^[i], FMaxCode^[i], FValCode^[i]])));
     {$ENDIF}
  end;

  FTableDefined := (FError = EC_OK);
  Result := FError;
end; // TmcmHuffmanJPEG.CreateReadTable.


function TmcmHuffmanJPEG.CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode;
var i, j, k, si : longint;
    TmpCode     : array[0..JPEGMaxHuffmanCodes] of cardinal;
    TmpCodeSize : array[0..JPEGMaxHuffmanCodes] of longint;
    TmpHuffSize : array[0..JPEGMaxHuffmanCodes] of longint;
    TmpHuffCode : array[0..JPEGMaxHuffmanCodes] of cardinal;
    Others      : array[0..JPEGMaxHuffmanCodes] of longint;
    v1, v2      : longint;
    Count       : cardinal;
    Code        : cardinal;
begin
  try
  FillChar(FHuffSize^, FMaxHuffmanCode * SizeOf(cardinal), 0);
  FillChar(FHuffCode^, FMaxHuffmanCode * SizeOf(cardinal), 0);

  // The tmp array is used for validating the integrity of the Huffman code
  // table. We need a temporary array since FFrequency^[] gets trashed
  // during the code generation process.

  for i := 0 to JPEGMaxHuffmanCodes
  do TmpCode[i] := FFrequency^[i];

  // Build the Huffman Code Length Lists
  for i := 0 to JPEGMaxHuffmanCodes
  do Others[i] := -1;

  FillChar(TmpCodeSize, (JPEGMaxHuffmanCodes + 1) * SizeOf(longint), 0);
  while (True)
  do begin
     // Find the two smallest non-zero values
     v1 := -1;
     v2 := -1;
     for i := 0 to JPEGMaxHuffmanCodes
     do begin
        if (FFrequency^[i] <> 0)
        then begin
             if (v1 < 0) or (FFrequency^[i] <= FFrequency^[v1])
             then begin
                  v2 := v1;
                  v1 := i;
             end
             else if (v2 < 0) or (FFrequency^[i] <= FFrequency^[v2])
                  then v2 := i;
        end;
     end;

     if (v2 < 0)
     then Break;

     // Join the two tree nodes.
     FFrequency^[v1] := FFrequency^[v1] + FFrequency^[v2];
     FFrequency^[v2] := 0;

     inc(TmpCodeSize[v1]);
     while (Others[v1] >= 0)
     do begin
        v1 := Others[v1];
        inc(TmpCodeSize[v1]);
     end;

     Others[v1] := v2;

     inc(TmpCodeSize[v2]);
     while (Others[v2] >= 0)
     do begin
        v2 := Others[v2];
        inc(TmpCodeSize[v2]);
     end;
  end;

  // Determine the number of codes of length[n]
  FillChar(FHuffmanBits, SizeOf(FHuffmanBits), 0);
  for i := 0 to JPEGMaxHuffmanCodes
  do begin
     if (TmpCodeSize[i] <> 0)
     then inc(FHuffmanBits[TmpCodeSize[i]-1]);
  end;

  // Ensure that no code is longer than maxlength.
  for i := (2 * JPEGMaxHuffmanCodeLength - 1) downto JPEGMaxHuffmanCodeLength
  do begin
     while (FHuffmanBits[i] <> 0)
     do begin
        j := i - 2;
        while (FHuffmanBits[j] = 0)
        do dec(j);

        dec(FHuffmanBits[i], 2);
        inc(FHuffmanBits[i-1]);
        inc(FHuffmanBits[j+1], 2);
        dec(FHuffmanBits[j]);
     end;
  end;

  // Remove reserved code from end of list.
  for i := (JPEGMaxHuffmanCodeLength - 1) downto 0
  do begin
     if (FHuffmanBits[i] <> 0)
     then begin
          dec(FHuffmanBits[i]);
          Break;
     end;
  end;

  // Make sure that the total number of symbols is correct.
  Count := 0;
  for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
  do Count := Count + FHuffmanBits[i];

  if (Count >= JPEGMaxHuffmanCodes)
  then Raise ERangeError.CreateFmt(resHuffmannTooManyCodes, ['']);

  // Sort the values in order of code length
  FillChar(FHuffValue^, JPEGMaxHuffmanCodes * SizeOf(cardinal), 0);
  k := 0;
  for i := 1 to (2 * JPEGMaxHuffmanCodeLength - 1)
  do begin
     for j := 0 to (JPEGMaxHuffmanCodes - 1)
     do begin
        if (TmpCodeSize[j] = i)
        then begin
             FHuffValue^[k] := j;
             inc(k);
        end;
     end;
  end;

  // Convert the array "HuffmanBits" containing the count of codes for each
  // length 1..MaxLength into an array containing the length for each code.
  FillChar(TmpHuffSize, JPEGMaxHuffmanCodes * SizeOf(longint), 0);
  k := 0;
  for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
  do begin
     for j := 0 to (longint(FHuffmanBits[i]) - 1) 
     do begin
        TmpHuffSize[k] := i + 1;
        inc(k);
     end;
     TmpHuffSize[k] := 0;
  end;

  // Calculate the Huffman code for each Huffman value.
  Code := 0;
  FillChar(TmpHuffCode, JPEGMaxHuffmanCodes * SizeOf(cardinal), 0);
  k := 0;
  si := TmpHuffSize[0];
  while (TmpHuffSize[k] <> 0)
  do begin
     while (TmpHuffSize[k] = si)
     do begin
        TmpHuffCode[k] := Code;
        inc(Code);
        inc(k);
     end;
     inc(si);
     Code := Code shl 1;
  end;

  FillChar(FHuffSize^, JPEGMaxHuffmanCodes * SizeOf(cardinal), 0);
  FillChar(FHuffCode^, JPEGMaxHuffmanCodes * SizeOf(cardinal), 0);
  for k := 0 to (JPEGMaxHuffmanCodes - 1)
  do begin
     if (TmpHuffSize[k] <> 0)
     then begin
          i := FHuffValue^[k];
          FHuffCode^[i] := TmpHuffCode[k];
          FHuffSize^[i] := TmpHuffSize[k];
     end;
  end;

  // Validations
  // The remaining code is not necessary other than to ensure the
  // integrity of the Huffman table that is generated.

  // Make sure each value is used only once.
  for i := 0 to (JPEGMaxHuffmanCodes - 1)
  do begin
     Count := 0;
     if (TmpCode[i] <> 0)
     then begin
          if (FHuffSize^[i] = 0)
          then begin
               FError := EC_BADCOMPRESS;    // <- change error code.
               Raise ERangeError.CreateFmt(resHuffmannMissingSize, ['']);
          end;

          for j := 0 to (JPEGMaxHuffmanCodes - 1)
          do begin
             if (cardinal(i) = FHuffValue^[j]) and (TmpHuffSize[j] <> 0)
             then inc(Count);
             if (Count > 1)
             then begin
                  FError := EC_BADCOMPRESS; // <- change error code.
                  Raise ERangeError.CreateFmt(resHuffmannDuplicateValue, ['']);
             end;
          end;
          if (Count = 0)
          then begin
               FError := EC_BADCOMPRESS; // <- change error code.
               Raise ERangeError.CreateFmt(resHuffmannMissingValue, ['']);
          end;
     end;
  end;

  // Ensure that each huffman code is used once and that the values
  // are in range.
  for i := 0 to (JPEGMaxHuffmanCodes - 1)
  do begin
     if (FHuffSize^[i] <> 0)
     then begin
          if (FHuffSize^[i] > JPEGMaxHuffmanCodeLength)
          then begin
               FError := EC_BADCOMPRESS; // <- change error code.
               Raise ERangeError.CreateFmt(resHuffmannInvalidRange, ['']);
          end;

          for j := i + 1 to (JPEGMaxHuffmanCodes - 1)
          do begin
             if (FHuffCode^[i] = FHuffCode^[j]) and (FHuffSize^[j] <> 0)
             then begin
                  FError := EC_BADCOMPRESS; // <- change error code.
                  Raise ERangeError.CreateFmt(resHuffmannDuplicateCode, ['']);
             end;
          end;
     end;
  end;

  FSymbolCount := 0;
  for i := 0 to (JPEGMaxHuffmanCodeLength - 1)
  do inc(FSymbolCount, HuffmanBits[i]);

  except
    On E:Exception
    do begin
       if (FError = EC_OK)
       then FError := EC_UNKNOWN;
    end;
  end;
  Result := FError;
end; // TmcmHuffmanJPEG.CreateWriteTable.


//------------------------------------------------------------------------------
// TmcmHuffmanPNG
//------------------------------------------------------------------------------

function TmcmHuffmanPNG.CreateReadTable(Count : cardinal) : TmcmErrorCode;
var i, j    : cardinal;
    Swap    : word;
    LastLen : cardinal;
    Code    : cardinal;
    Last    : cardinal;
begin
  FError := Inherited CreateReadTable(Count);

  // Pore sort. Sort Huffman table on primary key "HuffSize" and secondary key
  // HuffValue.
  for i := 0 to (FCount - 1)
  do FHuffValue^[i] := i;

  for i := 0 to (FCount - 1)
  do begin
     for j := (i + 1) to (FCount - 1)
     do begin
        if (FHuffSize^[j] < FHuffSize^[i]) or
           ((FHuffSize^[j] = FHuffSize^[i]) and (FHuffValue^[j] < FHuffValue^[i]))
        then begin
             Swap := FHuffSize^[i];
             FHuffSize^[i] := FHuffSize^[j];
             FHuffSize^[j] := Swap;

             Swap := FHuffValue^[i];
             FHuffValue^[i] := FHuffValue^[j];
             FHuffValue^[j] := Swap;
        end;
     end;
  end;

  FillChar(FHuffCode^, FMaxHuffmanCode * SizeOf(longint), 0);
  Code    := 0;
  LastLen := 0;
  for i := 0 to (FCount - 1)
  do begin
     while (LastLen <> FHuffSize^[i])
     do begin
        inc(LastLen);
        Code := Code shl 1;
     end;

     if (LastLen <> 0)
     then begin
          FHuffCode[i] := Code;
          inc(Code);
     end;
  end;

{
  for i := 0 to (FMaxCodeLength - 1)
  do begin
     FValCode[i] := 0;
     FMinCode[i] := $0FFFFFFF;
     FMaxCode[i] := -1;
  end;
}
  Last := 0;
  for i := 0 to (FCount - 1)
  do begin
     if (Last <> FHuffSize^[i])
     then begin
          Last := FHuffSize^[i];
          FValCode[Last-1] := i;
          FMinCode[Last-1] := FHuffCode[i];
     end;
     if (Last <> 0)
     then FMaxCode[Last-1] := FHuffCode[i];
  end;
  
  FTableDefined := (FError = EC_OK);
  Result := FError;
end; // TmcmHuffmanPNG.CreateReadTable.


function TmcmHuffmanPNG.CreateWriteTable(MaxLength : cardinal) : TmcmErrorCode;
var CodeTooLong : boolean;
    i, j, k, si : longint;
    TmpCode     : array[0..PNGMaxHuffmanCodes-1] of cardinal;
    TmpCodeSize : array[0..PNGMaxHuffmanCodes-1] of longint;
    TmpHuffSize : array[0..PNGMaxHuffmanCodes-1] of longint;
    TmpHuffCode : array[0..PNGMaxHuffmanCodes-1] of cardinal;
    Others      : array[0..PNGMaxHuffmanCodes-1] of longint;
    HuffmanBits : array[0..2*PNGMaxLengthCodeSize] of cardinal;
    v1, v2      : longint;
    Count       : cardinal;
    Value       : cardinal;
    Code        : cardinal;
    CodeSize    : longint;
    Bit         : cardinal;
begin
  CodeTooLong := False;

  FillChar(FHuffSize^, FMaxHuffmanCode * SizeOf(cardinal), 0);
  FillChar(FHuffCode^, FMaxHuffmanCode * SizeOf(cardinal), 0);

  // The tmp array is used for validating the integrity of the Huffman code
  // table. We need a temporary array since FFrequency^[] gets trashed
  // during the code generation process.

  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do TmpCode[i] := FFrequency^[i];

  // Build the Huffman Code Length Lists
  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do Others[i] := -1;

  FillChar(TmpCodeSize, PNGMaxHuffmanCodes * SizeOf(longint), 0);
  while (True)
  do begin
     // Find the two smallest non-zero values
     v1 := -1;
     v2 := -1;
     for i := 0 to (PNGMaxHuffmanCodes - 1)
     do begin
        if (FFrequency^[i] <> 0)
        then begin
             if (v1 < 0) or (FFrequency^[i] <= FFrequency^[v1])
             then begin
                  v2 := v1;
                  v1 := i;
             end
             else if (v2 < 0) or (FFrequency^[i] <= FFrequency^[v2])
                  then v2 := i;
        end;
     end;

     if (v2 < 0)
     then begin
          if (v1 < 0)
          then begin
               FError := EC_NOHUFFMANCODES;
               Result := FError;
               Exit; // No codes defined
          end;

          if (TmpCodeSize[v1] = 0)
          then TmpCodeSize[v1] := 1;  // Only one code defined
          Break;
     end;

     // Join the two tree nodes.
     FFrequency^[v1] := FFrequency^[v1] + FFrequency^[v2];
     FFrequency^[v2] := 0;

     inc(TmpCodeSize[v1]);
     while (Others[v1] >= 0)
     do begin
        v1 := Others[v1];
        inc(TmpCodeSize[v1]);
     end;

     Others[v1] := v2;

     inc(TmpCodeSize[v2]);
     while (Others[v2] >= 0)
     do begin
        v2 := Others[v2];
        inc(TmpCodeSize[v2]);
     end;
  end;

  // Determine the number of codes of length[n]
  FillChar(HuffmanBits, SizeOf(HuffmanBits), 0);
  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do begin
     if (TmpCodeSize[i] <> 0)
     then inc(HuffmanBits[TmpCodeSize[i]-1]);
  end;

  // Ensure that no code is longer than maxlength.
  for i := (2 * MaxLength - 1) downto MaxLength
  do begin
     while (HuffmanBits[i] <> 0)
     do begin
        CodeTooLong := True; // Remember that we had to reorder the tree

        j := i - 2;
        while (HuffmanBits[j] = 0)
        do dec(j);

        dec(HuffmanBits[i], 2);
        inc(HuffmanBits[i-1]);
        inc(HuffmanBits[j+1], 2);
        dec(HuffmanBits[j]);
     end;
  end;

  // Make sure that the total number of symbols is correct.
  Count := 0;
  for i := 0 to (MaxLength - 1)
  do Count := Count + HuffmanBits[i];

  if (Count >= PNGMaxHuffmanCodes)
  then Raise ERangeError.CreateFmt(resHuffmannTooManyCodes, ['']);

  // Sort the values in order of code length
  FillChar(FHuffValue^, PNGMaxHuffmanCodes * SizeOf(cardinal), 0);
  k := 0;
  for i := 1 to (2 * MaxLength - 1)
  do begin
     for j := 0 to (PNGMaxHuffmanCodes - 1)
     do begin
        if (TmpCodeSize[j] = i)
        then begin
             FHuffValue^[k] := j;
             inc(k);
        end;
     end;
  end;

  // Convert the array "HuffmanBits" containing the count of codes for each
  // length 1..MaxLength into an array containing the length for each code.
  FillChar(TmpHuffSize, PNGMaxHuffmanCodes * SizeOf(longint), 0);
  k := 0;
  for i := 0 to (MaxLength - 1)
  do begin
     for j := 0 to longint(HuffmanBits[i] - 1)
     do begin
        TmpHuffSize[k] := i + 1;
        inc(k);
     end;
     TmpHuffSize[k] := 0;
  end;

  // Calculate the Huffman code for each Huffman value.
  Code := 0;
  FillChar(TmpHuffCode, PNGMaxHuffmanCodes * SizeOf(cardinal), 0);
  k := 0;
  si := TmpHuffSize[0];
  while (TmpHuffSize[k] <> 0)
  do begin
     while (TmpHuffSize[k] = si)
     do begin
        TmpHuffCode[k] := Code;
        inc(Code);
        inc(k);
     end;
     inc(si);
     Code := Code shl 1;
  end;

  FillChar(FHuffSize^, PNGMaxHuffmanCodes * SizeOf(cardinal), 0);
  FillChar(FHuffCode^, PNGMaxHuffmanCodes * SizeOf(cardinal), 0);
  for k := 0 to (PNGMaxHuffmanCodes - 1)
  do begin
     if (TmpHuffSize[k] <> 0)
     then begin
          i := FHuffValue^[k];
          FHuffCode^[i] := TmpHuffCode[k];
          FHuffSize^[i] := TmpHuffSize[k];
          if (FHuffSize^[i] > MaxLength)
          then Raise ERangeError.CreateFmt(resHuffmannInvalidCode, ['']);
     end;
  end;

  // If the pure Huffman code generation created codes longer than the
  // maximum the it is possible that the order got screwed up. Such a
  // situation could occur if the maximum code length is 15 and during the
  // pure process we the value 150 got assigned a length of 13, 100 a length
  // of 15 and 200 a length of 17. During the process of reducing the code
  // length for 200 it is possible that 150 would have its code length
  // increased to 14 and 100 would have its code length reduced to 14.
  // Unfortunately the Huffman codes would be assigned using the old
  // order so that 150 would get assigned a smaller Huffman code than
  // 100.  Here we fix that and ensure that if (FHuffSize^[i] = FHuffSize^[j])
  //  and (i < j) then FHuffCode^[i] < FHuffCode^[j].
  if (CodeTooLong)
  then begin
       for i := 0 to (PNGMaxHuffmanCodes - 2)
       do begin
          for  j := i + 1 to (PNGMaxHuffmanCodes - 1)
          do begin
             if (FHuffSize^[i] = FHuffSize^[j]) and (FHuffCode^[i] > FHuffCode^[j])
             then begin
                  // The codes got out of order so switch them.
                  Code := FHuffCode^[j];
                  FHuffCode^[j] := FHuffCode^[i];
                  FHuffCode^[i] := Code;
             end;
          end;
       end;
  end;

  // Validations
  // This remaining code is not necessary other than to ensure the
  // integrity of the Huffman table that is generated.

  // Make sure each value is used exactly once.
  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do begin
     Count := 0;
     if (TmpCode[i] <> 0)
     then begin
          if (FHuffSize^[i] = 0)
          then Raise ERangeError.CreateFmt(resHuffmannMissingSize, ['']);

          for j := 0 to (PNGMaxHuffmanCodes - 1)
          do begin
             if (cardinal(i) = FHuffValue^[j]) and (TmpHuffSize[j] <> 0)
             then inc(Count);
             if (Count > 1)
             then Raise ERangeError.CreateFmt(resHuffmannDuplicateValue, ['']);
          end;
          if (Count = 0)
          then Raise ERangeError.CreateFmt(resHuffmannMissingValue, ['']);
     end;
  end;

  // Ensure that each huffman code is used once and that the values
  // are in range.
  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do begin
     if (FHuffSize^[i] <> 0)
     then begin
          if (FHuffSize^[i] > MaxLength)
          then Raise ERangeError.CreateFmt(resHuffmannInvalidRange, ['']);

          for j := i + 1 to (PNGMaxHuffmanCodes - 1)
          do begin
             if (FHuffCode^[i] = FHuffCode^[j]) and (FHuffSize^[j] <> 0)
             then Raise ERangeError.CreateFmt(resHuffmannDuplicateCode, ['']);
          end;
     end;
  end;

  // In PNG the Huffman codes are stored backwards. Therefore, reverse all codes.
  for i := 0 to (PNGMaxHuffmanCodes - 1)
  do begin
     Value    := 0;
     Code     := FHuffCode^[i];
     CodeSize := FHuffSize^[i];
     j := 0;
     while (cardinal(j) < FHuffSize^[i])
     do begin
        Bit := (Code and (1 shl j)) shr j;
        Value := Value or (Bit shl (CodeSize - j - 1));
        inc(j);
     end;
     FHuffCode^[i] := Value;
  end;
  Result := FError;
end; // TmcmHuffmanPNG.CreateWriteTable.


//------------------------------------------------------------------------------
// TmcmImageLZ
//------------------------------------------------------------------------------

constructor TmcmImageLZ.Create;
begin
  Inherited Create;
  FSupColors := [IF_BW, IF_GREY4, IF_PAL4, IF_GREY8, IF_PAL8, IF_RGB24, IF_RGBA32];

  DistanceShift   := 3;
  FFlagBit        := -1;
  FFlagIndex      := 0;

  FHashTable      := Nil;
  FSlidingWindow  := Nil;
  FEncoders       := Nil;
  FEncoderSize    := 4096;
end; // TmcmImageLZ.Create.


destructor TmcmImageLZ.Destroy;
begin
  if Assigned(FHashTable)
  then FHashTable.Destroy;
  FHashTable := Nil;
  if Assigned(FSlidingWindow)
  then FSlidingWindow.Destroy;
  FSlidingWindow := Nil;
  FreeMem(FEncoders);
  FEncoders := Nil;
  Inherited Destroy;
end; // TmcmImageLZ.Destroy.


procedure TmcmImageLZ.Clear;
begin
  Inherited Clear;
end; // TmcmImageLZ.Clear.


procedure TmcmImageLZ.SetDistanceShift(Value : word);
begin
  FDistanceShift  := Value; // Default is 3
  FLengthMask     := (1 shl FDistanceShift) - 1;
  FWindowSize     := (1 shl (16 - FDistanceShift));
  FMaxMatchLength := FLengthMask + 3; // Same as LookAheadSize
end; // TmcmImageLZ.SetDistanceShift.


procedure TmcmImageLZ.SetEncoderSize(Value : cardinal);
begin
  if (FEncoderSize <> Value)
  then FEncoderSize := Value;
end; // TmcmImageLZ.SetEncoderSize.


procedure TmcmImageLZ.SetImage(Value : HBITMAP);
var BitCount : integer;
begin
  Inherited SetImage(Value);

  BitCount := FDIBSection.dsBmih.biBitCount * FDIBSection.dsBmih.biPlanes;
  case BitCount of
  1  : FPackWidth := (FDIBSection.dsBmih.biWidth + 7) div 8;
  4  : FPackWidth := (FDIBSection.dsBmih.biWidth + 1) div 2;
  8  : FPackWidth := FDIBSection.dsBmih.biWidth;
  15,
  16 : FPackWidth := 2 * FDIBSection.dsBmih.biWidth;
  24 : FPackWidth := 3 * FDIBSection.dsBmih.biWidth;
  32 : FPackWidth := 4 * FDIBSection.dsBmih.biWidth;
  end;
  FDataToCompress := FPackWidth * cardinal(FMaxRow);
end; // TmcmImageLZ.SetImage.


procedure TmcmImageLZ.SetOnNeedData(AOnNeedData : TOnNeedData);
begin
  FOnNeedData := AOnNeedData;
end; // TmcmImageLZ.SetOnNeedData.


procedure TmcmImageLZ.OnSlidingHasData(Sender : TObject; pData : Pointer; NoBytes : cardinal);
var Count   : longint;
    dCount  : longint;
begin
  Count := NoBytes;
  while (Count > 0) and (FRow < FMaxRow)
  do begin
     if (FCol <> 0)
     then dCount := longint(FPackWidth) - FCol
     else begin
          if (Count > longint(FPackWidth))
          then dCount := FPackWidth
          else dCount := Count;
     end;
     FIndex := FRow * FLongWidth + FCol;
     CopyMemory(@FDibBits^[FIndex], pData, dCount);

     if (dCount < longint(FPackWidth))
     then begin
          FCol := FCol + dCount;
          if (FCol >= longint(FPackWidth))
          then begin
               FCol := 0;
               if FFlipVert
               then dec(FRow)
               else inc(FRow);
          end;
     end
     else begin
          FCol := 0;
          if FFlipVert
          then dec(FRow)
          else inc(FRow);
     end;

     dec(Count, dCount);
     pData := Pointer(longint(pData) + dCount);
  end;
end; // TmcmImageLZ.OnSlidingHasData.


procedure TmcmImageLZ.OnSlidingNeedData(    Sender  : TObject;
                                            pData   : Pointer;
                                        var NoBytes : cardinal);
var Count   : longint;
    dCount  : longint;
begin
  if (NoBytes > FDataToCompress)
  then NoBytes := FDataToCompress;

  dec(FDataToCompress, NoBytes);
  Count := NoBytes;
  while (Count > 0)
  do begin
     if (FCol <> 0)
     then dCount := FPackWidth - cardinal(FCol)
     else begin
          if (Count > longint(FPackWidth))
          then dCount := FPackWidth
          else dCount := Count;
     end;
     FIndex := FRow * FLongWidth + FCol;
     CopyMemory(pData, @FDibBits^[FIndex], dCount);

     if (dCount < longint(FPackWidth))
     then begin
          FCol := FCol + dCount;
          if (FCol >= longint(FPackWidth))
          then begin
               FCol := 0;
               if FFlipVert
               then dec(FRow)
               else inc(FRow);
          end;
     end
     else begin
          FCol := 0;
          if FFlipVert
          then dec(FRow)
          else inc(FRow);
     end;

     dec(Count, dCount);
     pData := Pointer(longint(pData) + dCount);
  end;
end; // TmcmImageLZ.OnSlidingNeedData.


procedure TmcmImageLZ.MatchLongest(      ExtraData   : pointer;
                                   const Key         : THashKey;
                                         Offset, Len, Code : longint);
var CurLen : longint;
    Dist   : longint;
begin
  with PLZEnumExtraData(ExtraData)^
  do begin
     CurLen := SW.Compare(Offset, Dist);
     if (CurLen > MaxLen)
     then begin
          MaxLen := CurLen;
          DistMaxMatch := Dist;
     end;
  end;
end; // TmcmImageLZ.MatchLongest.


function TmcmImageLZ.AddCodeToEncodings(    ALiteral  : byte;
                                        var Encodings : PVectorB;
                                        var Count     : cardinal) : TmcmErrorCode;
begin
  try
    if (FFlagBit < 0)
    then begin
         if (Count + 16 >= FEncoderSize)
         then begin
              // Return compressed data.
              if Assigned(FOnCompressData)
              then FOnCompressData(Self, FEncoders, FCodeCount);
              Count := 0;
         end;

         FFlagBit := 7;
         FFlagIndex := Count;
         Encodings^[FFlagIndex] := 0;
         inc(Count);
    end;

    Encodings^[Count] := ALiteral;
    inc(Count);
    dec(FFlagBit);
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmImageLZ.AddCodeToEncodings.


function TmcmImageLZ.AddDistLenToEncodings(    ADistance : integer;
                                               ALength   : integer;
                                           var Encodings : PVectorB;
                                           var Count     : cardinal) : TmcmErrorCode;
var Code : word;
begin
  try
    if (FFlagBit < 0)
    then begin
         if (Count + 16 >= FEncoderSize)
         then begin
              // Return compressed data.
              if Assigned(FOnCompressData)
              then FOnCompressData(Self, FEncoders, FCodeCount);
              Count := 0;
         end;

         FFlagBit := 7;
         FFlagIndex := Count;
         Encodings^[FFlagIndex] := 0;
         inc(Count);
    end;

    Code := (ADistance - 1) shl FDistanceShift;
    Code := Code or (ALength - 3);

    Encodings^[Count] := LoByte(Code);
    inc(Count);
    Encodings^[Count] := HiByte(Code);
    inc(Count);

    FEncoders^[FFlagIndex] := FEncoders^[FFlagIndex] or BitMask[FFlagBit];
    dec(FFlagBit);
  except
    FError := EC_UNKNOWN;
  end;
  Result := FError;
end; // TmcmImageLZ.AddDistLenToEncodings.


function TmcmImageLZ.GetDistLenToEncodings(var ALiteral  : byte;
                                           var ADistance : integer;
                                           var ALength   : integer;
                                               Encodings : PVectorB;
                                           var Count     : cardinal) : boolean;
var IsLiteral : boolean;
begin
  if (Count >= FEncoderSize)
  then begin
       if Assigned(FOnNeedData)
       then FOnNeedData(Self, Encodings, FEncoderSize);
       Count := 0;
  end;

  if (FFlagBit < 0)
  then begin
       FFlagBit := 7;
       FFlagIndex := Count;
       FFlag := Encodings^[Count];
       inc(Count);
  end;

  if (Count >= FEncoderSize)
  then begin
       if Assigned(FOnNeedData)
       then FOnNeedData(Self, Encodings, FEncoderSize);
       Count := 0;
  end;

  IsLiteral := (FFlag and BitMask[FFlagBit]) = 0;
  dec(FFlagBit);
  if IsLiteral
  then begin
       ALiteral := Encodings^[Count];
       inc(Count);
  end
  else begin
       ADistance := Encodings^[Count];
       inc(Count);

       if (Count >= FEncoderSize)
       then begin
            if Assigned(FOnNeedData)
            then FOnNeedData(Self, Encodings, FEncoderSize);
            Count := 0;
       end;

       ADistance := ADistance or Encodings^[Count] shl 8;
       inc(Count);
       ALength   := (ADistance and FLengthMask) + 3;
       ADistance := (ADistance shr FDistanceShift) + 1;
  end;

  Result := IsLiteral;
end; // TmcmImageLZ.GetDistLenToEncodings.


function TmcmImageLZ.Compress(var Buffer     : pointer;
                              var BufferSize : longword;
                              var DataCount  : longword) : TmcmErrorCode;
var i : longint;
begin
  FError    := EC_OK;
  DataCount := 0;

  // Prepare for the compression
  FEncoders := AllocMem(FEncoderSize * SizeOf(byte));
  FHashTable := TmcmHashTable.Create(521);
  FSlidingWindow := TmcmSlidingWindow.Create(FWindowSize, FMaxMatchLength, True);
  if Assigned(FOnNeedData)
  then FSlidingWindow.OnNeedData := FOnNeedData
  else FSlidingWindow.OnNeedData := OnSlidingNeedData;

  FCodeCount := 0;
  try
    FillChar(FEncoders^, FEncoderSize * SizeOf(byte), 0);

    // Get the first key
    FSlidingWindow.GetNextKey(FKey, FOffset);

    // while the key is three characters long...
    while (length(FKey.AsString) = 3) and (FError = EC_OK)
    do begin
       // Find the longest match in the sliding window using the hash
       // table to identify matches
       FEnumData.SW := FSlidingWindow;
       FEnumData.MaxLen := 0;
       if FHashTable.FindAll(FKey,
                             FOffset - FWindowSize,
                             MatchLongest,
                             @FEnumData)
       then begin
            // We have a match: save the distance / length pair and advance
            // the sliding window by the length.
            if (FEnumData.MaxLen > FMaxMatchLength)
            then FError := EC_BADDECOMPRESS
            else begin
                 AddDistLenToEncodings(FEnumData.DistMaxMatch,
                                       FEnumData.MaxLen,
                                       FEncoders, FCodeCount);
                 FSlidingWindow.Advance(FEnumData.MaxLen);
            end;
       end
       else begin
            // We don't have a match: save the current character and
            // advance by 1.
            AddCodeToEncodings(byte(FKey.AsString[1]),
                               FEncoders, FCodeCount);
            FSlidingWindow.Advance(1);
       end;
       // Now add this key to the hash table.
       FHashTable.Insert(FKey, FOffset, 0, 1);

       // Get the next key.
       FSlidingWindow.GetNextKey(FKey, FOffset);
    end;

    // If the last key was two characters or less, save them as
    // literal character encodings.
    if (Length(FKey.AsString) > 0)
    then begin
         for i := 1 to length(FKey.AsString)
         do AddCodeToEncodings(byte(FKey.AsString[i]),
                               FEncoders, FCodeCount);
    end;

    // Make sure we write out the final encodings.
    if (FCodeCount > 0)
    then begin
         if Assigned(FOnCompressData)
         then FOnCompressData(Self, FEncoders, FCodeCount);
         FCodeCount := 0;
    end;
  except
    FError := EC_BADCOMPRESS;
    DataCount := 0;
  end;
  if Assigned(FHashTable)
  then FHashTable.Destroy;
  FHashTable := Nil;
  if Assigned(FSlidingWindow)
  then FSlidingWindow.Destroy;
  FSlidingWindow := Nil;
  FreeMem(FEncoders);
  FEncoders := Nil;

  Result := FError;
end; // TmcmImageLZ.Compress.


function TmcmImageLZ.Decompress(    Buffer     : pointer;
                                var BufferSize : longword;
                                var DataCount  : longword) : TmcmErrorCode;
var i         : cardinal;
    ALiteral  : byte;
    ADistance : integer;
    ALength   : integer;
begin
  FError := EC_OK;

  // Prepare for the compression
  if (FSlidingWindow = Nil)
  then FSlidingWindow := TmcmSlidingWindow.Create(FWindowSize, FMaxMatchLength, False);

  FSlidingWindow.OnHasData := OnSlidingHasData;
  DataCount := FDataToCompress;
  FEncoderSize := BufferSize;

  FCodeCount := 0;
  if Assigned(FOnNeedData)
  then FOnNeedData(Self, Buffer, FEncoderSize);
  i := 0;

  try
    while (FCodeCount < FDataToCompress) and (FError = EC_OK)
    do begin
       if GetDistLenToEncodings(ALiteral,
                                ADistance,
                                ALength,
                                Buffer, i)
       then begin // Literal
            FSlidingWindow.AddByte(ALiteral);
            inc(FCodeCount);
       end
       else begin // Distance Length code
            inc(FCodeCount, ALength);
            if (ALength > FMaxMatchLength)
            then FError := EC_BADDECOMPRESS
            else FSlidingWindow.AddDistLen(ADistance, ALength);
       end;
    end;
  finally
    if Assigned(FSlidingWindow)
    then FSlidingWindow.Destroy;
    FSlidingWindow := Nil;
  end;
  Result := FError;
end; // TmcmImageLZ.Decompress.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}
{$IFDEF OVERFLOW_OFF}{$UNDEF OVERFLOW_OFF}{$Q+}{$ENDIF}

end.
