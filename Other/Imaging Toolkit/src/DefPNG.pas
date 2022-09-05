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
// $Log:  17513: DefPNG.pas 
//
//   Rev 1.2    27-10-2004 22:19:10  mcm    Version: IMG 2.6
// Renamed filter definitions to avoid naming conflicts.

//
//   Rev 1.1    27-01-2003 13:51:38  mcm

//
//   Rev 1.0    27-05-2002 16:21:48  mcm

unit DefPNG;

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

type
  PPNGHashEntry = ^TPNGHashEntry;
  TPNGHashEntry = packed record
    Index    : word;
    Next     : PPNGHashEntry;
    Previous : PPNGHashEntry;
  end;

  TPNGChunk = packed record
    Length : cardinal;        //
    Name   : string[4];       //
    Data   : pointer;         //
    CRC    : cardinal;        //
  end;
  PPNGChunk = ^TPNGChunk;

  TPNGHeader = packed record
    Width       : cardinal;   // Image width
    Height      : cardinal;   // Image height
    BitDepth    : byte;       // Bit depht
    ColorType   : byte;       //
    Compression : byte;       // Must be zero.
    Filter      : byte;       // Must be zero.
    Interlaced  : byte;       // = 0 -> Non-interlaced
                              // = 1 -> Interlaced.
  end;
  PPNGHeader = ^TPNGHeader;

  TPNGPixelSize = packed record
    XPixelPerUnit : cardinal; //
    YPixelPerUnit : cardinal; //
    UnitSpecifier : byte;     //
  end;
  PPNGPixelSize = ^TPNGPixelSize;

  TPNGTime = packed record
    Year   : word;            //
    Month  : byte;            //
    Day    : byte;            //
    Hour   : byte;            //
    Minute : byte;            //
    Second : byte;            //
  end;
  PPNGTime = ^TPNGTime;

  TPNGCompressHeader = packed record
    // 1. Compression Header = word
    CM_WS    : byte; // Windows size (must be 7 or less, upper 4 bit).
                     // Compress method (must be 8, lower 4 bit).
    CB_PD_CL : byte; // Compression Level (lower 2 bit),
                     //   = 0 -> Fastest compression used.
                     //   = 1 -> Fast compression used.
                     //   = 2 -> Default compression used.
                     //   = 3 -> Maximum compression used.
                     // Preset Directory (must be 0, 1 bit).
                     // Check bits (lower 5 bit), makes the AddlerCRC word a multiple of 31.
    // 2. Compressed data block follows. Variable size.
    // 3. Finally Adler 32 checksum (4 bytes), calculated from the entire
    //    uncompressed data, ie. all data chunks.
  end;
  PPNGCompressHeader = ^TPNGCompressHeader;

  // PNG compression methods.
  TPNGCompressType = (CPPNG_NONE, CPPNG_FIXEDHUFFMAN, CPPNG_DYNAMICHUFMANN);
  TPNGFilters      = (PNGFLT_NONE, PNGFLT_SUB, PNGFLT_UP, PNGFLT_AVERAGE, PNGFLT_PAETH);

const
  PNGSignature            : array[0..7] of byte = (137, 80, 78, 71, 13, 10, 26, 10);
  PNGWindowSize           : cardinal = 1 shl 15;
  PNGWindowMask           : cardinal = (1 shl 15) - 1;
  PNGAdlerPrime           : cardinal = 65521;
  // Number of passes in an interlaced image.
  PNGInterlaceCount       = 7;
  // Max. number of Huffman codes.
  PNGMaxHuffmanCodes      = 288;
  // Max. code length, bitwise.
  PNGMaxLengthCodeSize = 15;
  // Max. code length, bitwise.
  PNGMaxDistanceSize = 15;
  
  PNGMaxDistanceCode = 30;
  // Max. number of length codes used to decompress dynamic Huffman
  // Literal/Length and distance codes.
  PNGLengthSize = 19;

  // Size def. for look ahead buffer.
  PNGLookAheadSize = 1 shl 9;
  PNGLookAheadMask = PNGLookAheadSize - 1;

  PNGLongestLength = 258;

  PNGFirstLengthCode = 257;
  PNGEndCode = 256;

  // Hash table size
  PNGHashBits = 5;
  PNGHashTableSize : cardinal = 1 shl (3 * PNGHashBits);
  PNGHashMask = (1 shl PNGHashBits) - 1;

  PNGOutputBufferSize : cardinal = 1 shl 13;

  // End of Image marker used by Hash table.
  PNGEndImage = $0FFFF;

  PNGDistExtra   : array[0..29] of word = (0, 0, 0, 0, 1, 1, 2, 2,
                                           3, 3, 4, 4, 5, 5, 6, 6,
                                           7, 7, 8, 8, 9, 9, 10, 10,
                                           11, 11, 12, 12, 13, 13);

  PNGDistBase    : array[0..29] of word = (1, 2, 3, 4, 5, 7, 9, 13,
                                           17, 25, 33, 49, 65, 97, 129, 193,
                                           257, 385, 513, 769, 1025,  1537,
                                           2049, 3073, 4097, 6145, 8193, 12289,
                                           16385, 24577);

  PNGMaxDist     : array[0..29] of word = (1, 2, 3, 4, 6, 8, 12, 16, 24, 32,
                                           48, 64, 96, 128, 192, 256, 384, 512,
                                           768, 1024, 1536, 2048, 3072, 4096,
                                           6144, 8192, 12288, 16384, 24576, 32768);

  PNGLengthOrder : array[0..PNGLengthSize-1] of byte = (16, 17, 18,  0,  8,
                                                         7,  9,  6, 10,  5,
                                                        11,  4, 12,  3, 13,
                                                         2, 14,  1, 15);

  PNGLengthExtra : array[0..30] of word = (0, 0, 0, 0, 0, 0, 0, 0,
                                           1, 1, 1, 1, 2, 2, 2, 2,
                                           3, 3, 3, 3, 4, 4, 4, 4,
                                           5, 5, 5, 5, 0, 0, 0);

  PNGLengthBase  : array[0..30] of word = (3, 4, 5, 6, 7, 8, 9, 10,
                                           11, 13, 15, 17, 19, 23, 27, 31,
                                           35, 43, 51, 59, 67, 83, 99, 115,
                                           131, 163, 195, 227, 258, 0, 0);

  PNGLengthCodes : array[0..255] of word = (257, 258, 259, 260, 261, 262, 263, 264,
                                            265, 265, 266, 266, 267, 267, 268, 268,
                                            269, 269, 269, 269, 270, 270, 270, 270,
                                            271, 271, 271, 271, 272, 272, 272, 272,
                                            273, 273, 273, 273, 273, 273, 273, 273,
                                            274, 274, 274, 274, 274, 274, 274, 274,
                                            275, 275, 275, 275, 275, 275, 275, 275,
                                            276, 276, 276, 276, 276, 276, 276, 276,
                                            277, 277, 277, 277, 277, 277, 277, 277,
                                            277, 277, 277, 277, 277, 277, 277, 277,
                                            278, 278, 278, 278, 278, 278, 278, 278,
                                            278, 278, 278, 278, 278, 278, 278, 278,
                                            279, 279, 279, 279, 279, 279, 279, 279,
                                            279, 279, 279, 279, 279, 279, 279, 279,
                                            280, 280, 280, 280, 280, 280, 280, 280,
                                            280, 280, 280, 280, 280, 280, 280, 280,
                                            281, 281, 281, 281, 281, 281, 281, 281,
                                            281, 281, 281, 281, 281, 281, 281, 281,
                                            281, 281, 281, 281, 281, 281, 281, 281,
                                            281, 281, 281, 281, 281, 281, 281, 281,
                                            282, 282, 282, 282, 282, 282, 282, 282,
                                            282, 282, 282, 282, 282, 282, 282, 282,
                                            282, 282, 282, 282, 282, 282, 282, 282,
                                            282, 282, 282, 282, 282, 282, 282, 282,
                                            283, 283, 283, 283, 283, 283, 283, 283,
                                            283, 283, 283, 283, 283, 283, 283, 283,
                                            283, 283, 283, 283, 283, 283, 283, 283,
                                            283, 283, 283, 283, 283, 283, 283, 283,
                                            284, 284, 284, 284, 284, 284, 284, 284,
                                            284, 284, 284, 284, 284, 284, 284, 284,
                                            284, 284, 284, 284, 284, 284, 284, 284,
                                            284, 284, 284, 284, 284, 284, 284, 285);

type
  // Entry 0 = Row interval
  // Entry 1 = Column interval
  // Entry 2 = Start row
  // Entry 3 = Start column
  TPNGInterlacePass = array[0..3] of word;

const
  PNGInterlaceInfo  : array[0..6] of TPNGInterlacePass = ((8, 8, 0, 0),
                                                          (8, 8, 0, 4),
                                                          (8, 4, 4, 0),
                                                          (4, 4, 0, 2),
                                                          (4, 2, 2, 0),
                                                          (2, 2, 0, 1),
                                                          (2, 1, 1, 0));

implementation

end.
