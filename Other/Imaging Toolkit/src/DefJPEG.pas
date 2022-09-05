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
// $Log:  17509: DefJPEG.pas 
//
//    Rev 1.2    26-08-2009 22:49:16  mcm
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.1    27-01-2003 13:54:00  mcm

//
//   Rev 1.0    27-05-2002 16:21:48  mcm

unit DefJPEG;

interface

const // JPEG Markers : word
      // Stand-alone JPEG markers
      JPG_TEM   = $FF01; // Temporary for arithmetric coding
      JPG_RST0  = $FFD0; // Restart markers 0 - 7
      JPG_RST1  = $FFD1;
      JPG_RST2  = $FFD2;
      JPG_RST3  = $FFD3;
      JPG_RST4  = $FFD4;
      JPG_RST5  = $FFD5;
      JPG_RST6  = $FFD6;
      JPG_RST7  = $FFD7;
      JPG_SOI   = $FFD8; // Start Of Image
      JPG_EOI   = $FFD9; // End Of Image

      // JPEG markers with data.
      JPG_SOF0  = $FFC0; // Start Of Frame, baseline
      JPG_SOF1  = $FFC1; // Start Of Frame, extended sequential
      JPG_SOF2  = $FFC2; // Start Of Frame, progressive
      JPG_SOF3  = $FFC3; // Start Of Frame, lossless
      JPG_DHT   = $FFC4; // Define Huffman table
      JPG_SOF5  = $FFC5; // Start Of Frame, differential sequential
      JPG_SOF6  = $FFC6; // Start Of Frame, differential progressive
      JPG_SOF7  = $FFC7; // Start Of Frame, differential lossless
      JPG_JPG   = $FFC8; // Reserved
      JPG_SOF9  = $FFC9; // Start Of Frame, extended sequential, arithmetric coding
      JPG_SOF10 = $FFCA; // Start Of Frame, progressive, arithmetric coding
      JPG_SOF11 = $FFCB; // Start Of Frame, lossless, arithmetric coding
      JPG_DAC   = $FFCC; // Define arithmetric coding conditions
      JPG_SOF13 = $FFCD; // Start Of Frame, differential sequential, arithmetric coding
      JPG_SOF14 = $FFCE; // Start Of Frame, differential progressive, arithmetric coding
      JPG_SOF15 = $FFCF; // Start Of Frame, differential lossless, arithmetric coding
      JPG_SOS   = $FFDA; // Start Of Scan
      JPG_DQT   = $FFDB; // Define quantization tables
      JPG_DNL   = $FFDC; // Define Number of Lines
      JPG_DRI   = $FFDD; // Define Restart Interval
      JPG_DHP   = $FFDE; // Define Hierarchical Progression
      JPG_EXP   = $FFDF; // Expand reference components
      JPG_APP0  = $FFE0; // Application specific data 0 - 15
      JPG_APP1  = $FFE1;
      JPG_APP2  = $FFE2;
      JPG_APP3  = $FFE3;
      JPG_APP4  = $FFE4;
      JPG_APP5  = $FFE5;
      JPG_APP6  = $FFE6;
      JPG_APP7  = $FFE7;
      JPG_APP8  = $FFE8;
      JPG_APP9  = $FFE9;
      JPG_APP10 = $FFEA;
      JPG_APP11 = $FFEB;
      JPG_APP12 = $FFEC;
      JPG_APP13 = $FFED;
      JPG_APP14 = $FFEE;
      JPG_APP15 = $FFEF;
      JPG_COM   = $FFFE; // Comment
      JPG_JPG0  = $FFF0; // Reserved 0 - 13
      JPG_JPG1  = $FFF1;
      JPG_JPG2  = $FFF2;
      JPG_JPG3  = $FFF3;
      JPG_JPG4  = $FFF4;
      JPG_JPG5  = $FFF5;
      JPG_JPG6  = $FFF6;
      JPG_JPG7  = $FFF7;
      JPG_JPG8  = $FFF8;
      JPG_JPG9  = $FFF9;
      JPG_JPG10 = $FFFA;
      JPG_JPG11 = $FFFB;
      JPG_JPG12 = $FFFC;
      JPG_JPG13 = $FFFD;
      JPG_RES   = $FF02; // Reserved FF02 - FFBF

      JPG_SOB   = $FF00; // 

const
  JPEGMaxHuffmanTables           = 4;
  JPEGMaxQuantizationTables      = 4;
  JPEGMaxComponentsPerFrame      = 255;
  JPEGMaxComponentsPerScan       = 4;
  JPEGMinSamplingFrequency       = 1;
  JPEGMaxSamplingFrequency       = 4;
  JPEGSampleWidth                = 8;
  JPEGSampleSize                 = JpegSampleWidth * JpegSampleWidth;
  JPEGMinSampleValue             = 0;
  JPEGMaxSampleValue             = 255; // For 12-Bits this would be 4095
  JPEGMidPointSampleValue        = 128; // For 12-Bits this 2048
  JPEGMidPointSampleValueX2      = 256; //
  JPEGMaxHuffmanCodeLength       = 16;
  JPEGMaxHuffmanCodes            = 256;
  JPEGMaxDataUnitsPerMCU         = 10;
  JPEGMaxSuccessiveApproximation = 13;
  JPEGMax8BitQuantizationValue   = 255;
  JPEGMinQuantizationValue       = 1;

  JPEGQuantizationIntegerScale   = 12;

const
  JPEGZigZagInputOrder  : array[0..JpegSampleSize-1] of byte =
                          ( 0,  1,  8, 16,  9,  2,  3, 10,
                           17, 24, 32, 25, 18, 11,  4,  5,
                           12, 19, 26, 33, 40, 48, 41, 34,
                           27, 20, 13,  6,  7, 14, 21, 28,
                           35, 42, 49, 56, 57, 50, 43, 36,
                           29, 22, 15, 23, 30, 37, 44, 51,
                           58, 59, 52, 45, 38, 31, 39, 46,
                           53, 60, 61, 54, 47, 55, 62, 63);

  JPEGZigZagOutputOrder : array[0..JpegSampleSize-1] of byte =
                          ( 0,  1,  5,  6, 14, 15, 27, 28,
                            2,  4,  7, 13, 16, 26, 29, 42,
                            3,  8, 12, 17, 25, 30, 41, 43,
                            9, 11, 18, 24, 31, 40, 44, 53,
                           10, 19, 23, 32, 39, 45, 52, 54,
                           20, 21, 33, 38, 46, 51, 55, 60,
                           21, 34, 37, 47, 50, 56, 59, 61,
                           35, 36, 48, 49, 57, 58, 62, 63);

  JPEGQuantLuminance    : array[0..JpegSampleSize-1] of word =
                          (16, 11, 10, 16,  24,  40,  51,  61,
                           12, 12, 14, 19,  26,  58,  60,  55,
                           14, 13, 16, 24,  40,  57,  69,  56,
                           14, 17, 22, 29,  51,  87,  80,  62,
                           18, 22, 37, 56,  68, 109, 103,  77,
                           24, 35, 55, 64,  81, 104, 113,  92,
                           49, 64, 78, 87, 103, 121, 120, 101,
                           72, 92, 95, 98, 112, 100, 103,  99);

  JPEGQuantChrominance  : array[0..JpegSampleSize-1] of word =
                          (17, 18, 24, 47, 99, 99, 99, 99,
                           18, 21, 26, 66, 99, 99, 99, 99,
                           24, 26, 56, 99, 99, 99, 99, 99,
                           47, 66, 99, 99, 99, 99, 99, 99,
                           99, 99, 99, 99, 99, 99, 99, 99,
                           99, 99, 99, 99, 99, 99, 99, 99,
                           99, 99, 99, 99, 99, 99, 99, 99,
                           99, 99, 99, 99, 99, 99, 99, 99);
                          
type
  // APP0 table.
  TJFIFAPP0Header = packed record
    Identifier    : array[0..4] of AnsiChar; // "JFIF#0"
    MajorID       : byte; // Major ID of the file version.
    MinorID       : byte; // Minor ID of the file version.
    Units         : byte; // 0 -> No units used, X-YDensity specifies aspect ratio.
                          // 1 -> X-YDensity are pixels per inch.
                          // 2 -> X-YDensity are pixels per centimeter.
    XDensity      : word; // Horizontal resolution
    YDensity      : word; // Vertical resolution
    XThumbnail    : byte; // Width of optional thumbnail.
    YThumbnail    : byte; // Height of optional thumbnail.
    // Thumbnail     : variable size;
  end;

  // APP0 Extension table.
  TJFIFAPP0Extension = packed record
    Identifier    : array[0..4] of AnsiChar; // "JFXX#0"
    ExtCode       : byte; // Extension code
                          // 10h -> Thumbnail is encoded using JPEG.
                          // 11h -> Thumbnail is encoded with 1 byte per pixel.
                          //        Has extension data.
                          // 12h -> Thumbnail is encoded with 3 byte per pixel.
                          //        Has extension data.
  end;

  TJFIFAPP0ExtData = packed record
    XThumbnail    : byte; // Width of thumbnail.
    YThumbnail    : byte; // Height of thumbnail.
    // Palette       : array[0..255] of TmcmRGB;
  end;

  // DHT table.

  TJFIFDHT = packed record
    ClassIdentifier : byte; // 4 MSB specify the table class.
                            // 0 -> DC
                            // 1 -> AC
                            // 4 LSB specify the table identifier.
                            // 0 or 1 -> for base line frames
                            // 0..3   -> for progressive and extended frames.
    CodeLength      : array[0..15] of byte; // Count of Huffman code for each of
                                            // the 16 entries.
   // Variable        : byte; // Sum of the 16 Huffman code counts.
  end;

  // DQT table.
  TJFIFDQT = packed record
    Identifier : byte; // 4 MSB specify the quantification value size
                       // 0 -> 1 byte
                       // 1 -> 2 byte
                       // 4 LSB are the table identifier (0, 1, 2 or 3).
    Values     : array[0..JPEGSampleSize-1] of word; // Quantification values,
                                                     // 1 or 2 byte unsigned.
  end;

  // SOF table
  TJFIFSOFComponent = packed record
    Identifier    : byte; // Component identifier. JPEG allow values 0..255,
                          // JFIF restricts this to
                          // 1 -> Y
                          // 2 -> Cb
                          // 3 -> Cr
    Samples       : byte; // 4 MSB specify horizontal sampling for the component.
                          // 4 LSB specify vertical sampling for the component.
                          // Both 4 MSB and LSB can be 1..4.
    DQTIdentifier : byte; // Quantification Table (DQT) identifier (1..4).
  end;

  TJFIFSOF_Minor = packed record
    Bits         : byte; // Sample precision in bits, 8 or 12 bits.
    ImageHeight  : word; // Image height in pixels.
    ImageWidth   : word; // Image width in pixels.
    NoComponents : byte; // Number of components in the image.
  end;

  TJFIFSOF = packed record
    Bits         : byte; // Sample precision in bits, 8 or 12 bits.
    ImageHeight  : word; // Image height in pixels.
    ImageWidth   : word; // Image width in pixels.
    NoComponents : byte; // Number of components in the image.
    Components   : array[0..2] of TJFIFSOFComponent;
  end;

  // SOS table
  TJFIFSOSComponent = packed record
    Identifier    : byte; // Component identifier. JPEG allow values 0..255,
                          // JFIF restricts this to
                          // 1 -> Y
                          // 2 -> Cb
                          // 3 -> Cr
    DCACHuffman   : byte; // 4 MSB specify the DC Huffman table.
                          // 4 LSB specify the AC Huffman table.
  end;

  TJFIFSOS = packed record
    Count         : byte; // Number of components.
    Component     : array[0..2] of TJFIFSOSComponent;
    SpectralStart : byte; // Spectal selection start 0..63.
    SpectralEnd   : byte; // Spectal selection end 0..63.
    Approximation : byte; // Successive approximation, 4 MSB and 4 LSB 0..13.
  end;

  TJPEGCoefficient = array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of smallint; // Array of 16 bit integers.
  PJPEGCoefficient = ^TJPEGCoefficient;

  TJPEGQuantification = TJPEGCoefficient;
  PJPEGQuantification = ^TJPEGCoefficient;

implementation

end.
