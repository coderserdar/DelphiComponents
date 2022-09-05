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
// $Log:  17557: mcmImageTypeDef.pas 
//
//    Rev 1.34    2014-04-06 13:06:24  mcm
//
//    Rev 1.33    2014-02-02 21:10:04  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.32    01-01-2009 13:44:56  mcm
// Initial support for JBIG.
//
//    Rev 1.31    20-08-2007 20:28:36  mcm
// Added support for Delphi 2007
//
//    Rev 1.30    05-03-2006 10:51:02  mcm    Version: IMG 2.16
// Added OCR Color error.
//
//    Rev 1.29    22/02/2006 00:08:46  mcm
//
//    Rev 1.28    19-02-2006 21:13:48  mcm    Version: IMG 2.15
// Added custom cursor constant definitions.
//
//   Rev 1.27    23-08-2005 20:38:34  mcm    Version: IMG 2.9
// Modified TmcmFileFormat and all it's members to integer.
// Added FF_USERDEFINED constant.
// Removed TmcmFileFormats.
// CFileFormatStrings is depricated and will be removed.
// Changed TmcmCompress and all it's members to an integer.
// Removed TmcmCompression.
// Added CP_USERDEFINED constant.
// CCompressStrings is depricated and will be removed.

//
//   Rev 1.26    31-07-2005 21:02:46  mcm
// Added File move error code.

//
//   Rev 1.25    24-07-2005 18:49:02  mcm    Version: IMG 2.9
// Added error message for TmcmHandleStream.

//
//   Rev 1.24    23-05-2005 21:42:30  mcm    Version: IMG 2.9
// New error definition for Marr-Hilderth, and trace threshold.

//
//   Rev 1.23    13-02-2005 20:01:34  mcm
// Added TH_TRIANGULAR define.

//
//   Rev 1.22    03-02-2005 21:16:10  mcm
// Added DeskewMinMax potential error definition.

//
//   Rev 1.21    20-01-2005 22:24:08  mcm
// Added new BADPARAMETER error code.

//
//   Rev 1.20    28-10-2004 19:42:44  mcm    Version: IMG 2.6
// Included support for C++Builder.

//
//   Rev 1.19    06-10-2004 19:22:14  mcm    Version: IMG 2.6
// Added type definitions to support C++Builder.

//
//   Rev 1.18    26-09-2004 11:13:28  mcm    Version: IMG 2.6
// Renamed compression const.

//
//   Rev 1.17    26-08-2004 23:53:20  mcm    Version: IMG 2.6
// Added display error used by TmcmImage.Draw.

//
//   Rev 1.16    01-06-2004 00:04:50  mcm    Version: IMG 2.5
// Added destinct resource strings for GIF.

//
//   Rev 1.15    14-03-2004 09:06:00  mcm    Version: IMG 2.4
// Added definitions/string for NEF file format.

//
//   Rev 1.14    30-01-2004 20:30:22  mcm    Version: IMG 2.3
// Added Stream and TmcmProfile error codes.
// Added additional Border constants for use by TmcmImageCtrl. 

//
//   Rev 1.13    03-01-2004 13:01:18  mcm
// Added CPUID constant.

//
//   Rev 1.12    24-11-2003 20:18:12  mcm
// Removed IF_RGB32, IF_RGBA32 should be used instead.

//
//   Rev 1.11    16-10-2003 11:32:00  mcm    Version: IMG 1.6

//
//   Rev 1.10    29-09-2003 18:45:46  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.9    25-09-2003 23:41:48  mcm    Version: IMG 1.5
// Added new interpolation definitions and error code.

//
//   Rev 1.8    25-07-2003 00:52:28  mcm

//
//   Rev 1.7    06-07-2003 10:53:34  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.6    12-05-2003 15:29:32  mcm    Version: IMG 1.3.4
// Added new error definition and Shortint Vector.

//
//   Rev 1.5    25-03-2003 22:05:32  mcm    Version: IMG 1.3.3
// Added error codes for Multiple image/page files.

//
//   Rev 1.4    05-02-03 16:27:12  mcm
// Renamed YCbCr ratio definition.

//
//   Rev 1.3    29-01-2003 15:44:16  mcm
// New error codes for JPEG.

//
//   Rev 1.2    27-09-2002 13:07:12  mcm    Version: IMG 1.2
// New Threshold and SGI definitions.

//
//   Rev 1.1    07-08-2002 14:12:18  mcm    Version: IMG 1.1
// Added new error, file and compression definitions.

//
//   Rev 1.0    27-05-2002 16:22:08  mcm

unit mcmImageTypeDef;

interface

{$Include 'mcmDefines.pas'}

{$DEFINE DELPHI}

uses {$IFNDEF GE_DXE2}
      Graphics, Windows,
     {$ELSE}
      WinApi.Windows, Vcl.Graphics,
     {$ENDIF}
     mcmImageResStr;

const RegKey = '\SOFTWARE\MCM DESIGN\mcmImaging\'; // Used to store user selections.

//------------------------------------------------------------------------------
type
{$IFDEF DCB3_6}
  TOnProcessTime = procedure(Sender : TObject; StartTime, EndTime : TLargeInteger) of object;
{$ELSE}
  TOnProcessTime = procedure(Sender : TObject; StartTime, EndTime : int64) of object;
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

  //----------------------------------------------------------------------------
  // Definitions used for compatibility with C++Builder.
  // Problem: C++Builder converts HWND, HBITMAP, HPALETTE etc to void pointers
  // instead of integers, which causes linker errors. To solve this problem
  // we'll use our own difinitions as replacements.
  TmcmHDC = integer;
  TmcmHWND = integer; // An HWND
  TmcmHBITMAP = integer; // An HBITMAP
  TmcmHPALETTE = integer; // An HPALETTE

  //----------------------------------------------------------------------------
  // Error codes
  TmcmErrorCode = (EC_OK,             // No error.
                   EC_WINDOWS,        // A Windows function didn't return an expected value.
                   EC_NOMEMORY,       // Failed to allocate memory.

                   // TmcmImageFile errors.
                   EC_NOSTREAM,       // File stream is not initialised.
                   EC_OPENFILE,       // Cannot open image file.
                   EC_CREATEFILE,     // Cannot create file.
                   EC_MOVEFILE,       // Cannot move file.                   
                   EC_FILENOTOPEN,    // File was not open for read or write.
                   EC_READFROMFILE,   // Error reading data from file.
                   EC_WRITETOFILE,    // Error writing data to file.
                   EC_BADFORMAT,      // Un-supported or incorrect file format.
                   EC_UNKNOWNFORMAT,  // Format not supported.
                   EC_ENDOFFILE,      // Unexpected end of file.
                   EC_DISKSPACE,      // Insufficient disk space.
                   EC_COMPRESSION,    // Un-supported compression scheme.
                   EC_DECOMPRESSION,  // Un-supported decompression scheme,
                                      // or incorrect compressed data.
                   EC_BADCOMPRESS,    // Compression error.
                   EC_BADDECOMPRESS,  // Decompression error.
                   EC_ENDOFDATA,      // Unexpected end of data.
                   EC_COMPRESSCOLOR,  // Color format is not supported by compression scheme.
                   EC_DATAORDER,      // Bit order is not supported.
                   EC_BITDEPTH,       // Un-supported bit depth.
                   EC_TIFFTAG,        // Un-supported or Unknown TIFF Tag.
                   EC_BADIMAGEHANDLE, // Invalid image handle.
                   EC_CRC,            // CRC did not match, incorrect data.
                   EC_NOHUFFMANCODES, // Huffman codes are not defined.
                   EC_MAXHUFFMANCODES,// Number of huffman codes exceed allowed size.

                   // JPEG erros.
                   EC_SAMPLEFREQ,       // Horizontal or vertical sample frequency is zero.
                   EC_HORIZFREQ,        // Fractional horizontal frequency.
                   EC_VERTFREQ,         // Fractional vertical frequency.
                   EC_SUCCAPPROX,       // Successive approximation extends range.
                   EC_MAXHUFFMANTABLES, // Too many huffman tables.
                   EC_NOPROGRESSIVE,    // JPEG, Progressive JPEG is not supported.
                   EC_NOLOSSLESSHUFFMAN,// JPEG, Lossless Huffman coding is not supported.
                   EC_NODIFFSEQHUFFMAN, // JPEG, Differential sequential Huffman coding is not supported.
                   EC_NODIFFPRGHUFFNAM, // JPEG, Differential progressive Huffman coding is not supported.
                   EC_NODIFFLOSSHUFFMAN,// JPEG, Differential lossless Huffman coding is not supported.
                   EC_JFIFBADCOMPCOUNT, // JFIF only supports 1 and 3 components (Y, Cb and Cr).
                   EC_JPEGDHPCODE,      // JPEG, Hierarchical Progression is not supported.
                   EC_JFIFBADCOMPID,    // JFIF, Invalid component identifier or identifier is out of order.
                   EC_JPEGMISSOF,       // JPEG, Start Of Scan reached before Start Of Field.
                   EC_JPEGMISTABLE,     // JPEG, One or more tables are missing (AC, DC or Quantification).
                   EC_JPEGPROGACDC,     // Progressive scan contains both DC and AC data.
                   EC_DUPCOMPSCAN,      // Duplicate component scan.

                   // JBIG errors.
                   EC_JBIGMAXSIZE, // JBIG, Reached specified maximum size",                       /* JBG_EOK_INTR */
                   // EC_ENDOFDATA EC_, // JBIG, Unexpected end of data", /* JBG_EAGAIN */
                   // EC_NOMEMORY // EC_, // JBIG, Not enough memory available",                          /* JBG_ENOMEM */
                   EC_JBIGABORTMARK, // JBIG, ABORT marker found",                                   /* JBG_EABORT */
                   EC_JBIGMARKSEGMENT, // JBIG, Unknown marker segment encountered",                   /* JBG_EMARKER */
                   EC_JBIGBIENOFIT, // JBIG, Incremental BIE does not fit to previous one",         /* JBG_ENOCONT */
                   EC_JBIGINVALID, // JBIG, Invalid data encountered",                             /* JBG_EINVAL */
                   EC_UNIMPLFEATURE, // JBIG, Unimplemented features used"                           /* JBG_EIMPL */

                   EC_PATENTS,          // Arithmetic coding covered is covered by patents.

                   // TIFF errors.
                   EC_PREDICTORBIT,     // Difference Predictor does not support image bit depth.
                   EC_NOSUBIMAGE,       // No sub-images.
                   EC_APPENDIMAGE,      // Could not append image to file. File format does not support multiple images.

                   //  General TmcmImagexxx errors.
                   EC_MISSOURCEIMAGE,   // A source image is not assigned.
                   EC_MISRESULTIMAGE,   // A result image is not assigned or could not be created.

                   EC_BADTARGETSIZE,    // Target image does not have correct spatial dimensions.
                   EC_NOMATCHFORMAT,    // Source images does not have matching formats (Color, Height or Width).
                   EC_UNKNOWNMETHOD,    // Method parameter is unknown.
                   EC_DISPLAYIMAGE,     // Error copying image lines to device.

                   // TmcmImageColor errors.
                   EC_BADCOLORFORMAT,   // Color format or bit depht is not supported or incorrect.

                   // TmcmImageTransform errors.
                   EC_NOINTERPOLATE,    // Unknown interpolation method.
                   EC_DESKEWMAXMIN,     // The deskew angle may be incorrect, as the angle found is the maximum allowed.

                   // TmcmImageFilter errors.
                   EC_MARR_DEVIATION,   // Marr-Hildreth, the specified "standard deviation" results in a filter that is too large.

                   // TmcmProfile
                   EC_INVALIDSCANPARAM, // Invalid scan parameter (Scan length <= Average).
                   EC_CHECKXY,          // X or Y coordinate is not within the valid range.

                   EC_HANDLESTREAMERROR,// Out of memory while expanding memory stream.

                   // mcmOCR
                   EC_OCRBADCOLORFORMAT,// Color format or bit depht is not supported. TmcmOCR supports 8 bit Grey scale images!

                   // General
                   EC_BADPARAMETER,     // Bad parameters.
                   EC_USERCANCEL,       // User canceled process.
                   EC_DEMOMODE,         // Facility is not available in this demo edition.

                                        // Last entry in Error Codes.
                   EC_UNKNOWN           // Error is unknown (Exception).
                   );

  //----------------------------------------------------------------------------
  // Image Format.
  TmcmImageFormat  = (IF_BW,       // 1 bit
                      IF_GREY4,    // 4 bit
                      IF_PAL4,     // 4 bit indexed colour.
                      IF_GREY8,    // 8 bit grey
                      IF_PAL8,     // 8 bit indexed colour.
                      IF_RGB15,    // 3 x 5 bit colour.
                      IF_RGB16,    // 3 x 5 bit colour.
                      IF_RGB24,    // 3 x  8 bit colour.
                      IF_RGBA32,   // 3 x 8 bit colour + 8 bit alpha channel.
                      //IF_RGB32,    // 3 x 8 bit colour + 8 bit alpha channel.
                      IF_GREY12,   // 12 bit grey
                      IF_GREY16,   // 16 bit grey
                      IF_RGB36,    // 3 * 12 bit colour.
                      IF_RGB48,    // 3 * 16 bit colour.
                      IF_FLOAT,    // Floating point (double) pixel data.
                      IF_CUSTOM,   // Customer format?.
                      IF_NONE);    // None of the above!?!
  TmcmImageFormats  = Set of TmcmImageFormat;

  //----------------------------------------------------------------------------
  // Border style
  TmcmBorderStyle = (BS_NONE,
                     BS_SINGLE,
                     BS_SUNKEN,
                     BS_RAISED,
                     BS_BUMP,
                     BS_ETCHED);
  //----------------------------------------------------------------------------
  // Unit of measurement.
  TmcmUnit         = (UN_INCHS,
                      UN_CENTIMETERS,
                      UN_PICAS,
                      UN_POINTS,
                      UN_TWIPS,
                      UN_PIXELS,
                      UN_METERS);

  //----------------------------------------------------------------------------
  // Image File Format (FF_xxx).
  // Never change order nor existing entries (Ref. TmcmImageDB).
type
  TmcmFileFormat = integer;
  
const
  FF_GIF    = 0;
  FF_CGM    = 1;
  FF_CRI    = 2;
  FF_DICOM  = 3;
  FF_CUT    = 4;
  FF_FPX    = 5;
  FF_IMG    = 6;
  FF_JPEG   = 7;
  FF_KDC    = 8;
  FF_PCD    = 9;
  FF_PICT   = 10;
  FF_PSP    = 11;
  FF_PBM    = 12;
  FF_PGM    = 13;
  FF_PNG    = 14;
  FF_PPM    = 15;
  FF_SGI    = 16;
  FF_TIFF   = 17;
  FF_TARGA  = 18;
  FF_ICO    = 19;
  FF_BMP    = 20;
  FF_DIB    = 21;
  FF_PCX    = 22;
  FF_NEF    = 23;
  FF_EMF    = 24;
  FF_WMF    = 25;
  FF_WPG    = 26;
  FF_JBIG   = 27;
  FF_JPG2K  = 28;
  {$IFDEF MCMFILEFORMAT}
 // FF_MCF    = 28;
  {$ENDIF}
  FF_NONE   = 29;
  FF_DETECT = 30;

  // User defined file formats .
const
  FF_USERDEFINED = $8000;

(*
type
  TmcmFileFormat   = (FF_GIF,
                      FF_CGM,
                      FF_CRI,
                      FF_DICOM,
                      FF_CUT,
                      FF_FPX,
                      FF_IMG,
                      FF_JPEG,
                      FF_KDC,
                      FF_PCD,
                      FF_PICT,
                      FF_PSP,
                      FF_PBM,
                      FF_PGM,
                      FF_PNG,
                      FF_PPM,
                      FF_SGI,
                      FF_TIFF,
                      FF_TARGA,
                      FF_ICO,
                      FF_BMP,
                      FF_DIB,
                      FF_PCX,
                      FF_NEF,
                      FF_EMF,
                      FF_WMF,
                      FF_WPG,
                      FF_JPG2K,
                      {$IFDEF MCMFILEFORMAT}
                        FF_MCF,
                      {$ENDIF}
                      FF_NONE,
                      FF_DETECT);

  TmcmFileFormats = Set of TmcmFileFormat;
*)

const
  CErrorStrings : array[0..word(EC_UNKNOWN)] of string = (resEC_OK,
                                                          resEC_WINDOWS,
                                                          resEC_NOMEMORY,
                                                          resEC_NOSTREAM,
                                                          resEC_OPENFILE,
                                                          resEC_CREATEFILE,
                                                          resEC_MOVEFILE,
                                                          resEC_FILENOTOPEN,
                                                          resEC_READFROMFILE,
                                                          resEC_WRITETOFILE,
                                                          resEC_BADFORMAT,
                                                          resEC_UNKNOWNFORMAT,
                                                          resEC_ENDOFFILE,
                                                          resEC_DISKSPACE,
                                                          resEC_COMPRESSION,
                                                          resEC_DECOMPRESSION,
                                                          resEC_BADCOMPRESS,
                                                          resEC_BADDECOMPRESS,
                                                          resEC_ENDOFDATA,
                                                          resEC_COMPRESSCOLOR,
                                                          resEC_DATAORDER,
                                                          resEC_BITDEPTH,
                                                          resEC_TIFFTAG,
                                                          resEC_BADIMAGEHANDLE,
                                                          resEC_CRC,
                                                          resEC_NOHUFFMANCODES,
                                                          resEC_MAXHUFFMANCODES,
                                                          resEC_SAMPLEFREQ,
                                                          resEC_HORIZFREQ,
                                                          resEC_VERTFREQ,
                                                          resEC_SUCCAPPROX,
                                                          resEC_MAXHUFFMANTABLES,
                                                          resEC_JPGNOPROGRESSIVE,
                                                          resEC_NOLOSSLESSHUFFMAN,
                                                          resEC_NODIFFSEQHUFFMAN,
                                                          resEC_NODIFFPRGHUFFNAM,
                                                          resEC_NODIFFLOSSHUFFMAN,
                                                          resEC_JFIFBADCOMPCOUNT,
                                                          resEC_JPEGDHPCODE,
                                                          resEC_JFIFBADCOMPID,
                                                          resEC_JPEGMISSOF,
                                                          resEC_JPEGMISTABLE,
                                                          resEC_JPEGPROGACDC,
                                                          resEC_DUPCOMPSCAN,
                                                          resEC_JBIGMAXSIZE,
                                                          resEC_JBIGABORTMARK,
                                                          resEC_JBIGMARKSEGMENT,
                                                          resEC_JBIGBIENOFIT,
                                                          resEC_JBIGINVALID,
                                                          resEC_UNIMPLFEATURE,
                                                          resEC_PATENTS,
                                                          resEC_PREDICTORBIT,
                                                          resEC_NOSUBIMAGE,
                                                          resEC_APPENDIMAGE,
                                                          resEC_MISSOURCEIMAGE,
                                                          resEC_MISRESULTIMAGE,
                                                          resEC_BADTARGETSIZE,
                                                          resEC_NOMATCHFORMAT,
                                                          resEC_UNKNOWNMETHOD,
                                                          resEC_DISPLAYIMAGE,
                                                          resEC_BADCOLORFORMAT,
                                                          resEC_NOINTERPOLATE,
                                                          resEC_DESKEWMAXMIN,
                                                          resEC_MARR_DEVIATION,
                                                          resEC_INVALIDSCANPARAM,
                                                          resEC_CHECKXY,
                                                          resEC_HANDLESTREAMERROR,
                                                          resEC_OCRBADCOLORFORMAT,
                                                          resEC_BADPARAMETER,
                                                          resEC_USERCANCEL,
                                                          resEC_DEMOMODE,
                                                          resEC_UNKNOWN);

  // Depricated, CFileFormatStrings do not use!!!
  CFileFormatStrings : array[0..word(FF_DETECT)] of string = (resGIF,
                                                              resCGM,
                                                              resCRI,
                                                              resDICOM,
                                                              resCUT,
                                                              resFPX,
                                                              resIMG,
                                                              resJPEG,
                                                              resKDC,
                                                              resPCD,
                                                              resPICT,
                                                              resPSP,
                                                              resPBM,
                                                              resPGM,
                                                              resPNG,
                                                              resPPM,
                                                              resSGI,
                                                              resTIFF,
                                                              resTarga,
                                                              resWIcon,
                                                              resWBMP,
                                                              resWOS2DIB,
                                                              resPCX,
                                                              resNEF,
                                                              resEMF,
                                                              resWMF,
                                                              resWPG,
                                                              resJBIG,
                                                              resJPEG2K,
                                                              {$IFDEF MCMFILEFORMAT}
                                                              //resMCF,
                                                              {$ENDIF}
                                                              resUnknown,
                                                              resDetect);


type
  //----------------------------------------------------------------------------
  // Compression, Image files.
  TmcmCompress = integer;
const
  CP_NOCOMP             =  0; // No compression
  CP_RLE4               =  1; // Windows bitmap, 4 bit run-length
  CP_RLE8               =  2; // Windows bitmap, 8 bit run-length
  CP_RLE_TARGA          =  3; // Targa, 8, 16, 24 bit run-length
  CP_RLE_PCX            =  4; // PCX, x bit run-length
  CP_RLE_SGI            =  5; // SGI, 8,24 bit (16 bit not supported) run-length
  CP_ASCII_PBM          =  6; // Portable Bitmap, ASCII
  CP_BIN_PBM            =  7; // Portable Bitmap, Binary
  CP_ASCII_PPM          =  8; // Portable Pixel map, ASCII
  CP_BIN_PPM            =  9; // Portable Pixel map, Binary
  CP_ASCII_PGM          = 10; // Portable Grey map, ASCII
  CP_BIN_PGM            = 11; // Portable Grey map, Binary
  CP_HUFFMAN            = 12; // Huffman coding (8 bit)
  CP_PACKBITS           = 13; // TIFF, Pack bits (1, 4, 8, 24 bit) run-length
  CP_MODIFIED_HUFFMAN   = 14; // TIFF, Modified Huffman (1 bit)
  CP_MODIFIED_HUFFMAN_W = 15; // TIFF, Modified Huffman (1 bit) Word aligned.
  CP_CCITTGROUP3        = 16; // TIFF, CCITT (T.4) Group 3 FAX one dimensional (1 bit)
  CP_CCITTGROUP3_2D     = 17; // TIFF, CCITT (T.4) Group 3 FAX two dimensional (1 bit)
  CP_CCITTGROUP4        = 18; // TIFF, CCITT (T.6) Group 4 FAX two dimensional (1 bit)
  CP_LZW                = 19; // GIF & TIFF, (Variant of LZ78)
  CP_THUNDERSCAN        = 20; // TIFF, Thunderscan (4 bit)
  CP_JPEG_STD           = 21; // JPEG, Standard
  CP_JPEG_PROG          = 22; // JPEG, Progressive
  CP_JPEG2000           = 23; // JPEG 2000
  CP_GIF87A             = 24; // Interlaced
  CP_GIF89A             = 25; // Interlaced
  CP_PNG                = 26; // PNG
  // CP_LW77               = ;
  // CP_LW78               = ;
(*
type
  TmcmCompress = (CP_NOCOMP,                // No compression
                  CP_RLE4,                  // Windows bitmap, 4 bit run-length
                  CP_RLE8,                  // Windows bitmap, 8 bit run-length
                  CP_RLE_TARGA,             // Targa, 8, 16, 24 bit run-length
                  CP_RLE_PCX,               // PCX, x bit run-length
                  CP_RLE_SGI,               // SGI, 8,24 bit (16 bit not supported) run-length
                  CP_ASCII_PBM,             // Portable Bitmap, ASCII
                  CP_BIN_PBM,               // Portable Bitmap, Binary
                  CP_ASCII_PPM,             // Portable Pixel map, ASCII
                  CP_BIN_PPM,               // Portable Pixel map, Binary
                  CP_ASCII_PGM,             // Portable Grey map, ASCII
                  CP_BIN_PGM,               // Portable Grey map, Binary
                  CP_HUFFMAN,               // Huffman coding (8 bit)
                  CP_PACKBITS,              // TIFF, Pack bits (1, 4, 8, 24 bit) run-length
                  CP_MODIFIED_HUFFMAN,      // TIFF, Modified Huffman (1 bit)
                  CP_MODIFIED_HUFFMAN_W,    // TIFF, Modified Huffman (1 bit) Word aligned.
                  CP_CCITTGROUP3,           // TIFF, CCITT (T.4) Group 3 FAX one dimensional (1 bit)
                  CP_CCITTGROUP3_2D,        // TIFF, CCITT (T.4) Group 3 FAX two dimensional (1 bit)
                  CP_CCITTGROUP4,           // TIFF, CCITT (T.6) Group 4 FAX two dimensional (1 bit)
                  CP_LZW,                   // GIF & TIFF, (Variant of LZ78)
                  CP_THUNDERSCAN,           // TIFF, Thunderscan (4 bit)
                  CP_JPEG_STD,              // JPEG, Standard
                  CP_JPEG_PROG,             // JPEG, Progressive
                  CP_JPEG2000,              // JPEG 2000
                  CP_GIF87A,                // Interlaced
                  CP_GIF89A,                // Interlaced
                  CP_PNG                    // PNG
                  // CP_LW77,
                  // CP_LW78,
                  {$IFDEF MCMFILEFORMAT}
                    ,
                    CP_LZM
                  {$ENDIF}
                  );
  TmcmCompressions = Set of TmcmCompress;
*)
const
  CP_USERDEFINED = $8000;

type
  TmcmJPEGYCbCr = (JYCC_AUTO,   // FQuality determines Y, Cb and Cr frequency.
                   JYCC_411,    // Frequencies are fixed: Y = 2, Cb = 1 and Cr = 1.
                   JYCC_421);   // Frequencies are fixed: Y = 2,
                                // Horizontal Cb, Cr = 2
                                // Vertical Cb, Cr = 1.

const
  // Depricated, CCompressStrings do not use!!!
  CCompressStrings : array[0..{$IFDEF MCMFILEFORMAT}
                              word(CP_LZM)
                              {$ELSE}
                              word(CP_PNG)
                              {$ENDIF}] of string     = (resCP_NOCOMP,
                                                         resCP_RLE4,
                                                         resCP_RLE8,
                                                         resCP_RLE8_24,
                                                         resCP_RLE1_24,
                                                         resCP_RLE8_32,
                                                         resCP_ASCII,
                                                         resCP_BIN,
                                                         resCP_ASCII,
                                                         resCP_BIN,
                                                         resCP_ASCII,
                                                         resCP_BIN,
                                                         resCP_HUFFMAN,
                                                         resCP_PACKBITS,
                                                         resCP_MODIFIED_HUFFMAN,
                                                         resCP_MODIFIED_HUFFMAN_W,
                                                         resCP_CCITTGROUP3,
                                                         resCP_CCITTGROUP3_2D,
                                                         resCP_CCITTGROUP4,
                                                         resCP_LZW,
                                                         resCP_THUNDERSCAN,
                                                         resCP_JPEG_STD,
                                                         resCP_JPEG_PROG,
                                                         resCP_JPEG_2000,
                                                         resCP_GIF87A,
                                                         resCP_GIF89A,
                                                         resCP_PNG
                                                         {$IFDEF MCMFILEFORMAT},
                                                         resCP_LZM{$ENDIF});


  //----------------------------------------------------------------------------
  // Define Longword and int64 for Delphi 3.
  {$IFDEF DCB3}
type
    longword = dword;
    int64    = TLargeInteger;
  {$ENDIF}

const
  //----------------------------------------------------------------------------
  // Bit mask.
  BitMask : array[0..31] of longword = ($00000001,
                                        $00000002,
                                        $00000004,
                                        $00000008,
                                        $00000010,
                                        $00000020,
                                        $00000040,
                                        $00000080,
                                        $00000100,
                                        $00000200,
                                        $00000400,
                                        $00000800,
                                        $00001000,
                                        $00002000,
                                        $00004000,
                                        $00008000,
                                        $00010000,
                                        $00020000,
                                        $00040000,
                                        $00080000,
                                        $00100000,
                                        $00200000,
                                        $00400000,
                                        $00800000,
                                        $01000000,
                                        $02000000,
                                        $04000000,
                                        $08000000,
                                        $10000000,
                                        $20000000,
                                        $40000000,
                                        $80000000);


type
  //----------------------------------------------------------------------------
  // Data alignment, Image files.
  TmcmDataAlign = (DA_BYTE,
                   DA_WORD,
                   DA_DWORD);

  //----------------------------------------------------------------------------
  // Threshold methods.
  TmcmThreshold = (TH_LEVEL,
                   TH_ISODATA,
                   TH_SYMMETRY,
                   TH_TRACE,
                   TH_TRIANGULAR,
                   TH_EDGE,
                   TH_OPTIMIZED,
                   TH_STATISTIC);

  //----------------------------------------------------------------------------
  // Resize Image.
  TmcmInterpolate = (ST_NEAREST,
                     ST_BILINEAR,
                     ST_BIQUADRATIC,
                     ST_BICUBIC,
                     ST_HERMITE
                     {
                     ST_BELL,
                     ST_SPLINE,
                     ST_LANCZOS,
                     ST_MITCHELL,
                     }
                     );
  TmcmResize = TmcmInterpolate;

  //----------------------------------------------------------------------------
  // Image Math
  TmcmImageMathematics = (IM_ADD, IM_AND, IM_ANDBW, IM_AVE, IM_BLEND, IM_DIFF, IM_DIV,
                          IM_EQU, IM_GT, IM_MUL, IM_ORBW, IM_SUB, IM_XORBW, IM_MAG, IM_ORI);



  //----------------------------------------------------------------------------
  // Vector & Matrix definitions.

  TVectorB    = array[0..0] of byte;
  PVectorB    = ^TVectorB;
  TVectorShI  = array[0..0] of shortint;
  PVectorShI  = ^TVectorShI;
  TVectorW    = array[0..0] of word;
  PVectorW    = ^TVectorW;
  TVectorSI    = array[0..0] of smallint;
  PVectorSI    = ^TVectorSI;
  TVectorI    = array[0..0] of integer;
  PVectorI    = ^TVectorI;
  TVectorL    = array[0..0] of longint;
  PVectorL    = ^TVectorL;
  TVectorLW   = array[0..0] of longword;
  PVectorLW   = ^TVectorLW;
  TVectorS    = array[0..0] of single;
  PVectorS    = ^TVectorS;
  TVectorD    = array[0..0] of double;
  PVectorD    = ^TVectorD;
  TVectorE    = array[0..0] of extended;
  PVectorE    = ^TVectorE;
  TVectorRGB  = array[0..0] of TRGBTriple;
  PVectorRGB  = ^TVectorRGB;
  TVectorRGBA = array[0..0] of TRGBQuad;
  PVectorRGBA = ^TVectorRGBA;
  TVectorC    = array[0..0] of Cardinal;
  PVectorC    = ^TVectorC;

  TmcmPoint   = packed record // Introduced to avoid confusion in C++Builder
  {$IFDEF DELPHI}
    x : integer; // Must be integer in Delphi for Point() to work.
    y : integer;
  {$ELSE}
    x : longint; // BCB cannot translate TVectorPt correctly.
    y : longint;
  {$ENDIF}
  (*
  public
    class operator Equal(const Lhs, Rhs : TmcmPoint) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TmcmPoint): Boolean;
    class operator Add(const Lhs, Rhs : TmcmPoint): TmcmPoint;
    class operator Subtract(const Lhs, Rhs : TmcmPoint): TmcmPoint;
    class operator IntDivide(a : TmcmPoint; r : integer) : TmcmPoint;
  *)
  end;

  TPointDouble = packed record
    x : double;
    y : double;
  (*
  public
    class operator Equal(const Lhs, Rhs : TPointDouble) : Boolean;
    class operator NotEqual(const Lhs, Rhs : TPointDouble): Boolean;
    class operator Add(const Lhs, Rhs : TPointDouble): TPointDouble;
    class operator Subtract(const Lhs, Rhs : TPointDouble): TPointDouble;
    class operator Divide(a : TPointDouble; r : double) : TPointDouble;
  *)
  end;
  PPointDouble = ^TPointDouble;

  TVectorPtD   = array[0..0] of TPointDouble;
  PVectorPtD   = ^TVectorPtD;


  TVectorPt    = array[0..0] of TmcmPoint;
  PVectorPt    = ^TVectorPt;

  TMatrixB    = array[0..0] of PVectorB;
  PMatrixB    = ^TMatrixB;
  TMatrixW    = array[0..0] of PVectorW;
  PMatrixW    = ^TMatrixW;
  TMatrixI    = array[0..0] of PVectorI;
  PMatrixI    = ^TMatrixI;
  TMatrixL    = array[0..0] of PVectorL;
  PMatrixL    = ^TMatrixL;
  TMatrixLW   = array[0..0] of PVectorLW;
  PMatrixLW   = ^TMatrixLW;
  TMatrixS    = array[0..0] of PVectorS;
  PMatrixS    = ^TMatrixS;
  TMatrixD    = array[0..0] of PVectorD;
  PMatrixD    = ^TMatrixD;
  TMatrixE    = array[0..0] of PVectorE;
  PMatrixE    = ^TMatrixE;

  TVecPointerRec = record
                     case Word of
                     0 : (Ptr  : Pointer);
                     1 : (Long : longint);
                     2 : (Lo   : word;
                          Hi   : word);
                   end;
  TVecByteRec    = record
                     case Word of
                     0 : (Ptr  : PVectorB);
                     1 : (Long : longint);
                     2 : (Lo   : word;
                          Hi   : word);
                   end;
  TVecWordRec    = record
                     case Word of
                     0 : (Ptr  : PVectorW);
                     1 : (Long : longint);
                     2 : (Lo   : word;
                          Hi   : word);
                   end;
  TVecLongRec    = record
                     case Word of
                     0 : (Ptr  : PVectorL);
                     1 : (Long : longint);
                     2 : (Lo   : word;
                          Hi   : word);
                   end;

  TmcmRGB = record
    Red   : byte;
    Green : byte;
    Blue  : byte;
  end;

  
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [byte] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [byte] of TRGBQuad;

  {$TYPEINFO ON}
type
  TmcmRealFrame = class
  private
    FLeft   : double;
    FTop    : double;
    FRight  : double;
    FBottom : double;
  protected
  public
  published
    property Left : double
      read  FLeft
      write FLeft;
    property Top : double
      read  FTop
      write FTop;
    property Right : double
      read  FRight
      write FRight;
    property Bottom : double
      read  FBottom
      write FBottom;
  end;

type
  TNeighbour = record
               x1, y1 : integer;
               x2, y2 : integer;
               Up     : boolean;
               end;
  TVectorNeighbour = array[0..0] of TNeighbour;
  PVectorNeighbour = ^TVectorNeighbour;

  //----------------------------------------------------------------------------
  {$IFDEF DCB3_5}
const
  CPUID  = $A20F;
{$ENDIF}

{$IFDEF MCMTEST}
const
  // Used internally for test purpose.
  TestImageDir = 'E:\IMAGES\FILTER\';
{$ENDIF}

const // Custom cursor
  crPipette   = 128;
  crPencil    = 129;
  crRectangle = 130;
  crCircle    = 131;

function mcmPoint(X, Y : longint) : TmcmPoint;

function PointDouble(p : TmcmPoint) : TPointDouble;


{$IFDEF DCB3}{$UNDEF DCB3}{$ENDIF}

implementation

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}
(*
class operator TmcmPoint.Equal(const Lhs, Rhs: TmcmPoint) : Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TmcmPoint.NotEqual(const Lhs, Rhs: TmcmPoint) : Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TmcmPoint.Add(const Lhs, Rhs: TmcmPoint) : TmcmPoint;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TmcmPoint.Subtract(const Lhs, Rhs: TmcmPoint) : TmcmPoint;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

class operator TmcmPoint.IntDivide(a : TmcmPoint; r : integer) : TmcmPoint;
begin
  Result.X := (a.X div r);
  Result.Y := a.Y div r;
end; // TmcmPoint.IntDivide
*)

function mcmPoint(X, Y : longint) : TmcmPoint;
begin
  Result.X := X;
  Result.Y := Y;
end; // End mcmPoint.

(*
class operator TPointDouble.Equal(const Lhs, Rhs: TPointDouble) : Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TPointDouble.NotEqual(const Lhs, Rhs: TPointDouble) : Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TPointDouble.Add(const Lhs, Rhs: TPointDouble) : TPointDouble;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TPointDouble.Subtract(const Lhs, Rhs: TPointDouble) : TPointDouble;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

class operator TPointDouble.Divide(a : TPointDouble; r : double) : TPointDouble;
begin
  Result.x := a.x / r;
  Result.y := a.y / r;
end; // TPointDouble.Divide
*)

function PointDouble(p : TmcmPoint) : TPointDouble;
begin
  Result.x := p.x;
  Result.y := p.y;
end; // End TPointDouble.

end.
